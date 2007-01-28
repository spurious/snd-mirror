#include "snd.h"

/* PERHAPS: notebook style access to other tracks (next and previous by number are not very intuitive) -- pulldown menu of names?
 * PERHAPS: undo&apply for track / mix env?, multiple mix/track dialogs, tempo curves in dialog?
 * PERHAPS: "forget" button -> free_track (forget all?)
 * SOMEDAY: currently, if track, drag mix does not move axes (and waveform doesn't follow the tag)
 * PERHAPS: multiple mix/track dialogs
 * perhaps: mix pane sort of like current mark pane? (the mixes function has identical output to marks)
 *            this needs a hook for mix-creation and deletion (current mark-pane code uses mark-hook
 *            which is called at add/delete/reposition causing the lists to be rebuilt).  See tmp177.scm.
 *            But mark-hook is actually redundant -- we could use a watcher instead.
 *            This would mean adding run_watchers() to add_mix, perhaps finish_moving_mix, set_mix_position,
 *            perhaps delete_mix, and then doc/test etc
 * PERHAPS: quick access + edit of underlying mix data, ripple and update if saved (like region edit)
 *          also double click in edit history => go to that temp file?
 * PERHAPS: some way to tie multichannel mixes together without the track appearing in the track dialog
 *          mix-sync field?  or a button: ignore local tracks
 */

typedef struct {
  int chans;
  Float *scalers;
  Float speed;
  env **amp_envs;
  off_t len;
} mix_track_state;  

typedef struct {               /* save one mix state */
  int chans;                   /* size of arrays in this struct */
  int edit_ctr;                /* cp edit_ctr at time of creation of this struct */
  int track;
  off_t beg, end, orig, len;   /* samp positions in output (orig = where edit tree thinks it is) */
  bool locked, inverted;
  Float *scalers;
  Float speed;
  env **amp_envs;
  mix_track_state *as_built;
} mix_state;

typedef struct {
  chan_info *cp;
  mix_context *wg;
  char *in_filename;
  int in_chans;
  off_t in_samps;              /* in_samps needed to simplify speed changed duration calculations */
  int mix_state_size;          /* current size of mix_state list */
  mix_state **states;          /* list of mixer states */
  mix_state *active_mix_state;
  off_t tag_position, track_tag_position;
  int current_state;
  file_delete_t temporary;     /* in-filename was written by us and needs to be deleted when mix state is deleted */
  snd_info *add_snd;           /* readable snd_info struct for mix input */
  int id, x, nx, y, tagx, tagy, height, orig_chan, orig_edit_ctr, tag_y; 
  speed_style_t speed_style;
  env **dialog_envs;           /* mix dialog version of current amp envs */
  bool save_needed;            /* for mix drag display */
  char *name;
} mix_info;

typedef enum {C_STRAIGHT_SOUND, C_AMP_SOUND, C_SPEED_SOUND, C_ZERO_SOUND, C_AMP_ENV_SOUND, C_SPEED_AMP_SOUND, C_SPEED_ENV_SOUND,
	      C_STRAIGHT_PEAK, C_AMP_PEAK, C_SPEED_PEAK, C_ZERO_PEAK, C_AMP_ENV_PEAK, C_SPEED_AMP_PEAK, C_SPEED_ENV_PEAK} mix_calc_t;

typedef struct mix_fd {
  int type;
  mix_info *md;
  snd_fd **sfs;
  int chans;                           /* chans of input */
  mix_calc_t calc;
  int base;
  Float x, sr;
  Float *lst, *nxt;
  src_state **srcs;
  mus_any **segs;
  int *ctr;
  off_t *samples;
  mus_sample_t **idata;
  int samps_per_bin, dangling_loc;
  env_info **eps;                      /* new envs created via env_on_env */
  Float *scalers;
} mix_fd;


static mix_info *md_from_id(int n);
static void draw_mix_waveform(mix_info *md);
static void erase_mix_waveform(mix_info *md);
static mix_track_state *copy_mix_track_state(mix_track_state *cs);
static mix_track_state *free_mix_track_state(mix_track_state *cs);
static Float gather_track_amp(mix_state *cs);
static Float gather_track_speed(int id);
static env *gather_track_amp_env(mix_state *cs);
static void release_dangling_mix_readers(mix_info *md);
static void set_mix_track(mix_info *md, int trk, bool redisplay);
static int new_track(void);
static void remix_track_with_preset_times(int id, off_t new_position, off_t new_frames, 
					  void (*init)(mix_info *m1, void *p1), Float speed_change,
					  void (*func)(mix_info *m2, void *p2), void *func_val);
static void reset_bounds(mix_info *md, void *val);
static bool found_track_amp_env(int trk);
static int track_members(int track_id);
static int make_track(int *mixes, int len);

#define ALL_TRACK_CHANS -1

static XEN mix_release_hook;
static XEN mix_drag_hook;
/* also mix_click_hook in snd-chn.c */

chan_info *mix_dialog_mix_channel(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) return(md->cp);
  return(NULL);
}

static void color_one_mix_from_id(int mix_id, color_t color)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) md->wg->color = color;
}

#if (!USE_NO_GUI)
static color_t mix_to_color_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    return(md->wg->color);
  return(ss->sgx->basic_color);
}
#endif

static axis_context *set_mix_waveform_context(chan_info *cp, mix_info *md)
{
  axis_context *ax;
  ax = mix_waveform_context(cp);
  set_foreground_color(ax, md->wg->color); 
  return(ax);
}


/* -------- mix_contexts (saved graphs for quick erase and redraw) -------- */

mix_context *make_mix_context(chan_info *cp)
{
  mix_context *g;
  g = (mix_context *)CALLOC(1, sizeof(mix_context));
  g->graph = channel_graph(cp);
  g->color = ss->sgx->mix_color;
  return(g);
}

static mix_context *set_mix_info_context(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE) cp = sp->chans[0];
  return(make_mix_context(cp));
}

mix_context *free_mix_context(mix_context *ms)
{
  if (ms->p0) {FREE(ms->p0); ms->p0 = NULL;}
  if (ms->p1) {FREE(ms->p1); ms->p1 = NULL;}
  FREE(ms);
  return(NULL);
}


/* -------- history of mix dialog settings -------- */

static mix_state *make_mix_state(int chans, int edit_ctr, off_t beg, off_t end)
{
  mix_state *cs;
  cs = (mix_state *)CALLOC(1, sizeof(mix_state));
  cs->chans = chans;
  cs->edit_ctr = edit_ctr;
  cs->orig = beg;
  cs->beg = beg;
  cs->end = end;
  cs->len = end - beg + 1;
  cs->locked = false;
  cs->inverted = false;
  cs->scalers = (Float *)CALLOC(chans, sizeof(Float));
  cs->amp_envs = NULL; 
  cs->as_built = NULL;
  return(cs);
}

static mix_state *copy_mix_state(mix_state *cs)
{
  mix_state *ncs;
  int i;
  ncs = make_mix_state(cs->chans, cs->edit_ctr, cs->orig, cs->end);
  if (!(cs->locked))
    for (i = 0; i < cs->chans; i++)
      ncs->scalers[i] = cs->scalers[i];
  ncs->locked = cs->locked;
  ncs->inverted = cs->inverted;
  ncs->speed = cs->speed;
  ncs->track = cs->track;
  if (cs->amp_envs)
    {
      ncs->amp_envs = (env **)CALLOC(cs->chans, sizeof(env *));
      for (i = 0; i < cs->chans; i++) 
	ncs->amp_envs[i] = copy_env(cs->amp_envs[i]);
    }
  else ncs->amp_envs = NULL;
  ncs->len = cs->len;
  if (ncs->as_built)
    ncs->as_built = free_mix_track_state(ncs->as_built);
  if (cs->as_built)
    ncs->as_built = copy_mix_track_state(cs->as_built);
  return(ncs);
}

static void make_current_mix_state(mix_info *md)
{
  int i;
  mix_state *cs, *cur;
  cs = md->states[md->current_state];
  cur = md->active_mix_state;
  cur->chans = cs->chans;
  cur->edit_ctr = cs->edit_ctr;
  cur->orig = cs->beg;
  cur->beg = cs->beg;
  cur->end = cs->end;
  cur->len = cs->len;
  cur->locked = cs->locked;
  cur->inverted = cs->inverted;
  cur->track = cs->track;
  if (!(cs->locked))
    for (i = 0; i < cs->chans; i++)
      cur->scalers[i] = cs->scalers[i];
  cur->speed = cs->speed;
  if (cur->amp_envs)
    for (i = 0; i < cs->chans; i++) 
      {
	cur->amp_envs[i] = free_env(cur->amp_envs[i]);
	if (cs->amp_envs)
	  cur->amp_envs[i] = copy_env(cs->amp_envs[i]);
      }
  if (cur->as_built)
    cur->as_built = free_mix_track_state(cur->as_built);
  if (cs->as_built)
    cur->as_built = copy_mix_track_state(cs->as_built);
  else cur->as_built = NULL;
}

static mix_state *free_mix_state(mix_state *cs)
{
  if (cs)
    {
      if (cs->scalers) {FREE(cs->scalers); cs->scalers = NULL;}
      if (cs->amp_envs) 
	{
	  int i;
	  for (i = 0; i < cs->chans; i++) 
	    free_env(cs->amp_envs[i]);
	  FREE(cs->amp_envs);
	}
      if (cs->as_built)
	cs->as_built = free_mix_track_state(cs->as_built);
      FREE(cs);
    }
  return(NULL);
}

static void release_pending_mix_states(mix_info *md)
{
  int i;
  if (md->states)
    for (i = md->current_state + 1; i < md->mix_state_size; i++) 
      if (md->states[i]) 
	md->states[i] = free_mix_state(md->states[i]);
}


/* -------- mix_info (state of mix) -------- */

#define MIX_INFO_INCREMENT 16
static mix_info **mix_infos = NULL;
static int mix_infos_size = 0;
static int mix_infos_ctr = 0;

bool mix_ok(int n) 
{
  if ((n >= 0) && 
      (n < mix_infos_size))
    {
      mix_info *md; 
      md = mix_infos[n];
      return((md) && 
	     (md->states) && 
	     (md->states[0]) && 
	     (md->cp) &&
	     (((md->states[0])->edit_ctr) <= (md->cp->edit_ctr)));
    }
  return(false);
}

bool mix_ok_and_unlocked(int n)
{
  return((mix_ok(n)) &&
	 (mix_infos[n]->active_mix_state) &&
	 (!(mix_infos[n]->active_mix_state->locked)));
}

static mix_info *md_from_id(int n) 
{
  if (mix_ok(n))
    return(mix_infos[n]);
  return(NULL);
}

char *mix_name(int id)
{
  if (mix_ok(id))
    return(mix_infos[id]->name);
  return(NULL);
}

int mix_name_to_id(const char *name)
{
  int i, loc_so_far = -1;
  chan_info *selected_cp = NULL;
  selected_cp = selected_channel();
  for (i = 0; i < mix_infos_size; i++)
    if ((mix_infos[i]) &&
	(mix_infos[i]->name) &&
	(strcmp(mix_infos[i]->name, name) == 0))
      {
	if ((!selected_cp) ||
	    (mix_infos[i]->cp == selected_cp))  /* try to find mix in the currently selected channel (possible name collisions) */
	  return(i);
	if (loc_so_far == -1)
	  loc_so_far = i;
      }
  return(loc_so_far);
}

static int pending_mix_id(void) {return(mix_infos_ctr);}

static mix_info *make_mix_info(chan_info *cp)
{
  mix_info *md;
  if (mix_infos == NULL)
    {
      mix_infos_size = MIX_INFO_INCREMENT;
      mix_infos = (mix_info **)CALLOC(mix_infos_size, sizeof(mix_info *));
    }
  else
    {
      if (mix_infos_ctr >= mix_infos_size)
	{
	  int i;
	  mix_infos_size += MIX_INFO_INCREMENT;
	  mix_infos = (mix_info **)REALLOC(mix_infos, mix_infos_size * sizeof(mix_info *));
	  for (i = mix_infos_size - MIX_INFO_INCREMENT; i < mix_infos_size; i++) 
	    mix_infos[i] = NULL;
	}
    }
  md = (mix_info *)CALLOC(1, sizeof(mix_info));
  mix_infos[mix_infos_ctr] = md;
  cp->have_mixes = true;
  md->id = mix_infos_ctr++;
  md->cp = cp;
  md->add_snd = NULL;
  md->temporary = DONT_DELETE_ME;
  md->wg = set_mix_info_context(cp);
  md->y = 0;
  md->tag_y = 0;
  md->tag_position = 0;
  md->height = mix_waveform_height(ss);
  md->speed_style = speed_control_style(ss);
  md->dialog_envs = NULL;
  md->name = NULL;
  return(md);
}

static int map_over_channel_mixes_with_void(chan_info *cp, int (*func)(mix_info *umx, void *val1), void *ptr)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  int val;
	  val = (*func)(md, ptr);
	  if (val) return(val);
	}
    }
  return(0);
}

static void map_over_channel_mixes_with_int(chan_info *cp, void (*func)(mix_info *umx, int val1), int value)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	(*func)(md, value);
    }
}

static void map_over_channel_mixes(chan_info *cp, void (*func)(mix_info *umx))
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	(*func)(md);
    }
}

static int map_over_mixes_with_void(int (*func)(mix_info *umd, void *val), void *ptr)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if (md)
	{
	  int val;
	  val = (*func)(md, ptr);
	  if (val) return(val);
	}
    }
  return(0);
}

static void map_over_mixes_with_int(void (*func)(mix_info *umd, int val1), int value)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if (md) (*func)(md, value);
    }
}

static int look_for_mix_tempfile(mix_info *md, void *in_name) /* int rtn type for map_over_mixes */
{
  return((md->in_filename) && 
	 (strcmp(md->in_filename, (char *)in_name) == 0));
}

static mix_info *free_mix_info(mix_info *md)
{
  if (md)
    {
      int i;
      if (md->name) {FREE(md->name); md->name = NULL;}
      release_dangling_mix_readers(md);
      if (md->wg) md->wg = free_mix_context(md->wg);
      mix_infos[md->id] = NULL;
      if (md->temporary == DELETE_ME)
	{
	  if (mus_file_probe(md->in_filename))
	    snd_remove(md->in_filename, REMOVE_FROM_CACHE);
	}
      else
	{
	  if (md->temporary == MULTICHANNEL_DELETION) /* n-chan selection via C-x q for example */
	    {
	      if (!(map_over_mixes_with_void(look_for_mix_tempfile, (void *)(md->in_filename))))
		{
		  if (mus_file_probe(md->in_filename))
		    snd_remove(md->in_filename, REMOVE_FROM_CACHE);
		}
	    }
	}
      if (md->in_filename) {FREE(md->in_filename); md->in_filename = NULL;}
      if (md->add_snd) {completely_free_snd_info(md->add_snd); md->add_snd = NULL;}
      if (md->states)
	{
	  for (i = 0; i < md->mix_state_size; i++) 
	    if (md->states[i]) 
	      md->states[i] = free_mix_state(md->states[i]);
	  FREE(md->states);
	  md->states = NULL;
	}
      if (md->active_mix_state) 
	{
	  if (md->active_mix_state->as_built)
	    md->active_mix_state->as_built = free_mix_track_state(md->active_mix_state->as_built);
	  md->active_mix_state = free_mix_state(md->active_mix_state);
	}
      if (md->dialog_envs)
	{
	  for (i = 0; i < md->in_chans; i++)
	    free_env(md->dialog_envs[i]);
	  FREE(md->dialog_envs);
	  md->dialog_envs = NULL;
	}
      FREE(md);
    }
  return(NULL);
}

/* edit-list->function support for mixes/tracks:
 *      mix list search for current channel, make outer let holding all names as (-mix-### ###)
 *      origins for make-mix procs: (set! -mix-### (...))
 *      origins otherwise, reference to mix: -mix-###
 *      same mechanism for tracks
 *      eventually we'll need to deal with Ruby as well
 * what about mix not created within list then applied to some other channel? -- some auto-copy mechanism?
 * before ref, check that we re-made it, if not find some way to re-make or give up?
 */

#if HAVE_SCHEME
  #define PROC_SET_MIX "set! (%s -mix-%d) "
  #define PROC_SET_MIX_CHANNEL "set! (%s -mix-%d %d) "
  #define PROC_SET_TRACK "set! (%s %d) "
  #define PROC_SET_TRACK_CHANNEL "set! (%s %d %d) "
#else
  #define PROC_SET_MIX "set_%s(_mix_%d, "
  #define PROC_SET_MIX_CHANNEL "set_%s(_mix_%d, %d, "
  #define PROC_SET_TRACK "set_%s(%d, "
  #define PROC_SET_TRACK_CHANNEL "set_%s(%d, %d, "
#endif

char *edit_list_mix_and_track_init(chan_info *cp)
{
  char *mix_list = NULL, *old_list = NULL;
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  old_list = mix_list;
#if HAVE_SCHEME
	  mix_list = mus_format("%s%s(-mix-%d %d)", 
				(old_list) ? old_list : "", 
				(old_list) ? " " : "",  /* strcat of previous + possible space */
				i, i);                  /* i is md->id = mix id from user's point of view */
#endif
#if HAVE_RUBY
	  mix_list = mus_format("%s%s_mix_%d = %d", 
				(old_list) ? old_list : "", 
				(old_list) ? "; " : "",  /* strcat of previous + possible space */
				i, i);                   /* i is md->id = mix id from user's point of view */
#endif
#if HAVE_FORTH
	  mix_list = mus_format("%s%s%d { -mix-%d }", 
				(old_list) ? old_list : "", 
				(old_list) ? " " : "",   /* strcat of previous + possible space */
				i, i);                   /* i is md->id = mix id from user's point of view */
#endif
	  if (old_list) FREE(old_list);
	}
    }
  return(mix_list);
}


/* ---------------- MIX READ ---------------- */

static snd_info *make_mix_readable(mix_info *md)
{
  if (md == NULL) return(NULL);
  if (!md->add_snd) 
    {
      chan_info *cp;
      int i;
      snd_info *add_sp;
      cp = md->cp;
      if (mus_file_probe(md->in_filename))
	md->add_snd = make_sound_readable(md->in_filename, true);
      else 
	{
	  snd_error(_("mix reader can't find file %s: %s"), md->in_filename, snd_io_strerror());
	  return(NULL);
	}
      add_sp = md->add_snd;
      add_sp->filename = copy_string(md->in_filename);
      add_sp->short_filename = filename_without_directory(add_sp->filename);
      for (i = 0; i < add_sp->nchans; i++) 
	{
	  cp = add_sp->chans[i];
	  if ((cp) && (cp->show_mix_waveforms))
	    make_mix_input_amp_env(cp);
	}
    }
  return(md->add_snd);
}

static bool mix_input_amp_env_usable(mix_info *md, Float samples_per_pixel) 
{
  snd_info *sp;
  sp = make_mix_readable(md);
  if (sp)
    {
      int i;
      bool happy = true;
      int samps_per_bin = 0;
      for (i = 0; i < sp->nchans; i++)
	{
	  chan_info *cp;
	  env_info *ep = NULL;
	  cp = sp->chans[i];
	  if ((cp == NULL) || (cp->amp_envs == NULL)) return(false);
	  ep = cp->amp_envs[0]; /* mixed-in-sound cp->edit_ctr always 0 */
	  if ((ep == NULL) && 
	      (CURRENT_SAMPLES(cp) > AMP_ENV_CUTOFF))
	    ep = make_mix_input_amp_env(cp);
	  if ((ep) && 
	      (samps_per_bin == 0)) 
	    samps_per_bin = ep->samps_per_bin;
	  happy = ((ep) && 
		   (samples_per_pixel >= (Float)(ep->samps_per_bin)) && 
		   (ep->samps_per_bin == samps_per_bin));
	  if (!happy) break;
	}
      return(happy);
    }
  return(false);
}

#define MIX_INPUT_INVALID -1
#define MIX_INPUT_SOUND 0
#define MIX_INPUT_AMP_ENV 1
#define MIX_TYPE_OK(Type) ((Type == MIX_INPUT_SOUND) || (Type == MIX_INPUT_AMP_ENV))
 
static Float next_mix_input_amp_env_sample(mix_fd *mf, int chan)
{
  if (mf->ctr[chan] < mf->samples[chan]) 
    /* we check before calling that this is a MIX_INPUT_AMP_ENV case (not INPUT_SOUND), 
     * so we're reading the amp env (optimized redisplay)
     */
    {
      mf->ctr[chan]++;
      return(MUS_SAMPLE_TO_FLOAT(mf->idata[chan][mf->ctr[chan]]));
    }
  return(0.0);
}

static Float next_mix_sample(mix_fd *mf)
{
  int i;
  bool need_move = false;
  Float sum = 0.0;
  switch (mf->calc)
    {
    case C_STRAIGHT_SOUND:
      return(read_sample_to_float(mf->sfs[mf->base]));
      break;
    case C_STRAIGHT_PEAK:
      return(next_mix_input_amp_env_sample(mf, mf->base));
      break;
    case C_ZERO_SOUND: 
    case C_ZERO_PEAK:
      return(0.0);
      break;
    case C_AMP_SOUND:
      for (i = 0; i < mf->chans; i++)
	sum += (mf->scalers[i] * read_sample_to_float(mf->sfs[i]));
      break;
    case C_AMP_PEAK:
      for (i = 0; i < mf->chans; i++)
	sum += (mf->scalers[i] * next_mix_input_amp_env_sample(mf, i));
      break;
    case C_AMP_ENV_SOUND:
      for (i = 0; i < mf->chans; i++)
	if (mf->segs[i])
	  sum += (mus_env(mf->segs[i]) * read_sample_to_float(mf->sfs[i]));
	else sum += (mf->scalers[i] * read_sample_to_float(mf->sfs[i]));
      break;
    case C_AMP_ENV_PEAK:
      for (i = 0; i < mf->chans; i++)
	sum += (mf->scalers[i] * next_mix_input_amp_env_sample(mf, i));
      break;
    case C_SPEED_SOUND:
      return(mus_src(mf->srcs[mf->base]->gen, mf->sr, &src_input_as_needed));
      break;
    case C_SPEED_PEAK:
      sum = (mf->lst[mf->base] + mf->x * (mf->nxt[mf->base] - mf->lst[mf->base]));
      need_move = true;
      break;
    case C_SPEED_AMP_SOUND:
      for (i = 0; i < mf->chans; i++)
	if (mf->scalers[i] > 0.0)
	  sum += (mf->scalers[i] * mus_src(mf->srcs[i]->gen, mf->sr, &src_input_as_needed));
      break;
    case C_SPEED_AMP_PEAK:
      for (i = 0; i < mf->chans; i++)
	if (mf->scalers[i] > 0.0)
	  sum += (mf->scalers[i] * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
      need_move = true;
      break;
    case C_SPEED_ENV_SOUND:
      for (i = 0; i < mf->chans; i++)
	if (mf->scalers[i] > 0.0)
	  {
	    if (mf->segs[i])
	      sum += (mus_env(mf->segs[i]) * mus_src(mf->srcs[i]->gen, mf->sr, &src_input_as_needed));
	    else sum += (mf->scalers[i] * mus_src(mf->srcs[i]->gen, mf->sr, &src_input_as_needed));
	  }
      break;
    case C_SPEED_ENV_PEAK:
      for (i = 0; i < mf->chans; i++)
	if (mf->scalers[i] > 0.0)
	  sum += (mf->scalers[i] * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
      need_move = true;
      break;
    }
  if (need_move)
    {
      int j, move;
      mf->x += mf->sr;
      move = (int)(mf->x);
      if (move != 0)
	{
	  mf->x -= move;
	  for (j = 0; j < move; j++)
	    for (i = 0; i < mf->chans; i++)
	      {
		mf->lst[i] = mf->nxt[i];
		if (mf->type == MIX_INPUT_SOUND)
		  mf->nxt[i] = read_sample_to_float(mf->sfs[i]);
		else mf->nxt[i] = next_mix_input_amp_env_sample(mf, i);
	      }
	}
    }
  return(sum);
}

Float mix_read_sample_to_float(struct mix_fd *ptr) {return(next_mix_sample(ptr));}

#define PREVIOUS_MIX true
#define CURRENT_MIX false

static mix_fd *init_mix_read_any(mix_info *md, bool old, int type, off_t beg)
{
  mix_fd *mf;
  env *e;
  snd_info *add_sp;
  chan_info *cp;
  mix_state *cs;
  int i, chans;
  if (old == PREVIOUS_MIX)
     cs = md->states[md->current_state];
  else cs = md->active_mix_state;
  chans = md->in_chans;
  mf = (mix_fd *)CALLOC(1, sizeof(mix_fd));
  mf->type = type;
  mf->calc = C_ZERO_SOUND;
  mf->sr = cs->as_built->speed;
  mf->md = md;
  mf->dangling_loc = -1;
  mf->chans = chans;
  mf->scalers = (Float *)CALLOC(mf->chans, sizeof(Float));
  for (i = 0; i < chans; i++)
    {
      mf->scalers[i] = cs->as_built->scalers[i];
      if (mf->scalers[i] != 0.0) 
	{
	  mf->calc = C_STRAIGHT_SOUND; 
	  break;
	}
    }
  if (mf->calc == C_ZERO_SOUND) 
    return(mf);
  if (!(md->add_snd)) 
    {
      md->add_snd = make_mix_readable(md);
      if (!(md->add_snd)) 
	return(NULL);
    }
  add_sp = md->add_snd;
  if (type == MIX_INPUT_SOUND)
    mf->sfs = (snd_fd **)CALLOC(chans, sizeof(snd_fd *));
  else mf->sfs = NULL;
  cp = md->cp;
  mf->base = cp->chan;
  if (cs->as_built->amp_envs)
    {
      if (type == MIX_INPUT_SOUND)
	{
	  mf->eps = NULL;
	  mf->segs = (mus_any **)CALLOC(mf->chans, sizeof(mus_any *));
	  for (i = 0; i < chans; i++)
	    {
	      e = cs->as_built->amp_envs[i];
	      if ((e) && (mf->scalers[i] != 0.0))
		{
		  mf->segs[i] = mus_make_env(e->data, e->pts, mf->scalers[i], 0.0, e->base, 0.0, 0, cs->as_built->len - 1, NULL);
		  if (beg > 0)
		    mus_set_location(mf->segs[i], beg);
		}
	    }
	}
      else
	{
	  mf->segs = NULL;
	  mf->eps = (env_info **)CALLOC(mf->chans, sizeof(env_info *));
	}
    }
  else 
    {
      mf->segs = NULL;
      mf->eps = NULL;
    }
  if (mf->base >= chans) mf->base = 0; /* mono file mixed into sync'd stereo file for example */
  if (mf->sr != 1.0)
    {
      if (cs->as_built->amp_envs)
	mf->calc = C_SPEED_ENV_SOUND;
      else 
	{
	  mf->calc = C_SPEED_SOUND;
	  for (i = 0; i < chans; i++)
	    if (((i == mf->base) && (mf->scalers[i] != 1.0)) ||
		((i != mf->base) && (mf->scalers[i] != 0.0)))
	      {
		mf->calc = C_SPEED_AMP_SOUND;
		break;
	      }
	}
    }
  else
    {
      if (cs->as_built->amp_envs)
	mf->calc = C_AMP_ENV_SOUND;
      else
	{
	  mf->calc = C_STRAIGHT_SOUND;
	  for (i = 0; i < chans; i++)
	    {
	      if (((i == mf->base) && (mf->scalers[i] != 1.0)) ||
		  ((i != mf->base) && (mf->scalers[i] != 0.0)))
		{
		  mf->calc = C_AMP_SOUND;
		  break;
		}
	    }
	}
    }
  if (type == MIX_INPUT_SOUND)
    {
      if (mf->calc == C_STRAIGHT_SOUND)
	mf->sfs[mf->base] = init_sample_read(beg, add_sp->chans[mf->base], READ_FORWARD); 
      else
	{
	  for (i = 0; i < chans; i++)
	    mf->sfs[i] = init_sample_read(beg, add_sp->chans[i], READ_FORWARD);
	}
    }
  else
    {
      switch (mf->calc)
	{
	case C_STRAIGHT_SOUND:  mf->calc = C_STRAIGHT_PEAK;  break; 
	case C_AMP_SOUND:       mf->calc = C_AMP_PEAK;       break;
	case C_SPEED_SOUND:     mf->calc = C_SPEED_PEAK;     break;
	case C_ZERO_SOUND:      mf->calc = C_ZERO_PEAK;      break;
	case C_AMP_ENV_SOUND:   mf->calc = C_AMP_ENV_PEAK;   break;
	case C_SPEED_AMP_SOUND: mf->calc = C_SPEED_AMP_PEAK; break;
	case C_SPEED_ENV_SOUND: mf->calc = C_SPEED_ENV_PEAK; break;
	default: break;
	}
    }
  if (mf->sr != 1.0)
    {
      if (type == MIX_INPUT_SOUND)
	{
	  mf->srcs = (src_state **)CALLOC(chans, sizeof(src_state *));
	  for (i = 0; i < chans; i++)
	    mf->srcs[i] = make_src(0.0, mf->sfs[i], 1.0);
	  mf->lst = NULL;
	  mf->nxt = NULL;
	}
      else
	{
	  /* initialize interpolator */
	  mf->srcs = NULL;
	  mf->lst = (Float *)CALLOC(chans, sizeof(Float));
	  mf->nxt = (Float *)CALLOC(chans, sizeof(Float));
	  if (type == MIX_INPUT_SOUND)
	    {
	      for (i = 0; i < chans; i++)
		{
		  mf->lst[i] = read_sample_to_float(mf->sfs[i]);
		  mf->nxt[i] = read_sample_to_float(mf->sfs[i]);
		}
	    }
	}
    }
  else
    {
      mf->lst = NULL;
      mf->nxt = NULL;
      mf->srcs = NULL;
    }
  return(mf);
}

static mix_fd *init_mix_read(mix_info *md, bool old, off_t beg)
{
  return(init_mix_read_any(md, old, MIX_INPUT_SOUND, beg));
}

#define HI_PEAKS true
#define LO_PEAKS false

static mix_fd *init_mix_input_amp_env_read(mix_info *md, bool hi)
{
  int i;
  mix_fd *mf = NULL;
  snd_info *sp;
  mf = init_mix_read_any(md, CURRENT_MIX, MIX_INPUT_AMP_ENV, 0);
  if (!mf) return(NULL);
  sp = md->add_snd;
  mf->ctr = (int *)CALLOC(sp->nchans, sizeof(int));
  mf->samples = (off_t *)CALLOC(sp->nchans, sizeof(off_t));
  if ((mf->calc != C_ZERO_SOUND) && (mf->calc != C_ZERO_PEAK))
    mf->idata = (mus_sample_t **)CALLOC(sp->nchans, sizeof(mus_sample_t *));
  for (i = 0; i < sp->nchans; i++)
    {
      chan_info *cp;
      env_info *ep;
      mix_state *cs;
      cp = sp->chans[i];
      mf->ctr[i] = -1; /* preincremented */
      mf->samples[i] = CURRENT_SAMPLES(cp);
      cs = md->active_mix_state;
      if ((mf->calc != C_ZERO_SOUND) && 
	  (mf->calc != C_ZERO_PEAK) &&
	  (cs->as_built->amp_envs) && 
	  (cs->as_built->amp_envs[i]))
	{
	  ep = env_on_env(cs->as_built->amp_envs[i], cp);
	  mf->eps[i] = ep; /* save for GC */
	}
      else ep = cp->amp_envs[0]; /* mixed-in-sound cp->edit_ctr always 0 */
      mf->samps_per_bin = ep->samps_per_bin;
      if ((mf->calc != C_ZERO_SOUND) && (mf->calc != C_ZERO_PEAK))
	{
	  if (hi == HI_PEAKS)
	    mf->idata[i] = ep->data_max;
	  else mf->idata[i] = ep->data_min;
	}
    }
  return(mf);
}

static void unlist_mix_reader(mix_fd *fd);

static mix_fd *free_mix_fd_almost(mix_fd *mf)
{
  unlist_mix_reader(mf);
  if (mf)
    {
      int i;
      if (mf->lst) {FREE(mf->lst); mf->lst = NULL;}
      if (mf->nxt) {FREE(mf->nxt); mf->nxt = NULL;}
      if (mf->sfs)
	{
	  for (i = 0; i < mf->chans; i++)
	    mf->sfs[i] = free_snd_fd(mf->sfs[i]);
	  FREE(mf->sfs);
	  mf->sfs = NULL;
	}
      if (mf->ctr) {FREE(mf->ctr); mf->ctr = NULL;}
      if (mf->samples) {FREE(mf->samples); mf->samples = NULL;}
      if (mf->scalers) {FREE(mf->scalers); mf->scalers = NULL;}
      if (mf->eps) 
	{
	  for (i = 0; i < mf->chans; i++)
	    mf->eps[i] = free_env_info(mf->eps[i]); 
	  FREE(mf->eps);
	  mf->eps = NULL;
	}
      if (mf->idata) 
	{
	  FREE(mf->idata); 
	  mf->idata = NULL;
	}
      if (mf->segs)
	{
	  for (i = 0; i < mf->chans; i++)
	    if (mf->segs[i])
	      mus_free(mf->segs[i]);
	  FREE(mf->segs);
	  mf->segs = NULL;
	}
      if (mf->srcs) 
	{
	  for (i = 0; i < mf->chans; i++)
	    free_src(mf->srcs[i]);
	  FREE(mf->srcs);
	  mf->srcs = NULL;
	}
      mf->type = MIX_INPUT_INVALID;
      mf->md = NULL;
    }
  return(NULL);
}

static mix_fd *free_mix_fd(mix_fd *mf)
{
  if (mf)
    {
      free_mix_fd_almost(mf);
      FREE(mf);
    }
  return(NULL);
}

/* ---------------- MIXING ---------------- */

static void remove_temporary_mix_file(mix_info *md)
{
  if ((md->temporary == DELETE_ME) &&
      (mus_file_probe(md->in_filename)))
    snd_remove(md->in_filename, REMOVE_FROM_CACHE);
}

void free_mix_list(chan_info *cp)
{
  map_over_channel_mixes(cp, remove_temporary_mix_file);
}

void free_mixes(chan_info *cp)
{
  /* called in snd-data.c during chan_info cleanup */
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	mix_infos[i] = free_mix_info(md);
    }
  cp->have_mixes = false;
}

static char *save_as_temp_file(mus_sample_t **raw_data, int chans, int len, int nominal_srate)
{
  char *newname;
  int format, ofd;
  io_error_t err;
  disk_space_t no_space;
  format = MUS_OUT_FORMAT; /* can be double! */
  newname = shorter_tempnam(temp_dir(ss), "snd_mix_");
                      /* we're writing our own private version of this thing, so we can use our own formats */
  err = snd_write_header(newname, MUS_NEXT, nominal_srate, chans, len * chans, format, NULL, 0, NULL);
  /* Watch out!  28's below are assuming no comment here! */
  if (err != IO_NO_ERROR)
    {
      snd_warning("%s %s: %s", io_error_name(err), newname, snd_io_strerror());
      return(NULL);
    }
  ofd = snd_reopen_write(newname);
  snd_file_open_descriptors(ofd, newname, format, 28, chans, MUS_NEXT);
  lseek(ofd, 28, SEEK_SET);
  no_space = disk_space_p(len * chans * mus_bytes_per_sample(format), newname);
  if (no_space == DISK_SPACE_OK)
    mus_file_write(ofd, 0, len - 1, chans, raw_data);
  if (mus_file_close(ofd) != 0)
    snd_error(_("mix save temp: can't close %s: %s!"), newname, snd_io_strerror());
  return(newname);
}

#define SAMPLE_ENVS_INCR .1
#define OFFSET_FROM_TOP 0
/* axis top border width is 10 (snd-axis.c) */

static void gather_as_built(mix_info *md, mix_state *cs)
{
  mix_track_state *ms;
  Float trk_amp = 1.0;
  int i;
  bool found_scaler = false;
  ms = (mix_track_state *)CALLOC(1, sizeof(mix_track_state));
  ms->chans = cs->chans;
  ms->speed = cs->speed * gather_track_speed(cs->track);
  cs->len = (off_t)(ceil(md->in_samps / ms->speed));
  ms->len = cs->len;
  trk_amp = gather_track_amp(cs);
  ms->scalers = (Float *)CALLOC(ms->chans, sizeof(Float));
  for (i = 0; i < ms->chans; i++)
    {
      ms->scalers[i] = cs->scalers[i] * trk_amp;
      if (ms->scalers[i] != 0.0) found_scaler = true;
    }
  if ((found_scaler) && ((cs->amp_envs) || (cs->track != 0)))
    {
      env *track_env = NULL;
      if (cs->track != 0)
	track_env = gather_track_amp_env(cs);
      ms->amp_envs = (env **)CALLOC(ms->chans, sizeof(env *));
      for (i = 0; i < ms->chans; i++)
	if (cs->scalers[i] != 0.0)
	  {
	    if ((cs->amp_envs) && (cs->amp_envs[i]))
	      {
		if (track_env)
		  ms->amp_envs[i] = multiply_envs(cs->amp_envs[i], track_env, SAMPLE_ENVS_INCR);
		else ms->amp_envs[i] = copy_env(cs->amp_envs[i]);
	      }
	    else 
	      {
		if (track_env)
		  ms->amp_envs[i] = copy_env(track_env);
	      }
	  }
      track_env = free_env(track_env);
    }
  else ms->amp_envs = NULL;
  if (cs->as_built) free_mix_track_state(cs->as_built);
  cs->as_built = ms;
}

static mix_info *add_mix(chan_info *cp, int chan, off_t beg, off_t num, const char *full_original_file, int input_chans, file_delete_t auto_delete)
{ 
  mix_info *md;
  mix_state *cs;
  md = make_mix_info(cp);     /* add active mix to chan_info list */
  md->in_chans = input_chans;
  md->in_samps = num;
  md->temporary = auto_delete;
  md->mix_state_size = 1;
  md->states = (mix_state **)CALLOC(md->mix_state_size, sizeof(mix_state *));
  cs = make_mix_state(input_chans, cp->edit_ctr, beg, beg + num - 1);
  md->active_mix_state = make_mix_state(input_chans, cp->edit_ctr, beg, beg + num - 1);
  md->states[0] = cs;
  md->orig_chan = chan;
  if (chan < input_chans)
    cs->scalers[chan] = 1.0;
  cs->speed = 1.0;
  md->current_state = 0;
  gather_as_built(md, cs);
  make_current_mix_state(md);
  md->in_filename = copy_string(full_original_file);
  return(md);
}

static mix_info *file_mix_samples(off_t beg, off_t num, const char *mixfile, chan_info *cp, int chan, 
				  file_delete_t auto_delete, const char *origin, bool with_tag, 
				  int track_id, bool redisplay)
{
  /* open mixfile, current data, write to new temp file mixed, close others, open and use new as change case */
  /* used for clip-region temp file incoming and C-q in snd-chn.c (i.e. mix in file) so sync not relevant */
  snd_fd *csf = NULL;
  snd_info *sp;
  int ofd, ifd = -1;
  char *ofile, *new_origin = NULL;
  mus_sample_t **data;
  mus_sample_t *chandata;
  int in_chans, err = 0;
  mix_info *md = NULL;
  off_t i, j, len, size;
  file_info *ihdr, *ohdr;
  io_error_t io_err = IO_NO_ERROR;
  if (num <= 0) return(NULL); /* a no-op -- mixing in an empty file */
  if (!(editable_p(cp))) return(NULL);
  len = CURRENT_SAMPLES(cp);
  if ((beg >= len) &&
      (!(extend_with_zeros(cp, len, beg - len + 1, cp->edit_ctr))))
    return(NULL);
  if (beg < 0) beg = 0;
  sp = cp->sound;
  ihdr = make_file_info(mixfile, FILE_READ_ONLY, FILE_NOT_SELECTED);
  if (!ihdr) 
    {
      cp->edit_hook_checked = false;
      return(NULL);
    }
  in_chans = ihdr->chans;
  if (chan >= in_chans)
    {
      free_file_info(ihdr);
      cp->edit_hook_checked = false;
      return(NULL); /* we're reading input[chan] so if chan >= in_chans (no such channel exists) give up */
    }
  ofile = snd_tempnam();
  ohdr = make_temp_header(ofile, SND_SRATE(sp), 1, 0, (char *)origin);
  ofd = open_temp_file(ofile, 1, ohdr, &io_err);
  if (ofd == -1) 
    {
      free_file_info(ihdr);
      cp->edit_hook_checked = false;
      snd_error(_("%s mix temp file %s: %s"), 
		(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
		ofile, 
		snd_open_strerror()); 
      return(NULL);
    }
  if ((disk_space_p(num * mus_bytes_per_sample(ohdr->format), ofile)) == DISK_SPACE_OK)
    csf = init_sample_read(beg, cp, READ_FORWARD);
  if (csf) ifd = snd_open_read(mixfile);
  if ((!csf) ||        /* i.e. no space for temp, I guess */
      (ifd < 0))       /* maybe too many files open? */
    {
      free_file_info(ihdr);
      mus_file_close(ofd);
      snd_remove(ofile, REMOVE_FROM_CACHE);
      FREE(ofile);
      cp->edit_hook_checked = false;
      return(NULL);
    }
  snd_file_open_descriptors(ifd, mixfile,
			    ihdr->format,
			    ihdr->data_location,
			    ihdr->chans,
			    ihdr->type);
  during_open(ifd, mixfile, SND_MIX_FILE);
  if (num < MAX_BUFFER_SIZE) size = num; else size = MAX_BUFFER_SIZE;
  data = (mus_sample_t **)CALLOC(in_chans, sizeof(mus_sample_t *));
  data[chan] = (mus_sample_t *)CALLOC(size, sizeof(mus_sample_t));
  chandata = data[chan];
  lseek(ofd, ohdr->data_location, SEEK_SET);
  lseek(ifd, ihdr->data_location, SEEK_SET);
  mus_file_read_chans(ifd, 0, size - 1, in_chans, data, data);
  for (i = 0, j = 0; i < num; i++)
    {
      if (j == size)
	{
	  err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
	  mus_file_read_chans(ifd, 0, size - 1, in_chans, data, data);
	  j = 0;
	  if (err == -1) break;
	}
      chandata[j] += read_sample(csf);
      j++;
    }
  if (j > 0) mus_file_write(ofd, 0, j - 1, 1, &chandata);
  close_temp_file(ofile, ofd, ohdr->type, num * mus_bytes_per_sample(ohdr->format));
  mus_file_close(ifd);
  csf = free_snd_fd(csf);
  FREE(data[chan]);
  FREE(data);
  free_file_info(ihdr);
  free_file_info(ohdr);
  if ((origin) && (with_tag))
    {
      int next_mix;
      next_mix = pending_mix_id();
#if HAVE_SCHEME
      new_origin = mus_format("set! -mix-%d (%s)", next_mix, origin);
#endif
#if HAVE_RUBY
      new_origin = mus_format("_mix_%d = %s", next_mix, origin);
#endif
#if HAVE_FORTH
      new_origin = mus_format("%s to -mix-%d", origin, next_mix);
#endif
    }
  else new_origin = copy_string(origin);
  file_mix_change_samples(beg, num, ofile, cp, 0, DELETE_ME, DONT_LOCK_MIXES, new_origin, cp->edit_ctr, with_tag); /* editable checked already */
  if (ofile) FREE(ofile);
  if (with_tag)
    {
      md = add_mix(cp, chan, beg, num, mixfile, in_chans, auto_delete);
      if (track_id > 0) 
	{
	  int edpos;
	  edpos = cp->edit_ctr;
	  set_mix_track(md, track_id, false);
	  while (cp->edit_ctr > edpos) backup_edit_list(cp);
	  backup_mix_list(cp, edpos); /* needed if track exists and imposes changes on mixed-in data */
	  if (cp->edits[cp->edit_ctr]->origin) FREE(cp->edits[cp->edit_ctr]->origin);
	  if (new_origin)
	    cp->edits[cp->edit_ctr]->origin = copy_string(new_origin);
	  else cp->edits[cp->edit_ctr]->origin = copy_string(origin);
	}
      reflect_mix_or_track_change(md->id, ANY_TRACK_ID, false);
      after_edit(cp);
    }
  else
    {
      if (auto_delete == DELETE_ME)
	snd_remove(mixfile, REMOVE_FROM_CACHE);
    }
  if (redisplay) update_graph(cp);
  cp->edit_hook_checked = false;
  if (new_origin) FREE(new_origin);
  return(md);
}

/* next functions canonicalize mixer input -- 
 *   all end up with a filename containing the original to-be-mixed input
 *                     length (per channel samples in input)
 *                     begin sample in output for mix start
 *                     an array of cps for mixing into
 *                     a notion of initial scalers
 */

int mix_file(off_t beg, off_t num, int chans, chan_info **cps, const char *mixinfile, file_delete_t temp, const char *origin, bool with_tag, int track_id)
{
  /* loop through out_chans cps writing the new mixed temp files and fixing up the edit trees */
  int id = MIX_FILE_NO_MIX, in_chans;
  mix_info *md;
  char *new_origin = NULL;
  in_chans =  mus_sound_chans(mixinfile);
  if (chans > in_chans) chans = in_chans;
  if (chans > 1)
    {
      int i;
      /* as-one-edit style track op */
      if (track_id == 0)
	track_id = new_track();  /* create a track for this mix to keep all chans sync'd */
      for (i = 0; i < chans; i++) 
	{
	  int edpos;
	  chan_info *cp;
	  cp = cps[i];
	  edpos = cp->edit_ctr + 1;
	  if (!origin)
#if HAVE_FORTH
	    new_origin = mus_format("\"%s\" " OFF_TD " %d %s", 
				    mixinfile, beg, i, S_mix);
#else
	    new_origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP OFF_TD PROC_SEP "%d", 
				    TO_PROC_NAME(S_mix), mixinfile, beg, i);
#endif
	  else new_origin = copy_string(origin);
	  md = file_mix_samples(beg, num, mixinfile, cp, i, temp, new_origin, with_tag, track_id, false);
	  if (md)
	    {
	      if (id == MIX_FILE_NO_MIX) id = md->id;
	      while (cp->edit_ctr > edpos) backup_edit_list(cp);
	      backup_mix_list(cp, edpos);
	      if (cp->edits[cp->edit_ctr]->origin) FREE(cp->edits[cp->edit_ctr]->origin);
	      cp->edits[cp->edit_ctr]->origin = copy_string(new_origin);
	      update_graph(cp);
	    }
	  if (new_origin) {FREE(new_origin); new_origin = NULL;}
	}
    }
  else 
    {
      if (!origin)
#if HAVE_FORTH
	new_origin = mus_format("\"%s\" " OFF_TD " 0 %s", 
				mixinfile, beg, S_mix);
#else
	new_origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP OFF_TD PROC_SEP "0", 
				TO_PROC_NAME(S_mix), mixinfile, beg);
#endif
      else new_origin = copy_string(origin);
      md = file_mix_samples(beg, num, mixinfile, cps[0], 0, temp, new_origin, with_tag, track_id, true);
      if (md) id = md->id;
      if (new_origin) FREE(new_origin);
    }
  return(id);
}

#if HAVE_GUILE_DYNAMIC_WIND
typedef struct {
  off_t beg, len;
  int chans, track_id;
  chan_info **cps;
  const char *fullname;
  bool with_tag;
  file_delete_t auto_delete;
  sync_info *si;
  int old_sync;
  snd_info *sp;
} mix_file_context;

static void before_mix_file(void *ignore) {}

static XEN mix_file_body(void *context)
{
  mix_file_context *mx = (mix_file_context *)context;
  int id;
  id = mix_file(mx->beg, mx->len, mx->chans, mx->cps, mx->fullname, mx->auto_delete, NULL, mx->with_tag, mx->track_id);
  return(C_TO_XEN_INT(id));
}

static void after_mix_file(void *context)
{
  mix_file_context *mx = (mix_file_context *)context;
  if (mx->si) 
    mx->si = free_sync_info(mx->si); 
  else 
    if (mx->cps) 
      FREE(mx->cps);
  mx->sp->sync = mx->old_sync;
  FREE(mx);
}
#endif

int mix_complete_file(snd_info *sp, off_t beg, const char *fullname, bool with_tag, file_delete_t auto_delete, int track_id, bool all_chans)
{
  /* no need to save as temp here, but we do need sync info (from menu and keyboard) */
  /* returns -1 if with_tag is false, -2 if no such file */
  chan_info *cp;
  chan_info **cps = NULL;
  int chans, id = MIX_FILE_NO_MIX, old_sync;
  off_t len;
  sync_info *si = NULL;
  len = mus_sound_frames(fullname);
  if (len < 0) return(MIX_FILE_NO_FILE);
  if (len == 0) return(MIX_FILE_NO_MIX);
  cp = any_selected_channel(sp);
  old_sync = sp->sync;
  if ((old_sync == 0) && (all_chans))
    {
      sp->sync = ss->sound_sync_max + 1;
      ss->sound_sync_max++;
    }
  if (sp->sync != 0)
    {
      si = snd_sync(sp->sync); 
      cps = si->cps;
      chans = si->chans;
    }
  else
    {
      cps = (chan_info **)CALLOC(1, sizeof(chan_info *));
      cps[0] = cp;
      chans = 1;
    }
#if HAVE_GUILE_DYNAMIC_WIND
  {
    mix_file_context *mx;
    XEN result;
    mx = (mix_file_context *)CALLOC(1, sizeof(mix_file_context));
    mx->beg = beg;
    mx->len = len;
    mx->chans = chans;
    mx->track_id = track_id;
    mx->cps = cps;
    mx->fullname = fullname;
    mx->si = si;
    mx->sp = sp;
    mx->with_tag = with_tag;
    mx->auto_delete = auto_delete;
    mx->old_sync = old_sync;
    result = scm_internal_dynamic_wind((scm_t_guard)before_mix_file, 
				       (scm_t_inner)mix_file_body, 
				       (scm_t_guard)after_mix_file, 
				       (void *)mx,
				       (void *)mx);
    id = XEN_TO_C_INT(result);
  }
#else
  id = mix_file(beg, len, chans, cps, fullname, auto_delete, NULL, with_tag, track_id);
  if (si) 
    si = free_sync_info(si); 
  else 
    if (cps) 
      FREE(cps);
  sp->sync = old_sync;
#endif
  return(id);
}

int mix_complete_file_at_cursor(snd_info *sp, const char *str, bool with_tag, int track_id)
{
  if ((sp) && (str) && (*str))
    {
      chan_info *cp;
      int err = 0;
      char *fullname;
      fullname = mus_expand_filename(str);
      cp = any_selected_channel(sp);
      err = mix_complete_file(sp, CURSOR(cp), fullname, with_tag, DONT_DELETE_ME, track_id, false);
      if (err == MIX_FILE_NO_FILE) 
	snd_error("can't mix file: %s, %s", str, snd_io_strerror());
      else
	{
	  if (err == MIX_FILE_NO_MIX) 
	    snd_error("no data to mix in %s", str);
	}
      if (fullname) FREE(fullname);
      return(err);
    }
  return(MIX_FILE_NO_SP);
}

#define MIX_STATE_INCREMENT 8

static void extend_mix_state_list(mix_info *md)
{
  md->current_state++;
  if (md->current_state >= md->mix_state_size)
    {
      int i, lim;
      lim = md->mix_state_size;
      md->mix_state_size += MIX_STATE_INCREMENT;
      md->states = (mix_state **)REALLOC(md->states, md->mix_state_size * sizeof(mix_state *));
      for (i = lim; i < md->mix_state_size; i++) md->states[i] = NULL;
    }
}

/* (as-one-edit (lambda () (set! (mix-position 0) 0) (set! (mix-position 1) 1))) */

static void backup_mix(mix_info *md, int one_edit)
{
  int current_state;
  mix_state *cs, *curcs;
  current_state = md->current_state;
  curcs = md->states[current_state];
  if (curcs->edit_ctr >= one_edit)
    {
      while ((md->current_state > 0) && 
	     ((md->states[md->current_state - 1])->edit_ctr >= one_edit))
	{
	  md->current_state--; 
	  cs = md->states[md->current_state];
	  if (cs) md->states[md->current_state] = free_mix_state(cs); 
	}
      if (md->current_state != current_state)
	{
	  md->states[current_state] = NULL;
	  md->states[md->current_state] = curcs;
	}
      curcs->edit_ctr = one_edit;
    }
}

void backup_mix_list(chan_info *cp, int edit_ctr)
{
  /* we're at md->states[md->current_state] (mix_state) with cs->edit_ctr at value upon local edit */
  /* edit_ctr is the one-edit point for this channel */
  map_over_channel_mixes_with_int(cp, backup_mix, edit_ctr);
}


static void remix_file(mix_info *md, const char *origin, bool redisplay)
{
  off_t beg, end, i, num;
  int j = 0, ofd = 0, size;
  bool use_temp_file;
  Float val = 0.0, maxy, miny;
  snd_info *cursp;
  mix_fd *add = NULL, *sub = NULL;
  snd_fd *cur = NULL, *sfb = NULL, *afb = NULL;
  char *ofile = NULL;
  mus_sample_t **data;
  mus_sample_t *chandata;
  mus_sample_t mval, mmax, mmin;
  file_info *ohdr = NULL;
  axis_info *ap;
  chan_info *cp;
  off_t old_beg, old_end, new_beg, new_end, true_old_beg, true_old_end, true_new_beg, true_new_end;
  int err = 0;
  mix_state *cs;
  if (!(editable_p(md->cp))) return;
  release_pending_mix_states(md);
  cs = md->active_mix_state;
  gather_as_built(md, cs);
  cp = md->cp;
  ap = cp->axis;
  old_beg = cs->orig;
  old_end = cs->end;
  new_beg = cs->beg;
  new_end = cs->beg + cs->len - 1;
  /* save these for special 0-scaler cases below */
  true_old_beg = old_beg;
  true_old_end = old_end;
  true_new_beg = new_beg;
  true_new_end = new_end;
  cursp = cp->sound;
  beg = (old_beg < new_beg) ? old_beg : new_beg;
  end = (old_end > new_end) ? old_end : new_end;
  num = end - beg + 1; /* this is the max size we might need -- for the common 0 scaler case, less will be written */
  old_beg -= beg;
  old_end -= beg;
  new_beg -= beg;
  new_end -= beg;

  maxy = ap->ymax;
  miny = ap->ymin;
  mmax = MUS_SAMPLE_MIN;
  mmin = MUS_SAMPLE_MAX;

#if 0
  fprintf(stderr, "remix file (dpy: %d) origin: \"%s\", mix id: %d,\n        beg: " OFF_TD ", end: " OFF_TD ", old: " OFF_TD " to " OFF_TD ", new: " OFF_TD " to " OFF_TD "\n",
	  redisplay, origin, md->id, beg, end, old_beg, old_end, new_beg, new_end);
#endif

  use_temp_file = (num >= MAX_BUFFER_SIZE);
  if (use_temp_file)
    {
      io_error_t io_err = IO_NO_ERROR;
      disk_space_t no_space;
      ofile = snd_tempnam();
#if MUS_DEBUGGING
      {
	char *info;
	info = mus_format("%s: (remix_file " OFF_TD ", " OFF_TD ", %s, id %d, delete: %d, track: %d)",
			  origin, beg, num, md->in_filename, md->id, (int)(md->temporary), cs->track);
	ohdr = make_temp_header(ofile, SND_SRATE(cursp), 1, 0, info);
	FREE(info);
      }
#else
      ohdr = make_temp_header(ofile, SND_SRATE(cursp), 1, 0, (char *)origin);
#endif
      ofd = open_temp_file(ofile, 1, ohdr, &io_err);
      if (ofd == -1)
	{
	  free_file_info(ohdr);
	  FREE(ofile);
	  cp->edit_hook_checked = false;
	  snd_error(_("%s mix temp file %s: %s\n"), 
		    (io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
		    ofile, 
		    snd_open_strerror());
	  return;
	}
      no_space = disk_space_p(num * mus_bytes_per_sample(ohdr->format), ofile);
      if (no_space != DISK_SPACE_OK)
	{
	  close_temp_file(ofile, ofd, ohdr->type, 0);
	  free_file_info(ohdr);
	  snd_remove(ofile, REMOVE_FROM_CACHE);
	  FREE(ofile);
	  cp->edit_hook_checked = false;
	  return;
	}
      lseek(ofd, ohdr->data_location, SEEK_SET);
    }
  add = init_mix_read(md, CURRENT_MIX, 0);
  if (add) sub = init_mix_read(md, PREVIOUS_MIX, 0);
  if ((add) && (sub))
    {
      if (add->calc == C_ZERO_SOUND)
	{
	  if (sub->calc == C_ZERO_SOUND)
	    {
	      /* not actually a no-op unless true_old_beg == true_new_beg */
	      if (use_temp_file) 
		{
		  close_temp_file(ofile, ofd, ohdr->type, 0);
		  free_file_info(ohdr);
		  snd_remove(ofile, REMOVE_FROM_CACHE);
		}
	      add = free_mix_fd(add);
	      sub = free_mix_fd(sub);
	      cp->edit_hook_checked = false;
	      if (ofile) {FREE(ofile); ofile = NULL;}
	      if (true_new_beg != true_old_beg)
		{
		  /* conjure up a fake edit op to hold the position change */
		  extend_edit_list(cp, cp->edit_ctr);
		  use_temp_file = false;
		  goto REMIX_END;
		}
	      return;
	    }
	  else
	    {
	      beg = true_old_beg;
	      num = true_old_end - true_old_beg + 1;
	    }
	}
      else
	{
	  if (sub->calc == C_ZERO_SOUND)
	    {
	      beg = true_new_beg;
	      num = true_new_end - true_new_beg + 1;
	    }
	}
      /* now "beg" reflects actual changed portion */
      cur = init_sample_read(beg, cp, READ_FORWARD);
    }
  if (!cur)
    {
      cp->edit_hook_checked = false;
      if (use_temp_file) 
	{
	  close_temp_file(ofile, ofd, ohdr->type, 0);
	  free_file_info(ohdr);
	  snd_remove(ofile, REMOVE_FROM_CACHE);
	}
      add = free_mix_fd(add);
      sub = free_mix_fd(sub);
      if (ofile) FREE(ofile);
      return;
    }
  if ((num > 0) && (num < MAX_BUFFER_SIZE)) size = (int)num; else size = MAX_BUFFER_SIZE;
  data = (mus_sample_t **)CALLOC(1, sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)CALLOC(size, sizeof(mus_sample_t));
  chandata = data[0];

  /* these max/min values are only used to reset y-axis limits if overflow occurred */
  /* split out special simple cases */
  if (add->calc == C_ZERO_SOUND)
    {
      /* no add, need sub (current scalers are 0, previous were not) */
      if (sub->calc == C_STRAIGHT_SOUND) sfb = sub->sfs[sub->base];
      for (i = 0, j = 0; i < num; i++)
	{
	  if (j == size)
	    {
	      if (use_temp_file) err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
	      j = 0;
	      if (err == -1) break;
	      /* don't call check_for_event in these loops! can cause nested button motion events and endless trouble */
	    }
	  if (sub->calc == C_STRAIGHT_SOUND) 
	    mval = read_sample(cur) - read_sample(sfb);
	  else mval = MUS_FLOAT_TO_SAMPLE((read_sample_to_float(cur) - next_mix_sample(sub)));
	  if (mval > mmax) mmax = mval;
	  else if (mval < mmin) mmin = mval;
	  chandata[j++] = mval;
	}
    }
  else
    {
      if (sub->calc == C_ZERO_SOUND)
	{
	  /* no sub, need add (current scalers are not zero, previous were) */
	  if (add->calc == C_STRAIGHT_SOUND) afb = add->sfs[add->base];
	  for (i = 0, j = 0; i < num; i++)
	    {
	      if (j == size)
		{
		  if (use_temp_file) err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
		  j = 0;
		  if (err == -1) break;
		}
	      if (add->calc == C_STRAIGHT_SOUND)
		mval = read_sample(cur) + read_sample(afb);
	      else mval = MUS_FLOAT_TO_SAMPLE((read_sample_to_float(cur) + next_mix_sample(add)));
	      if (mval > mmax) mmax = mval;
	      else if (mval < mmin) mmin = mval;
	      chandata[j++] = mval;
	    }
	}
      else
	{
	  if ((add->calc == C_STRAIGHT_SOUND) && (sub->calc == C_STRAIGHT_SOUND))
	    {
	      /* can use direct mix underlying data read here since mix is not applying any change */
	      sfb = sub->sfs[sub->base];
	      afb = add->sfs[add->base];
	      for (i = 0, j = 0; i < num; i++)
		{
		  if (j == size)
		    {
		      if (use_temp_file) err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
		      j = 0;
		      if (err == -1) break;
		    }
		  mval = read_sample(cur);
		  if ((i >= old_beg) && (i <= old_end))
		    mval -= read_sample(sfb);
		  if ((i >= new_beg) && (i <= new_end))
		    mval += read_sample(afb);
		  if (mval > mmax) mmax = mval;
		  else if (mval < mmin) mmin = mval;
		  chandata[j++] = mval;
		}
	    }
	  else
	    {
	      /* mix changes are in effect and neither is 0'd */
	      for (i = 0, j = 0; i < num; i++)
		{
		  if (j == size)
		    {
		      if (use_temp_file) err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
		      j = 0;
		      if (err == -1) break;
		    }
		  val = read_sample_to_float(cur);
		  if ((i >= old_beg) && (i <= old_end))
		    val -= next_mix_sample(sub);
		  if ((i >= new_beg) && (i <= new_end))
		    val += next_mix_sample(add);
		  if (val > maxy) maxy = val;
		  else if (val < miny) miny = val;
		  chandata[j++] = MUS_FLOAT_TO_SAMPLE(val);
		}
	    }
	}
    }

  cur = free_snd_fd(cur);
  add = free_mix_fd(add);
  sub = free_mix_fd(sub);

  if (err != -1)
    {
      bool squelched;
      squelched = cp->squelch_update;
      cp->squelch_update = !redisplay;
      if (use_temp_file)
	{
	  if (j > 0) mus_file_write(ofd, 0, j - 1, 1, &chandata);
	  close_temp_file(ofile, ofd, ohdr->type, num * mus_bytes_per_sample(ohdr->format));
	  free_file_info(ohdr);
	  file_change_samples(beg, num, ofile, cp, 0, DELETE_ME, DONT_LOCK_MIXES, origin, cp->edit_ctr);
	}
      else change_samples(beg, num, data[0], cp, DONT_LOCK_MIXES, origin, cp->edit_ctr);
      cp->squelch_update = squelched;
      if (!squelched)                    /* make the dependency explicit -- saves searching sources... */
	reflect_edit_history_change(cp); /* this may still be squelched if we're in remix_track */
    }

  FREE(data[0]);
  FREE(data);
  cp->edit_hook_checked = false;
  if (ofile) FREE(ofile);

  if (err == -1) return;

 REMIX_END:
  extend_mix_state_list(md);
  cs = copy_mix_state(cs);
  cs->edit_ctr = cp->edit_ctr;
  cs->orig = true_new_beg;
  cs->beg = cs->orig;
  cs->end = cs->beg + cs->len - 1;
  md->states[md->current_state] = cs;
  make_current_mix_state(md);

  /* fix up graph if we overflowed during mix */
  if (redisplay)
    {
      val = MUS_SAMPLE_TO_FLOAT(mmax);
      if (val > maxy) maxy = val;
      val = MUS_SAMPLE_TO_FLOAT(mmin);
      if (val < miny) miny = val;
      if ((maxy > ap->ymax) || (miny < ap->ymin)) 
	{
	  if (maxy < -miny) maxy = -miny; 
	  ap->y0 = -maxy;
	  ap->y1 = maxy;
	  ap->ymin = -maxy;
	  ap->ymax = maxy;
	  ap->y_ambit = (ap->ymax - ap->ymin);
	}
      update_graph(cp);
    }
}


/* ---------------- MIX GRAPHS ---------------- */

/* these are copies from snd-axis.c; didn't want to use macros here */
static Locus local_grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((Locus)(ap->x_base + val * ap->x_scale));
}

static Locus local_grf_y(Float val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((Locus)(ap->y_base + val * ap->y_scale));
}

static int make_temporary_amp_env_mixed_graph(chan_info *cp, axis_info *ap, mix_info *md, Float samples_per_pixel, off_t newbeg, off_t newend)
{
  /* temp graph using cp->amp_env and mix (sample-by-sample) data */
  off_t main_start, new_start;
  mix_fd *new_fd;
  double xi;
  Float xf, xfinc;
  off_t lo, hi, i;
  int j, main_loc;
  Locus lastx;
  env_info *ep;
  lo = ap->losamp;
  hi = ap->hisamp;
  if (newbeg < lo)
    new_fd = init_mix_read(md, CURRENT_MIX, lo - newbeg);
  else new_fd = init_mix_read(md, CURRENT_MIX, 0);
  if (!new_fd) return(0);
  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((double)(ap->losamp) / (double)(ep->samps_per_bin));
  main_start = ap->losamp;
  if ((lo > newbeg) && (lo < newend)) 
    {
      for (i = newbeg; i < lo; i++) next_mix_sample(new_fd);
      new_start = lo;
    }
  else new_start = newbeg;
  lastx = ap->x_axis_x0;
  xfinc = samples_per_pixel / (Float)SND_SRATE(cp->sound);
  for (j = 0, xi = (double)lo, xf = ap->x0; 
       xi < (double)hi; 
       xi += samples_per_pixel, lastx++, xf += xfinc, j++)
    {
      Float val, new_ymin, new_ymax, main_ymin, main_ymax;
      main_ymin = 100.0;
      main_ymax = -100.0;
      new_ymin = 100.0;
      new_ymax = -100.0;
      if ((xi >= newbeg) && (xi < newend))
	{
	  while (new_start <= xi)
	    {
	      val = next_mix_sample(new_fd);
	      if (val > new_ymax) new_ymax = val;
	      if (val < new_ymin) new_ymin = val;
	      new_start++;
	    }
	}
      else
	{
	  new_ymin = 0.0;
	  new_ymax = 0.0;
	}
      while (main_start <= xi)
	{
	  val = MUS_SAMPLE_TO_FLOAT(ep->data_min[main_loc]);
	  if (val < main_ymin) main_ymin = val;
	  val = MUS_SAMPLE_TO_FLOAT(ep->data_max[main_loc]);
	  if (val > main_ymax) main_ymax = val;
	  if (main_loc < (ep->amp_env_size - 1))
	    main_loc++;
	  main_start += ep->samps_per_bin;
	}
      set_grf_points(lastx, j,
		     local_grf_y(main_ymin + new_ymin, ap),
		     local_grf_y(main_ymax + new_ymax, ap));

    }
  erase_and_draw_both_grf_points(md->wg, cp, j);
  free_mix_fd(new_fd);
  return(j);
}

static int make_temporary_amp_env_graph(chan_info *cp, axis_info *ap, mix_info *md, Float samples_per_pixel, off_t newbeg, off_t newend)
{
  /* temp graph using cp->amp_env and mix input amp envs */
  off_t main_start, new_start;
  Float val;
  mix_fd *new_min_fd, *new_max_fd;
  Float new_ymin, new_ymax, main_ymin, main_ymax;
  Float new_high, new_low;
  double xi, xf, xfinc;
  off_t lo, hi, x;
  int main_loc, j;
  Locus lastx;
  env_info *ep;
  lo = ap->losamp;
  hi = ap->hisamp;
  new_min_fd = init_mix_input_amp_env_read(md, LO_PEAKS); 
  new_max_fd = init_mix_input_amp_env_read(md, HI_PEAKS); 
  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((double)(ap->losamp) / (double)(ep->samps_per_bin));
  main_start = ap->losamp;
  if (lo > newbeg) 
    {
      for (x = lo; x < newbeg; x += new_max_fd->samps_per_bin) 
	{
	  new_low = next_mix_sample(new_min_fd);
	  new_high = next_mix_sample(new_max_fd);
	}
      new_ymin = new_low;
      new_ymax = new_high;
      new_start = lo;
    }
  else 
    {
      new_ymin = 0.0;
      new_ymax = 0.0;
      new_start = newbeg;
    }
  lastx = ap->x_axis_x0;
  xfinc = samples_per_pixel / (Float)SND_SRATE(cp->sound);
  for (j = 0, xi = (double)lo, xf = ap->x0; 
       xi < (double)hi; 
       xi += samples_per_pixel, lastx++, xf += xfinc, j++)
    {
      main_ymin = 100.0;
      main_ymax = -100.0;
      new_ymin = 100.0;
      new_ymax = -100.0;
      if ((xi >= newbeg) && (xi < newend))
	{
	  while (new_start <= xi)
	    {
	      new_low = next_mix_sample(new_min_fd);
	      new_high = next_mix_sample(new_max_fd);
	      if (new_high > new_ymax) new_ymax = new_high;
	      if (new_low < new_ymin) new_ymin = new_low;
	      new_start += new_max_fd->samps_per_bin;
	    }
	}
      else
	{
	  new_ymin = 0.0;
	  new_ymax = 0.0;
	}
      while (main_start <= xi)
	{
	  val = MUS_SAMPLE_TO_FLOAT(ep->data_min[main_loc]);
	  if (val < main_ymin) main_ymin = val;
	  val = MUS_SAMPLE_TO_FLOAT(ep->data_max[main_loc]);
	  if (val > main_ymax) main_ymax = val;
	  if (main_loc < (ep->amp_env_size - 1))
	    main_loc++;
	  main_start += ep->samps_per_bin;
	}
      set_grf_points(lastx, j,
		     local_grf_y(main_ymin + new_ymin, ap),
		     local_grf_y(main_ymax + new_ymax, ap));

    }
  erase_and_draw_both_grf_points(md->wg, cp, j);
  free_mix_fd(new_min_fd);
  free_mix_fd(new_max_fd);
  return(j);
}

static void make_temporary_graph(chan_info *cp, mix_info *md, mix_state *cs)
{
  off_t newbeg, newend;
  off_t i, samps;
  int j;
  Locus xi;
  bool widely_spaced;
  axis_info *ap;
  snd_info *sp;
  mix_context *ms;
  Float samples_per_pixel, xf, ina;
  double x, incr, initial_x;
  off_t lo, hi;
  snd_fd *sf = NULL;
  mix_fd *add = NULL;
  int x_start, x_end;
  double start_time, cur_srate;

  /* if fft is being displayed, this does not update it as we drag the mix because the fft-data reader
   * (apply_fft_window in snd-fft.c) reads the current to-be-fft'd data using init_sample_read, and
   * that sees the old data in this case (also if the fft is large enough, it looks for data beyond
   * the current graph right edge, but the mix dragger stops at the edge).
   */
  ms = md->wg;
  if (!(ms->p0)) return;
  newbeg = cs->beg;
  newend = newbeg + cs->len;
  sp = cp->sound;
  ap = cp->axis;
  cur_srate = (double)SND_SRATE(sp);
  start_time = (double)(ap->losamp) / cur_srate;
  x_start = local_grf_x(start_time, ap);
  x_end = local_grf_x((double)(ap->hisamp) / cur_srate, ap);
  lo = ap->losamp;
  hi = ap->hisamp;
  samps = ap->hisamp - ap->losamp + 1;
  samples_per_pixel = (Float)((double)(samps - 1) / (Float)(x_end - x_start));

  if ((samples_per_pixel < 5.0) && 
      (samps < POINT_BUFFER_SIZE))
    {
      sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
      if (sf == NULL) return;
      if (newbeg < lo)
	add = init_mix_read(md, CURRENT_MIX, lo - newbeg);
      else add = init_mix_read(md, CURRENT_MIX, 0); 
      if (!add) return;

      if (samples_per_pixel < 1.0)
	{
	  incr = 1.0 / samples_per_pixel;
	  initial_x = x_start;
	  widely_spaced = true;
	}
      else
	{
	  incr = (double)1.0 / cur_srate;
	  initial_x = start_time;
	  widely_spaced = false;
	}
      for (j = 0, i = lo, x = initial_x; i <= hi; i++, j++, x += incr)
	{
	  ina = read_sample_to_float(sf);

	  if ((i >= newbeg) && (i <= newend)) ina += next_mix_sample(add);
	  if (widely_spaced)
	    set_grf_point((Locus)x, j, local_grf_y(ina, ap));
	  else set_grf_point(local_grf_x(x, ap), j, local_grf_y(ina, ap));
	}
      erase_and_draw_grf_points(md->wg, cp, j);
    }
  else
    {
      if (amp_env_usable(cp, samples_per_pixel, ap->hisamp, true, cp->edit_ctr, (samps > AMP_ENV_CUTOFF)))
	{
	  if (mix_input_amp_env_usable(md, samples_per_pixel))
	    j = make_temporary_amp_env_graph(cp, ap, md, samples_per_pixel, newbeg, newend);
	  else j = make_temporary_amp_env_mixed_graph(cp, ap, md, samples_per_pixel, newbeg, newend);
	}
      else
	{
	  mus_sample_t mina, mymax, mymin;
	  Float ymin, ymax;
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (sf == NULL) return;
	  if (newbeg < lo)
	    add = init_mix_read(md, CURRENT_MIX, lo - newbeg);
	  else add = init_mix_read(md, CURRENT_MIX, 0);
	  if (!add) return;

	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = local_grf_x(x, ap);
	  i = lo;
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = 100.0;
	  ymax = -100.0;

	  if (add->calc == C_ZERO_SOUND)
	    {
	      mymax = MUS_SAMPLE_MIN;
	      mymin = MUS_SAMPLE_MAX;
	      while (i <= hi)
		{
		  mina = read_sample(sf);
		  if (mina > mymax) mymax = mina;
		  if (mina < mymin) mymin = mina;
		  xf += 1.0;
		  i++;
		  if (xf > samples_per_pixel)
		    {
		      set_grf_points(xi, j, 
					 local_grf_y(MUS_SAMPLE_TO_FLOAT(mymin), ap), 
					 local_grf_y(MUS_SAMPLE_TO_FLOAT(mymax), ap));
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      mymax = MUS_SAMPLE_MIN;
		      mymin = MUS_SAMPLE_MAX;
		    }
		}
	    }
	  else
	    {
	      if (add->calc == C_STRAIGHT_SOUND)
		{
		  snd_fd *afb;
		  mymax = MUS_SAMPLE_MIN;
		  mymin = MUS_SAMPLE_MAX;

		  afb = add->sfs[add->base];
		  while (i <= hi)
		    {
		      mina = read_sample(sf);

		      if ((i >= newbeg) && (i <= newend)) mina += read_sample(afb);
		      if (mina > mymax) mymax = mina;
		      if (mina < mymin) mymin = mina;
		      xf += 1.0;
		      i++;
		      if (xf > samples_per_pixel)
			{
			  set_grf_points(xi, j, 
					 local_grf_y(MUS_SAMPLE_TO_FLOAT(mymin), ap), 
					 local_grf_y(MUS_SAMPLE_TO_FLOAT(mymax), ap));
			  xi++;
			  j++;
			  xf -= samples_per_pixel;
			  mymax = MUS_SAMPLE_MIN;
			  mymin = MUS_SAMPLE_MAX;
			}
		    }
		}
	      else
		{
		  while (i <= hi)
		    {
		      ina = read_sample_to_float(sf);

		      if ((i >= newbeg) && (i <= newend)) ina += next_mix_sample(add);
		      if (ina > ymax) ymax = ina;
		      if (ina < ymin) ymin = ina;
		      xf += 1.0;
		      i++;
		      if (xf > samples_per_pixel)
			{
			  set_grf_points(xi, j, local_grf_y(ymin, ap), local_grf_y(ymax, ap));
			  xi++;
			  j++;
			  xf -= samples_per_pixel;
			  ymin = 100.0;
			  ymax = -100.0;
			}
		    }
		}
	    }
	  erase_and_draw_both_grf_points(md->wg, cp, j);
	}
    }
  free_snd_fd(sf);
  free_mix_fd(add);
  ms->lastpj = j;
}

static int prepare_mix_amp_env(mix_info *md, Float scl, int yoff, off_t newbeg, off_t newend, double srate, axis_info *ap)
{
  /* need min and max readers */
  mix_fd *min_fd, *max_fd;
  off_t hi, lo, sum;
  int j;
  Locus lastx, newx;
  Float ymin, ymax, high = 0.0, low = 0.0;
  double xend, xstart, xstep;
  min_fd = init_mix_input_amp_env_read(md, LO_PEAKS);
  max_fd = init_mix_input_amp_env_read(md, HI_PEAKS);
  lo = ap->losamp;
  hi = ap->hisamp;

  /* mix starts at newbeg, current display starts at lo,
     mix goes to newend, current display goes to hi,
     cp->amp_envs[cp->edit_ctr]->samps_per_bin is the original's amp_env step size
     mf->samps_per_bin is the mix input step size
  */
  if (lo > newbeg) 
    {
      for (sum = lo; sum < newbeg; sum += max_fd->samps_per_bin) 
	{
	  low = next_mix_sample(min_fd);
	  high = next_mix_sample(max_fd);
	}
      xstart = ap->x0;
      ymin = low;
      ymax = high;
    }
  else 
    {
      xstart = (double)(newbeg) / srate;
      ymin = 100.0;
      ymax = -100.0;
    }
  if (hi < newend)
    xend = ap->x1;
  else xend = (double)(newend) / srate;
  xstep = (double)(max_fd->samps_per_bin) / srate;
  lastx = local_grf_x(xstart, ap);
  for (j = 0; xstart < xend; xstart += xstep)
    {
      low = next_mix_sample(min_fd);
      high = next_mix_sample(max_fd);
      newx = local_grf_x(xstart, ap);
      if (newx > lastx)
	{
	  set_grf_points(lastx, j,
			 (Locus)(yoff - scl * ymin),
			 (Locus)(yoff - scl * ymax));
	  lastx = newx;
	  j++;
	  ymin = low;
	  ymax = high;
	}
      else
	{
	  if (high > ymax) ymax = high;
	  if (low < ymin) ymin = low;
	}
    }
  free_mix_fd(min_fd);
  free_mix_fd(max_fd);
  return(j);
}

static void draw_mix_tag(mix_info *md)
{
  chan_info *cp;
  int width, height;
  axis_context *ax;
  char *lab = NULL;

#if USE_MOTIF
  #define STRING_Y_OFFSET 3
  #define STRING_HEIGHT 12
#else
  #define STRING_Y_OFFSET -8
  #define STRING_HEIGHT 12
#endif

  if ((md->x == md->tagx) && (md->y == md->tagy)) return; 
  cp = md->cp;

  /* draw the mix tag */
  width = mix_tag_width(ss);
  height = mix_tag_height(ss);
  if (md->tagx > 0)
    {
      ax = erase_context(cp);
      fill_rectangle(ax, md->tagx - width - 1, md->tagy - height / 2 - 1, width + 2, height + STRING_HEIGHT);
    }

  ax = set_mix_waveform_context(cp, md);
  fill_rectangle(ax, md->x - width, md->y - height / 2, width, height);

  /* redraw the mix id underneath the tag */
  set_tiny_numbers_font(cp);
  if (cp->printing) ps_set_tiny_numbers_font();

  if (md->name)
    lab = copy_string(md->name);
  else
    {
      lab = (char *)CALLOC(16, sizeof(char));
      mus_snprintf(lab, 16, "%d", md->id);
    }
  ax = copy_context(cp);
  draw_string(ax, md->x - width, md->y + height + STRING_Y_OFFSET, lab, strlen(lab));
  if (cp->printing) ps_draw_string(cp->axis, md->x - width, md->y + height + STRING_Y_OFFSET, lab);

  if (lab) {FREE(lab); lab = NULL;}
  md->tagx = md->x;
  md->tagy = md->y;
}

static int prepare_mix_waveform(mix_info *md, mix_state *cs, axis_info *ap, Float scl, int yoff, double cur_srate, bool *two_sided)
{
  off_t i, newbeg, newend, endi;
  int j = 0;
  off_t samps;
  Locus xi;
  bool widely_spaced;
  Float samples_per_pixel, xf;
  double x, incr, initial_x;
  Float ina, ymin, ymax;
  off_t lo, hi;
  mix_fd *add = NULL;
  int x_start, x_end;
  double start_time;
  newbeg = cs->beg;
  newend = newbeg + cs->len;
  lo = ap->losamp;
  hi = ap->hisamp;
  if ((newend <= lo) || (newbeg >= hi)) return(0);
  if ((ap->y_axis_y0 - ap->y_axis_y1) < scl) return(0);
  start_time = (double)(ap->losamp) / cur_srate;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  if (newend > hi) newend = hi;
  samps = ap->hisamp - ap->losamp + 1;
  samples_per_pixel = (Float)((double)(samps - 1) / (Float)(x_end - x_start));

  if ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE))
    {
      if (newbeg < lo)
	{
	  add = init_mix_read(md, CURRENT_MIX, lo - newbeg);
	  newbeg = lo;
	}
      else add = init_mix_read(md, CURRENT_MIX, 0);
      if (!add) return(0);
      if (samples_per_pixel < 1.0)
	{
	  incr = 1.0 / samples_per_pixel;
	  initial_x = x_start;
	  widely_spaced = true;
	}
      else
	{
	  incr = (double)1.0 /cur_srate;
	  initial_x = start_time;
	  widely_spaced = false;
	}
      x = initial_x + (incr * (newbeg - lo));
      for (j = 0, i = newbeg; i <= newend; i++, j++, x += incr)
	{
	  ina = next_mix_sample(add);
	  if (widely_spaced)
	    set_grf_point((Locus)x, j, (Locus)(yoff - scl * ina));
	  else set_grf_point(local_grf_x(x, ap), j, (Locus)(yoff - scl * ina));
	}
      (*two_sided) = false;
    }
  else
    {
      (*two_sided) = true;
      if (mix_input_amp_env_usable(md, samples_per_pixel))
	j = prepare_mix_amp_env(md, scl, yoff, newbeg, newend, (double)cur_srate, ap);
      else
	{
	  if (newbeg < lo)
	    {
	      add = init_mix_read(md, CURRENT_MIX, lo - newbeg);
	      newbeg = lo;
	    }
	  else add = init_mix_read(md, CURRENT_MIX, 0);
	  if (!add) return(0);

	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = local_grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = 100.0;
	  ymax = -100.0;
	  if (newend < hi) endi = newend; else endi = hi;
	  for (i = lo; i < newbeg; i++)
	    {
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  xi++;
		  xf -= samples_per_pixel;
		}
	    }
	  for (i = newbeg; i <= endi; i++)
	    {

	      ina = next_mix_sample(add);
	      if (ina > ymax) ymax = ina;
	      if (ina < ymin) ymin = ina;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j,
				 (Locus)(yoff - scl * ymin),
				 (Locus)(yoff - scl * ymax));
		  j++;
		  ymin = 100.0;
		  ymax = -100.0;
		  xi++;
		  xf -= samples_per_pixel;
		}
	    }
	}
    }
  free_mix_fd(add);
  return(j);
}

int prepare_mix_id_waveform(int mix_id, axis_info *ap, bool *two_sided)
{
  mix_info *md;
  mix_state *cs;
  Float scl, x0, x1, y0, y1;
  off_t old_lo, old_hi;
  double cur_srate;
  int pts;
  md = md_from_id(mix_id);
  if (!md) return(0);
  scl = ap->y_axis_y0 - ap->y_axis_y1;
  old_lo = ap->losamp;
  old_hi = ap->hisamp;
  x0 = ap->x0;
  x1 = ap->x1;
  y0 = ap->y0;
  y1 = ap->y1;
  cur_srate = (double)SND_SRATE(md->cp->sound);
  cs = md->active_mix_state;
  ap->losamp = cs->beg;
  ap->hisamp = cs->beg + cs->len;
  ap->x0 = (double)(ap->losamp) / cur_srate;
  ap->x1 = (double)(ap->hisamp) / cur_srate;
  ap->y0 = -1.0;
  ap->y1 = 1.0;
  init_axis_scales(ap);
  pts = prepare_mix_waveform(md, cs, ap, scl * .5, (int)(scl * .5), cur_srate, two_sided);
  ap->x0 = x0;
  ap->x1 = x1;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->losamp = old_lo;
  ap->hisamp = old_hi;
  init_axis_scales(ap);
  return(pts);
}

static Cessator watch_mix_proc = 0;    /* work proc if mouse outside graph causing axes to move */

static void display_mix_waveform(chan_info *cp, mix_info *md, bool draw)
{
  int j = 0;
  mix_state *cs;
  axis_info *ap;
  axis_context *ax;
  double cur_srate;
  bool two_sided = false;
  if (!(cp->sound)) return;
  ap = cp->axis;
  cs = md->active_mix_state;
  cur_srate = (double)SND_SRATE(cp->sound);
  j = prepare_mix_waveform(md, cs, ap, md->height, md->y, cur_srate, &two_sided);
  if (j == 0) return;
  if ((draw) && (!(watch_mix_proc)))
    {
      int xx;
      xx = local_grf_x((double)(cs->beg + md->tag_position) / cur_srate, ap);
      md->x = xx;
      draw_mix_tag(md);
    }
  if (draw)
    ax = set_mix_waveform_context(cp, md);
  else ax = erase_context(cp);
  if (two_sided)
    draw_both_grf_points(cp->dot_size, ax, j, cp->time_graph_style);
  else draw_grf_points(cp->dot_size, ax, j, ap, ungrf_y(ap, md->y), cp->time_graph_style);
  copy_context(cp);
}



/* -------------------------------- moving mix tags -------------------------------- */

typedef struct {int *xs; int orig, x; int *edpos;} track_graph_t;
static track_graph_t *track_drag_data;
static track_graph_t *track_save_graph(mix_info *orig_md, int track_id);
static void finish_dragging_track(int track_id, track_graph_t *data);
static void move_mix(mix_info *md);
static void move_track(int track_id, track_graph_t *data);
static track_graph_t *free_track_graph(track_graph_t *ptr);
static bool mix_dragged = false;

static void clear_mix_tags_1(mix_info *md)
{
  md->tagy = 0;
  md->tagx = 0;
}

void clear_mix_tags(chan_info *cp)
{
  map_over_channel_mixes(cp, clear_mix_tags_1);
}

static void clear_mix_y_1(mix_info *md)
{
  md->y = 0;
}

void clear_mix_y(chan_info *cp)
{
  map_over_channel_mixes(cp, clear_mix_y_1);
}

static void wrap_mix_save_graph(mix_info *md, const char *origin)
{
  mix_state *cs, *old_cs;
  int k;
  md->orig_edit_ctr = md->cp->edit_ctr;
  cs = md->active_mix_state;
  old_cs = copy_mix_state(cs);
  for (k = 0; k < md->in_chans; k++) 
    cs->scalers[k] = 0.0;
  remix_file(md, origin, true); /* may be no-op */
  mix_save_graph(md->wg, make_graph(md->cp));
  free_mix_state(md->active_mix_state);
  md->active_mix_state = old_cs;
  old_cs->edit_ctr = md->cp->edit_ctr;
}

void move_mix_tag(int mix_tag, int x)
{
  /* from mouse tag drag in snd-chn.c only */
  mix_info *md;

  md = md_from_id(mix_tag);
  if (!md) return;

  mix_dragged = true;
  if (md->active_mix_state->track == 0)
    {
      if (md->save_needed)
	{
	  wrap_mix_save_graph(md, "drag mix");
	  md->save_needed = false;
	}
      md->x = x;
      move_mix(md);
    }
  else
    {
      /* track drag */
      md->save_needed = false;
      if (track_drag_data == NULL)
	track_drag_data = track_save_graph(md, md->active_mix_state->track); /* calls mix_save_graph */
      track_drag_data->x = x;
      move_track(md->active_mix_state->track, track_drag_data);
    }
}

void finish_moving_mix_tag(int mix_tag, int x)
{
  /* from mouse release after tag drag in snd-chn.c only */
  mix_info *md;
  mix_state *cs = NULL;
  XEN res = XEN_FALSE;
  if (watch_mix_proc) 
    {
      BACKGROUND_REMOVE(watch_mix_proc);
      watch_mix_proc = 0;
    }
  md = md_from_id(mix_tag);
  if (!md) return;
  cs = md->active_mix_state;
  mix_dragged = false;
  if (XEN_HOOKED(mix_release_hook))
    res = run_progn_hook(mix_release_hook,
			 XEN_LIST_2(C_TO_XEN_INT(md->id),
				    C_TO_XEN_OFF_T(cs->beg - cs->orig)),
			 S_mix_release_hook);
  if (md->active_mix_state->track == 0)
    {
      mix_context *ms;
      md->x = x;
      ms = md->wg;
      ms->lastpj = 0;
      if (cs->beg == cs->orig) return;
      reflect_mix_or_track_change(md->id, ANY_TRACK_ID, false);
      if (!(XEN_TRUE_P(res)))
	{
	  char *origin;
#if HAVE_FORTH
	  origin = mus_format(" -mix-%d " OFF_TD " set-%s",
			      md->id, cs->beg, S_mix_position);
#else
	  origin = mus_format(PROC_SET_MIX OFF_TD PROC_CLOSE, 
			      TO_PROC_NAME(S_mix_position), md->id, cs->beg);
#endif
	  remix_file(md, origin, false); /* may be no-op */
	  FREE(origin);
	  if (md->orig_edit_ctr < (md->cp->edit_ctr - 1))
	    {
	      backup_edit_list(md->cp);
	      backup_mix_list(md->cp, md->cp->edit_ctr);
	    }
	  update_graph(md->cp);
	}
    }
  else
    {
      if (track_drag_data)
	{
	  track_drag_data->x = x;
	  finish_dragging_track(md->active_mix_state->track, track_drag_data);
	  track_drag_data = free_track_graph(track_drag_data);
	}
    }
}

#define SLOPPY_MOUSE 2
static int hit_mix_1(mix_info *md, void *uvals)
{
  /* only from hit_mix */
  int *vals = (int *)uvals;
  int mx, my, width, height;
  width = mix_tag_width(ss);
  height = mix_tag_height(ss);
  mx = md->x - width;
  my = md->y - height / 2;
  if ((vals[0] + SLOPPY_MOUSE >= mx) && (vals[0] - SLOPPY_MOUSE <= (mx + width)) &&
      (vals[1] + SLOPPY_MOUSE >= my) && (vals[1] - SLOPPY_MOUSE <= (my + height)))
    return(md->id + 1);
  return(0);
}

int hit_mix(chan_info *cp, int x, int y)
{
  /* from mix tag press in snd-chn.c only */
  int xy[2];
  int mx;
  xy[0] = x;
  xy[1] = y;
  mx = map_over_channel_mixes_with_void(cp, hit_mix_1, (void *)xy);
  if (mx > 0)
    {
      mix_info *md;
      md = md_from_id(mx - 1);
      if (!md) return(NO_MIX_TAG);
      if (md->active_mix_state->track == 0)
	{
	  md->save_needed = true;
	}
      else
	{
	  track_drag_data = NULL;
	  md->save_needed = true;
	}
      return(mx - 1);
    }
  return(NO_MIX_TAG);
}

static Cessate watch_mix(Indicium m)
{
  if (watch_mix_proc)
    {
      move_mix((mix_info *)m);
      return(BACKGROUND_CONTINUE);
    }
  else return(BACKGROUND_QUIT);
}

void mix_dialog_start_drag(int mix_id)
{
  /* the "drag" here refers to the mix dialog amp or speed control (from drag callbacks) */
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    md->save_needed = true;
}

/* for axis movement (as in mark drag off screen) */

static void move_mix(mix_info *md)
{
  axis_info *ap;
  chan_info *cp;
  mix_state *cs;
  int nx, x;
  bool updated = false;

  cp = md->cp;
  if (!cp) return;
  ap = cp->axis;
  if (!ap) return;
  x = md->x;
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (watch_mix_proc)
	{
	  if ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) return;
	  if ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) return;
	}
      nx = move_axis(cp, ap, x); /* calls update_graph eventually (in snd-chn.c reset_x_display) */
      updated = true;
      if ((mix_dragged) && (!watch_mix_proc))
	{
	  md->x = x; /* might have been reset by move_axis->display_mix_waveform with watch_mix_proc 0 (not yet started) */
	  watch_mix_proc = BACKGROUND_ADD(watch_mix, md);
	}
    }
  else 
    {
      nx = x;
      if (watch_mix_proc) 
	{
	  BACKGROUND_REMOVE(watch_mix_proc);
	  watch_mix_proc = 0;
	}
    }
  cs = md->active_mix_state;
  if (md->nx != nx)
    {
      off_t samps, samp;
      erase_mix_waveform(md);
      md->nx = nx;
      samp = (off_t)(ungrf_x(ap, nx) * SND_SRATE(cp->sound));
      if (samp < md->tag_position) samp = md->tag_position;
      if ((md->track_tag_position > 0) && (samp < md->track_tag_position)) samp = md->track_tag_position;
      samps = CURRENT_SAMPLES(cp);
      if (samp > samps) samp = samps;
      /* now redraw the mix and reset its notion of begin time */
      /* actually should make a new state if cp->edit_ctr has changed ?? */
      cs->beg = samp - md->tag_position;
      if (cs->beg < 0) cs->beg = 0; 
      reflect_mix_or_track_change(md->id, ANY_TRACK_ID, false);
      draw_mix_waveform(md);
      /* can't easily use work proc here because the erasure gets complicated */
      make_temporary_graph(cp, md, cs);
      if (XEN_HOOKED(mix_drag_hook))
	run_hook(mix_drag_hook,
		 XEN_LIST_1(C_TO_XEN_INT(md->id)),
		 S_mix_drag_hook);
    }
  else
    {
      if (updated) 
	make_temporary_graph(cp, md, md->active_mix_state);
    }
}



/* ---------------- MIX ACTIONS ---------------- */

static mix_info *active_mix(chan_info *cp)
{
  mix_info *curmd = NULL;
  axis_info *ap;
  off_t lo, hi, spot;
  int i, curmaxctr = -1;
  ap = cp->axis;
  lo = ap->losamp;
  hi = ap->hisamp;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  mix_state *cs;
	  cs = md->active_mix_state;
	  spot = cs->orig + md->tag_position;
	  if ((spot >= lo) && 
	      (spot <= hi) &&
	      /* this mix is within current graph window -- look for last edited mix */
	      (cs->edit_ctr > curmaxctr))
	    {
	      curmaxctr = cs->edit_ctr;
	      curmd = md;
	    }
	}
    }
  return(curmd);
}

bool active_mix_p(chan_info *cp) {return(active_mix(cp) != NULL);}

off_t mix_beg(chan_info *cp)
{
  /* used in snd-chn.c for zoom focus active */
  mix_info *md;
  mix_state *cs;
  md = active_mix(cp);
  if (md) 
    {
      cs = md->active_mix_state;
      if (cs) return(cs->orig + md->tag_position);
    }
  return(INVALID_MIX_ID);
}

static mix_state *backup_mix_state(chan_info *cp, mix_info *md)
{
  mix_state *cs;
  /* undo -- look for first (last in list) cs with cs->edit_ctr <= cp->edit_ctr mix_state */
  cs = md->states[md->current_state];
  /*
  fprintf(stderr, "backup mix: mix %d > cp %d?\n", cs->edit_ctr, cp->edit_ctr);
  */
  while ((md->current_state > 0) && (cs->edit_ctr > cp->edit_ctr)) 
    {
      md->current_state--; 
      cs = md->states[md->current_state];
    }
  if (cs->edit_ctr > cp->edit_ctr) return(NULL);
  make_current_mix_state(md);
  return(md->active_mix_state);
}

static mix_state *restore_mix_state(chan_info *cp, mix_info *md)
{
  mix_state *cs;
  /* redo -- looking for the mix_state that brackets cp->edit_ctr */
  cs = md->states[md->current_state];
  while ((md->current_state < (md->mix_state_size - 1)) && 
	 (cs->edit_ctr < cp->edit_ctr) && 
	 (md->states[md->current_state + 1]))
    {
      md->current_state++; 
      cs = md->states[md->current_state];
    }
  if ((md->current_state > 0) && 
      (cs->edit_ctr > cp->edit_ctr)) 
    md->current_state--;
  make_current_mix_state(md);
  return(md->active_mix_state);
}

void reset_mix_graph_parent(chan_info *cp)
{
  /* if channel style (output drawing area widget) changed, reflect that in all cp's mixes */
  if (cp)
    {
      int i;
      for (i = 0; i < mix_infos_ctr; i++)
	{
	  mix_info *md;
	  md = mix_infos[i];
	  if ((md) && (md->cp == cp))
	    {
	      if (md->wg) FREE(md->wg);
	      md->wg = set_mix_info_context(cp);
	    }
	}
    }
}

static int track_tag_y(int trk);

void sync_mixes_with_edits(chan_info *cp)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  mix_state *cs;
	  cs = md->active_mix_state;
	  if (cs->edit_ctr > cp->edit_ctr) 
	    cs = backup_mix_state(cp, md);
	  else
	    if (cs->edit_ctr < cp->edit_ctr)
	      cs = restore_mix_state(cp, md);
	}
    }
}

void display_channel_mixes(chan_info *cp)
{
  /* called in display_channel_data if show_mix_mix_states(ss) and cp->have_mixes
   * this needs to spin through the mixes, 
   *   un-manage those whose mix has wandered off screen
   *   and re-manage those that are now active
   */
  axis_info *ap;
  off_t lo, hi;
  int i, turnover, y, hgt;
  bool combined;
  combined = (cp->sound->channel_style == CHANNELS_SUPERIMPOSED);
  ap = cp->axis;
  lo = ap->losamp;
  hi = ap->hisamp;
  turnover = (int)(ap->height * 0.4);
  y = mix_tag_height(ss);
  hgt = y;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  mix_state *cs;
	  cs = md->active_mix_state;
	  if (cs->edit_ctr > cp->edit_ctr) 
	    cs = backup_mix_state(cp, md);
	  else
	    if (cs->edit_ctr < cp->edit_ctr)
	      cs = restore_mix_state(cp, md);
	  if ((cs) && (!cs->locked))
	    {
	      off_t spot;
	      spot = cs->orig + md->tag_position;
	      if ((cs->edit_ctr <= cp->edit_ctr) && (spot >= lo) && (spot <= hi))
		{
		  int xspot;
		  xspot = local_grf_x((double)spot / (double)SND_SRATE(cp->sound), ap);

		  if ((cs->track > 0) &&                /* track tag-y takes precedence over mix tag-y */
		      (track_tag_y(cs->track) != 0))
		    md->y = OFFSET_FROM_TOP + ap->y_offset + track_tag_y(cs->track);
		  else
		    {
		      if (md->tag_y != 0)
			md->y = OFFSET_FROM_TOP + ap->y_offset + md->tag_y;
		      else
			{
			  /* this used to check md->y == 0, but that messes up combined channel display */
			  md->y = OFFSET_FROM_TOP + y + ap->y_offset;
			}
		    }
		  if (((xspot + mix_tag_width(ss)) <= ap->x_axis_x1) &&      /* not cut off on right */
		      (!combined))
		    {
		      draw_mix_waveform(md);
		      if (md->tag_y == 0)
			{
			  y += hgt;
			  if (y >= turnover) y = hgt;
			}
		    }
		}
	    }
	}
    }
}

typedef struct {off_t lt_beg, lt_end;} lock_mixes_t;

static int lock_affected_mixes_1(mix_info *md, void *ptr)
{
  mix_state *cs;
  lock_mixes_t *times = (lock_mixes_t *)ptr;
  /* find affected mix_state, extend and lock */
  cs = md->states[md->current_state];
  if (!(cs->locked))
    {
      off_t old_beg;
      old_beg = cs->beg;
      /* fprintf(stderr,"lock? " OFF_TD " < " OFF_TD " and " OFF_TD " > " OFF_TD "\n",cs->orig ,times->lt_end, cs->end, times->lt_beg); */
      if ((cs->orig < times->lt_end) && (cs->end > times->lt_beg))
	{
	  chan_info *cp;
	  extend_mix_state_list(md);
	  if (md->states[md->current_state]) free_mix_state(md->states[md->current_state]);
	  md->states[md->current_state] = (mix_state *)CALLOC(1, sizeof(mix_state));
	  cs = md->states[md->current_state];
	  cs->locked = true;
	  cs->beg = old_beg;
	  cp = md->cp;
	  cs->edit_ctr = cp->edit_ctr;
	  cs = md->active_mix_state;
	  cs->locked = true;
	}
    }
  return(0);
}

void lock_affected_mixes(chan_info *cp, off_t beg, off_t end)
{
  /* search through mix list for any active (cp-based) mixes that overlap beg..end 
   * and lock them down until possible undo.
   *
   * We need to lock down any active mixes that overlap the current
   * edit because any number of subsequent edits can take place, 
   * so to unmix (e.g. to move the mix) we'd either need to keep track 
   * of the complete path throughout and be able to unravel it, or 
   * backup the tree, unmix, remix, then re-run the subsequent edits --
   * this would become a nightmare upon a subsequent undo.  Also there
   * are ambiguities in how we interpret this action: for example,
   * if the user mixes, then deletes a portion of the mixed portion,
   * then moves the original mix, how should it behave in the deleted
   * portion?  
   */
  lock_mixes_t lt;
  lt.lt_beg = beg;
  lt.lt_end = end;
  map_over_channel_mixes_with_void(cp, lock_affected_mixes_1, (void *)(&lt));
}

void release_pending_mixes(chan_info *cp, int edit_ctr)
{
  /* look for mixes that have been cancelled by undo followed by unrelated edit */
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  mix_state *cs;
	  cs = md->states[0];
	  if (cs->edit_ctr >= edit_ctr) free_mix_info(md);
	}
    }
}

static void update_mix(mix_info *md)
{
  mix_state *cur;
  int i, lim;
  lim = md->current_state;
  cur = md->states[lim];
  /* make states[0] and [1] both look like cur, collapse rest */
  if (lim > 0)
    {
      for (i = 0; i < lim; i++)
	md->states[i] = free_mix_state(md->states[i]);
      md->states[0] = cur;
      md->states[lim] = NULL;
      md->current_state = 0;
    }
  cur->edit_ctr = 0;
  md->states[0]->edit_ctr = 0;
  make_current_mix_state(md);
}

void reset_mix_list(chan_info *cp)
{
  map_over_channel_mixes(cp, update_mix);
}



/* ---------------- paste/delete move affected mixes ---------------- */

typedef struct {
  off_t beg, change;
  chan_info *cp;
  axis_info *ap;
} mixrip;

static bool ready_mix(mix_info *md)
{
  mix_state *cs;
  chan_info *cp;
  cp = md->cp;
  cs = md->states[md->current_state];
  return(((cs) && (!(cs->locked)) && 
	  (cs->edit_ctr <= cp->edit_ctr)));
}

static int ripple_mixes_1(mix_info *md, void *ptr)
{
  mix_state *cs;
  mixrip *data;
  chan_info *cp;
  data = (mixrip *)ptr;
  cp = data->cp;
  cs = md->active_mix_state;
  release_pending_mix_states(md);
  if ((cs) && (!(cs->locked)) && (cs->beg > data->beg) && (ready_mix(md)))
    {
      mix_state *ncs;
      ncs = copy_mix_state(cs);
      ncs->edit_ctr = cp->edit_ctr;
      ncs->orig = cs->beg + data->change;
      ncs->beg = ncs->orig;
      ncs->end = ncs->beg + cs->len - 1;
      erase_mix_waveform(md);
      extend_mix_state_list(md);
      if (md->states[md->current_state]) free_mix_state(md->states[md->current_state]);
      md->states[md->current_state] = ncs;
      make_current_mix_state(md);
      draw_mix_waveform(md);
    }
  return(0);
}

void ripple_mixes(chan_info *cp, off_t beg, off_t change)
{
  /* look for active mixes in cp, fixup pointers and move along with edit_ctr */
  /* if they have a mixer, move it too */
  if ((cp) && (cp->have_mixes))
    {
      mixrip *mp;
      mp = (mixrip *)CALLOC(1, sizeof(mixrip));
      mp->beg = beg;
      mp->change = change;
      mp->cp = cp;
      mp->ap = cp->axis;
      map_over_channel_mixes_with_void(cp, ripple_mixes_1, (void *)mp);
      FREE(mp);
    }
}


/* ---------------- C-x C-j (go to mix) ---------------- */

static int compare_mix_states(const void *umx1, const void *umx2)
{
  off_t mx1, mx2;
  mx1 = (*((off_t *)umx1));
  mx2 = (*((off_t *)umx2));
  if (mx1 > mx2) return(1);
  if (mx1 == mx2) return(0);
  return(-1);
}

void goto_mix(chan_info *cp, int count)
{
  int i, j, k;
  mix_info *md;
  if ((!cp) || (!cp->have_mixes)) return;
  k = 0;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp)) k++;
    }
  if (k > 0)
    {
      off_t *css;
      css = (off_t *)CALLOC(k, sizeof(off_t));
      j = 0;
      for (i = 0; i < mix_infos_ctr; i++)
	{
	  md = mix_infos[i];
	  if ((md) &&
	      (md->cp == cp) && 
	      (md->states) && 
	      (md->states[0]) && 
	      (((md->states[0])->edit_ctr) <= cp->edit_ctr))
	    {
	      mix_state *cs;
	      cs = md->active_mix_state;
	      if (!(cs->locked))
		{
		  css[j] = cs->orig;
		  j++;
		}
	    }
	}
      if (j == 0) 
	{
	  FREE(css);
	  return;
	}
      if (j == 1)
	{
	  cursor_moveto(cp, css[0]);
	  FREE(css);
	  return;
	}
      qsort((void *)css, j, sizeof(off_t), compare_mix_states);
      /* now find where we are via CURSOR(cp) and go forward or back as per count */
      if (count > 0)
	{
	  for (i = 0; i < j; i++)
	    if (css[i] > CURSOR(cp))
	      {
		count--;
		if (count == 0)
		  {
		    cursor_moveto(cp, css[i]);
		    break;
		  }
	      }
	  if ((count > 0) && (CURSOR(cp) < css[j - 1]))
	    cursor_moveto(cp, css[j - 1]);
	}
      else
	{
	  for (i = j - 1; i >= 0; i--)
	    if (css[i] < CURSOR(cp))
	      {
		count++;
		if (count == 0)
		  {
		    cursor_moveto(cp, css[i]);
		    break;
		  }
	      }
	  if ((count < 0) && (CURSOR(cp) > css[0]))
	    cursor_moveto(cp, css[0]);
	}
      FREE(css);
    }
}

int any_mix_id(void)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++) 
    if (mix_ok_and_unlocked(i)) 
      return(i);
  return(INVALID_MIX_ID);
}

int next_mix_id(int id)
{
  int i;
  for (i = id + 1; i < mix_infos_ctr; i++) 
    if (mix_ok_and_unlocked(i)) 
      return(i);
  return(INVALID_MIX_ID);
}

int previous_mix_id(int id)
{
  int i, top;
  top = id - 1;
  if (top >= mix_infos_ctr) top = mix_infos_ctr - 1;
  for (i = top; i >= 0; i--) 
    if (mix_ok_and_unlocked(i)) 
      return(i);
  return(INVALID_MIX_ID);
}

static void draw_mix_waveform(mix_info *md) 
{
  if ((md->cp->show_mix_waveforms) && (!(md->cp->squelch_update)))
    display_mix_waveform(md->cp, md, true);
}

static void erase_mix_waveform(mix_info *md) 
{
  if ((md->cp->show_mix_waveforms) && (!(md->cp->squelch_update)))
    display_mix_waveform(md->cp, md, false);
}


#if MUS_MAC_OSX
  #define OutSample float
  #define MUS_CONVERT(samp) samp
#else
  #define OutSample short
  #define MUS_CONVERT(samp) MUS_SAMPLE_TO_SHORT(MUS_FLOAT_TO_SAMPLE(samp))
#endif

static void play_mix(mix_info *md, off_t beg, bool from_gui)
{
  chan_info *cp;
  snd_info *sp;
  mix_fd *mf;
  mix_state *cs;
#if HAVE_ALSA
  mus_sample_t **buf;
  char *outbuf;
  float val[4];
  int err;
#else
  OutSample *buf;
#endif
  int play_fd, j, format, datum_bytes, outchans, frames;
  off_t i, samps;
  if (md == NULL) return;
  cp = md->cp;
  sp = cp->sound;
  cs = md->active_mix_state;
  samps = cs->len;
  format = mus_audio_compatible_format(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss));
  datum_bytes = mus_bytes_per_sample(format);
  outchans = 1;
  frames = 256;

#if HAVE_ALSA
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_CHANNEL, 2, val);
  if (outchans < (int)(val[1])) outchans = (int)(val[1]);
  if ((err != MUS_NO_ERROR) || (outchans <= 0))
    {
      clear_minibuffer(sp);
      snd_error_without_format(_("can't get basic soundcard info!"));
      return;
    }
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, val);
  frames = (int)(val[0]);
  if ((err != MUS_NO_ERROR) || (frames <= 0))
    {
      clear_minibuffer(sp);
      snd_error(_("samples per channel is %d?"), frames);
      return;
    }
  set_dac_size(outchans * frames * mus_bytes_per_sample(format));
  buf = (mus_sample_t **)CALLOC(outchans, sizeof(mus_sample_t *));
  buf[0] = (mus_sample_t *)CALLOC(frames, sizeof(mus_sample_t));
  outbuf = (char *)CALLOC(frames * datum_bytes * outchans, sizeof(char));
#else
  buf = (OutSample *)CALLOC(frames, sizeof(OutSample));
#endif

  play_fd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss),
				  SND_SRATE(sp),
				  outchans, 
				  format, 
				  dac_size(ss));
  if (play_fd != -1)
    {
      clear_minibuffer(sp);
      mf = init_mix_read(md, CURRENT_MIX, beg);
      if (mf)
	{
	  ss->stopped_explicitly = false;
	  samps -= beg;
	  for (i = 0; i < samps; i += frames)
	    {
#if HAVE_ALSA
	      for (j = 0; j < frames; j++)
		buf[0][j] = MUS_FLOAT_TO_SAMPLE(next_mix_sample(mf));
	      mus_file_write_buffer(format, 0, frames - 1, outchans, buf, outbuf, true);
	      mus_audio_write(play_fd, outbuf, frames * datum_bytes * outchans);
#else
	      for (j = 0; j < frames; j++) 
		buf[j] = MUS_CONVERT(next_mix_sample(mf));
	      mus_audio_write(play_fd, (char *)buf, frames * datum_bytes);
#endif
	      check_for_event();
	      if ((ss->stopped_explicitly) || 
		  ((from_gui) && (mix_play_stopped())) || 
		  (!(sp->active)))
		{
		  ss->stopped_explicitly = false;
		  string_to_minibuffer(sp, _("stopped"));
		  break;
		}
	    }
	  mus_audio_close(play_fd);
	  play_fd = -1;
	}
      mf = free_mix_fd(mf);
    }
  reflect_mix_play_stop();
#if HAVE_ALSA
  FREE(buf[0]);
  FREE(outbuf);
#endif
  FREE(buf);
}

void mix_dialog_mix_play(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    play_mix(md, 0, true);
}

static mix_state *cs_from_id(int n)
{
  mix_info *md;
  md = md_from_id(n);
  if (md) 
    return(md->active_mix_state);
  return(NULL);
}

off_t mix_frames(int n) 
{
  mix_state *cs; 
  cs = cs_from_id(n); 
  if (cs) 
    return(cs->len); 
  return(INVALID_MIX_ID);
}

static bool mix_slider_dragged = false;
static int set_mix_amp(int mix_id, int chan, Float val, bool from_gui, bool remix)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      if (val >= 0.0)
	{
	  if (md->in_chans > chan)
	    {
	      char *origin = NULL;
	      mix_state *cs;
	      cs = md->active_mix_state;
	      if (!from_gui) 
		{
		  if ((cs->scalers[chan] == val) && /* drag sets scalers, so we can't optimize that case without some difficulties */
		      (md->cp->sound->sync == 0))
		    return(mix_id);

		  cs->scalers[chan] = val;
		  reflect_mix_or_track_change(mix_id, ANY_TRACK_ID, false);
#if HAVE_FORTH
		  origin = mus_format(" -mix-%d %d %.4f set-%s",
				      md->id, chan, val, S_mix_amp);
#else
		  origin = mus_format(PROC_SET_MIX_CHANNEL "%.4f" PROC_CLOSE, 
				      TO_PROC_NAME(S_mix_amp), md->id, chan, val);
#endif
		  remix_file(md, origin, true);
		  FREE(origin);
		}
	      else
		{
		  if (!remix) erase_mix_waveform(md);
		  cs->scalers[chan] = val;
		  if (remix)
		    {
#if HAVE_FORTH
		      origin = mus_format(" -mix-%d %d %.4f set-%s",
					  md->id, chan, val, S_mix_amp);
#else
		      origin = mus_format(PROC_SET_MIX_CHANNEL "%.4f" PROC_CLOSE, 
					  TO_PROC_NAME(S_mix_amp), md->id, chan, val);
#endif
		      remix_file(md, origin, false);
		      FREE(origin);
		    }
		  else 
		    {
		      cs->as_built->scalers[chan] = val * gather_track_amp(cs);
		      draw_mix_waveform(md);
		      make_temporary_graph(md->cp, md, cs);
		    }
		}
	    }
	  else return(INVALID_MIX_CHANNEL);
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

void mix_dialog_set_mix_amp(int mix_id, int chan, Float val, bool dragging)
{
  int id;
  mix_info *md;
  md = md_from_id(mix_id);
  if (!md) return;
  if ((md->save_needed) && (dragging))
    {
      wrap_mix_save_graph(md, S_setB S_mix_amp);
      md->save_needed = false;
      mix_slider_dragged = true;
    }
  id = set_mix_amp(mix_id, chan, val, true, !dragging);
  if (!dragging)
    {
      if (mix_slider_dragged)
	{
	  if (md->orig_edit_ctr < (md->cp->edit_ctr - 1))
	    {
	      backup_edit_list(md->cp);
	      backup_mix_list(md->cp, md->cp->edit_ctr);
	    }
	  mix_slider_dragged = false;
	}
      update_graph(md->cp);
    }
}

Float mix_dialog_mix_amp(int mix_id, int chan)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    {
      if (md->in_chans > chan)
	{
	  mix_state *cs;
	  cs = md->active_mix_state;
	  return(cs->scalers[chan]);
	}
    }
  return(0.0);
}

typedef struct {int id; Float speed; Float trk_speed; char *caller;} set_mix_speed_t;

static void set_mix_speed_1(mix_info *md, void *val)
{
  set_mix_speed_t *ptr = (set_mix_speed_t *)val;
  if (md->id == ptr->id) 
    {
      md->active_mix_state->speed = ptr->speed;
      md->active_mix_state->len = (off_t)(ceil(md->in_samps / (ptr->speed * ptr->trk_speed)));
    }
  remix_file(md, ptr->caller, false);
}

static int set_mix_speed(int mix_id, Float val, bool from_gui, bool remix)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      if (val != 0.0)
	{
	  char srcbuf[16];
	  char *origin = NULL;
	  Float new_speed, trk_speed = 1.0, new_final_speed;
	  mix_state *cs;
	  cs = md->active_mix_state;
	  new_speed = speed_changed(val, srcbuf, md->cp->sound->speed_control_style, md->cp->sound->speed_control_tones, 16); 
	  trk_speed = gather_track_speed(cs->track);
	  new_final_speed = new_speed * trk_speed;
	  if ((!from_gui) &&
	      (cs->speed == new_speed) && /* drag sets speed, so we can't optimize that case without some difficulties */
	      (md->cp->sound->sync == 0))
	    return(mix_id);
#if HAVE_FORTH
	  origin = mus_format(" -mix-%d %.4f set-%s",
			      md->id, val, S_mix_speed);
#else
	  origin = mus_format(PROC_SET_MIX "%.4f" PROC_CLOSE, 
			      TO_PROC_NAME(S_mix_speed), md->id, val);
#endif
	  if ((found_track_amp_env(cs->track)) && /* amp-env needs bounds check; if bounds are changed by mix speed change, re-calc entire track */
	      (track_members(cs->track) > 1))
	    {
	      off_t trk_beg, trk_len, trk_end, new_len, new_end, old_len, new_trk_end;
	      trk_beg = track_position(cs->track, ALL_TRACK_CHANS);
	      trk_len = track_frames(cs->track, ALL_TRACK_CHANS);
	      trk_end = trk_beg + trk_len;
	      new_len = (off_t)(ceil(md->in_samps / new_final_speed));
	      new_end = cs->beg + new_len;
	      if (new_end >= trk_end)
		new_trk_end = new_end;
	      else
		{
		  /* gad -- track end might be less */
		  old_len = cs->len;
		  cs->len = (off_t)(ceil(md->in_samps / new_final_speed));
		  new_trk_end = trk_beg + track_frames(cs->track, ALL_TRACK_CHANS);
		  cs->len = old_len;
		}
	      if (new_trk_end != trk_end)
		{
		  set_mix_speed_t *mp;
		  trk_len = new_trk_end - trk_beg;
		  mp = (set_mix_speed_t *)CALLOC(1, sizeof(set_mix_speed_t));
		  mp->id = mix_id;
		  mp->speed = new_speed;
		  mp->trk_speed = trk_speed;
		  mp->caller = origin;
		  remix_track_with_preset_times(cs->track, trk_beg, trk_len, reset_bounds, 1.0, set_mix_speed_1, (void *)mp);
		  FREE(mp);
		  /* "1.0" = (track?) speed change, reset_bounds just sets bounds of chained tracks */
		}
	      else
		{
		  /* no fanciness needed */
		  if (!remix) erase_mix_waveform(md);
		  cs->speed = new_speed;
		  cs->len = (off_t)(ceil(md->in_samps / new_final_speed));
		  if (remix)
		    remix_file(md, origin, (!from_gui));
		  else 
		    {
		      cs->as_built->speed = new_final_speed;
		      cs->as_built->len = cs->len;
		      draw_mix_waveform(md);
		      make_temporary_graph(md->cp, md, cs);
		    }
		}
	    }
	  else
	    {
	      if (!remix) erase_mix_waveform(md);
	      cs->speed = new_speed;
	      cs->len = (off_t)(ceil(md->in_samps / new_final_speed));
	      if (remix)
		remix_file(md, origin, (!from_gui));
	      else 
		{
		  cs->as_built->speed = new_final_speed;
		  cs->as_built->len = cs->len;
		  draw_mix_waveform(md);
		  make_temporary_graph(md->cp, md, cs);
		}
	    }
	  if (origin) FREE(origin);
	  if (!from_gui)
	    reflect_mix_or_track_change(mix_id, ANY_TRACK_ID, false);
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

void mix_dialog_set_mix_speed(int mix_id, Float val, bool dragging) 
{
  int id;
  mix_info *md;
  md = md_from_id(mix_id);
  if (!md) return;
  if ((md->save_needed) && (dragging))
    {
      wrap_mix_save_graph(md, S_setB S_mix_speed);
      md->save_needed = false;
      mix_slider_dragged = true;
    }
  id = set_mix_speed(mix_id, val, true, !dragging);
  if (!dragging)
    {
      if (mix_slider_dragged)
	{
	  if (md->orig_edit_ctr < (md->cp->edit_ctr - 1))
	    {
	      backup_edit_list(md->cp);
	      backup_mix_list(md->cp, md->cp->edit_ctr);
	    }
	  mix_slider_dragged = false;
	}
      update_graph(md->cp);
    }
}

Float mix_dialog_mix_speed(int mix_id)
{
  mix_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->speed);
  return(1.0);
}

void mix_dialog_set_mix_track(int mix_id, int track)
{
  /* mix dialog track widget */
  mix_info *md;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(md->id))) 
    {
      if ((track > 0) && (!(track_p(track))))
	{
	  int id = 0;
	  while (id < track) id = make_track(NULL, 0);
	}
      set_mix_track(md, track, false);
    }
}

int mix_dialog_mix_track(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) 
    return(md->active_mix_state->track);
  return(0);
}

typedef struct {int id; off_t pos;} set_mix_position_t;

static void set_mix_position_1(mix_info *md, void *val)
{
  char *origin;
  set_mix_position_t *ptr = (set_mix_position_t *)val;
  if (md->id == ptr->id) 
    md->active_mix_state->beg = ptr->pos;
#if HAVE_FORTH
  origin = mus_format(" -mix-%d " OFF_TD " set-%s",
		      md->id, ptr->pos, S_mix_position);
#else
  origin = mus_format(PROC_SET_MIX OFF_TD PROC_CLOSE, 
		      TO_PROC_NAME(S_mix_position), md->id, ptr->pos);
#endif
  remix_file(md, origin, false);
  FREE(origin);
}

int set_mix_position(int mix_id, off_t val)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      mix_state *cs = NULL;
      cs = md->active_mix_state;
      if (cs)
	{
	  char *origin = NULL;
	  if ((val == cs->beg) &&
	      (md->cp->sound->sync == 0))
	    return(mix_id);
	  if (val < 0) val = 0;
	  if ((found_track_amp_env(cs->track)) && /* amp-env needs bounds check; if bounds are changed by mix position change, re-calc entire track */
	      (track_members(cs->track) > 1))
	    {
	      off_t trk_beg, trk_len, trk_end, new_trk_beg, new_trk_end, new_trk_len, old_beg;
	      trk_beg = track_position(cs->track, ALL_TRACK_CHANS);
	      trk_len = track_frames(cs->track, ALL_TRACK_CHANS);
	      trk_end = trk_beg + trk_len;
	      /* see if the new pos changes track bounds */
	      old_beg = cs->beg;
	      cs->beg = val;
	      new_trk_beg = track_position(cs->track, ALL_TRACK_CHANS);
	      new_trk_len = track_frames(cs->track, ALL_TRACK_CHANS);
	      new_trk_end = new_trk_beg + new_trk_len;
	      cs->beg = old_beg;
	      if ((new_trk_beg != trk_beg) || (new_trk_end != trk_end))
		{
		  set_mix_position_t *mp;
		  mp = (set_mix_position_t *)CALLOC(1, sizeof(set_mix_position_t));
		  mp->id = mix_id;
		  mp->pos = val;
		  remix_track_with_preset_times(cs->track, new_trk_beg, new_trk_len, reset_bounds, 1.0, set_mix_position_1, (void *)mp);
		  FREE(mp);
		  /* "1.0" = speed change, reset_bounds just sets bounds of chained tracks */
		}
	      else
		{
		  /* no fanciness needed */
#if HAVE_FORTH
		  origin = mus_format(" -mix-%d " OFF_TD " set-%s",
				      md->id, val, S_mix_position);
#else
		  origin = mus_format(PROC_SET_MIX OFF_TD PROC_CLOSE, 
				      TO_PROC_NAME(S_mix_position), md->id, val);
#endif
		  cs->beg = val; 
		  remix_file(md, origin, true);
		  FREE(origin);
		}
	    }
	  else
	    {
#if HAVE_FORTH
	      origin = mus_format(" -mix-%d " OFF_TD " set-%s",
				  md->id, val, S_mix_position);
#else
	      origin = mus_format(PROC_SET_MIX OFF_TD PROC_CLOSE, 
				  TO_PROC_NAME(S_mix_position), md->id, val);
#endif
	      cs->beg = val; 
	      remix_file(md, origin, true); 
	      FREE(origin);
	    }
	  reflect_mix_or_track_change(mix_id, ANY_TRACK_ID, false);
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

off_t mix_dialog_mix_position(int mix_id)
{
  mix_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->beg);
  return(0);
}

int mix_dialog_mix_input_chans(int mix_id)
{
  mix_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->chans);
  return(0);
}

env *mix_dialog_mix_amp_env(int n, int chan) 
{
  mix_state *cs; 
  cs = cs_from_id(n); 
  if ((cs) && (chan < cs->chans) && (cs->amp_envs))
    return(cs->amp_envs[chan]);
  return(NULL);
}

env **mix_dialog_envs(int n)
{
  mix_info *md;
  md = md_from_id(n);
  if (md) 
    {
      mix_state *cs; 
      cs = md->active_mix_state;
      if (cs)
	{
	  if (md->dialog_envs == NULL)
	    {
	      int i;
	      Float flat[4] = {0.0, 1.0, 1.0, 1.0};
	      md->dialog_envs = (env **)CALLOC(md->in_chans, sizeof(env *));
	      for (i = 0; i < md->in_chans; i++)
		if ((cs->amp_envs) && (cs->amp_envs[i]))
		  md->dialog_envs[i] = copy_env(cs->amp_envs[i]);
		else md->dialog_envs[i] = make_envelope(flat, 4);
	    }
	}
      return(md->dialog_envs);
    }
  return(NULL);
}

void reflect_edit_in_mix_dialog_envs(int n)
{
  mix_info *md;
  md = md_from_id(n);
  if (md) 
    {
      mix_state *cs; 
      cs = md->active_mix_state;
      if (cs)
	{
	  int i;
	  Float flat[4] = {0.0, 1.0, 1.0, 1.0};
	  if (md->dialog_envs == NULL)
	    md->dialog_envs = (env **)CALLOC(md->in_chans, sizeof(env *));
	  for (i = 0; i < md->in_chans; i++)
	    {
	      if (md->dialog_envs[i]) 
		md->dialog_envs[i] = free_env(md->dialog_envs[i]);
	      if ((cs->amp_envs) && (cs->amp_envs[i]))
		md->dialog_envs[i] = copy_env(cs->amp_envs[i]);
	      else md->dialog_envs[i] = make_envelope(flat, 4);
	    }
	}
    }
}

env *mix_dialog_env(int n, int chan)
{
  env **envs;
  envs = mix_dialog_envs(n);
  if (envs) return(envs[chan]);
  return(NULL);
}

static int set_mix_amp_env_1(int n, int chan, env *val, bool remix)
{
  mix_info *md;
  md = md_from_id(n);
  if ((md) && (mix_ok_and_unlocked(n)))
    {
      if (md->in_chans > chan)
	{
	  env *old_env = NULL;
	  mix_state *cs;
	  cs = md->active_mix_state;
	  if ((cs->amp_envs) && (cs->amp_envs[chan])) 
	    {
	      old_env = cs->amp_envs[chan];
	      if ((old_env) &&
		  (md->cp->sound->sync == 0) &&
		  (envs_equal(old_env, val)))
		return(0);
	    }
	  if (cs->amp_envs == NULL) cs->amp_envs = (env **)CALLOC(cs->chans, sizeof(env *));
	  cs->amp_envs[chan] = copy_env(val);
	  if ((md->dialog_envs) &&
	      (md->dialog_envs[chan]))
	    {
	      env *old_dialog_env;
	      old_dialog_env = md->dialog_envs[chan];
	      md->dialog_envs[chan] = copy_env(val);
	      free_env(old_dialog_env);
	    }
	  old_env = free_env(old_env);
	  if (remix)
	    {
	      char *origin = NULL;
	      char *env_str = NULL;
	      if ((cs->amp_envs) && (cs->amp_envs[chan]))
		{
		  if (cs->amp_envs[chan]->pts < 128)
		    env_str = env_to_string(cs->amp_envs[chan]);
		  else env_str = copy_string("big env...");
		}
	      else env_str = env_to_string(NULL);
#if HAVE_FORTH
	      origin = mus_format(" -mix-%d %d %s set-%s",
				  md->id, chan, env_str, S_mix_amp_env);
#else
	      origin = mus_format(PROC_SET_MIX_CHANNEL "%s" PROC_CLOSE, 
				  TO_PROC_NAME(S_mix_amp_env), md->id, chan, env_str); /* mix chan here, not snd chn */
#endif
	      remix_file(md, origin, true);
	      FREE(origin);
	      if (env_str) FREE(env_str);
	    }
	  return(0);
	}
      else return(INVALID_MIX_CHANNEL);
    }
  else return(INVALID_MIX_ID);
}

int set_mix_amp_env(int n, int chan, env *val) {return(set_mix_amp_env_1(n, chan, val, true));}
void mix_dialog_set_mix_amp_env_without_edit(int n, int chan, env *val) {set_mix_amp_env_1(n, chan, val, false);}

void mix_at_x_y(int data, const char *filename, int x, int y)
{
  int chn, snd;
  chn = UNPACK_CHANNEL(data);
  snd = UNPACK_SOUND(data);
  if ((snd >= 0) &&
      (snd < ss->max_sounds) && 
      (snd_ok(ss->sounds[snd])) &&
      (chn >= 0) &&
      (chn < ss->sounds[snd]->nchans) &&
      (mus_file_probe(filename)))
    {
      snd_info *sp = NULL;
      chan_info *cp;
      off_t sample;
      char *fullname = NULL;
      sp = ss->sounds[snd];
      cp = sp->chans[chn];
      if ((sp->nchans > 1) && (sp->channel_style == CHANNELS_COMBINED))
	{
	  cp = which_channel(sp, y);
	  chn = cp->chan;
	}
      select_channel(sp, chn);
      sample = snd_round_off_t(ungrf_x(cp->axis, x) * (double)(SND_SRATE(sp)));
      if (sample < 0) sample = 0;
      fullname = mus_expand_filename(filename);
      mix_complete_file(sp, sample, fullname, with_mix_tags(ss), DONT_DELETE_ME, 0, false);
      if (fullname) FREE(fullname);
    }
}

#if WITH_RUN
off_t r_mix_position(int n);
off_t r_mix_position(int n) {mix_state *cs; cs = cs_from_id(n); if (cs) return(cs->beg); return(INVALID_MIX_ID);}
int r_mix_chans(int n);
int r_mix_chans(int n) {mix_state *cs; cs = cs_from_id(n); if (cs) return(cs->chans); return(0);}
off_t r_mix_frames(int n);
off_t r_mix_frames(int n) {return(mix_frames(n));}
bool r_mix_locked(int n);
bool r_mix_locked(int n) {mix_info *md; md = md_from_id(n); if (md) return(md->active_mix_state->locked); return(false);}
bool r_mix_inverted(int n);
bool r_mix_inverted(int n) {mix_info *md; md = md_from_id(n); if (md) return(md->active_mix_state->inverted); return(false);}
int r_mix_track(int n);
int r_mix_track(int n) {mix_info *md; md = md_from_id(n); if (md) return(md->active_mix_state->track); return(0);}
Float r_mix_speed(int n);
Float r_mix_speed(int n) {mix_state *cs; cs = cs_from_id(n); if (cs) return(cs->speed); return(0.0);}
#endif

static XEN snd_no_such_mix_error(const char *caller, XEN n)
{
  XEN_ERROR(XEN_ERROR_TYPE("no-such-mix"),
	XEN_LIST_2(C_TO_XEN_STRING(caller),
		   n));
  return(XEN_FALSE);
}

static XEN g_mix_position(XEN n) 
{
  #define H_mix_position "(" S_mix_position " id): sample number of start of mix"
  mix_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_position, "an integer");
  cs = cs_from_id(XEN_TO_C_INT(n));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_position, n));
  return(C_TO_XEN_OFF_T(cs->beg));  /* was orig 6-May-03 */
}

static XEN g_set_mix_position(XEN n, XEN uval) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_position, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(uval), uval, XEN_ARG_2, S_setB S_mix_position, "an integer");
  if (set_mix_position(XEN_TO_C_INT(n), 
		       beg_to_sample(uval, S_setB S_mix_position)) == INVALID_MIX_ID)
    return(snd_no_such_mix_error(S_setB S_mix_position, n));
  return(uval);
}


static XEN g_mix_chans(XEN n) 
{
  #define H_mix_chans "(" S_mix_chans " id): input channels in mix"
  mix_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_chans, "an integer");
  cs = cs_from_id(XEN_TO_C_INT(n));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_chans, n));
  return(C_TO_XEN_INT(cs->chans));
}


static XEN g_mix_p(XEN n) 
{
  #define H_mix_p "(" S_mix_p " id): " PROC_TRUE " if mix is active and accessible"
  if (XEN_INTEGER_P(n))
    return(C_TO_XEN_BOOLEAN(mix_ok(XEN_TO_C_INT_OR_ELSE(n, 0))));
  return(XEN_FALSE);
}


static XEN g_mix_frames(XEN n) 
{
  #define H_mix_frames "(" S_mix_frames " id): mix's length in frames"
  off_t len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_frames, "an integer");
  len = mix_frames(XEN_TO_C_INT(n));
  if (len == INVALID_MIX_ID) 
    return(snd_no_such_mix_error(S_mix_frames, n));
  return(C_TO_XEN_OFF_T(len));
}


static XEN g_mix_locked(XEN n) 
{
  #define H_mix_locked_p "(" S_mix_locked_p " id): " PROC_TRUE " if mix cannot be moved (due to subsequent edits overlapping it)"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_locked_p, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_locked_p, n));
  return(C_TO_XEN_BOOLEAN(md->active_mix_state->locked));
}

static void set_mix_locked(mix_info *md, bool on, bool redisplay)
{
  char *origin;
  mix_state *cs;
  cs = md->active_mix_state;
  cs->locked = on;
#if HAVE_FORTH
  origin = mus_format(" -mix-%d %s set-%s",
		      md->id, (on) ? PROC_TRUE : PROC_FALSE, S_mix_locked_p);
#else
  origin = mus_format(PROC_SET_MIX "%s" PROC_CLOSE, 
		      TO_PROC_NAME(S_mix_locked_p), md->id, (on) ? PROC_TRUE : PROC_FALSE);
#endif
  remix_file(md, origin, redisplay);
  FREE(origin);
}

static XEN g_set_mix_locked(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_locked_p, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ARG_2, S_setB S_mix_locked_p, "a boolean");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_locked_p, n));
  set_mix_locked(md, XEN_TO_C_BOOLEAN(val), true);
  return(val);
}


static XEN g_mix_inverted(XEN n) 
{
  #define H_mix_inverted_p "(" S_mix_inverted_p " id): " PROC_TRUE " if mix should invert track amp-env values (for panning)"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_inverted_p, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_inverted_p, n));
  return(C_TO_XEN_BOOLEAN(md->active_mix_state->inverted));
}

static void set_mix_inverted(mix_info *md, bool on, bool redisplay)
{
  char *origin;
  mix_state *cs;
  cs = md->active_mix_state;
  cs->inverted = on;
#if HAVE_FORTH
  origin = mus_format(" -mix-%d %s set-%s",
		      md->id, (on) ? PROC_TRUE : PROC_FALSE, S_mix_inverted_p);
#else
  origin = mus_format(PROC_SET_MIX "%s" PROC_CLOSE, 
		      TO_PROC_NAME(S_mix_inverted_p), md->id, (on) ? PROC_TRUE : PROC_FALSE);
#endif
  remix_file(md, origin, redisplay);
  FREE(origin);
}

static XEN g_set_mix_inverted(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_inverted_p, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ARG_2, S_setB S_mix_inverted_p, "a boolean");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_inverted_p, n));
  set_mix_inverted(md, XEN_TO_C_BOOLEAN(val), true);
  return(val);
}

bool mix_dialog_mix_inverted(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->active_mix_state->inverted);
  return(false);
}

void mix_dialog_set_mix_inverted(int id, bool on)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    set_mix_inverted(md, on, true);
}


static XEN g_mix_tag_position(XEN n) 
{
  #define H_mix_tag_position "(" S_mix_tag_position " id): location (sample offset) of mix tag within mix (defaults to 0)"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_tag_position, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_tag_position, n));
  return(C_TO_XEN_OFF_T(md->tag_position));
}

static XEN g_set_mix_tag_position(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_tag_position, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(val), val, XEN_ARG_2, S_setB S_mix_tag_position, "an integer");

  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_tag_position, n));
  md->tag_position = XEN_TO_C_OFF_T(val);
  update_graph(md->cp);

  return(val);
}


static XEN g_mix_track(XEN n) 
{
  #define H_mix_track "(" S_mix_track " id): track that mix is a member of"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_track, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_track, n));
  return(C_TO_XEN_INT(md->active_mix_state->track));
}

static XEN g_set_mix_track(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_track, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, S_setB S_mix_track, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_track, n));
  if (mix_ok_and_unlocked(md->id))
    {
      int trk;
      trk = XEN_TO_C_INT(val);
      if (trk >= 0)
	{
	  set_mix_track(md, trk, true);
	  reflect_mix_or_track_change(md->id, ANY_TRACK_ID, false);
	}
      else XEN_OUT_OF_RANGE_ERROR(S_setB S_mix_track, XEN_ARG_2, val, "track id must be >= 0");
    }
  return(val);
}


static XEN g_mix_name(XEN n) 
{
  #define H_mix_name "(" S_mix_name " id): name of mix"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_name, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_name, n));
  return(C_TO_XEN_STRING(md->name));
}

static XEN g_set_mix_name(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_name, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ARG_2, S_setB S_mix_name, "a string");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_name, n));
  if (md->name) FREE(md->name);
  if (XEN_STRING_P(val))
    md->name = copy_string(XEN_TO_C_STRING(val));
  else md->name = NULL;
  update_graph(md->cp);
  return(val);
}


static XEN g_mix_tag_y(XEN n) 
{
  #define H_mix_tag_y "(" S_mix_tag_y " id): height of mix's tag"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_tag_y, "an integer");

  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_tag_y, n));
  return(C_TO_XEN_INT(md->tag_y));
}

static XEN g_set_mix_tag_y(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_tag_y, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, S_setB S_mix_tag_y, "an integer");

  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_tag_y, n));
  md->tag_y = XEN_TO_C_INT(val);
  update_graph(md->cp);

  return(val);
}


static XEN g_mix_speed(XEN n) 
{
  #define H_mix_speed "(" S_mix_speed " id): srate (speed slider setting) of mix"
  mix_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_speed, "an integer");
  cs = cs_from_id(XEN_TO_C_INT(n));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_speed, n));
  return(C_TO_XEN_DOUBLE(cs->speed));
}

static XEN g_set_mix_speed(XEN n, XEN uval) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_speed, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(uval), uval, XEN_ARG_2, S_setB S_mix_speed, "a number");
  if (set_mix_speed(XEN_TO_C_INT(n), XEN_TO_C_DOUBLE(uval), false, true) == INVALID_MIX_ID)
    return(snd_no_such_mix_error(S_setB S_mix_speed, n));
  return(uval);
}


static XEN g_mixes(XEN snd, XEN chn)
{
  #define H_mixes "(" S_mixes " :optional snd chn): list of mixes (ids) associated with snd and chn"
  snd_info *sp;
  chan_info *cp;
  int i, j;
  XEN res1 = XEN_EMPTY_LIST;
  if (XEN_INTEGER_P(snd))
    {
      if (XEN_INTEGER_P(chn))
	{
	  /* scan all mixes for any associated with this channel */
	  cp = get_cp(snd, chn, S_mixes);
	  if (!cp) return(XEN_FALSE);
	  for (i = mix_infos_ctr - 1; i >= 0; i--)
	    if ((mix_ok(i)) && (mix_infos[i]->cp == cp))
	      res1 = XEN_CONS(C_TO_XEN_INT(i), res1);
	}
      else
	{
	  sp = get_sp(snd, NO_PLAYERS);
	  if (sp == NULL) 
	    return(snd_no_such_sound_error(S_mixes, snd));
	  for (i = sp->nchans - 1; i >= 0; i--)
	    res1 = XEN_CONS(g_mixes(snd, C_TO_XEN_INT(i)), res1);
	}
    }
  else
    {
      for (j = ss->max_sounds - 1; j >= 0; j--)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res1 = XEN_CONS(g_mixes(C_TO_XEN_INT(j), 
				    XEN_UNDEFINED), 
			    res1);
	}
    }
  return(res1);
}


static XEN g_mix_home(XEN n) 
{
  #define H_mix_home "(" S_mix_home " id): list of sound index and channel affected by mix"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_home, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_home, n));
  return(XEN_LIST_2(C_TO_XEN_INT((md->cp->sound)->index),
		    C_TO_XEN_INT((md->cp->chan))));
}


speed_style_t mix_speed_style(int id)
{
  mix_info *md; 
  md = md_from_id(id);
  if (md)
    return(md->speed_style);
  return(SPEED_CONTROL_AS_FLOAT);
}


static XEN g_mix_speed_style(XEN n)
{
  #define H_mix_speed_style "(" S_mix_speed_style " id): speed-style choice for mix"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_mix_speed_style, "mix id (an integer)");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_speed_style, n));
  return(C_TO_XEN_INT((int)(md->speed_style)));
}

speed_style_t set_mix_speed_style(int id, speed_style_t choice, bool from_gui)
{
  mix_info *md; 
  md = md_from_id(id);
  if (md)
    md->speed_style = choice;
  if (!from_gui) 
    reflect_mix_or_track_change(id, ANY_TRACK_ID, false); /* currently can't happen (all calls are from mix dialog) */
  return(choice);
}

static XEN g_set_mix_speed_style(XEN n, XEN speed)
{
  speed_style_t spd;
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_speed_style, "mix id (an integer)");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speed), speed, XEN_ARG_2, S_setB S_mix_speed_style, "an integer"); 
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_speed_style, n));
  spd = (speed_style_t)XEN_TO_C_INT(speed);
  if (spd > SPEED_CONTROL_AS_SEMITONE)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_mix_speed_style, 
			   1, speed, 
			   "~A, but must be " S_speed_control_as_float ", " S_speed_control_as_ratio ", or " S_speed_control_as_semitone);
  md->speed_style = spd;
  return(speed);
}


static XEN g_mix_amp(XEN n, XEN uchan) 
{
  #define H_mix_amp "(" S_mix_amp " id :optional (chan 0)): amp of mix's channel chan"
  mix_state *cs; 
  int chan;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_mix_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(uchan), uchan, XEN_ARG_2, S_mix_amp, "an integer");
  cs = cs_from_id(XEN_TO_C_INT(n));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_amp, n));
  chan = XEN_TO_C_INT_OR_ELSE(uchan, 0);
  if (chan >= cs->chans)
    return(snd_no_such_channel_error(S_mix_amp, XEN_LIST_1(n), uchan));
  return(C_TO_XEN_DOUBLE(cs->scalers[chan]));
}

static XEN g_set_mix_amp(XEN n, XEN uchan_1, XEN uval_1) 
{
  int res;
  XEN uchan, uval;
  if (XEN_BOUND_P(uval_1))
    {
      uchan = uchan_1;
      uval = uval_1;
    }
  else
    {
      uchan = uval_1;
      uval = uchan_1;
    }
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(uchan), uchan, XEN_ARG_2, S_setB S_mix_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(uval), uval, XEN_ARG_3, S_setB S_mix_amp, "a number");
  res = set_mix_amp(XEN_TO_C_INT(n), (XEN_BOUND_P(uchan)) ? XEN_TO_C_INT(uchan) : 0, XEN_TO_C_DOUBLE(uval), false, true);
  if (res == INVALID_MIX_ID)
    return(snd_no_such_mix_error(S_setB S_mix_amp, n));
  else 
    if (res == INVALID_MIX_CHANNEL)
      return(snd_no_such_channel_error(S_setB S_mix_amp, n, uchan));  
  return(uval);
}


static XEN g_mix_amp_env(XEN n, XEN chan) 
{
  #define H_mix_amp_env "(" S_mix_amp_env " id :optional (chan 0)): amplitude envelope applied to mix's channel chan"
  env *e;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_mix_amp_env, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_mix_amp_env, "an integer");
  e = mix_dialog_mix_amp_env(XEN_TO_C_INT(n), 
			     XEN_TO_C_INT_OR_ELSE(chan, 0));
  if (e) return(env_to_xen(e));
  return(XEN_EMPTY_LIST);
}

static XEN g_set_mix_amp_env(XEN n, XEN chan_1, XEN val_1) 
{
  static env *e = NULL;
  int res;
  XEN chan, val;
  if (XEN_BOUND_P(val_1))
    {
      chan = chan_1;
      val = val_1;
    }
  else
    {
      chan = val_1;
      val = chan_1;
    }
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_amp_env, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_setB S_mix_amp_env, "an integer");
  XEN_ASSERT_TYPE(XEN_LIST_P(val) || XEN_FALSE_P(val), val, XEN_ARG_3, S_setB S_mix_amp_env, "a list or " PROC_FALSE);
  if (e) e = free_env(e); /* somehow there's a memory leak here? */
  if (XEN_LIST_P(val)) e = get_env(val, S_setB S_mix_amp_env);
  res = set_mix_amp_env(XEN_TO_C_INT(n), 
			(XEN_BOUND_P(chan)) ? XEN_TO_C_INT(chan) : 0,
			e);
  if (e) e = free_env(e);
  if (res == INVALID_MIX_ID)
    return(snd_no_such_mix_error(S_setB S_mix_amp_env, n));
  else 
    if (res == INVALID_MIX_CHANNEL)
      return(snd_no_such_channel_error(S_setB S_mix_amp_env, n, chan)); 
  return(val);
}


static bool delete_mix_1(int mix_id, bool redisplay)
{
  if (mix_ok_and_unlocked(mix_id))
    {
      mix_info *md; 
      mix_state *cs;
      int i;
      md = md_from_id(mix_id);
      if (!md) return(false);
      cs = md->active_mix_state;
      for (i = 0; i < md->in_chans; i++)
	cs->scalers[i] = 0.0;
      set_mix_locked(md, true, redisplay);
      reflect_mix_or_track_change(mix_id, ANY_TRACK_ID, false);
      return(true);
    }
  return(false);
}

static XEN g_delete_mix(XEN n)
{
  #define H_delete_mix "(" S_delete_mix " id) deletes the given mix (this is undo-able)"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_delete_mix, "an integer");
  if (!(delete_mix_1(XEN_TO_C_INT(n), true)))
    return(snd_no_such_mix_error(S_delete_mix, n));
  return(n);
}


static void update_mix_waveforms(chan_info *cp)
{
  if ((cp) && (cp->have_mixes)) update_graph(cp);
}

static void update_mix_waveform_height(mix_info *md, int val)
{
  md->height = val;
}

static XEN g_mix_waveform_height(void) {return(C_TO_XEN_INT(mix_waveform_height(ss)));}
static XEN g_set_mix_waveform_height(XEN val) 
{
  #define H_mix_waveform_height "(" S_mix_waveform_height "): max height (pixels) of mix waveforms (20)"
  int new_val;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_waveform_height, "a number"); 
  new_val = mus_iclamp(0, XEN_TO_C_INT_OR_ELSE(val, 0), LOTSA_PIXELS);
  in_set_mix_waveform_height(new_val);
  map_over_mixes_with_int(update_mix_waveform_height, new_val);
  for_each_normal_chan(update_mix_waveforms);
  return(C_TO_XEN_INT(mix_waveform_height(ss)));
}


/* this is for internal (auto)testing */
#define S_mix_tag_xy "mix-tag-xy"
static XEN g_mix_tag_xy(XEN id)
{
  #define H_mix_tag_xy "(" S_mix_tag_xy " id): position of mix tag (list of x and y)"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ONLY_ARG, S_mix_tag_xy, "an integer");
  md = md_from_id(XEN_TO_C_INT(id));
  if (md) 
    return(XEN_LIST_2(C_TO_XEN_INT(md->x),
		      C_TO_XEN_INT(md->y)));
  return(snd_no_such_mix_error(S_mix_tag_xy, id));
}


static XEN g_mix_tag_width(void) {return(C_TO_XEN_INT(mix_tag_width(ss)));}
static XEN g_set_mix_tag_width(XEN val) 
{
  #define H_mix_tag_width "(" S_mix_tag_width "): width (pixels) of mix tags (6)"
  int width;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_tag_width, "an integer"); 
  width = mus_iclamp(0, XEN_TO_C_INT(val), LOTSA_PIXELS);
  set_mix_tag_width(width);
  for_each_normal_chan(update_graph);
  return(C_TO_XEN_INT(mix_tag_width(ss)));
}


static XEN g_mix_tag_height(void) {return(C_TO_XEN_INT(mix_tag_height(ss)));}
static XEN g_set_mix_tag_height(XEN val) 
{
  #define H_mix_tag_height "(" S_mix_tag_height "): height (pixels) of mix tags (14)"
  int height;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_tag_height, "an integer"); 
  height = mus_iclamp(0, XEN_TO_C_INT(val), LOTSA_PIXELS);
  set_mix_tag_height(height);
  for_each_normal_chan(update_graph);
  return(C_TO_XEN_INT(mix_tag_height(ss)));
}


static XEN g_mix(XEN file, XEN chn_samp_n, XEN file_chn, XEN snd_n, XEN chn_n, XEN tag, XEN auto_delete, XEN track_id)
{
  #define H_mix "(" S_mix " file :optional (chn-start 0) (file-chan 0) snd chn (with-tag " S_with_mix_tags ") auto-delete (track-id 0)): \
mix file channel file-chan into snd's channel chn starting at chn-start (or at the cursor location if chn-start \
is omitted), returning the new mix's id.  if with-tag is " PROC_FALSE ", the data is mixed (no draggable tag is created). \
If file_chn is omitted or " PROC_TRUE ", file's channels are mixed until snd runs out of channels. \
track-id is the track value for each newly created mix."

  chan_info *cp = NULL;
  static char *name = NULL;
  int chans, id = -1, file_channel, track_num = 0;
  bool with_mixer = true, delete_file = false;
  mix_info *md;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mix, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(chn_samp_n), chn_samp_n, XEN_ARG_2, S_mix, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(file_chn) || XEN_BOOLEAN_P(file_chn) || (!(XEN_BOUND_P(file_chn))), file_chn, XEN_ARG_3, S_mix, "an integer or boolean");
  ASSERT_CHANNEL(S_mix, snd_n, chn_n, 4);
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(tag), tag, XEN_ARG_6, S_mix, "a boolean");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, XEN_ARG_7, S_mix, "a boolean");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(track_id), track_id, XEN_ARG_8, S_mix, "a track id");
  if (name) FREE(name);
  name = mus_expand_filename(XEN_TO_C_STRING(file));
  if (!(mus_file_probe(name)))
    return(snd_no_such_file_error(S_mix, file));
  if (XEN_NOT_BOUND_P(tag))
    with_mixer = with_mix_tags(ss);
  else with_mixer = XEN_TO_C_BOOLEAN(tag);
  cp = get_cp(snd_n, chn_n, S_mix);
  if (!cp) return(XEN_FALSE);
  beg = XEN_TO_C_OFF_T_OR_ELSE(chn_samp_n, CURSOR(cp));
  if (XEN_BOOLEAN_P(auto_delete)) delete_file = XEN_TO_C_BOOLEAN(auto_delete);
  if (XEN_INTEGER_P(track_id))
    {
      track_num = XEN_TO_C_INT(track_id);
      if ((track_num > 0) && (!(track_p(track_num))))
	{
	  XEN_ERROR(NO_SUCH_TRACK,
		    XEN_LIST_2(C_TO_XEN_STRING(S_mix),
			       track_id));
	}
    }
  if (((!(XEN_INTEGER_P(file_chn))) || (XEN_TO_C_INT(file_chn) == 0)) &&
      (!(XEN_INTEGER_P(chn_n))))
    {
      /* #t = if not sync, set it for this op */
      id = mix_complete_file(cp->sound, beg, name, with_mixer,
			     (delete_file) ? DELETE_ME : DONT_DELETE_ME, 
			     track_num,
			     XEN_TRUE_P(file_chn) || (!(XEN_BOUND_P(file_chn))));
      if (id == MIX_FILE_NO_FILE) 
	{
	  XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		    XEN_LIST_3(C_TO_XEN_STRING(S_mix),
			       file,
			       C_TO_XEN_STRING(snd_io_strerror())));
	}
    }
  else
    {
      chans = mus_sound_chans(name);
      if (chans <= 0)
	{
	  XEN_ERROR(BAD_HEADER,
		    XEN_LIST_4(C_TO_XEN_STRING(S_mix),
			       file,
			       C_TO_XEN_STRING("chans <= 0"),
			       C_TO_XEN_INT(chans)));
	}
      file_channel = XEN_TO_C_INT_OR_ELSE(file_chn, 0);
      if (file_channel >= chans)
	{
	  XEN_ERROR(NO_SUCH_CHANNEL,
		    XEN_LIST_3(C_TO_XEN_STRING(S_mix),
			       C_TO_XEN_STRING("chan: ~A, ~A chans: ~A"),
			       XEN_LIST_3(file_chn,
					  file,
					  C_TO_XEN_INT(chans))));
	}
      if (chans > 0)
	{
	  char *origin;
#if HAVE_FORTH
	  origin = mus_format("\"%s\" " OFF_TD " %d %s", 
			      name, beg, file_channel, S_mix);
#else
	  origin = mus_format("%s" PROC_OPEN "\"%s\"" PROC_SEP OFF_TD PROC_SEP "%d", 
			      TO_PROC_NAME(S_mix), name, beg, file_channel);
#endif
	  md = file_mix_samples(beg,
				mus_sound_frames(name), 
				name,
				cp, 
				file_channel,
				(delete_file) ? DELETE_ME : DONT_DELETE_ME, 
				origin,
				with_mixer,
				track_num, 
				true);
	  FREE(origin);
	  if (md) 
	    id = md->id;
	  else
	    {
	      if (ss->local_errno != 0)
		{
		  XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
			    XEN_LIST_3(C_TO_XEN_STRING(S_mix),
				       file,
				       C_TO_XEN_STRING(snd_io_strerror())));
		}
	    }
	}
      else 
	{
	  return(snd_no_such_file_error(S_mix, file));
	}
    }
  return(C_TO_XEN_INT(id));
}



/* ---------------- mix sample readers ---------------- */

static XEN_OBJECT_TYPE mf_tag;
bool mf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, mf_tag));}
#define TO_MIX_SAMPLE_READER(obj) ((mix_fd *)XEN_OBJECT_REF(obj))
#define MIX_SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, mf_tag)
struct mix_fd *get_mf(XEN obj) {if (MIX_SAMPLE_READER_P(obj)) return(TO_MIX_SAMPLE_READER(obj)); else return(NULL);}

static XEN g_mf_p(XEN obj) 
{
  #define H_mf_p "(" S_mix_sample_reader_p " obj): " PROC_TRUE " if obj is a mix-sample-reader"
  return(C_TO_XEN_BOOLEAN(mf_p(obj)));
}

static char *mf_to_string(mix_fd *fd) 
{
  char *desc;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if ((fd == NULL) || (fd->sfs == NULL) || (fd->sfs[0] == NULL))
    sprintf(desc, "#<mix-sample-reader: null>");
  else
    {
      mix_info *md;
      md = fd->md;
      if ((md) && (MIX_TYPE_OK(fd->type)))
	mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<mix-sample-reader mix %d, (from " OFF_TD ", at " OFF_TD "%s): %s, %d chan%s>",
		     md->id,
		     fd->sfs[0]->initial_samp,
		     fd->sfs[0]->loc,
		     (fd->sfs[0]->at_eof) ? ", at eof" : "",
		     md->in_filename,
		     fd->chans,
		     (fd->chans > 1) ? "s" : "");
      else sprintf(desc, "#<mix-sample-reader: inactive>");
    }
  return(desc);
}

char *run_mix_sample_reader_to_string(struct mix_fd *ptr) {return(mf_to_string(ptr));}


XEN_MAKE_OBJECT_PRINT_PROCEDURE(mix_fd, print_mf, mf_to_string)

static mix_fd **dangling_mix_readers = NULL;
static int dangling_mix_reader_size = 0, dangling_top = -1;
#define DANGLING_MIX_READER_INCREMENT 16

static void list_mix_reader(mix_fd *fd)
{
  int loc = -1;
  if (dangling_mix_reader_size == 0)
    {
      dangling_mix_reader_size = DANGLING_MIX_READER_INCREMENT;
      dangling_mix_readers = (mix_fd **)CALLOC(dangling_mix_reader_size, sizeof(mix_fd *));
      loc = 0;
    }
  else
    {
      int i;
      for (i = 0; i < dangling_mix_reader_size; i++)
	if (dangling_mix_readers[i] == NULL)
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = dangling_mix_reader_size;
	  dangling_mix_reader_size += DANGLING_MIX_READER_INCREMENT;
	  dangling_mix_readers = (mix_fd **)REALLOC(dangling_mix_readers, dangling_mix_reader_size * sizeof(mix_fd *));
	  for (i = loc; i < dangling_mix_reader_size; i++) dangling_mix_readers[i] = NULL;
	}
    }
  if (loc > dangling_top) dangling_top = loc;
  fd->dangling_loc = loc;
  dangling_mix_readers[loc] = fd;
}

static void unlist_mix_reader(mix_fd *fd)
{
  if ((fd) && (fd->dangling_loc >= 0))
    {
      dangling_mix_readers[fd->dangling_loc] = NULL;
      if (fd->dangling_loc == dangling_top)
	while ((dangling_top >= 0) && 
	       (!(dangling_mix_readers[dangling_top]))) 
	  dangling_top--;
      fd->dangling_loc = -1;
    }
}

static void mf_free(mix_fd *fd)
{
  if (fd) 
    free_mix_fd(fd); 
}

void run_free_mix_fd(struct mix_fd *ptr) {mf_free(ptr);}

XEN_MAKE_OBJECT_FREE_PROCEDURE(mix_fd, free_mf, mf_free)

static void release_dangling_mix_readers(mix_info *md)
{
  int i;
  for (i = 0; i <= dangling_top; i++)
    {
      mix_fd *fd;
      fd = dangling_mix_readers[i];
      if ((fd) && 
	  (fd->md == md))
	{
	  fd->calc = C_ZERO_SOUND;
	  fd->md = NULL;
	  fd->dangling_loc = -1;
	  dangling_mix_readers[i] = NULL;
	}
    }
  while ((dangling_top >= 0) && 
	 (!(dangling_mix_readers[dangling_top]))) 
    dangling_top--;
}

static XEN g_make_mix_sample_reader(XEN mix_id, XEN ubeg)
{
  #define H_make_mix_sample_reader "(" S_make_mix_sample_reader " id :optional (beg 0)): return a reader ready to access mix id"
  mix_info *md = NULL;
  mix_fd *mf = NULL;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(mix_id), mix_id, XEN_ARG_1, S_make_mix_sample_reader, "an integer");
  ASSERT_SAMPLE_TYPE(S_make_mix_sample_reader, ubeg, XEN_ARG_2);
  md = md_from_id(XEN_TO_C_INT(mix_id));
  if (md == NULL)
    return(snd_no_such_mix_error(S_make_mix_sample_reader, mix_id));
  beg = beg_to_sample(ubeg, S_make_mix_sample_reader);
  mf = init_mix_read(md, CURRENT_MIX, beg); 
  if (mf)
    {
      list_mix_reader(mf);
      XEN_MAKE_AND_RETURN_OBJECT(mf_tag, mf, 0, free_mf);
    }
  return(XEN_FALSE);
}

struct mix_fd *run_make_mix_sample_reader(int id, off_t beg)
{
  return(init_mix_read(md_from_id(id), CURRENT_MIX, beg));
}

static XEN g_read_mix_sample(XEN obj)
{
  #define H_read_mix_sample "(" S_read_mix_sample " reader): read sample from mix reader"
  XEN_ASSERT_TYPE(MIX_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_mix_sample, "a mix-sample-reader");
  return(C_TO_XEN_DOUBLE(next_mix_sample(TO_MIX_SAMPLE_READER(obj))));
}

/* tie into generic sample-reader procedures (snd-edits.c) */

XEN g_copy_mix_sample_reader(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  return(g_make_mix_sample_reader(C_TO_XEN_INT(mf->md->id),
				  C_TO_XEN_OFF_T(current_location(mf->sfs[mf->base]))));
}

XEN g_mix_sample_reader_home(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  return(C_TO_XEN_INT(mf->md->id));
}

bool mix_sample_reader_at_end_p(struct mix_fd *mf)
{
  return(mf->sfs[mf->base]->at_eof);
}

XEN g_mix_sample_reader_at_end_p(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  return(C_TO_XEN_BOOLEAN(mf->sfs[mf->base]->at_eof));
}

XEN g_mix_sample_reader_position(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  if (mix_sample_reader_at_end_p(mf)) return(XEN_ZERO);
  return(C_TO_XEN_OFF_T(current_location(mf->sfs[mf->base])));
}

XEN g_free_mix_sample_reader(XEN obj)
{
  mix_fd *mf;
  mf = TO_MIX_SAMPLE_READER(obj);
  free_mix_fd_almost(mf);
  return(xen_return_first(XEN_FALSE, obj));
}


static XEN g_play_mix(XEN num, XEN beg)
{
  #define H_play_mix "(" S_play_mix " id :optional (beg 0)): play mix.  'beg' is where to start playing."
  mix_info *md;
  off_t samp;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ONLY_ARG, S_play_mix, "an integer");
  md = md_from_id(XEN_TO_C_INT(num));
  if (md == NULL)
    return(snd_no_such_mix_error(S_play_mix, num));
  ASSERT_SAMPLE_TYPE(S_play_mix, beg, XEN_ARG_4);
  samp = beg_to_sample(beg, S_play_mix);
  play_mix(md, samp, false); 
  return(num);
}

static XEN g_mix_vct(XEN obj, XEN beg, XEN snd, XEN chn, XEN with_tag, XEN origin, XEN track_id)
{
  #define H_mix_vct "(" S_mix_vct " data :optional (beg 0) snd chn (with-tag " S_with_mix_tags ") origin (track-id 0)): \
mix data (a vct) into snd's channel chn starting at beg; return the new mix id"

  vct *v;
  off_t bg;
  chan_info *cp;
  char *edname = NULL;
  mus_sample_t *data;
  int i, len, mix_id = -1;
  bool with_mixer = true;
  XEN_ASSERT_TYPE(MUS_VCT_P(obj), obj, XEN_ARG_1, S_mix_vct, "a vct");
  ASSERT_CHANNEL(S_mix_vct, snd, chn, 3);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg), beg, XEN_ARG_2, S_mix_vct, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(with_tag), with_tag, XEN_ARG_5, S_mix_vct, "a boolean");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(track_id), track_id, XEN_ARG_8, S_mix_vct, "a track id");
  v = XEN_TO_VCT(obj);
  len = v->length;
  cp = get_cp(snd, chn, S_mix_vct);
  if (!cp) return(XEN_FALSE);
  if (!(editable_p(cp))) return(XEN_FALSE);
  bg = beg_to_sample(beg, S_mix_vct);
  if (XEN_NOT_BOUND_P(with_tag))
    with_mixer = with_mix_tags(ss);
  else with_mixer = XEN_TO_C_BOOLEAN(with_tag);
#if SNDLIB_USE_FLOATS
  data = v->data;
#else
  data = (mus_sample_t *)CALLOC(len, sizeof(mus_sample_t));
  for (i = 0; i < len; i++)
    data[i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
#endif
  if (XEN_STRING_P(origin))
    edname = XEN_TO_C_STRING(origin);
  if ((len < MAX_BUFFER_SIZE) && (!with_mixer))
    {
      snd_fd *sf;
      sf = init_sample_read(bg, cp, READ_FORWARD);
      for (i = 0; i < len; i++)
	data[i] += read_sample(sf);
      sf = free_snd_fd(sf);
      change_samples(bg, len, data, cp, LOCK_MIXES, (char *)((edname == NULL) ? S_mix_vct : edname), cp->edit_ctr);
    }
  else
    {
      int track_num = 0;
      char *newname;
      if (XEN_INTEGER_P(track_id))
	{
	  track_num = XEN_TO_C_INT(track_id);
	  if ((track_num > 0) && (!(track_p(track_num))))
	    {
#if (!SNDLIB_USE_FLOATS)
	      FREE(data);
#endif
	      cp->edit_hook_checked = false;
	      XEN_ERROR(NO_SUCH_TRACK,
			XEN_LIST_2(C_TO_XEN_STRING(S_mix_vct),
				   track_id));
	    }
	}
      newname = save_as_temp_file(&data, 1, len, SND_SRATE(cp->sound));
      if (newname)
	{
	  /* if (temp) file troubles, snd_error called here -- surely no need for wrapper */
	  mix_id = mix_file(bg, len, 1, &cp, newname, DELETE_ME, (char *)((edname == NULL) ? S_mix_vct : edname), with_mixer, track_num); /* ORIGIN? */
	  FREE(newname);
	}
    }
  update_graph(cp);
#if (!SNDLIB_USE_FLOATS)
  FREE(data);
#endif
  cp->edit_hook_checked = false;
  return(xen_return_first(C_TO_XEN_INT(mix_id), obj, origin));
}

static int set_existing_mix_color(mix_info *md, void *arg)
{
  color_t *pixel = (color_t *)arg;
  if (md) md->wg->color = pixel[0];
  return(0);
}

void color_mixes(color_t color)
{
  color_t pixel[1];
  pixel[0] = color;
  set_mix_color(color);
  map_over_mixes_with_void(set_existing_mix_color, (void *)pixel);
  for_each_normal_chan(update_graph);
}

static XEN g_mix_color(XEN mix_id) 
{
  #define H_mix_color "(" S_mix_color "): color of mix tags"
  if (XEN_INTEGER_P(mix_id))
    return(XEN_WRAP_PIXEL(mix_to_color_from_id(XEN_TO_C_INT(mix_id))));
  return(XEN_WRAP_PIXEL(ss->sgx->mix_color));
}

static XEN g_set_mix_color(XEN arg1, XEN arg2)
{
  XEN color; 
  XEN mix_id = XEN_UNDEFINED;
  if (XEN_NOT_BOUND_P(arg2))
    color = arg1;
  else
    {
      color = arg2;
      mix_id = arg1;
    }
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_mix_color, "a color"); 
  if (XEN_INTEGER_P(mix_id))
    color_one_mix_from_id(XEN_TO_C_INT(mix_id), XEN_UNWRAP_PIXEL(color));
  else color_mixes(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_with_mix_tags(void) {return(C_TO_XEN_BOOLEAN(with_mix_tags(ss)));}
static XEN g_set_with_mix_tags(XEN val) 
{
  #define H_with_mix_tags "(" S_with_mix_tags "): " PROC_TRUE " if Snd should display mixed portions with a draggable tag"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_mix_tags, "a boolean");
  set_with_mix_tags(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_mix_tags(ss)));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_make_mix_sample_reader_w, g_make_mix_sample_reader)
XEN_NARGIFY_1(g_read_mix_sample_w, g_read_mix_sample)
XEN_NARGIFY_1(g_mf_p_w, g_mf_p)
XEN_ARGIFY_2(g_play_mix_w, g_play_mix)
XEN_NARGIFY_1(g_mix_position_w, g_mix_position)
XEN_NARGIFY_2(g_set_mix_position_w, g_set_mix_position)
XEN_NARGIFY_1(g_mix_frames_w, g_mix_frames)
XEN_NARGIFY_1(g_mix_locked_w, g_mix_locked)
XEN_NARGIFY_2(g_set_mix_locked_w, g_set_mix_locked)
XEN_NARGIFY_1(g_mix_inverted_w, g_mix_inverted)
XEN_NARGIFY_2(g_set_mix_inverted_w, g_set_mix_inverted)
XEN_NARGIFY_1(g_mix_tag_position_w, g_mix_tag_position)
XEN_NARGIFY_2(g_set_mix_tag_position_w, g_set_mix_tag_position)
XEN_NARGIFY_1(g_mix_track_w, g_mix_track)
XEN_NARGIFY_2(g_set_mix_track_w, g_set_mix_track)
XEN_NARGIFY_1(g_mix_name_w, g_mix_name)
XEN_NARGIFY_2(g_set_mix_name_w, g_set_mix_name)
XEN_NARGIFY_1(g_mix_tag_y_w, g_mix_tag_y)
XEN_NARGIFY_2(g_set_mix_tag_y_w, g_set_mix_tag_y)
XEN_NARGIFY_1(g_mix_speed_w, g_mix_speed)
XEN_NARGIFY_2(g_set_mix_speed_w, g_set_mix_speed)
XEN_NARGIFY_1(g_mix_speed_style_w, g_mix_speed_style)
XEN_NARGIFY_2(g_set_mix_speed_style_w, g_set_mix_speed_style)
XEN_NARGIFY_0(g_mix_waveform_height_w, g_mix_waveform_height)
XEN_NARGIFY_1(g_set_mix_waveform_height_w, g_set_mix_waveform_height)
XEN_NARGIFY_1(g_mix_tag_xy_w, g_mix_tag_xy)
XEN_NARGIFY_0(g_mix_tag_width_w, g_mix_tag_width)
XEN_NARGIFY_1(g_set_mix_tag_width_w, g_set_mix_tag_width)
XEN_NARGIFY_0(g_mix_tag_height_w, g_mix_tag_height)
XEN_NARGIFY_1(g_set_mix_tag_height_w, g_set_mix_tag_height)
XEN_ARGIFY_2(g_mix_amp_w, g_mix_amp)
XEN_ARGIFY_3(g_set_mix_amp_w, g_set_mix_amp)
XEN_ARGIFY_2(g_mix_amp_env_w, g_mix_amp_env)
XEN_ARGIFY_3(g_set_mix_amp_env_w, g_set_mix_amp_env)
XEN_NARGIFY_1(g_mix_chans_w, g_mix_chans)
XEN_NARGIFY_1(g_mix_p_w, g_mix_p)
XEN_NARGIFY_1(g_mix_home_w, g_mix_home)
XEN_ARGIFY_2(g_mixes_w, g_mixes)
XEN_ARGIFY_8(g_mix_w, g_mix)
XEN_ARGIFY_7(g_mix_vct_w, g_mix_vct)
XEN_ARGIFY_1(g_mix_color_w, g_mix_color)
XEN_ARGIFY_2(g_set_mix_color_w, g_set_mix_color)
XEN_NARGIFY_0(g_with_mix_tags_w, g_with_mix_tags)
XEN_NARGIFY_1(g_set_with_mix_tags_w, g_set_with_mix_tags)
XEN_NARGIFY_1(g_delete_mix_w, g_delete_mix)
#else
#define g_make_mix_sample_reader_w g_make_mix_sample_reader
#define g_read_mix_sample_w g_read_mix_sample
#define g_mf_p_w g_mf_p
#define g_play_mix_w g_play_mix
#define g_mix_position_w g_mix_position
#define g_set_mix_position_w g_set_mix_position
#define g_mix_frames_w g_mix_frames
#define g_mix_locked_w g_mix_locked
#define g_set_mix_locked_w g_set_mix_locked
#define g_mix_inverted_w g_mix_inverted
#define g_set_mix_inverted_w g_set_mix_inverted
#define g_mix_tag_position_w g_mix_tag_position
#define g_set_mix_tag_position_w g_set_mix_tag_position
#define g_mix_track_w g_mix_track
#define g_set_mix_track_w g_set_mix_track
#define g_mix_name_w g_mix_name
#define g_set_mix_name_w g_set_mix_name
#define g_mix_tag_y_w g_mix_tag_y
#define g_set_mix_tag_y_w g_set_mix_tag_y
#define g_mix_speed_w g_mix_speed
#define g_set_mix_speed_w g_set_mix_speed
#define g_mix_speed_style_w g_mix_speed_style
#define g_set_mix_speed_style_w g_set_mix_speed_style
#define g_mix_waveform_height_w g_mix_waveform_height
#define g_set_mix_waveform_height_w g_set_mix_waveform_height
#define g_mix_tag_xy_w g_mix_tag_xy
#define g_mix_tag_width_w g_mix_tag_width
#define g_set_mix_tag_width_w g_set_mix_tag_width
#define g_mix_tag_height_w g_mix_tag_height
#define g_set_mix_tag_height_w g_set_mix_tag_height
#define g_mix_amp_w g_mix_amp
#define g_set_mix_amp_w g_set_mix_amp
#define g_mix_amp_env_w g_mix_amp_env
#define g_set_mix_amp_env_w g_set_mix_amp_env
#define g_mix_chans_w g_mix_chans
#define g_mix_p_w g_mix_p
#define g_mix_home_w g_mix_home
#define g_mixes_w g_mixes
#define g_mix_w g_mix
#define g_mix_vct_w g_mix_vct
#define g_mix_color_w g_mix_color
#define g_set_mix_color_w g_set_mix_color
#define g_with_mix_tags_w g_with_mix_tags
#define g_set_with_mix_tags_w g_set_with_mix_tags
#define g_delete_mix_w g_delete_mix
#endif

void g_init_mix(void)
{
#if (!HAVE_GAUCHE)
  mf_tag = XEN_MAKE_OBJECT_TYPE("MixSampleReader", sizeof(mix_fd));
#else
  mf_tag = XEN_MAKE_OBJECT_TYPE("<mix-sample-reader>", sizeof(mix_fd), print_mf, free_mf);
  XEN_EVAL_C_STRING("(define-method object-apply ((rd <mix-sample-reader>)) (read-mix-sample rd))");
#endif

#if HAVE_GUILE
  scm_set_smob_print(mf_tag, print_mf);
  scm_set_smob_free(mf_tag, free_mf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(mf_tag, XEN_PROCEDURE_CAST g_read_mix_sample, 0, 0, 0);
#endif
#endif

#if HAVE_RUBY
  rb_define_method(mf_tag, "to_s", XEN_PROCEDURE_CAST print_mf, 0);
  rb_define_method(mf_tag, "call", XEN_PROCEDURE_CAST g_read_mix_sample, 0);
#endif

#if HAVE_FORTH
  fth_set_object_inspect(mf_tag, print_mf);
  fth_set_object_free(mf_tag, free_mf);
  fth_set_object_apply(mf_tag, XEN_PROCEDURE_CAST g_read_mix_sample, 0, 0, 0);
#endif

  XEN_DEFINE_PROCEDURE(S_make_mix_sample_reader, g_make_mix_sample_reader_w, 1, 1, 0, H_make_mix_sample_reader);
  XEN_DEFINE_PROCEDURE(S_read_mix_sample,        g_read_mix_sample_w,        1, 0, 0, H_read_mix_sample);
  XEN_DEFINE_PROCEDURE(S_mix_sample_reader_p,    g_mf_p_w,                   1, 0, 0, H_mf_p);

  XEN_DEFINE_PROCEDURE(S_play_mix,               g_play_mix_w,               0, 2, 0, H_play_mix);
  XEN_DEFINE_PROCEDURE(S_delete_mix,             g_delete_mix_w,             1, 0, 0, H_delete_mix);
  XEN_DEFINE_PROCEDURE(S_mix_frames,             g_mix_frames_w,             1, 0, 0, H_mix_frames);

  XEN_DEFINE_PROCEDURE(S_mix_tag_xy,             g_mix_tag_xy_w,             1, 0, 0, H_mix_tag_xy);
  XEN_DEFINE_PROCEDURE(S_mix_chans,              g_mix_chans_w,              1, 0, 0, H_mix_chans);
  XEN_DEFINE_PROCEDURE(S_mix_p,                  g_mix_p_w,                  1, 0, 0, H_mix_p);
  XEN_DEFINE_PROCEDURE(S_mix_home,               g_mix_home_w,               1, 0, 0, H_mix_home);
  XEN_DEFINE_PROCEDURE(S_mixes,                  g_mixes_w,                  0, 2, 0, H_mixes);
  XEN_DEFINE_PROCEDURE(S_mix,                    g_mix_w,                    1, 7, 0, H_mix);
  XEN_DEFINE_PROCEDURE(S_mix_vct,                g_mix_vct_w,                1, 6, 0, H_mix_vct);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_position,   g_mix_position_w,   H_mix_position,   S_setB S_mix_position,   g_set_mix_position_w,   1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_locked_p,   g_mix_locked_w,     H_mix_locked_p,   S_setB S_mix_locked_p,   g_set_mix_locked_w,     1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_inverted_p, g_mix_inverted_w,   H_mix_inverted_p, S_setB S_mix_inverted_p, g_set_mix_inverted_w,   1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_track,      g_mix_track_w,      H_mix_track,      S_setB S_mix_track,      g_set_mix_track_w,      1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_name,       g_mix_name_w,       H_mix_name,       S_setB S_mix_name,       g_set_mix_name_w,       1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_y,      g_mix_tag_y_w,      H_mix_tag_y,      S_setB S_mix_tag_y,      g_set_mix_tag_y_w,      1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_speed,      g_mix_speed_w,      H_mix_speed,      S_setB S_mix_speed,      g_set_mix_speed_w,      1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_color,      g_mix_color_w,      H_mix_color,      S_setB S_mix_color,      g_set_mix_color_w,      0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_amp,        g_mix_amp_w,        H_mix_amp,        S_setB S_mix_amp,        g_set_mix_amp_w,        1, 1, 2, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_amp_env,    g_mix_amp_env_w,    H_mix_amp_env,    S_setB S_mix_amp_env,    g_set_mix_amp_env_w,    1, 1, 2, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_width,  g_mix_tag_width_w,  H_mix_tag_width,  S_setB S_mix_tag_width,  g_set_mix_tag_width_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_height, g_mix_tag_height_w, H_mix_tag_height, S_setB S_mix_tag_height, g_set_mix_tag_height_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_speed_style, g_mix_speed_style_w, H_mix_speed_style, 
				   S_setB S_mix_speed_style, g_set_mix_speed_style_w, 1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_position, g_mix_tag_position_w, H_mix_tag_position, 
				   S_setB S_mix_tag_position, g_set_mix_tag_position_w, 1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_waveform_height, g_mix_waveform_height_w, H_mix_waveform_height,
				   S_setB S_mix_waveform_height, g_set_mix_waveform_height_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_mix_tags, g_with_mix_tags_w, H_with_mix_tags,
				   S_setB S_with_mix_tags, g_set_with_mix_tags_w,  0, 0, 1, 0);

  #define H_mix_release_hook S_mix_release_hook " (mix-id samps): called after the mouse has dragged a mix to some new position. \
'samps' = samples moved in the course of the drag. If it returns " PROC_TRUE ", the actual remix is the hook's responsibility."

  mix_release_hook = XEN_DEFINE_HOOK(S_mix_release_hook, 2, H_mix_release_hook);

  #define H_mix_drag_hook S_mix_drag_hook " (id): called when a mix is dragged"

  mix_drag_hook = XEN_DEFINE_HOOK(S_mix_drag_hook, 1, H_mix_drag_hook); /* arg = id */
}


/* ---------------- TRACKS ---------------- */

static void record_track_info_given_track(int track_id);
static void record_track_info(chan_info *cp, int loc);

typedef struct {
  Float amp, speed, tempo;
  env *amp_env;
  int track;
  color_t color;
  bool color_set; /* groan... 0 is a legit Pixel value */
  speed_style_t speed_style;
} track_state;

typedef struct {
  int size, loc, tag_y;
  track_state **states;
  off_t *beg, *dur;
  env *dialog_env;
  char *name;
} track_list;
  
static track_list **tracks;
static int tracks_size = 0;
static int track_ctr = 0; /* 0 is the no-track indication */

bool track_p(int trk)
{
  return((trk > 0) &&
	 (trk < track_ctr) &&
	 (tracks[trk]));
}

int track_name_to_id(const char *name)
{
  int i;
  for (i = 0; i < track_ctr; i++)
    if ((tracks[i]) &&
	(tracks[i]->name) &&
	(strcmp(tracks[i]->name, name) == 0))
      return(i);
  return(-1);
}

char *track_name(int trk)
{
  if (track_p(trk))
    return(tracks[trk]->name);
  return(NULL);
}

static int track_tag_y(int trk)
{
  return(tracks[trk]->tag_y);
}

static track_state *active_track_state(int trk)
{
  if (track_p(trk))
    {
      track_list *tl;
      tl = tracks[trk];
      if ((tl->states) &&
	  (tl->states[tl->loc]))
	return(tl->states[tl->loc]);
    }
  return(NULL);
}

static int active_track_track(int id)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) return(ts->track);
  return(0);
}

static Float active_track_amp(int id)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) return(ts->amp);
  return(1.0);
}

static Float active_track_speed(int id)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) return(ts->speed);
  return(1.0);
}

static Float active_track_tempo(int id)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) return(ts->tempo);
  return(1.0);
}

static env *active_track_amp_env(int id)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) return(ts->amp_env);
  return(NULL);
}

static color_t active_track_color(int id)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) return(ts->color);
  return((color_t)0);
}

static bool active_track_color_set(int id)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) return(ts->color_set);
  return(false);
}

static bool found_track_amp_env(int trk)
{
  while (track_p(trk))
    {
      if (active_track_amp_env(trk)) return(true);
      trk = active_track_track(trk);      
    }
  return(false);
}

static Float gather_track_amp(mix_state *cs)
{
  Float amp = 1.0;
  int id;
  id = cs->track;
  while (track_p(id))
    {
      amp *= active_track_amp(id);
      id = active_track_track(id);
    }
  return(amp);
}

static Float gather_track_speed(int id)
{
  Float speed = 1.0;
  while (track_p(id))
    {
      speed *= active_track_speed(id);
      id = active_track_track(id);
    }
  return(speed);
}

static env *gather_track_amp_env(mix_state *cs)
{
  int id;
  bool inverted;
  off_t mix_beg, mix_dur;
  env *e = NULL;
  id = cs->track;
  mix_beg = cs->beg;
  mix_dur = cs->len;
  inverted = cs->inverted;
  while (track_p(id))
    {
      env *temp;
      off_t track_beg, track_dur;
      track_beg = track_position(id, ALL_TRACK_CHANS);
      track_dur = track_frames(id, ALL_TRACK_CHANS);
      if (active_track_amp_env(id))
	{
	  /* track amps can be left until later (gather track amp) */
	  if (!e) 
	    e = window_env(active_track_amp_env(id), mix_beg, mix_dur, track_beg, track_dur, SAMPLE_ENVS_INCR);
	  else 
	    {
	      env *tmpe;
	      tmpe = e;
	      e = multiply_envs(tmpe, 
				temp = window_env(active_track_amp_env(id), mix_beg, mix_dur, track_beg, track_dur, SAMPLE_ENVS_INCR),
				SAMPLE_ENVS_INCR);
	      if (tmpe) free_env(tmpe);
	      temp = free_env(temp);
	    }
	}
      if (inverted)
	{
	  if (e)
	    {
	      temp = e;
	      e = invert_env(e);
	      temp = free_env(temp);
	    }
	  inverted = false;
	}
      /* if intermediate track has no env, that's as if it were a flat env -- a no-op, not a break in the chain */
      id = active_track_track(id);      
    }
  return(e);
}


static track_state *free_track_state(track_state *ts)
{
  if (ts)
    {
      free_env(ts->amp_env);
      FREE(ts);
    }
  return(NULL);
}

static mix_track_state *copy_mix_track_state(mix_track_state *cs)
{
  mix_track_state *ncs = NULL;
  if (cs)
    {
      int i;
      ncs = (mix_track_state *)CALLOC(1, sizeof(mix_track_state));
      ncs->chans = cs->chans;
      ncs->speed = cs->speed;
      ncs->len = cs->len;
      ncs->scalers = (Float *)CALLOC(ncs->chans, sizeof(Float));
      if (cs->amp_envs)
	ncs->amp_envs = (env **)CALLOC(ncs->chans, sizeof(env *));
      else ncs->amp_envs = NULL;
      for (i = 0; i < ncs->chans; i++)
	{
	  ncs->scalers[i] = cs->scalers[i];
	  if ((cs->amp_envs) && (cs->amp_envs[i]))
	    ncs->amp_envs[i] = copy_env(cs->amp_envs[i]);
	}
    }
  return(ncs);
}

static mix_track_state *free_mix_track_state(mix_track_state *cs)
{
  if (cs)
    {
      int i;
      if (cs->amp_envs)
	{
	  for (i = 0; i < cs->chans; i++)
	    free_env(cs->amp_envs[i]);
	  FREE(cs->amp_envs);
	  cs->amp_envs = NULL;
	}
      if (cs->scalers) FREE(cs->scalers);
      cs->scalers = NULL;
      cs->chans = 0;
      FREE(cs);
    }
  return(NULL);
}

/* each field setting prunes all existing states in the list beyond loc,
 *   adds a new track_state to the list,
 *   copies the loc state (or makes initial?), sets field, increments loc
 */

static track_state *copy_track_state(track_state *old_ts, bool copy_amp_env)
{
  track_state *ts;
  ts = (track_state *)CALLOC(1, sizeof(track_state));
  if (old_ts)
    {
      ts->amp = old_ts->amp;
      ts->speed = old_ts->speed;
      ts->speed_style = old_ts->speed_style;
      ts->tempo = old_ts->tempo;
      ts->track = old_ts->track;
      if (copy_amp_env) ts->amp_env = copy_env(old_ts->amp_env); else ts->amp_env = NULL;
      ts->color = old_ts->color;
      ts->color_set = old_ts->color_set;
    }
  else
    {
      ts->amp = 1.0;
      ts->speed = 1.0;
      ts->speed_style = speed_control_style(ss);
      ts->tempo = 1.0;
      ts->amp_env = NULL;
      ts->track = 0;
      ts->color = 0;
      ts->color_set = false;
    }
  return(ts);
}

static track_state *extend_track_list(int trk, bool copy_amp_env)
{
  /* loc is ok, we'll be setting loc+1 */
  int new_loc = 0;
  track_list *tl;
  track_state *old_ts = NULL;
  if (tracks[trk] == NULL)
    {
      tracks[trk] = (track_list *)CALLOC(1, sizeof(track_list));
      tracks[trk]->size = 0;
      tracks[trk]->name = NULL;
    }
  else
    {
      record_track_info_given_track(trk);
    }
  tl = tracks[trk];
  if (tl->states)
    {
      int i;
      new_loc = tl->loc + 1;
      old_ts = tl->states[tl->loc];
      if ((tl->loc + 1) < tl->size)
	{
	  for (i = new_loc; i < tl->size; i++)
	    tl->states[i] = free_track_state(tl->states[i]);
	}
      else
	{
	  tl->size += 4;
	  tl->states = (track_state **)REALLOC(tl->states, tl->size * sizeof(track_state *));
	  for (i = new_loc; i < tl->size; i++)
	    tl->states[i] = NULL;
	}
    }
  else
    {
      tl->size = 4;
      new_loc = 0;
      tl->states = (track_state **)CALLOC(tl->size, sizeof(track_state *));
    }
  tl->states[new_loc] = copy_track_state(old_ts, copy_amp_env);
  tl->loc = new_loc;
  return(tl->states[new_loc]);
}

static void set_active_track_track(int id, int trk)
{
  track_state *ts;
  ts = extend_track_list(id, true);
  if (ts) ts->track = trk;
}

static void set_active_track_amp(int id, Float amp)
{
  track_state *ts;
  ts = extend_track_list(id, true);
  if (ts) ts->amp = amp;
}

static void set_active_track_speed(int id, Float speed)
{
  track_state *ts;
  ts = extend_track_list(id, true);
  if (ts) ts->speed = speed;
}

static Float set_active_track_tempo(int id, Float tempo)
{
  track_state *ts;
  ts = extend_track_list(id, true);
  if (ts) ts->tempo = tempo;
  return(tempo);
}

static env *set_active_track_amp_env(int id, env *e)
{
  track_state *ts;
  ts = extend_track_list(id, false);
  if (ts) ts->amp_env = copy_env(e);
  return(e);
}

static void set_active_track_color(int id, color_t c)
{
  track_state *ts;
  ts = extend_track_list(id, true);
  if (ts) 
    {
      ts->color = c;
      ts->color_set = true;
    }
}


typedef struct {int id; int mixes;} track_members_t;

static int track_members_1(mix_info *md, void *ptr)
{
  if (mix_ok_and_unlocked(md->id))
    {
      track_members_t *tm = (track_members_t *)ptr;
      int tid;
      tid = md->active_mix_state->track;
      while (track_p(tid))
	{
	  if (tid == tm->id)
	    {
	      tm->mixes++;
	      return(0);
	    }
	  tid = active_track_track(tid);
	}
    }
  return(0);
}

static int track_members(int track_id)
{
  track_members_t *tm;
  int mixes = 0;
  tm = (track_members_t *)CALLOC(1, sizeof(track_members_t));
  tm->id = track_id;
  tm->mixes = 0;
  map_over_mixes_with_void(track_members_1, (void *)tm);
  mixes = tm->mixes;
  FREE(tm);
  return(mixes);
}

typedef struct {
  int *lst; 
  int id, lst_size, lst_ctr;
  chan_info **cps;
  bool *cps_squelched;
  int cps_size, cps_ctr;
} track_mix_list_t;

static void gather_mixes(mix_info *md, track_mix_list_t *trk)
{
  int tid;
  tid = md->active_mix_state->track;
  while (track_p(tid))
    {
      if (tid == trk->id)
	{
	  int i;
	  if (trk->lst_size <= trk->lst_ctr)
	    {
	      trk->lst_size += 8;
	      trk->lst = (int *)REALLOC(trk->lst, trk->lst_size * sizeof(int));
	    }
	  trk->lst[trk->lst_ctr++] = md->id;
	  for (i = 0; i < trk->cps_ctr; i++)
	    if (md->cp == trk->cps[i])
	      return;
	  /* if we get here, the current channel pointer hasn't been listed */
	  if (trk->cps_size <= trk->cps_ctr)
	    {
	      trk->cps_size += 2;
	      trk->cps = (chan_info **)REALLOC(trk->cps, trk->cps_size * sizeof(chan_info *));
	      trk->cps_squelched = (bool *)REALLOC(trk->cps_squelched, trk->cps_size * sizeof(bool));
	    }
	  trk->cps_squelched[trk->cps_ctr] = md->cp->squelch_update;
	  trk->cps[trk->cps_ctr++] = md->cp;
	  return;
	}
      tid = active_track_track(tid);
    }
}

static void map_over_active_mixes(void (*func)(mix_info *umx, track_mix_list_t *val1), track_mix_list_t *ptr)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      mix_info *md;
      md = mix_infos[i];
      if ((md) && (mix_ok_and_unlocked(md->id)))
	(*func)(md, ptr);
    }
}

static track_mix_list_t *track_mixes(int track_id)
{
  track_mix_list_t *trk;
  trk = (track_mix_list_t *)CALLOC(1, sizeof(track_mix_list_t));
  trk->lst_size = 4;
  trk->lst_ctr = 0;
  trk->lst = (int *)CALLOC(trk->lst_size, sizeof(int));
  trk->cps_size = 2;
  trk->cps_ctr = 0;
  trk->cps = (chan_info **)CALLOC(trk->cps_size, sizeof(chan_info *));
  trk->cps_squelched = (bool *)CALLOC(trk->cps_size, sizeof(bool));
  trk->id = track_id;
  map_over_active_mixes(gather_mixes, trk);
  return(trk);
}

static void free_track_mix_list(track_mix_list_t *trk)
{
  if (trk)
    {
      if (trk->lst) FREE(trk->lst);
      if (trk->cps) FREE(trk->cps);
      if (trk->cps_squelched) FREE(trk->cps_squelched);
      FREE(trk);
    }
}

static void map_over_track_mixes(int track_id, void (*func)(mix_info *, void *), void *val)
{
  track_mix_list_t *trk;
  int i;
  trk = track_mixes(track_id);
  for (i = 0; i < trk->lst_ctr; i++)
    (*func)(md_from_id(trk->lst[i]), val);
  free_track_mix_list(trk);
}

static void remix_track(int id, void (*func)(mix_info *m1, void *p1), void *val)
{
  track_mix_list_t *trk;
  trk = track_mixes(id);
  if (trk->cps_ctr > 0)
    {
      int i;
      int *edpos;

      for (i = 0; i < trk->cps_ctr; i++) 
	{
	  trk->cps_squelched[i] = trk->cps[i]->squelch_update;
	  trk->cps[i]->squelch_update = true;
	}

      edpos = (int *)CALLOC(trk->cps_ctr, sizeof(int));
      for (i = 0; i < trk->cps_ctr; i++)
	edpos[i] = trk->cps[i]->edit_ctr + 1; /* prepare as_one_edit */
      for (i = 0; i < trk->lst_ctr; i++)      /* has to be one at a time to keep state stacks in sync */
	(*func)(md_from_id(trk->lst[i]), val);
      for (i = 0; i < trk->cps_ctr; i++)      /* remix is one edit */
	{
	  chan_info *cp;
	  cp = trk->cps[i];
	  while (cp->edit_ctr > edpos[i]) backup_edit_list(cp);
	  backup_mix_list(cp, edpos[i]);
	}
      FREE(edpos);

      for (i = 0; i < trk->cps_ctr; i++) 
	trk->cps[i]->squelch_update = trk->cps_squelched[i];

      for (i = 0; i < trk->cps_ctr; i++)
	{
	  reflect_edit_history_change(trk->cps[i]);
	  update_graph(trk->cps[i]);
	}
    }
  free_track_mix_list(trk);
}

static void remix_track_channel(int id, int chan, void (*func)(mix_info *m1, void *p1), void *val)
{
  track_mix_list_t *trk;
  trk = track_mixes(id);
  if (trk->cps_ctr > chan)
    {
      int i, edpos;
      chan_info *cp;

      edpos = trk->cps[chan]->edit_ctr + 1; 
      cp = trk->cps[chan];

      trk->cps_squelched[chan] = cp->squelch_update;
      cp->squelch_update = true;

      for (i = 0; i < trk->lst_ctr; i++)  
	{
	  mix_info *md;
	  md = md_from_id(trk->lst[i]);
	  if (md->cp == cp)
	    (*func)(md_from_id(trk->lst[i]), val);
	}
      while (cp->edit_ctr > edpos) backup_edit_list(cp);
      backup_mix_list(cp, edpos);

      cp->squelch_update = trk->cps_squelched[chan];
      if (!(cp->squelch_update))
	{
	  reflect_edit_history_change(cp);
	  update_graph(cp);
	}
    }
  free_track_mix_list(trk);
}

chan_info *track_channel(int id, int chn)
{
  track_mix_list_t *trk;
  chan_info *cp = NULL;
  trk = track_mixes(id);
  if ((chn >= 0) && (chn < trk->cps_ctr))
    cp = trk->cps[chn];
  free_track_mix_list(trk);
  return(cp);
}

typedef struct {
  off_t pos; 
  chan_info *cp;
} track_pos_t;

static void gather_track_position(mix_info *md, void *ptr)
{
  track_pos_t *pt = (track_pos_t *)ptr;
  mix_state *cs;
  cs = md->active_mix_state;
  if ((cs) &&
      (cs->beg < pt->pos)) 
    pt->pos = cs->beg;
}

static void gather_track_channel_position(mix_info *md, void *ptr)
{
  track_pos_t *pt = (track_pos_t *)ptr;
  mix_state *cs;
  cs = md->active_mix_state;
  if ((cs) && 
      (md->cp == pt->cp) &&
      (cs->beg < pt->pos))
    pt->pos = cs->beg;
}

#define UNLIKELY_POSITION 1234567890

off_t track_position(int id, int chan)
{
  track_pos_t *pt;
  off_t result;
  if ((chan == ALL_TRACK_CHANS) && (tracks[id]->beg)) return(tracks[id]->beg[0]);
  pt = (track_pos_t *)CALLOC(1, sizeof(track_pos_t));
  pt->pos = UNLIKELY_POSITION;
  if (chan == ALL_TRACK_CHANS)
    map_over_track_mixes(id, gather_track_position, (void *)pt);
  else 
    {
      pt->cp = track_channel(id, chan);
      map_over_track_mixes(id, gather_track_channel_position, (void *)pt);
    }
  result = pt->pos;
  FREE(pt);
  if (result == UNLIKELY_POSITION)
    return(-1);
  return(result);
}

static void gather_track_end(mix_info *md, void *ptr)
{
  track_pos_t *pt = (track_pos_t *)ptr;
  mix_state *cs;
  cs = md->active_mix_state;
  if ((cs) &&
      (cs->beg + cs->len) > pt->pos)
    pt->pos = cs->beg + cs->len;
}

static void gather_track_channel_end(mix_info *md, void *ptr)
{
  track_pos_t *pt = (track_pos_t *)ptr;
  mix_state *cs;
  cs = md->active_mix_state;
  if ((cs) &&
      (md->cp == pt->cp) &&
      (cs->beg + cs->len) > pt->pos)
    pt->pos = cs->beg + cs->len;
}

off_t track_frames(int id, int chan)
{
  track_pos_t *pt;
  off_t curend, curpos;
  if ((chan == ALL_TRACK_CHANS) && (tracks[id]->dur)) return(tracks[id]->dur[0]);
  curpos = track_position(id, chan);
  if (curpos == -1) return(0);
  pt = (track_pos_t *)CALLOC(1, sizeof(track_pos_t));
  pt->pos = -1;
  if (chan == ALL_TRACK_CHANS)
    map_over_track_mixes(id, gather_track_end, (void *)pt);
  else 
    {
      pt->cp = track_channel(id, chan);
      map_over_track_mixes(id, gather_track_channel_end, (void *)pt);
    }
  curend = pt->pos;
  FREE(pt);
  if (curend == -1) /* no mixes found? */
    return(0);
  return(curend - curpos); /* no -1 in the end point above, so no +1 here */
}

typedef struct {off_t beg, end; int id, trk; Float speed;} track_reset_bounds_t;

static void remix_track_with_preset_times(int id, off_t new_position, off_t new_frames, 
					  void (*init)(mix_info *m1, void *p1), Float speed_change,
					  void (*func)(mix_info *m2, void *p2), void *func_val)
{
  int trk;
  track_reset_bounds_t *tr;
  /* during re-positioning/re-speed, the track-amp-env may be in use, and
   *  it needs to know the true (after positioning/src) track position and length
   *  for the window-env operation. But the mixes can be moved in any order,
   *  and the in-between state is bound to show the wrong length.  (This
   *  affects the entire track chain as well).  We can't set all the mix
   *  positions first, then remix (can't remember why this didn't work --
   *  I think because the erase doesn't work after the first mix because the
   *  mix state stack no longer reflects the original state).
   */
  tr = (track_reset_bounds_t *)CALLOC(1, sizeof(track_reset_bounds_t));
  tr->id = id;
  tr->speed = speed_change;
  tracks[id]->beg = (off_t *)CALLOC(1, sizeof(off_t));
  tracks[id]->beg[0] = new_position;
  tracks[id]->dur = (off_t *)CALLOC(1, sizeof(off_t));
  tracks[id]->dur[0] = new_frames;
  trk = active_track_track(id);
  while (track_p(trk))
    {
      /* now find all other track mixes not in current (id) track and reset beg/dur */
      tr->beg = new_position;
      tr->end = new_position + new_frames;
      tr->trk = trk;
      if (init)
	map_over_track_mixes(trk, init, (void *)tr);
      tracks[trk]->beg = (off_t *)CALLOC(1, sizeof(off_t));
      tracks[trk]->dur = (off_t *)CALLOC(1, sizeof(off_t));
      tracks[trk]->beg[0] = tr->beg;
      tracks[trk]->dur[0] = tr->end - tr->beg;
      trk = active_track_track(trk);
    }
  FREE(tr);
  tr = NULL;
  remix_track(id, func, func_val);
  /* now unset all the global positioning data */
  FREE(tracks[id]->beg);
  tracks[id]->beg = NULL;
  FREE(tracks[id]->dur);
  tracks[id]->dur = NULL;
  trk = active_track_track(id);
  while (track_p(trk))
    {
      FREE(tracks[trk]->beg);
      tracks[trk]->beg = NULL;
      FREE(tracks[trk]->dur);
      tracks[trk]->dur = NULL;
      trk = active_track_track(trk);
    }
}

typedef struct {off_t change; chan_info *cp; char *caller;} track_position_t;

static void set_track_position_1(mix_info *md, void *val)
{
  mix_state *cs;
  track_position_t *ptr = (track_position_t *)val;
  cs = md->active_mix_state;
  cs->beg += ptr->change;
  if (cs->beg < 0) cs->beg = 0;
  remix_file(md, ptr->caller, false);
}

static void reset_bounds(mix_info *md, void *val)
{
  track_reset_bounds_t *tr = (track_reset_bounds_t *)val;
  if (md->active_mix_state->track != tr->id)
    {
      mix_state *cs;
      cs = md->active_mix_state;
      if (cs)
	{
	  if (cs->beg < tr->beg)
	    tr->beg = cs->beg;
	  if ((cs->beg + cs->len) > tr->end)
	    tr->end = cs->beg + cs->len;
	}
    }
}

void set_track_position(int id, off_t pos)
{
  off_t curpos;
  curpos = track_position(id, ALL_TRACK_CHANS);
  if ((track_p(id)) && (curpos != pos))
    {
      track_position_t *val;
      val = (track_position_t *)CALLOC(1, sizeof(track_position_t));
      val->change = pos - curpos;
#if HAVE_FORTH
      val->caller = mus_format("%d " OFF_TD " set-%s",
			       id, pos, S_track_position);
#else
      val->caller = mus_format(PROC_SET_TRACK OFF_TD PROC_CLOSE, 
			       TO_PROC_NAME(S_track_position), id, pos);
#endif
      if ((found_track_amp_env(id)) &&
	  (track_members(id) > 1))
	{
	  off_t cur_frames;
	  cur_frames = track_frames(id, ALL_TRACK_CHANS);
	  remix_track_with_preset_times(id, pos, cur_frames, reset_bounds, 1.0, set_track_position_1, (void *)val);
	}
      else
	{
	  remix_track(id, set_track_position_1, (void *)val);
	}
      FREE(val->caller);
      FREE(val);
      reflect_mix_or_track_change(ANY_MIX_ID, id, false);
    }
}

static void set_track_channel_position_1(mix_info *md, void *val)
{
  track_position_t *tc = (track_position_t *)val;
  mix_state *cs;
  if (md->cp == tc->cp)
    {
      cs = md->active_mix_state;
      cs->beg += tc->change;
      if (cs->beg < 0) cs->beg = 0;
    }
  remix_file(md, tc->caller, false);
}

typedef struct {chan_info *cp; off_t beg, end;} track_channel_bounds_t;

static void set_track_channel_bounds(mix_info *md, void *ptr)
{
  track_channel_bounds_t *tcb = (track_channel_bounds_t *)ptr;
  mix_state *cs;
  if (md->cp != tcb->cp)
    {
      cs = md->active_mix_state;
      if ((tcb->beg == -1) || (cs->beg < tcb->beg)) tcb->beg = cs->beg;
      if ((cs->beg + cs->len) > tcb->end) tcb->end = cs->beg + cs->len;
    }
}

int track_chans(int id)
{
  int chans = 0;
  track_mix_list_t *trk;
  trk = track_mixes(id);
  chans = trk->cps_ctr;
  free_track_mix_list(trk);
  return(chans);
}

static void set_track_channel_position(int id, int chan, off_t pos)
{
  off_t curpos;
  curpos = track_position(id, chan);
  if ((track_p(id)) && (curpos != pos))
    {
      track_position_t *tc;
      tc = (track_position_t *)CALLOC(1, sizeof(track_position_t));
      tc->change = pos - curpos;
#if HAVE_FORTH
      tc->caller = mus_format("%d %d " OFF_TD " set-%s",
			      chan, id, pos, S_track_position);
#else
      tc->caller = mus_format(PROC_SET_TRACK_CHANNEL OFF_TD PROC_CLOSE, 
			      TO_PROC_NAME(S_track_position), chan, id, pos);
#endif
      if ((track_chans(id) > 1) && 
	  (found_track_amp_env(id)))  /* if amp-env and bounds change, need preset bounds during remix */
	{
	  off_t old_trk_beg, old_trk_len, old_trk_end, chn_len;
	  track_channel_bounds_t *tcb;
	  old_trk_len = track_frames(id, ALL_TRACK_CHANS);
	  old_trk_beg = track_position(id, ALL_TRACK_CHANS);
	  old_trk_end = old_trk_beg + old_trk_len;
	  chn_len = track_frames(id, chan);
	  tcb = (track_channel_bounds_t *)CALLOC(1, sizeof(track_channel_bounds_t));
	  tcb->cp = track_channel(id, chan);
	  tcb->beg = -1;
	  tcb->end = -1;
	  map_over_track_mixes(id, set_track_channel_bounds, (void *)tcb);
	  if (pos < tcb->beg) tcb->beg = pos;
	  if ((pos + chn_len) > tcb->end) tcb->end = pos + chn_len;
	  if ((old_trk_beg != tcb->beg) || 
	      (old_trk_end != tcb->end))
	    {
	      tc->cp = track_channel(id, chan);
	      remix_track_with_preset_times(id, tcb->beg, tcb->end - tcb->beg, reset_bounds, 1.0, set_track_channel_position_1, (void *)tc);
	    }
	  else
	    {
	      remix_track_channel(id, chan, set_track_position_1, (void *)tc);
	    }
	  FREE(tcb);
	}
      else
	{
	  remix_track_channel(id, chan, set_track_position_1, (void *)tc);
	}
      reflect_mix_or_track_change(ANY_MIX_ID, id, false);
      FREE(tc->caller);
      FREE(tc);
    }
}

static void set_mix_track_1(mix_info *md, void *val)
{
  remix_file(md, (char *)val, false);
}

static void set_mix_track(mix_info *md, int trk, bool redisplay)
{
  if ((md->active_mix_state->track != trk) && 
      ((trk == 0) || (track_p(trk))))
    {
      int old_track;
      off_t old_beg = 0, old_len = 0, new_beg = 0, new_len = 0;
      bool check_old = false, check_new = false;
      char *origin;
      old_track = md->active_mix_state->track;
      if ((track_p(old_track)) && 
	  (found_track_amp_env(old_track)) && 
	  (track_members(old_track) > 1))
	{
	  old_beg = track_position(old_track, ALL_TRACK_CHANS);
	  old_len = track_frames(old_track, ALL_TRACK_CHANS);
	  check_old = true;
	}
      if ((track_p(trk)) && 
	  (found_track_amp_env(trk)) && 
	  (track_members(trk) > 0))
	{
	  new_beg = track_position(trk, ALL_TRACK_CHANS);
	  new_len = track_frames(trk, ALL_TRACK_CHANS);
	  check_new = true;
	}
      md->active_mix_state->track = trk;
#if HAVE_FORTH
      origin = mus_format(" -mix-%d %d set-%s",
			  md->id, trk, S_mix_track);
#else
      origin = mus_format(PROC_SET_MIX "%d" PROC_CLOSE, 
			  TO_PROC_NAME(S_mix_track), md->id, trk);
#endif
      if ((track_p(trk)) && (active_track_color_set(trk)))
	color_one_mix_from_id(md->id, active_track_color(trk));
      if ((check_old) && 
	  ((track_position(old_track, ALL_TRACK_CHANS) != old_beg) || 
	   (track_frames(old_track, ALL_TRACK_CHANS) != old_len)))
	remix_track(old_track, set_mix_track_1, (void *)origin);
      if ((check_new) && 
	  ((track_position(trk, ALL_TRACK_CHANS) != new_beg) || 
	   (track_frames(trk, ALL_TRACK_CHANS) != new_len)))
	remix_track(trk, set_mix_track_1, (void *)origin);
      else remix_file(md, origin, redisplay); 
      FREE(origin);
      reflect_mix_or_track_change(md->id, trk, false);
    }
}


static void set_track_track_1(mix_info *md, void *ptr)
{
  remix_file(md, (char *)ptr, false);
}

bool set_track_track(int id, int trk)
{
  if (active_track_track(id) != trk)
    {
      char *origin;
      if (trk > 0)
	{
	  int tid;
	  tid = trk;
	  while (track_p(tid))
	    {
	      if (tid == id) return(false);
	      tid = active_track_track(tid);
	    }
	  if (!(track_p(trk))) /* set_mix_track ensures that track exists */
	    {
	      tid = make_track(NULL, 0);
	      while (tid < trk) tid = make_track(NULL, 0);
	    }
	}
#if HAVE_FORTH
      origin = mus_format("%d %d set-%s",
			  id, trk, S_track_track);
#else
      origin = mus_format(PROC_SET_TRACK "%d" PROC_CLOSE, 
			  TO_PROC_NAME(S_track_track), id, trk);
#endif
      set_active_track_track(id, trk);
      remix_track(id, set_track_track_1, (void *)origin);
      FREE(origin);
      reflect_mix_or_track_change(ANY_MIX_ID, id, false);
    }
  return(true);
}

static void set_track_amp_1(mix_info *md, void *val)
{
  remix_file(md, (char *)val, false);
}

static void set_track_amp(int id, Float amp)
{
  if ((track_p(id)) && (active_track_amp(id) != amp))
    {
      char *origin;
#if HAVE_FORTH
      origin = mus_format("%d %.4f set-%s",
			  id, amp, S_track_amp);
#else
      origin = mus_format(PROC_SET_TRACK "%.4f" PROC_CLOSE, 
			  TO_PROC_NAME(S_track_amp), id, amp);
#endif
      set_active_track_amp(id, amp);
      remix_track(id, set_track_amp_1, (void *)origin);
      FREE(origin);
      reflect_mix_or_track_change(ANY_MIX_ID, id, false);
    }
}

static void speed_reset_frames(mix_info *md, void *val)
{
  track_reset_bounds_t *tr = (track_reset_bounds_t *)val;
  if (md->active_mix_state->track != tr->id)
    {
      mix_state *cs;
      cs = md->active_mix_state;
      if (cs)
	{
	  off_t end;
	  if (cs->beg < tr->beg) tr->beg = cs->beg;
	  end = cs->beg + (off_t)(cs->len * tr->speed);
	  if (end > tr->end) tr->end = end;
	}
    }
}

static void set_track_speed_1(mix_info *md, void *val)
{
  remix_file(md, (char *)val, false);
}

static void set_track_speed(int id, Float speed)
{
  if ((track_p(id)) && (active_track_speed(id) != speed))
    {
      char *origin;
#if HAVE_FORTH
      origin = mus_format("%d %.4f set-%s",
			  id, speed, S_track_speed);
#else
      origin = mus_format(PROC_SET_TRACK "%.4f" PROC_CLOSE, 
			  TO_PROC_NAME(S_track_speed), id, speed);
#endif
      if ((found_track_amp_env(id)) &&
	  (track_members(id) > 1))
	{
	  off_t beg, dur;
	  Float change;
	  track_reset_bounds_t *tr;
	  beg = track_position(id, ALL_TRACK_CHANS);
	  change = active_track_speed(id) / speed;
	  tr = (track_reset_bounds_t *)CALLOC(1, sizeof(track_reset_bounds_t));
	  tr->id = INVALID_TRACK_ID;
	  tr->speed = change;
	  tr->beg = beg;
	  tr->end = beg + track_frames(id, ALL_TRACK_CHANS);
	  tr->trk = id;
	  map_over_track_mixes(id, speed_reset_frames, (void *)tr);
	  dur = tr->end - tr->beg;
	  FREE(tr);
	  tr = NULL;
	  set_active_track_speed(id, speed);
	  remix_track_with_preset_times(id, beg, dur, speed_reset_frames, change, set_track_speed_1, (void *)origin);
	}
      else
	{     
	  set_active_track_speed(id, speed);
	  remix_track(id, set_track_speed_1, (void *)origin); /* was speed? */
	}
      FREE(origin);
      reflect_mix_or_track_change(ANY_MIX_ID, id, false);
    }
}

static void set_track_amp_env_1(mix_info *md, void *val)
{
  remix_file(md, (char *)val, false);
}

static void set_track_amp_env(int id, env *e)
{
  if (track_p(id))
    {
      char *origin, *env_str;
      if (e)
	{
	  if (e->pts < 128)
	    env_str = env_to_string(e);
	  else env_str = copy_string("big env...");
	}
      else env_str = env_to_string(NULL);
#if HAVE_FORTH
      origin = mus_format("%d %s set-%s",
			  id, env_str, S_track_amp_env);
#else
      origin = mus_format(PROC_SET_TRACK "%s" PROC_CLOSE, 
			  TO_PROC_NAME(S_track_amp_env), id, env_str);
#endif
      set_active_track_amp_env(id, e);
      remix_track(id, set_track_amp_env_1, (void *)origin);
      FREE(origin);
      FREE(env_str);
      reflect_mix_or_track_change(ANY_MIX_ID, id, false);
    }
}


static void tempo_reset_frames(mix_info *md, void *val)
{
  track_reset_bounds_t *tr = (track_reset_bounds_t *)val;
  /* tr->beg = track_position, tr->speed = tempo mult */
  if (md->active_mix_state->track != tr->id)
    {
      mix_state *cs;
      cs = md->active_mix_state;
      if (cs)
	{
	  off_t end;
	  end = tr->beg + (off_t)((cs->beg - tr->beg) * tr->speed);
	  if (end > tr->end) tr->end = end;
	}
    }
}

typedef struct {Float tempo_mult; off_t beg; char *caller;} track_tempo_t;

static void set_track_tempo_1(mix_info *md, void *val)
{
  track_tempo_t *tt = (track_tempo_t *)val;
  mix_state *cs;
  cs = md->active_mix_state;
  if (cs->beg != tt->beg)
    {
      cs->beg = tt->beg + (off_t)((cs->beg - tt->beg) * tt->tempo_mult);
      remix_file(md, tt->caller, false);
    }
}

static void set_track_tempo(int id, Float tempo)
{
  if ((track_p(id)) && (active_track_tempo(id) != tempo))
    {
      if (track_members(id) > 1)
	{
	  char *origin;
	  track_tempo_t tt;
#if HAVE_FORTH
	  origin = mus_format("%d %.4f set-%s",
			      id, tempo, S_track_tempo);
#else
	  origin = mus_format(PROC_SET_TRACK "%.4f" PROC_CLOSE, 
			      TO_PROC_NAME(S_track_tempo), id, tempo);
#endif
	  if (found_track_amp_env(id))
	    {
	      off_t beg, dur;
	      Float change;
	      track_reset_bounds_t *tr;
	      Float cur_tempo;
	      beg = track_position(id, ALL_TRACK_CHANS);
	      change = active_track_tempo(id) / tempo;
	      tr = (track_reset_bounds_t *)CALLOC(1, sizeof(track_reset_bounds_t));
	      tr->id = INVALID_TRACK_ID;
	      tr->speed = change;
	      tr->beg = beg;
	      tr->end = beg;
	      tr->trk = id;
	      map_over_track_mixes(id, tempo_reset_frames, (void *)tr);
	      dur = tr->end - tr->beg;
	      FREE(tr);
	      tr = NULL;
	      tt.beg = beg;
	      tt.caller = origin;
	      cur_tempo = active_track_tempo(id);
	      tt.tempo_mult = cur_tempo / tempo; /* we'll be multiplying for the new diff */
	      set_active_track_tempo(id, tempo);
	      remix_track_with_preset_times(id, beg, dur, NULL, change, set_track_tempo_1, (void *)(&tt));
	    }
	  else
	    {
	      Float cur_tempo;
	      tt.beg = track_position(id, ALL_TRACK_CHANS);
	      tt.caller = origin;
	      cur_tempo = active_track_tempo(id);
	      tt.tempo_mult = cur_tempo / tempo; /* we'll be multiplying for the new diff */
	      set_active_track_tempo(id, tempo);
	      remix_track(id, set_track_tempo_1, (void *)(&tt));
	    }
	  FREE(origin);
	}
      else
	{     
	  /* just 1 (or 0) track members, so this has no effect on mix positions */
	  set_active_track_tempo(id, tempo);
	}
      reflect_mix_or_track_change(ANY_MIX_ID, id, false);
    }
}



/* -------- chan-relative view of track lists -------- */

track_info *free_track_info(chan_info *cp, int loc)
{
  if (cp->tracks[loc])
    {
      track_info *ti;
      ti = cp->tracks[loc];
      if (ti->ids) FREE(ti->ids);
      if (ti->locs) FREE(ti->locs);
      FREE(ti);
    }
  return(NULL);
}

void free_track_info_list(chan_info *cp)
{
  if (cp->tracks)
    {
      int i;
      for (i = 0; i < cp->edit_size; i++)
	free_track_info(cp, i);
      FREE(cp->tracks);
      cp->tracks = NULL;
    }
}

static int gather_track_info(mix_info *md, void *ptr)
{
  track_info *ti = (track_info *)ptr;
  if (md->active_mix_state->track > 0)
    {
      int track_id;
      track_id = md->active_mix_state->track;
      while (track_p(track_id))
	{
	  int i;
	  if (ti->size == 0)
	    {
	      ti->size = 1;
	      ti->ids = (short *)CALLOC(1, sizeof(short));
	      ti->locs = (short *)CALLOC(1, sizeof(short));
	      ti->ids[0] = track_id;
	      ti->locs[0] = tracks[track_id]->loc;
	    }
	  else
	    {
	      bool happy = false;
	      for (i = 0; i < ti->size; i++)
		if (ti->ids[i] == track_id)
		  {
		    happy = true;
		    break;
		  }
	      if (!happy)
		{
		  ti->size++;
		  ti->ids = (short *)REALLOC(ti->ids, ti->size * sizeof(short));
		  ti->locs = (short *)REALLOC(ti->locs, ti->size * sizeof(short));
		  ti->ids[ti->size - 1] = track_id;
		  ti->locs[ti->size - 1] = tracks[track_id]->loc;
		}
	    }
	  track_id = active_track_track(track_id);
	}
    }
  return(0);
}

static void record_track_info_given_track(int track_id)
{
  track_mix_list_t *trk;
  int i;
  trk = track_mixes(track_id);
  for (i = 0; i < trk->cps_ctr; i++)
    record_track_info(trk->cps[i], trk->cps[i]->edit_ctr);
  free_track_mix_list(trk);
}

static void record_track_info(chan_info *cp, int loc)
{
  if (track_ctr == 0)
    cp->tracks[loc] = NULL;
  else
    {
      track_info *ti;
      if (cp->tracks[loc]) free_track_info(cp, loc);
      ti = (track_info *)CALLOC(1, sizeof(track_info));
      ti->size = 0;
      map_over_channel_mixes_with_void(cp, gather_track_info, (void *)ti);
      if (ti->size == 0)
	{
	  FREE(ti);
	  ti = NULL;
	}
      cp->tracks[loc] = ti;
    }
}

void record_initial_track_info(chan_info *cp)
{
  record_track_info(cp, cp->edit_ctr);
}


/*---------------- snd-edit chan track update ---------------- */

void update_track_lists(chan_info *cp, int top_ctr)
{
  if (cp->tracks)
    {
      track_info *ti;
      ti = cp->tracks[cp->edit_ctr];
      if ((!ti) && (cp->edit_ctr < top_ctr))
	{
	  /* revert-sound can undo past the last track info record */
	  int ctr;
	  ctr = cp->edit_ctr;
	  while ((!ti) && (ctr < top_ctr))
	    ti = cp->tracks[ctr++];	    
	}
      if (ti)
	{
	  /* use ids to reset locs */
	  int i;
	  for (i = 0; i < ti->size; i++)
	    if (track_p(ti->ids[i]))
	      {
		track_list *tl;
		tl = tracks[ti->ids[i]];
		tl->loc = (int)(ti->locs[i]);
	      }
	}
    }
}

void release_pending_track_states(void)
{
  /* snd-file if active_sounds == 0 (so no way undo can re-activate something) */
  int i;
  for (i = 1; i < track_ctr; i++)
    {
      track_list *tl;
      tl = tracks[i];
      if ((tl) && (tl->states) && (tl->loc > 0))
	{
	  int k;
	  free_track_state(tl->states[0]);
	  tl->states[0] = copy_track_state(tl->states[tl->loc], true);
	  tl->loc = 0;
	  for (k = 1; k < tl->size; k++)
	    tl->states[k] = free_track_state(tl->states[k]);
	}
    }
}


/* ---------------- drag track ---------------- */

static track_graph_t *track_save_graph(mix_info *orig_md, int track_id)
{
  track_graph_t *tg = NULL;
  track_mix_list_t *trk;
  trk = track_mixes(track_id);
  if (trk->lst_ctr > 0)
    {
      mix_info *md;
      mix_state *cs;
      chan_info *cp = NULL;
      int i;
      off_t track_orig;
      char *origin;
      mix_state **old_cs;
      track_orig = track_position(track_id, ALL_TRACK_CHANS);
      old_cs = (mix_state **)CALLOC(trk->lst_ctr, sizeof(mix_state *));
      tg = (track_graph_t *)CALLOC(1, sizeof(track_graph_t));
      tg->xs = (int *)CALLOC(trk->lst_ctr, sizeof(int));
      tg->edpos = (int *)CALLOC(trk->cps_ctr, sizeof(int));
      for (i = 0; i < trk->cps_ctr; i++)
	tg->edpos[i] = trk->cps[i]->edit_ctr + 1;

#if HAVE_FORTH
      origin = mus_format("%d " OFF_TD " set-%s",
			  track_id, track_orig, S_track_position);
#else
      origin = mus_format(PROC_SET_TRACK OFF_TD PROC_CLOSE, 
			  TO_PROC_NAME(S_track_position), track_id, track_orig);
#endif

      for (i = 0; i < trk->lst_ctr; i++)
	{
	  int k;
	  md = md_from_id(trk->lst[i]);
	  cs = md->active_mix_state;
	  old_cs[i] = copy_mix_state(cs);
	  for (k = 0; k < md->in_chans; k++) 
	    cs->scalers[k] = 0.0;
	  remix_file(md, origin, false);
	}
      FREE(origin);

      for (i = 0; i < trk->lst_ctr; i++)
	{
	  int pts = 0;
	  md = md_from_id(trk->lst[i]);
	  if (orig_md == NULL) orig_md = md;
	  cs = md->active_mix_state;
	  tg->xs[i] = md->x;
	  if (md == orig_md) tg->orig = i;
	  md->track_tag_position = (cs->orig - track_orig) + md->tag_position;
	  if (cp != md->cp)
	    {
	      cp = md->cp;
	      pts = make_graph(cp);
	    }
	  mix_save_graph(md->wg, pts);
	}
      for (i = 0; i < trk->lst_ctr; i++)
	{
	  md = md_from_id(trk->lst[i]);
	  free_mix_state(md->active_mix_state);
	  md->active_mix_state = old_cs[i];
	  old_cs[i]->edit_ctr = md->cp->edit_ctr;
	}
      FREE(old_cs);
    }
  free_track_mix_list(trk);
  return(tg);
}

static void move_track(int track_id, track_graph_t *data)
{
  track_mix_list_t *trk;
  int i, diff;
  trk = track_mixes(track_id);
  diff = data->x - data->xs[data->orig];
  for (i = 0; i < trk->lst_ctr; i++)
    {
      mix_info *md;
      md = md_from_id(trk->lst[i]);
      if (i == data->orig)
	md->x = data->x;
      else md->x = data->xs[i] + diff;
      draw_mix_tag(md);
    }
  free_track_mix_list(trk);
}

static void finish_dragging_track(int track_id, track_graph_t *data)
{
  track_mix_list_t *trk;
  int i;
  mix_info *md;
  mix_state *cs;
  off_t change = 0;
  char *origin;
  trk = track_mixes(track_id);
  md = md_from_id(trk->lst[data->orig]);
  md->x = data->x;
  move_mix(md);          /* update to last beg */
  cs = md->active_mix_state; 
  change = cs->beg - cs->orig;

  for (i = 0; i < trk->cps_ctr; i++) 
    {
      trk->cps_squelched[i] = trk->cps[i]->squelch_update;
      trk->cps[i]->squelch_update = true;
    }

  for (i = 0; i < trk->lst_ctr; i++)
    {
      mix_context *ms;
      md = md_from_id(trk->lst[i]);
      md->track_tag_position = 0;
      ms = md->wg;
      ms->lastpj = 0;
      cs = md->active_mix_state;
      cs->beg = cs->orig + change;
    }
#if HAVE_FORTH
  origin = mus_format("%d " OFF_TD " set-%s",
		      track_id, cs->beg, S_track_position);
#else
  origin = mus_format(PROC_SET_TRACK OFF_TD PROC_CLOSE, 
		      TO_PROC_NAME(S_track_position), track_id, cs->beg);
#endif
  for (i = 0; i < trk->lst_ctr; i++)
    {
      int k;
      md = md_from_id(trk->lst[i]);
      cs = md->states[md->current_state];
      for (k = 0; k < md->in_chans; k++) 
	cs->as_built->scalers[k] = 0.0;
      remix_file(md, origin, false);
    }
  FREE(origin);
  for (i = 0; i < trk->cps_ctr; i++)      /* drag is one edit */
    {
      chan_info *cp;
      cp = trk->cps[i];
      
      while (cp->edit_ctr > data->edpos[i]) backup_edit_list(cp);
      backup_mix_list(cp, data->edpos[i]);
    }

  for (i = 0; i < trk->cps_ctr; i++) 
    trk->cps[i]->squelch_update = trk->cps_squelched[i];

  for (i = 0; i < trk->cps_ctr; i++)
    {
      reflect_edit_history_change(trk->cps[i]);
      update_graph(trk->cps[i]);
    }

  free_track_mix_list(trk);
  reflect_mix_or_track_change(ANY_MIX_ID, track_id, false);
}

static track_graph_t *free_track_graph(track_graph_t *ptr)
{
  if (ptr)
    {
      if (ptr->xs) FREE(ptr->xs);
      if (ptr->edpos) FREE(ptr->edpos);
      FREE(ptr);
    }
  return(NULL);
}


/* display track for track dialog env editor */

void display_track_waveform(int track_id, axis_info *ap)
{
  track_mix_list_t *trk;
  trk = track_mixes(track_id);
  if (trk->lst_ctr > 0)
    {
      Float scl, x0, x1, y0, y1;
      off_t old_lo, old_hi;
      double cur_srate = -1.0;
      off_t t_beg, t_dur;
      int i;
      mix_info *md;
      md = md_from_id(trk->lst[0]);
      cur_srate = (double)SND_SRATE(md->cp->sound);
      scl = ap->y_axis_y0 - ap->y_axis_y1;
      old_lo = ap->losamp;
      old_hi = ap->hisamp;
      x0 = ap->x0;
      x1 = ap->x1;
      y0 = ap->y0;
      y1 = ap->y1;
      t_beg = track_position(track_id, ALL_TRACK_CHANS);
      t_dur = track_frames(track_id, ALL_TRACK_CHANS);
      ap->losamp = t_beg;
      ap->hisamp = t_beg + t_dur;
      ap->x0 = (double)(ap->losamp) / cur_srate;
      ap->x1 = (double)(ap->hisamp) / cur_srate;
      ap->y0 = -1.0;
      ap->y1 = 1.0;
      init_axis_scales(ap);
      for (i = 0; i < trk->lst_ctr; i++)
	{
	  int pts;
	  bool two_sided;
	  md = md_from_id(trk->lst[i]);
	  pts = prepare_mix_waveform(md, md->active_mix_state, ap, scl * .5, (int)(scl * .5), cur_srate, &two_sided);
	  if (pts > 0)
	    show_track_background_wave(pts, two_sided);
	}
      ap->x0 = x0;
      ap->x1 = x1;
      ap->y0 = y0;
      ap->y1 = y1;
      ap->losamp = old_lo;
      ap->hisamp = old_hi;
      init_axis_scales(ap);
    }
  free_track_mix_list(trk);
}


/* ---------------- track dialog ---------------- */

env *track_dialog_env(int n)
{
  if (track_p(n))
    {
      if (tracks[n]->dialog_env == NULL)
	{
	  Float flat[4] = {0.0, 1.0, 1.0, 1.0};
	  env *e;
	  e = active_track_amp_env(n);
	  if (!e)
	    tracks[n]->dialog_env = make_envelope(flat, 4);
	  else tracks[n]->dialog_env = copy_env(e);
	}
      return(tracks[n]->dialog_env);
    }
  return(NULL);
}

void reflect_edit_in_track_dialog_env(int n)
{
  if (track_p(n))
    {
      Float flat[4] = {0.0, 1.0, 1.0, 1.0};
      env *e;
      if (tracks[n]->dialog_env) free_env(tracks[n]->dialog_env);
      e = active_track_amp_env(n);
      if (!e)
	tracks[n]->dialog_env = make_envelope(flat, 4);
      else tracks[n]->dialog_env = copy_env(e);
    }
}

int any_track_id(void)
{
  if (tracks)
    {
      int i;
      for (i = 1; i < track_ctr; i++)
	if (tracks[i]) return(i);
    }
  return(INVALID_TRACK_ID);
}

int next_track_id(int id)
{
  int i;
  for (i = id + 1; i < track_ctr; i++)
    if (tracks[i]) return(i);
  return(INVALID_TRACK_ID);
}

int previous_track_id(int id)
{
  int i;
  for (i = id - 1; i > 0; i--)
    if (tracks[i]) return(i);
  return(INVALID_TRACK_ID);
}

env *track_dialog_track_amp_env(int id) {return(active_track_amp_env(id));}
Float track_dialog_track_speed(int id) {return(active_track_speed(id));}
Float track_dialog_track_tempo(int id) {return(active_track_tempo(id));}
Float track_dialog_track_amp(int id) {return(active_track_amp(id));}
int track_dialog_track_track(int id) {return(active_track_track(id));}
bool track_dialog_track_color_set(int id) {return(active_track_color_set(id));}
color_t track_dialog_track_color(int id) {return(active_track_color(id));}

typedef enum {DRAG_AMP, DRAG_SPEED, DRAG_TEMPO} track_drag_t;

static void track_finish_drag(int track_id, Float amp, track_drag_t field)
{
  track_mix_list_t *trk;
  int i, k;
  mix_info *md;
  mix_state *cs;
  chan_info *cp;
  char *origin = NULL;
  trk = track_mixes(track_id);
  switch (field)
    {
    case DRAG_AMP:   
      set_active_track_amp(track_id, amp); 
#if HAVE_FORTH
      origin = mus_format("%d %.4f set-%s",
			  track_id, amp, S_track_amp);
#else
      origin = mus_format(PROC_SET_TRACK "%.4f" PROC_CLOSE, 
			  TO_PROC_NAME(S_track_amp), track_id, amp);
#endif
      break;
    case DRAG_SPEED: 
      set_active_track_speed(track_id, amp); 
#if HAVE_FORTH
      origin = mus_format("%d %.4f set-%s",
			  track_id, amp, S_track_speed);
#else
      origin = mus_format(PROC_SET_TRACK "%.4f" PROC_CLOSE, 
			  TO_PROC_NAME(S_track_speed), track_id, amp);
#endif
      break;
    case DRAG_TEMPO: 
      set_active_track_tempo(track_id, amp); 
#if HAVE_FORTH
      origin = mus_format("%d %.4f set-%s",
			  track_id, amp, S_track_tempo);
#else
      origin = mus_format(PROC_SET_TRACK "%.4f" PROC_CLOSE, 
			  TO_PROC_NAME(S_track_tempo), track_id, amp);
#endif
      break;
    }

  for (i = 0; i < trk->cps_ctr; i++) 
    {
      trk->cps_squelched[i] = trk->cps[i]->squelch_update;
      trk->cps[i]->squelch_update = true;
    }

  for (i = 0; i < trk->lst_ctr; i++)
    {
      md = md_from_id(trk->lst[i]);
      cs = md->states[md->current_state];
      for (k = 0; k < md->in_chans; k++) 
	cs->as_built->scalers[k] = 0.0;
      remix_file(md, origin, false);
    }
  if (origin) FREE(origin);

  if (track_drag_data)
    {
      for (i = 0; i < trk->cps_ctr; i++)      /* drag is one edit */
	{
	  cp = trk->cps[i];
	  while (cp->edit_ctr > track_drag_data->edpos[i]) backup_edit_list(cp);
	  backup_mix_list(cp, track_drag_data->edpos[i]);
	}
    }

  for (i = 0; i < trk->cps_ctr; i++) 
    trk->cps[i]->squelch_update = trk->cps_squelched[i];

  for (i = 0; i < trk->cps_ctr; i++)
    {
      reflect_edit_history_change(trk->cps[i]);
      update_graph(trk->cps[i]);
    }

  free_track_mix_list(trk);
  reflect_mix_or_track_change(ANY_MIX_ID, track_id, false);
  track_drag_data = free_track_graph(track_drag_data);
}

static bool track_slider_drag_in_progress = false;

void track_dialog_start_slider_drag(int track_id) 
{
  track_slider_drag_in_progress = true;
  track_drag_data = track_save_graph(NULL, track_id);
}

void track_dialog_set_speed(int track_id, Float val) 
{
  if (track_slider_drag_in_progress)
    track_finish_drag(track_id, val, DRAG_SPEED);
  else set_track_speed(track_id, val);
  track_slider_drag_in_progress = false;
}


static void temporary_track_tempo(mix_info *md, void *ptr)
{
  track_tempo_t *tt = (track_tempo_t *)ptr;
  mix_state *cs;
  cs = md->active_mix_state;
  /* use orig to get original position (we're using the original tempo as well) */
  if (cs->orig != tt->beg)
    {
      erase_mix_waveform(md);
      cs->beg = tt->beg + (off_t)((cs->orig - tt->beg) * tt->tempo_mult);  
      draw_mix_waveform(md);
    }
  /*
  make_temporary_graph(md->cp, md, cs);
  */
}

void track_dialog_set_tempo(int track_id, Float val, bool dragging) 
{
  if (!dragging)
    {
      if (track_slider_drag_in_progress)
	track_finish_drag(track_id, val, DRAG_TEMPO);
      else set_track_tempo(track_id, val);
    }
  else
    {
      track_tempo_t tt;
      Float cur_tempo;
      tt.beg = track_position(track_id, ALL_TRACK_CHANS);
      cur_tempo = active_track_tempo(track_id);
      tt.tempo_mult = cur_tempo / val; /* we'll be multiplying for the new diff */
      map_over_track_mixes(track_id, temporary_track_tempo, (void *)(&tt));
    }
  track_slider_drag_in_progress = dragging;
}

void track_dialog_set_amp(int track_id, Float val)
{
  if (track_slider_drag_in_progress)
    track_finish_drag(track_id, val, DRAG_AMP);
  else set_track_amp(track_id, val);
  track_slider_drag_in_progress = false;
}

void track_dialog_set_amp_env(int id, env *e)
{
  set_track_amp_env(id, e);
}

char *track_dialog_track_info(int id)
{
  #define MAX_MIX_IDS 8
  char *str;
  /* track dialog label describing current track */
  track_mix_list_t *trk;
  if (!(track_p(id)))
    return(mus_format("%d is not a track", id));
  trk = track_mixes(id);
  if (trk->lst_ctr > 0)
    {
      int i;
      char *temp;
      mix_info *md;
      int slen;
      slen = MAX_MIX_IDS * 16;
      str = (char *)CALLOC(slen, sizeof(char));
      temp = (char *)CALLOC(16, sizeof(char));
      mus_snprintf(str, slen, "mix%s: ", (trk->lst_ctr > 1) ? "es" : "");
      for (i = 0; (i < MAX_MIX_IDS) && (i < trk->lst_ctr); i++)
	{
	  mus_snprintf(temp, 16, "%s%d", (i > 0) ? ", " : "", trk->lst[i]);
	  strcat(str, temp);
	  md = md_from_id(trk->lst[i]);
	  if (md->active_mix_state->track != id)
	    {
	      mus_snprintf(temp, 16, "(%d)", md->active_mix_state->track);
	      strcat(str, temp);
	    }
	}
      if (trk->lst_ctr > MAX_MIX_IDS)
	strcat(str, " ...");
      FREE(temp);
    }
  else str = mus_format("track %d is empty", id);
  free_track_mix_list(trk);
  return(str);
}



/* ---------------- xen side ---------------- */

static int xen_to_c_track(XEN id, const char *origin)
{
  int track_id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ARG_1, origin, "an integer");
  track_id = XEN_TO_C_INT(id);
  if (!(track_p(track_id)))
    XEN_ERROR(NO_SUCH_TRACK,
	      XEN_LIST_2(C_TO_XEN_STRING(origin),
			 id));
  return(track_id);
}

/* ---------------- track?, track, tracks ---------------- */

static XEN g_track_p(XEN id)
{
  #define H_track_p "(" S_track_p " id) -> " PROC_TRUE " if id refers to an active track"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ONLY_ARG, S_track_p, "an integer");
  return(C_TO_XEN_BOOLEAN(track_p(XEN_TO_C_INT(id))));
}

static XEN g_tracks(void)
{
  #define H_tracks "(" S_tracks ") returns a list of the currently active tracks (the tracks that contain active mixes)"
  int i;
  XEN lst = XEN_EMPTY_LIST;
  for (i = 1; i < track_ctr; i++)
    if (track_p(i))
      lst = XEN_CONS(C_TO_XEN_INT(i), lst);
  return(lst);
}

static XEN g_track(XEN id, XEN chn)
{
  /* given track id, return list of constituent mixes */
  #define H_track "(" S_track " track-id :optional (chn 0)) returns a list of the mixes contained in the given track (and channel if specified)"
  int i, track_id;
  XEN result = XEN_EMPTY_LIST;
  track_mix_list_t *trk;
  track_id = xen_to_c_track(id, S_track);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn), chn, XEN_ARG_2, S_track, "int");  
  trk = track_mixes(track_id);
  if (XEN_INTEGER_P(chn))
    {
      chan_info *cp;
      cp = track_channel(track_id, XEN_TO_C_INT(chn));
      if (cp == NULL)
	{
	  free_track_mix_list(trk);
	  XEN_ERROR(NO_SUCH_CHANNEL,
		    XEN_LIST_2(C_TO_XEN_STRING(S_track),
			       chn));
	}
      for (i = trk->lst_ctr - 1; i >= 0; i--)
	{
	  mix_info *md;
	  md = md_from_id(trk->lst[i]);
	  if (md->cp == cp)
	    result = XEN_CONS(C_TO_XEN_INT(trk->lst[i]), result);
	}
    }
  else
    {
      for (i = trk->lst_ctr - 1; i >= 0; i--)
	result = XEN_CONS(C_TO_XEN_INT(trk->lst[i]), result);
    }
  free_track_mix_list(trk);
  return(result);
}


/* ---------------- make-track ---------------- */

static int new_track(void)
{
  int id;
  if (tracks_size <= track_ctr)
    {
      if (tracks_size == 0)
	{
	  tracks_size = 8;
	  track_ctr = 1; /* track 0 is "no track" indication */
	  tracks = (track_list **)CALLOC(tracks_size, sizeof(track_list *));
	}
      else
	{
	  int i;
	  tracks_size += 8;
	  tracks = (track_list **)REALLOC(tracks, tracks_size * sizeof(track_list *));
	  for (i = track_ctr; i < tracks_size; i++) tracks[i] = NULL;
	}
    }
  id = track_ctr++;
  extend_track_list(id, true);
  return(id);
}

static void make_track_1(mix_info *md, void *val)
{
  remix_file(md, (char *)val, false); /* ORIGIN? */
}

int make_track(int *mixes, int len)
{
  int track_id;
  track_id = new_track();
  if (len > 0)
    {
      mix_info *md;
      int *edpos = NULL;
      chan_info **cps = NULL;
      bool *cps_squelched = NULL;
      mix_state *cs;
      int *old_tracks = NULL;
      int old_tracks_size = 0, old_tracks_ctr = 0;
      off_t *old_tracks_beg = NULL, *old_tracks_len = NULL;
      bool *old_tracks_got_env = NULL;
      int mix_id, k;
      bool got_that_one = false;
      int ctr = 0, i;
      char *origin = NULL;
      origin = mus_format("%s: %d", S_make_track, track_id);
      edpos = (int *)CALLOC(len, sizeof(int));
      cps = (chan_info **)CALLOC(len, sizeof(chan_info *));
      cps_squelched = (bool *)CALLOC(len, sizeof(bool));

      /* collect current track bounds if found-amp-env and not yet collected */
      for (k = 0; k < len; k++)
	{
	  mix_id = mixes[k];
	  md = md_from_id(mix_id);
	  cs = md->active_mix_state;
	  got_that_one = false;
	  for (i = 0; i < ctr; i++)
	    if (cps[i] == md->cp)
	      {
		got_that_one = true;
		break;
	      }
	  if (!got_that_one)
	    {
	      cps[ctr] = md->cp;
	      cps_squelched[ctr] = md->cp->squelch_update;
	      edpos[ctr] = md->cp->edit_ctr + 1;
	      ctr++;
	    }
	  if (track_p(cs->track))
	    {
	      got_that_one = false;
	      for (i = 0; i < old_tracks_ctr; i++)
		if (old_tracks[i] == cs->track)
		  {
		    got_that_one = true;
		    break;
		  }
	      if (!got_that_one)
		{
		  if (old_tracks_ctr == 0)
		    {
		      old_tracks_size = 4;
		      old_tracks = (int *)CALLOC(old_tracks_size, sizeof(int));
		      old_tracks_got_env = (bool *)CALLOC(old_tracks_size, sizeof(bool));
		      old_tracks_beg = (off_t *)CALLOC(old_tracks_size, sizeof(off_t));
		      old_tracks_len = (off_t *)CALLOC(old_tracks_size, sizeof(off_t));
		    }
		  else
		    {
		      if (old_tracks_ctr >= old_tracks_size)
			{
			  old_tracks_size += 4;
			  old_tracks = (int *)REALLOC(old_tracks, old_tracks_size * sizeof(int));
			  old_tracks_got_env = (bool *)REALLOC(old_tracks_got_env, old_tracks_size * sizeof(bool));
			  old_tracks_beg = (off_t *)REALLOC(old_tracks_beg, old_tracks_size * sizeof(off_t));
			  old_tracks_len = (off_t *)REALLOC(old_tracks_len, old_tracks_size * sizeof(off_t));
			}
		    }
		  old_tracks[old_tracks_ctr] = cs->track;
		  old_tracks_got_env[old_tracks_ctr] = ((found_track_amp_env(cs->track)) && (track_members(cs->track) > 1));
		  if (old_tracks_got_env[old_tracks_ctr])
		    {
		      old_tracks_beg[old_tracks_ctr] = track_position(cs->track, ALL_TRACK_CHANS);
		      old_tracks_len[old_tracks_ctr] = track_frames(cs->track, ALL_TRACK_CHANS);

		    }
		  old_tracks_ctr++;
		}
	    }
	}
      /* set new track */
      for (k = 0; k < len; k++)
	{
	  int i;
	  mix_id = mixes[k];
	  md = md_from_id(mix_id);
	  md->active_mix_state->track = track_id;
	  md->states[md->current_state]->track = track_id;
	  for (i = 0; i < md->mix_state_size; i++) 
	    if ((md->states[i]) &&
		(md->states[i]->edit_ctr > md->cp->edit_ctr))
	      md->states[i]->track = track_id;
	}

      /* if any old_tracks above, recheck bounds, if changed remix track */
      if (old_tracks_ctr > 0)
	{
	  for (k = 0; k < old_tracks_ctr; k++)
	    {
	      if (old_tracks_got_env[k])
		{
		  off_t new_track_beg, new_track_len;
		  new_track_beg = track_position(old_tracks[k], ALL_TRACK_CHANS);
		  new_track_len = track_frames(old_tracks[k], ALL_TRACK_CHANS);
		  if ((new_track_beg != old_tracks_beg[k]) ||
		      (new_track_len != old_tracks_len[k]))
		    {
		      remix_track(old_tracks[k], make_track_1, (void *)origin);
		    }
		}
	    }
	}
      if (old_tracks_beg) FREE(old_tracks_beg);
      if (old_tracks_len) FREE(old_tracks_len);
      if (old_tracks) FREE(old_tracks);
      if (old_tracks_got_env) FREE(old_tracks_got_env);

      for (i = 0; i < ctr; i++)
	cps[i]->squelch_update = true;

      /* set up current track */
      for (k = 0; k < len; k++)
	{
	  mix_id = mixes[k];
	  md = md_from_id(mix_id);
	  remix_file(md, origin, false); /* ORIGIN? */
	}

      for (i = 0; i < ctr; i++)
	cps[i]->squelch_update = cps_squelched[i];

      /* make sure this looks like one edit operation */
      for (i = 0; i < ctr; i++)
	{
	  chan_info *cp;
	  cp = cps[i];
	  while (cp->edit_ctr > edpos[i]) backup_edit_list(cp);
	  reflect_edit_history_change(cp);
	  update_graph(cp);
	}
      if (edpos) FREE(edpos);
      if (cps) FREE(cps);
      if (origin) FREE(origin);
    }
  reflect_mix_or_track_change(ANY_MIX_ID, track_id, false);
  return(track_id);
}

#if HAVE_SCHEME
  #define make_track_example "(make-track 1 3)"
  #define track_amp_env_example "(set! (track-amp-env 1) '(0 0 1 1))"
  #define track_color_example "(set! (track-color 1) (make-color 0 0 1))"
#endif
#if HAVE_RUBY
  #define make_track_example "make_track(1, 3)"
  #define track_amp_env_example "set_track_amp_env(1, [0.0, 0.0, 1.0, 1.0])"
  #define track_color_example "set_track_color(1, make_color(0.0, 0.0, 1.0))" 
#endif
#if HAVE_FORTH
  #define make_track_example "1 3 make-track"
  #define track_amp_env_example "1 '( 0.0 0.0 1.0 1.0 ) set-track-amp-env"
  #define track_color_example "1 0.0 0.0 1.0 make-color set-track-color"
#endif

static XEN g_make_track(XEN ids)
{
  #define H_make_track "(" S_make_track "mix-ids...) returns a new track containing the mixes passed as its argument. \
For example, to create a new track containing mixes 1 and 3: " make_track_example "."
  int len = 0, track_id;
  XEN lst;
  int *mixes = NULL;
  for (lst = XEN_COPY_ARG(ids); XEN_NOT_NULL_P(lst); lst = XEN_CDR(lst))
    if ((!(XEN_INTEGER_P(XEN_CAR(lst)))) ||
	(!(mix_ok(XEN_TO_C_INT(XEN_CAR(lst))))))
      XEN_ERROR(XEN_ERROR_TYPE("no-such-mix"),
		XEN_LIST_3(C_TO_XEN_STRING(S_make_track),
			   XEN_CAR(lst),
			   ids));
  len = XEN_LIST_LENGTH(ids);
  if (len > 0)
    {
      int i;
      mixes = (int *)CALLOC(len, sizeof(int));
      for (i = 0, lst = XEN_COPY_ARG(ids); XEN_NOT_NULL_P(lst); lst = XEN_CDR(lst), i++)
	mixes[i] = XEN_TO_C_INT(XEN_CAR(lst));
    }
  /* null arg is ok here -- just open track for later mix adds */
  track_id = make_track(mixes, len);
  if (mixes) FREE(mixes);
  return(xen_return_first(C_TO_XEN_INT(track_id), ids));
}


/* ---------------- free-track ---------------- */

static void unset_track(mix_info *md, int id)
{
  if (md->active_mix_state->track == id)
    set_mix_track(md, 0, true); /* redisplay here? mix dialog? */
}

static void free_track_list(int id)
{
  track_list *tl;
  tl = tracks[id];
  if (tl)
    {
      map_over_mixes_with_int(unset_track, id);
      if (tl->name) {FREE(tl->name); tl->name = NULL;}
      if (tl->states)
	{
	  int i;
	  for (i = 0; i < tl->size; i++)
	    if (tl->states[i])
	      tl->states[i] = free_track_state(tl->states[i]);
	  FREE(tl->states);
	  tl->states = NULL;
	}
      if (tl->dialog_env) 
	tl->dialog_env = free_env(tl->dialog_env);
      /* tl->beg and dur are freed already */
      FREE(tl);
      tracks[id] = NULL;
    }
}

static XEN g_free_track(XEN id)
{
  #define H_free_track "(" S_free_track " id) frees all memory associated with track 'id'. This is not undoable."
  int track_id;
  track_id = xen_to_c_track(id, S_free_track);
  free_track_list(track_id);
  return(XEN_FALSE);
}


/* ---------------- track-name ---------------- */

static XEN g_track_name(XEN id)
{
  #define H_track_name "(" S_track_name " id) -> track's name"
  int track_id;
  track_id = xen_to_c_track(id, S_track_name);
  return(C_TO_XEN_STRING(tracks[track_id]->name));
}

static XEN g_set_track_name(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track(id, S_setB S_track_name);
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ARG_2, S_setB S_track_name, "a string");
  if (tracks[track_id]->name) FREE(tracks[track_id]->name);
  if (XEN_STRING_P(val))
    tracks[track_id]->name = copy_string(XEN_TO_C_STRING(val));
  else tracks[track_id]->name = NULL;
  return(val);
}


/* ---------------- track-tag-y ---------------- */

static XEN g_track_tag_y(XEN id)
{
  #define H_track_tag_y "(" S_track_tag_y " id) -> track's tag height in pixels"
  int track_id;
  track_id = xen_to_c_track(id, S_track_tag_y);
  return(C_TO_XEN_INT(tracks[track_id]->tag_y));
}

static XEN g_set_track_tag_y(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track(id, S_setB S_track_tag_y);
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, S_setB S_track_tag_y, "an integer");
  tracks[track_id]->tag_y = XEN_TO_C_INT(val);
  return(val);
}


/* ---------------- track-amp ---------------- */

static XEN g_track_amp(XEN id)
{
  #define H_track_amp "(" S_track_amp " id) -> track's amp"
  int track_id;
  track_id = xen_to_c_track(id, S_track_amp);
  return(C_TO_XEN_DOUBLE(active_track_amp(track_id)));
}

static XEN g_set_track_amp(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track(id, S_setB S_track_amp);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_track_amp, "a number");
  set_track_amp(track_id, (Float)XEN_TO_C_DOUBLE(val));
  return(val);
}


/* ---------------- track-color ---------------- */

static void set_track_color(int id, color_t color)
{
  /* not considered an edit */
  track_mix_list_t *trk;
  if ((track_p(id)) && (active_track_color(id) != color))
    {
      int i;
      set_active_track_color(id, color);
      trk = track_mixes(id);
      for (i = 0; i < trk->lst_ctr; i++)
	color_one_mix_from_id(trk->lst[i], color); 
      for (i = 0; i < trk->cps_ctr; i++)
	update_graph(trk->cps[i]);
      free_track_mix_list(trk);
      reflect_mix_or_track_change(ANY_MIX_ID, id, false);
    }
}

static void unset_track_color(int id)
{
  /* not considered an edit */
  if ((track_p(id)) && (active_track_color_set(id)))
    {
      track_mix_list_t *trk;
      track_state *ts;
      int i;
      ts = active_track_state(id);
      if (ts) 
	{
	  ts->color = (color_t)0;
	  ts->color_set = false;
	}
      trk = track_mixes(id);
      for (i = 0; i < trk->lst_ctr; i++)
	color_one_mix_from_id(trk->lst[i], ss->sgx->mix_color);
      for (i = 0; i < trk->cps_ctr; i++)
	update_graph(trk->cps[i]);
      free_track_mix_list(trk);
    }
}

static XEN g_track_color(XEN id)
{
  #define H_track_color "(" S_track_color " id) -> track's (track-wide) mix waveform color. \
To display track 1's members in blue: " track_color_example "."
  int track_id;
  track_id = xen_to_c_track(id, S_track_color);
  if (active_track_color_set(track_id))
    return(XEN_WRAP_PIXEL(active_track_color(track_id)));
  return(XEN_FALSE);
}

static XEN g_set_track_color(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track(id, S_setB S_track_color);
  XEN_ASSERT_TYPE(XEN_PIXEL_P(val) || XEN_FALSE_P(val), val, XEN_ARG_2, S_setB S_track_color, "a color");
  if (XEN_FALSE_P(val))
    unset_track_color(track_id);
  else set_track_color(track_id, XEN_UNWRAP_PIXEL(val));
  return(val);
}


/* ---------------- track-speed ---------------- */

static XEN g_track_speed(XEN id)
{
  #define H_track_speed "(" S_track_speed " id) -> track's speed"
  int track_id;
  track_id = xen_to_c_track(id, S_track_speed);
  return(C_TO_XEN_DOUBLE(active_track_speed(track_id)));
}

static XEN g_set_track_speed(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track(id, S_setB S_track_speed);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_track_speed, "a number");
  set_track_speed(track_id, XEN_TO_C_DOUBLE(val));
  return(val);
}

speed_style_t track_speed_style(int id)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) return(ts->speed_style);
  return(speed_control_style(ss));
}

static XEN g_track_speed_style(XEN n)
{
  #define H_track_speed_style "(" S_track_speed_style " id): speed-style choice for track"
  int track_id;
  track_id = xen_to_c_track(n, S_track_speed_style);
  return(C_TO_XEN_INT(track_speed_style(track_id)));
}

speed_style_t set_track_speed_style(int id, speed_style_t choice, bool from_gui)
{
  track_state *ts;
  ts = active_track_state(id);
  if (ts) ts->speed_style = choice;
  if (!from_gui) 
    reflect_mix_or_track_change(id, ANY_TRACK_ID, false);
  return(choice);
}

static XEN g_set_track_speed_style(XEN n, XEN speed)
{
  speed_style_t spd;
  int track_id;
  track_id = xen_to_c_track(n, S_setB S_track_speed_style);
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speed), speed, XEN_ARG_2, S_setB S_track_speed_style, "an integer"); 
  spd = (speed_style_t)XEN_TO_C_INT(speed);
  if (spd > SPEED_CONTROL_AS_SEMITONE)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_track_speed_style, 1, speed, 
			   "~A, but must be " S_speed_control_as_float ", " S_speed_control_as_ratio ", or " S_speed_control_as_semitone);
  set_track_speed_style(track_id, spd, false);
  return(speed);
}



/* ---------------- track-tempo ---------------- */

static XEN g_track_tempo(XEN id)
{
  #define H_track_tempo "(" S_track_tempo " id) -> track's tempo"
  int track_id;
  track_id = xen_to_c_track(id, S_track_tempo);
  return(C_TO_XEN_DOUBLE(active_track_tempo(track_id)));
}

static XEN g_set_track_tempo(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track(id, S_setB S_track_tempo);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_track_tempo, "a number");
  set_track_tempo(track_id, XEN_TO_C_DOUBLE(val));
  return(val);
}


/* ---------------- track-amp-env ---------------- */

static XEN g_track_amp_env(XEN id) 
{
  #define H_track_amp_env "(" S_track_amp_env " id): amplitude envelope applied to all of track's mixes. \
For example, to apply a ramp envelope to the mixes in track 1: " track_amp_env_example "."
  int track_id;
  track_id = xen_to_c_track(id, S_track_amp_env);
  if (active_track_amp_env(track_id))
    return(env_to_xen(active_track_amp_env(track_id)));
  return(XEN_EMPTY_LIST);
}

static XEN g_set_track_amp_env(XEN id, XEN e)
{
  int track_id;
  env *new_e;
  track_id = xen_to_c_track(id, S_setB S_track_amp_env);
  XEN_ASSERT_TYPE(XEN_LIST_P(e) || XEN_FALSE_P(e), e, XEN_ARG_2, S_setB S_track_amp_env, "an envelope (a list of breakpoints) or " PROC_FALSE);
  new_e = xen_to_env(e);
  set_track_amp_env(track_id, new_e); /* copies env */
  new_e = free_env(new_e);
  return(e);
}


/* ---------------- track-track ---------------- */

static XEN g_track_track(XEN id)
{
  #define H_track_track "(" S_track_track " id) -> tracks's track (0 = none); tracks can be members of other tracks."
  int track_id;
  track_id = xen_to_c_track(id, S_track_track);
  return(C_TO_XEN_INT(active_track_track(track_id)));
}

static XEN g_set_track_track(XEN id, XEN trk)
{
  int track_id, its_track;
  track_id = xen_to_c_track(id, S_setB S_track_track);
  XEN_ASSERT_TYPE(XEN_INTEGER_P(trk), trk, XEN_ARG_2, S_setB S_track_track, "an integer");
  its_track = XEN_TO_C_INT(trk);
  if (its_track >= 0)
    {
      if (!(set_track_track(track_id, its_track)))
	XEN_OUT_OF_RANGE_ERROR(S_setB S_track_track, XEN_ARG_2, trk, "a track's track can't be a member of that track");
    }
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_track_track, XEN_ARG_2, trk, "track id must be >= 0");
  return(trk);
}

/* ---------------- track position, track-frames, track-chans  ---------------- */

static XEN g_track_position(XEN id, XEN chn)
{
  #define H_track_position "(" S_track_position " id :optional (chn 0)) -> track's position (location of first mixed sample)"
  int track_id;
  track_id = xen_to_c_track(id, S_track_position);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn), chn, XEN_ARG_2, S_track_position, "integer");
  return(C_TO_XEN_OFF_T(track_position(track_id, XEN_TO_C_INT_OR_ELSE(chn, ALL_TRACK_CHANS))));
}

static XEN g_set_track_position(XEN id, XEN pos, XEN chn)
{
  int track_id;
  track_id = xen_to_c_track(id, S_setB S_track_position);
  if (XEN_NOT_BOUND_P(chn))
    {
      XEN_ASSERT_TYPE(XEN_OFF_T_P(pos), pos, XEN_ARG_2, S_setB S_track_position, "a sample number");
      set_track_position(track_id, XEN_TO_C_OFF_T(pos));
      return(pos);
    }
  else
    {
      /* in this case, (set! (track-position id chn) pos) flips pos and chn */
      XEN_ASSERT_TYPE(XEN_OFF_T_P(chn), chn, XEN_ARG_3, S_setB S_track_position, "a sample number");
      XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(pos), pos, XEN_ARG_2, S_setB S_track_position, "int");
      /* ASSERT_CHANNEL needs index, but it would be better if this only accepted #f */
      set_track_channel_position(track_id, XEN_TO_C_INT(pos), XEN_TO_C_OFF_T(chn));
      return(chn);
    }
}

static XEN g_track_frames(XEN id, XEN chn)
{
  #define H_track_frames "(" S_track_frames " id :optional (chn 0)) -> id's length (samples)"
  int track_id;
  track_id = xen_to_c_track(id, S_track_frames);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chn), chn, XEN_ARG_2, S_track_frames, "int");
  return(C_TO_XEN_OFF_T(track_frames(track_id, (XEN_INTEGER_P(chn)) ? XEN_TO_C_INT(chn) : ALL_TRACK_CHANS)));
}

static XEN g_track_chans(XEN id)
{
  #define H_track_chans "(" S_track_chans " id) -> chans associated with track 'id'. Each such channel \
has at least one active mix that is a member of the given track."
  int track_id;
  track_id = xen_to_c_track(id, S_track_chans);
  return(C_TO_XEN_OFF_T(track_chans(track_id)));
}


/* ---------------- delete-track, lock-track ---------------- */

static void redisplay_track(int id)
{
  int i;
  track_mix_list_t *trk;
  trk = track_mixes(id);
  if (trk->cps_ctr > 0)
    for (i = 0; i < trk->cps_ctr; i++)
      update_graph(trk->cps[i]);
  free_track_mix_list(trk);
}

static void delete_track_1(mix_info *md, void *ignore)
{
  delete_mix_1(md->id, false);
}

static XEN g_delete_track(XEN id)
{
  #define H_delete_track "(" S_delete_track " id) removes all mixes associated with track 'id'"
  int track_id;
  track_id = xen_to_c_track(id, S_delete_track);
  set_active_track_amp(track_id, 0.0);
  remix_track(track_id, delete_track_1, NULL);
  redisplay_track(track_id);
  reflect_mix_or_track_change(ANY_MIX_ID, track_id, false);
  return(id);
}

static void lock_track_1(mix_info *md, void *ignore)
{
  set_mix_locked(md, true, false);
}

static XEN g_lock_track(XEN id)
{
  #define H_lock_track "(" S_lock_track " id) locks all mixes associated with track 'id'."
  int track_id;
  off_t track_beg, track_dur;
  track_id = xen_to_c_track(id, S_lock_track);
  track_beg = track_position(track_id, ALL_TRACK_CHANS);
  track_dur = track_frames(track_id, ALL_TRACK_CHANS);
  if (found_track_amp_env(track_id))
    remix_track_with_preset_times(track_id, track_beg, track_dur, NULL, 0.0, lock_track_1, NULL);
  else remix_track(track_id, lock_track_1, NULL);
  redisplay_track(track_id);
  return(id);
}


/* ---------------- copy-mix, copy-track ---------------- */

static int copy_mix(int id, off_t beg)
{
  int new_id = INVALID_MIX_ID;
  mix_info *md;
  char *origin;
  md = md_from_id(id);
  if (md)
    {
      mix_info *new_md;
      int edpos, i;
      chan_info *cp;
      cp = md->cp;
#if HAVE_FORTH
      origin = mus_format("%d " OFF_TD " %s",
			  id, beg, S_copy_mix);
#else
      origin = mus_format("%s" PROC_OPEN "%d" PROC_SEP OFF_TD PROC_CLOSE, 
			  TO_PROC_NAME(S_copy_mix), id, beg);
#endif
      new_md = file_mix_samples(beg, md->in_samps, md->in_filename, cp, md->orig_chan,
				((md->temporary == DELETE_ME) || (md->temporary == MULTICHANNEL_DELETION)) ? MULTICHANNEL_DELETION : DONT_DELETE_ME,
				origin, true, 0, false);
      if (md->temporary == DELETE_ME) md->temporary = MULTICHANNEL_DELETION;
      edpos = cp->edit_ctr;
      new_id = new_md->id;
      new_md->active_mix_state->track = 0;
      new_md->tag_position = md->tag_position;
      new_md->tag_y = md->tag_y;
      new_md->speed_style = md->speed_style;
      if (md->current_state > 0)
	{
	  mix_state *cs, *old_cs;
	  old_cs = md->states[md->current_state];
	  cs = new_md->active_mix_state;
	  for (i = 0; i < cs->chans; i++) 
	    cs->scalers[i] = old_cs->scalers[i];
	  cs->speed = old_cs->speed;
	  if (old_cs->amp_envs)
	    {
	      cs->amp_envs = (env **)CALLOC(old_cs->chans, sizeof(env *));
	      for (i = 0; i < old_cs->chans; i++) 
		cs->amp_envs[i] = copy_env(old_cs->amp_envs[i]);
	    }
	  else cs->amp_envs = NULL;
	  cs->len = old_cs->len;
	  reflect_mix_or_track_change(new_id, ANY_TRACK_ID, false);
	  remix_file(new_md, origin, true);
	  while (cp->edit_ctr > edpos) backup_edit_list(cp);
	  backup_mix_list(cp, edpos); /* needed if track exists and imposes changes on mixed-in data */
	}
      FREE(origin);
    }
  return(new_id);
}

static int make_initial_track_state(int id)
{
  track_state *old_ts, *new_ts;
  int new_id;
  new_id = new_track();
  new_ts = active_track_state(new_id);
  old_ts = active_track_state(id);
  new_ts->amp = old_ts->amp;
  new_ts->speed = old_ts->speed;
  new_ts->speed_style = old_ts->speed_style;
  new_ts->tempo = old_ts->tempo;
  new_ts->track = 0;
  new_ts->amp_env = copy_env(old_ts->amp_env);
  new_ts->color = old_ts->color;
  new_ts->color_set = old_ts->color_set;
  return(new_id);
}

static void copy_track_1(mix_info *md, void *ignore)
{
  remix_file(md, S_copy_track, false); /* ORIGIN? */
}

static int copy_track(int id, off_t beg)
{
  int new_id = INVALID_TRACK_ID, i, trk_ctr = 0, trk_size = 0, j, k, tid;
  int *old_tracks = NULL, *new_mixes = NULL, *new_tracks = NULL, *edpos = NULL;
  track_mix_list_t *trk;
  track_state *ts;
  mix_info *md;
  mix_state *cs;
  bool found_track = false;
  if (!(track_p(id))) return(INVALID_TRACK_ID);
  trk = track_mixes(id);
  if (trk->lst_ctr > 0)
    {
      off_t old_beg, change;
      edpos = (int *)CALLOC(trk->cps_ctr, sizeof(int));
      for (i = 0; i < trk->cps_ctr; i++)
	edpos[i] = trk->cps[i]->edit_ctr + 1; /* prepare global as_one_edit */
      old_beg = track_position(id, ALL_TRACK_CHANS);
      change = beg - old_beg;
      new_mixes = (int *)CALLOC(trk->lst_ctr, sizeof(int));
      trk_size = 4;
      old_tracks = (int *)CALLOC(trk_size, sizeof(int));
      for (i = 0; i < trk->lst_ctr; i++)
	{
	  md = md_from_id(trk->lst[i]);
	  cs = md->states[md->current_state];
	  new_mixes[i] = copy_mix(trk->lst[i], cs->beg + change);
	  if (md->active_mix_state->track != id)
	    {
	      tid = md->active_mix_state->track;
	      while ((track_p(tid)) && (tid != id))
		{
		  found_track = false;
		  for (k = 0; k < trk_ctr; k++)
		    if (old_tracks[k] == tid)
		      {
			found_track = true;
			break;
		      }
		  if (!found_track)
		    {
		      old_tracks[trk_ctr++] = tid;
		      if (trk_ctr >= trk_size)
			{
			  trk_size += 4;
			  old_tracks = (int *)REALLOC(old_tracks, trk_size * sizeof(int));
			  for (j = trk_size - 4; j < trk_size; j++) old_tracks[j] = 0;
			}
		    }
		  tid = active_track_track(tid);
		}
	    }
	}
    }
  /* now we have copied all the constiuent mixes and placed them at their new position */
  /* new_mixes[i] corresponds to trk->lst[i] */
  /* and we have all the non-top-level tracks that will intervene */
  if (trk_ctr > 0)
    {
      new_tracks = (int *)CALLOC(trk_ctr, sizeof(int));
      for (i = 0; i < trk_ctr; i++)
	new_tracks[i] = make_initial_track_state(old_tracks[i]);
    }
  new_id = make_initial_track_state(id);
  
  if (trk->lst_ctr > 0)
    {
      /* now all the embedded tracks have been copied */
      /* next step is to run through the mixes in parallel, setting all the track->track and mix->track fields */
      for (i = 0; i < trk_ctr; i++)
	{
	  /* old_tracks[i] -> track sets new_tracks[i] -> track to corresponding value */
	  tid = active_track_track(old_tracks[i]);
	  ts = active_track_state(new_tracks[i]);
	  if (tid == id)
	    ts->track = new_id;
	  else
	    {
	      for (j = 0; j < trk_ctr; j++)
		if (old_tracks[j] == tid)
		  {
		    ts->track = new_tracks[j];
		    break;
		  }
	    }
	}
      for (i = 0; i < trk->lst_ctr; i++)
	{
	  mix_info *old_md, *new_md;
	  old_md = md_from_id(trk->lst[i]);
	  new_md = md_from_id(new_mixes[i]);
	  if (old_md->active_mix_state->track == id)
	    new_md->active_mix_state->track = new_id;
	  else
	    {
	      for (j = 0; j < trk_ctr; j++)
		if (old_tracks[j] == old_md->active_mix_state->track)
		  {
		    new_md->active_mix_state->track = new_tracks[j];
		    break;
		  }
	    }
	}
      /* now all the track pointers have been set */
      remix_track(new_id, copy_track_1, NULL);
      /* that's locally backed-up, but we also copied all the mixes above */
      for (i = 0; i < trk->cps_ctr; i++)
	{
	  chan_info *cp;
	  cp = trk->cps[i];
	  while (cp->edit_ctr > edpos[i]) backup_edit_list(cp);
	  backup_mix_list(cp, edpos[i]);
	}
      FREE(edpos);
      for (i = 0; i < trk->cps_ctr; i++)
	update_graph(trk->cps[i]);

    }
  free_track_mix_list(trk);
  if (new_mixes) FREE(new_mixes);
  if (old_tracks) FREE(old_tracks);
  if (new_tracks) FREE(new_tracks);
  reflect_mix_or_track_change(ANY_MIX_ID, new_id, false);
  return(new_id);
}

static XEN g_copy_mix(XEN id, XEN beg)
{
  #define H_copy_mix "(" S_copy_mix " mix-id :optional (beg mix-beg)) copies the given mix, placing \
the copy at 'beg' which defaults to the copied mix's position."

  int new_id, old_id;
  off_t pos;
  mix_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ARG_1, S_copy_mix, "int");
  ASSERT_SAMPLE_TYPE(S_copy_mix, beg, 2);
  old_id = XEN_TO_C_INT(id);
  cs = cs_from_id(old_id);
  if (cs == NULL)
    return(snd_no_such_mix_error(S_copy_mix, id));
  pos = XEN_TO_C_OFF_T_OR_ELSE(beg, cs->beg);
  new_id = copy_mix(old_id, pos);
  return(C_TO_XEN_INT(new_id));
}

static XEN g_copy_track(XEN id, XEN beg)
{
  #define H_copy_track "(" S_copy_track " track-id :optional (beg track-beg)) copies the given track, placing \
the copy at 'beg' which defaults to the copied track's position."

  int new_id, old_id;
  off_t old_pos, pos;
  new_id = xen_to_c_track(id, S_copy_track);
  ASSERT_SAMPLE_TYPE(S_copy_track, beg, 2);
  old_id = XEN_TO_C_INT(id);
  if (!(track_p(old_id)))
    XEN_ERROR(NO_SUCH_TRACK,
	      XEN_LIST_2(C_TO_XEN_STRING(S_copy_track),
			 id));
  old_pos = track_position(old_id, ALL_TRACK_CHANS);
  pos = XEN_TO_C_OFF_T_OR_ELSE(beg, old_pos);
  new_id = copy_track(old_id, pos);
  return(C_TO_XEN_INT(new_id));
}


/* ---------------- track-reader, play-track ---------------- */

/* track reader: an array of mix readers with state: active, waiting, null (done) */
typedef struct track_fd {
  int mixes, track, initial_chan;
  off_t loc;
  off_t *state;
  off_t *len;
  mix_fd **fds;
} track_fd;

static track_fd *init_track_sample_reader(int track_num, int chan, off_t beg, bool global)
{
  /* beg is offset within track which starts within overall sound(s) at track_beg */
  off_t track_beg = UNLIKELY_POSITION;
  chan_info *cp;
  track_fd *fd = NULL;
  int mixes = 0, i, mix;
  track_mix_list_t *trk;
  mix_info *md;
  mix_state *cs;
  trk = track_mixes(track_num);
  if ((chan >= 0) && (chan < trk->cps_ctr))
    {
      cp = trk->cps[chan];
      for (i = 0; i < trk->lst_ctr; i++)
	{
	  md = md_from_id(trk->lst[i]);
	  if (md->cp == cp) mixes++;
	  cs = md->active_mix_state;
	  if ((global) || (md->cp == cp))
	    if (cs->orig < track_beg) 
	      track_beg = cs->orig;
	}
      if (mixes > 0)
	{
	  fd = (track_fd *)CALLOC(1, sizeof(track_fd));
	  fd->track = track_num;
	  fd->initial_chan = chan;
	  fd->loc = beg;
	  fd->mixes = mixes;
	  fd->state = (off_t *)CALLOC(mixes, sizeof(off_t));
	  fd->len = (off_t *)CALLOC(mixes, sizeof(off_t));
	  fd->fds = (mix_fd **)CALLOC(mixes, sizeof(mix_fd *));
	  mix = 0;
	  for (i = 0; i < trk->lst_ctr; i++)
	    {
	      md = md_from_id(trk->lst[i]);
	      if (md->cp == cp)
		{
		  off_t true_beg, mix_start;
		  true_beg = track_beg + beg;
		  cs = md->active_mix_state;
		  mix_start = true_beg - cs->orig;
		  fd->state[mix] = -mix_start;
		  if (mix_start > 0)
		    {
		      fd->fds[mix] = init_mix_read(md, CURRENT_MIX, mix_start);
		      fd->len[mix] = cs->len - mix_start;
		    }
		  else 
		    {
		      fd->fds[mix] = init_mix_read(md, CURRENT_MIX, 0);
		      fd->len[mix] = cs->len;
		    }

		  if (fd->fds[mix])
		    list_mix_reader(fd->fds[mix]); /* was in g_make... */
		  mix++;
		}
	    }
	}
    }
  free_track_mix_list(trk);
  return(fd);
}

static void free_track_fd_almost(track_fd *fd)
{
  if (fd)
    {
      if (fd->fds)
	{
	  int i;
	  for (i = 0; i < fd->mixes; i++)
	    fd->fds[i] = free_mix_fd(fd->fds[i]);
	  FREE(fd->fds);
	  fd->fds = NULL;
	}
      fd->mixes = 0;
      if (fd->state) {FREE(fd->state); fd->state = NULL;}
      if (fd->len) {FREE(fd->len); fd->len = NULL;}
    }
}

static track_fd *free_track_fd(track_fd *fd)
{
  if (fd)
    {
      free_track_fd_almost(fd);
      FREE(fd);
    }
  return(NULL);
}

static Float next_track_sample(track_fd *fd)
{
  Float sum = 0.0;
  if (fd)
    {
      int i;
      bool eof = true;
      for (i = 0; i < fd->mixes; i++)
	if ((fd->fds[i]) && 
	    (fd->len[i] > 0))
	  {
	    if (fd->state[i] <= 0)
	      {
		sum += next_mix_sample(fd->fds[i]);
		fd->len[i]--;
		if (fd->len[i] <= 0) 
		  fd->fds[i] = free_mix_fd(fd->fds[i]);
		else eof = false;
	      }
	    else 
	      {
		fd->state[i]--;
		eof = false;
	      }
	  }
      if (eof)
	fd->loc = -1;
      else fd->loc++;
    }
  return(sum);
}

Float track_read_sample_to_float(struct track_fd *ptr) {return(next_track_sample(ptr));}

static void play_track(int track_num, int chan, off_t beg, bool from_gui)
{
  track_fd **fds;
  snd_info *sp;
  int playfd, i, j, k, n, samps = 0, chans, format, datum_bytes, outchans, frames;
#if HAVE_ALSA
  mus_sample_t **buf;
  char *outbuf;
  float val[4]; /* not Float */
  int err;
#else
  OutSample *buf;
#endif
  track_mix_list_t *trk;

  trk = track_mixes(track_num);
  if (chan == -1) chans = trk->cps_ctr; else chans = 1;
  if (chans == 0) 
    {
      free_track_mix_list(trk);
      return;
    }
  sp = trk->cps[0]->sound;
  outchans = chans;
  format = mus_audio_compatible_format(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss));
  datum_bytes = mus_bytes_per_sample(format);
  frames = 256;
  fds = (track_fd **)CALLOC(chans, sizeof(track_fd *));

#if HAVE_ALSA
  /* in ALSA we have no way to tell what the possible output format is, or min chans, so... */
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_CHANNEL, 2, val);
  if (outchans < (int)(val[1])) outchans = (int)(val[1]);
  if ((err != MUS_NO_ERROR) || (outchans <= 0))
    {
      clear_minibuffer(sp);
      snd_error_without_format(_("can't get basic soundcard info!"));
      return;
    }
  err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, val);
  frames = (int)(val[0]);
  if ((err != MUS_NO_ERROR) || (frames <= 0))
    {
      clear_minibuffer(sp);
      snd_error(_("samples per channel is %d?"), frames);
      return;
    }
  set_dac_size(outchans * frames * mus_bytes_per_sample(format));
  buf = (mus_sample_t **)CALLOC(outchans, sizeof(mus_sample_t *));
  for (i = 0; i < chans; i++) buf[i] = (mus_sample_t *)CALLOC(frames, sizeof(mus_sample_t));
  outbuf = (char *)CALLOC(frames * datum_bytes * outchans, sizeof(char));
#else
  buf = (OutSample *)CALLOC(chans * frames, sizeof(OutSample));
#endif
  for (i = 0; i < chans; i++)
    {
      fds[i] = init_track_sample_reader(track_num, i, beg, (chan == -1));
      if (fds[i]) /* perhaps bad track number? */
	for (n = 0; n < fds[i]->mixes; n++)
	  {
	    j = fds[i]->state[n] + fds[i]->len[n];
	    if (j > samps) samps = j;
	  }
    }
  if (samps > 0)
    {
      clear_minibuffer(sp);
      playfd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), 
				     SND_SRATE(sp), 
				     outchans, 
				     format, 
				     dac_size(ss));
      if (playfd != -1)
	{
	  ss->stopped_explicitly = false;
	  for (i = 0; i < samps; i += frames)
	    {
#if HAVE_ALSA
	      for (k = 0; k < chans; k++)
		if (fds[k])
		  for (j = 0; j < frames; j++)
		    buf[k][j] = MUS_FLOAT_TO_SAMPLE(next_track_sample(fds[k]));
	      mus_file_write_buffer(format, 0, frames - 1, outchans, buf, outbuf, true);
	      mus_audio_write(playfd, outbuf, frames * datum_bytes * outchans);
#else
	      for (k = 0; k < chans; k++)
		if (fds[k])
		  for (j = k; j < frames * chans; j += chans)
		    buf[j] = MUS_CONVERT(next_track_sample(fds[k]));
	      mus_audio_write(playfd, (char *)buf, frames * datum_bytes * chans);
#endif
	      check_for_event();
	      if ((ss->stopped_explicitly) || 
		  ((from_gui) && (track_play_stopped())) ||
		  (!(sp->active)))
		{
		  ss->stopped_explicitly = false;
		  string_to_minibuffer(sp, _("stopped"));
		  break;
		}
	    }
	  mus_audio_close(playfd);
	}
    }
  for (i = 0; i < chans; i++) free_track_fd(fds[i]);
  reflect_track_play_stop();
  FREE(fds);
#if HAVE_ALSA
  for (i = 0; i < chans; i++) if (buf[i]) FREE(buf[i]);
  FREE(outbuf);
#endif
  FREE(buf);
  free_track_mix_list(trk);
}

void mix_dialog_track_play(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (!md) return;
  if (md->active_mix_state->track != 0)
    play_track(md->active_mix_state->track, -1, 0, true);
  else play_mix(md, 0, true);
}

void track_dialog_play(int track_id)
{
  if (track_p(track_id))
    play_track(track_id, -1, 0, true);
}

static XEN g_play_track(XEN id, XEN chn, XEN beg)
{
  #define H_play_track "(" S_play_track " track :optional chn (beg 0)): play track. If 'chn' is " PROC_TRUE ", \
play all the mixes in the track, even if in different channels.  'beg' is where to start playing within the track."
  int track_id;
  off_t samp;
  track_id = xen_to_c_track(id, S_play_track);
  /* in this case if chn=#t, play all associated mixes in all chans */
  ASSERT_SAMPLE_TYPE(S_play_track, beg, XEN_ARG_3);
  samp = beg_to_sample(beg, S_play_track);
  if (!(XEN_INTEGER_P(chn)))
    play_track(track_id, -1, samp, false);
  else play_track(track_id, XEN_TO_C_INT(chn), samp, false);
  return(id);
}


/* ---------------- track sample readers ---------------- */

static XEN_OBJECT_TYPE tf_tag;
bool tf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, tf_tag));}
#define TO_TRACK_SAMPLE_READER(obj) ((track_fd *)XEN_OBJECT_REF(obj))
#define TRACK_SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, tf_tag)
struct track_fd *get_tf(XEN obj) {if (TRACK_SAMPLE_READER_P(obj)) return(TO_TRACK_SAMPLE_READER(obj)); else return(NULL);}

static XEN g_tf_p(XEN obj) 
{
  #define H_tf_p "(" S_track_sample_reader_p " obj): " PROC_TRUE " if obj is a track-sample-reader"
  return(C_TO_XEN_BOOLEAN(tf_p(obj)));
}

static char *tf_to_string(track_fd *fd) 
{
  char *desc;
  int desc_len;
  desc_len = PRINT_BUFFER_SIZE;
  desc = (char *)CALLOC(desc_len, sizeof(char));
  if (fd == NULL)
    sprintf(desc, "#<track-sample-reader: null>");
  else
    {
      bool happy = false, banner = false, previous = false;
      if ((fd->fds) && (fd->mixes > 0))
	{
	  int i, len;
	  len = fd->mixes;
	  for (i = 0; i < len; i++)
	    {
	      mix_fd *mf = NULL;
	      mf = fd->fds[i];
	      if ((mf) && (mf->md) && (MIX_TYPE_OK(mf->type)))
		{
		  char toi[16];
		  mix_info *md;
		  md = mf->md;
		  happy = true;
		  if (!banner)
		    {
		      mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<track-sample-reader track %d: %s chan %d via mixes '(",
				   fd->track,
				   md->in_filename,
				   md->cp->chan);
		      banner = true;
		    }
		  mus_snprintf(toi, 16, "%s%d", (previous) ? " ": "", mf->md->id);
		  previous = true;
		  desc = snd_strcat(desc, toi, &desc_len);
		}
	    }
	  desc = snd_strcat(desc, ")>", &desc_len);
	}
      if (!happy)
	mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<track-sample-reader %p: inactive>", fd);
    }
  return(desc);
}

char *run_track_sample_reader_to_string(struct track_fd *ptr) {return(tf_to_string(ptr));}


XEN_MAKE_OBJECT_PRINT_PROCEDURE(track_fd, print_tf, tf_to_string)

static void tf_free(track_fd *fd)
{
  if (fd) 
    {
      if (fd->fds)
	{
	  int i;
	  for (i = 0; i < fd->mixes; i++)
	    if (fd->fds[i]) 
	      unlist_mix_reader(fd->fds[i]);
	}
      free_track_fd(fd); 
    }
}

void run_free_track_sample_reader(struct track_fd *ptr) {tf_free(ptr);}

XEN_MAKE_OBJECT_FREE_PROCEDURE(track_fd, free_tf, tf_free)

static XEN g_make_track_sample_reader(XEN id, XEN chn, XEN beg)
{
  #define H_make_track_sample_reader "(" S_make_track_sample_reader " track :optional chn (beg 0)): \
return a reader ready to access track's data associated with track's channel chn, starting in the track from beg"

  track_fd *tf = NULL;
  int chan;
  off_t samp;
  int track_id;
  track_id = xen_to_c_track(id, S_make_track_sample_reader);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chn), chn, XEN_ARG_2, S_make_track_sample_reader, "int");
  ASSERT_SAMPLE_TYPE(S_make_track_sample_reader, beg, XEN_ARG_3);
  samp = beg_to_sample(beg, S_make_track_sample_reader);
  chan = XEN_TO_C_INT_OR_ELSE(chn, 0);
  tf = init_track_sample_reader(track_id, chan, samp, false); 
  if (tf == NULL)
    XEN_ERROR(NO_SUCH_CHANNEL,
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_track_sample_reader),
			 id, chn));
  XEN_MAKE_AND_RETURN_OBJECT(tf_tag, tf, 0, free_tf);
}

struct track_fd *run_make_track_sample_reader(int id, int chan, off_t beg)
{
  return(init_track_sample_reader(id, chan, beg, false));
}

static XEN g_read_track_sample(XEN obj)
{
  #define H_read_track_sample "(" S_read_track_sample " reader): read sample from track reader"
  XEN_ASSERT_TYPE(TRACK_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_track_sample, "a track-sample-reader");
  return(C_TO_XEN_DOUBLE(next_track_sample(TO_TRACK_SAMPLE_READER(obj))));
}


/* tie into generic sample-reader procedures (snd-edits.c) */

XEN g_free_track_sample_reader(XEN obj)
{
  track_fd *tf = NULL;
  tf = TO_TRACK_SAMPLE_READER(obj);
  if ((tf) && (tf->fds))
    {
      int i;
      for (i = 0; i < tf->mixes; i++)
	if (tf->fds[i]) 
	  unlist_mix_reader(tf->fds[i]);
    }
  free_track_fd_almost(tf);
  return(xen_return_first(XEN_FALSE, obj));
}

XEN g_copy_track_sample_reader(XEN obj)
{
  track_fd *tf;
  tf = TO_TRACK_SAMPLE_READER(obj);
  if (tf->loc != -1)
    return(g_make_track_sample_reader(C_TO_XEN_INT(tf->track),
				      C_TO_XEN_INT(tf->initial_chan),
				      C_TO_XEN_OFF_T(tf->loc)));
  return(XEN_FALSE);
}

XEN g_track_sample_reader_home(XEN obj)
{
  /* returns (list track-id initial-chan) */
  track_fd *tf;
  tf = TO_TRACK_SAMPLE_READER(obj);
  return(XEN_LIST_2(C_TO_XEN_INT(tf->track),
		    C_TO_XEN_INT(tf->initial_chan)));
}

bool track_sample_reader_at_end_p(struct track_fd *tf)
{
  return(tf->loc == -1);
}

XEN g_track_sample_reader_at_end_p(XEN obj)
{
  track_fd *tf;
  tf = TO_TRACK_SAMPLE_READER(obj);
  return(C_TO_XEN_BOOLEAN(tf->loc == -1));
}

XEN g_track_sample_reader_position(XEN obj)
{
  track_fd *tf;
  tf = TO_TRACK_SAMPLE_READER(obj);
  return(C_TO_XEN_OFF_T(tf->loc));
}


/* -------- mix/track dialog ---------------- */

static XEN g_mix_dialog_mix(void)
{
  #define H_mix_dialog_mix "(" S_mix_dialog_mix ") -> current mix id displayed in mix dialog."
  return(C_TO_XEN_INT(mix_dialog_mix()));
}

static XEN g_set_mix_dialog_mix(XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_dialog_mix, "mix id (an int)");
  mix_dialog_set_mix(XEN_TO_C_INT(val));
  return(val);
}

static XEN g_track_dialog_track(void)
{
  #define H_track_dialog_track "(" S_track_dialog_track ") -> current track id displayed in track dialog."
  return(C_TO_XEN_INT(track_dialog_track()));
}

static XEN g_set_track_dialog_track(XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_track_dialog_track, "track id (an int)");
  track_dialog_set_track(XEN_TO_C_INT(val));
  return(val);
}

static XEN g_view_mixes_dialog(void)
{
  widget_t w;
  #define H_view_mixes_dialog "(" S_view_mixes_dialog "): start the Mix browser"
  w = make_mix_dialog();
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_view_tracks_dialog(void)
{
  widget_t w;
  #define H_view_tracks_dialog "(" S_view_tracks_dialog "): start the Track browser"
  w = make_track_dialog();
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_tempo_control_bounds(void) 
{
  #define H_tempo_control_bounds "(" S_tempo_control_bounds "): current (Track dialog) tempo slider bounds (default: '(0.0 8.0))"
  return(XEN_LIST_2(C_TO_XEN_DOUBLE(tempo_control_min(ss)), 
		    C_TO_XEN_DOUBLE(tempo_control_max(ss))));
}

static XEN g_set_tempo_control_bounds(XEN on) 
{
  XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_tempo_control_bounds, "a list of the new min and max values"); 
  if ((XEN_LIST_LENGTH(on) != 2) ||
      (!(XEN_NUMBER_P(XEN_CAR(on)))) ||
      (!(XEN_NUMBER_P(XEN_CADR(on)))))
    XEN_WRONG_TYPE_ARG_ERROR(S_setB S_tempo_control_bounds, XEN_ARG_1, on, "a list of 2 numbers");
  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) >= XEN_TO_C_DOUBLE(XEN_CADR(on)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_tempo_control_bounds, 1, on, "min >= max");

  in_set_tempo_control_min(ss, XEN_TO_C_DOUBLE(XEN_CAR(on)));
  in_set_tempo_control_max(ss, XEN_TO_C_DOUBLE(XEN_CADR(on)));
  reflect_mix_or_track_change(ANY_MIX_ID, track_dialog_track(), false);
  return(on);
}



/* ---------------- init track xen connection ---------------- */

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_3(g_make_track_sample_reader_w, g_make_track_sample_reader)
XEN_NARGIFY_1(g_read_track_sample_w, g_read_track_sample)
XEN_NARGIFY_1(g_tf_p_w, g_tf_p)
XEN_ARGIFY_3(g_play_track_w, g_play_track)
XEN_NARGIFY_1(g_free_track_w, g_free_track)
XEN_NARGIFY_1(g_track_p_w, g_track_p)
XEN_NARGIFY_0(g_tracks_w, g_tracks)
XEN_ARGIFY_2(g_track_w, g_track)
XEN_NARGIFY_1(g_track_color_w, g_track_color)
XEN_NARGIFY_2(g_set_track_color_w, g_set_track_color)
XEN_NARGIFY_1(g_track_name_w, g_track_name)
XEN_NARGIFY_2(g_set_track_name_w, g_set_track_name)
XEN_NARGIFY_1(g_track_tag_y_w, g_track_tag_y)
XEN_NARGIFY_2(g_set_track_tag_y_w, g_set_track_tag_y)
XEN_NARGIFY_1(g_track_amp_w, g_track_amp)
XEN_NARGIFY_2(g_set_track_amp_w, g_set_track_amp)
XEN_NARGIFY_1(g_track_speed_w, g_track_speed)
XEN_NARGIFY_2(g_set_track_speed_w, g_set_track_speed)
XEN_NARGIFY_1(g_track_speed_style_w, g_track_speed_style)
XEN_NARGIFY_2(g_set_track_speed_style_w, g_set_track_speed_style)
XEN_NARGIFY_1(g_track_tempo_w, g_track_tempo)
XEN_NARGIFY_2(g_set_track_tempo_w, g_set_track_tempo)
XEN_NARGIFY_1(g_track_amp_env_w, g_track_amp_env)
XEN_NARGIFY_2(g_set_track_amp_env_w, g_set_track_amp_env)
XEN_ARGIFY_2(g_track_position_w, g_track_position)
XEN_ARGIFY_3(g_set_track_position_w, g_set_track_position)
XEN_NARGIFY_1(g_track_track_w, g_track_track)
XEN_NARGIFY_1(g_track_chans_w, g_track_chans)
XEN_NARGIFY_2(g_set_track_track_w, g_set_track_track)
XEN_ARGIFY_2(g_track_frames_w, g_track_frames)
XEN_NARGIFY_1(g_delete_track_w, g_delete_track)
XEN_NARGIFY_1(g_lock_track_w, g_lock_track)
XEN_VARGIFY(g_make_track_w, g_make_track)
XEN_ARGIFY_2(g_copy_mix_w, g_copy_mix)
XEN_ARGIFY_2(g_copy_track_w, g_copy_track)
XEN_NARGIFY_0(g_mix_dialog_mix_w, g_mix_dialog_mix)
XEN_NARGIFY_1(g_set_mix_dialog_mix_w, g_set_mix_dialog_mix)
XEN_NARGIFY_0(g_track_dialog_track_w, g_track_dialog_track)
XEN_NARGIFY_1(g_set_track_dialog_track_w, g_set_track_dialog_track)
XEN_NARGIFY_0(g_view_mixes_dialog_w, g_view_mixes_dialog)
XEN_NARGIFY_0(g_view_tracks_dialog_w, g_view_tracks_dialog)
XEN_NARGIFY_0(g_tempo_control_bounds_w, g_tempo_control_bounds)
XEN_NARGIFY_1(g_set_tempo_control_bounds_w, g_set_tempo_control_bounds)
#else
#define g_make_track_sample_reader_w g_make_track_sample_reader
#define g_read_track_sample_w g_read_track_sample
#define g_tf_p_w g_tf_p
#define g_play_track_w g_play_track
#define g_free_track_w g_free_track
#define g_track_p_w g_track_p
#define g_tracks_w g_tracks
#define g_track_w g_track
#define g_track_color_w g_track_color
#define g_set_track_color_w g_set_track_color
#define g_track_name_w g_track_name
#define g_set_track_name_w g_set_track_name
#define g_track_tag_y_w g_track_tag_y
#define g_set_track_tag_y_w g_set_track_tag_y
#define g_track_amp_w g_track_amp
#define g_set_track_amp_w g_set_track_amp
#define g_track_speed_w g_track_speed
#define g_set_track_speed_w g_set_track_speed
#define g_track_speed_style_w g_track_speed_style
#define g_set_track_speed_style_w g_set_track_speed_style
#define g_track_tempo_w g_track_tempo
#define g_set_track_tempo_w g_set_track_tempo
#define g_track_amp_env_w g_track_amp_env
#define g_set_track_amp_env_w g_set_track_amp_env
#define g_track_position_w g_track_position
#define g_set_track_position_w g_set_track_position
#define g_track_track_w g_track_track
#define g_set_track_track_w g_set_track_track
#define g_track_frames_w g_track_frames
#define g_track_chans_w g_track_chans
#define g_delete_track_w g_delete_track
#define g_lock_track_w g_lock_track
#define g_make_track_w g_make_track
#define g_copy_mix_w g_copy_mix
#define g_copy_track_w g_copy_track
#define g_mix_dialog_mix_w g_mix_dialog_mix
#define g_set_mix_dialog_mix_w g_set_mix_dialog_mix
#define g_track_dialog_track_w g_track_dialog_track
#define g_set_track_dialog_track_w g_set_track_dialog_track
#define g_view_mixes_dialog_w g_view_mixes_dialog
#define g_view_tracks_dialog_w g_view_tracks_dialog
#define g_tempo_control_bounds_w g_tempo_control_bounds
#define g_set_tempo_control_bounds_w g_set_tempo_control_bounds
#endif

void g_init_track(void)
{
#if (!HAVE_GAUCHE)
  tf_tag = XEN_MAKE_OBJECT_TYPE("TrackSampleReader", sizeof(track_fd));
#else
  tf_tag = XEN_MAKE_OBJECT_TYPE("<track-sample-reader>", sizeof(track_fd), print_tf, free_tf);
  XEN_EVAL_C_STRING("(define-method object-apply ((rd <track-sample-reader>)) (read-track-sample rd))");
#endif

#if HAVE_RUBY
  rb_define_method(tf_tag, "to_s", XEN_PROCEDURE_CAST print_tf, 0);
  rb_define_method(tf_tag, "call", XEN_PROCEDURE_CAST g_read_track_sample, 0);
#endif

#if HAVE_GUILE
  scm_set_smob_print(tf_tag, print_tf);
  scm_set_smob_free(tf_tag, free_tf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(tf_tag, XEN_PROCEDURE_CAST g_read_track_sample, 0, 0, 0);
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(tf_tag, print_tf);
  fth_set_object_free(tf_tag, free_tf);
  fth_set_object_apply(tf_tag, XEN_PROCEDURE_CAST g_read_track_sample, 0, 0, 0);
#endif

  XEN_DEFINE_PROCEDURE(S_make_track_sample_reader, g_make_track_sample_reader_w, 1, 2, 0, H_make_track_sample_reader);
  XEN_DEFINE_PROCEDURE(S_read_track_sample,        g_read_track_sample_w,        1, 0, 0, H_read_track_sample);
  XEN_DEFINE_PROCEDURE(S_track_sample_reader_p,    g_tf_p_w,                     1, 0, 0, H_tf_p);
  XEN_DEFINE_PROCEDURE(S_play_track,               g_play_track_w,               1, 2, 0, H_play_track);
  XEN_DEFINE_PROCEDURE(S_free_track,               g_free_track_w,               1, 0, 0, H_free_track);
  XEN_DEFINE_PROCEDURE(S_track_frames,             g_track_frames_w,             1, 1, 0, H_track_frames);
  XEN_DEFINE_PROCEDURE(S_delete_track,             g_delete_track_w,             1, 0, 0, H_delete_track);
  XEN_DEFINE_PROCEDURE(S_lock_track,               g_lock_track_w,               1, 0, 0, H_lock_track);
  XEN_DEFINE_PROCEDURE(S_track,                    g_track_w,                    1, 1, 0, H_track);
  XEN_DEFINE_PROCEDURE(S_tracks,                   g_tracks_w,                   0, 0, 0, H_tracks);
  XEN_DEFINE_PROCEDURE(S_make_track,               g_make_track_w,               0, 0, 1, H_make_track);  
  XEN_DEFINE_PROCEDURE(S_track_p,                  g_track_p_w,                  1, 0, 0, H_track_p);
  XEN_DEFINE_PROCEDURE(S_track_chans,              g_track_chans_w,              1, 0, 0, H_track_chans);
  XEN_DEFINE_PROCEDURE(S_copy_mix,                 g_copy_mix_w,                 1, 1, 0, H_copy_mix);
  XEN_DEFINE_PROCEDURE(S_copy_track,               g_copy_track_w,               1, 1, 0, H_copy_track);
  XEN_DEFINE_PROCEDURE(S_view_mixes_dialog,        g_view_mixes_dialog_w,        0, 0, 0, H_view_mixes_dialog);
  XEN_DEFINE_PROCEDURE(S_view_tracks_dialog,       g_view_tracks_dialog_w,       0, 0, 0, H_view_tracks_dialog);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_color,    g_track_color_w,    H_track_color,    S_setB S_track_color,    g_set_track_color_w,    1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_name,     g_track_name_w,     H_track_name,     S_setB S_track_name,     g_set_track_name_w,     1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_tag_y,    g_track_tag_y_w,    H_track_tag_y,    S_setB S_track_tag_y,    g_set_track_tag_y_w,    1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_amp,      g_track_amp_w,      H_track_amp,      S_setB S_track_amp,      g_set_track_amp_w,      1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_speed,    g_track_speed_w,    H_track_speed,    S_setB S_track_speed,    g_set_track_speed_w,    1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_tempo,    g_track_tempo_w,    H_track_tempo,    S_setB S_track_tempo,    g_set_track_tempo_w,    1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_amp_env,  g_track_amp_env_w,  H_track_amp_env,  S_setB S_track_amp_env,  g_set_track_amp_env_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_position, g_track_position_w, H_track_position, S_setB S_track_position, g_set_track_position_w, 1, 1, 2, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_track,    g_track_track_w,    H_track_track,    S_setB S_track_track,    g_set_track_track_w,    1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_speed_style, g_track_speed_style_w, H_track_speed_style, 
				   S_setB S_track_speed_style, g_set_track_speed_style_w, 1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_dialog_mix, g_mix_dialog_mix_w, H_mix_dialog_mix, 
				   S_setB S_mix_dialog_mix, g_set_mix_dialog_mix_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_dialog_track, g_track_dialog_track_w, H_track_dialog_track, 
				   S_setB S_track_dialog_track, g_set_track_dialog_track_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_tempo_control_bounds, g_tempo_control_bounds_w, H_tempo_control_bounds,
				   S_setB S_tempo_control_bounds, g_set_tempo_control_bounds_w,
				   0, 0, 1, 0);

}
