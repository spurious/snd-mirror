#include "snd.h"

/* MIX FIX:
 * --> add mix/track button to mix-panel to automate track edits (tempo slider), or add new track panel
 * --> add easy pan choices with linear, sinusoidal, exponential envs
 * --> add easy synchronization across mixes ("snap")
 * --> mix-maxamp, filter-track in mix.scm
 * --> sync multichan mixes should change together in graph
 * track-tempo?
 * copy_mix|track
 * filter_track
 * choice of y-style (i.e. split out tracks vertically -- track all at same height)
 * timing grid
 */

/* TODO: if mix-waveform-height 50 top of mix waveform can be pushed off top of graph */
/* TODO: first and last samples of mix-peak-amp-waveform don't cancel */

typedef struct {
  int chans;
  Float *scalers;
  Float speed;
  env **amp_envs;
} mix_track_state;  

typedef struct {         /* save one mix state */
  int chans;             /* size of arrays in this struct */
  int edit_ctr;          /* cp edit_ctr at time of creation of this struct */
  off_t beg, end, orig, len;  /* samp positions in output (orig = where edit tree thinks it is) */
  bool locked;
  Float *scalers;
  Float speed;
  env **amp_envs;
  mix_track_state *as_built;
} mix_state;

typedef struct {
  chan_info *cp;
  mix_context *wg;
  char *name;
  char *in_filename;
  int in_chans;
  off_t in_samps;              /* in_samps needed to simplify speed changed duration calculations */
  int mix_state_size;          /* current size of mix_state list */
  mix_state **states;          /* list of mixer states */
  mix_state *active_mix_state;
  off_t anchor, orig_beg;      /* sample in in-data of attachment */
  int current_state;
  file_delete_t temporary;     /* in-filename was written by us and needs to be deleted when mix state is deleted */
  snd_info *add_snd;           /* readable snd_info struct for mix input */
  int id, x, nx, y, track, tagx, tagy, tag_y, height; 
                               /* tag_y only for user set mix_tag_y */
  env **panel_envs;            /* mix panel version of current amp envs */
} mix_info;

typedef enum {C_STRAIGHT_SOUND, C_AMP_SOUND, C_SPEED_SOUND, C_ZERO_SOUND, C_AMP_ENV_SOUND, C_SPEED_AMP_SOUND, C_SPEED_ENV_SOUND,
	      C_STRAIGHT_PEAK, C_AMP_PEAK, C_SPEED_PEAK, C_ZERO_PEAK, C_AMP_ENV_PEAK, C_SPEED_AMP_PEAK, C_SPEED_ENV_PEAK} mix_calc_t;

typedef struct {
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
  env_info **eps; /* new envs created via env_on_env */
  Float *scalers;
} mix_fd;


static mix_info *md_from_id(int n);
static void draw_mix_waveform(mix_info *md);
static void erase_mix_waveform(mix_info *md);
static mix_track_state *copy_mix_track_state(mix_track_state *cs);
static mix_track_state *free_mix_track_state(mix_track_state *cs);
static Float gather_track_amp(int id);
static Float gather_track_speed(int id);
static env *gather_track_amp_env(int id, off_t mix_beg, off_t mix_dur);
static void release_dangling_mix_readers(mix_info *md);
#if DEBUGGING
  void report_dangling_mix_readers(FILE *fp);
  static void check_dangling_mix_readers(mix_fd *md);
#endif
static void set_mix_track(mix_info *md, int trk, bool redisplay);
static int new_track(void);
/* static void map_over_track_mixes(int track_id, void (*func)(mix_info *, void *), void *val); */
static void redisplay_track(int id);
static void remix_track(int id, void (*func)(mix_info *, void *), void *val);
static void set_track_position_1(mix_info *md, void *val);

static XEN mix_release_hook;
static XEN mix_drag_hook;
/* also mix_click_hook in snd-chn.c */

chan_info *mix_channel_from_id(int mix_id)
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

static color_t mix_to_color_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    return(md->wg->color);
  return(ss->sgx->basic_color);
}

static axis_context *set_mix_waveform_context(chan_info *cp, mix_info *md)
{
  axis_context *ax;
  ax = mix_waveform_context(cp);
  set_foreground_color(cp, ax, md->wg->color); 
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

mix_context *cp_to_mix_context(chan_info *cp)
{
  mix_info *md;
  md = (mix_info *)(cp->mix_dragging);
  if (md) return(md->wg);
  return(NULL);
}


/* -------- history of mix panel settings -------- */

static mix_state *make_mix_state(int chans, int edit_ctr, off_t beg, off_t end)
{
  mix_state *cs;
  cs = (mix_state *)CALLOC(1, sizeof(mix_state));
  cs->chans = chans;
  cs->edit_ctr = edit_ctr;
  /* fprintf(stderr,"make_mix_state set orig to " OFF_TD " -> " OFF_TD "\n", beg, end); */
  cs->orig = beg;
  cs->beg = beg;
  cs->end = end;
  cs->len = end - beg + 1;
  cs->locked = false;
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
  ncs->speed = cs->speed;
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
  /* fprintf(stderr,"make_current_mix_state set orig to " OFF_TD "\n", cs->beg); */
  cur->orig = cs->beg;
  cur->beg = cs->beg;
  cur->end = cs->end;
  cur->len = cs->len;
  cur->locked = cs->locked;
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
  cur->as_built = cs->as_built;
}

static mix_state *free_mix_state(mix_state *cs)
{
  int i;
  if (cs)
    {
      if (cs->scalers) {FREE(cs->scalers); cs->scalers = NULL;}
      if (cs->amp_envs) 
	{
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

static mix_info *md_from_id(int n) 
{
  if ((n >= 0) && (n < mix_infos_size)) 
    return(mix_infos[n]); 
  else return(NULL);
}

static mix_info *make_mix_info(chan_info *cp)
{
  int i;
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
	  mix_infos_size += MIX_INFO_INCREMENT;
	  mix_infos = (mix_info **)REALLOC(mix_infos, mix_infos_size * sizeof(mix_info *));
	  for (i = mix_infos_size - MIX_INFO_INCREMENT; i < mix_infos_size; i++) 
	    mix_infos[i] = NULL;
	}
    }
  md = (mix_info *)CALLOC(1, sizeof(mix_info));
  mix_infos[mix_infos_ctr] = md;
  cp->mixes = 1;
  md->id = mix_infos_ctr++;
  md->cp = cp;
  md->add_snd = NULL;
  md->temporary = DONT_DELETE_ME;
  md->wg = set_mix_info_context(cp);
  md->anchor = 0;
  md->y = 0;
  md->height = mix_waveform_height(ss);
  md->panel_envs = NULL;
  return(md);
}

static int map_over_channel_mixes(chan_info *cp, int (*func)(mix_info *, void *), void *ptr)
{
  int i, val;
  mix_info *md;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  val = (*func)(md, ptr);
	  if (val) return(val);
	}
    }
  return(0);
}

static int map_over_mixes(int (*func)(mix_info *, void *), void *ptr)
{
  int i, val;
  mix_info *md;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if (md)
	{
	  val = (*func)(md, ptr);
	  if (val) return(val);
	}
    }
  return(0);
}

static int map_over_active_mixes(int (*func)(mix_info *, void *), void *ptr)
{
  int i, val;
  mix_info *md;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (mix_ok_and_unlocked(md->id)))
	{
	  val = (*func)(md, ptr);
	  if (val) return(val);
	}
    }
  return(0);
}

static int look_for_mix_tempfile(mix_info *md, void *in_name) /* int rtn type for map_over_mixes */
{
  return((md->in_filename) && 
	 (strcmp(md->in_filename, (char *)in_name) == 0));
}

static mix_info *free_mix_info(mix_info *md)
{
  int i;
  if (md)
    {
      release_dangling_mix_readers(md);
      if (md->wg) md->wg = free_mix_context(md->wg);
      mix_infos[md->id] = NULL;
      if (md->temporary == DELETE_ME) 
	snd_remove(md->in_filename, REMOVE_FROM_CACHE);
      else
	{
	  if (md->temporary == MULTICHANNEL_DELETION) /* n-chan selection via C-x q for example */
	    {
	      if (!(map_over_mixes(look_for_mix_tempfile, (void *)(md->in_filename))))
		snd_remove(md->in_filename, REMOVE_FROM_CACHE);
	    }
	}
      if (md->name) {FREE(md->name); md->name = NULL;}
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
	  md->active_mix_state->as_built = NULL;
	  md->active_mix_state = free_mix_state(md->active_mix_state);
	}
      if (md->panel_envs)
	{
	  for (i = 0; i < md->in_chans; i++)
	    if (md->panel_envs[i])
	      free_env(md->panel_envs[i]);
	  FREE(md->panel_envs);
	  md->panel_envs = NULL;
	}
      FREE(md);
    }
  return(NULL);
}


/* ---------------- MIX READ ---------------- */

#if 1
static char *mix_calc_names[] = {
  "straight_sound", "amp_sound", "speed_sound", "zero_sound", "amp_env_sound", "speed_amp_sound", "speed_env_sound",
  "straight_peak", "amp_peak", "speed_peak", "zero_peak", "amp_env_peak", "speed_amp_peak", "speed_env_peak"
};
#endif

static snd_info *make_mix_readable(mix_info *md)
{
  chan_info *cp;
  int i;
  snd_info *add_sp;
  if (md == NULL) return(NULL);
  if (!md->add_snd) 
    {
      cp = md->cp;
      if (mus_file_probe(md->in_filename))
	md->add_snd = make_sound_readable(md->in_filename, true);
      else 
	{
	  snd_error(_("can't find file %s: %s"), md->in_filename, strerror(errno));
	  return(NULL);
	}
      add_sp = md->add_snd;
      add_sp->filename = copy_string(md->in_filename);
      add_sp->short_filename = filename_without_home_directory(add_sp->filename);
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
  env_info *ep = NULL;
  int i;
  bool happy = true;
  chan_info *cp;
  snd_info *sp;
  int samps_per_bin = 0;
  mix_state *cs;
  cs = md->active_mix_state;
  sp = make_mix_readable(md);
  if (sp)
    {
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  if ((cp == NULL) || (cp->amp_envs == NULL)) return(false);
	  ep = cp->amp_envs[0]; /* mixed-in-sound cp->edit_ctr always 0 */
#if DEBUGGING	  
	  if (cp->edit_ctr != 0) fprintf(stderr,"%s[%d]->edit_ctr = %d\n", sp->short_filename, cp->chan, cp->edit_ctr);
#endif
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
  int i, j, move;
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
    {
#if DEBUGGING
      /* fprintf(stderr, "mix_read: %s\n", mix_calc_names[mf->calc]); */
#endif
      return(mf);
    }
  if (!(md->add_snd)) 
    {
      md->add_snd = make_mix_readable(md);
      if (!(md->add_snd)) return(NULL);
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
		  /* fprintf(stderr,"%d %d: %s\n", (int)beg, (int)(cs->len - 1), (env_to_string(e))); */
		  mf->segs[i] = mus_make_env(e->data, e->pts, mf->scalers[i], 0.0, 1.0, 0.0, 0, cs->len - 1, NULL);
		  /* fprintf(stderr,"env: %s\n", mus_describe(mf->segs[i])); */
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
#ifdef __cplusplus
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
#else
  else mf->calc += C_STRAIGHT_PEAK;
#endif
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
#if DEBUGGING
  /* fprintf(stderr, "mix_read: %s\n", mix_calc_names[mf->calc]); */
#endif
  return(mf);
}

static mix_fd *init_mix_read(mix_info *md, bool old, off_t beg)
{
  return(init_mix_read_any(md, old, MIX_INPUT_SOUND, beg));
}

#define HI_PEAKS true
#define LO_PEAKS false

static mix_fd *init_mix_input_amp_env_read(mix_info *md, bool old, bool hi)
{
  int i;
  mix_fd *mf = NULL;
  snd_info *sp;
  chan_info *cp;
  env_info *ep;
  mf = init_mix_read_any(md, old, MIX_INPUT_AMP_ENV, 0);
  if (!mf) return(NULL);
  sp = md->add_snd;
  mf->ctr = (int *)CALLOC(sp->nchans, sizeof(int));
  mf->samples = (off_t *)CALLOC(sp->nchans, sizeof(off_t));
  if ((mf->calc != C_ZERO_SOUND) && (mf->calc != C_ZERO_PEAK))
    mf->idata = (mus_sample_t **)CALLOC(sp->nchans, sizeof(mus_sample_t *));
  for (i = 0; i < sp->nchans; i++)
    {
      mix_state *cs;
      cp = sp->chans[i];
      mf->ctr[i] = -1; /* preincremented */
      mf->samples[i] = CURRENT_SAMPLES(cp);
      if (old == PREVIOUS_MIX) 
	cs = md->states[md->current_state];
      else cs = md->active_mix_state;
      if ((mf->calc != C_ZERO_SOUND) && 
	  (mf->calc != C_ZERO_PEAK) &&
	  (cs->as_built->amp_envs) && 
	  (cs->as_built->amp_envs[i]))
	{
	  ep = env_on_env(cs->as_built->amp_envs[i], cp);
	  if (!(mf->eps)) {fprintf(stderr,"no eps! %s\n", mix_calc_names[mf->calc]); abort();}
	  mf->eps[i] = ep; /* save for GC */
	}
      else ep = cp->amp_envs[0]; /* mixed-in-sound cp->edit_ctr always 0 */
#if DEBUGGING	  
      if (cp->edit_ctr != 0) fprintf(stderr,"amp env %s[%d]->edit_ctr = %d\n", sp->short_filename, cp->chan, cp->edit_ctr);
#endif
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

static mix_fd *free_mix_fd_almost(mix_fd *mf)
{
  int i;
  if (mf)
    {
      if (mf->lst) {FREE(mf->lst); mf->lst = NULL;}
      if (mf->nxt) {FREE(mf->nxt); mf->nxt = NULL;}
      if (mf->sfs)
	{
	  for (i = 0; i < mf->chans; i++)
	    if (mf->sfs[i]) 
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
	    if (mf->eps[i])
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
#if DEBUGGING
      check_dangling_mix_readers(mf);
#endif      
      free_mix_fd_almost(mf);
      FREE(mf);
    }
  return(NULL);
}

/* ---------------- MIXING ---------------- */

static int remove_temporary_mix_file(mix_info *md, void *ptr)
{
  if (md->temporary == DELETE_ME) 
    snd_remove(md->in_filename, REMOVE_FROM_CACHE);
  return(0);
}

void free_mix_list(chan_info *cp)
{
  map_over_channel_mixes(cp, remove_temporary_mix_file, NULL);
}

void free_mixes(chan_info *cp)
{
  /* called in snd-data.c during chan_info cleanup */
  int i;
  mix_info *md;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	mix_infos[i] = free_mix_info(md);
    }
  cp->mixes = 0;
}

disk_space_t disk_space_p(snd_info *sp, off_t bytes, off_t other_bytes, char *filename)
{
  bool go_on;
  off_t kfree, kneeded, kother;
  kfree = disk_kspace(filename);
  if (kfree < 0) 
    {
      report_in_minibuffer_and_save(sp, strerror(errno)); 
      return(NO_PROBLEM);  /* what?? -- disk_kspace => -1 if no such disk, etc -- not really a disk *space* problem */
    }
  kneeded = bytes >> 10;
  if (kfree < kneeded)
    {
      if (other_bytes > 0)
	{
	  kother = other_bytes >> 10;
	  if (kother > kfree)
	    {
	      report_in_minibuffer_and_save(sp, _("only " PRId64 " Kbytes left on disk, changing to 16-bit temp output"), kfree);
	      return(HUNKER_DOWN);
	    }
	}
      go_on = snd_yes_or_no_p(_("only " PRId64 " Kbytes left on disk; continue?"), kfree);
      if (!go_on) return(GIVE_UP);
      report_in_minibuffer(sp, _("ok -- here we go..."));
      return(BLIND_LEAP);
    }
  return(NO_PROBLEM);
}

static char *save_as_temp_file(mus_sample_t **raw_data, int chans, int len, int nominal_srate)
{
  char *newname;
  int format, ofd, err;
  disk_space_t no_space;
  format = MUS_OUT_FORMAT;
  newname = shorter_tempnam(temp_dir(ss), "snd_");
                      /* we're writing our own private version of this thing, so we can use our own formats */
  err = snd_write_header(newname, MUS_NEXT, nominal_srate, chans, 28, len * chans, format, NULL, 0, NULL);
  if (err == -1) return(NULL);
  ofd = snd_reopen_write(newname);
  mus_file_open_descriptors(ofd, newname, format, 4, 28, chans, MUS_NEXT);
  /* mus_file_set_data_clipped(ofd, data_clipped(ss)); */
  lseek(ofd, 28, SEEK_SET);
  no_space = disk_space_p(any_selected_sound(), len * chans * 4, 0, newname);
  if (no_space != GIVE_UP)
    mus_file_write(ofd, 0, len - 1, chans, raw_data);
  if (mus_file_close(ofd) != 0)
    snd_error(_("mix save temp: can't close %s: %s!"), newname, strerror(errno));
  return(newname);
}

#define MULTIPLY_ENVS_INCR .1
#define OFFSET_FROM_TOP 0
/* axis top border width is 10 (snd-axis.c) */

static mix_track_state *gather_as_built(mix_info *md, mix_state *cs)
{
  mix_track_state *ms;
  Float trk_amp = 1.0;
  int i;
  ms = (mix_track_state *)CALLOC(1, sizeof(mix_track_state));
  ms->chans = cs->chans;
  ms->speed = cs->speed * gather_track_speed(md->track);
  cs->len = (off_t)(ceil(md->in_samps / ms->speed));
  trk_amp = gather_track_amp(md->track);
  ms->scalers = (Float *)CALLOC(ms->chans, sizeof(Float));
  for (i = 0; i < ms->chans; i++)
    ms->scalers[i] = cs->scalers[i] * trk_amp;
  if ((cs->amp_envs) || (md->track != 0))
    {
      env *track_env = NULL;
      /* fprintf(stderr,"gather as built beg: " OFF_TD ", orig: " OFF_TD "\n", cs->beg, cs->orig); */
      if (md->track != 0)
	track_env = gather_track_amp_env(md->track, cs->beg, cs->len);
      /* fprintf(stderr,"2 beg: " OFF_TD ", orig: " OFF_TD "\n", cs->beg, cs->orig); */
      ms->amp_envs = (env **)CALLOC(ms->chans, sizeof(env *));
      for (i = 0; i < ms->chans; i++)
	if ((cs->amp_envs) && (cs->amp_envs[i]))
	  {
	    if (track_env)
	      ms->amp_envs[i] = multiply_envs(cs->amp_envs[i], track_env, MULTIPLY_ENVS_INCR);
	    else ms->amp_envs[i] = copy_env(cs->amp_envs[i]);
	    /* fprintf(stderr,"-> %s\n", env_to_string(ms->amp_envs[i])); */
	  }
	else 
	  {
	    if (track_env)
	      ms->amp_envs[i] = copy_env(track_env);
	  }
      if (track_env) free_env(track_env);
      /* fprintf(stderr,"3 beg: " OFF_TD ", orig: " OFF_TD "\n", cs->beg, cs->orig); */
    }
  else ms->amp_envs = NULL;
  cs->as_built = ms;
  return(ms);
}

static mix_info *add_mix(chan_info *cp, int chan, off_t beg, off_t num, char *full_original_file, int input_chans, file_delete_t auto_delete)
{ 
  mix_info *md;
  char *namebuf;
  mix_state *cs;
  md = make_mix_info(cp);     /* add active mix to chan_info list */
  md->in_chans = input_chans;
  md->orig_beg = beg;
  md->in_samps = num;
  namebuf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(namebuf, LABEL_BUFFER_SIZE, "mix%d", md->id);
  md->name = namebuf;
  md->temporary = auto_delete;
  md->mix_state_size = 1;
  md->states = (mix_state **)CALLOC(md->mix_state_size, sizeof(mix_state *));
  cs = make_mix_state(input_chans, cp->edit_ctr, beg, beg + num - 1);
  md->active_mix_state = make_mix_state(input_chans, cp->edit_ctr, beg, beg + num - 1);
  md->states[0] = cs;
  if (chan < input_chans)
    cs->scalers[chan] = 1.0;
  cs->speed = 1.0;
  md->current_state = 0;
  gather_as_built(md, cs);
  make_current_mix_state(md);
  md->in_filename = copy_string(full_original_file);
  reflect_mix_in_menu();
  reflect_mix_in_mix_panel(md->id);
  return(md);
}

static mix_info *file_mix_samples(off_t beg, off_t num, char *mixfile, chan_info *cp, int chan, 
				  file_delete_t auto_delete, const char *origin, bool with_tag, 
				  int track_id, bool redisplay)
{
  /* open mixfile, current data, write to new temp file mixed, close others, open and use new as change case */
  /* used for clip-region temp file incoming and C-q in snd-chn.c (i.e. mix in file) so sync not relevant */
  snd_fd *csf = NULL;
  snd_info *sp;
  int ofd, ifd;
  char *ofile;
  mus_sample_t **data;
  mus_sample_t *chandata;
  int in_chans, err = 0;
  mix_info *md = NULL;
  off_t i, j, len, size;
  file_info *ihdr, *ohdr;
  if (num <= 0) return(NULL); /* a no-op -- mixing in an empty file */
  len = CURRENT_SAMPLES(cp);
  if (beg >= len)
    extend_with_zeros(cp, len, beg - len + 1, "(mix-extend)", cp->edit_ctr);
  /* TODO: set flag here that we need backup after file_mix_samples below (backup_edit_list(cp)) */
  /* otherwise user sees unexplained mix-extend in edit history list */
  if (beg < 0) beg = 0;
  sp = cp->sound;
  ihdr = make_file_info(mixfile);
  if (!ihdr) return(NULL);
  in_chans = ihdr->chans;
  if (chan >= in_chans) 
    {
      free_file_info(ihdr);
      return(NULL); /* we're reading input[chan] so if chan >= in_chans (no such channel exists) give up */
    }
  ofile = snd_tempnam();
  ohdr = make_temp_header(ofile, SND_SRATE(sp), 1, 0, (char *)origin);
  ofd = open_temp_file(ofile, 1, ohdr);
  if (ofd == -1) 
    {
      free_file_info(ihdr);
      snd_error(_("open mix temp file %s hit error: %s"), ofile, strerror(errno)); 
      return(NULL);
    }
  if ((disk_space_p(sp, num * 4, 0, ofile)) != GIVE_UP)
    csf = init_sample_read(beg, cp, READ_FORWARD);
  if (csf == NULL) /* i.e. no space for temp, I guess */
    {
      free_file_info(ihdr);
      mus_file_close(ofd);
      snd_remove(ofile, REMOVE_FROM_CACHE);
      FREE(ofile);
      return(NULL);
    }
  ifd = snd_open_read(mixfile);
  mus_file_open_descriptors(ifd, mixfile,
			    ihdr->format,
			    mus_bytes_per_sample(ihdr->format),
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
  mus_file_read_chans(ifd, 0, size - 1, in_chans, data, (mus_sample_t *)data);
  for (i = 0, j = 0; i < num; i++)
    {
      if (j == size)
	{
	  err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
	  mus_file_read_chans(ifd, 0, size - 1, in_chans, data, (mus_sample_t *)data);
	  j = 0;
	  if (err == -1) break;
	}
      chandata[j] += read_sample(csf);
      j++;
    }
  if (j > 0) mus_file_write(ofd, 0, j - 1, 1, &chandata);
  close_temp_file(ofd, ohdr, num * mus_bytes_per_sample(ohdr->format), sp);
  mus_file_close(ifd);
  free_snd_fd(csf);
  FREE(data[chan]);
  FREE(data);
  free_file_info(ihdr);
  free_file_info(ohdr);
  file_change_samples(beg, num, ofile, cp, 0, DELETE_ME, DONT_LOCK_MIXES, origin, cp->edit_ctr);
  if (ofile) FREE(ofile);
  if (with_tag)
    {
      md = add_mix(cp, chan, beg, num, mixfile, in_chans, auto_delete);
      set_mix_track(md, track_id, false);
    }
  else
    {
      if (auto_delete == DELETE_ME)
	snd_remove(mixfile, REMOVE_FROM_CACHE);
    }
  if (redisplay) update_graph(cp);
  return(md);
}

/* next functions canonicalize mixer input -- 
 *   all end up with a filename containing the original to-be-mixed input
 *                     length (per channel samples in input)
 *                     begin sample in output for mix start
 *                     an array of cps for mixing into
 *                     a notion of initial scalers
 */

#define MIX_FILE_NO_MIX -1
#define MIX_FILE_NO_FILE -2

int mix_file(off_t beg, off_t num, int chans, chan_info **cps, char *mixinfile, file_delete_t temp, const char *origin, bool with_tag, int track_id)
{
  /* loop through out_chans cps writing the new mixed temp files and fixing up the edit trees */
  int i, edpos, id = MIX_FILE_NO_MIX;
  mix_info *md;
  chan_info *cp;
  if ((track_id == 0) && (chans > 1))
    track_id = new_track();  /* create a track for this mix to keep all chans sync'd */
  if (chans > 1)
    {
      /* as-one-edit style track op */
      for (i = 0; i < chans; i++) 
	{
	  cp = cps[i];
	  edpos = cp->edit_ctr + 1;
	  md = file_mix_samples(beg, num, mixinfile, cp, i, temp, origin, with_tag, track_id, false);
	  if ((md) && (id == MIX_FILE_NO_MIX)) id = md->id;
	  while (cp->edit_ctr > edpos) backup_edit_list(cp);
	  backup_mix_list(cp, edpos);
	  if (cp->edits[cp->edit_ctr]->origin) FREE(cp->edits[cp->edit_ctr]->origin);
	  cp->edits[cp->edit_ctr]->origin = copy_string(origin);
	  update_graph(cp);
	}
    }
  else 
    {
      md = file_mix_samples(beg, num, mixinfile, cps[0], 0, temp, origin, with_tag, track_id, true);
      if (md) id = md->id;
    }
  return(id);
}

static int mix_complete_file(snd_info *sp, off_t beg, char *fullname, const char *origin, 
			     bool with_tag, file_delete_t auto_delete, int track_id, bool all_chans)
{
  /* no need to save as temp here, but we do need sync info (from menu and keyboard) */
  /* returns -1 if with_tag is false, -2 if no such file */
  chan_info *cp;
  snd_info *nsp;
  chan_info **cps = NULL;
  int i, chans, id = MIX_FILE_NO_MIX, old_sync = 0, new_sync = 1;
  off_t len;
  sync_info *si = NULL;
  len = mus_sound_frames(fullname);
  if (len < 0) return(MIX_FILE_NO_FILE);
  if (len == 0) return(MIX_FILE_NO_MIX);
  cp = any_selected_channel(sp);
  old_sync = sp->sync;
  if ((old_sync == 0) && (all_chans))
    {
      /* TODO: dynamic-wind here? */
      for (i = 0; i < ss->max_sounds; i++)
	{
	  nsp = ss->sounds[i];
	  if ((nsp) && (nsp->inuse == SOUND_NORMAL))
	    {
	      if (nsp->sync >= new_sync)
		new_sync = nsp->sync + 1;
	    }
	}
      sp->sync = new_sync;
    }
  if (sp->sync != 0)
    {
      /* fprintf(stderr,"collect chans syncd"); */
      si = snd_sync(sp->sync); 
      cps = si->cps;
      chans = si->chans;
    }
  else
    {
      /* fprintf(stderr,"no sync"); */
      cps = (chan_info **)CALLOC(1, sizeof(chan_info *));
      cps[0] = cp;
      chans = 1;
    }
  id = mix_file(beg, len, chans, cps, fullname, auto_delete, origin, with_tag, track_id);
  if (si) 
    si = free_sync_info(si); 
  else 
    if (cps) 
      FREE(cps);
  sp->sync = old_sync;
  return(id);
}

void mix_complete_file_at_cursor(snd_info *sp, char *str, const char *origin, bool with_tag, int track_id)
{
  chan_info *cp;
  int err;
  char *fullname = NULL;
  if ((sp) && (str) && (*str))
    {
      fullname = mus_expand_filename(str);
      cp = any_selected_channel(sp);
      err = mix_complete_file(sp, CURSOR(cp), fullname, origin, with_tag, DONT_DELETE_ME, track_id, false);
      if (err == MIX_FILE_NO_FILE) 
	report_in_minibuffer_and_save(sp, _("can't mix file: %s, %s"), str, strerror(errno));
      if (fullname) FREE(fullname);
    }
}

#define MIX_STATE_INCREMENT 8

static void extend_mix_state_list(mix_info *md)
{
  int i, lim;
  md->current_state++;
  if (md->current_state >= md->mix_state_size)
    {
      lim = md->mix_state_size;
      md->mix_state_size += MIX_STATE_INCREMENT;
      md->states = (mix_state **)REALLOC(md->states, md->mix_state_size * sizeof(mix_state *));
      for (i = lim; i < md->mix_state_size; i++) md->states[i] = NULL;
    }
}

/* (as-one-edit (lambda () (set! (mix-position 0) 0) (set! (mix-position 1) 1))) */

static int backup_mix(mix_info *md, void *ptr)
{
  int one_edit, current_state;
  mix_state *cs, *curcs;
  one_edit = (*((int *)ptr));
  current_state = md->current_state;
  curcs = md->states[current_state];
  while ((md->current_state > 1) && 
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
  return(0);
}

void backup_mix_list(chan_info *cp, int edit_ctr)
{
  /* we're at md->states[md->current_state] (mix_state) with cs->edit_ctr at value upon local edit */
  /* edit_ctr is the one-edit point for this channel */
  map_over_channel_mixes(cp, backup_mix, (void *)(&edit_ctr));
}


static void remix_file(mix_info *md, const char *origin, bool redisplay)
{
  off_t beg, end, i, num;
  int j, ofd = 0, size;
  bool use_temp_file;
  disk_space_t no_space;
  Float val = 0.0, maxy, miny;
  snd_info *cursp;
  mix_fd *add, *sub;
  snd_fd *cur, *sfb, *afb;
  char *ofile = NULL;
  mus_sample_t **data;
  mus_sample_t *chandata;
  mus_sample_t mval, mmax, mmin;
  file_info *ohdr = NULL;
  axis_info *ap;
  chan_info *cp;
  off_t old_beg, old_end, new_beg, new_end;
  int err = 0;
  mix_state *cs;
  mix_track_state *ms;
  release_pending_mix_states(md);
  cs = md->active_mix_state;
  ms = gather_as_built(md, cs); /* copied after mix, so needs to be freed */ /* TODO: this in make_temporary_graph as well, I think */
  cp = md->cp;
  ap = cp->axis;
  /*
  fprintf(stderr,"remix %d from " OFF_TD " to " OFF_TD " (" OFF_TD ":" OFF_TD ")\n", md->id, cs->orig, cs->beg, cs->end, cs->len);
  */
  old_beg = cs->orig;
  old_end = cs->end;
  new_beg = cs->beg;
  new_end = cs->beg + cs->len - 1;
  cursp = cp->sound;
  beg = (old_beg < new_beg) ? old_beg : new_beg;
  end = (old_end > new_end) ? old_end : new_end;
  num = end - beg + 1;
  use_temp_file = (num >= MAX_BUFFER_SIZE);
  if (use_temp_file)
    {
      ofile = snd_tempnam();
      ohdr = make_temp_header(ofile, SND_SRATE(cursp), 1, 0, (char *)origin);
      ofd = open_temp_file(ofile, 1, ohdr);
      if (ofd == -1)
	{
	  snd_error(_("can't write mix temp file %s: %s\n"), ofile, strerror(errno));
	  return;
	}
    }
  add = init_mix_read(md, CURRENT_MIX, 0);
  if (!add) return;
  sub = init_mix_read(md, PREVIOUS_MIX, 0);
  if (!sub) return;
  cur = init_sample_read(beg, cp, READ_FORWARD);
  if (cur == NULL) return;

  if (use_temp_file)
    {
      no_space = disk_space_p(cursp, num * 4, num * 2, ofile);
      switch (no_space)
	{
	case GIVE_UP:
	  close_temp_file(ofd, ohdr, 0, cursp);
	  free_snd_fd(cur);
	  free_mix_fd(add);
	  free_mix_fd(sub);
	  free_file_info(ohdr);
	  snd_remove(ofile, REMOVE_FROM_CACHE);
	  FREE(ofile);
	  return;
	  break;
	case HUNKER_DOWN:
	  close_temp_file(ofd, ohdr, 0, cursp);
	  if (mus_bytes_per_sample(MUS_OUT_FORMAT) == 2)
	    ohdr->format = MUS_OUT_FORMAT;
	  else
	    {
	      if (mus_bytes_per_sample(MUS_COMPATIBLE_FORMAT) == 2)
		ohdr->format = MUS_COMPATIBLE_FORMAT;
	      else
#if MUS_LITTLE_ENDIAN
		ohdr->format = MUS_LSHORT;
#else
		ohdr->format = MUS_BSHORT;
#endif
	    }
	  ofd = open_temp_file(ofile, 1, ohdr);
	  break;
	case NO_PROBLEM: case BLIND_LEAP: break;
	}
    }
  if (num < MAX_BUFFER_SIZE) size = (int)num; else size = MAX_BUFFER_SIZE;
  data = (mus_sample_t **)CALLOC(1, sizeof(mus_sample_t *));
  data[0] = (mus_sample_t *)CALLOC(size, sizeof(mus_sample_t));
  chandata = data[0];
  if (use_temp_file) lseek(ofd, ohdr->data_location, SEEK_SET);

  old_beg -= beg;
  old_end -= beg;
  new_beg -= beg;
  new_end -= beg;

  /* these max/min values are only used to reset y-axis limits if overflow occurred */
  maxy = ap->ymax;
  miny = ap->ymin;
  mmax = MUS_SAMPLE_MIN;
  mmin = MUS_SAMPLE_MAX;

  /* split out special simple cases */
  if ((add->calc == C_ZERO_SOUND) && (sub->calc == C_ZERO_SOUND))
    {
      for (i = 0, j = 0; i < num; i++)
	{
	  if (j == size)
	    {
	      if (use_temp_file) err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
	      j = 0;
	      if (err == -1) break;
	    }
	  chandata[j++] = read_sample(cur);
	}
    }
  else
    {
      if ((add->calc == C_STRAIGHT_SOUND) && (sub->calc == C_STRAIGHT_SOUND))
	{
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

  if (use_temp_file)
    {
      if (j > 0) mus_file_write(ofd, 0, j - 1, 1, &chandata);
      close_temp_file(ofd, ohdr, num * mus_bytes_per_sample(ohdr->format), cursp);
      free_file_info(ohdr);
    }
  free_snd_fd(cur);
  free_mix_fd(add);
  free_mix_fd(sub);

  if (use_temp_file)
    file_change_samples(beg, num, ofile, cp, 0, DELETE_ME, DONT_LOCK_MIXES, origin, cp->edit_ctr);
  else change_samples(beg, num, data[0], cp, DONT_LOCK_MIXES, origin, cp->edit_ctr);
  FREE(data[0]);
  FREE(data);
 
  extend_mix_state_list(md);
  cs = copy_mix_state(cs);
  cs->edit_ctr = cp->edit_ctr;
  cs->orig = new_beg + beg;
  /* fprintf(stderr,"remix_file set orig to " OFF_TD "\n", cs->orig); */
  cs->beg = cs->orig;
  /* fprintf(stderr,"remix at end: " OFF_TD ", beg: " OFF_TD "\n", cs->orig, cs->beg); */
  cs->end = cs->beg + cs->len - 1;
  md->states[md->current_state] = cs;
  make_current_mix_state(md);
  free_mix_track_state(ms);

  /* fix up graph if we overflowed during mix */
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
  if (redisplay) update_graph(cp);
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


static int make_temporary_amp_env_mixed_graph(chan_info *cp, axis_info *ap, mix_info *md, Float samples_per_pixel,
					      off_t newbeg, off_t newend, off_t oldbeg, off_t oldend)
{
  /* temp graph using cp->amp_env and mix (sample-by-sample) data */
  off_t main_start, new_start, old_start;
  mix_fd *new_fd, *old_fd;
  Float val, new_ymin, new_ymax, old_ymin, old_ymax, main_ymin, main_ymax;
  double xi;
  Float xf, xfinc;
  off_t lo, hi, i;
  int j, main_loc;
  Locus lastx;
  env_info *ep;

  lo = ap->losamp;
  hi = ap->hisamp;

  new_fd = init_mix_read(md, CURRENT_MIX, 0);
  if (!new_fd) return(0);
  old_fd = init_mix_read(md, PREVIOUS_MIX, 0);
  if (!old_fd) return(0);

  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((double)(ap->losamp) / (double)(ep->samps_per_bin));
  main_start = ap->losamp;
  
  if ((lo > newbeg) && (lo < newend)) 
    {
      for (i = newbeg; i < lo; i++) next_mix_sample(new_fd);
      new_start = lo;
    }
  else new_start = newbeg;
  if ((lo > oldbeg) && (lo < oldend)) 
    {
      for (i = oldbeg; i < lo; i++) next_mix_sample(old_fd);
      old_start = lo;
    }
  else old_start = oldbeg;

  lastx = ap->x_axis_x0;
  xfinc = samples_per_pixel / (Float)SND_SRATE(cp->sound);

  for (j = 0, xi = (double)lo, xf = ap->x0; 
       xi < (double)hi; 
       xi += samples_per_pixel, lastx++, xf += xfinc, j++)
    {
      main_ymin = 100.0;
      main_ymax = -100.0;
      old_ymin = 100.0;
      old_ymax = -100.0;
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
      if ((xi >= oldbeg) && (xi < oldend))
	{
	  while (old_start <= xi)
	    {
	      val = next_mix_sample(old_fd);
	      if (val > old_ymax) old_ymax = val;
	      if (val < old_ymin) old_ymin = val;
	      old_start++;
	    }
	}
      else
	{
	  old_ymin = 0.0;
	  old_ymax = 0.0;
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
		     local_grf_y(main_ymin - old_ymin + new_ymin, ap),
		     local_grf_y(main_ymax - old_ymax + new_ymax, ap));

    }

  erase_and_draw_both_grf_points(md->wg, cp, j);

  free_mix_fd(new_fd);
  free_mix_fd(old_fd);
  return(j);
}

static int make_temporary_amp_env_graph(chan_info *cp, axis_info *ap, mix_info *md, Float samples_per_pixel,
					off_t newbeg, off_t newend, off_t oldbeg, off_t oldend)
{
  /* temp graph using cp->amp_env and mix input amp envs */
  off_t main_start, new_start, old_start;
  Float val;
  mix_fd *new_min_fd, *new_max_fd, *old_min_fd, *old_max_fd;
  Float new_ymin, new_ymax, old_ymin, old_ymax, main_ymin, main_ymax;
  Float new_high, new_low, old_high, old_low;
  double xi, xf, xfinc;
  off_t lo, hi, x;
  int main_loc, j;
  Locus lastx;
  env_info *ep;

  lo = ap->losamp;
  hi = ap->hisamp;

  new_min_fd = init_mix_input_amp_env_read(md, CURRENT_MIX, LO_PEAKS); 
  new_max_fd = init_mix_input_amp_env_read(md, CURRENT_MIX, HI_PEAKS); 
  old_min_fd = init_mix_input_amp_env_read(md, PREVIOUS_MIX, LO_PEAKS);
  old_max_fd = init_mix_input_amp_env_read(md, PREVIOUS_MIX, HI_PEAKS); 

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

  if ((lo > oldbeg) && (oldend > lo))
    {
      for (x = lo; x < oldbeg; x += old_max_fd->samps_per_bin) 
	{
	  old_low = next_mix_sample(old_min_fd);
	  old_high = next_mix_sample(old_max_fd);
	}
      old_ymin = old_low;
      old_ymax = old_high;
      old_start = lo;
    }
  else 
    {
      old_start = oldbeg;
      old_ymin = 0.0;
      old_ymax = 0.0;
    }

  lastx = ap->x_axis_x0;
  xfinc = samples_per_pixel / (Float)SND_SRATE(cp->sound);

  for (j = 0, xi = (double)lo, xf = ap->x0; 
       xi < (double)hi; 
       xi += samples_per_pixel, lastx++, xf += xfinc, j++)
    {
      main_ymin = 100.0;
      main_ymax = -100.0;
      old_ymin = 100.0;
      old_ymax = -100.0;
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
      if ((xi >= oldbeg) && (xi < oldend))
	{
	  while (old_start <= xi)
	    {
	      old_low = next_mix_sample(old_min_fd);
	      old_high = next_mix_sample(old_max_fd);
	      if (old_high > old_ymax) old_ymax = old_high;
	      if (old_low < old_ymin) old_ymin = old_low;
	      old_start += old_max_fd->samps_per_bin;
	    }
	}
      else
	{
	  old_ymin = 0.0;
	  old_ymax = 0.0;
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
		     local_grf_y(main_ymin - old_ymin + new_ymin, ap),
		     local_grf_y(main_ymax - old_ymax + new_ymax, ap));

    }

  erase_and_draw_both_grf_points(md->wg, cp, j);

  free_mix_fd(new_min_fd);
  free_mix_fd(new_max_fd);
  free_mix_fd(old_min_fd);
  free_mix_fd(old_max_fd);
  return(j);
}

static void make_temporary_graph(chan_info *cp, mix_info *md, mix_state *cs)
{
  off_t oldbeg, newbeg, oldend, newend;
  off_t i, samps;
  int j;
  Locus xi;
  bool widely_spaced;
  axis_info *ap;
  snd_info *sp;
  mix_context *ms;
  Float samples_per_pixel, xf;
  double x, incr, initial_x;
  Float ina, ymin, ymax;
  off_t lo, hi;
  snd_fd *sf = NULL, *sfb, *afb;
  mix_fd *add = NULL, *sub = NULL;
  int x_start, x_end;
  double start_time, cur_srate;
  mus_sample_t mina, mymax, mymin;
  /* if fft is being displayed, this does not update it as we drag the mix because the fft-data reader
   * (apply_fft_window in snd-fft.c) reads the current to-be-fft'd data using init_sample_read, and
   * that sees the old data in this case (also if the fft is large enough, it looks for data beyond
   * the current graph right edge, but the mix dragger stops at the edge).
   */
  ms = md->wg;
  if (!(ms->p0)) return;
  oldbeg = cs->orig;
  newbeg = cs->beg;
  oldend = cs->end;
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
      add = init_mix_read(md, CURRENT_MIX, 0);
      if (!add) return;
      sub = init_mix_read(md, PREVIOUS_MIX, 0);
      if (!sub) return;
      if ((oldbeg < lo) && (lo < oldend)) 
	for (i = oldbeg; i < lo; i++) 
	  next_mix_sample(sub);
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
	  if ((i >= oldbeg) && (i <= oldend)) ina -= next_mix_sample(sub);
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
	    j = make_temporary_amp_env_graph(cp, ap, md, samples_per_pixel, newbeg, newend, oldbeg, oldend);
	  else j = make_temporary_amp_env_mixed_graph(cp, ap, md, samples_per_pixel, newbeg, newend, oldbeg, oldend);
	}
      else
	{
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (sf == NULL) return;
	  add = init_mix_read(md, CURRENT_MIX, 0);
	  if (!add) return;
	  sub = init_mix_read(md, PREVIOUS_MIX, 0);
	  if (!sub) return;
	  if ((oldbeg < lo) && (lo < oldend)) 
	    for (i = oldbeg; i < lo; i++) 
	      next_mix_sample(sub);
	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = local_grf_x(x, ap);
	  i = lo;
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = 100.0;
	  ymax = -100.0;

	  if ((add->calc == C_ZERO_SOUND) && (sub->calc == C_ZERO_SOUND))
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
	      if ((add->calc == C_STRAIGHT_SOUND) && (sub->calc == C_STRAIGHT_SOUND))
		{
		  mymax = MUS_SAMPLE_MIN;
		  mymin = MUS_SAMPLE_MAX;
		  sfb = sub->sfs[sub->base];
		  afb = add->sfs[add->base];
		  while (i <= hi)
		    {
		      mina = read_sample(sf);
		      if ((i >= oldbeg) && (i <= oldend)) mina -= read_sample(sfb);
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
		      if ((i >= oldbeg) && (i <= oldend)) ina -= next_mix_sample(sub);
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
  if (sf) free_snd_fd(sf);
  if (add) free_mix_fd(add);
  if (sub) free_mix_fd(sub);
  ms->lastpj = j;
}

static int display_mix_amp_env(mix_info *md, Float scl, int yoff, off_t newbeg, off_t newend, chan_info *cp, Float srate, axis_info *ap, bool draw)
{
  /* need min and max readers */
  mix_fd *min_fd, *max_fd;
  off_t hi, lo, sum;
  int j;
  Locus lastx, newx;
  Float ymin, ymax, high = 0.0, low = 0.0;
  double xend, xstart, xstep;
  min_fd = init_mix_input_amp_env_read(md, CURRENT_MIX, LO_PEAKS);
  max_fd = init_mix_input_amp_env_read(md, CURRENT_MIX, HI_PEAKS);
  lo = ap->losamp;
  hi = ap->hisamp;

  /* mix starts at newbeg, current display starts at lo,
     mix goes to newend, current display goes to hi,
     cp->amp_envs[cp->edit_ctr]->samps_per_bin is the original's amp_env step size
     mf->samps_per_bin is the mix input step size
  */

  /* fprintf(stderr, "display mix amp env: %f %d\n", scl, yoff); */

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
      xstart = (double)(newbeg) / (double)srate;
      ymin = 100.0;
      ymax = -100.0;
    }
  if (hi < newend)
    xend = ap->x1;
  else xend = (double)(newend) / (double)srate;
  xstep = (double)(max_fd->samps_per_bin) / (double)srate;
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
  if (draw)
    draw_both_grf_points(cp,
			 set_mix_waveform_context(cp, md),
			 j,
			 cp->time_graph_style);
  else draw_both_grf_points(cp, erase_context(cp), j, cp->time_graph_style);
  free_mix_fd(min_fd);
  free_mix_fd(max_fd);
  return(j);
}

static void draw_mix_tag(mix_info *md)
{
  chan_info *cp;
  int width, height;
  axis_context *ax;
  if ((md->x == md->tagx) && (md->y == md->tagy)) return; 
  cp = md->cp;
  width = mix_tag_width(ss);
  height = mix_tag_height(ss);
  ax = mark_context(cp);
  if (md->tagx > 0)
    fill_rectangle(ax,
		   md->tagx - width, md->tagy - height / 2,
		   width, height);
  fill_rectangle(ax,
		 md->x - width, md->y - height / 2,
		 width, height);
  md->tagx = md->x;
  md->tagy = md->y;
}

static Cessator watch_mix_proc = 0;    /* work proc if mouse outside graph causing axes to move */

static int display_mix_waveform(chan_info *cp, mix_info *md, mix_state *cs, bool draw)
{
  off_t i, newbeg, newend, endi;
  Float scl;
  int j = 0;
  off_t samps;
  Locus xi;
  bool widely_spaced;
  axis_info *ap;
  snd_info *sp;
  int yoff;
  Float samples_per_pixel, xf;
  double x, incr, initial_x;
  Float ina, ymin, ymax;
  off_t lo, hi;
  mix_fd *add = NULL;
  int x_start, x_end;
  double start_time, cur_srate;
  newbeg = cs->beg;
  newend = newbeg + cs->len;
  sp = cp->sound;
  ap = cp->axis;
  lo = ap->losamp;
  hi = ap->hisamp;
  if ((newend <= lo) || (newbeg >= hi)) return(0);
  scl = md->height;

  /* fprintf(stderr,"display mix waveform: %f\n", scl); */

  if ((ap->y_axis_y0 - ap->y_axis_y1) < scl) return(0);
  if (sp) 
    cur_srate = (double)SND_SRATE(sp);
  else cur_srate = (double)SND_SRATE((md->cp)->sound);
  start_time = (double)(ap->losamp) / cur_srate;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  if (newend > hi) newend = hi;
  samps = ap->hisamp - ap->losamp + 1;
  samples_per_pixel = (Float)((double)(samps - 1) / (Float)(x_end - x_start));
  yoff = md->y;

  if ((draw) && (!(watch_mix_proc)))
    {
      int xx;
      xx = local_grf_x((double)(newbeg + md->anchor) / cur_srate, ap);
      md->x = xx;
      draw_mix_tag(md);
    }

  if ((ss->just_time) && (event_pending())) return(0);

  if (sp) set_mix_waveform_context(cp, md);
  if ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE))
    {
      add = init_mix_read(md, CURRENT_MIX, 0);
      if (!add) return(0);
      if (lo > newbeg) 
	{
	  for (i = newbeg; i < lo; i++) next_mix_sample(add);
	  newbeg = lo;
	}
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
      if (sp)
	{
	  if (draw)
	    draw_grf_points(cp,
			    set_mix_waveform_context(cp, md),
			    j, ap, 
			    ungrf_y(ap, yoff), 
			    cp->time_graph_style);
	  else draw_grf_points(cp, 
			       erase_context(cp), 
			       j, ap, 
			       ungrf_y(ap, yoff), 
			       cp->time_graph_style);
	}
    }
  else
    {
      if ((cp->sound) && (mix_input_amp_env_usable(md, samples_per_pixel)))
	j = display_mix_amp_env(md, scl, yoff, newbeg, newend, cp, (Float)cur_srate, ap, draw);
      else
	{
	  add = init_mix_read(md, CURRENT_MIX, 0);
	  if (!add) return(0);
	  if (lo > newbeg) 
	    {
	      for (i = newbeg; i < lo; i++) next_mix_sample(add);
	      newbeg = lo;
	    }
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
	  if (sp)
	    {
	      if (draw)
		draw_both_grf_points(cp,
				     set_mix_waveform_context(cp, md),
				     j,
				     cp->time_graph_style);
	      else draw_both_grf_points(cp, 
					erase_context(cp), 
					j, 
					cp->time_graph_style);
	    }
	}
    }
  if (sp) copy_context(cp);
  if (add) free_mix_fd(add);
  return(j);
}



/* -------------------------------- moving mix tags -------------------------------- */

typedef struct {int *xs; int orig, x; off_t *origs;} track_graph_t;
static track_graph_t *track_drag_data;
static track_graph_t *track_save_graph(mix_info *orig_md);
static void finish_dragging_track(int track_id, track_graph_t *data, bool remix);
static track_graph_t *free_track_graph(track_graph_t *ptr);
static bool mix_dragged = false;

static int clear_mix_tags_1(mix_info *md, void *ignore)
{
  md->tagy = 0;
  md->tagx = 0;
  return(0);
}

void clear_mix_tags(chan_info *cp)
{
  map_over_channel_mixes(cp, clear_mix_tags_1, NULL);
}

static int clear_mix_y_1(mix_info *md, void *ignore)
{
  md->y = 0;
  return(0);
}

void clear_mix_y(chan_info *cp)
{
  map_over_channel_mixes(cp, clear_mix_y_1, NULL);
}

static void move_mix(mix_info *md);
static void move_track(int track_id, track_graph_t *data);

void move_mix_tag(int mix_tag, int x)
{
  /* from mouse tag drag in snd-chn.c only */
  mix_info *md;
  mix_dragged = true;
  md = md_from_id(mix_tag);
  if (md->track == 0)
    {
      md->x = x;
      move_mix(md);
    }
  else
    {
      /* track drag */
      track_drag_data->x = x;
      move_track(md->track, track_drag_data);
    }
}

void finish_moving_mix_tag(int mix_tag, int x)
{
  /* from mouse release after tag drag in snd-chn.c only */
  mix_info *md;
  mix_state *cs = NULL;
  mix_context *ms;
  XEN res = XEN_FALSE;
  if (watch_mix_proc) 
    {
      BACKGROUND_REMOVE(watch_mix_proc);
      watch_mix_proc = 0;
    }
  md = md_from_id(mix_tag);
  mix_dragged = false;
  if (XEN_HOOKED(mix_release_hook))
    res = run_progn_hook(mix_release_hook,
			 XEN_LIST_2(C_TO_XEN_INT(md->id),
				    C_TO_XEN_OFF_T(cs->beg - cs->orig)),
			 S_mix_release_hook);
  if (md->track == 0)
    {
      md->x = x;
      cs = md->active_mix_state;
      ms = md->wg;
      ms->lastpj = 0;
      if (cs->beg == cs->orig) return;
      reflect_mix_in_mix_panel(md->id);
      if (!(XEN_TRUE_P(res)))
	remix_file(md, "Mix: drag", true);
    }
  else
    {
      track_drag_data->x = x;
      finish_dragging_track(md->track, track_drag_data, (!(XEN_TRUE_P(res))));
      track_drag_data = free_track_graph(track_drag_data);
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
  mx = map_over_channel_mixes(cp, hit_mix_1, (void *)xy);
  if (mx > 0)
    {
      mix_info *md;
      md = md_from_id(mx - 1);
      if (md->track == 0)
	{
	  mix_save_graph(md->wg, make_graph(cp));
	}
      else
	{
	  track_drag_data = track_save_graph(md);
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

void start_mix_panel_slider_drag(int mix_id)
{
  /* the "drag" here refers to the mix panel amp or speed control */
  mix_info *md;
  md = md_from_id(mix_id);
  mix_save_graph(md->wg, make_graph(md->cp));
}

/* for axis movement (as in mark drag off screen) */

static void move_mix(mix_info *md)
{
  axis_info *ap;
  chan_info *cp;
  mix_state *cs;
  int nx, x;
  bool updated = false;
  off_t samps, samp;
  cp = md->cp;
  if (!cp) return;
  ap = cp->axis;
  if (!ap) return;
  /* fprintf(stderr,"move %d to %d\n", md->id, md->x); */
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
	watch_mix_proc = BACKGROUND_ADD(watch_mix, md);
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
      if (show_mix_waveforms(ss)) erase_mix_waveform(md);
      md->nx = nx;
      samp = (off_t)(ungrf_x(ap, nx) * SND_SRATE(cp->sound));
      if (samp < 0) samp = 0;
      samps = CURRENT_SAMPLES(cp);
      if (samp > samps) samp = samps;
      /* now redraw the mix and reset its notion of begin time */
      /* actually should make a new state if cp->edit_ctr has changed ?? */
      cs->beg = samp - md->anchor;
      if (cs->beg < 0) 
	{
	  cs->beg = 0; 
	  md->anchor = samp;
	}
      reflect_mix_in_mix_panel(md->id);
      if (show_mix_waveforms(ss)) draw_mix_waveform(md);
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
  mix_info *md, *curmd = NULL;
  mix_state *cs;
  axis_info *ap;
  off_t lo, hi, spot;
  int i, curmaxctr = -1;
  ap = cp->axis;
  lo = ap->losamp;
  hi = ap->hisamp;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  cs = md->active_mix_state;
	  spot = cs->orig + md->anchor;
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
      if (cs) return(cs->orig + md->anchor);
    }
  return(-1);
}

static mix_state *backup_mix_state(chan_info *cp, mix_info *md)
{
  mix_state *cs;
  /* undo -- look for first (last in list) cs with cs->edit_ctr <= cp->edit_ctr mix_state */
  cs = md->states[md->current_state];
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
  mix_info *md;
  int i;
  if (cp)
    {
      for (i = 0; i < mix_infos_ctr; i++)
	{
	  md = mix_infos[i];
	  if ((md) && (md->cp == cp))
	    {
	      if (md->wg) FREE(md->wg);
	      md->wg = set_mix_info_context(cp);
	    }
	}
    }
}

void display_channel_mixes(chan_info *cp)
{
  /* called in display_channel_data if show_mix_mix_states(ss) and cp->mixes
   * this needs to spin through the mixes, 
   *   un-manage those whose mix has wandered off screen, and release the associated widgets,
   *   and re-manage those that are now active, grabbing widgets if necessary.
   */
  mix_info *md;
  mix_state *cs;
  axis_info *ap;
  off_t lo, hi, spot;
  int i, xspot, turnover, y, hgt;
  bool combined;
  combined = (((snd_info *)(cp->sound))->channel_style == CHANNELS_SUPERIMPOSED);
  ap = cp->axis;
  lo = ap->losamp;
  hi = ap->hisamp;
  turnover = (int)(ap->height * 0.4);
  y = mix_tag_height(ss);
  hgt = y;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  cs = md->active_mix_state;
	  if (cs->edit_ctr > cp->edit_ctr) 
	    cs = backup_mix_state(cp, md);
	  else
	    if (cs->edit_ctr < cp->edit_ctr)
	      cs = restore_mix_state(cp, md);
	  if ((cs) && (!cs->locked))
	    {
	      spot = cs->orig + md->anchor;
	      if ((cs->edit_ctr <= cp->edit_ctr) && (spot >= lo) && (spot <= hi))
		{
		  xspot = local_grf_x((double)spot / (double)SND_SRATE(cp->sound), ap);
		  if (md->tag_y != 0)
		    md->y = OFFSET_FROM_TOP + ap->y_offset + md->tag_y;
		  else
		    {
		      if (md->y == 0)
			md->y = OFFSET_FROM_TOP + y + ap->y_offset;
		    }
		  if (((xspot + mix_tag_width(ss)) <= ap->x_axis_x1) &&      /* not cut off on right */
		      (!combined))
		    {
		      if (cp->show_mix_waveforms) draw_mix_waveform(md);
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
  chan_info *cp;
  lock_mixes_t *times = (lock_mixes_t *)ptr;
  /* find affected mix_state, extend and lock */
  cs = md->states[md->current_state];
  if (!(cs->locked))
    {
      if (((cs->orig >= times->lt_beg) && (cs->orig <= times->lt_end)) ||
	  ((cs->end >= times->lt_beg) && (cs->end <= times->lt_end)) ||
	  ((cs->orig < times->lt_beg) && (cs->end > times->lt_end)))
	{
	  extend_mix_state_list(md);
	  if (md->states[md->current_state]) free_mix_state(md->states[md->current_state]);
	  md->states[md->current_state] = (mix_state *)CALLOC(1, sizeof(mix_state));
	  cs = md->states[md->current_state];
	  cs->locked = true;
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
  /* search through cp->mixes for any active mixes that overlap beg..end 
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
  map_over_channel_mixes(cp, lock_affected_mixes_1, (void *)(&lt));
  reflect_mix_in_menu();
}

void release_pending_mixes(chan_info *cp, int edit_ctr)
{
  /* look for mixes that have been cancelled by undo followed by unrelated edit */
  mix_info *md;
  mix_state *cs;
  int i;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  cs = md->states[0];
	  if (cs->edit_ctr >= edit_ctr) free_mix_info(md);
	}
    }
}

static int update_mix(mix_info *md, void *ptr)
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
  return(0);
}

void reset_mix_list(chan_info *cp)
{
  map_over_channel_mixes(cp, update_mix, NULL);
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
  mix_state *cs, *ncs;
  mixrip *data;
  chan_info *cp;
  data = (mixrip *)ptr;
  cp = data->cp;
  cs = md->active_mix_state;
  if ((cs) && (!(cs->locked)) && (cs->beg > data->beg) && (ready_mix(md)))
    {
      ncs = copy_mix_state(cs);
      ncs->edit_ctr = cp->edit_ctr;
      ncs->orig = cs->beg + data->change;
      /* fprintf(stderr,"ripple set %d orig to " OFF_TD "\n", md->id, ncs->orig); */
      ncs->beg = ncs->orig;
      ncs->end = ncs->beg + cs->len - 1;
      if (cp->show_mix_waveforms) erase_mix_waveform(md);
      extend_mix_state_list(md);
      if (md->states[md->current_state]) free_mix_state(md->states[md->current_state]);
      md->states[md->current_state] = ncs;
      make_current_mix_state(md);
      if (cp->show_mix_waveforms) draw_mix_waveform(md);
    }
  return(0);
}

void ripple_mixes(chan_info *cp, off_t beg, off_t change)
{
  /* look for active mixes in cp, fixup pointers and move along with edit_ctr */
  /* if they have a mixer, move it too */
  mixrip *mp;
  if ((cp) && (cp->mixes))
    {
      mp = (mixrip *)CALLOC(1, sizeof(mixrip));
      mp->beg = beg;
      mp->change = change;
      mp->cp = cp;
      mp->ap = cp->axis;
      map_over_channel_mixes(cp, ripple_mixes_1, (void *)mp);
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
  off_t *css;
  mix_state *cs;
  if ((!cp) || (!cp->mixes)) return;
  k = 0;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp)) k++;
    }
  if (k > 0)
    {
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
	      cs = md->active_mix_state;
	      if (!(cs->locked))
		{
		  css[j] = cs->orig;
		  j++;
		}
	    }
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

bool mix_ok(int n) 
{
  mix_info *md; 
  md = md_from_id(n); 
  return((md) && 
	 (md->states) && 
	 (md->states[0]) && 
	 (md->cp) &&
	 (((md->states[0])->edit_ctr) <= ((md->cp)->edit_ctr)));
}

bool mix_ok_and_unlocked(int n)
{
  mix_info *md; 
  md = md_from_id(n); 
  return((md) && 
	 (md->active_mix_state) &&
	 (!(md->active_mix_state->locked)) &&
	 (md->states) && 
	 (md->states[0]) && 
	 (md->cp) &&
	 (((md->states[0])->edit_ctr) <= ((md->cp)->edit_ctr)));
}

int any_mix_id(void)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++) 
    if (mix_ok_and_unlocked(i)) 
      return(i);
  reflect_no_mix_in_mix_panel();
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
  display_mix_waveform(md->cp, md, md->active_mix_state, true);
}

static void erase_mix_waveform(mix_info *md) 
{
  display_mix_waveform(md->cp, md, md->active_mix_state, false);
}


#if MAC_OSX
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
  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_CHANNEL, 2, val);
  if (outchans < (int)(val[1])) outchans = (int)(val[1]);
  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, val);
  frames = (int)(val[0]);
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
	      if ((ss->stopped_explicitly) || ((from_gui) && (mix_play_stopped())))
		{
		  ss->stopped_explicitly = false;
		  report_in_minibuffer(sp, _("stopped"));
		  break;
		}
	    }
	  mus_audio_close(play_fd);
	  play_fd = -1;
	}
      free_mix_fd(mf);
    }
  reflect_mix_play_stop();
#if HAVE_ALSA
  FREE(buf[0]);
  FREE(outbuf);
#endif
  FREE(buf);
}

void mix_play_from_id(int mix_id)
{
  play_mix(md_from_id(mix_id), 0, true);
}

static mix_state *cs_from_id(int n)
{
  mix_info *md;
  mix_state *cs;
  chan_info *cp;
  md = md_from_id(n);
  if (md)
    {
      cp = md->cp;
      cs = md->active_mix_state;
      if (cs->edit_ctr > cp->edit_ctr) /* used to depend on redisplay here */
	cs = backup_mix_state(cp, md);
      else
	{
	  if (cs->edit_ctr < cp->edit_ctr)
	    cs = restore_mix_state(cp, md);
	}
      return(cs);
    }
  return(NULL);
}

static int mix_id_from_channel_position(chan_info *cp, off_t pos)
{
  int n;
  mix_info *md;
  mix_state *cs; 
  if (cp->mixes)
    {
      for (n = 0; n < mix_infos_size; n++)
	{
	  md = mix_infos[n];
	  if ((md) && (md->cp == cp))
	    {
	      cs = md->active_mix_state;
	      if ((cs) && 
		  ((cs->orig == pos) || (pos == -1)))
		return(md->id);
	    }
	}
    }
  return(INVALID_MIX_ID);
}

off_t mix_frames(int n) 
{
  mix_state *cs; 
  cs = cs_from_id(n); 
  if (cs) 
    return(cs->len); 
  return(-1);
}

static int set_mix_amp(int mix_id, int chan, Float val, bool from_gui, bool remix)
{
  mix_info *md;
  mix_state *cs;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      if (val >= 0.0)
	{
	  if (md->in_chans > chan)
	    {
	      cs = md->active_mix_state;
	      if (!from_gui) 
		{
		  if ((cs->scalers[chan] == val) && /* drag sets scalers, so we can't optimize that case without some difficulties */
		      (md->cp->sound->sync == 0))
		    return(mix_id);
		  cs->scalers[chan] = val;
		  reflect_mix_in_mix_panel(mix_id);
		  remix_file(md, S_setB S_mix_amp, true);
		}
	      else
		{
		  cs->scalers[chan] = val;
		  if (remix)
		    remix_file(md, S_setB S_mix_amp, true);
		  else make_temporary_graph(md->cp, md, cs);
		}
	    }
	  else return(INVALID_MIX_CHANNEL);
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

int set_mix_amp_from_id(int mix_id, int chan, Float val, bool dragging)
{
  return(set_mix_amp(mix_id, chan, val, true, !dragging));
}

Float mix_amp_from_id(int mix_id, int chan)
{
  mix_info *md;
  mix_state *cs;
  md = md_from_id(mix_id);
  if (md)
    {
      if (md->in_chans > chan)
	{
	  cs = md->active_mix_state;
	  return(cs->scalers[chan]);
	}
    }
  return(0.0);
}

static int set_mix_speed(int mix_id, Float val, bool from_gui, bool remix)
{
  mix_info *md;
  char srcbuf[16];
  mix_state *cs;
  Float new_speed, trk_speed = 1.0;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      if (val != 0.0)
	{
	  cs = md->active_mix_state;
	  new_speed = srate_changed(val, srcbuf, speed_control_style(ss), speed_control_tones(ss)); 
	  trk_speed = gather_track_speed(md->track);
	  if (!from_gui)
	    {
	      if ((cs->speed == new_speed) && /* drag sets speed, so we can't optimize that case without some difficulties */
		  (md->cp->sound->sync == 0))
		return(mix_id);
	      cs->speed = new_speed;
	      cs->len = (off_t)(ceil(md->in_samps / (cs->speed * trk_speed)));
	      reflect_mix_in_mix_panel(mix_id);
	      remix_file(md, S_setB S_mix_speed, true);
	    }
	  else
	    {
	      cs->speed = new_speed;
	      cs->len = (off_t)(ceil(md->in_samps / (cs->speed * trk_speed)));
	      if (remix)
		remix_file(md, S_setB S_mix_speed, true);
	      else make_temporary_graph(md->cp, md, cs);
	    }
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

int set_mix_speed_from_id(int mix_id, Float val, bool dragging) 
{
  return(set_mix_speed(mix_id, val, true, !dragging));
}

Float mix_speed_from_id(int mix_id)
{
  mix_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->speed);
  return(1.0);
}

void set_mix_track_from_id(int mix_id, int track)
{
  /* mix panel track widget */
  mix_info *md;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(md->id))) set_mix_track(md, track, false);
}

int mix_track_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) return(md->track);
  return(0);
}

int set_mix_position(int mix_id, off_t val)
{
  mix_info *md;
  mix_state *cs = NULL;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      cs = md->active_mix_state;
      if (cs)
	{
	  if ((val == cs->beg) &&
	      (md->cp->sound->sync == 0))
	    return(mix_id);
	  if (val >= 0) 
	    cs->beg = val; 
	  else cs->beg = 0;
	  reflect_mix_in_mix_panel(mix_id);
	  remix_file(md, S_setB S_mix_position, true); 
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

off_t mix_position_from_id(int mix_id)
{
  mix_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->beg);
  return(0);
}

int mix_input_chans_from_id(int mix_id)
{
  mix_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->chans);
  return(0);
}

env *mix_amp_env_from_id(int n, int chan) 
{
  mix_state *cs; 
  cs = cs_from_id(n); 
  if ((cs) && (chan < cs->chans) && (cs->amp_envs))
    return(cs->amp_envs[chan]);
  return(NULL);
}

env **mix_panel_envs(int n)
{
  mix_info *md;
  mix_state *cs; 
  md = md_from_id(n);
  if (md) 
    {
      cs = md->active_mix_state;
      if (cs)
	{
	  if (md->panel_envs == NULL)
	    {
	      int i;
	      Float flat[4] = {0.0, 1.0, 1.0, 1.0};
	      md->panel_envs = (env **)CALLOC(md->in_chans, sizeof(env *));
	      for (i = 0; i < md->in_chans; i++)
		if ((cs->amp_envs) && (cs->amp_envs[i]))
		  md->panel_envs[i] = copy_env(cs->amp_envs[i]);
		else md->panel_envs[i] = make_envelope(flat, 4);
	    }
	}
      return(md->panel_envs);
    }
  return(NULL);
}

void reflect_edit_in_mix_panel_envs(int n)
{
  mix_info *md;
  mix_state *cs; 
  md = md_from_id(n);
  if (md) 
    {
      cs = md->active_mix_state;
      if (cs)
	{
	  int i;
	  Float flat[4] = {0.0, 1.0, 1.0, 1.0};
	  if (md->panel_envs == NULL)
	    md->panel_envs = (env **)CALLOC(md->in_chans, sizeof(env *));
	  for (i = 0; i < md->in_chans; i++)
	    {
	      if (md->panel_envs[i]) free_env(md->panel_envs[i]);
	      if ((cs->amp_envs) && (cs->amp_envs[i]))
		md->panel_envs[i] = copy_env(cs->amp_envs[i]);
	      else md->panel_envs[i] = make_envelope(flat, 4);
	    }
	}
    }
}

env *mix_panel_env(int n, int chan)
{
  env **envs;
  envs = mix_panel_envs(n);
  if (envs) return(envs[chan]);
  return(NULL);
}

static int set_mix_amp_env_1(int n, int chan, env *val, bool remix)
{
  mix_info *md;
  env *old_env = NULL, *old_panel_env = NULL;
  mix_state *cs;
  md = md_from_id(n);
  if ((md) && (mix_ok_and_unlocked(n)))
    {
      if (md->in_chans > chan)
	{
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
	  if ((md->panel_envs) &&
	      (md->panel_envs[chan]))
	    {
	      old_panel_env = md->panel_envs[chan];
	      md->panel_envs[chan] = copy_env(val);
	      free_env(old_panel_env);
	    }
	  if (old_env) free_env(old_env);
	  if (remix) 
	    remix_file(md, S_setB S_mix_amp_env, true);
	  return(0);
	}
      else return(INVALID_MIX_CHANNEL);
    }
  else return(INVALID_MIX_ID);
}

int set_mix_amp_env(int n, int chan, env *val) {return(set_mix_amp_env_1(n, chan, val, true));}
int set_mix_amp_env_without_edit(int n, int chan, env *val) {return(set_mix_amp_env_1(n, chan, val, false));}

void mix_at_x_y(int data, char *filename, int x, int y)
{
  int chn, snd;
  snd_info *sp = NULL;
  chan_info *cp;
  char *origin;
  off_t sample;
  char *fullname = NULL;
  chn = UNPACK_CHANNEL(data);
  snd = UNPACK_SOUND(data);
  if ((snd >= 0) &&
      (snd < ss->max_sounds) && 
      (snd_ok(ss->sounds[snd])) &&
      (chn >= 0) &&
      (chn < ss->sounds[snd]->nchans) &&
      (mus_file_probe(filename)))
    {
      sp = ss->sounds[snd];
      cp = sp->chans[chn];
      if ((sp->nchans > 1) && (sp->channel_style == CHANNELS_COMBINED))
	{
	  cp = which_channel(sp, y);
	  chn = cp->chan;
	}
      select_channel(sp, chn);
      origin = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      sample = snd_round_off_t(ungrf_x(cp->axis, x) * (double)(SND_SRATE(sp)));
      if (sample < 0) sample = 0;
      mus_snprintf(origin, PRINT_BUFFER_SIZE, "drop mix %s " OFF_TD, filename, sample);
      fullname = mus_expand_filename(filename);
      mix_complete_file(sp, sample, fullname, origin, with_mix_tags(ss), DONT_DELETE_ME, 0, false);
      if (fullname) FREE(fullname);
      FREE(origin);
    }
}


static XEN snd_no_such_mix_error(const char *caller, XEN n)
{
  XEN_ERROR(NO_SUCH_MIX,
	XEN_LIST_2(C_TO_XEN_STRING(caller),
		   n));
  return(XEN_FALSE);
}

static XEN g_mix_position(XEN n) 
{
  #define H_mix_position "(" S_mix_position " id): sample number of start of mix"
  mix_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_position, "an integer");
  cs = cs_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_position, n));
  return(C_TO_XEN_OFF_T(cs->beg));  /* was orig 6-May-03 */
}

static XEN g_mix_chans(XEN n) 
{
  #define H_mix_chans "(" S_mix_chans " id): input channels in mix"
  mix_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_chans, "an integer");
  cs = cs_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_chans, n));
  return(C_TO_XEN_INT(cs->chans));
}

static XEN g_mix_p(XEN n) 
{
  #define H_mix_p "(" S_mix_p " id): #t if mix is active and accessible"
  if (XEN_INTEGER_P(n))
    return(C_TO_XEN_BOOLEAN(mix_ok(XEN_TO_C_INT_OR_ELSE(n, 0))));
  return(XEN_FALSE);
}

static XEN g_mix_frames(XEN n) 
{
  #define H_mix_frames "(" S_mix_frames " id): mix's length in frames"
  off_t len;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_frames, "an integer");
  len = mix_frames(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (len == -1) 
    return(snd_no_such_mix_error(S_mix_frames, n));
  return(C_TO_XEN_OFF_T(len));
}

static XEN g_mix_locked(XEN n) 
{
  #define H_mix_locked_p "(" S_mix_locked_p " id): #t if mix cannot be moved (due to subsequent edits overlapping it)"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_locked_p, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_locked_p, n));
  return(C_TO_XEN_BOOLEAN((md->active_mix_state)->locked));
}

static XEN g_mix_anchor(XEN n) 
{
  #define H_mix_anchor "(" S_mix_anchor " id): location of mix anchor (determines tag position within mix)"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_anchor, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_anchor, n));
  return(C_TO_XEN_OFF_T(md->anchor));
}

static XEN g_mix_track(XEN n) 
{
  #define H_mix_track "(" S_mix_track " id): track that mix is a member of"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_track, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_track, n));
  return(C_TO_XEN_INT(md->track));
}

static XEN g_mix_tag_y(XEN n) 
{
  #define H_mix_tag_y "(" S_mix_tag_y " id): height of mix's tag"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_tag_y, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_tag_y, n));
  return(C_TO_XEN_INT(md->tag_y));
}

static XEN g_mix_speed(XEN n) 
{
  #define H_mix_speed "(" S_mix_speed " id): srate (speed slider setting) of mix"
  mix_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_speed, "an integer");
  cs = cs_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_speed, n));
  return(C_TO_XEN_DOUBLE(cs->speed));
}

static XEN g_mixes(XEN snd, XEN chn)
{
  #define H_mixes "(" S_mixes " (snd) (chn)): list of mixes (ids) associated with snd and chn"
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
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_home, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_home, n));
  return(XEN_LIST_2(C_TO_XEN_INT(((md->cp)->sound)->index),
		    C_TO_XEN_INT(((md->cp)->chan))));
}

static XEN g_mix_amp(XEN n, XEN uchan) 
{
  #define H_mix_amp "(" S_mix_amp " id (chan 0)): amp of mix's channel chan"
  mix_state *cs; 
  int chan;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ARG_1, S_mix_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(uchan), uchan, XEN_ARG_2, S_mix_amp, "an integer");
  cs = cs_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_amp, n));
  chan = XEN_TO_C_INT_OR_ELSE(uchan, 0);
  if (chan >= cs->chans)
    return(snd_no_such_channel_error(S_mix_amp, XEN_LIST_1(n), uchan));
  return(C_TO_XEN_DOUBLE(cs->scalers[chan]));
}

static XEN g_mix_amp_env(XEN n, XEN chan) 
{
  #define H_mix_amp_env "(" S_mix_amp_env " id (chan 0)): amplitude envelope applied to mix's channel chan"
  env *e;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ARG_1, S_mix_amp_env, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_mix_amp_env, "an integer");
  e = mix_amp_env_from_id(XEN_TO_C_INT_OR_ELSE(n, 0), 
			  XEN_TO_C_INT_OR_ELSE(chan, 0));
  if (e) return(env_to_xen(e));
  return(XEN_EMPTY_LIST);
}

static XEN g_set_mix_position(XEN n, XEN uval) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(n), n, XEN_ARG_1, S_setB S_mix_position, "a number");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(uval), uval, XEN_ARG_2, S_setB S_mix_position, "an integer");
  if (set_mix_position(XEN_TO_C_INT_OR_ELSE(n, 0), 
		       beg_to_sample(uval, S_setB S_mix_position)) == INVALID_MIX_ID)
    return(snd_no_such_mix_error(S_setB S_mix_position, n));
  return(uval);
}

static void set_mix_locked(mix_info *md, bool on, bool redisplay)
{
  mix_state *cs;
  cs = md->states[md->current_state];
  cs->locked = on;
  cs = md->active_mix_state;
  cs->locked = on;
  if (redisplay)
    {
      reflect_mix_in_menu();
      update_graph(md->cp);
    }
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

static bool delete_mix_1(int mix_id, bool redisplay)
{
  mix_info *md; 
  mix_state *cs;
  int i;
  if (mix_ok_and_unlocked(mix_id))
    {
      md = md_from_id(mix_id);
      cs = md->active_mix_state;
      for (i = 0; i < md->in_chans; i++)
	cs->scalers[i] = 0.0;
      reflect_mix_in_mix_panel(mix_id);
      remix_file(md, S_delete_mix, false);
      cs->locked = true;
      cs = md->states[md->current_state];
      cs->locked = true;
      if (redisplay) update_graph(md->cp);
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

static XEN g_set_mix_anchor(XEN n, XEN uval) 
{
  mix_info *md;
  mix_state *cs = NULL;
  off_t val;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_anchor, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(uval), uval, XEN_ARG_2, S_setB S_mix_anchor, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error(S_setB S_mix_anchor, n));
  cs = md->active_mix_state;
  if (cs)
    {
      val = beg_to_sample(uval, S_setB S_mix_anchor);
      if (val >= 0)
	{
	  md->anchor = val;
	  update_graph(md->cp);
	}
    }
  return(uval);
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
      set_mix_track(md, XEN_TO_C_INT(val), true);
      reflect_mix_in_mix_panel(md->id);
    }
  return(val);
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

static XEN g_set_mix_speed(XEN n, XEN uval) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_mix_speed, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(uval), uval, XEN_ARG_2, S_setB S_mix_speed, "a number");
  if (set_mix_speed(XEN_TO_C_INT(n), XEN_TO_C_DOUBLE(uval), false, true) == INVALID_MIX_ID)
    return(snd_no_such_mix_error(S_setB S_mix_speed, n));
  return(uval);
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

static XEN g_set_mix_amp_env(XEN n, XEN chan_1, XEN val_1) 
{
  env *e = NULL;
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
  XEN_ASSERT_TYPE(XEN_LIST_P(val) || XEN_FALSE_P(val), val, XEN_ARG_3, S_setB S_mix_amp_env, "a list or #f");
  if (XEN_LIST_P(val)) e = get_env(val, S_setB S_mix_amp_env);
  res = set_mix_amp_env(XEN_TO_C_INT(n), 
			(XEN_BOUND_P(chan)) ? XEN_TO_C_INT(chan) : 0,
			e);
  if (e) free_env(e);
  if (res == INVALID_MIX_ID)
    return(snd_no_such_mix_error(S_setB S_mix_amp_env, n));
  else 
    if (res == INVALID_MIX_CHANNEL)
      return(snd_no_such_channel_error(S_setB S_mix_amp_env, n, chan)); 
  return(val);
}

static void update_mix_waveforms(chan_info *cp)
{
  if ((cp) && (cp->mixes)) update_graph(cp);
}

static int update_mix_waveform_height(mix_info *md, void *new_val)
{
  int *val = (int *)new_val;
  md->height = val[0];
  return(0);
}

static XEN g_mix_waveform_height(void) {return(C_TO_XEN_INT(mix_waveform_height(ss)));}
static XEN g_set_mix_waveform_height(XEN val) 
{
  #define H_mix_waveform_height "(" S_mix_waveform_height "): max height (pixels) of mix waveforms (20)"
  int new_val[1];
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_waveform_height, "a number"); 
  new_val[0] = XEN_TO_C_INT_OR_ELSE(val, 0);
  in_set_mix_waveform_height(new_val[0]);
  map_over_mixes(update_mix_waveform_height, (void *)new_val);
  for_each_chan(update_mix_waveforms);
  return(C_TO_XEN_INT(mix_waveform_height(ss)));
}

/* I think this is for internal debugging */
#define S_mix_tag_position "mix-tag-position"
static XEN g_mix_tag_position(XEN id)
{
  #define H_mix_tag_position "(" S_mix_tag_position " id): position of mix tag (list of x and y)"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ONLY_ARG, S_mix_tag_position, "an integer");
  md = md_from_id(XEN_TO_C_INT(id));
  if (md) 
    return(XEN_LIST_2(C_TO_XEN_INT(md->x),
		      C_TO_XEN_INT(md->y)));
  return(snd_no_such_mix_error(S_mix_tag_position, id));
}

static XEN g_mix_tag_width(void) {return(C_TO_XEN_INT(mix_tag_width(ss)));}
static XEN g_set_mix_tag_width(XEN val) 
{
  #define H_mix_tag_width "(" S_mix_tag_width "): width (pixels) of mix tags (6)"
  int width;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_tag_width, "an integer"); 
  width = XEN_TO_C_INT(val);
  set_mix_tag_width(width);
  for_each_chan(update_graph);
  return(C_TO_XEN_INT(mix_tag_width(ss)));
}

static XEN g_mix_tag_height(void) {return(C_TO_XEN_INT(mix_tag_height(ss)));}
static XEN g_set_mix_tag_height(XEN val) 
{
  #define H_mix_tag_height "(" S_mix_tag_height "): height (pixels) of mix tags (14)"
  int height;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mix_tag_height, "an integer"); 
  height = XEN_TO_C_INT(val);
  set_mix_tag_height(height);
  for_each_chan(update_graph);
  return(C_TO_XEN_INT(mix_tag_height(ss)));
}

static XEN g_mix_file(XEN file, XEN chn_samp_n, XEN file_chn, XEN snd_n, XEN chn_n, XEN tag, XEN auto_delete, XEN track_id)
{
  #define H_mix_file "(" S_mix " file (chn-start 0) (file-chan 0) (snd #f) (chn #f) (with-tag " S_with_mix_tags ") (auto-delete #f) (track-id 0)): \
mix file channel file-chan into snd's channel chn starting at chn-start (or at the cursor location if chn-start \
is omitted), returning the new mix's id.  if with-tag is #f, the data is mixed (no draggable tag is created). \
If file_chn is omitted or #t, file's channels are mixed until snd runs out of channels. \
track-id is the track value for each newly created mix."

  chan_info *cp = NULL;
  char *name = NULL;
  int chans, id = -1, file_channel, track_num = 0;
  bool with_mixer = true, delete_file = false;
  mix_info *md;
  off_t beg;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mix, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(chn_samp_n), chn_samp_n, XEN_ARG_2, S_mix, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(file_chn) || XEN_BOOLEAN_P(file_chn) || (!(XEN_BOUND_P(file_chn))), file_chn, XEN_ARG_3, S_mix, "an integer or boolean");
  ASSERT_CHANNEL(S_mix, snd_n, chn_n, 4);
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(tag), tag, XEN_ARG_6, S_mix, "a number");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(auto_delete), auto_delete, XEN_ARG_7, S_mix, "a boolean");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(track_id), track_id, XEN_ARG_8, S_mix, "a track id");
  name = mus_expand_filename(XEN_TO_C_STRING(file));
  if (!(mus_file_probe(name)))
    {
      if (name) FREE(name);
      return(snd_no_such_file_error(S_mix, file));
    }
  ss->catch_message = NULL;
  if (XEN_NOT_BOUND_P(tag))
    with_mixer = with_mix_tags(ss);
  else with_mixer = XEN_TO_C_BOOLEAN_OR_TRUE(tag);
  cp = get_cp(snd_n, chn_n, S_mix);
  beg = XEN_TO_C_OFF_T_OR_ELSE(chn_samp_n, CURSOR(cp));
  if (XEN_BOOLEAN_P(auto_delete)) delete_file = XEN_TO_C_BOOLEAN(auto_delete);
  if (XEN_INTEGER_P(track_id))
    {
      track_num = XEN_TO_C_INT(track_id);
      if ((track_num > 0) && (!(track_p(track_num))))
	XEN_ERROR(NO_SUCH_TRACK,
		  XEN_LIST_2(C_TO_XEN_STRING(S_mix),
			     C_TO_XEN_INT(track_id)));
    }
  if ((!(XEN_INTEGER_P(file_chn))) || (XEN_TO_C_INT(file_chn) == 0))
    {
      /* #t = if not sync, set it for this op */
      id = mix_complete_file(any_selected_sound(), beg, name, S_mix, with_mixer, 
			     (delete_file) ? DELETE_ME : DONT_DELETE_ME, track_num,
			     XEN_TRUE_P(file_chn) || (!(XEN_BOUND_P(file_chn))));
      if (id == MIX_FILE_NO_FILE) 
	{
	  if (name) FREE(name);
	  if (ss->catch_message)
	    XEN_ERROR(MUS_MISC_ERROR,
		      XEN_LIST_2(C_TO_XEN_STRING(S_mix),
				 C_TO_XEN_STRING(ss->catch_message)));
	  return(snd_no_such_file_error(S_mix, file));
	}
    }
  else
    {
      chans = mus_sound_chans(name);
      if (chans <= 0)
	XEN_ERROR(BAD_HEADER,
		  XEN_LIST_4(C_TO_XEN_STRING(S_mix),
			     file,
			     C_TO_XEN_STRING("chans <= 0"),
			     C_TO_XEN_INT(chans)));
      file_channel = XEN_TO_C_INT(file_chn);
      if (file_channel >= chans)
	XEN_ERROR(NO_SUCH_CHANNEL,
		  XEN_LIST_3(C_TO_XEN_STRING(S_mix),
			     C_TO_XEN_STRING("chan: ~A, ~A chans: ~A"),
			     XEN_LIST_3(file_chn,
					file,
					C_TO_XEN_INT(chans))));
      if (chans > 0)
	{
	  ss->catch_message = NULL;
	  md = file_mix_samples(beg,
				mus_sound_frames(name), 
				name,
				cp, 
				file_channel,
				(delete_file) ? DELETE_ME : DONT_DELETE_ME, 
				S_mix,
				with_mixer,
				track_num, 
				true);
	  if (md) 
	    id = md->id;
	  else
	    if (ss->catch_message)
	      XEN_ERROR(MUS_MISC_ERROR,
			XEN_LIST_2(C_TO_XEN_STRING(S_mix),
				   C_TO_XEN_STRING(ss->catch_message)));

	}
      else 
	{
	  if (name) FREE(name);
	  return(snd_no_such_file_error(S_mix, file));
	}
    }
  if (name) FREE(name);
  return(C_TO_XEN_INT(id));
}



/* ---------------- mix sample readers ---------------- */

static XEN_OBJECT_TYPE mf_tag;
static bool mf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, mf_tag));}
#define TO_MIX_SAMPLE_READER(obj) ((mix_fd *)XEN_OBJECT_REF(obj))
#define MIX_SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, mf_tag)

static XEN g_mf_p(XEN obj) 
{
  #define H_mf_p "(" S_mix_sample_reader_p " obj): #t if obj is a mix-sample-reader"
  return(C_TO_XEN_BOOLEAN(mf_p(obj)));
}

static char *mf_to_string(mix_fd *fd) 
{
  mix_info *md;
  char *desc;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if ((fd == NULL) || (fd->sfs == NULL) || (fd->sfs[0] == NULL))
    sprintf(desc, "#<mix-sample-reader: null>");
  else
    {
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

XEN_MAKE_OBJECT_PRINT_PROCEDURE(mix_fd, print_mf, mf_to_string)

static mix_fd **dangling_mix_readers = NULL;
static int dangling_mix_reader_size = 0;
#define DANGLING_MIX_READER_INCREMENT 16

static void list_mix_reader(mix_fd *fd)
{
  int i, loc = -1;
  if (dangling_mix_reader_size == 0)
    {
      dangling_mix_reader_size = DANGLING_MIX_READER_INCREMENT;
      dangling_mix_readers = (mix_fd **)CALLOC(dangling_mix_reader_size, sizeof(mix_fd *));
      loc = 0;
    }
  else
    {
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
  fd->dangling_loc = loc;
  dangling_mix_readers[loc] = fd;
}

static void unlist_mix_reader(mix_fd *fd)
{
  if ((fd) && (fd->dangling_loc >= 0))
    {
      dangling_mix_readers[fd->dangling_loc] = NULL;
      fd->dangling_loc = -1;
    }
}

static void mf_free(mix_fd *fd)
{
  if (fd) 
    {
      unlist_mix_reader(fd);
      free_mix_fd(fd); 
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(mix_fd, free_mf, mf_free)

static void release_dangling_mix_readers(mix_info *md)
{
  int i;
  mix_fd *fd;
  for (i = 0; i < dangling_mix_reader_size; i++)
    {
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
}

#if DEBUGGING
void report_dangling_mix_readers(FILE *fp)
{
  int i;
  bool titled = false;
  for (i = 0; i < dangling_mix_reader_size; i++)
    if (dangling_mix_readers[i])
      {
	mix_fd *sf;
	sf = dangling_mix_readers[i];
	if (!titled)
	  {
	    fprintf(fp, "\nDangling mix_fd:\n");
	    fprintf(stderr, "\nDangling mix_fd:\n");
	    titled = true;
	  }
	fprintf(fp, "  %p, md: %p, type: %d\n",	sf, sf->md, sf->type);
	fprintf(stderr, "  %p, md: %p, type: %d\n",	sf, sf->md, sf->type);
      }
}

static void check_dangling_mix_readers(mix_fd *md)
{
  int i;
  if (md)
    {
      for (i = 0; i < dangling_mix_reader_size; i++)
	if (dangling_mix_readers[i] == md)
	  {
	    fprintf(stderr, "lost mix reader: %p\n", md);
	    abort();
	  }
    }
}
#endif

static XEN g_make_mix_sample_reader(XEN mix_id, XEN ubeg)
{
  /* TODO: this is different from make-sample-reader (beg snd) not (snd beg) */
  #define H_make_mix_sample_reader "(" S_make_mix_sample_reader " id (beg 0)): return a reader ready to access mix id"
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

static XEN g_next_mix_sample(XEN obj)
{
  #define H_next_mix_sample "(" S_next_mix_sample " reader): next sample from mix reader"
  XEN_ASSERT_TYPE(MIX_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_next_mix_sample, "a mix-sample-reader");
  return(C_TO_XEN_DOUBLE(next_mix_sample(TO_MIX_SAMPLE_READER(obj))));
}

static XEN g_read_mix_sample(XEN obj)
{
  #define H_read_mix_sample "(" S_read_mix_sample " reader): read sample from mix reader"
  XEN_ASSERT_TYPE(MIX_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_mix_sample, "a mix-sample-reader");
  return(C_TO_XEN_DOUBLE(next_mix_sample(TO_MIX_SAMPLE_READER(obj))));
}

static XEN g_free_mix_sample_reader(XEN obj)
{
  #define H_free_mix_sample_reader "(" S_free_mix_sample_reader " reader): free mix sample reader"
  mix_fd *mf;
  XEN_ASSERT_TYPE(MIX_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_free_mix_sample_reader, "a mix-sample-reader");
  mf = TO_MIX_SAMPLE_READER(obj);
  free_mix_fd_almost(mf);
  return(xen_return_first(XEN_FALSE, obj));
}

static XEN g_play_mix(XEN num, XEN beg)
{
  #define H_play_mix "(" S_play_mix " id (beg 0)): play mix.  'beg' is where to start playing."
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

static XEN mix_vct(XEN obj, XEN beg, XEN snd, XEN chn, XEN with_tag, XEN origin, XEN track_id)
{
  #define H_mix_vct "(" S_mix_vct " data (beg 0) (snd #f) (chn #f) (with-tag " S_with_mix_tags ") (origin #f) (track-id 0)): \
mix data (a vct) into snd's channel chn starting at beg; return the new mix id"

  vct *v;
  off_t bg;
  chan_info *cp;
  char *edname = NULL, *newname = NULL;
  snd_fd *sf;
  mus_sample_t *data;
  int i, len, mix_id = -1, track_num = 0;
  bool with_mixer = true;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_mix_vct, "a vct");
  ASSERT_CHANNEL(S_mix_vct, snd, chn, 3);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg), beg, XEN_ARG_2, S_mix_vct, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(with_tag), with_tag, XEN_ARG_5, S_mix_vct, "a boolean");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(track_id), track_id, XEN_ARG_8, S_mix_vct, "a track id");
  v = TO_VCT(obj);
  len = v->length;
  cp = get_cp(snd, chn, S_mix_vct);
  bg = beg_to_sample(beg, S_mix_vct);
  if (XEN_NOT_BOUND_P(with_tag))
    with_mixer = with_mix_tags(ss);
  else with_mixer = XEN_TO_C_BOOLEAN_OR_TRUE(with_tag);
  data = (mus_sample_t *)CALLOC(len, sizeof(mus_sample_t));
  for (i = 0; i < len; i++)
    data[i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
  if (XEN_STRING_P(origin))
    edname = XEN_TO_C_STRING(origin);
  if ((len < MAX_BUFFER_SIZE) && (!with_mixer))
    {
      sf = init_sample_read(bg, cp, READ_FORWARD);
      for (i = 0; i < len; i++)
	data[i] += read_sample(sf);
      free_snd_fd(sf);
      change_samples(bg, len, data, cp, LOCK_MIXES, S_mix_vct, cp->edit_ctr);
    }
  else
    {
      if (XEN_INTEGER_P(track_id))
	{
	  track_num = XEN_TO_C_INT(track_id);
	  if ((track_num > 0) && (!(track_p(track_num))))
	    {
	      FREE(data);
	      XEN_ERROR(NO_SUCH_TRACK,
			XEN_LIST_2(C_TO_XEN_STRING(S_mix_vct),
				   C_TO_XEN_INT(track_id)));
	    }
	}
      newname = save_as_temp_file(&data, 1, len, SND_SRATE(cp->sound));
      mix_id = mix_file(bg, len, 1, &cp, newname, DELETE_ME, (char *)((edname == NULL) ? S_mix_vct : edname), with_mixer, track_num);
      if (!with_mixer) snd_remove(newname, REMOVE_FROM_CACHE);
      FREE(newname);
    }
  update_graph(cp);
  FREE(data);
  return(xen_return_first(C_TO_XEN_INT(mix_id), obj));
}

static XEN g_set_mix_color (XEN arg1, XEN arg2)
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
  else set_mix_color(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}

static XEN g_mix_color(XEN mix_id) 
{
  #define H_mix_color "(" S_mix_color "): color of mix tags"
  if (XEN_INTEGER_P(mix_id))
    return(XEN_WRAP_PIXEL(mix_to_color_from_id(XEN_TO_C_INT(mix_id))));
  return(XEN_WRAP_PIXEL((ss->sgx)->mix_color));
}

static XEN g_with_mix_tags(void) {return(C_TO_XEN_BOOLEAN(with_mix_tags(ss)));}
static XEN g_set_with_mix_tags(XEN val) 
{
  #define H_with_mix_tags "(" S_with_mix_tags "): #t if Snd should display mixed portions with a draggable tag"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_mix_tags, "a boolean");
  set_with_mix_tags(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_mix_tags(ss)));
}

static XEN g_forward_mix(XEN count, XEN snd, XEN chn) 
{
  #define H_forward_mix "(" S_forward_mix " (count 1) (snd #f) (chn #f)): move the cursor forward count mixes, returns mix id if any"
  int val;
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(count), count, XEN_ARG_1, S_forward_mix, "an integer");
  ASSERT_CHANNEL(S_forward_mix, snd, chn, 2);
  cp = get_cp(snd, chn, S_forward_mix);
  val = XEN_TO_C_INT_OR_ELSE(count, 1); 
  goto_mix(cp, val);
  return(C_TO_XEN_INT(mix_id_from_channel_position(cp, CURSOR(cp))));
}

static XEN g_backward_mix(XEN count, XEN snd, XEN chn) 
{
  #define H_backward_mix "(" S_backward_mix " (count 1) (snd #f) (chn #f)): move the cursor back count mixes, returns mix id if any"
  int val; 
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(count), count, XEN_ARG_1, S_backward_mix, "an integer");
  ASSERT_CHANNEL(S_backward_mix, snd, chn, 2);
  cp = get_cp(snd, chn, S_backward_mix);
  val = -(XEN_TO_C_INT_OR_ELSE(count, 1)); 
  goto_mix(cp, val);
  return(C_TO_XEN_INT(mix_id_from_channel_position(cp, CURSOR(cp))));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_make_mix_sample_reader_w, g_make_mix_sample_reader)
XEN_NARGIFY_1(g_read_mix_sample_w, g_read_mix_sample)
XEN_NARGIFY_1(g_next_mix_sample_w, g_next_mix_sample)
XEN_NARGIFY_1(g_free_mix_sample_reader_w, g_free_mix_sample_reader)
XEN_NARGIFY_1(g_mf_p_w, g_mf_p)
XEN_ARGIFY_2(g_play_mix_w, g_play_mix)
XEN_ARGIFY_1(g_mix_position_w, g_mix_position)
XEN_NARGIFY_2(g_set_mix_position_w, g_set_mix_position)
XEN_ARGIFY_1(g_mix_frames_w, g_mix_frames)
XEN_ARGIFY_1(g_mix_locked_w, g_mix_locked)
XEN_NARGIFY_2(g_set_mix_locked_w, g_set_mix_locked)
XEN_ARGIFY_1(g_mix_anchor_w, g_mix_anchor)
XEN_NARGIFY_2(g_set_mix_anchor_w, g_set_mix_anchor)
XEN_ARGIFY_1(g_mix_track_w, g_mix_track)
XEN_NARGIFY_2(g_set_mix_track_w, g_set_mix_track)
XEN_ARGIFY_1(g_mix_tag_y_w, g_mix_tag_y)
XEN_NARGIFY_2(g_set_mix_tag_y_w, g_set_mix_tag_y)
XEN_ARGIFY_1(g_mix_speed_w, g_mix_speed)
XEN_NARGIFY_2(g_set_mix_speed_w, g_set_mix_speed)
XEN_NARGIFY_0(g_mix_waveform_height_w, g_mix_waveform_height)
XEN_NARGIFY_1(g_set_mix_waveform_height_w, g_set_mix_waveform_height)
XEN_NARGIFY_1(g_mix_tag_position_w, g_mix_tag_position)
XEN_NARGIFY_0(g_mix_tag_width_w, g_mix_tag_width)
XEN_NARGIFY_1(g_set_mix_tag_width_w, g_set_mix_tag_width)
XEN_NARGIFY_0(g_mix_tag_height_w, g_mix_tag_height)
XEN_NARGIFY_1(g_set_mix_tag_height_w, g_set_mix_tag_height)
XEN_ARGIFY_2(g_mix_amp_w, g_mix_amp)
XEN_ARGIFY_3(g_set_mix_amp_w, g_set_mix_amp)
XEN_ARGIFY_2(g_mix_amp_env_w, g_mix_amp_env)
XEN_ARGIFY_3(g_set_mix_amp_env_w, g_set_mix_amp_env)
XEN_ARGIFY_1(g_mix_chans_w, g_mix_chans)
XEN_ARGIFY_1(g_mix_p_w, g_mix_p)
XEN_ARGIFY_1(g_mix_home_w, g_mix_home)
XEN_ARGIFY_2(g_mixes_w, g_mixes)
XEN_ARGIFY_8(g_mix_file_w, g_mix_file)
XEN_ARGIFY_7(mix_vct_w, mix_vct)
XEN_ARGIFY_1(g_mix_color_w, g_mix_color)
XEN_ARGIFY_2(g_set_mix_color_w, g_set_mix_color)
XEN_NARGIFY_0(g_with_mix_tags_w, g_with_mix_tags)
XEN_NARGIFY_1(g_set_with_mix_tags_w, g_set_with_mix_tags)
XEN_NARGIFY_1(g_delete_mix_w, g_delete_mix)
XEN_ARGIFY_3(g_forward_mix_w, g_forward_mix)
XEN_ARGIFY_3(g_backward_mix_w, g_backward_mix)
#else
#define g_make_mix_sample_reader_w g_make_mix_sample_reader
#define g_next_mix_sample_w g_next_mix_sample
#define g_read_mix_sample_w g_read_mix_sample
#define g_free_mix_sample_reader_w g_free_mix_sample_reader
#define g_mf_p_w g_mf_p
#define g_play_mix_w g_play_mix
#define g_mix_position_w g_mix_position
#define g_set_mix_position_w g_set_mix_position
#define g_mix_frames_w g_mix_frames
#define g_mix_locked_w g_mix_locked
#define g_set_mix_locked_w g_set_mix_locked
#define g_mix_anchor_w g_mix_anchor
#define g_set_mix_anchor_w g_set_mix_anchor
#define g_mix_track_w g_mix_track
#define g_set_mix_track_w g_set_mix_track
#define g_mix_tag_y_w g_mix_tag_y
#define g_set_mix_tag_y_w g_set_mix_tag_y
#define g_mix_speed_w g_mix_speed
#define g_set_mix_speed_w g_set_mix_speed
#define g_mix_waveform_height_w g_mix_waveform_height
#define g_set_mix_waveform_height_w g_set_mix_waveform_height
#define g_mix_tag_position_w g_mix_tag_position
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
#define g_mix_file_w g_mix_file
#define mix_vct_w mix_vct
#define g_mix_color_w g_mix_color
#define g_set_mix_color_w g_set_mix_color
#define g_with_mix_tags_w g_with_mix_tags
#define g_set_with_mix_tags_w g_set_with_mix_tags
#define g_delete_mix_w g_delete_mix
#define g_forward_mix_w g_forward_mix
#define g_backward_mix_w g_backward_mix
#endif

void g_init_mix(void)
{
  mf_tag = XEN_MAKE_OBJECT_TYPE("MixSampleReader", sizeof(mix_fd));

#if HAVE_GUILE
  scm_set_smob_print(mf_tag, print_mf);
  scm_set_smob_free(mf_tag, free_mf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(mf_tag, XEN_PROCEDURE_CAST g_read_mix_sample, 0, 0, 0);
#endif
#endif
#if HAVE_RUBY
  rb_define_method(mf_tag, "to_s", XEN_PROCEDURE_CAST print_mf, 0);
#endif

  XEN_DEFINE_PROCEDURE(S_make_mix_sample_reader, g_make_mix_sample_reader_w, 1, 1, 0, H_make_mix_sample_reader);
  XEN_DEFINE_PROCEDURE(S_next_mix_sample,        g_next_mix_sample_w, 1, 0, 0,        H_next_mix_sample);
  XEN_DEFINE_PROCEDURE(S_read_mix_sample,        g_read_mix_sample_w, 1, 0, 0,        H_read_mix_sample);
  XEN_DEFINE_PROCEDURE(S_free_mix_sample_reader, g_free_mix_sample_reader_w, 1, 0, 0, H_free_mix_sample_reader);
  XEN_DEFINE_PROCEDURE(S_mix_sample_reader_p,    g_mf_p_w, 1, 0, 0,                   H_mf_p);

  XEN_DEFINE_PROCEDURE(S_play_mix,               g_play_mix_w, 0, 2, 0,               H_play_mix);
  XEN_DEFINE_PROCEDURE(S_delete_mix,             g_delete_mix_w, 1, 0, 0,             H_delete_mix);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_position, g_mix_position_w, H_mix_position, S_setB S_mix_position, g_set_mix_position_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE(S_mix_frames, g_mix_frames_w, 0, 1, 0, H_mix_frames);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_locked_p, g_mix_locked_w, H_mix_locked_p, S_setB S_mix_locked_p, g_set_mix_locked_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_anchor, g_mix_anchor_w, H_mix_anchor, S_setB S_mix_anchor, g_set_mix_anchor_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_track, g_mix_track_w, H_mix_track, S_setB S_mix_track, g_set_mix_track_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_y, g_mix_tag_y_w, H_mix_tag_y, S_setB S_mix_tag_y, g_set_mix_tag_y_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_speed, g_mix_speed_w, H_mix_speed, S_setB S_mix_speed, g_set_mix_speed_w, 0, 1, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_waveform_height, g_mix_waveform_height_w, H_mix_waveform_height,
				   S_setB S_mix_waveform_height, g_set_mix_waveform_height_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_width, g_mix_tag_width_w, H_mix_tag_width,
				   S_setB S_mix_tag_width, g_set_mix_tag_width_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_height, g_mix_tag_height_w, H_mix_tag_height,
				   S_setB S_mix_tag_height, g_set_mix_tag_height_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_color, g_mix_color_w, H_mix_color,
				   S_setB S_mix_color, g_set_mix_color_w,  0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_amp, g_mix_amp_w, H_mix_amp, S_setB S_mix_amp, g_set_mix_amp_w, 0, 2, 2, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_amp_env, g_mix_amp_env_w, H_mix_amp_env, S_setB S_mix_amp_env, g_set_mix_amp_env_w, 0, 2, 2, 1);

  XEN_DEFINE_PROCEDURE(S_mix_tag_position, g_mix_tag_position_w, 1, 0, 0, H_mix_tag_position);

  XEN_DEFINE_PROCEDURE(S_mix_chans,    g_mix_chans_w, 0, 1, 0,    H_mix_chans);
  XEN_DEFINE_PROCEDURE(S_mix_p,        g_mix_p_w, 0, 1, 0,        H_mix_p);
  XEN_DEFINE_PROCEDURE(S_mix_home,     g_mix_home_w, 0, 1, 0,     H_mix_home);
  XEN_DEFINE_PROCEDURE(S_mixes,        g_mixes_w, 0, 2, 0,        H_mixes);
  XEN_DEFINE_PROCEDURE(S_mix,          g_mix_file_w, 1, 7, 0,     H_mix_file);
  XEN_DEFINE_PROCEDURE(S_mix_vct,      mix_vct_w, 1, 6, 0,        H_mix_vct);
  XEN_DEFINE_PROCEDURE(S_forward_mix,  g_forward_mix_w, 0, 3, 0,  H_forward_mix);
  XEN_DEFINE_PROCEDURE(S_backward_mix, g_backward_mix_w, 0, 3, 0, H_backward_mix);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_mix_tags, g_with_mix_tags_w, H_with_mix_tags,
				   S_setB S_with_mix_tags, g_set_with_mix_tags_w,  0, 0, 1, 0);

  #define H_mix_release_hook S_mix_release_hook " (mix-id samps): called after the mouse has dragged a mix to some new position. \
'samps' = samples moved in the course of the drag. If it returns #t, the actual remix is the hook's responsibility."

  XEN_DEFINE_HOOK(mix_release_hook, S_mix_release_hook, 2, H_mix_release_hook);

  #define H_mix_drag_hook S_mix_drag_hook " (id): called when a mix is dragged"

  XEN_DEFINE_HOOK(mix_drag_hook, S_mix_drag_hook, 1, H_mix_drag_hook); /* arg = id */
}


/* ---------------- TRACKS ---------------- */

/* track reader: an array of mix readers with state: active, waiting, null (done) */
typedef struct {
  int mixes, track;
  off_t *state;
  off_t *len;
  mix_fd **fds;
} track_fd;

static track_fd *init_track_reader(chan_info *cp, int track_num, bool global, off_t beg) /* edit-position? direction? */
{
  /* beg is offset within track which starts within overall sound(s) at track_beg */
  track_fd *fd = NULL;
  int mixes = 0, i, mix;
  off_t track_beg;
  mix_info *md;
  mix_state *cs;
  track_beg = CURRENT_SAMPLES(cp);
  for (i = 0; i < mix_infos_ctr; i++) 
    if (mix_ok(i))
      {
	md = mix_infos[i];
	if (md->track == track_num)
	  {
	    if (md->cp == cp) mixes++;
	    cs = md->active_mix_state;
	    if ((global) || (md->cp == cp))
	      if (cs->orig < track_beg) 
		track_beg = cs->orig;
	  }
      }
  if (mixes > 0)
    {
      fd = (track_fd *)CALLOC(1, sizeof(track_fd));
      fd->track = track_num;
      fd->mixes = mixes;
      fd->state = (off_t *)CALLOC(mixes, sizeof(off_t));
      fd->len = (off_t *)CALLOC(mixes, sizeof(off_t));
      fd->fds = (mix_fd **)CALLOC(mixes, sizeof(mix_fd *));
      mix = 0;
      for (i = 0; i < mix_infos_ctr; i++)
	if (mix_ok(i))
	  {
	    md = mix_infos[i];
	    if ((md->track == track_num) && 
		(md->cp == cp))
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
		mix++;
	      }
	  }
    }
  return(fd);
}

static void free_track_fd_almost(track_fd *fd)
{
  int i;
  if (fd)
    {
      if (fd->fds)
	{
	  for (i = 0; i < fd->mixes; i++)
	    if (fd->fds[i]) 
	      fd->fds[i] = free_mix_fd(fd->fds[i]);
	  FREE(fd->fds);
	  fd->fds = NULL;
	}
      fd->mixes = 0;
      if (fd->state) {FREE(fd->state); fd->state = NULL;}
      if (fd->len) {FREE(fd->len); fd->len = NULL;}
    }
}

static void free_track_fd(track_fd *fd)
{
  if (fd)
    {
      free_track_fd_almost(fd);
      FREE(fd);
    }
}

static Float next_track_sample(track_fd *fd)
{
  int i;
  Float sum = 0.0;
  if (fd)
    for (i = 0; i < fd->mixes; i++)
      if ((fd->fds[i]) && 
	  (fd->len[i] > 0))
	{
	  if (fd->state[i] <= 0)
	    {
	      sum += next_mix_sample(fd->fds[i]);
	      fd->len[i]--;
	      if (fd->len[i] <= 0) 
		{
		  unlist_mix_reader(fd->fds[i]);
		  fd->fds[i] = free_mix_fd(fd->fds[i]);
		}
	    }
	  else fd->state[i]--;
	}
  return(sum);
}

/* TODO: play-track can use mix-lists? */

static int play_track(chan_info **ucps, int chans, int track_num, off_t beg, bool from_gui)
{
  track_fd **fds;
  chan_info **cps;
  snd_info *sp;
  chan_info *locp;
  int playfd, i, j, k, n, samps, chan = 0, format, datum_bytes, outchans, frames;
  bool happy = false, need_free = false;
#if HAVE_ALSA
  mus_sample_t **buf;
  char *outbuf;
  float val[4]; /* not Float */
#else
  OutSample *buf;
#endif
  if (ucps == NULL)
    {
      chans = active_channels(WITH_VIRTUAL_CHANNELS);
      if (chans == 0) return(-1);
      cps = (chan_info **)CALLOC(chans, sizeof(chan_info *));
      need_free = true;
      chan = 0;
      for (i = 0; i < mix_infos_ctr; i++) 
	if ((mix_ok(i)) && 
	    (mix_infos[i]->track == track_num))
	  {
	    locp = mix_infos[i]->cp;
	    happy = false;
	    for (j = 0; j < chan; j++) 
	      if (cps[j] == locp) 
		{
		  happy = true; 
		  break;
		}
	    if (!happy)
	      {
		cps[chan] = locp;
		chan++;
	      }
	  }
      chans = chan;
    }
  else cps = ucps;
  samps = 0;
  if (chans == 0) return(-1);
  outchans = chans;
  format = mus_audio_compatible_format(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss));
  datum_bytes = mus_bytes_per_sample(format);
  frames = 256;
  fds = (track_fd **)CALLOC(chans, sizeof(track_fd *));

#if HAVE_ALSA
  /* in ALSA we have no way to tell what the possible output format is, or min chans, so... */
  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_CHANNEL, 2, val);
  if (chans < (int)(val[1])) outchans = (int)(val[1]);
  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, val);
  frames = (int)(val[0]);
  set_dac_size(outchans * frames * mus_bytes_per_sample(format));
  buf = (mus_sample_t **)CALLOC(outchans, sizeof(mus_sample_t *));
  for (i = 0; i < chans; i++) buf[i] = (mus_sample_t *)CALLOC(frames, sizeof(mus_sample_t));
  outbuf = (char *)CALLOC(frames * datum_bytes * outchans, sizeof(char));
#else
  buf = (OutSample *)CALLOC(chans * frames, sizeof(OutSample));
#endif
  for (i = 0; i < chans; i++)
    {
      fds[i] = init_track_reader(cps[i], track_num, need_free, beg);
      if (fds[i]) /* perhaps bad track number? */
	for (n = 0; n < fds[i]->mixes; n++)
	  {
	    j = fds[i]->state[n] + fds[i]->len[n];
	    if (j > samps) samps = j;
	  }
    }
  if (samps > 0)
    {
      sp = cps[0]->sound;
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
	      if ((ss->stopped_explicitly) || ((from_gui) && (mix_play_stopped())))
		{
		  ss->stopped_explicitly = false;
		  report_in_minibuffer(sp, _("stopped"));
		  break;
		}
	    }
	  mus_audio_close(playfd);
	}
    }
  for (i = 0; i < chans; i++) free_track_fd(fds[i]);
  reflect_mix_play_stop();
  FREE(fds);
#if HAVE_ALSA
  for (i = 0; i < chans; i++) if (buf[i]) FREE(buf[i]);
  FREE(outbuf);
#endif
  FREE(buf);
  if (need_free) FREE(cps);
  return(0);
}

void track_play_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md->track != 0)
    play_track(NULL, 0, md->track, 0, true);
  else play_mix(md, 0, true);
}

/* ---------------- track sample readers ---------------- */

static XEN_OBJECT_TYPE tf_tag;
static bool tf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, tf_tag));}
#define TO_TRACK_SAMPLE_READER(obj) ((track_fd *)XEN_OBJECT_REF(obj))
#define TRACK_SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, tf_tag)

static XEN g_tf_p(XEN obj) 
{
  #define H_tf_p "(" S_track_sample_reader_p " obj): #t if obj is a track-sample-reader"
  return(C_TO_XEN_BOOLEAN(tf_p(obj)));
}

static char *tf_to_string(track_fd *fd) 
{
  mix_info *md;
  mix_fd *mf = NULL;
  char *desc;
  char toi[16];
  int i, len, desc_len;
  bool happy = false, banner = false, previous = false;
  desc_len = PRINT_BUFFER_SIZE;
  desc = (char *)CALLOC(desc_len, sizeof(char));
  if (fd == NULL)
    sprintf(desc, "#<track-sample-reader: null>");
  else
    {
      if ((fd->fds) && (fd->mixes > 0))
	{
	  len = fd->mixes;
	  for (i = 0; i < len; i++)
	    {
	      mf = fd->fds[i];
	      if ((mf) && (mf->md) && (MIX_TYPE_OK(mf->type)))
		{
		  md = mf->md;
		  happy = true;
		  if (!banner)
		    {

		      mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<track-sample-reader track %d: %s chan %d via mixes '(",
				   fd->track,
				   md->in_filename,
				   (md->cp)->chan);
		      banner = true;
		    }
		  mus_snprintf(toi, 16, "%s%d", (previous) ? " ": "", (mf->md)->id);
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

XEN_MAKE_OBJECT_PRINT_PROCEDURE(track_fd, print_tf, tf_to_string)

static void tf_free(track_fd *fd)
{
  int i;
  if (fd) 
    {
      if (fd->fds)
	{
	  for (i = 0; i < fd->mixes; i++)
	    if (fd->fds[i]) 
	      unlist_mix_reader(fd->fds[i]);
	}
      free_track_fd(fd); 
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(track_fd, free_tf, tf_free)

  /* TODO: play-track all chans */

static XEN g_make_track_sample_reader(XEN track_id, XEN snd, XEN chn, XEN beg)
{
  #define H_make_track_sample_reader "(" S_make_track_sample_reader " track (snd #f) (chn #f) (beg 0)): \
return a reader ready to access track's data associated with snd's channel chn, starting in the track from beg"

  track_fd *tf = NULL;
  chan_info *cp;
  int i;
  off_t samp;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(track_id), track_id, XEN_ARG_1, S_make_track_sample_reader, "an integer");
  ASSERT_CHANNEL(S_make_track_sample_reader, snd, chn, 2); 
  cp = get_cp(snd, chn, S_make_track_sample_reader);
  ASSERT_SAMPLE_TYPE(S_make_track_sample_reader, beg, XEN_ARG_4);
  samp = beg_to_sample(beg, S_make_track_sample_reader);
  tf = init_track_reader(cp, 
			 XEN_TO_C_INT(track_id), 
			 false, /* true to track all chans in parallel (assuming different starting points) */
			 samp); 
  if (tf == NULL)
    XEN_ERROR(NO_SUCH_TRACK,
	      XEN_LIST_2(C_TO_XEN_STRING(S_make_track_sample_reader),
			 track_id));
  if (tf->fds)
    {
      for (i = 0; i < tf->mixes; i++)
	if (tf->fds[i]) 
	  list_mix_reader(tf->fds[i]);
    }
  XEN_MAKE_AND_RETURN_OBJECT(tf_tag, tf, 0, free_tf);
}

static XEN g_next_track_sample(XEN obj)
{
  #define H_next_track_sample "(" S_next_track_sample " reader): next sample from track reader"
  XEN_ASSERT_TYPE(TRACK_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_next_track_sample, "a track-sample-reader");
  return(C_TO_XEN_DOUBLE(next_track_sample(TO_TRACK_SAMPLE_READER(obj))));
}

static XEN g_read_track_sample(XEN obj)
{
  #define H_read_track_sample "(" S_read_track_sample " reader): read sample from track reader"
  XEN_ASSERT_TYPE(TRACK_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_track_sample, "a track-sample-reader");
  return(C_TO_XEN_DOUBLE(next_track_sample(TO_TRACK_SAMPLE_READER(obj))));
}

static XEN g_free_track_sample_reader(XEN obj)
{
  #define H_free_track_sample_reader "(" S_free_track_sample_reader " reader): free the track sample reader"
  track_fd *tf = NULL;
  int i;
  XEN_ASSERT_TYPE(TRACK_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_free_track_sample_reader, "a track-sample-reader");
  tf = TO_TRACK_SAMPLE_READER(obj);
  if ((tf) && (tf->fds))
    {
      for (i = 0; i < tf->mixes; i++)
	if (tf->fds[i]) 
	  unlist_mix_reader(tf->fds[i]);
    }
  free_track_fd_almost(tf);
  return(xen_return_first(XEN_FALSE, obj));
}


/* -------- track handlers -------- */

typedef struct {
  Float amp, speed;
  env *amp_env;
  int track;
  color_t color;
} track_state;

typedef struct {
  int size, loc;
  track_state **states;
} track_list;
  
static track_state *free_track_state(track_state *ts);

static track_list **tracks;
static int tracks_size = 0;
static int track_ctr = 0; /* 0 is the no-track indication */

#define INVALID_TRACK -1

static int plausible_track(void)
{
  int i;
  for (i = 1; i < track_ctr; i++)
    if (track_p(i))
      return(i);
  return(INVALID_TRACK);
}

static int xen_to_c_track(XEN id, const char *origin)
{
  int track_id;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(id), id, XEN_ARG_1, origin, "an integer");
  if (XEN_INTEGER_P(id))
    track_id = XEN_TO_C_INT(id);
  else track_id = plausible_track();
  /* fprintf(stderr,"track: %d %d\n", track_id, XEN_TO_C_INT(id)); */
  if (!(track_p(track_id)))
    XEN_ERROR(NO_SUCH_TRACK,
	      XEN_LIST_2(C_TO_XEN_STRING(origin),
			 C_TO_XEN_INT(id)));
  return(track_id);
}

static int xen_to_c_track_no_default(XEN id, const char *origin)
{
  int track_id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ARG_1, origin, "an integer");
  track_id = XEN_TO_C_INT(id);
  if (!(track_p(track_id)))
    XEN_ERROR(NO_SUCH_TRACK,
	      XEN_LIST_2(C_TO_XEN_STRING(origin),
			 C_TO_XEN_INT(id)));
  return(track_id);
}

static int xen_to_c_track_no_default_zero_ok(XEN id, const char *origin)
{
  int track_id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ARG_1, origin, "an integer");
  track_id = XEN_TO_C_INT(id);
  if ((track_id != 0) && (!(track_p(track_id))))
    XEN_ERROR(NO_SUCH_TRACK,
	      XEN_LIST_2(C_TO_XEN_STRING(origin),
			 C_TO_XEN_INT(id)));
  return(track_id);
}

static XEN g_play_track(XEN id, XEN snd, XEN chn, XEN beg)
{
  #define H_play_track "(" S_play_track " track (snd #f) (chn #f) (beg 0)): play track. If 'snd' is #t, \
play all the mixes in the track, even if in different sounds.  'beg' is where to start playing within the track."
  chan_info *cp = NULL;
  int err, track_id;
  off_t samp;
  track_id = xen_to_c_track(id, S_play_track);
  /* in this case if snd=#t, play all associated mixes in all chans */
  ASSERT_SAMPLE_TYPE(S_play_track, beg, XEN_ARG_4);
  samp = beg_to_sample(beg, S_play_track);
  if (XEN_TRUE_P(snd))
    err = play_track(NULL, 0, track_id, samp, false);
  else 
    {
      cp = get_cp(snd, chn, S_play_track);
      err = play_track(&cp, 1, track_id, samp, false);
    }
  return(id);
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
	    if (cs->amp_envs[i]) free_env(cs->amp_envs[i]);
	  FREE(cs->amp_envs);
	  cs->amp_envs = NULL;
	}
      if (cs->scalers) FREE(cs->scalers);
      cs->scalers = NULL;
      FREE(cs);
    }
  return(NULL);
}

#if 0
bool track_p_1(int trk, char *caller)
#else
bool track_p(int trk)
#endif
{
  /* fprintf(stderr, "%s: track? %d %d %p %p\n", caller, trk, track_ctr, tracks, (tracks) ? tracks[trk] : 0); */
  return((trk > 0) &&
	 (trk < track_ctr) &&
	 (tracks[trk]));
}

static XEN g_track_p(XEN id)
{
  #define H_track_p "(" S_track_p " id) -> #t if id refers to an active track"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ONLY_ARG, S_track_p, "an integer");
  return(C_TO_XEN_BOOLEAN(track_p(XEN_TO_C_INT(id))));
}


static track_state *active_track_state(int trk)
{
  track_list *tl;
  if (track_p(trk))
    {
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

/* each field setting prunes all existing states in the list beyond loc,
 *   adds a new track_state to the list,
 *   copies the loc state (or makes initial?), sets field, increments loc
 */
static void record_track_info_given_track(int track_id);
static void record_track_info(chan_info *cp, int loc);

static track_state *extend_track_list(int trk)
{
  /* loc is ok, we'll be setting loc+1 */
  int i, new_loc = 0;
  track_list *tl;
  track_state *ts, *old_ts = NULL;
  if (tracks[trk] == NULL)
    {
      tracks[trk] = (track_list *)CALLOC(1, sizeof(track_list));
      tracks[trk]->size = 0;
    }
  else
    {
      /* fprintf(stderr,"extend so record info on %d\n", trk); */
      record_track_info_given_track(trk);
    }
  tl = tracks[trk];
  if (tl->states)
    {
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
  tl->states[new_loc] = (track_state *)CALLOC(1, sizeof(track_state));
  ts = tl->states[new_loc];
  tl->loc = new_loc;
  /* fprintf(stderr,"make new track state at: %d\n", new_loc); */
  if (old_ts)
    {
      ts->amp = old_ts->amp;
      ts->speed = old_ts->speed;
      ts->track = old_ts->track;
      ts->amp_env = copy_env(old_ts->amp_env);
      ts->color = old_ts->color;
    }
  else
    {
      ts->amp = 1.0;
      ts->speed = 1.0;
      ts->amp_env = NULL;
      ts->track = 0;
      ts->color = 0;
    }
  return(ts);
}

static int set_active_track_track(int id, int trk)
{
  track_state *ts;
  ts = extend_track_list(id);
  if (ts) ts->track = trk;
  return(trk);
}

static Float set_active_track_amp(int id, Float amp)
{
  track_state *ts;
  /* fprintf(stderr,"set active track amp\n"); */
  ts = extend_track_list(id);
  if (ts) ts->amp = amp;
  return(amp);
}

static Float set_active_track_speed(int id, Float speed)
{
  track_state *ts;
  ts = extend_track_list(id);
  if (ts) ts->speed = speed;
  return(speed);
}

static env *set_active_track_amp_env(int id, env *e)
{
  track_state *ts;
  ts = extend_track_list(id);
  if (ts->amp_env) free_env(ts->amp_env);
  if (ts) ts->amp_env = copy_env(e);
  return(e);
}

static color_t set_active_track_color(int id, color_t c)
{
  track_state *ts;
  ts = extend_track_list(id);
  if (ts) ts->color = c;
  return(c);
}


/* -------- track chain -------- */

typedef struct {
  int *lst; 
  int id, lst_size, lst_ctr;
  chan_info **cps;
  int cps_size, cps_ctr;
} track_mix_list_t;

static int gather_mixes(mix_info *md, void *ptr)
{
  track_mix_list_t *trk = (track_mix_list_t *)ptr;
  int tid, i;
  tid = md->track;
  /* fprintf(stderr,"gather mixes\n"); */
  while (track_p(tid))
    {
      if (tid == trk->id)
	{
	  if (trk->lst_size <= trk->lst_ctr)
	    {
	      trk->lst_size += 8;
	      trk->lst = (int *)REALLOC(trk->lst, trk->lst_size * sizeof(int));
	    }
	  trk->lst[trk->lst_ctr++] = md->id;
	  for (i = 0; i < trk->cps_ctr; i++)
	    if (md->cp == trk->cps[i])
	      return(0);
	  /* if we get here, the current channel pointer hasn't been listed */
	  if (trk->cps_size <= trk->cps_ctr)
	    {
	      trk->cps_size += 2;
	      trk->cps = (chan_info **)REALLOC(trk->cps, trk->cps_size * sizeof(chan_info *));
	    }
	  trk->cps[trk->cps_ctr++] = md->cp;
	  return(0);
	}
      tid = active_track_track(tid);
    }
  return(0);
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
  trk->id = track_id;
  map_over_active_mixes(gather_mixes, (void *)trk);
  return(trk);
}

static void free_track_mix_list(track_mix_list_t *trk)
{
  if (trk)
    {
      if (trk->lst) FREE(trk->lst);
      if (trk->cps) FREE(trk->cps);
      FREE(trk);
    }
}

#if 0
static void map_over_track_mixes(int track_id, void (*func)(mix_info *, void *), void *val)
{
  track_mix_list_t *trk;
  int i;
  trk = track_mixes(track_id);
  for (i = 0; i < trk->lst_ctr; i++)
    (*func)(md_from_id(trk->lst[i]), val);
  free_track_mix_list(trk);
}
#endif


static track_graph_t *track_save_graph(mix_info *orig_md)
{
  track_graph_t *tg = NULL;
  track_mix_list_t *trk;
  mix_info *md;
  mix_state *cs;
  chan_info *cp = NULL;
  int i, pts = 0, track_id;
  track_id = orig_md->track;
  trk = track_mixes(track_id);
  if (trk->lst_ctr > 0)
    {
      tg = (track_graph_t *)CALLOC(1, sizeof(track_graph_t));
      tg->xs = (int *)CALLOC(trk->lst_ctr, sizeof(int));
      tg->origs = (off_t *)CALLOC(trk->lst_ctr, sizeof(off_t));
      for (i = 0; i < trk->lst_ctr; i++)
	{
	  md = md_from_id(trk->lst[i]);
	  cs = md->active_mix_state;
	  tg->origs[i] = cs->orig;
	  tg->xs[i] = md->x;
	  if (md == orig_md) tg->orig = i;
	  if (cp != md->cp)
	    {
	      cp = md->cp;
	      pts = make_graph(cp);
	    }
	  mix_save_graph(md->wg, pts);
	}
    }
  free_track_mix_list(trk);
  return(tg);
}

static void move_track(int track_id, track_graph_t *data)
{
  track_mix_list_t *trk;
  int i, diff;
  mix_info *md;
  trk = track_mixes(track_id);
  diff = data->x - data->xs[data->orig];
  for (i = 0; i < trk->lst_ctr; i++)
    {
      md = md_from_id(trk->lst[i]);
      if (i == data->orig)
	md->x = data->x;
      else md->x = data->xs[i] + diff;
      move_mix(md);
    }
  free_track_mix_list(trk);
}

static void finish_dragging_track(int track_id, track_graph_t *data, bool remix)
{
  track_mix_list_t *trk;
  int i;
  mix_info *md;
  mix_context *ms;
  mix_state *cs;
  off_t change = 0;
  trk = track_mixes(track_id);
  for (i = 0; i < trk->lst_ctr; i++)
    {
      md = md_from_id(trk->lst[i]);
      ms = md->wg;
      ms->lastpj = 0;
      if (i == data->orig)
	{
	  /* TODO: finish drag still broken if drag past end */
	  md->x = data->x;
	  move_mix(md);          /* update to last beg */
	  cs = md->active_mix_state; 
	  change = cs->beg - cs->orig;
	}
      cs = md->active_mix_state;
      cs->orig = data->origs[i]; /* clear out temp drag-related changes */
      cs->beg = data->origs[i];
    }
  free_track_mix_list(trk);
  remix_track(track_id, set_track_position_1, (void *)(&change));
  /* redisplay_track(track_id); */
}

static track_graph_t *free_track_graph(track_graph_t *ptr)
{
  if (ptr)
    {
      if (ptr->origs) FREE(ptr->origs);
      if (ptr->xs) FREE(ptr->xs);
      FREE(ptr);
    }
  return(NULL);
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

static XEN g_track(XEN id)
{
  /* given track id, return list of constituent mixes */
  #define H_track "(" S_track " track-id) returns a list of the mixes contained in the given track"
  int i, track_id;
  XEN result;
  track_mix_list_t *trk;
  track_id = xen_to_c_track(id, S_track);
  trk = track_mixes(track_id);
  result = XEN_EMPTY_LIST;
  for (i = trk->lst_ctr - 1; i >= 0; i--)
    result = XEN_CONS(C_TO_XEN_INT(trk->lst[i]), result);
  free_track_mix_list(trk);
  return(result);
}

static void set_track_color(int id, color_t color)
{
  /* not considered an edit */
  int i;
  track_mix_list_t *trk;
  if ((track_p(id)) && (active_track_color(id) != color))
    {
      set_active_track_color(id, color);
      trk = track_mixes(id);
      for (i = 0; i < trk->lst_ctr; i++)
	color_one_mix_from_id(trk->lst[i], color); 
      for (i = 0; i < trk->cps_ctr; i++)
	update_graph(trk->cps[i]);
      free_track_mix_list(trk);
    }
}

static XEN g_track_color(XEN id)
{
  #define H_track_color "(" S_track_color " id) -> id's mix's mix-color"
  int track_id;
  track_id = xen_to_c_track(id, S_track_color);
  return(XEN_WRAP_PIXEL(active_track_color(track_id)));
}

static XEN g_set_track_color(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track_no_default(id, S_setB S_track_color);
  XEN_ASSERT_TYPE(XEN_PIXEL_P(val), val, XEN_ARG_2, S_setB S_track_color, "a color");
  set_track_color(track_id, XEN_UNWRAP_PIXEL(val));
  return(val);
}

static void remix_track(int id, void (*func)(mix_info *, void *), void *val)
{
  int i;
  int *edpos;
  chan_info *cp;
  track_mix_list_t *trk;
  trk = track_mixes(id);
  if (trk->cps_ctr > 0)
    {
      edpos = (int *)CALLOC(trk->cps_ctr, sizeof(int));
      for (i = 0; i < trk->cps_ctr; i++)
	edpos[i] = trk->cps[i]->edit_ctr + 1; /* prepare as_one_edit */
      for (i = 0; i < trk->lst_ctr; i++)      /* has to be one at a time to keep state stacks in sync */
	(*func)(md_from_id(trk->lst[i]), val);
      for (i = 0; i < trk->cps_ctr; i++)      /* remix is one edit */
	{
	  cp = trk->cps[i];
	  while (cp->edit_ctr > edpos[i]) backup_edit_list(cp);
	  backup_mix_list(cp, edpos[i]);
	}
      FREE(edpos);
      for (i = 0; i < trk->cps_ctr; i++)
	update_graph(trk->cps[i]);
    }
  free_track_mix_list(trk);
}

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

static void set_track_amp_1(mix_info *md, void *val)
{
  remix_file(md, S_setB S_track_amp, false);
}

static void set_track_amp(int id, Float amp)
{
  if ((track_p(id)) && (active_track_amp(id) != amp))
    {
      set_active_track_amp(id, amp);
      remix_track(id, set_track_amp_1, NULL);
    }
}

/* TODO: for mix-panel display, we could follow selected chan */

static XEN g_track_amp(XEN id)
{
  #define H_track_amp "(" S_track_amp " id) -> id's amp"
  int track_id;
  track_id = xen_to_c_track(id, S_track_amp);
  return(C_TO_XEN_DOUBLE(active_track_amp(track_id)));
}

static XEN g_set_track_amp(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track_no_default(id, S_setB S_track_amp);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_track_amp, "a number");
  set_track_amp(track_id, XEN_TO_C_DOUBLE(val));
  return(val);
}

static void set_track_speed_1(mix_info *md, void *val)
{
  remix_file(md, S_setB S_track_speed, false);
}

static void set_track_speed(int id, Float speed)
{
  if ((track_p(id)) && (active_track_speed(id) != speed))
    {
      set_active_track_speed(id, speed);
      remix_track(id, set_track_speed_1, (void *)(&speed));
    }
}

static XEN g_track_speed(XEN id)
{
  #define H_track_speed "(" S_track_speed " id) -> id's speed"
  int track_id;
  track_id = xen_to_c_track(id, S_track_speed);
  return(C_TO_XEN_DOUBLE(active_track_speed(track_id)));
}

static XEN g_set_track_speed(XEN id, XEN val)
{
  int track_id;
  track_id = xen_to_c_track_no_default(id, S_setB S_track_speed);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_setB S_track_speed, "a number");
  set_track_speed(track_id, XEN_TO_C_DOUBLE(val));
  return(val);
}

typedef struct {off_t pos; int track_id;} track_pos_t;

static int gather_track_position(mix_info *md, void *ptr)
{
  track_pos_t *pt = (track_pos_t *)ptr;
  mix_state *cs;
  /* fprintf(stderr,"gather track position\n"); */
  if (md->track == pt->track_id)
    {
      cs = md->active_mix_state;
      /* fprintf(stderr,"%d at " OFF_TD "\n",md->id, cs->beg); */
      if ((cs) && (cs->beg < pt->pos)) 
	pt->pos = cs->beg;
    }
  return(0);
}

static off_t track_position(int id)
{
  #define UNLIKELY_POSITION 1234567890
  track_pos_t *pt;
  off_t result;
  /* fprintf(stderr,"track_position"); */
  pt = (track_pos_t *)CALLOC(1, sizeof(track_pos_t));
  pt->pos = UNLIKELY_POSITION;
  pt->track_id = id;
  map_over_active_mixes(gather_track_position, (void *)pt);
  result = pt->pos;
  FREE(pt);
  if (result == UNLIKELY_POSITION)
    return(-1);
  return(result);
}

static XEN g_track_position(XEN id)
{
  #define H_track_position "(" S_track_position " id) -> id's position (location of first mixed sample)"
  int track_id;
  track_id = xen_to_c_track(id, S_track_position);
  return(C_TO_XEN_OFF_T(track_position(track_id)));
}

static void set_track_position_1(mix_info *md, void *val)
{
  mix_state *cs;
  off_t change;
  change = (*((off_t *)val));
  cs = md->active_mix_state;
  /* fprintf(stderr,"%d from " OFF_TD " to ",md->id, cs->beg); */
  cs->beg += change;
  /* fprintf(stderr,OFF_TD ", orig: " OFF_TD "\n", cs->beg, cs->orig); */
  if (cs->beg < 0) cs->beg = 0;
  remix_file(md, S_setB S_track_position, false);	 
}

static void set_track_position(int id, off_t pos)
{
  off_t curpos, change;
  curpos = track_position(id);
  if ((track_p(id)) && (curpos != pos))
    {
      change = pos - curpos;
      remix_track(id, set_track_position_1, (void *)(&change));
    }
}

static XEN g_set_track_position(XEN id, XEN pos)
{
  int track_id;
  track_id = xen_to_c_track_no_default(id, S_setB S_track_position);
  XEN_ASSERT_TYPE(XEN_OFF_T_P(pos), pos, XEN_ARG_2, S_setB S_track_position, "a sample number");
  set_track_position(track_id, XEN_TO_C_OFF_T(pos));
  return(pos);
}

static int gather_track_end(mix_info *md, void *ptr)
{
  track_pos_t *pt = (track_pos_t *)ptr;
  mix_state *cs;
  if ((md) && 
      (mix_ok_and_unlocked(md->id)) &&
      (md->track == pt->track_id))
    {
      cs = md->active_mix_state;
      if ((cs) && (cs->beg + cs->len) > pt->pos)
	pt->pos = cs->beg + cs->len;
    }
  return(0);
}

static off_t track_frames(int id)
{
  track_pos_t *pt;
  off_t curend, curpos;
  /* fprintf(stderr,"trac_frames"); */
  curpos = track_position(id);
  if (curpos == -1) return(0);
  pt = (track_pos_t *)CALLOC(1, sizeof(track_pos_t));
  pt->pos = -1;
  pt->track_id = id;
  map_over_active_mixes(gather_track_end, (void *)pt);
  curend = pt->pos;
  FREE(pt);
  if (curend == -1) /* no mixes found? */
    return(0);
  return(curend - curpos); /* no -1 in the end point above, so no +1 here */
}

static XEN g_track_frames(XEN id)
{
  #define H_track_frames "(" S_track_frames " id) -> id's length (samples)"
  int track_id;
  track_id = xen_to_c_track(id, S_track_frames);
  return(C_TO_XEN_OFF_T(track_frames(track_id)));
}

static XEN g_track_amp_env(XEN id) 
{
  #define H_track_amp_env "(" S_track_amp_env " id): amplitude envelope applied to all of track's mixes"
  int track_id;
  track_id = xen_to_c_track(id, S_track_amp_env);
  if (active_track_amp_env(track_id))
    return(env_to_xen(active_track_amp_env(track_id)));
  return(XEN_EMPTY_LIST);
}

static void set_track_amp_env_1(mix_info *md, void *val)
{
  remix_file(md, S_setB S_track_amp_env, false);
}

static void set_track_amp_env(int id, env *e)
{
  if (track_p(id))
    {
      if (active_track_amp_env(id)) free_env(active_track_amp_env(id));
      set_active_track_amp_env(id, e);
      remix_track(id, set_track_amp_env_1, NULL);
    }
}

static XEN g_set_track_amp_env(XEN id, XEN e)
{
  int track_id;
  track_id = xen_to_c_track_no_default(id, S_setB S_track_amp_env);
  XEN_ASSERT_TYPE(XEN_LIST_P(e) || XEN_FALSE_P(e), e, XEN_ARG_2, S_setB S_track_amp_env, "an envelope (a list of breakpoints) or #f");
  set_track_amp_env(track_id, xen_to_env(e));
  return(e);
}

static XEN g_track_track(XEN id)
{
  #define H_track_track "(" S_track_track " id) -> id's track (0 = none)"
  int track_id;
  track_id = xen_to_c_track(id, S_track_track);
  return(C_TO_XEN_INT(active_track_track(track_id)));
}

static void set_track_track_1(mix_info *md, void *ptr)
{
  remix_file(md, S_setB S_track_track, false);
}

static bool set_track_track(int id, int trk)
{
  if (active_track_track(id) != trk)
    {
      if (trk > 0)
	{
	  int tid;
	  tid = trk;
	  while (track_p(tid))
	    {
	      if (tid == id) return(false);
	      tid = active_track_track(tid);
	    }
	}
      set_active_track_track(id, trk);
      remix_track(id, set_track_track_1, NULL);
    }
  return(true);
}

static XEN g_set_track_track(XEN id, XEN trk)
{
  int track_id, its_track;
  track_id = xen_to_c_track_no_default(id, S_setB S_track_track);
  its_track = xen_to_c_track_no_default_zero_ok(trk, S_setB S_track_track);
  if (!(set_track_track(track_id, its_track)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_track_track, XEN_ARG_2, trk, "a track's track can't be a member of that track");
  return(trk);
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
  extend_track_list(id);
  /* fprintf(stderr,"new track %d\n", id); */
  return(id);
}

static void set_mix_track(mix_info *md, int trk, bool redisplay)
{
  if ((md->track != trk) && 
      ((trk == 0) || (track_p(trk))))
    {
      md->track = trk;
      remix_file(md, S_setB S_mix_track, redisplay); 
      /* or remix_track? -- track amp env? */
    }
}

static XEN g_make_track(XEN ids)
{
  #define H_make_track "(" S_make_track "mix-ids...) returns a new track containing the mixes passed as its argument"
  int track_id, mix_id, len;
  mix_info *md;
  XEN lst;
  int *edpos = NULL;
  chan_info **cps = NULL;
  for (lst = XEN_COPY_ARG(ids); XEN_NOT_NULL_P(lst); lst = XEN_CDR(lst))
    if ((!(XEN_INTEGER_P(XEN_CAR(lst)))) ||
	(!(mix_ok(XEN_TO_C_INT(XEN_CAR(lst))))))
      XEN_ERROR(NO_SUCH_MIX,
		XEN_LIST_3(C_TO_XEN_STRING(S_make_track),
			   XEN_CAR(lst),
			   ids));
  track_id = new_track();
  len = XEN_LIST_LENGTH(ids);
  if (len > 0)
    {
      bool got_that_one = false;
      int ctr = 0, i;
      edpos = (int *)CALLOC(len, sizeof(int));
      cps = (chan_info **)CALLOC(len, sizeof(chan_info *));
      for (lst = XEN_COPY_ARG(ids); XEN_NOT_NULL_P(lst); lst = XEN_CDR(lst))
	{
	  mix_id = XEN_TO_C_INT(XEN_CAR(lst));
	  md = md_from_id(mix_id);
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
	      edpos[ctr] = md->cp->edit_ctr + 1;
	      ctr++;
	    }
	  if (md->track == 0)
	    md->track = track_id; /* no need for mix fixups since it's a brand-new track and the mix had none previously */
	  else
	    {
	      if (md->track != track_id)
		set_mix_track(md, track_id, false);
	    }
	}
      /* now make sure this looks like one edit operation */
      for (i = 0; i < ctr; i++)
	{
	  chan_info *cp;
	  cp = cps[i];
	  while (cp->edit_ctr > edpos[i]) backup_edit_list(cp);
	  update_graph(cp);
	}
      if (edpos) FREE(edpos);
      if (cps) FREE(cps);
    }
  return(C_TO_XEN_INT(track_id));
}

static int unset_track(mix_info *md, void *ptr)
{
  int id = (int)ptr;
  if (md->track == id)
    set_mix_track(md, 0, true); /* redisplay here? mix panel? */
  return(0);
}

static track_state *free_track_state(track_state *ts)
{
  if (ts)
    {
      if (ts->amp_env) ts->amp_env = free_env(ts->amp_env);
      FREE(ts);
    }
  return(NULL);
}

static void free_track_list(int id)
{
  int i;
  track_list *tl;
  tl = tracks[id];
  if (tl)
    {
      map_over_mixes(unset_track, (void *)id);
      if (tl->states)
	{
	  for (i = 0; i < tl->size; i++)
	    if (tl->states[i])
	      tl->states[i] = free_track_state(tl->states[i]);
	  FREE(tl->states);
	  tl->states = NULL;
	}
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

static Float gather_track_amp(int id)
{
  Float amp = 1.0;
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

static env *gather_track_amp_env(int id, off_t mix_beg, off_t mix_dur)
{
  env *e = NULL;
  off_t track_beg, track_dur;
  while (track_p(id))
    {
      /* fprintf(stderr,"track_amp_env"); */
      track_beg = track_position(id);
      track_dur = track_frames(id);
      if (active_track_amp_env(id))
	{
	  /* track amps can be left until later (gather track amp) */
	  if (!e) 
	    e = window_env(active_track_amp_env(id), mix_beg, mix_dur, track_beg, track_dur);
	  else e = multiply_envs(e, 
				 window_env(active_track_amp_env(id), mix_beg, mix_dur, track_beg, track_dur),
				 MULTIPLY_ENVS_INCR);
	  /*
	  fprintf(stderr,"track: %s\n  env: %s\n", 
		  env_to_string(window_env(active_track_amp_env(id), mix_beg, mix_dur, track_beg, track_dur)), 
		  env_to_string(e));
	  */
	}
      /* if intermediate track has no env, that's as if it were a flat env -- a no-op, not a break in the chain */
      id = active_track_track(id);      
    }
  return(e);
}

static void delete_track_1(mix_info *md, void *ptr)
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
  return(id);
}

static void lock_track_1(mix_info *md, void *ptr)
{
  set_mix_locked(md, true, false);
}

static XEN g_lock_track(XEN id)
{
  #define H_lock_track "(" S_lock_track " id) locks all mixes associated with track 'id'."
  int track_id;
  track_id = xen_to_c_track(id, S_lock_track);
  remix_track(track_id, lock_track_1, NULL);
  redisplay_track(track_id);
  return(id);
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
  int i;
  if (cp->tracks)
    {
      for (i = 0; i < cp->edit_size; i++)
	free_track_info(cp, i);
      FREE(cp->tracks);
      cp->tracks = NULL;
    }
}

static int gather_track_info(mix_info *md, void *ptr)
{
  track_info *ti = (track_info *)ptr;
  int track_id;
  if (md->track > 0)
    {
      track_id = md->track;
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
	      /* fprintf(stderr,"gather info track: %d, loc: %d\n", ti->ids[0], ti->locs[0]); */
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
		  /* fprintf(stderr,"gather info track: %d, loc %d\n", ti->ids[ti->size-1], ti->locs[ti->size-1]); */
		}
	    }
	  track_id = active_track_track(track_id);
	}
    }
  return(0);
}

static void record_track_info(chan_info *cp, int loc)
{
  track_info *ti;
  /* fprintf(stderr, "record_track_info at %d (%p, ctr: %d)\n", loc, cp->tracks, cp->edit_ctr); */
  if (track_ctr == 0)
    {
#if DEBUGGING
      if (cp->tracks[loc]) fprintf(stderr,"tracks overwritten");
#endif
      cp->tracks[loc] = NULL;
    }
  else
    {
      if (cp->tracks[loc]) free_track_info(cp, loc);
      ti = (track_info *)CALLOC(1, sizeof(track_info));
      ti->size = 0;
      map_over_channel_mixes(cp, gather_track_info, (void *)ti);
      if (ti->size == 0)
	{
	  /* fprintf(stderr,"no tracks?"); */
#if DEBUGGING
	  if (ti->ids) fprintf(stderr,"ids lost");
#endif
	  FREE(ti);
	  ti = NULL;
	}
      /* fprintf(stderr,"record track info set loc %d to %p\n", loc, ti); */
      cp->tracks[loc] = ti;
    }
}

void record_initial_track_info(chan_info *cp)
{
  record_track_info(cp, cp->edit_ctr);
}

void update_track_lists(chan_info *cp, int top_ctr)
{
  track_info *ti;
  /* fprintf(stderr,"update_track_lists cp: %d %p\n", cp->edit_ctr, cp->tracks); */
  if (cp->tracks)
    {
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
	  /* fprintf(stderr,"size: %d\n", ti->size); */
	  for (i = 0; i < ti->size; i++)
	    if (track_p(ti->ids[i]))
	      {
		track_list *tl;
		tl = tracks[ti->ids[i]];
		tl->loc = (int)(ti->locs[i]);
		/* fprintf(stderr,"set track %d to loc %d\n", ti->ids[i], tl->loc); */
	      }
	}
    }
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_4(g_make_track_sample_reader_w, g_make_track_sample_reader)
XEN_NARGIFY_1(g_next_track_sample_w, g_next_track_sample)
XEN_NARGIFY_1(g_read_track_sample_w, g_read_track_sample)
XEN_NARGIFY_1(g_free_track_sample_reader_w, g_free_track_sample_reader)
XEN_NARGIFY_1(g_tf_p_w, g_tf_p)
XEN_ARGIFY_4(g_play_track_w, g_play_track)
XEN_NARGIFY_1(g_free_track_w, g_free_track)
XEN_NARGIFY_1(g_track_p_w, g_track_p)
XEN_NARGIFY_0(g_tracks_w, g_tracks)
XEN_NARGIFY_1(g_track_w, g_track)
XEN_ARGIFY_1(g_track_color_w, g_track_color)
XEN_NARGIFY_2(g_set_track_color_w, g_set_track_color)
XEN_ARGIFY_1(g_track_amp_w, g_track_amp)
XEN_NARGIFY_2(g_set_track_amp_w, g_set_track_amp)
XEN_ARGIFY_1(g_track_speed_w, g_track_speed)
XEN_NARGIFY_2(g_set_track_speed_w, g_set_track_speed)
XEN_ARGIFY_1(g_track_amp_env_w, g_track_amp_env)
XEN_NARGIFY_2(g_set_track_amp_env_w, g_set_track_amp_env)
XEN_ARGIFY_1(g_track_position_w, g_track_position)
XEN_NARGIFY_2(g_set_track_position_w, g_set_track_position)
XEN_ARGIFY_1(g_track_track_w, g_track_track)
XEN_NARGIFY_2(g_set_track_track_w, g_set_track_track)
XEN_ARGIFY_1(g_track_frames_w, g_track_frames)
XEN_ARGIFY_1(g_delete_track_w, g_delete_track)
XEN_ARGIFY_1(g_lock_track_w, g_lock_track)
XEN_VARGIFY(g_make_track_w, g_make_track)
#else
#define g_make_track_sample_reader_w g_make_track_sample_reader
#define g_next_track_sample_w g_next_track_sample
#define g_read_track_sample_w g_read_track_sample
#define g_free_track_sample_reader_w g_free_track_sample_reader
#define g_tf_p_w g_tf_p
#define g_play_track_w g_play_track
#define g_free_track_w g_free_track
#define g_track_p_w g_track_p
#define g_tracks_w g_tracks
#define g_track_w g_track
#define g_track_color_w g_track_color
#define g_set_track_color_w g_set_track_color
#define g_track_amp_w g_track_amp
#define g_set_track_amp_w g_set_track_amp
#define g_track_speed_w g_track_speed
#define g_set_track_speed_w g_set_track_speed
#define g_track_amp_env_w g_track_amp_env
#define g_set_track_amp_env_w g_set_track_amp_env
#define g_track_position_w g_track_position
#define g_set_track_position_w g_set_track_position
#define g_track_track_w g_track_track
#define g_set_track_track_w g_set_track_track
#define g_track_frames_w g_track_frames
#define g_delete_track_w g_delete_track
#define g_lock_track_w g_lock_track
#define g_make_track_w g_make_track
#endif


  /* TODO: test mix|mix-vct|mix-region|selection track arg */

void g_init_track(void)
{
  tf_tag = XEN_MAKE_OBJECT_TYPE("TrackSampleReader", sizeof(track_fd));
#if HAVE_RUBY
  rb_define_method(tf_tag, "to_s", XEN_PROCEDURE_CAST print_tf, 0);
#endif
#if HAVE_GUILE
  scm_set_smob_print(tf_tag, print_tf);
  scm_set_smob_free(tf_tag, free_tf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(tf_tag, XEN_PROCEDURE_CAST g_read_track_sample, 0, 0, 0);
#endif
#endif

  XEN_DEFINE_PROCEDURE(S_make_track_sample_reader, g_make_track_sample_reader_w, 1, 3, 0, H_make_track_sample_reader);
  XEN_DEFINE_PROCEDURE(S_next_track_sample,        g_next_track_sample_w,        1, 0, 0, H_next_track_sample);
  XEN_DEFINE_PROCEDURE(S_read_track_sample,        g_read_track_sample_w,        1, 0, 0, H_read_track_sample);
  XEN_DEFINE_PROCEDURE(S_free_track_sample_reader, g_free_track_sample_reader_w, 1, 0, 0, H_free_track_sample_reader);
  XEN_DEFINE_PROCEDURE(S_track_sample_reader_p,    g_tf_p_w,                     1, 0, 0, H_tf_p);
  XEN_DEFINE_PROCEDURE(S_play_track,               g_play_track_w,               1, 3, 0, H_play_track);
  XEN_DEFINE_PROCEDURE(S_free_track,               g_free_track_w,               1, 0, 0, H_free_track);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_color,    g_track_color_w,    H_track_color,    S_setB S_track_color,    g_set_track_color_w,    0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_amp,      g_track_amp_w,      H_track_amp,      S_setB S_track_amp,      g_set_track_amp_w,      0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_speed,    g_track_speed_w,    H_track_speed,    S_setB S_track_speed,    g_set_track_speed_w,    0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_amp_env,  g_track_amp_env_w,  H_track_amp_env,  S_setB S_track_amp_env,  g_set_track_amp_env_w,  0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_position, g_track_position_w, H_track_position, S_setB S_track_position, g_set_track_position_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_track_track,    g_track_track_w,    H_track_track,    S_setB S_track_track,    g_set_track_track_w,    0, 1, 2, 0);

  XEN_DEFINE_PROCEDURE(S_track_frames, g_track_frames_w, 0, 1, 0, H_track_frames);
  XEN_DEFINE_PROCEDURE(S_delete_track, g_delete_track_w, 0, 1, 0, H_delete_track);
  XEN_DEFINE_PROCEDURE(S_lock_track,   g_lock_track_w,   0, 1, 0, H_lock_track);
  XEN_DEFINE_PROCEDURE(S_track,        g_track_w,        1, 0, 0, H_track);
  XEN_DEFINE_PROCEDURE(S_tracks,       g_tracks_w,       0, 0, 0, H_tracks);
  XEN_DEFINE_PROCEDURE(S_make_track,   g_make_track_w,   0, 0, 1, H_make_track);  
  XEN_DEFINE_PROCEDURE(S_track_p,      g_track_p_w,      1, 0, 0, H_track_p);
}

/* how to save-state here? -- mixes are not currently saved, but the track states could be
   dlp/mix-menu etc
   TODO: test lock-track and make it work with undo/redo somehow (similarly for mix-locked?
*/
