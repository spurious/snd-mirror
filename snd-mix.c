#include "snd.h"

typedef struct {         /* save one mix console state */
  int chans;             /* size of arrays in this struct */
  int edit_ctr;          /* cp edit_ctr at time of creation of this struct */
  off_t beg, end, orig, len;  /* samp positions in output (orig = where edit tree thinks it is) */
  int locked;
  Float *scalers;
  Float speed;
  env **amp_envs;
  int *mix_edit_ctr;     /* edit_ctr's in underlying mix sound */
} console_state;

typedef struct {
  snd_state *ss;
  chan_info *cp;
  mix_context *wg;
  char *name;
  char *in_filename;
  int in_chans;
  off_t in_samps;              /* in_samps needed to simplify speed changed duration calculations */
  int console_state_size;      /* current size of console_state list */
  console_state **states;      /* list of mixer states */
  console_state *current_cs;
  off_t anchor, orig_beg;      /* sample in in-data of console attachment */
  int curcons;
  int temporary;               /* in-filename was written by us and needs to be deleted when mix console is deleted */
  snd_info *add_snd;           /* readable snd_info struct for mix input */
  int id, x, nx, y, track, selected_chan, tagx, tagy, tag_y, height; 
                               /* tag_y only for user set mix_tag_y */
  env **panel_envs;            /* mix panel version of current amp envs */
} mix_info;


static mix_info *md_from_id(int n);
static void draw_mix_waveform(mix_info *md);
static void erase_mix_waveform(mix_info *md);

static int call_mix_speed_changed_hook(mix_info *md);
static int call_mix_amp_changed_hook(mix_info *md);
static int call_mix_position_changed_hook(mix_info *md, off_t samps);
static void call_multichannel_mix_hook(int *ids, int n);

chan_info *mix_channel_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) return(md->cp);
  return(NULL);
}


static void color_one_mix_from_id(int mix_id, COLOR_TYPE color)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) md->wg->color = color;
}

static COLOR_TYPE mix_to_color_from_id(int mix_id)
{
  mix_info *md;
  snd_state *ss;
  md = md_from_id(mix_id);
  if (md)
    return(md->wg->color);
  ss = get_global_state();
#if USE_NO_GUI
  return(0);
#else
  return(ss->sgx->basic_color);
#endif
}

static axis_context *unselected_mix_waveform_context(chan_info *cp, mix_info *md)
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
  g->color = cp->state->sgx->mix_color;
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


/* -------- console state (history of mix panel settings) -------- */

static console_state *make_console_state(int chans, int edit_ctr, off_t beg, off_t end)
{
  console_state *cs;
  cs = (console_state *)CALLOC(1, sizeof(console_state));
  cs->chans = chans;
  cs->edit_ctr = edit_ctr;
  cs->orig = beg;
  cs->beg = beg;
  cs->end = end;
  cs->len = end - beg + 1;
  cs->locked = 0;
  cs->mix_edit_ctr = (int *)CALLOC(chans, sizeof(int));
  cs->scalers = (Float *)CALLOC(chans, sizeof(Float));
  cs->amp_envs = NULL; 
  return(cs);
}

static console_state *copy_console(console_state *cs)
{
  console_state *ncs;
  int i;
  ncs = make_console_state(cs->chans, cs->edit_ctr, cs->orig, cs->end);
  if (!(cs->locked))
    for (i = 0; i < cs->chans; i++)
      ncs->scalers[i] = cs->scalers[i];
  ncs->locked = cs->locked;
  ncs->speed = cs->speed;
  for (i = 0; i < cs->chans; i++) 
    ncs->mix_edit_ctr[i] = cs->mix_edit_ctr[i];
  if (cs->amp_envs)
    {
      ncs->amp_envs = (env **)CALLOC(cs->chans, sizeof(env *));
      for (i = 0; i < cs->chans; i++) 
	ncs->amp_envs[i] = copy_env(cs->amp_envs[i]);
    }
  else ncs->amp_envs = NULL;
  ncs->len = cs->len;
  return(ncs);
}

static void make_current_console(mix_info *md)
{
  int i;
  console_state *cs, *cur;
  cs = md->states[md->curcons];
  cur = md->current_cs;
  cur->chans = cs->chans;
  for (i = 0; i < cs->chans; i++) 
    {
      cur->mix_edit_ctr[i] = cs->mix_edit_ctr[i];
      if (md->add_snd)
	{
	  /*
	  if (md->add_snd->chans[i]->edit_ctr != cur->mix_edit_ctr[i])
	    fprintf(stderr, "reset underlying chan %d pos from %d to %d\n", 
		    i, md->add_snd->chans[i]->edit_ctr, cur->mix_edit_ctr[i]);
	  */
	  md->add_snd->chans[i]->edit_ctr = cur->mix_edit_ctr[i];
	}
    }
  cur->edit_ctr = cs->edit_ctr;
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
  reflect_mix_in_mix_panel(md->id); /* ??? */
}

static console_state *free_console_state(console_state *cs)
{
  int i;
  if (cs)
    {
      if (cs->scalers) {FREE(cs->scalers); cs->scalers = NULL;}
      if (cs->mix_edit_ctr) {FREE(cs->mix_edit_ctr); cs->mix_edit_ctr = NULL;}
      if (cs->amp_envs) 
	{
	  for (i = 0; i < cs->chans; i++) 
	    free_env(cs->amp_envs[i]);
	  FREE(cs->amp_envs);
	}
      FREE(cs);
    }
  return(NULL);
}

static void release_pending_consoles(mix_info *md)
{
  int i;
  if (md->states)
    for (i = md->curcons + 1; i < md->console_state_size; i++) 
      if (md->states[i]) 
	md->states[i] = free_console_state(md->states[i]);
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
  snd_state *ss;
  ss = cp->state;
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
  md->ss = ss;
  md->add_snd = NULL;
  md->temporary = DONT_DELETE_ME;
  md->wg = set_mix_info_context(cp);
  md->anchor = 0;
  md->y = 0;
  md->selected_chan = 0;
  md->height = mix_waveform_height(ss);
  md->panel_envs = NULL;
  return(md);
}

static mix_info *free_mix_info(mix_info *md)
{
  int i;
  snd_state *ss;
  if (md)
    {
      ss = md->ss;
      if (md->id == ss->selected_mix) ss->selected_mix = INVALID_MIX_ID;
      if (md->wg) md->wg = free_mix_context(md->wg);
      mix_infos[md->id] = NULL;
      if (md->temporary == DELETE_ME) 
	snd_remove(md->in_filename, TRUE);
      if (md->name) {FREE(md->name); md->name = NULL;}
      if (md->in_filename) {FREE(md->in_filename); md->in_filename = NULL;}
      if (md->add_snd) {completely_free_snd_info(md->add_snd); md->add_snd = NULL;}
      if (md->states)
	{
	  for (i = 0; i < md->console_state_size; i++) 
	    if (md->states[i]) 
	      md->states[i] = free_console_state(md->states[i]);
	  FREE(md->states);
	  md->states = NULL;
	}
      if (md->current_cs) md->current_cs = free_console_state(md->current_cs);
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

static XEN select_mix_hook;

static void select_mix(mix_info *md)
{
  mix_info *old_md = NULL;
  snd_state *ss;
  ss = get_global_state();
  if ((ss->selected_mix != INVALID_MIX_ID) && 
      ((md == NULL) || (ss->selected_mix != md->id)))
    {
      old_md = md_from_id(ss->selected_mix);
      ss->selected_mix = INVALID_MIX_ID; /* force color update in wave display */
      if ((old_md) && (old_md->cp->show_mix_waveforms)) 
	draw_mix_waveform(old_md);
    }
  if ((md) && (mix_ok_and_unlocked(md->id)))
    {
      ss->selected_mix = md->id; 
      if (md->cp->show_mix_waveforms) 
	draw_mix_waveform(md);
      reflect_mix_in_mix_panel(md->id);
      if (XEN_HOOKED(select_mix_hook))
	run_hook(select_mix_hook,
		 XEN_LIST_1(C_TO_XEN_INT(md->id)),
		 S_select_mix_hook);
    }
  else ss->selected_mix = INVALID_MIX_ID;
}

void select_mix_from_id(int mix_id) {select_mix(md_from_id(mix_id));}


/* ---------------- MIX READ ---------------- */

typedef struct {
  int type;
  mix_info *md;
  console_state *cs;
  snd_fd **sfs;
  int chans;                           /* chans of input */
  int calc, base;
  Float x, sr;
  Float *lst, *nxt;
  src_state **srcs;
  mus_any **segs;
  int *ctr;
  off_t *samples;
  mus_sample_t **idata;
  Float samps_per_bin;
} mix_fd;

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
	md->add_snd = make_sound_readable(cp->state, md->in_filename, TRUE);
      else 
	{
	  snd_error("can't find %s: %s", md->in_filename, strerror(errno));
	  return(NULL);
	}
      add_sp = md->add_snd;
      add_sp->filename = copy_string(md->in_filename);
      add_sp->short_filename = filename_without_home_directory(add_sp->filename);
      for (i = 0; i < add_sp->nchans; i++) 
	{
	  cp = add_sp->chans[i];
	  if (cp) 
	    {
	      cp->mix_md = md;
	      if (cp->show_mix_waveforms) make_mix_input_amp_env(cp);
	    }
	}
    }
  return(md->add_snd);
}

snd_info *make_mix_readable_from_id(int id) {return(make_mix_readable(md_from_id(id)));}

static int mix_input_amp_env_usable(mix_info *md, Float samples_per_pixel) 
{
  env_info *ep = NULL;
  int i, happy = 1;
  chan_info *cp;
  snd_info *sp;
  Float samps_per_bin = 0.0;
  console_state *cs;
  cs = md->current_cs;
  if ((cs) && (cs->amp_envs))
    {
      for (i = 0; i < cs->chans; i++)
	if (cs->amp_envs[i])
	  return(FALSE);
    }
  sp = make_mix_readable(md);
  if (sp)
    {
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  if ((cp == NULL) || (cp->amp_envs == NULL)) return(FALSE);
	  ep = cp->amp_envs[cp->edit_ctr];
	  if ((ep == NULL) && 
	      (CURRENT_SAMPLES(cp) > AMP_ENV_CUTOFF))
	    ep = make_mix_input_amp_env(cp);
	  if ((ep) && 
	      (samps_per_bin == 0.0)) 
	    samps_per_bin = ep->samps_per_bin;
	  happy = ((ep) && 
		   (samples_per_pixel >= (Float)(ep->samps_per_bin)) && 
		   (ep->samps_per_bin == samps_per_bin));
	  if (!happy) break;
	}
      return(happy);
    }
  return(FALSE);
}


#define C_STRAIGHT 0
#define C_AMP 1
#define C_SPEED 2
#define C_ZERO 3

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
  Float spd, samp, sum = 0.0;
  console_state *cs;
  switch (mf->calc)
    {
    case C_STRAIGHT:
      if (mf->type == MIX_INPUT_SOUND)
	return(read_sample_to_float(mf->sfs[mf->base]));
      else return(next_mix_input_amp_env_sample(mf, mf->base));
      break;
    case C_ZERO: 
      return(0.0);
      break;
    case C_AMP:
      cs = mf->cs;
      if (mf->segs)
	{
	  for (i = 0; i < mf->chans; i++)
	    {
	      if (mf->type == MIX_INPUT_SOUND)
		samp = read_sample_to_float(mf->sfs[i]);
	      else samp = next_mix_input_amp_env_sample(mf, i);
	      if (mf->segs[i])
		sum += (samp * mus_env(mf->segs[i]));
	      else sum += (samp * cs->scalers[i]);
	    }
	}
      else
	{
	  if (mf->type == MIX_INPUT_SOUND)
	    {
	      for (i = 0; i < mf->chans; i++)
		sum += (read_sample_to_float(mf->sfs[i]) * cs->scalers[i]);
	    }
	  else
	    {
	      for (i = 0; i < mf->chans; i++)
		sum += (cs->scalers[i] * next_mix_input_amp_env_sample(mf, i));
	    }
	}
      break;
    case C_SPEED:
      cs = mf->cs;
      if (mf->srcs)
	{
	  if (mf->segs)
	    {
	      for (i = 0; i < mf->chans; i++)
		if (cs->scalers[i] > 0.0)
		  {
		    if (mf->segs[i])
		      sum += (mus_env(mf->segs[i]) * mus_src(mf->srcs[i]->gen, mf->sr, &src_input_as_needed));
		    else sum += (cs->scalers[i] * mus_src(mf->srcs[i]->gen, mf->sr, &src_input_as_needed));
		  }
	    }
	  else
	    {
	      for (i = 0; i < mf->chans; i++)
		if (cs->scalers[i] > 0.0)
		  sum += (cs->scalers[i] * mus_src(mf->srcs[i]->gen, mf->sr, &src_input_as_needed));
	    }
	}
      else
	{
	  spd = mf->sr;
	  if (mf->segs)
	    {
	      for (i = 0; i < mf->chans; i++)
		if (cs->scalers[i] > 0.0)
		  {
		    if (mf->segs[i])
		      sum += (mus_env(mf->segs[i]) * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
		    else sum += (cs->scalers[i] * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
		  }
	    }
	  else
	    {
	      for (i = 0; i < mf->chans; i++)
		if (cs->scalers[i] > 0.0)
		  sum += (cs->scalers[i] * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
	    }
	  mf->x += spd;
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
      break;
    }
  return(sum);
}

static off_t amp_env_len(mix_info *md, int chan)
{
  return(CURRENT_SAMPLES((md->add_snd)->chans[chan]));
}

static mix_fd *init_mix_read_any(mix_info *md, int old, int type)
{
  mix_fd *mf;
  env *e;
  snd_info *add_sp;
  chan_info *cp;
  console_state *cs;
  int i, chans;
  if (old) 
    cs = md->states[md->curcons];
  else cs = md->current_cs;
  if (cs->scalers == NULL) return(NULL);
  chans = md->in_chans;
  mf = (mix_fd *)CALLOC(1, sizeof(mix_fd));
  mf->type = type;
  mf->calc = C_ZERO;
  mf->sr = cs->speed;
  mf->md = md;
  mf->cs = cs;
  mf->chans = chans;
  for (i = 0; i < chans; i++)
    if (cs->scalers[i] != 0.0) 
      {
	mf->calc = C_STRAIGHT; 
	break;
      }
  if (mf->calc == C_ZERO) return(mf);
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
  if (cs->amp_envs)
    {
      mf->segs = (mus_any **)CALLOC(mf->chans, sizeof(mus_any *));
      for (i = 0; i < chans; i++)
	{
	  e = cs->amp_envs[i];
	  if ((e) && (cs->scalers[i] != 0.0))
	    mf->segs[i] = mus_make_env(e->data, e->pts, cs->scalers[i], 0.0, 1.0, 0.0, 0,
				       (type == MIX_INPUT_SOUND) ? (cs->len - 1) : amp_env_len(md, i),
				       NULL);
	  else mf->segs[i] = NULL;
	}
    }
  else mf->segs = NULL;
  if (mf->base >= chans) mf->base = 0; /* mono file mixed into sync'd stereo file for example */
  if (mf->sr != 1.0)
    mf->calc = C_SPEED;
  else
    {
      if (mf->segs)
	mf->calc = C_AMP;
      else
	{
	  mf->calc = C_STRAIGHT;
	  for (i = 0; i < chans; i++)
	    {
	      if (((i == mf->base) && (cs->scalers[i] != 1.0)) ||
		  ((i != mf->base) && (cs->scalers[i] != 0.0)))
		{
		  mf->calc = C_AMP;
		  break;
		}
	    }
	}
    }
  if (type == MIX_INPUT_SOUND)
    {
      if (mf->calc == C_STRAIGHT)
	/* to include edpos here we'd need to use it also during the mix_input_amp_env_read business below, so it's non-trivial to add */
	mf->sfs[mf->base] = init_sample_read_any(0, add_sp->chans[mf->base], READ_FORWARD, cs->mix_edit_ctr[mf->base]); 
      else
	{
	  for (i = 0; i < chans; i++)
	    mf->sfs[i] = init_sample_read_any(0, add_sp->chans[i], READ_FORWARD, cs->mix_edit_ctr[i]);
	}
    }
  if (mf->calc == C_SPEED)
    {
      if ((type == MIX_INPUT_SOUND) && (use_sinc_interp((md->ss))))
	{
	  mf->srcs = (src_state **)CALLOC(chans, sizeof(src_state *));
	  for (i = 0; i < chans; i++)
	    mf->srcs[i] = make_src(md->ss, 0.0, mf->sfs[i], 1.0);
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

static mix_fd *init_mix_read(mix_info *md, int old)
{
  return(init_mix_read_any(md, old, MIX_INPUT_SOUND));
}

static mix_fd *init_mix_input_amp_env_read(mix_info *md, int old, int hi)
{
  int i;
  mix_fd *mf = NULL;
  snd_info *sp;
  chan_info *cp;
  env_info *ep;
  mf = init_mix_read_any(md, old, MIX_INPUT_AMP_ENV);
  if (!mf) return(NULL);
  sp = md->add_snd;
  mf->ctr = (int *)CALLOC(sp->nchans, sizeof(int));
  mf->samples = (off_t *)CALLOC(sp->nchans, sizeof(off_t));
  mf->idata = (mus_sample_t **)CALLOC(sp->nchans, sizeof(mus_sample_t *));
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      mf->ctr[i] = -1; /* preincremented */
      mf->samples[i] = CURRENT_SAMPLES(cp);
      ep = cp->amp_envs[cp->edit_ctr];
      mf->samps_per_bin = ep->samps_per_bin;
      if (hi)
	mf->idata[i] = ep->data_max;
      else mf->idata[i] = ep->data_min;
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
      if (mf->idata) {FREE(mf->idata); mf->idata = NULL;}
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

static int remove_temporary_mix_file(mix_info *md, void *ptr)
{
  if (md->temporary == DELETE_ME) 
    snd_remove(md->in_filename, TRUE);
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

int disk_space_p(snd_info *sp, off_t bytes, off_t other_bytes, char *filename)
{
  int go_on;
  off_t kfree, kneeded, kother;
  kfree = disk_kspace(filename);
  if (kfree < 0) 
    {
      report_in_minibuffer_and_save(sp, strerror(errno)); 
      return(NO_PROBLEM);  /* what?? */
    }
  kneeded = bytes >> 10;
  if (kfree < kneeded)
    {
      if (other_bytes > 0)
	{
	  kother = other_bytes >> 10;
	  if (kother > kfree)
	    {
	      report_in_minibuffer_and_save(sp, "only " OFF_TD " Kbytes left on disk, changing to 16-bit temp output", kfree);
	      return(HUNKER_DOWN);
	    }
	}
      go_on = snd_yes_or_no_p(sp->state, "only " OFF_TD " Kbytes left on disk; continue?", kfree);
      if (!go_on) return(GIVE_UP);
      report_in_minibuffer(sp, "ok -- here we go...");
      return(BLIND_LEAP);
    }
  return(NO_PROBLEM);
}

static char *save_as_temp_file(mus_sample_t **raw_data, int chans, int len, int nominal_srate)
{
  char *newname;
  snd_state *ss;
  int format, ofd, no_space, hfd;
  format = MUS_OUT_FORMAT;
  ss = get_global_state();
  newname = shorter_tempnam(temp_dir(ss), "snd_");
                      /* we're writing our own private version of this thing, so we can use our own formats */
  hfd = snd_write_header(ss, newname, MUS_NEXT, nominal_srate, chans, 28, len * chans, format, NULL, 0, NULL);
  if (hfd == -1) return(NULL);
  ofd = snd_reopen_write(ss, newname);
  mus_file_open_descriptors(ofd, newname, format, 4, 28, chans, MUS_NEXT);
  /* mus_file_set_data_clipped(ofd, data_clipped(ss)); */
  lseek(ofd, 28, SEEK_SET);
  no_space = disk_space_p(any_selected_sound(ss), len * chans * 4, 0, newname);
  if (no_space != GIVE_UP)
    mus_file_write(ofd, 0, len - 1, chans, raw_data);
  if (mus_file_close(ofd) != 0)
    snd_error("mix save temp: can't close %s: %s!", newname, strerror(errno));
  return(newname);
}

#define OFFSET_FROM_TOP 0
/* axis top border width is 10 (snd-axis.c) */

static mix_info *add_mix(chan_info *cp, int chan, off_t beg, off_t num, 
			char *full_original_file, 
			int input_chans, int temp)
{ /* temp -> delete original file also */
  mix_info *md;
  char *namebuf;
  console_state *cs;
  md = make_mix_info(cp);     /* add active mix to chan_info list */
  md->in_chans = input_chans;
  md->orig_beg = beg;
  md->in_samps = num;
  namebuf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(namebuf, LABEL_BUFFER_SIZE, "mix%d", md->id);
  md->name = namebuf;
  md->temporary = temp;
  md->console_state_size = 1;
  md->states = (console_state **)CALLOC(md->console_state_size, sizeof(console_state *));
  cs = make_console_state(input_chans, cp->edit_ctr, beg, beg + num - 1);
  md->current_cs = make_console_state(input_chans, cp->edit_ctr, beg, beg + num - 1);
  md->states[0] = cs;
  if (chan < input_chans)
    cs->scalers[chan] = 1.0;
  cs->speed = 1.0;
  md->curcons = 0;
  make_current_console(md);
  md->in_filename = copy_string(full_original_file);
  reflect_mix_in_menu();
  reflect_mix_in_enved();
  update_graph(cp);
  return(md);
}

static mix_info *file_mix_samples(off_t beg, off_t num, char *tempfile, chan_info *cp, int chan, int temp, const char *origin, int with_tag)
{
  /* open tempfile, current data, write to new temp file mixed, close others, open and use new as change case */
  /* used for clip-region temp file incoming and C-q in snd-chn.c (i.e. mix in file) so sync not relevant */
  snd_fd *csf;
  snd_state *ss;
  snd_info *sp;
  int ofd, ifd;
  char *ofile;
  mus_sample_t **data;
  Float scaler;
  mus_sample_t *chandata;
  int in_chans, base, no_space, err = 0;
  off_t i, j, cursamps, len, size;
  file_info *ihdr, *ohdr;
  ss = cp->state;
  if (num <= 0) return(NULL); /* a no-op -- mixing in an empty file */
  len = CURRENT_SAMPLES(cp);
  if (beg >= len)
    extend_with_zeros(cp, len, beg - len + 1, "(mix-extend)", cp->edit_ctr);
  /* might set flag here that we need backup after file_mix_samples below (backup_edit_list(cp)) */
  /* otherwise user sees unexplained mix-extend in edit history list */
  if (beg < 0) beg = 0;
  ihdr = make_file_info(tempfile, ss);
  if (!ihdr) return(NULL);
  sp = cp->sound;
  in_chans = ihdr->chans;
  if (in_chans <= chan) 
    {
      base = 0; 
      scaler = 0.0;
    } 
  else 
    {
      base = chan; 
      scaler = 1.0;
    }
  ofile = snd_tempnam(ss);
  ohdr = make_temp_header(ofile, SND_SRATE(sp), 1, 0, (char *)origin);
  ofd = open_temp_file(ofile, 1, ohdr, ss);
  if (ofd == -1) 
    {
      if (ihdr) free_file_info(ihdr);
      snd_error("mix temp file %s: %s", ofile, strerror(errno)); 
      return(NULL);
    }
  no_space = disk_space_p(sp, num * 4, 0, ofile);
  if (no_space == GIVE_UP)
    {
      if (ihdr) free_file_info(ihdr);
      mus_file_close(ofd);
      snd_remove(ofile, TRUE);
      FREE(ofile);
      return(NULL);
    }
  csf = init_sample_read(beg, cp, READ_FORWARD);
  if (csf == NULL) 
    {
      if (ihdr) free_file_info(ihdr);
      mus_file_close(ofd);
      snd_remove(ofile, TRUE);
      FREE(ofile);
      return(NULL);
    }
  ifd = snd_open_read(ss, tempfile);
  mus_file_open_descriptors(ifd, tempfile,
			    ihdr->format,
			    mus_data_format_to_bytes_per_sample(ihdr->format),
			    ihdr->data_location,
			    ihdr->chans,
			    ihdr->type);
  during_open(ifd, tempfile, SND_MIX_FILE);
  if (num < MAX_BUFFER_SIZE) size = num; else size = MAX_BUFFER_SIZE;
  data = (mus_sample_t **)CALLOC(in_chans, sizeof(mus_sample_t *));
  data[base] = (mus_sample_t *)CALLOC(size, sizeof(mus_sample_t));
  chandata = data[base];
  lseek(ofd, ohdr->data_location, SEEK_SET);
  lseek(ifd, ihdr->data_location, SEEK_SET);
  if (scaler == 0.0)
    {
      for (i = 0; i < num; i += MAX_BUFFER_SIZE)
	{
	  cursamps = num - i;
	  if (cursamps > MAX_BUFFER_SIZE) cursamps = MAX_BUFFER_SIZE;
	  for (j = 0; j < cursamps; j++)
	    chandata[j] = read_sample(csf);
	  err = mus_file_write(ofd, 0, cursamps - 1, 1, &chandata);
	  if (err == -1) break;
	}
    }
  else
    {
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
    }
  close_temp_file(ofd, ohdr, num * mus_data_format_to_bytes_per_sample(ohdr->format), sp);
  mus_file_close(ifd);
  free_snd_fd(csf);
  FREE(data[base]);
  FREE(data);
  free_file_info(ihdr);
  free_file_info(ohdr);
  file_change_samples(beg, num, ofile, cp, 0, DELETE_ME, DONT_LOCK_MIXES, origin, cp->edit_ctr);
  if (ofile) FREE(ofile);
  if (with_tag)
    return(add_mix(cp, chan, beg, num, tempfile, in_chans, temp));
  else return(NULL);
}

/* next functions canonicalize mixer input -- 
 *   all end up with a filename containing the original to-be-mixed input
 *                     length (per channel samples in input)
 *                     begin sample in output for mix start
 *                     an array of cps for mixing into
 *                     a notion of initial scalers
 */

static int mix(off_t beg, off_t num, int chans, chan_info **cps, char *mixinfile, int temp, const char *origin, int with_tag)
{
  /* loop through out_chans cps writing the new mixed temp files and fixing up the edit trees */
  int i, id = -1, j = 0;
  int *ids;
  mix_info *md;
  snd_info *sp;
  ids = (int *)CALLOC(chans, sizeof(int));
  for (i = 0; i < chans; i++)
    {
      ids[i] = -1;
      sp = cps[i]->sound;
      md = file_mix_samples(beg, num, mixinfile, cps[i], i, temp, origin, with_tag); /* explode in this file, or mix in snd-clm.c */
      if (md) 
	{
	  if (id == -1) id = md->id;
	  if ((sp) && (sp->sync)) ids[j++] = md->id;
	}
    }
  if (j > 1) call_multichannel_mix_hook(ids, j);
  FREE(ids);
  return(id);
}

int copy_file_and_mix(off_t beg, off_t num, char *file, chan_info **cps, int out_chans, const char *origin, int with_tag)
{
  /* always write to tempfile (protect section/lisp temps from possible overwrites) */
  char *newname;
  int err, id = -1;
  snd_state *ss;
  ss = cps[0]->state;
  newname = shorter_tempnam(temp_dir(ss), "snd_");
  err = copy_file(file, newname);
  if (err != MUS_NO_ERROR)
    snd_error("can't save mix temp file (%s: %s)", newname, strerror(errno));
  else
    id = mix(beg, num, out_chans, cps, newname, DELETE_ME, origin, with_tag);
  if (newname) FREE(newname);
  return(id);
}

int mix_file_and_delete(off_t beg, off_t num, char *file, chan_info **cps, int out_chans, const char *origin, int with_tag)
{
  return(mix(beg, num, out_chans, cps, file, DELETE_ME, origin, with_tag));
}

int mix_complete_file(snd_info *sp, char *str, const char *origin, int with_tag)
{
  /* no need to save as temp here, but we do need sync info (from menu and keyboard) */
  chan_info *cp;
  chan_info **cps = NULL;
  int nc, chans, id = -1;
  off_t len;
  sync_info *si = NULL;
  char *fullname = NULL;
  if ((sp) && (str) && (*str))
    {
      clear_minibuffer(sp);
      fullname = mus_expand_filename(str);
      nc = mus_sound_chans(fullname);
      if (nc != -1)
	{
	  len = mus_sound_samples(fullname) / nc;
	  if (len == 0)
	    {
	      if (fullname) FREE(fullname);
	      return(-2);
	    }
	  cp = any_selected_channel(sp);
	  if (sp->sync != 0)
	    {
	      si = snd_sync(sp->state, sp->sync); 
	      cps = si->cps;
	      chans = si->chans;
	    }
	  else
	    {
	      cps = (chan_info **)CALLOC(1, sizeof(chan_info *));
	      cps[0] = cp;
	      chans = 1;
	    }
	  id = mix(cp->cursor, len, chans, cps, fullname, DONT_DELETE_ME, origin, with_tag);
	  if (si) 
	    si = free_sync_info(si); 
	  else 
	    if (cps) FREE(cps);
	}
      else 
	report_in_minibuffer_and_save(sp, "can't open file: %s, %s ", fullname, strerror(errno));
      if (fullname) FREE(fullname);
    }
  return(id);
}

#define CONSOLE_INCREMENT 8

static void extend_console_list(mix_info *md)
{
  int i, lim;
  md->curcons++;
  if (md->curcons >= md->console_state_size)
    {
      lim = md->console_state_size;
      md->console_state_size += CONSOLE_INCREMENT;
      md->states = (console_state **)REALLOC(md->states, md->console_state_size * sizeof(console_state *));
      for (i = lim; i < md->console_state_size; i++) md->states[i] = NULL;
    }
}

static int backup_mix(mix_info *md, void *ptr)
{
  int one_edit, curcons;
  console_state *cs, *curcs;
  one_edit = (*((int *)ptr));
  curcons = md->curcons;
  curcs = md->states[curcons];
  while ((md->curcons > 1) && 
	 ((md->states[md->curcons - 1])->edit_ctr > one_edit))
    {
      md->curcons--; 
      cs = md->states[md->curcons];
      if (cs) md->states[md->curcons] = free_console_state(cs); 
    }
  if (md->curcons != curcons)
    {
      md->states[curcons] = NULL;
      md->states[md->curcons] = curcs;
    }
  curcs->edit_ctr = one_edit;
  return(0);
}

void backup_mix_list(chan_info *cp, int edit_ctr)
{
  /* we're at md->states[md->curcons] (console_state) with cs->edit_ctr at value upon local edit */
  /* edit_ctr is the one-edit point for this channel */
  map_over_channel_mixes(cp, backup_mix, (void *)(&edit_ctr));
}


static void remix_file(mix_info *md, const char *origin)
{
  off_t beg, end, i, num;
  int j, ofd = 0, size, no_space, use_temp_file;
  Float val = 0.0, maxy, miny;
  snd_info *cursp;
  mix_fd *add, *sub;
  snd_fd *cur, *sfb, *afb;
  snd_state *ss;
  char *ofile = NULL;
  mus_sample_t **data;
  mus_sample_t *chandata;
  mus_sample_t mval, mmax, mmin;
  file_info *ohdr = NULL;
  axis_info *ap;
  chan_info *cp;
  off_t old_beg, old_end, new_beg, new_end;
  int err = 0;
  console_state *cs;
  release_pending_consoles(md);
  cs = md->current_cs;
  cp = md->cp;
  ap = cp->axis;
  old_beg = cs->orig;
  old_end = cs->end;
  new_beg = cs->beg;
  new_end = cs->beg + cs->len - 1;

  ss = cp->state;
  cursp = cp->sound;

  beg = (old_beg < new_beg) ? old_beg : new_beg;
  end = (old_end > new_end) ? old_end : new_end;
  num = end - beg + 1;
  use_temp_file = (num >= MAX_BUFFER_SIZE);

  if (use_temp_file)
    {
      ofile = snd_tempnam(ss);
      ohdr = make_temp_header(ofile, SND_SRATE(cursp), 1, 0, (char *)origin);
      ofd = open_temp_file(ofile, 1, ohdr, ss);
      if (ofd == -1)
	{
	  snd_error("can't write mix temp file %s: %s\n", ofile, strerror(errno));
	  return;
	}
    }
  add = init_mix_read(md, 0);
  if (!add) return;
  sub = init_mix_read(md, 1);
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
	  snd_remove(ofile, TRUE);
	  FREE(ofile);
	  return;
	  break;
	case HUNKER_DOWN:
	  close_temp_file(ofd, ohdr, 0, cursp);
	  if (mus_data_format_to_bytes_per_sample(MUS_OUT_FORMAT) == 2)
	    ohdr->format = MUS_OUT_FORMAT;
	  else
	    {
	      if (mus_data_format_to_bytes_per_sample(MUS_COMPATIBLE_FORMAT) == 2)
		ohdr->format = MUS_COMPATIBLE_FORMAT;
	      else
#if MUS_LITTLE_ENDIAN
		ohdr->format = MUS_LSHORT;
#else
		ohdr->format = MUS_BSHORT;
#endif
	    }
	  ofd = open_temp_file(ofile, 1, ohdr, ss);
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
  if ((add->calc == C_ZERO) && (sub->calc == C_ZERO))
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
      if ((add->calc == C_STRAIGHT) && (sub->calc == C_STRAIGHT))
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
      close_temp_file(ofd, ohdr, num * mus_data_format_to_bytes_per_sample(ohdr->format), cursp);
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
 
  extend_console_list(md);
  cs = copy_console(cs);
  cs->edit_ctr = cp->edit_ctr;
  cs->orig = new_beg + beg;
  cs->beg = cs->orig;
  cs->end = cs->beg + cs->len - 1;

  md->states[md->curcons] = cs;
  make_current_console(md);

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
  update_graph(cp);
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
  double main_start, new_start, old_start;
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

  new_fd = init_mix_read(md, 0);
  if (!new_fd) return(0);
  old_fd = init_mix_read(md, 1);
  if (!old_fd) return(0);

  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((double)(ap->losamp) / (double)(ep->samps_per_bin));
  main_start = (double)ap->losamp;
  
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
  double main_start, new_start, old_start;
  Float val;
  mix_fd *new_min_fd, *new_max_fd, *old_min_fd, *old_max_fd;
  Float new_ymin, new_ymax, old_ymin, old_ymax, main_ymin, main_ymax;
  Float new_high, new_low, old_high, old_low;
  double xi, xf, xfinc, x;
  off_t lo, hi;
  int main_loc, j;
  Locus lastx;
  env_info *ep;

  lo = ap->losamp;
  hi = ap->hisamp;

  new_min_fd = init_mix_input_amp_env_read(md, 0, 0); /* not old, not hi */
  new_max_fd = init_mix_input_amp_env_read(md, 0, 1); /* not old, hi */
  old_min_fd = init_mix_input_amp_env_read(md, 1, 0); /* old, not hi */
  old_max_fd = init_mix_input_amp_env_read(md, 1, 1); /* old, hi */

  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((double)(ap->losamp) / (double)(ep->samps_per_bin));
  main_start = (double)(ap->losamp);

  if (lo > newbeg) 
    {
      for (x = (double)lo; x < (double)newbeg; x += new_max_fd->samps_per_bin) 
	{
	  new_low = next_mix_sample(new_min_fd);
	  new_high = next_mix_sample(new_max_fd);
	}
      new_ymin = new_low;
      new_ymax = new_high;
      new_start = (double)lo;
    }
  else 
    {
      new_ymin = 0.0;
      new_ymax = 0.0;
      new_start = (double)newbeg;
    }

  if ((lo > oldbeg) && (oldend > lo))
    {
      for (x = (double)lo; x < (double)oldbeg; x += old_max_fd->samps_per_bin) 
	{
	  old_low = next_mix_sample(old_min_fd);
	  old_high = next_mix_sample(old_max_fd);
	}
      old_ymin = old_low;
      old_ymax = old_high;
      old_start = (double)lo;
    }
  else 
    {
      old_start = (double)oldbeg;
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

static void make_temporary_graph(chan_info *cp, mix_info *md, console_state *cs)
{
  off_t oldbeg, newbeg, oldend, newend;
  off_t i, samps;
  int j;
  Locus xi;
  int widely_spaced;
  axis_info *ap;
  snd_info *sp;
  mix_context *ms;
  snd_state *ss;
  Float samples_per_pixel, xf;
  double x, incr, initial_x;
  Float ina, ymin, ymax;
  off_t lo, hi;
  snd_fd *sf = NULL, *sfb, *afb;
  mix_fd *add = NULL, *sub = NULL;
  int x_start, x_end;
  double start_time, cur_srate;
  mus_sample_t mina, mymax, mymin;
  ss = cp->state;
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
      add = init_mix_read(md, 0);
      if (!add) return;
      sub = init_mix_read(md, 1);
      if (!sub) return;
      if ((oldbeg < lo) && (lo < oldend)) 
	for (i = oldbeg; i < lo; i++) 
	  next_mix_sample(sub);
      if (samples_per_pixel < 1.0)
	{
	  incr = 1.0 / samples_per_pixel;
	  initial_x = x_start;
	  widely_spaced = 1;
	}
      else
	{
	  incr = (double)1.0 / cur_srate;
	  initial_x = start_time;
	  widely_spaced = 0;
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
      if (amp_env_usable(cp, samples_per_pixel, ap->hisamp, TRUE, cp->edit_ctr))
	{
	  if (mix_input_amp_env_usable(md, samples_per_pixel))
	    j = make_temporary_amp_env_graph(cp, ap, md, samples_per_pixel, newbeg, newend, oldbeg, oldend);
	  else j = make_temporary_amp_env_mixed_graph(cp, ap, md, samples_per_pixel, newbeg, newend, oldbeg, oldend);
	}
      else
	{
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (sf == NULL) return;
	  add = init_mix_read(md, 0);
	  if (!add) return;
	  sub = init_mix_read(md, 1);
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

	  if ((add->calc == C_ZERO) && (sub->calc == C_ZERO))
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
	      if ((add->calc == C_STRAIGHT) && (sub->calc == C_STRAIGHT))
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

static int display_mix_amp_env(mix_info *md, Float scl, int yoff, off_t newbeg, off_t newend, chan_info *cp, Float srate, axis_info *ap, int draw)
{
  /* need min and max readers */
  snd_state *ss;
  mix_fd *min_fd, *max_fd;
  off_t hi, lo;
  int j;
  Locus lastx, newx;
  Float ymin, ymax, high = 0.0, low = 0.0;
  double sum, xend, xstart, xstep;
  min_fd = init_mix_input_amp_env_read(md, 0, 0); /* not old, not hi */
  max_fd = init_mix_input_amp_env_read(md, 0, 1); /* not old, hi */
  lo = ap->losamp;
  hi = ap->hisamp;
  ss = md->ss;

  /* mix starts at newbeg, current display starts at lo,
     mix goes to newend, current display goes to hi,
     cp->amp_envs[cp->edit_ctr]->samps_per_bin is the original's amp_env step size
     mf->samps_per_bin is the mix input step size
  */

  if (lo > newbeg) 
    {
      for (sum = (double)lo; sum < (double)newbeg; sum += max_fd->samps_per_bin) 
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
  if (draw)
    draw_both_grf_points(cp,
			 (md->id == ss->selected_mix) ? selected_mix_waveform_context(cp) : unselected_mix_waveform_context(cp, md),
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
  width = mix_tag_width(cp->state);
  height = mix_tag_height(cp->state);
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

static BACKGROUND_FUNCTION_TYPE watch_mix_proc = 0;    /* work proc if mouse outside graph causing axes to move */

static int display_mix_waveform(chan_info *cp, mix_info *md, console_state *cs, int draw)
{
  snd_state *ss;
  off_t i, newbeg, newend, endi;
  Float scl;
  int j = 0;
  off_t samps;
  Locus xi;
  int widely_spaced;
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
  ss = cp->state;
  ap = cp->axis;
  lo = ap->losamp;
  hi = ap->hisamp;
  if ((newend <= lo) || (newbeg >= hi)) return(0);
  scl = md->height;
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

  if ((ss->just_time == 1) && (event_pending(ss))) return(0);

  if (sp) 
    {
      if (md->id == ss->selected_mix)
	selected_mix_waveform_context(cp);
      else unselected_mix_waveform_context(cp, md);
    }
  if ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE))
    {
      add = init_mix_read(md, 0);
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
	  widely_spaced = 1;
	}
      else
	{
	  incr = (double)1.0 /cur_srate;
	  initial_x = start_time;
	  widely_spaced = 0;
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
			    (md->id == ss->selected_mix) ? selected_mix_waveform_context(cp) : unselected_mix_waveform_context(cp, md),
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
	  add = init_mix_read(md, 0);
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
				     (md->id == ss->selected_mix) ? selected_mix_waveform_context(cp) : unselected_mix_waveform_context(cp, md),
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



/* -------------------------------- moving mix consoles -------------------------------- */

static int mix_dragged = 0;

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

static void move_mix(mix_info *md);

void move_mix_tag(int mix_tag, int x)
{
  mix_info *md;
  mix_dragged = 1;
  md = md_from_id(mix_tag);
  md->x = x;
  move_mix(md);
}

void finish_moving_mix_tag(int mix_tag, int x)
{
  mix_info *md;
  console_state *cs;
  mix_context *ms;
  off_t samps_moved = 0;
  md = md_from_id(mix_tag);
  md->x = x;
  cs = md->current_cs;
  if (watch_mix_proc) 
    {
      BACKGROUND_REMOVE(watch_mix_proc);
      watch_mix_proc = 0;
    }
  mix_dragged = 0;
  ms = md->wg;
  ms->lastpj = 0;
  if (cs->beg == cs->orig) return;
  samps_moved = cs->beg - cs->orig;
  if (!(call_mix_position_changed_hook(md, samps_moved)))
    remix_file(md, "Mix: drag");
}

#define SLOPPY_MOUSE 2
static int hit_mix_1(mix_info *md, void *uvals)
{
  int *vals = (int *)uvals;
  int mx, my, width, height;
  width = mix_tag_width(md->ss);
  height = mix_tag_height(md->ss);
  mx = md->x - width;
  my = md->y - height / 2;
  if ((vals[0] + SLOPPY_MOUSE >= mx) && (vals[0] - SLOPPY_MOUSE <= (mx + width)) &&
      (vals[1] + SLOPPY_MOUSE >= my) && (vals[1] - SLOPPY_MOUSE <= (my + height)))
    return(md->id + 1);
  return(0);
}

int hit_mix(chan_info *cp, int x, int y)
{
  int xy[2];
  int mx;
  xy[0] = x;
  xy[1] = y;
  mx = map_over_channel_mixes(cp, hit_mix_1, (void *)xy);
  if (mx > 0)
    {
      mix_info *md;
      md = md_from_id(mx - 1);
      mix_save_graph(md->ss, md->wg, make_graph(cp, cp->sound, cp->state));
      select_mix(md);
      return(mx - 1);
    }
  return(NO_MIX_TAG);
}

void start_mix_drag(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  mix_save_graph(md->ss, md->wg, make_graph(md->cp, md->cp->sound, md->cp->state));
}

/* for axis movement (as in mark drag off screen) */

static BACKGROUND_TYPE watch_mix(GUI_POINTER m)
{
  if (watch_mix_proc)
    {
      move_mix((mix_info *)m);
      return(BACKGROUND_CONTINUE);
    }
  else return(BACKGROUND_QUIT);
}

static void move_mix(mix_info *md)
{
  snd_state *ss;
  axis_info *ap;
  chan_info *cp;
  console_state *cs;
  int nx, x, updated = 0;
  off_t samps, samp;
  cp = md->cp;
  if (!cp) return;
  ap = cp->axis;
  if (!ap) return;
  ss = md->ss;
  x = md->x;
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (watch_mix_proc)
	{
	  if ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) return;
	  if ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) return;
	}
      nx = move_axis(cp, ap, x); /* calls update_graph eventually (in snd-chn.c reset_x_display) */
      updated = 1;
      if ((mix_dragged) && (!watch_mix_proc))
	watch_mix_proc = BACKGROUND_ADD(ss, watch_mix, md);
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
  cs = md->current_cs;
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
    }
  else
    {
      if (updated) 
	{
	  cs = md->current_cs;
	  make_temporary_graph(cp, md, cs);
	}
    }
}



/* ---------------- MIX ACTIONS ---------------- */

static mix_info *active_mix(chan_info *cp)
{
  mix_info *md, *curmd = NULL;
  console_state *cs;
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
	  cs = md->current_cs;
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

int active_mix_p(chan_info *cp) {return(active_mix(cp) != NULL);}

off_t mix_beg(chan_info *cp)
{
  /* used in snd-chn.c for zoom focus active */
  mix_info *md;
  console_state *cs;
  md = active_mix(cp);
  if (md) 
    {
      cs = md->current_cs;
      if (cs) return(cs->orig + md->anchor);
    }
  return(-1);
}

static console_state *backup_console(chan_info *cp, mix_info *md)
{
  console_state *cs;
  /* undo -- look for first (last in list) cs with cs->edit_ctr <= cp->edit_ctr console */
  cs = md->states[md->curcons];
  while ((md->curcons > 0) && (cs->edit_ctr > cp->edit_ctr)) 
    {
      md->curcons--; 
      cs = md->states[md->curcons];
    }
  if (cs->edit_ctr > cp->edit_ctr) return(NULL);
  make_current_console(md);
  return(md->current_cs);
}

static console_state *restore_console(chan_info *cp, mix_info *md)
{
  console_state *cs;
  /* redo -- looking for the console that brackets cp->edit_ctr */
  cs = md->states[md->curcons];
  while ((md->curcons < (md->console_state_size - 1)) && 
	 (cs->edit_ctr < cp->edit_ctr) && 
	 (md->states[md->curcons + 1]))
    {
      md->curcons++; 
      cs = md->states[md->curcons];
    }
  if ((md->curcons > 0) && 
      (cs->edit_ctr > cp->edit_ctr)) 
    md->curcons--;
  make_current_console(md);
  return(md->current_cs);
}

void reset_mix_graph_parent(chan_info *cp)
{
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
  /* called in display_channel_data if show_mix_consoles(ss) and cp->mixes
   * this needs to spin through the mixes, 
   *   un-manage those whose mix has wandered off screen, and release the associated widgets,
   *   and re-manage those that are now active, grabbing widgets if necessary.
   */
  mix_info *md;
  snd_state *ss;
  console_state *cs;
  axis_info *ap;
  off_t lo, hi, spot;
  int i, xspot, turnover, y, combined, hgt;
  combined = (((snd_info *)(cp->sound))->channel_style != CHANNELS_SEPARATE);
  ap = cp->axis;
  ss = cp->state;
  lo = ap->losamp;
  hi = ap->hisamp;
  turnover = (int)(ap->height * 0.4);
  y = mix_tag_height(cp->state);
  hgt = y;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp))
	{
	  cs = md->current_cs;
	  if (cs->edit_ctr > cp->edit_ctr) 
	    cs = backup_console(cp, md);
	  else
	    if (cs->edit_ctr < cp->edit_ctr)
	      cs = restore_console(cp, md);
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

static off_t lt_beg, lt_end;

static int lock_mixes(mix_info *md, void *ptr)
{
  console_state *cs;
  chan_info *cp;
  snd_state *ss;
  /* find affected console, extend and lock */
  cs = md->states[md->curcons];
  if (!(cs->locked))
    {
      if (((cs->orig >= lt_beg) && (cs->orig <= lt_end)) ||
	  ((cs->end >= lt_beg) && (cs->end <= lt_end)) ||
	  ((cs->orig < lt_beg) && (cs->end > lt_end)))
	{
	  extend_console_list(md);
	  if (md->states[md->curcons]) free_console_state(md->states[md->curcons]);
	  md->states[md->curcons] = (console_state *)CALLOC(1, sizeof(console_state));
	  cs = md->states[md->curcons];
	  cs->locked = 1;
	  ss = get_global_state();
	  if (ss->selected_mix == md->id) ss->selected_mix = INVALID_MIX_ID;
	  cp = md->cp;
	  cs->edit_ctr = cp->edit_ctr;
	  cs = md->current_cs;
	  cs->locked = 1;
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
  lt_beg = beg;
  lt_end = end;
  map_over_channel_mixes(cp, lock_mixes, NULL);
  reflect_mix_in_menu();
  reflect_mix_in_enved();
}

void release_pending_mixes(chan_info *cp, int edit_ctr)
{
  /* look for mixes that have been cancelled by undo followed by unrelated edit */
  mix_info *md;
  console_state *cs;
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
  console_state *cur;
  int i, lim;
  lim = md->curcons;
  cur = md->states[lim];
  /* make states[0] and [1] both look like cur, collapse rest */
  if (lim > 0)
    {
      for (i = 0; i < lim; i++)
	md->states[i] = free_console_state(md->states[i]);
      md->states[0] = cur;
      md->states[lim] = NULL;
      md->curcons = 0;
    }
  cur->edit_ctr = 0;
  md->states[0]->edit_ctr = 0;
  make_current_console(md);
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

static int ready_mix(mix_info *md)
{
  console_state *cs;
  chan_info *cp;
  cp = md->cp;
  cs = md->states[md->curcons];
  return(((cs) && (!(cs->locked)) && 
	  (cs->edit_ctr <= cp->edit_ctr)));
}

static int ripple_mixes_1(mix_info *md, void *ptr)
{
  console_state *cs, *ncs;
  mixrip *data;
  chan_info *cp;
  data = (mixrip *)ptr;
  cp = data->cp;
  cs = md->current_cs;
  if ((cs) && (!(cs->locked)) && (cs->beg > data->beg) && (ready_mix(md)))
    {
      ncs = copy_console(cs);
      ncs->edit_ctr = cp->edit_ctr;
      ncs->orig = cs->beg + data->change;
      ncs->beg = ncs->orig;
      ncs->end = ncs->beg + cs->len - 1;
      if (cp->show_mix_waveforms) erase_mix_waveform(md);
      extend_console_list(md);
      md->states[md->curcons] = ncs;
      make_current_console(md);
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

static int compare_consoles(const void *umx1, const void *umx2)
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
  console_state *cs;
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
	      cs = md->current_cs;
	      if (!(cs->locked))
		{
		  css[j] = cs->orig;
		  j++;
		}
	    }
	}
      qsort((void *)css, j, sizeof(off_t), compare_consoles);
      /* now find where we are via cp->cursor and go forward or back as per count */
      if (count > 0)
	{
	  for (i = 0; i < j; i++)
	    if (css[i] > cp->cursor)
	      {
		count--;
		if (count == 0)
		  {
		    cursor_moveto(cp, css[i]);
		    break;
		  }
	      }
	  if ((count > 0) && (cp->cursor < css[j - 1]))
	    cursor_moveto(cp, css[j - 1]);
	}
      else
	{
	  for (i = j - 1; i >= 0; i--)
	    if (css[i] < cp->cursor)
	      {
		count++;
		if (count == 0)
		  {
		    cursor_moveto(cp, css[i]);
		    break;
		  }
	      }
	  if ((count < 0) && (cp->cursor > css[0]))
	    cursor_moveto(cp, css[0]);
	}
      FREE(css);
    }
}


int mix_ok(int n) 
{
  mix_info *md; 
  md = md_from_id(n); 
  return((md) && 
	 (md->states) && 
	 (md->states[0]) && 
	 (md->cp) &&
	 (((md->states[0])->edit_ctr) <= ((md->cp)->edit_ctr)));
}

int mix_ok_and_unlocked(int n)
{
  mix_info *md; 
  md = md_from_id(n); 
  return((md) && 
	 (md->current_cs) &&
	 (md->current_cs->locked == 0) &&
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

int current_mix_id(snd_state *ss)
{
  if (ss->selected_mix != INVALID_MIX_ID)
    return(ss->selected_mix);
  return(any_mix_id());
}

static void draw_mix_waveform(mix_info *md) 
{
  display_mix_waveform(md->cp, md, md->current_cs, TRUE);
}

static void erase_mix_waveform(mix_info *md) 
{
  display_mix_waveform(md->cp, md, md->current_cs, FALSE);
}



/* ---------------- TRACKS ---------------- */
/* track reader: an array of mix readers with state: active, waiting, null (done) */

typedef struct {
  int mixes;
  off_t *state;
  off_t *len;
  mix_fd **fds;
} track_fd;

static track_fd *init_track_reader(chan_info *cp, int track_num, int global) /* edit-position? direction? */
{
  track_fd *fd = NULL;
  int mixes = 0, i, mix;
  off_t track_beg;
  mix_info *md;
  console_state *cs;
  track_beg = CURRENT_SAMPLES(cp);
  for (i = 0; i < mix_infos_ctr; i++) 
    if (mix_ok(i))
      {
	md = mix_infos[i];
	if (md->track == track_num)
	  {
	    if (md->cp == cp) mixes++;
	    cs = md->current_cs;
	    if ((global) || (md->cp == cp))
	      if (cs->orig < track_beg) 
		track_beg = cs->orig;
	  }
      }
  if (mixes > 0)
    {
      fd = (track_fd *)CALLOC(1, sizeof(track_fd));
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
		fd->fds[mix] = init_mix_read(md, FALSE);
		cs = md->current_cs;
		fd->state[mix] = cs->orig - track_beg;
		fd->len[mix] = cs->len;
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
		fd->fds[i] = free_mix_fd(fd->fds[i]);
	    }
	  else fd->state[i]--;
	}
  return(sum);
}

static void play_track(snd_state *ss, chan_info **ucps, int chans, int track_num)
{
  track_fd **fds;
  chan_info **cps;
  chan_info *locp;
  int playfd, i, j, k, n, samps, chan = 0, happy = 0, need_free = 0, format, datum_bytes, outchans, frames;
#if MAC_OSX
  float *buf;
#else
  #if HAVE_ALSA
    mus_sample_t **buf;
    char *outbuf;
    Float val[4];
  #else
    short *buf;
  #endif
#endif
  if (ucps == NULL)
    {
      chans = active_channels(ss, WITH_VIRTUAL_CHANNELS);
      if (chans == 0) return;
      cps = (chan_info **)CALLOC(chans, sizeof(chan_info *));
      need_free = 1;
      chan = 0;
      for (i = 0; i < mix_infos_ctr; i++) 
	if ((mix_ok(i)) && 
	    (mix_infos[i]->track == track_num))
	  {
	    locp = mix_infos[i]->cp;
	    happy = 0;
	    for (j = 0; j < chan; j++) 
	      if (cps[j] == locp) 
		{
		  happy = 1; 
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
  if (chans == 0) return;
  outchans = chans;
  format = mus_audio_compatible_format(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss));
  datum_bytes = mus_data_format_to_bytes_per_sample(format);
  frames = 256;
#if MAC_OSX
  fds = (track_fd **)CALLOC(2, sizeof(track_fd *));
  buf = (float *)CALLOC(2 * frames, sizeof(float));
#else
  fds = (track_fd **)CALLOC(chans, sizeof(track_fd *));

  #if HAVE_ALSA
    /* in ALSA we have no way to tell what the possible output format is, or min chans, so... */
    mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_CHANNEL, 2, val);
    if (chans < (int)(val[1])) outchans = (int)(val[1]);
    mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, val);
    frames = (int)(val[0]);
    set_dac_size(ss, outchans * frames * mus_data_format_to_bytes_per_sample(format));
    buf = (mus_sample_t **)CALLOC(outchans, sizeof(mus_sample_t *));
    for (i = 0; i < chans; i++) buf[i] = (mus_sample_t *)CALLOC(frames, sizeof(mus_sample_t));
    outbuf = (char *)CALLOC(frames * datum_bytes * outchans, sizeof(char));

  #else
    buf = (short *)CALLOC(chans * frames, sizeof(short));
  #endif
#endif
  for (i = 0; i < chans; i++)
    {
      fds[i] = init_track_reader(cps[i], track_num, need_free);
      if (fds[i]) /* perhaps bad track number? */
	for (n = 0; n < fds[i]->mixes; n++)
	  {
	    j = fds[i]->state[n] + fds[i]->len[n];
	    if (j > samps) samps = j;
	  }
    }
  if (samps > 0)
    {
      playfd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), 
				     SND_SRATE(cps[0]->sound), 
				     outchans, 
				     format, 
				     dac_size(ss));
      if (playfd != -1)
	{
	  for (i = 0; i < samps; i += frames)
	    {
#if MAC_OSX
	      for (k = 0; k < chans; k++)
		if (fds[k])
		  for (j = k; j < frames * 2; j += 2)
		    buf[j] = next_track_sample(fds[k]);
	      mus_audio_write(playfd, (char *)buf, frames * datum_bytes * 2);
#else
  #if HAVE_ALSA
	      for (k = 0; k < chans; k++)
		if (fds[k])
		  for (j = 0; j < frames; j++)
		    buf[k][j] = MUS_FLOAT_TO_SAMPLE(next_track_sample(fds[k]));
	      mus_file_write_buffer(format, 0, frames - 1, outchans, buf, outbuf, TRUE);
	      mus_audio_write(playfd, outbuf, frames * datum_bytes * outchans);
  #else
	      for (k = 0; k < chans; k++)
		if (fds[k])
		  for (j = k; j < frames * chans; j += chans)
		    buf[j] = MUS_SAMPLE_TO_SHORT(MUS_FLOAT_TO_SAMPLE(next_track_sample(fds[k])));
	      mus_audio_write(playfd, (char *)buf, frames * datum_bytes * chans);
  #endif
#endif
	      check_for_event(ss);
	      if (ss->stopped_explicitly)
		{
		  ss->stopped_explicitly = 0;
		  report_in_minibuffer(cps[0]->sound, "stopped");
		  break;
		}
	    }
	  mus_audio_close(playfd);
	}
    }
  for (i = 0; i < chans; i++) free_track_fd(fds[i]);
  FREE(fds);
#if HAVE_ALSA
  for (i = 0; i < chans; i++) if (buf[i]) FREE(buf[i]);
  FREE(outbuf);
#endif
  FREE(buf);
  if (need_free) FREE(cps);
}

void reflect_mix_edit(chan_info *input_cp, const char *origin)
{
  /* input_cp is the underlying (mix input) channel */
  mix_info *md;
  console_state *cs;
  md = (mix_info *)(input_cp->mix_md);
  cs = md->current_cs;
  cs->mix_edit_ctr[input_cp->chan] = input_cp->edit_ctr;
  cs->len = CURRENT_SAMPLES(input_cp);
  remix_file(md, origin);
}

static void play_mix(snd_state *ss, mix_info *md)
{
  chan_info *cp;
  mix_fd *mf;
  console_state *cs;
#if MAC_OSX
  float *buf;
#else
  #if HAVE_ALSA
    mus_sample_t **buf;
    char *outbuf;
    Float val[4];
  #else
    short *buf;
  #endif
#endif
  int play_fd, j, format, datum_bytes, outchans, frames;
  off_t i, samps;
  if (md == NULL) return;
  cp = md->cp;
  cs = md->current_cs;
  samps = cs->len;
  format = mus_audio_compatible_format(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss));
  datum_bytes = mus_data_format_to_bytes_per_sample(format);
  outchans = 1;
  frames = 256;

#if MAC_OSX
  buf = (float *)CALLOC(2 * 256, sizeof(float));
#else
  #if HAVE_ALSA
    mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_CHANNEL, 2, val);
    if (outchans < (int)(val[1])) outchans = (int)(val[1]);
    mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, val);
    frames = (int)(val[0]);
    set_dac_size(ss, outchans * frames * mus_data_format_to_bytes_per_sample(format));
    buf = (mus_sample_t **)CALLOC(outchans, sizeof(mus_sample_t *));
    buf[0] = (mus_sample_t *)CALLOC(frames, sizeof(mus_sample_t));
    outbuf = (char *)CALLOC(frames * datum_bytes * outchans, sizeof(char));
  #else
    buf = (short *)CALLOC(frames, sizeof(short));
  #endif
#endif

  play_fd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss),
				  SND_SRATE(cp->sound),
				  outchans, 
				  format, 
				  dac_size(ss));
  if (play_fd != -1)
    {
      mf = init_mix_read(md, FALSE);
      if (mf)
	{
	  for (i = 0; i < samps; i += frames)
	    {
#if MAC_OSX
	      for (j = 0; j < 512; j += 2) 
		buf[j] = next_mix_sample(mf);
	      mus_audio_write(play_fd, (char *)buf, 2048);
#else
  #if HAVE_ALSA
	      for (j = 0; j < frames; j++)
		buf[0][j] = MUS_FLOAT_TO_SAMPLE(next_mix_sample(mf));
	      mus_file_write_buffer(format, 0, frames - 1, outchans, buf, outbuf, TRUE);
	      mus_audio_write(play_fd, outbuf, frames * datum_bytes * outchans);
  #else
	      for (j = 0; j < frames; j++) 
		buf[j] = MUS_SAMPLE_TO_SHORT(MUS_FLOAT_TO_SAMPLE(next_mix_sample(mf)));
	      mus_audio_write(play_fd, (char *)buf, frames * datum_bytes);
  #endif
#endif
	      check_for_event(ss);
	      if ((ss->stopped_explicitly) || (mix_play_stopped()))
		{
		  ss->stopped_explicitly = 0;
		  report_in_minibuffer(cp->sound, "stopped");
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
  play_mix(get_global_state(), md_from_id(mix_id));
}

static console_state *cs_from_id(int n)
{
  mix_info *md;
  md = md_from_id(n);
  if (md) return(md->current_cs);
  return(NULL);
}

static int mix_id_from_channel_position(chan_info *cp, off_t pos)
{
  int n;
  mix_info *md;
  console_state *cs; 
  if (cp->mixes)
    {
      for (n = 0; n < mix_infos_size; n++)
	{
	  md = mix_infos[n];
	  if ((md) && (md->cp == cp))
	    {
	      cs = md->current_cs;
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
  console_state *cs; 
  cs = cs_from_id(n); 
  if (cs) 
    return(cs->len); 
  return(-1);
}

static int set_mix_amp(int mix_id, int chan, Float val, int from_gui, int remix)
{
  mix_info *md;
  console_state *cs;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      if (val >= 0.0)
	{
	  if (md->in_chans > chan)
	    {
	      cs = md->current_cs;
	      cs->scalers[chan] = val;
	      if (!from_gui) 
		{
		  reflect_mix_in_mix_panel(mix_id);
		  remix_file(md, "set-" S_mix_amp);
		}
	      else
		{
		  if (remix)
		    {
		      if (!(call_mix_amp_changed_hook(md)))
			remix_file(md, "set-" S_mix_amp);
		    }
		  else make_temporary_graph(md->cp, md, cs);
		}
	    }
	  else return(INVALID_MIX_CHANNEL);
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

int set_mix_amp_from_id(int mix_id, int chan, Float val, int dragging)
{
  return(set_mix_amp(mix_id, chan, val, TRUE, !dragging));
}

Float mix_amp_from_id(int mix_id, int chan)
{
  mix_info *md;
  console_state *cs;
  md = md_from_id(mix_id);
  if (md)
    {
      if (md->in_chans > chan)
	{
	  cs = md->current_cs;
	  return(cs->scalers[chan]);
	}
    }
  return(0.0);
}

static int set_mix_speed(int mix_id, Float val, int from_gui, int remix)
{
  mix_info *md;
  snd_state *ss;
  char srcbuf[16];
  console_state *cs;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      if (val != 0.0)
	{
	  ss = md->ss;
	  cs = md->current_cs;
	  cs->speed = srate_changed(val, srcbuf, speed_control_style(ss), speed_control_tones(ss)); 
	  cs->len = (off_t)(ceil(md->in_samps / cs->speed));
	  if (!from_gui)
	    {
	      reflect_mix_in_mix_panel(mix_id);
	      remix_file(md, "set-" S_mix_speed);
	    }
	  else
	    {
	      if (remix)
		{
		  if (!(call_mix_speed_changed_hook(md)))
		    remix_file(md, "set-" S_mix_speed);
		}
	      else make_temporary_graph(md->cp, md, cs);
	    }
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

int set_mix_speed_from_id(int mix_id, Float val, int dragging) 
{
  return(set_mix_speed(mix_id, val, TRUE, !dragging));
}

Float mix_speed_from_id(int mix_id)
{
  console_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->speed);
  return(1.0);
}

void set_mix_track_from_id(int mix_id, int track)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) md->track = track;
}

int mix_track_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) return(md->track);
  return(0);
}

char *mix_name_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) return(md->name);
  return(NULL);
}

void set_mix_name_from_id(int mix_id, char *name)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) md->name = copy_string(name);
}

static int set_mix_position(int mix_id, off_t val, int from_gui)
{
  mix_info *md;
  console_state *cs = NULL;
  md = md_from_id(mix_id);
  if ((md) && (mix_ok_and_unlocked(mix_id)))
    {
      cs = md->current_cs;
      if (cs)
	{
	  if (val >= 0) 
	    cs->beg = val; 
	  else cs->beg = 0;
	  reflect_mix_in_mix_panel(mix_id);
	  if (!from_gui)
	    {
	      remix_file(md, "set-" S_mix_position); 
	    }
	  else
	    if (!(call_mix_position_changed_hook(md, cs->beg - cs->orig)))
	      remix_file(md, "set-" S_mix_position); 
	}
      return(mix_id);
    }
  return(INVALID_MIX_ID);
}

void set_mix_position_from_id(int mix_id, off_t beg)
{
  set_mix_position(mix_id, beg, FALSE);
}

off_t mix_position_from_id(int mix_id)
{
  console_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->beg);
  return(0);
}

int mix_input_chans_from_id(int mix_id)
{
  console_state *cs;
  cs = cs_from_id(mix_id);
  if (cs) return(cs->chans);
  return(0);
}

env *mix_amp_env_from_id(int n, int chan) 
{
  console_state *cs; 
  cs = cs_from_id(n); 
  if (cs) 
    {
      if (chan < cs->chans)
	{
	  if (cs->amp_envs)
	    return(cs->amp_envs[chan]);
	  else return(NULL);
	}
    }
  return(NULL);
}

env **mix_panel_envs(int n)
{
  mix_info *md;
  console_state *cs; 
  md = md_from_id(n);
  if (md) 
    {
      cs = md->current_cs;
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

env *mix_panel_env(int n, int chan)
{
  env **envs;
  envs = mix_panel_envs(n);
  if (envs) return(envs[chan]);
  return(NULL);
}

static int set_mix_amp_env_1(int n, int chan, env *val, int remix)
{
  mix_info *md;
  env *old_env = NULL, *old_panel_env = NULL;
  console_state *cs;
  md = md_from_id(n);
  if ((md) && (mix_ok_and_unlocked(n)))
    {
      if (chan == NO_SELECTION) chan = md->selected_chan;
      if (md->in_chans > chan)
	{
	  cs = md->current_cs;
	  if ((cs->amp_envs) && (cs->amp_envs[chan])) old_env = cs->amp_envs[chan];
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
	  if (remix) remix_file(md, "set-" S_mix_amp_env);
	  return(0);
	}
      else return(INVALID_MIX_CHANNEL);
    }
  else return(INVALID_MIX_ID);
}

int set_mix_amp_env(int n, int chan, env *val) {return(set_mix_amp_env_1(n, chan, val, TRUE));}
int set_mix_amp_env_without_edit(int n, int chan, env *val) {return(set_mix_amp_env_1(n, chan, val, FALSE));}

int mix_selected_channel(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->selected_chan);
  return(NO_SELECTION);
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
  #define H_mix_position "(" S_mix_position " id) -> sample number of start of mix"
  console_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_position, "an integer");
  cs = cs_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_position, n));
  return(C_TO_XEN_OFF_T(cs->orig)); 
}

static XEN g_mix_chans(XEN n) 
{
  #define H_mix_chans "(" S_mix_chans " id) -> (input) channels in mix"
  console_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_chans, "an integer");
  cs = cs_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_chans, n));
  return(C_TO_XEN_INT(cs->chans));
}

static XEN g_mix_p(XEN n) 
{
  #define H_mix_p "(" S_mix_p " id) -> #t if mix is active and accessible"
  if (XEN_INTEGER_P(n))
    return(C_TO_XEN_BOOLEAN(mix_ok(XEN_TO_C_INT_OR_ELSE(n, 0))));
  return(XEN_FALSE);
}

static XEN g_mix_frames(XEN n) 
{
  #define H_mix_frames "(" S_mix_frames " id) -> mix's length in samples"
  off_t len;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_frames, "an integer");
  len = mix_frames(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (len == -1) 
    return(snd_no_such_mix_error(S_mix_frames, n));
  return(C_TO_XEN_OFF_T(len));
}

static XEN g_mix_locked(XEN n) 
{
  #define H_mix_locked "(" S_mix_locked " id) -> #t if mix cannot be moved (due to subsequent edits overlapping it)"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_locked, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_locked, n));
  return(C_TO_XEN_BOOLEAN((md->current_cs)->locked));
}

static XEN g_mix_anchor(XEN n) 
{
  #define H_mix_anchor "(" S_mix_anchor " id) -> location of mix 'anchor' (determines console position within mix)"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_anchor, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_anchor, n));
  return(C_TO_XEN_OFF_T(md->anchor));
}

static XEN g_mix_name(XEN n) 
{
  #define H_mix_name "(" S_mix_name " id) -> name associated with mix"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_name, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_name, n));
  return(C_TO_XEN_STRING(md->name));
}

static XEN g_mix_track(XEN n) 
{
  #define H_mix_track "(" S_mix_track " id) -> track that mix is a member of"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_track, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_track, n));
  return(C_TO_XEN_INT(md->track));
}

static XEN g_mix_tag_y(XEN n) 
{
  #define H_mix_tag_y "(" S_mix_tag_y " id) -> height of mix's tag"
  mix_info *md; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_tag_y, "an integer");
  md = md_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (md == NULL)
    return(snd_no_such_mix_error(S_mix_tag_y, n));
  return(C_TO_XEN_INT(md->tag_y));
}

static XEN g_mix_speed(XEN n) 
{
  #define H_mix_speed "(" S_mix_speed " id) -> srate (speed slider setting) of mix"
  console_state *cs; 
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(n), n, XEN_ONLY_ARG, S_mix_speed, "an integer");
  cs = cs_from_id(XEN_TO_C_INT_OR_ELSE(n, 0));
  if (cs == NULL)
    return(snd_no_such_mix_error(S_mix_speed, n));
  return(C_TO_XEN_DOUBLE(cs->speed));
}

static XEN g_mixes(XEN snd, XEN chn)
{
  #define H_mixes "(" S_mixes ") -> list of mixes (ids) associated with snd and chn"
  snd_state *ss;
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
	  for (i = 0; i < mix_infos_ctr; i++)
	    if ((mix_ok(i)) && (mix_infos[i]->cp == cp))
	      res1 = XEN_CONS(C_TO_SMALL_XEN_INT(i), res1);
	}
      else
	{
	  sp = get_sp(snd);
	  if (sp == NULL) 
	    return(snd_no_such_sound_error(S_mixes, snd));
	  for (i = sp->nchans - 1; i >= 0; i--)
	    res1 = XEN_CONS(g_mixes(snd, C_TO_SMALL_XEN_INT(i)), res1);
	}
    }
  else
    {
      ss = get_global_state();
      for (j = ss->max_sounds - 1; j >= 0; j--)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse))
	    res1 = XEN_CONS(g_mixes(C_TO_SMALL_XEN_INT(j), 
				    XEN_UNDEFINED), 
			    res1);
	}
    }
  return(res1);
}

static XEN g_mix_home(XEN n) 
{
  #define H_mix_home "(" S_mix_home " id) -> list of index of sound and channel number affected by mix"
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
  #define H_mix_amp "(" S_mix_amp " id &optional (chan 0)) -> amp (console slider setting) of mix's channel chan"
  console_state *cs; 
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
  #define H_mix_amp_env "(" S_mix_amp_env " id &optional (chan 0)) -> amplitude envelope applied to mix's channel chan"
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
  XEN_ASSERT_TYPE(XEN_NUMBER_P(n), n, XEN_ARG_1, "set-" S_mix_position, "a number");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(uval), uval, XEN_ARG_2, "set-" S_mix_position, "a number");
  if (set_mix_position(XEN_TO_C_INT_OR_ELSE(n, 0), 
		       XEN_TO_C_OFF_T_OR_ELSE(uval, 0),
		       FALSE) == INVALID_MIX_ID)
    snd_no_such_mix_error("set-" S_mix_position, n);
  return(uval);
}

static XEN g_set_mix_frames(XEN n, XEN uval) 
{
  mix_info *md;
  off_t val;
  console_state *cs = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_frames, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(uval), uval, XEN_ARG_2, "set-" S_mix_frames, "a number");
  md = md_from_id(XEN_TO_C_INT(n));
  if ((md == NULL) || 
      (!(mix_ok_and_unlocked(md->id))))
    return(snd_no_such_mix_error("set-" S_mix_frames, n));
  cs = md->current_cs;
  if (cs)
    {
      val = XEN_TO_C_OFF_T_OR_ELSE(uval, 0);
      if (val >= 0)
	{
	  cs->len = val;
	  reflect_mix_in_mix_panel(md->id);
	  remix_file(md, "set-" S_mix_frames); 
	}
    }
  return(uval);
}

static XEN g_set_mix_locked(XEN n, XEN val) 
{
  console_state *cs;
  mix_info *md;
  int on;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_locked, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ARG_2, "set-" S_mix_locked, "a boolean");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error("set-" S_mix_locked, n));
  on = XEN_TO_C_BOOLEAN_OR_TRUE(val);
  cs = md->states[md->curcons];
  cs->locked = on;
  cs = md->current_cs;
  cs->locked = on;
  ss = get_global_state();
  if ((on) &&  (ss->selected_mix == md->id)) ss->selected_mix = INVALID_MIX_ID;
  reflect_mix_in_menu();
  reflect_mix_in_enved();
  display_channel_mixes(md->cp);
  return(val);
}

static XEN g_set_mix_anchor(XEN n, XEN uval) 
{
  mix_info *md;
  console_state *cs = NULL;
  off_t val;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_anchor, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(uval), uval, XEN_ARG_2, "set-" S_mix_anchor, "a number");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error("set-" S_mix_anchor, n));
  cs = md->current_cs;
  if (cs)
    {
      val = XEN_TO_C_OFF_T_OR_ELSE(uval, 0);
      if (val >= 0)
	{
	  md->anchor = val;
	  update_graph(md->cp);
	}
    }
  return(uval);
}

static XEN g_set_mix_name(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_name, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ARG_2, "set-" S_mix_name, "a string");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error("set-" S_mix_name, n));
  if (md->name) FREE(md->name);
  md->name = copy_string(XEN_TO_C_STRING(val));
  reflect_mix_in_mix_panel(md->id);
  return(val);
}

static XEN g_set_mix_track(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_track, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, "set-" S_mix_track, "an integer");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error("set-" S_mix_track, n));
  md->track = XEN_TO_C_INT(val);
  reflect_mix_in_mix_panel(md->id);
  return(val);
}

static XEN g_set_mix_tag_y(XEN n, XEN val) 
{
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_tag_y, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, "set-" S_mix_tag_y, "a number");
  md = md_from_id(XEN_TO_C_INT(n));
  if (md == NULL)
    return(snd_no_such_mix_error("set-" S_mix_tag_y, n));
  md->tag_y = XEN_TO_C_INT_OR_ELSE(val, 0);
  update_graph(md->cp);
  return(val);
}

static XEN g_set_mix_speed(XEN n, XEN uval) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_speed, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(uval), uval, XEN_ARG_2, "set-" S_mix_speed, "a number");
  if (set_mix_speed(XEN_TO_C_INT(n), XEN_TO_C_DOUBLE(uval), FALSE, TRUE) == INVALID_MIX_ID)
    snd_no_such_mix_error("set-" S_mix_speed, n);
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
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(uchan), uchan, XEN_ARG_2, "set-" S_mix_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(uval), uval, XEN_ARG_3, "set-" S_mix_amp, "a number");
  res = set_mix_amp(XEN_TO_C_INT(n), (XEN_BOUND_P(uchan)) ? XEN_TO_C_INT(uchan) : 0, XEN_TO_C_DOUBLE(uval), FALSE, TRUE);
  if (res == INVALID_MIX_ID)
    snd_no_such_mix_error("set-" S_mix_amp, n);
  else 
    if (res == INVALID_MIX_CHANNEL)
      snd_no_such_channel_error("set-" S_mix_amp, n, uchan);  
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
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, "set-" S_mix_amp_env, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, "set-" S_mix_amp_env, "an integer");
  XEN_ASSERT_TYPE(XEN_LIST_P(val), val, XEN_ARG_3, "set-" S_mix_amp_env, "a list");
  res = set_mix_amp_env(XEN_TO_C_INT(n), 
			(XEN_BOUND_P(chan)) ? XEN_TO_C_INT(chan) : 0,
			e = get_env(val, "set-" S_mix_amp_env));
  if (e) free_env(e);
  if (res == INVALID_MIX_ID)
    snd_no_such_mix_error("set-" S_mix_amp_env, n);
  else 
    if (res == INVALID_MIX_CHANNEL)
      snd_no_such_channel_error("set-" S_mix_amp_env, n, chan); 
  return(val);
}

static XEN g_mix_sound(XEN file, XEN start_samp)
{
  #define H_mix_sound "(" S_mix_sound " file start_samp) mixes file (all channels) into the currently selected sound at start_samp."

  char *filename;
  snd_state *ss;
  snd_info *sp;
  off_t beg, len = 0;
  int err = 0;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mix_sound, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(start_samp), start_samp, XEN_ARG_2, S_mix_sound, "a number");
  ss = get_global_state();
  sp = any_selected_sound(ss);  /* why not as arg?? -- apparently this is assuming CLM with-sound explode */
  if (sp == NULL) mus_misc_error(S_mix_sound, "no sound to mix into!", file);
  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  beg = XEN_TO_C_OFF_T_OR_ELSE(start_samp, 0);
  ss->catch_message = NULL;
  if (mus_file_probe(filename))
    {
      len = mus_sound_frames(filename);
      if (len > 0)
	err = mix(beg, len,
		  sp->nchans, sp->chans,
		  filename, DONT_DELETE_ME, S_mix_sound, with_mix_tags(ss)); 
    }
  else err = -1;
  if (filename) FREE(filename);
  if (err == -1) 
    {
      if (ss->catch_message)
	XEN_ERROR(MUS_MISC_ERROR,
		  XEN_LIST_2(C_TO_XEN_STRING(S_mix),
			     C_TO_XEN_STRING(ss->catch_message)));
      snd_no_such_file_error(S_mix_sound, file);
    }
  return(C_TO_XEN_INT(err));
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

static XEN g_mix_waveform_height(void) {snd_state *ss; ss = get_global_state(); return(C_TO_XEN_INT(mix_waveform_height(ss)));}
static XEN g_set_mix_waveform_height(XEN val) 
{
  #define H_mix_waveform_height "(" S_mix_waveform_height ") -> max height (pixels) of mix waveforms (20)"
  snd_state *ss; 
  int new_val[1];
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_mix_waveform_height, "a number"); 
  ss = get_global_state(); 
  new_val[0] = XEN_TO_C_INT_OR_ELSE(val, 0);
  in_set_mix_waveform_height(ss, new_val[0]);
  map_over_mixes(update_mix_waveform_height, (void *)new_val);
  for_each_chan(ss, update_mix_waveforms);
  return(C_TO_XEN_INT(mix_waveform_height(ss)));
}

#define S_mix_tag_position "mix-tag-position"
static XEN g_mix_tag_position(XEN id)
{
  #define H_mix_tag_position "(" S_mix_tag_position " id) returns position of mix tag"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id), id, XEN_ONLY_ARG, S_mix_tag_position, "an integer");
  md = md_from_id(XEN_TO_C_INT(id));
  if (md) 
    return(XEN_LIST_2(C_TO_XEN_INT(md->x),
		      C_TO_XEN_INT(md->y)));
  return(snd_no_such_mix_error(S_mix_tag_position, id));
}

static XEN g_mix_tag_width(void) {snd_state *ss; ss = get_global_state(); return(C_TO_XEN_INT(mix_tag_width(ss)));}
static XEN g_set_mix_tag_width(XEN val) 
{
  #define H_mix_tag_width "(" S_mix_tag_width ") -> width (pixels) of mix tags (6)"
  snd_state *ss; 
  int width;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_mix_tag_width, "a number"); 
  ss = get_global_state(); 
  width = XEN_TO_C_INT_OR_ELSE(val, DEFAULT_MIX_TAG_WIDTH);
  set_mix_tag_width(ss, width);
  for_each_chan(ss, update_graph);
  return(C_TO_XEN_INT(mix_tag_width(ss)));
}

static XEN g_mix_tag_height(void) {snd_state *ss; ss = get_global_state(); return(C_TO_XEN_INT(mix_tag_height(ss)));}
static XEN g_set_mix_tag_height(XEN val) 
{
  #define H_mix_tag_height "(" S_mix_tag_height ") -> height (pixels) of mix tags (14)"
  snd_state *ss; 
  int height;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set-" S_mix_tag_height, "a number"); 
  ss = get_global_state(); 
  height = XEN_TO_C_INT_OR_ELSE(val, DEFAULT_MIX_TAG_HEIGHT);
  set_mix_tag_height(ss, height);
  for_each_chan(ss, update_graph);
  return(C_TO_XEN_INT(mix_tag_height(ss)));
}

static XEN g_select_mix(XEN id)
{
  #define H_select_mix "(" S_select_mix " id) makes mix is the selected mix"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id) || XEN_FALSE_P(id), id, XEN_ONLY_ARG, S_select_mix, "an integer or #f");
  if (XEN_FALSE_P(id))
    ss->selected_mix = INVALID_MIX_ID;
  else select_mix(md_from_id(XEN_TO_C_INT(id)));
  return(id);
}

static XEN g_selected_mix(void)
{
  #define H_selected_mix "(" S_selected_mix ") -> the id of the currently selected mix"
  snd_state *ss;
  ss = get_global_state();
  if (ss->selected_mix != INVALID_MIX_ID)
    return(C_TO_SMALL_XEN_INT(ss->selected_mix));
  return(XEN_FALSE); /* changed 26-Mar-02 */
}

static XEN g_forward_mix(XEN count, XEN snd, XEN chn) 
{
  #define H_forward_mix "(" S_forward_mix " &optional (count 1) snd chn) moves the cursor forward count mixes, returns mix id if any"
  int val;
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(count), count, XEN_ARG_1, S_forward_mix, "an integer");
  ASSERT_CHANNEL(S_forward_mix, snd, chn, 2);
  cp = get_cp(snd, chn, S_forward_mix);
  val = XEN_TO_C_INT_OR_ELSE(count, 1); 
  goto_mix(cp, val);
  return(C_TO_XEN_INT(mix_id_from_channel_position(cp, cp->cursor)));
}

static XEN g_backward_mix(XEN count, XEN snd, XEN chn) 
{
  #define H_backward_mix "(" S_backward_mix " &optional (count 1) snd chn) moves the cursor back count mixes, returns mix id if any"
  int val; 
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(count), count, XEN_ARG_1, S_backward_mix, "an integer");
  ASSERT_CHANNEL(S_backward_mix, snd, chn, 2);
  cp = get_cp(snd, chn, S_backward_mix);
  val = -(XEN_TO_C_INT_OR_ELSE(count, 1)); 
  goto_mix(cp, val);
  return(C_TO_XEN_INT(mix_id_from_channel_position(cp, cp->cursor)));
}


static XEN g_mix(XEN file, XEN chn_samp_n, XEN file_chn, XEN snd_n, XEN chn_n, XEN console)
{
  #define H_mix "(" S_mix " file &optional (chn-start 0) (file-chan 0) snd chn with-console))\n\
mixes file channel file-chan into snd's channel chn starting at chn-start (or at the cursor location if chan-start \
is omitted), returning the new mix's id.  if with-console is #f, the data is mixed (no console is created). \
If chn is omitted, file's channels are mixed until snd runs out of channels.  If the file-to-be-mixed has \
no data, the 'id' value returned is -2, and no edit takes place."

  chan_info *cp = NULL;
  char *name = NULL;
  int chans, id = -1;
  int with_mixer = 1;
  snd_state *ss;
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mix, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(chn_samp_n), chn_samp_n, XEN_ARG_2, S_mix, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(file_chn), file_chn, XEN_ARG_3, S_mix, "an integer");
  ASSERT_CHANNEL(S_mix, snd_n, chn_n, 4);
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(console), console, XEN_ARG_6, S_mix, "a number");
  name = mus_expand_filename(XEN_TO_C_STRING(file));
  if (!(mus_file_probe(name)))
    {
      if (name) FREE(name);
      return(snd_no_such_file_error(S_mix, file));
    }
  ss = get_global_state();
  ss->catch_message = NULL;
  if (XEN_NOT_BOUND_P(console))
    with_mixer = with_mix_tags(ss);
  else with_mixer = XEN_TO_C_BOOLEAN_OR_TRUE(console);
  if (XEN_NOT_BOUND_P(chn_samp_n))
    {
      id = mix_complete_file(any_selected_sound(ss), name, S_mix, with_mixer);
      if (id == -1) 
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
      cp = get_cp(snd_n, chn_n, S_mix);
      chans = mus_sound_chans(name);
      if (chans > 0)
	{
	  ss->catch_message = NULL;
	  md = file_mix_samples(XEN_TO_C_OFF_T_OR_ELSE(chn_samp_n, 0),
				mus_sound_samples(name) / chans, 
				name,
				cp, 
				XEN_TO_C_INT_OR_ELSE(file_chn, 0),
				DONT_DELETE_ME, 
				S_mix,
				with_mixer);
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
static int mf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, mf_tag));}
#define TO_MIX_SAMPLE_READER(obj) ((mix_fd *)XEN_OBJECT_REF(obj))
#define MIX_SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, mf_tag)

static XEN g_mf_p(XEN obj) 
{
  #define H_mf_p "(" S_mix_sample_reader_p " obj) -> #t if obj is a mix-sample-reader"
  return(C_TO_XEN_BOOLEAN(mf_p(obj)));
}

static char *mf_to_string(mix_fd *fd) 
{
  mix_info *md;
  char *desc;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (fd == NULL)
    sprintf(desc, "#<mix-sample-reader: null>");
  else
    {
      md = fd->md;
      if ((md) && (MIX_TYPE_OK(fd->type)))
	mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<mix-sample-reader %p: %s via mix %d>",
		     fd,
		     md->in_filename,
		     md->id);
      else sprintf(desc, "#<mix-sample-reader: inactive>");
    }
  return(desc);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(mix_fd, print_mf, mf_to_string)

static void mf_free(mix_fd *fd)
{
  if (fd) free_mix_fd(fd); 
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(mix_fd, free_mf, mf_free)

static XEN g_make_mix_sample_reader(XEN mix_id)
{
  #define H_make_mix_sample_reader "(" S_make_mix_sample_reader " id) returns a reader ready to access mix 'id'"
  mix_info *md = NULL;
  mix_fd *mf = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(mix_id), mix_id, XEN_ONLY_ARG, S_make_mix_sample_reader, "an integer");
  md = md_from_id(XEN_TO_C_INT(mix_id));
  if (md == NULL)
    return(snd_no_such_mix_error(S_make_mix_sample_reader, mix_id));
  mf = init_mix_read(md, FALSE); 
  if (mf)
    {
      XEN_MAKE_AND_RETURN_OBJECT(mf_tag, mf, 0, free_mf);
    }
  return(XEN_FALSE);
}

static XEN g_next_mix_sample(XEN obj)
{
  #define H_next_mix_sample "(" S_next_mix_sample " reader) -> next sample from mix reader"
  XEN_ASSERT_TYPE(MIX_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_next_mix_sample, "a mix-sample-reader");
  return(C_TO_XEN_DOUBLE(next_mix_sample(TO_MIX_SAMPLE_READER(obj))));
}

static XEN g_read_mix_sample(XEN obj)
{
  #define H_read_mix_sample "(" S_read_mix_sample " reader) -> read sample from mix reader"
  XEN_ASSERT_TYPE(MIX_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_mix_sample, "a mix-sample-reader");
  return(C_TO_XEN_DOUBLE(next_mix_sample(TO_MIX_SAMPLE_READER(obj))));
}

static XEN g_free_mix_sample_reader(XEN obj)
{
  #define H_free_mix_sample_reader "(" S_free_mix_sample_reader " reader) frees mix sample reader 'reader'"
  mix_fd *mf;
  XEN_ASSERT_TYPE(MIX_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_free_mix_sample_reader, "a mix-sample-reader");
  mf = TO_MIX_SAMPLE_READER(obj);
  free_mix_fd_almost(mf);
  return(xen_return_first(XEN_FALSE, obj));
}



/* ---------------- track sample readers ---------------- */

static XEN_OBJECT_TYPE tf_tag;
static int tf_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, tf_tag));}
#define TO_TRACK_SAMPLE_READER(obj) ((track_fd *)XEN_OBJECT_REF(obj))
#define TRACK_SAMPLE_READER_P(Obj) XEN_OBJECT_TYPE_P(Obj, tf_tag)

static XEN g_tf_p(XEN obj) 
{
  #define H_tf_p "(" S_track_sample_reader_p " obj) -> #t if obj is a track-sample-reader"
  return(C_TO_XEN_BOOLEAN(tf_p(obj)));
}

static char *tf_to_string(track_fd *fd) 
{
  mix_info *md;
  mix_fd *mf = NULL;
  char *desc;
  char toi[16];
  int i, len;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (fd == NULL)
    sprintf(desc, "#<track-sample-reader: null>");
  else
    {
      if ((fd->fds) && (fd->mixes > 0))
	mf = fd->fds[0];
      if (mf == NULL)
	mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<track-sample-reader %p: inactive>", fd);
      else
	{
	  md = mf->md;
	  mus_snprintf(desc, PRINT_BUFFER_SIZE, "#<track-sample-reader %p: %s chan %d via mixes '(",
		       fd,
		       md->in_filename,
		       (md->cp)->chan);
	  len = fd->mixes;
	  if (len > 0)
	    {
	      for (i = 0; i < len - 1; i++)
		{
		  mf = fd->fds[i];
		  mus_snprintf(toi, 16, "%d ", (mf->md)->id);
		  strcat(desc, toi);
		}
	      mf = fd->fds[len - 1];
	      mus_snprintf(toi, 16, "%d)>", (mf->md)->id);
	      strcat(desc, toi);
	    }
	  else strcat(desc, ")>");
	}
    }
  return(desc);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(track_fd, print_tf, tf_to_string)

static void tf_free(track_fd *fd)
{
  if (fd) free_track_fd(fd); 
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(track_fd, free_tf, tf_free)

static XEN g_make_track_sample_reader(XEN track_id, XEN snd, XEN chn)
{
  #define H_make_track_sample_reader "(" S_make_track_sample_reader " track &optional snd chn)\n\
returns a reader ready to access track's data associated with snd's channel chn"

  track_fd *tf = NULL;
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(track_id), track_id, XEN_ARG_1, S_make_track_sample_reader, "an integer");
  ASSERT_CHANNEL(S_make_track_sample_reader, snd, chn, 2); 
  cp = get_cp(snd, chn, S_make_track_sample_reader);
  tf = init_track_reader(cp, 
			 XEN_TO_C_INT(track_id), 
			 FALSE); /* true to track all chans in parallel (assuming different starting points) */
  if (tf)
    {
      XEN_MAKE_AND_RETURN_OBJECT(tf_tag, tf, 0, free_tf);
    }
  XEN_ERROR(NO_SUCH_TRACK,
	    XEN_LIST_2(C_TO_XEN_STRING(S_make_track_sample_reader),
		       track_id));
  return(track_id);
}

static XEN g_next_track_sample(XEN obj)
{
  #define H_next_track_sample "(" S_next_track_sample " reader) -> next sample from track reader"
  XEN_ASSERT_TYPE(TRACK_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_next_track_sample, "a track-sample-reader");
  return(C_TO_XEN_DOUBLE(next_track_sample(TO_TRACK_SAMPLE_READER(obj))));
}

static XEN g_read_track_sample(XEN obj)
{
  #define H_read_track_sample "(" S_read_track_sample " reader) -> read sample from track reader"
  XEN_ASSERT_TYPE(TRACK_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_read_track_sample, "a track-sample-reader");
  return(C_TO_XEN_DOUBLE(next_track_sample(TO_TRACK_SAMPLE_READER(obj))));
}

static XEN g_free_track_sample_reader(XEN obj)
{
  #define H_free_track_sample_reader "(" S_free_track_sample_reader " reader) frees the track sample reader 'reader'"
  track_fd *tf = NULL;
  XEN_ASSERT_TYPE(TRACK_SAMPLE_READER_P(obj), obj, XEN_ONLY_ARG, S_free_track_sample_reader, "a track-sample-reader");
  tf = TO_TRACK_SAMPLE_READER(obj);
  free_track_fd_almost(tf);
  return(xen_return_first(XEN_FALSE, obj));
}

static XEN g_play_track(XEN num, XEN snd, XEN chn)
{
  #define H_play_track "(" S_play_track " track &optional snd chn) plays track"
  /* just a dummy for testing */
  chan_info *cp = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(num), num, XEN_ARG_1, S_play_track, "an integer");
  /* in this case if snd=#t, play all associated mixes in all chans */
  if (XEN_TRUE_P(snd))
    play_track(get_global_state(), NULL, 0, XEN_TO_C_INT_OR_ELSE(num, 0));
  else 
    {
      cp = get_cp(snd, chn, S_play_track);
      if (cp)
	play_track(cp->state, &cp, 1, XEN_TO_C_INT_OR_ELSE(num, 0));
    }
  return(num);
}

static XEN g_play_mix(XEN num)
{
  #define H_play_mix "(" S_play_mix " id) plays mix"
  mix_info *md;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ONLY_ARG, S_play_mix, "an integer");
  md = md_from_id(XEN_TO_C_INT(num));
  if (md == NULL)
    return(snd_no_such_mix_error(S_play_mix, num));
  play_mix(md->ss, md); 
  return(num);
}

static XEN multichannel_mix_hook;
static XEN mix_speed_changed_hook;
static XEN mix_amp_changed_hook;
static XEN mix_position_changed_hook;

static void call_multichannel_mix_hook(int *ids, int n)
{
  XEN lst = XEN_EMPTY_LIST;
  int i;
  /* create list from ids, pass to hook, if any */
  if (XEN_HOOKED(multichannel_mix_hook))
    {
      for (i = n - 1; i >= 0; i--)
	lst = XEN_CONS(C_TO_SMALL_XEN_INT(ids[i]), lst);
      run_hook(multichannel_mix_hook,
	       XEN_LIST_1(lst),
	       S_multichannel_mix_hook);
    }
}

static int call_mix_speed_changed_hook(mix_info *md)
{  
  XEN res = XEN_FALSE;
  if ((md) && 
      (XEN_HOOKED(mix_speed_changed_hook)))
    res = run_progn_hook(mix_speed_changed_hook,
			 XEN_LIST_1(C_TO_SMALL_XEN_INT(md->id)),
			 S_mix_speed_changed_hook);
  return(XEN_TRUE_P(res));
}

static int call_mix_amp_changed_hook(mix_info *md)
{  
  XEN res = XEN_FALSE;
  if ((md) && 
      (XEN_HOOKED(mix_amp_changed_hook)))
    res = run_progn_hook(mix_amp_changed_hook,
			 XEN_LIST_1(C_TO_SMALL_XEN_INT(md->id)),
			 S_mix_amp_changed_hook);
  return(XEN_TRUE_P(res));
}

static int call_mix_position_changed_hook(mix_info *md, off_t samps)
{  
  XEN res = XEN_FALSE;
  if ((md) && 
      (XEN_HOOKED(mix_position_changed_hook)))
    res = run_progn_hook(mix_position_changed_hook,
			 XEN_LIST_2(C_TO_SMALL_XEN_INT(md->id),
				    C_TO_XEN_OFF_T(samps)),
			 S_mix_position_changed_hook);
  return(XEN_TRUE_P(res));
}

#include "vct.h"

static XEN mix_vct(XEN obj, XEN beg, XEN snd, XEN chn, XEN with_tag, XEN origin)
{
  #define H_mix_vct "(" S_mix_vct " data &optional (beg 0) snd chn (with-tag #t) origin)\n\
mixes data (a vct object) into snd's channel chn starting at beg; returns the new mix id"

  vct *v;
  off_t bg;
  chan_info *cp;
  char *edname = NULL, *newname = NULL;
  snd_fd *sf;
  mus_sample_t *data;
  int i, len, mix_id = -1, with_mixer = 1;
  XEN_ASSERT_TYPE(VCT_P(obj), obj, XEN_ARG_1, S_mix_vct, "a vct");
  ASSERT_CHANNEL(S_mix_vct, snd, chn, 3);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(beg), beg, XEN_ARG_2, S_mix_vct, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(with_tag), with_tag, XEN_ARG_5, S_mix_vct, "a boolean");
  v = TO_VCT(obj);
  len = v->length;
  cp = get_cp(snd, chn, S_mix_vct);
  bg = XEN_TO_C_OFF_T_OR_ELSE(beg, 0);
  if (bg < 0)
    mus_misc_error(S_mix_vct, "beg < 0?", beg);
  else
    {
      if (XEN_NOT_BOUND_P(with_tag))
	with_mixer = with_mix_tags(cp->state);
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
	  newname = save_as_temp_file(&data, 1, len, SND_SRATE(cp->sound));
	  mix_id = mix(bg, len, 1, &cp, newname, DELETE_ME, (char *)((edname == NULL) ? S_mix_vct : edname), with_mixer);
	  if (!with_mixer) snd_remove(newname, TRUE);
	  FREE(newname);
	}
      update_graph(cp);
      FREE(data);
    }
  return(xen_return_first(C_TO_SMALL_XEN_INT(mix_id), obj));
}

static XEN g_find_mix(XEN samp_n, XEN snd_n, XEN chn_n) 
{
  #define H_find_mix "(" S_find_mix " samp &optional snd chn)\n\
finds the mix in snd's channel chn at samp, returning the mix id; returns #f if no mix found."

  int id;
  chan_info *cp = NULL;
  XEN_ASSERT_TYPE(XEN_OFF_T_P(samp_n) || XEN_NOT_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_find_mix, "a number");
  ASSERT_CHANNEL(S_find_mix, snd_n, chn_n, 2); 
  cp = get_cp(snd_n, chn_n, S_find_mix);
  id = mix_id_from_channel_position(cp, XEN_TO_C_OFF_T_OR_ELSE(samp_n, -1));
  if (id == INVALID_MIX_ID)
    return(XEN_FALSE);
  return(C_TO_XEN_INT(id));
}

static XEN g_set_mix_color (XEN arg1, XEN arg2)
{
  XEN color; 
  XEN mix_id = XEN_UNDEFINED;
  snd_state *ss;
  ss = get_global_state();
  if (XEN_NOT_BOUND_P(arg2))
    color = arg1;
  else
    {
      color = arg2;
      mix_id = arg1;
    }
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, "set-" S_mix_color, "a color"); 
  if (XEN_INTEGER_P(mix_id))
    color_one_mix_from_id(XEN_TO_SMALL_C_INT(mix_id), XEN_UNWRAP_PIXEL(color));
  else set_mix_color(ss, XEN_UNWRAP_PIXEL(color));
  for_each_chan(ss, update_graph);
  return(color);
}

static XEN g_mix_color(XEN mix_id) 
{
  #define H_mix_color "(" S_mix_color ") -> color of mix consoles"
  snd_state *ss;
  ss = get_global_state();
  if (XEN_INTEGER_P(mix_id))
    return(XEN_WRAP_PIXEL(mix_to_color_from_id(XEN_TO_SMALL_C_INT(mix_id))));
  return(XEN_WRAP_PIXEL((ss->sgx)->mix_color));
}

static XEN g_set_selected_mix_color (XEN color) 
{
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, "set-" S_selected_mix_color, "a color"); 
  set_selected_mix_color(ss, XEN_UNWRAP_PIXEL(color));
  for_each_chan(ss, update_graph);
  return(color);
}

static XEN g_selected_mix_color(void) 
{
  #define H_selected_mix_color "(" S_selected_mix_color ") -> color of the currently selected mix"
  snd_state *ss;
  ss = get_global_state();
  return(XEN_WRAP_PIXEL((ss->sgx)->selected_mix_color));
}



#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_make_mix_sample_reader_w, g_make_mix_sample_reader)
XEN_NARGIFY_1(g_read_mix_sample_w, g_read_mix_sample)
XEN_NARGIFY_1(g_next_mix_sample_w, g_next_mix_sample)
XEN_NARGIFY_1(g_free_mix_sample_reader_w, g_free_mix_sample_reader)
XEN_NARGIFY_1(g_mf_p_w, g_mf_p)
XEN_ARGIFY_3(g_make_track_sample_reader_w, g_make_track_sample_reader)
XEN_NARGIFY_1(g_next_track_sample_w, g_next_track_sample)
XEN_NARGIFY_1(g_read_track_sample_w, g_read_track_sample)
XEN_NARGIFY_1(g_free_track_sample_reader_w, g_free_track_sample_reader)
XEN_NARGIFY_1(g_tf_p_w, g_tf_p)
XEN_ARGIFY_1(g_play_mix_w, g_play_mix)
XEN_ARGIFY_3(g_play_track_w, g_play_track)
XEN_ARGIFY_1(g_mix_position_w, g_mix_position)
XEN_NARGIFY_2(g_set_mix_position_w, g_set_mix_position)
XEN_ARGIFY_1(g_mix_frames_w, g_mix_frames)
XEN_NARGIFY_2(g_set_mix_frames_w, g_set_mix_frames)
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
XEN_ARGIFY_1(g_mix_name_w, g_mix_name)
XEN_NARGIFY_2(g_set_mix_name_w, g_set_mix_name)
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
XEN_NARGIFY_2(g_mix_sound_w, g_mix_sound)
XEN_NARGIFY_1(g_select_mix_w, g_select_mix)
XEN_ARGIFY_3(g_find_mix_w, g_find_mix)
XEN_NARGIFY_0(g_selected_mix_w, g_selected_mix)
XEN_ARGIFY_3(g_forward_mix_w, g_forward_mix)
XEN_ARGIFY_3(g_backward_mix_w, g_backward_mix)
XEN_ARGIFY_6(g_mix_w, g_mix)
XEN_ARGIFY_6(mix_vct_w, mix_vct)
XEN_ARGIFY_1(g_mix_color_w, g_mix_color)
XEN_ARGIFY_2(g_set_mix_color_w, g_set_mix_color)
XEN_NARGIFY_0(g_selected_mix_color_w, g_selected_mix_color)
XEN_ARGIFY_1(g_set_selected_mix_color_w, g_set_selected_mix_color)
#else
#define g_make_mix_sample_reader_w g_make_mix_sample_reader
#define g_next_mix_sample_w g_next_mix_sample
#define g_read_mix_sample_w g_read_mix_sample
#define g_free_mix_sample_reader_w g_free_mix_sample_reader
#define g_mf_p_w g_mf_p
#define g_make_track_sample_reader_w g_make_track_sample_reader
#define g_next_track_sample_w g_next_track_sample
#define g_read_track_sample_w g_read_track_sample
#define g_free_track_sample_reader_w g_free_track_sample_reader
#define g_tf_p_w g_tf_p
#define g_play_mix_w g_play_mix
#define g_play_track_w g_play_track
#define g_mix_position_w g_mix_position
#define g_set_mix_position_w g_set_mix_position
#define g_mix_frames_w g_mix_frames
#define g_set_mix_frames_w g_set_mix_frames
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
#define g_mix_name_w g_mix_name
#define g_set_mix_name_w g_set_mix_name
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
#define g_mix_sound_w g_mix_sound
#define g_select_mix_w g_select_mix
#define g_find_mix_w g_find_mix
#define g_selected_mix_w g_selected_mix
#define g_forward_mix_w g_forward_mix
#define g_backward_mix_w g_backward_mix
#define g_mix_w g_mix
#define mix_vct_w mix_vct
#define g_mix_color_w g_mix_color
#define g_set_mix_color_w g_set_mix_color
#define g_selected_mix_color_w g_selected_mix_color
#define g_set_selected_mix_color_w g_set_selected_mix_color
#endif

void g_init_mix(void)
{
  mf_tag = XEN_MAKE_OBJECT_TYPE("MixSampleReader", sizeof(mix_fd));
  tf_tag = XEN_MAKE_OBJECT_TYPE("TrackSampleReader", sizeof(track_fd));

#if HAVE_GUILE
  scm_set_smob_print(mf_tag, print_mf);
  scm_set_smob_free(mf_tag, free_mf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(mf_tag, XEN_PROCEDURE_CAST g_read_mix_sample, 0, 0, 0);
#endif
#endif
#if HAVE_RUBY
  rb_define_method(mf_tag, "to_s", XEN_PROCEDURE_CAST print_mf, 0);
  rb_define_method(tf_tag, "to_s", XEN_PROCEDURE_CAST print_tf, 0);
#endif

  XEN_DEFINE_PROCEDURE(S_make_mix_sample_reader, g_make_mix_sample_reader_w, 1, 0, 0, H_make_mix_sample_reader);
  XEN_DEFINE_PROCEDURE(S_next_mix_sample,        g_next_mix_sample_w, 1, 0, 0,        H_next_mix_sample);
  XEN_DEFINE_PROCEDURE(S_read_mix_sample,        g_read_mix_sample_w, 1, 0, 0,        H_read_mix_sample);
  XEN_DEFINE_PROCEDURE(S_free_mix_sample_reader, g_free_mix_sample_reader_w, 1, 0, 0, H_free_mix_sample_reader);
  XEN_DEFINE_PROCEDURE(S_mix_sample_reader_p,    g_mf_p_w, 1, 0, 0,                   H_mf_p);

#if HAVE_GUILE
  scm_set_smob_print(tf_tag, print_tf);
  scm_set_smob_free(tf_tag, free_tf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(tf_tag, XEN_PROCEDURE_CAST g_read_track_sample, 0, 0, 0);
#endif
#endif

  XEN_DEFINE_PROCEDURE(S_make_track_sample_reader, g_make_track_sample_reader_w, 1, 2, 0, H_make_track_sample_reader);
  XEN_DEFINE_PROCEDURE(S_next_track_sample,        g_next_track_sample_w, 1, 0, 0,        H_next_track_sample);
  XEN_DEFINE_PROCEDURE(S_read_track_sample,        g_read_track_sample_w, 1, 0, 0,        H_read_track_sample);
  XEN_DEFINE_PROCEDURE(S_free_track_sample_reader, g_free_track_sample_reader_w, 1, 0, 0, H_free_track_sample_reader);
  XEN_DEFINE_PROCEDURE(S_track_sample_reader_p,    g_tf_p_w, 1, 0, 0,                     H_tf_p);
  XEN_DEFINE_PROCEDURE(S_play_mix,                 g_play_mix_w, 0, 1, 0,                 H_play_mix);
  XEN_DEFINE_PROCEDURE(S_play_track,               g_play_track_w, 1, 2, 0,               H_play_track);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_position, g_mix_position_w, H_mix_position, "set-" S_mix_position, g_set_mix_position_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_frames, g_mix_frames_w, H_mix_frames, "set-" S_mix_frames, g_set_mix_frames_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_locked, g_mix_locked_w, H_mix_locked, "set-" S_mix_locked, g_set_mix_locked_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_anchor, g_mix_anchor_w, H_mix_anchor, "set-" S_mix_anchor, g_set_mix_anchor_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_track, g_mix_track_w, H_mix_track, "set-" S_mix_track, g_set_mix_track_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_y, g_mix_tag_y_w, H_mix_tag_y, "set-" S_mix_tag_y, g_set_mix_tag_y_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_speed, g_mix_speed_w, H_mix_speed, "set-" S_mix_speed, g_set_mix_speed_w, 0, 1, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_name, g_mix_name_w, H_mix_name, "set-" S_mix_name, g_set_mix_name_w, 0, 1, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_waveform_height, g_mix_waveform_height_w, H_mix_waveform_height,
				   "set-" S_mix_waveform_height, g_set_mix_waveform_height_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_width, g_mix_tag_width_w, H_mix_tag_width,
				   "set-" S_mix_tag_width, g_set_mix_tag_width_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_tag_height, g_mix_tag_height_w, H_mix_tag_height,
				   "set-" S_mix_tag_height, g_set_mix_tag_height_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_color, g_mix_color_w, H_mix_color,
				   "set-" S_mix_color, g_set_mix_color_w,  0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_mix_color, g_selected_mix_color_w, H_selected_mix_color,
				   "set-" S_selected_mix_color, g_set_selected_mix_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_amp, g_mix_amp_w, H_mix_amp, "set-" S_mix_amp, g_set_mix_amp_w, 0, 2, 2, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_amp_env, g_mix_amp_env_w, H_mix_amp_env, "set-" S_mix_amp_env, g_set_mix_amp_env_w, 0, 2, 2, 1);

  XEN_DEFINE_PROCEDURE(S_mix_tag_position, g_mix_tag_position_w, 1, 0, 0, H_mix_tag_position);

  XEN_DEFINE_PROCEDURE(S_mix_chans,    g_mix_chans_w, 0, 1, 0,    H_mix_chans);
  XEN_DEFINE_PROCEDURE(S_mix_p,        g_mix_p_w, 0, 1, 0,        H_mix_p);
  XEN_DEFINE_PROCEDURE(S_mix_home,     g_mix_home_w, 0, 1, 0,     H_mix_home);
  XEN_DEFINE_PROCEDURE(S_mixes,        g_mixes_w, 0, 2, 0,        H_mixes);
  XEN_DEFINE_PROCEDURE(S_mix_sound,    g_mix_sound_w, 2, 0, 0,    H_mix_sound);
  XEN_DEFINE_PROCEDURE(S_select_mix,   g_select_mix_w, 1, 0, 0,   H_select_mix);
  XEN_DEFINE_PROCEDURE(S_find_mix,     g_find_mix_w, 0, 3, 0,     H_find_mix);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_mix, g_selected_mix_w, H_selected_mix, "set-" S_selected_mix, g_select_mix_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_forward_mix,  g_forward_mix_w, 0, 3, 0,  H_forward_mix);
  XEN_DEFINE_PROCEDURE(S_backward_mix, g_backward_mix_w, 0, 3, 0, H_backward_mix);
  XEN_DEFINE_PROCEDURE(S_mix,          g_mix_w, 1, 5, 0,          H_mix);
  XEN_DEFINE_PROCEDURE(S_mix_vct,      mix_vct_w, 1, 5, 0,        H_mix_vct);

  #define H_multichannel_mix_hook S_multichannel_mix_hook "(ids) is called when a multichannel mix happens in a sync'd sound. \
'ids' is a list of mix id numbers."

  #define H_mix_speed_changed_hook S_mix_speed_changed_hook " (mix-id) is called when a mix speed changes via the mouse. \
If it returns #t, the actual remix is the hook's responsibility."

  #define H_mix_amp_changed_hook S_mix_amp_changed_hook " (mix-id) is called when a mix amp changes via the mouse. \
If it returns #t, the actual remix is the hook's responsibility."

  #define H_mix_position_changed_hook S_mix_position_changed_hook " (mix-id samps) is called when a mix position changes via the mouse. \
'samps' = samples moved. If it returns #t, the actual remix is the hook's responsibility."

  XEN_DEFINE_HOOK(multichannel_mix_hook, S_multichannel_mix_hook, 1, H_multichannel_mix_hook);
  XEN_DEFINE_HOOK(mix_speed_changed_hook, S_mix_speed_changed_hook, 1, H_mix_speed_changed_hook);
  XEN_DEFINE_HOOK(mix_amp_changed_hook, S_mix_amp_changed_hook, 1, H_mix_amp_changed_hook);
  XEN_DEFINE_HOOK(mix_position_changed_hook, S_mix_position_changed_hook, 2, H_mix_position_changed_hook);

  #define H_select_mix_hook S_select_mix_hook " (id) is called when a mix is selected. \
The hook function argument 'id' is the newly selected mix's id."

  XEN_DEFINE_HOOK(select_mix_hook, S_select_mix_hook, 1, H_select_mix_hook); /* arg = newly selected mix id */

#if HAVE_GUILE
  XEN_EVAL_C_STRING("(define mix-sync mix-track)");
#endif
}



