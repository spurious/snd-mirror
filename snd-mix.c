#include "snd.h"

/* 
 * TODO enved waveform for mix is off and draws twice at start?
 */


typedef struct {         /* save one mix console state */
  int chans;             /* size of arrays in this struct */
  int edit_ctr;          /* cp edit_ctr at time of creation of this struct */
  int beg, end, orig, len;  /* samp positions in output (orig = where edit tree thinks it is) */
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
  int in_chans, in_samps;       /* in_samps needed to simplify speed changed duration calculations */
  int console_state_size;      /* current size of console_state list */
  console_state **states;      /* list of mixer states */
  console_state *current_cs;
  int anchor, orig_beg;         /* sample in in-data of console attachment */
  int curcons;
  int temporary;               /* in-filename was written by us and needs to be deleted when mix console is deleted */
  snd_info *add_snd;           /* readable snd_info struct for mix input */
  int id, x, nx, y, track, selected_chan, tagx, tagy, tag_y, height; 
                               /* number used in snd-scm calls, tag_y only for user set mix_tag_y */
} mix_info;


static mix_info *md_from_id(int n);
static void draw_mix_waveform(mix_info *md);
static void erase_mix_waveform(mix_info *md);

static int call_mix_speed_changed_hook(mix_info *md);
static int call_mix_amp_changed_hook(mix_info *md);
static int call_mix_position_changed_hook(mix_info *md, int samps);

chan_info *mix_channel_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) return(md->cp);
  return(NULL);
}


void color_one_mix_from_id(int mix_id, COLOR_TYPE color)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md) md->wg->color = color;
}

COLOR_TYPE mix_to_color_from_id(int mix_id)
{
  mix_info *md;
  md = md_from_id(mix_id);
  if (md)
    return(md->wg->color);
  return(NO_COLOR);
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
  return(g);
}

static mix_context *set_mix_info_context(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if (sp->combining != CHANNELS_SEPARATE) cp = sp->chans[0];
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

static console_state *make_console_state(int chans, int edit_ctr, int beg, int end)
{
  console_state *cs;
  cs = (console_state *)CALLOC(1, sizeof(console_state));
  cs->chans = chans;
  cs->edit_ctr = edit_ctr;
  cs->orig = beg;
  cs->beg = beg;
  cs->end = end;
  cs->len = end-beg+1;
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
    cur->mix_edit_ctr[i] = cs->mix_edit_ctr[i];
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
    {
      for (i = md->curcons+1; i < md->console_state_size; i++) 
	{
	  if (md->states[i]) 
	    md->states[i] = free_console_state(md->states[i]);
	}
    }
}



/* -------- mix_info (state of mix) -------- */

#define MIX_INFO_INCREMENT 16
static mix_info **mix_infos = NULL;
static int mix_infos_size = 0;
static int mix_infos_ctr = 0;

int mixes(void) {return(mix_infos_ctr);}

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
  if (mix_infos_ctr == 0) reflect_mix_in_enved();
  md->id = mix_infos_ctr++;
  md->cp = cp;
  md->ss = ss;
  md->add_snd = NULL;
  md->temporary = DONT_DELETE_ME;
  md->wg = set_mix_info_context(cp);
  md->wg->color = ss->sgx->mix_color;
  md->anchor = 0;
  md->y = 0;
  md->selected_chan = 0;
  md->height = mix_waveform_height(ss);
  return(md);
}

static mix_info *free_mix_info(mix_info *md)
{
  int i;
  snd_state *ss;
  if (md)
    {
      ss = md->ss;
      if (md->id == ss->selected_mix) ss->selected_mix = NO_SELECTION;
      if (md->wg) md->wg = free_mix_context(md->wg);
      mix_infos[md->id] = NULL;
      if (md->temporary == DELETE_ME) 
	{
	  mus_sound_forget(md->in_filename); 
	  remove(md->in_filename);
	}
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

static void select_mix(mix_info *md)
{
  mix_info *old_md = NULL;
  snd_state *ss;
  ss = get_global_state();
  if ((ss->selected_mix != NO_SELECTION) && 
      ((md == NULL) || (ss->selected_mix != md->id)))
    old_md = md_from_id(ss->selected_mix);
  if (md) 
    ss->selected_mix = md->id; 
  else ss->selected_mix = NO_SELECTION;
  if ((old_md) && (old_md->cp->show_mix_waveforms)) 
    draw_mix_waveform(old_md);
  if (md)
    {
      if (md->cp->show_mix_waveforms) 
	draw_mix_waveform(md);
      reflect_mix_in_mix_panel(md->id);
    }
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
  int *ctr, *samples;
  MUS_SAMPLE_TYPE **idata;
  Float samps_per_bin;
} mix_fd;

static env_info *make_mix_input_amp_env(chan_info *cp)
{
  env_state *es;
  if (current_ed_samples(cp) > AMP_ENV_CUTOFF)
    {
      es = make_env_state(cp, current_ed_samples(cp)); /* sets cp->amp_envs[pos] */
      while (!(tick_amp_env(cp, es)));
      if (es->sf) es->sf = free_snd_fd(es->sf);
      FREE(es);
      return(cp->amp_envs[cp->edit_ctr]);
    }
  return(NULL);
}


static snd_info *make_mix_readable(mix_info *md)
{
  chan_info *cp;
  int i;
  snd_info *add_sp;
  if (md == NULL) return(NULL);
  if (!md->add_snd) 
    {
      cp = md->cp;
      md->add_snd = make_sound_readable(cp->state, md->in_filename, TRUE);
      if (!(md->add_snd)) 
	snd_error("can't find %s", md->in_filename);
      else
	{
	  add_sp = md->add_snd;
	  add_sp->fullname = copy_string(md->in_filename);
	  add_sp->shortname = filename_without_home_directory(add_sp->fullname);
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
  sp = make_mix_readable(md);
  if (sp)
    {
      for (i = 0; i < sp->nchans; i++)
	{
	  cp = sp->chans[i];
	  if ((cp == NULL) || (cp->amp_envs == NULL)) return(FALSE);
	  ep = cp->amp_envs[cp->edit_ctr];
	  if ((ep == NULL) && (current_ed_samples(cp) > AMP_ENV_CUTOFF))
	    ep = make_mix_input_amp_env(cp);
	  if ((ep) && (samps_per_bin == 0.0)) samps_per_bin = ep->samps_per_bin;
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

#define MIX_INPUT_SOUND 0
#define MIX_INPUT_AMP_ENV 1
 
static Float next_mix_input_amp_env_sample(mix_fd *mf, int chan)
{
  if (mf->ctr[chan] < mf->samples[chan])
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
	return(next_sample_to_float(mf->sfs[mf->base]));
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
		samp = next_sample_to_float(mf->sfs[i]);
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
		sum += (next_sample_to_float(mf->sfs[i]) * cs->scalers[i]);
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
		{
		  if (cs->scalers[i] > 0.0)
		    {
		      if (mf->segs[i])
			sum += (mus_env(mf->segs[i]) * run_src(mf->srcs[i], mf->sr));
		      else sum += (cs->scalers[i] * run_src(mf->srcs[i], mf->sr));
		    }
		}
	    }
	  else
	    {
	      for (i = 0; i < mf->chans; i++)
		if (cs->scalers[i] > 0.0)
		  sum += (cs->scalers[i] * run_src(mf->srcs[i], mf->sr));
	    }
	}
      else
	{
	  spd = mf->sr;
	  if (mf->segs)
	    {
	      for (i = 0; i < mf->chans; i++)
		{
		  if (cs->scalers[i] > 0.0)
		    {
		      if (mf->segs[i])
			sum += (mus_env(mf->segs[i]) * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
		      else sum += (cs->scalers[i] * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
		    }
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
		{
		  for (i = 0; i < mf->chans; i++)
		    {
		      mf->lst[i] = mf->nxt[i];
		      if (mf->type == MIX_INPUT_SOUND)
			mf->nxt[i] = next_sample_to_float(mf->sfs[i]);
		      else mf->nxt[i] = next_mix_input_amp_env_sample(mf, i);
		    }
		}
	    }
	}
      break;
    }
  return(sum);
}

static int amp_env_len(mix_info *md, int chan)
{
  return(current_ed_samples((md->add_snd)->chans[chan]));
}

static mix_fd *init_mix_read_1(mix_info *md, int old, int type)
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
	    mf->srcs[i] = make_src(md->ss, 0.0, mf->sfs[i]);
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
		  mf->lst[i] = next_sample_to_float(mf->sfs[i]);
		  mf->nxt[i] = next_sample_to_float(mf->sfs[i]);
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
  return(init_mix_read_1(md, old, MIX_INPUT_SOUND));
}

static mix_fd *init_mix_input_amp_env_read(mix_info *md, int old, int hi)
{
  int i;
  mix_fd *mf = NULL;
  snd_info *sp;
  chan_info *cp;
  env_info *ep;
  mf = init_mix_read_1(md, old, MIX_INPUT_AMP_ENV);
  if (!mf) return(NULL);
  sp = md->add_snd;
  mf->ctr = (int *)CALLOC(sp->nchans, sizeof(int));
  mf->samples = (int *)CALLOC(sp->nchans, sizeof(int));
  mf->idata = (MUS_SAMPLE_TYPE **)CALLOC(sp->nchans, sizeof(MUS_SAMPLE_TYPE *));
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      mf->ctr[i] = -1; /* preincremented */
      mf->samples[i] = current_ed_samples(cp);
      ep = cp->amp_envs[cp->edit_ctr];
      mf->samps_per_bin = ep->samps_per_bin;
      if (hi)
	mf->idata[i] = ep->data_max;
      else mf->idata[i] = ep->data_min;
    }
  return(mf);
}

static mix_fd *free_mix_fd(mix_fd *mf)
{
  int i;
  if (mf)
    {
      if (mf->lst) FREE(mf->lst);
      if (mf->nxt) FREE(mf->nxt);
      if (mf->sfs)
	{
	  for (i = 0; i < mf->chans; i++)
	    if (mf->sfs[i]) 
	      mf->sfs[i] = free_snd_fd(mf->sfs[i]);
	  FREE(mf->sfs);
	  mf->sfs = NULL;
	}
      if (mf->ctr) FREE(mf->ctr); mf->ctr = NULL; 
      if (mf->samples) FREE(mf->samples); mf->samples = NULL; 
      if (mf->idata) FREE(mf->idata); mf->idata = NULL; 
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
      FREE(mf);
    }
  return(NULL);
}



/* ---------------- MIXING ---------------- */

static int remove_temporary_mix_file(mix_info *md, void *ptr)
{
  if (md->temporary == DELETE_ME) 
    {
      mus_sound_forget(md->in_filename); 
      remove(md->in_filename);
    }
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

int disk_space_p(snd_info *sp, int fd, int bytes, int other_bytes)
{
  int kfree, kneeded, kother, go_on;
  kfree = disk_kspace(fd);
  if (kfree < 0) 
    {
      report_in_minibuffer_and_save(sp, strerror(errno)); 
      return(NO_PROBLEM);
    }
  kneeded = bytes >> 10;
  if (kfree < kneeded)
    {
      if (other_bytes > 0)
	{
	  kother = other_bytes >> 10;
	  if (kother > kfree)
	    {
	      report_in_minibuffer_and_save(sp, "only %d Kbytes left on disk, changing to 16-bit temp output", kfree);
	      return(HUNKER_DOWN);
	    }
	}
      go_on = snd_yes_or_no_p(sp->state, "only %d Kbytes left on disk; continue?", kfree);
      if (!go_on) return(GIVE_UP);
      report_in_minibuffer(sp, "ok -- here we go...");
      return(BLIND_LEAP);
    }
  return(NO_PROBLEM);
}

/* TODO: throw for snd_error? */

static char *save_as_temp_file(MUS_SAMPLE_TYPE **raw_data, int chans, int len, int nominal_srate)
{
  char *newname;
  snd_state *ss;
  int format, ofd, no_space, hfd;
  format = MUS_OUT_FORMAT;
  ss = get_global_state();
  newname = shorter_tempnam(temp_dir(ss), "snd_");
  /* we're writing our own private version of this thing, so we can use our own formats */
  hfd = snd_write_header(ss, newname, MUS_NEXT, nominal_srate, chans, 28, len*chans, format, NULL, 0, NULL);
  if (hfd == -1) return(NULL);
  ofd = snd_reopen_write(ss, newname);
  mus_file_set_descriptors(ofd, newname, format, 4, 28, chans, MUS_NEXT);
  mus_file_set_data_clipped(ofd, data_clipped(ss));
  lseek(ofd, 28, SEEK_SET);
  no_space = disk_space_p(any_selected_sound(ss), ofd, len*chans*4, 0);
  if (no_space == GIVE_UP)
    {
      if (mus_file_close(ofd) != 0)
	snd_error("can't close %d (%s): %s! [%s[%d] %s]",
		  ofd, newname,
		  strerror(errno),
		  __FILE__, __LINE__, __FUNCTION__);
      return(newname);
    }
  mus_file_write(ofd, 0, len-1, chans, raw_data);
  if (mus_file_close(ofd) != 0)
    snd_error("can't close %d (%s): %s! [%s[%d] %s]",
	      ofd, newname,
	      strerror(errno),
	      __FILE__, __LINE__, __FUNCTION__);
  return(newname);
}

#define OFFSET_FROM_TOP 0
/* axis top border width is 10 (snd-axis.c) */

static mix_info *add_mix(chan_info *cp, int chan, int beg, int num, 
			char *full_original_file, 
			int input_chans, int temp)
{ /* temp -> delete original file also */
  mix_info *md;
  char *namebuf;
  console_state *cs;
  reflect_mix_active_in_menu();
  md = make_mix_info(cp);     /* add active mix to chan_info list */
  md->in_chans = input_chans;
  md->orig_beg = beg;
  md->in_samps = num;
  if (md->id < 100)
    namebuf = (char *)CALLOC(8, sizeof(char));
  else namebuf = (char *)CALLOC(12, sizeof(char));
  sprintf(namebuf, "mix%d", md->id);
  md->name = namebuf;
  md->temporary = temp;
  md->console_state_size = 1;
  md->states = (console_state **)CALLOC(md->console_state_size, sizeof(console_state *));
  cs = make_console_state(input_chans, cp->edit_ctr, beg, beg+num-1);
  md->current_cs = make_console_state(input_chans, cp->edit_ctr, beg, beg+num-1);
  md->states[0] = cs;
  if (chan < input_chans)
    cs->scalers[chan] = 1.0;
  cs->speed = 1.0;
  md->curcons = 0;
  make_current_console(md);
  md->in_filename = copy_string(full_original_file);
  update_graph(cp, NULL);
  return(md);
}

static mix_info *file_mix_samples(int beg, int num, char *tempfile, chan_info *cp, int chan, int temp, char *origin, int with_tag)
{
  /* open tempfile, current data, write to new temp file mixed, close others, open and use new as change case */
  /* used for clip-region temp file incoming and C-q in snd-chn.c (i.e. mix in file) so sync not relevant */
  snd_fd *csf;
  snd_state *ss;
  snd_info *sp;
  int ofd, ifd;
  char *ofile;
  MUS_SAMPLE_TYPE **data;
  Float scaler;
  MUS_SAMPLE_TYPE *chandata;
  int i, size, j, cursamps, in_chans, base, no_space, len, err = 0;
  file_info *ihdr, *ohdr;
  if (num <= 0) 
    {
      snd_error("mix %s which has %d samples?", tempfile, num);
      return(NULL);
    }

  len = current_ed_samples(cp);
  if (beg >= len)
    extend_with_zeros(cp, len, beg - len + 1, "(mix-extend)");
  /* might set flag here that we need backup after file_mix_samples below (backup_edit_list(cp)) */
  /* otherwise user sees unexplained mix-extend in edit history list */
  if (beg < 0) beg = 0;
  sp = cp->sound;
  ss = cp->state;
  ihdr = make_file_info(tempfile, ss);
  if (!ihdr) return(NULL);
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
  ohdr = make_temp_header(ofile, SND_SRATE(sp), 1, 0);
  ofd = open_temp_file(ofile, 1, ohdr, ss);
  if (ofd == -1) 
    {
      snd_error("mix temp file %s: %s", ofile, strerror(errno)); 
      return(NULL);
    }
  no_space = disk_space_p(cp->sound, ofd, num * 4, 0);
  if (no_space == GIVE_UP)
    {
      mus_file_close(ofd);
      mus_sound_forget(ofile);
      remove(ofile);
      free(ofile);
      return(NULL);
    }
  csf = init_sample_read(beg, cp, READ_FORWARD);
  ifd = snd_open_read(ss, tempfile);
  mus_file_set_descriptors(ifd, tempfile,
			   ihdr->format,
			   mus_data_format_to_bytes_per_sample(ihdr->format),
			   ihdr->data_location,
			   ihdr->chans,
			   ihdr->type);
#if HAVE_HOOKS
  during_open(ifd, tempfile, SND_MIX_FILE);
#endif
  if (num < MAX_BUFFER_SIZE) size = num; else size = MAX_BUFFER_SIZE;
  data = (MUS_SAMPLE_TYPE **)CALLOC(in_chans, sizeof(MUS_SAMPLE_TYPE *));
  data[base] = (MUS_SAMPLE_TYPE *)CALLOC(size, sizeof(MUS_SAMPLE_TYPE));
  chandata = data[base];
  mus_file_seek(ofd, ohdr->data_location, SEEK_SET);
  mus_file_seek(ifd, ihdr->data_location, SEEK_SET);
  if (scaler == 0.0)
    {
      for (i = 0; i < num; i += MAX_BUFFER_SIZE)
	{
	  cursamps = num-i;
	  if (cursamps > MAX_BUFFER_SIZE) cursamps = MAX_BUFFER_SIZE;
	  for (j = 0; j < cursamps; j++)
	    chandata[j] = next_sample(csf);
	  err = mus_file_write(ofd, 0, cursamps - 1, 1, &chandata);
	  if (err == -1) break;
	}
    }
  else
    {
      mus_file_read_chans(ifd, 0, size - 1, in_chans, data, (MUS_SAMPLE_TYPE *)data);
      for (i = 0, j = 0; i < num; i++)
	{
	  if (j == size)
	    {
	      err = mus_file_write(ofd, 0, size - 1, 1, &chandata);
	      mus_file_read_chans(ifd, 0, size - 1, in_chans, data, (MUS_SAMPLE_TYPE *)data);
	      j = 0;
	      if (err == -1) break;
	    }
	  chandata[j] += next_sample(csf);
	  j++;
	}
      if (j > 1) mus_file_write(ofd, 0, j - 1, 1, &chandata);
    }
  close_temp_file(ofd, ohdr, num * mus_data_format_to_bytes_per_sample(ohdr->format), sp);
  mus_file_close(ifd);
  free_snd_fd(csf);
  FREE(data[base]);
  FREE(data);
  free_file_info(ihdr);
  free_file_info(ohdr);
  file_change_samples(beg, num, ofile, cp, 0, DELETE_ME, DONT_LOCK_MIXES, origin);
  if (with_tag)
    return(add_mix(cp, chan, beg, num, tempfile, in_chans, temp));
  else return(NULL);
}

/* next three functions canonicalize mixer input -- 
 *   all end up with a filename containing the original to-be-mixed input
 *                     length (per channel samples in input)
 *                     begin sample in output for mix start
 *                     an array of cps for mixing into
 *                     a notion of initial scalers (1.0 or arg to mix_array)
 */

#if HAVE_GUILE
static void call_multichannel_mix_hook(int *ids, int n);
#else
void call_multichannel_mix_hook(int *ids, int n) {}
#endif

static int mix(int beg, int num, int chans, chan_info **cps, char *mixinfile, int temp, char *origin, int with_tag)
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
	  if ((sp) && (sp->syncing)) ids[j++] = md->id;
	}
    }
  if (j > 1) call_multichannel_mix_hook(ids, j);
  FREE(ids);
  return(id);
}

int mix_array(int beg, int num, MUS_SAMPLE_TYPE **data, chan_info **out_cps, int in_chans, int out_chans, int nominal_srate, char *origin, int with_tag)
{
  /* always write to tempfile */
  char *newname;
  int id = -1;
  /* this seems excessive -- why not just change_samples? */
  newname = save_as_temp_file(data, in_chans, num, nominal_srate);
  if (newname) 
    {
      id = mix(beg, num, out_chans, out_cps, newname, DELETE_ME, origin, with_tag);
      if (with_tag == 0) remove(newname);
      FREE(newname);
    }
  else
    snd_error("can't save mix temp array in file (%s: %s)", newname, strerror(errno));
  return(id);
}

int copy_file_and_mix(int beg, int num, char *file, chan_info **cps, int out_chans, char *origin, int with_tag)
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

int mix_file_and_delete(int beg, int num, char *file, chan_info **cps, int out_chans, char *origin, int with_tag)
{
  return(mix(beg, num, out_chans, cps, file, DELETE_ME, origin, with_tag));
}

int mix_complete_file(snd_info *sp, char *str, char *origin, int with_tag)
{
  /* no need to save as temp here, but we do need sync info (from menu and keyboard) */
  chan_info *cp;
  chan_info **cps = NULL;
  int nc, len, chans, id = -1;
  sync_info *si = NULL;
  char *fullname = NULL;
  if ((sp) && (str) && (*str))
    {
      clear_minibuffer(sp);
      fullname = mus_file_full_name(str);
      nc = mus_sound_chans(fullname);
      if (nc != -1)
	{
	  len = mus_sound_samples(fullname)/nc;
	  cp = any_selected_channel(sp);
	  if (sp->syncing != 0)
	    {
	      si = snd_sync(sp->state, sp->syncing); 
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


static void remix_file(mix_info *md, char *origin)
{
  int beg, end, i, j, ofd = 0, size, num, no_space, use_temp_file;
  Float val, maxy, miny;
  snd_info *cursp;
  mix_fd *add, *sub;
  snd_fd *cur, *sfb, *afb;
  snd_state *ss;
  char *ofile = NULL;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *chandata;
  file_info *ohdr = NULL;
  axis_info *ap;
  chan_info *cp;
  int old_beg, old_end, new_beg, new_end, err = 0;
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

  add = init_mix_read(md, 0);
  if (!add) return;
  sub = init_mix_read(md, 1);
  if (!sub) return;
  cur = init_sample_read(beg, cp, READ_FORWARD);

  if (use_temp_file)
    {
      ofile = snd_tempnam(ss);
      ohdr = make_temp_header(ofile, SND_SRATE(cursp), 1, 0);
      ofd = open_temp_file(ofile, 1, ohdr, ss);
      no_space = disk_space_p(cursp, ofd, num * 4, num * 2);
      switch (no_space)
	{
	case GIVE_UP:
	  close_temp_file(ofd, ohdr, 0, cursp);
	  free_snd_fd(cur);
	  free_mix_fd(add);
	  free_mix_fd(sub);
	  free_file_info(ohdr);
	  mus_sound_forget(ofile);
	  remove(ofile);
	  free(ofile);
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
  if (num < MAX_BUFFER_SIZE) size = num; else size = MAX_BUFFER_SIZE;
  data = (MUS_SAMPLE_TYPE **)CALLOC(1, sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(size, sizeof(MUS_SAMPLE_TYPE));
  chandata = data[0];
  if (use_temp_file) mus_file_seek(ofd, ohdr->data_location, SEEK_SET);

  old_beg -= beg;
  old_end -= beg;
  new_beg -= beg;
  new_end -= beg;

  /* these max/min values are only used to reset y-axis limits if overflow occurred */
  maxy = ap->ymax;
  miny = ap->ymin;

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
	  chandata[j++] = next_sample(cur);
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
	      val = next_sample_to_float(cur);
	      if ((i >= old_beg) && (i <= old_end))
		val -= next_sample_to_float(sfb);
	      if ((i >= new_beg) && (i <= new_end))
		val += next_sample_to_float(afb);
	      if (val > maxy) maxy = val;
	      else if (val < miny) miny = val;
	      chandata[j++] = MUS_FLOAT_TO_SAMPLE(val);
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
	      val = next_sample_to_float(cur);
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
    file_change_samples(beg, num, ofile, cp, 0, DELETE_ME, DONT_LOCK_MIXES, origin);
  else change_samples(beg, num, data[0], cp, DONT_LOCK_MIXES, origin);
  FREE(data[0]);
  FREE(data);
 
  extend_console_list(md);
  cs = copy_console(cs);
  cs->edit_ctr = cp->edit_ctr;
  cs->orig = new_beg+beg;
  cs->beg = cs->orig;
  cs->end = cs->beg + cs->len - 1;

  md->states[md->curcons] = cs;
  make_current_console(md);

  /* fix up graph if we overflowed during mix */
  if ((maxy > ap->ymax) || (miny < ap->ymin)) 
    {
      if (maxy < -miny) maxy = -miny; 
      ap->y0 = -maxy;
      ap->y1 = maxy;
      ap->ymin = -maxy;
      ap->ymax = maxy;
      ap->y_ambit = (ap->ymax - ap->ymin);
    }
  update_graph(cp, NULL);
}

/* ---------------- MIX GRAPHS ---------------- */

/* these are copies from snd-axis.c; didn't want to use macros here */
static inline short local_grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((short)(ap->x_base + val * ap->x_scale));
}

static inline short local_grf_y(Float val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((short)(ap->y_base + val * ap->y_scale));
}


static int make_temporary_amp_env_mixed_graph(chan_info *cp, axis_info *ap, mix_info *md, Float samples_per_pixel,
					      int newbeg, int newend, int oldbeg, int oldend)
{
  /* temp graph using cp->amp_env and mix (sample-by-sample) data */
  Float main_start, new_start, old_start;
  mix_fd *new_fd, *old_fd;
  Float val, new_ymin, new_ymax, old_ymin, old_ymax, main_ymin, main_ymax;
  Float xi, xf, xfinc;
  int lastx, lo, hi, main_loc, j, i;
  env_info *ep;

  lo = ap->losamp;
  hi = ap->hisamp;

  new_fd = init_mix_read(md, 0);
  if (!new_fd) return(0);
  old_fd = init_mix_read(md, 1);
  if (!old_fd) return(0);

  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((Float)(ap->losamp) / (Float)(ep->samps_per_bin));
  main_start = (Float)ap->losamp;
  
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

  for (j = 0, xi = (Float)lo, xf = ap->x0; 
       xi < (Float)hi; 
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
					int newbeg, int newend, int oldbeg, int oldend)
{
  /* temp graph using cp->amp_env and mix input amp envs */
  Float main_start, new_start, old_start, val;
  mix_fd *new_min_fd, *new_max_fd, *old_min_fd, *old_max_fd;
  Float new_ymin, new_ymax, old_ymin, old_ymax, main_ymin, main_ymax;
  Float new_high, new_low, old_high, old_low;
  Float xi, xf, xfinc, x;
  int lastx, lo, hi, main_loc, j;
  env_info *ep;

  lo = ap->losamp;
  hi = ap->hisamp;

  new_min_fd = init_mix_input_amp_env_read(md, 0, 0); /* not old, not hi */
  new_max_fd = init_mix_input_amp_env_read(md, 0, 1); /* not old, hi */
  old_min_fd = init_mix_input_amp_env_read(md, 1, 0); /* old, not hi */
  old_max_fd = init_mix_input_amp_env_read(md, 1, 1); /* old, hi */

  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((Float)(ap->losamp) / (Float)(ep->samps_per_bin));
  main_start = (Float)ap->losamp;

  if (lo > newbeg) 
    {
      for (x = (Float)lo; x < (Float)newbeg; x += new_max_fd->samps_per_bin) 
	{
	  new_low = next_mix_sample(new_min_fd);
	  new_high = next_mix_sample(new_max_fd);
	}
      new_ymin = new_low;
      new_ymax = new_high;
      new_start = (Float)lo;
    }
  else 
    {
      new_ymin = 0.0;
      new_ymax = 0.0;
      new_start = (Float)newbeg;
    }

  if ((lo > oldbeg) && (oldend > lo))
    {
      for (x = (Float)lo; x < (Float)oldbeg; x += old_max_fd->samps_per_bin) 
	{
	  old_low = next_mix_sample(old_min_fd);
	  old_high = next_mix_sample(old_max_fd);
	}
      old_ymin = old_low;
      old_ymax = old_high;
      old_start = (Float)lo;
    }
  else 
    {
      old_start = (Float)oldbeg;
      old_ymin = 0.0;
      old_ymax = 0.0;
    }

  lastx = ap->x_axis_x0;
  xfinc = samples_per_pixel / (Float)SND_SRATE(cp->sound);

  for (j = 0, xi = (Float)lo, xf = ap->x0; 
       xi < (Float)hi; 
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
  int oldbeg, newbeg, oldend, newend;
  int i, j, samps, xi;
  int widely_spaced;
  axis_info *ap;
  snd_info *sp;
  mix_context *ms;
  snd_state *ss;
  Float samples_per_pixel, xf;
  double x, incr, initial_x;
  Float ina, ymin, ymax;
  int lo, hi;
  snd_fd *sf = NULL, *sfb, *afb;
  mix_fd *add = NULL, *sub = NULL;
  int x_start, x_end;
  double start_time, cur_srate;
  ss = cp->state;
  if (!(movies(ss))) return;
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
  samps = ap->hisamp-ap->losamp + 1;
  samples_per_pixel = (Float)(samps - 1) / (Float)(x_end - x_start);
  if ((samples_per_pixel < 5.0) && 
      (samps < POINT_BUFFER_SIZE))
    {
      sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
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
	  ina = next_sample_to_float(sf);
	  if ((i >= oldbeg) && (i <= oldend)) ina -= next_mix_sample(sub);
	  if ((i >= newbeg) && (i <= newend)) ina += next_mix_sample(add);
	  if (widely_spaced)
	    set_grf_point((int)x, j, local_grf_y(ina, ap));
	  else set_grf_point(local_grf_x(x, ap), j, local_grf_y(ina, ap));
	}
      erase_and_draw_grf_points(md->wg, cp, j);
    }
  else
    {
      if (amp_env_usable(cp, samples_per_pixel, ap->hisamp, TRUE))
	{
	  if (mix_input_amp_env_usable(md, samples_per_pixel))
	    j = make_temporary_amp_env_graph(cp, ap, md, samples_per_pixel, newbeg, newend, oldbeg, oldend);
	  else j = make_temporary_amp_env_mixed_graph(cp, ap, md, samples_per_pixel, newbeg, newend, oldbeg, oldend);
	}
      else
	{
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
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
	      while (i <= hi)
		{
		  ina = next_sample_to_float(sf);
		  if (ina > ymax) ymax = ina;
		  if (ina < ymin) ymin = ina;
		  xf+=1.0;
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
	  else
	    {
	      if ((add->calc == C_STRAIGHT) && (sub->calc == C_STRAIGHT))
		{
		  sfb = sub->sfs[sub->base];
		  afb = add->sfs[add->base];
		  while (i <= hi)
		    {
		      ina = next_sample_to_float(sf);
		      if ((i >= oldbeg) && (i <= oldend)) ina -= next_sample_to_float(sfb);
		      if ((i >= newbeg) && (i <= newend)) ina += next_sample_to_float(afb);
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
	      else
		{
		  while (i <= hi)
		    {
		      ina = next_sample_to_float(sf);
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

static int display_mix_amp_env(mix_info *md, Float scl, int yoff, int newbeg, int newend, chan_info *cp, axis_info *ap, int draw)
{
  /* need min and max readers */
  snd_state *ss;
  mix_fd *min_fd, *max_fd;
  int hi, lo, j, lastx, newx;
  Float ymin, ymax, high = 0.0, low = 0.0;
  Float sum, xend, xstart, xstep;
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
      for (sum = (Float)lo; sum < (Float)newbeg; sum += max_fd->samps_per_bin) 
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
      xstart = (Float)(newbeg)/(Float)SND_SRATE(cp->sound);
      ymin = 100.0;
      ymax = -100.0;
    }

  if (hi < newend)
    xend = ap->x1;
  else xend = (Float)(newend)/(Float)SND_SRATE(cp->sound);
  xstep = (Float)(max_fd->samps_per_bin) / (Float)SND_SRATE(cp->sound);
  lastx = local_grf_x(xstart, ap);
  for (j = 0; xstart < xend; xstart += xstep)
    {
      low = next_mix_sample(min_fd);
      high = next_mix_sample(max_fd);
      newx = local_grf_x(xstart, ap);
      if (newx > lastx)
	{
	  set_grf_points(lastx, j,
			 (int)(yoff - scl * ymin),
			 (int)(yoff - scl * ymax));
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
			 MAIN_GRAPH_STYLE(cp));
  else draw_both_grf_points(cp, erase_context(cp), j, MAIN_GRAPH_STYLE(cp));
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
		   md->tagx - width, md->tagy-height / 2,
		   width, height);
    
  fill_rectangle(ax,
		 md->x - width, md->y-height / 2,
		 width, height);
  md->tagx = md->x;
  md->tagy = md->y;
}

static BACKGROUND_FUNCTION_TYPE watch_mix_proc = 0;    /* work proc if mouse outside graph causing axes to move */

static int display_mix_waveform(chan_info *cp, mix_info *md, console_state *cs, int draw)
{
  snd_state *ss;
  int newbeg, newend, endi;
  Float scl;
  int i, j = 0, samps, xi;
  int widely_spaced;
  axis_info *ap;
  snd_info *sp;
  int yoff;
  Float samples_per_pixel, xf;
  double x, incr, initial_x;
  Float ina, ymin, ymax;
  int lo, hi;
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
  samps = ap->hisamp-ap->losamp + 1;
  samples_per_pixel = (Float)(samps - 1) / (Float)(x_end - x_start);
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
      for (j = 0, i = newbeg; i <= newend; i++, j++, x+=incr)
	{
	  ina = next_mix_sample(add);
	  if (widely_spaced)
	    set_grf_point((int)x, j, (int)(yoff - scl * ina));
	  else set_grf_point(local_grf_x(x, ap), j, (int)(yoff - scl * ina));
	}
      if (sp)
	{
	  if (draw)
	    draw_grf_points(cp,
			    (md->id == ss->selected_mix) ? selected_mix_waveform_context(cp) : unselected_mix_waveform_context(cp, md),
			    j, ap, 
			    ungrf_y(ap, yoff), 
			    MAIN_GRAPH_STYLE(cp));
	  else draw_grf_points(cp, 
			       erase_context(cp), 
			       j, ap, 
			       ungrf_y(ap, yoff), 
			       MAIN_GRAPH_STYLE(cp));
	}
    }
  else
    {
      if (mix_input_amp_env_usable(md, samples_per_pixel))
	j = display_mix_amp_env(md, scl, yoff, newbeg, newend, cp, ap, draw);
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
				 (int)(yoff - scl * ymin),
				 (int)(yoff - scl * ymax));
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
				     MAIN_GRAPH_STYLE(cp));
	      else draw_both_grf_points(cp, 
					erase_context(cp), 
					j, 
					MAIN_GRAPH_STYLE(cp));
	    }
	}
    }
  if (sp) copy_context(cp);
  if (add) free_mix_fd(add);
  return(j);
}

int display_mix_waveform_at_zero(chan_info *cp, int mix_id)
{
  int pts;
  console_state *cs;
  mix_info *md;
  int old_beg;
  md = md_from_id(mix_id);
  cs = md->current_cs;
  old_beg = cs->beg;
  cs->beg = 0;
  pts = display_mix_waveform(cp, md, cs, TRUE);
  cs->beg = old_beg;
  return(pts);
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
  int samps_moved = 0;
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
  my = md->y - height/2;
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

int mix_dragging(void) {return(mix_dragged);}          /* for snd-xchn.c */

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
  int samps, nx, x, samp, updated = 0;
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
      samp = (int)(ungrf_x(ap, nx) * SND_SRATE(cp->sound));
      if (samp < 0) samp = 0;
      samps = current_ed_samples(cp);
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
  int lo, hi, spot, i, curmaxctr = -1;
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

int mix_beg(chan_info *cp)
{
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
  int lo, hi, spot, i, xspot, turnover, y, combined, hgt;
  combined = (((snd_info *)(cp->sound))->combining != CHANNELS_SEPARATE);
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

static int lt_beg, lt_end;

static int lock_mixes(mix_info *md, void *ptr)
{
  console_state *cs;
  chan_info *cp;
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
	  cp = md->cp;
	  cs->edit_ctr = cp->edit_ctr;
	  cs = md->current_cs;
	  cs->locked = 1;
	}
    }
  return(0);
}

void lock_affected_mixes(chan_info *cp, int beg, int end)
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
  int beg, change;
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

void ripple_mixes(chan_info *cp, int beg, int change)
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
  int mx1, mx2;
  mx1 = (*((int *)umx1));
  mx2 = (*((int *)umx2));
  if (mx1 > mx2) return(1);
  if (mx1 == mx2) return(0);
  return(-1);
}

int goto_mix(chan_info *cp, int count)
{
  int i, j, k, samp;
  mix_info *md;
  int *css;
  console_state *cs;
  if ((!cp) || (!cp->mixes)) return(CURSOR_IN_VIEW);
  k = 0;
  for (i = 0; i < mix_infos_ctr; i++)
    {
      md = mix_infos[i];
      if ((md) && (md->cp == cp)) k++;
    }
  if (k > 0)
    {
      css = (int *)CALLOC(k, sizeof(int));
      j = 0;
      for (i = 0; i < mix_infos_ctr; i++)
	{
	  md = mix_infos[i];
	  if ((md) && (md->cp == cp))
	    {
	      cs = md->current_cs;
	      css[j] = cs->orig;
	      j++;
	    }
	}
      qsort((void *)css, j, sizeof(int), compare_consoles);
      /* now find where we are via cp->cursor and go forward or back as per count */
      samp = cp->cursor;
      k = j - 1;
      for (i = 0; i < j; i++)
	if (css[i] > samp) 
	  {
	    k = i; 
	    break;
	  }
      if (css[k] != samp) 
	{
	  if (count < 0) 
	    k++; 
	  else k--;
	}
      k += count;
      if (k < 0) k = 0;
      if (k >= j) k = j - 1;
      samp = css[k];
      cp->cursor = samp;
      FREE(css);
      if ((count > 0) && (samp > (cp->axis)->hisamp)) return(CURSOR_IN_MIDDLE);
      if ((count < 0) && (samp < (cp->axis)->losamp)) return(CURSOR_IN_MIDDLE);
      return(CURSOR_IN_VIEW);
    }
  return(CURSOR_IN_VIEW);
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

int any_mix_id(void)
{
  int i;
  for (i = 0; i < mix_infos_ctr; i++) 
    if (mix_ok(i)) 
      return(i);
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
  int *state, *len;
  mix_fd **fds;
} track_fd;

static track_fd *init_track_reader(chan_info *cp, int track_num, int global) /* edit-position? direction? */
{
  track_fd *fd = NULL;
  int mixes = 0, i, mix, track_beg;
  mix_info *md;
  console_state *cs;
  track_beg = current_ed_samples(cp);
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
      fd->state = (int *)CALLOC(mixes, sizeof(int));
      fd->len = (int *)CALLOC(mixes, sizeof(int));
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

static void free_track_fd(track_fd *fd)
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
	}
      if (fd->state) FREE(fd->state);
      if (fd->len) FREE(fd->len);
      FREE(fd);
    }
}

static MUS_SAMPLE_TYPE next_track_sample(track_fd *fd)
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
  return(MUS_FLOAT_TO_SAMPLE(sum));
}

static void play_track(snd_state *ss, chan_info **ucps, int chans, int track_num)
{
  track_fd **fds;
  chan_info **cps;
  chan_info *locp;
  int playfd, i, j, k, n, samps, chan = 0, happy = 0, need_free = 0;
  short *buf;
  if (ucps == NULL)
    {
      chans = active_channels(ss, WITH_VIRTUAL_CHANNELS);
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
  fds = (track_fd **)CALLOC(chans, sizeof(track_fd *));
  buf = (short *)CALLOC(chans * 256, sizeof(short));
  for (i = 0; i < chans; i++)
    {
      fds[i] = init_track_reader(cps[i], track_num, need_free);
      for (n = 0; n < fds[i]->mixes; n++)
	{
	  j = fds[i]->state[n] + fds[i]->len[n];
	  if (j > samps) samps = j;
	}
    }
  playfd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), 
				 SND_SRATE(cps[0]->sound), 
				 chans, 
				 MUS_COMPATIBLE_FORMAT, 
				 dac_size(ss));
  for (i = 0; i < samps; i+=256)
    {
      for (k = 0; k < chans; k++)
	for (j = k; j < 256*chans; j+=chans)
	  buf[j] = MUS_SAMPLE_TO_SHORT(next_track_sample(fds[k]));
      mus_audio_write(playfd, (char *)buf, 256 * 2 * chans);
      check_for_event(ss);
      if (ss->stopped_explicitly)
	{
	  ss->stopped_explicitly = 0;
	  report_in_minibuffer(cps[0]->sound, "stopped");
	  break;
	}
    }
  mus_audio_close(playfd);
  for (i = 0; i < chans; i++) free_track_fd(fds[i]);
  FREE(fds);
  FREE(buf);
  if (need_free) FREE(cps);
}

void reflect_mix_edit(chan_info *input_cp, char *origin)
{
  /* input_cp is the underlying (mix input) channel */
  mix_info *md;
  console_state *cs;
  md = (mix_info *)(input_cp->mix_md);
  cs = md->current_cs;
  cs->mix_edit_ctr[input_cp->chan] = input_cp->edit_ctr;
  cs->len = input_cp->samples[input_cp->edit_ctr];
  remix_file(md, origin);
}

static void play_mix(snd_state *ss, mix_info *md)
{
  chan_info *cp;
  mix_fd *mf;
  console_state *cs;
  short *buf;
  int play_fd, i, j, samps;
  if (md == NULL) return;
  cp = md->cp;
  cs = md->current_cs;
  samps = cs->len;
  play_fd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss),
				  SND_SRATE(cp->sound),
				  1, 
				  MUS_COMPATIBLE_FORMAT, 
				  dac_size(ss));
  if (play_fd != -1)
    {
      mf = init_mix_read(md, FALSE);
      if (mf)
	{
	  buf = (short *)CALLOC(256, sizeof(short));
	  if (buf)
	    {
	      for (i = 0; i < samps; i+=256)
		{
		  for (j = 0; j < 256; j++) 
		    buf[j] = MUS_SAMPLE_TO_SHORT(MUS_FLOAT_TO_SAMPLE(next_mix_sample(mf)));
		  mus_audio_write(play_fd, (char *)buf, 512);
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
	      FREE(buf);
	    }
	  free_mix_fd(mf);
	}
      if (play_fd != -1) mus_audio_close(play_fd);
    }
  reflect_mix_play_stop();
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

int mix_length(int n) 
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
  if (md)
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
  if (md)
    {
      if (val != 0.0)
	{
	  ss = md->ss;
	  cs = md->current_cs;
	  cs->speed = srate_changed(val, srcbuf, speed_style(ss), speed_tones(ss)); 
	  cs->len = (int)(ceil(md->in_samps / cs->speed));
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

static int set_mix_position(int mix_id, int val, int from_gui)
{
  /* should a locked mix be settable? Currently it is. */
  mix_info *md;
  console_state *cs = NULL;
  md = md_from_id(mix_id);
  if (md)
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

void set_mix_position_from_id(int mix_id, int beg)
{
  set_mix_position(mix_id, beg, FALSE);
}

int mix_position_from_id(int mix_id)
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

int set_mix_amp_env(int n, int chan, env *val)
{
  mix_info *md;
  console_state *cs;
  md = md_from_id(n);
  if (md)
    {
      if (chan == NO_SELECTION) chan = md->selected_chan;
      if (md->in_chans > chan)
	{
	  cs = md->current_cs;
	  if ((cs->amp_envs) && (cs->amp_envs[chan])) free_env(cs->amp_envs[chan]);
	  if (cs->amp_envs == NULL) cs->amp_envs = (env **)CALLOC(cs->chans, sizeof(env *));
	  cs->amp_envs[chan] = copy_env(val);
	  remix_file(md, "set-" S_mix_amp_env);
	  return(0);
	}
      else return(INVALID_MIX_CHANNEL);
    }
  else return(INVALID_MIX_ID);
}

int mix_selected_channel(int id)
{
  mix_info *md;
  md = md_from_id(id);
  if (md)
    return(md->selected_chan);
  return(NO_SELECTION);
}

static env *flat_env = NULL;

void display_mix_amp_envs(snd_state *ss, chan_info *axis_cp, axis_context *ax, int width, int height)
{
  axis_info *ap;
  int chans, chan, mix_id;
  env *e;
  int i, j;
  Float ex0, ey0, ex1, ey1, val;
  int ix0, ix1, iy0, iy1;
  Float flat[4];

  mix_id = current_mix_id(ss);
  chans = mix_input_chans_from_id(mix_id);

  for (chan = 0; chan < chans; chan++)
    {
      e = mix_amp_env_from_id(mix_id, chan);
      if (!e)
	{
	  if (!flat_env)
	    {
	      flat[0] = 0.0; flat[1] = 1.0; flat[2] = 1.0; flat[3] = 1.0;
	      flat_env = make_envelope(flat, 4);
	    }
	  e = flat_env;
	}

      ex0 = e->data[0];
      ey0 = e->data[1];
      ex1 = e->data[(e->pts * 2) - 2];
      ey1 = ey0;
      for (i = 3; i < e->pts*2; i+=2)
	{
	  val = e->data[i];
	  if (ey0 > val) ey0 = val;
	  if (ey1 < val) ey1 = val;
	}
      if (ey0 > 0.0) ey0 = 0.0;
      if ((ey0 == ey1) && (ey1 == 0.0)) ey1 = 1.0; /* fixup degenerate case */
      if (ey1 < 1.0) ey1 = 1.0;

      init_env_axes(axis_cp, "mix env",
		    (int)(chan * width / chans),
		    (int)ex0, (int)ey0,
		    width/chans, height,
		    ex0, ex1, ey0, ey1);
      ap = axis_cp->axis;

      ix1 = local_grf_x(e->data[0], ap);
      iy1 = local_grf_y(e->data[1], ap);
      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
	{
	  ix0 = ix1;
	  iy0 = iy1;
	  ix1 = local_grf_x(e->data[i], ap);
	  iy1 = local_grf_y(e->data[i+1], ap);
	  draw_line(ax, ix0, iy0, ix1, iy1);
	}
    }
}


/* -------------------------------- SCM connection -------------------------------- */
#if HAVE_GUILE

static SCM g_mix_position(SCM n) 
{
  #define H_mix_position "(" S_mix_position " id) -> sample number of start of mix"
  console_state *cs; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_position);
  cs = cs_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (cs) return(TO_SCM_INT(cs->orig)); 
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_position),
			     n)));
}

static SCM g_mix_chans(SCM n) 
{
  #define H_mix_chans "(" S_mix_chans " id) -> (input) channels in mix"
  console_state *cs; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_chans);
  cs = cs_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (cs) return(TO_SCM_INT(cs->chans));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_chans),
			     n)));
}

static SCM g_mixQ(SCM n) 
{
  #define H_mixQ "(" S_mixQ " id) -> #t if mix is active and accessible"
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mixQ);
  return(TO_SCM_BOOLEAN(mix_ok(TO_C_INT_OR_ELSE(n, 0))));
}

static SCM g_mix_length(SCM n) 
{
  #define H_mix_length "(" S_mix_length " id) -> length (frames) of mix"
  int len;
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_length);
  len = mix_length(TO_C_INT_OR_ELSE(n, 0));
  if (len == -1) 
    return(scm_throw(NO_SUCH_MIX,
		     SCM_LIST2(TO_SCM_STRING(S_mix_length),
			       n)));
  return(TO_SCM_INT(len));
}

static SCM g_mix_locked(SCM n) 
{
  #define H_mix_locked "(" S_mix_locked " id) -> #t if mix cannot be moved (due to subsequent edits overlapping it)"
  mix_info *md;
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_locked);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md) return(TO_SCM_BOOLEAN((md->current_cs)->locked));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_locked),
			     n)));
}

static SCM g_mix_anchor(SCM n) 
{
  #define H_mix_anchor "(" S_mix_anchor " id) -> location of mix 'anchor' (determines console position within mix)"
  mix_info *md; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_anchor);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md) return(TO_SCM_INT(md->anchor));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_anchor),
			     n)));
}

static SCM g_mix_name(SCM n) 
{
  #define H_mix_name "(" S_mix_name " id) -> name associated with mix"
  mix_info *md; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_name);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md) return(TO_SCM_STRING(md->name));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_name),
			     n)));
}

static SCM g_mix_track(SCM n) 
{
  #define H_mix_track "(" S_mix_track " id) -> track that mix is a member of"
  mix_info *md; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_track);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md) return(TO_SCM_INT(md->track));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_track),
			     n)));
}

static SCM g_mix_tag_y(SCM n) 
{
  #define H_mix_tag_y "(" S_mix_tag_y " id) -> height of mix's tag"
  mix_info *md; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_tag_y);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md) return(TO_SCM_INT(md->tag_y));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_tag_y),
			     n)));
}

static SCM g_mix_speed(SCM n) 
{
  #define H_mix_speed "(" S_mix_speed " id) -> srate (speed slider setting) of mix"
  console_state *cs; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_speed);
  cs = cs_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (cs) return(TO_SCM_DOUBLE(cs->speed));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_speed),
			     n)));
}

static SCM g_mixes(SCM snd, SCM chn)
{
  #define H_mixes "(" S_mixes ") -> list of mixes (ids) associated with snd and chn"
  snd_state *ss;
  snd_info *sp;
  chan_info *cp;
  int i, j;
  SCM res1 = SCM_EOL;
  if (SCM_INUMP(snd))
    {
      if (SCM_INUMP(chn))
	{
	  /* scan all mixes for any associated with this channel */
	  cp = get_cp(snd, chn, S_mixes);
	  for (i = 0; i < mix_infos_ctr; i++)
	    if ((mix_ok(i)) && (mix_infos[i]->cp == cp))
	      res1 = gh_cons(TO_SMALL_SCM_INT(i), res1);
	}
      else
	{
	  sp = get_sp(snd);
	  if (sp == NULL) 
	    return(scm_throw(NO_SUCH_SOUND,
			     SCM_LIST2(TO_SCM_STRING(S_mixes),
				       snd)));
	  for (i = sp->nchans - 1; i >= 0; i--)
	    res1 = gh_cons(g_mixes(snd, TO_SMALL_SCM_INT(i)), res1);
	}
    }
  else
    {
      ss = get_global_state();
      for (j = ss->max_sounds - 1; j >= 0; j--)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse))
	    res1 = gh_cons(g_mixes(TO_SMALL_SCM_INT(j), SCM_UNDEFINED), res1);
	}
    }
  return(res1);
}

static SCM g_mix_sound_index(SCM n) 
{
  #define H_mix_sound_index "(" S_mix_sound_index " id) -> index of sound affected by mix"
  mix_info *md; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_sound_index);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md) return(TO_SCM_INT(((md->cp)->sound)->index));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_sound_index),
			     n)));
}

static SCM g_mix_sound_channel(SCM n) 
{
  #define H_mix_sound_channel "(" S_mix_sound_channel " id) -> channel affected by mix"
  mix_info *md; 
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_sound_channel);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md) return(TO_SCM_INT((md->cp)->chan));
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_sound_channel),
			     n)));
}

/* these last two refer to the mix output location, not the underlying mix (input) data */

static SCM g_mix_amp(SCM n, SCM uchan) 
{
  #define H_mix_amp "(" S_mix_amp " id &optional (chan 0)) -> amp (console slider setting) of mix's channel chan"
  console_state *cs; 
  int chan;
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_amp);
  SCM_ASSERT(bool_or_arg_p(uchan), uchan, SCM_ARG2, S_mix_amp);
  cs = cs_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (cs) 
    {
      chan = TO_C_INT_OR_ELSE(uchan, 0);
      if (chan < cs->chans) return(TO_SCM_DOUBLE(cs->scalers[chan]));
      return(scm_throw(NO_SUCH_CHANNEL,
		       SCM_LIST3(TO_SCM_STRING(S_mix_amp),
				 SCM_LIST1(n),
				 uchan)));
    }
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING(S_mix_amp),
			     n)));
}

static SCM g_mix_amp_env(SCM n, SCM chan) 
{
  #define H_mix_amp_env "(" S_mix_amp_env " id &optional (chan 0)) -> amplitude envelope applied to mix's channel chan"
  env *e;
  SCM_ASSERT(bool_or_arg_p(n), n, SCM_ARG1, S_mix_amp_env);
  SCM_ASSERT(bool_or_arg_p(chan), chan, SCM_ARG2, S_mix_amp_env);
  e = mix_amp_env_from_id(TO_C_INT_OR_ELSE(n, 0), 
			  TO_C_INT_OR_ELSE(chan, 0));
  if (e) return(env2scm(e));
  return(SCM_EOL);
}

static SCM g_set_mix_position(SCM n, SCM uval) 
{
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_position);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(uval)), uval, SCM_ARG2, "set-" S_mix_position);
  if (set_mix_position(TO_C_INT_OR_ELSE(n, 0), 
		       TO_C_INT_OR_ELSE(uval, 0),
		       FALSE) == INVALID_MIX_ID)
    return(scm_throw(NO_SUCH_MIX,
		     SCM_LIST2(TO_SCM_STRING("set-" S_mix_position),
			       n)));
  return(uval);
}

static SCM g_set_mix_length(SCM n, SCM uval) 
{
  mix_info *md;
  int val;
  console_state *cs = NULL;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_length);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(uval)), uval, SCM_ARG2, "set-" S_mix_length);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md)
    {
      cs = md->current_cs;
      if (cs)
	{
	  val = TO_C_INT_OR_ELSE(uval, 0);
	  if (val >= 0)
	    {
	      cs->len = val;
	      reflect_mix_in_mix_panel(md->id);
	      remix_file(md, "set-" S_mix_length); 
	    }
	}
      return(uval);
    }
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING("set-" S_mix_length),
			     n)));
}

static SCM g_set_mix_locked(SCM n, SCM val) 
{
  console_state *cs;
  mix_info *md;
  int on;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_locked);
  SCM_ASSERT(bool_or_arg_p(val), val, SCM_ARG2, "set-" S_mix_locked);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md)
    {
      on = bool_int_or_one(val);
      cs = md->states[md->curcons];
      cs->locked = on;
      cs = md->current_cs;
      cs->locked = on;
      display_channel_mixes(md->cp);
      return(val);
    }
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING("set-" S_mix_locked),
			     n)));
}

static SCM g_set_mix_anchor(SCM n, SCM uval) 
{
  mix_info *md;
  console_state *cs = NULL;
  int val;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_anchor);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(uval)), uval, SCM_ARG2, "set-" S_mix_anchor);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md)
    {
      cs = md->current_cs;
      if (cs)
	{
	  val = TO_C_INT_OR_ELSE(uval, 0);
	  if (val >= 0)
	    {
	      md->anchor = val;
	      update_graph(md->cp, NULL);
	    }
	}
      return(uval);
    }
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING("set-" S_mix_anchor),
			     n)));
}

static SCM g_set_mix_name(SCM n, SCM val) 
{
  mix_info *md;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_name);
  SCM_ASSERT(gh_string_p(val), val, SCM_ARG2, "set-" S_mix_name);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md)
    {
      if (md->name) FREE(md->name);
      md->name = copy_string(SCM_STRING_CHARS(val));
      reflect_mix_in_mix_panel(md->id);
      return(val);
    }
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING("set-" S_mix_name),
			     n)));
}

static SCM g_set_mix_track(SCM n, SCM val) 
{
  mix_info *md;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_track);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)), val, SCM_ARG2, "set-" S_mix_track);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md)
    {
      md->track = TO_C_INT_OR_ELSE(val, 0);
      reflect_mix_in_mix_panel(md->id);
      return(val);
    }
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING("set-" S_mix_track),
			     n)));
}

static SCM g_set_mix_tag_y(SCM n, SCM val) 
{
  mix_info *md;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_tag_y);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)), val, SCM_ARG2, "set-" S_mix_tag_y);
  md = md_from_id(TO_C_INT_OR_ELSE(n, 0));
  if (md)
    {
      md->tag_y = TO_C_INT_OR_ELSE(val, 0);
      update_graph(md->cp, NULL);
      return(val);
    }
  return(scm_throw(NO_SUCH_MIX,
		   SCM_LIST2(TO_SCM_STRING("set-" S_mix_tag_y),
			     n)));
}

static SCM g_set_mix_speed(SCM n, SCM uval) 
{
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_speed);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(uval)), uval, SCM_ARG2, "set-" S_mix_speed);
  if (set_mix_speed(TO_C_INT_OR_ELSE(n, 0), TO_C_DOUBLE(uval), FALSE, TRUE) == INVALID_MIX_ID)
    return(scm_throw(NO_SUCH_MIX,
		     SCM_LIST2(TO_SCM_STRING("set-" S_mix_speed),
			       n)));
  return(uval);
}

static SCM g_set_mix_amp(SCM n, SCM uchan, SCM uval) 
{
  int res;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_amp);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(uchan)), uchan, SCM_ARG2, "set-" S_mix_amp);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(uval)), uval, SCM_ARG3, "set-" S_mix_amp);
  res = set_mix_amp(TO_C_INT_OR_ELSE(n, 0), TO_C_INT_OR_ELSE(uchan, 0), TO_C_DOUBLE(uval), FALSE, TRUE);
  if (res == INVALID_MIX_ID)
    return(scm_throw(NO_SUCH_MIX,
		     SCM_LIST2(TO_SCM_STRING("set-" S_mix_amp),
			       n)));
  else 
    if (res == INVALID_MIX_CHANNEL)
      return(scm_throw(NO_SUCH_CHANNEL,
		       SCM_LIST3(TO_SCM_STRING("set-" S_mix_amp),
				 n, uchan)));
  return(uval);
}

static SCM g_set_mix_amp_env(SCM n, SCM chan, SCM val) 
{
  env *e = NULL;
  int res;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(n)), n, SCM_ARG1, "set-" S_mix_amp_env);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)), chan, SCM_ARG2, "set-" S_mix_amp_env);
  res = set_mix_amp_env(TO_C_INT_OR_ELSE(n, 0), 
			TO_C_INT_OR_ELSE(chan, 0), 
			e = get_env(val, 
				    SCM_BOOL_F, 
				    "set-" S_mix_amp_env));
  if (e) free_env(e);
  if (res == INVALID_MIX_ID)
    return(scm_throw(NO_SUCH_MIX,
		     SCM_LIST2(TO_SCM_STRING("set-" S_mix_amp_env),
			       n)));
  else 
    if (res == INVALID_MIX_CHANNEL)
      return(scm_throw(NO_SUCH_CHANNEL,
		       SCM_LIST3(TO_SCM_STRING("set-" S_mix_amp_env),
				 n, chan)));
  return(val);
}

static SCM g_mix_sound(SCM file, SCM start_samp)
{
  #define H_mix_sound "(" S_mix_sound " file start_samp) mixes file (all channels) into the currently selected sound at start_samp."

  char *filename;
  snd_state *ss;
  snd_info *sp;
  int beg, err = 0;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_mix_sound);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(start_samp)), start_samp, SCM_ARG1, S_mix_sound);
  filename = full_filename(file);
  beg = TO_C_INT_OR_ELSE(start_samp, 0);
  ss = get_global_state();
  sp = any_selected_sound(ss);  /* why not as arg?? -- apparently this is assuming CLM with-sound explode */
  if (mus_file_probe(filename))
    err = mix(beg, 
	      mus_sound_frames(filename), 
	      sp->nchans, 
	      sp->chans,
	      filename, DONT_DELETE_ME, S_mix_sound, with_mix_tags(ss)); 
  else err = -1;
  if (filename) FREE(filename);
  if (err == -1) 
    return(scm_throw(NO_SUCH_FILE,
		     SCM_LIST3(TO_SCM_STRING(S_mix_sound),
			       file,
			       TO_SCM_STRING(strerror(errno)))));
  return(TO_SCM_INT(err));
}

static int update_mix_waveforms(chan_info *cp, void *ptr)
{
  if ((cp) && (cp->mixes)) update_graph(cp, NULL);
  return(0);
}

static int update_mix_waveform_height(mix_info *md, void *new_val)
{
  int *val = (int *)new_val;
  md->height = val[0];
  return(0);
}

static SCM g_mix_waveform_height(void) {snd_state *ss; ss = get_global_state(); return(TO_SCM_INT(mix_waveform_height(ss)));}
static SCM g_set_mix_waveform_height(SCM val) 
{
  #define H_mix_waveform_height "(" S_mix_waveform_height ") -> max height (pixels) of mix waveforms (20)"
  snd_state *ss; 
  int new_val[1];
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)), val, SCM_ARG1, "set-" S_mix_waveform_height); 
  ss = get_global_state(); 
  new_val[0] = TO_C_INT_OR_ELSE(val, 0);
  in_set_mix_waveform_height(ss, new_val[0]);
  map_over_mixes(update_mix_waveform_height, (void *)new_val);
  map_over_chans(ss, update_mix_waveforms, NULL);
  return(TO_SCM_INT(mix_waveform_height(ss)));
}

static SCM g_mix_tag_width(void) {snd_state *ss; ss = get_global_state(); return(TO_SCM_INT(mix_tag_width(ss)));}
static SCM g_set_mix_tag_width(SCM val) 
{
  #define H_mix_tag_width "(" S_mix_tag_width ") -> width (pixels) of mix tags (6)"
  snd_state *ss; 
  int width;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)), val, SCM_ARG1, "set-" S_mix_tag_width); 
  ss = get_global_state(); 
  width = TO_C_INT_OR_ELSE(val, DEFAULT_MIX_TAG_WIDTH);
  set_mix_tag_width(ss, width);
  map_over_chans(ss, update_graph, NULL);
  return(TO_SCM_INT(mix_tag_width(ss)));
}

static SCM g_mix_tag_height(void) {snd_state *ss; ss = get_global_state(); return(TO_SCM_INT(mix_tag_height(ss)));}
static SCM g_set_mix_tag_height(SCM val) 
{
  #define H_mix_tag_height "(" S_mix_tag_height ") -> height (pixels) of mix tags (14)"
  snd_state *ss; 
  int height;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)), val, SCM_ARG1, "set-" S_mix_tag_height); 
  ss = get_global_state(); 
  height = TO_C_INT_OR_ELSE(val, DEFAULT_MIX_TAG_HEIGHT);
  set_mix_tag_height(ss, height);
  map_over_chans(ss, update_graph, NULL);
  return(TO_SCM_INT(mix_tag_height(ss)));
}

static SCM g_select_mix(SCM id)
{
  #define H_select_mix "(" S_select_mix " id) makes mix is the selected mix"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(id)), id, SCM_ARG1, S_select_mix);
  select_mix(md_from_id(TO_C_INT_OR_ELSE(id, 0)));
  return(id);
}

static SCM g_selected_mix(void)
{
  #define H_selected_mix "(" S_selected_mix ") -> the id of the currently selected mix"
  snd_state *ss;
  ss = get_global_state();
  return(TO_SMALL_SCM_INT(ss->selected_mix));
}

static SCM g_forward_mix(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_mix "(" S_forward_mix " &optional (count 1) snd chn) moves the cursor forward count mix consoles"
  int val;
  chan_info *cp;
  SCM_ASSERT(bool_or_arg_p(count), count, SCM_ARG1, S_forward_mix);
  SND_ASSERT_CHAN(S_forward_mix, snd, chn, 2);
  cp = get_cp(snd, chn, S_forward_mix);
  val = TO_C_INT_OR_ELSE(count, 1); 
  handle_cursor(cp, goto_mix(cp, val));
  return(TO_SCM_INT(val));
}

static SCM g_backward_mix(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_mix "(" S_backward_mix " &optional (count 1) snd chn) moves the cursor back count mix consoles"
  int val; 
  chan_info *cp;
  SCM_ASSERT(bool_or_arg_p(count), count, SCM_ARG1, S_backward_mix);
  SND_ASSERT_CHAN(S_backward_mix, snd, chn, 2);
  cp = get_cp(snd, chn, S_backward_mix);
  val = -(TO_C_INT_OR_ELSE(count, 1)); 
  handle_cursor(cp, goto_mix(cp, val));
  return(TO_SCM_INT(val));
}


static SCM g_mix(SCM file, SCM chn_samp_n, SCM file_chn, SCM snd_n, SCM chn_n, SCM console)
{
  #define H_mix "(" S_mix " file &optional (chn-start 0) (file-chan 0) snd chn with-console))\n\
mixes file channel file-chan into snd's channel chn starting at chn-start (or at the cursor location if chan-start \
is omitted), returning the new mix's id.  if with-console is #f, the data is mixed (no console is created). \
If chn is omitted, file's channels are mixed until snd runs out of channels"

  chan_info *cp = NULL;
  char *name = NULL;
  int chans, id=-1;
  int with_mixer = 1;
  snd_state *ss;
  mix_info *md;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_mix);
  SCM_ASSERT(bool_or_arg_p(chn_samp_n), chn_samp_n, SCM_ARG2, S_mix);
  SCM_ASSERT(bool_or_arg_p(file_chn), file_chn, SCM_ARG3, S_mix);
  SND_ASSERT_CHAN(S_mix, snd_n, chn_n, 4);
  SCM_ASSERT((gh_number_p(console)) || (gh_boolean_p(console)) || (SCM_UNBNDP(console)), console, SCM_ARG6, S_mix);
  name = full_filename(file);
  ss = get_global_state();
  if (SCM_UNBNDP(console))
    with_mixer = with_mix_tags(ss);
  else with_mixer = bool_int_or_one(console);
  if (SCM_UNBNDP(chn_samp_n))
    {
      id = mix_complete_file(any_selected_sound(ss), name, S_mix, with_mixer);
      if (id == -1) 
	{
	  if (name) FREE(name);
	  return(scm_throw(NO_SUCH_FILE,
			   SCM_LIST3(TO_SCM_STRING(S_mix),
				     file,
				     TO_SCM_STRING(strerror(errno)))));
	}
    }
  else
    {
      cp = get_cp(snd_n, chn_n, S_mix);
      chans = mus_sound_chans(name);
      if (chans > 0)
	{
	  md = file_mix_samples(TO_C_INT_OR_ELSE(chn_samp_n, 0),
				mus_sound_samples(name) / chans, 
				name,
				cp, 
				TO_C_INT_OR_ELSE(file_chn, 0),
				DONT_DELETE_ME, 
				S_mix,
				with_mixer);
	  if (md) id = md->id;
	}
      else 
	{
	  if (name) FREE(name);
	  return(scm_throw(NO_SUCH_FILE,
			   SCM_LIST2(TO_SCM_STRING(S_mix),
				     file)));
	}
    }
  if (name) FREE(name);
  return(TO_SCM_INT(id));
}



/* ---------------- mix sample readers ---------------- */

static SND_TAG_TYPE mf_tag = 0;
static SCM mark_mf(SCM obj) {SND_SETGCMARK(obj); return(SCM_BOOL_F);}
static int mf_p(SCM obj) {return((SCM_NIMP(obj)) && (SND_SMOB_TYPE(mf_tag, obj)));}

static SCM g_mf_p(SCM obj) 
{
  #define H_mf_p "(" S_mix_sample_readerQ " obj) -> #t if obj is a mix-sample-reader"
  return(TO_SCM_BOOLEAN(mf_p(obj)));
}

static mix_fd *get_mf(SCM obj) 
{
  if (mf_p(obj)) 
    return((mix_fd *)SND_VALUE_OF(obj)); 
  else return(NULL);
}

static SCM equalp_mf(SCM obj1, SCM obj2) 
{
  return(TO_SCM_BOOLEAN(get_mf(obj1) == get_mf(obj2)));
}

static int print_mf(SCM obj, SCM port, scm_print_state *pstate) 
{
  mix_fd *fd;
  mix_info *md;
  char *desc;
  fd = get_mf(obj);
  if (fd == NULL)
    scm_puts("<null>", port);
  else
    {
      md = fd->md;
      desc = (char *)CALLOC(128, sizeof(char));
      sprintf(desc, "<mix-sample-reader %p: %s via mix %d>",
	      fd,
	      md->in_filename,
	      md->id);
      scm_puts(desc, port); 
      FREE(desc);
    }
  return(1);
}

static scm_sizet free_mf(SCM obj) 
{
  mix_fd *fd = (mix_fd *)SND_VALUE_OF(obj); 
  if (fd) free_mix_fd(fd); 
  return(0);
}

#if (!(HAVE_NEW_SMOB))
static scm_smobfuns mf_smobfuns = {
  &mark_mf,
  &free_mf,
  &print_mf,
  &equalp_mf};
#endif

static SCM g_make_mix_sample_reader(SCM mix_id)
{
  #define H_make_mix_sample_reader "(" S_make_mix_sample_reader " id) returns a reader ready to access mix 'id'"
  mix_info *md = NULL;
  mix_fd *mf = NULL;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(mix_id)), mix_id, SCM_ARG1, S_make_mix_sample_reader);
  md = md_from_id(TO_C_INT_OR_ELSE(mix_id, 0));
  if (md) 
    mf = init_mix_read(md, FALSE); 
  else return(scm_throw(NO_SUCH_MIX,
			SCM_LIST2(TO_SCM_STRING(S_make_mix_sample_reader),
				  mix_id)));
  if (mf)
    {
      SND_RETURN_NEWSMOB(mf_tag, (SCM)mf);
    }
  return(SCM_BOOL_F);
}

static SCM g_next_mix_sample(SCM obj)
{
  #define H_next_mix_sample "(" S_next_mix_sample " reader) -> next sample from mix reader"
  SCM_ASSERT(mf_p(obj), obj, SCM_ARG1, S_next_mix_sample);
  return(TO_SCM_DOUBLE(next_mix_sample(get_mf(obj))));
}

static SCM g_free_mix_sample_reader(SCM obj)
{
  #define H_free_mix_sample_reader "(" S_free_mix_sample_reader " reader) frees mix sample reader 'reader'"
  mix_fd *mf;
  SCM_ASSERT(mf_p(obj), obj, SCM_ARG1, S_free_mix_sample_reader);
  mf = get_mf(obj);
  SND_SET_VALUE_OF(obj, (SCM)NULL);
  free_mix_fd(mf);
  return(scm_return_first(SCM_BOOL_F, obj));
}



/* ---------------- track sample readers ---------------- */

static SND_TAG_TYPE tf_tag = 0;
static SCM mark_tf(SCM obj) {SND_SETGCMARK(obj); return(SCM_BOOL_F);}
static int tf_p(SCM obj) {return((SCM_NIMP(obj)) && (SND_SMOB_TYPE(tf_tag, obj)));}

static SCM g_tf_p(SCM obj) 
{
  #define H_tf_p "(" S_track_sample_readerQ " obj) -> #t if obj is a track-sample-reader"
  return(TO_SCM_BOOLEAN(tf_p(obj)));
}

static track_fd *get_tf(SCM obj) {if (tf_p(obj)) return((track_fd *)SND_VALUE_OF(obj)); else return(NULL);}
static SCM equalp_tf(SCM obj1, SCM obj2) {return(TO_SCM_BOOLEAN(get_tf(obj1) == get_tf(obj2)));}

static int print_tf(SCM obj, SCM port, scm_print_state *pstate) 
{
  track_fd *fd;
  mix_info *md;
  mix_fd *mf;
  char *desc;
  int i, len;
  fd = get_tf(obj);
  if (fd == NULL)
    scm_puts("<null>", port);
  else
    {
      desc = (char *)CALLOC(128, sizeof(char));
      mf = fd->fds[0];
      md = mf->md;
      sprintf(desc, "<track-sample-reader %p: %s chan %d via mixes '(",
	      fd,
	      md->in_filename,
	      (md->cp)->chan);
      scm_puts(desc, port); 
      len = fd->mixes;
      if (len > 0)
	{
	  for (i = 0; i < len-1; i++)
	    {
	      mf = fd->fds[i];
	      sprintf(desc, "%d ", (mf->md)->id);
	      scm_puts(desc, port); 
	    }
	  mf = fd->fds[len-1];
	  sprintf(desc, "%d)>",(mf->md)->id);
	}
      else sprintf(desc, ")>");
      scm_puts(desc, port); 
      FREE(desc);
    }
  return(1);
}

static scm_sizet free_tf(SCM obj) 
{
  track_fd *fd = (track_fd *)SND_VALUE_OF(obj); 
  if (fd) free_track_fd(fd); 
  return(0);
}

#if (!(HAVE_NEW_SMOB))
static scm_smobfuns tf_smobfuns = {
  &mark_tf,
  &free_tf,
  &print_tf,
  &equalp_tf};
#endif

static SCM g_make_track_sample_reader(SCM track_id, SCM samp, SCM snd, SCM chn)
{
  #define H_make_track_sample_reader "(" S_make_track_sample_reader " track &optional (start-samp 0) snd chn)\n\
returns a reader ready to access track's data associated with snd's channel chn starting at 'start-samp'"

  track_fd *tf = NULL;
  chan_info *cp;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(track_id)), track_id, SCM_ARG1, S_make_track_sample_reader);
  SND_ASSERT_CHAN(S_make_track_sample_reader, snd, chn, 3); 
  cp = get_cp(snd, chn, S_make_track_sample_reader);
  tf = init_track_reader(cp, 
			 TO_C_INT_OR_ELSE(track_id, 0), 
			 TO_C_INT_OR_ELSE(samp, 0));
  if (tf)
    {
      SND_RETURN_NEWSMOB(tf_tag, (SCM)tf);
    }
  return(scm_throw(NO_SUCH_TRACK,
		   SCM_LIST2(TO_SCM_STRING(S_make_track_sample_reader),
			     track_id)));
}

static SCM g_next_track_sample(SCM obj)
{
  #define H_next_track_sample "(" S_next_track_sample " reader) -> next sample from track reader"
  SCM_ASSERT(tf_p(obj), obj, SCM_ARG1, S_next_track_sample);
  return(TO_SCM_DOUBLE(MUS_SAMPLE_TO_FLOAT(next_track_sample(get_tf(obj)))));
}

static SCM g_free_track_sample_reader(SCM obj)
{
  #define H_free_track_sample_reader "(" S_free_track_sample_reader " reader) frees the track sample reader 'reader'"
  track_fd *tf = NULL;
  SCM_ASSERT(tf_p(obj), obj, SCM_ARG1, S_free_track_sample_reader);
  tf = get_tf(obj);
  SND_SET_VALUE_OF(obj, (SCM)NULL);
  free_track_fd(tf);
  return(scm_return_first(SCM_BOOL_F, obj));
}

static SCM g_play_track(SCM num, SCM snd, SCM chn)
{
  #define H_play_track "(" S_play_track " track &optional snd chn) plays track"
  /* just a dummy for testing */
  chan_info *cp;
  /* in this case if snd=#t, play all associated mixes in all chans */
  if (SCM_TRUE_P(snd))
    play_track(get_global_state(), NULL, 0, TO_C_INT_OR_ELSE(num, 0));
  else 
    {
      cp = get_cp(snd, chn, S_play_track);
      play_track(cp->state, &cp, 1, TO_C_INT_OR_ELSE(num, 0));
    }
  return(num);
}

static SCM g_play_mix(SCM num)
{
  #define H_play_mix "(" S_play_mix " id) plays mix"
  mix_info *md;
  md = md_from_id(TO_C_INT_OR_ELSE(num, 0));
  if (md) 
    play_mix(md->ss, md); 
  else return(scm_throw(NO_SUCH_MIX,
			SCM_LIST2(TO_SCM_STRING(S_play_mix),
				  num)));
  return(num);
}

static SCM multichannel_mix_hook, mix_speed_changed_hook, mix_amp_changed_hook, mix_position_changed_hook;

#if HAVE_HOOKS
static void call_multichannel_mix_hook(int *ids, int n)
{
  SCM lst = SCM_EOL;
  int i;
  /* create list from ids, pass to hook, if any */
  if (HOOKED(multichannel_mix_hook))
    {
      for (i = n-1; i >= 0; i--)
	lst = scm_cons(TO_SMALL_SCM_INT(ids[i]), lst);
      g_c_run_progn_hook(multichannel_mix_hook,
			 SCM_LIST1(lst));
    }
}

static int call_mix_speed_changed_hook(mix_info *md)
{  
  SCM res = SCM_BOOL_F;
  if ((md) && 
      (HOOKED(mix_speed_changed_hook)))
    res = g_c_run_progn_hook(mix_speed_changed_hook,
			     SCM_LIST1(TO_SMALL_SCM_INT(md->id)));
  return(SCM_TRUE_P(res));
}

static int call_mix_amp_changed_hook(mix_info *md)
{  
  SCM res = SCM_BOOL_F;
  if ((md) && 
      (HOOKED(mix_amp_changed_hook)))
    res = g_c_run_progn_hook(mix_amp_changed_hook,
			     SCM_LIST1(TO_SMALL_SCM_INT(md->id)));
  return(SCM_TRUE_P(res));
}

static int call_mix_position_changed_hook(mix_info *md, int samps)
{  
  SCM res = SCM_BOOL_F;
  if ((md) && 
      (HOOKED(mix_position_changed_hook)))
    res = g_c_run_progn_hook(mix_position_changed_hook,
			     SCM_LIST2(TO_SMALL_SCM_INT(md->id),
				       TO_SCM_INT(samps)));
  return(SCM_TRUE_P(res));
}
#else
static void call_multichannel_mix_hook(int *ids, int n) {}
static int call_mix_speed_changed_hook(mix_info *md) {return(0);}
static int call_mix_amp_changed_hook(mix_info *md) {return(0);}
static int call_mix_position_changed_hook(mix_info *md, int samps) {return(0);}
#endif

void g_init_mix(SCM local_doc)
{
#if HAVE_NEW_SMOB
  mf_tag = scm_make_smob_type("mf", sizeof(SCM));
  scm_set_smob_mark(mf_tag, mark_mf);
  scm_set_smob_print(mf_tag, print_mf);
  scm_set_smob_free(mf_tag, free_mf);
  scm_set_smob_equalp(mf_tag, equalp_mf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(mf_tag, g_next_mix_sample, 0, 0, 0);
#endif
#else
  mf_tag = scm_newsmob(&mf_smobfuns);
#endif
  DEFINE_PROC(gh_new_procedure1_0(S_make_mix_sample_reader, g_make_mix_sample_reader), H_make_mix_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_next_mix_sample,        g_next_mix_sample),        H_next_mix_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_free_mix_sample_reader, g_free_mix_sample_reader), H_free_mix_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_mix_sample_readerQ,     g_mf_p),                   H_mf_p);

#if HAVE_NEW_SMOB
  tf_tag = scm_make_smob_type("tf", sizeof(SCM));
  scm_set_smob_mark(tf_tag, mark_tf);
  scm_set_smob_print(tf_tag, print_tf);
  scm_set_smob_free(tf_tag, free_tf);
  scm_set_smob_equalp(tf_tag, equalp_tf);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(tf_tag, g_next_track_sample, 0, 0, 0);
#endif
#else
  tf_tag = scm_newsmob(&tf_smobfuns);
#endif
  DEFINE_PROC(gh_new_procedure(S_make_track_sample_reader, SCM_FNC g_make_track_sample_reader, 1, 3, 0), H_make_track_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_next_track_sample,             g_next_track_sample),                 H_next_track_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_free_track_sample_reader,      g_free_track_sample_reader),          H_free_track_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_track_sample_readerQ,          g_tf_p),                              H_tf_p);
  DEFINE_PROC(gh_new_procedure0_1(S_play_mix,                      g_play_mix),                          H_play_mix);
  DEFINE_PROC(gh_new_procedure1_2(S_play_track,                    g_play_track),                        H_play_track);

  define_procedure_with_setter(S_mix_position, SCM_FNC g_mix_position, H_mix_position,
			       "set-" S_mix_position, SCM_FNC g_set_mix_position,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter(S_mix_length, SCM_FNC g_mix_length, H_mix_length,
			       "set-" S_mix_length, SCM_FNC g_set_mix_length,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter(S_mix_locked, SCM_FNC g_mix_locked, H_mix_locked,
			       "set-" S_mix_locked, SCM_FNC g_set_mix_locked,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter(S_mix_anchor, SCM_FNC g_mix_anchor, H_mix_anchor,
			       "set-" S_mix_anchor, SCM_FNC g_set_mix_anchor,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter(S_mix_track, SCM_FNC g_mix_track, H_mix_track,
			       "set-" S_mix_track, SCM_FNC g_set_mix_track,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter("mix-sync", SCM_FNC g_mix_track, H_mix_track,
			       "set-mix-sync", SCM_FNC g_set_mix_track,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter(S_mix_tag_y, SCM_FNC g_mix_tag_y, H_mix_tag_y,
			       "set-" S_mix_tag_y, SCM_FNC g_set_mix_tag_y,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter(S_mix_speed, SCM_FNC g_mix_speed, H_mix_speed,
			       "set-" S_mix_speed, SCM_FNC g_set_mix_speed,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter(S_mix_name, SCM_FNC g_mix_name, H_mix_name,
			       "set-" S_mix_name, SCM_FNC g_set_mix_name,
			       local_doc, 0, 1, 2, 0);

  define_procedure_with_setter(S_mix_waveform_height, SCM_FNC g_mix_waveform_height, H_mix_waveform_height,
			       "set-" S_mix_waveform_height, SCM_FNC g_set_mix_waveform_height,
			       local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_mix_tag_width, SCM_FNC g_mix_tag_width, H_mix_tag_width,
			       "set-" S_mix_tag_width, SCM_FNC g_set_mix_tag_width,
			       local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_mix_tag_height, SCM_FNC g_mix_tag_height, H_mix_tag_height,
			       "set-" S_mix_tag_height, SCM_FNC g_set_mix_tag_height,
			       local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_mix_amp, SCM_FNC g_mix_amp, H_mix_amp,
			       "set-" S_mix_amp, SCM_FNC g_set_mix_amp,
			       local_doc, 0, 2, 3, 0);

  define_procedure_with_setter(S_mix_amp_env, SCM_FNC g_mix_amp_env, H_mix_amp_env,
			       "set-" S_mix_amp_env, SCM_FNC g_set_mix_amp_env,
			       local_doc, 0, 2, 3, 0);


  DEFINE_PROC(gh_new_procedure0_1(S_mix_chans,         g_mix_chans),         H_mix_chans);
  DEFINE_PROC(gh_new_procedure0_1(S_mixQ,              g_mixQ),              H_mixQ);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_sound_channel, g_mix_sound_channel), H_mix_sound_channel);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_sound_index,   g_mix_sound_index),   H_mix_sound_index);
  DEFINE_PROC(gh_new_procedure0_2(S_mixes,             g_mixes),             H_mixes);
  DEFINE_PROC(gh_new_procedure2_0(S_mix_sound,         g_mix_sound),         H_mix_sound);
  DEFINE_PROC(gh_new_procedure1_0(S_select_mix,        g_select_mix),        H_select_mix);

  /* DEFINE_PROC(gh_new_procedure0_0(S_selected_mix, g_selected_mix), H_selected_mix); */
  define_procedure_with_setter(S_selected_mix, SCM_FNC g_selected_mix, H_selected_mix,
			       "set-" S_selected_mix, SCM_FNC g_select_mix,
			       local_doc, 0, 0, 1, 0);

  DEFINE_PROC(gh_new_procedure(S_forward_mix,  SCM_FNC g_forward_mix, 0, 3, 0),  H_forward_mix);
  DEFINE_PROC(gh_new_procedure(S_backward_mix, SCM_FNC g_backward_mix, 0, 3, 0), H_backward_mix);
  DEFINE_PROC(gh_new_procedure(S_mix,          SCM_FNC g_mix, 1, 5, 0),          H_mix);

  #define H_multichannel_mix_hook S_multichannel_mix_hook "(ids) is called when a multichannel mix happens in a sync'd sound. \
'ids' is a list of mix id numbers."

  #define H_mix_speed_changed_hook S_mix_speed_changed_hook " (mix-id) is called when a mix speed changes via the mouse. \
If it returns #t, the actual remix is the hook's responsibility."

  #define H_mix_amp_changed_hook S_mix_amp_changed_hook " (mix-id) is called when a mix amp changes via the mouse. \
If it returns #t, the actual remix is the hook's responsibility."

  #define H_mix_position_changed_hook S_mix_position_changed_hook " (mix-id samps) is called when a mix position changes via the mouse. \
'samps' = samples moved. If it returns #t, the actual remix is the hook's responsibility."

  multichannel_mix_hook =     MAKE_HOOK(S_multichannel_mix_hook, 1, H_multichannel_mix_hook);
  mix_speed_changed_hook =    MAKE_HOOK(S_mix_speed_changed_hook, 1, H_mix_speed_changed_hook);
  mix_amp_changed_hook =      MAKE_HOOK(S_mix_amp_changed_hook, 1, H_mix_amp_changed_hook);
  mix_position_changed_hook = MAKE_HOOK(S_mix_position_changed_hook, 2, H_mix_position_changed_hook);
}

#else
static int call_mix_speed_changed_hook(mix_info *md) {return(0);}
static int call_mix_amp_changed_hook(mix_info *md) {return(0);}
static int call_mix_position_changed_hook(mix_info *md, int samps) {return(0);}
#endif


