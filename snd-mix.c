#include "snd.h"

#if 0
static void display_md(mixdata *md)
{
  console_state *cs;
  int i;
  fprintf(stderr,"\n  md: %d %d",md->curcons,(md->cp)->edit_ctr);
  cs = md->current_cs;
  fprintf(stderr,"\n    cur: (%d %d)",cs->mix_edit_ctr[0],cs->edit_ctr);
  for (i = md->curcons;i>=0;i--)
    {
      cs = md->states[i];
      fprintf(stderr,", (%d %d)",cs->mix_edit_ctr[0],cs->edit_ctr);
    }
  fprintf(stderr,"\n\n");
}
#endif

/* ---------------- MIX POOL ---------------- */

#define MPOOL_INCREMENT 16
static mixdata **mixdatas = NULL;
static int mixdatas_size = 0;
static int mixdatas_ctr = 0;

static mixdata *new_mixdata(void)
{
  int i;
  if (mixdatas == NULL)
    {
      mixdatas_size = MPOOL_INCREMENT;
      mixdatas = (mixdata **)CALLOC(mixdatas_size,sizeof(mixdata *));
    }
  else
    {
      if (mixdatas_ctr >= mixdatas_size)
	{
	  mixdatas_size += MPOOL_INCREMENT;
	  mixdatas = (mixdata **)REALLOC(mixdatas,mixdatas_size * sizeof(mixdata *));
	  for (i=mixdatas_size-MPOOL_INCREMENT;i<mixdatas_size;i++) mixdatas[i] = NULL;
	}
    }
  mixdatas[mixdatas_ctr] = (mixdata *)CALLOC(1,sizeof(mixdata));
  return(mixdatas[mixdatas_ctr]);
}

static console_state *make_console_state(int chans, int edit_ctr, int beg, int end)
{
  console_state *cs;
  cs = (console_state *)CALLOC(1,sizeof(console_state));
  cs->chans = chans;
  cs->edit_ctr = edit_ctr;
  cs->orig = beg;
  cs->beg = beg;
  cs->end = end;
  cs->len = end-beg+1;
  cs->locked = 0;
  cs->mix_edit_ctr = (int *)CALLOC(chans,sizeof(int));
  cs->scalers = (Float *)CALLOC(chans,sizeof(Float));
  cs->old_scalers = (Float *)CALLOC(chans,sizeof(Float));
  /* scalers hold the local (scale widget relative) value
     old_scalers hold the last local value set explicitly by the user
     */
  cs->amp_envs = NULL; 
  return(cs);
}

static console_state *copy_console(console_state *cs)
{
  console_state *ncs;
  int i;
  ncs = make_console_state(cs->chans,cs->edit_ctr,cs->orig,cs->end);
  if (!(cs->locked))
    {
      for (i=0;i<cs->chans;i++)
	{
	  ncs->scalers[i] = cs->scalers[i];
	  ncs->old_scalers[i] = cs->old_scalers[i];
	}
    }
  ncs->locked = cs->locked;
  ncs->speed = cs->speed;
  for (i=0;i<cs->chans;i++) ncs->mix_edit_ctr[i] = cs->mix_edit_ctr[i];
  ncs->scl_speed = cs->scl_speed;
  ncs->old_speed = cs->old_speed;
  if (cs->amp_envs)
    {
      ncs->amp_envs = (env **)CALLOC(cs->chans,sizeof(env *));
      for (i=0;i<cs->chans;i++) ncs->amp_envs[i] = copy_env(cs->amp_envs[i]);
    }
  else ncs->amp_envs = NULL;
  ncs->len = cs->len;
  return(ncs);
}

static void make_current_console(mixdata *md)
{
  int i;
  console_state *cs,*cur;
  cs = md->states[md->curcons];
  cur = md->current_cs;
  cur->chans = cs->chans;
  for (i=0;i<cs->chans;i++) cur->mix_edit_ctr[i] = cs->mix_edit_ctr[i];
  cur->edit_ctr = cs->edit_ctr;
  cur->orig = cs->beg;
  cur->beg = cs->beg;
  cur->end = cs->end;
  cur->len = cs->len;
  cur->locked = cs->locked;
  if (!(cs->locked))
    {
      for (i=0;i<cs->chans;i++)
	{
	  cur->scalers[i] = cs->scalers[i];
	  cur->old_scalers[i] = cs->old_scalers[i];
	}
    }
  cur->speed = cs->speed;
  cur->scl_speed = cs->scl_speed;
  cur->old_speed = cs->old_speed;
  if (cur->amp_envs)
    {
      for (i=0;i<cs->chans;i++) 
	{
	  cur->amp_envs[i] = free_env(cur->amp_envs[i]);
	  if (cs->amp_envs)
	    cur->amp_envs[i] = copy_env(cs->amp_envs[i]);
	}
    }
}

static console_state *free_console_state(console_state *cs)
{
  int i;
  if (cs)
    {
      if (cs->scalers) {FREE(cs->scalers); cs->scalers = NULL;}
      if (cs->old_scalers) {FREE(cs->old_scalers); cs->old_scalers = NULL;}
      if (cs->mix_edit_ctr) {FREE(cs->mix_edit_ctr); cs->mix_edit_ctr = NULL;}
      if (cs->amp_envs) 
	{
	  for (i=0;i<cs->chans;i++) free_env(cs->amp_envs[i]);
	  FREE(cs->amp_envs);
	}
      FREE(cs);
    }
  return(NULL);
}


static void release_mixmark(mixmark *m)
{
  mixdata *md;
  m->inuse = 0;
  md = (mixdata *)(m->owner);      /* tell owner his widgets have been taken */
  if (md) {md->mixer = NULL; m->owner = NULL;}
  release_mixmark_widgets(m);
}

mix_context *free_mix_context(mix_context *ms)
{
  if (ms->p0) {FREE(ms->p0); ms->p0 = NULL;}
  if (ms->p1) {FREE(ms->p1); ms->p1 = NULL;}
  FREE(ms);
  return(NULL);
}

static mixdata *free_mixdata(mixdata *m)
{
  int i;
  snd_state *ss;
  if (m)
    {
      ss = m->ss;
      if (m->id == ss->selected_mix) ss->selected_mix = NO_SELECTION;
      if (m->wg) m->wg = free_mix_context(m->wg);
      mixdatas[m->id] = NULL;
      if (m->temporary == DELETE_ME) {mus_sound_forget(m->in_filename); remove(m->in_filename);}
      if (m->mixer) {release_mixmark(m->mixer); m->mixer = NULL;}
      if (m->name) {FREE(m->name); m->name = NULL;}
      if (m->in_filename) {FREE(m->in_filename); m->in_filename = NULL;}
      if (m->out_filename) {FREE(m->out_filename); m->out_filename = NULL;}
      if (m->add_snd) {completely_free_snd_info(m->add_snd); m->add_snd = NULL;}
      if (m->states)
	{
	  for (i=0;i<m->console_state_size;i++) {if (m->states[i]) m->states[i] = free_console_state(m->states[i]);}
	  FREE(m->states);
	  m->states = NULL;
	}
      if (m->current_cs) m->current_cs = free_console_state(m->current_cs);
      FREE(m);
    }
  return(NULL);
}

chan_info *m_to_cp(mixmark *m)
{
  return(((mixdata *)(m->owner))->cp);
}

static void release_pending_consoles(mixdata *md)
{
  int i;
  if (md->states)
    {
      for (i=md->curcons+1;i<md->console_state_size;i++) 
	{
	  if (md->states[i]) 
	    md->states[i] = free_console_state(md->states[i]);
	}
    }
}


/* ---------------- MIX READ ---------------- */

typedef struct {
  int type;
  mixdata *md;
  console_state *cs;
  snd_fd **sfs;
  int chans;                           /* chans of input */
  int calc,base;
  Float x,sr;
  MUS_SAMPLE_TYPE *lst,*nxt;
  src_state **srcs;
  mus_any **segs;
  int *ctr,*samples;
  MUS_SAMPLE_TYPE **idata;
  Float samps_per_bin;
} mix_fd;

static env_info *make_mix_input_amp_env(chan_info *cp)
{
  env_state *es;
  if (current_ed_samples(cp) > AMP_ENV_CUTOFF)
    {
      es = make_env_state(cp,current_ed_samples(cp)); /* sets cp->amp_envs[pos] */
      while (!(tick_amp_env(cp,es)));
      if (es->sf) es->sf = free_snd_fd(es->sf);
      FREE(es);
      return(cp->amp_envs[cp->edit_ctr]);
    }
  return(NULL);
}

static int mix_input_amp_env_usable(mixdata *md, Float samples_per_pixel) 
{
  env_info *ep=NULL;
  int i,happy=1;
  chan_info *cp;
  snd_info *sp;
  Float samps_per_bin = 0.0;
  sp = make_mix_readable(md);
  if (sp)
    {
      for (i=0;i<sp->nchans;i++)
	{
	  cp = sp->chans[i];
	  if ((cp == NULL) || (cp->amp_envs == NULL)) return(FALSE);
	  ep = cp->amp_envs[cp->edit_ctr];
	  if ((ep == NULL) && (current_ed_samples(cp) > AMP_ENV_CUTOFF))
	    ep = make_mix_input_amp_env(cp);
	  if ((ep) && (samps_per_bin == 0.0)) samps_per_bin = ep->samps_per_bin;
	  happy = ((ep) && (samples_per_pixel >= (Float)(ep->samps_per_bin)) && (ep->samps_per_bin == samps_per_bin));
	  if (!happy) break;
	}
      return(happy);
    }
  return(FALSE);
}

snd_info *make_mix_readable(mixdata *md)
{
  chan_info *cp;
  int i;
  snd_state *ss;
  snd_info *add_sp;
  if (!md->add_snd) 
    {
      cp = md->cp;
      md->add_snd = make_sound_readable(cp->state,md->in_filename,TRUE);
      if (!(md->add_snd)) 
	snd_error(STR_cant_find,md->in_filename);
      else
	{
	  add_sp = md->add_snd;
	  add_sp->fullname = copy_string(md->in_filename);
	  add_sp->shortname = filename_without_home_directory(add_sp->fullname);
	  ss = add_sp->state;
	  for (i=0;i<add_sp->nchans;i++) 
	    {
	      cp = add_sp->chans[i];
	      if (cp) 
		{
		  cp->mix_md = md;
		  if (show_mix_waveforms(ss)) make_mix_input_amp_env(cp);
		}
	    }
	}
    }
  return(md->add_snd);
}

#define C_STRAIGHT 0
#define C_AMP 1
#define C_SPEED 2
#define C_ZERO 3

#define MIX_INPUT_SOUND 0
#define MIX_INPUT_AMP_ENV 1
 
static MUS_SAMPLE_TYPE next_mix_input_amp_env_sample(mix_fd *mf, int chan)
{
  if (mf->ctr[chan] < mf->samples[chan])
    {
      mf->ctr[chan]++;
      return(mf->idata[chan][mf->ctr[chan]]);
    }
  return(0);
}

static MUS_SAMPLE_TYPE next_mix_sample(mix_fd *mf)
{
  int i,j,move;
  MUS_SAMPLE_TYPE val,sum = MUS_SAMPLE_0;
  Float spd;
  console_state *cs;
  switch (mf->calc)
    {
    case C_STRAIGHT:
      if (mf->type == MIX_INPUT_SOUND)
	NEXT_SAMPLE(sum,mf->sfs[mf->base]);
      else sum = next_mix_input_amp_env_sample(mf,mf->base);
      break;
    case C_ZERO: 
      return(MUS_SAMPLE_0);
      break;
    case C_AMP:
      sum = MUS_SAMPLE_0;
      cs = mf->cs;
      if (mf->segs)
	{
	  for (i=0;i<mf->chans;i++)
	    {
	      if (mf->type == MIX_INPUT_SOUND)
		NEXT_SAMPLE(val,mf->sfs[i]);
	      else val = next_mix_input_amp_env_sample(mf,i);
	      if (mf->segs[i])
		sum += ((MUS_SAMPLE_TYPE)(val * mus_env(mf->segs[i])));
	      else sum += ((MUS_SAMPLE_TYPE)(val * cs->scalers[i]));
	    }
	}
      else
	{
	  if (mf->type == MIX_INPUT_SOUND)
	    {
	      for (i=0;i<mf->chans;i++)
		{
		  NEXT_SAMPLE(val,mf->sfs[i]);
		  sum += ((MUS_SAMPLE_TYPE)(val * cs->scalers[i]));
		}
	    }
	  else
	    {
	      for (i=0;i<mf->chans;i++)
		sum += (MUS_SAMPLE_TYPE)(cs->scalers[i] * next_mix_input_amp_env_sample(mf,i));
	    }
	}
      break;
    case C_SPEED:
      sum = MUS_SAMPLE_0;
      cs = mf->cs;
      if (mf->srcs)
	{
	  if (mf->segs)
	    {
	      for (i=0;i<mf->chans;i++)
		{
		  if (cs->scalers[i] > 0.0)
		    {
		      (mf->srcs[i])->incr = mf->sr;
		      if (mf->segs[i])
			sum += (MUS_SAMPLE_TYPE)(mus_env(mf->segs[i]) * run_src(mf->srcs[i],0.0));
		      else sum += (MUS_SAMPLE_TYPE)(cs->scalers[i] * run_src(mf->srcs[i],0.0));
		    }
		}
	    }
	  else
	    {
	      for (i=0;i<mf->chans;i++)
		{
		  if (cs->scalers[i] > 0.0)
		    {
		      (mf->srcs[i])->incr = mf->sr;
		      sum += (MUS_SAMPLE_TYPE)(cs->scalers[i] * run_src(mf->srcs[i],0.0));
		    }
		}
	    }
	}
      else
	{
	  spd = mf->sr;
	  if (mf->segs)
	    {
	      for (i=0;i<mf->chans;i++)
		{
		  if (cs->scalers[i] > 0.0)
		    {
		      if (mf->segs[i])
			sum += (MUS_SAMPLE_TYPE)(mus_env(mf->segs[i]) * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
		      else sum += (MUS_SAMPLE_TYPE)(cs->scalers[i] * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
		    }
		}
	    }
	  else
	    {
	      for (i=0;i<mf->chans;i++)
		{
		  if (cs->scalers[i] > 0.0)
		    sum += (MUS_SAMPLE_TYPE)(cs->scalers[i] * (mf->lst[i] + mf->x * (mf->nxt[i] - mf->lst[i])));
		}
	    }
	  mf->x += spd;
	  move = (int)(mf->x);
	  if (move != 0)
	    {
	      mf->x -= move;
	      for (j=0;j<move;j++)
		{
		  for (i=0;i<mf->chans;i++)
		    {
		      mf->lst[i] = mf->nxt[i];
		      if (mf->type == MIX_INPUT_SOUND)
			NEXT_SAMPLE(mf->nxt[i],mf->sfs[i]);
		      else mf->nxt[i] = next_mix_input_amp_env_sample(mf,i);
		    }
		}
	    }
	}
      break;
    }
  return(sum);
}

static int amp_env_len(mixdata *md, int chan)
{
  return(current_ed_samples((md->add_snd)->chans[chan]));
}

static mix_fd *init_mix_read_1(mixdata *md, int old, int type, int hi)
{
  mix_fd *mf;
  env *e;
  snd_info *add_sp;
  chan_info *cp;
  console_state *cs;
  int i,chans;
  if (old) 
    cs = md->states[md->curcons];
  else cs = md->current_cs;
  if (cs->scalers == NULL) return(NULL);
  chans = md->in_chans;
  mf = (mix_fd *)CALLOC(1,sizeof(mix_fd));
  mf->type = type;
  mf->calc = C_ZERO;
  mf->sr = cs->speed;
  mf->md = md;
  mf->cs = cs;
  mf->chans = chans;
  for (i=0;i<chans;i++)
    {
      if (cs->scalers[i] != 0.0) {mf->calc = C_STRAIGHT; break;}
    }
  if (mf->calc == C_ZERO) return(mf);
  if (!(md->add_snd)) md->add_snd = make_mix_readable(md);
  add_sp = md->add_snd;
  if (type == MIX_INPUT_SOUND)
    mf->sfs = (snd_fd **)CALLOC(chans,sizeof(snd_fd *));
  else mf->sfs = NULL;
  cp = md->cp;
  mf->base = cp->chan;
  if (cs->amp_envs)
    {
      mf->segs = (mus_any **)CALLOC(mf->chans,sizeof(mus_any *));
      for (i=0;i<chans;i++)
	{
	  e = cs->amp_envs[i];
	  if ((e) && (cs->scalers[i] != 0.0))
	    mf->segs[i] = mus_make_env(e->data,e->pts,cs->scalers[i],0.0,1.0,0.0,0,(type == MIX_INPUT_SOUND) ? (cs->len - 1) : amp_env_len(md,i),NULL);
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
	  for (i=0;i<chans;i++)
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
	mf->sfs[mf->base] = init_sample_read_any(0,add_sp->chans[mf->base],READ_FORWARD,cs->mix_edit_ctr[mf->base]);
      else
	{
	  for (i=0;i<chans;i++)
	    mf->sfs[i] = init_sample_read_any(0,add_sp->chans[i],READ_FORWARD,cs->mix_edit_ctr[i]);
	}
    }
  if (mf->calc == C_SPEED)
    {
      if ((type == MIX_INPUT_SOUND) && (use_sinc_interp((md->ss))))
	{
	  mf->srcs = (src_state **)CALLOC(chans,sizeof(src_state *));
	  for (i=0;i<chans;i++)
	    {
	      mf->srcs[i] = make_src(md->ss,1.0,mf->sfs[i]);
	    }
	  mf->lst = NULL;
	  mf->nxt = NULL;
	}
      else
	{
	  /* initialize interpolator */
	  mf->srcs = NULL;
	  mf->lst = (MUS_SAMPLE_TYPE *)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE));
	  mf->nxt = (MUS_SAMPLE_TYPE *)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE));
	  if (type == MIX_INPUT_SOUND)
	    {
	      for (i=0;i<chans;i++)
		{
		  NEXT_SAMPLE(mf->lst[i],mf->sfs[i]);
		  NEXT_SAMPLE(mf->nxt[i],mf->sfs[i]);
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

static mix_fd *init_mix_read(mixdata *md, int old)
{
  return(init_mix_read_1(md,old,MIX_INPUT_SOUND,0));
}

static mix_fd *init_mix_input_amp_env_read(mixdata *md, int old, int hi)
{
  int i;
  mix_fd *mf = NULL;
  snd_info *sp;
  chan_info *cp;
  env_info *ep;
  mf = init_mix_read_1(md,old,MIX_INPUT_AMP_ENV,hi);
  sp = md->add_snd;
  mf->ctr = (int *)CALLOC(sp->nchans,sizeof(int));
  mf->samples = (int *)CALLOC(sp->nchans,sizeof(int));
  mf->idata = (MUS_SAMPLE_TYPE **)CALLOC(sp->nchans,sizeof(MUS_SAMPLE_TYPE *));
  for (i=0;i<sp->nchans;i++)
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
	  for (i=0;i<mf->chans;i++)
	    if (mf->sfs[i]) mf->sfs[i] = free_snd_fd(mf->sfs[i]);
	  FREE(mf->sfs);
	  mf->sfs = NULL;
	}
      if (mf->ctr) FREE(mf->ctr); mf->ctr = NULL; 
      if (mf->samples) FREE(mf->samples); mf->samples = NULL; 
      if (mf->idata) FREE(mf->idata); mf->idata = NULL; 
      if (mf->segs)
	{
	  for (i=0;i<mf->chans;i++)
	    if (mf->segs[i])
	      mus_free(mf->segs[i]);
	  FREE(mf->segs);
	  mf->segs = NULL;
	}
      if (mf->srcs) 
	{
	  for (i=0;i<mf->chans;i++)
	    free_src(mf->srcs[i]);
	  FREE(mf->srcs);
	  mf->srcs = NULL;
	}
      FREE(mf);
    }
  return(NULL);
}


/* ---------------- MIX STATE (chan_info mix widget pool) ---------------- */

typedef struct {
  int widget_size;
  mixmark **widget_pool;
} mix_state;

static mixmark *ur_get_mixmark(chan_info *ncp, int in_chans)
{
  int i,next;
  mix_state *ms;
  mixmark *m;

  chan_info *cp;
  snd_info *sp;
  sp = ncp->sound;
  if ((ncp->chan == 0) || (sp->combining == CHANNELS_SEPARATE))
    cp = ncp;
  else cp = sp->chans[0];
  if (!cp->mixes) cp->mixes = (mix_state *)CALLOC(1,sizeof(mix_state));
  ms = (mix_state *)(cp->mixes);
  if (ms->widget_size == 0) 
    {
      ms->widget_size = MPOOL_INCREMENT;
      ms->widget_pool = (mixmark **)CALLOC(ms->widget_size,sizeof(mixmark *));
      m = (mixmark *)CALLOC(1,sizeof(mixmark));
      ms->widget_pool[0] = m; 
      return(m);
    }
  for (i=0;i<ms->widget_size;i++)
    {
      m = ms->widget_pool[i];
      if (!m)
	{
	  ms->widget_pool[i] = (mixmark *)CALLOC(1,sizeof(mixmark));
	  return(ms->widget_pool[i]);
	}
      if ((!(m->inuse)) && (m->chans_allocated == in_chans)) return(m);
    }
  /* no free mixmark slots found */
  next = ms->widget_size;
  ms->widget_size += MPOOL_INCREMENT;
  ms->widget_pool = (mixmark **)REALLOC(ms->widget_pool,sizeof(mixmark *)*ms->widget_size);
  for (i=next;i<ms->widget_size;i++) ms->widget_pool[i] = NULL;
  m = (mixmark *)CALLOC(1,sizeof(mixmark));
  ms->widget_pool[next] = m; 
  return(ms->widget_pool[next]);
}

static mixmark *get_mixmark(chan_info *ncp, int in_chans)
{
  mixmark *m;
  m = ur_get_mixmark(ncp,in_chans);
  m->inuse = 1;
  m->owner = NULL;
  return(m);
}

/* ---------------- MIXING ---------------- */

static int map_over_channel_mixes(chan_info *cp, int (*func)(mixdata *,void *), void *ptr)
{
  int i,val;
  mixdata *md;
  for (i=0;i<mixdatas_ctr;i++)
    {
      md = mixdatas[i];
      if ((md) && (md->cp == cp))
	{
	  val = (*func)(md,ptr);
	  if (val) return(val);
	}
    }
  return(0);
}

int map_over_mixes(int (*func)(mixdata *,void *), void *ptr)
{
  int i,val;
  mixdata *md;
  for (i=0;i<mixdatas_ctr;i++)
    {
      md = mixdatas[i];
      if (md)
	{
	  val = (*func)(md,ptr);
	  if (val) return(val);
	}
    }
  return(0);
}

static int remove_temporary_mix_file(mixdata *md, void *ptr)
{
  if (md->temporary == DELETE_ME) {mus_sound_forget(md->in_filename); remove(md->in_filename);}
  return(0);
}

void free_mix_list(chan_info *cp)
{
  map_over_channel_mixes(cp,remove_temporary_mix_file,NULL);
}

void free_mixes(chan_info *cp)
{
  /* called in snd-data.c during chan_info cleanup */
  int i;
  mixdata *md;
  mixmark **mxm;
  mix_state *ms;
  for (i=0;i<mixdatas_ctr;i++)
    {
      md = mixdatas[i];
      if ((md) && (md->cp == cp))
	mixdatas[i] = free_mixdata(md);
    }
  ms = (mix_state *)(cp->mixes);
  if (ms)
    {
      if (ms->widget_size > 0)
	{
	  mxm = (mixmark **)(ms->widget_pool);
	  for (i=0;i<ms->widget_size;i++) 
	    if (mxm[i]) 
	      {
		release_mixmark(mxm[i]);
		FREE(mxm[i]);
	      }
	  FREE(mxm);
	}
      FREE(ms);
    }
  cp->mixes = NULL;
}

#define MXD_INCREMENT 16

int mixes(void) {return(mixdatas_ctr);}

static mixdata *make_mixdata(chan_info *cp)
{
  mixdata *md;
  if (!cp->mixes) cp->mixes = (mix_state *)CALLOC(1,sizeof(mix_state));
  md = new_mixdata();
  if (mixdatas_ctr == 0) reflect_mix_in_enved();
  md->id = mixdatas_ctr++;
  md->cp = cp;
  md->ss = cp->state;
  md->mixer = NULL;
  md->add_snd = NULL;
  md->temporary = DONT_DELETE_ME;
  md->wg = set_mixdata_context(cp);
  md->anchor = 0;
  md->changed = 0;
  md->width = 25;
  md->state = MD_TITLE;
  md->y = 0;
  md->selected_chan = 0;
  return(md);
}

static char *save_as_temp_file(snd_state *ss, MUS_SAMPLE_TYPE **raw_data, int chans, int len, int nominal_srate)
{
  char *newname;
  int format,ofd,no_space,hfd;
  format = MUS_OUT_FORMAT;
  /* newname = tempnam(temp_dir(ss),"snd_"); */
  newname = shorter_tempnam(temp_dir(ss),"snd_");
  /* we're writing our own private version of this thing, so we can use our own formats */
  hfd = snd_write_header(ss,newname,MUS_NEXT,nominal_srate,chans,28,len*chans,format,NULL,0,NULL);
  if (hfd == -1) return(NULL);
  ofd = snd_reopen_write(ss,newname);
  mus_file_set_descriptors(ofd,newname,format,4,28,chans,MUS_NEXT);
  mus_file_set_data_clipped(ofd,data_clipped(ss));
  lseek(ofd,28,SEEK_SET);
  no_space = disk_space_p(any_selected_sound(ss),ofd,len*chans*4,0);
  if (no_space == GIVE_UP)
    {
      snd_close(ofd);
      return(newname);
    }
  mus_file_write(ofd,0,len-1,chans,raw_data);
  snd_close(ofd);
  return(newname);
}

#define OFFSET_FROM_TOP 0
/* axis top border width is 10 (snd-axis.c) */

static mixdata *add_mix(chan_info *cp, int chan, int beg, int num, 
			char *mixed_chan_file, char *full_original_file, 
			int past_end, int input_chans, int temp, int out_chans, Float scaler)
{ /* temp -> delete original file also */
  mixdata *md;
  mixmark *m = NULL;
  axis_info *ap;
  char *namebuf;
  console_state *cs;
  int xspot;
  reflect_mix_active_in_menu();
  ap = cp->axis;
  md = make_mixdata(cp);     /* add active mix to chan_info list */
  md->in_chans = input_chans;
  md->out_chan = chan;
  md->orig_beg = beg;
  if (chan < input_chans)
    {
      if (out_chans > 1) 
	md->main_chan = chan;
      else md->main_chan = 0;
    }
  else md->main_chan = -1;
  md->in_samps = num;
  if (md->id < 100)
    namebuf = (char *)CALLOC(8,sizeof(char));
  else namebuf = (char *)CALLOC(12,sizeof(char));
  sprintf(namebuf,"mix%d",md->id);
  md->name = namebuf;
  md->temporary = temp;
  m = get_mixmark(cp,input_chans);
  md->mixer = m; 
  md->console_state_size = 1;
  md->states = (console_state **)CALLOC(md->console_state_size,sizeof(console_state *));
  cs = make_console_state(input_chans,cp->edit_ctr,beg,beg+num-1);
  md->current_cs = make_console_state(input_chans,cp->edit_ctr,beg,beg+num-1);
  md->states[0] = cs;
  if (chan < input_chans)
    {
      cs->scalers[chan] = scaler;
      cs->old_scalers[chan] = scaler;
    }
  cs->speed = 1.0;
  cs->scl_speed = 1.0;
  cs->old_speed = 50;
  md->curcons = 0;
  make_current_console(md);
  md->out_filename = copy_string(mixed_chan_file);
  md->in_filename = copy_string(full_original_file);
  xspot = grf_x((double)beg/(double)SND_SRATE(cp->sound),ap);
  if ((xspot+16*5 + mark_name_width(md->ss,md->name))>ap->x_axis_x1) md->state = MD_M;
  use_mixmark(md,xspot,OFFSET_FROM_TOP);
  if (md->mixer) color_mix(md,(void *)(cp->state));
  update_graph(cp,NULL);
  return(md);
}

static mixdata *file_mix_samples(int beg, int num, char *tempfile, chan_info *cp, int chan, int temp, int out_chans, char *origin, int with_console)
{
  /* open tempfile, current data, write to new temp file mixed, close others, open and use new as change case */
  /* used for clip-region temp file incoming and C-q in snd-chn.c (i.e. mix in file) so sync not relevant */
  snd_fd *csf;
  snd_state *ss;
  snd_info *sp;
  int ofd,ifd;
  char *ofile;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *zeros;
  Float scaler;
  MUS_SAMPLE_TYPE *chandata;
  int i,size,j,cursamps,in_chans,base,no_space,len,err=0;
  MUS_SAMPLE_TYPE val;
  file_info *ihdr,*ohdr;
  if (num <= 0) 
    {
      snd_error("mix %s which has %d samples?",tempfile,num);
      return(NULL);
    }

  len = current_ed_samples(cp);
  if (beg >= len)
    {
      zeros = (MUS_SAMPLE_TYPE *)CALLOC(beg-len+1,sizeof(MUS_SAMPLE_TYPE));
      insert_samples(len,beg-len+1,zeros,cp,"mix-extend");
      /* might set flag here that we need backup after file_mix_samples below (backup_edit_list(cp)) */
      /* otherwise user sees unexplained mix-extend in edit history list */
      FREE(zeros);
    }
  if (beg < 0) beg = 0;

  sp = cp->sound;
  ss = cp->state;
  ihdr = make_file_info(tempfile,ss);
  if (!ihdr) return(NULL);
  in_chans = ihdr->chans;
  if (in_chans <= chan) {base = 0; scaler = 0.0;} else {base = chan; scaler = 1.0;}
  ofile = snd_tempnam(ss);
  ohdr = make_temp_header(ss,ofile,sp->hdr,0);
  ohdr->chans = 1;
  ohdr->format = MUS_OUT_FORMAT;
  ofd = open_temp_file(ofile,1,ohdr,ss);
  if (ofd == -1) {snd_error("mix temp file %s: %s",ofile,strerror(errno)); return(NULL);}
  no_space = disk_space_p(cp->sound,ofd,num*4,0);
  if (no_space == GIVE_UP)
    {
      snd_close(ofd);
      mus_sound_forget(ofile);
      remove(ofile);
      free(ofile);
      return(NULL);
    }
  csf = init_sample_read(beg,cp,READ_FORWARD);
  ifd = snd_open_read(ss,tempfile);
  mus_file_set_descriptors(ifd,tempfile,
			   ihdr->format,mus_data_format_to_bytes_per_sample(ihdr->format),ihdr->data_location,
			   ihdr->chans,ihdr->type);
  during_open(ifd,tempfile,SND_MIX_FILE);
  if (num < MAX_BUFFER_SIZE) size = num; else size = MAX_BUFFER_SIZE;
  data = (MUS_SAMPLE_TYPE **)CALLOC(in_chans,sizeof(MUS_SAMPLE_TYPE *));
  data[base] = (MUS_SAMPLE_TYPE *)CALLOC(size,sizeof(MUS_SAMPLE_TYPE));
  chandata = data[base];
  mus_file_seek(ofd,ohdr->data_location,SEEK_SET);
  mus_file_seek(ifd,ihdr->data_location,SEEK_SET);
  if (scaler == 0.0)
    {
      for (i=0;i<num;i+=MAX_BUFFER_SIZE)
	{
	  cursamps = num-i;
	  if (cursamps > MAX_BUFFER_SIZE) cursamps = MAX_BUFFER_SIZE;
	  for (j=0;j<cursamps;j++)
	    {
	      NEXT_SAMPLE(chandata[j],csf);
	    }
	  err = mus_file_write(ofd,0,cursamps-1,1,&chandata);
	  if (err == -1) break;
	}
    }
  else
    {
      mus_file_read_chans(ifd,0,size-1,in_chans,data,(MUS_SAMPLE_TYPE *)data);
      for (i=0,j=0;i<num;i++)
	{
	  if (j == size)
	    {
	      err = mus_file_write(ofd,0,size-1,1,&chandata);
	      mus_file_read_chans(ifd,0,size-1,in_chans,data,(MUS_SAMPLE_TYPE *)data);
	      j = 0;
	      if (err == -1) break;
	    }
	  NEXT_SAMPLE(val,csf);
	  chandata[j] += val;
	  j++;
	}
      if (j > 1) mus_file_write(ofd,0,j-1,1,&chandata);
    }
  close_temp_file(ofd,ohdr,num*mus_data_format_to_bytes_per_sample(ohdr->format),sp);
  snd_close(ifd);
  free_snd_fd(csf);
  FREE(data[base]);
  FREE(data);
  free_file_info(ihdr);
  free_file_info(ohdr);
  file_change_samples(beg,num,ofile,cp,0,DELETE_ME,DONT_LOCK_MIXES,origin);
  if (with_console)
    return(add_mix(cp,chan,beg,num,ofile,tempfile,0,in_chans,temp,out_chans,1.0));
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

static int mix(int beg, int num, int chans, chan_info **cps, char *mixinfile, int temp, char *origin, Float scaler, int with_console)
{
  /* loop through out_chans cps writing the new mixed temp files and fixing up the edit trees */
  int i,id=-1,j=0;
  int *ids;
  mixdata *md;
  snd_info *sp;
  ids = (int *)CALLOC(chans,sizeof(int));
  for (i=0;i<chans;i++)
    {
      ids[i]=-1;
      sp = cps[i]->sound;
      md = file_mix_samples(beg,num,mixinfile,cps[i],i,temp,chans,origin,with_console); /* explode in this file, or mix in snd-clm.c */
      if (md) 
	{
	  if (id == -1) id = md->id;
	  if ((sp) && (sp->syncing)) ids[j++] = md->id;
	}
    }
  if (j>1) call_multichannel_mix_hook(ids,j);
  FREE(ids);
  return(id);
}

int mix_array(int beg, int num, MUS_SAMPLE_TYPE **data, chan_info **out_cps, int in_chans, int out_chans, int nominal_srate, char *origin, int with_console)
{
  /* always write to tempfile */
  char *newname;
  int id=-1;
  snd_state *ss;
  ss = out_cps[0]->state;
  /* this seems excessive -- why not just change_samples? */
  newname = save_as_temp_file(ss,data,in_chans,num,nominal_srate);
  if (newname) 
    {
      id = mix(beg,num,out_chans,out_cps,newname,DELETE_ME,origin,1.0,with_console);
      if (with_console == 0) remove(newname);
      FREE(newname);
    }
  else
    snd_error("can't save mix temp array in file (%s: %s)",newname,strerror(errno));
  return(id);
}

int mix_file(int beg, int num, char *file, chan_info **cps, int out_chans, char *origin, int with_console)
{
  /* always write to tempfile (protect section/lisp temps from possible overwrites) */
  char *newname;
  int err,id=-1;
  snd_state *ss;
  ss = cps[0]->state;
  newname = shorter_tempnam(temp_dir(ss),"snd_");
  err = copy_file(file,newname);
  if (err != SND_NO_ERROR)
    snd_error("can't save mix temp file (%s: %s)",newname,strerror(errno));
  else
    id = mix(beg,num,out_chans,cps,newname,DELETE_ME,origin,1.0,with_console);
  if (newname) FREE(newname);
  return(id);
}

int mix_complete_file(snd_info *sp, char *str, char *origin, int with_console)
{
  /* no need to save as temp here, but we do need sync info (from menu and keyboard) */
  chan_info *cp;
  chan_info **cps = NULL;
  int nc,len,chans,id=-1;
  sync_info *si = NULL;
  char *str1,*fullname = NULL;
  if ((sp) && (str) && (*str))
    {
      fullname = mus_file_full_name(str);
      nc = mus_sound_chans(fullname);
      if (nc != -1)
	{
	  len = mus_sound_samples(fullname)/nc;
	  cp = any_selected_channel(sp);
	  if (sp->syncing != 0)
	    {
	      si = snd_sync(sp->state,sp->syncing); 
	      cps = si->cps;
	      chans = si->chans;
	    }
	  else
	    {
	      cps = (chan_info **)CALLOC(1,sizeof(chan_info *));
	      cps[0] = cp;
	      chans = 1;
	    }
	  id = mix(cp->cursor,len,chans,cps,fullname,DONT_DELETE_ME,origin,1.0,with_console);
	  if (si) free_sync_info(si); else if (cps) FREE(cps);
	}
      else 
	{
	  str1 = (char *)CALLOC(512,sizeof(char));
	  sprintf(str1,"%s: %s, %s ",STR_cant_open_file,fullname,strerror(errno));
	  report_in_minibuffer(sp,str1);
	  FREE(str1);
	}
      if (fullname) FREE(fullname);
    }
  return(id);
}

#define CONSOLE_INCREMENT 8

static void extend_console_list(mixdata *md)
{
  int i,lim;
  md->curcons++;
  if (md->curcons >= md->console_state_size)
    {
      lim = md->console_state_size;
      md->console_state_size += CONSOLE_INCREMENT;
      md->states = (console_state **)REALLOC(md->states,md->console_state_size * sizeof(console_state *));
      for (i=lim;i<md->console_state_size;i++) md->states[i] = NULL;
    }
}

static int backup_mix(mixdata *md, void *ptr)
{
  int one_edit,curcons;
  console_state *cs,*curcs;
  one_edit = (*((int *)ptr));
  curcons = md->curcons;
  curcs = md->states[curcons];
  while ((md->curcons > 1) && ((md->states[md->curcons-1])->edit_ctr > one_edit))
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
  map_over_channel_mixes(cp,backup_mix,(void *)(&edit_ctr));
}


void remix_file(mixdata *md, char *origin)
{
  int beg,end,i,j,ofd = 0,size,num,no_space,use_temp_file;
  MUS_SAMPLE_TYPE val,maxy,miny;
  Float fmax,fmin;
  snd_info *cursp;
  mix_fd *add,*sub;
  snd_fd *cur,*sfb,*afb;
  snd_state *ss;
  char *ofile = NULL;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *chandata;
  file_info *ohdr = NULL;
  axis_info *ap;
  chan_info *cp;
  int old_beg,old_end,new_beg,new_end,err=0;
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
  num = end-beg+1;
  use_temp_file = (num >= MAX_BUFFER_SIZE);

  add = init_mix_read(md,0);
  sub = init_mix_read(md,1);
  cur = init_sample_read(beg,cp,READ_FORWARD);

  if (use_temp_file)
    {
      ofile = snd_tempnam(ss);
      ohdr = make_temp_header(ss,ofile,cursp->hdr,0);
      /* old header used for srate */
      ohdr->chans = 1;

      /* we can't (normally) write 16-bit files here because the current mix state may overflow -1.0..1.0 */
      ohdr->format = MUS_OUT_FORMAT;
      ofd = open_temp_file(ofile,1,ohdr,ss);
      no_space = disk_space_p(cursp,ofd,num*4,num*2);
      switch (no_space)
	{
	case GIVE_UP:
	  close_temp_file(ofd,ohdr,0,cursp);
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
	  close_temp_file(ofd,ohdr,0,cursp);
	  ohdr->format = MUS_OUT_FORMAT;
	  ofd = open_temp_file(ofile,1,ohdr,ss);
	  break;
	case NO_PROBLEM: case BLIND_LEAP: break;
	}
    }
  if (num < MAX_BUFFER_SIZE) size = num; else size = MAX_BUFFER_SIZE;
  data = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(size,sizeof(MUS_SAMPLE_TYPE));
  chandata = data[0];
  if (use_temp_file) mus_file_seek(ofd,ohdr->data_location,SEEK_SET);

  old_beg -= beg;
  old_end -= beg;
  new_beg -= beg;
  new_end -= beg;

  maxy = MUS_SAMPLE_MAX;
  miny = MUS_SAMPLE_MIN;

  /* split out special simple cases */
  if ((add->calc == C_ZERO) && (sub->calc == C_ZERO))
    {
      for (i=0,j=0;i<num;i++)
	{
	  if (j == size)
	    {
	      if (use_temp_file) err = mus_file_write(ofd,0,size-1,1,&chandata);
	      j = 0;
	      if (err == -1) break;
	    }
	  NEXT_SAMPLE(chandata[j],cur);
	  j++;
	}
    }
  else
    {
      if ((add->calc == C_STRAIGHT) && (sub->calc == C_STRAIGHT))
	{
	  sfb = sub->sfs[sub->base];
	  afb = add->sfs[add->base];
	  for (i=0,j=0;i<num;i++)
	    {
	      if (j == size)
		{
		  if (use_temp_file) err = mus_file_write(ofd,0,size-1,1,&chandata);
		  j = 0;
		  if (err == -1) break;
		}
	      NEXT_SAMPLE(chandata[j],cur);
	      if ((i>=old_beg) && (i<=old_end))
		{
		  NEXT_SAMPLE(val,sfb);
		  chandata[j] -= val;
		}
	      if ((i>=new_beg) && (i<=new_end))
		{
		  NEXT_SAMPLE(val,afb);
		  chandata[j] += val;
		}
	      if (chandata[j] > maxy) maxy = chandata[j];
	      else if (chandata[j] < miny) miny = chandata[j];
	      j++;
	    }
	}
      else
	{
	  for (i=0,j=0;i<num;i++)
	    {
	      if (j == size)
		{
		  if (use_temp_file) err = mus_file_write(ofd,0,size-1,1,&chandata);
		  j = 0;
		  if (err == -1) break;
		}
	      NEXT_SAMPLE(chandata[j],cur);
	      if ((i>=old_beg) && (i<=old_end))
		{
		  val = next_mix_sample(sub);
		  chandata[j] -= val;
		}
	      if ((i>=new_beg) && (i<=new_end))
		{
		  val = next_mix_sample(add);
		  chandata[j] += val;
		}
	      if (chandata[j] > maxy) maxy = chandata[j];
	      else if (chandata[j] < miny) miny = chandata[j];
	      j++;
	    }
	}
    }

  if (use_temp_file)
    {
      if (j > 0) mus_file_write(ofd,0,j-1,1,&chandata);
      close_temp_file(ofd,ohdr,num*mus_data_format_to_bytes_per_sample(ohdr->format),cursp);
      free_file_info(ohdr);
    }
  free_snd_fd(cur);
  free_mix_fd(add);
  free_mix_fd(sub);

  if (use_temp_file)
    file_change_samples(beg,num,ofile,cp,0,DELETE_ME,DONT_LOCK_MIXES,origin);
  else change_samples(beg,num,data[0],cp,DONT_LOCK_MIXES,origin);
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
  fmax = MUS_SAMPLE_TO_FLOAT(maxy);
  fmin = MUS_SAMPLE_TO_FLOAT(miny);
  if ((fmax > ap->ymax) || (fmin < ap->ymin)) 
    {
      if (fmax < -fmin) fmax = -fmin; 
      ap->y0 = -fmax;
      ap->y1 = fmax;
      ap->ymin = -fmax;
      ap->ymax = fmax;
    }
  update_graph(cp,NULL);
}

/* ---------------- MIX GRAPHS ---------------- */

static int make_temporary_amp_env_mixed_graph(chan_info *cp, axis_info *ap, mixdata *md, Float samples_per_pixel,
					      int newbeg, int newend, int oldbeg, int oldend)
{
  /* temp graph using cp->amp_env and mix (sample-by-sample) data */
  Float main_start,new_start,old_start;
  mix_fd *new_fd,*old_fd;
  MUS_SAMPLE_TYPE val,new_ymin,new_ymax,old_ymin,old_ymax,main_ymin,main_ymax;
  Float xi,xf,xfinc;
  int lastx,lo,hi,main_loc,j,i;
  env_info *ep;

  lo = ap->losamp;
  hi = ap->hisamp;

  new_fd = init_mix_read(md,0);
  old_fd = init_mix_read(md,1);

  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((Float)(ap->losamp)/(Float)(ep->samps_per_bin));
  main_start = (Float)ap->losamp;
  
  if ((lo > newbeg) && (lo < newend)) 
    {
      for (i=newbeg;i<lo;i++) next_mix_sample(new_fd);
      new_start = lo;
    }
  else new_start = newbeg;
  if ((lo > oldbeg) && (lo < oldend)) 
    {
      for (i=oldbeg;i<lo;i++) next_mix_sample(old_fd);
      old_start = lo;
    }
  else old_start = oldbeg;

  lastx = ap->x_axis_x0;
  xfinc = samples_per_pixel / (Float)SND_SRATE(cp->sound);

  for (j=0,xi=(Float)lo,xf=ap->x0;xi<(Float)hi;xi+=samples_per_pixel,lastx++,xf+=xfinc,j++)
    {
      main_ymin = MUS_MIX_MAX;
      main_ymax = MUS_MIX_MIN;
      old_ymin = MUS_MIX_MAX;
      old_ymax = MUS_MIX_MIN;
      new_ymin = MUS_MIX_MAX;
      new_ymax = MUS_MIX_MIN;
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
	  new_ymin = 0;
	  new_ymax = 0;
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
	  old_ymin = 0;
	  old_ymax = 0;
	}

      while (main_start <= xi)
	{
	  if (ep->data_min[main_loc] < main_ymin) main_ymin=ep->data_min[main_loc];
	  if (ep->data_max[main_loc] > main_ymax) main_ymax=ep->data_max[main_loc];
	  if (main_loc < (ep->amp_env_size - 1))
	    main_loc++;
	  main_start += ep->samps_per_bin;
	}

      set_grf_points(lastx,j,
		     grf_y(MUS_SAMPLE_TO_FLOAT(main_ymin - old_ymin + new_ymin),ap),
		     grf_y(MUS_SAMPLE_TO_FLOAT(main_ymax - old_ymax + new_ymax),ap));

    }

  erase_and_draw_both_grf_points(md->ss,md->wg,cp,j);

  free_mix_fd(new_fd);
  free_mix_fd(old_fd);
  return(j);
}

static int make_temporary_amp_env_graph(chan_info *cp, axis_info *ap, mixdata *md, Float samples_per_pixel,
					int newbeg, int newend, int oldbeg, int oldend)
{
  /* temp graph using cp->amp_env and mix input amp envs */
  Float main_start,new_start,old_start;
  mix_fd *new_min_fd,*new_max_fd,*old_min_fd,*old_max_fd;
  MUS_SAMPLE_TYPE new_ymin,new_ymax,old_ymin,old_ymax,main_ymin,main_ymax;
  MUS_SAMPLE_TYPE new_high,new_low,old_high,old_low;
  Float xi,xf,xfinc,x;
  int lastx,lo,hi,main_loc,j;
  env_info *ep;

  lo = ap->losamp;
  hi = ap->hisamp;

  new_min_fd = init_mix_input_amp_env_read(md,0,0); /* not old, not hi */
  new_max_fd = init_mix_input_amp_env_read(md,0,1); /* not old, hi */
  old_min_fd = init_mix_input_amp_env_read(md,1,0); /* old, not hi */
  old_max_fd = init_mix_input_amp_env_read(md,1,1); /* old, hi */

  ep = cp->amp_envs[cp->edit_ctr];
  main_loc = (int)((Float)(ap->losamp)/(Float)(ep->samps_per_bin));
  main_start = (Float)ap->losamp;

  if (lo > newbeg) 
    {
      for (x=(Float)lo;x<(Float)newbeg;x+=new_max_fd->samps_per_bin) 
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
      new_ymin=0;
      new_ymax=0;
      new_start = (Float)newbeg;
    }

  if ((lo > oldbeg) && (oldend > lo))
    {
      for (x=(Float)lo;x<(Float)oldbeg;x+=old_max_fd->samps_per_bin) 
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
      old_ymin=0;
      old_ymax=0;
    }

  lastx = ap->x_axis_x0;
  xfinc = samples_per_pixel / (Float)SND_SRATE(cp->sound);

  for (j=0,xi=(Float)lo,xf=ap->x0;xi<(Float)hi;xi+=samples_per_pixel,lastx++,xf+=xfinc,j++)
    {
      main_ymin = MUS_MIX_MAX;
      main_ymax = MUS_MIX_MIN;
      old_ymin = MUS_MIX_MAX;
      old_ymax = MUS_MIX_MIN;
      new_ymin = MUS_MIX_MAX;
      new_ymax = MUS_MIX_MIN;
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
	  new_ymin = MUS_SAMPLE_0;
	  new_ymax = MUS_SAMPLE_0;
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
	  old_ymin = MUS_SAMPLE_0;
	  old_ymax = MUS_SAMPLE_0;
	}

      while (main_start <= xi)
	{
	  if (ep->data_min[main_loc] < main_ymin) main_ymin=ep->data_min[main_loc];
	  if (ep->data_max[main_loc] > main_ymax) main_ymax=ep->data_max[main_loc];
	  if (main_loc < (ep->amp_env_size - 1))
	    main_loc++;
	  main_start += ep->samps_per_bin;
	}

      set_grf_points(lastx,j,
		     grf_y(MUS_SAMPLE_TO_FLOAT(main_ymin - old_ymin + new_ymin),ap),
		     grf_y(MUS_SAMPLE_TO_FLOAT(main_ymax - old_ymax + new_ymax),ap));

    }

  erase_and_draw_both_grf_points(md->ss,md->wg,cp,j);

  free_mix_fd(new_min_fd);
  free_mix_fd(new_max_fd);
  free_mix_fd(old_min_fd);
  free_mix_fd(old_max_fd);
  return(j);
}

void make_temporary_graph(chan_info *cp, mixdata *md, console_state *cs)
{
  int oldbeg,newbeg,oldend,newend;
  int i,j,samps,xi;
  int widely_spaced;
  axis_info *ap;
  snd_info *sp;
  mix_context *ms;
  snd_state *ss;
  Float samples_per_pixel,xf;
  double x,incr,initial_x;
  MUS_SAMPLE_TYPE ina,ymin,ymax,val;
  int lo,hi;
  snd_fd *sf = NULL,*sfb,*afb;
  mix_fd *add = NULL,*sub = NULL;
  int x_start,x_end;
  double start_time,cur_srate;
  ss = cp->state;
  if (!(movies(ss))) return;
  /* if fft is being displayed, this does not update it as we drag the mix because the fft-data reader
   * (apply_fft_window in snd-fft.c) reads the current to-be-fft'd data using init_sample_read, and
   * that sees the old data in this case (also if the fft is large enough, it looks for data beyond
   * the current graph right edge, but the mix dragger stops at the edge).
   */

  ms = md->wg;
  oldbeg = cs->orig;
  newbeg = cs->beg;
  oldend = cs->end;
  newend = newbeg + cs->len;
  sp = cp->sound;
  ap = cp->axis;
  cur_srate = (double)SND_SRATE(sp);
  start_time = (double)(ap->losamp)/cur_srate;
  x_start = grf_x(start_time,ap);
  x_end = grf_x((double)(ap->hisamp)/cur_srate,ap);
  lo = ap->losamp;
  hi = ap->hisamp;
  samps = ap->hisamp-ap->losamp+1;
  samples_per_pixel = (Float)(samps-1)/(Float)(x_end-x_start);
  if ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE))
    {
      sf = init_sample_read(ap->losamp,cp,READ_FORWARD);
      add = init_mix_read(md,0);
      sub = init_mix_read(md,1);
      if ((oldbeg < lo) && (lo < oldend)) {for (i=oldbeg;i<lo;i++) next_mix_sample(sub);}
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
      for (j=0,i=lo,x=initial_x;i<=hi;i++,j++,x+=incr)
	{
	  NEXT_SAMPLE(ina,sf);
	  if ((i >= oldbeg) && (i <= oldend)) ina -= next_mix_sample(sub);
	  if ((i >= newbeg) && (i <= newend)) ina += next_mix_sample(add);
	  if (widely_spaced)
	    set_grf_point((int)x,j,grf_y(MUS_SAMPLE_TO_FLOAT(ina),ap));
	  else set_grf_point(grf_x(x,ap),j,grf_y(MUS_SAMPLE_TO_FLOAT(ina),ap));
	}
      erase_and_draw_grf_points(ss,md->wg,cp,j);
    }
  else
    {
      if (amp_env_usable(cp,samples_per_pixel,ap->hisamp))
	{
	  if (mix_input_amp_env_usable(md,samples_per_pixel))
	    j = make_temporary_amp_env_graph(cp,ap,md,samples_per_pixel,newbeg,newend,oldbeg,oldend);
	  else j = make_temporary_amp_env_mixed_graph(cp,ap,md,samples_per_pixel,newbeg,newend,oldbeg,oldend);
	}
      else
	{
	  sf = init_sample_read(ap->losamp,cp,READ_FORWARD);
	  add = init_mix_read(md,0);
	  sub = init_mix_read(md,1);
	  if ((oldbeg < lo) && (lo < oldend)) {for (i=oldbeg;i<lo;i++) next_mix_sample(sub);}
	  j = 0;      /* graph point counter */
	  x=ap->x0;
	  xi=grf_x(x,ap);
	  i=lo;
	  xf=0.0;     /* samples per pixel counter */
	  ymin = MUS_MIX_MAX;
	  ymax = MUS_MIX_MIN;

	  if ((add->calc == C_ZERO) && (sub->calc == C_ZERO))
	    {
	      while (i<=hi)
		{
		  NEXT_SAMPLE(ina,sf);
		  if (ina > ymax) ymax = ina;
		  if (ina < ymin) ymin = ina;
		  xf+=1.0;
		  i++;
		  if (xf>samples_per_pixel)
		    {
		      set_grf_points(xi,j,grf_y(MUS_SAMPLE_TO_FLOAT(ymin),ap),grf_y(MUS_SAMPLE_TO_FLOAT(ymax),ap));
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      ymin = MUS_MIX_MAX;
		      ymax = MUS_MIX_MIN;
		    }
		}
	    }
	  else
	    {
	      if ((add->calc == C_STRAIGHT) && (sub->calc == C_STRAIGHT))
		{
		  sfb = sub->sfs[sub->base];
		  afb = add->sfs[add->base];
		  while (i<=hi)
		    {
		      NEXT_SAMPLE(ina,sf);
		      if ((i >= oldbeg) && (i <= oldend)) {NEXT_SAMPLE(val,sfb); ina -= val;}
		      if ((i >= newbeg) && (i <= newend)) {NEXT_SAMPLE(val,afb); ina += val;}
		      if (ina > ymax) ymax = ina;
		      if (ina < ymin) ymin = ina;
		      xf+=1.0;
		      i++;
		      if (xf>samples_per_pixel)
			{
			  set_grf_points(xi,j,grf_y(MUS_SAMPLE_TO_FLOAT(ymin),ap),grf_y(MUS_SAMPLE_TO_FLOAT(ymax),ap));
			  xi++;
			  j++;
			  xf -= samples_per_pixel;
			  ymin = MUS_MIX_MAX;
			  ymax = MUS_MIX_MIN;
			}
		    }
		}
	      else
		{
		  while (i<=hi)
		    {
		      NEXT_SAMPLE(ina,sf);
		      if ((i >= oldbeg) && (i <= oldend)) ina -= next_mix_sample(sub);
		      if ((i >= newbeg) && (i <= newend)) ina += next_mix_sample(add);
		      if (ina > ymax) ymax = ina;
		      if (ina < ymin) ymin = ina;
		      xf+=1.0;
		      i++;
		      if (xf>samples_per_pixel)
			{
			  set_grf_points(xi,j,grf_y(MUS_SAMPLE_TO_FLOAT(ymin),ap),grf_y(MUS_SAMPLE_TO_FLOAT(ymax),ap));
			  xi++;
			  j++;
			  xf -= samples_per_pixel;
			  ymin = MUS_MIX_MAX;
			  ymax = MUS_MIX_MIN;
			}
		    }
		}
	    }
	  erase_and_draw_both_grf_points(ss,md->wg,cp,j);
	}
    }
  if (sf) free_snd_fd(sf);
  if (add) free_mix_fd(add);
  if (sub) free_mix_fd(sub);
  ms->lastpj = j;
}

static int display_mix_amp_env(mixdata *md, Float scl, int yoff, int newbeg, int newend, chan_info *cp, axis_info *ap, int draw)
{
  /* need min and max readers */
  mix_fd *min_fd,*max_fd;
  int hi,lo,j,lastx,newx;
  MUS_SAMPLE_TYPE ymin,ymax,high=MUS_SAMPLE_0,low=MUS_SAMPLE_0;
  Float sum,xend,xstart,xstep;
  min_fd = init_mix_input_amp_env_read(md,0,0); /* not old, not hi */
  max_fd = init_mix_input_amp_env_read(md,0,1); /* not old, hi */
  lo = ap->losamp;
  hi = ap->hisamp;

  /* mix starts at newbeg, current display starts at lo,
     mix goes to newend, current display goes to hi,
     cp->amp_envs[cp->edit_ctr]->samps_per_bin is the original's amp_env step size
     mf->samps_per_bin is the mix input step size
  */

  if (lo > newbeg) 
    {
      for (sum=(Float)lo;sum<(Float)newbeg;sum+=max_fd->samps_per_bin) 
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
      ymin = MUS_MIX_MAX;
      ymax = MUS_MIX_MIN;
    }

  if (hi < newend)
    xend = ap->x1;
  else xend = (Float)(newend)/(Float)SND_SRATE(cp->sound);
  xstep = (Float)(max_fd->samps_per_bin)/(Float)SND_SRATE(cp->sound);
  lastx = grf_x(xstart,ap);
  for (j=0;xstart<xend;xstart+=xstep)
    {
      low = next_mix_sample(min_fd);
      high = next_mix_sample(max_fd);
      newx = grf_x(xstart,ap);
      if (newx > lastx)
	{
	  set_grf_points(lastx,j,(int)(yoff - scl*MUS_SAMPLE_TO_FLOAT(ymin)), (int)(yoff - scl*MUS_SAMPLE_TO_FLOAT(ymax)));
	  lastx = newx;
	  j++;
	  ymin=low;
	  ymax=high;
	}
      else
	{
	  if (high > ymax) ymax = high;
	  if (low < ymin) ymin = low;
	}
    }
  draw_both_grf_points(md->ss,(draw) ? mix_waveform_context(cp) : erase_context(cp),j,ap);
  free_mix_fd(min_fd);
  free_mix_fd(max_fd);
  return(j);
}

int display_mix_waveform(chan_info *cp, mixdata *md, console_state *cs, int yoff, int yscale, int draw)
{
  int newbeg,newend,endi;
  Float scl;
  int i,j=0,samps,xi;
  int widely_spaced;
  axis_info *ap;
  snd_info *sp;
  snd_state *ss;
  Float samples_per_pixel,xf;
  double x,incr,initial_x;
  MUS_SAMPLE_TYPE ina,ymin,ymax;
  int lo,hi;
  mix_fd *add = NULL;
  int x_start,x_end;
  double start_time,cur_srate;
  ss = cp->state;
  newbeg = cs->beg;
  newend = newbeg + cs->len;
  sp = cp->sound;
  ap = cp->axis;
  if ((ap->y_axis_y0 - ap->y_axis_y1) < yscale) return(0);
  if (sp) 
    cur_srate = (double)SND_SRATE(sp);
  else cur_srate = (double)SND_SRATE((md->cp)->sound);
  start_time = (double)(ap->losamp)/cur_srate;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  lo = ap->losamp;
  hi = ap->hisamp;
  if (newend > hi) newend = hi;
  samps = ap->hisamp-ap->losamp+1;
  samples_per_pixel = (Float)(samps-1)/(Float)(x_end-x_start);
  scl = yscale;
  if (sp) mix_waveform_context(cp);
  if ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE))
    {
      add = init_mix_read(md,0);
      if (lo > newbeg) 
	{
	  for (i=newbeg;i<lo;i++) next_mix_sample(add);
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
      x = initial_x + (incr*(newbeg-lo));
      for (j=0,i=newbeg;i<=newend;i++,j++,x+=incr)
	{
	  ina = next_mix_sample(add);
	  if (widely_spaced)
	    set_grf_point((int)x,j,(int)(yoff - scl*MUS_SAMPLE_TO_FLOAT(ina)));
	  else set_grf_point(grf_x(x,ap),j, (int)(yoff - scl*MUS_SAMPLE_TO_FLOAT(ina)));
	}
      if (sp)
	draw_grf_points(ss,(draw) ? mix_waveform_context(cp) : erase_context(cp),j,ap,ungrf_y(ap,yoff));
    }
  else
    {
      if (mix_input_amp_env_usable(md,samples_per_pixel))
	j = display_mix_amp_env(md,scl,yoff,newbeg,newend,cp,ap,draw);
      else
	{
	  add = init_mix_read(md,0);
	  if (lo > newbeg) 
	    {
	      for (i=newbeg;i<lo;i++) next_mix_sample(add);
	      newbeg = lo;
	    }
	  j = 0;      /* graph point counter */
	  x=ap->x0;
	  xi=grf_x(x,ap);
	  xf=0.0;     /* samples per pixel counter */
	  ymin = MUS_MIX_MAX;
	  ymax = MUS_MIX_MIN;
	  if (newend < hi) endi = newend; else endi = hi;
	  for (i=lo;i<newbeg;i++)
	    {
	      xf+=1.0;
	      if (xf>samples_per_pixel)
		{
		  xi++;
		  xf -= samples_per_pixel;
		}
	    }
	  for (i=newbeg;i<=endi;i++)
	    {
	      ina = next_mix_sample(add);
	      if (ina > ymax) ymax = ina;
	      if (ina < ymin) ymin = ina;
	      xf+=1.0;
	      if (xf>samples_per_pixel)
		{
		  set_grf_points(xi,j,(int)(yoff - scl*MUS_SAMPLE_TO_FLOAT(ymin)), (int)(yoff - scl*MUS_SAMPLE_TO_FLOAT(ymax)));
		  j++;
		  ymin = MUS_MIX_MAX;
		  ymax = MUS_MIX_MIN;
		  xi++;
		  xf -= samples_per_pixel;
		}
	    }
	  if (sp) 
	    draw_both_grf_points(ss,(draw) ? mix_waveform_context(cp) : erase_context(cp),j,ap);
	}
    }
  if (sp) copy_context(cp);
  if (add) free_mix_fd(add);
  return(j);
}


/* ---------------- MIX ACTIONS ---------------- */

static int turn_off_mix(mixdata *md, void *ptr)
{
  if (md->mixer) {release_mixmark(md->mixer); md->mixer = NULL;}
  return(0);
}

static int turn_off_mixes(chan_info *cp, void *ptr)
{
  map_over_channel_mixes(cp,turn_off_mix,NULL);
  return(0);
}

void update_all_consoles(snd_state *ss)
{
  /* called to turn console display on/off in snd-xmenu (show_consoles) */
  if (!(show_mix_consoles(ss))) 
    map_over_chans(ss,turn_off_mixes,NULL);
  else map_over_chans(ss,update_graph,NULL);
}

mixdata *active_mix(chan_info *cp)
{
  mixdata *md,*curmd = NULL;
  console_state *cs;
  axis_info *ap;
  int lo,hi,spot,i,curmaxctr = -1;
  ap = cp->axis;
  lo = ap->losamp;
  hi = ap->hisamp;
  for (i=0;i<mixdatas_ctr;i++)
    {
      md = mixdatas[i];
      if ((md) && (md->cp == cp))
	{
	  cs = md->current_cs;
	  spot = cs->orig + md->anchor;
	  if ((spot >= lo) && (spot <= hi))
	    {
	      /* this mix is within current graph window -- look for last edited mix */
	      if (cs->edit_ctr > curmaxctr)
		{
		  curmaxctr = cs->edit_ctr;
		  curmd = md;
		}}}}
  return(curmd);
}

int mix_beg(chan_info *cp)
{
  mixdata *md;
  console_state *cs;
  md = active_mix(cp);
  if (md) 
    {
      cs = md->current_cs;
      if (cs) return(cs->orig + md->anchor);
    }
  return(-1);
}

static console_state *backup_console(chan_info *cp, mixdata *md)
{
  console_state *cs;
  /* undo -- look for first (last in list) cs with cs->edit_ctr<=cp->edit_ctr console */
  cs = md->states[md->curcons];
  while ((md->curcons > 0) && (cs->edit_ctr > cp->edit_ctr)) 
    {
      md->curcons--; 
      cs = md->states[md->curcons];
    }
  if (cs->edit_ctr > cp->edit_ctr) return(NULL);
  make_current_console(md);
  md->changed = 1;
  return(md->current_cs);
}

static console_state *restore_console(chan_info *cp, mixdata *md)
{
  console_state *cs;
  /* redo -- looking for the console that brackets cp->edit_ctr */
  cs = md->states[md->curcons];
  while ((md->curcons < (md->console_state_size-1)) && (cs->edit_ctr < cp->edit_ctr) && (md->states[md->curcons+1]))
    {
      md->curcons++; 
      cs = md->states[md->curcons];
    }
  if ((md->curcons > 0) && (cs->edit_ctr > cp->edit_ctr)) md->curcons--;
  make_current_console(md);
  md->changed = 1;
  return(md->current_cs);
}

void release_mixes(chan_info *cp)
{
  mixdata *md;
  int i;
  for (i=0;i<mixdatas_ctr;i++)
    {
      md = mixdatas[i];
      if ((md) && (md->cp == cp) && (md->mixer)) release_mixmark(md->mixer);
    }
}

void regraph_all_mixmarks(chan_info *cp)
{
  mixdata *md;
  mixmark *m;
  int i;
  if (cp)
    {
      for (i=0;i<mixdatas_ctr;i++)
	{
	  md = mixdatas[i];
	  if ((md) && (md->cp == cp))
	    {
	      m = md->mixer;
	      if (m) release_mixmark(m);
	      if (md->wg) FREE(md->wg);
	      md->wg = set_mixdata_context(cp);
	    }
	}
    }
}

#define MIX_Y_OFFSET 10
#define MIX_Y_INCR 10
/* may need fancier decision here -- leave moved mix at its current height and adjust others */


void display_channel_mixes(chan_info *cp)
{
  /* called in display_channel_data if show_mix_consoles(ss) and cp->mixes
   * this needs to spin through the mixes, 
   *   un-manage those whose mix has wandered off screen, and release the associated widgets,
   *   and re-manage those that are now active, grabbing widgets if necessary.
   */
  snd_state *ss;
  mixdata *md;
  mixmark *m;
  console_state *cs;
  axis_info *ap;
  int lo,hi,spot,i,xspot,turnover,y,yspot,combined;
  combined = (((snd_info *)(cp->sound))->combining != CHANNELS_SEPARATE);
  ss = cp->state;
  ap = cp->axis;
  lo = ap->losamp;
  hi = ap->hisamp;
  turnover = (int)(ap->height * 0.5);
  y = MIX_Y_OFFSET;
  for (i=0;i<mixdatas_ctr;i++)
    {
      md = mixdatas[i];
      if ((md) && (md->cp == cp))
	{
	  m = md->mixer;
	  if ((!m) || (!(m->moving)))
	    {
	      cs = md->current_cs;
	      if (cs->edit_ctr > cp->edit_ctr) 
		cs = backup_console(cp,md);
	      else
		if (cs->edit_ctr < cp->edit_ctr)
		  cs = restore_console(cp,md);
	      if ((cs) && (!cs->locked))
		{
		  spot = cs->orig + md->anchor;
		  if ((cs->edit_ctr <= cp->edit_ctr) && (spot >= lo) && (spot <= hi))
		    {
		      xspot = grf_x((double)spot/(double)SND_SRATE(cp->sound),ap);
		      if (md->y == 0)
			yspot = OFFSET_FROM_TOP+y+ap->y_offset;
		      else yspot = md->y;
		      if (((xspot + md->width) <= ap->x_axis_x1) &&      /* not cut off on right */
			  (!combined))
			{
			  if (!m)
			    {
			      m = get_mixmark(cp,md->in_chans);
			      md->mixer = m;
			      use_mixmark(md,xspot,yspot);
			      md->changed = 0;
			    }
			  move_mixmark(m,xspot,yspot);
			  color_mix(md,(void *)(cp->state));
			  if (show_mix_waveforms(ss)) draw_mix_waveform(md,xspot,yspot);
			  y += MIX_Y_INCR;
			  if (y >= turnover) y=0;
			  if (md->changed) 
			    {
			      if (md->state == MD_CS)
				fixup_mixmark(md);
			      else
				if (md->state == MD_TITLE)
				  set_mix_title_beg(md,m);
			    }
			}
		      else if (m) release_mixmark(m);
		    }
		  else
		    {
		      if ((m) && (!(m->moving)))
			release_mixmark(m); /* if we have widgets deactivate and release them */
		    }
		}
	      else
		if (m) release_mixmark(m);
	    }
	}
    }
}

static int lt_beg,lt_end;

static int lock_mixes(mixdata *md, void *ptr)
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
	  md->states[md->curcons] = (console_state *)CALLOC(1,sizeof(console_state));
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
  map_over_channel_mixes(cp,lock_mixes,NULL);
}

void release_pending_mixes(chan_info *cp, int edit_ctr)
{
  /* look for mixes that have been cancelled by undo followed by unrelated edit */
  mixdata *md;
  console_state *cs;
  int i;
  for (i=0;i<mixdatas_ctr;i++)
    {
      md = mixdatas[i];
      if ((md) && (md->cp == cp))
	{
	  cs = md->states[0];
	  if (cs->edit_ctr >= edit_ctr) free_mixdata(md);
	}
    }
}

static int update_mix(mixdata *md, void *ptr)
{
  console_state *cur;
  int i,lim;
  lim = md->curcons;
  cur = md->states[lim];
  /* make states[0] and [1] both look like cur, collapse rest */
  if (lim>0)
    {
      for (i=0;i<lim;i++)
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
  map_over_channel_mixes(cp,update_mix,NULL);
}

static int mix_sound(snd_state *ss, snd_info *sp, char *file, int beg, Float scaler)
{
  /* returns mix id, or -1 if failure */
  char *buf;
  if (snd_probe_file(file) == FILE_EXISTS)
    {
      return(mix(beg,mus_sound_frames(file),sp->nchans,sp->chans,file,DONT_DELETE_ME,S_mix_sound,scaler,with_mix_consoles(ss))); 
    }
  else 
    {
      buf = (char *)CALLOC(256,sizeof(char));
      sprintf(buf,"mix_overlay can't find %s!",file);
      report_in_minibuffer(sp,buf);
      FREE(buf);
    }
  return(-1);
}



/* ---------------- paste/delete move affected mixes ---------------- */

typedef struct {
  int beg,change;
  chan_info *cp;
  axis_info *ap;
} mixrip;

static int ready_mix(mixdata *md)
{
  console_state *cs;
  chan_info *cp;
  cp = md->cp;
  cs = md->states[md->curcons];
  return(((cs) && (!(cs->locked)) && (cs->edit_ctr <= cp->edit_ctr)));
}

static int ripple_mixes_1(mixdata *md, void *ptr)
{
  console_state *cs,*ncs;
  mixmark *m;
  int xspot;
  snd_state *ss;
  mixrip *data;
  chan_info *cp;
  data = (mixrip *)ptr;
  cp = data->cp;
  cs = md->current_cs;
  if ((cs) && (!(cs->locked)) && (cs->beg > data->beg) && (ready_mix(md)))
    {
      ss = md->ss;
      ncs = copy_console(cs);
      ncs->edit_ctr = cp->edit_ctr;
      ncs->orig = cs->beg + data->change;
      ncs->beg = ncs->orig;
      ncs->end = ncs->beg + cs->len - 1;
      m = md->mixer;
      if ((m) && (show_mix_waveforms(ss))) erase_mix_waveform(md,m->x,m->y);
      extend_console_list(md);
      md->states[md->curcons] = ncs;
      make_current_console(md);
      if ((m) && (m->inuse))
	{
	  xspot = grf_x((double)(ncs->beg)/(double)SND_SRATE(cp->sound),data->ap);
	  move_mix_x(m,xspot);
	  set_mix_title_beg(md,m);
	  if (show_mix_waveforms(ss)) draw_mix_waveform(md,m->x,m->y);
	}
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
      mp = (mixrip *)CALLOC(1,sizeof(mixrip));
      mp->beg = beg;
      mp->change = change;
      mp->cp = cp;
      mp->ap = cp->axis;
      map_over_channel_mixes(cp,ripple_mixes_1,(void *)mp);
      FREE(mp);
    }
}


/* ---------------- C-x C-j (go to mix) ---------------- */

static int compare_consoles(const void *umx1, const void *umx2)
{
  int mx1,mx2;
  mx1 = (*((int *)umx1));
  mx2 = (*((int *)umx2));
  if (mx1 > mx2) return(1);
  if (mx1 == mx2) return(0);
  return(-1);
}

int goto_mix(chan_info *cp,int count)
{
  int i,j,k,samp;
  mixdata *md;
  int *css;
  console_state *cs;
  if ((!cp) || (!cp->mixes)) return(CURSOR_IN_VIEW);
  k=0;
  for (i=0;i<mixdatas_ctr;i++)
    {
      md = mixdatas[i];
      if ((md) && (md->cp == cp)) k++;
    }
  if (k>0)
    {
      css = (int *)CALLOC(k,sizeof(int));
      j=0;
      for (i=0;i<mixdatas_ctr;i++)
	{
	  md = mixdatas[i];
	  if ((md) && (md->cp == cp))
	    {
	      cs = md->current_cs;
	      css[j] = cs->orig;
	      j++;
	    }
	}
      qsort((void *)css,j,sizeof(int),compare_consoles);
      /* now find where we are via cp->cursor and go forward or back as per count */
      samp = cp->cursor;
      k = j-1;
      for (i=0;i<j;i++)	{if (css[i] > samp) {k=i; break;}}
      if (css[k] != samp) {if (count < 0) k++; else k--;}
      k+=count;
      if (k<0) k=0;
      if (k>=j) k=j-1;
      samp = css[k];
      cp->cursor = samp;
      FREE(css);
      if ((count > 0) && (samp > (cp->axis)->hisamp)) return(CURSOR_IN_MIDDLE);
      if ((count < 0) && (samp < (cp->axis)->losamp)) return(CURSOR_IN_MIDDLE);
      return(CURSOR_IN_VIEW);
    }
  return(CURSOR_IN_VIEW);
}


static int mix_ok(int n) 
{
  mixdata *md; 
  md = md_from_int(n); 
  return((md) && 
	 (md->states) && 
	 (md->states[0]) && 
	 (md->cp) &&
	 (((md->states[0])->edit_ctr) <= ((md->cp)->edit_ctr)));
}

int any_mix_id(void)
{
  int i;
  for (i=0;i<mixdatas_ctr;i++) if (mix_ok(i)) return(i);
  return(-1);
}

void draw_mix_waveform(mixdata *md, int xspot, int yspot) 
{
  display_mix_waveform(md->cp,md,md->current_cs,yspot+20,mix_waveform_height(md->ss),TRUE);
}

void erase_mix_waveform(mixdata *md, int xspot, int yspot) 
{
  display_mix_waveform(md->cp,md,md->current_cs,yspot+20,mix_waveform_height(md->ss),FALSE);
}



/* ---------------- TRACKS ---------------- */
/* track reader: an array of mix readers with state: active, waiting, null (done) */

typedef struct {
  int mixes;
  int *state,*len;
  mix_fd **fds;
} track_fd;

static track_fd *init_track_reader(chan_info *cp, int track_num, int global) /* edit-position? direction? */
{
  track_fd *fd = NULL;
  int mixes=0,i,mix,track_beg;
  mixdata *md;
  console_state *cs;
  track_beg = current_ed_samples(cp);
  for (i=0;i<mixdatas_ctr;i++) 
    {
      if (mix_ok(i))
	{
	  md = mixdatas[i];
	  if (md->track == track_num)
	    {
	      if (md->cp == cp) mixes++;
	      cs = md->current_cs;
	      if ((global) || (md->cp == cp))
		if (cs->orig < track_beg) track_beg = cs->orig;
	    }
	}
    }
  if (mixes > 0)
    {
      fd = (track_fd *)CALLOC(1,sizeof(track_fd));
      fd->mixes = mixes;
      fd->state = (int *)CALLOC(mixes,sizeof(int));
      fd->len = (int *)CALLOC(mixes,sizeof(int));
      fd->fds = (mix_fd **)CALLOC(mixes,sizeof(mix_fd *));
      mix=0;
      for (i=0;i<mixdatas_ctr;i++)
	{
	  if (mix_ok(i))
	    {
	      md = mixdatas[i];
	      if ((md->track == track_num) && (md->cp == cp))
		{
		  fd->fds[mix] = init_mix_read(md,FALSE);
		  cs = md->current_cs;
		  fd->state[mix] = cs->orig - track_beg;
		  fd->len[mix] = cs->len;
		  mix++;
		}
	    }
	}
    }
  return(fd);
}

static track_fd *free_track_fd(track_fd *fd)
{
  int i;
  if (fd)
    {
      if (fd->fds)
	{
	  for (i=0;i<fd->mixes;i++)
	    {
	      if (fd->fds[i]) fd->fds[i] = free_mix_fd(fd->fds[i]);
	    }
	  FREE(fd->fds);
	}
      if (fd->state) FREE(fd->state);
      if (fd->len) FREE(fd->len);
      FREE(fd);
    }
  return(NULL);
}

static MUS_SAMPLE_TYPE next_track_sample(track_fd *fd)
{
  int i;
  MUS_SAMPLE_TYPE sum=MUS_SAMPLE_0;
  if (fd)
    {
      for (i=0;i<fd->mixes;i++)
	{
	  if ((fd->fds[i]) && (fd->len[i] > 0))
	    {
	      if (fd->state[i] <= 0)
		{
		  sum += next_mix_sample(fd->fds[i]);
		  fd->len[i]--;
		  if (fd->len[i] <= 0) fd->fds[i] = free_mix_fd(fd->fds[i]);
		}
	      else fd->state[i]--;
	    }
	}
    }
  return(sum);
}

static void play_track(snd_state *ss, chan_info **ucps, int chans, int track_num)
{
  track_fd **fds;
  chan_info **cps;
  chan_info *locp;
  int playfd,i,j,k,n,samps,chan=0,happy=0,need_free=0;
  short *buf;
  if (ucps == NULL)
    {
      chans = active_channels(ss,TRUE); /* true: count virtual chans as well */
      cps = (chan_info **)CALLOC(chans,sizeof(chan_info *));
      need_free = 1;
      chan = 0;
      for (i=0;i<mixdatas_ctr;i++) 
	if ((mix_ok(i)) && (mixdatas[i]->track == track_num))
	  {
	    locp = mixdatas[i]->cp;
	    happy = 0;
	    for (j=0;j<chan;j++) if (cps[j] == locp) {happy = 1; break;}
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
  fds = (track_fd **)CALLOC(chans,sizeof(track_fd *));
  buf = (short *)CALLOC(chans * 256,sizeof(short));
  for (i=0;i<chans;i++)
    {
      fds[i] = init_track_reader(cps[i],track_num,need_free);
      for (n=0;n<fds[i]->mixes;n++)
	{
	  j = fds[i]->state[n]+fds[i]->len[n];
	  if (j > samps) samps = j;
	}
    }
  playfd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss),SND_SRATE(cps[0]->sound),chans,MUS_COMPATIBLE_FORMAT,dac_size(ss));
  for (i=0;i<samps;i+=256)
    {
      for (k=0;k<chans;k++)
	{
	  for (j=k;j<256*chans;j+=chans)
	    buf[j] = MUS_SAMPLE_TO_SHORT(next_track_sample(fds[k]));
	}
      mus_audio_write(playfd,(char *)buf,256*2*chans);
      check_for_event(ss);
      if (ss->stopped_explicitly)
	{
	  ss->stopped_explicitly = 0;
	  report_in_minibuffer(cps[0]->sound,"stopped");
	  break;
	}
    }
  mus_audio_close(playfd);
  for (i=0;i<chans;i++) free_track_fd(fds[i]);
  FREE(fds);
  FREE(buf);
  if (need_free) FREE(cps);
}

void reflect_mix_edit(chan_info *input_cp, char *origin)
{
  /* input_cp is the underlying (mix input) channel */
  mixdata *md;
  console_state *cs;
  md = (mixdata *)(input_cp->mix_md);
  cs = md->current_cs;
  cs->mix_edit_ctr[input_cp->chan] = input_cp->edit_ctr;
  cs->len = input_cp->samples[input_cp->edit_ctr];
  remix_file(md,origin);
}

void play_mix(snd_state *ss, mixdata *md)
{
  chan_info *cp;
  mix_fd *mf;
  mixmark *m;
  console_state *cs;
  short *buf;
  int play_fd,i,j,samps;
  m = md->mixer; /* can be null */
  if (m) m->playing = 1;
  cp = md->cp;
  cs = md->current_cs;
  samps = cs->len;
  play_fd = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss),SND_SRATE(cp->sound),1,MUS_COMPATIBLE_FORMAT,dac_size(ss));
  if (play_fd != -1)
    {
      mf = init_mix_read(md,FALSE);
      if (mf)
	{
	  buf = (short *)CALLOC(256,sizeof(short));
	  if (buf)
	    {
	      for (i=0;i<samps;i+=256)
		{
		  for (j=0;j<256;j++) buf[j] = MUS_SAMPLE_TO_SHORT(next_mix_sample(mf));
		  mus_audio_write(play_fd,(char *)buf,512);
		  check_for_event(ss);
		  if ((ss->stopped_explicitly) || ((m) && (m->playing == 0)))
		    {
		      ss->stopped_explicitly = 0;
		      report_in_minibuffer(cp->sound,"stopped");
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
  if (m) reflect_mix_stop_playing(ss,m);
}


mixdata *md_from_int(int n) 
{
  if ((n>=0) && (n<mixdatas_size)) 
    return(mixdatas[n]); 
  else return(NULL);
}

static console_state *cs_from_int(int n)
{
  mixdata *md;
  md = md_from_int(n);
  if (md) return(md->current_cs);
  return(NULL);
}

int mix_length(int n) 
{
  console_state *cs; 
  cs = cs_from_int(n); 
  if (cs) 
    return(cs->len); 
  return(-1);
}

env *mix_amp_env(int n, int chan) 
{
  console_state *cs; 
  cs = cs_from_int(n); 
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

env *set_mix_amp_env(int n, int chan, env *val)
{
  mixdata *md;
  console_state *cs;
  md = md_from_int(n);
  if (md)
    {
      if (chan == NO_SELECTION) chan = md->selected_chan;
      if (md->in_chans > chan)
	{
	  cs = md->current_cs;
	  if ((cs->amp_envs) && (cs->amp_envs[chan])) free_env(cs->amp_envs[chan]);
	  if (cs->amp_envs == NULL) cs->amp_envs = (env **)CALLOC(cs->chans,sizeof(env *));
	  cs->amp_envs[chan] = copy_env(val);
	  remix_file(md,S_set_mix_amp_env);
	}
    }
  return(val);
}


/* -------------------------------- SCM connection -------------------------------- */
#if HAVE_GUILE
#include "sg.h"

static SCM g_mix_position(SCM n) 
{
  #define H_mix_position "(" S_mix_position " id) -> sample number of start of mix"
  console_state *cs; 
  ERRB1(n,S_mix_position); 
  cs = cs_from_int(g_scm2intdef(n,0));
  if (cs) RTNINT(cs->orig); 
  return(NO_SUCH_MIX);
}

static SCM g_mix_chans(SCM n) 
{
  #define H_mix_chans "(" S_mix_chans " id) -> (input) channels in mix"
  console_state *cs; 
  ERRB1(n,S_mix_chans); 
  cs = cs_from_int(g_scm2intdef(n,0));
  if (cs) RTNINT(cs->chans);
  return(NO_SUCH_MIX);
}

static SCM g_mixQ(SCM n) 
{
  #define H_mixQ "(" S_mixQ " id) -> #t if mix is active and accessible"
  ERRB1(n,S_mixQ); 
  RTNBOOL(mix_ok(g_scm2intdef(n,0)));
}

static SCM g_mix_length(SCM n) 
{
  #define H_mix_length "(" S_mix_length " id) -> length (frames) of mix"
  int len;
  ERRB1(n,S_mix_length); 
  len = mix_length(g_scm2intdef(n,0));
  if (len == -1) return(NO_SUCH_MIX);
  RTNINT(len);
}

static SCM g_mix_locked(SCM n) 
{
  #define H_mix_locked "(" S_mix_locked " id) -> #t if mix cannot be moved (due to subsequent edits overlapping it)"
  mixdata *md;
  ERRB1(n,S_mix_locked); 
  md = md_from_int(g_scm2intdef(n,0));
  if (md) RTNBOOL((md->current_cs)->locked);
  return(NO_SUCH_MIX);
}

static SCM g_mix_anchor(SCM n) 
{
  #define H_mix_anchor "(" S_mix_anchor " id) -> location of mix 'anchor' (determines console position within mix)"
  mixdata *md; 
  ERRB1(n,S_mix_anchor); 
  md = md_from_int(g_scm2intdef(n,0));
  if (md) RTNINT(md->anchor);
  return(NO_SUCH_MIX);
}

static SCM g_mix_name(SCM n) 
{
  #define H_mix_name "(" S_mix_name " id) -> name associated with mix"
  mixdata *md; 
  ERRB1(n,S_mix_name); 
  md = md_from_int(g_scm2intdef(n,0));
  if (md) RTNSTR(md->name);
  return(NO_SUCH_MIX);
}

static SCM g_mix_track(SCM n) 
{
  #define H_mix_track "(" S_mix_track " id) -> track that mix is a member of"
  mixdata *md; 
  ERRB1(n,S_mix_track); 
  md = md_from_int(g_scm2intdef(n,0));
  if (md) RTNINT(md->track);
  return(NO_SUCH_MIX);
}

static SCM g_mix_console_state(SCM n) 
{
  #define H_mix_console_state "(" S_mix_console_state " id) -> display state of mix's console (0=open, 1=title, 2=named)"
  mixdata *md; 
  ERRB1(n,S_mix_console_state); 
  md = md_from_int(g_scm2intdef(n,0));
  if (md) RTNINT(md->state);
  return(NO_SUCH_MIX);
}

static SCM g_mix_console_y(SCM n) 
{
  #define H_mix_console_y "(" S_mix_console_y " id) -> height of mix's console"
  mixdata *md; 
  ERRB1(n,S_mix_console_y); 
  md = md_from_int(g_scm2intdef(n,0));
  if (md) RTNINT(md->y);
  return(NO_SUCH_MIX);
}

static SCM g_mix_speed(SCM n) 
{
  #define H_mix_speed "(" S_mix_speed " id) -> srate (speed slider setting) of mix"
  console_state *cs; 
  ERRB1(n,S_mix_speed); 
  cs = cs_from_int(g_scm2intdef(n,0));
  if (cs) RTNFLT(cs->speed);
  return(NO_SUCH_MIX);
}

static SCM g_mixes(void) 
{
  #define H_mixes "(" S_mixes ") -> number of mixes created so far (for looping through mix id's)"
  RTNINT(mixes());
}

static SCM g_mix_sound_index(SCM n) 
{
  #define H_mix_sound_index "(" S_mix_sound_index " id) -> index of sound affected by mix"
  mixdata *md; 
  ERRB1(n,S_mix_sound_index); 
  md = md_from_int(g_scm2intdef(n,0));
  if (md) RTNINT(((md->cp)->sound)->index);
  return(NO_SUCH_MIX);
}

static SCM g_mix_sound_channel(SCM n) 
{
  #define H_mix_sound_channel "(" S_mix_sound_channel " id) -> channel affected by mix"
  mixdata *md; 
  ERRB1(n,S_mix_sound_channel);
  md = md_from_int(g_scm2intdef(n,0));
  if (md) RTNINT((md->cp)->chan);
  return(NO_SUCH_MIX);
}

/* these last two refer to the mix output location, not the underlying mix (input) data */

static SCM g_mix_amp(SCM n, SCM uchan) 
{
  #define H_mix_amp "(" S_mix_amp " id &optional (chan 0)) -> amp (console slider setting) of mix's channel chan"
  console_state *cs; 
  int chan;
  ERRB1(n,S_mix_amp);
  ERRB2(uchan,S_mix_amp);
  cs = cs_from_int(g_scm2intdef(n,0));
  if (cs) 
    {
      chan = g_scm2intdef(uchan,0);
      if (chan < cs->chans) RTNFLT(cs->scalers[chan]);
      return(NO_SUCH_CHANNEL);
    }
  return(NO_SUCH_MIX);
}

static SCM g_mix_amp_env(SCM n, SCM chan) 
{
  #define H_mix_amp_env "(" S_mix_amp_env " id &optional (chan 0)) -> amplitude envelope applied to mix's channel chan"
  env *e;
  ERRB1(n,S_mix_amp_env);
  ERRB2(chan,S_mix_amp_env);
  e = mix_amp_env(g_scm2intdef(n,0),g_scm2intdef(chan,0));
  if (e) return(env2scm(e));
  return(SCM_EOL);
}

static SCM g_set_mix_position(SCM n, SCM uval) 
{
  /* should a locked mix be settable? Currently it is. */
  #define H_set_mix_position "(" S_set_mix_position " id val) sets mix's begin time (sample number) to val (moves the mix)"
  mixdata *md;
  int val;
  console_state *cs = NULL;
  ERRN1(n,S_set_mix_position);
  ERRN2(uval,S_set_mix_position);
  md = md_from_int(g_scm2int(n));
  if (md)
    {
      cs = md->current_cs;
      if (cs)
	{
	  val = g_scm2int(uval);
	  if (val >= 0) cs->beg = val; else cs->beg = 0;
	  if (md->mixer) set_mix_title_beg(md,md->mixer);
	  remix_file(md,S_set_mix_position); 
	}
      return(uval);
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_length(SCM n, SCM uval) 
{
  #define H_set_mix_length "(" S_set_mix_length " id val) sets the mix's length (truncating -- dangerous!)"
  mixdata *md;
  int val;
  console_state *cs = NULL;
  ERRN1(n,S_set_mix_length);
  ERRN2(uval,S_set_mix_length);
  md = md_from_int(g_scm2int(n));
  if (md)
    {
      cs = md->current_cs;
      if (cs)
	{
	  val = g_scm2int(uval);
	  if (val >= 0)
	    {
	      cs->len = val;
	      if (md->mixer) set_mix_title_beg(md,md->mixer);
	      remix_file(md,S_set_mix_length); 
	    }
	}
      return(uval);
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_locked(SCM n, SCM val) 
{
  #define H_set_mix_locked "(" S_set_mix_locked " id &optional (val #t)) sets whether the mix can be changed"
  console_state *cs;
  mixdata *md;
  int on;
  ERRN1(n,S_set_mix_locked);
  ERRB2(val,S_set_mix_locked);
  md = md_from_int(g_scm2int(n));
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
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_anchor(SCM n, SCM uval) 
{
  #define H_set_mix_anchor "(" S_set_mix_anchor " id val) sets the mix console position (sample) within the mix"
  mixdata *md;
  console_state *cs = NULL;
  int val;
  ERRN1(n,S_set_mix_anchor);
  ERRN2(uval,S_set_mix_anchor);
  md = md_from_int(g_scm2int(n));
  if (md)
    {
      cs = md->current_cs;
      if (cs)
	{
	  val = g_scm2int(uval);
	  if (val >= 0)
	    {
	      md->anchor = val;
	      update_graph(md->cp,NULL);
	    }
	}
      return(uval);
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_name(SCM n, SCM val) 
{
  #define H_set_mix_name "(" S_set_mix_name " id name) sets the mix's name"
  char *name;
  mixdata *md;
  ERRN1(n,S_set_mix_name);
  ERRS2(val,S_set_mix_name);
  md = md_from_int(g_scm2int(n));
  if (md)
    {
      if (md->name) FREE(md->name);
      name = gh_scm2newstr(val,NULL);
      md->name = copy_string(name);
      free(name);
      reflect_mix_name(md);
      return(val);
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_track(SCM n, SCM val) 
{
  #define H_set_mix_track "(" S_set_mix_track " id track) sets the track that mix is a member of"
  mixdata *md;
  ERRN1(n,S_set_mix_track);
  ERRN2(val,S_set_mix_track);
  md = md_from_int(g_scm2int(n));
  if (md)
    {
      md->track = g_scm2int(val);
      set_mix_track_button_color(md,md->track);
      return(val);
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_console_state(SCM n, SCM uval) 
{
  #define H_set_mix_console_state "(" S_set_mix_console_state " id state) sets the mix's console display state"
  mixdata *md;
  console_state *cs = NULL;
  int val;
  ERRN1(n,S_set_mix_console_state);
  ERRN2(uval,S_set_mix_console_state);
  md = md_from_int(g_scm2int(n));
  if (md) 
    {
      val = g_scm2int(uval);
      cs = md->current_cs;
      if ((cs) && (val >= 0) && (val < 3))
	{
	  md->state = val;
	  fixup_mixmark(md);
	}
      return(uval);
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_console_y(SCM n, SCM val) 
{
  #define H_set_mix_console_y "(" S_set_mix_console_y " id y) sets the mix console's height"
  mixdata *md;
  ERRN1(n,S_set_mix_console_y);
  ERRN2(val,S_set_mix_console_y);
  md = md_from_int(g_scm2int(n));
  if (md)
    {
      md->y = g_scm2int(val);
      if (md->mixer) move_mix_y(md->mixer,md->y);
      return(val);
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_speed(SCM n, SCM uval) 
{
  #define H_set_mix_speed "(" S_set_mix_speed " id speed) sets the mix's speed (console slider setting)"
  mixdata *md;
  Float val;
  ERRN1(n,S_set_mix_speed);
  ERRN2(uval,S_set_mix_speed);
  md = md_from_int(g_scm2int(n));
  if (md)
    {
      val = gh_scm2double(uval);
      if (val != 0.0)
	{
	  respeed(md,val);
	  remix_file(md,S_set_mix_speed);
	}
      return(uval);
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_amp(SCM n, SCM uchan, SCM uval) 
{
  #define H_set_mix_amp "(" S_set_mix_amp " id chan val) sets mix channel chan's amp (console slider setting)"
  mixdata *md;
  Float val;
  int chan;
  ERRN1(n,S_set_mix_amp);
  ERRN2(uchan,S_set_mix_amp);
  ERRN3(uval,S_set_mix_amp);
  md = md_from_int(g_scm2int(n));
  if (md)
    {
      chan = g_scm2int(uchan);
      val = gh_scm2double(uval);
      if (val >= 0.0)
	{
	  if (md->in_chans > chan)
	    {
	      reamp(md,chan,val);
	      remix_file(md,S_set_mix_amp);
	    }
	  else return(NO_SUCH_CHANNEL);
	  return(uval);
	}
    }
  return(NO_SUCH_MIX);
}

static SCM g_set_mix_amp_env(SCM n, SCM chan, SCM val) 
{
  #define H_set_mix_amp_env "(" S_set_mix_amp_env " id chan env) sets the amplitude envelope applied to mix channel chan"
  ERRN1(n,S_set_mix_amp_env);
  ERRN2(chan,S_set_mix_amp_env);
  set_mix_amp_env(g_scm2int(n),g_scm2int(chan),get_env(val,SCM_BOOL_F,S_set_mix_amp_env));
  return(val);
}

static SCM g_mix_sound(SCM file, SCM start_samp, SCM scaler)
{
  #define H_mix_sound "(" S_mix_sound " file start_samp &optional (scaler 1.0)) mixes file (all channels)\n\
   into the currently selected sound at start_samp, scaled by scaler."

  char *urn,*filename;
  Float scl = 1.0;
  int beg,err=0;
  ERRS1(file,S_mix_sound);
  ERRN1(start_samp,S_mix_sound);
  urn = gh_scm2newstr(file,NULL);
  filename = mus_file_full_name(urn);
  free(urn);
  beg = g_scm2int(start_samp);
  if (gh_number_p(scaler)) scl = gh_scm2double(scaler);
  err = mix_sound(get_global_state(),any_selected_sound(get_global_state()),filename,beg,scl);
  if (filename) free(filename);
  if (err == -1) return(NO_SUCH_FILE);
  RTNINT(err);
}

static int update_mix_waveforms(chan_info *cp, void *ptr)
{
  if ((cp) && (cp->mixes)) update_graph(cp,NULL);
  return(0);
}

static SCM g_mix_waveform_height(void) {snd_state *ss; ss = get_global_state(); RTNINT(mix_waveform_height(ss));}
static SCM g_set_mix_waveform_height(SCM val) 
{
  #define H_mix_waveform_height "(" S_mix_waveform_height ") -> max height (pixels) of mix waveforms (20)"
  #define H_set_mix_waveform_height "(" S_set_mix_waveform_height " val) sets " S_mix_waveform_height
  snd_state *ss; 
  ERRN1(val,S_set_mix_waveform_height); 
  ss = get_global_state(); 
  in_set_mix_waveform_height(ss,g_scm2int(val));
  map_over_chans(ss,update_mix_waveforms,NULL);
  RTNINT(mix_waveform_height(ss));
}

static SCM g_select_mix(SCM id)
{
  #define H_select_mix "(" S_select_mix " id) makes mix is the selected mix"
  ERRN1(id,S_select_mix);
  select_mix(get_global_state(),md_from_int(g_scm2int(id)));
  return(id);
}

static SCM g_selected_mix(void)
{
  #define H_selected_mix "(" S_selected_mix ") -> the id of the currently selected mix"
  snd_state *ss;
  ss = get_global_state();
  return(gh_int2scm(ss->selected_mix));
}

static SCM g_forward_mix(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_mix "(" S_forward_mix " &optional (count 1) snd chn) moves the cursor forward count mix consoles"
  int val;
  chan_info *cp;
  ERRB1(count,S_forward_mix); 
  ERRCP(S_forward_mix,snd,chn,2);
  cp = get_cp(snd,chn);
  val = g_scm2intdef(count,1); 
  if (cp) handle_cursor(cp,goto_mix(cp,val)); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

static SCM g_backward_mix(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_mix "(" S_backward_mix " &optional (count 1) snd chn) moves the cursor back count mix consoles"
  int val; 
  chan_info *cp;
  ERRB1(count,S_backward_mix); 
  ERRCP(S_backward_mix,snd,chn,2);
  cp = get_cp(snd,chn);
  val = -(g_scm2intdef(count,1)); 
  if (cp) handle_cursor(cp,goto_mix(cp,val)); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}


static SCM g_mix(SCM file, SCM chn_samp_n, SCM file_chn, SCM snd_n, SCM chn_n, SCM console)
{
  #define H_mix "(" S_mix " file &optional (chn-start 0) (file-chan 0) snd chn with-console)) mixes file\n\
   channel file-chan into snd's channel chn starting at chn-start (or at the cursor location if chan-start\n\
   is omitted), returning the new mix's id.  if with-console is #f, the data is mixed (no console is created).\n\
   If chn is omitted, file's channels are mixed until snd runs out of channels"

  chan_info *cp = NULL;
  char *urn,*name = NULL;
  int chans,id=-1;
  int with_mixer = 1;
  snd_state *ss;
  mixdata *md;
  ERRS1(file,S_mix);
  ERRB2(chn_samp_n,S_mix);
  ERRB3(file_chn,S_mix);
  ERRCP(S_mix,snd_n,chn_n,4);
  SCM_ASSERT((gh_number_p(console)) || (gh_boolean_p(console)) || (SCM_UNBNDP(console)),console,SCM_ARG6,S_mix);
  urn = gh_scm2newstr(file,NULL);
  name = mus_file_full_name(urn);
  free(urn);
  ss = get_global_state();
  if (SCM_UNBNDP(console))
    with_mixer = with_mix_consoles(ss);
  else with_mixer = bool_int_or_one(console);
  if (SCM_UNBNDP(chn_samp_n))
    {
      id = mix_complete_file(any_selected_sound(ss),name,S_mix,with_mixer);
    }
  else
    {
      cp = get_cp(snd_n,chn_n);
      if (cp)
	{
	  chans = mus_sound_chans(name);
	  if (chans > 0)
	    {
	      md = file_mix_samples(g_scm2intdef(chn_samp_n,0),
				    mus_sound_samples(name)/chans,name,
				    cp,g_scm2intdef(file_chn,0),
				    DONT_DELETE_ME,1,S_mix,
				    with_mixer);
	      if (md) id = md->id;
	    }
	  else return(NO_SUCH_FILE);
	}
    }
  if (name) FREE(name);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  RTNINT(id);
}



/* ---------------- mix sample readers ---------------- */

static int mf_tag = 0;
static SCM mark_mf(SCM obj) {SCM_SETGC8MARK(obj); return(SCM_BOOL_F);}
static int mf_p(SCM obj) {return((SCM_NIMP(obj)) && (GH_TYPE_OF(obj) == (SCM)mf_tag));}

static SCM g_mf_p(SCM obj) 
{
  #define H_mf_p "(" S_mix_sample_readerQ " obj) -> #t if obj is a mix-sample-reader"
  RTNBOOL(mf_p(obj));
}

static mix_fd *get_mf(SCM obj) {if (mf_p(obj)) return((mix_fd *)GH_VALUE_OF(obj)); else return(NULL);}
static SCM equalp_mf(SCM obj1, SCM obj2) {RTNBOOL(get_mf(obj1) == get_mf(obj2));}

static int print_mf(SCM obj, SCM port, scm_print_state *pstate) 
{
  mix_fd *fd;
  mixdata *md;
  char *desc;
  fd = get_mf(obj);
  if (fd == NULL)
    scm_puts("<null>",port);
  else
    {
      md = fd->md;
      desc = (char *)CALLOC(128,sizeof(char));
      sprintf(desc,"<mix-sample-reader %p: %s via mix %d>",
	      fd,
	      md->in_filename,
	      md->id);
      scm_puts(desc,port); 
      FREE(desc);
    }
  return(1);
}

static scm_sizet free_mf(SCM obj) 
{
  mix_fd *fd = (mix_fd *)GH_VALUE_OF(obj); 
  if (fd) 
    {
#ifdef DEBUGGING
      snd_warning("Guile's GC is freeing a mix sample reader!");
#endif
      free_mix_fd(fd); 
    }
  return(0);
}

#if HAVE_GUILE_1_3_0
static scm_smobfuns mf_smobfuns = {
  &mark_mf,
  &free_mf,
  &print_mf,
  &equalp_mf};
#endif

static SCM g_make_mix_sample_reader(SCM mix_id)
{
  #define H_make_mix_sample_reader "(" S_make_mix_sample_reader " id) returns a reader ready to access mix 'id'"
  mixdata *md = NULL;
  mix_fd *mf = NULL;
#if HAVE_GUILE_1_3_0
  SCM new_mf;
#endif
  ERRN1(mix_id,S_make_mix_sample_reader);
  md = md_from_int(g_scm2int(mix_id));
  if (md) mf = init_mix_read(md,FALSE); else return(NO_SUCH_MIX);
  if (mf)
    {
#if (!HAVE_GUILE_1_3_0)
      SCM_RETURN_NEWSMOB(mf_tag,(SCM)mf);
#else
      SCM_NEWCELL(new_mf);
      SCM_SETCDR(new_mf,(SCM)mf);
      SCM_SETCAR(new_mf,mf_tag);
      return(new_mf);
#endif
    }
  return(SCM_BOOL_F);
}

static SCM g_next_mix_sample(SCM obj)
{
  #define H_next_mix_sample "(" S_next_mix_sample " reader) -> next sample from mix reader"
  SCM_ASSERT(mf_p(obj),obj,SCM_ARG1,S_next_mix_sample);
  return(gh_double2scm(MUS_SAMPLE_TO_FLOAT(next_mix_sample(get_mf(obj)))));
}

static SCM g_free_mix_sample_reader(SCM obj)
{
  #define H_free_mix_sample_reader "(" S_free_mix_sample_reader " reader) frees mix sample reader 'reader'"
  SCM_ASSERT(mf_p(obj),obj,SCM_ARG1,S_free_mix_sample_reader);
  free_mix_fd(get_mf(obj));
  GH_SET_VALUE_OF(obj,(SCM)NULL);
  return(SCM_BOOL_F);
}



/* ---------------- track sample readers ---------------- */

static int tf_tag = 0;
static SCM mark_tf(SCM obj) {SCM_SETGC8MARK(obj); return(SCM_BOOL_F);}
static int tf_p(SCM obj) {return((SCM_NIMP(obj)) && (GH_TYPE_OF(obj) == (SCM)tf_tag));}

static SCM g_tf_p(SCM obj) 
{
  #define H_tf_p "(" S_track_sample_readerQ " obj) -> #t if obj is a track-sample-reader"
  RTNBOOL(tf_p(obj));
}

static track_fd *get_tf(SCM obj) {if (tf_p(obj)) return((track_fd *)GH_VALUE_OF(obj)); else return(NULL);}
static SCM equalp_tf(SCM obj1, SCM obj2) {RTNBOOL(get_tf(obj1) == get_tf(obj2));}

static int print_tf(SCM obj, SCM port, scm_print_state *pstate) 
{
  track_fd *fd;
  mixdata *md;
  mix_fd *mf;
  char *desc;
  int i,len;
  fd = get_tf(obj);
  if (fd == NULL)
    scm_puts("<null>",port);
  else
    {
      desc = (char *)CALLOC(128,sizeof(char));
      mf = fd->fds[0];
      md = mf->md;
      sprintf(desc,"<track-sample-reader %p: %s chan %d via mixes '(",
	      fd,
	      md->in_filename,
	      (md->cp)->chan);
      scm_puts(desc,port); 
      len = fd->mixes;
      if (len > 0)
	{
	  for (i=0;i<len-1;i++)
	    {
	      mf = fd->fds[i];
	      sprintf(desc,"%d ",(mf->md)->id);
	      scm_puts(desc,port); 
	    }
	  mf = fd->fds[len-1];
	  sprintf(desc,"%d)>",(mf->md)->id);
	}
      else sprintf(desc,")>");
      scm_puts(desc,port); 
      FREE(desc);
    }
  return(1);
}

static scm_sizet free_tf(SCM obj) 
{
  track_fd *fd = (track_fd *)GH_VALUE_OF(obj); 
  if (fd) 
    {
#ifdef DEBUGGING
      snd_warning("Guile's GC is freeing a track sample reader!");
#endif
      free_track_fd(fd); 
    }
  return(0);
}

#if HAVE_GUILE_1_3_0
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
#if HAVE_GUILE_1_3_0
  SCM new_tf;
#endif
  ERRN1(track_id,S_make_track_sample_reader);
  ERRCP(S_make_track_sample_reader,snd,chn,3); 
  cp = get_cp(snd,chn);
  tf = init_track_reader(cp,g_scm2int(track_id),g_scm2intdef(samp,0));
  if (tf)
    {
#if (!HAVE_GUILE_1_3_0)
      SCM_RETURN_NEWSMOB(tf_tag,(SCM)tf);
#else
      SCM_NEWCELL(new_tf);
      SCM_SETCDR(new_tf,(SCM)tf);
      SCM_SETCAR(new_tf,tf_tag);
      return(new_tf);
#endif
    }
  return(SCM_BOOL_F);
}

static SCM g_next_track_sample(SCM obj)
{
  #define H_next_track_sample "(" S_next_track_sample " reader) -> next sample from track reader"
  SCM_ASSERT(tf_p(obj),obj,SCM_ARG1,S_next_track_sample);
  return(gh_double2scm(MUS_SAMPLE_TO_FLOAT(next_track_sample(get_tf(obj)))));
}

static SCM g_free_track_sample_reader(SCM obj)
{
  #define H_free_track_sample_reader "(" S_free_track_sample_reader " reader) frees the track sample reader 'reader'"
  SCM_ASSERT(tf_p(obj),obj,SCM_ARG1,S_free_track_sample_reader);
  free_track_fd(get_tf(obj));
  GH_SET_VALUE_OF(obj,(SCM)NULL);
  return(SCM_BOOL_F);
}

static SCM g_play_track(SCM num, SCM snd, SCM chn)
{
  #define H_play_track "(" S_play_track " track &optional snd chn) plays track"
  /* just a dummy for testing */
  chan_info *cp;
  /* in this case if snd=#t, play all associated mixes in all chans */
  if (SCM_TRUE_P(snd))
    play_track(get_global_state(),NULL,0,g_scm2int(num));
  else 
    {
      cp = get_cp(snd,chn);
      if (cp) play_track(cp->state,&cp,1,g_scm2int(num)); else return(NO_SUCH_CHANNEL);
    }
  return(num);
}

static SCM g_play_mix(SCM num)
{
  #define H_play_mix "(" S_play_mix " id) plays mix"
  mixdata *md;
  md = md_from_int(g_scm2intdef(num,0));
  if (md) play_mix(md->ss,md); else return(NO_SUCH_MIX);
  return(num);
}

static SCM multichannel_mix_hook;

#if (!HAVE_GUILE_1_3_0)
static void call_multichannel_mix_hook(int *ids, int n)
{
  SCM lst = SCM_EOL;
  int i;
  /* create list from ids, pass to hook, if any */
  if (HOOKED(multichannel_mix_hook))
    {
      for (i=n-1;i>=0;i--)
	lst = scm_cons(SCM_MAKINUM(ids[i]),lst);
      g_c_run_progn_hook(multichannel_mix_hook,SCM_LIST1(lst));
    }
}
#else
static void call_multichannel_mix_hook(int *ids, int n) {}
#endif


void g_init_mix(SCM local_doc)
{
#if (!HAVE_GUILE_1_3_0)
  /* mf_tag = scm_make_smob_type_mfpe("mf",sizeof(SCM),mark_mf,free_mf,print_mf,equalp_mf); */
  mf_tag = scm_make_smob_type("mf",sizeof(SCM));
  scm_set_smob_mark(mf_tag,mark_mf);
  scm_set_smob_print(mf_tag,print_mf);
  scm_set_smob_free(mf_tag,free_mf);
  scm_set_smob_equalp(mf_tag,equalp_mf);
#else
  mf_tag = scm_newsmob(&mf_smobfuns);
#endif
  DEFINE_PROC(gh_new_procedure1_0(S_make_mix_sample_reader,g_make_mix_sample_reader),H_make_mix_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_next_mix_sample,g_next_mix_sample),H_next_mix_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_free_mix_sample_reader,g_free_mix_sample_reader),H_free_mix_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_mix_sample_readerQ,g_mf_p),H_mf_p);

#if (!HAVE_GUILE_1_3_0)
  /* tf_tag = scm_make_smob_type_mfpe("tf",sizeof(SCM),mark_tf,free_tf,print_tf,equalp_tf); */
  tf_tag = scm_make_smob_type("tf",sizeof(SCM));
  scm_set_smob_mark(tf_tag,mark_tf);
  scm_set_smob_print(tf_tag,print_tf);
  scm_set_smob_free(tf_tag,free_tf);
  scm_set_smob_equalp(tf_tag,equalp_tf);
#else
  tf_tag = scm_newsmob(&tf_smobfuns);
#endif
  DEFINE_PROC(gh_new_procedure(S_make_track_sample_reader,SCM_FNC g_make_track_sample_reader,1,3,0),H_make_track_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_next_track_sample,g_next_track_sample),H_next_track_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_free_track_sample_reader,g_free_track_sample_reader),H_free_track_sample_reader);
  DEFINE_PROC(gh_new_procedure1_0(S_track_sample_readerQ,g_tf_p),H_tf_p);
  DEFINE_PROC(gh_new_procedure0_1(S_play_mix,g_play_mix),H_play_mix);
  DEFINE_PROC(gh_new_procedure1_2(S_play_track,g_play_track),H_play_track);

  DEFINE_PROC(gh_new_procedure0_1(S_mix_position,g_mix_position),H_mix_position);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_length,g_mix_length),H_mix_length);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_locked,g_mix_locked),H_mix_locked);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_anchor,g_mix_anchor),H_mix_anchor);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_track,g_mix_track),H_mix_track);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_console_state,g_mix_console_state),H_mix_console_state);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_console_y,g_mix_console_y),H_mix_console_y);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_speed,g_mix_speed),H_mix_speed);
  DEFINE_PROC(gh_new_procedure0_2(S_mix_amp,g_mix_amp),H_mix_amp);
  DEFINE_PROC(gh_new_procedure0_2(S_mix_amp_env,g_mix_amp_env),H_mix_amp_env);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_name,g_mix_name),H_mix_name);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_chans,g_mix_chans),H_mix_chans);
  DEFINE_PROC(gh_new_procedure0_1(S_mixQ,g_mixQ),H_mixQ);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_sound_channel,g_mix_sound_channel),H_mix_sound_channel);
  DEFINE_PROC(gh_new_procedure0_1(S_mix_sound_index,g_mix_sound_index),H_mix_sound_index);
  DEFINE_PROC(gh_new_procedure0_0(S_mixes,g_mixes),H_mixes);
  DEFINE_PROC(gh_new_procedure2_0(S_set_mix_position,g_set_mix_position),H_set_mix_position);
  DEFINE_PROC(gh_new_procedure2_0(S_set_mix_length,g_set_mix_length),H_set_mix_length);
  DEFINE_PROC(gh_new_procedure1_1(S_set_mix_locked,g_set_mix_locked),H_set_mix_locked);
  DEFINE_PROC(gh_new_procedure2_0(S_set_mix_anchor,g_set_mix_anchor),H_set_mix_anchor);
  DEFINE_PROC(gh_new_procedure2_0(S_set_mix_track,g_set_mix_track),H_set_mix_track);
  DEFINE_PROC(gh_new_procedure2_0(S_set_mix_console_state,g_set_mix_console_state),H_set_mix_console_state);
  DEFINE_PROC(gh_new_procedure2_0(S_set_mix_console_y,g_set_mix_console_y),H_set_mix_console_y);
  DEFINE_PROC(gh_new_procedure2_0(S_set_mix_speed,g_set_mix_speed),H_set_mix_speed);
  DEFINE_PROC(gh_new_procedure3_0(S_set_mix_amp,g_set_mix_amp),H_set_mix_amp);
  DEFINE_PROC(gh_new_procedure3_0(S_set_mix_amp_env,g_set_mix_amp_env),H_set_mix_amp_env);
  DEFINE_PROC(gh_new_procedure2_0(S_set_mix_name,g_set_mix_name),H_set_mix_name);
  DEFINE_PROC(gh_new_procedure2_1(S_mix_sound,g_mix_sound),H_mix_sound);
  DEFINE_PROC(gh_new_procedure1_0(S_set_mix_waveform_height,g_set_mix_waveform_height),H_set_mix_waveform_height);
  DEFINE_PROC(gh_new_procedure0_0(S_mix_waveform_height,g_mix_waveform_height),H_mix_waveform_height);
  DEFINE_PROC(gh_new_procedure1_0(S_select_mix,g_select_mix),H_select_mix);
  DEFINE_PROC(gh_new_procedure0_0(S_selected_mix,g_selected_mix),H_selected_mix);
  DEFINE_PROC(gh_new_procedure(S_forward_mix,SCM_FNC g_forward_mix,0,3,0),H_forward_mix);
  DEFINE_PROC(gh_new_procedure(S_backward_mix,SCM_FNC g_backward_mix,0,3,0),H_backward_mix);
  DEFINE_PROC(gh_new_procedure(S_mix,SCM_FNC g_mix,1,5,0),H_mix);

#if (!HAVE_GUILE_1_3_0)
  multichannel_mix_hook = scm_create_hook(S_multichannel_mix_hook,1);
#else
  multichannel_mix_hook = gh_define(S_multichannel_mix_hook,SCM_BOOL_F);
#endif
}

#endif

