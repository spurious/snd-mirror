#include "snd.h"
#include "sndlib-strings.h"

snd_info *snd_new_file(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *new_comment, off_t samples)
{
  int chan, err;
  off_t size;
  unsigned char* buf;
  if (snd_overwrite_ok(ss, newname))
    {
      if (mus_header_writable(header_type, data_format))
	{
	  err = snd_write_header(ss, newname, header_type, srate, chans, 0, 
				 chans /* one sample in each chan */, 
				 data_format, new_comment, 
				 snd_strlen(new_comment), NULL);
	  if (err == -1)
	    snd_error(_("can't write %s"),newname);
	  else
	    {
	      chan = snd_reopen_write(ss, newname);
	      lseek(chan, mus_header_data_location(), SEEK_SET);
	      size = chans * mus_samples_to_bytes(data_format, samples);
	      buf = (unsigned char *)CALLOC(size, sizeof(unsigned char));
	      write(chan, buf, size);
	      snd_close(chan, newname);
	      FREE(buf);
	      return(snd_open_file(newname, ss, false));
	    }
	}
      else 
	snd_error(_("can't write %s file with %s data format"),
		  mus_header_type_name(header_type),
		  mus_data_format_name(data_format));
    }
  return(NULL);
}


/* ---------------- amp envs ---------------- */

typedef struct {
  int slice, edpos;
  off_t samples, m;  
  env_info *ep; 
  snd_fd *sf;
} env_state;

env_info *free_env_info(env_info *ep)
{
  if (ep)
    {
      if (ep->data_max) {FREE(ep->data_max); ep->data_max = NULL;}
      if (ep->data_min) {FREE(ep->data_min); ep->data_min = NULL;}
      FREE(ep);
    }
  return(NULL);
}

env_info *free_amp_env(chan_info *cp, int pos)
{
  /* can be either during channel close, or premature work proc removal */
  if ((cp) && 
      (cp->amp_envs) && 
      (pos < cp->edit_size))
    return(free_env_info(cp->amp_envs[pos]));
  return(NULL);
}

/* during processing, cgx->amp_env_state -> env_state for that channel
 *  cgx->amp_env_in_progress is the associated X work proc
 */

void free_env_state(chan_info *cp)
{
  /* env info is tied into cp->amp_envs immediately upon env start, released via normal cp cleanups */
  /* this function just cleans up the current work proc stuff (amp_env in this case can be incomplete) */
  env_state *es;
  chan_context *cgx;
  if (cp)
    {
      cgx = cp->cgx;
      if (cgx)
	{
	  es = (env_state *)(cgx->amp_env_state);
	  if (es)
	    {
	      if (es->sf) 
		es->sf = free_snd_fd(es->sf);
	      FREE(es);
	    }
	  cgx->amp_env_state = NULL;
	  cgx->amp_env_in_progress = 0;
	}
    }
}

static env_state *make_env_state(chan_info *cp, off_t samples)
{
  int val, pos, i, j, start_bin, end_bin, old_end_bin;
  bool happy = false;
  off_t start, end, old_samples;
  env_info *ep, *old_ep = NULL;
  env_state *es;
  if (samples <= 0) return(NULL);
  stop_amp_env(cp);
  pos = cp->edit_ctr;
  es = (env_state *)CALLOC(1, sizeof(env_state));
  es->samples = samples;
  es->slice = 0;
  es->edpos = pos;
  es->m = 0;
  if (cp->amp_envs[pos])
    {
      es->ep = cp->amp_envs[pos];
      ep = es->ep;
    }
  else 
    {
      es->ep = (env_info *)CALLOC(1, sizeof(env_info));
      ep = es->ep;
      if (pos > 0)
	{
	  old_ep = cp->amp_envs[pos - 1];
	  if ((old_ep) && 
	      (old_ep->completed))
	    {
	      /* here in many cases, the preceding edit's amp env has most of the data we need.
	       * cp->edits[cp->edit_ctr] describes the current edit, with beg and end, so in the
	       * simplest case, we can just copy to the bin representing beg, and from the bin
	       * representing end (setting ep->top_bin and ep->bin); if the file's length has
	       * changed dramatically, we need to do it all.  fmin/fmax need to be set as we copy.
	       * as-one-edit can mess this up...
	       */
	      old_samples = cp->samples[pos - 1];
	      if (abs(samples - old_samples) < (samples / 2))
		{
		  start = edit_changes_begin_at(cp);
		  /* as-one-edit backs up edit records without resetting any beg/len values.
		   *   but this is tricky stuff -- not worth pushing...
		   */
		  end = edit_changes_end_at(cp);
		  if (abs(end - start) < (samples / 2))
		    {
		      /* here we'll try to take advantage of an existing envelope */
		      old_ep = cp->amp_envs[pos - 1];
		      ep->samps_per_bin = old_ep->samps_per_bin;
		      ep->amp_env_size = (int)(ceil((double)(es->samples) / (double)(ep->samps_per_bin)));
		      ep->data_max = (mus_sample_t *)CALLOC(ep->amp_env_size, sizeof(mus_sample_t));
		      ep->data_min = (mus_sample_t *)CALLOC(ep->amp_env_size, sizeof(mus_sample_t));
		      start_bin = (int)(start / ep->samps_per_bin);
		      ep->fmin = MUS_SAMPLE_MAX;
		      ep->fmax = MUS_SAMPLE_MIN;
		      for (i = 0; i < start_bin; i++) 
			{
			  ep->data_min[i] = old_ep->data_min[i];
			  ep->data_max[i] = old_ep->data_max[i];
			  if (ep->data_min[i] < ep->fmin) ep->fmin = ep->data_min[i];
			  if (ep->data_max[i] > ep->fmax) ep->fmax = ep->data_max[i];
			}
		      ep->bin = start_bin;
		      if (end != 0)
			{
			  old_end_bin = (int)(end / old_ep->samps_per_bin);
			  end += (samples - old_samples);
			  end_bin = (int)(end / ep->samps_per_bin);
			  for (i = end_bin, j = old_end_bin; (i < ep->amp_env_size) && (j < old_ep->amp_env_size); i++, j++)
			    {
			      ep->data_min[i] = old_ep->data_min[j];
			      ep->data_max[i] = old_ep->data_max[j];
			      if (ep->data_min[i] < ep->fmin) ep->fmin = ep->data_min[i];
			      if (ep->data_max[i] > ep->fmax) ep->fmax = ep->data_max[i];
			    }
			  ep->top_bin = end_bin;
			}
		      else ep->top_bin = 0;
		      happy = true;
		    }
		}
	    }
	}
      if (!happy)
	{
	  /* we want samps_per_bin to be useful over a wide range of file sizes */
	  /* 160e6 = about a hour at 44KHz */
	  val = (int)(log((double)(es->samples)));
	  if (val > 20) val = 20;
	  ep->amp_env_size = snd_ipow2(val);
	  ep->samps_per_bin = (int)(ceil((double)(es->samples) / (double)(ep->amp_env_size)));
	  ep->data_max = (mus_sample_t *)CALLOC(ep->amp_env_size, sizeof(mus_sample_t));
	  ep->data_min = (mus_sample_t *)CALLOC(ep->amp_env_size, sizeof(mus_sample_t));
	  ep->bin = 0;
	  ep->top_bin = 0;
	  ep->fmin = MUS_SAMPLE_MAX;
	  ep->fmax = MUS_SAMPLE_MIN;
	  /* preset as much as possible of the envelope */
	}
      cp->amp_envs[pos] = ep;
      ep->completed = false;
    }
  es->sf = NULL;
  return(es);
}

void start_env_state(chan_info *cp)
{
  cp->cgx->amp_env_state = make_env_state(cp, CURRENT_SAMPLES(cp));
}

static bool tick_amp_env(chan_info *cp, env_state *es)
{
  env_info *ep;
  int i, n, sb, lm;
  off_t samps_to_read;
  mus_sample_t ymin, ymax, val;
  snd_fd *sfd;
  ep = es->ep;
  if (es->slice == 0)
    {
      if (ep->top_bin != 0)
	lm = (ep->top_bin - ep->bin + 1);
      else lm = (ep->amp_env_size - ep->bin);
      if (lm <= 0) lm = 1;
      samps_to_read = (off_t)(lm * ep->samps_per_bin);
      if (samps_to_read > 1000000)
	{
	  lm = 1000000 / ep->samps_per_bin;
	  samps_to_read = (off_t)(lm * ep->samps_per_bin);
	}
      sb = ep->bin;
      if (sb >= ep->amp_env_size)
	{
	  /* oops... */
	  es->slice++;
	  if (es->sf) es->sf = free_snd_fd(es->sf);
	  ep->completed = true;
	  return(true);
	}
      if (es->sf == NULL) 
	{
	  es->sf = init_sample_read_any(ep->bin * ep->samps_per_bin, cp, READ_FORWARD, es->edpos);
	}
      sfd = es->sf;
      if (sfd == NULL) return(false);
      for (n = 0; n < lm; n++, sb++)
	{
	  val = read_sample(sfd);
	  ymin = val;
	  ymax = val;
	  for (i = 1; i < ep->samps_per_bin; i++)
	    {
	      val = read_sample(sfd);
	      if (ymin > val) 
		ymin = val; 
	      else 
		if (ymax < val) 
		  ymax = val;
	    }
	  ep->data_max[sb] = ymax;
	  ep->data_min[sb] = ymin;
	  if (ymin < ep->fmin) ep->fmin = ymin;
	  if (ymax > ep->fmax) ep->fmax = ymax;
	}

      es->m += samps_to_read;
      ep->bin += lm;
      if ((es->m >= es->samples) || 
	  ((ep->top_bin > 0) && (ep->bin >= ep->top_bin))) /* this applies to partial amp envs */
	{
	  es->slice++;
	  if (es->sf) es->sf = free_snd_fd(es->sf);
	  ep->completed = true;
	  return(true);
	}
      return(false);
    }
  else
    {
      ep->completed = true;
      return(true);
    }
}

Cessate get_amp_env(Indicium ptr)
{
  /* calculate the amp env of channel */
  chan_info *cp = (chan_info *)ptr;
  env_state *es;
  int pos;
  chan_context *cgx;
  if ((!cp) || (!(cp->cgx))) return(BACKGROUND_QUIT);
  cgx = cp->cgx;
  pos = cp->edit_ctr;
  if ((pos == -1) || (!(cp->active)))
    {
      free_env_state(cp);
      return(BACKGROUND_QUIT);
    }
  if (!(cgx->amp_env_state)) 
    cgx->amp_env_state = make_env_state(cp, CURRENT_SAMPLES(cp));
  es = (env_state *)(cgx->amp_env_state);
  if (tick_amp_env(cp, es))
    {
      free_env_state(cp);
      reflect_amp_env_completion(cp->sound);
      if (cp->waiting_to_make_graph) 
	{
	  cp->waiting_to_make_graph = false;
	  update_graph(cp);
	}
      return(BACKGROUND_QUIT);
    }
  else return(BACKGROUND_CONTINUE);
}

bool amp_env_maxamp_ok(chan_info *cp, int edpos)
{
  env_info *ep;
  if ((cp) && (cp->amp_envs))
    {
      ep = cp->amp_envs[edpos];
      return((ep) && (ep->completed));
    }
  return(false);
}

Float amp_env_maxamp(chan_info *cp, int edpos)
{
  env_info *ep;
  mus_sample_t ymax;
  ep = cp->amp_envs[edpos];
  ymax = -ep->fmin;
  if (ymax < ep->fmax) 
    return(MUS_SAMPLE_TO_FLOAT(ep->fmax));
  return(MUS_SAMPLE_TO_FLOAT(ymax));
}

bool amp_env_usable(chan_info *cp, Float samples_per_pixel, off_t hisamp, bool start_new, int edit_pos) 
{
  env_info *ep;
  int bin;
  chan_context *cgx;
  cgx = cp->cgx;
  if ((!cgx) || 
      (!(cp->amp_envs))) 
    return(false);
  ep = cp->amp_envs[edit_pos];
  if (ep)
    {
      bin = (int)(hisamp / ep->samps_per_bin); 
      if ((ep->completed) || 
	  (bin < ep->bin) || 
	  ((ep->top_bin != 0) && (bin > ep->top_bin)))
	return(samples_per_pixel >= (Float)(ep->samps_per_bin));
    }
  if ((start_new) &&
      (!(cgx->amp_env_in_progress)) && 
      (CURRENT_SAMPLES(cp) > AMP_ENV_CUTOFF) &&
      (cp->sound->short_filename != NULL))             /* region browser jumped in too soon during autotest */
    start_amp_env(cp);
  return(false);
}

static short local_grf_y(Float val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((short)(ap->y_base + val * ap->y_scale));
}

int amp_env_graph(chan_info *cp, axis_info *ap, Float samples_per_pixel, int srate) 
{
  Float step, x, pinc = 0.0;
  double xf, xk;
  mus_sample_t ymin, ymax;
  int j, k, kk;
  Locus xi;
  off_t i;
  env_info *ep;
  ep = cp->amp_envs[cp->edit_ctr];
  step = samples_per_pixel / (Float)(ep->samps_per_bin);
  xf = (double)(ap->losamp) / (double)(ep->samps_per_bin);
  j = 0;
  x = ap->x0;
  xi = grf_x(x, ap);
  i = ap->losamp;
  xk = (double)i;
  if (cp->printing) pinc = (Float)samples_per_pixel / (Float)srate;
  ymin = ep->fmax;
  ymax = ep->fmin;
  while (i <= ap->hisamp)
    {
      k = (int)xf;
      xf += step;
      kk = (int)xf;
      if (kk >= ep->amp_env_size) 
	kk = ep->amp_env_size - 1;
      for (; k <= kk; k++)
	{
	  if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
	  if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
	}
      xk += samples_per_pixel;
      i = (off_t)xk;
      set_grf_points(xi, j,
		     local_grf_y(MUS_SAMPLE_TO_FLOAT(ymin), ap),
		     local_grf_y(MUS_SAMPLE_TO_FLOAT(ymax), ap));
      if (cp->printing) 
	{
	  x += pinc; 
	  ps_set_grf_points(x, j,
			    MUS_SAMPLE_TO_FLOAT(ymin),
			    MUS_SAMPLE_TO_FLOAT(ymax));
	}
      xi++;
      j++;
      ymin = ep->fmax;
      ymax = ep->fmin;
    }
  return(j);
}

void amp_env_scale_by(chan_info *cp, Float scl, int pos)
{
  env_info *old_ep, *new_ep;
  int i;
  old_ep = cp->amp_envs[pos];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if ((new_ep) && 
	  (new_ep->amp_env_size != old_ep->amp_env_size)) 
	new_ep = free_amp_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
	  new_ep->data_max = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	  new_ep->data_min = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	}
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      if (scl >= 0.0)
	{
	  new_ep->fmin = (mus_sample_t)(old_ep->fmin * scl);
	  new_ep->fmax = (mus_sample_t)(old_ep->fmax * scl);
	  for (i = 0; i < new_ep->amp_env_size; i++) 
	    {
	      new_ep->data_min[i] = (mus_sample_t)(old_ep->data_min[i] * scl);
	      new_ep->data_max[i] = (mus_sample_t)(old_ep->data_max[i] * scl);
	    }
	}
      else
	{
	  new_ep->fmax = (mus_sample_t)(old_ep->fmin * scl);
	  new_ep->fmin = (mus_sample_t)(old_ep->fmax * scl);
	  for (i = 0; i < new_ep->amp_env_size; i++) 
	    {
	      new_ep->data_max[i] = (mus_sample_t)(old_ep->data_min[i] * scl);
	      new_ep->data_min[i] = (mus_sample_t)(old_ep->data_max[i] * scl);
	    }
	}
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->amp_envs[cp->edit_ctr] = new_ep;
    }
}

void pick_one_bin(env_info *ep, int bin, off_t cursamp, chan_info *cp, int edpos)
{
  snd_fd *sf;
  int n;
  mus_sample_t val, ymin = MUS_SAMPLE_MAX, ymax = MUS_SAMPLE_MIN;
  /* here we have to read the current bin using the current fragments */
  sf = init_sample_read_any(cursamp, cp, READ_FORWARD, edpos);
  if (sf == NULL) return;
  for (n = 0; n < ep->samps_per_bin; n++)
    {
      val = read_sample(sf); 
      if (ymin > val) ymin = val; 
      if (ymax < val) ymax = val;
    }
  ep->data_max[bin] = ymax;
  ep->data_min[bin] = ymin;
  free_snd_fd(sf);
}

void amp_env_scale_selection_by(chan_info *cp, Float scl, off_t beg, off_t num, int pos)
{
  env_info *old_ep, *new_ep;
  mus_sample_t fmax = MUS_SAMPLE_MIN, fmin = MUS_SAMPLE_MAX;
  off_t cursamp, start, end;
  int i;
  old_ep = cp->amp_envs[pos];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if ((new_ep) && 
	  (new_ep->amp_env_size != old_ep->amp_env_size)) 
	new_ep = free_amp_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
	  new_ep->data_max = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	  new_ep->data_min = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	}
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      end = beg + num - 1;
      start = beg - new_ep->samps_per_bin;
      for (i = 0, cursamp = 0; i < new_ep->amp_env_size; i++, cursamp += new_ep->samps_per_bin) 
	{
	  if ((cursamp >= end) || (cursamp <= start))
	    {
	      new_ep->data_min[i] = old_ep->data_min[i];
	      new_ep->data_max[i] = old_ep->data_max[i];
	    }
	  else
	    {
	      /* if segment is entirely in scaled section, just scale it */
	      if ((cursamp >= beg) && ((cursamp + new_ep->samps_per_bin) <= end))
		{
		  if (scl >= 0.0)
		    {
		      new_ep->data_max[i] = (mus_sample_t)(old_ep->data_max[i] * scl);
		      new_ep->data_min[i] = (mus_sample_t)(old_ep->data_min[i] * scl);
		    }
		  else
		    {
		      new_ep->data_max[i] = (mus_sample_t)(old_ep->data_min[i] * scl);
		      new_ep->data_min[i] = (mus_sample_t)(old_ep->data_max[i] * scl);
		    }
		}
	      else pick_one_bin(new_ep, i, cursamp, cp, cp->edit_ctr);
	    }
	  if (fmin > new_ep->data_min[i]) fmin = new_ep->data_min[i];
	  if (fmax < new_ep->data_max[i]) fmax = new_ep->data_max[i];
	}
      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->amp_envs[cp->edit_ctr] = new_ep;
    }
}

env_info *amp_env_section(chan_info *cp, off_t beg, off_t num, int edpos)
{
  /* used in snd-region.c to create the region amp env */
  env_info *old_ep, *new_ep = NULL;
  mus_sample_t fmax = MUS_SAMPLE_MIN, fmin = MUS_SAMPLE_MAX;
  int i, j;
  off_t cursamp, start, end;
  old_ep = cp->amp_envs[edpos];
  if (old_ep == NULL) return(NULL);
  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
  new_ep->data_max = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
  new_ep->data_min = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
  new_ep->amp_env_size = old_ep->amp_env_size;
  new_ep->samps_per_bin = old_ep->samps_per_bin;
  end = beg + num - 1;
  start = beg - new_ep->samps_per_bin;
  for (i = 0, j = 0, cursamp = 0; (i < new_ep->amp_env_size) && (cursamp < end); i++, cursamp += new_ep->samps_per_bin) 
    {
      if (cursamp > start)
	{
	  /* if segment is entirely in region, just scale it */
	  if ((cursamp >= beg) && ((cursamp + new_ep->samps_per_bin) <= end))
	    {
	      new_ep->data_max[j] = old_ep->data_max[i];
	      new_ep->data_min[j] = old_ep->data_min[i];
	    }
	  else pick_one_bin(new_ep, j, cursamp, cp, edpos);
	  if (fmin > new_ep->data_min[j]) fmin = new_ep->data_min[j];
	  if (fmax < new_ep->data_max[j]) fmax = new_ep->data_max[j];
	  j++;
	}
      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
    }
  return(new_ep);
}

env_info *amp_env_copy(chan_info *cp, bool reversed, int edpos)
{
  env_info *old_ep, *new_ep = NULL;
  int i, j;
  old_ep = cp->amp_envs[edpos];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = (env_info *)CALLOC(1, sizeof(env_info));
      new_ep->data_max = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
      new_ep->data_min = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      new_ep->fmin = old_ep->fmin;
      new_ep->fmax = old_ep->fmax;
      if (reversed)
	{
	  for (i = 0, j = new_ep->amp_env_size - 1; i < new_ep->amp_env_size; i++, j--) 
	    {
	      new_ep->data_min[j] = old_ep->data_min[i];
	      new_ep->data_max[j] = old_ep->data_max[i];
	    }
	}
      else
	{
	  memcpy((void *)new_ep->data_min, (void *)old_ep->data_min, sizeof(mus_sample_t) * new_ep->amp_env_size);
	  memcpy((void *)new_ep->data_max, (void *)old_ep->data_max, sizeof(mus_sample_t) * new_ep->amp_env_size);
	}
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      
    }
  return(new_ep);
}

void amp_env_env(chan_info *cp, Float *brkpts, int npts, int pos, Float base, Float scaler, Float offset)
{
  env_info *old_ep, *new_ep;
  int i;
  mus_any *e;
  Float val;
  mus_sample_t fmin, fmax;
  old_ep = cp->amp_envs[pos];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if ((new_ep) && 
	  (new_ep->amp_env_size != old_ep->amp_env_size)) 
	new_ep = free_amp_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
	  new_ep->data_max = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	  new_ep->data_min = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	}
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      if (base == 1.0)
	e = mus_make_env(brkpts, npts, scaler, offset, base, 0.0, 0, new_ep->amp_env_size - 1, brkpts);
      else e = mus_make_env(brkpts, npts, 1.0, 0.0, base, 0.0, 0, new_ep->amp_env_size - 1, brkpts);
      fmin = MUS_SAMPLE_MAX;
      fmax = MUS_SAMPLE_MIN;
      for (i = 0; i < new_ep->amp_env_size; i++) 
	{
	  val = mus_env(e);
	  if (val >= 0.0)
	    {
	      new_ep->data_min[i] = (mus_sample_t)(old_ep->data_min[i] * val);
	      new_ep->data_max[i] = (mus_sample_t)(old_ep->data_max[i] * val);
	    }
	  else
	    {
	      new_ep->data_min[i] = (mus_sample_t)(old_ep->data_max[i] * val);
	      new_ep->data_max[i] = (mus_sample_t)(old_ep->data_min[i] * val);
	    }
	  if (new_ep->data_min[i] < fmin) fmin = new_ep->data_min[i];
	  if (new_ep->data_max[i] > fmax) fmax = new_ep->data_max[i];
	}
      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->amp_envs[cp->edit_ctr] = new_ep;
      mus_free(e);
    }
}

void amp_env_env_selection_by(chan_info *cp, mus_any *e, off_t beg, off_t num, int pos)
{
  env_info *old_ep, *new_ep = NULL;
  Float val, xmax = 1.0;
  Float *data;
  mus_sample_t fmax = MUS_SAMPLE_MIN, fmin = MUS_SAMPLE_MAX;
  int i;
  off_t cursamp, start, end;
  old_ep = cp->amp_envs[pos];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if ((new_ep) && 
	  (new_ep->amp_env_size != old_ep->amp_env_size)) 
	new_ep = free_amp_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
	  new_ep->data_max = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	  new_ep->data_min = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	}
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      end = beg + num - 1;
      start = beg - new_ep->samps_per_bin;
      data = mus_data(e);
      xmax = data[mus_env_breakpoints(e) * 2 - 2];
      for (i = 0, cursamp = 0; i < new_ep->amp_env_size; i++, cursamp += new_ep->samps_per_bin) 
	{
	  if ((cursamp >= end) || (cursamp <= start))
	    {
	      new_ep->data_min[i] = old_ep->data_min[i];
	      new_ep->data_max[i] = old_ep->data_max[i];
	    }
	  else
	    {
	      /* if segment is entirely in scaled section, just scale it */
	      if ((cursamp >= beg) && ((cursamp + new_ep->samps_per_bin) <= end))
		{
		  val = mus_env_interp((double)(cursamp - beg) * xmax / (double)num, e);
		  if (val >= 0.0)
		    {
		      new_ep->data_max[i] = (mus_sample_t)(old_ep->data_max[i] * val);
		      new_ep->data_min[i] = (mus_sample_t)(old_ep->data_min[i] * val);
		    }
		  else
		    {
		      new_ep->data_max[i] = (mus_sample_t)(old_ep->data_min[i] * val);
		      new_ep->data_min[i] = (mus_sample_t)(old_ep->data_max[i] * val);
		    }

		}
	      else pick_one_bin(new_ep, i, cursamp, cp, cp->edit_ctr);
	    }
	  if (fmin > new_ep->data_min[i]) fmin = new_ep->data_min[i];
	  if (fmax < new_ep->data_max[i]) fmax = new_ep->data_max[i];
	}
      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->amp_envs[cp->edit_ctr] = new_ep;
    }
}

void amp_env_ptree(chan_info *cp, void *pt, int pos, XEN init_func, bool is_xen)
{
  env_info *old_ep, *new_ep;
  int i;
  bool need_unprotect = false;
  vct *vlo = NULL, *vhi = NULL;
  XEN init_lo = XEN_UNDEFINED, init_hi = XEN_UNDEFINED;
  int hi_loc = 0, lo_loc = 0;
  mus_sample_t fmin, fmax, dmin, dmax;
  old_ep = cp->amp_envs[pos];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if ((new_ep) && 
	  (new_ep->amp_env_size != old_ep->amp_env_size)) 
	new_ep = free_amp_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
	  new_ep->data_max = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	  new_ep->data_min = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	}
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      fmin = MUS_SAMPLE_MAX;
      fmax = MUS_SAMPLE_MIN;
      if (XEN_PROCEDURE_P(init_func))
	{
	  if (is_xen)
	    {
	      init_lo = XEN_CALL_2(init_func,
				   C_TO_XEN_OFF_T(0),
				   C_TO_XEN_OFF_T(new_ep->amp_env_size),
				   "xen-channel init func");
	      lo_loc = snd_protect(init_lo);
	      init_hi = XEN_CALL_2(init_func,
				   C_TO_XEN_OFF_T(0),
				   C_TO_XEN_OFF_T(new_ep->amp_env_size),
				   "xen-channel init func");
	      hi_loc = snd_protect(init_hi);
	      need_unprotect = true;
	    }
	  else
	    {
	      /* probably faster to copy locally than protect from GC */
	      vlo = c_vct_copy((vct *)(XEN_OBJECT_REF(XEN_CALL_2(init_func,
								 C_TO_XEN_OFF_T(0),
								 C_TO_XEN_OFF_T(new_ep->amp_env_size),
								 "ptree-channel init func"))));
	      vhi = c_vct_copy(vlo);
	    }
	}
      for (i = 0; i < new_ep->amp_env_size; i++) 
	{
	  if (is_xen)
	    {
	      XEN val;
	      val = XEN_CALL_3((XEN)pt, C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(old_ep->data_min[i])), init_lo, XEN_TRUE, "xen-channel");
	      dmin = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE_OR_ELSE(val, 0.0));
	      val = XEN_CALL_3((XEN)pt, C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(old_ep->data_max[i])), init_hi, XEN_TRUE, "xen-channel");
	      dmax = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE_OR_ELSE(val, 0.0));
	    }
	  else
	    {
	      if (vlo)
		{
		  dmin = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f1v1b2f(pt, MUS_SAMPLE_TO_FLOAT(old_ep->data_min[i]), vlo, true));
		  dmax = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f1v1b2f(pt, MUS_SAMPLE_TO_FLOAT(old_ep->data_max[i]), vhi, true));
		}
	      else
		{
		  dmin = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f2f(pt, MUS_SAMPLE_TO_FLOAT(old_ep->data_min[i])));
		  dmax = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f2f(pt, MUS_SAMPLE_TO_FLOAT(old_ep->data_max[i])));
		}
	    }
	  if (dmin <= dmax)
	    {
	      new_ep->data_min[i] = dmin;
	      new_ep->data_max[i] = dmax;
	    }
	  else
	    {
	      new_ep->data_min[i] = dmax;
	      new_ep->data_max[i] = dmin;
	    }
	  if (new_ep->data_min[i] < fmin) fmin = new_ep->data_min[i];
	  if (new_ep->data_max[i] > fmax) fmax = new_ep->data_max[i];
	}
      if (is_xen)
	{
	  if (need_unprotect)
	    {
	      snd_unprotect_at(lo_loc);
	      snd_unprotect_at(hi_loc);
	    }
	}
      else
	{
	  if (vlo) c_free_vct(vlo);
	  if (vhi) c_free_vct(vhi);
	}
      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->amp_envs[cp->edit_ctr] = new_ep;
    }
}

void amp_env_ptree_selection(chan_info *cp, void *pt, off_t beg, off_t num, int pos, XEN init_func, bool is_xen)
{
  env_info *old_ep, *new_ep = NULL;
  mus_sample_t fmax = MUS_SAMPLE_MIN, fmin = MUS_SAMPLE_MAX, dmin, dmax;
  int i;
  bool closure = false, inited = false, need_unprotect = false;
  vct *vlo = NULL, *vhi = NULL;
  off_t cursamp, start, end;
  XEN init_lo = XEN_FALSE, init_hi = XEN_FALSE;
  int hi_loc = 0, lo_loc = 0;
  old_ep = cp->amp_envs[pos];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if ((new_ep) && 
	  (new_ep->amp_env_size != old_ep->amp_env_size)) 
	new_ep = free_amp_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
	  new_ep->data_max = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	  new_ep->data_min = (mus_sample_t *)MALLOC(old_ep->amp_env_size * sizeof(mus_sample_t));
	}
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      end = beg + num - 1;
      start = beg - new_ep->samps_per_bin;
      if (XEN_PROCEDURE_P(init_func)) closure = true;
      for (i = 0, cursamp = 0; i < new_ep->amp_env_size; i++, cursamp += new_ep->samps_per_bin) 
	{
	  if ((cursamp >= end) || (cursamp <= start))
	    {
	      new_ep->data_min[i] = old_ep->data_min[i];
	      new_ep->data_max[i] = old_ep->data_max[i];
	    }
	  else
	    {
	      if ((cursamp >= beg) && ((cursamp + new_ep->samps_per_bin) <= end))
		{
		  if (is_xen)
		    {
		      XEN val;
		      if ((!inited) && (closure))
			{
			  init_lo = XEN_CALL_2(init_func,
					       C_TO_XEN_OFF_T((off_t)((Float)(cursamp - beg) / (Float)(num))),
					       C_TO_XEN_OFF_T((off_t)(num / new_ep->samps_per_bin)),
					       "xen-channel init-func");
			  lo_loc = snd_protect(init_lo);
			  init_hi = XEN_CALL_2(init_func,
					       C_TO_XEN_OFF_T((off_t)((Float)(cursamp - beg) / (Float)(num))),
					       C_TO_XEN_OFF_T((off_t)(num / new_ep->samps_per_bin)),
					       "xen-channel init-func");
			  hi_loc = snd_protect(init_hi);
			  need_unprotect = true;
			  inited = true;
			}
		      val = XEN_CALL_3((XEN)pt, C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(old_ep->data_min[i])), init_lo, XEN_TRUE, "xen-channel");
		      dmin = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE_OR_ELSE(val, 0.0));
		      val = XEN_CALL_3((XEN)pt, C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(old_ep->data_max[i])), init_hi, XEN_TRUE, "xen-channel");
		      dmax = MUS_FLOAT_TO_SAMPLE(XEN_TO_C_DOUBLE_OR_ELSE(val, 0.0));
		    }
		  else
		    {
		      if (closure)
			{
			  if (!inited)
			    {
			      vlo = c_vct_copy((vct *)(XEN_OBJECT_REF(XEN_CALL_2(init_func,
										 C_TO_XEN_OFF_T((off_t)((Float)(cursamp - beg) / (Float)(num))),
										 C_TO_XEN_OFF_T((off_t)(num / new_ep->samps_per_bin)),
										 "ptree-channel init func"))));
			      vhi = c_vct_copy(vlo);
			      inited = true;
			    }
			  dmin = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f1v1b2f(pt, MUS_SAMPLE_TO_FLOAT(old_ep->data_min[i]), vlo, true));
			  dmax = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f1v1b2f(pt, MUS_SAMPLE_TO_FLOAT(old_ep->data_max[i]), vhi, true));
			}
		      else
			{
			  dmin = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f2f(pt, MUS_SAMPLE_TO_FLOAT(old_ep->data_min[i])));
			  dmax = MUS_FLOAT_TO_SAMPLE(evaluate_ptree_1f2f(pt, MUS_SAMPLE_TO_FLOAT(old_ep->data_max[i])));
			}
		    }
		  if (dmin <= dmax)
		    {
		      new_ep->data_min[i] = dmin;
		      new_ep->data_max[i] = dmax;
		    }
		  else
		    {
		      new_ep->data_min[i] = dmax;
		      new_ep->data_max[i] = dmin;
		    }
		}
	      else pick_one_bin(new_ep, i, cursamp, cp, cp->edit_ctr);
	    }
	  if (fmin > new_ep->data_min[i]) fmin = new_ep->data_min[i];
	  if (fmax < new_ep->data_max[i]) fmax = new_ep->data_max[i];
	}
      if (is_xen)
	{
	  if (need_unprotect)
	    {
	      snd_unprotect_at(lo_loc);
	      snd_unprotect_at(hi_loc);
	    }
	}
      else
	{
	  if (vlo) c_free_vct(vlo);
	  if (vhi) c_free_vct(vhi);
	}
      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->amp_envs[cp->edit_ctr] = new_ep;
    }
}

env_info *make_mix_input_amp_env(chan_info *cp)
{
  env_state *es;
  if (CURRENT_SAMPLES(cp) > AMP_ENV_CUTOFF)
    {
      es = make_env_state(cp, CURRENT_SAMPLES(cp)); /* sets cp->amp_envs[pos] */
      while (!(tick_amp_env(cp, es)));
      if (es->sf) es->sf = free_snd_fd(es->sf);
      FREE(es);
      return(cp->amp_envs[cp->edit_ctr]);
    }
  return(NULL);
}

void amp_env_insert_zeros(chan_info *cp, off_t beg, off_t num, int pos)
{
  env_info *old_ep, *new_ep;
  off_t end, old_samps, cur_samps;
  int i, j, subsamp, val, bins;
  old_ep = cp->amp_envs[pos];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if (new_ep) new_ep = free_amp_env(cp, cp->edit_ctr);
      old_samps = cp->samples[pos];
      cur_samps = cp->samples[cp->edit_ctr];
      val = (int)(log((double)(cur_samps)));
      if (val > 20) val = 20;
      val = snd_ipow2(val);
      subsamp = val / old_ep->amp_env_size;
      if (subsamp != 1) return;
      new_ep = (env_info *)CALLOC(1, sizeof(env_info));
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      new_ep->amp_env_size = (int)(ceil(cur_samps / new_ep->samps_per_bin));
      new_ep->completed = true;
      cp->amp_envs[cp->edit_ctr] = new_ep;
      new_ep->bin = new_ep->amp_env_size;
      new_ep->top_bin = new_ep->amp_env_size;
      new_ep->data_max = (mus_sample_t *)CALLOC(new_ep->amp_env_size, sizeof(mus_sample_t));
      new_ep->data_min = (mus_sample_t *)CALLOC(new_ep->amp_env_size, sizeof(mus_sample_t));
      new_ep->fmin = old_ep->fmin;
      if (new_ep->fmin > MUS_SAMPLE_0) new_ep->fmin = MUS_SAMPLE_0;
      new_ep->fmax = old_ep->fmax;
      if (new_ep->fmax < MUS_SAMPLE_0) new_ep->fmax = MUS_SAMPLE_0;
      end = beg + num - 1;
      if (beg == 0)
	{
	  /* insert at start, so copy to end */
	  i = (int)ceil(end / new_ep->samps_per_bin);
	  bins = new_ep->amp_env_size - i;
	  if (old_ep->amp_env_size < bins) bins = old_ep->amp_env_size;
	  memcpy((void *)(&(new_ep->data_min[i])), (void *)old_ep->data_min, sizeof(mus_sample_t) * bins);
	  memcpy((void *)(&(new_ep->data_max[i])), (void *)old_ep->data_max, sizeof(mus_sample_t) * bins);
	}
      else
	{
	  if (beg >= old_samps)
	    {
	      /* copy start */
	      bins = (int)floor(beg / old_ep->samps_per_bin);
	      if (bins > old_ep->amp_env_size) bins = old_ep->amp_env_size;
	      memcpy((void *)new_ep->data_min, (void *)old_ep->data_min, sizeof(mus_sample_t) * bins);
	      memcpy((void *)new_ep->data_max, (void *)old_ep->data_max, sizeof(mus_sample_t) * bins);
	    }
	  else
	    {
	      i = (int)floor(beg / old_ep->samps_per_bin);
	      if (i > 0)
		{
		  memcpy((void *)new_ep->data_min, (void *)old_ep->data_min, sizeof(mus_sample_t) * i);
		  memcpy((void *)new_ep->data_max, (void *)old_ep->data_max, sizeof(mus_sample_t) * i);
		}
	      if (i < new_ep->amp_env_size)
		{
		  pick_one_bin(new_ep, i, i * old_ep->samps_per_bin, cp, cp->edit_ctr);
		  i++;
		}
	      j = (int)floor(end / new_ep->samps_per_bin);
	      if (j < new_ep->amp_env_size)
		{
		  pick_one_bin(new_ep, j, j * new_ep->samps_per_bin, cp, cp->edit_ctr);
		  j++;
		}
	      if (i < old_ep->amp_env_size)
		{
		  bins = new_ep->amp_env_size - j;
		  if ((i + bins) >= old_ep->amp_env_size)
		    bins = old_ep->amp_env_size - i;
		  memcpy((void *)(&(new_ep->data_min[j])), (void *)(&(old_ep->data_min[i])), sizeof(mus_sample_t) * bins);
		  memcpy((void *)(&(new_ep->data_max[j])), (void *)(&(old_ep->data_max[i])), sizeof(mus_sample_t) * bins);
		}
	    }
	}
    }
}




/* -------- control panel speed -------- */

static char src_txt_buf[8];

#define TOTAL_RATS 77

static char *rat_names[] = {
  "1/20", "5/96", "1/16", "3/40", "5/64", "1/12", "3/32", "1/10", "5/48", "7/64", " 1/8", "9/64", "3/20", "5/32", 
  " 1/6", "3/16", " 1/5", "5/24", "7/32", " 1/4", "9/32", "3/10", "5/16", " 1/3", " 3/8", " 2/5", "5/12", "7/16", 
  " 1/2", "9/16", " 3/5", " 5/8", " 2/3", " 3/4", " 4/5", " 5/6", " 7/8", " 1/1", " 9/8", " 6/5", " 5/4", " 4/3", 
  " 3/2", " 8/5", " 5/3", " 7/4", " 2/1", " 9/4", "12/5", " 5/2", " 8/3", " 3/1", "16/5", "10/3", " 7/2", " 4/1", 
  " 9/2", "24/5", " 5/1", "16/3", " 6/1", "32/5", "20/3", " 7/1", " 8/1", " 9/1", "48/5", "10/1", "32/3", "12/1", 
  "64/5", "40/3", "14/1", "16/1", "18/1", "96/5", "20/1"};

static Float rat_values[] = {
  0.05, 0.052083332, 0.0625, 0.075, 0.078125, 0.083333336, 0.09375, 0.1, 0.104166664, 0.109375, 0.125, 0.140625, 
  0.15, 0.15625, 0.16666667, 0.1875, 0.2, 0.20833333, 0.21875, 0.25, 0.28125, 0.3, 0.3125, 0.33333334, 0.375, 
  0.4, 0.41666666, 0.4375, 0.5, 0.5625, 0.6, 0.625, 0.6666667, 0.75, 0.8, 0.8333333, 0.875, 1.0, 1.125, 1.2, 
  1.25, 1.3333334, 1.5, 1.6, 1.6666666, 1.75, 2.0, 2.25, 2.4, 2.5, 2.6666667, 3.0, 3.2, 3.3333333, 3.5, 4.0, 
  4.5, 4.8, 5.0, 5.3333335, 6.0, 6.4, 6.6666665, 7.0, 8.0, 9.0, 9.6, 10.0, 10.666667, 12.0, 12.8, 13.333333, 
  14.0, 16.0, 18.0, 19.2, 20.0};

Float srate_changed(Float val, char *srcbuf, speed_style_t style, int tones)
{
  char *sfs;
  int semi, i, j;
  switch (style)
    {
    case SPEED_CONTROL_AS_RATIO: 
      for (i = 1; i < TOTAL_RATS; i++)
	if (rat_values[i] > val) 
	  break;
      sprintf(srcbuf, "%s", rat_names[i - 1]);
      return(rat_values[i - 1]);
      break;
    case SPEED_CONTROL_AS_SEMITONE: 
      /* find closest semitone to val */
      semi = snd_round(log(val) * ((Float)tones / log(2.0)));
      /* space until (-) num (-52 to 52 is its range if 12-tone) */
      for (i = 0; i < 3; i++) srcbuf[i] = ' '; 
      sprintf(src_txt_buf, "%d", semi);
      j = strlen(src_txt_buf) - 1;
      for (i = 3; (i >= 0) && (j >= 0); i--, j--) 
	srcbuf[i] = src_txt_buf[j];
      return(pow(2.0, ((Float)semi / (Float)tones)));
      break;
    default: 
      sfs = prettyf(val, 2);
      fill_number(sfs, srcbuf);
      if (sfs) FREE(sfs);
      return(val);
      break;
    }
}


/* -------- name click etc */

static char sname[PRINT_BUFFER_SIZE];
char *shortname(snd_info *sp)
{
  if (link_p(sp->filename))
    {
      mus_snprintf(sname, PRINT_BUFFER_SIZE, "(%s)", sp->short_filename);
      return(sname);
    }
  return(sp->short_filename);
}

char *shortname_indexed(snd_info *sp)
{
  if (show_indices(sp->state))
    {
      if (link_p(sp->filename))
	mus_snprintf(sname, PRINT_BUFFER_SIZE, "%d: (%s)", sp->index, sp->short_filename); /* don't try to share sname */
      else mus_snprintf(sname, PRINT_BUFFER_SIZE, "%d: %s", sp->index, sp->short_filename);
      return(sname);
    }
  return(shortname(sp));
}

void add_sound_data(char *filename, snd_info *sp, channel_graph_t graphed)
{
  int i;
  for (i = 0; i < sp->nchans; i++) 
    add_channel_data(filename, sp->chans[i], graphed);
}


static char timebuf[TIME_STR_SIZE];

#if HAVE_READLINK
static char *link_file = NULL;
static char *linked_file(char *link_name)
{
  int bytes;
  #define READLINK_FILE_SIZE 256
  if (link_file == NULL) link_file = (char *)CALLOC(READLINK_FILE_SIZE, sizeof(char));
  bytes = readlink(link_name, link_file, READLINK_FILE_SIZE);
  link_file[bytes] = 0;
  return(link_file);
}
#endif

static XEN name_click_hook;

void sp_name_click(snd_info *sp)
{
  file_info *hdr;
  Float dur;
  bool linked = false;
  if (sp)
    {
      /* call name-click-hook (if any) return #t = don't print info in minibuffer */
      if ((XEN_HOOKED(name_click_hook)) &&
	  (XEN_TRUE_P(run_or_hook(name_click_hook, 
				  XEN_LIST_1(C_TO_SMALL_XEN_INT(sp->index)),
				  S_name_click_hook))))
	return;
      hdr = sp->hdr;
      if (hdr)
	{
	  linked = link_p(sp->filename);
	  dur = (Float)((double)(hdr->samples) / (double)(hdr->chans * hdr->srate));
#if HAVE_STRFTIME
	  strftime(timebuf,
		   TIME_STR_SIZE,
		   STRFTIME_FORMAT,
		   localtime(&(sp->write_date)));
#else
	  sprintf(timebuf, "");
#endif
	  report_in_minibuffer(sp, "%d, %d chan%s, %.3f sec%s, %s: %s, %s%s%s%s",
			       hdr->srate,
			       hdr->chans,
			       ((hdr->chans > 1) ? "s" : ""),
			       dur,
			       ((dur == 1.0) ? "" : "s"),
			       mus_header_type_name(hdr->type),
			       mus_short_data_format_name(hdr->format),
			       timebuf,
			       (linked) ? ", (link to " : "",
#if HAVE_READLINK
			       (linked) ? linked_file(sp->filename) : "",
#else
			       (linked) ? "?" : "",
#endif
			       (linked) ? ")" : "");
	}
    }
}



/* ---------------- save and restore control panel buttons ----------------*/

typedef struct {
  Float amp, srate, contrast, expand, revscl, revlen;
  env *filter_env;
  bool expand_on, contrast_on, reverb_on, filter_on, direction;
  int filter_order;
} ctrl_state;

void free_controls(snd_info *sp)
{
  ctrl_state *cs;
  cs = (ctrl_state *)(sp->saved_controls);
  if (cs)
    {
      if (cs->filter_env) free_env(cs->filter_env);
      FREE(cs);
      sp->saved_controls = NULL;
    }
}

void save_controls(snd_info *sp) 
{
  ctrl_state *cs;
  cs = (ctrl_state *)(sp->saved_controls);
  if (!cs)
    {
      sp->saved_controls = (ctrl_state *)CALLOC(1, sizeof(ctrl_state));
      cs = (ctrl_state *)(sp->saved_controls);
    }
  cs->amp = sp->amp_control;
  cs->srate = sp->speed_control;
  cs->expand = sp->expand_control;
  cs->revscl = sp->reverb_control_scale;
  cs->revlen = sp->reverb_control_length;
  cs->contrast = sp->contrast_control;
  cs->expand_on = sp->expand_control_p;
  cs->reverb_on = sp->reverb_control_p;
  cs->contrast_on = sp->contrast_control_p;
  cs->filter_on = sp->filter_control_p;
  cs->filter_order = sp->filter_control_order;
  if (sp->filter_control_env) 
    {
      if (cs->filter_env) cs->filter_env = free_env(cs->filter_env);
      cs->filter_env = copy_env(sp->filter_control_env);
    }
  if (sp->speed_control_direction == 1) 
    cs->direction = false; 
  else cs->direction = true;
}

void restore_controls(snd_info *sp) 
{
  ctrl_state *cs;
  cs = (ctrl_state *)(sp->saved_controls);
  if (!cs) 
    {
      sp->saved_controls = (ctrl_state *)CALLOC(1, sizeof(ctrl_state));
      cs = (ctrl_state *)(sp->saved_controls);
      cs->amp = DEFAULT_AMP_CONTROL;
      cs->srate = DEFAULT_SPEED_CONTROL;
      cs->direction = 0; /* 0 = forward, 1 = backward (this is the button's view) */
      cs->expand = DEFAULT_EXPAND_CONTROL;
      cs->expand_on = DEFAULT_EXPAND_CONTROL_P;
      cs->revscl = DEFAULT_REVERB_CONTROL_SCALE;
      cs->revlen = DEFAULT_REVERB_CONTROL_LENGTH;
      cs->reverb_on = DEFAULT_REVERB_CONTROL_P;
      cs->contrast = DEFAULT_CONTRAST_CONTROL;
      cs->contrast_on = DEFAULT_CONTRAST_CONTROL_P;
      cs->filter_on = DEFAULT_FILTER_CONTROL_P;
      cs->filter_order = DEFAULT_FILTER_CONTROL_ORDER;
      cs->filter_env = NULL;
    }
  toggle_expand_button(sp, cs->expand_on);
  toggle_contrast_button(sp, cs->contrast_on);
  toggle_reverb_button(sp, cs->reverb_on);
  toggle_filter_button(sp, cs->filter_on);
  toggle_direction_arrow(sp, cs->direction);
  set_snd_amp(sp, cs->amp);
  set_snd_srate(sp, cs->srate);
  set_snd_contrast(sp, cs->contrast);
  set_snd_expand(sp, cs->expand);
  set_snd_revscl(sp, cs->revscl);
  set_snd_revlen(sp, cs->revlen);
  if (sp->filter_control_env) sp->filter_control_env = free_env(sp->filter_control_env); 
  if (cs->filter_env) 
    sp->filter_control_env = copy_env(cs->filter_env);
  else sp->filter_control_env = default_env(sp->filter_control_env_xmax, 1.0);
  set_snd_filter_order(sp, cs->filter_order);
}

void reset_controls(snd_info *sp) 
{
  toggle_expand_button(sp, DEFAULT_EXPAND_CONTROL_P);
  toggle_contrast_button(sp, DEFAULT_CONTRAST_CONTROL_P);
  toggle_reverb_button(sp, DEFAULT_REVERB_CONTROL_P);
  toggle_filter_button(sp, DEFAULT_FILTER_CONTROL_P);
  toggle_direction_arrow(sp, 0);
  set_snd_amp(sp, DEFAULT_AMP_CONTROL);
  set_snd_srate(sp, DEFAULT_SPEED_CONTROL);
  set_snd_contrast(sp, DEFAULT_CONTRAST_CONTROL);
  set_snd_expand(sp, DEFAULT_EXPAND_CONTROL);
  set_snd_revscl(sp, DEFAULT_REVERB_CONTROL_SCALE);
  set_snd_revlen(sp, DEFAULT_REVERB_CONTROL_LENGTH);
  set_snd_filter_order(sp, DEFAULT_FILTER_CONTROL_ORDER);
  if (sp->filter_control_env) sp->filter_control_env = free_env(sp->filter_control_env);
  sp->filter_control_env = default_env(sp->filter_control_env_xmax, 1.0);
}


/* ---------------- minibuffer/filter text history ---------------- */

typedef struct {
  char **strings;
  int strings_size, strings_pos;
  bool first_time;
} mini_history;
  
static mini_history *listener_history = NULL;
enum {MINIBUFFER, FILTER_TEXT, LISTENER_TEXT};

static void remember_string(snd_info *sp, char *str, int which)
{
  /* sp can be NULL */
  mini_history *mh = NULL;
  int i, top;
  switch (which)
    {
    case MINIBUFFER: mh = (mini_history *)(sp->minibuffer_history); break;
    case FILTER_TEXT: mh = (mini_history *)(sp->filter_history); break;
    case LISTENER_TEXT: mh = listener_history; break;
    default: return; break;
    }
  if (mh == NULL)
    {
      mh = (mini_history *)CALLOC(1, sizeof(mini_history));
      mh->strings_size = minibuffer_history_length(get_global_state());
      mh->strings = (char **)CALLOC(mh->strings_size, sizeof(char *));
      switch (which)
	{
	case MINIBUFFER: sp->minibuffer_history = (void *)mh; break;
	case FILTER_TEXT: sp->filter_history = (void *)mh; break;
	case LISTENER_TEXT: listener_history = mh; break;
	}
    }
  top = mh->strings_size - 1;
  if (mh->strings[top]) FREE(mh->strings[top]);
  for (i = top; i > 0; i--) mh->strings[i] = mh->strings[i - 1];
  mh->strings[0] = copy_string(str);
  mh->strings_pos = 0;
  mh->first_time = true;
}

void remember_mini_string(snd_info *sp, char *str) {remember_string(sp, str, MINIBUFFER);}
void remember_filter_string(snd_info *sp, char *str) {remember_string(sp, str, FILTER_TEXT);}
void remember_listener_string(char *str) {remember_string(NULL, str, LISTENER_TEXT);}

static void restore_string(snd_info *sp, bool back, int which)
{
  /* sp can be NULL */
  mini_history *mh = NULL;
  char *str;
  switch (which)
    {
    case MINIBUFFER: mh = (mini_history *)(sp->minibuffer_history); break;
    case FILTER_TEXT: mh = (mini_history *)(sp->filter_history); break;
    case LISTENER_TEXT: mh = listener_history; break;
    }
  if (mh)
    {
      if (!(mh->first_time))
	{
	  if (back)
	    mh->strings_pos++;
	  else mh->strings_pos--;
	}
      mh->first_time = false;
      if (mh->strings_pos < 0) mh->strings_pos = 0;
      if (mh->strings_pos > (mh->strings_size - 1)) mh->strings_pos = mh->strings_size - 1;
      str = mh->strings[mh->strings_pos];
      if (str)
	{
	  switch (which)
	    {
	    case MINIBUFFER: set_minibuffer_string(sp, str); break;
	    case FILTER_TEXT: set_filter_text(sp, str); break;
	    case LISTENER_TEXT: append_listener_text(-1, str); break;
	    }
	}
    }
}

void restore_mini_string(snd_info *sp, bool back) {restore_string(sp, back, MINIBUFFER);}
void restore_filter_string(snd_info *sp, bool back) {restore_string(sp, back, FILTER_TEXT);}
void restore_listener_string(bool back) {restore_string(NULL, back, LISTENER_TEXT);}

static void clear_strings(snd_info *sp, int which)
{
  /* sp can be NULL */
  mini_history *mh = NULL;
  int i;
  switch (which)
    {
    case MINIBUFFER: mh = (mini_history *)(sp->minibuffer_history); break;
    case FILTER_TEXT: mh = (mini_history *)(sp->filter_history); break;
    case LISTENER_TEXT: mh = listener_history; break;
    }
  if (mh)
    {
      switch (which)
	{
	case MINIBUFFER: sp->minibuffer_history = NULL; break;
	case FILTER_TEXT: sp->filter_history = NULL; break;
	case LISTENER_TEXT: listener_history = NULL; break;
	}
      for (i = 0; i < mh->strings_size; i++) 
	if (mh->strings[i])
	  FREE(mh->strings[i]);
      FREE(mh->strings);
      FREE(mh);
    }
}

void clear_mini_strings(snd_info *sp) {clear_strings(sp, MINIBUFFER);}
void clear_filter_strings(snd_info *sp) {clear_strings(sp, FILTER_TEXT);}
/* void clear_listener_strings(void) {clear_strings(NULL, LISTENER_TEXT);} */


/* ---------------- control panel apply button ---------------- */

void stop_applying(snd_info *sp)
{
  /* called if user unset the apply button during the apply process */
  sp->apply_ok = false;
}

typedef struct {
  int slice;
  snd_info *sp;
  off_t i;
  int ofd;
  char *ofile;
  ctrl_state *cs;
  file_info *hdr;
} apply_state;

static XEN before_apply_hook;
static XEN after_apply_hook;

static void *make_apply_state(void *xp)
{
  /* set up initial state for apply_controls */
  apply_state *ap = NULL;
  snd_info *sp = (snd_info *)xp;
  /* call apply-hook (if any) return #t = don't apply */
  if ((XEN_HOOKED(before_apply_hook)) &&
      (XEN_TRUE_P(run_or_hook(before_apply_hook, 
			      XEN_LIST_1(C_TO_SMALL_XEN_INT(sp->index)),
			      S_before_apply_hook))))
    return(NULL);
  ap = (apply_state *)CALLOC(1, sizeof(apply_state));
  ap->slice = 0;
  ap->hdr = NULL;
  ap->sp = sp;
  return((void *)ap);
}

#define APPLY_TICKS 4

static void max_sync(snd_info *sp, void *val)
{
  int *maxsync = (int *)val;
  if (sp->sync > maxsync[0])
    maxsync[0] = sp->sync;
}

static int apply_tick = 0;
static bool apply_reporting = false;
static off_t apply_dur = 0, orig_dur, apply_beg = 0;

void *make_apply_state_with_implied_beg_and_dur(void *xp)
{
  apply_beg = 0;
  apply_dur = 0;
  return(make_apply_state(xp));
}

Cessate apply_controls(Indicium ptr)
{
  apply_state *ap = (apply_state *)ptr;
  snd_state *ss;
  snd_context *sgx;
  snd_info *sp;
  chan_info *cp, *ncp;
  sync_info *si;
  Float ratio, mult_dur;
  int i, len, curchan = 0, added_dur = 0, old_sync;
  bool over_selection;
  int maxsync[1];
  Float *scalers = NULL;
  if (ptr == NULL) return(BACKGROUND_QUIT);
  sp = ap->sp;
  if ((!(sp->active)) || (sp->inuse != SOUND_NORMAL)) return(BACKGROUND_QUIT);
  ss = sp->state;
  if (sp->filter_control_p) added_dur = sp->filter_control_order;
  mult_dur = 1.0 / fabs(sp->speed_control);
  if (sp->expand_control_p) mult_dur *= sp->expand_control;
  if (sp->reverb_control_p) added_dur += (int)((SND_SRATE(sp) * reverb_control_decay(ss)));
  if ((ss->apply_choice != APPLY_TO_SELECTION) &&
      (sp->speed_control == 1.0) && (apply_beg == 0) &&
      (sp->speed_control_direction == 1) &&
      (!(sp->filter_control_p)) && (!(sp->expand_control_p)) && (!(sp->reverb_control_p)) && (!(sp->contrast_control_p)))
    {
      old_sync = sp->sync;
      /* get unused sync val */
      if (ss->apply_choice == APPLY_TO_SOUND)
	{
	  maxsync[0] = 0;
	  for_each_sound(ss, max_sync, (void *)maxsync);
	  sp->sync = maxsync[0] + 1;
	}
      else sp->sync = 0;
      /* check for local amp_control vals */
      if (sp->selected_channel == NO_SELECTION) 
	cp = sp->chans[0];
      else cp = sp->chans[sp->selected_channel];
      si = sync_to_chan(cp);
      if (si == NULL) return(BACKGROUND_QUIT);
      scalers = (Float *)CALLOC(si->chans, sizeof(Float));
      for (i = 0; i < si->chans; i++)
	{
	  ncp = si->cps[i];
	  if (ncp->amp_control)
	    scalers[i] = ncp->amp_control[0];
	  else scalers[i] = sp->amp_control;
	}
      scale_by(cp, scalers, si->chans, false);
      sp->sync = old_sync;
      FREE(scalers);
      si = free_sync_info(si);
    }
  else
    {
      switch (ap->slice)
	{
	case 0:
	  /* apply_beg = 0; */
	  ap->ofile = NULL;
	  ap->ofile = snd_tempnam(ss);
	  ap->hdr = make_temp_header(ap->ofile, SND_SRATE(sp), sp->nchans, 0, (char *)c__FUNCTION__);
	  switch (ss->apply_choice)
	    {
	    case APPLY_TO_CHANNEL:   
	      ap->hdr->chans = 1; 
	      if (sp->selected_channel != NO_SELECTION) 
		curchan = sp->selected_channel;
	      if (apply_dur == 0)
		apply_dur = CURRENT_SAMPLES(sp->chans[curchan]) - apply_beg;
	      break;
	    case APPLY_TO_SOUND:     
	      ap->hdr->chans = sp->nchans; 
	      if (apply_dur == 0)
		apply_dur = CURRENT_SAMPLES(sp->chans[0]) - apply_beg;
	      break;
	    case APPLY_TO_SELECTION: 
	      ap->hdr->chans = selection_chans();
	      if (ap->hdr->chans <= 0) return(BACKGROUND_QUIT);
	      if (apply_dur == 0)
		apply_dur = selection_len(); 
	      break;
	    }
	  orig_dur = apply_dur;
	  apply_dur = (off_t)(mult_dur * (apply_dur + added_dur));
	  ap->ofd = open_temp_file(ap->ofile, ap->hdr->chans, ap->hdr, ss);
	  if (ap->ofd == -1)
	    {
	      snd_error(_("can't open apply temp file %s: %s\n"), ap->ofile, strerror(errno));
	      sp->applying = false;
	      FREE(ap);
	      return(BACKGROUND_QUIT);
	    }
	  lock_apply(ss, sp);
	  sp->apply_ok = true;
	  initialize_apply(sp, ap->hdr->chans, apply_beg, apply_dur); /* snd-dac.c, called only here */
	  apply_reporting = (apply_dur > REPORTING_SIZE);
	  if (apply_reporting) 
	    start_progress_report(sp, NOT_FROM_ENVED);
	  ap->i = 0;
	  ap->slice++;
	  return(BACKGROUND_CONTINUE);
	  break;
	  
	case 1:
	  if (!(sp->apply_ok))
	    ap->slice++;
	  else
	    {
	      len = run_apply(ap->ofd); /* returns frames written */
	      ap->i += len;
	      if (ap->i >= apply_dur) ap->slice++;
	      check_for_event(ss);
	      /* if C-G, stop_applying called which cancels and backs out */
	      if (ss->stopped_explicitly)
		{
		  finish_progress_report(sp, NOT_FROM_ENVED);
		  apply_reporting = false;
		  ap->slice++;
		}
	      else
		{
		  if (apply_reporting) 
		    {
		      apply_tick++;
		      if (apply_tick > APPLY_TICKS)
			{
			  apply_tick = 0;
			  progress_report(sp, "apply-controls", 1, 1, (double)(ap->i) / (double)apply_dur, NOT_FROM_ENVED);
			}
		    }
		}
	      /* sr .07 -> infinite output? */
	    }
	  
	  return(BACKGROUND_CONTINUE);
	  break;
	  
	case 2:
	  finalize_apply(sp);
	  if (apply_reporting) finish_progress_report(sp, NOT_FROM_ENVED);
	  close_temp_file(ap->ofd,
			  ap->hdr,
			  apply_dur * (ap->hdr->chans) * mus_bytes_per_sample((ap->hdr)->format),
			  sp);
	  if ((sp->apply_ok) && (apply_dur > 0))
	    {
	      switch (ss->apply_choice)
		{
		case APPLY_TO_SOUND:
		  if (sp->nchans > 1) 
		    remember_temp(ap->ofile, sp->nchans);
		  if (apply_beg > 0)
		    {
		      for (i = 0; i < sp->nchans; i++)
			{
			  file_change_samples(apply_beg, apply_dur, ap->ofile, sp->chans[i], i,
					      (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
					      LOCK_MIXES, "Apply to sound", sp->chans[i]->edit_ctr);
			  update_graph(sp->chans[i]);
			}
		    }
		  else
		    {
		      for (i = 0; i < sp->nchans; i++)
			{
			  file_override_samples(apply_dur, ap->ofile, sp->chans[i], i,
						(sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						LOCK_MIXES, "Apply to sound");
			  update_graph(sp->chans[i]);
			}
		    }
		  break;
		case APPLY_TO_CHANNEL: 
		  if (sp->selected_channel != NO_SELECTION) 
		    curchan = sp->selected_channel;
		  if (apply_beg > 0)
		    file_change_samples(apply_beg, apply_dur, ap->ofile, sp->chans[curchan], 0, 
					DELETE_ME, LOCK_MIXES, "Apply to channel", sp->chans[curchan]->edit_ctr);
		  else file_override_samples(apply_dur, ap->ofile, sp->chans[curchan], 0, 
					     DELETE_ME, LOCK_MIXES, "Apply to channel");
		  update_graph(sp->chans[curchan]);
		  break;
		case APPLY_TO_SELECTION:
		  if (selection_chans() > 1) 
		    remember_temp(ap->ofile, selection_chans());
		  si = selection_sync();
		  if (apply_dur == selection_len())
		    {
		      for (i = 0; i < si->chans; i++)
			{
			  file_change_samples(si->begs[i], apply_dur, ap->ofile, si->cps[i], i,
					      (si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
					      LOCK_MIXES, "Apply to selection", si->cps[i]->edit_ctr);
			  update_graph(si->cps[i]);
			}
		    }
		  else
		    {
		      int ok;
		      ok = delete_selection("Apply to selection", DONT_UPDATE_DISPLAY);
		      if (apply_dur > 0)
			{
			  for (i = 0; i < si->chans; i++)
			    {
			      file_insert_samples(si->begs[i], apply_dur, ap->ofile, si->cps[i], 0, 
						  (si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						  "Apply to selection", si->cps[i]->edit_ctr);
			      reactivate_selection(si->cps[i], si->begs[i], si->begs[i] + apply_dur);
			      if (ok) backup_edit_list(si->cps[i]);
			    }
			}
		    }
		  si = free_sync_info(si); 
		  break;
		}
	      clear_minibuffer(sp);
	      set_apply_button(sp, false);
	      sp->apply_ok = false;
	      
	      if ((sp->expand_control_p) || 
		  (sp->speed_control_direction != 1) || (sp->speed_control != 1.0))
		{
		  for (i = 0; i < sp->nchans; i++)
		    {
		      cp = sp->chans[i];
		      if (cp->marks)
			{
			  if (!(sp->expand_control_p))
			    ratio = sp->speed_control;
			  else ratio = sp->speed_control / sp->expand_control;
			  if (ratio != 1.0)
			    {
			      over_selection = (ss->apply_choice == APPLY_TO_SELECTION);
			      src_marks(cp, ratio, orig_dur, apply_dur, 
					(over_selection) ? selection_beg(cp) : 0,
					over_selection);
			      update_graph(cp);
			    }
			}
		    }
		}
	    }
	  else
	    {
	      snd_remove(ap->ofile, REMOVE_FROM_CACHE);
	    }
	  FREE(ap->ofile);
	  ap->ofile = NULL;
	  if (ap->hdr) ap->hdr = free_file_info(ap->hdr);
	  free_controls(sp);
	  break;
	}
    }
  unlock_apply(ss, sp);
  reset_controls(sp); /* i.e. clear it */
  sp->applying = false;
  sgx = sp->sgx;
  if ((sgx) && (sgx->apply_in_progress)) sgx->apply_in_progress = 0;
  if (XEN_HOOKED(after_apply_hook))
    run_hook(after_apply_hook, 
	     XEN_LIST_1(C_TO_SMALL_XEN_INT(sp->index)),
	     S_after_apply_hook);
  FREE(ap);
  ss->stopped_explicitly = false;
  return(BACKGROUND_QUIT);
}

void remove_apply(snd_info *sp)
{
  snd_context *sgx;
  if ((sp) && (sgx = sp->sgx) && (sgx->apply_in_progress))
    {
      BACKGROUND_REMOVE(sgx->apply_in_progress);
      sgx->apply_in_progress = 0;
    }
}

static void set_reverb_decay(snd_state *ss, Float val) 
{
  int i;
  snd_info *sp;
  in_set_reverb_control_decay(ss, val);
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	sp->reverb_control_decay = val;
    }
}

static void map_sounds_speed_tones(snd_info *sp, void *val) 
{
  sp->speed_control_tones = (*((int *)val)); 
}

static void set_speed_tones(snd_state *ss, int val) 
{
  in_set_speed_control_tones(ss, val); 
  for_each_sound(ss, map_sounds_speed_tones, (void *)(&val));
}      

static void map_sounds_speed_style(snd_info *sp, void *val) 
{
  sp->speed_control_style = (*((speed_style_t *)val)); 
}

void set_speed_style(snd_state *ss, speed_style_t val) 
{
  in_set_speed_control_style(ss, val); 
  for_each_sound(ss, map_sounds_speed_style, (void *)(&val));
}      

XEN snd_no_such_sound_error(const char *caller, XEN n)
{
  XEN_ERROR(NO_SUCH_SOUND,
	    XEN_LIST_2(C_TO_XEN_STRING(caller),
		       n));
  return(XEN_FALSE);
}

static XEN g_sound_p(XEN snd_n)
{
  #define H_sound_p "(" S_sound_p " (index 0)): #t if sound associated with index is active (accessible)"
  snd_info *sp;
  sp = get_sp(snd_n, PLAYERS_OK);
  return(C_TO_XEN_BOOLEAN((sp) && 
			  (snd_ok(sp)) &&
			  (sp->inuse == SOUND_NORMAL)));
}

static XEN g_select_sound(XEN snd_n)
{
  #define H_select_sound "(" S_select_sound " (snd #f)): select snd."
  int val;
  snd_state *ss;
  snd_info *sp;
  ss = get_global_state();
  ASSERT_JUST_SOUND(S_select_sound, snd_n, 1);
  if (XEN_FALSE_P(snd_n))
    ss->selected_sound = NO_SELECTION;
  else
    {
      val = XEN_TO_C_INT_OR_ELSE(snd_n, 0);
      if ((val >= 0) && 
	  (val < ss->max_sounds))
	{
	  sp = ss->sounds[val];
	  if ((snd_ok(sp)) &&
	      (sp->inuse == SOUND_NORMAL))
	    {
	      select_channel(sp, 0);
	      equalize_sound_panes(ss, sp, sp->chans[0], false);
	      for_each_chan(ss, update_graph);
	      return(snd_n);
	    }
	}
      return(snd_no_such_sound_error(S_select_sound, snd_n));
    }
  return(snd_n);
}

static XEN g_select_channel(XEN chn_n)
{
  #define H_select_channel "(" S_select_channel " (chn 0)): select channel"
  snd_info *sp;
  snd_state *ss;
  int chan;
  ss = get_global_state();
  ASSERT_JUST_SOUND(S_select_channel, chn_n, 1);
  chan = XEN_TO_C_INT_OR_ELSE(chn_n, 0);
  sp = any_selected_sound(ss);
  if ((sp) && 
      (chan >= 0) &&
      (chan < sp->nchans)) 
    {
      select_channel(sp, chan);
      return(chn_n);
    }
  return(snd_no_such_channel_error(S_select_channel, C_TO_XEN_STRING("selected-sound"), chn_n));
}

static XEN g_find_sound(XEN filename, XEN which)
{
  #define H_find_sound "(" S_find_sound " name (nth 0)): return the id of the sound associated with file 'name'. \
If more than one such sound exists, 'nth' chooses which one to return."
  snd_state *ss;
  snd_info *sp;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_find_sound, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(which), which, XEN_ARG_2, S_find_sound, "an integer");
  ss = get_global_state();
  sp = find_sound(ss, XEN_TO_C_STRING(filename), XEN_TO_C_INT_OR_ELSE(which, 0));
  if (sp) return(C_TO_XEN_INT(sp->index));
  return(xen_return_first(XEN_FALSE, filename));
}


static XEN g_bomb(XEN snd, XEN on)
{
  #define H_bomb "(" S_bomb " (snd #f) (on #t)): display (or erase if on=#f) the bomb icon"
  snd_info *sp;
  ASSERT_JUST_SOUND(S_bomb, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_bomb, snd));
  x_bomb(sp, XEN_TO_C_BOOLEAN_OR_TRUE(on));
  return(on);
}

enum {SP_SYNC, SP_READ_ONLY, SP_NCHANS, SP_CONTRASTING, SP_EXPANDING, SP_REVERBING, SP_FILTERING, SP_FILTER_ORDER,
      SP_SRATE, SP_DATA_FORMAT, SP_DATA_LOCATION, SP_HEADER_TYPE, SP_SAVE_CONTROLS, SP_RESTORE_CONTROLS, SP_SELECTED_CHANNEL,
      SP_COMMENT, SP_FILE_NAME, SP_SHORT_FILE_NAME, SP_CLOSE, SP_UPDATE, SP_SAVE, SP_CURSOR_FOLLOWS_PLAY, SP_SHOW_CONTROLS,
      SP_FILTER_DBING, SP_SPEED_TONES, SP_SPEED_STYLE, SP_RESET_CONTROLS,
      SP_AMP, SP_CONTRAST, SP_CONTRAST_AMP, SP_EXPAND, SP_EXPAND_LENGTH, SP_EXPAND_RAMP, SP_EXPAND_HOP,
      SP_SPEED, SP_REVERB_LENGTH, SP_REVERB_FEEDBACK, SP_REVERB_SCALE, SP_REVERB_LOW_PASS,
      SP_REVERB_DECAY, SP_PROPERTIES, SP_FILTER_COEFFS, SP_DATA_SIZE
};

static XEN sound_get(XEN snd_n, int fld, char *caller, bool just_sound)
{
  snd_info *sp;
  snd_state *ss;
  int i;
  XEN res = XEN_EMPTY_LIST;
  if (XEN_TRUE_P(snd_n))
    {
      ss = get_global_state();
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res = XEN_CONS(sound_get(C_TO_SMALL_XEN_INT(i), fld, caller, just_sound), res);
	}
      return(res);
    }
  if (just_sound)
    {
      ASSERT_JUST_SOUND(caller, snd_n, 1);
    }
  else 
    {
      ASSERT_SOUND(caller, snd_n, 1);
    }
  sp = get_sp(snd_n, PLAYERS_OK);
  if ((sp == NULL) || (sp->inuse == SOUND_WRAPPER))
    return(snd_no_such_sound_error(caller, snd_n));
  switch (fld)
    {
    case SP_SYNC:                return(C_TO_XEN_INT(sp->sync));                       break;
    case SP_READ_ONLY:           return(C_TO_XEN_BOOLEAN(sp->read_only));              break;
    case SP_NCHANS:              return(C_TO_XEN_INT(sp->nchans));                     break;
    case SP_EXPANDING:           return(C_TO_XEN_BOOLEAN(sp->expand_control_p));       break;
    case SP_CONTRASTING:         return(C_TO_XEN_BOOLEAN(sp->contrast_control_p));     break;
    case SP_REVERBING:           return(C_TO_XEN_BOOLEAN(sp->reverb_control_p));       break;
    case SP_FILTERING:           return(C_TO_XEN_BOOLEAN(sp->filter_control_p));       break;
    case SP_FILTER_DBING:        return(C_TO_XEN_BOOLEAN(sp->filter_control_in_dB));   break;
    case SP_FILTER_ORDER:        return(C_TO_XEN_INT(sp->filter_control_order));       break;
    case SP_SRATE:               return(C_TO_XEN_INT((sp->hdr)->srate));               break;
    case SP_DATA_FORMAT:         return(C_TO_XEN_INT((sp->hdr)->format));              break;
    case SP_HEADER_TYPE:         return(C_TO_XEN_INT((sp->hdr)->type));                break;
    case SP_DATA_LOCATION:       return(C_TO_XEN_OFF_T((sp->hdr)->data_location));     break;
    case SP_DATA_SIZE:           return(C_TO_XEN_OFF_T(mus_samples_to_bytes((sp->hdr)->format, (sp->hdr)->samples))); break;
    case SP_SAVE_CONTROLS:       if (!(IS_PLAYER(sp))) save_controls(sp);              break;
    case SP_RESTORE_CONTROLS:    if (!(IS_PLAYER(sp))) restore_controls(sp);           break;
    case SP_RESET_CONTROLS:      if (!(IS_PLAYER(sp))) reset_controls(sp);             break;
    case SP_SELECTED_CHANNEL:    if (sp->selected_channel != NO_SELECTION) return(C_TO_XEN_INT(sp->selected_channel)); else return(XEN_FALSE); break;
    case SP_FILE_NAME:           return(C_TO_XEN_STRING(sp->filename));                break;
    case SP_SHORT_FILE_NAME:     return(C_TO_XEN_STRING(sp->short_filename));          break;
    case SP_CLOSE:               if (!(IS_PLAYER(sp))) snd_close_file(sp, sp->state);  break;
    case SP_SAVE:                if (!(IS_PLAYER(sp))) save_edits(sp, NULL);           break;
    case SP_UPDATE:              if (!(IS_PLAYER(sp))) {sp = snd_update(sp->state, sp); if (sp) return(C_TO_XEN_INT(sp->index));} break;
    case SP_CURSOR_FOLLOWS_PLAY: return(C_TO_XEN_BOOLEAN(sp->cursor_follows_play));    break;
    case SP_SHOW_CONTROLS:       if (!(IS_PLAYER(sp))) return(C_TO_XEN_BOOLEAN(control_panel_open(sp))); break;
    case SP_SPEED_TONES:         return(C_TO_XEN_INT(sp->speed_control_tones));        break;
    case SP_SPEED_STYLE:         return(C_TO_XEN_INT((int)(sp->speed_control_style))); break;
    case SP_COMMENT:             return(C_TO_XEN_STRING(sp->hdr->comment));            break;
    case SP_PROPERTIES:
      if (!(IS_PLAYER(sp))) 
	{
	  if (!(XEN_VECTOR_P(sp->properties)))
	    {
	      sp->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
	      /* snd_protect(sp->properties); */
	      XEN_PROTECT_FROM_GC(sp->properties);
	    }
	  return(XEN_VECTOR_REF(sp->properties, 0));
	}
      break;
    case SP_AMP:                 return(C_TO_XEN_DOUBLE(sp->amp_control));             break;
    case SP_CONTRAST:            return(C_TO_XEN_DOUBLE(sp->contrast_control));        break;
    case SP_CONTRAST_AMP:        return(C_TO_XEN_DOUBLE(sp->contrast_control_amp));    break;
    case SP_EXPAND:              return(C_TO_XEN_DOUBLE(sp->expand_control));          break;
    case SP_EXPAND_LENGTH:       return(C_TO_XEN_DOUBLE(sp->expand_control_length));   break;
    case SP_EXPAND_RAMP:         return(C_TO_XEN_DOUBLE(sp->expand_control_ramp));     break;
    case SP_EXPAND_HOP:          return(C_TO_XEN_DOUBLE(sp->expand_control_hop));      break;
    case SP_SPEED:           
      if (sp->speed_control_direction == -1) 
	return(C_TO_XEN_DOUBLE((-(sp->speed_control)))); 
      else return(C_TO_XEN_DOUBLE(sp->speed_control)); 
      break;
    case SP_REVERB_LENGTH:       return(C_TO_XEN_DOUBLE(sp->reverb_control_length));   break;
    case SP_REVERB_FEEDBACK:     return(C_TO_XEN_DOUBLE(sp->reverb_control_feedback)); break;
    case SP_REVERB_SCALE:        return(C_TO_XEN_DOUBLE(sp->reverb_control_scale));    break;
    case SP_REVERB_LOW_PASS:     return(C_TO_XEN_DOUBLE(sp->reverb_control_lowpass));  break;
    case SP_REVERB_DECAY:        return(C_TO_XEN_DOUBLE(sp->reverb_control_decay));    break;
    case SP_FILTER_COEFFS: 
      if (sp->filter_control_env)
	{
	  int len;
	  Float *coeffs, *data;
	  len = sp->filter_control_order;
	  coeffs = (Float *)CALLOC(len, len * sizeof(Float));
	  data = sample_linear_env(sp->filter_control_env, len);
	  mus_make_fir_coeffs(len, data, coeffs);
	  FREE(data);
	  return(make_vct(len, coeffs));
	}
      return(XEN_FALSE);
      break;
    }
  return(XEN_FALSE);
}

static XEN sound_set(XEN snd_n, XEN val, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  int i, ival, old_format;
  Float fval;
  if (XEN_TRUE_P(snd_n))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    sound_set(C_TO_SMALL_XEN_INT(i), val, fld, caller);
	}
      return(val);
    }
  ASSERT_JUST_SOUND(caller, snd_n, 2);
  sp = get_sp(snd_n, PLAYERS_OK);
  if ((sp == NULL) || (sp->inuse == SOUND_WRAPPER))
    return(snd_no_such_sound_error(caller, snd_n));
  ss = sp->state;
  switch (fld)
    {
    case SP_SYNC:  
      if (XEN_INTEGER_P(val))
	syncb(sp, XEN_TO_C_INT(val));
      else syncb(sp, (int)XEN_TO_C_BOOLEAN(val));
      break;
    case SP_READ_ONLY:
      if (!(IS_PLAYER(sp)))
	{
	  sp->read_only = XEN_TO_C_BOOLEAN(val); 
	  snd_file_lock_icon(sp, sp->read_only); 
	}
      break;
    case SP_EXPANDING:
      toggle_expand_button(sp, XEN_TO_C_BOOLEAN(val));
      break;
    case SP_CONTRASTING:
      toggle_contrast_button(sp, XEN_TO_C_BOOLEAN(val));
      break;
    case SP_REVERBING:
      toggle_reverb_button(sp, XEN_TO_C_BOOLEAN(val));
      break;
    case SP_FILTERING:
      toggle_filter_button(sp, XEN_TO_C_BOOLEAN(val));
      break;
    case SP_FILTER_DBING:   
      set_filter_in_dB(sp, XEN_TO_C_BOOLEAN(val));
      break;
    case SP_FILTER_ORDER:
      set_snd_filter_order(sp, XEN_TO_C_INT(val));
      break;
    case SP_CURSOR_FOLLOWS_PLAY:
      sp->cursor_follows_play = XEN_TO_C_BOOLEAN(val);
      break;
    case SP_SHOW_CONTROLS:
      if (!(IS_PLAYER(sp))) 
	{
	  if (XEN_TO_C_BOOLEAN(val))
	    sound_show_ctrls(sp); 
	  else sound_hide_ctrls(sp); 
	}
      break;
    case SP_SPEED_TONES:
      sp->speed_control_tones = XEN_TO_C_INT(val);
      if (sp->speed_control_tones <= 0) 
	sp->speed_control_tones = DEFAULT_SPEED_CONTROL_TONES;
      break;
    case SP_SPEED_STYLE:
      sp->speed_control_style = (speed_style_t)XEN_TO_C_INT(val); /* range checked already */
      break;
    case SP_SRATE:
      if (!(IS_PLAYER(sp))) 
	{
	  ival = XEN_TO_C_INT(val);
	  if ((ival <= 0) || (ival > 100000000))
	    XEN_OUT_OF_RANGE_ERROR(S_setB S_srate, 1, val, "~A: impossible srate");
	  mus_sound_set_srate(sp->filename, ival);
	  sp->hdr->srate = ival;
	  snd_update(ss, sp); 
	}
      break;
    case SP_NCHANS: 
      if (!(IS_PLAYER(sp))) 
	{
	  ival = XEN_TO_C_INT(val);
	  if ((ival <= 0) || (ival > 256))
	    XEN_OUT_OF_RANGE_ERROR(S_setB S_channels, 1, val, "~A: highly unlikely number of channels");
	  mus_sound_set_chans(sp->filename, ival);
	  sp->hdr->chans = ival;
	  snd_update(ss, sp); 
	}
      break;
    case SP_DATA_FORMAT:
      if (!(IS_PLAYER(sp))) 
	{
	  ival = XEN_TO_C_INT(val);
	  if (MUS_DATA_FORMAT_OK(ival))
	    {
	      int i;
	      chan_info *cp;
	      old_format = sp->hdr->format;
	      mus_sound_set_data_format(sp->filename, ival);
	      sp->hdr->format = ival;
	      if (mus_bytes_per_sample(old_format) != mus_bytes_per_sample(ival))
		{
		  sp->hdr->samples = (sp->hdr->samples * mus_bytes_per_sample(old_format)) / mus_bytes_per_sample(ival);
		  mus_sound_set_samples(sp->filename, sp->hdr->samples);
		}
	      /* clear peak amp envs, if any -- is this right?  (snd-update below...) */
	      for (i = 0; i < sp->nchans; i++)
		{
		  cp = sp->chans[i];
		  if ((cp) && (cp->amp_envs) && (cp->amp_envs[cp->edit_ctr]))
		    cp->amp_envs[cp->edit_ctr] = free_amp_env(cp, cp->edit_ctr);
		}
	      snd_update(ss, sp);
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_setB S_data_format, 1, val, "~A: unknown data format");
	}
      break;
    case SP_HEADER_TYPE:
      if (!(IS_PLAYER(sp))) 
	{
	  ival = XEN_TO_C_INT(val);
	  if (MUS_HEADER_TYPE_OK(ival))
	    {
	      mus_sound_set_header_type(sp->filename, ival);
	      snd_update(ss, sp); 
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_setB S_header_type, 1, val, "~A: unknown header type");
	}
      break;
    case SP_DATA_LOCATION:  
      if (!(IS_PLAYER(sp))) 
	{
	  off_t loc;
	  loc = XEN_TO_C_OFF_T(val);
	  if (loc >= 0)
	    {
	      mus_sound_set_data_location(sp->filename, loc);
	      snd_update(ss, sp); 
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_setB S_data_location, 1, val, "data location ~A < 0?");
	}
      break;
    case SP_DATA_SIZE:  
      if (!(IS_PLAYER(sp))) 
	{
	  off_t size;
	  size = XEN_TO_C_OFF_T(val);
	  if (size >= 0)
	    {
	      mus_sound_set_samples(sp->filename, mus_bytes_to_samples((sp->hdr)->format, size));
	      snd_update(ss, sp); 
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_setB S_data_size, 1, val, "data size ~A < 0?");
	}
      break;
    case SP_COMMENT:
      if (!(IS_PLAYER(sp))) 
	{
	  if (sp->hdr->comment) FREE(sp->hdr->comment);
	  sp->hdr->comment = copy_string(XEN_TO_C_STRING(val));
	}
      break;
    case SP_PROPERTIES:
      if (!(IS_PLAYER(sp)))
	{
	  if (!(XEN_VECTOR_P(sp->properties)))
	    {
	      sp->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
	      /* snd_protect(sp->properties); */
	      XEN_PROTECT_FROM_GC(sp->properties); /* permanent */
	    }
	  XEN_VECTOR_SET(sp->properties, 0, val);
	  return(XEN_VECTOR_REF(sp->properties, 0));
	}
      break;
    case SP_AMP:           
      fval = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (fval >= 0.0) set_snd_amp(sp, fval); 
      return(C_TO_XEN_DOUBLE(sp->amp_control)); 
      break;
    case SP_CONTRAST:      
      set_snd_contrast(sp, XEN_TO_C_DOUBLE_WITH_CALLER(val, caller));
      return(C_TO_XEN_DOUBLE(sp->contrast_control)); 
      break;
    case SP_CONTRAST_AMP:  
      sp->contrast_control_amp = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (sp->playing) dac_set_contrast_amp(sp, sp->contrast_control_amp);
      break;
    case SP_EXPAND:        
      fval = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (fval > 0.0) set_snd_expand(sp, fval); 
      return(C_TO_XEN_DOUBLE(sp->expand_control)); 
      break;
    case SP_EXPAND_LENGTH: 
      fval = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (fval > 0.0) 
	{
	  sp->expand_control_length = fval; 
	  if (sp->playing) dac_set_expand_length(sp, sp->expand_control_length);
	}
      return(C_TO_XEN_DOUBLE(sp->expand_control_length));
      break;
    case SP_EXPAND_RAMP:   
      fval = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if ((fval >= 0.0) && (fval < 0.5)) 
	{
	  sp->expand_control_ramp = fval; 
	  if (sp->playing) dac_set_expand_ramp(sp, fval); 
	}
      return(C_TO_XEN_DOUBLE(sp->expand_control_ramp));
      break;
    case SP_EXPAND_HOP:    
      fval = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (fval > 0.0) 
	{
	  sp->expand_control_hop = fval; 
	  if (sp->playing) dac_set_expand_hop(sp, fval); 
	}
      return(C_TO_XEN_DOUBLE(sp->expand_control_hop));
      break;
    case SP_SPEED: 
      fval = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (fval != 0.0)
	{
	  int direction;
	  if (fval > 0.0) direction = 1; else direction = -1;
	  set_snd_srate(sp, direction * fval); 
	  toggle_direction_arrow(sp, (direction == -1));
	  if (sp->speed_control_direction == -1) 
	    return(C_TO_XEN_DOUBLE((-(sp->speed_control)))); 
	  else return(C_TO_XEN_DOUBLE(sp->speed_control));
	}
      break;
    case SP_REVERB_LENGTH:    
      fval = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (fval >= 0.0) set_snd_revlen(sp, fval); 
      return(C_TO_XEN_DOUBLE(sp->reverb_control_length)); 
      break;
    case SP_REVERB_FEEDBACK:  
      sp->reverb_control_feedback = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (sp->playing) dac_set_reverb_feedback(sp, sp->reverb_control_feedback);
      break;
    case SP_REVERB_SCALE:     
      set_snd_revscl(sp, XEN_TO_C_DOUBLE_WITH_CALLER(val, caller));
      return(C_TO_XEN_DOUBLE(sp->reverb_control_scale)); 
      break;
    case SP_REVERB_LOW_PASS:  
      sp->reverb_control_lowpass = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      if (sp->playing) dac_set_reverb_lowpass(sp, sp->reverb_control_lowpass);
      break;
    case SP_REVERB_DECAY:     
      sp->reverb_control_decay = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      break;
    }
  return(val);
}

#define MIX_OK false
#define NO_MIX true

static XEN g_channels(XEN snd_n)
{
  #define H_channels "("  S_channels " (snd #f)): how many channels snd has"
  return(sound_get(snd_n, SP_NCHANS, S_channels, MIX_OK));
}

static XEN check_number(XEN val, char *caller)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, caller, "a number");
  return(val);
}

static XEN g_set_channels(XEN snd_n, XEN val)
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd_n, S_setB S_channels), SP_NCHANS, S_setB S_channels));
  else return(sound_set(snd_n, check_number(val, S_setB S_channels), SP_NCHANS, S_setB S_channels));
}

static XEN g_srate(XEN snd_n) 
{
  #define H_srate "(" S_srate " (snd #f)): snd's srate"
  return(sound_get(snd_n, SP_SRATE, S_srate, MIX_OK));
}

static XEN g_set_srate(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd_n, S_setB S_srate), SP_SRATE, S_setB S_srate));
  else return(sound_set(snd_n, check_number(val, S_setB S_srate), SP_SRATE, S_setB S_srate));
}

static XEN g_data_location(XEN snd_n) 
{
  #define H_data_location "(" S_data_location " (snd #f)): snd's data location (bytes)"
  return(sound_get(snd_n, SP_DATA_LOCATION, S_data_location, MIX_OK));
}

static XEN g_set_data_location(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd_n, S_setB S_data_location), SP_DATA_LOCATION, S_setB S_data_location));
  else return(sound_set(snd_n, check_number(val, S_setB S_data_location), SP_DATA_LOCATION, S_setB S_data_location));
}

static XEN g_data_size(XEN snd_n) 
{
  #define H_data_size "(" S_data_size " (snd #f)): snd's data size (bytes)"
  return(sound_get(snd_n, SP_DATA_SIZE, S_data_size, MIX_OK));
}

static XEN g_set_data_size(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd_n, S_setB S_data_size), SP_DATA_SIZE, S_setB S_data_size));
  else return(sound_set(snd_n, check_number(val, S_setB S_data_size), SP_DATA_SIZE, S_setB S_data_size));
}

static XEN g_data_format(XEN snd_n) 
{
  #define H_data_format "(" S_data_format " (snd #f)): snd's data format (e.g. " S_mus_bshort ")"
  return(sound_get(snd_n, SP_DATA_FORMAT, S_data_format, MIX_OK));
}

static XEN g_set_data_format(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd_n, S_setB S_data_format), SP_DATA_FORMAT, S_setB S_data_format));
  else return(sound_set(snd_n, check_number(val, S_setB S_data_format), SP_DATA_FORMAT, S_setB S_data_format));
}

static XEN g_header_type(XEN snd_n) 
{
  #define H_header_type "(" S_header_type " (snd #f)): snd's header type (e.g. " S_mus_aiff ")"
  return(sound_get(snd_n, SP_HEADER_TYPE, S_header_type, MIX_OK));
}

static XEN g_set_header_type(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd_n, S_setB S_header_type), SP_HEADER_TYPE, S_setB S_header_type));
  else return(sound_set(snd_n, check_number(val, S_setB S_header_type), SP_HEADER_TYPE, S_setB S_header_type));
}

static XEN g_comment(XEN snd_n)
{
  #define H_comment "(" S_comment " (snd #f)): snd's comment (in its header)"
  return(sound_get(snd_n, SP_COMMENT, S_comment, MIX_OK));
}

static XEN g_set_comment(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    {
      XEN_ASSERT_TYPE(XEN_STRING_P(snd_n), snd_n, XEN_ARG_1, S_setB S_comment, "a string");
      return(sound_set(XEN_UNDEFINED, snd_n, SP_COMMENT, S_setB S_comment));
    }
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ARG_2, S_setB S_comment, "a string");
  return(sound_set(snd_n, val, SP_COMMENT, S_setB S_comment));
}

#if HAVE_GUILE
#define WITH_REVERSED_BOOLEAN_ARGS(name_reversed, name) \
static XEN name_reversed(XEN arg1, XEN arg2) \
{ \
  if (XEN_NOT_BOUND_P(arg1)) \
    return(name(XEN_TRUE, XEN_UNDEFINED)); \
  else \
    if (XEN_NOT_BOUND_P(arg2)) \
      return(name(arg1, XEN_UNDEFINED)); \
    else return(name(arg2, arg1)); \
}
#else
#define WITH_REVERSED_BOOLEAN_ARGS(name_reversed, name)
#endif

static XEN g_sync(XEN snd_n) 
{
  #define H_sync "(" S_sync " (snd #f)): snd's sync value (0 = no sync)"
  return(sound_get(snd_n, SP_SYNC, S_sync, NO_MIX));
}

static XEN g_set_sync(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_sync, "an integer");
  return(sound_set(snd_n, on, SP_SYNC, S_setB S_sync));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_sync_reversed, g_set_sync)


static XEN g_sound_properties(XEN snd_n) 
{
  #define H_sound_properties "(" S_sound_properties " snd): snd's property list"
  return(sound_get(snd_n, SP_PROPERTIES, S_sound_properties, NO_MIX));
}

static XEN g_set_sound_properties(XEN on, XEN snd_n) 
{
  /* XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_sound_properties, "a property list"); */
  return(sound_set(snd_n, on, SP_PROPERTIES, S_setB S_sound_properties));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_sound_properties_reversed, g_set_sound_properties)


static XEN g_channel_style(XEN snd) 
{
  snd_info *sp;
  if (XEN_NOT_BOUND_P(snd))
    return(C_TO_XEN_INT(channel_style(get_global_state())));
  ASSERT_JUST_SOUND(S_channel_style, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_channel_style, snd));
  return(C_TO_XEN_INT((int)(sp->channel_style)));
}

static XEN g_set_channel_style(XEN style, XEN snd) 
{
  snd_state *ss;
  snd_info *sp;
  channel_style_t new_style = CHANNELS_SEPARATE;
  #define H_channel_style "(" S_channel_style " (snd #f)): how multichannel sounds lay out the channels. \
Default is " S_channels_separate ", other values are " S_channels_combined " and " S_channels_superimposed ". \
As a global (if the 'snd' arg is omitted), it is the default setting for each sound's 'unite' button."

  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_channel_style, "an integer or boolean"); 
  new_style = (channel_style_t)XEN_TO_C_INT(style);
  if ((new_style < CHANNELS_SEPARATE) || (new_style > CHANNELS_SUPERIMPOSED))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_channel_style, 1, style, "~A, but must be " S_channels_separate ", " S_channels_combined ", or " S_channels_superimposed);
  if (XEN_NOT_BOUND_P(snd))
    {
      ss = get_global_state();
      set_channel_style(ss, new_style);
      return(C_TO_XEN_INT(channel_style(ss)));
    }
  ASSERT_JUST_SOUND(S_setB S_channel_style, snd, 2);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_setB S_channel_style, snd));
  set_sound_channel_style(sp, new_style);
  return(C_TO_XEN_INT((int)(sp->channel_style)));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_channel_style_reversed, g_set_channel_style)

static XEN g_read_only(XEN snd_n) 
{
  #define H_read_only "(" S_read_only " (snd #f)): whether snd is write-protected"
  return(sound_get(snd_n, SP_READ_ONLY, S_read_only, NO_MIX));
}

static XEN g_set_read_only(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_read_only, "a boolean");
  return(sound_set(snd_n, on, SP_READ_ONLY, S_setB S_read_only));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_read_only_reversed, g_set_read_only)

static XEN g_contrast_control_p(XEN snd_n) 
{
  #define H_contrast_control_p "(" S_contrast_control_p " (snd #f)): snd's control panel constrast button state"
  return(sound_get(snd_n, SP_CONTRASTING, S_contrast_control_p, NO_MIX));
}

static XEN g_set_contrast_control_p(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_contrast_control_p, "a boolean");
  return(sound_set(snd_n, on, SP_CONTRASTING, S_setB S_contrast_control_p));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_contrast_control_p_reversed, g_set_contrast_control_p)

static XEN g_expand_control_p(XEN snd_n) 
{
  #define H_expand_control_p "(" S_expand_control_p " (snd #f)): snd's control panel expand button state"
  return(sound_get(snd_n, SP_EXPANDING, S_expand_control_p, NO_MIX));
}

static XEN g_set_expand_control_p(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_expand_control_p, "a boolean");
  return(sound_set(snd_n, on, SP_EXPANDING, S_setB S_expand_control_p));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_expand_control_p_reversed, g_set_expand_control_p)

static XEN g_reverb_control_p(XEN snd_n) 
{
  #define H_reverb_control_p "(" S_reverb_control_p " (snd #f)): snd's control panel reverb button state"
  return(sound_get(snd_n, SP_REVERBING, S_reverb_control_p, NO_MIX));
}

static XEN g_set_reverb_control_p(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_reverb_control_p, "a boolean");
  return(sound_set(snd_n, on, SP_REVERBING, S_setB S_reverb_control_p));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_reverb_control_p_reversed, g_set_reverb_control_p)

static XEN g_filter_control_p(XEN snd_n) 
{
  #define H_filter_control_p "(" S_filter_control_p " (snd #f)): snd's control panel filter button state"
  return(sound_get(snd_n, SP_FILTERING, S_filter_control_p, NO_MIX));
}

static XEN g_set_filter_control_p(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_filter_control_p, "a boolean");
  return(sound_set(snd_n, on, SP_FILTERING, S_setB S_filter_control_p));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_filter_control_p_reversed, g_set_filter_control_p)

static XEN g_filter_control_in_dB(XEN snd_n) 
{
  #define H_filter_control_in_dB "(" S_filter_control_in_dB " (snd #f)): #t if snd's filter envelope is displayed in dB in control panel"
  return(sound_get(snd_n, SP_FILTER_DBING, S_filter_control_in_dB, NO_MIX));
}

static XEN g_set_filter_control_in_dB(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_filter_control_in_dB, "a boolean");
  return(sound_set(snd_n, on, SP_FILTER_DBING, S_setB S_filter_control_in_dB));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_filter_control_in_dB_reversed, g_set_filter_control_in_dB)

static XEN g_filter_control_coeffs(XEN snd_n) 
{
  #define H_filter_control_coeffs "(" S_filter_control_coeffs " (snd #f)): control panel filter coeffs"
  return(sound_get(snd_n, SP_FILTER_COEFFS, S_filter_control_coeffs, NO_MIX));
}

static XEN g_filter_control_order(XEN snd_n) 
{
  #define H_filter_control_order "(" S_filter_control_order " (snd #f)): filter order (in control panel)"
  return(sound_get(snd_n, SP_FILTER_ORDER, S_filter_control_order, NO_MIX));
}

static XEN g_set_filter_control_order(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, S_setB S_filter_control_order, "an integer"); 
  return(sound_set(snd_n, on, SP_FILTER_ORDER, S_setB S_filter_control_order));
}

#if HAVE_GUILE
#define WITH_REVERSED_ARGS(name_reversed, name) \
static XEN name_reversed(XEN arg1, XEN arg2) \
{ \
  if (XEN_NOT_BOUND_P(arg2)) \
    return(name(arg1, XEN_UNDEFINED)); \
  else return(name(arg2, arg1)); \
}
#else
#define WITH_REVERSED_ARGS(name_reversed, name)
#endif

WITH_REVERSED_ARGS(g_set_filter_control_order_reversed, g_set_filter_control_order)

static XEN g_cursor_follows_play(XEN snd_n) 
{
  #define H_cursor_follows_play "("  S_cursor_follows_play " (snd #f)): #t if cursor moves along in waveform display as sound is played (#f)"
  return(sound_get(snd_n, SP_CURSOR_FOLLOWS_PLAY, S_cursor_follows_play, NO_MIX));
}

static XEN g_set_cursor_follows_play(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_cursor_follows_play, "a boolean");
  return(sound_set(snd_n, on, SP_CURSOR_FOLLOWS_PLAY, S_setB S_cursor_follows_play));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_cursor_follows_play_reversed, g_set_cursor_follows_play)

static XEN g_show_controls(XEN snd_n) 
{
  #define H_show_controls "(" S_show_controls " (snd #f)): #t if snd's control panel is known to be open"
  return(sound_get(snd_n, SP_SHOW_CONTROLS, S_show_controls, NO_MIX));
}

static XEN g_set_show_controls(XEN on, XEN snd_n)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_show_controls, "a boolean");
  return(sound_set(snd_n, on, SP_SHOW_CONTROLS, S_setB S_show_controls));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_show_controls_reversed, g_set_show_controls)

static XEN g_save_controls(XEN snd_n) 
{
  #define H_save_controls "(" S_save_controls " (snd #f)): save the control panel settings for subsequent " S_restore_controls
  return(sound_get(snd_n, SP_SAVE_CONTROLS, S_save_controls, NO_MIX));
}

static XEN g_restore_controls(XEN snd_n) 
{
  #define H_restore_controls "(" S_restore_controls " (snd #f)): restore the previously saved control panel settings"
  return(sound_get(snd_n, SP_RESTORE_CONTROLS, S_restore_controls, NO_MIX));
}

static XEN g_reset_controls(XEN snd_n) 
{
  #define H_reset_controls "(" S_reset_controls " (snd #f)): reset (clear) the control panel settings"
  return(sound_get(snd_n, SP_RESET_CONTROLS, S_reset_controls, NO_MIX));
}

static XEN g_selected_channel(XEN snd_n) 
{
  #define H_selected_channel "(" S_selected_channel " (snd #f)): currently selected channel in snd (or #f if none)"
  return(sound_get(snd_n, SP_SELECTED_CHANNEL, S_selected_channel, NO_MIX));
}

static XEN g_set_selected_channel(XEN snd_n, XEN chn_n) 
{
  snd_info *sp;
  int chan;
  if (XEN_NOT_BOUND_P(chn_n))
    return(g_select_channel(snd_n));
  else
    {
      ASSERT_JUST_SOUND(S_setB S_selected_channel, snd_n, 1); 
      sp = get_sp(snd_n, NO_PLAYERS);
      if (sp == NULL) 
	return(snd_no_such_sound_error(S_setB S_selected_channel, snd_n));
      if (XEN_FALSE_P(chn_n))
	sp->selected_channel = NO_SELECTION;
      else
	{
	  chan = XEN_TO_C_INT_OR_ELSE(chn_n, 0);
	  if ((chan >= 0) && 
	      (chan < sp->nchans)) 
	    {
	      select_channel(sp, chan);
	      return(chn_n);
	    }
	  else return(snd_no_such_channel_error(S_setB S_selected_channel, snd_n, chn_n));
	}
    }
  return(XEN_FALSE);
}

static XEN g_file_name(XEN snd_n) 
{
  #define H_file_name "(" S_file_name " (snd #f)): snd's full filename"
  return(sound_get(snd_n, SP_FILE_NAME, S_file_name, MIX_OK));
}

static XEN g_short_file_name(XEN snd_n) 
{
  #define H_short_file_name "(" S_short_file_name " (snd #f)): short form of snd's file name (no directory)"
  return(sound_get(snd_n, SP_SHORT_FILE_NAME, S_short_file_name, MIX_OK));
}

static XEN g_close_sound(XEN snd_n) 
{
  #define H_close_sound "(" S_close_sound " (snd #f)): close snd"
  return(sound_get(snd_n, SP_CLOSE, S_close_sound, NO_MIX));
}

static XEN g_update_sound(XEN snd_n) 
{
  #define H_update_sound "(" S_update_sound " (snd #f)): update snd (re-read it from the disk after flushing pending edits)"
  return(sound_get(snd_n, SP_UPDATE, S_update_sound, NO_MIX));
}

/* should this throw an error if write-protected or whatever (not snd_error in this context)?  */
static XEN g_save_sound(XEN snd_n) 
{
  #define H_save_sound "(" S_save_sound " (snd #f)): save snd (update the on-disk data to match Snd's current version)"
  return(sound_get(snd_n, SP_SAVE, S_save_sound, NO_MIX));
}

static XEN g_revert_sound(XEN index)
{
  #define H_revert_sound "("  S_revert_sound " (snd #f)): revert snd to its unedited state (undo all)"
  snd_info *sp;
  int i;
  if (XEN_LIST_P(index))
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_3(C_TO_XEN_STRING(S_revert_sound),
			 index,
			 C_TO_XEN_STRING("can't revert an underlying mix edit except through the outer (mixed-into) sound")));
  ASSERT_SOUND(S_revert_sound, index, 1);
  sp = get_sp(index, NO_PLAYERS);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_revert_sound, index));
  for (i = 0; i < sp->nchans; i++) 
    {
      revert_edits(sp->chans[i], NULL); 
      update_graph(sp->chans[i]);
    }
  reflect_file_revert_in_label(sp);
  reflect_file_revert_in_menu(sp->state);
  return(XEN_TRUE);
}

static XEN g_selected_sound(void)
{
  #define H_selected_sound "(" S_selected_sound "): index of currently selected sound (or #f if none)"
  snd_state *ss;
  ss = get_global_state();
  if ((ss->selected_sound != NO_SELECTION) && 
      (snd_ok(ss->sounds[ss->selected_sound])))
    return(C_TO_SMALL_XEN_INT(ss->selected_sound));
  return(XEN_FALSE); /* was -1 before 26-Mar-02 */
}

static XEN g_open_sound(XEN filename)
{ /* returns index of new sound if successful */
  #define H_open_sound "(" S_open_sound " filename): \
open filename (as if opened from File:Open menu option), and return the new sound's index"
  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_open_sound, "a string");
  fname = mus_expand_filename(XEN_TO_C_STRING(filename));
  if (!(mus_file_probe(fname)))
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_open_sound, filename));
    }
  ss->catch_message = NULL;
  sp = snd_open_file(fname, ss, false);
  if (fname) FREE(fname);
  if (sp) 
    return(C_TO_XEN_INT(sp->index));
  /* sp NULL is not an error (open-hook func returned #t) */
  return(XEN_FALSE);
}

static XEN g_open_raw_sound(XEN filename, XEN chans, XEN srate, XEN format)
{
  #define H_open_raw_sound "(" S_open_raw_sound " filename chans srate format): \
open filename assuming the data matches the attributes indicated unless the file actually has a header"

  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  int os, oc, ofr;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_open_raw_sound, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_2, S_open_raw_sound, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_3, S_open_raw_sound, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ARG_4, S_open_raw_sound, "an integer");
  fname = mus_expand_filename(XEN_TO_C_STRING(filename));
  if (!(mus_file_probe(fname)))
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_open_raw_sound, filename));
    }
  ss = get_global_state();
  mus_header_raw_defaults(&os, &oc, &ofr);
  mus_header_set_raw_defaults(XEN_TO_C_INT_OR_ELSE(srate, os),
			      XEN_TO_C_INT_OR_ELSE(chans, oc),
			      XEN_TO_C_INT_OR_ELSE(format, ofr));
  ss->reloading_updated_file = -1;
  ss->catch_message = NULL;
  sp = snd_open_file(fname, ss, false);
  ss->reloading_updated_file = 0;
  /* snd_open_file -> snd_open_file_1 -> add_sound_window -> make_file_info -> raw_data_dialog_to_file_info */
  /*   so here if hooked, we'd need to save the current hook, make it return the current args, open, then restore */
  if (fname) FREE(fname);
  if (sp) 
    return(C_TO_XEN_INT(sp->index));
  return(XEN_FALSE);
}

static XEN g_view_sound(XEN filename)
{
  #define H_view_sound "(" S_view_sound " filename): open a file in read-only mode. \
You can subsequently make it writable by (set! (read-only) #f)."
  char *fname = NULL;
  snd_info *sp = NULL;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_view_sound, "a string");
  fname = mus_expand_filename(XEN_TO_C_STRING(filename));
  if (!(mus_file_probe(fname)))
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_view_sound, filename));
    }
  ss->catch_message = NULL;
  sp = snd_open_file(fname, ss, true);
  FREE(fname);
  if (sp) 
    return(C_TO_XEN_INT(sp->index));
  return(XEN_FALSE);
}

static XEN g_save_sound_as(XEN newfile, XEN index, XEN type, XEN format, XEN srate, XEN channel, XEN edpos)
{
  #define H_save_sound_as "("  S_save_sound_as " filename (snd #f) (header-type #f) (data-format #f) (srate #f) (channel #f) (edpos #f)): \
save snd in filename using the indicated attributes.  If channel is specified, only that channel is saved (extracted). \
Any argument can be #f which causes its value to be taken from the sound being saved.\n\
  (save-sound-as \"test.snd\" index mus-next mus-bshort)"

  snd_info *sp;
  chan_info *cp;
  file_info *hdr;
  snd_state *ss;
  int ht, df, sr, chan, err;
  char *fname = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(newfile), newfile, XEN_ARG_1, S_save_sound_as, "a string");
  ASSERT_SOUND(S_save_sound_as, index, 2);
  sp = get_sp(index, NO_PLAYERS);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_save_sound_as, index));
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(type), type, XEN_ARG_3, S_save_sound_as, "an integer (a header type id)");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(format), format, XEN_ARG_4, S_save_sound_as, "an integer (a data format id)");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(srate), srate, XEN_ARG_5, S_save_sound_as, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(channel), channel, XEN_ARG_6, S_save_sound_as, "an integer");
  fname = mus_expand_filename(XEN_TO_C_STRING(newfile));
  hdr = sp->hdr;
  ht = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(type, hdr->type, S_save_sound_as);
  sr = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(srate, hdr->srate, S_save_sound_as);
  sp->state->catch_message = NULL;
  if (XEN_INTEGER_P(format)) 
    df = XEN_TO_C_INT(format);
  else    
    {
      if (mus_header_writable(ht, hdr->format))
	df = hdr->format;
      else df = MUS_OUT_FORMAT;
    }
  if (!(mus_header_writable(ht, -2)))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_sound_as),
			 C_TO_XEN_STRING(_("can't write this header type:")),
			 C_TO_XEN_STRING(mus_header_type_name(ht))));
  if (!(mus_header_writable(ht, df)))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_4(C_TO_XEN_STRING(S_save_sound_as),
			 C_TO_XEN_STRING(_("can't write this combination of header type and data format:")),
			 C_TO_XEN_STRING(mus_header_type_name(ht)),
			 C_TO_XEN_STRING(mus_data_format_name(df))));
  if (XEN_INTEGER_P(channel))
    {
      chan = XEN_TO_C_INT(channel);
      if ((chan >= sp->nchans) || 
	  (chan < 0))
	{
	  if (fname) FREE(fname);
	  return(snd_no_such_channel_error(S_save_sound_as, index, channel));
	}
      else 
	{
	  cp = sp->chans[chan];
	  err = save_channel_edits(cp, fname, edpos, S_save_sound_as, 7);
	}
    }
  else 
    {
      char *outcom;
      outcom = output_comment(hdr);
      err = save_edits_without_display(sp, fname, ht, df, sr, outcom, edpos, S_save_sound_as, 7);
      if (outcom) FREE(outcom);
    }
  if (err != MUS_NO_ERROR)
    {
      ss = sp->state;
      if (ss->catch_message)
	XEN_ERROR(CANNOT_SAVE,
		  XEN_LIST_2(C_TO_XEN_STRING(S_save_sound_as),
			     C_TO_XEN_STRING(sp->state->catch_message)));
      else
	XEN_ERROR(CANNOT_SAVE,
		  XEN_LIST_3(C_TO_XEN_STRING(S_save_sound_as),
			     C_TO_XEN_STRING(fname),
			     C_TO_XEN_STRING(strerror(errno))));
    }
  if (fname) FREE(fname);
  return(newfile);
}

static XEN g_new_sound(XEN name, XEN type, XEN format, XEN srate, XEN chans, XEN comment, XEN initial_length) 
{
  #define H_new_sound "(" S_new_sound " (name #f) (type #f) (format #f) (srate #f) (chans #f) (comment #f) (initial-length #f)): \
create a new sound file with the indicated attributes; if any are omitted, the corresponding default-output variable is used. \
The 'initial-length' argument sets the number of samples (zeros) in the newly created sound. \n\
  (new-sound \"test.snd\" mus-next mus-bshort 22050 1 \"no comment\" 22050)"

  snd_info *sp = NULL; 
  int ht, df, sr, ch, err;
  snd_state *ss;
  int chan;
  off_t size, len = 1;
  unsigned char* buf;
  char *str = NULL, *com = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(name), name, XEN_ARG_1, S_new_sound, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(type), type, XEN_ARG_2, S_new_sound, "an integer (a header type id)");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(format), format, XEN_ARG_3, S_new_sound, "an integer (a data format id)");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(srate), srate, XEN_ARG_4, S_new_sound, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chans), chans, XEN_ARG_5, S_new_sound, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(comment), comment, XEN_ARG_6, S_new_sound, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(initial_length), initial_length, XEN_ARG_7, S_new_sound, "an integer");
  ss = get_global_state();
  if (XEN_STRING_P(name))
    str = mus_expand_filename(XEN_TO_C_STRING(name));
  else str = snd_tempnam(ss);
  if (snd_overwrite_ok(ss, str))
    {
      mus_sound_forget(str);
      ht = XEN_TO_C_INT_OR_ELSE(type, default_output_type(ss));
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  df = XEN_TO_C_INT_OR_ELSE(format, default_output_format(ss));
	  if (MUS_DATA_FORMAT_OK(df))
	    {
	      if (mus_header_writable(ht, df))
		{
		  sr = XEN_TO_C_INT_OR_ELSE(srate, default_output_srate(ss));
		  ch = XEN_TO_C_INT_OR_ELSE(chans, default_output_chans(ss));
		  if (ch <= 0)
		    {
		      if (str) FREE(str);
		      XEN_OUT_OF_RANGE_ERROR(S_new_sound, 5, chans, "chans ~A <= 0?");
		    }
		  if (XEN_STRING_P(comment))
		    com = XEN_TO_C_STRING(comment);
		  ss->catch_message = NULL;
		  if (XEN_INTEGER_P(initial_length)) len = XEN_TO_C_OFF_T(initial_length);
		  err = snd_write_header(ss, str, ht, sr, ch, 0, len * ch, df, com, snd_strlen(com), NULL);
		  if (err == -1)
		    {
		      if (str) FREE(str);
		      if (ss->catch_message)
			mus_misc_error(S_new_sound, ss->catch_message, name);
		      else mus_misc_error(S_new_sound, strerror(errno), name);
		    }
		  chan = snd_reopen_write(ss, str);
		  lseek(chan, mus_header_data_location(), SEEK_SET);
		  size = ch * mus_samples_to_bytes(df, len);
		  buf = (unsigned char *)CALLOC(size, sizeof(unsigned char));
		  write(chan, buf, size);
		  snd_close(chan, str);
		  FREE(buf);
		  sp = snd_open_file(str, ss, false);
		}
	      else 
		{
		  if (str) FREE(str);
		  mus_misc_error(S_new_sound, _("can't write this combination of data format and header type"), XEN_LIST_2(type, format));
		}
	    }
	  else 
	    {
	      if (str) FREE(str);
	      XEN_OUT_OF_RANGE_ERROR(S_new_sound, 3, format, "~A: invalid data format");
	    }
	}
      else
	{
	  if (str) FREE(str);
	  XEN_OUT_OF_RANGE_ERROR(S_new_sound, 2, type, "~A: invalid header type");
	}
    }
  if (str) FREE(str);
  if (sp) return(C_TO_XEN_INT(sp->index));
  return(XEN_FALSE);
}

static XEN g_speed_control_style(XEN snd)
{
  #define H_speed_control_style "(" S_speed_control_style " (snd #t)): speed control panel interpretation choice (" S_speed_control_as_float ")"
  if (XEN_BOUND_P(snd))
    return(sound_get(snd, SP_SPEED_STYLE, S_speed_control_style, NO_MIX));
  return(C_TO_XEN_INT((int)speed_control_style(get_global_state())));
}

static XEN g_set_speed_control_style(XEN speed, XEN snd) 
{
  snd_state *ss;
  speed_style_t spd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speed), speed, XEN_ARG_1, S_setB S_speed_control_style, "an integer"); 
  spd = (speed_style_t)XEN_TO_C_INT(speed);
  if ((spd < SPEED_CONTROL_AS_FLOAT) || (spd > SPEED_CONTROL_AS_SEMITONE))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_speed_control_style, 
			   1, speed, 
			   "~A, but must be " S_speed_control_as_float ", " S_speed_control_as_ratio ", or " S_speed_control_as_semitone);
  if (XEN_BOUND_P(snd))
    return(sound_set(snd, speed, SP_SPEED_STYLE, S_setB S_speed_control_style));
  else
    {
      ss = get_global_state();
      activate_speed_in_menu(ss, spd);
      return(C_TO_XEN_INT((int)speed_control_style(ss)));
    }
}

WITH_REVERSED_ARGS(g_set_speed_control_style_reversed, g_set_speed_control_style)

static XEN g_speed_control_tones(XEN snd)
{
  #define H_speed_control_tones "(" S_speed_control_tones " (snd #t)): if " S_speed_control_style " is " S_speed_control_as_semitone ", this chooses the octave divisions (12)"
  if (XEN_BOUND_P(snd))
    return(sound_get(snd, SP_SPEED_TONES, S_speed_control_tones, NO_MIX));
  return(C_TO_XEN_INT(speed_control_tones(get_global_state())));
}

static XEN g_set_speed_control_tones(XEN val, XEN snd)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_speed_control_tones, "a number"); 
  if (XEN_BOUND_P(snd))
    return(sound_set(snd, val, SP_SPEED_TONES, S_setB S_speed_control_tones));
  else
    {
      ss = get_global_state();
      set_speed_tones(ss, XEN_TO_C_INT_OR_ELSE(val, 0));
      return(C_TO_XEN_INT(speed_control_tones(ss)));
    }
}

WITH_REVERSED_ARGS(g_set_speed_control_tones_reversed, g_set_speed_control_tones)

static XEN g_amp_control(XEN snd_n, XEN chn_n) 
{
  #define H_amp_control "(" S_amp_control " (snd #f) (chn #f)): current amp slider setting"
  chan_info *cp;
  if (XEN_BOUND_P(chn_n))
    {
      ASSERT_JUST_CHANNEL(S_amp_control, snd_n, chn_n, 1);
      cp = get_cp(snd_n, chn_n, S_amp_control);
      if (cp->amp_control)
	return(C_TO_XEN_DOUBLE(cp->amp_control[0]));
    }
  return(sound_get(snd_n, SP_AMP, S_amp_control, NO_MIX));
}

static XEN g_set_amp_control(XEN on, XEN snd_n, XEN chn_n) 
{
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_amp_control, "a number"); 
  if (XEN_BOUND_P(chn_n))
    {
      ASSERT_JUST_CHANNEL(S_amp_control, snd_n, chn_n, 2);
      cp = get_cp(snd_n, chn_n, S_amp_control);
      if (cp->amp_control == NULL)
	cp->amp_control = (Float *)CALLOC(1, sizeof(Float));
      cp->amp_control[0] = (Float)XEN_TO_C_DOUBLE_WITH_CALLER(on, S_setB S_amp_control);
      return(on);
    }
  return(sound_set(snd_n, on, SP_AMP, S_setB S_amp_control));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_amp_control_reversed, g_set_amp_control)

static XEN g_contrast_control(XEN snd_n) 
{
  #define H_contrast_control "(" S_contrast_control " (snd #f)): current contrast slider setting"
  return(sound_get(snd_n, SP_CONTRAST, S_contrast_control, NO_MIX));
}

static XEN g_set_contrast_control(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_contrast_control, "a number"); 
  return(sound_set(snd_n, on, SP_CONTRAST, S_setB S_contrast_control));
}

WITH_REVERSED_ARGS(g_set_contrast_control_reversed, g_set_contrast_control)

static XEN g_contrast_control_amp(XEN snd_n) 
{
  #define H_contrast_control_amp "(" S_contrast_control_amp " (snd #f)): snd's contrast amp\n\
   (scaler on data before contrast operation in control panel, 1.0)"

  return(sound_get(snd_n, SP_CONTRAST_AMP, S_contrast_control_amp, NO_MIX));
}

static XEN g_set_contrast_control_amp(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_contrast_control_amp, "a number");
  return(sound_set(snd_n, on, SP_CONTRAST_AMP, S_setB S_contrast_control_amp));
}

WITH_REVERSED_ARGS(g_set_contrast_control_amp_reversed, g_set_contrast_control_amp)

static XEN g_expand_control(XEN snd_n) 
{
  #define H_expand_control "(" S_expand_control " (snd #f)): current expand slider setting"
  return(sound_get(snd_n, SP_EXPAND, S_expand_control, NO_MIX));
}

static XEN g_set_expand_control(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control, "a number"); 
  return(sound_set(snd_n, on, SP_EXPAND, S_setB S_expand_control));
}

WITH_REVERSED_ARGS(g_set_expand_control_reversed, g_set_expand_control)

static XEN g_expand_control_length(XEN snd_n) 
{
  #define H_expand_control_length "(" S_expand_control_length " (snd #f)): current expansion segment length in seconds (.15)"
  return(sound_get(snd_n, SP_EXPAND_LENGTH, S_expand_control_length, NO_MIX));
}

static XEN g_set_expand_control_length(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control_length, "a number"); 
  return(sound_set(snd_n, on, SP_EXPAND_LENGTH, S_setB S_expand_control_length));
}

WITH_REVERSED_ARGS(g_set_expand_control_length_reversed, g_set_expand_control_length)

static XEN g_expand_control_ramp(XEN snd_n) 
{
  #define H_expand_control_ramp "(" S_expand_control_ramp " (snd #f)): current expansion ramp time (.4)"
  return(sound_get(snd_n, SP_EXPAND_RAMP, S_expand_control_ramp, NO_MIX));
}

static XEN g_set_expand_control_ramp(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control_ramp, "a number");
  return(sound_set(snd_n, on, SP_EXPAND_RAMP, S_setB S_expand_control_ramp));
}

WITH_REVERSED_ARGS(g_set_expand_control_ramp_reversed, g_set_expand_control_ramp)

static XEN g_expand_control_hop(XEN snd_n) 
{
  #define H_expand_control_hop "(" S_expand_control_hop " (snd #f)): current expansion output grain spacing in seconds (0.05)"
  return(sound_get(snd_n, SP_EXPAND_HOP, S_expand_control_hop, NO_MIX));
}

static XEN g_set_expand_control_hop(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control_hop, "a number"); 
  return(sound_set(snd_n, on, SP_EXPAND_HOP, S_setB S_expand_control_hop));
}

WITH_REVERSED_ARGS(g_set_expand_control_hop_reversed, g_set_expand_control_hop)

static XEN g_speed_control(XEN snd_n) 
{
  #define H_speed_control "(" S_speed_control " (snd #f)): current speed (srate) slider setting"
  return(sound_get(snd_n, SP_SPEED, S_speed_control, NO_MIX));
}

static XEN g_set_speed_control(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_speed_control, "a number"); 
  return(sound_set(snd_n, on, SP_SPEED, S_setB S_speed_control));
}

WITH_REVERSED_ARGS(g_set_speed_control_reversed, g_set_speed_control)

static XEN g_reverb_control_length(XEN snd_n) 
{
  #define H_reverb_control_length "(" S_reverb_control_length " (snd #f)): reverb decay length scaler"
  return(sound_get(snd_n, SP_REVERB_LENGTH, S_reverb_control_length, NO_MIX));
}

static XEN g_set_reverb_control_length(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_reverb_control_length, "a number"); 
  return(sound_set(snd_n, on, SP_REVERB_LENGTH, S_setB S_reverb_control_length));
}

WITH_REVERSED_ARGS(g_set_reverb_control_length_reversed, g_set_reverb_control_length)

static XEN g_reverb_control_feedback(XEN snd_n) 
{
  #define H_reverb_control_feedback "(" S_reverb_control_feedback " (snd #f)): reverb feedback scaler"
  return(sound_get(snd_n, SP_REVERB_FEEDBACK, S_reverb_control_feedback, NO_MIX));
}

static XEN g_set_reverb_control_feedback(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_reverb_control_feedback, "a number"); 
  return(sound_set(snd_n, on, SP_REVERB_FEEDBACK, S_setB S_reverb_control_feedback));
}

WITH_REVERSED_ARGS(g_set_reverb_control_feedback_reversed, g_set_reverb_control_feedback)

static XEN g_reverb_control_scale(XEN snd_n) 
{
  #define H_reverb_control_scale "(" S_reverb_control_scale " (snd #f)): reverb scaler (the amount of reverb)"
  return(sound_get(snd_n, SP_REVERB_SCALE, S_reverb_control_scale, NO_MIX));
}

static XEN g_set_reverb_control_scale(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_reverb_control_scale, "a number"); 
  return(sound_set(snd_n, on, SP_REVERB_SCALE, S_setB S_reverb_control_scale));
}

WITH_REVERSED_ARGS(g_set_reverb_control_scale_reversed, g_set_reverb_control_scale)

static XEN g_reverb_control_lowpass(XEN snd_n) 
{
  #define H_reverb_control_lowpass "(" S_reverb_control_lowpass " (snd #f)): reverb lowpass filter coefficient"
  return(sound_get(snd_n, SP_REVERB_LOW_PASS, S_reverb_control_lowpass, NO_MIX));
}

static XEN g_set_reverb_control_lowpass(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_reverb_control_lowpass, "a number"); 
  return(sound_set(snd_n, on, SP_REVERB_LOW_PASS, S_setB S_reverb_control_lowpass));
}

WITH_REVERSED_ARGS(g_set_reverb_control_lowpass_reversed, g_set_reverb_control_lowpass)

static XEN g_reverb_control_decay(XEN snd)
{
  #define H_reverb_control_decay "(" S_reverb_control_decay " (snd #t)): 'Apply' button reverb decay time (1.0 seconds)"
  if (XEN_BOUND_P(snd))
    return(sound_get(snd, SP_REVERB_DECAY, S_reverb_control_decay, NO_MIX));
  return(C_TO_XEN_DOUBLE(reverb_control_decay(get_global_state())));
}

static XEN g_set_reverb_control_decay(XEN val, XEN snd)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_reverb_control_decay, "a number"); 
  if (XEN_BOUND_P(snd))
    return(sound_set(snd, val, SP_REVERB_DECAY, S_setB S_reverb_control_decay));
  else
    {
      ss = get_global_state();
      set_reverb_decay(ss, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(reverb_control_decay(ss)));
    }
}

WITH_REVERSED_ARGS(g_set_reverb_control_decay_reversed, g_set_reverb_control_decay)

static XEN g_set_filter_control_env(XEN edata, XEN snd_n)
{
  snd_info *sp;
  env *e;
  int i;
  ASSERT_JUST_SOUND(S_setB S_filter_control_env, snd_n, 2);
  sp = get_sp(snd_n, PLAYERS_OK);
  if ((sp == NULL) || (sp->inuse == SOUND_WRAPPER))
    return(snd_no_such_sound_error(S_setB S_filter_control_env, snd_n));
  if (sp->filter_control_env) sp->filter_control_env = free_env(sp->filter_control_env);  /* set to null in case get_env throws error */
  e = get_env(edata, S_setB S_filter_control_env);
  if (e)
    {
      for (i = 0; i < e->pts; i++)
	if ((e->data[i * 2 + 1] > 1.0) ||
	    (e->data[i * 2 + 1] < 0.0))
	  {
	    free_env(e);
	    XEN_OUT_OF_RANGE_ERROR(S_setB S_filter_control_env, 1, edata, "y values ~A < 0.0 or > 1.0");
	  }
      sp->filter_control_env = e;
      filter_env_changed(sp, sp->filter_control_env);
    }
  return(edata);
}

static XEN g_filter_control_env(XEN snd_n)
{
  #define H_filter_control_env "(" S_filter_control_env " (snd #f)): snd's filter envelope (in the control panel)"
  snd_info *sp = NULL;
  ASSERT_JUST_SOUND(S_filter_control_env, snd_n, 1);
  sp = get_sp(snd_n, PLAYERS_OK);
  if ((sp == NULL) || (sp->inuse == SOUND_WRAPPER))
    return(snd_no_such_sound_error(S_filter_control_env, snd_n));
  return(env_to_xen(sp->filter_control_env)); 
}

WITH_REVERSED_ARGS(g_set_filter_control_env_reversed, g_set_filter_control_env)

static XEN g_apply_controls(XEN snd, XEN choice, XEN beg, XEN dur)
{
  #define H_apply_controls "(" S_apply_controls " (snd #f) (choice 0) (beg 0) (dur len)): \
equivalent to clicking the control panel 'Apply' button.\
The 'choices' are 0 (apply to sound), 1 (apply to channel), and 2 (apply to selection).  If 'beg' is given, the apply starts there."

  snd_info *sp;
  snd_state *ss;
  apply_state *ap;
  snd_apply_t cur_choice;
  ASSERT_JUST_SOUND(S_apply_controls, snd, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(choice), choice, XEN_ARG_2, S_apply_controls, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(beg), beg, XEN_ARG_3, S_apply_controls, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(dur), dur, XEN_ARG_4, S_apply_controls, "an integer");
  sp = get_sp(snd, PLAYERS_OK);
  if (sp)
    {
      ss = sp->state;
      if (XEN_OFF_T_P(beg)) apply_beg = XEN_TO_C_OFF_T(beg); else apply_beg = 0;
      if (XEN_OFF_T_P(dur)) apply_dur = XEN_TO_C_OFF_T(dur); else apply_dur = 0;
      cur_choice = (snd_apply_t)XEN_TO_C_INT_OR_ELSE(choice, APPLY_TO_SOUND);
      if ((cur_choice < APPLY_TO_SOUND) || (cur_choice > APPLY_TO_SELECTION))
	XEN_OUT_OF_RANGE_ERROR(S_apply_controls, 2, choice, "~A, but must be 0=sound, 1=channel, or 2=selection");
      ss->apply_choice = cur_choice;
      sp->applying = true;
      ap = (apply_state *)make_apply_state((void *)sp);
      if (ap)
	while (apply_controls((Indicium)ap) == BACKGROUND_CONTINUE);
      return(snd);
    }
  return(snd_no_such_sound_error(S_apply_controls, snd));
}

static XEN g_peak_env_info(XEN snd, XEN chn, XEN pos)
{
  #define H_peak_env_info "(" S_peak_env_info " (snd #f) (chn #f) (edpos #f)): \
return some of the overall amplitude envelope data for the given channel \
at the given edit list position.  The data currently returned are whether \
the envelopes are complete (they are the result of a background process), and the min and max data values."

  chan_info *cp;
  env_info *ep;
  int edpos;
  chan_context *cgx;
  ASSERT_JUST_CHANNEL(S_peak_env_info, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(pos), pos, XEN_ARG_3, S_peak_env_info, "an integer");
  cp = get_cp(snd, chn, S_peak_env_info);
  cgx = cp->cgx;
  if ((!cgx) || (!(cp->amp_envs))) 
    return(XEN_EMPTY_LIST);
  edpos = XEN_TO_C_INT_OR_ELSE(pos, cp->edit_ctr);
  if ((edpos < -1) || (edpos >= cp->edit_size) || (cp->edits[edpos] == NULL))
    XEN_ERROR(NO_SUCH_EDIT,
	      XEN_LIST_2(C_TO_XEN_STRING(S_peak_env_info),
			 C_TO_XEN_INT(pos)));
  if (edpos == -1) edpos = cp->edit_ctr;
  ep = cp->amp_envs[edpos];
  if (ep)
    return(XEN_LIST_3(C_TO_XEN_BOOLEAN(ep->completed),
		      C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(ep->fmin)),
		      C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(ep->fmax))));
  /* don't throw an error here since the env may be in progress */
  return(XEN_EMPTY_LIST);
}

static int pack_mus_sample_type(void)
{
  /* put mus_sample_t description in peak-env info file (in case user opens it from incompatible machine) */
  int val = MUS_SAMPLE_BITS;
#if MUS_LITTLE_ENDIAN
  val |= (1 << 8);
#endif
#if SNDLIB_USE_FLOATS
  val |= (1 << 9);
#endif
  val |= (sizeof(mus_sample_t) << 10);
  return(val);
}

static int mus_sample_type_ok(int val)
{
  return((val == 0) ||                            /* for backwards compatibility */
	 (val == pack_mus_sample_type()));
}

static XEN g_write_peak_env_info_file(XEN snd, XEN chn, XEN name)
{
  #define H_write_peak_env_info_file "(" S_write_peak_env_info_file " snd chn name): save current peak-env info in file"
  chan_info *cp;
  char *fullname = NULL;
  env_info *ep;
  int fd;
  XEN errstr;
  int ibuf[5];
  mus_sample_t mbuf[2];
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_2, S_write_peak_env_info_file, "a string");
  ASSERT_JUST_CHANNEL(S_write_peak_env_info_file, snd, chn, 1);
  cp = get_cp(snd, chn, S_write_peak_env_info_file);
  if ((cp->amp_envs == NULL) || (cp->amp_envs[0] == NULL))
    XEN_ERROR(NO_SUCH_ENVELOPE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_write_peak_env_info_file),
			 snd,
			 chn));
  fullname = mus_expand_filename(XEN_TO_C_STRING(name));
  fd = mus_file_create(fullname);
  if (fd == -1)
    {
      errstr = C_TO_XEN_STRING(fullname);
      if (fullname) FREE(fullname);
      XEN_ERROR(CANNOT_SAVE,
		XEN_LIST_3(C_TO_XEN_STRING(S_write_peak_env_info_file),
			   errstr,
			   C_TO_XEN_STRING(strerror(errno))));
    }
  if (fullname) FREE(fullname);
  ep = cp->amp_envs[0];
  ibuf[0] = ((ep->completed) ? 1 : 0) | (pack_mus_sample_type() << 16);
  ibuf[1] = ep->amp_env_size;
  ibuf[2] = ep->samps_per_bin;
  ibuf[3] = ep->bin;
  ibuf[4] = ep->top_bin;
  mbuf[0] = ep->fmin;
  mbuf[1] = ep->fmax;
  write(fd, (char *)ibuf, (5 * sizeof(int)));
  write(fd, (char *)mbuf, (2 * sizeof(mus_sample_t)));
  write(fd, (char *)(ep->data_min), (ep->amp_env_size * sizeof(mus_sample_t)));
  write(fd, (char *)(ep->data_max), (ep->amp_env_size * sizeof(mus_sample_t)));
  snd_close(fd, fullname);
  return(name);
}

enum {PEAK_ENV_NO_ERROR, PEAK_ENV_BAD_HEADER, PEAK_ENV_BAD_FORMAT, PEAK_ENV_BAD_SIZE, PEAK_ENV_NO_FILE, PEAK_ENV_NO_DATA};
static char *peak_env_error[6] = {"no error", "bad header", "bad format", "bad size", "no file", "no data in file"};

static env_info *get_peak_env_info(char *fullname, int *error)
{
  env_info *ep;
  int fd, bytes, hdr = 0;
  int ibuf[5];
  mus_sample_t mbuf[2];
  fd = mus_file_open_read(fullname);
  if (fd == -1) 
    {
      (*error) = PEAK_ENV_NO_FILE;
      return(NULL);
    }
  bytes = read(fd, (char *)ibuf, (5 * sizeof(int)));
  if (bytes != (5 * sizeof(int)))
    {
      snd_close(fd, fullname);
      (*error) = PEAK_ENV_NO_DATA;
      return(NULL);
    }
  hdr = ibuf[0];
  (*error) = PEAK_ENV_NO_ERROR;
  if (((hdr & 0xff) != 0) && ((hdr & 0xff) != 1)) 
    (*error) = PEAK_ENV_BAD_HEADER;
  else
    {
      if (!(mus_sample_type_ok(hdr >> 16)))
	(*error) = PEAK_ENV_BAD_FORMAT;
      else
	{
	  if ((ibuf[1] <= 0) || (!(POWER_OF_2_P(ibuf[1]))))
	    (*error) = PEAK_ENV_BAD_SIZE;
	  else
	    {
	      if ((ibuf[2] <= 0) || (ibuf[4] > ibuf[1]))
		(*error) = PEAK_ENV_BAD_HEADER;
	    }
	}
    }
  if ((*error) != PEAK_ENV_NO_ERROR) 
    {
      snd_close(fd, fullname);
      return(NULL);
    }
  ep = (env_info *)CALLOC(1, sizeof(env_info));
  ep->completed = (bool)(hdr & 0xff);
  ep->amp_env_size = ibuf[1];
  ep->samps_per_bin = ibuf[2];
  ep->bin = ibuf[3];
  ep->top_bin = ibuf[4];
  read(fd, (char *)mbuf, (2 * sizeof(mus_sample_t)));
  ep->fmin = mbuf[0];
  ep->fmax = mbuf[1];
  ep->data_min = (mus_sample_t *)MALLOC(ep->amp_env_size * sizeof(mus_sample_t));
  ep->data_max = (mus_sample_t *)MALLOC(ep->amp_env_size * sizeof(mus_sample_t));
  read(fd, (char *)(ep->data_min), (ep->amp_env_size * sizeof(mus_sample_t)));
  read(fd, (char *)(ep->data_max), (ep->amp_env_size * sizeof(mus_sample_t)));
  snd_close(fd, fullname);
  return(ep);
}

static XEN g_read_peak_env_info_file(XEN snd, XEN chn, XEN name)
{
  #define H_read_peak_env_info_file "(" S_read_peak_env_info_file " snd chn name): read stored peak-env info from file"
  /* has to happen in initial_graph_hook to precede add_amp_env */
  chan_info *cp;
  int err = 0;
  char *fullname;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_3, S_read_peak_env_info_file, "a string");
  ASSERT_JUST_CHANNEL(S_read_peak_env_info_file, snd, chn, 1);
  cp = get_cp(snd, chn, S_read_peak_env_info_file);
  fullname = mus_expand_filename(XEN_TO_C_STRING(name));
  cp->amp_envs[0] = get_peak_env_info(fullname, &err);
  if (fullname) FREE(fullname);
  if (cp->amp_envs[0] == NULL)
    mus_misc_error(S_read_peak_env_info_file, peak_env_error[err], name);
  /* assume cp->amp_envs already exists (needs change to snd-chn) */
  return(name);
}

static XEN g_env_info_to_vcts(env_info *ep, int len)
{
  /* changed 5-Jan-03 to return vcts (Guile vector printer is stupid) */
  XEN res;
  int i, j, lim;
  vct *vmax, *vmin;
  Float incr, x;
  mus_sample_t cmax, cmin;
  int loc;
  if (ep->amp_env_size < len) lim = ep->amp_env_size; else lim = len;
  res = XEN_LIST_2(make_vct(lim, (Float *)CALLOC(lim, sizeof(Float))),
		   make_vct(lim, (Float *)CALLOC(lim, sizeof(Float))));
  loc = snd_protect(res);
  vmin = get_vct(XEN_CAR(res));
  vmax = get_vct(XEN_CADR(res));
  if (ep->amp_env_size == lim)
    {
      for (i = 0; i < lim; i++)
	{
	  vmin->data[i] = MUS_SAMPLE_TO_DOUBLE(ep->data_min[i]);
	  vmax->data[i] = MUS_SAMPLE_TO_DOUBLE(ep->data_max[i]);
	}
    }
  else
    {
      incr = (Float)(ep->amp_env_size - 1) / (Float)len; /* make extra room on left */
      cmax = ep->fmin;
      cmin = ep->fmax;
      vmin->data[0] = MUS_SAMPLE_TO_DOUBLE(ep->data_min[0]);
      vmax->data[0] = MUS_SAMPLE_TO_DOUBLE(ep->data_max[0]);
      for (i = 1, j = 1, x = 0.0; i < ep->amp_env_size; i++)
	{
	  if (ep->data_max[i] > cmax) cmax = ep->data_max[i];
	  if (ep->data_min[i] < cmin) cmin = ep->data_min[i];
	  x += 1.0;
	  if (x >= incr)
	    {
	      vmin->data[j] = MUS_SAMPLE_TO_DOUBLE(cmin);
	      vmax->data[j++] = MUS_SAMPLE_TO_DOUBLE(cmax);
	      x -= incr;
	      cmax = ep->fmin;
	      cmin = ep->fmax;
	      if (j == lim) break;
	    }
	}
    }
  snd_unprotect_at(loc);
  return(res);
}

typedef struct {
  chan_info *cp;
  env_state *es;
  int len;
  XEN filename;
  XEN func;
} env_tick;

static Cessate tick_it(Indicium pet)
{
  bool val;
  int loc;
  env_state *es;
  chan_info *cp;
  XEN peak;
  env_tick *et = (env_tick *)pet;
  es = et->es;
  cp = et->cp;
  val = tick_amp_env(cp, es);
  if (val)
    {
      if (es->sf) free_snd_fd(es->sf);
      FREE(es);
      peak = g_env_info_to_vcts(cp->amp_envs[0], et->len);
      loc = snd_protect(peak);
      XEN_CALL_3(et->func,
		 et->filename,
		 C_TO_XEN_INT(cp->chan),
		 peak,
		 "amp env tick");
      snd_unprotect(et->func);
      snd_unprotect_at(loc);
      completely_free_snd_info(cp->sound);
      FREE(et);
      return(BACKGROUND_QUIT);
    }
  return(BACKGROUND_CONTINUE);
}

static XEN g_channel_amp_envs(XEN filename, XEN chan, XEN pts, XEN peak_func, XEN done_func)
{
  /* return two vectors of size pts containing y vals (min and max) of amp env
   *   if peak_func, use it to get peak_env_info file if needed
   *   if done_func set workproc that calls it when done
   */
  #define H_channel_amp_envs "(" S_channel_amp_envs " (file #f) (chan 0) (size #f) (peak-file-func #f) (work-proc-func #f)): \
return two vcts of length 'size' containing y vals (min and max) of file's channel chan's amp envs. \
'peak-file-func' is used to get the name of the associated peak_env_info file if the file is very large. \
'work-proc-func' is called when the amp envs are ready if the amp envs are gathered in the background. \
If 'filename' is a sound index (an integer), 'size' is an edit-position, and the current amp envs are returned."

  char *fullname = NULL, *peakname;
  int len, chn, pos;
  snd_info *sp = NULL;
  chan_info *cp = NULL;
  XEN peak = XEN_FALSE;
  env_state *es;
  snd_state *ss;
  env_info *ep;
  int id, err = 0;
  env_tick *et;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename) || XEN_INTEGER_P(filename) || XEN_NOT_BOUND_P(filename), 
		  filename, XEN_ARG_1, S_channel_amp_envs, "a string or sound index");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_channel_amp_envs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(pts), pts, XEN_ARG_3, S_channel_amp_envs, "an integer");
  XEN_ASSERT_TYPE(((XEN_PROCEDURE_P(peak_func)) && (XEN_TO_C_INT(XEN_CAR(XEN_ARITY(peak_func))) == 2)) ||
		  (XEN_NOT_BOUND_P(peak_func)), peak_func, XEN_ARG_4, S_channel_amp_envs, "a procedure of 2 args");
  XEN_ASSERT_TYPE(((XEN_PROCEDURE_P(done_func)) && (XEN_TO_C_INT(XEN_CAR(XEN_ARITY(done_func))) == 3)) ||
		  (XEN_NOT_BOUND_P(done_func)), done_func, XEN_ARG_5, S_channel_amp_envs, "a procedure of 3 args");

  if ((XEN_INTEGER_P(filename)) || (XEN_NOT_BOUND_P(filename)))
    {
      cp = get_cp(filename, chan, S_channel_amp_envs);
      if (cp)
	{
	  pos = to_c_edit_position(cp, pts, S_channel_amp_envs, 3);
	  if ((pos != cp->edit_ctr) || (cp->amp_envs == NULL)) 
	    return(XEN_EMPTY_LIST);
	  ep = cp->amp_envs[pos];
	  if (ep)
	    return(g_env_info_to_vcts(ep, ep->amp_env_size));
	  /* force amp env to completion */
	  stop_amp_env(cp);
	  es = make_env_state(cp, cp->samples[pos]);
	  if (es)
	    {
	      while (!(tick_amp_env(cp, es)));
	      FREE(es);
	      ep = cp->amp_envs[pos];
	      if (ep)
		return(g_env_info_to_vcts(ep, ep->amp_env_size));
	    }
	  return(XEN_EMPTY_LIST);
	}
    }
  /* filename is a string from here down */
  ss = get_global_state();
  fullname = mus_expand_filename(XEN_TO_C_STRING(filename));
  chn = XEN_TO_C_INT(chan);
  len = XEN_TO_C_INT(pts);
  /* look for sp->filename = fullname
     then peak
     then read direct (via make_sound_readable)
  */
  sp = find_sound(ss, fullname, 0);
  if (sp)
    {
      cp = sp->chans[chn];
      if ((cp->amp_envs) && (cp->amp_envs[0]))
	{
	  /* read amp_env data into pts (presumably smaller) */
	  peak = g_env_info_to_vcts(cp->amp_envs[0], len);
	  if (fullname) FREE(fullname);
	  return(peak);
	}
    }
  if (XEN_PROCEDURE_P(peak_func))
    {
      peak = XEN_CALL_2(peak_func,
			filename,
			chan,
			"peak env procedure");
      if (XEN_STRING_P(peak))
	{
	  peakname = mus_expand_filename(XEN_TO_C_STRING(peak));
	  if (mus_file_probe(peakname))
	    {
	      ep = get_peak_env_info(peakname, &err);
	      if (ep)
		{
		  /* read amp_env data into pts (presumably smaller) */
		  peak = g_env_info_to_vcts(ep, len);
		  ep = free_env_info(ep);
		  if (peakname) FREE(peakname);
		  if (fullname) FREE(fullname);
		  return(peak);
		}
	    }
	  if (peakname) FREE(peakname);
	}
    }
  /* now set up to read direct... */
  peak = XEN_FALSE;
  if (mus_file_probe(fullname))
    sp = make_sound_readable(ss, fullname, false);
  if (sp)
    {
      cp = sp->chans[chn];
      cp->edit_ctr = 0;
      cp->active = true;
      es = make_env_state(cp, cp->samples[0]);
      if (es)
	{
	  if (XEN_PROCEDURE_P(done_func))
	    {
	      et = (env_tick *)CALLOC(1, sizeof(env_tick));
	      et->cp = cp;
	      et->es = es;
	      et->func = done_func;
	      et->len = len;
	      et->filename = filename;
	      snd_protect(done_func);
	      id = (int)BACKGROUND_ADD(ss, tick_it, (Indicium)et);
	      if (fullname) FREE(fullname);
	      return(C_TO_XEN_INT(id));
	    }
	  while (!(tick_amp_env(cp, es)));
	  if (es->sf) free_snd_fd(es->sf);
	  FREE(es);
	  peak = g_env_info_to_vcts(cp->amp_envs[0], len);
	}
      cp->active = false;
      completely_free_snd_info(sp);
    }
  if (fullname) FREE(fullname);
  return(xen_return_first(peak, peak_func));
}

static XEN g_start_progress_report(XEN snd)
{
  #define H_start_progress_report "(" S_start_progress_report " (snd #f)): post the hour-glass icon"
  snd_info *sp;
  ASSERT_JUST_SOUND(S_start_progress_report, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_start_progress_report, snd));
  start_progress_report(sp, NOT_FROM_ENVED);
  return(snd);
}

static XEN g_finish_progress_report(XEN snd)
{
  #define H_finish_progress_report "(" S_finish_progress_report " (snd #f)): remove the hour-glass icon"
  snd_info *sp;
  ASSERT_JUST_SOUND(S_finish_progress_report, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_finish_progress_report, snd));
  finish_progress_report(sp, NOT_FROM_ENVED);
  return(snd); 
}

static XEN g_progress_report(XEN pct, XEN name, XEN cur_chan, XEN chans, XEN snd)
{
  #define H_progress_report "(" S_progress_report " pct (name #f) (cur-chan #f) (chans #f) (snd #f)): \
update an on-going 'progress report' (an animated hour-glass icon) in snd using pct to indicate how far along we are"

  snd_info *sp;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(pct), pct, XEN_ARG_1, S_progress_report, "a number");
  ASSERT_JUST_SOUND(S_progress_report, snd, 5);
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(name), name, XEN_ARG_2, S_progress_report, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(cur_chan), cur_chan, XEN_ARG_3, S_progress_report, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chans), chans, XEN_ARG_4, S_progress_report, "an integer");
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_progress_report, snd));
  progress_report(sp,
		  (XEN_STRING_P(name)) ? XEN_TO_C_STRING(name) : "something useful",
		  XEN_TO_C_INT_OR_ELSE(cur_chan, 0),
		  XEN_TO_C_INT_OR_ELSE(chans, sp->nchans),
		  XEN_TO_C_DOUBLE(pct),
		  NOT_FROM_ENVED);
  return(snd);
}



#ifdef XEN_ARGIFY_1
XEN_ARGIFY_1(g_sound_p_w, g_sound_p)
XEN_ARGIFY_2(g_bomb_w, g_bomb)
XEN_ARGIFY_2(g_find_sound_w, g_find_sound)
XEN_ARGIFY_1(g_channels_w, g_channels)
XEN_ARGIFY_2(g_set_channels_w, g_set_channels)
XEN_ARGIFY_1(g_srate_w, g_srate)
XEN_ARGIFY_2(g_set_srate_w, g_set_srate)
XEN_ARGIFY_1(g_data_location_w, g_data_location)
XEN_ARGIFY_2(g_set_data_location_w, g_set_data_location)
XEN_ARGIFY_1(g_data_size_w, g_data_size)
XEN_ARGIFY_2(g_set_data_size_w, g_set_data_size)
XEN_ARGIFY_1(g_data_format_w, g_data_format)
XEN_ARGIFY_2(g_set_data_format_w, g_set_data_format)
XEN_ARGIFY_1(g_header_type_w, g_header_type)
XEN_ARGIFY_2(g_set_header_type_w, g_set_header_type)
XEN_ARGIFY_1(g_comment_w, g_comment)
XEN_ARGIFY_2(g_set_comment_w, g_set_comment)
XEN_ARGIFY_1(g_file_name_w, g_file_name)
XEN_ARGIFY_1(g_short_file_name_w, g_short_file_name)
XEN_ARGIFY_1(g_save_controls_w, g_save_controls)
XEN_ARGIFY_1(g_restore_controls_w, g_restore_controls)
XEN_ARGIFY_1(g_reset_controls_w, g_reset_controls)
XEN_NARGIFY_0(g_selected_sound_w, g_selected_sound)
XEN_ARGIFY_1(g_selected_channel_w, g_selected_channel)
XEN_ARGIFY_2(g_set_selected_channel_w, g_set_selected_channel)
XEN_ARGIFY_1(g_select_sound_w, g_select_sound)
XEN_ARGIFY_1(g_select_channel_w, g_select_channel)
XEN_ARGIFY_1(g_close_sound_w, g_close_sound)
XEN_ARGIFY_1(g_update_sound_w, g_update_sound)
XEN_ARGIFY_1(g_save_sound_w, g_save_sound)
XEN_NARGIFY_1(g_open_sound_w, g_open_sound)
XEN_NARGIFY_4(g_open_raw_sound_w, g_open_raw_sound)
XEN_NARGIFY_1(g_view_sound_w, g_view_sound)
XEN_ARGIFY_7(g_new_sound_w, g_new_sound)
XEN_ARGIFY_1(g_revert_sound_w, g_revert_sound)
XEN_ARGIFY_7(g_save_sound_as_w, g_save_sound_as)
XEN_ARGIFY_4(g_apply_controls_w, g_apply_controls)
XEN_ARGIFY_1(g_filter_control_env_w, g_filter_control_env)
XEN_ARGIFY_2(g_set_filter_control_env_w, g_set_filter_control_env)
XEN_ARGIFY_1(g_cursor_follows_play_w, g_cursor_follows_play)
XEN_ARGIFY_2(g_set_cursor_follows_play_w, g_set_cursor_follows_play)
XEN_ARGIFY_1(g_show_controls_w, g_show_controls)
XEN_ARGIFY_2(g_set_show_controls_w, g_set_show_controls)
XEN_ARGIFY_1(g_sync_w, g_sync)
XEN_ARGIFY_2(g_set_sync_w, g_set_sync)
XEN_ARGIFY_1(g_sound_properties_w, g_sound_properties)
XEN_ARGIFY_2(g_set_sound_properties_w, g_set_sound_properties)
XEN_ARGIFY_1(g_channel_style_w, g_channel_style)
XEN_ARGIFY_2(g_set_channel_style_w, g_set_channel_style)
XEN_ARGIFY_1(g_read_only_w, g_read_only)
XEN_ARGIFY_2(g_set_read_only_w, g_set_read_only)
XEN_ARGIFY_1(g_expand_control_p_w, g_expand_control_p)
XEN_ARGIFY_2(g_set_expand_control_p_w, g_set_expand_control_p)
XEN_ARGIFY_1(g_contrast_control_p_w, g_contrast_control_p)
XEN_ARGIFY_2(g_set_contrast_control_p_w, g_set_contrast_control_p)
XEN_ARGIFY_1(g_reverb_control_p_w, g_reverb_control_p)
XEN_ARGIFY_2(g_set_reverb_control_p_w, g_set_reverb_control_p)
XEN_ARGIFY_1(g_filter_control_p_w, g_filter_control_p)
XEN_ARGIFY_2(g_set_filter_control_p_w, g_set_filter_control_p)
XEN_ARGIFY_1(g_filter_control_in_dB_w, g_filter_control_in_dB)
XEN_ARGIFY_2(g_set_filter_control_in_dB_w, g_set_filter_control_in_dB)
XEN_ARGIFY_1(g_filter_control_coeffs_w, g_filter_control_coeffs)
XEN_ARGIFY_1(g_filter_control_order_w, g_filter_control_order)
XEN_ARGIFY_2(g_set_filter_control_order_w, g_set_filter_control_order)
XEN_ARGIFY_1(g_contrast_control_w, g_contrast_control)
XEN_ARGIFY_2(g_set_contrast_control_w, g_set_contrast_control)
XEN_ARGIFY_1(g_contrast_control_amp_w, g_contrast_control_amp)
XEN_ARGIFY_2(g_set_contrast_control_amp_w, g_set_contrast_control_amp)
XEN_ARGIFY_1(g_expand_control_w, g_expand_control)
XEN_ARGIFY_2(g_set_expand_control_w, g_set_expand_control)
XEN_ARGIFY_1(g_expand_control_length_w, g_expand_control_length)
XEN_ARGIFY_2(g_set_expand_control_length_w, g_set_expand_control_length)
XEN_ARGIFY_1(g_expand_control_ramp_w, g_expand_control_ramp)
XEN_ARGIFY_2(g_set_expand_control_ramp_w, g_set_expand_control_ramp)
XEN_ARGIFY_1(g_expand_control_hop_w, g_expand_control_hop)
XEN_ARGIFY_2(g_set_expand_control_hop_w, g_set_expand_control_hop)
XEN_ARGIFY_1(g_speed_control_w, g_speed_control)
XEN_ARGIFY_2(g_set_speed_control_w, g_set_speed_control)
XEN_ARGIFY_1(g_reverb_control_length_w, g_reverb_control_length)
XEN_ARGIFY_2(g_set_reverb_control_length_w, g_set_reverb_control_length)
XEN_ARGIFY_1(g_reverb_control_scale_w, g_reverb_control_scale)
XEN_ARGIFY_2(g_set_reverb_control_scale_w, g_set_reverb_control_scale)
XEN_ARGIFY_1(g_reverb_control_feedback_w, g_reverb_control_feedback)
XEN_ARGIFY_2(g_set_reverb_control_feedback_w, g_set_reverb_control_feedback)
XEN_ARGIFY_1(g_reverb_control_lowpass_w, g_reverb_control_lowpass)
XEN_ARGIFY_2(g_set_reverb_control_lowpass_w, g_set_reverb_control_lowpass)
XEN_ARGIFY_2(g_amp_control_w, g_amp_control)
XEN_ARGIFY_3(g_set_amp_control_w, g_set_amp_control)
XEN_ARGIFY_1(g_reverb_control_decay_w, g_reverb_control_decay)
XEN_ARGIFY_2(g_set_reverb_control_decay_w, g_set_reverb_control_decay)
XEN_ARGIFY_1(g_speed_control_style_w, g_speed_control_style)
XEN_ARGIFY_2(g_set_speed_control_style_w, g_set_speed_control_style)
XEN_ARGIFY_1(g_speed_control_tones_w, g_speed_control_tones)
XEN_ARGIFY_2(g_set_speed_control_tones_w, g_set_speed_control_tones)
XEN_ARGIFY_3(g_peak_env_info_w, g_peak_env_info)
XEN_NARGIFY_3(g_write_peak_env_info_file_w, g_write_peak_env_info_file)
XEN_NARGIFY_3(g_read_peak_env_info_file_w, g_read_peak_env_info_file)
XEN_ARGIFY_5(g_channel_amp_envs_w, g_channel_amp_envs);
XEN_ARGIFY_1(g_start_progress_report_w, g_start_progress_report)
XEN_ARGIFY_1(g_finish_progress_report_w, g_finish_progress_report)
XEN_ARGIFY_5(g_progress_report_w, g_progress_report)
#else
#define g_sound_p_w g_sound_p
#define g_bomb_w g_bomb
#define g_find_sound_w g_find_sound
#define g_channels_w g_channels
#define g_set_channels_w g_set_channels
#define g_srate_w g_srate
#define g_set_srate_w g_set_srate
#define g_data_location_w g_data_location
#define g_set_data_location_w g_set_data_location
#define g_data_size_w g_data_size
#define g_set_data_size_w g_set_data_size
#define g_data_format_w g_data_format
#define g_set_data_format_w g_set_data_format
#define g_header_type_w g_header_type
#define g_set_header_type_w g_set_header_type
#define g_comment_w g_comment
#define g_set_comment_w g_set_comment
#define g_file_name_w g_file_name
#define g_short_file_name_w g_short_file_name
#define g_save_controls_w g_save_controls
#define g_restore_controls_w g_restore_controls
#define g_reset_controls_w g_reset_controls
#define g_selected_sound_w g_selected_sound
#define g_selected_channel_w g_selected_channel
#define g_set_selected_channel_w g_set_selected_channel
#define g_select_sound_w g_select_sound
#define g_select_channel_w g_select_channel
#define g_close_sound_w g_close_sound
#define g_update_sound_w g_update_sound
#define g_save_sound_w g_save_sound
#define g_open_sound_w g_open_sound
#define g_open_raw_sound_w g_open_raw_sound
#define g_view_sound_w g_view_sound
#define g_new_sound_w g_new_sound
#define g_revert_sound_w g_revert_sound
#define g_save_sound_as_w g_save_sound_as
#define g_apply_controls_w g_apply_controls
#define g_filter_control_env_w g_filter_control_env
#define g_set_filter_control_env_w g_set_filter_control_env
#define g_cursor_follows_play_w g_cursor_follows_play
#define g_set_cursor_follows_play_w g_set_cursor_follows_play
#define g_show_controls_w g_show_controls
#define g_set_show_controls_w g_set_show_controls
#define g_sync_w g_sync
#define g_set_sync_w g_set_sync
#define g_sound_properties_w g_sound_properties
#define g_set_sound_properties_w g_set_sound_properties
#define g_channel_style_w g_channel_style
#define g_set_channel_style_w g_set_channel_style
#define g_read_only_w g_read_only
#define g_set_read_only_w g_set_read_only
#define g_expand_control_p_w g_expand_control_p
#define g_set_expand_control_p_w g_set_expand_control_p
#define g_contrast_control_p_w g_contrast_control_p
#define g_set_contrast_control_p_w g_set_contrast_control_p
#define g_reverb_control_p_w g_reverb_control_p
#define g_set_reverb_control_p_w g_set_reverb_control_p
#define g_filter_control_p_w g_filter_control_p
#define g_set_filter_control_p_w g_set_filter_control_p
#define g_filter_control_in_dB_w g_filter_control_in_dB
#define g_set_filter_control_in_dB_w g_set_filter_control_in_dB
#define g_filter_control_coeffs_w g_filter_control_coeffs
#define g_filter_control_order_w g_filter_control_order
#define g_set_filter_control_order_w g_set_filter_control_order
#define g_contrast_control_w g_contrast_control
#define g_set_contrast_control_w g_set_contrast_control
#define g_contrast_control_amp_w g_contrast_control_amp
#define g_set_contrast_control_amp_w g_set_contrast_control_amp
#define g_expand_control_w g_expand_control
#define g_set_expand_control_w g_set_expand_control
#define g_expand_control_length_w g_expand_control_length
#define g_set_expand_control_length_w g_set_expand_control_length
#define g_expand_control_ramp_w g_expand_control_ramp
#define g_set_expand_control_ramp_w g_set_expand_control_ramp
#define g_expand_control_hop_w g_expand_control_hop
#define g_set_expand_control_hop_w g_set_expand_control_hop
#define g_speed_control_w g_speed_control
#define g_set_speed_control_w g_set_speed_control
#define g_reverb_control_length_w g_reverb_control_length
#define g_set_reverb_control_length_w g_set_reverb_control_length
#define g_reverb_control_scale_w g_reverb_control_scale
#define g_set_reverb_control_scale_w g_set_reverb_control_scale
#define g_reverb_control_feedback_w g_reverb_control_feedback
#define g_set_reverb_control_feedback_w g_set_reverb_control_feedback
#define g_reverb_control_lowpass_w g_reverb_control_lowpass
#define g_set_reverb_control_lowpass_w g_set_reverb_control_lowpass
#define g_amp_control_w g_amp_control
#define g_set_amp_control_w g_set_amp_control
#define g_reverb_control_decay_w g_reverb_control_decay
#define g_set_reverb_control_decay_w g_set_reverb_control_decay
#define g_speed_control_style_w g_speed_control_style
#define g_set_speed_control_style_w g_set_speed_control_style
#define g_speed_control_tones_w g_speed_control_tones
#define g_set_speed_control_tones_w g_set_speed_control_tones
#define g_peak_env_info_w g_peak_env_info
#define g_write_peak_env_info_file_w g_write_peak_env_info_file
#define g_read_peak_env_info_file_w g_read_peak_env_info_file
#define g_channel_amp_envs_w g_channel_amp_envs
#define g_start_progress_report_w g_start_progress_report
#define g_finish_progress_report_w g_finish_progress_report
#define g_progress_report_w g_progress_report
#endif

void g_init_snd(void)
{
  #define H_name_click_hook S_name_click_hook " (snd): called when sound name clicked. \
If it returns #t, the usual informative minibuffer babbling is squelched."

  #define H_before_apply_hook S_before_apply_hook " (snd): called when 'Apply' is clicked or apply-controls called. \
If it returns #t, the apply is aborted."

  #define H_after_apply_hook S_after_apply_hook " (snd): called when 'Apply' finishes."

  XEN_DEFINE_HOOK(name_click_hook,   S_name_click_hook,   1, H_name_click_hook);       /* args = snd-index */
  XEN_DEFINE_HOOK(before_apply_hook, S_before_apply_hook, 1, H_before_apply_hook);     /* args = snd-index */
  XEN_DEFINE_HOOK(after_apply_hook,  S_after_apply_hook,  1, H_after_apply_hook);      /* args = snd-index */

  #define H_channels_separate "The value for " S_channel_style " that causes channel graphs to occupy separate panes"
  #define H_channels_combined "The value for " S_channel_style " that causes channel graphs to occupy one panes (the 'unite' button)"
  #define H_channels_superimposed "The value for " S_channel_style " that causes channel graphs to occupy one pane and one axis"

  XEN_DEFINE_CONSTANT(S_channels_separate,     CHANNELS_SEPARATE,     H_channels_separate);
  XEN_DEFINE_CONSTANT(S_channels_combined,     CHANNELS_COMBINED,     H_channels_combined);
  XEN_DEFINE_CONSTANT(S_channels_superimposed, CHANNELS_SUPERIMPOSED, H_channels_superimposed);

  XEN_DEFINE_PROCEDURE(S_sound_p, g_sound_p_w, 0, 1, 0, H_sound_p);
  XEN_DEFINE_PROCEDURE(S_bomb, g_bomb_w, 0, 2, 0, H_bomb);
  XEN_DEFINE_PROCEDURE(S_find_sound, g_find_sound_w, 1, 1, 0, H_find_sound);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_channels,      g_channels_w,      H_channels,      S_setB S_channels,      g_set_channels_w,       0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_chans,         g_channels_w,      H_channels,      S_setB S_chans,         g_set_channels_w,       0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_srate,         g_srate_w,         H_srate,         S_setB S_srate,         g_set_srate_w,          0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_location, g_data_location_w, H_data_location, S_setB S_data_location, g_set_data_location_w,  0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_size,     g_data_size_w,     H_data_size,     S_setB S_data_size,     g_set_data_size_w,      0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_format,   g_data_format_w,   H_data_format,   S_setB S_data_format,   g_set_data_format_w,    0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_header_type,   g_header_type_w,   H_header_type,   S_setB S_header_type,   g_set_header_type_w,    0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_comment,       g_comment_w,       H_comment,       S_setB S_comment,       g_set_comment_w,        0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE(S_file_name,             g_file_name_w, 0, 1, 0,             H_file_name);
  XEN_DEFINE_PROCEDURE(S_short_file_name,       g_short_file_name_w, 0, 1, 0,       H_short_file_name);
  XEN_DEFINE_PROCEDURE(S_save_controls,         g_save_controls_w, 0, 1, 0,         H_save_controls);
  XEN_DEFINE_PROCEDURE(S_restore_controls,      g_restore_controls_w, 0, 1, 0,      H_restore_controls);
  XEN_DEFINE_PROCEDURE(S_reset_controls,        g_reset_controls_w, 0, 1, 0,        H_reset_controls);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_sound, g_selected_sound_w, H_selected_sound, S_setB S_selected_sound, g_select_sound_w,  0, 0, 0, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_channel, g_selected_channel_w, H_selected_channel, 
				   S_setB S_selected_channel, g_set_selected_channel_w,  0, 1, 0, 2);
  XEN_DEFINE_PROCEDURE(S_select_sound, g_select_sound_w, 0, 1, 0, H_select_sound);
  XEN_DEFINE_PROCEDURE(S_select_channel, g_select_channel_w, 0, 1, 0, H_select_channel);

  XEN_DEFINE_PROCEDURE(S_start_progress_report, g_start_progress_report_w, 0, 1, 0, H_start_progress_report);
  XEN_DEFINE_PROCEDURE(S_finish_progress_report, g_finish_progress_report_w, 0, 1, 0, H_finish_progress_report);
  XEN_DEFINE_PROCEDURE(S_progress_report,      g_progress_report_w, 1, 4, 0,      H_progress_report);

  XEN_DEFINE_PROCEDURE(S_close_sound,          g_close_sound_w, 0, 1, 0,          H_close_sound);
  XEN_DEFINE_PROCEDURE(S_update_sound,         g_update_sound_w, 0, 1, 0,         H_update_sound);
  XEN_DEFINE_PROCEDURE(S_save_sound,           g_save_sound_w, 0, 1, 0,           H_save_sound);
  XEN_DEFINE_PROCEDURE(S_open_sound,           g_open_sound_w, 1, 0, 0,           H_open_sound);
  XEN_DEFINE_PROCEDURE(S_open_raw_sound,       g_open_raw_sound_w, 4, 0, 0,       H_open_raw_sound);
  XEN_DEFINE_PROCEDURE(S_view_sound,           g_view_sound_w, 1, 0, 0,           H_view_sound);
  XEN_DEFINE_PROCEDURE(S_new_sound,            g_new_sound_w, 0, 7, 0,            H_new_sound);
  XEN_DEFINE_PROCEDURE(S_revert_sound,         g_revert_sound_w, 0, 1, 0,         H_revert_sound);
  XEN_DEFINE_PROCEDURE(S_save_sound_as,        g_save_sound_as_w, 1, 6, 0,        H_save_sound_as);
  XEN_DEFINE_PROCEDURE(S_apply_controls,       g_apply_controls_w, 0, 4, 0,       H_apply_controls);


  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_env, g_filter_control_env_w, H_filter_control_env,
					    S_setB S_filter_control_env, g_set_filter_control_env_w, g_set_filter_control_env_reversed, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_cursor_follows_play, g_cursor_follows_play_w, H_cursor_follows_play,
					    S_setB S_cursor_follows_play, g_set_cursor_follows_play_w, g_set_cursor_follows_play_reversed, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_controls, g_show_controls_w, H_show_controls,
					    S_setB S_show_controls, g_set_show_controls_w, g_set_show_controls_reversed, 0, 1, 1, 1);
  
#if HAVE_GUILE
  XEN_EVAL_C_STRING("(define %sync sync)"); /* protect the original meaning (a Guile/Posix built-in function) */
#endif

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sync, g_sync_w, H_sync,
					    S_setB S_sync, g_set_sync_w, g_set_sync_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sound_properties, g_sound_properties_w, H_sound_properties,
					    S_setB S_sound_properties, g_set_sound_properties_w, g_set_sound_properties_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_channel_style, g_channel_style_w, H_channel_style,
					    S_setB S_channel_style, g_set_channel_style_w, g_set_channel_style_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_read_only, g_read_only_w, H_read_only,
					    S_setB S_read_only, g_set_read_only_w, g_set_read_only_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_p, g_expand_control_p_w, H_expand_control_p,
					    S_setB S_expand_control_p, g_set_expand_control_p_w, g_set_expand_control_p_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control_p, g_contrast_control_p_w, H_contrast_control_p,
					    S_setB S_contrast_control_p, g_set_contrast_control_p_w, g_set_contrast_control_p_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_p, g_reverb_control_p_w, H_reverb_control_p,
					    S_setB S_reverb_control_p, g_set_reverb_control_p_w, g_set_reverb_control_p_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_p, g_filter_control_p_w, H_filter_control_p,
					    S_setB S_filter_control_p, g_set_filter_control_p_w, g_set_filter_control_p_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_in_dB, g_filter_control_in_dB_w, H_filter_control_in_dB,
					    S_setB S_filter_control_in_dB, g_set_filter_control_in_dB_w, g_set_filter_control_in_dB_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE(S_filter_control_coeffs, g_filter_control_coeffs_w, 0, 1, 0, H_filter_control_coeffs);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_order, g_filter_control_order_w, H_filter_control_order,
					    S_setB S_filter_control_order, g_set_filter_control_order_w, g_set_filter_control_order_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control, g_contrast_control_w, H_contrast_control,
					    S_setB S_contrast_control, g_set_contrast_control_w, g_set_contrast_control_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control_amp, g_contrast_control_amp_w, H_contrast_control_amp,
					    S_setB S_contrast_control_amp, g_set_contrast_control_amp_w, g_set_contrast_control_amp_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control, g_expand_control_w, H_expand_control,
					    S_setB S_expand_control, g_set_expand_control_w, g_set_expand_control_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_length, g_expand_control_length_w, H_expand_control_length,
					    S_setB S_expand_control_length, g_set_expand_control_length_w, g_set_expand_control_length_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_ramp, g_expand_control_ramp_w, H_expand_control_ramp,
					    S_setB S_expand_control_ramp, g_set_expand_control_ramp_w, g_set_expand_control_ramp_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_hop, g_expand_control_hop_w, H_expand_control_hop,
					    S_setB S_expand_control_hop, g_set_expand_control_hop_w, g_set_expand_control_hop_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_speed_control, g_speed_control_w, H_speed_control,
					    S_setB S_speed_control, g_set_speed_control_w, g_set_speed_control_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_length, g_reverb_control_length_w, H_reverb_control_length,
					    S_setB S_reverb_control_length, g_set_reverb_control_length_w, g_set_reverb_control_length_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_scale, g_reverb_control_scale_w, H_reverb_control_scale,
					    S_setB S_reverb_control_scale, g_set_reverb_control_scale_w, g_set_reverb_control_scale_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_feedback, g_reverb_control_feedback_w, H_reverb_control_feedback,
					    S_setB S_reverb_control_feedback, g_set_reverb_control_feedback_w, g_set_reverb_control_feedback_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_lowpass, g_reverb_control_lowpass_w, H_reverb_control_lowpass,
					    S_setB S_reverb_control_lowpass, g_set_reverb_control_lowpass_w, g_set_reverb_control_lowpass_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_amp_control, g_amp_control_w, H_amp_control,
					    S_setB S_amp_control, g_set_amp_control_w, g_set_amp_control_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_decay, g_reverb_control_decay_w, H_reverb_control_decay,
					    S_setB S_reverb_control_decay, g_set_reverb_control_decay_w, g_set_reverb_control_decay_reversed, 0, 1, 1, 1);
  
  #define H_speed_control_as_float "The value for " S_speed_control_style " that interprets the speed slider as a float"
  #define H_speed_control_as_ratio "The value for " S_speed_control_style " that interprets the speed slider as a just-intonation ratio"
  #define H_speed_control_as_semitone "The value for " S_speed_control_style " that interprets the speed slider as a microtone (via " S_speed_control_tones ")"
  
  XEN_DEFINE_CONSTANT(S_speed_control_as_float,        SPEED_CONTROL_AS_FLOAT,    H_speed_control_as_float);
  XEN_DEFINE_CONSTANT(S_speed_control_as_ratio,        SPEED_CONTROL_AS_RATIO,    H_speed_control_as_ratio);
  XEN_DEFINE_CONSTANT(S_speed_control_as_semitone,     SPEED_CONTROL_AS_SEMITONE, H_speed_control_as_semitone);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_speed_control_style, g_speed_control_style_w, H_speed_control_style,
					    S_setB S_speed_control_style, g_set_speed_control_style_w, g_set_speed_control_style_reversed, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_speed_control_tones, g_speed_control_tones_w, H_speed_control_tones,
					    S_setB S_speed_control_tones, g_set_speed_control_tones_w, g_set_speed_control_tones_reversed, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE(S_peak_env_info, g_peak_env_info_w, 0, 3, 0, H_peak_env_info);
  XEN_DEFINE_PROCEDURE(S_write_peak_env_info_file, g_write_peak_env_info_file_w, 3, 0, 0, H_write_peak_env_info_file);
  XEN_DEFINE_PROCEDURE(S_read_peak_env_info_file,  g_read_peak_env_info_file_w,  3, 0, 0, H_read_peak_env_info_file);
  XEN_DEFINE_PROCEDURE(S_channel_amp_envs, g_channel_amp_envs_w, 0, 5, 0, H_channel_amp_envs);
}

