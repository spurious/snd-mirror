#include "snd.h"

snd_info *snd_new_file(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *new_comment)
{
  snd_info *sp;
  int chan, size, err;
  unsigned char* buf;
  if (snd_overwrite_ok(ss, newname))
    {
      if (mus_header_writable(header_type, data_format))
	{
	  err = snd_write_header(ss, newname, header_type, srate, chans, 0, 
				 chans * 2, data_format, new_comment, 
				 snd_strlen(new_comment), NULL);
	  if (err == -1)
	    snd_error("can't write %s: %s",newname, strerror(errno));
	  else
	    {
	      chan = snd_reopen_write(ss, newname);
	      lseek(chan, mus_header_data_location(), SEEK_SET);
	      size = chans * mus_samples_to_bytes(data_format, 2); /* why 2 samples? */
	      buf = (unsigned char *)CALLOC(size, sizeof(unsigned char));
	      write(chan, buf, size);
	      if (close(chan) != 0)
		snd_error("can't close %d (%s): %s [%s[%d] %s]",
			  chan, newname, strerror(errno),
			  __FILE__, __LINE__, __FUNCTION__);
	      FREE(buf);
	      sp = snd_open_file(newname, ss);
	      return(sp);
	    }
	}
      else 
	snd_error("can't write %s %s file with %s data format",
		  ((header_type != MUS_RIFF) && (header_type != MUS_NEXT)) ? "an" : "a",
		  mus_header_type_name(header_type),
		  data_format_name(data_format));
    }
  return(NULL);
}


/* ---------------- amp envs ---------------- */

#define MULTIPLIER 100

env_info *free_amp_env(chan_info *cp, int pos)
{
  /* can be either during channel close, or premature work proc removal */
  env_info *ep;
  if ((cp) && (cp->amp_envs) && (pos < cp->edit_size))
    {
      ep = cp->amp_envs[pos];
      if (ep)
	{
	  if (ep->data_max) {FREE(ep->data_max); ep->data_max = NULL;}
	  if (ep->data_min) {FREE(ep->data_min); ep->data_min = NULL;}
	  FREE(ep);
	}
    }
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
  if (!cp) return;
  cgx = cp->cgx;
  if (!cgx) return;
  es = (env_state *)(cgx->amp_env_state);
  if (!es) return;
  if (es->sf) es->sf = free_snd_fd(es->sf);
  FREE(es);
  cgx->amp_env_state = NULL;
  cgx->amp_env_in_progress = 0;
}

env_state *make_env_state(chan_info *cp, int samples)
{
  int val, pos, i, j, start, start_bin, end, end_bin, old_end_bin, old_samples, happy = 0;
  env_info *ep, *old_ep;
  env_state *es;
  stop_amp_env(cp);
  pos = cp->edit_ctr;
  es = (env_state *)CALLOC(1, sizeof(env_state));
  es->samples = samples;
  es->slice = 0;
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
      old_ep = cp->amp_envs[cp->edit_ctr - 1];
      if ((cp->edit_ctr > 0) && 
	  (old_ep) && 
	  (old_ep->completed))
	{
	  /* here in many cases, the preceding edit's amp env has most of the data we need.
	   * cp->edits[cp->edit_ctr] describes the current edit, with beg and end, so in the
	   * simplest case, we can just copy to the bin representing beg, and from the bin
	   * representing end (setting ep->top_bin and ep->bin); if the file's length has
	   * changed dramatically, we need to do it all.  fmin/fmax need to be set as we copy.
	   * as-one-edit can mess this up...
	   */
	  old_samples = cp->samples[cp->edit_ctr - 1];
	  if (abs(samples - old_samples) < (samples / 2))
	    {
	      start = edit_changes_begin_at(cp);
	      end = edit_changes_end_at(cp);
	      if (abs(end - start) < (samples / 2))
		{
		  /* here we'll try to take advantage of an existing envelope */
		  old_ep = cp->amp_envs[cp->edit_ctr - 1];
		  ep->samps_per_bin = old_ep->samps_per_bin;
		  ep->amp_env_size = (int)(ceil((Float)(es->samples) / (Float)(ep->samps_per_bin)));
		  ep->data_max = (MUS_SAMPLE_TYPE *)CALLOC(ep->amp_env_size, sizeof(MUS_SAMPLE_TYPE));
		  ep->data_min = (MUS_SAMPLE_TYPE *)CALLOC(ep->amp_env_size, sizeof(MUS_SAMPLE_TYPE));
		  start_bin = (int)(start / ep->samps_per_bin);
		  ep->fmin = MUS_SAMPLE_0;
		  ep->fmax = MUS_SAMPLE_0;
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
		  happy = 1;
		}
	    }
	}
      if (happy == 0)
	{
	  /* we want samps_per_bin to be useful over a wide range of file sizes */
	  /* 160e6 = about a hour at 44KHz */
	  val = (int)(log((double)(es->samples)));
	  ep->amp_env_size = (int)pow(2.0, val);
	  ep->samps_per_bin = (int)(ceil((Float)(es->samples) / (Float)(ep->amp_env_size)));
	  ep->data_max = (MUS_SAMPLE_TYPE *)CALLOC(ep->amp_env_size, sizeof(MUS_SAMPLE_TYPE));
	  ep->data_min = (MUS_SAMPLE_TYPE *)CALLOC(ep->amp_env_size, sizeof(MUS_SAMPLE_TYPE));
	  ep->bin = 0;
	  ep->top_bin = 0;
	  ep->fmin = MUS_SAMPLE_0;
	  ep->fmax = MUS_SAMPLE_0;
	  /* preset as much as possible of the envelope */
	}
      cp->amp_envs[pos] = ep;
      ep->completed = 0;
    }
  es->sf = NULL;
  es->amp_buffer_size = ep->samps_per_bin * MULTIPLIER;
  return(es);
}

#ifndef AMP_ENV_SUBSAMPLE
  #define AMP_ENV_SUBSAMPLE 50
  /* sets how may samples we actually check per bin in huge sound files -- 10 looked ok too */
  /* the lower this number, the faster the amp-envs are calculated, but the higher chance of missing something */
#endif

int tick_amp_env(chan_info *cp, env_state *es)
{
  env_info *ep;
  int i, n, sb, lm;
  MUS_SAMPLE_TYPE ymin, ymax, val;
  snd_fd *sfd;
  ep = es->ep;
  if (es->slice == 0)
    {
      if (ep->top_bin != 0)
	lm = (ep->top_bin - ep->bin + 1);
      else lm = (ep->amp_env_size - ep->bin);
      if (lm <= 0) 
	lm = 1;
      else 
	if (lm > MULTIPLIER) 
	  lm = MULTIPLIER;
      sb = ep->bin;
      if ((cp->edit_ctr != 0) || 
	  (ep->samps_per_bin < (2 * AMP_ENV_SUBSAMPLE)))
	{
	  if (es->sf == NULL) 
	    es->sf = init_sample_read(ep->bin * ep->samps_per_bin, cp, READ_FORWARD);
	  sfd = es->sf;
	  if (sfd == NULL) return(FALSE);
	  for (n = 0; n < lm; n++, sb++)
	    {
	      val = next_sample(sfd);
	      ymin = val;
	      ymax = val;
	      for (i = 1; i < ep->samps_per_bin; i++)
		{
		  val = next_sample(sfd);
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
	}
      else
	{
	  /* an experiment... */
	  /* sub sample reads even at the lowest level (io.c -- using dummy chans to subsample) */
	  /* this actually only helps after the initial read -- the first pass has to read the file into the RAM cache */

	  int fd, subsamp, bin_size, nc, m;
	  snd_info *sp;
	  MUS_SAMPLE_TYPE **bufs;
	  sp = cp->sound;
	  subsamp = ep->samps_per_bin / AMP_ENV_SUBSAMPLE;
	  bin_size = ep->samps_per_bin / subsamp;
	  nc = cp->chan;

	  bufs = (MUS_SAMPLE_TYPE **)CALLOC(sp->nchans * subsamp, sizeof(MUS_SAMPLE_TYPE *));
	  /* we're claiming the file has sp->nchans * subsamp channels to get mus_file_read to subsample by subsamp */
	  bufs[nc] = (MUS_SAMPLE_TYPE *)CALLOC(lm * (bin_size + 2), sizeof(MUS_SAMPLE_TYPE));
	  /* and we only read the current channel (bufs[nc]).
	   * (bin_size+1) should be ok (it can't be bin_size due to annoying round-off problems that
	   *   accumulate over 100 bins, and I'm paranoid)
	   */
	  fd = mus_file_open_read(sp->fullname);
	  mus_file_open_descriptors(fd,
				   sp->fullname,
				   mus_sound_data_format(sp->fullname),
				   mus_sound_datum_size(sp->fullname),
				   mus_sound_data_location(sp->fullname),
				   sp->nchans,
				   mus_sound_header_type(sp->fullname));
	  mus_file_seek_frame(fd, ep->bin * ep->samps_per_bin);
	  mus_file_read_any(fd, 0,
			    sp->nchans * subsamp,
			    lm * (bin_size + 2),
			    bufs,
			    (MUS_SAMPLE_TYPE *)bufs);

	  for (m = 0, n = 0; n < lm; n++, sb++)
	    {
	      val = bufs[nc][m++];
	      ymin = val;
	      ymax = val;
	      for (i = 1; i < ep->samps_per_bin; i+=subsamp, m++)
		{
		  val = bufs[nc][m];
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		}
	      ep->data_max[sb] = ymax;
	      ep->data_min[sb] = ymin;
	      if (ymin < ep->fmin) ep->fmin = ymin;
	      if (ymax > ep->fmax) ep->fmax = ymax;
	    }
	  FREE(bufs[nc]);
	  FREE(bufs);   
	  mus_file_close(fd);
	}

      es->m += es->amp_buffer_size;
      ep->bin += lm;
      if ((es->m >= es->samples) || 
	  ((ep->top_bin > 0) && (ep->bin >= ep->top_bin))) /* this applies to partial amp envs */
	{
	  es->slice++;
	  if (es->sf) es->sf = free_snd_fd(es->sf);
	  ep->completed = 1;
	  return(TRUE);
	}
      return(FALSE);
    }
  else
    {
      ep->completed = 1;
      return(TRUE);
    }
}

BACKGROUND_TYPE get_amp_env(GUI_POINTER ptr)
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
    cgx->amp_env_state = make_env_state(cp, current_ed_samples(cp));
  es = (env_state *)(cgx->amp_env_state);
  if (tick_amp_env(cp, es))
    {
      free_env_state(cp);
      reflect_amp_env_completion(cp->sound);
      if (cp->waiting_to_make_graph) 
	{
	  cp->waiting_to_make_graph = 0;
	  update_graph(cp, NULL);
	}
      return(BACKGROUND_QUIT);
    }
  else return(BACKGROUND_CONTINUE);
}

int amp_env_maxamp_ok(chan_info *cp, int edpos)
{
  env_info *ep;
  if ((cp) && (cp->amp_envs))
    {
      ep = cp->amp_envs[edpos];
      return((ep) && (ep->completed));
    }
  return(FALSE);
}

Float amp_env_maxamp(chan_info *cp, int edpos)
{
  env_info *ep;
  MUS_SAMPLE_TYPE ymax = MUS_SAMPLE_0;
  ep = cp->amp_envs[edpos];
  ymax = -ep->fmin;
  if (ymax < ep->fmax) 
    return(MUS_SAMPLE_TO_FLOAT(ep->fmax));
  return(MUS_SAMPLE_TO_FLOAT(ymax));
}

int amp_env_usable(chan_info *cp, Float samples_per_pixel, int hisamp, int start_new, int edit_pos) 
{
  env_info *ep;
  int bin;
  chan_context *cgx;
  cgx = cp->cgx;
  if ((!cgx) || 
      (!(cp->amp_envs))) 
    return(FALSE);
  ep = cp->amp_envs[edit_pos];
  if (ep)
    {
      bin = (int)hisamp / ep->samps_per_bin; 
      if ((ep) && ((ep->completed) || 
		   (bin < ep->bin) || 
		   ((ep->top_bin != 0) && (bin > ep->top_bin))))
	return(samples_per_pixel >= (Float)(ep->samps_per_bin));
    }
  if ((start_new) &&
      (!(cgx->amp_env_in_progress)) && 
      (current_ed_samples(cp) > AMP_ENV_CUTOFF)) 
    start_amp_env(cp);
  return(FALSE);
}

static short local_grf_y(Float val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((short)(ap->y_base + val * ap->y_scale));
}

int amp_env_graph(chan_info *cp, axis_info *ap, Float samples_per_pixel, int srate) 
{
  Float step, x, xf, xk, pinc = 0.0;
  MUS_SAMPLE_TYPE ymin, ymax;
  int i, j, xi, k, kk;
  env_info *ep;
  ep = cp->amp_envs[cp->edit_ctr];
  step = samples_per_pixel / (Float)(ep->samps_per_bin);
  xf = (Float)(ap->losamp) / (Float)(ep->samps_per_bin);
  j = 0;
  x = ap->x0;
  xi = grf_x(x, ap);
  i = ap->losamp;
  xk = i;
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
      i = (int)xk;
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

void amp_env_scale_by(chan_info *cp, Float scl)
{
  env_info *old_ep, *new_ep;
  int i;
  old_ep = cp->amp_envs[cp->edit_ctr - 1];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if ((new_ep) && 
	  (new_ep->amp_env_size != old_ep->amp_env_size)) 
	new_ep = free_amp_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
	  new_ep->data_max = (MUS_SAMPLE_TYPE *)MALLOC(old_ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE));
	  new_ep->data_min = (MUS_SAMPLE_TYPE *)MALLOC(old_ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE));
	}
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      if (scl >= 0.0)
	{
	  new_ep->fmin = (MUS_SAMPLE_TYPE)(old_ep->fmin * scl);
	  new_ep->fmax = (MUS_SAMPLE_TYPE)(old_ep->fmax * scl);
	  for (i = 0; i < new_ep->amp_env_size; i++) 
	    {
	      new_ep->data_min[i] = (MUS_SAMPLE_TYPE)(old_ep->data_min[i] * scl);
	      new_ep->data_max[i] = (MUS_SAMPLE_TYPE)(old_ep->data_max[i] * scl);
	    }
	}
      else
	{
	  new_ep->fmax = (MUS_SAMPLE_TYPE)(old_ep->fmin * scl);
	  new_ep->fmin = (MUS_SAMPLE_TYPE)(old_ep->fmax * scl);
	  for (i = 0; i < new_ep->amp_env_size; i++) 
	    {
	      new_ep->data_max[i] = (MUS_SAMPLE_TYPE)(old_ep->data_min[i] * scl);
	      new_ep->data_min[i] = (MUS_SAMPLE_TYPE)(old_ep->data_max[i] * scl);
	    }
	}
      new_ep->completed = 1;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->amp_envs[cp->edit_ctr] = new_ep;
    }
}

void amp_env_scale_selection_by(chan_info *cp, Float scl, int beg, int num)
{
  env_info *old_ep, *new_ep;
  MUS_SAMPLE_TYPE fmax = MUS_SAMPLE_0, fmin = MUS_SAMPLE_0;
  int i, cursamp, start, end;
  old_ep = cp->amp_envs[cp->edit_ctr - 1];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = cp->amp_envs[cp->edit_ctr];
      if ((new_ep) && 
	  (new_ep->amp_env_size != old_ep->amp_env_size)) 
	new_ep = free_amp_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (env_info *)CALLOC(1, sizeof(env_info));
	  new_ep->data_max = (MUS_SAMPLE_TYPE *)MALLOC(old_ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE));
	  new_ep->data_min = (MUS_SAMPLE_TYPE *)MALLOC(old_ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE));
	}
      new_ep->amp_env_size = old_ep->amp_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      end = beg+num-1;
      start = beg - new_ep->samps_per_bin;
      for (i = 0, cursamp = 0; i < new_ep->amp_env_size; i++, cursamp+=new_ep->samps_per_bin) 
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
		      new_ep->data_max[i] = (MUS_SAMPLE_TYPE)(old_ep->data_max[i] * scl);
		      new_ep->data_min[i] = (MUS_SAMPLE_TYPE)(old_ep->data_min[i] * scl);
		    }
		  else
		    {
		      new_ep->data_max[i] = (MUS_SAMPLE_TYPE)(old_ep->data_min[i] * scl);
		      new_ep->data_min[i] = (MUS_SAMPLE_TYPE)(old_ep->data_max[i] * scl);
		    }
		}
	      else
		{
		  snd_fd *sf;
		  int n, segstart, segend;
		  MUS_SAMPLE_TYPE val, ymin, ymax;
		  /* here we have to read the current bin using the current fragments */
		  segstart = beg - cursamp;
		  if (segstart < 0) segstart = 0;
		  segend = cursamp + new_ep->samps_per_bin - end;
		  if (segend < 0) segend = new_ep->samps_per_bin;
		  sf = init_sample_read(cursamp, cp, READ_FORWARD);
		  if (sf == NULL) return;
		  ymin = MUS_SAMPLE_0;
		  ymax = MUS_SAMPLE_0;
		  for (n = 0; n < new_ep->samps_per_bin; n++)
		    {
		      val = next_sample(sf);
		      if ((n >= segstart) && 
			  (n <= segend)) 
			val = (MUS_SAMPLE_TYPE)(val * scl);
		      if (ymin > val) 
			ymin = val; 
		      else 
			if (ymax < val) 
			  ymax = val;
		    }
		  new_ep->data_max[i] = ymax;
		  new_ep->data_min[i] = ymin;
		  free_snd_fd(sf);
		}
	    }
	  if (fmin > new_ep->data_min[i]) fmin = new_ep->data_min[i];
	  if (fmax < new_ep->data_max[i]) fmax = new_ep->data_max[i];
	  fmin = MUS_SAMPLE_0;
	  fmax = MUS_SAMPLE_0;
	}
      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = 1;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->amp_envs[cp->edit_ctr] = new_ep;
    }
}

env_info *amp_env_copy(chan_info *cp, int reversed)
{
  env_info *old_ep, *new_ep = NULL;
  int i, j;
  old_ep = cp->amp_envs[cp->edit_ctr];
  if ((old_ep) && (old_ep->completed))
    {
      new_ep = (env_info *)CALLOC(1, sizeof(env_info));
      new_ep->data_max = (MUS_SAMPLE_TYPE *)MALLOC(old_ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE));
      new_ep->data_min = (MUS_SAMPLE_TYPE *)MALLOC(old_ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE));
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
	  for (i = 0; i < new_ep->amp_env_size; i++) 
	    {
	      new_ep->data_min[i] = old_ep->data_min[i];
	      new_ep->data_max[i] = old_ep->data_max[i];
	    }
	}
      new_ep->completed = 1;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      
    }
  return(new_ep);
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

Float srate_changed(Float val, char *srcbuf, int style, int tones)
{
  char *sfs;
  int semi, i, j;
  switch (style)
    {
    case SPEED_AS_RATIO: 
      for (i = 1; i < TOTAL_RATS; i++)
	if (rat_values[i] > val) 
	  break;
      sprintf(srcbuf, "%s", rat_names[i-1]);
      return(rat_values[i - 1]);
      break;
    case SPEED_AS_SEMITONE: 
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
  if (is_link(sp->fullname))
    {
      mus_snprintf(sname, PRINT_BUFFER_SIZE, "(%s)", sp->shortname);
      return(sname);
    }
  else return(sp->shortname);
}

char *shortname_indexed(snd_info *sp)
{
  if (show_indices(sp->state))
    {
      mus_snprintf(sname, PRINT_BUFFER_SIZE, "%d: %s", sp->index, shortname(sp));
      return(sname);
    }
  else return(shortname(sp));
}

void add_sound_data(char *filename, snd_info *sp, snd_state *ss, int graphed)
{
  int i;
  for (i = 0; i < sp->nchans; i++) 
    add_channel_data(filename, sp->chans[i], sp->hdr, ss, graphed);
}


static char *short_sound_format (int format, int type)
{
  switch (format)
    {
    case MUS_BSHORT:  if (type == MUS_RIFF) return("short swapped"); else return("short"); break;
    case MUS_LSHORT:  if (type == MUS_AIFC) return("short swapped"); else return("short"); break;
    case MUS_UBSHORT: 
    case MUS_ULSHORT: return("unsigned short"); break;
    case MUS_MULAW:   return("mulaw"); break;
    case MUS_BYTE:    return("byte"); break;
    case MUS_ALAW:    return("alaw"); break;
    case MUS_BFLOAT:  if (type == MUS_RIFF) return("float swapped"); else return("float"); break;
    case MUS_LFLOAT:  if (type == MUS_AIFC) return("float swapped"); else return("float"); break;
    case MUS_BINT:    if (type == MUS_RIFF) return("int swapped"); else return("int"); break;
    case MUS_LINT:    if (type == MUS_AIFC) return("int swapped"); else return("int"); break;
    case MUS_BINTN:   if (type == MUS_RIFF) return("normalized int swapped"); else return("normalized int"); break;
    case MUS_LINTN:   if (type == MUS_AIFC) return("normalized int swapped"); else return("normalized int"); break;
    case MUS_UBYTE:   return("unsigned byte"); break;
    case MUS_B24INT:  if (type == MUS_RIFF) return("24-bit swapped"); else return("24-bit"); break;
    case MUS_L24INT:  if (type == MUS_AIFC) return("24-bit swapped"); else return("24-bit"); break;
    case MUS_BDOUBLE: 
    case MUS_LDOUBLE: return("double"); break;
    case MUS_L12INT:  return("12-bit"); break;
    default:          return("unknown"); break;
    }
}

static char timebuf[TIME_STR_SIZE];
static char *link_file = NULL;

static char *linked_file(char *link_name)
{
  int bytes;
#if HAVE_READLINK
  if (link_file == NULL) link_file = (char *)CALLOC(128, sizeof(char));
  bytes = readlink(link_name, link_file, 128);
  if (bytes > 0)
    {
      link_file[bytes] = 0;
      return(link_file);
    }
  else 
#endif
    return("?");
}

static int dont_babble_info(snd_info *sp);

void sp_name_click(snd_info *sp)
{
  file_info *hdr;
  Float dur;
  int linked = 0;
  if (sp)
    {
      if (dont_babble_info(sp)) return;
      hdr = sp->hdr;
      if (hdr)
	{
	  linked = is_link(sp->fullname);
	  dur = (Float)(hdr->samples) / (Float)(hdr->chans * hdr->srate);
#if HAVE_STRFTIME
	  strftime(timebuf,
		   TIME_STR_SIZE,
		   STRFTIME_FORMAT,
		   localtime(&(sp->write_date)));
#else
	  sprintf(timebuf, "");
#endif
	  report_in_minibuffer(sp, "%d, %d chan%s, %.3f sec%s, %s:%s, %s%s%s%s",
			       hdr->srate,
			       hdr->chans,
			       ((hdr->chans > 1) ? "s" : ""),
			       dur,
			       ((dur == 1.0) ? "" : "s"),
			       mus_header_type_name(hdr->type),
			       short_sound_format(hdr->format, hdr->type),
			       timebuf,
			       (linked) ? ", (link to " : "",
			       (linked) ? linked_file(sp->fullname) : "",
			       (linked) ? ")" : "");
	}
    }
}



/* ---------------- save and restore control panel buttons ----------------*/

typedef struct {
  Float amp, srate, contrast, expand, revscl, revlen;
  env *filter_env;
  int expand_on, contrast_on, reverb_on, filter_on, direction, filter_order;
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

void save_control_panel(snd_info *sp) 
{
  ctrl_state *cs;
  cs = (ctrl_state *)(sp->saved_controls);
  if (!cs)
    {
      sp->saved_controls = (ctrl_state *)CALLOC(1, sizeof(ctrl_state));
      cs = (ctrl_state *)(sp->saved_controls);
    }
  cs->amp = sp->amp;
  cs->srate = sp->srate;
  cs->expand = sp->expand;
  cs->revscl = sp->revscl;
  cs->revlen = sp->revlen;
  cs->contrast = sp->contrast;
  cs->expand_on = sp->expanding;
  cs->reverb_on = sp->reverbing;
  cs->contrast_on = sp->contrasting;
  cs->filter_on = sp->filtering;
  cs->filter_order = sp->filter_order;
  if (sp->filter_env) 
    {
      if (cs->filter_env) cs->filter_env = free_env(cs->filter_env);
      cs->filter_env = copy_env(sp->filter_env);
    }
  if (sp->play_direction == 1) 
    cs->direction = 0; 
  else cs->direction = 1;
}

void restore_control_panel(snd_info *sp) 
{
  ctrl_state *cs;
  cs = (ctrl_state *)(sp->saved_controls);
  if (!cs) 
    {
      sp->saved_controls = (ctrl_state *)CALLOC(1, sizeof(ctrl_state));
      cs = (ctrl_state *)(sp->saved_controls);
      cs->amp = DEFAULT_AMP;
      cs->srate = DEFAULT_SPEED;
      cs->direction = 0; /* 0 = forward, 1 = backward (this is the button's view) */
      cs->expand = DEFAULT_EXPAND;
      cs->expand_on = DEFAULT_EXPANDING;
      cs->revscl = DEFAULT_REVERB_SCALE;
      cs->revlen = DEFAULT_REVERB_LENGTH;
      cs->reverb_on = DEFAULT_REVERBING;
      cs->contrast = DEFAULT_CONTRAST;
      cs->contrast_on = DEFAULT_CONTRASTING;
      cs->filter_on = DEFAULT_FILTERING;
      cs->filter_order = DEFAULT_FILTER_ORDER;
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
  if (sp->filter_env) sp->filter_env = free_env(sp->filter_env); 
  if (cs->filter_env) 
    sp->filter_env = copy_env(cs->filter_env);
  else sp->filter_env = default_env(sp->filter_env_xmax, 1.0);
  set_snd_filter_order(sp, cs->filter_order);
}

void reset_control_panel(snd_info *sp) 
{
  toggle_expand_button(sp, DEFAULT_EXPANDING);
  toggle_contrast_button(sp, DEFAULT_CONTRASTING);
  toggle_reverb_button(sp, DEFAULT_REVERBING);
  toggle_filter_button(sp, DEFAULT_FILTERING);
  toggle_direction_arrow(sp, 0);
  set_snd_amp(sp, DEFAULT_AMP);
  set_snd_srate(sp, DEFAULT_SPEED);
  set_snd_contrast(sp, DEFAULT_CONTRAST);
  set_snd_expand(sp, DEFAULT_EXPAND);
  set_snd_revscl(sp, DEFAULT_REVERB_SCALE);
  set_snd_revlen(sp, DEFAULT_REVERB_LENGTH);
  set_snd_filter_order(sp, DEFAULT_FILTER_ORDER);
  if (sp->filter_env) sp->filter_env = free_env(sp->filter_env);
  sp->filter_env = default_env(sp->filter_env_xmax, 1.0);
}


/* ---------------- minibuffer/filter text history ---------------- */

typedef struct {
  char **strings;
  int strings_size, strings_pos, first_time;
} mini_history;
  
enum {MINIBUFFER, FILTER_TEXT};

static void remember_string(snd_info *sp, char *str, int which)
{
  mini_history *mh = NULL;
  int i, top;
  switch (which)
    {
    case MINIBUFFER: mh = (mini_history *)(sp->minibuffer_history); break;
    case FILTER_TEXT: mh = (mini_history *)(sp->filter_history); break;
    default: return; break;
    }
  if (mh == NULL)
    {
      mh = (mini_history *)CALLOC(1, sizeof(mini_history));
      mh->strings_size = minibuffer_history_length(sp->state);
      mh->strings = (char **)CALLOC(mh->strings_size, sizeof(char *));
      switch (which)
	{
	case MINIBUFFER: sp->minibuffer_history = (void *)mh; break;
	case FILTER_TEXT: sp->filter_history = (void *)mh; break;
	}
    }
  top = mh->strings_size - 1;
  if (mh->strings[top]) FREE(mh->strings[top]);
  for (i = top; i > 0; i--) mh->strings[i] = mh->strings[i-1];
  mh->strings[0] = copy_string(str);
  mh->strings_pos = 0;
  mh->first_time = 1;
}

void remember_mini_string(snd_info *sp, char *str) {remember_string(sp, str, MINIBUFFER);}
void remember_filter_string(snd_info *sp, char *str) {remember_string(sp, str, FILTER_TEXT);}

static void restore_string(snd_info *sp, int back, int which)
{
  mini_history *mh = NULL;
  char *str;
  switch (which)
    {
    case MINIBUFFER: mh = (mini_history *)(sp->minibuffer_history); break;
    case FILTER_TEXT: mh = (mini_history *)(sp->filter_history); break;
    }
  if (mh)
    {
      if (mh->first_time == 0)
	{
	  if (back)
	    mh->strings_pos++;
	  else mh->strings_pos--;
	}
      mh->first_time = 0;
      if (mh->strings_pos < 0) mh->strings_pos = 0;
      if (mh->strings_pos > (mh->strings_size - 1)) mh->strings_pos = mh->strings_size - 1;
      str = mh->strings[mh->strings_pos];
      if (str)
	{
	  switch (which)
	    {
	    case MINIBUFFER: set_minibuffer_string(sp, str); break;
	    case FILTER_TEXT: set_filter_text(sp, str); break;
	    }
	}
    }
}

void restore_mini_string(snd_info *sp, int back) {restore_string(sp, back, MINIBUFFER);}
void restore_filter_string(snd_info *sp, int back) {restore_string(sp, back, FILTER_TEXT);}

static void clear_strings(snd_info *sp, int which)
{
  mini_history *mh = NULL;
  int i;
  switch (which)
    {
    case MINIBUFFER: mh = (mini_history *)(sp->minibuffer_history); break;
    case FILTER_TEXT: mh = (mini_history *)(sp->filter_history); break;
    }
  if (mh)
    {
      switch (which)
	{
	case MINIBUFFER: sp->minibuffer_history = NULL; break;
	case FILTER_TEXT: sp->filter_history = NULL; break;
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


/* ---------------- control panel apply button ---------------- */

void stop_applying(snd_info *sp)
{
  /* called if user unset the apply button during the apply process */
  sp->apply_ok = 0;
}

typedef struct {
  int slice;
  snd_info *sp;
  int i;
  int ofd;
  char *ofile;
  ctrl_state *cs;
  file_info *hdr;
} apply_state;

void *make_apply_state(void *xp)
{
  /* set up initial state for apply_controls */
  apply_state *ap;
  ap = (apply_state *)CALLOC(1, sizeof(apply_state));
  ap->slice = 0;
  ap->hdr = NULL;
  ap->sp = (snd_info *)xp;
  return((void *)ap);
}

#define APPLY_TICKS 4

static int max_sync(snd_info *sp, void *val)
{
  int *maxsync = (int *)val;
  if (sp->sync > maxsync[0])
    maxsync[0] = sp->sync;
  return(0);
}

static int apply_dur, apply_tick = 0, apply_reporting = 0, orig_dur;

BACKGROUND_TYPE apply_controls(GUI_POINTER ptr)
{
  apply_state *ap = (apply_state *)ptr;
  snd_state *ss;
  snd_context *sgx;
  snd_info *sp;
  chan_info *cp;
  sync_info *si;
  Float ratio, mult_dur;
  int i, len, over_selection, curchan = 0, added_dur = 0, old_sync;
  int maxsync[1];
  Float scaler[1];
  if (ptr == NULL) return(BACKGROUND_QUIT);
  sp = ap->sp;
  if (!(sp->active)) return(BACKGROUND_QUIT);
  ss = sp->state;
  if (sp->filtering) added_dur = sp->filter_order;
  mult_dur = 1.0 / fabs(sp->srate);
  if (sp->expanding) mult_dur *= sp->expand;
  if (sp->reverbing) added_dur += (int)((SND_SRATE(sp) * reverb_decay(ss)));
  if ((ss->apply_choice != APPLY_TO_SELECTION) &&
      (sp->srate == 1.0) && 
      (sp->play_direction == 1) &&
      (!(sp->filtering)) && (!(sp->expanding)) && (!(sp->reverbing)) && (!(sp->contrasting)))
    {
      old_sync = sp->sync;
      if (ss->apply_choice == APPLY_TO_SOUND)
	{
	  maxsync[0] = 0;
	  map_over_sounds(ss, max_sync, (void *)maxsync);
	  sp->sync = maxsync[0] + 1;
	}
      else sp->sync = 0;
      scaler[0] = sp->amp;
      scale_by((sp->selected_channel == NO_SELECTION) ? sp->chans[0] : sp->chans[sp->selected_channel], 
	       scaler, 1, FALSE);
      sp->sync = old_sync;
    }
  else
    {
      switch (ap->slice)
	{
	case 0: 
	  ap->ofile = NULL;
	  lock_apply(ss, sp);
	  ap->ofile = snd_tempnam(ss);
	  ap->hdr = make_temp_header(ap->ofile, SND_SRATE(sp), sp->nchans, 0);
	  switch (ss->apply_choice)
	    {
	    case APPLY_TO_CHANNEL:   
	      ap->hdr->chans = 1; 
	      if (sp->selected_channel != NO_SELECTION) 
		curchan = sp->selected_channel;
	      apply_dur = current_ed_samples(sp->chans[curchan]);
	      break;
	    case APPLY_TO_SOUND:     
	      ap->hdr->chans = sp->nchans; 
	      apply_dur = current_ed_samples(sp->chans[0]); 
	      break;
	    case APPLY_TO_SELECTION: 
	      ap->hdr->chans = region_chans(0); 
	      apply_dur = selection_len(); 
	      break;
	    }
	  orig_dur = apply_dur;
	  apply_dur = (int)(mult_dur * (apply_dur + added_dur));
	  ap->ofd = open_temp_file(ap->ofile, ap->hdr->chans, ap->hdr, ss);
	  sp->apply_ok = 1;
	  initialize_apply(sp, ap->hdr->chans, apply_dur);
	  apply_reporting = (apply_dur > (MAX_BUFFER_SIZE * 4));
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
		  apply_reporting = 0;
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
			  progress_report(sp, "apply", 1, 1, (Float)(ap->i) / (Float)apply_dur, NOT_FROM_ENVED);
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
			  apply_dur * (ap->hdr->chans) * mus_data_format_to_bytes_per_sample((ap->hdr)->format),
			  sp);
	  if (sp->apply_ok)
	    {
	      switch (ss->apply_choice)
		{
		case APPLY_TO_SOUND:
		  if (sp->nchans > 1) 
		    remember_temp(ap->ofile, sp->nchans);
		  for (i = 0; i < sp->nchans; i++)
		    file_override_samples(apply_dur, ap->ofile, sp->chans[i], i,
					  (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
					  LOCK_MIXES, "Apply");
		  break;
		case APPLY_TO_CHANNEL: 
		  if (sp->selected_channel != NO_SELECTION) 
		    curchan = sp->selected_channel;
		  file_override_samples(apply_dur, ap->ofile, sp->chans[curchan], 0, DELETE_ME, LOCK_MIXES, "Apply to channel");
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
					      LOCK_MIXES, "Apply to selection");
			  update_graph(si->cps[i], NULL);
			}
		    }
		  else
		    {
		      int ok;
		      ok = delete_selection(S_src_selection, DONT_UPDATE_DISPLAY);
		      for (i = 0; i < si->chans; i++)
			{
			  file_insert_samples(si->begs[i], apply_dur, ap->ofile, si->cps[i], 0, DELETE_ME, "Apply to selection");
			  reactivate_selection(si->cps[i], si->begs[i], si->begs[i] + apply_dur);
			  if (ok) backup_edit_list(si->cps[i]);
			}
		    }
		  si = free_sync_info(si); 
		  break;
		}
	      clear_minibuffer(sp);
	      set_apply_button(sp, FALSE);
	      sp->apply_ok = 0;
	      
	      if ((sp->expanding) || 
		  (sp->play_direction != 1) || (sp->srate != 1.0))
		{
		  for (i = 0; i < sp->nchans; i++)
		    {
		      cp = sp->chans[i];
		      if (cp->marks)
			{
			  if (!(sp->expanding))
			    ratio = sp->srate;
			  else ratio = sp->srate / sp->expand;
			  if (ratio != 1.0)
			    {
			      over_selection = (ss->apply_choice == APPLY_TO_SELECTION);
			      src_marks(cp, ratio, orig_dur, apply_dur, 
					(over_selection) ? selection_beg(cp) : 0,
					over_selection);
			      update_graph(cp, NULL);
			    }
			}
		    }
		}
	    }
	  else
	    {
	      remove(ap->ofile);
	      mus_sound_forget(ap->ofile);
	      report_in_minibuffer(sp, "apply flushed!");
	    }
	  FREE(ap->ofile);
	  ap->ofile = NULL;
	  if (ap->hdr) ap->hdr = free_file_info(ap->hdr);
	  free_controls(sp);
	  break;
	}
    }
  unlock_apply(ss, sp);
  reset_control_panel(sp); /* i.e. clear it */
  sp->applying = 0;
  sgx = sp->sgx;
  if (sgx->apply_in_progress) sgx->apply_in_progress = 0;
  FREE(ap);
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

static void run_apply_to_completion(snd_info *sp)
{
  /* this version called from Guile, so we want it to complete before returning */
  apply_state *ap;
  if (sp)
    {
      sp->applying = 1;
      ap = (apply_state *)make_apply_state((void *)sp);
      while (apply_controls((GUI_POINTER)ap) == BACKGROUND_CONTINUE);
    }
}

static void set_reverb_decay(snd_state *ss, Float val) 
{
  int i;
  snd_info *sp;
  in_set_reverb_decay(ss, val);
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse))
	sp->reverb_decay = val;
    }
}

static int map_sounds_speed_tones(snd_info *sp, void *val) 
{
  sp->speed_tones = (*((int *)val)); 
  return(0);
}

static void set_speed_tones(snd_state *ss, int val) 
{
  in_set_speed_tones(ss, val); 
  map_over_sounds(ss, map_sounds_speed_tones, (void *)(&val));
}      

static int map_sounds_speed_style(snd_info *sp, void *val) 
{
  sp->speed_style = (*((int *)val)); 
  return(0);
}

void set_speed_style(snd_state *ss, int val) 
{
  in_set_speed_style(ss, val); 
  map_over_sounds(ss, map_sounds_speed_style, (void *)(&val));
}      

SCM snd_no_such_sound_error(const char *caller, SCM n)
{
  ERROR(NO_SUCH_SOUND,
	SCM_LIST2(TO_SCM_STRING(caller),
		  n));
  return(SCM_BOOL_F);
}

static SCM g_soundQ(SCM snd_n)
{
  #define H_soundQ "(" S_soundQ " &optional (index 0)) -> #t if sound associated with index is active (accessible)"
  snd_info *sp;
  sp = get_sp(snd_n);
  return(TO_SCM_BOOLEAN((sp) && snd_ok(sp)));
}

static SCM g_select_sound(SCM snd_n)
{
  #define H_select_sound "(" S_select_sound " &optional snd) makes snd the selected (active) sound"
  int val;
  snd_state *ss;
  snd_info *sp;
  ss = get_global_state();
  SND_ASSERT_SND(S_select_sound, snd_n, 1);
  val = TO_C_INT_OR_ELSE(snd_n, 0);
  if ((val >= 0) && 
      (val < ss->max_sounds))
    {
      sp = ss->sounds[val];
      if (snd_ok(sp))
	{
	  select_channel(sp, 0);
	  normalize_sound(ss, sp, sp->chans[0]);
	  map_over_chans(ss, update_graph, NULL);
	  return(snd_n);
	}
    }
  return(snd_no_such_sound_error(S_select_sound, snd_n));
}

static SCM g_select_channel(SCM chn_n)
{
  #define H_select_channel "(" S_select_channel " &optional chn) makes chn the selected (active) channel"
  snd_info *sp;
  snd_state *ss;
  int chan;
  ss = get_global_state();
  SND_ASSERT_SND(S_select_channel, chn_n, 1);
  chan = TO_C_INT_OR_ELSE(chn_n, 0);
  sp = any_selected_sound(ss);
  if ((sp) && 
      (chan < sp->nchans)) 
    {
      select_channel(sp, chan);
      return(chn_n);
    }
  return(snd_no_such_channel_error(S_select_channel, TO_SCM_STRING("selected-sound"), chn_n));
}

static SCM g_find_sound(SCM filename)
{
  #define H_find_sound "(" S_find_sound " name) returns the id of the sound associated with file 'name'"
  snd_state *ss;
  snd_info *sp;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_find_sound, "a string");
  ss = get_global_state();
  sp = find_sound(ss, TO_C_STRING(filename));
  if (sp) return(TO_SCM_INT(sp->index));
  return(scm_return_first(SCM_BOOL_F, filename));
}


static SCM g_bomb(SCM snd, SCM on)
{
  #define H_bomb "(" S_bomb " &optional snd (on #t)) displays (or erases if on=#f) the bomb icon"
  snd_info *sp;
  SND_ASSERT_SND(S_bomb, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_bomb, snd));
  x_bomb(sp, TO_C_BOOLEAN_OR_T(on));
  return(on);
}

enum {SP_SYNC, SP_UNITE, SP_READ_ONLY, SP_NCHANS, SP_CONTRASTING, SP_EXPANDING, SP_REVERBING, SP_FILTERING, SP_FILTER_ORDER,
      SP_SRATE, SP_DATA_FORMAT, SP_DATA_LOCATION, SP_HEADER_TYPE, SP_CONTROL_PANEL_SAVE, SP_CONTROL_PANEL_RESTORE, SP_SELECTED_CHANNEL,
      SP_COMMENT, SP_FILE_NAME, SP_SHORT_FILE_NAME, SP_CLOSE, SP_UPDATE, SP_SAVE, SP_CURSOR_FOLLOWS_PLAY, SP_SHOW_CONTROLS,
      SP_FILTER_DBING, SP_SPEED_TONES, SP_SPEED_STYLE, SP_CONTROL_PANEL_RESET
};

static SCM sp_iread(SCM snd_n, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  char *str;
  int i;
  SCM res = SCM_EOL;
  if (SCM_EQ_P(snd_n, SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = CONS(sp_iread(TO_SMALL_SCM_INT(i), fld, caller), res);
	}
      return(REVERSE_LIST(res));
    }
  SND_ASSERT_SND(caller, snd_n, 1);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    return(snd_no_such_sound_error(caller, snd_n));
  switch (fld)
    {
    case SP_SYNC:                  return(TO_SCM_INT(sp->sync));                 break;
    case SP_UNITE:                 return(TO_SCM_INT(sp->combining));               break;
    case SP_READ_ONLY:             return(TO_SCM_BOOLEAN(sp->read_only));           break;
    case SP_NCHANS:                return(TO_SCM_INT(sp->nchans));                  break;
    case SP_EXPANDING:             return(TO_SCM_BOOLEAN(sp->expanding));           break;
    case SP_CONTRASTING:           return(TO_SCM_BOOLEAN(sp->contrasting));         break;
    case SP_REVERBING:             return(TO_SCM_BOOLEAN(sp->reverbing));           break;
    case SP_FILTERING:             return(TO_SCM_BOOLEAN(sp->filtering));           break;
    case SP_FILTER_DBING:          return(TO_SCM_BOOLEAN(sp->filter_dBing));        break;
    case SP_FILTER_ORDER:          return(TO_SCM_INT(sp->filter_order));            break;
    case SP_SRATE:                 return(TO_SCM_INT((sp->hdr)->srate));            break;
    case SP_DATA_FORMAT:           return(TO_SCM_INT((sp->hdr)->format));           break;
    case SP_HEADER_TYPE:           return(TO_SCM_INT((sp->hdr)->type));             break;
    case SP_DATA_LOCATION:         return(TO_SCM_INT((sp->hdr)->data_location));    break;
    case SP_CONTROL_PANEL_SAVE:    save_control_panel(sp);                          break;
    case SP_CONTROL_PANEL_RESTORE: restore_control_panel(sp);                       break;
    case SP_CONTROL_PANEL_RESET:   reset_control_panel(sp);                         break;
    case SP_SELECTED_CHANNEL:      return(TO_SCM_INT(sp->selected_channel));        break;
    case SP_FILE_NAME:             return(TO_SCM_STRING(sp->fullname));             break;
    case SP_SHORT_FILE_NAME:       return(TO_SCM_STRING(sp->shortname));            break;
    case SP_CLOSE:                 snd_close_file(sp, sp->state);                   break;
    case SP_SAVE:                  save_edits(sp, NULL);                            break;
    case SP_UPDATE:                snd_update(sp->state, sp);                       break;
    case SP_CURSOR_FOLLOWS_PLAY:   return(TO_SCM_BOOLEAN(sp->cursor_follows_play)); break;
    case SP_SHOW_CONTROLS:         return(TO_SCM_BOOLEAN(control_panel_open(sp)));  break;
    case SP_SPEED_TONES:           return(TO_SCM_INT(sp->speed_tones));             break;
    case SP_SPEED_STYLE:           return(TO_SCM_INT(sp->speed_style));             break;
    case SP_COMMENT:
      str = mus_sound_comment(sp->fullname);
      res = TO_SCM_STRING(str);
      if (str) FREE(str);
      return(res);
      break;
    }
  return(SCM_BOOL_F);
}

static SCM sp_iwrite(SCM snd_n, SCM val, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  char *com;
  int i, ival;
  if (SCM_EQ_P(snd_n, SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    sp_iwrite(TO_SMALL_SCM_INT(i), val, fld, caller);
	}
      return(val);
    }
  SND_ASSERT_SND(caller, snd_n, 2); /* 2 from caller's point of view */
  sp = get_sp(snd_n);
  if (sp == NULL) 
    return(snd_no_such_sound_error(caller, snd_n));
  ss = sp->state;
  switch (fld)
    {
    case SP_SYNC:  
      if (NUMBER_P(val))
	syncb(sp, TO_C_INT_OR_ELSE_WITH_ORIGIN(val, 1, caller));
      else syncb(sp, TO_C_BOOLEAN(val));
      break;
    case SP_UNITE:      
      if (NUMBER_P(val))    
	combineb(sp, TO_C_INT_OR_ELSE_WITH_ORIGIN(val, 1, caller));
      else combineb(sp, TO_C_BOOLEAN(val));
      break;
    case SP_READ_ONLY:
      sp->read_only = TO_C_BOOLEAN_OR_T(val); 
      snd_file_lock_icon(sp, sp->read_only); 
      break;
    case SP_EXPANDING:
      toggle_expand_button(sp, TO_C_BOOLEAN_OR_T(val));
      break;
    case SP_CONTRASTING:
      toggle_contrast_button(sp, TO_C_BOOLEAN_OR_T(val));
      break;
    case SP_REVERBING:
      toggle_reverb_button(sp, TO_C_BOOLEAN_OR_T(val));
      break;
    case SP_FILTERING:
      toggle_filter_button(sp, TO_C_BOOLEAN_OR_T(val));
      break;
    case SP_FILTER_DBING:   
      set_filter_dBing(sp, TO_C_BOOLEAN_OR_T(val));
      break;
    case SP_FILTER_ORDER:
      set_snd_filter_order(sp, TO_C_INT(val));
      break;
    case SP_CURSOR_FOLLOWS_PLAY:
      sp->cursor_follows_play = TO_C_BOOLEAN_OR_T(val);
      break;
    case SP_SHOW_CONTROLS:
      if (TO_C_BOOLEAN_OR_T(val))
	sound_show_ctrls(sp); 
      else sound_hide_ctrls(sp); 
      break;
    case SP_SPEED_TONES:
      sp->speed_tones = TO_C_INT(val);
      if (sp->speed_tones <= 0) 
	sp->speed_tones = DEFAULT_SPEED_TONES;
      break;
    case SP_SPEED_STYLE:
      sp->speed_style = mus_iclamp(0, TO_C_INT(val), MAX_SPEED_STYLE);
      break;
    case SP_SRATE:
      mus_sound_set_srate(sp->fullname, TO_C_INT(val));
      snd_update(ss, sp); 
      break;
    case SP_NCHANS: 
      mus_sound_set_chans(sp->fullname, TO_C_INT(val));
      snd_update(ss, sp); 
      break;
    case SP_DATA_FORMAT:
      ival = TO_C_INT(val);
      if (MUS_DATA_FORMAT_OK(ival))
	{
	  mus_sound_set_data_format(sp->fullname, ival);
	  snd_update(ss, sp);
	}
      else mus_misc_error("set-" S_data_format, "unknown data format", val);
      break;
    case SP_HEADER_TYPE:
      ival = TO_C_INT(val);
      if (MUS_HEADER_TYPE_OK(ival))
	{
	  mus_sound_set_header_type(sp->fullname, ival);
	  snd_update(ss, sp); 
	}
      else mus_misc_error("set-" S_header_type, "unknown header type", val);
      break;
    case SP_DATA_LOCATION:  
      mus_sound_set_data_location(sp->fullname, TO_C_INT(val));
      snd_update(ss, sp); 
      break;
    case SP_COMMENT:      
      /* this is safe only with aifc and riff headers */
      com = TO_NEW_C_STRING(val);
      mus_header_update_comment(sp->fullname, 0, com, snd_strlen(com), mus_sound_header_type(sp->fullname));
      free(com);
      snd_update(ss, sp);
      break;
    }
  return(val);
}

static SCM g_channels(SCM snd_n)
{
  #define H_channels "("  S_channels " &optional snd) how many channels snd has"
  return(sp_iread(snd_n, SP_NCHANS, S_channels));
}

static SCM check_number(SCM val, char *caller)
{
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, caller, "a number");
  return(val);
}

static SCM g_set_channels(SCM snd_n, SCM val)
{
  if (NOT_BOUND_P(val))
    return(sp_iwrite(SCM_UNDEFINED, check_number(snd_n, "set-" S_channels), SP_NCHANS, "set-" S_channels));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_channels), SP_NCHANS, "set-" S_channels));
}

static SCM g_srate(SCM snd_n) 
{
  #define H_srate "(" S_srate " &optional snd) -> snd's srate"
  return(sp_iread(snd_n, SP_SRATE, S_srate));
}

static SCM g_set_srate(SCM snd_n, SCM val) 
{
  if (NOT_BOUND_P(val))
    return(sp_iwrite(SCM_UNDEFINED, check_number(snd_n, "set-" S_srate), SP_SRATE, "set-" S_srate));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_srate), SP_SRATE, "set-" S_srate));
}

static SCM g_data_location(SCM snd_n) 
{
  #define H_data_location "(" S_data_location " &optional snd) -> snd's data location"
  return(sp_iread(snd_n, SP_DATA_LOCATION, S_data_location));
}

static SCM g_set_data_location(SCM snd_n, SCM val) 
{
  if (NOT_BOUND_P(val))
    return(sp_iwrite(SCM_UNDEFINED, check_number(snd_n, "set-" S_data_location), SP_DATA_LOCATION, "set-" S_data_location));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_data_location), SP_DATA_LOCATION, "set-" S_data_location));
}

static SCM g_data_format(SCM snd_n) 
{
  #define H_data_format "(" S_data_format " &optional snd) -> snd's data format (e.g. mus-bshort)"
  return(sp_iread(snd_n, SP_DATA_FORMAT, S_data_format));
}

static SCM g_set_data_format(SCM snd_n, SCM val) 
{
  if (NOT_BOUND_P(val))
    return(sp_iwrite(SCM_UNDEFINED, check_number(snd_n, "set-" S_data_format), SP_DATA_FORMAT, "set-" S_data_format));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_data_format), SP_DATA_FORMAT, "set-" S_data_format));
}

static SCM g_header_type(SCM snd_n) 
{
  #define H_header_type "(" S_header_type " &optional snd) -> snd's header type (e.g. mus-aiff)"
  return(sp_iread(snd_n, SP_HEADER_TYPE, S_header_type));
}

static SCM g_set_header_type(SCM snd_n, SCM val) 
{
  if (NOT_BOUND_P(val))
    return(sp_iwrite(SCM_UNDEFINED, check_number(snd_n, "set-" S_header_type), SP_HEADER_TYPE, "set-" S_header_type));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_header_type), SP_HEADER_TYPE, "set-" S_header_type));
}

static SCM g_comment(SCM snd_n)
{
  #define H_comment "(" S_comment " &optional snd) -> snd's comment (in its header)"
  return(sp_iread(snd_n, SP_COMMENT, S_comment));
}

static SCM g_set_comment(SCM snd_n, SCM val) 
{
  if (NOT_BOUND_P(val))
    return(sp_iwrite(SCM_UNDEFINED, snd_n, SP_COMMENT, "set-" S_comment));
  else return(sp_iwrite(snd_n, val, SP_COMMENT, "set-" S_comment));
}


#define WITH_REVERSED_BOOLEAN_ARGS(name_reversed, name) \
static SCM name_reversed(SCM arg1, SCM arg2) \
{ \
  if (NOT_BOUND_P(arg1)) \
    return(name(SCM_BOOL_T, SCM_UNDEFINED)); \
  else \
    if (NOT_BOUND_P(arg2)) \
      return(name(arg1, SCM_UNDEFINED)); \
    else return(name(arg2, arg1)); \
}

#define WITH_REVERSED_ARGS(name_reversed, name) \
static SCM name_reversed(SCM arg1, SCM arg2) \
{ \
  if (NOT_BOUND_P(arg2)) \
    return(name(arg1, SCM_UNDEFINED)); \
  else return(name(arg2, arg1)); \
}


static SCM g_sync(SCM snd_n) 
{
  #define H_sync "(" S_sync " &optional snd) -> whether snd is sync'd to other sounds"
  return(sp_iread(snd_n, SP_SYNC, S_sync));
}

static SCM g_set_sync(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_sync, "an integer");
  return(sp_iwrite(snd_n, on, SP_SYNC, "set-" S_sync));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_sync_reversed, g_set_sync)

static SCM g_uniting(SCM snd_n) 
{
  #define H_uniting "(" S_uniting " &optional snd) -> whether snd's channels are conbined into one graph"
  return(sp_iread(snd_n, SP_UNITE, S_uniting));
}

static SCM g_set_uniting(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_uniting, "an integer");
  return(sp_iwrite(snd_n, on, SP_UNITE, "set-" S_uniting));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_uniting_reversed, g_set_uniting)

static SCM g_read_only(SCM snd_n) 
{
  #define H_read_only "(" S_read_only " &optional snd) -> whether snd is write-protected"
  return(sp_iread(snd_n, SP_READ_ONLY, S_read_only));
}

static SCM g_set_read_only(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_read_only, "a boolean");
  return(sp_iwrite(snd_n, on, SP_READ_ONLY, "set-" S_read_only));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_read_only_reversed, g_set_read_only)

static SCM g_contrasting(SCM snd_n) 
{
  #define H_contrasting "(" S_contrasting " &optional snd) -> snd's control panel constrast button state"
  return(sp_iread(snd_n, SP_CONTRASTING, S_contrasting));
}

static SCM g_set_contrasting(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_contrasting, "a boolean");
  return(sp_iwrite(snd_n, on, SP_CONTRASTING, "set-" S_contrasting));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_contrasting_reversed, g_set_contrasting)

static SCM g_expanding(SCM snd_n) 
{
  #define H_expanding "(" S_expanding " &optional snd) -> snd's control panel expand button state"
  return(sp_iread(snd_n, SP_EXPANDING, S_expanding));
}

static SCM g_set_expanding(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_expanding, "a boolean");
  return(sp_iwrite(snd_n, on, SP_EXPANDING, "set-" S_expanding));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_expanding_reversed, g_set_expanding)

static SCM g_reverbing(SCM snd_n) 
{
  #define H_reverbing "(" S_reverbing " &optional snd) -> snd's control panel reverb button state"
  return(sp_iread(snd_n, SP_REVERBING, S_reverbing));
}

static SCM g_set_reverbing(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_reverbing, "a boolean");
  return(sp_iwrite(snd_n, on, SP_REVERBING, "set-" S_reverbing));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_reverbing_reversed, g_set_reverbing)

static SCM g_filtering(SCM snd_n) 
{
  #define H_filtering "(" S_filtering " &optional snd) -> snd's control panel filter button state"
  return(sp_iread(snd_n, SP_FILTERING, S_filtering));
}

static SCM g_set_filtering(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_filtering, "a boolean");
  return(sp_iwrite(snd_n, on, SP_FILTERING, "set-" S_filtering));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_filtering_reversed, g_set_filtering)

static SCM g_filter_dBing(SCM snd_n) 
{
  #define H_filter_dBing "(" S_filter_dBing " &optional snd) -> #t if snd's filter envelope is displayed in dB in control panel"
  return(sp_iread(snd_n, SP_FILTER_DBING, S_filter_dBing));
}

static SCM g_set_filter_dBing(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_filter_dBing, "a boolean");
  return(sp_iwrite(snd_n, on, SP_FILTER_DBING, "set-" S_filter_dBing));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_filter_dBing_reversed, g_set_filter_dBing)

static SCM g_filter_order(SCM snd_n) 
{
  #define H_filter_order "(" S_filter_order " &optional snd) -> filter order (in control panel)"
  return(sp_iread(snd_n, SP_FILTER_ORDER, S_filter_order));
}

static SCM g_set_filter_order(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(INTEGER_P(on), on, SCM_ARG1, "set-" S_filter_order, "an integer"); 
  return(sp_iwrite(snd_n, on, SP_FILTER_ORDER, "set-" S_filter_order));
}

WITH_REVERSED_ARGS(g_set_filter_order_reversed, g_set_filter_order)

static SCM g_cursor_follows_play(SCM snd_n) 
{
  #define H_cursor_follows_play "("  S_cursor_follows_play " &optional snd) -> #t if cursor moves along in waveform display as sound is played (#f)"
  return(sp_iread(snd_n, SP_CURSOR_FOLLOWS_PLAY, S_cursor_follows_play));
}

static SCM g_set_cursor_follows_play(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_cursor_follows_play, "a boolean");
  return(sp_iwrite(snd_n, on, SP_CURSOR_FOLLOWS_PLAY, "set-" S_cursor_follows_play));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_cursor_follows_play_reversed, g_set_cursor_follows_play)

static SCM g_show_controls(SCM snd_n) 
{
  #define H_show_controls "(" S_show_controls " &optional snd) -> #t if snd's control panel is known to be open"
  return(sp_iread(snd_n, SP_SHOW_CONTROLS, S_show_controls));
}

static SCM g_set_show_controls(SCM on, SCM snd_n)
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_show_controls, "a boolean");
  return(sp_iwrite(snd_n, on, SP_SHOW_CONTROLS, "set-" S_show_controls));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_show_controls_reversed, g_set_show_controls)

static SCM g_save_control_panel(SCM snd_n) 
{
  #define H_save_control_panel "(" S_save_control_panel " &optional snd) saves the current control panel settings for subsequent " S_restore_control_panel
  return(sp_iread(snd_n, SP_CONTROL_PANEL_SAVE, S_save_control_panel));
}

static SCM g_restore_control_panel(SCM snd_n) 
{
  #define H_restore_control_panel "(" S_restore_control_panel " &optional snd) restores the previously saved control panel settings"
  return(sp_iread(snd_n, SP_CONTROL_PANEL_RESTORE, S_restore_control_panel));
}

static SCM g_reset_control_panel(SCM snd_n) 
{
  #define H_reset_control_panel "(" S_reset_control_panel " &optional snd) resets (clears) the control panel settings"
  return(sp_iread(snd_n, SP_CONTROL_PANEL_RESET, S_reset_control_panel));
}

static SCM g_selected_channel(SCM snd_n) 
{
  #define H_selected_channel "(" S_selected_channel " &optional snd) -> currently selected channel in snd"
  return(sp_iread(snd_n, SP_SELECTED_CHANNEL, S_selected_channel));
}

static SCM g_set_selected_channel(SCM snd_n, SCM chn_n) 
{
  snd_info *sp;
  int chan;
  if (NOT_BOUND_P(chn_n))
    return(g_select_channel(snd_n));
  else
    {
      SND_ASSERT_SND("set-" S_selected_channel, snd_n, 1); 
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error("set-" S_selected_channel, snd_n));
      chan = TO_C_INT_OR_ELSE(chn_n, 0);
      if ((sp) && (chan < sp->nchans)) 
	{
	  select_channel(sp, chan);
	  return(chn_n);
	}
    }
  return(snd_no_such_channel_error("set-" S_selected_channel, snd_n, chn_n));
}

static SCM g_file_name(SCM snd_n) 
{
  #define H_file_name "(" S_file_name " &optional snd) -> snd's full filename"
  return(sp_iread(snd_n, SP_FILE_NAME, S_file_name));
}

static SCM g_short_file_name(SCM snd_n) 
{
  #define H_short_file_name "(" S_short_file_name " &optional snd) -> short form of snd's file name (no directory)"
  return(sp_iread(snd_n, SP_SHORT_FILE_NAME, S_short_file_name));
}

static SCM g_close_sound(SCM snd_n) 
{
  #define H_close_sound "(" S_close_sound " snd) closes snd"
  return(sp_iread(snd_n, SP_CLOSE, S_close_sound));
}

static SCM g_update_sound(SCM snd_n) 
{
  #define H_update_sound "(" S_update_sound " snd) updates snd (re-reads from disk flushing pending edits)"
  return(sp_iread(snd_n, SP_UPDATE, S_update_sound));
}

static SCM g_save_sound(SCM snd_n) 
{
  #define H_save_sound "(" S_save_sound " &optional snd) saves snd (updates the on-disk data to match Snd's current version)"
  return(sp_iread(snd_n, SP_SAVE, S_save_sound));
}

static SCM g_revert_sound(SCM index)
{
  #define H_revert_sound "("  S_revert_sound " &optional snd) reverts snd to its unedited state (undo all)"
  snd_info *sp;
  int i;
  SND_ASSERT_SND(S_revert_sound, index, 1);
  sp = get_sp(index);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_revert_sound, index));
  for (i = 0; i < sp->nchans; i++) 
    {
      revert_edits(sp->chans[i], NULL); 
      update_graph(sp->chans[i], NULL);
    }
  reflect_file_revert_in_label(sp);
  reflect_file_revert_in_menu(sp->state);
  return(SCM_BOOL_T);
}

static SCM g_selected_sound(void)
{
  #define H_selected_sound "(" S_selected_sound ") -> index of currently selected sound"
  snd_state *ss;
  ss = get_global_state();
  if ((ss->selected_sound != NO_SELECTION) && 
      (snd_ok(ss->sounds[ss->selected_sound])))
    return(TO_SMALL_SCM_INT(ss->selected_sound));
  return(TO_SMALL_SCM_INT(NO_SELECTION));
}

/* TODO: open-sound (et al) should throw NO-SUCH-FILE or whatever, not call snd_error (via make_file_info) 
 *         this either requires homogenizing snd_error/mus_error or making snd_error redirectable,
 *         or setting and restoring the snd_error_hook or splitting make_file_info into the _with_error parts
 */

static SCM g_open_sound(SCM filename)
{ /* returns index of new sound if successful */
  #define H_open_sound "(" S_open_sound " filename)\n\
opens filename (as if opened from File:Open menu option), and returns the new file's index"
  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  ss = get_global_state();
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_open_sound, "a string");
  fname = mus_expand_filename(TO_C_STRING(filename));
  if (!(mus_file_probe(fname)))
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_open_sound, filename));
    }
  sp = snd_open_file(fname, ss);
  if (fname) FREE(fname);
  if (sp) return(TO_SCM_INT(sp->index));
  return(SCM_BOOL_F);
}

static SCM g_open_raw_sound(SCM filename, SCM chans, SCM srate, SCM format)
{
  #define H_open_raw_sound "(" S_open_raw_sound " filename chans srate format)\n\
opens filename assuming the data matches the attributes indicated unless the file actually has a header"

  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  int os, oc, ofr, ou, ofit;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARG1, S_open_raw_sound, "a string");
  ASSERT_TYPE(NUMBER_P(srate), srate, SCM_ARG2, S_open_raw_sound, "a number");
  ASSERT_TYPE(INTEGER_P(chans), chans, SCM_ARG3, S_open_raw_sound, "an integer");
  ASSERT_TYPE(INTEGER_P(format), format, SCM_ARG4, S_open_raw_sound, "an integer");
  ss = get_global_state();
  ou = use_raw_defaults(ss);
  os = raw_srate(ss);
  oc = raw_chans(ss);
  ofr = raw_format(ss);
  ofit = fit_data_on_open(ss);
  set_raw_srate(ss, TO_C_INT_OR_ELSE(srate, 0));
  set_raw_chans(ss, TO_C_INT_OR_ELSE(chans, 0));
  set_raw_format(ss, TO_C_INT_OR_ELSE(format, 0));
  set_use_raw_defaults(ss, 1);
  set_fit_data_on_open(ss, 1);
  mus_header_set_raw_defaults(TO_C_INT_OR_ELSE(srate, 0),
			      TO_C_INT(chans),
			      TO_C_INT(format));
  fname = mus_expand_filename(TO_C_STRING(filename));
  if (!(mus_file_probe(fname)))
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_open_raw_sound, filename));
    }
  sp = snd_open_file(fname, ss);
  /* snd_open_file -> snd_open_file_1 -> add_sound_window -> make_file_info -> raw_data_dialog_to_file_info */
  /*   so here if hooked, we'd need to save the current hook, make it return the current args, open, then restore */
  if (fname) FREE(fname);
  set_raw_srate(ss, os);
  set_raw_chans(ss, oc);
  set_raw_format(ss, ofr);
  set_use_raw_defaults(ss, ou);
  set_fit_data_on_open(ss, ofit);
  if (sp) return(TO_SCM_INT(sp->index));
  return(SCM_BOOL_F);
}

static SCM g_open_alternate_sound(SCM filename)
{
  #define H_open_alternate_sound "(" S_open_alternate_sound " filename) replace currently selected sound with filename"
  /* returns index of new sound if successful */
  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_open_alternate_sound, "a string");
  ss = get_global_state();
  sp = any_selected_sound(ss);
  if (sp) snd_close_file(sp, ss); /* should we ask about saving edits here? */
  fname = mus_expand_filename(TO_C_STRING(filename));
  if (!(mus_file_probe(fname)))
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_open_alternate_sound, filename));
    }
  sp = snd_open_file(fname, ss);
  if (fname) FREE(fname);
  if (sp) return(TO_SCM_INT(sp->index));
  return(SCM_BOOL_F);
}

static SCM g_view_sound(SCM filename)
{
  #define H_view_sound "(" S_view_sound " filename) opens a file in read-only mode.\n\
You can subsequently make it writable by (set! (read-only) #f)."
  char *fname = NULL;
  snd_info *sp = NULL;
  snd_state *ss;
  ss = get_global_state();
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_view_sound, "a string");
  fname = mus_expand_filename(TO_C_STRING(filename));
  if (!(mus_file_probe(fname)))
    {
      if (fname) FREE(fname);
      return(snd_no_such_file_error(S_view_sound, filename));
    }
  if (fname)
    {
      ss->viewing = 1;
      sp = snd_open_file(fname, ss);
      FREE(fname);
      ss->viewing = 0;
      if (sp) return(TO_SCM_INT(sp->index));
    }
  return(SCM_BOOL_F);
}

static mus_error_handler_t *old_mus_error;

static void mus_local_error(int type, char *msg)
{
  mus_error_set_handler(old_mus_error);           /* make sure subsequent errors are handled by the default handler */
  ERROR(CANNOT_SAVE,
	SCM_LIST2(TO_SCM_STRING(S_save_sound_as),
		  TO_SCM_STRING(msg)));
}

static SCM g_save_sound_as(SCM newfile, SCM index, SCM type, SCM format, SCM srate, SCM channel, SCM edpos)
{
  #define H_save_sound_as "("  S_save_sound_as " filename\n     &optional snd header-type data-format srate channel edpos)\n\
saves snd in filename using the indicated attributes.  If channel is specified, only that channel is saved (extracted). \
Any argument can be #f which causes its value to be taken from the sound being saved.\n\
  (save-sound-as \"test.snd\" index mus-next mus-bshort)"

  snd_info *sp;
  chan_info *cp;
  file_info *hdr;
  int ht, df, sr, chan;
  char *fname = NULL;
  ASSERT_TYPE(STRING_P(newfile), newfile, SCM_ARG1, S_save_sound_as, "a string");
  SND_ASSERT_SND(S_save_sound_as, index, 2);
  sp = get_sp(index);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_save_sound_as, index));
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(type), type, SCM_ARG3, S_save_sound_as, "an integer (a header type id)");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(format), format, SCM_ARG4, S_save_sound_as, "an integer (a data format id)");
  ASSERT_TYPE(NUMBER_OR_BOOLEAN_IF_BOUND_P(srate), srate, SCM_ARG5, S_save_sound_as, "a number");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(channel), channel, SCM_ARG6, S_save_sound_as, "an integer");
  fname = mus_expand_filename(TO_C_STRING(newfile));
  hdr = sp->hdr;
  ht = TO_C_INT_OR_ELSE_WITH_ORIGIN(type, hdr->type, S_save_sound_as);
  sr = TO_C_INT_OR_ELSE_WITH_ORIGIN(srate, hdr->srate, S_save_sound_as);
  if (INTEGER_P(format)) 
    df = TO_C_INT(format);
  else    
    {
      if (mus_header_writable(ht, hdr->format))
	df = hdr->format;
      else df = MUS_OUT_FORMAT;
    }

  old_mus_error = mus_error_set_handler(mus_local_error);
  /* errors here can be so deeply nested in the various file save funcs that 
   * there's little hope of passing a flag around to say "throw a scheme error"
   * where the default throughout the path is to call snd_error
   */
  if (INTEGER_P(channel))
    {
      chan = TO_C_INT(channel);
      if ((chan >= sp->nchans) || 
	  (chan < 0))
	{
	  if (fname) FREE(fname);
	  return(snd_no_such_channel_error(S_save_sound_as, index, channel));
	}
      else 
	{
	  cp = sp->chans[chan];
	  save_channel_edits(cp, fname, edpos, S_save_sound_as);
	}
    }
  else save_edits_without_display(sp, fname, ht, df, sr, NULL, edpos, S_save_sound_as);
  mus_error_set_handler(old_mus_error);

  if (fname) FREE(fname);
  return(newfile);
}

static mus_error_handler_t *local_error;
static char *toss_this = NULL;
static SCM scm_this;
static void new_local_error(int type, char *msg)
{
  mus_error_set_handler(local_error);           /* make sure subsequent errors are handled by the default handler */
  if (toss_this) {FREE(toss_this); toss_this = NULL;}
  mus_misc_error(S_new_sound, msg, scm_this);
}

static SCM g_new_sound(SCM name, SCM type, SCM format, SCM srate, SCM chans, SCM comment) 
{
  #define H_new_sound "(" S_new_sound " name\n    &optional type format srate chans comment)\n\
creates a new sound file with the indicated attributes; if any are omitted, the corresponding default-output variable is used"

  snd_info *sp = NULL; 
  int ht, df, sr, ch, err;
  snd_state *ss;
  int chan, size;
  unsigned char* buf;
  char *str = NULL, *com = NULL;
  ASSERT_TYPE(STRING_P(name), name, SCM_ARG1, S_new_sound, "a string");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(type), type, SCM_ARG2, S_new_sound, "an integer (a header type id)");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(format), format, SCM_ARG3, S_new_sound, "an integer (a data format id)");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(srate), srate, SCM_ARG4, S_new_sound, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(chans), chans, SCM_ARG5, S_new_sound, "an integer");
  ASSERT_TYPE(STRING_IF_BOUND_P(comment), comment, SCM_ARG6, S_new_sound, "a string");
  ss = get_global_state();
  str = mus_expand_filename(TO_C_STRING(name));
  if (snd_overwrite_ok(ss, str))
    {
      mus_sound_forget(str);
      ht = TO_C_INT_OR_ELSE(type, default_output_type(ss));
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  df = TO_C_INT_OR_ELSE(format, default_output_format(ss));
	  if (MUS_DATA_FORMAT_OK(df))
	    {
	      if (mus_header_writable(ht, df))
		{
		  sr = TO_C_INT_OR_ELSE(srate, default_output_srate(ss));
		  ch = TO_C_INT_OR_ELSE(chans, default_output_chans(ss));
		  if (ch <= 0)
		    {
		      if (str) FREE(str);
		      mus_misc_error(S_new_sound, "chans <= 0?", chans);
		    }
		  if (STRING_P(comment))
		    com = TO_C_STRING(comment);
		  toss_this = str;
		  scm_this = name;
		  local_error = mus_error_set_handler(new_local_error);
		  err = snd_write_header(ss, str, ht, sr, ch, 0, ch * 2, df, com, snd_strlen(com), NULL);
		  mus_error_set_handler(local_error);
		  toss_this = NULL;
		  if (err == -1)
		    {
		      if (str) FREE(str);
		      mus_misc_error(S_new_sound, strerror(errno), name);
		    }
		  chan = snd_reopen_write(ss, str);
		  lseek(chan, mus_header_data_location(), SEEK_SET);
		  size = ch * mus_samples_to_bytes(df, 2); /* why 2 samples? */
		  buf = (unsigned char *)CALLOC(size, sizeof(unsigned char));
		  write(chan, buf, size);
		  close(chan);
		  FREE(buf);
		  sp = snd_open_file(str, ss);
		}
	      else 
		{
		  if (str) FREE(str);
		  mus_misc_error(S_new_sound, "can't write this combination of data format and header type", SCM_LIST2(type, format));
		}
	    }
	  else 
	    {
	      if (str) FREE(str);
	      mus_misc_error(S_new_sound, "invalid data format", format);
	    }
	}
      else
	{
	  if (str) FREE(str);
	  mus_misc_error(S_new_sound, "invalid header type", type);
	}
    }
  if (str) FREE(str);
  if (sp) return(TO_SCM_INT(sp->index));
  return(SCM_BOOL_F);
}

static SCM g_speed_style(SCM snd)
{
  #define H_speed_style "(" S_speed_style " (snd #t)) -> speed control panel interpretation choice (speed-as-float)"
  if (BOUND_P(snd))
    return(sp_iread(snd, SP_SPEED_STYLE, S_speed_style));
  return(TO_SCM_INT(speed_style(get_global_state())));
}

static SCM g_set_speed_style(SCM speed, SCM snd) 
{
  snd_state *ss;
  ASSERT_TYPE(INTEGER_P(speed), speed, SCM_ARG1, "set-" S_speed_style, "an integer"); 
  if (BOUND_P(snd))
    return(sp_iwrite(snd, speed, SP_SPEED_STYLE, "set-" S_speed_style));
  else
    {
      ss = get_global_state();
      activate_speed_in_menu(ss, mus_iclamp(SPEED_AS_FLOAT,
					    TO_C_INT(speed),
					    SPEED_AS_SEMITONE));
      return(TO_SCM_INT(speed_style(ss)));
    }
}

WITH_REVERSED_ARGS(g_set_speed_style_reversed, g_set_speed_style)

static SCM g_speed_tones(SCM snd)
{
  #define H_speed_tones "(" S_speed_tones " (snd #t)) -> if speed-style is speed-as-semitone, this chooses the octave divisions (12)"
  if (BOUND_P(snd))
    return(sp_iread(snd, SP_SPEED_TONES, S_speed_tones));
  return(TO_SCM_INT(speed_tones(get_global_state())));
}

static SCM g_set_speed_tones(SCM val, SCM snd)
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_speed_tones, "a number"); 
  if (BOUND_P(snd))
    return(sp_iwrite(snd, val, SP_SPEED_TONES, "set-" S_speed_tones));
  else
    {
      ss = get_global_state();
      set_speed_tones(ss, TO_C_INT_OR_ELSE(val, 0));
      return(TO_SCM_INT(speed_tones(ss)));
    }
}

WITH_REVERSED_ARGS(g_set_speed_tones_reversed, g_set_speed_tones)

enum {SP_AMP, SP_CONTRAST, SP_CONTRAST_AMP, SP_EXPAND, SP_EXPAND_LENGTH, SP_EXPAND_RAMP, SP_EXPAND_HOP,
      SP_SPEED, SP_REVERB_LENGTH, SP_REVERB_FEEDBACK, SP_REVERB_SCALE, SP_REVERB_LOW_PASS,
      SP_REVERB_DECAY
};

static SCM sp_fread(SCM snd_n, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  int i;
  SCM res = SCM_EOL;
  if (SCM_EQ_P(snd_n, SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = CONS(sp_fread(TO_SMALL_SCM_INT(i), fld, caller), res);
	}
      return(REVERSE_LIST(res));
    }
  SND_ASSERT_SND(caller, snd_n, 1);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    return(snd_no_such_sound_error(caller, snd_n));
  switch (fld)
    {
    case SP_AMP:             return(TO_SCM_DOUBLE(sp->amp));           break;
    case SP_CONTRAST:        return(TO_SCM_DOUBLE(sp->contrast));      break;
    case SP_CONTRAST_AMP:    return(TO_SCM_DOUBLE(sp->contrast_amp));  break;
    case SP_EXPAND:          return(TO_SCM_DOUBLE(sp->expand));        break;
    case SP_EXPAND_LENGTH:   return(TO_SCM_DOUBLE(sp->expand_length)); break;
    case SP_EXPAND_RAMP:     return(TO_SCM_DOUBLE(sp->expand_ramp));   break;
    case SP_EXPAND_HOP:      return(TO_SCM_DOUBLE(sp->expand_hop));    break;
    case SP_SPEED:           if (sp->play_direction == -1) return(TO_SCM_DOUBLE((-(sp->srate)))); else return(TO_SCM_DOUBLE(sp->srate)); break;
    case SP_REVERB_LENGTH:   return(TO_SCM_DOUBLE(sp->revlen));        break;
    case SP_REVERB_FEEDBACK: return(TO_SCM_DOUBLE(sp->revfb));         break;
    case SP_REVERB_SCALE:    return(TO_SCM_DOUBLE(sp->revscl));        break;
    case SP_REVERB_LOW_PASS: return(TO_SCM_DOUBLE(sp->revlp));         break;
    case SP_REVERB_DECAY:    return(TO_SCM_DOUBLE(sp->reverb_decay));  break;
    }
  return(SCM_BOOL_F);
}

static SCM sp_fwrite(SCM snd_n, SCM val, int fld, char *caller)
{
  snd_info *sp;
  Float fval;
  int direction, i;
  snd_state *ss;
  if (SCM_EQ_P(snd_n, SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    sp_fwrite(TO_SMALL_SCM_INT(i), val, fld, caller);
	}
    }
  else
    {
      SND_ASSERT_SND(caller, snd_n, 2);
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      fval = TO_C_DOUBLE_WITH_ORIGIN(val, caller);
      switch (fld)
	{
	case SP_AMP:           
	  if (fval >= 0.0) set_snd_amp(sp, fval); 
	  return(TO_SCM_DOUBLE(sp->amp)); 
	  break;
	case SP_CONTRAST:      
	  set_snd_contrast(sp, fval); 
	  return(TO_SCM_DOUBLE(sp->contrast)); 
	  break;
	case SP_CONTRAST_AMP:  
	  sp->contrast_amp = fval; 
	  if (sp->playing) dac_set_contrast_amp(sp, fval); 
	  break;
	case SP_EXPAND:        
	  if (fval > 0.0) set_snd_expand(sp, fval); 
	  return(TO_SCM_DOUBLE(sp->expand)); 
	  break;
	case SP_EXPAND_LENGTH: 
	  if (fval > 0.0) sp->expand_length = fval; 
	  if (sp->playing) dac_set_expand_length(sp, fval); 
	  break;
	case SP_EXPAND_RAMP:   
	  if ((fval >= 0.0) && (fval < 0.5)) sp->expand_ramp = fval; 
	  if (sp->playing) dac_set_expand_ramp(sp, fval); 
	  break;
	case SP_EXPAND_HOP:    
	  if (fval > 0.0) sp->expand_hop = fval; 
	  if (sp->playing) dac_set_expand_hop(sp, fval); 
	  break;
	case SP_SPEED: 
	  if (fval != 0.0)
	    {
	      if (fval > 0.0) direction = 1; else direction = -1;
	      set_snd_srate(sp, direction * fval); 
	      toggle_direction_arrow(sp, (direction == -1));
	      if (sp->play_direction == -1) 
		return(TO_SCM_DOUBLE((-(sp->srate)))); 
	      else return(TO_SCM_DOUBLE(sp->srate));
	    }
	  break;
	case SP_REVERB_LENGTH:    
	  if (fval >= 0.0) set_snd_revlen(sp, fval); 
	  return(TO_SCM_DOUBLE(sp->revlen)); 
	  break;
	case SP_REVERB_FEEDBACK:  
	  sp->revfb = fval; 
	  if (sp->playing) dac_set_reverb_feedback(sp, fval); 
	  break;
	case SP_REVERB_SCALE:     
	  set_snd_revscl(sp, fval); 
	  return(TO_SCM_DOUBLE(sp->revscl)); 
	  break;
	case SP_REVERB_LOW_PASS:  
	  sp->revlp = fval; 
	  if (sp->playing) dac_set_reverb_lowpass(sp, fval); 
	  break;
	case SP_REVERB_DECAY:     
	  sp->reverb_decay = fval; 
	  break;
	}
    }
  return(val);
}

static SCM g_amp(SCM snd_n) 
{
  #define H_amp "(" S_amp " &optional snd) -> current amp slider setting"
  return(sp_fread(snd_n, SP_AMP, S_amp));
}

static SCM g_set_amp(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_amp, "a number"); 
  return(sp_fwrite(snd_n, on, SP_AMP, "set-" S_amp));
}

WITH_REVERSED_ARGS(g_set_amp_reversed, g_set_amp)

static SCM g_contrast(SCM snd_n) 
{
  #define H_contrast "(" S_contrast " &optional snd) -> current contrast slider setting"
  return(sp_fread(snd_n, SP_CONTRAST, S_contrast));
}

static SCM g_set_contrast(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_contrast, "a number"); 
  return(sp_fwrite(snd_n, on, SP_CONTRAST, "set-" S_contrast));
}

WITH_REVERSED_ARGS(g_set_contrast_reversed, g_set_contrast)

static SCM g_contrast_amp(SCM snd_n) 
{
  #define H_contrast_amp "(" S_contrast_amp " &optional snd) -> snd's contrast amp\n\
   (scaler on data before contrast operation in control panel, 1.0)"

  return(sp_fread(snd_n, SP_CONTRAST_AMP, S_contrast_amp));
}

static SCM g_set_contrast_amp(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_contrast_amp, "a number");
  return(sp_fwrite(snd_n, on, SP_CONTRAST_AMP, "set-" S_contrast_amp));
}

WITH_REVERSED_ARGS(g_set_contrast_amp_reversed, g_set_contrast_amp)

static SCM g_expand(SCM snd_n) 
{
  #define H_expand "(" S_expand " &optional snd) -> current expand slider setting"
  return(sp_fread(snd_n, SP_EXPAND, S_expand));
}

static SCM g_set_expand(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_expand, "a number"); 
  return(sp_fwrite(snd_n, on, SP_EXPAND, "set-" S_expand));
}

WITH_REVERSED_ARGS(g_set_expand_reversed, g_set_expand)

static SCM g_expand_length(SCM snd_n) 
{
  #define H_expand_length "(" S_expand_length " &optional snd) -> current expansion segment length in seconds (.15)"
  return(sp_fread(snd_n, SP_EXPAND_LENGTH, S_expand_length));
}

static SCM g_set_expand_length(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_expand_length, "a number"); 
  return(sp_fwrite(snd_n, on, SP_EXPAND_LENGTH, "set-" S_expand_length));
}

WITH_REVERSED_ARGS(g_set_expand_length_reversed, g_set_expand_length)

static SCM g_expand_ramp(SCM snd_n) 
{
  #define H_expand_ramp "(" S_expand_ramp " &optional snd) -> current expansion ramp time (.4)"
  return(sp_fread(snd_n, SP_EXPAND_RAMP, S_expand_ramp));
}

static SCM g_set_expand_ramp(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_expand_ramp, "a number");
  return(sp_fwrite(snd_n, on, SP_EXPAND_RAMP, "set-" S_expand_ramp));
}

WITH_REVERSED_ARGS(g_set_expand_ramp_reversed, g_set_expand_ramp)

static SCM g_expand_hop(SCM snd_n) 
{
  #define H_expand_hop "(" S_expand_hop " &optional snd) -> current expansion output grain spacing in seconds (0.05)"
  return(sp_fread(snd_n, SP_EXPAND_HOP, S_expand_hop));
}

static SCM g_set_expand_hop(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_expand_hop, "a number"); 
  return(sp_fwrite(snd_n, on, SP_EXPAND_HOP, "set-" S_expand_hop));
}

WITH_REVERSED_ARGS(g_set_expand_hop_reversed, g_set_expand_hop)

static SCM g_speed(SCM snd_n) 
{
  #define H_speed "(" S_speed " &optional snd) -> current speed (srate) slider setting"
  return(sp_fread(snd_n, SP_SPEED, S_speed));
}

static SCM g_set_speed(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_speed, "a number"); 
  return(sp_fwrite(snd_n, on, SP_SPEED, "set-" S_speed));
}

WITH_REVERSED_ARGS(g_set_speed_reversed, g_set_speed)

static SCM g_reverb_length(SCM snd_n) 
{
  #define H_reverb_length "(" S_reverb_length " &optional snd) -> reverb decay length scaler"
  return(sp_fread(snd_n, SP_REVERB_LENGTH, S_reverb_length));
}

static SCM g_set_reverb_length(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_reverb_length, "a number"); 
  return(sp_fwrite(snd_n, on, SP_REVERB_LENGTH, "set-" S_reverb_length));
}

WITH_REVERSED_ARGS(g_set_reverb_length_reversed, g_set_reverb_length)

static SCM g_reverb_feedback(SCM snd_n) 
{
  #define H_reverb_feedback "(" S_reverb_feedback " &optional snd) -> reverb feedback scaler"
  return(sp_fread(snd_n, SP_REVERB_FEEDBACK, S_reverb_feedback));
}

static SCM g_set_reverb_feedback(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_reverb_feedback, "a number"); 
  return(sp_fwrite(snd_n, on, SP_REVERB_FEEDBACK, "set-" S_reverb_feedback));
}

WITH_REVERSED_ARGS(g_set_reverb_feedback_reversed, g_set_reverb_feedback)

static SCM g_reverb_scale(SCM snd_n) 
{
  #define H_reverb_scale "(" S_reverb_scale " &optional snd) -> reverb scaler (the amount of reverb)"
  return(sp_fread(snd_n, SP_REVERB_SCALE, S_reverb_scale));
}

static SCM g_set_reverb_scale(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_reverb_scale, "a number"); 
  return(sp_fwrite(snd_n, on, SP_REVERB_SCALE, "set-" S_reverb_scale));
}

WITH_REVERSED_ARGS(g_set_reverb_scale_reversed, g_set_reverb_scale)

static SCM g_reverb_lowpass(SCM snd_n) 
{
  #define H_reverb_lowpass "(" S_reverb_lowpass " &optional snd) -> reverb lowpass filter coefficient"
  return(sp_fread(snd_n, SP_REVERB_LOW_PASS, S_reverb_lowpass));
}

static SCM g_set_reverb_lowpass(SCM on, SCM snd_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_reverb_lowpass, "a number"); 
  return(sp_fwrite(snd_n, on, SP_REVERB_LOW_PASS, "set-" S_reverb_lowpass));
}

WITH_REVERSED_ARGS(g_set_reverb_lowpass_reversed, g_set_reverb_lowpass)

static SCM g_reverb_decay(SCM snd)
{
  #define H_reverb_decay "(" S_reverb_decay " &optional (snd #t)) -> 'Apply' button reverb decay time (1.0 seconds)"
  if (BOUND_P(snd))
    return(sp_fread(snd, SP_REVERB_DECAY, S_reverb_decay));
  return(TO_SCM_DOUBLE(reverb_decay(get_global_state())));
}

static SCM g_set_reverb_decay(SCM val, SCM snd)
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_reverb_decay, "a number"); 
  if (BOUND_P(snd))
    return(sp_fwrite(snd, val, SP_REVERB_DECAY, "set-" S_reverb_decay));
  else
    {
      ss = get_global_state();
      set_reverb_decay(ss, TO_C_DOUBLE(val));
      return(TO_SCM_DOUBLE(reverb_decay(ss)));
    }
}

WITH_REVERSED_ARGS(g_set_reverb_decay_reversed, g_set_reverb_decay)

static SCM g_set_filter_env(SCM edata, SCM snd_n)
{
  snd_info *sp;
  SND_ASSERT_SND("set-" S_filter_env, snd_n, 2);
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error("set-" S_filter_env, snd_n));
  if (sp->filter_env) sp->filter_env = free_env(sp->filter_env);  /* set to null in case get_env throws error */
  sp->filter_env = get_env(edata, SCM_BOOL_F, "set-" S_filter_env);
  filter_env_changed(sp, sp->filter_env);
  return(edata);
}

static SCM g_filter_env(SCM snd_n)
{
  #define H_filter_env "(" S_filter_env " &optional snd) -> snd's filter envelope (in the control panel)"
  snd_info *sp = NULL;
  SND_ASSERT_SND(S_filter_env, snd_n, 1);
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_filter_env, snd_n));
  return(env2scm(sp->filter_env)); 
}

WITH_REVERSED_ARGS(g_set_filter_env_reversed, g_set_filter_env)

static SCM g_call_apply(SCM snd, SCM choice)
{
  #define H_call_apply "(" S_call_apply " &optional snd choice) is equivalent to clicking the control panel 'Apply' button"
  snd_info *sp;
  snd_state *ss;
  SND_ASSERT_SND(S_call_apply, snd, 1);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(choice), choice, SCM_ARG2, S_call_apply, "an integer");
  sp = get_sp(snd);
  if (sp) 
    {
      ss = sp->state;
      ss->apply_choice = mus_iclamp(0, TO_C_INT_OR_ELSE(choice, 0), 2);
      run_apply_to_completion(sp); 
      return(SCM_BOOL_F);
    }
  return(snd_no_such_sound_error(S_call_apply, snd));
}

static SCM name_click_hook;
static int dont_babble_info(snd_info *sp)
{
  /* call name-click-hook (if any) return #t = don't print info in minibuffer */
  SCM res = SCM_BOOL_F, ind;
  ind = TO_SMALL_SCM_INT(sp->index);
  if (HOOKED(name_click_hook))
    res = g_c_run_or_hook(name_click_hook, 
			  SCM_LIST1(ind),
			  S_name_click_hook);
  return(TRUE_P(res));
}

#if (!USE_NO_GUI)
static SCM g_sound_widgets(SCM snd)
{
  snd_info *sp;
  SND_ASSERT_SND("sound_widgets", snd, 1);
  sp = get_sp(snd);
  return(CONS(SND_WRAP(w_snd_pane(sp)),
	  CONS(SND_WRAP(w_snd_name(sp)),
           CONS(SND_WRAP(w_snd_ctrls(sp)),
		SCM_EOL))));
}
#endif

static SCM g_peak_env_info(SCM snd, SCM chn, SCM pos)
{
  #define H_peak_env_info "(" S_peak_env_info " snd chn pos) -> '(complete ymin ymax)"
  chan_info *cp;
  env_info *ep;
  chan_context *cgx;
  SND_ASSERT_CHAN(S_peak_env_info, snd, chn, 1);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(pos), pos, SCM_ARG3, S_peak_env_info, "an integer");
  cp = get_cp(snd, chn, S_peak_env_info);
  cgx = cp->cgx;
  if ((!cgx) || (!(cp->amp_envs))) 
    return(SCM_LIST0);
  ep = cp->amp_envs[TO_C_INT_OR_ELSE(pos, cp->edit_ctr)];
  if (ep)
    return(SCM_LIST3(TO_SCM_BOOLEAN(ep->completed),
		     TO_SCM_DOUBLE(MUS_SAMPLE_TO_FLOAT(ep->fmin)),
		     TO_SCM_DOUBLE(MUS_SAMPLE_TO_FLOAT(ep->fmax))));
  /* don't throw an error here since the env may be in progress */
  return(SCM_LIST0);
}

static SCM g_write_peak_env_info_file(SCM snd, SCM chn, SCM name)
{
  chan_info *cp;
  char *fullname = NULL;
  env_info *ep;
  int fd;
  SCM errstr;
  int ibuf[5];
  MUS_SAMPLE_TYPE mbuf[2];
  SND_ASSERT_CHAN(S_write_peak_env_info_file, snd, chn, 1);
  cp = get_cp(snd, chn, S_write_peak_env_info_file);
  ASSERT_TYPE(STRING_P(name), name, SCM_ARG2, S_write_peak_env_info_file, "a string");
  if ((cp->amp_envs) && (cp->amp_envs[0]))
    {
      fullname = mus_expand_filename(TO_C_STRING(name));
      fd = mus_file_create(fullname);
      if (fd == -1)
	{
	  errstr = TO_SCM_STRING(fullname);
	  if (fullname) FREE(fullname);
	  ERROR(CANNOT_SAVE,
		SCM_LIST3(TO_SCM_STRING(S_write_peak_env_info_file),
			  errstr,
			  TO_SCM_STRING(strerror(errno))));
	}
      if (fullname) FREE(fullname);
      ep = cp->amp_envs[0];
      ibuf[0] = ep->completed;
      ibuf[1] = ep->amp_env_size;
      ibuf[2] = ep->samps_per_bin;
      ibuf[3] = ep->bin;
      ibuf[4] = ep->top_bin;
      mbuf[0] = ep->fmin;
      mbuf[1] = ep->fmax;
      write(fd, (char *)ibuf, (5 * sizeof(int)));
      write(fd, (char *)mbuf, (2 * sizeof(MUS_SAMPLE_TYPE)));
      write(fd, (char *)(ep->data_min), (ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE)));
      write(fd, (char *)(ep->data_max), (ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE)));
      close(fd);
      return(name);
    }
  ERROR(NO_SUCH_ENVELOPE,
	SCM_LIST3(TO_SCM_STRING(S_write_peak_env_info_file),
		  snd,
		  chn));
  return(snd);
}

static SCM g_read_peak_env_info_file(SCM snd, SCM chn, SCM name)
{
  /* has to happen in initial_graph_hook to precede add_amp_env */
  chan_info *cp;
  char *fullname;
  env_info *ep;
  int fd;
  int ibuf[5];
  MUS_SAMPLE_TYPE mbuf[2];
  SND_ASSERT_CHAN(S_read_peak_env_info_file, snd, chn, 1);
  cp = get_cp(snd, chn, S_read_peak_env_info_file);
  fullname = mus_expand_filename(TO_C_STRING(name));
  fd = mus_file_open_read(fullname);
  if (fullname) FREE(fullname);
  if (fd == -1)
    return(snd_no_such_file_error(S_read_peak_env_info_file, name));
  /* assume cp->amp_envs already exists (needs change to snd-chn) */
  cp->amp_envs[0] = (env_info *)CALLOC(1, sizeof(env_info));
  ep = cp->amp_envs[0];
  read(fd, (char *)ibuf, (5 * sizeof(int)));
  ep->completed = ibuf[0];
  ep->amp_env_size = ibuf[1];
  ep->samps_per_bin = ibuf[2];
  ep->bin = ibuf[3];
  ep->top_bin = ibuf[4];
  read(fd, (char *)mbuf, (2 * sizeof(MUS_SAMPLE_TYPE)));
  ep->fmin = mbuf[0];
  ep->fmax = mbuf[1];
  ep->data_min = (MUS_SAMPLE_TYPE *)MALLOC(ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE));
  ep->data_max = (MUS_SAMPLE_TYPE *)MALLOC(ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE));
  read(fd, (char *)(ep->data_min), (ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE)));
  read(fd, (char *)(ep->data_max), (ep->amp_env_size * sizeof(MUS_SAMPLE_TYPE)));
  close(fd);
  return(name);
}

void g_init_snd(SCM local_doc)
{
  #define H_name_click_hook S_name_click_hook " (snd) is called when sound name clicked. \
If it returns #t, the usual informative minibuffer babbling is squelched."

  name_click_hook = MAKE_HOOK(S_name_click_hook, 1, H_name_click_hook);       /* args = snd-index */

#if (!USE_NO_GUI)
  DEFINE_PROC(S_sound_widgets, g_sound_widgets, 0, 1, 0, "returns sound widgets");
#endif

  DEFINE_PROC(S_soundQ, g_soundQ, 0, 1, 0, H_soundQ);
  DEFINE_PROC(S_bomb, g_bomb, 0, 2, 0, H_bomb);
  DEFINE_PROC(S_find_sound, g_find_sound, 1, 0, 0, H_find_sound);

  define_procedure_with_setter(S_channels, SCM_FNC g_channels, H_channels,
			       "set-" S_channels, SCM_FNC g_set_channels, local_doc, 0, 1, 0, 2);

  define_procedure_with_setter(S_chans, SCM_FNC g_channels, H_channels,
			       "set-" S_chans, SCM_FNC g_set_channels, local_doc, 0, 1, 0, 2);

  define_procedure_with_setter(S_srate, SCM_FNC g_srate, H_srate,
			       "set-" S_srate, SCM_FNC g_set_srate, local_doc, 0, 1, 0, 2);

  define_procedure_with_setter(S_data_location, SCM_FNC g_data_location, H_data_location,
			       "set-" S_data_location, SCM_FNC g_set_data_location, local_doc, 0, 1, 0, 2);

  define_procedure_with_setter(S_data_format, SCM_FNC g_data_format, H_data_format,
			       "set-" S_data_format, SCM_FNC g_set_data_format, local_doc, 0, 1, 0, 2);

  define_procedure_with_setter(S_header_type, SCM_FNC g_header_type, H_header_type,
			       "set-" S_header_type, SCM_FNC g_set_header_type, local_doc, 0, 1, 0, 2);

  define_procedure_with_setter(S_comment, SCM_FNC g_comment, H_comment,
			       "set-" S_comment, SCM_FNC g_set_comment, local_doc, 0, 1, 0, 2);

  DEFINE_PROC(S_file_name,             g_file_name, 0, 1, 0,             H_file_name);
  DEFINE_PROC(S_short_file_name,       g_short_file_name, 0, 1, 0,       H_short_file_name);
  DEFINE_PROC(S_save_control_panel,    g_save_control_panel, 0, 1, 0,    H_save_control_panel);
  DEFINE_PROC(S_restore_control_panel, g_restore_control_panel, 0, 1, 0, H_restore_control_panel);
  DEFINE_PROC(S_reset_control_panel,   g_reset_control_panel, 0, 1, 0,   H_reset_control_panel);

  define_procedure_with_setter(S_selected_sound, SCM_FNC g_selected_sound, H_selected_sound,
			       "set-" S_selected_sound, SCM_FNC g_select_sound, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_selected_channel, SCM_FNC g_selected_channel, H_selected_channel,
			       "set-" S_selected_channel, SCM_FNC g_set_selected_channel, local_doc, 0, 1, 0, 2);

  DEFINE_PROC(S_select_sound, g_select_sound, 0, 1, 0, H_select_sound);
  DEFINE_PROC(S_select_channel, g_select_channel, 0, 1, 0, H_select_channel);

  DEFINE_PROC(S_close_sound,          g_close_sound, 0, 1, 0,          H_close_sound);
  DEFINE_PROC(S_update_sound,         g_update_sound, 0, 1, 0,         H_update_sound);
  DEFINE_PROC(S_save_sound,           g_save_sound, 0, 1, 0,           H_save_sound);
  DEFINE_PROC(S_open_sound,           g_open_sound, 1, 0, 0,           H_open_sound);
  DEFINE_PROC(S_open_raw_sound,       g_open_raw_sound, 4, 0, 0,       H_open_raw_sound);
  DEFINE_PROC(S_open_alternate_sound, g_open_alternate_sound, 1, 0, 0, H_open_alternate_sound);
  DEFINE_PROC(S_view_sound,           g_view_sound, 1, 0, 0,           H_view_sound);
  DEFINE_PROC(S_new_sound,            g_new_sound, 1, 5, 0,            H_new_sound);
  DEFINE_PROC(S_revert_sound,         g_revert_sound, 0, 1, 0,         H_revert_sound);
  DEFINE_PROC(S_save_sound_as,        g_save_sound_as, 1, 6, 0,        H_save_sound_as);
  DEFINE_PROC(S_call_apply,           g_call_apply, 0, 2, 0,           H_call_apply);


  define_procedure_with_reversed_setter(S_filter_env, SCM_FNC g_filter_env, H_filter_env,
					"set-" S_filter_env, SCM_FNC g_set_filter_env, SCM_FNC g_set_filter_env_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_cursor_follows_play, SCM_FNC g_cursor_follows_play, H_cursor_follows_play,
					"set-" S_cursor_follows_play, SCM_FNC g_set_cursor_follows_play, SCM_FNC g_set_cursor_follows_play_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_show_controls, SCM_FNC g_show_controls, H_show_controls,
					"set-" S_show_controls, SCM_FNC g_set_show_controls, SCM_FNC g_set_show_controls_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_sync, SCM_FNC g_sync, H_sync,
					"set-" S_sync, SCM_FNC g_set_sync, SCM_FNC g_set_sync_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_uniting, SCM_FNC g_uniting, H_uniting,
					"set-" S_uniting, SCM_FNC g_set_uniting, SCM_FNC g_set_uniting_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_read_only, SCM_FNC g_read_only, H_read_only,
					"set-" S_read_only, SCM_FNC g_set_read_only, SCM_FNC g_set_read_only_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_expanding, SCM_FNC g_expanding, H_expanding,
					"set-" S_expanding, SCM_FNC g_set_expanding, SCM_FNC g_set_expanding_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_contrasting, SCM_FNC g_contrasting, H_contrasting,
					"set-" S_contrasting, SCM_FNC g_set_contrasting, SCM_FNC g_set_contrasting_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_reverbing, SCM_FNC g_reverbing, H_reverbing,
					"set-" S_reverbing, SCM_FNC g_set_reverbing, SCM_FNC g_set_reverbing_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_filtering, SCM_FNC g_filtering, H_filtering,
					"set-" S_filtering, SCM_FNC g_set_filtering, SCM_FNC g_set_filtering_reversed,
					local_doc, 0, 1, 0, 2);


  define_procedure_with_reversed_setter(S_filter_dBing, SCM_FNC g_filter_dBing, H_filter_dBing,
					"set-" S_filter_dBing, SCM_FNC g_set_filter_dBing, SCM_FNC g_set_filter_dBing_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_filter_order, SCM_FNC g_filter_order, H_filter_order,
					"set-" S_filter_order, SCM_FNC g_set_filter_order, SCM_FNC g_set_filter_order_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_contrast, SCM_FNC g_contrast, H_contrast,
					"set-" S_contrast, SCM_FNC g_set_contrast, SCM_FNC g_set_contrast_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_contrast_amp, SCM_FNC g_contrast_amp, H_contrast_amp,
					"set-" S_contrast_amp, SCM_FNC g_set_contrast_amp, SCM_FNC g_set_contrast_amp_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_expand, SCM_FNC g_expand, H_expand,
					"set-" S_expand, SCM_FNC g_set_expand, SCM_FNC g_set_expand_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_expand_length, SCM_FNC g_expand_length, H_expand_length,
					"set-" S_expand_length, SCM_FNC g_set_expand_length, SCM_FNC g_set_expand_length_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_expand_ramp, SCM_FNC g_expand_ramp, H_expand_ramp,
					"set-" S_expand_ramp, SCM_FNC g_set_expand_ramp, SCM_FNC g_set_expand_ramp_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_expand_hop, SCM_FNC g_expand_hop, H_expand_hop,
					"set-" S_expand_hop, SCM_FNC g_set_expand_hop, SCM_FNC g_set_expand_hop_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_speed, SCM_FNC g_speed, H_speed,
					"set-" S_speed, SCM_FNC g_set_speed, SCM_FNC g_set_speed_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_reverb_length, SCM_FNC g_reverb_length, H_reverb_length,
					"set-" S_reverb_length, SCM_FNC g_set_reverb_length, SCM_FNC g_set_reverb_length_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_reverb_scale, SCM_FNC g_reverb_scale, H_reverb_scale,
					"set-" S_reverb_scale, SCM_FNC g_set_reverb_scale, SCM_FNC g_set_reverb_scale_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_reverb_feedback, SCM_FNC g_reverb_feedback, H_reverb_feedback,
					"set-" S_reverb_feedback, SCM_FNC g_set_reverb_feedback, SCM_FNC g_set_reverb_feedback_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_reverb_lowpass, SCM_FNC g_reverb_lowpass, H_reverb_lowpass,
					"set-" S_reverb_lowpass, SCM_FNC g_set_reverb_lowpass, SCM_FNC g_set_reverb_lowpass_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_amp, SCM_FNC g_amp, H_amp,
					"set-" S_amp, SCM_FNC g_set_amp, SCM_FNC g_set_amp_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_reverb_decay, SCM_FNC g_reverb_decay, H_reverb_decay,
					"set-" S_reverb_decay, SCM_FNC g_set_reverb_decay, SCM_FNC g_set_reverb_decay_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_speed_style, SCM_FNC g_speed_style, H_speed_style,
					"set-" S_speed_style, SCM_FNC g_set_speed_style, SCM_FNC g_set_speed_style_reversed,
					local_doc, 0, 1, 0, 2);

  define_procedure_with_reversed_setter(S_speed_tones, SCM_FNC g_speed_tones, H_speed_tones,
					"set-" S_speed_tones, SCM_FNC g_set_speed_tones, SCM_FNC g_set_speed_tones_reversed,
					local_doc, 0, 1, 0, 2);

  DEFINE_PROC(S_peak_env_info, g_peak_env_info, 0, 3, 0, H_peak_env_info);


  DEFINE_PROC(S_write_peak_env_info_file, g_write_peak_env_info_file, 3, 0, 0, "(" S_write_peak_env_info_file " snd chn filename)");
  DEFINE_PROC(S_read_peak_env_info_file,  g_read_peak_env_info_file,  3, 0, 0, "(" S_read_peak_env_info_file " snd chn filename)");
}

