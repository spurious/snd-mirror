#include "snd.h"

snd_info *snd_new_file(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *new_comment)
{
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
	      return(snd_open_file(newname, ss, FALSE));
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
  env_info *ep, *old_ep = NULL;
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
      if (cp->edit_ctr > 0)
	{
	  old_ep = cp->amp_envs[cp->edit_ctr - 1];
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
  #define AMP_ENV_SUBSAMPLE 100
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
      if (sb >= ep->amp_env_size)
	{
	  /* oops... */
	  es->slice++;
	  if (es->sf) es->sf = free_snd_fd(es->sf);
	  ep->completed = 1;
	  return(TRUE);
	}
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
	  fd = mus_file_open_read(sp->filename);
	  mus_file_open_descriptors(fd,
				   sp->filename,
				   mus_sound_data_format(sp->filename),
				   mus_sound_datum_size(sp->filename),
				   mus_sound_data_location(sp->filename),
				   sp->nchans,
				   mus_sound_header_type(sp->filename));
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
	      for (i = 1; i < ep->samps_per_bin; i += subsamp, m++)
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
		  int n;
		  MUS_SAMPLE_TYPE val, ymin, ymax;
		  /* here we have to read the current bin using the current fragments */
		  sf = init_sample_read(cursamp, cp, READ_FORWARD);
		  if (sf == NULL) return;
		  ymin = MUS_SAMPLE_0;
		  ymax = MUS_SAMPLE_0;
		  for (n = 0; n < new_ep->samps_per_bin; n++)
		    {
		      val = next_sample(sf); /* not scaled again! (sample reader sees scaled version) */
		      if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
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

env_info *amp_env_copy(chan_info *cp, int reversed, int edpos)
{
  env_info *old_ep, *new_ep = NULL;
  int i, j;
  old_ep = cp->amp_envs[edpos];
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
    case SPEED_CONTROL_AS_RATIO: 
      for (i = 1; i < TOTAL_RATS; i++)
	if (rat_values[i] > val) 
	  break;
      sprintf(srcbuf, "%s", rat_names[i-1]);
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
  if (is_link(sp->filename))
    {
      mus_snprintf(sname, PRINT_BUFFER_SIZE, "(%s)", sp->short_filename);
      return(sname);
    }
  else return(sp->short_filename);
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

static XEN name_click_hook;

void sp_name_click(snd_info *sp)
{
  file_info *hdr;
  Float dur;
  int linked = 0;
  if (sp)
    {
      /* call name-click-hook (if any) return #t = don't print info in minibuffer */
      if ((XEN_HOOKED(name_click_hook)) &&
	  (XEN_TRUE_P(g_c_run_or_hook(name_click_hook, 
				      XEN_LIST_1(C_TO_SMALL_XEN_INT(sp->index)),
				      S_name_click_hook))))
	return;
      hdr = sp->hdr;
      if (hdr)
	{
	  linked = is_link(sp->filename);
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
			       (linked) ? linked_file(sp->filename) : "",
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
    cs->direction = 0; 
  else cs->direction = 1;
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

static XEN before_apply_hook;
static XEN after_apply_hook;

static void *make_apply_state(void *xp)
{
  /* set up initial state for apply_controls */
  apply_state *ap = NULL;
  snd_info *sp = (snd_info *)xp;
  /* call apply-hook (if any) return #t = don't apply */
  if ((XEN_HOOKED(before_apply_hook)) &&
      (XEN_TRUE_P(g_c_run_or_hook(before_apply_hook, 
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

static int max_sync(snd_info *sp, void *val)
{
  int *maxsync = (int *)val;
  if (sp->sync > maxsync[0])
    maxsync[0] = sp->sync;
  return(0);
}

static int apply_dur = 0, apply_tick = 0, apply_reporting = 0, orig_dur, apply_beg = 0;

void *make_apply_state_with_implied_beg_and_dur(void *xp)
{
  apply_beg = 0;
  apply_dur = 0;
  return(make_apply_state(xp));
}

BACKGROUND_TYPE apply_controls(GUI_POINTER ptr)
{
  apply_state *ap = (apply_state *)ptr;
  snd_state *ss;
  snd_context *sgx;
  snd_info *sp;
  chan_info *cp, *ncp;
  sync_info *si;
  Float ratio, mult_dur;
  int i, len, over_selection, curchan = 0, added_dur = 0, old_sync;
  int maxsync[1];
  Float *scalers = NULL;
  if (ptr == NULL) return(BACKGROUND_QUIT);
  sp = ap->sp;
  if (!(sp->active)) return(BACKGROUND_QUIT);
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
	  map_over_sounds(ss, max_sync, (void *)maxsync);
	  sp->sync = maxsync[0] + 1;
	}
      else sp->sync = 0;
      /* check for local amp_control vals */
      if (sp->selected_channel == NO_SELECTION) 
	cp = sp->chans[0];
      else cp = sp->chans[sp->selected_channel];
      si = sync_to_chan(cp);
      scalers = (Float *)CALLOC(si->chans, sizeof(Float));
      for (i = 0; i < si->chans; i++)
	{
	  ncp = si->cps[i];
	  if (ncp->amp_control)
	    scalers[i] = ncp->amp_control[0];
	  else scalers[i] = sp->amp_control;
	}
      scale_by(cp, scalers, si->chans, FALSE);
      sp->sync = old_sync;
      FREE(scalers);
    }
  else
    {
      switch (ap->slice)
	{
	case 0:
	  /* apply_beg = 0; */
	  ap->ofile = NULL;
	  lock_apply(ss, sp);
	  ap->ofile = snd_tempnam(ss);
	  ap->hdr = make_temp_header(ap->ofile, SND_SRATE(sp), sp->nchans, 0, (char *)__FUNCTION__);
	  switch (ss->apply_choice)
	    {
	    case APPLY_TO_CHANNEL:   
	      ap->hdr->chans = 1; 
	      if (sp->selected_channel != NO_SELECTION) 
		curchan = sp->selected_channel;
	      if (apply_dur == 0)
		apply_dur = current_ed_samples(sp->chans[curchan]) - apply_beg;
	      break;
	    case APPLY_TO_SOUND:     
	      ap->hdr->chans = sp->nchans; 
	      if (apply_dur == 0)
		apply_dur = current_ed_samples(sp->chans[0]) - apply_beg;
	      break;
	    case APPLY_TO_SELECTION: 
	      ap->hdr->chans = selection_chans();
	      if (ap->hdr->chans <= 0) return(BACKGROUND_QUIT);
	      if (apply_dur == 0)
		apply_dur = selection_len(); 
	      break;
	    }
	  orig_dur = apply_dur;
	  apply_dur = (int)(mult_dur * (apply_dur + added_dur));
	  ap->ofd = open_temp_file(ap->ofile, ap->hdr->chans, ap->hdr, ss);
	  sp->apply_ok = 1;
	  initialize_apply(sp, ap->hdr->chans, apply_beg, apply_dur); /* snd-dac.c, called only here */
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
		  if (apply_beg > 0)
		    {
		      for (i = 0; i < sp->nchans; i++)
			{
			  file_change_samples(apply_beg, apply_dur, ap->ofile, sp->chans[i], i,
					      (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
					      LOCK_MIXES, "Apply");
			  update_graph(sp->chans[i], NULL);
			}
		    }
		  else
		    {
		      for (i = 0; i < sp->nchans; i++)
			file_override_samples(apply_dur, ap->ofile, sp->chans[i], i,
					      (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
					      LOCK_MIXES, "Apply");
		    }
		  break;
		case APPLY_TO_CHANNEL: 
		  if (sp->selected_channel != NO_SELECTION) 
		    curchan = sp->selected_channel;
		  if (apply_beg > 0)
		    {
		      file_change_samples(apply_beg, apply_dur, ap->ofile, sp->chans[curchan], 0, 
					  DELETE_ME, LOCK_MIXES, "Apply to channel");
		      update_graph(sp->chans[curchan], NULL);
		    }
		  else file_override_samples(apply_dur, ap->ofile, sp->chans[curchan], 0, 
					     DELETE_ME, LOCK_MIXES, "Apply to channel");
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
  reset_controls(sp); /* i.e. clear it */
  sp->applying = 0;
  sgx = sp->sgx;
  if (sgx->apply_in_progress) sgx->apply_in_progress = 0;
  if (XEN_HOOKED(after_apply_hook))
    g_c_run_progn_hook(after_apply_hook, 
		       XEN_LIST_1(C_TO_SMALL_XEN_INT(sp->index)),
		       S_after_apply_hook);
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

static void set_reverb_decay(snd_state *ss, Float val) 
{
  int i;
  snd_info *sp;
  in_set_reverb_control_decay(ss, val);
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse))
	sp->reverb_control_decay = val;
    }
}

static int map_sounds_speed_tones(snd_info *sp, void *val) 
{
  sp->speed_control_tones = (*((int *)val)); 
  return(0);
}

static void set_speed_tones(snd_state *ss, int val) 
{
  in_set_speed_control_tones(ss, val); 
  map_over_sounds(ss, map_sounds_speed_tones, (void *)(&val));
}      

static int map_sounds_speed_style(snd_info *sp, void *val) 
{
  sp->speed_control_style = (*((int *)val)); 
  return(0);
}

void set_speed_style(snd_state *ss, int val) 
{
  in_set_speed_control_style(ss, val); 
  map_over_sounds(ss, map_sounds_speed_style, (void *)(&val));
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
  #define H_sound_p "(" S_sound_p " &optional (index 0)) -> #t if sound associated with index is active (accessible)"
  snd_info *sp;
  sp = get_sp(snd_n);
  return(C_TO_XEN_BOOLEAN((sp) && snd_ok(sp)));
}

static XEN g_select_sound(XEN snd_n)
{
  #define H_select_sound "(" S_select_sound " &optional snd) makes snd the selected (active) sound"
  int val;
  snd_state *ss;
  snd_info *sp;
  ss = get_global_state();
  ASSERT_SOUND(S_select_sound, snd_n, 1);
  val = XEN_TO_C_INT_OR_ELSE(snd_n, 0);
  if ((val >= 0) && 
      (val < ss->max_sounds))
    {
      sp = ss->sounds[val];
      if (snd_ok(sp))
	{
	  select_channel(sp, 0);
	  equalize_sound_panes(ss, sp, sp->chans[0], FALSE);
	  map_over_chans(ss, update_graph, NULL);
	  return(snd_n);
	}
    }
  return(snd_no_such_sound_error(S_select_sound, snd_n));
}

static XEN g_select_channel(XEN chn_n)
{
  #define H_select_channel "(" S_select_channel " &optional chn) makes chn the selected (active) channel"
  snd_info *sp;
  snd_state *ss;
  int chan;
  ss = get_global_state();
  ASSERT_SOUND(S_select_channel, chn_n, 1);
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

static XEN g_find_sound(XEN filename)
{
  #define H_find_sound "(" S_find_sound " name) returns the id of the sound associated with file 'name'"
  snd_state *ss;
  snd_info *sp;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_find_sound, "a string");
  ss = get_global_state();
  sp = find_sound(ss, XEN_TO_C_STRING(filename));
  if (sp) return(C_TO_XEN_INT(sp->index));
  return(xen_return_first(XEN_FALSE, filename));
}


static XEN g_bomb(XEN snd, XEN on)
{
  #define H_bomb "(" S_bomb " &optional snd (on #t)) displays (or erases if on=#f) the bomb icon"
  snd_info *sp;
  ASSERT_SOUND(S_bomb, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_bomb, snd));
  x_bomb(sp, XEN_TO_C_BOOLEAN_OR_TRUE(on));
  return(on);
}

enum {SP_SYNC, SP_READ_ONLY, SP_NCHANS, SP_CONTRASTING, SP_EXPANDING, SP_REVERBING, SP_FILTERING, SP_FILTER_ORDER,
      SP_SRATE, SP_DATA_FORMAT, SP_DATA_LOCATION, SP_HEADER_TYPE, SP_SAVE_CONTROLS, SP_RESTORE_CONTROLS, SP_SELECTED_CHANNEL,
      SP_COMMENT, SP_FILE_NAME, SP_SHORT_FILE_NAME, SP_CLOSE, SP_UPDATE, SP_SAVE, SP_CURSOR_FOLLOWS_PLAY, SP_SHOW_CONTROLS,
      SP_FILTER_DBING, SP_SPEED_TONES, SP_SPEED_STYLE, SP_RESET_CONTROLS
};

static XEN sp_iread(XEN snd_n, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  char *str;
  int i;
  XEN res = XEN_EMPTY_LIST;
  if (XEN_TRUE_P(snd_n))
    {
      ss = get_global_state();
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = XEN_CONS(sp_iread(C_TO_SMALL_XEN_INT(i), fld, caller), res);
	}
      return(res);
    }
  ASSERT_SOUND(caller, snd_n, 1);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    return(snd_no_such_sound_error(caller, snd_n));
  switch (fld)
    {
    case SP_SYNC:                return(C_TO_XEN_INT(sp->sync));                    break;
    case SP_READ_ONLY:           return(C_TO_XEN_BOOLEAN(sp->read_only));           break;
    case SP_NCHANS:              return(C_TO_XEN_INT(sp->nchans));                  break;
    case SP_EXPANDING:           return(C_TO_XEN_BOOLEAN(sp->expand_control_p));    break;
    case SP_CONTRASTING:         return(C_TO_XEN_BOOLEAN(sp->contrast_control_p));  break;
    case SP_REVERBING:           return(C_TO_XEN_BOOLEAN(sp->reverb_control_p));    break;
    case SP_FILTERING:           return(C_TO_XEN_BOOLEAN(sp->filter_control_p));    break;
    case SP_FILTER_DBING:        return(C_TO_XEN_BOOLEAN(sp->filter_control_in_dB));break;
    case SP_FILTER_ORDER:        return(C_TO_XEN_INT(sp->filter_control_order));    break;
    case SP_SRATE:               return(C_TO_XEN_INT((sp->hdr)->srate));            break;
    case SP_DATA_FORMAT:         return(C_TO_XEN_INT((sp->hdr)->format));           break;
    case SP_HEADER_TYPE:         return(C_TO_XEN_INT((sp->hdr)->type));             break;
    case SP_DATA_LOCATION:       return(C_TO_XEN_INT((sp->hdr)->data_location));    break;
    case SP_SAVE_CONTROLS:       save_controls(sp);                                 break;
    case SP_RESTORE_CONTROLS:    restore_controls(sp);                              break;
    case SP_RESET_CONTROLS:      reset_controls(sp);                                break;
    case SP_SELECTED_CHANNEL:    return(C_TO_XEN_INT(sp->selected_channel));        break;
    case SP_FILE_NAME:           return(C_TO_XEN_STRING(sp->filename));             break;
    case SP_SHORT_FILE_NAME:     return(C_TO_XEN_STRING(sp->short_filename));       break;
    case SP_CLOSE:               snd_close_file(sp, sp->state);                     break;
    case SP_SAVE:                save_edits(sp, NULL);                              break;
    case SP_UPDATE:              snd_update(sp->state, sp);                         break;
    case SP_CURSOR_FOLLOWS_PLAY: return(C_TO_XEN_BOOLEAN(sp->cursor_follows_play)); break;
    case SP_SHOW_CONTROLS:       return(C_TO_XEN_BOOLEAN(control_panel_open(sp)));  break;
    case SP_SPEED_TONES:         return(C_TO_XEN_INT(sp->speed_control_tones));     break;
    case SP_SPEED_STYLE:         return(C_TO_XEN_INT(sp->speed_control_style));     break;
    case SP_COMMENT:
      str = mus_sound_comment(sp->filename);
      res = C_TO_XEN_STRING(str);
      if (str) FREE(str);
      return(res);
      break;
    }
  return(XEN_FALSE);
}

static XEN sp_iwrite(XEN snd_n, XEN val, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  char *com;
  int i, ival;
  if (XEN_TRUE_P(snd_n))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    sp_iwrite(C_TO_SMALL_XEN_INT(i), val, fld, caller);
	}
      return(val);
    }
  ASSERT_SOUND(caller, snd_n, 2); /* 2 from caller's point of view */
  sp = get_sp(snd_n);
  if (sp == NULL) 
    return(snd_no_such_sound_error(caller, snd_n));
  ss = sp->state;
  switch (fld)
    {
    case SP_SYNC:  
      if (XEN_NUMBER_P(val))
	syncb(sp, XEN_TO_C_INT_OR_ELSE_WITH_CALLER(val, 1, caller));
      else syncb(sp, XEN_TO_C_BOOLEAN(val));
      break;
    case SP_READ_ONLY:
      sp->read_only = XEN_TO_C_BOOLEAN_OR_TRUE(val); 
      snd_file_lock_icon(sp, sp->read_only); 
      break;
    case SP_EXPANDING:
      toggle_expand_button(sp, XEN_TO_C_BOOLEAN_OR_TRUE(val));
      break;
    case SP_CONTRASTING:
      toggle_contrast_button(sp, XEN_TO_C_BOOLEAN_OR_TRUE(val));
      break;
    case SP_REVERBING:
      toggle_reverb_button(sp, XEN_TO_C_BOOLEAN_OR_TRUE(val));
      break;
    case SP_FILTERING:
      toggle_filter_button(sp, XEN_TO_C_BOOLEAN_OR_TRUE(val));
      break;
    case SP_FILTER_DBING:   
      set_filter_in_dB(sp, XEN_TO_C_BOOLEAN_OR_TRUE(val));
      break;
    case SP_FILTER_ORDER:
      set_snd_filter_order(sp, XEN_TO_C_INT(val));
      break;
    case SP_CURSOR_FOLLOWS_PLAY:
      sp->cursor_follows_play = XEN_TO_C_BOOLEAN_OR_TRUE(val);
      break;
    case SP_SHOW_CONTROLS:
      if (XEN_TO_C_BOOLEAN_OR_TRUE(val))
	sound_show_ctrls(sp); 
      else sound_hide_ctrls(sp); 
      break;
    case SP_SPEED_TONES:
      sp->speed_control_tones = XEN_TO_C_INT(val);
      if (sp->speed_control_tones <= 0) 
	sp->speed_control_tones = DEFAULT_SPEED_CONTROL_TONES;
      break;
    case SP_SPEED_STYLE:
      sp->speed_control_style = mus_iclamp(0, XEN_TO_C_INT(val), MAX_SPEED_CONTROL_STYLE);
      break;
    case SP_SRATE:
      mus_sound_set_srate(sp->filename, XEN_TO_C_INT(val));
      snd_update(ss, sp); 
      break;
    case SP_NCHANS: 
      mus_sound_set_chans(sp->filename, XEN_TO_C_INT(val));
      snd_update(ss, sp); 
      break;
    case SP_DATA_FORMAT:
      ival = XEN_TO_C_INT(val);
      if (MUS_DATA_FORMAT_OK(ival))
	{
	  mus_sound_set_data_format(sp->filename, ival);
	  snd_update(ss, sp);
	}
      else mus_misc_error("set-" S_data_format, "unknown data format", val);
      break;
    case SP_HEADER_TYPE:
      ival = XEN_TO_C_INT(val);
      if (MUS_HEADER_TYPE_OK(ival))
	{
	  mus_sound_set_header_type(sp->filename, ival);
	  snd_update(ss, sp); 
	}
      else mus_misc_error("set-" S_header_type, "unknown header type", val);
      break;
    case SP_DATA_LOCATION:  
      mus_sound_set_data_location(sp->filename, XEN_TO_C_INT(val));
      snd_update(ss, sp); 
      break;
    case SP_COMMENT:      
      /* this is safe only with aifc and riff headers */
      com = XEN_TO_NEW_C_STRING(val);
      mus_header_update_comment(sp->filename, 0, com, snd_strlen(com), mus_sound_header_type(sp->filename));
      free(com);
      snd_update(ss, sp);
      break;
    }
  return(val);
}

static XEN g_channels(XEN snd_n)
{
  #define H_channels "("  S_channels " &optional snd) how many channels snd has"
  return(sp_iread(snd_n, SP_NCHANS, S_channels));
}

static XEN check_number(XEN val, char *caller)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, caller, "a number");
  return(val);
}

static XEN g_set_channels(XEN snd_n, XEN val)
{
  if (XEN_NOT_BOUND_P(val))
    return(sp_iwrite(XEN_UNDEFINED, check_number(snd_n, "set-" S_channels), SP_NCHANS, "set-" S_channels));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_channels), SP_NCHANS, "set-" S_channels));
}

static XEN g_srate(XEN snd_n) 
{
  #define H_srate "(" S_srate " &optional snd) -> snd's srate"
  return(sp_iread(snd_n, SP_SRATE, S_srate));
}

static XEN g_set_srate(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sp_iwrite(XEN_UNDEFINED, check_number(snd_n, "set-" S_srate), SP_SRATE, "set-" S_srate));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_srate), SP_SRATE, "set-" S_srate));
}

static XEN g_data_location(XEN snd_n) 
{
  #define H_data_location "(" S_data_location " &optional snd) -> snd's data location"
  return(sp_iread(snd_n, SP_DATA_LOCATION, S_data_location));
}

static XEN g_set_data_location(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sp_iwrite(XEN_UNDEFINED, check_number(snd_n, "set-" S_data_location), SP_DATA_LOCATION, "set-" S_data_location));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_data_location), SP_DATA_LOCATION, "set-" S_data_location));
}

static XEN g_data_format(XEN snd_n) 
{
  #define H_data_format "(" S_data_format " &optional snd) -> snd's data format (e.g. mus-bshort)"
  return(sp_iread(snd_n, SP_DATA_FORMAT, S_data_format));
}

static XEN g_set_data_format(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sp_iwrite(XEN_UNDEFINED, check_number(snd_n, "set-" S_data_format), SP_DATA_FORMAT, "set-" S_data_format));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_data_format), SP_DATA_FORMAT, "set-" S_data_format));
}

static XEN g_header_type(XEN snd_n) 
{
  #define H_header_type "(" S_header_type " &optional snd) -> snd's header type (e.g. mus-aiff)"
  return(sp_iread(snd_n, SP_HEADER_TYPE, S_header_type));
}

static XEN g_set_header_type(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sp_iwrite(XEN_UNDEFINED, check_number(snd_n, "set-" S_header_type), SP_HEADER_TYPE, "set-" S_header_type));
  else return(sp_iwrite(snd_n, check_number(val, "set-" S_header_type), SP_HEADER_TYPE, "set-" S_header_type));
}

static XEN g_comment(XEN snd_n)
{
  #define H_comment "(" S_comment " &optional snd) -> snd's comment (in its header)"
  return(sp_iread(snd_n, SP_COMMENT, S_comment));
}

static XEN g_set_comment(XEN snd_n, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sp_iwrite(XEN_UNDEFINED, snd_n, SP_COMMENT, "set-" S_comment));
  else return(sp_iwrite(snd_n, val, SP_COMMENT, "set-" S_comment));
}


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

#define WITH_REVERSED_ARGS(name_reversed, name) \
static XEN name_reversed(XEN arg1, XEN arg2) \
{ \
  if (XEN_NOT_BOUND_P(arg2)) \
    return(name(arg1, XEN_UNDEFINED)); \
  else return(name(arg2, arg1)); \
}


static XEN g_sync(XEN snd_n) 
{
  #define H_sync "(" S_sync " &optional snd) -> whether snd is sync'd to other sounds"
  return(sp_iread(snd_n, SP_SYNC, S_sync));
}

static XEN g_set_sync(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_sync, "an integer");
  return(sp_iwrite(snd_n, on, SP_SYNC, "set-" S_sync));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_sync_reversed, g_set_sync)

static XEN g_channel_style(XEN snd) 
{
  snd_info *sp;
  if (XEN_NOT_BOUND_P(snd))
    return(C_TO_XEN_INT(channel_style(get_global_state())));
  ASSERT_SOUND(S_channel_style, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_channel_style, snd));
  return(C_TO_XEN_INT(sp->channel_style));
}

static XEN g_set_channel_style(XEN style, XEN snd) 
{
  snd_state *ss;
  snd_info *sp;
  int new_style = CHANNELS_SEPARATE;
  #define H_channel_style "(" S_channel_style " &optional snd) -> how multichannel sounds layout the channels \
default is channels-separate, other values are channels-combined and channels-superimposed. \
As a global (if the 'snd' arg is omitted), it is the default setting for each sound's 'unite' button."

  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(style), style, XEN_ARG_1, "set-" S_channel_style, "an integer or boolean"); 
  if (XEN_INTEGER_P(style))
    new_style = mus_iclamp(CHANNELS_SEPARATE,
			   XEN_TO_C_INT(style),
			   CHANNELS_SUPERIMPOSED);
  else new_style = ((XEN_TRUE_P(style)) ? CHANNELS_COMBINED : CHANNELS_SEPARATE);
  if (XEN_NOT_BOUND_P(snd))
    {
      ss = get_global_state();
      set_channel_style(ss, new_style);
      return(C_TO_XEN_INT(channel_style(ss)));
    }
  ASSERT_SOUND("set-" S_channel_style, snd, 2);
  sp = get_sp(snd);
  if (sp == NULL) 
    return(snd_no_such_sound_error("set-" S_channel_style, snd));
  set_sound_channel_style(sp, new_style);
  return(C_TO_XEN_INT(sp->channel_style));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_channel_style_reversed, g_set_channel_style)

static XEN g_read_only(XEN snd_n) 
{
  #define H_read_only "(" S_read_only " &optional snd) -> whether snd is write-protected"
  return(sp_iread(snd_n, SP_READ_ONLY, S_read_only));
}

static XEN g_set_read_only(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_read_only, "a boolean");
  return(sp_iwrite(snd_n, on, SP_READ_ONLY, "set-" S_read_only));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_read_only_reversed, g_set_read_only)

static XEN g_contrast_control_p(XEN snd_n) 
{
  #define H_contrast_control_p "(" S_contrast_control_p " &optional snd) -> snd's control panel constrast button state"
  return(sp_iread(snd_n, SP_CONTRASTING, S_contrast_control_p));
}

static XEN g_set_contrast_control_p(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_contrast_control_p, "a boolean");
  return(sp_iwrite(snd_n, on, SP_CONTRASTING, "set-" S_contrast_control_p));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_contrast_control_p_reversed, g_set_contrast_control_p)

static XEN g_expand_control_p(XEN snd_n) 
{
  #define H_expand_control_p "(" S_expand_control_p " &optional snd) -> snd's control panel expand button state"
  return(sp_iread(snd_n, SP_EXPANDING, S_expand_control_p));
}

static XEN g_set_expand_control_p(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_expand_control_p, "a boolean");
  return(sp_iwrite(snd_n, on, SP_EXPANDING, "set-" S_expand_control_p));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_expand_control_p_reversed, g_set_expand_control_p)

static XEN g_reverb_control_p(XEN snd_n) 
{
  #define H_reverb_control_p "(" S_reverb_control_p " &optional snd) -> snd's control panel reverb button state"
  return(sp_iread(snd_n, SP_REVERBING, S_reverb_control_p));
}

static XEN g_set_reverb_control_p(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_reverb_control_p, "a boolean");
  return(sp_iwrite(snd_n, on, SP_REVERBING, "set-" S_reverb_control_p));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_reverb_control_p_reversed, g_set_reverb_control_p)

static XEN g_filter_control_p(XEN snd_n) 
{
  #define H_filter_control_p "(" S_filter_control_p " &optional snd) -> snd's control panel filter button state"
  return(sp_iread(snd_n, SP_FILTERING, S_filter_control_p));
}

static XEN g_set_filter_control_p(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_filter_control_p, "a boolean");
  return(sp_iwrite(snd_n, on, SP_FILTERING, "set-" S_filter_control_p));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_filter_control_p_reversed, g_set_filter_control_p)

static XEN g_filter_control_in_dB(XEN snd_n) 
{
  #define H_filter_control_in_dB "(" S_filter_control_in_dB " &optional snd) -> #t if snd's filter envelope is displayed in dB in control panel"
  return(sp_iread(snd_n, SP_FILTER_DBING, S_filter_control_in_dB));
}

static XEN g_set_filter_control_in_dB(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_filter_control_in_dB, "a boolean");
  return(sp_iwrite(snd_n, on, SP_FILTER_DBING, "set-" S_filter_control_in_dB));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_filter_control_in_dB_reversed, g_set_filter_control_in_dB)

static XEN g_filter_control_order(XEN snd_n) 
{
  #define H_filter_control_order "(" S_filter_control_order " &optional snd) -> filter order (in control panel)"
  return(sp_iread(snd_n, SP_FILTER_ORDER, S_filter_control_order));
}

static XEN g_set_filter_control_order(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, "set-" S_filter_control_order, "an integer"); 
  return(sp_iwrite(snd_n, on, SP_FILTER_ORDER, "set-" S_filter_control_order));
}

WITH_REVERSED_ARGS(g_set_filter_control_order_reversed, g_set_filter_control_order)

static XEN g_cursor_follows_play(XEN snd_n) 
{
  #define H_cursor_follows_play "("  S_cursor_follows_play " &optional snd) -> #t if cursor moves along in waveform display as sound is played (#f)"
  return(sp_iread(snd_n, SP_CURSOR_FOLLOWS_PLAY, S_cursor_follows_play));
}

static XEN g_set_cursor_follows_play(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_cursor_follows_play, "a boolean");
  return(sp_iwrite(snd_n, on, SP_CURSOR_FOLLOWS_PLAY, "set-" S_cursor_follows_play));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_cursor_follows_play_reversed, g_set_cursor_follows_play)

static XEN g_show_controls(XEN snd_n) 
{
  #define H_show_controls "(" S_show_controls " &optional snd) -> #t if snd's control panel is known to be open"
  return(sp_iread(snd_n, SP_SHOW_CONTROLS, S_show_controls));
}

static XEN g_set_show_controls(XEN on, XEN snd_n)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, "set-" S_show_controls, "a boolean");
  return(sp_iwrite(snd_n, on, SP_SHOW_CONTROLS, "set-" S_show_controls));
}

WITH_REVERSED_BOOLEAN_ARGS(g_set_show_controls_reversed, g_set_show_controls)

static XEN g_save_controls(XEN snd_n) 
{
  #define H_save_controls "(" S_save_controls " &optional snd) saves the current control panel settings for subsequent " S_restore_controls
  return(sp_iread(snd_n, SP_SAVE_CONTROLS, S_save_controls));
}

static XEN g_restore_controls(XEN snd_n) 
{
  #define H_restore_controls "(" S_restore_controls " &optional snd) restores the previously saved control panel settings"
  return(sp_iread(snd_n, SP_RESTORE_CONTROLS, S_restore_controls));
}

static XEN g_reset_controls(XEN snd_n) 
{
  #define H_reset_controls "(" S_reset_controls " &optional snd) resets (clears) the control panel settings"
  return(sp_iread(snd_n, SP_RESET_CONTROLS, S_reset_controls));
}

static XEN g_selected_channel(XEN snd_n) 
{
  #define H_selected_channel "(" S_selected_channel " &optional snd) -> currently selected channel in snd"
  return(sp_iread(snd_n, SP_SELECTED_CHANNEL, S_selected_channel));
}

static XEN g_set_selected_channel(XEN snd_n, XEN chn_n) 
{
  snd_info *sp;
  int chan;
  if (XEN_NOT_BOUND_P(chn_n))
    return(g_select_channel(snd_n));
  else
    {
      ASSERT_SOUND("set-" S_selected_channel, snd_n, 1); 
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error("set-" S_selected_channel, snd_n));
      chan = XEN_TO_C_INT_OR_ELSE(chn_n, 0);
      if ((sp) && 
	  (chan >= 0) && 
	  (chan < sp->nchans)) 
	{
	  select_channel(sp, chan);
	  return(chn_n);
	}
    }
  return(snd_no_such_channel_error("set-" S_selected_channel, snd_n, chn_n));
}

static XEN g_file_name(XEN snd_n) 
{
  #define H_file_name "(" S_file_name " &optional snd) -> snd's full filename"
  return(sp_iread(snd_n, SP_FILE_NAME, S_file_name));
}

static XEN g_short_file_name(XEN snd_n) 
{
  #define H_short_file_name "(" S_short_file_name " &optional snd) -> short form of snd's file name (no directory)"
  return(sp_iread(snd_n, SP_SHORT_FILE_NAME, S_short_file_name));
}

static XEN g_close_sound(XEN snd_n) 
{
  #define H_close_sound "(" S_close_sound " snd) closes snd"
  return(sp_iread(snd_n, SP_CLOSE, S_close_sound));
}

static XEN g_update_sound(XEN snd_n) 
{
  #define H_update_sound "(" S_update_sound " snd) updates snd (re-reads from disk flushing pending edits)"
  return(sp_iread(snd_n, SP_UPDATE, S_update_sound));
}

static XEN g_save_sound(XEN snd_n) 
{
  #define H_save_sound "(" S_save_sound " &optional snd) saves snd (updates the on-disk data to match Snd's current version)"
  return(sp_iread(snd_n, SP_SAVE, S_save_sound));
}

static XEN g_revert_sound(XEN index)
{
  #define H_revert_sound "("  S_revert_sound " &optional snd) reverts snd to its unedited state (undo all)"
  snd_info *sp;
  int i;
  ASSERT_SOUND(S_revert_sound, index, 1);
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
  return(XEN_TRUE);
}

static XEN g_selected_sound(void)
{
  #define H_selected_sound "(" S_selected_sound ") -> index of currently selected sound"
  snd_state *ss;
  ss = get_global_state();
  if ((ss->selected_sound != NO_SELECTION) && 
      (snd_ok(ss->sounds[ss->selected_sound])))
    return(C_TO_SMALL_XEN_INT(ss->selected_sound));
  return(C_TO_SMALL_XEN_INT(NO_SELECTION));
}

static XEN g_open_sound(XEN filename)
{ /* returns index of new sound if successful */
  #define H_open_sound "(" S_open_sound " filename)\n\
opens filename (as if opened from File:Open menu option), and returns the new file's index"
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
  sp = snd_open_file(fname, ss, FALSE);
  if (fname) FREE(fname);
  if (sp) 
    return(C_TO_XEN_INT(sp->index));
  else
    XEN_ERROR(MUS_MISC_ERROR,
	      XEN_LIST_2(C_TO_XEN_STRING(S_open_sound),
			 C_TO_XEN_STRING(ss->catch_message)));
  return(XEN_FALSE);
}

static XEN g_open_raw_sound(XEN filename, XEN chans, XEN srate, XEN format)
{
  #define H_open_raw_sound "(" S_open_raw_sound " filename chans srate format)\n\
opens filename assuming the data matches the attributes indicated unless the file actually has a header"

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
  ss->reloading_updated_file = TRUE;
  ss->catch_message = NULL;
  sp = snd_open_file(fname, ss, FALSE);
  ss->reloading_updated_file = FALSE;
  /* snd_open_file -> snd_open_file_1 -> add_sound_window -> make_file_info -> raw_data_dialog_to_file_info */
  /*   so here if hooked, we'd need to save the current hook, make it return the current args, open, then restore */
  if (fname) FREE(fname);
  if (sp) 
    return(C_TO_XEN_INT(sp->index));
  else
    XEN_ERROR(MUS_MISC_ERROR,
	      XEN_LIST_2(C_TO_XEN_STRING(S_open_raw_sound),
			 C_TO_XEN_STRING(ss->catch_message)));
  return(XEN_FALSE);
}

static XEN g_view_sound(XEN filename)
{
  #define H_view_sound "(" S_view_sound " filename) opens a file in read-only mode.\n\
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
  sp = snd_open_file(fname, ss, TRUE);
  FREE(fname);
  if (sp) 
    return(C_TO_XEN_INT(sp->index));
  else 
    XEN_ERROR(MUS_MISC_ERROR,
	      XEN_LIST_2(C_TO_XEN_STRING(S_view_sound),
			 C_TO_XEN_STRING(ss->catch_message)));
  return(XEN_FALSE);
}

static XEN g_save_sound_as(XEN newfile, XEN index, XEN type, XEN format, XEN srate, XEN channel, XEN edpos)
{
  #define H_save_sound_as "("  S_save_sound_as " filename\n     &optional snd header-type data-format srate channel edpos)\n\
saves snd in filename using the indicated attributes.  If channel is specified, only that channel is saved (extracted). \
Any argument can be #f which causes its value to be taken from the sound being saved.\n\
  (save-sound-as \"test.snd\" index mus-next mus-bshort)"

  snd_info *sp;
  chan_info *cp;
  file_info *hdr;
  int ht, df, sr, chan, err;
  char *fname = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(newfile), newfile, XEN_ARG_1, S_save_sound_as, "a string");
  ASSERT_SOUND(S_save_sound_as, index, 2);
  sp = get_sp(index);
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
  else err = save_edits_without_display(sp, fname, ht, df, sr, NULL, edpos, S_save_sound_as, 7);
  if (fname) FREE(fname);
  if (err != MUS_NO_ERROR)
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_save_sound_as),
			 C_TO_XEN_STRING(sp->state->catch_message)));
  return(newfile);
}

static XEN g_new_sound(XEN name, XEN type, XEN format, XEN srate, XEN chans, XEN comment) 
{
  #define H_new_sound "(" S_new_sound " &optional name type format srate chans comment)\n\
creates a new sound file with the indicated attributes; if any are omitted, the corresponding default-output variable is used"

  snd_info *sp = NULL; 
  int ht, df, sr, ch, err;
  snd_state *ss;
  int chan, size;
  unsigned char* buf;
  char *str = NULL, *com = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(name), name, XEN_ARG_1, S_new_sound, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(type), type, XEN_ARG_2, S_new_sound, "an integer (a header type id)");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(format), format, XEN_ARG_3, S_new_sound, "an integer (a data format id)");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(srate), srate, XEN_ARG_4, S_new_sound, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chans), chans, XEN_ARG_5, S_new_sound, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(comment), comment, XEN_ARG_6, S_new_sound, "a string");
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
		      mus_misc_error(S_new_sound, "chans <= 0?", chans);
		    }
		  if (XEN_STRING_P(comment))
		    com = XEN_TO_C_STRING(comment);
		  ss->catch_message = NULL;
		  err = snd_write_header(ss, str, ht, sr, ch, 0, ch * 2, df, com, snd_strlen(com), NULL);
		  if (err == -1)
		    {
		      if (str) FREE(str);
		      if (ss->catch_message)
			mus_misc_error(S_new_sound, ss->catch_message, name);
		      else mus_misc_error(S_new_sound, strerror(errno), name);
		    }
		  chan = snd_reopen_write(ss, str);
		  lseek(chan, mus_header_data_location(), SEEK_SET);
		  size = ch * mus_samples_to_bytes(df, 2); /* why 2 samples? */
		  buf = (unsigned char *)CALLOC(size, sizeof(unsigned char));
		  write(chan, buf, size);
		  close(chan);
		  FREE(buf);
		  sp = snd_open_file(str, ss, FALSE);
		}
	      else 
		{
		  if (str) FREE(str);
		  mus_misc_error(S_new_sound, "can't write this combination of data format and header type", XEN_LIST_2(type, format));
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
  if (sp) return(C_TO_XEN_INT(sp->index));
  return(XEN_FALSE);
}

static XEN g_speed_control_style(XEN snd)
{
  #define H_speed_control_style "(" S_speed_control_style " (snd #t)) -> speed control panel interpretation choice (speed-as-float)"
  if (XEN_BOUND_P(snd))
    return(sp_iread(snd, SP_SPEED_STYLE, S_speed_control_style));
  return(C_TO_XEN_INT(speed_control_style(get_global_state())));
}

static XEN g_set_speed_control_style(XEN speed, XEN snd) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speed), speed, XEN_ARG_1, "set-" S_speed_control_style, "an integer"); 
  if (XEN_BOUND_P(snd))
    return(sp_iwrite(snd, speed, SP_SPEED_STYLE, "set-" S_speed_control_style));
  else
    {
      ss = get_global_state();
      activate_speed_in_menu(ss, mus_iclamp(SPEED_CONTROL_AS_FLOAT,
					    XEN_TO_C_INT(speed),
					    SPEED_CONTROL_AS_SEMITONE));
      return(C_TO_XEN_INT(speed_control_style(ss)));
    }
}

WITH_REVERSED_ARGS(g_set_speed_control_style_reversed, g_set_speed_control_style)

static XEN g_speed_control_tones(XEN snd)
{
  #define H_speed_control_tones "(" S_speed_control_tones " (snd #t)) -> if " S_speed_control_style " is " S_speed_control_as_semitone ", this chooses the octave divisions (12)"
  if (XEN_BOUND_P(snd))
    return(sp_iread(snd, SP_SPEED_TONES, S_speed_control_tones));
  return(C_TO_XEN_INT(speed_control_tones(get_global_state())));
}

static XEN g_set_speed_control_tones(XEN val, XEN snd)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, "set-" S_speed_control_tones, "a number"); 
  if (XEN_BOUND_P(snd))
    return(sp_iwrite(snd, val, SP_SPEED_TONES, "set-" S_speed_control_tones));
  else
    {
      ss = get_global_state();
      set_speed_tones(ss, XEN_TO_C_INT_OR_ELSE(val, 0));
      return(C_TO_XEN_INT(speed_control_tones(ss)));
    }
}

WITH_REVERSED_ARGS(g_set_speed_control_tones_reversed, g_set_speed_control_tones)

enum {SP_AMP, SP_CONTRAST, SP_CONTRAST_AMP, SP_EXPAND, SP_EXPAND_LENGTH, SP_EXPAND_RAMP, SP_EXPAND_HOP,
      SP_SPEED, SP_REVERB_LENGTH, SP_REVERB_FEEDBACK, SP_REVERB_SCALE, SP_REVERB_LOW_PASS,
      SP_REVERB_DECAY
};

static XEN sp_fread(XEN snd_n, int fld, char *caller)
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
	  if ((sp) && (sp->inuse))
	    res = XEN_CONS(sp_fread(C_TO_SMALL_XEN_INT(i), fld, caller), res);
	}
      return(res);
    }
  ASSERT_SOUND(caller, snd_n, 1);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    return(snd_no_such_sound_error(caller, snd_n));
  switch (fld)
    {
    case SP_AMP:             return(C_TO_XEN_DOUBLE(sp->amp_control));           break;
    case SP_CONTRAST:        return(C_TO_XEN_DOUBLE(sp->contrast_control));      break;
    case SP_CONTRAST_AMP:    return(C_TO_XEN_DOUBLE(sp->contrast_control_amp));  break;
    case SP_EXPAND:          return(C_TO_XEN_DOUBLE(sp->expand_control));        break;
    case SP_EXPAND_LENGTH:   return(C_TO_XEN_DOUBLE(sp->expand_control_length)); break;
    case SP_EXPAND_RAMP:     return(C_TO_XEN_DOUBLE(sp->expand_control_ramp));   break;
    case SP_EXPAND_HOP:      return(C_TO_XEN_DOUBLE(sp->expand_control_hop));    break;
    case SP_SPEED:           
      if (sp->speed_control_direction == -1) 
	return(C_TO_XEN_DOUBLE((-(sp->speed_control)))); 
      else return(C_TO_XEN_DOUBLE(sp->speed_control)); 
      break;
    case SP_REVERB_LENGTH:   return(C_TO_XEN_DOUBLE(sp->reverb_control_length));   break;
    case SP_REVERB_FEEDBACK: return(C_TO_XEN_DOUBLE(sp->reverb_control_feedback)); break;
    case SP_REVERB_SCALE:    return(C_TO_XEN_DOUBLE(sp->reverb_control_scale));    break;
    case SP_REVERB_LOW_PASS: return(C_TO_XEN_DOUBLE(sp->reverb_control_lowpass));  break;
    case SP_REVERB_DECAY:    return(C_TO_XEN_DOUBLE(sp->reverb_control_decay));    break;
    }
  return(XEN_FALSE);
}

static XEN sp_fwrite(XEN snd_n, XEN val, int fld, char *caller)
{
  snd_info *sp;
  Float fval;
  int direction, i;
  snd_state *ss;
  if (XEN_TRUE_P(snd_n))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    sp_fwrite(C_TO_SMALL_XEN_INT(i), val, fld, caller);
	}
    }
  else
    {
      ASSERT_SOUND(caller, snd_n, 2);
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      fval = XEN_TO_C_DOUBLE_WITH_CALLER(val, caller);
      switch (fld)
	{
	case SP_AMP:           
	  if (fval >= 0.0) set_snd_amp(sp, fval); 
	  return(C_TO_XEN_DOUBLE(sp->amp_control)); 
	  break;
	case SP_CONTRAST:      
	  set_snd_contrast(sp, fval); 
	  return(C_TO_XEN_DOUBLE(sp->contrast_control)); 
	  break;
	case SP_CONTRAST_AMP:  
	  sp->contrast_control_amp = fval; 
	  if (sp->playing) dac_set_contrast_amp(sp, fval); 
	  break;
	case SP_EXPAND:        
	  if (fval > 0.0) set_snd_expand(sp, fval); 
	  return(C_TO_XEN_DOUBLE(sp->expand_control)); 
	  break;
	case SP_EXPAND_LENGTH: 
	  if (fval > 0.0) sp->expand_control_length = fval; 
	  if (sp->playing) dac_set_expand_length(sp, fval); 
	  break;
	case SP_EXPAND_RAMP:   
	  if ((fval >= 0.0) && (fval < 0.5)) sp->expand_control_ramp = fval; 
	  if (sp->playing) dac_set_expand_ramp(sp, fval); 
	  break;
	case SP_EXPAND_HOP:    
	  if (fval > 0.0) sp->expand_control_hop = fval; 
	  if (sp->playing) dac_set_expand_hop(sp, fval); 
	  break;
	case SP_SPEED: 
	  if (fval != 0.0)
	    {
	      if (fval > 0.0) direction = 1; else direction = -1;
	      set_snd_srate(sp, direction * fval); 
	      toggle_direction_arrow(sp, (direction == -1));
	      if (sp->speed_control_direction == -1) 
		return(C_TO_XEN_DOUBLE((-(sp->speed_control)))); 
	      else return(C_TO_XEN_DOUBLE(sp->speed_control));
	    }
	  break;
	case SP_REVERB_LENGTH:    
	  if (fval >= 0.0) set_snd_revlen(sp, fval); 
	  return(C_TO_XEN_DOUBLE(sp->reverb_control_length)); 
	  break;
	case SP_REVERB_FEEDBACK:  
	  sp->reverb_control_feedback = fval; 
	  if (sp->playing) dac_set_reverb_feedback(sp, fval); 
	  break;
	case SP_REVERB_SCALE:     
	  set_snd_revscl(sp, fval); 
	  return(C_TO_XEN_DOUBLE(sp->reverb_control_scale)); 
	  break;
	case SP_REVERB_LOW_PASS:  
	  sp->reverb_control_lowpass = fval; 
	  if (sp->playing) dac_set_reverb_lowpass(sp, fval); 
	  break;
	case SP_REVERB_DECAY:     
	  sp->reverb_control_decay = fval; 
	  break;
	}
    }
  return(val);
}

static XEN g_amp_control(XEN snd_n, XEN chn_n) 
{
  #define H_amp_control "(" S_amp_control " &optional snd chn) -> current amp slider setting"
  chan_info *cp;
  if (XEN_BOUND_P(chn_n))
    {
      ASSERT_CHANNEL(S_amp_control, snd_n, chn_n, 1);
      cp = get_cp(snd_n, chn_n, S_amp_control);
      if ((cp) && (cp->amp_control))
	return(C_TO_XEN_DOUBLE(cp->amp_control[0]));
    }
  return(sp_fread(snd_n, SP_AMP, S_amp_control));
}

static XEN g_set_amp_control(XEN on, XEN snd_n, XEN chn_n) 
{
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_amp_control, "a number"); 
  if (XEN_BOUND_P(chn_n))
    {
      ASSERT_CHANNEL(S_amp_control, snd_n, chn_n, 2);
      cp = get_cp(snd_n, chn_n, S_amp_control);
      if (cp)
	{
	  if (cp->amp_control == NULL)
	    cp->amp_control = (Float *)CALLOC(1, sizeof(Float));
	  cp->amp_control[0] = (Float)XEN_TO_C_DOUBLE_WITH_CALLER(on, "set-" S_amp_control);
	  return(on);
	}
    }
  return(sp_fwrite(snd_n, on, SP_AMP, "set-" S_amp_control));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_amp_control_reversed, g_set_amp_control)

static XEN g_contrast_control(XEN snd_n) 
{
  #define H_contrast_control "(" S_contrast_control " &optional snd) -> current contrast slider setting"
  return(sp_fread(snd_n, SP_CONTRAST, S_contrast_control));
}

static XEN g_set_contrast_control(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_contrast_control, "a number"); 
  return(sp_fwrite(snd_n, on, SP_CONTRAST, "set-" S_contrast_control));
}

WITH_REVERSED_ARGS(g_set_contrast_control_reversed, g_set_contrast_control)

static XEN g_contrast_control_amp(XEN snd_n) 
{
  #define H_contrast_control_amp "(" S_contrast_control_amp " &optional snd) -> snd's contrast amp\n\
   (scaler on data before contrast operation in control panel, 1.0)"

  return(sp_fread(snd_n, SP_CONTRAST_AMP, S_contrast_control_amp));
}

static XEN g_set_contrast_control_amp(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_contrast_control_amp, "a number");
  return(sp_fwrite(snd_n, on, SP_CONTRAST_AMP, "set-" S_contrast_control_amp));
}

WITH_REVERSED_ARGS(g_set_contrast_control_amp_reversed, g_set_contrast_control_amp)

static XEN g_expand_control(XEN snd_n) 
{
  #define H_expand_control "(" S_expand_control " &optional snd) -> current expand slider setting"
  return(sp_fread(snd_n, SP_EXPAND, S_expand_control));
}

static XEN g_set_expand_control(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_expand_control, "a number"); 
  return(sp_fwrite(snd_n, on, SP_EXPAND, "set-" S_expand_control));
}

WITH_REVERSED_ARGS(g_set_expand_control_reversed, g_set_expand_control)

static XEN g_expand_control_length(XEN snd_n) 
{
  #define H_expand_control_length "(" S_expand_control_length " &optional snd) -> current expansion segment length in seconds (.15)"
  return(sp_fread(snd_n, SP_EXPAND_LENGTH, S_expand_control_length));
}

static XEN g_set_expand_control_length(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_expand_control_length, "a number"); 
  return(sp_fwrite(snd_n, on, SP_EXPAND_LENGTH, "set-" S_expand_control_length));
}

WITH_REVERSED_ARGS(g_set_expand_control_length_reversed, g_set_expand_control_length)

static XEN g_expand_control_ramp(XEN snd_n) 
{
  #define H_expand_control_ramp "(" S_expand_control_ramp " &optional snd) -> current expansion ramp time (.4)"
  return(sp_fread(snd_n, SP_EXPAND_RAMP, S_expand_control_ramp));
}

static XEN g_set_expand_control_ramp(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_expand_control_ramp, "a number");
  return(sp_fwrite(snd_n, on, SP_EXPAND_RAMP, "set-" S_expand_control_ramp));
}

WITH_REVERSED_ARGS(g_set_expand_control_ramp_reversed, g_set_expand_control_ramp)

static XEN g_expand_control_hop(XEN snd_n) 
{
  #define H_expand_control_hop "(" S_expand_control_hop " &optional snd) -> current expansion output grain spacing in seconds (0.05)"
  return(sp_fread(snd_n, SP_EXPAND_HOP, S_expand_control_hop));
}

static XEN g_set_expand_control_hop(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_expand_control_hop, "a number"); 
  return(sp_fwrite(snd_n, on, SP_EXPAND_HOP, "set-" S_expand_control_hop));
}

WITH_REVERSED_ARGS(g_set_expand_control_hop_reversed, g_set_expand_control_hop)

static XEN g_speed_control(XEN snd_n) 
{
  #define H_speed_control "(" S_speed_control " &optional snd) -> current speed (srate) slider setting"
  return(sp_fread(snd_n, SP_SPEED, S_speed_control));
}

static XEN g_set_speed_control(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_speed_control, "a number"); 
  return(sp_fwrite(snd_n, on, SP_SPEED, "set-" S_speed_control));
}

WITH_REVERSED_ARGS(g_set_speed_control_reversed, g_set_speed_control)

static XEN g_reverb_control_length(XEN snd_n) 
{
  #define H_reverb_control_length "(" S_reverb_control_length " &optional snd) -> reverb decay length scaler"
  return(sp_fread(snd_n, SP_REVERB_LENGTH, S_reverb_control_length));
}

static XEN g_set_reverb_control_length(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_reverb_control_length, "a number"); 
  return(sp_fwrite(snd_n, on, SP_REVERB_LENGTH, "set-" S_reverb_control_length));
}

WITH_REVERSED_ARGS(g_set_reverb_control_length_reversed, g_set_reverb_control_length)

static XEN g_reverb_control_feedback(XEN snd_n) 
{
  #define H_reverb_control_feedback "(" S_reverb_control_feedback " &optional snd) -> reverb feedback scaler"
  return(sp_fread(snd_n, SP_REVERB_FEEDBACK, S_reverb_control_feedback));
}

static XEN g_set_reverb_control_feedback(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_reverb_control_feedback, "a number"); 
  return(sp_fwrite(snd_n, on, SP_REVERB_FEEDBACK, "set-" S_reverb_control_feedback));
}

WITH_REVERSED_ARGS(g_set_reverb_control_feedback_reversed, g_set_reverb_control_feedback)

static XEN g_reverb_control_scale(XEN snd_n) 
{
  #define H_reverb_control_scale "(" S_reverb_control_scale " &optional snd) -> reverb scaler (the amount of reverb)"
  return(sp_fread(snd_n, SP_REVERB_SCALE, S_reverb_control_scale));
}

static XEN g_set_reverb_control_scale(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_reverb_control_scale, "a number"); 
  return(sp_fwrite(snd_n, on, SP_REVERB_SCALE, "set-" S_reverb_control_scale));
}

WITH_REVERSED_ARGS(g_set_reverb_control_scale_reversed, g_set_reverb_control_scale)

static XEN g_reverb_control_lowpass(XEN snd_n) 
{
  #define H_reverb_control_lowpass "(" S_reverb_control_lowpass " &optional snd) -> reverb lowpass filter coefficient"
  return(sp_fread(snd_n, SP_REVERB_LOW_PASS, S_reverb_control_lowpass));
}

static XEN g_set_reverb_control_lowpass(XEN on, XEN snd_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, "set-" S_reverb_control_lowpass, "a number"); 
  return(sp_fwrite(snd_n, on, SP_REVERB_LOW_PASS, "set-" S_reverb_control_lowpass));
}

WITH_REVERSED_ARGS(g_set_reverb_control_lowpass_reversed, g_set_reverb_control_lowpass)

static XEN g_reverb_control_decay(XEN snd)
{
  #define H_reverb_control_decay "(" S_reverb_control_decay " &optional (snd #t)) -> 'Apply' button reverb decay time (1.0 seconds)"
  if (XEN_BOUND_P(snd))
    return(sp_fread(snd, SP_REVERB_DECAY, S_reverb_control_decay));
  return(C_TO_XEN_DOUBLE(reverb_control_decay(get_global_state())));
}

static XEN g_set_reverb_control_decay(XEN val, XEN snd)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, "set-" S_reverb_control_decay, "a number"); 
  if (XEN_BOUND_P(snd))
    return(sp_fwrite(snd, val, SP_REVERB_DECAY, "set-" S_reverb_control_decay));
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
  ASSERT_SOUND("set-" S_filter_control_env, snd_n, 2);
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error("set-" S_filter_control_env, snd_n));
  if (sp->filter_control_env) sp->filter_control_env = free_env(sp->filter_control_env);  /* set to null in case get_env throws error */
  sp->filter_control_env = get_env(edata, "set-" S_filter_control_env);
  filter_env_changed(sp, sp->filter_control_env);
  return(edata);
}

static XEN g_filter_control_env(XEN snd_n)
{
  #define H_filter_control_env "(" S_filter_control_env " &optional snd) -> snd's filter envelope (in the control panel)"
  snd_info *sp = NULL;
  ASSERT_SOUND(S_filter_control_env, snd_n, 1);
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_filter_control_env, snd_n));
  return(env_to_xen(sp->filter_control_env)); 
}

WITH_REVERSED_ARGS(g_set_filter_control_env_reversed, g_set_filter_control_env)

static XEN g_apply_controls(XEN snd, XEN choice, XEN beg, XEN dur)
{
  #define H_apply_controls "(" S_apply_controls " &optional snd choice beg dur) is equivalent to clicking the control panel 'Apply' button.\
The 'choices' are 0 (apply to sound), 1 (apply to channel), and 2 (apply to selection).  If 'beg' is given, the apply starts there."

  snd_info *sp;
  snd_state *ss;
  apply_state *ap;
  ASSERT_SOUND(S_apply_controls, snd, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(choice), choice, XEN_ARG_2, S_apply_controls, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(beg), beg, XEN_ARG_3, S_apply_controls, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(dur), dur, XEN_ARG_4, S_apply_controls, "an integer");
  sp = get_sp(snd);
  if (sp) 
    {
      ss = sp->state;
      if (XEN_INTEGER_P(beg)) apply_beg = XEN_TO_C_INT(beg); else apply_beg = 0;
      if (XEN_INTEGER_P(dur)) apply_dur = XEN_TO_C_INT(dur); else apply_dur = 0;
      ss->apply_choice = mus_iclamp(0, XEN_TO_C_INT_OR_ELSE(choice, 0), 2);
      sp->applying = 1;
      ap = (apply_state *)make_apply_state((void *)sp);
      if (ap)
	while (apply_controls((GUI_POINTER)ap) == BACKGROUND_CONTINUE);
      return(snd);
    }
  return(snd_no_such_sound_error(S_apply_controls, snd));
}

#if (!USE_NO_GUI)
static XEN g_sound_widgets(XEN snd)
{
  #define H_sound_widgets "(" S_sound_widgets " snd) -> list of widgets \
((0)pane (1)name (2)ctrls (3)minibuffer (4)play (5)filter-env (6)combine (7)name-label (8)name-icon)"
  /* perhaps this belongs in snd-xsnd where we can grab every widget */
  snd_info *sp;
  ASSERT_SOUND(S_sound_widgets, snd, 1);
  sp = get_sp(snd);
  return(XEN_CONS(XEN_WRAP_C_POINTER(w_snd_pane(sp)),
	  XEN_CONS(XEN_WRAP_C_POINTER(w_snd_name(sp)),
           XEN_CONS(XEN_WRAP_C_POINTER(w_snd_ctrls(sp)),
	    XEN_CONS(XEN_WRAP_C_POINTER(w_snd_minibuffer(sp)),
	     XEN_CONS(XEN_WRAP_C_POINTER(w_snd_play(sp)),
	      XEN_CONS(XEN_WRAP_C_POINTER(w_snd_filter_env(sp)), /* this is the drawingarea widget */
	       XEN_CONS(XEN_WRAP_C_POINTER(w_snd_combine(sp)),
	        XEN_CONS(XEN_WRAP_C_POINTER(w_snd_minibuffer_label(sp)),
	         XEN_CONS(XEN_WRAP_C_POINTER(w_snd_name_icon(sp)),
	          XEN_EMPTY_LIST))))))))));
}
#endif

static XEN g_peak_env_info(XEN snd, XEN chn, XEN pos)
{
  #define H_peak_env_info "(" S_peak_env_info " snd chn pos) -> '(complete ymin ymax)"
  chan_info *cp;
  env_info *ep;
  chan_context *cgx;
  ASSERT_CHANNEL(S_peak_env_info, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(pos), pos, XEN_ARG_3, S_peak_env_info, "an integer");
  cp = get_cp(snd, chn, S_peak_env_info);
  cgx = cp->cgx;
  if ((!cgx) || (!(cp->amp_envs))) 
    return(XEN_EMPTY_LIST);
  ep = cp->amp_envs[XEN_TO_C_INT_OR_ELSE(pos, cp->edit_ctr)];
  if (ep)
    return(XEN_LIST_3(C_TO_XEN_BOOLEAN(ep->completed),
		      C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(ep->fmin)),
		      C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(ep->fmax))));
  /* don't throw an error here since the env may be in progress */
  return(XEN_EMPTY_LIST);
}

static XEN g_write_peak_env_info_file(XEN snd, XEN chn, XEN name)
{
  #define H_write_peak_env_info_file "(" S_write_peak_env_info_file " snd chn name) saves current peak-env info in file"
  chan_info *cp;
  char *fullname = NULL;
  env_info *ep;
  int fd;
  XEN errstr;
  int ibuf[5];
  MUS_SAMPLE_TYPE mbuf[2];
  ASSERT_CHANNEL(S_write_peak_env_info_file, snd, chn, 1);
  cp = get_cp(snd, chn, S_write_peak_env_info_file);
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_2, S_write_peak_env_info_file, "a string");
  if ((cp->amp_envs) && (cp->amp_envs[0]))
    {
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
  XEN_ERROR(NO_SUCH_ENVELOPE,
	    XEN_LIST_3(C_TO_XEN_STRING(S_write_peak_env_info_file),
		       snd,
		       chn));
  return(snd);
}

static env_info *get_peak_env_info(char *fullname)
{
  env_info *ep;
  int fd;
  int ibuf[5];
  MUS_SAMPLE_TYPE mbuf[2];
  fd = mus_file_open_read(fullname);
  if (fd == -1) return(NULL);
  read(fd, (char *)ibuf, (5 * sizeof(int)));
  if (((ibuf[0] != 0) && (ibuf[0] != 1)) ||
      (ibuf[1] <= 0) ||
      (!(POWER_OF_2_P(ibuf[1]))) ||
      (ibuf[2] <= 0) ||
      (ibuf[4] > ibuf[1]))
    {
#if DEBUGGING
      fprintf(stderr,"bogus peak info file: ibuf: [%d %d %d %d %d]\n", ibuf[0], ibuf[1], ibuf[2], ibuf[3], ibuf[4]);
#endif
      return(NULL);
    }
  ep = (env_info *)CALLOC(1, sizeof(env_info));
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
  return(ep);
}

static XEN g_read_peak_env_info_file(XEN snd, XEN chn, XEN name)
{
  #define H_read_peak_env_info_file "(" S_read_peak_env_info_file " snd chn name) reads stored peak-env info from file"
  /* has to happen in initial_graph_hook to precede add_amp_env */
  chan_info *cp;
  char *fullname;
  ASSERT_CHANNEL(S_read_peak_env_info_file, snd, chn, 1);
  cp = get_cp(snd, chn, S_read_peak_env_info_file);
  fullname = mus_expand_filename(XEN_TO_C_STRING(name));
  cp->amp_envs[0] = get_peak_env_info(fullname);
  if (fullname) FREE(fullname);
  if (cp->amp_envs[0] == NULL)
    return(snd_no_such_file_error(S_read_peak_env_info_file, name));
  /* assume cp->amp_envs already exists (needs change to snd-chn) */
  return(name);
}

static XEN g_env_info_to_vectors(env_info *ep, int len)
{
  XEN res;
  XEN *velts_max, *velts_min;
  int i, j, lim;
  Float incr, x;
  MUS_SAMPLE_TYPE cmax, cmin;
  if (ep->amp_env_size < len) lim = ep->amp_env_size; else lim = len;
  res = XEN_LIST_2(XEN_MAKE_VECTOR(lim, XEN_ZERO),
		   XEN_MAKE_VECTOR(lim, XEN_ZERO));
  snd_protect(res);
  velts_min = XEN_VECTOR_ELEMENTS(XEN_CAR(res));
  velts_max = XEN_VECTOR_ELEMENTS(XEN_CADR(res));
  if (ep->amp_env_size == lim)
    {
      for (i = 0; i < lim; i++)
	{
	  velts_min[i] = C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(ep->data_min[i]));
	  velts_max[i] = C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(ep->data_max[i]));
	}
    }
  else
    {
      incr = (Float)(ep->amp_env_size - 1) / (Float)len; /* make extra room on left */
      cmax = ep->fmin;
      cmin = ep->fmax;
      velts_min[0] = C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(ep->data_min[0]));
      velts_max[0] = C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(ep->data_max[0]));
      for (i = 1, j = 1, x = 0.0; i < ep->amp_env_size; i++)
	{
	  if (ep->data_max[i] > cmax) cmax = ep->data_max[i];
	  if (ep->data_min[i] < cmin) cmin = ep->data_min[i];
	  x += 1.0;
	  if (x >= incr)
	    {
	      velts_min[j] = C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(cmin));
	      velts_max[j++] = C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(cmax));
	      x -= incr;
	      cmax = ep->fmin;
	      cmin = ep->fmax;
	    }
	}
    }
  snd_unprotect(res);
  return(res);
}

typedef struct {
  chan_info *cp;
  env_state *es;
  int len;
  XEN filename;
  XEN func;
} env_tick;

static BACKGROUND_TYPE tick_it(GUI_POINTER pet)
{
  int val;
  env_state *es;
  chan_info *cp;
  XEN peak;
  env_tick *et = (env_tick *)pet;
  es = et->es;
  val = tick_amp_env(cp, es);
  if (val)
    {
      cp = et->cp;
      if (es->sf) free_snd_fd(es->sf);
      FREE(es);
      peak = g_env_info_to_vectors(cp->amp_envs[0], et->len);
      snd_protect(peak);
      XEN_CALL_3(et->func,
		 et->filename,
		 C_TO_XEN_INT(cp->chan),
		 peak,
		 __FUNCTION__);
      snd_unprotect(et->func);
      snd_unprotect(peak);
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
  #define H_channel_amp_envs "(" S_channel_amp_envs " file chan size peak-file-func work-proc-func)\n\
return two vectors of length 'size' containing y vals (min and max) of file's channel chan's amp envs. \
'peak-file-func' if any is used to get the name of the associated peak_env_info file if the file is very large. \
'work-proc-func' is called when the amp envs are ready if the amp envs are gathered in the background."

  char *fullname, *peakname;
  int len, chn;
  snd_info *sp;
  chan_info *cp;
  XEN peak = XEN_FALSE;
  env_info *ep;
  env_state *es;
  snd_state *ss;
  int id;
  env_tick *et;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_channel_amp_envs, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_channel_amp_envs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pts), pts, XEN_ARG_3, S_channel_amp_envs, "an integer");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(peak_func) || XEN_NOT_BOUND_P(peak_func), peak_func, XEN_ARG_4, S_channel_amp_envs, "a procedure");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(done_func) || XEN_NOT_BOUND_P(done_func), done_func, XEN_ARG_5, S_channel_amp_envs, "a procedure");
  ss = get_global_state();
  fullname = mus_expand_filename(XEN_TO_C_STRING(filename));
  chn = XEN_TO_C_INT(chan);
  len = XEN_TO_C_INT(pts);
  /* look for sp->filename = fullname
     then peak
     then read direct (via make_sound_readable)
  */
  sp = find_sound(ss, fullname);
  if (sp)
    {
      cp = sp->chans[chn];
      if ((cp->amp_envs) && (cp->amp_envs[0]))
	{
	  /* read amp_env data into pts (presumably smaller) */
	  peak = g_env_info_to_vectors(cp->amp_envs[0], len);
	  if (fullname) FREE(fullname);
	  return(peak);
	}
    }
  if (XEN_PROCEDURE_P(peak_func))
    {
      peak = XEN_CALL_2(peak_func,
			filename,
			chan,
			__FUNCTION__);
      if (XEN_STRING_P(peak))
	{
	  peakname = mus_expand_filename(XEN_TO_C_STRING(peak));
	  if (mus_file_probe(peakname))
	    {
	      ep = get_peak_env_info(peakname);
	      if (ep)
		{
		  /* read amp_env data into pts (presumably smaller) */
		  peak = g_env_info_to_vectors(ep, len);
		  FREE(ep);
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
  sp = make_sound_readable(ss, fullname, FALSE);
  if (sp)
    {
      cp = sp->chans[chn];
      cp->edit_ctr = 0;
      cp->active = 1;
      es = make_env_state(cp, cp->samples[0]);
      if (XEN_PROCEDURE_P(done_func))
	{
	  et = (env_tick *)CALLOC(1, sizeof(env_tick));
	  et->cp = cp;
	  et->es = es;
	  et->func = done_func;
	  et->len = len;
	  et->filename = filename;
	  snd_protect(done_func);
	  id = (int)BACKGROUND_ADD(ss, tick_it, (GUI_POINTER)et);
	  if (fullname) FREE(fullname);
	  return(C_TO_XEN_INT(id));
	}
      while (tick_amp_env(cp, es) == FALSE);
      if (es->sf) free_snd_fd(es->sf);
      FREE(es);
      peak = g_env_info_to_vectors(cp->amp_envs[0], len);
      completely_free_snd_info(sp);
    }
  if (fullname) FREE(fullname);
  return(xen_return_first(peak, peak_func));
}


#ifdef XEN_ARGIFY_1
#if (!USE_NO_GUI)
XEN_ARGIFY_1(g_sound_widgets_w, g_sound_widgets)
#endif
XEN_ARGIFY_1(g_sound_p_w, g_sound_p)
XEN_ARGIFY_2(g_bomb_w, g_bomb)
XEN_NARGIFY_1(g_find_sound_w, g_find_sound)
XEN_ARGIFY_1(g_channels_w, g_channels)
XEN_ARGIFY_2(g_set_channels_w, g_set_channels)
XEN_ARGIFY_1(g_srate_w, g_srate)
XEN_ARGIFY_2(g_set_srate_w, g_set_srate)
XEN_ARGIFY_1(g_data_location_w, g_data_location)
XEN_ARGIFY_2(g_set_data_location_w, g_set_data_location)
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
XEN_ARGIFY_6(g_new_sound_w, g_new_sound)
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
#else
#if (!USE_NO_GUI)
#define g_sound_widgets_w g_sound_widgets
#endif
#define g_sound_p_w g_sound_p
#define g_bomb_w g_bomb
#define g_find_sound_w g_find_sound
#define g_channels_w g_channels
#define g_set_channels_w g_set_channels
#define g_srate_w g_srate
#define g_set_srate_w g_set_srate
#define g_data_location_w g_data_location
#define g_set_data_location_w g_set_data_location
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
#endif

void g_init_snd(void)
{
  #define H_name_click_hook S_name_click_hook " (snd) is called when sound name clicked. \
If it returns #t, the usual informative minibuffer babbling is squelched."

  #define H_before_apply_hook S_before_apply_hook " (snd) is called when 'Apply' is clicked or apply-controls called. \
If it returns #t, the apply is aborted."

  #define H_after_apply_hook S_after_apply_hook " (snd) is called when 'Apply' finishes."

  XEN_DEFINE_HOOK(name_click_hook,   S_name_click_hook,   1, H_name_click_hook);       /* args = snd-index */
  XEN_DEFINE_HOOK(before_apply_hook, S_before_apply_hook, 1, H_before_apply_hook);     /* args = snd-index */
  XEN_DEFINE_HOOK(after_apply_hook,  S_after_apply_hook,  1, H_after_apply_hook);      /* args = snd-index */

  #define H_channels_separate "The value for " S_channel_style " that causes channel graphs to occupy separate panes"
  #define H_channels_combined "The value for " S_channel_style " that causes channel graphs to occupy one panes (the 'unite' button)"
  #define H_channels_superimposed "The value for " S_channel_style " that causes channel graphs to occupy one pane and one axis"

  XEN_DEFINE_CONSTANT(S_channels_separate,     CHANNELS_SEPARATE,     H_channels_separate);
  XEN_DEFINE_CONSTANT(S_channels_combined,     CHANNELS_COMBINED,     H_channels_combined);
  XEN_DEFINE_CONSTANT(S_channels_superimposed, CHANNELS_SUPERIMPOSED, H_channels_superimposed);

#if (!USE_NO_GUI)
  XEN_DEFINE_PROCEDURE(S_sound_widgets, g_sound_widgets_w, 0, 1, 0, "returns sound widgets");
#endif

  XEN_DEFINE_PROCEDURE(S_sound_p, g_sound_p_w, 0, 1, 0, H_sound_p);
  XEN_DEFINE_PROCEDURE(S_bomb, g_bomb_w, 0, 2, 0, H_bomb);
  XEN_DEFINE_PROCEDURE(S_find_sound, g_find_sound_w, 1, 0, 0, H_find_sound);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_channels, g_channels_w, H_channels, "set-" S_channels, g_set_channels_w,  0, 1, 0, 2);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_chans, g_channels_w, H_channels, "set-" S_chans, g_set_channels_w,  0, 1, 0, 2);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_srate, g_srate_w, H_srate, "set-" S_srate, g_set_srate_w,  0, 1, 0, 2);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_location, g_data_location_w, H_data_location, "set-" S_data_location, g_set_data_location_w,  0, 1, 0, 2);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_format, g_data_format_w, H_data_format, "set-" S_data_format, g_set_data_format_w,  0, 1, 0, 2);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_header_type, g_header_type_w, H_header_type, "set-" S_header_type, g_set_header_type_w,  0, 1, 0, 2);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_comment, g_comment_w, H_comment, "set-" S_comment, g_set_comment_w,  0, 1, 0, 2);

  XEN_DEFINE_PROCEDURE(S_file_name,             g_file_name_w, 0, 1, 0,             H_file_name);
  XEN_DEFINE_PROCEDURE(S_short_file_name,       g_short_file_name_w, 0, 1, 0,       H_short_file_name);
  XEN_DEFINE_PROCEDURE(S_save_controls,         g_save_controls_w, 0, 1, 0,         H_save_controls);
  XEN_DEFINE_PROCEDURE(S_restore_controls,      g_restore_controls_w, 0, 1, 0,      H_restore_controls);
  XEN_DEFINE_PROCEDURE(S_reset_controls,        g_reset_controls_w, 0, 1, 0,        H_reset_controls);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_sound, g_selected_sound_w, H_selected_sound, "set-" S_selected_sound, g_select_sound_w,  0, 0, 0, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_channel, g_selected_channel_w, H_selected_channel, "set-" S_selected_channel, g_set_selected_channel_w,  0, 1, 0, 2);
  XEN_DEFINE_PROCEDURE(S_select_sound, g_select_sound_w, 0, 1, 0, H_select_sound);
  XEN_DEFINE_PROCEDURE(S_select_channel, g_select_channel_w, 0, 1, 0, H_select_channel);

  XEN_DEFINE_PROCEDURE(S_close_sound,          g_close_sound_w, 0, 1, 0,          H_close_sound);
  XEN_DEFINE_PROCEDURE(S_update_sound,         g_update_sound_w, 0, 1, 0,         H_update_sound);
  XEN_DEFINE_PROCEDURE(S_save_sound,           g_save_sound_w, 0, 1, 0,           H_save_sound);
  XEN_DEFINE_PROCEDURE(S_open_sound,           g_open_sound_w, 1, 0, 0,           H_open_sound);
  XEN_DEFINE_PROCEDURE(S_open_raw_sound,       g_open_raw_sound_w, 4, 0, 0,       H_open_raw_sound);
  XEN_DEFINE_PROCEDURE(S_view_sound,           g_view_sound_w, 1, 0, 0,           H_view_sound);
  XEN_DEFINE_PROCEDURE(S_new_sound,            g_new_sound_w, 0, 6, 0,            H_new_sound);
  XEN_DEFINE_PROCEDURE(S_revert_sound,         g_revert_sound_w, 0, 1, 0,         H_revert_sound);
  XEN_DEFINE_PROCEDURE(S_save_sound_as,        g_save_sound_as_w, 1, 6, 0,        H_save_sound_as);
  XEN_DEFINE_PROCEDURE(S_apply_controls,       g_apply_controls_w, 0, 4, 0,       H_apply_controls);


  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_env, g_filter_control_env_w, H_filter_control_env,
					    "set-" S_filter_control_env, g_set_filter_control_env_w, g_set_filter_control_env_reversed,
					    0, 1, 0, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_cursor_follows_play, g_cursor_follows_play_w, H_cursor_follows_play,
					    "set-" S_cursor_follows_play, g_set_cursor_follows_play_w, g_set_cursor_follows_play_reversed,
					    0, 1, 0, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_controls, g_show_controls_w, H_show_controls,
					    "set-" S_show_controls, g_set_show_controls_w, g_set_show_controls_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sync, g_sync_w, H_sync,
					    "set-" S_sync, g_set_sync_w, g_set_sync_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_channel_style, g_channel_style_w, H_channel_style,
					    "set-" S_channel_style, g_set_channel_style_w, g_set_channel_style_reversed,
					    0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_read_only, g_read_only_w, H_read_only,
					    "set-" S_read_only, g_set_read_only_w, g_set_read_only_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_p, g_expand_control_p_w, H_expand_control_p,
					    "set-" S_expand_control_p, g_set_expand_control_p_w, g_set_expand_control_p_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control_p, g_contrast_control_p_w, H_contrast_control_p,
					    "set-" S_contrast_control_p, g_set_contrast_control_p_w, g_set_contrast_control_p_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_p, g_reverb_control_p_w, H_reverb_control_p,
					    "set-" S_reverb_control_p, g_set_reverb_control_p_w, g_set_reverb_control_p_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_p, g_filter_control_p_w, H_filter_control_p,
					    "set-" S_filter_control_p, g_set_filter_control_p_w, g_set_filter_control_p_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_in_dB, g_filter_control_in_dB_w, H_filter_control_in_dB,
					    "set-" S_filter_control_in_dB, g_set_filter_control_in_dB_w, g_set_filter_control_in_dB_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_order, g_filter_control_order_w, H_filter_control_order,
					    "set-" S_filter_control_order, g_set_filter_control_order_w, g_set_filter_control_order_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control, g_contrast_control_w, H_contrast_control,
					    "set-" S_contrast_control, g_set_contrast_control_w, g_set_contrast_control_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control_amp, g_contrast_control_amp_w, H_contrast_control_amp,
					    "set-" S_contrast_control_amp, g_set_contrast_control_amp_w, g_set_contrast_control_amp_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control, g_expand_control_w, H_expand_control,
					    "set-" S_expand_control, g_set_expand_control_w, g_set_expand_control_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_length, g_expand_control_length_w, H_expand_control_length,
					    "set-" S_expand_control_length, g_set_expand_control_length_w, g_set_expand_control_length_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_ramp, g_expand_control_ramp_w, H_expand_control_ramp,
					    "set-" S_expand_control_ramp, g_set_expand_control_ramp_w, g_set_expand_control_ramp_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_hop, g_expand_control_hop_w, H_expand_control_hop,
					    "set-" S_expand_control_hop, g_set_expand_control_hop_w, g_set_expand_control_hop_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_speed_control, g_speed_control_w, H_speed_control,
					    "set-" S_speed_control, g_set_speed_control_w, g_set_speed_control_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_length, g_reverb_control_length_w, H_reverb_control_length,
					    "set-" S_reverb_control_length, g_set_reverb_control_length_w, g_set_reverb_control_length_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_scale, g_reverb_control_scale_w, H_reverb_control_scale,
					    "set-" S_reverb_control_scale, g_set_reverb_control_scale_w, g_set_reverb_control_scale_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_feedback, g_reverb_control_feedback_w, H_reverb_control_feedback,
					    "set-" S_reverb_control_feedback, g_set_reverb_control_feedback_w, g_set_reverb_control_feedback_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_lowpass, g_reverb_control_lowpass_w, H_reverb_control_lowpass,
					    "set-" S_reverb_control_lowpass, g_set_reverb_control_lowpass_w, g_set_reverb_control_lowpass_reversed,
					    0, 1, 0, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_amp_control, g_amp_control_w, H_amp_control,
					    "set-" S_amp_control, g_set_amp_control_w, g_set_amp_control_reversed,
					    0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_decay, g_reverb_control_decay_w, H_reverb_control_decay,
					    "set-" S_reverb_control_decay, g_set_reverb_control_decay_w, g_set_reverb_control_decay_reversed,
					    0, 1, 0, 2);
  
  #define H_speed_control_as_float "The value for " S_speed_control_style " that interprets the speed slider as a float"
  #define H_speed_control_as_ratio "The value for " S_speed_control_style " that interprets the speed slider as a just-intonation ratio"
  #define H_speed_control_as_semitone "The value for " S_speed_control_style " that interprets the speed slider as a microtone (via " S_speed_control_tones ")"
  
  XEN_DEFINE_CONSTANT(S_speed_control_as_float,        SPEED_CONTROL_AS_FLOAT,    H_speed_control_as_float);
  XEN_DEFINE_CONSTANT(S_speed_control_as_ratio,        SPEED_CONTROL_AS_RATIO,    H_speed_control_as_ratio);
  XEN_DEFINE_CONSTANT(S_speed_control_as_semitone,     SPEED_CONTROL_AS_SEMITONE, H_speed_control_as_semitone);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_speed_control_style, g_speed_control_style_w, H_speed_control_style,
					    "set-" S_speed_control_style, g_set_speed_control_style_w, g_set_speed_control_style_reversed,
					    0, 1, 0, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_speed_control_tones, g_speed_control_tones_w, H_speed_control_tones,
					    "set-" S_speed_control_tones, g_set_speed_control_tones_w, g_set_speed_control_tones_reversed,
					    0, 1, 0, 2);

  XEN_DEFINE_PROCEDURE(S_peak_env_info, g_peak_env_info_w, 0, 3, 0, H_peak_env_info);
  XEN_DEFINE_PROCEDURE(S_write_peak_env_info_file, g_write_peak_env_info_file_w, 3, 0, 0, H_write_peak_env_info_file);
  XEN_DEFINE_PROCEDURE(S_read_peak_env_info_file,  g_read_peak_env_info_file_w,  3, 0, 0, H_read_peak_env_info_file);
  XEN_DEFINE_PROCEDURE(S_channel_amp_envs, g_channel_amp_envs_w, 3, 2, 0, H_channel_amp_envs);
}

