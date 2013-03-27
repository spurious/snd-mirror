#include "snd.h"
#include "sndlib-strings.h"
#include "clm2xen.h"

/* TODO: move apply-controls out of C
 */

static snd_info *get_sp_1(int index)
{
  if ((index >= 0) &&
      (index < ss->max_sounds) && 
      (snd_ok(ss->sounds[index])))
    return(ss->sounds[index]);

  return(NULL);
}

snd_info *get_sp(XEN snd)
{
  if (XEN_SOUND_P(snd))
    return(get_sp_1(xen_sound_to_int(snd)));
      
  if (XEN_INTEGER_P(snd))
    return(get_sp_1(XEN_TO_C_INT(snd)));

  if (XEN_NUMBER_P(snd)) /* actually we only want to accept boolean and #<undefined> here */
    return(NULL);

  /* use default sound, if any */
  return(any_selected_sound());
}


snd_info *snd_new_file(const char *newname, int header_type, int data_format, int srate, int chans, const char *new_comment, mus_long_t samples)
{
  /* caller checks newname != null, and runs overwrite hook */
  if (mus_header_writable(header_type, data_format))
    {
      io_error_t err;
      err = snd_write_header(newname, header_type, srate, chans, samples * chans, data_format, new_comment, NULL);
      if (err != IO_NO_ERROR)
	snd_error("%s %s: %s", 
		  io_error_name(err),
		  newname, 
		  snd_io_strerror());
      else
	{
	  int chan;
	  mus_long_t size;
	  /* send out the initial samples */
	  chan = snd_reopen_write(newname);
	  lseek(chan, mus_header_data_location(), SEEK_SET);
	  size = chans * mus_samples_to_bytes(data_format, samples);
	  if (size > 0)
	    {
	      ssize_t bytes;
	      unsigned char *buf = NULL;
	      buf = (unsigned char *)calloc(size, sizeof(unsigned char));
	      bytes = write(chan, buf, size);
	      if (bytes == 0)
		fprintf(stderr, "%s: write error", newname);
	      free(buf);
	    }
	  snd_close(chan, newname);
	  ss->open_requestor = FROM_NEW_FILE_DIALOG;
	  return(sound_is_silence(snd_open_file(newname, FILE_READ_WRITE)));
	}
    }
  else 
    snd_error("%s: can't write %s header with %s data format",
	      newname,
	      mus_header_type_name(header_type),
	      mus_data_format_name(data_format));
  return(NULL);
}


/* ---------------- peak amp envs ---------------- */

typedef struct env_state {
  int slice, edpos;
  mus_long_t samples, m;  
  peak_env_info *ep; 
  snd_fd *sf;

  unsigned char *direct_data;
  int format, chans, bytes, fd;
  bool file_open;
} env_state;


static env_state *free_env_state(env_state *es)
{
  if (es)
    {
      if (es->sf)
	es->sf = free_snd_fd(es->sf);
      if (es->file_open)
	{
	  close(es->fd);
	  es->file_open = false;
	}
      if (es->direct_data)
	{
	  free(es->direct_data);
	  es->direct_data = NULL;
	}
      free(es);
    }
  return(NULL);
}


peak_env_info *free_peak_env_info(peak_env_info *ep)
{
  if (ep)
    {
      if (ep->data_max) {free(ep->data_max); ep->data_max = NULL;}
      if (ep->data_min) {free(ep->data_min); ep->data_min = NULL;}
      free(ep);
    }
  return(NULL);
}


peak_env_info *free_peak_env(chan_info *cp, int pos)
{
  /* can be either during channel close, or premature work proc removal */
  if ((cp) && 
      (cp->edits) &&
      (pos < cp->edit_size) &&
      (cp->edits[pos]->peak_env))
    {
      free_peak_env_info(cp->edits[pos]->peak_env);
      cp->edits[pos]->peak_env = NULL;
    }
  return(NULL);
}


/* during processing, cp->peak_env_state -> env_state for that channel
 *  cp->peak_env_in_progress is the associated X work proc
 */

void free_peak_env_state(chan_info *cp)
{
  /* env info is tied into cp edit list peak envs immediately upon env start, released via normal cp cleanups */
  /* this function just cleans up the current work proc stuff (amp_env in this case can be incomplete) */
  if (cp)
    {
      cp->peak_env_state = free_env_state(cp->peak_env_state);
      cp->peak_env_in_progress = 0;
    }
}

#define MIN_INIT 1000000.0
#define MAX_INIT -1000000.0

static env_state *make_env_state(chan_info *cp, mus_long_t samples)
{
  int pos, orig_pos;
  peak_env_info *ep;
  env_state *es;

  if (samples <= 0) return(NULL);
  stop_peak_env(cp);
  pos = cp->edit_ctr;
  orig_pos = cp->edits[pos]->edpos; /* don't assume we're editing the preceding state! */
  es = (env_state *)calloc(1, sizeof(env_state)); /* only creation point */
  es->file_open = false;
  es->samples = samples;
  es->slice = 0;
  es->edpos = pos;
  es->m = 0;
  
  es->direct_data = NULL;

  if (cp->edits[pos]->peak_env)
    {
      es->ep = cp->edits[pos]->peak_env;
      ep = es->ep;
    }
  else 
    {
      bool happy = false;
      es->ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
      ep = es->ep;
      if (pos > 0)
	{
	  peak_env_info *old_ep;
	  old_ep = cp->edits[orig_pos]->peak_env;

	  if ((old_ep) && 
	      (old_ep->completed))
	    {
	      mus_long_t start, end, old_samples;

	      /* here in many cases, the underlying edit's amp env has most of the data we need.
	       * cp->edits[cp->edit_ctr] describes the current edit, with beg and end, so in the
	       * simplest case, we can just copy to the bin representing beg, and from the bin
	       * representing end (setting ep->top_bin and ep->bin); if the file's length has
	       * changed dramatically, we need to do it all.  fmin/fmax need to be set as we copy.
	       * as-one-edit can mess this up...
	       */

	      old_samples = cp->edits[orig_pos]->samples;
	      if (snd_abs_mus_long_t(samples - old_samples) < (samples / 2))
		{
		  int start_bin, end_bin, old_end_bin;

		  start = edit_changes_begin_at(cp, cp->edit_ctr);
		  end = edit_changes_end_at(cp, cp->edit_ctr);

		  if (snd_abs_mus_long_t(end - start) < (samples / 2))
		    {
		      int i, j;

		      /* here we'll try to take advantage of an existing envelope */
		      old_ep = cp->edits[orig_pos]->peak_env;
		      ep->samps_per_bin = old_ep->samps_per_bin;
		      ep->peak_env_size = (int)(ceil((double)(es->samples) / (double)(ep->samps_per_bin)));
		      ep->data_max = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
		      ep->data_min = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
		      start_bin = (int)(start / ep->samps_per_bin);
		      ep->fmin = old_ep->data_min[0];
		      ep->fmax = old_ep->data_max[0];
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
			  if (end_bin <= 0)
			    {
			      old_end_bin += (1 - end_bin);
			      end_bin = 1;
			    }

			  for (i = end_bin, j = old_end_bin; (i < ep->peak_env_size) && (j < old_ep->peak_env_size); i++, j++)
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
	  int val;
	  /* we want samps_per_bin to be useful over a wide range of file sizes */
	  /* 160e6 = about a hour at 44KHz */

	  val = (int)(log((double)(es->samples)));
	  if (val > 20) val = 20;
	  ep->peak_env_size = snd_int_pow2(val);
	  ep->samps_per_bin = (int)(ceil((double)(es->samples) / (double)(ep->peak_env_size)));
	  ep->data_max = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
	  ep->data_min = (mus_float_t *)calloc(ep->peak_env_size, sizeof(mus_float_t));
	  ep->bin = 0;
	  ep->top_bin = 0;
	  ep->fmin = 10000000.0;
	  ep->fmax = -10000000.0;
	  /* preset as much as possible of the envelope */
	}
      cp->edits[pos]->peak_env = ep;
      ep->completed = false;
    }
  es->sf = NULL;
  return(es);
}


void start_peak_env_state(chan_info *cp)
{
  cp->peak_env_state = make_env_state(cp, CURRENT_SAMPLES(cp));
}



static bool tick_peak_env(chan_info *cp, env_state *es)
{
  peak_env_info *ep;

  ep = es->ep;
  if (es->slice == 0)
    {
      int n, sb, lm;
      mus_long_t samps_to_read;
      snd_fd *sfd;

      if (ep->top_bin != 0)
	lm = (ep->top_bin - ep->bin + 1);
      else lm = (ep->peak_env_size - ep->bin);
      if (lm <= 0) lm = 1;

      samps_to_read = (mus_long_t)lm * (mus_long_t)(ep->samps_per_bin);
      if (samps_to_read > 1000000)
	{
	  lm = 1000000 / ep->samps_per_bin;
	  samps_to_read = (mus_long_t)lm * (mus_long_t)(ep->samps_per_bin);
	}

      sb = ep->bin;
      if (sb >= ep->peak_env_size)
	{
	  /* oops... */
	  es->slice++;
	  if (es->sf)
	    es->sf = free_snd_fd(es->sf);
	  if (es->direct_data)
	    {
	      free(es->direct_data);
	      es->direct_data = NULL;
	    }
	  ep->completed = true;
	  return(true);
	}

      if ((es->sf == NULL) &&
	  (es->direct_data == NULL))
	{
	  if ((cp->edit_ctr == 0) &&
	      (cp->sound) &&
	      (cp->sound->inuse == SOUND_NORMAL) &&
	      (cp->sound->hdr) &&
	      (cp->sounds) &&
	      (cp->sounds[0] != NULL) &&
	      (cp->sounds[0]->io))
	    {
	      es->fd = mus_file_open_read(cp->sound->filename);
	      if (es->fd == -1)
		{
		  snd_warning("%s no longer exists!", cp->sound->filename);
		  return(true);
		}
	      es->file_open = true;
	      lseek(es->fd, cp->sound->hdr->data_location, SEEK_SET);

	      es->format = cp->sound->hdr->format;
	      es->chans = cp->sound->nchans;
	      es->bytes = ep->samps_per_bin * mus_bytes_per_sample(es->format) * es->chans;
	      es->direct_data = (unsigned char *)malloc(es->bytes * lm);
	    }
	  else es->sf = init_sample_read_any(ep->bin * ep->samps_per_bin, cp, READ_FORWARD, es->edpos);
	}
      
      if (es->direct_data == NULL)
	{
	  sfd = es->sf;
	  if (sfd == NULL) return(false);

	  for (n = 0; (n < lm) && (sb < ep->peak_env_size); n++, sb++)
	    {
	      mus_float_t ymin, ymax, val;
	      int i, lim;
	      val = read_sample(sfd);
	      ymin = val;
	      ymax = val;
	      i = 1;
	      lim = ep->samps_per_bin - 4;
	      while (i <= lim)
		{
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		  i += 4;
		}
	      for (; i < ep->samps_per_bin; i++)
		{
		  val = read_sample(sfd);
		  if (ymin > val) ymin = val; else if (ymax < val) ymax = val;
		}
	      ep->data_max[sb] = ymax;
	      ep->data_min[sb] = ymin;
	      
	      if (ymin < ep->fmin) ep->fmin = ymin;
	      if (ymax > ep->fmax) ep->fmax = ymax;
	    }
	}
      else
	{
	  ssize_t bytes_read;
	  
	  bytes_read = read(es->fd, (char *)(es->direct_data), lm * es->bytes);
	  if (bytes_read < lm * es->bytes)
	    {
	      int zero_byte;
	      zero_byte = mus_data_format_zero(es->format);
	      if ((zero_byte == 0) ||
		  ((es->format != MUS_UBSHORT) &&
		   (es->format != MUS_ULSHORT)))
		memset((void *)(es->direct_data + bytes_read), zero_byte, lm * es->bytes - bytes_read);
	      else /* MUS_UB|LSHORT 32768 or 128(as a short)=>0 */
		{
		  mus_long_t i, start, len;
		  unsigned short *buf;

		  /* (with-sound (:data-format mus-ubshort) (fm-violin 0 2 440 .1)) */

		  buf = (unsigned short *)(es->direct_data);
		  start = bytes_read / 2;
		  len = lm * es->bytes / 2;
		  for (i = start; i < len; i++)
		    buf[i] = (unsigned short)zero_byte;
		}
	    }
	  
	  for (n = 0; (n < lm) && (sb < ep->peak_env_size); n++, sb++)
	    {
	      mus_float_t cur_min = 0.0, cur_max = 0.0;
	      mus_samples_bounds((unsigned char *)(es->direct_data + es->bytes * n), es->bytes, cp->chan, es->chans, es->format, &cur_min, &cur_max);
	      
	      ep->data_max[sb] = cur_max;
	      ep->data_min[sb] = cur_min;
	      
	      if (cur_min < ep->fmin) ep->fmin = cur_min;
	      if (cur_max > ep->fmax) ep->fmax = cur_max;
	    }
	}

      es->m += samps_to_read;
      ep->bin += lm;
      if ((es->m >= es->samples) || 
	  ((ep->top_bin > 0) && (ep->bin >= ep->top_bin))) /* this applies to partial amp envs */
	{
	  es->slice++;

	  if (es->sf)
	    es->sf = free_snd_fd(es->sf);

	  if (es->direct_data)
	    {
	      free(es->direct_data);
	      es->direct_data = NULL;
	    }
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


static XEN peak_env_hook;

static void run_peak_env_hook(chan_info *cp)
{
  if (XEN_HOOKED(peak_env_hook))
    run_hook(peak_env_hook, 
	     XEN_LIST_2(C_INT_TO_XEN_SOUND(cp->sound->index),
			C_TO_XEN_INT(cp->chan)),
	     S_peak_env_hook);
}


void finish_peak_env(chan_info *cp)
{
  if ((cp->peak_env_in_progress) && 
      (cp->peak_env_state))
    {
      while (!(tick_peak_env(cp, cp->peak_env_state))) ; /* finish peak-env scan */
      enved_reflect_peak_env_completion(cp->sound);
      free_peak_env_state(cp);
    }
}


idle_func_t get_peak_env(any_pointer_t ptr)
{
  /* calculate the amp env of channel */
  chan_info *cp = (chan_info *)ptr;
  env_state *es;
  int pos;

  if (!cp) return(BACKGROUND_QUIT);

  pos = cp->edit_ctr;
  if ((pos == -1) || 
      (cp->active < CHANNEL_HAS_EDIT_LIST))
    {
      free_peak_env_state(cp);
      return(BACKGROUND_QUIT);
    }

  if (!(cp->peak_env_state)) 
    cp->peak_env_state = make_env_state(cp, CURRENT_SAMPLES(cp));

  es = cp->peak_env_state;
  if (es)
    {
      if (tick_peak_env(cp, es))
	{
	  free_peak_env_state(cp);
	  enved_reflect_peak_env_completion(cp->sound);
	  if (cp->waiting_to_make_graph) 
	    {
	      cp->waiting_to_make_graph = false;
	      cp->new_peaks = true;
	      update_graph(cp);
	      cp->new_peaks = false;
	    }
	  run_peak_env_hook(cp);
	  return(BACKGROUND_QUIT);
	}
      else return(BACKGROUND_CONTINUE);
    }
  return(BACKGROUND_QUIT);
}


bool peak_env_maxamp_ok(chan_info *cp, int edpos)
{
  if (cp)
    {
      peak_env_info *ep;
      ep = cp->edits[edpos]->peak_env;
      return((ep) && (ep->completed));
    }
  return(false);
}


mus_float_t peak_env_maxamp(chan_info *cp, int edpos)
{
  peak_env_info *ep;
  mus_float_t ymax;
  ep = cp->edits[edpos]->peak_env;
  ymax = -ep->fmin;
  if (ymax < ep->fmax) 
    return(ep->fmax);
  return(ymax);
}


bool peak_env_usable(chan_info *cp, mus_float_t samples_per_pixel, mus_long_t hisamp, bool start_new, int edit_pos, bool finish_env) 
{
  peak_env_info *ep;

#if USE_NO_GUI
  return(false);
#endif

  if (!(cp->edits)) return(false);

  ep = cp->edits[edit_pos]->peak_env;
  if (ep)
    {
      int bin;
      bin = (int)(hisamp / ep->samps_per_bin); 
      if ((ep->completed) || 
	  (bin < ep->bin) || 
	  ((ep->top_bin != 0) && (bin > ep->top_bin)))
	return(samples_per_pixel >= (mus_float_t)(ep->samps_per_bin));
    }

  if ((finish_env) && (cp->peak_env_in_progress) && (cp->peak_env_state))
    {
      /* caller wants data, but a read is in progress -- finish it as quickly as possible */
      finish_peak_env(cp);
      if (cp->waiting_to_make_graph) 
	{
	  cp->waiting_to_make_graph = false;
	  update_graph(cp);
	}
      run_peak_env_hook(cp);
      return(peak_env_usable(cp, samples_per_pixel, hisamp, start_new, edit_pos, false));
    }

  if ((start_new) &&
      (!(cp->peak_env_in_progress)) && 
      (CURRENT_SAMPLES(cp) > PEAK_ENV_CUTOFF) &&
      (cp->sound->short_filename != NULL))             /* region browser jumped in too soon during autotest */
    start_peak_env(cp);
  return(false);
}


static short local_grf_y(mus_float_t val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((short)(ap->y_base + val * ap->y_scale));
}


int peak_env_graph(chan_info *cp, mus_float_t samples_per_pixel, int srate) 
{
  mus_float_t step, x, pinc = 0.0;
  double xf, xk;
  mus_float_t ymin, ymax;
  int xi;
  int j = 0;
  mus_long_t i;
  peak_env_info *ep;
  axis_info *ap;

  ap = cp->axis;
  ep = cp->edits[cp->edit_ctr]->peak_env;
  step = samples_per_pixel / (mus_float_t)(ep->samps_per_bin);
  xf = (double)(ap->losamp) / (double)(ep->samps_per_bin);
  x = ap->x0;
  xi = grf_x(x, ap);
  i = ap->losamp;
  xk = (double)i;
  if (cp->printing) pinc = (mus_float_t)samples_per_pixel / (mus_float_t)srate;
  ymin = ep->fmax;
  ymax = ep->fmin;

  while (i <= ap->hisamp)
    {
      int k, kk;
      k = (int)xf;
      xf += step;
      kk = (int)xf;
      if (kk >= ep->peak_env_size) 
	kk = ep->peak_env_size - 1;
      for (; k <= kk; k++)
	{
	  if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
	  if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
	}
      xk += samples_per_pixel;
      i = (mus_long_t)xk;
      set_grf_points(xi, j,
		     local_grf_y(ymin, ap),
		     local_grf_y(ymax, ap));
      if (cp->printing) 
	{
	  x += pinc; 
	  ps_set_grf_points(x, j, ymin, ymax);
	}
      xi++;
      j++;
      if (j >= POINT_BUFFER_SIZE) break;
      ymin = ep->fmax;
      ymax = ep->fmin;
    }
  return(j);
}


int peak_env_partial_graph(chan_info *cp, mus_long_t beg, mus_long_t end, mus_float_t samples_per_pixel, int srate)
{
  mus_float_t step, x;
  double xf, xk;
  mus_float_t ymin, ymax;
  int xi;
  int j = 0;
  mus_long_t i;
  peak_env_info *ep;
  axis_info *ap;

  ap = cp->axis;
  ep = cp->edits[cp->edit_ctr]->peak_env;
  step = samples_per_pixel / (mus_float_t)(ep->samps_per_bin);
  xf = (double)(beg) / (double)(ep->samps_per_bin);
  x = beg / srate;
  xi = grf_x(x, ap);
  i = beg;
  xk = (double)i;

  ymin = ep->fmax;
  ymax = ep->fmin;

  while (i <= end)
    {
      int k, kk;
      k = (int)xf;
      xf += step;
      kk = (int)xf;
      if (kk >= ep->peak_env_size) 
	kk = ep->peak_env_size - 1;
      for (; k <= kk; k++)
	{
	  if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
	  if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
	}
      xk += samples_per_pixel;
      i = (mus_long_t)xk;
      set_grf_points(xi, j,
		     local_grf_y(ymin, ap),
		     local_grf_y(ymax, ap));
      xi++;
      j++;
      if (j >= POINT_BUFFER_SIZE) break;
      ymin = ep->fmax;
      ymax = ep->fmin;
    }
  return(j);
}


void peak_env_scale_by(chan_info *cp, mus_float_t scl, int pos)
{
  peak_env_info *old_ep;
  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      int i;
      peak_env_info *new_ep;
      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if ((new_ep) && 
	  (new_ep->peak_env_size != old_ep->peak_env_size)) 
	new_ep = free_peak_env(cp, cp->edit_ctr);
      if (new_ep == NULL)
	{
	  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
	  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	}
      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      if (scl >= 0.0)
	{
	  new_ep->fmin = old_ep->fmin * scl;
	  new_ep->fmax = old_ep->fmax * scl;
	  for (i = 0; i < new_ep->peak_env_size; i++) 
	    {
	      new_ep->data_min[i] = old_ep->data_min[i] * scl;
	      new_ep->data_max[i] = old_ep->data_max[i] * scl;
	    }
	}
      else
	{
	  new_ep->fmax = old_ep->fmin * scl;
	  new_ep->fmin = old_ep->fmax * scl;
	  for (i = 0; i < new_ep->peak_env_size; i++) 
	    {
	      new_ep->data_max[i] = old_ep->data_min[i] * scl;
	      new_ep->data_min[i] = old_ep->data_max[i] * scl;
	    }
	}
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
    }
}


void pick_one_bin(peak_env_info *ep, int bin, mus_long_t cursamp, chan_info *cp, int edpos)
{
  snd_fd *sf;
  int n;
  mus_float_t val, ymin, ymax;

  /* here we have to read the current bin using the current fragments */
  sf = init_sample_read_any(cursamp, cp, READ_FORWARD, edpos);
  if (sf == NULL) return;

  val = read_sample(sf); 
  ymin = val;
  ymax = val;

  for (n = 1; n < ep->samps_per_bin; n++)
    {
      val = read_sample(sf); 
      if (ymin > val) ymin = val; 
      if (ymax < val) ymax = val;
    }

  ep->data_max[bin] = ymax;
  ep->data_min[bin] = ymin;
  free_snd_fd(sf);
}


void peak_env_scale_selection_by(chan_info *cp, mus_float_t scl, mus_long_t beg, mus_long_t num, int pos)
{
  peak_env_info *old_ep;

  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      mus_float_t fmax = MAX_INIT, fmin = MIN_INIT;
      mus_long_t cursamp, start, end;
      int i;
      peak_env_info *new_ep;

      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if ((new_ep) && 
	  (new_ep->peak_env_size != old_ep->peak_env_size)) 
	new_ep = free_peak_env(cp, cp->edit_ctr);

      if (new_ep == NULL)
	{
	  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
	  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	}

      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      end = beg + num - 1;
      start = beg - new_ep->samps_per_bin;

      for (i = 0, cursamp = 0; i < new_ep->peak_env_size; i++, cursamp += new_ep->samps_per_bin) 
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
		      new_ep->data_max[i] = old_ep->data_max[i] * scl;
		      new_ep->data_min[i] = old_ep->data_min[i] * scl;
		    }
		  else
		    {
		      new_ep->data_max[i] = old_ep->data_min[i] * scl;
		      new_ep->data_min[i] = old_ep->data_max[i] * scl;
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
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
    }
}


peak_env_info *peak_env_section(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos)
{
  /* used in snd-region.c to create the region peak amp env */
  peak_env_info *old_ep, *new_ep = NULL;
  mus_float_t fmax = MAX_INIT, fmin = MIN_INIT;
  int i, j;
  mus_long_t cursamp, start, end;

  old_ep = cp->edits[edpos]->peak_env;
  if (old_ep == NULL) return(NULL);

  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
  new_ep->peak_env_size = old_ep->peak_env_size;
  new_ep->samps_per_bin = old_ep->samps_per_bin;

  end = beg + num - 1;
  start = beg - new_ep->samps_per_bin;
  for (i = 0, j = 0, cursamp = 0; (i < new_ep->peak_env_size) && (cursamp < end); i++, cursamp += new_ep->samps_per_bin) 
    {
      if (cursamp > start)
	{
	  /* if segment is entirely in region, just copy it */
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


peak_env_info *copy_peak_env_info(peak_env_info *old_ep, bool reversed)
{
  peak_env_info *new_ep = NULL;
  if ((old_ep) && 
      (old_ep->completed))
    {
      int i, j;

      new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
      new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
      new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      new_ep->fmin = old_ep->fmin;
      new_ep->fmax = old_ep->fmax;

      if (reversed)
	{
	  for (i = 0, j = new_ep->peak_env_size - 1; i < new_ep->peak_env_size; i++, j--) 
	    {
	      new_ep->data_min[j] = old_ep->data_min[i];
	      new_ep->data_max[j] = old_ep->data_max[i];
	    }
	}
      else
	{
	  memcpy((void *)new_ep->data_min, (void *)old_ep->data_min, sizeof(mus_float_t) * new_ep->peak_env_size);
	  memcpy((void *)new_ep->data_max, (void *)old_ep->data_max, sizeof(mus_float_t) * new_ep->peak_env_size);
	}

      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
    }
  return(new_ep);
}


peak_env_info *peak_env_copy(chan_info *cp, bool reversed, int edpos)
{
  return(copy_peak_env_info(cp->edits[edpos]->peak_env, reversed));
}


void amp_env_env(chan_info *cp, mus_float_t *brkpts, int npts, int pos, mus_float_t base, mus_float_t scaler, mus_float_t offset)
{
  peak_env_info *old_ep;
  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      int i;
      mus_any *e;
      mus_float_t val, fmin, fmax;
      peak_env_info *new_ep;

      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if ((new_ep) && 
	  (new_ep->peak_env_size != old_ep->peak_env_size)) 
	new_ep = free_peak_env(cp, cp->edit_ctr);

      if (new_ep == NULL)
	{
	  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
	  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	}

      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      if (base == 1.0)
	e = mus_make_env(brkpts, npts, scaler, offset, base, 0.0, new_ep->peak_env_size - 1, brkpts);
      else e = mus_make_env(brkpts, npts, 1.0, 0.0, base, 0.0, new_ep->peak_env_size - 1, brkpts);
      fmin = MIN_INIT;
      fmax = MAX_INIT;

      for (i = 0; i < new_ep->peak_env_size; i++) 
	{
	  val = mus_env(e);
	  if (val >= 0.0)
	    {
	      new_ep->data_min[i] = old_ep->data_min[i] * val;
	      new_ep->data_max[i] = old_ep->data_max[i] * val;
	    }
	  else
	    {
	      new_ep->data_min[i] = old_ep->data_max[i] * val;
	      new_ep->data_max[i] = old_ep->data_min[i] * val;
	    }
	  if (new_ep->data_min[i] < fmin) fmin = new_ep->data_min[i];
	  if (new_ep->data_max[i] > fmax) fmax = new_ep->data_max[i];
	}

      new_ep->fmin = fmin;
      new_ep->fmax = fmax;
      new_ep->completed = true;
      new_ep->bin = old_ep->bin;
      new_ep->top_bin = old_ep->top_bin;
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
      mus_free(e);
    }
}


void amp_env_env_selection_by(chan_info *cp, mus_any *e, mus_long_t beg, mus_long_t num, int pos)
{
  peak_env_info *old_ep;
  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      mus_float_t xmax = 1.0;
      mus_float_t *data;
      mus_float_t fmax = MAX_INIT, fmin = MIN_INIT;
      int i;
      mus_long_t cursamp, start, end;
      peak_env_info *new_ep;

      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if ((new_ep) && 
	  (new_ep->peak_env_size != old_ep->peak_env_size)) 
	new_ep = free_peak_env(cp, cp->edit_ctr);

      if (new_ep == NULL)
	{
	  new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
	  new_ep->data_max = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	  new_ep->data_min = (mus_float_t *)malloc(old_ep->peak_env_size * sizeof(mus_float_t));
	}

      new_ep->peak_env_size = old_ep->peak_env_size;
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      end = beg + num - 1;
      start = beg - new_ep->samps_per_bin;
      data = mus_data(e);
      xmax = data[mus_env_breakpoints(e) * 2 - 2];

      for (i = 0, cursamp = 0; i < new_ep->peak_env_size; i++, cursamp += new_ep->samps_per_bin) 
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
		  mus_float_t val;
		  val = mus_env_interp((double)(cursamp - beg) * xmax / (double)num, e);
		  if (val >= 0.0)
		    {
		      new_ep->data_max[i] = old_ep->data_max[i] * val;
		      new_ep->data_min[i] = old_ep->data_min[i] * val;
		    }
		  else
		    {
		      new_ep->data_max[i] = old_ep->data_min[i] * val;
		      new_ep->data_min[i] = old_ep->data_max[i] * val;
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
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
    }
}


void peak_env_insert_zeros(chan_info *cp, mus_long_t beg, mus_long_t num, int pos)
{
  peak_env_info *old_ep;
  old_ep = cp->edits[pos]->peak_env;
  if ((old_ep) && (old_ep->completed))
    {
      mus_long_t end, old_samps, cur_samps;
      int i, j, subsamp, val, bins;
      peak_env_info *new_ep;

      new_ep = cp->edits[cp->edit_ctr]->peak_env;
      if (new_ep) new_ep = free_peak_env(cp, cp->edit_ctr);

      old_samps = cp->edits[pos]->samples;
      cur_samps = CURRENT_SAMPLES(cp);
      val = (int)(log((double)(cur_samps)));
      if (val > 20) val = 20;
      val = snd_int_pow2(val);
      subsamp = val / old_ep->peak_env_size;
      if (subsamp != 1) return;

      new_ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
      new_ep->samps_per_bin = old_ep->samps_per_bin;
      new_ep->peak_env_size = (int)(ceil(cur_samps / new_ep->samps_per_bin));
      new_ep->completed = true;
      cp->edits[cp->edit_ctr]->peak_env = new_ep;
      new_ep->bin = new_ep->peak_env_size;
      new_ep->top_bin = new_ep->peak_env_size;
      new_ep->data_max = (mus_float_t *)calloc(new_ep->peak_env_size, sizeof(mus_float_t));
      new_ep->data_min = (mus_float_t *)calloc(new_ep->peak_env_size, sizeof(mus_float_t));
      new_ep->fmin = old_ep->fmin;
      if (new_ep->fmin > 0.0) new_ep->fmin = 0.0;
      new_ep->fmax = old_ep->fmax;
      if (new_ep->fmax < 0.0) new_ep->fmax = 0.0;
      end = beg + num - 1;

      if (beg == 0)
	{
	  /* insert at start, so copy to end */
	  i = (int)ceil(end / new_ep->samps_per_bin);
	  bins = new_ep->peak_env_size - i;
	  if (old_ep->peak_env_size < bins) bins = old_ep->peak_env_size;
	  memcpy((void *)(&(new_ep->data_min[i])), (void *)old_ep->data_min, sizeof(mus_float_t) * bins);
	  memcpy((void *)(&(new_ep->data_max[i])), (void *)old_ep->data_max, sizeof(mus_float_t) * bins);
	}
      else
	{
	  if (beg >= old_samps)
	    {
	      /* copy start */
	      bins = (int)floor(beg / old_ep->samps_per_bin);
	      if (bins > old_ep->peak_env_size) bins = old_ep->peak_env_size;
	      memcpy((void *)new_ep->data_min, (void *)old_ep->data_min, sizeof(mus_float_t) * bins);
	      memcpy((void *)new_ep->data_max, (void *)old_ep->data_max, sizeof(mus_float_t) * bins);
	    }
	  else
	    {
	      i = (int)floor(beg / old_ep->samps_per_bin);
	      if (i > 0)
		{
		  memcpy((void *)new_ep->data_min, (void *)old_ep->data_min, sizeof(mus_float_t) * i);
		  memcpy((void *)new_ep->data_max, (void *)old_ep->data_max, sizeof(mus_float_t) * i);
		}
	      if (i < new_ep->peak_env_size)
		{
		  pick_one_bin(new_ep, i, i * old_ep->samps_per_bin, cp, cp->edit_ctr);
		  i++;
		}
	      j = (int)floor(end / new_ep->samps_per_bin);
	      if (j < new_ep->peak_env_size)
		{
		  pick_one_bin(new_ep, j, j * new_ep->samps_per_bin, cp, cp->edit_ctr);
		  j++;
		}
	      if (i < old_ep->peak_env_size)
		{
		  bins = new_ep->peak_env_size - j;
		  if ((i + bins) >= old_ep->peak_env_size)
		    bins = old_ep->peak_env_size - i;
		  memcpy((void *)(&(new_ep->data_min[j])), (void *)(&(old_ep->data_min[i])), sizeof(mus_float_t) * bins);
		  memcpy((void *)(&(new_ep->data_max[j])), (void *)(&(old_ep->data_max[i])), sizeof(mus_float_t) * bins);
		}
	    }
	}
    }
}


#if XEN_HAVE_RATIOS
void snd_rationalize(mus_float_t a, int *num, int *den)
{
  XEN ratio;
  int gloc;
  ratio = XEN_RATIONALIZE(C_TO_XEN_DOUBLE(a), C_TO_XEN_DOUBLE(a * .04)); /* was .02 until 13-Dec-07 but that gives too many useless choices */
  gloc = snd_protect(ratio);
  (*num) = (int)XEN_NUMERATOR(ratio);
  (*den) = (int)XEN_DENOMINATOR(ratio);
  snd_unprotect_at(gloc);
}
#endif


/* -------- control panel speed -------- */

#if (!XEN_HAVE_RATIOS)
#define TOTAL_RATS 123

static const char *rat_names[TOTAL_RATS] = {
  "1/20", "5/96", "7/128", "15/256", "31/512", "1/16", "1/15", "5/72", "9/128", "3/40", "5/64", "1/12", "11/128", "3/32", "1/10", "5/48", "7/64", "15/128", "31/256", "1/8", "2/15", "5/36", "9/64", "3/20", "5/32", "1/6", "11/64", "3/16", "1/5", "5/24", "7/32", "15/64", "31/128", "1/4", "4/15", "5/18", "9/32", "3/10", "5/16", "1/3", "11/32", "3/8", "2/5", "5/12", "7/16", "15/32", "31/64", "1/2", "8/15", "5/9", "9/16", "3/5", "5/8", "2/3", "11/16", "3/4", "4/5", "5/6", "7/8", "15/16", "31/32", "1/1", "16/15", "10/9", "9/8", "6/5", "5/4", "4/3", "11/8", "3/2", "8/5", "5/3", "7/4", "15/8", "31/16", "2/1", "32/15", "20/9", "9/4", "12/5", "5/2", "8/3", "11/4", "3/1", "16/5", "10/3", "7/2", "15/4", "31/8", "4/1", "64/15", "40/9", "9/2", "24/5", "5/1", "16/3", "11/2", "6/1", "32/5", "20/3", "7/1", "15/2", "31/4", "8/1", "128/15", "80/9", "9/1", "48/5", "10/1", "32/3", "11/1", "12/1", "64/5", "40/3", "14/1", "15/1", "31/2", "16/1", "256/15", "160/9", "18/1", "96/5", "20/1"};

static mus_float_t rat_values[TOTAL_RATS] = {
  0.050, 0.052, 0.055, 0.059, 0.061, 0.063, 0.067, 0.069, 0.070, 0.075, 0.078, 0.083, 0.086, 0.094, 0.100, 0.104, 0.109, 0.117, 0.121, 0.125, 0.133, 0.139, 0.141, 0.150, 0.156, 0.167, 0.172, 0.188, 0.200, 0.208, 0.219, 0.234, 0.242, 0.250, 0.267, 0.278, 0.281, 0.300, 0.313, 0.333, 0.344, 0.375, 0.400, 0.417, 0.438, 0.469, 0.484, 0.500, 0.533, 0.556, 0.563, 0.600, 0.625, 0.667, 0.688, 0.750, 0.800, 0.833, 0.875, 0.938, 0.969, 1.000, 1.067, 1.111, 1.125, 1.200, 1.250, 1.333, 1.375, 1.500, 1.600, 1.667, 1.750, 1.875, 1.938, 2.000, 2.133, 2.222, 2.250, 2.400, 2.500, 2.667, 2.750, 3.000, 3.200, 3.333, 3.500, 3.750, 3.875, 4.000, 4.267, 4.444, 4.500, 4.800, 5.000, 5.333, 5.500, 6.000, 6.400, 6.667, 7.000, 7.500, 7.750, 8.000, 8.533, 8.889, 9.000, 9.600, 10.000, 10.667, 11.000, 12.000, 12.800, 13.333, 14.000, 15.000, 15.500, 16.000, 17.067, 17.778, 18.000, 19.200, 20.000};
#endif


mus_float_t speed_changed(mus_float_t val, char *srcbuf, speed_style_t style, int tones, int srcbuf_size)
{
  char numbuf[16];
  int semi, i, j;
  switch (style)
    {
    case SPEED_CONTROL_AS_RATIO:
#if XEN_HAVE_RATIOS
      {
	int num, den;
	snd_rationalize(val, &num, &den);
	mus_snprintf(srcbuf, srcbuf_size, "%d/%d", num, den);
	return((mus_float_t)num / (mus_float_t)den);
      }
#else
      for (i = 1; i < TOTAL_RATS; i++)
	if (rat_values[i] > val) 
	  break;
      if ((rat_values[i] - val) < (val - rat_values[i - 1]))
	{
	  mus_snprintf(srcbuf, srcbuf_size, "%s", rat_names[i]);
	  return(rat_values[i]);
	}
      else
	{
	  mus_snprintf(srcbuf, srcbuf_size, "%s", rat_names[i - 1]);
	  return(rat_values[i - 1]);
	}
#endif
      break;

    case SPEED_CONTROL_AS_SEMITONE: 
      /* find closest semitone to val */
      semi = snd_round(log(val) * ((mus_float_t)tones / log(2.0)));
      /* space until (-) num (-52 to 52 is its range if 12-tone) */
      for (i = 0; i < srcbuf_size; i++) srcbuf[i] = ' '; 
      mus_snprintf(numbuf, 16, "%d", semi);
      j = strlen(numbuf) - 1;
      for (i = 3; (i >= 0) && (j >= 0); i--, j--) 
	srcbuf[i] = numbuf[j];
      srcbuf[srcbuf_size - 1] = 0;
      return(pow(2.0, ((mus_float_t)semi / (mus_float_t)tones)));
      break;

    default: 
      mus_snprintf(srcbuf, srcbuf_size, "%.3f", val);
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
  if (show_indices(ss))
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


#if HAVE_READLINK
static char *linked_file(const char *link_name)
{
  char *link_file;
  ssize_t bytes;
  #define READLINK_FILE_SIZE 256
  link_file = (char *)calloc(READLINK_FILE_SIZE, sizeof(char));
  bytes = readlink(link_name, link_file, READLINK_FILE_SIZE);
  link_file[bytes] = 0;
  return(link_file);
}
#endif


static XEN name_click_hook;

char *sp_name_click(snd_info *sp) /* caller should free returned string */
{
  if (sp)
    {
      file_info *hdr;

      /* call name-click-hook (if any) return #t = don't print info in the status area */
      if ((XEN_HOOKED(name_click_hook)) &&
	  (XEN_TRUE_P(run_or_hook(name_click_hook, 
				  XEN_LIST_1(C_INT_TO_XEN_SOUND(sp->index)),
				  S_name_click_hook))))
	return(NULL);

      hdr = sp->hdr;
      if (hdr)
	{
	  mus_float_t dur;
	  char *result, *str = NULL;

	  bool linked = false;
	  linked = link_p(sp->filename);
	  dur = (mus_float_t)((double)(hdr->samples) / (double)(hdr->chans * hdr->srate));
	  result = mus_format("%d, %d chan%s, %.3f sec%s, %s: %s, %s%s%s%s",
			       hdr->srate,
			       hdr->chans,
			       ((hdr->chans > 1) ? "s" : ""),
			       dur,
			       ((dur == 1.0) ? "" : "s"),
			       mus_header_type_to_string(hdr->type),
			       mus_data_format_to_string(hdr->format),
			       snd_strftime("%d-%b-%Y %H:%M", sp->write_date),
			       (linked) ? ", (link to " : "",
#if HAVE_READLINK
			       (linked) ? str = linked_file(sp->filename) : "",
#else
			       (linked) ? "?" : "",
#endif
			       (linked) ? ")" : "");
	  if (str) free(str);
	  return(result);
	}
    }
  return(NULL);
}



/* ---------------- save and restore control panel buttons ----------------*/

typedef struct ctrl_state {
  mus_float_t amp, speed, contrast, expand, revscl, revlen;
  env *filter_env;
  bool expand_on, contrast_on, reverb_on, filter_on, reversed;
  int filter_order;
  mus_float_t contrast_amp, expand_ramp, expand_length, expand_hop, expand_jitter, reverb_feedback, reverb_decay, reverb_lowpass;
} ctrl_state;


static ctrl_state *free_control_settings(ctrl_state *cs)
{
  if (cs)
    {
      if (cs->filter_env) free_env(cs->filter_env);
      free(cs);
    }
  return(NULL);
}


void free_controls(snd_info *sp)
{
  sp->saved_controls = free_control_settings(sp->saved_controls);
}


static ctrl_state *current_control_settings(snd_info *sp, ctrl_state *cs)
{
  if (!cs) cs = (ctrl_state *)calloc(1, sizeof(ctrl_state));
  cs->amp = sp->amp_control;
  cs->speed = sp->speed_control;
  cs->expand = sp->expand_control;
  cs->revscl = sp->reverb_control_scale;
  cs->revlen = sp->reverb_control_length;
  cs->contrast = sp->contrast_control;
  cs->expand_on = sp->expand_control_p;
  cs->reverb_on = sp->reverb_control_p;
  cs->contrast_on = sp->contrast_control_p;
  cs->filter_on = sp->filter_control_p;
  cs->filter_order = sp->filter_control_order;
  if (sp->filter_control_envelope) 
    {
      if (cs->filter_env) cs->filter_env = free_env(cs->filter_env);
      cs->filter_env = copy_env(sp->filter_control_envelope);
    }
  if (sp->speed_control_direction == 1) 
    cs->reversed = false; 
  else cs->reversed = true;

  cs->contrast_amp = sp->contrast_control_amp;
  cs->expand_ramp = sp->expand_control_ramp;
  cs->expand_hop = sp->expand_control_hop;
  cs->expand_jitter = sp->expand_control_jitter;
  cs->expand_length = sp->expand_control_length;
  cs->reverb_feedback = sp->reverb_control_feedback;
  cs->reverb_decay = sp->reverb_control_decay;
  cs->reverb_lowpass = sp->reverb_control_lowpass;
  return(cs);
}


void save_controls(snd_info *sp) 
{
  sp->saved_controls = current_control_settings(sp, sp->saved_controls);
}


static ctrl_state *restore_control_settings(snd_info *sp, ctrl_state *cs)
{
  /* for use in controls->channel when the actual control panel is not in use */
  if (cs)
    {
      sp->amp_control = cs->amp;
      sp->speed_control = cs->speed;
      sp->expand_control = cs->expand;
      sp->reverb_control_scale = cs->revscl;
      sp->reverb_control_length = cs->revlen;
      sp->contrast_control = cs->contrast;
      sp->expand_control_p = cs->expand_on;
      sp->reverb_control_p = cs->reverb_on;
      sp->contrast_control_p = cs->contrast_on;
      sp->filter_control_p = cs->filter_on;
      sp->filter_control_order = cs->filter_order;
      if (cs->filter_env)
	{
	  sp->filter_control_envelope = free_env(sp->filter_control_envelope);
	  sp->filter_control_envelope = copy_env(cs->filter_env);
	}
      if (cs->reversed)
	sp->speed_control_direction = -1;
      else sp->speed_control_direction = 1;
      sp->contrast_control_amp = cs->contrast_amp;
      sp->expand_control_ramp = cs->expand_ramp;
      sp->expand_control_hop = cs->expand_hop;
      sp->expand_control_jitter = cs->expand_jitter;
      sp->expand_control_length = cs->expand_length;
      sp->reverb_control_feedback = cs->reverb_feedback;
      sp->reverb_control_decay = cs->reverb_decay;
      sp->reverb_control_lowpass = cs->reverb_lowpass;
    }
  return(cs);
}


void restore_controls(snd_info *sp) 
{
  ctrl_state *cs;
  char *tmpstr;
  cs = sp->saved_controls;
  if (!cs) 
    {
      sp->saved_controls = (ctrl_state *)calloc(1, sizeof(ctrl_state));
      cs = sp->saved_controls;
      cs->amp = DEFAULT_AMP_CONTROL;
      cs->speed = DEFAULT_SPEED_CONTROL;
      cs->reversed = false; /* (this is the button's view) */
      cs->expand = DEFAULT_EXPAND_CONTROL;
      cs->expand_on = DEFAULT_EXPAND_CONTROL_P;
      cs->revscl = DEFAULT_REVERB_CONTROL_SCALE;
      cs->revlen = DEFAULT_REVERB_CONTROL_LENGTH;
      cs->reverb_on = DEFAULT_REVERB_CONTROL_P;
      cs->contrast = DEFAULT_CONTRAST_CONTROL;
      cs->contrast_on = DEFAULT_CONTRAST_CONTROL_P;
      cs->filter_on = DEFAULT_FILTER_CONTROL_P;
      cs->filter_order = filter_control_order(ss);
      cs->filter_env = NULL;
    }
  toggle_expand_button(sp, cs->expand_on);
  toggle_contrast_button(sp, cs->contrast_on);
  toggle_reverb_button(sp, cs->reverb_on);
  toggle_filter_button(sp, cs->filter_on);
  toggle_direction_arrow(sp, cs->reversed);
  set_amp(sp, cs->amp);
  set_speed(sp, cs->speed);
  set_contrast(sp, cs->contrast);
  set_expand(sp, cs->expand);
  set_revscl(sp, cs->revscl);
  set_revlen(sp, cs->revlen);
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope); 
  if (cs->filter_env) 
    sp->filter_control_envelope = copy_env(cs->filter_env);
  else sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  set_filter_order(sp, cs->filter_order); /* causes redisplay */
  tmpstr = env_to_string(sp->filter_control_envelope);
  set_filter_text(sp, tmpstr);
  if (tmpstr) free(tmpstr);
}


void reset_controls(snd_info *sp) 
{
  char *tmpstr;
  toggle_expand_button(sp, DEFAULT_EXPAND_CONTROL_P);
  toggle_contrast_button(sp, DEFAULT_CONTRAST_CONTROL_P);
  toggle_reverb_button(sp, DEFAULT_REVERB_CONTROL_P);
  toggle_filter_button(sp, DEFAULT_FILTER_CONTROL_P);
  toggle_direction_arrow(sp, false);
  set_amp(sp, DEFAULT_AMP_CONTROL);
  set_speed(sp, DEFAULT_SPEED_CONTROL);
  set_contrast(sp, DEFAULT_CONTRAST_CONTROL);
  set_expand(sp, DEFAULT_EXPAND_CONTROL);
  set_revscl(sp, DEFAULT_REVERB_CONTROL_SCALE);
  set_revlen(sp, DEFAULT_REVERB_CONTROL_LENGTH);
  set_filter_order(sp, filter_control_order(ss));
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  tmpstr = env_to_string(sp->filter_control_envelope);
  set_filter_text(sp, tmpstr);
  display_filter_env(sp);
  if (tmpstr) free(tmpstr);

}


static void apply_unset_controls(snd_info *sp) 
{
  /* after apply_controls there's no need to clear everything! */
  toggle_expand_button(sp, DEFAULT_EXPAND_CONTROL_P);
  toggle_contrast_button(sp, DEFAULT_CONTRAST_CONTROL_P);
  toggle_reverb_button(sp, DEFAULT_REVERB_CONTROL_P);
  toggle_filter_button(sp, DEFAULT_FILTER_CONTROL_P);
  toggle_direction_arrow(sp, false);
  set_amp(sp, DEFAULT_AMP_CONTROL);
  set_speed(sp, DEFAULT_SPEED_CONTROL);
}


void set_show_controls(bool val)
{
  in_set_show_controls(ss, val);
#if (!USE_NO_GUI)
  if (in_show_controls(ss))
    show_all_controls();
  else hide_all_controls(); 
#endif
}



/* ---------------- control panel apply ---------------- */

void stop_applying(snd_info *sp)
{
  /* called if C-g during the apply process */
  sp->apply_ok = false;
}

typedef struct {
  int slice;
  snd_info *sp;
  mus_long_t i;
  int ofd;
  char *ofile;
  ctrl_state *cs;
  file_info *hdr;
  char *origin;
} apply_state;


static XEN after_apply_controls_hook;

static void *make_apply_state(snd_info *sp)
{
  /* set up initial state for apply_controls */
  apply_state *ap = NULL;
  ap = (apply_state *)calloc(1, sizeof(apply_state));
  ap->slice = 0;
  ap->hdr = NULL;
  ap->sp = sp;
  return((void *)ap);
}


static apply_state *free_apply_state(apply_state *ap)
{
  if (ap)
    {
      if (ap->ofile) {free(ap->ofile); ap->ofile = NULL;}
      if (ap->origin) {free(ap->origin); ap->origin = NULL;}
      ap->hdr = free_file_info(ap->hdr);
      free(ap);
    }
  return(NULL);
}


static mus_long_t apply_dur = 0, orig_dur, apply_beg = 0;

static bool apply_controls(apply_state *ap)
{
  snd_info *sp;
  chan_info *cp = NULL;
  sync_info *si;
  mus_float_t mult_dur;
  int i, curchan = 0, added_dur = 0;

  if (ap == NULL) return(false);
  sp = ap->sp;
  if ((!(sp->active)) || (sp->inuse != SOUND_NORMAL)) return(false);

  if (sp->filter_control_p) 
    added_dur = sp->filter_control_order;
  mult_dur = 1.0 / fabs(sp->speed_control);
  if (sp->expand_control_p) 
    mult_dur *= sp->expand_control;
  if (sp->reverb_control_p) 
    added_dur += (int)((SND_SRATE(sp) * sp->reverb_control_decay));

  if ((ss->apply_choice != APPLY_TO_SELECTION) &&
      (snd_feq(sp->speed_control, 1.0)) && 
      (apply_beg == 0) &&
      (sp->speed_control_direction == 1) &&
      (!(sp->filter_control_p)) && (!(sp->expand_control_p)) && (!(sp->reverb_control_p)) && (!(sp->contrast_control_p)))
    {
      int old_sync;
      bool need_scaling = false;
      mus_float_t *scalers = NULL;

      old_sync = sp->sync;
      /* get unused sync val */
      if (ss->apply_choice == APPLY_TO_SOUND)
	{
	  sp->sync = ss->sound_sync_max + 1;
	  ss->sound_sync_max++;
	}
      else sp->sync = 0;

      /* check for local amp_control vals */
      if (sp->selected_channel == NO_SELECTION) 
	cp = sp->chans[0];
      else cp = sp->chans[sp->selected_channel];
      si = sync_to_chan(cp);
      if (si == NULL)
	{
	  sp->sync = old_sync;
	  return(false);
	}

      scalers = (mus_float_t *)calloc(si->chans, sizeof(mus_float_t));
      for (i = 0; i < si->chans; i++)
	{
	  chan_info *ncp;
	  ncp = si->cps[i];
	  if (ncp->amp_control)
	    scalers[i] = ncp->amp_control[0];
	  else scalers[i] = sp->amp_control;
	  if (!(snd_feq(scalers[i], 1.0))) need_scaling = true; /* could possibly check all edit_ctrs, but this seems easier */
	}

      if (need_scaling)
	scale_by(cp, scalers, si->chans, false);
      else snd_warning_without_format("apply controls: no changes to apply!");

      sp->sync = old_sync;
      free(scalers);
      si = free_sync_info(si);
    }
  else
    {
      mus_long_t orig_apply_dur;
      io_error_t io_err = IO_NO_ERROR;

      orig_apply_dur = apply_dur;

      switch (ap->slice)
	{
	case 0:
	  /* apply_beg = 0; */
	  ap->ofile = NULL;
	  ap->ofile = snd_tempnam();
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
	      if (ap->hdr->chans <= 0) return(false);
	      if (apply_dur == 0)
		apply_dur = selection_len(); 
	      break;
	    }

	  if (ap->origin == NULL)
	    {
	      /* from apply-controls */
	      /* to reproduce this on a channel-independent basis, we need to use controls->channel
	       *   and conjure up a list of settings that match the current ones.
	       */
	      char *ampstr, *speedstr, *contraststr, *expandstr, *filterstr, *reverbstr;
	      if (sp->amp_control != DEFAULT_AMP_CONTROL)
		ampstr = mus_format("%.4f", 
				    sp->amp_control);
	      else ampstr = mus_strdup(PROC_FALSE);
	      if ((!(snd_feq(sp->speed_control, DEFAULT_SPEED_CONTROL))) || 
		  (sp->speed_control_direction == -1))
		speedstr = mus_format("%.4f", 
				      sp->speed_control * sp->speed_control_direction);
	      else speedstr = mus_strdup(PROC_FALSE);
	      if (sp->contrast_control_p)
		contraststr = mus_format(LIST_OPEN "%.4f" PROC_SEP "%.4f" LIST_CLOSE, 
					 sp->contrast_control, sp->contrast_control_amp);
	      else contraststr = mus_strdup(PROC_FALSE);
	      if (sp->expand_control_p)
		expandstr = mus_format(LIST_OPEN "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" LIST_CLOSE,
				       sp->expand_control, sp->expand_control_length, sp->expand_control_ramp, 
				       sp->expand_control_hop, sp->expand_control_jitter);
	      else expandstr = mus_strdup(PROC_FALSE);
	      if (sp->reverb_control_p)
		reverbstr = mus_format(LIST_OPEN "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" PROC_SEP "%.4f" LIST_CLOSE,
				       sp->reverb_control_scale, sp->reverb_control_length, sp->reverb_control_feedback, 
				       sp->reverb_control_lowpass, sp->reverb_control_decay);
	      else reverbstr = mus_strdup(PROC_FALSE);
	      if (sp->filter_control_p)
		{
		  char *envstr;
		  envstr = env_to_string(sp->filter_control_envelope);
		  filterstr = mus_format(LIST_OPEN "%d" PROC_SEP "%s" LIST_CLOSE, 
					 sp->filter_control_order, envstr);
		  free(envstr);
		}
	      else filterstr = mus_strdup(PROC_FALSE);
#if HAVE_FORTH
	      if (orig_apply_dur == 0)
	      ap->origin = mus_format(" '( %s %s %s %s %s %s ) %lld" PROC_SEP PROC_FALSE " %s", 
				      ampstr, speedstr, contraststr, expandstr, reverbstr, filterstr, 
				      apply_beg, S_controls_to_channel);
	      else ap->origin = mus_format(" '( %s %s %s %s %s %s ) %lld" PROC_SEP "%lld %s",
					   ampstr, speedstr, contraststr, expandstr, reverbstr, filterstr,
					   apply_beg, apply_dur, S_controls_to_channel);
#else
	      if (orig_apply_dur == 0)
	      ap->origin = mus_format("%s" PROC_OPEN LIST_OPEN "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" LIST_CLOSE PROC_SEP "%lld" PROC_SEP PROC_FALSE, 
				      TO_PROC_NAME(S_controls_to_channel),
				      ampstr, speedstr, contraststr, expandstr, reverbstr, filterstr, 
				      apply_beg);
	      else ap->origin = mus_format("%s" PROC_OPEN LIST_OPEN "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" PROC_SEP "%s" LIST_CLOSE PROC_SEP "%lld" PROC_SEP "%lld",
					   TO_PROC_NAME(S_controls_to_channel),
					   ampstr, speedstr, contraststr, expandstr, reverbstr, filterstr,
					   apply_beg, apply_dur);
#endif
	      free(ampstr);
	      free(speedstr);
	      free(contraststr);
	      free(expandstr);
	      free(reverbstr);
	      free(filterstr);
	    }

	  orig_dur = apply_dur;
	  apply_dur = (mus_long_t)(mult_dur * (apply_dur + added_dur));
	  ap->ofd = open_temp_file(ap->ofile, ap->hdr->chans, ap->hdr, &io_err);

	  if (ap->ofd == -1)
	    {
	      snd_error("%s apply temp file %s: %s\n", 
			(io_err != IO_NO_ERROR) ? io_error_name(io_err) : "can't open",
			ap->ofile, 
			snd_open_strerror());
	      sp->applying = false;
	      ap = free_apply_state(ap);
	      return(false);
	    }

	  sp->apply_ok = true;
	  initialize_apply(sp, ap->hdr->chans, apply_beg, orig_dur + added_dur); /* dur here is input dur */
	  ap->i = 0;
	  ap->slice++;
	  return(true);
	  break;
	  
	case 1:
	  if (!(sp->apply_ok))
	    ap->slice++;
	  else
	    {
	      int len;
	      len = run_apply(ap->ofd); /* returns frames written (an int) */
	      if (len <= 0)
		{
		  ap->slice++;
		  return(true);
		}
	      ap->i += len;
	      if (ap->i >= apply_dur) ap->slice++;
	      check_for_event();
	      /* if C-G, stop_applying called which cancels and backs out */
	      if ((ss->stopped_explicitly) || (!(sp->active)))
		ap->slice++;
	    }
	  return(true);
	  break;
	  
	case 2:
	  finalize_apply(sp);
	  close_temp_file(ap->ofile,
			  ap->ofd,
			  ap->hdr->type,
			  apply_dur * (ap->hdr->chans) * mus_bytes_per_sample((ap->hdr)->format));
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
			  if (file_change_samples(apply_beg, apply_dur, ap->ofile, sp->chans[i], i,
						  (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						  ap->origin, sp->chans[i]->edit_ctr))
			    update_graph(sp->chans[i]);
			}
		    }
		  else
		    {
		      for (i = 0; i < sp->nchans; i++)
			{
			  if (file_override_samples(apply_dur, ap->ofile, sp->chans[i], i,
						    (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						    ap->origin))
			    update_graph(sp->chans[i]);
			}
		    }
		  break;

		case APPLY_TO_CHANNEL: 
		  if (sp->selected_channel != NO_SELECTION) 
		    curchan = sp->selected_channel;
		  if (apply_beg > 0)
		    file_change_samples(apply_beg, apply_dur, ap->ofile, sp->chans[curchan], 0, 
					DELETE_ME, ap->origin, sp->chans[curchan]->edit_ctr);
		  else file_override_samples(apply_dur, ap->ofile, sp->chans[curchan], 0, 
					     DELETE_ME, ap->origin);
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
			  if (file_change_samples(si->begs[i], apply_dur, ap->ofile, si->cps[i], i,
						  (si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						  ap->origin, si->cps[i]->edit_ctr))
			    update_graph(si->cps[i]);
			}
		    }
		  else
		    {
		      bool ok;
		      ok = delete_selection(DONT_UPDATE_DISPLAY);
		      if (apply_dur > 0)
			{
			  for (i = 0; i < si->chans; i++)
			    {
			      file_insert_samples(si->begs[i], apply_dur, ap->ofile, si->cps[i], 0, 
						  (si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
						  ap->origin, si->cps[i]->edit_ctr);
			      reactivate_selection(si->cps[i], si->begs[i], si->begs[i] + apply_dur);
			      if (ok) backup_edit_list(si->cps[i]);
			    }
			}
		    }
		  si = free_sync_info(si); 
		  break;
		}
	      clear_status_area(sp);
	      sp->apply_ok = false;
	      
	      if ((sp->expand_control_p) || 
		  (sp->speed_control_direction != 1) || (!(snd_feq(sp->speed_control, 1.0))))
		{
		  for (i = 0; i < sp->nchans; i++)
		    {
		      cp = sp->chans[i];
		      if (cp->edits[cp->edit_ctr]->marks)
			{
			  mus_float_t ratio;
			  if (!(sp->expand_control_p))
			    ratio = sp->speed_control;
			  else ratio = sp->speed_control / sp->expand_control;
			  if (ratio != 1.0)
			    {
			      bool over_selection;
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
	  break;
	}
    }

  apply_unset_controls(sp);

  if (XEN_HOOKED(after_apply_controls_hook))
    run_hook(after_apply_controls_hook, 
	     XEN_LIST_1(C_INT_TO_XEN_SOUND(sp->index)),
	     S_after_apply_controls_hook);

  sp->applying = false;
  ap = free_apply_state(ap);
  ss->stopped_explicitly = false;
  return(false);
}


void expand_control_set_hop(mus_float_t hop)
{
  int i;
  in_set_expand_control_hop(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->expand_control_hop = expand_control_hop(ss);
	  if (sp->playing) dac_set_expand_hop(sp, expand_control_hop(ss));
	}
    }
}

void expand_control_set_length(mus_float_t hop)
{
  int i;
  in_set_expand_control_length(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->expand_control_length = expand_control_length(ss);
	  if (sp->playing) dac_set_expand_length(sp, expand_control_length(ss));
	}
    }
}

void expand_control_set_ramp(mus_float_t hop)
{
  int i;
  in_set_expand_control_ramp(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->expand_control_ramp = expand_control_ramp(ss);
	  if (sp->playing) dac_set_expand_ramp(sp, expand_control_ramp(ss));
	}
    }
}

void expand_control_set_jitter(mus_float_t hop)
{
  int i;
  in_set_expand_control_jitter(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->expand_control_jitter = expand_control_jitter(ss);
	}
    }
}

void contrast_control_set_amp(mus_float_t hop)
{
  int i;
  in_set_contrast_control_amp(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->contrast_control_amp = contrast_control_amp(ss);
	  if (sp->playing) dac_set_contrast_amp(sp, contrast_control_amp(ss));
	}
    }
}

void reverb_control_set_lowpass(mus_float_t hop)
{
  int i;
  in_set_reverb_control_lowpass(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->reverb_control_lowpass = reverb_control_lowpass(ss);
	  if (sp->playing) dac_set_reverb_lowpass(sp, reverb_control_lowpass(ss));
	}
    }
}

void reverb_control_set_feedback(mus_float_t hop)
{
  int i;
  in_set_reverb_control_feedback(ss, hop);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL))
	{
	  sp->reverb_control_feedback = reverb_control_feedback(ss);
	  if (sp->playing) dac_set_reverb_feedback(sp, reverb_control_feedback(ss));
	}
    }
}



/* ---------------------------------------- sound objects ---------------------------------------- */

/* this is my long term plan right now... */

/* generics (besides length, srate, channels, frames, file-name, sync, maxamp, play, copy, fill!, [apply], [set!], [for-each], [map]):
 *
 *             source:         procedure-source[s7_procedure_source] mix-home mark-home region-home player-home sampler-home
 *                               mus cases: readin=file+chan? etc, port -> filename?, sound->filename?
 *               seems redundant -- why not use file-name?
 *
 *             position:       mark-sample mix-position region-position sampler-position
 *               [location?]     port->line number?, mus cases = mus_location?, player? widget? cursor if sound?
 *               (CL has position)
 *
 *             properties:     edit|mark|mix|sound|channel-properties procedure-property?  [also property as accessor]
 *               but then typos lead to seriously confusing behavior -- I think I'll leave out properties for now.
 *
 *             name:           mark|mix-name file-name (widget name via XtName) mus-name colormap
 *                               __func__? port-filename sampler-filename
 *
 *             reverse is a special case -- it really only makes sense with lists where reversal happens all the time
 *               and for all other types we'd want reverse! (in-place change), but extremely rarely.
 *
 *     which leaves:
 *             scale(-to|by) convolve fft env filter insert mix save smooth src map|scan pan-mix sampler?
 *
 * save with all the optkey args for all types (treat list/vector etc as sound data)
 * check map/for-each cases -- sound selection etc (and add to extsnd) and set/ref
 *   for-each depends on ref, map could depend on copy/set/ref
 *
 *    objects are generator(clm2xen), player(snd-dac), sampler(snd-edits), sound-data(sndlib2xen), transform (snd-fft),
 *               mark(snd-marks), mix(snd-mix), selection(snd-select), region(snd-region), colormap(snd-gxcolormap),
 *               vct(vct), hook(xen), XmObj(xm, xg), plus the base types(s7): string, hash-table, vector, pair, object
 *
 *    also needed are further cases of ref/set
 *    ref needed: mix? region selection sound
 *    set needed: selection? sound 
 *
 * possible new objects: menu-item, file-filter|sorter, fft-window?, variable-graph?
 *
 * (scan-channel -> channel-for-each)
 *   and channel-map rather than map-channel
 *   channel-for-each in the multiarg case would make it easier to compare chans 
 *   (find-if func snd...)
 * (eval-over-selection -> selection-for-each) *-for-each
 *   then selection-map would replace the current selected samples
 *
 * do everything through generic sampler (vector etc)
 *   in/out/readin generic as well, so we can use instruments for edits and vice-versa without change
 *   selection sampler [no need for snd-sampler (snd-edits) I think]
 *
 *  the goal being that all clm and snd structures are compatible, all operations interchangeable
 *    so (violin 0 1 440 .1) can appear sensibly in any context
 *
 * then deprecate all the special case stuff, add generic versions of *.scm examples
 * sound-file properties?  a data base saving all such info (rather than kludges involving headers)
 *   (hash-table makes more sense as a property list than a list in large cases) -- currently it's actually a vector
 *   or a global data base for all properties? -- easy to search or display in various ways
 */


typedef struct {
  int n;
} xen_sound;


#define XEN_TO_XEN_SOUND(arg) ((xen_sound *)XEN_OBJECT_REF(arg))

int xen_sound_to_int(XEN n)
{
  xen_sound *mx;
  mx = XEN_TO_XEN_SOUND(n);
  return(mx->n);
}


static XEN_OBJECT_TYPE xen_sound_tag;

bool xen_sound_p(XEN obj) 
{
  return(XEN_OBJECT_TYPE_P(obj, xen_sound_tag));
}


static void xen_sound_free(xen_sound *v) {if (v) free(v);}

XEN_MAKE_OBJECT_FREE_PROCEDURE(xen_sound, free_xen_sound, xen_sound_free)


static char *xen_sound_to_string(xen_sound *v)
{
  #define XEN_SOUND_PRINT_BUFFER_SIZE 64
  char *buf;
  if (v == NULL) return(NULL);
  buf = (char *)calloc(XEN_SOUND_PRINT_BUFFER_SIZE, sizeof(char));
  snprintf(buf, XEN_SOUND_PRINT_BUFFER_SIZE, "#<sound %d>", v->n);
  return(buf);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(xen_sound, print_xen_sound, xen_sound_to_string)


#if HAVE_FORTH || HAVE_RUBY
static XEN g_xen_sound_to_string(XEN obj)
{
  char *vstr;
  XEN result;
  #define S_xen_sound_to_string "sound->string"

  XEN_ASSERT_TYPE(XEN_SOUND_P(obj), obj, XEN_ONLY_ARG, S_xen_sound_to_string, "a sound");

  vstr = xen_sound_to_string(XEN_TO_XEN_SOUND(obj));
  result = C_TO_XEN_STRING(vstr);
  free(vstr);
  return(result);
}
#endif


#if (!HAVE_SCHEME)
static bool xen_sound_equalp(xen_sound *v1, xen_sound *v2) 
{
  return((v1 == v2) ||
	 (v1->n == v2->n));
}

static XEN equalp_xen_sound(XEN obj1, XEN obj2)
{
  if ((!(XEN_SOUND_P(obj1))) || (!(XEN_SOUND_P(obj2)))) return(XEN_FALSE);
  return(C_TO_XEN_BOOLEAN(xen_sound_equalp(XEN_TO_XEN_SOUND(obj1), XEN_TO_XEN_SOUND(obj2))));
}
#endif


static xen_sound *xen_sound_make(int n)
{
  xen_sound *new_v;
  new_v = (xen_sound *)malloc(sizeof(xen_sound));
  new_v->n = n;
  return(new_v);
}


XEN new_xen_sound(int n)
{
  xen_sound *mx;
  if (n < 0)
    return(XEN_FALSE);

  mx = xen_sound_make(n);
  XEN_MAKE_AND_RETURN_OBJECT(xen_sound_tag, mx, 0, free_xen_sound);
}


#if HAVE_SCHEME
static bool s7_xen_sound_equalp(void *obj1, void *obj2)
{
  return((obj1 == obj2) ||
	 (((xen_sound *)obj1)->n == ((xen_sound *)obj2)->n));
}


static XEN s7_xen_sound_length(s7_scheme *sc, XEN obj)
{
  return(g_frames(obj, XEN_ZERO, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION)));
}


static XEN s7_xen_sound_copy(s7_scheme *sc, XEN obj)
{
  snd_info *sp;
  sp = get_sp(obj);
  if (sp)
    {
      io_error_t err;
      char *name;
      name = snd_tempnam();
      if (mus_header_writable(sp->hdr->type, sp->hdr->format))
	err = save_edits_without_display(sp, name, sp->hdr->type, sp->hdr->format, sp->hdr->srate, NULL, AT_CURRENT_EDIT_POSITION);
      else err = save_edits_without_display(sp, name, MUS_NEXT, MUS_OUT_FORMAT, sp->hdr->srate, NULL, AT_CURRENT_EDIT_POSITION);
      sp = snd_open_file(name, FILE_READ_WRITE);
      free(name);
      if (sp)
	return(new_xen_sound(sp->index));
      if (SERIOUS_IO_ERROR(err))
	XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		  XEN_LIST_2(C_TO_XEN_STRING("copy sound: can't save edits, ~A"),
			     C_TO_XEN_STRING(io_error_name(err))));
    }
  return(XEN_FALSE);
}


static XEN s7_xen_sound_fill(s7_scheme *sc, XEN obj, XEN val)
{
  snd_info *sp;
  sp = get_sp(obj);
  if (sp)
    {
      mus_float_t valf;
      chan_info *cp;
      int i;

      valf = XEN_TO_C_DOUBLE(val);
      if (valf == 0.0)
	{
	  for (i = 0; i < sp->nchans; i++)
	    {
	      cp = sp->chans[i];
	      scale_channel(cp, 0.0, 0, CURRENT_SAMPLES(cp), cp->edit_ctr, false);
	      update_graph(cp);
	    }
	}
      else
	{
	  /* this was #if (!HAVE_SCHEME) which makes no sense -- I think it meant (!HAVE_RUN)
	   *   but that means (fill! <sound>) fails if optimization is off.
	   */
	  mus_long_t len = -1, j;
	  mus_float_t *data = NULL;
	  
	  for (i = 0; i < sp->nchans; i++)
	    {
	      cp = sp->chans[i];
	      if ((!data) || (CURRENT_SAMPLES(cp) != len))
		{
		  len = CURRENT_SAMPLES(cp);
		  if (data) free(data);
		  data = (mus_float_t *)malloc(len * sizeof(mus_float_t));
		  for (j = 0; j < len; j++)
		    data[j] = valf;
		}
	      if (change_samples(0, len, data, cp, "fill! sound", cp->edit_ctr, fabs(valf)))
		update_graph(cp);
	    }
	  free(data);
	}
    }
  return(XEN_FALSE);
}
#endif


static void init_xen_sound(void)
{
#if HAVE_SCHEME
  xen_sound_tag = XEN_MAKE_OBJECT_TYPE("<sound>", print_xen_sound, free_xen_sound, s7_xen_sound_equalp, 
				       NULL, NULL, NULL, s7_xen_sound_length, s7_xen_sound_copy, NULL, s7_xen_sound_fill);
#else
#if HAVE_RUBY
  xen_sound_tag = XEN_MAKE_OBJECT_TYPE("XenSound", sizeof(xen_sound));
#else
  xen_sound_tag = XEN_MAKE_OBJECT_TYPE("Sound", sizeof(xen_sound));
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(xen_sound_tag,   print_xen_sound);
  fth_set_object_dump(xen_sound_tag,      g_xen_sound_to_string);
  fth_set_object_equal(xen_sound_tag,     equalp_xen_sound);
  fth_set_object_free(xen_sound_tag,      free_xen_sound);
#endif

#if HAVE_RUBY
  rb_define_method(xen_sound_tag, "to_s",     XEN_PROCEDURE_CAST print_xen_sound, 0);
  rb_define_method(xen_sound_tag, "eql?",     XEN_PROCEDURE_CAST equalp_xen_sound, 1);
  rb_define_method(xen_sound_tag, "==",       XEN_PROCEDURE_CAST equalp_xen_sound, 1);
  rb_define_method(xen_sound_tag, "to_str",   XEN_PROCEDURE_CAST g_xen_sound_to_string, 0);
#endif
}

/* -------------------------------------------------------------------------------- */

static XEN g_integer_to_sound(XEN n)
{
  #define H_integer_to_sound "(" S_integer_to_sound " n) returns a sound object corresponding to the given integer"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ONLY_ARG, S_integer_to_sound, "an integer");
  return(new_xen_sound(XEN_TO_C_INT(n)));
}


static XEN g_sound_to_integer(XEN n)
{
  #define H_sound_to_integer "(" S_sound_to_integer " id) returns the integer corresponding to the given sound"
  XEN_ASSERT_TYPE(XEN_SOUND_P(n), n, XEN_ONLY_ARG, S_sound_to_integer, "a sound");
  return(C_TO_XEN_INT(xen_sound_to_int(n)));
}


XEN snd_no_such_sound_error(const char *caller, XEN n)
{
  XEN_ERROR(XEN_ERROR_TYPE("no-such-sound"),
	    XEN_LIST_3(C_TO_XEN_STRING("~A: no such sound: ~A"),
		       C_TO_XEN_STRING(caller),
		       n));
  return(XEN_FALSE);
}


static XEN g_sound_p(XEN snd)
{
  #define H_sound_p "(" S_sound_p " snd): " PROC_TRUE " if 'snd' (a sound object or an integer) is an active (accessible) sound"

  if (XEN_INTEGER_P(snd) || XEN_SOUND_P(snd))
    {
      snd_info *sp;
      sp = get_sp(snd);
      return(C_TO_XEN_BOOLEAN((sp) && 
			      (snd_ok(sp)) &&
			      (sp->inuse == SOUND_NORMAL)));
    }
  return(XEN_FALSE);
}


#if HAVE_SCHEME
bool r_sound_p(int i);
bool r_sound_p(int i)
{
  return((i < ss->max_sounds) && (snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL));
}
#endif


static XEN g_select_sound(XEN snd)
{
  #define H_select_sound "(" S_select_sound " snd): make sound 'snd' (a sound object or an index) the default sound for \
any editing operations."
  snd_info *sp;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd) || XEN_SOUND_P(snd), snd, XEN_ONLY_ARG, S_select_sound, "a sound object or index");

  sp = get_sp(snd);
  if (sp)
    {
      select_channel(sp, 0);
      return(snd);
    }

  return(snd_no_such_sound_error(S_select_sound, snd));
}


static XEN g_select_channel(XEN chn_n)
{
  #define H_select_channel "(" S_select_channel " :optional (chn 0)): make channel 'chn' of the currently selected sound the default \
channel for editing."
  snd_info *sp;
  int chan;

  ASSERT_SOUND(S_select_channel, chn_n, 1);
  chan = XEN_TO_C_INT_OR_ELSE(chn_n, 0);

  sp = any_selected_sound();
  if ((sp) && 
      (chan >= 0) &&
      (chan < sp->nchans)) 
    {
      select_channel(sp, chan);
      return(chn_n);
    }

  return(snd_no_such_channel_error(S_select_channel, C_TO_XEN_STRING(S_selected_sound), chn_n));
}


static XEN g_find_sound(XEN filename, XEN which)
{
  #define H_find_sound "(" S_find_sound " name :optional (nth 0)): return the sound associated with file 'name'. \
If more than one such sound exists, 'nth' chooses which one to return."
  snd_info *sp;

  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_find_sound, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(which), which, XEN_ARG_2, S_find_sound, "an integer");

  sp = find_sound(XEN_TO_C_STRING(filename), XEN_TO_C_INT_OR_ELSE(which, 0));
  if (sp) return(C_INT_TO_XEN_SOUND(sp->index));

  return(XEN_FALSE);
}


typedef enum {SP_SYNC, SP_READ_ONLY, SP_NCHANS, SP_CONTRASTING, SP_EXPANDING, SP_REVERBING, SP_FILTERING, SP_FILTER_ORDER,
	      SP_SRATE, SP_DATA_FORMAT, SP_DATA_LOCATION, SP_HEADER_TYPE, SP_SAVE_CONTROLS, SP_RESTORE_CONTROLS, SP_SELECTED_CHANNEL,
	      SP_COMMENT, SP_FILE_NAME, SP_SHORT_FILE_NAME, SP_CLOSE, SP_UPDATE, SP_SHOW_CONTROLS,
	      SP_FILTER_DBING, SP_SPEED_TONES, SP_SPEED_STYLE, SP_RESET_CONTROLS,
	      SP_AMP, SP_CONTRAST, SP_CONTRAST_AMP, SP_EXPAND, SP_EXPAND_LENGTH, SP_EXPAND_RAMP, SP_EXPAND_HOP,
	      SP_SPEED, SP_REVERB_LENGTH, SP_REVERB_FEEDBACK, SP_REVERB_SCALE, SP_REVERB_LOW_PASS,
	      SP_REVERB_DECAY, SP_PROPERTIES, SP_FILTER_COEFFS, SP_DATA_SIZE, SP_FILTER_HZING, SP_EXPAND_JITTER,
	      SP_CONTRAST_BOUNDS, SP_AMP_BOUNDS, SP_SPEED_BOUNDS, SP_EXPAND_BOUNDS, SP_REVERB_LENGTH_BOUNDS, SP_REVERB_SCALE_BOUNDS,
	      SP_FILTER_ENVELOPE
} sp_field_t;


static XEN sound_get(XEN snd, sp_field_t fld, const char *caller)
{
  snd_info *sp;
  int i;
  XEN res = XEN_EMPTY_LIST;

  if (XEN_TRUE_P(snd))
    {
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL))
	    res = XEN_CONS(sound_get(C_TO_XEN_INT(i), fld, caller), res);
	}
      return(res);
    }

  if (XEN_PLAYER_P(snd))
    {
      sp = get_player_sound(snd);
      if (!sp)
	return(no_such_player_error(caller, snd));
    }
  else
    {
      ASSERT_SOUND(caller, snd, 1);
      sp = get_sp(snd);
      if (!sp)
	return(snd_no_such_sound_error(caller, snd));
    }
  if ((sp == NULL) || 
      (sp->inuse == SOUND_WRAPPER))
    return(snd_no_such_sound_error(caller, snd));

  switch (fld)
    {
    case SP_SYNC:                return(C_TO_XEN_INT(sp->sync));                                                    break;
    case SP_READ_ONLY:           return(C_TO_XEN_BOOLEAN(sp->user_read_only == FILE_READ_ONLY));                    break;
    case SP_NCHANS:              return(C_TO_XEN_INT(sp->nchans));                                                  break;
    case SP_EXPANDING:           return(C_TO_XEN_BOOLEAN(sp->expand_control_p));                                    break;
    case SP_CONTRASTING:         return(C_TO_XEN_BOOLEAN(sp->contrast_control_p));                                  break;
    case SP_REVERBING:           return(C_TO_XEN_BOOLEAN(sp->reverb_control_p));                                    break;
    case SP_FILTERING:           return(C_TO_XEN_BOOLEAN(sp->filter_control_p));                                    break;
    case SP_FILTER_DBING:        return(C_TO_XEN_BOOLEAN(sp->filter_control_in_dB));                                break;
    case SP_FILTER_HZING:        return(C_TO_XEN_BOOLEAN(sp->filter_control_in_hz));                                break;
    case SP_FILTER_ORDER:        return(C_TO_XEN_INT(sp->filter_control_order));                                    break;
    case SP_SRATE:               return(C_TO_XEN_INT(sp->hdr->srate));                                              break;
    case SP_DATA_FORMAT:         return(C_TO_XEN_INT(sp->hdr->format));                                             break;
    case SP_HEADER_TYPE:         return(C_TO_XEN_INT(sp->hdr->type));                                               break;
    case SP_DATA_LOCATION:       return(C_TO_XEN_LONG_LONG(sp->hdr->data_location));                                  break;
    case SP_DATA_SIZE:           return(C_TO_XEN_LONG_LONG(mus_samples_to_bytes(sp->hdr->format, sp->hdr->samples))); break;
    case SP_SAVE_CONTROLS:       if (HAS_WIDGETS(sp)) save_controls(sp);                                            break;
    case SP_RESTORE_CONTROLS:    if (HAS_WIDGETS(sp)) restore_controls(sp);                                         break;
    case SP_RESET_CONTROLS:      if (HAS_WIDGETS(sp)) reset_controls(sp);                                           break;
    case SP_FILE_NAME:           return(C_TO_XEN_STRING(sp->filename));                                             break;
    case SP_SHORT_FILE_NAME:     return(C_TO_XEN_STRING(sp->short_filename));                                       break;
    case SP_CLOSE:               if (!(IS_PLAYER_SOUND(sp))) snd_close_file(sp);                                    break;
    case SP_SHOW_CONTROLS:       if (HAS_WIDGETS(sp)) return(C_TO_XEN_BOOLEAN(showing_controls(sp)));               break;
    case SP_SPEED_TONES:         return(C_TO_XEN_INT(sp->speed_control_tones));                                     break;
    case SP_SPEED_STYLE:         return(C_TO_XEN_INT((int)(sp->speed_control_style)));                              break;
    case SP_COMMENT:             return(C_TO_XEN_STRING(sp->hdr->comment));                                         break;
    case SP_AMP:                 return(C_TO_XEN_DOUBLE(sp->amp_control));                                          break;
    case SP_CONTRAST:            return(C_TO_XEN_DOUBLE(sp->contrast_control));                                     break;
    case SP_CONTRAST_AMP:        return(C_TO_XEN_DOUBLE(sp->contrast_control_amp));                                 break;
    case SP_EXPAND:              return(C_TO_XEN_DOUBLE(sp->expand_control));                                       break;
    case SP_EXPAND_LENGTH:       return(C_TO_XEN_DOUBLE(sp->expand_control_length));                                break;
    case SP_EXPAND_RAMP:         return(C_TO_XEN_DOUBLE(sp->expand_control_ramp));                                  break;
    case SP_EXPAND_HOP:          return(C_TO_XEN_DOUBLE(sp->expand_control_hop));                                   break;
    case SP_EXPAND_JITTER:       return(C_TO_XEN_DOUBLE(sp->expand_control_jitter));                                break;
    case SP_REVERB_LENGTH:       return(C_TO_XEN_DOUBLE(sp->reverb_control_length));                                break;
    case SP_REVERB_FEEDBACK:     return(C_TO_XEN_DOUBLE(sp->reverb_control_feedback));                              break;
    case SP_REVERB_SCALE:        return(C_TO_XEN_DOUBLE(sp->reverb_control_scale));                                 break;
    case SP_REVERB_LOW_PASS:     return(C_TO_XEN_DOUBLE(sp->reverb_control_lowpass));                               break;
    case SP_REVERB_DECAY:        return(C_TO_XEN_DOUBLE(sp->reverb_control_decay));                                 break;

    case SP_AMP_BOUNDS:          
      return(XEN_LIST_2(C_TO_XEN_DOUBLE(sp->amp_control_min), C_TO_XEN_DOUBLE(sp->amp_control_max))); 
      break;

    case SP_CONTRAST_BOUNDS:     
      return(XEN_LIST_2(C_TO_XEN_DOUBLE(sp->contrast_control_min), C_TO_XEN_DOUBLE(sp->contrast_control_max))); 
      break;

    case SP_EXPAND_BOUNDS:       
      return(XEN_LIST_2(C_TO_XEN_DOUBLE(sp->expand_control_min), C_TO_XEN_DOUBLE(sp->expand_control_max))); 
      break;

    case SP_SPEED_BOUNDS:        
      return(XEN_LIST_2(C_TO_XEN_DOUBLE(sp->speed_control_min), C_TO_XEN_DOUBLE(sp->speed_control_max)));
      break;

    case SP_REVERB_LENGTH_BOUNDS: 
      return(XEN_LIST_2(C_TO_XEN_DOUBLE(sp->reverb_control_length_min), C_TO_XEN_DOUBLE(sp->reverb_control_length_max))); 
      break;

    case SP_REVERB_SCALE_BOUNDS: 
      return(XEN_LIST_2(C_TO_XEN_DOUBLE(sp->reverb_control_scale_min), C_TO_XEN_DOUBLE(sp->reverb_control_scale_max))); 
      break;

    case SP_SELECTED_CHANNEL:    
      if (sp->selected_channel != NO_SELECTION) 
	return(C_TO_XEN_INT(sp->selected_channel));
      return(XEN_FALSE); 
      break;

    case SP_UPDATE:              
      if (!(IS_PLAYER_SOUND(sp)))
	{
	  mus_sound_forget(sp->filename); /* old record must be out-of-date, so flush it (write date can be troublesome) */
	  sp = snd_update_within_xen(sp, caller); 
	  if (sp) 
	    return(C_INT_TO_XEN_SOUND(sp->index));
	} 
      break;

    case SP_PROPERTIES:
      if (!(IS_PLAYER_SOUND(sp)))
	{
	  if (!(XEN_VECTOR_P(sp->properties)))
	    {
	      sp->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
	      sp->properties_loc = snd_protect(sp->properties);
	    }
	  return(XEN_VECTOR_REF(sp->properties, 0));
	}
      break;

    case SP_SPEED:
#if XEN_HAVE_RATIOS
      if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
	{
	  if (sp->speed_control_direction == -1)
	    return(XEN_MAKE_RATIO(C_TO_XEN_INT(-sp->speed_control_numerator), C_TO_XEN_INT(sp->speed_control_denominator)));
	  else return(XEN_MAKE_RATIO(C_TO_XEN_INT(sp->speed_control_numerator), C_TO_XEN_INT(sp->speed_control_denominator)));
	}
#endif
      if (sp->speed_control_direction == -1) 
	return(C_TO_XEN_DOUBLE((-(sp->speed_control)))); 
      else return(C_TO_XEN_DOUBLE(sp->speed_control)); 
      break;

    case SP_FILTER_COEFFS: 
      if (sp->filter_control_envelope)
	{
	  int len;
	  mus_float_t *coeffs, *data;
	  len = sp->filter_control_order;
	  coeffs = (mus_float_t *)calloc(len, len * sizeof(mus_float_t));
	  data = sample_linear_env(sp->filter_control_envelope, len);
	  mus_make_fir_coeffs(len, data, coeffs);
	  free(data);
	  return(xen_make_vct(len, coeffs));
	}
      break;

    case SP_FILTER_ENVELOPE:
      if (sp->filter_control_envelope)
	return(env_to_xen(sp->filter_control_envelope));
      break;
    }
  return(XEN_FALSE);
}


static XEN sound_get_global(XEN snd, sp_field_t fld, const char *caller)
{
  if (XEN_NOT_BOUND_P(snd))
    switch (fld)
      {
      case SP_FILTER_DBING:         return(C_TO_XEN_BOOLEAN(filter_control_in_dB(ss)));    break;
      case SP_FILTER_HZING:         return(C_TO_XEN_BOOLEAN(filter_control_in_hz(ss)));    break;
      case SP_FILTER_ORDER:         return(C_TO_XEN_INT(filter_control_order(ss)));        break;
      case SP_SHOW_CONTROLS:        return(C_TO_XEN_BOOLEAN(in_show_controls(ss)));        break;
      case SP_SPEED_TONES:          return(C_TO_XEN_INT(speed_control_tones(ss)));         break;
      case SP_SPEED_STYLE:          return(C_TO_XEN_INT((int)(speed_control_style(ss))));  break;
      case SP_CONTRAST_AMP:         return(C_TO_XEN_DOUBLE(contrast_control_amp(ss)));     break;
      case SP_EXPAND_LENGTH:        return(C_TO_XEN_DOUBLE(expand_control_length(ss)));    break;
      case SP_EXPAND_RAMP:          return(C_TO_XEN_DOUBLE(expand_control_ramp(ss)));      break;
      case SP_EXPAND_HOP:           return(C_TO_XEN_DOUBLE(expand_control_hop(ss)));       break;
      case SP_EXPAND_JITTER:        return(C_TO_XEN_DOUBLE(expand_control_jitter(ss)));    break;
      case SP_REVERB_FEEDBACK:      return(C_TO_XEN_DOUBLE(reverb_control_feedback(ss)));  break;
      case SP_REVERB_LOW_PASS:      return(C_TO_XEN_DOUBLE(reverb_control_lowpass(ss)));   break;
      case SP_REVERB_DECAY:         return(C_TO_XEN_DOUBLE(reverb_control_decay(ss)));     break;

      case SP_AMP_BOUNDS:           
	return(XEN_LIST_2(C_TO_XEN_DOUBLE(amp_control_min(ss)), C_TO_XEN_DOUBLE(amp_control_max(ss)))); 
	break;

      case SP_CONTRAST_BOUNDS:     
	return(XEN_LIST_2(C_TO_XEN_DOUBLE(contrast_control_min(ss)), C_TO_XEN_DOUBLE(contrast_control_max(ss))));
	break;

      case SP_EXPAND_BOUNDS:        
	return(XEN_LIST_2(C_TO_XEN_DOUBLE(expand_control_min(ss)), C_TO_XEN_DOUBLE(expand_control_max(ss)))); 
	break;

      case SP_SPEED_BOUNDS:         
	return(XEN_LIST_2(C_TO_XEN_DOUBLE(speed_control_min(ss)), C_TO_XEN_DOUBLE(speed_control_max(ss)))); 
	break;

      case SP_REVERB_LENGTH_BOUNDS: 
	return(XEN_LIST_2(C_TO_XEN_DOUBLE(reverb_control_length_min(ss)), C_TO_XEN_DOUBLE(reverb_control_length_max(ss)))); 
	break;

      case SP_REVERB_SCALE_BOUNDS:  
	return(XEN_LIST_2(C_TO_XEN_DOUBLE(reverb_control_scale_min(ss)), C_TO_XEN_DOUBLE(reverb_control_scale_max(ss)))); 
	break;

      default: 
	break;
      }
  return(sound_get(snd, fld, caller));
}


static XEN sound_set(XEN snd, XEN val, sp_field_t fld, const char *caller)
{
  snd_info *sp;
  int i, ival;
  mus_float_t fval;

  if (XEN_TRUE_P(snd))
    {
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && 
	      (sp->inuse == SOUND_NORMAL))
	    sound_set(C_TO_XEN_INT(i), val, fld, caller);
	}
      return(val);
    }

  if (XEN_PLAYER_P(snd))
    {
      sp = get_player_sound(snd);
      if (!sp)
	return(no_such_player_error(caller, snd));
    }
  else
    {
      ASSERT_SOUND(caller, snd, 1);
      sp = get_sp(snd);
      if (!sp)
	return(snd_no_such_sound_error(caller, snd));
    }
  if ((sp == NULL) || 
      (sp->inuse == SOUND_WRAPPER))
    return(snd_no_such_sound_error(caller, snd));

  switch (fld)
    {
    case SP_SYNC:  
      if (XEN_INTEGER_P(val))
	syncb(sp, XEN_TO_C_INT(val));
      else syncb(sp, (int)XEN_TO_C_BOOLEAN(val));
      break;

    case SP_READ_ONLY:
      if (HAS_WIDGETS(sp))
	{
	  sp->user_read_only = (XEN_TO_C_BOOLEAN(val) ? FILE_READ_ONLY : FILE_READ_WRITE);
	  if ((sp->user_read_only == FILE_READ_ONLY) || 
	      (sp->file_read_only == FILE_READ_ONLY))
	    show_lock(sp); 
	  else hide_lock(sp);
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

    case SP_FILTER_HZING:   
      set_filter_in_hz(sp, XEN_TO_C_BOOLEAN(val));
      break;

    case SP_FILTER_ORDER:
      set_filter_order(sp, XEN_TO_C_INT(val));
      break;

    case SP_SHOW_CONTROLS:
      if (HAS_WIDGETS(sp)) 
	{
	  if (XEN_TO_C_BOOLEAN(val))
	    show_controls(sp); 
	  else hide_controls(sp); 
	}
      break;

    case SP_SPEED_TONES:
      sp->speed_control_tones = XEN_TO_C_INT(val);
      if (sp->speed_control_tones <= 0) 
	sp->speed_control_tones = DEFAULT_SPEED_CONTROL_TONES;
      set_speed(sp, sp->speed_control); /* update label etc */
      break;

    case SP_SPEED_STYLE:
      sp->speed_control_style = (speed_style_t)XEN_TO_C_INT(val); /* range checked already */
#if XEN_HAVE_RATIOS
      if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
	snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
      set_speed(sp, sp->speed_control); /* update label etc */
      break;

    case SP_SRATE:
      if (!(IS_PLAYER_SOUND(sp))) 
	{
	  ival = XEN_TO_C_INT_OR_ELSE(val, 44100);
	  if ((ival <= 0) || (ival > 100000000))
	    XEN_OUT_OF_RANGE_ERROR(S_setB S_srate, 1, val, "impossible srate");
	  mus_sound_set_srate(sp->filename, ival);
	  sp->hdr->srate = ival;
	  /* if there are pending edits, we certainly don't want to flush them in this case! */
	  if (!(has_unsaved_edits(sp)))
	    snd_update_within_xen(sp, caller); 
	  else 
	    {
	      /* reset x axis bounds */
	      int i;
	      for (i = 0; i < sp->nchans; i++)
		set_x_axis_x0x1(sp->chans[i], 0.0, (double)(CURRENT_SAMPLES(sp->chans[i])) / (double)ival);
	    }
	}
      break;

    case SP_NCHANS: 
      if (!(IS_PLAYER_SOUND(sp))) 
	{
	  ival = XEN_TO_C_INT_OR_ELSE(val, 1);
	  if ((ival <= 0) || (ival > 256))
	    XEN_OUT_OF_RANGE_ERROR(S_setB S_channels, 1, val, "highly unlikely number of channels");
	  mus_sound_set_chans(sp->filename, ival);
	  sp->hdr->chans = ival;
	  snd_update_within_xen(sp, caller); 
	}
      break;

    case SP_DATA_FORMAT:
      if (!(IS_PLAYER_SOUND(sp))) 
	{
	  ival = XEN_TO_C_INT(val);
	  if (mus_data_format_p(ival))
	    {
	      chan_info *cp;
	      int old_format;
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
		  if ((cp) && (cp->edits[cp->edit_ctr]->peak_env))
		    cp->edits[cp->edit_ctr]->peak_env = free_peak_env(cp, cp->edit_ctr);
		}
	      snd_update_within_xen(sp, caller);
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_setB S_data_format, 1, val, "unknown data format");
	}
      break;

    case SP_HEADER_TYPE:
      if (!(IS_PLAYER_SOUND(sp))) 
	{
	  ival = XEN_TO_C_INT(val);
	  if (mus_header_type_p(ival))
	    {
	      mus_sound_set_header_type(sp->filename, ival);
	      snd_update_within_xen(sp, caller); 
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_setB S_header_type, 1, val, "unknown header type");
	}
      break;

    case SP_DATA_LOCATION:  
      if (!(IS_PLAYER_SOUND(sp))) 
	{
	  mus_long_t loc;
	  loc = XEN_TO_C_LONG_LONG(val);
	  if (loc >= 0)
	    {
	      mus_sound_set_data_location(sp->filename, loc);
	      snd_update_within_xen(sp, caller); 
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_setB S_data_location, 1, val, "data location < 0?");
	}
      break;

    case SP_DATA_SIZE:  
      if (!(IS_PLAYER_SOUND(sp))) 
	{
	  mus_long_t size;
	  size = XEN_TO_C_LONG_LONG(val);
	  if (size >= 0)
	    {
	      mus_sound_set_samples(sp->filename, mus_bytes_to_samples(sp->hdr->format, size));
	      snd_update_within_xen(sp, caller); 
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_setB S_data_size, 1, val, "data size < 0?");
	}
      break;

    case SP_COMMENT:
      if (!(IS_PLAYER_SOUND(sp))) 
	{
	  if (sp->hdr->comment) free(sp->hdr->comment);
	  if (XEN_FALSE_P(val))
	    sp->hdr->comment = NULL;
	  else sp->hdr->comment = mus_strdup(XEN_TO_C_STRING(val));
	}
      break;

    case SP_PROPERTIES:
      if (!(IS_PLAYER_SOUND(sp)))
	{
	  if (!(XEN_VECTOR_P(sp->properties)))
	    {
	      sp->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
	      sp->properties_loc = snd_protect(sp->properties);
	    }
	  XEN_VECTOR_SET(sp->properties, 0, val);
	  return(XEN_VECTOR_REF(sp->properties, 0));
	}
      break;

    case SP_AMP:           
      fval = XEN_TO_C_DOUBLE(val);
      if (fval >= 0.0) set_amp(sp, fval); 
      return(C_TO_XEN_DOUBLE(sp->amp_control)); 
      break;

    case SP_AMP_BOUNDS:
      sp->amp_control_min = XEN_TO_C_DOUBLE(XEN_CAR(val));
      sp->amp_control_max = XEN_TO_C_DOUBLE(XEN_CADR(val));
      set_amp(sp, mus_fclamp(sp->amp_control_min, sp->amp_control, sp->amp_control_max));
      return(val);
      break;

    case SP_CONTRAST:      
      set_contrast(sp, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(sp->contrast_control)); 
      break;

    case SP_CONTRAST_BOUNDS:
      sp->contrast_control_min = XEN_TO_C_DOUBLE(XEN_CAR(val));
      sp->contrast_control_max = XEN_TO_C_DOUBLE(XEN_CADR(val));
      set_contrast(sp, mus_fclamp(sp->contrast_control_min, sp->contrast_control, sp->contrast_control_max));
      return(val);
      break;

    case SP_CONTRAST_AMP:  
      sp->contrast_control_amp = XEN_TO_C_DOUBLE(val);
      if (sp->playing) dac_set_contrast_amp(sp, sp->contrast_control_amp);
      break;

    case SP_EXPAND:        
      fval = XEN_TO_C_DOUBLE(val);
      if (fval > 0.0) set_expand(sp, fval); 
      return(C_TO_XEN_DOUBLE(sp->expand_control)); 
      break;

    case SP_EXPAND_BOUNDS:
      sp->expand_control_min = XEN_TO_C_DOUBLE(XEN_CAR(val));
      sp->expand_control_max = XEN_TO_C_DOUBLE(XEN_CADR(val));
      set_expand(sp, mus_fclamp(sp->expand_control_min, sp->expand_control, sp->expand_control_max));
      return(val);
      break;

    case SP_EXPAND_LENGTH: 
      fval = XEN_TO_C_DOUBLE(val);
      if (fval > 0.0) 
	{
	  sp->expand_control_length = fval; 
	  if (sp->playing) dac_set_expand_length(sp, sp->expand_control_length);
	}
      return(C_TO_XEN_DOUBLE(sp->expand_control_length));
      break;

    case SP_EXPAND_RAMP:   
      fval = XEN_TO_C_DOUBLE(val);
      if ((fval >= 0.0) && (fval < 0.5)) 
	{
	  sp->expand_control_ramp = fval; 
	  if (sp->playing) dac_set_expand_ramp(sp, fval); 
	}
      return(C_TO_XEN_DOUBLE(sp->expand_control_ramp));
      break;

    case SP_EXPAND_HOP:    
      fval = XEN_TO_C_DOUBLE(val);
      if (fval > 0.0) 
	{
	  sp->expand_control_hop = fval; 
	  if (sp->playing) dac_set_expand_hop(sp, fval); 
	}
      return(C_TO_XEN_DOUBLE(sp->expand_control_hop));
      break;

    case SP_EXPAND_JITTER:    
      fval = mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), 100.0);
      sp->expand_control_jitter = fval; 
      return(C_TO_XEN_DOUBLE(sp->expand_control_jitter));
      break;

    case SP_SPEED: 
#if XEN_HAVE_RATIOS
      if ((sp->speed_control_style == SPEED_CONTROL_AS_RATIO) &&
	  (XEN_RATIO_P(val)))
	{
	  sp->speed_control_numerator = (int)XEN_NUMERATOR(val);
	  sp->speed_control_denominator = (int)XEN_DENOMINATOR(val);
	  fval = (mus_float_t)(sp->speed_control_numerator) / (mus_float_t)(sp->speed_control_denominator);
	  if (sp->speed_control_numerator < 0)
	    {
	      sp->speed_control_direction = -1;
	      sp->speed_control_numerator = -sp->speed_control_numerator;
	    }
	  else sp->speed_control_direction = 1;
	  set_speed(sp, fabs(fval));
	  sp->speed_control = fabs(fval); /* not redundant */
	  toggle_direction_arrow(sp, (sp->speed_control_direction == -1));
	  return(val);
	}
#endif
      fval = XEN_TO_C_DOUBLE(val);
      if (fval != 0.0)
	{
	  int direction;
	  if (fval > 0.0) direction = 1; else direction = -1;
	  set_speed(sp, fabs(fval)); 
#if XEN_HAVE_RATIOS
	  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
	    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
	  toggle_direction_arrow(sp, (direction == -1));
	  if (sp->speed_control_direction == -1) 
	    return(C_TO_XEN_DOUBLE((-(sp->speed_control)))); 
	  else return(C_TO_XEN_DOUBLE(sp->speed_control));
	}
      break;

    case SP_SPEED_BOUNDS:
      sp->speed_control_min = XEN_TO_C_DOUBLE(XEN_CAR(val));
      sp->speed_control_max = XEN_TO_C_DOUBLE(XEN_CADR(val));
      set_speed(sp, mus_fclamp(sp->speed_control_min, sp->speed_control, sp->speed_control_max));
      return(val);
      break;

    case SP_REVERB_LENGTH:    
      fval = XEN_TO_C_DOUBLE(val);
      if (fval >= 0.0) set_revlen(sp, fval); 
      return(C_TO_XEN_DOUBLE(sp->reverb_control_length)); 
      break;

    case SP_REVERB_LENGTH_BOUNDS:
      sp->reverb_control_length_min = XEN_TO_C_DOUBLE(XEN_CAR(val));
      sp->reverb_control_length_max = XEN_TO_C_DOUBLE(XEN_CADR(val));
      set_revlen(sp, mus_fclamp(sp->reverb_control_length_min, sp->reverb_control_length, sp->reverb_control_length_max));
      return(val);
      break;

    case SP_REVERB_FEEDBACK:  
      sp->reverb_control_feedback = mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), 100.0);
      if (sp->playing) dac_set_reverb_feedback(sp, sp->reverb_control_feedback);
      break;

    case SP_REVERB_SCALE:     
      set_revscl(sp, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(sp->reverb_control_scale)); 
      break;

    case SP_REVERB_SCALE_BOUNDS:
      sp->reverb_control_scale_min = XEN_TO_C_DOUBLE(XEN_CAR(val));
      sp->reverb_control_scale_max = XEN_TO_C_DOUBLE(XEN_CADR(val));
      set_revscl(sp, mus_fclamp(sp->reverb_control_scale_min, sp->reverb_control_scale, sp->reverb_control_scale_max));
      return(val);
      break;

    case SP_REVERB_LOW_PASS:  
      sp->reverb_control_lowpass = mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), 1.0);
      if (sp->playing) dac_set_reverb_lowpass(sp, sp->reverb_control_lowpass);
      break;

    case SP_REVERB_DECAY:     
      sp->reverb_control_decay = XEN_TO_C_DOUBLE(val);
      break;

    case SP_FILTER_ENVELOPE:
      {
	env *e = NULL;
	if (sp->filter_control_envelope) 
	  sp->filter_control_envelope = free_env(sp->filter_control_envelope);  /* set to null in case get_env throws error */
	if (!(XEN_FALSE_P(val)))
	  e = get_env(val, caller); /* has some error checks -- val must be list, but we can be #f -- see "get" case above: null env (nogui) -> #f */
	if (e)
	  {
	    for (i = 0; i < e->pts; i++)
	      if ((e->data[i * 2 + 1] > 1.0) ||
		  (e->data[i * 2 + 1] < 0.0))
		{
		  free_env(e);
		  XEN_OUT_OF_RANGE_ERROR(caller, 1, val, "y values < 0.0 or > 1.0");
		}
	    sp->filter_control_envelope = e;
	    filter_env_changed(sp, sp->filter_control_envelope);
	  }
      }
      break;

    default:
      break;
    }
  return(val);
}


static XEN sound_set_global(XEN snd, XEN val, sp_field_t fld, const char *caller)
{
  mus_float_t fval;
  if (XEN_NOT_BOUND_P(snd))
    switch (fld)
      {
      case SP_FILTER_DBING:   
	in_set_filter_control_in_dB(ss, XEN_TO_C_BOOLEAN(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_FILTER_HZING:   
	in_set_filter_control_in_hz(ss, XEN_TO_C_BOOLEAN(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_FILTER_ORDER:
	if (XEN_TO_C_INT(val) > 0)
	  in_set_filter_control_order(ss, XEN_TO_C_INT(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_SHOW_CONTROLS:
	in_set_show_controls(ss, XEN_TO_C_BOOLEAN(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_SPEED_TONES:
	in_set_speed_control_tones(ss, XEN_TO_C_INT(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_SPEED_STYLE:
	in_set_speed_control_style(ss, (speed_style_t)XEN_TO_C_INT(val)); /* range checked already */
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_AMP_BOUNDS:
	in_set_amp_control_min(ss, XEN_TO_C_DOUBLE(XEN_CAR(val)));
	in_set_amp_control_max(ss, XEN_TO_C_DOUBLE(XEN_CADR(val)));
	reflect_mix_change(ANY_MIX_ID);
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_CONTRAST_BOUNDS:
	in_set_contrast_control_min(ss, XEN_TO_C_DOUBLE(XEN_CAR(val)));
	in_set_contrast_control_max(ss, XEN_TO_C_DOUBLE(XEN_CADR(val)));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_CONTRAST_AMP:  
	in_set_contrast_control_amp(ss, XEN_TO_C_DOUBLE(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_EXPAND_BOUNDS:
	in_set_expand_control_min(ss, XEN_TO_C_DOUBLE(XEN_CAR(val)));
	in_set_expand_control_max(ss, XEN_TO_C_DOUBLE(XEN_CADR(val)));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_EXPAND_LENGTH: 
	fval = XEN_TO_C_DOUBLE(val);
	if (fval > 0.0)
	  in_set_expand_control_length(ss, fval);
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_EXPAND_RAMP:
	fval = XEN_TO_C_DOUBLE(val);
	if ((fval >= 0.0) && (fval < 0.5)) 
	  in_set_expand_control_ramp(ss, fval);
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_EXPAND_HOP:
	fval = XEN_TO_C_DOUBLE(val);
	if (fval > 0.0)
	  in_set_expand_control_hop(ss, fval);
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_EXPAND_JITTER:    
	in_set_expand_control_jitter(ss, XEN_TO_C_DOUBLE(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_SPEED_BOUNDS:
	in_set_speed_control_min(ss, XEN_TO_C_DOUBLE(XEN_CAR(val)));
	in_set_speed_control_max(ss, XEN_TO_C_DOUBLE(XEN_CADR(val)));
	reflect_mix_change(ANY_MIX_ID);
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_REVERB_LENGTH_BOUNDS:
	in_set_reverb_control_length_min(ss, XEN_TO_C_DOUBLE(XEN_CAR(val)));
	in_set_reverb_control_length_max(ss, XEN_TO_C_DOUBLE(XEN_CADR(val)));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_REVERB_FEEDBACK:  
	in_set_reverb_control_feedback(ss, XEN_TO_C_DOUBLE(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_REVERB_SCALE_BOUNDS:
	in_set_reverb_control_scale_min(ss, XEN_TO_C_DOUBLE(XEN_CAR(val)));
	in_set_reverb_control_scale_max(ss, XEN_TO_C_DOUBLE(XEN_CADR(val)));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_REVERB_LOW_PASS:  
	in_set_reverb_control_lowpass(ss, XEN_TO_C_DOUBLE(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      case SP_REVERB_DECAY:     
	in_set_reverb_control_decay(ss, XEN_TO_C_DOUBLE(val));
	return(sound_set(XEN_TRUE, val, fld, caller));
	break;

      default: break;
      }
  return(sound_set(snd, val, fld, caller));
}


static XEN g_channels(XEN snd)
{
  #define H_channels "("  S_channels " :optional obj): how many channels the object obj has"

  if (XEN_STRING_P(snd))
    return(g_mus_sound_chans(snd));              /* mus-sound-chans */

  if ((mus_xen_p(snd)) ||
      (sound_data_p(snd)) ||                     /* sound-data-chans */
      (MUS_VCT_P(snd)) ||
      (XEN_LIST_P(snd)))
    return(g_mus_channels(snd));                 /* mus-channels */

  if (XEN_MIX_P(snd))                            /* mixes are always 1 chan */
    return(C_TO_XEN_INT(1));

  if (XEN_REGION_P(snd))                         /* region-chans */
    return(g_region_chans(snd));

  if (XEN_SELECTION_P(snd))                      /* selection-chans */
    return(g_selection_chans());

  if (XEN_VECTOR_P(snd))                         /* vector as output in clm */
    return(C_TO_XEN_INT(XEN_VECTOR_RANK(snd)));

  return(sound_get(snd, SP_NCHANS, S_channels));
}


static XEN check_number(XEN val, const char *caller)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, caller, "a number");
  return(val);
}


static XEN g_set_channels(XEN snd, XEN val)
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd, S_setB S_channels), SP_NCHANS, S_setB S_channels));
  else return(sound_set(snd, check_number(val, S_setB S_channels), SP_NCHANS, S_setB S_channels));
}


static XEN g_srate(XEN snd) 
{
  #define H_srate "(" S_srate " :optional obj): obj's srate; obj can be a region, a string (sound file name), a sound, or an integer (sound index)"

  if (XEN_STRING_P(snd))
    return(g_mus_sound_srate(snd));

  if (XEN_REGION_P(snd))
    return(g_region_srate(snd));

  if (XEN_SELECTION_P(snd))
    return(g_selection_srate());

  return(sound_get(snd, SP_SRATE, S_srate));
}


static XEN g_set_srate(XEN snd, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd, S_setB S_srate), SP_SRATE, S_setB S_srate));
  else return(sound_set(snd, check_number(val, S_setB S_srate), SP_SRATE, S_setB S_srate));
}


static XEN g_data_location(XEN snd) 
{
  #define H_data_location "(" S_data_location " :optional snd): snd's data location (bytes)"
  return(sound_get(snd, SP_DATA_LOCATION, S_data_location));
}


static XEN g_set_data_location(XEN snd, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd, S_setB S_data_location), SP_DATA_LOCATION, S_setB S_data_location));
  else return(sound_set(snd, check_number(val, S_setB S_data_location), SP_DATA_LOCATION, S_setB S_data_location));
}


static XEN g_data_size(XEN snd) 
{
  #define H_data_size "(" S_data_size " :optional snd): snd's data size (bytes)"
  return(sound_get(snd, SP_DATA_SIZE, S_data_size));
}


static XEN g_set_data_size(XEN snd, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd, S_setB S_data_size), SP_DATA_SIZE, S_setB S_data_size));
  else return(sound_set(snd, check_number(val, S_setB S_data_size), SP_DATA_SIZE, S_setB S_data_size));
}


static XEN g_data_format(XEN snd) 
{
  #define H_data_format "(" S_data_format " :optional snd): snd's data format (e.g. " S_mus_bshort ")"
  return(sound_get(snd, SP_DATA_FORMAT, S_data_format));
}


static XEN g_set_data_format(XEN snd, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd, S_setB S_data_format), SP_DATA_FORMAT, S_setB S_data_format));
  else return(sound_set(snd, check_number(val, S_setB S_data_format), SP_DATA_FORMAT, S_setB S_data_format));
}


static XEN g_header_type(XEN snd) 
{
  #define H_header_type "(" S_header_type " :optional snd): snd's header type (e.g. " S_mus_aiff ")"
  return(sound_get(snd, SP_HEADER_TYPE, S_header_type));
}


static XEN g_set_header_type(XEN snd, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    return(sound_set(XEN_UNDEFINED, check_number(snd, S_setB S_header_type), SP_HEADER_TYPE, S_setB S_header_type));
  else return(sound_set(snd, check_number(val, S_setB S_header_type), SP_HEADER_TYPE, S_setB S_header_type));
}


static XEN g_comment(XEN snd)
{
  #define H_comment "(" S_comment " :optional snd): snd's comment (in its header)"
  return(sound_get(snd, SP_COMMENT, S_comment));
}


static XEN g_set_comment(XEN snd, XEN val) 
{
  if (XEN_NOT_BOUND_P(val))
    {
      XEN_ASSERT_TYPE(XEN_STRING_P(snd) || XEN_FALSE_P(snd), snd, XEN_ARG_1, S_setB S_comment, "a string");
      return(sound_set(XEN_UNDEFINED, snd, SP_COMMENT, S_setB S_comment));
    }

  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ARG_2, S_setB S_comment, "a string");
  return(sound_set(snd, val, SP_COMMENT, S_setB S_comment));
}


static XEN g_sync(XEN snd) 
{
  #define H_sync "(" S_sync " :optional snd): snd's sync value (0 = no sync).  Some editing operations \
are applied to all sounds sharing the sync value of the selected sound.  'snd' can also be a mix or mark object."

  if (XEN_MIX_P(snd))                            /* mix-sync */
    return(g_mix_sync(snd));

  if (XEN_MARK_P(snd))                           /* mark-sync */
    return(g_mark_sync(snd));

  return(sound_get(snd, SP_SYNC, S_sync));       /* sync */
}


static XEN g_set_sync(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_sync, "an integer");

  if (XEN_MIX_P(snd))
    return(g_set_mix_sync(snd, on));

  if (XEN_MARK_P(snd))
    return(g_set_mark_sync(snd, on));

  return(sound_set(snd, on, SP_SYNC, S_setB S_sync));
}

WITH_TWO_SETTER_ARGS(g_set_sync_reversed, g_set_sync)


static XEN g_sync_max(void) 
{
  #define H_sync_max "(" S_sync_max "): max sound sync value seen so far"
  return(C_TO_XEN_INT(ss->sound_sync_max));
}


static XEN g_sound_properties(XEN snd) 
{
  #define H_sound_properties "(" S_sound_properties " :optional snd): snd's property list"
  return(sound_get(snd, SP_PROPERTIES, S_sound_properties));
}


static XEN g_set_sound_properties(XEN on, XEN snd) 
{
  return(sound_set(snd, on, SP_PROPERTIES, S_setB S_sound_properties));
}

WITH_TWO_SETTER_ARGS(g_set_sound_properties_reversed, g_set_sound_properties)


static XEN g_sound_property(XEN key, XEN snd) 
{
  #define H_sound_property "(" S_sound_property " key snd) returns the value associated with 'key' in the given sound's\
property list, or " PROC_FALSE "."
  return(XEN_ASSOC_REF(key, g_sound_properties(snd)));
}

#if HAVE_SCHEME
static XEN g_set_sound_property(XEN val, XEN key, XEN snd) 
#else
static XEN g_set_sound_property(XEN key, XEN val, XEN snd) 
#endif
{
  g_set_sound_properties(XEN_ASSOC_SET(key, val, g_sound_properties(snd)), snd);
  return(val);
}

WITH_THREE_SETTER_ARGS(g_set_sound_property_reversed, g_set_sound_property)



static XEN g_channel_style(XEN snd) 
{
  snd_info *sp;

  if (XEN_NOT_BOUND_P(snd))
    return(C_TO_XEN_INT(channel_style(ss)));

  ASSERT_SOUND(S_channel_style, snd, 1);
  sp = get_sp(snd);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_channel_style, snd));

  return(C_TO_XEN_INT((int)(sp->channel_style)));
}


static void update_sound(snd_info *sp)
{
  if (sp)
    {
      switch (channel_style(ss))
	{
	case CHANNELS_SEPARATE:     separate_sound(sp);    break;
	case CHANNELS_COMBINED:     combine_sound(sp);     break;
	case CHANNELS_SUPERIMPOSED: superimpose_sound(sp); break;
	default:
	  break;
	}
    }
}


void set_channel_style(channel_style_t val)
{
  in_set_channel_style(val);
  for_each_sound(update_sound);
  for_each_chan(update_graph);
}


static XEN g_set_channel_style(XEN style, XEN snd) 
{
  snd_info *sp;
  int in_style;
  channel_style_t new_style = CHANNELS_SEPARATE;

  #define H_channel_style "(" S_channel_style " :optional snd): how multichannel sounds lay out the channels. \
The default is " S_channels_combined "; other values are " S_channels_separate " and " S_channels_superimposed ". \
As a global (if the 'snd' arg is omitted), it is the default setting for each sound's 'unite' button."

  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_channel_style, "an integer"); 
  in_style = XEN_TO_C_INT(style);
  if ((in_style < 0) ||
      (in_style >= NUM_CHANNEL_STYLES))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_channel_style, 1, style, S_channel_style " should be " S_channels_separate ", " S_channels_combined ", or " S_channels_superimposed);
  new_style = (channel_style_t)in_style;

  if (XEN_NOT_BOUND_P(snd))
    {
      set_channel_style(new_style);
      return(C_TO_XEN_INT(channel_style(ss)));
    }

  ASSERT_SOUND(S_setB S_channel_style, snd, 2);
  sp = get_sp(snd);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_setB S_channel_style, snd));

  set_sound_channel_style(sp, new_style);

  return(C_TO_XEN_INT((int)(sp->channel_style)));
}

WITH_TWO_SETTER_ARGS(g_set_channel_style_reversed, g_set_channel_style)


static XEN g_read_only(XEN snd) 
{
  #define H_read_only "(" S_read_only " :optional snd): whether snd is write-protected"
  return(sound_get(snd, SP_READ_ONLY, S_read_only));
}


static XEN g_set_read_only(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_read_only, "a boolean");
  return(sound_set(snd, on, SP_READ_ONLY, S_setB S_read_only));
}

WITH_TWO_SETTER_ARGS(g_set_read_only_reversed, g_set_read_only)


static XEN g_contrast_control_p(XEN snd) 
{
  #define H_contrast_control_p "(" S_contrast_control_p " :optional snd): snd's control panel constrast button state"
  return(sound_get(snd, SP_CONTRASTING, S_contrast_control_p));
}


static XEN g_set_contrast_control_p(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_contrast_control_p, "a boolean");
  return(sound_set(snd, on, SP_CONTRASTING, S_setB S_contrast_control_p));
}

WITH_TWO_SETTER_ARGS(g_set_contrast_control_p_reversed, g_set_contrast_control_p)


static XEN g_expand_control_p(XEN snd) 
{
  #define H_expand_control_p "(" S_expand_control_p " :optional snd): snd's control panel expand button state"
  return(sound_get(snd, SP_EXPANDING, S_expand_control_p));
}


static XEN g_set_expand_control_p(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_expand_control_p, "a boolean");
  return(sound_set(snd, on, SP_EXPANDING, S_setB S_expand_control_p));
}

WITH_TWO_SETTER_ARGS(g_set_expand_control_p_reversed, g_set_expand_control_p)


static XEN g_reverb_control_p(XEN snd) 
{
  #define H_reverb_control_p "(" S_reverb_control_p " :optional snd): snd's control panel reverb button state"
  return(sound_get(snd, SP_REVERBING, S_reverb_control_p));
}


static XEN g_set_reverb_control_p(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_reverb_control_p, "a boolean");
  return(sound_set(snd, on, SP_REVERBING, S_setB S_reverb_control_p));
}

WITH_TWO_SETTER_ARGS(g_set_reverb_control_p_reversed, g_set_reverb_control_p)


static XEN g_filter_control_p(XEN snd) 
{
  #define H_filter_control_p "(" S_filter_control_p " :optional snd): snd's control panel filter button state"
  return(sound_get(snd, SP_FILTERING, S_filter_control_p));
}


static XEN g_set_filter_control_p(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_filter_control_p, "a boolean");
  return(sound_set(snd, on, SP_FILTERING, S_setB S_filter_control_p));
}

WITH_TWO_SETTER_ARGS(g_set_filter_control_p_reversed, g_set_filter_control_p)


static XEN g_filter_control_in_dB(XEN snd) 
{
  #define H_filter_control_in_dB "(" S_filter_control_in_dB " :optional snd): " PROC_TRUE " if snd's filter envelope is displayed in dB in control panel"
  return(sound_get_global(snd, SP_FILTER_DBING, S_filter_control_in_dB));
}


static XEN g_set_filter_control_in_dB(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_filter_control_in_dB, "a boolean");
  return(sound_set_global(snd, on, SP_FILTER_DBING, S_setB S_filter_control_in_dB));
}

WITH_TWO_SETTER_ARGS(g_set_filter_control_in_dB_reversed, g_set_filter_control_in_dB)


static XEN g_filter_control_in_hz(XEN snd) 
{
  #define H_filter_control_in_hz "(" S_filter_control_in_hz " :optional snd): " PROC_TRUE " if snd's filter envelope x axis should be in hz (control panel filter)"
  return(sound_get_global(snd, SP_FILTER_HZING, S_filter_control_in_hz));
}


static XEN g_set_filter_control_in_hz(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_filter_control_in_hz, "a boolean");
  return(sound_set_global(snd, on, SP_FILTER_HZING, S_setB S_filter_control_in_hz));
}

WITH_TWO_SETTER_ARGS(g_set_filter_control_in_hz_reversed, g_set_filter_control_in_hz)


static XEN g_filter_control_coeffs(XEN snd) 
{
  #define H_filter_control_coeffs "(" S_filter_control_coeffs " :optional snd): control panel filter coeffs"
  return(sound_get(snd, SP_FILTER_COEFFS, S_filter_control_coeffs));
}


static XEN g_filter_control_order(XEN snd) 
{
  #define H_filter_control_order "(" S_filter_control_order " :optional snd): filter order (in control panel)"
  return(sound_get_global(snd, SP_FILTER_ORDER, S_filter_control_order));
}


static XEN g_set_filter_control_order(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, S_setB S_filter_control_order, "an integer"); 
  return(sound_set_global(snd, on, SP_FILTER_ORDER, S_setB S_filter_control_order));
}

WITH_TWO_SETTER_ARGS(g_set_filter_control_order_reversed, g_set_filter_control_order)


static XEN g_show_controls(XEN snd) 
{
  #define H_show_controls "(" S_show_controls " :optional snd): " PROC_TRUE " if snd's control panel is known to be open"
  return(sound_get_global(snd, SP_SHOW_CONTROLS, S_show_controls));
}


static XEN g_set_show_controls(XEN on, XEN snd)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_show_controls, "a boolean");
  return(sound_set_global(snd, on, SP_SHOW_CONTROLS, S_setB S_show_controls));
}

WITH_TWO_SETTER_ARGS(g_set_show_controls_reversed, g_set_show_controls)


static XEN g_save_controls(XEN snd) 
{
  #define H_save_controls "(" S_save_controls " :optional snd): save the control panel settings for subsequent " S_restore_controls
  return(sound_get(snd, SP_SAVE_CONTROLS, S_save_controls));
}


static XEN g_restore_controls(XEN snd) 
{
  #define H_restore_controls "(" S_restore_controls " :optional snd): restore the previously saved control panel settings"
  return(sound_get(snd, SP_RESTORE_CONTROLS, S_restore_controls));
}


static XEN g_reset_controls(XEN snd) 
{
  #define H_reset_controls "(" S_reset_controls " :optional snd): reset (clear) the control panel settings"
  return(sound_get(snd, SP_RESET_CONTROLS, S_reset_controls));
}


static XEN g_selected_channel(XEN snd) 
{
  #define H_selected_channel "(" S_selected_channel " :optional snd): currently selected channel in snd (or " PROC_FALSE " if none)"
  return(sound_get(snd, SP_SELECTED_CHANNEL, S_selected_channel));
}


static XEN g_set_selected_channel(XEN snd, XEN chn_n) 
{
  snd_info *sp;

  if (XEN_NOT_BOUND_P(chn_n))
    return(g_select_channel(snd));

  ASSERT_SOUND(S_setB S_selected_channel, snd, 1); 
  sp = get_sp(snd);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_setB S_selected_channel, snd));

  if (XEN_FALSE_P(chn_n))
    sp->selected_channel = NO_SELECTION;
  else
    {
      int chan;
      chan = XEN_TO_C_INT_OR_ELSE(chn_n, 0);
      if ((chan >= 0) && 
	  (chan < sp->nchans)) 
	{
	  select_channel(sp, chan);
	  return(chn_n);
	}
      return(snd_no_such_channel_error(S_setB S_selected_channel, snd, chn_n));
    }

  return(XEN_FALSE);
}


static XEN g_file_name(XEN snd) 
{
  #define H_file_name "(" S_file_name " :optional snd): snd's full filename; snd can be a sound, mix, region, string, or generator."

  if (XEN_SOUND_P(snd))
    return(sound_get(snd, SP_FILE_NAME, S_file_name));

  if (mus_xen_p(snd))
    return(g_mus_file_name(snd));

  if (XEN_MIX_P(snd))
    return(C_TO_XEN_STRING(mix_file_name(XEN_MIX_TO_C_INT(snd))));

  if (XEN_REGION_P(snd))
    return(C_TO_XEN_STRING(region_file_name(XEN_REGION_TO_C_INT(snd))));

#if HAVE_SCHEME
  if ((s7_is_input_port(s7, snd)) || (s7_is_output_port(s7, snd)))
    return(C_TO_XEN_STRING(s7_port_filename(snd)));
#endif

  if (XEN_STRING_P(snd))
    return(g_mus_expand_filename(snd));

  if ((sampler_p(snd)) || (mix_sampler_p(snd)))
    return(g_sampler_file_name(snd));

  return(sound_get(snd, SP_FILE_NAME, S_file_name));
}


static XEN g_short_file_name(XEN snd) 
{
  #define H_short_file_name "(" S_short_file_name " :optional snd): short form of snd's file name (no directory)"
  return(sound_get(snd, SP_SHORT_FILE_NAME, S_short_file_name));
}


static XEN g_close_sound_1(int snd)
{
  if ((snd >= 0) &&
      (snd < ss->max_sounds))
    {
      snd_info *sp;
      sp = ss->sounds[snd];
      if (snd_ok(sp))
	{
	  if (sp->inuse == SOUND_WRAPPER) /* from make_simple_channel_display (variable-graph and the region graphs) */
	    {
	      /* not sure what to do in this case, but at least we can get it out of the various #t chan loops */
	      sp->inuse = SOUND_IDLE;
	      ss->sounds[sp->index] = NULL; /* a huge memory leak... */
	    }
	  else snd_close_file(sp);
	}
    }
  return(XEN_FALSE);
}


static XEN g_close_sound(XEN snd) 
{
  #define H_close_sound "(" S_close_sound " :optional snd): close snd"

  if (XEN_INTEGER_P(snd))
    return(g_close_sound_1(XEN_TO_C_INT(snd)));

  if (XEN_SOUND_P(snd))
    return(g_close_sound_1(XEN_SOUND_TO_C_INT(snd)));

  return(sound_get(snd, SP_CLOSE, S_close_sound));
}


static XEN g_update_sound(XEN snd) 
{
  #define H_update_sound "(" S_update_sound " :optional snd): update snd (re-read it from the disk after flushing pending edits)"
  return(sound_get(snd, SP_UPDATE, S_update_sound));
}


static void save_sound_error_handler(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  XEN_ERROR(CANNOT_SAVE,
	    XEN_LIST_3(C_TO_XEN_STRING("~A: ~A"),
		       C_TO_XEN_STRING((char *)data),
		       C_TO_XEN_STRING(msg)));
}


static XEN g_save_sound(XEN index) 
{
  snd_info *sp;
  io_error_t err = IO_NO_ERROR;
  #define H_save_sound "(" S_save_sound " :optional snd): save snd (update the on-disk data to match Snd's current version)"

  ASSERT_SOUND(S_save_sound, index, 1);

  sp = get_sp(index);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_save_sound, index));

  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    {
      char *msg;
      XEN str;
      msg = mus_format("%s (index %d) is write-protected", 
		       sp->short_filename, 
		       sp->index);
      str = C_TO_XEN_STRING(msg);
      free(msg);
      XEN_ERROR(CANNOT_SAVE,
		XEN_LIST_2(C_TO_XEN_STRING(S_save_sound ": can't save sound, ~A"),
			   str));
      return(XEN_FALSE);
    }

  redirect_snd_error_to(save_sound_error_handler, (void *)S_save_sound);
  redirect_snd_warning_to(save_sound_error_handler, (void *)S_save_sound);
  err = save_edits_without_asking(sp);      
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);

  /* if err and we got here, report it */
  if (SERIOUS_IO_ERROR(err))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_save_sound ": IO error ~A"),
			 C_TO_XEN_STRING(io_error_name(err))));
	      
  return(C_INT_TO_XEN_SOUND(sp->index));
}


static XEN g_revert_sound(XEN index)
{
  #define H_revert_sound "("  S_revert_sound " :optional snd): revert snd to its unedited state (undo all)"
  snd_info *sp;
  int i;

  ASSERT_SOUND(S_revert_sound, index, 1);

  sp = get_sp(index);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_revert_sound, index));

  for (i = 0; i < sp->nchans; i++) 
    {
      revert_edits(sp->chans[i]); 
      update_graph(sp->chans[i]);
    }
  reflect_file_revert_in_label(sp);

  return(index); /* was #t */
}


static XEN g_selected_sound(void)
{
  #define H_selected_sound "(" S_selected_sound "): currently selected sound (or " PROC_FALSE " if none)"
  if ((ss->selected_sound != NO_SELECTION) && 
      (snd_ok(ss->sounds[ss->selected_sound])))
    return(C_INT_TO_XEN_SOUND(ss->selected_sound));

  return(XEN_FALSE);
}


static void open_sound_error_handler(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  XEN_ERROR(XEN_ERROR_TYPE("not-a-sound-file"),
	    XEN_LIST_3(C_TO_XEN_STRING("~A: ~A"),
		       C_TO_XEN_STRING((char *)data),
		       C_TO_XEN_STRING(msg)));
}


static XEN g_open_sound(XEN filename)
{ 
  /* return new sound if successful */
  #define H_open_sound "(" S_open_sound " filename): \
open filename (as if opened from File:Open menu option), and return the new sound"

  const char *fname = NULL;
  snd_info *sp;
  bool file_exists;

  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_open_sound, "a string");

  fname = XEN_TO_C_STRING(filename);
  {
    char *fullname;
    /* before probing, need to undo all the Unix-isms */
    fullname = mus_expand_filename(fname);
    file_exists = mus_file_probe(fullname);
    free(fullname);
  }

  if (!file_exists)
    return(snd_no_such_file_error(S_open_sound, filename));
  ss->open_requestor = FROM_OPEN_SOUND;

  redirect_snd_error_to(open_sound_error_handler, (void *)S_open_sound);
  sp = snd_open_file(fname, FILE_READ_WRITE); /* this will call mus_expand_filename */
  redirect_snd_error_to(NULL, NULL);

  if (sp) 
    return(C_INT_TO_XEN_SOUND(sp->index));

  /* sp NULL is not an error (open-hook func returned #t) */
  return(XEN_FALSE);
}


static XEN kw_header_type, kw_data_format, kw_file, kw_srate, kw_channel, kw_sound, kw_edit_position, kw_channels, kw_size, kw_comment;

static void init_sound_keywords(void)
{
  kw_header_type = XEN_MAKE_KEYWORD("header-type");
  kw_data_format = XEN_MAKE_KEYWORD("data-format");
  kw_file = XEN_MAKE_KEYWORD("file");
  kw_srate = XEN_MAKE_KEYWORD("srate");
  kw_channel = XEN_MAKE_KEYWORD("channel");
  kw_sound = XEN_MAKE_KEYWORD("sound");
  kw_edit_position = XEN_MAKE_KEYWORD("edit-position");
  kw_channels = XEN_MAKE_KEYWORD("channels");
  kw_size = XEN_MAKE_KEYWORD("size");
  kw_comment = XEN_MAKE_KEYWORD("comment");
}


static XEN g_open_raw_sound(XEN arglist)
{
  #define H_open_raw_sound "(" S_open_raw_sound " file channels srate data-format): \
open file assuming the data matches the attributes indicated unless the file actually has a header"

  const char *file = NULL;
  char *fullname;
  snd_info *sp;
  bool file_exists;
  int os = 1, oc = 1, ofr = MUS_BSHORT;
  XEN args[8]; 
  XEN keys[4];
  int orig_arg[4] = {0, 0, 0, 0};
  int vals, i, arglist_len;

  keys[0] = kw_file;
  keys[1] = kw_channels;
  keys[2] = kw_srate;
  keys[3] = kw_data_format;

  mus_header_raw_defaults(&os, &oc, &ofr);

  for (i = 0; i < 8; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);

  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = mus_optkey_unscramble(S_open_raw_sound, 4, keys, args, orig_arg);

  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_open_raw_sound, orig_arg[0], NULL);
      oc = mus_optkey_to_int(keys[1], S_open_raw_sound, orig_arg[1], oc);
      if (!(XEN_KEYWORD_P(keys[1]))) set_fallback_chans(oc);
      os = mus_optkey_to_int(keys[2], S_open_raw_sound, orig_arg[2], os);
      if (!(XEN_KEYWORD_P(keys[2]))) set_fallback_srate(os);
      ofr = mus_optkey_to_int(keys[3], S_open_raw_sound, orig_arg[3], ofr);
      if (!(XEN_KEYWORD_P(keys[3]))) set_fallback_format(ofr);
    }

  if (file == NULL) 
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_1(C_TO_XEN_STRING(S_open_raw_sound ": no output file?")));

  fullname = mus_expand_filename(file);
  file_exists = mus_file_probe(fullname);
  free(fullname);
  if (!file_exists)
    return(snd_no_such_file_error(S_open_raw_sound, keys[0]));

  mus_header_set_raw_defaults(os, oc, ofr);
  ss->reloading_updated_file = -1;
  ss->open_requestor = FROM_OPEN_RAW_SOUND;

  sp = snd_open_file(file, FILE_READ_WRITE);

  set_fallback_chans(0);
  set_fallback_srate(0);
  set_fallback_format(MUS_UNKNOWN);
  ss->reloading_updated_file = 0;

  /* snd_open_file -> snd_open_file_1 -> add_sound_window -> make_file_info -> raw_data_dialog_to_file_info */
  /*   so here if hooked, we'd need to save the current hook, make it return the current args, open, then restore */

  if (sp) 
    return(C_INT_TO_XEN_SOUND(sp->index));
  return(XEN_FALSE);
}


#if HAVE_SCHEME
  #define read_only_example "You can make it writable via: (set! (" S_read_only ") #f)"
#endif
#if HAVE_RUBY
  #define read_only_example "You can make it writable via: set_read_only(false)"
#endif
#if HAVE_FORTH
  #define read_only_example "You can make it writable via: #f set-read-only"
#endif

static XEN g_view_sound(XEN filename)
{
  #define H_view_sound "(" S_view_sound " filename): open a file in read-only mode. " read_only_example " at any time."

  const char *fname = NULL;
  char *fullname;
  snd_info *sp = NULL;
  bool file_exists;

  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_view_sound, "a string");

  fname = XEN_TO_C_STRING(filename);
  fullname = mus_expand_filename(fname);
  file_exists = mus_file_probe(fullname);
  free(fullname);
  if (!file_exists)
    return(snd_no_such_file_error(S_view_sound, filename));

  ss->open_requestor = FROM_VIEW_SOUND;
  sp = snd_open_file(fname, FILE_READ_ONLY);

  if (sp) 
    return(C_INT_TO_XEN_SOUND(sp->index));
  return(XEN_FALSE);
}


static XEN g_save_sound_as(XEN arglist)
{
  #if HAVE_SCHEME
    #define save_as_example "(" S_save_sound_as " \"test.snd\" index " S_mus_next " " S_mus_bshort ")"
  #endif
  #if HAVE_RUBY
    #define save_as_example "save_sound_as(\"test.snd\", index, Mus_next, Mus_bshort)"
  #endif
  #if HAVE_FORTH
    #define save_as_example "\"test.snd\" index mus-next mus-bshort save-sound-as"
  #endif

  #define H_save_sound_as "("  S_save_sound_as " file sound header-type data-format srate channel edit-position comment): \
save sound in file using the indicated attributes.  If channel is specified, only that channel is saved (extracted). \
Omitted arguments take their value from the sound being saved.\n  " save_as_example
  
  snd_info *sp;
  file_info *hdr;
  int ht = -1, df = -1, sr = -1, chan = -1, edit_position = AT_CURRENT_EDIT_POSITION;
  io_error_t io_err = IO_NO_ERROR;
  char *fname = NULL;
  const char *file = NULL, *outcom = NULL;
  XEN args[16]; 
  XEN keys[8];
  int orig_arg[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len;
  XEN edpos = XEN_UNDEFINED, index = XEN_UNDEFINED;
  bool got_edpos = false, free_outcom = false;

  keys[0] = kw_file;
  keys[1] = kw_sound;
  keys[2] = kw_header_type;
  keys[3] = kw_data_format;
  keys[4] = kw_srate;
  keys[5] = kw_channel;
  keys[6] = kw_edit_position;
  keys[7] = kw_comment;

  for (i = 0; i < 16; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = mus_optkey_unscramble(S_save_sound_as, 8, keys, args, orig_arg);

  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_save_sound_as, orig_arg[0], NULL);
      if (!(XEN_KEYWORD_P(keys[1]))) index = keys[1];
      ht = mus_optkey_to_int(keys[2], S_save_sound_as, orig_arg[2], ht);
      df = mus_optkey_to_int(keys[3], S_save_sound_as, orig_arg[3], df);
      sr = mus_optkey_to_int(keys[4], S_save_sound_as, orig_arg[4], sr);

      if ((sr <= 0) && (!XEN_KEYWORD_P(keys[4])))
	XEN_ERROR(CANNOT_SAVE,
		  XEN_LIST_2(C_TO_XEN_STRING(S_save_sound_as ": srate (~A) can't be <= 0"),
			     C_TO_XEN_INT(sr)));

      chan = mus_optkey_to_int(keys[5], S_save_sound_as, orig_arg[5], chan);
      if (!(XEN_KEYWORD_P(keys[6]))) 
	{
	  edpos = keys[6];
	  if ((XEN_INTEGER_P(edpos)) || (XEN_PROCEDURE_P(edpos)))
	    got_edpos = true;
	}
      outcom = mus_optkey_to_string(keys[7], S_save_sound_as, orig_arg[7], NULL);
    }

  if ((file == NULL) || 
      (directory_p(file)))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_1(C_TO_XEN_STRING(S_save_sound_as ": no output file?")));

  ASSERT_SOUND(S_save_sound_as, index, 2);

  sp = get_sp(index);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_save_sound_as, index));
  hdr = sp->hdr;

  if (ht == -1) ht = hdr->type;
  if (!(mus_header_writable(ht, -2)))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_save_sound_as ": can't write ~A headers"),
			 C_TO_XEN_STRING(mus_header_type_name(ht))));

  if (sr == -1) 
    sr = hdr->srate;

  if (df == -1) 
    {
      /* try to find some writable data_format */
      df = hdr->format;
      if (!mus_header_writable(ht, df)) 
	df = MUS_OUT_FORMAT;
      if (!mus_header_writable(ht, df))
	{
	  switch (df)
	    {
	    case MUS_BFLOAT:  df = MUS_LFLOAT;  break;
	    case MUS_BDOUBLE: df = MUS_LDOUBLE; break;
	    case MUS_BINT:    df = MUS_LINT;    break;
	    case MUS_LFLOAT:  df = MUS_BFLOAT;  break;
	    case MUS_LDOUBLE: df = MUS_BDOUBLE; break;
	    case MUS_LINT:    df = MUS_BINT;    break;
	    }
	  if (!mus_header_writable(ht, df))
	    {
	      int i;
	      for (i = 1; i < MUS_NUM_DATA_FORMATS; i++) /* MUS_UNSUPPORTED is 0 */
		{
		  df = i;
		  if (mus_header_writable(ht, df))
		    break;
		}
	    }
	}
    }

  if (!mus_header_writable(ht, df))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_sound_as ": can't write ~A data to ~A headers"),
			 C_TO_XEN_STRING(mus_data_format_name(df)),
			 C_TO_XEN_STRING(mus_header_type_name(ht))));

  if (chan >= sp->nchans)
    return(snd_no_such_channel_error(S_save_sound_as, index, keys[5]));

  if (got_edpos)
    {
      edit_position = to_c_edit_position(sp->chans[(chan >= 0) ? chan : 0], edpos, S_save_sound_as, 7);
      for (i = 0; i < sp->nchans; i++)
	if (edit_position > sp->chans[i]->edit_ctr)
	  XEN_ERROR(NO_SUCH_EDIT,
		    XEN_LIST_5(C_TO_XEN_STRING(S_save_sound_as ": no such edit position: ~A (~S chan ~A has ~A edits)"),
			       C_TO_XEN_INT(edit_position),
			       C_TO_XEN_STRING(sp->short_filename),
			       C_TO_XEN_INT(i),
			       C_TO_XEN_INT(sp->chans[i]->edit_ctr)));
    }

  fname = mus_expand_filename(file);
  if (outcom == NULL) 
    {
      outcom = output_comment(hdr);
      if (outcom) free_outcom = true;
    }

  if (!(run_before_save_as_hook(sp, fname, false, sr, ht, df, outcom)))
    {
      if (chan >= 0)
	io_err = channel_to_file_with_settings(sp->chans[chan], fname, ht, df, sr, outcom, edit_position);
      else io_err = save_edits_without_display(sp, fname, ht, df, sr, outcom, edit_position);
    }

  if (free_outcom) 
    {
      free((char *)outcom); 
      outcom = NULL;
    }

  if (io_err == IO_NO_ERROR) 
    run_after_save_as_hook(sp, fname, false); /* true => from dialog */
  else
    {
      if (io_err != IO_SAVE_HOOK_CANCELLATION)
	{
	  XEN errstr;
	  errstr = C_TO_XEN_STRING(fname);
	  if (fname) {free(fname); fname = NULL;}
	  XEN_ERROR(CANNOT_SAVE,
		    XEN_LIST_3(C_TO_XEN_STRING(S_save_sound_as ": ~A (~A)"),
			       errstr,
			       C_TO_XEN_STRING(snd_open_strerror())));
	}
    }

  if (fname) free(fname);
  return(args[orig_arg[0] - 1]);
}


static XEN g_new_sound(XEN arglist)
{
  #if HAVE_SCHEME
    #define new_sound_example "(" S_new_sound " \"test.snd\" " S_mus_next " " S_mus_bshort " 22050 1 \"no comment\" 1000)"
  #endif
  #if HAVE_RUBY
    #define new_sound_example "new_sound(\"test.snd\", Mus_next, Mus_bshort, 22050, 1, \"no comment\", 1000)"
  #endif
  #if HAVE_FORTH
    #define new_sound_example "\"test.snd\" mus-next mus-bshort 22050 1 \"no comment\" 1000 new-sound"
  #endif

  #define H_new_sound "(" S_new_sound " file header-type data-format srate channels comment size): \
creates a new sound file with the indicated attributes; if any are omitted, the corresponding default-output variable is used. \
The 'size' argument sets the number of samples (zeros) in the newly created sound. \n  " new_sound_example

  snd_info *sp = NULL; 
  int ht, df, sr, ch;
  int chan;
  mus_long_t size, len = 1;
  char *str = NULL;
  const char *com = NULL, *file = NULL;
  XEN args[14]; 
  XEN keys[7];
  int orig_arg[7] = {0, 0, 0, 0, 0, 0, 0};
  int vals, i, arglist_len;
  io_error_t io_err;

  keys[0] = kw_file;
  keys[1] = kw_header_type;
  keys[2] = kw_data_format;
  keys[3] = kw_srate;
  keys[4] = kw_channels;
  keys[5] = kw_comment;
  keys[6] = kw_size;

  for (i = 0; i < 14; i++) args[i] = XEN_UNDEFINED;
  arglist_len = XEN_LIST_LENGTH(arglist);
  for (i = 0; i < arglist_len; i++) args[i] = XEN_LIST_REF(arglist, i);
  vals = mus_optkey_unscramble(S_new_sound, 7, keys, args, orig_arg);

  ht = default_output_header_type(ss);
  df = default_output_data_format(ss);
  sr = default_output_srate(ss);
  ch = default_output_chans(ss);

  if (vals > 0)
    {
      file = mus_optkey_to_string(keys[0], S_new_sound, orig_arg[0], NULL);
      /* this can be null if :file is not passed as an arg (use temp name below) */
      ht = mus_optkey_to_int(keys[1], S_new_sound, orig_arg[1], ht);
      df = mus_optkey_to_int(keys[2], S_new_sound, orig_arg[2], df);
      sr = mus_optkey_to_int(keys[3], S_new_sound, orig_arg[3], sr);
      ch = mus_optkey_to_int(keys[4], S_new_sound, orig_arg[4], ch);
      com = mus_optkey_to_string(keys[5], S_new_sound, orig_arg[5], NULL);
      len = mus_optkey_to_mus_long_t(keys[6], S_new_sound, orig_arg[6], len);
    }

  if (!(mus_header_type_p(ht)))
    XEN_OUT_OF_RANGE_ERROR(S_new_sound, orig_arg[1], keys[1], "invalid header type");

  if (!(mus_data_format_p(df)))
    XEN_OUT_OF_RANGE_ERROR(S_new_sound, orig_arg[2], keys[2], "invalid data format");

  if (!(mus_header_writable(ht, df)))
    XEN_ERROR(BAD_HEADER,
	      XEN_LIST_3(C_TO_XEN_STRING(S_new_sound ": can't write ~A data to a ~A header"),
			 keys[2], 
			 keys[1]));

  if (sr <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_new_sound, orig_arg[3], keys[3], "srate <= 0?");

  if (ch <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_new_sound, orig_arg[4], keys[4], "channels <= 0?");

  if (len < 0)
    XEN_OUT_OF_RANGE_ERROR(S_new_sound, orig_arg[6], keys[6], "size < 0?");

  if (file)
    {
      str = mus_expand_filename(file);
      if (!str)
	XEN_OUT_OF_RANGE_ERROR(S_new_sound, orig_arg[0], keys[0], "bad file name?");
    }
  else str = snd_tempnam();
  mus_sound_forget(str);

  io_err = snd_write_header(str, ht, sr, ch, len * ch, df, com, NULL); /* last arg is loop info */
  if (io_err != IO_NO_ERROR)
    {
      if (str) {free(str); str = NULL;}
      XEN_ERROR(XEN_ERROR_TYPE("IO-error"),
		XEN_LIST_3(C_TO_XEN_STRING(S_new_sound ": ~S, ~A"),
			   keys[0],
			   C_TO_XEN_STRING(snd_io_strerror())));
    }

  chan = snd_reopen_write(str);
  lseek(chan, mus_header_data_location(), SEEK_SET);

  size = ch * mus_samples_to_bytes(df, len);
  if (size > 0)
    {
      unsigned char *buf;
      buf = (unsigned char *)calloc(size, sizeof(unsigned char));
      if (write(chan, buf, size) != size) fprintf(stderr, "new-sound %s write error", str);
      free(buf);
    }

  snd_close(chan, str);
  ss->open_requestor = FROM_NEW_SOUND;

  sp = sound_is_silence(snd_open_file(str, FILE_READ_WRITE));

  if (str) free(str);
  if (sp) return(C_INT_TO_XEN_SOUND(sp->index));
  return(XEN_FALSE);
}


static XEN g_speed_control_style(XEN snd)
{
  #define H_speed_control_style "(" S_speed_control_style " :optional snd): speed control panel interpretation \
choice: " S_speed_control_as_float ", " S_speed_control_as_ratio ", or " S_speed_control_as_semitone "."

  return(sound_get_global(snd, SP_SPEED_STYLE, S_speed_control_style));
}


static XEN g_set_speed_control_style(XEN speed, XEN snd) 
{
  int in_spd;
  speed_style_t spd;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(speed), speed, XEN_ARG_1, S_setB S_speed_control_style, "an integer"); 

  in_spd = XEN_TO_C_INT(speed);
  if (in_spd < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_speed_control_style, 1, speed, "invalid " S_speed_control_style);

  spd = (speed_style_t)in_spd;
  if (spd >= NUM_SPEED_CONTROL_STYLES)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_speed_control_style, 1, speed, 
			   S_speed_control_style " should be " S_speed_control_as_float ", " S_speed_control_as_ratio ", or " S_speed_control_as_semitone);

  return(sound_set_global(snd, speed, SP_SPEED_STYLE, S_setB S_speed_control_style));
}

WITH_TWO_SETTER_ARGS(g_set_speed_control_style_reversed, g_set_speed_control_style)


static XEN g_speed_control_tones(XEN snd)
{
  #define H_speed_control_tones "(" S_speed_control_tones " :optional snd): if " S_speed_control_style " is " S_speed_control_as_semitone ", this chooses the octave divisions (12)"
  return(sound_get_global(snd, SP_SPEED_TONES, S_speed_control_tones));
}


static XEN g_set_speed_control_tones(XEN val, XEN snd)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_speed_control_tones, "a number"); 
  return(sound_set_global(snd, val, SP_SPEED_TONES, S_setB S_speed_control_tones));
}

WITH_TWO_SETTER_ARGS(g_set_speed_control_tones_reversed, g_set_speed_control_tones)


static XEN g_amp_control(XEN snd, XEN chn_n) 
{
  #define H_amp_control "(" S_amp_control " :optional snd chn): current amp slider setting"
  if (XEN_BOUND_P(chn_n))
    {
      chan_info *cp;
      ASSERT_CHANNEL(S_amp_control, snd, chn_n, 1);
      cp = get_cp(snd, chn_n, S_amp_control);
      if (!cp) return(XEN_FALSE);
      if (cp->amp_control)
	return(C_TO_XEN_DOUBLE(cp->amp_control[0]));
    }
  return(sound_get(snd, SP_AMP, S_amp_control));
}


static XEN g_set_amp_control(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_amp_control, "a number"); 

  if (XEN_BOUND_P(chn_n))
    {
      chan_info *cp;
      ASSERT_CHANNEL(S_amp_control, snd, chn_n, 2);
      cp = get_cp(snd, chn_n, S_amp_control);
      if (!cp) return(XEN_FALSE);
      if (cp->amp_control == NULL)
	cp->amp_control = (mus_float_t *)calloc(1, sizeof(mus_float_t));
      cp->amp_control[0] = (mus_float_t)XEN_TO_C_DOUBLE(on);
      return(on);
    }

  return(sound_set(snd, on, SP_AMP, S_setB S_amp_control));
}

WITH_THREE_SETTER_ARGS(g_set_amp_control_reversed, g_set_amp_control)


static XEN g_amp_control_bounds(XEN snd) 
{
  #define H_amp_control_bounds "(" S_amp_control_bounds " :optional snd): current amp slider bounds (default: '(0.0 8.0))"
  return(sound_get_global(snd, SP_AMP_BOUNDS, S_amp_control_bounds));
}


static XEN g_set_amp_control_bounds(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_amp_control_bounds, "a list of the new min and max values"); 

  if ((XEN_LIST_LENGTH(on) != 2) ||
      (!(XEN_NUMBER_P(XEN_CAR(on)))) ||
      (!(XEN_NUMBER_P(XEN_CADR(on)))))
    XEN_WRONG_TYPE_ARG_ERROR(S_setB S_amp_control_bounds, XEN_ARG_1, on, "a list of 2 numbers");

  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) >= XEN_TO_C_DOUBLE(XEN_CADR(on)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_amp_control_bounds, 1, on, "min >= max");

  return(sound_set_global(snd, on, SP_AMP_BOUNDS, S_setB S_amp_control_bounds));
}

WITH_TWO_SETTER_ARGS(g_set_amp_control_bounds_reversed, g_set_amp_control_bounds)


static XEN g_contrast_control(XEN snd) 
{
  #define H_contrast_control "(" S_contrast_control " :optional snd): current contrast slider setting"
  return(sound_get(snd, SP_CONTRAST, S_contrast_control));
}


static XEN g_set_contrast_control(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_contrast_control, "a number"); 
  return(sound_set(snd, on, SP_CONTRAST, S_setB S_contrast_control));
}

WITH_TWO_SETTER_ARGS(g_set_contrast_control_reversed, g_set_contrast_control)


static XEN g_contrast_control_bounds(XEN snd) 
{
  #define H_contrast_control_bounds "(" S_contrast_control_bounds " :optional snd): current contrast slider bounds (default: '(0.0 10.0))"
  return(sound_get_global(snd, SP_CONTRAST_BOUNDS, S_contrast_control_bounds));
}


static XEN g_set_contrast_control_bounds(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_contrast_control_bounds, "a list of the new min and max values"); 

  if ((XEN_LIST_LENGTH(on) != 2) ||
      (!(XEN_NUMBER_P(XEN_CAR(on)))) ||
      (!(XEN_NUMBER_P(XEN_CADR(on)))))
    XEN_WRONG_TYPE_ARG_ERROR(S_setB S_contrast_control_bounds, XEN_ARG_1, on, "a list of 2 numbers");

  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) >= XEN_TO_C_DOUBLE(XEN_CADR(on)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_contrast_control_bounds, 1, on, "min >= max");

  return(sound_set_global(snd, on, SP_CONTRAST_BOUNDS, S_setB S_contrast_control_bounds));
}

WITH_TWO_SETTER_ARGS(g_set_contrast_control_bounds_reversed, g_set_contrast_control_bounds)


static XEN g_contrast_control_amp(XEN snd) 
{
  #define H_contrast_control_amp "(" S_contrast_control_amp " :optional snd): snd's contrast amp\n\
   (scaler on data before contrast operation in control panel, 1.0)"

  return(sound_get_global(snd, SP_CONTRAST_AMP, S_contrast_control_amp));
}


static XEN g_set_contrast_control_amp(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_contrast_control_amp, "a number");
  return(sound_set_global(snd, on, SP_CONTRAST_AMP, S_setB S_contrast_control_amp));
}

WITH_TWO_SETTER_ARGS(g_set_contrast_control_amp_reversed, g_set_contrast_control_amp)


static XEN g_expand_control(XEN snd) 
{
  #define H_expand_control "(" S_expand_control " :optional snd): current expand slider setting"
  return(sound_get(snd, SP_EXPAND, S_expand_control));
}


static XEN g_set_expand_control(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control, "a number"); 
  return(sound_set(snd, on, SP_EXPAND, S_setB S_expand_control));
}

WITH_TWO_SETTER_ARGS(g_set_expand_control_reversed, g_set_expand_control)


static XEN g_expand_control_bounds(XEN snd) 
{
  #define H_expand_control_bounds "(" S_expand_control_bounds " :optional snd): current expand slider bounds (default: '(0.001 20.0))"
  return(sound_get_global(snd, SP_EXPAND_BOUNDS, S_expand_control_bounds));
}


static XEN g_set_expand_control_bounds(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_expand_control_bounds, "a list of the new min and max values"); 

  if ((XEN_LIST_LENGTH(on) != 2) ||
      (!(XEN_NUMBER_P(XEN_CAR(on)))) ||
      (!(XEN_NUMBER_P(XEN_CADR(on)))))
    XEN_WRONG_TYPE_ARG_ERROR(S_setB S_expand_control_bounds, XEN_ARG_1, on, "a list of 2 numbers");

  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) >= XEN_TO_C_DOUBLE(XEN_CADR(on)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_expand_control_bounds, 1, on, "min >= max");

  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) <= 0.0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_expand_control_bounds, 1, on, "min <= 0.0");

  return(sound_set_global(snd, on, SP_EXPAND_BOUNDS, S_setB S_expand_control_bounds));
}

WITH_TWO_SETTER_ARGS(g_set_expand_control_bounds_reversed, g_set_expand_control_bounds)


static XEN g_expand_control_length(XEN snd) 
{
  #define H_expand_control_length "(" S_expand_control_length " :optional snd): current expansion segment length in seconds (.15)"
  return(sound_get_global(snd, SP_EXPAND_LENGTH, S_expand_control_length));
}


static XEN g_set_expand_control_length(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control_length, "a number"); 
  return(sound_set_global(snd, on, SP_EXPAND_LENGTH, S_setB S_expand_control_length));
}

WITH_TWO_SETTER_ARGS(g_set_expand_control_length_reversed, g_set_expand_control_length)


static XEN g_expand_control_ramp(XEN snd) 
{
  #define H_expand_control_ramp "(" S_expand_control_ramp " :optional snd): current expansion ramp time (.4)"
  return(sound_get_global(snd, SP_EXPAND_RAMP, S_expand_control_ramp));
}


static XEN g_set_expand_control_ramp(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control_ramp, "a number");
  return(sound_set_global(snd, on, SP_EXPAND_RAMP, S_setB S_expand_control_ramp));
}

WITH_TWO_SETTER_ARGS(g_set_expand_control_ramp_reversed, g_set_expand_control_ramp)


static XEN g_expand_control_hop(XEN snd) 
{
  #define H_expand_control_hop "(" S_expand_control_hop " :optional snd): current expansion output grain spacing in seconds (0.05)"
  return(sound_get_global(snd, SP_EXPAND_HOP, S_expand_control_hop));
}


static XEN g_set_expand_control_hop(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control_hop, "a number"); 
  return(sound_set_global(snd, on, SP_EXPAND_HOP, S_setB S_expand_control_hop));
}

WITH_TWO_SETTER_ARGS(g_set_expand_control_hop_reversed, g_set_expand_control_hop)


static XEN g_expand_control_jitter(XEN snd) 
{
  #define H_expand_control_jitter "(" S_expand_control_jitter " :optional snd): current expansion output grain spacing jitter (0.1)"
  return(sound_get_global(snd, SP_EXPAND_JITTER, S_expand_control_jitter));
}


static XEN g_set_expand_control_jitter(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_expand_control_jitter, "a number"); 
  return(sound_set_global(snd, on, SP_EXPAND_JITTER, S_setB S_expand_control_jitter));
}

WITH_TWO_SETTER_ARGS(g_set_expand_control_jitter_reversed, g_set_expand_control_jitter)


static XEN g_speed_control(XEN snd) 
{
  #define H_speed_control "(" S_speed_control " :optional snd): current speed (srate) slider setting"
  return(sound_get(snd, SP_SPEED, S_speed_control));
}


static XEN g_set_speed_control(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_speed_control, "a number"); 
  return(sound_set(snd, on, SP_SPEED, S_setB S_speed_control));
}

WITH_TWO_SETTER_ARGS(g_set_speed_control_reversed, g_set_speed_control)


static XEN g_speed_control_bounds(XEN snd) 
{
  #define H_speed_control_bounds "(" S_speed_control_bounds " :optional snd): current speed slider bounds (default: '(0.05 20.0))"
  return(sound_get_global(snd, SP_SPEED_BOUNDS, S_speed_control_bounds));
}


static XEN g_set_speed_control_bounds(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_speed_control_bounds, "a list of the new min and max values"); 

  if ((XEN_LIST_LENGTH(on) != 2) ||
      (!(XEN_NUMBER_P(XEN_CAR(on)))) ||
      (!(XEN_NUMBER_P(XEN_CADR(on)))))
    XEN_WRONG_TYPE_ARG_ERROR(S_setB S_speed_control_bounds, XEN_ARG_1, on, "a list of 2 numbers");

  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) >= XEN_TO_C_DOUBLE(XEN_CADR(on)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_speed_control_bounds, 1, on, "min >= max");

  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) <= 0.0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_speed_control_bounds, 1, on, "min <= 0.0");

  return(sound_set_global(snd, on, SP_SPEED_BOUNDS, S_setB S_speed_control_bounds));
}

WITH_TWO_SETTER_ARGS(g_set_speed_control_bounds_reversed, g_set_speed_control_bounds)


static XEN g_reverb_control_length(XEN snd) 
{
  #define H_reverb_control_length "(" S_reverb_control_length " :optional snd): reverb decay length scaler"
  return(sound_get(snd, SP_REVERB_LENGTH, S_reverb_control_length));
}


static XEN g_set_reverb_control_length(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_reverb_control_length, "a number"); 
  return(sound_set(snd, on, SP_REVERB_LENGTH, S_setB S_reverb_control_length));
}

WITH_TWO_SETTER_ARGS(g_set_reverb_control_length_reversed, g_set_reverb_control_length)


static XEN g_reverb_control_length_bounds(XEN snd) 
{
  #define H_reverb_control_length_bounds "(" S_reverb_control_length_bounds " :optional snd): current reverb length slider bounds (default: '(0.0 5.0))"
  return(sound_get_global(snd, SP_REVERB_LENGTH_BOUNDS, S_reverb_control_length_bounds));
}


static XEN g_set_reverb_control_length_bounds(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_reverb_control_length_bounds, "a list of the new min and max values"); 

  if ((XEN_LIST_LENGTH(on) != 2) ||
      (!(XEN_NUMBER_P(XEN_CAR(on)))) ||
      (!(XEN_NUMBER_P(XEN_CADR(on)))))
    XEN_WRONG_TYPE_ARG_ERROR(S_setB S_reverb_control_length_bounds, XEN_ARG_1, on, "a list of 2 numbers");

  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) >= XEN_TO_C_DOUBLE(XEN_CADR(on)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_reverb_control_length_bounds, 1, on, "min >= max");

  return(sound_set_global(snd, on, SP_REVERB_LENGTH_BOUNDS, S_setB S_reverb_control_length_bounds));
}

WITH_TWO_SETTER_ARGS(g_set_reverb_control_length_bounds_reversed, g_set_reverb_control_length_bounds)


static XEN g_reverb_control_feedback(XEN snd) 
{
  #define H_reverb_control_feedback "(" S_reverb_control_feedback " :optional snd): reverb feedback scaler"
  return(sound_get_global(snd, SP_REVERB_FEEDBACK, S_reverb_control_feedback));
}


static XEN g_set_reverb_control_feedback(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_reverb_control_feedback, "a number"); 
  return(sound_set_global(snd, on, SP_REVERB_FEEDBACK, S_setB S_reverb_control_feedback));
}

WITH_TWO_SETTER_ARGS(g_set_reverb_control_feedback_reversed, g_set_reverb_control_feedback)


static XEN g_reverb_control_scale(XEN snd) 
{
  #define H_reverb_control_scale "(" S_reverb_control_scale " :optional snd): reverb scaler (the amount of reverb)"
  return(sound_get(snd, SP_REVERB_SCALE, S_reverb_control_scale));
}


static XEN g_set_reverb_control_scale(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_reverb_control_scale, "a number"); 
  return(sound_set(snd, on, SP_REVERB_SCALE, S_setB S_reverb_control_scale));
}

WITH_TWO_SETTER_ARGS(g_set_reverb_control_scale_reversed, g_set_reverb_control_scale)


static XEN g_reverb_control_scale_bounds(XEN snd) 
{
  #define H_reverb_control_scale_bounds "(" S_reverb_control_scale_bounds " :optional snd): current reverb scale slider bounds (default: '(0.0 4.0))"
  return(sound_get_global(snd, SP_REVERB_SCALE_BOUNDS, S_reverb_control_scale_bounds));
}


static XEN g_set_reverb_control_scale_bounds(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_reverb_control_scale_bounds, "a list of the new min and max values"); 

  if ((XEN_LIST_LENGTH(on) != 2) ||
      (!(XEN_NUMBER_P(XEN_CAR(on)))) ||
      (!(XEN_NUMBER_P(XEN_CADR(on)))))
    XEN_WRONG_TYPE_ARG_ERROR(S_setB S_reverb_control_scale_bounds, XEN_ARG_1, on, "a list of 2 numbers");

  if (XEN_TO_C_DOUBLE(XEN_CAR(on)) >= XEN_TO_C_DOUBLE(XEN_CADR(on)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_reverb_control_scale_bounds, 1, on, "min >= max");

  return(sound_set_global(snd, on, SP_REVERB_SCALE_BOUNDS, S_setB S_reverb_control_scale_bounds));
}

WITH_TWO_SETTER_ARGS(g_set_reverb_control_scale_bounds_reversed, g_set_reverb_control_scale_bounds)


static XEN g_reverb_control_lowpass(XEN snd) 
{
  #define H_reverb_control_lowpass "(" S_reverb_control_lowpass " :optional snd): reverb lowpass filter coefficient"
  return(sound_get_global(snd, SP_REVERB_LOW_PASS, S_reverb_control_lowpass));
}


static XEN g_set_reverb_control_lowpass(XEN on, XEN snd) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_reverb_control_lowpass, "a number"); 
  return(sound_set_global(snd, on, SP_REVERB_LOW_PASS, S_setB S_reverb_control_lowpass));
}

WITH_TWO_SETTER_ARGS(g_set_reverb_control_lowpass_reversed, g_set_reverb_control_lowpass)


static XEN g_reverb_control_decay(XEN snd)
{
  #define H_reverb_control_decay "(" S_reverb_control_decay " :optional snd): " S_apply_controls " reverb decay time (1.0 seconds)"
  return(sound_get_global(snd, SP_REVERB_DECAY, S_reverb_control_decay));
}


static XEN g_set_reverb_control_decay(XEN val, XEN snd)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_reverb_control_decay, "a number"); 
  return(sound_set_global(snd, val, SP_REVERB_DECAY, S_setB S_reverb_control_decay));
}

WITH_TWO_SETTER_ARGS(g_set_reverb_control_decay_reversed, g_set_reverb_control_decay)


static XEN g_filter_control_envelope(XEN snd)
{
  #define H_filter_control_envelope "(" S_filter_control_envelope " :optional snd): snd's filter envelope (in the control panel)"
  return(sound_get(snd, SP_FILTER_ENVELOPE, S_filter_control_envelope));
}


static XEN g_set_filter_control_envelope(XEN val, XEN snd)
{
  return(sound_set(snd, val, SP_FILTER_ENVELOPE, S_setB S_filter_control_envelope));
}

WITH_TWO_SETTER_ARGS(g_set_filter_control_envelope_reversed, g_set_filter_control_envelope)


static void squelch_printout(const char *msg, void *ignore)
{
}


static void apply_controls_error(const char *msg, void *data)
{
  redirect_snd_warning_to(NULL, NULL);
  redirect_snd_error_to(NULL, NULL);
  XEN_ERROR(XEN_ERROR_TYPE("cannot-apply-controls"),
	    XEN_LIST_3(C_TO_XEN_STRING("~A: ~A"),
		       C_TO_XEN_STRING((char *)data),
		       C_TO_XEN_STRING(msg)));
}


static XEN g_controls_to_channel(XEN settings, XEN beg, XEN dur, XEN snd, XEN chn, XEN origin)
{
  #define H_controls_to_channel "(" S_controls_to_channel " settings :optional beg dur snd chn origin) sets up \
snd's controls to reflect 'settings' (unspecified settings are not changed), then applies the controls as \
an edit of channel 'chn'. The 'settings' argument is a list:\n\
\n\
  (list amp speed\n\
    (list contrast contrast_amp)\n\
    (list expand expand_length expand_ramp expand_hop expand_jitter)\n\
    (list reverb_scale reverb_length reverb_feedback reverb_low_pass reverb_decay)\n\
    (list filter_order filter_env))\n\
\n\
where each inner list entry can also be " PROC_FALSE "."

  snd_info *sp;
  chan_info *cp;

  XEN_ASSERT_TYPE(XEN_LIST_P(settings), settings, XEN_ARG_1, S_controls_to_channel, "a list");
  ASSERT_CHANNEL(S_controls_to_channel, snd, chn, 4);
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(beg) || XEN_FALSE_P(beg) || XEN_NOT_BOUND_P(beg), beg, XEN_ARG_2, S_controls_to_channel, "an integer");
  XEN_ASSERT_TYPE(XEN_LONG_LONG_P(dur) || XEN_FALSE_P(dur) || XEN_NOT_BOUND_P(dur), dur, XEN_ARG_3, S_controls_to_channel, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_7, S_controls_to_channel, "a string");

  sp = get_sp(snd); /* control changes make sense, but not 'apply' -- expecting just 'play' if a player */
  if (sp)
    {
      XEN lst;
      apply_state *ap;
      int old_selected_channel;
      ctrl_state *saved_settings;
      if (sp->applying)
	{
	  XEN_ERROR(XEN_ERROR_TYPE("cannot-apply-controls"),
		    XEN_LIST_1(C_TO_XEN_STRING(S_controls_to_channel ": already applying controls")));
	}
      if (XEN_LONG_LONG_P(beg)) apply_beg = XEN_TO_C_LONG_LONG(beg); else apply_beg = 0;
      if (XEN_LONG_LONG_P(dur)) apply_dur = XEN_TO_C_LONG_LONG(dur); else apply_dur = 0;
      cp = get_cp(snd, chn, S_controls_to_channel);
      old_selected_channel = sp->selected_channel;
      sp->selected_channel = cp->chan;
      saved_settings = current_control_settings(sp, NULL);

      /* now read the 'settings' list for any new settings */
      if ((XEN_LIST_P(settings)) && (XEN_NOT_NULL_P(settings)))
	{
	  int i, len, elen;
	  /* settings: 
	     (list amp speed
	       (list contrast contrast_amp)
	       (list expand expand_length expand_ramp expand_hop expand_jitter)
	       (list reverb_scale reverb_length reverb_feedback reverb_low_pass reverb_decay)
	       (list filter_order filter_env))
	     where any (outer) items can be #f
	  */
	  len = XEN_LIST_LENGTH(settings);
	  for (i = 0, lst = XEN_COPY_ARG(settings); i < len; i++, lst = XEN_CDR(lst))
	    {
	      XEN element;
	      element = XEN_CAR(lst);
	      switch (i)
		{
		case 0: 
		  if (XEN_NUMBER_P(element)) sp->amp_control = XEN_TO_C_DOUBLE(element);
		  break;

		case 1:
		  if (XEN_NUMBER_P(element)) sp->speed_control = XEN_TO_C_DOUBLE(element);
		  break;

		case 2:
		  if (XEN_LIST_P(element))
		    {
		      elen = XEN_LIST_LENGTH(element);
		      if (elen > 0) sp->contrast_control_p = true;
		      if (elen > 0) sp->contrast_control = XEN_TO_C_DOUBLE(XEN_CAR(element));
		      if (elen > 1) sp->contrast_control_amp = XEN_TO_C_DOUBLE(XEN_CADR(element));
		    }
		  break;

		case 3:
		  if (XEN_LIST_P(element))
		    {
		      elen = XEN_LIST_LENGTH(element);
		      if (elen > 0) sp->expand_control_p = true;
		      if (elen > 0) sp->expand_control = XEN_TO_C_DOUBLE(XEN_CAR(element));
		      if (elen > 1) sp->expand_control_length = XEN_TO_C_DOUBLE(XEN_CADR(element));
		      if (elen > 2) sp->expand_control_ramp = XEN_TO_C_DOUBLE(XEN_CADDR(element));
		      if (elen > 3) sp->expand_control_hop = XEN_TO_C_DOUBLE(XEN_LIST_REF(element, 3));
		      if (elen > 4) sp->expand_control_jitter = XEN_TO_C_DOUBLE(XEN_LIST_REF(element, 4));
		    }
		  break;

		case 4:
		  if (XEN_LIST_P(element))
		    {
		      elen = XEN_LIST_LENGTH(element);
		      if (elen > 0) sp->reverb_control_p = true;
		      if (elen > 0) sp->reverb_control_scale = XEN_TO_C_DOUBLE(XEN_CAR(element));
		      if (elen > 1) sp->reverb_control_length = XEN_TO_C_DOUBLE(XEN_CADR(element));
		      if (elen > 2) sp->reverb_control_feedback = XEN_TO_C_DOUBLE(XEN_CADDR(element));
		      if (elen > 3) sp->reverb_control_lowpass = XEN_TO_C_DOUBLE(XEN_LIST_REF(element, 3));
		      if (elen > 4) sp->reverb_control_decay = XEN_TO_C_DOUBLE(XEN_LIST_REF(element, 4));
		    }
		  break;

		case 5:
		  if (XEN_LIST_P(element))
		    {
		      elen = XEN_LIST_LENGTH(element);
		      if (elen > 0) sp->filter_control_p = true;
		      if (elen > 0) sp->filter_control_order = XEN_TO_C_INT(XEN_CAR(element));
		      if (elen > 1) sp->filter_control_envelope = get_env(XEN_CADR(element), S_controls_to_channel);
		    }
		}
	    }
	}

      ss->apply_choice = APPLY_TO_CHANNEL;
      sp->applying = true;
      ap = (apply_state *)make_apply_state(sp);

#if HAVE_EXTENSION_LANGUAGE
#if HAVE_FORTH
      if (!(XEN_NUMBER_P(dur)))
	ap->origin = mus_format("%s %lld" PROC_SEP PROC_FALSE " %s", 
				XEN_AS_STRING(settings), 
				apply_beg, S_controls_to_channel);
      else ap->origin = mus_format("%s " PROC_SEP "%lld" PROC_SEP "%lld %s", 
				   XEN_AS_STRING(settings), 
				   apply_beg, apply_dur, S_controls_to_channel);
#else
      {
	char *temp = NULL;
	if (!(XEN_NUMBER_P(dur)))
	  ap->origin = mus_format("%s" PROC_OPEN "%s%s" PROC_SEP "%lld" PROC_SEP PROC_FALSE, 
				  TO_PROC_NAME(S_controls_to_channel), 
				  PROC_QUOTE,
				  temp = XEN_AS_STRING(settings), 
				  apply_beg);
	else ap->origin = mus_format("%s" PROC_OPEN "%s%s" PROC_SEP "%lld" PROC_SEP "%lld", 
				     TO_PROC_NAME(S_controls_to_channel), 
				     PROC_QUOTE,
				     temp = XEN_AS_STRING(settings), 
				     apply_beg, apply_dur);
#if HAVE_SCHEME
	if (temp) free(temp);
#endif
      }
#endif
#endif

      if (ap)
	{
	  redirect_snd_error_to(apply_controls_error, (void *)S_controls_to_channel);
	  redirect_snd_warning_to(squelch_printout, NULL);
	  while (apply_controls(ap)) {};
	  redirect_snd_warning_to(NULL, NULL); /* no-op message pointless within xen */
	  redirect_snd_error_to(NULL, NULL);
	}
      sp->selected_channel = old_selected_channel;
      restore_control_settings(sp, saved_settings);
      free_control_settings(saved_settings);
    }

  return(settings);
}


static XEN g_apply_controls(XEN snd, XEN choice, XEN beg, XEN dur)
{
  #define H_apply_controls "(" S_apply_controls " :optional snd (choice 0) (beg 0) (dur len)): \
applies the current control panel state as an edit. \
The 'choices' are 0 (apply to sound), 1 (apply to channel), and 2 (apply to selection).  If 'beg' is given, the apply starts there."

  snd_info *sp;

  ASSERT_SOUND(S_apply_controls, snd, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(choice), choice, XEN_ARG_2, S_apply_controls, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(beg), beg, XEN_ARG_3, S_apply_controls, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(dur), dur, XEN_ARG_4, S_apply_controls, "an integer");

  sp = get_sp(snd); /* control changes make sense, but not 'apply' -- expecting just 'play' if a player */
  if (sp)
    {
      apply_state *ap;
      snd_apply_t cur_choice;

      if (sp->applying)
	{
	  XEN_ERROR(XEN_ERROR_TYPE("cannot-apply-controls"),
		    XEN_LIST_1(C_TO_XEN_STRING(S_apply_controls ": already applying controls")));
	}

      if (XEN_LONG_LONG_P(beg)) apply_beg = XEN_TO_C_LONG_LONG(beg); else apply_beg = 0;
      if (XEN_LONG_LONG_P(dur)) apply_dur = XEN_TO_C_LONG_LONG(dur); else apply_dur = 0;
      cur_choice = (snd_apply_t)XEN_TO_C_INT_OR_ELSE(choice, (int)APPLY_TO_SOUND);

      if (cur_choice > APPLY_TO_SELECTION)
	XEN_OUT_OF_RANGE_ERROR(S_apply_controls, 2, choice, "choice must be 0=sound, 1=channel, or 2=selection");
      ss->apply_choice = cur_choice;
      sp->applying = true;
      ap = (apply_state *)make_apply_state(sp);

      if (ap)
	{
	  redirect_snd_error_to(apply_controls_error, (void *)S_apply_controls);
	  redirect_snd_warning_to(squelch_printout, NULL);
	  while (apply_controls(ap)) {};
	  redirect_snd_warning_to(NULL, NULL); /* no-op message pointless within xen */
	  redirect_snd_error_to(NULL, NULL);
	}
      return(snd);
    }
  return(snd_no_such_sound_error(S_apply_controls, snd));
}


/* ---------------------------------------- peak env files ---------------------------------------- */

static int pack_env_info_type(void)
{
  /* put data description in peak-env info file (in case user opens it from incompatible machine) */
  int val = 0;
#if MUS_LITTLE_ENDIAN
  val |= (1 << 8);
#endif
  val |= (1 << 9); /* always float now */
  val |= (sizeof(mus_float_t) << 10);
  return(val);
}


static char *peak_clean(const char *name)
{
  int len, i;
  char *peak_name;
  len = mus_strlen(name);
  peak_name = (char *)calloc(len + 1, sizeof(char));
  for (i = 0; i < len; i++)
    {
      if ((name[i] == '\\') ||
	  (name[i] == '/'))
	peak_name[i] = '_';
      else peak_name[i] = name[i];
    }
  return(peak_name);
}


static char *expanded_peak_name(const char *name, int chan)
{
  char *fullname, *peak_file_name, *clean_name;

  clean_name = peak_clean(name);
  peak_file_name = mus_format("%s/%s-peaks-%d", peak_env_dir(ss), clean_name, chan);
  fullname = mus_expand_filename(peak_file_name);

  if (clean_name) free(clean_name);
  if (peak_file_name) free(peak_file_name);
  return(fullname);
}


void delete_peak_env_info_file(chan_info *cp)
{
  char *fullname;
  fullname = expanded_peak_name(cp->sound->filename, cp->chan);

  if (mus_file_probe(fullname))
    remove(fullname);

  if (fullname) free(fullname);
}


#define PEAK_ENV_VERSION 0
#define PEAK_ENV_INTS 5
#define PEAK_ENV_SAMPS 2


bool write_peak_env_info_file(chan_info *cp)
{
  char *fullname;
  peak_env_info *ep;
  int fd;
  int ibuf[PEAK_ENV_INTS];
  mus_float_t mbuf[PEAK_ENV_SAMPS];
  ssize_t bytes;

  if (!(cp->edits)) return(true);
  ep = cp->edits[0]->peak_env;
  if (ep == NULL) return(false);

  fullname = expanded_peak_name(cp->sound->filename, cp->chan);
  fd = mus_file_create(fullname);
  if (fd == -1)
    {
      if (fullname) free(fullname);
      return(false);
    }

  ibuf[0] = ((ep->completed) ? 1 : 0) | PEAK_ENV_VERSION | (pack_env_info_type() << 16);
  ibuf[1] = ep->peak_env_size;
  ibuf[2] = ep->samps_per_bin;
  ibuf[3] = ep->bin;
  ibuf[4] = ep->top_bin;
  mbuf[0] = ep->fmin;
  mbuf[1] = ep->fmax;

  bytes = write(fd, (char *)ibuf, (PEAK_ENV_INTS * sizeof(int)));
  if (bytes != 0) bytes = write(fd, (char *)mbuf, (PEAK_ENV_SAMPS * sizeof(mus_float_t)));
  if (bytes != 0) bytes = write(fd, (char *)(ep->data_min), (ep->peak_env_size * sizeof(mus_float_t)));
  if (bytes != 0) bytes = write(fd, (char *)(ep->data_max), (ep->peak_env_size * sizeof(mus_float_t)));
  if (bytes == 0) fprintf(stderr, "write error while writing peak env file");

  snd_close(fd, fullname);
  if (fullname) free(fullname);
  return(true);
}


typedef enum {PEAK_ENV_NO_ERROR, PEAK_ENV_BAD_HEADER, PEAK_ENV_BAD_FORMAT, PEAK_ENV_BAD_SIZE, PEAK_ENV_NO_FILE, PEAK_ENV_NO_DATA} peak_env_error_t;
static const char *peak_env_error[6] = {
  "no error", 
  "peak-env file has a bad header!", 
  "peak-env file is in the wrong data format; will re-make it.", 
  "peak-env file size is messed up!", 
  "peak-env file has vanished!", 
  "peak-env file is empty!"};

static bool peak_env_info_type_ok(int val)
{
  return((val == 0) ||                            /* for backwards compatibility */
	 (val == pack_env_info_type()));
}


static peak_env_info *get_peak_env_info(const char *fullname, peak_env_error_t *error)
{
  peak_env_info *ep;
  int fd, hdr = 0;
  ssize_t bytes;
  int ibuf[PEAK_ENV_INTS];
  mus_float_t mbuf[PEAK_ENV_SAMPS];

  fd = mus_file_open_read(fullname);
  if (fd == -1) 
    {
      (*error) = PEAK_ENV_NO_FILE;
      return(NULL);
    }

  bytes = read(fd, (char *)ibuf, (PEAK_ENV_INTS * sizeof(int)));
  if (bytes != (PEAK_ENV_INTS * sizeof(int)))
    {
      snd_close(fd, fullname);
      (*error) = PEAK_ENV_NO_DATA;
      return(NULL);
    }

  hdr = ibuf[0];
  (*error) = PEAK_ENV_NO_ERROR;
  if (((hdr & 0xf) != 0) && ((hdr & 0xf) != 1)) 
    (*error) = PEAK_ENV_BAD_HEADER;
  else
    {
      if (!(peak_env_info_type_ok(hdr >> 16)))
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

  ep = (peak_env_info *)calloc(1, sizeof(peak_env_info));
  ep->completed = (bool)(hdr & 0xf); /* version number in higher bits */
  ep->peak_env_size = ibuf[1];
  ep->samps_per_bin = ibuf[2];
  ep->bin = ibuf[3];
  ep->top_bin = ibuf[4];

  if (read(fd, (char *)mbuf, (PEAK_ENV_SAMPS * sizeof(mus_float_t))) == 0) fprintf(stderr, "%s: read error", fullname);

  ep->fmin = mbuf[0];
  ep->fmax = mbuf[1];

  ep->data_min = (mus_float_t *)malloc(ep->peak_env_size * sizeof(mus_float_t));
  ep->data_max = (mus_float_t *)malloc(ep->peak_env_size * sizeof(mus_float_t));

  if (read(fd, (char *)(ep->data_min), (ep->peak_env_size * sizeof(mus_float_t))) == 0) fprintf(stderr, "%s: read error", fullname);
  if (read(fd, (char *)(ep->data_max), (ep->peak_env_size * sizeof(mus_float_t))) == 0) fprintf(stderr, "%s: read error", fullname);

  snd_close(fd, fullname);
  return(ep);
}


const char *read_peak_env_info_file(chan_info *cp)
{
  peak_env_error_t err = PEAK_ENV_NO_ERROR;
  char *fullname;

  if (!(cp->edits)) return(NULL);

  fullname = expanded_peak_name(cp->sound->filename, cp->chan);
  if (mus_file_probe(fullname))
    {
      if (file_write_date(fullname) > cp->sound->write_date)
	cp->edits[0]->peak_env = get_peak_env_info(fullname, &err);
      else remove(fullname);
    }
  if (fullname) free(fullname);

  if ((cp->edits[0]->peak_env == NULL) &&
      (err != PEAK_ENV_NO_ERROR))
    return(peak_env_error[(int)err]);

  return(NULL);
}


static XEN g_peak_env_info_to_vcts(peak_env_info *ep, int len)
{
  /* changed 5-Jan-03 to return vcts */
  /* in snd-test this causes unfreed memory because the sound-icon-box saves all the data for each icon (vcts unfreed) */
  XEN res;
  int i, j, lim;
  vct *vmax, *vmin;
  int loc;
  if ((len == 0) || (len > ep->peak_env_size))
    lim = ep->peak_env_size;
  else lim = len;
  res = XEN_LIST_2(xen_make_vct(lim, (mus_float_t *)calloc(lim, sizeof(mus_float_t))),
		   xen_make_vct(lim, (mus_float_t *)calloc(lim, sizeof(mus_float_t))));
  loc = snd_protect(res);
  vmin = xen_to_vct(XEN_CAR(res));
  vmax = xen_to_vct(XEN_CADR(res));
  if (ep->peak_env_size == lim)
    {
      for (i = 0; i < lim; i++)
	{
	  vmin->data[i] = ep->data_min[i];
	  vmax->data[i] = ep->data_max[i];
	}
    }
  else
    {
      mus_float_t cmax, cmin, incr, x;
      incr = (mus_float_t)(ep->peak_env_size - 1) / (mus_float_t)lim; /* make extra room on left */
      cmax = ep->fmin;
      cmin = ep->fmax;
      vmin->data[0] = ep->data_min[0];
      vmax->data[0] = ep->data_max[0];
      for (i = 1, j = 1, x = 0.0; i < ep->peak_env_size; i++)
	{
	  if (ep->data_max[i] > cmax) cmax = ep->data_max[i];
	  if (ep->data_min[i] < cmin) cmin = ep->data_min[i];
	  x += 1.0;
	  if (x >= incr)
	    {
	      vmin->data[j] = cmin;
	      vmax->data[j++] = cmax;
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


#if (!USE_NO_GUI)
typedef struct {
  chan_info *cp;
  env_state *es;
  int len;
  XEN filename;
  XEN func;
  int func_gc_loc;
} env_tick;

static idle_func_t tick_it(any_pointer_t pet)
{
  bool val;
  env_state *es;
  chan_info *cp;
  env_tick *et = (env_tick *)pet;
  es = et->es;
  cp = et->cp;
  val = tick_peak_env(cp, es);
  if (val)
    {
      es = free_env_state(es);
      if (XEN_PROCEDURE_P(et->func))
	{
	  int loc;
	  XEN peak;
	  peak = g_peak_env_info_to_vcts(cp->edits[0]->peak_env, et->len);
	  loc = snd_protect(peak);
	  XEN_CALL_3(et->func,
		     et->filename,
		     C_TO_XEN_INT(cp->chan),
		     peak,
		     "amp env tick");
	  snd_unprotect_at(et->func_gc_loc);
	  snd_unprotect_at(loc);
	}
      completely_free_snd_info(cp->sound);
      free(et);
      return(BACKGROUND_QUIT);
    }
  return(BACKGROUND_CONTINUE);
}
#endif


static XEN g_channel_amp_envs(XEN filename, XEN chan, XEN pts, XEN peak_func, XEN done_func)
{
  /* return two vectors of size pts containing y vals (min and max) of amp env
   *   if peak_func, use it to get peak_env_info file if needed
   *   if done_func set workproc that calls it when done
   */
  #define H_channel_amp_envs "(" S_channel_amp_envs " :optional file (chan 0) size peak-file-func work-proc-func): \
return two vcts of length 'size' containing y vals (min and max) of file's channel chan's amp envs. \
'peak-file-func' is used to get the name of the associated peak_env_info file if the file is very large. \
'work-proc-func' is called when the amp envs are ready if the amp envs are gathered in the background. \
If 'filename' is a sound index or a sound object, 'size' is interpreted as an edit-position, and the current amp envs are returned."

  char *fullname = NULL;
  int len, chn;
  snd_info *sp = NULL;
  chan_info *cp = NULL;
  peak_env_error_t err = PEAK_ENV_NO_ERROR;

  XEN_ASSERT_TYPE(XEN_STRING_P(filename) || XEN_INTEGER_P(filename) || XEN_NOT_BOUND_P(filename) || XEN_SOUND_P(filename), 
		  filename, XEN_ARG_1, S_channel_amp_envs, "a string or sound index");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_channel_amp_envs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(pts), pts, XEN_ARG_3, S_channel_amp_envs, "an integer");

  XEN_ASSERT_TYPE(((XEN_PROCEDURE_P(peak_func)) && (procedure_arity_ok(peak_func, 2))) ||
		  (XEN_FALSE_P(peak_func)) ||
		  (XEN_NOT_BOUND_P(peak_func)), 
		  peak_func, XEN_ARG_4, S_channel_amp_envs, "a procedure of 2 args");
  XEN_ASSERT_TYPE(((XEN_PROCEDURE_P(done_func)) && (procedure_arity_ok(done_func, 3))) ||
		  (XEN_FALSE_P(done_func)) ||
		  (XEN_NOT_BOUND_P(done_func)), 
		  done_func, XEN_ARG_5, S_channel_amp_envs, "a procedure of 3 args");

  if (!(XEN_STRING_P(filename)))
    {
      cp = get_cp(filename, chan, S_channel_amp_envs);
      if (cp)
	{
	  env_state *es;
	  peak_env_info *ep;
	  int pos;

	  pos = to_c_edit_position(cp, pts, S_channel_amp_envs, 3); /* here "pts" is edpos, not vector size */
	  if (cp->edits == NULL)
	    return(XEN_EMPTY_LIST);

	  ep = cp->edits[pos]->peak_env; /* this can be null -- we run the peak envs if necessary */
	  if ((ep) &&
	      (ep->completed))
	    return(g_peak_env_info_to_vcts(ep, ep->peak_env_size));

	  /* force amp env to completion */
	  stop_peak_env(cp);
	  es = make_env_state(cp, cp->edits[pos]->samples);
	  if (es)
	    {
	      while (!(tick_peak_env(cp, es))) {};
	      es = free_env_state(es);
	      ep = cp->edits[pos]->peak_env;
	      if (ep)
		return(g_peak_env_info_to_vcts(ep, ep->peak_env_size));
	    }
	  return(XEN_EMPTY_LIST);
	}
      /* else get_cp threw an error */
    }

  /* filename is a string from here down */

  fullname = mus_expand_filename(XEN_TO_C_STRING(filename));
  chn = XEN_TO_C_INT_OR_ELSE(chan, 0);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_channel_amp_envs, XEN_ARG_2, chan, "must be >= 0");
  len = XEN_TO_C_INT_OR_ELSE(pts, 0);

  /* look for sp->filename = fullname
     then peak
     then read direct (via make_sound_readable)
  */

  sp = find_sound(fullname, 0);
  if (sp)
    {
      if (chn < sp->nchans)
	{
	  cp = sp->chans[chn];
	  if (cp->edits[0]->peak_env)
	    {
	      if (fullname) free(fullname);
	      /* here len can be 0 */
	      return(g_peak_env_info_to_vcts(cp->edits[0]->peak_env, len));
	    }
	}
      else
	{
	  if (fullname) free(fullname);
	  XEN_ERROR(NO_SUCH_CHANNEL, 
		    XEN_LIST_3(C_TO_XEN_STRING(S_channel_amp_envs ": no such channel (~A in ~S)"),
			       chan,
			       filename));
	  return(XEN_FALSE);
	}
    }

  if (!(mus_file_probe(fullname)))
    {
      if (fullname) free(fullname);
      XEN_ERROR(NO_SUCH_FILE, 
		XEN_LIST_2(C_TO_XEN_STRING(S_channel_amp_envs ": no such file: ~S"),
			   filename));
      return(XEN_FALSE);
    }
  if (mus_sound_chans(fullname) < chn)
    {
      if (fullname) free(fullname);
      XEN_ERROR(NO_SUCH_CHANNEL, 
		XEN_LIST_3(C_TO_XEN_STRING(S_channel_amp_envs ": no such channel (~A in ~S)"),
			   chan,
			   filename));
      return(XEN_FALSE);
    }

  if (XEN_PROCEDURE_P(peak_func))
    {
      XEN peak_filename;
      peak_filename = XEN_CALL_2(peak_func,
				 filename,
				 chan,
				 "peak env filename procedure");
      if (XEN_STRING_P(peak_filename))
	{
	  char *peakname;
	  peakname = mus_expand_filename(XEN_TO_C_STRING(peak_filename));
	  if (mus_file_probe(peakname))
	    {
	      peak_env_info *ep;
	      ep = get_peak_env_info(peakname, &err);
	      if (ep)
		{
		  XEN vcts;
		  vcts = g_peak_env_info_to_vcts(ep, len);
		  ep = free_peak_env_info(ep);
		  if (peakname) free(peakname);
		  if (fullname) free(fullname);
		  return(vcts);
		}
	    }
	  /* the else side (no such file) could be considered a request to make the peak env file (i.e. not necessarily an error) */
	  if (peakname) {free(peakname); peakname = NULL;}
	}
    }

  /* now set up to read direct... */
  sp = make_sound_readable(fullname, false);
  if (fullname) free(fullname);
  fullname = NULL;
  if ((sp) &&
      (chn < sp->nchans))
    {
      env_state *es;
      XEN peak = XEN_FALSE;
      cp = sp->chans[chn];
      if (cp)
	{
	  es = make_env_state(cp, cp->edits[0]->samples);
	  if (es)
	    {
#if (!USE_NO_GUI)
	      if (XEN_PROCEDURE_P(done_func))
		{
		  int id;
		  env_tick *et;

		  if (len <= 0)
		    XEN_OUT_OF_RANGE_ERROR(S_channel_amp_envs, XEN_ARG_3, pts, "must be > 0");

		  et = (env_tick *)calloc(1, sizeof(env_tick));
		  et->cp = cp;
		  et->es = es;
		  et->func = done_func;
		  et->func_gc_loc = snd_protect(done_func);
		  et->len = len;
		  et->filename = filename;
		  id = (int)BACKGROUND_ADD(tick_it, (any_pointer_t)et);
		  return(C_TO_XEN_INT(id));
		}
#endif
	      while (!(tick_peak_env(cp, es))) {};
	      es = free_env_state(es);
	      peak = g_peak_env_info_to_vcts(cp->edits[0]->peak_env, len);
	    }
	  cp->active = CHANNEL_INACTIVE;
	  completely_free_snd_info(sp);
	  return(peak);
	}
    }
  return(XEN_FALSE);
}

/* -------------------------------------------------------------------------------- */


static XEN g_start_progress_report(XEN snd, XEN chn)
{
  #define H_start_progress_report "(" S_start_progress_report " :optional snd chn): post the hour-glass icon"
  chan_info *cp;

  ASSERT_CHANNEL(S_start_progress_report, snd, chn, 1);
  cp = get_cp(snd, chn, S_start_progress_report);
  if (!cp)
    return(snd_no_such_channel_error(S_start_progress_report, snd, chn));

  start_progress_report(cp);

  return(XEN_TRUE);
}


static XEN g_finish_progress_report(XEN snd, XEN chn)
{
  #define H_finish_progress_report "(" S_finish_progress_report " :optional snd chn): remove the hour-glass icon"
  chan_info *cp;

  ASSERT_CHANNEL(S_finish_progress_report, snd, chn, 1);
  cp = get_cp(snd, chn, S_finish_progress_report);
  if (!cp)
    return(snd_no_such_channel_error(S_finish_progress_report, snd, chn));

  finish_progress_report(cp);

  return(XEN_FALSE);
}


static XEN g_progress_report(XEN pct, XEN snd, XEN chn)
{
  #define H_progress_report "(" S_progress_report " pct :optional snd chn): \
update an on-going 'progress report' (an animated hour-glass icon) in snd's channel chn using pct to indicate how far along we are"
  chan_info *cp;

  ASSERT_CHANNEL(S_progress_report, snd, chn, 2);
  cp = get_cp(snd, chn, S_progress_report);
  if (!cp)
    return(snd_no_such_channel_error(S_progress_report, snd, chn));

  XEN_ASSERT_TYPE(XEN_NUMBER_P(pct), pct, XEN_ARG_1, S_progress_report, "a number");

  progress_report(cp, XEN_TO_C_DOUBLE(pct));
  return(pct);
}


static XEN g_sounds(void)
{
  #define H_sounds "(" S_sounds "): list of active sounds"
  int i;
  XEN result;
  result = XEN_EMPTY_LIST;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	result = XEN_CONS(C_INT_TO_XEN_SOUND(i),
			  result);
    }
  return(result);
}



#ifdef XEN_ARGIFY_1

XEN_NARGIFY_1(g_sound_p_w, g_sound_p)
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
XEN_NARGIFY_1(g_select_sound_w, g_select_sound)
XEN_ARGIFY_1(g_select_channel_w, g_select_channel)
XEN_ARGIFY_1(g_close_sound_w, g_close_sound)
XEN_ARGIFY_1(g_update_sound_w, g_update_sound)
XEN_ARGIFY_1(g_save_sound_w, g_save_sound)
XEN_NARGIFY_1(g_open_sound_w, g_open_sound)
XEN_VARGIFY(g_open_raw_sound_w, g_open_raw_sound)
XEN_NARGIFY_1(g_view_sound_w, g_view_sound)
XEN_VARGIFY(g_new_sound_w, g_new_sound)
XEN_ARGIFY_1(g_revert_sound_w, g_revert_sound)
XEN_VARGIFY(g_save_sound_as_w, g_save_sound_as)
XEN_ARGIFY_4(g_apply_controls_w, g_apply_controls)
XEN_ARGIFY_6(g_controls_to_channel_w, g_controls_to_channel)
XEN_ARGIFY_1(g_filter_control_envelope_w, g_filter_control_envelope)
XEN_ARGIFY_2(g_set_filter_control_envelope_w, g_set_filter_control_envelope)
XEN_ARGIFY_1(g_show_controls_w, g_show_controls)
XEN_ARGIFY_2(g_set_show_controls_w, g_set_show_controls)
XEN_ARGIFY_1(g_sync_w, g_sync)
XEN_ARGIFY_2(g_set_sync_w, g_set_sync)
XEN_NARGIFY_0(g_sync_max_w, g_sync_max)
XEN_ARGIFY_1(g_sound_properties_w, g_sound_properties)
XEN_ARGIFY_2(g_set_sound_properties_w, g_set_sound_properties)
XEN_ARGIFY_2(g_sound_property_w, g_sound_property)
XEN_ARGIFY_3(g_set_sound_property_w, g_set_sound_property)
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
XEN_ARGIFY_1(g_filter_control_in_hz_w, g_filter_control_in_hz)
XEN_ARGIFY_2(g_set_filter_control_in_hz_w, g_set_filter_control_in_hz)
XEN_ARGIFY_1(g_filter_control_coeffs_w, g_filter_control_coeffs)
XEN_ARGIFY_1(g_filter_control_order_w, g_filter_control_order)
XEN_ARGIFY_2(g_set_filter_control_order_w, g_set_filter_control_order)
XEN_ARGIFY_1(g_contrast_control_w, g_contrast_control)
XEN_ARGIFY_2(g_set_contrast_control_w, g_set_contrast_control)
XEN_ARGIFY_1(g_contrast_control_bounds_w, g_contrast_control_bounds)
XEN_ARGIFY_2(g_set_contrast_control_bounds_w, g_set_contrast_control_bounds)
XEN_ARGIFY_1(g_contrast_control_amp_w, g_contrast_control_amp)
XEN_ARGIFY_2(g_set_contrast_control_amp_w, g_set_contrast_control_amp)
XEN_ARGIFY_1(g_expand_control_w, g_expand_control)
XEN_ARGIFY_2(g_set_expand_control_w, g_set_expand_control)
XEN_ARGIFY_1(g_expand_control_bounds_w, g_expand_control_bounds)
XEN_ARGIFY_2(g_set_expand_control_bounds_w, g_set_expand_control_bounds)
XEN_ARGIFY_1(g_expand_control_length_w, g_expand_control_length)
XEN_ARGIFY_2(g_set_expand_control_length_w, g_set_expand_control_length)
XEN_ARGIFY_1(g_expand_control_ramp_w, g_expand_control_ramp)
XEN_ARGIFY_2(g_set_expand_control_ramp_w, g_set_expand_control_ramp)
XEN_ARGIFY_1(g_expand_control_hop_w, g_expand_control_hop)
XEN_ARGIFY_2(g_set_expand_control_hop_w, g_set_expand_control_hop)
XEN_ARGIFY_1(g_expand_control_jitter_w, g_expand_control_jitter)
XEN_ARGIFY_2(g_set_expand_control_jitter_w, g_set_expand_control_jitter)
XEN_ARGIFY_1(g_speed_control_w, g_speed_control)
XEN_ARGIFY_2(g_set_speed_control_w, g_set_speed_control)
XEN_ARGIFY_1(g_speed_control_bounds_w, g_speed_control_bounds)
XEN_ARGIFY_2(g_set_speed_control_bounds_w, g_set_speed_control_bounds)
XEN_ARGIFY_1(g_reverb_control_length_w, g_reverb_control_length)
XEN_ARGIFY_2(g_set_reverb_control_length_w, g_set_reverb_control_length)
XEN_ARGIFY_1(g_reverb_control_length_bounds_w, g_reverb_control_length_bounds)
XEN_ARGIFY_2(g_set_reverb_control_length_bounds_w, g_set_reverb_control_length_bounds)
XEN_ARGIFY_1(g_reverb_control_scale_w, g_reverb_control_scale)
XEN_ARGIFY_2(g_set_reverb_control_scale_w, g_set_reverb_control_scale)
XEN_ARGIFY_1(g_reverb_control_scale_bounds_w, g_reverb_control_scale_bounds)
XEN_ARGIFY_2(g_set_reverb_control_scale_bounds_w, g_set_reverb_control_scale_bounds)
XEN_ARGIFY_1(g_reverb_control_feedback_w, g_reverb_control_feedback)
XEN_ARGIFY_2(g_set_reverb_control_feedback_w, g_set_reverb_control_feedback)
XEN_ARGIFY_1(g_reverb_control_lowpass_w, g_reverb_control_lowpass)
XEN_ARGIFY_2(g_set_reverb_control_lowpass_w, g_set_reverb_control_lowpass)
XEN_ARGIFY_2(g_amp_control_w, g_amp_control)
XEN_ARGIFY_3(g_set_amp_control_w, g_set_amp_control)
XEN_ARGIFY_1(g_amp_control_bounds_w, g_amp_control_bounds)
XEN_ARGIFY_2(g_set_amp_control_bounds_w, g_set_amp_control_bounds)
XEN_ARGIFY_1(g_reverb_control_decay_w, g_reverb_control_decay)
XEN_ARGIFY_2(g_set_reverb_control_decay_w, g_set_reverb_control_decay)
XEN_ARGIFY_1(g_speed_control_style_w, g_speed_control_style)
XEN_ARGIFY_2(g_set_speed_control_style_w, g_set_speed_control_style)
XEN_ARGIFY_1(g_speed_control_tones_w, g_speed_control_tones)
XEN_ARGIFY_2(g_set_speed_control_tones_w, g_set_speed_control_tones)
XEN_ARGIFY_5(g_channel_amp_envs_w, g_channel_amp_envs);
XEN_ARGIFY_2(g_start_progress_report_w, g_start_progress_report)
XEN_ARGIFY_2(g_finish_progress_report_w, g_finish_progress_report)
XEN_ARGIFY_3(g_progress_report_w, g_progress_report)
XEN_NARGIFY_0(g_sounds_w, g_sounds)
XEN_NARGIFY_1(g_integer_to_sound_w, g_integer_to_sound)
XEN_NARGIFY_1(g_sound_to_integer_w, g_sound_to_integer)

#else

#define g_sound_p_w g_sound_p
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
#define g_controls_to_channel_w g_controls_to_channel
#define g_filter_control_envelope_w g_filter_control_envelope
#define g_set_filter_control_envelope_w g_set_filter_control_envelope
#define g_show_controls_w g_show_controls
#define g_set_show_controls_w g_set_show_controls
#define g_sync_w g_sync
#define g_set_sync_w g_set_sync
#define g_sync_max_w g_sync_max
#define g_sound_properties_w g_sound_properties
#define g_set_sound_properties_w g_set_sound_properties
#define g_sound_property_w g_sound_property
#define g_set_sound_property_w g_set_sound_property
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
#define g_filter_control_in_hz_w g_filter_control_in_hz
#define g_set_filter_control_in_hz_w g_set_filter_control_in_hz
#define g_filter_control_coeffs_w g_filter_control_coeffs
#define g_filter_control_order_w g_filter_control_order
#define g_set_filter_control_order_w g_set_filter_control_order
#define g_contrast_control_w g_contrast_control
#define g_set_contrast_control_w g_set_contrast_control
#define g_contrast_control_bounds_w g_contrast_control_bounds
#define g_set_contrast_control_bounds_w g_set_contrast_control_bounds
#define g_contrast_control_amp_w g_contrast_control_amp
#define g_set_contrast_control_amp_w g_set_contrast_control_amp
#define g_expand_control_w g_expand_control
#define g_set_expand_control_w g_set_expand_control
#define g_expand_control_bounds_w g_expand_control_bounds
#define g_set_expand_control_bounds_w g_set_expand_control_bounds
#define g_expand_control_length_w g_expand_control_length
#define g_set_expand_control_length_w g_set_expand_control_length
#define g_expand_control_ramp_w g_expand_control_ramp
#define g_set_expand_control_ramp_w g_set_expand_control_ramp
#define g_expand_control_hop_w g_expand_control_hop
#define g_set_expand_control_hop_w g_set_expand_control_hop
#define g_expand_control_jitter_w g_expand_control_jitter
#define g_set_expand_control_jitter_w g_set_expand_control_jitter
#define g_speed_control_w g_speed_control
#define g_set_speed_control_w g_set_speed_control
#define g_speed_control_bounds_w g_speed_control_bounds
#define g_set_speed_control_bounds_w g_set_speed_control_bounds
#define g_reverb_control_length_w g_reverb_control_length
#define g_set_reverb_control_length_w g_set_reverb_control_length
#define g_reverb_control_length_bounds_w g_reverb_control_length_bounds
#define g_set_reverb_control_length_bounds_w g_set_reverb_control_length_bounds
#define g_reverb_control_scale_w g_reverb_control_scale
#define g_set_reverb_control_scale_w g_set_reverb_control_scale
#define g_reverb_control_scale_bounds_w g_reverb_control_scale_bounds
#define g_set_reverb_control_scale_bounds_w g_set_reverb_control_scale_bounds
#define g_reverb_control_feedback_w g_reverb_control_feedback
#define g_set_reverb_control_feedback_w g_set_reverb_control_feedback
#define g_reverb_control_lowpass_w g_reverb_control_lowpass
#define g_set_reverb_control_lowpass_w g_set_reverb_control_lowpass
#define g_amp_control_w g_amp_control
#define g_set_amp_control_w g_set_amp_control
#define g_amp_control_bounds_w g_amp_control_bounds
#define g_set_amp_control_bounds_w g_set_amp_control_bounds
#define g_reverb_control_decay_w g_reverb_control_decay
#define g_set_reverb_control_decay_w g_set_reverb_control_decay
#define g_speed_control_style_w g_speed_control_style
#define g_set_speed_control_style_w g_set_speed_control_style
#define g_speed_control_tones_w g_speed_control_tones
#define g_set_speed_control_tones_w g_set_speed_control_tones
#define g_channel_amp_envs_w g_channel_amp_envs
#define g_start_progress_report_w g_start_progress_report
#define g_finish_progress_report_w g_finish_progress_report
#define g_progress_report_w g_progress_report
#define g_sounds_w g_sounds
#define g_integer_to_sound_w g_integer_to_sound
#define g_sound_to_integer_w g_sound_to_integer

#endif


void g_init_snd(void)
{
  init_xen_sound();
  init_sound_keywords();

  #define H_name_click_hook S_name_click_hook " (snd): called when sound name clicked. \
If it returns " PROC_TRUE ", the usual informative status babbling is squelched."

  #define H_after_apply_controls_hook S_after_apply_controls_hook " (snd): called when " S_apply_controls " finishes."

  #define H_peak_env_hook S_peak_env_hook " (snd chn): called when a new peak env is ready."

  name_click_hook =           XEN_DEFINE_HOOK(S_name_click_hook,           "(make-hook 'snd)",      1, H_name_click_hook);
  after_apply_controls_hook = XEN_DEFINE_HOOK(S_after_apply_controls_hook, "(make-hook 'snd)",      1, H_after_apply_controls_hook);
  peak_env_hook =             XEN_DEFINE_HOOK(S_peak_env_hook,             "(make-hook 'snd 'chn)", 2, H_peak_env_hook);

  #define H_channels_separate "The value for " S_channel_style " that causes channel graphs to occupy separate panes"
  #define H_channels_combined "The value for " S_channel_style " that causes channel graphs to occupy one pane (set by the 'unite' button)"
  #define H_channels_superimposed "The value for " S_channel_style " that causes channel graphs to occupy one pane and one axis"

  XEN_DEFINE_CONSTANT(S_channels_separate,     CHANNELS_SEPARATE,     H_channels_separate);
  XEN_DEFINE_CONSTANT(S_channels_combined,     CHANNELS_COMBINED,     H_channels_combined);
  XEN_DEFINE_CONSTANT(S_channels_superimposed, CHANNELS_SUPERIMPOSED, H_channels_superimposed);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_channels,      g_channels_w,      H_channels,      S_setB S_channels,      g_set_channels_w,       0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_chans,         g_channels_w,      H_channels,      S_setB S_chans,         g_set_channels_w,       0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_srate,         g_srate_w,         H_srate,         S_setB S_srate,         g_set_srate_w,          0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_location, g_data_location_w, H_data_location, S_setB S_data_location, g_set_data_location_w,  0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_size,     g_data_size_w,     H_data_size,     S_setB S_data_size,     g_set_data_size_w,      0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_format,   g_data_format_w,   H_data_format,   S_setB S_data_format,   g_set_data_format_w,    0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_header_type,   g_header_type_w,   H_header_type,   S_setB S_header_type,   g_set_header_type_w,    0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_comment,       g_comment_w,       H_comment,       S_setB S_comment,       g_set_comment_w,        0, 1, 1, 1);

  XEN_DEFINE_SAFE_PROCEDURE(S_sound_p,               g_sound_p_w,          1, 0, 0, H_sound_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_find_sound,            g_find_sound_w,       1, 1, 0, H_find_sound);
  XEN_DEFINE_SAFE_PROCEDURE(S_file_name,             g_file_name_w,        0, 1, 0, H_file_name);
  XEN_DEFINE_SAFE_PROCEDURE(S_short_file_name,       g_short_file_name_w,  0, 1, 0, H_short_file_name);
  XEN_DEFINE_SAFE_PROCEDURE(S_save_controls,         g_save_controls_w,    0, 1, 0, H_save_controls);
  XEN_DEFINE_SAFE_PROCEDURE(S_restore_controls,      g_restore_controls_w, 0, 1, 0, H_restore_controls);
  XEN_DEFINE_SAFE_PROCEDURE(S_reset_controls,        g_reset_controls_w,   0, 1, 0, H_reset_controls);
  XEN_DEFINE_SAFE_PROCEDURE(S_select_sound,          g_select_sound_w,     1, 0, 0, H_select_sound);
  XEN_DEFINE_SAFE_PROCEDURE(S_select_channel,        g_select_channel_w,   0, 1, 0, H_select_channel);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_sound, g_selected_sound_w, H_selected_sound, 
				   S_setB S_selected_sound, g_select_sound_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_channel, g_selected_channel_w, H_selected_channel, 
				   S_setB S_selected_channel, g_set_selected_channel_w,  0, 1, 0, 2);

  XEN_DEFINE_PROCEDURE(S_start_progress_report,  g_start_progress_report_w,   0, 2, 0, H_start_progress_report);
  XEN_DEFINE_PROCEDURE(S_finish_progress_report, g_finish_progress_report_w,  0, 2, 0, H_finish_progress_report);
  XEN_DEFINE_PROCEDURE(S_progress_report,        g_progress_report_w,         1, 2, 0, H_progress_report);

  XEN_DEFINE_PROCEDURE(S_close_sound,            g_close_sound_w,             0, 1, 0, H_close_sound);
  XEN_DEFINE_PROCEDURE(S_update_sound,           g_update_sound_w,            0, 1, 0, H_update_sound);
  XEN_DEFINE_PROCEDURE(S_save_sound,             g_save_sound_w,              0, 1, 0, H_save_sound);
  XEN_DEFINE_PROCEDURE(S_open_sound,             g_open_sound_w,              1, 0, 0, H_open_sound); /* not "safe" procedure! */
  XEN_DEFINE_PROCEDURE(S_open_raw_sound,         g_open_raw_sound_w,          0, 0, 1, H_open_raw_sound);
  XEN_DEFINE_PROCEDURE(S_view_sound,             g_view_sound_w,              1, 0, 0, H_view_sound);
  XEN_DEFINE_PROCEDURE(S_new_sound,              g_new_sound_w,               0, 0, 1, H_new_sound);
  XEN_DEFINE_PROCEDURE(S_revert_sound,           g_revert_sound_w,            0, 1, 0, H_revert_sound);
  XEN_DEFINE_PROCEDURE(S_save_sound_as,          g_save_sound_as_w,           0, 0, 1, H_save_sound_as);
  XEN_DEFINE_PROCEDURE(S_apply_controls,         g_apply_controls_w,          0, 4, 0, H_apply_controls);
  XEN_DEFINE_PROCEDURE(S_controls_to_channel,    g_controls_to_channel_w,     0, 6, 0, H_controls_to_channel);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_envelope, g_filter_control_envelope_w, H_filter_control_envelope,
					    S_setB S_filter_control_envelope, g_set_filter_control_envelope_w, g_set_filter_control_envelope_reversed, 
					    0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_controls, g_show_controls_w, H_show_controls,
					    S_setB S_show_controls, g_set_show_controls_w, g_set_show_controls_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sync, g_sync_w, H_sync,
					    S_setB S_sync, g_set_sync_w, g_set_sync_reversed, 0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE(S_sync_max, g_sync_max_w, 0, 0, 0, H_sync_max);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sound_properties, g_sound_properties_w, H_sound_properties,
					    S_setB S_sound_properties, g_set_sound_properties_w, g_set_sound_properties_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_sound_property, g_sound_property_w, H_sound_property,
					    S_setB S_sound_property, g_set_sound_property_w, g_set_sound_property_reversed, 1, 1, 2, 1);
  
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
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_in_hz, g_filter_control_in_hz_w, H_filter_control_in_hz,
					    S_setB S_filter_control_in_hz, g_set_filter_control_in_hz_w, g_set_filter_control_in_hz_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE(S_filter_control_coeffs, g_filter_control_coeffs_w, 0, 1, 0, H_filter_control_coeffs);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_filter_control_order, g_filter_control_order_w, H_filter_control_order,
					    S_setB S_filter_control_order, g_set_filter_control_order_w, g_set_filter_control_order_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control, g_contrast_control_w, H_contrast_control,
					    S_setB S_contrast_control, g_set_contrast_control_w, g_set_contrast_control_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control_bounds, g_contrast_control_bounds_w, H_contrast_control_bounds,
					    S_setB S_contrast_control_bounds, g_set_contrast_control_bounds_w, g_set_contrast_control_bounds_reversed, 
					    0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_contrast_control_amp, g_contrast_control_amp_w, H_contrast_control_amp,
					    S_setB S_contrast_control_amp, g_set_contrast_control_amp_w, g_set_contrast_control_amp_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control, g_expand_control_w, H_expand_control,
					    S_setB S_expand_control, g_set_expand_control_w, g_set_expand_control_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_bounds, g_expand_control_bounds_w, H_expand_control_bounds,
					    S_setB S_expand_control_bounds, g_set_expand_control_bounds_w, g_set_expand_control_bounds_reversed, 
					    0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_length, g_expand_control_length_w, H_expand_control_length,
					    S_setB S_expand_control_length, g_set_expand_control_length_w, g_set_expand_control_length_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_ramp, g_expand_control_ramp_w, H_expand_control_ramp,
					    S_setB S_expand_control_ramp, g_set_expand_control_ramp_w, g_set_expand_control_ramp_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_hop, g_expand_control_hop_w, H_expand_control_hop,
					    S_setB S_expand_control_hop, g_set_expand_control_hop_w, g_set_expand_control_hop_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_expand_control_jitter, g_expand_control_jitter_w, H_expand_control_jitter,
					    S_setB S_expand_control_jitter, g_set_expand_control_jitter_w, g_set_expand_control_jitter_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_speed_control, g_speed_control_w, H_speed_control,
					    S_setB S_speed_control, g_set_speed_control_w, g_set_speed_control_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_speed_control_bounds, g_speed_control_bounds_w, H_speed_control_bounds,
					    S_setB S_speed_control_bounds, g_set_speed_control_bounds_w, g_set_speed_control_bounds_reversed, 
					    0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_length, g_reverb_control_length_w, H_reverb_control_length,
					    S_setB S_reverb_control_length, g_set_reverb_control_length_w, g_set_reverb_control_length_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_length_bounds, g_reverb_control_length_bounds_w, H_reverb_control_length_bounds,
					    S_setB S_reverb_control_length_bounds, g_set_reverb_control_length_bounds_w, 
					    g_set_reverb_control_length_bounds_reversed, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_scale, g_reverb_control_scale_w, H_reverb_control_scale,
					    S_setB S_reverb_control_scale, g_set_reverb_control_scale_w, g_set_reverb_control_scale_reversed, 0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_scale_bounds, g_reverb_control_scale_bounds_w, H_reverb_control_scale_bounds,
					    S_setB S_reverb_control_scale_bounds, g_set_reverb_control_scale_bounds_w, 
					    g_set_reverb_control_scale_bounds_reversed, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_feedback, g_reverb_control_feedback_w, H_reverb_control_feedback,
					    S_setB S_reverb_control_feedback, g_set_reverb_control_feedback_w, g_set_reverb_control_feedback_reversed, 
					    0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_reverb_control_lowpass, g_reverb_control_lowpass_w, H_reverb_control_lowpass,
					    S_setB S_reverb_control_lowpass, g_set_reverb_control_lowpass_w, g_set_reverb_control_lowpass_reversed, 
					    0, 1, 1, 1);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_amp_control, g_amp_control_w, H_amp_control,
					    S_setB S_amp_control, g_set_amp_control_w, g_set_amp_control_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_amp_control_bounds, g_amp_control_bounds_w, H_amp_control_bounds,
					    S_setB S_amp_control_bounds, g_set_amp_control_bounds_w, g_set_amp_control_bounds_reversed, 
					    0, 1, 1, 1);

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

  XEN_DEFINE_PROCEDURE(S_channel_amp_envs,         g_channel_amp_envs_w,         0, 5, 0, H_channel_amp_envs);

  XEN_DEFINE_SAFE_PROCEDURE(S_sounds,                   g_sounds_w,                   0, 0, 0, H_sounds);

  XEN_DEFINE_SAFE_PROCEDURE(S_integer_to_sound,         g_integer_to_sound_w,         1, 0, 0, H_integer_to_sound);
  XEN_DEFINE_SAFE_PROCEDURE(S_sound_to_integer,         g_sound_to_integer_w,         1, 0, 0, H_sound_to_integer);
}
