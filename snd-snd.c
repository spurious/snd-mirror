/* TODO: apply to selection has two major bugs (see line 780 below)
 */

#include "snd.h"

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
  int val,pos,i,j,start,start_bin,end,end_bin,old_end_bin,old_samples,happy=0;
  env_info *ep,*old_ep;
  env_state *es;
  stop_amp_env(cp);
  pos = cp->edit_ctr;
  es = (env_state *)CALLOC(1,sizeof(env_state));
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
      es->ep = (env_info *)CALLOC(1,sizeof(env_info));
      ep = es->ep;
      old_ep = cp->amp_envs[cp->edit_ctr - 1];
      if ((cp->edit_ctr > 0) && (old_ep) && (old_ep->completed))
	{
	  /* here in many cases, the preceding edit's amp env has most of the data we need.
	   * cp->edits[cp->edit_ctr] describes the current edit, with beg and end, so in the
	   * simplest case, we can just copy to the bin representing beg, and from the bin
	   * representing end (setting ep->top_bin and ep->bin); if the file's length has
	   * changed dramatically, we need to do it all.  fmin/fmax need to be set as we copy.
	   * as-one-edit can mess this up...
	   */
	  old_samples = cp->samples[cp->edit_ctr - 1];
	  if (abs(samples - old_samples) < (samples/2))
	    {
	      start = edit_changes_begin_at(cp);
	      end = edit_changes_end_at(cp);
	      if (abs(end - start) < (samples/2))
		{
		  /* here we'll try to take advantage of an existing envelope */
		  old_ep = cp->amp_envs[cp->edit_ctr - 1];
		  ep->samps_per_bin = old_ep->samps_per_bin;
		  ep->amp_env_size = (int)(ceil((Float)(es->samples)/(Float)(ep->samps_per_bin)));
		  ep->data_max = (MUS_SAMPLE_TYPE *)CALLOC(ep->amp_env_size,sizeof(MUS_SAMPLE_TYPE));
		  ep->data_min = (MUS_SAMPLE_TYPE *)CALLOC(ep->amp_env_size,sizeof(MUS_SAMPLE_TYPE));
		  start_bin = (int)(start / ep->samps_per_bin);
		  ep->fmin = MUS_SAMPLE_0;
		  ep->fmax = MUS_SAMPLE_0;
		  for (i=0;i<start_bin;i++) 
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
		      for (i=end_bin,j=old_end_bin;(i < ep->amp_env_size) && (j < old_ep->amp_env_size);i++,j++)
			{
			  ep->data_min[i] = old_ep->data_min[j];
			  ep->data_max[i] = old_ep->data_max[j];
			  if (ep->data_min[i] < ep->fmin) ep->fmin = ep->data_min[i];
			  if (ep->data_max[i] > ep->fmax) ep->fmax = ep->data_max[i];
			}
		      ep->top_bin = end_bin;
		    }
		  else ep->top_bin = 0;
		  happy=1;
		}
	    }
	}
      if (happy == 0)
	{
	  /* we want samps_per_bin to be useful over a wide range of file sizes */
	  /* 160e6 = about a hour at 44KHz */
	  val = (int)(log((double)(es->samples)));
	  ep->amp_env_size = (int)pow(2.0,val);
	  ep->samps_per_bin = (int)(ceil((Float)(es->samples)/(Float)(ep->amp_env_size)));
	  ep->data_max = (MUS_SAMPLE_TYPE *)CALLOC(ep->amp_env_size,sizeof(MUS_SAMPLE_TYPE));
	  ep->data_min = (MUS_SAMPLE_TYPE *)CALLOC(ep->amp_env_size,sizeof(MUS_SAMPLE_TYPE));
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
  int i,n,sb,lm;
  MUS_SAMPLE_TYPE ymin,ymax,val;
  snd_fd *sfd;
  ep = es->ep;
  if (es->slice == 0)
    {
      if (ep->top_bin != 0)
	lm = (ep->top_bin - ep->bin);
      else lm = (ep->amp_env_size - ep->bin);
      if (lm > MULTIPLIER) lm = MULTIPLIER;
      sb = ep->bin;
      if ((cp->edit_ctr != 0) || (ep->samps_per_bin < (2*AMP_ENV_SUBSAMPLE)))
	{
	  if (es->sf == NULL) es->sf = init_sample_read(ep->bin * ep->samps_per_bin,cp,READ_FORWARD);
	  sfd = es->sf;
	  for (n=0;n<lm;n++,sb++)
	    {
	      NEXT_SAMPLE(val,sfd);
	      ymin = val;
	      ymax = val;
	      for (i=1;i<ep->samps_per_bin;i++)
		{
		  NEXT_SAMPLE(val,sfd);
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
	  /* an experiment... */
	  /* sub sample reads even at the lowest level (io.c -- using dummy chans to subsample) */
	  /* this actually only helps after the initial read -- the first pass has to read the file into the RAM cache */

	  int fd,subsamp,bin_size,nc,m;
	  snd_info *sp;
	  MUS_SAMPLE_TYPE **bufs;
	  sp = cp->sound;
	  subsamp = ep->samps_per_bin / AMP_ENV_SUBSAMPLE;
	  bin_size = ep->samps_per_bin / subsamp;
	  nc = cp->chan;

	  bufs = (MUS_SAMPLE_TYPE **)CALLOC(sp->nchans * subsamp,sizeof(MUS_SAMPLE_TYPE *));
	  /* we're claiming the file has sp->nchans * subsamp channels to get mus_file_read to subsample by subsamp */
	  bufs[nc] = (MUS_SAMPLE_TYPE *)CALLOC(lm * (bin_size+2),sizeof(MUS_SAMPLE_TYPE));
	  /* and we only read the current channel (bufs[nc]).
	   * (bin_size+1) should be ok (it can't be bin_size due to annoying round-off problems that
	   *   accumulate over 100 bins, and I'm paranoid)
	   */
	  fd = mus_file_open_read(sp->fullname);
	  mus_file_set_descriptors(fd,
				   sp->fullname,
				   mus_sound_data_format(sp->fullname),
				   mus_sound_datum_size(sp->fullname),
				   mus_sound_data_location(sp->fullname),
				   sp->nchans,
				   mus_sound_header_type(sp->fullname));
	  mus_file_seek_frame(fd,ep->bin*ep->samps_per_bin);
	  mus_file_read_any(fd,0,sp->nchans * subsamp,lm * (bin_size+2),bufs,(MUS_SAMPLE_TYPE *)bufs);

	  for (m=0,n=0;n<lm;n++,sb++)
	    {
	      val = bufs[nc][m++];
	      ymin = val;
	      ymax = val;
	      for (i=1;i<ep->samps_per_bin;i+=subsamp,m++)
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
      if (es->m >= es->samples) 
	{
	  es->slice++;
	  if (es->sf) es->sf = free_snd_fd(es->sf);
	}
      return(FALSE);
    }
  else
    {
      ep->completed = 1;
      return(TRUE);
    }
}

BACKGROUND_TYPE get_amp_env(chan_info *cp)
{
  /* calculate the amp env of channel */
  env_state *es;
  int pos;
  chan_context *cgx;
  if ((!cp) || (!(cp->cgx))) return(BACKGROUND_QUIT);
  cgx = cp->cgx;
  pos = cp->edit_ctr;
  if ((pos == -1) || (!(cp->sound)))
    {
      free_env_state(cp);
      return(BACKGROUND_QUIT);
    }
  if (!(cgx->amp_env_state)) 
    cgx->amp_env_state = make_env_state(cp,current_ed_samples(cp));
  es = (env_state *)(cgx->amp_env_state);
  if (tick_amp_env(cp,es))
    {
      free_env_state(cp);
      reflect_amp_env_completion(cp->sound);
      if (cp->waiting_to_make_graph) 
	{
	  cp->waiting_to_make_graph = 0;
	  update_graph(cp,NULL);
	}
      return(BACKGROUND_QUIT);
    }
  else return(BACKGROUND_CONTINUE);
}

int amp_env_maxamp_ok(chan_info *cp)
{
  env_info *ep;
  if ((cp) && (cp->amp_envs))
    {
      ep = cp->amp_envs[cp->edit_ctr];
      return((ep) && (ep->completed));
    }
  return(FALSE);
}

Float amp_env_maxamp(chan_info *cp)
{
  env_info *ep;
  MUS_SAMPLE_TYPE ymax = MUS_SAMPLE_0;
  ep = cp->amp_envs[cp->edit_ctr];
  ymax = -ep->fmin;
  if (ymax < ep->fmax) return(MUS_SAMPLE_TO_FLOAT(ep->fmax));
  return(MUS_SAMPLE_TO_FLOAT(ymax));
}

int amp_env_usable(chan_info *cp, Float samples_per_pixel, int hisamp) 
{
  env_info *ep;
  int bin;
  chan_context *cgx;
  cgx = cp->cgx;
  if ((!cgx) || (!(cp->amp_envs))) return(FALSE);
  ep = cp->amp_envs[cp->edit_ctr];
  if (ep)
    {
      bin = (int)hisamp / ep->samps_per_bin; 
      if ((ep) && ((ep->completed) || (bin < ep->bin) || ((ep->top_bin != 0) && (bin > ep->top_bin))))
	return(samples_per_pixel >= (Float)(ep->samps_per_bin));
    }
  if ((!(cgx->amp_env_in_progress)) && (current_ed_samples(cp) > AMP_ENV_CUTOFF)) 
    start_amp_env(cp);
  return(FALSE);
}

int amp_env_graph(chan_info *cp, axis_info *ap, Float samples_per_pixel, int srate) 
{
  Float step,x,xf,xk,pinc = 0.0;
  MUS_SAMPLE_TYPE ymin,ymax;
  int i,j,xi,k,kk;
  env_info *ep;
  ep = cp->amp_envs[cp->edit_ctr];
  step = samples_per_pixel/(Float)(ep->samps_per_bin);
  xf = (Float)(ap->losamp)/(Float)(ep->samps_per_bin);
  j=0;
  x=ap->x0;
  xi=grf_x(x,ap);
  i = ap->losamp;
  xk = i;
  if (cp->printing) pinc = (Float)samples_per_pixel/(Float)srate;
  ymin=ep->fmax;
  ymax=ep->fmin;
  while (i<=ap->hisamp)
    {
      k=(int)xf;
      xf+=step;
      kk=(int)xf;
      if (kk>=ep->amp_env_size) kk=ep->amp_env_size-1;
      for (;k<=kk;k++)
	{
	  if (ep->data_min[k] < ymin) ymin=ep->data_min[k];
	  if (ep->data_max[k] > ymax) ymax=ep->data_max[k];
	}
      xk += samples_per_pixel;
      i = (int)xk;
      set_grf_points(xi,j,grf_y(MUS_SAMPLE_TO_FLOAT(ymin),ap),grf_y(MUS_SAMPLE_TO_FLOAT(ymax),ap));
      if (cp->printing) {x+=pinc; ps_set_grf_points(x,j,MUS_SAMPLE_TO_FLOAT(ymin),MUS_SAMPLE_TO_FLOAT(ymax));}
      xi++;
      j++;
      ymin=ep->fmax;
      ymax=ep->fmin;
    }
  return(j);
}



static char sname[256];
char *shortname(snd_info *sp)
{
  if (is_link(sp->fullname))
    {
      sprintf(sname,"(%s)",sp->shortname);
      return(sname);
    }
  else return(sp->shortname);
}

char *shortname_indexed(snd_info *sp)
{
  if (show_indices(sp->state))
    {
      sprintf(sname,"%d: %s",sp->index,shortname(sp));
      return(sname);
    }
  else return(shortname(sp));
}

void add_sound_data(char *filename, snd_info *sp, snd_state *ss)
{
  int i;
  for (i=0;i<sp->nchans;i++) add_channel_data(filename,sp->chans[i],sp->hdr,ss);
}


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
  int semi,i,j;
  switch (style)
    {
    case SPEED_AS_RATIO: 
      for (i=1;i<TOTAL_RATS;i++)
	{
	  if (rat_values[i]>val) break;
	}
      sprintf(srcbuf,"%s",rat_names[i-1]);
      return(rat_values[i-1]);
      break;
    case SPEED_AS_SEMITONE: 
      /* find closest semitone to val */
      semi = round(log(val)*((Float)tones/log(2.0)));
      /* space until (-) num (-52 to 52 is its range if 12-tone) */
      for (i=0;i<3;i++) srcbuf[i] = ' '; 
      sprintf(src_txt_buf,"%d",semi);
      j = strlen(src_txt_buf) - 1;
      for (i=3;(i>=0) && (j>=0);i--,j--) srcbuf[i] = src_txt_buf[j];
      return(pow(2.0,((Float)semi/(Float)tones)));
      break;
    default: 
      sfs=prettyf(val,2);
      fill_number(sfs,srcbuf);
      if (sfs) FREE(sfs);
      return(val);
      break;
    }
}

static char *short_sound_format (int format, int type)
{
  switch (format)
    {
    case MUS_BSHORT: if (type == MUS_RIFF) return("short swapped"); else return("short"); break;
    case MUS_LSHORT: if (type == MUS_AIFC) return("short swapped"); else return("short"); break;
    case MUS_UBSHORT: case MUS_ULSHORT: return("unsigned short"); break;
    case MUS_MULAW: return("mulaw"); break;
    case MUS_BYTE: return("byte"); break;
    case MUS_ALAW: return("alaw"); break;
    case MUS_BFLOAT: if (type == MUS_RIFF) return("float swapped"); else return("float"); break;
    case MUS_LFLOAT: if (type == MUS_AIFC) return("float swapped"); else return("float"); break;
    case MUS_BINT: if (type == MUS_RIFF) return("int swapped"); else return("int"); break;
    case MUS_LINT: if (type == MUS_AIFC) return("int swapped"); else return("int"); break;
    case MUS_BINTN: if (type == MUS_RIFF) return("normalized int swapped"); else return("normalized int"); break;
    case MUS_LINTN: if (type == MUS_AIFC) return("normalized int swapped"); else return("normalized int"); break;
    case MUS_UBYTE: return("unsigned byte"); break;
    case MUS_B24INT: if (type == MUS_RIFF) return("24-bit swapped"); else return("24-bit"); break;
    case MUS_L24INT: if (type == MUS_AIFC) return("24-bit swapped"); else return("24-bit"); break;
    case MUS_BDOUBLE: case MUS_LDOUBLE: return("double"); break;
    case MUS_L12INT: return("12-bit"); break;
    default: return("unknown"); break;
    }
}

static char timebuf[TIME_STR_SIZE];
static char *link_file = NULL;

static char *linked_file(char *link_name)
{
  int bytes;
#if HAVE_READLINK
  if (link_file == NULL) link_file = (char *)CALLOC(128,sizeof(char));
  bytes = readlink(link_name,link_file,128);
  if (bytes > 0)
    {
      link_file[bytes] = 0;
      return(link_file);
    }
  else 
#endif
    return("?");
}

#if HAVE_GUILE
  static int dont_babble_info(snd_info *sp);
#endif

void sp_name_click(snd_info *sp)
{
  char *str;
  file_info *hdr;
  Float dur;
  int linked = 0;
  if (sp)
    {
#if HAVE_GUILE
      if (dont_babble_info(sp)) return;
#endif
      hdr = sp->hdr;
      if (hdr)
	{
	  linked = is_link(sp->fullname);
	  dur = (Float)(hdr->samples)/(Float)(hdr->chans * hdr->srate);
	  str = (char *)CALLOC(256,sizeof(char));
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
	  strftime(timebuf,TIME_STR_SIZE,STRFTIME_FORMAT,localtime(&(sp->write_date)));
#else
	  sprintf(timebuf,"");
#endif
	  sprintf(str,"%d, %d chan%s, %.3f sec%s, %s:%s, %s%s%s%s",
		  hdr->srate,
		  hdr->chans,
		  ((hdr->chans > 1) ? "s" : ""),
		  dur,
		  ((dur == 1.0) ? "" : "s"),
		  mus_header_type_name(hdr->type),
		  short_sound_format(hdr->format,hdr->type),
		  timebuf,
		  (linked) ? ", (link to " : "",
		  (linked) ? linked_file(sp->fullname) : "",
		  (linked) ? ")" : "");
	  report_in_minibuffer(sp,str);
	  FREE(str);
	}
    }
}



/* ---------------- SAVE and RESTORE control panel buttons ----------------*/

typedef struct {
  Float amp,srate,contrast,expand,revscl,revlen;
  env *filter_env;
  int expand_on,contrast_on,reverb_on,filter_on,direction,filter_order;
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
      sp->saved_controls = (ctrl_state *)CALLOC(1,sizeof(ctrl_state));
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
  if (sp->filter_env) cs->filter_env = copy_env(sp->filter_env);
  if (sp->play_direction == 1) cs->direction = 0; else cs->direction = 1;
}

void restore_control_panel(snd_info *sp) 
{
  ctrl_state *cs;
  cs = (ctrl_state *)(sp->saved_controls);
  if (!cs) 
    {
      sp->saved_controls = (ctrl_state *)CALLOC(1,sizeof(ctrl_state));
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
  toggle_expand_button(sp,cs->expand_on);
  toggle_contrast_button(sp,cs->contrast_on);
  toggle_reverb_button(sp,cs->reverb_on);
  toggle_filter_button(sp,cs->filter_on);
  toggle_direction_arrow(sp,cs->direction);
  set_snd_amp(sp,cs->amp);
  set_snd_srate(sp,cs->srate);
  set_snd_contrast(sp,cs->contrast);
  set_snd_expand(sp,cs->expand);
  set_snd_revscl(sp,cs->revscl);
  set_snd_revlen(sp,cs->revlen);
  if (sp->filter_env) free_env(sp->filter_env); 
  sp->filter_env = default_env(1.0);
  if (cs->filter_env) sp->filter_env = copy_env(cs->filter_env);
  set_snd_filter_order(sp,cs->filter_order);
}

void reset_control_panel(snd_info *sp) 
{
  toggle_expand_button(sp,DEFAULT_EXPANDING);
  toggle_contrast_button(sp,DEFAULT_CONTRASTING);
  toggle_reverb_button(sp,DEFAULT_REVERBING);
  toggle_filter_button(sp,DEFAULT_FILTERING);
  toggle_direction_arrow(sp,0);
  set_snd_amp(sp,DEFAULT_AMP);
  set_snd_srate(sp,DEFAULT_SPEED);
  set_snd_contrast(sp,DEFAULT_CONTRAST);
  set_snd_expand(sp,DEFAULT_EXPAND);
  set_snd_revscl(sp,DEFAULT_REVERB_SCALE);
  set_snd_revlen(sp,DEFAULT_REVERB_LENGTH);
  set_snd_filter_order(sp,DEFAULT_FILTER_ORDER);
  if (sp->filter_env) free_env(sp->filter_env);
  sp->filter_env = default_env(1.0);
}


/* ---------------- APPLY ---------------- */

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
  ap = (apply_state *)CALLOC(1,sizeof(apply_state));
  ap->slice = 0;
  ap->hdr = NULL;
  ap->sp = (snd_info *)xp;
  return((void *)ap);
}

#define APPLY_TICKS 4

static int apply_dur,apply_tick = 0, apply_reporting = 0, apply_trust_dur = 1, orig_dur;

BACKGROUND_TYPE apply_controls(GUI_POINTER ptr)
{
  apply_state *ap = (apply_state *)ptr;
  snd_state *ss;
  snd_context *sgx;
  snd_info *sp;
  chan_info *cp;
  Float ratio,mult_dur;
  int i,len,over_selection,curchan=0,added_dur=0;
  sp = ap->sp;
  if (!(sp->inuse)) return(BACKGROUND_QUIT);
  ss = sp->state;
  if (sp->filtering) added_dur = sp->filter_order;
  mult_dur = 1.0 / fabs(sp->srate);
  if (sp->expanding) mult_dur *= sp->expand;
  if (sp->reverbing) added_dur += (int)((SND_SRATE(sp)*reverb_decay(ss)));
  switch (ap->slice)
    {
    case 0: 
      ap->ofile = NULL;
      lock_apply(ss,sp);
      finish_keyboard_selection();
      ap->ofile = snd_tempnam(ss);
      ap->hdr = make_temp_header(ss,ap->ofile,sp->hdr,0);
      switch (ss->apply_choice)
	{
	case APPLY_TO_CHANNEL:   
	  deactivate_selection(); /* any edit will change the data within the selection highlight region */
	  ap->hdr->chans = 1; 
	  if (sp->selected_channel != NO_SELECTION) curchan = sp->selected_channel;
	  apply_dur = current_ed_samples(sp->chans[curchan]);
	  break;
	case APPLY_TO_SOUND:     
	  deactivate_selection();
	  ap->hdr->chans = sp->nchans; 
	  apply_dur = current_ed_samples(sp->chans[0]); 
	  break;
	case APPLY_TO_SELECTION: 
	  ap->hdr->chans = region_chans(0); 
	  apply_dur = region_len(0); 
	  break;
	}
      orig_dur = apply_dur;
      apply_dur = (int)(mult_dur * (apply_dur + added_dur));
      ap->ofd = open_temp_file(ap->ofile,ap->hdr->chans,ap->hdr,ss);
      sp->apply_ok = 1;
      initialize_apply(sp,ap->hdr->chans,apply_dur);
      apply_reporting = (apply_dur > (MAX_BUFFER_SIZE * 4));
      if (apply_reporting) 
	start_progress_report(sp,NOT_FROM_ENVED);
      ap->i = 0;
      if ((sp->expanding) || (sp->srate != 1.0) || (sp->reverbing)) apply_trust_dur = 0;
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
	      finish_progress_report(sp,NOT_FROM_ENVED);
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
		      progress_report(sp,"apply",1,1,(Float)(ap->i) / (Float)apply_dur,NOT_FROM_ENVED);
		    }
		}
	    }
	  /* sr .07 -> infinite output? */
	}

      return(BACKGROUND_CONTINUE);
      break;

    case 2:
      finalize_apply(sp);
      if (apply_reporting) finish_progress_report(sp,NOT_FROM_ENVED);
      close_temp_file(ap->ofd,ap->hdr,apply_dur*(ap->hdr->chans)*mus_data_format_to_bytes_per_sample((ap->hdr)->format),sp);
      if (sp->apply_ok)
	{
	  switch (ss->apply_choice)
	    {
	    case APPLY_TO_SOUND:
	      if (sp->nchans > 1) remember_temp(ap->ofile,sp->nchans);
	      for (i=0;i<sp->nchans;i++)
		file_override_samples(apply_dur,ap->ofile,sp->chans[i],i,(sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,LOCK_MIXES,"Apply");
	      break;
	    case APPLY_TO_CHANNEL: 
	      if (sp->selected_channel != NO_SELECTION) curchan = sp->selected_channel;
	      file_override_samples(apply_dur,ap->ofile,sp->chans[curchan],0,DELETE_ME,LOCK_MIXES,"Apply to channel");
	      break;
	    case APPLY_TO_SELECTION:
	      if (region_chans(0) > 1) remember_temp(ap->ofile,region_chans(0));

	      /* TODO: if duration changed, delete original first:
		  ok = delete_selection(S_src_selection,DONT_UPDATE_DISPLAY);
		  file_insert_samples(si->begs[i],k,ofile,cp,0,DELETE_ME,S_src_selection);
		  if (ok) backup_edit_list(cp);
		  if (cp->marks) ripple_marks(cp,0,0);
		  
		  then deactivate_selection?
	      */

	      /* also!! selection chans may have no relation to sound chans:
		 sync_info *si = NULL; 
		 si = region_sync(0); or some version that doesn't set up readers -- we need the channel list here
                 for (i=0;i<si->chans;i++)
		   now use si->cps[i] ->sound etc, not sp->chans[i]
		 free_sync_info(si); does not free sample readers
	      */

	      for (i=0;i<region_chans(0);i++)
		file_change_samples(selection_beg(sp->chans[i]),region_len(0),ap->ofile,sp->chans[i],i,
				    (region_chans(0) > 1) ? MULTICHANNEL_DELETION : DELETE_ME,LOCK_MIXES,"Apply to selection");
	      for (i=0;i<region_chans(0);i++)
		update_graph(sp->chans[i],NULL);
	      break;
	    }
	  report_in_minibuffer(sp,"");
	  set_apply_button(sp,FALSE);
	  sp->apply_ok = 0;

	  if ((sp->expanding) || (sp->play_direction != 1) || (sp->srate != 1.0))
	    {
	      for (i=0;i<sp->nchans;i++)
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
			  src_marks(cp,ratio,orig_dur,apply_dur,(over_selection) ? selection_beg(cp) : 0,over_selection);
			  update_graph(cp,NULL);
			}
		    }
		}
	    }
	}
      else
	{
	  remove(ap->ofile);
	  mus_sound_forget(ap->ofile);
	  report_in_minibuffer(sp,"apply flushed!");
	}
      free(ap->ofile);                                              /* safe only if tempnam, not tmpnam used */
      ap->ofile=NULL;
      if (ap->hdr) ap->hdr = free_file_info(ap->hdr);

      free_controls(sp);
      sp->saved_controls = NULL;
      restore_control_panel(sp); /* i.e. clear it */

      sp->applying = 0;
      sgx = sp->sgx;
      if (sgx->apply_in_progress) sgx->apply_in_progress = 0;
      FREE(ap);
      unlock_apply(ss,sp);
      return(BACKGROUND_QUIT);
      break;
    }
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
  sp->applying = 1;
  ap = (apply_state *)make_apply_state((void *)sp);
  while (apply_controls((GUI_POINTER)ap) == BACKGROUND_CONTINUE);
}

#if FILE_PER_CHAN
multifile_info *sort_multifile_channels(snd_state *ss, char *filename)
{
  int i,k,len,total_chans=0,ok,chan_num;
  char *chan_name;
  file_info *hdr = NULL;
  dir *file_chans;
  int *chan_locs;
  multifile_info *res;
  /* read files in dir, assuming chan.take notation (take currently ignored)
   * set chan_type FILE_PER_CHANNEL (hdr and sp), make dummy header for sound
   * hook: (func name) -> channel if to be included else #f (or -1 to pass)
   */
  file_chans = all_files_in_dir(filename);
  /* file_chans->len is how many files we found, now we need to decide how many channels they represent and make a header */
  /* assume as default that files are named chan.<whatever> -- if a number look for 0? */
  /* assume 1-based channel numbering */
  total_chans = 0;
  chan_locs = (int *)CALLOC(file_chans->len,sizeof(int));
  for (i=0;i<file_chans->len;i++)
    {
      chan_name = just_filename(file_chans->files[i]);
      len = snd_strlen(chan_name);
      if (len > 0)
	{
	  ok = 1;
#if HAVE_GUILE
	  chan_num = multifile_channel(chan_name);
	  if (chan_num != -2) /* -2 => not a member of this sound (hook procedure returned #f) */
	    {
	      if (chan_num != -1) /* -1 => no hook procedures, or hook can't decide */
		{
		  chan_locs[chan_num] = i;
		  if (total_chans < (chan_num+1)) total_chans = chan_num+1;
		}
	      else
		{
#endif
		  for (k=0;k<len;k++)
		    if (!(isdigit(chan_name[k]))) {ok=0; break;}
		  if (ok)
		    {
		      sscanf(chan_name,"%d",&chan_num);
		      if ((chan_num > 0) && (chan_num <= file_chans->len))
			{
			  chan_locs[chan_num-1] = i;
			  if (total_chans < chan_num) total_chans = chan_num;
			}
		    }
#if HAVE_GUILE
		}
	    }
#endif
	  FREE(chan_name);
	}
    }
  if (total_chans > 0)
    {
      /* make sp level header */
      hdr = (file_info *)CALLOC(1,sizeof(file_info));
      hdr->name = just_filename(filename); 
      chan_name = (char *)CALLOC(MUS_MAX_FILE_NAME,sizeof(char));
      sprintf(chan_name,"%s/%s",filename,file_chans->files[chan_locs[0]]);
      hdr->type = mus_sound_header_type(chan_name);
      if ((hdr->type == MUS_RAW) && (use_raw_defaults(ss)))
	{
	  hdr->srate = raw_srate(ss);
	  hdr->format = raw_format(ss);
	  mus_header_set_raw_defaults(raw_srate(ss),raw_chans(ss),raw_format(ss));
	}
      else
	{
	  hdr->srate = mus_sound_srate(chan_name);
	  hdr->format = mus_sound_data_format(chan_name);
	}
      hdr->samples = mus_sound_samples(chan_name) * total_chans; /* assume same length for starters */
      hdr->data_location = mus_sound_data_location(chan_name);
      hdr->comment = NULL;
      hdr->chan_type = FILE_PER_CHANNEL;
      hdr->chans = total_chans;
      FREE(chan_name);
    }
  res = (multifile_info *)CALLOC(1,sizeof(multifile_info));
  res->hdr = hdr;
  res->file_chans = file_chans;
  res->chan_locs = chan_locs;
  return(res);
}
#endif

static void set_reverb_decay(snd_state *ss, Float val) 
{
  int i;
  snd_info *sp;
  in_set_reverb_decay(ss,val);
  for (i=0;i<ss->max_sounds;i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse))
	sp->reverb_decay = val;
    }
}

static int map_sounds_speed_tones(snd_info *sp, void *val) {sp->speed_tones = (int)val; return(0);}
static void set_speed_tones(snd_state *ss, int val) {in_set_speed_tones(ss,val); map_over_sounds(ss,map_sounds_speed_tones,(void *)val);}      
static int map_sounds_speed_style(snd_info *sp, void *val) {sp->speed_style = (int)val; return(0);}
void set_speed_style(snd_state *ss, int val) {in_set_speed_style(ss,val); map_over_sounds(ss,map_sounds_speed_style,(void *)val);}      


#if HAVE_GUILE
#include "sg.h"

static char *full_filename(SCM file)
{
  char *urn,*filename;
  urn = gh_scm2newstr(file,NULL);
  filename = mus_file_full_name(urn);
  free(urn);
  return(filename);
}


static SCM g_soundQ(SCM snd_n)
{
  #define H_soundQ "(" S_soundQ " &optional (index 0)) -> #t if sound associated with index is active (accessible)"
  snd_info *sp;
  sp = get_sp(snd_n);
  RTNBOOL((sp) && snd_ok(sp));
}

static SCM g_select_sound(SCM snd_n)
{
  #define H_select_sound "(" S_select_sound " &optional snd) makes snd the selected (active) sound"
  int val;
  snd_state *ss;
  snd_info *sp;
  ERRSP(S_select_sound,snd_n,1);
  ss = get_global_state();
  val = g_scm2intdef(snd_n,0);
  if ((val >= 0) && (val < ss->max_sounds))
    {
      sp = ss->sounds[val];
      if (snd_ok(sp))
	{
	  select_channel(sp,0);
	  normalize_sound(ss,sp,sp->chans[0]);
	  map_over_chans(ss,update_graph,NULL);
	  return(snd_n);
	}
    }
  return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_select_sound),snd_n)));
}

static SCM g_select_channel(SCM chn_n)
{
  #define H_select_channel "(" S_select_channel " &optional chn) makes chn the selected (active) channel"
  snd_info *sp;
  snd_state *ss;
  int chan;
  ERRSP(S_select_channel,chn_n,1);
  ss = get_global_state();
  chan = g_scm2intdef(chn_n,0);
  sp = any_selected_sound(ss);
  if ((sp) && (chan < sp->nchans)) 
    {
      select_channel(sp,chan);
      return(chn_n);
    }
  return(scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(S_select_channel),gh_str02scm("selected-sound"),chn_n)));
}

static SCM g_find_sound(SCM filename)
{
  #define H_find_sound "(" S_find_sound " name) returns the id of the sound associated with file 'name'"
  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  ERRS1(filename,S_find_sound);
  ss = get_global_state();
  fname = gh_scm2newstr(filename,NULL);
  sp = find_sound(ss,fname);
  if (fname) free(fname);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}


static SCM g_bomb(SCM snd, SCM on)
{
  #define H_bomb "(" S_bomb " &optional snd (on #t)) displays (or erases if on=#f) the bomb icon"
  snd_info *sp;
  sp = get_sp(snd);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_bomb),snd)));
  x_bomb(sp,bool_int_or_one(on));
  return(on);
}

enum {SYNCF,UNITEF,READONLYF,NCHANSF,CONTRASTINGF,EXPANDINGF,REVERBINGF,FILTERINGF,FILTERORDERF,
      SRATEF,DATAFORMATF,DATALOCATIONF,HEADERTYPEF,CONTROLPANELSAVEF,CONTROLPANELRESTOREF,SELECTEDCHANNELF,
      COMMENTF,FILENAMEF,SHORTFILENAMEF,CLOSEF,UPDATEF,SAVEF,CURSORFOLLOWSPLAYF,SHOWCONTROLSF,
      FILTERDBING,SPEEDTONESF,SPEEDSTYLEF
};

static SCM sp_iread(SCM snd_n, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  int i;
  SCM res = SCM_EOL;
  if (SCM_EQ_P(snd_n,SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i=0;i<ss->max_sounds;i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = gh_cons(sp_iread(gh_int2scm(i),fld,caller),res);
	}
      return(scm_reverse(res));
    }
  else
    {
      sp = get_sp(snd_n);
      if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(caller),snd_n)));
      switch (fld)
	{
	case SYNCF: RTNINT(sp->syncing); break;
	case UNITEF: RTNINT(sp->combining); break;
	case READONLYF: RTNBOOL(sp->read_only); break;
	case NCHANSF: RTNINT(sp->nchans); break;
	case EXPANDINGF: RTNBOOL(sp->expanding); break;
	case CONTRASTINGF: RTNBOOL(sp->contrasting); break;
	case REVERBINGF: RTNBOOL(sp->reverbing); break;
	case FILTERINGF: RTNBOOL(sp->filtering); break;
	case FILTERDBING: RTNBOOL(sp->filter_dBing); break;
	case FILTERORDERF: RTNINT(sp->filter_order); break;
	case SRATEF: RTNINT((sp->hdr)->srate); break;
	case DATAFORMATF: return(SCM_LIST2(gh_int2scm((sp->hdr)->format),gh_str02scm(mus_data_format_name((sp->hdr)->format)))); break;
	case HEADERTYPEF: return(SCM_LIST2(gh_int2scm((sp->hdr)->type),gh_str02scm(mus_header_type_name((sp->hdr)->type)))); break;
	case DATALOCATIONF: RTNINT((sp->hdr)->data_location); break;
	case CONTROLPANELSAVEF: save_control_panel(sp); break;
	case CONTROLPANELRESTOREF: restore_control_panel(sp); break;
	case SELECTEDCHANNELF: RTNINT(sp->selected_channel); break;
	case FILENAMEF: RTNSTR(sp->fullname); break;
	case SHORTFILENAMEF: RTNSTR(sp->shortname); break;
	case COMMENTF: RTNSTR(mus_sound_comment(sp->fullname)); break;
	case CLOSEF: snd_close_file(sp,sp->state); break;
	case SAVEF: save_edits(sp,NULL); break;
	case UPDATEF: snd_update(sp->state,sp); break;
	case CURSORFOLLOWSPLAYF: RTNBOOL(sp->cursor_follows_play); break;
	case SHOWCONTROLSF: RTNBOOL(control_panel_open(sp)); break;
	case SPEEDTONESF: RTNINT(sp->speed_tones); break;
	case SPEEDSTYLEF: RTNINT(sp->speed_style); break;
	}
    }
  return(SCM_BOOL_F);
}

static SCM sp_iwrite(SCM snd_n, SCM val, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  int ival=0,i;
  if (SCM_EQ_P(snd_n,SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i=0;i<ss->max_sounds;i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    sp_iwrite(gh_int2scm(i),val,fld,caller);
	}
      return(val);
    }
  else
    {
      sp = get_sp(snd_n);
      if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(caller),snd_n)));
      ival = bool_int_or_one(val);
      switch (fld)
	{
	case SYNCF: syncb(sp,ival); break;
	case UNITEF: combineb(sp,ival); break;
	case READONLYF: sp->read_only = ival; snd_file_lock_icon(sp,ival); break;
	case EXPANDINGF: toggle_expand_button(sp,ival); break;
	case CONTRASTINGF: toggle_contrast_button(sp,ival); break;
	case REVERBINGF: toggle_reverb_button(sp,ival); break;
	case FILTERINGF: toggle_filter_button(sp,ival); break;
	case FILTERDBING: set_filter_dBing(sp,ival); break;
	case FILTERORDERF: set_snd_filter_order(sp,ival); break;
	case CURSORFOLLOWSPLAYF: sp->cursor_follows_play = ival; break;
	case SHOWCONTROLSF: if (ival) sound_show_ctrls(sp); else sound_hide_ctrls(sp); break;
	case SPEEDTONESF: sp->speed_tones = ival; break;
	case SPEEDSTYLEF: sp->speed_style = ival; break;
	}
    }
  RTNBOOL(ival);
}

#define ERRSPT(caller,index,argn) if (!(SCM_EQ_P(index,SCM_BOOL_T))) ERRSP(caller,index,argn)

static SCM g_syncing(SCM snd_n) 
{
  #define H_syncing "(" S_syncing " &optional snd) -> whether snd is sync'd to other sounds"
  ERRSPT(S_syncing,snd_n,1); 
  return(sp_iread(snd_n,SYNCF,S_syncing));
}

static SCM g_set_syncing(SCM on, SCM snd_n) 
{
  #define H_set_syncing "(" S_set_syncing " &optional (on #t) snd) sets whether snd's is sync'd to others"
  ERRB1(on,S_set_syncing); 
  ERRSPT(S_set_syncing,snd_n,2); 
  return(sp_iwrite(snd_n,on,SYNCF,S_set_syncing));
}

static SCM g_uniting(SCM snd_n) 
{
  #define H_uniting "(" S_uniting " &optional snd) -> whether snd's channels are conbined into one graph"
  ERRSPT(S_uniting,snd_n,1); 
  return(sp_iread(snd_n,UNITEF,S_uniting));
}

static SCM g_set_uniting(SCM on, SCM snd_n) 
{
  #define H_set_uniting "(" S_set_uniting " &optional (on #t) snd) sets whether snd's channels are combined"
  ERRB1(on,S_set_uniting); 
  ERRSPT(S_set_uniting,snd_n,2); 
  return(sp_iwrite(snd_n,on,UNITEF,S_set_uniting));
}

static SCM g_read_only(SCM snd_n) 
{
  #define H_read_only "(" S_read_only " &optional snd) -> whether snd is write-protected"
  ERRSPT(S_read_only,snd_n,1);
  return(sp_iread(snd_n,READONLYF,S_read_only));
}

static SCM g_set_read_only(SCM on, SCM snd_n) 
{
  #define H_set_read_only "(" S_set_read_only " &optional (on #t) snd) sets whether snd is write-protected"
  ERRB1(on,S_set_read_only); 
  ERRSPT(S_set_read_only,snd_n,2);
  return(sp_iwrite(snd_n,on,READONLYF,S_set_read_only));
}

static SCM g_channels(SCM snd_n)
{
  #define H_channels "("  S_channels " &optional snd) how many channels snd has"
  ERRSPT(S_channels,snd_n,1); 
  return(sp_iread(snd_n,NCHANSF,S_channels));
}

static SCM g_srate(SCM snd_n) 
{
  #define H_srate "(" S_srate " &optional snd) -> snd's srate"
  ERRSPT(S_srate,snd_n,1); 
  return(sp_iread(snd_n,SRATEF,S_srate));
}

static SCM g_data_location(SCM snd_n) 
{
  #define H_data_location "(" S_data_location " &optional snd) -> snd's data location"
  ERRSPT(S_data_location,snd_n,1); 
  return(sp_iread(snd_n,DATALOCATIONF,S_data_location));
}

static SCM g_data_format(SCM snd_n) 
{
  #define H_data_format "(" S_data_format " &optional snd) -> snd's data format (e.g. mus-bshort)"
  ERRSPT(S_data_format,snd_n,1); 
  return(sp_iread(snd_n,DATAFORMATF,S_data_format));
}

static SCM g_header_type(SCM snd_n) 
{
  #define H_header_type "(" S_header_type " &optional snd) -> snd's header type (e.g. mus-aiff)"
  ERRSPT(S_header_type,snd_n,1); 
  return(sp_iread(snd_n,HEADERTYPEF,S_header_type));
}

static SCM g_comment(SCM snd_n)
{
  #define H_comment "(" S_comment " &optional snd) -> snd's comment (in its header)"
  ERRSPT(S_comment,snd_n,1); 
  return(sp_iread(snd_n,COMMENTF,S_comment));
}

static SCM g_contrasting(SCM snd_n) 
{
  #define H_contrasting "(" S_contrasting " &optional snd) -> snd's control panel constrast button state"
  ERRSPT(S_contrasting,snd_n,1); 
  return(sp_iread(snd_n,CONTRASTINGF,S_contrasting));
}

static SCM g_set_contrasting(SCM on, SCM snd_n) 
{
  #define H_set_contrasting "(" S_set_contrasting " &optional (on #t) snd) sets snd's control panel constrast button state"
  ERRB1(on,S_set_contrasting); 
  ERRSPT(S_set_contrasting,snd_n,2); 
  return(sp_iwrite(snd_n,on,CONTRASTINGF,S_set_contrasting));
}

static SCM g_expanding(SCM snd_n) 
{
  #define H_expanding "(" S_expanding " &optional snd) -> snd's control panel expand button state"
  ERRSPT(S_expanding,snd_n,1); 
  return(sp_iread(snd_n,EXPANDINGF,S_expanding));
}

static SCM g_set_expanding(SCM on, SCM snd_n) 
{
  #define H_set_expanding "(" S_set_expanding " &optional (on #t) snd) sets snd's control panel expand button state"
  ERRB1(on,S_set_expanding); 
  ERRSPT(S_set_expanding,snd_n,2); 
  return(sp_iwrite(snd_n,on,EXPANDINGF,S_set_expanding));
}

static SCM g_reverbing(SCM snd_n) 
{
  #define H_reverbing "(" S_reverbing " &optional snd) -> snd's control panel reverb button state"
  ERRSPT(S_reverbing,snd_n,1); 
  return(sp_iread(snd_n,REVERBINGF,S_reverbing));
}

static SCM g_set_reverbing(SCM on, SCM snd_n) 
{
  #define H_set_reverbing "(" S_set_reverbing " &optional (on #t) snd) sets snd's control panel reverb button state"
  ERRB1(on,S_set_reverbing); 
  ERRSPT(S_set_reverbing,snd_n,2); 
  return(sp_iwrite(snd_n,on,REVERBINGF,S_set_reverbing));
}

static SCM g_filtering(SCM snd_n) 
{
  #define H_filtering "(" S_filtering " &optional snd) -> snd's control panel filter button state"
  ERRSPT(S_filtering,snd_n,1); 
  return(sp_iread(snd_n,FILTERINGF,S_filtering));
}

static SCM g_set_filtering(SCM on, SCM snd_n) 
{
  #define H_set_filtering "(" S_set_filtering " &optional (on #t) snd) sets snd's control panel filter button state"
  ERRB1(on,S_set_filtering); 
  ERRSPT(S_set_filtering,snd_n,2); 
  return(sp_iwrite(snd_n,on,FILTERINGF,S_set_filtering));
}

static SCM g_filter_dBing(SCM snd_n) 
{
  #define H_filter_dBing "(" S_filter_dBing " &optional snd) -> #t if snd's filter envelope is displayed in dB in control panel"
  ERRSPT(S_filter_dBing,snd_n,1); 
  return(sp_iread(snd_n,FILTERDBING,S_filter_dBing));
}

static SCM g_set_filter_dBing(SCM on, SCM snd_n) 
{
  #define H_set_filter_dBing "(" S_set_filter_dBing " &optional (val #t) snd) sets whether snd's filter envelope is displayed in dB in control panel"
  ERRB1(on,S_set_filter_dBing); 
  ERRSPT(S_set_filter_dBing,snd_n,2); 
  return(sp_iwrite(snd_n,on,FILTERDBING,S_set_filter_dBing));
}

static SCM g_filter_order(SCM snd_n) 
{
  #define H_filter_order "(" S_filter_order " &optional snd) -> filter order (in control panel)"
  ERRSPT(S_filter_order,snd_n,1); 
  return(sp_iread(snd_n,FILTERORDERF,S_filter_order));
}

static SCM g_set_filter_order(SCM on, SCM snd_n) 
{
  #define H_set_filter_order "(" S_set_filter_order " val &optional snd) sets snd's filter order (in control panel)"
  ERRN1(on,S_set_filter_order); 
  ERRSPT(S_set_filter_order,snd_n,2); 
  return(sp_iwrite(snd_n,on,FILTERORDERF,S_set_filter_order));
}

static SCM g_save_control_panel(SCM snd_n) 
{
  #define H_save_control_panel "(" S_save_control_panel " &optional snd) saves the current control panel settings for subsequent " S_restore_control_panel
  ERRSPT(S_save_control_panel,snd_n,1);
  return(sp_iread(snd_n,CONTROLPANELSAVEF,S_save_control_panel));
}

static SCM g_restore_control_panel(SCM snd_n) 
{
  #define H_restore_control_panel "(" S_restore_control_panel " &optional snd) restores the previously saved control panel settings"
  ERRSPT(S_restore_control_panel,snd_n,1); 
  return(sp_iread(snd_n,CONTROLPANELRESTOREF,S_restore_control_panel));
}

static SCM g_selected_channel(SCM snd_n) 
{
  #define H_selected_channel "(" S_selected_channel " &optional snd) -> currently selected channel in snd"
  ERRSPT(S_selected_channel,snd_n,1); 
  return(sp_iread(snd_n,SELECTEDCHANNELF,S_selected_channel));
}

static SCM g_file_name(SCM snd_n) 
{
  #define H_file_name "(" S_file_name " &optional snd) -> snd's full filename"
  ERRSPT(S_file_name,snd_n,1);
  return(sp_iread(snd_n,FILENAMEF,S_file_name));
}

static SCM g_short_file_name(SCM snd_n) 
{
  #define H_short_file_name "(" S_short_file_name " &optional snd) -> short form of snd's file name (no directory)"
  ERRSPT(S_short_file_name,snd_n,1);
  return(sp_iread(snd_n,SHORTFILENAMEF,S_short_file_name));
}

static SCM g_speed_style(SCM snd)
{
  #define H_speed_style "(" S_speed_style " (snd #t)) -> speed control panel interpretation choice (speed-as-float)"
  if ((gh_number_p(snd)) || (gh_boolean_p(snd)))
    return(sp_iread(snd,SPEEDSTYLEF,S_speed_style));
  SCM_ASSERT((SCM_EQ_P(snd,SCM_UNDEFINED)),snd,SCM_ARG1,S_speed_style);
  RTNINT(speed_style(get_global_state()));
}

static SCM g_set_speed_style(SCM speed, SCM snd) 
{
  #define H_set_speed_style "(" S_set_speed_style " val) sets " S_speed_style
  snd_state *ss;
  ERRN1(speed,S_set_speed_style); 
  if ((gh_number_p(snd)) || (gh_boolean_p(snd)))
    return(sp_iwrite(snd,speed,SPEEDSTYLEF,S_set_speed_style));
  else
    {
      SCM_ASSERT((SCM_EQ_P(snd,SCM_UNDEFINED)),snd,SCM_ARG2,S_set_speed_style);
      ss = get_global_state();
      activate_speed_in_menu(ss,iclamp(SPEED_AS_FLOAT,g_scm2int(speed),SPEED_AS_SEMITONE));
      RTNINT(speed_style(ss));
    }
}

static SCM g_speed_tones(SCM snd)
{
  #define H_speed_tones "(" S_speed_tones " (snd #t)) -> if speed-style is speed-as-semitone, this chooses the octave divisions (12)"
  if ((gh_number_p(snd)) || (gh_boolean_p(snd)))
    return(sp_iread(snd,SPEEDTONESF,S_speed_tones));
  SCM_ASSERT((SCM_EQ_P(snd,SCM_UNDEFINED)),snd,SCM_ARG1,S_speed_tones);
  RTNINT(speed_tones(get_global_state()));
}

static SCM g_set_speed_tones(SCM val, SCM snd)
{
  #define H_set_speed_tones "(" S_set_speed_tones " val (snd #t)) sets " S_speed_tones
  snd_state *ss;
  ERRN1(val,S_set_speed_tones); 
  if ((gh_number_p(snd)) || (gh_boolean_p(snd)))
    return(sp_iwrite(snd,val,SPEEDTONESF,S_set_speed_tones));
  else
    {
      SCM_ASSERT((SCM_EQ_P(snd,SCM_UNDEFINED)),snd,SCM_ARG2,S_set_speed_tones);
      ss = get_global_state();
      set_speed_tones(ss,g_scm2int(val));
      RTNINT(speed_tones(ss));
    }
}


#if FILE_PER_CHAN
static SCM string_array_to_list(char **arr, int i, int len)
{
  if (i < (len-1))
    return(gh_cons(gh_str02scm(arr[i]),string_array_to_list(arr,i+1,len)));
  else return(gh_cons(gh_str02scm(arr[i]),SCM_EOL));
}

static SCM g_file_names(SCM snd_n) 
{
  #define H_file_names "(" S_file_names " &optional snd) -> channel file names associated with snd"
  snd_info *sp;
  ERRSPT(S_file_names,snd_n,1);
  sp = get_sp(snd_n);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_file_names),snd_n)));
  if (sp->chan_type == FILE_PER_SOUND) return(sp_iread(snd_n,FILENAMEF,S_file_names));
  return(string_array_to_list(sp->channel_filenames,0,sp->nchans));
}

static SCM g_short_file_names(SCM snd_n) 
{
  #define H_short_file_names "(" S_short_file_names " &optional snd) -> channel file names (no directory) associated with snd"
  snd_info *sp;
  SCM result = SCM_EOL;
  int i;
  char **strs;
  ERRSPT(S_short_file_names,snd_n,1);
  sp = get_sp(snd_n);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_short_file_names),snd_n)));
  if (sp->chan_type == FILE_PER_SOUND) return(sp_iread(snd_n,SHORTFILENAMEF,S_short_file_names));
  strs = (char **)CALLOC(sp->nchans,sizeof(char *));
  for (i=0;i<sp->nchans;i++) strs[i] = filename_without_home_directory(sp->channel_filenames[i]);
  result = string_array_to_list(strs,0,sp->nchans);
  FREE(strs);
  return(result);
}
#endif


static SCM g_close_sound(SCM snd_n) 
{
  #define H_close_sound "(" S_close_sound " snd) closes snd"
  ERRSPT(S_close_sound,snd_n,1); 
  return(sp_iread(snd_n,CLOSEF,S_close_sound));
}

static SCM g_update_sound(SCM snd_n) 
{
  #define H_update_sound "(" S_update_sound " snd) updates snd (re-reads from disk flushing pending edits)"
  ERRSPT(S_update_sound,snd_n,1); 
  return(sp_iread(snd_n,UPDATEF,S_update_sound));
}

static SCM g_save_sound(SCM snd_n) 
{
  #define H_save_sound "(" S_save_sound " &optional snd) saves snd (updates the on-disk data to match Snd's current version)"
  ERRSPT(S_save_sound,snd_n,1); 
  return(sp_iread(snd_n,SAVEF,S_save_sound));
}

static SCM g_revert_sound(SCM index)
{
  #define H_revert_sound "("  S_revert_sound " &optional snd) reverts snd to its unedited state (undo all)"
  snd_info *sp;
  int i;
  sp = get_sp(index);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_revert_sound),index)));
  for (i=0;i<sp->nchans;i++) 
    {
      revert_edits(sp->chans[i],NULL); 
      update_graph(sp->chans[i],NULL);
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
  if ((ss->selected_sound != NO_SELECTION) && (snd_ok(ss->sounds[ss->selected_sound])))
    RTNINT(ss->selected_sound);
  return(gh_int2scm(NO_SELECTION));
}

static SCM g_open_sound(SCM filename)
{ /* returns index of new sound if successful */
  #define H_open_sound "(" S_open_sound " filename) opens filename (as if opened from File:Open menu option)"
  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  ERRS1(filename,S_open_sound);
  ss = get_global_state();
  fname = full_filename(filename);
  sp = snd_open_file(fname,ss);
  if (fname) FREE(fname);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}

static SCM g_open_raw_sound(SCM filename, SCM chans, SCM srate, SCM format)
{
  #define H_open_raw_sound "(" S_open_raw_sound " filename chans srate format) opens filename assuming the data\n\
   matches the attributes indicated unless the file actually has a header"

  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  int os,oc,ofr,ou,ofit;
  ERRS1(filename,S_open_raw_sound);
  ERRN2(srate,S_open_raw_sound);
  ERRN3(chans,S_open_raw_sound);
  ERRN4(format,S_open_raw_sound);
  ss = get_global_state();
  ou=use_raw_defaults(ss);
  os=raw_srate(ss);
  oc=raw_chans(ss);
  ofr=raw_format(ss);
  ofit=fit_data_on_open(ss);
  set_raw_srate(ss,g_scm2int(srate));
  set_raw_chans(ss,g_scm2int(chans));
  set_raw_format(ss,g_scm2int(format));
  set_use_raw_defaults(ss,1);
  set_fit_data_on_open(ss,1);
  mus_header_set_raw_defaults(g_scm2int(srate),g_scm2int(chans),g_scm2int(format));
  fname = full_filename(filename);
  sp = snd_open_file(fname,ss);
  if (fname) FREE(fname);
  set_raw_srate(ss,os);
  set_raw_chans(ss,oc);
  set_raw_format(ss,ofr);
  set_use_raw_defaults(ss,ou);
  set_fit_data_on_open(ss,ofit);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}

static SCM g_open_alternate_sound(SCM filename)
{
  #define H_open_alternate_sound "(" S_open_alternate_sound " filename) replace currently selected sound with filename"
  /* returns index of new sound if successful */
  char *fname = NULL;
  snd_state *ss;
  snd_info *sp;
  ERRS1(filename,S_open_alternate_sound);
  ss = get_global_state();
  sp = any_selected_sound(ss);
  if (sp) snd_close_file(sp,ss); /* should we ask about saving edits here? */
  fname = full_filename(filename);
  sp = snd_open_file(fname,ss);
  if (fname) FREE(fname);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}

static SCM g_view_sound(SCM filename)
{
  #define H_view_sound "(" S_view_sound " filename) opens file name read-only"
  char *fname = NULL;
  snd_info *sp = NULL;
  snd_state *ss;
  ERRS1(filename,S_view_sound);
  ss = get_global_state();
  fname = full_filename(filename);
  if (fname)
    {
      ss->viewing = 1;
      sp = snd_open_file(fname,ss);
      FREE(fname);
      ss->viewing = 0;
      if (sp) RTNINT(sp->index);
    }
  return(SCM_BOOL_F);
}

static SCM g_save_sound_as(SCM newfile, SCM index, SCM type, SCM format, SCM srate, SCM channel)
{
  #define H_save_sound_as "("  S_save_sound_as " filename &optional snd header-type data-format srate channel)\n\
   saves snd in filename using the indicated attributes.  If channel is specified, only that channel is saved (extracted)."

  snd_info *sp;
  file_info *hdr;
  int ht,df,sr,chan;
  char *fname = NULL;
  ERRS1(newfile,S_save_sound_as);
  sp = get_sp(index);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_save_sound_as),index)));
  fname = full_filename(newfile);
  hdr = sp->hdr;
  ht = g_scm2intdef(type,hdr->type);
  sr = g_scm2intdef(srate,hdr->srate);
  if (gh_number_p(format)) 
    df = g_scm2int(format);
  else
    {
      if (mus_header_writable(ht,hdr->format))
	df = hdr->format;
      else df = MUS_OUT_FORMAT;
    }
  if (gh_number_p(channel))
    {
      chan = g_scm2int(channel);
      if ((chan >= sp->nchans) || (chan < 0))
	{
	  if (fname) FREE(fname);
	  return(scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(S_save_sound_as),index,channel)));
	}
      else chan_save_edits(sp->chans[chan],fname);
    }
  else save_edits_2(sp,fname,ht,df,sr,NULL); /* last arg is comment */
  if (fname) FREE(fname);
  return(newfile);
}

static SCM g_new_sound(SCM name, SCM type, SCM format, SCM srate, SCM chans, SCM comment) 
{
  #define H_new_sound "(" S_new_sound " name &optional type format srate chans comment) creates a new sound file\n\
   with the indicated attributes; if any are omitted, the corresponding default-output variable is used"

  snd_info *sp; 
  int ht,df,sr,ch;
  snd_state *ss;
  char *str = NULL,*com = NULL;
  ERRS1(name,S_new_sound);
  ss = get_global_state();
  str = full_filename(name);
  if ((!(gh_number_p(type))) || (g_scm2int(type) == MUS_UNSUPPORTED))
    sp = snd_new_file(ss,str,MUS_UNSUPPORTED,MUS_UNSUPPORTED,0,0,NULL);
  else 
    {
      ht = g_scm2int(type);
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  df = g_scm2intdef(format,MUS_OUT_FORMAT);
	  if (MUS_DATA_FORMAT_OK(df))
	    {
	      if (mus_header_writable(ht,df))
		{
		  sr = g_scm2intdef(srate,22050);
		  ch = g_scm2intdef(chans,1);
		  if (gh_string_p(comment))
		    com = gh_scm2newstr(comment,NULL);
		  sp = snd_new_file(ss,str,ht,df,sr,ch,com);
		  if (com) free(com);
		}
#if HAVE_GUILE_1_3_0
	      else scm_misc_error(S_new_sound,"can't write this combination of data format (%S) and header type (%S)",SCM_LIST2(type,format));
	    }
	  else scm_misc_error(S_new_sound,"invalid data format: %S",SCM_LIST1(format));
	}
      else scm_misc_error(S_new_sound,"invalid header type: %S",SCM_LIST1(type));
#else
	      else scm_misc_error(S_new_sound,"can't write this combination of data format (~S) and header type (~S)",SCM_LIST2(type,format));
	    }
	  else scm_misc_error(S_new_sound,"invalid data format: ~S",SCM_LIST1(format));
	}
      else scm_misc_error(S_new_sound,"invalid header type: ~S",SCM_LIST1(type));
#endif
    }
  if (str) FREE(str);
  if (sp) RTNINT(sp->index);
  return(SCM_BOOL_F);
}



static SCM g_cursor_follows_play(SCM snd_n) 
{
  #define H_cursor_follows_play "("  S_cursor_follows_play " &optional snd) -> #t if cursor moves along in waveform display as sound is played (#f)"
  ERRSPT(S_cursor_follows_play,snd_n,1); 
  return(sp_iread(snd_n,CURSORFOLLOWSPLAYF,S_cursor_follows_play));
}

static SCM g_set_cursor_follows_play(SCM on, SCM snd_n) 
{
  #define H_set_cursor_follows_play "(" S_set_cursor_follows_play " &optional (val #t)) sets " S_cursor_follows_play
  ERRB1(on,S_set_cursor_follows_play); 
  ERRSPT(S_set_cursor_follows_play,snd_n,2); 
  return(sp_iwrite(snd_n,on,CURSORFOLLOWSPLAYF,S_set_cursor_follows_play));
}

static SCM g_showing_controls(SCM snd_n) 
{
  #define H_showing_controls "(" S_showing_controls " &optional snd) -> #t if snd's control panel is known to be open"
  ERRSPT(S_showing_controls,snd_n,1); 
  return(sp_iread(snd_n,SHOWCONTROLSF,S_showing_controls));
}

static SCM g_set_showing_controls(SCM on, SCM snd_n)
{
  #define H_set_showing_controls "(" S_set_showing_controls " &optional (on #t) snd) sets whether snd's control panel is open"
  ERRB1(on,S_set_showing_controls); 
  ERRSPT(S_set_showing_controls,snd_n,2); 
  return(sp_iwrite(snd_n,on,SHOWCONTROLSF,S_set_showing_controls));
}

enum {AMPF,CONTRASTF,CONTRASTAMPF,EXPANDF,EXPANDLENGTHF,EXPANDRAMPF,EXPANDHOPF,
      SPEEDF,REVERBLENGTHF,REVERBFEEDBACKF,REVERBSCALEF,REVERBLOWPASSF,
      SP_REVERB_DECAY
};

static SCM sp_fread(SCM snd_n, int fld, char *caller)
{
  snd_info *sp;
  snd_state *ss;
  int i;
  SCM res = SCM_EOL;
  if (SCM_EQ_P(snd_n,SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i=0;i<ss->max_sounds;i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = gh_cons(sp_fread(gh_int2scm(i),fld,caller),res);
	}
      return(scm_reverse(res));
    }
  else
    {
      sp = get_sp(snd_n);
      if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(caller),snd_n)));
      switch (fld)
	{
	case AMPF: RTNFLT(sp->amp); break;
	case CONTRASTF: RTNFLT(sp->contrast); break;
	case CONTRASTAMPF: RTNFLT(sp->contrast_amp); break;
	case EXPANDF: RTNFLT(sp->expand); break;
	case EXPANDLENGTHF: RTNFLT(sp->expand_length); break;
	case EXPANDRAMPF: RTNFLT(sp->expand_ramp); break;
	case EXPANDHOPF: RTNFLT(sp->expand_hop); break;
	case SPEEDF: if (sp->play_direction == -1) RTNFLT((-(sp->srate))); else RTNFLT(sp->srate); break;
	case REVERBLENGTHF: RTNFLT(sp->revlen); break;
	case REVERBFEEDBACKF: RTNFLT(sp->revfb); break;
	case REVERBSCALEF: RTNFLT(sp->revscl); break;
	case REVERBLOWPASSF: RTNFLT(sp->revlp); break;
	case SP_REVERB_DECAY: RTNFLT(sp->reverb_decay); break;
	}
    }
  return(SCM_BOOL_F);
}

static SCM sp_fwrite(SCM snd_n, SCM val, int fld, char *caller)
{
  snd_info *sp;
  Float fval;
  int direction,i;
  snd_state *ss;
  if (SCM_EQ_P(snd_n,SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i=0;i<ss->max_sounds;i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    sp_fwrite(gh_int2scm(i),val,fld,caller);
	}
    }
  else
    {
      sp = get_sp(snd_n);
      if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(caller),snd_n)));
      fval = gh_scm2double(val);
      switch (fld)
	{
	case AMPF: if (fval >= 0.0) set_snd_amp(sp,fval); break;
	case CONTRASTF: set_snd_contrast(sp,fval); break;
	case CONTRASTAMPF: sp->contrast_amp = fval; if (sp->playing) dac_set_contrast_amp(sp,fval); break;
	case EXPANDF: if (fval > 0.0) set_snd_expand(sp,fval); break;
	case EXPANDLENGTHF: if (fval > 0.0) sp->expand_length = fval; if (sp->playing) dac_set_expand_length(sp,fval); break;
	case EXPANDRAMPF: if ((fval >= 0.0) && (fval < 0.5)) sp->expand_ramp = fval; if (sp->playing) dac_set_expand_ramp(sp,fval); break;
	case EXPANDHOPF: if (fval > 0.0) sp->expand_hop = fval; if (sp->playing) dac_set_expand_hop(sp,fval); break;
	case SPEEDF: 
	  if (fval != 0.0)
	    {
	      if (fval > 0.0) direction=1; else direction=-1;
	      set_snd_srate(sp,direction*fval); 
	      toggle_direction_arrow(sp,(direction == -1));
	    }
	  break;
	case REVERBLENGTHF: if (fval >= 0.0) set_snd_revlen(sp,fval); break;
	case REVERBFEEDBACKF: sp->revfb = fval; if (sp->playing) dac_set_reverb_feedback(sp,fval); break;
	case REVERBSCALEF: set_snd_revscl(sp,fval); break;
	case REVERBLOWPASSF: sp->revlp = fval; if (sp->playing) dac_set_reverb_lowpass(sp,fval); break;
	case SP_REVERB_DECAY: sp->reverb_decay = fval; break;
	}
    }
  return(val);
}

static SCM g_amp(SCM snd_n) 
{
  #define H_amp "(" S_amp " &optional snd) -> current amp slider setting"
  ERRSPT(S_amp,snd_n,1); 
  return(sp_fread(snd_n,AMPF,S_amp));
}

static SCM g_set_amp(SCM on, SCM snd_n) 
{
  #define H_set_amp "(" S_set_amp " val &optional snd) sets snd's amp slider value to val"
  ERRN1(on,S_set_amp); 
  ERRSPT(S_set_amp,snd_n,2); 
  return(sp_fwrite(snd_n,on,AMPF,S_set_amp));
}

static SCM g_contrast(SCM snd_n) 
{
  #define H_contrast "(" S_contrast " &optional snd) -> current contrast slider setting"
  ERRSPT(S_contrast,snd_n,1); 
  return(sp_fread(snd_n,CONTRASTF,S_contrast));
}

static SCM g_set_contrast(SCM on, SCM snd_n) 
{
  #define H_set_contrast " val &optional snd) sets snd's contrast slider value to val"
  ERRN1(on,S_set_contrast); 
  ERRSPT(S_set_contrast,snd_n,2); 
  return(sp_fwrite(snd_n,on,CONTRASTF,S_set_contrast));
}

static SCM g_contrast_amp(SCM snd_n) 
{
  #define H_contrast_amp "(" S_contrast_amp " &optional snd) -> snd's contrast amp\n\
   (scaler on data before contrast operation in control panel, 1.0)"

  ERRSPT(S_contrast_amp,snd_n,1); 
  return(sp_fread(snd_n,CONTRASTAMPF,S_contrast_amp));
}

static SCM g_set_contrast_amp(SCM on, SCM snd_n) 
{
  #define H_set_contrast_amp "(" S_set_contrast_amp " val &optional snd) sets snd's contrast scaler"
  ERRN1(on,S_set_contrast_amp);
  ERRSPT(S_set_contrast_amp,snd_n,2); 
  return(sp_fwrite(snd_n,on,CONTRASTAMPF,S_set_contrast_amp));
}

static SCM g_expand(SCM snd_n) 
{
  #define H_expand "(" S_expand " &optional snd) -> current expand slider setting"
  ERRSPT(S_expand,snd_n,1); 
  return(sp_fread(snd_n,EXPANDF,S_expand));
}

static SCM g_set_expand(SCM on, SCM snd_n) 
{
  #define H_set_expand "(" S_set_expand " val &optional snd) sets snd's expand (granular synthesis) slider value to val"
  ERRN1(on,S_set_expand); 
  ERRSPT(S_set_expand,snd_n,2); 
  return(sp_fwrite(snd_n,on,EXPANDF,S_set_expand));
}

static SCM g_expand_length(SCM snd_n) 
{
  #define H_expand_length "(" S_expand_length " &optional snd) -> current expansion segment length in seconds (.15)"
  ERRSPT(S_expand_length,snd_n,1); 
  return(sp_fread(snd_n,EXPANDLENGTHF,S_expand_length));
}

static SCM g_set_expand_length(SCM on, SCM snd_n) 
{
  #define H_set_expand_length "(" S_set_expand_length " val &optional snd) sets snd's current expansion segment length"
  ERRN1(on,S_set_expand_length); 
  ERRSPT(S_set_expand_length,snd_n,2); 
  return(sp_fwrite(snd_n,on,EXPANDLENGTHF,S_set_expand_length));
}

static SCM g_expand_ramp(SCM snd_n) 
{
  #define H_expand_ramp "(" S_expand_ramp " &optional snd) -> current expansion ramp time (.4)"
  ERRSPT(S_expand_ramp,snd_n,1); 
  return(sp_fread(snd_n,EXPANDRAMPF,S_expand_ramp));
}

static SCM g_set_expand_ramp(SCM on, SCM snd_n) 
{
  #define H_set_expand_ramp "(" S_set_expand_ramp " val &optional snd) sets snd's current expansion ramp time (between 0.0 and 0.5)"
  ERRN1(on,S_set_expand_ramp);
  ERRSPT(S_set_expand_ramp,snd_n,2); 
  return(sp_fwrite(snd_n,on,EXPANDRAMPF,S_set_expand_ramp));
}

static SCM g_expand_hop(SCM snd_n) 
{
  #define H_expand_hop "(" S_expand_hop " &optional snd) -> current expansion output grain spacing in seconds (0.05)"
  ERRSPT(S_expand_hop,snd_n,1); 
  return(sp_fread(snd_n,EXPANDHOPF,S_expand_hop));
}

static SCM g_set_expand_hop(SCM on, SCM snd_n) 
{
  #define H_set_expand_hop "(" S_set_expand_hop " val &optional snd) sets snd's current expansion output grain spacing in seconds"
  ERRN1(on,S_set_expand_hop); 
  ERRSPT(S_set_expand_hop,snd_n,2);
  return(sp_fwrite(snd_n,on,EXPANDHOPF,S_set_expand_hop));
}

static SCM g_speed(SCM snd_n) 
{
  #define H_speed "(" S_speed " &optional snd) -> current speed (srate) slider setting"
  ERRSPT(S_speed,snd_n,1); 
  return(sp_fread(snd_n,SPEEDF,S_speed));
}

static SCM g_set_speed(SCM on, SCM snd_n) 
{
  #define H_set_speed "(" S_set_speed " val &optiona snd) sets snd's current speed slider value to val"
  ERRN1(on,S_set_speed); 
  ERRSPT(S_set_speed,snd_n,2);
  return(sp_fwrite(snd_n,on,SPEEDF,S_set_speed));
}

static SCM g_reverb_length(SCM snd_n) 
{
  #define H_reverb_length "(" S_reverb_length " &optional snd) -> reverb decay length scaler"
  ERRSPT(S_reverb_length,snd_n,1); 
  return(sp_fread(snd_n,REVERBLENGTHF,S_reverb_length));
}

static SCM g_set_reverb_length(SCM on, SCM snd_n) 
{
  #define H_set_reverb_length "(" S_set_reverb_length " val &optional snd) sets snd's reverb decay length scaler"
  ERRN1(on,S_set_reverb_length); 
  ERRSPT(S_set_reverb_length,snd_n,2); 
  return(sp_fwrite(snd_n,on,REVERBLENGTHF,S_set_reverb_length));
}

static SCM g_reverb_feedback(SCM snd_n) 
{
  #define H_reverb_feedback "(" S_reverb_feedback " &optional snd) -> reverb feedback scaler"
  ERRSPT(S_reverb_feedback,snd_n,1); 
  return(sp_fread(snd_n,REVERBFEEDBACKF,S_reverb_feedback));
}

static SCM g_set_reverb_feedback(SCM on, SCM snd_n) 
{
  #define H_set_reverb_feedback "(" S_set_reverb_feedback " val &optional snd) sets snd's reverb feedback scaler"
  ERRN1(on,S_set_reverb_feedback); 
  ERRSPT(S_set_reverb_feedback,snd_n,2);
  return(sp_fwrite(snd_n,on,REVERBFEEDBACKF,S_set_reverb_feedback));
}

static SCM g_reverb_scale(SCM snd_n) 
{
  #define H_reverb_scale "(" S_reverb_scale " &optional snd) -> reverb scaler (the amount of reverb)"
  ERRSPT(S_reverb_scale,snd_n,1);
  return(sp_fread(snd_n,REVERBSCALEF,S_reverb_scale));
}

static SCM g_set_reverb_scale(SCM on, SCM snd_n) 
{
  #define H_set_reverb_scale "(" S_set_reverb_scale " val &optional snd) sets snd's reverb amount"
  ERRN1(on,S_set_reverb_scale); 
  ERRSPT(S_set_reverb_scale,snd_n,2); 
  return(sp_fwrite(snd_n,on,REVERBSCALEF,S_set_reverb_scale));
}

static SCM g_reverb_lowpass(SCM snd_n) 
{
  #define H_reverb_lowpass "(" S_reverb_lowpass " &optional snd) -> reverb lowpass filter coefficient"
  ERRSPT(S_reverb_lowpass,snd_n,1); 
  return(sp_fread(snd_n,REVERBLOWPASSF,S_reverb_lowpass));
}

static SCM g_set_reverb_lowpass(SCM on, SCM snd_n) 
{
  #define H_set_reverb_lowpass "(" S_set_reverb_lowpass " val &optional snd) sets snd's reverb lowpass filter coefficient"
  ERRN1(on,S_set_reverb_lowpass); 
  ERRSPT(S_set_reverb_lowpass,snd_n,2); 
  return(sp_fwrite(snd_n,on,REVERBLOWPASSF,S_set_reverb_lowpass));
}

static SCM g_reverb_decay(SCM snd)
{
  #define H_reverb_decay "(" S_reverb_decay " &optional (snd #t)) -> 'Apply' button reverb decay time (1.0 seconds)"
  if ((gh_number_p(snd)) || (gh_boolean_p(snd)))
    return(sp_fread(snd,SP_REVERB_DECAY,S_reverb_decay));
  SCM_ASSERT((SCM_EQ_P(snd,SCM_UNDEFINED)),snd,SCM_ARG1,S_reverb_decay);
  RTNFLT(reverb_decay(get_global_state()));
}

static SCM g_set_reverb_decay(SCM val, SCM snd)
{
  #define H_set_reverb_decay "(" S_set_reverb_decay " val &optional (snd #t)) sets " S_reverb_decay
  snd_state *ss;
  ERRN1(val,S_set_reverb_decay); 
  if ((gh_number_p(snd)) || (gh_boolean_p(snd)))
    return(sp_fwrite(snd,val,SP_REVERB_DECAY,S_set_reverb_decay));
  else
    {
      SCM_ASSERT((SCM_EQ_P(snd,SCM_UNDEFINED)),snd,SCM_ARG2,S_set_reverb_decay);
      ss = get_global_state();
      set_reverb_decay(ss,gh_scm2double(val));
      RTNFLT(reverb_decay(ss));
    }
}


static SCM g_set_filter_env(SCM edata, SCM snd_n)
{
  #define H_set_filter_env "(" S_set_filter_env " val &optional snd) sets snd's filter envelope (in the control panel)"
  snd_info *sp;
  sp = get_sp(snd_n);
  if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_set_filter_env),snd_n)));
  if (sp->filter_env) free_env(sp->filter_env);
  sp->filter_env = get_env(edata,SCM_BOOL_F,S_set_filter_env);
  filter_env_changed(sp,sp->filter_env);
  return(edata);
}

static SCM g_filter_env(SCM snd_n)
{
  #define H_filter_env "(" S_filter_env " &optional snd) -> snd's filter envelope (in the control panel)"
  snd_info *sp;
  ERRSP(S_filter_env,snd_n,1);
  sp = get_sp(snd_n);
  if (sp) return(env2scm(sp->filter_env)); 
  return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_filter_env),snd_n)));
}

static SCM g_call_apply(SCM snd, SCM choice)
{
  #define H_call_apply "(" S_call_apply " &optional snd) is equivalent to clicking the control panel 'Apply' button"
  snd_info *sp;
  snd_state *ss;
  ERRSP(S_call_apply,snd,1);
  sp = get_sp(snd);
  if (sp) 
    {
      ss = sp->state;
      ss->apply_choice = iclamp(0,g_scm2intdef(choice,0),2);
      run_apply_to_completion(sp); 
      return(SCM_BOOL_F);
    }
  return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_call_apply),snd)));
}

static SCM name_click_hook;
static int dont_babble_info(snd_info *sp)
{
  /* call name-click-hook (if any) return #t = don't print info in minibuffer */
#if (HAVE_GUILE && (!HAVE_GUILE_1_3_0))
  SCM res = SCM_BOOL_F,ind;
  ind = gh_int2scm(sp->index);
  if (HOOKED(name_click_hook))
    res = g_c_run_or_hook(name_click_hook,SCM_LIST1(ind));
  return(SCM_TRUE_P(res));
#else
  return(0);
#endif  
}

void g_init_snd(SCM local_doc)
{
#if (!HAVE_GUILE_1_3_0)
  name_click_hook = scm_create_hook(S_name_click_hook,1);       /* args = snd-index */
#else
  name_click_hook = gh_define(S_name_click_hook,SCM_BOOL_F);
#endif

  DEFINE_PROC(gh_new_procedure0_1(S_soundQ,SCM_FNC g_soundQ),H_soundQ);
  DEFINE_PROC(gh_new_procedure0_2(S_bomb,SCM_FNC g_bomb),H_bomb);
  DEFINE_PROC(gh_new_procedure1_0(S_find_sound,SCM_FNC g_find_sound),H_find_sound);

  DEFINE_PROC(gh_new_procedure0_1(S_channels,SCM_FNC g_channels),H_channels);
  DEFINE_PROC(gh_new_procedure0_1(S_chans,SCM_FNC g_channels),H_channels);
  DEFINE_PROC(gh_new_procedure0_1(S_srate,SCM_FNC g_srate),H_srate);
  DEFINE_PROC(gh_new_procedure0_1(S_data_location,SCM_FNC g_data_location),H_data_location);
  DEFINE_PROC(gh_new_procedure0_1(S_data_format,SCM_FNC g_data_format),H_data_format);
  DEFINE_PROC(gh_new_procedure0_1(S_header_type,SCM_FNC g_header_type),H_header_type);
  DEFINE_PROC(gh_new_procedure0_1(S_comment,SCM_FNC g_comment),H_comment);

  DEFINE_PROC(gh_new_procedure0_1(S_syncing,SCM_FNC g_syncing),H_syncing);
  DEFINE_PROC(gh_new_procedure0_2(S_set_syncing,SCM_FNC g_set_syncing),H_set_syncing);
  DEFINE_PROC(gh_new_procedure0_1(S_uniting,SCM_FNC g_uniting),H_uniting);
  DEFINE_PROC(gh_new_procedure0_2(S_set_uniting,SCM_FNC g_set_uniting),H_set_uniting);
  DEFINE_PROC(gh_new_procedure0_1(S_read_only,SCM_FNC g_read_only),H_read_only);
  DEFINE_PROC(gh_new_procedure0_2(S_set_read_only,SCM_FNC g_set_read_only),H_set_read_only);
  DEFINE_PROC(gh_new_procedure0_1(S_expanding,SCM_FNC g_expanding),H_expanding);
  DEFINE_PROC(gh_new_procedure0_2(S_set_expanding,SCM_FNC g_set_expanding),H_set_expanding);
  DEFINE_PROC(gh_new_procedure0_1(S_contrasting,SCM_FNC g_contrasting),H_contrasting);
  DEFINE_PROC(gh_new_procedure0_2(S_set_contrasting,SCM_FNC g_set_contrasting),H_set_contrasting);
  DEFINE_PROC(gh_new_procedure0_1(S_reverbing,SCM_FNC g_reverbing),H_reverbing);
  DEFINE_PROC(gh_new_procedure0_2(S_set_reverbing,SCM_FNC g_set_reverbing),H_set_reverbing);
  DEFINE_PROC(gh_new_procedure0_1(S_filtering,SCM_FNC g_filtering),H_filtering);
  DEFINE_PROC(gh_new_procedure0_2(S_set_filtering,SCM_FNC g_set_filtering),H_set_filtering);
  DEFINE_PROC(gh_new_procedure0_1(S_filter_dBing,SCM_FNC g_filter_dBing),H_filter_dBing);
  DEFINE_PROC(gh_new_procedure1_1(S_set_filter_dBing,SCM_FNC g_set_filter_dBing),H_set_filter_dBing);
  DEFINE_PROC(gh_new_procedure0_1(S_filter_order,SCM_FNC g_filter_order),H_filter_order);
  DEFINE_PROC(gh_new_procedure1_1(S_set_filter_order,SCM_FNC g_set_filter_order),H_set_filter_order);
  DEFINE_PROC(gh_new_procedure0_1(S_file_name,SCM_FNC g_file_name),H_file_name);
  DEFINE_PROC(gh_new_procedure0_1(S_short_file_name,SCM_FNC g_short_file_name),H_short_file_name);
#if FILE_PER_CHAN
  DEFINE_PROC(gh_new_procedure0_1(S_file_names,SCM_FNC g_file_names),H_file_names);
  DEFINE_PROC(gh_new_procedure0_1(S_short_file_names,SCM_FNC g_short_file_names),H_short_file_names);
#endif
  DEFINE_PROC(gh_new_procedure0_1(S_cursor_follows_play,SCM_FNC g_cursor_follows_play),H_cursor_follows_play);
  DEFINE_PROC(gh_new_procedure0_2(S_set_cursor_follows_play,SCM_FNC g_set_cursor_follows_play),H_set_cursor_follows_play);
  DEFINE_PROC(gh_new_procedure0_1(S_showing_controls,SCM_FNC g_showing_controls),H_showing_controls);
  DEFINE_PROC(gh_new_procedure0_2(S_set_showing_controls,SCM_FNC g_set_showing_controls),H_set_showing_controls);
  DEFINE_PROC(gh_new_procedure0_1(S_save_control_panel,SCM_FNC g_save_control_panel),H_save_control_panel);
  DEFINE_PROC(gh_new_procedure0_1(S_restore_control_panel,SCM_FNC g_restore_control_panel),H_restore_control_panel);
  DEFINE_PROC(gh_new_procedure0_1(S_filter_env,SCM_FNC g_filter_env),H_filter_env);
  DEFINE_PROC(gh_new_procedure1_1(S_set_filter_env,SCM_FNC g_set_filter_env),H_set_filter_env);

  DEFINE_PROC(gh_new_procedure0_1(S_selected_channel,SCM_FNC g_selected_channel),H_selected_channel);
  DEFINE_PROC(gh_new_procedure0_0(S_selected_sound,SCM_FNC g_selected_sound),H_selected_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_select_sound,SCM_FNC g_select_sound),H_select_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_select_channel,SCM_FNC g_select_channel),H_select_channel);

  DEFINE_PROC(gh_new_procedure0_1(S_close_sound,SCM_FNC g_close_sound),H_close_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_update_sound,SCM_FNC g_update_sound),H_update_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_save_sound,SCM_FNC g_save_sound),H_save_sound);
  DEFINE_PROC(gh_new_procedure1_0(S_open_sound,SCM_FNC g_open_sound),H_open_sound);
  DEFINE_PROC(gh_new_procedure4_0(S_open_raw_sound,SCM_FNC g_open_raw_sound),H_open_raw_sound);
  DEFINE_PROC(gh_new_procedure1_0(S_open_alternate_sound,SCM_FNC g_open_alternate_sound),H_open_alternate_sound);
  DEFINE_PROC(gh_new_procedure1_0(S_view_sound,SCM_FNC g_view_sound),H_view_sound);
  DEFINE_PROC(gh_new_procedure(S_new_sound,SCM_FNC g_new_sound,1,5,0),H_new_sound);
  DEFINE_PROC(gh_new_procedure0_1(S_revert_sound,SCM_FNC g_revert_sound),H_revert_sound);
  DEFINE_PROC(gh_new_procedure(S_save_sound_as,SCM_FNC g_save_sound_as,1,5,0),H_save_sound_as);

  DEFINE_PROC(gh_new_procedure0_1(S_contrast,SCM_FNC g_contrast),H_contrast);
  DEFINE_PROC(gh_new_procedure1_1(S_set_contrast,SCM_FNC g_set_contrast),H_set_contrast);
  DEFINE_PROC(gh_new_procedure0_1(S_contrast_amp,SCM_FNC g_contrast_amp),H_contrast_amp);
  DEFINE_PROC(gh_new_procedure1_1(S_set_contrast_amp,SCM_FNC g_set_contrast_amp),H_set_contrast_amp);
  DEFINE_PROC(gh_new_procedure0_1(S_expand,SCM_FNC g_expand),H_expand);
  DEFINE_PROC(gh_new_procedure1_1(S_set_expand,SCM_FNC g_set_expand),H_set_expand);
  DEFINE_PROC(gh_new_procedure0_1(S_expand_length,SCM_FNC g_expand_length),H_expand_length);
  DEFINE_PROC(gh_new_procedure1_1(S_set_expand_length,SCM_FNC g_set_expand_length),H_set_expand_length);
  DEFINE_PROC(gh_new_procedure0_1(S_expand_ramp,SCM_FNC g_expand_ramp),H_expand_ramp);
  DEFINE_PROC(gh_new_procedure1_1(S_set_expand_ramp,SCM_FNC g_set_expand_ramp),H_set_expand_ramp);
  DEFINE_PROC(gh_new_procedure0_1(S_expand_hop,SCM_FNC g_expand_hop),H_expand_hop);
  DEFINE_PROC(gh_new_procedure1_1(S_set_expand_hop,SCM_FNC g_set_expand_hop),H_set_expand_hop);
  DEFINE_PROC(gh_new_procedure0_1(S_speed,SCM_FNC g_speed),H_speed);
  DEFINE_PROC(gh_new_procedure1_1(S_set_speed,SCM_FNC g_set_speed),H_set_speed);
  DEFINE_PROC(gh_new_procedure0_1(S_reverb_length,SCM_FNC g_reverb_length),H_reverb_length);
  DEFINE_PROC(gh_new_procedure1_1(S_set_reverb_length,SCM_FNC g_set_reverb_length),H_set_reverb_length);
  DEFINE_PROC(gh_new_procedure0_1(S_reverb_scale,SCM_FNC g_reverb_scale),H_reverb_scale);
  DEFINE_PROC(gh_new_procedure1_1(S_set_reverb_scale,SCM_FNC g_set_reverb_scale),H_set_reverb_scale);
  DEFINE_PROC(gh_new_procedure0_1(S_reverb_feedback,SCM_FNC g_reverb_feedback),H_reverb_feedback);
  DEFINE_PROC(gh_new_procedure1_1(S_set_reverb_feedback,SCM_FNC g_set_reverb_feedback),H_set_reverb_feedback);
  DEFINE_PROC(gh_new_procedure0_1(S_reverb_lowpass,SCM_FNC g_reverb_lowpass),H_reverb_lowpass);
  DEFINE_PROC(gh_new_procedure1_1(S_set_reverb_lowpass,SCM_FNC g_set_reverb_lowpass),H_set_reverb_lowpass);
  DEFINE_PROC(gh_new_procedure0_1(S_amp,SCM_FNC g_amp),H_amp);
  DEFINE_PROC(gh_new_procedure1_1(S_set_amp,SCM_FNC g_set_amp),H_set_amp);
  DEFINE_PROC(gh_new_procedure0_2(S_call_apply,SCM_FNC g_call_apply),H_call_apply);

  DEFINE_PROC(gh_new_procedure(S_reverb_decay,SCM_FNC g_reverb_decay,0,1,0),H_reverb_decay);
  DEFINE_PROC(gh_new_procedure(S_set_reverb_decay,SCM_FNC g_set_reverb_decay,0,2,0),H_set_reverb_decay);
  DEFINE_PROC(gh_new_procedure(S_speed_style,SCM_FNC g_speed_style,0,1,0),H_speed_style);
  DEFINE_PROC(gh_new_procedure(S_set_speed_style,SCM_FNC g_set_speed_style,0,2,0),H_set_speed_style);
  DEFINE_PROC(gh_new_procedure(S_speed_tones,SCM_FNC g_speed_tones,0,1,0),H_speed_tones);
  DEFINE_PROC(gh_new_procedure(S_set_speed_tones,SCM_FNC g_set_speed_tones,0,2,0),H_set_speed_tones);
}

#endif
