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
#if DEBUGGING
      if (ep->completed) {snd_error("amp-env confusion"); abort();}
#endif
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

int tick_amp_env(chan_info *cp, env_state *es)
{
  env_info *ep;
  int i,n,sb,lm;
  MUS_SAMPLE_TYPE ymin,ymax,val;
  snd_fd *sfd;
  ep = es->ep;
  if (es->slice == 0)
    {
      if (es->sf == NULL) es->sf = init_sample_read(ep->bin * ep->samps_per_bin,cp,READ_FORWARD);
      if (ep->top_bin != 0)
	lm = (ep->top_bin - ep->bin);
      else lm = (ep->amp_env_size - ep->bin);
      if (lm > MULTIPLIER) lm = MULTIPLIER;
      sb = ep->bin;
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
      es->m += es->amp_buffer_size;
      ep->bin += lm;
      if (es->m >= es->samples) 
	{
	  es->slice++;
	  es->sf = free_snd_fd(es->sf);
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



static char sname[128];
char *shortname(snd_info *sp)
{
  if (is_link(sp->fullname))
    {
      sprintf(sname,"(%s)",sp->shortname);
      return(sname);
    }
  else return(sp->shortname);
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
    default: return(STR_unknown); break;
    }
}

static char timebuf[64];
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

void sp_name_click(snd_info *sp)
{
  char *str;
  file_info *hdr;
  Float dur;
  int linked = 0;
  if (sp)
    {
      hdr = sp->hdr;
      if (hdr)
	{
	  linked = is_link(sp->fullname);
	  dur = (Float)(hdr->samples)/(Float)(hdr->chans * hdr->srate);
	  str = (char *)CALLOC(256,sizeof(char));
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
	  strftime(timebuf,64,STRFTIME_FORMAT,localtime(&(sp->write_date)));
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

static int update_axes(chan_info *cp, void *ptr)
{
  set_xy_bounds(cp,cp->axis);
  return(0);
}

void set_wavo(snd_state *ss, int on)
{
  in_set_wavo(ss,on);
  if (on == 0) map_over_chans(ss,update_axes,NULL);
  map_over_chans(ss,update_graph,NULL);
}

#if ((USE_MOTIF) && (XmVERSION == 1))
void edit_history(snd_state *ss, int on)
{
  /* update menu label also */
  int wid;
  if ((on) && (edit_history_width(ss) == 0))
    {
      wid = widget_width(MAIN_PANE(ss));
      if (wid > 200) wid = 100; else wid = wid/2;
      in_set_edit_history_width(ss,wid); 
    }
  set_show_edit_history(ss,on);
  set_menu_label(edit_history_menu(),(on) ? STR_Hide_edit_history : STR_Show_edit_history);
  if (on)
    map_over_chans(ss,open_edit_histories,NULL);
  else map_over_chans(ss,close_edit_histories,NULL);
}
#endif




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
} apply_manager;

void *make_apply_state(void *xp)
{
  /* set up initial state for apply_controls */
  apply_manager *ap;
  ap = (apply_manager *)CALLOC(1,sizeof(apply_manager));
  ap->slice = 0;
  ap->hdr = NULL;
  ap->sp = (snd_info *)xp;
  return((void *)ap);
}

static BACKGROUND_TYPE apply_controls_1(apply_manager *ap)
{
  snd_state *ss;
  snd_context *sgx;
  snd_info *sp;
  chan_info *cp;
  Float ratio;
  int i,len,dac_op,over_selection;
  sp = ap->sp;
  if (!(sp->inuse)) return(BACKGROUND_QUIT);
  ss = sp->state;
  switch (ap->slice)
    {
    case 0: 
      ap->ofile = NULL;
      lock_apply(ss,sp);
      finish_keyboard_selection();
      deactivate_selection(); /* any edit will change the data within the selection highlight region */
      ap->ofile = snd_tempnam(ss);
      ap->hdr = make_temp_header(ss,ap->ofile,sp->hdr,0);
      ap->ofd = open_temp_file(ap->ofile,sp->nchans,ap->hdr,ss);
      sp->apply_ok = 1;
      initialize_apply(sp);
      ap->i = 0;
      ap->slice++;
      return(BACKGROUND_CONTINUE);
      break;

    case 1:
      if (!(sp->apply_ok))
	ap->slice++;
      else
	{
	  len = run_apply(sp,ap->ofd); /* returns samps in chan (dac_buffer_size/chans) */
	  if (len > 0)
	    ap->i += len;
	  else 
	    {
	      ap->slice++;
	      if (len < 0) ap->i -= len;
	    }
	}
      return(BACKGROUND_CONTINUE);
      break;

    case 2:
      dac_op = finalize_apply(sp);
      close_temp_file(ap->ofd,ap->hdr,ap->i*sp->nchans*mus_data_format_to_bytes_per_sample((ap->hdr)->format),sp);
      if (sp->apply_ok)
	{
	  switch (ss->apply_choice)
	    {
	    case APPLY_TO_SOUND:
	      if (sp->nchans > 1) remember_temp(ap->ofile,sp->nchans);
	      for (i=0;i<sp->nchans;i++)
		file_override_samples(ap->i,ap->ofile,sp->chans[i],i,(sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,LOCK_MIXES,"Apply");
	      break;
	    case APPLY_TO_CHANNEL: 
	      file_override_samples(ap->i,ap->ofile,sp->chans[sp->selected_channel],0,DELETE_ME,LOCK_MIXES,"Apply to channel");
	      break;
	    case APPLY_TO_SELECTION:
	      /* cannot work across sounds or with weird channel collections -- see snd-clip.c load_region */
	      if (region_chans(0) > 1) remember_temp(ap->ofile,region_chans(0));
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

	  if ((dac_op != JUST_AMP) && (dac_op != NO_CHANGE))
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
			  src_marks(cp,ratio,apply_duration(),ap->i,(over_selection) ? selection_beg(cp) : 0,over_selection);
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
	  report_in_minibuffer(sp,STR_apply_flushed);
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

BACKGROUND_TYPE apply_controls(void *xp)
{
  /* a background procedure so that it's interruptible */
  return(apply_controls_1((apply_manager *)xp));
}

void run_apply_to_completion(snd_info *sp)
{
  /* this version called from Guile, so we want it to complete before returning */
  apply_manager *ap;
  sp->applying = 1;
  ap = (apply_manager *)make_apply_state((void *)sp);
  while (apply_controls_1(ap) == BACKGROUND_CONTINUE);
}
