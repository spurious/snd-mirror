#include "snd.h"

#if defined(NEXT) || defined(HAVE_SYS_DIR_H)
  #include <sys/dir.h>
  #include <sys/dirent.h>
#else
  #if defined(WINDOZE) && (!(defined(__CYGWIN__)))
    #include <direct.h>
  #else
    #include <dirent.h>
  #endif
#endif

#if HAVE_GUILE
#include "sg.h"
static int after_fft(snd_state *ss, chan_info *cp, Float scaler);
static int dont_graph(snd_state *ss, chan_info *cp);
static void after_graph(chan_info *cp);
static int handle_mark_click(int id);
static void handle_mouse_release(snd_info *sp, chan_info *cp, Float x, Float y, int button, int state);
static void handle_mouse_press(snd_info *sp, chan_info *cp, Float x, Float y, int button, int state);
static void handle_mouse_drag(snd_info *sp, chan_info *cp, Float x, Float y);
static int handle_key_press(chan_info *cp, int key, int state);
#else
static int after_fft(snd_state *ss, chan_info *cp, Float scaler) {return(0);}
static int dont_graph(snd_state *ss, chan_info *cp) {return(0);}
static void after_graph(chan_info *cp) {}
static int handle_mark_click(int id) {return(0);}
static void handle_mouse_release(snd_info *sp, chan_info *cp, Float x, Float y, int button, int state) {}
static void handle_mouse_press(snd_info *sp, chan_info *cp, Float x, Float y, int button, int state) {}
static void handle_mouse_drag(snd_info *sp, chan_info *cp, Float x, Float y) {}
static int handle_key_press(chan_info *cp, int key, int state) {return(0);}
#endif

static char expr_str[256];

void report_in_minibuffer(snd_info *sp, char *message)
{
  set_minibuffer_string(sp,message);
  sp->minibuffer_temp = 1;
  /* leave sp->minibuffer off so that keyboard_command doesn't clear it */
}

static void append_to_minibuffer(snd_info *sp, char *message)
{
  char *str=NULL;
  sprintf(expr_str,"%s%s",str=get_minibuffer_string(sp),message);
  set_minibuffer_string(sp,expr_str);
  sp->minibuffer_temp = 1;
  if (str) free(str);
}

void clear_minibuffer_prompt(snd_info *sp)
{
  make_minibuffer_label(sp,"     ");
  sp->minibuffer_temp = 0;
}

static int prompt(snd_info *sp, char *msg, char *preload)
{
  if (preload)
    {
      set_minibuffer_string(sp,preload);
      set_minibuffer_cursor_position(sp,snd_strlen(preload));
    }
  else
    {
      expr_str[0] = '\0';
      set_minibuffer_string(sp,expr_str);
    }
  make_minibuffer_label(sp,msg);
  sp->minibuffer_on = 1;
  sp->minibuffer_temp = 0;
  goto_minibuffer(sp);
  return(CURSOR_NO_ACTION); /* make sure verbose cursor doesn't preload our prompt text field with garbage! */
}

#if HAVE_GUILE
static void g_prompt(snd_info *sp, char *prompt)
{
  make_minibuffer_label(sp,prompt);
  sp->minibuffer_on = 1;
  sp->minibuffer_temp = 0;
  sp->prompting = 1;
  goto_minibuffer(sp);
}
#endif

static int set_dot_size(snd_state *ss, int val)
{
  if (val>0)  /* -1 here can crash X! */
    {
      in_set_dot_size(ss,val);
      if ((graph_style(ss) == GRAPH_DOTS) || (graph_style(ss) == GRAPH_DOTS_AND_LINES) || (graph_style(ss) == GRAPH_LOLLIPOPS))
	map_over_chans(ss,update_graph,NULL);
    }
  return(val);
}

static chan_info *virtual_selected_channel(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp->combining == CHANNELS_SEPARATE) || (sp->selected_channel == NO_SELECTION)) 
    return(cp);
  else return(sp->chans[sp->selected_channel]);
}

static void goto_next_graph (chan_info *cp, int count);

static void goto_previous_graph (chan_info *cp, int count)
{
  snd_info *sp,*osp;
  snd_state *ss;
  chan_info *ncp,*vcp;
  int i,k,j,chan;
  if (count == 0) return;
  sp=cp->sound;
  ss=cp->state;
  osp = sp;
  vcp = virtual_selected_channel(cp);
  chan = vcp->chan;
  ncp = NULL;
  if (count < 0) k = -count; else {goto_next_graph(cp,count); return;}
  if (chan > 0)
    {
      /* goto previous channel in current sound */
      k -= chan;
      if (k <= 0)
	ncp = sp->chans[chan+count];
    }
  while (k > 0)
    {
      /* look for previous sound, (wrap around) */
      /* goto channel n */
      for (i=(sp->index-1);i>=0;i--)
	{
	  if (snd_ok(ss->sounds[i]))
	    {
	      sp = (snd_info *)(ss->sounds[i]);
	      j = k;
	      k -= sp->nchans;
	      if (k <= 0)
		ncp = sp->chans[sp->nchans-j];
	      break;
	    }
	}
      if (k > 0)
	{
	  for (i=ss->max_sounds-1;i>=sp->index;i--)
	    {
	      if (snd_ok(ss->sounds[i]))
		{
		  sp = (snd_info *)(ss->sounds[i]);
		  j = k;
		  k -= sp->nchans;
		  if (k <= 0)
		    ncp = sp->chans[sp->nchans-j];
		  break;
		}
	    }
	}
    }
  if (ncp == vcp) return;
  if (!ncp) snd_error("goto previous graph lost!");
  select_channel(ncp->sound,ncp->chan);
  normalize_sound(ss,ncp->sound,osp,ncp); /* snd-xsnd.c */
  /* goto_graph(ncp); */
}

static void goto_next_graph (chan_info *cp, int count)
{
  snd_info *sp,*osp;
  snd_state *ss;
  chan_info *ncp,*vcp;
  int i,k,j,chan;
  if (count == 0) return;
  sp=cp->sound;
  osp = sp;
  ss=cp->state;
  vcp = virtual_selected_channel(cp);
  chan = vcp->chan;
  ncp = NULL;
  if (count < 0) {goto_previous_graph(cp,count); return;}
  k = count;
  if (chan < (sp->nchans-1))
    {
      /* goto next channel in current sound */
      k -= (sp->nchans-chan-1);
      if (k <= 0)
	ncp = sp->chans[chan+count];
    }
  while (k > 0)
    {
      /* look for next sound, (wrap around) */
      /* goto channel 0 */
      for (i=(sp->index+1);i<ss->max_sounds;i++)
	{
	  if (snd_ok(ss->sounds[i]))
	    {
	      sp = (snd_info *)(ss->sounds[i]);
	      j = k;
	      k -= sp->nchans;
	      if (k <= 0)
		ncp = sp->chans[j-1];
	      break;
	    }
	}
      /* if (ss->listening) {goto_listener(); k--; if (k == 0) return;} */
      /* not really right because C-x in listener already exits listener (so C-x O in one chan case bounces back to self) */
      if (k > 0)
	{
	  for (i=0;i<=sp->index;i++)
	    {
	      if (snd_ok(ss->sounds[i]))
		{
		  sp = (snd_info *)(ss->sounds[i]);
		  j = k;
		  k -= sp->nchans;
		  if (k <= 0)
		    ncp = sp->chans[j-1];
		  break;
		}
	    }
	}
    }
  if (ncp == vcp) return;
  if (!ncp) snd_error("goto next graph lost!");
  select_channel(ncp->sound,ncp->chan);
  normalize_sound(ss,ncp->sound,osp,ncp);
  /* goto_graph(ncp); */
}

static int updating = 0;

int update_graph(chan_info *cp, void *ptr)
{
  /* don't put display stuff here!  This is needed so that the fft display does not get caught in a loop */
  double cur_srate;
  snd_state *ss;
  snd_info *sp;
  axis_info *ap;
  if ((updating) || (cp->cgx == NULL) || (cp->squelch_update) || (cp->sounds == NULL) || (cp->sounds[cp->sound_ctr] == NULL)) return(0);
  updating = 1;
  ss = cp->state;
  sp = cp->sound;
  /* next two are needed by fft and lisp displays, but if put off until make_graph cause
   * the display to happen twice in some cases 
   */
  ap = cp->axis;
  if (ap)
    {
      cur_srate = (double)(SND_SRATE(sp));
      ap->losamp = (int)(ap->x0*cur_srate); 
      if (ap->losamp < 0) ap->losamp = 0;
      ap->hisamp = (int)(ap->x1*cur_srate);
    }
  if (!(((cp->cgx)->ax)->wn)) 
    if (!(fixup_cp_cgx_ax_wn(cp))) return(0);
  if ((cp->ffting) && (!(chan_fft_in_progress(cp)))) calculate_fft(cp,NULL);
  display_channel_data(cp,sp,ss);
  updating = 0;
  return(0);
}

#define INITIAL_EDIT_SIZE 8

void add_channel_data_1(chan_info *cp, snd_info *sp, snd_state *ss, int graphed)
{
  /* initialize channel, including edit/sound lists */
  axis_info *ap;
  Float ymin,ymax,xmin,xmax,y0,y1,x0,x1,dur,gdur;
  char *label;
  file_info *hdr;
  int samples_per_channel;
  hdr = sp->hdr;
  samples_per_channel = hdr->samples/hdr->chans;
  ymax = ymax(ss);
  ymin = ymin(ss);
  xmax = xmax(ss);
  xmin = xmin(ss);
  x0 = initial_x0(ss);
  x1 = initial_x1(ss);
  y0 = initial_y0(ss);
  y1 = initial_y1(ss);
  label = STR_time;
  dur = (Float)samples_per_channel/(Float)hdr->srate;
  if (dur == 0.0) gdur = .001; else gdur = dur;
  if (xmax == 0.0) xmax = gdur;
  if (x1 == 0.0) x1 = gdur;
  if ((sp->fit_data_amps) && (fit_data_on_open(ss)))
    {
      ymax = sp->fit_data_amps[cp->chan];
      if ((ymax == 0.0) || (ymax == FALSE)) ymax = 1.0;
      ymin = -ymax;
      xmax = gdur;
      xmin = 0.0;
      y0 = ymin; y1 = ymax; x0 = xmin; x1 = xmax;
    }
  if (dur <= 0.0)
    {
      /* empty sound */
      label = STR_no_data;
      xmin = 0.0;
      xmax = .001;
    }
  else
    {
      if (xmax > dur) xmax = dur;
      if (x0 >= x1) x0 = x1-.01;
    }
  if (xmax <= xmin) xmax = xmin+.001;
  if (ymin >= ymax) ymin = ymax-.01;
  if (y0 >= y1) y0 = y1-.01;
  ap = make_axis_info(cp,xmin,xmax,ymin,ymax,label,x0,x1,y0,y1,NULL);
  if (dur == 0)
    {
      ap->zx = 1.0;
      ap->sx = 1.0;
      ap->no_data = 1;
    }
  else
    {
      ap->zx = (ap->x1-ap->x0)/(ap->xmax-ap->xmin);
      ap->sx = (ap->x0-ap->xmin)/(ap->xmax-ap->xmin);
      ap->no_data = 0;
    }
  ap->zy = (ap->y1-ap->y0)/(ap->ymax-ap->ymin);
  ap->sy = (ap->y0-ap->ymin)/(ap->ymax-ap->ymin);
  cp->axis = ap;
  if (graphed) initialize_scrollbars(cp);

  /* our initial edit_list size will be relatively small */
  cp->edit_size = INITIAL_EDIT_SIZE;
  cp->edit_ctr = 0;
  allocate_ed_list(cp);
  cp->amp_envs = (env_info **)CALLOC(cp->edit_size,sizeof(env_info *));
  cp->samples = (int *)CALLOC(cp->edit_size,sizeof(int));
  cp->sound_size = INITIAL_EDIT_SIZE;
  cp->sound_ctr = 0;
  cp->sounds = (snd_data **)CALLOC(cp->sound_size,sizeof(snd_data *));
  cp->samples[0] = samples_per_channel;
}

void add_channel_data(char *filename, chan_info *cp, file_info *hdr, snd_state *ss)
{
  int fd,chn=0;
  int *datai;
  snd_info *sp;
  file_info *chdr;
  sp = cp->sound;
  add_channel_data_1(cp,sp,ss,1);
  set_initial_ed_list(cp,(hdr->samples/hdr->chans) - 1);
#if FILE_PER_CHAN
  /* if FILE_PER_CHAN
     check for special case here (can be in hdr or sp),
     set up individual header (sp->hdr[cp->chan] = chdr for filename = sp->filenames[cp->chan])
     open that as cp->sounds[0]
  */
  if (sp->chan_type == FILE_PER_SOUND)
    {
      chdr = copy_header(filename,sp->hdr);
      chn = cp->chan;
    }
  else
    {
      filename = sp->channel_filenames[cp->chan];
      chdr = make_file_info(filename,ss);
      cp->filename = copy_string(filename);
      cp->hdr = copy_header(filename,chdr);
      chn = 0;
    }
#else     
  chdr = copy_header(filename,sp->hdr); /* need one separate from snd_info case */
  chn = cp->chan;
#endif
  if (chdr)
    {
      fd = snd_open_read(ss,filename);
      if (fd != -1)
	{
	  mus_file_set_descriptors(fd,
				   filename,chdr->format,
				   mus_data_format_to_bytes_per_sample(chdr->format),
				   chdr->data_location,
				   chdr->chans,
				   chdr->type);
	  during_open(fd,filename,SND_OPEN_CHANNEL);
	  datai = make_file_state(fd,chdr,SND_IO_IN_FILE,chn,FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(filename,datai,
					     MUS_SAMPLE_ARRAY(datai[SND_IO_DATS+SND_AREF_HEADER_SIZE+chn]),
					     chdr,DONT_DELETE_ME,cp->edit_ctr,chn);
	  if (show_usage_stats(ss)) gather_usage_stats(cp);
	}
    }
  if (current_ed_samples(cp) > AMP_ENV_CUTOFF) start_amp_env(cp);
}

static void set_y_bounds(axis_info *ap)
{
  Float range;
  range = ap->zy*(ap->ymax - ap->ymin);
  ap->y0 = ap->ymin + ap->sy*(ap->ymax - ap->ymin);
  ap->y1 = (ap->y0 + range);
  if (ap->y1 > ap->ymax)
    {
      ap->y1 = ap->ymax;
      ap->y0 = ap->y1 - range;
    }
  if (ap->y0 < ap->ymin) ap->y0 = ap->ymin;
}

void set_x_bounds(axis_info *ap)
{
  Float range;
  if (ap->xmax <= ap->xmin) ap->xmax = ap->xmin+.001;
  range = ap->zx*(ap->xmax - ap->xmin);
  ap->x0 = ap->xmin + ap->sx*(ap->xmax - ap->xmin);
#ifdef LINUX
  if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
  ap->x1 = (ap->x0 + range);
  if (ap->x1 > ap->xmax)
    {
      ap->x1 = ap->xmax;
      ap->x0 = ap->x1 - range;
    }
  if (ap->x0 < ap->xmin) ap->x0 = ap->xmin;
}

void apply_y_axis_change (axis_info *ap, chan_info *cp)
{
  snd_info *sp;
  chan_info *ncp;
  axis_info *nap;
  int i;
  Float zy,sy;
  set_y_bounds(ap);
  update_graph(cp,NULL);
  sp = cp->sound;
  if (sp->combining != CHANNELS_SEPARATE)
    {
      sy = ap->sy;
      zy = ap->zy;
      for (i=0;i<sp->nchans;i++)
	{
	  ncp = sp->chans[i];
	  if (ncp != cp)
	    {
	      nap = ncp->axis;
	      if (nap)
		{
		  nap->zy = zy;
		  nap->sy = sy;
		  set_y_bounds(nap);
		  update_graph(ncp,NULL);
		}
	    }
	}
    }
}

static void set_x_axis_x0x1 (chan_info *cp, Float x0, Float x1) 
{
  axis_info *ap;
  ap = cp->axis;
  if (x0 >= 0.0) ap->x0 = x0; else ap->x0 = 0.0;
  ap->x1 = x1;
  if (x1 > ap->xmax) ap->xmax = x1;
  if (ap->xmax <= ap->xmin) ap->xmax = ap->xmin+.001;
  ap->zx = (x1-x0)/(ap->xmax-ap->xmin);
  ap->sx = (x0-ap->xmin)/(ap->xmax-ap->xmin);
  resize_sx(cp);
  resize_zx(cp);
  apply_x_axis_change(ap,cp,cp->sound); /* this checks sync */
}

static void set_x_axis_x0(chan_info *cp, int left)
{
  Float x1x0;
  axis_info *ap;
  if (cp)
    {
      ap = cp->axis; 
      if (ap) 
	{
	  x1x0 = ap->x1 - ap->x0; 
	  ap->x0 = (double)left / (double)SND_SRATE(cp->sound); 
#ifdef LINUX
	  if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
	  set_x_axis_x0x1(cp,ap->x0,ap->x0 + x1x0);
	}
    }
}

static void set_x_axis_x1(chan_info *cp, int right)
{
  Float x1x0;
  axis_info *ap;
  if (cp)
    {
      ap = cp->axis; 
      if (ap) 
	{
	  x1x0 = ap->x1 - ap->x0; 
	  ap->x1 = (double)right / (double)SND_SRATE(cp->sound); 
	  set_x_axis_x0x1(cp,ap->x1 - x1x0,ap->x1);
	}
    }
}

static void set_y_axis_y0y1 (chan_info *cp, Float y0, Float y1) 
{
  axis_info *ap;
  ap = cp->axis;
  if (y0 < ap->ymin) ap->ymin = y0;
  if (y1 > ap->ymax) ap->ymax = y1;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->zy = (y1-y0)/(ap->ymax-ap->ymin);
  ap->sy = (y0-ap->ymin)/(ap->ymax-ap->ymin);
  resize_sy(cp);
  resize_zy(cp);
  apply_y_axis_change(ap,cp);
}

static void reset_x_display(chan_info *cp, double sx, double zx)
{
  axis_info *ap;
  ap=cp->axis;
  ap->sx = sx;
  ap->zx = zx;
  set_x_bounds(ap);
  resize_sx(cp);
  resize_zx(cp);
  update_graph(cp,NULL);
}

static void update_xs(chan_info *ncp, axis_info *ap)
{
  Float scl;
  axis_info *nap;
  nap = ncp->axis;
  if (nap->xmax > 0.0)
    {
      scl = ap->xmax/nap->xmax;
      reset_x_display(ncp,ap->sx*scl,ap->zx*scl);
    }
}

void apply_x_axis_change(axis_info *ap, chan_info *cp, snd_info *sp)
{
  int i;
  sync_info *si;
  si = NULL;
  sp->lacp = cp;
  set_x_bounds(ap);
  update_graph(cp,NULL);
  if (sp->syncing != 0)
    {
      si = snd_sync(cp->state,sp->syncing);
      for (i=0;i<si->chans;i++) 
	{
	  if (cp != si->cps[i]) update_xs(si->cps[i],ap);
	}
      free_sync_info(si);
    }
  else 
    {
      if (sp->combining != CHANNELS_SEPARATE)
	{
	  for (i=1;i<sp->nchans;i++) update_xs(sp->chans[i],ap);
	}
    }
}

static int visible_syncd_cursor(chan_info *cp)
{
  snd_info *sp;
  snd_state *ss;
  chan_info *ncp;
  int i,j,sync;
  sp = cp->sound;
  sync = sp->syncing;
  if (sync != 0)
    {
      for (i=0;i<sp->nchans;i++)
	{
	  ncp = sp->chans[i];
	  if (ncp->cursor_visible) return(ncp->cursor);
	}
      /* geez, maybe it's in a separate syncd sound */
      ss = cp->state;
      for (j=0;j<ss->max_sounds;j++)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse) && (sp->syncing == sync) && (sp != cp->sound))
	    {
	      for (i=0;i<sp->nchans;i++)
		{
		  ncp = sp->chans[i];
		  if (ncp->cursor_visible) return(ncp->cursor);
		}
	    }
	}
    }
  return(-1);
}

void focus_x_axis_change(axis_info *ap, chan_info *cp, snd_info *sp, int focus_style)
{
  /* prepare for set_x_bounds given desired focus point, then drop into default
   * we need to set ap->sx to reflect the new zx value and the focus type
   * if focus_left - go on (nothing to do)
   *    focus_right - get old right, set sx to keep it as is
   *    focus_middle - ditto mid window
   *    focus_active - find the currently active entity, if none use focus_middle 
   */
  chan_info *ncp;
  int newf;
  Float loc,pos;
  if (ap->xmax == 0.0) return;
  if (ap->xmax <= ap->xmin) ap->xmax = ap->xmin+.001;
  if (focus_style != FOCUS_LEFT)
    {
      switch (focus_style)
	{
	case FOCUS_RIGHT:   ap->x0 = ap->x1 - ap->zx*(ap->xmax - ap->xmin); break;
	case FOCUS_MIDDLE:  ap->x0 = 0.5 * ((ap->x1 + ap->x0) - ap->zx*(ap->xmax - ap->xmin)); break;
	case FOCUS_ACTIVE:
	  ncp = virtual_selected_channel(cp);
	  /* axes should be the same, since all move together in this mode */
	  if (ncp->cursor_visible)
	    newf = ncp->cursor;
	  else
	    {
	      /* perhaps user has syncd chans (sounds), has cursor in only one, is zooming using some other channel's (sound's) slider */
	      newf = visible_syncd_cursor(cp);
	      if (newf == -1)
		{
		  if (active_selection(ncp)) 
		    newf = selection_beg(ncp);
		  else
		    if (active_mix(ncp))
		      newf = mix_beg(ncp);
		    else
		      if (active_mark(ncp))
			newf = mark_beg(ncp);
		      else newf = -1;
		}
	    }
	  if (newf != -1)
	    {
	      loc = (double)newf/(double)SND_SRATE(ncp->sound);
	      if ((loc > ap->x0) && (loc < ap->x1) && (ap->x1 > ap->x0))
		/* try to maintain current relative position in window */
		{
		  pos = (loc - ap->x0)/(ap->x1 - ap->x0);
		  ap->x0 = loc - pos*ap->zx*(ap->xmax - ap->xmin);
		}
	      else ap->x0 = loc - 0.5*ap->zx*(ap->xmax - ap->xmin);
	    }
	  break;
	}
#ifdef LINUX
      if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
      if (ap->x0 < 0.0) ap->x0 = 0.0;
      ap->sx = (Float)(ap->x0 - ap->xmin) / (Float)(ap->xmax - ap->xmin);
    }
  apply_x_axis_change(ap,cp,sp);
}

void sx_incremented(chan_info *cp, double amount)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sx += (ap->zx*amount);
  if (ap->sx < 0.0) ap->sx = 0.0;
  if (ap->sx > (1.0-ap->zx)) ap->sx = 1.0 - ap->zx;
  apply_x_axis_change(ap,cp,cp->sound);
  resize_sx(cp);
}

static void zx_incremented(chan_info *cp, double amount)
{ /* kbd arrows etc -- needs to be able to return to original */
  axis_info *ap;
  int samps;
  samps = current_ed_samples(cp);
  ap = cp->axis;
  if ((amount >= 1.0) || ((samps > 0) && (ap->zx > (1.0 / (double)samps))))
    {
      ap->zx *= amount;
      focus_x_axis_change(ap,cp,cp->sound,zoom_focus_style(cp->state));
      /* apply_x_axis_change(ap,cp,cp->sound); */
      resize_sx(cp);
      resize_zx(cp);
    }
}

int move_axis(chan_info *cp, axis_info *ap, int x)
{
  /* need to scroll axis forward or backward -- distance per call depends on x distance from end of axis */
  Float off;
  int nx;
  if (x > ap->x_axis_x1)
    {
      off = x - ap->x_axis_x1;
      ap->sx += (off*ap->zx/1000.0);
      if (ap->sx > (1.0-ap->zx)) ap->sx = 1.0 - ap->zx;
      nx = ap->x_axis_x1;
    }
  else
    {
      off = ap->x_axis_x0 - x;
      ap->sx -= (off*ap->zx/1000.0);
      if (ap->sx < 0.0) ap->sx = 0.0;
      nx = ap->x_axis_x0;
    }
  apply_x_axis_change(ap,cp,cp->sound);
  resize_sx(cp);
  return(nx);
}

void set_axes(chan_info *cp,Float x0,Float x1,Float y0,Float y1)
{
  axis_info *ap;
  ap = cp->axis;
  ap->x0 = x0;
  ap->x1 = x1;
  ap->zx = (x1-x0)/(ap->xmax-ap->xmin);
  ap->sx = (x0-ap->xmin)/(ap->xmax-ap->xmin);
  resize_sx(cp);
  resize_zx(cp);
  ap->y0 = y0;
  ap->y1 = y1;
  ap->zy = (y1-y0)/(ap->ymax-ap->ymin);
  ap->sy = (y0-ap->ymin)/(ap->ymax-ap->ymin);
  resize_sy(cp);
  resize_zy(cp);
}

void set_xy_bounds(chan_info *cp,axis_info *ap)
{
  set_y_bounds(ap);
  resize_sy(cp);
  set_x_bounds(ap);
  resize_sx(cp);
}


/* ---------------- CHANNEL GRAPHICS ---------------- */

static void display_zero (chan_info *cp)
{
  axis_info *ap;
  short zero;
  ap = cp->axis;
  if ((ap->y0 < 0.0) && (ap->y1 > 0.0))
    {
      zero = grf_y(0.0,ap);
      draw_line(copy_context(cp),ap->x_axis_x0,zero,ap->x_axis_x1,zero);
      if (cp->printing) ps_draw_line(cp,ap->x_axis_x0,0,ap->x_axis_x1,0);
    }
}

static char chn_id_str[32];

static void display_channel_id (chan_info *cp, int height, int chans)
{
  int x0,y0;
  if ((chans>1) || (cp->edit_ctr > 0))
    {
      set_peak_numbers_font(cp);
      if (cp->printing) ps_set_peak_numbers_font(cp);
      x0 = 5;
      y0 = height - 5;
      if (chans > 1)
	{
	  if (cp->edit_ctr == 0)
	    sprintf(chn_id_str,"%s%d",STR_channel_id,(cp->chan+1)); /* cp chan numbers are 0 based to index sp->chans array */
	  else sprintf(chn_id_str,"%s%d:(%d)",STR_channel_id,(cp->chan+1),cp->edit_ctr);
	}
      else sprintf(chn_id_str,"(%d)",cp->edit_ctr);
      draw_string(copy_context(cp),x0,y0,chn_id_str,strlen(chn_id_str));
      if (cp->printing) ps_draw_string(cp,x0,y0,chn_id_str);
    }
}

static void display_selection_fft_size (chan_info *cp, axis_info *fap)
{
  int x0,y0;
  if (fap->height < 60) return;
  set_tiny_numbers_font(cp);
  if (cp->printing) ps_set_tiny_numbers_font(cp);
  y0 = fap->height + fap->y_offset - 3;
  x0 = fap->x_axis_x0 + 10;
  sprintf(chn_id_str,"(len: %d/%d)",region_len(0),cp->selection_transform_size);
  draw_string(copy_context(cp),x0,y0,chn_id_str,strlen(chn_id_str));
  if (cp->printing) ps_draw_string(cp,x0,y0,chn_id_str);
}

static void make_wavogram(chan_info *cp, snd_info *sp, snd_state *ss);

int make_graph(chan_info *cp, snd_info *sp, snd_state *ss)
{
  /* axes are already set, determining the data we will show -- do we need explicit clipping ? */
  int i,j=0,samps,xi;
  axis_info *ap;
  Float samples_per_pixel,xf,pinc = 0.0;
  double x,incr;  
  /* in long files with small windows we can run into floating point errors that accumulate
   * in the loop (incrementing by a truncated amount each time), making the x amounts smaller
   * than they should be (so the graph appears squeezed).
   *
   * There is a similar problem with exceedingly long files (say 1 hour at 44KHz), in which 
   * the x increment gets quantized making the graphs step-like, so the
   * axis_info fields x0, x1, and x_scale are doubles as well. The y axis is less problematic
   * since we are assuming sound files here.
   *
   * And if the data window itself is small (in the sense that we're looking at individual samples)
   * we need to make decisions about in-between sample mouse clicks, and the cursor placement
   * must match the sample placement (which means that the initial x-axis offset from the
   * first sample (similarly the last) must be taken into account).  Currently, the selection
   * definition (by mouse drag) has a sloppy rounding mechanism (snd-clip.c round_up) so that
   * within reason only samples within the selection-rectangle are in the final selection,
   * and cursor placement (mouse click) rounds to the nearest sample (snd-xchn.c cursor_round).
   *
   * And lastly, the axis x_scale double can be enormous whereas the between-sample choice can
   * be tiny, so we're pushing the arithmetic into dangerous realms.  This stuff has been tested
   * on some extreme cases (hour-long 44KHz stereo), but there's always another special case...
   */
  MUS_SAMPLE_TYPE ina,ymin,ymax;
  int pixels;
  snd_fd *sf = NULL;
  int x_start,x_end;
  double start_time=0.0,cur_srate=1.0;
  axis_context *ax=NULL;
  chan_context *cgx;
  env_info *ep;
  if (wavo(ss)) {make_wavogram(cp,sp,ss); return(0);}
  ap = cp->axis;
  /* check for no graph */
  if ((!ap) || (!(ap->graph_active)) || (ap->x0 == ap->x1)) return(0);
  if (sp)
    {
      cur_srate = (double)SND_SRATE(sp);
      ap->losamp = (int)(ap->x0*cur_srate);
      if (ap->losamp < 0) ap->losamp = 0;
      if (ap->x0 != (ap->losamp/cur_srate)) ap->losamp++;
      start_time = (double)(ap->losamp)/cur_srate;
      ap->hisamp = (int)(ap->x1*cur_srate);
      if ((ap->losamp == 0) && (ap->hisamp == 0)) return(0);
    }
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = ap->hisamp - ap->losamp+1;
  if ((x_start == x_end) && (samps > 10)) return(0); /* must be too-tiny graph */
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE-1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01; /* any non-zero value < 1.0 should be ok here */
  else samples_per_pixel = (Float)(samps-1)/(Float)pixels;
  allocate_grf_points();
  if (cp->printing) ps_allocate_grf_points();
  if (sp)
    {
      if (sp->combining == CHANNELS_SUPERIMPOSED) 
	{
	  ax = combined_context(cp); 
	  if (cp->printing) ps_recolor(cp);
	}
      else ax = copy_context(cp);
    }
  if (samples_per_pixel < 1.0)
    {
      sf = init_sample_read(ap->losamp,cp,READ_FORWARD);
      /* i.e. individual samples are widely spaced */
      incr = 1.0 / samples_per_pixel;
      for (j=0,i=ap->losamp,x=x_start;i<=ap->hisamp;i++,j++,x+=incr)
	{
	  NEXT_SAMPLE(ina,sf);
	  set_grf_point((int)x,j,grf_y(MUS_SAMPLE_TO_FLOAT(ina),ap));

	  /* for colored lines (mix as green for example), we'd check sf->cb[ED_COLOR],
	   * if it has changed, send out the current points in the current color,
	   * change to the new color.
	   * mix groups by color could OR together base colors -- could be based
	   * on current color map.
	   * lisp edits could specifiy any color etc.
	   * need map from cb[ED_COLOR] to Pixel (or could it be the index itself?)
	   */

	  /* to show fft extent, losamp to losamp+fft_size min hisamp in fft-color */

	  if (cp->printing) ps_set_grf_point((double)i/cur_srate,j,MUS_SAMPLE_TO_FLOAT(ina));
	}
      if (sp)
	{
	  draw_grf_points(ss,ax,j,ap,0.0);
	  if (cp->printing) ps_draw_grf_points(ss,cp,ap,j,0.0);
	}
    }
  else
    {
      if ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE))
	{
	  sf = init_sample_read(ap->losamp,cp,READ_FORWARD);
	  incr = (double)1.0 /cur_srate;
	  for (j=0,i=ap->losamp,x=start_time;i<=ap->hisamp;i++,j++,x+=incr)
	    {
	      NEXT_SAMPLE(ina,sf);
	      set_grf_point(grf_x(x,ap),j,grf_y(MUS_SAMPLE_TO_FLOAT(ina),ap));
	      if (cp->printing) ps_set_grf_point(x,j,MUS_SAMPLE_TO_FLOAT(ina));
	    }
	  if (sp)
	    {
	      draw_grf_points(ss,ax,j,ap,0.0);
	      if (cp->printing) ps_draw_grf_points(ss,cp,ap,j,0.0);
	    }
	}
      else
	{
	  /* take min, max */
	  if (amp_env_usable(cp,samples_per_pixel,ap->hisamp))
	    j = amp_env_graph(cp,ap,samples_per_pixel,(sp) ? ((int)SND_SRATE(sp)) : 1);
	  else
	    {
	      if ((ap->hisamp - ap->losamp) > (current_ed_samples(cp)/4))
		{                                /* we're trying to view a large portion of the (large) sound */
		  cgx = cp->cgx;
		  if (cgx->amp_env_in_progress)
		    {                            /* but the amp-env background process is still working on it */
		      ep = cp->amp_envs[cp->edit_ctr];
		      if ((ep) && samples_per_pixel >= (Float)(ep->samps_per_bin))
			{                        /* and it will be useful when it finishes */
			  cp->waiting_to_make_graph = 1;
			  return(0);             /* so don't run two enormous data readers in parallel */
			}
		    }
		}
	      sf = init_sample_read(ap->losamp,cp,READ_FORWARD);
	      j = 0;      /* graph point counter */
	      x=ap->x0;
	      xi=grf_x(x,ap);
	      xf=0.0;     /* samples per pixel counter */
	      ymin = MUS_MIX_MAX;
	      ymax = MUS_MIX_MIN;
	      if (cp->printing) pinc = samples_per_pixel/cur_srate;
	      for (i=ap->losamp,xf=0.0;i<=ap->hisamp;i++)
		{
		  NEXT_SAMPLE(ina,sf);
		  if (ina > ymax) ymax = ina;
		  if (ina < ymin) ymin = ina;
		  xf+=1.0;
		  if (xf>samples_per_pixel)
		    {
		      set_grf_points(xi,j,grf_y(MUS_SAMPLE_TO_FLOAT(ymin),ap),grf_y(MUS_SAMPLE_TO_FLOAT(ymax),ap));
		      if (cp->printing) {x+=pinc; ps_set_grf_points(x,j,MUS_SAMPLE_TO_FLOAT(ymin),MUS_SAMPLE_TO_FLOAT(ymax));}
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      ymin = MUS_MIX_MAX;
		      ymax = MUS_MIX_MIN;
		    }
		}
	    }
	  if (sp)
	    {
	      draw_both_grf_points(ss,ax,j,ap);
	      if (cp->printing) ps_draw_both_grf_points(ss,cp,ap,j);
	    }
	}
    }
  if (sf) {free_snd_fd(sf); sf = NULL;}
  if ((sp) && (sp->combining == CHANNELS_SUPERIMPOSED))
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing) ps_reset_color(cp);
    }
  return(j);
}

static fft_peak *peak_freqs = NULL;
static fft_peak *peak_amps = NULL;

void set_max_fft_peaks(snd_state *ss, int n)
{
  if ((peak_freqs != NULL) && (max_fft_peaks(ss) < n))
    {
      peak_freqs = (fft_peak *)REALLOC(peak_freqs,n * sizeof(fft_peak));
      peak_amps = (fft_peak *)REALLOC(peak_amps,n * sizeof(fft_peak));
    }
  in_set_max_fft_peaks(ss,n);
}

static int compare_peak_amps(const void *pk1, const void *pk2)
{
  if (((fft_peak *)pk1)->amp > ((fft_peak *)pk2)->amp) return(-1);
  else if (((fft_peak *)pk1)->amp == ((fft_peak *)pk2)->amp) return(0);
  return(1);
}

static short linear_or_log_x(Float x, Float scaler, axis_info *fap, snd_state *ss)
{
  if (fft_log_frequency(ss)) return(grf_x(log(x+1.0)*scaler,fap)); else return(grf_x(x,fap));
}

static short linear_or_log_y(Float py, axis_info *fap, snd_state *ss)
{
  if (fft_log_magnitude(ss)) py = dB(ss,py);
  return(grf_y(py,fap));
}

static Float ps_linear_or_log_x(Float x, Float scaler, snd_state *ss)
{
  if (fft_log_frequency(ss)) return(log(x+1.0)*scaler); else return(x);
}

static Float ps_linear_or_log_y(Float py, snd_state *ss)
{
  if (fft_log_magnitude(ss)) return(dB(ss,py)); else return(py);
}

static char ampstr[8];
#define AMP_ROOM 35
#define AMP_ROOM_CUTOFF 3.0

#define LOG_FACTOR 25.0
/* determines how we view the log */

static void display_peaks(chan_info *cp,axis_info *fap,Float *data,int scaler,int samps,Float samps_per_pixel,int fft_data, Float fft_scale)
{
  snd_state *ss;
  int num_peaks,row,col,tens,i,with_amps,acol,acols;
  Float amp0,px;
  char *fstr;
  ss = cp->state;
  if (peak_freqs == NULL) peak_freqs = (fft_peak *)CALLOC(max_fft_peaks(ss),sizeof(fft_peak));
  if (peak_amps == NULL) peak_amps = (fft_peak *)CALLOC(max_fft_peaks(ss),sizeof(fft_peak));
  if (samps > (scaler*10)) tens = 2; else if (samps > scaler) tens = 1; else if (samps > (scaler/10)) tens = 0; else tens = -1;
  num_peaks = (fap->y_axis_y0-fap->y_axis_y1) / 20;
  if (num_peaks <= 0) return;
  if (num_peaks > max_fft_peaks(ss)) num_peaks = max_fft_peaks(ss);
  if (fft_data)
    num_peaks = find_and_sort_fft_peaks(data,peak_freqs,num_peaks,samps,1,samps_per_pixel,fft_scale); /* srate 1.0=>freqs between 0 and 1.0 */
  else num_peaks = find_and_sort_peaks(data,peak_freqs,num_peaks,samps);
  if ((num_peaks == 1) && (peak_freqs[0].freq == 0.0)) return;
  with_amps = (fap->width > ((30+5*tens+AMP_ROOM)*AMP_ROOM_CUTOFF));
  acols = 3;
  col = fap->x_axis_x1 - 30 - tens*5; 
  /* in some cases (lisp graph with integer peak "freqs") this can move the freq column over too far */
  acol = fap->x_axis_x1-AMP_ROOM+15;
  if (with_amps) 
    {
      col -= AMP_ROOM;
      if ((fft_data) && (!(normalize_fft(ss))))
	{
	  col -= 5;
	  acol -= 5;
	  acols = 4;
	}
    }
  if (num_peaks > 6)
    {
      for (i=0;i<num_peaks;i++) peak_amps[i]=peak_freqs[i];
      qsort((void *)peak_amps,num_peaks,sizeof(fft_peak),compare_peak_amps);
      if (num_peaks < 12) amp0 = peak_amps[2].amp; else amp0 = peak_amps[5].amp;
      set_bold_peak_numbers_font(cp);
      if (cp->printing) ps_set_bold_peak_numbers_font(cp);
      row = fap->y_axis_y1 + 15;
      for (i=0;i<num_peaks;i++)
	{
	  if (peak_freqs[i].amp >= amp0)
	    {
	      px = peak_freqs[i].freq;
	      fstr = prettyf(px*scaler,tens);
	      draw_string(copy_context(cp),col,row,fstr,strlen(fstr));
	      if (cp->printing) ps_draw_string(cp,col,row,fstr);
	      FREE(fstr);
	      fstr=NULL;
	      if (with_amps)
		{
		  if ((fft_data) && (fft_log_magnitude(ss)))
		    sprintf(ampstr,"%.1f",dB(ss,peak_freqs[i].amp));
		  else sprintf(ampstr,"%.*f",acols,peak_freqs[i].amp);
		  draw_string(copy_context(cp),acol,row,ampstr,strlen(ampstr));
		  if (cp->printing) ps_draw_string(cp,acol,row,ampstr);
		}
	    }
	  row+=15;
	}
    }
  else amp0 = 100.0;
  set_peak_numbers_font(cp);
  if (cp->printing) ps_set_peak_numbers_font(cp);
  /* choose a small font for these numbers */
  row = fap->y_axis_y1 + 15;
  for (i=0;i<num_peaks;i++)
    {
      if (peak_freqs[i].amp < amp0)
	{
	  px = peak_freqs[i].freq;
	  fstr = prettyf(px*scaler,tens);
	  draw_string(copy_context(cp),col,row,fstr,strlen(fstr));
	  if (cp->printing) ps_draw_string(cp,col,row,fstr);
	  FREE(fstr);
	  fstr=NULL;
	  if (with_amps)
	    {
	      if ((fft_data) && (fft_log_magnitude(ss)))
		sprintf(ampstr,"%.1f",dB(ss,peak_freqs[i].amp));
	      else sprintf(ampstr,"%.*f",acols,peak_freqs[i].amp);
	      draw_string(copy_context(cp),acol,row,ampstr,strlen(ampstr));
	      if (cp->printing) ps_draw_string(cp,acol,row,ampstr);
	    }
	}
      row+=15;
    }
}

static void make_fft_graph(chan_info *cp, snd_info *sp, snd_state *ss)
{
  /* axes are already set, data is in the fft_info struct */
  /* since the fft size menu callback can occur while we are calculating the next fft, we have to lock the current size until the graph goes out */
  fft_info *fp,*nfp;
  axis_info *fap;
  axis_context *ax;
  Float *data,*tdata;
  chan_info *ncp;
  Float incr,x,scale;
  int i,j,xi,samps,losamp=0,di;
  Float samples_per_pixel,xf,ina,ymax,scaler,data_max;
  fp = cp->fft;
  if (chan_fft_in_progress(cp)) return;
  fap = fp->axis;
  if (!fap->graph_active) return;
  data = fp->data;
  if (transform_type(ss) == FOURIER)
    {
      samps = (int)(fp->current_size*spectro_cutoff(ss)/2);
      losamp = (int)(fp->current_size*spectro_start(ss)/2);
      incr = (Float)SND_SRATE(sp)/(Float)(fp->current_size);
    }
  else
    {
      /* samps here is in terms of transform values, not original sampled data values */
      samps = (int)(fp->current_size * spectro_cutoff(ss));
      losamp = (int)(fp->current_size * spectro_start(ss));
      incr = 1.0;
    }

  data_max = 0.0;
  if ((!(normalize_fft(ss))) && (sp->nchans > 1) && (sp->combining == CHANNELS_SUPERIMPOSED))
    {
      for (j=0;j<sp->nchans;j++)
	{
	  ncp = sp->chans[j];
	  nfp = ncp->fft;
	  tdata = nfp->data;
	  for (i=losamp;i<samps;i++) if (tdata[i]>data_max) data_max=tdata[i];
	}
    }
  else
    {
      if (transform_type(ss) == FOURIER)
	{
	  for (i=losamp;i<samps;i++) if (data[i]>data_max) data_max=data[i];
	}
      else
	{
	  for (i=losamp;i<samps;i++)
	    {
	      if (data[i]>data_max) 
		data_max=data[i];
	      else
		if (data[i]<-data_max) 
		  data_max=-data[i];
	    }
	}
    }
  if (data_max == 0.0) data_max = 1.0;
  if (normalize_fft(ss))
    scale = 1.0/data_max;
  else 
    {
      if (transform_type(ss) == FOURIER)
	{
	  scale = 1.0/(Float)samps;
	  di = (int)(10*data_max*scale + 1);
	  if (di == 1)
	    {
	      di = (int)(100*data_max*scale + 1);
              if (di == 1)
		{
		  di = (int)(1000*data_max*scale + 1);
		  data_max = (Float)di/1000.0;
		}
	      else data_max = (Float)di/100.0;
	    }
	  else data_max = (Float)di/10.0;
	}
      else 
	{
	  scale = 1.0;
	  fap->y0 = -data_max;
	  fap->ymin = -data_max;
	}
      fap->y1 = data_max;
      fap->ymax = data_max;
    }
  fp->scale = scale;
  allocate_grf_points();
  if (cp->printing) ps_allocate_grf_points();
  samples_per_pixel = (Float)(samps - losamp)/(Float)(fap->x_axis_x1-fap->x_axis_x0);
  if (sp->combining == CHANNELS_SUPERIMPOSED) 
    {
      ax = combined_context(cp); 
      if (cp->printing) ps_recolor(cp);
    }
  else ax = copy_context(cp);
  if (samples_per_pixel < 4.0)
    {
      if ((!fft_log_magnitude(ss)) && (!fft_log_frequency(ss)))
	{
	  if (losamp == 0)
	    {
	      for (i=0,x=0.0;i<samps;i++,x+=incr)
		{
		  set_grf_point(grf_x(x,fap),i,grf_y(data[i]*scale,fap));
		  if (cp->printing) ps_set_grf_point(x,i,data[i]*scale);
		}
	    }
	  else
	    {
	      for (i=losamp,x=fap->x0;i<samps;i++,x+=incr)
		{
		  set_grf_point(grf_x(x,fap),i-losamp,grf_y(data[i]*scale,fap));
		  if (cp->printing) ps_set_grf_point(x,i-losamp,data[i]*scale);
		}
	    }
	}
      else
	{
	  if (fft_log_frequency(ss)) 
	    {
	      ymax = LOG_FACTOR;
	      incr = ymax/(Float)(samps - losamp);
	      scaler = 1.0/log(ymax+1.0);
	    }
	  else scaler = 0.0;
	  for (i=losamp,x=fap->x0;i<samps;i++,x+=incr)
	    {
	      set_grf_point(linear_or_log_x(x,scaler,fap,ss),i-losamp,linear_or_log_y(data[i]*scale,fap,ss));
	      if (cp->printing) ps_set_grf_point(ps_linear_or_log_x(x,scaler,ss),i-losamp,ps_linear_or_log_y(data[i]*scale,ss));
	    }
	}
      draw_grf_points(ss,ax,i-losamp,fap,0.0);
      if (cp->printing) ps_draw_grf_points(ss,cp,fap,i-losamp,0.0);
    }
  else
    {
      /* take max -- min not interesting here */
      j = 0;      /* graph point counter */
      i=losamp;
      if (losamp == 0) x = 0.0; else x = fap->x0;
      xi=grf_x(x,fap);
      xf=0.0;     /* samples per pixel counter */
      if (fft_log_frequency(ss)) 
	{
	  ymax = LOG_FACTOR;
	  incr = ymax/(Float)(samps - losamp);
	  scaler = 1.0/log(ymax+1.0);
	}
      else scaler = 0.0;
      ymax=-1.0;
      if ((!fft_log_magnitude(ss)) && (!fft_log_frequency(ss)))
	{
	  while (i<samps)
	    {
	      ina = data[i];
	      if (ina > ymax) ymax = ina;
	      xf+=1.0;
	      i++;
	      if (xf>samples_per_pixel)
		{
		  set_grf_point(xi,j,grf_y(ymax*scale,fap));
		  if (cp->printing) {x+=(incr*samples_per_pixel); ps_set_grf_point(x,j,ymax*scale);}
		  xi++;
		  j++;
		  xf -= samples_per_pixel;
		  ymax=-1.0;
		}
	    }
	}
      else
	{
	  while (i<samps)
	    {
	      ina = data[i];
	      if (ina > ymax) ymax = ina;
	      xf+=1.0;
	      i++;
	      if (xf>samples_per_pixel)
		{
		  set_grf_point(linear_or_log_x(x,scaler,fap,ss),j,linear_or_log_y(ymax*scale,fap,ss));
		  if (cp->printing) ps_set_grf_point(ps_linear_or_log_x(x,scaler,ss),j,ps_linear_or_log_y(ymax*scale,ss));
		  x+=(incr*samples_per_pixel);
		  j++;
		  xf -= samples_per_pixel;
		  ymax=-1.0;
		}
	    }
	}
      draw_grf_points(ss,ax,j,fap,0.0);
      if (cp->printing) ps_draw_grf_points(ss,cp,fap,j,0.0);
    }
  if (sp->combining == CHANNELS_SUPERIMPOSED) 
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing) ps_reset_color(cp);
    }
  if (show_fft_peaks(ss)) 
    {
      if (transform_type(ss) == FOURIER)
	display_peaks(cp,fap,data,(int)(SND_SRATE(sp)*spectro_cutoff(ss)/2),samps,samples_per_pixel,1,scale);
      else display_peaks(cp,fap,data,(int)(fp->current_size*spectro_cutoff(ss)),samps,samples_per_pixel,1,0.0);
    }
  if (cp->selection_transform_size != 0) display_selection_fft_size(cp,fap);
  if (cp->hookable) after_fft(ss,cp,scale);
}

static int display_fft_peaks(chan_info *ucp, char *filename)
{
  /* put (sync'd) peak info in help window */
  char *str = NULL,*mcf = NULL;
  fft_info *fp;
  axis_info *fap,*ap;
  snd_state *ss;
  snd_info *sp;
  Float *data;
  Float srate2;
  time_t ts;
  char *timbuf;
  FILE *fd = NULL;
  int i,chn,samps,num_peaks,tens,srate,err = 0,tmp_file = 1,chars;
  Float samples_per_pixel;
  sync_info *si = NULL;
  chan_info *cp;
  ss = ucp->state;
  sp = ucp->sound;
  if (sp->syncing != 0)
    si = snd_sync(ss,sp->syncing);
  else si = make_simple_sync(ucp,0);
  if (peak_freqs == NULL) peak_freqs = (fft_peak *)CALLOC(max_fft_peaks(ss),sizeof(fft_peak));
  if (peak_amps == NULL) peak_amps = (fft_peak *)CALLOC(max_fft_peaks(ss),sizeof(fft_peak));
  if ((filename) && (snd_strlen(filename) > 0))
    {
      fd = fopen(mcf = mus_file_full_name(filename),"w");
      if (mcf) FREE(mcf);
      if (fd == NULL) 
	{
	  str = (char *)CALLOC(256,sizeof(char));
	  sprintf(str,STR_cant_write_p,filename,strerror(errno));
	  report_in_minibuffer(sp,str);
	  err = 1;
	  FREE(str);
	  str = NULL;
	}
      else tmp_file = 0;
    }
  if (tmp_file == 1)
    {
      filename = snd_tempnam(ss);
      fd = fopen(filename,"w");
    }
  if (fd) 
    {
#if (!defined(HAVE_CONFIG_H)) || defined(HAVE_STRFTIME)
      timbuf = (char *)CALLOC(TIME_STR_SIZE,sizeof(char));
      time(&ts);
      strftime(timbuf,TIME_STR_SIZE,STRFTIME_FORMAT,localtime(&ts));
      fprintf(fd,"Snd: fft peaks (%s)\n\n",timbuf);
      FREE(timbuf);
#else
      fprintf(fd,"Snd: fft peaks (%s)\n\n");
#endif
      for (chn=0;chn<si->chans;chn++)
	{
	  cp = si->cps[chn];
	  fp = cp->fft;
	  if (fp)
	    {
	      fap = fp->axis;
	      if ((fap) && (fap->graph_active))
		{
		  ap = cp->axis;
		  sp = cp->sound;
		  data = fp->data;
		  samps = fp->current_size/2;
		  samples_per_pixel = (Float)samps/(Float)(fap->x_axis_x1-fap->x_axis_x0);
		  srate = SND_SRATE(sp);
		  srate2 = (Float)srate * .5;
		  if (samps > (5*srate)) tens = 2; else if (samps > (int)srate2) tens = 1; else if (samps > (srate/20)) tens = 0; else tens = -1;
		  num_peaks = find_and_sort_fft_peaks(data,peak_freqs,max_fft_peaks(ss),samps,1,samples_per_pixel,fp->scale);
		  if ((num_peaks != 1) || (peak_freqs[0].freq != 0.0))
		    {
		      fprintf(fd,sp->shortname);
		      if (sp->nchans > 1) fprintf(fd,": chan %d",cp->chan);
		      fprintf(fd,", fft %d points beginning at sample %d (%.3f secs)\n\n",fp->current_size,ap->losamp,(Float)(ap->losamp)/(Float)srate);
		      for (i=0;i<num_peaks;i++)
			fprintf(fd,"  %.*f  %.5f\n",tens,peak_freqs[i].freq * srate2,peak_freqs[i].amp); 
		      fprintf(fd,"\n");
		    }
		}
	    }
	}
      fclose(fd);
      if (tmp_file)
	{
	  fd = fopen(filename,"r");
	  fseek(fd, 0, SEEK_END);
	  chars = ftell(fd);
	  rewind(fd);
	  str = (char *)CALLOC(chars+1,sizeof(char));
	  fread(str, 1, chars, fd);
	  fclose(fd);
	  snd_help(ss,"fft peaks",str);
	  FREE(str);
	  remove(filename);
	}
    }
  if (si) free_sync_info(si);
  return(err);
}

static Float skew_color(Float x, Float base)
{
  if ((base <= 0.0) || (base == 1.0)) return(x);
  return((pow(base,x)-1.0)/(base-1.0));
}

static int js[GRAY_SCALES];

static void make_sonogram(chan_info *cp, snd_info *sp, snd_state *ss)
{ /* colormap allocated in snd-fft via allocate_sono_rects in snd-xchn */
  sono_info *si;
  int i,slice,fwidth,fheight,rectw,recth,j,bins;
  fft_info *fp;
  axis_info *fap;
  Float *fdata;
  Float binval,xf,xfincr,yf,yfincr,scaler = 0.0,frectw,frecth,xscl,scl = 1.0;
  Float *hfdata;
  int *hidata;
  if (chan_fft_in_progress(cp)) return;
  si = (sono_info *)(cp->sonogram_data);
  if ((si) && (si->scale > 0.0))
    {
      bins = (int)(si->target_bins * spectro_cutoff(ss));
      allocate_grf_points();
      if (cp->printing) ps_allocate_grf_points();
      if (fft_log_frequency(ss)) scaler = 1.0/log(LOG_FACTOR+1.0);
      scl = si->scale; 
      fp = cp->fft;
      fap = fp->axis;
      fwidth = fap->x_axis_x1 - fap->x_axis_x0;
      fheight = fap->y_axis_y0 - fap->y_axis_y1;
      frectw = (Float)fwidth/(Float)(si->target_slices);
      frecth = (Float)fheight/(Float)bins;
      xscl = (Float)(fap->x1 - fap->x0)/(Float)(si->target_slices);
      rectw = (int)(ceil(frectw));
      recth = (int)(ceil(frecth));
      if (rectw==0) rectw=1;
      if (recth==0) recth=1;
      hfdata = (Float *)CALLOC(bins+1,sizeof(Float));
      hidata = (int *)CALLOC(bins+1,sizeof(int));
      if (transform_type(ss) == FOURIER)
	{
	  if (fft_log_frequency(ss))
	      yfincr = (spectro_cutoff(ss) * LOG_FACTOR)/(Float)bins;
	  else yfincr = spectro_cutoff(ss) * (Float)SND_SRATE(cp->sound) * 0.5 / (Float)bins;
	}
      else yfincr = 1.0;
      if (fft_log_frequency(ss))
	{
	  for (yf=0.0,i=0;i<=bins;i++,yf+=yfincr)
	    {
	      hfdata[i] = log(yf+1.0) * scaler;
	      hidata[i] = grf_y(hfdata[i],fap);
	    }
	}
      else
	{
	  for (yf=0.0,i=0;i<=bins;i++,yf+=yfincr)
	    {
	      hfdata[i] = yf;
	      hidata[i] = grf_y(yf,fap);
	    }
	}
      xfincr = ((Float)fwidth/(Float)(si->target_slices));
      xf = 2+fap->x_axis_x0;
      for (slice=0;slice<si->active_slices;slice++,xf+=xfincr)
	{
	  for (i=0;i<GRAY_SCALES;i++) js[i] = 0;
	  fdata = si->data[slice];
	  for (i=0;i<bins;i++)
	    {
	      /* above is fdata[i-1], left is si->data[slice-1][i] */
	      binval = fdata[i]/scl;
	      if (fft_log_magnitude(ss)) binval = 1.0 - (dB(ss,binval))/ss->min_dB;
	      if (binval >= color_cutoff(ss))
		{
		  if (color_inverted(ss)) 
		    j = (int)(skew_color((1.0 - binval),color_scale(ss))*GRAY_SCALES); 
		  else j = (int)(skew_color(binval,color_scale(ss))*GRAY_SCALES);
		  if (j>0) j--; else j=0;
		  if (fft_log_frequency(ss))
		    set_sono_rectangle(js[j],j,(int)xf,hidata[i+1],rectw,hidata[i]-hidata[i+1]);
		  else set_sono_rectangle(js[j],j,(int)xf,hidata[i+1],rectw,recth);
		  if (cp->printing)
		    {
		      if (fft_log_frequency(ss)) 
			ps_draw_sono_rectangle(cp,fap,j,fap->x0 + xscl*slice,hfdata[i+1],frectw,hfdata[i]-hfdata[i+1]);
		      else ps_draw_sono_rectangle(cp,fap,j,fap->x0 + xscl*slice,hfdata[i+1],frectw,frecth);
		    }
		  js[j]++;
		}
	    }
	  for (i=0;i<GRAY_SCALES;i++)
	    {
	      if (js[i] > 0) draw_sono_rectangles(copy_context(cp),i,js[i]);
	    }
	  if (cp->printing)
	    {
	      check_for_event(ss);
	      if ((ss->stopped_explicitly) || (!(cp->sound))) /* user closed file while trying to print */
		{
		  ss->stopped_explicitly = 0;
		  report_in_minibuffer(sp,"stopped");
		  break;
		}
	    }
	}
      if (cp->printing) ps_reset_color(cp);
      FREE(hfdata);
      FREE(hidata);
      if (cp->hookable) after_fft(ss,cp,1.0/scl);
    }
}

static void rotate_matrix(Float xangle, Float yangle, Float zangle, Float xscl, Float yscl, Float zscl, Float *mat)
{
  /* return rotation matrix for rotation through angles xangle, yangle, then zangle with scaling by xscl, yscl, zscl */
  Float sinx,siny,sinz,cosx,cosy,cosz,deg;
  deg = TWO_PI / 360.0;
  /* might be nice to use sincosf here, but it segfaults in GnuC */
  sinx = sin(xangle*deg);
  siny = sin(yangle*deg);
  sinz = sin(zangle*deg);
  cosx = cos(xangle*deg);
  cosy = cos(yangle*deg);
  cosz = cos(zangle*deg);
  mat[0] = cosy*cosz*xscl;
  mat[1] = (sinx*siny*cosz - cosx*sinz)*yscl;
  mat[2] = (cosx*siny*cosz + sinx*sinz)*zscl;
  mat[3] = cosy*sinz*xscl;
  mat[4] = (sinx*siny*sinz + cosx*cosz)*yscl;
  mat[5] = (cosx*siny*sinz - sinx*cosz)*zscl;
  mat[6] = -siny*xscl;
  mat[7] = sinx*cosy*yscl;
  mat[8] = cosx*cosy*zscl;
}

static void rotate(Float *xyz, Float *mat)
{ /* use rotation/scaling matrix set up by rotate_matrix to rotate and scale the vector xyz */
  Float x,y,z;
  x = xyz[0];
  y = xyz[1];
  z = xyz[2];
  xyz[0] = mat[0]*x + mat[1]*y + mat[2]*z;
  xyz[1] = mat[3]*x + mat[4]*y + mat[5]*z;
  xyz[2] = mat[6]*x + mat[7]*y + mat[8]*z;
}

void reset_spectro(snd_state *state)
{
  set_spectro_cutoff(state,DEFAULT_SPECTRO_CUTOFF);
  set_spectro_hop(state,DEFAULT_SPECTRO_HOP);
  set_spectro_x_scale(state,DEFAULT_SPECTRO_X_SCALE);
  set_spectro_y_scale(state,DEFAULT_SPECTRO_Y_SCALE);
  set_spectro_z_scale(state,DEFAULT_SPECTRO_Z_SCALE);
  set_spectro_z_angle(state,DEFAULT_SPECTRO_Z_ANGLE);
  set_spectro_x_angle(state,DEFAULT_SPECTRO_X_ANGLE);
  set_spectro_y_angle(state,DEFAULT_SPECTRO_Y_ANGLE);
  set_color_map(state,-1);
  set_color_cutoff(state,DEFAULT_COLOR_CUTOFF);
  set_color_scale(state,DEFAULT_COLOR_SCALE);
  set_color_inverted(state,DEFAULT_COLOR_INVERTED);
  set_wavo_hop(state,DEFAULT_WAVO_HOP);
  set_wavo_trace(state,DEFAULT_WAVO_TRACE);
}

static void make_spectrogram(chan_info *cp, snd_info *sp, snd_state *ss)
{
  sono_info *si;
  fft_info *fp;
  axis_info *fap;
  Float *fdata;
  Float matrix[9];
  Float xyz[3];
  Float xoff,yoff,x,y,xincr,yincr,x0,y0,binval,scl = 1.0;
  Float fwidth,fheight,zscl,yval,xval;
  int bins,slice,i,j,xx,yy;
  if (chan_fft_in_progress(cp)) return;
  si = (sono_info *)(cp->sonogram_data);
  if ((si) && (si->scale > 0.0))
    {
      allocate_grf_points();
      if (cp->printing) ps_allocate_grf_points();
      scl = si->scale; /* unnormalized fft doesn't make much sense here (just washes out the graph) */
      fp = cp->fft;
      fap = fp->axis;
      bins = (int)(si->target_bins * spectro_cutoff(ss));
      fwidth = (fap->x_axis_x1 - fap->x_axis_x0);
      fheight = (fap->y_axis_y1 - fap->y_axis_y0); /* negative! */
      xincr = fwidth/(Float)bins;
      yincr = fheight/(Float)si->active_slices;
      x0 = (fap->x_axis_x0+fap->x_axis_x1)*0.5;
      y0 = (fap->y_axis_y0+fap->y_axis_y1)*0.5;
      if (!(fft_log_magnitude(ss)))
	zscl = -(spectro_z_scale(ss)*fheight/scl);
      else zscl = -(spectro_z_scale(ss)*fheight);
      rotate_matrix(spectro_x_angle(ss),spectro_y_angle(ss),spectro_z_angle(ss),
		    spectro_x_scale(ss),spectro_y_scale(ss),zscl,
		    matrix);
      if (color_map(ss) == -1)
	{
	  for (slice=0,xoff=fap->x_axis_x0,yoff=fap->y_axis_y0;slice<si->active_slices;slice++,yoff+=yincr)
	    {
	      fdata = si->data[slice];
	      x=xoff;
	      y=yoff;
	      for (i=0;i<bins;i++,x+=xincr)
		{
		  xyz[0]=x-x0; 
		  xyz[1]=y-y0; 
		  if (!(fft_log_magnitude(ss))) 
		    xyz[2]=fdata[i];
		  else {binval = fdata[i]/scl; xyz[2] = 1.0 - (dB(ss,binval))/ss->min_dB;}
		  rotate(xyz,matrix);
		  yval = xyz[1]+xyz[2];
		  xval = xyz[0];
		  set_grf_point((int)(xval+x0),i,(int)(yval+y0));
		  if (cp->printing) ps_set_grf_point(ungrf_x(fap,(int)(xval+x0)),i,ungrf_y(fap,(int)(yval+y0)));
		}
	      draw_grf_points(ss,copy_context(cp),bins,fap,0.0);
	      if (cp->printing) 
		{
		  ps_draw_grf_points(ss,cp,fap,bins,0.0);
		  check_for_event(ss);
		  if ((ss->stopped_explicitly) || (!(cp->sound)))
		    {
		      ss->stopped_explicitly = 0;
		      report_in_minibuffer(sp,"stopped");
		      break;
		    }
		}
	    }
	}
      else
	{
	  /* spectrogram in various colors */
	  allocate_color_map(ss,color_map(ss));
	  for (slice=0,xoff=fap->x_axis_x0,yoff=fap->y_axis_y0;slice<si->active_slices;slice++,yoff+=yincr)
	    {
	      fdata = si->data[slice];
	      x=xoff;
	      y=yoff;
	      xyz[0]=x-x0; xyz[1]=y-y0; xyz[2]=fdata[0]; rotate(xyz,matrix); xx=(int)(xyz[0]+x0); yy=(int)(xyz[1]+xyz[2]+y0);
	      for (i=0;i<bins;i++,x+=xincr)
		{
		  xyz[0]=x-x0; xyz[1]=y-y0;
		  binval = fdata[i]/scl;
		  if (!(fft_log_magnitude(ss))) 		  
		    xyz[2]=fdata[i];
		  else {xyz[2] = 1.0 - (dB(ss,binval))/ss->min_dB; binval = xyz[2];}
		  rotate(xyz,matrix);
		  yval = xyz[1]+xyz[2];
		  xval = xyz[0];
		  if (binval >= color_cutoff(ss))
		    {
		      if (color_inverted(ss)) 
			j = (int)(skew_color((1.0 - binval),color_scale(ss))*GRAY_SCALES); 
		      else j = (int)(skew_color(binval,color_scale(ss))*GRAY_SCALES);
		      if (j>0) j--;
		      if (j >= GRAY_SCALES) j=GRAY_SCALES-1;
		      draw_spectro_line(copy_context(cp),j,xx,yy,(int)(xval+x0),(int)(yval+y0));
		      if (cp->printing) ps_draw_spectro_line(cp,j,xx,yy,xval+x0,yval+y0);
		    }
		  xx = (int)(xval+x0); yy=(int)(yval+y0);
		}
	      if (cp->printing) 
		{
		  check_for_event(ss);
		  if ((ss->stopped_explicitly) || (!(cp->sound)))
		    {
		      ss->stopped_explicitly = 0;
		      report_in_minibuffer(sp,"stopped");
		      break;
		    }
		}
	    }
	  if (cp->printing) ps_reset_color(cp);
	}
      if (cp->hookable) after_fft(ss,cp,1.0/scl);
    }
}

static void make_wavogram(chan_info *cp, snd_info *sp, snd_state *ss)
{
  Float xoff,x,y,x0,y0,xincr;
  Float width,height,zscl,yval,xval,binval;
  int i,j,yincr,yoff,xx,yy;
  MUS_SAMPLE_TYPE ina;
  Float matrix[9];
  Float xyz[3];
  snd_fd *sf;
  axis_info *ap;
  ap = cp->axis;
  ap->losamp = (int)(ap->x0*SND_SRATE(sp));
  sf = init_sample_read(ap->losamp,cp,READ_FORWARD);
  allocate_grf_points();
  if (cp->printing) ps_allocate_grf_points();
  width = (ap->x_axis_x1 - ap->x_axis_x0);
  height = (ap->y_axis_y1 - ap->y_axis_y0); /* negative! */
  xincr = width/(Float)wavo_trace(ss);
  yincr = -(wavo_hop(ss));
  if (yincr > 0) yincr = -yincr;
  if (yincr == 0) yincr = -1;
  x0 = (ap->x_axis_x0+ap->x_axis_x1)*0.5;
  y0 = (ap->y_axis_y0+ap->y_axis_y1)*0.5;
  zscl = -(spectro_z_scale(ss)*height);
  rotate_matrix(spectro_x_angle(ss),spectro_y_angle(ss),spectro_z_angle(ss),
		spectro_x_scale(ss),spectro_y_scale(ss),zscl,
		matrix);
  if (color_map(ss) == -1)
    {
      for (xoff=ap->x_axis_x0,yoff=ap->y_axis_y0;yoff>ap->y_axis_y1;yoff+=yincr)
	{
	  x=xoff;
	  y=yoff;
	  for (i=0;i<wavo_trace(ss);i++,x+=xincr)
	    {
	      NEXT_SAMPLE(ina,sf);
	      xyz[0]=x-x0; xyz[1]=y-y0; xyz[2]=MUS_SAMPLE_TO_FLOAT(ina);
	      rotate(xyz,matrix);
	      yval = xyz[1]+xyz[2];
	      xval = xyz[0];
	      set_grf_point((int)(xval+x0),i,(int)(yval+y0));
	      if (cp->printing) ps_set_grf_point(ungrf_x(ap,(int)(xval+x0)),i,ungrf_y(ap,(int)(y0+yval)));
	    }
	  draw_grf_points(ss,copy_context(cp),wavo_trace(ss),ap,0.0);
	  if (cp->printing) ps_draw_grf_points(ss,cp,ap,wavo_trace(ss),0.0);
	}
    }
  else
    {
      allocate_color_map(ss,color_map(ss));
      for (xoff=ap->x_axis_x0,yoff=ap->y_axis_y0;yoff>ap->y_axis_y1;yoff+=yincr)
	{
	  xx=-1;
	  x=xoff;
	  y=yoff;
	  yy = (int)y0; /* ? */
	  for (i=0;i<wavo_trace(ss);i++,x+=xincr)
	    {
	      NEXT_SAMPLE(ina,sf);
	      binval = MUS_SAMPLE_TO_FLOAT(ina);
	      xyz[0]=x-x0; xyz[1]=y-y0; xyz[2]=binval;
	      rotate(xyz,matrix);
	      yval = xyz[1]+xyz[2];
	      xval = xyz[0];
	      /* for color decision here we need absolute value of data */
	      if (binval < 0.0) binval = -binval;
	      if ((binval >= color_cutoff(ss)) && (xx != -1))
		{
		  if (color_inverted(ss)) 
		    j = (int)(skew_color((1.0 - binval),color_scale(ss))*GRAY_SCALES); 
		  else j = (int)(skew_color(binval,color_scale(ss))*GRAY_SCALES);
		  if (j>0) j--;
		  if (j >= GRAY_SCALES) j=GRAY_SCALES-1;
		  draw_spectro_line(copy_context(cp),j,xx,yy,(int)(xval+x0),(int)(yval+y0));
		  if (cp->printing) ps_draw_spectro_line(cp,j,xx,yy,xval+x0,yval+y0);
		}
	      xx = (int)(xval+x0); yy=(int)(yval+y0);
	    }
	}
      if (cp->printing) ps_reset_color(cp);
    }
  free_snd_fd(sf);
}

static void make_lisp_graph(chan_info *cp, snd_info *sp, snd_state *ss)
{
  lisp_grf *up;
  axis_info *uap = NULL;
  int i,j,grf_len,graph;
  axis_context *ax;
  COLOR_TYPE old_color=0;
  Float x,y,samples_per_pixel=1.0,xinc,start_x,xf,ymin,ymax,xi,pinc;
  up = (lisp_grf *)(cp->lisp_info);
  if (up) uap = up->axis; else return;
  if ((!uap) || (!uap->graph_active) || (up->len[0] <= 0)) return;
  allocate_grf_points();
  if (cp->printing) ps_allocate_grf_points();
  if (sp->combining == CHANNELS_SUPERIMPOSED) 
    {
      ax = combined_context(cp); 
      if (cp->printing) ps_recolor(cp);
    }
  else ax = copy_context(cp);
  if (up->graphs > 1) old_color = get_foreground_color(cp,ax);
  for (graph=0;graph<up->graphs;graph++)
    {
      /* check for up->len[graph] > pixels available and use ymin ymax if needed */
      switch (graph)
	{
	case 0: break;
	case 1: set_foreground_color(cp,ax,(ss->sgx)->red); break;
	case 2: set_foreground_color(cp,ax,(ss->sgx)->green); break;
	case 3: set_foreground_color(cp,ax,(ss->sgx)->light_blue); break;
	case 4: set_foreground_color(cp,ax,(ss->sgx)->yellow); break;
	default: set_foreground_color(cp,ax,(ss->sgx)->black); break;
	}
      samples_per_pixel = (Float)(up->len[graph])/(Float)(uap->x_axis_x1 - uap->x_axis_x0);
      if (samples_per_pixel < 4.0)
	{
	  grf_len = up->len[graph];
	  start_x = uap->x0;
	  if (grf_len <= 1) 
	    xinc = 1.0;
	  else xinc = (uap->x1 - uap->x0) / (Float)(grf_len-1);
	  for (i=0,x=start_x;i<grf_len;i++,x+=xinc)
	    {
	      y = up->data[graph][i];
	      set_grf_point(grf_x(x,uap),i,grf_y(y,uap));
	      if (cp->printing) ps_set_grf_point(x,i,y);
	    }
	  draw_grf_points(ss,ax,grf_len,uap,0.0);
	  if (cp->printing) ps_draw_grf_points(ss,cp,uap,grf_len,0.0);
	}
      else
	{
	  j = 0;
	  i = 0;
	  xf = 0.0;
	  x = 0.0;
	  pinc = samples_per_pixel/(Float)(up->len[graph]);
	  xi = grf_x(0.0,uap);
	  ymin = 32768.0;
	  ymax = -32768.0;
	  while (i < up->len[graph])
	    {
	      y = up->data[graph][i];
	      if (y > ymax) ymax = y;
	      if (y < ymin) ymin = y;
	      xf += 1.0;
	      i++;
	      if (xf>samples_per_pixel)
		{
		  set_grf_points((int)xi,j,grf_y(ymin,uap),grf_y(ymax,uap));
		  if (cp->printing) {x += pinc; ps_set_grf_points(x,j,ymin,ymax);}
		  xi++;
		  j++;
		  xf -= samples_per_pixel;
		  ymin=32767.0;
		  ymax=-32768.0;
		}
	    }
	  draw_both_grf_points(ss,ax,j,uap);
	  if (cp->printing) ps_draw_both_grf_points(ss,cp,uap,j);
	}
    }
  if (up->graphs > 1) set_foreground_color(cp,ax,old_color);
  if (sp->combining == CHANNELS_SUPERIMPOSED) 
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing) ps_reset_color(cp);
    }
  if (show_fft_peaks(ss)) display_peaks(cp,uap,up->data[0],1,up->len[0]-1,samples_per_pixel,0,0.0);
}

static void draw_graph_cursor(chan_info *cp);

static void display_channel_data_with_size (chan_info *cp, snd_info *sp, snd_state *ss, int width, int height, int offset)
{
  int with_fft,with_lisp,displays,points;
  axis_info *ap = NULL;
  axis_info *fap = NULL;
  axis_info *uap = NULL;
  fft_info *fp = NULL;
  lisp_grf *up = NULL;
  if ((cp->hookable) && (dont_graph(ss,cp))) return;
  ap = cp->axis;
  if (ap == NULL) return;
  /* now check for fft/wave/user-function decisions */
  ap->height = height;
  ap->window_width = width;
  ap->width = width;
  ap->y_offset = offset;
  if (cp->waving) displays = 1; else displays = 0;
  if ((cp->lisp_graphing) && (up = (lisp_grf *)(cp->lisp_info)))
    {
      displays++;
      uap = up->axis;
      if (uap)
	{
	  with_lisp = 1;
	  uap->height = height;
	  uap->y_offset = offset;
	  uap->width = width;
	  uap->window_width = width;
	}
      else with_lisp = 0;
    }
  else with_lisp = 0;

  if ((cp->ffting) && (fp = cp->fft))
    {
      displays++;
      fap = fp->axis; 
      if (fap)
	{
	  with_fft = 1;
	  fap->height = height;
	  fap->y_offset = offset;
	  fap->width = width;
	  fap->window_width = width;
	}
      else with_fft = 0;
    }
  else with_fft = 0;

  if (graphs_horizontal(ss))
    {
      if (cp->waving) ap->width = width/displays;
      if (with_fft) fap->width =  width/displays;
      if (with_lisp) uap->width = width/displays;

      /* now the x axis offsets for fap and uap */
      if (with_fft) {if (cp->waving) fap->graph_x0 = ap->width; else fap->graph_x0 = 0;}
      if (with_lisp) uap->graph_x0 = uap->width*(displays - 1);
    }
  else
    {
      if (displays > 0) height /= displays;
      if (cp->waving) ap->height = height;
      if (with_fft) fap->height =  height;
      if (with_lisp) uap->height = height;

      /* now the y axis offsets for fap and uap */
      if (with_fft)
	{
	  if (cp->waving) fap->y_offset += height; 
	  fap->graph_x0 = 0;
	}
      if (with_lisp) 
	{
	  uap->y_offset += (height*(displays - 1)); 
	  uap->graph_x0 = 0;
	}
    }
  if ((!with_fft) && (!(cp->waving)) && (!(with_lisp)))
    clear_window(ap->ax);
  else 
    {
      if (sp->combining != CHANNELS_SUPERIMPOSED)
	cp->clear = 1; /* deferred clear (can be wave, fft, lisp triggered) in make_axes (snd-xdraw.c) */
      else 
	{
	  if (cp->chan == 0)
	    cp->clear = updating; /* a horrible kludge... */
	  else cp->clear = 1;
	}
    }

  marks_off(cp);
  selection_off(cp);
  if (cp->waving)
    {
      if (wavo(ss))
	{
	  if (ap->y_axis_y0 == ap->y_axis_y1) make_axes(cp,ap,x_axis_style(ss)); /* first time needs setup */
	  ap->y0 = ap->x0;
	  ap->y1 = ap->y0+(Float)(wavo_trace(ss) * (ap->y_axis_y0 - ap->y_axis_y1)) / ((Float)(wavo_hop(ss))*SND_SRATE(sp));
	  ap->x1 = ap->x0 + (Float)(wavo_trace(ss))/(Float)SND_SRATE(sp);
	}
      make_axes(cp,ap,x_axis_style(ss));
      cp->cursor_visible = 0;
      points = make_graph(cp,sp,ss);
      if (points == 0) return;
      if ((show_mix_consoles(ss)) && (cp->mixes) && (cp->mix_dragging)) mix_save_graph(ss,((mixdata *)(cp->mix_dragging))->wg,points);
      if (cp->cursor_on) draw_graph_cursor(cp);
    }
  if (with_fft)
    {
      if (fft_style(ss) == SPECTROGRAM) cp->drawing = 0;
      make_axes(cp,fap,(x_axis_style(ss) == X_IN_SAMPLES) ? X_IN_SECONDS : (x_axis_style(ss)));
      cp->drawing = 1;
      if (!cp->waving)
	{ /* make_graph does this -- sets losamp needed by fft to find its starting point */
	  ap->losamp = (int)(ap->x0*(double)SND_SRATE(sp));
	  ap->hisamp = (int)(ap->x1*(double)SND_SRATE(sp));
	}
      switch (fft_style(ss))
	{
	case NORMAL_FFT: make_fft_graph(cp,sp,ss); break;
	case SONOGRAM: make_sonogram(cp,sp,ss); break;
	case SPECTROGRAM: make_spectrogram(cp,sp,ss); break;
	default: snd_error("unknown fft style: %d",fft_style(ss)); break;
	}
    }
  if (with_lisp)
    {
      make_axes(cp,uap,X_IN_LENGTH);
      if ((!(cp->waving)) && (!(with_fft)))
	{
	  ap->losamp = (int)(ap->x0*(double)SND_SRATE(sp));
	  ap->hisamp = (int)(ap->x1*(double)SND_SRATE(sp));
	}
      make_lisp_graph(cp,sp,ss);
    }
  if (cp->waving)
    {
      display_selection(cp);
      if ((cp->marks) && (show_marks(ss))) display_channel_marks(cp);
      if (cp == selected_channel(ss)) draw_graph_border(cp);
      if (show_y_zero(ss)) display_zero(cp);
      if ((show_mix_consoles(ss)) && (cp->mixes)) display_channel_mixes(cp);
    }
  else 
    {
      if (cp->mixes) release_mixes(cp);
    }
  if ((displays > 0) && (sp->combining != CHANNELS_SUPERIMPOSED) && (height > 10)) 
    display_channel_id(cp,height+offset,sp->nchans);
  if (cp->hookable) after_graph(cp);
}

void display_channel_data (chan_info *cp, snd_info *sp, snd_state *ss)
{
  int width,height,offset,full_height,chan_height,y0,y1,bottom,top;
  Float val,size;
  axis_info *ap;
  if ((sp->combining == CHANNELS_SEPARATE) || (sp->nchans == 1))
    {
      width = widget_width(channel_graph(cp));
      height = widget_height(channel_graph(cp));
      if ((height > 5) && (width > 5))
	display_channel_data_with_size(cp,sp,ss,width,height,0);
    }
  else
    {
      /* all chans in one graph widget, sy->scroll entire set, zy->zoom entire set etc */
      /* updates are asynchronous (dependent on background ffts etc), so we can't do the whole window at once */
      /* complication here is that we're growing down from the top, causing endless confusion */
      width = widget_width(channel_graph(sp->chans[0])) - (2 * ss->position_slider_width);
      height = widget_height(channel_graph(sp->chans[0]));
      cp->height = height;
      if (sp->combining == CHANNELS_SUPERIMPOSED)
	display_channel_data_with_size(cp,sp,ss,width,height,0);
      else
	{
	  val = gsy_value(sp->chans[0]);
	  size = gsy_size(sp->chans[0]);
	  full_height = (int)((Float)height / size);
	  chan_height = full_height / sp->nchans;
	  bottom = (int)(full_height * val);
	  top = (int)(full_height * (val+size));
	  y1 = (sp->nchans - cp->chan) * chan_height;
	  y0 = y1 - chan_height;
	  offset = top - y1;
	  if ((cp->chan == 0) && (offset > 0))
	    {
	      /* round off trouble can lead to garbage collecting at the top of the window (similarly at the bottom I suppose) */
	      chan_height += offset;
	      offset = 0;
	    }
	  if ((cp->chan == (sp->nchans - 1)) && ((offset+chan_height) < height))
	    chan_height = height - offset;
	  if (((y0 < top) && (y0 >= bottom)) || ((y1 > bottom) && (y1 <= top)))
	    display_channel_data_with_size(cp,sp,ss,width,chan_height,offset);
	  else 
	    {
	      ap = cp->axis;
	      ap->y_offset = offset; /* needed for mouse click channel determination */
	      if (cp->mixes) release_mixes(cp);
	    }
	}
    }
}



/* ---------------- CHANNEL CURSOR ---------------- */


#define cursor_size 15

static void draw_graph_cursor(chan_info *cp)
{
  axis_info *ap;
  axis_context *ax;
  ap = cp->axis;
  if ((cp->cursor < ap->losamp) || (cp->cursor > ap->hisamp)) return;
  ax = cursor_context(cp);
  if (cp->cursor_visible)
    {
      switch (cp->cursor_style)
	{
	case CURSOR_CROSS:
	  draw_line(ax,cp->cx,cp->cy - cursor_size,cp->cx,cp->cy + cursor_size);
	  draw_line(ax,cp->cx - cursor_size,cp->cy,cp->cx + cursor_size,cp->cy);
	  break;
	case CURSOR_LINE:
	  draw_line(ax,cp->cx,ap->y_axis_y0,cp->cx,ap->y_axis_y1);
	  break;
	}
    }
  cp->cx = grf_x((double)(cp->cursor)/(double)SND_SRATE(cp->sound),ap); /* not Float -- this matters in very long files (i.e. > 40 minutes) */
  cp->cy = grf_y(sample(cp->cursor,cp),ap);
  switch (cp->cursor_style)
    {
    case CURSOR_CROSS:
      draw_line(ax,cp->cx,cp->cy - cursor_size,cp->cx,cp->cy + cursor_size);
      draw_line(ax,cp->cx - cursor_size,cp->cy,cp->cx + cursor_size,cp->cy);
      break;
    case CURSOR_LINE:
      draw_line(ax,cp->cx,ap->y_axis_y0,cp->cx,ap->y_axis_y1);
      break;
    }
  cp->cursor_visible = 1;
}

static int cursor_decision(chan_info *cp)
{
  int len;
  len = current_ed_samples(cp);
  if (cp->cursor >= len) cp->cursor = len-1; /* zero based, but in 0-length files, len=0 */
  if (cp->cursor < 0) cp->cursor = 0;        /* perhaps the cursor should be forced off in empty files? */
  if (cp->cursor < (cp->axis)->losamp)
    {
      if (cp->cursor == 0) return(CURSOR_ON_LEFT);
      else return(CURSOR_IN_MIDDLE);
    }
  if (cp->cursor > (cp->axis)->hisamp)
    {
      if (cp->cursor >= (len-1)) return(CURSOR_ON_RIGHT);
      else return(CURSOR_IN_MIDDLE);
    }
  return(CURSOR_IN_VIEW);
}

static void handle_cursor_with_sync(chan_info *cp,int decision)
{
  snd_info *sp;
  sync_info *si = NULL;
  int i;
  if (cp)
    {
      sp = cp->sound;
      if ((sp) && (sp->syncing != 0))
	{
	  si = snd_sync(cp->state,sp->syncing);
	  for (i=0;i<si->chans;i++)
	    handle_cursor(si->cps[i],decision);
	  free_sync_info(si);
	}
      else handle_cursor(cp,decision);
    }
}

int cursor_moveto (chan_info *cp, int samp)
{
  snd_info *sp;
  chan_info *ncp;
  sync_info *si;
  int i;
  sp = cp->sound;
  if ((sp) && (sp->syncing != 0))
    {
      si = snd_sync(cp->state,sp->syncing);
      for (i=0;i<si->chans;i++)
	{
	  ncp = si->cps[i];
	  ncp->cursor = samp;
	  if (ncp != cp) handle_cursor(ncp,cursor_decision(ncp)); /* checks len */
	}
      free_sync_info(si);
    }
  else cp->cursor = samp;
  return(cursor_decision(cp));
}

static int cursor_move (chan_info *cp,int samps)
{
  return(cursor_moveto(cp,cp->cursor+samps));
}

static int cursor_moveto_beginning(chan_info *cp) 
{
  return(cursor_moveto(cp,0)); /* is ap->xmin ever going to be non-zero? */
}

static int cursor_moveto_end(chan_info *cp)
{
  return(cursor_moveto(cp,current_ed_samples(cp)-1));
}


static int region_count = 0;
static void get_amp_expression(snd_info *sp,int count, int regexpr) {prompt(sp,STR_env_p,NULL); sp->amping = count; sp->reging = regexpr;}
static void get_eval_expression(snd_info *sp, int count, int regexpr) {prompt(sp,STR_eval_p,NULL); sp->evaling = count; sp->reging = regexpr;}

void show_cursor_info(chan_info *cp)
{
  snd_info *sp;
  chan_info *ncp;
  Float y,absy;
  int digits,i,samp;
  char *s1,*s2;
  sp = cp->sound;
  if ((sp->syncing != 0) && (cp->chan != 0)) return;
  samp = cp->cursor;
  y = sample(samp,cp);
  absy = fabs(y);
  if (absy < .0001) digits=4;
  else if (absy<.001) digits=3;
  else digits=2;
  if (sp->nchans == 1)
    sprintf(expr_str,STR_cursor_at_sample,
	    s1 = prettyf((double)samp/(double)SND_SRATE(sp),2),
	    samp,
	    s2 = prettyf(y,digits));
  else
    {
      if (sp->syncing == 0)
	sprintf(expr_str,STR_chan_cursor_at_sample,
		cp->chan + 1,
		s1 = prettyf((double)samp/(double)SND_SRATE(sp),2),
		samp,
		s2 = prettyf(y,digits));
      else
	{
	  /* in this case, assume we show all on chan 0 and ignore the call otherwise (see above) */
	  /* "cursor at..." then list of values */
	  sprintf(expr_str,STR_cursor_at_sample_p,
		  s1 = prettyf((double)samp/(double)SND_SRATE(sp),2),
		  samp,
		  s2 = prettyf(y,digits));
	  for (i=1;i<sp->nchans;i++)
	    {
	      strcat(expr_str,", ");
	      FREE(s2);
	      ncp = sp->chans[i];
	      y = sample(samp,ncp);
	      absy = fabs(y);
	      if (absy < .0001) digits=4;
	      else if (absy<.001) digits=3;
	      else digits=2;
	      s2 = prettyf(y,digits);
	      strcat(expr_str,s2);
	    }
	}
    }
  set_minibuffer_string(sp,expr_str);
  sp->minibuffer_on = 1;
  FREE(s1);
  FREE(s2);
}

void clear_minibuffer(snd_info *sp)
{
  clear_minibuffer_prompt(sp);
  expr_str[0] = '\0';
  set_minibuffer_string(sp,expr_str);
  sp->searching = 0;
  sp->evaling = 0;
  sp->marking = 0;
  sp->filing = NOT_FILING;
  sp->printing = 0;
  sp->minibuffer_on = 0;
  sp->loading = 0;
  sp->amping = 0;
  sp->macroing = 0;
  sp->prompting = 0;
}

static int set_window_bounds(chan_info *cp,int count) 
{
  /* count = sample number to start at */
  axis_info *ap;
  Float sx;
  ap = cp->axis;
  sx = (((double)count/(double)SND_SRATE(cp->sound)) - ap->xmin) / (ap->xmax -ap->xmin);
  reset_x_display(cp,sx,ap->zx);
  return(CURSOR_IN_VIEW);
}

static int set_window_size(chan_info *cp, int count) 
{
  /* set samples within window */
  axis_info *ap;
  Float zx;
  ap = cp->axis;
  zx = ((double)count/(((double)SND_SRATE(cp->sound)) * (ap->xmax -ap->xmin)));
  reset_x_display(cp,ap->sx,zx);
  return(CURSOR_IN_VIEW);
}

static int set_window_percentage(chan_info *cp, int count) 
{
  /* set percentage of file within window */
  axis_info *ap;
  Float zx;
  ap = cp->axis;
  zx = (double)count/(double)SND_SRATE(cp->sound);
  reset_x_display(cp,ap->sx,zx);
  return(CURSOR_IN_VIEW);
}

static void window_frames_selection(chan_info *cp)
{
  axis_info *ap;
  Float sx,zx;
  ap = cp->axis;
  sx = (((double)(selection_beg(cp)))/((double)SND_SRATE(cp->sound)) - ap->xmin) / (ap->xmax - ap->xmin);
  zx = ((double)(region_len(0)))/(((double)(SND_SRATE(cp->sound))) * (ap->xmax - ap->xmin));
  reset_x_display(cp,sx,zx);
}

void handle_cursor(chan_info *cp, int redisplay)
{
  axis_info *ap;
  axis_context *ax;
  snd_state *ss;
  snd_info *sp;
  Float gx = 0.0;
  if (cp == NULL) return;
  if ((redisplay != CURSOR_NO_ACTION) && (redisplay != KEYBOARD_NO_ACTION))
    {
      ss = cp->state;
      sp = cp->sound;
      if ((verbose_cursor(ss)) && (sp->minibuffer_on == 0)) /* don't overwrite M-X results with cursor garbage! */
	{
	  show_cursor_info(cp); 
	  sp->minibuffer_on = 0;
	} 
      if (redisplay == CURSOR_UPDATE_DISPLAY)
	{
	  update_graph(cp,NULL);
	}
      else
	{
	  if (redisplay != CURSOR_IN_VIEW)
	    {
	      ap = cp->axis;
	      if (cp->cursor_visible)
		{
		  ax = cursor_context(cp);
		  draw_line(ax,cp->cx,cp->cy - cursor_size,cp->cx,cp->cy + cursor_size);
		  draw_line(ax,cp->cx - cursor_size,cp->cy,cp->cx + cursor_size,cp->cy);
		  cp->cursor_visible = 0; /* don't redraw at old location */
		}
	      switch (redisplay)
		{
		case CURSOR_ON_LEFT: gx = (double)(cp->cursor)/(double)SND_SRATE(sp); break;
		case CURSOR_ON_RIGHT: gx = (double)(cp->cursor)/(double)SND_SRATE(sp) - ap->zx*(ap->xmax-ap->xmin); break;
		case CURSOR_IN_MIDDLE: gx = (double)(cp->cursor)/(double)SND_SRATE(sp) - ap->zx*0.5*(ap->xmax-ap->xmin); break;
		}
	      if (gx < 0.0) gx = 0.0;
	      reset_x_display(cp,(gx - ap->xmin) / (ap->xmax - ap->xmin),ap->zx);
	    }
	  else {if (cp->cursor_on) draw_graph_cursor(cp);}
	}
    }
  check_keyboard_selection(cp,cp->cx);
}

/* collect syncd chans */
typedef struct {
  sync_info *si;
  snd_fd **sfs;
  int dur;
} sync_state;

static void free_sync_state(sync_state *sc)
{
  if (sc)
    {
      if (sc->si) free_sync_info(sc->si);
      if (sc->sfs) FREE(sc->sfs);
      FREE(sc);
    }
}

static sync_state *get_sync_state_1(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr, int forwards, int prebeg)
{
  /* can return NULL if regexpr and no current selection */
  sync_info *si = NULL;
  snd_fd **sfs = NULL;
  int dur,i,pbeg;
  sync_state *sc;
  dur = 0;
  if ((!regexpr) && (sp == NULL)) return(NULL);
  if ((!regexpr) && (sp->syncing != 0))
    {
      si = snd_sync(ss,sp->syncing);
      sfs = (snd_fd **)CALLOC(si->chans,sizeof(snd_fd *));
      for (i=0;i<si->chans;i++) 
	{
	  si->begs[i] = beg;
	  if (forwards == READ_FORWARD)
	    sfs[i] = init_sample_read(beg,si->cps[i],READ_FORWARD);
	  else sfs[i] = init_sample_read(current_ed_samples(si->cps[i])-1,si->cps[i],READ_BACKWARD);
	}
    }
  else
    {
      if (regexpr)
	{
	  if (selection_is_current())
	    {
	      si = region_sync(0);
	      dur = region_len(0);
	      sfs = (snd_fd **)CALLOC(si->chans,sizeof(snd_fd *));
	      for (i=0;i<si->chans;i++) 
		{
		  if (forwards == READ_FORWARD)
		    {
		      pbeg = si->begs[i] - prebeg;
		      if (pbeg < 0) pbeg = 0;
		      sfs[i] = init_sample_read(pbeg,si->cps[i],READ_FORWARD);
		    }
		  else sfs[i] = init_sample_read(si->begs[i]+dur-1,si->cps[i],READ_BACKWARD);
		}
	    }
	  else 
	    {
	      snd_warning(STR_no_current_selection);
	      return(NULL);
	    }
	}
    }
  if (si == NULL) 
    {
      si = make_simple_sync(cp,beg);
      sfs = (snd_fd **)CALLOC(1,sizeof(snd_fd *));
      if (forwards == READ_FORWARD)
	sfs[0] = init_sample_read(beg,cp,READ_FORWARD);
      else sfs[0] = init_sample_read(current_ed_samples(cp)-1,cp,READ_BACKWARD);
    }
  sc = (sync_state *)CALLOC(1,sizeof(sync_state));
  sc->dur = dur;
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}

static sync_state *get_sync_state(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr, int forwards)
{
  return(get_sync_state_1(ss,sp,cp,beg,regexpr,forwards,0));
}

#if HAVE_GUILE
/* support for scan and map functions */

static sync_state *get_chan_sync_state(chan_info *cp, int beg)
{
  sync_info *si;
  snd_fd **sfs;
  sync_state *sc;
  si = make_simple_sync(cp,beg);
  sfs = (snd_fd **)CALLOC(1,sizeof(snd_fd *));
  sfs[0] = init_sample_read(beg,cp,READ_FORWARD);
  sc = (sync_state *)CALLOC(1,sizeof(sync_state));
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}

static sync_state *get_sound_chans_sync_state(chan_info *cp, int beg)
{
  sync_info *si;
  snd_fd **sfs;
  sync_state *sc;
  snd_info *sp;
  int i;
  sp = cp->sound;
  si = (sync_info *)CALLOC(1,sizeof(sync_info));
  si->chans = sp->nchans;
  si->cps = (chan_info **)CALLOC(sp->nchans,sizeof(chan_info *));
  si->begs = (int *)CALLOC(sp->nchans,sizeof(int));
  sfs = (snd_fd **)CALLOC(sp->nchans,sizeof(snd_fd *));
  for (i=0;i<sp->nchans;i++) 
    {
      si->cps[i] = sp->chans[i];
      si->begs[i] = beg;
      sfs[i] = init_sample_read(beg,si->cps[i],READ_FORWARD);
    }
  sc = (sync_state *)CALLOC(1,sizeof(sync_state));
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}

static sync_state *get_active_chans_sync_state(snd_state *ss, int beg)
{
  sync_info *si;
  snd_fd **sfs;
  sync_state *sc;
  snd_info *sp;
  int snd,chn,i;
  si = (sync_info *)CALLOC(1,sizeof(sync_info));
  si->chans = active_channels(ss,TRUE);
  si->cps = (chan_info **)CALLOC(si->chans,sizeof(chan_info *));
  si->begs = (int *)CALLOC(si->chans,sizeof(int));
  sfs = (snd_fd **)CALLOC(si->chans,sizeof(snd_fd *));
  chn = 0;
  for (snd=0;snd<ss->max_sounds;snd++)
    {
      sp = (ss->sounds[snd]);
      if (snd_ok(sp))
	{
	  for (i=0;i<sp->nchans;i++)
	    {
	      si->cps[chn] = sp->chans[i];
	      si->begs[chn] = beg;
	      sfs[chn] = init_sample_read(beg,si->cps[chn],READ_FORWARD);
	      chn++;
	    }
	}
    }
  sc = (sync_state *)CALLOC(1,sizeof(sync_state));
  sc->sfs = sfs;
  sc->si = si;
  return(sc);
}

static SCM series_scan(snd_state *ss, chan_info *cp, SCM proc, int chan_choice, int beg, int end)
{
  sync_state *sc=NULL;
  sync_info *si;
  snd_info *sp;
  snd_fd **sfs;
  snd_fd *sf;
  char *cgbuf;
  int kp,j,ip,len,num,reporting = 0,rpt = 0,rpt4;
  MUS_SAMPLE_TYPE val;
  SCM res;
  sp = cp->sound;
  switch (chan_choice)
    {
    case SCAN_SYNCD_CHANS: sc = get_sync_state(ss,sp,cp,beg,FALSE,READ_FORWARD); break;
    case SCAN_ALL_CHANS: sc = get_active_chans_sync_state(ss,beg); break;
    case SCAN_SOUND_CHANS: sc = get_sound_chans_sync_state(cp,beg); break;
    case SCAN_CURRENT_CHAN: sc = get_chan_sync_state(cp,beg); break;
    }
  if (sc == NULL) return(SCM_BOOL_T);
  si = sc->si;
  sfs = sc->sfs;
  rpt4 = MAX_BUFFER_SIZE / 4;
  for (ip=0;ip<si->chans;ip++)
    {
      cp = si->cps[ip];
      sp = cp->sound;
      sf = sfs[ip];
      /* since each channel can be of arbitrary length, it is unreasonable to
       * expect the caller to deal with that unless a specific subsequence is
       * known in advance; so here if end is 0, that means it was not set
       * by the caller, meaning go from beg to whatever the current end is.
       * if beg > len, omit this chaneel,
       * if beg > end after fixup, omit.
       */
      len = current_ed_samples(cp);
      if (end >= len) end = len-1;
      if (end == 0) end = len-1;
      num = end-beg+1;
      if (num > 0)
	{
	  reporting = (num > MAX_BUFFER_SIZE);
	  if (reporting) start_progress_report(ss,sp,NOT_FROM_ENVED);
	  for (kp=0;kp<num;kp++)
	    {
	      NEXT_SAMPLE(val,sf);
	      res = g_call1(proc,gh_double2scm((double)(MUS_SAMPLE_TO_FLOAT(val))));
	      if ((SCM_NFALSEP(res)) || (ss->eval_error))
		{
		  for (j=ip;j<si->chans;j++) free_snd_fd(sfs[j]);
		  free_sync_state(sc); 
		  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
		  if (ss->eval_error)
		    {
		      ss->eval_error = 0;
		      return(SND_EVAL_ERROR);
		    }
		  return(gh_list(res,gh_int2scm(kp+beg),gh_int2scm(cp->chan),gh_int2scm(sp->index),SCM_UNDEFINED));
		}
	      if (reporting) 
		{
		  rpt++;
		  if (rpt > rpt4)
		    {
		      progress_report(ss,sp,"scan",ip+1,si->chans,(Float)kp / (Float)num,NOT_FROM_ENVED);
		      rpt = 0;
		      if (ss->stopped_explicitly)
			{
			  ss->stopped_explicitly = 0;
			  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
			  cgbuf = (char *)CALLOC(128,sizeof(char));
			  if (si->chans == 1)
			    sprintf(cgbuf,"C-G stopped scan at sample %d",kp+beg);
			  else sprintf(cgbuf,"C-G stopped scan in %s chan %d at sample %d",sp->shortname,cp->chan+1,kp+beg);
			  report_in_minibuffer(sp,cgbuf);
			  FREE(cgbuf);
			  for (j=ip;j<si->chans;j++) free_snd_fd(sfs[j]);
			  free_sync_state(sc); 
			  return(SCM_BOOL_F);
			}
		    }
		}
	    }
	  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
	  sfs[ip] = free_snd_fd(sfs[ip]);
	}
    }
  free_sync_state(sc);
  return(g_call1(proc,SCM_BOOL_F));
}

static SCM parallel_scan(snd_state *ss, chan_info *cp, SCM proc, int chan_choice, int beg, int end)
{
  /* here we need to load the arglist in order */
  sync_state *sc=NULL;
  sync_info *si;
  snd_info *sp;
  snd_fd **sfs;
  int kp,ip,pos = 0,len,num,reporting = 0,rpt = 0,rpt4;
  MUS_SAMPLE_TYPE val;
  SCM res=SCM_UNDEFINED,args,gh_chans,zero;
  char *cgbuf;
  sp = cp->sound;
  switch (chan_choice)
    {
    case SCAN_SYNCD_CHANS: sc = get_sync_state(ss,sp,cp,beg,FALSE,READ_FORWARD); break;
    case SCAN_ALL_CHANS: sc = get_active_chans_sync_state(ss,beg); break;
    case SCAN_SOUND_CHANS: sc = get_sound_chans_sync_state(cp,beg); break;
    case SCAN_CURRENT_CHAN: sc = get_chan_sync_state(cp,beg); break;
    }
  if (sc == NULL) return(SCM_BOOL_T);
  si = sc->si;
  sfs = sc->sfs;
  rpt4 = MAX_BUFFER_SIZE / 4;
  gh_chans = gh_int2scm(si->chans);
  args = gh_make_vector(gh_chans,SCM_BOOL_F);
  /* fixup for different length chans as above, but in this case
   * we need to capture the longest of the channels, padding the
   * others with zero.
   */
  len = current_ed_samples(si->cps[0]);
  for (ip=1;ip<si->chans;ip++)
    {
      num = current_ed_samples(si->cps[ip]);
      if (num > len) len = num;
    }
  if (end >= len) end = len-1;
  if (end == 0) end = len-1;
  num = end-beg+1;
  reporting = (num > MAX_BUFFER_SIZE);
  if (reporting) start_progress_report(ss,sp,NOT_FROM_ENVED);
  if (si->chans == 1)
    { /* optimize common special case */
      zero = gh_int2scm(0);
      for (kp=0;kp<num;kp++)
	{
	  NEXT_SAMPLE(val,sfs[0]);
	  gh_vector_set_x(args,zero,gh_double2scm((double)(MUS_SAMPLE_TO_FLOAT(val))));
	  res = g_call2(proc,args,gh_chans);
	  if ((SCM_NFALSEP(res)) || (ss->eval_error)) {pos = kp+beg; break;}
	  if (reporting) 
	    {
	      rpt++;
	      if (rpt > rpt4)
		{
		  progress_report(ss,sp,"scan",1,1,(Float)kp / (Float)num,NOT_FROM_ENVED);
		  rpt = 0;
		  check_for_event(ss);
 		  if ((ss->stopped_explicitly) || (!(cp->sound))) break;
		}
	    }
	}
    }
  else
    {
      for (kp=0;kp<num;kp++)
	{
	  for (ip=0;ip<si->chans;ip++)
	    {
	      NEXT_SAMPLE(val,sfs[ip]);
	      gh_vector_set_x(args,gh_int2scm(ip),gh_double2scm((double)(MUS_SAMPLE_TO_FLOAT(val))));
	    }
	  res = g_call2(proc,args,gh_chans);
	  if ((SCM_NFALSEP(res)) || (ss->eval_error)) {pos = kp+beg; break;}
	  if (reporting) 
	    {
	      rpt++;
	      if (rpt > rpt4)
		{
		  progress_report(ss,sp,"scan",1,1,(Float)kp / (Float)num,NOT_FROM_ENVED);
		  rpt = 0;
 		  if (ss->stopped_explicitly) break;
		}
	    }
	}
    }
  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
  for (ip=0;ip<si->chans;ip++) free_snd_fd(sfs[ip]);
  free_sync_state(sc); 
  if (ss->stopped_explicitly)
    {
      cgbuf = (char *)CALLOC(128,sizeof(char));
      sprintf(cgbuf,"C-G stopped scan at sample %d",kp+beg);
      report_in_minibuffer(sp,cgbuf);
      FREE(cgbuf);
      ss->stopped_explicitly = 0;
      return(SCM_BOOL_F);
    }
  else
    {
      if (ss->eval_error)
	{
	  ss->eval_error = 0;
	  return(SND_EVAL_ERROR);
	}
      else
	{
	  if (SCM_FALSEP(res))
	    return(g_call2(proc,SCM_BOOL_F,SCM_BOOL_F));
	  else return(gh_int2scm(pos));
	}
    }
}

/* someday it might be good to tie this into the rest of this file */
typedef struct {
  int buffer_size,data_size,datumb,loc,fd,filing,orig_size;
  char *filename;
  MUS_SAMPLE_TYPE *buffer;
  MUS_SAMPLE_TYPE **mus_data;
  file_info *hdr,*sp_hdr;
} output_state;

static output_state *start_output(int bufsize, file_info *default_hdr, int orig_size)
{
  output_state *os;
  os = (output_state *)CALLOC(1,sizeof(output_state));
  os->buffer_size = bufsize;
  os->buffer = (MUS_SAMPLE_TYPE *)CALLOC(bufsize,sizeof(MUS_SAMPLE_TYPE));
  os->filing = 0;
  os->sp_hdr = default_hdr;
  os->loc = 0;
  os->data_size = 0;
  os->orig_size = orig_size;
  os->hdr = NULL;
  return(os);
}

static void output_sample(snd_state *ss, output_state *os, MUS_SAMPLE_TYPE sample)
{
  os->buffer[os->loc] = sample;
  os->loc++;
  os->data_size++;
  if (os->loc == os->buffer_size)
    {
      if (os->filing == 0)
	{
	  os->filename = snd_tempnam(ss);
	  os->filing = 1;
	  os->hdr = make_temp_header(ss,os->filename,os->sp_hdr,0);
	  (os->hdr)->chans = 1;
	  os->fd = open_temp_file(os->filename,1,os->hdr,ss);
	  os->datumb = mus_data_format_to_bytes_per_sample((os->hdr)->format);
	  os->mus_data = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
	  os->mus_data[0] = os->buffer;
	}
      mus_file_write(os->fd,0,os->loc-1,1,os->mus_data);
      os->loc = 0;
    }
}

static output_state *end_output(output_state *os, int beg, chan_info *cp, char *origin)
{
  int cured;
  if (os->data_size > 0)
    {
      if (os->filing)
	{
	  if (os->loc > 0) mus_file_write(os->fd,0,os->loc-1,1,os->mus_data);
	  close_temp_file(os->fd,os->hdr,os->data_size * os->datumb,cp->sound);
	  if (os->data_size == os->orig_size)
	    file_change_samples(beg,os->data_size,os->filename,cp,0,DELETE_ME,LOCK_MIXES,origin);
	  else
	    {
	      cured = cp->edit_ctr;
	      delete_samples(beg,os->orig_size,cp,origin);
	      file_insert_samples(beg,os->data_size,os->filename,cp,0,DELETE_ME,origin);
	      backup_edit_list(cp);
	      if (cp->edit_ctr > cured) backup_edit_list(cp);
	      if (cp->marks) ripple_trailing_marks(cp,beg,os->orig_size,os->data_size);
	    }
	  free(os->filename);
	  FREE(os->mus_data);
	  if (os->hdr) os->hdr = free_file_info(os->hdr);
	}
      else 
	{
	  if (os->orig_size == os->data_size)
	    change_samples(beg,os->data_size,os->buffer,cp,LOCK_MIXES,origin);
	  else
	    {
	      cured = cp->edit_ctr;
	      delete_samples(beg,os->orig_size,cp,origin);
	      insert_samples(beg,os->data_size,os->buffer,cp,origin);
	      backup_edit_list(cp);
	      if (cp->edit_ctr > cured) backup_edit_list(cp);
	      if (cp->marks) ripple_trailing_marks(cp,beg,os->orig_size,os->data_size);
	    }
	}
      update_graph(cp,NULL);
    }
  FREE(os->buffer);
  FREE(os);
  return(NULL);
}

static SCM series_map(snd_state *ss, chan_info *cp, SCM proc, int chan_choice, int beg, int end, char *origin)
{
  sync_state *sc=NULL;
  sync_info *si;
  snd_info *sp;
  snd_fd **sfs;
  snd_fd *sf;
  output_state *os = NULL;
  int kp,j,k,ip,num,val_size,reporting = 0,rpt = 0,rpt4;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE *vals;
  SCM res;
  sp = cp->sound;
  switch (chan_choice)
    {
    case SCAN_SYNCD_CHANS: sc = get_sync_state(ss,sp,cp,beg,FALSE,READ_FORWARD); break;
    case SCAN_ALL_CHANS: sc = get_active_chans_sync_state(ss,beg); break;
    case SCAN_SOUND_CHANS: sc = get_sound_chans_sync_state(cp,beg); break;
    case SCAN_CURRENT_CHAN: sc = get_chan_sync_state(cp,beg); break;
    }
  if (sc == NULL) return(SCM_BOOL_T);
  si = sc->si;
  sfs = sc->sfs;
  rpt4 = MAX_BUFFER_SIZE / 4;
  for (ip=0;ip<si->chans;ip++)
    {
      cp = si->cps[ip];
      sp = cp->sound;
      sf = sfs[ip];
      if (end == 0) end = current_ed_samples(cp) - 1;
      num = end-beg+1;
      if (num > 0)
	{
	  reporting = (num > MAX_BUFFER_SIZE);
	  if (reporting) start_progress_report(ss,sp,NOT_FROM_ENVED);
	  os = start_output(MAX_BUFFER_SIZE,sp->hdr,num);
	  for (kp=0;kp<num;kp++)
	    {
	      NEXT_SAMPLE(val,sf);
	      res = g_call1(proc,gh_double2scm((double)(MUS_SAMPLE_TO_FLOAT(val))));
	      if (ss->eval_error)
		{
		  end_output(os,beg,cp,origin);
		  for (j=ip;j<si->chans;j++) free_snd_fd(sfs[j]);    
		  free_sync_state(sc); 
		  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
		  ss->eval_error = 0;
		  return(SND_EVAL_ERROR);
		}
	      if (SCM_NFALSEP(res)) /* if #f, no output on this pass */
		{
		  if (res != SCM_BOOL_T) /* if #t we halt the entire map */
		    {
		      if (gh_number_p(res)) /* one number -> replace current sample */
			output_sample(ss,os,MUS_FLOAT_TO_SAMPLE(gh_scm2double(res)));
		      else /* list or vector or vct, splice in data */
			{
			  val_size = 0;
			  vals = g_floats_to_samples(res,&val_size,origin,1);
			  if (vals)
			    {
			      for (k=0;k<val_size;k++) output_sample(ss,os,vals[k]);
			      FREE(vals);
			    }
			}
		    }
		  else /* #t -> halt */
		    {
		      os = end_output(os,beg,cp,origin);
		      for (j=ip;j<si->chans;j++) free_snd_fd(sfs[j]);    
		      free_sync_state(sc); 
		      if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
		      return(res);
		    }
		}
	      if (reporting) 
		{
		  rpt++;
		  if (rpt > rpt4)
		    {
		      progress_report(ss,sp,origin,ip+1,si->chans,(Float)kp / (Float)num,NOT_FROM_ENVED);
		      rpt = 0;
		      if (ss->stopped_explicitly) 
			{
			  os = end_output(os,beg,cp,origin);
			  for (j=ip;j<si->chans;j++) free_snd_fd(sfs[j]);    
			  free_sync_state(sc); 
			  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
			  ss->stopped_explicitly = 0;
			  return(SCM_BOOL_F);
			}
		    }
		}
	    }
	  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
	  sfs[ip] = free_snd_fd(sfs[ip]);
	  os = end_output(os,beg,cp,origin);
	}
    }
  free_sync_state(sc);
  return(g_call1(proc,SCM_BOOL_F));
}


static SCM parallel_map(snd_state *ss, chan_info *cp, SCM proc, int chan_choice, int beg, int end, char *origin)
{
  sync_state *sc=NULL;
  sync_info *si;
  snd_info *sp;
  snd_fd **sfs;
  output_state **os_arr;
  int kp,k,n,ip,len,num,val_size,res_size,reporting = 0,rpt = 0,rpt4;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE *vals;
  SCM res=SCM_UNDEFINED,args,gh_chans,resval;
  char *cgbuf;

  sp = cp->sound;
  switch (chan_choice)
    {
    case SCAN_SYNCD_CHANS: sc = get_sync_state(ss,sp,cp,beg,FALSE,READ_FORWARD); break;
    case SCAN_ALL_CHANS: sc = get_active_chans_sync_state(ss,beg); break;
    case SCAN_SOUND_CHANS: sc = get_sound_chans_sync_state(cp,beg); break;
    case SCAN_CURRENT_CHAN: sc = get_chan_sync_state(cp,beg); break;
    }
  if (sc == NULL) return(SCM_BOOL_T);
  si = sc->si;
  sfs = sc->sfs;
  rpt4 = MAX_BUFFER_SIZE / 4;
  len = current_ed_samples(si->cps[0]);
  for (ip=1;ip<si->chans;ip++)
    {
      num = current_ed_samples(si->cps[ip]);
      if (num > len) len = num;
    }
  if (end == 0) end = len-1;
  num = end-beg+1;
  reporting = (num > MAX_BUFFER_SIZE);
  if (reporting) start_progress_report(ss,sp,NOT_FROM_ENVED);
  os_arr = (output_state **)CALLOC(si->chans,sizeof(output_state *)); 
  for (ip=0;ip<si->chans;ip++)
    os_arr[ip] = start_output(MAX_BUFFER_SIZE,sp->hdr,num);
  gh_chans = gh_int2scm(si->chans);
  args = gh_make_vector(gh_chans,SCM_BOOL_F);
  for (kp=0;kp<num;kp++)
    {
      for (ip=0;ip<si->chans;ip++)
	{
	  NEXT_SAMPLE(val,sfs[ip]);
	  gh_vector_set_x(args,gh_int2scm(ip),gh_double2scm((double)(MUS_SAMPLE_TO_FLOAT(val))));
	}
      res = g_call2(proc,args,gh_chans);
      /* #f -> no output in any channel, #t -> halt */
      if (ss->eval_error) break;
      if (SCM_NFALSEP(res)) /* if #f, no output on this pass */
	{
	  if (res != SCM_BOOL_T) /* if #t we halt the entire map */
	    {
	      /* assume res here is a vector */
	      if (gh_vector_p(res))
		{
		  res_size = gh_vector_length(res);
		  for (n=0;n<res_size;n++)
		    {
		      resval = gh_vector_ref(res,gh_int2scm(n));
		      if (SCM_NFALSEP(resval))
			{
			  if (gh_number_p(resval)) /* one number -> replace current sample */
			    output_sample(ss,os_arr[n],MUS_FLOAT_TO_SAMPLE(gh_scm2double(resval)));
			  else /* list or vector or vct, splice in data */
			    {
			      val_size = 0;
			      vals = g_floats_to_samples(resval,&val_size,origin,1);
			      if (vals)
				{
				  for (k=0;k<val_size;k++) output_sample(ss,os_arr[n],vals[k]);
				  FREE(vals);
				}
			    }
			}
		    }
		}
	      else break; /* #t -> halt */
	    }
	}
      if (reporting) 
	{
	  rpt++;
	  if (rpt > rpt4)
	    {
	      progress_report(ss,sp,origin,ip+1,si->chans,(Float)kp / (Float)num,NOT_FROM_ENVED);
	      rpt = 0;
	      if (ss->stopped_explicitly) break;
	    }
	}
    }
  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
  for (ip=0;ip<si->chans;ip++) 
    {
      os_arr[ip] = end_output(os_arr[ip],beg,si->cps[ip],origin);
      sfs[ip] = free_snd_fd(sfs[ip]);
    }
  FREE(os_arr);
  free_sync_state(sc);
  if (ss->eval_error)
    {
      ss->eval_error = 0;
      return(SND_EVAL_ERROR);
    }
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = 0;
      cgbuf = (char *)CALLOC(128,sizeof(char));
      sprintf(cgbuf,"C-G stopped map at sample %d",kp+beg);
      report_in_minibuffer(sp,cgbuf);
      FREE(cgbuf);
      return(SCM_BOOL_F);
    }
  else
    {
      if (SCM_FALSEP(res))
	return(g_call2(proc,SCM_BOOL_F,SCM_BOOL_F));
      else return(gh_int2scm(kp+beg));
    }
}

#endif /* HAVE_GUILE */

int save_selection(snd_state *ss, char *ofile, int type, int format, int srate, char *comment)
{
  /* can't use save_region here because the currently selected data may not
   * match region(0) -- reverse-selection, for example, reverses the underlying
   * data, but leaves region(0) unchanged -- maybe this is a bug?
   */
  int ofd,oloc,comlen,err=0;
  sync_state *sc;
  sync_info *si;
  int i,dur,j,k;
  MUS_SAMPLE_TYPE val;
  snd_fd **sfs;
  MUS_SAMPLE_TYPE **data;
  if (MUS_DATA_FORMAT_OK(format))
    {
      if (MUS_HEADER_TYPE_OK(type))
	{
	  sc = get_sync_state(ss,NULL,NULL,0,TRUE,READ_FORWARD);
	  if (sc == NULL) return(SND_NO_ERROR);
	  si = sc->si;
	  sfs = sc->sfs;
	  comlen = snd_strlen(comment);
	  dur = sc->dur;
	  if ((snd_write_header(ss,ofile,type,srate,si->chans,28,si->chans*dur,format,comment,comlen,NULL)) == -1) 
	    {
	      free_sync_state(sc);
	      return(SND_CANNOT_WRITE_HEADER);
	    }
	  oloc = mus_header_data_location();
	  if ((ofd = snd_reopen_write(ss,ofile)) == -1) 
	    {
	      free_sync_state(sc);
	      return(SND_CANNOT_OPEN_TEMP_FILE);
	    }
	  mus_file_set_descriptors(ofd,ofile,format,mus_data_format_to_bytes_per_sample(format),oloc,si->chans,type);
	  mus_file_set_data_clipped(ofd,data_clipped(ss));
	  mus_file_seek(ofd,oloc,SEEK_SET);
	  data = (MUS_SAMPLE_TYPE **)CALLOC(si->chans,sizeof(MUS_SAMPLE_TYPE *));
	  for (i=0;i<si->chans;i++) data[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 
	  j = 0;
	  for (i=0;i<dur;i++)
	    {
	      for (k=0;k<si->chans;k++)
		{
		  NEXT_SAMPLE(val,sfs[k]);
		  data[k][j] = val;
		}
	      j++;
	      if (j == FILE_BUFFER_SIZE)
		{
		  err = mus_file_write(ofd,0,j-1,si->chans,data);
		  j = 0;
		  if (err == -1) break;
		}
	    }
	  if (j > 0) mus_file_write(ofd,0,j-1,si->chans,data);
	  for (i=0;i<si->chans;i++)
	    {
	      free_snd_fd(sfs[i]);
	      FREE(data[i]);
	    }
	  FREE(data);
	  free_sync_state(sc);
	  snd_close(ofd);
	  alert_new_file();
	  return(SND_NO_ERROR);
	}
      else 
	{
	  snd_error("unknown header type?!? %d ",type);
	  return(SND_UNSUPPORTED_HEADER_TYPE);
	}
    }
  else snd_error("impossible data format?!? %d ",format);
  return(SND_UNSUPPORTED_DATA_FORMAT);
}

void convolve_with(char *filename, Float amp, chan_info *cp)
{
  /* amp == 0.0 means unnormalized, cp == NULL means current selection */
  sync_state *sc;
  sync_info *si;
  snd_state *ss;
  snd_info *sp = NULL,*gsp = NULL;
  int ip,stop_point = 0,err,impulse_chan = 0,filter_chans,ok=0;
  snd_fd **sfs;
  file_info *hdr;
  int scfd,fltfd,fftsize,ipow,filtersize=0,filesize=0,dataloc,dataformat;
  char *ofile = NULL,*saved_chan_file;
  chan_info *ncp,*ucp;
  char *origin;

  if (cp) 
    {
      ss = cp->state; 
      sp = cp->sound; 
      ncp = cp;
    }
  else
    {
      ss = get_global_state();
      sp = any_selected_sound(ss);
      ncp = any_selected_channel(sp);
    }
  sc = get_sync_state(ss,sp,ncp,0,(cp == NULL),READ_FORWARD);
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;

  origin = (char *)CALLOC(128,sizeof(char));
  sprintf(origin,"%s \"%s\" %.3f",(cp == NULL) ? S_convolve_selection_with : S_convolve_with,filename,amp);
  filter_chans = mus_sound_chans(filename);
  filtersize = mus_sound_samples(filename) / filter_chans;
  /* if impulse response is stereo, we need to use both its channels */
  dataloc = mus_sound_data_location(filename);
  dataformat = mus_sound_data_format(filename);
  if (!(ss->stopped_explicitly))
    {
      for (ip=0;ip<si->chans;ip++)
	{
	  ok = 0;
      	  ucp = si->cps[ip];
	  sp = ucp->sound;
	  if ((ip>0) && (sp != gsp)) finish_progress_report(ss,gsp,NOT_FROM_ENVED);
	  if ((ip == 0) || (sp != gsp)) {gsp = ucp->sound; start_progress_report(ss,gsp,NOT_FROM_ENVED);}

	  /* ofile here = new convolved data */
	  ofile = snd_tempnam(ss);

	  saved_chan_file = snd_tempnam(ss);
	  err = chan_save_edits(ucp,saved_chan_file);
	  if (err != SND_NO_ERROR)
	    snd_error("save chan: %s, %s\n",strerror(errno),snd_error_name(err));
	  else
	    {
	      scfd = mus_file_open_read(saved_chan_file);
	      if (scfd == -1) 
		snd_error("open saved chan file: %s\n",strerror(errno));
	      else
		{
		  hdr = sp->hdr;
		  mus_file_set_descriptors(scfd,saved_chan_file,
					   hdr->format,mus_data_format_to_bytes_per_sample(hdr->format),hdr->data_location,
					   1,hdr->type); /* ??? */
		  fltfd = mus_file_open_read(filename);
		  if (fltfd == -1) 
		    snd_error("open filter file: %s\n",strerror(errno));
		  else
		    {
		      mus_file_set_descriptors(fltfd,filename,
					       dataformat,mus_data_format_to_bytes_per_sample(dataformat),dataloc,
					       filter_chans,mus_sound_header_type(filename));
		      if (cp == NULL)
			filesize = region_len(0);
		      else filesize = current_ed_samples(ucp);
		      if (filesize > 0)
			{
			  ipow = (int)(ceil(log(filtersize + filesize)/log(2.0))) + 1;
			  fftsize = (int)(pow(2.0,ipow));
			  ok = 1;
			  c_convolve(ss,ofile,amp,scfd,mus_sound_data_location(saved_chan_file),
				     fltfd,dataloc,filtersize,fftsize,filter_chans,impulse_chan,filtersize + filesize + 1,
				     gsp,NOT_FROM_ENVED,ip,si->chans);
			  impulse_chan++;
			  if (impulse_chan >= filter_chans) impulse_chan = 0;
			}
		      mus_file_close(fltfd);
		    }
		}
	      mus_file_close(scfd);
	    }
	  mus_sound_forget(saved_chan_file);
	  remove(saved_chan_file);
	  free(saved_chan_file);

	  if (ok)
	    {
	      if (cp == NULL)
		{
		  ok = delete_selection(origin,DONT_UPDATE_DISPLAY);
		  file_insert_samples(si->begs[ip],filtersize + filesize,ofile,ucp,0,DELETE_ME,origin);
		  if (ok) backup_edit_list(ucp); /* pray... */
		  if (ucp->marks) ripple_trailing_marks(ucp,si->begs[ip],sc->dur,filtersize+filesize);
		}
	      else file_override_samples(filtersize + filesize,ofile,ucp,0,DELETE_ME,LOCK_MIXES,origin);
	      update_graph(ucp,NULL); 
	    }
	  if (ofile) free(ofile);
	  sfs[ip] = free_snd_fd(sfs[ip]);
	  check_for_event(ss);
	  if (ss->stopped_explicitly) 
	    {
	      stop_point = ip;
	      break;
	    }
	}
    }
  if (origin) {FREE(origin); origin = NULL;}
  if (gsp) finish_progress_report(ss,gsp,NOT_FROM_ENVED);
  if (ss->stopped_explicitly)
    {
      /* clean up and undo all edits up to stop_point */
      ss->stopped_explicitly = 0;
      for (ip=0;ip<=stop_point;ip++)
	{
	  ucp = si->cps[ip];
	  undo_edit(ucp,1);
	}
    }
  free_sync_state(sc);
}


/* amplitude scaling */

static void scale_with(snd_state *ss, sync_state *sc, Float *scalers, char *origin)
{
  sync_info *si;
  snd_fd **sfs;
  chan_info *cp;
  snd_info *sp;
  int i,scdur,dur,k,lim;
  snd_fd *sf;
  file_info *hdr = NULL;
  int j,ofd = 0,datumb = 0,temp_file,err=0;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  int reporting = 0;
  Float env_val;
  char *ofile = NULL;
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;
  /* for each decide whether a file or internal array is needed, scale, update edit tree */
  data = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 
  for (i=0;i<si->chans;i++)
    {
      /* done channel at a time here, rather than in parallel as in apply_env because */
      /* in this case, the various sync'd channels may be different lengths */
      cp = si->cps[i];
      sp = cp->sound;
      if (scdur == 0) dur = current_ed_samples(cp); else dur = scdur;
      if (dur == 0) continue;
      reporting = ((sp) && (dur > (MAX_BUFFER_SIZE * 10)));
      if (reporting) start_progress_report(ss,sp,NOT_FROM_ENVED);
      if (dur > MAX_BUFFER_SIZE)
	{
	  temp_file = 1; 
	  ofile = snd_tempnam(ss);
	  hdr = make_temp_header(ss,ofile,sp->hdr,dur);
	  hdr->chans = 1;
	  ofd = open_temp_file(ofile,1,hdr,ss);
	  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
	}
      else temp_file = 0;
      sf = sfs[i];
      idata = data[0];
      env_val = scalers[i];
      j = 0;
      /* handle a couple special cases */
      if (env_val == 0.0)
	{
	  if (dur < MAX_BUFFER_SIZE) lim = dur; else lim = MAX_BUFFER_SIZE;
#if HAVE_MEMSET
	  memset((void *)idata,0,(lim * sizeof(MUS_SAMPLE_0)));
#else
	  for (k=0;k<lim;k++) idata[k] = MUS_SAMPLE_0;
#endif
	  if (temp_file)
	    {
	      for (k=0;k<dur;k+=MAX_BUFFER_SIZE)
		{
		  if ((k+MAX_BUFFER_SIZE)<dur) j=MAX_BUFFER_SIZE; else j=dur-k;
		  err = mus_file_write(ofd,0,j-1,1,data);
		  j=0;
		  if (err == -1) break;
		  if (reporting) progress_report(ss,sp,"scl",i+1,si->chans,(Float)k / (Float)dur,NOT_FROM_ENVED);
		}
	    }
	}
      else
	{
	  if (env_val == -1.0)
	    {
	      /* this case doesn't actually run much faster (15% or so) */
	      for (k=0;k<dur;k++)
		{
		  NEXT_SAMPLE(val,sf);
		  idata[j] = -val;
		  j++;
		  if (temp_file)
		    {
		      if (j == MAX_BUFFER_SIZE)
			{
			  err = mus_file_write(ofd,0,j-1,1,data);
			  j=0;
			  if (err == -1) break;
			  if (reporting) progress_report(ss,sp,"scl",i+1,si->chans,(Float)k / (Float)dur,NOT_FROM_ENVED);
			}
		    }
		}
	    }
	  else
	    {
	      for (k=0;k<dur;k++)
		{
		  NEXT_SAMPLE(val,sf);
		  idata[j] = (MUS_SAMPLE_TYPE)(val*env_val);
		  j++;
		  if (temp_file)
		    {
		      if (j == MAX_BUFFER_SIZE)
			{
			  err = mus_file_write(ofd,0,j-1,1,data);
			  j=0;
			  if (err == -1) break;
			  if (reporting) progress_report(ss,sp,"scl",i+1,si->chans,(Float)k / (Float)dur,NOT_FROM_ENVED);
			}
		    }
		}
	    }
	}
      if (temp_file)
	{
	  if (j > 0) mus_file_write(ofd,0,j-1,1,data);
	  close_temp_file(ofd,hdr,dur*datumb,sp);
	  free_file_info(hdr);
	  file_change_samples(si->begs[i],dur,ofile,cp,0,DELETE_ME,LOCK_MIXES,origin); /* was beg 0 -- can't be right for selection */
	  if (ofile) {free(ofile); ofile=NULL;}
	  if (reporting) finish_progress_report(ss,sp,NOT_FROM_ENVED);
	}
      else change_samples(si->begs[i],dur,data[0],cp,LOCK_MIXES,origin);
      update_graph(cp,NULL); /* is this needed? */
      if (ofile) free(ofile);
      free_snd_fd(sfs[i]);
    }
  FREE(data[0]);
  FREE(data);
}

void scale_by(snd_state *ss, snd_info *sp, chan_info *cp, Float *ur_scalers, int len, int selection)
{
  /* if selection, sync to current selection, else sync to current sound */
  sync_state *sc;
  sync_info *si;
  int i,chans,nlen;
  Float *scalers = NULL;
  sc = get_sync_state(ss,sp,cp,0,selection,READ_FORWARD);
  if (sc == NULL) return;
  si = sc->si;
  chans = si->chans;
  scalers = (Float *)CALLOC(chans,sizeof(Float));
  if (chans<len) nlen=chans; else nlen=len;
  for (i=0;i<nlen;i++) scalers[i] = ur_scalers[i];
  if (chans > len)
    for (i=len;i<chans;i++) scalers[i] = ur_scalers[len-1];
  scale_with(ss,sc,scalers,S_scale_by);
  FREE(scalers);
  free_sync_state(sc);
}

Float get_maxamp(snd_info *sp, chan_info *cp)
{
  snd_fd *sf;
  MUS_SAMPLE_TYPE ymax,val;
  int i;
  if (!sp) return(0.0);
  if (!cp) cp = sp->chans[0];
  if (amp_env_maxamp_ok(cp)) return(amp_env_maxamp(cp));
  sf = init_sample_read(0,cp,READ_FORWARD);
  ymax = 0;
  for (i=0;i<current_ed_samples(cp);i++)
    {
      NEXT_SAMPLE(val,sf);
      if (val < 0) val = -val;
      if (val > ymax) ymax = val;
    }
  free_snd_fd(sf);
  return(MUS_SAMPLE_TO_FLOAT(ymax));
}

void scale_to(snd_state *ss, snd_info *sp, chan_info *cp, Float *ur_scalers, int len, int selection)
{
  /* essentially the same as scale-by, but first take into account current maxamps */
  /* here it matters if more than one arg is given -- if one, get overall maxamp */
  /*   if more than one, get successive maxamps */
  int i,chans,nlen;
  sync_state *sc;
  sync_info *si;
  chan_info *ncp;
  Float *scalers;
  Float maxamp,val;
  if ((!selection) && (cp == NULL)) return;
  sc = get_sync_state(ss,sp,cp,0,selection,READ_FORWARD);
  if (sc == NULL) return;
  si = sc->si;
  chans = si->chans;
  scalers = (Float *)CALLOC(chans,sizeof(Float));
  if (chans<len) nlen=chans; else nlen=len;
  for (i=0;i<nlen;i++) scalers[i] = ur_scalers[i];
  if (chans > len)
    for (i=len;i<chans;i++) scalers[i] = ur_scalers[len-1];
  /* now find maxamps (special if len==1) and fixup the scalers */
  if (len == 1)
    {
      maxamp = 0.0;
      for (i=0;i<chans;i++)
	{
	  ncp = si->cps[i];
	  val = get_maxamp(ncp->sound,ncp);
	  if (val > maxamp) maxamp = val;
	}
      if ((data_clipped(ss) == 0) && (scalers[0] == 1.0) && (mus_data_format_to_bytes_per_sample((sp->hdr)->format) < 4)) scalers[0] = 32767.0/32768.0;
      /* 1.0 = -1.0 in these cases, so we'll get a click  -- added 13-Dec-99 */
      val = scalers[0]/maxamp;
      for (i=0;i<chans;i++) scalers[i] = val;
    }
  else
    {
      for (i=0;i<chans;i++)
	{
	  ncp = si->cps[i];
	  val = get_maxamp(ncp->sound,ncp);
	  if ((data_clipped(ss) == 0) && (scalers[i] == 1.0) && (mus_data_format_to_bytes_per_sample((sp->hdr)->format) < 4)) scalers[i] = 32767.0/32768.0;
	  scalers[i] /= val;
	}
    }
  scale_with(ss,sc,scalers,S_scale_to);
  FREE(scalers);
  free_sync_state(sc);
}

static void swap_channels(snd_state *ss, int beg, int dur, snd_fd *c0, snd_fd *c1)
{
  chan_info *cp0,*cp1;
  snd_info *sp0;
  file_info *hdr0 = NULL,*hdr1 = NULL;
  int j,k,ofd0 = 0,ofd1 = 0,datumb = 0,temp_file,err=0;
  MUS_SAMPLE_TYPE **data0,**data1;
  MUS_SAMPLE_TYPE *idata0,*idata1;
  int reporting = 0;
  char *ofile0 = NULL,*ofile1 = NULL;
  if (dur <= 0) return;
  data0 = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data0[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 
  data1 = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data1[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 
  cp0 = c0->cp;
  sp0 = cp0->sound;
  cp1 = c1->cp;
  reporting = ((sp0) && (dur > (MAX_BUFFER_SIZE * 10)));
  if (reporting) start_progress_report(ss,sp0,NOT_FROM_ENVED);
  if (dur > MAX_BUFFER_SIZE)
    {
      temp_file = 1; 
      ofile0 = snd_tempnam(ss);
      hdr0 = make_temp_header(ss,ofile0,sp0->hdr,dur);
      hdr0->chans = 1;
      ofd0 = open_temp_file(ofile0,1,hdr0,ss);
      datumb = mus_data_format_to_bytes_per_sample(hdr0->format);
      ofile1 = snd_tempnam(ss);
      hdr1 = make_temp_header(ss,ofile1,sp0->hdr,dur);
      hdr1->chans = 1;
      ofd1 = open_temp_file(ofile1,1,hdr1,ss);
    }
  else temp_file = 0;
  idata0 = data0[0];
  idata1 = data1[0];
  j = 0;
  for (k=0;k<dur;k++)
    {
      NEXT_SAMPLE(idata0[j],c1);
      NEXT_SAMPLE(idata1[j],c0);
      j++;
      if (temp_file)
	{
	  if (j == MAX_BUFFER_SIZE)
	    {
	      err = mus_file_write(ofd0,0,j-1,1,data0);
	      err = mus_file_write(ofd1,0,j-1,1,data1);
	      j=0;
	      if (err == -1) break;
	      if (reporting) progress_report(ss,sp0,"scl",1,1,(Float)k / (Float)dur,NOT_FROM_ENVED);
	    }
	}
    }
  if (temp_file)
    {
      if (j > 0) 
	{
	  mus_file_write(ofd0,0,j-1,1,data0);
	  mus_file_write(ofd1,0,j-1,1,data1);
	}
      close_temp_file(ofd0,hdr0,dur*datumb,sp0);
      close_temp_file(ofd1,hdr1,dur*datumb,sp0); /* sp0 used here in case of error report */
      free_file_info(hdr0);
      free_file_info(hdr1);
      file_change_samples(beg,dur,ofile0,cp0,0,DELETE_ME,LOCK_MIXES,S_swap_channels);
      file_change_samples(beg,dur,ofile1,cp1,0,DELETE_ME,LOCK_MIXES,S_swap_channels);
      if (ofile0) {free(ofile0); ofile0=NULL;}
      if (ofile1) {free(ofile1); ofile1=NULL;}
      if (reporting) finish_progress_report(ss,sp0,NOT_FROM_ENVED);
    }
  else 
    {
      change_samples(beg,dur,data0[0],cp0,LOCK_MIXES,S_swap_channels);
      change_samples(beg,dur,data1[0],cp1,LOCK_MIXES,S_swap_channels);
    }
  update_graph(cp0,NULL);
  update_graph(cp1,NULL);
  if (ofile0) free(ofile0);
  if (ofile1) free(ofile1);
  FREE(data0[0]);
  FREE(data0);
  FREE(data1[0]);
  FREE(data1);
}

typedef struct {int selection; int files; char **old_filenames; char **new_filenames; void *sc;} snd_exf;

static snd_exf *snd_to_temp(chan_info *cp, int selection, int one_file, int header_type, int data_format)
{
  /* save current state starting at cp and following sync buttons
   *   just current selection if selection, else entire channel
   *   in one file is one_file, else separate files.
   *   return array of temp file names
   * this is intended as the first third of the editor interface to an arbitrary external program.
   *   the next stage calls the external program passing the temp file names and getting new temps.
   *   the third step is temp_to_snd -- snd assumes it is deciding when these temps can be deleted.
   *   if a name is null in the returned array (third step input) no edit is performed on the corresponding channel
   */
  snd_state *ss;
  snd_info *sp;
  sync_state *sc;
  sync_info *si;
  int i,chans,len;
  file_info *nhdr;
  snd_fd **temp_sfs;
  snd_exf *data = NULL;
  if (!cp) return(NULL);
  ss = cp->state;
  sp = cp->sound;
  if (!sp->inuse) return(NULL);
  sc = get_sync_state(ss,sp,cp,0,selection,READ_FORWARD);
  if (sc == NULL) return(NULL);
  si = sc->si;
  chans = si->chans;
  data = (snd_exf *)CALLOC(1,sizeof(snd_exf));
  data->selection = selection;
  if (one_file) data->files = 1; else data->files = chans;
  data->old_filenames = (char **)CALLOC(data->files,sizeof(char *));
  data->new_filenames = (char **)CALLOC(data->files,sizeof(char *));
  for (i=0;i<data->files;i++)
    {
      data->old_filenames[i] = snd_tempnam(ss);
      data->new_filenames[i] = NULL;
    }
  data->sc = (void *)sc;
  if (one_file)
    {
      if (selection) len = sc->dur; else len = current_ed_samples(cp);
      nhdr = copy_header(data->old_filenames[0],sp->hdr);
      if (header_type != MUS_UNSUPPORTED) nhdr->type = header_type;
      if (data_format != MUS_UNSUPPORTED) nhdr->format = data_format;
      snd_make_file(data->old_filenames[0],chans,nhdr,sc->sfs,len,ss);
      free_file_info(nhdr);
    }
  else
    {
      temp_sfs = (snd_fd **)CALLOC(1,sizeof(snd_fd *));
      for (i=0;i<chans;i++)
	{
	  if (selection) len = sc->dur; else len = current_ed_samples(si->cps[i]);
	  nhdr = copy_header(data->old_filenames[i],sp->hdr);
	  if (header_type != MUS_UNSUPPORTED) nhdr->type = header_type;
	  if (data_format != MUS_UNSUPPORTED) nhdr->format = data_format;
	  temp_sfs[0] = sc->sfs[i];
	  snd_make_file(data->old_filenames[i],1,nhdr,temp_sfs,len,ss);
	  nhdr = free_file_info(nhdr);
	}
      FREE(temp_sfs);
    }
  for (i=0;i<chans;i++) sc->sfs[i] = free_snd_fd(sc->sfs[i]);
  return(data);
}

static int temp_to_snd(snd_exf *data, char *origin)
{
  sync_state *sc;
  sync_info *si;
  int i,k,chans,err,new_len,old_len,new_chans,ok,orig_chans;
  if (!data) return(-1);
  sc = (sync_state *)(data->sc);
  si = sc->si;
  chans = si->chans;
  orig_chans = chans;
  for (i=0;i<data->files;i++)
    {
      if (data->old_filenames[i])
	{
	  /* if user didn't re-use the temp file for his output, delete it */
	  if ((data->new_filenames[i] == NULL) || (strcmp(data->new_filenames[i],data->old_filenames[i]) != 0))
	    {
	      mus_sound_forget(data->old_filenames[i]);
	      err = remove(data->old_filenames[i]);
	    }
	  free(data->old_filenames[i]);
	}
    }
  if (data->selection)
    {
      /* should this balk if there's no active selection? */
      old_len = region_len(0);
      if (data->files == 1)
	{
	  if (snd_strlen(data->new_filenames[0]) > 0)
	    {
	      new_len = mus_sound_samples(data->new_filenames[0])/chans;
	      if (new_len != -1)
		{
		  new_chans = mus_sound_chans(data->new_filenames[0]);
		  if (chans != new_chans)
		    {
		      snd_warning("temp-to-selection: original chans: %d, new chans: %d",chans,new_chans);
		      if (chans > new_chans) chans = new_chans;
		    }
		  if (old_len == new_len)
		    {
		      for (k=0;k<chans;k++)
			file_change_samples(si->begs[k],old_len,data->new_filenames[0],si->cps[k],k,DELETE_ME,LOCK_MIXES,origin);
		    }
		  else
		    {
		      ok = delete_selection(origin,DONT_UPDATE_DISPLAY);
		      if (!ok) snd_warning("temp-to-selection: no active selection? (inserting at sample %d...)",si->begs[0]);
		      for (k=0;k<chans;k++)
			{
			  file_insert_samples(si->begs[k],new_len,data->new_filenames[0],si->cps[k],k,DELETE_ME,origin);
			  if (ok) backup_edit_list(si->cps[k]);
			  if ((si->cps[k])->marks) ripple_trailing_marks(si->cps[k],si->begs[k],old_len,new_len);
			}
		    }
		}
	      else snd_warning("temp-to-selection: %s not readable?",data->new_filenames[0]);
	    }
	}
      else
	{
	  ok = delete_selection(origin,DONT_UPDATE_DISPLAY); /* "ok" means there was a selection that was deleted */
	  if (!ok) snd_warning("temps-to-selection: no active selection? (inserting at sample %d...)",si->begs[0]);
	  for (k=0;k<data->files;k++)
	    {
	      if (snd_strlen(data->new_filenames[k]) > 0)
		{
		  new_len = mus_sound_samples(data->new_filenames[k]);
		  if (new_len != -1)
		    {
		      file_insert_samples(si->begs[k],new_len,data->new_filenames[k],si->cps[k],0,DELETE_ME,origin);
		      if (ok) backup_edit_list(si->cps[k]);
		      if ((si->cps[k])->marks) ripple_trailing_marks(si->cps[k],si->begs[k],old_len,new_len);
		    }
		  else snd_warning("temps-to-selection: %s not readable?",data->new_filenames[k]);
		}
	    }
	}
    }
  else
    {
      if (data->files == 1)
	{
	  if (snd_strlen(data->new_filenames[0]) > 0)
	    {
	      new_len = mus_sound_samples(data->new_filenames[0])/chans;
	      if (new_len != -1)
		{
		  new_chans = mus_sound_chans(data->new_filenames[0]);
		  if (chans != new_chans)
		    {
		      snd_warning("temp-to-sound: original chans: %d, new chans: %d",chans,new_chans);
		      if (chans > new_chans) chans = new_chans;
		    }
		  for (k=0;k<chans;k++)
		    file_override_samples(new_len,data->new_filenames[0],si->cps[k],k,DELETE_ME,LOCK_MIXES,origin);
		}
	      else snd_warning("temp-to-sound: %s not readable?",data->new_filenames[0]);
	    }
	}
      else
	{
	  for (k=0;k<data->files;k++)
	    {
	      if (snd_strlen(data->new_filenames[k]) > 0)
		{
		  new_len = mus_sound_samples(data->new_filenames[k]);
		  if (new_len != -1)
		    file_override_samples(new_len,data->new_filenames[k],si->cps[k],0,DELETE_ME,LOCK_MIXES,origin);
		  else snd_warning("temps-to-sound: %s not readable?",data->new_filenames[k]);
		}
	    }
	}
    }
  for (i=0;i<orig_chans;i++)
    {
      update_graph(si->cps[i],NULL); 
      sc->sfs[i] = free_snd_fd(sc->sfs[i]);
    }
  for (i=0;i<data->files;i++)
    if (data->new_filenames[i]) free(data->new_filenames[i]); /* from tempnam */
  FREE(data->new_filenames);
  FREE(data->old_filenames);
  free_sync_state(sc);
  FREE(data);
  return(0);
}


/* -------- src -------- */

static Float input_as_needed(void *arg, int dir) 
{
  MUS_SAMPLE_TYPE val; 
  src_state *sr = (src_state *)arg;
  snd_fd *sf;
  sf = sr->sf;
  if (dir > 0) 
    NEXT_SAMPLE(val,sf);
  else PREVIOUS_SAMPLE(val,sf);
  sr->sample++;
  return(MUS_SAMPLE_TO_FLOAT(val));
}

src_state *make_src(snd_state *ss, Float srate, snd_fd *sf)
{
  src_state *sr;
  sr = (src_state *)CALLOC(1,sizeof(src_state));
  sr->sf = sf;
  sr->gen = mus_make_src(&input_as_needed,srate,sinc_width(ss),(void *)sr);
  sr->sample = 0; /* this is how the old form worked */
  return(sr);
}

Float run_src(src_state *sr, Float sr_change)
{
  return(mus_src(sr->gen,sr_change,&input_as_needed));
}

src_state *free_src(src_state *sr)
{
  mus_free(sr->gen);
  FREE(sr);
  return(NULL);
}

void src_env_or_num(snd_state *ss, chan_info *cp, env *e, Float ratio, int just_num, int from_enved, char *origin, int over_selection)
{
  snd_info *sp = NULL;
  int reporting = 0;
  sync_state *sc;
  sync_info *si;
  snd_fd **sfs;
  snd_fd *sf;
  int i;
  MUS_SAMPLE_TYPE **data;
  file_info *hdr = NULL;
  int j,k,ofd = 0,datumb = 0,ok,err=0;
  char *ofile = NULL;
  MUS_SAMPLE_TYPE *idata;
  src_state *sr;
  int scdur,dur;
  Float *ef = NULL;
  int *old_marks = NULL,*new_marks = NULL;
  mark **mps;
  int cur_mark=0,cur_mark_sample=-1,cur_marks=0,m,need_free_env=0;
  Float env_val,env_incr,xoffset,xscaler,logbase=0.0,step_val;
  int ef_ctr,pass,next_pass,diff,stop_point=0,need_step=0,need_exponential=0;

  if ((!just_num) && (e == NULL)) return;

  /* get envelope or src ratio */
  if (cp == NULL)
    {
      sp = any_selected_sound(ss); 
      if (sp) 
	cp = any_selected_channel(sp); 
      else if (!over_selection) return;
    }
  else sp = cp->sound;
  /* get current syncd chans */
  sc = get_sync_state(ss,sp,cp,0,over_selection,(ratio < 0.0) ? READ_BACKWARD : READ_FORWARD); /* 0->beg, 0->regexpr (ratio = 0.0 if from_enved) */
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;
  data = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 

  if (!just_num)
    {
      need_step = (e->base <= 0.0);
      need_exponential = ((!need_step) && (e->base != 1.0));
      if (need_exponential)
	{
	  xoffset = 0.0;
	  xscaler = 1.0;
	  logbase = log(e->base);
	  ef = fixup_exp_env(e,&xoffset,&xscaler,e->base);
	  if (ef == NULL) return;
	  e = make_envelope(ef,e->pts*2);
	  need_free_env=1;
	  FREE(ef);
	}
    }
  
  if (!(ss->stopped_explicitly))
    {
      for (i=0;i<si->chans;i++)
	{
	  cp = si->cps[i];
	  sp = cp->sound;
	  cur_marks = 0;
	  new_marks = NULL;
	  if (scdur == 0) dur = current_ed_samples(cp); else dur = scdur;
	  if (dur == 0) continue;
	  reporting = ((sp) && (dur > (MAX_BUFFER_SIZE * 4)));
	  if (reporting) start_progress_report(ss,sp,from_enved);
	  ofile = snd_tempnam(ss);
	  hdr = make_temp_header(ss,ofile,sp->hdr,dur);
	  hdr->chans = 1;
	  ofd = open_temp_file(ofile,1,hdr,ss);
	  datumb = mus_data_format_to_bytes_per_sample(hdr->format);
	  sf = sfs[i];
	  idata = data[0];
	  sr = make_src(ss,ratio,sf);
	  j=0;
	  if (just_num)
	    {
	      for (k=0;sr->sample<dur;k++) /* sr->sample tracks input location -- produce output until input exhausted */
		{
		  idata[j]=(MUS_FLOAT_TO_SAMPLE(run_src(sr,0.0)));
		  j++;
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(ofd,0,j-1,1,data);
		      j=0;
		      if (err == -1) break;
		      if (reporting) 
			{
			  progress_report(ss,sp,S_src_sound,i+1,si->chans,(Float)(sr->sample) / (Float)dur,from_enved);
			  if (ss->stopped_explicitly) break;
			}
		    }
		}
	    }
	  else
	    {
	      /* envelope case -- have to go by sr->sample, not output sample counter, also check marks */
	      cur_mark_sample = -1;
	      if ((cp->marks) && (cp->mark_ctr[cp->edit_ctr] >= 0))
		{
		  mps = cp->marks[cp->edit_ctr];
		  cur_marks = cp->mark_ctr[cp->edit_ctr]+1;
		  new_marks = (int *)CALLOC(cur_marks,sizeof(int));
		  old_marks = (int *)CALLOC(cur_marks,sizeof(int));
		  for (m=0;m<cur_marks;m++)
		    {
		      new_marks[m] = -1;
		      old_marks[m] = mps[m]->samp;
		    }
		  for (m=0;m<cur_marks;m++)
		    {
		      if (old_marks[m] > si->begs[i])
			{
			  cur_mark_sample = old_marks[m];
			  cur_mark = m;
			  break;
			}
		    }
		}
	      ef = magify_env(e,dur,1.0); /* dur=input samples (might change, so re-magify) */
	      ef_ctr = 0;
	      env_val = e->data[1];
	      step_val = env_val;
	      env_incr = ef[1];
	      pass = (int)(ef[0]);
	      next_pass = sr->sample;
	      for (k=0;sr->sample<dur;k++)
		{
		  if (need_step)
		    idata[j] = (MUS_FLOAT_TO_SAMPLE(run_src(sr,step_val)));
		  else
		    if (need_exponential)
		      idata[j] = (MUS_FLOAT_TO_SAMPLE(run_src(sr,xoffset + xscaler * (exp(logbase*env_val) - 1.0))));
		    else idata[j] = (MUS_FLOAT_TO_SAMPLE(run_src(sr,env_val)));
		  j++;
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(ofd,0,j-1,1,data);
		      j=0;
		      if (err == -1) break;
		      if (reporting) 
			{
			  progress_report(ss,sp,S_src_sound,i+1,si->chans,(Float)(sr->sample) / (Float)dur,from_enved);
			  if (ss->stopped_explicitly) break;
			}
		    }
		  if (next_pass != sr->sample)  /* tick env forward dependent on sr->sample */
		    {
		      diff = (sr->sample - next_pass);
		      next_pass = sr->sample;
		      if ((new_marks) && (cur_mark_sample != -1) && (next_pass >= (cur_mark_sample - si->begs[i]))) 
			{
#if DEBUGGING
			  if (cur_mark >= cur_marks) abort();
#endif
			  /* not '==' because sr->sample can be incremented by more than 1 */
			  new_marks[cur_mark] = k + si->begs[i];
			  cur_mark++;
			  if (cur_mark < cur_marks)
			    cur_mark_sample = old_marks[cur_mark];
			  else cur_mark_sample = -1;
			}
		      env_val += (diff * env_incr);
		      pass -= diff;
		      if (pass <= 0)
			{
			  ef_ctr += 2;
			  pass = (int)(ef[ef_ctr]);
			  env_incr = ef[ef_ctr+1];
			  step_val = env_val;
			  /* 1 pass segments are special in this case */
			  if (pass == 1)
			    {
			      env_val += env_incr;
			      ef_ctr += 2;
			      pass = (int)(ef[ef_ctr]);
			      env_incr = ef[ef_ctr+1];
			      step_val = env_val;
			    }
			}
		    }
		}
	      FREE(ef);
	    }
	  if (reporting) finish_progress_report(ss,sp,from_enved);
	  sr = free_src(sr);
	  if (j > 0) mus_file_write(ofd,0,j-1,1,data);
	  close_temp_file(ofd,hdr,k*datumb,sp);
	  hdr = free_file_info(hdr);
	  if (over_selection)
	    {
	      /* here we need delete followed by insert since dur is probably different */
	      if (k == dur)
		file_change_samples(si->begs[i],dur,ofile,cp,0,DELETE_ME,LOCK_MIXES,origin);
	      else
		{
		  ok = delete_selection(S_src_selection,DONT_UPDATE_DISPLAY);
		  file_insert_samples(si->begs[i],k,ofile,cp,0,DELETE_ME,S_src_selection);
		  if (ok) backup_edit_list(cp); /* pray... */
		  if (cp->marks) ripple_marks(cp,0,0);
		  update_graph(cp,NULL);
		}
	    }
	  else file_override_samples(k,ofile,cp,0,DELETE_ME,LOCK_MIXES,origin);
	  /* not file_change_samples because that would not necessarily change the current file length */
	  if (cp->marks)
	    {
	      if (just_num)
		src_marks(cp,ratio,dur,k,si->begs[i],over_selection);
	      else
		{
		  if (new_marks) 
		    reset_marks(cp,cur_marks,new_marks,si->begs[i]+dur,(k - dur),over_selection);
		}
	      update_graph(cp,NULL);
	    }
	  if (old_marks) FREE(old_marks);
	  old_marks = NULL;
	  if (new_marks) FREE(new_marks);
	  new_marks = NULL;
	  free(ofile); 
	  ofile=NULL;
	  sfs[i] = free_snd_fd(sfs[i]);
	  if (ss->stopped_explicitly) 
	    {
	      stop_point = i;
	      break;
	    }
	}
    }
  if (ss->stopped_explicitly)
    {
      /* clean up and undo all edits up to stop_point */
      ss->stopped_explicitly = 0;
      for (i=0;i<=stop_point;i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp,1);
	}
    }
  FREE(data[0]);
  FREE(data);
  free_sync_state(sc);
  if ((e) && (need_free_env)) free_env(e);
}


/* FIR filtering */

static Float *get_filter_coeffs(int order, env *e)
{
  /* interpret e as frequency response */
  Float *fdata = NULL,*a = NULL,*ef = NULL;
  Float x;
  env *ne;
  int i,j,cur_pt,ef_ctr,pass;
  Float last_x,step,xoffset,xscaler,logbase,env_pow,env_incr;

  if (!e) return(NULL);
  /* get the frequency envelope and design the FIR filter */
  fdata = (Float *)CALLOC(order,sizeof(Float));
  a = (Float *)CALLOC(order,sizeof(Float));
  last_x = e->data[(e->pts-1)*2];
  step = 2*last_x/((Float)order-1);
  if (e->base == 1.0)
    {
      for (i=0,x=0.0;i<order/2;i++,x+=step) 
	fdata[i] = list_interp(x,e->data,e->pts);
    }
  else
    {
      if (e->base == 0.0)
	{
	  cur_pt = 2;
	  for (i=0,x=0.0;i<order/2;i++,x+=step) 
	    {
	      while ((cur_pt < (e->pts*2)) && (x > e->data[cur_pt])) cur_pt+=2;
	      fdata[i] = e->data[cur_pt - 1];
	    }
	}
      else
	{
	  /* exponential segments */
	  xoffset = 0.0;
	  xscaler = 1.0;
	  logbase = log(e->base);
	  ef = fixup_exp_env(e,&xoffset,&xscaler,e->base);
	  if (ef == NULL) {if (fdata) FREE(fdata); if (a) FREE(a); return(NULL);}
	  ne = make_envelope(ef,e->pts*2);
	  FREE(ef);
	  ef = magify_env(ne,order/2,1.0);
	  env_pow = ne->data[1];
	  env_incr = ef[1];
	  pass = (int)(ef[0]);
	  ef_ctr = 0;
	  for (i=0;i<order/2;i++)
	    {
	      fdata[i] = xoffset + xscaler * (exp(logbase * env_pow) - 1.0);
	      env_pow += env_incr;
	      pass--;
	      if (pass <= 0) 
		{
		  ef_ctr += 2;
		  pass = (int)(ef[ef_ctr]);
		  env_incr = ef[ef_ctr+1];
		}
	    }
	  FREE(ef);
	  free_env(ne);
	}
    }
  for (j=order/2-1,i=order/2;(i<order) && (j>=0);i++,j--) fdata[i] = fdata[j];
  mus_make_fir_coeffs(order,fdata,a);
  FREE(fdata);
  return(a);
}

static Float frequency_response(Float *coeffs, int order, Float frq)
{
  Float at = 0.0,am;
  int n2,i;
  n2 = order>>1;
  am = (order+1) * 0.5;
  for (i=0;i<n2;i++) at += coeffs[i] * cos(M_PI*(am-i-1)*frq);
  if (at<0.0) return(-2*at);
  return(2*at);
}

void apply_filter(chan_info *ncp, int order, env *e, int from_enved, char *origin, int over_selection, Float *ur_a)
{
  /* interpret e as frequency response and apply as filter to all sync'd chans */
  Float *a,*d;
  Float x;
  sync_state *sc;
  sync_info *si;
  snd_state *ss;
  snd_info *sp;
  int reporting = 0;
  int i,m,scdur,dur,k,stop_point = 0,prebeg=0;
  snd_fd **sfs;
  snd_fd *sf;
  file_info *hdr = NULL;
  int j,ofd = 0,datumb = 0,temp_file,err=0;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  char *ofile = NULL;
  chan_info *cp;

  if ((!e) && (!ur_a)) return;
  if (ur_a)
    a = ur_a;
  else a = get_filter_coeffs(order,e);
  if (!a) return;
  d = (Float *)CALLOC(order,sizeof(Float));
  /* now filter all currently sync'd chans (one by one) */
  ss = ncp->state;
  sp = ncp->sound;
  sc = get_sync_state_1(ss,sp,ncp,0,over_selection,READ_FORWARD,(over_selection) ? (order-1) : 0);
  if (sc == NULL) {if (!ur_a) FREE(a); FREE(d); return;}
  si = sc->si;
  sfs = sc->sfs;
  scdur = sc->dur;
  /* for each decide whether a file or internal array is needed, scale, update edit tree */
  data = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 

  if (!(ss->stopped_explicitly))
    {
      for (i=0;i<si->chans;i++)
	{
	  /* done channel at a time here, rather than in parallel as in apply_env because */
	  /* in this case, the various sync'd channels may be different lengths */
	  cp = si->cps[i];
	  sp = cp->sound;
	  if (scdur == 0) dur = current_ed_samples(cp); else dur = scdur;
	  if (dur == 0) continue;
	  reporting = ((sp) && (dur > (MAX_BUFFER_SIZE * 4)));
	  if (reporting) start_progress_report(ss,sp,from_enved);
	  if (dur > MAX_BUFFER_SIZE)
	    {
	      temp_file = 1; 
	      ofile = snd_tempnam(ss);
	      hdr = make_temp_header(ss,ofile,sp->hdr,dur);
	      hdr->chans = 1;
	      ofd = open_temp_file(ofile,1,hdr,ss);
	      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
	    }
	  else temp_file = 0;
	  sf = sfs[i];
	  idata = data[0];
	  for (m=0;m<order;m++) d[m] = 0.0;
	  if (over_selection)
	    {
	      /* see if there's data to pre-load the filter */
	      if (si->begs[i] >= order)
		prebeg = order-1;
	      else prebeg = si->begs[i];
	      if (prebeg > 0)
		{
		  for (m=prebeg;m>0;m--)
		    {
		      NEXT_SAMPLE(val,sf);
		      d[m] = (Float)val;
		    }
		}
	    }
	  j = 0;
	  for (k=0;k<dur;k++)
	    {
	      NEXT_SAMPLE(val,sf);
	      x=0.0; 
	      d[0]=(Float)val;
	      for (m=order-1;m>0;m--) 
		{
		  x+=d[m]*a[m]; 
		  d[m]=d[m-1];
		} 
	      x+=d[0]*a[0]; 
	      idata[j] = (MUS_SAMPLE_TYPE)x;
	      j++;
	      if (temp_file)
		{
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(ofd,0,j-1,1,data);
		      j=0;
		      if (err == -1) break;
		      if (reporting) 
			{
			  progress_report(ss,sp,S_filter_sound,i+1,si->chans,(Float)k / (Float)dur,from_enved);
			  if (ss->stopped_explicitly) break;
			}
		    }
		}
	    }
	  if (reporting) finish_progress_report(ss,sp,from_enved);
	  if (temp_file)
	    {
	      if (j > 0) mus_file_write(ofd,0,j-1,1,data);
	      close_temp_file(ofd,hdr,dur*datumb,sp);
	      hdr = free_file_info(hdr);
	      if (over_selection)
		file_change_samples(si->begs[i],dur,ofile,cp,0,DELETE_ME,LOCK_MIXES,origin);
	      else file_change_samples(0,dur,ofile,cp,0,DELETE_ME,LOCK_MIXES,origin);
	      if (ofile) {free(ofile); ofile=NULL;}
	    }
	  else change_samples(si->begs[i],dur,data[0],cp,LOCK_MIXES,origin);
	  update_graph(cp,NULL); /* is this needed? */
	  sfs[i] = free_snd_fd(sfs[i]);
	  if (ss->stopped_explicitly) 
	    {
	      stop_point = i;
	      break;
	    }
	}
    }

  if (ss->stopped_explicitly)
    {
      /* clean up and undo all edits up to stop_point */
      ss->stopped_explicitly = 0;
      for (i=0;i<=stop_point;i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp,1);
	}
    }

  FREE(data[0]);
  FREE(data);
  if (!ur_a) FREE(a);
  FREE(d);
  free_sync_state(sc);
}

static void reverse_sound(chan_info *ncp, int over_selection)
{
  sync_state *sc;
  sync_info *si;
  snd_state *ss;
  snd_info *sp;
  int i,dur,k,stop_point = 0;
  MUS_SAMPLE_TYPE val;
  snd_fd **sfs;
  snd_fd *sf;
  file_info *hdr = NULL;
  int j,ofd = 0,datumb = 0,temp_file,err=0;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  char *ofile = NULL;
  chan_info *cp;
  ss = ncp->state;
  sp = ncp->sound;
  sc = get_sync_state(ss,sp,ncp,0,over_selection,READ_BACKWARD);
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  data = (MUS_SAMPLE_TYPE **)CALLOC(1,sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 
  if (!(ss->stopped_explicitly))
    {
      for (i=0;i<si->chans;i++)
	{
	  cp = si->cps[i];
	  sp = cp->sound;
	  if (over_selection)
	    dur = sc->dur;
	  else dur = current_ed_samples(cp);
	  if (dur == 0) continue;
	  if (dur > MAX_BUFFER_SIZE)
	    {
	      temp_file = 1; 
	      ofile = snd_tempnam(ss);
	      hdr = make_temp_header(ss,ofile,sp->hdr,dur);
	      hdr->chans = 1;
	      ofd = open_temp_file(ofile,1,hdr,ss);
	      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
	    }
	  else temp_file = 0;
	  sf = sfs[i];
	  idata = data[0];
	  j = 0;
	  for (k=0;k<dur;k++)
	    {
	      PREVIOUS_SAMPLE(val,sf);
	      idata[j] = val;
	      j++;
	      if (temp_file)
		{
		  if (j == MAX_BUFFER_SIZE)
		    {
		      err = mus_file_write(ofd,0,j-1,1,data);
		      j=0;
		      if (err == -1) break;
		    }
		}
	    }
	  if (temp_file)
	    {
	      if (j > 0) mus_file_write(ofd,0,j-1,1,data);
	      close_temp_file(ofd,hdr,dur*datumb,sp);
	      hdr = free_file_info(hdr);
	      if (over_selection)
		file_change_samples(si->begs[i],dur,ofile,cp,0,DELETE_ME,LOCK_MIXES,S_reverse_selection);
	      else file_change_samples(0,dur,ofile,cp,0,DELETE_ME,LOCK_MIXES,S_reverse_sound);
	      if (ofile) {free(ofile); ofile=NULL;}
	    }
	  else change_samples(si->begs[i],dur,data[0],cp,LOCK_MIXES,(over_selection) ? S_reverse_selection : S_reverse_sound);
	  if (cp->marks)
	    {
	      /* marks refer to particular samples, not positions, so they too must be reversed */
	      /* it just occurs to me that mark indices cannot be used across undo/redo */
	      reverse_marks(cp,over_selection);
	    }
	  update_graph(cp,NULL); 
	  sfs[i] = free_snd_fd(sfs[i]);
	  if (ss->stopped_explicitly) 
	    {
	      stop_point = i;
	      break;
	    }
	}
    }
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = 0;
      for (i=0;i<=stop_point;i++)
	{
	  cp = si->cps[i];
	  undo_edit(cp,1);
	}
    }
  FREE(data[0]);
  FREE(data);
  free_sync_state(sc);
}


/* amplitude envelopes */


void apply_env(chan_info *cp, env *e, int beg, int dur, Float scaler, int regexpr, int from_enved, char *origin)
{
  snd_fd *sf;
  snd_info *sp;
  sync_info *si;
  sync_state *sc;
  snd_fd **sfs;
  file_info *hdr;
  Float *ef = NULL;
  double *efd = NULL;
  int i,j,k,ef_ctr,pass,ofd = 0,datumb = 0,temp_file,need_free_env=0,err=0;
  MUS_SAMPLE_TYPE val;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
  int reporting = 0;
  Float env_val,env_incr,step_val;
  double env_vald=0.0,env_incrd;
  int need_doubles=0,need_exponential=0,need_step=0;
  char *ofile = NULL;
  snd_state *ss;
  Float xoffset,xscaler,logbase=0.0,env_powd;

  if (!e) return;
  need_step = (e->base <= 0.0);
  need_exponential = ((!need_step) && (e->base != 1.0) && (fabs(e->base-1.0) > 0.00001));
  need_doubles = (((dur > 5000000) && (!need_step)) || (need_exponential));
  si = NULL;
  sp = cp->sound;
  ss = cp->state;
  hdr = sp->hdr;
  sc = get_sync_state(ss,sp,cp,beg,regexpr,READ_FORWARD);
  if (sc == NULL) return;
  si = sc->si;
  sfs = sc->sfs;
  if (regexpr) dur = sc->dur;
  if (dur == 0) {free_sync_state(sc); return;}
  if (dur > MAX_BUFFER_SIZE) /* if smaller than this, we don't gain anything by using a temp file (its buffers are this large) */
    {
      temp_file = 1; 
      ofile = snd_tempnam(ss); /* see warning below -- don't use tmpnam without deleting free */
      ofd = open_temp_file(ofile,si->chans,hdr,ss);
      datumb = mus_data_format_to_bytes_per_sample(hdr->format);
    }
  else temp_file = 0;
  if (need_exponential)
    {
      xoffset = 0.0;
      xscaler = 1.0;
      logbase = log(e->base);
      ef = fixup_exp_env(e,&xoffset,&xscaler,e->base);
      if (ef == NULL) 
	need_exponential = 0;
      else 
	{
	  e = make_envelope(ef,e->pts*2);
	  need_free_env=1;
	  FREE(ef);
	  ef = NULL;
	  scaler = 1.0;
	}
    }
  if (need_doubles)
    efd = dmagify_env(e,dur,scaler);
  else ef = magify_env(e,dur,scaler);
  data = (MUS_SAMPLE_TYPE **)CALLOC(si->chans,sizeof(MUS_SAMPLE_TYPE *));
  for (i=0;i<si->chans;i++) 
    {
      if (temp_file)
	data[i] = (MUS_SAMPLE_TYPE *)CALLOC(FILE_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE)); 
      else data[i] = (MUS_SAMPLE_TYPE *)CALLOC(dur,sizeof(MUS_SAMPLE_TYPE)); 
    }
  ef_ctr = 0;
  j=0;
  reporting = (dur > (MAX_BUFFER_SIZE * 4));
  if (reporting) start_progress_report(ss,sp,from_enved);
  if (!need_exponential)
    {
      if (!need_step)
	{
	  /* LINEAR SEGMENTS */
	  if (need_doubles)
	    { /* this case can take long enough that it probably should be a background process */
	      env_vald = e->data[1];
	      env_incrd = efd[1];
	      pass = (int)(efd[0]);
	      if (si->chans > 1)
		{
		  for (i=0;i<dur;i++)
		    {
		      for (k=0;k<si->chans;k++)
			{
			  NEXT_SAMPLE(val,sfs[k]);
			  data[k][j] = (MUS_SAMPLE_TYPE)(val*env_vald);
			}
		      env_vald += env_incrd;
		      pass--;
		      if (pass <= 0) 
			{
			  ef_ctr += 2;
			  pass = (int)(efd[ef_ctr]);
			  env_incrd = efd[ef_ctr+1];
			}
		      j++;
		      if (temp_file)
			{
			  if (j == FILE_BUFFER_SIZE)
			    {
			      progress_report(ss,sp,S_env_sound,0,0,(Float)i/((Float)dur),from_enved);
			      err = mus_file_write(ofd,0,j-1,si->chans,data);
			      j=0;
			      if (err == -1) break;
			      if (ss->stopped_explicitly) break;
			    }
			}
		    }
		}
	      else
		{
		  sf = sfs[0];
		  idata = data[0];
		  for (i=0;i<dur;i++)
		    {
		      NEXT_SAMPLE(val,sf);
		      idata[j] = (MUS_SAMPLE_TYPE)(val*env_vald);
		      env_vald += env_incrd;
		      pass--;
		      if (pass <= 0) 
			{
			  ef_ctr += 2;
			  pass = (int)(efd[ef_ctr]);
			  env_incrd = efd[ef_ctr+1];
			}
		      j++;
		      if (temp_file)
			{
			  if (j == FILE_BUFFER_SIZE)
			    {
			      progress_report(ss,sp,S_env_sound,0,0,(Float)i/((Float)dur),from_enved);
			      err = mus_file_write(ofd,0,j-1,1,data);
			      j=0;
			      if (err == -1) break;
			      if (ss->stopped_explicitly) break;
			    }
			}
		    }
		}
	    }
	  else /* Float case */
	    {
	      env_val = e->data[1];
	      env_incr = ef[1];
	      pass = (int)(ef[0]);
	      if (si->chans > 1)
		{
		  for (i=0;i<dur;i++)
		    {
		      for (k=0;k<si->chans;k++)
			{
			  NEXT_SAMPLE(val,sfs[k]);
			  data[k][j] = (MUS_SAMPLE_TYPE)(val*env_val);
			}
		      env_val += env_incr;
		      pass--;
		      if (pass <= 0) 
			{
			  ef_ctr += 2;
			  pass = (int)(ef[ef_ctr]);
			  env_incr = ef[ef_ctr+1];
			}
		      j++;
		      if (temp_file)
			{
			  if (j == FILE_BUFFER_SIZE)
			    {
			      err = mus_file_write(ofd,0,j-1,si->chans,data);
			      j=0;
			      if (err == -1) break;
			      if (reporting) progress_report(ss,sp,S_env_sound,0,0,(Float)i/((Float)dur),from_enved);
			      if (ss->stopped_explicitly) break;
			    }
			}
		    }
		}
	      else
		{
		  sf = sfs[0];
		  idata = data[0];
		  for (i=0;i<dur;i++)
		    {
		      NEXT_SAMPLE(val,sf);
		      idata[j] = (MUS_SAMPLE_TYPE)(val*env_val);
		      env_val += env_incr;
		      pass--;
		      if (pass <= 0) 
			{
			  ef_ctr += 2;
			  pass = (int)(ef[ef_ctr]);
			  env_incr = ef[ef_ctr+1];
			}
		      j++;
		      if (temp_file)
			{
			  if (j == FILE_BUFFER_SIZE)
			    {
			      err = mus_file_write(ofd,0,j-1,1,data);
			      j=0;
			      if (err == -1) break;
			      if (reporting) progress_report(ss,sp,S_env_sound,0,0,(Float)i/((Float)dur),from_enved);
			      if (ss->stopped_explicitly) break;
			    }
			}
		    }
		}
	    }
	}
      else
	{
	  /* STEP SEGMENTS -- I doubt this is actually ever useful */
	  env_val = e->data[1];
	  step_val = env_val;
	  env_incr = ef[1];
	  pass = (int)(ef[0]);
	  for (i=0;i<dur;i++)
	    {
	      for (k=0;k<si->chans;k++)
		{
		  NEXT_SAMPLE(val,sfs[k]);
		  data[k][j] = (MUS_SAMPLE_TYPE)(val*step_val);
		}
	      env_val += env_incr;
	      pass--;
	      if (pass <= 0) 
		{
		  ef_ctr += 2;
		  pass = (int)(ef[ef_ctr]);
		  env_incr = ef[ef_ctr+1];
		  step_val = env_val;
		}
	      j++;
	      if (temp_file)
		{
		  if (j == FILE_BUFFER_SIZE)
		    {
		      err = mus_file_write(ofd,0,j-1,si->chans,data);
		      j=0;
		      if (err == -1) break;
		      if (reporting) progress_report(ss,sp,S_env_sound,0,0,(Float)i/((Float)dur),from_enved);
		      if (ss->stopped_explicitly) break;
		    }
		}
	    }
	}
    }
  else
    {
      /* EXPONENTIAL SEGMENTS */
      env_powd = e->data[1];
      env_incrd = efd[1];
      pass = (int)(efd[0]);
      for (i=0;i<dur;i++)
	{
	  if (env_incrd != 0.0) 
	    {
	      env_vald = xoffset + xscaler * (exp(logbase * env_powd) - 1.0);
	      env_powd += env_incrd;
	    }
	  for (k=0;k<si->chans;k++)
	    {
	      NEXT_SAMPLE(val,sfs[k]);
	      data[k][j] = (MUS_SAMPLE_TYPE)(val*env_vald);
	    }
	  pass--;
	  if (pass <= 0) 
	    {
	      ef_ctr += 2;
	      pass = (int)(efd[ef_ctr]);
	      env_incrd = efd[ef_ctr+1];
	    }
	  j++;
	  if (temp_file)
	    {
	      if (j == FILE_BUFFER_SIZE)
		{
		  progress_report(ss,sp,S_env_sound,0,0,(Float)i/((Float)dur),from_enved);
		  err = mus_file_write(ofd,0,j-1,si->chans,data);
		  j=0;
		  if (err == -1) break;
		  if (ss->stopped_explicitly) break;
		}
	    }
	}
    }
  if (temp_file)
    {
      if (j>0) mus_file_write(ofd,0,j-1,si->chans,data);
      close_temp_file(ofd,hdr,dur*si->chans*datumb,sp);
    }
  if (reporting) finish_progress_report(ss,sp,from_enved);
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = 0;
      if (temp_file) {mus_sound_forget(ofile); remove(ofile);}
    }
  else
    {
      if ((temp_file) && (si->chans > 1)) remember_temp(ofile,si->chans);
      for (i=0;i<si->chans;i++)
	{
	  if (temp_file)
	    file_change_samples(si->begs[i],dur,ofile,si->cps[i],i,(si->chans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,LOCK_MIXES,origin);
	  else change_samples(si->begs[i],dur,data[i],si->cps[i],LOCK_MIXES,origin);
	  update_graph(si->cps[i],NULL); /* is this needed? */
	}
    }
  for (i=0;i<si->chans;i++)
    {
      sfs[i] = free_snd_fd(sfs[i]);
      FREE(data[i]);
    }
  if ((temp_file) && (ofile)) {free(ofile); ofile=NULL;} /* safe only if snd_tempnam, not tmpnam used */
  if (ef) FREE(ef);
  if (efd) FREE(efd);
  if (data) FREE(data);
  free_sync_state(sc);
  if ((e) && (need_free_env)) free_env(e);
}


/* various simple editing ops */

static int cursor_delete(chan_info *cp, int count, char *origin)
{
  int i,beg;
  snd_info *sp;
  sync_info *si;
  chan_info **cps;
  si = NULL;
  beg = cp->cursor;
  sp = cp->sound;
  if (sp->syncing!= 0)
    {
      si = snd_sync(cp->state,sp->syncing);
      cps = si->cps;
      for (i=0;i<si->chans;i++)
	{
	  if (count > 0)
	    delete_samples(beg,count,cps[i],origin); 
	  else delete_samples(beg+count,-count,cps[i],origin);
	  update_graph(si->cps[i],NULL);
	}
      free_sync_info(si);
    }
  else
    {
      if (count > 0)
	delete_samples(beg,count,cp,origin);
      else delete_samples(beg+count,-count,cp,origin);
    }
  return(CURSOR_UPDATE_DISPLAY);
}

static int cursor_delete_previous(chan_info *cp, int count, char *origin)
{
  if (cp->cursor <= 0) return(CURSOR_UPDATE_DISPLAY);
  cp->cursor -= count;
  if (cp->cursor < 0)
    {
      count += cp->cursor;
      cp->cursor = 0;
    }
  return(cursor_delete(cp,count,origin));
}

static int cursor_insert(chan_info *cp, int count)
{
  MUS_SAMPLE_TYPE *zeros;
  int i,beg;
  snd_info *sp;
  sync_info *si;
  chan_info **cps;
  si = NULL;
  if (count < 0) count = -count;
  sp = cp->sound;
  beg = cp->cursor;
  zeros = (MUS_SAMPLE_TYPE *)CALLOC(count,sizeof(MUS_SAMPLE_TYPE));
  if (sp->syncing != 0)
    {
      si = snd_sync(cp->state,sp->syncing);
      cps = si->cps;
      for (i=0;i<si->chans;i++)
	{
	  insert_samples(beg,count,zeros,cps[i],"C-o");
	  update_graph(si->cps[i],NULL);
	}
      free_sync_info(si);
    }
  else
    {
      insert_samples(beg,count,zeros,cp,"C-o");
    }
  FREE(zeros);
  return(CURSOR_UPDATE_DISPLAY);
}

static int cursor_zeros(chan_info *cp, int count, int regexpr)
{
  MUS_SAMPLE_TYPE *zeros;
  int i,num;
  snd_info *sp;
  sync_info *si;
  si = NULL;
  sp = cp->sound;
  if (count < 0) num = -count; else num = count;
  if ((sp->syncing != 0) && (!regexpr))
    {
      si = snd_sync(cp->state,sp->syncing);
      for (i=0;i<si->chans;i++) si->begs[i] = cp->cursor;
    }
  else
    {
      if ((regexpr) && (region_ok(0)))
	{
	  si = region_sync(0);
	  num = region_len(0);
	}
    }
  if (!si) si = make_simple_sync(cp,cp->cursor);
  zeros = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
  for (i=0;i<si->chans;i++)
    {
      if (count < 0) 
	change_samples(si->begs[i]+count,num,zeros,si->cps[i],LOCK_MIXES,"C-z"); 
      else change_samples(si->begs[i],num,zeros,si->cps[i],LOCK_MIXES,"C-z");
      update_graph(si->cps[i],NULL);
    }
  FREE(zeros);
  free_sync_info(si);
  return(CURSOR_IN_VIEW);
}

static sync_state *get_sync_state_without_snd_fds(snd_state *ss, snd_info *sp, chan_info *cp, int beg, int regexpr)
{
  sync_info *si = NULL;
  int dur,i;
  sync_state *sc;
  dur = 0;
  if ((sp->syncing != 0) && (!regexpr))
    {
      si = snd_sync(ss,sp->syncing);
      for (i=0;i<si->chans;i++) si->begs[i] = beg;
    }
  else
    {
      if (regexpr)
	{
	  if (selection_is_current())
	    {
	      si = region_sync(0);
	      dur = region_len(0);
	    }
	  else
	    {
	      snd_warning(STR_no_current_selection);
	      return(NULL);
	    }
	}
    }
  if (si == NULL) 
    {
      si = make_simple_sync(cp,beg);
    }
  sc = (sync_state *)CALLOC(1,sizeof(sync_state));
  sc->dur = dur;
  sc->si = si;
  sc->sfs = NULL;
  return(sc);
}

static void cos_smooth(chan_info *cp, int beg, int num, int regexpr, char *origin)
{
  /* verbatim, so to speak from Dpysnd */
  /* start at beg, apply a cosine for num samples, matching endpoints */
  MUS_SAMPLE_TYPE *data = NULL;
  chan_info *ncp;
  sync_state *sc;
  int i,k;
  Float y0,y1,angle,incr,off,scale;
  snd_info *sp;
  sync_info *si;
  sp = cp->sound;
  sc = get_sync_state_without_snd_fds(cp->state,sp,cp,beg,regexpr);
  if (sc == NULL) return;
  si = sc->si;
  if (regexpr) num = sc->dur;
  for (i=0;i<si->chans;i++)
    {
      ncp = si->cps[i];
      y0 = sample(si->begs[i],ncp);
      y1 = sample(si->begs[i]+num,ncp);
      if (y1 > y0) angle=M_PI; else angle=0.0;
      incr = M_PI/(Float)num;
      off = 0.5*(y1+y0);
      scale = 0.5*fabs(y0-y1);
      data = (MUS_SAMPLE_TYPE *)CALLOC(num,sizeof(MUS_SAMPLE_TYPE));
      for (k=0;k<num;k++,angle+=incr) 
	data[k] = MUS_FLOAT_TO_SAMPLE(off + scale * cos(angle));
      change_samples(si->begs[i],num,data,ncp,LOCK_MIXES,origin);
      update_graph(ncp,NULL);
      FREE(data);
    }
  free_sync_state(sc);
}

/* another possibility is fft-based smoothing (see env.lisp etc) */

static void eval_expression(chan_info *cp, snd_info *sp, int count, int regexpr)
{
#if HAVE_GUILE
  sync_state *sc; 
  sync_info *si;
  snd_fd *sf;
  int dur,chan_dur=0,chan,j,k;
  int beg = 0;
  SCM res;
  snd_state *ss;
  snd_fd **sfs = NULL;
  Float val = 0.0;
  char *s1;
  if (gh_procedure_p(sp->eval_proc))
    {
      ss = cp->state;
      if (!regexpr)
	{
	  beg = cp->cursor;
	  if (count < 0)
	    {
	      count = -count;
	      if ((beg-count) >= 0) 
		beg -= count;
	      else
		{
		  count = beg;
		  beg = 0;
		}
	    }
	}
      sc = get_sync_state(ss,sp,cp,beg,regexpr,READ_FORWARD); /* beg ignored if regexpr (starts at region 0 = si->begs[]) */
      if (sc == NULL) return;
      si = sc->si;
      sfs = sc->sfs;
      if (regexpr)
	dur = sc->dur;
      else dur = count;
      if (dur > 0)
	{
	  for (chan=0;chan<si->chans;chan++)
	    {
	      sf = sfs[chan];	      
	      if (regexpr) 
		chan_dur = dur;
	      else
		{
		  chan_dur = current_ed_samples(si->cps[chan]) - si->begs[chan];
		  if (dur < chan_dur) chan_dur = dur;
		  if (chan_dur == 0) chan_dur = 1;
		  if (dur > MAX_BUFFER_SIZE) start_progress_report(ss,sp,FALSE);
		}
	      j = 0;
	      for (k=0;k<chan_dur;k++)
		{
		  res = g_call1(sp->eval_proc,gh_double2scm((double)(MUS_SAMPLE_TO_FLOAT(sf->current_value))));
		  if (ss->eval_error)
		    {
		      ss->eval_error = 0;
		      for (j=chan;j<si->chans;j++) free_snd_fd(sfs[j]);
		      free_sync_state(sc);
		      return;
		    }
		  next_sample_1(sf);
		  if (gh_number_p(res)) val = gh_scm2double(res);
		  j++;
		  if (j == MAX_BUFFER_SIZE)
		    {
		      progress_report(ss,sp,"C-x C-x",chan,si->chans,(Float)k/((Float)chan_dur),FALSE);
		      check_for_event(ss);
		      if ((ss->stopped_explicitly) || (!(sp->inuse)))
			{
			  ss->stopped_explicitly = 0;
			  report_in_minibuffer(sp,"stopped");
			  break;
			}
		      j = 0;
		    }
		}
	      free_snd_fd(sf);
	      if (dur > MAX_BUFFER_SIZE) finish_progress_report(ss,sp,NOT_FROM_ENVED);
	    }
	  if ((!regexpr) && (chan_dur == 1))
	    {
	      sprintf(expr_str,"%s = %s",sp->eval_expr,s1 = prettyf(val,2));
	      report_in_minibuffer(sp,expr_str);
	      FREE(s1);
	    }
	}
      free_sync_state(sc);
    }
#endif
}

static int prompt_named_mark(chan_info *cp) 
{
  snd_info *sp = cp->sound;
  clear_minibuffer(sp);
  make_minibuffer_label(sp,STR_mark_p);
  sp->minibuffer_on = 1;
  goto_minibuffer(sp);
  sp->marking = cp->cursor+1; /* +1 so it's not confused with 0 (if (sp->marking)...) */
  return(CURSOR_IN_VIEW);
}


static int defining_macro = 0;

#if HAVE_GUILE
/* -------- Keyboard Macros -------- */
/* optimized for the most common case (pure keyboard commands) */

static int macro_cmd_size = 0;
static int macro_size = 0;
typedef struct {int keysym; int state; char *sndop;} macro_cmd;
static macro_cmd **macro_cmds = NULL;
typedef struct {char *name; int macro_size; macro_cmd **cmds;} named_macro;
static named_macro **named_macros = NULL;
static int named_macro_ctr = 0;
static int named_macro_size = 0;

static void allocate_macro_cmds(void)
{
  int i,old_size;
  old_size = macro_cmd_size;
  macro_cmd_size += 16;
  if (!macro_cmds)
    macro_cmds = (macro_cmd **)CALLOC(macro_cmd_size,sizeof(macro_cmd *));
  else 
    {
      macro_cmds = (macro_cmd **)REALLOC(macro_cmds,macro_cmd_size * sizeof(macro_cmd *));
      for (i=old_size;i<macro_cmd_size;i++) macro_cmds[i] = NULL;
    }
}

static void start_defining_macro (void)
{
  macro_size = 0;
  defining_macro = 1;
  if ((!macro_cmds) || (macro_size == macro_cmd_size)) allocate_macro_cmds();
}

static void stop_defining_macro (void)
{
  /* the last C-x ) went into the macro before we noticed it should not have */
  macro_size -= 2;
  defining_macro = 0;
}

static void execute_last_macro (chan_info *cp, int count)
{
  int i,j;
  if (macro_cmds)
    {
      for (j=0;j<count;j++)
	{
	  for (i=0;i<macro_size;i++) 
	    {
	      keyboard_command(cp,macro_cmds[i]->keysym,macro_cmds[i]->state);
	    }
	}
    }
}

static void continue_macro (int keysym, int state)
{
  if (!(macro_cmds[macro_size])) macro_cmds[macro_size] = (macro_cmd *)CALLOC(1,sizeof(macro_cmd));
  macro_cmds[macro_size]->keysym = keysym;
  macro_cmds[macro_size]->state = state;
  macro_size++;
  if (macro_size == macro_cmd_size) allocate_macro_cmds();
}

static named_macro *name_macro(char *name)
{
  named_macro *nm;
  int i,old_size;
  if (named_macro_ctr == named_macro_size)
    {
      old_size = named_macro_size;
      named_macro_size += 16;
      if (!named_macros) named_macros = (named_macro **)CALLOC(named_macro_size,sizeof(named_macro *));
      else 
	{
	  named_macros = (named_macro **)REALLOC(named_macros,named_macro_size * sizeof(named_macro *));
	  for (i=old_size;i<named_macro_size;i++) named_macros[i] = NULL;
	}
    }
  if (!(named_macros[named_macro_ctr])) named_macros[named_macro_ctr] = (named_macro *)CALLOC(1,sizeof(named_macro));
  nm = named_macros[named_macro_ctr];
  nm->name = copy_string(name);
  named_macro_ctr++;
  return(nm);
}

static void name_last_macro (char *name)
{
  named_macro *nm;
  macro_cmd *mc;
  int i;
  nm = name_macro(name);
  nm->macro_size = macro_size;
  nm->cmds = (macro_cmd **)CALLOC(macro_size,sizeof(macro_cmd *));
  for (i=0;i<macro_size;i++)
    {
      nm->cmds[i] = (macro_cmd *)CALLOC(1,sizeof(macro_cmd));
      mc = nm->cmds[i];
      mc->keysym = macro_cmds[i]->keysym;
      mc->state = macro_cmds[i]->state;
    }
}

static void save_macro_1(named_macro *nm, FILE *fd)
{
  int i;
  macro_cmd *mc;
  fprintf(fd,"(defmacro %s ()\n",nm->name);
  for (i=0;i<nm->macro_size;i++)
    {
      mc = nm->cmds[i];
      if (mc->keysym == 0)
	fprintf(fd,"  %s\n",mc->sndop);
      else fprintf(fd,"  (%s %c %d)\n",S_key,(char)(mc->keysym),mc->state);
    }
  fprintf(fd,")\n");
}

static int execute_named_macro_1(chan_info *cp, char *name, int count)
{
  int i,j,k;
  named_macro *nm;
  macro_cmd *mc;
  for (k=0;k<named_macro_ctr;k++)
    {
      if ((named_macros[k]->name) && (strcmp(name,named_macros[k]->name) == 0))
	{
	  nm = named_macros[k];
	  for (j=0;j<count;j++)
	    {
	      for (i=0;i<nm->macro_size;i++) 
		{
		  mc = nm->cmds[i];
		  if (mc->keysym != 0)
		    {
		      keyboard_command(cp,mc->keysym,mc->state);
		    }
		  else
		    {
		      snd_eval_str(cp->state,mc->sndop,1);
		    }
		}
	    }
	  return(1);
	}
    }
  return(0);
}

static void execute_named_macro(chan_info *cp, char *name, int count)
{
  int one_edit;
  if (!(execute_named_macro_1(cp,name,count)))
    /* not a macro...*/
    {
      one_edit = cp->edit_ctr + 1;
      snd_eval_str(cp->state,name,count);
      if (cp->edit_ctr > one_edit)
	{
	  while (cp->edit_ctr > one_edit) backup_edit_list(cp);
	  if (cp->mixes) backup_mix_list(cp,one_edit);
	}
    }
}

typedef struct {int key; int state; int ignore_prefix; SCM func;} key_entry;
static key_entry *user_keymap = NULL;
static int keymap_size = 0;
static int keymap_top = 0;

static int in_user_keymap(int key, int state)
{
  int i;
  if (keymap_top == 0) return(-1);
  for (i=0;i<keymap_top;i++)
    {
      if ((user_keymap[i].key == key) && (user_keymap[i].state == state) && (user_keymap[i].func != SCM_UNDEFINED))
	return(i);
    }
  return(-1);
}

static void set_keymap_entry(int key, int state, int ignore, SCM func)
{
  int i;
  i = in_user_keymap(key,state);
  if (i == -1)
    {
      if (keymap_size == keymap_top)
	{
	  keymap_size += 16;
	  if (keymap_top == 0)
	    {
	      user_keymap = (key_entry *)CALLOC(keymap_size,sizeof(key_entry));
	      for (i=0;i<keymap_size;i++) user_keymap[i].func = SCM_UNDEFINED;
	    }
	  else 
	    {
	      user_keymap = (key_entry *)REALLOC(user_keymap,keymap_size * sizeof(key_entry));
	      for (i=keymap_top;i<keymap_size;i++) 
		{
		  user_keymap[i].key=0; 
		  user_keymap[i].state=0; 
		  user_keymap[i].func = SCM_UNDEFINED;
		}
	    }
	}
      user_keymap[keymap_top].key = key;
      user_keymap[keymap_top].state = state;
      user_keymap[keymap_top].ignore_prefix = ignore;
      if ((func != SCM_UNDEFINED) &&
	  (procedure_ok(func,0,0,S_bind_key,"func",3)))
	{
	  user_keymap[keymap_top].func = func;
	  snd_protect(func);
	}
      else user_keymap[keymap_top].func = SCM_UNDEFINED;
      keymap_top++;
    }
  else
    {
      if ((user_keymap[i].func) && (gh_procedure_p(user_keymap[i].func))) snd_unprotect(user_keymap[i].func);
      if ((func != SCM_UNDEFINED) &&
	  (procedure_ok(func,0,0,S_bind_key,"func",3)))
	{
	  user_keymap[i].func = func;
	  snd_protect(func);
	}
      else user_keymap[i].func = SCM_UNDEFINED;
      user_keymap[i].ignore_prefix = ignore;
    }
}

static int call_user_keymap(int hashedsym, int count)
{
  int i,res = KEYBOARD_NO_ACTION;
  /* if guile call the associated scheme code, else see if basic string parser can handle it */
  if (user_keymap[hashedsym].ignore_prefix) count=1;
  if (user_keymap[hashedsym].func != SCM_UNDEFINED)
    for (i=0;i<count;i++) res = g_scm2intdef(g_call0(user_keymap[hashedsym].func),KEYBOARD_NO_ACTION);
  /* in emacs, apparently, prefix-arg refers to the next command, and current-prefix-arg is this command */
  return(res);
}
#endif

void save_macro_state (FILE *fd)
{
#if HAVE_GUILE
  int i;
  for (i=0;i<named_macro_ctr;i++) save_macro_1(named_macros[i],fd);
#endif
}


static char *dir_from_tempnam(snd_state *ss)
{
  char *name;
  int i;
  name = snd_tempnam(ss);
  i = strlen(name)-1;
  while ((name[i] != '/') && (i>0)) i--;
  if (i == 0) name[0]='.';
  name[i+1]='\0';
  return(name);
}

void snd_minibuffer_activate(snd_info *sp, int keysym)
{
  snd_state *ss;
  snd_info *nsp;
  int s_or_r = 0;
  int nc,i,len;
  chan_info *active_chan;
  char *str = NULL,*mcf = NULL;
  char *tok,*newdir,*str1;
  env *e;
  mark *m;
#if HAVE_OPENDIR
  DIR *dp;
#endif
#if HAVE_GUILE
  SCM proc;
#endif
  if ((keysym == snd_K_s) || (keysym == snd_K_r)) s_or_r = 1;
  ss = sp->state;
  if (sp != selected_sound(ss)) select_channel(sp,0);
  active_chan = current_channel(sp);
  if (active_chan)
    {
      goto_graph(active_chan);
    }
  if ((keysym == snd_K_g) || (keysym == snd_K_G)) /* c-g => abort whatever we're doing and return */
    {
      set_minibuffer_string(sp,NULL);
      clear_minibuffer(sp);
      return;
    }
#if HAVE_GUILE
  if (sp->searching)
    {
      /* it's the search expr request */
      /* if not nil, replace previous */
      if (!s_or_r)
	{
	  str = get_minibuffer_string(sp);
	  if ((str) && (*str))
	    {
	      /* check for procedure as arg, or lambda form:
	       * (lambda (y) (> y .1)) is the same as y>.1
	       * if returns #t, search stops
	       */
	      if (sp->search_expr) free(sp->search_expr);
	      sp->search_expr = str;
	      if ((sp->search_proc) && (gh_procedure_p(sp->search_proc))) snd_unprotect(sp->search_proc);
	      sp->search_proc = SCM_UNDEFINED;
	      proc = parse_proc(str);
	      if (procedure_ok(proc,1,0,"find","find procedure",1))
		{
		  sp->search_proc = proc;
		  snd_protect(proc);
		}
	    }
	}
      if (active_chan)
	handle_cursor_with_sync(active_chan,cursor_search(active_chan,sp->searching));
      return;
    }
#endif
  
  str = get_minibuffer_string(sp);
  if ((sp->marking) || (sp->finding_mark))
    {
      if (sp->marking) 
	{
	  m = add_mark(sp->marking-1,str,active_chan);
	  if (m)
	    {
	      sprintf(expr_str,STR_placed_at_sample,str,sp->marking-1);
	      draw_mark(active_chan,active_chan->axis,m);
	    }
	  else
	    {
	      sprintf(expr_str,STR_There_is_already_a_mark_at_sample,sp->marking-1);
	    }
	  report_in_minibuffer(sp,expr_str);
	  sp->marking = 0;
	}	
      else 
	{
	  handle_cursor_with_sync(active_chan,goto_named_mark(active_chan,str));
	  sp->finding_mark = 0;
	}
      if (str) free(str);
      return;
    }
  if (snd_strlen(str) != 0)
    {
      if (sp->printing)
	{
	  snd_print(ss,str,(sp->printing != 1));
	  sp->printing = 0;
	  clear_minibuffer(sp);
	  free(str);
	  return;
	}
      if (sp->loading)
	{
	  snd_load_file(str);
	  sp->loading = 0;
	  clear_minibuffer(sp);
	  free(str);
	  return;
	}
      if (sp->filing)
	{
	  switch (sp->filing)
	    {
	    case NOT_FILING: snd_error("%s[%d] %s: oh good grief!",__FILE__,__LINE__,__FUNCTION__); break;
	    case INPUT_FILING:
	      nsp = snd_open_file(str,ss); /* will post error if any */
	      if (nsp) 
		{
		  select_channel(nsp,0);
		  clear_minibuffer(sp);
		}
	      break;
	    case REGION_FILING:
	      str1 = mus_file_full_name(str);
	      if (!(snd_overwrite_ok(ss,str1))) {free(str); FREE(str1); return;}
	      save_region(ss,region_count,str1,((file_info *)(sp->hdr))->format);
	      clear_minibuffer(sp);
	      free(str);
	      FREE(str1);
	      break;
	    case CHANNEL_FILING:
	      chan_save_edits(active_chan,mcf = mus_file_full_name(str));
	      if (mcf) FREE(mcf);
	      clear_minibuffer(sp);
	      break;
#if HAVE_OPENDIR
	    case TEMP_FILING:
	      newdir = copy_string(str);
	      clear_minibuffer(sp);
	      dp = opendir(newdir);
	      if (dp) 
		{
		  closedir(dp);
		  set_temp_dir(ss,newdir);
		}
	      else 
		{
		  tok = dir_from_tempnam(ss);
		  sprintf(expr_str,STR_cant_access,newdir,tok);
		  report_in_minibuffer(sp,expr_str);
		  if (newdir) FREE(newdir);
		  if (tok) free(tok);
		}
	      break;
#endif
	    case CHANGE_FILING:
	      mix_complete_file(sp,str,"C-x C-q",with_mix_consoles(ss));
	      break;
	    case INSERT_FILING:
	      str1 = mus_file_full_name(str);
	      nc = mus_sound_chans(str1);
	      if (nc != -1)
		{
		  len = mus_sound_samples(str1)/nc;
		  if (nc > sp->nchans) nc = sp->nchans;
		  if (!active_chan) active_chan = sp->chans[0];
		  for (i=0;i<nc;i++)
		    {
		      file_insert_samples(active_chan->cursor,len,str1,sp->chans[i],i,DONT_DELETE_ME,"C-x C-i");
		      update_graph(sp->chans[i],NULL);
		    }
		  clear_minibuffer(sp);
		}
	      else 
		{
		  sprintf(expr_str,STR_cant_read_his_header,str);
		  report_in_minibuffer(sp,expr_str);
		}
	      FREE(str1);
	      break;
#if HAVE_GUILE
	    case MACRO_FILING: name_last_macro(str); clear_minibuffer(sp); break;
#endif
	    default: snd_error("%s[%d] %s: unknown filing option: %d",__FILE__,__LINE__,__FUNCTION__,sp->filing); break;
	    }
	  sp->filing = NOT_FILING;
	  if (str) free(str);
	  return;
	}
      if (sp->amping)
	{
	  if (!active_chan) active_chan = sp->chans[0];
	  e = string2env(str);
	  if (e)
	    {
	      if (sp->amping != 1)
		apply_env(active_chan,e,active_chan->cursor,sp->amping,1.0,sp->reging,FALSE,(sp->reging) ? "C-x a" : "C-x C-a");
	      else apply_env(active_chan,e,0,current_ed_samples(active_chan),1.0,sp->reging,FALSE,(sp->reging) ? "C-x a" : "C-x C-a");
	    }
	  sp->reging = 0;
	  sp->amping = 0;
	  clear_minibuffer(sp);
	  if (str) free(str);
	  return;
	}
#if HAVE_GUILE
      if (sp->macroing)
	{
	  len = active_chan->cursor;
	  execute_named_macro(active_chan,str,sp->macroing);
	  /* if this is a close command from the current minibuffer, the sound may not exist when we return */
	  ss->mx_sp = NULL;
	  if (sp->state == NULL) return;
	  sp->macroing = 0;
	  if (active_chan->cursor != len)
	    {
	      nc = cursor_decision(active_chan);
	      if (nc == CURSOR_IN_VIEW) nc = CURSOR_UPDATE_DISPLAY; 
	    }
	  else nc = CURSOR_UPDATE_DISPLAY; 
	  handle_cursor_with_sync(active_chan,nc);
	  if (str) free(str);
	  return;
	}
      if (sp->prompting)
	{
	  proc = parse_proc(str);
	  snd_protect(proc);
	  if ((sp->prompt_callback) && (gh_procedure_p(sp->prompt_callback)))
	    g_call2(sp->prompt_callback,proc,gh_int2scm(sp->index));
	  snd_unprotect(proc);
	  if (str) free(str);
	  sp->prompting = 0;
	  return;
	}
      if ((sp->evaling) || (snd_eval_str(sp->state,str,1) == -1))
	{
	  /* check for procedure as arg, or lambda form:
	   * (lambda (y) (+ y .1))
	   * if returns non-number, original value is used
	   * need handle in eval_expression and a flag in sp l4540
	   */
	  if (sp->eval_expr) free(sp->eval_expr);
	  sp->eval_expr = str;
	  if (sp->evaling)
	    {
	      if ((sp->eval_proc) && (gh_procedure_p(sp->eval_proc))) snd_unprotect(sp->eval_proc);
	      sp->eval_proc = SCM_UNDEFINED;
	      proc = parse_proc(str);
	      if (procedure_ok(proc,1,0,"eval","eval procedure",1))
		{
		  sp->eval_proc = proc;
		  snd_protect(proc);
		  eval_expression(active_chan,sp,sp->evaling,sp->reging);
		  clear_minibuffer_prompt(sp);
		  sp->evaling = 0;
		  sp->reging = 0;
		  return;
		}
	    }
	}
#endif
      sp->evaling = 0;
      sp->reging = 0;
    }
  else clear_minibuffer(sp);
}

static int get_count_1(char *number_buffer,int number_ctr, int dot_seen, chan_info *cp)
{
  /* allow floats here = secs */
  float f;
  int i;
  if (number_ctr == 0) return(1); /* c-u followed by nothing = 1 */
  number_buffer[number_ctr] = '\0';
  if (number_ctr == 1)
    { /* handle special cases of just - or + */
      if (number_buffer[0] == '-') return(-1);
      if (number_buffer[0] == '+') return(1);
    }
  if (dot_seen)
    {
      sscanf(number_buffer,"%f",&f);
      return((int)(f*SND_SRATE(cp->sound)));
    }
  else
    {
      sscanf(number_buffer,"%d",&i);
      return(i);
    }
  return(1);
}

static int get_count(char *number_buffer,int number_ctr, int dot_seen, chan_info *cp, int mark_wise)
{
  int val,old_cursor;
  val = get_count_1(number_buffer,number_ctr,dot_seen,cp);
  if (!mark_wise) return(val);
  old_cursor = cp->cursor;
  goto_mark(cp,val);
  val = cp->cursor - old_cursor; /* will be 0 if no relevant marks */
  cp->cursor = old_cursor;
  return(val);
}

#define NUMBER_BUFFER_SIZE 12

static void dks(void) {finish_keyboard_selection();}
static int cks(void) {return(cancel_keyboard_selection());}

static Float state_amount (int state)
{
  Float amount;
  amount = 1.0;
  if (state & snd_ControlMask) amount *= 0.5;
  if (state & snd_MetaMask) amount *= 0.5;
  if (state & snd_ShiftMask) amount *= 0.5;
  return(amount);
}

static void no_selection_error(snd_info *sp)
{
  report_in_minibuffer(sp,"no active selection");
}

#define NO_CX_ARG_SPECIFIED -1

int keyboard_command (chan_info *cp, int keysym, int state)
{
  /* we can't use the meta bit in most cases because this is trapped at a higher level for the Menu mnemonics */
  /* state here is the kbd bucky-bit state */
  /* keysym has Shift taken into account already (see snd-xchn.c XKeycodeToKeysym, and same snd-xsnd.c) */
  static int number_ctr = 0;
  static int dot_seen = 0;
  static int counting = 0;
  static int u_count = 0;
  static char number_buffer[NUMBER_BUFFER_SIZE];
  static int extended_mode = 0;
  static int count = 1, got_count = 0;
  static int m = 0;
  int redisplay,searching,cursor_searching,old_cursor_loc,hashloc,loc,sync_num,i;
  static int ext_count = NO_CX_ARG_SPECIFIED;
  snd_info *sp;
  axis_info *ap;
  snd_state *ss;
  sync_info *si;
  mark *mk = NULL;
  /* fprintf(stderr,"kbd: %d %d %d ",keysym,state,extended_mode); */
  if (!cp) return(KEYBOARD_NO_ACTION);
  redisplay = CURSOR_IN_VIEW;
  searching = 0;
  cursor_searching = 0;
  /* cp->cursor_on = 1; */
  sp = cp->sound;
  ss = cp->state;
  ap = cp->axis;
  if (keysym >= snd_K_Shift_L) return(KEYBOARD_NO_ACTION); 
  /* this happens when the user presses Control or Shift etc prior to hitting the actual (modified) key */
#if HAVE_GUILE
  if (defining_macro) continue_macro(keysym,state);
#endif
  if (!m) count = 1; else m=0;
  if (u_count == 0) set_prefix_arg(ss,0);
  
  /* should we check snd_keypad_Decimal as well as snd_K_period? -- is this assuming USA float syntax? */
  /*   (similarly snd_keypad_0...9) */

  if ((counting) && (((keysym < snd_K_0) || (keysym > snd_K_9)) && (keysym != snd_K_minus) && (keysym != snd_K_period) && (keysym != snd_K_plus)))
    {
      m = ((u_count) && ((keysym == snd_K_M) || (keysym == snd_K_m)));
      count = get_count(number_buffer,number_ctr,dot_seen,cp,m);
      got_count = 1;
      number_ctr = 0;
      counting = 0;
      dot_seen = 0;
      set_prefix_arg(ss,count);
      if (m) return(KEYBOARD_NO_ACTION);
    }
  u_count = 0;
  if ((keysym != snd_K_X) && (keysym != snd_K_x))
    {
      got_count = 0;
      if (count == 0) return(KEYBOARD_NO_ACTION);
    }
  if ((state & snd_MetaMask) && ((keysym == snd_K_X) || (keysym == snd_K_x)))
    {
      /* named macros invoked and saved here */
      ss->mx_sp = sp;
      prompt(sp,"M-x:",NULL);
      sp->macroing = count;
      return(KEYBOARD_NO_ACTION);
    }
#if HAVE_GUILE
  hashloc = in_user_keymap(keysym,state);
  if (hashloc != -1) {return(call_user_keymap(hashloc,count));}
#endif
  if (sp->minibuffer_temp) clear_minibuffer(sp);
  if (state & snd_ControlMask)
    {
      if (!extended_mode)
	{
	  switch (keysym)
	    {
	    case snd_K_A: case snd_K_a: 
	      cp->cursor_on = 1; 
	      loc = (int)(ap->x0*SND_SRATE(sp)); 
	      if ((loc+1) == ap->losamp) loc=ap->losamp; /* handle dumb rounding problem */
	      cursor_moveto(cp,loc); 
	      break;
	    case snd_K_B: case snd_K_b: cp->cursor_on = 1; redisplay = cursor_move(cp,-count); break;
	    case snd_K_D: case snd_K_d: cp->cursor_on = 1; cks(); redisplay = cursor_delete(cp,count,"C-d"); break;
	    case snd_K_E: case snd_K_e:
	      cp->cursor_on = 1; 
	      loc = (int)(ap->x1*(double)SND_SRATE(sp));
	      if ((loc+1) == ap->hisamp) loc=ap->hisamp;
	      cursor_moveto(cp,loc); 
	      break;
	    case snd_K_F: case snd_K_f: cp->cursor_on = 1; redisplay = cursor_move(cp,count); break;
	    case snd_K_G: case snd_K_g: 
	      number_ctr = 0; 
	      counting = 0; 
	      dot_seen = 0; 
	      if ((cks() == -1) && (selection_is_current()))
		deactivate_selection();
	      defining_macro = 0;
	      if (ss->checking_explicitly) ss->stopped_explicitly = 1; 
	      /* this tries to break out of long filter/src computations (and perhaps others) */
	      if (sp->playing) stop_playing_all_sounds();
	      if (sp->applying) stop_applying(sp);
	      map_over_sound_chans(sp,stop_fft_in_progress,NULL);
	      clear_minibuffer(sp);
	      break;
	    case snd_K_H: case snd_K_h: cp->cursor_on = 1; cks(); redisplay = cursor_delete_previous(cp,count,"C-h"); break; 
	    case snd_K_I: case snd_K_i: show_cursor_info(cp); searching = 1; break;
	    case snd_K_J: case snd_K_j: cp->cursor_on = 1; redisplay = goto_mark(cp,count); break;
	    case snd_K_K: case snd_K_k: cp->cursor_on = 1; cks(); redisplay = cursor_delete(cp,count*line_size(ss),"C-k"); break;
	    case snd_K_L: case snd_K_l: cp->cursor_on = 1; redisplay = CURSOR_IN_MIDDLE; break;
	    case snd_K_M: case snd_K_m:
	      if (count > 0) 
		{
		  cp->cursor_on = 1;
		  set_show_marks(ss,1);
		  mk = add_mark(cp->cursor,NULL,cp);
		  if (mk) draw_mark(cp,cp->axis,mk);
		}
	      else delete_mark_samp(cp->cursor,cp);
	      if ((keysym == snd_K_M) && ((cp->sound)->syncing != 0))
		{
		  sync_num = mark_sync_max() + 1; 
		  if (mk) set_mark_sync(mk,sync_num);
		  si = snd_sync(cp->state,(cp->sound)->syncing);
		  for (i=0;i<si->chans;i++) 
		    {
		      if (cp != si->cps[i])
			{
			  if (count > 0)
			    {
			      mk = add_mark(cp->cursor,NULL,si->cps[i]);
			      if (mk)
				{
				  set_mark_sync(mk,sync_num);
				  draw_mark(si->cps[i],(si->cps[i])->axis,mk);
				}
			    }
			  else delete_mark_samp(cp->cursor,si->cps[i]);
			}
		    }
		  free_sync_info(si);
		}
	      break;
	    case snd_K_N: case snd_K_n: cp->cursor_on = 1; redisplay = cursor_move(cp,count*line_size(ss)); break;
	    case snd_K_O: case snd_K_o: cp->cursor_on = 1; cks(); redisplay = cursor_insert(cp,count); break;
	    case snd_K_P: case snd_K_p: cp->cursor_on = 1; redisplay = cursor_move(cp,-count*line_size(ss)); break;
	    case snd_K_Q: case snd_K_q: 
	      start_playing(cp,cp->cursor,NO_END_SPECIFIED); 
	      set_play_button(sp,1); 
	      redisplay = NO_ACTION; 
	      break;
	    case snd_K_R: case snd_K_r: cp->cursor_on = 1; redisplay = cursor_search(cp,-count); searching = 1; cursor_searching = 1; break;
	    case snd_K_S: case snd_K_s: cp->cursor_on = 1; redisplay = cursor_search(cp,count); searching = 1; cursor_searching = 1; break;
	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp); 
	      set_play_button(sp,0);
	      redisplay = NO_ACTION; 
	      break;
	    case snd_K_U: case snd_K_u: counting = 1; u_count = 1; number_ctr = 0; dot_seen = 0; break;
	    case snd_K_V: case snd_K_v:
	      cp->cursor_on = 1;
	      if (count > 0)
		redisplay = cursor_moveto(cp,(int)(ap->x1*SND_SRATE(sp)+1+(count-1)*SND_SRATE(sp)*(ap->x1 - ap->x0)));
	      else redisplay = cursor_moveto(cp,(int)(ap->x0*SND_SRATE(sp)-1+(count+1)*SND_SRATE(sp)*(ap->x1 - ap->x0)));
	      break;
	    case snd_K_W: case snd_K_w: dks(); delete_selection("C-x C-w",UPDATE_DISPLAY); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_K_X: case snd_K_x: dks(); extended_mode = 1; if (got_count) {ext_count = count; got_count = 0;} break;
	    case snd_K_Y: case snd_K_y: dks(); paste_region(count,cp,"C-y"); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_K_Z: case snd_K_z: cks(); cp->cursor_on = 1; redisplay = cursor_zeros(cp,count,0); break;
	    case snd_K_Right: sx_incremented(cp,state_amount(state)); break;
	    case snd_K_Left: sx_incremented(cp,-state_amount(state)); break;
	    case snd_K_Up: zx_incremented(cp,1.0+state_amount(state)); break;
	    case snd_K_Down: zx_incremented(cp,1.0/(1.0+state_amount(state))); break;
	    case snd_K_0: case snd_K_1: case snd_K_2: case snd_K_3: case snd_K_4:
	    case snd_K_5: case snd_K_6: case snd_K_7: case snd_K_8: case snd_K_9: 
	      counting = 1;
	      number_buffer[number_ctr]=(char)('0'+keysym-snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE-2)) number_ctr++; 
	      /* there is also the bare-number case below */
	      break;
	    case snd_K_space: 
	      if (selection_is_current())
		{
		  if (cks() == -1)
		    deactivate_selection();
		}
	      else
		{
		  if (count > 0)
		    {
		      start_keyboard_selection(cp,cp->cx); 
		      redisplay = NO_ACTION;
		    }
		}
	      break;
	    case snd_K_period: counting = 1; number_buffer[number_ctr]='.'; number_ctr++; dot_seen = 1; break;
	    case snd_K_greater: cp->cursor_on = 1; redisplay = cursor_moveto_end(cp); break;
	    case snd_K_less: cp->cursor_on = 1; redisplay = cursor_moveto_beginning(cp); break;
	    case snd_K_minus: counting = 1; number_ctr = 1; number_buffer[0]='-'; break;
	    case snd_K_underscore: undo_EDIT(cp,count); redisplay = CURSOR_UPDATE_DISPLAY; break;
#if !defined(NEXT) && !defined(UW2)
	    case snd_keypad_Left: case snd_keypad_4: 
	      set_spectro_y_angle(ss,spectro_y_angle(ss)-1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Right: case snd_keypad_6: 
	      set_spectro_y_angle(ss,spectro_y_angle(ss)+1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Down: case snd_keypad_2: 
	      set_spectro_x_angle(ss,spectro_x_angle(ss)-1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Up: case snd_keypad_8: 
	      set_spectro_x_angle(ss,spectro_x_angle(ss)+1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
#else
	    case snd_keypad_4: 
	      set_spectro_y_angle(ss,spectro_y_angle(ss)-1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_6: 
	      set_spectro_y_angle(ss,spectro_y_angle(ss)+1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_2:
	      set_spectro_x_angle(ss,spectro_x_angle(ss)-1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_8: 
	      set_spectro_x_angle(ss,spectro_x_angle(ss)+1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
#endif
	    default:
	      sprintf(expr_str,"C-%s undefined",key_to_name(keysym));
	      report_in_minibuffer(sp,expr_str);
	      break;
	    }
	}
      else /* extended mode with ctrl down */
	{
	  if (ext_count == NO_CX_ARG_SPECIFIED) ext_count = 1;
	  extended_mode = 0;
	  switch (keysym)
	    {
	    case snd_K_A: case snd_K_a: get_amp_expression(sp,ext_count,0); searching = 1; redisplay = CURSOR_IN_VIEW; break;
	    case snd_K_B: case snd_K_b: redisplay = set_window_bounds(cp,ext_count); break;
	    case snd_K_C: case snd_K_c: sound_hide_ctrls(sp); break;
	    case snd_K_D: case snd_K_d: redisplay = prompt(sp,STR_eps_file_p,NULL); sp->printing = ext_count; searching = 1; break;
	    case snd_K_E: case snd_K_e: redisplay = prompt(sp,STR_macro_name_p,NULL); sp->filing = MACRO_FILING; searching = 1; break;
	    case snd_K_F: case snd_K_f: redisplay = prompt(sp,STR_file_p,NULL); sp->filing = INPUT_FILING; searching = 1; break;
	    case snd_K_G: case snd_K_g: 
	      number_ctr = 0;
	      counting = 0; 
	      dot_seen = 0; 
	      defining_macro = 0;
	      if (ss->checking_explicitly) ss->stopped_explicitly = 1; 
	      break;
	    case snd_K_H: case snd_K_h: break;
	    case snd_K_I: case snd_K_i: redisplay = prompt(sp,STR_insert_file_p,NULL); sp->filing = INSERT_FILING; searching = 1; break;
	    case snd_K_J: case snd_K_j: cp->cursor_on = 1; redisplay = goto_mix(cp,ext_count); break;
	    case snd_K_L: case snd_K_l: redisplay = prompt(sp,STR_load_p,NULL); sp->loading = 1; searching = 1; break;
	    case snd_K_M: case snd_K_m:
	      cp->cursor_on = 1; 
	      redisplay = prompt_named_mark(cp);
	      set_show_marks(ss,1); 
	      searching = 1; 
	      break;
	    case snd_K_N: case snd_K_n: eval_expression(cp,sp,ext_count,0); searching = 1; redisplay = CURSOR_IN_VIEW; break;
	    case snd_K_O: case snd_K_o: sound_show_ctrls(sp); break;
	    case snd_K_P: case snd_K_p: redisplay = set_window_size(cp,ext_count); break;
	    case snd_K_Q: case snd_K_q: redisplay = prompt(sp,STR_mix_file_p,NULL); sp->filing = CHANGE_FILING; searching = 1; break;
	    case snd_K_R: case snd_K_r: redo_EDIT(cp,ext_count); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_K_S: case snd_K_s: save_edits(sp,NULL); redisplay = CURSOR_IN_VIEW; break;
	    case snd_K_T: case snd_K_t: stop_playing_sound(sp); redisplay = NO_ACTION; break;
	    case snd_K_U: case snd_K_u: undo_EDIT(cp,ext_count); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_K_V: case snd_K_v: redisplay = set_window_percentage(cp,ext_count); break;
	    case snd_K_W: case snd_K_w: redisplay = prompt(sp,STR_file_p,NULL); sp->filing = CHANNEL_FILING; searching = 1; break;
	    case snd_K_X: case snd_K_x: get_eval_expression(sp,ext_count,0); searching = 1; redisplay = CURSOR_IN_VIEW; break;
	    case snd_K_Y: case snd_K_y: break;
	    case snd_K_Z: case snd_K_z: cp->cursor_on = 1; cos_smooth(cp,cp->cursor,ext_count,0,"C-x C-z"); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_K_Right: sx_incremented(cp,state_amount(state)); break;
	    case snd_K_Left:  sx_incremented(cp,-state_amount(state)); break;
	    case snd_K_Up: zx_incremented(cp,1.0+state_amount(state)); break;
	    case snd_K_Down: zx_incremented(cp,1.0/(1.0+state_amount(state))); break;
	    default:
	      sprintf(expr_str,"C-x C-%s undefined",key_to_name(keysym));
	      report_in_minibuffer(sp,expr_str);
	      break;
	    }
	}
    }
  else
    {
      if (!extended_mode)
	{ /* no control/meta, not extended mode -- bare alpha chars sent to listener if possible */
	  switch (keysym)
	    {
	    case snd_K_0: case snd_K_1: case snd_K_2: case snd_K_3: case snd_K_4:
	    case snd_K_5: case snd_K_6: case snd_K_7: case snd_K_8: case snd_K_9: 
	      counting = 1;
	      number_buffer[number_ctr]=(char)('0'+keysym-snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE-2)) number_ctr++; 
	      break;
	    case snd_K_period: counting=1; number_buffer[number_ctr]='.'; number_ctr++; dot_seen = 1; break;
	    case snd_K_greater: cp->cursor_on = 1; redisplay = cursor_moveto_end(cp); break;
	    case snd_K_less: cp->cursor_on = 1; redisplay = cursor_moveto_beginning(cp); break;
	    case snd_K_minus: counting=1; number_buffer[0]='-'; number_ctr=1; break;
	    case snd_K_Right: sx_incremented(cp,state_amount(state)); break;
	    case snd_K_Left:  sx_incremented(cp,-state_amount(state)); break;
	    case snd_K_Up: zx_incremented(cp,1.0+state_amount(state)); break;
	    case snd_K_Down: zx_incremented(cp,1.0/(1.0+state_amount(state))); break;
	    case snd_K_Home: snd_update(ss,sp); break;
	    case snd_K_space: 
	      old_cursor_loc = cks(); /* can be -1 => not actively selecting via kbd */
	      if (old_cursor_loc == -1) 
		{
		  deactivate_selection();
		  if (play_in_progress()) toggle_dac_pausing(ss);
		}
	      else /* all syncd chans need to reset cursor */
		cursor_moveto(cp,old_cursor_loc);
	      break;

	      /* fUn WiTh KeYpAd! */
#if !defined(NEXT) && !defined(UW2)
	    case snd_keypad_Up: case snd_keypad_8: 
	      set_spectro_z_scale(ss,spectro_z_scale(ss)+.01);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Down: case snd_keypad_2: 
	      set_spectro_z_scale(ss,spectro_z_scale(ss)-.01);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Left: case snd_keypad_4: 
	      set_spectro_z_angle(ss,spectro_z_angle(ss)-1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Right: case snd_keypad_6: 
	      set_spectro_z_angle(ss,spectro_z_angle(ss)+1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
#else
	    case snd_keypad_8: 
	      set_spectro_z_scale(ss,spectro_z_scale(ss)+.01);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_2: 
	      set_spectro_z_scale(ss,spectro_z_scale(ss)-.01);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_4: 
	      set_spectro_z_angle(ss,spectro_z_angle(ss)-1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_6: 
	      set_spectro_z_angle(ss,spectro_z_angle(ss)+1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss); 
	      break;
#endif
	    case snd_keypad_Add:
	      if (wavo(ss)) 
		set_wavo_trace(ss,wavo_trace(ss)+1); 
	      else set_spectro_hop(ss,spectro_hop(ss)+1);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Subtract: 
	      if (wavo(ss)) 
		{
		  if (wavo_trace(ss)>1) 
		    set_wavo_trace(ss,wavo_trace(ss)-1);
		} 
	      else 
		{
		  if (spectro_hop(ss)>1) 
		    set_spectro_hop(ss,spectro_hop(ss)-1);
		}
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Multiply: set_fft_size(ss,fft_size(ss) * 2); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_keypad_Divide: 
	      if (fft_size(ss) > 4) set_fft_size(ss,fft_size(ss) / 2); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
#if !defined(NEXT) && !defined(UW2)
	    case snd_keypad_Delete: case snd_keypad_Decimal: set_dot_size(ss,dot_size(ss)+1); redisplay = KEYBOARD_NO_ACTION; break;
	    case snd_keypad_Insert: case snd_keypad_0: if (dot_size(ss) > 1) set_dot_size(ss,dot_size(ss)-1); redisplay = KEYBOARD_NO_ACTION; break;
	    case snd_keypad_PageDown: case snd_keypad_3: 
	      set_spectro_cutoff(ss,spectro_cutoff(ss)*.95); redisplay = CURSOR_UPDATE_DISPLAY; reflect_spectro(ss); break;
	    case snd_keypad_PageUp: case snd_keypad_9: 
	      if (spectro_cutoff(ss) < 1.0) set_spectro_cutoff(ss,spectro_cutoff(ss)/.95); 
	      redisplay = CURSOR_UPDATE_DISPLAY; reflect_spectro(ss); 
	      break;
#else
	    case snd_keypad_Decimal: set_dot_size(ss,dot_size(ss)+1); redisplay = KEYBOARD_NO_ACTION; break;
	    case_snd_keypad_0: if (dot_size(ss) > 1) set_dot_size(ss,dot_size(ss)-1); redisplay = KEYBOARD_NO_ACTION; break;
	    case snd_keypad_3: 
	      set_spectro_cutoff(ss,spectro_cutoff(ss)*.95); redisplay = CURSOR_UPDATE_DISPLAY; reflect_spectro(ss); break;
	    case snd_keypad_9: 
	      if (spectro_cutoff(ss) < 1.0) set_spectro_cutoff(ss,spectro_cutoff(ss)/.95); 
	      redisplay = CURSOR_UPDATE_DISPLAY; reflect_spectro(ss); 
	      break;
#endif
	    case snd_keypad_Enter: reset_spectro(ss); redisplay = CURSOR_UPDATE_DISPLAY; reflect_spectro(ss); break;
	    default:
	      /* try to send unbuckified random keyboard input to the lisp listener */
	      ss = cp->state;
	      if (ss->listening != LISTENER_CLOSED)
		{
		  if (listener_height() > 5)
		    {
		      char buf[2];
		      goto_listener();
		      buf[0]=keysym; buf[1]=0;
		      snd_append_char(ss,buf);
		    }
		  /* else activate?? */
		}
	      else 
		{
		  if (keysym == snd_K_openparen)
		    {
		      ss->mx_sp = sp;
		      prompt(sp,"M-x:","(");
		      sp->macroing = count;
		      redisplay = KEYBOARD_NO_ACTION;
		    }
		  sprintf(expr_str,"%s undefined",key_to_name(keysym));
		  report_in_minibuffer(sp,expr_str);
		}
	      /* should we open the minibuffer in all cases? */
	      break;
	    }
	}
      else /* extended mode with ctrl up -- where not an emacs analogy, related to the current selection */
	{
	  extended_mode = 0;
	  switch (keysym)
	    {
	    case snd_K_A: case snd_K_a: 
	      if (selection_active(cp)) 
		{
		  get_amp_expression(sp,(ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count,1); 
		  searching = 1; 
		  redisplay = CURSOR_IN_VIEW;
		} 
	      else no_selection_error(sp); 
	      break;
	    case snd_K_B: case snd_K_b: cp->cursor_on = 1; redisplay = CURSOR_ON_LEFT; break;
	    case snd_K_C: case snd_K_c: mark_define_region(cp,(ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count); redisplay = CURSOR_CLAIM_SELECTION; break;
	    case snd_K_D: case snd_K_d: redisplay = prompt(sp,STR_temp_dir_p,NULL); sp->filing = TEMP_FILING; searching = 1; break;
#if HAVE_GUILE
	    case snd_K_E: case snd_K_e: 
	      execute_last_macro(cp,(ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count);
	      redisplay = cursor_decision(cp);
	      if (redisplay == CURSOR_IN_VIEW) redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
#endif
	    case snd_K_F: case snd_K_f: cp->cursor_on = 1; redisplay = CURSOR_ON_RIGHT; break;
	    case snd_K_H: case snd_K_h: 
	      break;
	    case snd_K_I: case snd_K_i: paste_region((ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count,cp,"C-x i"); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_K_J: case snd_K_j: redisplay = prompt(sp,STR_mark_p,NULL); sp->finding_mark = 1; searching = 1; break;
	    case snd_K_K: case snd_K_k: snd_close_file(sp,ss); redisplay = CURSOR_NO_ACTION; break;
	    case snd_K_L: case snd_K_l: 
	      cp->cursor_on = 1;
	      if (selection_active(cp))
		cursor_moveto(cp,(int)(selection_beg(cp)+0.5*region_len(0)));
	      else no_selection_error(sp); 
	      redisplay = CURSOR_IN_MIDDLE;
	      break;
	    case snd_K_N: case snd_K_n: 
	      if (selection_active(cp))
		eval_expression(cp,sp,(ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count,TRUE); 
	      else no_selection_error(sp); 
	      break;
	    case snd_K_O: case snd_K_o: 
	      if (ext_count > 0) goto_next_graph(cp,ext_count); else goto_previous_graph(cp,ext_count); redisplay = CURSOR_NO_ACTION; break;
	    case snd_K_P: case snd_K_p: 
	      if (ext_count == NO_CX_ARG_SPECIFIED)
		play_selection(ss);
	      else play_region(ss,ext_count,NULL,FALSE); 
	      redisplay = NO_ACTION;
	      break;
	    case snd_K_Q: case snd_K_q: add_region((ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count,cp,"C-x q"); redisplay = CURSOR_UPDATE_DISPLAY; break;
	      /* if count is Float, it becomes the scaler on the added data */
	    case snd_K_R: case snd_K_r: redo_EDIT(cp,(ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_K_U: case snd_K_u: undo_EDIT(cp,(ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count); redisplay = CURSOR_UPDATE_DISPLAY; break;
	    case snd_K_V: case snd_K_v: 
	      if (selection_active(cp))
		{
		  window_frames_selection(cp); 
		  redisplay = CURSOR_UPDATE_DISPLAY; 
		}
	      else no_selection_error(sp); 
	      break;
	    case snd_K_W: case snd_K_w: 
	      region_count = (ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count;
	      redisplay = prompt(sp,STR_file_p,NULL); 
	      sp->filing = REGION_FILING; 
	      searching = 1;
	      break;
	    case snd_K_X: case snd_K_x: 
	      if (selection_active(cp))
		{
		  get_eval_expression(sp,(ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count,TRUE); 
		  searching = 1; 
		  redisplay = CURSOR_IN_VIEW;
		}
	      else no_selection_error(sp); 
	      break;
	    case snd_K_Z: case snd_K_z: 
	      if (selection_active(cp))
		{
		  cos_smooth(cp,cp->cursor,(ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count,1,"C-x z"); 
		  redisplay = CURSOR_UPDATE_DISPLAY; 
		}
	      else no_selection_error(sp); 
	      break;
	    case snd_K_Right: sx_incremented(cp,state_amount(state)); break;
	    case snd_K_Left:  sx_incremented(cp,-state_amount(state)); break;
	    case snd_K_Up: zx_incremented(cp,1.0+state_amount(state)); break;
	    case snd_K_Down: zx_incremented(cp,1.0/(1.0+state_amount(state))); break;
	    case snd_K_less: cp->cursor_on = 1; redisplay = cursor_moveto_beginning(cp); break;
	    case snd_K_greater: cp->cursor_on = 1; redisplay = cursor_moveto_end(cp); break;
	    case snd_K_openparen:
#if HAVE_GUILE
	      if (defining_macro) 
		report_in_minibuffer(sp,STR_macro_definition_already_in_progress);
	      else
		{
		  start_defining_macro(); 
		  report_in_minibuffer(sp,STR_defining_kbd_macro); 
		}
#endif
	      redisplay = NO_ACTION; 
	      break;
	    case snd_K_closeparen: 
#if HAVE_GUILE
	      if (defining_macro)
		{
		  stop_defining_macro(); 
		  clear_minibuffer(sp); 
		}
#endif
	      redisplay = NO_ACTION;
	      break;
	    case snd_K_slash: 
	      cp->cursor_on = 1;
	      redisplay = prompt_named_mark(cp); 
	      set_show_marks(ss,1); 
	      searching = 1; 
	      break;
	    default:
	      sprintf(expr_str,"C-x %s undefined",key_to_name(keysym));
	      report_in_minibuffer(sp,expr_str);
	      break;
	    }
	}
    }
  if (!extended_mode) ext_count = NO_CX_ARG_SPECIFIED;
  if (redisplay != NO_ACTION)
    {
      if ((sp->minibuffer_on) && (!searching)) 
	clear_minibuffer(sp);
      else if (!cursor_searching) 
	sp->searching = 0;
      return(redisplay);
    }
  else return(KEYBOARD_NO_ACTION);
}


/* ---------------- graphics callbacks ---------------- */

#define SLOPPY_MOUSE 10

static int within_graph(chan_info *cp, int x, int y)
{
  axis_info *ap;
  fft_info *fp;
  snd_state *ss;
  int x0,x1;
  x0 = x-SLOPPY_MOUSE;
  x1 = x+SLOPPY_MOUSE;
  if (cp->waving)
    {
      ap = cp->axis;
      /* does (x,y) fall within the current axis bounds x_axis_x0|x1, y_axis_y0|y1 */
      if (((x0<=ap->x_axis_x1) && (x1>=ap->x_axis_x0)) && ((y<=ap->y_axis_y0) && (y>=ap->y_axis_y1)))
	return(WAVE);
    }
  if (cp->ffting)
    {
      fp = cp->fft;
      ap = fp->axis;
      ss = cp->state;
      if (fft_style(ss) != SONOGRAM)
	{
	  if (((x>=ap->x_axis_x0) && (x<=ap->x_axis_x1)) && (((y-SLOPPY_MOUSE)<=ap->y_axis_y0) && ((y+SLOPPY_MOUSE)>=ap->y_axis_y0)))
	    return(FFT);
	}
      else
	{
	  if ((((x-SLOPPY_MOUSE)<=ap->x_axis_x0) && ((x+SLOPPY_MOUSE)>=ap->x_axis_x0)) && ((y<=ap->y_axis_y0) && (y>=ap->y_axis_y1)))
	    return(FFT);
	}
      if (((x0<=ap->x_axis_x1) && (x1>=ap->x_axis_x0)) && ((y<=ap->y_axis_y0) && (y>=ap->y_axis_y1)))
	return(FFT_MAIN);
    }
  if ((cp->lisp_graphing) && (cp->lisp_info))
    {
      ap = (cp->lisp_info)->axis;
      if (((x0<=ap->x_axis_x1) && (x1>=ap->x_axis_x0)) && ((y<=ap->y_axis_y0) && (y>=ap->y_axis_y1)))
	return(LISP);
    }
  return(NOGRAPH);
}

static char *describe_fft_point(chan_info *cp, int x, int y)
{
  char *fftdes;
  Float xf,yf;
  axis_info *ap;
  fft_info *fp;
  snd_state *ss;
  sono_info *si;
  int ind,time;
  fftdes = (char *)CALLOC(128,sizeof(char));
  fp = cp->fft;
  ap = fp->axis;
  ss = cp->state;
  if (x < ap->x_axis_x0) x = ap->x_axis_x0; else if (x > ap->x_axis_x1) x = ap->x_axis_x1;
  xf = ap->x0 + (ap->x1 - ap->x0) * (Float)(x - ap->x_axis_x0)/(Float)(ap->x_axis_x1 - ap->x_axis_x0);
  if (fft_style(ss) == NORMAL_FFT)        /* fp->data[bins] */
    {
      if (transform_type(ss) == FOURIER)
	ind = (int)((fp->current_size * xf) / (Float)SND_SRATE(cp->sound));
      else ind = (int)xf;
      sprintf(fftdes,"(%.1f%s, transform val: %.3f%s (raw: %.3f)",
	      xf,
	      ((transform_type(ss) == AUTOCORRELATION) ? " samps" : " Hz"),
	      fp->data[ind]*fp->scale,(fft_log_magnitude(ss)) ? "dB" : "",fp->data[ind]);
    }
  else 
    {
      if (fft_style(ss) == SONOGRAM) 	  /* si->data[slices][bins] */
	{
	  yf = ap->y0 + (ap->y1 - ap->y0) * (Float)(y - ap->y_axis_y0)/(Float)(ap->y_axis_y1 - ap->y_axis_y0);
	  si = (sono_info *)(cp->sonogram_data);
	  if (transform_type(ss) == FOURIER)
	    ind = (int)((fp->current_size * yf) / (Float)SND_SRATE(cp->sound));
	  else ind = (int)yf;
	  time = (int)(si->target_slices * (Float)(x - ap->x_axis_x0)/(Float)(ap->x_axis_x1 - ap->x_axis_x0));
	  sprintf(fftdes,"(time: %.2f, freq: %.1f, val: %.3f (raw: %.3f))",xf,yf,si->data[time][ind]/si->scale,si->data[time][ind]);
	}
      else sprintf(fftdes,"?");
    }
  return(fftdes);
}

void fftb(chan_info *cp, int on)
{
  cp->ffting = on;
  set_toggle_button(channel_f(cp),on,FALSE,(void *)cp);
  calculate_fft(cp,NULL);
}

void waveb(chan_info *cp, int on)
{
  cp->waving = on;
  set_toggle_button(channel_w(cp),on,FALSE,(void *)cp);
  update_graph(cp,NULL);
}

static void propogate_wf_state(snd_info *sp)
{
  int i,w,f;
  chan_info *cp;
  if (sp->combining != CHANNELS_SEPARATE)
    {
      cp = sp->chans[0];
      if (sp->combining == CHANNELS_SUPERIMPOSED) clear_window(((axis_info *)(cp->axis))->ax);
      w = cp->waving;
      f = cp->ffting;
      for (i=1;i<sp->nchans;i++) 
	{
	  cp = sp->chans[i];
	  cp->waving = w;
	  cp->ffting = f;
	  set_toggle_button(channel_f(cp),(f) ? TRUE : FALSE,FALSE,(void *)cp);
	  set_toggle_button(channel_w(cp),(w) ? TRUE : FALSE,FALSE,(void *)cp);
	}
      if (f) 
	map_over_sound_chans(sp,calculate_fft,NULL);
      else map_over_sound_chans(sp,update_graph,NULL);
    }
}

void f_button_callback(chan_info *cp, int on, int with_control)
{
  snd_info *sp;
  int i;
  chan_info *ncp;
  cp->ffting = on;
  sp = cp->sound;
  if (sp->combining != CHANNELS_SEPARATE)
    propogate_wf_state(sp);
  else
    {
      if (cp->ffting) calculate_fft(cp,NULL); else update_graph(cp,NULL);
      if (with_control)
	{
	  for (i=0;i<sp->nchans;i++) 
	    {
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->ffting = on;
		  set_toggle_button(channel_f(ncp),(on) ? TRUE : FALSE,FALSE,(void *)cp);
		  if (ncp->ffting) calculate_fft(ncp,NULL); else update_graph(ncp,NULL);
		}
	    }
	}
      goto_graph(cp);
    }
}

void w_button_callback(chan_info *cp, int on, int with_control)
{
  snd_info *sp;
  int i;
  chan_info *ncp;
  cp->waving = on;
  sp = cp->sound;
  if (sp->combining != CHANNELS_SEPARATE)
    propogate_wf_state(sp);
  else
    {
      update_graph(cp,NULL);
      if (with_control)
	{
	  for (i=0;i<sp->nchans;i++) 
	    {
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->waving = on;
		  set_toggle_button(channel_w(ncp),(on) ? TRUE : FALSE,FALSE,(void *)cp);
		  update_graph(ncp,NULL);
		}
	    }
	}
      goto_graph(cp);
    }
}

void edit_select_callback(chan_info *cp, int ed, int with_control)
{
  if (ed != cp->edit_ctr)
    {
      if (cp->edit_ctr > ed)
	{
	  if (with_control)
	    undo_EDIT(cp,cp->edit_ctr - ed);
	  else undo_edit(cp,cp->edit_ctr - ed);
	}
      else 
	{
	  if (with_control)
	    redo_EDIT(cp,ed - cp->edit_ctr);	      
	  else redo_edit(cp,ed - cp->edit_ctr);
	}
    }
  /* don't let this list trap all subsequent clicks and keypresses! */
  goto_graph(cp);
}

void draw_graph_border(chan_info *cp)
{
  axis_info *ap;
  axis_context *ax;
  int h,w,displays,y0,y1;
  if (cp->waving) ap = cp->axis;
  else if (cp->ffting) ap = (cp->fft)->axis;
  else if (cp->lisp_graphing) ap = ((lisp_grf *)(cp->lisp_info))->axis;
  else ap = cp->axis;
  h = ap->height-2;
  if (cp->waving) displays = 1; else displays = 0;
  if (cp->ffting) displays++;
  if (cp->lisp_graphing) displays++;
  w = ap->width*displays - 1;
  y0 = 1+ap->y_offset;
  y1 = h+ap->y_offset;
  if (w > 0)
    {
      ax = copy_context(cp);
      draw_line(ax,1,y0,1,y1);
      draw_line(ax,1,y0,w,y0);
      draw_line(ax,w,y0,w,y1);
      draw_line(ax,1,y1,w,y1);
    }
}

int key_press_callback(snd_state *ss, snd_info *ur_sp, chan_info *ur_cp, int x, int y, int key_state, int keysym, char *keyname)
{
  /* called by every key-intercepting widget in the entire sound pane */
  chan_info *cp,*ncp;
  snd_info *sp;
  int redisplay;
  sp = ur_sp;
  if (ss)
    {
      sp = any_selected_sound(ss);
      if (sp)
	ncp = current_channel(sp);
      else
	{
	  snd_append_command(ss,keyname);
	  return(TRUE);
	}
    }
  else
    {
      if (sp)
	ncp = any_selected_channel(sp);
      else ncp = ur_cp;
    }
  cp = virtual_selected_channel(ncp);
  sp = cp->sound;
  select_channel(sp,cp->chan);
  if ((cp->lisp_graphing) && 
      (within_graph(cp,x,y) == LISP) &&
      (handle_key_press(cp,keysym,key_state) == TRUE))
    return(FALSE);
  redisplay = keyboard_command(cp,keysym,key_state);
  if (redisplay == CURSOR_CLAIM_SELECTION)
    {
      reflect_edit_with_selection_in_menu();
      redisplay = CURSOR_UPDATE_DISPLAY;
    }
  /* if lisp graph has cursor? */
  handle_cursor_with_sync(cp,redisplay);
  return(FALSE);
}

static int dragged = 0;
static TIME_TYPE mouse_down_time;
static mark *mouse_mark = NULL;
static mark *play_mark = NULL;
static int click_within_graph = NOGRAPH;

static chan_info *which_channel(snd_info *sp, int y)
{
  int i;
  chan_info *cp,*ncp;
  axis_info *ap;
  ncp = NULL;
  for (i=0;i<sp->nchans;i++)
    {
      cp = sp->chans[i];
      ap = cp->axis;
      if (y < ap->y_offset) return(ncp);
      ncp = cp;
    }
  return(ncp);
}

static int fft_axis_start = 0;

void graph_button_press_callback(chan_info *cp, int x, int y, int key_state, int button, TIME_TYPE time)
{
  snd_info *sp;
  snd_state *ss;
  sp = cp->sound;
  ss = cp->state;
  /* if combining, figure out which virtual channel the mouse is in */
  if (sp->combining == CHANNELS_COMBINED) cp = which_channel(sp,y);
  start_selection(cp,x);
  /* cp->cursor_on = 1; */
  mouse_down_time = time;
  select_channel(sp,cp->chan);
  dragged = 0;
  mouse_mark = hit_mark(cp,x,y,key_state);
  if (mouse_mark == NULL) play_mark = hit_triangle(cp,x,y);
  click_within_graph = within_graph(cp,x,y);
  if (click_within_graph == FFT) 
    {
      if (fft_style(ss) != SONOGRAM)
	fft_axis_start = x;
      else fft_axis_start = y;
    }
  else
    if (click_within_graph == LISP)
      handle_mouse_press(sp,cp,ungrf_x((cp->lisp_info)->axis,x),ungrf_y((cp->lisp_info)->axis,y),button,key_state);
}

static Float fft_axis_extent(chan_info *cp)
{
  axis_info *ap;
  fft_info *fp;
  snd_state *ss;
  fp = cp->fft;
  ap = fp->axis;
  ss = cp->state;
  if (fft_style(ss) != SONOGRAM)
    return((Float)(ap->x_axis_x1 - ap->x_axis_x0));
  else return((Float)(ap->y_axis_y0 - ap->y_axis_y1));
}

static int cursor_round(double x)
{
  int xint;
  Float xfrac;
  xint = (int)x;
  xfrac = x-xint;
  if (xfrac>.5) return(xint+1);
  return(xint);
}

static int calculate_syncd_fft(chan_info *cp, void *ptr)
{
  snd_info *sp;
  int sync = (int)ptr;
  if (cp)
    {
      sp = cp->sound;
      if (sp->syncing == sync) calculate_fft(cp,NULL);
    }
  return(0);
}

void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button)
{
  snd_info *sp;
  snd_state *ss;
  axis_info *ap;
  mark *old_mark;
  int actax,samps;
  char *des;
  sp = cp->sound;
  ss = cp->state;
  if (sp->combining == CHANNELS_COMBINED) cp = which_channel(sp,y);
  if (!dragged)
    {
      if (play_mark)
	{
	  old_mark = sp->playing_mark; /* needed because stop_playing clobbers sp->playing_mark */
	  if (sp->playing)
	    {
	      stop_playing_sound(sp);
	      set_play_button(sp,0);
	    }
	  if (play_mark != old_mark)
	    {
	      if (play_mark->sync)
		play_syncd_mark(cp,play_mark);
	      else start_playing(cp,play_mark->samp,NO_END_SPECIFIED);
	      sp->playing_mark = play_mark;
	      set_play_button(sp,1);
	    }
	  else sp->playing_mark = NULL;
	  play_mark = NULL;
	}
      else
	{
	  actax = within_graph(cp,x,y);
	  if (actax == WAVE)
	    {
	      if (button == BUTTON_2) /* the middle button */
		{
		  cp->cursor_on = 1;
		  cursor_moveto(cp,cursor_round(ungrf_x(cp->axis,x) * (double)SND_SRATE(sp)));
		  draw_graph_cursor(cp);
		  paste_region(0,cp,"Btn2");
		}
	      else 
		{
		  if (key_state & (snd_ShiftMask | snd_ControlMask | snd_MetaMask))
		    {
		      /* zoom request -> each added key zooms closer, as does each successive click */
		      ap = cp->axis;
		      samps = current_ed_samples(cp);
		      if ((samps > 0) && (ap->zx > (1.0 / (double)samps)))
			{
			  if (key_state & snd_ShiftMask) ap->zx *= .5;
			  if (key_state & snd_ControlMask) ap->zx *= .5;
			  if (key_state & snd_MetaMask) ap->zx *= .5;
			  ap->sx = (((double)(cp->cursor)/(double)SND_SRATE(sp) - ap->zx*0.5*(ap->xmax-ap->xmin)) - ap->xmin)/(ap->xmax - ap->xmin);
			  apply_x_axis_change(ap,cp,sp);
			  resize_sx(cp);
			  set_zx_scrollbar_value(cp,sqrt(ap->zx));
			}
		    }
		  else
		    {
		      cp->cursor_on = 1;
		      handle_cursor(cp,cursor_moveto(cp,cursor_round(ungrf_x(cp->axis,x) * (double)SND_SRATE(sp))));
		      if (mouse_mark)
			{
			  if (handle_mark_click(mark_id(mouse_mark)) == FALSE)
			    {
			      des = (char *)CALLOC(64,sizeof(char));
			      sprintf(des,"mark %d at sample %d",mark_id(mouse_mark),mouse_mark->samp);
			      report_in_minibuffer(sp,des);
			      FREE(des);
			    }
			}
		    }
		}
	    }
	  else
	    {
	      if (actax == FFT_MAIN)
		{
		  des = describe_fft_point(cp,x,y);
		  report_in_minibuffer(sp,des);
		  FREE(des);
		}
	      else
		if (actax == LISP)
		  handle_mouse_release(sp,cp,ungrf_x((cp->lisp_info)->axis,x),ungrf_y((cp->lisp_info)->axis,y),button,key_state);
	    }
	}
    }
  else
    {
      /* lisp graph dragged? */
      if (mouse_mark)
	{
	  finish_moving_mark(cp,mouse_mark);
	  mouse_mark = NULL;
	  dragged = 0;
	}
      else
	{
	  if (play_mark)
	    {
	      finish_moving_play_mark(cp);
	      stop_playing_sound(sp);
	      play_mark = NULL;
	      dragged = 0;
	    }
	  else
	    {
	      if (click_within_graph == WAVE)
		{
		  define_selection(cp);
		  dragged = 0;
		  reflect_edit_with_selection_in_menu();
		  if (show_selection_transform(ss)) 
		    {
		      if (sp->syncing)
			map_over_chans(ss,calculate_syncd_fft,(void *)(sp->syncing));
		      else calculate_fft(cp,NULL);
		    }
		}
	    }
	}
    }
}

static TIME_TYPE first_time = 0;
static int mouse_cursor = 0;

void graph_button_motion_callback(chan_info *cp,int x, int y, TIME_TYPE time, TIME_TYPE click_time)
{
  snd_info *sp;
  snd_state *ss;
  TIME_TYPE mouse_time,time_interval;
  int samps;
  Float old_cutoff;
  char *des;
  /* this needs to be a little slow about deciding that we are dragging, as opposed to a slow click */
  mouse_time = time;
  if ((mouse_time - mouse_down_time) < (click_time/2)) return;
  sp = cp->sound;
  if (sp->combining == CHANNELS_COMBINED) cp = which_channel(sp,y);
  select_channel(sp,cp->chan);
  if (mouse_mark)
    {
      move_mark(cp,mouse_mark,x);
      dragged = 1;
    }
  else
    {
      if (play_mark)
	{
	  if (!dragged)
	    {
	      first_time = mouse_time;
	      dragged = 1;
	      sp->srate = 0.0;
	      mouse_cursor = cp->cursor;
	      start_playing(cp,play_mark->samp,NO_END_SPECIFIED);
	      set_play_button(sp,1);
	    }
	  else
	    {
	      time_interval = mouse_time - first_time;
	      first_time = mouse_time;
	      samps = move_play_mark(cp,&mouse_cursor,x);
	      if (time_interval != 0)
		sp->srate = (Float)(samps*1000)/(Float)(time_interval*SND_SRATE(sp));
	      else sp->srate = 0.0;
	    }
	}
      else
	{
	  if (click_within_graph == WAVE)
	    {
	      if (!dragged) 
		{
		  deactivate_selection();
		  create_selection(cp);
		}
	      dragged = 1;
	      move_selection(cp,x);
	    }
	  else
	    {
	      ss = cp->state;
	      if (click_within_graph == FFT)
		{
		  /* change spectro_cutoff(ss) and redisplay fft */
		  old_cutoff = spectro_cutoff(ss);
		  if (fft_style(ss) != SONOGRAM)
		    {
		      set_spectro_cutoff(ss,spectro_cutoff(ss) + ((Float)(fft_axis_start - x)/fft_axis_extent(cp)));
		      fft_axis_start = x;
		    }
		  else 
		    {
		      set_spectro_cutoff(ss,spectro_cutoff(ss) + ((Float)(y - fft_axis_start)/fft_axis_extent(cp)));
		      fft_axis_start = y;
		    }
		  if (spectro_cutoff(ss) > 1.0) set_spectro_cutoff(ss,1.0);
		  if (spectro_cutoff(ss) < 0.001) set_spectro_cutoff(ss,0.001);
		  if (old_cutoff != spectro_cutoff(ss)) 
		    {
		      reflect_spectro(ss);
		      if (fft_style(ss) != NORMAL_FFT)
			map_over_chans(ss,sono_update,NULL);
		      else map_over_chans(ss,update_graph,NULL);
		    }
		}
	      else
		{
		  if (click_within_graph == LISP)
		    {
		      handle_mouse_drag(cp->sound,cp,
					ungrf_x((cp->lisp_info)->axis,x),
					ungrf_y((cp->lisp_info)->axis,y));
		      return;
		    }
		  if ((verbose_cursor(ss)) && (within_graph(cp,x,y) == FFT_MAIN))
		    {
		      des = describe_fft_point(cp,x,y);
		      report_in_minibuffer(cp->sound,des);
		      FREE(des);
		    }
		}
	    }
	}
    }
}


void display_frequency_response(snd_state *ss, env *e, axis_info *ap, axis_context *gax, int order, int dBing)
{
  Float *coeffs = NULL;
  int height,width,i,pts,x0,y0,x1,y1;
  Float samps_per_pixel,invpts,resp,frq,pix;
  coeffs = get_filter_coeffs(order,e);
  if (!coeffs) return;
  height = (ap->y_axis_y1 - ap->y_axis_y0);
  width = (ap->x_axis_x1 - ap->x_axis_x0);
  pts = order*4;
  if (width<50) pts = width; else if (pts < 50) pts = 50;
  if (pts <= 0) pts = 1;
  invpts = 1.0/(Float)pts;
  samps_per_pixel = (Float)(ap->x_axis_x1 - ap->x_axis_x0) * invpts;
  x1 = ap->x_axis_x0;
  resp = frequency_response(coeffs,order,0.0);
  if (dBing)
    y1 = (int)(ap->y_axis_y0 + (ss->min_dB - dB(ss,resp)) * height / ss->min_dB);
  else y1 = (int)(ap->y_axis_y0 + resp * height);
  for (i=1,pix=x1,frq=invpts;i<pts;i++,pix+=samps_per_pixel,frq+=invpts)
    {
      x0 = x1;
      x1 = (int)(pix);
      y0 = y1;
      resp = frequency_response(coeffs,order,frq);
      if (dBing)
	y1 = (int)(ap->y_axis_y0 + (ss->min_dB - dB(ss,resp)) * height / ss->min_dB);
      else y1 = (int)(ap->y_axis_y0 + resp * height);
      draw_line(gax,x0,y0,x1,y1);
    }
  FREE(coeffs);
}

#if HAVE_GUILE
#include "sg.h"


/* -------- EXTERNAL PROGRAMS -------- */

#define USE_FULL_FILE 0
#define USE_SELECTION 1
#define USE_ONE_FILE 1
#define USE_MANY_FILES 0

static SCM g_temp_filenames(SCM data)
{
  #define H_temp_filenames "(" S_temp_filenames " data) -> vector of temp filenames (used by sound-to-temp et al)"
  snd_exf *program_data;
  int i;
  SCM lst;
  program_data = (snd_exf *)(gh_scm2ulong(data));
  lst = gh_make_vector(gh_int2scm(program_data->files),SCM_BOOL_F);
  for (i=0;i<program_data->files;i++)
    gh_vector_set_x(lst,gh_int2scm(i),gh_str02scm(program_data->old_filenames[i]));
  return(lst);
}

static SCM g_sound_to_temp_1(SCM ht, SCM df, int selection, int one_file)
{
  snd_exf *program_data;
  chan_info *cp;
  int type,format;
  snd_state *ss;
  if ((selection) && (selection_is_current() == 0)) return(NO_ACTIVE_SELECTION);
  ss = get_global_state();
  cp = current_channel(ss);
  if (cp)
    {
      type = g_scm2intdef(ht,MUS_UNSUPPORTED);
      format = g_scm2intdef(df,MUS_UNSUPPORTED);
      program_data = snd_to_temp(cp,selection,one_file,type,format);
      if (program_data)
	return((SCM)(gh_ulong2scm((unsigned long)program_data)));
    }
  return(SCM_BOOL_F);
}

static SCM g_sound_to_temp(SCM ht, SCM df) 
{
  #define H_sound_to_temp "(" S_sound_to_temp " &optional header-type data-format) writes the syncd data to a temp file\n\
   with the indicated header type and data format; returns temp file name"

  return(g_sound_to_temp_1(ht,df,USE_FULL_FILE,USE_ONE_FILE));
}

static SCM g_sound_to_temps(SCM ht, SCM df) 
{
  #define H_sound_to_temps "(" S_sound_to_temps " &optional header-type data-format) writes the syncd data to mono temp files\n\
   with the indicated header type and data format; returns temp file names"

  return(g_sound_to_temp_1(ht,df,USE_FULL_FILE,USE_MANY_FILES));
}

static SCM g_selection_to_temp(SCM ht, SCM df) 
{
  #define H_selection_to_temp "(" S_selection_to_temp " &optional header-type data-format) writes the selected data to a temp file\n\
   with the indicated header type and data format; returns temp file name"

  return(g_sound_to_temp_1(ht,df,USE_SELECTION,USE_ONE_FILE));
}

static SCM g_selection_to_temps(SCM ht, SCM df) 
{
  #define H_selection_to_temps "(" S_selection_to_temps " &optional header-type data-format) writes the selected data to mono temp files\n\
   with the indicated header type and data format; returns temp file names"

  return(g_sound_to_temp_1(ht,df,USE_SELECTION,USE_MANY_FILES));
}

static SCM g_temp_to_sound(SCM data, SCM new_name, SCM origin)
{
  #define H_temp_to_sound "(" S_temp_to_sound " data new-name origin) reads new-name to complete the edit begun by " S_sound_to_temp "\n\
   using data returned by the latter and origin as the edit history entry for the edit"

  snd_exf *program_data;
  ERRS2(new_name,S_temp_to_sound);
  ERRS3(origin,S_temp_to_sound);
  program_data = (snd_exf *)(gh_scm2ulong(data));
  program_data->new_filenames[0] = gh_scm2newstr(new_name,NULL);
  temp_to_snd(program_data,gh_scm2newstr(origin,NULL));
  return(SCM_BOOL_T);
}

static SCM g_temps_to_sound(SCM data, SCM new_names, SCM origin)
{
  #define H_temps_to_sound "(" S_temps_to_sound " data new-names origin) reads new-names to complete the edit begun by " S_sound_to_temps "\n\
   using data returned by the latter and origin as the edit history entry for the edit"

  snd_exf *program_data;
  int i;
  SCM_ASSERT((gh_vector_p(new_names)),new_names,SCM_ARG2,S_temps_to_sound);
  ERRS3(origin,S_temps_to_sound);
  program_data = (snd_exf *)(gh_scm2ulong(data));
  for (i=0;i<(int)gh_vector_length(new_names);i++)
    program_data->new_filenames[i] = gh_scm2newstr(gh_vector_ref(new_names,gh_int2scm(i)),NULL);
  temp_to_snd(program_data,gh_scm2newstr(origin,NULL));
  return(SCM_BOOL_T);
}

static SCM g_sp_scan(SCM proc, int chan_choice, SCM s_beg, SCM s_end, int series, int scan, SCM org, SCM snd, SCM chn)
{
  snd_state *ss;
  chan_info *cp;
  int beg,end;
  SCM result;
  char *origin=NULL,*ed_origin = NULL;
  if (scan)
    {
      switch (chan_choice)
	{
	case SCAN_CURRENT_CHAN: origin = S_scan_chan; break;
	case SCAN_SOUND_CHANS: if (series) origin = S_scan_sound_chans; else origin = S_scan_across_sound_chans; break;
	case SCAN_ALL_CHANS: if (series) origin = S_scan_all_chans; else origin = S_scan_across_all_chans; break;
	case SCAN_SYNCD_CHANS: if (series) origin = S_scan_chans; else origin = S_scan_across_chans; break;
	}
    }
  else
    {
      switch (chan_choice)
	{
	case SCAN_CURRENT_CHAN: origin = S_map_chan; break;
	case SCAN_SOUND_CHANS: if (series) origin = S_map_sound_chans; else origin = S_map_across_sound_chans; break;
	case SCAN_ALL_CHANS: if (series) origin = S_map_all_chans; else origin = S_map_across_all_chans; break;
	case SCAN_SYNCD_CHANS: if (series) origin = S_map_chans; else origin = S_map_across_chans; break;
	}
      if (gh_string_p(org)) ed_origin = gh_scm2newstr(org,NULL);
    }
  SCM_ASSERT((gh_procedure_p(proc)),proc,SCM_ARG1,origin);
  SCM_ASSERT((gh_number_p(s_beg)) || (gh_boolean_p(s_beg)) || (SCM_UNBNDP(s_beg)),s_beg,SCM_ARG2,origin);
  SCM_ASSERT((gh_number_p(s_end)) || (gh_boolean_p(s_end)) || (SCM_UNBNDP(s_end)),s_end,SCM_ARG3,origin);
  ss = get_global_state();
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  beg = g_scm2intdef(s_beg,0);
  end = g_scm2intdef(s_end,0);
  if (scan)
    {
      if (series)
	return(series_scan(ss,cp,proc,chan_choice,beg,end));
      else return(parallel_scan(ss,cp,proc,chan_choice,beg,end));
    }
  else
    {
      if (ed_origin) origin = ed_origin;
      if (series)
	result = series_map(ss,cp,proc,chan_choice,beg,end,origin); /* origin copied by insert_samples_1 or whatever in snd-edits.c */
      else result = parallel_map(ss,cp,proc,chan_choice,beg,end,origin);
      if (ed_origin) free(ed_origin);
      return(result);
    }
}

static SCM g_scan_chan(SCM proc, SCM beg, SCM end, SCM snd, SCM chn) 
{ 
  #define H_scan_chan "(" S_scan_chan " func &optional (start 0) end snd chn)\n\
   apply func to samples in current channel (or the specified channel)\n\
   func is a function of one argument, either the current sample, or #f (to indicate end-of-data)\n\
   if func returns non-#f, the scan stops, and the value is returned to the caller with the sample number"

  ERRCP(S_scan_chan,snd,chn,4); 
  return(g_sp_scan(proc,SCAN_CURRENT_CHAN,beg,end,TRUE,TRUE,SCM_BOOL_F,snd,chn));
}

static SCM g_scan_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_chans "(" S_scan_chans " func &optional (start 0) end)\n\
   apply func to samples in all sync'd channels, one channel after another"
   
  return(g_sp_scan(proc,SCAN_SYNCD_CHANS,beg,end,TRUE,TRUE,SCM_BOOL_F,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_scan_all_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_all_chans "(" S_scan_all_chans " func &optional (start 0) end)\n\
   apply func to samples in all channels, one after the other"

  return(g_sp_scan(proc,SCAN_ALL_CHANS,beg,end,TRUE,TRUE,SCM_BOOL_F,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_scan_sound_chans(SCM proc, SCM beg, SCM end, SCM snd) 
{ 
  #define H_scan_sound_chans "(" S_scan_sound_chans " func &optional (start 0) end snd)\n\
   apply func to samples in all of sound snd's channels"

  ERRSP(S_scan_sound_chans,snd,4); 
  return(g_sp_scan(proc,SCAN_SOUND_CHANS,beg,end,TRUE,TRUE,SCM_BOOL_F,snd,SCM_BOOL_F));
}

static SCM g_scan_across_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_across_chans "(" S_scan_across_chans " func &optional (start 0) end)\n\
   apply func to samples in all sync'd channels in parallel"

  return(g_sp_scan(proc,SCAN_SYNCD_CHANS,beg,end,FALSE,TRUE,SCM_BOOL_F,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_scan_across_all_chans(SCM proc, SCM beg, SCM end) 
{ 
  #define H_scan_across_all_chans "(" S_scan_across_all_chans " func &optional (start 0) end)\n\
   apply func to samples in all channels in parallel"

  return(g_sp_scan(proc,SCAN_ALL_CHANS,beg,end,FALSE,TRUE,SCM_BOOL_F,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_scan_across_sound_chans(SCM proc, SCM beg, SCM end, SCM snd) 
{ 
  #define H_scan_across_sound_chans "(" S_scan_across_sound_chans " func &optional (start 0) end snd)\n\
   apply func to samples in sound snd's channels in parallel"

  ERRSP(S_scan_across_sound_chans,snd,4); 
  return(g_sp_scan(proc,SCAN_SOUND_CHANS,beg,end,FALSE,TRUE,SCM_BOOL_F,snd,SCM_BOOL_F));
}

static SCM g_map_chan(SCM proc, SCM beg, SCM end, SCM org, SCM snd, SCM chn) 
{ 
  #define H_map_chan "(" S_map_chan "func &optional (start 0) end edname snd chn)\n\
   apply func to samples in current channel, edname is the edit history name for this editing operation"

  ERRCP(S_map_chan,snd,chn,5); 
  return(g_sp_scan(proc,SCAN_CURRENT_CHAN,beg,end,TRUE,FALSE,org,snd,chn));
}

static SCM g_map_chans(SCM proc, SCM beg, SCM end, SCM org) 
{ 
  #define H_map_chans "(" S_map_chans "func &optional (start 0) end edname)\n\
   apply func to currently sync'd channels, edname is the edit history name for this editing operation"

  return(g_sp_scan(proc,SCAN_SYNCD_CHANS,beg,end,TRUE,FALSE,org,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_map_all_chans(SCM proc, SCM beg, SCM end, SCM org) 
{ 
  #define H_map_all_chans "(" S_map_all_chans "func &optional (start 0) end edname)\n\
    apply func to all channels, edname is the edit history name for this editing operation"

  return(g_sp_scan(proc,SCAN_ALL_CHANS,beg,end,TRUE,FALSE,org,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_map_sound_chans(SCM proc, SCM beg, SCM end, SCM org,SCM snd) 
{
  #define H_map_sound_chans "(" S_map_sound_chans "func &optional (start 0) end edname snd)\n\
    apply func to sound snd's channels, edname is the edit history name for this editing operation"

  ERRSP(S_map_sound_chans,snd,5); 
  return(g_sp_scan(proc,SCAN_SOUND_CHANS,beg,end,TRUE,FALSE,org,snd,SCM_BOOL_F));
}

static SCM g_map_across_chans(SCM proc, SCM beg, SCM end, SCM org) 
{
  #define H_map_across_chans "(" S_map_across_chans "func &optional (start 0) end edname)\n\
   apply func to currently sync'd channels in parallel, edname is the edit history name for this editing operation"

  return(g_sp_scan(proc,SCAN_SYNCD_CHANS,beg,end,FALSE,FALSE,org,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_map_across_all_chans(SCM proc, SCM beg, SCM end, SCM org) 
{
  #define H_map_across_all_chans "(" S_map_across_all_chans "func &optional (start 0) end edname)\n\
   apply func to all channels in parallel, edname is the edit history name for this editing operation"

  return(g_sp_scan(proc,SCAN_ALL_CHANS,beg,end,FALSE,FALSE,org,SCM_BOOL_F,SCM_BOOL_F));
}

static SCM g_map_across_sound_chans(SCM proc, SCM beg, SCM end, SCM org, SCM snd) 
{
  #define H_map_across_sound_chans "(" S_map_across_sound_chans "func &optional (start 0) end edname snd)\n\
   apply func to sound snd's channels in parallel, edname is the edit history name for this editing operation"

  ERRSP(S_map_across_sound_chans,snd,5); 
  return(g_sp_scan(proc,SCAN_SOUND_CHANS,beg,end,FALSE,FALSE,org,snd,SCM_BOOL_F));
}

static SCM g_find(SCM expr, SCM sample, SCM snd_n, SCM chn_n)
{
  #define H_find "(" S_find " func &optional (start-samp 0) snd chn) applyies func, a function of one argument,\n\
   the current sample, to each sample in snd's channel chn, starting at 'start-samp' until func returns #t"

  /* no free here -- it's handled as ss->search_expr in snd-find.c */
  chan_info *cp = NULL;
  SCM_ASSERT((gh_string_p(expr) || gh_procedure_p(expr)),expr,SCM_ARG1,S_find);
  ERRB2(sample,S_find);
  ERRCP(S_find,snd_n,chn_n,3);
  if (gh_string_p(expr))
    {
      cp = get_cp(snd_n,chn_n);
      if (cp) 
	RTNINT(snd_find_1(cp,gh_scm2newstr(expr,NULL),g_scm2intdef(sample,0),FALSE)); 
      else return(NO_SUCH_CHANNEL);
    }
  else return(g_scan_chan(expr,sample,SCM_BOOL_F,snd_n,chn_n));
  return(SCM_BOOL_F);
}

static SCM g_count_matches(SCM expr, SCM sample, SCM snd_n, SCM chn_n)
{
  #define H_count_matches "(" S_count_matches " func &optional (start-samp 0) snd chn) returns how many\n\
   samples satisfy func (a function of one argument, the current sample, returning #t upon match)"

  chan_info *cp = NULL;
  int samp = 0,matches,lim;
  SCM match,cursamp;
  SCM_ASSERT((gh_string_p(expr) || gh_procedure_p(expr)),expr,SCM_ARG1,S_count_matches);
  ERRB2(sample,S_count_matches);
  ERRCP(S_count_matches,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  samp = g_scm2intdef(sample,0);
  if (gh_string_p(expr))
    RTNINT(snd_find_1(cp,gh_scm2newstr(expr,NULL),g_scm2intdef(sample,0),TRUE));
  else
    {
      matches = 0;
      lim = current_ed_samples(cp);
      while (samp < lim)
	{
	  cursamp = gh_int2scm(samp);
	  match = g_scan_chan(expr,cursamp,SCM_BOOL_F,snd_n,chn_n);
	  if ((gh_list_p(match)) && (SCM_TRUE_P(scm_list_ref(match,gh_int2scm(0)))))
	    {
	      matches++;
	      samp = g_scm2int(scm_list_ref(match,gh_int2scm(1))) + 1;
	    }
	  else break;
	}
      return(gh_int2scm(matches));
    }
  return(SCM_BOOL_F);
}

static SCM g_prompt_in_minibuffer(SCM msg, SCM callback, SCM snd_n)
{
  #define H_prompt_in_minibuffer "(" S_prompt_in_minibuffer " msg callback &optional snd) posts msg in snd's minibuffer\n\
   then when the user eventually responds, invokes the function callback with the response and snd (the index)"

  snd_info *sp;
  char *str;  
  ERRS1(msg,S_prompt_in_minibuffer);
  SCM_ASSERT((SCM_UNBNDP(callback)) || (gh_boolean_p(callback)) || gh_procedure_p(callback),callback,SCM_ARG2,S_prompt_in_minibuffer);
  ERRSP(S_prompt_in_minibuffer,snd_n,3);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  if ((sp->prompt_callback) && (gh_procedure_p(sp->prompt_callback))) snd_unprotect(sp->prompt_callback);
  if (gh_procedure_p(callback)) 
    {
      sp->prompt_callback = callback;
      snd_protect(sp->prompt_callback);
    }
  else sp->prompt_callback = SCM_BOOL_F;
  str = gh_scm2newstr(msg,NULL);
  g_prompt(sp,str);
  free(str);
  return(SCM_BOOL_F);
}

static SCM g_report_in_minibuffer(SCM msg, SCM snd_n)
{
  #define H_report_in_minibuffer "(" S_report_in_minibuffer " msg &optional snd) displays msg in snd's minibuffer"
  snd_info *sp;
  char *str;  
  ERRS1(msg,S_report_in_minibuffer);
  ERRSP(S_report_in_minibuffer,snd_n,2);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  str = gh_scm2newstr(msg,NULL);
  report_in_minibuffer(sp,str);
  free(str);
  return(msg);
}

static SCM g_append_to_minibuffer(SCM msg, SCM snd_n)
{
  #define H_append_to_minibuffer "(" S_append_to_minibuffer " msg &optional snd) appends msg to snd's minibuffer"
  snd_info *sp;
  char *str;
  ERRS1(msg,S_append_to_minibuffer);
  ERRSP(S_append_to_minibuffer,snd_n,2);
  sp = get_sp(snd_n);
  if (sp == NULL) return(NO_SUCH_SOUND);
  str = gh_scm2newstr(msg,NULL);
  append_to_minibuffer(sp,str);
  free(str);
  return(msg);
}

static SCM g_bind_key(SCM key, SCM state, SCM code, SCM ignore_prefix)
{
  #define H_bind_key "(" S_bind_key " key modifiers func (ignore-prefix #f)) causes 'key' (an integer)\n\
   when typed with 'modifiers' (1:shift, 4:control, 8:meta) to invoke 'func', a function of\n\
   no arguments.  If ignore-prefix is #t, preceding C-u arguments are not handled by Snd itself.\n\
   The function should return one of the cursor choices (e.g. cursor-no-action)."

  int ip;
  ERRN1(key,S_bind_key);
  ERRN2(state,S_bind_key);
  SCM_ASSERT((SCM_FALSEP(code) || gh_procedure_p(code)),code,SCM_ARG3,S_bind_key);
  if ((SCM_FALSEP(ignore_prefix)) || (SCM_UNBNDP(ignore_prefix)) ||  
      ((gh_number_p(ignore_prefix)) && (g_scm2int(ignore_prefix) == 0)))
    ip = 0;
  else ip = 1;
  if (SCM_FALSEP(code))
    set_keymap_entry(g_scm2int(key),g_scm2int(state),ip,SCM_UNDEFINED);
  else 
    if (procedure_ok(code,0,0,S_bind_key,"func",3))
      set_keymap_entry(g_scm2int(key),g_scm2int(state),ip,code);
  return(SCM_BOOL_T);
}

static SCM g_key(SCM kbd, SCM buckybits, SCM snd, SCM chn)
{
  #define H_key "(" S_key " key modifiers &optional snd chn) simulates typing 'key' with 'modifiers' in snd's channel chn"
  chan_info *cp;
  ERRN1(kbd,S_key);
  ERRN2(buckybits,S_key);
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  RTNINT(keyboard_command(cp,g_scm2int(kbd),g_scm2int(buckybits)));
}

static SCM g_save_macros(void) 
{
  #define H_save_macros "(" S_save_macros ") saves keyboard macros in Snd's init file (.snd)"
  FILE *fd;
  snd_state *ss;
  ss = get_global_state();
  fd = open_snd_init_file(ss);
  save_macro_state(fd);
  fclose(fd);
  return(SCM_BOOL_F);
}

static SCM g_forward_graph(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_graph "(" S_forward_graph " &optional (count 1) snd chn) moves the 'selected' graph forward by count"
  int val;
  chan_info *cp;
  ERRB1(count,S_forward_graph);
  ERRCP(S_forward_graph,snd,chn,2);
  cp = get_cp(snd,chn);
  val = g_scm2intdef(count,1);
  if (cp) goto_next_graph(cp,val); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

static SCM g_backward_graph(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_graph "(" S_backward_graph " &optional (count 1) snd chn) moves the 'selected' graph back by count"
  int val;
  chan_info *cp;
  ERRB1(count,S_backward_graph);
  ERRCP(S_backward_graph,snd,chn,2);
  cp = get_cp(snd,chn);
  val = -(g_scm2intdef(count,1));
  if (cp) goto_previous_graph(cp,val); else return(NO_SUCH_CHANNEL);
  RTNINT(val);
}

enum {FFTF,WAVEF,LENGTHF,CURSORF,MAXAMPF,GRAPHINGF,LOSAMPF,HISAMPF,SQUELCH_UPDATE,AP_SX,AP_SY,AP_ZX,AP_ZY,EDITF,CURSOR_STYLE,EDIT_HOOK,UNDO_HOOK};

static SCM cp_iread(SCM snd_n, SCM chn_n, int fld)
{
  chan_info *cp;
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  switch(fld)
    {
    case EDITF: RTNINT(cp->edit_ctr); break;
    case FFTF: RTNBOOL(cp->ffting); break;
    case WAVEF: RTNBOOL(cp->waving); break;
    case CURSORF: RTNINT(cp->cursor); break;
    case LENGTHF: RTNINT(current_ed_samples(cp)); break;
    case MAXAMPF: RTNFLT(get_maxamp(cp->sound,cp)); break;
    case GRAPHINGF: RTNBOOL(cp->lisp_graphing); break;
    case LOSAMPF: if (cp->axis) RTNINT((cp->axis)->losamp); break;
    case HISAMPF: if (cp->axis) RTNINT((cp->axis)->hisamp); break;
    case SQUELCH_UPDATE: RTNBOOL(cp->squelch_update); break;
    case AP_SX: if (cp->axis) RTNFLT((cp->axis)->sx); break;
    case AP_SY: if (cp->axis) RTNFLT((cp->axis)->sy); break;
    case AP_ZX: if (cp->axis) RTNFLT((cp->axis)->zx); break;
    case AP_ZY: if (cp->axis) RTNFLT((cp->axis)->zy); break;
    case CURSOR_STYLE: RTNINT(cp->cursor_style); break;
    case EDIT_HOOK: return(cp->edit_hook); break;
    case UNDO_HOOK: return(cp->undo_hook); break;
    }
  return(SCM_BOOL_F);
}

static SCM cp_iwrite(SCM snd_n, SCM chn_n, SCM on, int fld)
{
  chan_info *cp;
  int val = 0;
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  switch (fld)
    {
    case FFTF: fftb(cp,val = bool_int_or_one(on)); update_graph(cp,NULL); break;
    case WAVEF: waveb(cp,val = bool_int_or_one(on)); update_graph(cp,NULL); break;
    case CURSORF: cp->cursor_on = 1; handle_cursor(cp,cursor_moveto(cp,val = g_scm2intdef(on,1))); break;
    case GRAPHINGF: 
      cp->lisp_graphing = bool_int_or_one(on); 
      val = cp->lisp_graphing;
      update_graph(cp,NULL); 
      break;
    case LOSAMPF: set_x_axis_x0(cp,val = g_scm2intdef(on,0)); return(on); break;
    case HISAMPF: set_x_axis_x1(cp,val = g_scm2intdef(on,1)); return(on); break;
    case SQUELCH_UPDATE: cp->squelch_update = bool_int_or_one(on); break;
    case CURSOR_STYLE: cp->cursor_style = g_scm2intdef(on,0); update_graph(cp,NULL); return(on); break;
    }
  RTNBOOL(val);
}

static SCM g_edit_position(SCM snd_n, SCM chn_n) 
{
  #define H_edit_position "(" S_edit_position " &optional snd chn) -> current edit history position in snd's channel chn"
  ERRCP(S_edit_position,snd_n,chn_n,1);
  return(cp_iread(snd_n,chn_n,EDITF));
}

static SCM g_ffting(SCM snd_n, SCM chn_n) 
{
  #define H_ffting "(" S_ffting " &optional snd chn) -> #t if fft display is active in snd's channel chn"
  ERRCP(S_ffting,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,FFTF));
}

static SCM g_set_ffting(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_ffting "(" S_set_ffting " &optional (val #t) snd chn) sets whether snd's channel chn is displaying ffts"
  ERRB1(on,S_set_ffting); 
  ERRCP(S_set_ffting,snd_n,chn_n,2);
  return(cp_iwrite(snd_n,chn_n,on,FFTF));
}

static SCM g_waving(SCM snd_n, SCM chn_n) 
{
  #define H_waving "(" S_waving " &optional snd chn) -> #t if time domain display is active in snd's channel chn"
  ERRCP(S_waving,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,WAVEF));
}

static SCM g_set_waving(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_waving "(" S_set_waving " &optional (val #t) snd chn) sets whether snd's channel chn is displaying time domain data"
  ERRB1(on,S_set_waving); 
  ERRCP(S_set_waving,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,WAVEF));
}

static SCM g_graphing(SCM snd_n, SCM chn_n) 
{
  #define H_graphing "(" S_graphing " &optional snd chn) -> #t if lisp-generated data display is active in snd's channel chn"
  ERRCP(S_graphing,snd_n,chn_n,1);
  return(cp_iread(snd_n,chn_n,GRAPHINGF));
}

static SCM g_set_graphing(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_graphing "(" S_set_graphing " &optional (val #t) snd chn) sets whether snd's channel chn is displaying lisp-generated data"
  ERRB1(on,S_set_graphing); 
  ERRCP(S_set_graphing,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,GRAPHINGF));
}

static SCM g_cursor(SCM snd_n, SCM chn_n) 
{
  #define H_cursor "(" S_cursor " &optional snd chn) -> current cursor location in snd's channel chn"
  ERRCP(S_cursor,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,CURSORF));
}

static SCM g_set_cursor(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_cursor "(" S_set_cursor " val &optional snd chn) sets the current cursor location in snd's channel chn"
  ERRB1(on,S_set_cursor); 
  ERRCP(S_set_cursor,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,CURSORF));
}

static SCM g_cursor_style(SCM snd_n, SCM chn_n) 
{
  #define H_cursor_style "(" S_cursor_style " &optional snd chn) -> current cursor style in snd's channel chn"
  ERRCP(S_cursor_style,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,CURSOR_STYLE));
}

static SCM g_set_cursor_style(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_cursor_style "(" S_set_cursor_style " val &optional snd chn) sets the current cursor style in snd's channel chn"
  ERRB1(on,S_set_cursor_style); 
  ERRCP(S_set_cursor_style,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,CURSOR_STYLE));
}

static SCM g_frames(SCM snd_n, SCM chn_n) 
{
  #define H_frames "(" S_frames " &optional snd chn) -> number of frames of data in snd's channel chn"
  ERRCP(S_frames,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,LENGTHF));
}

static SCM g_maxamp(SCM snd_n, SCM chn_n) 
{
  #define H_maxamp "(" S_maxamp " &optional snd chn) -> max amp of data in snd's channel chn"
  ERRCP(S_maxamp,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,MAXAMPF));
}

static SCM g_squelch_update(SCM snd_n, SCM chn_n) 
{
  #define H_squelch_update "(" S_squelch_update " &optional snd chn) -> #t if updates (redisplays) are off in snd's channel chn"
  ERRCP(S_squelch_update,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,SQUELCH_UPDATE));
}

static SCM g_set_squelch_update(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_squelch_update "(" S_set_squelch_update " &optional (on #t) snd chn) sets whether updates (redisplays) are off in snd's channel chn"
  ERRB1(on,S_set_squelch_update); 
  ERRCP(S_set_squelch_update,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,SQUELCH_UPDATE));
}

static SCM g_ap_sx(SCM snd_n, SCM chn_n) 
{
  #define H_x_position_slider "(" S_x_position_slider " &optional snd chn) -> current x axis position slider of snd channel chn"
  ERRCP(S_x_position_slider,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,AP_SX));
}

static SCM g_ap_sy(SCM snd_n, SCM chn_n) 
{
  #define H_y_position_slider "(" S_y_position_slider " &optional snd chn) -> current y axis position slider of snd channel chn"
  ERRCP(S_y_position_slider,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,AP_SY));
}

static SCM g_ap_zx(SCM snd_n, SCM chn_n) 
{
  #define H_x_zoom_slider "(" S_x_zoom_slider " &optional snd chn) -> current x axis zoom slider of snd channel chn"
  ERRCP(S_x_zoom_slider,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,AP_ZX));
}

static SCM g_ap_zy(SCM snd_n, SCM chn_n) 
{
  #define H_y_zoom_slider "(" S_y_zoom_slider " &optional snd chn) -> current y axis zoom slider of snd channel chn"
  ERRCP(S_y_zoom_slider,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,AP_ZY));
}

static SCM g_edit_hook(SCM snd_n, SCM chn_n) 
{
  #define H_edit_hook "(" S_edit_hook " &optional snd chn) -> snd's channel chn's edit-hook"
  ERRCP(S_edit_hook,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,EDIT_HOOK));
}

static SCM g_undo_hook(SCM snd_n, SCM chn_n) 
{
  #define H_undo_hook "(" S_undo_hook " &optional snd chn) -> snd's channel chn's undo-hook"
  ERRCP(S_undo_hook,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,UNDO_HOOK));
}

static SCM g_peaks(SCM filename, SCM snd_n, SCM chn_n)
{
  #define H_peaks "(" S_peaks " &optional filename snd chn) writes current fft peaks data to filename, or\n\
   to the help dialog if filename is omitted"

  chan_info *cp;
  char *name = NULL,*urn;
  int err;
  ERRSB1(filename,S_peaks);
  ERRCP(S_peaks,snd_n,chn_n,2);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_string_p(filename))
    {
      urn = gh_scm2newstr(filename,NULL);
      name = mus_file_full_name(urn);
      free(urn);
    }
  else name = NULL;
  err = display_fft_peaks(cp,name);
  if (name) FREE(name);
  if ((gh_string_p(filename)) && (err == 0)) return(filename);
  return(SCM_BOOL_F);
}

static SCM g_left_sample(SCM snd_n, SCM chn_n) 
{
  #define H_left_sample "(" S_left_sample " &optional snd chn) -> left sample number in time domain window"
  ERRCP(S_left_sample,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,LOSAMPF));
}

static SCM g_set_left_sample(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_left_sample "(" S_set_left_sample " val &optional snd chn) sets left sample number in time domain window to val"
  ERRB1(on,S_set_left_sample); 
  ERRCP(S_set_left_sample,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,LOSAMPF));
}

static SCM g_right_sample(SCM snd_n, SCM chn_n) 
{
  #define H_right_sample "(" S_right_sample " &optional snd chn) -> right sample number in time domain window"
  ERRCP(S_right_sample,snd_n,chn_n,1); 
  return(cp_iread(snd_n,chn_n,HISAMPF));
}

static SCM g_set_right_sample(SCM on, SCM snd_n, SCM chn_n) 
{
  #define H_set_right_sample "(" S_set_right_sample " val &optional snd chn) sets right sample number in time domain window to val"
  ERRB1(on,S_set_right_sample); 
  ERRCP(S_set_right_sample,snd_n,chn_n,2); 
  return(cp_iwrite(snd_n,chn_n,on,HISAMPF));
}

static SCM g_edits(SCM snd_n, SCM chn_n)
{
  #define H_edits "(" S_edits " &optional snd chn) returns a vector of undoable and redoable edits in snd's channel chn"
  chan_info *cp;
  int i;
  ERRCP(S_edits,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  for (i=cp->edit_ctr+1;i<cp->edit_size;i++)
    if (!(cp->edits[i])) break;
  return(SCM_LIST2(gh_int2scm(cp->edit_ctr),gh_int2scm(i-cp->edit_ctr-1)));
}

static SCM g_set_x_bounds(SCM beg, SCM end, SCM snd_n, SCM chn_n)
{
  #define H_set_x_bounds "(" S_set_x_bounds " x0 x1 &optional snd chn) sets x axis bounds (seconds) for snd's channel chn"
  chan_info *cp;
  Float x0,x1;
  ERRN1(beg,S_set_x_bounds);
  ERRN2(end,S_set_x_bounds);
  ERRCP(S_set_x_bounds,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  x0 = gh_scm2double(beg);
  x1 = gh_scm2double(end);
  if (x1 > x0)
    set_x_axis_x0x1(cp,x0,x1);
  else return(IMPOSSIBLE_BOUNDS);
  return(SCM_BOOL_F);
}

static SCM g_set_y_bounds(SCM y0, SCM y1, SCM snd_n, SCM chn_n)
{
  #define H_set_y_bounds "(" S_set_y_bounds " &optional y0 y1 snd chn) sets y axis bounds for snd's channel chn.\n\
   if bounds are omitted, they're set to reflect current max amp"

  chan_info *cp;
  Float low,hi;
  ERRB1(y0,S_set_x_bounds);
  ERRB2(y1,S_set_x_bounds);
  ERRCP(S_set_y_bounds,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  if (gh_number_p(y0))
    {
      low = gh_scm2double(y0);
      if (gh_number_p(y1))
	hi = gh_scm2double(y1);
      else
	{
	  if (low < 0.0)
	    hi = -low;
	  else
	    {
	      hi = low;
	      low = -low;
	    }
	}
    }
  else
    {
      /* if no bounds given, use maxamp */
      hi = get_maxamp(cp->sound,cp);
      if (hi < 0.0) hi = -hi;
      if (hi == 0.0) hi = .001;
      low = -hi;
    }
  if (hi > low)
    set_y_axis_y0y1(cp,low,hi);
  else return(IMPOSSIBLE_BOUNDS);
  return(SCM_BOOL_F);
}

static SCM g_x_bounds(SCM snd_n, SCM chn_n)
{
  #define H_x_bounds "(" S_x_bounds " &optional snd chn) returns a list (x0 x1) giving the current x axis bounds of snd channel chn"
  chan_info *cp;
  axis_info *ap;
  ERRCP(S_x_bounds,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  ap = cp->axis;
  return(SCM_LIST2(gh_double2scm(ap->x0),gh_double2scm(ap->x1)));
}

static SCM g_y_bounds(SCM snd_n, SCM chn_n)
{
  #define H_y_bounds "(" S_y_bounds " &optional snd chn) returns a list (y0 y1) giving the current y axis bounds of snd channel chn"
  chan_info *cp;
  axis_info *ap;
  ERRCP(S_y_bounds,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  ap = cp->axis;
  return(SCM_LIST2(gh_double2scm(ap->y0),gh_double2scm(ap->y1)));
}

static SCM g_dot_size(void) {snd_state *ss; ss = get_global_state(); RTNINT(dot_size(ss));}
static SCM g_set_dot_size(SCM size) 
{
  #define H_dot_size "(" S_dot_size ") -> size in pixels of dots when graphing with dots (1)"
  #define H_set_dot_size "(" S_set_dot_size " val) sets " S_dot_size
  snd_state *ss;
  ERRN1(size,S_set_dot_size); 
  ss = get_global_state();
  set_dot_size(ss,g_scm2int(size));
  RTNINT(dot_size(ss));
}

static SCM g_forward_sample(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_sample "(" S_forward_sample " &optional (count 1) snd chn) moves the cursor forward count samples"
  chan_info *cp;
  ERRB1(count,S_forward_sample); 
  ERRCP(S_forward_sample,snd,chn,2);
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  handle_cursor(cp,cursor_move(cp,g_scm2intdef(count,1))); 
  RTNINT(cp->cursor);
}

static SCM g_backward_sample(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_sample "(" S_backward_sample " &optional (count 1) snd chn) moves the cursor back count samples"
  chan_info *cp;
  ERRB1(count,S_backward_sample); 
  ERRCP(S_backward_sample,snd,chn,2);
  cp = get_cp(snd,chn);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  handle_cursor(cp,cursor_move(cp,-(g_scm2intdef(count,1)))); 
  RTNINT(cp->cursor);
}

static SCM g_smooth(SCM beg, SCM num, SCM snd_n, SCM chn_n)
{
  #define H_smooth "(" S_smooth " start-samp samps &optional snd chn) smooths data from start-samp for samps in snd's channel chn"
  chan_info *cp;
  ERRN1(beg,S_smooth);
  ERRN2(num,S_smooth);
  ERRCP(S_smooth,snd_n,chn_n,3);
  cp = get_cp(snd_n,chn_n);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  cos_smooth(cp,g_scm2int(beg),g_scm2int(num),FALSE,S_smooth); 
  return(SCM_BOOL_T);
}

static SCM g_smooth_selection(void)
{
  #define H_smooth_selection "(" S_smooth_selection ") smooths the data in the currently selected portion"
  chan_info *cp;
  if (selection_is_current() == 0) return(NO_ACTIVE_SELECTION);
  cp = get_cp(SCM_BOOL_F,SCM_BOOL_F);
  if (cp == NULL) return(NO_SUCH_CHANNEL);
  cos_smooth(cp,0,0,TRUE,S_smooth_selection);
  return(SCM_BOOL_T);
}

static SCM g_reverse_sound(SCM snd_n, SCM chn_n)
{
  #define H_reverse_sound "(" S_reverse_sound " &optional snd chn) reverses snd's channel chn"
  chan_info *cp;
  ERRCP(S_reverse_sound,snd_n,chn_n,1);
  cp = get_cp(snd_n,chn_n);
  if (cp) reverse_sound(cp,FALSE); else return(NO_SUCH_CHANNEL);
  return(SCM_BOOL_F);
}

static SCM g_reverse_selection(void)
{
  #define H_reverse_selection "(" S_reverse_selection ") reverses the data in the currently selected portion"
  chan_info *cp;
  if (selection_is_current() == 0) return(NO_ACTIVE_SELECTION);
  cp = get_cp(SCM_BOOL_F,SCM_BOOL_F);
  if (cp) reverse_sound(cp,TRUE); else return(NO_SUCH_CHANNEL);
  return(SCM_BOOL_F);
}

static SCM g_swap_channels(SCM snd0, SCM chn0, SCM snd1, SCM chn1, SCM beg, SCM dur)
{
  #define H_swap_channels "(" S_swap_channels " snd0 chn0 snd1 chn1) swaps the indicated channels"
  chan_info *cp0,*cp1;
  snd_fd *c0,*c1;
  int dur0,dur1,beg0=0,num;
  snd_info *sp;
  ERRCP(S_swap_channels,snd0,chn0,1);
  cp0 = get_cp(snd0,chn0);
  if (SCM_INUMP(snd1) && SCM_INUMP(chn1)) 
    {
      ERRCP(S_swap_channels,snd1,chn1,3);
      cp1 = get_cp(snd1,chn1);
    }
  else
    {
      if (SCM_INUMP(snd1))
	sp = get_sp(snd1);
      else sp = cp0->sound;
      if (SCM_INUM(snd0) == SCM_INUM(snd1))
	{
	  if ((cp0->chan+1) < sp->nchans)
	    cp1 = sp->chans[cp0->chan + 1];
	  else cp1 = sp->chans[0];
	}
      else cp1 = sp->chans[0];
    }
  if (SCM_INUMP(beg)) beg0 = SCM_INUM(beg);
  if (SCM_INUMP(dur)) 
    num = SCM_INUM(dur);
  else
    {
      dur0 = current_ed_samples(cp0);
      dur1 = current_ed_samples(cp1);
      if (dur0 > dur1) num = dur1; else num = dur0;
    }
  if ((cp0) && (cp1))
    {
      c0 = init_sample_read(beg0,cp0,READ_FORWARD);
      c1 = init_sample_read(beg0,cp1,READ_FORWARD);
      swap_channels(cp0->state,beg0,num,c0,c1);
      free_snd_fd(c0);
      free_snd_fd(c1);
      return(SCM_BOOL_F);
    }
  return(NO_SUCH_CHANNEL);
}

static SCM mouse_press_hook,mark_click_hook;
static SCM mouse_release_hook,mouse_drag_hook,key_press_hook,fft_hook;
static SCM graph_hook,after_graph_hook;


#if (!HAVE_GUILE_1_3_0)
static int after_fft(snd_state *ss, chan_info *cp, Float scaler)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->fft_hook_active))
    {
      if (HOOKED(fft_hook))
	{
	  ss->fft_hook_active = 1;
	  res = g_c_run_progn_hook(fft_hook,
				   SCM_LIST3(gh_int2scm((cp->sound)->index),
					     gh_int2scm(cp->chan),
					     gh_double2scm(scaler)));
	  ss->fft_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

static int dont_graph(snd_state *ss, chan_info *cp)
{
  SCM res = SCM_BOOL_F;
  if (!(ss->graph_hook_active))
    {
      if (HOOKED(graph_hook))
	{
	  ss->graph_hook_active = 1;
	  res = g_c_run_progn_hook(graph_hook,
				   SCM_LIST4(gh_int2scm((cp->sound)->index),
					     gh_int2scm(cp->chan),
					     gh_double2scm((cp->axis)->y0),
					     gh_double2scm((cp->axis)->y1)));
	  /* (add-hook! graph-hook (lambda (a b c d) (snd-print (format #f "~A ~A ~A ~A" a b c d)))) */
	  ss->graph_hook_active = 0;
	}
    }
  return(SCM_TRUE_P(res));
}

static void after_graph(chan_info *cp)
{
  if (HOOKED(after_graph_hook))
    g_c_run_progn_hook(after_graph_hook,SCM_LIST2(gh_int2scm((cp->sound)->index),gh_int2scm(cp->chan)));
  /* (add-hook! after-graph-hook (lambda (a b) (snd-print (format #f "~A ~A" a b)))) */
}

static int handle_mark_click(int id)
{
  SCM res = SCM_BOOL_F;
  if (HOOKED(mark_click_hook))
    res = g_c_run_progn_hook(mark_click_hook,SCM_LIST1(gh_int2scm(id)));
  return(SCM_TRUE_P(res));
}
  
/* mouse/key events within lisp graph */

static void handle_mouse_event(SCM hook, snd_info *sp, chan_info *cp, Float x, Float y, int button, int state)
{
  if (HOOKED(hook))
    g_c_run_progn_hook(hook,
		       SCM_LIST6(gh_int2scm(sp->index),
				 gh_int2scm(cp->chan),
				 gh_int2scm(button),
				 gh_int2scm(state),
				 gh_double2scm(x),
				 gh_double2scm(y)));
}

static void handle_mouse_release(snd_info *sp, chan_info *cp, Float x, Float y, int button, int state)
{
  handle_mouse_event(mouse_release_hook,sp,cp,x,y,button,state);
}

static void handle_mouse_press(snd_info *sp, chan_info *cp, Float x, Float y, int button, int state)
{
  handle_mouse_event(mouse_press_hook,sp,cp,x,y,button,state);
}

static void handle_mouse_drag(snd_info *sp, chan_info *cp, Float x, Float y)
{
  handle_mouse_event(mouse_drag_hook,sp,cp,x,y,-1,-1);
}

static int handle_key_press(chan_info *cp, int key, int state)
{
  /* return TRUE to keep this key press from being passed to keyboard_command */
  SCM res = SCM_BOOL_F;
  if (HOOKED(key_press_hook))
    res = g_c_run_or_hook(key_press_hook,
			  SCM_LIST4(gh_int2scm((cp->sound)->index),
				    gh_int2scm(cp->chan),
				    gh_int2scm(key),
				    gh_int2scm(state)));
  return(SCM_TRUE_P(res));
}

#else
static int after_fft(snd_state *ss, chan_info *cp, Float scaler) {return(0);}
static int dont_graph(snd_state *ss, chan_info *cp) {return(0);}
static void after_graph(chan_info *cp) {}
static int handle_mark_click(int id) {return(0);}
static void handle_mouse_release(snd_info *sp, chan_info *cp, Float x, Float y, int button, int state) {}
static void handle_mouse_press(snd_info *sp, chan_info *cp, Float x, Float y, int button, int state) {}
static void handle_mouse_drag(snd_info *sp, chan_info *cp, Float x, Float y) {}
static int handle_key_press(chan_info *cp, int key, int state) {return(0);}
#endif

void g_init_chn(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure(S_temp_filenames,SCM_FNC g_temp_filenames,1,0,0),H_temp_filenames);
  DEFINE_PROC(gh_new_procedure(S_sound_to_temp,SCM_FNC g_sound_to_temp,0,2,0),H_sound_to_temp);
  DEFINE_PROC(gh_new_procedure(S_sound_to_temps,SCM_FNC g_sound_to_temps,0,2,0),H_sound_to_temps);
  DEFINE_PROC(gh_new_procedure(S_selection_to_temp,SCM_FNC g_selection_to_temp,0,2,0),H_selection_to_temp);
  DEFINE_PROC(gh_new_procedure(S_selection_to_temps,SCM_FNC g_selection_to_temps,0,2,0),H_selection_to_temps);
  DEFINE_PROC(gh_new_procedure(S_temp_to_sound,SCM_FNC g_temp_to_sound,3,0,0),H_temp_to_sound);
  DEFINE_PROC(gh_new_procedure(S_temps_to_sound,SCM_FNC g_temps_to_sound,3,0,0),H_temps_to_sound);
  DEFINE_PROC(gh_new_procedure(S_temp_to_selection,SCM_FNC g_temp_to_sound,3,0,0),H_temp_to_sound);
  DEFINE_PROC(gh_new_procedure(S_temps_to_selection,SCM_FNC g_temps_to_sound,3,0,0),H_temps_to_sound);
  DEFINE_PROC(gh_new_procedure(S_scan_chan,SCM_FNC g_scan_chan,1,4,0),H_scan_chan);
  DEFINE_PROC(gh_new_procedure(S_scan_chans,SCM_FNC g_scan_chans,1,2,0),H_scan_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_all_chans,SCM_FNC g_scan_all_chans,1,2,0),H_scan_all_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_sound_chans,SCM_FNC g_scan_sound_chans,1,3,0),H_scan_sound_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_across_chans,SCM_FNC g_scan_across_chans,1,2,0),H_scan_across_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_across_all_chans,SCM_FNC g_scan_across_all_chans,1,2,0),H_scan_across_all_chans);
  DEFINE_PROC(gh_new_procedure(S_scan_across_sound_chans,SCM_FNC g_scan_across_sound_chans,1,3,0),H_scan_across_sound_chans);
  DEFINE_PROC(gh_new_procedure(S_map_chan,SCM_FNC g_map_chan,1,5,0),H_map_chan);
  DEFINE_PROC(gh_new_procedure(S_map_chans,SCM_FNC g_map_chans,1,3,0),H_map_chans);
  DEFINE_PROC(gh_new_procedure(S_map_all_chans,SCM_FNC g_map_all_chans,1,3,0),H_map_all_chans);
  DEFINE_PROC(gh_new_procedure(S_map_sound_chans,SCM_FNC g_map_sound_chans,1,4,0),H_map_sound_chans);
  DEFINE_PROC(gh_new_procedure(S_map_across_chans,SCM_FNC g_map_across_chans,1,3,0),H_map_across_chans);
  DEFINE_PROC(gh_new_procedure(S_map_across_all_chans,SCM_FNC g_map_across_all_chans,1,3,0),H_map_across_all_chans);
  DEFINE_PROC(gh_new_procedure(S_map_across_sound_chans,SCM_FNC g_map_across_sound_chans,1,4,0),H_map_across_sound_chans);
  DEFINE_PROC(gh_new_procedure(S_find,SCM_FNC g_find,1,3,0),H_find);
  DEFINE_PROC(gh_new_procedure(S_count_matches,SCM_FNC g_count_matches,1,3,0),H_count_matches);
  DEFINE_PROC(gh_new_procedure(S_report_in_minibuffer,SCM_FNC g_report_in_minibuffer,1,1,0),H_report_in_minibuffer);
  DEFINE_PROC(gh_new_procedure(S_prompt_in_minibuffer,SCM_FNC g_prompt_in_minibuffer,1,2,0),H_prompt_in_minibuffer);
  DEFINE_PROC(gh_new_procedure(S_append_to_minibuffer,SCM_FNC g_append_to_minibuffer,1,1,0),H_append_to_minibuffer);
  DEFINE_PROC(gh_new_procedure(S_bind_key,SCM_FNC g_bind_key,3,1,0),H_bind_key);
  DEFINE_PROC(gh_new_procedure(S_key,SCM_FNC g_key,2,2,0),H_key);
  DEFINE_PROC(gh_new_procedure(S_save_macros,SCM_FNC g_save_macros,0,0,0),H_save_macros);
  DEFINE_PROC(gh_new_procedure(S_forward_graph,SCM_FNC g_forward_graph,0,3,0),H_forward_graph);
  DEFINE_PROC(gh_new_procedure(S_backward_graph,SCM_FNC g_backward_graph,0,3,0),H_backward_graph);
  DEFINE_PROC(gh_new_procedure(S_edit_position,SCM_FNC g_edit_position,0,2,0),H_edit_position);
  DEFINE_PROC(gh_new_procedure(S_ffting,SCM_FNC g_ffting,0,2,0),H_ffting);
  DEFINE_PROC(gh_new_procedure(S_set_ffting,SCM_FNC g_set_ffting,0,3,0),H_set_ffting);
  DEFINE_PROC(gh_new_procedure(S_waving,SCM_FNC g_waving,0,2,0),H_waving);
  DEFINE_PROC(gh_new_procedure(S_set_waving,SCM_FNC g_set_waving,0,3,0),H_set_waving);
  DEFINE_PROC(gh_new_procedure(S_graphing,SCM_FNC g_graphing,0,2,0),H_graphing);
  DEFINE_PROC(gh_new_procedure(S_set_graphing,SCM_FNC g_set_graphing,0,3,0),H_set_graphing);
  DEFINE_PROC(gh_new_procedure(S_squelch_update,SCM_FNC g_squelch_update,0,2,0),H_squelch_update);
  DEFINE_PROC(gh_new_procedure(S_set_squelch_update,SCM_FNC g_set_squelch_update,0,3,0),H_set_squelch_update);
  DEFINE_PROC(gh_new_procedure(S_edits,SCM_FNC g_edits,0,2,0),H_edits);
  DEFINE_PROC(gh_new_procedure(S_maxamp,SCM_FNC g_maxamp,0,2,0),H_maxamp);
  DEFINE_PROC(gh_new_procedure(S_peaks,SCM_FNC g_peaks,0,3,0),H_peaks);
  DEFINE_PROC(gh_new_procedure(S_x_bounds,SCM_FNC g_x_bounds,0,2,0),H_x_bounds);
  DEFINE_PROC(gh_new_procedure(S_y_bounds,SCM_FNC g_y_bounds,0,2,0),H_y_bounds);
  DEFINE_PROC(gh_new_procedure(S_set_x_bounds,SCM_FNC g_set_x_bounds,2,2,0),H_set_x_bounds);
  DEFINE_PROC(gh_new_procedure(S_set_y_bounds,SCM_FNC g_set_y_bounds,0,4,0),H_set_y_bounds);
  DEFINE_PROC(gh_new_procedure(S_cursor,SCM_FNC g_cursor,0,2,0),H_cursor);
  DEFINE_PROC(gh_new_procedure(S_set_cursor,SCM_FNC g_set_cursor,1,2,0),H_set_cursor);
  DEFINE_PROC(gh_new_procedure(S_cursor_style,SCM_FNC g_cursor_style,0,2,0),H_cursor_style);
  DEFINE_PROC(gh_new_procedure(S_set_cursor_style,SCM_FNC g_set_cursor_style,1,2,0),H_set_cursor_style);
  DEFINE_PROC(gh_new_procedure(S_left_sample,SCM_FNC g_left_sample,0,2,0),H_left_sample);
  DEFINE_PROC(gh_new_procedure(S_set_left_sample,SCM_FNC g_set_left_sample,1,2,0),H_set_left_sample);
  DEFINE_PROC(gh_new_procedure(S_right_sample,SCM_FNC g_right_sample,0,2,0),H_right_sample);
  DEFINE_PROC(gh_new_procedure(S_set_right_sample,SCM_FNC g_set_right_sample,1,2,0),H_set_right_sample);
  DEFINE_PROC(gh_new_procedure(S_edit_hook,SCM_FNC g_edit_hook,0,2,0),H_edit_hook);
  DEFINE_PROC(gh_new_procedure(S_undo_hook,SCM_FNC g_undo_hook,0,2,0),H_undo_hook);
  DEFINE_PROC(gh_new_procedure(S_x_position_slider,SCM_FNC g_ap_sx,0,2,0),H_x_position_slider);
  DEFINE_PROC(gh_new_procedure(S_y_position_slider,SCM_FNC g_ap_sy,0,2,0),H_y_position_slider);
  DEFINE_PROC(gh_new_procedure(S_x_zoom_slider,SCM_FNC g_ap_zx,0,2,0),H_x_zoom_slider);
  DEFINE_PROC(gh_new_procedure(S_y_zoom_slider,SCM_FNC g_ap_zy,0,2,0),H_y_zoom_slider);
  DEFINE_PROC(gh_new_procedure(S_frames,SCM_FNC g_frames,0,2,0),H_frames);
  DEFINE_PROC(gh_new_procedure(S_dot_size,SCM_FNC g_dot_size,0,0,0),H_dot_size);
  DEFINE_PROC(gh_new_procedure(S_set_dot_size,SCM_FNC g_set_dot_size,1,0,0),H_set_dot_size);
  DEFINE_PROC(gh_new_procedure(S_forward_sample,SCM_FNC g_forward_sample,0,3,0),H_forward_sample);
  DEFINE_PROC(gh_new_procedure(S_backward_sample,SCM_FNC g_backward_sample,0,3,0),H_backward_sample);
  DEFINE_PROC(gh_new_procedure(S_smooth,SCM_FNC g_smooth,2,2,0),H_smooth);
  DEFINE_PROC(gh_new_procedure(S_smooth_selection,SCM_FNC g_smooth_selection,0,0,0),H_smooth_selection);
  DEFINE_PROC(gh_new_procedure(S_reverse_sound,SCM_FNC g_reverse_sound,0,2,0),H_reverse_sound);
  DEFINE_PROC(gh_new_procedure(S_reverse_selection,SCM_FNC g_reverse_selection,0,0,0),H_reverse_selection);

  DEFINE_PROC(gh_new_procedure("swap-channels",SCM_FNC g_swap_channels,0,6,0),H_swap_channels);

#if (!HAVE_GUILE_1_3_0)
  fft_hook = scm_create_hook(S_fft_hook,3);                       /* args = sound channel scaler */
  graph_hook = scm_create_hook(S_graph_hook,4);                   /* args = sound channel y0 y1 */
  after_graph_hook = scm_create_hook(S_after_graph_hook,2);       /* args = sound channel */
  mouse_press_hook = scm_create_hook(S_mouse_press_hook,6);       /* args = sound channel button state x y */
  mouse_release_hook = scm_create_hook(S_mouse_release_hook,6);   /* args = sound channel button state x y */
  mouse_drag_hook = scm_create_hook(S_mouse_drag_hook,6);         /* args = sound channel button state x y */
  key_press_hook = scm_create_hook(S_key_press_hook,4);           /* args = sound channel key state */
  mark_click_hook = scm_create_hook(S_mark_click_hook,1);         /* arg = id */
#else
  fft_hook = gh_define(S_fft_hook,SCM_BOOL_F);
  graph_hook = gh_define(S_graph_hook,SCM_BOOL_F);
  after_graph_hook = gh_define(S_after_graph_hook,SCM_BOOL_F);
  mouse_press_hook = gh_define(S_mouse_press_hook,SCM_BOOL_F);
  mouse_release_hook = gh_define(S_mouse_release_hook,SCM_BOOL_F);
  mouse_drag_hook = gh_define(S_mouse_drag_hook,SCM_BOOL_F);
  key_press_hook = gh_define(S_key_press_hook,SCM_BOOL_F);
  mark_click_hook = gh_define(S_mark_click_hook,SCM_BOOL_F);
#endif
}

#endif
