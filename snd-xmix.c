#include "snd.h"
#include <X11/cursorfont.h>


/* ---------------- MIX CURSOR ---------------- */

static void mix_mouse_enter(Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{
  snd_state *ss = (snd_state *)clientData;
  XDefineCursor(XtDisplay(w),XtWindow(w),(ss->sgx)->mix_cursor);
}

static void mix_mouse_leave(Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{
  XUndefineCursor(XtDisplay(w),XtWindow(w));
}


/* ---------------- MIX ICONS ---------------- 
 *
 * bitmaps for play (a speaker), close (an x), open/minify boxes, '?', and so on
 */

#define p_speaker_width 12
#define p_speaker_height 12
static unsigned char p_speaker_bits[] = {
   0x00, 0x07, 0xc0, 0x04, 0x30, 0x04, 0x0e, 0x04, 0x06, 0x04, 0x06, 0x04,
   0x06, 0x04, 0x06, 0x04, 0x0e, 0x04, 0x30, 0x04, 0xc0, 0x04, 0x00, 0x07};

#define p_cross_width 12
#define p_cross_height 12
static unsigned char p_cross_bits[] = {
   0x00, 0x00, 0x02, 0x04, 0x04, 0x02, 0x08, 0x01, 0x90, 0x00, 0x60, 0x00,
   0x60, 0x00, 0x90, 0x00, 0x08, 0x01, 0x04, 0x02, 0x02, 0x04, 0x00, 0x00};

/* an open box (maxify icon -- name "mixer" is out-of-date) */
#define p_mixer_width 12
#define p_mixer_height 12
static unsigned char p_mixer_bits[] = {
   0x00, 0x00, 0xfe, 0x07, 0x02, 0x06, 0x02, 0x06, 0x02, 0x06, 0x02, 0x06,
   0x02, 0x06, 0x02, 0x06, 0x02, 0x06, 0xfe, 0x07, 0xfc, 0x07, 0x00, 0x00};

#define p_mini_width 12
#define p_mini_height 12
static unsigned char p_mini_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x01, 0x90, 0x01,
   0xf0, 0x01, 0xf0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

static Pixmap mixer_r, cross_r, speaker_r, mini_r, playing_r;
static int icons_created = 0;

Pixmap make_pixmap(snd_state *ss, unsigned char *bits, int width, int height, int depth, GC gc)
{
  Pixmap rb,nr;
  state_context *sx;
  sx = ss->sgx;
  rb = XCreateBitmapFromData(sx->mdpy,RootWindowOfScreen(XtScreen(sx->mainpane)),(const char *)bits,width,height);
  nr = XCreatePixmap(sx->mdpy,RootWindowOfScreen(XtScreen(sx->mainpane)),width,height,depth);
  XCopyPlane(sx->mdpy,rb,nr,gc,0,0,width,height,0,0,1);
  XFreePixmap(sx->mdpy,rb);
  return(nr);
}

static void create_icons(Widget w, snd_state *ss)
{
  XGCValues v;
  GC gc;
  int depth;
  icons_created = 1;

  XtVaGetValues(w,XmNforeground,&v.foreground,XmNbackground,&v.background,XmNdepth,&depth,NULL);
  gc = XtGetGC(w,GCForeground | GCBackground,&v);

  mixer_r = make_pixmap(ss,p_mixer_bits,p_mixer_width,p_mixer_height,depth,gc);
  cross_r = make_pixmap(ss,p_cross_bits,p_cross_width,p_cross_height,depth,gc);
  speaker_r = make_pixmap(ss,p_speaker_bits,p_speaker_width,p_speaker_height,depth,gc);
  mini_r = make_pixmap(ss,p_mini_bits,p_mini_width,p_mini_height,depth,gc);

  v.background = (ss->sgx)->pushed_button_color;
  gc = XtGetGC(w,GCForeground | GCBackground,&v);
  playing_r = make_pixmap(ss,p_speaker_bits,p_speaker_width,p_speaker_height,depth,gc);
}



/* ---------------- WIDGET POOL ----------------
 *
 * the widgets used to handle mix-marks are independent of the mix-data structs;
 * off-screen mix data has no associated widget; on-screen mixes have a
 * widget while they remain on screen, and remain unsaved (active);  the 
 * mix-mark widget pool is local to a given channel.  The intent here
 * is to minimize widgets as far as possible.
 *
 * It would be better if these widget pools were global across channels, but
 * that requires either a way to change the main widget's parent widget (i.e.
 * move it to another channel graph's tree), or the use of dialog widgets
 * which means much more difficult placement decisions and display code.
 *
 * The chan_info mix's pointer points to a controlling struct that holds
 * the pointer to the mixer-widget pool and the list of active mixes.
 *
 * Due to unforeseen widget resizing woes, the consoles are also sorted by 
 * in_chans (so that the console need not actually resize itself vertically).
 */

enum {mm_main,           /* form holds console, catches mouse acts (click, drag) and cursor motion */
      mm_fmain,
      mm_title,          /* top row of widgets */
      mm_name,
      mm_beg,
      mm_play,           /* togglebutton, click to play "solo" or to stop */
      mm_close,          /* pushbutton; if clicked, remove mini-console altogether (no undo) */
      mm_open,           /* pushbutton; if just title, open to full console, else close to title row */
      mm_console,        /* holds the rest of the console under the title bar */
      mm_title_sep,      /* if open, horizontal sep below title */
      mm_amp,            /* the "amp:" push button */
      mm_track,          /* track button on right */
      mm_speed,          /* if open, srate control */
      mm_speed_label,
      mm_spdscl,
      mm_spdsep,
      mm_chans           /* if open, start of controls (per input channel) */
};         

#define mm_amp_label 0
#define mm_scl 1
#define MIX_CHAN_SIZE 2

#define title_row_start mm_beg
#define title_row_end mm_open


void release_mixmark_widgets(mixmark *m)
{
  if ((m->w) && ((m->w[mm_main]) && (XtIsManaged(m->w[mm_main])))) 
    {
      XtUnmanageChild(m->w[mm_main]);
      m->active = 0;
      m->playing = 0;
      XtVaSetValues(m->w[mm_play],XmNlabelPixmap,speaker_r,NULL);
    }
}

static void activate_mixmark_widgets(mixmark *m)
{
  if ((m->w) && ((m->w[mm_main]) && (!XtIsManaged(m->w[mm_main])))) 
    {
      XtManageChild(m->w[mm_main]);
      m->active = 1;
    }
}

mix_context *make_mix_context(chan_info *cp)
{
  mix_context *g;
  g = (mix_context *)CALLOC(1,sizeof(mix_context));
  g->graph = channel_graph(cp);
  return(g);
}

mix_context *set_mixdata_context(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if (sp->combining != CHANNELS_SEPARATE) cp=sp->chans[0];
  return(make_mix_context(cp));
}

void select_mix(snd_state *ss, mixdata *md)
{
  mixmark *m;
  mixdata *old_md;
  if (!(md)) return;
  if ((ss->selected_mix != NO_SELECTION) && (ss->selected_mix != md->id))
    {
      old_md = md_from_int(ss->selected_mix);
      if (old_md)
	{
	  m = old_md->mixer;
	  if (m)
	    {
	      XtVaSetValues(m->w[mm_name],XmNbackground,(old_md->wg->color) ? (old_md->wg->color) : ((ss->sgx)->mix_color),NULL);
	      XtVaSetValues(m->w[mm_title],XmNbackground,(old_md->wg->color) ? (old_md->wg->color) : ((ss->sgx)->mix_color),NULL);
	    }
	}
    }
  ss->selected_mix = md->id;
  m = md->mixer;
  if (m) 
    {
      XtVaSetValues(m->w[mm_name],XmNbackground,(ss->sgx)->mix_focus_color,NULL);
      XtVaSetValues(m->w[mm_title],XmNbackground,(ss->sgx)->mix_focus_color,NULL);
    }
}

void color_selected_mix(snd_state *ss)
{
  mixmark *m;
  mixdata *md;
  if (ss->selected_mix != NO_SELECTION)
    {
      md = md_from_int(ss->selected_mix);
      if (md)
	{
	  m = md->mixer;
	  if (m) 
	    {
	      if (m->playing)
		XtVaSetValues(m->w[mm_play],XmNlabelPixmap,playing_r,XmNbackground,(ss->sgx)->pushed_button_color,NULL);
	      else XtVaSetValues(m->w[mm_play],XmNlabelPixmap,speaker_r,XmNbackground,(ss->sgx)->mix_color,NULL);
	      XtVaSetValues(m->w[mm_name],XmNbackground,(ss->sgx)->mix_focus_color,NULL);
	      XtVaSetValues(m->w[mm_title],XmNbackground,(ss->sgx)->mix_focus_color,NULL);
	    }
	}
    }
}

int color_mix(mixdata *md, void *ptr)
{
  mixmark *m;
  snd_state *ss = (snd_state *)ptr;
  if (md)
    {
      m = md->mixer;
      if (m)
	{
	  if (m->playing)
	    XtVaSetValues(m->w[mm_play],XmNlabelPixmap,playing_r,XmNbackground,(ss->sgx)->pushed_button_color,NULL);
	  else XtVaSetValues(m->w[mm_play],XmNlabelPixmap,speaker_r,XmNbackground,(ss->sgx)->mix_color,NULL);
	  if (ss->selected_mix == md->id)
	    {
	      XtVaSetValues(m->w[mm_name],XmNbackground,(ss->sgx)->mix_focus_color,NULL);
	      XtVaSetValues(m->w[mm_title],XmNbackground,(ss->sgx)->mix_focus_color,NULL);
	    }
	  else
	    {
	      XtVaSetValues(m->w[mm_name],XmNbackground,(md->wg->color) ? (Pixel)(md->wg->color) : ((ss->sgx)->mix_color),NULL);
	      XtVaSetValues(m->w[mm_title],XmNbackground,(md->wg->color) ? (Pixel)(md->wg->color) : ((ss->sgx)->mix_color),NULL);
	    }
	}
    }
  return(0);
}

void color_one_mix(mixdata *md, Pixel color)
{
  mixmark *m;
  if ((md) && (md->mixer))
    {
      m = md->mixer;
      XtVaSetValues(m->w[mm_name],XmNbackground,color,NULL);
      XtVaSetValues(m->w[mm_title],XmNbackground,color,NULL);
      md->wg->color = (int)color;
    }
}

void color_unselected_mixes(snd_state *ss)
{
  map_over_mixes(color_mix,(void *)ss);
}



/* ---------------- MIX CONSOLE CALLBACKS ---------------- */

void reflect_mix_stop_playing(snd_state *ss, mixmark *m)
{
  m->playing = 0;
  XtVaSetValues(m->w[mm_play],XmNlabelPixmap,speaker_r,XmNbackground,(ss->sgx)->mix_color,NULL);
}

static void mix_console_play_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  mixmark *m = (mixmark *)clientData;
  mixdata *md;
  snd_state *ss;
  md = (mixdata *)(m->owner);
  ss = md->ss;
  if (m->playing) 
    reflect_mix_stop_playing(ss,m);
  else
    {
      m->playing = 1;
      XtVaSetValues(m->w[mm_play],XmNlabelPixmap,playing_r,XmNbackground,(ss->sgx)->pushed_button_color,NULL);
      play_mix(ss,md);
    }
}

static void mix_console_close_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  console_state *cs;
  mixmark *m = (mixmark *)clientData;
  mixdata *md;
  md = (mixdata *)(m->owner);
  cs = md->states[md->curcons];
  cs->locked = 1;
  cs = md->current_cs;
  cs->locked = 1;
  display_channel_mixes(md->cp);
}

static int current_mix_x = 0;

/* basically copied from snd-xsnd snd_amp_changed with different ranges since the widgets are smaller in this case */
static char ampbuf[5]={'1',STR_decimal,'0','0','\0'};
static char srcbuf[5]={'1',STR_decimal,'0','0','\0'};

static Float mix_console_amp_scaler = 1.0;
static Float mix_console_speed_scaler = 1.0;
void set_mix_console_amp_scaler(Float amp) {mix_console_amp_scaler = amp;}
void set_mix_console_speed_scaler(Float amp) {mix_console_speed_scaler = amp;}

Float get_mix_console_amp_scaler(void) {return(mix_console_amp_scaler);}
Float get_mix_console_speed_scaler(void) {return(mix_console_speed_scaler);}


static Float amp_int_to_Float(int val) {if (val == 0) return(0.0); else return(exp((Float)((val-50)*mix_console_amp_scaler)/20.0));}
static int amp_Float_to_int(Float scl_amp) {if (scl_amp == 0.0) return(0); else return((int)(50.5 + 20.0*log(scl_amp)/mix_console_amp_scaler));}

static Float speed_int_to_Float(int val) {return(exp((Float)(val-50)*mix_console_speed_scaler/20.0));}
static int speed_Float_to_int(Float spd) {return((int)(50.5+20*log(spd)/mix_console_speed_scaler));}

static Float change_amp_label(Widget w, Float amp)
{
  char *sfs;
  sfs=prettyf(amp,2);
  fill_number(sfs,ampbuf);
  FREE(sfs);
  set_button_label(w,ampbuf);
  return(amp);
}

static void reflect_mix_amp(Widget scl, Widget lab, Float amp)
{
  change_amp_label(lab,amp);
  XmScaleSetValue(scl,amp_Float_to_int(amp));
}

static Float change_speed_label(Widget w, snd_state *ss, Float true_speed)
{
  /* return quantized speed (cs->speed normally) */
  Float spd;
  spd = srate_changed(true_speed,srcbuf,speed_style(ss),speed_tones(ss)); 
  set_button_label(w,srcbuf);
  return(spd);
}

static Float reflect_mix_speed(Widget scl, Widget lab, snd_state *ss, Float true_speed, int scl_spd)
{
  /* true_speed as above, scl_spd = int version of scl_speed (old_speed normally) */
  XmScaleSetValue(scl,scl_spd);
  return(change_speed_label(lab,ss,true_speed));
}

void set_mix_title_beg(mixdata *md, mixmark *m)
{
  console_state *cs;
  char *str;
  chan_info *cp;
  if (m->state != MD_M)
    {
      cs = md->current_cs;
      cp = md->cp;
      str = (char *)CALLOC(32,sizeof(char));
      if (md->beg_in_samps)
	sprintf(str,"%d : %d",cs->beg,cs->beg+cs->len);
      else sprintf(str,"%.3f : %.3f",(Float)(cs->beg)/SND_SRATE(cp->sound),(Float)(cs->beg+cs->len)/SND_SRATE(cp->sound));
      set_button_label_normal(m->w[mm_beg],str);
      FREE(str);
    }
}

static void beg_click_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
   mixmark *m = (mixmark *)clientData;
   mixdata *md;
   md = (mixdata *)(m->owner);
   md->beg_in_samps = (!(md->beg_in_samps));
   set_mix_title_beg(md,m);
}

static void amp_click_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* 1 click: set to 1 and 0's ("identity"), 2 click: mute (all 0's) */
  /* cntrl-click once to reset to previous saved state */
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)callData;
  mixmark *m = (mixmark *)clientData;
  mixdata *md;
  int chan;
  console_state *cs;
  chan_info *cp;
  int i,j;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  md = (mixdata *)(m->owner);
  cp = md->cp;
  chan = cp->chan;
  cs = md->current_cs;
  if (cb->click_count == 1)
    {
      for (i=0,j=mm_chans;i<md->in_chans;i++,j+=MIX_CHAN_SIZE) 
	{
	  if (ev->state & (snd_ControlMask | snd_MetaMask))
	    cs->scalers[i] = cs->old_scalers[i];
	  else 
	    {
	      if (i != md->main_chan) 
		cs->scalers[i] = 0.0; 
	      else cs->scalers[i] = 1.0;
	    }
	  reflect_mix_amp(m->w[j+mm_scl],m->w[j+mm_amp_label],cs->scalers[i]);
	}
    }
  else
    {
      if (cb->click_count == 2)
	{
	  /* can't get here without going through click==1, so just reset chan from callData */
	  if (md->main_chan >= 0) 
	    {
	      cs->scalers[md->main_chan] = 0.0;
	      reflect_mix_amp(m->w[mm_chans+mm_scl+md->main_chan*MIX_CHAN_SIZE],m->w[mm_chans+mm_amp_label+md->main_chan*MIX_CHAN_SIZE],0.0);
	    }
	}
    }
  select_channel(cp->sound,chan);
  if (!(call_mix_amp_changed_hook(md)))
    remix_file(md,"Mix: amp click");
}

void reamp(mixdata *md, int chan, Float amp)
{
  console_state *cs;
  mixmark *m;
  if (chan >= md->in_chans) return;
  cs = md->current_cs;
  m = md->mixer;
  if (chan >= cs->chans) return;
  cs->scalers[chan] = amp;
  if (m) reflect_mix_amp(m->w[mm_chans+mm_scl+chan*MIX_CHAN_SIZE],m->w[mm_chans+mm_amp_label+chan*MIX_CHAN_SIZE],amp);
}

void respeed(mixdata *md, Float spd)
{ /* used in set-mix-speed */
  console_state *cs;
  mixmark *m;
  snd_state *ss;
  ss = md->ss;
  cs = md->current_cs;
  m = md->mixer;
  cs->scl_speed = spd;
  cs->old_speed = speed_Float_to_int(spd); 
  if (m)
    cs->speed = reflect_mix_speed(m->w[mm_spdscl],m->w[mm_speed_label],md->ss,cs->scl_speed,cs->old_speed);
  else cs->speed = srate_changed(spd,srcbuf,speed_style(ss),speed_tones(ss)); 
  cs->len = (int)(ceil(md->in_samps / cs->speed));
  if (m) set_mix_title_beg(md,m);
}

static void speed_click_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* set speed to 1 (control-click for reset to previous?) */
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)callData;
  mixmark *m = (mixmark *)clientData;
  mixdata *md;
  console_state *cs;
  XButtonEvent *ev;
  int val;
  chan_info *cp;
  ev = (XButtonEvent *)(cb->event);
  md = (mixdata *)(m->owner);
  cs = md->current_cs;
  if (ev->state & (snd_ControlMask | snd_MetaMask))
    {
      cs->scl_speed = speed_int_to_Float(cs->old_speed);
      val = cs->old_speed;
    }
  else
    {
      cs->scl_speed = 1.0;
      val = 50;
    }
  cs->speed = reflect_mix_speed(m->w[mm_spdscl],m->w[mm_speed_label],md->ss,cs->scl_speed,val);
  cs->len = (int)(ceil(md->in_samps / cs->speed));
  set_mix_title_beg(md,m);
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if (!(call_mix_speed_changed_hook(md)))
    remix_file(md,"Mix: speed click");
}

static void m_amp_drag_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* amp slider drag */
  console_state *cs;
  chan_info *cp;
  mixdata *md;
  mix_context *ms;
  int chan;
  mixmark *m = (mixmark *)clientData;
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)callData;
  md = (mixdata *)(m->owner);
  ms = md->wg;
  cp = md->cp;
  /* select_channel(cp->sound,cp->chan); */
  XtVaGetValues(w,XmNuserData,&chan,NULL);
  if (md->in_chans > 1)
    {
      XtVaSetValues(m->w[mm_chans + mm_amp_label + (md->selected_chan)*MIX_CHAN_SIZE],XmNforeground,((md->ss)->sgx)->black,NULL);
      XtVaSetValues(m->w[mm_chans + mm_amp_label + chan*MIX_CHAN_SIZE],XmNforeground,((md->ss)->sgx)->red,NULL);
    }
  md->selected_chan = chan;
  cs = md->current_cs;
  cs->scalers[chan] = amp_int_to_Float(cb->value);
  change_amp_label(m->w[mm_chans + mm_amp_label + chan*MIX_CHAN_SIZE],cs->scalers[chan]);
  if (!(ms->lastpj)) {ms->lastpj = make_graph(cp,cp->sound,cp->state); mix_save_graph(md->ss,md->wg,ms->lastpj);}
  make_temporary_graph(cp,md,cs);
}

static void m_amp_value_changed_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* amp slider button release */
  int chan;
  mixdata *md;
  mix_context *ms;
  console_state *cs;
  chan_info *cp;
  mixmark *m = (mixmark *)clientData;
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)callData;
  md = (mixdata *)(m->owner);
  ms = md->wg;
  ms->lastpj = 0;
  XtVaGetValues(w,XmNuserData,&chan,NULL);
  cs = md->current_cs;
  if (md->in_chans > 1)
    {
      XtVaSetValues(m->w[mm_chans + mm_amp_label + (md->selected_chan)*MIX_CHAN_SIZE],XmNforeground,((md->ss)->sgx)->black,NULL);
      XtVaSetValues(m->w[mm_chans + mm_amp_label + chan*MIX_CHAN_SIZE],XmNforeground,((md->ss)->sgx)->red,NULL);
    }
  md->selected_chan = chan;
  cs->scalers[chan] = amp_int_to_Float(cb->value);
  cs->old_scalers[chan] = cs->scalers[chan];
  change_amp_label(m->w[mm_chans + mm_amp_label + chan*MIX_CHAN_SIZE],cs->scalers[chan]);
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if (!(call_mix_amp_changed_hook(md)))
    remix_file(md,"Mix: amp");
}

static void m_speed_drag_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* speed slider drag */
  console_state *cs;
  chan_info *cp;
  mixdata *md;
  mix_context *ms;
  int chan;
  snd_state *ss;
  mixmark *m = (mixmark *)clientData;
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)callData;
  md = (mixdata *)(m->owner);
  ms = md->wg;
  XtVaGetValues(w,XmNuserData,&chan,NULL);
  cs = md->current_cs;
  cp = md->cp;
  ss = md->ss;
  cs->scl_speed = speed_int_to_Float(cb->value);
  cs ->speed = change_speed_label(m->w[mm_speed_label],md->ss,cs->scl_speed);
  if (!(ms->lastpj)) {ms->lastpj = make_graph(cp,cp->sound,cp->state); mix_save_graph(md->ss,md->wg,ms->lastpj);}
  if (show_mix_waveforms(ss)) erase_mix_waveform(md,m->y);      
  cs->len = (int)(ceil(md->in_samps / cs->speed));
  make_temporary_graph(cp,md,cs);
  set_mix_title_beg(md,m);
  if (show_mix_waveforms(ss)) draw_mix_waveform(md,m->y);      
}

static void m_speed_value_changed_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* speed slider button release */
  int chan;
  mixdata *md;
  mix_context *ms;
  console_state *cs;
  chan_info *cp;
  mixmark *m = (mixmark *)clientData;
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)callData;
  md = (mixdata *)(m->owner);
  ms = md->wg;
  ms->lastpj = 0;
  XtVaGetValues(w,XmNuserData,&chan,NULL);
  cs = md->current_cs;
  cs->scl_speed = speed_int_to_Float(cb->value);
  cs ->speed = change_speed_label(m->w[mm_speed_label],md->ss,cs->scl_speed);
  cs->old_speed = cb->value;
  cs->len = (int)(ceil(md->in_samps / cs->speed));
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if (!(call_mix_speed_changed_hook(md)))
    remix_file(md,"Mix: speed");
}

/* manage the mix console (4 states: undisplayed, name[0], title row, title+scalers) */

static void set_minimal_title(mixdata *md, mixmark *m)
{
  char buf[2];
  buf[0] = md->name[0];
  buf[1] = '\0';
  set_button_label_normal(m->w[mm_name],buf);
}

static void set_mix_title_name(mixdata *md, mixmark *m)
{
  set_button_label_normal(m->w[mm_name],md->name);
}

void reflect_mix_name(mixdata *md)
{
  mixmark *m;
  m = md->mixer;
  if (m) set_mix_title_name(md,m);
}

static void set_console(mixdata *md, mixmark *m)
{
  console_state *cs;
  int i,j;
  snd_state *ss;
  ss = md->ss;
  cs = md->current_cs;
  for (i=0,j=mm_chans;i<md->in_chans;i++,j+=MIX_CHAN_SIZE)
    reflect_mix_amp(m->w[j+mm_scl],m->w[j+mm_amp_label],cs->scalers[i]);
  reflect_mix_speed(m->w[mm_spdscl],m->w[mm_speed_label],ss,cs->scl_speed,cs->old_speed);
}

static void open_console(mixmark *m)
{
  XtVaSetValues(m->w[mm_open],XmNlabelPixmap,mini_r,NULL);
  XtManageChild(m->w[mm_console]);
}

static void close_console(mixmark *m)
{
  XtUnmanageChild(m->w[mm_console]);
  XtVaSetValues(m->w[mm_open],XmNlabelPixmap,mixer_r,NULL);
}

static void open_title(mixmark *m)
{
  int i;
  for (i=title_row_start;i<=title_row_end;i++) XtManageChild(m->w[i]);
}

static void close_title(mixmark *m)
{
  int i;
  for (i=title_row_start;i<=title_row_end;i++) XtUnmanageChild(m->w[i]);
}

static void title_to_m(mixdata *md, mixmark *m)
{
  close_title(m);
  set_minimal_title(md,m);
}

static void m_to_title(mixdata *md, mixmark *m)
{
  set_mix_title_name(md,m);
  set_mix_title_beg(md,m);
  open_title(m);
}

static void console_to_m(mixdata *md, mixmark *m)
{
  close_console(m);
  close_title(m);
  set_minimal_title(md,m);
}

static void m_to_console(mixdata *md, mixmark *m)
{
  set_mix_title_name(md,m);
  set_mix_title_beg(md,m);
  open_title(m);
  set_console(md,m);
  open_console(m);
}

static void console_to_title(mixdata *md, mixmark *m)
{
  close_console(m);
  set_mix_title_name(md,m);
  set_mix_title_beg(md,m);
}

static void title_to_console(mixdata *md, mixmark *m)
{
  set_mix_title_name(md,m);
  set_mix_title_beg(md,m);
  set_console(md,m);
  open_console(m);
}

static void console_to_console(mixdata *md, mixmark *m)
{
  set_mix_title_name(md,m);
  set_mix_title_beg(md,m);
  set_console(md,m);
}

static void title_to_title(mixdata *md, mixmark *m)
{
  set_mix_title_name(md,m);
  set_mix_title_beg(md,m);
}

void fixup_mixmark(mixdata *md)
{
  mixmark *m;
  m = md->mixer;
  switch (md->state)
    {
    case MD_M:
      switch (m->state)
	{
	case MD_M: break;
	case MD_TITLE: title_to_m(md,m); break;
	case MD_CS: console_to_m(md,m); break;
	}
      break;
    case MD_TITLE:
      switch (m->state)
	{
	case MD_M: m_to_title(md,m); break;
	case MD_TITLE: title_to_title(md,m); break;
	case MD_CS: console_to_title(md,m); break;
	}
      break;
    case MD_CS:
      switch (m->state)
	{
	case MD_M: m_to_console(md,m); break;
	case MD_TITLE: title_to_console(md,m); break;
	case MD_CS: console_to_console(md,m); break;
	}
      break;
    }
  m->state = md->state;
  if (m->active == 0) activate_mixmark_widgets(m);
  md->width = widget_width(m->w[mm_main]);
}

static void mix_console_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  click_for_mix_console_help((mixmark *)clientData);
}

static void mix_console_open_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  mixmark *m = (mixmark *)clientData;
  mixdata *md;
  chan_info *cp;
  md = (mixdata *)(m->owner);
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if (md->state == MD_CS) md->state = MD_TITLE; else md->state = MD_CS;
  fixup_mixmark(md);
  call_mix_console_state_changed_hook(md);
}

static int mix_dragged = 0;                /* are we dragging the mouse while inside a console */
static Time mix_down_time;                 /* click vs drag check */
static XtWorkProcId watch_mix_proc = 0;    /* work proc if mouse outside graph causing axes to move */
static int last_mix_x = 0;                 /* mouse position within console title bar (console coordinates) */

/* for axis movement (as in mark drag off screen) */
static int move_mix(mixmark *m, int evx);

int mix_dragging(void) {return(mix_dragged);} /* snd-xchn.c */

static BACKGROUND_TYPE watch_mix(XtPointer m)
{
  /* there is apparently a race condition here or something that leaves this work proc
   * running even after XtRemoveWorkProc has been called on it, so we need to
   * check watch_mix_proc and return true if it's 0.
   */
  if (watch_mix_proc)
    {
      move_mix((mixmark *)m,current_mix_x);
      return(BACKGROUND_CONTINUE);
    }
  else return(BACKGROUND_QUIT);
}

static int xoff; /* yoff */

static void mix_title_button_press(Widget w, XtPointer clientData, XEvent *event, Boolean *flag) 
{
  mixmark *m = (mixmark *)clientData;
  XButtonEvent *ev = (XButtonEvent *)event;
  /* Window wn; */
  chan_info *cp;
  snd_state *ss;
  cp = m_to_cp(m);
  ss = cp->state;
  if (!(ss->using_schemes)) XtVaSetValues(m->w[mm_name],XmNforeground,(ss->sgx)->red,NULL);
  mix_dragged = 0;
  mix_down_time = ev->time;
#if 0
  XTranslateCoordinates(XtDisplay(w),XtWindow(m->w[mm_main]),DefaultRootWindow(XtDisplay(w)),0,0,&xoff,&yoff,&wn);
  last_mix_x = ev->x_root - xoff;
  XTranslateCoordinates(XtDisplay(w),XtWindow(channel_graph(m_to_cp(m))),DefaultRootWindow(XtDisplay(w)),0,0,&xoff,&yoff,&wn);
#else
  xoff = widget_x(m->w[mm_main]);
  last_mix_x =  (int)(ev->x_root);
#endif
  select_channel(cp->sound,cp->chan);
  select_mix(ss,(mixdata *)(m->owner));
}

/* does not currently extend the base file as we push off the right (or left???) edge */
/* also what about dragging in the y direction? */


static int move_mix(mixmark *m, int evx)
{
  snd_state *ss;
  axis_info *ap;
  chan_info *cp;
  mixdata *md;
  console_state *cs;
  int samps,nx,x,samp,kx,updated=0,old_beg;
  Position xx;
  Dimension len;
  Widget w;
  cp = m_to_cp(m);
  if (!cp) return(0);
  ap = cp->axis;
  if (!ap) return(0);
  md = (mixdata *)(m->owner);
  if (!md) return(0);
  ss = md->ss;
#if 0
  /* evx here is in root window coordinates */
  x = evx - last_mix_x - xoff; /* console left edge relative to graph */
#else
  x = evx - last_mix_x + xoff;
#endif
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (watch_mix_proc)
	{
	  if ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) return(0);
	  if ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) return(0);
	}
      nx = move_axis(cp,ap,x); /* calls update_graph eventually (in snd-chn.c reset_x_display) */
      updated = 1;
      if ((mix_dragged) && (!watch_mix_proc))
	watch_mix_proc = XtAppAddWorkProc((ss->sgx)->mainapp,watch_mix,(XtPointer)m);
    }
  else 
    {
      nx = x;
      if (watch_mix_proc) 
	{
	  XtRemoveWorkProc(watch_mix_proc);
	  watch_mix_proc = 0;
	}
    }
  cs = md->current_cs;
  old_beg = cs->beg;
  if (m->x != nx)
    {
      if (show_mix_waveforms(ss)) erase_mix_waveform(md,m->y);
      kx = m->x;
      m->x = nx;
      w = m->w[mm_main];
      XtVaSetValues(w,XmNx,(Position)(m->x),NULL);
      /* if widget refuses to move, reset notion of last_mix_x instead */
      /* this happens when we hit the graph borders with one or the other end of the title bar */
      XtVaGetValues(w,XmNx,&xx,XmNwidth,&len,NULL);
      if (xx != nx) 
	{
	  /* if we've moved off the right side, check for moving the axis */
	  m->x = xx;
	  if ((int)(nx+len) >= (int)(ap->x_axis_x1))
	    {
	      nx = move_axis(cp,ap,nx+len);
	      if (!watch_mix_proc)
		watch_mix_proc = XtAppAddWorkProc((ss->sgx)->mainapp,watch_mix,(XtPointer)m);
	    }
	  else if ((xx == kx) && (!updated)) return(0);
	}
      samp = (int)(ungrf_x(ap,m->x) * SND_SRATE(cp->sound));
      if (samp < 0) samp = 0;
      samps = current_ed_samples(cp);
      if (samp > samps) samp = samps;
      /* now redraw the mix and reset its notion of begin time */
      /* actually should make a new state if cp->edit_ctr has changed ?? */
      cs->beg = samp - md->anchor;
      if (cs->beg < 0) {cs->beg = 0; md->anchor = samp;}
      if (show_mix_waveforms(ss)) draw_mix_waveform(md,m->y);

      /* can't easily use work proc here because the erasure gets complicated */
      make_temporary_graph(cp,md,cs);
      set_mix_title_beg(md,m);
      return(cs->beg - old_beg);
    }
  else
    {
      if (updated) 
	{
	  cs = md->current_cs;
	  make_temporary_graph(cp,md,cs);
	  return(cs->beg - old_beg);
	}
    }
  return(0);
}

static void mix_title_button_release(Widget w, XtPointer clientData, XEvent *event, Boolean *flag) 
{
  mixmark *m = (mixmark *)clientData;
  console_state *cs;
  mixdata *md;
  mix_context *ms;
  snd_state *ss; 
  chan_info *cp;
  char *buf;
  int samps_moved=0;
  m->moving = 0;
  cp = m_to_cp(m);
  ss = cp->state;
  if (!(ss->using_schemes)) XtVaSetValues(m->w[mm_name],XmNforeground,(ss->sgx)->black,NULL);
  md = (mixdata *)(m->owner);
  cs = md->current_cs;
  if (mix_dragged)
    {
      if (watch_mix_proc) 
	{
	  XtRemoveWorkProc(watch_mix_proc);
	  watch_mix_proc = 0;
	}
      mix_dragged = 0;
      /* now finalize the current state of the mix as an edit */
      ms = md->wg;
      ms->lastpj = 0;
      if (cs->beg == cs->orig) return;
      samps_moved = cs->beg - cs->orig;
      if (!(call_mix_position_changed_hook(md,samps_moved)))
	remix_file(md,"Mix: drag");
    }
  else
    {
      raise_widget(m->w[mm_main]); /* bring to top of possibly stacked console tree */
      buf = (char *)CALLOC(16,sizeof(char));
      sprintf(buf,"mix %d",md->id);
      report_in_minibuffer(cp->sound,buf);
      FREE(buf);
    }
}

static void mix_console_name_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* called upon button release in name widget */
  mixmark *m = (mixmark *)clientData;
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)callData;
  mixdata *md;
  chan_info *cp;
  md = (mixdata *)(m->owner);
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if ((cb->reason == XmCR_ACTIVATE) && (cb->click_count == 2))
    {
      /* if we're completely closed, open the title bar, else collapse to a single char */
      if (md->state == MD_M) md->state = MD_TITLE; else md->state = MD_M;
      fixup_mixmark(md);
      call_mix_console_state_changed_hook(md);
    }
}
  
void set_mix_track_button_color(mixdata *md, int track)
{
  Widget w;
  snd_state *ss;
  mixmark *m;
  ss = md->ss;
  m = md->mixer;
  if (m)
    {
      w = m->w[mm_track];
      switch (track)
	{
	case 1: case 0: XtVaSetValues(w,XmNselectColor,(ss->sgx)->pushed_button_color,NULL); break;
	case 2: XtVaSetValues(w,XmNselectColor,(ss->sgx)->green,NULL); break;
	case 3: XtVaSetValues(w,XmNselectColor,(ss->sgx)->yellow,NULL); break;
	case 4: XtVaSetValues(w,XmNselectColor,(ss->sgx)->red,NULL); break;
	default: XtVaSetValues(w,XmNselectColor,(ss->sgx)->black,NULL); break;
	}
      XmToggleButtonSetState(w,(track == 0) ? FALSE : TRUE,FALSE);
    }
}

static void mix_track_button_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  mixmark *m = (mixmark *)clientData;
  int track;
  mixdata *md;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);
  if (cb->set)
    if (ev->state & snd_ControlMask) 
      if (ev->state & snd_MetaMask)
	if (ev->state & snd_ShiftMask)
	  track = 4;
	else track = 3;
      else track = 2;
    else track = 1;
  else track = 0;
  md = (mixdata *)(m->owner);
  md->track = track;
  set_mix_track_button_color(md,track);
}

static void mix_title_button_motion(Widget w, XtPointer clientData, XEvent *event, Boolean *flag) 
{
  Time mix_time;
  chan_info *cp;
  mixdata *md;
  mixmark *m = (mixmark *)clientData;
  XMotionEvent *ev = (XMotionEvent *)event;
  /* this needs to be a little slow about deciding that we are dragging, as opposed to a slow click */
  mix_time = ev->time;
  if ((mix_time - mix_down_time) < (0.5 * XtGetMultiClickTime(XtDisplay(w)))) return;
  cp = m_to_cp(m);
  if (!mix_dragged) 
    {
      md = (mixdata *)(m->owner);
      mix_save_graph(md->ss,md->wg,make_graph(cp,cp->sound,cp->state));
    }
  mix_dragged = 1;
  current_mix_x = ev->x_root;
  m->moving = 1;
  move_mix(m,ev->x_root);
  md = (mixdata *)(m->owner);
  set_mix_title_beg(md,m);
}

void move_mixmark(mixmark *m, int x, int y)
{
  Position xx,yy;
  Dimension wid;
  XtVaSetValues(m->w[mm_main],XmNx,(Position)x,XmNy,(Position)y,NULL);
  XtVaGetValues(m->w[mm_main],XmNx,&xx,XmNy,&yy,XmNwidth,&wid,NULL);
  m->x = xx;
  m->y = yy;
  if (!(m->active)) activate_mixmark_widgets(m);
  if (wid < 15)
    {
      open_console(m); /* try to force it to pop into existence */
      close_console(m); 
    }
}

void move_mix_y(mixmark *m, int yspot)
{
  Position xx;
  XtVaSetValues(m->w[mm_main],XmNy,(Position)yspot,NULL);
  XtVaGetValues(m->w[mm_main],XmNy,&xx,NULL);
  m->y = xx;
}

void move_mix_x(mixmark *m, int xspot)
{
  Position xx;
  XtVaSetValues(m->w[mm_main],XmNx,(Position)xspot,NULL);
  XtVaGetValues(m->w[mm_main],XmNx,&xx,NULL);
  m->x = xx;
}


static void add_key_press(Widget w, void *ptr)
{
  if (XtIsManaged(w))
    XtAddEventHandler(w,KeyPressMask,FALSE,graph_key_press,(XtPointer)ptr);
}

static void mousify(mixmark *m)
{
  snd_info *sp;
  chan_info *cp;

  cp = m_to_cp(m);
  sp = cp->sound;

  XtAddEventHandler(m->w[mm_main],EnterWindowMask,FALSE,mix_mouse_enter,(XtPointer)(sp->state));
  XtAddEventHandler(m->w[mm_main],LeaveWindowMask,FALSE,mix_mouse_leave,(XtPointer)NULL);

  XtAddEventHandler(m->w[mm_title],ButtonPressMask,FALSE,mix_title_button_press,(XtPointer)m);
  XtAddEventHandler(m->w[mm_title],ButtonMotionMask,FALSE,mix_title_button_motion,(XtPointer)m);
  XtAddEventHandler(m->w[mm_title],ButtonReleaseMask,FALSE,mix_title_button_release,(XtPointer)m);

  XtAddEventHandler(m->w[mm_name],ButtonPressMask,FALSE,mix_title_button_press,(XtPointer)m);
  XtAddEventHandler(m->w[mm_name],ButtonMotionMask,FALSE,mix_title_button_motion,(XtPointer)m);
  XtAddEventHandler(m->w[mm_name],ButtonReleaseMask,FALSE,mix_title_button_release,(XtPointer)m);

  XtAddEventHandler(m->w[mm_console],ButtonPressMask,FALSE,mix_title_button_press,(XtPointer)m);
  XtAddEventHandler(m->w[mm_console],ButtonMotionMask,FALSE,mix_title_button_motion,(XtPointer)m);
  XtAddEventHandler(m->w[mm_console],ButtonReleaseMask,FALSE,mix_title_button_release,(XtPointer)m);
  
  /* now everyone has to respond correctly to keypress (problematic apparently because traversal is off) */
  map_over_children(m->w[mm_main],add_key_press,(void *)sp);
}


#define TRACK_BUTTON_SIZE 13

static void create_mixer(mixdata *md, int x, int y)
{
  /* make a new mixer console */
  /* this is made complicated by the fact that, to quote the XmManager man page:
   *
   *   "In general a widget can receive keyboard focus when it is a primitive, a
   *    gadget, or a manager (such as a DrawingArea with no traversable children) 
   *    that acts as a primitive."
   *
   * which severely limits what we can do in the mix console, since it is a child
   * of the channel's graphics drawingarea widget.  We need to retain that structure
   * so that mix position correlates with waveform position. But this means we
   * can't use manager widgets to handle the console (no scrolling window, which
   * is what I wanted originally), and (sigh!) no text fields (for envelope definition).
   * I had some very fancy plans...
   */
  snd_state *ss;
  mixmark *m;
  mix_context *ms;
  int n,i,old_squelch=0;
  Dimension wid;
  Arg args[32];
  XmString s1;
  Widget last_widget,amp_widget;

  if (md->cp) 
    {
      old_squelch = (md->cp)->squelch_update;
      (md->cp)->squelch_update = 1;
    }
  /* some Motifs feel the need to resize the channel graph during this process,
   *   but that can get us caught with a mixer that wants to be displayed, but
   *   which has not finished allocating its widgets -> segfault.
   */
#ifdef DEBUGGING
  else snd_error("create mixer without a channel pointer?");
#endif
  ss = md->ss;
  ms = md->wg;
  m = md->mixer;

  m->w = (Widget *)CALLOC(mm_chans+(md->in_chans*MIX_CHAN_SIZE),sizeof(Widget));
  m->chans_allocated = md->in_chans;
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->mix_color); n++;}
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XmNx,x); n++;
  XtSetArg(args[n],XmNy,y); n++;
  XtSetArg(args[n],XmNshadowType,XmSHADOW_OUT); n++;
  XtSetArg(args[n],XmNshadowThickness,1); n++;
  m->w[mm_main] = sndCreateFrameWidget("mm_main",ms->graph,args,n);
  XtAddCallback(m->w[mm_main],XmNhelpCallback,mix_console_help_callback,m);
  m->x = x;
  m->y = y;

  n=0;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  m->w[mm_fmain] = sndCreateFormWidget("mm_main",m->w[mm_main],args,n);
  XtAddCallback(m->w[mm_fmain],XmNhelpCallback,mix_console_help_callback,m);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->mix_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  m->w[mm_title] = sndCreateRowColumnWidget("mm_title",m->w[mm_fmain],args,n);

  n=0;
  s1=XmStringCreate(md->name,"button_font");
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->mix_color); n++;}
  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNlabelString,s1); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XmNshadowThickness,0); n++;
  XtSetArg(args[n],XmNhighlightThickness,0); n++;
  XtSetArg(args[n],XmNfillOnArm,FALSE); n++;
  XtSetArg(args[n],XmNmultiClick,XmMULTICLICK_KEEP); n++;
  m->w[mm_name] = XtCreateManagedWidget("mm_name",xmPushButtonWidgetClass,m->w[mm_title],args,n);
  XtAddCallback(m->w[mm_name],XmNactivateCallback,mix_console_name_callback,m);
  XmStringFree(s1);
  
  if (!icons_created) create_icons(m->w[mm_name],ss); /* pick up console background color for label pixmaps */

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
  m->w[mm_beg] = XtCreateManagedWidget("beg",xmPushButtonWidgetClass,m->w[mm_title],args,n);
  XtAddCallback(m->w[mm_beg],XmNactivateCallback,beg_click_callback,m);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->mix_color); n++;}
  XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
  XtSetArg(args[n],XmNlabelPixmap,speaker_r); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;}
  m->w[mm_play] = XtCreateManagedWidget("play",xmPushButtonWidgetClass,m->w[mm_title],args,n);
  XtAddCallback(m->w[mm_play],XmNactivateCallback,mix_console_play_callback,m);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->mix_color); n++;}
  XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
  XtSetArg(args[n],XmNlabelPixmap,cross_r); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  m->w[mm_close] = XtCreateManagedWidget("close",xmPushButtonWidgetClass,m->w[mm_title],args,n);
  XtAddCallback(m->w[mm_close],XmNactivateCallback,mix_console_close_callback,m);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->mix_color); n++;}
  XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
  XtSetArg(args[n],XmNlabelPixmap,mixer_r); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  m->w[mm_open] = XtCreateManagedWidget("open",xmPushButtonWidgetClass,m->w[mm_title],args,n);
  XtAddCallback(m->w[mm_open],XmNactivateCallback,mix_console_open_callback,m);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,m->w[mm_title]); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XmNresizable,TRUE); n++;
  m->w[mm_console] = sndCreateFormWidget("mm_console",m->w[mm_fmain],args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XmNheight,0); n++;
  m->w[mm_title_sep] = XtCreateManagedWidget("mm-title-sep",xmSeparatorWidgetClass,m->w[mm_console],args,n);

  n=0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
      XtSetArg(args[n],XmNarmColor,(ss->sgx)->mix_color); n++;
      XtSetArg(args[n],XmNfillOnArm,TRUE); n++;
    }
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,m->w[mm_title_sep]); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
  XtSetArg(args[n],XmNshadowThickness,0); n++;
  XtSetArg(args[n],XmNhighlightThickness,0); n++;
  XtSetArg(args[n],XmNmultiClick,XmMULTICLICK_KEEP); n++;
  m->w[mm_amp] = sndCreatePushButtonWidget(STR_amp_m,m->w[mm_console],args,n);
  XtAddCallback(m->w[mm_amp],XmNactivateCallback,amp_click_callback,m);
  last_widget = m->w[mm_amp];

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,m->w[mm_title_sep]); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNheight,TRACK_BUTTON_SIZE); n++;
  XtSetArg(args[n],XmNindicatorSize,TRACK_BUTTON_SIZE); n++;
  XtSetArg(args[n],XmNmarginWidth,0); n++;
  XtSetArg(args[n],XmNmarginLeft,0); n++;
  XtSetArg(args[n],XmNtopOffset,2); n++;
  XtSetArg(args[n],XmNspacing,0); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNselectColor,(ss->sgx)->pushed_button_color); n++;}
  /* someday we might want the color to code the group (title, waveform, etc) */
  m->w[mm_track] = sndCreateToggleButtonWidget("",m->w[mm_console],args,n);
  XtAddCallback(m->w[mm_track],XmNvalueChangedCallback,mix_track_button_callback,m);

  last_widget = m->w[mm_amp];
  for (i=0;i<md->in_chans;i++)
    {
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      s1=XmStringCreate((i == md->out_chan) ? "1.00" : "0.00","button_font");
      XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,last_widget); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
      /* XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++; */
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
      XtSetArg(args[n],XmNrightPosition,98); n++;
      XtSetArg(args[n],XmNlabelString,s1); n++;
      XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
      /* XtSetArg(args[n],XmNrecomputeSize,FALSE); n++; */
      amp_widget = XtCreateManagedWidget ("amp-number",xmLabelWidgetClass,m->w[mm_console],args,n);
      m->w[mm_chans + mm_amp_label + i*MIX_CHAN_SIZE] = amp_widget;
      XmStringFree(s1);
      
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,last_widget); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      /* XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++; */
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_POSITION); n++;
      XtSetArg(args[n],XmNleftPosition,2); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNrightWidget,amp_widget); n++;
      XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
      XtSetArg(args[n],XmNdragCallback,make_callback_list(m_amp_drag_callback,(XtPointer)m)); n++;
      XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(m_amp_value_changed_callback,(XtPointer)m)); n++;
      /* has to be m here, not md since this is not changed once created */
      XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
      XtSetArg(args[n],XmNvalue,50); n++;
      XtSetArg(args[n],XmNheight,14); n++;
      XtSetArg(args[n],XmNuserData,i); n++; /* pass channel number of this scaler */
      last_widget = XtCreateManagedWidget ("mm-amp",xmScaleWidgetClass,m->w[mm_console],args,n);
      m->w[mm_chans + mm_scl + i*MIX_CHAN_SIZE] = last_widget;
    }
  
  n=0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
      XtSetArg(args[n],XmNarmColor,(ss->sgx)->mix_color); n++;
      XtSetArg(args[n],XmNfillOnArm,TRUE); n++;
    }
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,last_widget); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
  XtSetArg(args[n],XmNshadowThickness,0); n++;
  XtSetArg(args[n],XmNhighlightThickness,0); n++;	 
  XtSetArg(args[n],XmNmultiClick,XmMULTICLICK_DISCARD); n++;
  m->w[mm_speed] = sndCreatePushButtonWidget(STR_speed_m,m->w[mm_console],args,n);
  XtAddCallback(m->w[mm_speed],XmNactivateCallback,speed_click_callback,m);
  
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  s1=XmStringCreate("1.00","button_font");
  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,m->w[mm_speed]); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
  /* XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++; */
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
  XtSetArg(args[n],XmNrightPosition,98); n++;
  XtSetArg(args[n],XmNlabelString,s1); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  m->w[mm_speed_label] = XtCreateManagedWidget ("spd-number",xmLabelWidgetClass,m->w[mm_console],args,n);
  XmStringFree(s1);
  
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,m->w[mm_speed]); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  /* XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++; */
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_POSITION); n++;
  XtSetArg(args[n],XmNleftPosition,2); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNrightWidget,m->w[mm_speed_label]); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNdragCallback,make_callback_list(m_speed_drag_callback,(XtPointer)m)); n++;
  XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(m_speed_value_changed_callback,(XtPointer)m)); n++;
  /* has to be m here, not md since this is not changed once created */
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XmNvalue,50); n++;
  XtSetArg(args[n],XmNheight,14); n++;
  m->w[mm_spdscl] = XtCreateManagedWidget("mm-spdscl",xmScaleWidgetClass,m->w[mm_console],args,n);
  
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,m->w[mm_spdscl]); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
  XtSetArg(args[n],XmNtraversalOn,FALSE); n++;
  XtSetArg(args[n],XmNheight,4); n++;
  m->w[mm_spdsep] = XtCreateManagedWidget("mm-spdsep",xmSeparatorWidgetClass,m->w[mm_console],args,n);

  mousify(m);

  XtVaGetValues(m->w[mm_main],XmNwidth,&wid,NULL);
  md->width = wid;
  m->state = MD_CS;
  m->active = 1;

  if (md->cp) (md->cp)->squelch_update = old_squelch;
}

void use_mixmark(mixdata *md, int x, int y)
{
  mixmark *m;
  m = md->mixer;
  m->owner = md;
  if (!(m->w)) create_mixer(md,x,y);
  fixup_mixmark(md);
  set_mix_track_button_color(md,md->track);
}

