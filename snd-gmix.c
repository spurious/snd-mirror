/* TODO  close leaves full size, no complete minimum size
 */

#include "snd.h"


/* ---------------- MIX CURSOR ---------------- */

static void mix_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  gdk_window_set_cursor(w->window,(((snd_state *)data)->sgx)->mix_cursor);
  gtk_signal_emit_stop_by_name(GTK_OBJECT(w),"enter_notify_event");
}

static void mix_mouse_leave(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  gdk_window_set_cursor(w->window,(((snd_state *)data)->sgx)->graph_cursor); /* arrow_cursor? */
}

static GdkPixmap *speaker_pix,*cross_pix,*mini_pix,*mixer_pix;
static GdkBitmap *speaker_mask,*cross_mask,*mini_mask,*mixer_mask;

/* ---------------- MIX ICONS ---------------- 
 *
 * bitmaps for play (a speaker), close (an x), open/minify boxes, '?', and so on
 */

static int icons_created = 0;

static void create_icons(snd_state *ss)
{
  GdkWindow *wn;
  icons_created = 1;
  wn = MAIN_WINDOW(ss);
  speaker_pix = gdk_pixmap_create_from_xpm_d(wn,&speaker_mask,(ss->sgx)->mix_color,speaker_bits());
  cross_pix = gdk_pixmap_create_from_xpm_d(wn,&cross_mask,(ss->sgx)->mix_color,cross_bits());
  mini_pix = gdk_pixmap_create_from_xpm_d(wn,&mini_mask,(ss->sgx)->mix_color,mini_bits());
  mixer_pix = gdk_pixmap_create_from_xpm_d(wn,&mixer_mask,(ss->sgx)->mix_color,mixer_bits());
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

enum {mm_main,               /* form holds console, catches mouse acts (click, drag) and cursor motion */
      mm_fmain,mm_fevent,
      mm_title,              /* top row of widgets */
      mm_name,
      mm_beg,
      mm_play,mm_play_pix,   /* togglebutton, click to play "solo" or to stop */
      mm_close,mm_close_pix, /* pushbutton; if clicked, remove mini-console altogether (no undo) */
      mm_open,mm_open_pix,   /* pushbutton; if just title, open to full console, else close to title row */
      mm_console,            /* holds the rest of the console under the title bar */
      mm_title_sep,          /* if open, horizontal sep below title */
      mm_amp,                /* the "amp:" label */
      mm_track,mm_trackbox,  /* track button on right */
      mm_speed,              /* if open, srate control */
      mm_speed_label,
      mm_spdscl,
      mm_spdsep,mm_spdbox,mm_amptable,
      mm_chans               /* if open, start of controls (per input channel) */
};         

#define mm_amp_label 0
#define mm_scl 1
#define MIX_CHAN_SIZE 2

#define title_row_start mm_beg
#define title_row_end mm_open_pix


void release_mixmark_widgets(mixmark *m)
{
  if ((m->w) && ((m->w[mm_main]) && (GTK_WIDGET_VISIBLE(m->w[mm_main])))) 
    {
      gtk_widget_hide(m->w[mm_main]);
      m->active = 0;
      m->playing = 0;
      set_pixmap(m->w[mm_play_pix],speaker_pix,speaker_mask);
    }
}

static void activate_mixmark_widgets(mixmark *m)
{
  if ((m->w) && ((m->w[mm_main]) && (!GTK_WIDGET_VISIBLE(m->w[mm_main])))) 
    {
      gtk_widget_show(m->w[mm_main]);
      m->active = 1;
    }
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
	      set_backgrounds(m->w[mm_name],(old_md->wg->color) ? (old_md->wg->color) : ((ss->sgx)->mix_color));
	      set_backgrounds(m->w[mm_title],(old_md->wg->color) ? (old_md->wg->color) : ((ss->sgx)->mix_color));
	    }
	}
    }
  ss->selected_mix = md->id;
  m = md->mixer;
  if (m) 
    {
      set_backgrounds(m->w[mm_name],(ss->sgx)->mix_focus_color);
      set_backgrounds(m->w[mm_title],(ss->sgx)->mix_focus_color);
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
		{
		  set_pixmap(m->w[mm_play_pix],speaker_pix,speaker_mask);
		  set_backgrounds(m->w[mm_play],(ss->sgx)->pushed_button_color);
		}
	      else
		{
		  set_pixmap(m->w[mm_play_pix],speaker_pix,speaker_mask);
		  set_backgrounds(m->w[mm_play],(ss->sgx)->mix_color);
		}
	      set_backgrounds(m->w[mm_name],(ss->sgx)->mix_focus_color);
	      set_backgrounds(m->w[mm_title],(ss->sgx)->mix_focus_color);
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
	    {
	      set_pixmap(m->w[mm_play_pix],speaker_pix,speaker_mask);
	      set_backgrounds(m->w[mm_play],(ss->sgx)->pushed_button_color);
	    }
	  else
	    {
	      set_pixmap(m->w[mm_play_pix],speaker_pix,speaker_mask);
	      set_backgrounds(m->w[mm_play],(ss->sgx)->mix_color);
	    }
	  if (ss->selected_mix == md->id)
	    {
	      set_backgrounds(m->w[mm_name],(ss->sgx)->mix_focus_color);
	      set_backgrounds(m->w[mm_title],(ss->sgx)->mix_focus_color);
	    }
	  else
	    {
	      if (md->wg->color)
		{
		  set_backgrounds(m->w[mm_name],md->wg->color);
		  set_backgrounds(m->w[mm_title],md->wg->color);
		}
	      else
		{
		  set_backgrounds(m->w[mm_name],(ss->sgx)->mix_color);
		  set_backgrounds(m->w[mm_title],(ss->sgx)->mix_color);
		}
	    }
	}
    }
  return(0);
}

void color_one_mix(mixdata *md, GdkColor *color)
{
  mixmark *m;
  if ((md) && (md->mixer))
    {
      m = md->mixer;
      set_backgrounds(m->w[mm_name],color);
      set_backgrounds(m->w[mm_title],color);
      md->wg->color = color;
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
  set_pixmap(m->w[mm_play_pix],speaker_pix,speaker_mask);
  set_backgrounds(m->w[mm_play],(ss->sgx)->mix_color);
}

static void mix_console_play_callback(GtkWidget *w,gpointer clientData) 
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
      set_pixmap(m->w[mm_play_pix],speaker_pix,speaker_mask);
      set_backgrounds(m->w[mm_play],(ss->sgx)->pushed_button_color);
      play_mix(ss,md);
    }
}

static void mix_console_close_callback(GtkWidget *w,gpointer clientData) 
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

static char ampbuf[5]={'1',STR_decimal,'0','0','\0'};
static char srcbuf[5]={'1',STR_decimal,'0','0','\0'};

static Float mix_console_amp_scaler = 1.0;
static Float mix_console_speed_scaler = 1.0;
void set_mix_console_amp_scaler(Float amp) {mix_console_amp_scaler = amp;}
void set_mix_console_speed_scaler(Float amp) {mix_console_speed_scaler = amp;}

Float get_mix_console_amp_scaler(void) {return(mix_console_amp_scaler);}
Float get_mix_console_speed_scaler(void) {return(mix_console_speed_scaler);}


static Float scale_amp_to_Float(Float val) {if (val <= 0.0) return(0.0); else return(exp((Float)((val-.50)*mix_console_amp_scaler)*5.0));}
static Float amp_Float_to_scale(Float scl_amp) {if (scl_amp == 0.0) return(0.0); else return((.50 + log(scl_amp)/(5.0 * mix_console_amp_scaler)));}

static Float scale_speed_to_Float(Float val) {return(exp((Float)(val-.50)*mix_console_speed_scaler*5.0));}
static Float speed_Float_to_scale(Float spd) {return((.50+log(spd)/(5.0*mix_console_speed_scaler)));}

static void change_amp_label(GtkWidget *w, Float amp)
{
  char *sfs;
  sfs=prettyf(amp,2);
  fill_number(sfs,ampbuf);
  FREE(sfs);
  set_label(w,ampbuf);
}

static void set_scale(GtkAdjustment *adj, Float val, GtkWidget *lab)
{
  if ((GTK_WIDGET_VISIBLE(lab) && (widget_width(lab) > 2) && (widget_height(lab) > 2))) /* idiotic Gtk! */
    gtk_adjustment_set_value(adj,val);
  else GTK_ADJUSTMENT(adj)->value = val;
}

static void reflect_mix_amp(GtkAdjustment *scl, GtkWidget *lab, Float amp)
{
  change_amp_label(lab,amp);
  set_scale(scl,amp_Float_to_scale(amp),lab);
}

static Float change_speed_label(GtkWidget *w, snd_state *ss, Float true_speed)
{
  /* return quantized speed (cs->speed normally) */
  Float spd;
  spd = srate_changed(true_speed,srcbuf,speed_style(ss),speed_tones(ss)); 
  set_label(w,srcbuf);
  return(spd);
}

static Float reflect_mix_speed(GtkAdjustment *scl, GtkWidget *lab, snd_state *ss, Float true_speed, Float scl_spd)
{
  /* true_speed as above, scl_spd = int version of scl_speed (old_speed normally) */
  set_scale(scl,scl_spd,lab);
  return(change_speed_label(lab,ss,true_speed));
}

void mix_set_title_beg(mixdata *md, mixmark *m)
{
  console_state *cs;
  chan_info *cp;
  char *str;
  if (m->state != MD_M)
    {
      cs = md->current_cs;
      cp = md->cp;
      str = (char *)CALLOC(32,sizeof(char));
      if (md->beg_in_samps)
	sprintf(str,"%d : %d",cs->beg,cs->beg+cs->len);
      else sprintf(str,"%.3f : %.3f",(Float)(cs->beg)/SND_SRATE(cp->sound),(Float)(cs->beg+cs->len)/SND_SRATE(cp->sound));
      set_button_label(m->w[mm_beg],str); /* button font ? */
      FREE(str);
    }
}

static void beg_click_callback(GtkWidget *w,gpointer clientData) 
{
   mixmark *m = (mixmark *)clientData;
   mixdata *md;
   md = (mixdata *)(m->owner);
   md->beg_in_samps = (!(md->beg_in_samps));
   mix_set_title_beg(md,m);
}


static void amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* 1 click: set to 1 and 0's ("identity"), 2 click: mute (all 0's) */
  /* cntrl-click once to reset to previous saved state */
  mixmark *m = (mixmark *)data;
  mixdata *md;
  static TIME_TYPE last_press = 0;
  int chan;
  console_state *cs;
  chan_info *cp;
  int i,j;
  md = (mixdata *)(m->owner);
  cp = md->cp;
  chan = cp->chan;
  cs = md->current_cs;
  if ((ev->time - last_press) > 200)
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
	  reflect_mix_amp(GTK_ADJUSTMENT(m->a[i+1]),m->w[j+mm_amp_label],cs->scalers[i]);
	}
    }
  else
    {
      /* can't get here without going through click==1, so just reset chan from callData */
      if (md->main_chan >= 0) 
	{
	  cs->scalers[md->main_chan] = 0.0;
	  reflect_mix_amp(GTK_ADJUSTMENT(m->a[1+md->main_chan]),m->w[mm_chans+mm_amp_label+md->main_chan*MIX_CHAN_SIZE],0.0);
	}
    }
  last_press = ev->time;
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
  if (m) reflect_mix_amp(GTK_ADJUSTMENT(m->a[1+chan]),m->w[mm_chans+mm_amp_label+chan*MIX_CHAN_SIZE],amp);
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
  cs->old_speed = speed_Float_to_scale(spd); 
  if (m)
    cs->speed = reflect_mix_speed(GTK_ADJUSTMENT(m->a[0]),m->w[mm_speed_label],md->ss,cs->scl_speed,cs->old_speed);
  else cs->speed = srate_changed(spd,srcbuf,speed_style(ss),speed_tones(ss)); 
  cs->len = (int)(ceil(md->in_samps / cs->speed));
  if (m) mix_set_title_beg(md,m);
}

static void speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* set speed to 1 (control-click for reset to previous?) */
  mixmark *m = (mixmark *)data;
  mixdata *md;
  console_state *cs;
  Float val;
  chan_info *cp;
  md = (mixdata *)(m->owner);
  cs = md->current_cs;
  if (ev->state & (snd_ControlMask | snd_MetaMask))
    {
      cs->scl_speed = scale_speed_to_Float(cs->old_speed);
      val = cs->old_speed;
    }
  else
    {
      cs->scl_speed = 1.0;
      val = .50;
    }
  cs->speed = reflect_mix_speed(GTK_ADJUSTMENT(m->a[0]),m->w[mm_speed_label],md->ss,cs->scl_speed,val);
  cs->len = (int)(ceil(md->in_samps / cs->speed));
  mix_set_title_beg(md,m);
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if (!(call_mix_speed_changed_hook(md)))
    remix_file(md,"Mix: speed click");
}

static void amp_drag_callback(GtkAdjustment *adj, gpointer data)
{
  /* amp slider drag = value_changed signal in gtk */
  console_state *cs;
  chan_info *cp;
  mixdata *md;
  mix_context *ms;
  int chan;
  mixmark *m = (mixmark *)data;
  md = (mixdata *)(m->owner);
  ms = md->wg;
  cp = md->cp;
  chan = (int)gtk_object_get_user_data(GTK_OBJECT(adj)); /* set the chan on the adjustment as well as the scale! */
  if (md->in_chans > 1)
    {
      set_foreground(m->w[mm_chans + mm_amp_label + (md->selected_chan)*MIX_CHAN_SIZE],((md->ss)->sgx)->black);
      set_foreground(m->w[mm_chans + mm_amp_label + chan*MIX_CHAN_SIZE],((md->ss)->sgx)->red);
    }
  md->selected_chan = chan;
  cs = md->current_cs;
  cs->scalers[chan] = scale_amp_to_Float(adj->value);
  change_amp_label(m->w[mm_chans + mm_amp_label + chan*MIX_CHAN_SIZE],cs->scalers[chan]);
  if (!(ms->lastpj)) {ms->lastpj = make_graph(cp,cp->sound,cp->state); mix_save_graph(md->ss,md->wg,ms->lastpj);}
  make_temporary_graph(cp,md,cs);
}

static void amp_value_changed_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* amp slider button release = button release event in gtk */
  int chan;
  mixdata *md;
  mix_context *ms;
  console_state *cs;
  chan_info *cp;
  mixmark *m = (mixmark *)data;
  md = (mixdata *)(m->owner);
  ms = md->wg;
  ms->lastpj = 0;
  chan = (int)gtk_object_get_user_data(GTK_OBJECT(w));
  cs = md->current_cs;
  cs->old_scalers[chan] = cs->scalers[chan];
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if (!(call_mix_amp_changed_hook(md)))
    remix_file(md,"Mix: amp");
}

static void speed_drag_callback(GtkAdjustment *adj, gpointer data)
{
  /* speed slider drag */
  console_state *cs;
  chan_info *cp;
  mixdata *md;
  mix_context *ms;
  int chan;
  snd_state *ss;
  mixmark *m = (mixmark *)data;
  md = (mixdata *)(m->owner);
  ms = md->wg;
  chan = (int)gtk_object_get_user_data(GTK_OBJECT(adj));
  cs = md->current_cs;
  cp = md->cp;
  ss = md->ss;
  cs->scl_speed = scale_speed_to_Float(adj->value);
  cs ->speed = change_speed_label(m->w[mm_speed_label],md->ss,cs->scl_speed);
  if (!(ms->lastpj)) {ms->lastpj = make_graph(cp,cp->sound,cp->state); mix_save_graph(md->ss,md->wg,ms->lastpj);}
  if (show_mix_waveforms(ss)) erase_mix_waveform(md,m->y);      
  cs->len = (int)(ceil(md->in_samps / cs->speed));
  make_temporary_graph(cp,md,cs);
  mix_set_title_beg(md,m);
  if (show_mix_waveforms(ss)) draw_mix_waveform(md,m->y);      
}

static void speed_value_changed_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* speed slider button release */
  int chan;
  mixdata *md;
  mix_context *ms;
  console_state *cs;
  chan_info *cp;
  mixmark *m = (mixmark *)data;
  md = (mixdata *)(m->owner);
  ms = md->wg;
  ms->lastpj = 0;
  chan = (int)gtk_object_get_user_data(GTK_OBJECT(w));
  cs = md->current_cs;
  cs->old_speed = cs->speed;
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if (!(call_mix_speed_changed_hook(md)))
    remix_file(md,"Mix: speed");
}

/* manage the mix console (4 states: undisplayed, name[0], title row, title+scalers) */

void mix_set_minimal_title(mixdata *md, mixmark *m)
{
  char buf[2];
  buf[0] = md->name[0];
  buf[1] = '\0';
  set_button_label(m->w[mm_name],buf);
}

void mix_set_title_name(mixdata *md, mixmark *m)
{
  set_button_label(m->w[mm_name],md->name); 
}

void mix_set_console(mixdata *md, mixmark *m)
{
  console_state *cs;
  int i,j;
  snd_state *ss;
  ss = md->ss;
  cs = md->current_cs;
  for (i=0,j=mm_chans;i<md->in_chans;i++,j+=MIX_CHAN_SIZE)
    reflect_mix_amp(GTK_ADJUSTMENT(m->a[i+1]),m->w[j+mm_amp_label],cs->scalers[i]);
  reflect_mix_speed(GTK_ADJUSTMENT(m->a[0]),m->w[mm_speed_label],ss,cs->scl_speed,cs->old_speed);
}

void mix_open_console(mixmark *m)
{
  int i;
  for (i=0;i<m->size;i++) if (m->w[i]) gtk_widget_show(m->w[i]);
  set_pixmap(m->w[mm_open_pix],mini_pix,mini_mask);
  gtk_widget_show(m->w[mm_console]);
}

void mix_close_console(mixmark *m)
{
  int i;
  for (i=title_row_end+1;i<m->size;i++) if (m->w[i]) gtk_widget_hide(m->w[i]);
  gtk_widget_hide(m->w[mm_console]);
  set_pixmap(m->w[mm_open_pix],mixer_pix,mixer_mask);
}

void mix_open_title(mixmark *m)
{
  int i;
  for (i=title_row_start;i<=title_row_end;i++) if (m->w[i]) gtk_widget_show(m->w[i]);
}

void mix_close_title(mixmark *m)
{
  int i;
  return;
  for (i=title_row_start;i<=title_row_end;i++) if (m->w[i]) gtk_widget_hide(m->w[i]);
}

void fixup_mixmark(mixdata *md)
{
  mixmark *m;
  m = md->mixer;
  fixup_mixmark_1(md,m);
  if (m->active == 0) activate_mixmark_widgets(m);
}

static void mix_console_open_callback(GtkWidget *w,gpointer clientData) 
{
  mixmark *m = (mixmark *)clientData;
  mixdata *md;
  chan_info *cp;
  md = (mixdata *)(m->owner);
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if (md->state == MD_CS) md->state = MD_M; else md->state = MD_CS;
  fixup_mixmark(md);
  call_mix_console_state_changed_hook(md);
}

static void mix_title_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  mixmark *m = (mixmark *)data;
  chan_info *cp;
  snd_state *ss;
  cp = m_to_cp(m);
  ss = cp->state;
  if (!(ss->using_schemes)) set_foreground(m->w[mm_name],(ss->sgx)->red);
  mix_remember_console(m,cp,ev->time,widget_x(m->w[mm_main]),(int)(ev->x_root));
}

/* does not currently extend the base file as we push off the right (or left???) edge */
/* also what about dragging in the y direction? */

int move_mix_console(mixmark *m, int *nx)
{
  GtkWidget *w;
  w = m->w[mm_main];
  set_widget_x(w,m->x);
  (*nx) = (int)widget_x(w);
  return((int)widget_width(w));
}

static int need_mix_position_update = 0;

void mix_raise_console(mixmark *m)
{
  raise_widget(m->w[mm_main]); /* bring to top of possibly stacked console tree */
}

static void mix_title_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  mixmark *m = (mixmark *)data;
  snd_state *ss; 
  chan_info *cp;
  if (need_mix_position_update)
    {
      need_mix_position_update = 0;
      move_mix(m,mix_current_mix_x());
    }
  m->moving = 0;
  cp = m_to_cp(m);
  ss = cp->state;
  if (!(ss->using_schemes)) set_foreground(m->w[mm_name],(ss->sgx)->black);
  mix_watch_title(m,cp);
}


static void mix_console_name_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* called upon button release in name widget */
  mixmark *m = (mixmark *)data;
  mixdata *md;
  static TIME_TYPE last_console_time = 0;
  chan_info *cp;
  md = (mixdata *)(m->owner);
  cp = md->cp;
  select_channel(cp->sound,cp->chan);
  if ((ev->time - last_console_time) < 200)
    {
      /* if we're completely closed, open the title bar, else collapse to a single char */
      if (md->state == MD_M) md->state = MD_CS; else md->state = MD_M;
      fixup_mixmark(md);
      call_mix_console_state_changed_hook(md);
    }
  last_console_time = ev->time;
}


void set_mix_track_button_color(mixdata *md, int track)
{
  GtkWidget *w;
  snd_state *ss;
  mixmark *m;
  ss = md->ss;
  m = md->mixer;
  if (m)
    {
      w = m->w[mm_track];
      switch (track)
	{
	case 0: set_active_color(w,(ss->sgx)->basic_color); break;
	case 1: set_active_color(w,(ss->sgx)->pushed_button_color); break;
	case 2: set_active_color(w,(ss->sgx)->green); break;
	case 3: set_active_color(w,(ss->sgx)->yellow); break;
	case 4: set_active_color(w,(ss->sgx)->red); break;
	default: set_active_color(w,(ss->sgx)->black); break;
	}
      set_toggle_button(w,(track == 0) ? FALSE : TRUE,FALSE,(void *)m);
    }
}

static int last_track_state = 0;

static void mix_track_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_track_state = ev->state;
}

static void track_button_click(GtkWidget *w, gpointer data)
{
  mixmark *m = (mixmark *)data;
  int track;
  mixdata *md;
  if (GTK_TOGGLE_BUTTON(w)->active)
    if (last_track_state & snd_ControlMask) 
      if (last_track_state & snd_MetaMask)
	if (last_track_state & snd_ShiftMask)
	  track = 4;
	else track = 3;
      else track = 2;
    else track = 1;
  else track = 0;
  md = (mixdata *)(m->owner);
  md->track = track;
  set_mix_track_button_color(md,track);
}

static void mix_title_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  mixmark *m = (mixmark *)data;
  int pending;
  pending = gtk_events_pending();
  mix_title_move(m,ev->x_root,ev->time,100,(!pending));
  need_mix_position_update = pending;
}

void move_mixmark(mixmark *m, int x, int y)
{
  if ((widget_x(m->w[mm_main]) != x) || (widget_y(m->w[mm_main]) != y))
    {
      set_widget_position(m->w[mm_main],x,y);
      m->x = widget_x(m->w[mm_main]);
      m->y = widget_y(m->w[mm_main]);
    }
  if (!(m->active)) activate_mixmark_widgets(m);
}

void move_mix_y(mixmark *m, int yspot)
{
  set_widget_y(m->w[mm_main],yspot);
  m->y = widget_y(m->w[mm_main]);
}

void move_mix_x(mixmark *m, int xspot)
{
  set_widget_x(m->w[mm_main],xspot);
  m->x = widget_x(m->w[mm_main]);
}

#define TRACK_BUTTON_SIZE 13

static void create_mixer(mixdata *md, int x, int y)
{
  /* make a new mixer console */
  snd_state *ss;
  mixmark *m;
  mix_context *ms;
  int i,n,old_squelch=0;
  GtkWidget *last_widget;
  ss = md->ss;
  ms = md->wg;
  m = md->mixer;

  if (!icons_created) create_icons(ss);
  if (md->cp) 
    {
      old_squelch = (md->cp)->squelch_update;
      (md->cp)->squelch_update = 1;
    }
  m->size = mm_chans+(md->in_chans*MIX_CHAN_SIZE);
  m->w = (GtkWidget **)CALLOC(m->size,sizeof(GtkWidget *));
  m->a = (GtkObject **)CALLOC(1+md->in_chans,sizeof(GtkObject *));
  m->chans_allocated = md->in_chans;
  n=0;

  m->w[mm_main] = gtk_event_box_new();
  gtk_snd_fixed_put(GTK_SND_FIXED(ms->graph),m->w[mm_main],x,y);
  set_widget_position(m->w[mm_main],x,y);
  gtk_widget_show(m->w[mm_main]);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_main]),"enter_notify_event",GTK_SIGNAL_FUNC(mix_mouse_enter),(gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_main]),"leave_notify_event",GTK_SIGNAL_FUNC(mix_mouse_leave),(gpointer)ss);

  m->w[mm_fevent] = gtk_button_new();
  set_backgrounds(m->w[mm_fevent],(ss->sgx)->basic_color);
  gtk_container_add(GTK_CONTAINER(m->w[mm_main]),m->w[mm_fevent]);
  gtk_widget_show(m->w[mm_fevent]);
  m->x = x;
  m->y = y;

  m->w[mm_fmain] = gtk_vbox_new(FALSE,0);
  gtk_container_add(GTK_CONTAINER(m->w[mm_fevent]),m->w[mm_fmain]);
  set_backgrounds(m->w[mm_fmain],(ss->sgx)->mix_color);
  gtk_widget_show(m->w[mm_fmain]);

  m->w[mm_title] = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(m->w[mm_fmain]),m->w[mm_title],FALSE,FALSE,0);
  set_backgrounds(m->w[mm_title],(ss->sgx)->mix_color);
  gtk_widget_show(m->w[mm_title]);

  m->w[mm_name] = gtk_button_new_with_label(md->name);
  set_backgrounds(m->w[mm_name],(ss->sgx)->mix_color);
  gtk_widget_add_events(m->w[mm_name],GDK_POINTER_MOTION_HINT_MASK);
  gtk_box_pack_start(GTK_BOX(m->w[mm_title]),m->w[mm_name],FALSE,FALSE,2);
  gtk_widget_show(m->w[mm_name]);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_name]),"button_press_event",GTK_SIGNAL_FUNC(mix_title_button_press),(gpointer)m);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_name]),"button_release_event",GTK_SIGNAL_FUNC(mix_title_button_release),(gpointer)m);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_name]),"motion_notify_event",GTK_SIGNAL_FUNC(mix_title_button_motion),(gpointer)m);

  m->w[mm_beg] = gtk_button_new_with_label("0.00:0.00");
  gtk_box_pack_start(GTK_BOX(m->w[mm_title]),m->w[mm_beg],TRUE,TRUE,2);
  set_backgrounds(m->w[mm_beg],(ss->sgx)->white);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_beg]),"clicked",GTK_SIGNAL_FUNC(beg_click_callback),(gpointer)m);
  gtk_widget_show(m->w[mm_beg]);

  m->w[mm_play] = gtk_button_new();
  set_backgrounds(m->w[mm_play],(ss->sgx)->mix_color);
  gtk_box_pack_start(GTK_BOX(m->w[mm_title]),m->w[mm_play],FALSE,FALSE,2);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_play]),"clicked",GTK_SIGNAL_FUNC(mix_console_play_callback),(gpointer)m);
  gtk_widget_show(m->w[mm_play]);
      
  m->w[mm_play_pix] = gtk_pixmap_new(speaker_pix,speaker_mask);
  gtk_container_add(GTK_CONTAINER(m->w[mm_play]),m->w[mm_play_pix]);
  gtk_widget_show(m->w[mm_play_pix]);

  m->w[mm_close] = gtk_button_new();
  set_backgrounds(m->w[mm_close],(ss->sgx)->mix_color);
  gtk_box_pack_start(GTK_BOX(m->w[mm_title]),m->w[mm_close],FALSE,FALSE,2);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_close]),"clicked",GTK_SIGNAL_FUNC(mix_console_close_callback),(gpointer)m);
  gtk_widget_show(m->w[mm_close]);
      
  m->w[mm_close_pix] = gtk_pixmap_new(cross_pix,cross_mask);
  gtk_container_add(GTK_CONTAINER(m->w[mm_close]),m->w[mm_close_pix]);
  gtk_widget_show(m->w[mm_close_pix]);

  m->w[mm_open] = gtk_button_new();
  set_backgrounds(m->w[mm_open],(ss->sgx)->mix_color);
  gtk_box_pack_start(GTK_BOX(m->w[mm_title]),m->w[mm_open],FALSE,FALSE,2);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_open]),"clicked",GTK_SIGNAL_FUNC(mix_console_open_callback),(gpointer)m);
  gtk_widget_show(m->w[mm_open]);
      
  m->w[mm_open_pix] = gtk_pixmap_new(mixer_pix,mixer_mask);
  gtk_container_add(GTK_CONTAINER(m->w[mm_open]),m->w[mm_open_pix]);
  gtk_widget_show(m->w[mm_open_pix]);

  m->w[mm_console] = gtk_vbox_new(FALSE,0); /* not strictly needed, but a convenience for open/close */
  set_backgrounds(m->w[mm_console],(ss->sgx)->basic_color);
  gtk_box_pack_start(GTK_BOX(m->w[mm_fmain]),m->w[mm_console],TRUE,TRUE,0);
  gtk_widget_show(m->w[mm_console]);

  m->w[mm_title_sep] = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(m->w[mm_console]),m->w[mm_title_sep],FALSE,FALSE,2);
  gtk_widget_show(m->w[mm_title_sep]);

  m->w[mm_trackbox] = gtk_hbox_new(FALSE,0);
  set_backgrounds(m->w[mm_trackbox],(ss->sgx)->basic_color);
  gtk_box_pack_start(GTK_BOX(m->w[mm_console]),m->w[mm_trackbox],FALSE,FALSE,0);
  gtk_widget_show(m->w[mm_trackbox]);

  m->w[mm_amp] = gtk_button_new_with_label(STR_amp_p);
  gtk_box_pack_start(GTK_BOX(m->w[mm_trackbox]),m->w[mm_amp],TRUE,TRUE,0);
  set_backgrounds(m->w[mm_amp],(ss->sgx)->basic_color);
  gtk_widget_show(m->w[mm_amp]);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_amp]),"button_press_event",GTK_SIGNAL_FUNC(amp_click_callback),(gpointer)m);

  m->w[mm_track] = gtk_check_button_new();
  set_backgrounds(m->w[mm_track],(ss->sgx)->basic_color);
  gtk_box_pack_end(GTK_BOX(m->w[mm_trackbox]),m->w[mm_track],FALSE,FALSE,0);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_track]),"button_press_event",GTK_SIGNAL_FUNC(mix_track_button_callback),(gpointer)m);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_track]),"toggled",GTK_SIGNAL_FUNC(track_button_click),(gpointer)m);
  gtk_widget_show(m->w[mm_track]);

  m->w[mm_amptable] = gtk_table_new(2,md->in_chans,FALSE);
  gtk_box_pack_start(GTK_BOX(m->w[mm_console]),m->w[mm_amptable],TRUE,TRUE,0);
  gtk_widget_show(m->w[mm_amptable]);

  for (i=0;i<md->in_chans;i++)
    {
      last_widget = gtk_label_new((i == md->out_chan) ? "1.00" : "0.00");
      set_backgrounds(last_widget,(ss->sgx)->basic_color);
      m->w[mm_chans + mm_amp_label + i*MIX_CHAN_SIZE] = last_widget;
      gtk_table_attach(GTK_TABLE(m->w[mm_amptable]),last_widget,0,1,i,i+1,0,0,0,0);
      gtk_widget_show(last_widget);

      m->a[i+1] = gtk_adjustment_new((i == md->out_chan) ? 0.5 : 0.0,0.0,1.01,0.001,0.01,.1);
      gtk_object_set_user_data(GTK_OBJECT(m->a[i+1]),(gpointer)i);

      last_widget = gtk_hscrollbar_new(GTK_ADJUSTMENT(m->a[i+1]));
      m->w[mm_chans + mm_scl + i*MIX_CHAN_SIZE] = last_widget;
      gtk_table_attach_defaults(GTK_TABLE(m->w[mm_amptable]),last_widget,1,2,i,i+1);
      gtk_widget_show(last_widget);
      gtk_signal_connect(GTK_OBJECT(last_widget),"button_release_event",GTK_SIGNAL_FUNC(amp_value_changed_callback),(gpointer)m);
      gtk_signal_connect(GTK_OBJECT(m->a[i+1]),"value_changed",GTK_SIGNAL_FUNC(amp_drag_callback),(gpointer)m);
    }

  m->w[mm_speed] = gtk_button_new_with_label(STR_speed);
  gtk_box_pack_start(GTK_BOX(m->w[mm_console]),m->w[mm_speed],TRUE,TRUE,0);
  set_backgrounds(m->w[mm_speed],(ss->sgx)->basic_color);
  gtk_widget_show(m->w[mm_speed]);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_speed]),"button_press_event",GTK_SIGNAL_FUNC(speed_click_callback),(gpointer)m);

  m->w[mm_spdbox] = gtk_hbox_new(FALSE,0);
  gtk_box_pack_start(GTK_BOX(m->w[mm_console]),m->w[mm_spdbox],FALSE,FALSE,0);
  gtk_widget_show(m->w[mm_spdbox]);
  
  m->w[mm_speed_label] = gtk_label_new("1.00");
  GTK_WIDGET_UNSET_FLAGS(m->w[mm_speed_label],GTK_CAN_FOCUS);
  set_backgrounds(m->w[mm_speed_label],(ss->sgx)->basic_color);
  gtk_box_pack_start(GTK_BOX(m->w[mm_spdbox]),m->w[mm_speed_label],FALSE,FALSE,0);
  gtk_widget_show(m->w[mm_speed_label]);
  
  m->a[0] = gtk_adjustment_new(0.5,0.0,1.01,0.001,0.01,.1);
  m->w[mm_spdscl] = gtk_hscrollbar_new(GTK_ADJUSTMENT(m->a[0]));
  gtk_box_pack_start(GTK_BOX(m->w[mm_spdbox]),m->w[mm_spdscl],TRUE,TRUE,2);
  gtk_widget_show(m->w[mm_spdscl]);
  gtk_signal_connect(GTK_OBJECT(m->a[0]),"value_changed",GTK_SIGNAL_FUNC(speed_drag_callback),(gpointer)m);
  gtk_signal_connect(GTK_OBJECT(m->w[mm_spdscl]),"button_release_event",GTK_SIGNAL_FUNC(speed_value_changed_callback),(gpointer)m);
  
  m->state = MD_CS;
  m->active = 1;

  for (i=0;i<mm_chans+(md->in_chans*MIX_CHAN_SIZE);i++) 
    if (m->w[i]) 
      {
	GTK_WIDGET_UNSET_FLAGS(m->w[i],GTK_CAN_FOCUS);
      }
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



#if HAVE_GUILE_GTK
#include <guile-gtk.h>

#define NO_SUCH_WIDGET gh_symbol2scm("no-such-widget")

static SCM get_mix_widget(SCM id, int which_widget, char *caller)
{
  mixdata *md;
  mixmark *mixwids;
  md = md_from_int(g_scm2intdef(id,0));
  if (md) 
    {
      mixwids = md->mixer;
      if (mixwids)
	{
	  if ((mixwids->w) && (mixwids->w[which_widget]))
	    return(sgtk_wrap_gtkobj((GtkObject *)(mixwids->w[which_widget])));
	}
      return(scm_throw(NO_SUCH_WIDGET,SCM_LIST1(gh_str02scm(caller))));
    }
  return(scm_throw(NO_SUCH_MIX,SCM_LIST2(gh_str02scm(caller),id)));
}

#if 0
enum {mm_main,mm_fevent,
      mm_title,              /* top row of widgets */
      mm_name,
      mm_beg,
      mm_play,
      mm_close,
      mm_open,
      mm_console,            /* holds the rest of the console under the title bar */
      mm_amp,      /* the "amp:" label */
      mm_track,
      mm_speed,  /* if open, srate control */
      mm_speed_label,
      mm_spdscl,
      mm_chans               /* if open, start of controls (per input channel) */
#endif
	/* the adjustments are in m->a */


#endif
