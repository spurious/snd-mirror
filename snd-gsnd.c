/* TODO: 
 *       if controls close, drag main sash should keep it closed
 */

#include "snd.h"

enum {W_pane,W_pane_box,W_ctrls,
      W_name_form,W_name,W_name_event,W_name_icon,W_name_pix,W_info_label,W_info,W_info_sep,
      W_play,W_sync,W_combine,
      W_amp_form,W_amp_event,W_amp,W_amp_label,W_amp_number,W_amp_sep,
      W_srate_form,W_srate,W_srate_event,W_srate_label,W_srate_number,W_srate_arrow,W_srate_pix,
      W_expand_form,W_expand,W_expand_event,W_expand_label,W_expand_number,W_expand_button,
      W_contrast_form,W_contrast,W_contrast_event,W_contrast_label,W_contrast_number,W_contrast_button,
      W_reverb_form,W_revscl,W_revscl_event,W_revscl_label,W_revscl_number,
      W_revlen,W_revlen_event,W_revlen_label,W_revlen_number,W_reverb_button,

      W_filter_form,W_filter_label,W_filter_order,W_filter_env,W_filter,W_filter_button,W_filter_dB,W_filter_frame,

      W_apply_form,W_remember,W_restore,W_apply,W_reset
};

enum {W_amp_adj,W_srate_adj,W_contrast_adj,W_expand_adj,W_revscl_adj,W_revlen_adj,W_filter_adj
};


#define NUM_SND_WIDGETS 62
#define NUM_SND_ADJS 7

GtkWidget *w_snd_ctrls(snd_info *sp) {if ((sp) && (sp->sgx)) return(((snd_context *)(sp->sgx))->snd_widgets[W_ctrls]); else return(NULL);}
GtkWidget *w_snd_pane(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_pane]); else return(NULL);}
GtkWidget *w_snd_pane_box(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_pane_box]); else return(NULL);}
GtkWidget *w_snd_name(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_name]); else return(NULL);}
GtkWidget *w_snd_combine(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_combine]); else return(NULL);}
GtkWidget *w_snd_play(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_play]); else return(NULL);}
GtkWidget *w_snd_filter_env(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter_env]); else return(NULL);}

static GtkWidget *w_snd_expand(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_expand]); else return(NULL);}
static GtkWidget *w_snd_contrast(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_contrast]); else return(NULL);}
static GtkWidget *w_snd_revscl(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_revscl]); else return(NULL);}
static GtkWidget *w_snd_revlen(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_revlen]); else return(NULL);}
static GtkWidget *w_snd_minibuffer(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_info]); else return(NULL);}
static GtkWidget *w_snd_minibuffer_label(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_info_label]); else return(NULL);}
static GtkWidget *w_snd_amp_number(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_amp_number]); else return(NULL);}
static GtkWidget *w_snd_srate_number(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_srate_number]); else return(NULL);}
static GtkWidget *w_snd_srate_pix(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_srate_pix]); else return(NULL);}
static GtkWidget *w_snd_expand_number(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_expand_number]); else return(NULL);}
static GtkWidget *w_snd_expand_button(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_expand_button]); else return(NULL);}
static GtkWidget *w_snd_contrast_number(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_contrast_number]); else return(NULL);}
static GtkWidget *w_snd_contrast_button(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_contrast_button]); else return(NULL);}
static GtkWidget *w_snd_revscl_number(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_revscl_number]); else return(NULL);}
static GtkWidget *w_snd_revlen_number(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_revlen_number]); else return(NULL);}
static GtkWidget *w_snd_reverb_button(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_reverb_button]); else return(NULL);}
static GtkWidget *w_snd_apply(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_apply]); else return(NULL);}
static GtkWidget *w_snd_filter_order(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter_order]); else return(NULL);}
static GtkWidget *w_snd_filter(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter]); else return(NULL);}
static GtkWidget *w_snd_filter_button(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter_button]); else return(NULL);}
static GtkWidget *w_snd_filter_dB(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_filter_dB]); else return(NULL);}
static GtkWidget *w_snd_name_pix(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_name_pix]); else return(NULL);}
static GtkWidget *w_snd_minibuffer_sep(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_info_sep]); else return(NULL);}
static GtkWidget *w_snd_sync(snd_info *sp) {if ((sp) && (sp->sgx)) return((sp->sgx)->snd_widgets[W_sync]); else return(NULL);}

static GtkObject *w_amp_adj(snd_info *sp) {return(((snd_context *)(sp->sgx))->snd_adjs[W_amp_adj]);}
static GtkObject *w_srate_adj(snd_info *sp) {return(((snd_context *)(sp->sgx))->snd_adjs[W_srate_adj]);}
static GtkObject *w_expand_adj(snd_info *sp) {return(((snd_context *)(sp->sgx))->snd_adjs[W_expand_adj]);}
static GtkObject *w_contrast_adj(snd_info *sp) {return(((snd_context *)(sp->sgx))->snd_adjs[W_contrast_adj]);}
static GtkObject *w_revscl_adj(snd_info *sp) {return(((snd_context *)(sp->sgx))->snd_adjs[W_revscl_adj]);}
static GtkObject *w_revlen_adj(snd_info *sp) {return(((snd_context *)(sp->sgx))->snd_adjs[W_revlen_adj]);}


/* -------- PIXMAPS -------- */

static GdkPixmap *mini_lock,*speed_r,*speed_l,*blank;
static int mini_lock_allocated = 0;
static GdkPixmap *mini_bombs[NUM_BOMBS];
static GdkPixmap *mini_glasses[NUM_GLASSES];
static GdkBitmap *lock_mask,*blank_mask,*speed_l_mask,*speed_r_mask,*bomb_mask,*glass_mask;

void snd_file_lock_icon(snd_info *sp, int on)
{
  snd_context *sx;
  if (mini_lock) 
    {
      sx = sp->sgx;
      if (on)
	{
	  sx->file_pix = mini_lock;
	  sx->file_mask = lock_mask;
	}
      else 
	{
	  sx->file_pix = blank;
	  sx->file_mask = blank_mask;
	}
      gtk_pixmap_set(GTK_PIXMAP(w_snd_name_pix(sp)),sx->file_pix,sx->file_mask);
    }
}


#define BOMB_TIME 200

static void show_bomb_icon(snd_info *sp, int on)
{
  snd_context *sx;
  if (sp->bomb_ctr >= NUM_BOMBS) sp->bomb_ctr = 0;
  if (mini_bombs[sp->bomb_ctr]) 
    {
      sx = sp->sgx;
      if (on)
	{
	  sx->file_pix = mini_bombs[sp->bomb_ctr];
	  sx->file_mask = bomb_mask;
	}
      else 
	{
	  sx->file_pix = blank;
	  sx->file_mask = blank_mask;
	}
      gtk_pixmap_set(GTK_PIXMAP(w_snd_name_pix(sp)),sx->file_pix,sx->file_mask);
    }
}

void x_bomb(snd_info *sp, int on)
{
  show_bomb_icon(sp,on);
  if (on) sp->bomb_ctr++; else sp->bomb_ctr = 0;
}

static int inc_bomb(snd_info *sp, void *ptr)
{
  int *buf;
  if (sp)
    {
      if (sp->need_update)
	{
	  buf = (int *)ptr;
	  buf[0]++;
	  show_bomb_icon(sp,sp->bomb_ctr);
	  sp->bomb_ctr++;
	}
    }
  return(0);
}

static int bomb_in_progress = 0;

static gint bomb_check(gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_state *ss;
  int incs[1];
  ss = sp->state;
  incs[0] = 0;
  map_over_sounds(ss,inc_bomb,(void *)incs);
  if (incs[0] > 0)
    gtk_timeout_add((guint32)BOMB_TIME,bomb_check,data);
  else bomb_in_progress = 0;
  return(0);
}

void snd_file_bomb_icon(snd_info *sp, int on)
{
  snd_state *ss;
  if ((on) && (bomb_in_progress == 0))
    {
      ss = sp->state;
      bomb_in_progress = 1;
      gtk_timeout_add((guint32)BOMB_TIME,bomb_check,(gpointer)sp);
    }
}

static void snd_file_glasses_icon(snd_info *sp, int on, int glass)
{
  GtkWidget *w;
  snd_context *sx;
  w = w_snd_name_pix(sp);
  if (on)
    {
      if (mini_glasses[glass]) gtk_pixmap_set(GTK_PIXMAP(w),mini_glasses[glass],glass_mask);
    }
  else
    {
      sx = sp->sgx;
      gtk_pixmap_set(GTK_PIXMAP(w),sx->file_pix,sx->file_mask);
    }
}

static void make_pixmaps(snd_state *ss)
{
  GdkWindow *wn;
  int k;
  if (!mini_lock_allocated)
    { 
      wn = MAIN_WINDOW(ss);
      mini_lock = gdk_pixmap_create_from_xpm_d(wn,&lock_mask,NULL,mini_lock_bits());
      blank = gdk_pixmap_create_from_xpm_d(wn,&blank_mask,NULL,blank_bits());
      speed_r = gdk_pixmap_create_from_xpm_d(wn,&speed_r_mask,NULL,speed_r_bits());
      speed_l = gdk_pixmap_create_from_xpm_d(wn,&speed_l_mask,NULL,speed_l_bits());
      for (k=0;k<NUM_BOMBS;k++) mini_bombs[k] = gdk_pixmap_create_from_xpm_d(wn,&bomb_mask,NULL,mini_bomb_bits(k));
      for (k=0;k<NUM_GLASSES;k++) mini_glasses[k] = gdk_pixmap_create_from_xpm_d(wn,&glass_mask,NULL,mini_glass_bits(k));
      mini_lock_allocated = 1;
    }
}

void set_blank_pixmap(GtkWidget *w)
{
  gtk_pixmap_set(GTK_PIXMAP(w),blank,blank_mask);
}

GtkWidget *get_blank_pixmap(snd_state *ss) 
{
  make_pixmaps(ss);
  return(gtk_pixmap_new(blank,blank_mask));
}


/* -------- MINIBUFFER CALLBACKS -------- */

void goto_minibuffer(snd_info *sp)
{
  snd_state *ss;
  if (sp) 
    {
      ss = sp->state;
      (sp->sgx)->mini_active = 1;
      set_text_background(w_snd_minibuffer(sp),(ss->sgx)->white);
      goto_window(w_snd_minibuffer(sp));
    }
}

void set_minibuffer_cursor_position(snd_info *sp, int pos)
{
  gtk_editable_set_position(GTK_EDITABLE(w_snd_minibuffer(sp)),pos);
}

char *get_minibuffer_string(snd_info *sp) 
{
  return(gtk_editable_get_chars(GTK_EDITABLE(w_snd_minibuffer(sp)),0,-1)); /* should be freed with g_free */
} 

void set_minibuffer_string(snd_info *sp, char *str) 
{
  gtk_entry_set_text(GTK_ENTRY(w_snd_minibuffer(sp)),str);
}

void make_minibuffer_label(snd_info *sp,char *str)
{
  GtkWidget *button;
  GtkStyle *style;
  snd_state *ss;
  ss = sp->state;
  button = w_snd_minibuffer_label(sp);
  style = gtk_style_copy(gtk_widget_get_style(button));
  style->font = (ss->sgx)->button_fnt;
  gtk_widget_set_style(button,style);
  gtk_label_set_text(GTK_LABEL(button),str);
  /* gtk_widget_queue_draw(button); */
}

static void minibuffer_activate_callback(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_state *ss;
  ss = sp->state;
  ss->mx_sp = sp; 
  snd_minibuffer_activate(sp,0);
  (sp->sgx)->mini_active = 1;
}

static gint minibuffer_key_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  chan_info *cp;
  snd_state *ss;
  ss = sp->state;
  if (((sp->sgx)->mini_active == 0) || 
      (((event->keyval == snd_K_s) || (event->keyval == snd_K_r)) && (event->state & snd_ControlMask)))
    {
      cp = current_channel(ss);
      graph_key_press(channel_graph(cp),event,(gpointer)cp); 
      gtk_signal_emit_stop_by_name(GTK_OBJECT(w),"key_press_event");
      return(TRUE);
    }
  return(FALSE);
}

static void minibuffer_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_state *ss;
  if ((sp) && (sp->inuse))
    {
      ss = sp->state;
      if (ss)
	{
	  set_text_background(w,(ss->sgx)->white);
	  (sp->sgx)->mini_active = 1;
	}
    }
}

static void minibuffer_mouse_leave(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_state *ss;
  if ((sp) && (sp->inuse))
    {
      ss = sp->state;
      if (ss)
	{
	  /* weird -- I'm getting this event sent to an inactive sound?? */
	  set_text_background(w,(ss->sgx)->basic_color);
	  (sp->sgx)->mini_active = 0;
	}
    }
}


/* -------- PLAY BUTTON -------- */

void set_play_button(snd_info *sp, int val)
{
  if (!(IS_PLAYER(sp)))
    {
      set_toggle_button(w_snd_play(sp),val,FALSE,(void *)sp);
      set_file_browser_play_button(sp->shortname,val);
    }
}

static int last_play_state = 0;

static void play_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_play_state = ev->state;
}

static void play_button_click_callback(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  chan_info *cp;
  snd_state *ss;
  int i,on;
  on = (GTK_TOGGLE_BUTTON(w)->active);
  if (sp->playing) 
    {
      if (sp->cursor_follows_play != DONT_FOLLOW)
	{
	  for (i=0;i<sp->nchans;i++)
	    {
	      cp = sp->chans[i];
	      cp->original_cursor = cp->cursor;
	    }
	}
      stop_playing_sound_no_toggle(sp);
    }
  if (sp->cursor_follows_play != FOLLOW_ALWAYS)         /* can be set in init file */
    {
      if ((on) && (last_play_state & (snd_ControlMask | snd_MetaMask)))
	sp->cursor_follows_play = FOLLOW_ONCE;
      else sp->cursor_follows_play = DONT_FOLLOW;
    }
  set_file_browser_play_button(sp->shortname,on);
  cp = any_selected_channel(sp);
  goto_graph(cp);
  if ((!(cp->cursor_on)) && (sp->cursor_follows_play != DONT_FOLLOW))
    for (i=0;i<sp->nchans;i++)
      {
	cp = sp->chans[i];
	cp->cursor_on = 1;
      }
  if (on) 
    {
      ss = sp->state;
      if (sp->cursor_follows_play != DONT_FOLLOW) 
	set_active_color(w,(ss->sgx)->green); 
      else set_active_color(w,(ss->sgx)->pushed_button_color);
      play_sound(sp,0,NO_END_SPECIFIED,IN_BACKGROUND); /* should this follow the sync button? */
    }
}

typedef struct {int pausing; snd_state *ss;} pause_data;

static int set_play_button_pause(snd_info *sp, void *ptr)
{
  pause_data *pd = (pause_data *)ptr;
  snd_state *ss;
  GtkWidget *w;
  if (sp->playing)
    {
      ss = pd->ss;
      w = w_snd_play(sp);
      if (pd->pausing)
	set_active_color(w,(ss->sgx)->red);
      else 
	if (sp->cursor_follows_play != DONT_FOLLOW) 
	  set_active_color(w,(ss->sgx)->green); 
	else set_active_color(w,(ss->sgx)->pushed_button_color);
    }
  return(0);
}

void play_button_pause(snd_state *ss, int pausing)
{
  pause_data *pd;
  pd = (pause_data *)CALLOC(1,sizeof(pause_data));
  pd->pausing = pausing;
  pd->ss = ss;
  map_over_sounds(ss,set_play_button_pause,(void *)pd);
  FREE(pd);
}

static void set_sync_color(snd_info *sp)
{
  snd_state *ss;
  GtkWidget *syb;
  syb = w_snd_sync(sp);
  ss = sp->state;
  switch (sp->syncing)
    {
    case 1: case 0: set_active_color(syb,(ss->sgx)->pushed_button_color); break;
    case 2: set_active_color(syb,(ss->sgx)->green); break;
    case 3: set_active_color(syb,(ss->sgx)->yellow); break;
    case 4: set_active_color(syb,(ss->sgx)->red); break;
    default: set_active_color(syb,(ss->sgx)->black); break;
    }
}

void syncb(snd_info *sp, int on)
{
  sp->syncing = on;
  if (!(IS_PLAYER(sp)))
    {
      set_sync_color(sp);
      set_toggle_button(w_snd_sync(sp),(on == 0) ? FALSE : TRUE,FALSE,(void *)sp);
    }
}

static int last_sync_state = 0;

static void sync_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_sync_state = ev->state;
}

static void sync_button_click(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  chan_info *cp;
  int on;
  on = (GTK_TOGGLE_BUTTON(w)->active);
  if (on)
    if (last_sync_state & snd_ControlMask) 
      if (last_sync_state & snd_MetaMask)
	if (last_sync_state & snd_ShiftMask)
	  sp->syncing = 4;
	else sp->syncing = 3;
      else sp->syncing = 2;
    else sp->syncing = 1;
  else sp->syncing = 0;
  if (sp->syncing != 0) 
    {
      set_sync_color(sp);
      cp = sp->lacp;
      if (cp == NULL) cp = any_selected_channel(sp);
      goto_graph(cp);
      if (cp->cursor_on) cursor_moveto(cp,cp->cursor);
      apply_x_axis_change(cp->axis,cp,sp);
    }
}

static int last_combine_state = 0;

static void combine_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* click if set unsets, click if unset->combine, ctrl-click->superimpose */
  last_combine_state = ev->state;
}

static void combine_button_click(GtkWidget *w, gpointer data)
{
  int val,on;
  snd_info *sp = (snd_info *)data;
  on = (GTK_TOGGLE_BUTTON(w)->active);
  if (on)
    {
      if (last_combine_state & (snd_ControlMask | snd_MetaMask)) 
	val = CHANNELS_SUPERIMPOSED;
      else val = CHANNELS_COMBINED;
    }
  else val = CHANNELS_SEPARATE;
  combineb(sp,val);
  last_combine_state = 0;
}



/* -------- AMP CALLBACKS -------- */

static void name_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  sp_name_click((snd_info *)data);
}

static char semitone_one[5]={' ',' ',' ','0','\0'};
static char ratio_one[5]={' ','1','/','1','\0'};

static char amp_number_buffer[5]={'1',STR_decimal,'0','0','\0'};

static void set_snd_amp_1(snd_info *sp, Float amp, int setadj)
{
  /* amp is exponential between .17 and around 7 or 8 with 1.0 at scroll midpoint */
  /*     is linear below .17, scroll-relative .15 */
  /* here we get the user-view and fixup the scrollbar */
  Float scrollval;
  char *sfs;
  GtkObject *adj;
  sp->amp = amp;
  if (amp <= 0.0)
    scrollval = 0.0;
  else
    {
      if (amp < .173)
	scrollval = amp * .867;
      else scrollval = (log(amp)*0.2 + 0.5);
      /* TODO: max? */
    }
  sfs=prettyf(sp->amp,2);
  fill_number(sfs,amp_number_buffer);
  gtk_label_set_text(GTK_LABEL(w_snd_amp_number(sp)),amp_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = w_amp_adj(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_amp(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->amp = amp;
  else set_snd_amp_1(sp,amp,TRUE);
}

static Float get_snd_amp(Float scrollval)
{
  if (scrollval < .15)
    return(scrollval * 1.13);
  else return(exp((scrollval - 0.5) * 5.0));
}

static void amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_amp(sp,sp->last_amp);
  else set_snd_amp(sp,1.0);
}

static void amp_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_amp_1((snd_info *)data,get_snd_amp((Float)(adj->value)),FALSE);
}

static void amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_amp = sp->saved_amp;
  sp->saved_amp = sp->amp;
}


/* -------- SRATE CALLBACKS -------- */

static char srate_number_buffer[5]={'1',STR_decimal,'0','0','\0'};

void set_snd_srate(snd_info *sp, Float amp)
{
  Float scrollval;
  GtkObject *adj;
  sp->srate = amp;
  if (!(IS_PLAYER(sp)))
    {
      if (amp > 0.0)
	scrollval = .45 + .15 * log(amp);
      else scrollval = 0.0;
      sprintf(srate_number_buffer,"%.3f",amp);
      gtk_label_set_text(GTK_LABEL(w_snd_srate_number(sp)),srate_number_buffer);
      adj = w_srate_adj(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

static void srate_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_srate(sp,sp->last_srate);
  else set_snd_srate(sp,1.0);
}

static void srate_changed_callback(GtkAdjustment *adj, gpointer data)
{
  Float scrollval,val;
  snd_info *sp = (snd_info *)data;
  val = srate_changed(exp((GTK_ADJUSTMENT(adj)->value-.45)/.15),srate_number_buffer,sp->speed_style,sp->speed_tones);
  sp->srate = val;
  if (val > 0.0)
    scrollval = .45 + .15 * log(val);
  else scrollval = 0.0;
  gtk_label_set_text(GTK_LABEL(w_snd_srate_number(sp)),srate_number_buffer);
}

static void srate_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_srate = sp->saved_srate;
  sp->saved_srate = sp->srate;
}

static void srate_arrow_callback(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (sp->play_direction == 1)
    {
      sp->play_direction = -1;
      gtk_pixmap_set(GTK_PIXMAP(w_snd_srate_pix(sp)),speed_l,speed_l_mask);
    }
  else
    {
      sp->play_direction = 1;
      gtk_pixmap_set(GTK_PIXMAP(w_snd_srate_pix(sp)),speed_r,speed_r_mask);
    }
}

void toggle_direction_arrow(snd_info *sp, int state)
{
  int dir = 1;
  if (state) dir = -1;
  if ((sp->play_direction != dir) && (!(IS_PLAYER(sp))))
    {
      if (dir == 1)
	gtk_pixmap_set(GTK_PIXMAP(w_snd_srate_pix(sp)),speed_r,speed_r_mask);
      else gtk_pixmap_set(GTK_PIXMAP(w_snd_srate_pix(sp)),speed_l,speed_l_mask);
    }
  sp->play_direction = dir;
}


/* -------- EXPAND CALLBACKS -------- */

static char expand_number_buffer[5]={'1',STR_decimal,'0','0','\0'};

static void set_snd_expand_1(snd_info *sp, Float expand, int setadj)
{
  Float scrollval;
  char *sfs;
  GtkObject *adj;
  sp->expand = expand;
  if (sp->playing) dac_set_expand(sp, sp->expand);
  if (expand < .1)
    scrollval = expand * 1.03;
  else scrollval = .45 + .15 * log(expand);
  sfs=prettyf(sp->expand,2);
  fill_number(sfs,expand_number_buffer);
  gtk_label_set_text(GTK_LABEL(w_snd_expand_number(sp)),expand_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = w_expand_adj(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_expand(snd_info *sp, Float val) 
{
  if (IS_PLAYER(sp))
    sp->expand = val;
  else set_snd_expand_1(sp,val,TRUE);
}

static Float get_snd_expand(Float scrollval)
{
  if (scrollval < .1)
    return(scrollval * .9697);
  else return(exp((scrollval - 0.45)/.15));
}

static void expand_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_expand_1((snd_info *)data,get_snd_expand((Float)(adj->value)),FALSE);
}

static void expand_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_expand = sp->saved_expand;
  sp->saved_expand = sp->expand;
}

static void expand_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_expand(sp,sp->last_expand);
  else set_snd_expand(sp,1.0);
}

static void expand_button_callback(GtkWidget *w, gpointer clientData)
{
  snd_state *ss;
  snd_info *sp = (snd_info *)clientData;
  ss = sp->state;
  sp->expanding = GTK_TOGGLE_BUTTON(w)->active;
 if (sp->expanding) 
   set_background(w_snd_expand(sp),(ss->sgx)->position_color);
 else set_background(w_snd_expand(sp),(ss->sgx)->basic_color);
}

void toggle_expand_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->expanding = state;
  else set_toggle_button(w_snd_expand_button(sp),state,TRUE,(void *)sp);
}


/* -------- CONTRAST CALLBACKS -------- */

static char contrast_number_buffer[5]={'0',STR_decimal,'0','0','\0'};

static void set_snd_contrast_1(snd_info *sp, Float val, int setadj)
{
  Float scrollval;
  char *sfs;
  GtkObject *adj;
  sp->contrast = val;
  scrollval = val/10.0;
  sfs=prettyf(sp->contrast,2);
  fill_number(sfs,contrast_number_buffer);
  gtk_label_set_text(GTK_LABEL(w_snd_contrast_number(sp)),contrast_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = w_contrast_adj(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_contrast(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->contrast = amp;
  else set_snd_contrast_1(sp,amp,TRUE);
}

static Float get_snd_contrast(Float scrollval)
{
  return(scrollval * 10.0);
}

static void contrast_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_contrast(sp,sp->last_contrast);
  else set_snd_contrast(sp,1.0);
}

static void contrast_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_contrast_1((snd_info *)data,get_snd_contrast((Float)(adj->value)),FALSE);
}

static void contrast_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_contrast = sp->saved_contrast;
  sp->saved_contrast = sp->contrast;
}

static void contrast_button_callback(GtkWidget *w, gpointer clientData)
{
  snd_state *ss;
  snd_info *sp = (snd_info *)clientData;
  ss = sp->state;
  sp->contrasting = GTK_TOGGLE_BUTTON(w)->active;
 if (sp->contrasting) 
   set_background(w_snd_contrast(sp),(ss->sgx)->position_color);
 else set_background(w_snd_contrast(sp),(ss->sgx)->basic_color);
}


void toggle_contrast_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->contrasting = state;
  else set_toggle_button(w_snd_contrast_button(sp),state,TRUE,(void *)sp);
}


/* -------- REVERB CALLBACKS -------- */

static char revscl_number_buffer[7]={'0',STR_decimal,'0','0','0','0','\0'};

static void set_snd_revscl_1(snd_info *sp, Float val, int setadj)
{
  Float scrollval;
  char *fs,*ps,*sfs;
  int i,j;
  GtkObject *adj;
  sp->revscl = val;
  scrollval = pow(val,.333)*.6;
  sfs=prettyf(sp->revscl,3);
  fs=sfs;
  ps=(char *)(revscl_number_buffer);
  j=strlen(fs);
  if (j>6) j=6;
  if (j<6) 
    {
      revscl_number_buffer[5]='0';
      revscl_number_buffer[4]='0'; 
      revscl_number_buffer[3]='0';
      revscl_number_buffer[2]='0'; 
      revscl_number_buffer[1]=STR_decimal;
    }
  for (i=0;i<j;i++) (*ps++) = (*fs++);
  gtk_label_set_text(GTK_LABEL(w_snd_revscl_number(sp)),revscl_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = w_revscl_adj(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_revscl(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->revscl = amp;
  else set_snd_revscl_1(sp,amp,TRUE);
}

static Float get_snd_revscl(Float scrollval)
{
  return(pow(scrollval*1.666,3.0));
}

static void revscl_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_revscl(sp,sp->last_revlen);
  else set_snd_revscl(sp,1.0);
}

static void revscl_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_revscl_1((snd_info *)data,get_snd_revscl((Float)(adj->value)),FALSE);
}

static void revscl_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_revscl = sp->saved_revscl;
  sp->saved_revscl = sp->revscl;
}


static char revlen_number_buffer[5]={'1',STR_decimal,'0','0','\0'};

static void set_snd_revlen_1(snd_info *sp, Float val, int setadj)
{
  Float scrollval;
  char *sfs;
  GtkObject *adj;
  sp->revlen = val;
  scrollval = val/5.0;
  sfs=prettyf(sp->revlen,2);
  fill_number(sfs,revlen_number_buffer);
  gtk_label_set_text(GTK_LABEL(w_snd_revlen_number(sp)),revlen_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = w_revlen_adj(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_revlen(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->revlen = amp;
  else set_snd_revlen_1(sp,amp,TRUE);
}

static Float get_snd_revlen(Float scrollval)
{
  return(scrollval * 5.0);
}

static void revlen_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_revlen(sp,sp->last_revlen);
  else set_snd_revlen(sp,1.0);
}

static void revlen_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_revlen_1((snd_info *)data,get_snd_revlen((Float)(adj->value)),FALSE);
}

static void revlen_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_revlen = sp->saved_revlen;
  sp->saved_revlen = sp->revlen;
}

static void reverb_button_callback(GtkWidget *w, gpointer clientData)
{
  snd_state *ss;
  snd_info *sp = (snd_info *)clientData;
  ss = sp->state;
  sp->reverbing = GTK_TOGGLE_BUTTON(w)->active;
 if (sp->reverbing)
   {
     set_background(w_snd_revscl(sp),(ss->sgx)->position_color);
     set_background(w_snd_revlen(sp),(ss->sgx)->position_color);
   }
 else 
   {
     set_background(w_snd_revscl(sp),(ss->sgx)->basic_color);
     set_background(w_snd_revlen(sp),(ss->sgx)->basic_color);
   }
}

void toggle_reverb_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->reverbing = state;
  else set_toggle_button(w_snd_reverb_button(sp),state,TRUE,(void *)sp);
}


/* -------- FILTER CALLBACKS -------- */

#define MIN_FILTER_GRAPH_HEIGHT 20

void sp_display_env(snd_info *sp)
{
  snd_state *ss;
  axis_context *ax;
  int height,width;
  GtkWidget *drawer;
  if (IS_PLAYER(sp)) return;
  ss = sp->state;
  if (ss == NULL) return;
  drawer = w_snd_filter_env(sp);
  height = widget_height(drawer);
  if (height < MIN_FILTER_GRAPH_HEIGHT) return;
  width = widget_width(drawer);
  ax = (axis_context *)CALLOC(1,sizeof(axis_context));
  ax->gc = (ss->sgx)->fltenv_basic_gc;
  ax->wn = drawer->window;
  gdk_window_clear(ax->wn);
  display_filter_graph(ss,sp,ax,width,height);
  FREE(ax);
}

static void filter_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  int evx,evy;
  GdkModifierType state;
  if (ev->state & GDK_BUTTON1_MASK)
    {
      if (ev->is_hint)
	{
	  gdk_window_get_pointer(ev->window,&evx,&evy,&state);
	}
      else
	{
	  evx = (int)(ev->x);
	  evy = (int)(ev->y);
	}
    }
  else return;
  handle_filter_point(sp->state,sp,evx,evy,ev->time);
}

static void filter_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  handle_filter_press((snd_info *)data,(int)(ev->x),(int)(ev->y), ev->time);
}

static void filter_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  handle_filter_release((snd_info *)data);
}

void set_filter_text(snd_info *sp, char *str)
{
  if (!(IS_PLAYER(sp)))
    gtk_entry_set_text(GTK_ENTRY(w_snd_filter(sp)),str);
}

static void filter_drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp_display_env(sp);
}

static void filter_drawer_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp_display_env(sp);
}

static void filter_button_callback(GtkWidget *w, gpointer clientData)
{
  snd_info *sp = (snd_info *)clientData;
  sp->filtering = GTK_TOGGLE_BUTTON(w)->active;
}

void toggle_filter_button(snd_info *sp, int state)
{
  if (IS_PLAYER(sp))
    sp->filtering = state;
  else set_toggle_button(w_snd_filter_button(sp),state,TRUE,(void *)sp);
}

static void filter_db_callback(GtkWidget *w, gpointer clientData)
{
  snd_info *sp = (snd_info *)clientData;
  sp->filter_dBing = GTK_TOGGLE_BUTTON(w)->active;
  sp_display_env(sp);
}

void set_filter_dBing(snd_info *sp, int val)
{
  sp->filter_dBing = val;
  if (!(IS_PLAYER(sp)))
    {
      set_toggle_button(w_snd_filter_dB(sp),val,FALSE,(void *)sp);
      sp_display_env(sp);
    }
}

static void set_snd_filter_order_1(snd_info *sp, int order, int setadj)
{
  sp->filter_order = order;
  if (setadj)
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(w_snd_filter_order(sp)),(gfloat)order);
  sp_display_env(sp);
  sp->filter_changed = 1;
}  

void set_snd_filter_order(snd_info *sp, int order) 
{
  if (IS_PLAYER(sp))
    sp->filter_order = order;
  else set_snd_filter_order_1(sp,order,TRUE);
}

static void filter_order_callback(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->filter_order = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(w_snd_filter_order(sp)));
  set_snd_filter_order_1(sp,sp->filter_order,FALSE);
}

static void filter_activate_callback(GtkWidget *w, gpointer clientData)
{
  /* make an envelope out of the data */
  snd_info *sp = (snd_info *)clientData;
  char *str=NULL;
  str = gtk_entry_get_text(GTK_ENTRY(w));
  if (sp->filter_env) free_env(sp->filter_env);
  sp->filter_env = string2env(str);
  if (!(sp->filter_env)) /* maybe user cleared text field? */
    sp->filter_env = default_env(1.0);
  report_filter_edit(sp);
  sp_display_env(sp);
  sp->filter_changed = 1;
}

void filter_env_changed(snd_info *sp, env *e)
{
  /* turn e back into a string for textfield widget */
  char *tmpstr;
  if (!(IS_PLAYER(sp)))
    {
      tmpstr=env_to_string(e);
      gtk_entry_set_text(GTK_ENTRY(w_snd_filter(sp)),tmpstr);
      if (tmpstr) FREE(tmpstr);
      sp_display_env(sp);
      /* this is called also from snd-scm.c */
    }
  sp->filter_changed = 1;
}

void color_filter_waveform(snd_state *ss, GdkColor *color)
{
  int i;
  snd_info *sp;
  gdk_gc_set_foreground((ss->sgx)->fltenv_data_gc,color);
  (ss->sgx)->filter_waveform_color = color;
  for (i=0;i<ss->max_sounds;i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse)) sp_display_env(sp);
    }
}


/* -------- APPLY CALLBACKS -------- */

static int last_apply_state = 0;

static void apply_button_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_apply_state = ev->state;
}

static void apply_button_callback(GtkWidget *w, gpointer clientData)
{
  /* create temp file of run over current file using the current (saved) ctrls state */
  snd_info *sp = (snd_info *)clientData;
  snd_state *ss;
  snd_context *sgx;
  sgx = sp->sgx;
  ss = sp->state;
  if (sp->applying) 
    {
      stop_applying(sp);
      set_background(w_snd_apply(sp),(ss->sgx)->basic_color);
      sp->applying = FALSE;
    }
  else
    {
      ss->apply_choice = APPLY_TO_SOUND;

      if (last_apply_state & snd_ControlMask) 
	{
	  if (selection_is_current())
	    ss->apply_choice = APPLY_TO_SELECTION;
	  else ss->apply_choice = APPLY_TO_CHANNEL;
	}
      sp->applying = TRUE;
      set_background(w_snd_apply(sp),(ss->sgx)->pushed_button_color);
      sgx->apply_in_progress = BACKGROUND_ADD(ss,apply_controls,(GUI_POINTER)(make_apply_state(sp)));
    }
}

/* apply is only safe if the DAC is currently inactive and remains safe only
 * if all other apply buttons are locked out (and play).
 */

static int lockapply(snd_info *sp, void *up) 
{
  if (sp != up) set_sensitive(w_snd_apply(sp),FALSE);
  return(0);
}

void lock_apply(snd_state *ss, snd_info *sp)
{
  /* if playing or applying, set other applys to insensitive */
  map_over_sounds(ss,lockapply,(void *)sp);
}

static int unlockapply(snd_info *sp, void *up) 
{
  set_sensitive(w_snd_apply(sp),TRUE);
  return(0);
}

void unlock_apply(snd_state *ss,snd_info *sp)
{
  map_over_sounds(ss,unlockapply,(void *)sp);
  if (sp) set_background(w_snd_apply(sp),(ss->sgx)->basic_color);
}

static void remember_button_callback(GtkWidget *w, gpointer clientData) {save_control_panel((snd_info *)clientData);}
static void restore_button_callback(GtkWidget *w, gpointer clientData) {restore_control_panel((snd_info *)clientData);}
static void reset_button_callback(GtkWidget *w, gpointer clientData) {reset_control_panel((snd_info *)clientData);}


/* -------- AMP ENVS ETC -------- */

static int cant_write(char *name)
{
#if HAVE_ACCESS
  return((access(name,W_OK)) != 0);
#else
  return(0);
#endif
}

void reflect_amp_env_completion(snd_info *sp)
{
  chan_info *cp;
  env_info *ep;
  int i;
  GtkWidget *info_sep;
  /* a channel completed an amp env, check to see if all are complete */
  for (i=0;i<sp->nchans;i++)
    {
      cp = sp->chans[i];
      if (!(cp->amp_envs)) return;
      ep = cp->amp_envs[cp->edit_ctr];
      if (!ep) return;
      if (!(ep->completed)) return;
    }
  info_sep = w_snd_minibuffer_sep(sp);
  if (info_sep) gtk_widget_show(info_sep);
  alert_enved_amp_env(sp);
#if DEBUGGING
  /* this finishes timing the amp env creation process, started below by start_timing -- it will print millisecs to stderr */
  /* stop_timing(); */
#endif
}

void reflect_amp_env_in_progress(snd_info *sp)
{
  GtkWidget *info_sep;
  info_sep = w_snd_minibuffer_sep(sp);
  if (info_sep) gtk_widget_hide(info_sep);
#if DEBUGGING
  /* start_timing(); */
#endif
}

static gint Close_Sound_Dialog(GtkWidget *w, GdkEvent *event, gpointer clientData)
{
  snd_info *sp = (snd_info *)clientData;
  finish_keyboard_selection();
  if (sp) snd_close_file(sp,sp->state);
  gtk_widget_hide(sp->sgx->dialog); 
  return(TRUE);
} 


/* -------- SOUND PANE -------- */

snd_info *add_sound_window(char *filename, snd_state *ss)
{
  snd_info *sp,*osp;
  file_info *hdr;
  GtkWidget **sw;
  GtkObject **adjs;
  GtkWidget *tablab;
  int snd_slot,nchans,make_widgets,i,k,old_chans;
  char *old_name = NULL,*title;
  int app_y,app_dy,screen_y,chan_min_y;
  /* these dimensions are used to try to get a reasonable channel graph size without falling off the screen bottom */
  int samples_per_channel;
  snd_context *sx;
#if FILE_PER_CHAN
  dir *file_chans = NULL;
  int *chan_locs;
  multifile_info *minfo;
#endif
  set_snd_IO_error(SND_NO_ERROR);
  errno = 0;
#if FILE_PER_CHAN
  if (is_directory(filename))
    {
      minfo = sort_multifile_channels(ss,filename);
      file_chans = minfo->file_chans;
      chan_locs = minfo->chan_locs;
      hdr = minfo->hdr;
      FREE(minfo);
    }
  else
    {
      hdr = make_file_info(filename,ss);
      if (hdr) hdr->chan_type = FILE_PER_SOUND; else return(NULL);
    }
#else
  hdr = make_file_info(filename,ss);
#endif
  if (!hdr) return(NULL);
  if (ss->pending_change) 
    {
      old_name = filename;
      filename = ss->pending_change;
      ss->pending_change = NULL;
    }
  nchans = hdr->chans;
  samples_per_channel = hdr->samples/nchans;

  app_y = widget_y(MAIN_SHELL(ss));
  app_dy = widget_height(MAIN_SHELL(ss));
  screen_y = gdk_screen_height();
  app_dy = (screen_y - app_y - app_dy - 20*nchans);
  chan_min_y = app_dy/nchans;
  if (chan_min_y > (ss->channel_min_height)) chan_min_y = ss->channel_min_height; else if (chan_min_y < 5) chan_min_y = 5;

  snd_slot = find_free_sound_slot(ss,nchans); /* expands sound list if needed */
  if (ss->sounds[snd_slot]) /* we're trying to re-use an old, inactive set of widgets and whatnot */
    {
      osp = ss->sounds[snd_slot];
      old_chans = osp->allocated_chans;
    }
  else old_chans = 0;
  make_widgets = (ss->sounds[snd_slot] == NULL);
  ss->sounds[snd_slot] = make_snd_info(ss->sounds[snd_slot],ss,filename,hdr,snd_slot);
  sp = ss->sounds[snd_slot];
  sp->inuse = 1;
#if FILE_PER_CHAN
  sp->chan_type = hdr->chan_type;
  if (sp->chan_type == FILE_PER_CHANNEL)
    {
      sp->channel_filenames = (char **)CALLOC(hdr->chans,sizeof(char *));
      for (i=0;i<hdr->chans;i++)
	{
	  sp->channel_filenames[i] = (char *)CALLOC(MUS_MAX_FILE_NAME,sizeof(char));
	  sprintf(sp->channel_filenames[i],"%s/%s",filename,file_chans->files[chan_locs[i]]);
	}
      FREE(chan_locs);
      chan_locs = NULL;
      free_dir(file_chans);
      file_chans = NULL;
    }
#endif
  sx = sp->sgx;
  sx->controls_fixed = 0;
  sx->file_pix = blank;
  sp->bomb_ctr = 0;
  make_pixmaps(ss);

  if (sx->snd_widgets == NULL)
    {
      sw = (GtkWidget **)CALLOC(NUM_SND_WIDGETS,sizeof(GtkWidget *));
      adjs = (GtkObject **)CALLOC(NUM_SND_ADJS,sizeof(GtkObject *));
      sx->snd_widgets = sw; 
      sx->snd_adjs = adjs;
    }
  else
    {
      sw = sx->snd_widgets;
      adjs = sx->snd_adjs;
    }

  if ((!make_widgets) && (old_chans < nchans))
    {
      for (i=old_chans;i<nchans;i++) add_channel_window(sp,i,ss,chan_min_y,1,NULL,WITH_FW_BUTTONS);
    }

  if (make_widgets)
    {
      sw[W_pane] = gtk_vpaned_new();
      set_backgrounds(sw[W_pane],(ss->sgx)->sash_color);

      gtk_paned_set_gutter_size(GTK_PANED(sw[W_pane]),8);
      gtk_paned_set_handle_size(GTK_PANED(sw[W_pane]),ss->sash_size);
      gtk_container_set_border_width(GTK_CONTAINER(sw[W_pane]),0);
      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	{
	  sx->dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	  set_background(sx->dialog,(ss->sgx)->basic_color);
	  gtk_window_set_policy(GTK_WINDOW(sx->dialog),TRUE,TRUE,FALSE); /* allow shrink or grow */
	  gtk_container_add(GTK_CONTAINER(sx->dialog),sw[W_pane]);
	  gtk_widget_show(sx->dialog);
	  add_dialog(ss,sx->dialog);
	  gtk_signal_connect(GTK_OBJECT(sx->dialog),"delete_event",GTK_SIGNAL_FUNC(Close_Sound_Dialog),(gpointer)sp);
	}
      else
	{
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	    {
	      tablab = gtk_label_new(sp->shortname);
	      gtk_widget_show(tablab);
	      gtk_notebook_append_page(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)),sw[W_pane],tablab);
	    }
	  else gtk_box_pack_start(GTK_BOX(SOUND_PANE_BOX(ss)),sw[W_pane],TRUE,TRUE,0);
	  /* child2 is listener */
	}
      sw[W_pane_box] = gtk_vbox_new(FALSE,0);
      gtk_paned_add1(GTK_PANED(sw[W_pane]),sw[W_pane_box]);
      gtk_widget_show(sw[W_pane_box]);

      sw[W_name_form] = gtk_hbox_new(FALSE,0);
      gtk_box_pack_end(GTK_BOX(sw[W_pane_box]),sw[W_name_form],FALSE,FALSE,0);
      
      for (i=0;i<nchans;i++) add_channel_window(sp,i,ss,chan_min_y,0,NULL,WITH_FW_BUTTONS);

      /* controls etc */

      sw[W_ctrls] = gtk_vbox_new(FALSE,0);
      gtk_paned_add2(GTK_PANED(sw[W_pane]),sw[W_ctrls]);
  

      /* -------- NAME FIELDS -------- */

      sw[W_name_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]),sw[W_name_event],FALSE,FALSE,5);
      gtk_widget_show(sw[W_name_event]);
      set_background(sw[W_name_event],(ss->sgx)->highlight_color);
      gtk_signal_connect(GTK_OBJECT(sw[W_name_event]),"button_press_event",GTK_SIGNAL_FUNC(name_click_callback),(gpointer)sp);
      
      sw[W_name] = gtk_label_new(shortname_indexed(sp));
      gtk_container_add(GTK_CONTAINER(sw[W_name_event]),sw[W_name]);
      gtk_widget_show(sw[W_name]);
      
      sw[W_name_icon] = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]),sw[W_name_icon],FALSE,FALSE,0);
      gtk_button_set_relief(GTK_BUTTON(sw[W_name_icon]),GTK_RELIEF_NONE);
      set_background(sw[W_name_icon],(ss->sgx)->basic_color);
      gtk_widget_show(sw[W_name_icon]);
      
      sw[W_name_pix] = gtk_pixmap_new(blank,blank_mask);
      gtk_container_add(GTK_CONTAINER(sw[W_name_icon]),sw[W_name_pix]);
      gtk_widget_show(sw[W_name_pix]);
      
      sw[W_info_sep] = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]),sw[W_info_sep],FALSE,FALSE,4);
      gtk_widget_show(sw[W_info_sep]);
      
      sw[W_info_label] = gtk_label_new(NULL);
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]),sw[W_info_label],FALSE,FALSE,0);
      gtk_widget_show(sw[W_info_label]);
      
      sw[W_info] = gtk_entry_new();
      gtk_entry_set_editable(GTK_ENTRY(sw[W_info]),TRUE);
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]),sw[W_info],TRUE,TRUE,2);
      gtk_signal_connect(GTK_OBJECT(sw[W_info]),"key_press_event",GTK_SIGNAL_FUNC(minibuffer_key_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_info]),"activate",GTK_SIGNAL_FUNC(minibuffer_activate_callback),(gpointer)sp);

      gtk_signal_connect(GTK_OBJECT(sw[W_info]),"enter_notify_event",GTK_SIGNAL_FUNC(minibuffer_mouse_enter),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_info]),"leave_notify_event",GTK_SIGNAL_FUNC(minibuffer_mouse_leave),(gpointer)sp);

      set_background(sw[W_info],(ss->sgx)->basic_color);
      set_text_background(sw[W_info],(ss->sgx)->basic_color);
      gtk_widget_show(sw[W_info]);
      
      /* now fill from other end */
      
      sw[W_play] = gtk_check_button_new_with_label(STR_play);
      gtk_box_pack_end(GTK_BOX(sw[W_name_form]),sw[W_play],FALSE,FALSE,0);
      set_pushed_button_colors(sw[W_play],ss);
      gtk_signal_connect(GTK_OBJECT(sw[W_play]),"button_press_event",GTK_SIGNAL_FUNC(play_button_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_play]),"key_press_event",GTK_SIGNAL_FUNC(graph_key_press),(gpointer)(any_selected_channel(sp)));
      gtk_signal_connect(GTK_OBJECT(sw[W_play]),"toggled",GTK_SIGNAL_FUNC(play_button_click_callback),(gpointer)sp);
      gtk_widget_show(sw[W_play]);
      
      sw[W_sync] = gtk_check_button_new_with_label(STR_sync);
      gtk_box_pack_end(GTK_BOX(sw[W_name_form]),sw[W_sync],FALSE,FALSE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_sync]),"button_press_event",GTK_SIGNAL_FUNC(sync_button_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_sync]),"key_press_event",GTK_SIGNAL_FUNC(graph_key_press),(gpointer)(any_selected_channel(sp)));
      gtk_signal_connect(GTK_OBJECT(sw[W_sync]),"toggled",GTK_SIGNAL_FUNC(sync_button_click),(gpointer)sp);
      gtk_widget_show(sw[W_sync]);
      
      sw[W_combine] = gtk_check_button_new_with_label(STR_unite);
      gtk_box_pack_end(GTK_BOX(sw[W_name_form]),sw[W_combine],FALSE,FALSE,0);
      set_pushed_button_colors(sw[W_combine],ss);
      gtk_signal_connect(GTK_OBJECT(sw[W_combine]),"button_press_event",GTK_SIGNAL_FUNC(combine_button_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_combine]),"toggled",GTK_SIGNAL_FUNC(combine_button_click),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_combine]),"key_press_event",GTK_SIGNAL_FUNC(graph_key_press),(gpointer)(any_selected_channel(sp)));
      gtk_widget_show(sw[W_combine]);
      
      gtk_widget_show(sw[W_name_form]);

      /* if control-panel */
      
      /* -------- AMP FIELDS -------- */
      
      sw[W_amp_sep] = gtk_hseparator_new();
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_amp_sep],FALSE,FALSE,4);
      gtk_widget_show(sw[W_amp_sep]);
      
      sw[W_amp_form] = gtk_hbox_new(FALSE,2);
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_amp_form],FALSE,FALSE,0);
      set_background(sw[W_amp_form],(ss->sgx)->basic_color);
      
      sw[W_amp_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]),sw[W_amp_event],FALSE,FALSE,4);
      gtk_widget_show(sw[W_amp_event]);
      gtk_signal_connect(GTK_OBJECT(sw[W_amp_event]),"button_press_event",GTK_SIGNAL_FUNC(amp_click_callback),(gpointer)sp);
      set_background(sw[W_amp_event],(ss->sgx)->basic_color);
      
      sw[W_amp_label] = gtk_label_new(STR_amp_p);
      gtk_container_add(GTK_CONTAINER(sw[W_amp_event]),sw[W_amp_label]);
      gtk_widget_show(sw[W_amp_label]);
      
      sw[W_amp_number] = gtk_label_new(amp_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]),sw[W_amp_number],FALSE,FALSE,0);
      gtk_widget_show(sw[W_amp_number]);
      
      adjs[W_amp_adj] = gtk_adjustment_new(0.5,0.0,1.0,0.001,0.01,.1);
      sw[W_amp] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_amp_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]),sw[W_amp],TRUE,TRUE,4);
      gtk_signal_connect(GTK_OBJECT(adjs[W_amp_adj]),"value_changed",GTK_SIGNAL_FUNC(amp_changed_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_amp]),"button_release_event",GTK_SIGNAL_FUNC(amp_release_callback),(gpointer)sp);
      gtk_widget_show(sw[W_amp]);
      
      gtk_widget_show(sw[W_amp_form]);
      
      
      /* -------- SRATE FIELDS -------- */
      
      sw[W_srate_form] = gtk_hbox_new(FALSE,2);
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_srate_form],FALSE,FALSE,0);
      set_background(sw[W_srate_form],(ss->sgx)->basic_color);
      
      sw[W_srate_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_srate_form]),sw[W_srate_event],FALSE,FALSE,4);
      gtk_widget_show(sw[W_srate_event]);
      gtk_signal_connect(GTK_OBJECT(sw[W_srate_event]),"button_press_event",GTK_SIGNAL_FUNC(srate_click_callback),(gpointer)sp);
      set_background(sw[W_srate_event],(ss->sgx)->basic_color);
      
      sw[W_srate_label] = gtk_label_new(STR_speed);
      gtk_container_add(GTK_CONTAINER(sw[W_srate_event]),sw[W_srate_label]);
      gtk_widget_show(sw[W_srate_label]);
      
      switch (speed_style(ss))
	{
	case SPEED_AS_RATIO: sw[W_srate_number] = gtk_label_new(ratio_one); break;
	case SPEED_AS_SEMITONE: sw[W_srate_number] = gtk_label_new(semitone_one); break;
	default:  sw[W_srate_number] = gtk_label_new(srate_number_buffer); break;
	}
      gtk_box_pack_start(GTK_BOX(sw[W_srate_form]),sw[W_srate_number],FALSE,FALSE,0);
      gtk_widget_show(sw[W_srate_number]);
      
      adjs[W_srate_adj] = gtk_adjustment_new(0.45,0.0,1.0,0.001,0.01,.1);
      sw[W_srate] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_srate_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_srate_form]),sw[W_srate],TRUE,TRUE,4);
      gtk_signal_connect(GTK_OBJECT(adjs[W_srate_adj]),"value_changed",GTK_SIGNAL_FUNC(srate_changed_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_srate]),"button_release_event",GTK_SIGNAL_FUNC(srate_release_callback),(gpointer)sp);
      gtk_widget_show(sw[W_srate]);
      
      sw[W_srate_arrow] = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_srate_form]),sw[W_srate_arrow],FALSE,FALSE,2);
      gtk_button_set_relief(GTK_BUTTON(sw[W_srate_arrow]),GTK_RELIEF_NONE);
      set_background(sw[W_srate_arrow],(ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(sw[W_srate_arrow]),"clicked",GTK_SIGNAL_FUNC(srate_arrow_callback),(gpointer)sp);
      gtk_widget_show(sw[W_srate_arrow]);
      
      sw[W_srate_pix] = gtk_pixmap_new(speed_r,speed_r_mask);
      gtk_container_add (GTK_CONTAINER(sw[W_srate_arrow]),sw[W_srate_pix]);
      gtk_widget_show(sw[W_srate_pix]);
      
      gtk_widget_show(sw[W_srate_form]);
      
      
      /* -------- EXPAND FIELDS -------- */
      
      sw[W_expand_form] = gtk_hbox_new(FALSE,2);
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_expand_form],FALSE,FALSE,0);
      set_background(sw[W_expand_form],(ss->sgx)->basic_color);
      
      sw[W_expand_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]),sw[W_expand_event],FALSE,FALSE,4);
      gtk_widget_show(sw[W_expand_event]);
      gtk_signal_connect(GTK_OBJECT(sw[W_expand_event]),"button_press_event",GTK_SIGNAL_FUNC(expand_click_callback),(gpointer)sp);
      set_background(sw[W_expand_event],(ss->sgx)->basic_color);
      
      sw[W_expand_label] = gtk_label_new(STR_expand);
      gtk_container_add(GTK_CONTAINER(sw[W_expand_event]),sw[W_expand_label]);
      gtk_widget_show(sw[W_expand_label]);
      
      sw[W_expand_number] = gtk_label_new(expand_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]),sw[W_expand_number],FALSE,FALSE,0);
      gtk_widget_show(sw[W_expand_number]);
      
      adjs[W_expand_adj] = gtk_adjustment_new(0.45,0.0,1.0,0.001,0.01,.1);
      sw[W_expand] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_expand_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]),sw[W_expand],TRUE,TRUE,4);
      gtk_signal_connect(GTK_OBJECT(adjs[W_expand_adj]),"value_changed",GTK_SIGNAL_FUNC(expand_changed_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_expand]),"button_release_event",GTK_SIGNAL_FUNC(expand_release_callback),(gpointer)sp);
      gtk_widget_show(sw[W_expand]);
      
      sw[W_expand_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]),sw[W_expand_button],FALSE,FALSE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_expand_button]),"clicked",GTK_SIGNAL_FUNC(expand_button_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_expand_button],ss);
      gtk_widget_show(sw[W_expand_button]);
      
      gtk_widget_show(sw[W_expand_form]);
      
      
      /* -------- CONTRAST FIELDS -------- */
      
      sw[W_contrast_form] = gtk_hbox_new(FALSE,2);
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_contrast_form],FALSE,FALSE,0);
      set_background(sw[W_contrast_form],(ss->sgx)->basic_color);
      
      sw[W_contrast_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]),sw[W_contrast_event],FALSE,FALSE,4);
      gtk_widget_show(sw[W_contrast_event]);
      gtk_signal_connect(GTK_OBJECT(sw[W_contrast_event]),"button_press_event",GTK_SIGNAL_FUNC(contrast_click_callback),(gpointer)sp);
      set_background(sw[W_contrast_event],(ss->sgx)->basic_color);
      
      sw[W_contrast_label] = gtk_label_new(STR_contrast);
      gtk_container_add(GTK_CONTAINER(sw[W_contrast_event]),sw[W_contrast_label]);
      gtk_widget_show(sw[W_contrast_label]);
      
      sw[W_contrast_number] = gtk_label_new(contrast_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]),sw[W_contrast_number],FALSE,FALSE,0);
      gtk_widget_show(sw[W_contrast_number]);
      
      adjs[W_contrast_adj] = gtk_adjustment_new(0.0,0.0,1.0,0.001,0.01,.1);
      sw[W_contrast] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_contrast_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]),sw[W_contrast],TRUE,TRUE,4);
      gtk_signal_connect(GTK_OBJECT(adjs[W_contrast_adj]),"value_changed",GTK_SIGNAL_FUNC(contrast_changed_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_contrast]),"button_release_event",GTK_SIGNAL_FUNC(contrast_release_callback),(gpointer)sp);
      gtk_widget_show(sw[W_contrast]);
      
      sw[W_contrast_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]),sw[W_contrast_button],FALSE,FALSE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_contrast_button]),"clicked",GTK_SIGNAL_FUNC(contrast_button_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_contrast_button],ss);
      gtk_widget_show(sw[W_contrast_button]);
      
      gtk_widget_show(sw[W_contrast_form]);
      
      
      /* -------- REVERB FIELDS -------- */
      
      sw[W_reverb_form] = gtk_hbox_new(FALSE,2);
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_reverb_form],FALSE,FALSE,0);
      set_background(sw[W_reverb_form],(ss->sgx)->basic_color);
      
      sw[W_revscl_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]),sw[W_revscl_event],FALSE,FALSE,4);
      gtk_widget_show(sw[W_revscl_event]);
      gtk_signal_connect(GTK_OBJECT(sw[W_revscl_event]),"button_press_event",GTK_SIGNAL_FUNC(revscl_click_callback),(gpointer)sp);
      set_background(sw[W_revscl_event],(ss->sgx)->basic_color);
      
      sw[W_revscl_label] = gtk_label_new(STR_reverb);
      gtk_container_add(GTK_CONTAINER(sw[W_revscl_event]),sw[W_revscl_label]);
      gtk_widget_show(sw[W_revscl_label]);
      
      sw[W_revscl_number] = gtk_label_new(revscl_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]),sw[W_revscl_number],FALSE,FALSE,0);
      gtk_widget_show(sw[W_revscl_number]);
      
      adjs[W_revscl_adj] = gtk_adjustment_new(0.0,0.0,1.0,0.001,0.01,.1);
      sw[W_revscl] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_revscl_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]),sw[W_revscl],TRUE,TRUE,4);
      gtk_signal_connect(GTK_OBJECT(adjs[W_revscl_adj]),"value_changed",GTK_SIGNAL_FUNC(revscl_changed_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_revscl]),"button_release_event",GTK_SIGNAL_FUNC(revscl_release_callback),(gpointer)sp);
      gtk_widget_show(sw[W_revscl]);
      
      sw[W_revlen_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]),sw[W_revlen_event],FALSE,FALSE,4);
      gtk_widget_show(sw[W_revlen_event]);
      gtk_signal_connect(GTK_OBJECT(sw[W_revlen_event]),"button_press_event",GTK_SIGNAL_FUNC(revlen_click_callback),(gpointer)sp);
      set_background(sw[W_revlen_event],(ss->sgx)->basic_color);
      
      sw[W_revlen_label] = gtk_label_new(STR_len);
      gtk_container_add(GTK_CONTAINER(sw[W_revlen_event]),sw[W_revlen_label]);
      gtk_widget_show(sw[W_revlen_label]);
      
      sw[W_revlen_number] = gtk_label_new(revlen_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]),sw[W_revlen_number],FALSE,FALSE,0);
      gtk_widget_show(sw[W_revlen_number]);
      
      adjs[W_revlen_adj] = gtk_adjustment_new(0.2,0.0,1.0,0.001,0.01,.1);
      sw[W_revlen] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_revlen_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]),sw[W_revlen],TRUE,TRUE,4);
      gtk_signal_connect(GTK_OBJECT(adjs[W_revlen_adj]),"value_changed",GTK_SIGNAL_FUNC(revlen_changed_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_revlen]),"button_release_event",GTK_SIGNAL_FUNC(revlen_release_callback),(gpointer)sp);
      gtk_widget_show(sw[W_revlen]);
      
      sw[W_reverb_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]),sw[W_reverb_button],FALSE,FALSE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_reverb_button]),"clicked",GTK_SIGNAL_FUNC(reverb_button_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_reverb_button],ss);
      gtk_widget_show(sw[W_reverb_button]);
      
      gtk_widget_show(sw[W_reverb_form]);
      
      
      /* -------- FILTER FIELDS -------- */
      
      sw[W_filter_form] = gtk_hbox_new(FALSE,2);
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_filter_form],FALSE,FALSE,0);
      set_background(sw[W_filter_form],(ss->sgx)->basic_color);
      
      sw[W_filter_label] = gtk_label_new(STR_filter);
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]),sw[W_filter_label],FALSE,FALSE,4);
      gtk_widget_show(sw[W_filter_label]);
      
      adjs[W_filter_adj] = gtk_adjustment_new(20,2,1000,2,10,0);
      sw[W_filter_order] = gtk_spin_button_new(GTK_ADJUSTMENT(adjs[W_filter_adj]),0.0,0);
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]),sw[W_filter_order],FALSE,FALSE,2);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(sw[W_filter_order]),TRUE);
      gtk_signal_connect(GTK_OBJECT(adjs[W_filter_adj]),"value_changed",GTK_SIGNAL_FUNC(filter_order_callback),(gpointer)sp);
      gtk_widget_show(sw[W_filter_order]);
      
      sw[W_filter] = gtk_entry_new();
      gtk_signal_connect(GTK_OBJECT(sw[W_filter]),"activate",GTK_SIGNAL_FUNC(filter_activate_callback),(gpointer)sp);
      gtk_entry_set_editable(GTK_ENTRY(sw[W_filter]),TRUE);
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]),sw[W_filter],TRUE,TRUE,2);
      gtk_widget_show(sw[W_filter]);
      
      sw[W_filter_dB] = gtk_check_button_new_with_label(STR_dB);
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]),sw[W_filter_dB],FALSE,FALSE,2);
      gtk_signal_connect(GTK_OBJECT(sw[W_filter_dB]),"clicked",GTK_SIGNAL_FUNC(filter_db_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_filter_dB],ss);
      gtk_widget_show(sw[W_filter_dB]);
      
      sw[W_filter_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]),sw[W_filter_button],FALSE,FALSE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_filter_button]),"clicked",GTK_SIGNAL_FUNC(filter_button_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_filter_button],ss);
      gtk_widget_show(sw[W_filter_button]);
      
      gtk_widget_show(sw[W_filter_form]);
      
      
      /* -------- APPLY BUTTONS -------- */
      
      sw[W_apply_form] = gtk_hbox_new(FALSE,2);
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_apply_form],FALSE,FALSE,0);
      set_background(sw[W_apply_form],(ss->sgx)->basic_color);
      
      sw[W_apply] = gtk_button_new_with_label(STR_Apply);
      gtk_box_pack_start(GTK_BOX(sw[W_apply_form]),sw[W_apply],TRUE,TRUE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_apply]),"clicked",GTK_SIGNAL_FUNC(apply_button_callback),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_apply]),"button_press_event",GTK_SIGNAL_FUNC(apply_button_press_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_apply],ss);
      gtk_widget_show(sw[W_apply]);
      
      sw[W_remember] = gtk_button_new_with_label(STR_Remember);
      gtk_box_pack_start(GTK_BOX(sw[W_apply_form]),sw[W_remember],TRUE,TRUE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_remember]),"clicked",GTK_SIGNAL_FUNC(remember_button_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_remember],ss);
      gtk_widget_show(sw[W_remember]);
      
      sw[W_restore] = gtk_button_new_with_label(STR_Restore);
      gtk_box_pack_start(GTK_BOX(sw[W_apply_form]),sw[W_restore],TRUE,TRUE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_restore]),"clicked",GTK_SIGNAL_FUNC(restore_button_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_restore],ss);
      gtk_widget_show(sw[W_restore]);
      
      sw[W_reset] = gtk_button_new_with_label(STR_Reset);
      gtk_box_pack_start(GTK_BOX(sw[W_apply_form]),sw[W_reset],TRUE,TRUE,0);
      gtk_signal_connect(GTK_OBJECT(sw[W_reset]),"clicked",GTK_SIGNAL_FUNC(reset_button_callback),(gpointer)sp);
      set_pushed_button_colors(sw[W_reset],ss);
      gtk_widget_show(sw[W_reset]);
      
      gtk_widget_show(sw[W_apply_form]);

      
      /* -------- FILTER GRAPH -------- */
      
      sw[W_filter_frame] = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(sw[W_ctrls]),sw[W_filter_frame],TRUE,TRUE,10);
      
      sw[W_filter_env] = gtk_drawing_area_new();
      gtk_widget_set_events(sw[W_filter_env], GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(sw[W_filter_frame]),sw[W_filter_env]);
      set_background(sw[W_filter_env],(ss->sgx)->basic_color);
      gtk_widget_show(sw[W_filter_env]);
      gtk_signal_connect(GTK_OBJECT(sw[W_filter_env]),"expose_event",GTK_SIGNAL_FUNC(filter_drawer_expose),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_filter_env]),"configure_event",GTK_SIGNAL_FUNC(filter_drawer_resize),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_filter_env]),"button_press_event",GTK_SIGNAL_FUNC(filter_drawer_button_press),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_filter_env]),"button_release_event",GTK_SIGNAL_FUNC(filter_drawer_button_release),(gpointer)sp);
      gtk_signal_connect(GTK_OBJECT(sw[W_filter_env]),"motion_notify_event",GTK_SIGNAL_FUNC(filter_drawer_button_motion),(gpointer)sp);


      gtk_widget_show(sw[W_filter_frame]);
      new_flt(sp);
      
      /* end if control-panel */
      gtk_widget_show(sw[W_ctrls]);
      gtk_widget_show(sw[W_pane]);
    } /* new sound ss */
  else
    { /* re-manage currently inactive chan */
      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS) 
	raise_dialog(sx->dialog);
      gtk_widget_show(w_snd_pane(sp));
      for (k=0;k<nchans;k++) add_channel_window(sp,k,ss,chan_min_y,0,NULL,WITH_FW_BUTTONS);
      gtk_label_set_text(GTK_LABEL(sw[W_name]),shortname_indexed(sp));

      reset_control_panel(sp);
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	{
	  gtk_notebook_set_tab_label_text(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)),sw[W_pane],sp->shortname);
	  set_text_background(gtk_notebook_get_tab_label(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)),sw[W_pane]),(ss->sgx)->basic_color);
	}
    }

  if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
    {
      title = (char *)CALLOC(128,sizeof(char));
      sprintf(title,"%d: %s",snd_slot,sp->shortname);
      gtk_window_set_title(GTK_WINDOW(sx->dialog),title);
      FREE(title);
    }

  if (sp->nchans == 1) gtk_widget_hide(sw[W_combine]);
  add_sound_data(filename,sp,ss);

  snd_file_lock_icon(sp,(ss->viewing || (cant_write(sp->fullname)))); /* sp->read_only not set yet */
  if (ss->pending_change)
    {
      char *snd_txt_buf;
      snd_txt_buf = (char *)CALLOC(256,sizeof(char));
      sprintf(snd_txt_buf,"(translated %s)",old_name);
      report_in_minibuffer(sp,snd_txt_buf);
      FREE(snd_txt_buf);
    }
#if HAVE_GUILE
  after_open(sp->index);
#endif
  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
    sx->page = gtk_notebook_page_num(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)),sw[W_pane]);
  return(sp);
}

void set_sound_pane_file_label(snd_info *sp, char *str)
{
  set_label(w_snd_name(sp),str);
}

int sound_unlock_ctrls(snd_info *sp, void *ptr)
{
  return(0);
}

int sound_lock_ctrls(snd_info *sp, void *ptr)
{
  return(0);
}

void snd_info_cleanup(snd_info *sp)
{
  snd_context *sx;
  snd_state *ss;
  if (w_snd_pane(sp)) gtk_widget_hide(w_snd_pane(sp));
  if ((sp) && (sp->sgx))
    {
      ss = sp->state;
      sx = sp->sgx;
      if (w_snd_sync(sp))
	{
	  sp->combining = CHANNELS_SEPARATE; 
	  if (sound_style(sp->state) == SOUNDS_IN_NOTEBOOK)
	    {
	      /* gtk_notebook_remove_page(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)),sx->page); */
	    }
	  /* gtk_widget_hide(w_snd_pane(sp)); */
	}
    }
}

void unlock_ctrls(snd_info *sp) {return;}

void set_apply_button(snd_info *sp, int val) 
{
  gtk_widget_set_sensitive(w_snd_apply(sp),val);
}

void normalize_sound(snd_state *ss, snd_info *sp, chan_info *ncp)
{
  return;
}

/* ---------------- normalize sounds ---------------- */

void normalize_all_sounds(snd_state *ss)
{
  return;
}

void sound_show_ctrls(snd_info *sp)
{
  int height;
  height = widget_height(w_snd_pane(sp));
  if (height>200)
    gtk_paned_set_position(GTK_PANED(w_snd_pane(sp)),height-150);
  else gtk_paned_set_position(GTK_PANED(w_snd_pane(sp)),height/2);
}

void sound_hide_ctrls(snd_info *sp)
{
  gtk_paned_set_position(GTK_PANED(w_snd_pane(sp)),widget_height(w_snd_pane(sp)));
}

int control_panel_open(snd_info *sp)
{
  return(widget_height(w_snd_ctrls(sp)) > CLOSED_CTRLS_HEIGHT);
}

void show_controls(snd_state *ss)
{
  snd_info *sp;
  int i;
  ss->ctrls_height = ss->open_ctrls_height;
  set_view_ctrls_label(STR_Hide_controls);
  for (i=0;i<ss->max_sounds;i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse)) sound_show_ctrls(sp);
    }
}

void hide_controls(snd_state *ss)
{
  snd_info *sp;
  int i;
  ss->ctrls_height = CLOSED_CTRLS_HEIGHT;
  set_view_ctrls_label(STR_Show_controls);
  for (i=0;i<ss->max_sounds;i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse)) sound_hide_ctrls(sp);
    }
}

void sound_check_control_panel(snd_info *sp, int height)
{
  if (((sp->sgx)->controls_fixed == 0) && (height > 50))
    {
      (sp->sgx)->controls_fixed = 1;
      sound_hide_ctrls(sp);
    }
}


/* -------- PROGRESS REPORT -------- */

void progress_report(snd_info *sp, char *funcname, int curchan, int chans, Float pct, int from_enved)
{
  int which;
  which = (int)(pct * NUM_GLASSES);
  if (which >= NUM_GLASSES) which = NUM_GLASSES-1;
  if (which < 0) which = 0;
  if (from_enved)
    display_enved_progress(NULL,mini_glasses[which],glass_mask);
  else snd_file_glasses_icon(sp,TRUE,which);
  check_for_event(sp->state);
}

void finish_progress_report(snd_info *sp, int from_enved)
{
  if (from_enved)
    display_enved_progress(NULL,blank,blank_mask);
  else snd_file_glasses_icon(sp,FALSE,0);
}

void start_progress_report(snd_info *sp, int from_enved)
{
  if (!(from_enved)) snd_file_glasses_icon(sp,TRUE,0);
}

#if HAVE_GUILE_GTK
#include <guile-gtk.h>

/* (gtk-label-set-text (sg-sound-amp-label-widget) "bang!") */

#define Sg_sound_pane_widget              "sg-sound-pane-widget"
#define Sg_sound_pane_box_widget          "sg-sound-pane-box-widget"
#define Sg_sound_ctrls_widget             "sg-sound-ctrls-widget"
#define Sg_sound_name_form_widget         "sg-sound-name-form-widget"
#define Sg_sound_name_widget              "sg-sound-name-widget"
#define Sg_sound_name_event_widget        "sg-sound-name-event-widget"
#define Sg_sound_name_icon_widget         "sg-sound-name-icon-widget"
#define Sg_sound_name_pix_widget          "sg-sound-name-pix-widget"
#define Sg_sound_info_label_widget        "sg-sound-info-label-widget"
#define Sg_sound_info_widget              "sg-sound-info-widget"
#define Sg_sound_play_widget              "sg-sound-play-widget"
#define Sg_sound_sync_widget              "sg-sound-sync-widget"
#define Sg_sound_combine_widget           "sg-sound-combine-widget"
#define Sg_sound_amp_widget               "sg-sound-amp-widget"
#define Sg_sound_amp_label_widget         "sg-sound-amp-label-widget"
#define Sg_sound_amp_number_widget        "sg-sound-amp-number-widget"
#define Sg_sound_srate_widget             "sg-sound-srate-widget"
#define Sg_sound_srate_label_widget       "sg-sound-srate-label-widget"
#define Sg_sound_srate_number_widget      "sg-sound-srate-number-widget"
#define Sg_sound_srate_arrow_widget       "sg-sound-srate-arrow-widget"
#define Sg_sound_srate_pix_widget         "sg-sound-srate-pix-widget"
#define Sg_sound_expand_widget            "sg-sound-expand-widget"
#define Sg_sound_expand_label_widget      "sg-sound-expand-label-widget"
#define Sg_sound_expand_number_widget     "sg-sound-expand-number-widget"
#define Sg_sound_expand_button_widget     "sg-sound-expand-button-widget"
#define Sg_sound_contrast_widget          "sg-sound-contrast-widget"
#define Sg_sound_contrast_label_widget    "sg-sound-contrast-label-widget"
#define Sg_sound_contrast_number_widget   "sg-sound-contrast-number-widget"
#define Sg_sound_contrast_button_widget   "sg-sound-contrast-button-widget"
#define Sg_sound_revscl_widget            "sg-sound-revscl-widget"
#define Sg_sound_revscl_label_widget      "sg-sound-revscl-label-widget"
#define Sg_sound_revscl_number_widget     "sg-sound-revscl-number-widget"
#define Sg_sound_revlen_widget            "sg-sound-revlen-widget"
#define Sg_sound_revlen_label_widget      "sg-sound-revlen-label-widget"
#define Sg_sound_revlen_number_widget     "sg-sound-revlen-number-widget"
#define Sg_sound_reverb_button_widget     "sg-sound-reverb-button-widget"
#define Sg_sound_filter_label_widget      "sg-sound-filter-label-widget"
#define Sg_sound_filter_order_widget      "sg-sound-filter-order-widget"
#define Sg_sound_filter_env_widget        "sg-sound-filter-env-widget"
#define Sg_sound_filter_widget            "sg-sound-filter-widget"
#define Sg_sound_filter_button_widget     "sg-sound-filter-button-widget"
#define Sg_sound_filter_dB_widget         "sg-sound-filter-dB-widget"
#define Sg_sound_remember_widget          "sg-sound-remember-widget"
#define Sg_sound_restore_widget           "sg-sound-restore-widget"
#define Sg_sound_apply_widget             "sg-sound-apply-widget"
#define Sg_sound_reset_widget             "sg-sound-reset-widget"
#define Sg_sound_amp_adjustment           "sg-sound-amp-adjustment"
#define Sg_sound_srate_adjustment         "sg-sound-srate-adjustment"
#define Sg_sound_contrast_adjustment      "sg-sound-contrast-adjustment"
#define Sg_sound_expand_adjustment        "sg-sound-expand-adjustment"
#define Sg_sound_revscl_adjustment        "sg-sound-revscl-adjustment"
#define Sg_sound_revlen_adjustment        "sg-sound-revlen-adjustment"
#define Sg_sound_filter_adjustment        "sg-sound-filter-adjustment"

static snd_info *get_sound_sp(SCM scm_snd_n)
{
  int snd_n;
  snd_state *ss;
  ss = get_global_state();
  if (gh_number_p(scm_snd_n))
    {
      snd_n = gh_scm2int(scm_snd_n);
      if ((snd_n >= 0) && (snd_n < ss->max_sounds) && (snd_ok(ss->sounds[snd_n])))
	return(ss->sounds[snd_n]);
      else return(NULL);
    }
  return(any_selected_sound(ss));
}

static SCM sg_sound_widget(SCM snd, int n)
{
  snd_info *sp; 
  sp = get_sound_sp(snd); 
  if (sp) return(sgtk_wrap_gtkobj((GtkObject *)(((snd_context *)(sp->sgx))->snd_widgets[n]))); 
  return(SCM_BOOL_F);
}

static SCM sg_sound_pane_widget(SCM snd) {return(sg_sound_widget(snd,W_pane));}
static SCM sg_sound_pane_box_widget(SCM snd) {return(sg_sound_widget(snd,W_pane_box));}
static SCM sg_sound_ctrls_widget(SCM snd) {return(sg_sound_widget(snd,W_ctrls));}
static SCM sg_sound_name_form_widget(SCM snd) {return(sg_sound_widget(snd,W_name_form));}
static SCM sg_sound_name_widget(SCM snd) {return(sg_sound_widget(snd,W_name));}
static SCM sg_sound_name_event_widget(SCM snd) {return(sg_sound_widget(snd,W_name_event));}
static SCM sg_sound_name_icon_widget(SCM snd) {return(sg_sound_widget(snd,W_name_icon));}
static SCM sg_sound_name_pix_widget(SCM snd) {return(sg_sound_widget(snd,W_name_pix));}
static SCM sg_sound_info_label_widget(SCM snd) {return(sg_sound_widget(snd,W_info_label));}
static SCM sg_sound_info_widget(SCM snd) {return(sg_sound_widget(snd,W_info));}
static SCM sg_sound_play_widget(SCM snd) {return(sg_sound_widget(snd,W_play));}
static SCM sg_sound_sync_widget(SCM snd) {return(sg_sound_widget(snd,W_sync));}
static SCM sg_sound_combine_widget(SCM snd) {return(sg_sound_widget(snd,W_combine));}
static SCM sg_sound_amp_widget(SCM snd) {return(sg_sound_widget(snd,W_amp));}
static SCM sg_sound_amp_label_widget(SCM snd) {return(sg_sound_widget(snd,W_amp_label));}
static SCM sg_sound_amp_number_widget(SCM snd) {return(sg_sound_widget(snd,W_amp_number));}
static SCM sg_sound_srate_widget(SCM snd) {return(sg_sound_widget(snd,W_srate));}
static SCM sg_sound_srate_label_widget(SCM snd) {return(sg_sound_widget(snd,W_srate_label));}
static SCM sg_sound_srate_number_widget(SCM snd) {return(sg_sound_widget(snd,W_srate_number));}
static SCM sg_sound_srate_arrow_widget(SCM snd) {return(sg_sound_widget(snd,W_srate_arrow));}
static SCM sg_sound_srate_pix_widget(SCM snd) {return(sg_sound_widget(snd,W_srate_pix));}
static SCM sg_sound_expand_widget(SCM snd) {return(sg_sound_widget(snd,W_expand));}
static SCM sg_sound_expand_label_widget(SCM snd) {return(sg_sound_widget(snd,W_expand_label));}
static SCM sg_sound_expand_number_widget(SCM snd) {return(sg_sound_widget(snd,W_expand_number));}
static SCM sg_sound_expand_button_widget(SCM snd) {return(sg_sound_widget(snd,W_expand_button));}
static SCM sg_sound_contrast_widget(SCM snd) {return(sg_sound_widget(snd,W_contrast));}
static SCM sg_sound_contrast_label_widget(SCM snd) {return(sg_sound_widget(snd,W_contrast_label));}
static SCM sg_sound_contrast_number_widget(SCM snd) {return(sg_sound_widget(snd,W_contrast_number));}
static SCM sg_sound_contrast_button_widget(SCM snd) {return(sg_sound_widget(snd,W_contrast_button));}
static SCM sg_sound_revscl_widget(SCM snd) {return(sg_sound_widget(snd,W_revscl));}
static SCM sg_sound_revscl_label_widget(SCM snd) {return(sg_sound_widget(snd,W_revscl_label));}
static SCM sg_sound_revscl_number_widget(SCM snd) {return(sg_sound_widget(snd,W_revscl_number));}
static SCM sg_sound_revlen_widget(SCM snd) {return(sg_sound_widget(snd,W_revlen));}
static SCM sg_sound_revlen_label_widget(SCM snd) {return(sg_sound_widget(snd,W_revlen_label));}
static SCM sg_sound_revlen_number_widget(SCM snd) {return(sg_sound_widget(snd,W_revlen_number));}
static SCM sg_sound_reverb_button_widget(SCM snd) {return(sg_sound_widget(snd,W_reverb_button));}
static SCM sg_sound_filter_label_widget(SCM snd) {return(sg_sound_widget(snd,W_filter_label));}
static SCM sg_sound_filter_order_widget(SCM snd) {return(sg_sound_widget(snd,W_filter_order));}
static SCM sg_sound_filter_env_widget(SCM snd) {return(sg_sound_widget(snd,W_filter_env));}
static SCM sg_sound_filter_widget(SCM snd) {return(sg_sound_widget(snd,W_filter));}
static SCM sg_sound_filter_button_widget(SCM snd) {return(sg_sound_widget(snd,W_filter_button));}
static SCM sg_sound_filter_dB_widget(SCM snd) {return(sg_sound_widget(snd,W_filter_dB));}
static SCM sg_sound_remember_widget(SCM snd) {return(sg_sound_widget(snd,W_remember));}
static SCM sg_sound_restore_widget(SCM snd) {return(sg_sound_widget(snd,W_restore));}
static SCM sg_sound_apply_widget(SCM snd) {return(sg_sound_widget(snd,W_apply));}
static SCM sg_sound_reset_widget(SCM snd) {return(sg_sound_widget(snd,W_reset));}
static SCM sg_sound_amp_adjustment(SCM snd) {return(sg_sound_widget(snd,W_amp_adj));}
static SCM sg_sound_srate_adjustment(SCM snd) {return(sg_sound_widget(snd,W_srate_adj));}
static SCM sg_sound_contrast_adjustment(SCM snd) {return(sg_sound_widget(snd,W_contrast_adj));}
static SCM sg_sound_expand_adjustment(SCM snd) {return(sg_sound_widget(snd,W_expand_adj));}
static SCM sg_sound_revscl_adjustment(SCM snd) {return(sg_sound_widget(snd,W_revscl_adj));}
static SCM sg_sound_revlen_adjustment(SCM snd) {return(sg_sound_widget(snd,W_revlen_adj));}
static SCM sg_sound_filter_adjustment(SCM snd) {return(sg_sound_widget(snd,W_filter_adj));}


void init_sound_widgets(SCM local_doc)
{
  gh_new_procedure0_1(Sg_sound_pane_widget,sg_sound_pane_widget);
  gh_new_procedure0_1(Sg_sound_pane_box_widget,sg_sound_pane_box_widget);
  gh_new_procedure0_1(Sg_sound_ctrls_widget,sg_sound_ctrls_widget);
  gh_new_procedure0_1(Sg_sound_name_form_widget,sg_sound_name_form_widget);
  gh_new_procedure0_1(Sg_sound_name_widget,sg_sound_name_widget);
  gh_new_procedure0_1(Sg_sound_name_event_widget,sg_sound_name_event_widget);
  gh_new_procedure0_1(Sg_sound_name_icon_widget,sg_sound_name_icon_widget);
  gh_new_procedure0_1(Sg_sound_name_pix_widget,sg_sound_name_pix_widget);
  gh_new_procedure0_1(Sg_sound_info_label_widget,sg_sound_info_label_widget);
  gh_new_procedure0_1(Sg_sound_info_widget,sg_sound_info_widget);
  gh_new_procedure0_1(Sg_sound_play_widget,sg_sound_play_widget);
  gh_new_procedure0_1(Sg_sound_sync_widget,sg_sound_sync_widget);
  gh_new_procedure0_1(Sg_sound_combine_widget,sg_sound_combine_widget);
  gh_new_procedure0_1(Sg_sound_amp_widget,sg_sound_amp_widget);
  gh_new_procedure0_1(Sg_sound_amp_label_widget,sg_sound_amp_label_widget);
  gh_new_procedure0_1(Sg_sound_amp_number_widget,sg_sound_amp_number_widget);
  gh_new_procedure0_1(Sg_sound_srate_widget,sg_sound_srate_widget);
  gh_new_procedure0_1(Sg_sound_srate_label_widget,sg_sound_srate_label_widget);
  gh_new_procedure0_1(Sg_sound_srate_number_widget,sg_sound_srate_number_widget);
  gh_new_procedure0_1(Sg_sound_srate_arrow_widget,sg_sound_srate_arrow_widget);
  gh_new_procedure0_1(Sg_sound_srate_pix_widget,sg_sound_srate_pix_widget);
  gh_new_procedure0_1(Sg_sound_expand_widget,sg_sound_expand_widget);
  gh_new_procedure0_1(Sg_sound_expand_label_widget,sg_sound_expand_label_widget);
  gh_new_procedure0_1(Sg_sound_expand_number_widget,sg_sound_expand_number_widget);
  gh_new_procedure0_1(Sg_sound_expand_button_widget,sg_sound_expand_button_widget);
  gh_new_procedure0_1(Sg_sound_contrast_widget,sg_sound_contrast_widget);
  gh_new_procedure0_1(Sg_sound_contrast_label_widget,sg_sound_contrast_label_widget);
  gh_new_procedure0_1(Sg_sound_contrast_number_widget,sg_sound_contrast_number_widget);
  gh_new_procedure0_1(Sg_sound_contrast_button_widget,sg_sound_contrast_button_widget);
  gh_new_procedure0_1(Sg_sound_revscl_widget,sg_sound_revscl_widget);
  gh_new_procedure0_1(Sg_sound_revscl_label_widget,sg_sound_revscl_label_widget);
  gh_new_procedure0_1(Sg_sound_revscl_number_widget,sg_sound_revscl_number_widget);
  gh_new_procedure0_1(Sg_sound_revlen_widget,sg_sound_revlen_widget);
  gh_new_procedure0_1(Sg_sound_revlen_label_widget,sg_sound_revlen_label_widget);
  gh_new_procedure0_1(Sg_sound_revlen_number_widget,sg_sound_revlen_number_widget);
  gh_new_procedure0_1(Sg_sound_reverb_button_widget,sg_sound_reverb_button_widget);
  gh_new_procedure0_1(Sg_sound_filter_label_widget,sg_sound_filter_label_widget);
  gh_new_procedure0_1(Sg_sound_filter_order_widget,sg_sound_filter_order_widget);
  gh_new_procedure0_1(Sg_sound_filter_env_widget,sg_sound_filter_env_widget);
  gh_new_procedure0_1(Sg_sound_filter_widget,sg_sound_filter_widget);
  gh_new_procedure0_1(Sg_sound_filter_button_widget,sg_sound_filter_button_widget);
  gh_new_procedure0_1(Sg_sound_filter_dB_widget,sg_sound_filter_dB_widget);
  gh_new_procedure0_1(Sg_sound_remember_widget,sg_sound_remember_widget);
  gh_new_procedure0_1(Sg_sound_restore_widget,sg_sound_restore_widget);
  gh_new_procedure0_1(Sg_sound_apply_widget,sg_sound_apply_widget);
  gh_new_procedure0_1(Sg_sound_reset_widget,sg_sound_reset_widget);
  gh_new_procedure0_1(Sg_sound_amp_adjustment,sg_sound_amp_adjustment);
  gh_new_procedure0_1(Sg_sound_srate_adjustment,sg_sound_srate_adjustment);
  gh_new_procedure0_1(Sg_sound_contrast_adjustment,sg_sound_contrast_adjustment);
  gh_new_procedure0_1(Sg_sound_expand_adjustment,sg_sound_expand_adjustment);
  gh_new_procedure0_1(Sg_sound_revscl_adjustment,sg_sound_revscl_adjustment);
  gh_new_procedure0_1(Sg_sound_revlen_adjustment,sg_sound_revlen_adjustment);
  gh_new_procedure0_1(Sg_sound_filter_adjustment,sg_sound_filter_adjustment);
}

#endif

