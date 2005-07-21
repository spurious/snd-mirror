#include "snd.h"

enum {W_pane, W_pane_box, W_control_panel,
      W_name_form, W_name, W_name_event, W_name_pix, W_info_label, W_info, W_info_sep,
      W_play, W_sync, W_unite,
      W_amp_form, W_amp_event, W_amp, W_amp_label, W_amp_number, W_amp_sep,
      W_speed_form, W_speed, W_speed_event, W_speed_label, W_speed_number, W_speed_pix,
      W_expand_form, W_expand, W_expand_event, W_expand_label, W_expand_number, W_expand_button,
      W_contrast_form, W_contrast, W_contrast_event, W_contrast_label, W_contrast_number, W_contrast_button,
      W_reverb_form, W_revscl, W_revscl_event, W_revscl_label, W_revscl_number,
      W_revlen, W_revlen_event, W_revlen_label, W_revlen_number, W_reverb_button,
      W_filter_form, W_filter_label, W_filter_order, W_filter_env, W_filter, W_filter_button, 
      W_filter_dB, W_filter_hz, W_filter_frame,
      W_error_info_frame, W_error_info_label,
      NUM_SND_WIDGETS
};

enum {W_amp_adj, W_speed_adj, W_contrast_adj, W_expand_adj, W_revscl_adj, 
      W_revlen_adj, W_filter_adj,
      NUM_SND_ADJS
};

GtkWidget *unite_button(snd_info *sp)   {return(sp->sgx->snd_widgets[W_unite]);}
GtkWidget *filter_graph(snd_info *sp)   {return(sp->sgx->snd_widgets[W_filter_env]);}

GtkWidget *w_snd_pane(snd_info *sp)     {return(sp->sgx->snd_widgets[W_pane]);}
GtkWidget *w_snd_pane_box(snd_info *sp) {return(sp->sgx->snd_widgets[W_pane_box]);}
GtkWidget *w_snd_name(snd_info *sp)     {return(sp->sgx->snd_widgets[W_name]);}

#define NAME_BOX(Sp)             Sp->sgx->snd_widgets[W_name_form]
#define MINIBUFFER_SEPARATOR(Sp) Sp->sgx->snd_widgets[W_info_sep]
#define MINIBUFFER_LABEL(Sp)     Sp->sgx->snd_widgets[W_info_label]
#define MINIBUFFER_TEXT(Sp)      Sp->sgx->snd_widgets[W_info]
#define NAME_PIX(Sp)             Sp->sgx->snd_widgets[W_name_pix]
#define SYNC_BUTTON(Sp)          Sp->sgx->snd_widgets[W_sync]
#define PLAY_BUTTON(Sp)          Sp->sgx->snd_widgets[W_play]
#define UNITE_BUTTON(Sp)         Sp->sgx->snd_widgets[W_unite]

#define ERROR_INFO(Sp)           Sp->sgx->snd_widgets[W_error_info_label]
#define ERROR_INFO_FRAME(Sp)     Sp->sgx->snd_widgets[W_error_info_frame]


#define CONTROL_PANEL(Sp)        Sp->sgx->snd_widgets[W_control_panel]

#define SPEED_ARROW(Sp)          Sp->sgx->snd_widgets[W_speed_pix]
#define SPEED_LABEL(Sp)          Sp->sgx->snd_widgets[W_speed_number]
#define AMP_LABEL(Sp)            Sp->sgx->snd_widgets[W_amp_number]
#define EXPAND_LABEL(Sp)         Sp->sgx->snd_widgets[W_expand_number]
#define EXPAND_BUTTON(Sp)        Sp->sgx->snd_widgets[W_expand_button]
#define CONTRAST_LABEL(Sp)       Sp->sgx->snd_widgets[W_contrast_number]
#define CONTRAST_BUTTON(Sp)      Sp->sgx->snd_widgets[W_contrast_button]
#define REVSCL_LABEL(Sp)         Sp->sgx->snd_widgets[W_revscl_number]
#define REVLEN_LABEL(Sp)         Sp->sgx->snd_widgets[W_revlen_number]
#define REVERB_BUTTON(Sp)        Sp->sgx->snd_widgets[W_reverb_button]
#define FILTER_ORDER_TEXT(Sp)    Sp->sgx->snd_widgets[W_filter_order]
#define FILTER_COEFFS_TEXT(Sp)   Sp->sgx->snd_widgets[W_filter]
#define FILTER_BUTTON(Sp)        Sp->sgx->snd_widgets[W_filter_button]
#define FILTER_DB_BUTTON(Sp)     Sp->sgx->snd_widgets[W_filter_dB]
#define FILTER_HZ_BUTTON(Sp)     Sp->sgx->snd_widgets[W_filter_hz]

#define AMP_ADJUSTMENT(Sp)       Sp->sgx->snd_adjs[W_amp_adj]
#define SPEED_ADJUSTMENT(Sp)     Sp->sgx->snd_adjs[W_speed_adj]
#define EXPAND_ADJUSTMENT(Sp)    Sp->sgx->snd_adjs[W_expand_adj]
#define CONTRAST_ADJUSTMENT(Sp)  Sp->sgx->snd_adjs[W_contrast_adj]
#define REVSCL_ADJUSTMENT(Sp)    Sp->sgx->snd_adjs[W_revscl_adj]
#define REVLEN_ADJUSTMENT(Sp)    Sp->sgx->snd_adjs[W_revlen_adj]


/* -------- minibuffer error -------- */

static void watch_minibuffer(GtkWidget *w, gpointer context)
{
  clear_minibuffer_error((snd_info *)context);
}

/* TODO: entry here so multi-lines not displayed correctly */
/* TODO: check libxm load process for the -pthread bug */

void clear_minibuffer_error(snd_info *sp)
{
  gtk_widget_hide(ERROR_INFO_FRAME(sp));
  gtk_widget_hide(ERROR_INFO(sp));
  if (sp->sgx->minibuffer_watcher)
    {
      g_signal_handler_disconnect(MINIBUFFER_TEXT(sp), sp->sgx->minibuffer_watcher);
      sp->sgx->minibuffer_watcher = 0;
    }
}

void display_minibuffer_error(snd_info *sp, const char *str) 
{
  gtk_entry_set_text(GTK_ENTRY(ERROR_INFO(sp)), str);
  gtk_widget_show(ERROR_INFO(sp));
  gtk_widget_show(ERROR_INFO_FRAME(sp));
  if (!(sp->sgx->minibuffer_watcher))
    sp->sgx->minibuffer_watcher = SG_SIGNAL_CONNECT(MINIBUFFER_TEXT(sp), "changed", watch_minibuffer, (gpointer)sp);
}


/* -------- PIXMAPS -------- */

static GdkPixmap *mini_lock, *speed_r, *speed_l, *blank;
static bool mini_lock_allocated = false;
static GdkPixmap *mini_bombs[NUM_BOMBS];
static GdkPixmap *mini_glasses[NUM_GLASSES];

void snd_file_lock_icon(snd_info *sp, bool on)
{
  if (mini_lock) 
    {
      snd_context *sx;
      sx = sp->sgx;
      if (on)
	sx->file_pix = mini_lock;
      else sx->file_pix = blank; 
      gdk_draw_drawable(GDK_DRAWABLE(NAME_PIX(sp)->window), ss->sgx->basic_gc, sx->file_pix, 0, 0, 0, 4, 18, 16);
    }
}


#define BOMB_TIME 200

static void show_bomb_icon(snd_info *sp, bool on)
{
  if (sp->bomb_ctr >= NUM_BOMBS) sp->bomb_ctr = 0;
  if (mini_bombs[sp->bomb_ctr]) 
    {
      snd_context *sx;
      sx = sp->sgx;
      if (on)
	sx->file_pix = mini_bombs[sp->bomb_ctr];
      else sx->file_pix = blank;
      gdk_draw_drawable(GDK_DRAWABLE(NAME_PIX(sp)->window), ss->sgx->basic_gc, sx->file_pix, 0, 0, 0, 4, 18, 16);
    }
}

void x_bomb(snd_info *sp, bool on)
{
  show_bomb_icon(sp, on);
  if (on) sp->bomb_ctr++; else sp->bomb_ctr = 0;
}

static void inc_bomb(snd_info *sp, void *ptr)
{
  if (sp->need_update || sp->file_unreadable)
    {
      int *buf;
      buf = (int *)ptr;
      buf[0]++;
      show_bomb_icon(sp, sp->bomb_ctr);
      sp->bomb_ctr++;
    }
}

static bool bomb_in_progress = false;

static gint bomb_check(gpointer data)
{
  int incs[1];
  incs[0] = 0;
  for_each_sound(inc_bomb, (void *)incs);
  if (incs[0] > 0)
    g_timeout_add_full(0, (guint32)BOMB_TIME, bomb_check, data, NULL);
  else bomb_in_progress = false;
  return(0);
}

void snd_file_bomb_icon(snd_info *sp, bool on)
{
  if ((on) && (!bomb_in_progress))
    {
      bomb_in_progress = true;
      g_timeout_add_full(0, (guint32)BOMB_TIME, bomb_check, (gpointer)sp, NULL);
    }
}

static void snd_file_glasses_icon(snd_info *sp, bool on, int glass)
{
  if (!(sp->sgx)) return;
  if (on)
    {
      if (mini_glasses[glass]) 
	gdk_draw_drawable(GDK_DRAWABLE(NAME_PIX(sp)->window), ss->sgx->basic_gc, mini_glasses[glass], 0, 0, 0, 4, 18, 16);
    }
  else gdk_draw_drawable(GDK_DRAWABLE(NAME_PIX(sp)->window), ss->sgx->basic_gc, sp->sgx->file_pix, 0, 0, 0, 4, 18, 16);
}

static void make_pixmaps(void)
{
  if (!mini_lock_allocated)
    { 
      GdkWindow *wn;
      int k;
      wn = MAIN_WINDOW(ss);
      mini_lock = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, mini_lock_bits());
      blank = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, blank_bits());
      speed_r = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, speed_r_bits());
      speed_l = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, speed_l_bits());
      for (k = 0; k < NUM_BOMBS; k++) 
	mini_bombs[k] = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, mini_bomb_bits(k));
      for (k = 0; k < NUM_GLASSES; k++) 
	mini_glasses[k] = gdk_pixmap_create_from_xpm_d(wn, NULL, NULL, mini_glass_bits(k));
      mini_lock_allocated = true;
    }
}

static gboolean name_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  /*
  snd_info *sp = (snd_info *)data;
    Gdk-CRITICAL **: gdk_draw_drawable: assertion `src != NULL' failed
    #3  0x403f94e8 in gdk_draw_drawable (drawable=0x878dfe0, gc=0x8600430, 
        src=0x0, xsrc=0, ysrc=0, xdest=0, ydest=4, width=16, height=16)
        at gdkdraw.c:666
	
	... typical...

  if (NAME_PIX(sp))
    gdk_draw_drawable(GDK_DRAWABLE(NAME_PIX(sp)->window), ss->sgx->basic_gc, sp->sgx->file_pix, 0, 0, 0, 4, 16, 16);
  */
  return(false);
}


/* -------- MINIBUFFER CALLBACKS -------- */

void goto_minibuffer(snd_info *sp)
{
  if (sp) 
    {
      sp->sgx->mini_active = true;
      goto_window(MINIBUFFER_TEXT(sp));
    }
}

void set_minibuffer_cursor_position(snd_info *sp, int pos)
{
  if ((sp->inuse != SOUND_NORMAL) || (!(sp->sgx))) return;
  gtk_editable_set_position(GTK_EDITABLE(MINIBUFFER_TEXT(sp)), pos);
}

char *get_minibuffer_string(snd_info *sp) 
{
  if ((sp->inuse != SOUND_NORMAL) || (!(sp->sgx))) return(NULL);
  return(copy_string((char *)gtk_entry_get_text(GTK_ENTRY(MINIBUFFER_TEXT(sp)))));
} 

static char stupid[1] = {'\0'};
void set_minibuffer_string(snd_info *sp, char *str, bool update) 
{
  if ((sp->inuse != SOUND_NORMAL) || (!(sp->sgx))) return;
  if (str)
    gtk_entry_set_text(GTK_ENTRY(MINIBUFFER_TEXT(sp)), str);
  else gtk_entry_set_text(GTK_ENTRY(MINIBUFFER_TEXT(sp)), stupid);
}

void make_minibuffer_label(snd_info *sp, char *str)
{
  if (sp->inuse != SOUND_NORMAL) return;
  gtk_label_set_text(GTK_LABEL(MINIBUFFER_LABEL(sp)), str);
}

static void minibuffer_activate_callback(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_minibuffer_activate(sp, 0, false);
  sp->sgx->mini_active = true;
}

static gboolean minibuffer_key_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  /* can't use M-p in gtk version because it's trapped by a menu accelerator (File:Print) -- M-n is File:New */
  snd_info *sp = (snd_info *)data;
  if (((!sp->sgx->mini_active)) || 
      (((event->keyval == snd_K_s) || 
	(event->keyval == snd_K_r)) && 
       (event->state & snd_ControlMask)))
    {
      chan_info *cp;
      cp = current_channel();
      if (cp) graph_key_press(channel_graph(cp), event, (gpointer)cp); 
      g_signal_stop_emission(GTK_OBJECT(w), g_signal_lookup("key_press_event", G_OBJECT_TYPE(GTK_OBJECT(w))), 0);
      return(true);
    }
  if (((event->keyval == snd_K_g) || (event->keyval == snd_K_G)) && 
      (event->state & snd_ControlMask))
    {
      clear_minibuffer(sp);
      return(true);
    }
  return(false);
}

static gboolean minibuffer_mouse_enter(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if ((sp) && (sp->inuse == SOUND_NORMAL))
    sp->sgx->mini_active = true;
  return(false);
}

static gboolean minibuffer_mouse_leave(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if ((sp) && (sp->inuse == SOUND_NORMAL))
    sp->sgx->mini_active = false;
  return(false);
}


/* -------- PLAY BUTTON -------- */

static void set_button_base(GtkWidget *w, GdkColor *col)
{
  gtk_widget_modify_base(w, GTK_STATE_NORMAL, col);
  gtk_widget_modify_base(w, GTK_STATE_PRELIGHT, col);
}

void set_play_button(snd_info *sp, bool val)
{
  if ((sp->sgx) && (!(IS_PLAYER(sp))))
    {
      set_toggle_button(PLAY_BUTTON(sp), val, false, (void *)sp);
      set_file_browser_play_button(sp->short_filename, val);
    }
}

void set_control_panel_play_button(snd_info *sp)
{
  if ((sp) && (sp->sgx) && (PLAY_BUTTON(sp)))
    {
      set_toggle_button(PLAY_BUTTON(sp), false, false, sp);
      set_button_base(PLAY_BUTTON(sp), ss->sgx->white);
    }
}

static int last_play_state = 0;

static gboolean play_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_play_state = ev->state;
  return(false);
}

static void play_button_click_callback(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  chan_info *cp;
  bool on;
  on = (bool)(GTK_TOGGLE_BUTTON(w)->active);
  if (sp->playing) 
    stop_playing_sound_no_toggle(sp, PLAY_BUTTON_UNSET);
  if (sp->cursor_follows_play != FOLLOW_ALWAYS)         /* can be set in init file */
    {
      if ((on) && (last_play_state & (snd_ControlMask | snd_MetaMask)))
	sp->cursor_follows_play = FOLLOW_ONCE;
      else sp->cursor_follows_play = DONT_FOLLOW;
    }
  set_file_browser_play_button(sp->short_filename, on);
  cp = any_selected_channel(sp);
  goto_graph(cp);
  if (on) 
    {
      if (sp->cursor_follows_play != DONT_FOLLOW) 
	set_button_base(w, ss->sgx->green);
      else set_button_base(w, ss->sgx->white);
      play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
    }
  else set_button_base(w, ss->sgx->white);
}

typedef struct {bool pausing; } pause_data;

static void set_play_button_pause(snd_info *sp, void *ptr)
{
  if (sp->playing)
    {
      pause_data *pd = (pause_data *)ptr;
      GtkWidget *w;
      w = PLAY_BUTTON(sp);
      if (pd->pausing)
	set_button_base(w, ss->sgx->red);
      else 
	if (sp->cursor_follows_play != DONT_FOLLOW)
	  set_button_base(w, ss->sgx->green); 
	else set_button_base(w, ss->sgx->white);
    }
}

void play_button_pause(bool pausing)
{
  pause_data *pd;
  pd = (pause_data *)CALLOC(1, sizeof(pause_data));
  pd->pausing = pausing;
  for_each_sound(set_play_button_pause, (void *)pd);
  FREE(pd);
}

static void set_sync_color(snd_info *sp)
{
  GtkWidget *syb;
  syb = SYNC_BUTTON(sp);
  switch (sp->sync)
    {
    case 0:  set_button_base(syb, ss->sgx->white);               break;
    case 1:  set_button_base(syb, ss->sgx->pushed_button_color); break;
    case 2:  set_button_base(syb, ss->sgx->green);               break;
    case 3:  set_button_base(syb, ss->sgx->yellow);              break;
    case 4:  set_button_base(syb, ss->sgx->red);                 break;
    default: set_button_base(syb, ss->sgx->black);               break;
    }
}

void syncb(snd_info *sp, int on)
{
  sp->sync = on;
  if (!(IS_PLAYER(sp)))
    {
      set_sync_color(sp);
      set_toggle_button(SYNC_BUTTON(sp), (bool)on, false, (void *)sp);
    }
}

static int last_sync_state = 0;

static gboolean sync_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_sync_state = ev->state;
  return(false);
}

static void sync_button_click(GtkWidget *w, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  bool on;
  on = (bool)(GTK_TOGGLE_BUTTON(w)->active);
  if (on)
    if (last_sync_state & snd_ControlMask) 
      if (last_sync_state & snd_MetaMask)
	if (last_sync_state & snd_ShiftMask)
	  sp->sync = 4;
	else sp->sync = 3;
      else sp->sync = 2;
    else sp->sync = 1;
  else sp->sync = 0;
  set_sync_color(sp);
  if (sp->sync != 0) 
    {
      chan_info *cp;
      cp = sp->lacp;
      if (cp == NULL) cp = any_selected_channel(sp);
      goto_graph(cp);
      if (cp->cursor_on) cursor_moveto(cp, CURSOR(cp));
      apply_x_axis_change(cp->axis, cp);
    }
}

static int last_combine_state = 0;

static gboolean unite_button_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* click if set unsets, click if unset->combine, ctrl-click->superimpose */
  last_combine_state = ev->state;
  return(false);
}

static void unite_button_click(GtkWidget *w, gpointer data)
{
  channel_style_t val;
  bool on;
  snd_info *sp = (snd_info *)data;
  on = (bool)(GTK_TOGGLE_BUTTON(w)->active);
  if (on)
    {
      if (last_combine_state & (snd_ControlMask | snd_MetaMask)) 
	val = CHANNELS_SUPERIMPOSED;
      else val = CHANNELS_COMBINED;
    }
  else val = CHANNELS_SEPARATE;
  set_sound_channel_style(sp, val);
  last_combine_state = 0;
}

static gboolean name_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  sp_name_click((snd_info *)data);
  return(false);
}


/* -------- AMP CALLBACKS -------- */

Float amp_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  if (val >= 1.0)
    return(0.9 * 0.5 * (1.0 + (val - 1.0) / (maxval - 1.0)));
  return(0.9 * 0.5 * ((val - minval) / (1.0 - minval)));
}

static Float scroll_to_amp(snd_info *sp, Float val)
{
  char amp_number_buffer[6];
  if (val <= 0.0) 
    sp->amp_control = sp->amp_control_min;
  else
    {
      if (val >= 0.9)
	sp->amp_control = sp->amp_control_max;
      else
	{
	  if (val > (0.5 * 0.9))
	    sp->amp_control = (((val / (0.5 * 0.9)) - 1.0) * (sp->amp_control_max - 1.0)) + 1.0;
	  else sp->amp_control = (val * (1.0 - sp->amp_control_min) / (0.5 * 0.9)) + sp->amp_control_min;
	}
    }
  mus_snprintf(amp_number_buffer, 6, "%.3f", sp->amp_control);
  gtk_label_set_text(GTK_LABEL(AMP_LABEL(sp)), amp_number_buffer);
  return(val);
}

void set_amp(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->amp_control = amp;
  else 
    {
      Float scrollval;
      GtkObject *adj;
      scroll_to_amp(sp, scrollval = amp_to_scroll(sp->amp_control_min, amp, sp->amp_control_max));
      adj = AMP_ADJUSTMENT(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

static gboolean amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_amp(sp, sp->last_amp_control);
  else set_amp(sp, 1.0);
  return(false);
}

static void amp_changed_callback(GtkAdjustment *adj, gpointer data)
{
  scroll_to_amp((snd_info *)data, (Float)(adj->value));
}

static gboolean amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_amp_control = sp->saved_amp_control;
  sp->saved_amp_control = sp->amp_control;
  return(false);
}


/* -------- SPEED CALLBACKS -------- */

static Float speed_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * ((log(val) - log(minval)) / (log(maxval) - log(minval))));
}

static Float scroll_to_speed(snd_info *sp, Float ival)
{
  char speed_number_buffer[6];
  sp->speed_control = speed_changed(exp((ival * (log(sp->speed_control_max) - log(sp->speed_control_min)) / 0.9) + log(sp->speed_control_min)),
				    speed_number_buffer,
				    sp->speed_control_style,
				    sp->speed_control_tones,
				    6);
  gtk_label_set_text(GTK_LABEL(SPEED_LABEL(sp)), speed_number_buffer);
  return(ival);
}

static bool ignore_callback = false;
void set_speed(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->speed_control = val;
  else
    {
      GtkObject *adj;
      adj = SPEED_ADJUSTMENT(sp);
      ignore_callback = true;
      GTK_ADJUSTMENT(adj)->value = scroll_to_speed(sp, speed_to_scroll(sp->speed_control_min, val, sp->speed_control_max));
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
      ignore_callback = false;
    }
}

static gboolean speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_speed(sp, sp->last_speed_control);
  else set_speed(sp, 1.0);
#if HAVE_RATIOS
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
  return(false);
}

static void speed_changed_callback(GtkAdjustment *adj, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (ignore_callback) return;
  scroll_to_speed(sp, adj->value);
#if HAVE_RATIOS
  if (sp->speed_control_style == SPEED_CONTROL_AS_RATIO)
    snd_rationalize(sp->speed_control, &(sp->speed_control_numerator), &(sp->speed_control_denominator));
#endif
}

static gboolean speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_speed_control = sp->saved_speed_control;
  sp->saved_speed_control = sp->speed_control;
  return(false);
}

static void draw_speed_arrow(snd_info *sp)
{
  if (sp->speed_control_direction == 1)
    gdk_draw_drawable(GDK_DRAWABLE(SPEED_ARROW(sp)->window), ss->sgx->basic_gc, speed_r, 0, 0, 0, 4, 18, 16);
  else gdk_draw_drawable(GDK_DRAWABLE(SPEED_ARROW(sp)->window), ss->sgx->basic_gc, speed_l, 0, 0, 0, 4, 18, 16);
}

static gboolean speed_arrow_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (sp->speed_control_direction == 1)
    sp->speed_control_direction = -1;
  else sp->speed_control_direction = 1;
  draw_speed_arrow(sp);
  return(false);
}
static gboolean speed_arrow_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  draw_speed_arrow((snd_info *)data);
  return(false);
}

void toggle_direction_arrow(snd_info *sp, bool state)
{
  int dir = 1;
  if (state) dir = -1;
  sp->speed_control_direction = dir;
  if (!(IS_PLAYER(sp))) draw_speed_arrow(sp);
}


/* -------- EXPAND CALLBACKS -------- */

static Float expand_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * ((log(val) - log(minval)) / (log(maxval) - log(minval))));
}

static Float scroll_to_expand(snd_info *sp, Float val)
{
  char expand_number_buffer[6];
  if (val <= 0.0) 
    sp->expand_control = sp->expand_control_min;
  else
    {
      if (val >= 0.9)
	sp->expand_control = sp->expand_control_max;
      else sp->expand_control = exp((val * (log(sp->expand_control_max) - log(sp->expand_control_min)) / 0.9) + log(sp->expand_control_min));
    }
  if (sp->playing) dac_set_expand(sp, sp->expand_control);
  mus_snprintf(expand_number_buffer, 6, "%.3f", sp->expand_control);
  gtk_label_set_text(GTK_LABEL(EXPAND_LABEL(sp)), expand_number_buffer);
  return(val);
}

void set_expand(snd_info *sp, Float val) 
{
  if (IS_PLAYER(sp))
    sp->expand_control = val;
  else
    {
      GtkObject *adj;
      adj = EXPAND_ADJUSTMENT(sp);
      ignore_callback = true;
      GTK_ADJUSTMENT(adj)->value = scroll_to_expand(sp, expand_to_scroll(sp->expand_control_min, val, sp->expand_control_max));
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
      ignore_callback = false;
    }
}

static void expand_changed_callback(GtkAdjustment *adj, gpointer data)
{
  if (ignore_callback) return;
  scroll_to_expand((snd_info *)data, adj->value);
}

static gboolean expand_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_expand_control = sp->saved_expand_control;
  sp->saved_expand_control = sp->expand_control;
  return(false);
}

static gboolean expand_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_expand(sp, sp->last_expand_control);
  else set_expand(sp, 1.0);
  return(false);
}

static void expand_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->expand_control_p = GTK_TOGGLE_BUTTON(w)->active;
  /* to change the trough color: (gtk_widget_modify_bg (list-ref (channel-widgets) 3) GTK_STATE_ACTIVE (zoom-color)) */
  /*   and the slider color:     (gtk_widget_modify_bg (list-ref (channel-widgets) 3) GTK_STATE_PRELIGHT (highlight-color)) */
}

void toggle_expand_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->expand_control_p = state;
  else set_toggle_button(EXPAND_BUTTON(sp), state, true, (void *)sp);
}


/* -------- CONTRAST CALLBACKS -------- */

static Float contrast_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return((val - minval) / (maxval - minval) * 0.9);
}

static Float scroll_to_contrast(snd_info *sp, Float val)
{
  char contrast_number_buffer[6];
  sp->contrast_control = sp->contrast_control_min + val * (sp->contrast_control_max - sp->contrast_control_min) / 0.9;
  mus_snprintf(contrast_number_buffer, 6, "%.3f", sp->contrast_control);
  gtk_label_set_text(GTK_LABEL(CONTRAST_LABEL(sp)), contrast_number_buffer);
  return(val);
}

void set_contrast(snd_info *sp, Float val) 
{
  if (IS_PLAYER(sp))
    sp->contrast_control = val;
  else
    {
      GtkObject *adj;
      adj = CONTRAST_ADJUSTMENT(sp);
      ignore_callback = true;
      GTK_ADJUSTMENT(adj)->value = scroll_to_contrast(sp, contrast_to_scroll(sp->contrast_control_min, val, sp->contrast_control_max));
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
      ignore_callback = false;
    }
}

static gboolean contrast_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_contrast(sp, sp->last_contrast_control);
  else set_contrast(sp, 0.0);
  return(false);
}

static void contrast_changed_callback(GtkAdjustment *adj, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (ignore_callback) return;
  scroll_to_contrast(sp, adj->value);
}

static gboolean contrast_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_contrast_control = sp->saved_contrast_control;
  sp->saved_contrast_control = sp->contrast_control;
  return(false);
}

static void contrast_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->contrast_control_p = GTK_TOGGLE_BUTTON(w)->active;
}


void toggle_contrast_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->contrast_control_p = state;
  else set_toggle_button(CONTRAST_BUTTON(sp), state, true, (void *)sp);
}


/* -------- REVERB CALLBACKS -------- */

static Float revscl_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * (pow(val, 0.333) - pow(minval, 0.333)) / (pow(maxval, 0.333) - pow(minval, 0.333)));
}

static Float cube (Float a) {return(a*a*a);}

static Float scroll_to_revscl(snd_info *sp, Float val)
{
  char revscl_number_buffer[7];
  if (val <= 0.0) 
    sp->reverb_control_scale = sp->reverb_control_scale_min;
  else
    {
      if (val >= 0.9)
	sp->reverb_control_scale = sp->reverb_control_scale_max;
      else sp->reverb_control_scale = cube((val * (pow(sp->reverb_control_scale_max, 0.333) - pow(sp->reverb_control_scale_min, 0.333)) / 0.9) + 
					   pow(sp->reverb_control_scale_min, 0.333));
    }
  mus_snprintf(revscl_number_buffer, 7, "%.4f", sp->reverb_control_scale);
  gtk_label_set_text(GTK_LABEL(REVSCL_LABEL(sp)), revscl_number_buffer);
  return(val);
}

void set_revscl(snd_info *sp, Float val) 
{
  if (IS_PLAYER(sp))
    sp->reverb_control_scale = val;
  else
    {
      GtkObject *adj;
      adj = REVSCL_ADJUSTMENT(sp);
      ignore_callback = true;
      GTK_ADJUSTMENT(adj)->value = scroll_to_revscl(sp, revscl_to_scroll(sp->reverb_control_scale_min, val, sp->reverb_control_scale_max));
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
      ignore_callback = false;
    }
}

static gboolean revscl_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_revscl(sp, sp->last_reverb_control_scale);
  else set_revscl(sp, 0.0);
  return(false);
}

static void revscl_changed_callback(GtkAdjustment *adj, gpointer data)
{
  if (ignore_callback) return;
  scroll_to_revscl((snd_info *)data, adj->value);
}

static gboolean revscl_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_reverb_control_scale = sp->saved_reverb_control_scale;
  sp->saved_reverb_control_scale = sp->reverb_control_scale;
  return(false);
}


static Float revlen_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return((val - minval) / (maxval - minval) * 0.9);
}

static Float scroll_to_revlen(snd_info *sp, Float val)
{
  char revlen_number_buffer[5];
  sp->reverb_control_length = sp->reverb_control_length_min + 
    (sp->reverb_control_length_max - sp->reverb_control_length_min) * (Float)val / 0.9;
  mus_snprintf(revlen_number_buffer, 5, "%.2f", sp->reverb_control_length);
  gtk_label_set_text(GTK_LABEL(REVLEN_LABEL(sp)), revlen_number_buffer);
  return(val);
}

void set_revlen(snd_info *sp, Float val)
{
  if (IS_PLAYER(sp))
    sp->reverb_control_length = val;
  else
    {
      GtkObject *adj;
      adj = REVLEN_ADJUSTMENT(sp);
      ignore_callback = true;
      GTK_ADJUSTMENT(adj)->value = scroll_to_revlen(sp, revlen_to_scroll(sp->reverb_control_length_min, val, sp->reverb_control_length_max));
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
      ignore_callback = false;
    }
}

static gboolean revlen_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_revlen(sp, sp->last_reverb_control_length);
  else set_revlen(sp, 1.0);
  return(false);
}

static void revlen_changed_callback(GtkAdjustment *adj, gpointer data)
{
  if (ignore_callback) return;
  scroll_to_revlen((snd_info *)data, adj->value);
}

static gboolean revlen_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_reverb_control_length = sp->saved_reverb_control_length;
  sp->saved_reverb_control_length = sp->reverb_control_length;
  return(false);
}

static void reverb_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->reverb_control_p = GTK_TOGGLE_BUTTON(w)->active;
}

void toggle_reverb_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->reverb_control_p = state;
  else set_toggle_button(REVERB_BUTTON(sp), state, true, (void *)sp);
}


/* -------- FILTER CALLBACKS -------- */

#define MIN_FILTER_GRAPH_HEIGHT 20

void display_filter_env(snd_info *sp)
{
  axis_context *ax;
  int height, width;
  GtkWidget *drawer;
  env_editor *edp;
  if (IS_PLAYER(sp)) return;
  edp = sp->sgx->flt;
  drawer = filter_graph(sp);
  height = widget_height(drawer);
  if (height < MIN_FILTER_GRAPH_HEIGHT) return;
  width = widget_width(drawer);
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->gc = ss->sgx->fltenv_basic_gc;
  ax->wn = drawer->window;
  ax->w = drawer;
  gdk_window_clear(ax->wn);
  edp->in_dB = sp->filter_control_in_dB;
  edp->with_dots = true;
  if (sp->filter_control_envelope == NULL) sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  env_editor_display_env(edp, sp->filter_control_envelope, ax, _("frequency response"), 0, 0, width, height, NOT_PRINTING);
  if (edp->edited)
    {
      ax->gc = ss->sgx->fltenv_data_gc;
      display_frequency_response(sp->filter_control_envelope, 
				 (SOUND_ENV_EDITOR(sp))->axis, ax, 
				 sp->filter_control_order, 
				 sp->filter_control_in_dB);
    }
  ax = free_axis_context(ax);
}

static gboolean filter_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  if (ev->state & GDK_BUTTON1_MASK)
    {
      snd_info *sp = (snd_info *)data;
      int evx, evy;
      GdkModifierType state;
      env_editor *edp;
      if (ev->is_hint)
	gdk_window_get_pointer(ev->window, &evx, &evy, &state);
      else
	{
	  evx = (int)(ev->x);
	  evy = (int)(ev->y);
	}
      edp = sp->sgx->flt;
      edp->in_dB = sp->filter_control_in_dB;
      env_editor_button_motion(edp, evx, evy, ev->time, sp->filter_control_envelope);
      display_filter_env(sp);
      sp->filter_control_changed = true;
    }
  return(false);
}

static gboolean filter_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  env_editor *edp;
  edp = sp->sgx->flt;
  edp->in_dB = sp->filter_control_in_dB;
  if (env_editor_button_press(edp, (int)(ev->x), (int)(ev->y), ev->time, sp->filter_control_envelope))
    display_filter_env(sp);
  return(false);
}

static gboolean filter_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  char *tmpstr = NULL;
  snd_info *sp = (snd_info *)data;
  env_editor_button_release(SOUND_ENV_EDITOR(sp), sp->filter_control_envelope);
  display_filter_env(sp);
  set_filter_text(sp, tmpstr = env_to_string(sp->filter_control_envelope));
  if (tmpstr) FREE(tmpstr);
  sp->filter_control_changed = true;
  return(false);
}

void set_filter_text(snd_info *sp, char *str)
{
  if (!(IS_PLAYER(sp)))
    {
      if (str)
	gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), str);
      else gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), stupid);
    }
}

static gboolean filter_drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  display_filter_env(sp);
  return(false);
}

static gboolean filter_drawer_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  display_filter_env(sp);
  return(false);
}

static void filter_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->filter_control_p = GTK_TOGGLE_BUTTON(w)->active;
}

void toggle_filter_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->filter_control_p = state;
  else set_toggle_button(FILTER_BUTTON(sp), state, true, (void *)sp);
}

static void filter_db_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->filter_control_in_dB = GTK_TOGGLE_BUTTON(w)->active;
  display_filter_env(sp);
}

void set_filter_in_dB(snd_info *sp, bool val)
{
  sp->filter_control_in_dB = val;
  if (!(IS_PLAYER(sp)))
    {
      set_toggle_button(FILTER_DB_BUTTON(sp), val, false, (void *)sp);
      display_filter_env(sp);
    }
}

static void new_in_hz(snd_info *sp, bool val)
{
  sp->filter_control_in_hz = val;
  if (val)
    sp->filter_control_xmax = (Float)(SND_SRATE(sp) / 2);
  else sp->filter_control_xmax = 1.0;
  if (sp->filter_control_envelope) free_env(sp->filter_control_envelope);
  sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
}

static void filter_hz_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  new_in_hz(sp, GTK_TOGGLE_BUTTON(w)->active);
  display_filter_env(sp);
}

void set_filter_in_hz(snd_info *sp, bool val)
{
  new_in_hz(sp, val);
  if (!(IS_PLAYER(sp)))
    {
      set_toggle_button(FILTER_HZ_BUTTON(sp), val, false, (void *)sp);
      display_filter_env(sp);
    }
}

static void set_filter_order_1(snd_info *sp, int order, bool setadj)
{
  sp->filter_control_order = order;
  if (setadj)
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(FILTER_ORDER_TEXT(sp)), (gfloat)order);
  display_filter_env(sp);
  sp->filter_control_changed = true;
}  

void set_filter_order(snd_info *sp, int order) 
{
  if (IS_PLAYER(sp))
    sp->filter_control_order = order;
  else set_filter_order_1(sp, order, true);
}

static void filter_order_callback(GtkWidget *w, gpointer data)
{
  int order;
  snd_info *sp = (snd_info *)data;
  order = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(FILTER_ORDER_TEXT(sp)));
  if (order & 1) order++;
  if (order <= 0) order = 2;
  sp->filter_control_order = order;
  set_filter_order_1(sp, sp->filter_control_order, false);
}

static void filter_activate_callback(GtkWidget *w, gpointer context)
{
  /* make an envelope out of the data */
  snd_info *sp = (snd_info *)context;
  char *str = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  if (sp->filter_control_envelope) sp->filter_control_envelope = free_env(sp->filter_control_envelope);
  sp->filter_control_envelope = string_to_env(str);
  if (!(sp->filter_control_envelope)) /* maybe user cleared text field? */
    sp->filter_control_envelope = default_env(sp->filter_control_xmax, 1.0);
  (SOUND_ENV_EDITOR(sp))->edited = true;
  display_filter_env(sp);
  sp->filter_control_changed = true;
}

void filter_env_changed(snd_info *sp, env *e)
{
  /* turn e back into a string for textfield widget */
  if (!(IS_PLAYER(sp)))
    {
      char *tmpstr;
      tmpstr = env_to_string(e);
      if (tmpstr)
	{
	  gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), tmpstr);
	  FREE(tmpstr);
	}
      else gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), stupid);
      (SOUND_ENV_EDITOR(sp))->edited = true;
      display_filter_env(sp);
      /* this is called also from snd-scm.c */
    }
  sp->filter_control_changed = true;
}

void color_filter_waveform(GdkColor *color)
{
  int i;
  gdk_gc_set_foreground(ss->sgx->fltenv_data_gc, color);
  ss->sgx->filter_control_waveform_color = color;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	display_filter_env(sp);
    }
}


/* -------- AMP ENVS ETC -------- */

static int cant_write(char *name)
{
#if HAVE_ACCESS
  return((access(name, W_OK)) != 0);
#else
  return(0);
#endif
}

void reflect_amp_env_completion(snd_info *sp)
{
  int i;
  GtkWidget *info_sep;
  /* a channel completed an amp env, check to see if all are complete */
  for (i = 0; i < sp->nchans; i++)
    {
      chan_info *cp;
      env_info *ep;
      cp = sp->chans[i];
      if (!(cp->amp_envs)) return;
      ep = cp->amp_envs[cp->edit_ctr];
      if (!ep) return;
      if (!(ep->completed)) return;
    }
  info_sep = MINIBUFFER_SEPARATOR(sp);
  if (info_sep) gtk_widget_show(info_sep);
  alert_enved_amp_env(sp);
}

void reflect_amp_env_in_progress(snd_info *sp)
{
  if ((sp) && (sp->sgx))
    {
      GtkWidget *info_sep;
      info_sep = MINIBUFFER_SEPARATOR(sp);
      if (info_sep) gtk_widget_hide(info_sep);
    }
}

static gint close_sound_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  if (sp) snd_close_file(sp);
  gtk_widget_hide(sp->sgx->dialog); 
  return(true);
} 


/* -------- SOUND PANE -------- */

static bool showing_controls = false;

snd_info *add_sound_window(char *filename, bool read_only, file_info *hdr)
{
  snd_info *sp, *osp;
  GtkWidget **sw;
  GtkObject **adjs;
  int snd_slot, nchans, i, k, old_chans;
  bool free_filename = false, make_widgets;
  char *old_name = NULL, *title;
  int app_y, app_dy, screen_y, chan_min_y;
  /* these dimensions are used to try to get a reasonable channel graph size without falling off the screen bottom */
  snd_context *sx;
  if (ss->translated_filename) 
    {
      old_name = filename;
      filename = ss->translated_filename;
      ss->translated_filename = NULL;
      free_filename = true;
    }
  nchans = hdr->chans;
  if (nchans <= 0) nchans = 1;
  app_y = widget_y(MAIN_SHELL(ss));
  app_dy = widget_height(MAIN_SHELL(ss));
  if (auto_resize(ss))
    {
      screen_y = gdk_screen_height();
      app_dy = (screen_y - app_y - app_dy - 20 * nchans);
    }
  else app_dy -= listener_height();
  chan_min_y = app_dy / nchans;
  if (chan_min_y > ss->channel_min_height)
    chan_min_y = ss->channel_min_height; 
  else 
    if (chan_min_y < 5) 
      chan_min_y = 5;

  snd_slot = find_free_sound_slot(nchans); /* expands sound list if needed */
  if (ss->sounds[snd_slot])                    /* we're trying to re-use an old, inactive set of widgets and whatnot */
    {
      osp = ss->sounds[snd_slot];
      old_chans = osp->allocated_chans;
    }
  else old_chans = 0;
  make_widgets = (ss->sounds[snd_slot] == NULL);
  ss->sounds[snd_slot] = make_snd_info(ss->sounds[snd_slot], filename, hdr, snd_slot, read_only);
  sp = ss->sounds[snd_slot];
  sp->inuse = SOUND_NORMAL;
  sx = sp->sgx;
  sx->file_pix = blank;
  sp->bomb_ctr = 0;
  make_pixmaps();
  if (sx->snd_widgets == NULL)
    {
      sw = (GtkWidget **)CALLOC(NUM_SND_WIDGETS, sizeof(GtkWidget *));
      adjs = (GtkObject **)CALLOC(NUM_SND_ADJS, sizeof(GtkObject *));
      sx->snd_widgets = sw; 
      sx->snd_adjs = adjs;
    }
  else
    {
      sw = sx->snd_widgets;
      adjs = sx->snd_adjs;
    }
  if (!(auto_resize(ss))) gtk_window_set_resizable(GTK_WINDOW(MAIN_SHELL(ss)), false);
  if ((!make_widgets) && (old_chans < nchans))
    {
      for (i = old_chans; i < nchans; i++) 
	add_channel_window(sp, i, chan_min_y, 1, NULL, WITH_FW_BUTTONS, WITH_EVENTS);
    }
  if (make_widgets)
    {
      sw[W_pane] = gtk_vpaned_new();
      set_user_int_data(G_OBJECT(sw[W_pane]), sp->index);
      gtk_container_set_border_width(GTK_CONTAINER(sw[W_pane]), 0);
      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	{
	  sx->dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	  sg_make_resizable(sx->dialog);
	  gtk_container_add(GTK_CONTAINER(sx->dialog), sw[W_pane]);
	  gtk_widget_show(sx->dialog);
	  SG_SIGNAL_CONNECT(sx->dialog, "delete_event", close_sound_dialog, sp);
	}
      else
	{
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	    {
	      GtkWidget *tablab;
	      tablab = gtk_label_new(sp->short_filename);
	      gtk_widget_show(tablab);
	      gtk_notebook_append_page(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), sw[W_pane], tablab);
	    }
	  else gtk_box_pack_start(GTK_BOX(SOUND_PANE_BOX(ss)), sw[W_pane], true, true, 0);
	  /* child2 is listener */
	}
      sw[W_pane_box] = gtk_vbox_new(false, 0);
      gtk_paned_add1(GTK_PANED(sw[W_pane]), sw[W_pane_box]);
      gtk_widget_show(sw[W_pane_box]);

      ERROR_INFO_FRAME(sp) = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(sw[W_pane_box]), ERROR_INFO_FRAME(sp), false, false, 10);

      NAME_BOX(sp) = gtk_hbox_new(false, 0);
      gtk_box_pack_end(GTK_BOX(sw[W_pane_box]), NAME_BOX(sp), false, false, 0);
      
      for (i = 0; i < nchans; i++) 
	add_channel_window(sp, i, chan_min_y, 0, NULL, WITH_FW_BUTTONS, WITH_EVENTS);

      /* controls etc */

      sw[W_control_panel] = gtk_vbox_new(false, 0);
      gtk_paned_add2(GTK_PANED(sw[W_pane]), sw[W_control_panel]);
  

      /* -------- NAME FIELDS -------- */

      sw[W_name_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(NAME_BOX(sp)), sw[W_name_event], false, false, 5);
      gtk_widget_show(sw[W_name_event]);
      SG_SIGNAL_CONNECT(sw[W_name_event], "button_press_event", name_click_callback, sp);
      
      sw[W_name] = gtk_label_new(shortname_indexed(sp));
      gtk_container_add(GTK_CONTAINER(sw[W_name_event]), sw[W_name]);
      gtk_widget_show(sw[W_name]);
      
      NAME_PIX(sp) = gtk_drawing_area_new();
      gtk_widget_set_events(NAME_PIX(sp), GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(NAME_PIX(sp), 16, 16);
      gtk_box_pack_start(GTK_BOX(NAME_BOX(sp)), NAME_PIX(sp), false, false, 0);
      gtk_widget_show(NAME_PIX(sp));
      SG_SIGNAL_CONNECT(NAME_PIX(sp), "expose_event", name_pix_expose, sp);

      MINIBUFFER_SEPARATOR(sp) = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(NAME_BOX(sp)), MINIBUFFER_SEPARATOR(sp), false, false, 4);
      gtk_widget_show(MINIBUFFER_SEPARATOR(sp));
      
      MINIBUFFER_LABEL(sp) = gtk_label_new(NULL);
      gtk_box_pack_start(GTK_BOX(NAME_BOX(sp)), MINIBUFFER_LABEL(sp), false, false, 0);
      gtk_widget_show(MINIBUFFER_LABEL(sp));
      
      MINIBUFFER_TEXT(sp) = snd_entry_new(NAME_BOX(sp), false);
      SG_SIGNAL_CONNECT(MINIBUFFER_TEXT(sp), "key_press_event", minibuffer_key_callback, sp);
      SG_SIGNAL_CONNECT(MINIBUFFER_TEXT(sp), "activate", minibuffer_activate_callback, sp);
      SG_SIGNAL_CONNECT(MINIBUFFER_TEXT(sp), "enter_notify_event", minibuffer_mouse_enter, sp);
      SG_SIGNAL_CONNECT(MINIBUFFER_TEXT(sp), "leave_notify_event", minibuffer_mouse_leave, sp);

      /* now fill from other end */
      
      PLAY_BUTTON(sp) = gtk_check_button_new_with_label(_("play"));
      gtk_box_pack_end(GTK_BOX(NAME_BOX(sp)), PLAY_BUTTON(sp), false, false, 0);
      SG_SIGNAL_CONNECT(PLAY_BUTTON(sp), "button_press_event", play_button_callback, sp);
      SG_SIGNAL_CONNECT(PLAY_BUTTON(sp), "toggled", play_button_click_callback, sp);
      gtk_widget_show(PLAY_BUTTON(sp));
      
      SYNC_BUTTON(sp) = gtk_check_button_new_with_label(_("sync"));
      gtk_box_pack_end(GTK_BOX(NAME_BOX(sp)), SYNC_BUTTON(sp), false, false, 0);
      SG_SIGNAL_CONNECT(SYNC_BUTTON(sp), "button_press_event", sync_button_callback, sp);
      SG_SIGNAL_CONNECT(SYNC_BUTTON(sp), "toggled", sync_button_click, sp);
      gtk_widget_show(SYNC_BUTTON(sp));
      
      UNITE_BUTTON(sp) = gtk_check_button_new_with_label(_("unite"));
      gtk_box_pack_end(GTK_BOX(NAME_BOX(sp)), UNITE_BUTTON(sp), false, false, 0);
      SG_SIGNAL_CONNECT(UNITE_BUTTON(sp), "button_press_event", unite_button_callback, sp);
      SG_SIGNAL_CONNECT(UNITE_BUTTON(sp), "toggled", unite_button_click, sp);
      gtk_widget_show(UNITE_BUTTON(sp));
      
      gtk_widget_show(NAME_BOX(sp));

      /* -------- minibuffer error display -------- */

      ERROR_INFO(sp) = snd_gtk_label_new(NULL, ss->sgx->highlight_color);
      gtk_container_add(GTK_CONTAINER(ERROR_INFO_FRAME(sp)), ERROR_INFO(sp));
      gtk_widget_show(ERROR_INFO(sp));

      /* if control-panel */
      
      /* -------- AMP FIELDS -------- */
      
      sw[W_amp_sep] = gtk_hseparator_new();
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_amp_sep], false, false, 4);
      gtk_widget_show(sw[W_amp_sep]);
      
      sw[W_amp_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_amp_form], false, false, 0);
      
      sw[W_amp_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]), sw[W_amp_event], false, false, 4);
      gtk_widget_show(sw[W_amp_event]);
      SG_SIGNAL_CONNECT(sw[W_amp_event], "button_press_event", amp_click_callback, sp);
      
      sw[W_amp_label] = gtk_label_new(_("amp:"));
      gtk_container_add(GTK_CONTAINER(sw[W_amp_event]), sw[W_amp_label]);
      gtk_widget_show(sw[W_amp_label]);
      
      sw[W_amp_number] = gtk_label_new("1.00 ");
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]), sw[W_amp_number], false, false, 0);
      gtk_widget_show(sw[W_amp_number]);
      
      adjs[W_amp_adj] = gtk_adjustment_new(amp_to_scroll(sp->amp_control_min, 1.0, sp->amp_control_max), 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_amp] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_amp_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]), sw[W_amp], true, true, 4);
      SG_SIGNAL_CONNECT(adjs[W_amp_adj], "value_changed", amp_changed_callback, sp);
      SG_SIGNAL_CONNECT(sw[W_amp], "button_release_event", amp_release_callback, sp);
      gtk_widget_show(sw[W_amp]);
      
      gtk_widget_show(sw[W_amp_form]);
      
      
      /* -------- SPEED FIELDS -------- */
      
      sw[W_speed_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_speed_form], false, false, 0);
      
      sw[W_speed_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_speed_form]), sw[W_speed_event], false, false, 4);
      gtk_widget_show(sw[W_speed_event]);
      SG_SIGNAL_CONNECT(sw[W_speed_event], "button_press_event", speed_click_callback, sp);
      
      sw[W_speed_label] = gtk_label_new(_("speed:"));
      gtk_container_add(GTK_CONTAINER(sw[W_speed_event]), sw[W_speed_label]);
      gtk_widget_show(sw[W_speed_label]);
      
      switch (sp->speed_control_style)
	{
	case SPEED_CONTROL_AS_RATIO: sw[W_speed_number] = gtk_label_new("  1/1"); break;
	case SPEED_CONTROL_AS_SEMITONE: sw[W_speed_number] = gtk_label_new("    0"); break;
	default:  sw[W_speed_number] = gtk_label_new("1.00 "); break;
	}
      gtk_box_pack_start(GTK_BOX(sw[W_speed_form]), sw[W_speed_number], false, false, 0);
      gtk_widget_show(sw[W_speed_number]);
      
      adjs[W_speed_adj] = gtk_adjustment_new(speed_to_scroll(sp->speed_control_min, 1.0, sp->speed_control_max), 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_speed] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_speed_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_speed_form]), sw[W_speed], true, true, 4);
      SG_SIGNAL_CONNECT(adjs[W_speed_adj], "value_changed", speed_changed_callback, sp);
      SG_SIGNAL_CONNECT(sw[W_speed], "button_release_event", speed_release_callback, sp);
      gtk_widget_show(sw[W_speed]);

      sw[W_speed_pix] = gtk_drawing_area_new();
      gtk_widget_set_events(sw[W_speed_pix], GDK_ALL_EVENTS_MASK);
      gtk_box_pack_start(GTK_BOX(sw[W_speed_form]), sw[W_speed_pix], false, false, 2);
      gtk_widget_set_size_request(sw[W_speed_pix], 18, 16);
      gtk_widget_show(sw[W_speed_pix]);
      SG_SIGNAL_CONNECT(sw[W_speed_pix], "expose_event", speed_arrow_expose, sp);
      SG_SIGNAL_CONNECT(sw[W_speed_pix], "button_press_event", speed_arrow_press, sp);

      gtk_widget_show(sw[W_speed_form]);
      
      
      /* -------- EXPAND FIELDS -------- */
      
      sw[W_expand_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_expand_form], false, false, 0);
      
      sw[W_expand_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]), sw[W_expand_event], false, false, 4);
      gtk_widget_show(sw[W_expand_event]);
      SG_SIGNAL_CONNECT(sw[W_expand_event], "button_press_event", expand_click_callback, sp);
      
      sw[W_expand_label] = gtk_label_new(_("expand:"));
      gtk_container_add(GTK_CONTAINER(sw[W_expand_event]), sw[W_expand_label]);
      gtk_widget_show(sw[W_expand_label]);
      
      sw[W_expand_number] = gtk_label_new("1.00 ");
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]), sw[W_expand_number], false, false, 0);
      gtk_widget_show(sw[W_expand_number]);
      
      adjs[W_expand_adj] = gtk_adjustment_new(expand_to_scroll(sp->expand_control_min, 1.0, sp->expand_control_max), 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_expand] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_expand_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]), sw[W_expand], true, true, 4);
      SG_SIGNAL_CONNECT(adjs[W_expand_adj], "value_changed", expand_changed_callback, sp);
      SG_SIGNAL_CONNECT(sw[W_expand], "button_release_event", expand_release_callback, sp);
      gtk_widget_show(sw[W_expand]);
      
      sw[W_expand_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]), sw[W_expand_button], false, false, 0);
      SG_SIGNAL_CONNECT(sw[W_expand_button], "clicked", expand_button_callback, sp);
      gtk_widget_show(sw[W_expand_button]);
      
      gtk_widget_show(sw[W_expand_form]);
      
      
      /* -------- CONTRAST FIELDS -------- */
      
      sw[W_contrast_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_contrast_form], false, false, 0);
      
      sw[W_contrast_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]), sw[W_contrast_event], false, false, 4);
      gtk_widget_show(sw[W_contrast_event]);
      SG_SIGNAL_CONNECT(sw[W_contrast_event], "button_press_event", contrast_click_callback, sp);
      
      sw[W_contrast_label] = gtk_label_new(_("contrast:"));
      gtk_container_add(GTK_CONTAINER(sw[W_contrast_event]), sw[W_contrast_label]);
      gtk_widget_show(sw[W_contrast_label]);
      
      sw[W_contrast_number] = gtk_label_new("0.00 ");
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]), sw[W_contrast_number], false, false, 0);
      gtk_widget_show(sw[W_contrast_number]);
      
      adjs[W_contrast_adj] = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_contrast] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_contrast_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]), sw[W_contrast], true, true, 4);
      SG_SIGNAL_CONNECT(adjs[W_contrast_adj], "value_changed", contrast_changed_callback, sp);
      SG_SIGNAL_CONNECT(sw[W_contrast], "button_release_event", contrast_release_callback, sp);
      gtk_widget_show(sw[W_contrast]);
      
      sw[W_contrast_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]), sw[W_contrast_button], false, false, 0);
      SG_SIGNAL_CONNECT(sw[W_contrast_button], "clicked", contrast_button_callback, sp);
      gtk_widget_show(sw[W_contrast_button]);
      
      gtk_widget_show(sw[W_contrast_form]);
      
      
      /* -------- REVERB FIELDS -------- */
      
      sw[W_reverb_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_reverb_form], false, false, 0);
      
      sw[W_revscl_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revscl_event], false, false, 4);
      gtk_widget_show(sw[W_revscl_event]);
      SG_SIGNAL_CONNECT(sw[W_revscl_event], "button_press_event", revscl_click_callback, sp);
      
      sw[W_revscl_label] = gtk_label_new(_("reverb:"));
      gtk_container_add(GTK_CONTAINER(sw[W_revscl_event]), sw[W_revscl_label]);
      gtk_widget_show(sw[W_revscl_label]);
      
      sw[W_revscl_number] = gtk_label_new("0.000 ");
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revscl_number], false, false, 0);
      gtk_widget_show(sw[W_revscl_number]);
      
      adjs[W_revscl_adj] = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_revscl] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_revscl_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revscl], true, true, 4);
      SG_SIGNAL_CONNECT(adjs[W_revscl_adj], "value_changed", revscl_changed_callback, sp);
      SG_SIGNAL_CONNECT(sw[W_revscl], "button_release_event", revscl_release_callback, sp);
      gtk_widget_show(sw[W_revscl]);
      
      sw[W_revlen_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revlen_event], false, false, 4);
      gtk_widget_show(sw[W_revlen_event]);
      SG_SIGNAL_CONNECT(sw[W_revlen_event], "button_press_event", revlen_click_callback, sp);
      
      sw[W_revlen_label] = gtk_label_new(_("len:"));
      gtk_container_add(GTK_CONTAINER(sw[W_revlen_event]), sw[W_revlen_label]);
      gtk_widget_show(sw[W_revlen_label]);
      
      sw[W_revlen_number] = gtk_label_new("1.0 ");
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revlen_number], false, false, 0);
      gtk_widget_show(sw[W_revlen_number]);
      
      adjs[W_revlen_adj] = gtk_adjustment_new(revlen_to_scroll(sp->reverb_control_length_min, 
							       sp->reverb_control_length, 
							       sp->reverb_control_length_max), 
					      0.0, 1.0, 0.001, 0.01, .1);
      sw[W_revlen] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_revlen_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revlen], true, true, 4);
      SG_SIGNAL_CONNECT(adjs[W_revlen_adj], "value_changed", revlen_changed_callback, sp);
      SG_SIGNAL_CONNECT(sw[W_revlen], "button_release_event", revlen_release_callback, sp);
      gtk_widget_show(sw[W_revlen]);
      
      sw[W_reverb_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_reverb_button], false, false, 0);
      SG_SIGNAL_CONNECT(sw[W_reverb_button], "clicked", reverb_button_callback, sp);
      gtk_widget_show(sw[W_reverb_button]);
      
      gtk_widget_show(sw[W_reverb_form]);
      
      
      /* -------- FILTER FIELDS -------- */
      
      sw[W_filter_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_filter_form], false, false, 0);
      
      sw[W_filter_label] = gtk_label_new(_("filter:"));
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_label], false, false, 4);
      gtk_widget_show(sw[W_filter_label]);
      
      adjs[W_filter_adj] = gtk_adjustment_new(20, 2, 100000, 2, 10, 0);
      sw[W_filter_order] = gtk_spin_button_new(GTK_ADJUSTMENT(adjs[W_filter_adj]), 0.0, 0);
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_order], false, false, 2);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(sw[W_filter_order]), true);
      SG_SIGNAL_CONNECT(adjs[W_filter_adj], "value_changed", filter_order_callback, sp);
      gtk_widget_show(sw[W_filter_order]);
      
      sw[W_filter] = snd_entry_new(sw[W_filter_form], false);
      SG_SIGNAL_CONNECT(sw[W_filter], "activate", filter_activate_callback, sp);

      sw[W_filter_hz] = gtk_check_button_new_with_label(_("hz"));
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_hz], false, false, 2);
      SG_SIGNAL_CONNECT(sw[W_filter_hz], "clicked", filter_hz_callback, sp);
      gtk_widget_show(sw[W_filter_hz]);
      
      sw[W_filter_dB] = gtk_check_button_new_with_label(_("dB"));
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_dB], false, false, 2);
      SG_SIGNAL_CONNECT(sw[W_filter_dB], "clicked", filter_db_callback, sp);
      gtk_widget_show(sw[W_filter_dB]);
      
      sw[W_filter_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_button], false, false, 0);
      SG_SIGNAL_CONNECT(sw[W_filter_button], "clicked", filter_button_callback, sp);
      gtk_widget_show(sw[W_filter_button]);
      
      gtk_widget_show(sw[W_filter_form]);
      
      
      /* -------- FILTER GRAPH -------- */
      
      sw[W_filter_frame] = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_filter_frame], true, true, 10);
      
      sw[W_filter_env] = gtk_drawing_area_new();
      gtk_widget_set_events(sw[W_filter_env], GDK_ALL_EVENTS_MASK);
      gtk_widget_modify_bg(sw[W_filter_env], GTK_STATE_NORMAL, ss->sgx->highlight_color);
      gtk_container_add(GTK_CONTAINER(sw[W_filter_frame]), sw[W_filter_env]);
      gtk_widget_show(sw[W_filter_env]);
      SG_SIGNAL_CONNECT(sw[W_filter_env], "expose_event", filter_drawer_expose, sp);
      SG_SIGNAL_CONNECT(sw[W_filter_env], "configure_event", filter_drawer_resize, sp);
      SG_SIGNAL_CONNECT(sw[W_filter_env], "button_press_event", filter_drawer_button_press, sp);
      SG_SIGNAL_CONNECT(sw[W_filter_env], "button_release_event", filter_drawer_button_release, sp);
      SG_SIGNAL_CONNECT(sw[W_filter_env], "motion_notify_event", filter_drawer_button_motion, sp);

      gtk_widget_show(sw[W_filter_frame]);
      sp->sgx->flt = new_env_editor();
      
      /* end if control-panel */
      gtk_widget_show(sw[W_control_panel]);
      gtk_widget_show(sw[W_pane]);
    } /* new sound ss */
  else
    { /* re-manage currently inactive chan */
      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS) 
	raise_dialog(sx->dialog);
      gtk_widget_show(UNITE_BUTTON(sp));
      gtk_widget_show(sw[W_pane]);
      for (k = 0; k < nchans; k++) 
	add_channel_window(sp, k, chan_min_y, 0, NULL, WITH_FW_BUTTONS, WITH_EVENTS);
      gtk_label_set_text(GTK_LABEL(sw[W_name]), shortname_indexed(sp));
      reset_user_int_data(G_OBJECT(sw[W_pane]), sp->index); /* is this necessary? */
      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	gtk_notebook_set_tab_label_text(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), sw[W_pane], sp->short_filename);
      else reset_controls(sp); /* segfault here in notebook case! */
    }
  gtk_window_set_resizable(GTK_WINDOW(MAIN_SHELL(ss)), true);
  if (showing_controls) sound_show_ctrls(sp); else sound_hide_ctrls(sp);

  if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
    {
      title = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(title, PRINT_BUFFER_SIZE, "%d: %s", snd_slot, sp->short_filename);
      gtk_window_set_title(GTK_WINDOW(sx->dialog), title);
      FREE(title);
    }

  if (sp->nchans == 1) 
    gtk_widget_hide(UNITE_BUTTON(sp));
  add_sound_data(filename, sp, WITH_GRAPH);
  if (cant_write(sp->filename)) sp->file_read_only = true;
  snd_file_lock_icon(sp, sp->user_read_only || sp->file_read_only);
  if (old_name)
    report_in_minibuffer(sp, _("(translated %s)"), old_name);
  after_open(sp->index);
  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
    {
      sx->page = gtk_notebook_page_num(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), sw[W_pane]);
      reset_controls(sp);
    }
  if (free_filename) FREE(filename);
  return(sp);
}

void set_sound_pane_file_label(snd_info *sp, char *str)
{
  if ((sp->name_string == NULL) || (strcmp(sp->name_string, str) != 0))
    {
      if (sp->name_string) FREE(sp->name_string);
      sp->name_string = copy_string(str);
      set_label(w_snd_name(sp), str);
    }
}

void snd_info_cleanup(snd_info *sp)
{
  if ((sp) && (sp->sgx))
    {
      snd_context *sx;
      sx = sp->sgx;
      clear_minibuffer_error(sp);
      if (SYNC_BUTTON(sp))
	{
	  set_toggle_button(SYNC_BUTTON(sp), false, false, (void *)sp);
	  set_sync_color(sp);
	  set_toggle_button(EXPAND_BUTTON(sp), false, false, (void *)sp);
	  set_toggle_button(CONTRAST_BUTTON(sp), false, false, (void *)sp);
	  set_toggle_button(FILTER_BUTTON(sp), false, false, (void *)sp);
	  set_toggle_button(REVERB_BUTTON(sp), false, false, (void *)sp);
	  set_toggle_button(unite_button(sp), false, false, (void *)sp);
	}
      if ((sx->dialog) && (GTK_WIDGET_VISIBLE(sx->dialog)))
	gtk_widget_hide(sx->dialog);

      if ((sx->snd_widgets) && (sx->snd_widgets[W_pane])) gtk_widget_hide(sx->snd_widgets[W_pane]);
      sp->channel_style = CHANNELS_SEPARATE; 
    }
}



/* ---------------- normalize sounds ---------------- */

void sound_show_ctrls(snd_info *sp)
{
  gtk_widget_show_all(CONTROL_PANEL(sp));
}

void sound_hide_ctrls(snd_info *sp)
{
  gtk_widget_hide_all(CONTROL_PANEL(sp));
}

bool control_panel_open(snd_info *sp)
{
  return((GTK_WIDGET_MAPPED(CONTROL_PANEL(sp))) && (GTK_WIDGET_VISIBLE(CONTROL_PANEL(sp))));
}


void show_controls(void)
{
  int i;
  showing_controls = true;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	sound_show_ctrls(sp);
    }
}

void hide_controls(void)
{
  int i;
  showing_controls = false;
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	sound_hide_ctrls(sp);
    }
}

int control_panel_height(snd_info *sp)
{
  return(widget_height(CONTROL_PANEL(sp)));
}



/* -------- PROGRESS REPORT -------- */

void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, enved_progress_t from_enved)
{
  int which;
  if ((!sp) || (sp->inuse != SOUND_NORMAL)) return;
  which = (int)(pct * NUM_GLASSES);
  if (which >= NUM_GLASSES) which = NUM_GLASSES - 1;
  if (which < 0) which = 0;
  if (from_enved == FROM_ENVED)
    display_enved_progress(NULL, mini_glasses[which]);
  else snd_file_glasses_icon(sp, true, which);
  check_for_event();
}

void finish_progress_report(snd_info *sp, enved_progress_t from_enved)
{
  if (sp->inuse != SOUND_NORMAL) return;
  if (from_enved == FROM_ENVED)
    display_enved_progress(NULL, blank);
  else snd_file_glasses_icon(sp, false, 0);
  if (!(ss->stopped_explicitly)) clear_minibuffer(sp);
}

void start_progress_report(snd_info *sp, enved_progress_t from_enved)
{
  if (sp->inuse != SOUND_NORMAL) return;
  if (from_enved == NOT_FROM_ENVED) 
    snd_file_glasses_icon(sp, true, 0);
}

void reflect_sound_selection(snd_info *sp)
{
  snd_info *osp = NULL;
  if (ss->selected_sound != NO_SELECTION) osp = ss->sounds[ss->selected_sound];
  if ((osp) && (sp != osp) && (osp->inuse == SOUND_NORMAL)) 
    gtk_widget_modify_fg(w_snd_name(osp), GTK_STATE_NORMAL, ss->sgx->black);
  if ((w_snd_name(sp)) && (sp->selected_channel != NO_SELECTION))
    gtk_widget_modify_fg(w_snd_name(sp), GTK_STATE_NORMAL, ss->sgx->red);
  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
    {
      int page, current_page;
      current_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)));
      page = sp->sgx->page;
      if ((page != current_page) && (current_page >= 0))
	{
	  ss->selected_sound = sp->index; /* break infinite recursion here */
	  gtk_notebook_set_current_page(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), page);
	}
    }
}


static XEN g_sound_widgets(XEN snd)
{
  #define H_sound_widgets "(" S_sound_widgets " (snd #f)): a list of \
widgets: ((0)pane (1)name (2)control-panel (3)minibuffer (4)play-button (5)filter-env (6)unite-button (7)name-label (8)name-icon) (9)\
pane-box (10)name-form"
  snd_info *sp;
  ASSERT_SOUND(S_sound_widgets, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_sound_widgets, snd));
  if (sp->sgx == NULL)
    return(XEN_EMPTY_LIST);
  return(XEN_CONS(XEN_WRAP_WIDGET(w_snd_pane(sp)),
	  XEN_CONS(XEN_WRAP_WIDGET(w_snd_name(sp)),
           XEN_CONS(XEN_WRAP_WIDGET(CONTROL_PANEL(sp)),
	    XEN_CONS(XEN_WRAP_WIDGET(MINIBUFFER_TEXT(sp)),
	     XEN_CONS(XEN_WRAP_WIDGET(PLAY_BUTTON(sp)),
	      XEN_CONS(XEN_WRAP_WIDGET(filter_graph(sp)), /* this is the (filter) drawingarea widget */
	       XEN_CONS(XEN_WRAP_WIDGET(unite_button(sp)),
	        XEN_CONS(XEN_WRAP_WIDGET(MINIBUFFER_LABEL(sp)),
	         XEN_CONS(XEN_WRAP_WIDGET(NAME_PIX(sp)),
		  XEN_CONS(XEN_WRAP_WIDGET(w_snd_pane_box(sp)),
		   XEN_CONS(XEN_WRAP_WIDGET(sp->sgx->snd_widgets[W_name_form]),
	            XEN_EMPTY_LIST))))))))))));
}

#ifdef XEN_ARGIFY_1
  XEN_ARGIFY_1(g_sound_widgets_w, g_sound_widgets)
#else
  #define g_sound_widgets_w g_sound_widgets
#endif

void g_init_gxsnd(void) 
{
  XEN_DEFINE_PROCEDURE(S_sound_widgets, g_sound_widgets_w, 0, 1, 0, H_sound_widgets);
}
