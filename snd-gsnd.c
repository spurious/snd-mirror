#include "snd.h"

enum {W_pane, W_pane_box, W_control_panel,
      W_name_form, W_name, W_name_event, W_name_pix, W_info_label, W_info, W_info_sep,
      W_play, W_sync, W_unite,
      W_amp_form, W_amp_event, W_amp, W_amp_label, W_amp_number, W_amp_sep,
      W_srate_form, W_srate, W_srate_event, W_srate_label, W_srate_number, W_srate_pix,
      W_expand_form, W_expand, W_expand_event, W_expand_label, W_expand_number, W_expand_button,
      W_contrast_form, W_contrast, W_contrast_event, W_contrast_label, W_contrast_number, W_contrast_button,
      W_reverb_form, W_revscl, W_revscl_event, W_revscl_label, W_revscl_number,
      W_revlen, W_revlen_event, W_revlen_label, W_revlen_number, W_reverb_button,
      W_filter_form, W_filter_label, W_filter_order, W_filter_env, W_filter, W_filter_button, W_filter_dB, W_filter_frame,
      W_apply_form, W_remember, W_restore, W_apply, W_reset
};

enum {W_amp_adj, W_srate_adj, W_contrast_adj, W_expand_adj, W_revscl_adj, W_revlen_adj, W_filter_adj};

#define NUM_SND_WIDGETS 60
#define NUM_SND_ADJS 7

GtkWidget *unite_button(snd_info *sp)   {return((sp->sgx)->snd_widgets[W_unite]);}
GtkWidget *filter_graph(snd_info *sp)   {return((sp->sgx)->snd_widgets[W_filter_env]);}

GtkWidget *w_snd_pane(snd_info *sp)     {return((sp->sgx)->snd_widgets[W_pane]);}
GtkWidget *w_snd_pane_box(snd_info *sp) {return((sp->sgx)->snd_widgets[W_pane_box]);}
GtkWidget *w_snd_name(snd_info *sp)     {return((sp->sgx)->snd_widgets[W_name]);}

#define CONTROL_PANEL(Sp)        (Sp->sgx)->snd_widgets[W_control_panel]
#define PLAY_BUTTON(Sp)          (Sp->sgx)->snd_widgets[W_play]
#define NAME_PIX(Sp)             (Sp->sgx)->snd_widgets[W_name_pix]
#define AMP_SCROLLBAR(Sp)        (Sp->sgx)->snd_widgets[W_amp]
#define SRATE_SCROLLBAR(Sp)      (Sp->sgx)->snd_widgets[W_srate]
#define SRATE_ARROW(Sp)          (Sp->sgx)->snd_widgets[W_srate_pix]
#define EXPAND_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_expand]
#define CONTRAST_SCROLLBAR(Sp)   (Sp->sgx)->snd_widgets[W_contrast]
#define REVSCL_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_revscl]
#define REVLEN_SCROLLBAR(Sp)     (Sp->sgx)->snd_widgets[W_revlen]
#define AMP_LABEL(Sp)            (Sp->sgx)->snd_widgets[W_amp_number]
#define SRATE_LABEL(Sp)          (Sp->sgx)->snd_widgets[W_srate_number]
#define EXPAND_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_expand_number]
#define EXPAND_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_expand_button]
#define CONTRAST_LABEL(Sp)       (Sp->sgx)->snd_widgets[W_contrast_number]
#define CONTRAST_BUTTON(Sp)      (Sp->sgx)->snd_widgets[W_contrast_button]
#define REVSCL_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_revscl_number]
#define REVLEN_LABEL(Sp)         (Sp->sgx)->snd_widgets[W_revlen_number]
#define REVERB_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_reverb_button]
#define APPLY_BUTTON(Sp)         (Sp->sgx)->snd_widgets[W_apply]
#define FILTER_ORDER_TEXT(Sp)    (Sp->sgx)->snd_widgets[W_filter_order]
#define FILTER_COEFFS_TEXT(Sp)   (Sp->sgx)->snd_widgets[W_filter]
#define FILTER_BUTTON(Sp)        (Sp->sgx)->snd_widgets[W_filter_button]
#define FILTER_DB_BUTTON(Sp)     (Sp->sgx)->snd_widgets[W_filter_dB]
#define MINIBUFFER_SEPARATOR(Sp) (Sp->sgx)->snd_widgets[W_info_sep]
#define MINIBUFFER_LABEL(Sp)     (Sp->sgx)->snd_widgets[W_info_label]
#define MINIBUFFER_TEXT(Sp)      (Sp->sgx)->snd_widgets[W_info]
#define SYNC_BUTTON(Sp)          (Sp->sgx)->snd_widgets[W_sync]

#define AMP_ADJUSTMENT(Sp)       (Sp->sgx)->snd_adjs[W_amp_adj]
#define SRATE_ADJUSTMENT(Sp)     (Sp->sgx)->snd_adjs[W_srate_adj]
#define EXPAND_ADJUSTMENT(Sp)    (Sp->sgx)->snd_adjs[W_expand_adj]
#define CONTRAST_ADJUSTMENT(Sp)  (Sp->sgx)->snd_adjs[W_contrast_adj]
#define REVSCL_ADJUSTMENT(Sp)    (Sp->sgx)->snd_adjs[W_revscl_adj]
#define REVLEN_ADJUSTMENT(Sp)    (Sp->sgx)->snd_adjs[W_revlen_adj]


/* -------- PIXMAPS -------- */

static GdkPixmap *mini_lock, *speed_r, *speed_l, *blank;
static bool mini_lock_allocated = false;
static GdkPixmap *mini_bombs[NUM_BOMBS];
static GdkPixmap *mini_glasses[NUM_GLASSES];

void snd_file_lock_icon(snd_info *sp, bool on)
{
  snd_context *sx;
  if (mini_lock) 
    {
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
  snd_context *sx;
  if (sp->bomb_ctr >= NUM_BOMBS) sp->bomb_ctr = 0;
  if (mini_bombs[sp->bomb_ctr]) 
    {
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
  int *buf;
  if (sp->need_update)
    {
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
    gtk_timeout_add((guint32)BOMB_TIME, bomb_check, data);
  else bomb_in_progress = false;
  return(0);
}

void snd_file_bomb_icon(snd_info *sp, bool on)
{
  if ((on) && (!bomb_in_progress))
    {
      bomb_in_progress = true;
      gtk_timeout_add((guint32)BOMB_TIME, bomb_check, (gpointer)sp);
    }
}

static void snd_file_glasses_icon(snd_info *sp, bool on, int glass)
{
  if (on)
    {
      if (mini_glasses[glass]) 
	gdk_draw_drawable(GDK_DRAWABLE(NAME_PIX(sp)->window), ss->sgx->basic_gc, mini_glasses[glass], 0, 0, 0, 4, 18, 16);
    }
  else gdk_draw_drawable(GDK_DRAWABLE(NAME_PIX(sp)->window), ss->sgx->basic_gc, sp->sgx->file_pix, 0, 0, 0, 4, 18, 16);
}

static void make_pixmaps(void)
{
  GdkWindow *wn;
  int k;
  if (!mini_lock_allocated)
    { 
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
  snd_info *sp = (snd_info *)data;
  gdk_draw_drawable(GDK_DRAWABLE(NAME_PIX(sp)->window), ss->sgx->basic_gc, sp->sgx->file_pix, 0, 0, 0, 4, 16, 16);
  return(false);
}


/* -------- MINIBUFFER CALLBACKS -------- */

void goto_minibuffer(snd_info *sp)
{
  if (sp) 
    {
      (sp->sgx)->mini_active = true;
      set_text_background(MINIBUFFER_TEXT(sp), (ss->sgx)->white);
      goto_window(MINIBUFFER_TEXT(sp));
    }
}

void set_minibuffer_cursor_position(snd_info *sp, int pos)
{
  gtk_editable_set_position(GTK_EDITABLE(MINIBUFFER_TEXT(sp)), pos);
}

char *get_minibuffer_string(snd_info *sp) 
{
  return(copy_string((char *)gtk_entry_get_text(GTK_ENTRY(MINIBUFFER_TEXT(sp)))));
} 

static char stupid[1] = {'\0'};
void set_minibuffer_string(snd_info *sp, char *str) 
{
  if (sp->inuse != SOUND_NORMAL) return;
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
  ss->mx_sp = sp; 
  snd_minibuffer_activate(sp, 0, false);
  (sp->sgx)->mini_active = true;
}

static gboolean minibuffer_key_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  /* can't use M-p in gtk version because it's trapped by a menu accelerator (File:Print) -- M-n is File:New */
  snd_info *sp = (snd_info *)data;
  chan_info *cp;
  if (((!(sp->sgx)->mini_active)) || 
      (((event->keyval == snd_K_s) || 
	(event->keyval == snd_K_r)) && 
       (event->state & snd_ControlMask)))
    {
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
    {
      set_text_background(w, (ss->sgx)->white);
      (sp->sgx)->mini_active = true;
    }
  return(false);
}

static gboolean minibuffer_mouse_leave(GtkWidget *w, GdkEventCrossing *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if ((sp) && (sp->inuse == SOUND_NORMAL))
    {
      /* weird -- I'm getting this event sent to an inactive sound?? */
      set_text_background(w, (ss->sgx)->basic_color);
      (sp->sgx)->mini_active = false;
    }
  return(false);
}


/* -------- PLAY BUTTON -------- */

void set_play_button(snd_info *sp, bool val)
{
  if ((sp->sgx) && (!(IS_PLAYER(sp))))
    {
      set_toggle_button(PLAY_BUTTON(sp), val, false, (void *)sp);
      set_file_browser_play_button(sp->short_filename, val);
    }
}

void set_control_panel_play_button(snd_info *sp, bool val)
{
  if ((sp) && (sp->sgx) && (PLAY_BUTTON(sp)))
    set_toggle_button(PLAY_BUTTON(sp), false, false, sp);
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
    stop_playing_sound_no_toggle(sp);
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
	set_active_color(w, (ss->sgx)->green); 
      else set_active_color(w, (ss->sgx)->pushed_button_color);
      play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, 
		 C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION),
		 "play_button", 0);                        /* should this follow the sync button? */
    }
}

typedef struct {bool pausing; } pause_data;

static void set_play_button_pause(snd_info *sp, void *ptr)
{
  pause_data *pd = (pause_data *)ptr;
  GtkWidget *w;
  if (sp->playing)
    {
      w = PLAY_BUTTON(sp);
      if (pd->pausing)
	set_active_color(w, (ss->sgx)->red);
      else 
	if (sp->cursor_follows_play != DONT_FOLLOW) 
	  set_active_color(w, (ss->sgx)->green); 
	else set_active_color(w, (ss->sgx)->pushed_button_color);
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
    case 1: case 0: set_active_color(syb, (ss->sgx)->pushed_button_color); break;
    case 2: set_active_color(syb, (ss->sgx)->green); break;
    case 3: set_active_color(syb, (ss->sgx)->yellow); break;
    case 4: set_active_color(syb, (ss->sgx)->red); break;
    default: set_active_color(syb, (ss->sgx)->black); break;
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
  chan_info *cp;
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
  if (sp->sync != 0) 
    {
      set_sync_color(sp);
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



/* -------- AMP CALLBACKS -------- */

static gboolean name_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  sp_name_click((snd_info *)data);
  return(false);
}

static char semitone_one[5] = {' ', ' ', ' ', '0', '\0'};
static char ratio_one[5] = {' ', '1', '/', '1', '\0'};

static char amp_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

static void set_snd_amp_1(snd_info *sp, Float amp, bool setadj)
{
  /* amp is exponential between .17 and around 7 or 8 with 1.0 at scroll midpoint */
  /*     is linear below .17, scroll-relative .15 */
  /* here we get the user-view and fixup the scrollbar */
  Float scrollval;
  char *sfs;
  GtkObject *adj;
  sp->amp_control = amp;
  if (amp <= 0.0)
    scrollval = 0.0;
  else
    {
      if (amp < .173)
	scrollval = amp * .867;
      else 
	{
	  scrollval = (log(amp) * 0.2 + 0.5);
	  if (scrollval > .9) scrollval = .9;
	}
    }
  sfs = prettyf(sp->amp_control, 2);
  fill_number(sfs, amp_number_buffer);
  gtk_label_set_text(GTK_LABEL(AMP_LABEL(sp)), amp_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = AMP_ADJUSTMENT(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_amp(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->amp_control = amp;
  else set_snd_amp_1(sp, amp, true);
}

static Float get_snd_amp(Float scrollval)
{
  if (scrollval < .15)
    return(scrollval * 1.13);
  else return(exp((scrollval - 0.5) * 5.0));
}

static gboolean amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_amp(sp, sp->last_amp_control);
  else set_snd_amp(sp, 1.0);
  return(false);
}

static void amp_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_amp_1((snd_info *)data, get_snd_amp((Float)(adj->value)), false);
}

static gboolean amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_amp_control = sp->saved_amp_control;
  sp->saved_amp_control = sp->amp_control;
  return(false);
}


/* -------- SRATE CALLBACKS -------- */

static char srate_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

void set_snd_srate(snd_info *sp, Float amp)
{
  Float scrollval;
  GtkObject *adj;
  sp->speed_control = amp;
  if (!(IS_PLAYER(sp)))
    {
      if (amp > 0.0)
	scrollval = .45 + .15 * log(amp);
      else scrollval = 0.0;
      sprintf(srate_number_buffer, "%.3f", amp);
      gtk_label_set_text(GTK_LABEL(SRATE_LABEL(sp)), srate_number_buffer);
      adj = SRATE_ADJUSTMENT(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

static gboolean srate_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_srate(sp, sp->last_speed_control);
  else set_snd_srate(sp, 1.0);
  return(false);
}

static void srate_changed_callback(GtkAdjustment *adj, gpointer data)
{
  Float scrollval, val;
  snd_info *sp = (snd_info *)data;
  val = srate_changed(exp((GTK_ADJUSTMENT(adj)->value - .45) / .15), srate_number_buffer, sp->speed_control_style, sp->speed_control_tones);
  sp->speed_control = val;
  if (val > 0.0)
    scrollval = .45 + .15 * log(val);
  else scrollval = 0.0;
  gtk_label_set_text(GTK_LABEL(SRATE_LABEL(sp)), srate_number_buffer);
}

static gboolean srate_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_speed_control = sp->saved_speed_control;
  sp->saved_speed_control = sp->speed_control;
  return(false);
}

static void draw_srate_arrow(snd_info *sp)
{
  if (sp->speed_control_direction == 1)
    gdk_draw_drawable(GDK_DRAWABLE(SRATE_ARROW(sp)->window), ss->sgx->basic_gc, speed_r, 0, 0, 0, 4, 18, 16);
  else gdk_draw_drawable(GDK_DRAWABLE(SRATE_ARROW(sp)->window), ss->sgx->basic_gc, speed_l, 0, 0, 0, 4, 18, 16);
}

static gboolean srate_arrow_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (sp->speed_control_direction == 1)
    sp->speed_control_direction = -1;
  else sp->speed_control_direction = 1;
  draw_srate_arrow(sp);
  return(false);
}
static gboolean srate_arrow_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  draw_srate_arrow((snd_info *)data);
  return(false);
}

void toggle_direction_arrow(snd_info *sp, bool state)
{
  int dir = 1;
  if (state) dir = -1;
  sp->speed_control_direction = dir;
  if (!(IS_PLAYER(sp))) draw_srate_arrow(sp);
}


/* -------- EXPAND CALLBACKS -------- */

static char expand_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

static void set_snd_expand_1(snd_info *sp, Float expand, bool setadj)
{
  Float scrollval;
  char *sfs;
  GtkObject *adj;
  sp->expand_control = expand;
  if (sp->playing) dac_set_expand(sp, sp->expand_control);
  if (expand < .1)
    scrollval = expand * 1.03;
  else scrollval = .45 + .15 * log(expand);
  sfs = prettyf(sp->expand_control, 2);
  fill_number(sfs, expand_number_buffer);
  gtk_label_set_text(GTK_LABEL(EXPAND_LABEL(sp)), expand_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = EXPAND_ADJUSTMENT(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_expand(snd_info *sp, Float val) 
{
  if (IS_PLAYER(sp))
    sp->expand_control = val;
  else set_snd_expand_1(sp, val, true);
}

static Float get_snd_expand(Float scrollval)
{
  if (scrollval < .1)
    return(scrollval * .9697);
  else return(exp((scrollval - 0.45) / .15));
}

static void expand_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_expand_1((snd_info *)data, get_snd_expand((Float)(adj->value)), false);
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
    set_snd_expand(sp, sp->last_expand_control);
  else set_snd_expand(sp, 1.0);
  return(false);
}

static void expand_button_callback(GtkWidget *w, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  sp->expand_control_p = GTK_TOGGLE_BUTTON(w)->active;
}

void toggle_expand_button(snd_info *sp, bool state)
{
  if (IS_PLAYER(sp))
    sp->expand_control_p = state;
  else set_toggle_button(EXPAND_BUTTON(sp), state, true, (void *)sp);
}


/* -------- CONTRAST CALLBACKS -------- */

static char contrast_number_buffer[5] = {'0', STR_decimal, '0', '0', '\0'};

static void set_snd_contrast_1(snd_info *sp, Float val, bool setadj)
{
  Float scrollval;
  char *sfs;
  GtkObject *adj;
  sp->contrast_control = val;
  scrollval = val / 10.0;
  sfs = prettyf(sp->contrast_control, 2);
  fill_number(sfs, contrast_number_buffer);
  gtk_label_set_text(GTK_LABEL(CONTRAST_LABEL(sp)), contrast_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = CONTRAST_ADJUSTMENT(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_contrast(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->contrast_control = amp;
  else set_snd_contrast_1(sp, amp, true);
}

static Float get_snd_contrast(Float scrollval)
{
  return(scrollval * 10.0);
}

static gboolean contrast_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_contrast(sp, sp->last_contrast_control);
  else set_snd_contrast(sp, 0.0);
  return(false);
}

static void contrast_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_contrast_1((snd_info *)data, get_snd_contrast((Float)(adj->value)), false);
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

static char revscl_number_buffer[7] = {'0', STR_decimal, '0', '0', '0', '0', '\0'};

static void set_snd_revscl_1(snd_info *sp, Float val, bool setadj)
{
  Float scrollval;
  char *fs, *ps, *sfs;
  int i, j;
  GtkObject *adj;
  sp->reverb_control_scale = val;
  scrollval = pow(val, .333) * .6;
  sfs = prettyf(sp->reverb_control_scale, 3);
  fs = sfs;
  ps = (char *)(revscl_number_buffer);
  j = strlen(fs);
  if (j > 6) j = 6;
  if (j < 6) 
    {
      revscl_number_buffer[5] = '0';
      revscl_number_buffer[4] = '0'; 
      revscl_number_buffer[3] = '0';
      revscl_number_buffer[2] = '0'; 
      revscl_number_buffer[1] = STR_decimal;
    }
  for (i = 0; i < j; i++) (*ps++) = (*fs++);
  gtk_label_set_text(GTK_LABEL(REVSCL_LABEL(sp)), revscl_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = REVSCL_ADJUSTMENT(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_revscl(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->reverb_control_scale = amp;
  else set_snd_revscl_1(sp, amp, true);
}

static Float get_snd_revscl(Float scrollval)
{
  return(pow(scrollval * 1.666, 3.0));
}

static gboolean revscl_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_revscl(sp, sp->last_reverb_control_scale);
  else set_snd_revscl(sp, 0.0);
  return(false);
}

static void revscl_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_revscl_1((snd_info *)data, get_snd_revscl((Float)(adj->value)), false);
}

static gboolean revscl_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  sp->last_reverb_control_scale = sp->saved_reverb_control_scale;
  sp->saved_reverb_control_scale = sp->reverb_control_scale;
  return(false);
}


static char revlen_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

static void set_snd_revlen_1(snd_info *sp, Float val, bool setadj)
{
  Float scrollval;
  char *sfs;
  GtkObject *adj;
  sp->reverb_control_length = val;
  scrollval = val / 5.0;
  sfs = prettyf(sp->reverb_control_length, 2);
  fill_number(sfs, revlen_number_buffer);
  gtk_label_set_text(GTK_LABEL(REVLEN_LABEL(sp)), revlen_number_buffer);
  FREE(sfs);
  if (setadj)
    {
      adj = REVLEN_ADJUSTMENT(sp);
      GTK_ADJUSTMENT(adj)->value = scrollval;
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(adj));
    }
}

void set_snd_revlen(snd_info *sp, Float amp) 
{
  if (IS_PLAYER(sp))
    sp->reverb_control_length = amp;
  else set_snd_revlen_1(sp, amp, true);
}

static Float get_snd_revlen(Float scrollval)
{
  return(scrollval * 5.0);
}

static gboolean revlen_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  snd_context *sx;
  sx = sp->sgx;
  if (ev->state & (snd_ControlMask | snd_MetaMask)) 
    set_snd_revlen(sp, sp->last_reverb_control_length);
  else set_snd_revlen(sp, 1.0);
  return(false);
}

static void revlen_changed_callback(GtkAdjustment *adj, gpointer data)
{
  set_snd_revlen_1((snd_info *)data, get_snd_revlen((Float)(adj->value)), false);
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

static void display_filter_env(snd_info *sp)
{
  axis_context *ax;
  int height, width;
  GtkWidget *drawer;
  if (IS_PLAYER(sp)) return;
  if (ss == NULL) return;
  drawer = filter_graph(sp);
  height = widget_height(drawer);
  if (height < MIN_FILTER_GRAPH_HEIGHT) return;
  width = widget_width(drawer);
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ax->gc = (ss->sgx)->fltenv_basic_gc;
  ax->wn = drawer->window;
  ax->w = drawer;
  gdk_window_clear(ax->wn);
  if (edp_display_graph(sp->sgx->flt,
			_("frequency response"),
			ax, 0, 0, width, height, 
			sp->filter_control_env, 
			sp->filter_control_in_dB, 
			true))
    {
      ax->gc = (ss->sgx)->fltenv_data_gc;
      display_frequency_response(sp->filter_control_env, 
				 edp_ap(sp->sgx->flt), ax, 
				 sp->filter_control_order, 
				 sp->filter_control_in_dB);
    }
  ax = free_axis_context(ax);
}

static gboolean filter_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  int evx, evy;
  GdkModifierType state;
  if (ev->state & GDK_BUTTON1_MASK)
    {
      if (ev->is_hint)
	gdk_window_get_pointer(ev->window, &evx, &evy, &state);
      else
	{
	  evx = (int)(ev->x);
	  evy = (int)(ev->y);
	}
      edp_handle_point(sp->sgx->flt,
		       evx, evy, ev->time, 
		       sp->filter_control_env, 
		       sp->filter_control_in_dB,
		       sp->filter_control_env_xmax);
      display_filter_env(sp);
      sp->filter_control_changed = true;
    }
  return(false);
}

static gboolean filter_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (edp_handle_press(sp->sgx->flt,
		       (int)(ev->x), (int)(ev->y), ev->time, 
		       sp->filter_control_env, 
		       sp->filter_control_in_dB,
		       sp->filter_control_env_xmax))
    display_filter_env(sp);
  return(false);
}

static gboolean filter_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  char *tmpstr = NULL;
  snd_info *sp = (snd_info *)data;
  edp_handle_release(sp->sgx->flt, sp->filter_control_env);
  display_filter_env(sp);
  set_filter_text(sp, tmpstr = env_to_string(sp->filter_control_env));
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

static void set_snd_filter_order_1(snd_info *sp, int order, bool setadj)
{
  sp->filter_control_order = order;
  if (setadj)
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(FILTER_ORDER_TEXT(sp)), (gfloat)order);
  display_filter_env(sp);
  sp->filter_control_changed = true;
}  

void set_snd_filter_order(snd_info *sp, int order) 
{
  if (IS_PLAYER(sp))
    sp->filter_control_order = order;
  else set_snd_filter_order_1(sp, order, true);
}

static void filter_order_callback(GtkWidget *w, gpointer data)
{
  int order;
  snd_info *sp = (snd_info *)data;
  order = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(FILTER_ORDER_TEXT(sp)));
  if (order & 1) order++;
  if (order <= 0) order = 2;
  sp->filter_control_order = order;
  set_snd_filter_order_1(sp, sp->filter_control_order, false);
}

static void filter_activate_callback(GtkWidget *w, gpointer context)
{
  /* make an envelope out of the data */
  snd_info *sp = (snd_info *)context;
  char *str = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  if (sp->filter_control_env) sp->filter_control_env = free_env(sp->filter_control_env);
  sp->filter_control_env = string2env(str);
  if (!(sp->filter_control_env)) /* maybe user cleared text field? */
    sp->filter_control_env = default_env(sp->filter_control_env_xmax, 1.0);
  edp_edited(sp->sgx->flt);
  display_filter_env(sp);
  sp->filter_control_changed = true;
}

void filter_env_changed(snd_info *sp, env *e)
{
  /* turn e back into a string for textfield widget */
  char *tmpstr;
  if (!(IS_PLAYER(sp)))
    {
      tmpstr = env_to_string(e);
      if (tmpstr)
	{
	  gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), tmpstr);
	  FREE(tmpstr);
	}
      else gtk_entry_set_text(GTK_ENTRY(FILTER_COEFFS_TEXT(sp)), stupid);
      edp_edited(sp->sgx->flt);
      display_filter_env(sp);
      /* this is called also from snd-scm.c */
    }
  sp->filter_control_changed = true;
}

void color_filter_waveform(GdkColor *color)
{
  int i;
  snd_info *sp;
  gdk_gc_set_foreground((ss->sgx)->fltenv_data_gc, color);
  (ss->sgx)->filter_waveform_color = color;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	display_filter_env(sp);
    }
}


/* -------- XEN_APPLY CALLBACKS -------- */

static int last_apply_state = 0;

static gboolean apply_button_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  last_apply_state = ev->state;
  return(false);
}

static void apply_button_callback(GtkWidget *w, gpointer context)
{
  /* create temp file of run over current file using the current (saved) ctrls state */
  snd_info *sp = (snd_info *)context;
  snd_context *sgx;
  sgx = sp->sgx;
  if (sp->applying) 
    {
      stop_applying(sp);
      set_background(APPLY_BUTTON(sp), (ss->sgx)->basic_color);
      sp->applying = false;
    }
  else
    {
      ss->apply_choice = APPLY_TO_SOUND;
      if (last_apply_state & snd_ControlMask) 
	{
	  if (selection_is_active())
	    ss->apply_choice = APPLY_TO_SELECTION;
	  else ss->apply_choice = APPLY_TO_CHANNEL;
	}
      sp->applying = true;
      set_background(APPLY_BUTTON(sp), (ss->sgx)->pushed_button_color);
      sgx->apply_in_progress = BACKGROUND_ADD(apply_controls, (Indicium)(make_apply_state_with_implied_beg_and_dur(sp)));
    }
}

/* apply is only safe if the DAC is currently inactive and remains safe only
 * if all other apply buttons are locked out (and play).
 */

static void lockapply(snd_info *sp, void *up) 
{
  if (sp != up) 
    set_sensitive(APPLY_BUTTON(sp), false);
}

void lock_apply(snd_info *sp)
{
  /* if playing or applying, set other applys to insensitive */
  for_each_sound(lockapply, (void *)sp);
}

static void unlockapply(snd_info *sp, void *up) 
{
  set_sensitive(APPLY_BUTTON(sp), true);
}

void unlock_apply(snd_info *sp)
{
  for_each_sound(unlockapply, (void *)sp);
  if (sp) 
    set_background(APPLY_BUTTON(sp), (ss->sgx)->basic_color);
}

static void remember_button_callback(GtkWidget *w, gpointer context) {save_controls((snd_info *)context);}
static void restore_button_callback(GtkWidget *w, gpointer context) {restore_controls((snd_info *)context);}
static void reset_button_callback(GtkWidget *w, gpointer context) {reset_controls((snd_info *)context);}


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
  chan_info *cp;
  env_info *ep;
  int i;
  GtkWidget *info_sep;
  /* a channel completed an amp env, check to see if all are complete */
  for (i = 0; i < sp->nchans; i++)
    {
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
  GtkWidget *info_sep;
  if ((sp) && (sp->sgx))
    {
      info_sep = MINIBUFFER_SEPARATOR(sp);
      if (info_sep) gtk_widget_hide(info_sep);
    }
}

static gboolean close_sound_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  if (sp) snd_close_file(sp);
  gtk_widget_hide(sp->sgx->dialog); 
  return(true);
} 


/* -------- SOUND PANE -------- */

snd_info *add_sound_window(char *filename, bool read_only)
{
  snd_info *sp, *osp;
  file_info *hdr;
  GtkWidget **sw;
  GtkObject **adjs;
  GtkWidget *tablab;
  int snd_slot, nchans, i, k, old_chans;
  bool free_filename = false, make_widgets;
  char *old_name = NULL, *title;
  int app_y, app_dy, screen_y, chan_min_y;
  /* these dimensions are used to try to get a reasonable channel graph size without falling off the screen bottom */
  off_t samples_per_channel;
  snd_context *sx;
  errno = 0;
  hdr = make_file_info(filename);
  if (!hdr) return(NULL);
  if (ss->pending_change) 
    {
      old_name = filename;
      filename = ss->pending_change;
      ss->pending_change = NULL;
      free_filename = true;
    }
  nchans = hdr->chans;
  if (nchans <= 0) nchans = 1;
  samples_per_channel = hdr->samples / nchans;
  app_y = widget_y(MAIN_SHELL(ss));
  app_dy = widget_height(MAIN_SHELL(ss));
  screen_y = gdk_screen_height();
  app_dy = (screen_y - app_y - app_dy - 20 * nchans);
  chan_min_y = app_dy / nchans;
  if (chan_min_y > (ss->channel_min_height)) 
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
  sx->controls_fixed = false;
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
  if ((!make_widgets) && (old_chans < nchans))
    {
      for (i = old_chans; i < nchans; i++) 
	add_channel_window(sp, i, chan_min_y, 1, NULL, WITH_FW_BUTTONS, true);
    }
  if (make_widgets)
    {
      sw[W_pane] = gtk_vpaned_new();
      set_backgrounds(sw[W_pane], (ss->sgx)->sash_color);
      gtk_container_set_border_width(GTK_CONTAINER(sw[W_pane]), 0);
      if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
	{
	  sx->dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	  set_background(sx->dialog, (ss->sgx)->basic_color);
	  sg_make_resizable(sx->dialog);
	  gtk_container_add(GTK_CONTAINER(sx->dialog), sw[W_pane]);
	  gtk_widget_show(sx->dialog);
	  g_signal_connect_closure_by_id(GTK_OBJECT(sx->dialog),
					 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(sx->dialog))),
					 0,
					 g_cclosure_new(GTK_SIGNAL_FUNC(close_sound_dialog), (gpointer)sp, 0),
					 0);
	}
      else
	{
	  if (sound_style(ss) == SOUNDS_IN_NOTEBOOK)
	    {
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

      sw[W_name_form] = gtk_hbox_new(false, 0);
      gtk_box_pack_end(GTK_BOX(sw[W_pane_box]), sw[W_name_form], false, false, 0);
      
      for (i = 0; i < nchans; i++) 
	add_channel_window(sp, i, chan_min_y, 0, NULL, WITH_FW_BUTTONS, true);

      /* controls etc */

      sw[W_control_panel] = gtk_vbox_new(false, 0);
      gtk_paned_add2(GTK_PANED(sw[W_pane]), sw[W_control_panel]);
  

      /* -------- NAME FIELDS -------- */

      sw[W_name_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]), sw[W_name_event], false, false, 5);
      gtk_widget_show(sw[W_name_event]);
      set_background(sw[W_name_event], (ss->sgx)->highlight_color);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_name_event]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_name_event]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(name_click_callback), (gpointer)sp, 0),
				     0);
      
      sw[W_name] = gtk_label_new(shortname_indexed(sp));
      gtk_container_add(GTK_CONTAINER(sw[W_name_event]), sw[W_name]);
      gtk_widget_show(sw[W_name]);
      
      sw[W_name_pix] = gtk_drawing_area_new();
      gtk_widget_set_events(sw[W_name_pix], GDK_EXPOSURE_MASK);
      set_background(sw[W_name_pix], (ss->sgx)->basic_color);
      gtk_widget_set_size_request(sw[W_name_pix], 16, 16);
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]), sw[W_name_pix], false, false, 0);
      gtk_widget_show(sw[W_name_pix]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_name_pix]),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_name_pix]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(name_pix_expose), (gpointer)sp, 0),
				     0);

      sw[W_info_sep] = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]), sw[W_info_sep], false, false, 4);
      gtk_widget_show(sw[W_info_sep]);
      
      sw[W_info_label] = gtk_label_new(NULL);
      gtk_box_pack_start(GTK_BOX(sw[W_name_form]), sw[W_info_label], false, false, 0);
      gtk_widget_show(sw[W_info_label]);
      
      sw[W_info] = snd_entry_new(sw[W_name_form], false);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_info]),
				     g_signal_lookup("key_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_info]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(minibuffer_key_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_info]),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(sw[W_info]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(minibuffer_activate_callback), (gpointer)sp, 0),
				     0);

      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_info]),
				     g_signal_lookup("enter_notify_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_info]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(minibuffer_mouse_enter), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_info]),
				     g_signal_lookup("leave_notify_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_info]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(minibuffer_mouse_leave), (gpointer)sp, 0),
				     0);

      set_background(sw[W_info], (ss->sgx)->basic_color);
      set_text_background(sw[W_info], (ss->sgx)->basic_color);
      
      /* now fill from other end */
      
      sw[W_play] = gtk_check_button_new_with_label(_("play"));
      gtk_box_pack_end(GTK_BOX(sw[W_name_form]), sw[W_play], false, false, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_play]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_play]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(play_button_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_play]),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(sw[W_play]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(play_button_click_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_play]);
      
      sw[W_sync] = gtk_check_button_new_with_label(_("sync"));
      gtk_box_pack_end(GTK_BOX(sw[W_name_form]), sw[W_sync], false, false, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_sync]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_sync]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(sync_button_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_sync]),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(sw[W_sync]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(sync_button_click), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_sync]);
      
      sw[W_unite] = gtk_check_button_new_with_label(_("unite"));
      gtk_box_pack_end(GTK_BOX(sw[W_name_form]), sw[W_unite], false, false, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_unite]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_unite]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(unite_button_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_unite]),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(sw[W_unite]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(unite_button_click), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_unite]);
      
      gtk_widget_show(sw[W_name_form]);

      /* if control-panel */
      
      /* -------- AMP FIELDS -------- */
      
      sw[W_amp_sep] = gtk_hseparator_new();
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_amp_sep], false, false, 4);
      gtk_widget_show(sw[W_amp_sep]);
      
      sw[W_amp_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_amp_form], false, false, 0);
      set_background(sw[W_amp_form], (ss->sgx)->basic_color);
      
      sw[W_amp_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]), sw[W_amp_event], false, false, 4);
      gtk_widget_show(sw[W_amp_event]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_amp_event]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_amp_event]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(amp_click_callback), (gpointer)sp, 0),
				     0);
      set_background(sw[W_amp_event], (ss->sgx)->basic_color);
      
      sw[W_amp_label] = gtk_label_new(_("amp:"));
      gtk_container_add(GTK_CONTAINER(sw[W_amp_event]), sw[W_amp_label]);
      gtk_widget_show(sw[W_amp_label]);
      
      sw[W_amp_number] = gtk_label_new(amp_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]), sw[W_amp_number], false, false, 0);
      gtk_widget_show(sw[W_amp_number]);
      
      adjs[W_amp_adj] = gtk_adjustment_new(0.5, 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_amp] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_amp_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_amp_form]), sw[W_amp], true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(adjs[W_amp_adj]),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(adjs[W_amp_adj]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(amp_changed_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_amp]),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_amp]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(amp_release_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_amp]);
      
      gtk_widget_show(sw[W_amp_form]);
      
      
      /* -------- SRATE FIELDS -------- */
      
      sw[W_srate_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_srate_form], false, false, 0);
      set_background(sw[W_srate_form], (ss->sgx)->basic_color);
      
      sw[W_srate_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_srate_form]), sw[W_srate_event], false, false, 4);
      gtk_widget_show(sw[W_srate_event]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_srate_event]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_srate_event]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(srate_click_callback), (gpointer)sp, 0),
				     0);
      set_background(sw[W_srate_event], (ss->sgx)->basic_color);
      
      sw[W_srate_label] = gtk_label_new(_("speed:"));
      gtk_container_add(GTK_CONTAINER(sw[W_srate_event]), sw[W_srate_label]);
      gtk_widget_show(sw[W_srate_label]);
      
      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO: sw[W_srate_number] = gtk_label_new(ratio_one); break;
	case SPEED_CONTROL_AS_SEMITONE: sw[W_srate_number] = gtk_label_new(semitone_one); break;
	default:  sw[W_srate_number] = gtk_label_new(srate_number_buffer); break;
	}
      gtk_box_pack_start(GTK_BOX(sw[W_srate_form]), sw[W_srate_number], false, false, 0);
      gtk_widget_show(sw[W_srate_number]);
      
      adjs[W_srate_adj] = gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_srate] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_srate_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_srate_form]), sw[W_srate], true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(adjs[W_srate_adj]),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(adjs[W_srate_adj]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(srate_changed_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_srate]),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_srate]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(srate_release_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_srate]);

      sw[W_srate_pix] = gtk_drawing_area_new();
      gtk_widget_set_events(sw[W_srate_pix], GDK_ALL_EVENTS_MASK);
      gtk_box_pack_start(GTK_BOX(sw[W_srate_form]), sw[W_srate_pix], false, false, 2);
      set_background(sw[W_srate_pix], (ss->sgx)->basic_color);
      gtk_widget_set_size_request(sw[W_srate_pix], 18, 16);
      gtk_widget_show(sw[W_srate_pix]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_srate_pix]),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_srate_pix]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(srate_arrow_expose), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_srate_pix]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_srate_pix]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(srate_arrow_press), (gpointer)sp, 0),
				     0);

      gtk_widget_show(sw[W_srate_form]);
      
      
      /* -------- EXPAND FIELDS -------- */
      
      sw[W_expand_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_expand_form], false, false, 0);
      set_background(sw[W_expand_form], (ss->sgx)->basic_color);
      
      sw[W_expand_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]), sw[W_expand_event], false, false, 4);
      gtk_widget_show(sw[W_expand_event]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_expand_event]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_expand_event]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(expand_click_callback), (gpointer)sp, 0),
				     0);
      set_background(sw[W_expand_event], (ss->sgx)->basic_color);
      
      sw[W_expand_label] = gtk_label_new(_("expand:"));
      gtk_container_add(GTK_CONTAINER(sw[W_expand_event]), sw[W_expand_label]);
      gtk_widget_show(sw[W_expand_label]);
      
      sw[W_expand_number] = gtk_label_new(expand_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]), sw[W_expand_number], false, false, 0);
      gtk_widget_show(sw[W_expand_number]);
      
      adjs[W_expand_adj] = gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_expand] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_expand_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]), sw[W_expand], true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(adjs[W_expand_adj]),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(adjs[W_expand_adj]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(expand_changed_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_expand]),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_expand]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(expand_release_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_expand]);
      
      sw[W_expand_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_expand_form]), sw[W_expand_button], false, false, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_expand_button]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_expand_button]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(expand_button_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_expand_button]);
      
      gtk_widget_show(sw[W_expand_form]);
      
      
      /* -------- CONTRAST FIELDS -------- */
      
      sw[W_contrast_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_contrast_form], false, false, 0);
      set_background(sw[W_contrast_form], (ss->sgx)->basic_color);
      
      sw[W_contrast_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]), sw[W_contrast_event], false, false, 4);
      gtk_widget_show(sw[W_contrast_event]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_contrast_event]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_contrast_event]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(contrast_click_callback), (gpointer)sp, 0),
				     0);
      set_background(sw[W_contrast_event], (ss->sgx)->basic_color);
      
      sw[W_contrast_label] = gtk_label_new(_("contrast:"));
      gtk_container_add(GTK_CONTAINER(sw[W_contrast_event]), sw[W_contrast_label]);
      gtk_widget_show(sw[W_contrast_label]);
      
      sw[W_contrast_number] = gtk_label_new(contrast_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]), sw[W_contrast_number], false, false, 0);
      gtk_widget_show(sw[W_contrast_number]);
      
      adjs[W_contrast_adj] = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_contrast] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_contrast_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]), sw[W_contrast], true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(adjs[W_contrast_adj]),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(adjs[W_contrast_adj]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(contrast_changed_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_contrast]),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_contrast]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(contrast_release_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_contrast]);
      
      sw[W_contrast_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_contrast_form]), sw[W_contrast_button], false, false, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_contrast_button]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_contrast_button]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(contrast_button_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_contrast_button]);
      
      gtk_widget_show(sw[W_contrast_form]);
      
      
      /* -------- REVERB FIELDS -------- */
      
      sw[W_reverb_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_reverb_form], false, false, 0);
      set_background(sw[W_reverb_form], (ss->sgx)->basic_color);
      
      sw[W_revscl_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revscl_event], false, false, 4);
      gtk_widget_show(sw[W_revscl_event]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_revscl_event]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_revscl_event]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(revscl_click_callback), (gpointer)sp, 0),
				     0);
      set_background(sw[W_revscl_event], (ss->sgx)->basic_color);
      
      sw[W_revscl_label] = gtk_label_new(_("reverb:"));
      gtk_container_add(GTK_CONTAINER(sw[W_revscl_event]), sw[W_revscl_label]);
      gtk_widget_show(sw[W_revscl_label]);
      
      sw[W_revscl_number] = gtk_label_new(revscl_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revscl_number], false, false, 0);
      gtk_widget_show(sw[W_revscl_number]);
      
      adjs[W_revscl_adj] = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_revscl] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_revscl_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revscl], true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(adjs[W_revscl_adj]),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(adjs[W_revscl_adj]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(revscl_changed_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_revscl]),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_revscl]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(revscl_release_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_revscl]);
      
      sw[W_revlen_event] = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revlen_event], false, false, 4);
      gtk_widget_show(sw[W_revlen_event]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_revlen_event]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_revlen_event]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(revlen_click_callback), (gpointer)sp, 0),
				     0);
      set_background(sw[W_revlen_event], (ss->sgx)->basic_color);
      
      sw[W_revlen_label] = gtk_label_new(_("len:"));
      gtk_container_add(GTK_CONTAINER(sw[W_revlen_event]), sw[W_revlen_label]);
      gtk_widget_show(sw[W_revlen_label]);
      
      sw[W_revlen_number] = gtk_label_new(revlen_number_buffer);
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revlen_number], false, false, 0);
      gtk_widget_show(sw[W_revlen_number]);
      
      adjs[W_revlen_adj] = gtk_adjustment_new(0.2, 0.0, 1.0, 0.001, 0.01, .1);
      sw[W_revlen] = gtk_hscrollbar_new(GTK_ADJUSTMENT(adjs[W_revlen_adj]));
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_revlen], true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(adjs[W_revlen_adj]),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(adjs[W_revlen_adj]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(revlen_changed_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_revlen]),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_revlen]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(revlen_release_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_revlen]);
      
      sw[W_reverb_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_reverb_form]), sw[W_reverb_button], false, false, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_reverb_button]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_reverb_button]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(reverb_button_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_reverb_button]);
      
      gtk_widget_show(sw[W_reverb_form]);
      
      
      /* -------- FILTER FIELDS -------- */
      
      sw[W_filter_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_filter_form], false, false, 0);
      set_background(sw[W_filter_form], (ss->sgx)->basic_color);
      
      sw[W_filter_label] = gtk_label_new(_("filter:"));
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_label], false, false, 4);
      gtk_widget_show(sw[W_filter_label]);
      
      adjs[W_filter_adj] = gtk_adjustment_new(20, 2, 100000, 2, 10, 0);
      sw[W_filter_order] = gtk_spin_button_new(GTK_ADJUSTMENT(adjs[W_filter_adj]), 0.0, 0);
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_order], false, false, 2);
      gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(sw[W_filter_order]), true);
      g_signal_connect_closure_by_id(GTK_OBJECT(adjs[W_filter_adj]),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(adjs[W_filter_adj]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_order_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_filter_order]);
      
      sw[W_filter] = snd_entry_new(sw[W_filter_form], false);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_filter]),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(sw[W_filter]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_activate_callback), (gpointer)sp, 0),
				     0);

      sw[W_filter_dB] = gtk_check_button_new_with_label(_("dB"));
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_dB], false, false, 2);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_filter_dB]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_filter_dB]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_db_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_filter_dB]);
      
      sw[W_filter_button] = gtk_check_button_new();
      gtk_box_pack_start(GTK_BOX(sw[W_filter_form]), sw[W_filter_button], false, false, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_filter_button]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_filter_button]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_button_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_filter_button]);
      
      gtk_widget_show(sw[W_filter_form]);
      
      
      /* -------- XEN_APPLY BUTTONS -------- */
      
      sw[W_apply_form] = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_apply_form], false, false, 0);
      set_background(sw[W_apply_form], (ss->sgx)->basic_color);
      
      sw[W_apply] = gtk_button_new_with_label(_("Apply"));
      gtk_box_pack_start(GTK_BOX(sw[W_apply_form]), sw[W_apply], true, true, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_apply]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_apply]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(apply_button_callback), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_apply]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_apply]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(apply_button_press_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_apply]);
      
      sw[W_remember] = gtk_button_new_with_label(_("Remember"));
      gtk_box_pack_start(GTK_BOX(sw[W_apply_form]), sw[W_remember], true, true, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_remember]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_remember]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(remember_button_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_remember]);
      
      sw[W_restore] = gtk_button_new_with_label(_("Restore"));
      gtk_box_pack_start(GTK_BOX(sw[W_apply_form]), sw[W_restore], true, true, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_restore]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_restore]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(restore_button_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_restore]);
      
      sw[W_reset] = gtk_button_new_with_label(_("Reset"));
      gtk_box_pack_start(GTK_BOX(sw[W_apply_form]), sw[W_reset], true, true, 0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_reset]),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sw[W_reset]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(reset_button_callback), (gpointer)sp, 0),
				     0);
      gtk_widget_show(sw[W_reset]);
      
      gtk_widget_show(sw[W_apply_form]);

      
      /* -------- FILTER GRAPH -------- */
      
      sw[W_filter_frame] = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(sw[W_control_panel]), sw[W_filter_frame], true, true, 10);
      
      sw[W_filter_env] = gtk_drawing_area_new();
      gtk_widget_set_events(sw[W_filter_env], GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(sw[W_filter_frame]), sw[W_filter_env]);
      set_background(sw[W_filter_env], (ss->sgx)->basic_color);
      gtk_widget_show(sw[W_filter_env]);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_filter_env]),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_filter_env]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_drawer_expose), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_filter_env]),
				     g_signal_lookup("configure_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_filter_env]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_drawer_resize), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_filter_env]),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_filter_env]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_drawer_button_press), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_filter_env]),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_filter_env]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_drawer_button_release), (gpointer)sp, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(sw[W_filter_env]),
				     g_signal_lookup("motion_notify_event", G_OBJECT_TYPE(GTK_OBJECT(sw[W_filter_env]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(filter_drawer_button_motion), (gpointer)sp, 0),
				     0);

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
      gtk_widget_show(sw[W_unite]);
      gtk_widget_show(sw[W_pane]);
      for (k = 0; k < nchans; k++) 
	add_channel_window(sp, k, chan_min_y, 0, NULL, WITH_FW_BUTTONS, true);
      gtk_label_set_text(GTK_LABEL(sw[W_name]), shortname_indexed(sp));

      if (sound_style(ss) == SOUNDS_IN_NOTEBOOK) 
	{
	  gtk_notebook_set_tab_label_text(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), sw[W_pane], sp->short_filename);
	  set_text_background(gtk_notebook_get_tab_label(GTK_NOTEBOOK(SOUND_PANE_BOX(ss)), 
							 sw[W_pane]), 
			      (ss->sgx)->basic_color);
	}
      else reset_controls(sp); /* segfault here in notebook case! */
    }

  if (sound_style(ss) == SOUNDS_IN_SEPARATE_WINDOWS)
    {
      title = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(title, PRINT_BUFFER_SIZE, "%d: %s", snd_slot, sp->short_filename);
      gtk_window_set_title(GTK_WINDOW(sx->dialog), title);
      FREE(title);
    }

  if (sp->nchans == 1) 
    gtk_widget_hide(sw[W_unite]);
  add_sound_data(filename, sp, WITH_GRAPH);

  snd_file_lock_icon(sp, (sp->read_only || (cant_write(sp->filename))));
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
  snd_context *sx;
  if ((sp) && (sp->sgx))
    {
      sx = sp->sgx;
      if ((sx->snd_widgets) && (sx->snd_widgets[W_pane])) gtk_widget_hide(sx->snd_widgets[W_pane]);
      sp->channel_style = CHANNELS_SEPARATE; 
    }
}

void set_apply_button(snd_info *sp, bool val) 
{
  gtk_widget_set_sensitive(APPLY_BUTTON(sp), val);
}

void equalize_sound_panes(snd_info *sp, chan_info *ncp, bool all_panes)
{
  return;
}

/* ---------------- normalize sounds ---------------- */

void equalize_all_panes(void)
{
  return;
}

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
  return(widget_height(CONTROL_PANEL(sp)) > CLOSED_CTRLS_HEIGHT);
}

void show_controls(void)
{
  snd_info *sp;
  int i;
  ss->ctrls_height = ss->open_ctrls_height;
  set_view_ctrls_label(_("Hide controls"));
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	sound_show_ctrls(sp);
    }
}

void hide_controls(void)
{
  snd_info *sp;
  int i;
  ss->ctrls_height = CLOSED_CTRLS_HEIGHT;
  set_view_ctrls_label(_("Show controls"));
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	sound_hide_ctrls(sp);
    }
}

int control_panel_height(snd_info *sp)
{
  return(widget_height(CONTROL_PANEL(sp)));
}

void sound_check_control_panel(snd_info *sp, int height)
{
  if (sp->inuse != SOUND_NORMAL) return;
  if ((!((sp->sgx)->controls_fixed)) && 
      (height > 50))
    {
      (sp->sgx)->controls_fixed = true;
      sound_hide_ctrls(sp);
    }
}


/* -------- PROGRESS REPORT -------- */

void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, enved_progress_t from_enved)
{
  int which;
  if (sp->inuse != SOUND_NORMAL) return;
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

static XEN g_sound_widgets(XEN snd)
{
  #define H_sound_widgets "(" S_sound_widgets " (snd #f)): a list of \
widgets: ((0)pane (1)name (2)control-panel (3)minibuffer (4)play-button (5)filter-env (6)unite-button (7)name-label (8)name-icon) (9)\
pane-box (10)name-form"
  snd_info *sp;
  ASSERT_JUST_SOUND(S_sound_widgets, snd, 1);
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
		   XEN_CONS(XEN_WRAP_WIDGET((sp->sgx)->snd_widgets[W_name_form]),
	            XEN_EMPTY_LIST))))))))))));
}

#ifdef XEN_ARGIFY_1
  XEN_ARGIFY_1(g_sound_widgets_w, g_sound_widgets)
#else
  #define g_sound_widgets_w g_sound_widgets
#endif

void g_init_gxsnd(void) 
{
  char dpoint;
  dpoint = local_decimal_point();
  amp_number_buffer[1] = dpoint;
  srate_number_buffer[1] = dpoint;
  expand_number_buffer[1] = dpoint;
  contrast_number_buffer[1] = dpoint;
  revscl_number_buffer[1] = dpoint;
  revlen_number_buffer[1] = dpoint;
  XEN_DEFINE_PROCEDURE(S_sound_widgets, g_sound_widgets_w, 0, 1, 0, H_sound_widgets);
}
