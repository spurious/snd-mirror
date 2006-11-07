#include "snd.h"

/* ---------------- mix dialog ---------------- */

static GtkWidget *mix_dialog = NULL;
static bool mix_dialog_slider_dragging = false;
static void update_mix_dialog(int mix_id);
static int mix_dialog_id = INVALID_MIX_ID;


/* -------- speed -------- */
static GtkWidget *w_speed, *w_speed_label, *w_speed_number, *w_speed_form, *w_speed_event, *w_speed_label_event;
static GtkObject *w_speed_adj;
static bool speed_pressed = false, speed_dragged = false;
/* can't use value_changed on adjustment and motion event happens even when the mouse merely moves across the slider without dragging */

static Float speed_to_scrollbar(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * ((log(val) - log(minval)) / (log(maxval) - log(minval))));
}

static Float scrollbar_to_speed(Float scroll)
{
  return(exp((scroll * (log(speed_control_max(ss)) - log(speed_control_min(ss))) / 0.9) + log(speed_control_min(ss))));
}

static Float set_speed_label(GtkWidget *label, Float in_speed)
{
  Float speed;
  char speed_number_buffer[6];
  speed = speed_changed(in_speed,
			speed_number_buffer,
			mix_speed_style(mix_dialog_id),
			speed_control_tones(ss),
			6);
  gtk_label_set_text(GTK_LABEL(label), speed_number_buffer);
  return(speed);
}

static void reflect_mix_speed(Float speed)
{
  GTK_ADJUSTMENT(w_speed_adj)->value = speed_to_scrollbar(speed_control_min(ss), set_speed_label(w_speed_number, speed), speed_control_max(ss));
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_speed_adj));
}

static gboolean speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* label click -- not part of slider stuff */
  speed_pressed = false;
  speed_dragged = false;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  mix_dialog_set_mix_speed(mix_dialog_id, 1.0, mix_dialog_slider_dragging);
  reflect_mix_speed(1.0);
  return(false);
}

static gboolean speed_label_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  /* (number) label click -- not part of slider stuff */
  speed_pressed = false;
  speed_dragged = false;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  switch (mix_speed_style(mix_dialog_id))
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    set_mix_speed_style(mix_dialog_id, SPEED_CONTROL_AS_RATIO, true);    break;
    case SPEED_CONTROL_AS_RATIO:    set_mix_speed_style(mix_dialog_id, SPEED_CONTROL_AS_SEMITONE, true); break;
    case SPEED_CONTROL_AS_SEMITONE: set_mix_speed_style(mix_dialog_id, SPEED_CONTROL_AS_FLOAT, true);    break;
    }
  set_speed_label(w_speed_number, mix_dialog_mix_speed(mix_dialog_id));
  return(false);
}

static gboolean speed_motion_callback(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  if (!speed_pressed) {speed_dragged = false; return(false);}
  speed_dragged = true;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  if (!mix_dialog_slider_dragging) mix_dialog_start_drag(mix_dialog_id);
  mix_dialog_slider_dragging = true;
  mix_dialog_set_mix_speed(mix_dialog_id, 
			   set_speed_label(w_speed_number, scrollbar_to_speed(GTK_ADJUSTMENT(w_speed_adj)->value)), 
			   true);
  return(false);
}

static gboolean speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data) 
{
  speed_pressed = false;
  /* if (!speed_dragged) return(false); */
  speed_dragged = false;
  mix_dialog_slider_dragging = false;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  mix_dialog_set_mix_speed(mix_dialog_id, 
			   set_speed_label(w_speed_number, scrollbar_to_speed(GTK_ADJUSTMENT(w_speed_adj)->value)), 
			   false);
  return(false);
}

static gboolean speed_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data) 
{
  speed_dragged = false;
  speed_pressed = true;
  return(false);
}

/* -------- amp -------- */
static GtkWidget **w_amps, **w_amp_labels, **w_amp_numbers, **w_amp_events, **w_amp_forms;
static GtkObject **w_amp_adjs;
#define CHANS_ALLOCATED 8

static Float scrollbar_to_amp(Float val)
{
  if (val <= 0.0) 
    return(amp_control_min(ss));
  if (val >= 0.9) 
    return(amp_control_max(ss));
  if (val > (0.5 * 0.9))
    return((((val / (0.5 * 0.9)) - 1.0) * (amp_control_max(ss) - 1.0)) + 1.0);
  else return((val * (1.0 - amp_control_min(ss)) / (0.5 * 0.9)) + amp_control_min(ss));
}

static bool amp_pressed = false, amp_dragged = false;

static void reflect_mix_amp(Float val, int chan)
{
  char sfs[6];
  GTK_ADJUSTMENT(w_amp_adjs[chan])->value = amp_to_scroll(amp_control_min(ss), val, amp_control_max(ss));
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_amp_adjs[chan]));
  mus_snprintf(sfs, 6, "%.2f", val);
  gtk_label_set_text(GTK_LABEL(w_amp_numbers[chan]), sfs);
}

static gboolean amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chan;
  amp_dragged = false;
  amp_pressed = false;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  chan = get_user_int_data(G_OBJECT(w));
  reflect_mix_amp(1.0, chan);
  mix_dialog_set_mix_amp(mix_dialog_id, chan, 1.0, mix_dialog_slider_dragging);    
  GTK_ADJUSTMENT(w_amp_adjs[chan])->value = amp_to_scroll(amp_control_min(ss), 1.0, amp_control_max(ss));
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_amp_adjs[chan]));
  return(false);
}

static gboolean amp_motion_callback(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  int chan;
  Float scrollval;
  if (!amp_pressed) {amp_dragged = false; return(false);}
  amp_dragged = true;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  if (!mix_dialog_slider_dragging) mix_dialog_start_drag(mix_dialog_id);
  mix_dialog_slider_dragging = true;
  chan = get_user_int_data(G_OBJECT(w));
  scrollval = GTK_ADJUSTMENT(w_amp_adjs[chan])->value;
  reflect_mix_amp(scrollbar_to_amp(scrollval), chan);
  mix_dialog_set_mix_amp(mix_dialog_id, chan, scrollbar_to_amp(scrollval), mix_dialog_slider_dragging);    
  return(false);
}

static gboolean amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chan;
  Float scrollval;
  mix_dialog_slider_dragging = false;
  amp_pressed = false;
  /* if (!amp_dragged) return(false); */
  amp_dragged = false;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  chan = get_user_int_data(G_OBJECT(w));
  scrollval = GTK_ADJUSTMENT(w_amp_adjs[chan])->value;
  reflect_mix_amp(scrollbar_to_amp(scrollval), chan);
  mix_dialog_set_mix_amp(mix_dialog_id, chan, scrollbar_to_amp(scrollval), mix_dialog_slider_dragging);
  return(false);
}

static gboolean amp_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  amp_pressed = true;
  amp_dragged = false;
  return(false);
}



/* -------- amp-envs -------- */
static GtkWidget *w_env_frame, *w_env;
static axis_context *ax = NULL;
static GdkGC *cur_gc;
static env_editor *spfs[8];
static int last_clicked_env_chan;
static bool with_mix_background_wave = false;

static void show_mix_background_wave(int mix_id, int chan)
{
  env_editor *e;
  int pts;
  bool two_sided = false;
  e = spfs[chan];
  if (e == NULL) return;
  pts = prepare_mix_id_waveform(mix_id, e->axis, &two_sided);
  if (pts > 0)
    {
      gdk_gc_set_foreground(ax->gc, ss->sgx->enved_waveform_color);
      if (two_sided)
	draw_both_grf_points(1, ax, pts, GRAPH_LINES);
      else draw_grf_points(1, ax, pts, e->axis, ungrf_y(e->axis, 0.0), GRAPH_LINES);
      gdk_gc_set_foreground(ax->gc, ss->sgx->black);
    }
}

static void mix_amp_env_resize(GtkWidget *w)
{
  int chans, chan;
  env **e;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  if (ax == NULL)
    {
      GdkWindow *wn;
      wn = MAIN_WINDOW(ss);
      cur_gc = gdk_gc_new(wn);
      gdk_gc_set_background(cur_gc, ss->sgx->graph_color);
      gdk_gc_set_foreground(cur_gc, ss->sgx->data_color);
      gdk_gc_set_function(cur_gc, GDK_COPY);
      ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      ax->wn = w_env->window;
      ax->w = w_env;
      ax->gc = cur_gc;
    }
  else clear_window(ax);
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  e = mix_dialog_envs(mix_dialog_id);
  for (chan = 0; chan < chans; chan++)
    {
      env *cur_env;
      spfs[chan]->with_dots = true;
      env_editor_display_env(spfs[chan], e[chan], ax, _("mix env"), (int)(chan * widget_width(w) / chans), 0,
			     widget_width(w) / chans, widget_height(w), NOT_PRINTING);
      cur_env = mix_dialog_mix_amp_env(mix_dialog_id, chan);
      if (cur_env)
	{
	  gdk_gc_set_foreground(ax->gc, ss->sgx->enved_waveform_color);
	  spfs[chan]->with_dots = false;
	  env_editor_display_env(spfs[chan], cur_env, ax, _("mix env"), (int)(chan * widget_width(w) / chans), 0,
				 widget_width(w) / chans, widget_height(w), NOT_PRINTING);
	  gdk_gc_set_foreground(ax->gc, ss->sgx->black);
	}
      if (with_mix_background_wave)
	show_mix_background_wave(mix_dialog_id, chan);
    }
}

static gboolean mix_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chans, chan;
  env *e;
  Float pos;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_dialog_env(mix_dialog_id, chan);
  spfs[chan]->with_dots = false;
  if (env_editor_button_press(spfs[chan], (int)(ev->x), (int)(ev->y), ev->time, e))
    mix_amp_env_resize(w);
  return(false);
}

static gboolean mix_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chans, chan;
  env *e;
  Float pos;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_dialog_env(mix_dialog_id, chan);
  env_editor_button_release(spfs[chan], e);
  mix_amp_env_resize(w);
  return(false);
}

static gboolean mix_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  if (ev->state & GDK_BUTTON1_MASK)
    {
      int chans, chan, x, y;
      GdkModifierType state;
      env *e;
      Float pos;
      if (ev->is_hint)
	gdk_window_get_pointer(ev->window, &x, &y, &state);
      else
	{
	  x = (int)(ev->x);
	  y = (int)(ev->y);
	}
      chans = mix_dialog_mix_input_chans(mix_dialog_id);
      pos = (Float)x / (Float)widget_width(w);
      chan = (int)(pos * chans);
      last_clicked_env_chan = chan;
      e = mix_dialog_env(mix_dialog_id, chan);
      spfs[chan]->with_dots = false;
      env_editor_button_motion(spfs[chan], x, y, ev->time, e);
      mix_amp_env_resize(w);
    }
  return(false);
}

static gboolean mix_amp_env_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  mix_amp_env_resize(w);
  return(false);
}

static gboolean mix_amp_env_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return(false);
  mix_amp_env_resize(w);
  return(false);
}


/* -------- track -------- */
static GtkWidget *w_id = NULL, *w_beg = NULL, *w_track = NULL, *mix_play = NULL, *w_id_label = NULL;
static GtkWidget *w_track_label = NULL, *mix_track_play_pix = NULL, *mix_play_pix = NULL, *mix_track_play = NULL, *w_mix_pan, *w_mix_pan1;
static GdkPixmap *speaker_off_pix, *speaker_on_pix, *mix_speaker_pix, *mix_track_speaker_pix, *mix_pan_pix, *mix_yellow_pan_pix, *mix_basic_pan_pix;

static bool id_changed = false;

static GtkWidget *error_frame = NULL, *error_label = NULL;

static void clear_mix_error(void)
{
  if ((error_frame) && (GTK_WIDGET_VISIBLE(error_frame)))
    gtk_widget_hide(error_frame);
}

static gint unpost_mix_error(gpointer data)
{
  clear_mix_error();
  return(0);
}

static void errors_to_mix_text(const char *msg, void *data)
{
  set_label(error_label, msg);
  gtk_widget_show(error_frame);
  g_timeout_add_full(0, (guint32)5000, unpost_mix_error, NULL, NULL);
}

static void id_activated(GtkWidget *w, gpointer context)
{
  char *val;
  id_changed = false;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_id));
  if (val)
    {
      int id;
      /* look for a mix name first, then a number */
      id = mix_name_to_id(val);
      if (id < 0)
	{
	  redirect_errors_to(errors_to_mix_text, NULL);
	  id = string_to_int(val, 0, "id");
	  redirect_errors_to(NULL, NULL);
	}
      if (mix_ok_and_unlocked(id)) 
	{
	  mix_dialog_id = id;
	  update_mix_dialog(id);
	}
    }
}

static gboolean id_check_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (id_changed) id_activated(w_id, NULL);
  return(false);
}

static gboolean id_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  id_changed = true;
  return(false);
}

static void beg_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_beg));
  if (val)
    {
      chan_info *cp;
      Float beg;
      char *up_to_colon;
      up_to_colon = string_to_colon(val);
      cp = mix_dialog_mix_channel(mix_dialog_id);
      redirect_errors_to(errors_to_mix_text, NULL);
      beg = string_to_Float(up_to_colon, 0.0, "begin time");
      redirect_errors_to(NULL, NULL);
      if (beg >= 0.0)
	set_mix_position(mix_dialog_id, (off_t)(beg * SND_SRATE(cp->sound)));
      update_mix_dialog(mix_dialog_id);
      FREE(up_to_colon);
    }
}

static void widget_track_to_text(GtkWidget *w, int trk)
{
  if (track_name(trk))
    gtk_entry_set_text(GTK_ENTRY(w), track_name(trk));
  else widget_int_to_text(w, trk);
}

static void widget_mix_to_track(GtkWidget *w, int id)
{
  if (mix_name(id))
    gtk_entry_set_text(GTK_ENTRY(w), mix_name(id));
  else widget_int_to_text(w, id);
}


static bool track_changed = false;

static void mix_track_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  track_changed = false;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track));
  if (val)
    {
      int trk;
      /* look for a track name first, then a number */
      trk = track_name_to_id(val);
      if (trk < 0)
	{
	  redirect_errors_to(errors_to_mix_text, NULL);
	  trk = string_to_int(val, 0, "track");
	  redirect_errors_to(NULL, NULL);
	}
      if (trk >= 0)
	mix_dialog_set_mix_track(mix_dialog_id, trk);
      else widget_track_to_text(w_track, mix_dialog_mix_track(mix_dialog_id));
    }
}

static gboolean track_check_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (track_changed) mix_track_activated(w_track, NULL);
  return(false);
}

static gboolean track_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  track_changed = true;
  return(false);
}


/* -------- mix play -------- */
static bool mix_playing = false;
bool mix_play_stopped(void) {return(!mix_playing);}

void reflect_mix_play_stop(void)
{
  mix_playing = false;
  mix_speaker_pix = speaker_off_pix;
  mix_track_speaker_pix = speaker_off_pix;
  if (mix_play_pix)
    gdk_draw_drawable(GDK_DRAWABLE(mix_play_pix->window), ss->sgx->basic_gc, mix_speaker_pix, 0, 0, 2, 4, 12, 12);
  if (mix_track_play_pix)
    gdk_draw_drawable(GDK_DRAWABLE(mix_track_play_pix->window), ss->sgx->basic_gc, mix_track_speaker_pix, 0, 0, 2, 4, 12, 12);
}

static void mix_play_callback(GtkWidget *w, gpointer context) 
{
  if (mix_playing)
    reflect_mix_play_stop();
  else
    {
      if (!(mix_ok(mix_dialog_id))) return;
      mix_playing = true;
      mix_speaker_pix = speaker_on_pix;
      gdk_draw_drawable(GDK_DRAWABLE(mix_play_pix->window), ss->sgx->basic_gc, mix_speaker_pix, 0, 0, 2, 4, 12, 12);
      mix_dialog_mix_play(mix_dialog_id);
    }
}

static void mix_track_play_callback(GtkWidget *w, gpointer context) 
{
  if (mix_playing)
    reflect_mix_play_stop();
  else
    {
      if (!(mix_ok(mix_dialog_id))) return;
      mix_playing = true;
      mix_track_speaker_pix = speaker_on_pix;
      gdk_draw_drawable(GDK_DRAWABLE(mix_track_play_pix->window), ss->sgx->basic_gc, mix_track_speaker_pix, 0, 0, 2, 4, 12, 12);
      mix_dialog_track_play(mix_dialog_id);
    }
}

static gboolean mix_play_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  gdk_draw_drawable(GDK_DRAWABLE(mix_play_pix->window), ss->sgx->basic_gc, mix_speaker_pix, 0, 0, 2, 4, 12, 12);
  return(false);
}

static gboolean mix_track_play_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  gdk_draw_drawable(GDK_DRAWABLE(mix_track_play_pix->window), ss->sgx->basic_gc, mix_track_speaker_pix, 0, 0, 2, 4, 12, 12);
  return(false);
}


static gboolean w_mix_pan_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  gdk_draw_drawable(GDK_DRAWABLE(w_mix_pan1->window), ss->sgx->basic_gc, mix_pan_pix, 0, 0, 0, 4, 14, 12);
  return(false);
}

static void mix_pan_callback(GtkWidget *w, gpointer context) 
{
  bool inverted;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  inverted = (!(mix_dialog_mix_inverted(mix_dialog_id)));
  if (inverted) mix_pan_pix = mix_yellow_pan_pix; else mix_pan_pix = mix_basic_pan_pix;
  gdk_draw_drawable(GDK_DRAWABLE(w_mix_pan1->window), ss->sgx->basic_gc, mix_pan_pix, 0, 0, 0, 4, 14, 12);
  mix_dialog_set_mix_inverted(mix_dialog_id, inverted);
}

static void mix_dB_callback(GtkWidget *w, gpointer context) 
{
  int i;
  for (i = 0; i < CHANS_ALLOCATED; i++)
    if (spfs[i])
      spfs[i]->in_dB = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  mix_amp_env_resize(w_env);
}

static void mix_clip_callback(GtkWidget *w, gpointer context) 
{
  int i;
  for (i = 0; i < CHANS_ALLOCATED; i++)
    if (spfs[i])
      spfs[i]->clip_p = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  mix_amp_env_resize(w_env);
}

static void mix_wave_callback(GtkWidget *w, gpointer context) 
{
  with_mix_background_wave = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  mix_amp_env_resize(w_env);
}

static void apply_mix_dialog(GtkWidget *w, gpointer context)
{
  /* set all mix amp envs, last one should remix */
  int i, chans;
  env **envs;
  if (!(mix_ok_and_unlocked(mix_dialog_id))) return;
  chans = mix_dialog_mix_input_chans(mix_dialog_id);
  envs = mix_dialog_envs(mix_dialog_id);
  for (i = 0; i < chans; i++)
    if (i != last_clicked_env_chan)
      mix_dialog_set_mix_amp_env_without_edit(mix_dialog_id, i, envs[i]);
  set_mix_amp_env(mix_dialog_id, last_clicked_env_chan, envs[last_clicked_env_chan]);
  mix_amp_env_resize(w_env);
}

static void dismiss_mix_dialog(GtkWidget *w, gpointer context)
{
  clear_mix_error();
  gtk_widget_hide(mix_dialog);
}

static gint delete_mix_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  clear_mix_error();
  gtk_widget_hide(mix_dialog);
  return(true);
}

static GtkWidget *nextb, *previousb, *apply_button;

static void mix_next_callback(GtkWidget *w, gpointer context)
{
  int id;
  clear_mix_error();
  id = next_mix_id(mix_dialog_id);
  if (id != INVALID_MIX_ID)
    {
      mix_dialog_id = id;
      update_mix_dialog(id);
      if (next_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(nextb, false);
    }
}

static void mix_previous_callback(GtkWidget *w, gpointer context)
{
  int id;
  clear_mix_error();
  id = previous_mix_id(mix_dialog_id);
  if (id != INVALID_MIX_ID)
    {
      mix_dialog_id = id;
      update_mix_dialog(id);
      if (previous_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(previousb, false);
    }
}

static void mix_dialog_help_callback(GtkWidget *w, gpointer context) 
{
  mix_dialog_help();
}

GtkWidget *make_mix_dialog(void)
{
  if (mix_dialog == NULL)
    {
      GtkWidget *dismiss_button, *help_button, *rc, *mix_frame, *track_frame, *rc_top, *rc1;
      GtkWidget *lo_hbox, *w_dB_frame, *w_dB, *w_clip, *w_wave, *w_dB_row;
      char amplab[LABEL_BUFFER_SIZE];
      int i;
      mix_dialog_id = any_mix_id();
      mix_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(mix_dialog, "delete_event", delete_mix_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(mix_dialog), _("Mixes"));
      sg_make_resizable(mix_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(mix_dialog), 6);
      gtk_widget_realize(mix_dialog);
      
      dismiss_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismiss_button, "quit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), dismiss_button, false, true, 10);
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", dismiss_mix_dialog, NULL);
      gtk_widget_show(dismiss_button);

      previousb = gtk_button_new_from_stock(GTK_STOCK_GO_BACK);
      gtk_widget_set_name(previousb, "reset_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), previousb, false, true, 10);
      SG_SIGNAL_CONNECT(previousb, "clicked", mix_previous_callback, NULL);
      gtk_widget_show(previousb);

      nextb = gtk_button_new_from_stock(GTK_STOCK_GO_FORWARD);
      gtk_widget_set_name(nextb, "doit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), nextb, false, true, 10);
      SG_SIGNAL_CONNECT(nextb, "clicked", mix_next_callback, NULL);
      gtk_widget_show(nextb);

      apply_button = sg_button_new_from_stock_with_label(_("Apply Env"), GTK_STOCK_APPLY);
      gtk_widget_set_name(apply_button, "doit_again_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), apply_button, false, true, 10);
      SG_SIGNAL_CONNECT(apply_button, "clicked", apply_mix_dialog, NULL);
      gtk_widget_show(apply_button);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), help_button, true, true, 10);
      SG_SIGNAL_CONNECT(help_button, "clicked", mix_dialog_help_callback, NULL);
      gtk_widget_show(help_button);

      /* normally hidden error indication at top */
      error_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->vbox), error_frame, false, false, 4);

      error_label = gtk_label_new("");
      gtk_container_add(GTK_CONTAINER(error_frame), error_label);
      gtk_widget_show(error_label);


      /* top row of mix id name position track etc */

      rc_top = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->vbox), rc_top, false, false, 4);
      gtk_widget_show(rc_top);

      mix_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(rc_top), mix_frame, false, false, 4);
      gtk_widget_show(mix_frame);

      track_frame = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(rc_top), track_frame, false, false, 4);
      gtk_widget_show(track_frame);

      rc = gtk_hbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(mix_frame), rc);
      gtk_widget_show(rc);

      rc1 = gtk_hbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(track_frame), rc1);
      gtk_widget_show(rc1);

      w_id_label = gtk_label_new(_("mix:"));
      gtk_box_pack_start(GTK_BOX(rc), w_id_label, false, false, 4);
      gtk_widget_show(w_id_label);

      w_id = snd_entry_new(rc, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(w_id, "activate", id_activated, NULL);
      SG_SIGNAL_CONNECT(w_id, "leave_notify_event", id_check_callback, NULL);
      SG_SIGNAL_CONNECT(w_id, "key_press_event", id_modify_callback, NULL);

      w_beg = snd_entry_new(rc, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(w_beg, "activate", beg_activated, NULL);

      mix_play = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(rc), mix_play, false, false, 2);
      SG_SIGNAL_CONNECT(mix_play, "clicked", mix_play_callback, NULL);
      gtk_widget_show(mix_play);
      gtk_widget_modify_bg(mix_play, GTK_STATE_ACTIVE, ss->sgx->basic_color);
      gtk_widget_modify_bg(mix_play, GTK_STATE_SELECTED, ss->sgx->basic_color);
      
      if (!speaker_off_pix)
	{
	  speaker_off_pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, speaker_bits());
	  speaker_on_pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, blue_speaker_bits());
	}

      mix_speaker_pix = speaker_off_pix;
      mix_track_speaker_pix = speaker_off_pix;
      mix_play_pix = gtk_drawing_area_new();
      gtk_widget_set_events(mix_play_pix, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(mix_play_pix, 16, 16);
      gtk_container_add(GTK_CONTAINER(mix_play), mix_play_pix);
      gtk_widget_show(mix_play_pix);
      SG_SIGNAL_CONNECT(mix_play_pix, "expose_event", mix_play_pix_expose, NULL);


      w_track_label = gtk_label_new(_("track:"));
      gtk_box_pack_start(GTK_BOX(rc1), w_track_label, false, false, 4);
      gtk_widget_show(w_track_label);

      w_track = snd_entry_new(rc1, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(w_track, "activate", mix_track_activated, NULL);
      SG_SIGNAL_CONNECT(w_track, "leave_notify_event", track_check_callback, NULL);
      SG_SIGNAL_CONNECT(w_track, "key_press_event", track_modify_callback, NULL);

      mix_track_play = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(rc1), mix_track_play, false, false, 2);
      SG_SIGNAL_CONNECT(mix_track_play, "clicked", mix_track_play_callback, NULL);
      gtk_widget_show(mix_track_play);
      gtk_widget_modify_bg(mix_track_play, GTK_STATE_ACTIVE, ss->sgx->basic_color);
      gtk_widget_modify_bg(mix_track_play, GTK_STATE_SELECTED, ss->sgx->basic_color);

      mix_track_play_pix = gtk_drawing_area_new();
      gtk_widget_set_events(mix_track_play_pix, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(mix_track_play_pix, 16, 16);
      gtk_container_add(GTK_CONTAINER(mix_track_play), mix_track_play_pix);
      gtk_widget_show(mix_track_play_pix);
      SG_SIGNAL_CONNECT(mix_track_play_pix, "expose_event", mix_track_play_pix_expose, NULL);

      mix_basic_pan_pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, pan_bits());
      mix_yellow_pan_pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, yellow_pan_bits());
      mix_pan_pix = mix_basic_pan_pix;
      w_mix_pan = gtk_button_new();
      gtk_box_pack_end(GTK_BOX(rc1), w_mix_pan, false, false, 2);
      SG_SIGNAL_CONNECT(w_mix_pan, "clicked", mix_pan_callback, NULL);
      gtk_widget_show(w_mix_pan);

      w_mix_pan1 = gtk_drawing_area_new();
      gtk_widget_set_events(w_mix_pan1, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(w_mix_pan1, 16, 16);
      gtk_container_add(GTK_CONTAINER(w_mix_pan), w_mix_pan1);
      gtk_widget_show(w_mix_pan1);
      SG_SIGNAL_CONNECT(w_mix_pan1, "expose_event", w_mix_pan_pix_expose, NULL);

      /* SPEED */
      w_speed_form = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->vbox), w_speed_form, false, false, 4);
      
      w_speed_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed_event, false, false, 4);
      gtk_widget_show(w_speed_event);
      SG_SIGNAL_CONNECT(w_speed_event, "button_press_event", speed_click_callback, NULL);
      
      w_speed_label = gtk_label_new(_("speed:"));
      gtk_container_add(GTK_CONTAINER(w_speed_event), w_speed_label);
      gtk_widget_show(w_speed_label);

      w_speed_label_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed_label_event, false, false, 4);
      gtk_widget_show(w_speed_label_event);
      SG_SIGNAL_CONNECT(w_speed_label_event, "button_press_event", speed_label_click_callback, NULL);

      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO:    w_speed_number = gtk_label_new("1/1");  break;
	case SPEED_CONTROL_AS_SEMITONE: w_speed_number = gtk_label_new("1");    break;
	default:                        w_speed_number = gtk_label_new("1.00"); break;
	}
      gtk_container_add(GTK_CONTAINER(w_speed_label_event), w_speed_number);
      gtk_widget_show(w_speed_number);
      
      w_speed_adj = gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
      w_speed = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_speed_adj));
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed, true, true, 4);
      SG_SIGNAL_CONNECT(w_speed, "button_release_event", speed_release_callback, NULL);
      SG_SIGNAL_CONNECT(w_speed, "motion_notify_event", speed_motion_callback, NULL);
      SG_SIGNAL_CONNECT(w_speed, "button_press_event", speed_press_callback, NULL);
      gtk_widget_show(w_speed);
      gtk_widget_show(w_speed_form);


      /* AMP */
      w_amp_numbers = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amp_labels = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amps = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amp_events = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amp_forms = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amp_adjs = (GtkObject **)CALLOC(CHANS_ALLOCATED, sizeof(GtkObject *));

      for (i = 0; i < CHANS_ALLOCATED; i++) spfs[i] = new_env_editor();

      for (i = 0; i < CHANS_ALLOCATED; i++)
	{
	  w_amp_forms[i] = gtk_hbox_new(false, 2);
	  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->vbox), w_amp_forms[i], false, false, 0);
      
	  w_amp_events[i] = gtk_event_box_new();
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amp_events[i], false, false, 4);
	  gtk_widget_show(w_amp_events[i]);
	  SG_SIGNAL_CONNECT(w_amp_events[i], "button_press_event", amp_click_callback, NULL);
      
	  mus_snprintf(amplab, LABEL_BUFFER_SIZE, _("amp %d:"), i);
	  w_amp_labels[i] = gtk_label_new(amplab);
	  gtk_container_add(GTK_CONTAINER(w_amp_events[i]), w_amp_labels[i]);
	  set_user_int_data(G_OBJECT(w_amp_events[i]), i);
	  gtk_widget_show(w_amp_labels[i]);
      
	  w_amp_numbers[i] = gtk_label_new("1.00");
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amp_numbers[i], false, false, 0);
	  gtk_widget_show(w_amp_numbers[i]);
	  
	  w_amp_adjs[i] = gtk_adjustment_new((i == 0) ? 0.5 : 0.0, 0.0, 1.0, 0.001, 0.01, .1);
	  w_amps[i] = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_amp_adjs[i]));
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amps[i], true, true, 4);

	  SG_SIGNAL_CONNECT(w_amps[i], "motion_notify_event", amp_motion_callback, NULL);
	  SG_SIGNAL_CONNECT(w_amps[i], "button_release_event", amp_release_callback, NULL);
	  SG_SIGNAL_CONNECT(w_amps[i], "button_press_event", amp_press_callback, NULL);
	  set_user_int_data(G_OBJECT(w_amps[i]), i);	  
	  set_user_int_data(G_OBJECT(w_amp_adjs[i]), i);
	  gtk_widget_show(w_amps[i]);
      
	  gtk_widget_show(w_amp_forms[i]);
	}

      lo_hbox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->vbox), lo_hbox, true, true, 5);
      gtk_widget_show(lo_hbox);

      /* GRAPH (frame) */
      w_env_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(lo_hbox), w_env_frame, true, true, 10);

      /* GRAPH (buttons) */
      w_dB_frame = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(lo_hbox), w_dB_frame, false, false, 2);
      gtk_widget_show(w_dB_frame);

      w_dB_row = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(w_dB_frame), w_dB_row);
      gtk_widget_show(w_dB_row);	

      w_clip = gtk_check_button_new_with_label(_("clip"));
      SG_SIGNAL_CONNECT(w_clip, "toggled", mix_clip_callback, NULL);
      gtk_box_pack_start(GTK_BOX(w_dB_row), w_clip, false, false, 0);
      gtk_widget_show(w_clip);

      w_wave = gtk_check_button_new_with_label(_("wave"));
      SG_SIGNAL_CONNECT(w_wave, "toggled", mix_wave_callback, NULL);
      gtk_box_pack_start(GTK_BOX(w_dB_row), w_wave, false, false, 0);
      gtk_widget_show(w_wave);

      w_dB = gtk_check_button_new_with_label(_("dB"));
      SG_SIGNAL_CONNECT(w_dB, "toggled", mix_dB_callback, NULL);
      gtk_box_pack_start(GTK_BOX(w_dB_row), w_dB, false, false, 0);
      gtk_widget_show(w_dB);

      /* GRAPH (drawing area) */
      w_env = gtk_drawing_area_new();
      gtk_widget_set_events(w_env, GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(w_env_frame), w_env);
      gtk_widget_modify_bg(w_env, GTK_STATE_NORMAL, ss->sgx->highlight_color);
      gtk_widget_show(w_env);
      SG_SIGNAL_CONNECT(w_env, "expose_event", mix_amp_env_expose_callback, NULL);
      SG_SIGNAL_CONNECT(w_env, "configure_event", mix_amp_env_resize_callback, NULL);
      SG_SIGNAL_CONNECT(w_env, "button_press_event", mix_drawer_button_press, NULL);
      SG_SIGNAL_CONNECT(w_env, "button_release_event", mix_drawer_button_release, NULL);
      SG_SIGNAL_CONNECT(w_env, "motion_notify_event", mix_drawer_button_motion, NULL);
      gtk_widget_show(w_env_frame);

      gtk_widget_show(mix_dialog);
      set_dialog_widget(MIX_DIALOG, mix_dialog);
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w_clip), true);

      gtk_widget_hide(error_frame);
    }
  else 
    {
      raise_dialog(mix_dialog);
      if (mix_dialog_id != INVALID_MIX_ID) reflect_edit_in_mix_dialog_envs(mix_dialog_id);
    }
  update_mix_dialog(mix_dialog_id);
  return(mix_dialog);
}

static bool track_dialog_slider_dragging = false;

static void update_mix_dialog(int mix_id) 
{
  if (!(mix_ok(mix_dialog_id)))
    {
      mix_dialog_id = mix_id; /* close-sound kills current mix, for example */
      if (!(mix_ok(mix_dialog_id))) 
	{
	  mix_dialog_id = any_mix_id(); 
	  mix_id = mix_dialog_id;
	}
    }
  if ((mix_id == mix_dialog_id) || (mix_id == ANY_MIX_ID))
    {
      int chans = 0;
      Float val;
      if (mix_dialog == NULL) 
	make_mix_dialog();
      else
	{
	  set_sensitive(nextb, (next_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	}
      
      /* now reflect current mix state in mix dialog controls */
      if (mix_ok(mix_dialog_id))
	{
	  char lab[LABEL_BUFFER_SIZE];
	  chan_info *cp;
	  off_t beg, len;
	  cp = mix_dialog_mix_channel(mix_dialog_id);
	  val = mix_dialog_mix_speed(mix_dialog_id);
	  reflect_mix_speed(val);

	  widget_track_to_text(w_track, mix_dialog_mix_track(mix_dialog_id));
	  widget_mix_to_track(w_id, mix_dialog_id);

	  beg = mix_dialog_mix_position(mix_dialog_id);
	  len = mix_frames(mix_dialog_id);
	  mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f%s",
		       (float)((double)beg / (float)SND_SRATE(cp->sound)),
		       (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)),
		       (mix_ok_and_unlocked(mix_dialog_id)) ? "" : " (locked)");
	  gtk_entry_set_text(GTK_ENTRY(w_beg), lab);

	  chans = mix_dialog_mix_input_chans(mix_dialog_id);
	  if (chans > 8) chans = 8;
	  set_sensitive(apply_button, true);
	  if (mix_dialog_mix_inverted(mix_dialog_id))
	    mix_pan_pix = mix_yellow_pan_pix; 
	  else mix_pan_pix = mix_basic_pan_pix;
	  gdk_draw_drawable(GDK_DRAWABLE(w_mix_pan1->window), ss->sgx->basic_gc, mix_pan_pix, 0, 0, 0, 4, 14, 12);
	}
      else
	{
	  chans = 1;
	  gtk_entry_set_text(GTK_ENTRY(w_track), "0");
	  gtk_entry_set_text(GTK_ENTRY(w_id), "-1");
	  gtk_entry_set_text(GTK_ENTRY(w_beg), "no active mixes");
	  set_sensitive(apply_button, false);
	}
      if ((!mix_dialog_slider_dragging) && (!track_dialog_slider_dragging))
	{
	  int i;
	  set_label(w_amp_labels[0], (chans == 1) ? "amp:" : "amp 0:");
	  for (i = 0; i < chans; i++)
	    {
	      gtk_widget_show(w_amp_labels[i]);	  
	      gtk_widget_show(w_amp_numbers[i]);	  
	      gtk_widget_show(w_amps[i]);
	      gtk_widget_show(w_amp_forms[i]);
	      if (mix_ok_and_unlocked(mix_dialog_id))
		val = mix_dialog_mix_amp(mix_dialog_id, i);
	      else val = 1.0;
	      reflect_mix_amp(val, i);
	    }
	  for (i = chans; i < CHANS_ALLOCATED; i++)
	    {
	      gtk_widget_hide(w_amp_labels[i]);	  
	      gtk_widget_hide(w_amp_numbers[i]);	  
	      gtk_widget_hide(w_amps[i]);
	      gtk_widget_hide(w_amp_forms[i]);
	    }
	}
      mix_amp_env_resize(w_env);
    }
}


/* ---------------- track dialog ---------------- */

static GtkWidget *track_dialog = NULL;
static void update_track_dialog(int track_id);
static int track_dialog_id = INVALID_TRACK_ID;

/* -------- speed -------- */
static GtkWidget *w_track_speed, *w_track_speed_label, *w_track_speed_number, *w_track_speed_form, *w_track_speed_event, *w_track_speed_label_event;
static GtkObject *w_track_speed_adj;
static bool track_speed_pressed = false;

static void reflect_track_speed(Float speed)
{
  GTK_ADJUSTMENT(w_track_speed_adj)->value = speed_to_scrollbar(speed_control_min(ss), set_speed_label(w_track_speed_number, speed), speed_control_max(ss));
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_track_speed_adj));
}

static gboolean track_speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_speed_pressed = false;
  track_dialog_slider_dragging = false;
  if (!(track_p(track_dialog_id))) return(false);
  track_dialog_set_speed(track_dialog_id, 1.0);
  reflect_track_speed(1.0);
  return(false);
}

static gboolean track_speed_label_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_speed_pressed = false;
  if (!(track_p(track_dialog_id))) return(false);
  switch (track_speed_style(track_dialog_id))
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    set_track_speed_style(track_dialog_id, SPEED_CONTROL_AS_RATIO, true);    break;
    case SPEED_CONTROL_AS_RATIO:    set_track_speed_style(track_dialog_id, SPEED_CONTROL_AS_SEMITONE, true); break;
    case SPEED_CONTROL_AS_SEMITONE: set_track_speed_style(track_dialog_id, SPEED_CONTROL_AS_FLOAT, true);    break;
    }
  set_speed_label(w_track_speed_number, track_dialog_track_speed(track_dialog_id));
  return(false);
}

static gboolean track_speed_motion_callback(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  if (!track_speed_pressed) return(false);
  if (!(track_p(track_dialog_id))) return(false);
  set_speed_label(w_track_speed_number, scrollbar_to_speed(GTK_ADJUSTMENT(w_track_speed_adj)->value));
  return(false);
}

static gboolean track_speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_speed_pressed = false;
  track_dialog_slider_dragging = false;
  if (!(track_p(track_dialog_id))) return(false);
  track_dialog_set_speed(track_dialog_id, 
			 set_speed_label(w_track_speed_number, scrollbar_to_speed(GTK_ADJUSTMENT(w_track_speed_adj)->value)));
  return(false);
}

static gboolean track_speed_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_speed_pressed = true;
  return(false);
}


/* -------- tempo -------- */
static GtkWidget *w_track_tempo, *w_track_tempo_label, *w_track_tempo_number, *w_track_tempo_event, *w_track_tempo_form;
static GtkObject *w_track_tempo_adj;
static bool track_tempo_pressed = false, track_tempo_dragged = false;

static Float tempo_to_scrollbar(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  if (val >= 1.0)
    return(0.9 * 0.5 * (1.0 + (val - 1.0) / (maxval - 1.0)));
  return(0.9 * 0.5 * ((val - minval) / (1.0 - minval)));
}

static Float scrollbar_to_tempo(Float val)
{
  char tempo_number_buffer[5];
  Float tempo;
  if (val <= 0.0) 
    tempo = tempo_control_min(ss);
  else
    {
      if (val >= 0.9)
	tempo = tempo_control_max(ss);
      else
	{
	  if (val > (0.5 * 0.9))
	    tempo = (((val / (0.5 * 0.9)) - 1.0) * (tempo_control_max(ss) - 1.0)) + 1.0;
	  else tempo = (val * (1.0 - tempo_control_min(ss)) / (0.5 * 0.9)) + tempo_control_min(ss);
	}
    }
  mus_snprintf(tempo_number_buffer, 5, "%.2f", tempo);
  gtk_label_set_text(GTK_LABEL(w_track_tempo_number), tempo_number_buffer);
  return(tempo);
}

static gboolean track_tempo_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_tempo_pressed = false;
  track_tempo_dragged = false;
  track_dialog_slider_dragging = false;
  if (!(track_p(track_dialog_id))) return(false);
  track_dialog_set_tempo(track_dialog_id, 1.0, false);
  gtk_label_set_text(GTK_LABEL(w_track_tempo_number), "1.00");
  GTK_ADJUSTMENT(w_track_tempo_adj)->value = tempo_to_scrollbar(tempo_control_min(ss), 1.0, tempo_control_max(ss));
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_track_tempo_adj));
  return(false);
}

static gboolean track_tempo_motion_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (!track_tempo_pressed) {track_tempo_dragged = false; return(false);}
  track_tempo_dragged = true;
  if (!(track_p(track_dialog_id))) return(false);
  if (!track_dialog_slider_dragging) track_dialog_start_slider_drag(track_dialog_id);
  track_dialog_slider_dragging = true;
  track_dialog_set_tempo(track_dialog_id, scrollbar_to_tempo(GTK_ADJUSTMENT(w_track_tempo_adj)->value), true);
  return(false);
}

static gboolean track_tempo_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_tempo_pressed = false;
  track_dialog_slider_dragging = false;
  /* if (!track_tempo_dragged) return(false); */
  if (!(track_p(track_dialog_id))) return(false);
  track_dialog_set_tempo(track_dialog_id, scrollbar_to_tempo(GTK_ADJUSTMENT(w_track_tempo_adj)->value), false);
  return(false);
}

static gboolean track_tempo_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_tempo_pressed = true;
  track_tempo_dragged = false;
  return(false);
}



/* -------- amp -------- */
static GtkWidget *w_track_amp, *w_track_amp_label, *w_track_amp_number, *w_track_amp_event, *w_track_amp_form;
static GtkObject *w_track_amp_adj;
static bool track_amp_pressed = false;

static void reflect_track_amp(Float val)
{
  char sfs[6];
  mus_snprintf(sfs, 6, "%.2f", val);
  gtk_label_set_text(GTK_LABEL(w_track_amp_number), sfs);
  GTK_ADJUSTMENT(w_track_amp_adj)->value = amp_to_scroll(amp_control_min(ss), val, amp_control_max(ss));
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_track_amp_adj));
}

static void change_track_amp(Float val)
{
  reflect_track_amp(val);
  track_dialog_set_amp(track_dialog_id, val);
}

static gboolean track_amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_amp_pressed = false;
  track_dialog_slider_dragging = false;
  if (!(track_p(track_dialog_id))) return(false);
  change_track_amp(1.0);
  return(false);
}

static gboolean track_amp_motion_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  if (!track_amp_pressed) return(false);
  if (!(track_p(track_dialog_id))) return(false);
  reflect_track_amp(scrollbar_to_amp(GTK_ADJUSTMENT(w_track_amp_adj)->value));
  return(false);
}

static gboolean track_amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_amp_pressed = false;
  track_dialog_slider_dragging = false;
  if (!(track_p(track_dialog_id))) return(false);
  change_track_amp(scrollbar_to_amp(GTK_ADJUSTMENT(w_track_amp_adj)->value));
  return(false);
}

static gboolean track_amp_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  track_amp_pressed = true;
  return(false);
}


/* -------- amp-env -------- */
static GtkWidget *w_track_env_frame, *w_track_env;
static axis_context *track_ax = NULL;
static GdkGC *track_cur_gc;
static env_editor *track_spf;
static bool with_track_background_wave = false;

void show_track_background_wave(int pts, bool two_sided)
{
  gdk_gc_set_foreground(track_ax->gc, ss->sgx->enved_waveform_color);
  if (two_sided)
    draw_both_grf_points(1, track_ax, pts, GRAPH_LINES);
  else draw_grf_points(1, track_ax, pts, track_spf->axis, ungrf_y(track_spf->axis, 0.0), GRAPH_LINES);
  gdk_gc_set_foreground(track_ax->gc, ss->sgx->black);
}

static void track_amp_env_resize(GtkWidget *w)
{
  env *e;
  env *cur_env;
  if (!(track_p(track_dialog_id))) return;
  if (track_ax == NULL)
    {
      GdkWindow *wn;
      wn = MAIN_WINDOW(ss);
      track_cur_gc = gdk_gc_new(wn);
      gdk_gc_set_background(track_cur_gc, ss->sgx->graph_color);
      gdk_gc_set_foreground(track_cur_gc, ss->sgx->data_color);
      gdk_gc_set_function(track_cur_gc, GDK_COPY);

      track_ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      track_ax->wn = w_track_env->window;
      track_ax->w = w_track_env;
      track_ax->gc = track_cur_gc;
    }
  else clear_window(track_ax);
  e = track_dialog_env(track_dialog_id);
  track_spf->with_dots = true;
  env_editor_display_env(track_spf, e, track_ax, _("track env"), 0, 0, widget_width(w), widget_height(w), NOT_PRINTING);
  cur_env = track_dialog_track_amp_env(track_dialog_id);
  if (cur_env)
    {
      gdk_gc_set_foreground(track_ax->gc, ss->sgx->enved_waveform_color);
      track_spf->with_dots = false;
      env_editor_display_env(track_spf, cur_env, track_ax, _("track env"), 0, 0, widget_width(w), widget_height(w), NOT_PRINTING);
      gdk_gc_set_foreground(track_ax->gc, ss->sgx->black);
    }
  if (with_track_background_wave)
    display_track_waveform(track_dialog_id, track_spf->axis);
}

static gboolean track_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  env *e;
  if (!(track_p(track_dialog_id))) return(false);
  e = track_dialog_env(track_dialog_id);
  if (env_editor_button_press(track_spf, (int)(ev->x), (int)(ev->y), ev->time, e))
    track_amp_env_resize(w);
  return(false);
}

static gboolean track_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  env *e;
  if (!(track_p(track_dialog_id))) return(false);
  e = track_dialog_env(track_dialog_id);
  env_editor_button_release(track_spf, e);
  track_amp_env_resize(w);
  return(false);
}

static gboolean track_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  if (!(track_p(track_dialog_id))) return(false);
  if (ev->state & GDK_BUTTON1_MASK)
    {
      int x, y;
      GdkModifierType state;
      env *e;
      if (ev->is_hint)
	gdk_window_get_pointer(ev->window, &x, &y, &state);
      else
	{
	  x = (int)(ev->x);
	  y = (int)(ev->y);
	}
      e = track_dialog_env(track_dialog_id);
      env_editor_button_motion(track_spf, x, y, ev->time, e);
      track_amp_env_resize(w);
    }
  return(false);
}

static gboolean track_amp_env_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  if (!(track_p(track_dialog_id))) return(false);
  track_amp_env_resize(w);
  return(false);
}

static gboolean track_amp_env_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  if (!(track_p(track_dialog_id))) return(false);
  track_amp_env_resize(w);
  return(false);
}


/* -------- track -------- */
static GtkWidget *w_track_id = NULL, *w_track_beg = NULL, *w_track_id_label = NULL, *w_track_text;
static GtkWidget *w_track_track_label = NULL, *w_track_play_pix = NULL, *w_track_play = NULL, *w_track_track;
static GdkPixmap *track_speaker_pix;

static bool track_id_changed = false;

static GtkWidget *track_error_frame = NULL, *track_error_label = NULL;

static void clear_track_error(void)
{
  if ((track_error_frame) && (GTK_WIDGET_VISIBLE(track_error_frame)))
    gtk_widget_hide(track_error_frame);
}

static gint unpost_track_error(gpointer data)
{
  clear_track_error();
  return(0);
}

static void errors_to_track_text(const char *msg, void *data)
{
  set_label(track_error_label, msg);
  gtk_widget_show(track_error_frame);
  g_timeout_add_full(0, (guint32)5000, unpost_track_error, NULL, NULL);
}


static void track_id_activated(GtkWidget *w, gpointer context)
{
  char *val;
  track_id_changed = false;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track_id));
  if (val)
    {
      int id;
      id = track_name_to_id(val);
      if (id < 0)
	{
	  redirect_errors_to(errors_to_track_text, NULL);
	  id = string_to_int(val, 0, "track");
	  redirect_errors_to(NULL, NULL);
	}
      if (id >= 0)
	{
	  if (track_p(id))
	    {
	      track_dialog_id = id;
	      update_track_dialog(id);
	    }
	  else errors_to_track_text(_("no such track"), NULL);
	}
    }
}

static gboolean track_id_check_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (track_id_changed) track_id_activated(w_track_id, NULL);
  return(false);
}

static gboolean track_id_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  track_id_changed = true;
  return(false);
}


static void redisplay_track_bounds(void)
{
  chan_info *cp;
  cp = track_channel(track_dialog_id, 0); /* can be NULL */
  if (cp)
    {
      char lab[LABEL_BUFFER_SIZE];
      off_t beg, len;
      beg = track_position(track_dialog_id, -1);
      len = track_frames(track_dialog_id, -1);
      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
		   (float)((double)beg / (float)SND_SRATE(cp->sound)),
		   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
      gtk_entry_set_text(GTK_ENTRY(w_track_beg), lab);
    }
  else
    {
      gtk_entry_set_text(GTK_ENTRY(w_track_beg), _("no mixes in track"));
    }
}

static void track_beg_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track_beg));
  if (val)
    {
      chan_info *cp;
      cp = track_channel(track_dialog_id, 0);
      if (cp)
	{
	  Float beg;
	  char *up_to_colon;
	  up_to_colon = string_to_colon(val);
	  redirect_errors_to(errors_to_track_text, NULL);
	  beg = string_to_Float(up_to_colon, 0.0, "begin time");
	  redirect_errors_to(NULL, NULL);
	  FREE(up_to_colon);
	  if (beg >= 0.0)
	    {
	      set_track_position(track_dialog_id, (off_t)(beg * SND_SRATE(cp->sound)));
	      update_track_dialog(track_dialog_id);
	    }
	  else redisplay_track_bounds(); 
	}
      else 
	{
	  errors_to_track_text(_("no mixes in track, so begin time ignored"), NULL);
	  redisplay_track_bounds();
	}
    }
}

static bool track_track_changed = false;

static void track_track_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  track_track_changed = false;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track_track));
  if (val)
    {
      int id;
      id = track_name_to_id(val);
      if (id < 0)
	{
	  redirect_errors_to(errors_to_track_text, NULL);
	  id = string_to_int(val, 0, "track");
	  redirect_errors_to(NULL, NULL);
	}
      if (id >= 0)
	{
	  if ((id == track_dialog_id) ||
	      (!(set_track_track(track_dialog_id, id))))
	    {
	      errors_to_track_text(_("circular track chain"), NULL);
	      widget_track_to_text(w_track_track, track_dialog_track_track(track_dialog_id));
	    }
	  else update_track_dialog(id);
	}
      else widget_track_to_text(w_track_track, track_dialog_track_track(track_dialog_id));
    }
}

static gboolean track_track_check_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer unknown)
{
  if (track_track_changed) track_track_activated(w_track_track, NULL);
  return(false);
}

static gboolean track_track_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  track_track_changed = true;
  return(false);
}


/* -------- play -------- */
static bool track_playing = false;
bool track_play_stopped(void) {return(!track_playing);}

void reflect_track_play_stop(void)
{
  track_playing = false;
  if (w_track_play_pix)
    {
      track_speaker_pix = speaker_off_pix;
      gdk_draw_drawable(GDK_DRAWABLE(w_track_play_pix->window), ss->sgx->basic_gc, track_speaker_pix, 0, 0, 2, 4, 12, 12);
    }
}

static void track_dialog_play_callback(GtkWidget *w, gpointer context) 
{
  if (track_playing)
    {
      reflect_track_play_stop();
    }
  else
    {
      if (!(track_p(track_dialog_id))) return;
      track_playing = true;
      track_speaker_pix = speaker_on_pix;
      gdk_draw_drawable(GDK_DRAWABLE(w_track_play_pix->window), ss->sgx->basic_gc, track_speaker_pix, 0, 0, 2, 4, 12, 12);
      track_dialog_play(track_dialog_id);
    }
}

static gboolean w_track_play_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  gdk_draw_drawable(GDK_DRAWABLE(w_track_play_pix->window), ss->sgx->basic_gc, track_speaker_pix, 0, 0, 2, 4, 12, 12);
  return(false);
}


static void track_dB_callback(GtkWidget *w, gpointer context) 
{
  if (track_spf) track_spf->in_dB = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  track_amp_env_resize(w_track_env);
}

static void track_clip_callback(GtkWidget *w, gpointer context) 
{
  if (track_spf) track_spf->clip_p = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  track_amp_env_resize(w_track_env);
}

static void track_wave_callback(GtkWidget *w, gpointer context) 
{
  with_track_background_wave = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
  track_amp_env_resize(w_track_env);
}


static void apply_track_dialog(GtkWidget *w, gpointer context)
{
  env *e;
  if (!(track_p(track_dialog_id))) return;
  e = track_dialog_env(track_dialog_id);
  track_dialog_set_amp_env(track_dialog_id, e);
  track_amp_env_resize(w_track_env);
}

static void dismiss_track_dialog(GtkWidget *w, gpointer context)
{
  clear_track_error();
  gtk_widget_hide(track_dialog);
}

static gint delete_track_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  clear_track_error();
  gtk_widget_hide(track_dialog);
  return(true);
}

static GtkWidget *track_nextb, *track_previousb;

static void track_next_callback(GtkWidget *w, gpointer context)
{
  int id;
  clear_track_error();
  id = next_track_id(track_dialog_id);
  if (id != INVALID_TRACK_ID)
    {
      track_dialog_id = id;
      update_track_dialog(id);
      if (next_track_id(id) == INVALID_TRACK_ID) 
	set_sensitive(track_nextb, false);
    }
}

static void track_previous_callback(GtkWidget *w, gpointer context)
{
  int id;
  clear_track_error();
  id = previous_track_id(track_dialog_id);
  if (id != INVALID_TRACK_ID)
    {
      track_dialog_id = id;
      update_track_dialog(id);
      if (previous_track_id(id) == INVALID_TRACK_ID) 
	set_sensitive(track_previousb, false);
    }
}

static void track_dialog_help_callback(GtkWidget *w, gpointer context) 
{
  track_dialog_help();
}

GtkWidget *make_track_dialog(void)
{
  if (track_dialog == NULL)
    {
      GtkWidget *dismiss_button, *help_button, *rc, *apply_button, *track_frame, *t_frame, *rc_top, *rc1;
      GtkWidget *lo_hbox, *w_dB_frame, *w_dB, *w_clip, *w_wave, *w_dB_row;
      track_spf = new_env_editor();
      track_dialog_id = any_track_id();
      track_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(track_dialog, "delete_event", delete_track_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(track_dialog), _("Tracks"));
      sg_make_resizable(track_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(track_dialog), 6);
      gtk_widget_realize(track_dialog);
      
      dismiss_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismiss_button, "quit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), dismiss_button, false, true, 10);
      SG_SIGNAL_CONNECT(dismiss_button, "clicked", dismiss_track_dialog, NULL);
      gtk_widget_show(dismiss_button);

      track_previousb = gtk_button_new_from_stock(GTK_STOCK_GO_BACK);
      gtk_widget_set_name(track_previousb, "reset_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), track_previousb, false, true, 10);
      SG_SIGNAL_CONNECT(track_previousb, "clicked", track_previous_callback, NULL);
      gtk_widget_show(track_previousb);

      track_nextb = gtk_button_new_from_stock(GTK_STOCK_GO_FORWARD);
      gtk_widget_set_name(track_nextb, "doit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), track_nextb, false, true, 10);
      SG_SIGNAL_CONNECT(track_nextb, "clicked", track_next_callback, NULL);
      gtk_widget_show(track_nextb);

      apply_button = sg_button_new_from_stock_with_label(_("Apply Env"), GTK_STOCK_APPLY);
      gtk_widget_set_name(apply_button, "doit_again_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), apply_button, false, true, 10);
      SG_SIGNAL_CONNECT(apply_button, "clicked", apply_track_dialog, NULL);
      gtk_widget_show(apply_button);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), help_button, true, true, 10);
      SG_SIGNAL_CONNECT(help_button, "clicked", track_dialog_help_callback, NULL);
      gtk_widget_show(help_button);


      /* normally hidden error indication at top */
      track_error_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), track_error_frame, false, false, 4);

      track_error_label = gtk_label_new("");
      gtk_container_add(GTK_CONTAINER(track_error_frame), track_error_label);
      gtk_widget_show(track_error_label);


      rc_top = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), rc_top, false, false, 4);
      gtk_widget_show(rc_top);

      track_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(rc_top), track_frame, false, false, 4);
      gtk_widget_show(track_frame);

      t_frame = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(rc_top), t_frame, false, false, 4);
      gtk_widget_show(t_frame);

      rc = gtk_hbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(track_frame), rc);
      gtk_widget_show(rc);

      rc1 = gtk_hbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(t_frame), rc1);
      gtk_widget_show(rc1);

      w_track_id_label = gtk_label_new(_("track:"));
      gtk_box_pack_start(GTK_BOX(rc), w_track_id_label, false, false, 4);
      gtk_widget_show(w_track_id_label);

      w_track_id = snd_entry_new(rc, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(w_track_id, "activate", track_id_activated, NULL);
      SG_SIGNAL_CONNECT(w_track_id, "leave_notify_event", track_id_check_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_id, "key_press_event", track_id_modify_callback, NULL);

      w_track_beg = snd_entry_new(rc, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(w_track_beg, "activate", track_beg_activated, NULL);

      w_track_play = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(rc), w_track_play, false, false, 2);
      SG_SIGNAL_CONNECT(w_track_play, "clicked", track_dialog_play_callback, NULL);
      gtk_widget_show(w_track_play);
      gtk_widget_modify_bg(w_track_play, GTK_STATE_ACTIVE, ss->sgx->basic_color);
      gtk_widget_modify_bg(w_track_play, GTK_STATE_SELECTED, ss->sgx->basic_color);
      
      if (!speaker_off_pix)
	{
	  speaker_off_pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, speaker_bits());
	  speaker_on_pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, blue_speaker_bits());
	}
      track_speaker_pix = speaker_off_pix;

      w_track_play_pix = gtk_drawing_area_new();
      gtk_widget_set_events(w_track_play_pix, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(w_track_play_pix, 16, 16);
      gtk_container_add(GTK_CONTAINER(w_track_play), w_track_play_pix);
      gtk_widget_show(w_track_play_pix);
      SG_SIGNAL_CONNECT(w_track_play_pix, "expose_event", w_track_play_pix_expose, NULL);


      w_track_track_label = gtk_label_new(_("track:"));
      gtk_box_pack_start(GTK_BOX(rc1), w_track_track_label, false, false, 4);
      gtk_widget_show(w_track_track_label);

      w_track_track = snd_entry_new(rc1, WITH_DEFAULT_BACKGROUND);
      SG_SIGNAL_CONNECT(w_track_track, "activate", track_track_activated, NULL);
      SG_SIGNAL_CONNECT(w_track_track, "leave_notify_event", track_track_check_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_track, "key_press_event", track_track_modify_callback, NULL);
      
      w_track_text = gtk_label_new("");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), w_track_text, false, false, 4); 
      gtk_widget_show(w_track_text);
      

      /* SPEED */
      w_track_speed_form = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), w_track_speed_form, false, false, 4);
      
      w_track_speed_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_track_speed_form), w_track_speed_event, false, false, 4);
      gtk_widget_show(w_track_speed_event);
      SG_SIGNAL_CONNECT(w_track_speed_event, "button_press_event", track_speed_click_callback, NULL);
      
      w_track_speed_label = gtk_label_new(_("speed:"));
      gtk_container_add(GTK_CONTAINER(w_track_speed_event), w_track_speed_label);
      gtk_widget_show(w_track_speed_label);

      w_track_speed_label_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_track_speed_form), w_track_speed_label_event, false, false, 4);
      gtk_widget_show(w_track_speed_label_event);
      SG_SIGNAL_CONNECT(w_track_speed_label_event, "button_press_event", track_speed_label_click_callback, NULL);
      
      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO:    w_track_speed_number = gtk_label_new("1/1");  break;
	case SPEED_CONTROL_AS_SEMITONE: w_track_speed_number = gtk_label_new("1");    break;
	default:                        w_track_speed_number = gtk_label_new("1.00"); break;
	}
      gtk_container_add(GTK_CONTAINER(w_track_speed_label_event), w_track_speed_number);
      gtk_widget_show(w_track_speed_number);
      
      w_track_speed_adj = gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
      w_track_speed = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_track_speed_adj));
      gtk_box_pack_start(GTK_BOX(w_track_speed_form), w_track_speed, true, true, 4);
      SG_SIGNAL_CONNECT(w_track_speed, "button_release_event", track_speed_release_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_speed, "motion_notify_event", track_speed_motion_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_speed, "button_press_event", track_speed_press_callback, NULL);
      gtk_widget_show(w_track_speed);
      gtk_widget_show(w_track_speed_form);

      /* TEMPO */
      w_track_tempo_form = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), w_track_tempo_form, false, false, 0);
      
      w_track_tempo_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_track_tempo_form), w_track_tempo_event, false, false, 4);
      gtk_widget_show(w_track_tempo_event);
      SG_SIGNAL_CONNECT(w_track_tempo_event, "button_press_event", track_tempo_click_callback, NULL);
      
      w_track_tempo_label = gtk_label_new(_("tempo:"));
      gtk_container_add(GTK_CONTAINER(w_track_tempo_event), w_track_tempo_label);
      gtk_widget_show(w_track_tempo_label);
      
      w_track_tempo_number = gtk_label_new("1.00");
      gtk_box_pack_start(GTK_BOX(w_track_tempo_form), w_track_tempo_number, false, false, 0);
      gtk_widget_show(w_track_tempo_number);
	  
      w_track_tempo_adj = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      w_track_tempo = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_track_tempo_adj));
      gtk_box_pack_start(GTK_BOX(w_track_tempo_form), w_track_tempo, true, true, 4);
      SG_SIGNAL_CONNECT(w_track_tempo, "button_release_event", track_tempo_release_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_tempo, "motion_notify_event", track_tempo_motion_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_tempo, "button_press_event", track_tempo_press_callback, NULL);
      gtk_widget_show(w_track_tempo);
      gtk_widget_show(w_track_tempo_form);


      /* AMP */
      w_track_amp_form = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), w_track_amp_form, false, false, 0);
      
      w_track_amp_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_track_amp_form), w_track_amp_event, false, false, 4);
      gtk_widget_show(w_track_amp_event);
      SG_SIGNAL_CONNECT(w_track_amp_event, "button_press_event", track_amp_click_callback, NULL);
      
      w_track_amp_label = gtk_label_new(_("amp:"));
      gtk_container_add(GTK_CONTAINER(w_track_amp_event), w_track_amp_label);
      gtk_widget_show(w_track_amp_label);
      
      w_track_amp_number = gtk_label_new("1.00");
      gtk_box_pack_start(GTK_BOX(w_track_amp_form), w_track_amp_number, false, false, 0);
      gtk_widget_show(w_track_amp_number);
	  
      w_track_amp_adj = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      w_track_amp = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_track_amp_adj));
      gtk_box_pack_start(GTK_BOX(w_track_amp_form), w_track_amp, true, true, 4);
      SG_SIGNAL_CONNECT(w_track_amp, "button_release_event", track_amp_release_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_amp, "motion_notify_event", track_amp_motion_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_amp, "button_press_event", track_amp_press_callback, NULL);
      gtk_widget_show(w_track_amp);
      gtk_widget_show(w_track_amp_form);


      lo_hbox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), lo_hbox, true, true, 5);
      gtk_widget_show(lo_hbox);

      /* GRAPH (frame) */
      w_track_env_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(lo_hbox), w_track_env_frame, true, true, 10);

      /* GRAPH (buttons) */
      w_dB_frame = gtk_frame_new(NULL);
      gtk_box_pack_end(GTK_BOX(lo_hbox), w_dB_frame, false, false, 2);
      gtk_widget_show(w_dB_frame);

      w_dB_row = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(w_dB_frame), w_dB_row);
      gtk_widget_show(w_dB_row);	

      w_clip = gtk_check_button_new_with_label(_("clip"));
      SG_SIGNAL_CONNECT(w_clip, "toggled", track_clip_callback, NULL);
      gtk_box_pack_start(GTK_BOX(w_dB_row), w_clip, false, false, 0);
      gtk_widget_show(w_clip);

      w_wave = gtk_check_button_new_with_label(_("wave"));
      SG_SIGNAL_CONNECT(w_wave, "toggled", track_wave_callback, NULL);
      gtk_box_pack_start(GTK_BOX(w_dB_row), w_wave, false, false, 0);
      gtk_widget_show(w_wave);

      w_dB = gtk_check_button_new_with_label(_("dB"));
      SG_SIGNAL_CONNECT(w_dB, "toggled", track_dB_callback, NULL);
      gtk_box_pack_start(GTK_BOX(w_dB_row), w_dB, false, false, 0);
      gtk_widget_show(w_dB);

      w_track_env = gtk_drawing_area_new();
      gtk_widget_set_events(w_track_env, GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(w_track_env_frame), w_track_env);
      gtk_widget_modify_bg(w_track_env, GTK_STATE_NORMAL, ss->sgx->highlight_color);
      gtk_widget_show(w_track_env);
      SG_SIGNAL_CONNECT(w_track_env, "expose_event", track_amp_env_expose_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_env, "configure_event", track_amp_env_resize_callback, NULL);
      SG_SIGNAL_CONNECT(w_track_env, "button_press_event", track_drawer_button_press, NULL);
      SG_SIGNAL_CONNECT(w_track_env, "button_release_event", track_drawer_button_release, NULL);
      SG_SIGNAL_CONNECT(w_track_env, "motion_notify_event", track_drawer_button_motion, NULL);
      gtk_widget_show(w_track_env_frame);

      gtk_widget_show(track_dialog);
      set_dialog_widget(TRACK_DIALOG, track_dialog);
      if ((widget_width(track_dialog) > 0) && (widget_height(track_dialog) < 300))
	set_widget_size(track_dialog, widget_width(track_dialog), 300);
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w_clip), true);
      gtk_widget_hide(track_error_frame);
    }
  else 
    {
      raise_dialog(track_dialog);
      reflect_edit_in_track_dialog_env(track_dialog_id);
    }
  update_track_dialog(track_dialog_id);
  return(track_dialog);
}

static void update_track_dialog(int track_id) 
{
  if (!(track_p(track_dialog_id))) track_dialog_id = track_id;
  if (!(track_p(track_dialog_id))) track_dialog_id = any_track_id();
  if (track_id == track_dialog_id)
    {
      if (track_dialog == NULL) 
	make_track_dialog();
      else
	{
	  set_sensitive(track_nextb, (next_track_id(track_id) != INVALID_TRACK_ID));
	  set_sensitive(track_previousb, (previous_track_id(track_id) != INVALID_TRACK_ID));
	}
      
      if (track_p(track_dialog_id))
	{
	  chan_info *cp;
	  off_t beg, len;
	  Float val;
	  char *temp = NULL;
	  char lab[LABEL_BUFFER_SIZE];
	  cp = track_channel(track_id, 0);
	  val = track_dialog_track_speed(track_id);
	  reflect_track_speed(val);
	  widget_track_to_text(w_track_track, track_dialog_track_track(track_id));
	  widget_track_to_text(w_track_id, track_id);
	  if (cp)
	    {
	      beg = track_position(track_id, -1);
	      len = track_frames(track_id, -1);
	      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
			   (float)((double)beg / (float)SND_SRATE(cp->sound)),
			   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
	      gtk_entry_set_text(GTK_ENTRY(w_track_beg), lab);
	    }
	  val = track_dialog_track_tempo(track_id);
	  mus_snprintf(lab, 5, "%.2f", val);
	  gtk_label_set_text(GTK_LABEL(w_track_tempo_number), lab);
	  GTK_ADJUSTMENT(w_track_tempo_adj)->value = tempo_to_scrollbar(tempo_control_min(ss), val, tempo_control_max(ss));
	  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_track_tempo_adj));
	  val = track_dialog_track_amp(track_id);
	  reflect_track_amp(val);
	  track_amp_env_resize(w_track_env);
	  set_sensitive(apply_button, true);
	  set_label(w_track_text, temp = track_dialog_track_info(track_dialog_id));
	  if (temp) FREE(temp);
	}
      else
	{
	  gtk_entry_set_text(GTK_ENTRY(w_track_id), "0");
	  gtk_entry_set_text(GTK_ENTRY(w_track_track), "0");
	  gtk_entry_set_text(GTK_ENTRY(w_track_beg), _("no active track"));
	  set_sensitive(apply_button, false);
	}
    }
}



/* ---------------- reflection ---------------- */

void reflect_mix_or_track_change(int mix_id, int track_id, bool forced)
{
  if ((mix_dialog) && 
      (GTK_WIDGET_VISIBLE(mix_dialog)))
    {
      if ((forced) && (mix_ok_and_unlocked(mix_id))) mix_dialog_id = mix_id;
      update_mix_dialog(mix_id);
      if (mix_id != INVALID_MIX_ID)
	{
	  set_sensitive(nextb, (next_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(mix_dialog_id) != INVALID_MIX_ID));
	}
    }
  if ((track_dialog) && 
      (GTK_WIDGET_VISIBLE(track_dialog)))
    {
      if ((forced) && (track_p(track_id))) track_dialog_id = track_id;
      update_track_dialog(track_id);
      if (track_id != INVALID_TRACK_ID)
	{
	  set_sensitive(track_nextb, (next_track_id(track_dialog_id) != INVALID_TRACK_ID));
	  set_sensitive(track_previousb, (previous_track_id(track_dialog_id) != INVALID_TRACK_ID));
	}
    }
}

int mix_dialog_mix(void) {return(mix_dialog_id);}
void mix_dialog_set_mix(int id) {mix_dialog_id = id; update_mix_dialog(mix_dialog_id);}
int track_dialog_track(void) {return(track_dialog_id);}
void track_dialog_set_track(int id) {track_dialog_id = id; update_track_dialog(track_dialog_id);}


