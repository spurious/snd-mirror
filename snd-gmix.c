#include "snd.h"

static GtkWidget *mix_panel = NULL;
static bool dragging = false;
static void update_mix_panel(int mix_id);

/* ---------------- SPEED ---------------- */

static GtkWidget *w_speed, *w_speed_label, *w_speed_number, *w_speed_form, *w_speed_event;
static GtkObject *w_speed_adj;
static char speed_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};
static Float current_speed = 1.0;

static void change_mix_speed(int mix_id, Float val)
{
  chan_info *cp;
  cp = mix_channel_from_id(mix_id);
  set_mix_speed_from_id(mix_id,
			srate_changed(val,
				      speed_number_buffer,
				      cp->sound->speed_control_style,
				      cp->sound->speed_control_tones),
			dragging);
  gtk_label_set_text(GTK_LABEL(w_speed_number), speed_number_buffer);
}

static gboolean speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  change_mix_speed(current_mix_id(), 1.0);
  GTK_ADJUSTMENT(w_speed_adj)->value = .45;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_speed_adj));
  return(false);
}

static void speed_changed_callback(GtkAdjustment *adj, gpointer data)
{
  Float val;
  int mix_id;
  chan_info *cp;
  snd_info *sp;
  mix_id = current_mix_id();
  cp = mix_channel_from_id(mix_id);
  sp = cp->sound;
  val = srate_changed(exp((GTK_ADJUSTMENT(w_speed_adj)->value - .45) / .15),
		      speed_number_buffer,
		      sp->speed_control_style,
		      sp->speed_control_tones);
  if (!dragging) start_mix_drag(mix_id);
  dragging = true;
  change_mix_speed(mix_id, val);
  gtk_label_set_text(GTK_LABEL(w_speed_number), speed_number_buffer);
}

static gboolean speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  Float val;
  int mix_id;
  chan_info *cp;
  snd_info *sp;
  mix_id = current_mix_id();
  cp = mix_channel_from_id(mix_id);
  sp = cp->sound;
  val = srate_changed(exp((GTK_ADJUSTMENT(w_speed_adj)->value - .45) / .15),
		      speed_number_buffer,
		      sp->speed_control_style,
		      sp->speed_control_tones);
  dragging = false;
  change_mix_speed(mix_id, val);
  return(false);
}

static void reflect_mix_speed(Float uval, snd_info *sp)
{
  Float val;
  val = srate_changed(uval,
		      speed_number_buffer,
		      sp->speed_control_style,
		      sp->speed_control_tones);
  gtk_label_set_text(GTK_LABEL(w_speed_number), speed_number_buffer);
  if (val > 0.0)
    GTK_ADJUSTMENT(w_speed_adj)->value = .45 + .15 * log(val);
  else GTK_ADJUSTMENT(w_speed_adj)->value = 0.0;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_speed_adj));
}

/* ---------------- AMP ---------------- */

static GtkWidget **w_amps, **w_amp_labels, **w_amp_numbers, **w_amp_events, **w_amp_forms;
static GtkObject **w_amp_adjs;
static Float *current_amps;
#define CHANS_ALLOCATED 8
static char amp_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

static void change_mix_amp(int mix_id, int chan, Float val)
{
  char *sfs;
  chan_info *cp;
  cp = mix_channel_from_id(mix_id);
  set_mix_amp_from_id(mix_id, chan, val, dragging);
  sfs = prettyf(val, 2);
  fill_number(sfs, amp_number_buffer);
  gtk_label_set_text(GTK_LABEL(w_amp_numbers[chan]), amp_number_buffer);
  FREE(sfs);
}

static gboolean amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chan;
  chan = get_user_int_data(G_OBJECT(w));
  change_mix_amp(current_mix_id(), chan, 1.0);
  GTK_ADJUSTMENT(w_amp_adjs[chan])->value = 0.5;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_amp_adjs[chan]));
  return(false);
}

static Float amp_to_scroll(Float amp)
{
  if (amp <= 0.0)
    return(0.0);
  else
    {
      if (amp < .173)
	return(amp * .867);
      else return(log(amp) * 0.2 + 0.5);
    }
}

static Float scroll_to_amp(Float scrollval)
{
  if (scrollval < .15)
    return(scrollval * 1.13);
  else return(exp((scrollval - 0.5) * 5.0));
}

static void amp_changed_callback(GtkAdjustment *adj, gpointer data)
{
  int chan;
  Float scrollval;
  chan = get_user_int_data(G_OBJECT(adj));
  scrollval = GTK_ADJUSTMENT(w_amp_adjs[chan])->value;
  if (!dragging) start_mix_drag(current_mix_id());
  dragging = true;
  change_mix_amp(current_mix_id(), chan, scroll_to_amp(scrollval));
}

static gboolean amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chan;
  Float scrollval;
  chan = get_user_int_data(G_OBJECT(w));
  scrollval = GTK_ADJUSTMENT(w_amp_adjs[chan])->value;
  dragging = false;
  change_mix_amp(current_mix_id(), chan, scroll_to_amp(scrollval));
  return(false);
}


/* ---------------- AMP ENV ---------------- */

static GtkWidget *w_env_frame, *w_env;
static axis_context *ax = NULL;
static GdkGC *cur_gc;
static void *spfs[8];
static int last_clicked_env_chan;

static void mix_amp_env_resize(GtkWidget *w)
{
  GdkWindow *wn;
  int chans, chan, mix_id;
  env **e;
  env *cur_env;
  if (ax == NULL)
    {
      wn = MAIN_WINDOW(ss);
      cur_gc = gdk_gc_new(wn);
      gdk_gc_set_background(cur_gc, (ss->sgx)->graph_color);
      gdk_gc_set_foreground(cur_gc, (ss->sgx)->data_color);
      gdk_gc_set_function(cur_gc, GDK_COPY);

      ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      ax->wn = w_env->window;
      ax->w = w_env;
      ax->gc = cur_gc;
    }
  else clear_window(ax);
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  e = mix_panel_envs(mix_id);
  for (chan = 0; chan < chans; chan++)
    {
      edp_display_graph(spfs[chan], _("mix env"), ax, 
			(int)(chan * widget_width(w) / chans), 0,
			widget_width(w) / chans, widget_height(w), 
			e[chan], false, true);
      cur_env = mix_amp_env_from_id(mix_id, chan);
      if (cur_env)
	{
	  gdk_gc_set_foreground(ax->gc, (ss->sgx)->enved_waveform_color);
	  edp_display_graph(spfs[chan], _("mix env"), ax, 
			    (int)(chan * widget_width(w) / chans), 0,
			    widget_width(w) / chans, widget_height(w), 
			    cur_env, false, false);
	  gdk_gc_set_foreground(ax->gc, (ss->sgx)->black);
	}
    }
}

static gboolean mix_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int mix_id, chans, chan;
  env *e;
  Float pos;
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_panel_env(mix_id, chan);
  if (edp_handle_press(
		       spfs[chan],
		       (int)(ev->x), (int)(ev->y), ev->time, 
		       e,
		       false,
		       1.0))
    mix_amp_env_resize(w);
  return(false);
}

static gboolean mix_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int mix_id, chans, chan;
  env *e;
  Float pos;
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_panel_env(mix_id, chan);
  edp_handle_release(spfs[chan], e);
  mix_amp_env_resize(w);
  return(false);
}

static gboolean mix_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  int mix_id, chans, chan, x, y;
  GdkModifierType state;
  env *e;
  Float pos;
  if (ev->state & GDK_BUTTON1_MASK)
    {
      if (ev->is_hint)
	gdk_window_get_pointer(ev->window, &x, &y, &state);
      else
	{
	  x = (int)(ev->x);
	  y = (int)(ev->y);
	}
      mix_id = current_mix_id();
      chans = mix_input_chans_from_id(mix_id);
      pos = (Float)x / (Float)widget_width(w);
      chan = (int)(pos * chans);
      last_clicked_env_chan = chan;
      e = mix_panel_env(mix_id, chan);
      edp_handle_point(
		       spfs[chan],
		       x, y, ev->time, 
		       e,
		       false,
		       1.0);
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
  mix_amp_env_resize(w);
  return(false);
}



/* ---------------- MIX PANEL ---------------- */

static GtkWidget *w_id = NULL, *w_beg = NULL, *w_track = NULL, *mix_play = NULL, *w_id_label = NULL;
static GtkWidget *w_track_label = NULL, *track_play_pix = NULL, *mix_play_pix = NULL, *track_play = NULL;
static GdkPixmap *speaker_off_pix, *speaker_on_pix, *mix_speaker_pix, *track_speaker_pix;

static void id_activated(GtkWidget *w, gpointer context)
{
  char *val;
  int id;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_id));
  if (val)
    {
      id = string2int(val);
      if (mix_ok(id))
	{
 	  select_mix_from_id(id);
	  update_mix_panel(ss->selected_mix);
	}
    }
}

static void beg_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  chan_info *cp;
  int mix_id;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_beg));
  if (val)
    {
      mix_id = current_mix_id();
      cp = mix_channel_from_id(mix_id);
      set_mix_position(mix_id, (off_t)(string2Float(val) * SND_SRATE(cp->sound)));
      update_mix_panel(mix_id);
    }
}

static void track_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track));
  if (val)
    set_mix_track_from_id(current_mix_id(), string2int(val));
}

static bool mix_playing = false;
bool mix_play_stopped(void) {return(!mix_playing);}

void reflect_mix_play_stop(void)
{
  mix_playing = false;
  mix_speaker_pix = speaker_off_pix;
  track_speaker_pix = speaker_off_pix;
  gdk_draw_drawable(GDK_DRAWABLE(mix_play_pix->window), ss->sgx->basic_gc, mix_speaker_pix, 0, 0, 2, 4, 12, 12);
  gdk_draw_drawable(GDK_DRAWABLE(track_play_pix->window), ss->sgx->basic_gc, track_speaker_pix, 0, 0, 2, 4, 12, 12);
}

static void mix_play_callback(GtkWidget *w, gpointer context) 
{
  if (mix_playing)
    {
      reflect_mix_play_stop();
    }
  else
    {
      mix_playing = true;
      mix_speaker_pix = speaker_on_pix;
      gdk_draw_drawable(GDK_DRAWABLE(mix_play_pix->window), ss->sgx->basic_gc, mix_speaker_pix, 0, 0, 2, 4, 12, 12);
      mix_play_from_id(current_mix_id());
    }
}

static void track_play_callback(GtkWidget *w, gpointer context) 
{
  if (mix_playing)
    {
      reflect_mix_play_stop();
    }
  else
    {
      mix_playing = true;
      track_speaker_pix = speaker_on_pix;
      gdk_draw_drawable(GDK_DRAWABLE(track_play_pix->window), ss->sgx->basic_gc, track_speaker_pix, 0, 0, 2, 4, 12, 12);
      track_play_from_id(current_mix_id());
    }
}

static gboolean mix_play_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  gdk_draw_drawable(GDK_DRAWABLE(mix_play_pix->window), ss->sgx->basic_gc, mix_speaker_pix, 0, 0, 2, 4, 12, 12);
  return(false);
}

static gboolean track_play_pix_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  gdk_draw_drawable(GDK_DRAWABLE(track_play_pix->window), ss->sgx->basic_gc, track_speaker_pix, 0, 0, 2, 4, 12, 12);
  return(false);
}

static void apply_mix_panel(GtkWidget *w, gpointer context)
{
  /* set all mix amp envs, last one should remix */
  int i, chans, mix_id;
  env **envs;
  mix_id = current_mix_id();
  chans = mix_input_chans_from_id(mix_id);
  envs = mix_panel_envs(mix_id);
  for (i = 0; i < chans; i++)
    if (i != last_clicked_env_chan)
      set_mix_amp_env_without_edit(mix_id, i, envs[i]);
  set_mix_amp_env_from_gui(mix_id, last_clicked_env_chan, envs[last_clicked_env_chan]);
  mix_amp_env_resize(w_env);
}

static void dismiss_mix_panel(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(mix_panel);
}

static void delete_mix_panel(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(mix_panel);
}

static GtkWidget *nextb, *previousb;

static void mix_next_callback(GtkWidget *w, gpointer context)
{
  int id;
  id = next_mix_id(current_mix_id());
  if (id != INVALID_MIX_ID)
    {
      select_mix_from_id(id);
      update_mix_panel(ss->selected_mix);
      if (next_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(nextb, false);
    }
}

static void mix_previous_callback(GtkWidget *w, gpointer context)
{
  int id;
  id = previous_mix_id(current_mix_id());
  if (id != INVALID_MIX_ID)
    {
      select_mix_from_id(id);
      update_mix_panel(ss->selected_mix);
      if (previous_mix_id(id) == INVALID_MIX_ID) 
	set_sensitive(previousb, false);
    }
}

static void mix_panel_help(GtkWidget *w, gpointer context) 
{
  snd_help(_("Mix Panel"),
"This dialog provides various commonly-used controls on the currently \
selected mix.  At the top are the mix id, begin and end times, \
track number, and a play button.  Beneath that are various sliders \
controlling the speed (sampling rate) of the mix, and the amplitude of each \
input channel; and finally, an envelope editor for the mix's (input) channels. \
The current mix amp env is not actually changed until you click 'Apply Env'.\
The editor envelope is drawn in black with dots whereas the current \
mix amp env (if any) is drawn in blue.",
	   true);
}

GtkWidget *make_mix_panel(void)
{
  GtkWidget *dismiss_button, *help_button, *rc, *apply_button, *mix_frame, *track_frame, *rc_top, *rc1;
  char amplab[LABEL_BUFFER_SIZE];
  int mix_id, i;
  mix_id = current_mix_id();
  if (mix_panel == NULL)
    {
      mix_panel = snd_gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(mix_panel),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(mix_panel))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(delete_mix_panel), NULL, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(mix_panel), _("Mix Panel"));
      sg_make_resizable(mix_panel);
      gtk_container_set_border_width (GTK_CONTAINER(mix_panel), 6);
      gtk_widget_realize(mix_panel);
      
      dismiss_button = gtk_button_new_with_label(_("Dismiss"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->action_area), dismiss_button, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(dismiss_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(dismiss_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_mix_panel), NULL, 0),
				     0);
      gtk_widget_show(dismiss_button);

      previousb = gtk_button_new_with_label(_("Previous"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->action_area), previousb, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(previousb),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(previousb))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_previous_callback), NULL, 0),
				     0);
      gtk_widget_show(previousb);

      nextb = gtk_button_new_with_label(_("Next"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->action_area), nextb, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(nextb),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(nextb))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_next_callback), NULL, 0),
				     0);
      gtk_widget_show(nextb);

      apply_button = gtk_button_new_with_label(_("Apply Env"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->action_area), apply_button, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(apply_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(apply_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(apply_mix_panel), NULL, 0),
				     0);
      gtk_widget_show(apply_button);

      help_button = gtk_button_new_with_label(_("Help"));
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(mix_panel)->action_area), help_button, true, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_panel_help), NULL, 0),
				     0);
      gtk_widget_show(help_button);


      /* top row of mix id name position track etc */

      rc_top = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->vbox), rc_top, false, false, 4);
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

      w_id = snd_entry_new(rc, false);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_id),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(w_id))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(id_activated), NULL, 0),
				     0);

      w_beg = snd_entry_new(rc, false);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_beg),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(w_beg))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(beg_activated), NULL, 0),
				     0);

      mix_play = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(rc), mix_play, false, false, 2);
      g_signal_connect_closure_by_id(GTK_OBJECT(mix_play),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(mix_play))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_play_callback), NULL, 0),
				     0);
      gtk_widget_show(mix_play);
      gtk_widget_modify_bg(mix_play, GTK_STATE_ACTIVE, ss->sgx->basic_color);
      gtk_widget_modify_bg(mix_play, GTK_STATE_SELECTED, ss->sgx->basic_color);
      
      speaker_off_pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, speaker_bits());
      speaker_on_pix = gdk_pixmap_create_from_xpm_d(MAIN_WINDOW(ss), NULL, NULL, blue_speaker_bits());
      mix_speaker_pix = speaker_off_pix;
      track_speaker_pix = speaker_off_pix;
      mix_play_pix = gtk_drawing_area_new();
      gtk_widget_set_events(mix_play_pix, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(mix_play_pix, 16, 16);
      gtk_container_add(GTK_CONTAINER(mix_play), mix_play_pix);
      gtk_widget_show(mix_play_pix);
      g_signal_connect_closure_by_id(GTK_OBJECT(mix_play_pix),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(mix_play_pix))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_play_pix_expose), NULL, 0),
				     0);


      w_track_label = gtk_label_new(_("track:"));
      gtk_box_pack_start(GTK_BOX(rc1), w_track_label, false, false, 4);
      gtk_widget_show(w_track_label);

      w_track = snd_entry_new(rc1, false);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(w_track))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_activated), NULL, 0),
				     0);

      track_play = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(rc1), track_play, false, false, 2);
      g_signal_connect_closure_by_id(GTK_OBJECT(track_play),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(track_play))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_play_callback), NULL, 0),
				     0);
      gtk_widget_show(track_play);
      gtk_widget_modify_bg(track_play, GTK_STATE_ACTIVE, ss->sgx->basic_color);
      gtk_widget_modify_bg(track_play, GTK_STATE_SELECTED, ss->sgx->basic_color);
      
      track_play_pix = gtk_drawing_area_new();
      gtk_widget_set_events(track_play_pix, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(track_play_pix, 16, 16);
      gtk_container_add(GTK_CONTAINER(track_play), track_play_pix);
      gtk_widget_show(track_play_pix);
      g_signal_connect_closure_by_id(GTK_OBJECT(track_play_pix),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(track_play_pix))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_play_pix_expose), NULL, 0),
				     0);

      /* SPEED */
      w_speed_form = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->vbox), w_speed_form, false, false, 4);
      
      w_speed_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed_event, false, false, 4);
      gtk_widget_show(w_speed_event);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_speed_event),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(w_speed_event))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(speed_click_callback), NULL, 0),
				     0);
      
      w_speed_label = gtk_label_new(_("speed:"));
      gtk_container_add(GTK_CONTAINER(w_speed_event), w_speed_label);
      gtk_widget_show(w_speed_label);

      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO: w_speed_number = gtk_label_new("1/1"); break;
	case SPEED_CONTROL_AS_SEMITONE: w_speed_number = gtk_label_new("1"); break;
	default:  w_speed_number = gtk_label_new(speed_number_buffer); break;
	}
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed_number, false, false, 0);
      gtk_widget_show(w_speed_number);
      
      w_speed_adj = gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
      w_speed = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_speed_adj));
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed, true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_speed_adj),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(w_speed_adj))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(speed_changed_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_speed),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(w_speed))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(speed_release_callback), NULL, 0),
				     0);
      gtk_widget_show(w_speed);
      gtk_widget_show(w_speed_form);


      /* AMP */
      w_amp_numbers = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amp_labels = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amps = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amp_events = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amp_forms = (GtkWidget **)CALLOC(CHANS_ALLOCATED, sizeof(GtkWidget *));
      w_amp_adjs = (GtkObject **)CALLOC(CHANS_ALLOCATED, sizeof(GtkObject *));
      current_amps = (Float *)CALLOC(CHANS_ALLOCATED, sizeof(Float));

      for (i = 0; i < CHANS_ALLOCATED; i++)
	{
	  w_amp_forms[i] = gtk_hbox_new(false, 2);
	  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->vbox), w_amp_forms[i], false, false, 0);
      
	  w_amp_events[i] = gtk_event_box_new();
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amp_events[i], false, false, 4);
	  gtk_widget_show(w_amp_events[i]);
	  g_signal_connect_closure_by_id(GTK_OBJECT(w_amp_events[i]),
					 g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(w_amp_events[i]))),
					 0,
					 g_cclosure_new(GTK_SIGNAL_FUNC(amp_click_callback), NULL, 0),
					 0);
      
	  mus_snprintf(amplab, LABEL_BUFFER_SIZE, _("amp %d:"), i);
	  w_amp_labels[i] = gtk_label_new(amplab);
	  gtk_container_add(GTK_CONTAINER(w_amp_events[i]), w_amp_labels[i]);
	  set_user_int_data(G_OBJECT(w_amp_events[i]), i);
	  gtk_widget_show(w_amp_labels[i]);
      
	  w_amp_numbers[i] = gtk_label_new(amp_number_buffer);
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amp_numbers[i], false, false, 0);
	  gtk_widget_show(w_amp_numbers[i]);
	  
	  w_amp_adjs[i] = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
	  w_amps[i] = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_amp_adjs[i]));
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amps[i], true, true, 4);
	  g_signal_connect_closure_by_id(GTK_OBJECT(w_amp_adjs[i]),
					 g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(w_amp_adjs[i]))),
					 0,
					 g_cclosure_new(GTK_SIGNAL_FUNC(amp_changed_callback), NULL, 0),
					 0);
	  g_signal_connect_closure_by_id(GTK_OBJECT(w_amps[i]),
					 g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(w_amps[i]))),
					 0,
					 g_cclosure_new(GTK_SIGNAL_FUNC(amp_release_callback), NULL, 0),
					 0);
	  set_user_int_data(G_OBJECT(w_amps[i]), i);	  
	  set_user_int_data(G_OBJECT(w_amp_adjs[i]), i);
	  gtk_widget_show(w_amps[i]);
      
	  gtk_widget_show(w_amp_forms[i]);
	}

      /* GRAPH */
      w_env_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->vbox), w_env_frame, true, true, 10);
      
      w_env = gtk_drawing_area_new();
      gtk_widget_set_events(w_env, GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(w_env_frame), w_env);
      gtk_widget_show(w_env);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_env),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(w_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_amp_env_expose_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_env),
				     g_signal_lookup("configure_event", G_OBJECT_TYPE(GTK_OBJECT(w_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_amp_env_resize_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_env),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(w_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_drawer_button_press), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_env),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(w_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_drawer_button_release), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_env),
				     g_signal_lookup("motion_notify_event", G_OBJECT_TYPE(GTK_OBJECT(w_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_drawer_button_motion), NULL, 0),
				     0);
      gtk_widget_show(w_env_frame);

      gtk_widget_show(mix_panel);
      set_dialog_widget(MIX_PANEL_DIALOG, mix_panel);

      for (i = 0; i < CHANS_ALLOCATED; i++) spfs[i] = new_env_editor();
      speed_number_buffer[1] = local_decimal_point();
      amp_number_buffer[1] = local_decimal_point();
    }
  else raise_dialog(mix_panel);

  update_mix_panel(current_mix_id());
  return(mix_panel);
}

static void update_mix_panel(int mix_id) 
{
  chan_info *cp;
  int i, chans;
  off_t beg, len;
  Float val;
  char lab[LABEL_BUFFER_SIZE];
  if (mix_id == INVALID_MIX_ID) return;
  if (mix_id == current_mix_id())
    {
      if (mix_panel == NULL) 
	make_mix_panel();
      else
	{
	  set_sensitive(nextb, (next_mix_id(mix_id) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(mix_id) != INVALID_MIX_ID));
	}
      
      /* now reflect current mix state in mix panel controls */
      cp = mix_channel_from_id(mix_id);

      val = mix_speed_from_id(mix_id);
      if (val != current_speed)
	{
	  reflect_mix_speed(val, cp->sound);
	  current_speed = val;
	}

      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%d", mix_track_from_id(mix_id));
      gtk_entry_set_text(GTK_ENTRY(w_track), lab);

      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%d", mix_id);
      gtk_entry_set_text(GTK_ENTRY(w_id), lab);

      beg = mix_position_from_id(mix_id);
      len = mix_frames(mix_id);
      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
		   (float)((double)beg / (float)SND_SRATE(cp->sound)),
		   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
      gtk_entry_set_text(GTK_ENTRY(w_beg), lab);

      chans = mix_input_chans_from_id(mix_id);
      if (chans > 8) chans = 8;

      set_label(w_amp_labels[0], (chans == 1) ? "amp:" : "amp 0:");
      for (i = 0; i < chans; i++)
	{
	  gtk_widget_show(w_amp_labels[i]);	  
	  gtk_widget_show(w_amp_numbers[i]);	  
	  gtk_widget_show(w_amps[i]);
	  gtk_widget_show(w_amp_forms[i]);
	  val = mix_amp_from_id(mix_id, i);
	  GTK_ADJUSTMENT(w_amp_adjs[i])->value = amp_to_scroll(val);
	  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_amp_adjs[i]));
	  current_amps[i] = val;
	}

      for (i = chans; i < CHANS_ALLOCATED; i++)
	{
	  gtk_widget_hide(w_amp_labels[i]);	  
	  gtk_widget_hide(w_amp_numbers[i]);	  
	  gtk_widget_hide(w_amps[i]);
	  gtk_widget_hide(w_amp_forms[i]);
	}

      mix_amp_env_resize(w_env);

    }
}

void reflect_mix_in_mix_panel(int mix_id)
{
  if ((mix_panel) &&
      (GTK_WIDGET_VISIBLE(mix_panel)))
    {
      if (current_mix_id() == mix_id)
	update_mix_panel(mix_id);
      if (mix_id != INVALID_MIX_ID)
	{
	  set_sensitive(nextb, (next_mix_id(current_mix_id()) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(current_mix_id()) != INVALID_MIX_ID));
	}
    }
}

void reflect_no_mix_in_mix_panel(void)
{
  if ((mix_panel) &&
      (GTK_WIDGET_VISIBLE(mix_panel)))
    gtk_widget_hide(mix_panel);
}
