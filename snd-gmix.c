#include "snd.h"

/* ---------------- mix dialog ---------------- */

static GtkWidget *mix_dialog = NULL;
static bool mix_dialog_slider_dragging = false;
static void update_mix_dialog(int mix_id);
static int mix_dialog_id = INVALID_MIX_ID;

static GtkWidget *w_speed, *w_speed_label, *w_speed_number, *w_speed_form, *w_speed_event;
static GtkObject *w_speed_adj;
static char speed_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};
static Float current_speed = 1.0;

static void change_mix_speed(int mix_id, Float val)
{
  chan_info *cp;
  cp = mix_dialog_mix_channel(mix_id);
  mix_dialog_set_mix_speed(mix_id,
			   srate_changed(val,
					 speed_number_buffer,
					 cp->sound->speed_control_style,
					 cp->sound->speed_control_tones),
			   mix_dialog_slider_dragging);
  gtk_label_set_text(GTK_LABEL(w_speed_number), speed_number_buffer);
}

static gboolean speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  change_mix_speed(mix_dialog_id, 1.0);
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
  mix_id = mix_dialog_id;
  cp = mix_dialog_mix_channel(mix_id);
  sp = cp->sound;
  val = srate_changed(exp((GTK_ADJUSTMENT(w_speed_adj)->value - .45) / .15),
		      speed_number_buffer,
		      sp->speed_control_style,
		      sp->speed_control_tones);
  if (!mix_dialog_slider_dragging) mix_dialog_start_drag(mix_id);
  mix_dialog_slider_dragging = true;
  change_mix_speed(mix_id, val);
  gtk_label_set_text(GTK_LABEL(w_speed_number), speed_number_buffer);
}

static gboolean speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  Float val;
  int mix_id;
  chan_info *cp;
  snd_info *sp;
  mix_id = mix_dialog_id;
  cp = mix_dialog_mix_channel(mix_id);
  sp = cp->sound;
  val = srate_changed(exp((GTK_ADJUSTMENT(w_speed_adj)->value - .45) / .15),
		      speed_number_buffer,
		      sp->speed_control_style,
		      sp->speed_control_tones);
  mix_dialog_slider_dragging = false;
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

static GtkWidget **w_amps, **w_amp_labels, **w_amp_numbers, **w_amp_events, **w_amp_forms;
static GtkObject **w_amp_adjs;
static Float *current_amps;
#define CHANS_ALLOCATED 8
static char amp_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

static void change_mix_amp(int mix_id, int chan, Float val)
{
  char *sfs;
  chan_info *cp;
  cp = mix_dialog_mix_channel(mix_id);
  mix_dialog_set_mix_amp(mix_id, chan, val, mix_dialog_slider_dragging);
  sfs = prettyf(val, 2);
  fill_number(sfs, amp_number_buffer);
  gtk_label_set_text(GTK_LABEL(w_amp_numbers[chan]), amp_number_buffer);
  FREE(sfs);
}

static gboolean amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chan;
  chan = get_user_int_data(G_OBJECT(w));
  change_mix_amp(mix_dialog_id, chan, 1.0);
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
  if (!mix_dialog_slider_dragging) mix_dialog_start_drag(mix_dialog_id);
  mix_dialog_slider_dragging = true;
  change_mix_amp(mix_dialog_id, chan, scroll_to_amp(scrollval));
}

static gboolean amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chan;
  Float scrollval;
  chan = get_user_int_data(G_OBJECT(w));
  scrollval = GTK_ADJUSTMENT(w_amp_adjs[chan])->value;
  mix_dialog_slider_dragging = false;
  change_mix_amp(mix_dialog_id, chan, scroll_to_amp(scrollval));
  return(false);
}

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
  mix_id = mix_dialog_id;
  chans = mix_dialog_mix_input_chans(mix_id);
  e = mix_dialog_envs(mix_id);
  for (chan = 0; chan < chans; chan++)
    {
      edp_display_graph(spfs[chan], _("mix env"), ax, 
			(int)(chan * widget_width(w) / chans), 0,
			widget_width(w) / chans, widget_height(w), 
			e[chan], false, true);
      cur_env = mix_dialog_mix_amp_env(mix_id, chan);
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
  mix_id = mix_dialog_id;
  chans = mix_dialog_mix_input_chans(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_dialog_env(mix_id, chan);
  if (edp_handle_press(spfs[chan],
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
  mix_id = mix_dialog_id;
  chans = mix_dialog_mix_input_chans(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  last_clicked_env_chan = chan;
  e = mix_dialog_env(mix_id, chan);
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
      mix_id = mix_dialog_id;
      chans = mix_dialog_mix_input_chans(mix_id);
      pos = (Float)x / (Float)widget_width(w);
      chan = (int)(pos * chans);
      last_clicked_env_chan = chan;
      e = mix_dialog_env(mix_id, chan);
      edp_handle_point(spfs[chan],
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

static GtkWidget *w_id = NULL, *w_beg = NULL, *w_track = NULL, *mix_play = NULL, *w_id_label = NULL;
static GtkWidget *w_track_label = NULL, *mix_track_play_pix = NULL, *mix_play_pix = NULL, *mix_track_play = NULL;
static GdkPixmap *speaker_off_pix, *speaker_on_pix, *mix_speaker_pix, *mix_track_speaker_pix;

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
	  mix_dialog_id = id;
	  update_mix_dialog(id);
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
      mix_id = mix_dialog_id;
      cp = mix_dialog_mix_channel(mix_id);
      set_mix_position(mix_id, (off_t)(string2Float(val) * SND_SRATE(cp->sound)));
      update_mix_dialog(mix_id);
    }
}

static void mix_track_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track));
  if (val)
    mix_dialog_set_mix_track(mix_dialog_id, string2int(val));
}

static bool mix_playing = false;
bool mix_play_stopped(void) {return(!mix_playing);}

void reflect_mix_play_stop(void)
{
  mix_playing = false;
  mix_speaker_pix = speaker_off_pix;
  mix_track_speaker_pix = speaker_off_pix;
  gdk_draw_drawable(GDK_DRAWABLE(mix_play_pix->window), ss->sgx->basic_gc, mix_speaker_pix, 0, 0, 2, 4, 12, 12);
  gdk_draw_drawable(GDK_DRAWABLE(mix_track_play_pix->window), ss->sgx->basic_gc, mix_track_speaker_pix, 0, 0, 2, 4, 12, 12);
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
      mix_dialog_mix_play(mix_dialog_id);
    }
}

static void mix_track_play_callback(GtkWidget *w, gpointer context) 
{
  if (mix_playing)
    {
      reflect_mix_play_stop();
    }
  else
    {
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

static void apply_mix_dialog(GtkWidget *w, gpointer context)
{
  /* set all mix amp envs, last one should remix */
  int i, chans, mix_id;
  env **envs;
  mix_id = mix_dialog_id;
  chans = mix_dialog_mix_input_chans(mix_id);
  envs = mix_dialog_envs(mix_id);
  for (i = 0; i < chans; i++)
    if (i != last_clicked_env_chan)
      set_mix_amp_env_without_edit(mix_id, i, envs[i]);
  set_mix_amp_env(mix_id, last_clicked_env_chan, envs[last_clicked_env_chan]);
  mix_amp_env_resize(w_env);
}

static void dismiss_mix_dialog(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(mix_dialog);
}

static void delete_mix_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(mix_dialog);
}

static GtkWidget *nextb, *previousb;

static void mix_next_callback(GtkWidget *w, gpointer context)
{
  int id;
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
  GtkWidget *dismiss_button, *help_button, *rc, *apply_button, *mix_frame, *track_frame, *rc_top, *rc1;
  char amplab[LABEL_BUFFER_SIZE];
  int mix_id, i;
  mix_id = mix_dialog_id;
  if (mix_dialog == NULL)
    {
      mix_dialog_id = any_mix_id();
      mix_dialog = snd_gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(mix_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(mix_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(delete_mix_dialog), NULL, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(mix_dialog), _("Mixes"));
      sg_make_resizable(mix_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(mix_dialog), 6);
      gtk_widget_realize(mix_dialog);
      
      dismiss_button = gtk_button_new_with_label(_("Dismiss"));
      gtk_widget_set_name(dismiss_button, "quit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), dismiss_button, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(dismiss_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(dismiss_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_mix_dialog), NULL, 0),
				     0);
      gtk_widget_show(dismiss_button);

      previousb = gtk_button_new_with_label(_("Previous"));
      gtk_widget_set_name(previousb, "reset_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), previousb, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(previousb),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(previousb))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_previous_callback), NULL, 0),
				     0);
      gtk_widget_show(previousb);

      nextb = gtk_button_new_with_label(_("Next"));
      gtk_widget_set_name(nextb, "doit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), nextb, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(nextb),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(nextb))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_next_callback), NULL, 0),
				     0);
      gtk_widget_show(nextb);

      apply_button = gtk_button_new_with_label(_("Apply Env"));
      gtk_widget_set_name(apply_button, "doit_again_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), apply_button, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(apply_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(apply_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(apply_mix_dialog), NULL, 0),
				     0);
      gtk_widget_show(apply_button);

      help_button = gtk_button_new_with_label(_("Help"));
      gtk_widget_set_name(help_button, "help_button");
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(mix_dialog)->action_area), help_button, true, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_dialog_help_callback), NULL, 0),
				     0);
      gtk_widget_show(help_button);


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
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_track_activated), NULL, 0),
				     0);

      mix_track_play = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(rc1), mix_track_play, false, false, 2);
      g_signal_connect_closure_by_id(GTK_OBJECT(mix_track_play),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(mix_track_play))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_track_play_callback), NULL, 0),
				     0);
      gtk_widget_show(mix_track_play);
      gtk_widget_modify_bg(mix_track_play, GTK_STATE_ACTIVE, ss->sgx->basic_color);
      gtk_widget_modify_bg(mix_track_play, GTK_STATE_SELECTED, ss->sgx->basic_color);
      
      mix_track_play_pix = gtk_drawing_area_new();
      gtk_widget_set_events(mix_track_play_pix, GDK_EXPOSURE_MASK);
      gtk_widget_set_size_request(mix_track_play_pix, 16, 16);
      gtk_container_add(GTK_CONTAINER(mix_track_play), mix_track_play_pix);
      gtk_widget_show(mix_track_play_pix);
      g_signal_connect_closure_by_id(GTK_OBJECT(mix_track_play_pix),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(mix_track_play_pix))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(mix_track_play_pix_expose), NULL, 0),
				     0);

      /* SPEED */
      w_speed_form = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->vbox), w_speed_form, false, false, 4);
      
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
	  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->vbox), w_amp_forms[i], false, false, 0);
      
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
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_dialog)->vbox), w_env_frame, true, true, 10);
      
      w_env = gtk_drawing_area_new();
      gtk_widget_set_events(w_env, GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(w_env_frame), w_env);
      gtk_widget_modify_bg(w_env, GTK_STATE_NORMAL, ss->sgx->highlight_color);
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

      gtk_widget_show(mix_dialog);
      set_dialog_widget(MIX_DIALOG, mix_dialog);

      for (i = 0; i < CHANS_ALLOCATED; i++) spfs[i] = new_env_editor();
      speed_number_buffer[1] = local_decimal_point();
      amp_number_buffer[1] = local_decimal_point();
    }
  else 
    {
      raise_dialog(mix_dialog);
      reflect_edit_in_mix_dialog_envs(mix_dialog_id);
    }
  update_mix_dialog(mix_dialog_id);
  return(mix_dialog);
}

static void update_mix_dialog(int mix_id) 
{
  chan_info *cp;
  int i, chans;
  off_t beg, len;
  Float val;
  char lab[LABEL_BUFFER_SIZE];
  if (mix_id == INVALID_MIX_ID) return;
  if (!(mix_ok(mix_dialog_id))) mix_dialog_id = mix_id;
  if (!(mix_ok(mix_dialog_id))) {mix_dialog_id = any_mix_id(); mix_id = mix_dialog_id;}
  if (mix_id == mix_dialog_id)
    {
      if (mix_dialog == NULL) 
	make_mix_dialog();
      else
	{
	  set_sensitive(nextb, (next_mix_id(mix_id) != INVALID_MIX_ID));
	  set_sensitive(previousb, (previous_mix_id(mix_id) != INVALID_MIX_ID));
	}
      
      /* now reflect current mix state in mix dialog controls */
      cp = mix_dialog_mix_channel(mix_id);

      val = mix_dialog_mix_speed(mix_id);
      if (val != current_speed)
	{
	  reflect_mix_speed(val, cp->sound);
	  current_speed = val;
	}

      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%d", mix_dialog_mix_track(mix_id));
      gtk_entry_set_text(GTK_ENTRY(w_track), lab);

      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%d", mix_id);
      gtk_entry_set_text(GTK_ENTRY(w_id), lab);

      beg = mix_dialog_mix_position(mix_id);
      len = mix_frames(mix_id);
      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
		   (float)((double)beg / (float)SND_SRATE(cp->sound)),
		   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
      gtk_entry_set_text(GTK_ENTRY(w_beg), lab);

      chans = mix_dialog_mix_input_chans(mix_id);
      if (chans > 8) chans = 8;

      set_label(w_amp_labels[0], (chans == 1) ? "amp:" : "amp 0:");
      for (i = 0; i < chans; i++)
	{
	  gtk_widget_show(w_amp_labels[i]);	  
	  gtk_widget_show(w_amp_numbers[i]);	  
	  gtk_widget_show(w_amps[i]);
	  gtk_widget_show(w_amp_forms[i]);
	  val = mix_dialog_mix_amp(mix_id, i);
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


/* ---------------- track dialog ---------------- */

static GtkWidget *track_dialog = NULL;
static bool track_dialog_slider_dragging = false;
static void update_track_dialog(int track_id);
static int track_dialog_id = INVALID_TRACK_ID;

static GtkWidget *w_track_speed, *w_track_speed_label, *w_track_speed_number, *w_track_speed_form, *w_track_speed_event;
static GtkObject *w_track_speed_adj;
static char track_speed_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};
static Float current_track_speed = 1.0;

static void change_track_speed(int track_id, Float val)
{
  chan_info *cp;
  cp = track_channel(track_id, 0);
  track_dialog_set_speed(track_id,
			 srate_changed(val, 
				       track_speed_number_buffer, 
				       (cp) ? cp->sound->speed_control_style : speed_control_style(ss), 
				       (cp) ? cp->sound->speed_control_tones : speed_control_tones(ss)),
			 track_dialog_slider_dragging);
  gtk_label_set_text(GTK_LABEL(w_track_speed_number), track_speed_number_buffer);
}

static gboolean track_speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  change_track_speed(track_dialog_id, 1.0);
  GTK_ADJUSTMENT(w_track_speed_adj)->value = .45;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_track_speed_adj));
  return(false);
}

static void track_speed_changed_callback(GtkAdjustment *adj, gpointer data)
{
  Float val;
  chan_info *cp;
  cp = track_channel(track_dialog_id, 0);
  val = srate_changed(exp((GTK_ADJUSTMENT(w_track_speed_adj)->value - .45) / .15),
		      track_speed_number_buffer,
		      (cp) ? cp->sound->speed_control_style : speed_control_style(ss),
		      (cp) ? cp->sound->speed_control_tones : speed_control_tones(ss));
  if (!track_dialog_slider_dragging) track_dialog_start_slider_drag(track_dialog_id);
  track_dialog_slider_dragging = true;
  change_track_speed(track_dialog_id, val);
  gtk_label_set_text(GTK_LABEL(w_track_speed_number), track_speed_number_buffer);
}

static gboolean track_speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  Float val;
  chan_info *cp;
  cp = track_channel(track_dialog_id, 0);
  val = srate_changed(exp((GTK_ADJUSTMENT(w_track_speed_adj)->value - .45) / .15),
		      track_speed_number_buffer,
		      (cp) ? cp->sound->speed_control_style : speed_control_style(ss),
		      (cp) ? cp->sound->speed_control_tones : speed_control_tones(ss));
  track_dialog_slider_dragging = false;
  change_track_speed(track_dialog_id, val);
  return(false);
}

static void reflect_track_speed(Float uval, snd_info *sp)
{
  Float val;
  chan_info *cp;
  cp = track_channel(track_dialog_id, 0);
  val = srate_changed(uval,
		      track_speed_number_buffer,
		      (cp) ? cp->sound->speed_control_style : speed_control_style(ss),
		      (cp) ? cp->sound->speed_control_tones : speed_control_tones(ss));
  gtk_label_set_text(GTK_LABEL(w_track_speed_number), track_speed_number_buffer);
  if (val > 0.0)
    GTK_ADJUSTMENT(w_track_speed_adj)->value = .45 + .15 * log(val);
  else GTK_ADJUSTMENT(w_track_speed_adj)->value = 0.0;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_track_speed_adj));
}

static GtkWidget *w_track_amp, *w_track_amp_label, *w_track_amp_number, *w_track_amp_event, *w_track_amp_form;
static GtkObject *w_track_amp_adj;
static Float current_track_amp = 1.0;
static char track_amp_number_buffer[5] = {'1', STR_decimal, '0', '0', '\0'};

static void change_track_amp(int track_id, Float val)
{
  char *sfs;
  track_dialog_set_amp(track_id, val, track_dialog_slider_dragging);
  sfs = prettyf(val, 2);
  fill_number(sfs, track_amp_number_buffer);
  gtk_label_set_text(GTK_LABEL(w_track_amp_number), track_amp_number_buffer);
  FREE(sfs);
}

static gboolean track_amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  change_track_amp(track_dialog_id, 1.0);
  GTK_ADJUSTMENT(w_track_amp_adj)->value = 0.5;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_track_amp_adj));
  return(false);
}

static void track_amp_changed_callback(GtkAdjustment *adj, gpointer data)
{
  Float scrollval;
  scrollval = GTK_ADJUSTMENT(w_track_amp_adj)->value;
  if (!track_dialog_slider_dragging) track_dialog_start_slider_drag(track_dialog_id);
  track_dialog_slider_dragging = true;
  change_track_amp(track_dialog_id, scroll_to_amp(scrollval));
}

static gboolean track_amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  Float scrollval;
  scrollval = GTK_ADJUSTMENT(w_track_amp_adj)->value;
  track_dialog_slider_dragging = false;
  change_track_amp(track_dialog_id, scroll_to_amp(scrollval));
  return(false);
}

static GtkWidget *w_track_env_frame, *w_track_env;
static axis_context *track_ax = NULL;
static GdkGC *track_cur_gc;
static void *track_spf;

static void track_amp_env_resize(GtkWidget *w)
{
  GdkWindow *wn;
  env *e;
  env *cur_env;
  if (track_ax == NULL)
    {
      wn = MAIN_WINDOW(ss);
      track_cur_gc = gdk_gc_new(wn);
      gdk_gc_set_background(track_cur_gc, (ss->sgx)->graph_color);
      gdk_gc_set_foreground(track_cur_gc, (ss->sgx)->data_color);
      gdk_gc_set_function(track_cur_gc, GDK_COPY);

      track_ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      track_ax->wn = w_track_env->window;
      track_ax->w = w_track_env;
      track_ax->gc = track_cur_gc;
    }
  else clear_window(track_ax);
  e = track_dialog_env(track_dialog_id);
  edp_display_graph(track_spf, _("track env"), track_ax, 
		    0, 0,
		    widget_width(w), widget_height(w), 
		    e, false, true);
  cur_env = track_dialog_track_amp_env(track_dialog_id);
  if (cur_env)
    {
      gdk_gc_set_foreground(track_ax->gc, (ss->sgx)->enved_waveform_color);
      edp_display_graph(track_spf, _("track env"), track_ax, 
			0, 0,
			widget_width(w), widget_height(w), 
			cur_env, false, false);
      gdk_gc_set_foreground(track_ax->gc, (ss->sgx)->black);
    }
}

static gboolean track_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  env *e;
  e = track_dialog_env(track_dialog_id);
  if (edp_handle_press(track_spf,
		       (int)(ev->x), (int)(ev->y), ev->time, 
		       e,
		       false,
		       1.0))
    track_amp_env_resize(w);
  return(false);
}

static gboolean track_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  env *e;
  e = track_dialog_env(track_dialog_id);
  edp_handle_release(track_spf, e);
  track_amp_env_resize(w);
  return(false);
}

static gboolean track_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  int x, y;
  GdkModifierType state;
  env *e;
  if (ev->state & GDK_BUTTON1_MASK)
    {
      if (ev->is_hint)
	gdk_window_get_pointer(ev->window, &x, &y, &state);
      else
	{
	  x = (int)(ev->x);
	  y = (int)(ev->y);
	}
      e = track_dialog_env(track_dialog_id);
      edp_handle_point(track_spf,
		       x, y, ev->time, 
		       e,
		       false,
		       1.0);
      track_amp_env_resize(w);
    }
  return(false);
}

static gboolean track_amp_env_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  track_amp_env_resize(w);
  return(false);
}

static gboolean track_amp_env_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  track_amp_env_resize(w);
  return(false);
}

static GtkWidget *w_track_id = NULL, *w_track_beg = NULL, *w_track_id_label = NULL;
static GtkWidget *w_track_track_label = NULL, *w_track_play_pix = NULL, *w_track_play = NULL;
static GdkPixmap *track_speaker_pix;

static void track_id_activated(GtkWidget *w, gpointer context)
{
  char *val;
  int id;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track_id));
  if (val)
    {
      id = string2int(val);
      if (track_p(id)) 
	{
	  track_dialog_id = id;
	  update_track_dialog(id);
	}
    }
}

static void track_beg_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  chan_info *cp;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track_beg));
  if (val)
    {
      cp = track_channel(track_dialog_id, 0);
      set_track_position(track_dialog_id, (off_t)(string2Float(val) * SND_SRATE(cp->sound)));
      update_track_dialog(track_dialog_id);
    }
}

static void track_track_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track));
  if (val)
    {
      int id;
      id = string2int(val);
      if (id != track_dialog_id)
	{
	  if (!(set_track_track(track_dialog_id, id)))
	    {
	      /* TODO: post error and undo */
	    }
	}
    }
}

static bool track_playing = false;
bool track_play_stopped(void) {return(!track_playing);}

void reflect_track_play_stop(void)
{
  track_playing = false;
  track_speaker_pix = speaker_off_pix;
  gdk_draw_drawable(GDK_DRAWABLE(w_track_play_pix->window), ss->sgx->basic_gc, track_speaker_pix, 0, 0, 2, 4, 12, 12);
}

static void track_dialog_play_callback(GtkWidget *w, gpointer context) 
{
  if (track_playing)
    {
      reflect_track_play_stop();
    }
  else
    {
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


static void apply_track_dialog(GtkWidget *w, gpointer context)
{
  env *e;
  e = track_dialog_env(track_dialog_id);
  track_dialog_set_amp_env(track_dialog_id, e);
  track_amp_env_resize(w_track_env);
}

static void dismiss_track_dialog(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(track_dialog);
}

static void delete_track_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(track_dialog);
}

static GtkWidget *track_nextb, *track_previousb;

static void track_next_callback(GtkWidget *w, gpointer context)
{
  int id;
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
  GtkWidget *dismiss_button, *help_button, *rc, *apply_button, *track_frame, *t_frame, *rc_top, *rc1;

  if (track_dialog == NULL)
    {
      track_dialog_id = any_track_id();
      track_dialog = snd_gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(track_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(track_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(delete_track_dialog), NULL, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(track_dialog), _("Tracks"));
      sg_make_resizable(track_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(track_dialog), 6);
      gtk_widget_realize(track_dialog);
      
      dismiss_button = gtk_button_new_with_label(_("Dismiss"));
      gtk_widget_set_name(dismiss_button, "quit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), dismiss_button, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(dismiss_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(dismiss_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_track_dialog), NULL, 0),
				     0);
      gtk_widget_show(dismiss_button);

      track_previousb = gtk_button_new_with_label(_("Previous"));
      gtk_widget_set_name(track_previousb, "reset_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), track_previousb, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(track_previousb),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(track_previousb))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_previous_callback), NULL, 0),
				     0);
      gtk_widget_show(track_previousb);

      track_nextb = gtk_button_new_with_label(_("Next"));
      gtk_widget_set_name(track_nextb, "doit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), track_nextb, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(track_nextb),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(track_nextb))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_next_callback), NULL, 0),
				     0);
      gtk_widget_show(track_nextb);

      apply_button = gtk_button_new_with_label(_("Apply Env"));
      gtk_widget_set_name(apply_button, "doit_again_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), apply_button, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(apply_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(apply_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(apply_track_dialog), NULL, 0),
				     0);
      gtk_widget_show(apply_button);

      help_button = gtk_button_new_with_label(_("Help"));
      gtk_widget_set_name(help_button, "help_button");
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(track_dialog)->action_area), help_button, true, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_dialog_help_callback), NULL, 0),
				     0);
      gtk_widget_show(help_button);

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

      w_track_id = snd_entry_new(rc, false);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_id),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(w_track_id))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_id_activated), NULL, 0),
				     0);

      w_track_beg = snd_entry_new(rc, false);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_beg),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(w_track_beg))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_beg_activated), NULL, 0),
				     0);

      w_track_play = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(rc), w_track_play, false, false, 2);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_play),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(w_track_play))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_dialog_play_callback), NULL, 0),
				     0);
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
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_play_pix),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_play_pix))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(w_track_play_pix_expose), NULL, 0),
				     0);


      w_track_track_label = gtk_label_new(_("track:"));
      gtk_box_pack_start(GTK_BOX(rc1), w_track_track_label, false, false, 4);
      gtk_widget_show(w_track_track_label);

      w_track = snd_entry_new(rc1, false);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(w_track))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_track_activated), NULL, 0),
				     0);


      /* SPEED */
      w_track_speed_form = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), w_track_speed_form, false, false, 4);
      
      w_track_speed_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_track_speed_form), w_track_speed_event, false, false, 4);
      gtk_widget_show(w_track_speed_event);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_speed_event),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_speed_event))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_speed_click_callback), NULL, 0),
				     0);
      
      w_track_speed_label = gtk_label_new(_("speed:"));
      gtk_container_add(GTK_CONTAINER(w_track_speed_event), w_track_speed_label);
      gtk_widget_show(w_track_speed_label);

      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO: w_track_speed_number = gtk_label_new("1/1"); break;
	case SPEED_CONTROL_AS_SEMITONE: w_track_speed_number = gtk_label_new("1"); break;
	default:  w_track_speed_number = gtk_label_new(track_speed_number_buffer); break;
	}
      gtk_box_pack_start(GTK_BOX(w_track_speed_form), w_track_speed_number, false, false, 0);
      gtk_widget_show(w_track_speed_number);
      
      w_track_speed_adj = gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
      w_track_speed = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_track_speed_adj));
      gtk_box_pack_start(GTK_BOX(w_track_speed_form), w_track_speed, true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_speed_adj),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(w_track_speed_adj))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_speed_changed_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_speed),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_speed))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_speed_release_callback), NULL, 0),
				     0);
      gtk_widget_show(w_track_speed);
      gtk_widget_show(w_track_speed_form);

      /* AMP */
      current_track_amp = 1.0;

      w_track_amp_form = gtk_hbox_new(false, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), w_track_amp_form, false, false, 0);
      
      w_track_amp_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_track_amp_form), w_track_amp_event, false, false, 4);
      gtk_widget_show(w_track_amp_event);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_amp_event),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_amp_event))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_amp_click_callback), NULL, 0),
				     0);
      
      w_track_amp_label = gtk_label_new(_("amp:"));
      gtk_container_add(GTK_CONTAINER(w_track_amp_event), w_track_amp_label);
      gtk_widget_show(w_track_amp_label);
      
      w_track_amp_number = gtk_label_new(track_amp_number_buffer);
      gtk_box_pack_start(GTK_BOX(w_track_amp_form), w_track_amp_number, false, false, 0);
      gtk_widget_show(w_track_amp_number);
	  
      w_track_amp_adj = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
      w_track_amp = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_track_amp_adj));
      gtk_box_pack_start(GTK_BOX(w_track_amp_form), w_track_amp, true, true, 4);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_amp_adj),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(w_track_amp_adj))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_amp_changed_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_amp),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_amp))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_amp_release_callback), NULL, 0),
				     0);
      gtk_widget_show(w_track_amp);
      gtk_widget_show(w_track_amp_form);

      /* GRAPH */
      w_track_env_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(track_dialog)->vbox), w_track_env_frame, true, true, 10);
      
      w_track_env = gtk_drawing_area_new();
      gtk_widget_set_events(w_track_env, GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(w_track_env_frame), w_track_env);
      gtk_widget_modify_bg(w_track_env, GTK_STATE_NORMAL, ss->sgx->highlight_color);
      gtk_widget_show(w_track_env);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_env),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_amp_env_expose_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_env),
				     g_signal_lookup("configure_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_amp_env_resize_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_env),
				     g_signal_lookup("button_press_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_drawer_button_press), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_env),
				     g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_drawer_button_release), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(w_track_env),
				     g_signal_lookup("motion_notify_event", G_OBJECT_TYPE(GTK_OBJECT(w_track_env))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(track_drawer_button_motion), NULL, 0),
				     0);
      gtk_widget_show(w_track_env_frame);

      gtk_widget_show(track_dialog);
      set_dialog_widget(TRACK_DIALOG, track_dialog);

      track_spf = new_env_editor();
      track_speed_number_buffer[1] = local_decimal_point();
      track_amp_number_buffer[1] = local_decimal_point();

      if ((widget_width(track_dialog) > 0) && (widget_height(track_dialog) < 300))
	set_widget_size(track_dialog, widget_width(track_dialog), 300);
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
  chan_info *cp;
  off_t beg, len;
  Float val;
  char lab[LABEL_BUFFER_SIZE];
  if (track_id == INVALID_TRACK_ID) return;
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
      
      cp = track_channel(track_id, 0);
      val = track_dialog_track_speed(track_id);
      if (val != current_track_speed)
	{
	  reflect_track_speed(val, cp->sound);
	  current_track_speed = val;
	}

      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%d", track_dialog_track_track(track_id));
      gtk_entry_set_text(GTK_ENTRY(w_track), lab);

      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%d", track_id);
      gtk_entry_set_text(GTK_ENTRY(w_track_id), lab);

      if (cp)
	{
	  beg = track_position(track_id, -1);
	  len = track_frames(track_id, -1);
	  mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
		       (float)((double)beg / (float)SND_SRATE(cp->sound)),
		       (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
	  gtk_entry_set_text(GTK_ENTRY(w_track_beg), lab);
	}

      val = track_dialog_track_amp(track_id);
      GTK_ADJUSTMENT(w_track_amp_adj)->value = amp_to_scroll(val);
      gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_track_amp_adj));
      current_track_amp = val;

      track_amp_env_resize(w_track_env);
    }
}



/* ---------------- reflection ---------------- */

void reflect_mix_or_track_change(int mix_id, int track_id, bool forced)
{
  if ((mix_dialog) &&
      (GTK_WIDGET_VISIBLE(mix_dialog)))
    {
      if ((forced) && (mix_ok(mix_id))) mix_dialog_id = mix_id;
      if ((mix_dialog_id == mix_id) || (!(mix_ok(mix_dialog_id))))
	{
	  /* reflect_edit_in_mix_dialog_envs(mix_dialog_id); */
	  update_mix_dialog(mix_id);
	}
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
      if ((track_dialog_id == track_id) || (!(track_p(track_dialog_id))))
	{
	  /* reflect_edit_in_track_dialog_env(track_dialog_id); */
	  update_track_dialog(track_id);
	}
      if (track_id != INVALID_TRACK_ID)
	{
	  set_sensitive(track_nextb, (next_track_id(track_dialog_id) != INVALID_TRACK_ID));
	  set_sensitive(track_previousb, (previous_track_id(track_dialog_id) != INVALID_TRACK_ID));
	}
    }
}



/* ---------------- xen connection ---------------- */

static XEN g_set_mix_dialog_mix(XEN val)
{
  mix_dialog_id = XEN_TO_C_INT(val);
  update_mix_dialog(mix_dialog_id);
  return(val);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_set_mix_dialog_mix_w, g_set_mix_dialog_mix)
#else
#define g_set_mix_dialog_mix_w g_set_mix_dialog_mix
#endif

void g_init_gxmix(void)
{
  XEN_DEFINE_PROCEDURE("set-mix-dialog-mix", g_set_mix_dialog_mix_w, 1, 0, 0, "internal testing func");
}

