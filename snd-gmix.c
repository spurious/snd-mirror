#include "snd.h"

static GtkWidget *mix_panel = NULL;
static int dragging = 0;
static void update_mix_panel(int mix_id);

/* ---------------- SPEED ---------------- */

static GtkWidget *w_speed, *w_speed_label, *w_speed_number, *w_speed_form, *w_speed_event;
static GtkObject *w_speed_adj;
static char speed_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};
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
  snd_state *ss = (snd_state *)data;
  change_mix_speed(current_mix_id(ss), 1.0);
  GTK_ADJUSTMENT(w_speed_adj)->value = .45;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_speed_adj));
  return(FALSE);
}

static void speed_changed_callback(GtkAdjustment *adj, gpointer data)
{
  Float val;
  int mix_id;
  snd_state *ss = (snd_state *)data;
  chan_info *cp;
  snd_info *sp;
  mix_id = current_mix_id(ss);
  cp = mix_channel_from_id(mix_id);
  sp = cp->sound;
  val = srate_changed(exp((GTK_ADJUSTMENT(w_speed_adj)->value - .45) / .15),
		      speed_number_buffer,
		      sp->speed_control_style,
		      sp->speed_control_tones);
  if (dragging == 0) start_mix_drag(mix_id);
  dragging = 1;
  change_mix_speed(mix_id, val);
  gtk_label_set_text(GTK_LABEL(w_speed_number), speed_number_buffer);
}

static gboolean speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  Float val;
  int mix_id;
  snd_state *ss = (snd_state *)data;
  chan_info *cp;
  snd_info *sp;
  mix_id = current_mix_id(ss);
  cp = mix_channel_from_id(mix_id);
  sp = cp->sound;
  val = srate_changed(exp((GTK_ADJUSTMENT(w_speed_adj)->value - .45) / .15),
		      speed_number_buffer,
		      sp->speed_control_style,
		      sp->speed_control_tones);
  dragging = 0;
  change_mix_speed(mix_id, val);
  return(FALSE);
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
static int chans_allocated = 0;
static char amp_number_buffer[5] ={'1', STR_decimal, '0', '0', '\0'};

static int allocate_amps(int chans)
{
  int i;
  if (chans > chans_allocated)
    {
      if (chans_allocated == 0)
	{
	  if (chans < 8)
	    chans_allocated = 8;
	  else chans_allocated = chans;
	  w_amp_numbers = (GtkWidget **)CALLOC(chans_allocated, sizeof(GtkWidget *));
	  w_amp_labels = (GtkWidget **)CALLOC(chans_allocated, sizeof(GtkWidget *));
	  w_amps = (GtkWidget **)CALLOC(chans_allocated, sizeof(GtkWidget *));
	  w_amp_events = (GtkWidget **)CALLOC(chans_allocated, sizeof(GtkWidget *));
	  w_amp_forms = (GtkWidget **)CALLOC(chans_allocated, sizeof(GtkWidget *));
	  w_amp_adjs = (GtkObject **)CALLOC(chans_allocated, sizeof(GtkObject *));
	  current_amps = (Float *)CALLOC(chans_allocated, sizeof(Float));
	}
      else
	{
	  w_amp_numbers = (GtkWidget **)REALLOC(w_amp_numbers, chans * sizeof(GtkWidget *));
	  w_amp_labels = (GtkWidget **)REALLOC(w_amp_labels, chans * sizeof(GtkWidget *));
	  w_amps = (GtkWidget **)REALLOC(w_amps, chans * sizeof(GtkWidget *));
	  w_amp_events = (GtkWidget **)REALLOC(w_amps, chans * sizeof(GtkWidget *));
	  w_amp_forms = (GtkWidget **)REALLOC(w_amps, chans * sizeof(GtkWidget *));
	  w_amp_adjs = (GtkObject **)REALLOC(w_amps, chans * sizeof(GtkObject *));
	  current_amps = (Float *)REALLOC(current_amps, chans * sizeof(Float));
	  for (i = chans_allocated; i < chans; i++)
	    {
	      w_amp_numbers[i] = NULL;
	      w_amp_labels[i] = NULL;
	      w_amps[i] = NULL;
	      w_amp_events[i] = NULL;
	      w_amp_forms[i] = NULL;
	      w_amp_adjs[i] = NULL;
	      current_amps[i] = 0.0;
	    }
	  chans_allocated = chans;
	}
    }
  return(chans_allocated);
}

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
  snd_state *ss = (snd_state *)data;
  chan = (int)get_user_data(GTK_OBJECT(w));
  change_mix_amp(current_mix_id(ss), chan, 1.0);
  GTK_ADJUSTMENT(w_amp_adjs[chan])->value = 0.5;
  gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_amp_adjs[chan]));
  return(FALSE);
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
  snd_state *ss = (snd_state *)data;
  chan = (int)get_user_data(GTK_OBJECT(adj));
  scrollval = GTK_ADJUSTMENT(w_amp_adjs[chan])->value;
  if (dragging == 0) start_mix_drag(current_mix_id(ss));
  dragging = 1;
  change_mix_amp(current_mix_id(ss), chan, scroll_to_amp(scrollval));
}

static gboolean amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  int chan;
  Float scrollval;
  snd_state *ss = (snd_state *)data;
  chan = (int)get_user_data(GTK_OBJECT(w));
  scrollval = GTK_ADJUSTMENT(w_amp_adjs[chan])->value;
  dragging = 0;
  change_mix_amp(current_mix_id(ss), chan, scroll_to_amp(scrollval));
  return(FALSE);
}


/* ---------------- AMP ENV ---------------- */

static GtkWidget *w_env_frame, *w_env;
static axis_context *ax = NULL;
static GdkGC *cur_gc;
static void *spfs[8];

static void mix_amp_env_resize(GtkWidget *w, snd_state *ss)
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
  mix_id = current_mix_id(ss);
  chans = mix_input_chans_from_id(mix_id);
  e = mix_panel_envs(mix_id);
  for (chan = 0; chan < chans; chan++)
    {
      edp_display_graph(ss, spfs[chan], "mix env", ax, 
			(int)(chan * widget_width(w) / chans), 0,
			widget_width(w) / chans, widget_height(w), 
			e[chan], FALSE, TRUE);
      cur_env = mix_amp_env_from_id(mix_id, chan);
      if (cur_env)
	{
	  gdk_gc_set_foreground(ax->gc, (ss->sgx)->enved_waveform_color);
	  edp_display_graph(ss, spfs[chan], "mix env", ax, 
			    (int)(chan * widget_width(w) / chans), 0,
			    widget_width(w) / chans, widget_height(w), 
			    cur_env, FALSE, FALSE);
	  gdk_gc_set_foreground(ax->gc, (ss->sgx)->black);
	}
    }
}

static gboolean mix_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  int mix_id, chans, chan;
  env *e;
  Float pos;
  mix_id = current_mix_id(ss);
  chans = mix_input_chans_from_id(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  e = mix_panel_env(mix_id, chan);
  if (edp_handle_press(ss,
		       spfs[chan],
		       (int)(ev->x), (int)(ev->y), ev->time, 
		       e,
		       FALSE,
		       1.0))
    mix_amp_env_resize(w, ss);
  return(FALSE);
}

static gboolean mix_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  snd_state *ss = (snd_state *)data;
  int mix_id, chans, chan;
  env *e;
  Float pos;
  mix_id = current_mix_id(ss);
  chans = mix_input_chans_from_id(mix_id);
  pos = (Float)(ev->x) / (Float)widget_width(w);
  chan = (int)(pos * chans);
  e = mix_panel_env(mix_id, chan);
  edp_handle_release(spfs[chan], e);
  mix_amp_env_resize(w, ss);
  return(FALSE);
}

static gboolean mix_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  snd_state *ss = (snd_state *)data;
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
      mix_id = current_mix_id(ss);
      chans = mix_input_chans_from_id(mix_id);
      pos = (Float)x / (Float)widget_width(w);
      chan = (int)(pos * chans);
      e = mix_panel_env(mix_id, chan);
      edp_handle_point(ss,
		       spfs[chan],
		       x, y, ev->time, 
		       e,
		       FALSE,
		       1.0);
      mix_amp_env_resize(w, ss);
    }
  return(FALSE);
}


static gboolean mix_amp_env_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  mix_amp_env_resize(w, (snd_state *)data);
  return(FALSE);
}

static gboolean mix_amp_env_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  mix_amp_env_resize(w, (snd_state *)data);
  return(FALSE);
}



/* ---------------- MIX PANEL ---------------- */

static GtkWidget *w_id = NULL, *w_name = NULL, *w_beg = NULL, *w_track = NULL, *w_play = NULL, *w_id_label = NULL, *w_track_label = NULL, *w_play_pix = NULL;
static SG_PIXMAP *speaker_pix;
static SG_BITMAP *speaker_mask;

static void id_activated(GtkWidget *w, gpointer context)
{
  char *val;
  int id;
  snd_state *ss = (snd_state *)context;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_id));
  if (val)
    {
      id = string2int(val);
      if (mix_ok(id))
	{
	  ss->selected_mix = id;
	  update_mix_panel(ss->selected_mix);
	}
    }
}

static void name_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  snd_state *ss = (snd_state *)context;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_name));
  if (val)
    set_mix_name_from_id(current_mix_id(ss), val);
}

static void beg_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  chan_info *cp;
  int mix_id;
  snd_state *ss = (snd_state *)context;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_beg));
  if (val)
    {
      mix_id = current_mix_id(ss);
      cp = mix_channel_from_id(mix_id);
      set_mix_position_from_id(mix_id, (int)(string2Float(val) * SND_SRATE(cp->sound)));
      update_mix_panel(mix_id);
    }
}

static void track_activated(GtkWidget *w, gpointer context) 
{
  char *val;
  snd_state *ss = (snd_state *)context;
  val = (char *)gtk_entry_get_text(GTK_ENTRY(w_track));
  if (val)
    set_mix_track_from_id(current_mix_id(ss), string2int(val));
}

static int mix_playing = 0;
int mix_play_stopped(void) {return(!mix_playing);}

void reflect_mix_play_stop(void)
{
  snd_state *ss;
  if (w_play) 
    {
      ss = get_global_state();
      set_backgrounds(w_play, (ss->sgx)->basic_color);
    }
  mix_playing = 0;
}

static void play_callback(GtkWidget *w, gpointer context) 
{
  snd_state *ss = (snd_state *)context;
  if (mix_playing)
    {
      reflect_mix_play_stop();
    }
  else
    {
      mix_playing = 1;
      if (w_play) set_backgrounds(w_play, (ss->sgx)->pushed_button_color);
      mix_play_from_id(current_mix_id(ss));
    }
}

static void apply_mix_panel(GtkWidget *w, gpointer context)
{
  /* set all mix amp envs, last one should remix */
  snd_state *ss = (snd_state *)context;
  int i, chans, mix_id;
  env **envs;
  mix_id = current_mix_id(ss);
  chans = mix_input_chans_from_id(mix_id);
  envs = mix_panel_envs(mix_id);
  for (i = 0; i < chans - 1; i++)
    set_mix_amp_env_without_edit(mix_id, i, envs[i]);
  set_mix_amp_env(mix_id, chans - 1, envs[chans - 1]);
}

static void dismiss_mix_panel(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(mix_panel);
}

static gboolean delete_mix_panel(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(mix_panel);
  return(FALSE);
}

static void mix_panel_help(GtkWidget *w, gpointer context) 
{
  snd_help_with_wrap((snd_state *)context,
		     "Mix Panel",
"This dialog controls the currently selected mix");
}

GtkWidget *make_mix_panel(snd_state *ss)
{
  GtkWidget *dismiss_button, *help_button, *rc, *apply_button;
  char amplab[LABEL_BUFFER_SIZE];
  int mix_id, i, chans;
  mix_id = current_mix_id(ss);

  if (mix_panel == NULL)
    {
      mix_panel = gtk_dialog_new();
      SG_SIGNAL_CONNECT(GTK_OBJECT(mix_panel), "delete_event", GTK_SIGNAL_FUNC(delete_mix_panel), (gpointer)ss);
      /* SG_SIGNAL_CONNECT(GTK_OBJECT(mix_panel), "destroy", (GtkSignalFunc)dismiss_mix_panel, (gpointer)ss); */
      gtk_window_set_title(GTK_WINDOW(mix_panel), STR_Mix_Panel);
      SG_MAKE_RESIZABLE(mix_panel);
      set_background(mix_panel, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(mix_panel), 6);
      /* SG_SET_SIZE (GTK_WIDGET(mix_panel), 200, 300); */
      gtk_widget_realize(mix_panel);
      
      dismiss_button = gtk_button_new_with_label(STR_Dismiss);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->action_area), dismiss_button, FALSE, TRUE, 10);
      SG_SIGNAL_CONNECT(GTK_OBJECT(dismiss_button), "clicked", GTK_SIGNAL_FUNC(dismiss_mix_panel), (gpointer)ss);
      set_pushed_button_colors(dismiss_button, ss);
      gtk_widget_show(dismiss_button);

      apply_button = gtk_button_new_with_label("Apply Env");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->action_area), apply_button, FALSE, TRUE, 10);
      SG_SIGNAL_CONNECT(GTK_OBJECT(apply_button), "clicked", GTK_SIGNAL_FUNC(apply_mix_panel), (gpointer)ss);
      set_pushed_button_colors(apply_button, ss);
      gtk_widget_show(apply_button);

      help_button = gtk_button_new_with_label(STR_Help);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(mix_panel)->action_area), help_button, TRUE, TRUE, 10);
      SG_SIGNAL_CONNECT(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(mix_panel_help), (gpointer)ss);
      set_pushed_button_colors(help_button, ss);
      gtk_widget_show(help_button);


      /* top row of mix id name position track etc */

      rc = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->vbox), rc, FALSE, FALSE, 4);
      gtk_widget_show(rc);

      w_id_label = gtk_label_new("mix id:");
      gtk_box_pack_start(GTK_BOX(rc), w_id_label, FALSE, FALSE, 4);
      gtk_widget_show(w_id_label);

      w_id = snd_entry_new(ss, rc, FALSE);
      set_widget_width(w_id, 60);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_id), "activate", GTK_SIGNAL_FUNC(id_activated), (gpointer)ss);

      w_name = snd_entry_new(ss, rc, FALSE);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_name), "activate", GTK_SIGNAL_FUNC(name_activated), (gpointer)ss);

      w_beg = snd_entry_new(ss, rc, FALSE);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_beg), "activate", GTK_SIGNAL_FUNC(beg_activated), (gpointer)ss);

      w_track_label = gtk_label_new("track:");
      gtk_box_pack_start(GTK_BOX(rc), w_track_label, FALSE, FALSE, 4);
      gtk_widget_show(w_track_label);

      w_track = snd_entry_new(ss, rc, FALSE);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_track), "activate", GTK_SIGNAL_FUNC(track_activated), (gpointer)ss);

      w_play = gtk_button_new();
      gtk_box_pack_start(GTK_BOX(rc), w_play, FALSE, FALSE, 2);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_play), "clicked", GTK_SIGNAL_FUNC(play_callback), (gpointer)ss);
      gtk_widget_show(w_play);
      
      speaker_pix = SG_XPM_TO_PIXMAP(MAIN_WINDOW(ss), speaker_bits(), speaker_mask);
      w_play_pix = SG_PIXMAP_NEW(speaker_pix, speaker_mask);
      gtk_container_add(GTK_CONTAINER(w_play), w_play_pix);
      gtk_widget_show(w_play_pix);


      /* SPEED */
      w_speed_form = gtk_hbox_new(FALSE, 2);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->vbox), w_speed_form, FALSE, FALSE, 4);
      set_background(w_speed_form, (ss->sgx)->basic_color);
      
      w_speed_event = gtk_event_box_new();
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed_event, FALSE, FALSE, 4);
      gtk_widget_show(w_speed_event);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_speed_event), "button_press_event", GTK_SIGNAL_FUNC(speed_click_callback), (gpointer)ss);
      set_background(w_speed_event, (ss->sgx)->basic_color);
      
      w_speed_label = gtk_label_new(STR_speed);
      gtk_container_add(GTK_CONTAINER(w_speed_event), w_speed_label);
      gtk_widget_show(w_speed_label);

      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO: w_speed_number = gtk_label_new("1/1"); break;
	case SPEED_CONTROL_AS_SEMITONE: w_speed_number = gtk_label_new("1"); break;
	default:  w_speed_number = gtk_label_new(speed_number_buffer); break;
	}
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed_number, FALSE, FALSE, 0);
      gtk_widget_show(w_speed_number);
      
      w_speed_adj = gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
      w_speed = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_speed_adj));
      gtk_box_pack_start(GTK_BOX(w_speed_form), w_speed, TRUE, TRUE, 4);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_speed_adj), "value_changed", GTK_SIGNAL_FUNC(speed_changed_callback), (gpointer)ss);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_speed), "button_release_event", GTK_SIGNAL_FUNC(speed_release_callback), (gpointer)ss);
      gtk_widget_show(w_speed);
      gtk_widget_show(w_speed_form);


      /* AMP */
      chans = allocate_amps(8);
      for (i = 0; i < chans; i++)
	{
	  w_amp_forms[i] = gtk_hbox_new(FALSE, 2);
	  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->vbox), w_amp_forms[i], FALSE, FALSE, 0);
	  set_background(w_amp_forms[i], (ss->sgx)->basic_color);
      
	  w_amp_events[i] = gtk_event_box_new();
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amp_events[i], FALSE, FALSE, 4);
	  gtk_widget_show(w_amp_events[i]);
	  SG_SIGNAL_CONNECT(GTK_OBJECT(w_amp_events[i]), "button_press_event", GTK_SIGNAL_FUNC(amp_click_callback), (gpointer)ss);
	  set_background(w_amp_events[i], (ss->sgx)->basic_color);
      
	  mus_snprintf(amplab, LABEL_BUFFER_SIZE, "amp %d:", i);
	  w_amp_labels[i] = gtk_label_new(amplab);
	  gtk_container_add(GTK_CONTAINER(w_amp_events[i]), w_amp_labels[i]);
	  set_user_data(GTK_OBJECT(w_amp_events[i]), (gpointer)i);
	  gtk_widget_show(w_amp_labels[i]);
      
	  w_amp_numbers[i] = gtk_label_new(amp_number_buffer);
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amp_numbers[i], FALSE, FALSE, 0);
	  gtk_widget_show(w_amp_numbers[i]);
	  
	  w_amp_adjs[i] = gtk_adjustment_new(0.0, 0.0, 1.0, 0.001, 0.01, .1);
	  w_amps[i] = gtk_hscrollbar_new(GTK_ADJUSTMENT(w_amp_adjs[i]));
	  gtk_box_pack_start(GTK_BOX(w_amp_forms[i]), w_amps[i], TRUE, TRUE, 4);
	  SG_SIGNAL_CONNECT(GTK_OBJECT(w_amp_adjs[i]), "value_changed", GTK_SIGNAL_FUNC(amp_changed_callback), (gpointer)ss);
	  SG_SIGNAL_CONNECT(GTK_OBJECT(w_amps[i]), "button_release_event", GTK_SIGNAL_FUNC(amp_release_callback), (gpointer)ss);
	  set_user_data(GTK_OBJECT(w_amps[i]), (gpointer)i);	  
	  set_user_data(GTK_OBJECT(w_amp_adjs[i]), (gpointer)i);
	  gtk_widget_show(w_amps[i]);
      
	  gtk_widget_show(w_amp_forms[i]);
	}

      /* GRAPH */
      w_env_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(mix_panel)->vbox), w_env_frame, TRUE, TRUE, 10);
      
      w_env = gtk_drawing_area_new();
      gtk_widget_set_events(w_env, GDK_ALL_EVENTS_MASK);
      gtk_container_add(GTK_CONTAINER(w_env_frame), w_env);
      set_background(w_env, (ss->sgx)->basic_color);
      gtk_widget_show(w_env);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_env), "expose_event", GTK_SIGNAL_FUNC(mix_amp_env_expose_callback), (gpointer)ss);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_env), "configure_event", GTK_SIGNAL_FUNC(mix_amp_env_resize_callback), (gpointer)ss);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_env), "button_press_event", GTK_SIGNAL_FUNC(mix_drawer_button_press), (gpointer)ss);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_env), "button_release_event", GTK_SIGNAL_FUNC(mix_drawer_button_release), (gpointer)ss);
      SG_SIGNAL_CONNECT(GTK_OBJECT(w_env), "motion_notify_event", GTK_SIGNAL_FUNC(mix_drawer_button_motion), (gpointer)ss);
      gtk_widget_show(w_env_frame);

      gtk_widget_show(mix_panel);
      set_dialog_widget(ss, MIX_PANEL_DIALOG, mix_panel);

      for (i = 0; i < chans; i++) spfs[i] = new_env_editor();

    }
  else raise_dialog(mix_panel);

  update_mix_panel(current_mix_id(ss));
  return(mix_panel);
}

static void update_mix_panel(int mix_id) 
{
  snd_state *ss;
  chan_info *cp;
  int i, chans;
  off_t beg, len;
  Float val;
  char lab[LABEL_BUFFER_SIZE];
  ss = get_global_state();
  if (mix_id == INVALID_MIX_ID) return;
  if (mix_id == current_mix_id(ss))
    {
      if (mix_panel == NULL) 
	make_mix_panel(get_global_state());
      
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

      gtk_entry_set_text(GTK_ENTRY(w_name), mix_name_from_id(mix_id));

      beg = mix_position_from_id(mix_id);
      len = mix_length(mix_id);
      mus_snprintf(lab, LABEL_BUFFER_SIZE, "%.3f : %.3f",
		   (float)((double)beg / (float)SND_SRATE(cp->sound)),
		   (float)((double)(beg + len) / (float)SND_SRATE(cp->sound)));
      gtk_entry_set_text(GTK_ENTRY(w_beg), lab);

      chans = mix_input_chans_from_id(mix_id);
      if (chans > 8) chans = 8;

      for (i = 0; i < chans; i++)
	{
	  gtk_widget_show(w_amp_labels[i]);	  
	  gtk_widget_show(w_amp_numbers[i]);	  
	  gtk_widget_show(w_amps[i]);
	  gtk_widget_show(w_amp_forms[i]);
	  val = mix_amp_from_id(mix_id, i);
	  if (val != current_amps[i])
	    {
	      GTK_ADJUSTMENT(w_amp_adjs[i])->value = amp_to_scroll(val);
	      gtk_adjustment_value_changed(GTK_ADJUSTMENT(w_amp_adjs[i]));
	      current_amps[i] = val;
	    }
	}

      for (i = chans; i < chans_allocated; i++)
	{
	  gtk_widget_hide(w_amp_labels[i]);	  
	  gtk_widget_hide(w_amp_numbers[i]);	  
	  gtk_widget_hide(w_amps[i]);
	  gtk_widget_hide(w_amp_forms[i]);
	}

      mix_amp_env_resize(w_env, ss);

    }
}



void reflect_mix_in_mix_panel(int mix_id)
{
  if ((mix_panel) &&
      (GTK_WIDGET_VISIBLE(mix_panel)) &&
      (current_mix_id(get_global_state()) == mix_id))
    update_mix_panel(mix_id);
}

void reflect_no_mix_in_mix_panel(void)
{
  if ((mix_panel) &&
      (GTK_WIDGET_VISIBLE(mix_panel)))
    gtk_widget_hide(mix_panel);
}
