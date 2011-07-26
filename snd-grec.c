#include "snd.h"

static GtkWidget *recorder = NULL, *meters = NULL, *record_button = NULL, *recorder_output = NULL, *info;
static bool reading = false, recording = false;
static char *recorder_filename = NULL;
static int recorder_fd = -1, recorder_srate = 44100, recorder_chans = 2, recorder_format = MUS_LFLOAT;
static mus_long_t recorder_total_bytes = 0;
static graphics_context *recorder_ax = NULL;
static int meter_width = 0, meter_height = 0, meters_width = 0;
static bool meters_in_db = false;

#define RECORDER_WIDTH 300
#define RECORDER_HEIGHT 200

static bool recorder_watching = false;
static gulong recorder_text_id = 0;

static char *base_message(void)
{
  return(mus_format("srate: %d, chans: %d", recorder_srate, recorder_chans));
}


static void clear_error(void)
{
  char *msg;
  msg = base_message();
  info_widget_display(info, msg);
  free(msg);
  widget_modify_base(info, GTK_STATE_NORMAL, ss->basic_color);
  widget_modify_base(info, GTK_STATE_ACTIVE, ss->basic_color);
  if (recorder_watching)
    {
      recorder_watching = false;
      g_signal_handler_disconnect(recorder_output, recorder_text_id);
      recorder_text_id = 0;
    }
}


static void watch_info(GtkWidget *w, gpointer context)
{
  clear_error();
}


static void report_in_error_info(const char *msg, void *ignore)
{
  info_widget_display(info, msg);
  widget_modify_base(info, GTK_STATE_NORMAL, ss->highlight_color);
  widget_modify_base(info, GTK_STATE_ACTIVE, ss->highlight_color);
  /*   info_widget_set_size(info, 1 + mus_strlen(msg)); */
  recorder_watching = true;
  recorder_text_id = SG_SIGNAL_CONNECT(recorder_output, "changed", watch_info, NULL);
}


static void stop_recording(void)
{
  clear_error();
  recording = false;
  mus_sound_close_output(recorder_fd, recorder_total_bytes);
  recorder_fd = -1;
  recorder_total_bytes = 0;
  set_stock_button_label(record_button, "Record");
}


static gint close_recorder(GtkWidget *w, GdkEvent *event, gpointer context)
{
  /* window manager close button */
  if (recording)
    stop_recording();
  reading = false;
  gtk_widget_hide(recorder);
  return(true);
}


static void quit_recorder(GtkWidget *w, gpointer context) 
{
  /* Quit button in the recorder dialog */
  if (recording)
    stop_recording();
  reading = false;
  gtk_widget_hide(recorder);
}


static void recorder_help(GtkWidget *w, gpointer context) 
{
  recording_help();
}


static void display_meters(mus_float_t *maxes)
{
  if (recorder_chans > 0)
    {
      cairo_t *cr = NULL;
      int i, x0 = 0;
      /* if maxes is NULL, assume all 0's */
      
      cr = MAKE_CAIRO(recorder_ax->wn);
      cairo_push_group(cr);
      
      cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
      cairo_rectangle(cr, 0, 0, meters_width, meter_height);
      cairo_fill(cr);
      
      for (i = 0; i < recorder_chans; i++, x0 += meter_width)
	{
	  mus_float_t cur_max = 0.0;
	  if (maxes) 
	    {
	      if (!meters_in_db)
		cur_max = maxes[i];
	      else
		{
		  mus_float_t dv;
		  dv = in_dB(min_dB(ss), ss->lin_dB, maxes[i]);
		  cur_max = 1.0 +  ((dv < -30.0) ? -30.0 : dv) / 30.0;
		}
	    }
	  cairo_save(cr);
	  
	  /* put our origin at the meter pivot point scaled (as a square so the dial remains circular) to 0..1 */
	  cairo_translate(cr, x0 + (0.5 * meter_width), 0.5 * meter_width + 0.2 * meter_height);
	  cairo_scale(cr, meter_width, meter_width);
	  
	  cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
	  cairo_set_line_width(cr, 2.0 / (float)meter_width);
	  cairo_arc(cr, 0, 0, 0.5, -0.75 * M_PI, -0.25 * M_PI);
	  cairo_stroke(cr);
	  
	  cairo_rotate(cr, 1.2 * M_PI + cur_max * M_PI * 0.5);
	  cairo_move_to(cr, 0, 0);
	  cairo_rel_line_to(cr, 0.55, 0.0);
	  cairo_stroke(cr);
	  
	  cairo_restore(cr);
	}
      
      cairo_pop_group_to_source(cr);
      cairo_paint(cr);
      FREE_CAIRO(cr);
    }
}


static void start_recording(void)
{
  clear_error();
  recorder_filename = (char *)gtk_entry_get_text(GTK_ENTRY(recorder_output));
  if (recorder_filename == NULL) recorder_filename = mus_strdup("test.snd");
  redirect_snd_error_to(report_in_error_info, NULL);
  recorder_fd = mus_sound_open_output(recorder_filename, recorder_srate, recorder_chans, recorder_format, 
				      (recorder_format == MUS_LFLOAT) ? MUS_RIFF : MUS_NEXT, 
				      NULL);
  redirect_snd_error_to(NULL, NULL);
  if (recorder_fd < 0)
    {
      recording = false;
    }
  else 
    {
      recording = true;
      set_stock_button_label(record_button, "Done");
    }
}


static void start_reading(void)
{
  mus_float_t *maxes;
  int input_device, buffer_size, err = MUS_NO_ERROR;
  unsigned char *inbuf;

  clear_error();

  recorder_chans = mus_audio_device_channels(MUS_AUDIO_DEFAULT);
  if (recorder_chans > 4) recorder_chans = 8;
  if (recorder_chans <= 0)
    {
      char *msg;
      msg = mus_format("chans: %d?", recorder_chans);
      report_in_error_info(msg, NULL);
      free(msg);
      reading = false;
      return;
    }

  recorder_srate = 44100;
  recorder_format = mus_audio_device_format(MUS_AUDIO_DEFAULT);
  buffer_size = 4096;
  
  input_device = mus_audio_open_input(MUS_AUDIO_DEFAULT, recorder_srate, recorder_chans, recorder_format, buffer_size);
  if (input_device < 0)
    {
      char *msg;
      msg = mus_format("open input failed: chans: %d, srate: %d, format: %s, size: %d -> %d", 
		       recorder_chans, recorder_srate, mus_data_format_short_name(recorder_format), buffer_size, input_device);
      report_in_error_info(msg, NULL);
      free(msg);

      reading = false;
      return;
    }

  maxes = (mus_float_t *)calloc(recorder_chans, sizeof(mus_float_t));
  inbuf = (unsigned char *)calloc(buffer_size, sizeof(unsigned char));

  reading = true;
  while (true)
    {
      err = mus_audio_read(input_device, (char *)inbuf, buffer_size);
      if (err != MUS_NO_ERROR) break;
      if (recording)
	{
	  ssize_t bytes;
	  bytes = write(recorder_fd, (char *)inbuf, buffer_size);
	  if (bytes != buffer_size)
	    {
	      char *msg;
	      msg = mus_format("recorder wrote " SSIZE_TD " bytes of %d requested?", bytes, buffer_size);
	      report_in_error_info(msg, NULL);
	      free(msg);
	    }
	  recorder_total_bytes += buffer_size;
	}
      check_for_event();  /* watch for close event or "go away" clicked */
      if (!reading) break;
      err = mus_samples_peak(inbuf, buffer_size, recorder_chans, recorder_format, maxes);
      if (err != MUS_ERROR) 
	display_meters(maxes);
      else
	{
	  char *msg;
	  msg = mus_format("data conversion problem; format is %s\n", mus_data_format_name(recorder_format));
	  report_in_error_info(msg, NULL);
	  free(msg);
	  err = MUS_NO_ERROR;
	  break;
	}
    }
  if (err != MUS_NO_ERROR)
    {
      char *msg;
      msg = mus_format("error: %s", mus_error_type_to_string(err));
      report_in_error_info(msg, NULL);
      free(msg);
    }

  mus_audio_close(input_device);
  free(inbuf);
  free(maxes);
}


static void start_or_stop_recorder(GtkWidget *w, gpointer context) 
{
  if (recording)
    stop_recording();
  else start_recording();
}


static gboolean meters_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  if (recorder_chans == 0) return(false);
  meters_width = widget_width(meters);
  meter_width = meters_width / recorder_chans;
  meter_height = widget_height(meters);
  display_meters(NULL);
  return(false);
}


static void db_callback(GtkWidget *w, gpointer context)
{
  meters_in_db = (bool)(TOGGLE_BUTTON_ACTIVE(w));
}


widget_t record_file(void) 
{
  if (!recorder)
    {
      GtkWidget *help_button, *quit_button, *hbox, *output_label, *db_button;

      recorder = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(recorder, "delete_event", close_recorder, NULL);
      gtk_window_set_title(GTK_WINDOW(recorder), "Record");
      sg_make_resizable(recorder);
      gtk_widget_realize(recorder);
      gtk_window_resize(GTK_WINDOW(recorder), RECORDER_WIDTH, RECORDER_HEIGHT);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "dialog_button");

      quit_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(quit_button, "dialog_button");
      set_stock_button_label(quit_button, "Go Away");

#ifdef GTK_STOCK_MEDIA_RECORD
      record_button = sg_button_new_from_stock_with_label("Record", GTK_STOCK_MEDIA_RECORD);
#else
      record_button = sg_button_new_from_stock_with_label("Record", GTK_STOCK_EXECUTE);
#endif
      gtk_widget_set_name(record_button, "dialog_button");

      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(recorder)), quit_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(recorder)), record_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(recorder)), help_button, true, true, 10);

      SG_SIGNAL_CONNECT(quit_button, "clicked", quit_recorder, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", recorder_help, NULL);
      SG_SIGNAL_CONNECT(record_button, "clicked", start_or_stop_recorder, NULL);

      gtk_widget_show(quit_button);
      gtk_widget_show(record_button);
      gtk_widget_show(help_button);

      meters = gtk_drawing_area_new();
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(recorder)), meters, true, true, 8);
      gtk_widget_show(meters);

      hbox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(recorder)), hbox, false, false, 6);
      gtk_widget_show(hbox);

      output_label = gtk_label_new("file:");
      gtk_box_pack_start(GTK_BOX(hbox), output_label, false, false, 2);
      gtk_widget_show(output_label);
      
      recorder_output = snd_entry_new(hbox, NULL, WITH_WHITE_BACKGROUND);
      if (!recorder_filename) recorder_filename = mus_strdup("test.snd");
      gtk_entry_set_text(GTK_ENTRY(recorder_output), recorder_filename);

      db_button = gtk_check_button_new_with_label("dB");
      gtk_box_pack_end(GTK_BOX(hbox), db_button, false, false, 10);
      gtk_widget_show(db_button);
      SG_SIGNAL_CONNECT(db_button, "toggled", db_callback, NULL);

      {
	GtkWidget *frame, *hbox, *fsep;

	hbox = gtk_hbox_new(false, 0);
	gtk_box_pack_end(GTK_BOX(DIALOG_CONTENT_AREA(recorder)), hbox, false, false, 8);
	gtk_widget_show(hbox);

	fsep = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(hbox), fsep, false, false, 20);
	widget_modify_bg(fsep, GTK_STATE_NORMAL, ss->basic_color);
	gtk_widget_show(fsep);

	frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	/* gtk_container_set_border_width(GTK_CONTAINER(frame), 1); */
	/* widget_modify_bg(frame, GTK_STATE_NORMAL, ss->zoom_color); */
	gtk_box_pack_start(GTK_BOX(hbox), frame, true, true, 0);
	gtk_widget_show(frame);

	info = snd_gtk_entry_label_new(NULL, ss->basic_color);
	gtk_container_add(GTK_CONTAINER(frame), info);
	gtk_widget_show(info);
      }

      gtk_widget_show(recorder);
      set_dialog_widget(RECORDER_DIALOG, recorder);

      recorder_ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      recorder_ax->wn = WIDGET_TO_WINDOW(meters);

      SG_SIGNAL_CONNECT(meters, DRAW_SIGNAL, meters_resize, NULL);
      SG_SIGNAL_CONNECT(meters, "configure_event", meters_resize, NULL);
    }
  else gtk_widget_show(recorder);
  start_reading();
  return(recorder);
}

