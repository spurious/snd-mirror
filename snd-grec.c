#include "snd.h"

static GtkWidget *recorder = NULL, *meters = NULL, *record_button = NULL, *recorder_output = NULL;
static bool reading = false, recording = false;
static char *recorder_filename = "test.snd";
static int recorder_fd = -1, recorder_srate = 44100, recorder_chans = 2, recorder_format = MUS_LFLOAT;
static off_t recorder_total_bytes = 0;
static axis_context *recorder_ax = NULL;
static int meter_width = 0, meter_height = 0, meters_width = 0;
static bool meters_in_db = false;


static void stop_recording(void)
{
  recording = false;
  mus_sound_close_output(recorder_fd, recorder_total_bytes);
  recorder_fd = -1;
  recorder_total_bytes = 0;
  set_stock_button_label(record_button, _("Record"));
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


static void display_meters(Float *maxes)
{
  int i, x0 = 0;
  /* if maxes is NULL, assume all 0's */
  if (recorder_chans == 0) return;
#if USE_CAIRO
  {
    cairo_t *cr;
    cr = gdk_cairo_create(recorder_ax->wn);
    
    cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
    cairo_rectangle(cr, 0, 0, meters_width, meter_height);
    cairo_fill(cr);
    
    cairo_save(cr);
    
    for (i = 0; i < recorder_chans; i++, x0 += meter_width)
      {
	Float cur_max = 0.0;
	if (maxes) 
	  {
	    if (!meters_in_db)
	      cur_max = maxes[i];
	    else
	      {
		Float dv;
		dv = in_dB(min_dB(ss), ss->lin_dB, maxes[i]);
		cur_max = 1.0 +  ((dv < -30.0) ? -30.0 : dv) / 30.0;
	      }
	  }

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
    cairo_destroy(cr);
  }
#else
  gdk_gc_set_background(recorder_ax->gc, ss->sgx->white);
  gdk_gc_set_foreground(recorder_ax->gc, ss->sgx->white);
  gdk_draw_rectangle(recorder_ax->wn, recorder_ax->gc, true, 0, 0, meters_width, meter_height);

  gdk_gc_set_line_attributes(recorder_ax->gc, 2, GDK_LINE_SOLID, GDK_CAP_ROUND, GDK_JOIN_ROUND);
  gdk_gc_set_foreground(recorder_ax->gc, ss->sgx->black);

  for (i = 0; i < recorder_chans; i++, x0 += meter_width)
    {
      Float cur_max = 0.0, rads;
      int xc, yc;
      if (maxes) 
	{
	  if (!meters_in_db)
	    cur_max = maxes[i];
	  else
	    {
	      Float dv;
	      dv = in_dB(min_dB(ss), ss->lin_dB, maxes[i]);
	      cur_max = 1.0 +  ((dv < -30.0) ? -30.0 : dv) / 30.0;
	    }
	}

      rads = (M_PI * 0.5 * cur_max) - (M_PI / 4);
      xc = (int)(x0 + 0.5 * meter_width);
      yc = (int)(0.5 * meter_width + 0.2 * meter_height);

      gdk_draw_arc(recorder_ax->wn, recorder_ax->gc, false, x0, 20, meter_width, meter_width, 45 * 64, 90 * 64);
      gdk_draw_line(recorder_ax->wn, recorder_ax->gc, xc, yc, 
		    (int)(xc + 0.55 * meter_width * sin(rads)),
		    (int)(yc - 0.55 * meter_width * cos(rads)));
    }
#endif
}


static void start_recording(void)
{
  recorder_filename = (char *)gtk_entry_get_text(GTK_ENTRY(recorder_output));
  if (recorder_filename == NULL) recorder_filename = copy_string("test.snd");
  recorder_fd = mus_sound_open_output(recorder_filename, recorder_srate, recorder_chans, recorder_format, MUS_NEXT, NULL);
  if (recorder_fd < 0)
    {
      fprintf(stderr,"open output file chans: %d, srate: %d, format: %s -> %d\n", 
	      recorder_chans, recorder_srate, mus_data_format_short_name(recorder_format), recorder_fd);
      recording = false;
    }
  else 
    {
      recording = true;
      set_stock_button_label(record_button, _("Done"));
    }
}


static int look_for_format (float *mixer_vals, int format)
{
  int i, lim;
  lim = (int)mixer_vals[0];
  for (i = 1; i <= lim; i++)
    if ((int)mixer_vals[i] == format)
      return(format);
  return(-1);
}


static void start_reading(void)
{
  Float *maxes;
  #define MIXER_SIZE 8
  float mixer_vals[MIXER_SIZE];
  int input_device, buffer_size, err = MUS_NO_ERROR;
  unsigned char *inbuf;

  mus_audio_mixer_read(MUS_AUDIO_DEFAULT, MUS_AUDIO_CHANNEL, MIXER_SIZE, mixer_vals);
  recorder_chans = (int)mixer_vals[0];
  if (recorder_chans > 4) recorder_chans = 8;
  if (recorder_chans <= 0)
    {
      fprintf(stderr,"chans: %d?\n", recorder_chans);
      reading = false;
      return;
    }

  /*
  mixer_vals[0] = 44100.0;
  mus_audio_mixer_write(MUS_AUDIO_DEFAULT, MUS_AUDIO_SRATE, 0, mixer_vals);
  */
  recorder_srate = 44100;

  mus_audio_mixer_read(MUS_AUDIO_DEFAULT, MUS_AUDIO_FORMAT, MIXER_SIZE, mixer_vals);
#if MUS_LITTLE_ENDIAN
  recorder_format = look_for_format(mixer_vals, MUS_LFLOAT);
  if (recorder_format == -1)
    {
      recorder_format = look_for_format(mixer_vals, MUS_LSHORT);
      if (recorder_format == -1)
	recorder_format = (int)mixer_vals[1];
    }
#else
  recorder_format = look_for_format(mixer_vals, MUS_BFLOAT);
  if (recorder_format == -1)
    {
      recorder_format = look_for_format(mixer_vals, MUS_BSHORT);
      if (recorder_format == -1)
	recorder_format = (int)mixer_vals[1];
    }
#endif

  /* TODO: buffer size */
  buffer_size = 4096;
  
  input_device = mus_audio_open_input(MUS_AUDIO_DEFAULT, recorder_srate, recorder_chans, recorder_format, buffer_size);
  if (input_device < 0)
    {
      fprintf(stderr,"open input failed: chans: %d, srate: %d, format: %s, size: %d -> %d\n", 
	      recorder_chans, recorder_srate, mus_data_format_short_name(recorder_format), buffer_size, input_device);

      /* TODO: try some fallbacks -- different srate/chans etc */
      
      reading = false;
      return;
    }

  maxes = (Float *)CALLOC(recorder_chans, sizeof(Float));
  inbuf = (unsigned char *)CALLOC(buffer_size, sizeof(unsigned char));

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
	    fprintf(stderr, "recorder wrote " SSIZE_TD " bytes of %d requested?", bytes, buffer_size);
	  recorder_total_bytes += buffer_size;
	}
      check_for_event();  /* watch for close event or "go away" clicked */
      if (!reading) break;
      err = mus_samples_peak(inbuf, buffer_size, recorder_chans, recorder_format, maxes);
      if (err != MUS_ERROR) 
	display_meters(maxes);
      else fprintf(stderr,"mus samples peak: %d\n", err);
    }
  if (err != MUS_NO_ERROR)
    fprintf(stderr,"error: %s\n", mus_error_type_to_string(err));
  else fprintf(stderr,"done\n");

  mus_audio_close(input_device);
  FREE(inbuf);
  FREE(maxes);

  /* TODO: if err, report it */
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
  meters_in_db = (bool)(GTK_TOGGLE_BUTTON(w)->active);
}


widget_t record_file(void) 
{
  if (!recorder)
    {
      GtkWidget *help_button, *quit_button, *hbox, *output_label, *db_button;

      recorder = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(recorder, "delete_event", close_recorder, NULL);
      gtk_window_set_title(GTK_WINDOW(recorder), _("Record"));
      sg_make_resizable(recorder);
      gtk_widget_realize(recorder);
      gtk_window_resize(GTK_WINDOW(recorder), 350, 150);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      quit_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(quit_button, "quit_button");
      set_stock_button_label(quit_button, _("Go Away"));

#ifdef GTK_STOCK_MEDIA_RECORD
      record_button = sg_button_new_from_stock_with_label(_("Record"), GTK_STOCK_MEDIA_RECORD);
#else
      record_button = sg_button_new_from_stock_with_label(_("Record"), GTK_STOCK_EXECUTE);
#endif
      gtk_widget_set_name(record_button, "doit_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area), quit_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->action_area), record_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(recorder)->action_area), help_button, true, true, 10);

      SG_SIGNAL_CONNECT(quit_button, "clicked", quit_recorder, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", recorder_help, NULL);
      SG_SIGNAL_CONNECT(record_button, "clicked", start_or_stop_recorder, NULL);

      gtk_widget_show(quit_button);
      gtk_widget_show(record_button);
      gtk_widget_show(help_button);

      meters = gtk_drawing_area_new();
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->vbox), meters, true, true, 8);
      gtk_widget_show(meters);

      hbox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(recorder)->vbox), hbox, false, false, 6);
      gtk_widget_show(hbox);

      output_label = gtk_label_new(_("file:"));
      gtk_box_pack_start(GTK_BOX(hbox), output_label, false, false, 2);
      gtk_widget_show(output_label);
      
      recorder_output = snd_entry_new(hbox, WITH_WHITE_BACKGROUND);
      gtk_entry_set_text(GTK_ENTRY(recorder_output), recorder_filename);

      db_button = gtk_check_button_new_with_label(_("dB"));
      gtk_box_pack_end(GTK_BOX(hbox), db_button, false, false, 10);
      gtk_widget_show(db_button);
      SG_SIGNAL_CONNECT(db_button, "toggled", db_callback, NULL);

      gtk_widget_show(recorder);
      set_dialog_widget(RECORDER_DIALOG, recorder);

      recorder_ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      recorder_ax->wn = meters->window;
#if (!USE_CAIRO)
      recorder_ax->gc = gc_new(GDK_DRAWABLE(meters->window));
      gc_set_background(recorder_ax->gc, ss->sgx->white);
      gc_set_foreground(recorder_ax->gc, ss->sgx->black);
      gc_set_function(recorder_ax->gc, GDK_COPY);
#endif      

      SG_SIGNAL_CONNECT(meters, "expose_event", meters_resize, NULL);
      SG_SIGNAL_CONNECT(meters, "configure_event", meters_resize, NULL);
    }
  else gtk_widget_show(recorder);
  start_reading();
  return(recorder);
}


/* TODO: doc new rec */
