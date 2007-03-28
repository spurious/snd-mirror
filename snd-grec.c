#include "snd.h"

/* vu meters white=reading/yellow=recording/red=clipped bgs, needle
 * output file widget
 * buttons ("Record/Stop" to turn on off)
 * current vu sizes: 100x40 200x70 300x90 400x110
 * perhaps buttons for chans/srate?
 */


static GtkWidget *recorder = NULL, *meters = NULL;
static bool reading = false, recording = false;
static char *recorder_output_filename = NULL;
static int recorder_fd = -1, recorder_srate = 44100, recorder_chans = 2, recorder_format = MUS_LFLOAT;
static off_t recorder_total_bytes = 0;
static axis_context *recorder_ax = NULL;


static void stop_recording(void)
{
  recording = false;
  mus_sound_close_output(recorder_fd, recorder_total_bytes);
  recorder_fd = -1;
  recorder_total_bytes = 0;
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

static void display_meters(Float *maxes, int chans)
{
  int i;
  /* expose/resize callback to choose size, here we xor out the previous meters/bubble, add new */
}

static void start_recording(void)
{
  if (recorder_output_filename == NULL) recorder_output_filename = copy_string("test.snd");
  recorder_fd = mus_sound_open_output(recorder_output_filename, recorder_srate, recorder_chans, recorder_format, MUS_NEXT, NULL);
  if (recorder_fd < 0)
    {
      fprintf(stderr,"open output file chans: %d, srate: %d, format: %s -> %d\n", 
	      recorder_chans, recorder_srate, mus_data_format_short_name(recorder_format), recorder_fd);
      recording = false;
    }
  else recording = true;
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

  fprintf(stderr,"reading...\n");
  reading = true;
  while (true)
    {
      err = mus_audio_read(input_device, (char *)inbuf, buffer_size);
      if (err != MUS_NO_ERROR) break;
      if (recording)
	{
	  write(recorder_fd, (char *)inbuf, buffer_size);
	  recorder_total_bytes += buffer_size;
	}
      check_for_event();  /* watch for close event or "go away" clicked */
      if (!reading) break;
      err = mus_samples_peak(inbuf, buffer_size, recorder_chans, recorder_format, maxes);
      if (err != MUS_ERROR) 
	display_meters(maxes, recorder_chans);
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

  /* if recording button="Stop" else button="Record" */
}

static void draw_meter(axis_context *ax, GdkPixbuf *pix, int x, int y)
{
#if USE_CAIRO
  cairo_t *cr;
  cr = gdk_cairo_create(ax->wn);
  gdk_cairo_set_source_pixbuf(cr, pix, x, y);
  cairo_paint(cr);
  cairo_destroy(cr);
#else
#if HAVE_GDK_DRAW_PIXBUF
  gdk_draw_pixbuf(ax->wn, ax->gc, pix, 0, 0, x, y, gdk_pixbuf_get_width(pix), gdk_pixbuf_get_height(pix), GDK_RGB_DITHER_NONE, 0, 0); 
#endif
#endif
}

static gboolean meters_resize(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  int i, x = 0, width, meter_width;
  width = widget_width(meters);
  meter_width = width / recorder_chans;
  /* gdk_pixbuf_new_from_file(name, &error) */

  /* if not ax ... */
  /* draw_meter(recorder_ax, meter, x, 0); */

  return(false);
}

static gboolean meters_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{

  return(false);
}


widget_t record_file(void) 
{
  if (!recorder)
    {
      GtkWidget *record_button, *help_button, *quit_button;

      recorder = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(recorder, "delete_event", close_recorder, NULL);
      gtk_window_set_title(GTK_WINDOW(recorder), _("Record"));
      sg_make_resizable(recorder);
      /* gtk_widget_realize(recorder); */ /* why was this needed before? */

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      quit_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(quit_button, "quit_button");

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


      gtk_widget_show(recorder);
      set_dialog_widget(RECORDER_DIALOG, recorder);


      SG_SIGNAL_CONNECT(meters, "expose_event", meters_expose, NULL);
      SG_SIGNAL_CONNECT(meters, "configure_event", meters_resize, NULL);

      
    }
  else gtk_widget_show(recorder);
  start_reading();
  return(recorder);
}


