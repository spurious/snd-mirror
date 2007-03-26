#include "snd.h"

/* vu meters white=reading/yellow=recording/red=clipped bgs, needle
 * output file widget
 * buttons ("Record/Stop" to turn on off)
 * current vu sizes: 100x40 200x70 300x90 400x110
 * perhaps buttons for chans/srate?
 */


static GtkWidget *recorder = NULL;
static bool reading = false, recording = false;
static char *recorder_output_filename = NULL;
static int recorder_fd = -1, recorder_srate = 44100, recorder_chans = 2, recorder_format = MUS_LFLOAT;
static off_t recorder_total_bytes = 0;


static void stop_recording(void)
{
  recording = false;
  mus_sound_close_output(recorder_fd, recorder_total_bytes);
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
  fprintf(stderr,"%.3f ", maxes[0]);
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

  mus_audio_mixer_read(MUS_AUDIO_DEFAULT, MUS_AUDIO_SRATE, MIXER_SIZE, mixer_vals);
  recorder_srate = (int)mixer_vals[0];
  /* TODO: can srate return be a list -- choose 44100 if possible */
  if (recorder_srate <= 0)
    {
      fprintf(stderr,"srate: %d?\n", recorder_srate);
      reading = false;
      return;
    }

  mus_audio_mixer_read(MUS_AUDIO_DEFAULT, MUS_AUDIO_FORMAT, MIXER_SIZE, mixer_vals);  
  /* TODO: get best */
  recorder_format = MUS_LFLOAT;

  /* TODO: buffer size */
  buffer_size = 256;
  
  input_device = mus_audio_open_input(MUS_AUDIO_DEFAULT, recorder_srate, recorder_chans, recorder_format, buffer_size);
  if (input_device < 0)
    {
      fprintf(stderr,"open chans: %d, srate: %d, format: %s, size: %d -> %d\n", 
	      recorder_chans, recorder_srate, mus_data_format_short_name(recorder_format), buffer_size, input_device);
      reading = false;
      return;
    }

  maxes = (Float *)CALLOC(recorder_chans, sizeof(Float));
  inbuf = (unsigned char *)CALLOC(buffer_size, sizeof(unsigned char));
  /* TODO: do all the reads return char? */

  fprintf(stderr,"reading...\n");
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

      set_dialog_widget(RECORDER_DIALOG, recorder);


      
    }
  gtk_widget_show(recorder);
  start_reading();
  return(recorder);
}


