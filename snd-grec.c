#include "snd.h"

/* vu meters (click=on?) white/yellow/red bgs, needle
 *    needle max without data conversion
 * output file widget
 * buttons ("Record/Stop" to turn on off)
 *
 * use whatever input device is the default, using all its current settings
 *   write data as received
 * local check_for_event, no global bg func
 *
 * current vu sizes: 100x40 200x70 300x90 400x110
 */

static GtkWidget *recorder = NULL;

static gint close_recorder(GtkWidget *w, GdkEvent *event, gpointer context)
{
  /* window manager close button */
  return(true);
}

static void quit_recorder(GtkWidget *w, gpointer context) 
{
  /* Quit button in the recorder dialog */
}

static void recorder_help(GtkWidget *w, gpointer context) 
{
  recording_help();
}

static void start_or_stop_recorder(GtkWidget *w, gpointer context) 
{
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



      gtk_widget_show(recorder);
    }
  return(recorder);
}


