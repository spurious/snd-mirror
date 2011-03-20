#include "snd.h"

/* are the printed graphs truncated because the "printer" assumes some paper size? 
 */

static GtkPrintSettings *settings = NULL;

static void begin_print(GtkPrintOperation *operation, GtkPrintContext *context,	gpointer data)
{
  gtk_print_operation_set_n_pages(operation, 1);
}


static void draw_page(GtkPrintOperation *operation, GtkPrintContext *context, gint page_num, gpointer data)
{
  chan_info *cp;

  ss->cr = gtk_print_context_get_cairo_context(context);

  switch (ss->print_choice)
    {
    case PRINT_SND:
      cp = selected_channel();
      display_channel_data_for_print(cp);
      break;

    case PRINT_ENV:
      env_redisplay_with_print();
      break;
    }
}


static void end_print(GtkPrintOperation *operation, GtkPrintContext *context, gpointer data)
{
  ss->cr = NULL;
}


void file_print_callback(GtkWidget *w, gpointer info) 
{
  /* called from File:Print in main menu */ 
  GtkPrintOperation *operation;
  GtkPrintOperationResult res;
  GError *error = NULL;

  operation = gtk_print_operation_new ();
  if (settings != NULL) 
    gtk_print_operation_set_print_settings(operation, settings);

  g_signal_connect(G_OBJECT(operation), "begin-print", G_CALLBACK(begin_print), NULL);
  g_signal_connect(G_OBJECT(operation), "draw-page", G_CALLBACK(draw_page), NULL);
  g_signal_connect(G_OBJECT(operation), "end-print", G_CALLBACK(end_print), NULL);

  res = gtk_print_operation_run(operation, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, GTK_WINDOW(MAIN_SHELL(ss)), &error);
  if (res == GTK_PRINT_OPERATION_RESULT_APPLY)
    {
      if (settings != NULL)
        g_object_unref(settings);
      settings = (GtkPrintSettings *)g_object_ref(gtk_print_operation_get_print_settings(operation));
    }
  g_object_unref(operation);

  if (error)
    {
      snd_warning_without_format(error->message);
      g_error_free (error);
    }
}


widget_t make_file_print_dialog(bool managed, bool direct_to_printer) 
{
  /* xen print-dialog code */ 
  file_print_callback(NULL, NULL);
  return(NULL);
}
