#include "snd.h" 

/* ---------------- yes or no ---------------- */

static bool yes_or_no = false;
static GtkWidget *yes_or_no_dialog = NULL;
static GtkWidget *yn_label;
static GtkWidget *yes_button, *no_button;

static void yes_callback(GtkWidget *w, gpointer context) {gtk_widget_hide(yes_or_no_dialog); yes_or_no = true;}
static void no_callback(GtkWidget *w, gpointer context) {gtk_widget_hide(yes_or_no_dialog); yes_or_no = false;}

static gint delete_yes_or_no_dialog(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  gtk_widget_hide(yes_or_no_dialog); 
  yes_or_no = true;
  return(true);
}

#define YES_OR_NO_BUFFER_SIZE 1024

bool snd_yes_or_no_p(const char *format, ...)
{
  char *yes_buf;
#if HAVE_VPRINTF
  va_list ap;
  yes_buf = (char *)CALLOC(YES_OR_NO_BUFFER_SIZE, sizeof(char));
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(yes_buf, YES_OR_NO_BUFFER_SIZE, format, ap);
#else
  vsprintf(yes_buf, format, ap);
#endif
  va_end(ap);
#else
  yes_buf = (char *)CALLOC(256, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(yes_buf, YES_OR_NO_BUFFER_SIZE, "%s...[you need vprintf]", format);
#else
  sprintf(yes_buf, "%s...[you need vprintf]", format);
#endif
#endif

  yes_or_no = false;
  if (!yes_or_no_dialog)
    {
      yes_or_no_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(yes_or_no_dialog, "delete_event", delete_yes_or_no_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(yes_or_no_dialog), _("Yow!"));
      sg_make_resizable(yes_or_no_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(yes_or_no_dialog), 4);
      gtk_widget_realize(yes_or_no_dialog);
      gtk_window_resize(GTK_WINDOW(yes_or_no_dialog), 180, 100);

      yes_button = gtk_button_new_from_stock(GTK_STOCK_YES);
      gtk_widget_set_name(yes_button, "doit_button");

      no_button = gtk_button_new_from_stock(GTK_STOCK_NO);
      gtk_widget_set_name(no_button, "quit_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(yes_or_no_dialog)->action_area), yes_button, false, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(yes_or_no_dialog)->action_area), no_button, false, true, 10);
      SG_SIGNAL_CONNECT(yes_button, "clicked", yes_callback, NULL);
      SG_SIGNAL_CONNECT(no_button, "clicked", no_callback, NULL);
      gtk_widget_show(yes_button);
      gtk_widget_show(no_button);
  
      yn_label = gtk_label_new(yes_buf);
      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(yes_or_no_dialog)->vbox), yn_label);

      gtk_widget_show(yn_label);
      set_dialog_widget(YES_OR_NO_DIALOG, yes_or_no_dialog);
    }
  else gtk_label_set_text(GTK_LABEL(yn_label), yes_buf);
  gtk_widget_show(yes_or_no_dialog);
  ss->error_lock = true;
  while ((GTK_WIDGET_VISIBLE(yes_or_no_dialog))  && (ss->error_lock))
    check_for_event();
  ss->error_lock = false;
  if (GTK_WIDGET_VISIBLE(yes_or_no_dialog))
    gtk_widget_hide(yes_or_no_dialog);
  FREE(yes_buf);
  return(yes_or_no);
}

