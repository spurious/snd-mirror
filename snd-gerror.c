#include "snd.h" 

/* error handlers -- these include the error dialog (in case no sound is active) and an error history list */

static GtkWidget *snd_error_dialog = NULL;
static GtkWidget *snd_error_history = NULL;

static void dismiss_snd_error(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(snd_error_dialog);
}

static void delete_snd_error(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(snd_error_dialog);
}

static void create_snd_error_dialog(bool popup)
{
  GtkWidget *ok_button;
  snd_error_dialog = snd_gtk_dialog_new();
  g_signal_connect_closure_by_id(GTK_OBJECT(snd_error_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(snd_error_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(delete_snd_error), NULL, 0),
				 0);
  gtk_window_set_title(GTK_WINDOW(snd_error_dialog), _("Error"));
  sg_make_resizable(snd_error_dialog);
  gtk_container_set_border_width (GTK_CONTAINER(snd_error_dialog), 10);
  gtk_window_resize(GTK_WINDOW(snd_error_dialog), 400, 300);
  gtk_widget_realize(snd_error_dialog);

  ok_button = gtk_button_new_with_label(_("Ok"));
  gtk_widget_set_name(ok_button, "quit_button");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(snd_error_dialog)->action_area), ok_button, false, true, 20);
  g_signal_connect_closure_by_id(GTK_OBJECT(ok_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(ok_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_snd_error), NULL, 0),
				 0);
  gtk_widget_show(ok_button);
  snd_error_history = make_scrolled_text(GTK_DIALOG(snd_error_dialog)->vbox, false, NULL, NULL);
  if (popup) gtk_widget_show(snd_error_dialog);
  set_dialog_widget(ERROR_DIALOG, snd_error_dialog);
}

void add_to_error_history(char *msg, bool popup)
{
#if HAVE_STRFTIME
  char *tim, *buf;
  time_t ts;
#endif
  int pos;
  if (!snd_error_dialog) 
    create_snd_error_dialog(popup);
  else
    if ((popup) && (!(GTK_WIDGET_VISIBLE(snd_error_dialog))))
      gtk_widget_show(snd_error_dialog);
  pos = gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(snd_error_history)));
  if (pos > 0) sg_set_cursor(snd_error_history, pos + 1);
#if HAVE_STRFTIME
  tim = (char *)CALLOC(TIME_STR_SIZE, sizeof(char));
  buf = (char *)CALLOC(TIME_STR_SIZE, sizeof(char));
  time(&ts);
  strftime(tim, TIME_STR_SIZE, "%H:%M:%S", localtime(&ts));
  sprintf(buf, "\n[%s] ", tim);
  sg_text_insert(snd_error_history, buf);
  FREE(buf);
  FREE(tim);
#endif
  sg_text_insert(snd_error_history, msg);
}

void post_error_dialog(char *msg)
{
  if (!snd_error_dialog) create_snd_error_dialog(true); else raise_dialog(snd_error_dialog);
  add_to_error_history(msg, true);
}

void show_snd_errors(void)
{
  if (snd_error_dialog)
    {
      if (GTK_WIDGET_VISIBLE(snd_error_dialog))
	raise_dialog(snd_error_dialog);
      gtk_widget_show(snd_error_dialog);
    }
  else post_error_dialog(_("no errors yet"));
}

static bool yes_or_no = false;
static GtkWidget *yes_or_no_dialog = NULL;
static GtkWidget *yn_label;
static GtkWidget *yes_button, *no_button;

static void yes_callback(GtkWidget *w, gpointer context) {gtk_widget_hide(yes_or_no_dialog); yes_or_no = true;}
static void no_callback(GtkWidget *w, gpointer context) {gtk_widget_hide(yes_or_no_dialog); yes_or_no = false;}
static void delete_yes_or_no_dialog(GtkWidget *w, GdkEvent *event, gpointer context) {gtk_widget_hide(yes_or_no_dialog); yes_or_no = true;}

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
      g_signal_connect_closure_by_id(GTK_OBJECT(yes_or_no_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(yes_or_no_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(delete_yes_or_no_dialog), NULL, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(yes_or_no_dialog), _("Yow!"));
      sg_make_resizable(yes_or_no_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(yes_or_no_dialog), 4);
      gtk_widget_realize(yes_or_no_dialog);
      gtk_window_resize(GTK_WINDOW(yes_or_no_dialog), 180, 100);

      yes_button = gtk_button_new_with_label(_("Yes"));
      gtk_widget_set_name(yes_button, "doit_button");
      no_button = gtk_button_new_with_label(_("No"));
      gtk_widget_set_name(no_button, "quit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(yes_or_no_dialog)->action_area), yes_button, false, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(yes_or_no_dialog)->action_area), no_button, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(yes_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(yes_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(yes_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(no_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(no_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(no_callback), NULL, 0),
				     0);
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

