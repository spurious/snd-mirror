#include "snd.h" 


void post_error_dialog(char *msg)
{
  post_it("Error", msg);
}


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

/* ---------------- POST-IT MONOLOG ---------------- */

#define POST_IT_ROWS 12
#define POST_IT_COLUMNS 56

static GtkWidget *post_it_text = NULL, *post_it_dialog = NULL;

static void dismiss_post_it(GtkWidget *w, gpointer context) {gtk_widget_hide(post_it_dialog);}

static gint delete_post_it(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  gtk_widget_hide(post_it_dialog);
  return(true);
}

static void create_post_it_monolog(void)
{
  /* create scrollable but not editable text window */
  GtkWidget *ok_button;
  post_it_dialog = gtk_dialog_new();
  SG_SIGNAL_CONNECT(post_it_dialog, "delete_event", delete_post_it, NULL);

  gtk_window_set_title(GTK_WINDOW(post_it_dialog), _("Info"));
  sg_make_resizable(post_it_dialog);
  gtk_container_set_border_width(GTK_CONTAINER(post_it_dialog), 10);
  gtk_window_resize(GTK_WINDOW(post_it_dialog), POST_IT_COLUMNS * 9, POST_IT_ROWS * 20);
  gtk_widget_realize(post_it_dialog);

  ok_button = gtk_button_new_from_stock(GTK_STOCK_OK);
  gtk_widget_set_name(ok_button, "quit_button");

  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(post_it_dialog)->action_area), ok_button, false, true, 20);
  SG_SIGNAL_CONNECT(ok_button, "clicked", dismiss_post_it, NULL);
  gtk_widget_show(ok_button);

  post_it_text = make_scrolled_text(GTK_DIALOG(post_it_dialog)->vbox, false, NULL, NULL);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(post_it_text), 10);
  gtk_widget_show(post_it_dialog);
  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}

widget_t post_it(const char *subject, const char *str)
{
  if ((ss == NULL) || (ss->sgx == NULL)) return(NULL);
  if (!(post_it_dialog)) create_post_it_monolog(); else raise_dialog(post_it_dialog);
  gtk_window_set_title(GTK_WINDOW(post_it_dialog), subject);
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(post_it_text)), "", 0);
  sg_text_insert(post_it_text, (char *)str);
  return(post_it_dialog);
}

void save_post_it_dialog_state(FILE *fd)
{
  if ((post_it_dialog) && (GTK_WIDGET_VISIBLE(post_it_dialog)))
    {
      const gchar *subject;
      gchar *text;
      subject = gtk_window_get_title(GTK_WINDOW(post_it_dialog)); /* don't free subject! */
      text = sg_get_text(post_it_text, 0, -1);
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\" \"%s\")\n", S_info_dialog, subject, text);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\", \"%s\")\n", TO_PROC_NAME(S_info_dialog), subject, text);
#endif
      if (text) g_free(text);
    }
}

