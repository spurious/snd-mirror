#include "snd.h"

static GtkWidget *help_dialog = NULL;
static char help_window_label[LABEL_BUFFER_SIZE];

static void dismiss_help(GtkWidget *w, gpointer context) {gtk_widget_hide(help_dialog);}
static void delete_help(GtkWidget *w, GdkEvent *event, gpointer context) {gtk_widget_hide(help_dialog);}


/* ---------------- HELP MONOLOG ---------------- */

#define HELP_ROWS 12
#define HELP_COLUMNS 56
/* these set the initial size of the (non XmHTML) help dialog text area */

static GtkWidget *help_text = NULL;

static void add_help_text (GtkWidget *text, const char *message)
{
  sg_text_insert(text, (char *)message);
}

static char *cr_to_space(char *val)
{
  int i, len;
  if (val)
    {
      len = strlen(val);
      for (i = 0; i < len; i++)
	if (val[i] == '\n')
	  val[i] = ' ';
    }
  return(val);
}

static bool no_cr(const char *val)
{
  int i, len;
  if (val)
    {
      len = strlen(val);
      for (i = 0; i < len; i++)
	if (val[i] == '\n')
	  return(false);
    }
  return(true);
}

static int help_text_width = 0;
static bool outer_with_wrap = false;

static gboolean help_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  int curwid;
  curwid = widget_width(help_text);
  if (help_text_width == 0)
    help_text_width = curwid;
  else
    {
      if ((outer_with_wrap) && (abs(curwid - help_text_width) > 10))
	{
	  char *cur_help = NULL;
	  char *new_help = NULL;
	  int end;
	  GtkTextIter s, e;
	  GtkTextBuffer *buf;
	  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text));
	  end = gtk_text_buffer_get_char_count(buf);
	  gtk_text_buffer_get_iter_at_offset(buf, &s, 0);
	  gtk_text_buffer_get_iter_at_offset(buf, &e, end);
	  cur_help = cr_to_space(gtk_text_buffer_get_text(buf, &s, &e, true));
	  new_help = word_wrap(cur_help, curwid);
	  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text)), "", 0);
	  sg_text_insert(help_text, new_help);
	  if (new_help) FREE(new_help);
	  if (cur_help) g_free(cur_help);
	  help_text_width = curwid;
	}
    }
  return(false);
}

static void create_help_monolog(void)
{
  /* create scrollable but not editable text window */
  GtkWidget *ok_button;
  help_dialog = gtk_dialog_new();
  g_signal_connect_closure_by_id(GTK_OBJECT(help_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(help_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(delete_help), NULL, 0),
				 0);

  g_signal_connect_closure_by_id(GTK_OBJECT(GTK_OBJECT(help_dialog)),
				 g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(help_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_expose_callback), NULL, 0),
				 0);

  gtk_window_set_title(GTK_WINDOW(help_dialog), _("Help"));
  sg_make_resizable(help_dialog);
  gtk_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_window_resize(GTK_WINDOW(help_dialog), HELP_COLUMNS * 9, HELP_ROWS * 20);
  gtk_widget_realize(help_dialog);

  ok_button = gtk_button_new_with_label(_("Ok"));
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), ok_button, false, true, 20);
  g_signal_connect_closure_by_id(GTK_OBJECT(ok_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(ok_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_help), NULL, 0),
				 0);
  gtk_widget_show(ok_button);

  help_text = make_scrolled_text(GTK_DIALOG(help_dialog)->vbox, false, NULL, NULL);
  gtk_widget_modify_base(help_text, GTK_STATE_NORMAL, ss->sgx->white);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(help_text), 10);
  gtk_widget_show(help_dialog);
  set_dialog_widget(HELP_DIALOG, help_dialog);
}

GtkWidget *snd_help(const char *subject, const char *helpstr, bool with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  outer_with_wrap = ((with_wrap) && (no_cr(helpstr)));
  if (!(help_dialog)) create_help_monolog(); else raise_dialog(help_dialog);
  mus_snprintf(help_window_label, LABEL_BUFFER_SIZE, _("%s help"), subject);
  gtk_window_set_title(GTK_WINDOW(help_dialog), help_window_label);
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text)), "", 0);
  if (with_wrap)
    {
      char *new_help = NULL;
      new_help = word_wrap(helpstr, (int)(widget_width(help_text) * 1.3));
      add_help_text(help_text, new_help);
      if (new_help) FREE(new_help);
    }
  else add_help_text(help_text, helpstr);
  return(help_dialog);
}
