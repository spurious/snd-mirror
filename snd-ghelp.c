#include "snd.h"

static GtkWidget *help_dialog = NULL;
static char help_window_label[LABEL_BUFFER_SIZE];

static void help_help_callback(GtkWidget *w, gpointer context) {help_dialog_help((snd_state *)context);}
static void dismiss_help(GtkWidget *w, gpointer context) {gtk_widget_hide(help_dialog);}
static void delete_help(GtkWidget *w, GdkEvent *event, gpointer context) {gtk_widget_hide(help_dialog);}


#if (!HAVE_HTML)

/* ---------------- HELP MONOLOG ---------------- */

#define HELP_ROWS 12
#define HELP_COLUMNS 56
/* these set the initial size of the (non XmHTML) help dialog text area */

static GtkWidget *help_text = NULL;

static void add_help_text (snd_state *ss, GtkWidget *text, char *message)
{
  sg_text_insert(text, message);
}

static void create_help_monolog(snd_state *ss)
{
  /* create scrollable but not editable text window */
  GtkWidget *help_button, *ok_button;
  help_dialog = gtk_dialog_new();
  g_signal_connect_closure_by_id(GTK_OBJECT(help_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(help_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(delete_help), (gpointer)ss, 0),
				 0);
  gtk_window_set_title(GTK_WINDOW(help_dialog), _("Help"));
  sg_make_resizable(help_dialog);
  set_background(help_dialog, (ss->sgx)->basic_color);
  gtk_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_window_resize(GTK_WINDOW(help_dialog), HELP_COLUMNS * 9, HELP_ROWS * 20);
  gtk_widget_realize(help_dialog);

  help_button = gtk_button_new_with_label(_("Help"));
  ok_button = gtk_button_new_with_label(_("Ok"));
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), ok_button, FALSE, TRUE, 20);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), help_button, FALSE, TRUE, 20);
  g_signal_connect_closure_by_id(GTK_OBJECT(ok_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(ok_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_help), (gpointer)ss, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_help_callback), (gpointer)ss, 0),
				 0);
  gtk_widget_show(ok_button);
  gtk_widget_show(help_button);

  help_text = make_scrolled_text(ss, GTK_DIALOG(help_dialog)->vbox, FALSE, NULL, NULL);
  gtk_widget_show(help_dialog);
  set_dialog_widget(ss, HELP_DIALOG, help_dialog);
}

static GtkWidget *snd_help_1(snd_state *ss, char *subject, char *helpstr, int with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  if (!(help_dialog)) create_help_monolog(ss); else raise_dialog(help_dialog);
  mus_snprintf(help_window_label, LABEL_BUFFER_SIZE, _("%s help"), subject);
  gtk_window_set_title(GTK_WINDOW(help_dialog), help_window_label);
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text)), "", 0);
  if (with_wrap)
    {
      char *new_help = NULL;
      new_help = word_wrap(helpstr, (int)(widget_width(help_text) * 1.3));
      add_help_text(ss, help_text, new_help);
      if (new_help) FREE(new_help);
    }
  else add_help_text(ss, help_text, helpstr);
  return(help_dialog);
}

#else

/* HAVE_HTML -- try the mozilla embedded widget -- this taken largely from TestGtkEmbed.cpp from the Mozilla sources */

#include <gtkembedmoz/gtkmozembed.h>
#include <gtk/gtk.h>

static GtkWidget *help_mozilla = NULL;

static void go_back(GtkButton *button, gpointer ignored) {gtk_moz_embed_go_back(GTK_MOZ_EMBED(help_mozilla));}
static void go_forward(GtkButton *button, gpointer ignored) {gtk_moz_embed_go_forward(GTK_MOZ_EMBED(help_mozilla));}

static void create_help_monolog(snd_state *ss)
{
  /* create scrollable but not editable text window */
  GtkWidget *help_button, *ok_button, *back_button, *forward_button;
  help_dialog = gtk_dialog_new();
  g_signal_connect_closure_by_id(GTK_OBJECT(help_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(help_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(delete_help), (gpointer)ss, 0),
				 0);
  gtk_window_set_title(GTK_WINDOW(help_dialog), _("Help"));
  sg_make_resizable(help_dialog);
  set_background(help_dialog, (ss->sgx)->basic_color);
  gtk_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_window_resize(GTK_WINDOW(help_dialog), 500, 500);
  gtk_widget_realize(help_dialog);

  help_button = gtk_button_new_with_label(_("Help"));
  ok_button = gtk_button_new_with_label(_("Ok"));
  back_button = gtk_button_new_with_label(_("Back"));
  forward_button = gtk_button_new_with_label(_("Forward"));
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), ok_button, FALSE, TRUE, 20);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), back_button, FALSE, TRUE, 20);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), forward_button, FALSE, TRUE, 20);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), help_button, FALSE, TRUE, 20);
  g_signal_connect_closure_by_id(GTK_OBJECT(ok_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(ok_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_help), (gpointer)ss, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(back_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(back_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(go_back), (gpointer)ss, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(forward_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(forward_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(go_forward), (gpointer)ss, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_help_callback), (gpointer)ss, 0),
				 0);
  gtk_widget_show(ok_button);
  gtk_widget_show(back_button);
  gtk_widget_show(forward_button);
  gtk_widget_show(help_button);

  help_mozilla = gtk_moz_embed_new();
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(help_dialog)->vbox), help_mozilla);

  gtk_widget_show(help_mozilla);
  gtk_widget_show(help_dialog);
  set_dialog_widget(ss, HELP_DIALOG, help_dialog);
}

static GtkWidget *snd_help_1(snd_state *ss, char *subject, char *helpstr, int with_wrap)
{
  int i, len, lim;
  char *url, *urldir;
  if (!(help_dialog)) create_help_monolog(ss); else raise_dialog(help_dialog);
  mus_snprintf(help_window_label, LABEL_BUFFER_SIZE, _("%s help"), subject);
  gtk_window_set_title(GTK_WINDOW(help_dialog), help_window_label);
  len = snd_strlen(helpstr);
  if (len < 15) lim = len; else lim = 15;
  for (i = 0; i < lim; i++)
    if (helpstr[i] == '#')
      {
	if (snd_strlen(html_dir(ss)) > 0) 
	  urldir = copy_string(html_dir(ss));
	else 
	  {
	    urldir = (char *)CALLOC(256, sizeof(char));
	    urldir = getcwd(urldir, 256);
	  }
	url = (char *)CALLOC(len + snd_strlen(urldir) + 64, sizeof(char));
	sprintf(url, "file:%s/%s%s", 
		urldir,
		(helpstr[0] == '#') ? "snd.html" : "",
		helpstr);
	gtk_moz_embed_load_url(GTK_MOZ_EMBED(help_mozilla), url);
	FREE(url);
	return(help_dialog);
      }
  gtk_moz_embed_open_stream(GTK_MOZ_EMBED(help_mozilla), "file://", "text/html");
  gtk_moz_embed_append_data(GTK_MOZ_EMBED(help_mozilla), "<html><body bgcolor=white><pre>", strlen("<html><body bgcolor=white><pre>"));
  gtk_moz_embed_append_data(GTK_MOZ_EMBED(help_mozilla), helpstr, len);
  gtk_moz_embed_append_data(GTK_MOZ_EMBED(help_mozilla), "</pre></body></html>", strlen("</pre></body></html>"));
  gtk_moz_embed_close_stream(GTK_MOZ_EMBED(help_mozilla));
  return(help_dialog);
}

#endif

GtkWidget *snd_help(snd_state *ss, char *subject, char *helpstr)
{
  return(snd_help_1(ss, subject, helpstr, FALSE));
}

GtkWidget *snd_help_with_wrap(snd_state *ss, char *subject, char *helpstr)
{
  return(snd_help_1(ss, subject, helpstr, TRUE));
}

