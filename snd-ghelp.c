/* 
 * TODO  test horizontal scrolling whenever gtk implements it
 * TODO  if from glistener, and text selected, complete in listener
 */

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
  gtk_text_freeze (GTK_TEXT (text));
  gtk_text_insert (GTK_TEXT (text), (ss->sgx)->help_text_fnt, (ss->sgx)->black, (ss->sgx)->white, message, -1);
  gtk_text_thaw (GTK_TEXT (text));
}

static GtkWidget *create_scrolled_text(snd_state *ss, int editable)
{
  GtkWidget *table;
  GtkWidget *hscrollbar;
  GtkWidget *vscrollbar;
  /* basically copied from the tutorial */
  table = gtk_table_new (2, 2, FALSE);
  help_text = gtk_text_new (NULL, NULL);
  gtk_table_attach (GTK_TABLE (table), help_text, 0, 1, 0, 1, 
		    (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		    (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		    0, 0);
  gtk_text_set_editable(GTK_TEXT(help_text), editable);
  gtk_text_set_word_wrap(GTK_TEXT(help_text), FALSE);
  gtk_text_set_line_wrap(GTK_TEXT(help_text), FALSE); /* apparently horizontal scrolling is not yet implemented (gtktext.c version 1.2.8) */
  gtk_widget_show (help_text);
  hscrollbar = gtk_hscrollbar_new (GTK_TEXT (help_text)->hadj);
  set_background(hscrollbar, (ss->sgx)->position_color);
  gtk_table_attach (GTK_TABLE (table), hscrollbar, 0, 1, 1, 2, 
		    (GtkAttachOptions)(GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions)(GTK_FILL), 
		    0, 0);
  gtk_widget_show (hscrollbar);
  vscrollbar = gtk_vscrollbar_new (GTK_TEXT (help_text)->vadj);
  set_background(vscrollbar, (ss->sgx)->position_color);
  gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1, 
		    (GtkAttachOptions)(GTK_FILL), 
		    (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		    0, 0);
  gtk_widget_show (vscrollbar);
  return(table);
}

static void create_help_monolog(snd_state *ss)
{
  /* create scrollable but not editable text window */
  GtkWidget *help_button, *ok_button, *table;
  help_dialog = gtk_dialog_new();
  set_dialog_widget(ss, HELP_DIALOG, help_dialog);
  gtk_signal_connect(GTK_OBJECT(help_dialog), "delete_event", GTK_SIGNAL_FUNC(delete_help), (gpointer)ss);
  gtk_window_set_title(GTK_WINDOW(help_dialog), STR_Help);
  gtk_window_set_policy(GTK_WINDOW(help_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
  set_background(help_dialog, (ss->sgx)->basic_color);
  gtk_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_widget_set_usize (GTK_WIDGET(help_dialog), HELP_COLUMNS * 9, HELP_ROWS * 20);
  gtk_widget_realize(help_dialog);

  help_button = gtk_button_new_with_label(STR_Help);
  ok_button = gtk_button_new_with_label(STR_Ok);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), ok_button, FALSE, TRUE, 20);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), help_button, FALSE, TRUE, 20);
  gtk_signal_connect(GTK_OBJECT(ok_button), "clicked", GTK_SIGNAL_FUNC(dismiss_help), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(help_help_callback), (gpointer)ss);
  set_pushed_button_colors(help_button, ss);
  set_pushed_button_colors(ok_button, ss);
  gtk_widget_show(ok_button);
  gtk_widget_show(help_button);

  table = create_scrolled_text(ss, FALSE);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(help_dialog)->vbox), table);
  gtk_widget_show(table);
  gtk_widget_show(help_dialog);
}

static GtkWidget *snd_help_1(snd_state *ss, char *subject, char *helpstr, int with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  guint chars;
  if (!(help_dialog)) create_help_monolog(ss); else raise_dialog(help_dialog);
  mus_snprintf(help_window_label, LABEL_BUFFER_SIZE, "%s help", subject);
  gtk_window_set_title(GTK_WINDOW(help_dialog), help_window_label);
  chars = gtk_text_get_length(GTK_TEXT(help_text));
  if (chars > 0) gtk_editable_delete_text(GTK_EDITABLE(help_text), 0, -1);
  if (with_wrap)
    {
      char *new_help = NULL;
      new_help = word_wrap(helpstr, widget_width(help_text));
      add_help_text(ss, help_text, new_help);
      if (new_help) FREE(new_help);
    }
  else add_help_text(ss, help_text, helpstr);
  return(help_dialog);
}

#else

/* HAVE_HTML -- try the mozilla embedded widget -- this taken largely from TestGtkEmbed.cpp from the Mozilla sources */

#include "gtkmozembed.h"
#include <gtk/gtk.h>

static GtkWidget *help_mozilla = NULL;

static void go_back(GtkButton *button, gpointer ignored) {gtk_moz_embed_go_back(GTK_MOZ_EMBED(help_mozilla));}
static void go_forward(GtkButton *button, gpointer ignored) {gtk_moz_embed_go_forward(GTK_MOZ_EMBED(help_mozilla));}

static void create_help_monolog(snd_state *ss)
{
  /* create scrollable but not editable text window */
  GtkWidget *help_button, *ok_button, *back_button, *forward_button;
  help_dialog = gtk_dialog_new();
  set_dialog_widget(ss, HELP_DIALOG, help_dialog);
  gtk_signal_connect(GTK_OBJECT(help_dialog), "delete_event", GTK_SIGNAL_FUNC(delete_help), (gpointer)ss);
  gtk_window_set_title(GTK_WINDOW(help_dialog), STR_Help);
  gtk_window_set_policy(GTK_WINDOW(help_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
  set_background(help_dialog, (ss->sgx)->basic_color);
  gtk_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_widget_set_usize (GTK_WIDGET(help_dialog), 500, 500);
  gtk_widget_realize(help_dialog);

  help_button = gtk_button_new_with_label(STR_Help);
  ok_button = gtk_button_new_with_label(STR_Ok);
  back_button = gtk_button_new_with_label(STR_Back);
  forward_button = gtk_button_new_with_label(STR_Forward);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), ok_button, FALSE, TRUE, 20);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), back_button, FALSE, TRUE, 20);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), forward_button, FALSE, TRUE, 20);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), help_button, FALSE, TRUE, 20);
  gtk_signal_connect(GTK_OBJECT(ok_button), "clicked", GTK_SIGNAL_FUNC(dismiss_help), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(back_button), "clicked", GTK_SIGNAL_FUNC(go_back), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(forward_button), "clicked", GTK_SIGNAL_FUNC(go_forward), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(help_help_callback), (gpointer)ss);
  set_pushed_button_colors(help_button, ss);
  set_pushed_button_colors(ok_button, ss);
  set_pushed_button_colors(back_button, ss);
  set_pushed_button_colors(forward_button, ss);
  gtk_widget_show(ok_button);
  gtk_widget_show(back_button);
  gtk_widget_show(forward_button);
  gtk_widget_show(help_button);

  help_mozilla = gtk_moz_embed_new();
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(help_dialog)->vbox), help_mozilla);

  gtk_widget_show(help_mozilla);
  gtk_widget_show(help_dialog);
}

static GtkWidget *snd_help_1(snd_state *ss, char *subject, char *helpstr, int with_wrap)
{
  int i, len, lim;
  char *url, *urldir;
  if (!(help_dialog)) create_help_monolog(ss); else raise_dialog(help_dialog);
  mus_snprintf(help_window_label, LABEL_BUFFER_SIZE, "%s help", subject);
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

void move_help_dialog_to(int x, int y)
{
  /* only used in snd-glistener, and is obsolete there */
  if (!(help_dialog)) 
    create_help_monolog(get_global_state()); 
  else raise_dialog(help_dialog);
  set_widget_position(help_dialog, x, y);
}

int help_dialog_is_active(void)
{
  return((help_dialog != NULL) && (GTK_WIDGET_VISIBLE(help_dialog)));
}

