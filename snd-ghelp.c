/* TODO  tie into gtkhtml widget if HAVE_HTML
 * TODO  test horizontal scrolling whenever gtk implements it
 */

#include "snd.h"

/* ---------------- HELP MONOLOG ---------------- */

#define HELP_ROWS 12
#define HELP_COLUMNS 56
/* these set the initial size of the (non XmHTML) help dialog text area */

static GtkWidget *help_dialog = NULL;
static GtkWidget *help_text = NULL;
static char help_window_label[64];

static void help_help_callback(GtkWidget *w,gpointer clientData) 
{
  help_dialog_help((snd_state *)clientData);
}

static void dismiss_help(GtkWidget *w,gpointer clientData)
{
  gtk_widget_hide(help_dialog);
}

static void delete_help(GtkWidget *w,GdkEvent *event,gpointer clientData)
{
  gtk_widget_hide(help_dialog);
}

static void add_help_text (snd_state *ss, GtkWidget *text, char *message)
{
  gtk_text_freeze (GTK_TEXT (text));
  gtk_text_insert (GTK_TEXT (text), (ss->sgx)->help_text_fnt, (ss->sgx)->black, (ss->sgx)->white, message,-1);
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
  gtk_text_set_editable(GTK_TEXT(help_text),editable);
  gtk_text_set_word_wrap(GTK_TEXT(help_text),FALSE);
  gtk_text_set_line_wrap(GTK_TEXT(help_text),FALSE); /* apparently horizontal scrolling is not yet implemented (gtktext.c version 1.2.8) */
  gtk_widget_show (help_text);
  hscrollbar = gtk_hscrollbar_new (GTK_TEXT (help_text)->hadj);
  set_background(hscrollbar,(ss->sgx)->position_color);
  gtk_table_attach (GTK_TABLE (table), hscrollbar, 0, 1, 1, 2, 
		    (GtkAttachOptions)(GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions)(GTK_FILL), 
		    0, 0);
  gtk_widget_show (hscrollbar);
  vscrollbar = gtk_vscrollbar_new (GTK_TEXT (help_text)->vadj);
  set_background(vscrollbar,(ss->sgx)->position_color);
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
  GtkWidget *help_button,*ok_button,*table;
  help_dialog = gtk_dialog_new();
  gtk_signal_connect(GTK_OBJECT(help_dialog),"delete_event",GTK_SIGNAL_FUNC(delete_help),(gpointer)ss);
  gtk_window_set_title(GTK_WINDOW(help_dialog),STR_Help);
  gtk_window_set_policy(GTK_WINDOW(help_dialog),TRUE,TRUE,FALSE); /* allow shrink or grow */
  set_background(help_dialog,(ss->sgx)->basic_color);
  gtk_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_widget_set_usize (GTK_WIDGET(help_dialog),HELP_COLUMNS * 9,HELP_ROWS * 20);
  gtk_widget_realize(help_dialog);
  add_dialog(ss,help_dialog);

  help_button = gtk_button_new_with_label(STR_Help);
  ok_button = gtk_button_new_with_label(STR_Ok);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area),ok_button,FALSE,TRUE,20);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(help_dialog)->action_area),help_button,FALSE,TRUE,20);
  gtk_signal_connect(GTK_OBJECT(ok_button),"clicked",GTK_SIGNAL_FUNC(dismiss_help),(gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(help_button),"clicked",GTK_SIGNAL_FUNC(help_help_callback),(gpointer)ss);
  set_pushed_button_colors(help_button,ss);
  set_pushed_button_colors(ok_button,ss);
  gtk_widget_show(ok_button);
  gtk_widget_show(help_button);

  table = create_scrolled_text(ss,FALSE);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(help_dialog)->vbox),table);
  gtk_widget_show(table);
  gtk_widget_show(help_dialog);
}

void snd_help(snd_state *ss, char *subject, char *helpstr)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  guint chars;
  if (!(help_dialog)) create_help_monolog(ss); else raise_dialog(help_dialog);
  sprintf(help_window_label,"%s help",subject);
  gtk_window_set_title(GTK_WINDOW(help_dialog),help_window_label);
  chars = gtk_text_get_length(GTK_TEXT(help_text));
  if (chars > 0) gtk_editable_delete_text(GTK_EDITABLE(help_text),0,-1);
  add_help_text(ss,help_text,helpstr);
}

void move_help_dialog_to(int x, int y)
{
  if (!(help_dialog)) create_help_monolog(get_global_state()); else raise_dialog(help_dialog);
  set_widget_position(help_dialog,x,y);
}

int help_dialog_is_active(void)
{
  return((help_dialog != NULL) && (GTK_WIDGET_VISIBLE(help_dialog)));
}

#if HAVE_GUILE_GTK
#include <guile-gtk.h>

#define Sg_help_dialog_widget  "sg-help-dialog-widget"
#define Sg_help_text_widget    "sg-help-text-widget"

static SCM sg_help_dialog_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(help_dialog)));}
static SCM sg_help_text_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(help_text)));}

void init_help_widgets(SCM local_doc)
{
  gh_new_procedure0_0(Sg_help_dialog_widget,sg_help_dialog_widget);
  gh_new_procedure0_0(Sg_help_text_widget,sg_help_text_widget);
}

#endif
