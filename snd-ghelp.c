#include "snd.h"

/* 
   SOMEDAY: highlight bracketed help text in red fg

   pango_parse_markup("some text <span foreground=\"red\">url</span> more text", -1, 0, **PangoAttrList, NULL, NULL, NULL);
   layout = pango_layout_new(pango_context_new());
   pango_layout_set_attributes(layout, *PangoAttrList);
   now how to use this in list?
   presumably at end: g_object_unref(G_OBJECT(layout));

   or:
   GList = gtk_tree_view_column_get_cell_renderers
   g_list_free when done
   gtk_cell_renderer_text_set_property(cell, PROP_MARKUP, wrapped str, NULL); but this is internal

   or: 
   make the "list" an uneditable text widget, use gtk_text_tags for the red/normal text, and have a click event handler
 */

static GtkWidget *help_dialog = NULL;
static void dismiss_help_dialog(GtkWidget *w, gpointer context) {gtk_widget_hide(help_dialog);}
static void delete_help_dialog(GtkWidget *w, GdkEvent *event, gpointer context) {gtk_widget_hide(help_dialog);}


/* ---------------- HELP MONOLOG ---------------- */

#define HELP_ROWS 12
#define HELP_COLUMNS 56
/* these set the initial size of the (non XmHTML) help dialog text area */

static GtkWidget *help_text = NULL, *related_items = NULL;
static char *original_help_text = NULL;

static void add_help_text(GtkWidget *text, const char *message)
{
  sg_text_insert(text, (char *)message);
}

int help_text_width(const char *txt, int start, int end)
{
  GdkFont *font;
  if (help_text)
    {
      font = gtk_style_get_font(help_text->style);
      if (font)
	return(gdk_text_width(font, (char *)(txt + start), end - start));
    }
  return(0);
}

static int old_help_text_width = 0;
static bool outer_with_wrap = false;

static gboolean help_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  int curwid;
  curwid = widget_width(help_text);
  if (old_help_text_width == 0)
    old_help_text_width = curwid;
  else
    {
      if ((outer_with_wrap) && (abs(curwid - old_help_text_width) > 10))
	{
	  char *cur_help_str = NULL;
	  char *new_help_str = NULL;
	  int end;
	  GtkTextIter s, e;
	  GtkTextBuffer *buf;
	  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text));
	  end = gtk_text_buffer_get_char_count(buf);
	  gtk_text_buffer_get_iter_at_offset(buf, &s, 0);
	  gtk_text_buffer_get_iter_at_offset(buf, &e, end);
	  cur_help_str = gtk_text_buffer_get_text(buf, &s, &e, true);
	  new_help_str = word_wrap(original_help_text, curwid);
	  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text)), "", 0);
	  sg_text_insert(help_text, new_help_str);
	  if (new_help_str) FREE(new_help_str);
	  if (cur_help_str) g_free(cur_help_str);
	  old_help_text_width = curwid;
	}
    }
  return(false);
}

static bool new_help(const char *pattern)
{
  char *url;
  char **xrefs;
  url = snd_url(pattern);
  if (url)
    {
      /* given name, find doc string, if any */
      XEN xstr;
      xstr = g_snd_help(C_TO_XEN_STRING(pattern), 0);
      if (XEN_STRING_P(xstr))
	{
	  int gc_loc;
	  gc_loc = snd_protect(xstr);
	  xrefs = help_name_to_xrefs(pattern);
	  snd_help_with_xrefs(pattern, XEN_TO_C_STRING(xstr), true, xrefs, NULL);
	  snd_unprotect_at(gc_loc);
	  if (xrefs) FREE(xrefs);
	  return(true);
	}
    }
  if (!(snd_topic_help(pattern)))
    {
      xrefs = help_name_to_xrefs(pattern);
      if (xrefs)
	{
	  snd_help_with_xrefs(pattern, "(no help found)", true, xrefs, NULL);
	  FREE(xrefs);
	  return(true);
	}
    }
  return(false);
}

static char *find_highlighted_text(const char *value)
{
  char *topic = NULL;
  int i, k, len, start = -1, end;
  len = snd_strlen(value);
  for (i = 0; i < len; i++)
    if (value[i] == '{')
      start = i + 1;
    else 
      {
	if (value[i] == '}')
	  {
	    end = i;
	    if ((start > 0) && ((end - start) > 0))
	      {
		topic = (char *)CALLOC(end - start + 1, sizeof(char));
		for (i = start, k = 0; i < end; i++, k++)
		  topic[k] = value[i];
		return(topic);
	      }
	  }
      }
  return(NULL);
}

static void help_browse_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value = NULL;
  GtkTreeModel *model;
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  if (value)
    {
      char *topic = NULL;
      topic = find_highlighted_text(value);
      if (topic)
	{
	  name_to_html_viewer(topic);
	  FREE(topic);
	}
      else
	{
	  if (value)
	    new_help(value);
	}
    }
  g_free(value);
}

static void double_callback(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer data)
{
  GtkTreeModel *model = gtk_tree_view_get_model(tree_view);
  GtkTreeIter iter;
  gchar *topic, *value;
  gtk_tree_model_get_iter(model, &iter, path);
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  if (value)
    {
      topic = find_highlighted_text(value);
      if (topic)
	{
	  name_to_html_viewer(topic);
	  FREE(topic);
	}
      else
	{
	  if (value)
	    name_to_html_viewer(value);
	}
      g_free(value);
    }
}

static gboolean text_release_callback(GtkTreeSelection *selection, gpointer *gp)
{
  /* this needs to be bool return false -- otherwise, apparently, the mouse-drag->selection never gets turned off! */
  GtkTextIter start, end;
  #define HELP_BUFFER gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text))
  if (gtk_text_buffer_get_selection_bounds(HELP_BUFFER, &start, &end))
    {
      char *txt;
      txt = gtk_text_buffer_get_text(HELP_BUFFER, &start, &end, true);
      if (txt)
	{
	  new_help(txt);
	  g_free(txt);
	}
    }

  return(false);
}

static void search_activated(GtkWidget *w, gpointer context)
{ 
  char *str = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w)); /* no free, I think */
  if (new_help(str))
    gtk_entry_set_text(GTK_ENTRY(w), "");
}

static void create_help_monolog(void)
{
  /* create scrollable but not editable text window */
  GtkWidget *ok_button, *search, *frame, *label, *hbox;
  help_dialog = snd_gtk_dialog_new();
  g_signal_connect_closure_by_id(GTK_OBJECT(help_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(help_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(delete_help_dialog), NULL, 0),
				 0);

  gtk_window_set_title(GTK_WINDOW(help_dialog), _("Help"));
  sg_make_resizable(help_dialog);
  gtk_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_window_resize(GTK_WINDOW(help_dialog), HELP_COLUMNS * 9, HELP_ROWS * 40);
  gtk_widget_realize(help_dialog);

  ok_button = gtk_button_new_with_label(_("Ok"));
  gtk_widget_set_name(ok_button, "quit_button");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), ok_button, false, true, 20);
  g_signal_connect_closure_by_id(GTK_OBJECT(ok_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(ok_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_help_dialog), NULL, 0),
				 0);
  gtk_widget_show(ok_button);

  frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->vbox), frame, true, true, 10); 
  gtk_widget_show(frame);

  help_text = make_scrolled_text(frame, false, NULL, NULL);
  gtk_widget_add_events(help_text, GDK_BUTTON_RELEASE);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(help_text), 10);
  g_signal_connect_closure_by_id(GTK_OBJECT(help_text),
				 g_signal_lookup("button_release_event", G_OBJECT_TYPE(GTK_OBJECT(help_text))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(text_release_callback), NULL, 0),
				 0);

  related_items = sg_make_list(_("related topics"), 
			       GTK_DIALOG(help_dialog)->vbox, 
			       BOX_PACK, NULL, 0, NULL,
			       GTK_SIGNAL_FUNC(help_browse_callback), 0, 0, 0, 0);
  gtk_widget_add_events(related_items, GDK_ALL_EVENTS_MASK);
  g_signal_connect(GTK_OBJECT(related_items), "row_activated", G_CALLBACK(double_callback), NULL);


  hbox = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->vbox), hbox, false, false, 10); 
  gtk_widget_show(hbox);

  label = gtk_label_new(_("help topic:"));
  gtk_box_pack_start(GTK_BOX(hbox), label, false, false, 0); 
  gtk_widget_show(label);

  search = snd_entry_new(hbox, true);
  g_signal_connect_closure_by_id(GTK_OBJECT(search),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(search))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(search_activated), NULL, 0),
				 0);
  gtk_widget_show(help_dialog);

  g_signal_connect_closure_by_id(GTK_OBJECT(GTK_OBJECT(help_dialog)),
				 g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(help_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_expose_callback), NULL, 0),
				 0);

  set_dialog_widget(HELP_DIALOG, help_dialog);
}

GtkWidget *snd_help(const char *subject, const char *helpstr, bool with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  outer_with_wrap = with_wrap;
  if (!(help_dialog)) create_help_monolog(); else raise_dialog(help_dialog);
  gtk_window_set_title(GTK_WINDOW(help_dialog), subject);
  original_help_text = (char *)helpstr;
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

GtkWidget *snd_help_with_xrefs(const char *subject, const char *helpstr, bool with_wrap, char **xrefs, char **urls)
{
  GtkWidget *w;
  w = snd_help(subject, helpstr, with_wrap);
  if (xrefs)
    {
      int i;
      gtk_list_store_clear(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(related_items))));
      for (i = 0; (xrefs[i]); i++)
	sg_list_append(related_items, xrefs[i]);
    }
  return(w);
}
