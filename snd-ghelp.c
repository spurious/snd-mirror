#include "snd.h"

/* ---------------- HELP MONOLOG ---------------- */

/* I tried gtkhtml (and XmHTML in 1999), and in both cases decided not to use them.  Both work fine,
 *   but is it that much of a difference to have an html widget in a dialog as opposed to mozilla
 *   running remotely?  And it would require fixing up all the help texts.  
 */

static GtkWidget *help_dialog = NULL;

static void dismiss_help_dialog(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(help_dialog);
}

static gint delete_help_dialog(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  gtk_widget_hide(help_dialog);
  return(true);
}

#define HELP_ROWS 12
#define HELP_COLUMNS 72
/* these set the initial size of the help dialog text area */

static GtkWidget *help_text = NULL;
slist *related_items = NULL;
static char *original_help_text = NULL;
static char **help_urls = NULL;

/* gtk_text_buffer_insert_with_tags_by_name (buffer, &iter, "some text in red", -1, "red_foreground", NULL); */

static void add_help_text(GtkWidget *text, const char *message)
{
  sg_text_insert(text, (char *)message);
}

int help_text_width(const char *txt, int start, int end)
{
#if 0
  char *buf;
  int len;
  buf = (char *)CALLOC(end - start + 2, sizeof(char));
  strncpy(buf, txt, end - start);
  len = sg_text_width(buf, LISTENER_FONT(ss));
  FREE(buf);
  if (len > 0) return(len);
#endif
  return((end - start) * 8);
}

static with_word_wrap_t outer_with_wrap = WITHOUT_WORD_WRAP;
static int old_help_text_width = 0;

static gboolean help_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  int curwid;
  curwid = widget_width(help_dialog); /* was help_text, but I'm getting shivering in expose events */
  if (old_help_text_width == 0)
    old_help_text_width = curwid;
  else
    {
      if ((outer_with_wrap == WITH_WORD_WRAP) && 
	  (abs(curwid - old_help_text_width) > 50))
	{
	  char *cur_help_str = NULL, *new_help_str = NULL;
	  cur_help_str = sg_get_text(help_text, 0, -1);
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

static bool new_help(const char *pattern, bool complain)
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
	  snd_help_with_xrefs(pattern, XEN_TO_C_STRING(xstr), WITH_WORD_WRAP, xrefs, NULL);
	  snd_unprotect_at(gc_loc);
	  if (xrefs) FREE(xrefs);
	  return(true);
	}
      url_to_html_viewer(url);
      return(true);
    }
  if ((!(snd_topic_help(pattern))) && (complain))
    {
      xrefs = help_name_to_xrefs(pattern);
      if (xrefs)
	{
	  snd_help_with_xrefs(pattern, "(no help found)", WITH_WORD_WRAP, xrefs, NULL);
	  FREE(xrefs);
	  return(true);
	}
      else snd_help_with_xrefs(pattern, "(no help found)", WITH_WORD_WRAP, NULL, NULL);
    }
  return(false);
}

static char **help_history = NULL;
static int help_history_size = 0;
static int help_history_pos = 0;
static bool help_needed = true;

static void add_pattern_to_help_history(const char *pattern)
{
  if (!help_needed) return;
  if (help_history_size == 0)
    {
      help_history_size = 16; /* not 8! -- need room for cycle below */
      help_history = (char **)CALLOC(help_history_size, sizeof(char *));
    }
  else
    {
      if (help_history_pos >= help_history_size)
	{
	  int i;
	  for (i = 0; i < 8; i++) 
	    {
	      if (help_history[i]) FREE(help_history[i]);
	      help_history[i] = help_history[i + 8];
	      help_history[i + 8] = NULL;
	    }
	  help_history_pos = 8;
	}
    }
  if (help_history[help_history_pos]) FREE(help_history[help_history_pos]);
  help_history[help_history_pos++] = copy_string(pattern);
}

static void help_next_callback(GtkWidget *w, gpointer context)
{
  if ((help_history_pos < help_history_size) && 
      (help_history[help_history_pos]))
    {
      help_needed = false;
      help_history_pos++;
      new_help(help_history[help_history_pos - 1], true);
      help_needed = true;
    }
}

static void help_previous_callback(GtkWidget *w, gpointer context)
{
  if ((help_history_pos > 1) &&
      (help_history[help_history_pos - 2]))
    {
      help_needed = false;
      help_history_pos--;
      new_help(help_history[help_history_pos - 1], true);
      help_needed = true;
    }
}

static char *find_highlighted_text(const char *value)
{
  int i, len, start = -1;
  len = snd_strlen(value);
  for (i = 0; i < len; i++)
    if (value[i] == '{')
      start = i + 1;
    else 
      {
	if (value[i] == '}')
	  {
	    int end;
	    end = i;
	    if ((start > 0) && ((end - start) > 0))
	      {
		int k;
		char *topic;
		topic = (char *)CALLOC(end - start + 1, sizeof(char));
		for (i = start, k = 0; i < end; i++, k++)
		  topic[k] = value[i];
		return(topic);
	      }
	  }
      }
  return(NULL);
}

static void help_browse_callback(const char *name, int row, void *data)
{
  char *topic = NULL;
  if ((help_urls) && (help_urls[row]))
    url_to_html_viewer(help_urls[row]);
  else
    {
      topic = find_highlighted_text(name);
      if (topic)
	{
	  name_to_html_viewer(topic);
	  FREE(topic);
	}
      else
	{
	  if (name)
	    new_help(name, true);
	}
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
      /* only change help text display if it's one word in the selection, and we can find new help */
      if (txt)
	{
	  int i, len;
	  bool one_word = true;
	  len = snd_strlen(txt);
	  for (i = 0; i < len; i++)
	    if (isspace(txt[i]))
	      {
		one_word = false;
		break;
	      }
	  if (one_word) new_help(txt, false);
	  g_free(txt);
	}
    }
  return(false);
}

static void search_activated(GtkWidget *w, gpointer context)
{ 
  char *str = NULL;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(w)); /* no free, I think */
  if (new_help(str, true))
    gtk_entry_set_text(GTK_ENTRY(w), "");
}

static GtkWidget *help_next_button = NULL, *help_previous_button = NULL;

static void create_help_monolog(void)
{
  /* create scrollable but not editable text window */
  GtkWidget *ok_button, *search, *frame, *label, *hbox;
  help_dialog = snd_gtk_dialog_new();
  SG_SIGNAL_CONNECT(help_dialog, "delete_event", delete_help_dialog, NULL);

  gtk_window_set_title(GTK_WINDOW(help_dialog), _("Help"));
  sg_make_resizable(help_dialog);
  gtk_container_set_border_width (GTK_CONTAINER(help_dialog), 10);
  gtk_window_resize(GTK_WINDOW(help_dialog), HELP_COLUMNS * 9, HELP_ROWS * 40);
  gtk_widget_realize(help_dialog);

  ok_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
  gtk_widget_set_name(ok_button, "quit_button");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), ok_button, false, true, 20);
  SG_SIGNAL_CONNECT(ok_button, "clicked", dismiss_help_dialog, NULL);
  gtk_widget_show(ok_button);

  help_previous_button = gtk_button_new_from_stock(GTK_STOCK_GO_BACK);
  gtk_widget_set_name(help_previous_button, "reset_button");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), help_previous_button, true, true, 10);
  SG_SIGNAL_CONNECT(help_previous_button, "clicked", help_previous_callback, NULL);
  gtk_widget_show(help_previous_button);

  help_next_button = gtk_button_new_from_stock(GTK_STOCK_GO_FORWARD);
  gtk_widget_set_name(help_next_button, "doit_button");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->action_area), help_next_button, true, true, 10);
  SG_SIGNAL_CONNECT(help_next_button, "clicked", help_next_callback, NULL);
  gtk_widget_show(help_next_button);

  frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->vbox), frame, true, true, 10); 
  gtk_widget_show(frame);

  help_text = make_scrolled_text(frame, false, NULL, false); /* last arg ignored */
  gtk_widget_add_events(help_text, GDK_BUTTON_RELEASE);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(help_text), 10);
  SG_SIGNAL_CONNECT(help_text, "button_release_event", text_release_callback, NULL);

  related_items = slist_new_with_title(_("related topics"), GTK_DIALOG(help_dialog)->vbox, NULL, 0, BOX_PACK);
  related_items->select_callback = help_browse_callback;

  hbox = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(help_dialog)->vbox), hbox, false, false, 10); 
  gtk_widget_show(hbox);

  label = gtk_label_new(_("help topic:"));
  gtk_box_pack_start(GTK_BOX(hbox), label, false, false, 0); 
  gtk_widget_show(label);

  search = snd_entry_new(hbox, WITH_WHITE_BACKGROUND);
  SG_SIGNAL_CONNECT(search, "activate", search_activated, NULL);
  gtk_widget_show(help_dialog);
  SG_SIGNAL_CONNECT(GTK_OBJECT(help_dialog), "expose_event", help_expose_callback, NULL);
  set_dialog_widget(HELP_DIALOG, help_dialog);
}

GtkWidget *snd_help(const char *subject, const char *helpstr, with_word_wrap_t with_wrap)
{
  /* place help string in scrollable help window */
  /* if window is already active, add this help at the top and reposition */
  outer_with_wrap = with_wrap;
  if (!(help_dialog)) create_help_monolog(); else raise_dialog(help_dialog);
  gtk_window_set_title(GTK_WINDOW(help_dialog), subject);
  original_help_text = (char *)helpstr;
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text)), "", 0);
  if (with_wrap == WITH_WORD_WRAP)
    {
      char *new_help = NULL;
      new_help = word_wrap(helpstr, (int)(widget_width(help_text) * 1.3));
      add_help_text(help_text, new_help);
      if (new_help) FREE(new_help);
    }
  else add_help_text(help_text, helpstr);
  if (help_needed) add_pattern_to_help_history(subject);
  slist_clear(related_items); /* this can clobber "subject"! */
  gtk_widget_set_sensitive(help_next_button, (help_history_pos < help_history_size) && (help_history[help_history_pos]));
  gtk_widget_set_sensitive(help_previous_button, (help_history_pos > 1));
  return(help_dialog);
}

GtkWidget *snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, char **xrefs, char **urls)
{
  GtkWidget *w;
  help_urls = urls;
  w = snd_help(subject, helpstr, with_wrap);
  if (xrefs)
    {
      int i;
      for (i = 0; (xrefs[i]); i++)
	slist_append(related_items, xrefs[i]);
    }
  return(w);
}

void snd_help_append(const char *text)
{
  if (help_text) sg_text_insert(help_text, text);
}

void snd_help_back_to_top(void)
{
  if (help_text)
    {
      GtkTextIter pos;
      GtkTextBuffer *buf;
      buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text));
      gtk_text_buffer_get_iter_at_offset(buf, &pos, 0);
      gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(help_text), &pos, 0.0, true, 0.0, 0.0);
    }
}

bool help_dialog_is_active(void)
{
  return((help_dialog) &&
	 (GTK_WIDGET_VISIBLE(help_dialog)));
}
