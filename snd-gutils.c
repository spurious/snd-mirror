#include "snd.h"

bool set_tiny_font(char *font)
{
  PangoFontDescription *fs = NULL;
  state_context *sgx;
  sgx = ss->sgx;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (tiny_font(ss)) FREE(tiny_font(ss));
      in_set_tiny_font(copy_string(font));
      TINY_FONT(ss) = fs;
      return(true);
    }
  return(false);
}

bool set_listener_font(char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (listener_font(ss)) FREE(listener_font(ss));
      in_set_listener_font(copy_string(font));
      LISTENER_FONT(ss) = fs;
      return(true);
    }
  return(false);
}

bool set_peaks_font(char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (peaks_font(ss)) FREE(peaks_font(ss));
      in_set_peaks_font(copy_string(font));
      PEAKS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}

bool set_bold_peaks_font(char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (bold_peaks_font(ss)) FREE(bold_peaks_font(ss));
      in_set_bold_peaks_font(copy_string(font));
      BOLD_PEAKS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}

bool set_axis_label_font(char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (axis_label_font(ss)) FREE(axis_label_font(ss));
      in_set_axis_label_font(copy_string(font));
      AXIS_LABEL_FONT(ss) = fs;
#if HAVE_GL
      reload_label_font();
#endif
      return(true);
    }
  return(false);
}

bool set_axis_numbers_font(char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (axis_numbers_font(ss)) FREE(axis_numbers_font(ss));
      in_set_axis_numbers_font(copy_string(font));
      AXIS_NUMBERS_FONT(ss) = fs;
#if HAVE_GL
      reload_number_font();
#endif
      return(true);
    }
  return(false);
}

int sg_text_width(char *txt, PangoFontDescription *font)
{
  PangoLayout *layout = NULL;
  int wid = 0;
  if (txt == NULL) return(0);
  if (strlen(txt) == 0) return(0);
  layout = pango_layout_new(gdk_pango_context_get());
  if (layout)
    {
      pango_layout_set_font_description(layout, font);
      pango_layout_set_text(layout, txt, -1);
      pango_layout_get_pixel_size(layout, &wid, NULL);
      g_object_unref(G_OBJECT(layout));
    }
  return(wid);
}

int label_width(char *txt)
{
  if (txt)
    return(sg_text_width(txt, AXIS_LABEL_FONT(ss)));
  else return(0);
}

int mark_name_width(char *txt)
{
  if (txt)
    return(sg_text_width(txt, PEAKS_FONT(ss)));
  return(0);
}

int number_width(char *num)
{
  if (num)
    return(sg_text_width(num, AXIS_NUMBERS_FONT(ss)));
  return(0);
}

static int sg_font2width(PangoFontDescription *font)
{
  PangoLayout *layout = NULL;
  int wid = 0;
  layout = pango_layout_new(gdk_pango_context_get());
  if (layout)
    {
      pango_layout_set_font_description(layout, font);
      pango_layout_set_text(layout, "1", -1);
      pango_layout_get_pixel_size(layout, &wid, NULL);
      g_object_unref(G_OBJECT(layout));
    }
  return(wid);
}

static int sg_font2height(PangoFontDescription *font)
{
  PangoLayout *layout = NULL;
  int wid = 0;
  layout = pango_layout_new(gdk_pango_context_get());
  if (layout)
    {
      pango_layout_set_font_description(layout, font);
      pango_layout_set_text(layout, "1", -1);
      pango_layout_get_pixel_size(layout, NULL, &wid);
      g_object_unref(G_OBJECT(layout));
    }
  return(wid);
}

int number_height(void)
{
  return(sg_font2height(AXIS_NUMBERS_FONT(ss)));
}

int label_height(void)
{
  return(sg_font2width(AXIS_LABEL_FONT(ss)));
}

void clear_window(axis_context *ax)
{
  if (ax) gdk_window_clear(ax->wn);
}

void raise_dialog(GtkWidget *w)
{
  /* since we're using non-transient message dialogs, the dialog window can become completely
   * hidden behind other windows, with no easy way to raise it back to the top, so...
   */
  gtk_widget_show(w);
  gdk_window_raise(w->window);
}

void set_button_label(GtkWidget *label, const char *str)
{
  gtk_label_set_text(GTK_LABEL(GTK_BIN(label)->child), str);
}

void set_label(GtkWidget *label, const char *str)
{
  gtk_label_set_text(GTK_LABEL(label), str);
}

void sg_left_justify_button(GtkWidget *button)
{
  gfloat x, y;
  gtk_misc_get_alignment(GTK_MISC(GTK_LABEL(GTK_BIN(button)->child)), &x, &y);
  gtk_misc_set_alignment(GTK_MISC(GTK_LABEL(GTK_BIN(button)->child)), 0.05, y);
}

void sg_left_justify_label(GtkWidget *label)
{
  /* the label justify function in Gtk refers to the text of the lines after the 1st! */
  gfloat x, y;
  gtk_misc_get_alignment(GTK_MISC(GTK_LABEL(label)), &x, &y);
  gtk_misc_set_alignment(GTK_MISC(GTK_LABEL(label)), 0.05, y);
}


void check_for_event(void)
{
  /* this is needed to force label updates and provide interrupts for long computations */
  int i = 0;
  if (ss->checking_explicitly) return;
  ss->checking_explicitly = true;
  while ((i < 50) && (gtk_events_pending()))
    {
      gtk_main_iteration();
      i++; /* don't hang! */
    }
  ss->checking_explicitly = false;
}

void force_update(GtkWidget *wid)
{
  gdk_window_invalidate_rect(GDK_WINDOW((GTK_WIDGET(wid))->window), NULL, true);
  gdk_window_process_updates(GDK_WINDOW((GTK_WIDGET(wid))->window), true);
}

bool event_pending(void)
{
  return((bool)gtk_events_pending());
}

void set_title(const char *title)
{
#ifndef SND_AS_WIDGET
  gtk_window_set_title(GTK_WINDOW(MAIN_SHELL(ss)), title);
#endif
}

void goto_window(GtkWidget *text)
{
  gtk_widget_grab_focus(text);
}

void gc_set_foreground_xor(GdkGC *gc, GdkColor *col1, GdkColor *col2)
{ 
  GdkColor newcol;
  newcol.pixel = XOR(col1->pixel, col2->pixel);
  newcol.red = XOR(col1->red, col2->red);
  newcol.green = XOR(col1->green, col2->green);
  newcol.blue = XOR(col1->blue, col2->blue);
  gdk_gc_set_foreground(gc, gdk_color_copy(&newcol)); /* memleak? */
}


void color_cursor(GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->cursor_color = color;
  gc_set_foreground_xor(sx->cursor_gc, color, sx->graph_color);
  gc_set_foreground_xor(sx->selected_cursor_gc, color, sx->selected_graph_color);
}

void color_marks(GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->mark_color = color;
  gc_set_foreground_xor(sx->mark_gc, color, sx->graph_color);
  gc_set_foreground_xor(sx->selected_mark_gc, color, sx->selected_graph_color);
}

void color_selection(GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selection_color = color;
  gc_set_foreground_xor(sx->selection_gc, color, sx->graph_color);
  gc_set_foreground_xor(sx->selected_selection_gc, color, sx->selected_graph_color);
}

void color_graph(GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->graph_color = color;
  gdk_gc_set_background(sx->basic_gc, color);
  gdk_gc_set_foreground(sx->erase_gc, color);
  gc_set_foreground_xor(sx->selection_gc, sx->selection_color, color);
  gc_set_foreground_xor(sx->cursor_gc, sx->cursor_color, color);
  gc_set_foreground_xor(sx->mark_gc, sx->mark_color, color);
}

void color_selected_graph(GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selected_graph_color = color;
  gdk_gc_set_background(sx->selected_basic_gc, color);
  gdk_gc_set_foreground(sx->selected_erase_gc, color);
  gc_set_foreground_xor(sx->selected_selection_gc, sx->selection_color, color);
  gc_set_foreground_xor(sx->selected_cursor_gc, sx->cursor_color, color);
  gc_set_foreground_xor(sx->selected_mark_gc, sx->mark_color, color);
}

void color_data(GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->data_color = color;
  gdk_gc_set_foreground(sx->basic_gc, color);
  gdk_gc_set_background(sx->erase_gc, color);
}

void color_selected_data(GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selected_data_color = color;
  gdk_gc_set_foreground(sx->selected_basic_gc, color);
  gdk_gc_set_background(sx->selected_erase_gc, color);
}

void set_mix_color(GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->mix_color = color;
  gdk_gc_set_foreground(sx->mix_gc, color);
}

void recolor_graph(chan_info *cp, bool selected)
{
  state_context *sx;
  sx = ss->sgx;
  gtk_widget_modify_bg(channel_graph(cp), GTK_STATE_NORMAL, (selected) ? sx->selected_graph_color : sx->graph_color);
}

void set_sensitive(GtkWidget *wid, bool val) 
{
  if (wid) 
    gtk_widget_set_sensitive(wid, val);
}

bool is_sensitive(GtkWidget *wid) 
{
  if (wid) 
    return(GTK_WIDGET_IS_SENSITIVE(wid)); 
  return(false);
}

void set_toggle_button(GtkWidget *wid, bool val, bool passed, void *data) 
{
  if (!passed) g_signal_handlers_block_matched(GTK_OBJECT(wid), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)data);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wid), val);
  if (!passed) g_signal_handlers_unblock_matched(GTK_OBJECT(wid), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)data);
}

guint16 widget_height(GtkWidget *w)
{
  gint x, y;
  gdk_drawable_get_size(w->window, &x, &y);
  return(y);
}

guint16 widget_width(GtkWidget *w)
{
  gint x, y;
  gdk_drawable_get_size(w->window, &x, &y);
  return(x);
}

void set_widget_height(GtkWidget *w, guint16 height)
{
  set_widget_size(w, widget_width(w), height);
}

void set_widget_width(GtkWidget *w, guint16 width)
{
  set_widget_size(w, width, widget_height(w));
}

gint16 widget_x(GtkWidget *w)
{
  gint x, y;
  gdk_window_get_position(w->window, &x, &y);
  return(x);
}

gint16 widget_y(GtkWidget *w)
{
  gint x, y;
  gdk_window_get_position(w->window, &x, &y);
  return(y);
}

void set_widget_x(GtkWidget *w, gint16 x)
{
  gtk_window_move(GTK_WINDOW(w), x, widget_y(w));
}

void set_widget_y(GtkWidget *w, gint16 y)
{
  gtk_window_move(GTK_WINDOW(w), widget_x(w), y);
}

void set_widget_size(GtkWidget *w, guint16 width, guint16 height)
{
#if 1
  /* This one resizes the window. */
  gdk_window_resize(w->window, width, height);
#else
  /* This one doesn't seem to do anything, but the function name seems at least to be quite similar to set_widget_size. */
  GtkRequisition r;
  r.width = width;
  r.height = height;
  gtk_widget_size_request(w, &r);
#endif
#if 0
  /* This one doesn't do anything, and prints out errors. */
  gtk_window_resize(GTK_WINDOW(w), width, height);
#endif
}

void set_widget_position(GtkWidget *w, gint16 x, gint16 y)
{
  gtk_window_move(GTK_WINDOW(w), x, y);
}

void fixup_axis_context(axis_context *ax, GtkWidget *w, GdkGC *gc)
{
  ax->wn = w->window;
  ax->w = w;
  if (gc) ax->gc = gc;
  ax->current_font = AXIS_NUMBERS_FONT(ss);
}

void set_user_data(GObject *obj, gpointer data)
{
  g_object_set_data(obj, "snd-data", data);
}

gpointer get_user_data(GObject *obj)
{
  return(g_object_get_data(obj, "snd-data"));
}

void set_user_int_data(GObject *obj, int data)
{
  int *gdata;
  gdata = (int *)MALLOC(sizeof(int));
  gdata[0] = data;
  g_object_set_data(obj, "snd-data", (gpointer)gdata);
}

int get_user_int_data(GObject *obj)
{
  gpointer gdata;
  gdata = g_object_get_data(obj, "snd-data");
  return(((int *)gdata)[0]);
}

char *sg_get_text(GtkWidget *w, int start, int end)
{
  GtkTextIter s, e;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &s, start);
  gtk_text_buffer_get_iter_at_offset(buf, &e, end);  /* this is utterly idiotic!!! */
  return(gtk_text_buffer_get_text(buf, &s, &e, true));
}

void sg_text_delete(GtkWidget *w, int start, int end)
{
  GtkTextIter s, e;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &s, start);
  gtk_text_buffer_get_iter_at_offset(buf, &e, end); 
  gtk_text_buffer_delete(buf, &s, &e);
}

void sg_text_insert(GtkWidget *w, char *text)
{
  GtkTextIter pos;
  GtkTextBuffer *buf;
  if (text)
    {
      buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
      gtk_text_buffer_get_end_iter(buf, &pos);
      gtk_text_buffer_insert(buf, &pos, text, strlen(text));
    }
  else gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(w)), "", 0);
}

void sg_set_cursor(GtkWidget *w, int position)
{
  GtkTextIter pos;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &pos, position - 1);
  gtk_text_buffer_place_cursor(buf, &pos);
  gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(w), gtk_text_buffer_get_insert(buf));
}

int sg_cursor_position(GtkWidget *w)
{
  GtkTextMark *m;
  GtkTextIter pos;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  m = gtk_text_buffer_get_insert(buf);
  gtk_text_buffer_get_iter_at_mark(buf, &pos, m);
  return(gtk_text_iter_get_offset(&pos));
}

GtkWidget *make_scrolled_text(GtkWidget *parent, bool editable, GtkWidget *boxer, GtkWidget *paner)
{
  /* returns new text widget */
  GtkWidget *sw, *new_text;
  GtkTextBuffer *buf;
  sw = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  if (boxer)
    gtk_box_pack_start(GTK_BOX(boxer), sw, true, true, 4);
  new_text = gtk_text_view_new();
  buf = gtk_text_buffer_new(NULL);
  gtk_text_view_set_buffer(GTK_TEXT_VIEW(new_text), buf);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(new_text), editable);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(new_text), GTK_WRAP_NONE);
  gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(new_text), editable);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(new_text), 4);
  gtk_container_add(GTK_CONTAINER(sw), new_text);
  if (editable) gtk_widget_set_events(new_text, GDK_ALL_EVENTS_MASK);
  gtk_widget_show(new_text);
  if (parent)
    gtk_container_add(GTK_CONTAINER(parent), sw);
  if (paner)
    gtk_paned_add2(GTK_PANED(paner), sw);
  gtk_widget_show(sw);
  return(new_text);
}

GtkWidget *sg_make_list(const char *title, GtkWidget *parent, widget_add_t paned, gpointer gp, 
			int num_items, char **items, GtkSignalFunc callback, int t1, int t2, int t3, int t4)
{
  GtkWidget *list;
  int i;
  GtkListStore *model;
  GtkTreeIter iter;
  GtkTreeViewColumn *column;
  GtkWidget *scrolled_win;
  GtkCellRenderer *celler;

  model = gtk_list_store_new(1, G_TYPE_STRING);
  list = gtk_tree_view_new_with_model(GTK_TREE_MODEL(model));
  celler = gtk_cell_renderer_text_new();
  celler->ypad = 0;
  column = gtk_tree_view_column_new_with_attributes(title, celler, "text", 0, NULL);
  gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);

  {
    GtkWidget *gad;
    char *padded_title;
    padded_title = (char *)CALLOC(16 + snd_strlen(title), sizeof(char));
    sprintf(padded_title, "    %s", title);
    gad = snd_gtk_label_new(padded_title, ss->sgx->highlight_color);
    FREE(padded_title);
    gtk_widget_show(gad);
    gtk_tree_view_column_set_widget(column, gad);
    gtk_widget_set_size_request(gad, -1, 16);
  }

  gtk_tree_view_append_column(GTK_TREE_VIEW(list), column);
  scrolled_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_win), GTK_SHADOW_IN);  
  gtk_container_add(GTK_CONTAINER(scrolled_win), list);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win), GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
  gtk_container_set_border_width(GTK_CONTAINER(scrolled_win), 0);
  g_signal_connect(gtk_tree_view_get_selection(GTK_TREE_VIEW(list)), "changed", G_CALLBACK(callback), gp);

  switch (paned)
    {
    case PANED_ADD: 
      gtk_paned_add1(GTK_PANED(parent), scrolled_win); 
      break;
    case BOX_PACK: 
      gtk_box_pack_start(GTK_BOX(parent), scrolled_win, true, true, 0); 
      break;
    case TABLE_ATTACH: 
      gtk_table_attach(GTK_TABLE(parent), scrolled_win, t1, t2, t3, t4,
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      break;
    case CONTAINER_ADD: 
      gtk_container_add(GTK_CONTAINER(parent), scrolled_win); 
      break;
    }
  gtk_widget_show(list);
  gtk_widget_show(scrolled_win);
  for (i = 0; i < num_items; i++) 
    {
      gtk_list_store_append(model, &iter);
      gtk_list_store_set(model, &iter, 0, items[i], -1);
    }
  return(list);
}

void sg_list_append(GtkWidget *lst, char *val)
{
  GtkTreeIter iter;
  GtkListStore *w;
  w = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(lst)));
  gtk_list_store_append(w, &iter);
  gtk_list_store_set(w, &iter, 0, val, -1);
}

void sg_list_insert(GtkWidget *lst, int row, char *val)
{
  GtkTreeIter iter;
  GtkListStore *w;
  w = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(lst)));
  gtk_list_store_insert(w, &iter, row);
  gtk_list_store_set(w, &iter, 0, val, -1);
}

#if 0
static void sg_list_set_text(GtkWidget *lst, int row, char *val)
{
  GtkTreeIter iter;
  GtkListStore *w;
  w = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(lst)));
  gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(w), &iter, NULL, row);
  gtk_list_store_set(w, &iter, 0, val, -1);
}
#endif

void sg_list_select(GtkWidget *lst, int row)
{
  GtkTreeIter iter;
  GtkTreeModel *w;
  GtkTreeSelection *tree;
  w = gtk_tree_view_get_model(GTK_TREE_VIEW(lst));
  gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(w), &iter, NULL, row);
  tree = gtk_tree_view_get_selection(GTK_TREE_VIEW(lst));
  gtk_tree_selection_select_iter(tree, &iter);
}

void sg_list_moveto(GtkWidget *lst, int row)
{
  GtkTreeIter iter;
  GtkTreeModel *w;
  GtkTreePath *path;
  w = gtk_tree_view_get_model(GTK_TREE_VIEW(lst));
  gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(w), &iter, NULL, row);
  path = gtk_tree_model_get_path(w, &iter);
  gtk_tree_view_scroll_to_cell(GTK_TREE_VIEW(lst), path, NULL, true, 0.5, 0.5);
  gtk_tree_path_free(path);
}

void sg_make_resizable(GtkWidget *w)
{
  if (GTK_IS_DIALOG(w))
    {
      gtk_window_set_default_size(GTK_WINDOW(w), -1, -1);
      gtk_window_set_resizable(GTK_WINDOW(w), true);
    }
}

Cessator add_work_proc(GtkFunction func, gpointer data)
{
  /* during auto-testing I need to force the background procs to run to completion */
  if (with_background_processes(ss))
    return(g_idle_add_full(G_PRIORITY_DEFAULT_IDLE, func, data, NULL));
  else
    {
      while (((*func)(data)) == BACKGROUND_CONTINUE);
      return(0);
    }
}

GtkWidget *snd_gtk_dialog_new(void)
{
  GtkWidget *w;
  w = gtk_dialog_new();
  g_object_ref(w);  /* don't ask... */
  return(w);
}

GtkWidget *snd_gtk_label_new(const char *label, GdkColor *color)
{
  GtkWidget *rlw;
  /* -------------------------------- fUn WiTh DuMb SoFtWaRe!!  -------------------------------- */
  /* this is what goddamn gtk forces us to use -- all I want is a label with a background color! */
  rlw = gtk_entry_new();
  gtk_entry_set_has_frame(GTK_ENTRY(rlw), false);
  gtk_entry_set_text(GTK_ENTRY(rlw), label);
  gtk_editable_set_editable(GTK_EDITABLE(rlw), false);
  GTK_WIDGET_UNSET_FLAGS(GTK_WIDGET(rlw), GTK_CAN_FOCUS); /* turn off the $%#@$! blinking cursor */
  gtk_widget_modify_base(rlw, GTK_STATE_NORMAL, color);
  gtk_widget_modify_base(rlw, GTK_STATE_ACTIVE, color);
  return(rlw);
}
