#include "snd.h"

int set_help_text_font(snd_state *ss, char *font)
{
  SG_FONT *fs = NULL;
  state_context *sgx;
  if (ss->using_schemes) return(FALSE);
  sgx = ss->sgx;
  fs = SG_FONT_LOAD(font);
  if (fs)
    {
      if (help_text_font(ss)) FREE(help_text_font(ss));
      in_set_help_text_font(ss, copy_string(font));
      sgx->help_text_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_tiny_font(snd_state *ss, char *font)
{
  SG_FONT *fs = NULL;
  state_context *sgx;
  if (ss->using_schemes) return(FALSE);
  sgx = ss->sgx;
  fs = SG_FONT_LOAD(font);
  if (fs)
    {
      if (tiny_font(ss)) FREE(tiny_font(ss));
      in_set_tiny_font(ss, copy_string(font));
      sgx->tiny_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_listener_font(snd_state *ss, char *font)
{
  SG_FONT *fs = NULL;
  if (ss->using_schemes) return(FALSE);
  fs = SG_FONT_LOAD(font);
  if (fs)
    {
      if (listener_font(ss)) FREE(listener_font(ss));
      in_set_listener_font(ss, copy_string(font));
      (ss->sgx)->listener_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_button_font(snd_state *ss, char *font)
{
  SG_FONT *fs = NULL;
  if (ss->using_schemes) return(FALSE);
  fs = SG_FONT_LOAD(font);
  if (fs)
    {
      if (button_font(ss)) FREE(button_font(ss));
      in_set_button_font(ss, copy_string(font));
      (ss->sgx)->button_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_bold_button_font(snd_state *ss, char *font)
{
  SG_FONT *fs = NULL;
  if (ss->using_schemes) return(FALSE);
  fs = SG_FONT_LOAD(font);
  if (fs)
    {
      if (bold_button_font(ss)) FREE(bold_button_font(ss));
      in_set_bold_button_font(ss, copy_string(font));
      (ss->sgx)->bold_button_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_axis_label_font(snd_state *ss, char *font)
{
  SG_FONT *fs = NULL;
  if (ss->using_schemes) return(FALSE);
  fs = SG_FONT_LOAD(font);
  if (fs)
    {
      if (axis_label_font(ss)) FREE(axis_label_font(ss));
      in_set_axis_label_font(ss, copy_string(font));
      (ss->sgx)->axis_label_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_axis_numbers_font(snd_state *ss, char *font)
{
  SG_FONT *fs = NULL;
  if (ss->using_schemes) return(FALSE);
  fs = SG_FONT_LOAD(font);
  if (fs)
    {
      if (axis_numbers_font(ss)) FREE(axis_numbers_font(ss));
      in_set_axis_numbers_font(ss, copy_string(font));
      (ss->sgx)->axis_numbers_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}


void activate_numbers_font(axis_context *ax)
{
  SG_SET_FONT(ax->gc, AXIS_NUMBERS_FONT(ax->ss));
  ax->current_font = AXIS_NUMBERS_FONT(ax->ss);
}
   
void activate_button_font(axis_context *ax, snd_state *ss)
{
  SG_SET_FONT(ax->gc, (ss->sgx)->button_fnt);
  ax->current_font = (ss->sgx)->button_fnt;
}

void activate_label_font(axis_context *ax)
{
  SG_SET_FONT(ax->gc, AXIS_LABEL_FONT(ax->ss));
  ax->current_font = AXIS_LABEL_FONT(ax->ss);
}

int label_width(axis_context *ax, char *txt)
{
  if (txt)
    return(SG_TEXT_WIDTH(txt, AXIS_LABEL_FONT(ax->ss)));
  else return(0);
}

int mark_name_width(snd_state *ss, char *txt)
{
  if (txt)
    return(SG_TEXT_WIDTH(txt, (ss->sgx)->button_fnt));
  return(0);
}


int number_width(axis_context *ax, char *num)
{
  if (num)
    return(SG_TEXT_WIDTH(num, AXIS_NUMBERS_FONT(ax->ss)));
  return(0);
}

int number_height(axis_context *ax)
{
  gint lb, rb, asc, des, wid;
#if HAVE_GTK2
  gdk_text_extents(gdk_font_from_description(AXIS_NUMBERS_FONT(ax->ss)), "1", 1, &lb, &rb, &wid, &asc, &des);
#else
  gdk_text_extents(AXIS_NUMBERS_FONT(ax->ss), "1", 1, &lb, &rb, &wid, &asc, &des);
#endif
  return(asc + des);
}

int label_height(axis_context *ax)
{
  gint lb, rb, asc, des, wid;
#if HAVE_GTK2
  gdk_text_extents(gdk_font_from_description(AXIS_LABEL_FONT(ax->ss)), "1", 1, &lb, &rb, &wid, &asc, &des);
#else
  gdk_text_extents(AXIS_LABEL_FONT(ax->ss), "1", 1, &lb, &rb, &wid, &asc, &des);
#endif
  return(asc + des);
}

void clear_window(axis_context *ax)
{
  if (ax) gdk_window_clear(ax->wn);
}

void map_over_children (GtkWidget *w, void (*func)(GtkWidget *w, gpointer ptr), void *userptr)
{
  /* apply func to each child in entire tree beneath top widget */
  /* used mostly to get colors right in "convenience" widgets */

  if (w)
    {
      (*func)(w, userptr);
      if (GTK_IS_CONTAINER(w))
	gtk_container_foreach(GTK_CONTAINER(w), func, (gpointer)userptr);
    }
}

void set_background(GtkWidget *w, GdkColor *col)
{ 
  GtkStyle *style;
  snd_state *ss;
  ss = get_global_state();
  if (ss->using_schemes) return;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->bg[GTK_STATE_NORMAL].pixel = col->pixel;
  style->bg[GTK_STATE_NORMAL].red = col->red;
  style->bg[GTK_STATE_NORMAL].green = col->green;
  style->bg[GTK_STATE_NORMAL].blue = col->blue;
  gtk_widget_set_style(w, style);
}

void set_backgrounds(GtkWidget *w, GdkColor *col)
{ 
  GtkStyle *style;
  int i;
  snd_state *ss;
  ss = get_global_state();
  if (ss->using_schemes) return;
  style = gtk_style_copy(gtk_widget_get_style(w));
  for (i = 0; i < 5; i++)
    {
      style->bg[i].pixel = col->pixel;
      style->bg[i].red = col->red;
      style->bg[i].green = col->green;
      style->bg[i].blue = col->blue;
    }
  gtk_widget_set_style(w, style);
}

void set_active_color(GtkWidget *w, GdkColor *col)
{ 
  GtkStyle *style;
  snd_state *ss;
  ss = get_global_state();
  if (ss->using_schemes) return;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->bg[GTK_STATE_ACTIVE].pixel = col->pixel;
  style->bg[GTK_STATE_ACTIVE].red = col->red;
  style->bg[GTK_STATE_ACTIVE].green = col->green;
  style->bg[GTK_STATE_ACTIVE].blue = col->blue;
  gtk_widget_set_style(w, style);
}

void set_background_and_redraw(GtkWidget *w, GdkColor *col) 
{
  set_background(w, col);
  gtk_widget_queue_draw(w);
}

void set_foreground(GtkWidget *w, GdkColor *col)
{ 
  GtkStyle *style;
  snd_state *ss;
  ss = get_global_state();
  if (ss->using_schemes) return;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->fg[GTK_STATE_NORMAL].pixel = col->pixel;
  style->fg[GTK_STATE_NORMAL].red = col->red;
  style->fg[GTK_STATE_NORMAL].green = col->green;
  style->fg[GTK_STATE_NORMAL].blue = col->blue;
  gtk_widget_set_style(w, style);
}

void set_text_background(GtkWidget *w, GdkColor *col)
{ 
  GtkStyle *style;
  snd_state *ss;
  ss = get_global_state();
  if (ss->using_schemes) return;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->base[GTK_STATE_NORMAL].pixel = col->pixel;
  style->base[GTK_STATE_NORMAL].red = col->red;
  style->base[GTK_STATE_NORMAL].green = col->green;
  style->base[GTK_STATE_NORMAL].blue = col->blue;
  gtk_widget_set_style(w, style);
}

void set_pushed_button_colors(GtkWidget *w, snd_state *ss)
{
  GtkStyle *style;
  if (ss->using_schemes) return;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->bg[GTK_STATE_ACTIVE] = (*((ss->sgx)->pushed_button_color));
  style->bg[GTK_STATE_NORMAL] = (*((ss->sgx)->basic_color));
  gtk_widget_set_style(w, style);
}

void highlight_color(snd_state *ss, GtkWidget *w)
{
  set_background(w, (ss->sgx)->highlight_color);
}

void white_color(snd_state *ss, GtkWidget *w)
{
  set_background(w, (ss->sgx)->white);
}

void raise_dialog(GtkWidget *w)
{
  /* since we're using non-transient message dialogs, the dialog window can become completely
   * hidden behind other windows, with no easy way to raise it back to the top, so...
   */
  gtk_widget_show(w);
  gdk_window_raise(w->window);
}

void set_button_label_bold(GtkWidget *button, const char *str)
{
  GtkStyle *style;
  snd_state *ss;
  ss = get_global_state();
  if (ss->using_schemes) return;
  style = gtk_style_copy(gtk_widget_get_style(button));
#if HAVE_GTK2
  /* style->font_desc = (ss->sgx)->bold_button_fnt; */
#else
  style->font = (ss->sgx)->bold_button_fnt;
#endif
  gtk_widget_set_style(button, style);
  gtk_label_set_text(GTK_LABEL(GTK_BIN(button)->child), str);
}

void set_button_label(GtkWidget *label, const char *str)
{
  gtk_label_set_text(GTK_LABEL(GTK_BIN(label)->child), str);
}

void set_label(GtkWidget *label, const char *str)
{
  gtk_label_set_text(GTK_LABEL(label), str);
}


void check_for_event(snd_state *ss)
{
  /* this is needed to force label updates and provide interrupts for long computations */
  int i = 0;
  if (ss->checking_explicitly) return;
  ss->checking_explicitly = 1;
  while ((i < 50) && (gtk_events_pending()))
    {
      gtk_main_iteration();
      i++; /* don't hang! */
    }
  ss->checking_explicitly = 0;
}

int event_pending(snd_state *ss)
{
  return(gtk_events_pending());
}

void set_title(snd_state *ss, const char *title)
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


void color_cursor(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->cursor_color = color;
  gc_set_foreground_xor(sx->cursor_gc, color, sx->graph_color);
  gc_set_foreground_xor(sx->selected_cursor_gc, color, sx->selected_graph_color);
}

void color_marks(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->mark_color = color;
  gc_set_foreground_xor(sx->mark_gc, color, sx->graph_color);
  gc_set_foreground_xor(sx->selected_mark_gc, color, sx->selected_graph_color);
}

void color_selection(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selection_color = color;
  gc_set_foreground_xor(sx->selection_gc, color, sx->graph_color);
  gc_set_foreground_xor(sx->selected_selection_gc, color, sx->selected_graph_color);
}

void color_graph(snd_state *ss, GdkColor *color)
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

void color_selected_graph(snd_state *ss, GdkColor *color)
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

void color_data(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->data_color = color;
  gdk_gc_set_foreground(sx->basic_gc, color);
  gdk_gc_set_background(sx->erase_gc, color);
}

void color_selected_data(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selected_data_color = color;
  gdk_gc_set_foreground(sx->selected_basic_gc, color);
  gdk_gc_set_background(sx->selected_erase_gc, color);
}

void set_mix_color(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->mix_color = color;
  gdk_gc_set_foreground(sx->mix_gc, color);
}


void set_selected_mix_color(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selected_mix_color = color;
  gdk_gc_set_foreground(sx->selected_mix_gc, color);
}

void recolor_graph(chan_info *cp, int selected)
{
  snd_state *ss;
  state_context *sx;
  ss = cp->state;
  sx = ss->sgx;
  set_background(channel_graph(cp), (selected) ? sx->selected_graph_color : sx->graph_color);
}


void reflect_resize(snd_state *ss)
{
  SG_SET_RESIZABLE(GTK_WINDOW(MAIN_SHELL(ss)), auto_resize(ss));
}

void set_sensitive(GtkWidget *wid, int val) {if (wid) gtk_widget_set_sensitive(wid, val);}
int is_sensitive(GtkWidget *wid) {if (wid) return(GTK_WIDGET_IS_SENSITIVE(wid)); return(0);}

void set_toggle_button(GtkWidget *wid, int val, int passed, void *data) 
{
  if (!passed) gtk_signal_handler_block_by_data(GTK_OBJECT(wid), (gpointer)data);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wid), val);
  if (!passed) gtk_signal_handler_unblock_by_data(GTK_OBJECT(wid), (gpointer)data);
}

guint16 widget_height(GtkWidget *w)
{
  gint x, y;
  gdk_window_get_size(w->window, &x, &y);
  return(y);
}

guint16 widget_width(GtkWidget *w)
{
  gint x, y;
  gdk_window_get_size(w->window, &x, &y);
  return(x);
}

void set_widget_height(GtkWidget *w, guint16 height)
{
  SG_SET_SIZE(w, -2, height);
}

void set_widget_width(GtkWidget *w, guint16 width)
{
  SG_SET_SIZE(w, width, -2);
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
  SG_SET_POSITION(w, x, -2);
}

void set_widget_y(GtkWidget *w, gint16 y)
{
  SG_SET_POSITION(w, -2, y);
}

void set_widget_size(GtkWidget *w, guint16 width, guint16 height)
{
  SG_SET_SIZE(w, width, height);
}

void set_widget_position(GtkWidget *w, gint16 x, gint16 y)
{
  SG_SET_POSITION(w, x, y);
}

void fixup_axis_context(axis_context *ax, GtkWidget *w, GdkGC *gc)
{
  ax->wn = w->window;
  if (gc) ax->gc = gc;
  ax->current_font = AXIS_NUMBERS_FONT(ax->ss);
}

#define OUR_DATA "SndData"
void set_user_data(GtkObject *obj, gpointer data)
{
  /* apparently gtk_object_get|set_user_data are used internally in Gtk and aren't considered safe */
  gtk_object_set_data(obj, OUR_DATA, data);
}

gpointer get_user_data(GtkObject *obj)
{
  return(gtk_object_get_data(obj, OUR_DATA));
}

/* many changes between gtk 1.2 and 1.3, some of which are handled here, others in snd-g0.h */

#if HAVE_GTK2

char *sg_get_text(GtkWidget *w, int start, int end)
{
  GtkTextIter s, e;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &s, start);
  gtk_text_buffer_get_iter_at_offset(buf, &e, end);  /* this is utterly idiotic!!! */
  return(gtk_text_buffer_get_text(buf, &s, &e, TRUE));
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
  else SG_TEXT_CLEAR(w);
}

void sg_set_cursor(GtkWidget *w, int position)
{
  GtkTextIter pos;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &pos, position);
  gtk_text_buffer_place_cursor(buf, &pos);
}

int sg_cursor_position(GtkWidget *w)
{
  GtkTextMark *m;
  GtkTextIter pos;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  m = gtk_text_buffer_get_insert(buf);
  gtk_text_buffer_get_iter_at_mark(buf, &pos, m);
  /* free m? */
  return(gtk_text_iter_get_offset(&pos));
}

void sg_select_text(GtkWidget *w, int s0, int s1)
{
  /* The currently-selected text in @buffer is the region between the
   * "selection_bound" and "insert" marks. If "selection_bound" and
   * "insert" are in the same place, then there is no current selection.
   * gtk_text_buffer_get_selection_bounds() is another convenient function
   * for handling the selection, if you just want to know whether there's a
   * selection and what its bounds are.
   */
  GtkTextIter start, end;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_iter_set_offset(&start, s0);
  gtk_text_iter_set_offset(&end, s1);
  gtk_text_buffer_move_mark_by_name(buf, "selection_bound", &start);
  gtk_text_buffer_move_mark_by_name(buf, "insert", &end);
}

void sg_unselect_text(GtkWidget *w)
{
  GtkTextIter start;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_iter_set_offset(&start, sg_cursor_position(w));
  gtk_text_buffer_move_mark_by_name(buf, "selection_bound", &start);
}

#else

char *sg_label_text(GtkLabel *w) {char *text; gtk_label_get(w, &text); return(text);}

#endif

GtkWidget *make_scrolled_text(snd_state *ss, GtkWidget *parent, int editable, GtkWidget *boxer, GtkWidget *paner)
{
  /* returns new text widget */
#if HAVE_GTK2
  GtkWidget *sw, *new_text;
  sw = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  if (boxer)
    gtk_box_pack_start(GTK_BOX(boxer), sw, TRUE, TRUE, 4);
  new_text = gtk_text_view_new();
  gtk_container_add(GTK_CONTAINER (sw), new_text);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(new_text), editable);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(new_text), GTK_WRAP_NONE);
  gtk_widget_show(new_text);
  set_background((GTK_SCROLLED_WINDOW(sw))->hscrollbar, (ss->sgx)->position_color);
  set_background((GTK_SCROLLED_WINDOW(sw))->vscrollbar, (ss->sgx)->position_color);
  if (parent)
    gtk_container_add(GTK_CONTAINER(parent), sw);
  if (paner)
    gtk_paned_add2(GTK_PANED(paner), sw);
  gtk_widget_show(sw);
#else
  GtkWidget *table, *hscrollbar, *vscrollbar, *new_text;
  table = gtk_table_new(2, 2, FALSE);
  if (boxer)
    gtk_box_pack_start(GTK_BOX(boxer), table, TRUE, TRUE, 4);
  new_text = gtk_text_new(NULL, NULL);
  gtk_table_attach(GTK_TABLE (table), new_text, 0, 1, 0, 1, 
		   (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		   (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		   0, 0);
  gtk_text_set_editable(GTK_TEXT(new_text), editable);
  gtk_text_set_word_wrap(GTK_TEXT(new_text), FALSE);
  gtk_text_set_line_wrap(GTK_TEXT(new_text), FALSE); /* apparently horizontal scrolling is not yet implemented (gtktext.c version 1.2.8) */
  gtk_widget_show(new_text);
  hscrollbar = gtk_hscrollbar_new(GTK_TEXT(new_text)->hadj);
  set_background(hscrollbar, (ss->sgx)->position_color);
  gtk_table_attach(GTK_TABLE (table), hscrollbar, 0, 1, 1, 2, 
		   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL),
		   (GtkAttachOptions)(GTK_FILL), 
		   0, 0);
  gtk_widget_show(hscrollbar);
  vscrollbar = gtk_vscrollbar_new(GTK_TEXT(new_text)->vadj);
  set_background(vscrollbar, (ss->sgx)->position_color);
  gtk_table_attach(GTK_TABLE (table), vscrollbar, 1, 2, 0, 1, 
		   (GtkAttachOptions)(GTK_FILL), 
		   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
		   0, 0);
  gtk_widget_show(vscrollbar);
  if (parent)
    gtk_container_add(GTK_CONTAINER(parent), table);
  if (paner)
    gtk_paned_add2(GTK_PANED(paner), table);
  gtk_widget_show(table);
#endif
  return(new_text);
}

GtkWidget *sg_make_list(gpointer gp, int num_items, char **items, GtkSignalFunc callback)
{
  GtkWidget *list;
#if HAVE_GTK2 && 0
#else
  char *str;
  int i;
  list = gtk_clist_new(1);
  gtk_clist_set_selection_mode(GTK_CLIST(list), GTK_SELECTION_SINGLE);
  gtk_clist_set_shadow_type(GTK_CLIST(list), GTK_SHADOW_ETCHED_IN);
  gtk_clist_column_titles_passive(GTK_CLIST(list));
  for (i = 0; i < num_items; i++) 
    {
      str = items[i];
      gtk_clist_append(GTK_CLIST(list), &str);
    }
  gtk_signal_connect(GTK_OBJECT(list), "select_row", callback, gp);
#endif
  return(list);
}

