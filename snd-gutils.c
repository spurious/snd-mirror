#include "snd.h"

bool set_tiny_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  state_context *sgx;
  sgx = ss->sgx;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (tiny_font(ss)) FREE(tiny_font(ss));
      in_set_tiny_font(copy_string(font));
      if (TINY_FONT(ss)) pango_font_description_free(TINY_FONT(ss));
      TINY_FONT(ss) = fs;
      return(true);
    }
  return(false);
}

bool set_listener_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (listener_font(ss)) FREE(listener_font(ss));
      in_set_listener_font(copy_string(font));
      if (LISTENER_FONT(ss)) pango_font_description_free(LISTENER_FONT(ss));
      LISTENER_FONT(ss) = fs;
      set_listener_text_font();
      return(true);
    }
  return(false);
}

bool set_peaks_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (peaks_font(ss)) FREE(peaks_font(ss));
      in_set_peaks_font(copy_string(font));
      if (PEAKS_FONT(ss)) pango_font_description_free(PEAKS_FONT(ss));
      PEAKS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}

bool set_bold_peaks_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (bold_peaks_font(ss)) FREE(bold_peaks_font(ss));
      in_set_bold_peaks_font(copy_string(font));
      if (BOLD_PEAKS_FONT(ss)) pango_font_description_free(BOLD_PEAKS_FONT(ss));
      BOLD_PEAKS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}

bool set_axis_label_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (axis_label_font(ss)) FREE(axis_label_font(ss));
      in_set_axis_label_font(copy_string(font));
      if (AXIS_LABEL_FONT(ss)) pango_font_description_free(AXIS_LABEL_FONT(ss));
      AXIS_LABEL_FONT(ss) = fs;
#if HAVE_GL
      reload_label_font();
#endif
      return(true);
    }
  return(false);
}

bool set_axis_numbers_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (axis_numbers_font(ss)) FREE(axis_numbers_font(ss));
      in_set_axis_numbers_font(copy_string(font));
      if (AXIS_NUMBERS_FONT(ss)) pango_font_description_free(AXIS_NUMBERS_FONT(ss));
      AXIS_NUMBERS_FONT(ss) = fs;
#if HAVE_GL
      reload_number_font();
#endif
      return(true);
    }
  return(false);
}

int sg_text_width(const char *txt, PangoFontDescription *font)
{
  int wid = 0;
  PangoLayout *layout = NULL;
  PangoContext *ctx;
  if (txt == NULL) return(0);
  if (snd_strlen(txt) == 0) return(0);
  if (!(g_utf8_validate(txt, -1, NULL)))
    {
#if MUS_DEBUGGING
      fprintf(stderr,"text width: invalid UTF-8: %s\n", txt);
      abort();
#endif
      return(0);
    }
  ctx = gdk_pango_context_get();
  layout = pango_layout_new(ctx);
  if (layout)
    {
      pango_layout_set_font_description(layout, font);
      pango_layout_set_text(layout, txt, -1);
      pango_layout_get_pixel_size(layout, &wid, NULL);
      g_object_unref(G_OBJECT(layout));
    }
  g_object_unref(ctx);
  return(wid);
}

int mark_name_width(const char *txt)
{
  if (txt)
    return(sg_text_width(txt, PEAKS_FONT(ss)));
  return(0);
}

int label_width(const char *txt, bool use_tiny_font)
{
  if (txt)
    return(sg_text_width(txt, (use_tiny_font) ? TINY_FONT(ss) : AXIS_LABEL_FONT(ss)));
  else return(0);
}

int number_width(const char *num, bool use_tiny_font)
{
  if (num)
    return(sg_text_width(num, (use_tiny_font) ? TINY_FONT(ss) : AXIS_NUMBERS_FONT(ss)));
  return(0);
}

#if 0
static int sg_font_width(PangoFontDescription *font)
{
  /* returns size in pixels */
  int wid = 0;
  double dpi = 96.0; /* see below */
  PangoContext *ctx;
  PangoFontMetrics *m;

#if HAVE_GTK_LINK_BUTTON_NEW
  dpi = gdk_screen_get_resolution(gdk_display_get_default_screen(gdk_display_get_default())); /* pixels/inch */
#endif

  ctx = gdk_pango_context_get();
  m = pango_context_get_metrics(ctx, font, gtk_get_default_language()); /* returns size in pango-scaled points (1024/72 inch) */
  wid = (int)((dpi / 72.0) * PANGO_PIXELS(pango_font_metrics_get_approximate_char_width(m)));
  pango_font_metrics_unref(m);
  g_object_unref(ctx);
  return(wid);
}
#endif

static int sg_font_height(PangoFontDescription *font)
{
  /* returns size in pixels */
  double dpi = 96.0; /* a plausible guess */
  int hgt = 0;
  PangoContext *ctx;
  PangoFontMetrics *m;

#if HAVE_GTK_LINK_BUTTON_NEW
  /* gtk 2.1: gdk_display_get_default, gdk_display_get_default_screen */
  /* gtk 2.9: gdk_screen_get_resolution */
  dpi = gdk_screen_get_resolution(gdk_display_get_default_screen(gdk_display_get_default()));
#endif

  ctx = gdk_pango_context_get();
  m = pango_context_get_metrics(ctx, font, gtk_get_default_language());
  hgt = (int)((dpi / 72.0) * PANGO_PIXELS(pango_font_metrics_get_ascent(m)));
  pango_font_metrics_unref(m);
  g_object_unref(ctx);
      
  return(hgt);
}

static PangoFontDescription *last_tiny_font = NULL, *last_numbers_font = NULL, *last_label_font = NULL;
static int last_numbers_height = 14, last_tiny_height = 10, last_label_height = 14;

int number_height(bool use_tiny_font)
{
  int hgt = 14;
  if (use_tiny_font)
    {
      if (last_tiny_font == TINY_FONT(ss))
	return(last_tiny_height);
      hgt = sg_font_height(TINY_FONT(ss));
      last_tiny_font = TINY_FONT(ss);
      last_tiny_height = hgt;
    }
  else
    {
      if (last_numbers_font == AXIS_NUMBERS_FONT(ss))
	return(last_numbers_height);
      hgt = sg_font_height(AXIS_NUMBERS_FONT(ss));
      last_numbers_font = AXIS_NUMBERS_FONT(ss);
      last_numbers_height = hgt;
    }
  return(hgt);
}

int label_height(bool use_tiny_font)
{
  int hgt = 14;
  if (use_tiny_font)
    {
      if (last_tiny_font == TINY_FONT(ss))
	return(last_tiny_height);
      hgt = sg_font_height(TINY_FONT(ss));
      last_tiny_font = TINY_FONT(ss);
      last_tiny_height = hgt;
    }
  else
    {
      if (last_label_font == AXIS_LABEL_FONT(ss))
	return(last_label_height);
      hgt = sg_font_height(AXIS_LABEL_FONT(ss));
      last_label_font = AXIS_LABEL_FONT(ss);
      last_label_height = hgt;
    }
  return(hgt);
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
  gtk_window_present(GTK_WINDOW(w));
}

static void set_stock_button_label_1(gpointer w1, gpointer label)
{
  GtkWidget *w = (GtkWidget *)w1;
  if (GTK_IS_LABEL(w))
    {
      gtk_widget_hide(w);
      gtk_label_set_text(GTK_LABEL(w), (char *)label);
      gtk_widget_show(w);
    }
  else
    {
      if (GTK_IS_CONTAINER(w))
	g_list_foreach(gtk_container_get_children(GTK_CONTAINER(w)), set_stock_button_label_1, label);
    }
}

void set_stock_button_label(GtkWidget *w, const char *new_label)
{
  set_stock_button_label_1((gpointer)w, (gpointer)new_label);
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

  /*   but... there is apparently a memory leak here -- gdk calls XTranslateCoordinates,
   *   which calls malloc for something, and according to Valgrind, this stuff just builds up:
   *
   *   ==22804== 53,294,592 bytes in 512,448 blocks are still reachable in loss record 886 of 886
   *   ==22804==    at 0x4020396: malloc (vg_replace_malloc.c:149)
   *   ==22804==    by 0x499FB1B: _XEnq (in /usr/lib/libX11.so.6.2.0)                   place packet on event queue
   *   ==22804==    by 0x49A14B5: _XReply (in /usr/lib/libX11.so.6.2.0)                 wait for reply packet
   *   ==22804==    by 0x4999A73: XTranslateCoordinates (in /usr/lib/libX11.so.6.2.0)
   *   ==22804==    by 0x449E199: gdk_event_translate (gdkevents-x11.c:1865)
   *   ==22804==    by 0x449E3D6: _gdk_events_queue (gdkevents-x11.c:2254)
   *   ==22804==    by 0x449E79E: gdk_event_dispatch (gdkevents-x11.c:2314)
   *   ==22804==    by 0x461A5B1: g_main_context_dispatch (gmain.c:2045)
   *   ==22804==    by 0x461D57E: g_main_context_iterate (gmain.c:2677)
   *   ==22804==    by 0x461DAE4: g_main_context_iteration (gmain.c:2736)
   *   ==22804==    by 0x424A383: gtk_main_iteration (gtkmain.c:1084)
   *   ==22804==    by 0x81D6507: check_for_event (snd-gutils.c:272)
   *
   * this happens (to the same extent) even in shorter cases
   */

  int i = 0;
  if (ss->checking_explicitly) return;
  ss->checking_explicitly = true;
  while ((i < 500) && (gtk_events_pending())) /* was 50, but gtk generates a huge number of these events */
    {
      gtk_main_iteration();
      i++; /* don't hang! */
    }
  ss->checking_explicitly = false;
}

void force_update(GtkWidget *wid)
{
  if ((wid) && (wid->window))
    {
      gdk_window_invalidate_rect(GDK_WINDOW(wid->window), NULL, true);
      gdk_window_process_updates(GDK_WINDOW(wid->window), true);
    }
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

void reset_user_int_data(GObject *obj, int data)
{
  gpointer gdata;
  gdata = g_object_get_data(obj, "snd-data");
  ((int *)gdata)[0] = data;
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

void sg_text_insert(GtkWidget *w, const char *text)
{
  if (text)
    {
      GtkTextIter pos;
      GtkTextBuffer *buf;
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

GtkWidget *make_scrolled_text(GtkWidget *parent, bool editable, GtkWidget *paner, bool resize)
{
  /* returns new text widget */
  GtkWidget *sw, *new_text;
  GtkTextBuffer *buf;
  sw = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
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
    gtk_paned_pack2(GTK_PANED(paner), sw, resize, true);
  if ((parent) || (paner)) gtk_widget_show(sw);
  return(new_text);
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
  g_object_ref(w); 
  return(w);
}

GtkWidget *snd_gtk_highlight_label_new(const char *label)
{
  GtkWidget *rlw;
  rlw = gtk_button_new_with_label(label);
  gtk_widget_set_name(rlw, "label_button");
  gtk_button_set_relief(GTK_BUTTON(rlw), GTK_RELIEF_HALF);
  return(rlw);
}

GtkWidget *snd_gtk_entry_label_new(const char *label, GdkColor *color)
{
  GtkWidget *rlw;
  rlw = gtk_entry_new();
  gtk_entry_set_has_frame(GTK_ENTRY(rlw), false);
  if (label) gtk_entry_set_text(GTK_ENTRY(rlw), label);
  gtk_editable_set_editable(GTK_EDITABLE(rlw), false);
  GTK_WIDGET_UNSET_FLAGS(GTK_WIDGET(rlw), GTK_CAN_FOCUS); /* turn off the $%#@$! blinking cursor */
  gtk_widget_modify_base(rlw, GTK_STATE_NORMAL, color);
  gtk_widget_modify_base(rlw, GTK_STATE_ACTIVE, color);
  return(rlw);
}


void widget_int_to_text(GtkWidget *w, int val)
{
  char *str;
  str = (char *)CALLOC(8, sizeof(char));
  mus_snprintf(str, 8, "%d", val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  FREE(str);
}

void widget_float_to_text(GtkWidget *w, Float val)
{
  char *str;
  str = (char *)CALLOC(8, sizeof(char));
  mus_snprintf(str, 8, "%.2f", val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  FREE(str);
}

void widget_off_t_to_text(GtkWidget *w, off_t val)
{
  char *str;
  str = (char *)CALLOC(8, sizeof(char));
  mus_snprintf(str, 8, OFF_TD, val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  FREE(str);
}

void rotate_text(GdkDrawable *wn, GdkGC *gc, PangoFontDescription *font, const char *text, int angle, gint x0, gint y0)
{
#if HAVE_PANGO_MATRIX_ROTATE
  PangoLayout *layout;
  PangoContext *context;
  PangoMatrix matrix = PANGO_MATRIX_INIT;
  context = gdk_pango_context_get();
  layout = pango_layout_new(context);
  pango_matrix_rotate(&matrix, angle);
  pango_context_set_matrix(context, &matrix);
  pango_layout_set_font_description(layout, font);
  pango_layout_set_text(layout, text, -1);
  gdk_draw_layout(wn, gc, x0, y0, layout);
  g_object_unref(layout);
  g_object_unref(context);
#endif
}

void draw_rotated_axis_label(chan_info *cp, GdkGC *gc, const char *text, gint x0, gint y0)
{
  GtkWidget *w;
  if ((cp->chan > 0) && (cp->sound->channel_style == CHANNELS_COMBINED))
    w = channel_graph(cp->sound->chans[0]);
  else w = channel_graph(cp);
  rotate_text(w->window, gc, AXIS_LABEL_FONT(ss), text, 90, x0, y0);
}

void ensure_scrolled_window_row_visible(widget_t list, int row, int num_rows)
{
  /* view files file list */
  /*   called in snd-file.c on vdat->file_list which is a vbox; its parent is a viewport */
  /* also used in slist_moveto below */
  GtkWidget *parent;
  GtkAdjustment *v;
  gdouble maximum, size, new_value, minimum;
  parent = gtk_widget_get_parent(list);
  v = gtk_viewport_get_vadjustment(GTK_VIEWPORT(parent));
  maximum = v->upper;
  minimum = v->lower;
  size = v->page_size;
  maximum -= size;
  if (row == 0)
    new_value = 0.0;
  else
    {
      if (row >= (num_rows - 1))
	new_value = maximum;
      else new_value = ((row + 0.5) * ((maximum - minimum) / (float)(num_rows - 1)));
    }
  if (new_value != v->value)
    gtk_adjustment_set_value(v, new_value);
}

GtkWidget *sg_button_new_from_stock_with_label(const char *text, gchar *stock_id)
{
  /* borrowed from gtk/gtkbutton.c */
  GtkWidget *button, *image, *label, *hbox, *align;
  button = gtk_button_new();
  label = gtk_label_new(text);
  image = (GtkWidget *)g_object_ref(gtk_image_new_from_stock(stock_id, GTK_ICON_SIZE_BUTTON));
  hbox = gtk_hbox_new(false, 2);
  align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
  gtk_box_pack_start(GTK_BOX(hbox), image, false, false, 0);
  gtk_box_pack_end(GTK_BOX(hbox), label, false, false, 0);
  gtk_container_add(GTK_CONTAINER(button), align);
  gtk_container_add(GTK_CONTAINER(align), hbox);
  gtk_widget_show_all(align);
  g_object_unref(image);
  return(button);
}
  


/* ---------------- scrolled list replacement ---------------- */

static int slist_row(GtkWidget *item);
static void slist_set_row(GtkWidget *item, int row);

static void slist_item_clicked(GtkWidget *w, gpointer gp)
{
  slist *lst = (slist *)gp;
  slist_select(lst, slist_row(w));
  if (lst->select_callback)
    (*(lst->select_callback))((const char *)gtk_button_get_label(GTK_BUTTON(w)), /* do not free this!! */
			      slist_row(w),
			      lst->select_callback_data);
}

static gboolean slist_item_button_pressed(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  slist *lst = (slist *)data;
  if (lst->button_press_callback)
    return((*(lst->button_press_callback))(ev, lst->button_press_callback_data));
  return(false);
}

static GtkWidget *slist_new_item(slist *lst, const char *label, int row)
{
  GtkWidget *item;
  item = gtk_button_new_with_label(label);
  gtk_widget_set_name(item, "white_button");
  slist_set_row(item, row);
  gtk_button_set_relief(GTK_BUTTON(item), GTK_RELIEF_HALF);
#if HAVE_GTK_BUTTON_SET_ALIGNMENT
  gtk_button_set_alignment(GTK_BUTTON(item), 0.05, 1.0);
#endif
  gtk_box_pack_start(GTK_BOX(lst->topics), item, false, false, 0);
  SG_SIGNAL_CONNECT(item, "clicked", slist_item_clicked, (gpointer)lst);
  SG_SIGNAL_CONNECT(item, "button_press_event", slist_item_button_pressed, (gpointer)lst);
  gtk_widget_show(item);
  return(item);
}

slist *slist_new_with_title_and_table_data(const char *title,
					   GtkWidget *parent, char **initial_items, int num_items, widget_add_t paned,
					   int t1, int t2, int t3, int t4)
{
  slist *lst;
  GtkWidget *topw = NULL;
  int i;
  lst = (slist *)CALLOC(1, sizeof(slist));
  lst->selected_item = SLIST_NO_ITEM_SELECTED;

  if (title)
    {
      lst->box = gtk_vbox_new(false, 0);

      lst->label = snd_gtk_highlight_label_new(title);
      gtk_box_pack_start(GTK_BOX(lst->box), lst->label, false, false, 0);
      topw = lst->box;
    }

  lst->topics = gtk_vbox_new(false, 0);
  lst->scroller = gtk_scrolled_window_new(NULL, NULL);
  if (!title) 
    topw = lst->scroller;
  else gtk_box_pack_start(GTK_BOX(lst->box), lst->scroller, true, true, 0);

  switch (paned)
    {
    case PANED_ADD1: 
      gtk_paned_add1(GTK_PANED(parent), topw);
      break;
    case PANED_ADD2: 
      gtk_paned_add2(GTK_PANED(parent), topw);
      break;
    case BOX_PACK: 
      gtk_box_pack_start(GTK_BOX(parent), topw, true, true, 0); 
      break;
    case TABLE_ATTACH: 
      gtk_table_attach(GTK_TABLE(parent), topw, t1, t2, t3, t4,
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      break;
    case CONTAINER_ADD: 
      gtk_container_add(GTK_CONTAINER(parent), topw); 
      break;
    }

  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(lst->scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(lst->scroller), lst->topics);

  if (title)
    {
      gtk_widget_show(lst->label);
      gtk_widget_show(lst->box);
    }
  gtk_widget_show(lst->topics);
  gtk_widget_show(lst->scroller);

  if (num_items > 0)
    {
      lst->items = (GtkWidget **)CALLOC(num_items, sizeof(GtkWidget *));
      lst->items_size = num_items;
      lst->num_items = num_items;
      for (i = 0; i < num_items; i++)
	lst->items[i] = slist_new_item(lst, initial_items[i], i);
    }
  return(lst);
}

slist *slist_new(GtkWidget *parent, char **initial_items, int num_items, widget_add_t paned)
{
  return(slist_new_with_title_and_table_data(NULL, parent, initial_items, num_items, paned, 0, 0, 0, 0));
}

slist *slist_new_with_title(const char *title, GtkWidget *parent, char **initial_items, int num_items, widget_add_t paned)
{
  return(slist_new_with_title_and_table_data(title, parent, initial_items, num_items, paned, 0, 0, 0, 0));
}

void slist_clear(slist *lst)
{
  int i;
  for (i = 0; i < lst->items_size; i++)
    if (lst->items[i])
      {
	gtk_widget_hide(lst->items[i]);
	gtk_button_set_label(GTK_BUTTON(lst->items[i]), " ");
      }
  lst->num_items = 0;
  if (lst->selected_item != SLIST_NO_ITEM_SELECTED)
    gtk_widget_modify_bg(lst->items[lst->selected_item], GTK_STATE_NORMAL, ss->sgx->white);
  lst->selected_item = SLIST_NO_ITEM_SELECTED;
}

static int slist_row(GtkWidget *item)
{
  gpointer gdata;
  gdata = g_object_get_data(G_OBJECT(item), "slist-row");
  return(((int *)gdata)[0]);
}

static void slist_set_row(GtkWidget *item, int row)
{
  int *gdata;
  gdata = (int *)MALLOC(sizeof(int));
  gdata[0] = row;
  g_object_set_data(G_OBJECT(item), "slist-row", (gpointer)gdata);
}

#define INITIAL_SLIST_LENGTH 8

void slist_append(slist *lst, const char *name)
{
  int loc = 0;
  if (lst->items_size == 0)
    {
      lst->items = (GtkWidget **)CALLOC(INITIAL_SLIST_LENGTH, sizeof(GtkWidget *));
      lst->items_size = INITIAL_SLIST_LENGTH;
      lst->num_items = 0;
    }
  if (lst->num_items == lst->items_size)
    {
      int i;
      lst->items_size += INITIAL_SLIST_LENGTH;
      lst->items = (GtkWidget **)REALLOC(lst->items, lst->items_size * sizeof(GtkWidget *));
      for (i = lst->num_items; i < lst->items_size; i++) lst->items[i] = NULL;
    }
  loc = lst->num_items++;
  if (lst->items[loc] == NULL)
    lst->items[loc] = slist_new_item(lst, name, loc);
  else 
    {
      gtk_button_set_label(GTK_BUTTON(lst->items[loc]), name); /* gtkbutton.c strdups name */
      gtk_widget_show(lst->items[loc]);
    }
}

void slist_moveto(slist *lst, int row)
{
  ensure_scrolled_window_row_visible(lst->topics, row, lst->num_items);
}

void slist_select(slist *lst, int row)
{
  if (lst->selected_item != SLIST_NO_ITEM_SELECTED)
    gtk_widget_modify_bg(lst->items[lst->selected_item], GTK_STATE_NORMAL, ss->sgx->white);
  if (row != SLIST_NO_ITEM_SELECTED)
    gtk_widget_modify_bg(lst->items[row], GTK_STATE_NORMAL, ss->sgx->light_blue);
  lst->selected_item = row;
}

char *slist_selection(slist *lst)
{
  if (lst->selected_item != SLIST_NO_ITEM_SELECTED)
    return((char *)gtk_button_get_label(GTK_BUTTON(lst->items[lst->selected_item])));
  return(NULL);
}
