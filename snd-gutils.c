#include "snd.h"

#if MUS_DEBUGGING
static int cairo_depth = 0;
#endif

#if HAVE_GTK_3
cairo_t *make_cairo(GdkWindow *win, const char *func, const char *file, int line)
#else
cairo_t *make_cairo(GdkDrawable *win, const char *func, const char *file, int line)
#endif
{
#if MUS_DEBUGGING
  cairo_depth++;
  /* fprintf(stderr, "make_cairo: %s %s[%d] %d\n", func, file, line, cairo_depth); */
  if (cairo_depth > 1)
    {
      fprintf(stderr, "make_cairo: %s %s[%d] %d\n", func, file, line, cairo_depth);
      /* abort(); */
    }
#endif
  ss->line_width = -1.0;
  return(gdk_cairo_create(win));
}

void free_cairo(cairo_t *cr, const char *func, const char *file, int line)
{
#if MUS_DEBUGGING
  cairo_depth--;
  /* fprintf(stderr, "free_cairo: %s %s[%d] %d\n", func, file, line, cairo_depth); */
#endif
  ss->line_width = -1.0;
  cairo_destroy(cr);
}


bool set_tiny_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false); /* pango accepts bogus font names, but then cairo segfaults trying to get the font height */
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (tiny_font(ss)) free(tiny_font(ss));
      in_set_tiny_font(mus_strdup(font));
      if (TINY_FONT(ss)) pango_font_description_free(TINY_FONT(ss));
      TINY_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


bool set_listener_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (listener_font(ss)) free(listener_font(ss));
      in_set_listener_font(mus_strdup(font));
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
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (peaks_font(ss)) free(peaks_font(ss));
      in_set_peaks_font(mus_strdup(font));
      if (PEAKS_FONT(ss)) pango_font_description_free(PEAKS_FONT(ss));
      PEAKS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


bool set_bold_peaks_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (bold_peaks_font(ss)) free(bold_peaks_font(ss));
      in_set_bold_peaks_font(mus_strdup(font));
      if (BOLD_PEAKS_FONT(ss)) pango_font_description_free(BOLD_PEAKS_FONT(ss));
      BOLD_PEAKS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


bool set_axis_label_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (axis_label_font(ss)) free(axis_label_font(ss));
      in_set_axis_label_font(mus_strdup(font));
      if (AXIS_LABEL_FONT(ss)) pango_font_description_free(AXIS_LABEL_FONT(ss));
      AXIS_LABEL_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


bool set_axis_numbers_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (axis_numbers_font(ss)) free(axis_numbers_font(ss));
      in_set_axis_numbers_font(mus_strdup(font));
      if (AXIS_NUMBERS_FONT(ss)) pango_font_description_free(AXIS_NUMBERS_FONT(ss));
      AXIS_NUMBERS_FONT(ss) = fs;
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
  if (mus_strlen(txt) == 0) return(0);
  if (!(g_utf8_validate(txt, -1, NULL)))
    return(0);

  ctx = gdk_pango_context_get();
  layout = pango_layout_new(ctx);
  if (layout)
    {
      pango_layout_set_font_description(layout, font);
      pango_layout_set_text(layout, txt, -1);
      pango_layout_get_pixel_size(layout, &wid, NULL); /* huge (6MBytes!) memleak here */
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


static PangoFontDescription *last_tiny_font = NULL, *last_numbers_font = NULL, *last_label_font = NULL, *last_peaks_font = NULL;
static int last_numbers_height = 14, last_tiny_height = 10, last_label_height = 14, last_peaks_height = 10;

int number_height(PangoFontDescription *font)
{
  int hgt = 14;
  if (font == TINY_FONT(ss))
    {
      if (last_tiny_font == TINY_FONT(ss))
	return(last_tiny_height);
      hgt = sg_font_height(TINY_FONT(ss));
      last_tiny_font = TINY_FONT(ss);
      last_tiny_height = hgt;
    }
  else
    {
      if (font == AXIS_NUMBERS_FONT(ss))
	{
	  if (last_numbers_font == AXIS_NUMBERS_FONT(ss))
	    return(last_numbers_height);
	  hgt = sg_font_height(AXIS_NUMBERS_FONT(ss));
	  last_numbers_font = AXIS_NUMBERS_FONT(ss);
	  last_numbers_height = hgt;
	}
      else
	{
	  if (last_peaks_font == PEAKS_FONT(ss))
	    return(last_peaks_height);
	  hgt = sg_font_height(PEAKS_FONT(ss));
	  last_peaks_font = PEAKS_FONT(ss);
	  last_peaks_height = hgt;
	}
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


void clear_window(graphics_context *ax)
{
  /* if (ax) gdk_window_clear(ax->wn); */
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
  gtk_label_set_text(GTK_LABEL(BIN_CHILD(label)), str);
}


void set_label(GtkWidget *label, const char *str)
{
  gtk_label_set_text(GTK_LABEL(label), str);
}


void sg_left_justify_button(GtkWidget *button)
{
  gfloat x, y;
  gtk_misc_get_alignment(GTK_MISC(GTK_LABEL(BIN_CHILD(button))), &x, &y);
  gtk_misc_set_alignment(GTK_MISC(GTK_LABEL(BIN_CHILD(button))), 0.05, y);
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
  /* this is needed to force label updates and provide interrupts for long computations
   *
   *   Valgrind is confused about something here -- it thinks _XEnq malloc in XTranslateCoordinates in gdk_event_translate is never freed
   *   but this way of letting events run is used (for example) in gtktreeview.c and gtkwidget.c, so if it's wrong here...
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


/* try to keep track of colors */
void gc_set_foreground(gc_t *gp, color_info *color)
{
  gp->fg_color = color;
}


void gc_set_background(gc_t *gp, color_info *color)
{
  gp->bg_color = color;
}


void gc_set_colors(gc_t *gp, color_info *col1, color_info *col2)
{ 
  gp->fg_color = col1;
  gp->bg_color = col2;
}


gc_t *gc_new(void)
{
  gc_t *gp;
  gp = (gc_t *)calloc(1, sizeof(gc_t));
  return(gp);
}


void color_cursor(color_info *color)
{
  ss->cursor_color = color;
  gc_set_colors(ss->cursor_gc, color, ss->graph_color);
  gc_set_colors(ss->selected_cursor_gc, color, ss->selected_graph_color);
}


void color_marks(color_info *color)
{
  ss->mark_color = color;
  gc_set_colors(ss->mark_gc, color, ss->graph_color);
  gc_set_colors(ss->selected_mark_gc, color, ss->selected_graph_color);
}


void color_selection(color_info *color)
{
  ss->selection_color = color;
  gc_set_colors(ss->selection_gc, color, ss->graph_color);
  gc_set_colors(ss->selected_selection_gc, color, ss->selected_graph_color);
}


void color_graph(color_info *color)
{
  ss->graph_color = color;
  gc_set_background(ss->basic_gc, color);
  gc_set_foreground(ss->erase_gc, color);
  gc_set_colors(ss->selection_gc, ss->selection_color, color);
  gc_set_colors(ss->cursor_gc, ss->cursor_color, color);
  gc_set_colors(ss->mark_gc, ss->mark_color, color);
}


void color_selected_graph(color_info *color)
{
  ss->selected_graph_color = color;
  gc_set_background(ss->selected_basic_gc, color);
  gc_set_foreground(ss->selected_erase_gc, color);
  gc_set_colors(ss->selected_selection_gc, ss->selection_color, color);
  gc_set_colors(ss->selected_cursor_gc, ss->cursor_color, color);
  gc_set_colors(ss->selected_mark_gc, ss->mark_color, color);
}


void color_data(color_info *color)
{
  ss->data_color = color;
  gc_set_foreground(ss->basic_gc, color);
  gc_set_background(ss->erase_gc, color);
}


void color_selected_data(color_info *color)
{
  ss->selected_data_color = color;
  gc_set_foreground(ss->selected_basic_gc, color);
  gc_set_background(ss->selected_erase_gc, color);
}


void set_mix_color(color_info *color)
{
  ss->mix_color = color;
  gc_set_foreground(ss->mix_gc, color);
}


color_t rgb_to_color(mus_float_t r, mus_float_t g, mus_float_t b)
{
  color_info *ccolor;
  ccolor = (color_info *)malloc(sizeof(color_info));
  ccolor->red = r;
  ccolor->green = g;
  ccolor->blue = b;
  ccolor->alpha = 1.0;
  return(ccolor);
}


#if (!HAVE_GTK_3)
GdkColor *rgb_to_gdk_color(color_t col)
{
  GdkColor gcolor;
  GdkColor *ccolor;
  gcolor.red = (unsigned short)(col->red * 65535);
  gcolor.green = (unsigned short)(col->green * 65535);
  gcolor.blue = (unsigned short)(col->blue * 65535);
  ccolor = gdk_color_copy(&gcolor);
  /* gdk_rgb_find_color(gdk_colormap_get_system(), ccolor); */
  return(ccolor);
}
#endif


void widget_modify_bg(GtkWidget *w, GtkStateType type, color_t color)
{
  /* the color has to stick around??? */
  /* another stop-gap: allocate a color each time... */
#if (!HAVE_GTK_3)
  gtk_widget_modify_bg(w, type, rgb_to_gdk_color(color));
#else
  gtk_widget_override_background_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)color);
#endif
}


void widget_modify_fg(GtkWidget *w, GtkStateType type, color_t color)
{
#if (!HAVE_GTK_3)
  gtk_widget_modify_fg(w, type, rgb_to_gdk_color(color));
#else
  gtk_widget_override_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)color);
#endif
}


void widget_modify_base(GtkWidget *w, GtkStateType type, color_t color)
{
#if (!HAVE_GTK_3)
  gtk_widget_modify_base(w, type, rgb_to_gdk_color(color));
#else
  gtk_widget_override_background_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)color);
#endif
}


void recolor_graph(chan_info *cp, bool selected)
{
  widget_modify_bg(channel_graph(cp), GTK_STATE_NORMAL, (selected) ? ss->selected_graph_color : ss->graph_color);
}


void set_sensitive(GtkWidget *wid, bool val) 
{
  if (wid) 
    gtk_widget_set_sensitive(wid, val);
}


void set_toggle_button(GtkWidget *wid, bool val, bool passed, void *data) 
{
  if (!passed) g_signal_handlers_block_matched((gpointer)wid, G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)data);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wid), val);
  if (!passed) g_signal_handlers_unblock_matched((gpointer)wid, G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)data);
}


#if HAVE_GTK_3

int widget_height(GtkWidget *w)
{
  return(gtk_widget_get_allocated_height(w));
}


int widget_width(GtkWidget *w)
{
  return(gtk_widget_get_allocated_width(w));
}

#else

guint16 widget_height(GtkWidget *w)
{
  gint x, y;
  gdk_drawable_get_size(WIDGET_TO_WINDOW(w), &x, &y);
  return(y);
}


guint16 widget_width(GtkWidget *w)
{
  gint x, y;
  gdk_drawable_get_size(WIDGET_TO_WINDOW(w), &x, &y);
  return(x);
}
#endif


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
  gdk_window_get_position(WIDGET_TO_WINDOW(w), &x, &y);
  return(x);
}


gint16 widget_y(GtkWidget *w)
{
  gint x, y;
  gdk_window_get_position(WIDGET_TO_WINDOW(w), &x, &y);
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
  gdk_window_resize(WIDGET_TO_WINDOW(w), width, height);
#if 0
  /* This one doesn't do anything, and prints out errors. */
  gtk_window_resize(GTK_WINDOW(w), width, height);
#endif
}


void set_widget_position(GtkWidget *w, gint16 x, gint16 y)
{
  gtk_window_move(GTK_WINDOW(w), x, y);
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
  gdata = (int *)malloc(sizeof(int));
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


char *sg_get_text(GtkWidget *w, int start, int end) /* g_free result */
{
  GtkTextIter s, e;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &s, start);
  gtk_text_buffer_get_iter_at_offset(buf, &e, end);
  return(gtk_text_buffer_get_text(buf, &s, &e, true));
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


GtkWidget *make_scrolled_text(GtkWidget *parent, bool editable, int add_choice, bool resize)
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

  switch (add_choice)
    {
    case 0: 
      gtk_container_add(GTK_CONTAINER(parent), sw);
      break;
    case 1:
      gtk_paned_pack2(GTK_PANED(parent), sw, resize, true);
      break;
    case 2:
    default:
      gtk_box_pack_start(GTK_BOX(parent), sw, true, true, 0);
      break;
    }
  gtk_widget_show(sw);

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


idle_t add_work_proc(GSourceFunc func, gpointer data)
{
  /* during auto-testing I need to force the background procs to run to completion */
  if (with_background_processes(ss))
    return(g_idle_add_full(G_PRIORITY_DEFAULT_IDLE, func, data, NULL));
  else
    {
      while (((*func)(data)) == BACKGROUND_CONTINUE) {};
      return((idle_t)0);
    }
}


GtkWidget *snd_gtk_dialog_new(void)
{
  GtkWidget *w;
  w = gtk_dialog_new();
  add_dialog_style(w);
  g_object_ref(w); 
  return(w);
}


GtkWidget *snd_gtk_highlight_label_new(const char *label)
{
  char *str;
  GtkWidget *w;

  w = gtk_label_new(label);
  str = mus_format("<b>%s</b>", label);
  gtk_label_set_markup(GTK_LABEL(w), str);
  gtk_label_set_use_markup(GTK_LABEL(w), true);
  free(str);

  return(w);
}


GtkWidget *snd_gtk_entry_label_new(const char *label, color_info *color)
{
  GtkWidget *rlw;
  rlw = gtk_entry_new();
  gtk_entry_set_has_frame(GTK_ENTRY(rlw), false);
  if (label) gtk_entry_set_text(GTK_ENTRY(rlw), label);
  gtk_editable_set_editable(GTK_EDITABLE(rlw), false);
  UNSET_CAN_FOCUS(GTK_WIDGET(rlw)); /* turn off the $%#@$! blinking cursor */
  widget_modify_base(rlw, GTK_STATE_NORMAL, color);
  widget_modify_base(rlw, GTK_STATE_ACTIVE, color);
  return(rlw);
}


GtkWidget *make_info_widget(void)
{
  return(snd_gtk_entry_label_new(NULL, ss->highlight_color));
}


void info_widget_display(GtkWidget *w, const char *message)
{
  gtk_entry_set_text(GTK_ENTRY(w), message);
}


void info_widget_set_size(GtkWidget *w, int size)
{
  gtk_entry_set_width_chars(GTK_ENTRY(w), size);
}


void widget_int_to_text(GtkWidget *w, int val)
{
  char *str;
  str = (char *)calloc(8, sizeof(char));
  mus_snprintf(str, 8, "%d", val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  free(str);
}

#if 0
void widget_float_to_text(GtkWidget *w, mus_float_t val)
{
  char *str;
  str = (char *)calloc(8, sizeof(char));
  mus_snprintf(str, 8, "%.2f", val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  free(str);
}
#endif


void widget_mus_long_t_to_text(GtkWidget *w, mus_long_t val)
{
  char *str;
  str = (char *)calloc(8, sizeof(char));
  mus_snprintf(str, 8, MUS_LD, val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  free(str);
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
#if HAVE_GTK_3
  v = gtk_scrollable_get_vadjustment(GTK_SCROLLABLE(parent));
#else
  v = gtk_viewport_get_vadjustment(GTK_VIEWPORT(parent));
#endif
  maximum = ADJUSTMENT_UPPER(v);
  minimum = ADJUSTMENT_LOWER(v);
  size = ADJUSTMENT_PAGE_SIZE(v);
  maximum -= size;
  if (row == 0)
    new_value = 0.0;
  else
    {
      if (row >= (num_rows - 1))
	new_value = maximum;
      else new_value = ((row + 0.5) * ((maximum - minimum) / (float)(num_rows - 1)));
    }
  if (new_value != ADJUSTMENT_VALUE(v))
    ADJUSTMENT_SET_VALUE(v, new_value);
}


GtkWidget *sg_button_new_from_stock_with_label(const char *text, const gchar *stock_id)
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


#if HAVE_GTK_3
static GtkCssProvider *wb_provider, *listener_provider, *dialog_provider;
void add_white_button_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(wb_provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  gtk_widget_set_name(w, "white_button");
}

void add_listener_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(listener_provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  gtk_widget_set_name(w, "listener_text");
}

void add_dialog_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(dialog_provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
}
#else
void add_white_button_style(GtkWidget *w) 
{
  gtk_widget_set_name(w, "white_button");
}

void add_listener_style(GtkWidget *w)
{
  gtk_widget_set_name(w, "listener_text");
}

void add_dialog_style(GtkWidget *w)
{
}
#endif

static GtkWidget *slist_new_item(slist *lst, const char *label, int row)
{
  GtkWidget *item;

  item = gtk_button_new_with_label(label);
  slist_set_row(item, row);
  gtk_button_set_relief(GTK_BUTTON(item), GTK_RELIEF_HALF);
  gtk_button_set_alignment(GTK_BUTTON(item), 0.05, 1.0);
  gtk_box_pack_start(GTK_BOX(lst->topics), item, false, false, 0);

  widget_modify_bg(item, GTK_STATE_NORMAL, ss->white);
  widget_modify_bg(item, GTK_STATE_PRELIGHT, ss->light_blue);
  add_white_button_style(item);

  SG_SIGNAL_CONNECT(item, "clicked", slist_item_clicked, (gpointer)lst);
  SG_SIGNAL_CONNECT(item, "button_press_event", slist_item_button_pressed, (gpointer)lst);

  gtk_widget_show(item);
  return(item);
}


slist *slist_new_with_title_and_table_data(const char *title,
					   GtkWidget *parent, 
					   const char **initial_items, 
					   int num_items, 
					   widget_add_t paned,
					   int t1, int t2, int t3, int t4)
{
  slist *lst;
  GtkWidget *topw = NULL;
  int i;
  lst = (slist *)calloc(1, sizeof(slist));
  lst->selected_item = SLIST_NO_ITEM_SELECTED;

  if (title)
    {
      lst->box = gtk_vbox_new(false, 0);

      lst->label = snd_gtk_highlight_label_new(title);
      gtk_box_pack_start(GTK_BOX(lst->box), lst->label, false, false, 0);
      topw = lst->box;
    }

  lst->topics = gtk_vbox_new(false, 2); /* sets list item vertical spacing */
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
      gtk_box_pack_start(GTK_BOX(parent), topw, true, true, 4); 
      break;

    case BOX_PACK_END: 
      gtk_box_pack_end(GTK_BOX(parent), topw, false, false, 4); 
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
      lst->items = (GtkWidget **)calloc(num_items, sizeof(GtkWidget *));
      lst->items_size = num_items;
      lst->num_items = num_items;
      for (i = 0; i < num_items; i++)
	lst->items[i] = slist_new_item(lst, initial_items[i], i);
    }
  return(lst);
}


slist *slist_new(GtkWidget *parent, const char **initial_items, int num_items, widget_add_t paned)
{
  return(slist_new_with_title_and_table_data(NULL, parent, initial_items, num_items, paned, 0, 0, 0, 0));
}


slist *slist_new_with_title(const char *title, GtkWidget *parent, const char **initial_items, int num_items, widget_add_t paned)
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
    widget_modify_bg(lst->items[lst->selected_item], GTK_STATE_NORMAL, ss->white);
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
  gdata = (int *)malloc(sizeof(int));
  gdata[0] = row;
  g_object_set_data(G_OBJECT(item), "slist-row", (gpointer)gdata);
}


#define INITIAL_SLIST_LENGTH 8

void slist_append(slist *lst, const char *name)
{
  int loc = 0;
  if (lst->items_size == 0)
    {
      lst->items = (GtkWidget **)calloc(INITIAL_SLIST_LENGTH, sizeof(GtkWidget *));
      lst->items_size = INITIAL_SLIST_LENGTH;
      lst->num_items = 0;
    }
  if (lst->num_items == lst->items_size)
    {
      int i;
      lst->items_size += INITIAL_SLIST_LENGTH;
      lst->items = (GtkWidget **)realloc(lst->items, lst->items_size * sizeof(GtkWidget *));
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
    widget_modify_bg(lst->items[lst->selected_item], GTK_STATE_NORMAL, ss->white);
  if (row != SLIST_NO_ITEM_SELECTED)
    widget_modify_bg(lst->items[row], GTK_STATE_NORMAL, ss->light_blue);
  lst->selected_item = row;
}


char *slist_selection(slist *lst)
{
  if (lst->selected_item != SLIST_NO_ITEM_SELECTED)
    return((char *)gtk_button_get_label(GTK_BUTTON(lst->items[lst->selected_item])));
  return(NULL);
}

/* (any place labels are erased see gtk_bin_get_child and snd-gregion.c) */


#if (!HAVE_GTK_3)

void init_gtk(void)
{
  gtk_rc_parse_string("\n\
\n					 \
style \"default\"\n			 \
{\n					 \
  fg[NORMAL]      = { 0.0,  0.00, 0.0 }\n	\
  text[NORMAL]    = { 0.0,  0.0,  0.0 }\n	\
  bg[NORMAL]      = { 0.96, 0.96, 0.90 }\n	\
  bg[ACTIVE]      = { 0.80, 0.80, 0.75 }\n	\
  bg[INSENSITIVE] = { 0.96, 0.96, 0.90 }\n	\
  base[NORMAL]    = { 1.00, 1.00, 1.00 }\n	\
  bg[PRELIGHT]    = { 0.70, 0.70, 0.64 }\n	\
  fg[PRELIGHT]    = { 1.0,  0.0,  0.0}\n	\
  GtkPaned::handle_size = 6\n			\
  xthickness = 1\n				\
  ythickness = 1\n				\
}\n						\
style \"default_button\" = \"default\"\n	\
{\n						\
  GtkButton::default_border = { 1, 0, 1, 0 }\n		\
  GtkButton::default_outside_border = { 1, 0, 1, 0 }\n	\
  GtkButton::inner_border = { 1, 0, 1, 0 }\n		\
  GtkButton::focus_line_width = 0\n			\
  GtkButton::focus_padding = 0\n			\
}\n							\
style \"default_pane\" = \"default\"\n			\
{\n							\
  bg[NORMAL] = { 0.56, 0.93, 0.56 }\n			\
  bg[PRELIGHT] = { 0.26, 0.8, 0.26}\n			\
}\n							\
style \"default_entry\" = \"default\"\n			\
{\n							\
  base[ACTIVE]      = { 0.96, 0.96, 0.90 }\n		\
  base[SELECTED]    = { 0.80, 0.80, 0.75 }\n		\
  base[PRELIGHT]    = { 1.0, 1.0, 1.0}\n		\
  base[NORMAL]      = { 0.96, 0.96, 0.90 }\n		\
  base[INSENSITIVE] = { 0.96, 0.96, 0.90 }\n		\
  bg[ACTIVE]        = { 1.0, 1.0, 1.0 }\n		\
  bg[SELECTED]      = { 1.0, 1.0, 1.0 }\n		\
  bg[PRELIGHT]      = { 1.0, 1.0, 1.0 }\n		\
  text[ACTIVE]      = { 0.0, 0.0, 0.0 }\n		\
  text[SELECTED]    = { 0.0, 0.0, 0.0 }\n		\
  text[PRELIGHT]    = { 0.0, 0.0, 0.0 }\n		\
}\n							\
style \"default_text\" = \"default_entry\"\n		\
{\n							\
  base[NORMAL] = { 1.0, 1.0, 1.0 }\n			\
}\n							\
style \"default_slider\" = \"default\"\n		\
{\n							\
  bg[NORMAL] = { 0.90, 0.90, 0.85 }\n			\
  bg[ACTIVE] = { 0.70, 0.70, 0.64 }\n			\
  bg[PRELIGHT] = { 0.90, 0.90, 0.85 }\n			\
  GtkRange::slider_width = 13\n				\
  GtkRange::stepper_size = 10\n				\
}\n							\
style \"prefs_scale\" = \"default_slider\"\n            \
{\n                                                     \
  GtkScale::slider-length = 24\n                        \
}\n                                                     \
widget \"*.prefs_color_scale\" style \"prefs_scale\"\n  \
style \"default_frame\" = \"default\"\n			\
{\n							\
  xthickness = 4\n					\
  ythickness = 4\n					\
}\n							\
class \"GtkWidget\" style \"default\"\n			\
class \"GtkButton\" style \"default_button\"\n		\
class \"GtkEntry\" style \"default_entry\"\n		\
class \"GtkTextView\" style \"default_text\"\n		\
class \"GtkPaned\" style \"default_pane\"\n		\
class \"GtkRange\" style \"default_slider\"\n		\
class \"GtkFrame\" style \"default_frame\"\n		\
\n							\
style \"zoom_slider\" = \"default_slider\"\n		\
{\n							\
  bg[NORMAL] = { 0.70, 0.70, 0.64 }\n			\
  bg[ACTIVE] = { 0.54, 0.54, 0.51 }\n			\
  bg[PRELIGHT] = { 0.70, 0.70, 0.64 }\n			\
\n							\
  GtkRange::slider_width = 12\n				\
  GtkRange::stepper_size = 12\n				\
}\n							\
widget \"*.zx_slider\" style \"zoom_slider\"\n		\
widget \"*.zy_slider\" style \"zoom_slider\"\n		\
widget \"*.gzy_slider\" style \"zoom_slider\"\n		\
style \"dialog_button\" = \"default_button\"\n          \
{\n                                                     \
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n                     \
  bg[PRELIGHT] = { 1.0, 1.0, 0.94 }\n                   \
}\n                                                     \
widget \"*.dialog_button\" style \"dialog_button\"\n    \
style \"white_button\" = \"default_button\"\n		\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 1.0 }\n			\
  bg[PRELIGHT] = { 0.94, 0.97, 1.0 }\n			\
  fg[PRELIGHT] = { 0.0,  0.0,  0.0}\n			\
  GtkButton::default_border = { 0, 0, 0, 0 }\n		\
  GtkButton::default_outside_border = { 0, 0, 0, 0 }\n	\
  GtkButton::inner_border = { 0, 0, 0, 0 }\n		\
  GtkButton::focus_line_width = 0\n			\
  GtkButton::focus_padding = 0\n			\
  xthickness = 0\n					\
  ythickness = 0\n					\
}\n							\
widget \"*.white_button\" style \"white_button\"\n");
}

#else

void init_gtk(void)
{
  wb_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(wb_provider),
    "GtkButton#white_button { \n"
    "  padding-top: 0;\n"
    "  padding-bottom: 0;\n"
    "  border-width: 0;\n"
    "  background-color: #ffffff;\n"
    "}\n"
    "GtkButton#white_button:prelight { \n"
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(#ffffff), to(rgb(200, 225, 255)));\n"
    "}\n",
    -1, NULL);

  listener_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(listener_provider),
    "#listener_text { \n"
    "  background-color: rgb(240, 248, 255);\n"			 
    "}\n",
    -1, NULL);
		
  dialog_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(dialog_provider),
    "GtkDialog { \n"
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(rgb(250, 250, 230)), to(rgb(235, 235, 210)));\n"
    "}\n",
    -1, NULL);
		
/* TODO: overall background color (gradient?), also how to reflect colors (in prefs as well)
*/
}
#endif
