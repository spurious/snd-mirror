#include "snd.h"


/* there are no references to gdk_draw_* or GdkGC or GdkGCValues outside this file except
 *   snd-gchn: gdk_draw_pixbuf (spectrum redisplay optimization)
 *   xg.c and gl.c -- this affects snd-gtk.scm which has gdk_gc_* stuff using snd-gcs
 *   also GdkColor is pretty well sequestered
 */


void draw_line(axis_context *ax, int x0, int y0, int x1, int y1) 
{
#if USE_CAIRO
  cairo_set_source_rgb(ax->cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);
  cairo_set_line_width(ax->cr, 1.0); 
  /* to get a thin line in cairo -- hooboy! you have to offset everything -- this is not pretty
   *    if line_width < 1.0, you get a smudgy mess in gray-scale!!  
   */
  cairo_move_to(ax->cr, x0 + 0.5, y0 + 0.5);
  cairo_line_to(ax->cr, x1 + 0.5, y1 + 0.5);
  cairo_stroke(ax->cr);
#else
  if (ax->wn == NULL) return;
  gdk_draw_line(ax->wn, ax->gc, (gint)x0, (gint)y0, (gint)x1, (gint)y1);
#endif
}


void draw_lines(axis_context *ax, point_t *points, int num)
{
  if (num == 0) return;
#if USE_CAIRO
  {
    int i;
    cairo_set_source_rgb(ax->cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);
    cairo_set_line_width(ax->cr, 1.0); 
    cairo_move_to(ax->cr, points[0].x + 0.5, points[0].y + 0.5);
    for (i = 1; i < num; i++)
      cairo_line_to(ax->cr, points[i].x + 0.5, points[i].y + 0.5);
    cairo_stroke(ax->cr);
  }
#else
  gdk_draw_lines(ax->wn, ax->gc, points, num);
#endif
}


void draw_dot(axis_context *ax, int x, int y, int size)
{
#if USE_CAIRO
  cairo_set_source_rgb(ax->cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);
  cairo_arc(ax->cr, x, y, size / 2, 0.0, 2 * M_PI);
  cairo_fill(ax->cr);
#else
  gdk_draw_arc(ax->wn, ax->gc, true, x - size / 2, y - size / 2, size, size, 0, 360 * 64);
#endif
}


#if 0
void draw_arc(axis_context *ax, int x, int y, int size, int angle0, int angle1)
{
#if USE_CAIRO
  cairo_set_source_rgb(ax->cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);
  cairo_arc(ax->cr, x, y, size / 2, mus_degrees_to_radians(angle0), mus_degrees_to_radians(angle1));
  cairo_stroke(ax->cr);
#else
  gdk_draw_arc(ax->wn, ax->gc, false, x - size / 2, y - size / 2, size, size, angle0 * 64, angle1 * 64);
#endif
}
#endif


#if 0
void draw_point(axis_context *ax, GdkPoint point, int size)
{
#if USE_CAIRO
  draw_dot(ax, point.x, point.y, size);
#else
  if (size == 1)
    gdk_draw_point(ax->wn, ax->gc, point.x, point.y);
  else draw_dot(ax, point.x, point.y, size);
#endif
}
#endif


void draw_points(axis_context *ax, point_t *points, int num, int size)
{
  if (num == 0) return;
#if (!USE_CAIRO)
  if (size == 1)
    gdk_draw_points(ax->wn, ax->gc, points, num);
  else
#endif
    {
      int i;
      for (i = 0; i < num; i++) 
	draw_dot(ax, points[i].x, points[i].y, size);
    }
}


void fill_rectangle(axis_context *ax, int x0, int y0, int width, int height)
{
#if USE_CAIRO
  cairo_set_source_rgb(ax->cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);
  cairo_rectangle(ax->cr, x0, y0, width, height);
  cairo_fill(ax->cr);
#else
  if (ax->wn == NULL) return;
  gdk_draw_rectangle(ax->wn, ax->gc, true, (gint)x0, (gint)y0, (gint)width, (gint)height);
#endif
}


void erase_rectangle(chan_info *cp, axis_context *ax, int x0, int y0, int width, int height)
{
  /* used only to clear the overall graph window in snd-chn.c */
#if USE_CAIRO
  if (ss->sgx->bg_gradient < .01)
    {
      cairo_set_source_rgb(ax->cr, ax->gc->bg_color->red, ax->gc->bg_color->green, ax->gc->bg_color->blue);
      cairo_rectangle(ax->cr, x0, y0, width, height);
      cairo_fill(ax->cr);
    }
  else
    {
      mus_float_t grad;
      grad = ss->sgx->bg_gradient;
      /* try gradient background: looks ok, but display is slow */
      cairo_pattern_t *pat;
      /* this is shaded toward the right
	 pat = cairo_pattern_create_linear(0, 0, width, height);
      */
      /* this is shaded toward the bottom 
       */
      pat = cairo_pattern_create_linear(0, 0, 0, height);
      cairo_pattern_add_color_stop_rgb(pat, 1, 
				       mus_fclamp(0.0, ax->gc->bg_color->red - grad, 1.0), 
				       mus_fclamp(0.0, ax->gc->bg_color->green - grad, 1.0), 
				       mus_fclamp(0.0, ax->gc->bg_color->blue - grad, 1.0));
      cairo_pattern_add_color_stop_rgb(pat, 0, 
				       mus_fclamp(0.0, ax->gc->bg_color->red + grad, 1.0), 
				       mus_fclamp(0.0, ax->gc->bg_color->green + grad, 1.0), 
				       mus_fclamp(0.0, ax->gc->bg_color->blue + grad, 1.0));
      cairo_rectangle(ax->cr, x0, y0, width, height);
      cairo_set_source(ax->cr, pat);
      cairo_fill(ax->cr);
      cairo_pattern_destroy(pat);
    }
#else
  if (ax->wn == NULL) return;
  gdk_draw_rectangle(ax->wn, erase_GC(cp), true, (gint)x0, (gint)y0, (gint)width, (gint)height);
#endif
}


void draw_string(axis_context *ax, int x0, int y0, const char *str, int len)
{
  if ((ax->wn == NULL) || (ax->current_font == NULL)) return;
  if ((!str) || (!(*str))) return;
  if (!(g_utf8_validate(str, -1, NULL)))
    return;

#if USE_CAIRO
  {
    PangoLayout *layout = NULL;
    layout = pango_cairo_create_layout(ax->cr);
    pango_layout_set_font_description(layout, ax->current_font);
    pango_layout_set_text(layout, str, -1);
    cairo_set_source_rgb(ax->cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);	
    cairo_move_to(ax->cr, x0, y0);
    pango_cairo_show_layout(ax->cr, layout);
    g_object_unref(G_OBJECT(layout));
  }
#endif
}


static void rotate_text(axis_context *ax, PangoFontDescription *font, const char *text, int angle, gint x0, gint y0)
{
#if USE_CAIRO
  cairo_t *cr;
  int width, height;
  PangoLayout *layout = NULL;
  cr = gdk_cairo_create(ax->wn);
  layout = pango_cairo_create_layout(cr);
  pango_layout_set_font_description(layout, font);
  pango_layout_set_text(layout, text, -1);
  pango_layout_get_size(layout, &width, &height);
  cairo_set_source_rgb(cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);
  cairo_move_to(cr, x0 + (double)height / (2 * PANGO_SCALE), y0 + (double)width / PANGO_SCALE);
  cairo_rotate(cr, mus_degrees_to_radians(-angle));
  pango_cairo_update_layout(cr, layout);
  pango_cairo_show_layout(cr, layout);
  cairo_destroy(cr);
  g_object_unref(layout);
#endif
}


void draw_rotated_axis_label(chan_info *cp, axis_context *ax, const char *text, gint x0, gint y0)
{
  rotate_text(ax, AXIS_LABEL_FONT(ss), text, 90, x0, y0);
}


void draw_picture(axis_context *ax, picture_t *src, gint xsrc, gint ysrc, gint xdest, gint ydest, gint width, gint height)
{
#if USE_CAIRO
  cairo_t *cr;
  if ((ax) && (GDK_IS_DRAWABLE(ax->wn)))
    {
      cr = gdk_cairo_create(ax->wn);
      gdk_cairo_set_source_pixmap(cr, src, xsrc + xdest, ysrc + ydest);
      cairo_paint(cr);
      cairo_destroy(cr);
    }
#else
  gdk_draw_drawable(ax->wn, ax->gc, GDK_DRAWABLE(src), xsrc, ysrc, xdest, ydest, width, height);
#endif
}


static void draw_polygon_va(axis_context *ax, bool filled, int points, va_list ap)
{
  int i;
#if USE_CAIRO
  {
    int x, y;
    x = va_arg(ap, int);
    y = va_arg(ap, int);
    cairo_set_source_rgb(ax->cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);
    cairo_set_line_width(ax->cr, 1.0);
    cairo_move_to(ax->cr, x, y);
    for (i = 1; i < points; i++)
      {
	x = va_arg(ap, int);
	y = va_arg(ap, int);
	cairo_line_to(ax->cr, x, y);
      }
    if (filled)
      {
	cairo_close_path(ax->cr);
	cairo_fill(ax->cr);
      }
    else
      {
	cairo_stroke(ax->cr);
      }
  }
#else
  {
    GdkPoint *pts;
    pts = (GdkPoint *)calloc(points, sizeof(GdkPoint));
    for (i = 0; i < points; i++)
      {
	pts[i].x = va_arg(ap, int);
	pts[i].y = va_arg(ap, int);
      }
    if (filled)
      gdk_draw_polygon(ax->wn, ax->gc, true, pts, points);
    else gdk_draw_lines(ax->wn, ax->gc, pts, points);
    free(pts);
  }
#endif
}


void fill_polygon(axis_context *ax, int points, ...)
{
  va_list ap;
  if (points == 0) return;
  va_start(ap, points);
  draw_polygon_va(ax, true, points, ap);
  va_end(ap);
}


void draw_polygon(axis_context *ax, int points, ...)
{
  va_list ap;
  if (points == 0) return;
  va_start(ap, points);
  draw_polygon_va(ax, false, points, ap);
  va_end(ap);
}


void fill_polygon_from_array(axis_context *ax, point_t *points, int npoints)
{
#if USE_CAIRO
  int i;
  cairo_set_source_rgb(ax->cr, ax->gc->fg_color->red, ax->gc->fg_color->green, ax->gc->fg_color->blue);
  cairo_set_line_width(ax->cr, 1.0);
  cairo_move_to(ax->cr, points[0].x, points[0].y);
  for (i = 1; i < npoints; i++)
    cairo_line_to(ax->cr, points[i].x, points[i].y);
  cairo_close_path(ax->cr);
  cairo_fill(ax->cr);
#else
  gdk_draw_polygon(ax->wn, ax->gc, true, points, npoints);
#endif
}


static GdkPoint polypts[4];

void fill_polygons(axis_context *ax, point_t *points, int num, int y0)
{
  int i;
  for (i = 1; i < num; i++)
    {
      polypts[0].x = points[i - 1].x;
      polypts[0].y = points[i - 1].y;
      polypts[1].x = points[i].x;
      polypts[1].y = points[i].y;
      polypts[2].x = polypts[1].x;
      polypts[2].y = y0;
      polypts[3].x = points[i - 1].x;
      polypts[3].y = y0;
      fill_polygon_from_array(ax, polypts, 4);
    }
}


void fill_two_sided_polygons(axis_context *ax, point_t *points, point_t *points1, int num)
{
  int i;
  for (i = 1; i < num; i++)
    {
      polypts[0].x = points[i - 1].x;
      polypts[0].y = points[i - 1].y;
      polypts[1].x = points[i].x;
      polypts[1].y = points[i].y;
      polypts[2].x = points1[i].x;
      polypts[2].y = points1[i].y;
      polypts[3].x = points1[i - 1].x;
      polypts[3].y = points1[i - 1].y;
      fill_polygon_from_array(ax, polypts, 4);
    }
}


void setup_axis_context(chan_info *cp, axis_context *ax)
{
  GtkWidget *w;
  snd_info *sp;
  sp = cp->sound;
  if (cp->tcgx) 
    w = channel_graph(sp->chans[0]);
  else w = channel_graph(cp);
  ax->gc = copy_GC(cp);
  ax->wn = WIDGET_TO_WINDOW(w);
  ax->w = w;
}


/* colormaps */


#if (!USE_CAIRO)

static int sono_bins = 0; /* total_bins */
static GdkColor **current_colors = NULL;
static int current_colors_size = 0;
static int current_colormap = BLACK_AND_WHITE_COLORMAP;
static GdkRectangle **sono_data = NULL;
static int sono_colors = 0; /* colormap_size */
static GdkGC *colormap_GC;

void check_colormap_sizes(int size)
{
  int i, old_size;
  if (current_colors_size > 0)
    {
      if ((current_colors) && (current_colors_size < size))
	{
	  old_size = current_colors_size;
	  current_colors_size = size;
	  if (current_colormap != BLACK_AND_WHITE_COLORMAP) 
	    {
	      for (i = 0; i < old_size; i++) 
		{
		  gdk_color_free(current_colors[i]);
		  current_colors[i] = NULL;
		}
	      current_colormap = BLACK_AND_WHITE_COLORMAP;
	    }
	  free(current_colors);
	  current_colors = (GdkColor **)calloc(current_colors_size, sizeof(GdkColor *));
	}
    }
  if ((sono_data) && (sono_colors < size) && (sono_bins > 0))
    {
      old_size = sono_colors;
      sono_colors = size;
      sono_data = (GdkRectangle **)realloc(sono_data, sono_colors * sizeof(GdkRectangle *));
      for (i = old_size; i < sono_colors; i++) sono_data[i] = (GdkRectangle *)calloc(sono_bins, sizeof(GdkRectangle));
    }
}


void initialize_colormap(void)
{
  state_context *sx;
  sx = ss->sgx;
  colormap_GC = gdk_gc_new(MAIN_WINDOW(ss));
  gc_set_background(sx->basic_gc, sx->graph_color);
  gc_set_foreground(sx->basic_gc, sx->data_color);
  sono_colors = color_map_size(ss);
  sono_data = (GdkRectangle **)calloc(sono_colors, sizeof(GdkRectangle *));
  current_colors_size = color_map_size(ss);
  current_colors = (GdkColor **)calloc(current_colors_size, sizeof(GdkColor *));
}


void draw_sono_rectangles(axis_context *ax, int color, int jmax)
{
  int i;
  if (current_colors[color])
    gdk_gc_set_foreground(colormap_GC, current_colors[color]);
  for (i = 0; i < jmax; i++)
    gdk_draw_rectangle(ax->wn, colormap_GC, true, 
		       sono_data[color][i].x, 
		       sono_data[color][i].y, 
		       sono_data[color][i].width, 
		       sono_data[color][i].height);
}


void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1)
{
  gdk_gc_set_foreground(colormap_GC, current_colors[color]);
  gdk_draw_line(ax->wn, colormap_GC, x0, y0, x1, y1);
}


void set_sono_rectangle(int j, int color, int x, int y, int width, int height)
{
  GdkRectangle *r;
  r = sono_data[color];
  r[j].x = x;
  r[j].y = y;
  r[j].width = width;
  r[j].height = height;
}


void allocate_sono_rects(int size)
{
  if (size != sono_bins)
    {
      int i;
      for (i = 0; i < sono_colors; i++)
	{
	  if ((sono_bins > 0) && (sono_data[i])) 
	    free(sono_data[i]); 
	  sono_data[i] = (GdkRectangle *)calloc(size, sizeof(GdkRectangle));
	}
      sono_bins = size;
    }
}


void allocate_color_map(int colormap)
{
  if (current_colormap != colormap)
    {
      int i;
      GdkColormap *cmap;
      GdkColor tmp_color;
      cmap = gdk_colormap_get_system();
      if (current_colormap != BLACK_AND_WHITE_COLORMAP) 
	for (i = 0; i < current_colors_size; i++) 
	  gdk_color_free(current_colors[i]);
      for (i = 0; i < current_colors_size; i++)
	{
	  get_current_color(colormap, i, &(tmp_color.red), &(tmp_color.green), &tmp_color.blue);
	  current_colors[i] = gdk_color_copy(&tmp_color);
	  gdk_rgb_find_color(cmap, current_colors[i]);
	}
      current_colormap = colormap;
    }
}


void draw_colored_lines(chan_info *cp, axis_context *ax, point_t *points, int num, int *colors, int axis_y0, color_t default_color)
{
  int i, x0, y0, x1, y1, cur, prev;
  color_t old_color;

  old_color = get_foreground_color(ax);

  x0 = points[0].x;
  y0 = points[0].y;

  if (abs(y0 - axis_y0) < 5)
    prev = -1;
  else prev = colors[0];

  gdk_gc_set_foreground(ax->gc, (prev == -1) ? default_color : current_colors[prev]);

  for (i = 1; i < num; i++)
    {
      x1 = points[i].x;
      y1 = points[i].y;
      if ((abs(y0 - axis_y0) < 5) &&
	  (abs(y1 - axis_y0) < 5))
	cur = -1;
      else 
	{
	  if (y0 > y1)
	    cur = colors[i];
	  else cur = colors[i - 1]; /* coords are upside down */
	}

      if (cur != prev)
	{
	  gdk_gc_set_foreground(ax->gc, (cur == -1) ? default_color : current_colors[cur]);
	  prev = cur;
	}

      if (cp->transform_graph_style == GRAPH_DOTS)
	gdk_draw_arc(ax->wn, ax->gc, true, x0 - cp->dot_size / 2, y0 - cp->dot_size / 2, cp->dot_size, cp->dot_size, 0, 360 * 64);
      else gdk_draw_line(ax->wn, ax->gc, (gint)x0, (gint)y0, (gint)x1, (gint)y1);

      x0 = x1;
      y0 = y1;
    }

  set_foreground_color(ax, old_color);
}

#else

/* cairo colormaps */

static int sono_bins = 0; /* total_bins */
static int sono_colors = 0; /* colormap_size */
static GdkRectangle **sono_data = NULL;

void check_colormap_sizes(int size)
{
  int i, old_size;
  if ((sono_data) && (sono_colors < size) && (sono_bins > 0))
    {
      old_size = sono_colors;
      sono_colors = size;
      sono_data = (GdkRectangle **)realloc(sono_data, sono_colors * sizeof(GdkRectangle *));
      for (i = old_size; i < sono_colors; i++) sono_data[i] = (GdkRectangle *)calloc(sono_bins, sizeof(GdkRectangle));
    }
}


void initialize_colormap(void)
{
  sono_colors = color_map_size(ss);
  sono_data = (GdkRectangle **)calloc(sono_colors, sizeof(GdkRectangle *));
}


void draw_sono_rectangles(axis_context *ax, int color, int jmax)
{
  int i;
  rgb_t r, g, b;
  get_current_color(color_map(ss), color, &r, &g, &b);
  cairo_save(ax->cr);
  cairo_set_source_rgb(ax->cr, r, g, b);
  for (i = 0; i < jmax; i++)
    {
      cairo_rectangle(ax->cr, 
		      sono_data[color][i].x, 
		      sono_data[color][i].y, 
		      sono_data[color][i].width, 
		      sono_data[color][i].height);
      cairo_fill(ax->cr);
    }
  cairo_restore(ax->cr);
}


void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1)
{
  rgb_t r, g,b;
  get_current_color(color_map(ss), color, &r, &g, &b);
  cairo_set_source_rgb(ax->cr, r, g, b);
  cairo_set_line_width(ax->cr, 1.0); 
  cairo_move_to(ax->cr, x0 + 0.5, y0 + 0.5);
  cairo_line_to(ax->cr, x1 + 0.5, y1 + 0.5);
  cairo_stroke(ax->cr);
}


void set_sono_rectangle(int j, int color, int x, int y, int width, int height)
{
  GdkRectangle *r;
  r = sono_data[color];
  r[j].x = x;
  r[j].y = y;
  r[j].width = width;
  r[j].height = height;
}


void allocate_sono_rects(int size)
{
  if (size != sono_bins)
    {
      int i;
      for (i = 0; i < sono_colors; i++)
	{
	  if ((sono_bins > 0) && (sono_data[i])) 
	    free(sono_data[i]); 
	  sono_data[i] = (GdkRectangle *)calloc(size, sizeof(GdkRectangle));
	}
      sono_bins = size;
    }
}


void allocate_color_map(int colormap)
{
}


void phases_rgb(float x, rgb_t *r, rgb_t *g, rgb_t *b);

void draw_colored_lines(chan_info *cp, axis_context *ax, point_t *points, int num, int *colors, int axis_y0, color_t default_color)
{
  int i, x0, y0, x1, y1, cur, prev;
  color_t old_color;
  rgb_t r, g, b;

  old_color = get_foreground_color(ax);
  cairo_save(ax->cr);

  x0 = points[0].x;
  y0 = points[0].y;

  if (abs(y0 - axis_y0) < 5)
    prev = -1;
  else prev = colors[0];

  if (prev == -1) 
    {
      r = default_color->red; 
      g = default_color->green; 
      b = default_color->blue; 
    }
  else phases_rgb((mus_float_t)(colors[prev]) / (mus_float_t)color_map_size(ss), &r, &g, &b);
  cairo_set_source_rgb(ax->cr, r, g, b);

  for (i = 1; i < num; i++)
    {
      x1 = points[i].x;
      y1 = points[i].y;
      if ((abs(y0 - axis_y0) < 5) &&
	  (abs(y1 - axis_y0) < 5))
	cur = -1;
      else 
	{
	  if (y0 > y1)
	    cur = colors[i];
	  else cur = colors[i - 1]; /* coords are upside down */
	}

      if (cur != prev)
	{
	  if (cur == -1) 
	    {
	      r = default_color->red; 
	      g = default_color->green; 
	      b = default_color->blue; 
	    }
	  else phases_rgb((mus_float_t)(colors[cur]) / (mus_float_t)color_map_size(ss), &r, &g, &b);
	  cairo_set_source_rgb(ax->cr, r, g, b);
	  prev = cur;
	}

      if (cp->transform_graph_style == GRAPH_DOTS)
	{
	  cairo_arc(ax->cr, x0, y0, cp->dot_size / 2, 0.0, 2 * M_PI);
	  cairo_fill(ax->cr);
	}
      else 
	{
	  cairo_set_line_width(ax->cr, 1.0); 
	  cairo_move_to(ax->cr, x0 + 0.5, y0 + 0.5);
	  cairo_line_to(ax->cr, x1 + 0.5, y1 + 0.5);
	  cairo_stroke(ax->cr);
	}

      x0 = x1;
      y0 = y1;
    }

  cairo_set_source_rgb(ax->cr, old_color->red, old_color->green, old_color->blue);

  cairo_restore(ax->cr);
}

#endif



/* -------- color browser -------- */

static XEN color_hook;
static GtkWidget *ccd_dialog = NULL, *ccd_scale, *ccd_invert, *ccd_cutoff;
static GtkObject *ccd_scale_adj, *ccd_cutoff_adj;
static slist *ccd_list;


static void check_color_hook(void)
{
  if (XEN_HOOKED(color_hook))
    run_hook(color_hook, XEN_EMPTY_LIST, S_color_hook);
}


static void update_graph_setting_fft_changed(chan_info *cp)
{
  cp->fft_changed = FFT_CHANGE_LOCKED;
  update_graph(cp);
}


static void invert_color_callback(GtkWidget *w, gpointer context)
{
  in_set_color_inverted(TOGGLE_BUTTON_ACTIVE(w));
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


void set_color_inverted(bool val)
{
  in_set_color_inverted(val);
  if (ccd_dialog) set_toggle_button(ccd_invert, false, false, NULL);
  check_color_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph_setting_fft_changed);
}


static void scale_color_callback(GtkAdjustment *adj, gpointer context)
{
  gfloat scale_val, val;
  scale_val = ADJUSTMENT_VALUE(adj);
  if (scale_val <= 50) 
    val = (mus_float_t)(scale_val + 1) / 51.0;
  else val = 1.0 + (mus_float_t)(scale_val - 50) * 20.0;
  in_set_color_scale(val);
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


static void reflect_color_scale(mus_float_t val)
{
  gfloat new_val;
  if (val < 0.02)
    new_val = 0.0;
  else
    {
      if (val <= 1.0) 
	new_val = (val * 51.0 - 1);
      else new_val = (val - 1.0) / 20.0 + 50.0;
    }
  if (ccd_dialog) ADJUSTMENT_SET_VALUE(ccd_scale_adj, new_val);
}


void set_color_scale(mus_float_t val)
{
  in_set_color_scale(val);
  if (ccd_dialog) reflect_color_scale(color_scale(ss));
  check_color_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph_setting_fft_changed);
}


static void list_color_callback(const char *name, int row, void *data)
{
  in_set_color_map(row);
  for_each_chan(update_graph_setting_fft_changed);
  check_color_hook();
}


void set_color_map(int val)
{
  in_set_color_map(val);
  if ((ccd_dialog) && (val >= 0)) slist_select(ccd_list, val);
  check_color_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph_setting_fft_changed);
}


static void cutoff_color_callback(GtkAdjustment *adj, gpointer context)
{
  in_set_color_cutoff(ADJUSTMENT_VALUE(adj));
  check_color_hook();
  for_each_chan(update_graph_setting_fft_changed);
}


void set_color_cutoff(mus_float_t val)
{
  in_set_color_cutoff(val);
  if (ccd_dialog) ADJUSTMENT_SET_VALUE(ccd_cutoff_adj, val);
  check_color_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph_setting_fft_changed);
}


static void dismiss_color_orientation_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(ccd_dialog);
}


static void help_color_orientation_callback(GtkWidget *w, gpointer context)
{
  color_orientation_dialog_help();
}


static gint delete_color_orientation_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(ccd_dialog);
  return(true);
}


void reflect_color_list(bool setup_time)
{
  if ((ccd_dialog) && (ccd_list))
    {
      int i, size;
      size = num_colormaps();
      slist_clear(ccd_list);
      for (i = 0; i < size; i++) 
	slist_append(ccd_list, colormap_name(i));
    }
}


/* -------- orientation browser -------- */

static XEN orientation_hook;

static void check_orientation_hook(void)
{
  run_hook(orientation_hook, XEN_EMPTY_LIST, S_orientation_hook);
}


static GtkWidget *oid_dialog = NULL, *oid_ax, *oid_ay, *oid_az, *oid_sx, *oid_sy, *oid_sz, *oid_hop; 
static GtkObject *oid_ax_adj, *oid_az_adj, *oid_ay_adj, *oid_sx_adj, *oid_sz_adj, *oid_sy_adj, *oid_hop_adj;

static void ax_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  in_set_spectro_x_angle((mus_float_t)(ADJUSTMENT_VALUE(adj)));
  chans_field(FCP_X_ANGLE, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_x_angle(mus_float_t val)
{
  in_set_spectro_x_angle(val);
  if (oid_dialog) ADJUSTMENT_SET_VALUE(oid_ax_adj, val);
  chans_field(FCP_X_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph);
}


static void ay_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  in_set_spectro_y_angle((mus_float_t)(ADJUSTMENT_VALUE(adj)));
  chans_field(FCP_Y_ANGLE, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_y_angle(mus_float_t val)
{
  in_set_spectro_y_angle(val);
  if (oid_dialog) ADJUSTMENT_SET_VALUE(oid_ay_adj, val);
  chans_field(FCP_Y_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph);
}


static void az_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  in_set_spectro_z_angle((mus_float_t)(ADJUSTMENT_VALUE(adj)));
  chans_field(FCP_Z_ANGLE, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_z_angle(mus_float_t val)
{
  in_set_spectro_z_angle(val);
  if (oid_dialog) ADJUSTMENT_SET_VALUE(oid_az_adj, val);
  chans_field(FCP_Z_ANGLE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph);
}


static void sx_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  in_set_spectro_x_scale((mus_float_t)(ADJUSTMENT_VALUE(adj)));
  chans_field(FCP_X_SCALE, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_x_scale(mus_float_t val)
{
  in_set_spectro_x_scale(val);
  if (oid_dialog) ADJUSTMENT_SET_VALUE(oid_sx_adj, val);
  chans_field(FCP_X_SCALE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph);
}


static void sy_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  in_set_spectro_y_scale((mus_float_t)(ADJUSTMENT_VALUE(adj)));
  chans_field(FCP_Y_SCALE, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_y_scale(mus_float_t val)
{
  in_set_spectro_y_scale(val);
  if (oid_dialog) ADJUSTMENT_SET_VALUE(oid_sy_adj, val);
  chans_field(FCP_Y_SCALE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph);
}


static void sz_orientation_callback(GtkAdjustment *adj, gpointer context) 
{
  in_set_spectro_z_scale((mus_float_t)(ADJUSTMENT_VALUE(adj)));
  chans_field(FCP_Z_SCALE, (mus_float_t)(ADJUSTMENT_VALUE(adj)));
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_z_scale(mus_float_t val)
{
  in_set_spectro_z_scale(val);
  if (oid_dialog) ADJUSTMENT_SET_VALUE(oid_sz_adj, val);
  chans_field(FCP_Z_SCALE, val);
  check_orientation_hook();
  if (!(ss->graph_hook_active)) for_each_chan(update_graph);
}


static void chans_spectro_hop(chan_info *cp, int value)
{
  cp->spectro_hop = value;
}


static void hop_callback(GtkAdjustment *adj, gpointer context) 
{
  int val;
  val = mus_iclamp(1, (int)(ADJUSTMENT_VALUE(adj)), 20);
  in_set_spectro_hop(val);
  for_each_chan_with_int(chans_spectro_hop, val);
  check_orientation_hook();
  for_each_chan(update_graph);
}


void set_spectro_hop(int val)
{
  if (val > 0)
    {
      in_set_spectro_hop(val);
      if (oid_dialog) ADJUSTMENT_SET_VALUE(oid_hop_adj, val);
      for_each_chan_with_int(chans_spectro_hop, val);
      check_orientation_hook();
      if (!(ss->graph_hook_active)) for_each_chan(update_graph);
    }
}


static int fixup_angle(mus_float_t ang)
{
  int na;
  na = (int)ang;
  na = na % 360;
  if (na < 0) na += 360;
  return(na);
}


void reflect_spectro(void)
{
  /* set color/orientaton widget values */
  if (ccd_dialog)
    {
      set_toggle_button(ccd_invert, color_inverted(ss), false, NULL);
      ADJUSTMENT_SET_VALUE(ccd_cutoff_adj, color_cutoff(ss));
      reflect_color_scale(color_scale(ss));
    }
  if (oid_dialog) 
    {
      ADJUSTMENT_SET_VALUE(oid_ax_adj, fixup_angle(spectro_x_angle(ss)));
      ADJUSTMENT_SET_VALUE(oid_ay_adj, fixup_angle(spectro_y_angle(ss)));
      ADJUSTMENT_SET_VALUE(oid_az_adj, fixup_angle(spectro_z_angle(ss)));
      ADJUSTMENT_SET_VALUE(oid_sx_adj, spectro_x_scale(ss));
      ADJUSTMENT_SET_VALUE(oid_sy_adj, spectro_y_scale(ss));
      ADJUSTMENT_SET_VALUE(oid_sz_adj, spectro_z_scale(ss));
      ADJUSTMENT_SET_VALUE(oid_hop_adj, (spectro_hop(ss) > 100) ? 100 : (spectro_hop(ss)));
      check_orientation_hook();
    }
}


static void reset_color_orientation_callback(GtkWidget *w, gpointer context)
{
  /* put everything back the way it was at the start */
  reset_spectro();
  reflect_spectro();
  for_each_chan(update_graph);
}


void set_with_gl(bool val, bool with_dialogs)
{
  in_set_with_gl(val);
}


void view_color_orientation_callback(GtkWidget *w, gpointer context)
{
  start_color_orientation_dialog(true);
}


bool color_orientation_dialog_is_active(void)
{
  return((ccd_dialog) && (widget_is_active(ccd_dialog)));
}


GtkWidget *start_color_orientation_dialog(bool managed)
{
  if (!ccd_dialog)
    {
      GtkWidget *light_label, *dark_label, *help_button, *dismiss_button, *reset_button;
      GtkWidget *outer_table, *scale_box, *cutoff_box, *cutoff_label;
      GtkWidget *color_frame, *color_label, *mainbox, *colorbox;
      GtkWidget *shbox;

      GtkWidget *ax_box, *ay_box, *az_box, *sx_box, *sy_box, *sz_box, *hop_box;
      GtkWidget *ax_label, *ay_label, *az_label, *sx_label, *sy_label, *sz_label, *hop_label;
      GtkWidget *orient_frame, *orient_label, *orientbox;
      GtkWidget *sep1, *sep2, *sep3, *sep5;


      ccd_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(ccd_dialog, "delete_event", delete_color_orientation_dialog, NULL);
      gtk_window_set_title(GTK_WINDOW(ccd_dialog), _("Color"));
      sg_make_resizable(ccd_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(ccd_dialog), 4);
      gtk_widget_realize(ccd_dialog);
      gtk_window_resize(GTK_WINDOW(ccd_dialog), 400, 200);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      dismiss_button = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismiss_button, "quit_button");
      set_stock_button_label(dismiss_button, _("Go Away"));

      reset_button = gtk_button_new_from_stock(GTK_STOCK_REVERT_TO_SAVED);
      gtk_widget_set_name(reset_button, "reset_button");

      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(ccd_dialog)), dismiss_button, false, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(ccd_dialog)), reset_button, false, true, 10);
      gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(ccd_dialog)), help_button, false, true, 10);

      SG_SIGNAL_CONNECT(dismiss_button, "clicked", dismiss_color_orientation_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", help_color_orientation_callback, NULL);
      SG_SIGNAL_CONNECT(reset_button, "clicked", reset_color_orientation_callback, NULL);

      gtk_widget_show(dismiss_button);
      gtk_widget_show(reset_button);
      gtk_widget_show(help_button);

      mainbox = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(DIALOG_CONTENT_AREA(ccd_dialog)), mainbox);
      gtk_widget_show(mainbox);
      

      color_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(mainbox), color_frame, false, false, 10);
      gtk_frame_set_shadow_type(GTK_FRAME(color_frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(color_frame);

      colorbox = gtk_vbox_new(false, 2);
      gtk_container_add(GTK_CONTAINER(color_frame), colorbox);
      gtk_widget_show(colorbox);

      color_label = snd_gtk_highlight_label_new("colors");
      gtk_box_pack_start(GTK_BOX(colorbox), color_label, false, false, 0);
      gtk_widget_show(color_label);

      outer_table = gtk_table_new(3, 5, true); /* this seems backwards! */
      gtk_box_pack_end(GTK_BOX(colorbox), outer_table, false, false, 4);
      gtk_widget_show(outer_table);

      scale_box = gtk_vbox_new(false, 0);
      gtk_table_attach(GTK_TABLE(outer_table), scale_box, 0, 3, 0, 1,
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       10, 0);
      gtk_widget_show(scale_box);
      
      ccd_scale_adj = gtk_adjustment_new(50.0, 0.0, 101.0, 0.1, 1.0, 1.0);
      ccd_scale = gtk_hscale_new(GTK_ADJUSTMENT(ccd_scale_adj));
      UNSET_CAN_FOCUS(ccd_scale);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(ccd_scale)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(ccd_scale), 0);
      gtk_scale_set_value_pos(GTK_SCALE(ccd_scale), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(ccd_scale), true);
      gtk_box_pack_start(GTK_BOX(scale_box), ccd_scale, true, true, 0);
      SG_SIGNAL_CONNECT(ccd_scale_adj, "value_changed", scale_color_callback, NULL);
      gtk_widget_show(ccd_scale);

      shbox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(scale_box), shbox, true, true, 0); 
      gtk_widget_show(shbox);

      light_label = gtk_label_new(_("light"));
      gtk_misc_set_alignment(GTK_MISC (light_label), 0.05, 0.0);
      gtk_box_pack_start(GTK_BOX(shbox), light_label, false, false, 0);
      gtk_widget_show(light_label);

      dark_label = gtk_label_new(_("dark"));
      gtk_misc_set_alignment(GTK_MISC(dark_label), 0.95, 0.0);
      gtk_box_pack_end(GTK_BOX(shbox), dark_label, false, false, 0);
      gtk_widget_show(dark_label);

      {
	char **names;
	GtkWidget *frame;
	int i, size;

	frame = gtk_frame_new(NULL);
	gtk_container_set_border_width(GTK_CONTAINER(frame), 0);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	widget_modify_bg(frame, GTK_STATE_NORMAL, ss->sgx->zoom_color);
	gtk_table_attach(GTK_TABLE(outer_table), frame, 3, 5, 0, 3,
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       4, 4);
	gtk_widget_show(frame);

	size = num_colormaps();
	names = (char **)calloc(size, sizeof(char *));
	for (i = 0; i < size; i++) names[i] = colormap_name(i);
	ccd_list = slist_new_with_title(S_colormap, frame, (const char**)names, size, CONTAINER_ADD);
	ccd_list->select_callback = list_color_callback;
	free(names);
      }

      cutoff_box = gtk_hbox_new(false, 0);
	gtk_table_attach(GTK_TABLE(outer_table), cutoff_box, 0, 3, 1, 2,
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      gtk_widget_show(cutoff_box);

      cutoff_label = gtk_label_new(_("data cutoff:"));
      gtk_box_pack_start(GTK_BOX(cutoff_box), cutoff_label, false, false, 4);
      gtk_widget_show(cutoff_label);      

      ccd_cutoff_adj = gtk_adjustment_new(color_cutoff(ss), 0.0, 1.01, 0.001, 0.01, .01);
      ccd_cutoff = gtk_hscale_new(GTK_ADJUSTMENT(ccd_cutoff_adj));
      UNSET_CAN_FOCUS(ccd_cutoff);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(ccd_cutoff)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(ccd_cutoff), 3);
      gtk_scale_set_value_pos(GTK_SCALE(ccd_cutoff), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(ccd_cutoff), true);
      SG_SIGNAL_CONNECT(ccd_cutoff_adj, "value_changed", cutoff_color_callback, NULL);
      gtk_box_pack_start(GTK_BOX(cutoff_box), ccd_cutoff, true, true, 0);
      gtk_widget_show(ccd_cutoff);

      ccd_invert = gtk_check_button_new_with_label(_("invert"));
      SG_SIGNAL_CONNECT(ccd_invert, "toggled", invert_color_callback, NULL);

	gtk_table_attach(GTK_TABLE(outer_table), ccd_invert, 0, 3, 2, 3,
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);

      gtk_widget_show(ccd_invert);
      set_toggle_button(ccd_invert, color_inverted(ss), false, NULL);

      set_dialog_widget(COLOR_ORIENTATION_DIALOG, ccd_dialog);
      if (color_map(ss) != BLACK_AND_WHITE_COLORMAP) slist_select(ccd_list, color_map(ss));


      /* orientation section */

      orient_frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(mainbox), orient_frame, false, false, 10);
      gtk_frame_set_shadow_type(GTK_FRAME(orient_frame), GTK_SHADOW_ETCHED_IN);
      gtk_widget_show(orient_frame);

      orientbox = gtk_vbox_new(false, 2);
      gtk_container_add(GTK_CONTAINER(orient_frame), orientbox);
      gtk_widget_show(orientbox);

      orient_label = snd_gtk_highlight_label_new("orientation");
      gtk_box_pack_start(GTK_BOX(orientbox), orient_label, false, false, 0);
      gtk_widget_show(orient_label);


      sep3 = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(orientbox), sep3, false, false, 3);
      widget_modify_bg(sep3, GTK_STATE_NORMAL, ss->sgx->basic_color);
      gtk_widget_show(sep3);


      /* AX */
      ax_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(orientbox), ax_box, true, true, 0);
      gtk_widget_show(ax_box);

      ax_label = gtk_label_new(_("x angle:"));
      gtk_box_pack_start(GTK_BOX(ax_box), ax_label, false, false, 4);
      gtk_widget_show(ax_label);

      oid_ax_adj = gtk_adjustment_new(spectro_x_angle(ss), 0.0, 361.0, 1.0, 10.0, 1.0);
      oid_ax = gtk_hscale_new(GTK_ADJUSTMENT(oid_ax_adj));
      UNSET_CAN_FOCUS(oid_ax);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid_ax)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid_ax), 0);
      gtk_scale_set_value_pos(GTK_SCALE(oid_ax), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(oid_ax), true);
      SG_SIGNAL_CONNECT(oid_ax_adj, "value_changed", ax_orientation_callback, NULL);
      gtk_box_pack_start(GTK_BOX(ax_box), oid_ax, true, true, 0);
      gtk_widget_show(oid_ax);


      /* AY */
      ay_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(orientbox), ay_box, true, true, 0);
      gtk_widget_show(ay_box);

      ay_label = gtk_label_new(_("y angle:"));
      gtk_box_pack_start(GTK_BOX(ay_box), ay_label, false, false, 4);
      gtk_widget_show(ay_label);

      oid_ay_adj = gtk_adjustment_new(spectro_y_angle(ss), 0.0, 361.0, 1.0, 10.0, 1.0);
      oid_ay = gtk_hscale_new(GTK_ADJUSTMENT(oid_ay_adj));
      UNSET_CAN_FOCUS(oid_ay);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid_ay)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid_ay), 0);
      gtk_scale_set_value_pos(GTK_SCALE(oid_ay), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(oid_ay), true);
      SG_SIGNAL_CONNECT(oid_ay_adj, "value_changed", ay_orientation_callback, NULL);
      gtk_box_pack_start(GTK_BOX(ay_box), oid_ay, true, true, 0);
      gtk_widget_show(oid_ay);


      /* AZ */
      az_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(orientbox), az_box, true, true, 0);
      gtk_widget_show(az_box);

      az_label = gtk_label_new(_("z angle:"));
      gtk_box_pack_start(GTK_BOX(az_box), az_label, false, false, 4);
      gtk_widget_show(az_label);

      oid_az_adj = gtk_adjustment_new(spectro_z_angle(ss), 0.0, 361.0, 1.0, 10.0, 1.0);
      oid_az = gtk_hscale_new(GTK_ADJUSTMENT(oid_az_adj));
      UNSET_CAN_FOCUS(oid_az);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid_az)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid_az), 0);
      gtk_scale_set_value_pos(GTK_SCALE(oid_az), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(oid_az), true);
      SG_SIGNAL_CONNECT(oid_az_adj, "value_changed", az_orientation_callback, NULL);
      gtk_box_pack_start(GTK_BOX(az_box), oid_az, true, true, 0);
      gtk_widget_show(oid_az);


      sep1 = gtk_vseparator_new(); /* not hseparator! */
      gtk_box_pack_start(GTK_BOX(orientbox), sep1, false, false, 6);
      widget_modify_bg(sep1, GTK_STATE_NORMAL, ss->sgx->basic_color);
      gtk_widget_show(sep1);


      /* SX */
      sx_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(orientbox), sx_box, true, true, 0);
      gtk_widget_show(sx_box);

      sx_label = gtk_label_new(_("x scale:"));
      gtk_box_pack_start(GTK_BOX(sx_box), sx_label, false, false, 4);
      gtk_widget_show(sx_label);

      oid_sx_adj = gtk_adjustment_new(spectro_x_scale(ss), 0.0, 2.01, .01, .1, .01);
      oid_sx = gtk_hscale_new(GTK_ADJUSTMENT(oid_sx_adj));
      UNSET_CAN_FOCUS(oid_sx);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid_sx)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid_sx), 2);
      gtk_scale_set_value_pos(GTK_SCALE(oid_sx), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(oid_sx), true);
      SG_SIGNAL_CONNECT(oid_sx_adj, "value_changed", sx_orientation_callback, NULL);
      gtk_box_pack_start(GTK_BOX(sx_box), oid_sx, true, true, 0);
      gtk_widget_show(oid_sx);


      /* SY */
      sy_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(orientbox), sy_box, true, true, 0);
      gtk_widget_show(sy_box);

      sy_label = gtk_label_new(_("y scale:"));
      gtk_box_pack_start(GTK_BOX(sy_box), sy_label, false, false, 4);
      gtk_widget_show(sy_label);

      oid_sy_adj = gtk_adjustment_new(spectro_y_scale(ss), 0.0, 2.01, .01, .1, .01);
      oid_sy = gtk_hscale_new(GTK_ADJUSTMENT(oid_sy_adj));
      UNSET_CAN_FOCUS(oid_sy);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid_sy)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid_sy), 2);
      gtk_scale_set_value_pos(GTK_SCALE(oid_sy), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(oid_sy), true);
      SG_SIGNAL_CONNECT(oid_sy_adj, "value_changed", sy_orientation_callback, NULL);
      gtk_box_pack_start(GTK_BOX(sy_box), oid_sy, true, true, 0);
      gtk_widget_show(oid_sy);


      /* SZ */
      sz_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(orientbox), sz_box, true, true, 0);
      gtk_widget_show(sz_box);


      sz_label = gtk_label_new(_("z scale:"));
      gtk_box_pack_start(GTK_BOX(sz_box), sz_label, false, false, 4);
      gtk_widget_show(sz_label);

      oid_sz_adj = gtk_adjustment_new(spectro_z_scale(ss), 0.0, 2.01, .01, .1, .01);
      oid_sz = gtk_hscale_new(GTK_ADJUSTMENT(oid_sz_adj));
      UNSET_CAN_FOCUS(oid_sz);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid_sz)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid_sz), 2);
      gtk_scale_set_value_pos(GTK_SCALE(oid_sz), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(oid_sz), true);
      SG_SIGNAL_CONNECT(oid_sz_adj, "value_changed", sz_orientation_callback, NULL);
      gtk_box_pack_start(GTK_BOX(sz_box), oid_sz, true, true, 0);
      gtk_widget_show(oid_sz);


      sep2 = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(orientbox), sep2, false, false, 6);
      widget_modify_bg(sep2, GTK_STATE_NORMAL, ss->sgx->basic_color);
      gtk_widget_show(sep2);


      /* HOP */
      hop_box = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(orientbox), hop_box, false, false, 0);
      gtk_widget_show(hop_box);

      hop_label = gtk_label_new(_("hop:     "));
      gtk_box_pack_start(GTK_BOX(hop_box), hop_label, false, false, 4);
      gtk_widget_show(hop_label);

      oid_hop_adj = gtk_adjustment_new((spectro_hop(ss) > 20) ? 20 : (spectro_hop(ss)), 0.0, 21.0, 0.1, 1.0, 1.0);
      oid_hop = gtk_hscale_new(GTK_ADJUSTMENT(oid_hop_adj));
      UNSET_CAN_FOCUS(oid_hop);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(oid_hop)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(oid_hop), 0);
      gtk_scale_set_value_pos(GTK_SCALE(oid_hop), GTK_POS_LEFT);
      gtk_scale_set_draw_value(GTK_SCALE(oid_hop), true);
      SG_SIGNAL_CONNECT(oid_hop_adj, "value_changed", hop_callback, NULL);
      gtk_box_pack_start(GTK_BOX(hop_box), oid_hop, true, true, 0);
      gtk_widget_show(oid_hop);

      sep5 = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(orientbox), sep5, false, false, 3);
      widget_modify_bg(sep5, GTK_STATE_NORMAL, ss->sgx->basic_color);
      gtk_widget_show(sep5);
    }
  else raise_dialog(ccd_dialog);
  if (managed) gtk_widget_show(ccd_dialog);

  return(ccd_dialog);
}


static XEN g_background_gradient(void) {return(C_TO_XEN_DOUBLE(ss->sgx->bg_gradient));}

static XEN g_set_background_gradient(XEN val) 
{
  #define H_background_gradient "(" S_background_gradient "): channel graph background color gradient"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_background_gradient, "a number between 0 (none) and 1");

  ss->sgx->bg_gradient = XEN_TO_C_DOUBLE(val);
  for_each_chan(update_graph);

  return(val);
}


#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_0(g_background_gradient_w, g_background_gradient)
  XEN_NARGIFY_1(g_set_background_gradient_w, g_set_background_gradient)
#else
  #define g_background_gradient_w g_background_gradient
  #define g_set_background_gradient_w g_set_background_gradient
#endif

void g_init_gxdraw(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_background_gradient, g_background_gradient_w, H_background_gradient,
				   S_setB S_background_gradient, g_set_background_gradient_w,  0, 0, 1, 0);

  #define H_orientation_hook S_orientation_hook " (): called whenever one of the variables associated with the \
orientation dialog changes"
  #define H_color_hook S_color_hook " (): called whenever one of the variables associated with the \
color dialog changes"

  orientation_hook = XEN_DEFINE_HOOK(S_orientation_hook, 0, H_orientation_hook);
  color_hook = XEN_DEFINE_HOOK(S_color_hook, 0, H_color_hook);
}
