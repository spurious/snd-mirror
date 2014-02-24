#include "snd.h"


#if (!USE_NO_GUI)

/* our "current path" */
static point_t points[POINT_BUFFER_SIZE];
static point_t points1[POINT_BUFFER_SIZE];

void set_grf_points(int xi, int j, int ymin, int ymax)
{
  points[j].x = xi;
  points1[j].x = xi;
  points[j].y = ymax;
  points1[j].y = ymin;
}


void set_grf_point(int xi, int j, int yi)
{
  points[j].x = xi;
  points[j].y = yi;
}


point_t *get_grf_points(void) {return(points);} 

point_t *get_grf_points1(void) {return(points1);}


void draw_both_grf_points(int dot_size, graphics_context *ax, int j, graph_style_t graph_style)
{
  int i;
  switch (graph_style)
    {
    case GRAPH_LINES:
    default:
      draw_lines(ax, points, j);
      draw_lines(ax, points1, j);
      break;

    case GRAPH_DOTS:
      draw_points(ax, points, j, dot_size);
      draw_points(ax, points1, j, dot_size);
      break;

    case GRAPH_FILLED:
      fill_two_sided_polygons(ax, points, points1, j);
      break;

    case GRAPH_DOTS_AND_LINES:
      if (dot_size > 1)
	{
	  draw_points(ax, points, j, dot_size);
	  draw_points(ax, points1, j, dot_size);
	}
      draw_lines(ax, points, j);
      draw_lines(ax, points1, j);
      break;

    case GRAPH_LOLLIPOPS:
      if (dot_size == 1)
	{
	  for (i = 0; i < j; i++)
	    draw_line(ax, points[i].x, points[i].y, points1[i].x, points1[i].y);
	}
      else
	{
	  int size8, size4;
	  size8 = dot_size / 8;
	  size4 = dot_size / 4;
	  if (size4 < 1) size4 = 1;
	  draw_points(ax, points, j, dot_size);
	  draw_points(ax, points1, j, dot_size);
	  for (i = 0; i < j; i++)
	    fill_rectangle(ax, points[i].x - size8, points[i].y, size4, points1[i].y - points[i].y);
	}
      break;
    }
}


void draw_grf_points(int dot_size, graphics_context *ax, int j, axis_info *ap, mus_float_t y0, graph_style_t graph_style)
{
  int i, gy0;
  switch (graph_style)
    {
    case GRAPH_LINES:
    default:
      draw_lines(ax, points, j); 
      break;

    case GRAPH_DOTS: 
      draw_points(ax, points, j, dot_size); 
      break;

    case GRAPH_FILLED: 
      fill_polygons(ax, points, j, grf_y(y0, ap)); 
      break;

    case GRAPH_DOTS_AND_LINES: 
      if (dot_size > 1) 
	draw_points(ax, points, j, dot_size); 
      draw_lines(ax, points, j); 
      break;

    case GRAPH_LOLLIPOPS:
      gy0 = grf_y(y0, ap);
      if (dot_size == 1)
	for (i = 0; i < j; i++)
	  draw_line(ax, points[i].x, points[i].y, points[i].x, gy0);
      else
	{
	  int size8, size4;
	  size8 = dot_size / 8;
	  size4 = dot_size / 4;
	  if (size4 < 1) size4 = 1;
	  draw_points(ax, points, j, dot_size);
	  for (i = 0; i < j; i++)
	    if (points[i].y > gy0)
	      fill_rectangle(ax, points[i].x - size8, gy0, size4, points[i].y - gy0);
	    else fill_rectangle(ax, points[i].x - size8, points[i].y, size4, gy0 - points[i].y);
	}
      break;
    }
}


void draw_cursor(chan_info *cp)
{
  cursor_style_t cur;
#if USE_GTK
  color_t old_color;
#endif
  axis_info *ap;
  graphics_context *ax;

  if (!(cp->graph_time_p)) return;
  ap = cp->axis;

#if USE_GTK
  ax = ap->ax;
  if (!ax)
    {
      fprintf(stderr,"axis->ax is null...");
      ap->ax = cp->ax;
      ax = ap->ax;
    }
  old_color = get_foreground_color(ax);
  set_foreground_color(ax, ss->cursor_color);

  /* if (cp->cx > cp->cursor_size) cx0 = cp->cx - cp->cursor_size; */
  /* if (cp->cy > cp->cursor_size) cy0 = cp->cy - cp->cursor_size; */
  /* csize = 2 * cp->cursor_size + 1; */
  /* what were those lines for? */
#else
  ax = cursor_context(cp);
#endif

  if (ss->tracking)
    cur = cp->tracking_cursor_style;
  else cur = cp->cursor_style;

  switch (cur)
    {
    case CURSOR_CROSS:
      draw_line(ax, cp->cx, cp->cy - cp->cursor_size, cp->cx, cp->cy + cp->cursor_size);
      draw_line(ax, cp->cx - cp->cursor_size, cp->cy, cp->cx + cp->cursor_size, cp->cy);
      break;

    case CURSOR_LINE:
      if ((with_inset_graph(ss)) &&
	  (cp->inset_graph))
	draw_inset_line_cursor(cp, ax);
      else draw_line(ax, cp->cx, ap->y_axis_y0 - 1, cp->cx, ap->y_axis_y1);
      break;

    case CURSOR_PROC:
#if USE_GTK
      free_cairo(ss->cr);
      ss->cr = NULL;
#endif
      XEN_CALL_3((Xen_is_procedure(cp->cursor_proc)) ? (cp->cursor_proc) : (ss->cursor_proc),
		 C_INT_TO_XEN_SOUND(cp->sound->index),
		 C_TO_XEN_INT(cp->chan),
		 /* this was time-graph, which was useless. It's now #t if we're in tracking-cursor mode */
		 C_TO_XEN_BOOLEAN(ss->tracking),
		 S_cursor_style " procedure");
#if USE_GTK
      ss->cr = make_cairo(ap->ax->wn);
      copy_context(cp);
#endif
      break;
    }

  /* now draw the play triangle below the x axis */
  fill_polygon(ax, 4,
	       cp->cx, ap->y_axis_y0,
	       cp->cx + play_arrow_size(ss), ap->y_axis_y0 + play_arrow_size(ss),
	       cp->cx, ap->y_axis_y0 + 2 * play_arrow_size(ss),
	       cp->cx, ap->y_axis_y0);

#if USE_GTK
  set_foreground_color(ax, old_color);
#endif
}



/* -------------------------------------------------------------------------------- */

#define AXIS_CONTEXT_ID_OK(Id) ((Id >= CHAN_GC) && (Id <= CHAN_TMPGC))
#define NO_SUCH_WIDGET XEN_ERROR_TYPE("no-such-widget")

#if USE_MOTIF
static graphics_context *get_ax(chan_info *cp, int ax_id, const char *caller, XEN ignored)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    return(set_context(cp, (chan_gc_t)ax_id));

  XEN_ERROR(XEN_ERROR_TYPE("no-such-graphics-context"),
	    XEN_LIST_6(C_TO_XEN_STRING("~A: no such graphics context: ~A, sound index: ~A (~A), chan: ~A"),
		       C_TO_XEN_STRING(caller),
		       C_TO_XEN_INT(ax_id),
		       C_INT_TO_XEN_SOUND(cp->sound->index),
		       C_TO_XEN_STRING(cp->sound->short_filename),
		       C_TO_XEN_INT(cp->chan)));
  return(NULL);
}


static graphics_context *get_ax_no_cr(chan_info *cp, int ax_id, const char *caller)
{
  return(get_ax(cp,ax_id, caller, XEN_FALSE));
}
#endif

#if USE_GTK
static graphics_context *get_ax(chan_info *cp, int ax_id, const char *caller, XEN xcr)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    {
      graphics_context *ax;
      ax = set_context(cp, (chan_gc_t)ax_id);
      /* (gdk_cairo_create (GDK_DRAWABLE (gtk_widget_get_window (car (channel-widgets 0 0)))))) -> '(cairo_t_ #<c_pointer 0x12bbca0>)
       *    (eq? (car hi) 'cairo_t_) -> #t
       */
      if ((Xen_is_list(xcr)) &&
	  (XEN_LIST_LENGTH(xcr) == 2) &&
	  (Xen_is_symbol(XEN_CAR(xcr))) &&
	  (strcmp("cairo_t_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(xcr))) == 0))
	ss->cr = (cairo_t *)XEN_UNWRAP_C_POINTER(XEN_CADR(xcr));
      else 
	XEN_ERROR(XEN_ERROR_TYPE("not-a-graphics-context"),
		  XEN_LIST_2(C_TO_XEN_STRING("~A: cairo_t argument is not a cairo_t pointer"),
			     C_TO_XEN_STRING(caller)));
      return(ax);
    }
  XEN_ERROR(XEN_ERROR_TYPE("no-such-graphics-context"),
	    XEN_LIST_6(C_TO_XEN_STRING("~A: no such graphics context: ~A, sound index: ~A (~A), chan: ~A"),
		       C_TO_XEN_STRING(caller),
		       C_TO_XEN_INT(ax_id),
		       C_INT_TO_XEN_SOUND(cp->sound->index),
		       C_TO_XEN_STRING(cp->sound->short_filename),
		       C_TO_XEN_INT(cp->chan)));
  return(NULL);
}

static graphics_context *get_ax_no_cr(chan_info *cp, int ax_id, const char *caller)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    {
      graphics_context *ax;
      ax = set_context(cp, (chan_gc_t)ax_id);
      return(ax);
    }
  XEN_ERROR(XEN_ERROR_TYPE("no-such-graphics-context"),
	    XEN_LIST_6(C_TO_XEN_STRING("~A: no such graphics context: ~A, sound index: ~A (~A), chan: ~A"),
		       C_TO_XEN_STRING(caller),
		       C_TO_XEN_INT(ax_id),
		       C_INT_TO_XEN_SOUND(cp->sound->index),
		       C_TO_XEN_STRING(cp->sound->short_filename),
		       C_TO_XEN_INT(cp->chan)));
  return(NULL);
}
#endif


#define TO_C_AXIS_CONTEXT(Snd, Chn, Ax, Caller, Cr) get_ax(get_cp(Snd, Chn, Caller), (Xen_is_integer(Ax)) ? XEN_TO_C_INT(Ax) : (int)CHAN_GC, Caller, Cr)
#define TO_C_AXIS_CONTEXT_NO_CR(Snd, Chn, Ax, Caller) get_ax_no_cr(get_cp(Snd, Chn, Caller), (Xen_is_integer(Ax)) ? XEN_TO_C_INT(Ax) : (int)CHAN_GC, Caller)


static XEN g_draw_line(XEN x0, XEN y0, XEN x1, XEN y1, XEN snd, XEN chn, XEN ax, XEN xcr)
{
  #define H_draw_line "(" S_draw_line " x0 y0 x1 y1 :optional snd chn (ax " S_time_graph ") cr): draw a line"

  ASSERT_CHANNEL(S_draw_line, snd, chn, 5);
  XEN_ASSERT_TYPE(Xen_is_number(x0), x0, 1, S_draw_line, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(y0), y0, 2, S_draw_line, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(x1), x1, 3, S_draw_line, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(y1), y1, 4, S_draw_line, "a number");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax), ax, 7, S_draw_line, "an integer such as " S_time_graph);

  draw_line(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_line, xcr),
	    XEN_TO_C_INT(x0),
	    XEN_TO_C_INT(y0),
	    XEN_TO_C_INT(x1),
	    XEN_TO_C_INT(y1));
  return(XEN_FALSE);
}


static XEN g_draw_dot(XEN x0, XEN y0, XEN size, XEN snd, XEN chn, XEN ax, XEN xcr)
{
  #define H_draw_dot "(" S_draw_dot " x0 y0 size :optional snd chn (ax " S_time_graph ") cr): draw a dot"
 
  ASSERT_CHANNEL(S_draw_dot, snd, chn, 4);
  XEN_ASSERT_TYPE(Xen_is_integer(x0), x0, 1, S_draw_dot, "an integer");
  XEN_ASSERT_TYPE(Xen_is_integer(y0), y0, 2, S_draw_dot, "an integer");
  XEN_ASSERT_TYPE(Xen_is_integer(size), size, 3, S_draw_dot, "an integer");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax), ax, 6, S_draw_dot, "an integer such as " S_time_graph);

  draw_dot(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dot, xcr),
	   XEN_TO_C_INT(x0),
	   XEN_TO_C_INT(y0),
	   XEN_TO_C_INT(size));
  return(XEN_FALSE);
}


static XEN g_fill_rectangle(XEN x0, XEN y0, XEN width, XEN height, XEN snd, XEN chn, XEN ax, XEN erase, XEN xcr)
{
  #define H_fill_rectangle "(" S_fill_rectangle " x0 y0 width height :optional snd chn (ax " S_time_graph ") erase cr): draw a filled rectangle"

  ASSERT_CHANNEL(S_fill_rectangle, snd, chn, 5);
  XEN_ASSERT_TYPE(Xen_is_number(x0), x0, 1, S_fill_rectangle, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(y0), y0, 2, S_fill_rectangle, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(width), width, 3, S_fill_rectangle, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(height), height, 4, S_fill_rectangle, "a number");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax), ax, 7, S_fill_rectangle, "an integer such as " S_time_graph);
  XEN_ASSERT_TYPE(Xen_is_boolean_or_unbound(erase), erase, 8, S_fill_rectangle, "a boolean");

  if ((Xen_is_boolean(erase)) &&
      (Xen_is_true(erase)))
    erase_rectangle(get_cp(snd, chn, S_fill_rectangle),
		    TO_C_AXIS_CONTEXT(snd, chn, ax, S_fill_rectangle, xcr),
		    XEN_TO_C_INT(x0),
		    XEN_TO_C_INT(y0),
		    XEN_TO_C_INT(width),
		    XEN_TO_C_INT(height));
  else fill_rectangle(TO_C_AXIS_CONTEXT(snd, chn, ax, S_fill_rectangle, xcr),
		      XEN_TO_C_INT(x0),
		      XEN_TO_C_INT(y0),
		      XEN_TO_C_INT(width),
		      XEN_TO_C_INT(height));
  return(XEN_FALSE);
}


static XEN g_draw_string(XEN text, XEN x0, XEN y0, XEN snd, XEN chn, XEN ax, XEN xcr)
{
  #define H_draw_string "(" S_draw_string " text x0 y0 :optional snd chn (ax " S_time_graph ") cr): draw a string"
  
  const char *tmp = NULL;
  ASSERT_CHANNEL(S_draw_string, snd, chn, 4);
  XEN_ASSERT_TYPE(Xen_is_string(text), text, 1, S_draw_string, "a string");
  XEN_ASSERT_TYPE(Xen_is_number(x0), x0, 2, S_draw_string, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(y0), y0, 3, S_draw_string, "a number");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax), ax, 6, S_draw_string, "an integer such as " S_time_graph);

  tmp = XEN_TO_C_STRING(text);
#if USE_MOTIF
  /* snd-xdraw to make motif draw-string act in the same way (coordinate-wise) as gtk */
  /*   despite the name, this is not a gtk function */
  gtk_style_draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string, xcr),
#else
  draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string, xcr),
#endif
	      XEN_TO_C_INT(x0),
	      XEN_TO_C_INT(y0),
	      tmp,
	      mus_strlen(tmp));
  return(text);
}


static point_t *vector_to_points(XEN pts, const char *caller, int *vector_len)
{
  int i, j, vlen = 0, len = 0;
  point_t *pack_pts;

  vlen = XEN_VECTOR_LENGTH(pts);
  if (vlen & 1)
    XEN_ERROR(XEN_ERROR_TYPE("bad-length"),
	      XEN_LIST_3(C_TO_XEN_STRING("~A: length of vector of points (~A) must be even"),
			 C_TO_XEN_STRING(caller),
			 C_TO_XEN_INT(vlen)));
  if (vlen <= 0) 
    XEN_ERROR(NO_DATA,
	      XEN_LIST_3(C_TO_XEN_STRING("~A: empty points vector? ~A"), 
			 C_TO_XEN_STRING(caller), 
			 pts));
  len = vlen / 2;
  (*vector_len) = len;
  pack_pts = (point_t *)calloc(len, sizeof(point_t));

  for (i = 0, j = 0; i < len; i++, j += 2)
    {
      pack_pts[i].x = XEN_TO_C_INT(XEN_VECTOR_REF(pts, j));
      pack_pts[i].y = XEN_TO_C_INT(XEN_VECTOR_REF(pts, j + 1));
    }
  return(pack_pts);
}


  static XEN g_draw_lines(XEN pts, XEN snd, XEN chn, XEN ax, XEN xcr)
{
  /* pts should be a vector of integers as (x y) pairs */
  #define H_draw_lines "(" S_draw_lines " lines :optional snd chn (ax " S_time_graph ") cr): draw a vector of lines"

  point_t *pack_pts;
  graphics_context *ax1;
  int vlen = 0;

  ASSERT_CHANNEL(S_draw_lines, snd, chn, 2);
  XEN_ASSERT_TYPE(Xen_is_vector(pts), pts, 1, S_draw_lines, "a vector");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax), ax, 4, S_draw_lines, "an integer such as " S_time_graph);

  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_lines, xcr);

  pack_pts = vector_to_points(pts, S_draw_lines, &vlen);
  draw_lines(ax1, pack_pts, vlen);

  free(pack_pts);
  return(pts);
}


  static XEN g_draw_dots(XEN pts, XEN size, XEN snd, XEN chn, XEN ax, XEN xcr)
{
  /* pts should be a vector of integers as (x y) pairs */
  #define H_draw_dots "(" S_draw_dots " positions :optional dot-size snd chn (ax " S_time_graph ") cr): draw a vector of dots"
 
  point_t *pack_pts;
  graphics_context *ax1;
  int vlen = 0;

  ASSERT_CHANNEL(S_draw_dots, snd, chn, 3);
  XEN_ASSERT_TYPE(Xen_is_vector(pts), pts, 1, S_draw_dots, "a vector");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(size), size, 2, S_draw_dots, "an integer");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax), ax, 5, S_draw_dots, "an integer such as " S_time_graph);

  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dots, xcr);

  pack_pts = vector_to_points(pts, S_draw_dots, &vlen);
  draw_points(ax1,
	      pack_pts, 
	      vlen,
	      (Xen_is_integer(size)) ? XEN_TO_C_INT(size) : 1);

  free(pack_pts);
  return(pts);
}


  static XEN g_fill_polygon(XEN pts, XEN snd, XEN chn, XEN ax_id, XEN xcr)
{ 
  #define H_fill_polygon "(" S_fill_polygon " points :optional snd chn (ax " S_time_graph ") cr): draw a filled polygon"

  point_t *pack_pts;
  graphics_context *ax;
  int vlen = 0;

  ASSERT_CHANNEL(S_fill_polygon, snd, chn, 2);
  XEN_ASSERT_TYPE(Xen_is_vector(pts), pts, 1, S_fill_polygon, "a vector");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax_id), ax_id, 4, S_fill_polygon, "an integer such as " S_time_graph);

  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_fill_polygon, xcr);

  pack_pts = vector_to_points(pts, S_fill_polygon, &vlen);
#if USE_MOTIF
  XFillPolygon(ax->dp, ax->wn, ax->gc, pack_pts, vlen, Complex, CoordModeOrigin);
#else
  fill_polygon_from_array(ax, pack_pts, vlen);
#endif

  free(pack_pts);
  return(pts);
}


static XEN g_make_bezier(XEN args1)
{
  /* used in musglyphs.rb -- not currently used anywhere else */

  #define S_make_bezier "make-bezier"
  #define H_make_bezier "(" S_make_bezier " x0 y0 x1 y1 x2 y2 x3 y3 n): return a vector of points corresponding to the bezier curve \
defined by the 4 controlling points x0..y3; 'n' is how many points to return"
  /* XDrawBezier from cmn's glfed.c (where it was assuming PostScript coordinates) */

  int ax, ay, bx, by, cx, cy, i;
  float incr, val;
  int x[4];
  int y[4];
  int n = 50;
  XEN pts, args;

  args = XEN_COPY_ARG(args1);
  for (i = 0; i < 4; i++)
    {
      x[i] = XEN_TO_C_INT(XEN_CAR(args));
      y[i] = XEN_TO_C_INT(XEN_CADR(args));
      args = XEN_CDDR(args);
    }
  if (!Xen_is_null(args)) 
    n = XEN_TO_C_INT(XEN_CAR(args));

  cx = 3 * (x[1] - x[0]);
  cy = 3 * (y[1] - y[0]);
  bx = 3 * (x[2] - x[1]) - cx;
  by = 3 * (y[2] - y[1]) - cy;
  ax = x[3] - (x[0] + cx + bx);
  ay = y[3] - (y[0] + cy + by);
  incr = 1.0 / (float)n;
  pts = XEN_MAKE_VECTOR(2 * (n + 1), XEN_ZERO);

  XEN_VECTOR_SET(pts, 0, C_TO_XEN_INT(x[0]));
  XEN_VECTOR_SET(pts, 1, C_TO_XEN_INT(y[0]));

  for (i = 1, val = incr; i <= n; i++, val += incr)
    {
      XEN_VECTOR_SET(pts, i * 2, C_TO_XEN_INT((int)(x[0] + val * (cx + (val * (bx + (val * ax)))))));
      XEN_VECTOR_SET(pts, i * 2 + 1, C_TO_XEN_INT((int)(y[0] + val * (cy + (val * (by + (val * ay)))))));
    }
  return(pts);
}


 static XEN g_foreground_color(XEN snd, XEN chn, XEN xax)
{
  #define H_foreground_color "(" S_foreground_color " :optional snd chn (ax " S_time_graph ")): current drawing color"
  chan_info *cp;
  graphics_context *ax;

  ASSERT_CHANNEL(S_foreground_color, snd, chn, 1);
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(xax), xax, 3, S_foreground_color, "an integer");

  cp = get_cp(snd, chn, S_foreground_color);
  if (!cp) return(XEN_FALSE);

  ax = get_ax_no_cr(cp, (Xen_is_integer(xax)) ? XEN_TO_C_INT(xax) : (int)CHAN_GC, S_foreground_color);
  return(XEN_WRAP_PIXEL(get_foreground_color(ax)));
}


 static XEN g_set_foreground_color(XEN color, XEN snd, XEN chn, XEN ax)
{
  chan_info *cp;

  ASSERT_CHANNEL(S_setB S_foreground_color, snd, chn, 2);
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_foreground_color, "a color");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax), ax, 4, S_setB S_foreground_color, "an integer");

  cp = get_cp(snd, chn, S_setB S_foreground_color);
  if (!cp) return(XEN_FALSE);

  set_foreground_color(get_ax_no_cr(cp, (Xen_is_integer(ax)) ? XEN_TO_C_INT(ax) : (int)CHAN_GC, S_setB S_foreground_color),
		       XEN_UNWRAP_PIXEL(color));
  return(color);
}

WITH_FOUR_SETTER_ARGS(g_set_foreground_color_reversed, g_set_foreground_color)


#if USE_MOTIF

static XEN g_set_current_font(XEN id, XEN snd, XEN chn, XEN ax_id)
{
  graphics_context *ax;

  ASSERT_CHANNEL(S_setB S_current_font, snd, chn, 2);
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax_id), ax_id, 4, S_setB S_current_font, "an integer such as time-graph");
  XEN_ASSERT_TYPE((Xen_is_list(id)) &&
		  (XEN_LIST_LENGTH(id) >= 2) &&
		  (Xen_is_symbol(XEN_CAR(id))) &&
		  (strcmp("Font", XEN_SYMBOL_TO_C_STRING(XEN_CAR(id))) == 0), id, 1, S_setB S_current_font, "a Font");

  ax = TO_C_AXIS_CONTEXT_NO_CR(snd, chn, ax_id, S_current_font);
  ax->current_font = (Font)XEN_TO_C_ULONG(XEN_CADR(id));
  XSetFont(ax->dp, ax->gc, ax->current_font);
  return(id);
}


static XEN g_current_font(XEN snd, XEN chn, XEN ax_id)
{
  #define H_current_font "(" S_current_font " :optional snd chn (ax " S_time_graph ")): current font id"
  graphics_context *ax;
  chan_info *cp;

  ASSERT_CHANNEL(S_current_font, snd, chn, 1);
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax_id), ax_id, 3, S_current_font, "an integer such as time-graph");
  cp = get_cp(snd, chn, S_current_font);
  if (!cp) return(XEN_FALSE);

  ax = get_ax_no_cr(cp, (Xen_is_integer(ax_id)) ? XEN_TO_C_INT(ax_id) : (int)CHAN_GC, S_current_font);
  if (ax->current_font == 0)
    {
      if ((cp->axis) && (cp->axis->ax))
	return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Font"),
			  C_TO_XEN_ULONG(cp->axis->ax->current_font)));
      else return(XEN_FALSE);
    }
  return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Font"),
		    C_TO_XEN_ULONG(ax->current_font)));
}


#else

static XEN g_set_current_font(XEN id, XEN snd, XEN chn, XEN ax_id)
{
  graphics_context *ax;

  ASSERT_CHANNEL(S_setB S_current_font, snd, chn, 2);
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax_id), ax_id, 4, S_setB S_current_font, "an integer such as time-graph");

  ax = TO_C_AXIS_CONTEXT_NO_CR(snd, chn, ax_id, S_setB S_current_font);
  XEN_ASSERT_TYPE((Xen_is_wrapped_c_pointer(id)) ||
		  (Xen_is_list(id) && 
		   (XEN_LIST_LENGTH(id) >= 2) &&
		   (Xen_is_symbol(XEN_CAR(id)))),
		  id, 1, S_setB S_current_font, "a wrapped object or a raw pointer");

  if (Xen_is_wrapped_c_pointer(id))
    ax->current_font = (PangoFontDescription *)XEN_UNWRAP_C_POINTER(id); 
  else ax->current_font = (PangoFontDescription *)XEN_UNWRAP_C_POINTER(XEN_CADR(id));
  return(id);
}


static XEN g_current_font(XEN snd, XEN chn, XEN ax_id)
{
  #define H_current_font "(" S_current_font " :optional snd chn (ax " S_time_graph ")): current font id"
  graphics_context *ax;
  ASSERT_CHANNEL(S_current_font, snd, chn, 1);
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(ax_id), ax_id, 3, S_current_font, "an integer such as time-graph");
  ax = TO_C_AXIS_CONTEXT_NO_CR(snd, chn, ax_id, S_current_font);
  return(XEN_WRAP_C_POINTER(ax->current_font));
}

#endif

WITH_FOUR_SETTER_ARGS(g_set_current_font_reversed, g_set_current_font)


static XEN g_make_graph_data(XEN snd, XEN chn, XEN edpos, XEN lo, XEN hi)
{
  #define H_make_graph_data "(" S_make_graph_data " :optional snd chn edpos low high): \
return either a vct (if the graph has one trace), or a list of two vcts (the two sides of the envelope graph). \
'edpos' defaults to the " S_current_edit_position ", 'low' defaults to the current window left sample, and \
'high' defaults to the current rightmost sample. (" S_graph_data " (" S_make_graph_data ")) reimplements the time domain graph."

  chan_info *cp;

  ASSERT_CHANNEL(S_make_graph_data, snd, chn, 1);
  cp = get_cp(snd, chn, S_make_graph_data);
  if (!cp) return(XEN_FALSE);

  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(lo), lo, 4, S_make_graph_data, "an integer");
  XEN_ASSERT_TYPE(Xen_is_integer_or_unbound(hi), hi, 5, S_make_graph_data, "an integer");

  return(make_graph_data(cp,
			 to_c_edit_position(cp, edpos, S_make_graph_data, 3),
			 (Xen_is_long_long_int(lo)) ? XEN_TO_C_LONG_LONG(lo) : -1,
			 (Xen_is_long_long_int(hi)) ? XEN_TO_C_LONG_LONG(hi) : -1));
}


 static XEN g_graph_data(XEN data, XEN snd, XEN chn, XEN ax, XEN lo, XEN hi, XEN style, XEN xcr)
{
  #define H_graph_data "(" S_graph_data " data :optional snd chn (context " S_copy_context ") low high graph-style cr): \
display 'data' in the time domain graph of snd's channel chn using the graphics context context (normally " S_copy_context "), placing the \
data in the recipient's graph between points low and high in the drawing mode graphic-style."

  chan_info *cp;
  vct *v0, *v1 = NULL;

  ASSERT_CHANNEL(S_graph_data, snd, chn, 2);

  cp = get_cp(snd, chn, S_graph_data);
  if (!cp) return(XEN_FALSE);

  if (Xen_is_false(data)) return(XEN_FALSE);
  XEN_ASSERT_TYPE((Xen_is_list(data) && 
		   (XEN_LIST_LENGTH(data) == 2) &&
		   (mus_is_vct(XEN_CAR(data))) &&
		   (mus_is_vct(XEN_CADR(data)))) || 
		  (mus_is_vct(data)), 
		  data, 1, S_graph_data, "a list of 2 vcts or vct");
  XEN_ASSERT_TYPE(Xen_is_integer_boolean_or_unbound(ax), ax, 4, S_graph_data, "an integer");
  XEN_ASSERT_TYPE(Xen_is_long_long_int(lo) || Xen_is_false(lo) || !Xen_is_bound(lo), lo, 5, S_graph_data, "a sample number");
  XEN_ASSERT_TYPE(Xen_is_long_long_int(hi) || Xen_is_false(hi) || !Xen_is_bound(hi), hi, 6, S_graph_data, "a sample number");
  XEN_ASSERT_TYPE(Xen_is_integer_boolean_or_unbound(style), style, 7, S_graph_data, "an integer");

  if (Xen_is_list(data))
    {
      v0 = xen_to_vct(XEN_CAR(data));
      v1 = xen_to_vct(XEN_CADR(data));
    }
  else v0 = xen_to_vct(data);

  draw_graph_data(cp, 
		  (Xen_is_long_long_int(lo)) ? XEN_TO_C_LONG_LONG(lo) : -1,
		  (Xen_is_long_long_int(hi)) ? XEN_TO_C_LONG_LONG(hi) : -1,
		  mus_vct_length(v0),
		  mus_vct_data(v0),
		  (v1) ? (mus_vct_data(v1)) : NULL,
		  get_ax(cp, (Xen_is_integer(ax)) ? XEN_TO_C_INT(ax) : (int)CHAN_GC, S_graph_data, xcr),
		  (Xen_is_integer(style)) ? (graph_style_t)XEN_TO_C_INT(style) : cp->time_graph_style);
  return(data);
}


static XEN g_main_widgets(void)
{
  #define H_main_widgets "(" S_main_widgets "): top level \
widgets (list (0)main-app (1)main-shell (2)main-pane (3)sound-pane (4)listener-pane (5)notebook-outer-pane)"

  XEN bad_temp, res; /* needed by old gcc -- gets confused by straight arg list */
  int loc;
#if USE_MOTIF
  bad_temp = XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("XtAppContext"), 
			C_TO_XEN_ULONG((unsigned long)MAIN_APP(ss)));
#else
  bad_temp = XEN_WRAP_WINDOW(MAIN_WINDOW(ss));
#endif
  loc = snd_protect(bad_temp);
  res = XEN_CONS(bad_temp,
	   XEN_CONS(XEN_WRAP_WIDGET(MAIN_SHELL(ss)),
             XEN_CONS(XEN_WRAP_WIDGET(MAIN_PANE(ss)),
               XEN_CONS(XEN_WRAP_WIDGET(SOUND_PANE(ss)),
		 XEN_CONS(XEN_WRAP_WIDGET(ss->listener_pane),
		   XEN_CONS(XEN_WRAP_WIDGET(SOUND_PANE_BOX(ss)),
		     XEN_EMPTY_LIST))))));
  snd_unprotect_at(loc);
  return(res);
}


static XEN dialog_widgets;
static XEN new_widget_hook;
/* ideally this would be an "after method" on XtCreateWidget or gtk_new_* */

void run_new_widget_hook(widget_t w)
{
  if (XEN_HOOKED(new_widget_hook))
    run_hook(new_widget_hook, XEN_LIST_1(XEN_WRAP_WIDGET(w)), S_new_widget_hook);
}


static void check_dialog_widget_table(void)
{
  if (!(Xen_is_vector(dialog_widgets)))
    {
      dialog_widgets = XEN_MAKE_VECTOR(NUM_DIALOGS, XEN_FALSE);
      XEN_PROTECT_FROM_GC(dialog_widgets);
    }
}


static XEN g_dialog_widgets(void)
{
  #define H_dialog_widgets "(" S_dialog_widgets "): dialog widgets (each " PROC_FALSE " if not yet created): (list \
 (0 " S_color_orientation_dialog ") (1 " PROC_FALSE ") (2 " S_enved_dialog ") (3 " PROC_FALSE ") (4 " PROC_FALSE ") (5 " S_transform_dialog ") \
 (6 " S_open_file_dialog ") (7 " S_save_sound_dialog ") (8 " S_view_files_dialog ") (9 raw data dialog) (10 new file dialog) \
 (11 " S_mix_file_dialog ") (12 " S_edit_header_dialog ") (13 " S_find_dialog ") (14 " S_help_dialog ") (15 listener completion) \
 (16 " S_view_mixes_dialog ") (17 " S_print_dialog ") (19 " S_view_regions_dialog ") \
 (20 " S_info_dialog ") (21 " PROC_FALSE ") (22 " S_save_selection_dialog ") (23 " S_insert_file_dialog ") \
 (24 " S_save_region_dialog ") (25 " S_preferences_dialog "))"

  check_dialog_widget_table();
  return(XEN_VECTOR_TO_LIST(dialog_widgets));
}


void set_dialog_widget(snd_dialog_t which, widget_t wid)
{
  if (ss->dialogs == NULL)
    {
      ss->dialogs_size = 8;
      ss->num_dialogs = 0;
      ss->dialogs = (widget_t *)calloc(ss->dialogs_size, sizeof(widget_t));
    }
  else
    {
      if (ss->num_dialogs == ss->dialogs_size)
	{
	  int i;
	  ss->dialogs_size += 8;
	  ss->dialogs = (widget_t *)realloc(ss->dialogs, ss->dialogs_size * sizeof(widget_t));
	  for (i = ss->num_dialogs; i < ss->dialogs_size; i++) ss->dialogs[i] = NULL;
	}
    }
  ss->dialogs[ss->num_dialogs++] = wid;
  check_dialog_widget_table();

#if USE_GTK && HAVE_GTK_3
  gtk_widget_override_background_color(wid, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)(ss->basic_color)); /* was GTK_STATE_NORMAL which can't be right */
#endif

  if (Xen_is_false(XEN_VECTOR_REF(dialog_widgets, (int)which)))
    XEN_VECTOR_SET(dialog_widgets, (int)which, 
		   XEN_WRAP_WIDGET(wid));
  else 
    {
      if (Xen_is_widget(XEN_VECTOR_REF(dialog_widgets, (int)which)))
	XEN_VECTOR_SET(dialog_widgets, (int)which, 
		       XEN_LIST_2(XEN_WRAP_WIDGET(wid), 
				  XEN_VECTOR_REF(dialog_widgets, (int)which)));
      else XEN_VECTOR_SET(dialog_widgets, (int)which, 
			  XEN_CONS(XEN_WRAP_WIDGET(wid), 
				   XEN_VECTOR_REF(dialog_widgets, (int)which)));
    }
  run_new_widget_hook(wid);
}


static XEN g_widget_position(XEN wid)
{
  #define H_widget_position "(" S_widget_position " wid): widget's position, (list x y), in pixels"
  widget_t w;
  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_widget_position, "a Widget");  
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (!w)
    XEN_ERROR(NO_SUCH_WIDGET,
	      XEN_LIST_2(C_TO_XEN_STRING(S_widget_position ": no such widget: ~A"),
			 wid));
  return(XEN_LIST_2(C_TO_XEN_INT(widget_x(w)),
		    C_TO_XEN_INT(widget_y(w))));
}


static XEN g_set_widget_position(XEN wid, XEN xy)
{
  widget_t w;
  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_setB S_widget_position, "a Widget");  
  XEN_ASSERT_TYPE(Xen_is_list(xy) && (XEN_LIST_LENGTH(xy) == 2), xy, 2, S_setB S_widget_position, "a list: (x y)");  
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    set_widget_position(w,
			mus_iclamp(0, XEN_TO_C_INT(XEN_CAR(xy)), LOTSA_PIXELS),
			mus_iclamp(0, XEN_TO_C_INT(XEN_CADR(xy)), LOTSA_PIXELS));
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_2(C_TO_XEN_STRING(S_setB S_widget_position ": no such widget: ~A"),
			    wid));
  return(wid);
}


static XEN g_widget_size(XEN wid)
{
  #define H_widget_size "(" S_widget_size " wid): widget's size, (list width height), in pixels"
  widget_t w;
  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_widget_size, "a Widget"); 
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (!w)
    XEN_ERROR(NO_SUCH_WIDGET,
	      XEN_LIST_2(C_TO_XEN_STRING(S_widget_size ": no such widget: ~A"),
			 wid));
  return(XEN_LIST_2(C_TO_XEN_INT(widget_width(w)),
		    C_TO_XEN_INT(widget_height(w))));
}


static XEN g_set_widget_size(XEN wid, XEN wh)
{
  widget_t w;
  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_setB S_widget_size, "a Widget");  
  XEN_ASSERT_TYPE(Xen_is_list(wh) && (XEN_LIST_LENGTH(wh) == 2), wh, 2, S_setB S_widget_size, "a list: (width height)");  
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    set_widget_size(w,
		    mus_iclamp(1, XEN_TO_C_INT(XEN_CAR(wh)), LOTSA_PIXELS),
		    mus_iclamp(1, XEN_TO_C_INT(XEN_CADR(wh)), LOTSA_PIXELS));
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_2(C_TO_XEN_STRING(S_setB S_widget_size ": no such widget: ~A"),
			    wid));
  return(wid);
}


static XEN g_widget_text(XEN wid)
{
  #define H_widget_text "(" S_widget_text " wid): widget's text or label"
  widget_t w;

  XEN res = XEN_FALSE;
  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_widget_text, "a Widget");

  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    {
#if USE_MOTIF
      char *text = NULL;
      if ((XmIsText(w)) || (XmIsTextField(w)))
	{
	  text = XmTextGetString(w);
	  res = C_TO_XEN_STRING(text);
	}
      else
	{
	  XmString s1 = NULL;
	  XtVaGetValues(w, XmNlabelString, &s1, NULL);
	  if (XmStringEmpty(s1)) return(XEN_FALSE);
	  text = (char *)XmStringUnparse(s1, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  XmStringFree(s1);
	  res = C_TO_XEN_STRING(text);
	}
      if (text) XtFree(text);
      return(res);
#else
      if (GTK_IS_ENTRY(w))
	return(C_TO_XEN_STRING((char *)gtk_entry_get_text(GTK_ENTRY(w))));
      else
	{
	  if ((GTK_IS_BIN(w)) && (GTK_IS_LABEL(BIN_CHILD(w))))
	    return(C_TO_XEN_STRING((char *)gtk_label_get_text(GTK_LABEL(BIN_CHILD(w)))));
	  else
	    {
	      if (GTK_IS_LABEL(w))
		return(C_TO_XEN_STRING((char *)gtk_label_get_text(GTK_LABEL(w))));
	      else
		{
		  if (GTK_IS_TEXT_VIEW(w))
		    {
		      XEN val = XEN_FALSE;
		      char *text;
		      text = sg_get_text(w, 0, -1);
		      if (text)
			{
			  val = C_TO_XEN_STRING(text); /* this copies, so it should be safe to free the original */
			  g_free(text);
			}
		      return(val);
		    }
		}
	    }
	}
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_2(C_TO_XEN_STRING(S_widget_text ": no such widget: ~A"),
			    wid));
  return(res);
}


static XEN g_set_widget_text(XEN wid, XEN text)
{
  widget_t w;
  const char *str = NULL;

  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_setB S_widget_text, "a Widget");
  XEN_ASSERT_TYPE(Xen_is_string(text) || Xen_is_false(text), text, 2, S_setB S_widget_text, "a string");

  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    {
      if (Xen_is_string(text)) str = XEN_TO_C_STRING(text);
#if USE_MOTIF
      if ((XmIsText(w)) || (XmIsTextField(w)))
	XmTextSetString(w, (char *)str);
      else set_button_label(w, str);
#else
      if (GTK_IS_ENTRY(w))
	gtk_entry_set_text(GTK_ENTRY(w), str);
      else set_button_label(w, str);
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_2(C_TO_XEN_STRING(S_setB S_widget_text ": no such widget: ~A"),
			    wid));
  return(text);
}


static XEN g_hide_widget(XEN wid)
{
  #define H_hide_widget "(" S_hide_widget " widget): hide or undisplay widget"
  widget_t w;
  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_hide_widget, "a Widget");  
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    {
#if USE_MOTIF
      XtUnmanageChild(w);
#else
      gtk_widget_hide(w);
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_2(C_TO_XEN_STRING(S_hide_widget ": no such widget: ~A"),
			    wid));
  return(wid);
}


static XEN g_show_widget(XEN wid)
{
  #define H_show_widget "(" S_show_widget " widget): show or display widget"
  widget_t w;
  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_show_widget, "a Widget");  
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    {
#if USE_MOTIF
      XtManageChild(w);
#else
      gtk_widget_show(w);
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_2(C_TO_XEN_STRING(S_show_widget ": no such widget: ~A"),
			    wid));
  return(wid);
}


static XEN g_focus_widget(XEN wid)
{
  #define H_focus_widget "(" S_focus_widget " widget): cause widget to receive input focus"
  widget_t w;
  XEN_ASSERT_TYPE(Xen_is_widget(wid), wid, 1, S_focus_widget, "a Widget");
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    goto_window(w);
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_2(C_TO_XEN_STRING(S_focus_widget ": no such widget: ~A"),
			    wid));
  return(wid);
}


static XEN g_snd_gcs(void)
{
  #define H_snd_gcs "(" S_snd_gcs "): a list of Snd graphics contexts (list (0 basic) (1 selected_basic) (2 combined) (3 \
cursor) (4 selected_cursor) (5 selection) (6 selected_selection) (7 erase) (8 selected_erase) (9 mark) (10 selected_mark) (11 mix) (12 \
fltenv_basic) (13 fltenv_data))."

#if USE_MOTIF
      #define XEN_WRAP_SND_GC(Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GC"), XEN_WRAP_C_POINTER(Value))
#else
  #if USE_GTK
      #define XEN_WRAP_SND_GC(Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("gc_t_"), XEN_WRAP_C_POINTER(Value))
  #else
      #define XEN_WRAP_SND_GC(Value) XEN_FALSE
  #endif
#endif

#if (!USE_NO_GUI)
    return(XEN_CONS(XEN_WRAP_SND_GC(ss->basic_gc),
	    XEN_CONS(XEN_WRAP_SND_GC(ss->selected_basic_gc), 
	     XEN_CONS(XEN_WRAP_SND_GC(ss->combined_basic_gc), 
	      XEN_CONS(XEN_WRAP_SND_GC(ss->cursor_gc), 
               XEN_CONS(XEN_WRAP_SND_GC(ss->selected_cursor_gc), 
                XEN_CONS(XEN_WRAP_SND_GC(ss->selection_gc), 
                 XEN_CONS(XEN_WRAP_SND_GC(ss->selected_selection_gc), 
                  XEN_CONS(XEN_WRAP_SND_GC(ss->erase_gc), 
                   XEN_CONS(XEN_WRAP_SND_GC(ss->selected_erase_gc), 
                    XEN_CONS(XEN_WRAP_SND_GC(ss->mark_gc), 
                     XEN_CONS(XEN_WRAP_SND_GC(ss->selected_mark_gc), 
                      XEN_CONS(XEN_WRAP_SND_GC(ss->mix_gc), 
                       XEN_CONS(XEN_WRAP_SND_GC(ss->fltenv_basic_gc), 
                        XEN_CONS(XEN_WRAP_SND_GC(ss->fltenv_data_gc), 
			 XEN_EMPTY_LIST)))))))))))))));
#else
  return(XEN_EMPTY_LIST);
#endif
}


static XEN g_snd_color(XEN choice)
{
  #define H_snd_color "(" S_snd_color " num): color associated with 'num' -- see table of colors in snd-draw.c"
  color_t col;
  XEN_ASSERT_TYPE(Xen_is_integer(choice), choice, 1, S_snd_color, "an integer");

  switch (XEN_TO_C_INT(choice))
    {
    case 0: col = ss->white;                          break;
    case 1: col = ss->black;                          break;
    case 2: col = ss->red;                            break;
    case 3: col = ss->yellow;                         break;
    case 4: col = ss->green;                          break;
    case 5: col = ss->light_blue;                     break;
    case 6: col = ss->lighter_blue;                   break;

    case 7: col = ss->data_color;                     break;
    case 8: col = ss->selected_data_color;            break;
    case 9: col = ss->mark_color;                     break;
    case 10: col = ss->graph_color;                   break;
    case 11: col = ss->selected_graph_color;          break;
    case 12: col = ss->listener_color;                break;
    case 13: col = ss->listener_text_color;           break;

    case 14: col = ss->basic_color;                   break;
    case 15: col = ss->selection_color;               break;
    case 16: col = ss->zoom_color;                    break;
    case 17: col = ss->position_color;                break;
    case 18: col = ss->highlight_color;               break;
    case 19: col = ss->enved_waveform_color;          break;
    case 20: col = ss->cursor_color;                  break;

    case 21: col = ss->text_focus_color;              break;
    case 22: col = ss->filter_control_waveform_color; break;
    case 23: col = ss->mix_color;                     break;
    case 25: col = ss->sash_color;                    break;

    case 31: col = ss->grid_color;                    break;
    case 32: col = ss->selected_grid_color;           break;
    case 33: 
      if (ss->axis_color_set)
	col = ss->axis_color;
      else col = ss->black;
      break;
    default: col = ss->black;                         break;
    }
  return(XEN_WRAP_PIXEL(col));
}


static XEN g_snd_font(XEN choice)
{
#if USE_MOTIF
  #define WRAP_FONT(Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Font"), C_TO_XEN_ULONG((unsigned long)Value))
#else
  #define WRAP_FONT(Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("PangoFontDescription_"), XEN_WRAP_C_POINTER(Value))
#endif

  #define H_snd_font "(" S_snd_font " num): font associated with 'num' -- see table of fonts in snd-draw.c"
  XEN_ASSERT_TYPE(Xen_is_integer(choice), choice, 1, S_snd_font, "an integer");

  switch (XEN_TO_C_INT(choice))
    {
#if USE_MOTIF
    case 0: return(WRAP_FONT(ss->peaks_fontstruct->fid));        break;
    case 1: return(WRAP_FONT(ss->bold_peaks_fontstruct->fid));   break;
    case 2: return(WRAP_FONT(ss->tiny_fontstruct->fid));         break;
    case 3: return(WRAP_FONT(ss->axis_label_fontstruct->fid));   break;
    case 4: return(WRAP_FONT(ss->axis_numbers_fontstruct->fid)); break;
    case 5: return(WRAP_FONT(ss->listener_fontstruct->fid));     break;
#endif
#if USE_GTK
    case 0: return(WRAP_FONT(ss->peaks_fnt));                    break;
    case 1: return(WRAP_FONT(ss->bold_peaks_fnt));               break;
    case 2: return(WRAP_FONT(ss->tiny_fnt));                     break;
    case 3: return(WRAP_FONT(ss->axis_label_fnt));               break;
    case 4: return(WRAP_FONT(ss->axis_numbers_fnt));             break;
    case 5: return(WRAP_FONT(ss->listener_fnt));                 break;
#endif
    default: return(XEN_FALSE);                                       break;
    }
  return(XEN_FALSE);
}


static XEN g_make_cairo(XEN drawer)
{
  #define H_make_cairo "(" S_make_cairo " widget) in gtk, this returns a new cairo_t to draw on the widget."

#if USE_GTK
  #define C_TO_XEN_cairo_t(Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("cairo_t_"), XEN_WRAP_C_POINTER(Value))
  cairo_t *cr;

  XEN_ASSERT_TYPE(Xen_is_widget(drawer), drawer, 1, S_make_cairo, "a widget");

#if (!HAVE_GTK_3)
  cr = make_cairo(GDK_DRAWABLE(gtk_widget_get_window(XEN_UNWRAP_WIDGET(drawer))));
#else
  cr = make_cairo(GDK_WINDOW(gtk_widget_get_window(XEN_UNWRAP_WIDGET(drawer))));
#endif

  return(C_TO_XEN_cairo_t(cr));
#endif

  return(XEN_FALSE);
}


static XEN g_free_cairo(XEN xcr)
{
  #define H_free_cairo "(" S_free_cairo " cr) in gtk, this frees (destroys) the cairo_t 'cr'."

#if USE_GTK
  if ((Xen_is_list(xcr)) &&
      (XEN_LIST_LENGTH(xcr) == 2) &&
      (Xen_is_symbol(XEN_CAR(xcr))) &&
      (strcmp("cairo_t_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(xcr))) == 0))
    free_cairo((cairo_t *)XEN_UNWRAP_C_POINTER(XEN_CADR(xcr)));
  else 
    XEN_ERROR(XEN_ERROR_TYPE("not-a-graphics-context"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_free_cairo ": cairo_t argument is not a cairo_t pointer: ~A"), xcr));
#endif
  return(XEN_FALSE);
}


#if HAVE_GL
static mus_float_t gl_currents[6] = {DEFAULT_SPECTRO_X_ANGLE, DEFAULT_SPECTRO_Y_ANGLE, DEFAULT_SPECTRO_Z_ANGLE, 
			       DEFAULT_SPECTRO_X_SCALE, DEFAULT_SPECTRO_Y_SCALE, DEFAULT_SPECTRO_Z_SCALE};
static mus_float_t x_currents[6] = {90.0, 0.0, 358.0, 1.0, 1.0, 0.1};

void sgl_save_currents(void)
{
  mus_float_t *vals;
  if (with_gl(ss)) vals = gl_currents; else vals = x_currents;
  vals[0] = spectro_x_angle(ss);
  vals[1] = spectro_y_angle(ss);
  vals[2] = spectro_z_angle(ss);
  vals[3] = spectro_x_scale(ss);
  vals[4] = spectro_y_scale(ss);
  vals[5] = spectro_z_scale(ss);
}


void sgl_set_currents(bool with_dialogs)
{
  mus_float_t *vals;
  if (with_gl(ss)) vals = gl_currents; else vals = x_currents;
  in_set_spectro_x_angle(vals[0]);
  in_set_spectro_y_angle(vals[1]);
  in_set_spectro_z_angle(vals[2]);
  in_set_spectro_x_scale(vals[3]);
  in_set_spectro_y_scale(vals[4]);
  in_set_spectro_z_scale(vals[5]);
  if (with_dialogs) reflect_spectro();
  chans_field(FCP_X_ANGLE, vals[0]);
  chans_field(FCP_Y_ANGLE, vals[1]);
  chans_field(FCP_Z_ANGLE, vals[2]);
  chans_field(FCP_X_SCALE, vals[3]);
  chans_field(FCP_Y_SCALE, vals[4]);
  chans_field(FCP_Z_SCALE, vals[5]);
}
#endif



/* -------- shared color funcs -------- */

static XEN g_color_p(XEN obj) 
{
  #define H_color_p "(" S_is_color " obj): " PROC_TRUE " if obj is a color"
  return(C_TO_XEN_BOOLEAN(Xen_is_pixel(obj)));
}


mus_float_t check_color_range(const char *caller, XEN val)
{
  mus_float_t rf;
  rf = XEN_TO_C_DOUBLE(val);
  if ((rf > 1.0) || (rf < 0.0))
    XEN_OUT_OF_RANGE_ERROR(caller, 1, val, "value must be between 0.0 and 1.0");
  return(rf);
}


static XEN g_set_cursor_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_cursor_color, "a color"); 
  color_cursor(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}


static XEN g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color "): cursor color"
  return(XEN_WRAP_PIXEL(ss->cursor_color));
}


#if USE_MOTIF
static void highlight_recolor_everything(widget_t w, color_t color)
{
  Pixel curcol;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == color)
	XmChangeColor(w, ss->highlight_color);
    }
  /* to handle the gtk side correctly here, we'd need a list of widgets to modify --
   *    currently basic-color hits every background, so the whole thing is messed up.
   */
}
#endif


void set_highlight_color(color_t color)
{
#if USE_MOTIF
  color_t old_color;
  old_color = ss->highlight_color;
#endif
  ss->highlight_color = color; 
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->highlight_color_symbol, XEN_WRAP_PIXEL(color));
#endif
#if USE_MOTIF
  map_over_children_with_color(MAIN_SHELL(ss), highlight_recolor_everything, old_color);
#endif
}


static XEN g_set_highlight_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_highlight_color, "a color"); 
  set_highlight_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color "): color of highlighted text or buttons"
  return(XEN_WRAP_PIXEL(ss->highlight_color));
}


static XEN g_set_mark_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_mark_color, "a color"); 
  color_marks(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}


static XEN g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color "): mark color"
  return(XEN_WRAP_PIXEL(ss->mark_color));
}


void set_zoom_color(color_t color)
{
  ss->zoom_color = color; 
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->zoom_color_symbol, XEN_WRAP_PIXEL(color));
#endif
  color_chan_components(ss->zoom_color, COLOR_ZOOM);
}


static XEN g_set_zoom_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_zoom_color, "a color"); 
  set_zoom_color(XEN_UNWRAP_PIXEL(color)); 
  return(color);
}


static XEN g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color "): color of zoom sliders"
  return(XEN_WRAP_PIXEL(ss->zoom_color));
}


void set_position_color(color_t color)
{
  ss->position_color = color; 
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->position_color_symbol, XEN_WRAP_PIXEL(color));
#endif
  color_chan_components(ss->position_color, COLOR_POSITION);
}


static XEN g_set_position_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_position_color, "a color"); 
  set_position_color(XEN_UNWRAP_PIXEL(color)); 
  return(color);
}


static XEN g_position_color(void) 
{
  #define H_position_color "(" S_position_color "): color of position sliders"
  return(XEN_WRAP_PIXEL(ss->position_color));
}


static XEN g_set_listener_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_listener_color, "a color"); 
  color_listener(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color "): background color of the lisp listener"
  return(XEN_WRAP_PIXEL(ss->listener_color));
}


static XEN g_set_listener_text_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_listener_text_color, "a color"); 
  color_listener_text(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_listener_text_color(void) 
{
  #define H_listener_text_color "(" S_listener_text_color "): text color in the lisp listener"
  return(XEN_WRAP_PIXEL(ss->listener_text_color));
}


static XEN g_set_enved_waveform_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_enved_waveform_color, "a color"); 
  ss->enved_waveform_color = XEN_UNWRAP_PIXEL(color);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->enved_waveform_color_symbol, color);
#endif
  color_enved_waveform(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color "): color of the envelope editor wave display"
  return(XEN_WRAP_PIXEL(ss->enved_waveform_color));
}


static XEN g_set_filter_control_waveform_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_filter_control_waveform_color, "a color");
  ss->filter_control_waveform_color = XEN_UNWRAP_PIXEL(color);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->filter_control_waveform_color_symbol, color);
#endif
  color_filter_waveform(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_filter_control_waveform_color(void) 
{
  #define H_filter_control_waveform_color "(" S_filter_control_waveform_color "): color of the filter waveform"
  return(XEN_WRAP_PIXEL(ss->filter_control_waveform_color));
}


static XEN g_set_selection_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_selection_color, "a color"); 
  color_selection(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}


static XEN g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color "): selection color"
  return(XEN_WRAP_PIXEL(ss->selection_color));
}


static XEN g_set_text_focus_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_text_focus_color, "a color"); 
  ss->text_focus_color = XEN_UNWRAP_PIXEL(color);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->text_focus_color_symbol, color);
#endif
  return(color);
}


static XEN g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color "): color used to show a text field has focus"
  return(XEN_WRAP_PIXEL(ss->text_focus_color));
}


static XEN g_set_sash_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_sash_color, "a color"); 
  ss->sash_color = XEN_UNWRAP_PIXEL(color);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->sash_color_symbol, color);
#endif
  return(color);
}


static XEN g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color "): color used to draw paned window sashes"
  return(XEN_WRAP_PIXEL(ss->sash_color));
}


static XEN g_data_color(void) 
{
  #define H_data_color "(" S_data_color "): color used to draw unselected data"
  return(XEN_WRAP_PIXEL(ss->data_color));
}


void set_data_color(color_t color)
{
  ss->data_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->data_color_symbol, XEN_WRAP_PIXEL(color));
#endif
  color_data(color);
  ss->grid_color = get_in_between_color(ss->data_color, ss->graph_color);
  for_each_chan(update_graph);
}


static XEN g_set_data_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_data_color, "a color"); 
  set_data_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}


void set_selected_data_color(color_t color)
{
  chan_info *cp;
  ss->selected_data_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->selected_data_color_symbol, XEN_WRAP_PIXEL(color));
#endif
  color_selected_data(color);
  ss->selected_grid_color = get_in_between_color(ss->selected_data_color, ss->selected_graph_color);
  cp = selected_channel();
  if (cp) update_graph(cp);
}


static XEN g_set_selected_data_color(XEN color)
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_selected_data_color, "a color"); 
  set_selected_data_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color "): color used for selected data"
  return(XEN_WRAP_PIXEL(ss->selected_data_color));
}


void set_graph_color(color_t color)
{
  color_graph(color);
  ss->graph_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->graph_color_symbol, XEN_WRAP_PIXEL(color));
#endif
  color_unselected_graphs(color);
  ss->grid_color = get_in_between_color(ss->data_color, ss->graph_color);
}


static XEN g_set_graph_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_graph_color, "a color");
  set_graph_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color "): background color used for unselected data"
  return(XEN_WRAP_PIXEL(ss->graph_color));
}


void set_selected_graph_color(color_t color)
{
  chan_info *cp;
  ss->selected_graph_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->selected_graph_color_symbol, XEN_WRAP_PIXEL(color));
#endif
  color_selected_graph(color);
  ss->selected_grid_color = get_in_between_color(ss->selected_data_color, ss->selected_graph_color);
  cp = selected_channel();
  if (cp) 
    {
#if USE_MOTIF
      XtVaSetValues(channel_graph(cp), XmNbackground, ss->selected_graph_color, NULL);
#else
      widget_modify_bg(channel_graph(cp), GTK_STATE_NORMAL, ss->selected_graph_color);
#endif
    }
}


static XEN g_set_selected_graph_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_selected_graph_color, "a color");
  set_selected_graph_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color "): background color of selected data"
  return(XEN_WRAP_PIXEL(ss->selected_graph_color));
}


static XEN g_set_axis_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_axis_color, "a color");
  ss->axis_color = XEN_UNWRAP_PIXEL(color);
  ss->axis_color_set = true;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->axis_color_symbol, color);
#endif
  for_each_chan(update_graph);
  return(color);
}


static XEN g_axis_color(void) 
{
  #define H_axis_color "(" S_axis_color "): color of axis (defaults to current data color)"
  return(XEN_WRAP_PIXEL(ss->axis_color));
}


static XEN g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color "): Snd's basic color"
  return(XEN_WRAP_PIXEL(ss->basic_color));
}


#if USE_GTK

#if HAVE_GTK_3
static bool is_dark(color_info *color)
{
  return(color->red + color->green + color->blue < 0.75);
}
#endif

static void recolor_everything_1(widget_t w, gpointer color)
{
  if ((GTK_IS_WIDGET(w)) &&
      (w != ss->listener_pane))
    {
#if (!HAVE_GTK_3)
      gtk_widget_modify_bg(w, GTK_STATE_NORMAL, (GdkColor *)color);
      if (GTK_IS_CONTAINER(w))
	gtk_container_foreach(GTK_CONTAINER(w), recolor_everything_1, color);
#else
      gtk_widget_override_background_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)color);
      if (GTK_IS_LABEL(w))
	{
	  if (is_dark((color_info *)color))
	    gtk_widget_override_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)(ss->white));
	  else gtk_widget_override_color(w, GTK_STATE_FLAG_ACTIVE, (GdkRGBA *)(ss->black));
	}
	
      if (GTK_IS_CONTAINER(w))
	gtk_container_foreach(GTK_CONTAINER(w), recolor_everything_1, color);
#endif
    }
}


void recolor_everything(widget_t w, gpointer color)
{
#if (!HAVE_GTK_3)
  GdkColor *nc;
  nc = rgb_to_gdk_color((color_t)color);
#else
  GdkRGBA *nc;
  nc = (GdkRGBA *)color;
#endif
  recolor_everything_1(w, (gpointer)nc);
}


static XEN g_color_to_list(XEN obj)
{
  #define H_color_to_list "(" S_color_to_list " obj): 'obj' rgb values as a list of floats"
  color_info *v;
  XEN_ASSERT_TYPE(Xen_is_pixel(obj), obj, 1, S_color_to_list, "a color"); 
  v = XEN_UNWRAP_PIXEL(obj);
  if (v)
    return(XEN_LIST_4(C_TO_XEN_DOUBLE(RGB_TO_FLOAT(v->red)),
		      C_TO_XEN_DOUBLE(RGB_TO_FLOAT(v->green)),
		      C_TO_XEN_DOUBLE(RGB_TO_FLOAT(v->blue)),
		      C_TO_XEN_DOUBLE(v->alpha)));
  return(XEN_EMPTY_LIST);
}


static XEN g_make_color(XEN r, XEN g, XEN b, XEN alpha)
{
  #define H_make_color "(" S_make_color " r g b alpha): return a color object with the indicated rgb values"
  color_info *ccolor;
  mus_float_t rf, gf, bf;

  XEN_ASSERT_TYPE(Xen_is_number(r), r, 1, S_make_color, "a number");
  /* someday accept a list as r */
  XEN_ASSERT_TYPE(Xen_is_number(g), g, 2, S_make_color, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(b), b, 3, S_make_color, "a number");

  rf = check_color_range(S_make_color, r);
  gf = check_color_range(S_make_color, g);
  bf = check_color_range(S_make_color, b);
  ccolor = (color_info *)calloc(1, sizeof(color_info)); /* memleak here */
  ccolor->red = rf;
  ccolor->green = gf;
  ccolor->blue = bf;

  if (Xen_is_number(alpha))
    ccolor->alpha = check_color_range(S_make_color, alpha);
  else ccolor->alpha = 1.0;

  return(XEN_WRAP_PIXEL(ccolor));
}
#endif



#if USE_MOTIF
static void recolor_everything(widget_t w, color_t color)
{
  Pixel curcol;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == color)
	XmChangeColor(w, ss->basic_color);
    }
}

static XEN g_color_to_list(XEN obj)
{
  #define H_color_to_list "(" S_color_to_list " obj): 'obj' rgb values as a list of floats"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;

  XEN_ASSERT_TYPE(Xen_is_pixel(obj), obj, 1, S_color_to_list, "a color"); 

  dpy = XtDisplay(MAIN_SHELL(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = XEN_UNWRAP_PIXEL(obj);
  XQueryColor(dpy, cmap, &tmp_color);
  return(XEN_LIST_3(C_TO_XEN_DOUBLE(RGB_TO_FLOAT(tmp_color.red)),
		    C_TO_XEN_DOUBLE(RGB_TO_FLOAT(tmp_color.green)),
		    C_TO_XEN_DOUBLE(RGB_TO_FLOAT(tmp_color.blue))));
}


static XEN g_make_color(XEN r, XEN g, XEN b, XEN alpha)
{
  /* alpha is ignored in Motif */
  #define H_make_color "(" S_make_color " r g b alpha): return a color object with the indicated rgb values"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  mus_float_t rf, gf, bf;

  XEN_ASSERT_TYPE(Xen_is_number(r), r, 1, S_make_color, "a number");
  /* someday accept a list as r */
  XEN_ASSERT_TYPE(Xen_is_number(g), g, 2, S_make_color, "a number");
  XEN_ASSERT_TYPE(Xen_is_number(b), b, 3, S_make_color, "a number");

  rf = check_color_range(S_make_color, r);
  gf = check_color_range(S_make_color, g);
  bf = check_color_range(S_make_color, b);
  dpy = XtDisplay(MAIN_SHELL(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.red = FLOAT_TO_RGB(rf);
  tmp_color.green = FLOAT_TO_RGB(gf);
  tmp_color.blue = FLOAT_TO_RGB(bf);

  if ((XAllocColor(dpy, cmap, &tmp_color)) == 0)
    XEN_ERROR(XEN_ERROR_TYPE("no-such-color"),
	      XEN_LIST_4(C_TO_XEN_STRING(S_make_color ": can't allocate this color! (~A ~A ~A)"),
			 r, g, b));

  return(XEN_WRAP_PIXEL(tmp_color.pixel));
}
#endif


void set_basic_color(color_t color)
{
#if USE_MOTIF
  color_t old_color;
  old_color = ss->basic_color;
#endif
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->basic_color_symbol, XEN_WRAP_PIXEL(color));
#endif
  ss->basic_color = color; 
#if USE_MOTIF
  map_over_children_with_color(MAIN_SHELL(ss), recolor_everything, old_color);
#endif
#if USE_GTK
  recolor_everything(MAIN_SHELL(ss), color);
#endif

#if USE_MOTIF
  make_sound_icons_transparent_again(old_color, ss->basic_color);
  make_mixer_icons_transparent_again(old_color, ss->basic_color);
#endif
}


static XEN g_set_basic_color(XEN color) 
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_basic_color, "a color"); 
  set_basic_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}


static XEN g_mix_color(XEN mix_id) 
{
  #define H_mix_color "(" S_mix_color " :optional mix-id): color of all mix tags (if mix-id is omitted), or of mix-id's tag"
  if (xen_is_mix(mix_id))
    return(XEN_WRAP_PIXEL(mix_color_from_id(XEN_MIX_TO_C_INT(mix_id))));
  return(XEN_WRAP_PIXEL(ss->mix_color));
}


static XEN g_set_mix_color(XEN color, XEN mix_id)
{
  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_mix_color, "a color"); 
  XEN_ASSERT_TYPE(xen_is_mix(mix_id) || !Xen_is_bound(mix_id), mix_id, 2, S_setB S_mix_color, "a mix");
  if (xen_is_mix(mix_id))
    mix_set_color_from_id(XEN_MIX_TO_C_INT(mix_id), XEN_UNWRAP_PIXEL(color));
  else color_mixes(XEN_UNWRAP_PIXEL(color));
  return(color);
}

WITH_TWO_SETTER_ARGS(g_set_mix_color_reversed, g_set_mix_color)


bool foreground_color_ok(XEN color, graphics_context *ax)
{
  if (Xen_is_pixel(color))
    {
      set_foreground_color(ax, (color_t)XEN_UNWRAP_PIXEL(color));
      return(true);
    }
  return(false);
}



static XEN g_combined_data_color(XEN snd, XEN chn)
{
  #define H_combined_data_color "(" S_combined_data_color " snd chn): color of this channel's data if graphed with channels-combined"
  chan_info *cp;

  ASSERT_CHANNEL(S_combined_data_color, snd, chn, 1);
  cp = get_cp(snd, chn, S_combined_data_color);
  if (!cp) return(XEN_FALSE);

  return(XEN_WRAP_PIXEL(cp->combined_data_color));
}


static XEN g_set_combined_data_color(XEN color, XEN snd, XEN chn)
{
  chan_info *cp;

  XEN_ASSERT_TYPE(Xen_is_pixel(color), color, 1, S_setB S_combined_data_color, "a color"); 
  ASSERT_CHANNEL(S_combined_data_color, snd, chn, 1);
  cp = get_cp(snd, chn, S_combined_data_color);
  if (!cp) return(XEN_FALSE);

  cp->combined_data_color = XEN_UNWRAP_PIXEL(color);
  update_graph(cp);
  return(color);
}

WITH_THREE_SETTER_ARGS(g_set_combined_data_color_reversed, g_set_combined_data_color)



XEN_ARGIFY_8(g_draw_line_w, g_draw_line)
XEN_ARGIFY_7(g_draw_dot_w, g_draw_dot)
XEN_ARGIFY_5(g_draw_lines_w, g_draw_lines)
XEN_ARGIFY_6(g_draw_dots_w, g_draw_dots)
XEN_ARGIFY_7(g_draw_string_w, g_draw_string)
XEN_ARGIFY_9(g_fill_rectangle_w, g_fill_rectangle)
XEN_ARGIFY_5(g_fill_polygon_w, g_fill_polygon)
XEN_ARGIFY_3(g_foreground_color_w, g_foreground_color)
XEN_ARGIFY_3(g_current_font_w, g_current_font)
XEN_NARGIFY_0(g_main_widgets_w, g_main_widgets)
XEN_NARGIFY_0(g_dialog_widgets_w, g_dialog_widgets)
XEN_NARGIFY_1(g_widget_size_w, g_widget_size)
XEN_NARGIFY_2(g_set_widget_size_w, g_set_widget_size)
XEN_NARGIFY_1(g_widget_position_w, g_widget_position)
XEN_NARGIFY_2(g_set_widget_position_w, g_set_widget_position)
XEN_NARGIFY_1(g_widget_text_w, g_widget_text)
XEN_NARGIFY_2(g_set_widget_text_w, g_set_widget_text)
XEN_NARGIFY_1(g_hide_widget_w, g_hide_widget)
XEN_NARGIFY_1(g_show_widget_w, g_show_widget)
XEN_NARGIFY_1(g_focus_widget_w, g_focus_widget)
XEN_ARGIFY_5(g_make_graph_data_w, g_make_graph_data)
XEN_ARGIFY_8(g_graph_data_w, g_graph_data)
XEN_VARGIFY(g_make_bezier_w, g_make_bezier)
XEN_NARGIFY_0(g_snd_gcs_w, g_snd_gcs)
XEN_NARGIFY_1(g_snd_color_w, g_snd_color)
XEN_NARGIFY_1(g_snd_font_w, g_snd_font)
XEN_NARGIFY_1(g_make_cairo_w, g_make_cairo)
XEN_NARGIFY_1(g_free_cairo_w, g_free_cairo)

XEN_NARGIFY_0(g_selection_color_w, g_selection_color)
XEN_NARGIFY_1(g_set_selection_color_w, g_set_selection_color)
XEN_NARGIFY_0(g_zoom_color_w, g_zoom_color)
XEN_NARGIFY_1(g_set_zoom_color_w, g_set_zoom_color)
XEN_NARGIFY_0(g_position_color_w, g_position_color)
XEN_NARGIFY_1(g_set_position_color_w, g_set_position_color)
XEN_NARGIFY_0(g_mark_color_w, g_mark_color)
XEN_NARGIFY_1(g_set_mark_color_w, g_set_mark_color)
XEN_NARGIFY_0(g_listener_color_w, g_listener_color)
XEN_NARGIFY_1(g_set_listener_color_w, g_set_listener_color)
XEN_NARGIFY_0(g_listener_text_color_w, g_listener_text_color)
XEN_NARGIFY_1(g_set_listener_text_color_w, g_set_listener_text_color)
XEN_NARGIFY_0(g_enved_waveform_color_w, g_enved_waveform_color)
XEN_NARGIFY_1(g_set_enved_waveform_color_w, g_set_enved_waveform_color)
XEN_NARGIFY_0(g_filter_control_waveform_color_w, g_filter_control_waveform_color)
XEN_NARGIFY_1(g_set_filter_control_waveform_color_w, g_set_filter_control_waveform_color)
XEN_NARGIFY_0(g_highlight_color_w, g_highlight_color)
XEN_NARGIFY_1(g_set_highlight_color_w, g_set_highlight_color)
XEN_NARGIFY_0(g_cursor_color_w, g_cursor_color)
XEN_NARGIFY_1(g_set_cursor_color_w, g_set_cursor_color)
XEN_NARGIFY_0(g_text_focus_color_w, g_text_focus_color)
XEN_NARGIFY_1(g_set_text_focus_color_w, g_set_text_focus_color)
XEN_NARGIFY_0(g_sash_color_w, g_sash_color)
XEN_NARGIFY_1(g_set_sash_color_w, g_set_sash_color)
XEN_NARGIFY_0(g_data_color_w, g_data_color)
XEN_NARGIFY_1(g_set_data_color_w, g_set_data_color)
XEN_NARGIFY_0(g_graph_color_w, g_graph_color)
XEN_NARGIFY_1(g_set_graph_color_w, g_set_graph_color)
XEN_NARGIFY_0(g_selected_graph_color_w, g_selected_graph_color)
XEN_NARGIFY_1(g_set_selected_graph_color_w, g_set_selected_graph_color)
XEN_NARGIFY_0(g_selected_data_color_w, g_selected_data_color)
XEN_NARGIFY_1(g_set_selected_data_color_w, g_set_selected_data_color)
XEN_NARGIFY_0(g_axis_color_w, g_axis_color)
XEN_NARGIFY_1(g_set_axis_color_w, g_set_axis_color)
XEN_NARGIFY_0(g_basic_color_w, g_basic_color)
XEN_NARGIFY_1(g_set_basic_color_w, g_set_basic_color)
XEN_NARGIFY_1(g_color_p_w, g_color_p)
XEN_ARGIFY_4(g_make_color_w, g_make_color)
XEN_NARGIFY_1(g_color_to_list_w, g_color_to_list)
XEN_ARGIFY_1(g_mix_color_w, g_mix_color)
XEN_NARGIFY_2(g_combined_data_color_w, g_combined_data_color)

#if HAVE_SCHEME
#define g_set_current_font_w g_set_current_font_reversed
#define g_set_foreground_color_w g_set_foreground_color_reversed
#define g_set_mix_color_w g_set_mix_color_reversed
#define g_set_combined_data_color_w g_set_combined_data_color_reversed
#else
XEN_ARGIFY_4(g_set_current_font_w, g_set_current_font)
XEN_ARGIFY_4(g_set_foreground_color_w, g_set_foreground_color)
XEN_ARGIFY_2(g_set_mix_color_w, g_set_mix_color)
XEN_NARGIFY_3(g_set_combined_data_color_w, g_set_combined_data_color)
#endif

void g_init_draw(void)
{
  dialog_widgets = XEN_UNDEFINED;

  XEN_DEFINE_CONSTANT(S_copy_context,      CHAN_GC,    "graphics context to draw a line");
  XEN_DEFINE_CONSTANT(S_cursor_context,    CHAN_CGC,   "graphics context for the cursor");
  XEN_DEFINE_CONSTANT(S_selection_context, CHAN_SELGC, "graphics context to draw in the selection color");
  XEN_DEFINE_CONSTANT(S_mark_context,      CHAN_MGC,   "graphics context for a mark");

  XEN_DEFINE_SAFE_PROCEDURE(S_draw_line,        g_draw_line_w,      4, 4, 0, H_draw_line);
  XEN_DEFINE_SAFE_PROCEDURE(S_draw_dot,         g_draw_dot_w,       2, 5, 0, H_draw_dot);
  XEN_DEFINE_SAFE_PROCEDURE(S_draw_lines,       g_draw_lines_w,     1, 4, 0, H_draw_lines); 
  XEN_DEFINE_SAFE_PROCEDURE(S_draw_dots,        g_draw_dots_w,      1, 5, 0, H_draw_dots);
  XEN_DEFINE_SAFE_PROCEDURE(S_draw_string,      g_draw_string_w,    3, 4, 0, H_draw_string);
  XEN_DEFINE_SAFE_PROCEDURE(S_fill_rectangle,   g_fill_rectangle_w, 4, 5, 0, H_fill_rectangle);
  XEN_DEFINE_SAFE_PROCEDURE(S_fill_polygon,     g_fill_polygon_w,   1, 4, 0, H_fill_polygon);
  XEN_DEFINE_SAFE_PROCEDURE(S_main_widgets,     g_main_widgets_w,   0, 0, 0, H_main_widgets);
  XEN_DEFINE_SAFE_PROCEDURE(S_dialog_widgets,   g_dialog_widgets_w, 0, 0, 0, H_dialog_widgets);
  XEN_DEFINE_SAFE_PROCEDURE(S_hide_widget,      g_hide_widget_w,    1, 0, 0, H_hide_widget);
  XEN_DEFINE_SAFE_PROCEDURE(S_show_widget,      g_show_widget_w,    1, 0, 0, H_show_widget);
  XEN_DEFINE_SAFE_PROCEDURE(S_focus_widget,     g_focus_widget_w,   1, 0, 0, H_focus_widget);

  XEN_DEFINE_SAFE_PROCEDURE(S_make_graph_data,  g_make_graph_data_w, 0, 5, 0, H_make_graph_data);
  XEN_DEFINE_SAFE_PROCEDURE(S_graph_data,       g_graph_data_w,     1, 7, 0,  H_graph_data);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_foreground_color, g_foreground_color_w, H_foreground_color, S_setB S_foreground_color, g_set_foreground_color_w, 0, 3, 1, 3);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_current_font, g_current_font_w, H_current_font, S_setB S_current_font, g_set_current_font_w, 0, 3, 1, 3);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_size, g_widget_size_w, H_widget_size, S_setB S_widget_size, g_set_widget_size_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_position, g_widget_position_w, H_widget_position, S_setB S_widget_position, g_set_widget_position_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_text, g_widget_text_w, H_widget_text, S_setB S_widget_text, g_set_widget_text_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selection_color, g_selection_color_w, H_selection_color, S_setB S_selection_color, g_set_selection_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_zoom_color, g_zoom_color_w, H_zoom_color, S_setB S_zoom_color, g_set_zoom_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_position_color, g_position_color_w, H_position_color, S_setB S_position_color, g_set_position_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_color, g_mark_color_w, H_mark_color, S_setB S_mark_color, g_set_mark_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_color, g_listener_color_w, H_listener_color, S_setB S_listener_color, g_set_listener_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_text_color, g_listener_text_color_w, H_listener_text_color, S_setB S_listener_text_color, g_set_listener_text_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_waveform_color, g_enved_waveform_color_w, H_enved_waveform_color, S_setB S_enved_waveform_color, g_set_enved_waveform_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_filter_control_waveform_color, g_filter_control_waveform_color_w, H_filter_control_waveform_color, S_setB S_filter_control_waveform_color, g_set_filter_control_waveform_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_highlight_color, g_highlight_color_w, H_highlight_color, S_setB S_highlight_color, g_set_highlight_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_cursor_color, g_cursor_color_w, H_cursor_color, S_setB S_cursor_color, g_set_cursor_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_text_focus_color, g_text_focus_color_w, H_text_focus_color, S_setB S_text_focus_color, g_set_text_focus_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sash_color, g_sash_color_w, H_sash_color, S_setB S_sash_color, g_set_sash_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_color, g_data_color_w, H_data_color, S_setB S_data_color, g_set_data_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_color, g_graph_color_w, H_graph_color, S_setB S_graph_color, g_set_graph_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_graph_color, g_selected_graph_color_w, H_selected_graph_color, S_setB S_selected_graph_color, g_set_selected_graph_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_data_color, g_selected_data_color_w, H_selected_data_color, S_setB S_selected_data_color, g_set_selected_data_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_axis_color, g_axis_color_w, H_axis_color, S_setB S_axis_color, g_set_axis_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_basic_color, g_basic_color_w, H_basic_color, S_setB S_basic_color, g_set_basic_color_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mix_color, g_mix_color_w, H_mix_color, S_setB S_mix_color, g_set_mix_color_w, 0, 1, 1, 1);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_combined_data_color, g_combined_data_color_w, H_combined_data_color, S_setB S_combined_data_color, g_set_combined_data_color_w, 2, 0, 3, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_is_color,       g_color_p_w,        1, 0, 0, H_color_p);
  XEN_DEFINE_SAFE_PROCEDURE(S_make_color,    g_make_color_w,     3, 1, 0, H_make_color);
  XEN_DEFINE_SAFE_PROCEDURE(S_color_to_list, g_color_to_list_w,  1, 0, 0, H_color_to_list);

  XEN_DEFINE_PROCEDURE(S_make_bezier,        g_make_bezier_w, 0, 0, 1,     H_make_bezier);
  XEN_DEFINE_SAFE_PROCEDURE(S_snd_gcs,       g_snd_gcs_w,     0, 0, 0,     H_snd_gcs);
  XEN_DEFINE_SAFE_PROCEDURE(S_snd_color,     g_snd_color_w,   1, 0, 0,     H_snd_color);
  XEN_DEFINE_SAFE_PROCEDURE(S_snd_font,      g_snd_font_w,    1, 0, 0,     H_snd_font);

  XEN_DEFINE_SAFE_PROCEDURE(S_make_cairo,    g_make_cairo_w,  1, 0, 0,     H_make_cairo);
  XEN_DEFINE_SAFE_PROCEDURE(S_free_cairo,    g_free_cairo_w,  1, 0, 0,     H_free_cairo);

  #define H_new_widget_hook S_new_widget_hook " (widget): called each time a dialog or \
a new set of channel or sound widgets is created."

  new_widget_hook = XEN_DEFINE_HOOK(S_new_widget_hook, "(make-hook 'widget)", 1, H_new_widget_hook);
}

#else
/* no gui -- extension lang tie-ins are in snd-nogui.c */
void set_grf_points(int xi, int j, int ymin, int ymax) {}
void set_grf_point(int xi, int j, int yi) {}
void draw_grf_points(int dot_size, graphics_context *ax, int j, axis_info *ap, mus_float_t y0, graph_style_t graph_style) {}
void draw_both_grf_points(int dot_size, graphics_context *ax, int j, graph_style_t graph_style) {}
void draw_cursor(chan_info *cp) {}
point_t *get_grf_points(void) {return(NULL);} 
point_t *get_grf_points1(void) {return(NULL);}
bool foreground_color_ok(XEN color, graphics_context *ax) {return(true);}
#endif
