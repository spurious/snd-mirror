#include "snd.h"

/* TODO  in -separate mode (and elsewhere?) need to save description (sizes) of window/channels etc 
 *         but to make this work requires the load-side deferred resizing and reshaping
 *         and some way to access the sound-widget parent dialog, since otherwise widget-size is unsettable
 * TODO: similar split for make_fft_graph [needs sonogram etc??] -- complicated by background processes etc
 * TODO: in gtk widget position always 0 0, or initial if using gdk_get_window_geometry?
 */

#if (!USE_NO_GUI)

#include "vct.h"

static axis_context *get_ax(chan_info *cp, int ax_id, const char *caller)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    return(set_context(cp, ax_id));
  XEN_ERROR(NO_SUCH_AXIS_CONTEXT,
	XEN_LIST_4(C_TO_XEN_STRING(caller),
	       C_TO_SMALL_XEN_INT(cp->sound->index),
	       C_TO_SMALL_XEN_INT(cp->chan),
	       C_TO_SMALL_XEN_INT(ax_id)));
  return(NULL);
}

#define TO_C_AXIS_CONTEXT(Snd, Chn, Ax, Caller) \
  get_ax(get_cp(Snd, Chn, Caller), \
         XEN_TO_C_INT_OR_ELSE(Ax, CHAN_GC), \
         Caller)

axis_info *get_ap(chan_info *cp, int ap_id, const char *caller)
{
  if ((cp) && (AXIS_INFO_ID_OK(ap_id)))
    switch (ap_id)
      {
      case TIME_AXIS_INFO: 
	return(cp->axis); 
	break;
      case TRANSFORM_AXIS_INFO:  
	if (cp->fft) return(cp->fft->axis); 
	break;
      case LISP_AXIS_INFO: 
	if (cp->lisp_info) return(cp->lisp_info->axis); 
	break;
      }
  XEN_ERROR(NO_SUCH_AXIS_INFO,
	XEN_LIST_4(C_TO_XEN_STRING(caller),
	       C_TO_SMALL_XEN_INT(cp->sound->index),
	       C_TO_SMALL_XEN_INT(cp->chan),
	       C_TO_SMALL_XEN_INT(ap_id)));
  return(NULL);
}


static XEN g_draw_line(XEN x0, XEN y0, XEN x1, XEN y1, XEN snd, XEN chn, XEN ax)
{
  #define H_draw_line "(" S_draw_line " x0 y0 x1 y1 snd chn ax) draws a line"

  ASSERT_CHANNEL(S_draw_line, snd, chn, 5);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x0), x0, XEN_ARG_1, S_draw_line, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(y0), y0, XEN_ARG_2, S_draw_line, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x1), x1, XEN_ARG_3, S_draw_line, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(y1), y1, XEN_ARG_4, S_draw_line, "a number");
  draw_line(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_line),
	    XEN_TO_C_INT(x0),
	    XEN_TO_C_INT(y0),
	    XEN_TO_C_INT(x1),
	    XEN_TO_C_INT(y1));
  return(XEN_FALSE);
}

static XEN g_draw_dot(XEN x0, XEN y0, XEN size, XEN snd, XEN chn, XEN ax)
{
  #define H_draw_dot "(" S_draw_dot " x0 y0 size snd chn ax) draws a dot"
 
  ASSERT_CHANNEL(S_draw_dot, snd, chn, 4);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x0), x0, XEN_ARG_1, S_draw_dot, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(y0), y0, XEN_ARG_2, S_draw_dot, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_3, S_draw_dot, "a number");
  draw_arc(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dot),
	   XEN_TO_C_INT(x0),
	   XEN_TO_C_INT(y0),
	   XEN_TO_C_INT_OR_ELSE(size, 1));
  return(XEN_FALSE);
}

static XEN g_fill_rectangle(XEN x0, XEN y0, XEN width, XEN height, XEN snd, XEN chn, XEN ax)
{
  #define H_fill_rectangle "(" S_fill_rectangle " x0 y0 width height snd chn ax) draws a filled rectangle"

  ASSERT_CHANNEL(S_fill_rectangle, snd, chn, 5);
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x0), x0, XEN_ARG_1, S_fill_rectangle, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(y0), y0, XEN_ARG_2, S_fill_rectangle, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(width), width, XEN_ARG_3, S_fill_rectangle, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(height), height, XEN_ARG_4, S_fill_rectangle, "a number");
  fill_rectangle(TO_C_AXIS_CONTEXT(snd, chn, ax, S_fill_rectangle),
		 XEN_TO_C_INT(x0),
		 XEN_TO_C_INT(y0),
		 XEN_TO_C_INT(width),
		 XEN_TO_C_INT(height));
  return(XEN_FALSE);
}

static XEN g_draw_string(XEN text, XEN x0, XEN y0, XEN snd, XEN chn, XEN ax)
{
  #define H_draw_string "(" S_draw_string " text x0 y0 snd chn ax) draws a string"

  ASSERT_CHANNEL(S_draw_string, snd, chn, 4);
  XEN_ASSERT_TYPE(XEN_STRING_P(text), text, XEN_ARG_1, S_draw_string, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x0), x0, XEN_ARG_2, S_draw_string, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(y0), y0, XEN_ARG_3, S_draw_string, "a number");
  draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string),
	      XEN_TO_C_INT(x0),
	      XEN_TO_C_INT(y0),
	      XEN_TO_C_STRING(text),
	      snd_strlen(XEN_TO_C_STRING(text)));
  return(XEN_FALSE);
}

#if USE_MOTIF
  #define POINT XPoint
#else
  #define POINT GdkPoint
#endif

static POINT *TO_C_POINTS(XEN pts, const char *caller)
{
  int i, j, len;
  POINT *pack_pts;
  XEN *data;
  len = XEN_VECTOR_LENGTH(pts) / 2;
  if (len <= 0) 
    mus_misc_error(caller, "empty vector?", XEN_LIST_1(pts));
  data = XEN_VECTOR_ELEMENTS(pts);
  pack_pts = (POINT *)CALLOC(len, sizeof(POINT));
  for (i = 0, j = 0; i < len; i++, j += 2)
    {
      pack_pts[i].x = XEN_TO_C_INT_OR_ELSE(data[j], 0);
      pack_pts[i].y = XEN_TO_C_INT_OR_ELSE(data[j + 1], 0);
    }
  return(pack_pts);
}

static XEN g_draw_lines(XEN pts, XEN snd, XEN chn, XEN ax)
{
  /* pts should be a vector of integers as (x y) pairs */
  #define H_draw_lines "(" S_draw_lines " lines snd chn ax) draws a vector of lines"

  POINT *pack_pts;
  axis_context *ax1;
  ASSERT_CHANNEL(S_draw_lines, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_VECTOR_P(pts), pts, XEN_ARG_1, S_draw_lines, "a vector");
  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_lines);
  pack_pts = TO_C_POINTS(pts, S_draw_lines);
  draw_lines(ax1,
	     pack_pts, 
	     XEN_VECTOR_LENGTH(pts) / 2);
  FREE(pack_pts);
  return(pts);
}

static XEN g_draw_dots(XEN pts, XEN size, XEN snd, XEN chn, XEN ax)
{
  /* pts should be a vector of integers as (x y) pairs */
  #define H_draw_dots "(" S_draw_dots " positions dot-size snd chn ax) draws a vector of dots"
 
  POINT *pack_pts;
  axis_context *ax1;
  ASSERT_CHANNEL(S_draw_dots, snd, chn, 3);
  XEN_ASSERT_TYPE(XEN_VECTOR_P(pts), pts, XEN_ARG_1, S_draw_dots, "a vector");
  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dots);
  pack_pts = TO_C_POINTS(pts, S_draw_dots);
  draw_points(ax1,
	      pack_pts, 
	      XEN_VECTOR_LENGTH(pts) / 2,
	      XEN_TO_C_INT_OR_ELSE(size, 1));
  FREE(pack_pts);
  return(pts);
}

static XEN g_fill_polygon(XEN pts, XEN snd, XEN chn, XEN ax_id)
{ 
  #define H_fill_polygon "(" S_fill_polygon " points snd chn ax) draws a filled polygon"

  POINT *pack_pts;
  axis_context *ax;
  ASSERT_CHANNEL(S_fill_polygon, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_VECTOR_P(pts), pts, XEN_ARG_1, S_fill_polygon, "a vector");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_fill_polygon);
  pack_pts = TO_C_POINTS(pts, S_fill_polygon);
#if USE_MOTIF
  XFillPolygon(ax->dp, ax->wn, ax->gc, pack_pts, XEN_VECTOR_LENGTH(pts) / 2, Complex, CoordModeOrigin);
#else
  gdk_draw_polygon(ax->wn, ax->gc, TRUE, pack_pts, XEN_VECTOR_LENGTH(pts) / 2);
#endif
  FREE(pack_pts);
  return(pts);
}

static XEN g_make_bezier(XEN args)
{
  #define S_make_bezier "make-bezier"
  #define H_make_bezier "(" S_make_bezier " x0 y0 x1 y1 x2 y2 x3 y3 n) -> vector of points"
  /* XDrawBezier from cmn's glfed.c (where it was assuming PostScript coordinates) */
  int ax, ay, bx, by, cx, cy, i;
  float incr, val;
  int x[4];
  int y[4];
  int n = 50;
  XEN pts;
  XEN *data;
  for (i = 0; i < 4; i++)
    {
      x[i] = XEN_TO_C_INT(XEN_CAR(args));
      y[i] = XEN_TO_C_INT(XEN_CADR(args));
      args = XEN_CDDR(args);
    }
  if (XEN_NOT_NULL_P(args)) 
    n = XEN_TO_C_INT(XEN_CAR(args));
  cx = 3 * (x[1] - x[0]);
  cy = 3 * (y[1] - y[0]);
  bx = 3 * (x[2] - x[1]) - cx;
  by = 3 * (y[2] - y[1]) - cy;
  ax = x[3] - (x[0] + cx + bx);
  ay = y[3] - (y[0] + cy + by);
  incr = 1.0 / (float)n;
  pts = XEN_MAKE_VECTOR(2 * (n + 1), C_TO_SMALL_XEN_INT(0));
  data = XEN_VECTOR_ELEMENTS(pts);
  data[0] = C_TO_XEN_INT(x[0]);
  data[1] = C_TO_XEN_INT(y[0]);
  for (i = 1, val = incr; i <= n; i++, val += incr)
    {
      data[i * 2] = C_TO_XEN_INT((int)(x[0] + val * (cx + (val * (bx + (val * ax))))));
      data[i * 2 + 1] = C_TO_XEN_INT((int)(y[0] + val * (cy + (val * (by + (val * ay))))));
    }
  return(pts);
}


static XEN g_foreground_color(XEN snd, XEN chn, XEN ax)
{
  chan_info *cp;
  ASSERT_CHANNEL(S_foreground_color, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax), ax, XEN_ARG_3, S_foreground_color, "an integer");
  cp = get_cp(snd, chn, S_foreground_color);
  return(pixel2color(get_foreground_color(cp,
					  get_ax(cp, 
						 XEN_TO_C_INT_OR_ELSE(ax, CHAN_GC),
						 S_foreground_color))));
}

static XEN g_set_foreground_color(XEN color, XEN snd, XEN chn, XEN ax)
{
  #define H_foreground_color "(" S_foreground_color " snd chn ax) -> current drawing color"

  chan_info *cp;
  ASSERT_CHANNEL("set-" S_foreground_color, snd, chn, 2);
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ARG_1, "set-" S_foreground_color, "a color object");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax), ax, XEN_ARG_4, "set-" S_foreground_color, "an integer");
  cp = get_cp(snd, chn, "set-" S_foreground_color);
  set_foreground_color(cp,                                  /* snd-xchn.c */
		       get_ax(cp, 
			      XEN_TO_C_INT_OR_ELSE(ax, CHAN_GC),
			      "set-" S_foreground_color),
		       color2pixel(color));
  return(color);
}

static XEN g_set_foreground_color_reversed(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  if (XEN_NOT_BOUND_P(arg2))
    return(g_set_foreground_color(arg1, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
  else
    {
      if (XEN_NOT_BOUND_P(arg3))
	return(g_set_foreground_color(arg2, arg1, XEN_UNDEFINED, XEN_UNDEFINED));
      else
	{
	  if (XEN_NOT_BOUND_P(arg4))
	    return(g_set_foreground_color(arg3, arg1, arg2, XEN_UNDEFINED));
	  else return(g_set_foreground_color(arg4, arg1, arg2, arg3));
	}
    }
}

#if USE_MOTIF

static XEN g_load_font(XEN font)
{
  #define H_load_font "(" S_load_font " <name>) -> font-id"
  XFontStruct *fs = NULL;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_STRING_P(font), font, XEN_ONLY_ARG, S_load_font, "a string");
  ss = get_global_state();
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), 
		      XEN_TO_C_STRING(font));
  if (fs) return(C_TO_XEN_INT(fs->fid));
  return(XEN_FALSE);
}

static XEN g_set_current_font(XEN id, XEN snd, XEN chn, XEN ax_id)
{
  axis_context *ax;
  ASSERT_CHANNEL("set-" S_current_font, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax_id), ax_id, XEN_ARG_4, "set-" S_current_font, "an integer");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_current_font);
  ax->current_font = (Font)XEN_TO_C_INT(id);
  XSetFont(ax->dp, ax->gc, ax->current_font);
  return(id);
}

static XEN g_current_font(XEN snd, XEN chn, XEN ax_id)
{
  #define H_current_font "(" S_current_font " snd chn ax) -> current font id"
  axis_context *ax;
  chan_info *cp;
  ASSERT_CHANNEL(S_current_font, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax_id), ax_id, XEN_ARG_3, S_current_font, "an integer");
  cp = get_cp(snd, chn, S_current_font);
  ax = get_ax(cp,
	      XEN_TO_C_INT_OR_ELSE(ax_id, CHAN_GC),
	      S_current_font);
  if (ax->current_font == 0)
    return(C_TO_XEN_INT(cp->axis->ax->current_font));
  return(C_TO_XEN_INT(ax->current_font));
}


#else

static XEN g_load_font(XEN font)
{
  #define H_load_font "(" S_load_font " <name>) -> font-id"
  GdkFont *fs = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(font), font, XEN_ONLY_ARG, S_load_font, "a string");
  fs = gdk_font_load(XEN_TO_C_STRING(font));
  if (fs) return(XEN_WRAP_C_POINTER(fs));
  return(XEN_FALSE);
}

static XEN g_set_current_font(XEN id, XEN snd, XEN chn, XEN ax_id)
{
  axis_context *ax;
  ASSERT_CHANNEL("set-" S_current_font, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax_id), ax_id, XEN_ARG_4, "set-" S_current_font, "an integer");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, "set-" S_current_font);
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(id), id, XEN_ARG_1, "set-" S_current_font, "a wrapped object");
  gdk_gc_set_font(ax->gc, (GdkFont *)XEN_UNWRAP_C_POINTER(id));
  ax->current_font = (GdkFont *)XEN_UNWRAP_C_POINTER(id);
  return(id);
}

static XEN g_current_font(XEN snd, XEN chn, XEN ax_id)
{
  #define H_current_font "(" S_current_font " snd chn ax) -> current font id"
  axis_context *ax;
  ASSERT_CHANNEL(S_current_font, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax_id), ax_id, XEN_ARG_3, S_current_font, "an integer");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_current_font);
  return(XEN_WRAP_C_POINTER(ax->current_font));
}

#endif

static XEN g_set_current_font_reversed(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  if (XEN_NOT_BOUND_P(arg2))
    return(g_set_current_font(arg1, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
  else
    {
      if (XEN_NOT_BOUND_P(arg3))
	return(g_set_current_font(arg2, arg1, XEN_UNDEFINED, XEN_UNDEFINED));
      else
	{
	  if (XEN_NOT_BOUND_P(arg4))
	    return(g_set_current_font(arg3, arg1, arg2, XEN_UNDEFINED));
	  else return(g_set_current_font(arg4, arg1, arg2, arg3));
	}
    }
}

static XEN g_make_graph_data(XEN snd, XEN chn, XEN edpos, XEN lo, XEN hi)
{
  #define H_make_graph_data "(" S_make_graph_data " snd chn edit-pos low high)\n\
returns either a vct (if the graph has one trace), or a \
list of two vcts (the two sides of the envelope graph). \
'edit-position' defaults to the current-edit-position, \
'low' defaults to the current window left sample, and \
'high' defaults to the current rightmost sample. \
(graph-data (make-graph-data)) reimplements the time domain graph."

  chan_info *cp;
  ASSERT_CHANNEL(S_make_graph_data, snd, chn, 1);
  cp = get_cp(snd, chn, S_make_graph_data);
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(lo), lo, XEN_ARG_4, S_make_graph_data, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(hi), hi, XEN_ARG_5, S_make_graph_data, "a number");
  return(make_graph_data(cp,
			 to_c_edit_position(cp, edpos, S_make_graph_data, 3),
			 XEN_TO_C_INT_OR_ELSE(lo, -1),
			 XEN_TO_C_INT_OR_ELSE(hi, -1)));
}

static XEN g_graph_data(XEN data, XEN snd, XEN chn, XEN ax, XEN lo, XEN hi, XEN style)
{
  #define H_graph_data "(" S_graph_data " snd chn context low high graph-style)\n\
displays data in the time domain graph of snd's channel \
chn using the graphics context context (normally copy-context), placing the \
data in the recipient's graph between points low and high \
in the drawing mode graphic-style."

  chan_info *cp;
  vct *v0, *v1 = NULL;
  ASSERT_CHANNEL(S_graph_data, snd, chn, 2);
  cp = get_cp(snd, chn, S_graph_data);
  XEN_ASSERT_TYPE((XEN_LIST_P(data) && 
              (XEN_LIST_LENGTH(data) == 2) &&
              (VCT_P(XEN_CAR(data))) &&
              (VCT_P(XEN_CADR(data)))) || VCT_P(data), data, XEN_ARG_1, S_graph_data, "a list of 2 vcts or vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax), ax, XEN_ARG_4, S_graph_data, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(lo), lo, XEN_ARG_5, S_graph_data, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(hi), hi, XEN_ARG_6, S_graph_data, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(style), style, XEN_ARG_7, S_graph_data, "an integer");
  if (XEN_LIST_P(data))
    {
      v0 = get_vct(XEN_CAR(data));
      v1 = get_vct(XEN_CADR(data));
      if ((v0 == NULL) || (v1 == NULL))
        mus_misc_error(S_graph_data, "null vcts in list?", data);
    }
  else v0 = get_vct(data);
  draw_graph_data(cp, 
		  XEN_TO_C_INT_OR_ELSE(lo, -1),
		  XEN_TO_C_INT_OR_ELSE(hi, -1),
		  v0->length,
		  v0->data,
		  (v1) ? (v1->data) : NULL,
		  get_ax(cp, XEN_TO_C_INT_OR_ELSE(ax, CHAN_GC), S_graph_data),
		  XEN_TO_C_INT_OR_ELSE(style, TIME_GRAPH_STYLE(cp)));

  return(XEN_FALSE);
}

static XEN g_main_widgets(void)
{
  #define H_main_widgets "(" S_main_widgets ") -> top level widgets (list main-app main-shell main-pane sound-pane)"
  snd_state *ss;
  XEN main_win;
  ss = get_global_state();
#if USE_MOTIF
  main_win = XEN_WRAP_C_POINTER(MAIN_APP(ss));
#else
  main_win = XEN_WRAP_C_POINTER(MAIN_WINDOW(ss));
#endif
  return(XEN_CONS(main_win,
	   XEN_CONS(XEN_WRAP_C_POINTER(MAIN_SHELL(ss)),
             XEN_CONS(XEN_WRAP_C_POINTER(MAIN_PANE(ss)),
               XEN_CONS(XEN_WRAP_C_POINTER(SOUND_PANE(ss)),
		 XEN_EMPTY_LIST)))));
}

#define NUM_DIALOGS 22
static XEN dialog_widgets;

static XEN g_dialog_widgets(void)
{
  #define H_dialog_widgets "(" S_dialog_widgets ") -> dialogs (each #f if not yet created) (list \
color orientation enved error yes_or_no transform \
file_open file_save_as view_files raw_data new_file \
file_mix edit_header find help completion mix_panel \
print recorder region stats listener"

  if (!(XEN_VECTOR_P(dialog_widgets)))
    {
      dialog_widgets = XEN_MAKE_VECTOR(NUM_DIALOGS, XEN_FALSE);
      XEN_PROTECT_FROM_GC(dialog_widgets);
    }
  return(XEN_VECTOR_TO_LIST(dialog_widgets));
}

void set_dialog_widget(int which, GUI_WIDGET wid)
{
  if (!(XEN_VECTOR_P(dialog_widgets)))
    {
      dialog_widgets = XEN_MAKE_VECTOR(NUM_DIALOGS, XEN_FALSE);
      XEN_PROTECT_FROM_GC(dialog_widgets);
    }
  XEN_VECTOR_SET(dialog_widgets, 
		 which, 
		 XEN_WRAP_C_POINTER(wid));
}

static XEN g_widget_position(XEN wid)
{
  #define H_widget_position "(" S_widget_position " wid) -> '(x y)"
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ONLY_ARG, S_widget_position, "a wrapped object");  
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    return(XEN_LIST_2(C_TO_XEN_INT(widget_x(w)),
		  C_TO_XEN_INT(widget_y(w))));
  XEN_ERROR(NO_SUCH_WIDGET,
	XEN_LIST_2(C_TO_XEN_STRING(S_widget_position),
		  wid));
  return(XEN_EMPTY_LIST);
}

static XEN g_set_widget_position(XEN wid, XEN xy)
{
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ONLY_ARG, "set-" S_widget_position, "a wrapped object");  
  XEN_ASSERT_TYPE(XEN_LIST_P(xy), xy, XEN_ARG_2, "set-" S_widget_position, "a list");  
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    set_widget_position(w,
			XEN_TO_C_INT(XEN_CAR(xy)),
			XEN_TO_C_INT(XEN_CADR(xy)));
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_3(C_TO_XEN_STRING("set-" S_widget_position),
		    wid,
		    xy));
  return(wid);
}

static XEN g_widget_size(XEN wid)
{
  #define H_widget_size "(" S_widget_size " wid) -> '(width height)"
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ONLY_ARG, S_widget_size, "a wrapped object"); 
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    return(XEN_LIST_2(C_TO_XEN_INT(widget_width(w)),
		  C_TO_XEN_INT(widget_height(w))));
  XEN_ERROR(NO_SUCH_WIDGET,
	XEN_LIST_2(C_TO_XEN_STRING(S_widget_size),
		  wid));
  return(XEN_EMPTY_LIST);
}

static XEN g_set_widget_size(XEN wid, XEN wh)
{
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ARG_1, "set-" S_widget_size, "a wrapped object");  
  XEN_ASSERT_TYPE(XEN_LIST_P(wh), wh, XEN_ARG_2, "set-" S_widget_size, "a list");  
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    set_widget_size(w,
		    XEN_TO_C_INT(XEN_CAR(wh)),
		    XEN_TO_C_INT(XEN_CADR(wh)));
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_3(C_TO_XEN_STRING("set-" S_widget_size),
		    wid,
		    wh));
  return(wid);
}

static XEN g_widget_text(XEN wid)
{
  #define H_widget_text "(" S_widget_text " wid) -> text)"
  GUI_WIDGET w;
  char *text = NULL;
  XEN res = XEN_FALSE;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ARG_1, S_widget_text, "a wrapped object");
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    {
#if USE_MOTIF
      if ((XmIsText(w)) || (XmIsTextField(w)))
	{
	  text = XmTextGetString(w);
	  res = C_TO_XEN_STRING(text);
	  XtFree(text);
	}
      else
	{
	  XmString s1;
	  XtVaGetValues(w, XmNlabelString, &s1, NULL);
	  XmStringGetLtoR(s1, XmFONTLIST_DEFAULT_TAG, &text);
	  if (text == NULL)
	    {
	      XmStringGetLtoR(s1, "button_font", &text);
	      if (text == NULL)
		XmStringGetLtoR(s1, "bold_button_font", &text);
	    }
	  res = C_TO_XEN_STRING(text);
	}
      return(res);
#else
      if (GTK_IS_ENTRY(w))
	return(C_TO_XEN_STRING(gtk_entry_get_text(GTK_ENTRY(w))));
      else 
	{
	  gtk_label_get(GTK_LABEL(GTK_BIN(w)->child), &text);
	  return(C_TO_XEN_STRING(text));
	}
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_2(C_TO_XEN_STRING(S_widget_text),
		    wid));
  return(res);
}

static XEN g_set_widget_text(XEN wid, XEN text)
{
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ARG_1, "set-" S_widget_text, "a wrapped object");
  XEN_ASSERT_TYPE(XEN_STRING_P(text), text, XEN_ARG_2, "set-" S_widget_text, "a string");
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    {
#if USE_MOTIF
      if ((XmIsText(w)) || (XmIsTextField(w)))
	XmTextSetString(w, XEN_TO_C_STRING(text));
      else set_button_label_normal(w, XEN_TO_C_STRING(text));
#else
      if (GTK_IS_ENTRY(w))
	gtk_entry_set_text(GTK_ENTRY(w), XEN_TO_C_STRING(text));
      else set_button_label(w, XEN_TO_C_STRING(text));
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_3(C_TO_XEN_STRING("set-" S_widget_text),
		    wid,
		    text));
  return(text);
}

static XEN g_recolor_widget(XEN wid, XEN color)
{
  #define H_recolor_widget "(" S_recolor_widget " wid color) resets widget color"
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ARG_1, S_recolor_widget, "a wrapped object");  
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ARG_2, S_recolor_widget, "a color object"); 
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    {
#if USE_MOTIF
      XmChangeColor(w, color2pixel(color));
#else
      set_background(w, color2pixel(color));
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_3(C_TO_XEN_STRING(S_recolor_widget),
		    wid,
		    color));
  return(color);
}

static XEN g_set_widget_foreground(XEN wid, XEN color)
{
  #define H_set_widget_foreground "(set-widget-foreground widget color)"
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ARG_1, "set-widget-foreground", "a wrapped object");  
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ARG_2, "set-widget-foreground", "a color object"); 
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    {
#if USE_MOTIF
      XtVaSetValues(w, XmNforeground, color2pixel(color), NULL);
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_3(C_TO_XEN_STRING("set-widget-foreground"),
		    wid,
		    color));
  return(color);
}


static XEN g_hide_widget(XEN wid)
{
  #define H_hide_widget "(" S_hide_widget " widget) undisplays widget"
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ONLY_ARG, S_hide_widget, "a wrapped object");  
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    {
#if USE_MOTIF
      XtUnmanageChild(w);
#else
      gtk_widget_hide(w);
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_2(C_TO_XEN_STRING(S_hide_widget),
		    wid));
  return(wid);
}

static XEN g_show_widget(XEN wid)
{
  #define H_show_widget "(" S_show_widget " widget) displays widget"
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ONLY_ARG, S_show_widget, "a wrapped object");  
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    {
#if USE_MOTIF
      XtManageChild(w);
#else
      gtk_widget_show(w);
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_2(C_TO_XEN_STRING(S_show_widget),
		    wid));
  return(wid);
}

static XEN g_focus_widget(XEN wid)
{
  #define H_focus_widget "(" S_focus_widget " widget) causes widget to receive input ('focus')"
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ONLY_ARG, S_focus_widget, "a wrapped object");
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    goto_window(w);
  else XEN_ERROR(NO_SUCH_WIDGET,
	     XEN_LIST_2(C_TO_XEN_STRING(S_focus_widget),
		    wid));
  return(wid);
}

static XEN g_set_pixmap(XEN wid, XEN pix)
{
  GUI_WIDGET w;
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(wid), wid, XEN_ONLY_ARG, S_focus_widget, "a wrapped object");
  w = (GUI_WIDGET)(XEN_UNWRAP_C_POINTER(wid));
  if (w)
    {
#if USE_MOTIF
      if (XmIsLabel(w))
	XtVaSetValues(w, XmNlabelPixmap, (Pixmap)(XEN_UNWRAP_C_POINTER(pix)), NULL);
      else XtVaSetValues(w, XmNbackgroundPixmap, (Pixmap)(XEN_UNWRAP_C_POINTER(pix)), NULL);
#else
      /* the "pixmap" here is a list (pixmap mask) */
      gtk_pixmap_set(GTK_PIXMAP(w), (GdkPixmap *)(XEN_UNWRAP_C_POINTER(XEN_CAR(pix))), (GdkBitmap *)(XEN_UNWRAP_C_POINTER(XEN_CADR(pix))));
#endif
    }
  return(pix);
}

#if USE_MOTIF && HAVE_XPM
  #include <X11/xpm.h>
#endif

#if 0
typedef struct {
  GUI_PIXMAP pix; /* Pixmap or GdkPixmap* */
} snd_pixmap;

/* if we want to free pixmaps we need to make an smob for them.
 * but I don't immediately see anyway to free them!
 */
#endif

static XEN g_make_pixmap(XEN vals)
{
  #define H_make_pixmap "(make-pixmap strs)"
  snd_state *ss;
  char **bits;
  int rows, i;
  XEN row; XEN result;

  ss = get_global_state();
  rows = XEN_LIST_LENGTH(vals);
  bits = (char **)CALLOC(rows, sizeof(char *));
  for (i = 0, row = vals; i < rows; i++, row = XEN_CDR(row))
    bits[i] = XEN_TO_C_STRING(XEN_CAR(row));
#if USE_MOTIF
#if HAVE_XPM
  {
    Pixmap shape1, pix;
    XpmAttributes attributes; 
    XpmColorSymbol symbols[1];
    Widget anywid;
    int scr, pixerr;
    Display *dp;
    Drawable wn;
    anywid = MAIN_PANE(ss);
    dp = XtDisplay(anywid);
    wn = XtWindow(anywid);
    scr = DefaultScreen(dp);
    XtVaGetValues(anywid, XmNdepth, &attributes.depth, XmNcolormap, &attributes.colormap, NULL);
    attributes.visual = DefaultVisual(dp, scr);
    symbols[0].name = "basiccolor";
    symbols[0].value = NULL;
    symbols[0].pixel = (ss->sgx)->basic_color;
    attributes.colorsymbols = symbols;
    attributes.numsymbols = 1;
    attributes.valuemask = XpmColorSymbols | XpmDepth | XpmColormap | XpmVisual;
    pixerr = XpmCreatePixmapFromData(dp, wn, bits, &pix, &shape1, &attributes);
    result = XEN_WRAP_C_POINTER(pix);
  }
#endif
#else
  {
    GdkPixmap *pix;
    GdkBitmap *mask;
    pix = gdk_pixmap_create_from_xpm_d((GdkWindow *)(MAIN_WINDOW(ss)), &mask, NULL, bits);
    result = XEN_LIST_2(XEN_WRAP_C_POINTER(pix), XEN_WRAP_C_POINTER(mask));
  }
#endif
  FREE(bits);
  return(result);
}

static XEN g_colormap_ref(XEN map, XEN pos)
{
  #define H_colormap_ref "(colormap-ref map position) -> (list r g b)"
  unsigned short r, g, b;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(map), map, XEN_ARG_1, "colormap-ref", "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_2, "colormap-ref", "an integer");
  get_current_color(XEN_TO_C_INT(map), XEN_TO_C_INT(pos), &r, &g, &b);
  return(XEN_LIST_3(C_TO_XEN_DOUBLE((float)r / 65535.0),
		C_TO_XEN_DOUBLE((float)g / 65535.0),
		C_TO_XEN_DOUBLE((float)b / 65535.0)));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_7(g_draw_line_w, g_draw_line)
XEN_ARGIFY_6(g_draw_dot_w, g_draw_dot)
XEN_ARGIFY_4(g_draw_lines_w, g_draw_lines)
XEN_ARGIFY_5(g_draw_dots_w, g_draw_dots)
XEN_ARGIFY_6(g_draw_string_w, g_draw_string)
XEN_ARGIFY_7(g_fill_rectangle_w, g_fill_rectangle)
XEN_ARGIFY_4(g_fill_polygon_w, g_fill_polygon)
XEN_ARGIFY_3(g_foreground_color_w, g_foreground_color)
XEN_ARGIFY_4(g_set_foreground_color_w, g_set_foreground_color)
XEN_NARGIFY_1(g_load_font_w, g_load_font)
XEN_ARGIFY_3(g_current_font_w, g_current_font)
XEN_ARGIFY_4(g_set_current_font_w, g_set_current_font)
XEN_NARGIFY_0(g_main_widgets_w, g_main_widgets)
XEN_NARGIFY_0(g_dialog_widgets_w, g_dialog_widgets)
XEN_NARGIFY_1(g_widget_size_w, g_widget_size)
XEN_NARGIFY_2(g_set_widget_size_w, g_set_widget_size)
XEN_NARGIFY_1(g_widget_position_w, g_widget_position)
XEN_NARGIFY_2(g_set_widget_position_w, g_set_widget_position)
XEN_NARGIFY_1(g_widget_text_w, g_widget_text)
XEN_NARGIFY_2(g_set_widget_text_w, g_set_widget_text)
XEN_NARGIFY_2(g_recolor_widget_w, g_recolor_widget)
XEN_NARGIFY_1(g_hide_widget_w, g_hide_widget)
XEN_NARGIFY_1(g_show_widget_w, g_show_widget)
XEN_NARGIFY_1(g_focus_widget_w, g_focus_widget)
XEN_ARGIFY_5(g_make_graph_data_w, g_make_graph_data)
XEN_ARGIFY_7(g_graph_data_w, g_graph_data)
XEN_NARGIFY_2(g_set_widget_foreground_w, g_set_widget_foreground)
XEN_VARGIFY(g_make_bezier_w, g_make_bezier)
XEN_NARGIFY_1(g_make_pixmap_w, g_make_pixmap)
XEN_NARGIFY_2(g_set_pixmap_w, g_set_pixmap)
XEN_NARGIFY_2(g_colormap_ref_w, g_colormap_ref)
#else
#define g_draw_line_w g_draw_line
#define g_draw_dot_w g_draw_dot
#define g_draw_lines_w g_draw_lines
#define g_draw_dots_w g_draw_dots
#define g_draw_string_w g_draw_string
#define g_fill_rectangle_w g_fill_rectangle
#define g_fill_polygon_w g_fill_polygon
#define g_foreground_color_w g_foreground_color
#define g_set_foreground_color_w g_set_foreground_color
#define g_load_font_w g_load_font
#define g_current_font_w g_current_font
#define g_set_current_font_w g_set_current_font
#define g_main_widgets_w g_main_widgets
#define g_dialog_widgets_w g_dialog_widgets
#define g_widget_size_w g_widget_size
#define g_set_widget_size_w g_set_widget_size
#define g_widget_position_w g_widget_position
#define g_set_widget_position_w g_set_widget_position
#define g_widget_text_w g_widget_text
#define g_set_widget_text_w g_set_widget_text
#define g_recolor_widget_w g_recolor_widget
#define g_hide_widget_w g_hide_widget
#define g_show_widget_w g_show_widget
#define g_focus_widget_w g_focus_widget
#define g_make_graph_data_w g_make_graph_data
#define g_graph_data_w g_graph_data
#define g_set_widget_foreground_w g_set_widget_foreground
#define g_make_bezier_w g_make_bezier
#define g_make_pixmap_w g_make_pixmap
#define g_set_pixmap_w g_set_pixmap
#define g_colormap_ref_w g_colormap_ref
#endif

void g_init_draw(void)
{
  dialog_widgets = XEN_UNDEFINED;

  XEN_DEFINE_CONSTANT(S_copy_context,         CHAN_GC,        "graphics context to draw a line");
  XEN_DEFINE_CONSTANT(S_cursor_context,       CHAN_CGC,       "graphics context for the cursor");
  XEN_DEFINE_CONSTANT(S_selection_context,    CHAN_SELGC,     "graphics context to draw in the selection color");

  XEN_DEFINE_PROCEDURE(S_draw_line,        g_draw_line_w, 4, 3, 0,       H_draw_line);
  XEN_DEFINE_PROCEDURE(S_draw_dot,         g_draw_dot_w, 2, 4, 0,        H_draw_dot);
  XEN_DEFINE_PROCEDURE(S_draw_lines,       g_draw_lines_w, 1, 3, 0,      H_draw_lines); 
  XEN_DEFINE_PROCEDURE(S_draw_dots,        g_draw_dots_w, 1, 4, 0,       H_draw_dots);
  XEN_DEFINE_PROCEDURE(S_draw_string,      g_draw_string_w, 3, 3, 0,     H_draw_string);
  XEN_DEFINE_PROCEDURE(S_fill_rectangle,   g_fill_rectangle_w, 4, 3, 0,  H_fill_rectangle);
  XEN_DEFINE_PROCEDURE(S_fill_polygon,     g_fill_polygon_w, 1, 3, 0,    H_fill_polygon);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_foreground_color, g_foreground_color_w, H_foreground_color,
					"set-" S_foreground_color, g_set_foreground_color_w, g_set_foreground_color_reversed,
					0, 3, 1, 3);

  XEN_DEFINE_PROCEDURE(S_load_font,        g_load_font_w, 1, 0, 0,       H_load_font);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_current_font, g_current_font_w, H_current_font,
					"set-" S_current_font, g_set_current_font_w, g_set_current_font_reversed,
					0, 3, 1, 3);

  XEN_DEFINE_PROCEDURE(S_main_widgets,     g_main_widgets_w, 0, 0, 0,    H_main_widgets);
  XEN_DEFINE_PROCEDURE(S_dialog_widgets,   g_dialog_widgets_w, 0, 0, 0,  H_dialog_widgets);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_size, g_widget_size_w, H_widget_size,
					"set-" S_widget_size, g_set_widget_size_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_position, g_widget_position_w, H_widget_position,
					"set-" S_widget_position, g_set_widget_position_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_text, g_widget_text_w, H_widget_text,
					"set-" S_widget_text, g_set_widget_text_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_recolor_widget,  g_recolor_widget_w, 2, 0, 0,  H_recolor_widget);
  XEN_DEFINE_PROCEDURE(S_hide_widget,     g_hide_widget_w, 1, 0, 0,     H_hide_widget);
  XEN_DEFINE_PROCEDURE(S_show_widget,     g_show_widget_w, 1, 0, 0,     H_show_widget);
  XEN_DEFINE_PROCEDURE(S_focus_widget,    g_focus_widget_w, 1, 0, 0,    H_focus_widget);

  XEN_DEFINE_PROCEDURE(S_make_graph_data, g_make_graph_data_w, 0, 5, 0, H_make_graph_data);
  XEN_DEFINE_PROCEDURE(S_graph_data,      g_graph_data_w, 1, 6, 0,      H_graph_data);


  /* ---------------- unstable ---------------- */

#if DEBUGGING
  XEN_DEFINE_CONSTANT("erase-context",        CHAN_IGC,       "graphics context to erase a line");
  XEN_DEFINE_CONSTANT("mark-context",         CHAN_MGC,       "graphics context for a mark");
  XEN_DEFINE_CONSTANT("mix-context",          CHAN_GC,        "graphics context for mix waveforms");
  XEN_DEFINE_CONSTANT("selected-mix-context", CHAN_SELMXGC,   "graphics context for selected mix waveforms");
  XEN_DEFINE_CONSTANT("combined-context",     CHAN_TMPGC,     "graphics context for superimposed graphics");
#endif

  XEN_DEFINE_PROCEDURE("set-widget-foreground", g_set_widget_foreground_w, 2, 0, 0, H_set_widget_foreground);
  XEN_DEFINE_PROCEDURE(S_make_bezier,     g_make_bezier_w, 0, 0, 1,     H_make_bezier);

  XEN_DEFINE_PROCEDURE(S_make_pixmap, g_make_pixmap_w, 1, 0, 0, H_make_pixmap);
  XEN_DEFINE_PROCEDURE("set-pixmap", g_set_pixmap_w, 2, 0, 0, "(set-pixmap widget pixmap)");

  XEN_DEFINE_PROCEDURE("colormap-ref", g_colormap_ref_w, 2, 0, 0, H_colormap_ref);
  XEN_DEFINE_CONSTANT("colormap-size", COLORMAP_SIZE, "colormap size");
}
#endif
