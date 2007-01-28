#include "snd.h"

#if (!USE_NO_GUI)

#define AXIS_CONTEXT_ID_OK(Id) ((Id >= CHAN_GC) && (Id <= CHAN_TMPGC))
#define AXIS_INFO_ID_OK(Id)    (Id <= (int)LISP_AXIS_INFO)
#define NO_SUCH_WIDGET         XEN_ERROR_TYPE("no-such-widget")

static axis_context *get_ax(chan_info *cp, int ax_id, const char *caller)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    return(set_context(cp, (chan_gc_t)ax_id));
  XEN_ERROR(XEN_ERROR_TYPE("no-such-graphics-context"),
	    XEN_LIST_3(C_TO_XEN_STRING(caller),
		       C_TO_XEN_STRING("axis: ~A, sound index: ~A (~A), chan: ~A"),
		       XEN_LIST_4(C_TO_XEN_INT(ax_id),
				  C_TO_XEN_INT(cp->sound->index),
				  C_TO_XEN_STRING(cp->sound->short_filename),
				  C_TO_XEN_INT(cp->chan))));
  return(NULL);
}

#define TO_C_AXIS_CONTEXT(Snd, Chn, Ax, Caller) \
  get_ax(get_cp(Snd, Chn, Caller), \
         XEN_TO_C_INT_OR_ELSE(Ax, CHAN_GC), \
         Caller)

axis_info *get_ap(chan_info *cp, axis_info_t ap_id, const char *caller)
{
  if ((cp) && (AXIS_INFO_ID_OK(ap_id)))
    switch (ap_id)
      {
      case TIME_AXIS_INFO:      return(cp->axis);                              break;
      case TRANSFORM_AXIS_INFO: if (cp->fft) return(cp->fft->axis);            break;
      case LISP_AXIS_INFO:      if (cp->lisp_info) return(lisp_info_axis(cp)); break;
      }
  XEN_ERROR(XEN_ERROR_TYPE("no-such-axis"),
	    XEN_LIST_3(C_TO_XEN_STRING(caller),
		       ((!(cp->squelch_update)) || (!(AXIS_INFO_ID_OK(ap_id)))) ?
		       C_TO_XEN_STRING("axis: ~A of sound ~A (~A), chan: ~A (axis should be " S_time_graph ", " S_lisp_graph ", or " S_transform_graph ")") :
		       C_TO_XEN_STRING("axis: ~A of sound ~A (~A), chan: ~A does not exist, probably because output is squelched"),
		       XEN_LIST_4(C_TO_XEN_INT((int)(ap_id)),
				  C_TO_XEN_INT(cp->sound->index),
				  C_TO_XEN_STRING(cp->sound->short_filename),
				  C_TO_XEN_INT(cp->chan))));
  return(NULL);
}


static XEN g_draw_line(XEN x0, XEN y0, XEN x1, XEN y1, XEN snd, XEN chn, XEN ax)
{
  #define H_draw_line "(" S_draw_line " x0 y0 x1 y1 :optional snd chn (ax " S_time_graph ")): draw a line"

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
  #define H_draw_dot "(" S_draw_dot " x0 y0 size :optional snd chn (ax " S_time_graph ")): draw a dot"
 
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
  #define H_fill_rectangle "(" S_fill_rectangle " x0 y0 width height :optional snd chn (ax " S_time_graph ")): draw a filled rectangle"

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
  #define H_draw_string "(" S_draw_string " text x0 y0 :optional snd chn (ax " S_time_graph ")): draw a string"
  
  char *tmp = NULL;
  ASSERT_CHANNEL(S_draw_string, snd, chn, 4);
  XEN_ASSERT_TYPE(XEN_STRING_P(text), text, XEN_ARG_1, S_draw_string, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(x0), x0, XEN_ARG_2, S_draw_string, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(y0), y0, XEN_ARG_3, S_draw_string, "a number");
  tmp = XEN_TO_C_STRING(text);
#if USE_MOTIF
  /* snd-xdraw to make motif draw-string act in the same way (coordinate-wise) as gtk */
  /*   despite the name, this is not a gtk function */
  gtk_style_draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string),
#else
  draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string),
#endif
	      XEN_TO_C_INT(x0),
	      XEN_TO_C_INT(y0),
	      tmp,
	      snd_strlen(tmp));
  return(text);
}

#if USE_MOTIF
  #define point_t XPoint
#else
  #define point_t GdkPoint
#endif

static point_t *vector_to_points(XEN pts, const char *caller, int *vector_len)
{
  int i, j, vlen = 0, len = 0;
  point_t *pack_pts;
  vlen = XEN_VECTOR_LENGTH(pts);
  if (vlen & 1)
    XEN_ERROR(XEN_ERROR_TYPE("bad-length"),
	      XEN_LIST_3(C_TO_XEN_STRING(caller),
			 C_TO_XEN_STRING("length of vector of points (~A) must be even"),
			 C_TO_XEN_INT(vlen)));
  if (vlen <= 0) 
    XEN_ERROR(NO_DATA,
	      XEN_LIST_3(C_TO_XEN_STRING(caller), 
			 C_TO_XEN_STRING("empty vector?"), 
			 pts));
  len = vlen / 2;
  (*vector_len) = len;
  pack_pts = (point_t *)CALLOC(len, sizeof(point_t));
  for (i = 0, j = 0; i < len; i++, j += 2)
    {
      pack_pts[i].x = XEN_TO_C_INT_OR_ELSE(XEN_VECTOR_REF(pts, j), 0);
      pack_pts[i].y = XEN_TO_C_INT_OR_ELSE(XEN_VECTOR_REF(pts, j + 1), 0);
    }
  return(pack_pts);
}

static XEN g_draw_lines(XEN pts, XEN snd, XEN chn, XEN ax)
{
  /* pts should be a vector of integers as (x y) pairs */
  #define H_draw_lines "(" S_draw_lines " lines :optional snd chn (ax " S_time_graph ")): draw a vector of lines"

  point_t *pack_pts;
  axis_context *ax1;
  int vlen = 0;
  ASSERT_CHANNEL(S_draw_lines, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_VECTOR_P(pts), pts, XEN_ARG_1, S_draw_lines, "a vector");
  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_lines);
  pack_pts = vector_to_points(pts, S_draw_lines, &vlen);
  draw_lines(ax1, pack_pts, vlen);
  FREE(pack_pts);
  return(pts);
}

static XEN g_draw_dots(XEN pts, XEN size, XEN snd, XEN chn, XEN ax)
{
  /* pts should be a vector of integers as (x y) pairs */
  #define H_draw_dots "(" S_draw_dots " positions dot-size :optional snd chn (ax " S_time_graph ")): draw a vector of dots"
 
  point_t *pack_pts;
  axis_context *ax1;
  int vlen = 0;
  ASSERT_CHANNEL(S_draw_dots, snd, chn, 3);
  XEN_ASSERT_TYPE(XEN_VECTOR_P(pts), pts, XEN_ARG_1, S_draw_dots, "a vector");
  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dots);
  pack_pts = vector_to_points(pts, S_draw_dots, &vlen);
  draw_points(ax1,
	      pack_pts, 
	      vlen,
	      XEN_TO_C_INT_OR_ELSE(size, 1));
  FREE(pack_pts);
  return(pts);
}

static XEN g_fill_polygon(XEN pts, XEN snd, XEN chn, XEN ax_id)
{ 
  #define H_fill_polygon "(" S_fill_polygon " points :optional snd chn (ax " S_time_graph ")): draw a filled polygon"

  point_t *pack_pts;
  axis_context *ax;
  int vlen = 0;
  ASSERT_CHANNEL(S_fill_polygon, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_VECTOR_P(pts), pts, XEN_ARG_1, S_fill_polygon, "a vector");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_fill_polygon);
  pack_pts = vector_to_points(pts, S_fill_polygon, &vlen);
#if USE_MOTIF
  XFillPolygon(ax->dp, ax->wn, ax->gc, pack_pts, vlen, Complex, CoordModeOrigin);
#else
  gdk_draw_polygon(ax->wn, ax->gc, true, pack_pts, vlen);
#endif
  FREE(pack_pts);
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
  if (XEN_NOT_NULL_P(args)) 
    n = XEN_TO_C_INT(XEN_CAR(args));
  cx = 3 * (x[1] - x[0]);
  cy = 3 * (y[1] - y[0]);
  bx = 3 * (x[2] - x[1]) - cx;
  by = 3 * (y[2] - y[1]) - cy;
  ax = x[3] - (x[0] + cx + bx);
  ay = y[3] - (y[0] + cy + by);
  incr = 1.0 / (float)n;
  pts = XEN_MAKE_VECTOR(2 * (n + 1), C_TO_XEN_INT(0));
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
  axis_context *ax;
  ASSERT_CHANNEL(S_foreground_color, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(xax), xax, XEN_ARG_3, S_foreground_color, "an integer");
  cp = get_cp(snd, chn, S_foreground_color);
  if (!cp) return(XEN_FALSE);
  ax = get_ax(cp, XEN_TO_C_INT_OR_ELSE(xax, CHAN_GC), S_foreground_color);
  return(XEN_WRAP_PIXEL(get_foreground_color(ax)));
}

static XEN g_set_foreground_color(XEN color, XEN snd, XEN chn, XEN ax)
{
  chan_info *cp;
  ASSERT_CHANNEL(S_setB S_foreground_color, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ARG_1, S_setB S_foreground_color, "a color");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax), ax, XEN_ARG_4, S_setB S_foreground_color, "an integer");
  cp = get_cp(snd, chn, S_setB S_foreground_color);
  if (!cp) return(XEN_FALSE);
  set_foreground_color(get_ax(cp, 
			      XEN_TO_C_INT_OR_ELSE(ax, CHAN_GC),
			      S_setB S_foreground_color),
		       XEN_UNWRAP_PIXEL(color));
  return(color);
}

WITH_FOUR_SETTER_ARGS(g_set_foreground_color_reversed, g_set_foreground_color)


#if USE_MOTIF

static XEN g_set_current_font(XEN id, XEN snd, XEN chn, XEN ax_id)
{
  axis_context *ax;
  ASSERT_CHANNEL(S_setB S_current_font, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax_id), ax_id, XEN_ARG_4, S_setB S_current_font, "an integer");
  XEN_ASSERT_TYPE((XEN_LIST_P(id)) &&
		  (XEN_LIST_LENGTH(id) >= 2) &&
		  (XEN_SYMBOL_P(XEN_CAR(id))) &&
		  (strcmp("Font", XEN_SYMBOL_TO_C_STRING(XEN_CAR(id))) == 0), id, XEN_ARG_1, S_setB S_current_font, "a Font");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_current_font);
  ax->current_font = (Font)XEN_TO_C_INT(XEN_CADR(id));
  XSetFont(ax->dp, ax->gc, ax->current_font);
  return(id);
}

static XEN g_current_font(XEN snd, XEN chn, XEN ax_id)
{
  #define H_current_font "(" S_current_font " :optional snd chn (ax " S_time_graph ")): current font id"
  axis_context *ax;
  chan_info *cp;
  ASSERT_CHANNEL(S_current_font, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax_id), ax_id, XEN_ARG_3, S_current_font, "an integer");
  cp = get_cp(snd, chn, S_current_font);
  if (!cp) return(XEN_FALSE);
  ax = get_ax(cp,
	      XEN_TO_C_INT_OR_ELSE(ax_id, CHAN_GC),
	      S_current_font);
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
  axis_context *ax;
  ASSERT_CHANNEL(S_setB S_current_font, snd, chn, 2);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax_id), ax_id, XEN_ARG_4, S_setB S_current_font, "an integer");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_setB S_current_font);
  XEN_ASSERT_TYPE((XEN_ULONG_P(id)) ||
		  (XEN_LIST_P(id) && 
		   (XEN_LIST_LENGTH(id) >= 2) &&
		   (XEN_SYMBOL_P(XEN_CAR(id)))),
		  id, XEN_ARG_1, S_setB S_current_font, "a wrapped object or a raw pointer");
  if (XEN_ULONG_P(id))
    ax->current_font = (PangoFontDescription *)XEN_TO_C_ULONG(id); 
  else ax->current_font = (PangoFontDescription *)XEN_TO_C_ULONG(XEN_CADR(id));
  return(id);
}

static XEN g_current_font(XEN snd, XEN chn, XEN ax_id)
{
  #define H_current_font "(" S_current_font " :optional snd chn (ax " S_time_graph ")): current font id"
  axis_context *ax;
  ASSERT_CHANNEL(S_current_font, snd, chn, 1);
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(ax_id), ax_id, XEN_ARG_3, S_current_font, "an integer");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_current_font);
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
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(lo), lo, XEN_ARG_4, S_make_graph_data, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(hi), hi, XEN_ARG_5, S_make_graph_data, "a number");
  return(make_graph_data(cp,
			 to_c_edit_position(cp, edpos, S_make_graph_data, 3),
			 XEN_TO_C_OFF_T_OR_ELSE(lo, -1),
			 XEN_TO_C_OFF_T_OR_ELSE(hi, -1)));
}

static XEN g_graph_data(XEN data, XEN snd, XEN chn, XEN ax, XEN lo, XEN hi, XEN style)
{
  #define H_graph_data "(" S_graph_data " data :optional snd chn (context " S_copy_context ") low high graph-style): \
display 'data' in the time domain graph of snd's channel chn using the graphics context context (normally " S_copy_context "), placing the \
data in the recipient's graph between points low and high in the drawing mode graphic-style."

  chan_info *cp;
  vct *v0, *v1 = NULL;
  ASSERT_CHANNEL(S_graph_data, snd, chn, 2);
  cp = get_cp(snd, chn, S_graph_data);
  if (!cp) return(XEN_FALSE);
  if (XEN_FALSE_P(data)) return(XEN_FALSE);
  XEN_ASSERT_TYPE((XEN_LIST_P(data) && 
		   (XEN_LIST_LENGTH(data) == 2) &&
		   (MUS_VCT_P(XEN_CAR(data))) &&
		   (MUS_VCT_P(XEN_CADR(data)))) || 
		  MUS_VCT_P(data), 
		  data, XEN_ARG_1, S_graph_data, "a list of 2 vcts or vct");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(ax), ax, XEN_ARG_4, S_graph_data, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(lo) || XEN_FALSE_P(lo) || XEN_NOT_BOUND_P(lo), lo, XEN_ARG_5, S_graph_data, "a sample number");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(hi) || XEN_FALSE_P(hi) || XEN_NOT_BOUND_P(hi), hi, XEN_ARG_6, S_graph_data, "a sample number");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(style), style, XEN_ARG_7, S_graph_data, "an integer");
  if (XEN_LIST_P(data))
    {
      v0 = xen_to_vct(XEN_CAR(data));
      v1 = xen_to_vct(XEN_CADR(data));
    }
  else v0 = xen_to_vct(data);
  draw_graph_data(cp, 
		  XEN_TO_C_OFF_T_OR_ELSE(lo, -1),
		  XEN_TO_C_OFF_T_OR_ELSE(hi, -1),
		  v0->length,
		  v0->data,
		  (v1) ? (v1->data) : NULL,
		  get_ax(cp, XEN_TO_C_INT_OR_ELSE(ax, CHAN_GC), S_graph_data),
		  (graph_style_t)XEN_TO_C_INT_OR_ELSE(style, cp->time_graph_style));
  return(data);
}

static XEN g_main_widgets(void)
{
  #define H_main_widgets "(" S_main_widgets "): top level \
widgets (list (0)main-app (1)main-shell (2)main-pane (3)sound-pane (4)listener-pane (5)notebook-outer-pane)"

  XEN main_win;
#if USE_MOTIF
  main_win = XEN_WRAP_APPCONTEXT(MAIN_APP(ss));
#else
  main_win = XEN_WRAP_WINDOW(MAIN_WINDOW(ss));
#endif
  return(XEN_CONS(main_win,
	   XEN_CONS(XEN_WRAP_WIDGET(MAIN_SHELL(ss)),
             XEN_CONS(XEN_WRAP_WIDGET(MAIN_PANE(ss)),
               XEN_CONS(XEN_WRAP_WIDGET(SOUND_PANE(ss)),
		 XEN_CONS(XEN_WRAP_WIDGET(ss->sgx->listener_pane),
		   XEN_CONS(XEN_WRAP_WIDGET(SOUND_PANE_BOX(ss)),
		     XEN_EMPTY_LIST)))))));
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
  if (!(XEN_VECTOR_P(dialog_widgets)))
    {
      dialog_widgets = XEN_MAKE_VECTOR(NUM_DIALOGS, XEN_FALSE);
      XEN_PROTECT_FROM_GC(dialog_widgets);
    }
}

static XEN g_dialog_widgets(void)
{
  #define H_dialog_widgets "(" S_dialog_widgets "): dialog widgets (each " PROC_FALSE " if not yet created): (list \
 (0 " S_color_dialog ") (1 " S_orientation_dialog ") (2 " S_enved_dialog ") (3 " PROC_FALSE ") (4 " PROC_FALSE ") (5 " S_transform_dialog ") \
 (6 " S_open_file_dialog ") (7 " S_save_sound_dialog ") (8 " S_view_files_dialog ") (9 raw data dialog) (10 new file dialog) \
 (11 " S_mix_file_dialog ") (12 " S_edit_header_dialog ") (13 " S_find_dialog ") (14 " S_help_dialog ") (15 listener completion) \
 (16 " S_view_mixes_dialog ") (17 " S_print_dialog ") (18 " S_recorder_dialog ") (19 " S_view_regions_dialog ") \
 (20 " S_info_dialog ") (21 " S_view_tracks_dialog ") (22 " S_save_selection_dialog ") (23 " S_insert_file_dialog ") \
 (24 " S_save_region_dialog ") (25 " S_preferences_dialog "))"

  check_dialog_widget_table();
  return(XEN_VECTOR_TO_LIST(dialog_widgets));
}

void set_dialog_widget(snd_dialog_t which, widget_t wid)
{
  state_context *sx;
  sx = ss->sgx;
  if (sx->dialogs == NULL)
    {
      sx->dialogs_size = 8;
      sx->num_dialogs = 0;
      sx->dialogs = (widget_t *)CALLOC(sx->dialogs_size, sizeof(widget_t));
    }
  else
    {
      if (sx->num_dialogs == sx->dialogs_size)
	{
	  int i;
	  sx->dialogs_size += 8;
	  sx->dialogs = (widget_t *)REALLOC(sx->dialogs, sx->dialogs_size * sizeof(widget_t));
	  for (i = sx->num_dialogs; i < sx->dialogs_size; i++) sx->dialogs[i] = NULL;
	}
    }
  sx->dialogs[sx->num_dialogs++] = wid;
  check_dialog_widget_table();
  if (XEN_FALSE_P(XEN_VECTOR_REF(dialog_widgets, (int)which)))
    XEN_VECTOR_SET(dialog_widgets, (int)which, 
		   XEN_WRAP_WIDGET(wid));
  else 
    {
      if (XEN_WIDGET_P(XEN_VECTOR_REF(dialog_widgets, (int)which)))
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
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ONLY_ARG, S_widget_position, "a Widget");  
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (!w)
    XEN_ERROR(NO_SUCH_WIDGET,
	      XEN_LIST_2(C_TO_XEN_STRING(S_widget_position),
			 wid));
  return(XEN_LIST_2(C_TO_XEN_INT(widget_x(w)),
		    C_TO_XEN_INT(widget_y(w))));
}

static XEN g_set_widget_position(XEN wid, XEN xy)
{
  widget_t w;
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ONLY_ARG, S_setB S_widget_position, "a Widget");  
  XEN_ASSERT_TYPE(XEN_LIST_P(xy) && (XEN_LIST_LENGTH(xy) == 2), xy, XEN_ARG_2, S_setB S_widget_position, "a list: (x y)");  
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    set_widget_position(w,
			mus_iclamp(0, XEN_TO_C_INT(XEN_CAR(xy)), LOTSA_PIXELS),
			mus_iclamp(0, XEN_TO_C_INT(XEN_CADR(xy)), LOTSA_PIXELS));
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_3(C_TO_XEN_STRING(S_setB S_widget_position),
			    wid,
			    xy));
  return(wid);
}

static XEN g_widget_size(XEN wid)
{
  #define H_widget_size "(" S_widget_size " wid): widget's size, (list width height), in pixels"
  widget_t w;
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ONLY_ARG, S_widget_size, "a Widget"); 
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (!w)
    XEN_ERROR(NO_SUCH_WIDGET,
	      XEN_LIST_2(C_TO_XEN_STRING(S_widget_size),
			 wid));
  return(XEN_LIST_2(C_TO_XEN_INT(widget_width(w)),
		    C_TO_XEN_INT(widget_height(w))));
}

static XEN g_set_widget_size(XEN wid, XEN wh)
{
  widget_t w;
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ARG_1, S_setB S_widget_size, "a Widget");  
  XEN_ASSERT_TYPE(XEN_LIST_P(wh) && (XEN_LIST_LENGTH(wh) == 2), wh, XEN_ARG_2, S_setB S_widget_size, "a list: (width height)");  
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    set_widget_size(w,
		    mus_iclamp(1, XEN_TO_C_INT(XEN_CAR(wh)), LOTSA_PIXELS),
		    mus_iclamp(1, XEN_TO_C_INT(XEN_CADR(wh)), LOTSA_PIXELS));
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_3(C_TO_XEN_STRING(S_setB S_widget_size),
			    wid,
			    wh));
  return(wid);
}

static XEN g_widget_text(XEN wid)
{
  #define H_widget_text "(" S_widget_text " wid): widget's text or label"
  widget_t w;
  XEN res = XEN_FALSE;
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ONLY_ARG, S_widget_text, "a Widget");
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
	  if ((GTK_IS_BIN(w)) && (GTK_IS_LABEL(GTK_BIN(w)->child)))
	    return(C_TO_XEN_STRING((char *)gtk_label_get_text(GTK_LABEL(GTK_BIN(w)->child))));
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
		 XEN_LIST_2(C_TO_XEN_STRING(S_widget_text),
			    wid));
  return(res);
}

static XEN g_set_widget_text(XEN wid, XEN text)
{
  widget_t w;
  char *str = NULL;
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ARG_1, S_setB S_widget_text, "a Widget");
  XEN_ASSERT_TYPE(XEN_STRING_P(text) || XEN_FALSE_P(text), text, XEN_ARG_2, S_setB S_widget_text, "a string");
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    {
      if (XEN_STRING_P(text)) str = XEN_TO_C_STRING(text);
#if USE_MOTIF
      if ((XmIsText(w)) || (XmIsTextField(w)))
	XmTextSetString(w, str);
      else set_button_label(w, str);
#else
      if (GTK_IS_ENTRY(w))
	gtk_entry_set_text(GTK_ENTRY(w), str);
      else set_button_label(w, str);
#endif
    }
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_3(C_TO_XEN_STRING(S_setB S_widget_text),
			    wid,
			    text));
  return(text);
}

static XEN g_hide_widget(XEN wid)
{
  #define H_hide_widget "(" S_hide_widget " widget): hide or undisplay widget"
  widget_t w;
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ONLY_ARG, S_hide_widget, "a Widget");  
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
		 XEN_LIST_2(C_TO_XEN_STRING(S_hide_widget),
			    wid));
  return(wid);
}

static XEN g_show_widget(XEN wid)
{
  #define H_show_widget "(" S_show_widget " widget): show or display widget"
  widget_t w;
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ONLY_ARG, S_show_widget, "a Widget");  
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
		 XEN_LIST_2(C_TO_XEN_STRING(S_show_widget),
			    wid));
  return(wid);
}

static XEN g_focus_widget(XEN wid)
{
  #define H_focus_widget "(" S_focus_widget " widget): cause widget to receive input focus"
  widget_t w;
  XEN_ASSERT_TYPE(XEN_WIDGET_P(wid), wid, XEN_ONLY_ARG, S_focus_widget, "a Widget");
  w = (widget_t)(XEN_UNWRAP_WIDGET(wid));
  if (w)
    goto_window(w);
  else XEN_ERROR(NO_SUCH_WIDGET,
		 XEN_LIST_2(C_TO_XEN_STRING(S_focus_widget),
			    wid));
  return(wid);
}

static XEN g_snd_gcs(void)
{
  #define H_snd_gcs "(" S_snd_gcs "): a list of Snd graphics contexts (list (0 basic) (1 selected_basic) (2 combined) (3 \
cursor) (4 selected_cursor) (5 selection) (6 selected_selection) (7 erase) (8 selected_erase) (9 mark) (10 selected_mark) (11 mix) (12 \
fltenv_basic) (13 fltenv_data))"

  state_context *sx;
  sx = ss->sgx;
  if (sx)
    return(XEN_CONS(XEN_WRAP_GC(sx->basic_gc),
	    XEN_CONS(XEN_WRAP_GC(sx->selected_basic_gc), 
	     XEN_CONS(XEN_WRAP_GC(sx->combined_basic_gc), 
	      XEN_CONS(XEN_WRAP_GC(sx->cursor_gc), 
               XEN_CONS(XEN_WRAP_GC(sx->selected_cursor_gc), 
                XEN_CONS(XEN_WRAP_GC(sx->selection_gc), 
                 XEN_CONS(XEN_WRAP_GC(sx->selected_selection_gc), 
                  XEN_CONS(XEN_WRAP_GC(sx->erase_gc), 
                   XEN_CONS(XEN_WRAP_GC(sx->selected_erase_gc), 
                    XEN_CONS(XEN_WRAP_GC(sx->mark_gc), 
                     XEN_CONS(XEN_WRAP_GC(sx->selected_mark_gc), 
                      XEN_CONS(XEN_WRAP_GC(sx->mix_gc), 
                       XEN_CONS(XEN_WRAP_GC(sx->fltenv_basic_gc), 
                        XEN_CONS(XEN_WRAP_GC(sx->fltenv_data_gc), 
			 XEN_EMPTY_LIST)))))))))))))));
  return(XEN_EMPTY_LIST);
}

static XEN g_snd_color(XEN choice)
{
  #define H_snd_color "(" S_snd_color " num) -> color associated with 'num' -- see table of colors in snd-draw.c"
  color_t col;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(choice), choice, XEN_ONLY_ARG, S_snd_color, "an integer");

  switch (XEN_TO_C_INT(choice))
    {
    case 0: col = ss->sgx->white;                          break;
    case 1: col = ss->sgx->black;                          break;
    case 2: col = ss->sgx->red;                            break;
    case 3: col = ss->sgx->yellow;                         break;
    case 4: col = ss->sgx->green;                          break;
    case 5: col = ss->sgx->light_blue;                     break;
    case 6: col = ss->sgx->lighter_blue;                   break;

    case 7: col = ss->sgx->data_color;                     break;
    case 8: col = ss->sgx->selected_data_color;            break;
    case 9: col = ss->sgx->mark_color;                     break;
    case 10: col = ss->sgx->graph_color;                   break;
    case 11: col = ss->sgx->selected_graph_color;          break;
    case 12: col = ss->sgx->listener_color;                break;
    case 13: col = ss->sgx->listener_text_color;           break;

    case 14: col = ss->sgx->basic_color;                   break;
    case 15: col = ss->sgx->selection_color;               break;
    case 16: col = ss->sgx->zoom_color;                    break;
    case 17: col = ss->sgx->position_color;                break;
    case 18: col = ss->sgx->highlight_color;               break;
    case 19: col = ss->sgx->enved_waveform_color;          break;
    case 20: col = ss->sgx->cursor_color;                  break;

    case 21: col = ss->sgx->text_focus_color;              break;
    case 22: col = ss->sgx->filter_control_waveform_color; break;
    case 23: col = ss->sgx->mix_color;                     break;
    case 24: col = ss->sgx->pushed_button_color;           break;
    case 25: col = ss->sgx->sash_color;                    break;

    case 26: col = ss->sgx->help_button_color;             break;
    case 27: col = ss->sgx->doit_button_color;             break;
    case 28: col = ss->sgx->doit_again_button_color;       break;
    case 29: col = ss->sgx->quit_button_color;             break;
    case 30: col = ss->sgx->reset_button_color;            break;

    case 31: col = ss->sgx->grid_color;                    break;
    case 32: col = ss->sgx->selected_grid_color;           break;
    default: col = ss->sgx->black;                         break;
    }
  return(XEN_WRAP_PIXEL(col));
}

static XEN g_snd_font(XEN choice)
{
#if USE_MOTIF
  #define WRAP_FONT(Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Font"), C_TO_XEN_ULONG((unsigned long)Value))
#else
  #define WRAP_FONT(Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("PangoFontDescription_"), C_TO_XEN_ULONG((unsigned long)Value))
#endif

  #define H_snd_font "(" S_snd_font " num) -> font associated with 'num' -- see table of fonts in snd-draw.c"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(choice), choice, XEN_ONLY_ARG, S_snd_font, "an integer");

  switch (XEN_TO_C_INT(choice))
    {
#if USE_MOTIF
    case 0: return(WRAP_FONT(ss->sgx->peaks_fontstruct->fid));        break;
    case 1: return(WRAP_FONT(ss->sgx->bold_peaks_fontstruct->fid));   break;
    case 2: return(WRAP_FONT(ss->sgx->tiny_fontstruct->fid));         break;
    case 3: return(WRAP_FONT(ss->sgx->axis_label_fontstruct->fid));   break;
    case 4: return(WRAP_FONT(ss->sgx->axis_numbers_fontstruct->fid)); break;
    case 5: return(WRAP_FONT(ss->sgx->listener_fontstruct->fid));     break;
#endif
#if USE_GTK
    case 0: return(WRAP_FONT(ss->sgx->peaks_fnt));                    break;
    case 1: return(WRAP_FONT(ss->sgx->bold_peaks_fnt));               break;
    case 2: return(WRAP_FONT(ss->sgx->tiny_fnt));                     break;
    case 3: return(WRAP_FONT(ss->sgx->axis_label_fnt));               break;
    case 4: return(WRAP_FONT(ss->sgx->axis_numbers_fnt));             break;
    case 5: return(WRAP_FONT(ss->sgx->listener_fnt));                 break;
#endif
    default: return(XEN_FALSE);                                       break;
    }
  return(XEN_FALSE);
}


#if HAVE_GL
static Float gl_currents[6] = {DEFAULT_SPECTRO_X_ANGLE, DEFAULT_SPECTRO_Y_ANGLE, DEFAULT_SPECTRO_Z_ANGLE, 
			       DEFAULT_SPECTRO_X_SCALE, DEFAULT_SPECTRO_Y_SCALE, DEFAULT_SPECTRO_Z_SCALE};
static Float x_currents[6] = {90.0, 0.0, 358.0, 1.0, 1.0, 0.1};

void sgl_save_currents(void)
{
  Float *vals;
  if (with_gl(ss)) vals = gl_currents; else vals = x_currents;
  vals[0] = spectro_x_angle(ss);
  vals[1] = spectro_y_angle(ss);
  vals[2] = spectro_z_angle(ss);
  vals[3] = spectro_x_scale(ss);
  vals[4] = spectro_y_scale(ss);
  vals[5] = spectro_z_scale(ss);
}

void sgl_set_currents(void)
{
  Float *vals;
  if (with_gl(ss)) vals = gl_currents; else vals = x_currents;
  in_set_spectro_x_angle(vals[0]);
  in_set_spectro_y_angle(vals[1]);
  in_set_spectro_z_angle(vals[2]);
  in_set_spectro_x_scale(vals[3]);
  in_set_spectro_y_scale(vals[4]);
  in_set_spectro_z_scale(vals[5]);
  reflect_spectro();
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
  #define H_color_p "(" S_color_p " obj): " PROC_TRUE " if obj is a color"
  return(C_TO_XEN_BOOLEAN(XEN_PIXEL_P(obj)));
}

Float check_color_range(const char *caller, XEN val)
{
  Float rf;
  rf = XEN_TO_C_DOUBLE(val);
  if ((rf > 1.0) || (rf < 0.0))
    XEN_OUT_OF_RANGE_ERROR(caller, 1, val, "value ~A must be between 0.0 and 1.0");
  return(rf);
}

static XEN g_set_cursor_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_cursor_color, "a color"); 
  color_cursor(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}

static XEN g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color "): cursor color"
  return(XEN_WRAP_PIXEL(ss->sgx->cursor_color));
}

#if USE_MOTIF
static void highlight_recolor_everything(widget_t w, color_t color)
{
  Pixel curcol;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == color)
	XmChangeColor(w, ss->sgx->highlight_color);
    }
  /* to handle the gtk side correctly here, we'd need a list of widgets to modify --
   *    currently basic-color hits every background, so the whole thing is messed up.
   */
}
#endif

void set_highlight_color(color_t color)
{
  color_t old_color;
  old_color = ss->sgx->highlight_color;
  ss->sgx->highlight_color = color; 
#if USE_MOTIF
  map_over_children_with_color(MAIN_SHELL(ss), highlight_recolor_everything, old_color);
#endif
}

static XEN g_set_highlight_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_highlight_color, "a color"); 
  set_highlight_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color "): color of highlighted text or buttons"
  return(XEN_WRAP_PIXEL(ss->sgx->highlight_color));
}

static XEN g_set_mark_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_mark_color, "a color"); 
  color_marks(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}

static XEN g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color "): mark color"
  return(XEN_WRAP_PIXEL(ss->sgx->mark_color));
}

void set_zoom_color(color_t color)
{
  ss->sgx->zoom_color = color; 
  color_chan_components(ss->sgx->zoom_color, COLOR_ZOOM);
}

static XEN g_set_zoom_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_zoom_color, "a color"); 
  set_zoom_color(XEN_UNWRAP_PIXEL(color)); 
  return(color);
}

static XEN g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color "): color of zoom sliders"
  return(XEN_WRAP_PIXEL(ss->sgx->zoom_color));
}

void set_position_color(color_t color)
{
  ss->sgx->position_color = color; 
  color_chan_components(ss->sgx->position_color, COLOR_POSITION);
}

static XEN g_set_position_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_position_color, "a color"); 
  set_position_color(XEN_UNWRAP_PIXEL(color)); 
  return(color);
}

static XEN g_position_color(void) 
{
  #define H_position_color "(" S_position_color "): color of position sliders"
  return(XEN_WRAP_PIXEL(ss->sgx->position_color));
}

static XEN g_set_listener_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_listener_color, "a color"); 
  color_listener(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color "): background color of the lisp listener"
  return(XEN_WRAP_PIXEL(ss->sgx->listener_color));
}

static XEN g_set_listener_text_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_listener_text_color, "a color"); 
  color_listener_text(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_listener_text_color(void) 
{
  #define H_listener_text_color "(" S_listener_text_color "): text color in the lisp listener"
  return(XEN_WRAP_PIXEL(ss->sgx->listener_text_color));
}

static XEN g_set_enved_waveform_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_enved_waveform_color, "a color"); 
  color_enved_waveform(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color "): color of the envelope editor wave display"
  return(XEN_WRAP_PIXEL(ss->sgx->enved_waveform_color));
}

static XEN g_set_filter_control_waveform_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_filter_control_waveform_color, "a color");
  color_filter_waveform(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_filter_control_waveform_color(void) 
{
  #define H_filter_control_waveform_color "(" S_filter_control_waveform_color "): color of the filter waveform"
  return(XEN_WRAP_PIXEL(ss->sgx->filter_control_waveform_color));
}

static XEN g_set_selection_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selection_color, "a color"); 
  color_selection(XEN_UNWRAP_PIXEL(color));
  for_each_chan(update_graph);
  return(color);
}

static XEN g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color "): selection color"
  return(XEN_WRAP_PIXEL(ss->sgx->selection_color));
}

static XEN g_set_text_focus_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_text_focus_color, "a color"); 
  ss->sgx->text_focus_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color "): color used to show a text field has focus"
  return(XEN_WRAP_PIXEL(ss->sgx->text_focus_color));
}

static XEN g_set_sash_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_sash_color, "a color"); 
  ss->sgx->sash_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color "): color used to draw paned window sashes"
  return(XEN_WRAP_PIXEL(ss->sgx->sash_color));
}

static XEN g_set_help_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_help_button_color, "a color"); 
  ss->sgx->help_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_help_button_color(void) 
{
  #define H_help_button_color "(" S_help_button_color "): color used to draw help buttons"
  return(XEN_WRAP_PIXEL(ss->sgx->help_button_color));
}

static XEN g_set_quit_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_quit_button_color, "a color"); 
  ss->sgx->quit_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_quit_button_color(void) 
{
  #define H_quit_button_color "(" S_quit_button_color "): color used to draw quit (dismiss, cancel) buttons"
  return(XEN_WRAP_PIXEL(ss->sgx->quit_button_color));
}

static XEN g_set_doit_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_doit_button_color, "a color"); 
  ss->sgx->doit_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_doit_button_color(void) 
{
  #define H_doit_button_color "(" S_doit_button_color "): color used to draw doit (Ok, Apply) buttons"
  return(XEN_WRAP_PIXEL(ss->sgx->doit_button_color));
}

static XEN g_set_doit_again_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_doit_again_button_color, "a color"); 
  ss->sgx->doit_again_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_doit_again_button_color(void) 
{
  #define H_doit_again_button_color "(" S_doit_again_button_color "): color used to doit again (Undo&Apply) buttons"
  return(XEN_WRAP_PIXEL(ss->sgx->doit_again_button_color));
}

static XEN g_set_reset_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_reset_button_color, "a color"); 
  ss->sgx->reset_button_color = XEN_UNWRAP_PIXEL(color);
  return(color);
}

static XEN g_reset_button_color(void) 
{
  #define H_reset_button_color "(" S_reset_button_color "): color used to draw reset buttons"
  return(XEN_WRAP_PIXEL(ss->sgx->reset_button_color));
}


static XEN g_data_color(void) 
{
  #define H_data_color "(" S_data_color "): color used to draw unselected data"
  return(XEN_WRAP_PIXEL(ss->sgx->data_color));
}

void set_data_color(color_t color)
{
  color_data(color);
  ss->sgx->grid_color = get_in_between_color(ss->sgx->data_color, ss->sgx->graph_color);
  for_each_chan(update_graph);
}

static XEN g_set_data_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_data_color, "a color"); 
  set_data_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}

void set_selected_data_color(color_t color)
{
  chan_info *cp;
  color_selected_data(color);
  ss->sgx->selected_grid_color = get_in_between_color(ss->sgx->selected_data_color, ss->sgx->selected_graph_color);
  cp = selected_channel();
  if (cp) update_graph(cp);
}

static XEN g_set_selected_data_color(XEN color)
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selected_data_color, "a color"); 
  set_selected_data_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color "): color used for selected data"
  return(XEN_WRAP_PIXEL(ss->sgx->selected_data_color));
}

void set_graph_color(color_t color)
{
  color_graph(color);
  color_unselected_graphs(color);
  ss->sgx->grid_color = get_in_between_color(ss->sgx->data_color, ss->sgx->graph_color);
}

static XEN g_set_graph_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_graph_color, "a color");
  set_graph_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color "): background color used for unselected data"
  return(XEN_WRAP_PIXEL(ss->sgx->graph_color));
}

void set_selected_graph_color(color_t color)
{
  chan_info *cp;
  color_selected_graph(color);
  ss->sgx->selected_grid_color = get_in_between_color(ss->sgx->selected_data_color, ss->sgx->selected_graph_color);
  cp = selected_channel();
  if (cp) 
    {
#if USE_MOTIF
      XtVaSetValues(channel_graph(cp), XmNbackground, ss->sgx->selected_graph_color, NULL);
#else
      gtk_widget_modify_bg(channel_graph(cp), GTK_STATE_NORMAL, ss->sgx->selected_graph_color);
#endif
    }
}

static XEN g_set_selected_graph_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_selected_graph_color, "a color");
  set_selected_graph_color(XEN_UNWRAP_PIXEL(color));
  return(color);
}

static XEN g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color "): background color of selected data"
  return(XEN_WRAP_PIXEL(ss->sgx->selected_graph_color));
}

#if USE_MOTIF
static void recolor_button(widget_t w)
{
  if (XtIsWidget(w))
    {
      if (XmIsPushButton(w))
	XtVaSetValues(w, XmNarmColor, ss->sgx->pushed_button_color, NULL);
      else
	{
	  if (XmIsToggleButton(w))
	    XtVaSetValues(w, XmNselectColor, ss->sgx->pushed_button_color, NULL);
	}
    }
}
#endif

static XEN g_set_pushed_button_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_pushed_button_color, "a color"); 
  ss->sgx->pushed_button_color = XEN_UNWRAP_PIXEL(color);
#if USE_MOTIF
  map_over_children(MAIN_SHELL(ss), recolor_button);
#endif
  return(color);
}

static XEN g_pushed_button_color(void) 
{
  #define H_pushed_button_color "(" S_pushed_button_color "): color of a pushed button"
  return(XEN_WRAP_PIXEL(ss->sgx->pushed_button_color));
}

static XEN g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color "): Snd's basic color"
  return(XEN_WRAP_PIXEL(ss->sgx->basic_color));
}

#if USE_GTK
static void recolor_everything(widget_t w, gpointer color)
{
  if (GTK_IS_WIDGET(w)) 
    {
      /* apparently there is a huge memory leak here */
      gtk_widget_modify_bg(w, GTK_STATE_NORMAL, (color_t)color);
      if (GTK_IS_CONTAINER(w))
	gtk_container_foreach(GTK_CONTAINER(w), recolor_everything, color);
    }
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
	XmChangeColor(w, ss->sgx->basic_color);
    }
}
#endif

void set_basic_color(color_t color)
{
  color_t old_color;
  old_color = ss->sgx->basic_color;
  ss->sgx->basic_color = color; 
#if USE_MOTIF
  map_over_children_with_color(MAIN_SHELL(ss), recolor_everything, old_color);
#endif
#if USE_GTK
  gtk_container_foreach(GTK_CONTAINER(MAIN_SHELL(ss)), recolor_everything, (gpointer)(ss->sgx->basic_color));
#endif

#if HAVE_XPM && USE_MOTIF
  make_sound_icons_transparent_again(old_color, ss->sgx->basic_color);
  make_recorder_icons_transparent_again(ss->sgx->basic_color);
  make_mixer_icons_transparent_again(old_color, ss->sgx->basic_color);
#endif
}

static XEN g_set_basic_color(XEN color) 
{
  XEN_ASSERT_TYPE(XEN_PIXEL_P(color), color, XEN_ONLY_ARG, S_setB S_basic_color, "a color"); 
  set_basic_color(XEN_UNWRAP_PIXEL(color));
  return(color);
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
XEN_NARGIFY_1(g_hide_widget_w, g_hide_widget)
XEN_NARGIFY_1(g_show_widget_w, g_show_widget)
XEN_NARGIFY_1(g_focus_widget_w, g_focus_widget)
XEN_ARGIFY_5(g_make_graph_data_w, g_make_graph_data)
XEN_ARGIFY_7(g_graph_data_w, g_graph_data)
XEN_VARGIFY(g_make_bezier_w, g_make_bezier)
XEN_NARGIFY_0(g_snd_gcs_w, g_snd_gcs)
XEN_NARGIFY_1(g_snd_color_w, g_snd_color)
XEN_NARGIFY_1(g_snd_font_w, g_snd_font)

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
XEN_NARGIFY_0(g_help_button_color_w, g_help_button_color)
XEN_NARGIFY_1(g_set_help_button_color_w, g_set_help_button_color)
XEN_NARGIFY_0(g_reset_button_color_w, g_reset_button_color)
XEN_NARGIFY_1(g_set_reset_button_color_w, g_set_reset_button_color)
XEN_NARGIFY_0(g_quit_button_color_w, g_quit_button_color)
XEN_NARGIFY_1(g_set_quit_button_color_w, g_set_quit_button_color)
XEN_NARGIFY_0(g_doit_button_color_w, g_doit_button_color)
XEN_NARGIFY_1(g_set_doit_button_color_w, g_set_doit_button_color)
XEN_NARGIFY_0(g_doit_again_button_color_w, g_doit_again_button_color)
XEN_NARGIFY_1(g_set_doit_again_button_color_w, g_set_doit_again_button_color)
XEN_NARGIFY_0(g_data_color_w, g_data_color)
XEN_NARGIFY_1(g_set_data_color_w, g_set_data_color)
XEN_NARGIFY_0(g_graph_color_w, g_graph_color)
XEN_NARGIFY_1(g_set_graph_color_w, g_set_graph_color)
XEN_NARGIFY_0(g_selected_graph_color_w, g_selected_graph_color)
XEN_NARGIFY_1(g_set_selected_graph_color_w, g_set_selected_graph_color)
XEN_NARGIFY_0(g_selected_data_color_w, g_selected_data_color)
XEN_NARGIFY_1(g_set_selected_data_color_w, g_set_selected_data_color)
XEN_NARGIFY_0(g_basic_color_w, g_basic_color)
XEN_NARGIFY_1(g_set_basic_color_w, g_set_basic_color)
XEN_NARGIFY_0(g_pushed_button_color_w, g_pushed_button_color)
XEN_NARGIFY_1(g_set_pushed_button_color_w, g_set_pushed_button_color)
XEN_NARGIFY_1(g_color_p_w, g_color_p)

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
#define g_hide_widget_w g_hide_widget
#define g_show_widget_w g_show_widget
#define g_focus_widget_w g_focus_widget
#define g_make_graph_data_w g_make_graph_data
#define g_graph_data_w g_graph_data
#define g_make_bezier_w g_make_bezier
#define g_snd_gcs_w g_snd_gcs
#define g_snd_color_w g_snd_color
#define g_snd_font_w g_snd_font

#define g_selection_color_w g_selection_color
#define g_set_selection_color_w g_set_selection_color
#define g_zoom_color_w g_zoom_color
#define g_set_zoom_color_w g_set_zoom_color
#define g_position_color_w g_position_color
#define g_set_position_color_w g_set_position_color
#define g_mark_color_w g_mark_color
#define g_set_mark_color_w g_set_mark_color
#define g_listener_color_w g_listener_color
#define g_set_listener_color_w g_set_listener_color
#define g_listener_text_color_w g_listener_text_color
#define g_set_listener_text_color_w g_set_listener_text_color
#define g_enved_waveform_color_w g_enved_waveform_color
#define g_set_enved_waveform_color_w g_set_enved_waveform_color
#define g_filter_control_waveform_color_w g_filter_control_waveform_color
#define g_set_filter_control_waveform_color_w g_set_filter_control_waveform_color
#define g_highlight_color_w g_highlight_color
#define g_set_highlight_color_w g_set_highlight_color
#define g_cursor_color_w g_cursor_color
#define g_set_cursor_color_w g_set_cursor_color
#define g_text_focus_color_w g_text_focus_color
#define g_set_text_focus_color_w g_set_text_focus_color
#define g_sash_color_w g_sash_color
#define g_set_sash_color_w g_set_sash_color
#define g_help_button_color_w g_help_button_color
#define g_set_help_button_color_w g_set_help_button_color
#define g_doit_again_button_color_w g_doit_again_button_color
#define g_set_doit_again_button_color_w g_set_doit_again_button_color
#define g_doit_button_color_w g_doit_button_color
#define g_set_doit_button_color_w g_set_doit_button_color
#define g_quit_button_color_w g_quit_button_color
#define g_set_quit_button_color_w g_set_quit_button_color
#define g_reset_button_color_w g_reset_button_color
#define g_set_reset_button_color_w g_set_reset_button_color
#define g_data_color_w g_data_color
#define g_set_data_color_w g_set_data_color
#define g_graph_color_w g_graph_color
#define g_set_graph_color_w g_set_graph_color
#define g_selected_graph_color_w g_selected_graph_color
#define g_set_selected_graph_color_w g_set_selected_graph_color
#define g_selected_data_color_w g_selected_data_color
#define g_set_selected_data_color_w g_set_selected_data_color
#define g_basic_color_w g_basic_color
#define g_set_basic_color_w g_set_basic_color
#define g_pushed_button_color_w g_pushed_button_color
#define g_set_pushed_button_color_w g_set_pushed_button_color
#define g_color_p_w g_color_p

#endif

void g_init_draw(void)
{
  dialog_widgets = XEN_UNDEFINED;

  XEN_DEFINE_CONSTANT(S_copy_context,      CHAN_GC,    "graphics context to draw a line");
  XEN_DEFINE_CONSTANT(S_cursor_context,    CHAN_CGC,   "graphics context for the cursor");
  XEN_DEFINE_CONSTANT(S_selection_context, CHAN_SELGC, "graphics context to draw in the selection color");
  XEN_DEFINE_CONSTANT(S_mark_context,      CHAN_MGC,   "graphics context for a mark");

  XEN_DEFINE_PROCEDURE(S_draw_line,        g_draw_line_w,      4, 3, 0, H_draw_line);
  XEN_DEFINE_PROCEDURE(S_draw_dot,         g_draw_dot_w,       2, 4, 0, H_draw_dot);
  XEN_DEFINE_PROCEDURE(S_draw_lines,       g_draw_lines_w,     1, 3, 0, H_draw_lines); 
  XEN_DEFINE_PROCEDURE(S_draw_dots,        g_draw_dots_w,      1, 4, 0, H_draw_dots);
  XEN_DEFINE_PROCEDURE(S_draw_string,      g_draw_string_w,    3, 3, 0, H_draw_string);
  XEN_DEFINE_PROCEDURE(S_fill_rectangle,   g_fill_rectangle_w, 4, 3, 0, H_fill_rectangle);
  XEN_DEFINE_PROCEDURE(S_fill_polygon,     g_fill_polygon_w,   1, 3, 0, H_fill_polygon);
  XEN_DEFINE_PROCEDURE(S_main_widgets,     g_main_widgets_w,   0, 0, 0, H_main_widgets);
  XEN_DEFINE_PROCEDURE(S_dialog_widgets,   g_dialog_widgets_w, 0, 0, 0, H_dialog_widgets);
  XEN_DEFINE_PROCEDURE(S_hide_widget,      g_hide_widget_w,    1, 0, 0, H_hide_widget);
  XEN_DEFINE_PROCEDURE(S_show_widget,      g_show_widget_w,    1, 0, 0, H_show_widget);
  XEN_DEFINE_PROCEDURE(S_focus_widget,     g_focus_widget_w,   1, 0, 0, H_focus_widget);

  XEN_DEFINE_PROCEDURE(S_make_graph_data,  g_make_graph_data_w, 0, 5, 0, H_make_graph_data);
  XEN_DEFINE_PROCEDURE(S_graph_data,       g_graph_data_w,     1, 6, 0,  H_graph_data);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_foreground_color, g_foreground_color_w, H_foreground_color,
					    S_setB S_foreground_color, g_set_foreground_color_w, g_set_foreground_color_reversed, 0, 3, 1, 3);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_current_font, g_current_font_w, H_current_font,
					    S_setB S_current_font, g_set_current_font_w, g_set_current_font_reversed, 0, 3, 1, 3);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_size, g_widget_size_w, H_widget_size,
				   S_setB S_widget_size, g_set_widget_size_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_position, g_widget_position_w, H_widget_position,
				   S_setB S_widget_position, g_set_widget_position_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_widget_text, g_widget_text_w, H_widget_text,
				   S_setB S_widget_text, g_set_widget_text_w,  1, 0, 2, 0);


  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selection_color, g_selection_color_w, H_selection_color,
				   S_setB S_selection_color, g_set_selection_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_zoom_color, g_zoom_color_w, H_zoom_color,
				   S_setB S_zoom_color, g_set_zoom_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_position_color, g_position_color_w, H_position_color,
				   S_setB S_position_color, g_set_position_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_color, g_mark_color_w, H_mark_color,
				   S_setB S_mark_color, g_set_mark_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_color, g_listener_color_w, H_listener_color,
				   S_setB S_listener_color, g_set_listener_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_text_color, g_listener_text_color_w, H_listener_text_color,
				   S_setB S_listener_text_color, g_set_listener_text_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_waveform_color, g_enved_waveform_color_w, H_enved_waveform_color,
				   S_setB S_enved_waveform_color, g_set_enved_waveform_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_filter_control_waveform_color, g_filter_control_waveform_color_w, H_filter_control_waveform_color,
				   S_setB S_filter_control_waveform_color, g_set_filter_control_waveform_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_highlight_color, g_highlight_color_w, H_highlight_color,
				   S_setB S_highlight_color, g_set_highlight_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_cursor_color, g_cursor_color_w, H_cursor_color,
				   S_setB S_cursor_color, g_set_cursor_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_text_focus_color, g_text_focus_color_w, H_text_focus_color,
				   S_setB S_text_focus_color, g_set_text_focus_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sash_color, g_sash_color_w, H_sash_color,
				   S_setB S_sash_color, g_set_sash_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_help_button_color, g_help_button_color_w, H_help_button_color,
				   S_setB S_help_button_color, g_set_help_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_reset_button_color, g_reset_button_color_w, H_reset_button_color,
				   S_setB S_reset_button_color, g_set_reset_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_quit_button_color, g_quit_button_color_w, H_quit_button_color,
				   S_setB S_quit_button_color, g_set_quit_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_doit_button_color, g_doit_button_color_w, H_doit_button_color,
				   S_setB S_doit_button_color, g_set_doit_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_doit_again_button_color, g_doit_again_button_color_w, H_doit_again_button_color,
				   S_setB S_doit_again_button_color, g_set_doit_again_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_data_color, g_data_color_w, H_data_color,
				   S_setB S_data_color, g_set_data_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_color, g_graph_color_w, H_graph_color,
				   S_setB S_graph_color, g_set_graph_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_graph_color, g_selected_graph_color_w, H_selected_graph_color,
				   S_setB S_selected_graph_color, g_set_selected_graph_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selected_data_color, g_selected_data_color_w, H_selected_data_color,
				   S_setB S_selected_data_color, g_set_selected_data_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_basic_color, g_basic_color_w, H_basic_color,
				   S_setB S_basic_color, g_set_basic_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_pushed_button_color, g_pushed_button_color_w, H_pushed_button_color,
				   S_setB S_pushed_button_color, g_set_pushed_button_color_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_color_p, g_color_p_w, 1, 0, 0, H_color_p);


  /* ---------------- unstable ---------------- */

  XEN_DEFINE_PROCEDURE(S_make_bezier,     g_make_bezier_w, 0, 0, 1,     H_make_bezier);
  XEN_DEFINE_PROCEDURE(S_snd_gcs,         g_snd_gcs_w,     0, 0, 0,     H_snd_gcs);
  XEN_DEFINE_PROCEDURE(S_snd_color,       g_snd_color_w,   1, 0, 0,     H_snd_color);
  XEN_DEFINE_PROCEDURE(S_snd_font,        g_snd_font_w,    1, 0, 0,     H_snd_font);

  #define H_new_widget_hook S_new_widget_hook " (widget): called each time a dialog or \
a new set of channel or sound widgets is created."

  new_widget_hook = XEN_DEFINE_HOOK(S_new_widget_hook, 1, H_new_widget_hook);      /* arg = widget */
}
#endif
