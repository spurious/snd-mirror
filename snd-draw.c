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
  ERROR(NO_SUCH_AXIS_CONTEXT,
	LIST_4(TO_SCM_STRING(caller),
	       TO_SMALL_SCM_INT(cp->sound->index),
	       TO_SMALL_SCM_INT(cp->chan),
	       TO_SMALL_SCM_INT(ax_id)));
  return(NULL);
}

#define TO_C_AXIS_CONTEXT(Snd, Chn, Ax, Caller) \
  get_ax(get_cp(Snd, Chn, Caller), \
         TO_C_INT_OR_ELSE(Ax, CHAN_GC), \
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
  ERROR(NO_SUCH_AXIS_INFO,
	LIST_4(TO_SCM_STRING(caller),
	       TO_SMALL_SCM_INT(cp->sound->index),
	       TO_SMALL_SCM_INT(cp->chan),
	       TO_SMALL_SCM_INT(ap_id)));
  return(NULL);
}


static SCM g_draw_line(SCM x0, SCM y0, SCM x1, SCM y1, SCM snd, SCM chn, SCM ax)
{
  ASSERT_CHANNEL(S_draw_line, snd, chn, 5);
  ASSERT_TYPE(NUMBER_P(x0), x0, ARG1, S_draw_line, "a number");
  ASSERT_TYPE(NUMBER_P(y0), y0, ARG2, S_draw_line, "a number");
  ASSERT_TYPE(NUMBER_P(x1), x1, ARG3, S_draw_line, "a number");
  ASSERT_TYPE(NUMBER_P(y1), y1, ARG4, S_draw_line, "a number");
  draw_line(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_line),
	    TO_C_INT(x0),
	    TO_C_INT(y0),
	    TO_C_INT(x1),
	    TO_C_INT(y1));
  return(FALSE_VALUE);
}

static SCM g_draw_dot(SCM x0, SCM y0, SCM size, SCM snd, SCM chn, SCM ax)
{
  ASSERT_CHANNEL(S_draw_dot, snd, chn, 4);
  ASSERT_TYPE(NUMBER_P(x0), x0, ARG1, S_draw_dot, "a number");
  ASSERT_TYPE(NUMBER_P(y0), y0, ARG2, S_draw_dot, "a number");
  ASSERT_TYPE(NUMBER_P(size), size, ARG3, S_draw_dot, "a number");
  draw_arc(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dot),
	   TO_C_INT(x0),
	   TO_C_INT(y0),
	   TO_C_INT_OR_ELSE(size, 1));
  return(FALSE_VALUE);
}

static SCM g_fill_rectangle(SCM x0, SCM y0, SCM width, SCM height, SCM snd, SCM chn, SCM ax)
{
  ASSERT_CHANNEL(S_fill_rectangle, snd, chn, 5);
  ASSERT_TYPE(NUMBER_P(x0), x0, ARG1, S_fill_rectangle, "a number");
  ASSERT_TYPE(NUMBER_P(y0), y0, ARG2, S_fill_rectangle, "a number");
  ASSERT_TYPE(NUMBER_P(width), width, ARG3, S_fill_rectangle, "a number");
  ASSERT_TYPE(NUMBER_P(height), height, ARG4, S_fill_rectangle, "a number");
  fill_rectangle(TO_C_AXIS_CONTEXT(snd, chn, ax, S_fill_rectangle),
		 TO_C_INT(x0),
		 TO_C_INT(y0),
		 TO_C_INT(width),
		 TO_C_INT(height));
  return(FALSE_VALUE);
}

static SCM g_draw_string(SCM text, SCM x0, SCM y0, SCM snd, SCM chn, SCM ax)
{
  ASSERT_CHANNEL(S_draw_string, snd, chn, 4);
  ASSERT_TYPE(STRING_P(text), text, ARG1, S_draw_string, "a string");
  ASSERT_TYPE(NUMBER_P(x0), x0, ARG2, S_draw_string, "a number");
  ASSERT_TYPE(NUMBER_P(y0), y0, ARG3, S_draw_string, "a number");
  draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string),
	      TO_C_INT(x0),
	      TO_C_INT(y0),
	      TO_C_STRING(text),
	      snd_strlen(TO_C_STRING(text)));
  return(FALSE_VALUE);
}

#if USE_MOTIF
  #define POINT XPoint
#else
  #define POINT GdkPoint
#endif

static POINT *TO_C_POINTS(SCM pts, const char *caller)
{
  int i, j, len;
  POINT *pack_pts;
  SCM *data;
  len = VECTOR_LENGTH(pts) / 2;
  if (len <= 0) 
    mus_misc_error(caller, "empty vector?", LIST_1(pts));
  data = VECTOR_ELEMENTS(pts);
  pack_pts = (POINT *)CALLOC(len, sizeof(POINT));
  for (i = 0, j = 0; i < len; i++, j += 2)
    {
      pack_pts[i].x = TO_C_INT_OR_ELSE(data[j], 0);
      pack_pts[i].y = TO_C_INT_OR_ELSE(data[j + 1], 0);
    }
  return(pack_pts);
}

static SCM g_draw_lines(SCM pts, SCM snd, SCM chn, SCM ax)
{
  /* pts should be a vector of integers as (x y) pairs */
  POINT *pack_pts;
  axis_context *ax1;
  ASSERT_CHANNEL(S_draw_lines, snd, chn, 2);
  ASSERT_TYPE(VECTOR_P(pts), pts, ARG1, S_draw_lines, "a vector");
  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_lines);
  pack_pts = TO_C_POINTS(pts, S_draw_lines);
  draw_lines(ax1,
	     pack_pts, 
	     VECTOR_LENGTH(pts) / 2);
  FREE(pack_pts);
  return(pts);
}

static SCM g_draw_dots(SCM pts, SCM size, SCM snd, SCM chn, SCM ax)
{
  /* pts should be a vector of integers as (x y) pairs */
  POINT *pack_pts;
  axis_context *ax1;
  ASSERT_CHANNEL(S_draw_dots, snd, chn, 3);
  ASSERT_TYPE(VECTOR_P(pts), pts, ARG1, S_draw_dots, "a vector");
  ax1 = TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dots);
  pack_pts = TO_C_POINTS(pts, S_draw_dots);
  draw_points(ax1,
	      pack_pts, 
	      VECTOR_LENGTH(pts) / 2,
	      TO_C_INT_OR_ELSE(size, 1));
  FREE(pack_pts);
  return(pts);
}

static SCM g_fill_polygon(SCM pts, SCM snd, SCM chn, SCM ax_id)
{ 
  POINT *pack_pts;
  axis_context *ax;
  ASSERT_CHANNEL(S_fill_polygon, snd, chn, 2);
  ASSERT_TYPE(VECTOR_P(pts), pts, ARG1, S_fill_polygon, "a vector");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_fill_polygon);
  pack_pts = TO_C_POINTS(pts, S_fill_polygon);
#if USE_MOTIF
  XFillPolygon(ax->dp, ax->wn, ax->gc, pack_pts, VECTOR_LENGTH(pts) / 2, Complex, CoordModeOrigin);
#else
  gdk_draw_polygon(ax->wn, ax->gc, TRUE, pack_pts, VECTOR_LENGTH(pts) / 2);
#endif
  FREE(pack_pts);
  return(pts);
}

static SCM g_make_bezier(SCM args)
{
  #define S_make_bezier "make-bezier"
  /* XDrawBezier from cmn's glfed.c (where it was assuming PostScript coordinates) */
  int ax, ay, bx, by, cx, cy, i;
  float incr, val;
  int x[4];
  int y[4];
  int n = 50;
  SCM pts;
  SCM *data;
  for (i = 0; i < 4; i++)
    {
      x[i] = TO_C_INT(CAR(args));
      y[i] = TO_C_INT(CADR(args));
      args = CDDR(args);
    }
  if (NOT_NULL_P(args)) 
    n = TO_C_INT(CAR(args));
  cx = 3 * (x[1] - x[0]);
  cy = 3 * (y[1] - y[0]);
  bx = 3 * (x[2] - x[1]) - cx;
  by = 3 * (y[2] - y[1]) - cy;
  ax = x[3] - (x[0] + cx + bx);
  ay = y[3] - (y[0] + cy + by);
  incr = 1.0 / (float)n;
  pts = MAKE_VECTOR(2 * (n + 1), TO_SMALL_SCM_INT(0));
  data = VECTOR_ELEMENTS(pts);
  data[0] = TO_SCM_INT(x[0]);
  data[1] = TO_SCM_INT(y[0]);
  for (i = 1, val = incr; i <= n; i++, val += incr)
    {
      data[i * 2] = TO_SCM_INT((int)(x[0] + val * (cx + (val * (bx + (val * ax))))));
      data[i * 2 + 1] = TO_SCM_INT((int)(y[0] + val * (cy + (val * (by + (val * ay))))));
    }
  return(pts);
}


static SCM g_foreground_color(SCM snd, SCM chn, SCM ax)
{
  chan_info *cp;
  ASSERT_CHANNEL(S_foreground_color, snd, chn, 1);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(ax), ax, ARG3, S_foreground_color, "an integer");
  cp = get_cp(snd, chn, S_foreground_color);
  return(pixel2color(get_foreground_color(cp,
					  get_ax(cp, 
						 TO_C_INT_OR_ELSE(ax, CHAN_GC),
						 S_foreground_color))));
}

static SCM g_set_foreground_color(SCM color, SCM snd, SCM chn, SCM ax)
{
  chan_info *cp;
  ASSERT_CHANNEL("set-" S_foreground_color, snd, chn, 2);
  ASSERT_TYPE(COLOR_P(color), color, ARG1, "set-" S_foreground_color, "a color object");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(ax), ax, ARG4, "set-" S_foreground_color, "an integer");
  cp = get_cp(snd, chn, "set-" S_foreground_color);
  set_foreground_color(cp,                                  /* snd-xchn.c */
		       get_ax(cp, 
			      TO_C_INT_OR_ELSE(ax, CHAN_GC),
			      "set-" S_foreground_color),
		       color2pixel(color));
  return(color);
}

static SCM g_set_foreground_color_reversed(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  if (NOT_BOUND_P(arg2))
    return(g_set_foreground_color(arg1, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE));
  else
    {
      if (NOT_BOUND_P(arg3))
	return(g_set_foreground_color(arg2, arg1, UNDEFINED_VALUE, UNDEFINED_VALUE));
      else
	{
	  if (NOT_BOUND_P(arg4))
	    return(g_set_foreground_color(arg3, arg1, arg2, UNDEFINED_VALUE));
	  else return(g_set_foreground_color(arg4, arg1, arg2, arg3));
	}
    }
}

#if USE_MOTIF
  #define INPUT_TYPE XtInputId
#else
  #define INPUT_TYPE gint
#endif
/* these should be in snd-xscm and snd-gscm once tested etc */

static int num_inputs = 0;
static SCM *added_input_callbacks = NULL;
static INPUT_TYPE *added_inputs = NULL;

static int get_callback_slot(void)
{
  int i, old_len;
  if (num_inputs == 0)
    {
      num_inputs = 4;
      added_input_callbacks = (SCM *)CALLOC(num_inputs, sizeof(SCM));
      added_inputs = (INPUT_TYPE *)CALLOC(num_inputs, sizeof(INPUT_TYPE));
      for (i = 0; i < num_inputs; i++) added_input_callbacks[i] = UNDEFINED_VALUE;
      return(0);
    }
  for (i = 0; i < num_inputs; i++)
    if (NOT_BOUND_P(added_input_callbacks[i]))
      return(i);
  old_len = num_inputs;
  num_inputs += 4;
  added_input_callbacks = (SCM *)REALLOC(added_input_callbacks, num_inputs * sizeof(SCM));
  added_inputs = (INPUT_TYPE *)REALLOC(added_inputs, num_inputs * sizeof(INPUT_TYPE));
  for (i = old_len; i < num_inputs; i++)
    added_input_callbacks[i] = UNDEFINED_VALUE;
  return(old_len);
}

#if USE_MOTIF

#define ADD_INPUT(File, Callback) \
  XtAppAddInput(MAIN_APP(ss), File, (XtPointer)XtInputReadMask, handle_input, (XtPointer)Callback)

#define REMOVE_INPUT(Id) XtRemoveInput((XtInputId)Id)

static void handle_input(XtPointer context, int *fd, XtInputId *id)
{
  int input_index = (int)context;  /* TODO: is this legal on the Alpha?  if not, can we use long here? */
  CALL_1(added_input_callbacks[input_index],
	TO_SCM_INT(*fd),
	"input callback");
}

static SCM g_load_font(SCM font)
{
  XFontStruct *fs = NULL;
  snd_state *ss;
  ASSERT_TYPE(STRING_P(font), font, ARGn, S_load_font, "a string");
  ss = get_global_state();
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), 
		      TO_C_STRING(font));
  if (fs) return(TO_SCM_INT(fs->fid));
  return(FALSE_VALUE);
}

static SCM g_set_current_font(SCM id, SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  ASSERT_CHANNEL("set-" S_current_font, snd, chn, 2);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(ax_id), ax_id, ARG4, "set-" S_current_font, "an integer");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_current_font);
  ax->current_font = (Font)TO_C_INT(id);
  XSetFont(ax->dp, ax->gc, ax->current_font);
  return(id);
}

static SCM g_current_font(SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  chan_info *cp;
  ASSERT_CHANNEL(S_current_font, snd, chn, 1);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(ax_id), ax_id, ARG3, S_current_font, "an integer");
  cp = get_cp(snd, chn, S_current_font);
  ax = get_ax(cp,
	      TO_C_INT_OR_ELSE(ax_id, CHAN_GC),
	      S_current_font);
  if (ax->current_font == 0)
    return(TO_SCM_INT(cp->axis->ax->current_font));
  return(TO_SCM_INT(ax->current_font));
}


#else

#define ADD_INPUT(File, Callback) \
  gdk_input_add(File, GDK_INPUT_READ, handle_input, (gpointer)Callback)

#define REMOVE_INPUT(Id) gdk_input_remove((gint)Id)

static void handle_input(gpointer context, gint fd, GdkInputCondition condition)
{
  int input_index = (int)context;
  CALL_1(added_input_callbacks[input_index],
	TO_SCM_INT(fd),
	"input callback");
}

static SCM g_load_font(SCM font)
{
  GdkFont *fs = NULL;
  ASSERT_TYPE(STRING_P(font), font, ARGn, S_load_font, "a string");
  fs = gdk_font_load(TO_C_STRING(font));
  if (fs) return(SND_WRAP(fs));
  return(FALSE_VALUE);
}

static SCM g_set_current_font(SCM id, SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  ASSERT_CHANNEL("set-" S_current_font, snd, chn, 2);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(ax_id), ax_id, ARG4, "set-" S_current_font, "an integer");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, "set-" S_current_font);
  ASSERT_TYPE(SND_WRAPPED(id), id, ARG1, "set-" S_current_font, "a wrapped object");
  gdk_gc_set_font(ax->gc, (GdkFont *)SND_UNWRAP(id));
  ax->current_font = (GdkFont *)SND_UNWRAP(id);
  return(id);
}

static SCM g_current_font(SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  ASSERT_CHANNEL(S_current_font, snd, chn, 1);
  ASSERT_TYPE(INTEGER_IF_BOUND_P(ax_id), ax_id, ARG3, S_current_font, "an integer");
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_current_font);
  return(SND_WRAP(ax->current_font));
}

#endif

static SCM g_set_current_font_reversed(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  if (NOT_BOUND_P(arg2))
    return(g_set_current_font(arg1, UNDEFINED_VALUE, UNDEFINED_VALUE, UNDEFINED_VALUE));
  else
    {
      if (NOT_BOUND_P(arg3))
	return(g_set_current_font(arg2, arg1, UNDEFINED_VALUE, UNDEFINED_VALUE));
      else
	{
	  if (NOT_BOUND_P(arg4))
	    return(g_set_current_font(arg3, arg1, arg2, UNDEFINED_VALUE));
	  else return(g_set_current_font(arg4, arg1, arg2, arg3));
	}
    }
}

static SCM g_add_input(SCM file, SCM callback)
{
  snd_state *ss;
  int loc;
  ASSERT_TYPE(INTEGER_P(file), file, ARG1, S_add_input, "an integer");
  if (!(procedure_fits(callback, 1)))
    mus_misc_error(S_add_input, "2nd argument should be a procedure of one arg", callback);
  ss = get_global_state();
  snd_protect(callback);
  loc = get_callback_slot();
  added_inputs[loc] = ADD_INPUT(TO_C_INT(file), loc);
  added_input_callbacks[loc] = callback;
  return(TO_SCM_INT(loc));
}

static SCM g_remove_input(SCM id)
{
  int index;
  ASSERT_TYPE(INTEGER_P(id), id, ARGn, S_remove_input, "an integer");
  index = TO_C_INT(id);
  REMOVE_INPUT(added_inputs[index]);
  snd_unprotect(added_input_callbacks[index]);
  added_input_callbacks[index] = UNDEFINED_VALUE;
  return(id);
}

static SCM *idler_code = NULL;
static BACKGROUND_FUNCTION_TYPE *idler_id = NULL;
static int idlers = 0;
static BACKGROUND_FUNCTION_TYPE remember_idler(SCM code, BACKGROUND_FUNCTION_TYPE id)
{
  int i, loc = -1;
  if (idlers == 0)
    {
      idlers = 4;
      idler_code = (SCM *)MALLOC(idlers * sizeof(SCM));
      for (i = 0; i< idlers; i++) idler_code[i] = UNDEFINED_VALUE;
      idler_id = (BACKGROUND_FUNCTION_TYPE *)CALLOC(idlers, sizeof(BACKGROUND_FUNCTION_TYPE));
      loc = 0;
    }
  else
    {
      for (i = 0; i < idlers; i++)
	if (idler_id[i] == 0)
	  {
	    loc = i;
	    break;
	  }
      if (loc < 0)
	{
	  loc = idlers;
	  idlers *= 2;
	  idler_code = (SCM *)REALLOC(idler_code, idlers * sizeof(SCM));
	  idler_id = (BACKGROUND_FUNCTION_TYPE *)REALLOC(idler_id, idlers * sizeof(BACKGROUND_FUNCTION_TYPE));
	  for (i = loc; i < idlers; i++) 
	    {
	      idler_id[i] = 0;
	      idler_code[i] = UNDEFINED_VALUE;
	    }
	}
    }
  idler_code[loc] = code;
  snd_protect(code);
  idler_id[loc] = id;
  return(id);
}

static BACKGROUND_FUNCTION_TYPE forget_idler(SCM code, BACKGROUND_FUNCTION_TYPE id)
{
  int i;
  for (i = 0; i < idlers; i++)
    if (((id != 0) && (id == idler_id[i])) ||
	((BOUND_P(code)) && (EQ_P(code, idler_code[i]))))
      {
	idler_id[i] = 0;
	if (BOUND_P(idler_code[i]))
	  snd_unprotect(idler_code[i]);
	return(id);
      }
  return(id);
}

static BACKGROUND_TYPE call_idler(GUI_POINTER code)
{
#if (SCM_DEBUG_TYPING_STRICTNESS != 2)
  if (TRUE_P(CALL_0((SCM)code, "idler callback")))
    return(BACKGROUND_CONTINUE);
  forget_idler((SCM)code, (BACKGROUND_FUNCTION_TYPE)0);
#endif
  return(BACKGROUND_QUIT);
}

static SCM g_add_idler(SCM code)
{
#if (SCM_DEBUG_TYPING_STRICTNESS != 2)
  if (!(procedure_fits(code, 0)))
    mus_misc_error(S_add_idler, "argument should be a procedure of no args", code);
  return(SND_WRAP(remember_idler(code, BACKGROUND_ADD(get_global_state(), 
						      call_idler, 
						      (GUI_POINTER)code))));
#endif
}

static SCM g_remove_idler(SCM id)
{
#if (SCM_DEBUG_TYPING_STRICTNESS != 2)
  ASSERT_TYPE(SND_WRAPPED(id), id, ARGn, S_remove_idler, "a wrapped object");
  BACKGROUND_REMOVE(forget_idler(UNDEFINED_VALUE, (BACKGROUND_FUNCTION_TYPE)(SND_UNWRAP(id))));
#endif
  return(id);
}


static SCM g_make_graph_data(SCM snd, SCM chn, SCM edpos, SCM lo, SCM hi)
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
  ASSERT_TYPE(NUMBER_IF_BOUND_P(lo), lo, ARG4, S_make_graph_data, "a number");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(hi), hi, ARG5, S_make_graph_data, "a number");
  return(make_graph_data(cp,
			 to_c_edit_position(cp, edpos, S_make_graph_data, 3),
			 TO_C_INT_OR_ELSE(lo, -1),
			 TO_C_INT_OR_ELSE(hi, -1)));
}

static SCM g_graph_data(SCM data, SCM snd, SCM chn, SCM ax, SCM lo, SCM hi, SCM style)
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
  ASSERT_TYPE((LIST_P(data) && 
              (LIST_LENGTH(data) == 2) &&
              (VCT_P(CAR(data))) &&
              (VCT_P(CADR(data)))) || VCT_P(data), data, ARG1, S_graph_data, "a list of 2 vcts or vct");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(ax), ax, ARG4, S_graph_data, "an integer");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(lo), lo, ARG5, S_graph_data, "a number");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(hi), hi, ARG6, S_graph_data, "a number");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(style), style, ARG7, S_graph_data, "an integer");
  if (LIST_P(data))
    {
      v0 = get_vct(CAR(data));
      v1 = get_vct(CADR(data));
      if ((v0 == NULL) || (v1 == NULL))
        mus_misc_error(S_graph_data, "null vcts in list?", data);
    }
  else v0 = get_vct(data);
  draw_graph_data(cp, 
		  TO_C_INT_OR_ELSE(lo, -1),
		  TO_C_INT_OR_ELSE(hi, -1),
		  v0->length,
		  v0->data,
		  (v1) ? (v1->data) : NULL,
		  get_ax(cp, TO_C_INT_OR_ELSE(ax, CHAN_GC), S_graph_data),
		  TO_C_INT_OR_ELSE(style, TIME_GRAPH_STYLE(cp)));

  return(FALSE_VALUE);
}

static SCM g_main_widgets(void)
{
  snd_state *ss;
  SCM main_win;
  ss = get_global_state();
#if USE_MOTIF
  main_win = SND_WRAP(MAIN_APP(ss));
#else
  main_win = SND_WRAP(MAIN_WINDOW(ss));
#endif
  return(CONS(main_win,
	   CONS(SND_WRAP(MAIN_SHELL(ss)),
             CONS(SND_WRAP(MAIN_PANE(ss)),
               CONS(SND_WRAP(SOUND_PANE(ss)),
		 EMPTY_LIST)))));
}

#define NUM_DIALOGS 22
static SCM dialog_widgets;

static SCM g_dialog_widgets(void)
{
  if (!(VECTOR_P(dialog_widgets)))
    dialog_widgets = MAKE_PERMANENT(MAKE_VECTOR(NUM_DIALOGS, FALSE_VALUE));
  return(VECTOR_TO_LIST(dialog_widgets));
}

void set_dialog_widget(int which, GUI_WIDGET wid)
{
  if (!(VECTOR_P(dialog_widgets)))
    dialog_widgets = MAKE_PERMANENT(MAKE_VECTOR(NUM_DIALOGS, FALSE_VALUE));
  VECTOR_SET(dialog_widgets, 
	     which, 
	     SND_WRAP(wid));
}

static SCM g_widget_position(SCM wid)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARGn, S_widget_position, "a wrapped object");  
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    return(LIST_2(TO_SCM_INT(widget_x(w)),
		  TO_SCM_INT(widget_y(w))));
  ERROR(NO_SUCH_WIDGET,
	LIST_2(TO_SCM_STRING(S_widget_position),
		  wid));
  return(EMPTY_LIST);
}

static SCM g_set_widget_position(SCM wid, SCM xy)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARGn, "set-" S_widget_position, "a wrapped object");  
  ASSERT_TYPE(LIST_P(xy), xy, ARG2, "set-" S_widget_position, "a list");  
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    set_widget_position(w,
			TO_C_INT(CAR(xy)),
			TO_C_INT(CADR(xy)));
  else ERROR(NO_SUCH_WIDGET,
	     LIST_3(TO_SCM_STRING("set-" S_widget_position),
		    wid,
		    xy));
  return(wid);
}

static SCM g_widget_size(SCM wid)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARGn, S_widget_size, "a wrapped object"); 
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    return(LIST_2(TO_SCM_INT(widget_width(w)),
		  TO_SCM_INT(widget_height(w))));
  ERROR(NO_SUCH_WIDGET,
	LIST_2(TO_SCM_STRING(S_widget_size),
		  wid));
  return(EMPTY_LIST);
}

static SCM g_set_widget_size(SCM wid, SCM wh)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARG1, "set-" S_widget_size, "a wrapped object");  
  ASSERT_TYPE(LIST_P(wh), wh, ARG2, "set-" S_widget_size, "a list");  
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    set_widget_size(w,
		    TO_C_INT(CAR(wh)),
		    TO_C_INT(CADR(wh)));
  else ERROR(NO_SUCH_WIDGET,
	     LIST_3(TO_SCM_STRING("set-" S_widget_size),
		    wid,
		    wh));
  return(wid);
}

static SCM g_widget_text(SCM wid)
{
  GUI_WIDGET w;
  char *text = NULL;
  SCM res = FALSE_VALUE;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARG1, S_widget_text, "a wrapped object");
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    {
#if USE_MOTIF
      if ((XmIsText(w)) || (XmIsTextField(w)))
	{
	  text = XmTextGetString(w);
	  res = TO_SCM_STRING(text);
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
	  res = TO_SCM_STRING(text);
	}
      return(res);
#else
      if (GTK_IS_ENTRY(w))
	return(TO_SCM_STRING(gtk_entry_get_text(GTK_ENTRY(w))));
      else 
	{
	  gtk_label_get(GTK_LABEL(GTK_BIN(w)->child), &text);
	  return(TO_SCM_STRING(text));
	}
#endif
    }
  else ERROR(NO_SUCH_WIDGET,
	     LIST_2(TO_SCM_STRING(S_widget_text),
		    wid));
  return(res);
}

static SCM g_set_widget_text(SCM wid, SCM text)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARG1, "set-" S_widget_text, "a wrapped object");
  ASSERT_TYPE(STRING_P(text), text, ARG2, "set-" S_widget_text, "a string");
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    {
#if USE_MOTIF
      if ((XmIsText(w)) || (XmIsTextField(w)))
	XmTextSetString(w, TO_C_STRING(text));
      else set_button_label_normal(w, TO_C_STRING(text));
#else
      if (GTK_IS_ENTRY(w))
	gtk_entry_set_text(GTK_ENTRY(w), TO_C_STRING(text));
      else set_button_label(w, TO_C_STRING(text));
#endif
    }
  else ERROR(NO_SUCH_WIDGET,
	     LIST_3(TO_SCM_STRING("set-" S_widget_text),
		    wid,
		    text));
  return(text);
}

static SCM g_recolor_widget(SCM wid, SCM color)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARG1, S_recolor_widget, "a wrapped object");  
  ASSERT_TYPE(COLOR_P(color), color, ARG2, S_recolor_widget, "a color object"); 
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    {
#if USE_MOTIF
      XmChangeColor(w, color2pixel(color));
#else
      set_background(w, color2pixel(color));
#endif
    }
  else ERROR(NO_SUCH_WIDGET,
	     LIST_3(TO_SCM_STRING(S_recolor_widget),
		    wid,
		    color));
  return(color);
}

static SCM g_set_widget_foreground(SCM wid, SCM color)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARG1, "set-widget-foreground", "a wrapped object");  
  ASSERT_TYPE(COLOR_P(color), color, ARG2, "set-widget-foreground", "a color object"); 
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    {
#if USE_MOTIF
      XtVaSetValues(w, XmNforeground, color2pixel(color), NULL);
#endif
    }
  else ERROR(NO_SUCH_WIDGET,
	     LIST_3(TO_SCM_STRING("set-widget-foreground"),
		    wid,
		    color));
  return(color);
}


static SCM g_hide_widget(SCM wid)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARGn, S_hide_widget, "a wrapped object");  
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    {
#if USE_MOTIF
      XtUnmanageChild(w);
#else
      gtk_widget_hide(w);
#endif
    }
  else ERROR(NO_SUCH_WIDGET,
	     LIST_2(TO_SCM_STRING(S_hide_widget),
		    wid));
  return(wid);
}

static SCM g_show_widget(SCM wid)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARGn, S_show_widget, "a wrapped object");  
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    {
#if USE_MOTIF
      XtManageChild(w);
#else
      gtk_widget_show(w);
#endif
    }
  else ERROR(NO_SUCH_WIDGET,
	     LIST_2(TO_SCM_STRING(S_show_widget),
		    wid));
  return(wid);
}

static SCM g_focus_widget(SCM wid)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARGn, S_focus_widget, "a wrapped object");
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    goto_window(w);
  else ERROR(NO_SUCH_WIDGET,
	     LIST_2(TO_SCM_STRING(S_focus_widget),
		    wid));
  return(wid);
}

static SCM g_set_pixmap(SCM wid, SCM pix)
{
  GUI_WIDGET w;
  ASSERT_TYPE(SND_WRAPPED(wid), wid, ARGn, S_focus_widget, "a wrapped object");
  w = (GUI_WIDGET)(SND_UNWRAP(wid));
  if (w)
    {
#if USE_MOTIF
      if (XmIsLabel(w))
	XtVaSetValues(w, XmNlabelPixmap, (Pixmap)(SND_UNWRAP(pix)), NULL);
      else XtVaSetValues(w, XmNbackgroundPixmap, (Pixmap)(SND_UNWRAP(pix)), NULL);
#else
      /* the "pixmap" here is a list (pixmap mask) */
      gtk_pixmap_set(GTK_PIXMAP(w), (GdkPixmap *)(SND_UNWRAP(CAR(pix))), (GdkBitmap *)(SND_UNWRAP(CADR(pix))));
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

static SCM g_make_pixmap(SCM vals)
{
  snd_state *ss;
  char **bits;
  int rows, i;
  SCM row, result;

  ss = get_global_state();
  rows = LIST_LENGTH(vals);
  bits = (char **)CALLOC(rows, sizeof(char *));
  for (i = 0, row = vals; i < rows; i++, row = CDR(row))
    bits[i] = TO_C_STRING(CAR(row));
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
    result = SND_WRAP(pix);
  }
#endif
#else
  {
    GdkPixmap *pix;
    GdkBitmap *mask;
    pix = gdk_pixmap_create_from_xpm_d((GdkWindow *)(MAIN_WINDOW(ss)), &mask, NULL, bits);
    result = LIST_2(SND_WRAP(pix), SND_WRAP(mask));
  }
#endif
  FREE(bits);
  return(result);
}

static SCM g_colormap_ref(SCM map, SCM pos)
{
  unsigned short r, g, b;
  ASSERT_TYPE(INTEGER_P(map), map, ARG1, "colormap-ref", "an integer");
  ASSERT_TYPE(INTEGER_P(pos), pos, ARG2, "colormap-ref", "an integer");
  get_current_color(TO_C_INT(map), TO_C_INT(pos), &r, &g, &b);
  return(LIST_3(TO_SCM_DOUBLE((float)r / 65535.0),
		TO_SCM_DOUBLE((float)g / 65535.0),
		TO_SCM_DOUBLE((float)b / 65535.0)));
}


#ifdef ARGIFY_1
ARGIFY_7(g_draw_line_w, g_draw_line)
ARGIFY_6(g_draw_dot_w, g_draw_dot)
ARGIFY_4(g_draw_lines_w, g_draw_lines)
ARGIFY_5(g_draw_dots_w, g_draw_dots)
ARGIFY_6(g_draw_string_w, g_draw_string)
ARGIFY_7(g_fill_rectangle_w, g_fill_rectangle)
ARGIFY_4(g_fill_polygon_w, g_fill_polygon)
ARGIFY_3(g_foreground_color_w, g_foreground_color)
ARGIFY_4(g_set_foreground_color_w, g_set_foreground_color)
NARGIFY_1(g_load_font_w, g_load_font)
ARGIFY_3(g_current_font_w, g_current_font)
ARGIFY_4(g_set_current_font_w, g_set_current_font)
NARGIFY_0(g_main_widgets_w, g_main_widgets)
NARGIFY_0(g_dialog_widgets_w, g_dialog_widgets)
NARGIFY_1(g_widget_size_w, g_widget_size)
NARGIFY_2(g_set_widget_size_w, g_set_widget_size)
NARGIFY_1(g_widget_position_w, g_widget_position)
NARGIFY_2(g_set_widget_position_w, g_set_widget_position)
NARGIFY_1(g_widget_text_w, g_widget_text)
NARGIFY_2(g_set_widget_text_w, g_set_widget_text)
NARGIFY_2(g_recolor_widget_w, g_recolor_widget)
NARGIFY_1(g_hide_widget_w, g_hide_widget)
NARGIFY_1(g_show_widget_w, g_show_widget)
NARGIFY_1(g_focus_widget_w, g_focus_widget)
NARGIFY_1(g_add_idler_w, g_add_idler)
NARGIFY_1(g_remove_idler_w, g_remove_idler)
ARGIFY_5(g_make_graph_data_w, g_make_graph_data)
ARGIFY_7(g_graph_data_w, g_graph_data)
NARGIFY_2(g_add_input_w, g_add_input)
NARGIFY_1(g_remove_input_w, g_remove_input)
NARGIFY_2(g_set_widget_foreground_w, g_set_widget_foreground)
VARGIFY(g_make_bezier_w, g_make_bezier)
NARGIFY_1(g_make_pixmap_w, g_make_pixmap)
NARGIFY_2(g_set_pixmap_w, g_set_pixmap)
NARGIFY_2(g_colormap_ref_w, g_colormap_ref)
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
#define g_add_idler_w g_add_idler
#define g_remove_idler_w g_remove_idler
#define g_make_graph_data_w g_make_graph_data
#define g_graph_data_w g_graph_data
#define g_add_input_w g_add_input
#define g_remove_input_w g_remove_input
#define g_set_widget_foreground_w g_set_widget_foreground
#define g_make_bezier_w g_make_bezier
#define g_make_pixmap_w g_make_pixmap
#define g_set_pixmap_w g_set_pixmap
#define g_colormap_ref_w g_colormap_ref
#endif

void g_init_draw(SCM local_doc)
{
  dialog_widgets = UNDEFINED_VALUE;

  DEFINE_CONST(S_copy_context,         CHAN_GC,        "graphics context to draw a line");
  DEFINE_CONST(S_cursor_context,       CHAN_CGC,       "graphics context for the cursor");
  DEFINE_CONST(S_selection_context,    CHAN_SELGC,     "graphics context to draw in the selection color");

  DEFINE_PROC(S_draw_line,        g_draw_line_w, 4, 3, 0,       "(" S_draw_line " x0 y0 x1 y1 snd chn ax)");
  DEFINE_PROC(S_draw_dot,         g_draw_dot_w, 2, 4, 0,        "(" S_draw_dot " x0 y0 size snd chn ax)");
  DEFINE_PROC(S_draw_lines,       g_draw_lines_w, 1, 3, 0,      "(" S_draw_lines " lines snd chn ax)");
  DEFINE_PROC(S_draw_dots,        g_draw_dots_w, 1, 4, 0,       "(" S_draw_dots " positions dot-size snd chn ax)");
  DEFINE_PROC(S_draw_string,      g_draw_string_w, 3, 3, 0,     "(" S_draw_string " text x0 y0 snd chn ax)");
  DEFINE_PROC(S_fill_rectangle,   g_fill_rectangle_w, 4, 3, 0,  "(" S_fill_rectangle " x0 y0 width height snd chn ax)");
  DEFINE_PROC(S_fill_polygon,     g_fill_polygon_w, 1, 3, 0,    "(" S_fill_polygon " points snd chn ax)");

  define_procedure_with_reversed_setter(S_foreground_color, PROCEDURE g_foreground_color_w, "(" S_foreground_color " snd chn ax) -> current drawing color",
					"set-" S_foreground_color, PROCEDURE g_set_foreground_color_w, PROCEDURE g_set_foreground_color_reversed,
					local_doc, 0, 3, 1, 3);

  DEFINE_PROC(S_load_font,        g_load_font_w, 1, 0, 0,        "(" S_load_font " <name>) -> font-id");

  define_procedure_with_reversed_setter(S_current_font, PROCEDURE g_current_font_w, "(" S_current_font " snd chn ax) -> current font id",
					"set-" S_current_font, PROCEDURE g_set_current_font_w, PROCEDURE g_set_current_font_reversed,
					local_doc, 0, 3, 1, 3);

  DEFINE_PROC(S_main_widgets,     g_main_widgets_w, 0, 0, 0,    "returns top level widgets");
  DEFINE_PROC(S_dialog_widgets,   g_dialog_widgets_w, 0, 0, 0,  "returns a list of dialog widgets");

  define_procedure_with_setter(S_widget_size, PROCEDURE g_widget_size_w, "(" S_widget_size " wid) -> '(width height)",
					"set-" S_widget_size, PROCEDURE g_set_widget_size_w, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_widget_position, PROCEDURE g_widget_position_w, "(" S_widget_position " wid) -> '(x y)",
					"set-" S_widget_position, PROCEDURE g_set_widget_position_w, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_widget_text, PROCEDURE g_widget_text_w, "(" S_widget_text " wid) -> text)",
					"set-" S_widget_text, PROCEDURE g_set_widget_text_w, local_doc, 1, 0, 2, 0);

  DEFINE_PROC(S_recolor_widget,  g_recolor_widget_w, 2, 0, 0,  "(" S_recolor_widget " wid color)");
  DEFINE_PROC(S_hide_widget,     g_hide_widget_w, 1, 0, 0,     "(" S_hide_widget " widget)");
  DEFINE_PROC(S_show_widget,     g_show_widget_w, 1, 0, 0,     "(" S_show_widget " widget)");
  DEFINE_PROC(S_focus_widget,    g_focus_widget_w, 1, 0, 0,    "(" S_focus_widget " widget) causes widget to receive input ('focus')");

  DEFINE_PROC(S_add_idler,       g_add_idler_w, 1, 0, 0,       "(" S_add_idler " code) -> id");
  DEFINE_PROC(S_remove_idler,    g_remove_idler_w, 1, 0, 0,    "(" S_remove_idler " id)");

  DEFINE_PROC(S_make_graph_data, g_make_graph_data_w, 0, 5, 0, H_make_graph_data);
  DEFINE_PROC(S_graph_data, g_graph_data_w, 1, 6, 0, H_graph_data);


  /* ---------------- unstable ---------------- */

#if DEBUGGING
  DEFINE_CONST("erase-context",        CHAN_IGC,       "graphics context to erase a line");
  DEFINE_CONST("mark-context",         CHAN_MGC,       "graphics context for a mark");
  DEFINE_CONST("mix-context",          CHAN_GC,        "graphics context for mix waveforms");
  DEFINE_CONST("selected-mix-context", CHAN_SELMXGC,   "graphics context for selected mix waveforms");
  DEFINE_CONST("combined-context",     CHAN_TMPGC,     "graphics context for superimposed graphics");
#endif

  DEFINE_PROC(S_add_input,       g_add_input_w, 2, 0, 0,       "(" S_add_input " file callback) -> id");
  DEFINE_PROC(S_remove_input,    g_remove_input_w, 1, 0, 0,    "(" S_remove_input " id)");

  DEFINE_PROC("set-widget-foreground", g_set_widget_foreground_w, 2, 0, 0, "(set-widget-foreground widget color)");
  DEFINE_PROC(S_make_bezier,     g_make_bezier_w, 0, 0, 1,     "(" S_make_bezier " x0 y0 x1 y1 x2 y2 x3 y3 n) -> vector of points");

  DEFINE_PROC(S_make_pixmap, g_make_pixmap_w, 1, 0, 0, "(make-pixmap strs)");
  DEFINE_PROC("set-pixmap", g_set_pixmap_w, 2, 0, 0, "(set-pixmap widget pixmap)");

  DEFINE_PROC("colormap-ref", g_colormap_ref_w, 2, 0, 0, "(colormap-ref map position) -> (list r g b)");
  DEFINE_CONST("colormap-size", COLORMAP_SIZE, "colormap size");
}
#endif
