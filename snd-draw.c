#include "snd.h"

/* TODO: widget-label set-widget-label: w-name is set-button-label in motif, set-label in gtk
 *         if we had label and map_over_children (widget-for-each?), we could dispense with the menu special cases (remove_option)
 *         and if widget_activate_hook, the hooks as well.
 * TODO  in -separate mode (and elsewhere?) need to save description (sizes) of window/channels etc 
 *         but to make this work requires the load-side deferred resizing and reshaping
 *         and some way to access the sound-widget parent dialog, since otherwise widget-size is unsettable
 * TODO: similar split for make_fft_graph [needs sonogram etc??] -- complicated by background processes etc
 * TODO: decide about the "info" functions, fft-info? sync_info + accessors?
 * TODO: in gtk widget position always 0 0, or initial if using gdk_get_window_geometry?
 */

#if (!USE_NO_GUI)

#include "vct.h"

static axis_context *get_ax(chan_info *cp, int ax_id, const char *caller)
{
  if ((cp) && (AXIS_CONTEXT_ID_OK(ax_id)))
    return(set_context(cp, ax_id));
  scm_throw(NO_SUCH_AXIS_CONTEXT,
	    SCM_LIST4(TO_SCM_STRING(caller),
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
      case WAVE_AXIS_INFO: 
	return(cp->axis); 
	break;
      case FFT_AXIS_INFO:  
	if (cp->fft) return(cp->fft->axis); 
	break;
      case LISP_AXIS_INFO: 
	if (cp->lisp_info) return(cp->lisp_info->axis); 
	break;
      }
  scm_throw(NO_SUCH_AXIS_INFO,
	    SCM_LIST4(TO_SCM_STRING(caller),
		      TO_SMALL_SCM_INT(cp->sound->index),
		      TO_SMALL_SCM_INT(cp->chan),
		      TO_SMALL_SCM_INT(ap_id)));
  return(NULL);
}


static SCM g_draw_line(SCM x0, SCM y0, SCM x1, SCM y1, SCM snd, SCM chn, SCM ax)
{
  SND_ASSERT_CHAN(S_draw_line, snd, chn, 5);
  SCM_ASSERT(NUMBER_P(x0), x0, SCM_ARG1, S_draw_line);
  SCM_ASSERT(NUMBER_P(y0), y0, SCM_ARG2, S_draw_line);
  SCM_ASSERT(NUMBER_P(x1), x1, SCM_ARG3, S_draw_line);
  SCM_ASSERT(NUMBER_P(y1), y1, SCM_ARG4, S_draw_line);
  draw_line(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_line),
	    TO_C_INT(x0),
	    TO_C_INT(y0),
	    TO_C_INT(x1),
	    TO_C_INT(y1));
  return(SCM_BOOL_F);
}

static SCM g_draw_dot(SCM x0, SCM y0, SCM size, SCM snd, SCM chn, SCM ax)
{
  SND_ASSERT_CHAN(S_draw_dot, snd, chn, 4);
  SCM_ASSERT(NUMBER_P(x0), x0, SCM_ARG1, S_draw_dot);
  SCM_ASSERT(NUMBER_P(y0), y0, SCM_ARG2, S_draw_dot);
  SCM_ASSERT(NUMBER_P(size), size, SCM_ARG3, S_draw_dot);
  draw_arc(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dot),
	   TO_C_INT(x0),
	   TO_C_INT(y0),
	   TO_C_INT_OR_ELSE(size, 1));
  return(SCM_BOOL_F);
}

static SCM g_fill_rectangle(SCM x0, SCM y0, SCM width, SCM height, SCM snd, SCM chn, SCM ax)
{
  SND_ASSERT_CHAN(S_fill_rectangle, snd, chn, 5);
  SCM_ASSERT(NUMBER_P(x0), x0, SCM_ARG1, S_fill_rectangle);
  SCM_ASSERT(NUMBER_P(y0), y0, SCM_ARG2, S_fill_rectangle);
  SCM_ASSERT(NUMBER_P(width), width, SCM_ARG3, S_fill_rectangle);
  SCM_ASSERT(NUMBER_P(height), height, SCM_ARG4, S_fill_rectangle);
  fill_rectangle(TO_C_AXIS_CONTEXT(snd, chn, ax, S_fill_rectangle),
		 TO_C_INT(x0),
		 TO_C_INT(y0),
		 TO_C_INT(width),
		 TO_C_INT(height));
  return(SCM_BOOL_F);
}

static SCM g_erase_rectangle(SCM x0, SCM y0, SCM width, SCM height, SCM snd, SCM chn, SCM ax)
{
  SND_ASSERT_CHAN(S_erase_rectangle, snd, chn, 5);
  SCM_ASSERT(NUMBER_P(x0), x0, SCM_ARG1, S_erase_rectangle);
  SCM_ASSERT(NUMBER_P(y0), y0, SCM_ARG2, S_erase_rectangle);
  SCM_ASSERT(NUMBER_P(width), width, SCM_ARG3, S_erase_rectangle);
  SCM_ASSERT(NUMBER_P(height), height, SCM_ARG4, S_erase_rectangle);
  erase_rectangle(get_cp(snd, chn, S_erase_rectangle),
		  TO_C_AXIS_CONTEXT(snd, chn, ax, S_erase_rectangle),
		  TO_C_INT(x0),
		  TO_C_INT(y0),
		  TO_C_INT(width),
		  TO_C_INT(height));
  return(SCM_BOOL_F);
}

static SCM g_draw_string(SCM text, SCM x0, SCM y0, SCM snd, SCM chn, SCM ax)
{
  SND_ASSERT_CHAN(S_draw_string, snd, chn, 4);
  SCM_ASSERT(STRING_P(text), text, SCM_ARG1, S_draw_string);
  SCM_ASSERT(NUMBER_P(x0), x0, SCM_ARG2, S_draw_string);
  SCM_ASSERT(NUMBER_P(y0), y0, SCM_ARG3, S_draw_string);
  draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_string),
	      TO_C_INT(x0),
	      TO_C_INT(y0),
	      TO_C_STRING(text),
	      snd_strlen(TO_C_STRING(text)));
  return(SCM_BOOL_F);
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
  SCM_ASSERT(VECTOR_P(pts), pts, SCM_ARG1, caller);
  len = VECTOR_LENGTH(pts) / 2;
  if (len <= 0) 
    mus_misc_error(caller, "empty vector?", SCM_LIST1(pts));
  data = SCM_VELTS(pts);
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
  SND_ASSERT_CHAN(S_draw_lines, snd, chn, 2);
  pack_pts = TO_C_POINTS(pts, S_draw_lines);
  draw_lines(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_lines), 
	     pack_pts, 
	     VECTOR_LENGTH(pts) / 2);
  FREE(pack_pts);
  return(pts);
}

static SCM g_draw_dots(SCM pts, SCM size, SCM snd, SCM chn, SCM ax)
{
  /* pts should be a vector of integers as (x y) pairs */
  POINT *pack_pts;
  SND_ASSERT_CHAN(S_draw_dots, snd, chn, 3);
  pack_pts = TO_C_POINTS(pts, S_draw_dots);
  draw_points(TO_C_AXIS_CONTEXT(snd, chn, ax, S_draw_dots), 
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
  SND_ASSERT_CHAN(S_fill_polygon, snd, chn, 2);
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_fill_polygon);
  pack_pts = TO_C_POINTS(pts, S_fill_polygon);
#if USE_MOTIF
  XFillPolygon(ax->dp, ax->wn, ax->gc, pack_pts, VECTOR_LENGTH(pts) / 2, Complex, CoordModeOrigin);
#else
  gdk_draw_polygon(ax->wn, ax->gc, TRUE, pack_pts, VECTOR_LENGTH(pts) / 2);
#endif
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
      x[i] = TO_C_INT(SCM_CAR(args));
      y[i] = TO_C_INT(SCM_CADR(args));
      args = SCM_CDDR(args);
    }
  if (NOT_NULL_P(args)) 
    n = TO_C_INT(SCM_CAR(args));
  cx = 3 * (x[1] - x[0]);
  cy = 3 * (y[1] - y[0]);
  bx = 3 * (x[2] - x[1]) - cx;
  by = 3 * (y[2] - y[1]) - cy;
  ax = x[3] - (x[0] + cx + bx);
  ay = y[3] - (y[0] + cy + by);
  incr = 1.0 / (float)n;
  pts = MAKE_VECTOR(2 * (n + 1), TO_SMALL_SCM_INT(0));
  data = SCM_VELTS(pts);
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
  SND_ASSERT_CHAN(S_foreground_color, snd, chn, 1);
  SCM_ASSERT(INTEGER_IF_BOUND_P(ax), ax, SCM_ARG3, S_foreground_color);
  cp = get_cp(snd, chn, S_foreground_color);
  return(pixel2color(get_foreground_color(cp,
					  get_ax(cp, 
						 TO_C_INT_OR_ELSE(ax, CHAN_GC),
						 S_foreground_color))));
}

static SCM g_set_foreground_color(SCM color, SCM snd, SCM chn, SCM ax)
{
  chan_info *cp;
  SND_ASSERT_CHAN("set-" S_foreground_color, snd, chn, 2);
  SCM_ASSERT(COLOR_P(color), color, SCM_ARG1, "set-" S_foreground_color);
  SCM_ASSERT(INTEGER_IF_BOUND_P(ax), ax, SCM_ARG4, "set-" S_foreground_color);
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
    return(g_set_foreground_color(arg1, SCM_UNDEFINED, SCM_UNDEFINED, SCM_UNDEFINED));
  else
    {
      if (NOT_BOUND_P(arg3))
	return(g_set_foreground_color(arg2, arg1, SCM_UNDEFINED, SCM_UNDEFINED));
      else
	{
	  if (NOT_BOUND_P(arg4))
	    return(g_set_foreground_color(arg3, arg1, arg2, SCM_UNDEFINED));
	  else return(g_set_foreground_color(arg4, arg1, arg2, arg3));
	}
    }
}



/* if useful, to snd-chn.c */
static SCM g_channel_info(SCM snd, SCM chn)
{
  chan_info *cp;
  SND_ASSERT_CHAN("channel-info", snd, chn, 1);
  cp = get_cp(snd, chn, "channel-info");
  return(SCM_LIST4(TO_SCM_BOOLEAN(cp->waving),
		   TO_SCM_BOOLEAN(cp->ffting),
		   TO_SCM_BOOLEAN(cp->lisp_graphing),
		   TO_SCM_BOOLEAN(cp->cursor_on)));
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
      for (i = 0; i < num_inputs; i++) added_input_callbacks[i] = SCM_UNDEFINED;
      return(0);
    }
  for (i = 0; i < num_inputs; i++)
    if (BOUND_P(added_input_callbacks[i]))
      return(i);
  old_len = num_inputs;
  num_inputs += 4;
  added_input_callbacks = (SCM *)REALLOC(added_input_callbacks, num_inputs * sizeof(SCM));
  added_inputs = (INPUT_TYPE *)REALLOC(added_inputs, num_inputs * sizeof(INPUT_TYPE));
  for (i = old_len; i < num_inputs; i++)
    added_input_callbacks[i] = SCM_UNDEFINED;
  return(old_len);
}

#if USE_MOTIF

#define ADD_INPUT(File, Callback) \
  XtAppAddInput(MAIN_APP(ss), File, (XtPointer)XtInputReadMask, handle_input, (XtPointer)Callback)

#define REMOVE_INPUT(Id) XtRemoveInput((XtInputId)Id)

static void handle_input(XtPointer context, int *fd, XtInputId *id)
{
  int input_index = (int)context;
  CALL1(added_input_callbacks[input_index],
	TO_SCM_INT(*fd),
	"input callback");
}

static SCM g_load_font(SCM font)
{
  XFontStruct *fs = NULL;
  snd_state *ss;
  SCM_ASSERT(STRING_P(font), font, SCM_ARG1, S_load_font);
  ss = get_global_state();
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), 
		      TO_C_STRING(font));
  if (fs) return(TO_SCM_INT(fs->fid));
  return(SCM_BOOL_F);
}

static SCM g_set_current_font(SCM id, SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  SND_ASSERT_CHAN("set-" S_current_font, snd, chn, 2);
  SCM_ASSERT(INTEGER_IF_BOUND_P(ax_id), ax_id, SCM_ARG4, "set-" S_current_font);
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_current_font);
  ax->current_font = (Font)TO_C_INT(id);
  XSetFont(ax->dp, ax->gc, ax->current_font);
  return(id);
}

static SCM g_current_font(SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  chan_info *cp;
  SND_ASSERT_CHAN(S_current_font, snd, chn, 1);
  SCM_ASSERT(INTEGER_IF_BOUND_P(ax_id), ax_id, SCM_ARG3, S_current_font);
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
  CALL1(added_input_callbacks[input_index],
	TO_SCM_INT(fd),
	"input callback");
}

static SCM g_load_font(SCM font)
{
  GdkFont *fs = NULL;
  SCM_ASSERT(STRING_P(font), font, SCM_ARG1, S_load_font);
  fs = gdk_font_load(TO_C_STRING(font));
  if (fs) return(SND_WRAP(fs));
  return(SCM_BOOL_F);
}

static SCM g_set_current_font(SCM id, SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  SND_ASSERT_CHAN("set-" S_current_font, snd, chn, 2);
  SCM_ASSERT(INTEGER_IF_BOUND_P(ax_id), ax_id, SCM_ARG4, "set-" S_current_font);
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, "set-" S_current_font);
  SCM_ASSERT(SND_WRAPPED(id), id, SCM_ARG1, "set-" S_current_font);
  gdk_gc_set_font(ax->gc, (GdkFont *)SND_UNWRAP(id));
  ax->current_font = (GdkFont *)SND_UNWRAP(id);
  return(id);
}

static SCM g_current_font(SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  SND_ASSERT_CHAN(S_current_font, snd, chn, 1);
  SCM_ASSERT(INTEGER_IF_BOUND_P(ax_id), ax_id, SCM_ARG3, S_current_font);
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id, S_current_font);
  return(SND_WRAP(ax->current_font));
}

#endif

static SCM g_set_current_font_reversed(SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  if (NOT_BOUND_P(arg2))
    return(g_set_current_font(arg1, SCM_UNDEFINED, SCM_UNDEFINED, SCM_UNDEFINED));
  else
    {
      if (NOT_BOUND_P(arg3))
	return(g_set_current_font(arg2, arg1, SCM_UNDEFINED, SCM_UNDEFINED));
      else
	{
	  if (NOT_BOUND_P(arg4))
	    return(g_set_current_font(arg3, arg1, arg2, SCM_UNDEFINED));
	  else return(g_set_current_font(arg4, arg1, arg2, arg3));
	}
    }
}

static SCM g_add_input(SCM file, SCM callback)
{
  snd_state *ss;
  int loc;
  SCM_ASSERT(INTEGER_P(file), file, SCM_ARG1, S_add_input);
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
  SCM_ASSERT(INTEGER_P(id), id, SCM_ARG1, S_remove_input);
  index = TO_C_INT(id);
  REMOVE_INPUT(added_inputs[index]);
  snd_unprotect(added_input_callbacks[index]);
  added_input_callbacks[index] = SCM_UNDEFINED;
  return(id);
}


static BACKGROUND_TYPE call_idler(GUI_POINTER code)
{
  if (TRUE_P(CALL0(SND_UNWRAP((SCM)code), "idler callback")))
    return(BACKGROUND_CONTINUE);
  return(BACKGROUND_QUIT);
}

static SCM g_add_idler(SCM code)
{
  if (!(procedure_fits(code, 0)))
    mus_misc_error(S_add_idler, "argument should be a procedure of no args", code);
  return(SND_WRAP(BACKGROUND_ADD(get_global_state(), 
				 call_idler, 
				 (GUI_POINTER)SND_WRAP(code))));
}

static SCM g_remove_idler(SCM id)
{
  SCM_ASSERT(SND_WRAPPED(id), id, SCM_ARG1, S_remove_idler);
  BACKGROUND_REMOVE(SND_UNWRAP(id));
  return(id);
}


static SCM g_make_graph_data(SCM snd, SCM chn, SCM pos, SCM lo, SCM hi)
{
  #define H_make_graph_data "(" S_make_graph_data " snd chn edit-pos low high)\n\
returns either a vct (if the graph has one trace), or a \
list of two vcts (the two sides of the envelope graph). \
'edit-position' defaults to the current edit history position, \
'low' defaults to the current window left sample, and \
'high' defaults to the current rightmost sample. \
(graph-data (make-graph-data)) reimplements the time domain graph."

  chan_info *cp;
  SND_ASSERT_CHAN(S_make_graph_data, snd, chn, 1);
  cp = get_cp(snd, chn, S_make_graph_data);
  SCM_ASSERT(INTEGER_IF_BOUND_P(pos), pos, SCM_ARG3, S_make_graph_data);
  SCM_ASSERT(NUMBER_IF_BOUND_P(lo), lo, SCM_ARG4, S_make_graph_data);
  SCM_ASSERT(NUMBER_IF_BOUND_P(hi), hi, SCM_ARG5, S_make_graph_data);
  return(make_graph_data(cp,
			 TO_C_INT_OR_ELSE(pos, cp->edit_ctr),
			 TO_C_INT_OR_ELSE(lo, -1),
			 TO_C_INT_OR_ELSE(hi, -1)));
}

static SCM g_graph_data(SCM data, SCM snd, SCM chn, SCM ax, SCM lo, SCM hi, SCM style)
{
  #define H_graph_data "(" S_graph_data " snd chn context low high graph-style)\n\
'graph-data' displays 'data' in the time domain graph of 'snd's channel \
'chn' using the graphics context 'context' (normally copy-context), placing the \
data in the recipient's graph between points 'low' and 'high' \
in the drawing mode 'graphic-style'."

  chan_info *cp;
  vct *v0, *v1 = NULL;
  SND_ASSERT_CHAN(S_graph_data, snd, chn, 2);
  cp = get_cp(snd, chn, S_graph_data);
  SCM_ASSERT(LIST_P(data) || VCT_P(data), data, SCM_ARG1, S_graph_data);
  SCM_ASSERT(INTEGER_IF_BOUND_P(ax), ax, SCM_ARG4, S_graph_data);
  SCM_ASSERT(NUMBER_IF_BOUND_P(lo), lo, SCM_ARG5, S_graph_data);
  SCM_ASSERT(NUMBER_IF_BOUND_P(hi), hi, SCM_ARG6, S_graph_data);
  SCM_ASSERT(INTEGER_IF_BOUND_P(style), style, SCM_ARG7, S_graph_data);
  if (LIST_P(data))
    {
      v0 = get_vct(SCM_CAR(data));
      v1 = get_vct(SCM_CADR(data));
    }
  else v0 = get_vct(data);

  draw_graph_data(cp, 
		  TO_C_INT_OR_ELSE(lo, -1),
		  TO_C_INT_OR_ELSE(hi, -1),
		  v0->length,
		  v0->data,
		  (v1) ? (v1->data) : NULL,
		  get_ax(cp, TO_C_INT_OR_ELSE(ax, CHAN_GC), S_graph_data),
		  TO_C_INT_OR_ELSE(style, MAIN_GRAPH_STYLE(cp)));

  return(SCM_BOOL_F);
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
		 SCM_EOL)))));
}

#define NUM_DIALOGS 22
static SCM dialog_widgets = SCM_UNDEFINED;

static SCM g_dialog_widgets(void)
{
  if (!(VECTOR_P(dialog_widgets)))
    dialog_widgets = scm_permanent_object(MAKE_VECTOR(NUM_DIALOGS, SCM_BOOL_F));
  return(scm_vector_to_list(dialog_widgets));
}

void set_dialog_widget(int which, GUI_WIDGET wid)
{
  if (!(VECTOR_P(dialog_widgets)))
    dialog_widgets = scm_permanent_object(MAKE_VECTOR(NUM_DIALOGS, SCM_BOOL_F));
  scm_vector_set_x(dialog_widgets, 
		   TO_SMALL_SCM_INT(which), 
		   SND_WRAP(wid));
}

static SCM g_widget_position(SCM wid)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, S_widget_position);  
  return(SCM_LIST2(TO_SCM_INT(widget_x((GUI_WIDGET)(SND_UNWRAP(wid)))),
		   TO_SCM_INT(widget_y((GUI_WIDGET)(SND_UNWRAP(wid))))));
}

static SCM g_set_widget_position(SCM wid, SCM xy)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, "set-" S_widget_position);  
  SCM_ASSERT(LIST_P(xy), xy, SCM_ARG2, "set-" S_widget_position);  
  set_widget_position((GUI_WIDGET)(SND_UNWRAP(wid)),
		      TO_C_INT(SCM_CAR(xy)),
		      TO_C_INT(SCM_CADR(xy)));
  return(wid);
}

static SCM g_widget_size(SCM wid)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, S_widget_size);  
  return(SCM_LIST2(TO_SCM_INT(widget_width((GUI_WIDGET)(SND_UNWRAP(wid)))),
		   TO_SCM_INT(widget_height((GUI_WIDGET)(SND_UNWRAP(wid))))));
}

static SCM g_set_widget_size(SCM wid, SCM wh)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, "set-" S_widget_size);  
  SCM_ASSERT(LIST_P(wh), wh, SCM_ARG2, "set-" S_widget_size);  
  set_widget_size((GUI_WIDGET)(SND_UNWRAP(wid)),
		  TO_C_INT(SCM_CAR(wh)),
		  TO_C_INT(SCM_CADR(wh)));
  return(wid);
}

static SCM g_recolor_widget(SCM wid, SCM color)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, S_recolor_widget);  
  SCM_ASSERT(COLOR_P(color), color, SCM_ARG2, S_recolor_widget); 
#if USE_MOTIF
  XmChangeColor((GUI_WIDGET)(SND_UNWRAP(wid)), color2pixel(color));
#else
  set_background((GUI_WIDGET)(SND_UNWRAP(wid)), color2pixel(color));
#endif
  return(color);
}

static SCM g_set_widget_foreground(SCM wid, SCM color)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, "set-widget-foreground");  
  SCM_ASSERT(COLOR_P(color), color, SCM_ARG2, "set-widget-foreground"); 
#if USE_MOTIF
  XtVaSetValues((GUI_WIDGET)(SND_UNWRAP(wid)), XmNforeground, color2pixel(color), NULL);
#endif
  return(color);
}


static SCM g_hide_widget(SCM wid)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, S_hide_widget);  
#if USE_MOTIF
  XtUnmanageChild((GUI_WIDGET)(SND_UNWRAP(wid)));
#else
  gtk_widget_hide((GUI_WIDGET)(SND_UNWRAP(wid)));
#endif
  return(wid);
}

static SCM g_show_widget(SCM wid)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, S_show_widget);  
#if USE_MOTIF
  XtManageChild((GUI_WIDGET)(SND_UNWRAP(wid)));
#else
  gtk_widget_show((GUI_WIDGET)(SND_UNWRAP(wid)));
#endif
  return(wid);
}

static SCM g_focus_widget(SCM wid)
{
  SCM_ASSERT(SND_WRAPPED(wid), wid, SCM_ARG1, S_focus_widget);
  goto_window((GUI_WIDGET)(SND_UNWRAP(wid)));
  return(wid);
}


#if USE_MOTIF
/* backwards compatibility */
static SCM g_main_shell(void)
{
  snd_state *ss;
  ss = get_global_state();
  return(SND_WRAP(MAIN_SHELL(ss)));
}

#endif

void g_init_draw(SCM local_doc)
{
  /* ---------------- stable? ---------------- */

  DEFINE_VAR(S_time_graph,           WAVE_AXIS_INFO, "time domain graph");
  DEFINE_VAR(S_fft_graph,            FFT_AXIS_INFO,  "frequency domain graph");
  DEFINE_VAR(S_lisp_graph,           LISP_AXIS_INFO, "lisp graph");

  DEFINE_VAR(S_copy_context,         CHAN_GC,        "graphics context to draw a line");
  DEFINE_VAR(S_cursor_context,       CHAN_CGC,       "graphics context for the cursor");

  DEFINE_PROC(S_draw_line,        g_draw_line, 4, 3, 0,       "(" S_draw_line " x0 y0 x1 y1 snd chn ax)");
  DEFINE_PROC(S_draw_dot,         g_draw_dot, 2, 4, 0,        "(" S_draw_dot " x0 y0 size snd chn ax)");
  DEFINE_PROC(S_draw_lines,       g_draw_lines, 1, 3, 0,      "(" S_draw_lines " lines snd chn ax)");
  DEFINE_PROC(S_draw_dots,        g_draw_dots, 1, 4, 0,       "(" S_draw_dots " positions dot-size snd chn ax)");
  DEFINE_PROC(S_draw_string,      g_draw_string, 3, 3, 0,     "(" S_draw_string " text x0 y0 snd chn ax)");
  DEFINE_PROC(S_fill_rectangle,   g_fill_rectangle, 4, 3, 0,  "(" S_fill_rectangle " x0 y0 width height snd chn ax)");
  DEFINE_PROC(S_fill_polygon,     g_fill_polygon, 1, 3, 0,    "(" S_fill_polygon " points snd chn ax)");
  DEFINE_PROC(S_erase_rectangle,  g_erase_rectangle, 4, 3, 0, "(" S_erase_rectangle " x0 y0 width height snd chn ax)");

  define_procedure_with_reversed_setter(S_foreground_color, SCM_FNC g_foreground_color, "(" S_foreground_color " snd chn ax) -> current drawing color",
					"set-" S_foreground_color, SCM_FNC g_set_foreground_color, SCM_FNC g_set_foreground_color_reversed,
					local_doc, 0, 3, 1, 3);

  DEFINE_PROC(S_load_font,        g_load_font, 1, 0, 0,        "(" S_load_font " <name>) -> font-id");

  define_procedure_with_reversed_setter(S_current_font, SCM_FNC g_current_font, "(" S_current_font " snd chn ax) -> current font id",
					"set-" S_current_font, SCM_FNC g_set_current_font, SCM_FNC g_set_current_font_reversed,
					local_doc, 0, 3, 1, 3);

  DEFINE_PROC(S_main_widgets,     g_main_widgets, 0, 0, 0,    "returns top level widgets");
  DEFINE_PROC(S_dialog_widgets,   g_dialog_widgets, 0, 0, 0,  "returns a list of dialog widgets");

  define_procedure_with_setter(S_widget_size, SCM_FNC g_widget_size, "(" S_widget_size " wid) -> '(width height)",
					"set-" S_widget_size, SCM_FNC g_set_widget_size, local_doc, 1, 0, 2, 0);

  define_procedure_with_setter(S_widget_position, SCM_FNC g_widget_position, "(" S_widget_position " wid) -> '(x y)",
					"set-" S_widget_position, SCM_FNC g_set_widget_position, local_doc, 1, 0, 2, 0);

  DEFINE_PROC(S_recolor_widget,  g_recolor_widget, 2, 0, 0,  "(" S_recolor_widget " wid color)");
  DEFINE_PROC(S_hide_widget,     g_hide_widget, 1, 0, 0,     "(" S_hide_widget " widget)");
  DEFINE_PROC(S_show_widget,     g_show_widget, 1, 0, 0,     "(" S_show_widget " widget)");
  DEFINE_PROC(S_focus_widget,    g_focus_widget, 1, 0, 0,    "(" S_focus_widget " widget) causes widget to receive input ('focus')");

  DEFINE_PROC(S_add_idler,       g_add_idler, 1, 0, 0,       "(" S_add_idler " code) -> id");
  DEFINE_PROC(S_remove_idler,    g_remove_idler, 1, 0, 0,    "(" S_remove_idler " id)");



  /* ---------------- unstable ---------------- */

  DEFINE_PROC(S_make_graph_data, g_make_graph_data, 0, 5, 0, H_make_graph_data);
  DEFINE_PROC(S_graph_data, g_graph_data, 1, 6, 0, H_graph_data);

  DEFINE_VAR("erase-context",        CHAN_IGC,       "graphics context to erase a line");
  DEFINE_VAR("selection-context",    CHAN_SELGC,     "graphics context to draw a line in a selection");
  DEFINE_VAR("mark-context",         CHAN_MGC,       "graphics context for a mark");
  DEFINE_VAR("mix-context",          CHAN_GC,        "graphics context for mix waveforms");
  DEFINE_VAR("selected-mix-context", CHAN_SELMXGC,   "graphics context for selected mix waveforms");
  DEFINE_VAR("combined-context",     CHAN_TMPGC,     "graphics context for superimposed graphics");

  DEFINE_PROC("channel-info",    g_channel_info, 0, 2, 0,    "(channel-info snd chn)");

  DEFINE_PROC(S_add_input,       g_add_input, 2, 0, 0,       "(" S_add_input " file callback) -> id");
  DEFINE_PROC(S_remove_input,    g_remove_input, 1, 0, 0,    "(" S_remove_input " id)");

  DEFINE_PROC("set-widget-foreground", g_set_widget_foreground, 2, 0, 0, "(set-widget-foreground widget color)");
  DEFINE_PROC(S_make_bezier,     g_make_bezier, 0, 0, 1,     "(" S_make_bezier " x0 y0 x1 y1 x2 y2 x3 y3 n) -> vector of points");


  /* ---------------- backwards compatibility ---------------- */
#if USE_MOTIF
  DEFINE_PROC("snd-main-shell", g_main_shell, 0, 0, 0, "snd-main-shell tries to return Snd's topmost widget");
#endif
}
#endif
