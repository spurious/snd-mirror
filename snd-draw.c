#include "snd.h"

/* this file is experimental */

#if HAVE_GUILE && (!USE_NO_GUI)

#include "vct.h"

#define AXIS_CONTEXT_ID_OK(Id) ((Id >= CHAN_GC) && (Id <= CHAN_SELMXGC))

#define TO_C_AXIS_CONTEXT(Snd, Chn, Ax) \
  get_ax(get_cp(Snd, Chn, __FUNCTION__), \
         TO_C_INT_OR_ELSE(Ax, CHAN_GC), \
         __FUNCTION__)

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


enum {WAVE_AXIS_INFO, FFT_AXIS_INFO, LISP_AXIS_INFO};

#define AXIS_INFO_ID_OK(Id) ((Id >= WAVE_AXIS_INFO) && ((Id <= LISP_AXIS_INFO)))

#define TO_C_AXIS_INFO(Snd, Chn, Ap) \
  get_ap(get_cp(Snd, Chn, __FUNCTION__), \
         TO_C_INT_OR_ELSE(Ap, WAVE_AXIS_INFO), \
         __FUNCTION__)

static axis_info *get_ap(chan_info *cp, int ap_id, const char *caller)
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
  draw_line(TO_C_AXIS_CONTEXT(snd, chn, ax),
	    TO_C_INT(x0),
	    TO_C_INT(y0),
	    TO_C_INT(x1),
	    TO_C_INT(y1));
  return(SCM_BOOL_F);
}

static SCM g_draw_dot(SCM x0, SCM y0, SCM size, SCM snd, SCM chn, SCM ax)
{
  draw_arc(TO_C_AXIS_CONTEXT(snd, chn, ax),
	   TO_C_INT(x0),
	   TO_C_INT(y0),
	   TO_C_INT_OR_ELSE(size, 1));
  return(SCM_BOOL_F);
}

static SCM g_fill_rectangle(SCM x0, SCM y0, SCM width, SCM height, SCM snd, SCM chn, SCM ax)
{
  fill_rectangle(TO_C_AXIS_CONTEXT(snd, chn, ax),
		 TO_C_INT(x0),
		 TO_C_INT(y0),
		 TO_C_INT(width),
		 TO_C_INT(height));
  return(SCM_BOOL_F);
}

static SCM g_erase_rectangle(SCM x0, SCM y0, SCM width, SCM height, SCM snd, SCM chn, SCM ax)
{
  erase_rectangle(get_cp(snd, chn, __FUNCTION__),
		  TO_C_AXIS_CONTEXT(snd, chn, ax),
		  TO_C_INT(x0),
		  TO_C_INT(y0),
		  TO_C_INT(width),
		  TO_C_INT(height));
  return(SCM_BOOL_F);
}

static SCM g_draw_string(SCM text, SCM x0, SCM y0, SCM snd, SCM chn, SCM ax)
{
  draw_string(TO_C_AXIS_CONTEXT(snd, chn, ax),
	      TO_C_INT(x0),
	      TO_C_INT(y0),
	      SCM_STRING_CHARS(text),
	      snd_strlen(SCM_STRING_CHARS(text)));
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
  SCM_ASSERT(gh_vector_p(pts), pts, SCM_ARG1, caller);
  len = gh_vector_length(pts) / 2;
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
  pack_pts = TO_C_POINTS(pts, "draw-lines");
  draw_lines(TO_C_AXIS_CONTEXT(snd, chn, ax), 
	     pack_pts, 
	     gh_vector_length(pts) / 2);
  FREE(pack_pts);
  return(pts);
}

static SCM g_draw_dots(SCM pts, SCM size, SCM snd, SCM chn, SCM ax)
{
  /* pts should be a vector of integers as (x y) pairs */
  POINT *pack_pts;
  pack_pts = TO_C_POINTS(pts, "draw-dots");
  draw_points(TO_C_AXIS_CONTEXT(snd, chn, ax), 
	      pack_pts, 
	      gh_vector_length(pts) / 2,
	      TO_C_INT_OR_ELSE(size, 1));
  FREE(pack_pts);
  return(pts);
}

static SCM g_fill_polygon(SCM pts, SCM snd, SCM chn, SCM ax_id)
{
  POINT *pack_pts;
  axis_context *ax;
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id);
  pack_pts = TO_C_POINTS(pts, "draw-dots");
#if USE_MOTIF
  XFillPolygon(ax->dp, ax->wn, ax->gc, pack_pts, gh_vector_length(pts) / 2, Convex, CoordModeOrigin);
#else
  gdk_draw_polygon(ax->wn, ax->gc, TRUE, pack_pts, gh_vector_length(pts) / 2);
#endif
  return(pts);
}

/* void draw_grf_points(chan_info *cp, axis_context *ax, int j, axis_info *ap, Float y0, int graph_style) */
/* void draw_both_grf_points(chan_info *cp, axis_context *ax, int j, int graph_style) */
static SCM g_draw_graph(SCM data, SCM snd, SCM chn, SCM ax, SCM graph_style)
{
  /* if data is a vector, assume it is a set of (x y) ints ready to be drawn,
   *   if a vct, assume it's data properly aligned and so on (i.e. make_graph with pos arg)
   * also g_draw_two_sided_graph
   */
  return(SCM_BOOL_F);
}

static SCM g_foreground_color(SCM snd, SCM chn, SCM ax)
{
  chan_info *cp;
  cp = get_cp(snd, chn, __FUNCTION__);
  return(pixel2color(get_foreground_color(cp,
					  get_ax(cp, 
						 TO_C_INT_OR_ELSE(ax, CHAN_GC),
						 __FUNCTION__))));
}

static SCM g_set_foreground_color(SCM color, SCM snd, SCM chn, SCM ax)
{
  chan_info *cp;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, __FUNCTION__);
  cp = get_cp(snd, chn, __FUNCTION__);
  set_foreground_color(cp,
		       get_ax(cp, 
			      TO_C_INT_OR_ELSE(ax, CHAN_GC),
			      __FUNCTION__),
		       color2pixel(color));
  return(color);
}

/* if useful, to snd-axis.c */
static SCM g_grf_x(SCM val, SCM snd, SCM chn, SCM ap)
{
  return(TO_SCM_INT(grf_x(TO_C_DOUBLE(val),
			  TO_C_AXIS_INFO(snd, chn, ap))));
}

static SCM g_grf_y(SCM val, SCM snd, SCM chn, SCM ap)
{
  return(TO_SCM_INT(grf_y(TO_C_DOUBLE(val),
			  TO_C_AXIS_INFO(snd, chn, ap))));
}

static SCM g_ungrf_x(SCM val, SCM snd, SCM chn, SCM ap)
{
  return(TO_SCM_DOUBLE(ungrf_x(TO_C_AXIS_INFO(snd, chn, ap),
			       TO_C_INT(val))));
}

static SCM g_ungrf_y(SCM val, SCM snd, SCM chn, SCM ap)
{
  return(TO_SCM_DOUBLE(ungrf_y(TO_C_AXIS_INFO(snd, chn, ap),
			       TO_C_INT(val))));
}


/* if useful, to snd-chn.c */
static SCM g_cursor_position(SCM snd, SCM chn)
{
  chan_info *cp;
  cp = get_cp(snd, chn, __FUNCTION__);
  return(SCM_LIST2(TO_SCM_INT(cp->cx),
		   TO_SCM_INT(cp->cy)));
}

/* to snd-axis.c */
static SCM g_axis_info(SCM snd, SCM chn, SCM ap_id)
{
  axis_info *ap;
  ap = TO_C_AXIS_INFO(snd, chn, ap_id);
  return(scm_cons(TO_SCM_INT(ap->losamp),
	  scm_cons(TO_SCM_INT(ap->hisamp),
	   scm_cons(TO_SCM_DOUBLE(ap->x0),
            scm_cons(TO_SCM_DOUBLE(ap->y0),
             scm_cons(TO_SCM_DOUBLE(ap->x1),
              scm_cons(TO_SCM_DOUBLE(ap->y1),
               scm_cons(TO_SCM_DOUBLE(ap->xmin),
                scm_cons(TO_SCM_DOUBLE(ap->ymin),
                 scm_cons(TO_SCM_DOUBLE(ap->xmax),
                  scm_cons(TO_SCM_DOUBLE(ap->ymax),
                   scm_cons(TO_SCM_INT(ap->x_axis_x0),
                    scm_cons(TO_SCM_INT(ap->y_axis_y0),
                     scm_cons(TO_SCM_INT(ap->x_axis_x1),
		      scm_cons(TO_SCM_INT(ap->y_axis_y1),
			       SCM_EOL)))))))))))))));
}

/* if useful, to snd-chn.c */
static SCM g_channel_info(SCM snd, SCM chn)
{
  chan_info *cp;
  cp = get_cp(snd, chn, __FUNCTION__);
  return(SCM_LIST4(TO_SCM_BOOLEAN(cp->waving),
		   TO_SCM_BOOLEAN(cp->ffting),
		   TO_SCM_BOOLEAN(cp->lisp_graphing),
		   TO_SCM_BOOLEAN(cp->cursor_on)));
}

static SCM make_vct_from_samples(int len, MUS_SAMPLE_TYPE *data)
{
  Float *new_data;
  int i;
  new_data = (Float *)CALLOC(len, sizeof(Float));
  for (i = 0; i < len; i++)
    new_data[i] = MUS_SAMPLE_TO_FLOAT(data[i]);
  return(make_vct(len, new_data));
}

#if SNDLIB_USE_FLOATS
  #define VCT_WRAP(Len, Data) make_vct_wrapper(Len, Data)
#else
  #define VCT_WRAP(Len,Data) make_vct_from_samples(Len, Data)
#endif

/* if useful, to snd-snd.c */
static SCM g_peak_env_info(SCM snd, SCM chn, SCM pos)
{
  chan_info *cp;
  env_info *ep;
  chan_context *cgx;
  cp = get_cp(snd, chn, __FUNCTION__);
  cgx = cp->cgx;
  if ((!cgx) || (!(cp->amp_envs))) 
    return(SCM_LIST0);
  ep = cp->amp_envs[TO_C_INT_OR_ELSE(pos, cp->edit_ctr)];
  if (ep)
    return(SCM_LIST7(TO_SCM_BOOLEAN(ep->completed),
		     TO_SCM_INT(ep->samps_per_bin),
		     TO_SCM_INT(ep->amp_env_size),
		     TO_SCM_DOUBLE(MUS_SAMPLE_TO_FLOAT(ep->fmin)),
		     TO_SCM_DOUBLE(MUS_SAMPLE_TO_FLOAT(ep->fmax)),
		     VCT_WRAP(ep->amp_env_size, ep->data_min),
		     VCT_WRAP(ep->amp_env_size, ep->data_max)));
  return(SCM_LIST0);
}

/* fft-info ? */


/* these should be in snd-xscm and snd-gscm once tested etc */

static int num_inputs = 0;
static SCM added_input_callbacks[4];

#if USE_MOTIF

#define ADD_INPUT(File, Callback) \
  XtAppAddInput(MAIN_APP(ss), File, (XtPointer)XtInputReadMask, handle_input, (XtPointer)Callback)

#define REMOVE_INPUT(Id) XtRemoveInput((XtInputId)Id)

static XtInputId added_inputs[4];

static void handle_input(XtPointer context, int *fd, XtInputId *id)
{
  int input_index = (int)context;
  g_call1(added_input_callbacks[input_index],
	  TO_SCM_INT(*fd));
}

static SCM g_load_font(SCM font)
{
  XFontStruct *fs = NULL;
  snd_state *ss;
  SCM_ASSERT(gh_string_p(font), font, SCM_ARG1, __FUNCTION__);
  ss = get_global_state();
  fs = XLoadQueryFont(MAIN_DISPLAY(ss), 
		      SCM_STRING_CHARS(font));
  if (fs) return(TO_SCM_INT(fs->fid));
  return(SCM_BOOL_F);
}

static SCM g_set_current_font(SCM id, SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id);
  ax->current_font = (Font)TO_C_INT(id);
  XSetFont(ax->dp, ax->gc, ax->current_font);
  return(id);
}

static SCM g_current_font(SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id);
  return(TO_SCM_INT(ax->current_font));
}

#else

#define ADD_INPUT(File, Callback) \
  gdk_input_add(File, GDK_INPUT_READ, handle_input, (gpointer)Callback)

#define REMOVE_INPUT(Id) gdk_input_remove((gint)Id)

static gint added_inputs[4];

static void handle_input(gpointer context, gint fd, GdkInputCondition condition)
{
  int input_index = (int)context;
  g_call1(added_input_callbacks[input_index],
	  TO_SCM_INT(fd));
}

static SCM g_load_font(SCM font)
{
  GdkFont *fs = NULL;
  SCM_ASSERT(gh_string_p(font), font, SCM_ARG1, __FUNCTION__);
  fs = gdk_font_load(SCM_STRING_CHARS(font));
  if (fs) return(SCM_WRAP(fs));
  return(SCM_BOOL_F);
}

static SCM g_set_current_font(SCM id, SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id);
  gdk_gc_set_font(ax->gc, (GdkFont *)SCM_UNWRAP(id));
  ax->current_font = (GdkFont *)SCM_UNWRAP(id);
  return(id);
}

static SCM g_current_font(SCM snd, SCM chn, SCM ax_id)
{
  axis_context *ax;
  ax = TO_C_AXIS_CONTEXT(snd, chn, ax_id);
  return(SCM_WRAP(ax->current_font));
}

#endif

static SCM g_add_input(SCM file, SCM callback)
{
  snd_state *ss;
  ss = get_global_state();
  snd_protect(callback);
  added_inputs[num_inputs] = ADD_INPUT(TO_C_INT(file), num_inputs);
  added_input_callbacks[num_inputs] = callback;
  num_inputs++;
  return(TO_SCM_INT(num_inputs - 1));
}

static SCM g_remove_input(SCM id)
{
  int index;
  index = TO_C_INT(id);
  REMOVE_INPUT(added_inputs[index]);
  snd_unprotect(added_input_callbacks[index]);
  return(id);
}

/* TODO: widget <num> | widget snd <num> | widget snd chn <num>
 * TODO:   hide-widget show-widget widget-color set-widget-color widget-label set-widget-label widget-position widget-size (+ sets?)
 */


void g_init_draw(SCM local_doc)
{
  DEFINE_VAR("copy-context",         TO_SMALL_SCM_INT(CHAN_GC),        "graphics context to draw a line");
  /* not sure the rest actually make any sense -- ax arg above might not be needed */

  DEFINE_VAR("erase-context",        TO_SMALL_SCM_INT(CHAN_IGC),       "graphics context to erase a line");
  DEFINE_VAR("selection-context",    TO_SMALL_SCM_INT(CHAN_SELGC),     "graphics context to draw a line in a selection");
  DEFINE_VAR("cursor-context",       TO_SMALL_SCM_INT(CHAN_CGC),       "graphics context for the cursor");
  DEFINE_VAR("mark-context",         TO_SMALL_SCM_INT(CHAN_MGC),       "graphics context for a mark");
  DEFINE_VAR("mix-context",          TO_SMALL_SCM_INT(CHAN_GC),        "graphics context for mix waveforms");
  DEFINE_VAR("selected-mix-context", TO_SMALL_SCM_INT(CHAN_SELMXGC),   "graphics context for selected mix waveforms");
  DEFINE_VAR("combined-context",     TO_SMALL_SCM_INT(CHAN_TMPGC),     "graphics context for superimposed graphics");

  DEFINE_VAR("time-graph",           TO_SMALL_SCM_INT(WAVE_AXIS_INFO), "time domain graph");
  DEFINE_VAR("fft-graph",            TO_SMALL_SCM_INT(FFT_AXIS_INFO),  "frequency domain graph");
  DEFINE_VAR("lisp-graph",           TO_SMALL_SCM_INT(LISP_AXIS_INFO), "lisp graph");

  DEFINE_PROC(gh_new_procedure("draw-line",        SCM_FNC g_draw_line, 4, 3, 0),       "(draw-line x0 y0 x1 y1 snd chn ax)");
  DEFINE_PROC(gh_new_procedure("draw-dot",         SCM_FNC g_draw_dot, 2, 4, 0),        "(draw-dot x0 y0 size snd chn ax)");
  DEFINE_PROC(gh_new_procedure("draw-lines",       SCM_FNC g_draw_lines, 1, 3, 0),      "(draw-lines lines snd chn ax)");
  DEFINE_PROC(gh_new_procedure("draw-dots",        SCM_FNC g_draw_dots, 1, 4, 0),       "(draw-dots positions dot-size snd chn ax)");
  DEFINE_PROC(gh_new_procedure("draw-string",      SCM_FNC g_draw_string, 3, 3, 0),     "(draw-string text x0 y0 snd chn ax)");
  DEFINE_PROC(gh_new_procedure("fill-rectangle",   SCM_FNC g_fill_rectangle, 4, 3, 0),  "(fill-rectangle x0 y0 width height snd chn ax)");
  DEFINE_PROC(gh_new_procedure("fill-polygon",     SCM_FNC g_fill_polygon, 1, 3, 0),    "(fill-polygon points snd chn ax)");
  DEFINE_PROC(gh_new_procedure("erase-rectangle",  SCM_FNC g_erase_rectangle, 4, 3, 0), "(erase-rectangle x0 y0 width height snd chn ax)");

  DEFINE_PROC(gh_new_procedure("foreground-color",     SCM_FNC g_foreground_color, 0, 3, 0),     "(foreground-color snd chn ax)");
  DEFINE_PROC(gh_new_procedure("set-foreground-color", SCM_FNC g_set_foreground_color, 1, 3, 0), "(set-foreground-color color snd chn ax)");

  DEFINE_PROC(gh_new_procedure("load-font",        SCM_FNC g_load_font, 1, 0, 0),        "(load-font <name>) -> font-id");
  DEFINE_PROC(gh_new_procedure("current-font",     SCM_FNC g_current_font, 0, 3, 0),     "(current-font snd chn ax) -> font-id");
  DEFINE_PROC(gh_new_procedure("set-current-font", SCM_FNC g_set_current_font, 1, 3, 0), "(current-font font-id snd chn ax)");

  DEFINE_PROC(gh_new_procedure("x->position",     SCM_FNC g_grf_x, 1, 3, 0),           "(x->position val snd chn ax)");
  DEFINE_PROC(gh_new_procedure("y->position",     SCM_FNC g_grf_y, 1, 3, 0),           "(y->position val snd chn ax)");
  DEFINE_PROC(gh_new_procedure("position->x",     SCM_FNC g_ungrf_x, 1, 3, 0),         "(position->x val snd chn ax)");
  DEFINE_PROC(gh_new_procedure("position->y",     SCM_FNC g_ungrf_y, 1, 3, 0),         "(position->y val snd chn ax)");
  DEFINE_PROC(gh_new_procedure("cursor-position", SCM_FNC g_cursor_position, 0, 2, 0), "(cursor-position snd chn -> '(x y))");

  DEFINE_PROC(gh_new_procedure("graph-info",      SCM_FNC g_axis_info, 0, 3, 0),       "(graph-info snd chn ax)");
  DEFINE_PROC(gh_new_procedure("channel-info",    SCM_FNC g_channel_info, 0, 2, 0),    "(channel-info snd chn)");
  DEFINE_PROC(gh_new_procedure("peak-env-info",   SCM_FNC g_peak_env_info, 0, 3, 0),   "(peak-env-info snd chn pos)");

  DEFINE_PROC(gh_new_procedure("add-input",       SCM_FNC g_add_input, 2, 0, 0),       "(add-input file callback) -> id");
  DEFINE_PROC(gh_new_procedure("remove-input",    SCM_FNC g_remove_input, 1, 0, 0),    "(remove-input id)");
}
#endif
