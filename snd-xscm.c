#include "snd.h"
#include "vct.h"

/* TODO:   user-loaded colormaps need to be added to the Color dialog list, etc
 * TODO    colored marks etc (requires 2 pixels for selected/unselected graphs)?
 * TODO    colormaps applied to lisp or data graph?
 * TODO    option to set the background-colour in the fft/transform-window?
 */

static snd_state *state;

static void timed_eval(XtPointer in_code, XtIntervalId *id)
{
  CALL0((SCM)in_code, "timed callback func");
  snd_unprotect((SCM)in_code);
}

static SCM g_in(SCM ms, SCM code)
{
  #define H_in "(" S_in " msecs thunk) invokes thunk in msecs milliseconds"
  ASSERT_TYPE(INTEGER_P(ms), ms, SCM_ARG1, S_in, "an integer");
  if (procedure_fits(code, 0))
    {
      XtAppAddTimeOut(MAIN_APP(state), 
		      TO_C_UNSIGNED_LONG(ms), 
		      (XtTimerCallbackProc)timed_eval, 
		      (XtPointer)code);
      snd_protect(code);
    }
  else mus_misc_error(S_in, "2nd argument should be a procedure of no args", code);
  return(ms);
}

/* color support */

static SND_TAG_TYPE snd_color_tag = 0;

int snd_color_p(SCM obj)
{
  return(SMOB_TYPE_P(obj, snd_color_tag));
}

static SCM g_color_p(SCM obj) 
{
  #define H_color_p "(" S_colorQ " obj) -> #t if obj is a col" STR_OR " object"
  return(TO_SCM_BOOLEAN(COLOR_P(obj)));
}

snd_color *get_snd_color(SCM arg)
{
  if (COLOR_P(arg))
    return((snd_color *)SND_VALUE_OF(arg));
  return(NULL);
}

static scm_sizet free_snd_color(SCM obj)
{
  Colormap cmap;
  Display *dpy;
  snd_color *v = (snd_color *)SND_VALUE_OF(obj);
  dpy = XtDisplay(MAIN_SHELL(state));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  XFreeColors(dpy, cmap, &(v->color), 1, 0);
  free(v);
  return(sizeof(snd_color));
}

static int print_snd_color(SCM obj, SCM port, scm_print_state *pstate)
{
  char *buf = NULL;
  snd_color *v = (snd_color *)SND_VALUE_OF(obj);
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  dpy = XtDisplay(MAIN_SHELL(state));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = v->color;
  XQueryColor(dpy, cmap, &tmp_color);
  mus_snprintf(buf, PRINT_BUFFER_SIZE, "#<col" STR_OR ": (%.2f %.2f %.2f)>",
	  (float)tmp_color.red / 65535.0,
	  (float)tmp_color.green / 65535.0,
	  (float)tmp_color.blue / 65535.0);
  WRITE_STRING(buf, port);
  FREE(buf);
#if HAVE_SCM_REMEMBER_UPTO_HERE
  scm_remember_upto_here(obj);
#endif
  return(1);
}

static SCM g_color2list(SCM obj)
{
  #define H_color2list "(" S_color2list " obj) -> col" STR_OR " rgb values as a list of floats"
  snd_color *v;
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  ASSERT_TYPE(COLOR_P(obj), obj, SCM_ARGn, S_color2list, "a color object"); 
  v = (snd_color *)SND_VALUE_OF(obj);
  dpy = XtDisplay(MAIN_SHELL(state));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = v->color;
  XQueryColor(dpy, cmap, &tmp_color);
  return(scm_return_first(SCM_LIST3(TO_SCM_DOUBLE((float)tmp_color.red / 65535.0),
				    TO_SCM_DOUBLE((float)tmp_color.green / 65535.0),
				    TO_SCM_DOUBLE((float)tmp_color.blue / 65535.0)),
			  obj));
}

static SCM equalp_snd_color(SCM obj1, SCM obj2)
{
  snd_color *v1, *v2;
  v1 = (snd_color *)SND_VALUE_OF(obj1);
  v2 = (snd_color *)SND_VALUE_OF(obj2);
  return(TO_SCM_BOOLEAN(v1->color == v2->color));
}

static SCM g_make_snd_color(SCM r, SCM g, SCM b)
{
  #define H_make_color "(" S_make_color " r g b) -> a col" STR_OR " object with the indicated rgb values"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  snd_color *new_color;
  ASSERT_TYPE(NUMBER_P(r), r, SCM_ARG1, S_make_color, "a number");
  /* someday accept a list as r */
  ASSERT_TYPE(NUMBER_P(g), g, SCM_ARG2, S_make_color, "a number");
  ASSERT_TYPE(NUMBER_P(b), b, SCM_ARG3, S_make_color, "a number");

  /* TODO: check rgb for 0.0 .. 1.0 and (perhaps) send NO_SUCH_COLOR or range-error if outside */

  new_color = (snd_color *)scm_must_malloc(sizeof(snd_color), S_make_color);
  dpy = XtDisplay(MAIN_SHELL(state));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.red = (unsigned short)(65535 * TO_C_DOUBLE(r));
  tmp_color.green = (unsigned short)(65535 * TO_C_DOUBLE(g));
  tmp_color.blue = (unsigned short)(65535 * TO_C_DOUBLE(b));
  if ((XAllocColor(dpy, cmap, &tmp_color)) == 0)
    /* TODO: should this throw NO_SUCH_COLOR? */
    new_color->color = BlackPixel(dpy, DefaultScreen(dpy)); 
  else new_color->color = tmp_color.pixel;
  SND_RETURN_NEWSMOB(snd_color_tag, new_color);
}

SCM pixel2color(COLOR_TYPE pix)
{
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  if (pix == NO_COLOR) return(SCM_BOOL_F);
  dpy = XtDisplay(MAIN_SHELL(state));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = pix;
  XQueryColor(dpy, cmap, &tmp_color);
  return(g_make_snd_color(TO_SCM_DOUBLE((Float)tmp_color.red / 65535.0),
			  TO_SCM_DOUBLE((Float)tmp_color.green / 65535.0),
			  TO_SCM_DOUBLE((Float)tmp_color.blue / 65535.0)));
}

COLOR_TYPE color2pixel(SCM color)
{
  snd_color *v;
  v = TO_SND_COLOR(color); 
  if (v)
    return(v->color);
  return(NO_COLOR);
}

void recolor_everything(GUI_WIDGET w, GUI_POINTER ptr)
{
  Pixel curcol;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == (Pixel)ptr)
	XtVaSetValues(w, XmNbackground, (state->sgx)->basic_color, NULL);
    }
}

void color_unselected_graphs(COLOR_TYPE color)
{
  int i, j;
  chan_info *cp;
  snd_info *sp;
  for (i = 0; i < state->max_sounds; i++)
    {
      sp = (snd_info *)state->sounds[i];
      if (sp)
	{
	  for (j = 0; j < sp->allocated_chans; j++)
	    {
	      cp = sp->chans[j];
	      if ((cp) && ((i != state->selected_sound) || (j != sp->selected_channel)))
		{
		  XtVaSetValues(channel_graph(cp), XmNbackground, color, NULL);
		}
	    }
	}
    }
}

void color_chan_components(COLOR_TYPE color, int which_component)
{
  int i, j;
  chan_info *cp;
  snd_info *sp;
  for (i = 0; i < state->max_sounds; i++)
    {
      sp = (snd_info *)state->sounds[i];
      if (sp)
	{
	  for (j = 0; j < sp->allocated_chans; j++)
	    {
	      cp = sp->chans[j];
	      if (cp)
		{
		  if (which_component == COLOR_POSITION)
		    {
		      XtVaSetValues(channel_sx(cp), XmNbackground, color, NULL);
		      XtVaSetValues(channel_sy(cp), XmNbackground, color, NULL);
		    }
		  else
		    {
		      XtVaSetValues(channel_zy(cp), XmNbackground, color, NULL);
		      XtVaSetValues(channel_zx(cp), XmNbackground, color, NULL);
		    }
		}
	    }
	}
    }
}

void recolor_button(GUI_WIDGET w, GUI_POINTER ptr)
{
  if (XtIsWidget(w))
    {
      if (XmIsPushButton(w))
	XtVaSetValues(w, XmNarmColor, (state->sgx)->pushed_button_color, NULL);
      else
	{
	  if (XmIsToggleButton(w))
	    XtVaSetValues(w, XmNselectColor, (state->sgx)->pushed_button_color, NULL);
	}
    }
}

static SCM g_load_colormap(SCM colors)
{
  #define H_load_colormap "(" S_load_colormap " col" STR_OR "s) uses the vector col" STR_OR "s to set the current col" STR_OR "map"
  int i, len;
  Pixel *xcs;
  snd_color *v = NULL;
  SCM *vdata;
  ASSERT_TYPE(VECTOR_P(colors), colors, SCM_ARGn, S_load_colormap, "a vector of color objects");
  len = VECTOR_LENGTH(colors);
  xcs = (Pixel *)CALLOC(len, sizeof(Pixel));
  vdata = SCM_VELTS(colors);
  for (i = 0; i < len; i++)
    {
      if (COLOR_P(vdata[i]))
	v = TO_SND_COLOR(vdata[i]);
      else 
	{
	  FREE(xcs);
	  mus_misc_error(S_load_colormap, "invalid color:", vdata[i]);
	}
      xcs[i] = v->color;
    }
  x_load_colormap(xcs);
  FREE(xcs);
  return(TO_SCM_INT(len));
}

static SCM g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor ") -> current graph cursor shape"
  return(TO_SCM_INT(in_graph_cursor(state)));
}

#include <X11/cursorfont.h>
static SCM g_set_graph_cursor(SCM curs)
{
  int val;
  ASSERT_TYPE(NUMBER_P(curs), curs, SCM_ARGn, "set-" S_graph_cursor, "a number");
  /* X11/cursorfont.h has various even-numbered glyphs, but the odd numbers are ok, and XC_num_glyphs is a lie */
  /*   if you use too high a number here, Goddamn X dies */
  val = TO_C_INT(curs);
  if ((val >= 0) && (val <= XC_xterm))
    {
      state->Graph_Cursor = val;
      (state->sgx)->graph_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(state)), in_graph_cursor(state));
    }
  else mus_misc_error("set-" S_graph_cursor, "invalid cursor", curs);
  return(curs);
}

#if 0
/* the docs say the XmNorientation field can be set at any time, but it seems to be ignored except at widget creation */
#if (XmVERSION > 1)
#define "set-" S_sounds_horizontal "set-sounds-horizontal"
static SCM g_set_sounds_horizontal(SCM val)
{
  int horizontal = 0;
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_P(val), val, SCM_ARGn, "set-" S_sounds_horizontal, "a boolean");
  horizontal = (NOT_FALSE_P(val));
  ss = get_global_state();
  XtVaSetValues(SOUND_PANE(ss), XmNorientation, (horizontal) ? XmHORIZONTAL : XmVERTICAL, NULL);
  return(TO_SCM_BOOLEAN(horizontal));
}
#endif
#endif

#if HAVE_THEMES

void make_bg(snd_state *ss, unsigned int width, unsigned int height);
static SCM g_make_bg(SCM wid, SCM hgt)
{
  make_bg(get_global_state(), TO_C_INT(wid), TO_C_INT(hgt));
  return(SCM_BOOL_F);
}

#endif

void g_initialize_xgh(snd_state *ss, SCM local_doc)
{
  state = ss;
#if HAVE_GUILE
  snd_color_tag = scm_make_smob_type("col" STR_OR, sizeof(snd_color));
  scm_set_smob_print(snd_color_tag, print_snd_color);
  scm_set_smob_free(snd_color_tag, free_snd_color);
  scm_set_smob_equalp(snd_color_tag, equalp_snd_color);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(snd_color_tag, SCM_FNC g_color2list, 0, 0, 0);
#endif
#endif

  DEFINE_PROC(S_in,            g_in, 2, 0, 0,             H_in);
  DEFINE_PROC(S_make_color,    g_make_snd_color, 3, 0, 0, H_make_color);
  DEFINE_PROC(S_colorQ,        g_color_p, 1, 0, 0,        H_color_p);
  DEFINE_PROC(S_color2list,    g_color2list, 1, 0, 0,     H_color2list);
  DEFINE_PROC(S_load_colormap, g_load_colormap, 1, 0, 0,  H_load_colormap);

  define_procedure_with_setter(S_graph_cursor, SCM_FNC g_graph_cursor, H_graph_cursor,
			       "set-" S_graph_cursor, SCM_FNC g_set_graph_cursor, local_doc, 0, 0, 1, 0);

#if HAVE_THEMES
  DEFINE_PROC("make-bg", g_make_bg, 2, 0, 0, "make background pixmap");
#endif
}
