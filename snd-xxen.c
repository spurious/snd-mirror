#include "snd.h"

#define NO_SUCH_COLOR XEN_ERROR_TYPE("no-such-color")

static void timed_eval(XtPointer in_code, XtIntervalId *id)
{
  XEN_CALL_0((XEN)in_code, "timed callback func");
  snd_unprotect((XEN)in_code);
}

static XEN g_in(XEN ms, XEN code)
{
  #define H_in "(" S_in " msecs thunk): invoke thunk in msecs milliseconds (named call_in in Ruby)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(ms), ms, XEN_ARG_1, S_in, "a number");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code), code, XEN_ARG_2, S_in, "a procedure");
  if (XEN_REQUIRED_ARGS(code) == 0)
    {
      XtAppAddTimeOut(MAIN_APP(get_global_state()), 
		      (unsigned long)XEN_TO_C_INT(ms),
		      (XtTimerCallbackProc)timed_eval, 
		      (XtPointer)code);
      snd_protect(code);
    }
  else XEN_BAD_ARITY_ERROR(S_in, 2, code, "should take no args");
  return(ms);
}

static XEN g_color2list(XEN obj)
{
  #define H_color2list "(" S_color2list " obj): 'obj' rgb values as a list of floats"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  XEN_ASSERT_TYPE(XEN_PIXEL_P(obj), obj, XEN_ONLY_ARG, S_color2list, "a color"); 
  dpy = XtDisplay(MAIN_SHELL(get_global_state()));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = XEN_UNWRAP_PIXEL(obj);
  XQueryColor(dpy, cmap, &tmp_color);
  return(xen_return_first(XEN_LIST_3(C_TO_XEN_DOUBLE((float)tmp_color.red / 65535.0),
				     C_TO_XEN_DOUBLE((float)tmp_color.green / 65535.0),
				     C_TO_XEN_DOUBLE((float)tmp_color.blue / 65535.0)),
			  obj));
}

static XEN g_make_snd_color(XEN r, XEN g, XEN b)
{
  #define H_make_color "(" S_make_color " r g b): return a color object with the indicated rgb values"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  Float rf, gf, bf;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(r), r, XEN_ARG_1, S_make_color, "a number");
  /* someday accept a list as r */
  XEN_ASSERT_TYPE(XEN_NUMBER_P(g), g, XEN_ARG_2, S_make_color, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(b), b, XEN_ARG_3, S_make_color, "a number");
  rf = check_color_range(S_make_color, r);
  gf = check_color_range(S_make_color, g);
  bf = check_color_range(S_make_color, b);
  dpy = XtDisplay(MAIN_SHELL(get_global_state()));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.red = (unsigned short)(65535 * rf); 
  tmp_color.green = (unsigned short)(65535 * gf);
  tmp_color.blue = (unsigned short)(65535 * bf);
  if ((XAllocColor(dpy, cmap, &tmp_color)) == 0)
    XEN_ERROR(NO_SUCH_COLOR,
	      XEN_LIST_2(C_TO_XEN_STRING(S_make_color),
			 XEN_LIST_3(r, g, b)));
  return(XEN_WRAP_PIXEL(tmp_color.pixel));
}

void recolor_everything(widget_t w, void *ptr)
{
  Pixel curcol;
  snd_state *ss;
  if (XtIsWidget(w))
    {
      XtVaGetValues(w, XmNbackground, &curcol, NULL);
      if (curcol == (Pixel)ptr)
	{
	  ss = get_global_state();
	  XtVaSetValues(w, XmNbackground, (ss->sgx)->basic_color, NULL);
	}
    }
}

void color_unselected_graphs(color_t color)
{
  int i, j;
  chan_info *cp;
  snd_info *sp;
  snd_state *ss;
  ss = get_global_state();
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = (snd_info *)(ss->sounds[i]);
      if (sp)
	for (j = 0; j < sp->allocated_chans; j++)
	  {
	    cp = sp->chans[j];
	    if ((cp) && ((i != ss->selected_sound) || (j != sp->selected_channel)))
	      XtVaSetValues(channel_graph(cp), XmNbackground, color, NULL);
	  }
    }
}

void color_chan_components(color_t color, int which_component)
{
  int i, j;
  chan_info *cp;
  snd_info *sp;
  snd_state *ss;
  ss = get_global_state();
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = (snd_info *)ss->sounds[i];
      if (sp)
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

void recolor_button(widget_t w, void *ptr)
{
  snd_state *ss;
  ss = get_global_state();
  if (XtIsWidget(w))
    {
      if (XmIsPushButton(w))
	XtVaSetValues(w, XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      else
	{
	  if (XmIsToggleButton(w))
	    XtVaSetValues(w, XmNselectColor, (ss->sgx)->pushed_button_color, NULL);
	}
    }
}

static XEN g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor "): current graph cursor shape"
  return(C_TO_XEN_INT(in_graph_cursor(get_global_state())));
}

#include <X11/cursorfont.h>
static XEN g_set_graph_cursor(XEN curs)
{
  int val;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(curs), curs, XEN_ONLY_ARG, S_setB S_graph_cursor, "an integer");
  /* X11/cursorfont.h has various even-numbered glyphs, but the odd numbers are ok, and XC_num_glyphs is a lie */
  /*   if you use too high a number here, X dies */
  val = XEN_TO_C_INT(curs);
  ss = get_global_state();
  if ((val >= 0) && (val <= XC_xterm))
    {
      ss->Graph_Cursor = val;
      (ss->sgx)->graph_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), in_graph_cursor(ss));
    }
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_graph_cursor, 1, curs, "~A: invalid cursor");
  return(curs);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_2(g_in_w, g_in)
XEN_NARGIFY_3(g_make_snd_color_w, g_make_snd_color)
XEN_NARGIFY_1(g_color2list_w, g_color2list)
XEN_NARGIFY_0(g_graph_cursor_w, g_graph_cursor)
XEN_NARGIFY_1(g_set_graph_cursor_w, g_set_graph_cursor)
#else
#define g_in_w g_in
#define g_make_snd_color_w g_make_snd_color
#define g_color2list_w g_color2list
#define g_graph_cursor_w g_graph_cursor
#define g_set_graph_cursor_w g_set_graph_cursor
#endif

void g_init_gxen(void)
{
  XEN_DEFINE_PROCEDURE(S_in,            g_in_w, 2, 0, 0,             H_in);
  XEN_DEFINE_PROCEDURE(S_make_color,    g_make_snd_color_w, 3, 0, 0, H_make_color);
  XEN_DEFINE_PROCEDURE(S_color2list,    g_color2list_w, 1, 0, 0,     H_color2list);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_cursor, g_graph_cursor_w, H_graph_cursor,
				   S_setB S_graph_cursor, g_set_graph_cursor_w,  0, 0, 1, 0);
}
