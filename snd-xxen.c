#include "snd.h"

static void timed_eval(XtPointer in_code, XtIntervalId *id)
{
#if HAVE_EXTENSION_LANGUAGE
  /* #if probably needed on 64-bit machines */
  XEN lst = (XEN)in_code;
  XEN_CALL_0(XEN_CADR(lst), "timed callback func");
  snd_unprotect_at(XEN_TO_C_INT(XEN_CAR(lst)));
#endif
}

static XEN g_in(XEN ms, XEN code)
{
  #define H_in "(" S_in " msecs thunk): invoke thunk in msecs milliseconds (named call_in in Ruby)"
#if HAVE_EXTENSION_LANGUAGE
  XEN_ASSERT_TYPE(XEN_NUMBER_P(ms), ms, XEN_ARG_1, S_in, "a number");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code), code, XEN_ARG_2, S_in, "a procedure");
  if (XEN_REQUIRED_ARGS_OK(code, 0))
    {
      int secs;
      secs = XEN_TO_C_INT(ms);
      if (secs < 0) 
	XEN_OUT_OF_RANGE_ERROR(S_in, XEN_ARG_1, ms, "a positive integer");
      else
	{
	  XEN lst;
	  lst = XEN_LIST_2(XEN_FALSE, code);
	  XEN_LIST_SET(lst, 0, C_TO_XEN_INT(snd_protect(lst)));
	  XtAppAddTimeOut(MAIN_APP(ss), 
			  (unsigned long)secs,
			  (XtTimerCallbackProc)timed_eval, 
			  (XtPointer)lst);
	  /* the "code" arg can still be something misleading like an applicable smob */
	  /*   there's a way to catch that in snd-run.c line 129, but I'm not sure it's the "right thing" here */
	}
    }
  else XEN_BAD_ARITY_ERROR(S_in, 2, code, "should take no args");
#endif
  return(ms);
}

static XEN g_color_to_list(XEN obj)
{
  #define H_color_to_list "(" S_color_to_list " obj): 'obj' rgb values as a list of floats"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  XEN_ASSERT_TYPE(XEN_PIXEL_P(obj), obj, XEN_ONLY_ARG, S_color_to_list, "a color"); 
  dpy = XtDisplay(MAIN_SHELL(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = XEN_UNWRAP_PIXEL(obj);
  XQueryColor(dpy, cmap, &tmp_color);
  return(xen_return_first(XEN_LIST_3(C_TO_XEN_DOUBLE((float)tmp_color.red / 65535.0),
				     C_TO_XEN_DOUBLE((float)tmp_color.green / 65535.0),
				     C_TO_XEN_DOUBLE((float)tmp_color.blue / 65535.0)),
			  obj));
}

static XEN g_make_color(XEN r, XEN g, XEN b)
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
  dpy = XtDisplay(MAIN_SHELL(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.red = (unsigned short)(65535 * rf); 
  tmp_color.green = (unsigned short)(65535 * gf);
  tmp_color.blue = (unsigned short)(65535 * bf);
  if ((XAllocColor(dpy, cmap, &tmp_color)) == 0)
    XEN_ERROR(XEN_ERROR_TYPE("no-such-color"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_make_color),
			 XEN_LIST_3(r, g, b)));
  return(XEN_WRAP_PIXEL(tmp_color.pixel));
}

void color_unselected_graphs(color_t color)
{
  int i, j;
  for (i = 0; i < ss->max_sounds; i++)
    {
      chan_info *cp;
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse != SOUND_WRAPPER))
	for (j = 0; j < sp->allocated_chans; j++)
	  {
	    cp = sp->chans[j];
	    if ((cp) && ((i != ss->selected_sound) || (j != sp->selected_channel)))
	      XtVaSetValues(channel_graph(cp), XmNbackground, color, NULL);
	  }
    }
}

void color_chan_components(color_t color, slider_choice_t which_component)
{
  int i, j;
  for (i = 0; i < ss->max_sounds; i++)
    {
      chan_info *cp;
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse != SOUND_WRAPPER))
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

static XEN g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor "): current graph cursor shape"
  return(C_TO_XEN_INT(in_graph_cursor(ss)));
}

#include <X11/cursorfont.h>
static XEN g_set_graph_cursor(XEN curs)
{
  int val;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(curs), curs, XEN_ONLY_ARG, S_setB S_graph_cursor, "an integer");
  /* X11/cursorfont.h has various even-numbered glyphs, but the odd numbers are ok, and XC_num_glyphs is a lie */
  /*   if you use too high a number here, X dies */
  val = XEN_TO_C_INT(curs);
  if ((val >= 0) && (val <= XC_xterm))
    {
      ss->Graph_Cursor = val;
      ss->sgx->graph_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), in_graph_cursor(ss));
    }
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_graph_cursor, 1, curs, "~A: invalid cursor");
  return(curs);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_2(g_in_w, g_in)
XEN_NARGIFY_3(g_make_color_w, g_make_color)
XEN_NARGIFY_1(g_color_to_list_w, g_color_to_list)
XEN_NARGIFY_0(g_graph_cursor_w, g_graph_cursor)
XEN_NARGIFY_1(g_set_graph_cursor_w, g_set_graph_cursor)
#else
#define g_in_w g_in
#define g_make_color_w g_make_color
#define g_color_to_list_w g_color_to_list
#define g_graph_cursor_w g_graph_cursor
#define g_set_graph_cursor_w g_set_graph_cursor
#endif

void g_init_gxen(void)
{
  XEN_DEFINE_PROCEDURE(S_in,            g_in_w,             2, 0, 0, H_in);
  XEN_DEFINE_PROCEDURE(S_make_color,    g_make_color_w,     3, 0, 0, H_make_color);
  XEN_DEFINE_PROCEDURE(S_color_to_list, g_color_to_list_w,  1, 0, 0, H_color_to_list);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_cursor, g_graph_cursor_w, H_graph_cursor,
				   S_setB S_graph_cursor, g_set_graph_cursor_w,  0, 0, 1, 0);
}
