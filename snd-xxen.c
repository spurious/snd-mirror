#include "snd.h"
#include "vct.h"

static void timed_eval(XtPointer in_code, XtIntervalId *id)
{
#if (SCM_DEBUG_TYPING_STRICTNESS != 2)
  XEN_CALL_0((XEN)in_code, "timed callback func");
  snd_unprotect((XEN)in_code);
#endif
}

static XEN g_in(XEN ms, XEN code)
{
  unsigned long time;
  #define H_in "(" S_in " msecs thunk) invokes thunk in msecs milliseconds"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(ms), ms, XEN_ARG_1, S_in, "a number");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code), code, XEN_ARG_2, S_in, "a procedure");
#if (SCM_DEBUG_TYPING_STRICTNESS != 2)
  if (XEN_INTEGER_P(ms))
    time = XEN_TO_C_ULONG(ms);
  else time = (unsigned long)snd_round(XEN_TO_C_DOUBLE(ms));
  if (XEN_REQUIRED_ARGS(code) == 0)
    {
      XtAppAddTimeOut(MAIN_APP(get_global_state()), 
		      time,
		      (XtTimerCallbackProc)timed_eval, 
		      (XtPointer)code);
      snd_protect(code);
    }
  else mus_misc_error(S_in, "2nd argument should be a procedure of no args", code);
#endif
  return(ms);
}

/* color support */

static XEN_OBJECT_TYPE snd_color_tag;

int snd_color_p(XEN obj)
{
  return(XEN_OBJECT_TYPE_P(obj, snd_color_tag));
}

static XEN g_color_p(XEN obj) 
{
  #define H_color_p "(" S_color_p " obj) -> #t if obj is a col" STR_OR " object"
  return(C_TO_XEN_BOOLEAN(COLOR_P(obj)));
}

snd_color *get_snd_color(XEN arg)
{
  if (COLOR_P(arg))
    return((snd_color *)XEN_OBJECT_REF(arg));
  return(NULL);
}

static void snd_color_free(snd_color *v)
{
  Colormap cmap;
  Display *dpy;
  dpy = XtDisplay(MAIN_SHELL(get_global_state()));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  XFreeColors(dpy, cmap, &(v->color), 1, 0);
  free(v);
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(snd_color, free_snd_color, snd_color_free)

static char *snd_color_to_string(snd_color *v)
{
  char *buf = NULL;
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  dpy = XtDisplay(MAIN_SHELL(get_global_state()));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = v->color;
  XQueryColor(dpy, cmap, &tmp_color);
  mus_snprintf(buf, PRINT_BUFFER_SIZE, "#<col" STR_OR ": (%.2f %.2f %.2f)>",
	       (float)tmp_color.red / 65535.0,
	       (float)tmp_color.green / 65535.0,
	       (float)tmp_color.blue / 65535.0);
  return(buf);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(snd_color, print_snd_color, snd_color_to_string)

static XEN g_color2list(XEN obj)
{
  #define H_color2list "(" S_color2list " obj) -> col" STR_OR " rgb values as a list of floats"
  snd_color *v;
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  XEN_ASSERT_TYPE(COLOR_P(obj), obj, XEN_ONLY_ARG, S_color2list, "a color object"); 
  v = (snd_color *)XEN_OBJECT_REF(obj);
  dpy = XtDisplay(MAIN_SHELL(get_global_state()));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = v->color;
  XQueryColor(dpy, cmap, &tmp_color);
  return(xen_return_first(XEN_LIST_3(C_TO_XEN_DOUBLE((float)tmp_color.red / 65535.0),
				     C_TO_XEN_DOUBLE((float)tmp_color.green / 65535.0),
				     C_TO_XEN_DOUBLE((float)tmp_color.blue / 65535.0)),
			  obj));
}

static XEN equalp_snd_color(XEN obj1, XEN obj2)
{
  snd_color *v1, *v2;
  v1 = (snd_color *)XEN_OBJECT_REF(obj1);
  v2 = (snd_color *)XEN_OBJECT_REF(obj2);
  return(C_TO_XEN_BOOLEAN(v1->color == v2->color));
}

static XEN g_make_snd_color(XEN r, XEN g, XEN b)
{
  #define H_make_color "(" S_make_color " r g b) -> a col" STR_OR " object with the indicated rgb values"
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  snd_color *new_color;
  Float rf, gf, bf;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(r), r, XEN_ARG_1, S_make_color, "a number");
  /* someday accept a list as r */
  XEN_ASSERT_TYPE(XEN_NUMBER_P(g), g, XEN_ARG_2, S_make_color, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(b), b, XEN_ARG_3, S_make_color, "a number");
  rf = check_color_range(S_make_color, r);
  gf = check_color_range(S_make_color, g);
  bf = check_color_range(S_make_color, b);
  new_color = (snd_color *)xen_malloc(sizeof(snd_color));
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
  new_color->color = tmp_color.pixel;
  XEN_MAKE_AND_RETURN_OBJECT(snd_color_tag, new_color, 0, free_snd_color);
}

XEN pixel2color(COLOR_TYPE pix)
{
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  dpy = XtDisplay(MAIN_SHELL(get_global_state()));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = pix;
  XQueryColor(dpy, cmap, &tmp_color);
  return(g_make_snd_color(C_TO_XEN_DOUBLE((Float)tmp_color.red / 65535.0),
			  C_TO_XEN_DOUBLE((Float)tmp_color.green / 65535.0),
			  C_TO_XEN_DOUBLE((Float)tmp_color.blue / 65535.0)));
}

COLOR_TYPE color2pixel(XEN color)
{
  snd_color *v;
  snd_state *ss;
  v = TO_SND_COLOR(color); 
  if (v)
    return(v->color);
  ss = get_global_state();
  return(ss->sgx->basic_color);
}

static XEN g_snd_pixel(XEN color)
{
  #define H_snd_pixel "(" S_snd_pixel " color) -> pixel of that color (use with |Pixel)"
  snd_color *v;
  XEN_ASSERT_TYPE(COLOR_P(color), color, XEN_ONLY_ARG, S_snd_pixel, "a Snd color");
  v = TO_SND_COLOR(color); 
  if (v)
    return(XEN_WRAP_PIXEL((unsigned long)(v->color)));
  return(XEN_ZERO);
}

void recolor_everything(GUI_WIDGET w, GUI_POINTER ptr)
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

void color_unselected_graphs(COLOR_TYPE color)
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

void color_chan_components(COLOR_TYPE color, int which_component)
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

void recolor_button(GUI_WIDGET w, GUI_POINTER ptr)
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
  #define H_graph_cursor "(" S_graph_cursor ") -> current graph cursor shape"
  return(C_TO_XEN_INT(in_graph_cursor(get_global_state())));
}

#include <X11/cursorfont.h>
static XEN g_set_graph_cursor(XEN curs)
{
  int val;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(curs), curs, XEN_ONLY_ARG, "set-" S_graph_cursor, "a number");
  /* X11/cursorfont.h has various even-numbered glyphs, but the odd numbers are ok, and XC_num_glyphs is a lie */
  /*   if you use too high a number here, X dies */
  val = XEN_TO_C_INT(curs);
  ss = get_global_state();
  if ((val >= 0) && (val <= XC_xterm))
    {
      ss->Graph_Cursor = val;
      (ss->sgx)->graph_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(ss)), in_graph_cursor(ss));
    }
  else mus_misc_error("set-" S_graph_cursor, "invalid cursor", curs);
  return(curs);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_2(g_in_w, g_in)
XEN_NARGIFY_3(g_make_snd_color_w, g_make_snd_color)
XEN_NARGIFY_1(g_color_p_w, g_color_p)
XEN_NARGIFY_1(g_color2list_w, g_color2list)
XEN_NARGIFY_0(g_graph_cursor_w, g_graph_cursor)
XEN_NARGIFY_1(g_set_graph_cursor_w, g_set_graph_cursor)
#else
#define g_in_w g_in
#define g_make_snd_color_w g_make_snd_color
#define g_color_p_w g_color_p
#define g_color2list_w g_color2list
#define g_graph_cursor_w g_graph_cursor
#define g_set_graph_cursor_w g_set_graph_cursor
#endif

void g_initialize_xgh(void)
{
  snd_color_tag = XEN_MAKE_OBJECT_TYPE("SndCol" STR_OR, sizeof(snd_color));

#if HAVE_GUILE
  scm_set_smob_print(snd_color_tag, print_snd_color);
  scm_set_smob_free(snd_color_tag, free_snd_color);
  scm_set_smob_equalp(snd_color_tag, equalp_snd_color);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(snd_color_tag, XEN_PROCEDURE_CAST g_color2list, 0, 0, 0);
#endif
#endif
#if HAVE_RUBY
  rb_define_method(snd_color_tag, "to_s", XEN_PROCEDURE_CAST print_snd_color, 0);
  rb_define_method(snd_color_tag, "eql?", XEN_PROCEDURE_CAST equalp_snd_color, 1);
#endif

  XEN_DEFINE_PROCEDURE(S_in,            g_in_w, 2, 0, 0,             H_in);
  XEN_DEFINE_PROCEDURE(S_make_color,    g_make_snd_color_w, 3, 0, 0, H_make_color);
  XEN_DEFINE_PROCEDURE(S_color_p,       g_color_p_w, 1, 0, 0,        H_color_p);
  XEN_DEFINE_PROCEDURE(S_color2list,    g_color2list_w, 1, 0, 0,     H_color2list);
  XEN_DEFINE_PROCEDURE(S_snd_pixel,     g_snd_pixel, 1, 0, 0,        H_snd_pixel);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_cursor, g_graph_cursor_w, H_graph_cursor,
				   "set-" S_graph_cursor, g_set_graph_cursor_w,  0, 0, 1, 0);
}
