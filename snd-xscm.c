#include "snd.h"

/* TODO:   user-loaded colormaps need to be added to the Color dialog list, etc
 * TODO    colored marks etc (requires 2 pixels for selected/unselected graphs)?
 * TODO    colormaps applied to lisp or data graph?
 * TODO    option to set the background-colour in the fft/transform-window?
 */

#if HAVE_GUILE

static snd_state *state;

#if HAVE_HTML
static SCM g_html_dir(void) 
{
  #define H_html_dir "(" S_html_dir ") -> location of Snd documentation"
  return(TO_SCM_STRING(html_dir(state)));
}

static SCM g_set_html_dir(SCM val) 
{
  SCM_ASSERT(gh_string_p(val), val, SCM_ARG1, "set-" S_html_dir);
  set_html_dir(state, TO_NEW_C_STRING(val)); 
  return(val);
}
#endif

static SCM g_region_dialog(void) 
{
  #define H_region_dialog "(" S_region_dialog ") starts the region dialog"
  if (snd_regions() > 0) 
    View_Region_Callback(MAIN_PANE(state), (XtPointer)state, NULL);
  return(SCM_BOOL_F);
}

static void timed_eval(XtPointer in_code, XtIntervalId *id)
{
  g_call0((SCM)in_code, "timed callback func");
}

static SCM g_in(SCM ms, SCM code)
{
  #define H_in "(" S_in " msecs thunk) invokes thunk in msecs milliseconds"
  SCM_ASSERT(NUMBER_P(ms), ms, SCM_ARG1, S_in);
  if (procedure_fits(code, 0))
    XtAppAddTimeOut(MAIN_APP(state), 
		    TO_C_UNSIGNED_LONG(ms), 
		    (XtTimerCallbackProc)timed_eval, 
		    (XtPointer)code);
  else scm_throw(MUS_MISC_ERROR,
		 SCM_LIST3(TO_SCM_STRING(S_in),
			   TO_SCM_STRING("2nd argument should be a procedure of no args"),
			   code));
  return(ms);
}

/* color support */

static SND_TAG_TYPE snd_color_tag = 0;

static SCM mark_snd_color(SCM obj)
{
  SND_SETGCMARK(obj);
  return(SCM_BOOL_F);
}

int snd_color_p(SCM obj)
{
  return((SCM_NIMP(obj)) && (SND_SMOB_TYPE(snd_color_tag, obj)));
}

static SCM g_color_p(SCM obj) 
{
  #define H_color_p "(" S_colorQ " obj) -> #t if obj is a col" STR_OR " object"
  return(TO_SCM_BOOLEAN(snd_color_p(obj)));
}

snd_color *get_snd_color(SCM arg)
{
  if (snd_color_p(arg))
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
  FREE(v);
  return(0);
}

static int print_snd_color(SCM obj, SCM port, scm_print_state *pstate)
{
  char *buf = NULL;
  snd_color *v = (snd_color *)SND_VALUE_OF(obj);
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  buf = (char *)CALLOC(128, sizeof(char));
  dpy = XtDisplay(MAIN_SHELL(state));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.pixel = v->color;
  XQueryColor(dpy, cmap, &tmp_color);
  sprintf(buf, "#<col" STR_OR ": (%.2f %.2f %.2f)>",
	  (float)tmp_color.red / 65535.0,
	  (float)tmp_color.green / 65535.0,
	  (float)tmp_color.blue / 65535.0);
  scm_puts(buf, port);
  FREE(buf);
  return(scm_return_first(1, obj));
}

static SCM g_color2list(SCM obj)
{
  #define H_color2list "(" S_color2list " obj) -> col" STR_OR " rgb values as a list of floats"
  snd_color *v;
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
  SCM_ASSERT(snd_color_p(obj), obj, SCM_ARG1, S_color2list); 
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
  SCM_ASSERT(NUMBER_P(r), r, SCM_ARG1, S_make_color);
  /* someday accept a list as r */
  SCM_ASSERT(NUMBER_P(g), g, SCM_ARG2, S_make_color);
  SCM_ASSERT(NUMBER_P(b), b, SCM_ARG3, S_make_color);
  new_color = (snd_color *)CALLOC(1, sizeof(snd_color));
  dpy = XtDisplay(MAIN_SHELL(state));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  tmp_color.red = (int)(65535 * TO_C_DOUBLE(r));
  tmp_color.green = (int)(65535 * TO_C_DOUBLE(g));
  tmp_color.blue = (int)(65535 * TO_C_DOUBLE(b));
  if ((XAllocColor(dpy, cmap, &tmp_color)) == 0)
    new_color->color = BlackPixel(dpy, DefaultScreen(dpy)); 
  else new_color->color = tmp_color.pixel;
  SND_RETURN_NEWSMOB(snd_color_tag, new_color);
}

SCM pixel2color(COLOR_TYPE pix)
{
  Colormap cmap;
  XColor tmp_color;
  Display *dpy;
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
  v = get_snd_color(color); 
  return(v->color);
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
  snd_color *v;
  SCM *vdata;
  SCM_ASSERT(gh_vector_p(colors), colors, SCM_ARG1, S_load_colormap);
  len = gh_vector_length(colors);
  xcs = (Pixel *)CALLOC(len, sizeof(Pixel));
  vdata = SCM_VELTS(colors);
  for (i = 0; i < len; i++)
    {
      if (snd_color_p(vdata[i]))
	v = get_snd_color(vdata[i]);
      else scm_throw(MUS_MISC_ERROR,
		     SCM_LIST3(TO_SCM_STRING(S_load_colormap),
			       TO_SCM_STRING("invalid color:"),
			       vdata[i]));
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

static SCM g_set_graph_cursor(SCM curs)
{
  SCM_ASSERT(NUMBER_P(curs), curs, SCM_ARG1, "set-" S_graph_cursor);
  state->Graph_Cursor = TO_C_INT(curs);
  (state->sgx)->graph_cursor = XCreateFontCursor(XtDisplay(MAIN_SHELL(state)), in_graph_cursor(state));
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
  SCM_ASSERT(BOOLEAN_P(val), val, SCM_ARG1, "set-" S_sounds_horizontal);
  horizontal = (SCM_NFALSEP(val));
  ss = get_global_state();
  XtVaSetValues(SOUND_PANE(ss), XmNorientation, (horizontal) ? XmHORIZONTAL : XmVERTICAL, NULL);
  return((horizontal) ? SCM_BOOL_T : SCM_BOOL_F);
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
  snd_color_tag = scm_make_smob_type("col" STR_OR, sizeof(snd_color));
  scm_set_smob_mark(snd_color_tag, mark_snd_color);
  scm_set_smob_print(snd_color_tag, print_snd_color);
  scm_set_smob_free(snd_color_tag, free_snd_color);
  scm_set_smob_equalp(snd_color_tag, equalp_snd_color);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(snd_color_tag, SCM_FNC g_color2list, 0, 0, 0);
#endif

#if HAVE_HTML
  scm_add_feature("snd-html");
  define_procedure_with_setter(S_html_dir, SCM_FNC g_html_dir, H_html_dir,
			       "set-" S_html_dir, SCM_FNC g_set_html_dir, local_doc, 0, 0, 1, 0);
#endif
  
  DEFINE_PROC(gh_new_procedure0_0(S_region_dialog, g_region_dialog),  H_region_dialog);
  DEFINE_PROC(gh_new_procedure2_0(S_in,            g_in),             H_in);
  DEFINE_PROC(gh_new_procedure3_0(S_make_color,    g_make_snd_color), H_make_color);
  DEFINE_PROC(gh_new_procedure1_0(S_colorQ,        g_color_p),        H_color_p);
  DEFINE_PROC(gh_new_procedure1_0(S_color2list,    g_color2list),     H_color2list);
  DEFINE_PROC(gh_new_procedure1_0(S_load_colormap, g_load_colormap),  H_load_colormap);

  define_procedure_with_setter(S_graph_cursor, SCM_FNC g_graph_cursor, H_graph_cursor,
			       "set-" S_graph_cursor, SCM_FNC g_set_graph_cursor, local_doc, 0, 0, 1, 0);

#if HAVE_THEMES
  DEFINE_PROC(gh_new_procedure("make-bg", SCM_FNC g_make_bg, 2, 0, 0), "make background pixmap");
#endif
}
#endif
