#include "snd.h"

static gint timed_eval(gpointer in_code)
{
  XEN_CALL_0((XEN)in_code, "timed callback func");
  snd_unprotect((XEN)in_code);
  return(0);
}

static XEN g_in(XEN ms, XEN code)
{
  #define H_in "(" S_in " msecs thunk): invoke thunk in msecs milliseconds"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(ms), ms, XEN_ARG_1, S_in, "a number");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code), code, XEN_ARG_2, S_in, "a procedure");
  if (XEN_REQUIRED_ARGS(code) == 0)
    {
      gtk_timeout_add((guint32)XEN_TO_C_INT(ms), timed_eval, (gpointer)code);
      snd_protect(code);
    }
  else XEN_BAD_ARITY_ERROR(S_in, 2, code, "should take no args");
  return(ms);
}


static XEN g_color2list(XEN obj)
{
  #define H_color2list "(" S_color2list " obj): 'obj' rgb values as a list of floats"
  GdkColor *v;
  XEN_ASSERT_TYPE(XEN_PIXEL_P(obj), obj, XEN_ONLY_ARG, S_color2list, "a color"); 
  v = XEN_UNWRAP_PIXEL(obj);
  return(xen_return_first(XEN_LIST_3(C_TO_XEN_DOUBLE((float)(v->red) / 65535.0),
				     C_TO_XEN_DOUBLE((float)(v->green) / 65535.0),
				     C_TO_XEN_DOUBLE((float)(v->blue) / 65535.0)),
			  obj));
}

static XEN g_make_snd_color(XEN r, XEN g, XEN b)
{
  #define H_make_color "(" S_make_color " r g b): return a color object with the indicated rgb values"
  GdkColor gcolor;
  GdkColor *ccolor;
  Float rf, gf, bf;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(r), r, XEN_ARG_1, S_make_color, "a number");
  /* someday accept a list as r */
  XEN_ASSERT_TYPE(XEN_NUMBER_P(g), g, XEN_ARG_2, S_make_color, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(b), b, XEN_ARG_3, S_make_color, "a number");
  rf = check_color_range(S_make_color, r);
  gf = check_color_range(S_make_color, g);
  bf = check_color_range(S_make_color, b);
  gcolor.red = (unsigned short)(65535 * rf);
  gcolor.green = (unsigned short)(65535 * gf);
  gcolor.blue = (unsigned short)(65535 * bf);
  ccolor = gdk_color_copy(&gcolor);
  gdk_rgb_find_color(gdk_colormap_get_system(), ccolor);
  return(XEN_WRAP_PIXEL(ccolor));
}

void recolor_everything(widget_t w, void *ptr)
{
  if (GTK_IS_WIDGET(w)) 
    set_background_and_redraw(w, (GdkColor *)ptr);
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
	      set_background_and_redraw(channel_graph(cp), color);
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
      sp = (snd_info *)(ss->sounds[i]);
      if (sp)
	for (j = 0; j < sp->allocated_chans; j++)
	  {
	    cp = sp->chans[j];
	    if (cp)
	      {
		if (which_component == COLOR_POSITION)
		  {
		    set_background_and_redraw(channel_sx(cp), color);
		    set_background_and_redraw(channel_sy(cp), color);
		  }
		else
		  {
		    set_background_and_redraw(channel_zx(cp), color);
		    set_background_and_redraw(channel_zy(cp), color);
		  }
	      }
	  }
    }
}

static XEN g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor "): current graph cursor shape"
  return(C_TO_SMALL_XEN_INT(in_graph_cursor(get_global_state())));
}

static XEN g_set_graph_cursor(XEN curs)
{
  int val;
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(curs), curs, XEN_ONLY_ARG, S_setB S_graph_cursor, "a number");
  val = XEN_TO_C_INT(curs);
  if ((val >= 0) && (val <= GDK_XTERM))
    {
      ss->Graph_Cursor = val;
      (ss->sgx)->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(ss));
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
