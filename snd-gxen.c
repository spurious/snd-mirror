#include "snd.h"
#include "vct.h"

static snd_state *state;

static gint timed_eval(gpointer in_code)
{
  XEN_CALL_0((XEN)in_code, "timed callback func");
  snd_unprotect((XEN)in_code);
  return(0);
}

static XEN g_in(XEN ms, XEN code)
{
  #define H_in "(" S_in " msecs thunk) invokes thunk in msecs milliseconds"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(ms), ms, XEN_ARG_1, S_in, "a number");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(code), code, XEN_ARG_2, S_in, "a procedure");
  if (XEN_REQUIRED_ARGS(code) == 0)
    {
      gtk_timeout_add((guint32)XEN_TO_C_UNSIGNED_LONG(ms), timed_eval, (gpointer)code);
      snd_protect(code);
    }
  else mus_misc_error(S_in, "2nd argument should be a procedure of no args", code);
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
  gdk_color_free(v->color);
  free(v);
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(snd_color, free_snd_color, snd_color_free)

static char *snd_color_to_string(snd_color *v)
{
  char *buf = NULL;
  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(buf, PRINT_BUFFER_SIZE, "#<col" STR_OR ": (%.2f %.2f %.2f)>",
	       (float)(v->color->red) / 65535.0,
	       (float)(v->color->green) / 65535.0,
	       (float)(v->color->blue) / 65535.0);
  return(buf);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(snd_color, print_snd_color, snd_color_to_string)

static XEN g_color2list(XEN obj)
{
  #define H_color2list "(" S_color2list " obj) -> col" STR_OR " rgb values as a list of floats"
  snd_color *v;
  XEN_ASSERT_TYPE(COLOR_P(obj), obj, XEN_ONLY_ARG, S_color2list, "a color object"); 
  v = (snd_color *)XEN_OBJECT_REF(obj);
  return(xen_return_first(XEN_LIST_3(C_TO_XEN_DOUBLE((float)(v->color->red) / 65535.0),
				    C_TO_XEN_DOUBLE((float)(v->color->green) / 65535.0),
				    C_TO_XEN_DOUBLE((float)(v->color->blue) / 65535.0)),
			  obj));
}

static XEN equalp_snd_color(XEN obj1, XEN obj2)
{
  snd_color *v1, *v2;
  v1 = (snd_color *)XEN_OBJECT_REF(obj1);
  v2 = (snd_color *)XEN_OBJECT_REF(obj2);
  return(C_TO_XEN_BOOLEAN(v1->color->pixel == v2->color->pixel));
}

static XEN g_make_snd_color(XEN r, XEN g, XEN b)
{
  #define H_make_color "(" S_make_color " r g b) -> a col" STR_OR " object with the indicated rgb values"
  snd_color *new_color;
  GdkColor gcolor;
  Float rf, gf, bf;
  gboolean rtn;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(r), r, XEN_ARG_1, S_make_color, "a number");
  /* someday accept a list as r */
  XEN_ASSERT_TYPE(XEN_NUMBER_P(g), g, XEN_ARG_2, S_make_color, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(b), b, XEN_ARG_3, S_make_color, "a number");
  rf = check_color_range(S_make_color, r);
  gf = check_color_range(S_make_color, g);
  bf = check_color_range(S_make_color, b);
  new_color = (snd_color *)xen_malloc(sizeof(snd_color));
  gcolor.red = (unsigned short)(65535 * rf);
  gcolor.green = (unsigned short)(65535 * gf);
  gcolor.blue = (unsigned short)(65535 * bf);
  new_color->color = gdk_color_copy(&gcolor);
  rtn = gdk_color_alloc(gdk_colormap_get_system(), new_color->color);
  if (rtn == FALSE)
    XEN_ERROR(NO_SUCH_COLOR,
	  XEN_LIST_2(C_TO_XEN_STRING(S_make_color),
		    XEN_LIST_3(r, g, b)));
  XEN_MAKE_AND_RETURN_OBJECT(snd_color_tag, new_color, 0, free_snd_color);
}

XEN pixel2color(COLOR_TYPE pix)
{
  return(g_make_snd_color(C_TO_XEN_DOUBLE((Float)(pix->red) / 65535.0),
			  C_TO_XEN_DOUBLE((Float)(pix->green) / 65535.0),
			  C_TO_XEN_DOUBLE((Float)(pix->blue) / 65535.0)));
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

void recolor_everything(GUI_WIDGET w, GUI_POINTER ptr)
{
  if (GTK_IS_WIDGET(w)) 
    set_background_and_redraw(w, (GdkColor *)ptr);
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
	for (j = 0; j < sp->allocated_chans; j++)
	  {
	    cp = sp->chans[j];
	    if ((cp) && ((i != state->selected_sound) || (j != sp->selected_channel)))
	      set_background_and_redraw(channel_graph(cp), color);
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

void recolor_button(GUI_WIDGET w, GUI_POINTER ptr)
{
  if ((GTK_IS_WIDGET(w)) && (GTK_IS_BUTTON(w)))
    set_pushed_button_colors(w, state);
}

static XEN g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor ") -> current graph cursor shape"
  return(C_TO_SMALL_XEN_INT(in_graph_cursor(state)));
}

static XEN g_set_graph_cursor(XEN curs)
{
  int val;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(curs), curs, XEN_ONLY_ARG, "set-" S_graph_cursor, "a number");
  /* X11/cursorfont.h has various even-numbered glyphs, but the odd numbers are ok, and XC_num_glyphs is a lie */
  /*   if you use too high a number here, Goddamn X dies */
  /* gdk/gdkcursors.h is just a capitalization of the original so I assume it has the same great features */
  val = XEN_TO_C_INT(curs);
  if ((val >= 0) && (val <= GDK_XTERM))
    {
      state->Graph_Cursor = val;
      (state->sgx)->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(state));
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

void g_initialize_xgh(snd_state *ss)
{
  state = ss;
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

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_graph_cursor, g_graph_cursor_w, H_graph_cursor,
			       "set-" S_graph_cursor, g_set_graph_cursor_w,  0, 0, 1, 0);
  
}
