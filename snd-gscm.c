#include "snd.h"
#include "vct.h"

static snd_state *state;

static SCM g_region_dialog(void) 
{
  #define H_region_dialog "(" S_region_dialog ") starts the region dialog"
  if (snd_regions() > 0) 
    View_Region_Callback(MAIN_PANE(state), (gpointer)state); 
  return(SCM_BOOL_F);
}

static gint timed_eval(gpointer in_code)
{
  CALL0((SCM)in_code, "timed callback func");
  return(0);
}

static SCM g_in(SCM ms, SCM code)
{
  #define H_in "(" S_in " msecs thunk) invokes thunk in msecs milliseconds"
  SCM_ASSERT(NUMBER_P(ms), ms, SCM_ARG1, S_in);
  if (procedure_fits(code, 0))
    gtk_timeout_add((guint32)TO_C_UNSIGNED_LONG(ms), timed_eval, (gpointer)code);
  else mus_misc_error(S_in, "2nd argument should be a procedure of no args", code);
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
  return(SMOB_TYPE_P(obj, snd_color_tag));
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
  snd_color *v = (snd_color *)SND_VALUE_OF(obj);
  gdk_color_free(v->color);
  FREE(v);
  return(0);
}

static int print_snd_color(SCM obj, SCM port, scm_print_state *pstate)
{
  char *buf = NULL;
  snd_color *v = (snd_color *)SND_VALUE_OF(obj);
  buf = (char *)CALLOC(128, sizeof(char));
  mus_snprintf(buf, 128, "#<col" STR_OR ": (%.2f %.2f %.2f)>",
	  (float)(v->color->red) / 65535.0,
	  (float)(v->color->green) / 65535.0,
	  (float)(v->color->blue) / 65535.0);
  scm_puts(buf, port);
  FREE(buf);
  return(scm_return_first(1, obj));
}

static SCM g_color2list(SCM obj)
{
  #define H_color2list "(" S_color2list " obj) -> col" STR_OR " rgb values as a list of floats"
  snd_color *v;
  SCM_ASSERT(snd_color_p(obj), obj, SCM_ARG1, S_color2list); 
  v = (snd_color *)SND_VALUE_OF(obj);
  return(scm_return_first(SCM_LIST3(TO_SCM_DOUBLE((float)(v->color->red) / 65535.0),
				    TO_SCM_DOUBLE((float)(v->color->green) / 65535.0),
				    TO_SCM_DOUBLE((float)(v->color->blue) / 65535.0)),
			  obj));
}

static SCM equalp_snd_color(SCM obj1, SCM obj2)
{
  snd_color *v1, *v2;
  v1 = (snd_color *)SND_VALUE_OF(obj1);
  v2 = (snd_color *)SND_VALUE_OF(obj2);
  return(TO_SCM_BOOLEAN(v1->color->pixel == v2->color->pixel));
}

static SCM g_make_snd_color(SCM r, SCM g, SCM b)
{
  #define H_make_color "(" S_make_color " r g b) -> a col" STR_OR " object with the indicated rgb values"
  snd_color *new_color;
  GdkColor gcolor;
  SCM_ASSERT(NUMBER_P(r), r, SCM_ARG1, S_make_color);
  /* someday accept a list as r */
  SCM_ASSERT(NUMBER_P(g), g, SCM_ARG2, S_make_color);
  SCM_ASSERT(NUMBER_P(b), b, SCM_ARG3, S_make_color);
  new_color = (snd_color *)CALLOC(1, sizeof(snd_color));
  gcolor.red = (unsigned short)(65535 * TO_C_DOUBLE(r));
  gcolor.green = (unsigned short)(65535 * TO_C_DOUBLE(g));
  gcolor.blue = (unsigned short)(65535 * TO_C_DOUBLE(b));
  new_color->color = gdk_color_copy(&gcolor);
  gdk_color_alloc(gdk_colormap_get_system(), new_color->color);
  SND_RETURN_NEWSMOB(snd_color_tag, new_color);
}

SCM pixel2color(COLOR_TYPE pix)
{
  if (pix == NO_COLOR) return(SCM_BOOL_F);
  return(g_make_snd_color(TO_SCM_DOUBLE((Float)(pix->red) / 65535.0),
			  TO_SCM_DOUBLE((Float)(pix->green) / 65535.0),
			  TO_SCM_DOUBLE((Float)(pix->blue) / 65535.0)));
}

COLOR_TYPE color2pixel(SCM color)
{
  snd_color *v;
  v = get_snd_color(color);
  if (v)
    return(v->color);
  return(NO_COLOR);
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

static SCM g_load_colormap(SCM colors)
{
  #define H_load_colormap "(" S_load_colormap " col" STR_OR "s) uses the vector col" STR_OR "s to set the current col" STR_OR "map"
  int i, len;
  GdkColor **xcs;
  snd_color *v = NULL;
  SCM *vdata;
  SCM_ASSERT(VECTOR_P(colors), colors, SCM_ARG1, S_load_colormap);
  len = VECTOR_LENGTH(colors);
  xcs = (GdkColor **)CALLOC(len, sizeof(GdkColor *));
  vdata = SCM_VELTS(colors);
  for (i = 0; i < len; i++)
    {
      if (snd_color_p(vdata[i]))
	v = get_snd_color(vdata[i]);
      else 
	{
	  FREE(xcs);
	  mus_misc_error(S_load_colormap, "invalid color:", vdata[i]);
	}
      xcs[i] = v->color;
    }
  /* TODO: colormaps? x_load_colormap(xcs); */
  FREE(xcs);
  return(TO_SCM_INT(len));
}

static SCM g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor ") -> current graph cursor shape"
  return(TO_SMALL_SCM_INT(in_graph_cursor(state)));
}

static SCM g_set_graph_cursor(SCM curs)
{
  int val;
  SCM_ASSERT(NUMBER_P(curs), curs, SCM_ARG1, "set-" S_graph_cursor);
  /* X11/cursorfont.h has various even-numbered glyphs, but the odd numbers are ok, and XC_num_glyphs is a lie */
  /*   if you use too high a number here, Goddamn X dies */
  /* gdk/gdkcursors.h is just a capitalization of the original so I assume it has the same great features */
  val = TO_C_INT(curs);
  if ((val >= 0) && (val <= GDK_XTERM))
    {
      state->Graph_Cursor = val;
      (state->sgx)->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(state));
    }
  else mus_misc_error("set-" S_graph_cursor, "invalid cursor", curs);
  return(curs);
}

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

  DEFINE_PROC(S_region_dialog, g_region_dialog, 0, 0, 0,  H_region_dialog);
  DEFINE_PROC(S_in,            g_in, 2, 0, 0,             H_in);
  DEFINE_PROC(S_make_color,    g_make_snd_color, 3, 0, 0, H_make_color);
  DEFINE_PROC(S_colorQ,        g_color_p, 1, 0, 0,        H_color_p);
  DEFINE_PROC(S_color2list,    g_color2list, 1, 0, 0,     H_color2list);
  DEFINE_PROC(S_load_colormap, g_load_colormap, 1, 0, 0,  H_load_colormap);

  define_procedure_with_setter(S_graph_cursor, SCM_FNC g_graph_cursor, H_graph_cursor,
			       "set-" S_graph_cursor, SCM_FNC g_set_graph_cursor, local_doc, 0, 0, 1, 0);
  
}
