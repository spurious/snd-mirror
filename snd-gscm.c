#include "snd.h"

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
    View_Region_Callback(MAIN_PANE(state), (gpointer)state); 
  return(SCM_BOOL_F);
}

static gint timed_eval(gpointer in_code)
{
  g_call0((SCM)in_code);
  return(0);
}

static SCM g_in(SCM ms, SCM code)
{
  #define H_in "(" S_in " msecs thunk) invokes thunk in msecs milliseconds"
  SCM_ASSERT(gh_number_p(ms), ms, SCM_ARG1, S_in);
  SCM_ASSERT(gh_procedure_p(code), code, SCM_ARG2, S_in);
  gtk_timeout_add((guint32)TO_C_UNSIGNED_LONG(ms), timed_eval, (gpointer)code);
  return(ms);
}


/* color support */

static SND_TAG_TYPE snd_color_tag = 0;

typedef struct {
  GdkColor *color;
} snd_color;

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

static snd_color *get_snd_color(SCM arg)
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
  sprintf(buf, "#<col" STR_OR ": (%.2f %.2f %.2f)>",
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(r)), r, SCM_ARG1, S_make_color);
  /* someday accept a list as r */
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(g)), g, SCM_ARG2, S_make_color);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(b)), b, SCM_ARG3, S_make_color);
  new_color = (snd_color *)CALLOC(1, sizeof(snd_color));
  gcolor.red = (unsigned short)(65535 * TO_C_DOUBLE(r));
  gcolor.green = (unsigned short)(65535 * TO_C_DOUBLE(g));
  gcolor.blue = (unsigned short)(65535 * TO_C_DOUBLE(b));
  new_color->color = gdk_color_copy(&gcolor);
  gdk_color_alloc(gdk_colormap_get_system(), new_color->color);
  SND_RETURN_NEWSMOB(snd_color_tag, new_color);
}

SCM pixel2color(GdkColor *pix)
{
  return(g_make_snd_color(TO_SCM_DOUBLE((Float)(pix->red) / 65535.0),
			  TO_SCM_DOUBLE((Float)(pix->green) / 65535.0),
			  TO_SCM_DOUBLE((Float)(pix->blue) / 65535.0)));
}

GdkColor *color2pixel(SCM color)
{
  snd_color *v;
  v = get_snd_color(color); 
  return(v->color);
}

#if (!(HAVE_NEW_SMOB))
static scm_smobfuns snd_color_smobfuns = {
  &mark_snd_color,
  &free_snd_color,
  &print_snd_color,
  &equalp_snd_color};
#endif

static void recolor_everything(GtkWidget *w, gpointer ptr)
{
  if (GTK_IS_WIDGET(w)) 
    set_background_and_redraw(w, (GdkColor *)ptr);
}

static SCM g_set_basic_color (SCM color) 
{
  snd_color *v; 
  GdkColor *old_color;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_basic_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      old_color = (state->sgx)->basic_color;
      (state->sgx)->basic_color = v->color; 
      map_over_children(MAIN_SHELL(state), recolor_everything, (gpointer)old_color);
    }
  return(color);
}

static SCM g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color ") -> Snd's basic col" STR_OR
  return(pixel2color((state->sgx)->basic_color));
}

static void color_unselected_graphs(GdkColor *color)
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

#define COLOR_POSITION 0
#define COLOR_ZOOM 1

static void color_chan_components(GdkColor *color, int which_component)
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

static SCM g_set_data_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_data_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_data(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_data_color(void) 
{
  #define H_data_color "(" S_data_color ") -> col" STR_OR " used to draw unselected data"
  return(pixel2color((state->sgx)->data_color));
}

static SCM g_set_selected_data_color (SCM color)
{
  snd_color *v; 
  chan_info *cp;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_selected_data_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_selected_data(state, v->color);
      cp = selected_channel(state);
      if (cp) 
	{
	  color_selected_data(state, v->color);
	  update_graph(cp, NULL);
	}
    }
  return(color);
}

static SCM g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color ") -> col" STR_OR " used for selected data"
  return(pixel2color((state->sgx)->selected_data_color));
}

static SCM g_set_graph_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_graph_color);
  v = get_snd_color(color);
  if (v) 
    {
      color_graph(state, v->color);
      color_unselected_graphs(v->color);
    }
  return(color);
}

static SCM g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color ") -> background col" STR_OR " used for unselected data"
  return(pixel2color((state->sgx)->graph_color));
}

static SCM g_set_selected_graph_color (SCM color) 
{
  snd_color *v; 
  chan_info *cp;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_selected_graph_color);
  v = get_snd_color(color); 
  if (v) 
    {
      color_selected_graph(state, v->color);
      cp = selected_channel(state);
      if (cp) 
	set_background_and_redraw(channel_graph(cp), v->color);
    }
  return(color);
}

static SCM g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color ") -> background col" STR_OR " of selected data"
  return(pixel2color((state->sgx)->selected_graph_color));
}

static SCM g_set_cursor_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_cursor_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_cursor(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color ") -> cursor col" STR_OR
  return(pixel2color((state->sgx)->cursor_color));
}

static SCM g_set_selection_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_selection_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_selection(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color ") -> selection col" STR_OR
  return(pixel2color((state->sgx)->selection_color));
}

static SCM g_set_highlight_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_highlight_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->highlight_color = v->color; 
  return(color);
}

static SCM g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color ") -> col" STR_OR " of highlighted text or buttons"
  return(pixel2color((state->sgx)->highlight_color));
}

static SCM g_set_mark_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_mark_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_marks(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color ") -> mark col" STR_OR
  return(pixel2color((state->sgx)->mark_color));
}

static SCM g_set_zoom_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_zoom_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->zoom_color = v->color; 
      color_chan_components(v->color, COLOR_ZOOM);
    }
  return(color);
}

static SCM g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color ") -> col" STR_OR " of zoom sliders"
  return(pixel2color((state->sgx)->zoom_color));
}

static SCM g_set_position_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_position_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->position_color = v->color; 
      color_chan_components(v->color, COLOR_POSITION);
    }
  return(color);
}

static SCM g_position_color(void) 
{
  #define H_position_color "(" S_position_color ") -> col" STR_OR " of position sliders"
  return(pixel2color((state->sgx)->position_color));
}

static SCM g_set_listener_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_listener_color); 
  v = get_snd_color(color);
  if (v) color_listener(v->color);
  return(color);
}

static SCM g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color ") -> background col" STR_OR " of the lisp listener"
  return(pixel2color((state->sgx)->listener_color));
}

static SCM g_set_listener_text_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_listener_text_color); 
  v = get_snd_color(color);
  if (v) color_listener_text(v->color);
  return(color);
}

static SCM g_listener_text_color(void) 
{
  #define H_listener_text_color "(" S_listener_text_color ") -> text col" STR_OR " in the lisp listener"
  return(pixel2color((state->sgx)->listener_text_color));
}

static SCM g_set_enved_waveform_color (SCM color) 
{
  snd_color *v;
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_enved_waveform_color); 
  v = get_snd_color(color); 
  if (v) color_enved_waveform(v->color);
  return(color);
}

static SCM g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color ") -> col" STR_OR " of the envelope editor wave display"
  return(pixel2color((state->sgx)->enved_waveform_color));
}

static SCM g_set_filter_waveform_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_filter_waveform_color);
  v = get_snd_color(color);
  if (v) color_filter_waveform(state, v->color);
  return(color);
}

static SCM g_filter_waveform_color(void) 
{
  #define H_filter_waveform_color "(" S_filter_waveform_color ") -> col" STR_OR " of the filter waveform"
  return(pixel2color((state->sgx)->filter_waveform_color));
}

static SCM g_set_mix_color (SCM arg1, SCM arg2) 
{
  snd_color *v; 
  SCM color, mix_id = SCM_UNDEFINED;
  if (SCM_UNBNDP(arg2))
    color = arg1;
  else
    {
      color = arg2;
      mix_id = arg1;
    }
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_mix_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      if (gh_number_p(mix_id))
	color_one_mix_from_id(TO_SMALL_C_INT(mix_id), v->color);
      else set_mix_color(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_mix_color(SCM mix_id) 
{
  #define H_mix_color "(" S_mix_color ") -> col" STR_OR " of mix consoles"
  if (gh_number_p(mix_id))
    return(pixel2color(mix_to_color_from_id(TO_SMALL_C_INT(mix_id))));
  return(pixel2color((state->sgx)->mix_color));
}

static SCM g_set_selected_mix_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_selected_mix_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      set_selected_mix_color(state, v->color);
      map_over_chans(state, update_graph, NULL);
    }
  return(color);
}

static SCM g_selected_mix_color(void) 
{
  #define H_selected_mix_color "(" S_selected_mix_color ") -> col" STR_OR " of the currently selected mix"
  return(pixel2color((state->sgx)->selected_mix_color));
}


static void recolor_button(GtkWidget *w, gpointer ptr)
{
  if ((GTK_IS_WIDGET(w)) && (GTK_IS_BUTTON(w)))
    set_pushed_button_colors(w, state);
}

static SCM g_set_pushed_button_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_pushed_button_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->pushed_button_color = v->color;
      map_over_children(MAIN_SHELL(state), recolor_button, NULL);
    }
  return(color);
}

static SCM g_pushed_button_color(void) 
{
  #define H_pushed_button_color "(" S_pushed_button_color ") -> col" STR_OR " of a pushed button"
  return(pixel2color((state->sgx)->pushed_button_color));
}

static SCM g_set_text_focus_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_text_focus_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->text_focus_color = v->color;
  return(color);
}

static SCM g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color ") -> col" STR_OR " used to show a text field has focus"
  return(pixel2color((state->sgx)->text_focus_color));
}

static SCM g_set_sash_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color), color, SCM_ARG1, "set-" S_sash_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->sash_color = v->color;
  return(color);
}

static SCM g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color ") -> col" STR_OR " used to draw paned window sashes"
  return(pixel2color((state->sgx)->sash_color));
}

static SCM g_load_colormap(SCM colors)
{
  #define H_load_colormap "(" S_load_colormap " col" STR_OR "s) uses the vector col" STR_OR "s to set the current col" STR_OR "map"
  int i, len;
  GdkColor **xcs;
  snd_color *v;
  SCM_ASSERT((gh_vector_p(colors)), colors, SCM_ARG1, S_load_colormap);
  len = gh_vector_length(colors);
  xcs = (GdkColor **)CALLOC(len, sizeof(GdkColor *));
  for (i = 0; i < len; i++)
    {
      v = get_snd_color(gh_vector_ref(colors, TO_SCM_INT(i)));
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(curs)), curs, SCM_ARG1, "set-" S_graph_cursor);
  state->Graph_Cursor = TO_C_INT(curs);
  (state->sgx)->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(state));
  return(curs);
}

void g_initialize_xgh(snd_state *ss, SCM local_doc)
{
  state = ss;
#if HAVE_NEW_SMOB
  snd_color_tag = scm_make_smob_type("col" STR_OR, sizeof(snd_color));
  scm_set_smob_mark(snd_color_tag, mark_snd_color);
  scm_set_smob_print(snd_color_tag, print_snd_color);
  scm_set_smob_free(snd_color_tag, free_snd_color);
  scm_set_smob_equalp(snd_color_tag, equalp_snd_color);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(snd_color_tag, SCM_FNC g_color2list, 0, 0, 0);
#endif
#else
  snd_color_tag = scm_newsmob(&snd_color_smobfuns);
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

  define_procedure_with_setter(S_basic_color, SCM_FNC g_basic_color, H_basic_color,
			       "set-" S_basic_color, SCM_FNC g_set_basic_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_zoom_color, SCM_FNC g_zoom_color, H_zoom_color,
			       "set-" S_zoom_color, SCM_FNC g_set_zoom_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_position_color, SCM_FNC g_position_color, H_position_color,
			       "set-" S_position_color, SCM_FNC g_set_position_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_mark_color, SCM_FNC g_mark_color, H_mark_color,
			       "set-" S_mark_color, SCM_FNC g_set_mark_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_listener_color, SCM_FNC g_listener_color, H_listener_color,
			       "set-" S_listener_color, SCM_FNC g_set_listener_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_listener_text_color, SCM_FNC g_listener_text_color, H_listener_text_color,
			       "set-" S_listener_text_color, SCM_FNC g_set_listener_text_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_selected_mix_color, SCM_FNC g_selected_mix_color, H_selected_mix_color,
			       "set-" S_selected_mix_color, SCM_FNC g_set_selected_mix_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_enved_waveform_color, SCM_FNC g_enved_waveform_color, H_enved_waveform_color,
			       "set-" S_enved_waveform_color, SCM_FNC g_set_enved_waveform_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_filter_waveform_color, SCM_FNC g_filter_waveform_color, H_filter_waveform_color,
			       "set-" S_filter_waveform_color, SCM_FNC g_set_filter_waveform_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_highlight_color, SCM_FNC g_highlight_color, H_highlight_color,
			       "set-" S_highlight_color, SCM_FNC g_set_highlight_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_graph_color, SCM_FNC g_graph_color, H_graph_color,
			       "set-" S_graph_color, SCM_FNC g_set_graph_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_selected_graph_color, SCM_FNC g_selected_graph_color, H_selected_graph_color,
			       "set-" S_selected_graph_color, SCM_FNC g_set_selected_graph_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_data_color, SCM_FNC g_data_color, H_data_color,
			       "set-" S_data_color, SCM_FNC g_set_data_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_selected_data_color, SCM_FNC g_selected_data_color, H_selected_data_color,
			       "set-" S_selected_data_color, SCM_FNC g_set_selected_data_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_cursor_color, SCM_FNC g_cursor_color, H_cursor_color,
			       "set-" S_cursor_color, SCM_FNC g_set_cursor_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_selection_color, SCM_FNC g_selection_color, H_selection_color,
			       "set-" S_selection_color, SCM_FNC g_set_selection_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_pushed_button_color, SCM_FNC g_pushed_button_color, H_pushed_button_color,
			       "set-" S_pushed_button_color, SCM_FNC g_set_pushed_button_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_text_focus_color, SCM_FNC g_text_focus_color, H_text_focus_color,
			       "set-" S_text_focus_color, SCM_FNC g_set_text_focus_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_sash_color, SCM_FNC g_sash_color, H_sash_color,
			       "set-" S_sash_color, SCM_FNC g_set_sash_color, local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_graph_cursor, SCM_FNC g_graph_cursor, H_graph_cursor,
			       "set-" S_graph_cursor, SCM_FNC g_set_graph_cursor, local_doc, 0, 0, 1, 0);
  
  define_procedure_with_setter(S_mix_color, SCM_FNC g_mix_color, H_mix_color,
			       "set-" S_mix_color, SCM_FNC g_set_mix_color, local_doc, 0, 1, 1, 1);

  define_procedure_with_setter(S_selected_mix_color, SCM_FNC g_selected_mix_color, H_selected_mix_color,
			       "set-" S_selected_mix_color, SCM_FNC g_set_selected_mix_color, local_doc, 0, 1, 1, 1);

}
#endif
