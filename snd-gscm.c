/* TODO  guile-gtk (hundreds of widgets, a dozen or so examples)
 * TODO  all sgx, popup, grec gmix
 * TODO  exs: annotation boxes (see grfsnd)
 * TODO       popup info in file viewer
 * TODO       special lisp graph effects
 * TODO       own fft peaks info
 */

#include "snd.h"

#if HAVE_GUILE

static snd_state *state;

#if HAVE_HTML
static SCM g_html_dir(void) 
{
  #define H_html_dir "(" S_html_dir ") -> location of Snd documentation"
  RTNSTR(html_dir(state));
}

static SCM g_set_html_dir(SCM val) 
{
  SCM_ASSERT(gh_string_p(val),val,SCM_ARG1,"set-" S_html_dir); 
  set_html_dir(state,gh_scm2newstr(val,0)); 
  return(val);
}
#endif

static SCM g_region_dialog(void) 
{
  #define H_region_dialog "(" S_region_dialog ") starts the region dialog"
  if (snd_regions() > 0) View_Region_Callback(MAIN_PANE(state),(gpointer)state); 
  return(SCM_BOOL_F);
}

static gint timed_eval(gpointer in_code)
{
  SCM code = (SCM)in_code;
  if ((code) && (gh_procedure_p(code))) g_call0(code);
  return(0);
}

static SCM g_in(SCM ms, SCM code)
{
  #define H_in "(" S_in " msecs func) invokes func in msecs milliseconds"
  gtk_timeout_add((guint32)gh_scm2ulong(ms),timed_eval,(gpointer)code);
  return(ms);
}


/* color support */

static SND_TAG_TYPE snd_color_tag = 0;

typedef struct {
  GdkColor *color;
} snd_color;

static SCM mark_snd_color(SCM obj)
{
  SCM_SETGC8MARK(obj);
  return(SCM_BOOL_F);
}

static int snd_color_p(SCM obj)
{
  return((SCM_NIMP(obj)) && (SND_SMOB_TYPE(snd_color_tag,obj)));
}

static SCM g_color_p(SCM obj) 
{
  #define H_color_p "(" S_colorQ " obj) -> #t if obj is a color object"
  RTNBOOL(snd_color_p(obj));
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
  buf = (char *)CALLOC(128,sizeof(char));
  sprintf(buf,"#<col" STR_OR ": (%.2f %.2f %.2f)>",
	  (float)(v->color->red) / 65535.0,
	  (float)(v->color->green) / 65535.0,
	  (float)(v->color->blue) / 65535.0);
  scm_puts(buf,port);
  FREE(buf);
  return(scm_return_first(1,obj));
}

static SCM g_color2list(SCM obj)
{
  #define H_color2list "(" S_color2list " obj) -> color rgb values as a list of floats"
  snd_color *v;
  SCM_ASSERT(snd_color_p(obj),obj,SCM_ARG1,S_color2list); 
  v = (snd_color *)SND_VALUE_OF(obj);
  return(scm_return_first(SCM_LIST3(gh_double2scm((float)(v->color->red) / 65535.0),
				    gh_double2scm((float)(v->color->green) / 65535.0),
				    gh_double2scm((float)(v->color->blue) / 65535.0)),
			  obj));
}

static SCM equalp_snd_color(SCM obj1, SCM obj2)
{
  snd_color *v1,*v2;
  v1 = (snd_color *)SND_VALUE_OF(obj1);
  v2 = (snd_color *)SND_VALUE_OF(obj2);
  RTNBOOL(v1->color->pixel == v2->color->pixel);
}

static SCM g_make_snd_color(SCM r, SCM g, SCM b)
{
  #define H_make_color "(" S_make_color " r g b) -> a color object with the indicated rgb values"
  snd_color *new_color;
  GdkColor gcolor;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(r)),r,SCM_ARG1,S_make_color);
  /* someday accept a list as r */
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(g)),g,SCM_ARG2,S_make_color);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(b)),b,SCM_ARG3,S_make_color);
  new_color = (snd_color *)CALLOC(1,sizeof(snd_color));
  gcolor.red = (unsigned short)(65535 * gh_scm2double(r));
  gcolor.green = (unsigned short)(65535 * gh_scm2double(g));
  gcolor.blue = (unsigned short)(65535 * gh_scm2double(b));
  new_color->color = gdk_color_copy(&gcolor);
  gdk_color_alloc(gdk_colormap_get_system(),new_color->color);
  SND_RETURN_NEWSMOB(snd_color_tag,new_color);
}

static SCM gcolor2sndcolor(GdkColor *pix)
{
  return(g_make_snd_color(gh_double2scm((Float)(pix->red) / 65535.0),
			  gh_double2scm((Float)(pix->green) / 65535.0),
			  gh_double2scm((Float)(pix->blue) / 65535.0)));
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
  if (GTK_IS_WIDGET(w)) set_background_and_redraw(w,(GdkColor *)ptr);
}

static SCM g_set_basic_color (SCM color) 
{
  snd_color *v; 
  GdkColor *old_color;
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_basic_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      old_color = (state->sgx)->basic_color;
      (state->sgx)->basic_color = v->color; 
      map_over_children(MAIN_SHELL(state),recolor_everything,(gpointer)old_color);
    }
  return(color);
}

static SCM g_basic_color(void) 
{
  #define H_basic_color "(" S_basic_color ") -> Snd's basic color"
  return(gcolor2sndcolor((state->sgx)->basic_color));
}

static void color_unselected_graphs(GdkColor *color)
{
  int i,j;
  chan_info *cp;
  snd_info *sp;
  for (i=0;i<state->max_sounds;i++)
    {
      sp = (snd_info *)state->sounds[i];
      if (sp)
	{
	  for (j=0;j<sp->allocated_chans;j++)
	    {
	      cp = sp->chans[j];
	      if ((cp) && ((i != state->selected_sound) || (j != sp->selected_channel)))
		set_background_and_redraw(channel_graph(cp),color);
	    }
	}
    }
}

#define COLOR_POSITION 0
#define COLOR_ZOOM 1

static void color_chan_components(GdkColor *color, int which_component)
{
  int i,j;
  chan_info *cp;
  snd_info *sp;
  for (i=0;i<state->max_sounds;i++)
    {
      sp = (snd_info *)state->sounds[i];
      if (sp)
	{
	  for (j=0;j<sp->allocated_chans;j++)
	    {
	      cp = sp->chans[j];
	      if (cp)
		{
		  if (which_component == COLOR_POSITION)
		    {
		      set_background_and_redraw(channel_sx(cp),color);
		      set_background_and_redraw(channel_sy(cp),color);
		    }
		  else
		    {
		      set_background_and_redraw(channel_zx(cp),color);
		      set_background_and_redraw(channel_zy(cp),color);
		    }
		}
	    }
	}
    }
}

static SCM g_set_data_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_data_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_data(state,v->color);
      map_over_chans(state,update_graph,NULL);
    }
  return(color);
}

static SCM g_data_color(void) 
{
  #define H_data_color "(" S_data_color ") -> color used to draw unselected data"
  return(gcolor2sndcolor((state->sgx)->data_color));
}

static SCM g_set_selected_data_color (SCM color)
{
  snd_color *v; 
  chan_info *cp;
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_selected_data_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_selected_data(state,v->color);
      cp = selected_channel(state);
      if (cp) 
	{
	  color_selected_data(state,v->color);
	  update_graph(cp,NULL);
	}
    }
  return(color);
}

static SCM g_selected_data_color(void) 
{
  #define H_selected_data_color "(" S_selected_data_color ") -> color used for selected data"
  return(gcolor2sndcolor((state->sgx)->selected_data_color));
}

static SCM g_set_graph_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_graph_color);
  v = get_snd_color(color);
  if (v) 
    {
      color_graph(state,v->color);
      color_unselected_graphs(v->color);
    }
  return(color);
}

static SCM g_graph_color(void) 
{
  #define H_graph_color "(" S_graph_color ") -> background color used for unselected data"
  return(gcolor2sndcolor((state->sgx)->graph_color));
}

static SCM g_set_selected_graph_color (SCM color) 
{
  snd_color *v; 
  chan_info *cp;
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_selected_graph_color);
  v = get_snd_color(color); 
  if (v) 
    {
      color_selected_graph(state,v->color);
      cp = selected_channel(state);
      if (cp) set_background_and_redraw(channel_graph(cp),v->color);
    }
  return(color);
}

static SCM g_selected_graph_color(void) 
{
  #define H_selected_graph_color "(" S_selected_graph_color ") -> background color of selected data"
  return(gcolor2sndcolor((state->sgx)->selected_graph_color));
}

static SCM g_set_cursor_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_cursor_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_cursor(state,v->color);
      map_over_chans(state,update_graph,NULL);
    }
  return(color);
}

static SCM g_cursor_color(void) 
{
  #define H_cursor_color "(" S_cursor_color ") -> cursor color"
  return(gcolor2sndcolor((state->sgx)->cursor_color));
}

static SCM g_set_selection_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_selection_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_selection(state,v->color);
      map_over_chans(state,update_graph,NULL);
    }
  return(color);
}

static SCM g_selection_color(void) 
{
  #define H_selection_color "(" S_selection_color ") -> selection color"
  return(gcolor2sndcolor((state->sgx)->selection_color));
}

static SCM g_set_highlight_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_highlight_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->highlight_color = v->color; 
  return(color);
}

static SCM g_highlight_color(void) 
{
  #define H_highlight_color "(" S_highlight_color ") -> color of highlighted text or buttons"
  return(gcolor2sndcolor((state->sgx)->highlight_color));
}

static SCM g_set_mark_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_mark_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      color_marks(state,v->color);
      map_over_chans(state,update_graph,NULL);
    }
  return(color);
}

static SCM g_mark_color(void) 
{
  #define H_mark_color "(" S_mark_color ") -> mark color"
  return(gcolor2sndcolor((state->sgx)->mark_color));
}

static SCM g_set_zoom_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_zoom_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->zoom_color = v->color; 
      color_chan_components(v->color,COLOR_ZOOM);
    }
  return(color);
}

static SCM g_zoom_color(void) 
{
  #define H_zoom_color "(" S_zoom_color ") -> color of zoom sliders"
  return(gcolor2sndcolor((state->sgx)->zoom_color));
}

static SCM g_set_position_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_position_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->position_color = v->color; 
      color_chan_components(v->color,COLOR_POSITION);
    }
  return(color);
}

static SCM g_position_color(void) 
{
  #define H_position_color "(" S_position_color ") -> color of position sliders"
  return(gcolor2sndcolor((state->sgx)->position_color));
}

static SCM g_set_listener_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_listener_color); 
  v = get_snd_color(color);
  if (v) color_listener(v->color);
  return(color);
}

static SCM g_listener_color(void) 
{
  #define H_listener_color "(" S_listener_color ") -> background color of the lisp listener"
  return(gcolor2sndcolor((state->sgx)->listener_color));
}

static SCM g_set_enved_waveform_color (SCM color) 
{
  snd_color *v;
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_enved_waveform_color); 
  v = get_snd_color(color); 
  if (v) color_enved_waveform(v->color);
  return(color);
}

static SCM g_enved_waveform_color(void) 
{
  #define H_enved_waveform_color "(" S_enved_waveform_color ") -> color of the envelope editor wave display"
  return(gcolor2sndcolor((state->sgx)->enved_waveform_color));
}

static SCM g_set_filter_waveform_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_filter_waveform_color);
  v = get_snd_color(color);
  if (v) color_filter_waveform(state,v->color);
  return(color);
}

static SCM g_filter_waveform_color(void) 
{
  #define H_filter_waveform_color "(" S_filter_waveform_color ") -> color of the filter waveform"
  return(gcolor2sndcolor((state->sgx)->filter_waveform_color));
}

static SCM g_set_mix_color (SCM arg1, SCM arg2) 
{
  snd_color *v; 
  SCM color,mix_id=SCM_UNDEFINED;
  if (SCM_UNBNDP(arg2))
    color = arg1;
  else
    {
      color = arg2;
      mix_id = arg1;
    }
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_mix_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      if (gh_number_p(mix_id))
	color_one_mix_from_id(gh_scm2int(mix_id),v->color);
      else set_mix_color(state,v->color);
      map_over_chans(state,update_graph,NULL);
    }
  return(color);
}

static SCM g_mix_color(SCM mix_id) 
{
  #define H_mix_color "(" S_mix_color ") -> color of mix consoles"
  if (gh_number_p(mix_id))
    return(gcolor2sndcolor(mix_to_color_from_id(gh_scm2int(mix_id))));
  return(gcolor2sndcolor((state->sgx)->mix_color));
}

static SCM g_set_selected_mix_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_selected_mix_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      set_selected_mix_color(state,v->color);
      map_over_chans(state,update_graph,NULL);
    }
  return(color);
}

static SCM g_selected_mix_color(void) 
{
  #define H_selected_mix_color "(" S_selected_mix_color ") -> color of the currently selected mix"
  return(gcolor2sndcolor((state->sgx)->selected_mix_color));
}


static void recolor_button(GtkWidget *w, gpointer ptr)
{
  if (GTK_IS_WIDGET(w))
    {
      if (GTK_IS_BUTTON(w))
	set_pushed_button_colors(w,state);
    }
}

static SCM g_set_pushed_button_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_pushed_button_color); 
  v = get_snd_color(color); 
  if (v) 
    {
      (state->sgx)->pushed_button_color = v->color;
      map_over_children(MAIN_SHELL(state),recolor_button,NULL);
    }
  return(color);
}

static SCM g_pushed_button_color(void) 
{
  #define H_pushed_button_color "(" S_pushed_button_color ") -> color of a pushed button"
  return(gcolor2sndcolor((state->sgx)->pushed_button_color));
}

static SCM g_set_text_focus_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_text_focus_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->text_focus_color = v->color;
  return(color);
}

static SCM g_text_focus_color(void) 
{
  #define H_text_focus_color "(" S_text_focus_color ") -> color used to show a text field has focus"
  return(gcolor2sndcolor((state->sgx)->text_focus_color));
}

static SCM g_set_sash_color (SCM color) 
{
  snd_color *v; 
  SCM_ASSERT(snd_color_p(color),color,SCM_ARG1,"set-" S_sash_color); 
  v = get_snd_color(color); 
  if (v) (state->sgx)->sash_color = v->color;
  return(color);
}

static SCM g_sash_color(void) 
{
  #define H_sash_color "(" S_sash_color ") -> color used to draw paned window sashes"
  return(gcolor2sndcolor((state->sgx)->sash_color));
}

static SCM g_load_colormap(SCM colors)
{
  #define H_load_colormap "(" S_load_colormap " colors) uses the vector colors to set the current colormap"
  int i,len;
  GdkColor **xcs;
  snd_color *v;
  SCM_ASSERT((gh_vector_p(colors)),colors,SCM_ARG1,S_load_colormap);
  len = gh_vector_length(colors);
  xcs = (GdkColor **)CALLOC(len,sizeof(GdkColor *));
  for (i=0;i<len;i++)
    {
      v = get_snd_color(gh_vector_ref(colors,gh_int2scm(i)));
      xcs[i] = v->color;
    }
  /* TODO: colormaps? x_load_colormap(xcs); */
  return(gh_int2scm(len));
}

static SCM g_graph_cursor(void)
{
  #define H_graph_cursor "(" S_graph_cursor ") -> current graph cursor shape"
  return(gh_int2scm(in_graph_cursor(state)));
}

static SCM g_set_graph_cursor(SCM curs)
{
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(curs)),curs,SCM_ARG1,"set-" S_graph_cursor);
  state->Graph_Cursor = gh_scm2int(curs);
  (state->sgx)->graph_cursor = gdk_cursor_new((GdkCursorType)in_graph_cursor(state));
  return(curs);
}

#if HAVE_GUILE_GTK
static void init_guile_gtk(SCM local_doc);
#endif

void g_initialize_xgh(snd_state *ss, SCM local_doc)
{
  state = ss;
#if HAVE_NEW_SMOB
  snd_color_tag = scm_make_smob_type("color",sizeof(snd_color));
  scm_set_smob_mark(snd_color_tag,mark_snd_color);
  scm_set_smob_print(snd_color_tag,print_snd_color);
  scm_set_smob_free(snd_color_tag,free_snd_color);
  scm_set_smob_equalp(snd_color_tag,equalp_snd_color);
#else
  snd_color_tag = scm_newsmob(&snd_color_smobfuns);
#endif
#if HAVE_HTML
  scm_add_feature("snd-html");
  define_procedure_with_setter(S_html_dir,SCM_FNC g_html_dir,H_html_dir,
			       "set-" S_html_dir,SCM_FNC g_set_html_dir,local_doc,0,0,1,0);
#endif
  DEFINE_PROC(gh_new_procedure0_0(S_region_dialog,g_region_dialog),H_region_dialog);
  DEFINE_PROC(gh_new_procedure2_0(S_in,g_in),H_in);
  DEFINE_PROC(gh_new_procedure3_0(S_make_color,g_make_snd_color),H_make_color);
  DEFINE_PROC(gh_new_procedure1_0(S_colorQ,g_color_p),H_color_p);
  DEFINE_PROC(gh_new_procedure1_0(S_color2list,g_color2list),H_color2list);
  DEFINE_PROC(gh_new_procedure1_0(S_load_colormap,g_load_colormap),H_load_colormap);

  define_procedure_with_setter(S_basic_color,SCM_FNC g_basic_color,H_basic_color,
			       "set-" S_basic_color,SCM_FNC g_set_basic_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_zoom_color,SCM_FNC g_zoom_color,H_zoom_color,
			       "set-" S_zoom_color,SCM_FNC g_set_zoom_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_position_color,SCM_FNC g_position_color,H_position_color,
			       "set-" S_position_color,SCM_FNC g_set_position_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_mark_color,SCM_FNC g_mark_color,H_mark_color,
			       "set-" S_mark_color,SCM_FNC g_set_mark_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_listener_color,SCM_FNC g_listener_color,H_listener_color,
			       "set-" S_listener_color,SCM_FNC g_set_listener_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_selected_mix_color,SCM_FNC g_selected_mix_color,H_selected_mix_color,
			       "set-" S_selected_mix_color,SCM_FNC g_set_selected_mix_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_enved_waveform_color,SCM_FNC g_enved_waveform_color,H_enved_waveform_color,
			       "set-" S_enved_waveform_color,SCM_FNC g_set_enved_waveform_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_filter_waveform_color,SCM_FNC g_filter_waveform_color,H_filter_waveform_color,
			       "set-" S_filter_waveform_color,SCM_FNC g_set_filter_waveform_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_highlight_color,SCM_FNC g_highlight_color,H_highlight_color,
			       "set-" S_highlight_color,SCM_FNC g_set_highlight_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_graph_color,SCM_FNC g_graph_color,H_graph_color,
			       "set-" S_graph_color,SCM_FNC g_set_graph_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_selected_graph_color,SCM_FNC g_selected_graph_color,H_selected_graph_color,
			       "set-" S_selected_graph_color,SCM_FNC g_set_selected_graph_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_data_color,SCM_FNC g_data_color,H_data_color,
			       "set-" S_data_color,SCM_FNC g_set_data_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_selected_data_color,SCM_FNC g_selected_data_color,H_selected_data_color,
			       "set-" S_selected_data_color,SCM_FNC g_set_selected_data_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_cursor_color,SCM_FNC g_cursor_color,H_cursor_color,
			       "set-" S_cursor_color,SCM_FNC g_set_cursor_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_selection_color,SCM_FNC g_selection_color,H_selection_color,
			       "set-" S_selection_color,SCM_FNC g_set_selection_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_pushed_button_color,SCM_FNC g_pushed_button_color,H_pushed_button_color,
			       "set-" S_pushed_button_color,SCM_FNC g_set_pushed_button_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_text_focus_color,SCM_FNC g_text_focus_color,H_text_focus_color,
			       "set-" S_text_focus_color,SCM_FNC g_set_text_focus_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_sash_color,SCM_FNC g_sash_color,H_sash_color,
			       "set-" S_sash_color,SCM_FNC g_set_sash_color,local_doc,0,0,1,0);

  define_procedure_with_setter(S_graph_cursor,SCM_FNC g_graph_cursor,H_graph_cursor,
			       "set-" S_graph_cursor,SCM_FNC g_set_graph_cursor,local_doc,0,0,1,0);
  
  define_procedure_with_setter(S_mix_color,SCM_FNC g_mix_color,H_mix_color,
			       "set-" S_mix_color,SCM_FNC g_set_mix_color,local_doc,0,1,1,1);

  define_procedure_with_setter(S_selected_mix_color,SCM_FNC g_selected_mix_color,H_selected_mix_color,
			       "set-" S_selected_mix_color,SCM_FNC g_set_selected_mix_color,local_doc,0,1,1,1);

#if HAVE_GUILE_GTK
  init_guile_gtk(local_doc);
#endif
}
#endif


#if HAVE_GUILE_GTK
#include <guile-gtk.h>
extern sgtk_boxed_info sgtk_gdk_gc_info;
extern sgtk_boxed_info sgtk_gdk_font_info;
extern sgtk_boxed_info sgtk_gdk_color_info;
extern sgtk_boxed_info sgtk_gdk_cursor_info;

#define Sg_selected_cursor_gc      "sg-selected-cursor-gc"
#define Sg_selected_basic_gc       "sg-selected-basic-gc"
#define Sg_combined_basic_gc       "sg-combined-basic-gc"
#define Sg_cursor_gc               "sg-cursor-gc"
#define Sg_selection_gc            "sg-selection-gc"
#define Sg_selected_selection_gc   "sg-selected-selection-gc"
#define Sg_erase_gc                "sg-erase-gc"
#define Sg_selected_erase_gc       "sg-selected-erase-gc"
#define Sg_mark_gc                 "sg-mark-gc"
#define Sg_selected_mark_gc        "sg-selected-mark-gc"
#define Sg_mix_gc                  "sg-mix-gc"
#define Sg_fltenv_basic_gc         "sg-fltenv-basic-gc"
#define Sg_fltenv_data_gc          "sg-fltenv-data-gc"
#define Sg_speed_gc                "sg-speed-gc"

#define Sg_listener_font           "sg-listener-font"
#define Sg_axis_label_font         "sg-axis-label-font"
#define Sg_axis_numbers_font       "sg-axis-numbers-font"
#define Sg_bold_button_font        "sg-bold-button-font"
#define Sg_button_font             "sg-button-font"
#define Sg_help_text_font          "sg-help-text-font"
#define Sg_tiny_font               "sg-tiny-font"

#define Sg_basic_color             "sg-basic-color"
#define Sg_white_color             "sg-white-color"
#define Sg_black_color             "sg-black-color"
#define Sg_red_color               "sg-red-color"
#define Sg_yellow_color            "sg-yellow-color"
#define Sg_green_color             "sg-green-color"
#define Sg_light_blue_color        "sg-light-blue-color"
#define Sg_lighter_blue_color      "sg-lighter-blue-color"
#define Sg_data_color              "sg-data-color"
#define Sg_selected_data_color     "sg-selected-data-color"
#define Sg_mark_color              "sg-mark-color"
#define Sg_graph_color             "sg-graph-color"
#define Sg_selected_graph_color    "sg-selected-graph-color"
#define Sg_listener_color          "sg-listener-color"
#define Sg_cursor_color            "sg-cursor-color"
#define Sg_selection_color         "sg-selection-color"
#define Sg_zoom_color              "sg-zoom-color"
#define Sg_position_color          "sg-position-color"
#define Sg_highlight_color         "sg-highlight-color"
#define Sg_enved_waveform_color    "sg-enved-waveform-color"
#define Sg_selected_mix_color      "sg-selected-mix-color"
#define Sg_text_focus_color        "sg-text-focus-color"
#define Sg_filter_waveform_color   "sg-filter-waveform-color"
#define Sg_mix_color               "sg-mix-color"
#define Sg_pushed_button_color     "sg-pushed-button-color"
#define Sg_sash_color              "sg-sash-color"

static SCM sg_selected_cursor_gc(void) 
{
  #define H_sg_selected_cursor_gc "(" Sg_selected_cursor_gc ") -> gtk gc of selected graph cursor"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selected_cursor_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_selected_basic_gc(void) 
{
  #define H_sg_selected_basic_gc "(" Sg_selected_basic_gc ") -> gtk gc of selected data"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selected_basic_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_combined_basic_gc(void) 
{
  #define H_sg_combined_basic_gc "(" Sg_combined_basic_gc ") -> gtk gc of combined graph"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->combined_basic_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_cursor_gc(void) 
{
  #define H_sg_cursor_gc "(" Sg_cursor_gc ") -> gtk gc of unselected graph cursor"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->cursor_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_selection_gc(void) 
{
  #define H_sg_selection_gc "(" Sg_selection_gc ") -> gtk gc of unselected graph's selection display"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selection_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_selected_selection_gc(void) 
{
  #define H_sg_selected_selection_gc "(" Sg_selected_selection_gc ") -> gtk gc of selected graph's selection display"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selected_selection_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_erase_gc(void) 
{
  #define H_sg_erase_gc "(" Sg_erase_gc ") -> gtk gc to erase unselected graph's data"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->erase_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_selected_erase_gc(void) 
{
  #define H_sg_selected_erase_gc "(" Sg_selected_erase_gc ") -> gtk gc to erase selected graph's data"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selected_erase_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_mark_gc(void) 
{
  #define H_sg_mark_gc "(" Sg_mark_gc ") -> gtk gc for unselected graph marks"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->mark_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_selected_mark_gc(void) 
{
  #define H_sg_selected_mark_gc "(" Sg_selected_mark_gc ") -> gtk gc for selected graph marks"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selected_mark_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_mix_gc(void) 
{
  #define H_sg_mix_gc "(" Sg_mix_gc ") -> mix waveform gtk gc"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->mix_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_fltenv_basic_gc(void) 
{
  #define H_sg_fltenv_basic_gc "(" Sg_fltenv_basic_gc ") -> filter envelope gtk gc (background)"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->fltenv_basic_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_fltenv_data_gc(void) 
{
  #define H_sg_fltenv_data_gc "(" Sg_fltenv_data_gc ") -> filter envelope gtk gc (waveform)"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->fltenv_data_gc),&sgtk_gdk_gc_info,0));
}

static SCM sg_speed_gc(void) 
{
  #define H_sg_speed_gc "(" Sg_speed_gc ") -> speed arrow pixmap background, not currently used"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->speed_gc),&sgtk_gdk_gc_info,0));
}


static SCM sg_listener_font(void) 
{
  #define H_sg_listener_font "(" Sg_listener_font ") -> lisp listener gtk font"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->listener_fnt),&sgtk_gdk_font_info,0));
}

static SCM sg_axis_label_font(void) 
{
  #define H_sg_axis_label_font "(" Sg_axis_label_font ") -> axis label gtk font"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->axis_label_fnt),&sgtk_gdk_font_info,0));
}

static SCM sg_axis_numbers_font(void) 
{
  #define H_sg_axis_numbers_font "(" Sg_axis_numbers_font ") -> axis numbers gtk font"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->axis_numbers_fnt),&sgtk_gdk_font_info,0));
}

static SCM sg_tiny_font(void) 
{
  #define H_sg_tiny_font "(" Sg_tiny_font ") -> small gtk font"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->tiny_fnt),&sgtk_gdk_font_info,0));
}

static SCM sg_help_text_font(void) 
{
  #define H_sg_help_text_font "(" Sg_help_text_font ") -> help dialog gtk font"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->help_text_fnt),&sgtk_gdk_font_info,0));
}

static SCM sg_button_font(void) 
{
  #define H_sg_button_font "(" Sg_button_font ") -> gtk font used by most buttons and labels"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->button_fnt),&sgtk_gdk_font_info,0));
}

static SCM sg_bold_button_font(void) 
{
  #define H_sg_bold_button_font "(" Sg_bold_button_font ") bold gtk font used by some buttons and labels"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->bold_button_fnt),&sgtk_gdk_font_info,0));
}


static SCM sg_basic_color(void) 
{
  #define H_sg_basic_color "(" Sg_basic_color ") -> gtk color used as widget basic color"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->basic_color),&sgtk_gdk_color_info,0));
}

static SCM sg_white_color(void) 
{
  #define H_sg_white_color "(" Sg_white_color ") -> white gtk color"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->white),&sgtk_gdk_color_info,0));
}

static SCM sg_black_color(void) 
{
  #define H_sg_black_color "(" Sg_black_color ") -> black gtk color"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->black),&sgtk_gdk_color_info,0));
}

static SCM sg_red_color(void) 
{
  #define H_sg_red_color "(" Sg_red_color ") -> red gtk color"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->red),&sgtk_gdk_color_info,0));
}

static SCM sg_yellow_color(void) 
{
  #define H_sg_yellow_color "(" Sg_yellow_color ") -> yellow gtk color"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->yellow),&sgtk_gdk_color_info,0));
}

static SCM sg_green_color(void) 
{
  #define H_sg_green_color "(" Sg_green_color ") -> green gtk color"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->green),&sgtk_gdk_color_info,0));
}

static SCM sg_light_blue_color(void) 
{
  #define H_sg_light_blue_color "(" Sg_light_blue_color ") -> light blue gtk color"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->light_blue),&sgtk_gdk_color_info,0));
}

static SCM sg_lighter_blue_color(void) 
{
  #define H_sg_lighter_blue_color "(" Sg_lighter_blue_color ") -> very light blue gtk color"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->lighter_blue),&sgtk_gdk_color_info,0));
}

static SCM sg_data_color(void) 
{
  #define H_sg_data_color "(" Sg_data_color ") -> gtk color used for unselected graphs"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->data_color),&sgtk_gdk_color_info,0));
}

static SCM sg_selected_data_color(void) 
{
  #define H_sg_selected_data_color "(" Sg_selected_data_color ") -> gtk color used for selected graphs"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selected_data_color),&sgtk_gdk_color_info,0));
}

static SCM sg_mark_color(void) 
{
  #define H_sg_mark_color "(" Sg_mark_color ") -> gtk color used for marks in unselected graphs"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->mark_color),&sgtk_gdk_color_info,0));
}

static SCM sg_graph_color(void) 
{
  #define H_sg_graph_color "(" Sg_graph_color ") -> gtk color for unselected graph background"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->graph_color),&sgtk_gdk_color_info,0));
}

static SCM sg_selected_graph_color(void) 
{
  #define H_sg_selected_graph_color "(" Sg_selected_graph_color ") -> gtk color for selected graph background"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selected_graph_color),&sgtk_gdk_color_info,0));
}

static SCM sg_listener_color(void) 
{
  #define H_sg_listener_color "(" Sg_listener_color ") -> gtk color of lisp listener"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->listener_color),&sgtk_gdk_color_info,0));
}

static SCM sg_cursor_color(void) 
{
  #define H_sg_cursor_color "(" Sg_cursor_color ") -> gtk color of cursor in unselected graphs"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->cursor_color),&sgtk_gdk_color_info,0));
}

static SCM sg_selection_color(void) 
{
  #define H_sg_selection_color "(" Sg_selection_color ") -> gtk color of selections in unselected graphs"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selection_color),&sgtk_gdk_color_info,0));
}

static SCM sg_zoom_color(void) 
{
  #define H_sg_zoom_color "(" Sg_zoom_color ") -> gtk color of zoom sliders"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->zoom_color),&sgtk_gdk_color_info,0));
}

static SCM sg_position_color(void) 
{
  #define H_sg_position_color "(" Sg_position_color ") -> gtk color of position sliders"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->position_color),&sgtk_gdk_color_info,0));
}

static SCM sg_highlight_color(void) 
{
  #define H_sg_highlight_color "(" Sg_highlight_color ") -> gtk color used to highlight labels"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->highlight_color),&sgtk_gdk_color_info,0));
}

static SCM sg_enved_waveform_color(void) 
{
  #define H_sg_enved_waveform_color "(" Sg_enved_waveform_color ") -> gtk color used by envelope editor for waveforms"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->enved_waveform_color),&sgtk_gdk_color_info,0));
}

static SCM sg_text_focus_color(void) 
{
  #define H_sg_text_focus_color "(" Sg_text_focus_color ") -> gtk color for currently active text widget"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->text_focus_color),&sgtk_gdk_color_info,0));
}

static SCM sg_filter_waveform_color(void) 
{
  #define H_sg_filter_waveform_color "(" Sg_filter_waveform_color ") -> gtk color for filter waveform"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->filter_waveform_color),&sgtk_gdk_color_info,0));
}

static SCM sg_mix_color(void) 
{
  #define H_sg_mix_color "(" Sg_mix_color ") -> gtk color for unselected mix"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->mix_color),&sgtk_gdk_color_info,0));
}

static SCM sg_selected_mix_color(void) 
{
  #define H_sg_selected_mix_color "(" Sg_selected_mix_color ") -> gtk color for selected mix"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->selected_mix_color),&sgtk_gdk_color_info,0));
}

static SCM sg_pushed_button_color(void) 
{
  #define H_sg_pushed_button_color "(" Sg_pushed_button_color ") -> gtk color for pushed button"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->pushed_button_color),&sgtk_gdk_color_info,0));
}

static SCM sg_sash_color(void) 
{
  #define H_sg_sash_color "(" Sg_sash_color ") -> gtk color of pane sash, not currently used"
  return(sgtk_boxed2scm((gpointer)((state->sgx)->sash_color),&sgtk_gdk_color_info,0));
}


/* ungrf_x and y would be ungrf_x(cp->axis,x) except for the 3-way division horizontally */

static void init_guile_gtk(SCM local_doc)
{
  init_menu_widgets(local_doc);
  init_find_widgets(local_doc);
  init_file_widgets(local_doc);
  init_recorder_widgets(local_doc);
  init_listener_widgets(local_doc);
  init_print_widgets(local_doc);
  init_stats_widgets(local_doc);
  init_fft_widgets(local_doc);
  init_enved_widgets(local_doc);
  init_help_widgets(local_doc);
  init_chn_widgets(local_doc);
  init_error_widgets(local_doc);
  init_sound_widgets(local_doc);
  init_region_widgets(local_doc);

  DEFINE_PROC(gh_new_procedure0_0(Sg_selected_cursor_gc,sg_selected_cursor_gc),H_sg_selected_cursor_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selected_basic_gc,sg_selected_basic_gc),H_sg_selected_basic_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_combined_basic_gc,sg_combined_basic_gc),H_sg_combined_basic_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_cursor_gc,sg_cursor_gc),H_sg_cursor_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selection_gc,sg_selection_gc),H_sg_selection_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selected_selection_gc,sg_selected_selection_gc),H_sg_selected_selection_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_erase_gc,sg_erase_gc),H_sg_erase_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selected_erase_gc,sg_selected_erase_gc),H_sg_selected_erase_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_mark_gc,sg_mark_gc),H_sg_mark_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selected_mark_gc,sg_selected_mark_gc),H_sg_selected_mark_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_mix_gc,sg_mix_gc),H_sg_mix_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_fltenv_basic_gc,sg_fltenv_basic_gc),H_sg_fltenv_basic_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_fltenv_data_gc,sg_fltenv_data_gc),H_sg_fltenv_data_gc);
  DEFINE_PROC(gh_new_procedure0_0(Sg_speed_gc,sg_speed_gc),H_sg_speed_gc);

  DEFINE_PROC(gh_new_procedure0_0(Sg_listener_font,sg_listener_font),H_sg_listener_font);
  DEFINE_PROC(gh_new_procedure0_0(Sg_axis_label_font,sg_axis_label_font),H_sg_axis_label_font);
  DEFINE_PROC(gh_new_procedure0_0(Sg_axis_numbers_font,sg_axis_numbers_font),H_sg_axis_numbers_font);
  DEFINE_PROC(gh_new_procedure0_0(Sg_tiny_font,sg_tiny_font),H_sg_tiny_font);
  DEFINE_PROC(gh_new_procedure0_0(Sg_button_font,sg_button_font),H_sg_button_font);
  DEFINE_PROC(gh_new_procedure0_0(Sg_bold_button_font,sg_bold_button_font),H_sg_bold_button_font);
  DEFINE_PROC(gh_new_procedure0_0(Sg_help_text_font,sg_help_text_font),H_sg_help_text_font);

  DEFINE_PROC(gh_new_procedure0_0(Sg_basic_color,sg_basic_color),H_sg_basic_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_white_color,sg_white_color),H_sg_white_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_black_color,sg_black_color),H_sg_black_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_red_color,sg_red_color),H_sg_red_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_yellow_color,sg_yellow_color),H_sg_yellow_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_green_color,sg_green_color),H_sg_green_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_light_blue_color,sg_light_blue_color),H_sg_light_blue_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_lighter_blue_color,sg_lighter_blue_color),H_sg_lighter_blue_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_data_color,sg_data_color),H_sg_data_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selected_data_color,sg_selected_data_color),H_sg_selected_data_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_mark_color,sg_mark_color),H_sg_mark_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_graph_color,sg_graph_color),H_sg_graph_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selected_graph_color,sg_selected_graph_color),H_sg_selected_graph_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_listener_color,sg_listener_color),H_sg_listener_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_cursor_color,sg_cursor_color),H_sg_cursor_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selection_color,sg_selection_color),H_sg_selection_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_zoom_color,sg_zoom_color),H_sg_zoom_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_position_color,sg_position_color),H_sg_position_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_highlight_color,sg_highlight_color),H_sg_highlight_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_enved_waveform_color,sg_enved_waveform_color),H_sg_enved_waveform_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_selected_mix_color,sg_selected_mix_color),H_sg_selected_mix_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_text_focus_color,sg_text_focus_color),H_sg_text_focus_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_filter_waveform_color,sg_filter_waveform_color),H_sg_filter_waveform_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_mix_color,sg_mix_color),H_sg_mix_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_pushed_button_color,sg_pushed_button_color),H_sg_pushed_button_color);
  DEFINE_PROC(gh_new_procedure0_0(Sg_sash_color,sg_sash_color),H_sg_sash_color);

  scm_add_feature("snd-guile-gtk");
}

#endif
