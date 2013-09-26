#include "snd.h"
#include "snd-menu.h"


#if (!USE_NO_GUI)
void edit_menu_update(void)
{
  /* called when the "Edit" top level menu is clicked -- make sure all items reflect current Snd state */
  snd_info *selected_sp = NULL, *any_sp = NULL;
  chan_info *cp = NULL;
  bool selection_p = false, region_p = false, file_p = false, undoable_edit_p = false, redoable_edit_p = false;
  selected_sp = selected_sound();
  if (selected_sp) 
    {
      file_p = true;
      cp = any_selected_channel(selected_sp);
      any_sp = selected_sp;
    }
  else 
    {
      any_sp = any_selected_sound();
      if (any_sp)
	{
	  cp = any_selected_channel(any_sp);
	  file_p = true;
	}
    }
  selection_p = selection_is_active();
  region_p = region_ok(region_list_position_to_id(0));
  if (cp)
    {
      undoable_edit_p = (cp->edit_ctr > 0);
      redoable_edit_p = (!(((cp->edit_ctr + 1) == cp->edit_size) || 
			   (!(cp->edits[cp->edit_ctr + 1]))));
    }
  
  /* is there an open sound? */
  set_sensitive(edit_header_menu, file_p);
#if HAVE_EXTENSION_LANGUAGE
  set_sensitive(edit_find_menu, file_p);
#endif
  set_sensitive(edit_select_all_menu, file_p);

  /* is there an active selection? */
  set_sensitive(edit_cut_menu, selection_p);
#if WITH_AUDIO
  set_sensitive(edit_play_menu, selection_p);
#endif
  set_sensitive(edit_mix_menu, selection_p);
  set_sensitive(edit_save_as_menu, selection_p);
  set_sensitive(edit_unselect_menu, selection_p);

  /* is there an undoable edit? */
  set_sensitive(edit_undo_menu, undoable_edit_p);

  /* is there a redoable edit? */
  set_sensitive(edit_redo_menu, redoable_edit_p);

  /* does paste make any sense? */
  set_sensitive(edit_paste_menu, (file_p) && (selection_p || region_p));  
 
  /* make sure edit-header menu option label correctly reflects current selected sound header type */
  if (any_sp)
    set_menu_label(edit_header_menu, (any_sp->hdr->type == MUS_RAW) ? "Add Header" : "Edit Header");
}


void view_menu_update(void)
{
  /* are there any viewable regions? are we even using them? */
  if ((snd_regions() == 0) &&
      (!(selection_creates_region(ss))))
    {
      deactivate_widget(view_region_menu);
    }
  else
    {
      set_sensitive(view_region_menu, snd_regions() > 0);
      activate_widget(view_region_menu);
    }

  /* graph_style */
  set_sensitive(view_lines_menu,          graph_style(ss) != GRAPH_LINES);
  set_sensitive(view_dots_menu,           graph_style(ss) != GRAPH_DOTS);
  set_sensitive(view_filled_menu,         graph_style(ss) != GRAPH_FILLED);
  set_sensitive(view_dots_and_lines_menu, graph_style(ss) != GRAPH_DOTS_AND_LINES);
  set_sensitive(view_lollipops_menu,      graph_style(ss) != GRAPH_LOLLIPOPS);

  /* x axis style */
  set_sensitive(view_x_axis_seconds_menu,    x_axis_style(ss) != X_AXIS_IN_SECONDS);
  set_sensitive(view_x_axis_beats_menu,      x_axis_style(ss) != X_AXIS_IN_BEATS);
  set_sensitive(view_x_axis_measures_menu,   x_axis_style(ss) != X_AXIS_IN_MEASURES);
  set_sensitive(view_x_axis_samples_menu,    x_axis_style(ss) != X_AXIS_IN_SAMPLES);
  set_sensitive(view_x_axis_percentage_menu, x_axis_style(ss) != X_AXIS_AS_PERCENTAGE);

  /* show y zero label */
  set_menu_label(view_zero_menu, (show_y_zero(ss)) ? "Hide Y = 0" : "Show Y = 0");

  /* verbose cursor label */
  set_menu_label(view_cursor_menu, (verbose_cursor(ss)) ? "Silent cursor" : "Verbose cursor");

#if HAVE_EXTENSION_LANGUAGE
  /* inset graph label */
  set_menu_label(view_inset_menu, (with_inset_graph(ss)) ? "Without inset graph" : "With inset graph");
#endif

  /* channel style */
  set_sensitive(view_combine_separate_menu,     channel_style(ss) != CHANNELS_SEPARATE);
  set_sensitive(view_combine_combined_menu,     channel_style(ss) != CHANNELS_COMBINED);
  set_sensitive(view_combine_superimposed_menu, channel_style(ss) != CHANNELS_SUPERIMPOSED);

  /* show axes */
  set_sensitive(view_no_axes_menu,                show_axes(ss) != SHOW_NO_AXES);
  set_sensitive(view_just_x_axis_menu,            show_axes(ss) != SHOW_X_AXIS);
  set_sensitive(view_just_x_axis_unlabelled_menu, show_axes(ss) != SHOW_X_AXIS_UNLABELLED);
  set_sensitive(view_all_axes_menu,               show_axes(ss) != SHOW_ALL_AXES);
  set_sensitive(view_all_axes_unlabelled_menu,    show_axes(ss) != SHOW_ALL_AXES_UNLABELLED);
  set_sensitive(view_bare_x_axis_menu,            show_axes(ss) != SHOW_BARE_X_AXIS);

#if HAVE_EXTENSION_LANGUAGE && USE_MOTIF
  /* make sure listener menu option label correctly reflects current listener state */
  set_menu_label(view_listener_menu, (listener_is_visible()) ? "Hide listener" : "Show listener");
#endif

  set_menu_label(view_controls_menu, (in_show_controls(ss)) ? "Hide controls" : "Show controls");

  /* set_sensitive(view_files_menu, get_view_files_end() >= 0); */

  /* zoom focus style */
  set_sensitive(view_focus_left_menu,   zoom_focus_style(ss) != ZOOM_FOCUS_LEFT);
  set_sensitive(view_focus_right_menu,  zoom_focus_style(ss) != ZOOM_FOCUS_RIGHT);
  set_sensitive(view_focus_middle_menu, zoom_focus_style(ss) != ZOOM_FOCUS_MIDDLE);
  set_sensitive(view_focus_active_menu, zoom_focus_style(ss) != ZOOM_FOCUS_ACTIVE);

  /* grid menu label */
  set_menu_label(view_grid_menu, (show_grid(ss) == WITH_GRID) ? "Without grid" : "With grid");
}


void file_menu_update(void)
{
  snd_info *any_sp = NULL;
  bool file_p = false, edits_p = false;

  any_sp = any_selected_sound();
  if (any_sp)
    {
      edits_p = has_unsaved_edits(any_sp);
      file_p = true;
    }

  set_sensitive(file_close_menu, file_p);
  set_sensitive(file_print_menu, file_p);
  set_sensitive(file_mix_menu, file_p);
  set_sensitive(file_insert_menu, file_p);
  set_sensitive(file_save_as_menu, file_p);
  set_sensitive(file_update_menu, file_p);

  set_sensitive(file_save_menu, edits_p);
  set_sensitive(file_revert_menu, edits_p);

  if (ss->active_sounds > 1)
    activate_widget(file_close_all_menu);
  else deactivate_widget(file_close_all_menu);
}
#endif


void reflect_file_revert_in_label(snd_info *sp)
{
#if (!USE_NO_GUI)
  bool editing;
  editing = has_unsaved_edits(sp);
  if (!editing)
    set_sound_pane_file_label(sp, shortname_indexed(sp));
#endif
}


static void file_update(snd_info *sp)
{
  /* here we should only update files that have changed on disk */
  if ((sp) && (sp->edited_region == NULL) &&
      ((sp->need_update) || 
       (file_write_date(sp->filename) != sp->write_date)))
    {
      redirect_everything_to(printout_to_status_area, (void *)sp);
      snd_update(sp);
      redirect_everything_to(NULL, NULL);
    }
}


void update_file_from_menu(void)
{
  for_each_sound(file_update);
}


void revert_file_from_menu(void)
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      int i;
      for (i = 0; i < sp->nchans; i++) 
	revert_edits(sp->chans[i]);
      reflect_file_revert_in_label(sp);
    }
}


static bool save_state_error_p = false;

static void save_state_from_menu_error_handler(const char *msg, void *ignore)
{
  snd_warning_without_format(msg);
  save_state_error_p = true;
}


void save_state_from_menu(void)
{
  if (save_state_file(ss))
    {
      save_state_error_p = false;
      redirect_everything_to(save_state_from_menu_error_handler, NULL);
      save_state(save_state_file(ss));
      redirect_everything_to(NULL, NULL);
      if (!save_state_error_p)
	{
	  if (any_selected_sound())
	    status_report(any_selected_sound(), "saved state in %s", save_state_file(ss));
	}
    }
  else 
    {
      snd_warning_without_format("can't save state: save-state-file is null");
    }
}



/* ---------------- extlang tie-ins ---------------- */

static XEN snd_no_such_menu_error(const char *caller, XEN id)
{
  XEN_ERROR(XEN_ERROR_TYPE("no-such-menu"),
	    XEN_LIST_3(C_TO_XEN_STRING("~A: no such menu, ~A"),
		       C_TO_XEN_STRING(caller),
		       id));
  return(XEN_FALSE);
}


static XEN *menu_functions = NULL;
static int *menu_functions_loc = NULL;
static int callbacks_size = 0;
static int callb = 0;
#define CALLBACK_INCR 16


static int make_callback_slot(void)
{
  int old_callb, i;
  for (i = 0; i < callb; i++)
    if (XEN_FALSE_P(menu_functions[i]))
      return(i);
  if (callbacks_size == callb)
    {
      callbacks_size += CALLBACK_INCR;
      if (callb == 0)
	{
	  menu_functions = (XEN *)calloc(callbacks_size, sizeof(XEN));
	  for (i = 0; i < callbacks_size; i++) menu_functions[i] = XEN_UNDEFINED;
	  menu_functions_loc = (int *)calloc(callbacks_size, sizeof(int));
	  for (i = 0; i < callbacks_size; i++) menu_functions_loc[i] = NOT_A_GC_LOC;
	}
      else 
	{
	  menu_functions = (XEN *)realloc(menu_functions, callbacks_size * sizeof(XEN));
	  for (i = callbacks_size - CALLBACK_INCR; i < callbacks_size; i++) menu_functions[i] = XEN_UNDEFINED;
	  menu_functions_loc = (int *)realloc(menu_functions_loc, callbacks_size * sizeof(int));
	  for (i = callbacks_size - CALLBACK_INCR; i < callbacks_size; i++) menu_functions_loc[i] = NOT_A_GC_LOC;
	}
    }
  old_callb = callb;
  callb++;
  return(old_callb);
}


static void add_callback(int slot, XEN callback)
{
  if ((slot >= 0) && (slot < callbacks_size))
    {
      menu_functions[slot] = callback;
      menu_functions_loc[slot] = snd_protect(callback);
    }
}


void unprotect_callback(int slot)
{
  /* called only if menu is being removed */
  if ((slot >= 0) && (slot < callbacks_size))
    {
      if (XEN_PROCEDURE_P(menu_functions[slot]))
	{
	  snd_unprotect_at(menu_functions_loc[slot]);
	  menu_functions_loc[slot] = NOT_A_GC_LOC;
	}
      menu_functions[slot] = XEN_FALSE;  /* not XEN_UNDEFINED -- need a way to distinguish "no callback" from "recyclable slot" */
    }
}


static XEN gl_add_to_main_menu(XEN label, XEN callback)
{
  #define H_add_to_main_menu "(" S_add_to_main_menu " label :optional callback): adds label to the main (top-level) menu, returning its index"
  int slot = -1;
  XEN_ASSERT_TYPE(XEN_STRING_P(label), label, 1, S_add_to_main_menu, "a string");
  slot = make_callback_slot();
  if (XEN_BOUND_P(callback))
    {
      char *err;
      err = procedure_ok(callback, 0, S_add_to_main_menu, "menu callback", 2);
      if (err == NULL)
	add_callback(slot, callback);
      else 
	{
	  XEN errm;
	  errm = C_TO_XEN_STRING(err);
	  free(err);
	  return(snd_bad_arity_error(S_add_to_main_menu, errm, callback));
	}
    }
  else menu_functions[slot] = XEN_UNDEFINED;
  return(C_TO_XEN_INT(g_add_to_main_menu((char *)XEN_TO_C_STRING(label), slot)));
}


static XEN gl_add_to_menu(XEN menu, XEN label, XEN callback, XEN gpos)
{
  #define H_add_to_menu "(" S_add_to_menu " menu label func :optional position): adds label to menu (a main menu index), invokes \
func (a function of no args) when the new menu is activated. Returns the new menu label widget."

#if (!USE_NO_GUI)
  widget_t result;
  char *errmsg = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(label) || XEN_FALSE_P(label), label, 2, S_add_to_menu, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, 1, S_add_to_menu, "an integer");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(callback) || XEN_FALSE_P(callback), callback, 3, S_add_to_menu, "a procedure");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(gpos), gpos, 4, S_add_to_menu, "an integer");

  /* fprintf(stderr, "add-to-menu %s\n", XEN_AS_STRING(XEN_CAR(callback))); */

  if (XEN_PROCEDURE_P(callback))
    errmsg = procedure_ok(callback, 0, S_add_to_menu, "menu callback", 3);
  if (errmsg == NULL)
    {
      int slot = -1, m, position = -1;

      m = XEN_TO_C_INT(menu);
      if (m < 0)
	return(snd_no_such_menu_error(S_add_to_menu, menu));

      if (XEN_PROCEDURE_P(callback)) slot = make_callback_slot();
      if (XEN_INTEGER_P(gpos)) position = XEN_TO_C_INT(gpos);

      result = g_add_to_menu(m,
			     (XEN_FALSE_P(label)) ? NULL : XEN_TO_C_STRING(label),
			     slot,
			     position);
      if (result == NULL)
	return(snd_no_such_menu_error(S_add_to_menu, menu));
      if (XEN_PROCEDURE_P(callback)) add_callback(slot, callback);
    }
  else 
    {
      XEN errm;
      errm = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      return(snd_bad_arity_error(S_add_to_menu, errm, callback));
    }
  return(XEN_WRAP_WIDGET(result));
#else
  return(XEN_FALSE);
#endif
}


void g_menu_callback(int callb)
{
  if ((callb >= 0) && (XEN_BOUND_P(menu_functions[callb])))
    XEN_CALL_0(menu_functions[callb], "menu callback func");
}


static XEN gl_remove_from_menu(XEN menu, XEN label)
{
  #define H_remove_from_menu "(" S_remove_from_menu " menu label): removes menu item label from menu"
  int m;

  XEN_ASSERT_TYPE(XEN_STRING_P(label), label, 2, S_remove_from_menu, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, 1, S_remove_from_menu, "an integer");

  m = XEN_TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error(S_remove_from_menu, menu));
  return(C_TO_XEN_INT(g_remove_from_menu(m, XEN_TO_C_STRING(label))));
}


static XEN g_main_menu(XEN which)
{
  #define H_main_menu "(" S_main_menu " menu): the top-level menu widget referred to by menu"
  int which_menu;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(which), which, 1, S_main_menu, "an integer");
  which_menu = XEN_TO_C_INT(which);
  if ((which_menu < 0) || (which_menu >= MAX_MAIN_MENUS))
    XEN_ERROR(XEN_ERROR_TYPE("no-such-menu"),
	      XEN_LIST_2(C_TO_XEN_STRING(S_main_menu ": no such menu, ~A"),
			 which));
  return(XEN_WRAP_WIDGET(menu_widget(which_menu)));
}


XEN_ARGIFY_2(gl_add_to_main_menu_w, gl_add_to_main_menu)
XEN_ARGIFY_4(gl_add_to_menu_w, gl_add_to_menu)
XEN_NARGIFY_2(gl_remove_from_menu_w, gl_remove_from_menu)
XEN_NARGIFY_1(g_main_menu_w, g_main_menu)

void g_init_menu(void)
{
  XEN_DEFINE_PROCEDURE(S_add_to_main_menu,  gl_add_to_main_menu_w,  1, 1, 0, H_add_to_main_menu);
  XEN_DEFINE_PROCEDURE(S_add_to_menu,       gl_add_to_menu_w,       3, 1, 0, H_add_to_menu);
  XEN_DEFINE_PROCEDURE(S_remove_from_menu,  gl_remove_from_menu_w,  2, 0, 0, H_remove_from_menu);
  XEN_DEFINE_PROCEDURE(S_main_menu,         g_main_menu_w,          1, 0, 0, H_main_menu);
}
