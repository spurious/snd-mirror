#include "snd.h"

void reflect_file_open_in_menu (void)
{
  set_sensitive(file_close_menu(), true);
  set_sensitive(file_print_menu(), true);
  set_sensitive(file_mix_menu(), true);
  set_sensitive(file_save_as_menu(), true);
  set_sensitive(file_update_menu(), true);
#if USE_MOTIF  
  set_sensitive(view_equalize_panes_menu(), true);
#endif
  set_sensitive(edit_header_menu(), true);
  set_sensitive(edit_find_menu(), true);
  set_sensitive(edit_select_all_menu(), true);
  if (popup_menu_exists())
    {
#if USE_MOTIF
      set_sensitive(popup_equalize_panes_menu(), true);
#endif
      set_sensitive(popup_play_menu(), true);
      set_sensitive(popup_info_menu(), true);
    }
}

void reflect_file_change_in_menu (void)
{
  set_sensitive(file_save_menu(), true);
  set_sensitive(file_revert_menu(), true);
  set_sensitive(edit_undo_menu(), true);
  if (popup_menu_exists())
    {
      set_sensitive(popup_undo_menu(), true);
      set_sensitive(popup_save_menu(), true);
      set_sensitive(popup_play_menu(), true);
    }
}

void reflect_file_lack_in_menu (void)
{
  set_sensitive(file_close_menu(), false);
  set_sensitive(file_save_as_menu(), false);
  set_sensitive(file_save_menu(), false);
  set_sensitive(file_revert_menu(), false);
  set_sensitive(file_print_menu(), false);
  set_sensitive(file_mix_menu(), false);
  set_sensitive(file_update_menu(), false);
  set_sensitive(edit_undo_menu(), false);
  set_sensitive(edit_redo_menu(), false);
#if USE_MOTIF
  set_sensitive(view_equalize_panes_menu(), false);
#endif
  set_sensitive(edit_header_menu(), false);
  set_sensitive(edit_find_menu(), false);
  set_sensitive(edit_select_all_menu(), false);
  if (popup_menu_exists())
    {
      set_sensitive(popup_undo_menu(), false);
      set_sensitive(popup_redo_menu(), false);
      set_sensitive(popup_save_menu(), false);
      set_sensitive(popup_play_menu(), false);
      set_sensitive(popup_info_menu(), false);
#if USE_MOTIF
      set_sensitive(popup_equalize_panes_menu(), false);
#endif
    }
}

void reflect_equalize_panes_in_menu(bool on)
{
#if USE_MOTIF
  set_sensitive(view_equalize_panes_menu(), on);
#endif
}

static bool find_any_edits (chan_info *cp, void *ignore)
{
  return(cp->edit_ctr > 0);
}

static bool find_any_possible_edits (chan_info *cp, void *ignore)
{
  return(cp->edit_size > 0);
}

void reflect_file_revert_in_menu (void)
{
  bool editing;
  editing = map_over_chans(find_any_edits, NULL);
  if (!editing)
    {
      set_sensitive(file_save_menu(), false);
      set_sensitive(file_revert_menu(), false);
      set_sensitive(edit_undo_menu(), false);
      if (popup_menu_exists())
	{
	  set_sensitive(popup_undo_menu(), false);
	  set_sensitive(popup_save_menu(), false);
	}
    }
  set_sensitive(edit_redo_menu(), true);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(), true);
}

void reflect_file_save_in_menu (void)
{
  bool editing;
  editing = map_over_chans(find_any_edits, NULL);
  if (!editing)
    {
      set_sensitive(file_save_menu(), false);
      set_sensitive(file_revert_menu(), false);
      set_sensitive(edit_undo_menu(), false);
      if (popup_menu_exists())
	{
	  set_sensitive(popup_undo_menu(), false);
	  set_sensitive(popup_save_menu(), false);
	  set_sensitive(popup_redo_menu(), false);
	}
      set_sensitive(edit_redo_menu(), false);
    }
  editing = map_over_chans(find_any_possible_edits, NULL);
}

void reflect_file_revert_in_label (snd_info *sp)
{
  if (sp->sgx)
    {
      bool editing;
      editing = map_over_sound_chans(sp, find_any_edits, NULL);
      if (!editing)
	set_sound_pane_file_label(sp, shortname_indexed(sp));
    }
}

void reflect_no_more_redo_in_menu(void)
{
  set_sensitive(edit_redo_menu(), false);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(), false);
}

void reflect_edit_with_selection_in_menu (void)
{
  set_sensitive(edit_cut_menu(), true);
  set_sensitive(edit_paste_menu(), true);
  set_sensitive(edit_play_menu(), true);
  set_sensitive(edit_mix_menu(), true);
  set_sensitive(edit_save_as_menu(), true);
  enved_reflect_selection(true);
}

void reflect_edit_without_selection_in_menu (void)
{
  set_sensitive(edit_cut_menu(), false);
  set_sensitive(edit_paste_menu(), false);
  set_sensitive(edit_mix_menu(), false);
  if ((!(selection_is_active())) && 
      (!(region_ok(region_list_position_to_id(0))))) 
    set_sensitive(edit_paste_menu(), false);
  set_sensitive(edit_play_menu(), false);
  set_sensitive(edit_save_as_menu(), false);
  enved_reflect_selection(false);
}

void reflect_undo_in_menu(void)
{
  set_sensitive(edit_redo_menu(), true);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(), true);
}

void reflect_redo_in_menu(void)
{
  set_sensitive(edit_undo_menu(), true);
  if (popup_menu_exists()) set_sensitive(popup_undo_menu(), true);
  reflect_file_change_in_menu();
}

void reflect_undo_ok_in_menu(void)
{
  set_sensitive(edit_undo_menu(), true);
  if (popup_menu_exists()) set_sensitive(popup_undo_menu(), true);
}

void reflect_undo_or_redo_in_menu(chan_info *cp)
{
  if ((cp) && (cp->cgx))
    {
      bool undoable, redoable;
      undoable = (cp->edit_ctr > 0);
      redoable = (!(((cp->edit_ctr + 1) == cp->edit_size) || 
		    (!(cp->edits[cp->edit_ctr + 1]))));
      set_sensitive(edit_undo_menu(), undoable);
      if (popup_menu_exists()) set_sensitive(popup_undo_menu(), undoable);
      set_sensitive(edit_redo_menu(), redoable);
      if (popup_menu_exists()) set_sensitive(popup_redo_menu(), redoable);
    }
}

void reflect_regions_in_menu(void)
{
  set_sensitive(view_region_menu(), true);
}

void reflect_no_regions_in_menu(void)
{
  set_sensitive(view_region_menu(), false);
}

void reflect_raw_open_in_menu(void)
{
  set_sensitive(file_open_menu(), true);
  set_sensitive(file_view_menu(), true);
  set_sensitive(file_new_menu(), true);
}

void reflect_raw_pending_in_menu(void)
{
  set_sensitive(file_open_menu(), false);
  set_sensitive(file_view_menu(), false);
  set_sensitive(file_new_menu(), false);
}

void close_file_from_menu(void)
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) snd_close_file(sp);
}

void save_file_from_menu(void)
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) save_edits(sp, NULL);
}

static void file_update(snd_info *sp, void *ptr)
{
  /* here we should only update files that have changed on disk */
  if ((sp) && (sp->edited_region == NULL) &&
      ((sp->need_update) || 
       (file_write_date(sp->filename) != sp->write_date)))
    snd_update(sp);
}

void update_file_from_menu(void)
{
  for_each_sound(file_update, NULL);
}

static char *output_name(void);
static int new_ctr = 0;

snd_info *new_file_from_menu(void)
{
  char *new_file_name = NULL, *extension = NULL, *new_comment = NULL;
  int header_type, data_format, chans, srate;
  snd_info *sp = NULL;
  new_file_name = output_name();
  header_type = default_output_type(ss);
  if (new_file_name == NULL)
    {
      new_file_name = (char *)CALLOC(64, sizeof(char));
      switch (header_type)
	{
	case MUS_AIFC: case MUS_AIFF: extension = "aiff"; break;
	case MUS_RIFF:                extension = "wav";  break;
	default:                      extension = "snd";  break;
	}
      mus_snprintf(new_file_name, 64, _("new-%d.%s"), new_ctr++, extension);
    }
  chans = default_output_chans(ss);
  data_format = default_output_format(ss);
  srate = default_output_srate(ss);
  new_comment = output_comment(NULL);
  mus_sound_forget(new_file_name);
  sp = make_new_file_dialog(new_file_name, header_type, data_format, srate, chans, new_comment);
  if (new_comment) FREE(new_comment);
  if (new_file_name) FREE(new_file_name);
  return(sp);
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
      reflect_file_revert_in_menu();
    }
}

void exit_from_menu(void)
{
  if (snd_exit_cleanly(false))
    snd_exit(1);
}

void save_options_from_menu(void)
{
  if ((save_options() == 0) && 
      (any_selected_sound()))
    report_in_minibuffer(any_selected_sound(), _("saved options in %s"), ss->init_file);
}

void save_state_from_menu(void)
{
  if ((save_state_file(ss)) && 
      (save_state(save_state_file(ss)) == 0) && 
      (any_selected_sound()))
    report_in_minibuffer(any_selected_sound(), _("saved state in %s"), save_state_file(ss));
}

static void chans_graph_style(chan_info *cp, void *ptr) 
{
  graph_style_t style = (*((graph_style_t *)ptr)); 
  cp->time_graph_style = style;
  cp->lisp_graph_style = style;
  cp->transform_graph_style = style;
  update_graph(cp);
}

void set_graph_style(graph_style_t val)
{
  switch (graph_style(ss))
    {
    case GRAPH_LINES:          set_sensitive(view_lines_menu(), true);          break;
    case GRAPH_DOTS:           set_sensitive(view_dots_menu(), true);           break;
    case GRAPH_FILLED:         set_sensitive(view_filled_menu(), true);         break;
    case GRAPH_DOTS_AND_LINES: set_sensitive(view_dots_and_lines_menu(), true); break;
    case GRAPH_LOLLIPOPS:      set_sensitive(view_lollipops_menu(), true);      break;
    }
  in_set_graph_style(val);
  for_each_chan_1(chans_graph_style, (void *)(&val));
  switch (val)
    {
    case GRAPH_LINES:          set_sensitive(view_lines_menu(), false);          break;
    case GRAPH_DOTS:           set_sensitive(view_dots_menu(), false);           break;
    case GRAPH_FILLED:         set_sensitive(view_filled_menu(), false);         break;
    case GRAPH_DOTS_AND_LINES: set_sensitive(view_dots_and_lines_menu(), false); break;
    case GRAPH_LOLLIPOPS:      set_sensitive(view_lollipops_menu(), false);      break;
    }
}

static void chans_marks(chan_info *cp, void *ptr)
{
  cp->show_marks = (*((bool *)ptr));
  update_graph(cp);
}

void set_show_marks(bool val)
{
  in_set_show_marks(val);
  for_each_chan_1(chans_marks, (void *)(&val));
}

static void chans_zero(chan_info *cp, void *ptr)
{
  cp->show_y_zero = (*((bool *)ptr));
  update_graph(cp);
}

void set_show_y_zero(bool val)
{
  in_set_show_y_zero(val);
  if (view_zero_menu())
    {
      set_menu_label(view_zero_menu(), 
		     (val) ? _("Hide Y = 0") : _("Show Y = 0"));
      for_each_chan_1(chans_zero, (void *)(&val));
    }
}

static void clrmini(snd_info *sp, void *ignore) {clear_minibuffer(sp);}
static void chans_verbose_cursor(chan_info *cp, void *ptr) 
{
  cp->verbose_cursor = (*((bool *)ptr));
  update_graph(cp);
}

void set_verbose_cursor(bool val)
{
  in_set_verbose_cursor(val);
  if (val == 0) for_each_sound(clrmini, NULL);
  for_each_chan_1(chans_verbose_cursor, (void *)(&val));
  if (view_cursor_menu())
    set_menu_label(view_cursor_menu(), 
		   (val) ? _("Silent cursor") : _("Verbose cursor"));
}

void set_view_ctrls_label(const char *lab)
{
  set_menu_label(view_ctrls_menu(), lab);
}

void set_view_listener_label(const char *lab)
{
  set_menu_label(view_listener_menu(), lab);
}

void activate_focus_menu(zoom_focus_t new_focus)
{
  if (options_focus_left_menu())
    {
      switch (zoom_focus_style(ss))
	{
	case ZOOM_FOCUS_LEFT:   set_sensitive(options_focus_left_menu(), true);   break;
	case ZOOM_FOCUS_RIGHT:  set_sensitive(options_focus_right_menu(), true);  break;
	case ZOOM_FOCUS_MIDDLE: set_sensitive(options_focus_middle_menu(), true); break;
	default:                set_sensitive(options_focus_active_menu(), true); break;
	}
    }
  set_zoom_focus_style(new_focus);
  if (options_focus_left_menu())
    {
      switch (new_focus)
	{
	case ZOOM_FOCUS_LEFT:   set_sensitive(options_focus_left_menu(), false);   break;
	case ZOOM_FOCUS_RIGHT:  set_sensitive(options_focus_right_menu(), false);  break;
	case ZOOM_FOCUS_MIDDLE: set_sensitive(options_focus_middle_menu(), false); break;
	default:                set_sensitive(options_focus_active_menu(), false); break;
	}
    }
}  

void chans_x_axis_style(chan_info *cp, void *ptr)
{
  axis_info *ap;
  x_axis_style_t new_style = (*((x_axis_style_t *)ptr));
  ap = cp->axis;
  cp->x_axis_style = new_style;
  if (ap)
    {
      if (ap->xlabel) FREE(ap->xlabel);
      if (ap->default_xlabel)
	ap->xlabel = copy_string(ap->default_xlabel);
      else
	{
	  switch (new_style)
	    {
	    case X_AXIS_IN_BEATS:      ap->xlabel = copy_string(_("time (beats)"));    break;
	    case X_AXIS_IN_MEASURES:   ap->xlabel = copy_string(_("time (measures)")); break;
	    case X_AXIS_IN_SAMPLES:    ap->xlabel = copy_string(_("time (samples)"));  break;
	    case X_AXIS_AS_PERCENTAGE: ap->xlabel = copy_string(_("time (percent)"));  break;
	    default:                   ap->xlabel = copy_string(_("time"));            break;
	    }
	}
      update_graph(cp);
    }
} 

static void reflect_x_axis_unit_change_in_menu(x_axis_style_t oldval, x_axis_style_t newval)
{
  switch (oldval)
    {
    case X_AXIS_IN_SECONDS:    set_sensitive(view_x_axis_seconds_menu(), true);    break;
    case X_AXIS_IN_BEATS:      set_sensitive(view_x_axis_beats_menu(), true);      break;
    case X_AXIS_IN_MEASURES:   set_sensitive(view_x_axis_measures_menu(), true);   break;
    case X_AXIS_IN_SAMPLES:    set_sensitive(view_x_axis_samples_menu(), true);    break;
    case X_AXIS_AS_PERCENTAGE: set_sensitive(view_x_axis_percentage_menu(), true); break;
    }
  switch (newval)
    {
    case X_AXIS_IN_SECONDS:    set_sensitive(view_x_axis_seconds_menu(), false);    break;
    case X_AXIS_IN_BEATS:      set_sensitive(view_x_axis_beats_menu(), false);      break;
    case X_AXIS_IN_MEASURES:   set_sensitive(view_x_axis_measures_menu(), false);   break;
    case X_AXIS_IN_SAMPLES:    set_sensitive(view_x_axis_samples_menu(), false);    break;
    case X_AXIS_AS_PERCENTAGE: set_sensitive(view_x_axis_percentage_menu(), false); break;
    }
}
  
void set_x_axis_style(x_axis_style_t val)
{
  reflect_x_axis_unit_change_in_menu(x_axis_style(ss), val);
  in_set_x_axis_style(val);
  for_each_chan_1(chans_x_axis_style, (void *)(&val));
}

static void update_sound(snd_info *sp, void *ptr)
{
  if (sp)
    {
      switch (channel_style(ss))
	{
	case CHANNELS_SEPARATE:     separate_sound(sp);    break;
	case CHANNELS_COMBINED:     combine_sound(sp);     break;
	case CHANNELS_SUPERIMPOSED: superimpose_sound(sp); break;
	}
    }
}

void set_channel_style(channel_style_t val)
{
  switch (channel_style(ss))
    {
    case CHANNELS_SEPARATE:     set_sensitive(view_combine_separate_menu(), true);     break;
    case CHANNELS_COMBINED:     set_sensitive(view_combine_combined_menu(), true);     break;
    case CHANNELS_SUPERIMPOSED: set_sensitive(view_combine_superimposed_menu(), true); break;
    }
  in_set_channel_style(val);
  switch (val)
    {
    case CHANNELS_SEPARATE:     set_sensitive(view_combine_separate_menu(), false);     break;
    case CHANNELS_COMBINED:     set_sensitive(view_combine_combined_menu(), false);     break;
    case CHANNELS_SUPERIMPOSED: set_sensitive(view_combine_superimposed_menu(), false); break;
    }
  for_each_sound(update_sound, NULL);
  for_each_chan(update_graph);
}

static XEN snd_no_such_menu_error(const char *caller, XEN id)
{
  XEN_ERROR(NO_SUCH_MENU,
	    XEN_LIST_2(C_TO_XEN_STRING(caller),
		       id));
  return(XEN_FALSE);
}

static XEN output_name_hook;

static char *output_name(void)
{
  if (XEN_HOOKED(output_name_hook))
    {
      XEN result;
      XEN procs = XEN_HOOK_PROCEDURES (output_name_hook);
      while (XEN_NOT_NULL_P(procs))
	{
	  result = XEN_CALL_0(XEN_CAR(procs), S_output_name_hook);
	  if (XEN_STRING_P(result)) return(copy_string(XEN_TO_C_STRING(result)));
	  procs = XEN_CDR (procs);
	}
    }
  return(NULL);
}

static XEN g_save_state_file(void) 
{
  return(C_TO_XEN_STRING(save_state_file(ss)));
}

static void set_save_state_file(char *name)
{
  if (save_state_file(ss)) FREE(save_state_file(ss));
  in_set_save_state_file(copy_string(name));
  set_sensitive(options_save_state_menu(), (snd_strlen(name) > 0));
}

static XEN g_set_save_state_file(XEN val) 
{
  #define H_save_state_file "(" S_save_state_file "): the name of the saved state file (\"saved-snd." XEN_FILE_EXTENSION "\")"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_save_state_file, "a string"); 
  set_save_state_file(XEN_TO_C_STRING(val));
  return(C_TO_XEN_STRING(save_state_file(ss)));
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
	  menu_functions = (XEN *)CALLOC(callbacks_size, sizeof(XEN));
	  for (i = 0; i < callbacks_size; i++) menu_functions[i] = XEN_UNDEFINED;
	  menu_functions_loc = (int *)CALLOC(callbacks_size, sizeof(int));
	  for (i = 0; i < callbacks_size; i++) menu_functions_loc[i] = -1;
	}
      else 
	{
	  menu_functions = (XEN *)REALLOC(menu_functions, callbacks_size * sizeof(XEN));
	  for (i = callbacks_size - CALLBACK_INCR; i < callbacks_size; i++) menu_functions[i] = XEN_UNDEFINED;
	  menu_functions_loc = (int *)REALLOC(menu_functions_loc, callbacks_size * sizeof(int));
	  for (i = callbacks_size - CALLBACK_INCR; i < callbacks_size; i++) menu_functions_loc[i] = -1;
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
	  menu_functions_loc[slot] = -1;
	}
      menu_functions[slot] = XEN_FALSE;  /* not XEN_UNDEFINED -- need a way to distinguish "no callback" from "recyclable slot" */
    }
}

static XEN gl_add_to_main_menu(XEN label, XEN callback)
{
  #define H_add_to_main_menu "(" S_add_to_main_menu " label (callback #f)): adds label to the main (top-level) menu, returning its index"
  int slot = -1;
  XEN_ASSERT_TYPE(XEN_STRING_P(label), label, XEN_ARG_1, S_add_to_main_menu, "a string");
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
	  FREE(err);
	  return(snd_bad_arity_error(S_add_to_main_menu, errm, callback));
	}
    }
  else menu_functions[slot] = XEN_UNDEFINED;
  return(C_TO_XEN_INT(g_add_to_main_menu(XEN_TO_C_STRING(label), slot)));
}

static XEN gl_add_to_menu(XEN menu, XEN label, XEN callback, XEN gpos)
{
  #define H_add_to_menu "(" S_add_to_menu " menu label func (position #f): adds label to menu (a main menu index), invokes \
func (a function of no args) when the new menu is activated. Returns the new menu label widget."

  widget_t result;
  char *errmsg = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(label) || XEN_FALSE_P(label), label, XEN_ARG_2, S_add_to_menu, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, XEN_ARG_1, S_add_to_menu, "an integer");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(callback) || XEN_FALSE_P(callback), callback, XEN_ARG_3, S_add_to_menu, "a procedure");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(gpos), gpos, XEN_ARG_4, S_add_to_menu, "an integer");
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
#if (SGI) && (!(HAVE_EXTENSION_LANGUAGE)) && (!(defined(__GNUC__)))
			     /* SGI C-compiler thinks NULL:NULL can't be char*! */
			     (XEN_FALSE_P(label)) ? "" : XEN_TO_C_STRING(label),
#else
			     (XEN_FALSE_P(label)) ? NULL : XEN_TO_C_STRING(label),
#endif
			     slot,
			     position);
#if (!USE_NO_GUI)
      if (result == NULL)
	return(snd_no_such_menu_error(S_add_to_menu, menu));
#endif
      if (XEN_PROCEDURE_P(callback)) add_callback(slot, callback);
    }
  else 
    {
      XEN errm;
      errm = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(S_add_to_menu, errm, callback));
    }
  return(XEN_WRAP_WIDGET(result));
}

void g_snd_callback(int callb)
{
  if ((callb >= 0) && (XEN_BOUND_P(menu_functions[callb])))
    XEN_CALL_0(menu_functions[callb], "menu callback func");
}

static XEN gl_remove_from_menu(XEN menu, XEN label)
{
  #define H_remove_from_menu "(" S_remove_from_menu " menu label): removes menu item label from menu"
  int m;
  XEN_ASSERT_TYPE(XEN_STRING_P(label), label, XEN_ARG_2, S_remove_from_menu, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, XEN_ARG_1, S_remove_from_menu, "an integer");
  m = XEN_TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error(S_remove_from_menu, menu));
  return(C_TO_XEN_INT(g_remove_from_menu(m, XEN_TO_C_STRING(label))));
}

static XEN g_main_menu(XEN which)
{
  #define H_main_menu "(" S_main_menu " menu): the top-level menu widget referred to by menu"
  int which_menu;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(which), which, XEN_ONLY_ARG, S_main_menu, "an integer");
  which_menu = XEN_TO_C_INT(which);
  if ((which_menu < 0) || (which_menu >= MAX_MAIN_MENUS))
    XEN_ERROR(NO_SUCH_MENU,
	      XEN_LIST_2(C_TO_XEN_STRING(S_main_menu),
			 which));
  return(XEN_WRAP_WIDGET(menu_widget(which_menu)));
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_save_state_file_w, g_save_state_file)
XEN_NARGIFY_1(g_set_save_state_file_w, g_set_save_state_file)
XEN_ARGIFY_2(gl_add_to_main_menu_w, gl_add_to_main_menu)
XEN_ARGIFY_4(gl_add_to_menu_w, gl_add_to_menu)
XEN_NARGIFY_2(gl_remove_from_menu_w, gl_remove_from_menu)
XEN_NARGIFY_1(g_main_menu_w, g_main_menu)
#else
#define g_save_state_file_w g_save_state_file
#define g_set_save_state_file_w g_set_save_state_file
#define gl_add_to_main_menu_w gl_add_to_main_menu
#define gl_add_to_menu_w gl_add_to_menu
#define gl_remove_from_menu_w gl_remove_from_menu
#define g_main_menu_w g_main_menu
#endif

void g_init_menu(void)
{
  #define H_output_name_hook S_output_name_hook " (): called from the File:New dialog.  If it returns a filename, \
that name is the default that appears in the New File dialog."
  XEN_DEFINE_HOOK(output_name_hook, S_output_name_hook, 0, H_output_name_hook);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_state_file, g_save_state_file_w, H_save_state_file,
				   S_setB S_save_state_file, g_set_save_state_file_w, 0, 0, 1, 0);
  
  XEN_DEFINE_PROCEDURE(S_add_to_main_menu,  gl_add_to_main_menu_w,  1, 1, 0, H_add_to_main_menu);
  XEN_DEFINE_PROCEDURE(S_add_to_menu,       gl_add_to_menu_w,       3, 1, 0, H_add_to_menu);
  XEN_DEFINE_PROCEDURE(S_remove_from_menu,  gl_remove_from_menu_w,  2, 0, 0, H_remove_from_menu);
  XEN_DEFINE_PROCEDURE(S_main_menu,         g_main_menu_w,          1, 0, 0, H_main_menu);
}
