#include "snd.h"

void reflect_file_open_in_menu (void)
{
  set_sensitive(file_close_menu(), TRUE);
  set_sensitive(file_print_menu(), TRUE);
  set_sensitive(file_mix_menu(), TRUE);
  set_sensitive(file_save_as_menu(), TRUE);
  set_sensitive(file_update_menu(), TRUE);  
  set_sensitive(view_equalize_panes_menu(), TRUE);  
  set_sensitive(edit_header_menu(), TRUE);
  set_sensitive(edit_find_menu(), TRUE);
  set_sensitive(edit_select_all_menu(), TRUE);
  if (popup_menu_exists())
    {
      set_sensitive(popup_equalize_panes_menu(), TRUE);
      set_sensitive(popup_play_menu(), TRUE);
      set_sensitive(popup_info_menu(), TRUE);
    }
}

void reflect_file_change_in_menu (void)
{
  set_sensitive(file_save_menu(), TRUE);
  set_sensitive(file_revert_menu(), TRUE);
  set_sensitive(edit_undo_menu(), TRUE);
  if (popup_menu_exists())
    {
      set_sensitive(popup_undo_menu(), TRUE);
      set_sensitive(popup_save_menu(), TRUE);
      set_sensitive(popup_play_menu(), TRUE);
    }
}

void reflect_file_lack_in_menu (void)
{
  set_sensitive(file_close_menu(), FALSE);
  set_sensitive(file_save_as_menu(), FALSE);
  set_sensitive(file_save_menu(), FALSE);
  set_sensitive(file_revert_menu(), FALSE);
  set_sensitive(file_print_menu(), FALSE);
  set_sensitive(file_mix_menu(), FALSE);
  set_sensitive(file_update_menu(), FALSE);
  set_sensitive(edit_undo_menu(), FALSE);
  set_sensitive(edit_redo_menu(), FALSE);
  set_sensitive(view_equalize_panes_menu(), FALSE);
  set_sensitive(edit_header_menu(), FALSE);
  set_sensitive(edit_find_menu(), FALSE);
  set_sensitive(edit_select_all_menu(), FALSE);
  if (popup_menu_exists())
    {
      set_sensitive(popup_undo_menu(), FALSE);
      set_sensitive(popup_redo_menu(), FALSE);
      set_sensitive(popup_save_menu(), FALSE);
      set_sensitive(popup_play_menu(), FALSE);
      set_sensitive(popup_info_menu(), FALSE);
      set_sensitive(popup_equalize_panes_menu(), FALSE);
    }
}

void reflect_mix_in_menu(void)
{
  set_sensitive(view_mix_panel_menu(), (any_mix_id() != INVALID_MIX_ID));
}

void reflect_equalize_panes_in_menu(int on)
{
  set_sensitive(view_equalize_panes_menu(), on);
}

static int find_any_edits (chan_info *cp, void *ignore)
{
  return(cp->edit_ctr);
}

static int find_any_possible_edits (chan_info *cp, void *ignore)
{
  return(cp->edit_size);
}

void reflect_file_revert_in_menu (snd_state *ss)
{
  int editing;
  editing = map_over_chans(ss, find_any_edits, NULL);
  if (!editing)
    {
      set_sensitive(file_save_menu(), FALSE);
      set_sensitive(file_revert_menu(), FALSE);
      set_sensitive(edit_undo_menu(), FALSE);
      if (popup_menu_exists())
	{
	  set_sensitive(popup_undo_menu(), FALSE);
	  set_sensitive(popup_save_menu(), FALSE);
	}
    }
  set_sensitive(edit_redo_menu(), TRUE);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(), TRUE);
}

void reflect_file_save_in_menu (snd_state *ss)
{
  int editing;
  editing = map_over_chans(ss, find_any_edits, NULL);
  if (!editing)
    {
      set_sensitive(file_save_menu(), FALSE);
      set_sensitive(file_revert_menu(), FALSE);
      set_sensitive(edit_undo_menu(), FALSE);
      if (popup_menu_exists())
	{
	  set_sensitive(popup_undo_menu(), FALSE);
	  set_sensitive(popup_save_menu(), FALSE);
	  set_sensitive(popup_redo_menu(), FALSE);
	}
      set_sensitive(edit_redo_menu(), FALSE);
    }
  editing = map_over_chans(ss, find_any_possible_edits, NULL);
}

void reflect_file_revert_in_label (snd_info *sp)
{
  int editing;
  if (sp->sgx)
    {
      editing = map_over_sound_chans(sp, find_any_edits, NULL);
      if (!editing)
	{
	  set_sound_pane_file_label(sp, shortname_indexed(sp));
	  make_a_big_star_outa_me(sp->short_filename, 0);
	}
    }
}

void reflect_no_more_redo_in_menu(void)
{
  set_sensitive(edit_redo_menu(), FALSE);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(), FALSE);
}

void reflect_edit_with_selection_in_menu (void)
{
  set_sensitive(edit_cut_menu(), TRUE);
  set_sensitive(edit_paste_menu(), TRUE);
  set_sensitive(edit_play_menu(), TRUE);
  set_sensitive(edit_mix_menu(), TRUE);
  set_sensitive(edit_save_as_menu(), TRUE);
  enved_reflect_selection(TRUE);
}

void reflect_edit_without_selection_in_menu (void)
{
  set_sensitive(edit_cut_menu(), FALSE);
  set_sensitive(edit_paste_menu(), FALSE);
  set_sensitive(edit_mix_menu(), FALSE);
  if ((!(selection_is_active())) && 
      (!(region_ok(stack_position_to_id(0))))) 
    set_sensitive(edit_paste_menu(), FALSE);
  set_sensitive(edit_play_menu(), FALSE);
  set_sensitive(edit_save_as_menu(), FALSE);
  enved_reflect_selection(FALSE);
}

void reflect_undo_in_menu(void)
{
  set_sensitive(edit_redo_menu(), TRUE);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(), TRUE);
}

void reflect_redo_in_menu(void)
{
  set_sensitive(edit_undo_menu(), TRUE);
  if (popup_menu_exists()) set_sensitive(popup_undo_menu(), TRUE);
  reflect_file_change_in_menu();
}

void reflect_undo_ok_in_menu(void)
{
  set_sensitive(edit_undo_menu(), TRUE);
  if (popup_menu_exists()) set_sensitive(popup_undo_menu(), TRUE);
}

void reflect_undo_or_redo_in_menu(chan_info *cp)
{
  int undoable, redoable;
  if ((cp) && (cp->cgx))
    {
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
  set_sensitive(view_region_menu(), TRUE);
}

void reflect_no_regions_in_menu(void)
{
  set_sensitive(view_region_menu(), FALSE);
}

void reflect_raw_open_in_menu(void)
{
  set_sensitive(file_open_menu(), TRUE);
  set_sensitive(file_view_menu(), TRUE);
  set_sensitive(file_new_menu(), TRUE);
}

void reflect_raw_pending_in_menu(void)
{
  set_sensitive(file_open_menu(), FALSE);
  set_sensitive(file_view_menu(), FALSE);
  set_sensitive(file_new_menu(), FALSE);
}

void close_file_from_menu(snd_state *ss)
{
  snd_info *sp;
  sp = any_selected_sound(ss);
  if (sp) snd_close_file(sp, ss);
}

void save_file_from_menu(snd_state *ss)
{
  snd_info *sp;
  sp = any_selected_sound(ss);
  if (sp) save_edits(sp, NULL);
}

static void file_update(snd_info *sp, void *ptr)
{
  snd_state *ss = (snd_state *)ptr;
  /* here we should only update files that have changed on disk */
  if ((sp) && (sp->edited_region == NULL) &&
      ((sp->need_update) || 
       (file_write_date(sp->filename) != sp->write_date)))
    snd_update(ss, sp);
}

void update_file_from_menu(snd_state *ss)
{
  for_each_sound(ss, file_update, (void *)ss);
}

static char *output_name(void);
static int new_ctr = 0;

snd_info *new_file_from_menu(snd_state *ss)
{
  char *new_file_name = NULL, *extension = NULL, *new_comment = NULL;
  int header_type, data_format, chans, srate;
  snd_info *sp = NULL;
  new_file_name = output_name();
  header_type = default_output_type(ss);
  if (new_file_name == NULL)
    {
      new_file_name = (char *)CALLOC(MUS_MAX_FILE_NAME, sizeof(char));
      switch (header_type)
	{
	case MUS_AIFC: case MUS_AIFF: extension = "aiff"; break;
	case MUS_RIFF:                extension = "wav";  break;
	default:                      extension = "snd";  break;
	}
      mus_snprintf(new_file_name, MUS_MAX_FILE_NAME, "new-%d.%s", new_ctr++, extension);
    }
  chans = default_output_chans(ss);
  data_format = default_output_format(ss);
  srate = default_output_srate(ss);
  new_comment = output_comment(NULL);
  mus_sound_forget(new_file_name);
  sp = make_new_file_dialog(ss, new_file_name, header_type, data_format, srate, chans, new_comment);
  if (new_comment) FREE(new_comment);
  if (new_file_name) FREE(new_file_name);
  return(sp);
}

void revert_file_from_menu(snd_state *ss)
{
  snd_info *sp;
  int i;
  sp = any_selected_sound(ss);
  if (sp)
    {
      for (i = 0; i < sp->nchans; i++) 
	revert_edits(sp->chans[i], NULL);
      reflect_file_revert_in_label(sp);
      reflect_file_revert_in_menu(ss);
    }
}

void exit_from_menu(snd_state *ss)
{
  if (snd_exit_cleanly(ss, FALSE))
    snd_exit(1);
}

void save_options_from_menu(snd_state *ss)
{
  if ((save_options(ss) == 0) && 
      (any_selected_sound(ss)))
    report_in_minibuffer(any_selected_sound(ss), "saved options in %s", ss->init_file);
}

void save_state_from_menu(snd_state *ss)
{
  if ((save_state_file(ss)) && 
      (save_state(ss, save_state_file(ss)) == 0) && 
      (any_selected_sound(ss)))
    report_in_minibuffer(any_selected_sound(ss), "saved state in %s", save_state_file(ss));
}

static void chans_graph_style(chan_info *cp, void *ptr) 
{
  int style = (*((int *)ptr)); 
  cp->time_graph_style = style;
  cp->lisp_graph_style = style;
  cp->transform_graph_style = style;
  update_graph(cp);
}

void set_graph_style(snd_state *ss, int val)
{
  switch (graph_style(ss))
    {
    case GRAPH_LINES:          set_sensitive(view_lines_menu(), TRUE);          break;
    case GRAPH_DOTS:           set_sensitive(view_dots_menu(), TRUE);           break;
    case GRAPH_FILLED:         set_sensitive(view_filled_menu(), TRUE);         break;
    case GRAPH_DOTS_AND_LINES: set_sensitive(view_dots_and_lines_menu(), TRUE); break;
    case GRAPH_LOLLIPOPS:      set_sensitive(view_lollipops_menu(), TRUE);      break;
    }
  in_set_graph_style(ss, val);
  for_each_chan_1(ss, chans_graph_style, (void *)(&val));
  switch (val)
    {
    case GRAPH_LINES:          set_sensitive(view_lines_menu(), FALSE);          break;
    case GRAPH_DOTS:           set_sensitive(view_dots_menu(), FALSE);           break;
    case GRAPH_FILLED:         set_sensitive(view_filled_menu(), FALSE);         break;
    case GRAPH_DOTS_AND_LINES: set_sensitive(view_dots_and_lines_menu(), FALSE); break;
    case GRAPH_LOLLIPOPS:      set_sensitive(view_lollipops_menu(), FALSE);      break;
    }
}

static void chans_marks(chan_info *cp, void *ptr)
{
  cp->show_marks = (*((int *)ptr));
  update_graph(cp);
}

void set_show_marks(snd_state *ss, int val)
{
  in_set_show_marks(ss, val);
  for_each_chan_1(ss, chans_marks, (void *)(&val));
}

static void chans_zero(chan_info *cp, void *ptr)
{
  cp->show_y_zero = (*((int *)ptr));
  update_graph(cp);
}

void set_show_y_zero(snd_state *ss, int val)
{
  in_set_show_y_zero(ss, val);
  if (view_zero_menu())
    {
      set_menu_label(view_zero_menu(), 
		     (val) ? "Hide Y = 0" : "Show Y = 0");
      for_each_chan_1(ss, chans_zero, (void *)(&val));
    }
}

static void clrmini(snd_info *sp, void *ignore) {clear_minibuffer(sp);}
static void chans_verbose_cursor(chan_info *cp, void *ptr) {cp->verbose_cursor = (*((int *)ptr));}

void set_verbose_cursor(snd_state *ss, int val)
{
  in_set_verbose_cursor(ss, val);
  if (val == 0) for_each_sound(ss, clrmini, NULL);
  for_each_chan_1(ss, chans_verbose_cursor, (void *)(&val));
  if (view_cursor_menu())
    set_menu_label(view_cursor_menu(), 
		   (val) ? "Silent cursor" : "Verbose cursor");
}

void set_view_ctrls_label(const char *lab)
{
  set_menu_label(view_ctrls_menu(), lab);
}

void set_view_listener_label(const char *lab)
{
  set_menu_label(view_listener_menu(), lab);
}

void activate_focus_menu(snd_state *ss, int new_focus)
{
  if (options_focus_left_menu())
    {
      switch (zoom_focus_style(ss))
	{
	case ZOOM_FOCUS_LEFT:   set_sensitive(options_focus_left_menu(), TRUE);   break;
	case ZOOM_FOCUS_RIGHT:  set_sensitive(options_focus_right_menu(), TRUE);  break;
	case ZOOM_FOCUS_MIDDLE: set_sensitive(options_focus_middle_menu(), TRUE); break;
	case ZOOM_FOCUS_ACTIVE: set_sensitive(options_focus_active_menu(), TRUE); break;
	}
    }
  set_zoom_focus_style(ss, new_focus);
  if (options_focus_left_menu())
    {
      switch (new_focus)
	{
	case ZOOM_FOCUS_LEFT:   set_sensitive(options_focus_left_menu(), FALSE);   break;
	case ZOOM_FOCUS_RIGHT:  set_sensitive(options_focus_right_menu(), FALSE);  break;
	case ZOOM_FOCUS_MIDDLE: set_sensitive(options_focus_middle_menu(), FALSE); break;
	case ZOOM_FOCUS_ACTIVE: set_sensitive(options_focus_active_menu(), FALSE); break;
	}
    }
}  

void activate_speed_in_menu(snd_state *ss, int newval)
{
  if (options_speed_ratio_menu())
    {
      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO:    set_sensitive(options_speed_ratio_menu(), TRUE);    break;
	case SPEED_CONTROL_AS_SEMITONE: set_sensitive(options_speed_semitone_menu(), TRUE); break;
	default:                        set_sensitive(options_speed_float_menu(), TRUE);    break;
	}
    }
  set_speed_style(ss, newval);
  if (options_speed_ratio_menu())
    {
      switch (speed_control_style(ss))
	{
	case SPEED_CONTROL_AS_RATIO:    set_sensitive(options_speed_ratio_menu(), FALSE);    break;
	case SPEED_CONTROL_AS_SEMITONE: set_sensitive(options_speed_semitone_menu(), FALSE); break;
	default:                        set_sensitive(options_speed_float_menu(), FALSE);    break;
	}
    }
}

void chans_x_axis_style(chan_info *cp, void *ptr)
{
  axis_info *ap;
  int new_style = (*((int *)ptr));
  ap = cp->axis;
  cp->x_axis_style = new_style;
  if (ap)
    {
      if (ap->xlabel) FREE(ap->xlabel);
      switch (new_style)
	{
	case X_AXIS_IN_BEATS:      ap->xlabel = copy_string("time (beats)"); break;
	case X_AXIS_IN_SAMPLES:    ap->xlabel = copy_string("time (samples)"); break;
	case X_AXIS_AS_PERCENTAGE: ap->xlabel = copy_string("time (%)"); break;
	default:                   ap->xlabel = copy_string("time");         break;
	}
      update_graph(cp);
    }
} 

static void reflect_x_axis_unit_change_in_menu(int oldval, int newval)
{
  switch (oldval)
    {
    case X_AXIS_IN_SECONDS:    set_sensitive(view_x_axis_seconds_menu(), TRUE);    break;
    case X_AXIS_IN_BEATS:      set_sensitive(view_x_axis_beats_menu(), TRUE);      break;
    case X_AXIS_IN_SAMPLES:    set_sensitive(view_x_axis_samples_menu(), TRUE);    break;
    case X_AXIS_AS_PERCENTAGE: set_sensitive(view_x_axis_percentage_menu(), TRUE); break;
    default:                   set_sensitive(view_x_axis_seconds_menu(), TRUE);    break;
    }
  switch (newval)
    {
    case X_AXIS_IN_SECONDS:    set_sensitive(view_x_axis_seconds_menu(), FALSE);    break;
    case X_AXIS_IN_BEATS:      set_sensitive(view_x_axis_beats_menu(), FALSE);      break;
    case X_AXIS_IN_SAMPLES:    set_sensitive(view_x_axis_samples_menu(), FALSE);    break;
    case X_AXIS_AS_PERCENTAGE: set_sensitive(view_x_axis_percentage_menu(), FALSE); break;
    default:                   set_sensitive(view_x_axis_seconds_menu(), FALSE);    break;
    }
}
  
void set_x_axis_style(snd_state *ss, int val)
{
  reflect_x_axis_unit_change_in_menu(x_axis_style(ss), val);
  in_set_x_axis_style(ss, val);
  for_each_chan_1(ss, chans_x_axis_style, (void *)(&val));
}

static void update_sound(snd_info *sp, void *ptr)
{
  snd_state *ss = (snd_state *)ptr;
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

void set_channel_style(snd_state *ss, int val)
{
  switch (channel_style(ss))
    {
    case CHANNELS_SEPARATE:     set_sensitive(view_combine_separate_menu(), TRUE);     break;
    case CHANNELS_COMBINED:     set_sensitive(view_combine_combined_menu(), TRUE);     break;
    case CHANNELS_SUPERIMPOSED: set_sensitive(view_combine_superimposed_menu(), TRUE); break;
    }
  in_set_channel_style(ss, val);
  switch (val)
    {
    case CHANNELS_SEPARATE:     set_sensitive(view_combine_separate_menu(), FALSE);     break;
    case CHANNELS_COMBINED:     set_sensitive(view_combine_combined_menu(), FALSE);     break;
    case CHANNELS_SUPERIMPOSED: set_sensitive(view_combine_superimposed_menu(), FALSE); break;
    }
  for_each_sound(ss, update_sound, (void *)ss);
  for_each_chan(ss, update_graph);
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
#if HAVE_GUILE
      while (XEN_NOT_NULL_P(procs))
	{
	  result = XEN_CALL_0(XEN_CAR(procs), S_output_name_hook);
	  if (XEN_STRING_P(result)) return(copy_string(XEN_TO_C_STRING(result)));
	  procs = XEN_CDR (procs);
	}
#else
	  result = XEN_CALL_0(procs, S_output_name_hook);
	  if (XEN_STRING_P(result)) return(copy_string(XEN_TO_C_STRING(result)));
#endif
    }
  return(NULL);
}

static XEN g_save_state_file(void) 
{
  return(C_TO_XEN_STRING(save_state_file(get_global_state())));
}

static void set_save_state_file(snd_state *ss, char *name)
{
  if (save_state_file(ss)) FREE(save_state_file(ss));
  in_set_save_state_file(ss, copy_string(name));
  set_sensitive(options_save_state_menu(), (snd_strlen(name) > 0));
}

static XEN g_set_save_state_file(XEN val) 
{
  #define H_save_state_file "(" S_save_state_file ") -> name of saved state file (\"saved-snd." XEN_FILE_EXTENSION "\")"
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, "set-" S_save_state_file, "a string"); 
  ss = get_global_state();
  set_save_state_file(ss, XEN_TO_C_STRING(val));
  return(C_TO_XEN_STRING(save_state_file(ss)));
}

static XEN *menu_functions = NULL;
static int callbacks_size = 0;
static int callb = 0;
#define CALLBACK_INCR 16

static int make_callback_slot(void)
{
  int old_callb, i;
  if (callbacks_size == callb)
    {
      callbacks_size += CALLBACK_INCR;
      if (callb == 0)
	{
	  menu_functions = (XEN *)CALLOC(callbacks_size, sizeof(XEN));
	  for (i = 0; i < callbacks_size; i++) menu_functions[i] = XEN_UNDEFINED;
	}
      else 
	{
	  menu_functions = (XEN *)REALLOC(menu_functions, callbacks_size * sizeof(XEN));
	  for (i = callbacks_size - CALLBACK_INCR; i < callbacks_size; i++) menu_functions[i] = XEN_UNDEFINED;
	}
    }
  old_callb = callb;
  callb++;
  return(old_callb);
}

static void add_callback(int slot, XEN callback)
{
  if ((XEN_BOUND_P(menu_functions[slot])) && 
      (XEN_PROCEDURE_P(menu_functions[slot])))
    snd_unprotect(menu_functions[slot]);
  menu_functions[slot] = callback;
  snd_protect(callback);
}

static XEN gl_add_to_main_menu(XEN label, XEN callback)
{
  #define H_add_to_main_menu "(" S_add_to_main_menu " label &optional callback) adds label to the main (top-level) menu, returning its index"
  int val = -1, slot = -1;
  char *err;
  XEN errm;
  XEN_ASSERT_TYPE(XEN_STRING_P(label), label, XEN_ARG_1, S_add_to_main_menu, "a string");
  slot = make_callback_slot();
  if (XEN_BOUND_P(callback))
    {
      err = procedure_ok(callback, 0, S_add_to_main_menu, "menu callback", 2);
      if (err == NULL)
	add_callback(slot, callback);
      else 
	{
	  errm = C_TO_XEN_STRING(err);
	  FREE(err);
	  return(snd_bad_arity_error(S_add_to_main_menu, errm, callback));
	}
    }
  val = g_add_to_main_menu(get_global_state(), 
			   XEN_TO_C_STRING(label), 
			   slot);
  return(C_TO_XEN_INT(val));
}

static XEN gl_add_to_menu(XEN menu, XEN label, XEN callback, XEN gpos)
{
  #define H_add_to_menu "(" S_add_to_menu " menu label func position) adds label to menu invoking func when activated \
menu is the index returned by add-to-main-menu, func should be a function of no arguments"

  int err = 0, slot = -1, m, position = -1;
  char *errmsg = NULL;
  XEN errm;
  XEN_ASSERT_TYPE(XEN_STRING_P(label) || XEN_FALSE_P(label), label, XEN_ARG_2, S_add_to_menu, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, XEN_ARG_1, S_add_to_menu, "an integer");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(callback) || XEN_FALSE_P(callback), callback, XEN_ARG_3, S_add_to_menu, "a procedure");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(gpos), gpos, XEN_ARG_4, S_add_to_menu, "an integer");
  if (XEN_PROCEDURE_P(callback))
    errmsg = procedure_ok(callback, 0, S_add_to_menu, "menu callback", 3);
  if (errmsg == NULL)
    {
      m = XEN_TO_C_INT(menu);
      if (m < 0)
	return(snd_no_such_menu_error(S_add_to_menu, menu));
      if (XEN_PROCEDURE_P(callback)) slot = make_callback_slot();
      if (XEN_INTEGER_P(gpos)) position = XEN_TO_C_INT(gpos);
      err = g_add_to_menu(get_global_state(), 
			  m,
			  (XEN_FALSE_P(label)) ? NULL : XEN_TO_C_STRING(label),
			  slot,
			  position);
      if (err == -1) 
	return(snd_no_such_menu_error(S_add_to_menu, menu));
      if (XEN_PROCEDURE_P(callback)) add_callback(slot, callback);
    }
  else 
    {
      errm = C_TO_XEN_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(S_add_to_menu, errm, callback));
    }
  return(label);
}

void g_snd_callback(int callb)
{
  if ((callb >= 0) && (XEN_BOUND_P(menu_functions[callb])))
    XEN_CALL_0(menu_functions[callb], "menu callback func");
}

static XEN gl_remove_from_menu(XEN menu, XEN label)
{
  #define H_remove_from_menu "(" S_remove_from_menu " menu label) removes menu item label from menu"
  int val, m;
  XEN_ASSERT_TYPE(XEN_STRING_P(label), label, XEN_ARG_2, S_remove_from_menu, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, XEN_ARG_1, S_remove_from_menu, "an integer");
  m = XEN_TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error(S_remove_from_menu, menu));
  val = g_remove_from_menu(m,
			   XEN_TO_C_STRING(label));
  return(C_TO_XEN_INT(val));
}

static XEN gl_change_menu_label(XEN menu, XEN old_label, XEN new_label)
{
  #define H_change_menu_label "(" S_change_menu_label " menu old-label new-label) changes menu's label"
  int val, m;
  XEN_ASSERT_TYPE(XEN_STRING_P(old_label), old_label, XEN_ARG_2, S_change_menu_label, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(new_label), new_label, XEN_ARG_3, S_change_menu_label, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, XEN_ARG_1, S_change_menu_label, "an integer");
  m = XEN_TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error(S_change_menu_label,	menu));
  val = g_change_menu_label(m,
			    XEN_TO_C_STRING(old_label), 
			    XEN_TO_C_STRING(new_label));
  return(C_TO_XEN_INT(val));
}

static XEN gl_menu_sensitive(XEN menu, XEN label)
{
  #define H_menu_sensitive "(" S_menu_sensitive " menu label) reflects whether item label in menu is sensitive"
  int val, m;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, XEN_ARG_1, "set-" S_menu_sensitive, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(label), label, XEN_ARG_2, "set-" S_menu_sensitive, "a string");
  m = XEN_TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error(S_menu_sensitive, menu));
  val = g_menu_is_sensitive(m,
			    XEN_TO_C_STRING(label));
  return(C_TO_XEN_BOOLEAN(val));
}

static XEN gl_set_menu_sensitive(XEN menu, XEN label, XEN on)
{
  int val, m;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(menu), menu, XEN_ARG_1, "set-" S_menu_sensitive, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(label), label, XEN_ARG_2, "set-" S_menu_sensitive, "a string");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_3, "set-" S_menu_sensitive, "a boolean");
  m = XEN_TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error("set-" S_menu_sensitive, menu));
  val = g_set_menu_sensitive(m,
			     XEN_TO_C_STRING(label), 
			     XEN_TO_C_BOOLEAN_OR_TRUE(on));
  return(C_TO_XEN_BOOLEAN(val));
}

static XEN g_main_menu(XEN which)
{
  #define H_main_menu "(" S_main_menu " menu) returns the top-level menu widget referred to by menu"
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
XEN_NARGIFY_2(gl_menu_sensitive_w, gl_menu_sensitive)
XEN_NARGIFY_3(gl_set_menu_sensitive_w, gl_set_menu_sensitive)
XEN_ARGIFY_2(gl_add_to_main_menu_w, gl_add_to_main_menu)
XEN_ARGIFY_4(gl_add_to_menu_w, gl_add_to_menu)
XEN_NARGIFY_2(gl_remove_from_menu_w, gl_remove_from_menu)
XEN_NARGIFY_3(gl_change_menu_label_w, gl_change_menu_label)
XEN_NARGIFY_1(g_main_menu_w, g_main_menu)
#else
#define g_save_state_file_w g_save_state_file
#define g_set_save_state_file_w g_set_save_state_file
#define gl_menu_sensitive_w gl_menu_sensitive
#define gl_set_menu_sensitive_w gl_set_menu_sensitive
#define gl_add_to_main_menu_w gl_add_to_main_menu
#define gl_add_to_menu_w gl_add_to_menu
#define gl_remove_from_menu_w gl_remove_from_menu
#define gl_change_menu_label_w gl_change_menu_label
#define g_main_menu_w g_main_menu
#endif

void g_init_menu(void)
{
  #define H_output_name_hook S_output_name_hook " () is called from the File:New dialog"
  XEN_DEFINE_HOOK(output_name_hook, S_output_name_hook, 0, H_output_name_hook);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_state_file, g_save_state_file_w, H_save_state_file,
				   "set-" S_save_state_file, g_set_save_state_file_w, 0, 0, 1, 0);
  
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_menu_sensitive, gl_menu_sensitive_w, H_menu_sensitive,
				   "set-" S_menu_sensitive, gl_set_menu_sensitive_w, 2, 0, 3, 0);

  XEN_DEFINE_PROCEDURE(S_add_to_main_menu,  gl_add_to_main_menu_w, 1, 1, 0,  H_add_to_main_menu);
  XEN_DEFINE_PROCEDURE(S_add_to_menu,       gl_add_to_menu_w, 3, 1, 0,       H_add_to_menu);
  XEN_DEFINE_PROCEDURE(S_remove_from_menu,  gl_remove_from_menu_w, 2, 0, 0,  H_remove_from_menu);
  XEN_DEFINE_PROCEDURE(S_change_menu_label, gl_change_menu_label_w, 3, 0, 0, H_change_menu_label);

  XEN_DEFINE_PROCEDURE(S_main_menu,         g_main_menu_w, 1, 0, 0,          H_main_menu);
}
