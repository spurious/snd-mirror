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

void reflect_mix_active_in_menu(void)
{
  set_sensitive(view_mix_panel_menu(), TRUE);
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

void set_show_usage_stats(snd_state *ss, int val)
{
  in_set_show_usage_stats(ss, val);
  if (options_stats_menu())
    set_menu_label(options_stats_menu(), (val) ? STR_Ignore_stats : STR_Show_stats);
  check_stats_window(ss, val);
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

static int file_update(snd_info *sp, void *ptr)
{
  snd_state *ss = (snd_state *)ptr;
  /* here we should only update files that have changed on disk */
  if ((sp) && (sp->edited_region == NULL) &&
      ((sp->need_update) || 
       (file_write_date(sp->filename) != sp->write_date)))
    snd_update(ss, sp);
  return(0);
}

void update_file_from_menu(snd_state *ss)
{
  map_over_sounds(ss, file_update, (void *)ss);
}

static char *output_name(void);
static int new_ctr = 0;

void new_file_from_menu(snd_state *ss)
{
  char *new_file_name = NULL, *extension = NULL, *new_comment = NULL;
  int header_type, data_format, chans, srate;
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
  make_new_file_dialog(ss, new_file_name, header_type, data_format, srate, chans, new_comment);
  if (new_comment) FREE(new_comment);
  if (new_file_name) FREE(new_file_name);
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

static int map_chans_graph_style(chan_info *cp, void *ptr) {cp->graph_style = (*((int *)ptr)); update_graph(cp, NULL); return(0);}

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
  map_over_chans(ss, map_chans_graph_style, (void *)(&val));
  switch (val)
    {
    case GRAPH_LINES:          set_sensitive(view_lines_menu(), FALSE);          break;
    case GRAPH_DOTS:           set_sensitive(view_dots_menu(), FALSE);           break;
    case GRAPH_FILLED:         set_sensitive(view_filled_menu(), FALSE);         break;
    case GRAPH_DOTS_AND_LINES: set_sensitive(view_dots_and_lines_menu(), FALSE); break;
    case GRAPH_LOLLIPOPS:      set_sensitive(view_lollipops_menu(), FALSE);      break;
    }
}

static int map_chans_marks(chan_info *cp, void *ptr)
{
  cp->show_marks = (*((int *)ptr));
  update_graph(cp, NULL);
  return(0);
}

void set_show_marks(snd_state *ss, int val)
{
  in_set_show_marks(ss, val);
  map_over_chans(ss, map_chans_marks, (void *)(&val));
}

static int map_chans_zero(chan_info *cp, void *ptr)
{
  cp->show_y_zero = (*((int *)ptr));
  update_graph(cp, NULL);
  return(0);
}

void set_show_y_zero(snd_state *ss, int val)
{
  in_set_show_y_zero(ss, val);
  if (view_zero_menu())
    {
      set_menu_label(view_zero_menu(), 
		     (val) ? STR_Hide_Y0 : STR_Show_Y0);
      map_over_chans(ss, map_chans_zero, (void *)(&val));
    }
}

static int clrmini(snd_info *sp, void *ignore) {clear_minibuffer(sp); return(0);}
static int map_chans_verbose_cursor(chan_info *cp, void *ptr) {cp->verbose_cursor = (*((int *)ptr)); return(0);}

void set_verbose_cursor(snd_state *ss, int val)
{
  in_set_verbose_cursor(ss, val);
  if (val == 0) map_over_sounds(ss, clrmini, NULL);
  map_over_chans(ss, map_chans_verbose_cursor, (void *)(&val));
  if (view_cursor_menu())
    set_menu_label(view_cursor_menu(), 
		   (val) ? STR_Silent_cursor : STR_Verbose_cursor);
}

void set_view_ctrls_label(char *lab)
{
  set_menu_label(view_ctrls_menu(), lab);
}

void set_view_listener_label(char *lab)
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

int map_chans_x_axis_style(chan_info *cp, void *ptr)
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
	case X_AXIS_IN_SAMPLES:    ap->xlabel = copy_string(STR_time_samples); break;
	case X_AXIS_AS_PERCENTAGE: ap->xlabel = copy_string(STR_time_percent); break;
	default:                   ap->xlabel = copy_string(STR_time);         break;
	}
      update_graph(cp, NULL);
    }
  return(0);
} 

static void reflect_x_axis_unit_change_in_menu(int oldval, int newval)
{
  switch (oldval)
    {
    case X_AXIS_IN_SECONDS:    set_sensitive(view_x_axis_seconds_menu(), TRUE);    break;
    case X_AXIS_IN_SAMPLES:    set_sensitive(view_x_axis_samples_menu(), TRUE);    break;
    case X_AXIS_AS_PERCENTAGE: set_sensitive(view_x_axis_percentage_menu(), TRUE); break;
    default:                   set_sensitive(view_x_axis_seconds_menu(), TRUE);    break;
    }
  switch (newval)
    {
    case X_AXIS_IN_SECONDS:    set_sensitive(view_x_axis_seconds_menu(), FALSE);    break;
    case X_AXIS_IN_SAMPLES:    set_sensitive(view_x_axis_samples_menu(), FALSE);    break;
    case X_AXIS_AS_PERCENTAGE: set_sensitive(view_x_axis_percentage_menu(), FALSE); break;
    default:                   set_sensitive(view_x_axis_seconds_menu(), FALSE);    break;
    }
}
  
void set_x_axis_style(snd_state *ss, int val)
{
  reflect_x_axis_unit_change_in_menu(x_axis_style(ss), val);
  in_set_x_axis_style(ss, val);
  map_over_chans(ss, map_chans_x_axis_style, (void *)(&val));
}

static int update_sound(snd_info *sp, void *ptr)
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
  return(0);
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
  map_over_sounds(ss, update_sound, (void *)ss);
  map_over_chans(ss, update_graph, NULL);
}

static SCM snd_no_such_menu_error(const char *caller, SCM id)
{
  ERROR(NO_SUCH_MENU,
	SCM_LIST2(TO_SCM_STRING(caller),
		  id));
  return(SCM_BOOL_F);
}

static SCM output_name_hook;

static char *output_name(void)
{
  if (HOOKED(output_name_hook))
    {
      SCM result;
      SCM procs = SCM_HOOK_PROCEDURES (output_name_hook);
      while (NOT_NULL_P(procs))
	{
	  result = CALL0(SCM_CAR(procs), S_output_name_hook);
	  if (STRING_P(result)) return(TO_NEW_C_STRING(result));
	  procs = SCM_CDR (procs);
	}
    }
  return(NULL);
}

static SCM g_save_state_file(void) 
{
  snd_state *ss;
  ss = get_global_state();
  return(TO_SCM_STRING(save_state_file(ss)));
}

static void set_save_state_file(snd_state *ss, char *name)
{
  if (save_state_file(ss)) free(save_state_file(ss));
  in_set_save_state_file(ss, snd_strdup(name));
  set_sensitive(options_save_state_menu(), (snd_strlen(name) > 0));
}

static SCM g_set_save_state_file(SCM val) 
{
  #define H_save_state_file "(" S_save_state_file ") -> name of saved state file (\"saved-snd.scm\")"
  snd_state *ss;
  ASSERT_TYPE(STRING_P(val), val, SCM_ARGn, "set-" S_save_state_file, "a string"); 
  ss = get_global_state();
  set_save_state_file(ss, TO_C_STRING(val));
  return(TO_SCM_STRING(save_state_file(ss)));
}

static SCM *menu_functions = NULL;
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
	  menu_functions = (SCM *)CALLOC(callbacks_size, sizeof(SCM));
	  for (i = 0; i < callbacks_size; i++) menu_functions[i] = SCM_UNDEFINED;
	}
      else 
	{
	  menu_functions = (SCM *)REALLOC(menu_functions, callbacks_size * sizeof(SCM));
	  for (i = callbacks_size - CALLBACK_INCR; i < callbacks_size; i++) menu_functions[i] = SCM_UNDEFINED;
	}
    }
  old_callb = callb;
  callb++;
  return(old_callb);
}

static void add_callback(int slot, SCM callback)
{
  if ((BOUND_P(menu_functions[slot])) && 
      (PROCEDURE_P(menu_functions[slot])))
    snd_unprotect(menu_functions[slot]);
  menu_functions[slot] = callback;
  snd_protect(callback);
}

static SCM gl_add_to_main_menu(SCM label, SCM callback)
{
  #define H_add_to_main_menu "(" S_add_to_main_menu " label &optional callback) adds label to the main (top-level) menu, returning its index"
  int val = -1, slot = -1;
  char *err;
  SCM errm;
  ASSERT_TYPE(STRING_P(label), label, SCM_ARG1, S_add_to_main_menu, "a string");
  slot = make_callback_slot();
  if (BOUND_P(callback))
    {
      err = procedure_ok(callback, 0, S_add_to_main_menu, "menu callback", 2);
      if (err == NULL)
	add_callback(slot, callback);
      else 
	{
	  errm = TO_SCM_STRING(err);
	  FREE(err);
	  return(snd_bad_arity_error(S_add_to_main_menu, errm, callback));
	}
    }
  val = g_add_to_main_menu(get_global_state(), 
			   TO_C_STRING(label), 
			   slot);
  return(TO_SCM_INT(val));
}

static SCM gl_add_to_menu(SCM menu, SCM label, SCM callback)
{
  #define H_add_to_menu "(" S_add_to_menu " menu label func) adds label to menu invoking func when activated \
menu is the index returned by add-to-main-menu, func should be a function of no arguments"

  int err = 0, slot, m;
  char *errmsg;
  SCM errm;
  ASSERT_TYPE(STRING_P(label), label, SCM_ARG2, S_add_to_menu, "a string");
  ASSERT_TYPE(INTEGER_P(menu), menu, SCM_ARG1, S_add_to_menu, "an integer");
  ASSERT_TYPE(PROCEDURE_P(callback), callback, SCM_ARG3, S_add_to_menu, "a procedure");
  errmsg = procedure_ok(callback, 0, S_add_to_menu, "menu callback", 3);
  if (errmsg == NULL)
    {
      m = TO_C_INT(menu);
      if (m < 0)
	return(snd_no_such_menu_error(S_add_to_menu, menu));
      slot = make_callback_slot();
      err = g_add_to_menu(get_global_state(), 
			  m,
			  TO_C_STRING(label),
			  slot);
      if (err == -1) 
	return(snd_no_such_menu_error(S_add_to_menu, menu));
      add_callback(slot, callback);
    }
  else 
    {
      errm = TO_SCM_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(S_add_to_menu, errm, callback));
    }
  return(label);
}

void g_snd_callback(int callb)
{
  if ((callb >= 0) && (BOUND_P(menu_functions[callb])))
    CALL0(menu_functions[callb], "menu callback func");
}

static SCM gl_remove_from_menu(SCM menu, SCM label)
{
  #define H_remove_from_menu "(" S_remove_from_menu " menu label) removes menu item label from menu"
  int val, m;
  ASSERT_TYPE(STRING_P(label), label, SCM_ARG2, S_remove_from_menu, "a string");
  ASSERT_TYPE(INTEGER_P(menu), menu, SCM_ARG1, S_remove_from_menu, "an integer");
  m = TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error(S_remove_from_menu, menu));
  val = g_remove_from_menu(m,
			   TO_C_STRING(label));
  return(TO_SCM_INT(val));
}

static SCM gl_change_menu_label(SCM menu, SCM old_label, SCM new_label)
{
  #define H_change_menu_label "(" S_change_menu_label " menu old-label new-label) changes menu's label"
  int val, m;
  ASSERT_TYPE(STRING_P(old_label), old_label, SCM_ARG2, S_change_menu_label, "a string");
  ASSERT_TYPE(STRING_P(new_label), new_label, SCM_ARG3, S_change_menu_label, "a string");
  ASSERT_TYPE(INTEGER_P(menu), menu, SCM_ARG1, S_change_menu_label, "an integer");
  m = TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error(S_change_menu_label,	menu));
  val = g_change_menu_label(m,
			    TO_C_STRING(old_label), 
			    TO_C_STRING(new_label));
  return(TO_SCM_INT(val));
}

static SCM gl_menu_sensitive(SCM menu, SCM label)
{
  #define H_menu_sensitive "(" S_menu_sensitive " menu label) reflects whether item label in menu is sensitive"
  int val, m;
  ASSERT_TYPE(INTEGER_P(menu), menu, SCM_ARG1, "set-" S_menu_sensitive, "an integer");
  ASSERT_TYPE(STRING_P(label), label, SCM_ARG2, "set-" S_menu_sensitive, "a string");
  m = TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error(S_menu_sensitive, menu));
  val = g_menu_is_sensitive(m,
			    TO_C_STRING(label));
  return(TO_SCM_BOOLEAN(val));
}

static SCM gl_set_menu_sensitive(SCM menu, SCM label, SCM on)
{
  int val, m;
  ASSERT_TYPE(INTEGER_P(menu), menu, SCM_ARG1, "set-" S_menu_sensitive, "an integer");
  ASSERT_TYPE(STRING_P(label), label, SCM_ARG2, "set-" S_menu_sensitive, "a string");
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG3, "set-" S_menu_sensitive, "a boolean");
  m = TO_C_INT(menu);
  if (m < 0) 
    return(snd_no_such_menu_error("set-" S_menu_sensitive, menu));
  val = g_set_menu_sensitive(m,
			     TO_C_STRING(label), 
			     TO_C_BOOLEAN_OR_T(on));
  return(TO_SCM_BOOLEAN(val));
}

void g_init_menu(SCM local_doc)
{
  #define H_output_name_hook S_output_name_hook " () is called from the File:New dialog"
  output_name_hook = MAKE_HOOK(S_output_name_hook, 0, H_output_name_hook);

  define_procedure_with_setter(S_save_state_file, SCM_FNC g_save_state_file, H_save_state_file,
			       "set-" S_save_state_file, SCM_FNC g_set_save_state_file,
			       local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_menu_sensitive, SCM_FNC gl_menu_sensitive, H_menu_sensitive,
			       "set-" S_menu_sensitive, SCM_FNC gl_set_menu_sensitive,
			       local_doc, 2, 0, 3, 0);

  DEFINE_PROC(S_add_to_main_menu,  gl_add_to_main_menu, 1, 1, 0,  H_add_to_main_menu);
  DEFINE_PROC(S_add_to_menu,       gl_add_to_menu, 3, 0, 0,       H_add_to_menu);
  DEFINE_PROC(S_remove_from_menu,  gl_remove_from_menu, 2, 0, 0,  H_remove_from_menu);
  DEFINE_PROC(S_change_menu_label, gl_change_menu_label, 3, 0, 0, H_change_menu_label);
}
