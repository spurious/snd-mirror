#include "snd.h"

/* TODO  if all popup choices are insensitive what should button-2 do?
 * TODO  user-add to popup menu
 */

void reflect_file_open_in_menu (void)
{
  set_sensitive(file_close_menu(), TRUE);
  set_sensitive(file_print_menu(), TRUE);
  set_sensitive(file_mix_menu(), TRUE);
  set_sensitive(file_save_as_menu(), TRUE);
  set_sensitive(file_update_menu(), TRUE);  
  set_sensitive(view_normalize_menu(), TRUE);  
  set_sensitive(edit_header_menu(), TRUE);
  set_sensitive(edit_find_menu(), TRUE);
  set_sensitive(edit_select_all_menu(), TRUE);
  if (popup_menu_exists())
    {
      set_sensitive(popup_normalize_menu(), TRUE);
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
  set_sensitive(view_normalize_menu(), FALSE);
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
      set_sensitive(popup_normalize_menu(), FALSE);
    }
}

void reflect_mix_active_in_menu(void)
{
  set_sensitive(view_mix_panel_menu(), TRUE);
}

void reflect_normalize_in_menu(int on)
{
  set_sensitive(view_normalize_menu(), on);
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
	  make_a_big_star_outa_me(sp->shortname, 0);
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
  if ((!(selection_is_active())) && (!(region_ok(0)))) set_sensitive(edit_paste_menu(), FALSE);
  set_sensitive(edit_play_menu(), FALSE);
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
      redoable = (!(((cp->edit_ctr+1) == cp->edit_size) || 
		    (!(cp->edits[cp->edit_ctr+1]))));
      set_sensitive(edit_undo_menu(), undoable);
      if (popup_menu_exists()) set_sensitive(popup_undo_menu(), undoable);
      set_sensitive(edit_redo_menu(), redoable);
      if (popup_menu_exists()) set_sensitive(popup_redo_menu(), redoable);
    }
}

void reflect_regions_in_menu(void)
{
  set_sensitive(view_region_menu(), TRUE);
  set_sensitive(edit_save_as_menu(), TRUE);
}

void reflect_no_regions_in_menu(void)
{
  set_sensitive(view_region_menu(), FALSE);
  set_sensitive(edit_save_as_menu(), FALSE);
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
       (file_write_date(sp->fullname) != sp->write_date)))
    snd_update(ss, sp);
  return(0);
}

void update_file_from_menu(snd_state *ss)
{
  map_over_sounds(ss, file_update, (void *)ss);
}

#if HAVE_GUILE
  static char *output_name(void);
#endif

static int new_ctr = 0;

void new_file_from_menu(snd_state *ss)
{
  char *new_file_name = NULL, *extension = NULL, *new_comment = NULL;
  int header_type, data_format, chans, srate;
#if HAVE_GUILE
  new_file_name = output_name();
#endif
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
      sprintf(new_file_name, "new-%d.%s", new_ctr++, extension);
    }
  chans = default_output_chans(ss);
  data_format = default_output_format(ss);
  srate = default_output_srate(ss);
  new_comment = output_comment(NULL);
  snd_new_file(ss, new_file_name, header_type, data_format, srate, chans, new_comment, WITH_DIALOG);
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
      for (i = 0; i < sp->nchans; i++) revert_edits(sp->chans[i], NULL);
      reflect_file_revert_in_label(sp);
      reflect_file_revert_in_menu(ss);
    }
}

#if HAVE_HOOKS
static SCM exit_hook;

int dont_exit(snd_state *ss)
{
  SCM res = SCM_BOOL_F;
  if (HOOKED(exit_hook))
    res = g_c_run_or_hook(exit_hook, 
			  SCM_LIST0);
  return(SCM_TRUE_P(res));
}
#endif
  
void exit_from_menu(snd_state *ss)
{
#if HAVE_HOOKS
  if (dont_exit(ss)) return;
#endif
  snd_exit_cleanly(ss);
  snd_exit(1);
}

void save_options_from_menu(snd_state *ss)
{
  if (save_options(ss) == 0)
    {
      if (any_selected_sound(ss))
	report_in_minibuffer(any_selected_sound(ss), "saved options in %s", ss->init_file);
    }
}

void save_state_from_menu(snd_state *ss)
{
  if (save_state_file(ss))
    {
      if (save_state(ss, save_state_file(ss)) == 0)
	{
	  if (any_selected_sound(ss))
	    report_in_minibuffer(any_selected_sound(ss), "saved state in %s", save_state_file(ss));
	}
    }
}

static int map_chans_graph_style(chan_info *cp, void *ptr) {cp->graph_style = (int)ptr; update_graph(cp, NULL); return(0);}

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
  map_over_chans(ss, map_chans_graph_style, (void *)val);
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
  cp->show_marks = (int)ptr;
  update_graph(cp, NULL);
  return(0);
}

void set_show_marks(snd_state *ss, int val)
{
  in_set_show_marks(ss, val);
  map_over_chans(ss, map_chans_marks, (void *)val);
}

static int map_chans_zero(chan_info *cp, void *ptr)
{
  cp->show_y_zero = (int)ptr;
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
      map_over_chans(ss, map_chans_zero, (void *)val);
    }
}

static int clrmini(snd_info *sp, void *ignore) {clear_minibuffer(sp); return(0);}
static int map_chans_verbose_cursor(chan_info *cp, void *ptr) {cp->verbose_cursor = (int)ptr; return(0);}

void set_verbose_cursor(snd_state *ss, int val)
{
  in_set_verbose_cursor(ss, val);
  if (val == 0) map_over_sounds(ss, clrmini, NULL);
  map_over_chans(ss, map_chans_verbose_cursor, (void *)val);
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
	case FOCUS_LEFT:   set_sensitive(options_focus_left_menu(), TRUE);   break;
	case FOCUS_RIGHT:  set_sensitive(options_focus_right_menu(), TRUE);  break;
	case FOCUS_MIDDLE: set_sensitive(options_focus_middle_menu(), TRUE); break;
	case FOCUS_ACTIVE: set_sensitive(options_focus_active_menu(), TRUE); break;
	}
    }
  set_zoom_focus_style(ss, new_focus);
  if (options_focus_left_menu())
    {
      switch (new_focus)
	{
	case FOCUS_LEFT:   set_sensitive(options_focus_left_menu(), FALSE);   break;
	case FOCUS_RIGHT:  set_sensitive(options_focus_right_menu(), FALSE);  break;
	case FOCUS_MIDDLE: set_sensitive(options_focus_middle_menu(), FALSE); break;
	case FOCUS_ACTIVE: set_sensitive(options_focus_active_menu(), FALSE); break;
	}
    }
}  

void activate_speed_in_menu(snd_state *ss, int newval)
{
  if (options_speed_ratio_menu())
    {
      switch (speed_style(ss))
	{
	case SPEED_AS_RATIO:    set_sensitive(options_speed_ratio_menu(), TRUE);    break;
	case SPEED_AS_SEMITONE: set_sensitive(options_speed_semitone_menu(), TRUE); break;
	default:                set_sensitive(options_speed_float_menu(), TRUE);    break;
	}
    }
  set_speed_style(ss, newval);
  if (options_speed_ratio_menu())
    {
      switch (speed_style(ss))
	{
	case SPEED_AS_RATIO:    set_sensitive(options_speed_ratio_menu(), FALSE);    break;
	case SPEED_AS_SEMITONE: set_sensitive(options_speed_semitone_menu(), FALSE); break;
	default:                set_sensitive(options_speed_float_menu(), FALSE);    break;
	}
    }
}

static void reflect_x_axis_unit_change_in_menu(int oldval, int newval)
{
  switch (oldval)
    {
    case X_IN_SECONDS: set_sensitive(view_x_axis_seconds_menu(), TRUE);    break;
    case X_IN_SAMPLES: set_sensitive(view_x_axis_samples_menu(), TRUE);    break;
    case X_TO_ONE:     set_sensitive(view_x_axis_percentage_menu(), TRUE); break;
    default:           set_sensitive(view_x_axis_seconds_menu(), TRUE);    break;
    }
  switch (newval)
    {
    case X_IN_SECONDS: set_sensitive(view_x_axis_seconds_menu(), FALSE);    break;
    case X_IN_SAMPLES: set_sensitive(view_x_axis_samples_menu(), FALSE);    break;
    case X_TO_ONE:     set_sensitive(view_x_axis_percentage_menu(), FALSE); break;
    default:           set_sensitive(view_x_axis_seconds_menu(), FALSE);    break;
    }
}
  
void set_x_axis_style(snd_state *ss, int val)
{
  reflect_x_axis_unit_change_in_menu(x_axis_style(ss), val);
  in_set_x_axis_style(ss, val);
  map_over_chans(ss, update_graph, NULL);
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

#if HAVE_GUILE

static SCM output_name_hook;

#if HAVE_HOOKS
static char *output_name(void)
{
  if (HOOKED(output_name_hook))
    {
      SCM result;
      SCM procs = SCM_HOOK_PROCEDURES (output_name_hook);
      while (SCM_NIMP (procs))
	{
	  result = g_call0(SCM_CAR(procs));
	  if (gh_string_p(result)) return(TO_NEW_C_STRING(result));
	  procs = SCM_CDR (procs);
	}
    }
  return(NULL);
}
#else
static char *output_name(void) {return(NULL);}
#endif

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
  SCM_ASSERT(gh_string_p(val), val, SCM_ARG1, "set-" S_save_state_file); 
  ss = get_global_state();
  set_save_state_file(ss, TO_NEW_C_STRING(val));
  return(TO_SCM_STRING(save_state_file(ss)));
}

static char **menu_strings = NULL; /* backwards compatibility */
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
	  menu_strings = (char **)CALLOC(callbacks_size, sizeof(char *));
	  menu_functions = (SCM *)CALLOC(callbacks_size, sizeof(SCM));
	}
      else 
	{
	  menu_strings = (char **)REALLOC(menu_strings, callbacks_size * sizeof(char *));
	  menu_functions = (SCM *)REALLOC(menu_functions, callbacks_size * sizeof(SCM));
	  for (i = callbacks_size - CALLBACK_INCR; i < callbacks_size; i++)
	    {
	      menu_strings[i] = NULL;
	      menu_functions[i] = 0;
	    }
	}
    }
  old_callb = callb;
  callb++;
  return(old_callb);
}

static void add_callback(int slot, SCM callstr)
{
  if (gh_string_p(callstr))
    menu_strings[slot] = TO_NEW_C_STRING(callstr);
  else 
    {
      if ((menu_functions[slot]) && 
	  (gh_procedure_p(menu_functions[slot]))) 
	snd_unprotect(menu_functions[slot]);
      menu_functions[slot] = callstr;
      snd_protect(callstr);
    }
}

static SCM g_add_to_main_menu(SCM label, SCM callback)
{
  #define H_add_to_main_menu "(" S_add_to_main_menu " label &optional callback) adds label to the main (top-level) menu, returning its index"
  int val, slot=-1;
  SCM_ASSERT(gh_string_p(label), label, SCM_ARG1, S_add_to_main_menu);
  if (gh_procedure_p(callback)) 
    {
      slot = make_callback_slot();
      add_callback(slot, callback);
    }
  val = gh_add_to_main_menu(get_global_state(), 
			    TO_C_STRING(label), 
			    slot);
  return(TO_SCM_INT(val));
}

static SCM g_add_to_menu(SCM menu, SCM label, SCM callstr)
{
  #define H_add_to_menu "(" S_add_to_menu " menu label func) adds label to menu invoking func when activated \
menu is the index returned by add-to-main-menu, func should be a function of no arguments"

  int err = 0, slot;
  SCM_ASSERT(gh_string_p(label), label, SCM_ARG2, S_add_to_menu);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(menu)), menu, SCM_ARG1, S_add_to_menu);
  slot = make_callback_slot();
  err = gh_add_to_menu(get_global_state(), 
		       TO_C_INT_OR_ELSE(menu, 0),
		       TO_C_STRING(label),
		       slot);
  if (err == -1) 
    return(scm_throw(NO_SUCH_MENU,
		     SCM_LIST2(TO_SCM_STRING(S_add_to_menu),
			       menu)));
  add_callback(slot, callstr);
  return(label);
}

void g_snd_callback(int callb)
{
  if ((callb >= 0) && (menu_functions[callb]))
    g_call0(menu_functions[callb]);
}

static SCM g_remove_from_menu(SCM menu, SCM label)
{
  #define H_remove_from_menu "(" S_remove_from_menu " menu label) removes menu item label from menu"
  int val;
  SCM_ASSERT(gh_string_p(label), label, SCM_ARG2, S_remove_from_menu);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(menu)), menu, SCM_ARG1, S_remove_from_menu);
  val = gh_remove_from_menu(TO_C_INT_OR_ELSE(menu, 0), 
			    TO_C_STRING(label));
  return(TO_SCM_INT(val));
}

static SCM g_change_menu_label(SCM menu, SCM old_label, SCM new_label)
{
  #define H_change_menu_label "(" S_change_menu_label " menu old-label new-label) changes menu's label"
  int val;
  SCM_ASSERT(gh_string_p(old_label), old_label, SCM_ARG2, S_change_menu_label);
  SCM_ASSERT(gh_string_p(new_label), new_label, SCM_ARG3, S_change_menu_label);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(menu)), menu, SCM_ARG1, S_change_menu_label);
  val = gh_change_menu_label(TO_C_INT_OR_ELSE(menu, 0), 
			     TO_C_STRING(old_label), 
			     TO_C_STRING(new_label));
  return(TO_SCM_INT(val));
}

static SCM g_menu_sensitive(SCM menu, SCM label)
{
  #define H_menu_sensitive "(" S_menu_sensitive " menu label) reflects whether item label in menu is sensitive"
  int val;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(menu)), menu, SCM_ARG1, "set-" S_menu_sensitive);
  SCM_ASSERT(gh_string_p(label), label, SCM_ARG2, "set-" S_menu_sensitive);
  val = gh_menu_is_sensitive(TO_C_INT_OR_ELSE(menu, 0), 
			     TO_C_STRING(label));
  return(TO_SCM_BOOLEAN(val));
}

static SCM g_set_menu_sensitive(SCM menu, SCM label, SCM on)
{
  int val;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(menu)), menu, SCM_ARG1, "set-" S_menu_sensitive);
  SCM_ASSERT(gh_string_p(label), label, SCM_ARG2, "set-" S_menu_sensitive);
  SCM_ASSERT(bool_or_arg_p(on), on, SCM_ARG3, "set-" S_menu_sensitive);
  val = gh_set_menu_sensitive(TO_C_INT_OR_ELSE(menu, 0), 
			      TO_C_STRING(label), 
			      bool_int_or_one(on));
  return(TO_SCM_BOOLEAN(val));
}

void g_init_menu(SCM local_doc)
{
  #define H_output_name_hook S_output_name_hook " () is called from the File:New dialog"
  output_name_hook = MAKE_HOOK(S_output_name_hook, 0, H_output_name_hook);

  define_procedure_with_setter(S_save_state_file, SCM_FNC g_save_state_file, H_save_state_file,
			       "set-" S_save_state_file, SCM_FNC g_set_save_state_file,
			       local_doc, 0, 0, 1, 0);

  define_procedure_with_setter(S_menu_sensitive, SCM_FNC g_menu_sensitive, H_menu_sensitive,
			       "set-" S_menu_sensitive, SCM_FNC g_set_menu_sensitive,
			       local_doc, 2, 0, 3, 0);

  DEFINE_PROC(gh_new_procedure1_1(S_add_to_main_menu,  g_add_to_main_menu),  H_add_to_main_menu);
  DEFINE_PROC(gh_new_procedure3_0(S_add_to_menu,       g_add_to_menu),       H_add_to_menu);
  DEFINE_PROC(gh_new_procedure2_0(S_remove_from_menu,  g_remove_from_menu),  H_remove_from_menu);
  DEFINE_PROC(gh_new_procedure3_0(S_change_menu_label, g_change_menu_label), H_change_menu_label);

#if HAVE_HOOKS

  #define H_exit_hook S_exit_hook " () is called upon exit. \
If it returns #t, Snd does not exit.  This can be used to check for unsaved edits, or to perform cleanup activities."

  exit_hook =           MAKE_HOOK(S_exit_hook, 0, H_exit_hook);

#endif
}
#endif
