#include "snd.h"

void reflect_file_open_in_menu (void)
{
  set_sensitive(file_close_menu(),TRUE);
  set_sensitive(file_print_menu(),TRUE);
  set_sensitive(file_mix_menu(),TRUE);
  set_sensitive(file_save_as_menu(),TRUE);
  set_sensitive(file_update_menu(),TRUE);  
  set_sensitive(view_normalize_menu(),TRUE);  
  set_sensitive(edit_header_menu(),TRUE);
  set_sensitive(edit_find_menu(),TRUE);
  set_sensitive(edit_select_all_menu(),TRUE);
  if (popup_menu_exists())
    {
      set_sensitive(popup_normalize_menu(),TRUE);
      set_sensitive(popup_play_menu(),TRUE);
      set_sensitive(popup_info_menu(),TRUE);
    }
}

void reflect_file_change_in_menu (void)
{
  set_sensitive(file_save_menu(),TRUE);
  set_sensitive(file_revert_menu(),TRUE);
  set_sensitive(edit_undo_menu(),TRUE);
  if (popup_menu_exists())
    {
      set_sensitive(popup_undo_menu(),TRUE);
      set_sensitive(popup_save_menu(),TRUE);
      set_sensitive(popup_play_menu(),TRUE);
    }
}

void reflect_file_lack_in_menu (void)
{
  set_sensitive(file_close_menu(),FALSE);
  set_sensitive(file_save_as_menu(),FALSE);
  set_sensitive(file_save_menu(),FALSE);
  set_sensitive(file_revert_menu(),FALSE);
  set_sensitive(file_print_menu(),FALSE);
  set_sensitive(file_mix_menu(),FALSE);
  set_sensitive(file_update_menu(),FALSE);
  set_sensitive(edit_undo_menu(),FALSE);
  set_sensitive(edit_redo_menu(),FALSE);
  set_sensitive(view_normalize_menu(),FALSE);
  set_sensitive(edit_header_menu(),FALSE);
  set_sensitive(edit_find_menu(),FALSE);
  set_sensitive(edit_select_all_menu(),FALSE);
  if (popup_menu_exists())
    {
      set_sensitive(popup_undo_menu(),FALSE);
      set_sensitive(popup_redo_menu(),FALSE);
      set_sensitive(popup_save_menu(),FALSE);
      set_sensitive(popup_play_menu(),FALSE);
      set_sensitive(popup_info_menu(),FALSE);
      set_sensitive(popup_normalize_menu(),FALSE);
    }
}

void reflect_mix_active_in_menu(void)
{
  set_sensitive(view_consoles_menu(),TRUE);
}

void reflect_normalize_in_menu(int on)
{
  set_sensitive(view_normalize_menu(),on);
}

static int find_any_edits (chan_info *cp, void *ptr)
{
  return(cp->edit_ctr);
}

static int find_any_possible_edits (chan_info *cp, void *ptr)
{
  return(cp->edit_size);
}

void reflect_file_revert_in_menu (snd_state *ss)
{
  int editing;
  editing = map_over_chans(ss,find_any_edits,NULL);
  if (!editing)
    {
      set_sensitive(file_save_menu(),FALSE);
      set_sensitive(file_revert_menu(),FALSE);
      set_sensitive(edit_undo_menu(),FALSE);
      if (popup_menu_exists())
	{
	  set_sensitive(popup_undo_menu(),FALSE);
	  set_sensitive(popup_save_menu(),FALSE);
	}
    }
  set_sensitive(edit_redo_menu(),TRUE);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(),TRUE);
}

void reflect_file_save_in_menu (snd_state *ss)
{
  int editing;
  editing = map_over_chans(ss,find_any_edits,NULL);
  if (!editing)
    {
      set_sensitive(file_save_menu(),FALSE);
      set_sensitive(file_revert_menu(),FALSE);
      set_sensitive(edit_undo_menu(),FALSE);
      if (popup_menu_exists())
	{
	  set_sensitive(popup_undo_menu(),FALSE);
	  set_sensitive(popup_save_menu(),FALSE);
	  set_sensitive(popup_redo_menu(),FALSE);
	}
      set_sensitive(edit_redo_menu(),FALSE);
    }
  editing = map_over_chans(ss,find_any_possible_edits,NULL);
}

void reflect_file_revert_in_label (snd_info *sp)
{
  int editing;
  if (sp->sgx)
    {
      editing = map_over_sound_chans(sp,find_any_edits,NULL);
      if (!editing)
	{
	  set_sound_pane_file_label(sp,shortname(sp));
	  make_a_big_star_outa_me(sp->state,sp->shortname,0);
	}
    }
}

void reflect_no_more_redo_in_menu(void)
{
  set_sensitive(edit_redo_menu(),FALSE);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(),FALSE);
}

void reflect_edit_with_selection_in_menu (void)
{
  set_sensitive(edit_cut_menu(),TRUE);
  set_sensitive(edit_paste_menu(),TRUE);
  set_sensitive(edit_play_menu(),TRUE);
  set_sensitive(edit_mix_menu(),TRUE);
  set_sensitive(edit_save_as_menu(),TRUE);
  enved_reflect_selection(TRUE);
}

void reflect_edit_without_selection_in_menu (void)
{
  set_sensitive(edit_cut_menu(),FALSE);
  if (!(region_ok(0))) set_sensitive(edit_paste_menu(),FALSE);
  set_sensitive(edit_play_menu(),FALSE);
  enved_reflect_selection(FALSE);
}

void reflect_undo_in_menu(void)
{
  set_sensitive(edit_redo_menu(),TRUE);
  if (popup_menu_exists()) set_sensitive(popup_redo_menu(),TRUE);
}

void reflect_redo_in_menu(void)
{
  set_sensitive(edit_undo_menu(),TRUE);
  if (popup_menu_exists()) set_sensitive(popup_undo_menu(),TRUE);
  reflect_file_change_in_menu();
}

void reflect_undo_ok_in_menu(void)
{
  set_sensitive(edit_undo_menu(),TRUE);
  if (popup_menu_exists()) set_sensitive(popup_undo_menu(),TRUE);
}

void reflect_undo_or_redo_in_menu(chan_info *cp)
{
  int undoable,redoable;
  if ((cp) && (cp->cgx))
    {
      undoable = (cp->edit_ctr > 0);
      redoable = (!(((cp->edit_ctr+1) == cp->edit_size) || (!(cp->edits[cp->edit_ctr+1]))));
      set_sensitive(edit_undo_menu(),undoable);
      if (popup_menu_exists()) set_sensitive(popup_undo_menu(),undoable);
      set_sensitive(edit_redo_menu(),redoable);
      if (popup_menu_exists()) set_sensitive(popup_redo_menu(),redoable);
    }
}

void reflect_regions_in_menu(void)
{
  set_sensitive(view_region_menu(),TRUE);
  set_sensitive(edit_save_as_menu(),TRUE);
}

void reflect_no_regions_in_menu(void)
{
  set_sensitive(view_region_menu(),FALSE);
  set_sensitive(edit_save_as_menu(),FALSE);
}

void reflect_raw_open_in_menu(void)
{
  set_sensitive(file_open_menu(),TRUE);
  set_sensitive(file_view_menu(),TRUE);
  set_sensitive(file_new_menu(),TRUE);
}

void reflect_raw_pending_in_menu(void)
{
  set_sensitive(file_open_menu(),FALSE);
  set_sensitive(file_view_menu(),FALSE);
  set_sensitive(file_new_menu(),FALSE);
}


static void set_save_state_file(snd_state *ss, char *name)
{
  in_set_save_state_file(ss,name);
  set_sensitive(options_save_state_menu(),(snd_strlen(name) > 0));
}

void set_show_usage_stats(snd_state *ss, int val)
{
  in_set_show_usage_stats(ss,val);
  if (options_stats_menu())
    set_menu_label(options_stats_menu(),(val) ? STR_Ignore_stats : STR_Show_stats);
  check_stats_window(ss,val);
}

void close_file_from_menu(snd_state *ss)
{
  snd_info *sp;
  finish_keyboard_selection();
  sp = any_selected_sound(ss);
  if (sp) snd_close_file(sp,ss);
}

void save_file_from_menu(snd_state *ss)
{
  snd_info *sp;
  finish_keyboard_selection();
  sp = any_selected_sound(ss);
  save_edits(sp,NULL);
}

static int file_update(snd_info *sp, void *ptr)
{
  snd_state *ss = (snd_state *)ptr;
  /* here we should only update files that have changed on disk */
  if ((sp) && (sp->edited_region == NULL) &&
      ((sp->need_update) || (file_write_date(sp->fullname) != sp->write_date)))
    snd_update(ss,sp);
  return(0);
}

void update_file_from_menu(snd_state *ss)
{
  finish_keyboard_selection();
  map_over_sounds(ss,file_update,(void *)ss);
}

#if HAVE_GUILE
  static char *output_name(void);
#endif

static int new_ctr = 0;

void new_file_from_menu(snd_state *ss)
{
  char *new_file_name = NULL,*extension = NULL, *new_comment = NULL;
  int header_type, data_format, chans, srate;
  finish_keyboard_selection();
#if HAVE_GUILE
  new_file_name = output_name();
#endif
  header_type = default_output_type(ss);
  if (new_file_name == NULL)
    {
      new_file_name = (char *)CALLOC(MUS_MAX_FILE_NAME,sizeof(char));
      switch (header_type)
	{
	case MUS_AIFC: case MUS_AIFF: extension = "aiff"; break;
	case MUS_RIFF: extension = "wav"; break;
	default: extension = "snd"; break;
	}
      sprintf(new_file_name,"new-%d.%s",new_ctr++,extension);
    }
  chans = default_output_chans(ss);
  data_format = default_output_format(ss);
  srate = default_output_srate(ss);
  new_comment = output_comment(NULL);
  snd_new_file(ss,new_file_name,header_type,data_format,srate,chans,new_comment);
  if (new_comment) FREE(new_comment);
  if (new_file_name) FREE(new_file_name);
}

void revert_file_from_menu(snd_state *ss)
{
  snd_info *sp;
  int i;
  finish_keyboard_selection();
  sp = any_selected_sound(ss);
  for (i=0;i<sp->nchans;i++) revert_edits(sp->chans[i],NULL);
  reflect_file_revert_in_label(sp);
  reflect_file_revert_in_menu(ss);
}

void exit_from_menu(snd_state *ss)
{
  finish_keyboard_selection();
  if (dont_exit(ss)) return;
  snd_exit_cleanly(ss);
  snd_exit(1);
}

void mix_selection_from_menu(snd_state *ss)
{
  chan_info *cp;
  finish_keyboard_selection();
  cp = selected_channel(ss);
  if (cp) add_region(0,cp,"Edit: mix");
}

void cut_selection_from_menu(void)
{
  finish_keyboard_selection();
  if (region_ok(0))
    {
      delete_selection("Edit: Cut",UPDATE_DISPLAY);
    }
}

void paste_selection_from_menu(snd_state *ss)
{
  chan_info *cp;
  finish_keyboard_selection();
  cp = selected_channel(ss);
  if (cp) paste_region(0,cp,"Edit: Paste");
}

void select_all_from_menu(snd_state *ss)
{
  chan_info *cp;
  cp = current_channel(ss);
  if (cp) define_region(cp,0,current_ed_samples(cp),TRUE);
  map_over_chans(ss,update_graph,NULL);
}

void save_options_from_menu(snd_state *ss)
{
  char *buf = NULL;
  if (save_options(ss) == 0)
    {
      if (any_selected_sound(ss))
	{
	  buf = (char *)CALLOC(128,sizeof(char));
	  sprintf(buf,"saved options in %s",ss->init_file);
	  report_in_minibuffer(any_selected_sound(ss),buf);
	  FREE(buf);
	}
    }
  else 
    snd_error(STR_cant_write_p,ss->init_file,strerror(errno));
}

void save_state_from_menu(snd_state *ss)
{
  char *buf = NULL;
  if (save_state_file(ss))
    {
      if (save_state(ss,save_state_file(ss)) == 0)
	{
	  if (any_selected_sound(ss))
	    {
	      buf = (char *)CALLOC(128,sizeof(char));
	      sprintf(buf,"saved state in %s",save_state_file(ss));
	      report_in_minibuffer(any_selected_sound(ss),buf);
	      FREE(buf);
	    }
	}
    }
}

void set_graph_style(snd_state *ss, int val)
{
  switch (graph_style(ss))
    {
    case GRAPH_LINES: set_sensitive(view_lines_menu(),TRUE); break;
    case GRAPH_DOTS: set_sensitive(view_dots_menu(),TRUE); break;
    case GRAPH_FILLED: set_sensitive(view_filled_menu(),TRUE); break;
    case GRAPH_DOTS_AND_LINES: set_sensitive(view_dots_and_lines_menu(),TRUE); break;
    case GRAPH_LOLLIPOPS: set_sensitive(view_lollipops_menu(),TRUE); break;
    }
  in_set_graph_style(ss,val);
  map_over_chans(ss,update_graph,NULL);
  switch (val)
    {
    case GRAPH_LINES: set_sensitive(view_lines_menu(),FALSE); break;
    case GRAPH_DOTS: set_sensitive(view_dots_menu(),FALSE); break;
    case GRAPH_FILLED: set_sensitive(view_filled_menu(),FALSE); break;
    case GRAPH_DOTS_AND_LINES: set_sensitive(view_dots_and_lines_menu(),FALSE); break;
    case GRAPH_LOLLIPOPS: set_sensitive(view_lollipops_menu(),FALSE); break;
    }
}

void set_show_marks(snd_state *ss, int val)
{
  in_set_show_marks(ss,val);
  if (view_marks_menu())
    {
      set_menu_label(view_marks_menu(),(val) ? STR_Hide_marks : STR_Show_marks);
      map_over_chans(ss,update_graph,NULL);
    }
}

void set_show_y_zero(snd_state *ss, int val)
{
  in_set_show_y_zero(ss,val);
  if (view_zero_menu())
    {
      set_menu_label(view_zero_menu(),(val) ? STR_Hide_Y0 : STR_Show_Y0);
      map_over_chans(ss,update_graph,NULL);
    }
}

static int update_filter_env(snd_info *sp, void *ptr)
{
  sp_display_env(sp);
  return(0);
}

void set_show_axes(snd_state *ss, int val)
{
  in_set_show_axes(ss,val);
  if (view_axes_menu())
    {
      set_menu_label(view_axes_menu(),(val) ? STR_Hide_axes : STR_Show_axes);
      map_over_chans(ss,update_graph,NULL);
      map_over_sounds(ss,update_filter_env,NULL);
    }
}
static int clrmini(snd_info *sp, void *ptr) {clear_minibuffer(sp); return(0);}


void set_verbose_cursor(snd_state *ss, int val)
{
  in_set_verbose_cursor(ss,val);
  if (val == 0) map_over_sounds(ss,clrmini,NULL);
  if (view_cursor_menu())
    set_menu_label(view_cursor_menu(),(val) ? STR_Silent_cursor : STR_Verbose_cursor);
}

void set_view_ctrls_label(char *lab)
{
  set_menu_label(view_ctrls_menu(),lab);
}

void set_view_listener_label(char *lab)
{
  set_menu_label(view_listener_menu(),lab);
}

void set_show_mix_consoles(snd_state *ss, int on)
{
  in_set_show_mix_consoles(ss,on);
  set_menu_label(view_consoles_menu(),(on) ? STR_Hide_consoles : STR_Show_consoles);
  update_all_consoles(ss);
}

void activate_focus_menu(snd_state *ss, int new_focus)
{
  if (options_focus_left_menu())
    {
      switch (zoom_focus_style(ss))
	{
	case FOCUS_LEFT: set_sensitive(options_focus_left_menu(),TRUE); break;
	case FOCUS_RIGHT: set_sensitive(options_focus_right_menu(),TRUE); break;
	case FOCUS_MIDDLE: set_sensitive(options_focus_middle_menu(),TRUE); break;
	case FOCUS_ACTIVE: set_sensitive(options_focus_active_menu(),TRUE); break;
	}
    }
  set_zoom_focus_style(ss,new_focus);
  if (options_focus_left_menu())
    {
      switch (new_focus)
	{
	case FOCUS_LEFT: set_sensitive(options_focus_left_menu(),FALSE); break;
	case FOCUS_RIGHT: set_sensitive(options_focus_right_menu(),FALSE); break;
	case FOCUS_MIDDLE: set_sensitive(options_focus_middle_menu(),FALSE); break;
	case FOCUS_ACTIVE: set_sensitive(options_focus_active_menu(),FALSE); break;
	}
    }
}  

void activate_speed_in_menu(snd_state *ss, int newval)
{
  if (options_speed_ratio_menu())
    {
      switch (speed_style(ss))
	{
	case SPEED_AS_RATIO: set_sensitive(options_speed_ratio_menu(),TRUE); break;
	case SPEED_AS_SEMITONE: set_sensitive(options_speed_semitone_menu(),TRUE); break;
	default: set_sensitive(options_speed_float_menu(),TRUE); break;
	}
    }
  in_set_speed_style(ss,newval);
  if (options_speed_ratio_menu())
    {
      switch (speed_style(ss))
	{
	case SPEED_AS_RATIO: set_sensitive(options_speed_ratio_menu(),FALSE); break;
	case SPEED_AS_SEMITONE: set_sensitive(options_speed_semitone_menu(),FALSE); break;
	default: set_sensitive(options_speed_float_menu(),FALSE); break;
	}
    }
}

static void reflect_x_axis_unit_change_in_menu(int oldval, int newval)
{
  switch (oldval)
    {
    case X_IN_SECONDS: set_sensitive(view_x_axis_seconds_menu(),TRUE); break;
    case X_IN_SAMPLES: set_sensitive(view_x_axis_samples_menu(),TRUE); break;
    case X_TO_ONE: set_sensitive(view_x_axis_percentage_menu(),TRUE); break;
    default: set_sensitive(view_x_axis_seconds_menu(),TRUE); break;
    }
  switch (newval)
    {
    case X_IN_SECONDS: set_sensitive(view_x_axis_seconds_menu(),FALSE); break;
    case X_IN_SAMPLES: set_sensitive(view_x_axis_samples_menu(),FALSE); break;
    case X_TO_ONE: set_sensitive(view_x_axis_percentage_menu(),FALSE); break;
    default: set_sensitive(view_x_axis_seconds_menu(),FALSE); break;
    }
}
  
void set_x_axis_style(snd_state *ss, int val)
{
  reflect_x_axis_unit_change_in_menu(x_axis_style(ss),val);
  in_set_x_axis_style(ss,val);
  map_over_chans(ss,update_graph,NULL);
}

static int update_sound(snd_info *sp, void *ptr)
{
  snd_state *ss = (snd_state *)ptr;
  if (sp)
    {
      switch (channel_style(ss))
	{
	case CHANNELS_SEPARATE: separate_sound(sp); break;
	case CHANNELS_COMBINED: combine_sound(sp); break;
	case CHANNELS_SUPERIMPOSED: superimpose_sound(sp); break;
	}
    }
  return(0);
}

void set_channel_style(snd_state *ss, int val)
{
  switch (channel_style(ss))
    {
    case CHANNELS_SEPARATE: set_sensitive(view_combine_separate_menu(),TRUE); break;
    case CHANNELS_COMBINED: set_sensitive(view_combine_combined_menu(),TRUE); break;
    case CHANNELS_SUPERIMPOSED: set_sensitive(view_combine_superimposed_menu(),TRUE); break;
    }
  in_set_channel_style(ss,val);
  switch (val)
    {
    case CHANNELS_SEPARATE: set_sensitive(view_combine_separate_menu(),FALSE); break;
    case CHANNELS_COMBINED: set_sensitive(view_combine_combined_menu(),FALSE); break;
    case CHANNELS_SUPERIMPOSED: set_sensitive(view_combine_superimposed_menu(),FALSE); break;
    }
  map_over_sounds(ss,update_sound,(void *)ss);
  map_over_chans(ss,update_graph,NULL);
}

#if HAVE_GUILE
#include "sg.h"

static SCM output_name_hook;

#if (!HAVE_GUILE_1_3_0)
static char *output_name(void)
{
  if (HOOKED(output_name_hook))
    {
      SCM result;
      SCM procs = SCM_HOOK_PROCEDURES (output_name_hook);
      while (SCM_NIMP (procs))
	{
	  result = g_call0(SCM_CAR(procs));
	  if (gh_string_p(result)) return(gh_scm2newstr(result,NULL));
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
  RTNSTR(save_state_file(ss));
}

static SCM g_set_save_state_file(SCM val) 
{
  #define H_save_state_file "(" S_save_state_file ") -> name of saved state file (\"saved-snd.scm\")"
  #define H_set_save_state_file "(" S_set_save_state_file " val) sets " S_save_state_file
  snd_state *ss;
  ERRS1(val,S_set_save_state_file); 
  ss = get_global_state();
  set_save_state_file(ss,gh_scm2newstr(val,0));
  RTNSTR(save_state_file(ss));
}


void g_init_menu(SCM local_doc)
{
#if (!HAVE_GUILE_1_3_0)
  output_name_hook = scm_create_hook(S_output_name_hook,0);
#else
  output_name_hook = gh_define(S_output_name_hook,SCM_BOOL_F);
#endif

  DEFINE_PROC(gh_new_procedure(S_save_state_file,SCM_FNC g_save_state_file,0,0,0),H_save_state_file);
  DEFINE_PROC(gh_new_procedure(S_set_save_state_file,SCM_FNC g_set_save_state_file,1,0,0),H_set_save_state_file);
}
#endif
