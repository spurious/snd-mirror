#include "snd.h"
#include <X11/cursorfont.h>

enum {menu_menu,
        file_menu, f_cascade_menu,
          f_open_menu, f_close_menu, f_save_menu, f_save_as_menu, f_revert_menu, f_exit_menu, f_new_menu,
          f_view_menu, f_print_menu, f_mix_menu, f_update_menu, f_record_menu, f_sep_menu,
        edit_menu, e_cascade_menu,
          e_cut_menu, e_paste_menu, e_mix_menu, e_play_menu, e_save_as_menu, e_undo_menu,
          e_redo_menu, e_find_menu, e_edenv_menu, e_header_menu, e_select_all_menu,
          e_select_sep_menu, e_edit_sep_menu,
        help_menu, h_cascade_menu,
          h_about_snd_menu, h_fft_menu, h_find_menu, h_undo_menu, h_sync_menu, h_controls_menu,
          h_env_menu, h_marks_menu, h_sound_files_menu, h_init_file_menu,
          h_mix_menu, h_recording_menu, h_keys_menu, 
          h_play_menu, h_save_menu, h_resample_menu, h_filter_menu, h_insert_menu, 
          h_delete_menu, h_reverb_menu, h_debug_menu,
        option_menu, o_cascade_menu,
          o_transform_menu,
          o_focus_style_menu, o_focus_cascade_menu,
            o_focus_right_menu, o_focus_left_menu, o_focus_middle_menu, o_focus_active_menu,
          o_save_menu, o_save_state_menu,
          o_speed_menu, o_speed_cascade_menu,
            o_speed_float_menu, o_speed_ratio_menu, o_speed_semitone_menu,
        view_menu, v_cascade_menu,
          v_equalize_panes_menu, 
          v_graph_style_menu, v_graph_style_cascade_menu,
            v_lines_menu, v_dots_menu, v_filled_menu, v_dots_and_lines_menu, v_lollipops_menu,
          v_zero_menu, v_cursor_menu, v_ctrls_menu, v_listener_menu,
          v_region_menu,
          v_combine_menu, v_combine_cascade_menu,
            v_combine_separate_menu, v_combine_combined_menu, v_combine_superimposed_menu,
          v_color_menu, v_orientation_menu, 
          v_files_menu, v_mix_panel_menu, v_track_panel_menu,
          v_x_axis_menu, v_x_axis_cascade_menu,
            v_x_axis_seconds_menu, v_x_axis_samples_menu, v_x_axis_percentage_menu, v_x_axis_beats_menu,
          v_error_history_menu,
          v_sep2_menu
};

#define NUM_MENU_WIDGETS 103
static Widget mw[NUM_MENU_WIDGETS];

enum {W_pop_menu, W_pop_sep, W_pop_play, W_pop_undo, W_pop_redo, W_pop_save, W_pop_equalize_panes, W_pop_info};
#define NUM_POPUP_CHILDREN 8
static Widget popup_menu = NULL;
static Widget popup_children[NUM_POPUP_CHILDREN];

bool popup_menu_exists(void) {return(popup_menu != NULL);}

Widget file_open_menu(void) {return(mw[f_open_menu]);}
Widget file_close_menu(void) {return(mw[f_close_menu]);}
Widget file_save_menu(void) {return(mw[f_save_menu]);}
Widget file_save_as_menu(void) {return(mw[f_save_as_menu]);}
Widget file_print_menu(void) {return(mw[f_print_menu]);}
Widget file_revert_menu(void) {return(mw[f_revert_menu]);}
Widget file_update_menu(void) {return(mw[f_update_menu]);}
Widget file_mix_menu(void) {return(mw[f_mix_menu]);}
Widget file_view_menu(void) {return(mw[f_view_menu]);}
Widget file_new_menu(void) {return(mw[f_new_menu]);}

Widget edit_cut_menu(void) {return(mw[e_cut_menu]);}
Widget edit_paste_menu(void) {return(mw[e_paste_menu]);}
Widget edit_mix_menu(void) {return(mw[e_mix_menu]);}
Widget edit_play_menu(void) {return(mw[e_play_menu]);}
Widget edit_save_as_menu(void) {return(mw[e_save_as_menu]);}
Widget edit_undo_menu(void) {return(mw[e_undo_menu]);}
Widget edit_redo_menu(void) {return(mw[e_redo_menu]);}
Widget edit_find_menu(void) {return(mw[e_find_menu]);}
Widget edit_select_all_menu(void) {return(mw[e_select_all_menu]);}
Widget edit_header_menu(void) {return(mw[e_header_menu]);}

Widget view_equalize_panes_menu(void) {return(mw[v_equalize_panes_menu]);}
Widget view_mix_panel_menu(void) {return(mw[v_mix_panel_menu]);}
Widget view_track_panel_menu(void) {return(mw[v_track_panel_menu]);}
Widget view_region_menu(void) {return(mw[v_region_menu]);}
Widget view_combine_separate_menu(void) {return(mw[v_combine_separate_menu]);}
Widget view_combine_combined_menu(void) {return(mw[v_combine_combined_menu]);}
Widget view_combine_superimposed_menu(void) {return(mw[v_combine_superimposed_menu]);}
Widget view_lines_menu(void) {return(mw[v_lines_menu]);}
Widget view_dots_menu(void) {return(mw[v_dots_menu]);}
Widget view_dots_and_lines_menu(void) {return(mw[v_dots_and_lines_menu]);}
Widget view_filled_menu(void) {return(mw[v_filled_menu]);}
Widget view_lollipops_menu(void) {return(mw[v_lollipops_menu]);}
Widget view_zero_menu(void) {return(mw[v_zero_menu]);}
Widget view_ctrls_menu(void) {return(mw[v_ctrls_menu]);}
Widget view_listener_menu(void) {return(mw[v_listener_menu]);}
Widget view_cursor_menu(void) {return(mw[v_cursor_menu]);}
Widget view_x_axis_seconds_menu(void) {return(mw[v_x_axis_seconds_menu]);}
Widget view_x_axis_beats_menu(void) {return(mw[v_x_axis_beats_menu]);}
Widget view_x_axis_samples_menu(void) {return(mw[v_x_axis_samples_menu]);}
Widget view_x_axis_percentage_menu(void) {return(mw[v_x_axis_percentage_menu]);}

Widget options_save_state_menu(void) {return(mw[o_save_state_menu]);}
Widget options_focus_left_menu(void) {return(mw[o_focus_left_menu]);}
Widget options_focus_right_menu(void) {return(mw[o_focus_right_menu]);}
Widget options_focus_middle_menu(void) {return(mw[o_focus_middle_menu]);}
Widget options_focus_active_menu(void) {return(mw[o_focus_active_menu]);}
Widget options_speed_ratio_menu(void) {return(mw[o_speed_ratio_menu]);}
Widget options_speed_float_menu(void) {return(mw[o_speed_float_menu]);}
Widget options_speed_semitone_menu(void) {return(mw[o_speed_semitone_menu]);}

Widget popup_play_menu(void) {return(popup_children[W_pop_play]);}
Widget popup_undo_menu(void) {return(popup_children[W_pop_undo]);}
Widget popup_redo_menu(void) {return(popup_children[W_pop_redo]);}
Widget popup_save_menu(void) {return(popup_children[W_pop_save]);}
Widget popup_equalize_panes_menu(void) {return(popup_children[W_pop_equalize_panes]);}
Widget popup_info_menu(void) {return(popup_children[W_pop_info]);}

void set_menu_label(Widget w, const char *label) {if (w) set_button_label(w, label);}

/* -------------------------------- FILE MENU -------------------------------- */

static void file_open_callback(Widget w, XtPointer info, XtPointer context) {make_open_file_dialog(false, true);}
static void file_view_callback(Widget w, XtPointer info, XtPointer context) {make_open_file_dialog(true, true);}
static void file_new_callback(Widget w, XtPointer info, XtPointer context) {new_file_from_menu();}
static void file_record_callback(Widget w, XtPointer info, XtPointer context) {snd_record_file();}
static void file_close_callback(Widget w, XtPointer info, XtPointer context) {close_file_from_menu();}
static void file_save_callback(Widget w, XtPointer info, XtPointer context) {save_file_from_menu();}
static void file_update_callback(Widget w, XtPointer info, XtPointer context) {update_file_from_menu();}
static void file_save_as_callback(Widget w, XtPointer info, XtPointer context) {make_file_save_as_dialog();}
static void file_revert_callback(Widget w, XtPointer info, XtPointer context) {revert_file_from_menu();}
static void file_exit_callback(Widget w, XtPointer info, XtPointer context) {exit_from_menu();}
static void file_mix_callback_1(Widget w, XtPointer info, XtPointer context) {make_mix_file_dialog(true);}
static void file_print_callback_1(Widget w, XtPointer info, XtPointer context) {file_print_callback(w, info, context);}


/* -------------------------------- EDIT MENU -------------------------------- */


static void edit_mix_callback(Widget w, XtPointer info, XtPointer context) {add_selection_or_region(0, selected_channel(), "Edit: mix");}
static void edit_envelope_callback(Widget w, XtPointer info, XtPointer context) {create_envelope_editor();}
static void edit_cut_callback(Widget w, XtPointer info, XtPointer context) {delete_selection("Edit: Cut", UPDATE_DISPLAY);}
static void edit_paste_callback(Widget w, XtPointer info, XtPointer context) {insert_selection_from_menu();}
static void edit_save_as_callback(Widget w, XtPointer info, XtPointer context) {make_edit_save_as_dialog();}
static void edit_select_all_callback(Widget w, XtPointer info, XtPointer context) {select_all(current_channel());}
static void edit_undo_callback(Widget w, XtPointer info, XtPointer context) {undo_edit_with_sync(current_channel(), 1);}
static void edit_redo_callback(Widget w, XtPointer info, XtPointer context) {redo_edit_with_sync(current_channel(), 1);}
static bool selection_play_stop = false;

static void edit_play_callback(Widget w, XtPointer info, XtPointer context) 
{
  if (selection_play_stop)
    {
      stop_playing_all_sounds();
    }
  else
    {
      set_menu_label(edit_play_menu(), _("Stop"));
      selection_play_stop = true;
      play_selection(IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "play selection", 0);
    }
}

void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu(), _("Play Selection"));
  selection_play_stop = false;
}

static void edit_header_callback_1(Widget w, XtPointer info, XtPointer context)
{
  snd_info *sp;
  sp = selected_sound();
  if (sp) edit_header(sp);
}

#if HAVE_EXTENSION_LANGUAGE
static void edit_find_callback_1(Widget w, XtPointer info, XtPointer context) 
{
  edit_find_callback(w, info, context);
}
#endif


/* -------------------------------- VIEW MENU -------------------------------- */

static void view_separate_callback(Widget w, XtPointer info, XtPointer context) {set_channel_style(CHANNELS_SEPARATE);}
static void view_combined_callback(Widget w, XtPointer info, XtPointer context) {set_channel_style(CHANNELS_COMBINED);}
static void view_superimposed_callback(Widget w, XtPointer info, XtPointer context) {set_channel_style(CHANNELS_SUPERIMPOSED);}
static void view_equalize_panes_callback(Widget w, XtPointer info, XtPointer context) {equalize_all_panes();}
static void view_dots_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_DOTS);}
static void view_lines_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_LINES);}
static void view_filled_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_FILLED);}
static void view_dots_and_lines_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_DOTS_AND_LINES);}
static void view_lollipops_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_LOLLIPOPS);}
#if HAVE_EXTENSION_LANGUAGE
static void view_listener_callback(Widget w, XtPointer info, XtPointer context) {handle_listener((listener_height() < 5));}
#endif
static void view_mix_panel_callback(Widget w, XtPointer info, XtPointer context) {make_mix_panel();}
static void view_track_panel_callback(Widget w, XtPointer info, XtPointer context) {make_track_panel();}
static void view_error_history_callback(Widget w, XtPointer info, XtPointer context) {show_snd_errors();}
static void view_zero_callback(Widget w, XtPointer info, XtPointer context){set_show_y_zero((!(show_y_zero(ss))));}
static void view_cursor_callback(Widget w, XtPointer info, XtPointer context){set_verbose_cursor((!(verbose_cursor(ss))));}
static void view_ctrls_callback(Widget w, XtPointer info, XtPointer context)
{
  if (ss->ctrls_height < 100) 
    show_controls(); 
  else hide_controls(); /* snd-xmain.c */
}
static void view_region_callback_1(Widget w, XtPointer info, XtPointer context) {view_region_callback(w, info, context);}
static void view_orientation_callback_1(Widget w, XtPointer info, XtPointer context) {view_orientation_callback(w, info, context);}
static void view_color_callback_1(Widget w, XtPointer info, XtPointer context) {view_color_callback(w, info, context);}
static void view_files_callback_1(Widget w, XtPointer info, XtPointer context) {view_files_callback(w, info, context);}
static void view_menu_update(Widget w, XtPointer info, XtPointer context) 
{
  /* make sure listener menu option label correctly reflects current listener state */
  set_view_listener_label((listener_height() > 10) ? _("Hide listener") : _("Show listener"));
}


/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_transform_callback(Widget w, XtPointer info, XtPointer context) {fire_up_transform_dialog(true);}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(Widget w, XtPointer info, XtPointer context) {save_options_from_menu();}
#endif
static void options_focus_right_callback(Widget w, XtPointer info, XtPointer Data) {activate_focus_menu(ZOOM_FOCUS_RIGHT);}
static void options_focus_left_callback(Widget w, XtPointer info, XtPointer Data) {activate_focus_menu(ZOOM_FOCUS_LEFT);}
static void options_focus_middle_callback(Widget w, XtPointer info, XtPointer Data) {activate_focus_menu(ZOOM_FOCUS_MIDDLE);}
static void options_focus_active_callback(Widget w, XtPointer info, XtPointer Data) {activate_focus_menu(ZOOM_FOCUS_ACTIVE);}
static void options_speed_float_callback(Widget w, XtPointer info, XtPointer Data) {activate_speed_in_menu(SPEED_CONTROL_AS_FLOAT);}
static void options_speed_ratio_callback(Widget w, XtPointer info, XtPointer Data) {activate_speed_in_menu(SPEED_CONTROL_AS_RATIO);}
static void options_speed_semitone_callback(Widget w, XtPointer info, XtPointer Data) {activate_speed_in_menu(SPEED_CONTROL_AS_SEMITONE);}
static void options_x_axis_seconds_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_IN_SECONDS);}
static void options_x_axis_beats_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_IN_BEATS);}
static void options_x_axis_samples_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_IN_SAMPLES);}
static void options_x_axis_percentage_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_AS_PERCENTAGE);}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(Widget w, XtPointer info, XtPointer context) {save_state_from_menu();}
#endif


/* -------------------------------- HELP MENU -------------------------------- */

static void help_about_snd_callback(Widget w, XtPointer info, XtPointer context) {about_snd_help();}
static void help_fft_callback (Widget w, XtPointer info, XtPointer context) {fft_help();}
static void help_find_callback (Widget w, XtPointer info, XtPointer context) {find_help();}
static void help_undo_callback (Widget w, XtPointer info, XtPointer context) {undo_help();}
static void help_sync_callback (Widget w, XtPointer info, XtPointer context) {sync_help();}
static void help_debug_callback (Widget w, XtPointer info, XtPointer context) {debug_help();}
static void help_controls_callback (Widget w, XtPointer info, XtPointer context) {controls_help();}
static void help_env_callback (Widget w, XtPointer info, XtPointer context) {env_help();}
static void help_marks_callback (Widget w, XtPointer info, XtPointer context) {marks_help();}
static void help_mix_callback (Widget w, XtPointer info, XtPointer context) {mix_help();}
static void help_sound_files_callback (Widget w, XtPointer info, XtPointer context) {sound_files_help();}
static void help_init_file_callback (Widget w, XtPointer info, XtPointer context) {init_file_help();}
static void help_recording_callback (Widget w, XtPointer info, XtPointer context) {recording_help();}
static void help_keys_callback (Widget w, XtPointer info, XtPointer context) {key_binding_help();}
static void help_play_callback (Widget w, XtPointer info, XtPointer context) {play_help();}
static void help_filter_callback (Widget w, XtPointer info, XtPointer context) {filter_help();}
static void help_save_callback (Widget w, XtPointer info, XtPointer context) {save_help();}
static void help_reverb_callback (Widget w, XtPointer info, XtPointer context) {reverb_help();}
static void help_resample_callback (Widget w, XtPointer info, XtPointer context) {resample_help();}
static void help_insert_callback (Widget w, XtPointer info, XtPointer context) {insert_help();}
static void help_delete_callback (Widget w, XtPointer info, XtPointer context) {delete_help();}

void check_menu_labels(int key, int state, bool extended)
{
  /* user has redefined key, so erase it from the menu label */
  if (extended)
    {
      if (state == snd_ControlMask)
	{
	  if (key == snd_K_f) set_label(mw[f_open_menu], _("Open")); else
	  if (key == snd_K_s) set_label(mw[f_save_menu], _("Save")); else
	  if (key == snd_K_q) set_label(mw[f_mix_menu], _("Mix")); else
	  if (key == snd_K_u) set_label(mw[e_undo_menu], _("Undo")); else
	  if (key == snd_K_r) set_label(mw[e_redo_menu], _("Redo"));
	}
      else
	{
	  if (key == snd_K_k) set_label(mw[f_close_menu], _("Close")); else
	  if (key == snd_K_i) set_label(mw[e_paste_menu], _("Insert Selection")); else	  
	  if (key == snd_K_q) set_label(mw[e_mix_menu], _("Mix Selection")); else	  
	  if (key == snd_K_p) set_label(mw[e_play_menu], _("Play Selection")); else	  
	  if (key == snd_K_w) set_label(mw[e_save_as_menu], _("Save Selection"));
	}
    }
  else 
    {
      if ((key == snd_K_s) && (state == snd_ControlMask))
	set_label(mw[e_find_menu], _("Find"));
    }
}

/* -------------------------------- MAIN MENU -------------------------------- */

void add_menu_drop(void)
{
  add_drop(mw[menu_menu]);
  /* can't figure out how to add the pulldown menus as well */
}

Widget add_menu(void)
{
  static Arg main_args[12];
  static Arg in_args[12];
  static Arg high_args[12];
  Arg sep_args[12];
  int in_n = 0, n, high_n = 0, main_n = 0, start_high_n, k, j;
  if (!(ss->using_schemes))
    {
      XtSetArg(main_args[main_n], XmNbackground, (ss->sgx)->basic_color); main_n++;
      XtSetArg(high_args[high_n], XmNbackground, (ss->sgx)->highlight_color); high_n++;
      XtSetArg(in_args[in_n], XmNbackground, (ss->sgx)->basic_color); in_n++;
    }
  start_high_n = high_n;
  XtSetArg(in_args[in_n], XmNsensitive, false); in_n++;
  
  n = high_n;
  XtSetArg(high_args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(high_args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(high_args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(high_args[n], XmNrightAttachment, XmATTACH_FORM); n++;

#ifdef SND_AS_WIDGET
  mw[menu_menu] = XtCreateWidget("mb", xmRowColumnWidgetClass, MAIN_PANE(ss), high_args, n);
#else
  mw[menu_menu] = XmCreateMenuBar(MAIN_PANE(ss), "menuBar", high_args, n);
#endif

  /* FILE MENU */
  XtSetArg(main_args[main_n], XmNuserData, 0);
  mw[file_menu] = XmCreatePulldownMenu(mw[menu_menu], "File", main_args, main_n + 1);
  
  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[file_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'F'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 0); high_n++;
  mw[f_cascade_menu] = XtCreateManagedWidget(_("File"), xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);

  mw[f_open_menu] = XtCreateManagedWidget(_("Open   C-x C-f"), xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_open_menu], XmNactivateCallback, file_open_callback, NULL);
  XtVaSetValues(mw[f_open_menu], XmNmnemonic, 'O', NULL);

  mw[f_close_menu] = XtCreateManagedWidget(_("Close  C-x k"), xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_close_menu], XmNactivateCallback, file_close_callback, NULL);
  XtVaSetValues(mw[f_close_menu], XmNmnemonic, 'C', NULL);
  
  mw[f_save_menu] = XtCreateManagedWidget(_("Save   C-x C-s"), xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_save_menu], XmNactivateCallback, file_save_callback, NULL);
  XtVaSetValues(mw[f_save_menu], XmNmnemonic, 'S', NULL);
  
  mw[f_save_as_menu] = XtCreateManagedWidget(_("Save as"), xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_save_as_menu], XmNactivateCallback, file_save_as_callback, NULL);
  XtVaSetValues(mw[f_save_as_menu], XmNmnemonic, 'a', NULL);
  
  mw[f_revert_menu] = XtCreateManagedWidget(_("Revert"), xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_revert_menu], XmNactivateCallback, file_revert_callback, NULL);
  XtVaSetValues(mw[f_revert_menu], XmNmnemonic, 'R', NULL);
  
  mw[f_mix_menu] = XtCreateManagedWidget(_("Mix    C-x C-q"), xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_mix_menu], XmNactivateCallback, file_mix_callback_1, NULL);
  XtVaSetValues(mw[f_mix_menu], XmNmnemonic, 'M', NULL);

  mw[f_update_menu] = XtCreateManagedWidget(_("Update"), xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_update_menu], XmNactivateCallback, file_update_callback, NULL);
  XtVaSetValues(mw[f_update_menu], XmNmnemonic, 'U', NULL);

  mw[f_new_menu] = XtCreateManagedWidget(_("New"), xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_new_menu], XmNactivateCallback, file_new_callback, NULL);
  XtVaSetValues(mw[f_new_menu], XmNmnemonic, 'N', NULL);

  mw[f_record_menu] = XtCreateManagedWidget(_("Record"), xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_record_menu], XmNactivateCallback, file_record_callback, NULL);

  mw[f_view_menu] = XtCreateManagedWidget(_("View"), xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_view_menu], XmNactivateCallback, file_view_callback, NULL);
  XtVaSetValues(mw[f_view_menu], XmNmnemonic, 'V', NULL);

  mw[f_print_menu] = XtCreateManagedWidget(_("Print"), xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_print_menu], XmNactivateCallback, file_print_callback_1, NULL);
  XtVaSetValues(mw[f_print_menu], XmNmnemonic, 'P', NULL);

  j = 0;
  if (!(ss->using_schemes)) {XtSetArg(sep_args[j], XmNbackground, (ss->sgx)->basic_color); j++;}
  XtSetArg(sep_args[j], XmNseparatorType, XmSHADOW_ETCHED_IN); j++;
  mw[f_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[file_menu], sep_args, j);

  mw[f_exit_menu] = XtCreateManagedWidget(_("Exit"), xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_exit_menu], XmNactivateCallback, file_exit_callback, NULL);
  XtVaSetValues(mw[f_exit_menu], XmNmnemonic, 'E', NULL);


  /* EDIT MENU */
  XtSetArg(main_args[main_n], XmNuserData, 1);
  mw[edit_menu] = XmCreatePulldownMenu(mw[menu_menu], "Edit", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[edit_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'E'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 1); high_n++;
  mw[e_cascade_menu] = XtCreateManagedWidget(_("Edit"), xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);
  
  mw[e_undo_menu] = XtCreateManagedWidget(_("Undo    C-x C-u"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_undo_menu], XmNactivateCallback, edit_undo_callback, NULL);
  XtVaSetValues(mw[e_undo_menu], XmNmnemonic, 'U', NULL);

  mw[e_redo_menu] = XtCreateManagedWidget(_("Redo    C-x C-r"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_redo_menu], XmNactivateCallback, edit_redo_callback, NULL);
  XtVaSetValues(mw[e_redo_menu], XmNmnemonic, 'R', NULL);

#if HAVE_EXTENSION_LANGUAGE
  mw[e_find_menu] = XtCreateManagedWidget(_("Find    C-s"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_find_menu], XmNactivateCallback, edit_find_callback_1, NULL);
  XtVaSetValues(mw[e_find_menu], XmNmnemonic, 'F', NULL);
#endif

  mw[e_select_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[edit_menu], sep_args, j);

  mw[e_cut_menu] = XtCreateManagedWidget(_("Delete Selection"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_cut_menu], XmNactivateCallback, edit_cut_callback, NULL);
  XtVaSetValues(mw[e_cut_menu], XmNmnemonic, 'C', NULL);

  mw[e_paste_menu] = XtCreateManagedWidget(_("Insert Selection C-x i"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_paste_menu], XmNactivateCallback, edit_paste_callback, NULL);
  XtVaSetValues(mw[e_paste_menu], XmNmnemonic, 'P', NULL);

  mw[e_mix_menu] = XtCreateManagedWidget(_("Mix Selection    C-x q"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_mix_menu], XmNactivateCallback, edit_mix_callback, NULL);
  XtVaSetValues(mw[e_mix_menu], XmNmnemonic, 'M', NULL);

  mw[e_play_menu] = XtCreateManagedWidget(_("Play Selection   C-x p"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_play_menu], XmNactivateCallback, edit_play_callback, NULL);
  XtVaSetValues(mw[e_play_menu], XmNmnemonic, 'P', NULL);

  mw[e_save_as_menu] = XtCreateManagedWidget(_("Save Selection   C-x w"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_save_as_menu], XmNactivateCallback, edit_save_as_callback, NULL);
  XtVaSetValues(mw[e_save_as_menu], XmNmnemonic, 'S', NULL);

  mw[e_select_all_menu] = XtCreateManagedWidget(_("Select all"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_select_all_menu], XmNactivateCallback, edit_select_all_callback, NULL);

  mw[e_edit_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[edit_menu], sep_args, j);

  mw[e_edenv_menu] = XtCreateManagedWidget(_("Edit Envelope"), xmPushButtonWidgetClass, mw[edit_menu], main_args, main_n);
  XtAddCallback(mw[e_edenv_menu], XmNactivateCallback, edit_envelope_callback, NULL);
  XtVaSetValues(mw[e_edenv_menu], XmNmnemonic, 'E', NULL);

  mw[e_header_menu] = XtCreateManagedWidget(_("Edit Header"), xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_header_menu], XmNactivateCallback, edit_header_callback_1, NULL);
  XtVaSetValues(mw[e_header_menu], XmNmnemonic, 'H', NULL);


  /* VIEW MENU */
  XtSetArg(main_args[main_n], XmNuserData, 2);
  mw[view_menu] = XmCreatePulldownMenu(mw[menu_menu], "View", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[view_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'V'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 2); high_n++;
  mw[v_cascade_menu] = XtCreateManagedWidget(_("View"), xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);
  XtAddCallback(mw[v_cascade_menu], XmNcascadingCallback, view_menu_update, NULL);

  mw[v_ctrls_menu] = XtCreateManagedWidget(_("Show controls"), xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_ctrls_menu], XmNactivateCallback, view_ctrls_callback, NULL);
  XtVaSetValues(mw[v_ctrls_menu], XmNmnemonic, 'S', NULL);

#if HAVE_EXTENSION_LANGUAGE
  mw[v_listener_menu] = XtCreateManagedWidget(_("Open listener"), xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_listener_menu], XmNactivateCallback, view_listener_callback, NULL);
  XtVaSetValues(mw[v_listener_menu], XmNmnemonic, 'L', NULL);
#endif

  mw[v_mix_panel_menu] = XtCreateManagedWidget(_("Mixes"), xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_mix_panel_menu], XmNactivateCallback, view_mix_panel_callback, NULL);

  mw[v_track_panel_menu] = XtCreateManagedWidget(_("Tracks"), xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_track_panel_menu], XmNactivateCallback, view_track_panel_callback, NULL);

  mw[v_region_menu] = XtCreateManagedWidget(_("Regions"), xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_region_menu], XmNactivateCallback, view_region_callback_1, NULL);
  XtVaSetValues(mw[v_region_menu], XmNmnemonic, 'R', NULL);

  mw[v_files_menu] = XtCreateManagedWidget(_("Files"), xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_files_menu], XmNactivateCallback, view_files_callback_1, NULL);
  XtVaSetValues(mw[v_files_menu], XmNmnemonic, 'F', NULL);

  mw[v_color_menu] = XtCreateManagedWidget(_("Color"), xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_color_menu], XmNactivateCallback, view_color_callback_1, NULL);

  mw[v_orientation_menu] = XtCreateManagedWidget(_("Orientation"), xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_orientation_menu], XmNactivateCallback, view_orientation_callback_1, NULL);
  XtVaSetValues(mw[v_orientation_menu], XmNmnemonic, 'O', NULL);

  mw[v_sep2_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[view_menu], sep_args, j);

  mw[v_graph_style_menu] = XmCreatePulldownMenu(mw[view_menu], "graph-style", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_graph_style_menu]); k++;
  mw[v_graph_style_cascade_menu] = XtCreateManagedWidget(_("Graph style"), xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_lines_menu] = XtCreateManagedWidget(_("lines"), xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_lines_menu], XmNactivateCallback, view_lines_callback, NULL); 
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(mw[v_lines_menu], false);

  mw[v_dots_menu] = XtCreateManagedWidget(_("dots"), xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_dots_menu], XmNactivateCallback, view_dots_callback, NULL);  
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(mw[v_dots_menu], false);

  mw[v_filled_menu] = XtCreateManagedWidget(_("filled"), xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_filled_menu], XmNactivateCallback, view_filled_callback, NULL);  
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(mw[v_filled_menu], false);

  mw[v_dots_and_lines_menu] = XtCreateManagedWidget(_("dots and lines"), xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_dots_and_lines_menu], XmNactivateCallback, view_dots_and_lines_callback, NULL);  
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(mw[v_dots_and_lines_menu], false);

  mw[v_lollipops_menu] = XtCreateManagedWidget(_("lollipops"), xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_lollipops_menu], XmNactivateCallback, view_lollipops_callback, NULL);  
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(mw[v_lollipops_menu], false);

  mw[v_cursor_menu] = XtCreateManagedWidget(_("Verbose cursor"), xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_cursor_menu], XmNactivateCallback, view_cursor_callback, NULL);

  mw[v_combine_menu] = XmCreatePulldownMenu(mw[view_menu], "combine", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_combine_menu]); k++;
  mw[v_combine_cascade_menu] = XtCreateManagedWidget(_("Channel style"), xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_combine_separate_menu] = XtCreateManagedWidget(_("separate"), xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_separate_menu], XmNactivateCallback, view_separate_callback, NULL); 
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(mw[v_combine_separate_menu], false);

  mw[v_combine_combined_menu] = XtCreateManagedWidget(_("combined"), xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_combined_menu], XmNactivateCallback, view_combined_callback, NULL);  
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(mw[v_combine_combined_menu], false);

  mw[v_combine_superimposed_menu] = XtCreateManagedWidget(_("superimposed"), xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_superimposed_menu], XmNactivateCallback, view_superimposed_callback, NULL);  
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(mw[v_combine_superimposed_menu], false);

  mw[v_equalize_panes_menu] = XtCreateManagedWidget(_("Equalize Panes"), xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_equalize_panes_menu], XmNactivateCallback, view_equalize_panes_callback, NULL);
  XtVaSetValues(mw[v_equalize_panes_menu], XmNmnemonic, 'N', NULL);

  mw[v_zero_menu] = XtCreateManagedWidget(_("Show Y = 0"), xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_zero_menu], XmNactivateCallback, view_zero_callback, NULL);
  XtVaSetValues(mw[v_zero_menu], XmNmnemonic, 'y', NULL);

  mw[v_x_axis_menu] = XmCreatePulldownMenu(mw[view_menu], "xaxis", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_x_axis_menu]); k++;
  mw[v_x_axis_cascade_menu] = XtCreateManagedWidget(_("X axis units"), xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_x_axis_seconds_menu] = XtCreateManagedWidget(_("seconds"), xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_seconds_menu], XmNactivateCallback, options_x_axis_seconds_callback, NULL);  
  set_sensitive(mw[v_x_axis_seconds_menu], false);

  mw[v_x_axis_samples_menu] = XtCreateManagedWidget(_("samples"), xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_samples_menu], XmNactivateCallback, options_x_axis_samples_callback, NULL);  

  mw[v_x_axis_percentage_menu] = XtCreateManagedWidget(_("percentage"), xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_percentage_menu], XmNactivateCallback, options_x_axis_percentage_callback, NULL);  

  mw[v_x_axis_beats_menu] = XtCreateManagedWidget(_("beats"), xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_beats_menu], XmNactivateCallback, options_x_axis_beats_callback, NULL);  

  mw[v_error_history_menu] = XtCreateManagedWidget(_("Error History"), xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_error_history_menu], XmNactivateCallback, view_error_history_callback, NULL);


  /* OPTIONS MENU */
  XtSetArg(main_args[main_n], XmNuserData, 3);
  mw[option_menu] = XmCreatePulldownMenu(mw[menu_menu], "Option", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[option_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'O'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 3); high_n++;
  mw[o_cascade_menu] = XtCreateManagedWidget(_("Options"), xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);

  mw[o_transform_menu] = XtCreateManagedWidget(_("Transform Options"), xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_transform_menu], XmNactivateCallback, options_transform_callback, NULL);
  XtVaSetValues(mw[o_transform_menu], XmNmnemonic, 't', NULL);


  mw[o_speed_menu] = XmCreatePulldownMenu(mw[option_menu], "speedstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[o_speed_menu]); k++;
  mw[o_speed_cascade_menu] = XtCreateManagedWidget(_("Speed style"), xmCascadeButtonWidgetClass, mw[option_menu], main_args, k);

  mw[o_speed_float_menu] = XtCreateManagedWidget(_("float"), xmPushButtonWidgetClass, mw[o_speed_menu], in_args, in_n);
  XtAddCallback(mw[o_speed_float_menu], XmNactivateCallback, options_speed_float_callback, NULL);  

  mw[o_speed_semitone_menu] = XtCreateManagedWidget(_("semitones"), xmPushButtonWidgetClass, mw[o_speed_menu], main_args, main_n);
  XtAddCallback(mw[o_speed_semitone_menu], XmNactivateCallback, options_speed_semitone_callback, NULL);  

  mw[o_speed_ratio_menu] = XtCreateManagedWidget(_("ratio"), xmPushButtonWidgetClass, mw[o_speed_menu], main_args, main_n);
  XtAddCallback(mw[o_speed_ratio_menu], XmNactivateCallback, options_speed_ratio_callback, NULL);  


  mw[o_focus_style_menu] = XmCreatePulldownMenu(mw[option_menu], "focusstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[o_focus_style_menu]); k++;
  mw[o_focus_cascade_menu] = XtCreateManagedWidget(_("Zoom focus"), xmCascadeButtonWidgetClass, mw[option_menu], main_args, k);

  mw[o_focus_left_menu] = XtCreateManagedWidget(_("window left edge"), xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_left_menu], XmNactivateCallback, options_focus_left_callback, NULL);  

  mw[o_focus_right_menu] = XtCreateManagedWidget(_("window right edge"), xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_right_menu], XmNactivateCallback, options_focus_right_callback, NULL);  

  mw[o_focus_middle_menu] = XtCreateManagedWidget(_("window midpoint"), xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_middle_menu], XmNactivateCallback, options_focus_middle_callback, NULL);  

  mw[o_focus_active_menu] = XtCreateManagedWidget(_("cursor or selection"), xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_active_menu], XmNactivateCallback, options_focus_active_callback, NULL);  
  activate_focus_menu(zoom_focus_style(ss));

#if HAVE_EXTENSION_LANGUAGE
  mw[o_save_menu] = XtCreateManagedWidget(_("Save options"), xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_save_menu], XmNactivateCallback, options_save_callback, NULL);
  XtVaSetValues(mw[o_save_menu], XmNmnemonic, 'a', NULL);

  mw[o_save_state_menu] = XtCreateManagedWidget(_("Save state"), xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_save_state_menu], XmNactivateCallback, options_save_state_callback, NULL);
#endif


  /* HELP MENU */
  XtSetArg(main_args[main_n], XmNuserData, 4);
  mw[help_menu] = XmCreatePulldownMenu(mw[menu_menu], "Help", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[help_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'H'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 4); high_n++;
  mw[h_cascade_menu] = XtCreateManagedWidget(_("Help"), xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);

  mw[h_about_snd_menu] = XtCreateManagedWidget(_("About Snd"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_about_snd_menu], XmNactivateCallback, help_about_snd_callback, NULL);
  XtVaSetValues(mw[h_about_snd_menu], XmNmnemonic, 'O', NULL);

  mw[h_init_file_menu] = XtCreateManagedWidget(_("Customization"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_init_file_menu], XmNactivateCallback, help_init_file_callback, NULL);

  mw[h_controls_menu] = XtCreateManagedWidget(_("Control Panel"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_controls_menu], XmNactivateCallback, help_controls_callback, NULL);

  mw[h_keys_menu] = XtCreateManagedWidget(_("Key bindings"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_keys_menu], XmNactivateCallback, help_keys_callback, NULL);

  mw[h_recording_menu] = XtCreateManagedWidget(_("Record"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_recording_menu], XmNactivateCallback, help_recording_callback, NULL);

  mw[h_play_menu] = XtCreateManagedWidget(_("Play"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_play_menu], XmNactivateCallback, help_play_callback, NULL);

  mw[h_save_menu] = XtCreateManagedWidget(_("Save"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_save_menu], XmNactivateCallback, help_save_callback, NULL);

  mw[h_mix_menu] = XtCreateManagedWidget(_("Mix"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_mix_menu], XmNactivateCallback, help_mix_callback, NULL);

  mw[h_resample_menu] = XtCreateManagedWidget(_("Resample"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_resample_menu], XmNactivateCallback, help_resample_callback, NULL);

  mw[h_fft_menu] = XtCreateManagedWidget(_("FFT"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_fft_menu], XmNactivateCallback, help_fft_callback, NULL);

  mw[h_filter_menu] = XtCreateManagedWidget(_("Filter"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_filter_menu], XmNactivateCallback, help_filter_callback, NULL);

  mw[h_reverb_menu] = XtCreateManagedWidget(_("Reverb"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_reverb_menu], XmNactivateCallback, help_reverb_callback, NULL);

  mw[h_env_menu] = XtCreateManagedWidget(_("Envelope"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_env_menu], XmNactivateCallback, help_env_callback, NULL);

  mw[h_marks_menu] = XtCreateManagedWidget(_("Mark"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_marks_menu], XmNactivateCallback, help_marks_callback, NULL);

  mw[h_insert_menu] = XtCreateManagedWidget(_("Insert"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_insert_menu], XmNactivateCallback, help_insert_callback, NULL);

  mw[h_delete_menu] = XtCreateManagedWidget(_("Delete"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_delete_menu], XmNactivateCallback, help_delete_callback, NULL);

  mw[h_undo_menu] = XtCreateManagedWidget(_("Undo and redo"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_undo_menu], XmNactivateCallback, help_undo_callback, NULL);

  mw[h_find_menu] = XtCreateManagedWidget(_("Search"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_find_menu], XmNactivateCallback, help_find_callback, NULL);

  mw[h_sync_menu] = XtCreateManagedWidget(_("Sync"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_sync_menu], XmNactivateCallback, help_sync_callback, NULL);

  mw[h_sound_files_menu] = XtCreateManagedWidget(_("Headers and Data"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_sound_files_menu], XmNactivateCallback, help_sound_files_callback, NULL);

  mw[h_debug_menu] = XtCreateManagedWidget(_("Debugging"), xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_debug_menu], XmNactivateCallback, help_debug_callback, NULL);

  XtVaSetValues(mw[menu_menu], XmNmenuHelpWidget, mw[h_cascade_menu], NULL);
#ifndef SND_AS_WIDGET
  XtManageChild(mw[menu_menu]);
#endif
  return(mw[menu_menu]);
}

#define INVALID_MENU -1
#define CALL_INDEX(Data) (Data >> 16)
#define MENU_INDEX(Data) (Data & 0xffff)
#define PACK_MENU_DATA(Slot, Menu) ((Slot << 16) | (Menu))

static void SND_callback(Widget w, XtPointer info, XtPointer context) 
{
  int callb;
  XtVaGetValues(w, XmNuserData, &callb, NULL);
  g_snd_callback(CALL_INDEX(callb)); /* menu option activate callback */
}

static void GHC_callback(Widget w, XtPointer info, XtPointer context) 
{
  int slot;
  XtVaGetValues(w, XmNuserData, &slot, NULL);
  g_snd_callback(CALL_INDEX(slot)); /* main menu cascading callback */
}

#include <X11/IntrinsicP.h>

Widget menu_widget(int which_menu)
{
  unsigned int i;
  Widget w, subw;
  CompositeWidget cw;
  int menu;
  if (which_menu == 5) return(popup_menu); /* special case -- not in main menuBar, presumably */
  w = mw[menu_menu];
  cw = (CompositeWidget)w;
  for (i = 0; i < cw->composite.num_children; i++)
    {
      w = cw->composite.children[i];
      if ((w) && (XtIsManaged(w)))
	{
	  XtVaGetValues(w, XmNuserData, &menu, NULL);
	  /* fprintf(stderr,"%s: menu: %d, slot: %d (%x)\n", XtName(w), MENU_INDEX(menu), CALL_INDEX(menu), menu); */
	  if (which_menu == MENU_INDEX(menu))
	    {
	      XtVaGetValues(w, XmNsubMenuId, &subw, NULL);
	      return(subw);
	    }
	}
    }
  return(NULL);
}

static bool or_over_children(Widget w, bool (*func)(Widget, void *), void *userptr)
{
  unsigned int i;
  if (w)
    {
      if ((*func)(w, userptr)) return(true);
      if (XtIsComposite(w))
	{
	  CompositeWidget cw = (CompositeWidget)w;
	  for (i = 0; i < cw->composite.num_children; i++)
	    if (or_over_children(cw->composite.children[i], func, userptr))
	      return(true);
	}
    }
  return(false);
}

static bool clobber_menu(Widget w, void *lab)
{
  char *name, *wname;
  name = (char *)lab;
  wname = XtName(w);
  if ((wname) && (name) && (strcmp(name, wname) == 0) && (XtIsManaged(w)))
    {
      int slot;
      XtVaGetValues(w, XmNuserData, &slot, NULL);
      unprotect_callback(CALL_INDEX(slot));
      XtUnmanageChild(w);
      return(true);
    }
  return(false);
}

int g_remove_from_menu(int which_menu, char *label)
{
  Widget top_menu;
  top_menu = menu_widget(which_menu);
  if (top_menu)
    {
      or_over_children(top_menu, clobber_menu, (void *)label);
      return(0);
    }
  return(INVALID_MENU);
}

static bool change_menu(Widget w, void *ulabels)
{
  char *name, *wname;
  char **labels = (char **)ulabels;
  name = labels[0];
  wname = XtName(w);
  if ((wname) && (name) && (strcmp(name, wname) == 0) && (XtIsManaged(w)))
    {
      set_button_label(w, labels[1]);
      return(true);
    }
  return(false);
}

int g_change_menu_label(int which_menu, char *old_label, char *new_label)
{
  Widget top_menu;
  char *labels[2];
  labels[0] = old_label;
  labels[1] = new_label;
  top_menu = menu_widget(which_menu);
  if (top_menu)
    {
      or_over_children(top_menu, change_menu, (void *)labels);
      return(0);
    }
  return(INVALID_MENU);
}

typedef struct {
  char *label;
  bool on;
} smenu;

static bool sensitize_menu(Widget w, void *usm)
{
  smenu *ism = (smenu *)usm;
  char *name, *wname;
  name = ism->label;
  wname = XtName(w);
  if ((wname) && (name) && (strcmp(name, wname) == 0) && (XtIsManaged(w)))
    {
      set_sensitive(w, ism->on);
      return(true);
    }
  return(false);
}

int g_set_menu_sensitive(int which_menu, char *old_label, bool on)
{
  Widget top_menu;
  smenu sm;
  top_menu = menu_widget(which_menu);
  if (top_menu)
    {
      sm.label = old_label;
      sm.on = on;
      or_over_children(top_menu, sensitize_menu, (void *)(&sm));
      return(0);
    }
  return(INVALID_MENU);
}

static bool sensitive_menu_p(Widget w, void *usm)
{
  smenu *ism = (smenu *)usm;
  char *name, *wname;
  name = ism->label;
  wname = XtName(w);
  if ((wname) && (name) && (strcmp(name, wname) == 0) && (XtIsManaged(w)))
    {
      ism->on = is_sensitive(w);
      return(true);
    }
  return(false);
}

int g_menu_is_sensitive(int which_menu, char *old_label)
{
  Widget top_menu;
  smenu sm;
  top_menu = menu_widget(which_menu);
  if (top_menu)
    {
      sm.label = old_label;
      sm.on = false;
      or_over_children(top_menu, sensitive_menu_p, (void *)(&sm));
      return(sm.on);
    }
  return(INVALID_MENU);
}

static void set_widget_name(Widget w, char *new_name)
{
  /* based on XtName in Xt/Intrinsic.c, Xt/Create.c, and Xt/ResourceI.h */
  w->core.xrm_name = XrmStringToName(new_name);
}

static int new_menu = 5;

int g_add_to_main_menu(char *label, int slot)
{
  static Arg args[12];
  Widget m, cas;
  int n;
  if (auto_resize(ss)) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, false, NULL);
  new_menu++;
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(slot, new_menu)); n++;
  m = XmCreatePulldownMenu(mw[menu_menu], label, args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n], XmNsubMenuId, m); n++;
  XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(slot, new_menu)); n++;
  cas = XtCreateManagedWidget(label, xmCascadeButtonWidgetClass, mw[menu_menu], args, n);
  if (slot >= 0) XtAddCallback(cas, XmNcascadingCallback, GHC_callback, NULL);

  if (auto_resize(ss)) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL);
  return(new_menu);
}

int g_add_to_menu(int which_menu, char *label, int callb, int position)
{
  Widget m, menw;
  Arg args[12];
  int n = 0;
  unsigned int i;
  menw = menu_widget(which_menu);
  if (menw == NULL) return(INVALID_MENU);
  if (label)
    {
      /* look for currently unused widget first */
      CompositeWidget cw = (CompositeWidget)menw;
      for (i = 0; i < cw->composite.num_children; i++)
	{
	  m = cw->composite.children[i];
	  if ((m) && (!(XtIsManaged(m))))
	    {
	      if (strcmp(XtName(m), label) != 0)
		{
		  set_widget_name(m, label);
		  set_button_label(m, label);
		}
	      if (position >= 0) XtVaSetValues(m, XmNpositionIndex, position, NULL);
	      XtVaSetValues(m, XmNuserData, PACK_MENU_DATA(callb, which_menu), NULL);
	      XtManageChild(m);
	      return(0);
	    }
	}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
      XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(callb, which_menu)); n++;
      m = XtCreateManagedWidget(label, xmPushButtonWidgetClass, menw, args, n);
      XtAddCallback(m, XmNactivateCallback, SND_callback, NULL);
    }
  else
    {
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
      XtCreateManagedWidget("sep", xmSeparatorWidgetClass, menw, args, n);
    }
  return(0);
}


/* -------------------------------- POPUP MENU -------------------------------- */

static bool stopping = false;

static void popup_play_callback(Widget w, XtPointer info, XtPointer context) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (stopping)
    {
      stop_playing_all_sounds();
      stopping = false;
      set_button_label(w, _("Play"));
      if (sp) set_play_button(sp, false);
    }
  else
    {
      if (sp)
	{
	  play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "popup play", 0);
	  stopping = true;
	  set_button_label(w, _("Stop playing"));
	}
    }
}

void reflect_play_stop_in_popup_menu(void)
{
  stopping = false;
  if (popup_menu)
    set_button_label(popup_children[W_pop_play], _("Play"));
}

static void popup_save_callback(Widget w, XtPointer info, XtPointer context) {save_edits(any_selected_sound(), NULL);}
static void popup_undo_callback(Widget w, XtPointer info, XtPointer context) {undo_edit_with_sync(current_channel(), 1);}
static void popup_redo_callback(Widget w, XtPointer info, XtPointer context) {redo_edit_with_sync(current_channel(), 1);}
static void popup_equalize_panes_callback(Widget w, XtPointer info, XtPointer context) {equalize_all_panes();}
static void popup_info_callback(Widget w, XtPointer info, XtPointer context) 
{
  snd_info *sp;
  sp = selected_sound();
  if (sp) display_info(sp);
}

void post_popup(XButtonPressedEvent *event)
{
  XmMenuPosition(popup_menu, event);
  XtManageChild(popup_menu);
}

#if (XmVERSION == 1)
static void post_popup_menu(Widget w, XtPointer info, XEvent *event, Boolean *flag)
{
  if (event->xbutton.button == Button3)
    post_popup((XButtonPressedEvent *)event);
}
#endif

void create_popup_menu(void)
{
  /* make it a child of the main window */
  Widget mainp;
  Arg args[20];
  int n;
  if (!popup_menu)
    {
      n = 0;
      if (!ss->using_schemes) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      mainp = MAIN_PANE(ss);
#if (XmVERSION >= 2)
      XtSetArg(args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC_RECURSIVE); n++;
#endif
      XtSetArg(args[n], XmNuserData, 5);
      popup_menu = XmCreatePopupMenu(mainp, "popup-menu", args, n + 1);
#if (XmVERSION == 1)
      XtAddEventHandler(mainp, ButtonPressMask, false, post_popup_menu, popup_menu);
#endif

      popup_children[W_pop_menu] = XtCreateManagedWidget("snd", xmLabelWidgetClass, popup_menu, args, n);
      popup_children[W_pop_sep] = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, popup_menu, args, n);
      popup_children[W_pop_play] = XtCreateManagedWidget(_("Play"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_children[W_pop_play], XmNactivateCallback, popup_play_callback, NULL);
      XtVaSetValues(popup_children[W_pop_play], XmNsensitive, false, NULL);
      popup_children[W_pop_undo] = XtCreateManagedWidget(_("Undo"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_undo], XmNsensitive, false, NULL);
      XtAddCallback(popup_children[W_pop_undo], XmNactivateCallback, popup_undo_callback, NULL);
      popup_children[W_pop_redo] = XtCreateManagedWidget(_("Redo"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_redo], XmNsensitive, false, NULL);
      XtAddCallback(popup_children[W_pop_redo], XmNactivateCallback, popup_redo_callback, NULL);
      popup_children[W_pop_save] = XtCreateManagedWidget(_("Save"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_children[W_pop_save], XmNactivateCallback, popup_save_callback, NULL);
      XtVaSetValues(popup_children[W_pop_save], XmNsensitive, false, NULL);
      popup_children[W_pop_equalize_panes] = XtCreateManagedWidget(_("Equalize Panes"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_equalize_panes], XmNsensitive, false, NULL);
      XtAddCallback(popup_children[W_pop_equalize_panes], XmNactivateCallback, popup_equalize_panes_callback, NULL);
      popup_children[W_pop_info] = XtCreateManagedWidget(_("Info"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_info], XmNsensitive, false, NULL);
      XtAddCallback(popup_children[W_pop_info], XmNactivateCallback, popup_info_callback, NULL);
    }
}

static XEN g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets "): a list of top level menu widgets: ((0)main (1)file (2)edit (3)view (4)options (5)help)"
  return(XEN_CONS(XEN_WRAP_WIDGET(mw[menu_menu]),
	  XEN_CONS(XEN_WRAP_WIDGET(mw[f_cascade_menu]),
           XEN_CONS(XEN_WRAP_WIDGET(mw[e_cascade_menu]),
            XEN_CONS(XEN_WRAP_WIDGET(mw[v_cascade_menu]),
             XEN_CONS(XEN_WRAP_WIDGET(mw[o_cascade_menu]),
              XEN_CONS(XEN_WRAP_WIDGET(mw[h_cascade_menu]),
	       XEN_EMPTY_LIST)))))));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_menu_widgets_w, g_menu_widgets)
#else
#define g_menu_widgets_w g_menu_widgets
#endif

void g_init_gxmenu(void)
{
  XEN_DEFINE_PROCEDURE(S_menu_widgets, g_menu_widgets_w, 0, 0, 0, H_menu_widgets);
}
