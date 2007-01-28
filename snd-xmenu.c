#include "snd.h"
#include "snd-menu.h"
#include <X11/cursorfont.h>

void set_menu_label(Widget w, const char *label) {if (w) set_button_label(w, label);}

/* -------------------------------- FILE MENU -------------------------------- */

static void file_menu_update_1(Widget w, XtPointer info, XtPointer context) {file_menu_update();}
static void file_open_callback(Widget w, XtPointer info, XtPointer context) {make_open_file_dialog(false, true);}
static void file_view_callback(Widget w, XtPointer info, XtPointer context) {make_open_file_dialog(true, true);}
static void file_new_callback(Widget w, XtPointer info, XtPointer context) {make_new_file_dialog(true);}
static void file_record_callback(Widget w, XtPointer info, XtPointer context) {snd_record_file();}
static void file_close_callback(Widget w, XtPointer info, XtPointer context) {if (any_selected_sound()) snd_close_file(any_selected_sound());}
static void file_save_callback(Widget w, XtPointer info, XtPointer context) {if (any_selected_sound()) save_edits_with_prompt(any_selected_sound());}
static void file_update_callback(Widget w, XtPointer info, XtPointer context) {update_file_from_menu();}
static void file_save_as_callback(Widget w, XtPointer info, XtPointer context) {make_sound_save_as_dialog(true);}
static void file_revert_callback(Widget w, XtPointer info, XtPointer context) {revert_file_from_menu();}
static void file_exit_callback(Widget w, XtPointer info, XtPointer context) {if (snd_exit_cleanly(EXIT_NOT_FORCED)) snd_exit(1);}
static void file_mix_callback_1(Widget w, XtPointer info, XtPointer context) {make_mix_file_dialog(true);}
static void file_insert_callback_1(Widget w, XtPointer info, XtPointer context) {make_insert_file_dialog(true);}
static void file_print_callback_1(Widget w, XtPointer info, XtPointer context) {file_print_callback(w, info, context);}


/* -------------------------------- EDIT MENU -------------------------------- */

static void edit_mix_callback(Widget w, XtPointer info, XtPointer context) {add_selection_or_region(0, selected_channel());}
static void edit_envelope_callback(Widget w, XtPointer info, XtPointer context) {create_envelope_editor();}
static void edit_cut_callback(Widget w, XtPointer info, XtPointer context) {delete_selection(UPDATE_DISPLAY);}
static void edit_paste_callback(Widget w, XtPointer info, XtPointer context) {insert_selection_from_menu();}
static void edit_save_as_callback(Widget w, XtPointer info, XtPointer context) {make_selection_save_as_dialog(true);}
static void edit_select_all_callback(Widget w, XtPointer info, XtPointer context) {select_all(current_channel());}
static void edit_undo_callback(Widget w, XtPointer info, XtPointer context) {undo_edit_with_sync(current_channel(), 1);}
static void edit_redo_callback(Widget w, XtPointer info, XtPointer context) {redo_edit_with_sync(current_channel(), 1);}

static void edit_menu_update_1(Widget w, XtPointer info, XtPointer context) {edit_menu_update();}

static bool selection_play_stop = false;

static void edit_play_callback(Widget w, XtPointer info, XtPointer context) 
{
  if (selection_play_stop)
    {
      stop_playing_all_sounds(PLAY_BUTTON_UNSET);
    }
  else
    {
      set_menu_label(edit_play_menu, _("Stop"));
      selection_play_stop = true;
      play_selection(IN_BACKGROUND);
    }
}

void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu, _("Play Selection"));
  selection_play_stop = false;
}

static void edit_header_callback_1(Widget w, XtPointer info, XtPointer context)
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) edit_header(sp);
}

#if HAVE_EXTENSION_LANGUAGE
static void edit_find_callback_1(Widget w, XtPointer info, XtPointer context) 
{
  edit_find_callback(w, info, context);
}
#endif


/* -------------------------------- VIEW MENU -------------------------------- */

static void view_menu_update_1(Widget w, XtPointer info, XtPointer context) {view_menu_update();}
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
static void view_listener_callback(Widget w, XtPointer info, XtPointer context) {handle_listener(!(listener_is_visible()));}
#endif
static void view_mix_dialog_callback(Widget w, XtPointer info, XtPointer context) {make_mix_dialog();}
static void view_track_dialog_callback(Widget w, XtPointer info, XtPointer context) {make_track_dialog();}
static void view_zero_callback(Widget w, XtPointer info, XtPointer context){set_show_y_zero((!(show_y_zero(ss))));}
static void view_cursor_callback(Widget w, XtPointer info, XtPointer context){set_verbose_cursor((!(verbose_cursor(ss))));}

static void view_controls_callback(Widget w, XtPointer info, XtPointer context)
{
  in_set_show_controls(ss, !in_show_controls(ss));
  if (in_show_controls(ss))
    show_all_controls();
  else hide_all_controls(); 
}
static void view_region_callback_1(Widget w, XtPointer info, XtPointer context) {view_region_callback(w, info, context);}
static void view_orientation_callback_1(Widget w, XtPointer info, XtPointer context) {view_orientation_callback(w, info, context);}
static void view_color_callback_1(Widget w, XtPointer info, XtPointer context) {view_color_callback(w, info, context);}
static void view_files_callback(Widget w, XtPointer info, XtPointer context) {start_view_files_dialog(true, false);}

static void view_x_axis_seconds_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_IN_SECONDS);}
static void view_x_axis_clock_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_AS_CLOCK);}
static void view_x_axis_beats_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_IN_BEATS);}
static void view_x_axis_measures_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_IN_MEASURES);}
static void view_x_axis_samples_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_IN_SAMPLES);}
static void view_x_axis_percentage_callback(Widget w, XtPointer info, XtPointer context) {set_x_axis_style(X_AXIS_AS_PERCENTAGE);}

static void view_no_axes_callback(Widget w, XtPointer info, XtPointer context) {set_show_axes(SHOW_NO_AXES);}
static void view_all_axes_callback(Widget w, XtPointer info, XtPointer context) {set_show_axes(SHOW_ALL_AXES);}
static void view_just_x_axis_callback(Widget w, XtPointer info, XtPointer context) {set_show_axes(SHOW_X_AXIS);}
static void view_all_axes_unlabelled_callback(Widget w, XtPointer info, XtPointer context) {set_show_axes(SHOW_ALL_AXES_UNLABELLED);}
static void view_just_x_axis_unlabelled_callback(Widget w, XtPointer info, XtPointer context) {set_show_axes(SHOW_X_AXIS_UNLABELLED);}
static void view_bare_x_axis_callback(Widget w, XtPointer info, XtPointer context) {set_show_axes(SHOW_BARE_X_AXIS);}



/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_menu_update_1(Widget w, XtPointer info, XtPointer context) {options_menu_update();}
static void options_transform_callback(Widget w, XtPointer info, XtPointer context) {fire_up_transform_dialog(true);}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(Widget w, XtPointer info, XtPointer context) {save_options_from_menu();}
#endif
static void options_focus_right_callback(Widget w, XtPointer info, XtPointer context) {set_zoom_focus_style(ZOOM_FOCUS_RIGHT);}
static void options_focus_left_callback(Widget w, XtPointer info, XtPointer context) {set_zoom_focus_style(ZOOM_FOCUS_LEFT);}
static void options_focus_middle_callback(Widget w, XtPointer info, XtPointer context) {set_zoom_focus_style(ZOOM_FOCUS_MIDDLE);}
static void options_focus_active_callback(Widget w, XtPointer info, XtPointer context) {set_zoom_focus_style(ZOOM_FOCUS_ACTIVE);}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(Widget w, XtPointer info, XtPointer context) {save_state_from_menu();}
#endif
static void options_preferences_callback(Widget w, XtPointer info, XtPointer context) {start_preferences_dialog();}



/* -------------------------------- HELP MENU -------------------------------- */

static void help_about_snd_callback(Widget w, XtPointer info, XtPointer context) {about_snd_help();}
static void help_fft_callback(Widget w, XtPointer info, XtPointer context) {fft_help();}
#if HAVE_EXTENSION_LANGUAGE
static void help_find_callback(Widget w, XtPointer info, XtPointer context) {find_help();}
static void help_init_file_callback(Widget w, XtPointer info, XtPointer context) {init_file_help();}
#endif
static void help_undo_callback(Widget w, XtPointer info, XtPointer context) {undo_help();}
static void help_sync_callback(Widget w, XtPointer info, XtPointer context) {sync_help();}
static void help_debug_callback(Widget w, XtPointer info, XtPointer context) {debug_help();}
static void help_controls_callback(Widget w, XtPointer info, XtPointer context) {controls_help();}
static void help_env_callback(Widget w, XtPointer info, XtPointer context) {env_help();}
static void help_marks_callback(Widget w, XtPointer info, XtPointer context) {marks_help();}
static void help_mix_callback(Widget w, XtPointer info, XtPointer context) {mix_help();}
static void help_track_callback(Widget w, XtPointer info, XtPointer context) {track_help();}
static void help_sound_files_callback(Widget w, XtPointer info, XtPointer context) {sound_files_help();}
static void help_recording_callback(Widget w, XtPointer info, XtPointer context) {recording_help();}
static void help_keys_callback(Widget w, XtPointer info, XtPointer context) {key_binding_help();}
static void help_play_callback(Widget w, XtPointer info, XtPointer context) {play_help();}
static void help_filter_callback(Widget w, XtPointer info, XtPointer context) {filter_help();}
static void help_save_callback(Widget w, XtPointer info, XtPointer context) {save_help();}
static void help_reverb_callback(Widget w, XtPointer info, XtPointer context) {reverb_help();}
static void help_resample_callback(Widget w, XtPointer info, XtPointer context) {resample_help();}
static void help_insert_callback(Widget w, XtPointer info, XtPointer context) {insert_help();}
static void help_delete_callback(Widget w, XtPointer info, XtPointer context) {delete_help();}
static void help_region_callback(Widget w, XtPointer info, XtPointer context) {region_help();}
static void help_selection_callback(Widget w, XtPointer info, XtPointer context) {selection_help();}
static void help_colors_callback(Widget w, XtPointer info, XtPointer context) {colors_help();}

void check_menu_labels(int key, int state, bool extended)
{
  /* user has redefined key, so erase old key binding info from the menu label */
  if (extended)
    {
      if (state == snd_ControlMask)
	{
	  if (key == snd_K_f) set_label(file_open_menu, _("Open")); else
	  if (key == snd_K_s) set_label(file_save_menu, _("Save")); else
	  if (key == snd_K_q) set_label(file_mix_menu, _("Mix")); else
	  if (key == snd_K_i) set_label(file_insert_menu, _("Insert")); else
	  if (key == snd_K_u) set_label(edit_undo_menu, _("Undo")); else
	  if (key == snd_K_r) set_label(edit_redo_menu, _("Redo"));
	}
      else
	{
	  if (key == snd_K_k) set_label(file_close_menu, _("Close")); else
	  if (key == snd_K_i) set_label(edit_paste_menu, _("Insert Selection")); else	  
	  if (key == snd_K_q) set_label(edit_mix_menu, _("Mix Selection")); else	  
	  if (key == snd_K_p) set_label(edit_play_menu, _("Play Selection")); else	  
	  if (key == snd_K_w) set_label(edit_save_as_menu, _("Save Selection"));
	}
    }
  else 
    {
      if ((key == snd_K_s) && (state == snd_ControlMask))
	set_label(edit_find_menu, _("Find"));
    }
}

/* -------------------------------- MAIN MENU -------------------------------- */

static void menu_drag_watcher(Widget w, const char *str, Position x, Position y, drag_style_t dtype, void *data)
{
  char *new_title;
  switch (dtype)
    {
    case DRAG_ENTER:
      new_title = mus_format("%s: drop to open file", ss->startup_title);
      XtVaSetValues(MAIN_SHELL(ss), XmNtitle, (char*)new_title, NULL);
      XmChangeColor(w, ss->sgx->pushed_button_color);
      FREE(new_title);
      break;
    case DRAG_LEAVE:
      reflect_file_change_in_title();
      XmChangeColor(w, ss->sgx->highlight_color);
      break;
    default:
      break;
    }
}

static void menu_drop_watcher(Widget w, const char *str, Position x, Position y, void *data)
{
  snd_info *sp = NULL;
  ss->open_requestor = FROM_DRAG_AND_DROP;
  sp = snd_open_file(str, FILE_READ_WRITE);
  if (sp) select_channel(sp, 0);
}

void add_menu_drop(void)
{
  add_drag_and_drop(main_menu, menu_drop_watcher, menu_drag_watcher, NULL);
}

Widget add_menu(void)
{
  static Arg main_args[12];
  static Arg in_args[12];
  static Arg high_args[12];
  Arg sep_args[12];
  int in_n = 0, n, high_n = 0, main_n = 0, start_high_n, k, j;

  ss->sgx->mw = (Widget *)calloc(NUM_MENU_WIDGETS, sizeof(Widget));

  XtSetArg(main_args[main_n], XmNbackground, ss->sgx->basic_color); main_n++;
  XtSetArg(high_args[high_n], XmNbackground, ss->sgx->highlight_color); high_n++;
  XtSetArg(in_args[in_n], XmNbackground, ss->sgx->basic_color); in_n++;

  start_high_n = high_n;
  XtSetArg(in_args[in_n], XmNsensitive, false); in_n++;
  
  n = high_n;
  XtSetArg(high_args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(high_args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(high_args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(high_args[n], XmNrightAttachment, XmATTACH_FORM); n++;

#ifdef SND_AS_WIDGET
  main_menu = XtCreateWidget("mb", xmRowColumnWidgetClass, MAIN_PANE(ss), high_args, n);
#else
  main_menu = XmCreateMenuBar(MAIN_PANE(ss), "menuBar", high_args, n);
#endif

  /* FILE MENU */
  XtSetArg(main_args[main_n], XmNuserData, 0);
  file_menu = XmCreatePulldownMenu(main_menu, "File", main_args, main_n + 1);
  
  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, file_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'F'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 0); high_n++;
  file_cascade_menu = XtCreateManagedWidget(_("File"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  file_open_menu = XtCreateManagedWidget(_("Open"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_open_menu, XmNactivateCallback, file_open_callback, NULL);
  XtVaSetValues(file_open_menu, XmNmnemonic, 'O', NULL);

  file_close_menu = XtCreateManagedWidget(_("Close"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_close_menu, XmNactivateCallback, file_close_callback, NULL);
  XtVaSetValues(file_close_menu, XmNmnemonic, 'C', NULL);
  
  file_save_menu = XtCreateManagedWidget(_("Save"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_save_menu, XmNactivateCallback, file_save_callback, NULL);
  XtVaSetValues(file_save_menu, XmNmnemonic, 'S', NULL);
  
  file_save_as_menu = XtCreateManagedWidget(_("Save as"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_save_as_menu, XmNactivateCallback, file_save_as_callback, NULL);
  XtVaSetValues(file_save_as_menu, XmNmnemonic, 'a', NULL);
  
  file_revert_menu = XtCreateManagedWidget(_("Revert"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_revert_menu, XmNactivateCallback, file_revert_callback, NULL);
  XtVaSetValues(file_revert_menu, XmNmnemonic, 'R', NULL);
  
  file_mix_menu = XtCreateManagedWidget(_("Mix"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_mix_menu, XmNactivateCallback, file_mix_callback_1, NULL);
  XtVaSetValues(file_mix_menu, XmNmnemonic, 'M', NULL);

  file_insert_menu = XtCreateManagedWidget(_("Insert"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_insert_menu, XmNactivateCallback, file_insert_callback_1, NULL);
  XtVaSetValues(file_insert_menu, XmNmnemonic, 'I', NULL);

  file_update_menu = XtCreateManagedWidget(_("Update"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_update_menu, XmNactivateCallback, file_update_callback, NULL);
  XtVaSetValues(file_update_menu, XmNmnemonic, 'U', NULL);

  file_new_menu = XtCreateManagedWidget(_("New"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_new_menu, XmNactivateCallback, file_new_callback, NULL);
  XtVaSetValues(file_new_menu, XmNmnemonic, 'N', NULL);

  file_record_menu = XtCreateManagedWidget(_("Record"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_record_menu, XmNactivateCallback, file_record_callback, NULL);

  file_view_menu = XtCreateManagedWidget(_("View"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_view_menu, XmNactivateCallback, file_view_callback, NULL);
  XtVaSetValues(file_view_menu, XmNmnemonic, 'V', NULL);

  file_print_menu = XtCreateManagedWidget(_("Print"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_print_menu, XmNactivateCallback, file_print_callback_1, NULL);
  XtVaSetValues(file_print_menu, XmNmnemonic, 'P', NULL);

  j = 0;
  XtSetArg(sep_args[j], XmNbackground, ss->sgx->basic_color); j++;
  XtSetArg(sep_args[j], XmNseparatorType, XmSHADOW_ETCHED_IN); j++;
  file_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, file_menu, sep_args, j);

  file_exit_menu = XtCreateManagedWidget(_("Exit"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_exit_menu, XmNactivateCallback, file_exit_callback, NULL);
  XtVaSetValues(file_exit_menu, XmNmnemonic, 'E', NULL);


  /* EDIT MENU */
  XtSetArg(main_args[main_n], XmNuserData, 1);
  edit_menu = XmCreatePulldownMenu(main_menu, "Edit", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, edit_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'E'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 1); high_n++;
  edit_cascade_menu = XtCreateManagedWidget(_("Edit"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);
  
  edit_undo_menu = XtCreateManagedWidget(_("Undo"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_undo_menu, XmNactivateCallback, edit_undo_callback, NULL);
  XtVaSetValues(edit_undo_menu, XmNmnemonic, 'U', NULL);

  edit_redo_menu = XtCreateManagedWidget(_("Redo"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_redo_menu, XmNactivateCallback, edit_redo_callback, NULL);
  XtVaSetValues(edit_redo_menu, XmNmnemonic, 'R', NULL);

#if HAVE_EXTENSION_LANGUAGE
  edit_find_menu = XtCreateManagedWidget(_("Find"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_find_menu, XmNactivateCallback, edit_find_callback_1, NULL);
  XtVaSetValues(edit_find_menu, XmNmnemonic, 'F', NULL);
#endif

  edit_select_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, edit_menu, sep_args, j);

  edit_cut_menu = XtCreateManagedWidget(_("Delete Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_cut_menu, XmNactivateCallback, edit_cut_callback, NULL);
  XtVaSetValues(edit_cut_menu, XmNmnemonic, 'C', NULL);

  edit_paste_menu = XtCreateManagedWidget(_("Insert Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_paste_menu, XmNactivateCallback, edit_paste_callback, NULL);
  XtVaSetValues(edit_paste_menu, XmNmnemonic, 'P', NULL);

  edit_mix_menu = XtCreateManagedWidget(_("Mix Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_mix_menu, XmNactivateCallback, edit_mix_callback, NULL);
  XtVaSetValues(edit_mix_menu, XmNmnemonic, 'M', NULL);

  edit_play_menu = XtCreateManagedWidget(_("Play Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_play_menu, XmNactivateCallback, edit_play_callback, NULL);
  XtVaSetValues(edit_play_menu, XmNmnemonic, 'P', NULL);

  edit_save_as_menu = XtCreateManagedWidget(_("Save Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_save_as_menu, XmNactivateCallback, edit_save_as_callback, NULL);
  XtVaSetValues(edit_save_as_menu, XmNmnemonic, 'S', NULL);

  edit_select_all_menu = XtCreateManagedWidget(_("Select all"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_select_all_menu, XmNactivateCallback, edit_select_all_callback, NULL);

  edit_edit_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, edit_menu, sep_args, j);

  edit_env_menu = XtCreateManagedWidget(_("Edit Envelope"), xmPushButtonWidgetClass, edit_menu, main_args, main_n);
  XtAddCallback(edit_env_menu, XmNactivateCallback, edit_envelope_callback, NULL);
  XtVaSetValues(edit_env_menu, XmNmnemonic, 'E', NULL);

  edit_header_menu = XtCreateManagedWidget(_("Edit Header"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_header_menu, XmNactivateCallback, edit_header_callback_1, NULL);
  XtVaSetValues(edit_header_menu, XmNmnemonic, 'H', NULL);


  /* VIEW MENU */
  XtSetArg(main_args[main_n], XmNuserData, 2);
  view_menu = XmCreatePulldownMenu(main_menu, "View", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, view_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'V'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 2); high_n++;
  view_cascade_menu = XtCreateManagedWidget(_("View"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  view_controls_menu = XtCreateManagedWidget(_("Show controls"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_controls_menu, XmNactivateCallback, view_controls_callback, NULL);
  XtVaSetValues(view_controls_menu, XmNmnemonic, 'S', NULL);

#if HAVE_EXTENSION_LANGUAGE
  view_listener_menu = XtCreateManagedWidget(_("Open listener"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_listener_menu, XmNactivateCallback, view_listener_callback, NULL);
  XtVaSetValues(view_listener_menu, XmNmnemonic, 'L', NULL);
#endif

  view_mix_dialog_menu = XtCreateManagedWidget(_("Mixes"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_mix_dialog_menu, XmNactivateCallback, view_mix_dialog_callback, NULL);

  view_track_dialog_menu = XtCreateManagedWidget(_("Tracks"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_track_dialog_menu, XmNactivateCallback, view_track_dialog_callback, NULL);

  view_region_menu = XtCreateManagedWidget(_("Regions"), xmPushButtonWidgetClass, view_menu, in_args, in_n);
  XtAddCallback(view_region_menu, XmNactivateCallback, view_region_callback_1, NULL);
  XtVaSetValues(view_region_menu, XmNmnemonic, 'R', NULL);

  view_files_menu = XtCreateManagedWidget(_("Files"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_files_menu, XmNactivateCallback, view_files_callback, NULL);
  XtVaSetValues(view_files_menu, XmNmnemonic, 'F', NULL);

  view_color_menu = XtCreateManagedWidget(_("Color"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_color_menu, XmNactivateCallback, view_color_callback_1, NULL);

  view_orientation_menu = XtCreateManagedWidget(_("Orientation"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_orientation_menu, XmNactivateCallback, view_orientation_callback_1, NULL);
  XtVaSetValues(view_orientation_menu, XmNmnemonic, 'O', NULL);

  view_sep2_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, view_menu, sep_args, j);

  view_graph_style_menu = XmCreatePulldownMenu(view_menu, "graph-style", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_graph_style_menu); k++;
  view_graph_style_cascade_menu = XtCreateManagedWidget(_("Graph style"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_lines_menu = XtCreateManagedWidget(_("lines"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_lines_menu, XmNactivateCallback, view_lines_callback, NULL); 
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(view_lines_menu, false);

  view_dots_menu = XtCreateManagedWidget(_("dots"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_dots_menu, XmNactivateCallback, view_dots_callback, NULL);  
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(view_dots_menu, false);

  view_filled_menu = XtCreateManagedWidget(_("filled"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_filled_menu, XmNactivateCallback, view_filled_callback, NULL);  
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(view_filled_menu, false);

  view_dots_and_lines_menu = XtCreateManagedWidget(_("dots and lines"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_dots_and_lines_menu, XmNactivateCallback, view_dots_and_lines_callback, NULL);  
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(view_dots_and_lines_menu, false);

  view_lollipops_menu = XtCreateManagedWidget(_("lollipops"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_lollipops_menu, XmNactivateCallback, view_lollipops_callback, NULL);  
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(view_lollipops_menu, false);

  view_cursor_menu = XtCreateManagedWidget(_("Verbose cursor"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_cursor_menu, XmNactivateCallback, view_cursor_callback, NULL);

  view_combine_menu = XmCreatePulldownMenu(view_menu, "combine", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_combine_menu); k++;
  view_combine_cascade_menu = XtCreateManagedWidget(_("Channel style"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_combine_separate_menu = XtCreateManagedWidget(_("separate"), xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_separate_menu, XmNactivateCallback, view_separate_callback, NULL); 
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(view_combine_separate_menu, false);

  view_combine_combined_menu = XtCreateManagedWidget(_("combined"), xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_combined_menu, XmNactivateCallback, view_combined_callback, NULL);  
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(view_combine_combined_menu, false);

  view_combine_superimposed_menu = XtCreateManagedWidget(_("superimposed"), xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_superimposed_menu, XmNactivateCallback, view_superimposed_callback, NULL);  
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(view_combine_superimposed_menu, false);

  view_equalize_panes_menu = XtCreateManagedWidget(_("Equalize Panes"), xmPushButtonWidgetClass, view_menu, in_args, in_n);
  XtAddCallback(view_equalize_panes_menu, XmNactivateCallback, view_equalize_panes_callback, NULL);
  XtVaSetValues(view_equalize_panes_menu, XmNmnemonic, 'N', NULL);

  view_zero_menu = XtCreateManagedWidget(_("Show Y = 0"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_zero_menu, XmNactivateCallback, view_zero_callback, NULL);
  XtVaSetValues(view_zero_menu, XmNmnemonic, 'y', NULL);

  view_x_axis_menu = XmCreatePulldownMenu(view_menu, "xaxis", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_x_axis_menu); k++;
  view_x_axis_cascade_menu = XtCreateManagedWidget(_("X axis units"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_x_axis_seconds_menu = XtCreateManagedWidget(_("seconds"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_seconds_menu, XmNactivateCallback, view_x_axis_seconds_callback, NULL);  
  set_sensitive(view_x_axis_seconds_menu, false);

  view_x_axis_samples_menu = XtCreateManagedWidget(_("samples"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_samples_menu, XmNactivateCallback, view_x_axis_samples_callback, NULL);  

  view_x_axis_clock_menu = XtCreateManagedWidget(_("clock"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_clock_menu, XmNactivateCallback, view_x_axis_clock_callback, NULL);  

  view_x_axis_percentage_menu = XtCreateManagedWidget(_("percentage"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_percentage_menu, XmNactivateCallback, view_x_axis_percentage_callback, NULL);  

  view_x_axis_beats_menu = XtCreateManagedWidget(_("beats"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_beats_menu, XmNactivateCallback, view_x_axis_beats_callback, NULL);  

  view_x_axis_measures_menu = XtCreateManagedWidget(_("measures"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_measures_menu, XmNactivateCallback, view_x_axis_measures_callback, NULL);  


  view_axes_menu = XmCreatePulldownMenu(view_menu, "axes", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_axes_menu); k++;
  view_axes_cascade_menu = XtCreateManagedWidget(_("Axes"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_no_axes_menu = XtCreateManagedWidget(_("no axes"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_no_axes_menu, XmNactivateCallback, view_no_axes_callback, NULL);  
  if (show_axes(ss) == SHOW_NO_AXES) set_sensitive(view_no_axes_menu, false); /* false because it is already chosen */

  view_all_axes_menu = XtCreateManagedWidget(_("both axes"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_all_axes_menu, XmNactivateCallback, view_all_axes_callback, NULL);  
  if (show_axes(ss) == SHOW_ALL_AXES) set_sensitive(view_all_axes_menu, false);

  view_just_x_axis_menu = XtCreateManagedWidget(_("just x axis"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_just_x_axis_menu, XmNactivateCallback, view_just_x_axis_callback, NULL);  
  if (show_axes(ss) == SHOW_X_AXIS) set_sensitive(view_just_x_axis_menu, false);

  view_all_axes_unlabelled_menu = XtCreateManagedWidget(_("both axes, no labels"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_all_axes_unlabelled_menu, XmNactivateCallback, view_all_axes_unlabelled_callback, NULL);  
  if (show_axes(ss) == SHOW_ALL_AXES_UNLABELLED) set_sensitive(view_all_axes_unlabelled_menu, false);

  view_just_x_axis_unlabelled_menu = XtCreateManagedWidget(_("just x axis, no label"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_just_x_axis_unlabelled_menu, XmNactivateCallback, view_just_x_axis_unlabelled_callback, NULL);  
  if (show_axes(ss) == SHOW_X_AXIS_UNLABELLED) set_sensitive(view_just_x_axis_unlabelled_menu, false);

  view_bare_x_axis_menu = XtCreateManagedWidget(_("bare x axis"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_bare_x_axis_menu, XmNactivateCallback, view_bare_x_axis_callback, NULL);  
  if (show_axes(ss) == SHOW_BARE_X_AXIS) set_sensitive(view_bare_x_axis_menu, false);


  /* OPTIONS MENU */
  XtSetArg(main_args[main_n], XmNuserData, 3);
  options_menu = XmCreatePulldownMenu(main_menu, "Option", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, options_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'O'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 3); high_n++;
  options_cascade_menu = XtCreateManagedWidget(_("Options"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  options_transform_menu = XtCreateManagedWidget(_("Transform Options"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_transform_menu, XmNactivateCallback, options_transform_callback, NULL);
  XtVaSetValues(options_transform_menu, XmNmnemonic, 't', NULL);


  options_focus_style_menu = XmCreatePulldownMenu(options_menu, "focusstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, options_focus_style_menu); k++;
  options_focus_cascade_menu = XtCreateManagedWidget(_("Zoom focus"), xmCascadeButtonWidgetClass, options_menu, main_args, k);

  options_focus_left_menu = XtCreateManagedWidget(_("window left edge"), xmPushButtonWidgetClass, options_focus_style_menu, main_args, main_n);
  XtAddCallback(options_focus_left_menu, XmNactivateCallback, options_focus_left_callback, NULL);  

  options_focus_right_menu = XtCreateManagedWidget(_("window right edge"), xmPushButtonWidgetClass, options_focus_style_menu, main_args, main_n);
  XtAddCallback(options_focus_right_menu, XmNactivateCallback, options_focus_right_callback, NULL);  

  options_focus_middle_menu = XtCreateManagedWidget(_("window midpoint"), xmPushButtonWidgetClass, options_focus_style_menu, main_args, main_n);
  XtAddCallback(options_focus_middle_menu, XmNactivateCallback, options_focus_middle_callback, NULL);  

  options_focus_active_menu = XtCreateManagedWidget(_("cursor or selection"), xmPushButtonWidgetClass, options_focus_style_menu, main_args, main_n);
  XtAddCallback(options_focus_active_menu, XmNactivateCallback, options_focus_active_callback, NULL);  

#if HAVE_EXTENSION_LANGUAGE
  options_save_menu = XtCreateManagedWidget(_("Save options"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_save_menu, XmNactivateCallback, options_save_callback, NULL);
  XtVaSetValues(options_save_menu, XmNmnemonic, 'a', NULL);

  options_save_state_menu = XtCreateManagedWidget(_("Save session"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_save_state_menu, XmNactivateCallback, options_save_state_callback, NULL);
#endif

  options_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, options_menu, sep_args, j);

  options_preferences_menu = XtCreateManagedWidget(_("Preferences"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_preferences_menu, XmNactivateCallback, options_preferences_callback, NULL);


  /* HELP MENU */
  XtSetArg(main_args[main_n], XmNuserData, 4);
  help_menu = XmCreatePulldownMenu(main_menu, "Help", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, help_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'H'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, 4); high_n++;
  help_cascade_menu = XtCreateManagedWidget(_("Help"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  help_about_snd_menu = XtCreateManagedWidget(_("About Snd"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_about_snd_menu, XmNactivateCallback, help_about_snd_callback, NULL);
  XtVaSetValues(help_about_snd_menu, XmNmnemonic, 'O', NULL);

#if HAVE_EXTENSION_LANGUAGE
  help_init_file_menu = XtCreateManagedWidget(_("Customization"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_init_file_menu, XmNactivateCallback, help_init_file_callback, NULL);
#endif

  help_controls_menu = XtCreateManagedWidget(_("Control Panel"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_controls_menu, XmNactivateCallback, help_controls_callback, NULL);

  help_keys_menu = XtCreateManagedWidget(_("Key bindings"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_keys_menu, XmNactivateCallback, help_keys_callback, NULL);

  help_recording_menu = XtCreateManagedWidget(_("Record"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_recording_menu, XmNactivateCallback, help_recording_callback, NULL);

  help_play_menu = XtCreateManagedWidget(_("Play"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_play_menu, XmNactivateCallback, help_play_callback, NULL);

  help_save_menu = XtCreateManagedWidget(_("Save"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_save_menu, XmNactivateCallback, help_save_callback, NULL);

  help_mix_menu = XtCreateManagedWidget(_("Mix"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_mix_menu, XmNactivateCallback, help_mix_callback, NULL);

  help_track_menu = XtCreateManagedWidget(_("Track"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_track_menu, XmNactivateCallback, help_track_callback, NULL);

  help_resample_menu = XtCreateManagedWidget(_("Resample"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_resample_menu, XmNactivateCallback, help_resample_callback, NULL);

  help_fft_menu = XtCreateManagedWidget(_("FFT"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_fft_menu, XmNactivateCallback, help_fft_callback, NULL);

  help_filter_menu = XtCreateManagedWidget(_("Filter"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_filter_menu, XmNactivateCallback, help_filter_callback, NULL);

  help_reverb_menu = XtCreateManagedWidget(_("Reverb"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_reverb_menu, XmNactivateCallback, help_reverb_callback, NULL);

  help_env_menu = XtCreateManagedWidget(_("Envelope"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_env_menu, XmNactivateCallback, help_env_callback, NULL);

  help_marks_menu = XtCreateManagedWidget(_("Mark"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_marks_menu, XmNactivateCallback, help_marks_callback, NULL);

  help_insert_menu = XtCreateManagedWidget(_("Insert"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_insert_menu, XmNactivateCallback, help_insert_callback, NULL);

  help_delete_menu = XtCreateManagedWidget(_("Delete"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_delete_menu, XmNactivateCallback, help_delete_callback, NULL);

  help_undo_menu = XtCreateManagedWidget(_("Undo and redo"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_undo_menu, XmNactivateCallback, help_undo_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  help_find_menu = XtCreateManagedWidget(_("Search"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_find_menu, XmNactivateCallback, help_find_callback, NULL);
#endif

  help_sync_menu = XtCreateManagedWidget(_("Sync and Unite"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_sync_menu, XmNactivateCallback, help_sync_callback, NULL);

  help_sound_files_menu = XtCreateManagedWidget(_("Headers and Data"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_sound_files_menu, XmNactivateCallback, help_sound_files_callback, NULL);

  help_debug_menu = XtCreateManagedWidget(_("Debugging"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_debug_menu, XmNactivateCallback, help_debug_callback, NULL);

  help_region_menu = XtCreateManagedWidget(_("Regions"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_region_menu, XmNactivateCallback, help_region_callback, NULL);

  help_selection_menu = XtCreateManagedWidget(_("Selections"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_selection_menu, XmNactivateCallback, help_selection_callback, NULL);

  help_colors_menu = XtCreateManagedWidget(_("Colors"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_colors_menu, XmNactivateCallback, help_colors_callback, NULL);

  XtVaSetValues(main_menu, XmNmenuHelpWidget, help_cascade_menu, NULL);

  XtAddCallback(file_cascade_menu, XmNcascadingCallback, file_menu_update_1, NULL);
  XtAddCallback(edit_cascade_menu, XmNcascadingCallback, edit_menu_update_1, NULL);
  XtAddCallback(view_cascade_menu, XmNcascadingCallback, view_menu_update_1, NULL);
  XtAddCallback(options_cascade_menu, XmNcascadingCallback, options_menu_update_1, NULL);

#ifndef SND_AS_WIDGET
  XtManageChild(main_menu);
#endif
  return(main_menu);
}


/* -------------------------------- POPUP MENU -------------------------------- */

static Widget popup_menu = NULL;

static void popup_menu_update_1(Widget w, XtPointer info, XtPointer context) {popup_menu_update();}

static bool stopping = false;

static void popup_play_callback(Widget w, XtPointer info, XtPointer context) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (stopping)
    {
      stop_playing_all_sounds(PLAY_BUTTON_UNSET);
      stopping = false;
      set_button_label(w, _("Play"));
      if (sp) set_play_button(sp, false);
    }
  else
    {
      if (sp)
	{
	  play_sound(sp, 0, NO_END_SPECIFIED);
	  stopping = true;
	  set_button_label(w, _("Stop playing"));
	}
    }
}

void reflect_play_stop_in_popup_menu(void)
{
  stopping = false;
  if (popup_menu)
    set_button_label(popup_play_menu, _("Play"));
}

static void popup_save_callback(Widget w, XtPointer info, XtPointer context) {save_edits_with_prompt(any_selected_sound());}
static void popup_undo_callback(Widget w, XtPointer info, XtPointer context) {undo_edit_with_sync(current_channel(), 1);}
static void popup_redo_callback(Widget w, XtPointer info, XtPointer context) {redo_edit_with_sync(current_channel(), 1);}
static void popup_equalize_panes_callback(Widget w, XtPointer info, XtPointer context) {equalize_all_panes();}
static void popup_info_callback(Widget w, XtPointer info, XtPointer context) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) display_info(sp);
}

static void popup_apply_callback(Widget w, XtPointer info, XtPointer context) {menu_apply_controls(any_selected_sound());}
static void popup_reset_callback(Widget w, XtPointer info, XtPointer context) {menu_reset_controls(any_selected_sound());}

void post_popup(XButtonPressedEvent *event)
{
  XmMenuPosition(popup_menu, event);
  XtManageChild(popup_menu);
}

void create_popup_menu(void)
{
  /* make it a child of the main window */
  if (!popup_menu)
    {
      Widget mainp, sep, pop;
      Arg args[20];
      int n;

      ss->sgx->pw = (Widget *)calloc(NUM_POPUP_WIDGETS, sizeof(Widget));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      mainp = MAIN_PANE(ss);
      XtSetArg(args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC_RECURSIVE); n++;
      XtSetArg(args[n], XmNuserData, 5);
      popup_menu = XmCreatePopupMenu(mainp, "popup-menu", args, n + 1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;

      pop = XtCreateManagedWidget("Snd", xmLabelWidgetClass, popup_menu, args, n);
      sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, popup_menu, args, n);

      popup_play_menu = XtCreateManagedWidget(_("Play"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_play_menu, XmNactivateCallback, popup_play_callback, NULL);
      XtVaSetValues(popup_play_menu, XmNsensitive, false, NULL);

      popup_undo_menu = XtCreateManagedWidget(_("Undo"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_undo_menu, XmNsensitive, false, NULL);
      XtAddCallback(popup_undo_menu, XmNactivateCallback, popup_undo_callback, NULL);

      popup_redo_menu = XtCreateManagedWidget(_("Redo"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_redo_menu, XmNsensitive, false, NULL);
      XtAddCallback(popup_redo_menu, XmNactivateCallback, popup_redo_callback, NULL);

      popup_save_menu = XtCreateManagedWidget(_("Save"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_save_menu, XmNactivateCallback, popup_save_callback, NULL);
      XtVaSetValues(popup_save_menu, XmNsensitive, false, NULL);

      popup_equalize_panes_menu = XtCreateManagedWidget(_("Equalize Panes"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_equalize_panes_menu, XmNsensitive, false, NULL);
      XtAddCallback(popup_equalize_panes_menu, XmNactivateCallback, popup_equalize_panes_callback, NULL);

      popup_info_menu = XtCreateManagedWidget(_("Info"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_info_menu, XmNsensitive, false, NULL);
      XtAddCallback(popup_info_menu, XmNactivateCallback, popup_info_callback, NULL);

      popup_apply_menu = XtCreateManagedWidget(_("Apply controls"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_apply_menu, XmNsensitive, true, NULL);
      XtAddCallback(popup_apply_menu, XmNactivateCallback, popup_apply_callback, NULL);

      popup_reset_menu = XtCreateManagedWidget(_("Reset controls"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_reset_menu, XmNsensitive, true, NULL);
      XtAddCallback(popup_reset_menu, XmNactivateCallback, popup_reset_callback, NULL);

      XtAddCallback(popup_menu, XmNmapCallback, popup_menu_update_1, NULL);
    }
}


/* ---------------- ext lang tie-ins ---------------- */

#define INVALID_MENU -1
#define CALL_INDEX(Data) (Data >> 16)
#define MENU_INDEX(Data) (Data & 0xffff)
#define PACK_MENU_DATA(Slot, Menu) ((Slot << 16) | (Menu))

static void SND_callback(Widget w, XtPointer info, XtPointer context) 
{
  int callb;
  XtVaGetValues(w, XmNuserData, &callb, NULL);
  g_snd_callback(CALL_INDEX(callb)); /* menu option activate callback */
  if (callb < 0) fprintf(stderr,"%s callback: %d\n", XtName(w), callb); /* this exists solely to prevent an inexplicable segfault in dual x86_64 FC4 systems?? */
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
  Widget w;
  CompositeWidget cw;
  int menu;
  if (which_menu == 5) return(popup_menu); /* special case -- not in main menuBar, presumably */
  w = main_menu;
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
	      Widget subw;
	      XtVaGetValues(w, XmNsubMenuId, &subw, NULL);
	      return(subw);
	    }
	}
    }
  return(NULL);
}

static bool or_over_children(Widget w, bool (*func)(Widget uw, const char *ustr), const char *str)
{
  if (w)
    {
      if ((*func)(w, str)) return(true);
      if (XtIsComposite(w))
	{
	  unsigned int i;
	  CompositeWidget cw = (CompositeWidget)w;
	  for (i = 0; i < cw->composite.num_children; i++)
	    if (or_over_children(cw->composite.children[i], func, str))
	      return(true);
	}
    }
  return(false);
}

static bool clobber_menu(Widget w, const char *name)
{
  char *wname;
  wname = XtName(w);
  if ((wname) && 
      (name) && 
      (strcmp(name, wname) == 0) && 
      (XtIsManaged(w)))
    {
      int slot;
      XtVaGetValues(w, XmNuserData, &slot, NULL);
      unprotect_callback(CALL_INDEX(slot));
      XtUnmanageChild(w);
      return(true);
    }
  return(false);
}

int g_remove_from_menu(int which_menu, const char *label)
{
  Widget top_menu;
  top_menu = menu_widget(which_menu);
  if (top_menu)
    {
      or_over_children(top_menu, clobber_menu, label);
      return(0);
    }
  return(INVALID_MENU);
}

static void set_widget_name(Widget w, const char *new_name)
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
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(slot, new_menu)); n++;
  m = XmCreatePulldownMenu(main_menu, label, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNsubMenuId, m); n++;
  XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(slot, new_menu)); n++;
  cas = XtCreateManagedWidget(label, xmCascadeButtonWidgetClass, main_menu, args, n);
  if (slot >= 0) XtAddCallback(cas, XmNcascadingCallback, GHC_callback, NULL);

  if (auto_resize(ss)) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL);
  return(new_menu);
}

Widget g_add_to_menu(int which_menu, const char *label, int callb, int position)
{
  Widget m, menw;
  Arg args[12];
  int n = 0;
  unsigned int i;
  menw = menu_widget(which_menu);
  if (menw == NULL) return(NULL);
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
	      return(m);
	    }
	}
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
      XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(callb, which_menu)); n++;
      m = XtCreateManagedWidget(label, xmPushButtonWidgetClass, menw, args, n);
      XtAddCallback(m, XmNactivateCallback, SND_callback, NULL);
    }
  else
    {
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
      m = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, menw, args, n);
    }
  return(m);
}


static XEN g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets "): a list of top level menu widgets: ((0)main (1)file (2)edit (3)view (4)options (5)help (6)popup)"
  return(XEN_CONS(XEN_WRAP_WIDGET(main_menu),
	  XEN_CONS(XEN_WRAP_WIDGET(file_cascade_menu),
           XEN_CONS(XEN_WRAP_WIDGET(edit_cascade_menu),
            XEN_CONS(XEN_WRAP_WIDGET(view_cascade_menu),
             XEN_CONS(XEN_WRAP_WIDGET(options_cascade_menu),
              XEN_CONS(XEN_WRAP_WIDGET(help_cascade_menu),
	       XEN_CONS(XEN_WRAP_WIDGET(popup_menu),
	        XEN_EMPTY_LIST))))))));
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

/* Motif bug: the button backgrounds remain in the original highlight color? but the widget (if it is one) is not the child of any obvious widget
 */
