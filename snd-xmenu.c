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
          h_click_for_help_menu, h_about_snd_menu, h_fft_menu, h_find_menu, h_undo_menu, h_sync_menu, h_speed_menu,
          h_expand_menu, h_contrast_menu, h_reverb_menu, h_env_menu, h_marks_menu, h_sound_files_menu, h_init_file_menu,
          h_mix_menu, h_recording_menu, h_clm_menu, h_news_menu,
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
          v_files_menu, v_mix_panel_menu,
          v_x_axis_menu, v_x_axis_cascade_menu,
            v_x_axis_seconds_menu, v_x_axis_samples_menu, v_x_axis_percentage_menu, v_x_axis_beats_menu,
          v_error_history_menu,
          v_sep2_menu
};

#define NUM_MENU_WIDGETS 99
static Widget mw[NUM_MENU_WIDGETS];

enum {W_pop_menu, W_pop_sep, W_pop_play, W_pop_undo, W_pop_redo, W_pop_save, W_pop_equalize_panes, W_pop_info};
#define NUM_POPUP_CHILDREN 8
static Widget popup_menu = NULL;
static Widget popup_children[NUM_POPUP_CHILDREN];

int popup_menu_exists(void) {return(popup_menu != NULL);}

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

static XEN menu_hook;
static int call_menu_hook(char *name, char *option)
{
  XEN res = XEN_TRUE;
  if ((name) && (XEN_HOOKED(menu_hook)))
    res = g_c_run_and_hook(menu_hook, 
			   XEN_LIST_2(C_TO_XEN_STRING(name), 
				      C_TO_XEN_STRING(option)),
			   S_menu_hook);
  return(XEN_TRUE_P(res));
}

#if HAVE_GUILE
  #define IF_MENU_HOOK(NAME, OPTION) if (call_menu_hook(NAME, OPTION))
#else
 #define IF_MENU_HOOK(NAME, OPTION)
#endif


/* -------------------------------- FILE MENU -------------------------------- */

static void file_open_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Open) make_open_file_dialog((snd_state *)cD, FALSE, TRUE);
}

static void file_view_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_View) make_open_file_dialog((snd_state *)cD, TRUE, TRUE);
}

static void file_new_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_New) new_file_from_menu((snd_state *)cD);
}

static void file_record_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Record) snd_record_file((snd_state *)cD);
}

static void file_close_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Close) close_file_from_menu((snd_state *)cD);
}

static void file_save_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Save) save_file_from_menu((snd_state *)cD);
}

static void file_update_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Update) update_file_from_menu((snd_state *)cD);
}

static void file_save_as_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Save_as) make_file_save_as_dialog((snd_state *)cD);
}

static void file_revert_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Revert) revert_file_from_menu((snd_state *)cD);
}

static void file_exit_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Exit) exit_from_menu((snd_state *)cD);
}

static void file_mix_callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Mix) make_mix_file_dialog((snd_state *)cD, TRUE);
}

static void file_print_callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Print) file_print_callback(w, cD, mD);
}



/* -------------------------------- EDIT MENU -------------------------------- */


static void edit_mix_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Mix_Selection) add_selection_or_region((snd_state *)cD, 0, selected_channel(((snd_state *)cD)), "Edit: mix");
}

static void edit_envelope_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Edit_Envelope) create_envelope_editor((snd_state *)cD);
}

static void edit_cut_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Delete_Selection) delete_selection("Edit: Cut", UPDATE_DISPLAY);
}

static void edit_paste_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Insert_Selection) insert_selection_from_menu((snd_state *)cD);
}

static void edit_save_as_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Save_Selection) make_edit_save_as_dialog((snd_state *)cD);
}

static void edit_select_all_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Select_all) select_all(current_channel((snd_state *)cD));
}

static void edit_undo_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Undo) undo_edit_with_sync(current_channel((snd_state *)cD), 1);
}

static void edit_redo_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Redo) redo_edit_with_sync(current_channel((snd_state *)cD), 1);
}

static int selection_play_stop = 0;

static void edit_play_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  if (selection_play_stop)
    {
      stop_playing_all_sounds();
    }
  else
    {
      IF_MENU_HOOK(STR_Edit, STR_Play_Selection) 
	{
	  set_menu_label(edit_play_menu(), STR_Stop);
	  selection_play_stop = 1;
	  play_selection(IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "play selection", 0);
	}
    }
}

void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu(), STR_Play_Selection);
  selection_play_stop = 0;
}

static void edit_header_callback_1(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  snd_info *sp;
  IF_MENU_HOOK(STR_Edit, STR_Edit_Header) 
    {
      sp = selected_sound(ss);
      if (sp) edit_header(sp);
    }
}

#if HAVE_EXTENSION_LANGUAGE
static void edit_find_callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Find) edit_find_callback(w, cD, mD);
}
#endif


/* -------------------------------- VIEW MENU -------------------------------- */

static void view_separate_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_separate) set_channel_style((snd_state *)cD, CHANNELS_SEPARATE);
}

static void view_combined_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_combined) set_channel_style((snd_state *)cD, CHANNELS_COMBINED);
}

static void view_superimposed_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_superimposed) set_channel_style((snd_state *)cD, CHANNELS_SUPERIMPOSED);
}

static void view_equalize_panes_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Equalize_Panes) equalize_all_panes((snd_state *)cD);
}

static void view_dots_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_dots) set_graph_style((snd_state *)cD, GRAPH_DOTS);
}

static void view_lines_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_lines) set_graph_style((snd_state *)cD, GRAPH_LINES);
}

static void view_filled_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_filled) set_graph_style((snd_state *)cD, GRAPH_FILLED);
}

static void view_dots_and_lines_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_dots_and_lines) set_graph_style((snd_state *)cD, GRAPH_DOTS_AND_LINES);
}

static void view_lollipops_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_lollipops) set_graph_style((snd_state *)cD, GRAPH_LOLLIPOPS);
}

#if HAVE_EXTENSION_LANGUAGE
static void view_listener_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Show_listener) handle_listener((snd_state *)cD, (listener_height() < 5));
}
#endif

static void view_mix_panel_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Mix_Panel) make_mix_panel((snd_state *)cD);
}

static void view_error_history_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Error_History) show_snd_errors((snd_state *)cD);
}

static void view_zero_callback(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Show_Y0) set_show_y_zero(ss, (!(show_y_zero(ss))));
}

static void view_cursor_callback(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Verbose_cursor) set_verbose_cursor(ss, (!(verbose_cursor(ss))));
}

static void view_ctrls_callback(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Show_controls) 
    {
      if (ss->ctrls_height < 100) 
	show_controls(ss); 
      else hide_controls(ss); /* snd-xmain.c */
    }
}

static void view_region_callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Regions) view_region_callback(w, cD, mD);
}

static void view_orientation_callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Orientation) view_orientation_callback(w, cD, mD);
}

static void view_color_callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Color) view_color_callback(w, cD, mD);
}

static void view_files_callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Files) view_files_callback(w, cD, mD);
}

static void view_menu_update(Widget w, XtPointer cD, XtPointer mD) 
{
  /* make sure listener menu option label correctly reflects current listener state */
  set_view_listener_label((listener_height() > 10) ? STR_Hide_listener : STR_Show_listener);
}

/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_transform_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_Transform_Options) fire_up_transform_dialog((snd_state *)cD, TRUE);
}

#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_Save_options) save_options_from_menu((snd_state *)cD);
}
#endif

static void options_focus_right_callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_right) activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_RIGHT);
}

static void options_focus_left_callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_left) activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_LEFT);
}

static void options_focus_middle_callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_middle) activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_MIDDLE);
}

static void options_focus_active_callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_active) activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_ACTIVE);
}

static void options_speed_float_callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_float) activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_FLOAT);
}

static void options_speed_ratio_callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_ratio) activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_RATIO);
}

static void options_speed_semitone_callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_semitone) activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_SEMITONE);
}

static void options_x_axis_seconds_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_seconds) set_x_axis_style((snd_state *)cD, X_AXIS_IN_SECONDS);
}

static void options_x_axis_beats_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_beats) set_x_axis_style((snd_state *)cD, X_AXIS_IN_BEATS);
}

static void options_x_axis_samples_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_samples) set_x_axis_style((snd_state *)cD, X_AXIS_IN_SAMPLES);
}

static void options_x_axis_percentage_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_percentage) set_x_axis_style((snd_state *)cD, X_AXIS_AS_PERCENTAGE);
}

#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_Save_state) save_state_from_menu((snd_state *)cD);
}
#endif




/* -------------------------------- HELP MENU -------------------------------- */

static void help_context_help_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)mD;
  Widget selectedWidget = NULL;
  static Cursor cursor = 0;
  Widget parent = XtParent(w);
  snd_state *ss = (snd_state *)cD;
  Widget mainWindow;
  mainWindow = MAIN_PANE(ss);
  if (!cursor) cursor = XCreateFontCursor(XtDisplay(parent), XC_question_arrow); 
  selectedWidget = XmTrackingLocate(mainWindow, cursor, FALSE);
  if (selectedWidget)
    {
      XmAnyCallbackStruct cb;
      cb.reason = XmCR_HELP;
      cb.event = cbs->event;
      do
        {
	  if ((XtHasCallbacks(selectedWidget, XmNhelpCallback) == XtCallbackHasSome))
            {
	      XtCallCallbacks(selectedWidget, XmNhelpCallback, &cb);
	      return;
            }
	  else
	    selectedWidget = XtParent(selectedWidget);
        } while (selectedWidget != NULL);
    }
}

static void help_about_snd_callback(Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Overview) about_snd_help((snd_state *)cD);}
static void help_fft_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_FFT) fft_help((snd_state *)cD);}
static void help_find_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Find) find_help((snd_state *)cD);}
static void help_undo_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Undo_and_redo) undo_help((snd_state *)cD);}
static void help_sync_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Sync) sync_help((snd_state *)cD);}
static void help_speed_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Speed) speed_help((snd_state *)cD);}
static void help_expand_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Expand) expand_help((snd_state *)cD);}
static void help_reverb_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Reverb) reverb_help((snd_state *)cD);}
static void help_contrast_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Contrast) contrast_help((snd_state *)cD);}
static void help_env_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Envelope) env_help((snd_state *)cD);}
static void help_marks_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Marks) marks_help((snd_state *)cD);}
static void help_mix_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Mixing) mix_help((snd_state *)cD);}
static void help_sound_files_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Formats) sound_files_help((snd_state *)cD);}
static void help_init_file_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Customization) init_file_help((snd_state *)cD);}
static void help_recording_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Recording) recording_help((snd_state *)cD);}

static void help_clm_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_CLM) clm_help((snd_state *)cD);}

static void help_news_callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_News) news_help((snd_state *)cD);}


void check_menu_labels(int key, int state, int extended)
{
  if (extended)
    {
      if (state == snd_ControlMask)
	{
	  if (key == snd_K_f) set_label(mw[f_open_menu], STR_Open); else
	  if (key == snd_K_s) set_label(mw[f_save_menu], STR_Save); else
	  if (key == snd_K_q) set_label(mw[f_mix_menu], STR_Mix); else
	  if (key == snd_K_u) set_label(mw[e_undo_menu], STR_Undo); else
	  if (key == snd_K_r) set_label(mw[e_redo_menu], STR_Redo);
	}
      else
	{
	  if (key == snd_K_k) set_label(mw[f_close_menu], STR_Close); else
	  if (key == snd_K_i) set_label(mw[e_paste_menu], STR_Insert_Selection); else	  
	  if (key == snd_K_q) set_label(mw[e_mix_menu], STR_Mix_Selection); else	  
	  if (key == snd_K_p) set_label(mw[e_play_menu], STR_Play_Selection); else	  
	  if (key == snd_K_w) set_label(mw[e_save_as_menu], STR_Save_Selection);
	}
    }
  else 
    {
      if ((key == snd_K_s) && (state == snd_ControlMask))
	set_label(mw[e_find_menu], STR_Find);
    }
}

/* -------------------------------- MAIN MENU -------------------------------- */

Widget get_menubar(void) {return(mw[menu_menu]);}

Widget add_menu(snd_state *ss)
{
  static Arg main_args[12];
  static Arg in_args[12];
  static Arg high_args[12];
  Arg sep_args[12];
  int in_n, n, high_n, main_n, start_high_n, k, j, p;
  /* this mainly passes the global data pointer (ss) to all the menu-related callbacks */
  
  in_n = 0;
  main_n = 0;
  high_n = 0;
  if (!(ss->using_schemes))
    {
      XtSetArg(main_args[main_n], XmNbackground, (ss->sgx)->basic_color); main_n++;
      XtSetArg(high_args[high_n], XmNbackground, (ss->sgx)->highlight_color); high_n++;
      XtSetArg(in_args[in_n], XmNbackground, (ss->sgx)->basic_color); in_n++;
    }
  start_high_n = high_n;
  XtSetArg(in_args[in_n], XmNsensitive, FALSE); in_n++;
  
  n = high_n;
  XtSetArg(high_args[n], XmNuserData, ss); n++; /* used in snd-xdrop.c by drop site to get main state data from whatever widget got the drop */
  p = n;
  XtSetArg(high_args[p], XmNtopAttachment, XmATTACH_FORM); p++;
  XtSetArg(high_args[p], XmNbottomAttachment, XmATTACH_NONE); p++;
  XtSetArg(high_args[p], XmNleftAttachment, XmATTACH_FORM); p++;
  XtSetArg(high_args[p], XmNrightAttachment, XmATTACH_FORM); p++;
#ifdef SND_AS_WIDGET
  mw[menu_menu] = XtCreateWidget("mb", xmRowColumnWidgetClass, MAIN_PANE(ss), high_args, p);
#else
  mw[menu_menu] = XmCreateMenuBar(MAIN_PANE(ss), "menuBar", high_args, p);
#endif

  /* FILE MENU */
  mw[file_menu] = XmCreatePulldownMenu(mw[menu_menu], "filem", main_args, main_n);
  
  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[file_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'F'); high_n++;
  mw[f_cascade_menu] = XtCreateManagedWidget(STR_File, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);

  mw[f_open_menu] = XtCreateManagedWidget(STR_Open "   C-x C-f", xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_open_menu], XmNactivateCallback, file_open_callback, ss);
  XtVaSetValues(mw[f_open_menu], XmNmnemonic, 'O', NULL);

  mw[f_close_menu] = XtCreateManagedWidget(STR_Close "  C-x k", xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_close_menu], XmNactivateCallback, file_close_callback, ss);
  XtVaSetValues(mw[f_close_menu], XmNmnemonic, 'C', NULL);
  
  mw[f_save_menu] = XtCreateManagedWidget(STR_Save "   C-x C-s", xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_save_menu], XmNactivateCallback, file_save_callback, ss);
  XtVaSetValues(mw[f_save_menu], XmNmnemonic, 'S', NULL);
  
  mw[f_save_as_menu] = XtCreateManagedWidget(STR_Save_as, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_save_as_menu], XmNactivateCallback, file_save_as_callback, ss);
  XtVaSetValues(mw[f_save_as_menu], XmNmnemonic, 'a', NULL);
  
  mw[f_revert_menu] = XtCreateManagedWidget(STR_Revert, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_revert_menu], XmNactivateCallback, file_revert_callback, ss);
  XtVaSetValues(mw[f_revert_menu], XmNmnemonic, 'R', NULL);
  
  mw[f_mix_menu] = XtCreateManagedWidget(STR_Mix "    C-x C-q", xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_mix_menu], XmNactivateCallback, file_mix_callback_1, ss);
  XtVaSetValues(mw[f_mix_menu], XmNmnemonic, 'M', NULL);

  mw[f_update_menu] = XtCreateManagedWidget(STR_Update, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_update_menu], XmNactivateCallback, file_update_callback, ss);
  XtVaSetValues(mw[f_update_menu], XmNmnemonic, 'U', NULL);

  mw[f_new_menu] = XtCreateManagedWidget(STR_New, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_new_menu], XmNactivateCallback, file_new_callback, ss);
  XtVaSetValues(mw[f_new_menu], XmNmnemonic, 'N', NULL);

  mw[f_record_menu] = XtCreateManagedWidget(STR_Record, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_record_menu], XmNactivateCallback, file_record_callback, ss);

  mw[f_view_menu] = XtCreateManagedWidget(STR_View, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_view_menu], XmNactivateCallback, file_view_callback, ss);
  XtVaSetValues(mw[f_view_menu], XmNmnemonic, 'V', NULL);

  mw[f_print_menu] = XtCreateManagedWidget(STR_Print, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_print_menu], XmNactivateCallback, file_print_callback_1, ss);
  XtVaSetValues(mw[f_print_menu], XmNmnemonic, 'P', NULL);

  j = 0;
  if (!(ss->using_schemes)) {XtSetArg(sep_args[j], XmNbackground, (ss->sgx)->basic_color); j++;}
  XtSetArg(sep_args[j], XmNseparatorType, XmSHADOW_ETCHED_IN); j++;
  mw[f_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[file_menu], sep_args, j);

  mw[f_exit_menu] = XtCreateManagedWidget(STR_Exit, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_exit_menu], XmNactivateCallback, file_exit_callback, ss);
  XtVaSetValues(mw[f_exit_menu], XmNmnemonic, 'E', NULL);


  /* EDIT MENU */
  mw[edit_menu] = XmCreatePulldownMenu(mw[menu_menu], "editm", main_args, main_n);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[edit_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'E'); high_n++;
  mw[e_cascade_menu] = XtCreateManagedWidget(STR_Edit, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);
  
  mw[e_undo_menu] = XtCreateManagedWidget(STR_Undo "    C-x C-u", xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_undo_menu], XmNactivateCallback, edit_undo_callback, ss);
  XtVaSetValues(mw[e_undo_menu], XmNmnemonic, 'U', NULL);

  mw[e_redo_menu] = XtCreateManagedWidget(STR_Redo "    C-x C-r", xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_redo_menu], XmNactivateCallback, edit_redo_callback, ss);
  XtVaSetValues(mw[e_redo_menu], XmNmnemonic, 'R', NULL);

#if HAVE_EXTENSION_LANGUAGE
  mw[e_find_menu] = XtCreateManagedWidget(STR_Find "    C-s", xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_find_menu], XmNactivateCallback, edit_find_callback_1, ss);
  XtVaSetValues(mw[e_find_menu], XmNmnemonic, 'F', NULL);
#endif

  mw[e_select_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[edit_menu], sep_args, j);

  mw[e_cut_menu] = XtCreateManagedWidget(STR_Delete_Selection, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_cut_menu], XmNactivateCallback, edit_cut_callback, ss);
  XtVaSetValues(mw[e_cut_menu], XmNmnemonic, 'C', NULL);

  mw[e_paste_menu] = XtCreateManagedWidget(STR_Insert_Selection " C-x i", xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_paste_menu], XmNactivateCallback, edit_paste_callback, ss);
  XtVaSetValues(mw[e_paste_menu], XmNmnemonic, 'P', NULL);

  mw[e_mix_menu] = XtCreateManagedWidget(STR_Mix_Selection "    C-x q", xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_mix_menu], XmNactivateCallback, edit_mix_callback, ss);
  XtVaSetValues(mw[e_mix_menu], XmNmnemonic, 'M', NULL);

  mw[e_play_menu] = XtCreateManagedWidget(STR_Play_Selection "   C-x p", xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_play_menu], XmNactivateCallback, edit_play_callback, ss);
  XtVaSetValues(mw[e_play_menu], XmNmnemonic, 'P', NULL);

  mw[e_save_as_menu] = XtCreateManagedWidget(STR_Save_Selection "   C-x w", xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_save_as_menu], XmNactivateCallback, edit_save_as_callback, ss);
  XtVaSetValues(mw[e_save_as_menu], XmNmnemonic, 'S', NULL);

  mw[e_select_all_menu] = XtCreateManagedWidget(STR_Select_all, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_select_all_menu], XmNactivateCallback, edit_select_all_callback, ss);

  mw[e_edit_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[edit_menu], sep_args, j);

  mw[e_edenv_menu] = XtCreateManagedWidget(STR_Edit_Envelope, xmPushButtonWidgetClass, mw[edit_menu], main_args, main_n);
  XtAddCallback(mw[e_edenv_menu], XmNactivateCallback, edit_envelope_callback, ss);
  XtVaSetValues(mw[e_edenv_menu], XmNmnemonic, 'E', NULL);

  mw[e_header_menu] = XtCreateManagedWidget(STR_Edit_Header, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_header_menu], XmNactivateCallback, edit_header_callback_1, ss);
  XtVaSetValues(mw[e_header_menu], XmNmnemonic, 'H', NULL);



  /* VIEW MENU */
  mw[view_menu] = XmCreatePulldownMenu(mw[menu_menu], "viewm", main_args, main_n);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[view_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'V'); high_n++;
  mw[v_cascade_menu] = XtCreateManagedWidget(STR_View, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);
  XtAddCallback(mw[v_cascade_menu], XmNcascadingCallback, view_menu_update, NULL);

  mw[v_ctrls_menu] = XtCreateManagedWidget(STR_Show_controls, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_ctrls_menu], XmNactivateCallback, view_ctrls_callback, ss);
  XtVaSetValues(mw[v_ctrls_menu], XmNmnemonic, 'S', NULL);

#if HAVE_EXTENSION_LANGUAGE
  mw[v_listener_menu] = XtCreateManagedWidget(STR_Open_listener, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_listener_menu], XmNactivateCallback, view_listener_callback, ss);
  XtVaSetValues(mw[v_listener_menu], XmNmnemonic, 'L', NULL);
#endif

  mw[v_mix_panel_menu] = XtCreateManagedWidget(STR_Mix_Panel, xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_mix_panel_menu], XmNactivateCallback, view_mix_panel_callback, ss);

  mw[v_region_menu] = XtCreateManagedWidget(STR_Regions, xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_region_menu], XmNactivateCallback, view_region_callback_1, ss);
  XtVaSetValues(mw[v_region_menu], XmNmnemonic, 'R', NULL);

  mw[v_files_menu] = XtCreateManagedWidget(STR_Files, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_files_menu], XmNactivateCallback, view_files_callback_1, ss);
  XtVaSetValues(mw[v_files_menu], XmNmnemonic, 'F', NULL);

  mw[v_color_menu] = XtCreateManagedWidget(STR_Color, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_color_menu], XmNactivateCallback, view_color_callback_1, ss);

  mw[v_orientation_menu] = XtCreateManagedWidget(STR_Orientation, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_orientation_menu], XmNactivateCallback, view_orientation_callback_1, ss);
  XtVaSetValues(mw[v_orientation_menu], XmNmnemonic, 'O', NULL);

  mw[v_sep2_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[view_menu], sep_args, j);

  mw[v_graph_style_menu] = XmCreatePulldownMenu(mw[view_menu], "graph-style", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_graph_style_menu]); k++;
  mw[v_graph_style_cascade_menu] = XtCreateManagedWidget(STR_Graph_style, xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_lines_menu] = XtCreateManagedWidget(STR_lines, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_lines_menu], XmNactivateCallback, view_lines_callback, ss); 
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(mw[v_lines_menu], FALSE);

  mw[v_dots_menu] = XtCreateManagedWidget(STR_dots, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_dots_menu], XmNactivateCallback, view_dots_callback, ss);  
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(mw[v_dots_menu], FALSE);

  mw[v_filled_menu] = XtCreateManagedWidget(STR_filled, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_filled_menu], XmNactivateCallback, view_filled_callback, ss);  
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(mw[v_filled_menu], FALSE);

  mw[v_dots_and_lines_menu] = XtCreateManagedWidget(STR_dots_and_lines, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_dots_and_lines_menu], XmNactivateCallback, view_dots_and_lines_callback, ss);  
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(mw[v_dots_and_lines_menu], FALSE);

  mw[v_lollipops_menu] = XtCreateManagedWidget(STR_lollipops, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_lollipops_menu], XmNactivateCallback, view_lollipops_callback, ss);  
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(mw[v_lollipops_menu], FALSE);

  mw[v_cursor_menu] = XtCreateManagedWidget(STR_Verbose_cursor, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_cursor_menu], XmNactivateCallback, view_cursor_callback, ss);

  mw[v_combine_menu] = XmCreatePulldownMenu(mw[view_menu], "combine", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_combine_menu]); k++;
  mw[v_combine_cascade_menu] = XtCreateManagedWidget(STR_Channel_style, xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_combine_separate_menu] = XtCreateManagedWidget(STR_separate, xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_separate_menu], XmNactivateCallback, view_separate_callback, ss); 
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(mw[v_combine_separate_menu], FALSE);

  mw[v_combine_combined_menu] = XtCreateManagedWidget(STR_combined, xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_combined_menu], XmNactivateCallback, view_combined_callback, ss);  
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(mw[v_combine_combined_menu], FALSE);

  mw[v_combine_superimposed_menu] = XtCreateManagedWidget(STR_superimposed, xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_superimposed_menu], XmNactivateCallback, view_superimposed_callback, ss);  
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(mw[v_combine_superimposed_menu], FALSE);

  mw[v_equalize_panes_menu] = XtCreateManagedWidget(STR_Equalize_Panes, xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_equalize_panes_menu], XmNactivateCallback, view_equalize_panes_callback, ss);
  XtVaSetValues(mw[v_equalize_panes_menu], XmNmnemonic, 'N', NULL);

  mw[v_zero_menu] = XtCreateManagedWidget(STR_Show_Y0, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_zero_menu], XmNactivateCallback, view_zero_callback, ss);
  XtVaSetValues(mw[v_zero_menu], XmNmnemonic, 'y', NULL);

  mw[v_x_axis_menu] = XmCreatePulldownMenu(mw[view_menu], "xaxis", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_x_axis_menu]); k++;
  mw[v_x_axis_cascade_menu] = XtCreateManagedWidget(STR_X_axis_units, xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_x_axis_seconds_menu] = XtCreateManagedWidget(STR_seconds, xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_seconds_menu], XmNactivateCallback, options_x_axis_seconds_callback, ss);  
  set_sensitive(mw[v_x_axis_seconds_menu], FALSE);

  mw[v_x_axis_samples_menu] = XtCreateManagedWidget(STR_samples, xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_samples_menu], XmNactivateCallback, options_x_axis_samples_callback, ss);  

  mw[v_x_axis_percentage_menu] = XtCreateManagedWidget(STR_percentage, xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_percentage_menu], XmNactivateCallback, options_x_axis_percentage_callback, ss);  

  mw[v_x_axis_beats_menu] = XtCreateManagedWidget(STR_beats, xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_beats_menu], XmNactivateCallback, options_x_axis_beats_callback, ss);  

  mw[v_error_history_menu] = XtCreateManagedWidget(STR_Error_History, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_error_history_menu], XmNactivateCallback, view_error_history_callback, ss);



  /* OPTIONS MENU */
  mw[option_menu] = XmCreatePulldownMenu(mw[menu_menu], "optionm", main_args, main_n);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[option_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'O'); high_n++;
  mw[o_cascade_menu] = XtCreateManagedWidget(STR_Options, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);

  mw[o_transform_menu] = XtCreateManagedWidget(STR_Transform_Options, xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_transform_menu], XmNactivateCallback, options_transform_callback, ss);
  XtVaSetValues(mw[o_transform_menu], XmNmnemonic, 't', NULL);


  mw[o_speed_menu] = XmCreatePulldownMenu(mw[option_menu], "speedstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[o_speed_menu]); k++;
  mw[o_speed_cascade_menu] = XtCreateManagedWidget(STR_Speed_style, xmCascadeButtonWidgetClass, mw[option_menu], main_args, k);

  mw[o_speed_float_menu] = XtCreateManagedWidget(STR_float, xmPushButtonWidgetClass, mw[o_speed_menu], in_args, in_n);
  XtAddCallback(mw[o_speed_float_menu], XmNactivateCallback, options_speed_float_callback, ss);  

  mw[o_speed_semitone_menu] = XtCreateManagedWidget(STR_semitone, xmPushButtonWidgetClass, mw[o_speed_menu], main_args, main_n);
  XtAddCallback(mw[o_speed_semitone_menu], XmNactivateCallback, options_speed_semitone_callback, ss);  

  mw[o_speed_ratio_menu] = XtCreateManagedWidget(STR_ratio, xmPushButtonWidgetClass, mw[o_speed_menu], main_args, main_n);
  XtAddCallback(mw[o_speed_ratio_menu], XmNactivateCallback, options_speed_ratio_callback, ss);  


  mw[o_focus_style_menu] = XmCreatePulldownMenu(mw[option_menu], "focusstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[o_focus_style_menu]); k++;
  mw[o_focus_cascade_menu] = XtCreateManagedWidget(STR_Focus_style, xmCascadeButtonWidgetClass, mw[option_menu], main_args, k);

  mw[o_focus_left_menu] = XtCreateManagedWidget(STR_focus_left, xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_left_menu], XmNactivateCallback, options_focus_left_callback, ss);  

  mw[o_focus_right_menu] = XtCreateManagedWidget(STR_focus_right, xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_right_menu], XmNactivateCallback, options_focus_right_callback, ss);  

  mw[o_focus_middle_menu] = XtCreateManagedWidget(STR_focus_middle, xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_middle_menu], XmNactivateCallback, options_focus_middle_callback, ss);  

  mw[o_focus_active_menu] = XtCreateManagedWidget(STR_focus_active, xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_active_menu], XmNactivateCallback, options_focus_active_callback, ss);  
  activate_focus_menu(ss, zoom_focus_style(ss));

#if HAVE_EXTENSION_LANGUAGE
  mw[o_save_menu] = XtCreateManagedWidget(STR_Save_options, xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_save_menu], XmNactivateCallback, options_save_callback, ss);
  XtVaSetValues(mw[o_save_menu], XmNmnemonic, 'a', NULL);

  mw[o_save_state_menu] = XtCreateManagedWidget(STR_Save_state, xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_save_state_menu], XmNactivateCallback, options_save_state_callback, ss);
#endif



  /* HELP MENU */
  mw[help_menu] = XmCreatePulldownMenu(mw[menu_menu], "helpm", main_args, main_n);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[help_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'H'); high_n++;
  mw[h_cascade_menu] = XtCreateManagedWidget(STR_Help, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);

  mw[h_click_for_help_menu] = XtCreateManagedWidget(STR_Click_for_help, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_click_for_help_menu], XmNactivateCallback, help_context_help_callback, ss);
  XtVaSetValues(mw[h_click_for_help_menu], XmNmnemonic, 'C', NULL);

  mw[h_about_snd_menu] = XtCreateManagedWidget(STR_Overview, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_about_snd_menu], XmNactivateCallback, help_about_snd_callback, ss);
  XtVaSetValues(mw[h_about_snd_menu], XmNmnemonic, 'O', NULL);

  mw[h_fft_menu] = XtCreateManagedWidget(STR_FFT, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_fft_menu], XmNactivateCallback, help_fft_callback, ss);

  mw[h_find_menu] = XtCreateManagedWidget(STR_Find, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_find_menu], XmNactivateCallback, help_find_callback, ss);

  mw[h_undo_menu] = XtCreateManagedWidget(STR_Undo_and_redo, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_undo_menu], XmNactivateCallback, help_undo_callback, ss);

  mw[h_sync_menu] = XtCreateManagedWidget(STR_Sync, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_sync_menu], XmNactivateCallback, help_sync_callback, ss);

  mw[h_speed_menu] = XtCreateManagedWidget(STR_Speed, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_speed_menu], XmNactivateCallback, help_speed_callback, ss);

  mw[h_expand_menu] = XtCreateManagedWidget(STR_Expand, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_expand_menu], XmNactivateCallback, help_expand_callback, ss);

  mw[h_reverb_menu] = XtCreateManagedWidget(STR_Reverb, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_reverb_menu], XmNactivateCallback, help_reverb_callback, ss);

  mw[h_contrast_menu] = XtCreateManagedWidget(STR_Contrast, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_contrast_menu], XmNactivateCallback, help_contrast_callback, ss);

  mw[h_env_menu] = XtCreateManagedWidget(STR_Envelope, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_env_menu], XmNactivateCallback, help_env_callback, ss);

  mw[h_marks_menu] = XtCreateManagedWidget(STR_Marks, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_marks_menu], XmNactivateCallback, help_marks_callback, ss);

  mw[h_mix_menu] = XtCreateManagedWidget(STR_Mixing, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_mix_menu], XmNactivateCallback, help_mix_callback, ss);

  mw[h_sound_files_menu] = XtCreateManagedWidget(STR_Formats, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_sound_files_menu], XmNactivateCallback, help_sound_files_callback, ss);

  mw[h_init_file_menu] = XtCreateManagedWidget(STR_Customization, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_init_file_menu], XmNactivateCallback, help_init_file_callback, ss);

  mw[h_recording_menu] = XtCreateManagedWidget(STR_Recording, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_recording_menu], XmNactivateCallback, help_recording_callback, ss);

  mw[h_clm_menu] = XtCreateManagedWidget(STR_CLM, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_clm_menu], XmNactivateCallback, help_clm_callback, ss);

  mw[h_news_menu] = XtCreateManagedWidget(STR_News, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_news_menu], XmNactivateCallback, help_news_callback, ss);

  XtVaSetValues(mw[menu_menu], XmNmenuHelpWidget, mw[h_cascade_menu], NULL);
#ifndef SND_AS_WIDGET
  XtManageChild(mw[menu_menu]);
#endif

  return(mw[menu_menu]);
}

static Widget added_menus[MAX_MAIN_MENUS];
static int new_menu = 5;
static Widget *added_options = NULL;
static char **added_options_names = NULL;
static char *main_menu_names[MAX_MAIN_MENUS];
static int *added_options_menus = NULL;
static int added_options_size = 0;
static int added_options_pos = 0;
static int *added_options_callb = NULL;
enum {FILE_MENU, EDIT_MENU, VIEW_MENU, OPTIONS_MENU, HELP_MENU, POPUP_MENU};
#define INVALID_MENU -1

static char *main_menu_name(int callb)
{
  if ((callb < 0) || (added_options_menus[callb] >= MAX_MAIN_MENUS)) return(NULL); 
  switch (added_options_menus[callb])
    {
    case FILE_MENU:    return(STR_File); break;
    case EDIT_MENU:    return(STR_Edit); break;
    case VIEW_MENU:    return(STR_View); break;
    case OPTIONS_MENU: return(STR_Options); break;
    case HELP_MENU:    return(STR_Help); break;
    case POPUP_MENU:   return(STR_Popup); break;
    }
  return(main_menu_names[added_options_menus[callb]]);
}

static int callb2option(int callb)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if (added_options_callb[i] == callb)
      return(i);
  return(-1);
}

static void SND_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  int callb, opt;
  XtVaGetValues(w, XmNuserData, &callb, NULL);
  opt = callb2option(callb);
  if (opt != -1)
    {
      IF_MENU_HOOK(main_menu_name(opt), added_options_names[(opt < 0) ? 0 : opt])
	g_snd_callback(callb); /* menu option activate callback */
    }
}

static void GHC_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  g_snd_callback((int)cD); /* main menu cascading callback */
}

static void add_option(Widget w, int which_menu, char *label, int callb)
{
  int i;
  if (added_options_pos == added_options_size)
    {
      added_options_size += 8;
      if (added_options_pos == 0)
	{
	  added_options = (Widget *)CALLOC(added_options_size, sizeof(Widget));
	  added_options_names = (char **)CALLOC(added_options_size, sizeof(char *));
	  added_options_menus = (int *)CALLOC(added_options_size, sizeof(int));
	  added_options_callb = (int *)CALLOC(added_options_size, sizeof(int));
	}
      else
	{
	  added_options = (Widget *)REALLOC(added_options, added_options_size * sizeof(Widget));
	  added_options_names = (char **)REALLOC(added_options_names, added_options_size * sizeof(char *));
	  added_options_menus = (int *)REALLOC(added_options_menus, added_options_size * sizeof(int));
	  added_options_callb = (int *)REALLOC(added_options_callb, added_options_size * sizeof(int));
	  for (i = added_options_pos; i < added_options_size; i++) 
	    {
	      added_options[i] = NULL;
	      added_options_callb[i] = 0;
	    }
	}
    }
  added_options[added_options_pos] = w;
  added_options_menus[added_options_pos] = which_menu;
  added_options_names[added_options_pos] = copy_string(label);
  added_options_callb[added_options_pos] = callb;
  added_options_pos++;
}

Widget menu_widget(int which_menu)
{
  switch (which_menu)
    {
    case FILE_MENU:    return(mw[file_menu]); break;
    case EDIT_MENU:    return(mw[edit_menu]); break;
    case VIEW_MENU:    return(mw[view_menu]); break;
    case OPTIONS_MENU: return(mw[option_menu]); break;
    case HELP_MENU:    return(mw[help_menu]); break;
    case POPUP_MENU:   return(popup_menu); break;
    default: 
      if (which_menu < MAX_MAIN_MENUS)
	return(added_menus[which_menu]); 
      break;
    }
  return(NULL);
}

static void clobber_menu(Widget w, void *lab)
{
  char *name, *wname;
  name = (char *)lab;
  wname = XtName(w);
  if ((wname) && (name) && (strcmp(name, wname) == 0))
    XtUnmanageChild(w);
}

int g_remove_from_menu(int which_menu, char *label)
{
  Widget top_menu;
  top_menu = menu_widget(which_menu);
  if (top_menu)
    {
      map_over_children(top_menu, clobber_menu, (void *)label);
      return(0);
    }
  return(INVALID_MENU);
}

int g_change_menu_label(int which_menu, char *old_label, char *new_label)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(added_options_names[i]) &&
	(strcmp(old_label, added_options_names[i]) == 0) && 
	(added_options[i]))
      {
	set_button_label(added_options[i], new_label);
	if (added_options_names[i]) FREE(added_options_names[i]);
	added_options_names[i] = copy_string(new_label);
	return(0);
      }
  return(INVALID_MENU);
}

int g_set_menu_sensitive(int which_menu, char *old_label, int on)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(added_options_names[i]) &&
	(strcmp(old_label, added_options_names[i]) == 0) && 
	(added_options[i]))
      {
	set_sensitive(added_options[i], on);
	return(0);
      }
  return(INVALID_MENU);
}

int g_menu_is_sensitive(int which_menu, char *old_label)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(added_options_names[i]) &&
	(strcmp(old_label, added_options_names[i]) == 0) && 
	(added_options[i]))
      return(is_sensitive(added_options[i]));
  return(0);
}

int g_add_to_main_menu(snd_state *ss, char *label, long slot)
{
  static Arg args[12];
  Widget m, cas;
  int n;

  if (auto_resize(ss)) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, FALSE, NULL);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  m = XmCreatePulldownMenu(mw[menu_menu], label, args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n], XmNsubMenuId, m); n++;
  cas = XtCreateManagedWidget(label, xmCascadeButtonWidgetClass, mw[menu_menu], args, n);
  if (slot >= 0) XtAddCallback(cas, XmNcascadingCallback, GHC_callback, (XtPointer)slot);

  if (auto_resize(ss)) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, TRUE, NULL);

  new_menu++;

  if (new_menu < MAX_MAIN_MENUS)
    {
      added_menus[new_menu] = m;
      main_menu_names[new_menu] = copy_string(label);
      return(new_menu);
    }
  else return(INVALID_MENU);
}

int g_add_to_menu(snd_state *ss, int which_menu, char *label, int callb, int position)
{
  Widget m, menw;
  static Arg args[12];
  int n;
  menw = menu_widget(which_menu);
  if (menw == NULL) return(INVALID_MENU);
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
  if (label)
    {
      XtSetArg(args[n], XmNuserData, callb); n++;
      m = XtCreateManagedWidget(label, xmPushButtonWidgetClass, menw, args, n);
      XtAddCallback(m, XmNactivateCallback, SND_callback, ss);
      add_option(m, which_menu, label, callb);
    }
  else
    {
      XtCreateManagedWidget("sep", xmSeparatorWidgetClass, menw, args, n);
    }
  return(0);
}



/* -------------------------------- POPUP MENU -------------------------------- */

static int stopping = 0;

static void popup_play_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_state *ss = (snd_state *)cD;
  snd_info *sp;
  IF_MENU_HOOK("Popup", STR_Play)
    {
      sp = any_selected_sound(ss);
      if (stopping)
	{
	  stop_playing_all_sounds();
	  stopping = 0;
	  set_button_label(w, "Play");
	  if (sp) set_play_button(sp, 0);
	}
      else
	{
	  if (sp)
	    {
	      play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "popup play", 0);
	      set_play_button(sp, 1);
	      stopping = 1;
	      set_button_label(w, "Stop playing");
	    }
	}
    }
}

void reflect_play_stop_in_popup_menu(void)
{
  stopping = 0;
  if (popup_menu)
    set_button_label(popup_children[W_pop_play], "Play");
}

static void popup_save_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK("Popup", STR_Save) save_edits(any_selected_sound((snd_state *)cD), NULL);
}

static void popup_undo_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", STR_Undo) undo_edit_with_sync(current_channel(ss), 1);
}

static void popup_redo_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", STR_Redo) redo_edit_with_sync(current_channel(ss), 1);
}

static void popup_equalize_panes_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK("Popup", STR_Equalize_Panes) equalize_all_panes((snd_state *)cD);
}

static void popup_info_callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_info *sp;
  IF_MENU_HOOK("Popup", STR_Info)
    {
      sp = selected_sound((snd_state *)cD);
      if (sp) display_info(sp);
    }
}

void post_popup(XButtonPressedEvent *event)
{
  XmMenuPosition(popup_menu, event);
  XtManageChild(popup_menu);
}

#if (XmVERSION == 1)
static void Post_Popup_Menu(Widget w, XtPointer cD, XEvent *event, Boolean *flag)
{
  if (event->xbutton.button == BUTTON_3)
    post_popup((XButtonPressedEvent *)event);
}
#endif

void create_popup_menu(snd_state *ss)
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
      popup_menu = XmCreatePopupMenu(mainp, "popup-menu", args, n);
#if (XmVERSION == 1)
      XtAddEventHandler(mainp, ButtonPressMask, FALSE, Post_Popup_Menu, popup_menu);
#endif

      popup_children[W_pop_menu] = XtCreateManagedWidget("snd", xmLabelWidgetClass, popup_menu, args, n);
      popup_children[W_pop_sep] = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, popup_menu, args, n);
      popup_children[W_pop_play] = XtCreateManagedWidget(STR_Play, xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_children[W_pop_play], XmNactivateCallback, popup_play_callback, ss);
      XtVaSetValues(popup_children[W_pop_play], XmNsensitive, FALSE, NULL);
      popup_children[W_pop_undo] = XtCreateManagedWidget(STR_Undo, xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_undo], XmNsensitive, FALSE, NULL);
      XtAddCallback(popup_children[W_pop_undo], XmNactivateCallback, popup_undo_callback, ss);
      popup_children[W_pop_redo] = XtCreateManagedWidget(STR_Redo, xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_redo], XmNsensitive, FALSE, NULL);
      XtAddCallback(popup_children[W_pop_redo], XmNactivateCallback, popup_redo_callback, ss);
      popup_children[W_pop_save] = XtCreateManagedWidget(STR_Save, xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_children[W_pop_save], XmNactivateCallback, popup_save_callback, ss);
      XtVaSetValues(popup_children[W_pop_save], XmNsensitive, FALSE, NULL);
      popup_children[W_pop_equalize_panes] = XtCreateManagedWidget(STR_Equalize_Panes, xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_equalize_panes], XmNsensitive, FALSE, NULL);
      XtAddCallback(popup_children[W_pop_equalize_panes], XmNactivateCallback, popup_equalize_panes_callback, ss);
      popup_children[W_pop_info] = XtCreateManagedWidget(STR_Info, xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_info], XmNsensitive, FALSE, NULL);
      XtAddCallback(popup_children[W_pop_info], XmNactivateCallback, popup_info_callback, ss);
    }
}

static XEN g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets ") returns list of top level menu widgets ((0)main (1)file (2)edit (3)view (4)options (5)help)"
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
  #define H_menu_hook S_menu_hook " (name option) is called each time a menu item is \
selected; its entries should be functions of two arguments, the top menu \
name and the option selected (both as strings), and should return #f if it \
wants to override the default menu action:\n\
  (add-hook! menu-hook\n\
    (lambda (name option)\n\
      (if (and (string=? name \"File\")\n\
               (string=? option \"Exit\"))\n\
        (begin\n\
          (snd-print \"no exit!\")\n\
          #f)\n\
        #t))) ; #t to make sure other menu items remain active"

  XEN_DEFINE_HOOK(menu_hook, S_menu_hook, 2, H_menu_hook);
  XEN_DEFINE_PROCEDURE(S_menu_widgets, g_menu_widgets_w, 0, 0, 0, H_menu_widgets);
}
