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
          o_stats_menu,
        view_menu, v_cascade_menu,
          v_normalize_menu, 
          v_graph_style_menu, v_graph_style_cascade_menu,
            v_lines_menu, v_dots_menu, v_filled_menu, v_dots_and_lines_menu, v_lollipops_menu,
          v_zero_menu, v_cursor_menu, v_ctrls_menu, v_listener_menu,
          v_region_menu,
          v_combine_menu, v_combine_cascade_menu,
            v_combine_separate_menu, v_combine_combined_menu, v_combine_superimposed_menu,
          v_color_menu, v_orientation_menu, 
          v_files_menu, v_mix_panel_menu,
          v_x_axis_menu, v_x_axis_cascade_menu,
            v_x_axis_seconds_menu, v_x_axis_samples_menu, v_x_axis_percentage_menu,
          v_error_history_menu,
          v_sep2_menu
};

#define NUM_MENU_WIDGETS 99
static Widget mw[NUM_MENU_WIDGETS];

enum {W_pop_menu, W_pop_sep, W_pop_play, W_pop_undo, W_pop_redo, W_pop_save, W_pop_normalize, W_pop_info};
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

Widget view_normalize_menu(void) {return(mw[v_normalize_menu]);}
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
Widget view_x_axis_samples_menu(void) {return(mw[v_x_axis_samples_menu]);}
Widget view_x_axis_percentage_menu(void) {return(mw[v_x_axis_percentage_menu]);}

Widget options_save_state_menu(void) {return(mw[o_save_state_menu]);}
Widget options_stats_menu(void) {return(mw[o_stats_menu]);}
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
Widget popup_normalize_menu(void) {return(popup_children[W_pop_normalize]);}
Widget popup_info_menu(void) {return(popup_children[W_pop_info]);}

void set_menu_label(Widget w, const char *label) {if (w) set_button_label(w, label);}

#if HAVE_HOOKS
static SCM menu_hook;
static int call_menu_hook(char *name, char *option)
{
  SCM res = SCM_BOOL_T;
  if ((name) && (HOOKED(menu_hook)))
    res = g_c_run_and_hook(menu_hook, SCM_LIST2(TO_SCM_STRING(name), TO_SCM_STRING(option)));
  return(SCM_TRUE_P(res));
}
#define IF_MENU_HOOK(NAME, OPTION) if (call_menu_hook(NAME, OPTION))
#else
#define IF_MENU_HOOK(NAME, OPTION)
#endif


/* -------------------------------- FILE MENU -------------------------------- */

static void File_Open_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Open) make_open_file_dialog((snd_state *)cD);
}

static void File_View_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_File, STR_View)
    {
      ss->viewing = 1;
      make_open_file_dialog(ss);
    }
}

static void File_New_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_New) new_file_from_menu((snd_state *)cD);
}

static void File_Help_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  click_for_file_menu_help((snd_state *)cD);
}

static void File_Record_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Record) snd_record_file((snd_state *)cD);
}

static void File_Close_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Close) close_file_from_menu((snd_state *)cD);
}

static void File_Save_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Save) save_file_from_menu((snd_state *)cD);
}

static void File_Update_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Update) update_file_from_menu((snd_state *)cD);
}

static void File_Save_As_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Save_as) make_file_save_as_dialog((snd_state *)cD);
}

static void File_Revert_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Revert) revert_file_from_menu((snd_state *)cD);
}

static void File_Exit_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Exit) exit_from_menu((snd_state *)cD);
}

static void File_Mix_Callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Mix) File_Mix_Callback(w, cD, mD);
}

static void File_Print_Callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_File, STR_Print) File_Print_Callback(w, cD, mD);
}



/* -------------------------------- EDIT MENU -------------------------------- */


static void Edit_Mix_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Mix_Selection) mix_selection_from_menu((snd_state *)cD);
}

static void Edit_Envelope_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Edit_Envelope) create_envelope_editor((snd_state *)cD);
}

static void Edit_Help_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  click_for_edit_menu_help((snd_state *)cD);
}

static void Edit_Cut_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Delete_Selection) delete_selection("Edit: Cut", UPDATE_DISPLAY);
}

static void Edit_Paste_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Insert_Selection) paste_selection_from_menu((snd_state *)cD);
}

static void Edit_Save_As_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Save_Selection) make_edit_save_as_dialog((snd_state *)cD);
}

static void Edit_Select_All_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Select_all) select_all(current_channel((snd_state *)cD));
}

static void Edit_Undo_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Undo) undo_edit_with_sync(current_channel((snd_state *)cD), 1);
}

static void Edit_Redo_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Redo) redo_edit_with_sync(current_channel((snd_state *)cD), 1);
}

static void Edit_Play_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Play_selection) play_selection(IN_BACKGROUND);
}

static void Edit_Header_Callback(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  snd_info *sp;
  IF_MENU_HOOK(STR_Edit, STR_Edit_Header) 
    {
      sp = selected_sound(ss);
      if (sp) edit_header(sp);
    }
}

static void Edit_Find_Callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Find) Edit_Find_Callback(w, cD, mD);
}


/* -------------------------------- VIEW MENU -------------------------------- */

static void View_Help_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  click_for_view_menu_help((snd_state *)cD);
}

static void View_Separate_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_separate) set_channel_style((snd_state *)cD, CHANNELS_SEPARATE);
}

static void View_Combined_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_combined) set_channel_style((snd_state *)cD, CHANNELS_COMBINED);
}

static void View_Superimposed_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_superimposed) set_channel_style((snd_state *)cD, CHANNELS_SUPERIMPOSED);
}

static void View_Normalize_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Normalize) normalize_all_sounds((snd_state *)cD);
}

static void View_Dots_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_dots) set_graph_style((snd_state *)cD, GRAPH_DOTS);
}

static void View_Lines_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_lines) set_graph_style((snd_state *)cD, GRAPH_LINES);
}

static void View_Filled_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_filled) set_graph_style((snd_state *)cD, GRAPH_FILLED);
}

static void View_Dots_and_Lines_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_dots_and_lines) set_graph_style((snd_state *)cD, GRAPH_DOTS_AND_LINES);
}

static void View_Lollipops_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_lollipops) set_graph_style((snd_state *)cD, GRAPH_LOLLIPOPS);
}

static void View_Listener_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Show_listener) handle_listener((snd_state *)cD, LISTENER_OPEN);
}

static void View_Mix_Panel_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Mix_Panel) make_mix_panel((snd_state *)cD);
}

static void View_Error_History_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Error_History) show_snd_errors((snd_state *)cD);
}

static void View_Zero_Callback(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Show_Y0) set_show_y_zero(ss, (!(show_y_zero(ss))));
}

static void View_Cursor_Callback(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Verbose_cursor) set_verbose_cursor(ss, (!(verbose_cursor(ss))));
}

static void View_Ctrls_Callback(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Show_controls) 
    {
      if (ss->ctrls_height < 100) 
	show_controls(ss); 
      else hide_controls(ss); /* snd-xmain.c */
    }
}

static void View_Region_Callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Regions) View_Region_Callback(w, cD, mD);
}

static void View_Orientation_Callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Orientation) View_Orientation_Callback(w, cD, mD);
}

static void View_Color_Callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Color) View_Color_Callback(w, cD, mD);
}

static void View_Files_Callback_1(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_View, STR_Files) View_Files_Callback(w, cD, mD);
}



/* -------------------------------- OPTIONS MENU -------------------------------- */

static void Options_Help_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  click_for_options_menu_help((snd_state *)cD);
}

static void Options_Transform_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_Transform_Options) fire_up_transform_dialog((snd_state *)cD);
}

static void Options_Save_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_Save_options) save_options_from_menu((snd_state *)cD);
}

static void Options_Focus_Right_Callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_right) activate_focus_menu((snd_state *)cD, FOCUS_RIGHT);
}

static void Options_Focus_Left_Callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_left) activate_focus_menu((snd_state *)cD, FOCUS_LEFT);
}

static void Options_Focus_Middle_Callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_middle) activate_focus_menu((snd_state *)cD, FOCUS_MIDDLE);
}

static void Options_Focus_Active_Callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_active) activate_focus_menu((snd_state *)cD, FOCUS_ACTIVE);
}

static void Options_Speed_Float_Callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_float) activate_speed_in_menu((snd_state *)cD, SPEED_AS_FLOAT);
}

static void Options_Speed_Ratio_Callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_ratio) activate_speed_in_menu((snd_state *)cD, SPEED_AS_RATIO);
}

static void Options_Speed_Semitone_Callback(Widget w, XtPointer cD, XtPointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_semitone) activate_speed_in_menu((snd_state *)cD, SPEED_AS_SEMITONE);
}

static void Options_X_Axis_Seconds_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_seconds) set_x_axis_style((snd_state *)cD, X_IN_SECONDS);
}

static void Options_X_Axis_Samples_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_samples) set_x_axis_style((snd_state *)cD, X_IN_SAMPLES);
}

static void Options_X_Axis_Percentage_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_percentage) set_x_axis_style((snd_state *)cD, X_TO_ONE);
}

static void Options_Save_State_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK(STR_Options, STR_Save_state) save_state_from_menu((snd_state *)cD);
}

static void Options_Stats_Callback(Widget w, XtPointer cD, XtPointer mD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_Options, STR_Show_stats) set_show_usage_stats(ss, (!(show_usage_stats(ss))));
}




/* -------------------------------- HELP MENU -------------------------------- */

static void Help_Help_Callback(Widget w, XtPointer cD, XtPointer mD) {click_for_help_menu_help((snd_state *)cD);}

static void Help_Context_Help_Callback(Widget w, XtPointer cD, XtPointer mD) 
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

static void Help_About_Snd_Callback(Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Overview) about_snd_help((snd_state *)cD);}
static void Help_FFT_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_FFT) fft_help((snd_state *)cD);}
static void Help_Find_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Find) find_help((snd_state *)cD);}
static void Help_Undo_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Undo_and_redo) undo_help((snd_state *)cD);}
static void Help_Sync_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Sync) sync_help((snd_state *)cD);}
static void Help_Speed_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Speed) speed_help((snd_state *)cD);}
static void Help_Expand_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Expand) expand_help((snd_state *)cD);}
static void Help_Reverb_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Reverb) reverb_help((snd_state *)cD);}
static void Help_Contrast_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Contrast) contrast_help((snd_state *)cD);}
static void Help_Env_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Envelope) env_help((snd_state *)cD);}
static void Help_Marks_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Marks) marks_help((snd_state *)cD);}
static void Help_Mix_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Mixing) mix_help((snd_state *)cD);}
static void Help_Sound_Files_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Formats) sound_files_help((snd_state *)cD);}
static void Help_Init_File_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Customization) init_file_help((snd_state *)cD);}
static void Help_Recording_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_Recording) recording_help((snd_state *)cD);}

#if HAVE_GUILE
static void Help_CLM_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_CLM) clm_help((snd_state *)cD);}
#endif

static void Help_News_Callback (Widget w, XtPointer cD, XtPointer mD) {IF_MENU_HOOK(STR_Help, STR_News) news_help((snd_state *)cD);}



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
  XtSetArg(high_args[n], XmNuserData, ss); n++; /* used in snd-xclip by drop site to get main state data from whatever widget got the drop */
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
  XtAddCallback(mw[f_cascade_menu], XmNhelpCallback, File_Help_Callback, ss);

  mw[f_open_menu] = XtCreateManagedWidget(STR_Open, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_open_menu], XmNactivateCallback, File_Open_Callback, ss);
  XtVaSetValues(mw[f_open_menu], XmNmnemonic, 'O', NULL);

  mw[f_close_menu] = XtCreateManagedWidget(STR_Close, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_close_menu], XmNactivateCallback, File_Close_Callback, ss);
  XtVaSetValues(mw[f_close_menu], XmNmnemonic, 'C', NULL);
  
  mw[f_save_menu] = XtCreateManagedWidget(STR_Save, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_save_menu], XmNactivateCallback, File_Save_Callback, ss);
  XtVaSetValues(mw[f_save_menu], XmNmnemonic, 'S', NULL);
  
  mw[f_save_as_menu] = XtCreateManagedWidget(STR_Save_as, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_save_as_menu], XmNactivateCallback, File_Save_As_Callback, ss);
  XtVaSetValues(mw[f_save_as_menu], XmNmnemonic, 'a', NULL);
  
  mw[f_revert_menu] = XtCreateManagedWidget(STR_Revert, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_revert_menu], XmNactivateCallback, File_Revert_Callback, ss);
  XtVaSetValues(mw[f_revert_menu], XmNmnemonic, 'R', NULL);
  
  mw[f_mix_menu] = XtCreateManagedWidget(STR_Mix, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_mix_menu], XmNactivateCallback, File_Mix_Callback_1, ss);
  XtVaSetValues(mw[f_mix_menu], XmNmnemonic, 'M', NULL);

  mw[f_update_menu] = XtCreateManagedWidget(STR_Update, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_update_menu], XmNactivateCallback, File_Update_Callback, ss);
  XtVaSetValues(mw[f_update_menu], XmNmnemonic, 'U', NULL);

  mw[f_new_menu] = XtCreateManagedWidget(STR_New, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_new_menu], XmNactivateCallback, File_New_Callback, ss);
  XtVaSetValues(mw[f_new_menu], XmNmnemonic, 'N', NULL);

  mw[f_record_menu] = XtCreateManagedWidget(STR_Record, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_record_menu], XmNactivateCallback, File_Record_Callback, ss);

  mw[f_view_menu] = XtCreateManagedWidget(STR_View, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_view_menu], XmNactivateCallback, File_View_Callback, ss);
  XtVaSetValues(mw[f_view_menu], XmNmnemonic, 'V', NULL);

  mw[f_print_menu] = XtCreateManagedWidget(STR_Print, xmPushButtonWidgetClass, mw[file_menu], in_args, in_n);
  XtAddCallback(mw[f_print_menu], XmNactivateCallback, File_Print_Callback_1, ss);
  XtVaSetValues(mw[f_print_menu], XmNmnemonic, 'P', NULL);

  j = 0;
  if (!(ss->using_schemes)) {XtSetArg(sep_args[j], XmNbackground, (ss->sgx)->basic_color); j++;}
  XtSetArg(sep_args[j], XmNseparatorType, XmSHADOW_ETCHED_IN); j++;
  mw[f_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[file_menu], sep_args, j);

  mw[f_exit_menu] = XtCreateManagedWidget(STR_Exit, xmPushButtonWidgetClass, mw[file_menu], main_args, main_n);
  XtAddCallback(mw[f_exit_menu], XmNactivateCallback, File_Exit_Callback, ss);
  XtVaSetValues(mw[f_exit_menu], XmNmnemonic, 'E', NULL);


  /* EDIT MENU */
  mw[edit_menu] = XmCreatePulldownMenu(mw[menu_menu], "editm", main_args, main_n);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[edit_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'E'); high_n++;
  mw[e_cascade_menu] = XtCreateManagedWidget(STR_Edit, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);
  XtAddCallback(mw[e_cascade_menu], XmNhelpCallback, Edit_Help_Callback, ss);
  
  mw[e_undo_menu] = XtCreateManagedWidget(STR_Undo, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_undo_menu], XmNactivateCallback, Edit_Undo_Callback, ss);
  XtVaSetValues(mw[e_undo_menu], XmNmnemonic, 'U', NULL);

  mw[e_redo_menu] = XtCreateManagedWidget(STR_Redo, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_redo_menu], XmNactivateCallback, Edit_Redo_Callback, ss);
  XtVaSetValues(mw[e_redo_menu], XmNmnemonic, 'R', NULL);

  mw[e_find_menu] = XtCreateManagedWidget(STR_Find, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_find_menu], XmNactivateCallback, Edit_Find_Callback_1, ss);
  XtVaSetValues(mw[e_find_menu], XmNmnemonic, 'F', NULL);

  mw[e_select_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[edit_menu], sep_args, j);

  mw[e_cut_menu] = XtCreateManagedWidget(STR_Delete_Selection, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_cut_menu], XmNactivateCallback, Edit_Cut_Callback, ss);
  XtVaSetValues(mw[e_cut_menu], XmNmnemonic, 'C', NULL);

  mw[e_paste_menu] = XtCreateManagedWidget(STR_Insert_Selection, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_paste_menu], XmNactivateCallback, Edit_Paste_Callback, ss);
  XtVaSetValues(mw[e_paste_menu], XmNmnemonic, 'P', NULL);

  mw[e_mix_menu] = XtCreateManagedWidget(STR_Mix_Selection, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_mix_menu], XmNactivateCallback, Edit_Mix_Callback, ss);
  XtVaSetValues(mw[e_mix_menu], XmNmnemonic, 'M', NULL);

  mw[e_play_menu] = XtCreateManagedWidget(STR_Play_selection, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_play_menu], XmNactivateCallback, Edit_Play_Callback, ss);
  XtVaSetValues(mw[e_play_menu], XmNmnemonic, 'P', NULL);

  mw[e_save_as_menu] = XtCreateManagedWidget(STR_Save_Selection, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_save_as_menu], XmNactivateCallback, Edit_Save_As_Callback, ss);
  XtVaSetValues(mw[e_save_as_menu], XmNmnemonic, 'S', NULL);

  mw[e_select_all_menu] = XtCreateManagedWidget(STR_Select_all, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_select_all_menu], XmNactivateCallback, Edit_Select_All_Callback, ss);

  mw[e_edit_sep_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[edit_menu], sep_args, j);

  mw[e_edenv_menu] = XtCreateManagedWidget(STR_Edit_Envelope, xmPushButtonWidgetClass, mw[edit_menu], main_args, main_n);
  XtAddCallback(mw[e_edenv_menu], XmNactivateCallback, Edit_Envelope_Callback, ss);
  XtVaSetValues(mw[e_edenv_menu], XmNmnemonic, 'E', NULL);

  mw[e_header_menu] = XtCreateManagedWidget(STR_Edit_Header, xmPushButtonWidgetClass, mw[edit_menu], in_args, in_n);
  XtAddCallback(mw[e_header_menu], XmNactivateCallback, Edit_Header_Callback, ss);
  XtVaSetValues(mw[e_header_menu], XmNmnemonic, 'H', NULL);



  /* VIEW MENU */
  mw[view_menu] = XmCreatePulldownMenu(mw[menu_menu], "viewm", main_args, main_n);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[view_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'V'); high_n++;
  mw[v_cascade_menu] = XtCreateManagedWidget(STR_View, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);
  XtAddCallback(mw[v_cascade_menu], XmNhelpCallback, View_Help_Callback, ss);

  mw[v_ctrls_menu] = XtCreateManagedWidget(STR_Show_controls, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_ctrls_menu], XmNactivateCallback, View_Ctrls_Callback, ss);
  XtVaSetValues(mw[v_ctrls_menu], XmNmnemonic, 'S', NULL);

  mw[v_listener_menu] = XtCreateManagedWidget(STR_Open_listener, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_listener_menu], XmNactivateCallback, View_Listener_Callback, ss);
  XtVaSetValues(mw[v_listener_menu], XmNmnemonic, 'L', NULL);

  mw[v_mix_panel_menu] = XtCreateManagedWidget(STR_Mix_Panel, xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_mix_panel_menu], XmNactivateCallback, View_Mix_Panel_Callback, ss);

  mw[v_region_menu] = XtCreateManagedWidget(STR_Regions, xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_region_menu], XmNactivateCallback, View_Region_Callback_1, ss);
  XtVaSetValues(mw[v_region_menu], XmNmnemonic, 'R', NULL);

  mw[v_files_menu] = XtCreateManagedWidget(STR_Files, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_files_menu], XmNactivateCallback, View_Files_Callback_1, ss);
  XtVaSetValues(mw[v_files_menu], XmNmnemonic, 'F', NULL);

  mw[v_color_menu] = XtCreateManagedWidget(STR_Color, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_color_menu], XmNactivateCallback, View_Color_Callback_1, ss);

  mw[v_orientation_menu] = XtCreateManagedWidget(STR_Orientation, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_orientation_menu], XmNactivateCallback, View_Orientation_Callback_1, ss);
  XtVaSetValues(mw[v_orientation_menu], XmNmnemonic, 'O', NULL);

  mw[v_sep2_menu] = XtCreateManagedWidget("", xmSeparatorWidgetClass, mw[view_menu], sep_args, j);

  mw[v_graph_style_menu] = XmCreatePulldownMenu(mw[view_menu], "graph-style", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_graph_style_menu]); k++;
  mw[v_graph_style_cascade_menu] = XtCreateManagedWidget(STR_Graph_style, xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_lines_menu] = XtCreateManagedWidget(STR_lines, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_lines_menu], XmNactivateCallback, View_Lines_Callback, ss); 
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(mw[v_lines_menu], FALSE);

  mw[v_dots_menu] = XtCreateManagedWidget(STR_dots, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_dots_menu], XmNactivateCallback, View_Dots_Callback, ss);  
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(mw[v_dots_menu], FALSE);

  mw[v_filled_menu] = XtCreateManagedWidget(STR_filled, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_filled_menu], XmNactivateCallback, View_Filled_Callback, ss);  
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(mw[v_filled_menu], FALSE);

  mw[v_dots_and_lines_menu] = XtCreateManagedWidget(STR_dots_and_lines, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_dots_and_lines_menu], XmNactivateCallback, View_Dots_and_Lines_Callback, ss);  
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(mw[v_dots_and_lines_menu], FALSE);

  mw[v_lollipops_menu] = XtCreateManagedWidget(STR_lollipops, xmPushButtonWidgetClass, mw[v_graph_style_menu], main_args, main_n);
  XtAddCallback(mw[v_lollipops_menu], XmNactivateCallback, View_Lollipops_Callback, ss);  
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(mw[v_lollipops_menu], FALSE);

  mw[v_cursor_menu] = XtCreateManagedWidget(STR_Verbose_cursor, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_cursor_menu], XmNactivateCallback, View_Cursor_Callback, ss);

  mw[v_combine_menu] = XmCreatePulldownMenu(mw[view_menu], "combine", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_combine_menu]); k++;
  mw[v_combine_cascade_menu] = XtCreateManagedWidget(STR_Channel_style, xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_combine_separate_menu] = XtCreateManagedWidget(STR_separate, xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_separate_menu], XmNactivateCallback, View_Separate_Callback, ss); 
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(mw[v_combine_separate_menu], FALSE);

  mw[v_combine_combined_menu] = XtCreateManagedWidget(STR_combined, xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_combined_menu], XmNactivateCallback, View_Combined_Callback, ss);  
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(mw[v_combine_combined_menu], FALSE);

  mw[v_combine_superimposed_menu] = XtCreateManagedWidget(STR_superimposed, xmPushButtonWidgetClass, mw[v_combine_menu], main_args, main_n);
  XtAddCallback(mw[v_combine_superimposed_menu], XmNactivateCallback, View_Superimposed_Callback, ss);  
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(mw[v_combine_superimposed_menu], FALSE);

  mw[v_normalize_menu] = XtCreateManagedWidget(STR_Normalize, xmPushButtonWidgetClass, mw[view_menu], in_args, in_n);
  XtAddCallback(mw[v_normalize_menu], XmNactivateCallback, View_Normalize_Callback, ss);
  XtVaSetValues(mw[v_normalize_menu], XmNmnemonic, 'N', NULL);

  mw[v_zero_menu] = XtCreateManagedWidget(STR_Show_Y0, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_zero_menu], XmNactivateCallback, View_Zero_Callback, ss);
  XtVaSetValues(mw[v_zero_menu], XmNmnemonic, 'y', NULL);

  mw[v_x_axis_menu] = XmCreatePulldownMenu(mw[view_menu], "xaxis", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[v_x_axis_menu]); k++;
  mw[v_x_axis_cascade_menu] = XtCreateManagedWidget(STR_X_axis_units, xmCascadeButtonWidgetClass, mw[view_menu], main_args, k);

  mw[v_x_axis_seconds_menu] = XtCreateManagedWidget(STR_seconds, xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_seconds_menu], XmNactivateCallback, Options_X_Axis_Seconds_Callback, ss);  
  set_sensitive(mw[v_x_axis_seconds_menu], FALSE);

  mw[v_x_axis_samples_menu] = XtCreateManagedWidget(STR_samples, xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_samples_menu], XmNactivateCallback, Options_X_Axis_Samples_Callback, ss);  

  mw[v_x_axis_percentage_menu] = XtCreateManagedWidget(STR_percentage, xmPushButtonWidgetClass, mw[v_x_axis_menu], main_args, main_n);
  XtAddCallback(mw[v_x_axis_percentage_menu], XmNactivateCallback, Options_X_Axis_Percentage_Callback, ss);  

  mw[v_error_history_menu] = XtCreateManagedWidget(STR_Error_History, xmPushButtonWidgetClass, mw[view_menu], main_args, main_n);
  XtAddCallback(mw[v_error_history_menu], XmNactivateCallback, View_Error_History_Callback, ss);



  /* OPTIONS MENU */
  mw[option_menu] = XmCreatePulldownMenu(mw[menu_menu], "optionm", main_args, main_n);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[option_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'O'); high_n++;
  mw[o_cascade_menu] = XtCreateManagedWidget(STR_Options, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);
  XtAddCallback(mw[o_cascade_menu], XmNhelpCallback, Options_Help_Callback, ss);

  mw[o_transform_menu] = XtCreateManagedWidget(STR_Transform_Options, xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_transform_menu], XmNactivateCallback, Options_Transform_Callback, ss);
  XtVaSetValues(mw[o_transform_menu], XmNmnemonic, 't', NULL);


  mw[o_speed_menu] = XmCreatePulldownMenu(mw[option_menu], "speedstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[o_speed_menu]); k++;
  mw[o_speed_cascade_menu] = XtCreateManagedWidget(STR_Speed_style, xmCascadeButtonWidgetClass, mw[option_menu], main_args, k);

  mw[o_speed_float_menu] = XtCreateManagedWidget(STR_float, xmPushButtonWidgetClass, mw[o_speed_menu], in_args, in_n);
  XtAddCallback(mw[o_speed_float_menu], XmNactivateCallback, Options_Speed_Float_Callback, ss);  

  mw[o_speed_semitone_menu] = XtCreateManagedWidget(STR_semitone, xmPushButtonWidgetClass, mw[o_speed_menu], main_args, main_n);
  XtAddCallback(mw[o_speed_semitone_menu], XmNactivateCallback, Options_Speed_Semitone_Callback, ss);  

  mw[o_speed_ratio_menu] = XtCreateManagedWidget(STR_ratio, xmPushButtonWidgetClass, mw[o_speed_menu], main_args, main_n);
  XtAddCallback(mw[o_speed_ratio_menu], XmNactivateCallback, Options_Speed_Ratio_Callback, ss);  


  mw[o_focus_style_menu] = XmCreatePulldownMenu(mw[option_menu], "focusstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, mw[o_focus_style_menu]); k++;
  mw[o_focus_cascade_menu] = XtCreateManagedWidget(STR_Focus_style, xmCascadeButtonWidgetClass, mw[option_menu], main_args, k);

  mw[o_focus_left_menu] = XtCreateManagedWidget(STR_focus_left, xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_left_menu], XmNactivateCallback, Options_Focus_Left_Callback, ss);  

  mw[o_focus_right_menu] = XtCreateManagedWidget(STR_focus_right, xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_right_menu], XmNactivateCallback, Options_Focus_Right_Callback, ss);  

  mw[o_focus_middle_menu] = XtCreateManagedWidget(STR_focus_middle, xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_middle_menu], XmNactivateCallback, Options_Focus_Middle_Callback, ss);  

  mw[o_focus_active_menu] = XtCreateManagedWidget(STR_focus_active, xmPushButtonWidgetClass, mw[o_focus_style_menu], main_args, main_n);
  XtAddCallback(mw[o_focus_active_menu], XmNactivateCallback, Options_Focus_Active_Callback, ss);  
  activate_focus_menu(ss, zoom_focus_style(ss));

  mw[o_save_menu] = XtCreateManagedWidget(STR_Save_options, xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_save_menu], XmNactivateCallback, Options_Save_Callback, ss);
  XtVaSetValues(mw[o_save_menu], XmNmnemonic, 'a', NULL);

  mw[o_save_state_menu] = XtCreateManagedWidget(STR_Save_state, xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_save_state_menu], XmNactivateCallback, Options_Save_State_Callback, ss);

  mw[o_stats_menu] = XtCreateManagedWidget(STR_Show_stats, xmPushButtonWidgetClass, mw[option_menu], main_args, main_n);
  XtAddCallback(mw[o_stats_menu], XmNactivateCallback, Options_Stats_Callback, ss);



  /* HELP MENU */
  mw[help_menu] = XmCreatePulldownMenu(mw[menu_menu], "helpm", main_args, main_n);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, mw[help_menu]); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'H'); high_n++;
  mw[h_cascade_menu] = XtCreateManagedWidget(STR_Help, xmCascadeButtonWidgetClass, mw[menu_menu], high_args, high_n);
  XtAddCallback(mw[h_cascade_menu], XmNhelpCallback, Help_Help_Callback, ss);

  mw[h_click_for_help_menu] = XtCreateManagedWidget(STR_Click_for_help, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_click_for_help_menu], XmNactivateCallback, Help_Context_Help_Callback, ss);
  XtVaSetValues(mw[h_click_for_help_menu], XmNmnemonic, 'C', NULL);

  mw[h_about_snd_menu] = XtCreateManagedWidget(STR_Overview, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_about_snd_menu], XmNactivateCallback, Help_About_Snd_Callback, ss);
  XtVaSetValues(mw[h_about_snd_menu], XmNmnemonic, 'O', NULL);

  mw[h_fft_menu] = XtCreateManagedWidget(STR_FFT, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_fft_menu], XmNactivateCallback, Help_FFT_Callback, ss);

  mw[h_find_menu] = XtCreateManagedWidget(STR_Find, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_find_menu], XmNactivateCallback, Help_Find_Callback, ss);

  mw[h_undo_menu] = XtCreateManagedWidget(STR_Undo_and_redo, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_undo_menu], XmNactivateCallback, Help_Undo_Callback, ss);

  mw[h_sync_menu] = XtCreateManagedWidget(STR_Sync, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_sync_menu], XmNactivateCallback, Help_Sync_Callback, ss);

  mw[h_speed_menu] = XtCreateManagedWidget(STR_Speed, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_speed_menu], XmNactivateCallback, Help_Speed_Callback, ss);

  mw[h_expand_menu] = XtCreateManagedWidget(STR_Expand, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_expand_menu], XmNactivateCallback, Help_Expand_Callback, ss);

  mw[h_reverb_menu] = XtCreateManagedWidget(STR_Reverb, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_reverb_menu], XmNactivateCallback, Help_Reverb_Callback, ss);

  mw[h_contrast_menu] = XtCreateManagedWidget(STR_Contrast, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_contrast_menu], XmNactivateCallback, Help_Contrast_Callback, ss);

  mw[h_env_menu] = XtCreateManagedWidget(STR_Envelope, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_env_menu], XmNactivateCallback, Help_Env_Callback, ss);

  mw[h_marks_menu] = XtCreateManagedWidget(STR_Marks, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_marks_menu], XmNactivateCallback, Help_Marks_Callback, ss);

  mw[h_mix_menu] = XtCreateManagedWidget(STR_Mixing, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_mix_menu], XmNactivateCallback, Help_Mix_Callback, ss);

  mw[h_sound_files_menu] = XtCreateManagedWidget(STR_Formats, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_sound_files_menu], XmNactivateCallback, Help_Sound_Files_Callback, ss);

  mw[h_init_file_menu] = XtCreateManagedWidget(STR_Customization, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_init_file_menu], XmNactivateCallback, Help_Init_File_Callback, ss);

  mw[h_recording_menu] = XtCreateManagedWidget(STR_Recording, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_recording_menu], XmNactivateCallback, Help_Recording_Callback, ss);

#if HAVE_GUILE
  mw[h_clm_menu] = XtCreateManagedWidget(STR_CLM, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_clm_menu], XmNactivateCallback, Help_CLM_Callback, ss);
#endif

  mw[h_news_menu] = XtCreateManagedWidget(STR_News, xmPushButtonWidgetClass, mw[help_menu], main_args, main_n);
  XtAddCallback(mw[h_news_menu], XmNactivateCallback, Help_News_Callback, ss);

  XtVaSetValues(mw[menu_menu], XmNmenuHelpWidget, mw[h_cascade_menu], NULL);
#ifndef SND_AS_WIDGET
  XtManageChild(mw[menu_menu]);
#endif

  return(mw[menu_menu]);
}

#define MAX_MAIN_MENUS 12
static Widget added_menus[MAX_MAIN_MENUS];
static int new_menu = 4;
static Widget *added_options = NULL;
static char **added_options_names = NULL;
static char *main_menu_names[MAX_MAIN_MENUS];
static int *added_options_menus = NULL;
static int added_options_size = 0;
static int added_options_pos = 0;
static int *added_options_callb = NULL;

static char *main_menu_name(int callb)
{
  if ((callb < 0) || (added_options_menus[callb] >= MAX_MAIN_MENUS)) return(NULL); 
  switch (added_options_menus[callb])
    {
    case 0: return(STR_File); break;
    case 1: return(STR_Edit); break;
    case 2: return(STR_View); break;
    case 3: return(STR_Options); break;
    case 4: return(STR_Help); break;
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

static void SND_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  int callb, opt;
  XtVaGetValues(w, XmNuserData, &callb, NULL);
  opt = callb2option(callb);
  IF_MENU_HOOK(main_menu_name(opt), added_options_names[(opt < 0) ? 0 : opt])
    g_snd_callback(callb); /* menu option activate callback */
}

static void GHC_Callback(Widget w, XtPointer cD, XtPointer mD) 
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

static int remove_option(int which_menu, char *label)
{
  int i;
  /* TODO if label NULL, remove which_menu itself and all its children
   *         (this requires that the cascade widget also be saved, at least -- 
   *          simply unmanaging added_menus[which_menu] doesn't work).
   */
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(strcmp(label, added_options_names[i]) == 0) && 
	(added_options[i]))
      {
	XtDestroyWidget(added_options[i]);
	added_options[i] = NULL;
	added_options_menus[i] = -1;
	FREE(added_options_names[i]);
	added_options_names[i] = NULL;
	return(0);
      }
  switch (which_menu)
    {
    case 0: if (strcmp(label, STR_Open) == 0) XtUnmanageChild(mw[f_open_menu]); else
            if (strcmp(label, STR_Close) == 0) XtUnmanageChild(mw[f_close_menu]); else
            if (strcmp(label, STR_Save) == 0) XtUnmanageChild(mw[f_save_menu]); else
            if (strcmp(label, STR_Save_as) == 0) XtUnmanageChild(mw[f_save_as_menu]); else
            if (strcmp(label, STR_Revert) == 0) XtUnmanageChild(mw[f_revert_menu]); else
            if (strcmp(label, STR_Mix) == 0) XtUnmanageChild(mw[f_mix_menu]); else
            if (strcmp(label, STR_Update) == 0) XtUnmanageChild(mw[f_update_menu]); else
            if (strcmp(label, STR_New) == 0) XtUnmanageChild(mw[f_new_menu]); else
            if (strcmp(label, STR_Record) == 0) XtUnmanageChild(mw[f_record_menu]); else
            if (strcmp(label, STR_View) == 0) XtUnmanageChild(mw[f_view_menu]); else
            if (strcmp(label, STR_Print) == 0) XtUnmanageChild(mw[f_print_menu]); else
	    if (strcmp(label, STR_Exit) == 0) XtUnmanageChild(mw[f_exit_menu]); else return(-1);
            return(0);
            break;
    case 1: if (strcmp(label, STR_Undo) == 0) XtUnmanageChild(mw[e_undo_menu]); else
            if (strcmp(label, STR_Redo) == 0) XtUnmanageChild(mw[e_redo_menu]); else
            if (strcmp(label, STR_Find) == 0) XtUnmanageChild(mw[e_find_menu]); else
            if (strcmp(label, STR_Delete_Selection) == 0) XtUnmanageChild(mw[e_cut_menu]); else
            if (strcmp(label, STR_Insert_Selection) == 0) XtUnmanageChild(mw[e_paste_menu]); else
            if (strcmp(label, STR_Mix_Selection) == 0) XtUnmanageChild(mw[e_mix_menu]); else
            if (strcmp(label, STR_Play_selection) == 0) XtUnmanageChild(mw[e_play_menu]); else
            if (strcmp(label, STR_Save_Selection) == 0) XtUnmanageChild(mw[e_save_as_menu]); else
            if (strcmp(label, STR_Select_all) == 0) XtUnmanageChild(mw[e_select_all_menu]); else
            if (strcmp(label, STR_Edit_Envelope) == 0) XtUnmanageChild(mw[e_edenv_menu]); else
            if (strcmp(label, STR_Edit_Header) == 0) XtUnmanageChild(mw[e_header_menu]); else return(-1);
            return(1);
            break;
    case 2: if (strcmp(label, STR_Normalize) == 0) XtUnmanageChild(mw[v_normalize_menu]); else
            if (strcmp(label, STR_Show_controls) == 0) XtUnmanageChild(mw[v_ctrls_menu]); else
            if (strcmp(label, STR_Show_listener) == 0) XtUnmanageChild(mw[v_listener_menu]); else
            if (strcmp(label, STR_Mix_Panel) == 0) XtUnmanageChild(mw[v_mix_panel_menu]); else
            if (strcmp(label, STR_Regions) == 0) XtUnmanageChild(mw[v_region_menu]); else
            if (strcmp(label, STR_File) == 0) XtUnmanageChild(mw[v_files_menu]); else
            if (strcmp(label, STR_Color) == 0) XtUnmanageChild(mw[v_color_menu]); else
            if (strcmp(label, STR_Orientation) == 0) XtUnmanageChild(mw[v_orientation_menu]); else
            if (strcmp(label, STR_Graph_style) == 0) XtUnmanageChild(mw[v_graph_style_menu]); else
            if (strcmp(label, STR_Verbose_cursor) == 0) XtUnmanageChild(mw[v_cursor_menu]); else
            if (strcmp(label, STR_Channel_style) == 0) XtUnmanageChild(mw[v_combine_menu]); else
            if (strcmp(label, STR_Show_Y0) == 0) XtUnmanageChild(mw[v_zero_menu]); else
            if (strcmp(label, STR_X_axis_units) == 0) XtUnmanageChild(mw[v_x_axis_menu]); else
            if (strcmp(label, STR_Error_History) == 0) XtUnmanageChild(mw[v_error_history_menu]); else return(-1);
            return(3);
            break;
    case 3: if (strcmp(label, STR_Transform_Options) == 0) XtUnmanageChild(mw[o_transform_menu]); else
            if (strcmp(label, STR_Speed_style) == 0) XtUnmanageChild(mw[o_speed_menu]); else
            if (strcmp(label, STR_Focus_style) == 0) XtUnmanageChild(mw[o_focus_style_menu]); else
            if (strcmp(label, STR_Save_options) == 0) XtUnmanageChild(mw[o_save_menu]); else
            if (strcmp(label, STR_Save_state) == 0) XtUnmanageChild(mw[o_save_state_menu]); else
            if (strcmp(label, STR_Show_stats) == 0) XtUnmanageChild(mw[o_stats_menu]); else return(-1);
            return(3);
            break;
    case 4: if (strcmp(label, STR_Click_for_help) == 0) XtUnmanageChild(mw[h_click_for_help_menu]); else
            if (strcmp(label, STR_Overview) == 0) XtUnmanageChild(mw[h_about_snd_menu]); else
            if (strcmp(label, STR_FFT) == 0) XtUnmanageChild(mw[h_fft_menu]); else
            if (strcmp(label, STR_Find) == 0) XtUnmanageChild(mw[h_find_menu]); else
            if (strcmp(label, STR_Undo_and_redo) == 0) XtUnmanageChild(mw[h_undo_menu]); else
            if (strcmp(label, STR_Sync) == 0) XtUnmanageChild(mw[h_sync_menu]); else
            if (strcmp(label, STR_Speed) == 0) XtUnmanageChild(mw[h_speed_menu]); else
            if (strcmp(label, STR_Expand) == 0) XtUnmanageChild(mw[h_expand_menu]); else
            if (strcmp(label, STR_Reverb) == 0) XtUnmanageChild(mw[h_reverb_menu]); else
            if (strcmp(label, STR_Contrast) == 0) XtUnmanageChild(mw[h_contrast_menu]); else
            if (strcmp(label, STR_Envelope) == 0) XtUnmanageChild(mw[h_env_menu]); else
            if (strcmp(label, STR_Marks) == 0) XtUnmanageChild(mw[h_marks_menu]); else
            if (strcmp(label, STR_Mixing) == 0) XtUnmanageChild(mw[h_mix_menu]); else
            if (strcmp(label, STR_Format) == 0) XtUnmanageChild(mw[h_sound_files_menu]); else
            if (strcmp(label, STR_Customization) == 0) XtUnmanageChild(mw[h_init_file_menu]); else
            if (strcmp(label, STR_Recording) == 0) XtUnmanageChild(mw[h_recording_menu]); else
            if (strcmp(label, STR_CLM) == 0) XtUnmanageChild(mw[h_clm_menu]); else
            if (strcmp(label, STR_News) == 0) XtUnmanageChild(mw[h_news_menu]); else return(-1); 
            return(4);
            break;
    }
  return(-1);
}

int gh_change_menu_label(int which_menu, char *old_label, char *new_label)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(strcmp(old_label, added_options_names[i]) == 0) && 
	(added_options[i]))
      {
	set_button_label(added_options[i], new_label);
	if (added_options_names[i]) FREE(added_options_names[i]);
	added_options_names[i] = copy_string(new_label);
	return(0);
      }
  return(-1);
}

int gh_set_menu_sensitive(int which_menu, char *old_label, int on)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(strcmp(old_label, added_options_names[i]) == 0) && 
	(added_options[i]))
      {
	set_sensitive(added_options[i], on);
	return(0);
      }
  return(-1);
}

int gh_menu_is_sensitive(int which_menu, char *old_label)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(strcmp(old_label, added_options_names[i]) == 0) && 
	(added_options[i]))
      return(is_sensitive(added_options[i]));
  return(0);
}

int gh_add_to_main_menu(snd_state *ss, char *label, int slot)
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
  if (slot >= 0) XtAddCallback(cas, XmNcascadingCallback, GHC_Callback, (XtPointer)slot);

  if (auto_resize(ss)) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, TRUE, NULL);

  new_menu++;

  if (new_menu < MAX_MAIN_MENUS)
    {
      added_menus[new_menu] = m;
      main_menu_names[new_menu] = copy_string(label);
      return(new_menu);
    }
  else return(-1);
}

int gh_add_to_menu(snd_state *ss, int which_menu, char *label, int callb)
{
  Widget m, menw;
  static Arg args[12];
  int n;
  switch (which_menu)
    {
    case 0: menw = mw[file_menu]; break;
    case 1: menw = mw[edit_menu]; break;
    case 2: menw = mw[view_menu]; break;
    case 3: menw = mw[option_menu]; break;
    case 4: menw = mw[help_menu]; break;
    default: 
      if (which_menu < MAX_MAIN_MENUS)
	menw = added_menus[which_menu]; 
      else return(-1);
      break;
    }
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNuserData, callb); n++;
  m = XtCreateManagedWidget(label, xmPushButtonWidgetClass, menw, args, n);
  XtAddCallback(m, XmNactivateCallback, SND_Callback, ss);
  add_option(m, which_menu, label, callb);
  return(0);
}

int gh_remove_from_menu(int which_menu, char *label)
{
  return(remove_option(which_menu, label));
}



/* -------------------------------- POPUP MENU -------------------------------- */

static void Popup_Play_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_state *ss = (snd_state *)cD;
  snd_info *sp;
  IF_MENU_HOOK("Popup", STR_Play)
    {
      sp = any_selected_sound(ss);
      if (sp)
	{
	  play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND);
	  set_play_button(sp, 1);
	}
    }
}

static void Popup_Save_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK("Popup", STR_Save) save_edits(any_selected_sound((snd_state *)cD), NULL);
}

static void Popup_Undo_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", STR_Undo) undo_edit_with_sync(current_channel(ss), 1);
}

static void Popup_Redo_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", STR_Redo) redo_edit_with_sync(current_channel(ss), 1);
}

static void Popup_Normalize_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  IF_MENU_HOOK("Popup", STR_Normalize) normalize_all_sounds((snd_state *)cD);
}

static void Popup_Info_Callback(Widget w, XtPointer cD, XtPointer mD) 
{
  snd_info *sp;
  IF_MENU_HOOK("Popup", STR_Info)
    {
      sp = selected_sound((snd_state *)cD);
      if (sp) display_info(sp);
    }
}

static void Post_Popup_Menu(Widget w, XtPointer cD, XEvent *event, Boolean *flag)
{
  if (event->xbutton.button == BUTTON_3)
    {
      XmMenuPosition(popup_menu, (XButtonPressedEvent *)event);
      XtManageChild(popup_menu);
    }
}

void create_popup_menu(snd_state *ss)
{
  /* make it a child of the main window */
  Widget mainp;
  Arg args[20];
  int n;
  if (!popup_menu)
    {
      n = 0;
      if (!ss->using_schemes) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      mainp = MAIN_PANE(ss);
      popup_menu = XmCreatePopupMenu(mainp, "popup-menu", args, n);
      XtAddEventHandler(mainp, ButtonPressMask, FALSE, Post_Popup_Menu, popup_menu);

      popup_children[W_pop_menu] = XtCreateManagedWidget("snd", xmLabelWidgetClass, popup_menu, args, n);
      popup_children[W_pop_sep] = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, popup_menu, args, n);
      popup_children[W_pop_play] = XtCreateManagedWidget(STR_Play, xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_children[W_pop_play], XmNactivateCallback, Popup_Play_Callback, ss);
      popup_children[W_pop_undo] = XtCreateManagedWidget(STR_Undo, xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_undo], XmNsensitive, FALSE, NULL);
      XtAddCallback(popup_children[W_pop_undo], XmNactivateCallback, Popup_Undo_Callback, ss);
      popup_children[W_pop_redo] = XtCreateManagedWidget(STR_Redo, xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_redo], XmNsensitive, FALSE, NULL);
      XtAddCallback(popup_children[W_pop_redo], XmNactivateCallback, Popup_Redo_Callback, ss);
      popup_children[W_pop_save] = XtCreateManagedWidget(STR_Save, xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_children[W_pop_save], XmNactivateCallback, Popup_Save_Callback, ss);
      popup_children[W_pop_normalize] = XtCreateManagedWidget(STR_Normalize, xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_normalize], XmNsensitive, FALSE, NULL);
      XtAddCallback(popup_children[W_pop_normalize], XmNactivateCallback, Popup_Normalize_Callback, ss);
      popup_children[W_pop_info] = XtCreateManagedWidget(STR_Info, xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_children[W_pop_info], XmNsensitive, FALSE, NULL);
      XtAddCallback(popup_children[W_pop_info], XmNactivateCallback, Popup_Info_Callback, ss);
    }
}

void add_popup_handler(Widget w)
{
  XtAddEventHandler(w, ButtonPressMask, FALSE, Post_Popup_Menu, popup_menu);
}

#if HAVE_GUILE

static SCM g_menu_widgets(void)
{
  return(scm_cons(SND_WRAP(mw[menu_menu]),
	  scm_cons(SND_WRAP(mw[f_cascade_menu]),
           scm_cons(SND_WRAP(mw[e_cascade_menu]),
            scm_cons(SND_WRAP(mw[v_cascade_menu]),
             scm_cons(SND_WRAP(mw[o_cascade_menu]),
              scm_cons(SND_WRAP(mw[h_cascade_menu]),
		       SCM_EOL)))))));
}


static SCM g_test_menus(void) 
{
  int i;
  snd_state *ss;
  ss = get_global_state();
#ifndef SGI
  for (i = 0; i < NUM_MENU_WIDGETS; i++)
    if ((mw[i]) && (XmIsPushButton(mw[i])) && (XtIsSensitive(mw[i])) &&
	(i != f_exit_menu) && (i != f_save_menu) && (i != f_close_menu) &&
	(i != f_new_menu) && (i != h_click_for_help_menu) && (i != v_mix_panel_menu))
      XtCallCallbacks(mw[i], XmNactivateCallback, (void *)ss);
#endif
  for (i = 0; i < added_options_pos; i++)
    if ((added_options[i]) && 
	(XmIsPushButton(mw[i])) &&
	(XtIsSensitive(mw[i])))
      XtCallCallbacks(added_options[i], XmNactivateCallback, (void *)ss);
  dismiss_all_dialogs(ss);
  return(SCM_BOOL_F);
}

void g_init_gxmenu(SCM local_doc)
{
#if HAVE_HOOKS

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

  menu_hook = MAKE_HOOK(S_menu_hook, 2, H_menu_hook);
#endif
  gh_new_procedure("test-menus", SCM_FNC g_test_menus, 0, 0, 0);

  DEFINE_PROC(gh_new_procedure(S_menu_widgets, SCM_FNC g_menu_widgets, 0, 0, 0), "returns top level menu widgets");
}
#endif
