#include "snd.h"

static gint middle_button_press (GtkWidget *widget, GdkEvent *bevent, gpointer data);

enum {menu_menu,
        file_menu, f_cascade_menu,
          f_open_menu, f_close_menu, f_save_menu, f_save_as_menu, f_revert_menu, f_exit_menu, f_new_menu,
          f_view_menu, f_print_menu, f_mix_menu, f_update_menu, f_record_menu, f_sep_menu,
        edit_menu, e_cascade_menu,
          e_cut_menu, e_paste_menu, e_mix_menu, e_play_menu, e_save_as_menu, e_undo_menu,
          e_redo_menu, e_find_menu, e_edenv_menu, e_header_menu,
          e_select_all_menu,
          e_select_sep_menu, e_edit_sep_menu,
        help_menu, h_cascade_menu,
          h_about_snd_menu, h_fft_menu, h_find_menu, h_undo_menu, h_sync_menu, h_speed_menu,
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
#if 0
          v_equalize_panes_menu, 
#endif
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

#define NUM_MENU_WIDGETS 98
static GtkWidget *mw[NUM_MENU_WIDGETS];

enum {W_pop_play, W_pop_undo, W_pop_redo, W_pop_save, W_pop_info};
#define NUM_POPUP_CHILDREN 6
static GtkWidget *popup_menu = NULL;
static GtkWidget *popup_children[NUM_POPUP_CHILDREN];

int popup_menu_exists(void) {return(popup_menu != NULL);}

GtkWidget *file_open_menu(void) {return(mw[f_open_menu]);}
GtkWidget *file_close_menu(void) {return(mw[f_close_menu]);}
GtkWidget *file_save_menu(void) {return(mw[f_save_menu]);}
GtkWidget *file_save_as_menu(void) {return(mw[f_save_as_menu]);}
GtkWidget *file_print_menu(void) {return(mw[f_print_menu]);}
GtkWidget *file_revert_menu(void) {return(mw[f_revert_menu]);}
GtkWidget *file_update_menu(void) {return(mw[f_update_menu]);}
GtkWidget *file_mix_menu(void) {return(mw[f_mix_menu]);}
GtkWidget *file_view_menu(void) {return(mw[f_view_menu]);}
GtkWidget *file_new_menu(void) {return(mw[f_new_menu]);}

GtkWidget *edit_cut_menu(void) {return(mw[e_cut_menu]);}
GtkWidget *edit_paste_menu(void) {return(mw[e_paste_menu]);}
GtkWidget *edit_mix_menu(void) {return(mw[e_mix_menu]);}
GtkWidget *edit_play_menu(void) {return(mw[e_play_menu]);}
GtkWidget *edit_save_as_menu(void) {return(mw[e_save_as_menu]);}
GtkWidget *edit_undo_menu(void) {return(mw[e_undo_menu]);}
GtkWidget *edit_redo_menu(void) {return(mw[e_redo_menu]);}
GtkWidget *edit_find_menu(void) {return(mw[e_find_menu]);}
GtkWidget *edit_select_all_menu(void) {return(mw[e_select_all_menu]);}
GtkWidget *edit_header_menu(void) {return(mw[e_header_menu]);}

/* GtkWidget *view_equalize_panes_menu(void) {return(mw[v_equalize_panes_menu]);} */
GtkWidget *view_equalize_panes_menu(void) {return(NULL);}

GtkWidget *view_mix_panel_menu(void) {return(mw[v_mix_panel_menu]);}
GtkWidget *view_region_menu(void) {return(mw[v_region_menu]);}
GtkWidget *view_combine_separate_menu(void) {return(mw[v_combine_separate_menu]);}
GtkWidget *view_combine_combined_menu(void) {return(mw[v_combine_combined_menu]);}
GtkWidget *view_combine_superimposed_menu(void) {return(mw[v_combine_superimposed_menu]);}
GtkWidget *view_lines_menu(void) {return(mw[v_lines_menu]);}
GtkWidget *view_dots_menu(void) {return(mw[v_dots_menu]);}
GtkWidget *view_dots_and_lines_menu(void) {return(mw[v_dots_and_lines_menu]);}
GtkWidget *view_filled_menu(void) {return(mw[v_filled_menu]);}
GtkWidget *view_lollipops_menu(void) {return(mw[v_lollipops_menu]);}
GtkWidget *view_zero_menu(void) {return(mw[v_zero_menu]);}
GtkWidget *view_ctrls_menu(void) {return(mw[v_ctrls_menu]);}
GtkWidget *view_listener_menu(void) {return(mw[v_listener_menu]);}
GtkWidget *view_cursor_menu(void) {return(mw[v_cursor_menu]);}
GtkWidget *view_x_axis_seconds_menu(void) {return(mw[v_x_axis_seconds_menu]);}
GtkWidget *view_x_axis_beats_menu(void) {return(mw[v_x_axis_beats_menu]);}
GtkWidget *view_x_axis_samples_menu(void) {return(mw[v_x_axis_samples_menu]);}
GtkWidget *view_x_axis_percentage_menu(void) {return(mw[v_x_axis_percentage_menu]);}

GtkWidget *options_save_state_menu(void) {return(mw[o_save_state_menu]);}
GtkWidget *options_stats_menu(void) {return(mw[o_stats_menu]);}
GtkWidget *options_focus_left_menu(void) {return(mw[o_focus_left_menu]);}
GtkWidget *options_focus_right_menu(void) {return(mw[o_focus_right_menu]);}
GtkWidget *options_focus_middle_menu(void) {return(mw[o_focus_middle_menu]);}
GtkWidget *options_focus_active_menu(void) {return(mw[o_focus_active_menu]);}
GtkWidget *options_speed_ratio_menu(void) {return(mw[o_speed_ratio_menu]);}
GtkWidget *options_speed_float_menu(void) {return(mw[o_speed_float_menu]);}
GtkWidget *options_speed_semitone_menu(void) {return(mw[o_speed_semitone_menu]);}

GtkWidget *popup_play_menu(void) {return(popup_children[W_pop_play]);}
GtkWidget *popup_undo_menu(void) {return(popup_children[W_pop_undo]);}
GtkWidget *popup_redo_menu(void) {return(popup_children[W_pop_redo]);}
GtkWidget *popup_save_menu(void) {return(popup_children[W_pop_save]);}
GtkWidget *popup_equalize_panes_menu(void) {return(NULL);}
GtkWidget *popup_info_menu(void) {return(popup_children[W_pop_info]);}

void set_menu_label(GtkWidget *w, const char *label) {if (w) set_button_label(w, label);}

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

static void file_open_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Open) make_open_file_dialog((snd_state *)cD, FALSE, TRUE);
}

static void file_view_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_View) make_open_file_dialog((snd_state *)cD, TRUE, TRUE);
}

static void file_new_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_New) new_file_from_menu((snd_state *)cD);
}

static void file_record_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Record) snd_record_file((snd_state *)cD);
}

static void file_close_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Close) close_file_from_menu((snd_state *)cD);
}

static void file_save_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Save) save_file_from_menu((snd_state *)cD);
}

static void file_update_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Update) update_file_from_menu((snd_state *)cD);
}

static void file_save_as_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Save_as) make_file_save_as_dialog((snd_state *)cD);
}

static void file_revert_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Revert) revert_file_from_menu((snd_state *)cD);
}

static void file_exit_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Exit) exit_from_menu((snd_state *)cD);
}

static void file_mix_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Mix) make_mix_file_dialog((snd_state *)cD, TRUE);
}

static void file_print_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_File, STR_Print) file_print_callback(w, cD);
}


/* -------------------------------- EDIT MENU -------------------------------- */


static void edit_mix_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Mix_Selection) mix_selection_from_menu((snd_state *)cD);
}

static void edit_envelope_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Edit_Envelope) create_envelope_editor((snd_state *)cD);
}

static void edit_cut_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Delete_Selection) delete_selection("Edit: Cut", UPDATE_DISPLAY);
}

static void edit_paste_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Insert_Selection) paste_selection_from_menu((snd_state *)cD);
}

static void edit_save_as_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Save_Selection) make_edit_save_as_dialog((snd_state *)cD);
}

static void edit_select_all_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Select_all) select_all(current_channel((snd_state *)cD));
}

static void edit_undo_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Undo) undo_edit_with_sync(current_channel((snd_state *)cD), 1);
}

static void edit_redo_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Edit, STR_Redo) redo_edit_with_sync(current_channel((snd_state *)cD), 1);
}

static int selection_play_stop = 0;

static void edit_play_callback(GtkWidget *w, gpointer cD) 
{
  if (selection_play_stop)
    {
      stop_playing_all_sounds();
    }
  else
    {
      IF_MENU_HOOK(STR_Edit, STR_Play_selection) 
	{
	  set_menu_label(edit_play_menu(), STR_Stop);
	  selection_play_stop = 1;
	  play_selection(IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "play selection", 0);
	}
    }
}

void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu(), STR_Play_selection);
  selection_play_stop = 0;
}

static void edit_header_callback_1(GtkWidget *w, gpointer cD)
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
static void edit_find_callback_1(GtkWidget *w, gpointer cD)
{
  IF_MENU_HOOK(STR_Edit, STR_Find) edit_find_callback(w, cD);
}
#endif


/* -------------------------------- VIEW MENU -------------------------------- */

static void view_separate_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_separate) set_channel_style((snd_state *)cD, CHANNELS_SEPARATE);
}

static void view_combined_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_combined) set_channel_style((snd_state *)cD, CHANNELS_COMBINED);
}

static void view_superimposed_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_superimposed) set_channel_style((snd_state *)cD, CHANNELS_SUPERIMPOSED);
}

static void view_dots_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_dots) set_graph_style((snd_state *)cD, GRAPH_DOTS);
}

static void view_lines_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_lines) set_graph_style((snd_state *)cD, GRAPH_LINES);
}

static void view_filled_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_filled) set_graph_style((snd_state *)cD, GRAPH_FILLED);
}

static void view_dots_and_lines_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_dots_and_lines) set_graph_style((snd_state *)cD, GRAPH_DOTS_AND_LINES);
}

static void view_lollipops_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_lollipops) set_graph_style((snd_state *)cD, GRAPH_LOLLIPOPS);
}

static void view_zero_callback(GtkWidget *w, gpointer cD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Show_Y0) set_show_y_zero(ss, (!(show_y_zero(ss))));
}

static void view_cursor_callback(GtkWidget *w, gpointer cD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Verbose_cursor) set_verbose_cursor(ss, (!(verbose_cursor(ss))));
}

static void view_ctrls_callback(GtkWidget *w, gpointer cD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Show_controls) 
    {
      if (ss->ctrls_height < 100) 
	show_controls(ss); 
      else hide_controls(ss); /* snd-xmain.c */
    }
}

#if HAVE_EXTENSION_LANGUAGE
static void view_listener_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_View, STR_Show_listener) handle_listener((snd_state *)cD, (!(ss->listening)));
}
#endif

static void view_mix_panel_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_Mix_Panel) make_mix_panel((snd_state *)cD);
}

static void view_error_history_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_Error_History) show_snd_errors((snd_state *)cD);
}

static void view_region_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_Regions) view_region_callback(w, cD);
}

static void view_orientation_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_Orientation) view_orientation_callback(w, cD);
}

static void view_color_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_Color) view_color_callback(w, cD);
}

static void view_files_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_View, STR_Files) view_files_callback(w, cD);
}


/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_transform_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Options, STR_Transform_Options) fire_up_transform_dialog((snd_state *)cD, TRUE);
}

#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Options, STR_Save_options) save_options_from_menu((snd_state *)cD);
}
#endif

static void options_focus_right_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_right) activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_RIGHT);
}

static void options_focus_left_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_left) activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_LEFT);
}

static void options_focus_middle_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_middle) activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_MIDDLE);
}

static void options_focus_active_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_focus_active) activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_ACTIVE);
}

static void options_speed_float_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_float) activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_FLOAT);
}

static void options_speed_ratio_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_ratio) activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_RATIO);
}

static void options_speed_semitone_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK(STR_Options, STR_semitone) activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_SEMITONE);
}

static void options_x_axis_seconds_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Options, STR_seconds) set_x_axis_style((snd_state *)cD, X_AXIS_IN_SECONDS);
}

static void options_x_axis_beats_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Options, STR_beats) set_x_axis_style((snd_state *)cD, X_AXIS_IN_BEATS);
}

static void options_x_axis_samples_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Options, STR_samples) set_x_axis_style((snd_state *)cD, X_AXIS_IN_SAMPLES);
}

static void options_x_axis_percentage_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Options, STR_percentage) set_x_axis_style((snd_state *)cD, X_AXIS_AS_PERCENTAGE);
}

#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK(STR_Options, STR_Save_state) save_state_from_menu((snd_state *)cD);
}
#endif

static void options_stats_callback(GtkWidget *w, gpointer cD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK(STR_Options, STR_Show_stats) set_show_usage_stats(ss, (!(show_usage_stats(ss))));
}


/* -------------------------------- HELP MENU -------------------------------- */

static void help_about_snd_callback(GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Overview) about_snd_help((snd_state *)cD);}
static void help_fft_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_FFT) fft_help((snd_state *)cD);}
static void help_find_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Find) find_help((snd_state *)cD);}
static void help_undo_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Undo_and_redo) undo_help((snd_state *)cD);}
static void help_sync_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Sync) sync_help((snd_state *)cD);}
static void help_speed_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Speed) speed_help((snd_state *)cD);}
static void help_expand_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Expand) expand_help((snd_state *)cD);}
static void help_reverb_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Reverb) reverb_help((snd_state *)cD);}
static void help_contrast_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Contrast) contrast_help((snd_state *)cD);}
static void help_env_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Envelope) env_help((snd_state *)cD);}
static void help_marks_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Marks) marks_help((snd_state *)cD);}
static void help_mix_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Mixing) mix_help((snd_state *)cD);}
static void help_sound_files_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Formats) sound_files_help((snd_state *)cD);}
static void help_init_file_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Customization) init_file_help((snd_state *)cD);}
static void help_recording_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_Recording) recording_help((snd_state *)cD);}

static void help_clm_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_CLM) clm_help((snd_state *)cD);}

static void help_news_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK(STR_Help, STR_News) news_help((snd_state *)cD);}


/* -------------------------------- MAIN MENU -------------------------------- */

GtkWidget *get_menubar(void) {return(mw[menu_menu]);}

GtkWidget *add_menu(snd_state *ss)
{
  /* this mainly passes the global data pointer (ss) to all the menu-related callbacks */
#ifndef SND_AS_WIDGET
  GtkAccelGroup *accel_group;

  accel_group = gtk_accel_group_new();
  gtk_window_add_accel_group(GTK_WINDOW(MAIN_SHELL(ss)), accel_group);
#endif

  mw[menu_menu] = gtk_menu_bar_new();
  set_background(mw[menu_menu], (ss->sgx)->highlight_color);
  gtk_box_pack_start(GTK_BOX(MAIN_PANE(ss)), mw[menu_menu], FALSE, TRUE, 0);
  gtk_widget_show(mw[menu_menu]);


  /* FILE MENU */
  mw[file_menu] = gtk_menu_item_new_with_label(STR_File);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]), mw[file_menu]);
  set_background(mw[file_menu], (ss->sgx)->highlight_color);
  gtk_widget_show(mw[file_menu]);

  mw[f_cascade_menu] = gtk_menu_new();
  set_background(mw[f_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[file_menu]), mw[f_cascade_menu]);

  mw[f_open_menu] = gtk_menu_item_new_with_label(STR_Open);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_open_menu]);
  set_background(mw[f_open_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_open_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_open_menu]), "activate", GTK_SIGNAL_FUNC(file_open_callback), (gpointer)ss);

  mw[f_close_menu] = gtk_menu_item_new_with_label(STR_Close);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_close_menu]);
  set_background(mw[f_close_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_close_menu]);
  set_sensitive(mw[f_close_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_close_menu]), "activate", GTK_SIGNAL_FUNC(file_close_callback), (gpointer)ss);
  
  mw[f_save_menu] = gtk_menu_item_new_with_label(STR_Save);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_save_menu]);
  set_background(mw[f_save_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_save_menu]);
  set_sensitive(mw[f_save_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_save_menu]), "activate", GTK_SIGNAL_FUNC(file_save_callback), (gpointer)ss);
  
  mw[f_save_as_menu] = gtk_menu_item_new_with_label(STR_Save_as);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_save_as_menu]);
  set_background(mw[f_save_as_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_save_as_menu]);
  set_sensitive(mw[f_save_as_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_save_as_menu]), "activate", GTK_SIGNAL_FUNC(file_save_as_callback), (gpointer)ss);
  
  mw[f_revert_menu] = gtk_menu_item_new_with_label(STR_Revert);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_revert_menu]);
  set_background(mw[f_revert_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_revert_menu]);
  set_sensitive(mw[f_revert_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_revert_menu]), "activate", GTK_SIGNAL_FUNC(file_revert_callback), (gpointer)ss);
  
  mw[f_mix_menu] = gtk_menu_item_new_with_label(STR_Mix);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_mix_menu]);
  set_background(mw[f_mix_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_mix_menu]);
  set_sensitive(mw[f_mix_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_mix_menu]), "activate", GTK_SIGNAL_FUNC(file_mix_callback_1), (gpointer)ss);

  mw[f_update_menu] = gtk_menu_item_new_with_label(STR_Update);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_update_menu]);
  set_background(mw[f_update_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_update_menu]);
  set_sensitive(mw[f_update_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_update_menu]), "activate", GTK_SIGNAL_FUNC(file_update_callback), (gpointer)ss);

  mw[f_new_menu] = gtk_menu_item_new_with_label(STR_New);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_new_menu]);
  set_background(mw[f_new_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_new_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_new_menu]), "activate", GTK_SIGNAL_FUNC(file_new_callback), (gpointer)ss);

  mw[f_record_menu] = gtk_menu_item_new_with_label(STR_Record);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_record_menu]);
  set_background(mw[f_record_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_record_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_record_menu]), "activate", GTK_SIGNAL_FUNC(file_record_callback), (gpointer)ss);

  mw[f_view_menu] = gtk_menu_item_new_with_label(STR_View);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_view_menu]);
  set_background(mw[f_view_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_view_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_view_menu]), "activate", GTK_SIGNAL_FUNC(file_view_callback), (gpointer)ss);

  mw[f_print_menu] = gtk_menu_item_new_with_label(STR_Print);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_print_menu]);
  set_background(mw[f_print_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_print_menu]);
  set_sensitive(mw[f_print_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_print_menu]), "activate", GTK_SIGNAL_FUNC(file_print_callback_1), (gpointer)ss);

  mw[f_sep_menu] = gtk_menu_item_new();
  set_background(mw[f_sep_menu], (ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_sep_menu]);
  gtk_widget_show(mw[f_sep_menu]);

  mw[f_exit_menu] = gtk_menu_item_new_with_label(STR_Exit);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]), mw[f_exit_menu]);
  set_background(mw[f_exit_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[f_exit_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_exit_menu]), "activate", GTK_SIGNAL_FUNC(file_exit_callback), (gpointer)ss);

  /* EDIT MENU */
  mw[edit_menu] = gtk_menu_item_new_with_label(STR_Edit);
  set_background(mw[edit_menu], (ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]), mw[edit_menu]);
  gtk_widget_show(mw[edit_menu]);

  mw[e_cascade_menu] = gtk_menu_new();
  set_background(mw[e_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[edit_menu]), mw[e_cascade_menu]);
  
  mw[e_undo_menu] = gtk_menu_item_new_with_label(STR_Undo);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_undo_menu]);
  set_background(mw[e_undo_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_undo_menu]);
  set_sensitive(mw[e_undo_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_undo_menu]), "activate", GTK_SIGNAL_FUNC(edit_undo_callback), (gpointer)ss);

  mw[e_redo_menu] = gtk_menu_item_new_with_label(STR_Redo);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_redo_menu]);
  set_background(mw[e_redo_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_redo_menu]);
  set_sensitive(mw[e_redo_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_redo_menu]), "activate", GTK_SIGNAL_FUNC(edit_redo_callback), (gpointer)ss);

#if HAVE_EXTENSION_LANGUAGE
  mw[e_find_menu] = gtk_menu_item_new_with_label(STR_Find);
  set_background(mw[e_find_menu], (ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_find_menu]);
  gtk_widget_show(mw[e_find_menu]);
  set_sensitive(mw[e_find_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_find_menu]), "activate", GTK_SIGNAL_FUNC(edit_find_callback_1), (gpointer)ss);
#endif

  mw[e_select_sep_menu] = gtk_menu_item_new();
  set_background(mw[e_select_sep_menu], (ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_select_sep_menu]);
  gtk_widget_show(mw[e_select_sep_menu]);

  mw[e_cut_menu] = gtk_menu_item_new_with_label(STR_Delete_Selection);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_cut_menu]);
  set_background(mw[e_cut_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_cut_menu]);
  set_sensitive(mw[e_cut_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_cut_menu]), "activate", GTK_SIGNAL_FUNC(edit_cut_callback), (gpointer)ss);

  mw[e_paste_menu] = gtk_menu_item_new_with_label(STR_Insert_Selection);
  set_background(mw[e_paste_menu], (ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_paste_menu]);
  gtk_widget_show(mw[e_paste_menu]);
  set_sensitive(mw[e_paste_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_paste_menu]), "activate", GTK_SIGNAL_FUNC(edit_paste_callback), (gpointer)ss);

  mw[e_mix_menu] = gtk_menu_item_new_with_label(STR_Mix_Selection);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_mix_menu]);
  set_background(mw[e_mix_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_mix_menu]);
  set_sensitive(mw[e_mix_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_mix_menu]), "activate", GTK_SIGNAL_FUNC(edit_mix_callback), (gpointer)ss);

  mw[e_play_menu] = gtk_menu_item_new_with_label(STR_Play_selection);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_play_menu]);
  set_background(mw[e_play_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_play_menu]);
  set_sensitive(mw[e_play_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_play_menu]), "activate", GTK_SIGNAL_FUNC(edit_play_callback), (gpointer)ss);

  mw[e_save_as_menu] = gtk_menu_item_new_with_label(STR_Save_Selection);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_save_as_menu]);
  set_background(mw[e_save_as_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_save_as_menu]);
  set_sensitive(mw[e_save_as_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_save_as_menu]), "activate", GTK_SIGNAL_FUNC(edit_save_as_callback), (gpointer)ss);

  mw[e_select_all_menu] = gtk_menu_item_new_with_label(STR_Select_all);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_select_all_menu]);
  set_background(mw[e_select_all_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_select_all_menu]);
  set_sensitive(mw[e_select_all_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_select_all_menu]), "activate", GTK_SIGNAL_FUNC(edit_select_all_callback), (gpointer)ss);

  mw[e_edit_sep_menu] = gtk_menu_item_new();
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_edit_sep_menu]);
  set_background(mw[e_edit_sep_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_edit_sep_menu]);

  mw[e_edenv_menu] = gtk_menu_item_new_with_label(STR_Edit_Envelope);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_edenv_menu]);
  set_background(mw[e_edenv_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_edenv_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[e_edenv_menu]), "activate", GTK_SIGNAL_FUNC(edit_envelope_callback), (gpointer)ss);

  mw[e_header_menu] = gtk_menu_item_new_with_label(STR_Edit_Header);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]), mw[e_header_menu]);
  set_background(mw[e_header_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[e_header_menu]);
  set_sensitive(mw[e_header_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_header_menu]), "activate", GTK_SIGNAL_FUNC(edit_header_callback_1), (gpointer)ss);


  /* VIEW MENU */
  mw[view_menu] = gtk_menu_item_new_with_label(STR_View);
  set_background(mw[view_menu], (ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]), mw[view_menu]);
  gtk_widget_show(mw[view_menu]);

  mw[v_cascade_menu] = gtk_menu_new();
  set_background(mw[v_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[view_menu]), mw[v_cascade_menu]);

  mw[v_ctrls_menu] = gtk_menu_item_new_with_label(STR_Show_controls);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_ctrls_menu]);
  set_background(mw[v_ctrls_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_ctrls_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_ctrls_menu]), "activate", GTK_SIGNAL_FUNC(view_ctrls_callback), (gpointer)ss);

#if 0
  mw[v_equalize_panes_menu] = gtk_menu_item_new_with_label(STR_Equalize_Panes);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_equalize_panes_menu]);
  set_background(mw[v_equalize_panes_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_equalize_panes_menu]);
  set_sensitive(mw[v_equalize_panes_menu], FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[v_equalize_panes_menu]), "activate", GTK_SIGNAL_FUNC(view_equalize_panes_callback), (gpointer)ss);
#endif

#if HAVE_EXTENSION_LANGUAGE
  mw[v_listener_menu] = gtk_menu_item_new_with_label(STR_Open_listener);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_listener_menu]);
  set_background(mw[v_listener_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_listener_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_listener_menu]), "activate", GTK_SIGNAL_FUNC(view_listener_callback), (gpointer)ss);
#endif

  mw[v_mix_panel_menu] = gtk_menu_item_new_with_label(STR_Mix_Panel);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_mix_panel_menu]);
  set_background(mw[v_mix_panel_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_mix_panel_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_mix_panel_menu]), "activate", GTK_SIGNAL_FUNC(view_mix_panel_callback), (gpointer)ss);
  set_sensitive(mw[v_mix_panel_menu], FALSE);

  mw[v_region_menu] = gtk_menu_item_new_with_label(STR_Regions);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_region_menu]);
  set_background(mw[v_region_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_region_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_region_menu]), "activate", GTK_SIGNAL_FUNC(view_region_callback_1), (gpointer)ss);
  set_sensitive(mw[v_region_menu], FALSE);

  mw[v_files_menu] = gtk_menu_item_new_with_label(STR_Files);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_files_menu]);
  set_background(mw[v_files_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_files_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_files_menu]), "activate", GTK_SIGNAL_FUNC(view_files_callback_1), (gpointer)ss);

  mw[v_color_menu] = gtk_menu_item_new_with_label(STR_Color);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_color_menu]);
  set_background(mw[v_color_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_color_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_color_menu]), "activate", GTK_SIGNAL_FUNC(view_color_callback_1), (gpointer)ss);

  mw[v_orientation_menu] = gtk_menu_item_new_with_label(STR_Orientation);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_orientation_menu]);
  set_background(mw[v_orientation_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_orientation_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_orientation_menu]), "activate", GTK_SIGNAL_FUNC(view_orientation_callback_1), (gpointer)ss);

  mw[v_sep2_menu] = gtk_menu_item_new();
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_sep2_menu]);
  set_background(mw[v_sep2_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_sep2_menu]);



  mw[v_graph_style_menu] = gtk_menu_item_new_with_label(STR_Graph_style);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_graph_style_menu]);
  set_background(mw[v_graph_style_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_graph_style_menu]);

  mw[v_graph_style_cascade_menu] = gtk_menu_new();
  set_background(mw[v_graph_style_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_graph_style_menu]), mw[v_graph_style_cascade_menu]);

  mw[v_lines_menu] = gtk_menu_item_new_with_label(STR_lines);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]), mw[v_lines_menu]);
  set_background(mw[v_lines_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_lines_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_lines_menu]), "activate", GTK_SIGNAL_FUNC(view_lines_callback), (gpointer)ss);
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(mw[v_lines_menu], FALSE);

  mw[v_dots_menu] = gtk_menu_item_new_with_label(STR_dots);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]), mw[v_dots_menu]);
  set_background(mw[v_dots_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_dots_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_dots_menu]), "activate", GTK_SIGNAL_FUNC(view_dots_callback), (gpointer)ss);
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(mw[v_dots_menu], FALSE);

  mw[v_filled_menu] = gtk_menu_item_new_with_label(STR_filled);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]), mw[v_filled_menu]);
  set_background(mw[v_filled_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_filled_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_filled_menu]), "activate", GTK_SIGNAL_FUNC(view_filled_callback), (gpointer)ss);
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(mw[v_filled_menu], FALSE);

  mw[v_dots_and_lines_menu] = gtk_menu_item_new_with_label(STR_dots_and_lines);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]), mw[v_dots_and_lines_menu]);
  set_background(mw[v_dots_and_lines_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_dots_and_lines_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_dots_and_lines_menu]), "activate", GTK_SIGNAL_FUNC(view_dots_and_lines_callback), (gpointer)ss);
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(mw[v_dots_and_lines_menu], FALSE);

  mw[v_lollipops_menu] = gtk_menu_item_new_with_label(STR_lollipops);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]), mw[v_lollipops_menu]);
  set_background(mw[v_lollipops_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_lollipops_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_lollipops_menu]), "activate", GTK_SIGNAL_FUNC(view_lollipops_callback), (gpointer)ss);
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(mw[v_lollipops_menu], FALSE);

  mw[v_cursor_menu] = gtk_menu_item_new_with_label(STR_Verbose_cursor);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_cursor_menu]);
  set_background(mw[v_cursor_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_cursor_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_cursor_menu]), "activate", GTK_SIGNAL_FUNC(view_cursor_callback), (gpointer)ss);


  mw[v_combine_menu] = gtk_menu_item_new_with_label(STR_Channel_style);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_combine_menu]);
  set_background(mw[v_combine_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_combine_menu]);

  mw[v_combine_cascade_menu] = gtk_menu_new();
  set_background(mw[v_combine_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_combine_menu]), mw[v_combine_cascade_menu]);

  mw[v_combine_separate_menu] = gtk_menu_item_new_with_label(STR_separate);
  gtk_menu_append(GTK_MENU(mw[v_combine_cascade_menu]), mw[v_combine_separate_menu]);
  set_background(mw[v_combine_separate_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_combine_separate_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_combine_separate_menu]), "activate", GTK_SIGNAL_FUNC(view_separate_callback), (gpointer)ss);
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(mw[v_combine_separate_menu], FALSE);

  mw[v_combine_combined_menu] = gtk_menu_item_new_with_label(STR_combined);
  gtk_menu_append(GTK_MENU(mw[v_combine_cascade_menu]), mw[v_combine_combined_menu]);
  set_background(mw[v_combine_combined_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_combine_combined_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_combine_combined_menu]), "activate", GTK_SIGNAL_FUNC(view_combined_callback), (gpointer)ss);
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(mw[v_combine_combined_menu], FALSE);

  mw[v_combine_superimposed_menu] = gtk_menu_item_new_with_label(STR_superimposed);
  gtk_menu_append(GTK_MENU(mw[v_combine_cascade_menu]), mw[v_combine_superimposed_menu]);
  set_background(mw[v_combine_superimposed_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_combine_superimposed_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_combine_superimposed_menu]), "activate", GTK_SIGNAL_FUNC(view_superimposed_callback), (gpointer)ss);
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(mw[v_combine_superimposed_menu], FALSE);


  mw[v_zero_menu] = gtk_menu_item_new_with_label(STR_Show_Y0);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_zero_menu]);
  set_background(mw[v_zero_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_zero_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_zero_menu]), "activate", GTK_SIGNAL_FUNC(view_zero_callback), (gpointer)ss);

  mw[v_x_axis_menu] = gtk_menu_item_new_with_label(STR_X_axis_units);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_x_axis_menu]);
  set_background(mw[v_x_axis_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_menu]);

  mw[v_x_axis_cascade_menu] = gtk_menu_new();
  set_background(mw[v_x_axis_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_x_axis_menu]), mw[v_x_axis_cascade_menu]);

  mw[v_x_axis_seconds_menu] = gtk_menu_item_new_with_label(STR_seconds);
  gtk_menu_append(GTK_MENU(mw[v_x_axis_cascade_menu]), mw[v_x_axis_seconds_menu]);
  set_background(mw[v_x_axis_seconds_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_seconds_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_x_axis_seconds_menu]), "activate", GTK_SIGNAL_FUNC(options_x_axis_seconds_callback), (gpointer)ss);
  set_sensitive(mw[v_x_axis_seconds_menu], FALSE);

  mw[v_x_axis_samples_menu] = gtk_menu_item_new_with_label(STR_samples);
  gtk_menu_append(GTK_MENU(mw[v_x_axis_cascade_menu]), mw[v_x_axis_samples_menu]);
  set_background(mw[v_x_axis_samples_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_samples_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_x_axis_samples_menu]), "activate", GTK_SIGNAL_FUNC(options_x_axis_samples_callback), (gpointer)ss);

  mw[v_x_axis_percentage_menu] = gtk_menu_item_new_with_label(STR_percentage);
  gtk_menu_append(GTK_MENU(mw[v_x_axis_cascade_menu]), mw[v_x_axis_percentage_menu]);
  set_background(mw[v_x_axis_percentage_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_percentage_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_x_axis_percentage_menu]), "activate", GTK_SIGNAL_FUNC(options_x_axis_percentage_callback), (gpointer)ss);

  mw[v_x_axis_beats_menu] = gtk_menu_item_new_with_label(STR_beats);
  gtk_menu_append(GTK_MENU(mw[v_x_axis_cascade_menu]), mw[v_x_axis_beats_menu]);
  set_background(mw[v_x_axis_beats_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_beats_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_x_axis_beats_menu]), "activate", GTK_SIGNAL_FUNC(options_x_axis_beats_callback), (gpointer)ss);

  mw[v_error_history_menu] = gtk_menu_item_new_with_label(STR_Error_History);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]), mw[v_error_history_menu]);
  set_background(mw[v_error_history_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[v_error_history_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_error_history_menu]), "activate", GTK_SIGNAL_FUNC(view_error_history_callback), (gpointer)ss);



  /* OPTIONS MENU */
  mw[option_menu] = gtk_menu_item_new_with_label(STR_Options);
  set_background(mw[option_menu], (ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]), mw[option_menu]);
  gtk_widget_show(mw[option_menu]);

  mw[o_cascade_menu] = gtk_menu_new();
  set_background(mw[o_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[option_menu]), mw[o_cascade_menu]);

  mw[o_transform_menu] = gtk_menu_item_new_with_label(STR_Transform_Options);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]), mw[o_transform_menu]);
  set_background(mw[o_transform_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_transform_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_transform_menu]), "activate", GTK_SIGNAL_FUNC(options_transform_callback), (gpointer)ss);

  mw[o_speed_menu] = gtk_menu_item_new_with_label(STR_Speed_style);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]), mw[o_speed_menu]);
  set_background(mw[o_speed_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_speed_menu]);

  mw[o_speed_cascade_menu] = gtk_menu_new();
  set_background(mw[o_speed_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[o_speed_menu]), mw[o_speed_cascade_menu]);

  mw[o_speed_float_menu] = gtk_menu_item_new_with_label(STR_float);
  gtk_menu_append(GTK_MENU(mw[o_speed_cascade_menu]), mw[o_speed_float_menu]);
  set_background(mw[o_speed_float_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_speed_float_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_speed_float_menu]), "activate", GTK_SIGNAL_FUNC(options_speed_float_callback), (gpointer)ss);

  mw[o_speed_semitone_menu] = gtk_menu_item_new_with_label(STR_semitone);
  gtk_menu_append(GTK_MENU(mw[o_speed_cascade_menu]), mw[o_speed_semitone_menu]);
  set_background(mw[o_speed_semitone_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_speed_semitone_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_speed_semitone_menu]), "activate", GTK_SIGNAL_FUNC(options_speed_semitone_callback), (gpointer)ss);

  mw[o_speed_ratio_menu] = gtk_menu_item_new_with_label(STR_ratio);
  gtk_menu_append(GTK_MENU(mw[o_speed_cascade_menu]), mw[o_speed_ratio_menu]);
  set_background(mw[o_speed_ratio_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_speed_ratio_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_speed_ratio_menu]), "activate", GTK_SIGNAL_FUNC(options_speed_ratio_callback), (gpointer)ss);


  mw[o_focus_style_menu] = gtk_menu_item_new_with_label(STR_Focus_style);
  set_background(mw[o_focus_style_menu], (ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]), mw[o_focus_style_menu]);
  gtk_widget_show(mw[o_focus_style_menu]);

  mw[o_focus_cascade_menu] = gtk_menu_new();
  set_background(mw[o_focus_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[o_focus_style_menu]), mw[o_focus_cascade_menu]);

  mw[o_focus_left_menu] = gtk_menu_item_new_with_label(STR_focus_left);
  gtk_menu_append(GTK_MENU(mw[o_focus_cascade_menu]), mw[o_focus_left_menu]);
  set_background(mw[o_focus_left_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_focus_left_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_focus_left_menu]), "activate", GTK_SIGNAL_FUNC(options_focus_left_callback), (gpointer)ss);

  mw[o_focus_right_menu] = gtk_menu_item_new_with_label(STR_focus_right);
  gtk_menu_append(GTK_MENU(mw[o_focus_cascade_menu]), mw[o_focus_right_menu]);
  set_background(mw[o_focus_right_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_focus_right_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_focus_right_menu]), "activate", GTK_SIGNAL_FUNC(options_focus_right_callback), (gpointer)ss);

  mw[o_focus_middle_menu] = gtk_menu_item_new_with_label(STR_focus_middle);
  gtk_menu_append(GTK_MENU(mw[o_focus_cascade_menu]), mw[o_focus_middle_menu]);
  set_background(mw[o_focus_middle_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_focus_middle_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_focus_middle_menu]), "activate", GTK_SIGNAL_FUNC(options_focus_middle_callback), (gpointer)ss);

  mw[o_focus_active_menu] = gtk_menu_item_new_with_label(STR_focus_active);
  gtk_menu_append(GTK_MENU(mw[o_focus_cascade_menu]), mw[o_focus_active_menu]);
  set_background(mw[o_focus_active_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_focus_active_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_focus_active_menu]), "activate", GTK_SIGNAL_FUNC(options_focus_active_callback), (gpointer)ss);
  activate_focus_menu(ss, zoom_focus_style(ss));

#if HAVE_EXTENSION_LANGUAGE
  mw[o_save_menu] = gtk_menu_item_new_with_label(STR_Save_options);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]), mw[o_save_menu]);
  set_background(mw[o_save_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_save_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_save_menu]), "activate", GTK_SIGNAL_FUNC(options_save_callback), (gpointer)ss);

  mw[o_save_state_menu] = gtk_menu_item_new_with_label(STR_Save_state);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]), mw[o_save_state_menu]);
  set_background(mw[o_save_state_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_save_state_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_save_state_menu]), "activate", GTK_SIGNAL_FUNC(options_save_state_callback), (gpointer)ss);
#endif

  mw[o_stats_menu] = gtk_menu_item_new_with_label(STR_Show_stats);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]), mw[o_stats_menu]);
  set_background(mw[o_stats_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[o_stats_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_stats_menu]), "activate", GTK_SIGNAL_FUNC(options_stats_callback), (gpointer)ss);



  /* HELP MENU */
  mw[help_menu] = gtk_menu_item_new_with_label(STR_Help);
  set_background(mw[help_menu], (ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]), mw[help_menu]);
  gtk_widget_show(mw[help_menu]);
  gtk_menu_item_right_justify(GTK_MENU_ITEM(mw[help_menu]));

  mw[h_cascade_menu] = gtk_menu_new();
  set_background(mw[h_cascade_menu], (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[help_menu]), mw[h_cascade_menu]);

  mw[h_about_snd_menu] = gtk_menu_item_new_with_label(STR_Overview);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_about_snd_menu]);
  set_background(mw[h_about_snd_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_about_snd_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_about_snd_menu]), "activate", GTK_SIGNAL_FUNC(help_about_snd_callback), (gpointer)ss);

  mw[h_fft_menu] = gtk_menu_item_new_with_label(STR_FFT);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_fft_menu]);
  set_background(mw[h_fft_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_fft_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_fft_menu]), "activate", GTK_SIGNAL_FUNC(help_fft_callback), (gpointer)ss);

  mw[h_find_menu] = gtk_menu_item_new_with_label(STR_Find);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_find_menu]);
  set_background(mw[h_find_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_find_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_find_menu]), "activate", GTK_SIGNAL_FUNC(help_find_callback), (gpointer)ss);

  mw[h_undo_menu] = gtk_menu_item_new_with_label(STR_Undo_and_redo);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_undo_menu]);
  set_background(mw[h_undo_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_undo_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_undo_menu]), "activate", GTK_SIGNAL_FUNC(help_undo_callback), (gpointer)ss);

  mw[h_sync_menu] = gtk_menu_item_new_with_label(STR_Sync);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_sync_menu]);
  set_background(mw[h_sync_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_sync_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_sync_menu]), "activate", GTK_SIGNAL_FUNC(help_sync_callback), (gpointer)ss);

  mw[h_speed_menu] = gtk_menu_item_new_with_label(STR_Speed);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_speed_menu]);
  set_background(mw[h_speed_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_speed_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_speed_menu]), "activate", GTK_SIGNAL_FUNC(help_speed_callback), (gpointer)ss);

  mw[h_expand_menu] = gtk_menu_item_new_with_label(STR_Expand);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_expand_menu]);
  set_background(mw[h_expand_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_expand_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_expand_menu]), "activate", GTK_SIGNAL_FUNC(help_expand_callback), (gpointer)ss);

  mw[h_reverb_menu] = gtk_menu_item_new_with_label(STR_Reverb);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_reverb_menu]);
  set_background(mw[h_reverb_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_reverb_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_reverb_menu]), "activate", GTK_SIGNAL_FUNC(help_reverb_callback), (gpointer)ss);

  mw[h_contrast_menu] = gtk_menu_item_new_with_label(STR_Contrast);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_contrast_menu]);
  set_background(mw[h_contrast_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_contrast_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_contrast_menu]), "activate", GTK_SIGNAL_FUNC(help_contrast_callback), (gpointer)ss);

  mw[h_env_menu] = gtk_menu_item_new_with_label(STR_Envelope);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_env_menu]);
  set_background(mw[h_env_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_env_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_env_menu]), "activate", GTK_SIGNAL_FUNC(help_env_callback), (gpointer)ss);

  mw[h_marks_menu] = gtk_menu_item_new_with_label(STR_Marks);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_marks_menu]);
  set_background(mw[h_marks_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_marks_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_marks_menu]), "activate", GTK_SIGNAL_FUNC(help_marks_callback), (gpointer)ss);

  mw[h_mix_menu] = gtk_menu_item_new_with_label(STR_Mixing);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_mix_menu]);
  set_background(mw[h_mix_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_mix_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_mix_menu]), "activate", GTK_SIGNAL_FUNC(help_mix_callback), (gpointer)ss);

  mw[h_sound_files_menu] = gtk_menu_item_new_with_label(STR_Formats);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_sound_files_menu]);
  set_background(mw[h_sound_files_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_sound_files_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_sound_files_menu]), "activate", GTK_SIGNAL_FUNC(help_sound_files_callback), (gpointer)ss);

  mw[h_init_file_menu] = gtk_menu_item_new_with_label(STR_Customization);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_init_file_menu]);
  set_background(mw[h_init_file_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_init_file_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_init_file_menu]), "activate", GTK_SIGNAL_FUNC(help_init_file_callback), (gpointer)ss);

  mw[h_recording_menu] = gtk_menu_item_new_with_label(STR_Recording);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_recording_menu]);
  set_background(mw[h_recording_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_recording_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_recording_menu]), "activate", GTK_SIGNAL_FUNC(help_recording_callback), (gpointer)ss);

  mw[h_clm_menu] = gtk_menu_item_new_with_label(STR_CLM);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_clm_menu]);
  set_background(mw[h_clm_menu], (ss->sgx)->basic_color);
  gtk_widget_show(mw[h_clm_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_clm_menu]), "activate", GTK_SIGNAL_FUNC(help_clm_callback), (gpointer)ss);

  mw[h_news_menu] = gtk_menu_item_new_with_label(STR_News);
  set_background(mw[h_news_menu], (ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]), mw[h_news_menu]);
  gtk_widget_show(mw[h_news_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_news_menu]), "activate", GTK_SIGNAL_FUNC(help_news_callback), (gpointer)ss);

  gtk_widget_add_events (MAIN_SHELL(ss), gtk_widget_get_events(MAIN_SHELL(ss)) | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
  gtk_signal_connect(GTK_OBJECT(MAIN_SHELL(ss)), "button_press_event", GTK_SIGNAL_FUNC(middle_button_press), (gpointer)ss); 

#ifndef SND_AS_WIDGET
  gtk_menu_set_accel_group(GTK_MENU(mw[f_cascade_menu]), accel_group);
  gtk_widget_add_accelerator(mw[f_open_menu], "activate", accel_group, GDK_O, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_close_menu], "activate", accel_group, GDK_C, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_save_menu], "activate", accel_group, GDK_S, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_save_as_menu], "activate", accel_group, GDK_A, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_revert_menu], "activate", accel_group, GDK_R, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_mix_menu], "activate", accel_group, GDK_M, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_update_menu], "activate", accel_group, GDK_U, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_new_menu], "activate", accel_group, GDK_N, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_view_menu], "activate", accel_group, GDK_V, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_print_menu], "activate", accel_group, GDK_P, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
#if HAVE_EXTENSION_LANGUAGE
  gtk_widget_add_accelerator(mw[e_find_menu], "activate", accel_group, GDK_F, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
#endif
  gtk_widget_add_accelerator(mw[o_transform_menu], "activate", accel_group, GDK_T, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
#endif

  return(mw[menu_menu]);
}

#define MAX_MAIN_MENUS 12
static GtkWidget *added_menus[MAX_MAIN_MENUS];
static int new_menu = 5;
static GtkWidget **added_options = NULL;
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

static void SND_callback(GtkWidget *w, gpointer cD) 
{
  int callb, opt;
  callb = (int)get_user_data(GTK_OBJECT(w));
  opt = callb2option(callb);
  IF_MENU_HOOK(main_menu_name(opt), added_options_names[(opt < 0) ? 0 : opt])
    g_snd_callback(callb);
}

static void add_option(GtkWidget *w, int which_menu, char *label, int callb)
{
  int i;
  if (added_options_pos == added_options_size)
    {
      added_options_size += 8;
      if (added_options_pos == 0)
	{
	  added_options = (GtkWidget **)CALLOC(added_options_size, sizeof(GtkWidget *));
	  added_options_names = (char **)CALLOC(added_options_size, sizeof(char *));
	  added_options_menus = (int *)CALLOC(added_options_size, sizeof(int));
	  added_options_callb = (int *)CALLOC(added_options_size, sizeof(int));
	}
      else
	{
	  added_options = (GtkWidget **)REALLOC(added_options, added_options_size * sizeof(GtkWidget *));
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
  /* where is the goddamn menu item text? I hate Gtk! */
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(added_options_names[i]) &&
	(strcmp(label, added_options_names[i]) == 0) && 
	(added_options[i]))
      {
	gtk_widget_destroy(added_options[i]);
	added_options[i] = NULL;
	added_options_menus[i] = -1;
	FREE(added_options_names[i]);
	added_options_names[i] = NULL;
	return(0);
      }
  switch (which_menu)
    {
    case FILE_MENU: 
      if (strcmp(label, STR_Open) == 0) gtk_widget_hide(mw[f_open_menu]); else
	if (strcmp(label, STR_Close) == 0) gtk_widget_hide(mw[f_close_menu]); else
	  if (strcmp(label, STR_Save) == 0) gtk_widget_hide(mw[f_save_menu]); else
            if (strcmp(label, STR_Save_as) == 0) gtk_widget_hide(mw[f_save_as_menu]); else
	      if (strcmp(label, STR_Revert) == 0) gtk_widget_hide(mw[f_revert_menu]); else
		if (strcmp(label, STR_Mix) == 0) gtk_widget_hide(mw[f_mix_menu]); else
		  if (strcmp(label, STR_Update) == 0) gtk_widget_hide(mw[f_update_menu]); else
		    if (strcmp(label, STR_New) == 0) gtk_widget_hide(mw[f_new_menu]); else
		      if (strcmp(label, STR_Record) == 0) gtk_widget_hide(mw[f_record_menu]); else
			if (strcmp(label, STR_View) == 0) gtk_widget_hide(mw[f_view_menu]); else
			  if (strcmp(label, STR_Print) == 0) gtk_widget_hide(mw[f_print_menu]); else
			    if (strcmp(label, STR_Exit) == 0) gtk_widget_hide(mw[f_exit_menu]); else 
			      return(INVALID_MENU);
      return(FILE_MENU);
      break;
    case EDIT_MENU: 
      if (strcmp(label, STR_Undo) == 0) gtk_widget_hide(mw[e_undo_menu]); else
	if (strcmp(label, STR_Redo) == 0) gtk_widget_hide(mw[e_redo_menu]); else
	  if (strcmp(label, STR_Find) == 0) gtk_widget_hide(mw[e_find_menu]); else
            if (strcmp(label, STR_Delete_Selection) == 0) gtk_widget_hide(mw[e_cut_menu]); else
	      if (strcmp(label, STR_Insert_Selection) == 0) gtk_widget_hide(mw[e_paste_menu]); else
		if (strcmp(label, STR_Mix_Selection) == 0) gtk_widget_hide(mw[e_mix_menu]); else
		  if (strcmp(label, STR_Play_selection) == 0) gtk_widget_hide(mw[e_play_menu]); else
		    if (strcmp(label, STR_Save_Selection) == 0) gtk_widget_hide(mw[e_save_as_menu]); else
		      if (strcmp(label, STR_Select_all) == 0) gtk_widget_hide(mw[e_select_all_menu]); else
			if (strcmp(label, STR_Edit_Envelope) == 0) gtk_widget_hide(mw[e_edenv_menu]); else
			  if (strcmp(label, STR_Edit_Header) == 0) gtk_widget_hide(mw[e_header_menu]); else 
			    return(INVALID_MENU);
      return(EDIT_MENU);
      break;
    case VIEW_MENU: 
      /* if (strcmp(label, STR_Equalize_Panes) == 0) gtk_widget_hide(mw[v_equalize_panes_menu]); else */
	if (strcmp(label, STR_Show_controls) == 0) gtk_widget_hide(mw[v_ctrls_menu]); else
	  if (strcmp(label, STR_Show_listener) == 0) gtk_widget_hide(mw[v_listener_menu]); else
            if (strcmp(label, STR_Mix_Panel) == 0) gtk_widget_hide(mw[v_mix_panel_menu]); else
	      if (strcmp(label, STR_Regions) == 0) gtk_widget_hide(mw[v_region_menu]); else
		if (strcmp(label, STR_File) == 0) gtk_widget_hide(mw[v_files_menu]); else
		  if (strcmp(label, STR_Color) == 0) gtk_widget_hide(mw[v_color_menu]); else
		    if (strcmp(label, STR_Orientation) == 0) gtk_widget_hide(mw[v_orientation_menu]); else
		      if (strcmp(label, STR_Graph_style) == 0) gtk_widget_hide(mw[v_graph_style_menu]); else
			if (strcmp(label, STR_Verbose_cursor) == 0) gtk_widget_hide(mw[v_cursor_menu]); else
			  if (strcmp(label, STR_Channel_style) == 0) gtk_widget_hide(mw[v_combine_menu]); else
			    if (strcmp(label, STR_Show_Y0) == 0) gtk_widget_hide(mw[v_zero_menu]); else
			      if (strcmp(label, STR_X_axis_units) == 0) gtk_widget_hide(mw[v_x_axis_menu]); else
				if (strcmp(label, STR_Error_History) == 0) gtk_widget_hide(mw[v_error_history_menu]); else
				  return(INVALID_MENU);
      return(VIEW_MENU);
      break;
    case OPTIONS_MENU: 
      if (strcmp(label, STR_Transform_Options) == 0) gtk_widget_hide(mw[o_transform_menu]); else
	if (strcmp(label, STR_Speed_style) == 0) gtk_widget_hide(mw[o_speed_menu]); else
	  if (strcmp(label, STR_Focus_style) == 0) gtk_widget_hide(mw[o_focus_style_menu]); else
            if (strcmp(label, STR_Save_options) == 0) gtk_widget_hide(mw[o_save_menu]); else
	      if (strcmp(label, STR_Save_state) == 0) gtk_widget_hide(mw[o_save_state_menu]); else
		if (strcmp(label, STR_Show_stats) == 0) gtk_widget_hide(mw[o_stats_menu]); else 
		  return(INVALID_MENU);
      return(OPTIONS_MENU);
      break;
    case HELP_MENU: 
      if (strcmp(label, STR_Overview) == 0) gtk_widget_hide(mw[h_about_snd_menu]); else
	if (strcmp(label, STR_FFT) == 0) gtk_widget_hide(mw[h_fft_menu]); else
	  if (strcmp(label, STR_Find) == 0) gtk_widget_hide(mw[h_find_menu]); else
	    if (strcmp(label, STR_Undo_and_redo) == 0) gtk_widget_hide(mw[h_undo_menu]); else
	      if (strcmp(label, STR_Sync) == 0) gtk_widget_hide(mw[h_sync_menu]); else
		if (strcmp(label, STR_Speed) == 0) gtk_widget_hide(mw[h_speed_menu]); else
		  if (strcmp(label, STR_Expand) == 0) gtk_widget_hide(mw[h_expand_menu]); else
		    if (strcmp(label, STR_Reverb) == 0) gtk_widget_hide(mw[h_reverb_menu]); else
		      if (strcmp(label, STR_Contrast) == 0) gtk_widget_hide(mw[h_contrast_menu]); else
			if (strcmp(label, STR_Envelope) == 0) gtk_widget_hide(mw[h_env_menu]); else
			  if (strcmp(label, STR_Marks) == 0) gtk_widget_hide(mw[h_marks_menu]); else
			    if (strcmp(label, STR_Mixing) == 0) gtk_widget_hide(mw[h_mix_menu]); else
			      if (strcmp(label, STR_Format) == 0) gtk_widget_hide(mw[h_sound_files_menu]); else
				if (strcmp(label, STR_Customization) == 0) gtk_widget_hide(mw[h_init_file_menu]); else
				  if (strcmp(label, STR_Recording) == 0) gtk_widget_hide(mw[h_recording_menu]); else
				    if (strcmp(label, STR_CLM) == 0) gtk_widget_hide(mw[h_clm_menu]); else
				      if (strcmp(label, STR_News) == 0) gtk_widget_hide(mw[h_news_menu]); 
				      else return(INVALID_MENU); 
      return(HELP_MENU);
      break;
    case POPUP_MENU: 
      if (strcmp(label, STR_Play) == 0) gtk_widget_hide(popup_children[W_pop_play]); else
	if (strcmp(label, STR_Undo) == 0) gtk_widget_hide(popup_children[W_pop_undo]); else
	  if (strcmp(label, STR_Redo) == 0) gtk_widget_hide(popup_children[W_pop_redo]); else
            if (strcmp(label, STR_Save) == 0) gtk_widget_hide(popup_children[W_pop_save]); else
	      if (strcmp(label, STR_Info) == 0) gtk_widget_hide(popup_children[W_pop_info]); 
	      /* else if (strcmp(label, STR_Equalize_Panes) == 0) gtk_widget_hide(popup_children[W_pop_equalize_panes]); */
		else return(INVALID_MENU);
      return(POPUP_MENU);
      break;
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
	if (added_options_names[i]) 
	  FREE(added_options_names[i]);
	added_options_names[i] = copy_string(new_label);
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

int g_add_to_main_menu(snd_state *ss, char *label, int slot)
{
  GtkWidget *m, *mc;
  m = gtk_menu_item_new_with_label(label);
  set_background(m, (ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]), m);
  gtk_widget_show(m);
  set_user_data(GTK_OBJECT(m), (gpointer)slot);
  if (slot >= 0) gtk_signal_connect(GTK_OBJECT(m), "activate", GTK_SIGNAL_FUNC(SND_callback), (gpointer)ss);

  mc = gtk_menu_new();
  set_background(mc, (ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(m), mc);

  new_menu++;
  if (new_menu < MAX_MAIN_MENUS)
    {
      added_menus[new_menu] = mc;
      main_menu_names[new_menu] = copy_string(label);
      return(new_menu);
    }
  else return(INVALID_MENU);
}

int g_add_to_menu(snd_state *ss, int which_menu, char *label, int callb, int position)
{
  GtkWidget *m, *menw;
   switch (which_menu)
    {
    case FILE_MENU:    menw = mw[f_cascade_menu]; break;
    case EDIT_MENU:    menw = mw[e_cascade_menu]; break;
    case VIEW_MENU:    menw = mw[v_cascade_menu]; break;
    case OPTIONS_MENU: menw = mw[o_cascade_menu]; break;
    case HELP_MENU:    menw = mw[h_cascade_menu]; break;
    case POPUP_MENU:   menw = popup_menu; break;
    default: 
      if (which_menu < MAX_MAIN_MENUS)
	menw = added_menus[which_menu]; 
      else return(INVALID_MENU);
      break;
    }
   if (label)
     m = gtk_menu_item_new_with_label(label);
   else m = gtk_menu_item_new();
   if (position >= 0)
     gtk_menu_insert(GTK_MENU(menw), m, position);
   else gtk_menu_append(GTK_MENU(menw), m);
   set_background(m, (ss->sgx)->basic_color);
   gtk_widget_show(m);
   if (label)
     {
       set_user_data(GTK_OBJECT(m), (gpointer)callb);
       gtk_signal_connect(GTK_OBJECT(m), "activate", GTK_SIGNAL_FUNC(SND_callback), (gpointer)ss);
       add_option(m, which_menu, label, callb);
     }
  return(0);
}

int g_remove_from_menu(int which_menu, char *label)
{
  return(remove_option(which_menu, label));
}


/* -------------------------------- POPUP MENU -------------------------------- */

static int stopping = 0;

static void popup_play_callback(GtkWidget *w, gpointer cD) 
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
  gtk_widget_hide(popup_menu);
}

void reflect_play_stop_in_popup_menu(void)
{
  stopping = 0;
  if (popup_menu)
    set_button_label(popup_children[W_pop_play], "Play");
}

static void popup_save_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", STR_Save) save_edits(any_selected_sound(ss), NULL);
  gtk_widget_hide(popup_menu);
}

static void popup_undo_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", STR_Undo) undo_edit_with_sync(current_channel(ss), 1);
  gtk_widget_hide(popup_menu);
}

static void popup_redo_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", STR_Redo) redo_edit_with_sync(current_channel(ss), 1);
  gtk_widget_hide(popup_menu);
}

static void popup_info_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  snd_info *sp;
  IF_MENU_HOOK("Popup", STR_Info)
    {
      sp = selected_sound(ss);
      if (sp) display_info(sp);
    }
  gtk_widget_hide(popup_menu);
}

void create_popup_menu(snd_state *ss, guint button, TIME_TYPE time)
{
  int undo_possible = 0, redo_possible = 0;
  chan_info *selcp = NULL;
  if (!popup_menu)
    {
      selcp = selected_channel(ss);
      if (selcp)
	{
	  undo_possible = (selcp->edit_ctr > 0);
	  redo_possible = ((selcp->edit_size > (selcp->edit_ctr + 1)) && 
			   (selcp->edits[selcp->edit_ctr + 1]));
	}
      popup_menu = gtk_menu_new();
      set_background(popup_menu, (ss->sgx)->highlight_color);

      popup_children[W_pop_play] = gtk_menu_item_new_with_label(STR_Play);
      gtk_menu_append(GTK_MENU(popup_menu), popup_children[W_pop_play]);
      set_background(popup_children[W_pop_play], (ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_play]), "activate", GTK_SIGNAL_FUNC(popup_play_callback), (gpointer)ss);
      set_sensitive(popup_children[W_pop_play], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_play]);

      popup_children[W_pop_undo] = gtk_menu_item_new_with_label(STR_Undo);
      gtk_menu_append(GTK_MENU(popup_menu), popup_children[W_pop_undo]);
      set_background(popup_children[W_pop_undo], (ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_undo]), "activate", GTK_SIGNAL_FUNC(popup_undo_callback), (gpointer)ss);
      set_sensitive(popup_children[W_pop_undo], undo_possible);
      gtk_widget_show(popup_children[W_pop_undo]);
      
      popup_children[W_pop_redo] = gtk_menu_item_new_with_label(STR_Redo);
      gtk_menu_append(GTK_MENU(popup_menu), popup_children[W_pop_redo]);
      set_background(popup_children[W_pop_redo], (ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_redo]), "activate", GTK_SIGNAL_FUNC(popup_redo_callback), (gpointer)ss);
      set_sensitive(popup_children[W_pop_redo], redo_possible);
      gtk_widget_show(popup_children[W_pop_redo]);
      
      popup_children[W_pop_save] = gtk_menu_item_new_with_label(STR_Save);
      gtk_menu_append(GTK_MENU(popup_menu), popup_children[W_pop_save]);
      set_background(popup_children[W_pop_save], (ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_save]), "activate", GTK_SIGNAL_FUNC(popup_save_callback), (gpointer)ss);
      set_sensitive(popup_children[W_pop_save], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_save]);

      popup_children[W_pop_info] = gtk_menu_item_new_with_label(STR_Info);
      gtk_menu_append(GTK_MENU(popup_menu), popup_children[W_pop_info]);
      set_background(popup_children[W_pop_info], (ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_info]), "activate", GTK_SIGNAL_FUNC(popup_info_callback), (gpointer)ss);
      set_sensitive(popup_children[W_pop_info], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_info]);
    }
  gtk_widget_show(popup_menu);
  gtk_menu_popup(GTK_MENU(popup_menu), NULL, NULL, NULL, NULL, button, time);
}

static gint middle_button_press (GtkWidget *widget, GdkEvent *bevent, gpointer data)
{
  GdkEventButton *event = (GdkEventButton *) bevent; 
  if ((event->type == GDK_BUTTON_PRESS) && (event->button == 2))
    {
      create_popup_menu((snd_state *)data, event->button, event->time);
      return(TRUE);
    }
  return(FALSE);
}

static XEN g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets ") returns list of top level menu widgets ((0)main (1)file (2)edit (3)view (4)options (5)help)"
  return(XEN_CONS(XEN_WRAP_C_POINTER(mw[menu_menu]),
	  XEN_CONS(XEN_WRAP_C_POINTER(mw[f_cascade_menu]),
           XEN_CONS(XEN_WRAP_C_POINTER(mw[e_cascade_menu]),
            XEN_CONS(XEN_WRAP_C_POINTER(mw[v_cascade_menu]),
             XEN_CONS(XEN_WRAP_C_POINTER(mw[o_cascade_menu]),
              XEN_CONS(XEN_WRAP_C_POINTER(mw[h_cascade_menu]),
		   XEN_EMPTY_LIST)))))));
}

static XEN g_test_menus(void)
{
  guint signal_id;
  int i;
  for (i = 0; i < NUM_MENU_WIDGETS; i++)
    if ((mw[i]) && (is_sensitive(mw[i])) &&
	(i != f_exit_menu) && (i != f_save_menu) && (i != f_close_menu) && (i != e_header_menu) &&
	(i != f_new_menu) && (i != f_record_menu) && (i != v_mix_panel_menu))
      {
	signal_id = gtk_signal_lookup("activate", GTK_OBJECT_TYPE(mw[i]));
	if (signal_id >= 1)
	  gtk_signal_emit_by_name(GTK_OBJECT(mw[i]), "activate");
      }
  dismiss_all_dialogs(get_global_state());
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_test_menus_w, g_test_menus)
XEN_NARGIFY_0(g_menu_widgets_w, g_menu_widgets)
#else
#define g_test_menus_w g_test_menus
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
  XEN_DEFINE_PROCEDURE("test-menus", g_test_menus_w, 0, 0, 0, "");
  XEN_DEFINE_PROCEDURE(S_menu_widgets, g_menu_widgets_w, 0, 0, 0, H_menu_widgets);
}


