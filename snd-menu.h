#ifndef SND_MENU_H
#define SND_MENU_H

enum {m_menu,
        f_menu, f_cascade_menu,
          f_open_menu, f_close_menu, f_save_menu, f_save_as_menu, f_revert_menu, f_exit_menu, f_new_menu,
          f_view_menu, f_print_menu, f_mix_menu, f_insert_menu, f_update_menu, f_record_menu, f_sep_menu,
        e_menu, e_cascade_menu,
          e_cut_menu, e_paste_menu, e_mix_menu, e_play_menu, e_save_as_menu, e_undo_menu,
          e_redo_menu, e_find_menu, e_env_menu, e_header_menu, e_select_all_menu,
          e_select_sep_menu, e_edit_sep_menu,
        h_menu, h_cascade_menu,
          h_about_snd_menu, h_fft_menu, h_find_menu, h_undo_menu, h_sync_menu, h_controls_menu,
          h_env_menu, h_marks_menu, h_sound_files_menu, h_init_file_menu,
          h_mix_menu, h_track_menu, h_recording_menu, h_keys_menu, 
          h_play_menu, h_save_menu, h_resample_menu, h_filter_menu, h_insert_menu, 
          h_delete_menu, h_reverb_menu, h_debug_menu, h_region_menu, h_selection_menu, h_colors_menu,
        o_menu, o_cascade_menu,
          o_transform_menu,
          o_focus_style_menu, o_focus_cascade_menu,
            o_focus_right_menu, o_focus_left_menu, o_focus_middle_menu, o_focus_active_menu,
          o_save_menu, o_save_state_menu, o_sep_menu, o_preferences_menu,
        v_menu, v_cascade_menu,
          v_equalize_panes_menu, 
          v_graph_style_menu, v_graph_style_cascade_menu,
            v_lines_menu, v_dots_menu, v_filled_menu, v_dots_and_lines_menu, v_lollipops_menu,
          v_zero_menu, v_cursor_menu, v_controls_menu, v_listener_menu,
          v_region_menu,
          v_combine_menu, v_combine_cascade_menu,
            v_combine_separate_menu, v_combine_combined_menu, v_combine_superimposed_menu,
          v_color_menu, v_orientation_menu, 
          v_files_menu, v_mix_dialog_menu, v_track_dialog_menu,
          v_x_axis_menu, v_x_axis_cascade_menu,
            v_x_axis_seconds_menu, v_x_axis_samples_menu, v_x_axis_percentage_menu, v_x_axis_beats_menu, v_x_axis_measures_menu, v_x_axis_clock_menu,
          v_axes_menu, v_axes_cascade_menu,
          v_no_axes_menu, v_all_axes_menu, v_just_x_axis_menu, v_all_axes_unlabelled_menu, v_just_x_axis_unlabelled_menu, v_bare_x_axis_menu,
      v_sep2_menu,
      NUM_MENU_WIDGETS
};

#define main_menu (ss->sgx->mw[m_menu])

#define file_menu (ss->sgx->mw[f_menu])
#define file_cascade_menu (ss->sgx->mw[f_cascade_menu])
#define file_open_menu (ss->sgx->mw[f_open_menu])
#define file_close_menu (ss->sgx->mw[f_close_menu])
#define file_save_menu (ss->sgx->mw[f_save_menu])
#define file_save_as_menu (ss->sgx->mw[f_save_as_menu])
#define file_revert_menu (ss->sgx->mw[f_revert_menu])
#define file_exit_menu (ss->sgx->mw[f_exit_menu])
#define file_new_menu (ss->sgx->mw[f_new_menu])
#define file_view_menu (ss->sgx->mw[f_view_menu])
#define file_print_menu (ss->sgx->mw[f_print_menu])
#define file_mix_menu (ss->sgx->mw[f_mix_menu])
#define file_insert_menu (ss->sgx->mw[f_insert_menu])
#define file_update_menu (ss->sgx->mw[f_update_menu])
#define file_record_menu (ss->sgx->mw[f_record_menu])
#define file_sep_menu (ss->sgx->mw[f_sep_menu])

#define edit_menu (ss->sgx->mw[e_menu])
#define edit_cascade_menu (ss->sgx->mw[e_cascade_menu])
#define edit_cut_menu (ss->sgx->mw[e_cut_menu])
#define edit_paste_menu (ss->sgx->mw[e_paste_menu])
#define edit_mix_menu (ss->sgx->mw[e_mix_menu])
#define edit_play_menu (ss->sgx->mw[e_play_menu])
#define edit_save_as_menu (ss->sgx->mw[e_save_as_menu])
#define edit_undo_menu (ss->sgx->mw[e_undo_menu])
#define edit_redo_menu (ss->sgx->mw[e_redo_menu])
#define edit_find_menu (ss->sgx->mw[e_find_menu])
#define edit_env_menu (ss->sgx->mw[e_env_menu])
#define edit_header_menu (ss->sgx->mw[e_header_menu])
#define edit_select_all_menu (ss->sgx->mw[e_select_all_menu])
#define edit_select_sep_menu (ss->sgx->mw[e_select_sep_menu])
#define edit_edit_sep_menu (ss->sgx->mw[e_edit_sep_menu])

#define help_menu (ss->sgx->mw[h_menu])
#define help_cascade_menu (ss->sgx->mw[h_cascade_menu])
#define help_about_snd_menu (ss->sgx->mw[h_about_snd_menu])
#define help_fft_menu (ss->sgx->mw[h_fft_menu])
#define help_find_menu (ss->sgx->mw[h_find_menu])
#define help_undo_menu (ss->sgx->mw[h_undo_menu])
#define help_sync_menu (ss->sgx->mw[h_sync_menu])
#define help_controls_menu (ss->sgx->mw[h_controls_menu])
#define help_env_menu (ss->sgx->mw[h_env_menu])
#define help_marks_menu (ss->sgx->mw[h_marks_menu])
#define help_sound_files_menu (ss->sgx->mw[h_sound_files_menu])
#define help_init_file_menu (ss->sgx->mw[h_init_file_menu])
#define help_mix_menu (ss->sgx->mw[h_mix_menu])
#define help_track_menu (ss->sgx->mw[h_track_menu])
#define help_recording_menu (ss->sgx->mw[h_recording_menu])
#define help_keys_menu (ss->sgx->mw[h_keys_menu])
#define help_play_menu (ss->sgx->mw[h_play_menu])
#define help_save_menu (ss->sgx->mw[h_save_menu])
#define help_resample_menu (ss->sgx->mw[h_resample_menu])
#define help_filter_menu (ss->sgx->mw[h_filter_menu])
#define help_insert_menu (ss->sgx->mw[h_insert_menu])
#define help_delete_menu (ss->sgx->mw[h_delete_menu])
#define help_reverb_menu (ss->sgx->mw[h_reverb_menu])
#define help_debug_menu (ss->sgx->mw[h_debug_menu])
#define help_region_menu (ss->sgx->mw[h_region_menu])
#define help_selection_menu (ss->sgx->mw[h_selection_menu])
#define help_colors_menu (ss->sgx->mw[h_colors_menu])

#define options_menu (ss->sgx->mw[o_menu])
#define options_cascade_menu (ss->sgx->mw[o_cascade_menu])
#define options_transform_menu (ss->sgx->mw[o_transform_menu])
#define options_focus_style_menu (ss->sgx->mw[o_focus_style_menu])
#define options_focus_cascade_menu (ss->sgx->mw[o_focus_cascade_menu])
#define options_focus_right_menu (ss->sgx->mw[o_focus_right_menu])
#define options_focus_left_menu (ss->sgx->mw[o_focus_left_menu])
#define options_focus_middle_menu (ss->sgx->mw[o_focus_middle_menu])
#define options_focus_active_menu (ss->sgx->mw[o_focus_active_menu])
#define options_save_menu (ss->sgx->mw[o_save_menu])
#define options_save_state_menu (ss->sgx->mw[o_save_state_menu])
#define options_sep_menu (ss->sgx->mw[o_sep_menu])
#define options_preferences_menu (ss->sgx->mw[o_preferences_menu])

#define view_menu (ss->sgx->mw[v_menu])
#define view_cascade_menu (ss->sgx->mw[v_cascade_menu])
#define view_equalize_panes_menu (ss->sgx->mw[v_equalize_panes_menu])
#define view_graph_style_menu (ss->sgx->mw[v_graph_style_menu])
#define view_graph_style_cascade_menu (ss->sgx->mw[v_graph_style_cascade_menu])
#define view_lines_menu (ss->sgx->mw[v_lines_menu])
#define view_dots_menu (ss->sgx->mw[v_dots_menu])
#define view_filled_menu (ss->sgx->mw[v_filled_menu])
#define view_dots_and_lines_menu (ss->sgx->mw[v_dots_and_lines_menu])
#define view_lollipops_menu (ss->sgx->mw[v_lollipops_menu])
#define view_zero_menu (ss->sgx->mw[v_zero_menu])
#define view_cursor_menu (ss->sgx->mw[v_cursor_menu])
#define view_controls_menu (ss->sgx->mw[v_controls_menu])
#define view_listener_menu (ss->sgx->mw[v_listener_menu])
#define view_region_menu (ss->sgx->mw[v_region_menu])
#define view_combine_menu (ss->sgx->mw[v_combine_menu])
#define view_combine_cascade_menu (ss->sgx->mw[v_combine_cascade_menu])
#define view_combine_separate_menu (ss->sgx->mw[v_combine_separate_menu])
#define view_combine_combined_menu (ss->sgx->mw[v_combine_combined_menu])
#define view_combine_superimposed_menu (ss->sgx->mw[v_combine_superimposed_menu])
#define view_color_menu (ss->sgx->mw[v_color_menu])
#define view_orientation_menu (ss->sgx->mw[v_orientation_menu])
#define view_files_menu (ss->sgx->mw[v_files_menu])
#define view_mix_dialog_menu (ss->sgx->mw[v_mix_dialog_menu])
#define view_track_dialog_menu (ss->sgx->mw[v_track_dialog_menu])
#define view_x_axis_menu (ss->sgx->mw[v_x_axis_menu])
#define view_x_axis_cascade_menu (ss->sgx->mw[v_x_axis_cascade_menu])
#define view_x_axis_seconds_menu (ss->sgx->mw[v_x_axis_seconds_menu])
#define view_x_axis_clock_menu (ss->sgx->mw[v_x_axis_clock_menu])
#define view_x_axis_samples_menu (ss->sgx->mw[v_x_axis_samples_menu])
#define view_x_axis_percentage_menu (ss->sgx->mw[v_x_axis_percentage_menu])
#define view_x_axis_beats_menu (ss->sgx->mw[v_x_axis_beats_menu])
#define view_x_axis_measures_menu (ss->sgx->mw[v_x_axis_measures_menu])
#define view_axes_menu (ss->sgx->mw[v_axes_menu])
#define view_axes_cascade_menu (ss->sgx->mw[v_axes_cascade_menu])
#define view_no_axes_menu (ss->sgx->mw[v_no_axes_menu])
#define view_all_axes_menu (ss->sgx->mw[v_all_axes_menu])
#define view_just_x_axis_menu (ss->sgx->mw[v_just_x_axis_menu])
#define view_all_axes_unlabelled_menu (ss->sgx->mw[v_all_axes_unlabelled_menu])
#define view_just_x_axis_unlabelled_menu (ss->sgx->mw[v_just_x_axis_unlabelled_menu])
#define view_bare_x_axis_menu (ss->sgx->mw[v_bare_x_axis_menu])
#define view_sep2_menu (ss->sgx->mw[v_sep2_menu])

enum {W_pop_play, W_pop_undo, W_pop_redo, W_pop_save, W_pop_equalize_panes, 
      W_pop_info, W_pop_apply, W_pop_reset, 
      NUM_POPUP_WIDGETS};

#define popup_play_menu (ss->sgx->pw[W_pop_play])
#define popup_undo_menu (ss->sgx->pw[W_pop_undo])
#define popup_redo_menu (ss->sgx->pw[W_pop_redo])
#define popup_save_menu (ss->sgx->pw[W_pop_save])
#define popup_equalize_panes_menu (ss->sgx->pw[W_pop_equalize_panes])
#define popup_info_menu (ss->sgx->pw[W_pop_info])
#define popup_apply_menu (ss->sgx->pw[W_pop_apply])
#define popup_reset_menu (ss->sgx->pw[W_pop_reset])


void edit_menu_update(void);
void view_menu_update(void);
void options_menu_update(void);
void file_menu_update(void);
void popup_menu_update(void);
void update_file_from_menu(void);
void revert_file_from_menu(void);
void save_options_from_menu(void);
void save_state_from_menu(void);
void unprotect_callback(int slot);

int g_add_to_main_menu(char *label, int slot);
widget_t g_add_to_menu(int which_menu, const char *label, int callb, int position);
int g_remove_from_menu(int which_menu, const char *label);
void g_snd_callback(int callb);

#endif
