#include "snd.h"

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
          h_about_snd_menu, h_fft_menu, h_find_menu, h_undo_menu, h_sync_menu, h_controls_menu,
          h_env_menu, h_marks_menu, h_sound_files_menu, h_init_file_menu,
          h_mix_menu, h_track_menu, h_recording_menu, h_keys_menu,
          h_play_menu, h_save_menu, h_resample_menu, h_filter_menu, h_insert_menu, 
          h_delete_menu, h_reverb_menu, h_debug_menu,
        option_menu, o_cascade_menu,
          o_transform_menu,
          o_focus_style_menu, o_focus_cascade_menu,
            o_focus_right_menu, o_focus_left_menu, o_focus_middle_menu, o_focus_active_menu,
          o_save_menu, o_save_state_menu,
        view_menu, v_cascade_menu,
          v_graph_style_menu, v_graph_style_cascade_menu,
            v_lines_menu, v_dots_menu, v_filled_menu, v_dots_and_lines_menu, v_lollipops_menu,
          v_zero_menu, v_cursor_menu, v_ctrls_menu, v_listener_menu,
          v_region_menu,
          v_combine_menu, v_combine_cascade_menu,
            v_combine_separate_menu, v_combine_combined_menu, v_combine_superimposed_menu,
          v_color_menu, v_orientation_menu, 
          v_files_menu, v_mix_dialog_menu, v_track_dialog_menu,
          v_x_axis_menu, v_x_axis_cascade_menu,
            v_x_axis_seconds_menu, v_x_axis_samples_menu, v_x_axis_percentage_menu, v_x_axis_beats_menu,
          v_error_history_menu,
          v_sep2_menu
};

#define NUM_MENU_WIDGETS 98
static GtkWidget *mw[NUM_MENU_WIDGETS];
static const char *ml[NUM_MENU_WIDGETS];

enum {W_pop_play, W_pop_undo, W_pop_redo, W_pop_save, W_pop_info};
#define NUM_POPUP_CHILDREN 6
static GtkWidget *popup_menu = NULL;
static GtkWidget *popup_children[NUM_POPUP_CHILDREN];
static const char *pl[NUM_POPUP_CHILDREN];

bool popup_menu_exists(void) {return(popup_menu != NULL);}

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
GtkWidget *options_focus_left_menu(void) {return(mw[o_focus_left_menu]);}
GtkWidget *options_focus_right_menu(void) {return(mw[o_focus_right_menu]);}
GtkWidget *options_focus_middle_menu(void) {return(mw[o_focus_middle_menu]);}
GtkWidget *options_focus_active_menu(void) {return(mw[o_focus_active_menu]);}

GtkWidget *popup_play_menu(void) {return(popup_children[W_pop_play]);}
GtkWidget *popup_undo_menu(void) {return(popup_children[W_pop_undo]);}
GtkWidget *popup_redo_menu(void) {return(popup_children[W_pop_redo]);}
GtkWidget *popup_save_menu(void) {return(popup_children[W_pop_save]);}
GtkWidget *popup_info_menu(void) {return(popup_children[W_pop_info]);}

void set_menu_label(GtkWidget *w, const char *label) {if (w) set_button_label(w, label);}

/* -------------------------------- FILE MENU -------------------------------- */

static void file_open_callback(GtkWidget *w, gpointer info) {make_open_file_dialog(false, true);}
static void file_view_callback(GtkWidget *w, gpointer info) {make_open_file_dialog(true, true);}
static void file_new_callback(GtkWidget *w, gpointer info) {new_file_from_menu();}
static void file_record_callback(GtkWidget *w, gpointer info) {snd_record_file();}
static void file_close_callback(GtkWidget *w, gpointer info) {close_file_from_menu();}
static void file_save_callback(GtkWidget *w, gpointer info) {save_file_from_menu();}
static void file_update_callback(GtkWidget *w, gpointer info) {update_file_from_menu();}
static void file_save_as_callback(GtkWidget *w, gpointer info) {make_file_save_as_dialog(true);}
static void file_revert_callback(GtkWidget *w, gpointer info) {revert_file_from_menu();}
static void file_exit_callback(GtkWidget *w, gpointer info) {exit_from_menu();}
static void file_mix_callback_1(GtkWidget *w, gpointer info) {make_mix_file_dialog(true);}
static void file_print_callback_1(GtkWidget *w, gpointer info) {file_print_callback(w, info);}


/* -------------------------------- EDIT MENU -------------------------------- */


static void edit_mix_callback(GtkWidget *w, gpointer info) {add_selection_or_region(0, selected_channel());}
static void edit_envelope_callback(GtkWidget *w, gpointer info) {create_envelope_editor();}
static void edit_cut_callback(GtkWidget *w, gpointer info) {delete_selection(UPDATE_DISPLAY);}
static void edit_paste_callback(GtkWidget *w, gpointer info) {insert_selection_from_menu();}
static void edit_save_as_callback(GtkWidget *w, gpointer info) {make_edit_save_as_dialog(true);}
static void edit_select_all_callback(GtkWidget *w, gpointer info) {select_all(current_channel());}
static void edit_undo_callback(GtkWidget *w, gpointer info) {undo_edit_with_sync(current_channel(), 1);}
static void edit_redo_callback(GtkWidget *w, gpointer info) {redo_edit_with_sync(current_channel(), 1);}

static bool selection_play_stop = false;

static void edit_play_callback(GtkWidget *w, gpointer info) 
{
  if (selection_play_stop)
    stop_playing_all_sounds(PLAY_BUTTON_UNSET);
  else
    {
      set_menu_label(edit_play_menu(), _("Stop"));
      selection_play_stop = true;
      play_selection(IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
    }
}

void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu(), _("Play Selection"));
  selection_play_stop = false;
}

static void edit_header_callback_1(GtkWidget *w, gpointer info)
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) edit_header(sp);
}

#if HAVE_EXTENSION_LANGUAGE
static void edit_find_callback_1(GtkWidget *w, gpointer info)
{
  edit_find_callback(w, info);
}
#endif


/* -------------------------------- VIEW MENU -------------------------------- */

static void view_separate_callback(GtkWidget *w, gpointer info) {set_channel_style(CHANNELS_SEPARATE);}
static void view_combined_callback(GtkWidget *w, gpointer info) {set_channel_style(CHANNELS_COMBINED);}
static void view_superimposed_callback(GtkWidget *w, gpointer info) {set_channel_style(CHANNELS_SUPERIMPOSED);}
static void view_dots_callback(GtkWidget *w, gpointer info) {set_graph_style(GRAPH_DOTS);}
static void view_lines_callback(GtkWidget *w, gpointer info) {set_graph_style(GRAPH_LINES);}
static void view_filled_callback(GtkWidget *w, gpointer info) {set_graph_style(GRAPH_FILLED);}
static void view_dots_and_lines_callback(GtkWidget *w, gpointer info) {set_graph_style(GRAPH_DOTS_AND_LINES);}
static void view_lollipops_callback(GtkWidget *w, gpointer info) {set_graph_style(GRAPH_LOLLIPOPS);}
static void view_zero_callback(GtkWidget *w, gpointer info) {set_show_y_zero((!(show_y_zero(ss))));}
static void view_cursor_callback(GtkWidget *w, gpointer info) {set_verbose_cursor((!(verbose_cursor(ss))));}

static void view_ctrls_callback(GtkWidget *w, gpointer info)
{
  char *label;
  label = (char *)gtk_label_get_text(GTK_LABEL(GTK_BIN(w)->child));
  if ((label) && (strcmp((const char *)label, _("Show controls")) == 0))
    show_controls(); 
  else hide_controls();
}

#if HAVE_EXTENSION_LANGUAGE
static void view_listener_callback(GtkWidget *w, gpointer info) 
{
  handle_listener(listener_height() < 5);
}
#endif

static void view_mix_dialog_callback(GtkWidget *w, gpointer info) {make_mix_dialog();}
static void view_track_dialog_callback(GtkWidget *w, gpointer info) {make_track_dialog();}
static void view_error_history_callback(GtkWidget *w, gpointer info) {show_snd_errors();}
static void view_region_callback_1(GtkWidget *w, gpointer info) {view_region_callback(w, info);}
static void view_orientation_callback_1(GtkWidget *w, gpointer info) {view_orientation_callback(w, info);}
static void view_color_callback_1(GtkWidget *w, gpointer info) {view_color_callback(w, info);}
static void view_files_callback_1(GtkWidget *w, gpointer info) {view_files_callback(w, info);}


/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_transform_callback(GtkWidget *w, gpointer info) {fire_up_transform_dialog(true);}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(GtkWidget *w, gpointer info) {save_options_from_menu();}
#endif
static void options_focus_right_callback(GtkWidget *w, gpointer info, gpointer Data) {activate_focus_menu(ZOOM_FOCUS_RIGHT);}
static void options_focus_left_callback(GtkWidget *w, gpointer info, gpointer Data) {activate_focus_menu(ZOOM_FOCUS_LEFT);}
static void options_focus_middle_callback(GtkWidget *w, gpointer info, gpointer Data) {activate_focus_menu(ZOOM_FOCUS_MIDDLE);}
static void options_focus_active_callback(GtkWidget *w, gpointer info, gpointer Data) {activate_focus_menu(ZOOM_FOCUS_ACTIVE);}
static void options_x_axis_seconds_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_IN_SECONDS);}
static void options_x_axis_beats_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_IN_BEATS);}
static void options_x_axis_samples_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_IN_SAMPLES);}
static void options_x_axis_percentage_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_AS_PERCENTAGE);}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(GtkWidget *w, gpointer info) {save_state_from_menu();}
#endif


/* -------------------------------- HELP MENU -------------------------------- */

static void help_about_snd_callback(GtkWidget *w, gpointer info) {about_snd_help();}
static void help_fft_callback (GtkWidget *w, gpointer info) {fft_help();}
static void help_find_callback (GtkWidget *w, gpointer info) {find_help();}
static void help_undo_callback (GtkWidget *w, gpointer info) {undo_help();}
static void help_sync_callback (GtkWidget *w, gpointer info) {sync_help();}
static void help_debug_callback (GtkWidget *w, gpointer info) {debug_help();}
static void help_controls_callback (GtkWidget *w, gpointer info) {controls_help();}
static void help_env_callback (GtkWidget *w, gpointer info) {env_help();}
static void help_marks_callback (GtkWidget *w, gpointer info) {marks_help();}
static void help_mix_callback (GtkWidget *w, gpointer info) {mix_help();}
static void help_track_callback (GtkWidget *w, gpointer info) {track_help();}
static void help_sound_files_callback (GtkWidget *w, gpointer info) {sound_files_help();}
static void help_init_file_callback (GtkWidget *w, gpointer info) {init_file_help();}
static void help_recording_callback (GtkWidget *w, gpointer info) {recording_help();}
static void help_keys_callback (GtkWidget *w, gpointer info) {key_binding_help();}
static void help_play_callback (GtkWidget *w, gpointer info) {play_help();}
static void help_resample_callback (GtkWidget *w, gpointer info) {resample_help();}
static void help_reverb_callback (GtkWidget *w, gpointer info) {reverb_help();}
static void help_insert_callback (GtkWidget *w, gpointer info) {insert_help();}
static void help_delete_callback (GtkWidget *w, gpointer info) {delete_help();}
static void help_filter_callback (GtkWidget *w, gpointer info) {filter_help();}
static void help_save_callback (GtkWidget *w, gpointer info) {save_help();}

void check_menu_labels(int key, int state, bool extended) {}


/* -------------------------------- MAIN MENU -------------------------------- */

/* I tried gtk_image_menu_item_new_from_stock here, and it looks cruddy to me -- will stick with bare-bones menus */

GtkWidget *add_menu(void)
{
  /* this mainly passes the global data pointer (ss) to all the menu-related callbacks */
#ifndef SND_AS_WIDGET
  GtkAccelGroup *accel_group;
  accel_group = gtk_accel_group_new();
  gtk_window_add_accel_group(GTK_WINDOW(MAIN_SHELL(ss)), accel_group);
#endif
  mw[menu_menu] = gtk_menu_bar_new();
  ml[menu_menu] = NULL;
  add_drop(mw[menu_menu]);
  gtk_box_pack_start(GTK_BOX(MAIN_PANE(ss)), mw[menu_menu], false, true, 0);
  gtk_widget_show(mw[menu_menu]);

  /* FILE MENU */
  mw[file_menu] = gtk_menu_item_new_with_label(_("File"));
  ml[file_menu] = _("File");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[file_menu]);
  gtk_widget_show(mw[file_menu]);

  mw[f_cascade_menu] = gtk_menu_new();
  ml[f_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[file_menu]), mw[f_cascade_menu]);

  mw[f_open_menu] = gtk_menu_item_new_with_label(_("Open"));
  ml[f_open_menu] = _("Open");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_open_menu]);
  gtk_widget_show(mw[f_open_menu]);
  SG_SIGNAL_CONNECT(mw[f_open_menu], "activate", file_open_callback, NULL);

  mw[f_close_menu] = gtk_menu_item_new_with_label(_("Close"));
  ml[f_close_menu] = _("Close");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_close_menu]);
  gtk_widget_show(mw[f_close_menu]);
  set_sensitive(mw[f_close_menu], false);
  SG_SIGNAL_CONNECT(mw[f_close_menu], "activate", file_close_callback, NULL);
  
  mw[f_save_menu] = gtk_menu_item_new_with_label(_("Save"));
  ml[f_save_menu] = _("Save");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_save_menu]);
  gtk_widget_show(mw[f_save_menu]);
  set_sensitive(mw[f_save_menu], false);
  SG_SIGNAL_CONNECT(mw[f_save_menu], "activate", file_save_callback, NULL);
  
  mw[f_save_as_menu] = gtk_menu_item_new_with_label(_("Save as"));
  ml[f_save_as_menu] = _("Save as");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_save_as_menu]);
  gtk_widget_show(mw[f_save_as_menu]);
  set_sensitive(mw[f_save_as_menu], false);
  SG_SIGNAL_CONNECT(mw[f_save_as_menu], "activate", file_save_as_callback, NULL);
  
  mw[f_revert_menu] = gtk_menu_item_new_with_label(_("Revert"));
  ml[f_revert_menu] = _("Revert");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_revert_menu]);
  gtk_widget_show(mw[f_revert_menu]);
  set_sensitive(mw[f_revert_menu], false);
  SG_SIGNAL_CONNECT(mw[f_revert_menu], "activate", file_revert_callback, NULL);
  
  mw[f_mix_menu] = gtk_menu_item_new_with_label(_("Mix"));
  ml[f_mix_menu] = _("Mix");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_mix_menu]);
  gtk_widget_show(mw[f_mix_menu]);
  set_sensitive(mw[f_mix_menu], false);
  SG_SIGNAL_CONNECT(mw[f_mix_menu], "activate", file_mix_callback_1, NULL);

  mw[f_update_menu] = gtk_menu_item_new_with_label(_("Update"));
  ml[f_update_menu] = _("Update");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_update_menu]);
  gtk_widget_show(mw[f_update_menu]);
  set_sensitive(mw[f_update_menu], false);
  SG_SIGNAL_CONNECT(mw[f_update_menu], "activate", file_update_callback, NULL);

  mw[f_new_menu] = gtk_menu_item_new_with_label(_("New"));
  ml[f_new_menu] = _("New");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_new_menu]);
  gtk_widget_show(mw[f_new_menu]);
  SG_SIGNAL_CONNECT(mw[f_new_menu], "activate", file_new_callback, NULL);

  mw[f_record_menu] = gtk_menu_item_new_with_label(_("Record"));
  ml[f_record_menu] = _("Record");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_record_menu]);
  gtk_widget_show(mw[f_record_menu]);
  SG_SIGNAL_CONNECT(mw[f_record_menu], "activate", file_record_callback, NULL);

  mw[f_view_menu] = gtk_menu_item_new_with_label(_("View"));
  ml[f_view_menu] = _("View");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_view_menu]);
  gtk_widget_show(mw[f_view_menu]);
  SG_SIGNAL_CONNECT(mw[f_view_menu], "activate", file_view_callback, NULL);

  mw[f_print_menu] = gtk_menu_item_new_with_label(_("Print"));
  ml[f_print_menu] = _("Print");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_print_menu]);
  gtk_widget_show(mw[f_print_menu]);
  set_sensitive(mw[f_print_menu], false);
  SG_SIGNAL_CONNECT(mw[f_print_menu], "activate", file_print_callback_1, NULL);

  mw[f_sep_menu] = gtk_menu_item_new();
  ml[f_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_sep_menu]);
  gtk_widget_show(mw[f_sep_menu]);
  gtk_widget_modify_bg(mw[f_sep_menu], GTK_STATE_NORMAL, ss->sgx->black);

  mw[f_exit_menu] = gtk_menu_item_new_with_label(_("Exit"));
  ml[f_exit_menu] = _("Exit");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_exit_menu]);
  gtk_widget_show(mw[f_exit_menu]);
  SG_SIGNAL_CONNECT(mw[f_exit_menu], "activate", file_exit_callback, NULL);

  /* EDIT MENU */
  mw[edit_menu] = gtk_menu_item_new_with_label(_("Edit"));
  ml[edit_menu] = _("Edit");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[edit_menu]);
  gtk_widget_show(mw[edit_menu]);

  mw[e_cascade_menu] = gtk_menu_new();
  ml[e_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[edit_menu]), mw[e_cascade_menu]);
  
  mw[e_undo_menu] = gtk_menu_item_new_with_label(_("Undo"));
  ml[e_undo_menu] = _("Undo");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_undo_menu]);
  gtk_widget_show(mw[e_undo_menu]);
  set_sensitive(mw[e_undo_menu], false);
  SG_SIGNAL_CONNECT(mw[e_undo_menu], "activate", edit_undo_callback, NULL);

  mw[e_redo_menu] = gtk_menu_item_new_with_label(_("Redo"));
  ml[e_redo_menu] = _("Redo");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_redo_menu]);
  gtk_widget_show(mw[e_redo_menu]);
  set_sensitive(mw[e_redo_menu], false);
  SG_SIGNAL_CONNECT(mw[e_redo_menu], "activate", edit_redo_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  mw[e_find_menu] = gtk_menu_item_new_with_label(_("Find"));
  ml[e_find_menu] = _("Find");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_find_menu]);
  gtk_widget_show(mw[e_find_menu]);
  set_sensitive(mw[e_find_menu], false);
  SG_SIGNAL_CONNECT(mw[e_find_menu], "activate", edit_find_callback_1, NULL);
#endif

  mw[e_select_sep_menu] = gtk_menu_item_new();
  ml[e_select_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_select_sep_menu]);
  gtk_widget_show(mw[e_select_sep_menu]);
  gtk_widget_modify_bg(mw[e_select_sep_menu], GTK_STATE_NORMAL, ss->sgx->black);

  mw[e_cut_menu] = gtk_menu_item_new_with_label(_("Delete Selection"));
  ml[e_cut_menu] = _("Delete Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_cut_menu]);
  gtk_widget_show(mw[e_cut_menu]);
  set_sensitive(mw[e_cut_menu], false);
  SG_SIGNAL_CONNECT(mw[e_cut_menu], "activate", edit_cut_callback, NULL);

  mw[e_paste_menu] = gtk_menu_item_new_with_label(_("Insert Selection"));
  ml[e_paste_menu] = _("Insert Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_paste_menu]);
  gtk_widget_show(mw[e_paste_menu]);
  set_sensitive(mw[e_paste_menu], false);
  SG_SIGNAL_CONNECT(mw[e_paste_menu], "activate", edit_paste_callback, NULL);

  mw[e_mix_menu] = gtk_menu_item_new_with_label(_("Mix Selection"));
  ml[e_mix_menu] = _("Mix Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_mix_menu]);
  gtk_widget_show(mw[e_mix_menu]);
  set_sensitive(mw[e_mix_menu], false);
  SG_SIGNAL_CONNECT(mw[e_mix_menu], "activate", edit_mix_callback, NULL);

  mw[e_play_menu] = gtk_menu_item_new_with_label(_("Play Selection"));
  ml[e_play_menu] = _("Play Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_play_menu]);
  gtk_widget_show(mw[e_play_menu]);
  set_sensitive(mw[e_play_menu], false);
  SG_SIGNAL_CONNECT(mw[e_play_menu], "activate", edit_play_callback, NULL);

  mw[e_save_as_menu] = gtk_menu_item_new_with_label(_("Save Selection"));
  ml[e_save_as_menu] = _("Save Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_save_as_menu]);
  gtk_widget_show(mw[e_save_as_menu]);
  set_sensitive(mw[e_save_as_menu], false);
  SG_SIGNAL_CONNECT(mw[e_save_as_menu], "activate", edit_save_as_callback, NULL);

  mw[e_select_all_menu] = gtk_menu_item_new_with_label(_("Select all"));
  ml[e_select_all_menu] = _("Select all");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_select_all_menu]);
  gtk_widget_show(mw[e_select_all_menu]);
  set_sensitive(mw[e_select_all_menu], false);
  SG_SIGNAL_CONNECT(mw[e_select_all_menu], "activate", edit_select_all_callback, NULL);

  mw[e_edit_sep_menu] = gtk_menu_item_new();
  ml[e_edit_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_edit_sep_menu]);
  gtk_widget_show(mw[e_edit_sep_menu]);
  gtk_widget_modify_bg(mw[e_edit_sep_menu], GTK_STATE_NORMAL, ss->sgx->black);

  mw[e_edenv_menu] = gtk_menu_item_new_with_label(_("Edit Envelope"));
  ml[e_edenv_menu] = _("Edit Envelope");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_edenv_menu]);
  gtk_widget_show(mw[e_edenv_menu]);
  SG_SIGNAL_CONNECT(mw[e_edenv_menu], "activate", edit_envelope_callback, NULL);

  mw[e_header_menu] = gtk_menu_item_new_with_label(_("Edit Header"));
  ml[e_header_menu] = _("Edit Header");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_header_menu]);
  gtk_widget_show(mw[e_header_menu]);
  set_sensitive(mw[e_header_menu], false);
  SG_SIGNAL_CONNECT(mw[e_header_menu], "activate", edit_header_callback_1, NULL);


  /* VIEW MENU */
  mw[view_menu] = gtk_menu_item_new_with_label(_("View"));
  ml[view_menu] = _("View");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[view_menu]);
  gtk_widget_show(mw[view_menu]);

  mw[v_cascade_menu] = gtk_menu_new();
  ml[v_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[view_menu]), mw[v_cascade_menu]);

  mw[v_ctrls_menu] = gtk_menu_item_new_with_label(_("Show controls"));
  ml[v_ctrls_menu] = _("Show controls");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_ctrls_menu]);
  gtk_widget_show(mw[v_ctrls_menu]);
  SG_SIGNAL_CONNECT(mw[v_ctrls_menu], "activate", view_ctrls_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  mw[v_listener_menu] = gtk_menu_item_new_with_label(_("Open listener"));
  ml[v_listener_menu] = _("Open listener");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_listener_menu]);
  gtk_widget_show(mw[v_listener_menu]);
  SG_SIGNAL_CONNECT(mw[v_listener_menu], "activate", view_listener_callback, NULL);
#endif

  mw[v_mix_dialog_menu] = gtk_menu_item_new_with_label(_("Mixes"));
  ml[v_mix_dialog_menu] = _("Mixes");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_mix_dialog_menu]);
  gtk_widget_show(mw[v_mix_dialog_menu]);
  SG_SIGNAL_CONNECT(mw[v_mix_dialog_menu], "activate", view_mix_dialog_callback, NULL);

  mw[v_track_dialog_menu] = gtk_menu_item_new_with_label(_("Tracks"));
  ml[v_track_dialog_menu] = _("Tracks");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_track_dialog_menu]);
  gtk_widget_show(mw[v_track_dialog_menu]);
  SG_SIGNAL_CONNECT(mw[v_track_dialog_menu], "activate", view_track_dialog_callback, NULL);

  mw[v_region_menu] = gtk_menu_item_new_with_label(_("Regions"));
  ml[v_region_menu] = _("Regions");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_region_menu]);
  gtk_widget_show(mw[v_region_menu]);
  SG_SIGNAL_CONNECT(mw[v_region_menu], "activate", view_region_callback_1, NULL);
  set_sensitive(mw[v_region_menu], false);

  mw[v_files_menu] = gtk_menu_item_new_with_label(_("Files"));
  ml[v_files_menu] = _("Files");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_files_menu]);
  gtk_widget_show(mw[v_files_menu]);
  SG_SIGNAL_CONNECT(mw[v_files_menu], "activate", view_files_callback_1, NULL);

  mw[v_color_menu] = gtk_menu_item_new_with_label(_("Color"));
  ml[v_color_menu] = _("Color");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_color_menu]);
  gtk_widget_show(mw[v_color_menu]);
  SG_SIGNAL_CONNECT(mw[v_color_menu], "activate", view_color_callback_1, NULL);

  mw[v_orientation_menu] = gtk_menu_item_new_with_label(_("Orientation"));
  ml[v_orientation_menu] = _("Orientation");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_orientation_menu]);
  gtk_widget_show(mw[v_orientation_menu]);
  SG_SIGNAL_CONNECT(mw[v_orientation_menu], "activate", view_orientation_callback_1, NULL);

  mw[v_sep2_menu] = gtk_menu_item_new();
  ml[v_sep2_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_sep2_menu]);
  gtk_widget_show(mw[v_sep2_menu]);
  gtk_widget_modify_bg(mw[v_sep2_menu], GTK_STATE_NORMAL, (ss->sgx)->black);


  mw[v_graph_style_menu] = gtk_menu_item_new_with_label(_("Graph style"));
  ml[v_graph_style_menu] = _("Graph style");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_graph_style_menu]);
  gtk_widget_show(mw[v_graph_style_menu]);

  mw[v_graph_style_cascade_menu] = gtk_menu_new();
  ml[v_graph_style_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_graph_style_menu]), mw[v_graph_style_cascade_menu]);

  mw[v_lines_menu] = gtk_menu_item_new_with_label(_("lines"));
  ml[v_lines_menu] = _("lines");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_lines_menu]);
  gtk_widget_show(mw[v_lines_menu]);
  SG_SIGNAL_CONNECT(mw[v_lines_menu], "activate", view_lines_callback, NULL);
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(mw[v_lines_menu], false);

  mw[v_dots_menu] = gtk_menu_item_new_with_label(_("dots"));
  ml[v_dots_menu] = _("dots");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_dots_menu]);
  gtk_widget_show(mw[v_dots_menu]);
  SG_SIGNAL_CONNECT(mw[v_dots_menu], "activate", view_dots_callback, NULL);
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(mw[v_dots_menu], false);

  mw[v_filled_menu] = gtk_menu_item_new_with_label(_("filled"));
  ml[v_filled_menu] = _("filled");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_filled_menu]);
  gtk_widget_show(mw[v_filled_menu]);
  SG_SIGNAL_CONNECT(mw[v_filled_menu], "activate", view_filled_callback, NULL);
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(mw[v_filled_menu], false);

  mw[v_dots_and_lines_menu] = gtk_menu_item_new_with_label(_("dots and lines"));
  ml[v_dots_and_lines_menu] = _("dots and lines");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_dots_and_lines_menu]);
  gtk_widget_show(mw[v_dots_and_lines_menu]);
  SG_SIGNAL_CONNECT(mw[v_dots_and_lines_menu], "activate", view_dots_and_lines_callback, NULL);
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(mw[v_dots_and_lines_menu], false);

  mw[v_lollipops_menu] = gtk_menu_item_new_with_label(_("lollipops"));
  ml[v_lollipops_menu] = _("lollipops");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_lollipops_menu]);
  gtk_widget_show(mw[v_lollipops_menu]);
  SG_SIGNAL_CONNECT(mw[v_lollipops_menu], "activate", view_lollipops_callback, NULL);
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(mw[v_lollipops_menu], false);

  mw[v_cursor_menu] = gtk_menu_item_new_with_label(_("Verbose cursor"));
  ml[v_cursor_menu] = _("Verbose cursor");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_cursor_menu]);
  gtk_widget_show(mw[v_cursor_menu]);
  SG_SIGNAL_CONNECT(mw[v_cursor_menu], "activate", view_cursor_callback, NULL);


  mw[v_combine_menu] = gtk_menu_item_new_with_label(_("Channel style"));
  ml[v_combine_menu] = _("Channel style");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_combine_menu]);
  gtk_widget_show(mw[v_combine_menu]);

  mw[v_combine_cascade_menu] = gtk_menu_new();
  ml[v_combine_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_combine_menu]), mw[v_combine_cascade_menu]);

  mw[v_combine_separate_menu] = gtk_menu_item_new_with_label(_("separate"));
  ml[v_combine_separate_menu] = _("separate");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_combine_cascade_menu]), mw[v_combine_separate_menu]);
  gtk_widget_show(mw[v_combine_separate_menu]);
  SG_SIGNAL_CONNECT(mw[v_combine_separate_menu], "activate", view_separate_callback, NULL);
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(mw[v_combine_separate_menu], false);

  mw[v_combine_combined_menu] = gtk_menu_item_new_with_label(_("combined"));
  ml[v_combine_combined_menu] = _("combined");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_combine_cascade_menu]), mw[v_combine_combined_menu]);
  gtk_widget_show(mw[v_combine_combined_menu]);
  SG_SIGNAL_CONNECT(mw[v_combine_combined_menu], "activate", view_combined_callback, NULL);
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(mw[v_combine_combined_menu], false);

  mw[v_combine_superimposed_menu] = gtk_menu_item_new_with_label(_("superimposed"));
  ml[v_combine_superimposed_menu] = _("superimposed");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_combine_cascade_menu]), mw[v_combine_superimposed_menu]);
  gtk_widget_show(mw[v_combine_superimposed_menu]);
  SG_SIGNAL_CONNECT(mw[v_combine_superimposed_menu], "activate", view_superimposed_callback, NULL);
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(mw[v_combine_superimposed_menu], false);


  mw[v_zero_menu] = gtk_menu_item_new_with_label(_("Show Y = 0"));
  ml[v_zero_menu] = _("Show Y = 0");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_zero_menu]);
  gtk_widget_show(mw[v_zero_menu]);
  SG_SIGNAL_CONNECT(mw[v_zero_menu], "activate", view_zero_callback, NULL);

  mw[v_x_axis_menu] = gtk_menu_item_new_with_label(_("X axis units"));
  ml[v_x_axis_menu] = _("X axis units");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_x_axis_menu]);
  gtk_widget_show(mw[v_x_axis_menu]);

  mw[v_x_axis_cascade_menu] = gtk_menu_new();
  ml[v_x_axis_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_x_axis_menu]), mw[v_x_axis_cascade_menu]);

  mw[v_x_axis_seconds_menu] = gtk_menu_item_new_with_label(_("seconds"));
  ml[v_x_axis_seconds_menu] = _("seconds");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_x_axis_cascade_menu]), mw[v_x_axis_seconds_menu]);
  gtk_widget_show(mw[v_x_axis_seconds_menu]);
  SG_SIGNAL_CONNECT(mw[v_x_axis_seconds_menu], "activate", options_x_axis_seconds_callback, NULL);
  set_sensitive(mw[v_x_axis_seconds_menu], false);

  mw[v_x_axis_samples_menu] = gtk_menu_item_new_with_label(_("samples"));
  ml[v_x_axis_samples_menu] = _("samples");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_x_axis_cascade_menu]), mw[v_x_axis_samples_menu]);
  gtk_widget_show(mw[v_x_axis_samples_menu]);
  SG_SIGNAL_CONNECT(mw[v_x_axis_samples_menu], "activate", options_x_axis_samples_callback, NULL);

  mw[v_x_axis_percentage_menu] = gtk_menu_item_new_with_label(_("percentage"));
  ml[v_x_axis_percentage_menu] = _("percentage");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_x_axis_cascade_menu]), mw[v_x_axis_percentage_menu]);
  gtk_widget_show(mw[v_x_axis_percentage_menu]);
  SG_SIGNAL_CONNECT(mw[v_x_axis_percentage_menu], "activate", options_x_axis_percentage_callback, NULL);

  mw[v_x_axis_beats_menu] = gtk_menu_item_new_with_label(_("beats"));
  ml[v_x_axis_beats_menu] = _("beats");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_x_axis_cascade_menu]), mw[v_x_axis_beats_menu]);
  gtk_widget_show(mw[v_x_axis_beats_menu]);
  SG_SIGNAL_CONNECT(mw[v_x_axis_beats_menu], "activate", options_x_axis_beats_callback, NULL);

  mw[v_error_history_menu] = gtk_menu_item_new_with_label(_("Error History"));
  ml[v_error_history_menu] = _("Error History");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_error_history_menu]);
  gtk_widget_show(mw[v_error_history_menu]);
  SG_SIGNAL_CONNECT(mw[v_error_history_menu], "activate", view_error_history_callback, NULL);



  /* OPTIONS MENU */
  mw[option_menu] = gtk_menu_item_new_with_label(_("Options"));
  ml[option_menu] = _("Options");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[option_menu]);
  gtk_widget_show(mw[option_menu]);

  mw[o_cascade_menu] = gtk_menu_new();
  ml[o_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[option_menu]), mw[o_cascade_menu]);

  mw[o_transform_menu] = gtk_menu_item_new_with_label(_("Transform Options"));
  ml[o_transform_menu] = _("Transform Options");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_transform_menu]);
  gtk_widget_show(mw[o_transform_menu]);
  SG_SIGNAL_CONNECT(mw[o_transform_menu], "activate", options_transform_callback, NULL);

  mw[o_focus_style_menu] = gtk_menu_item_new_with_label(_("Zoom focus"));
  ml[o_focus_style_menu] = _("Zoom focus");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_focus_style_menu]);
  gtk_widget_show(mw[o_focus_style_menu]);

  mw[o_focus_cascade_menu] = gtk_menu_new();
  ml[o_focus_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[o_focus_style_menu]), mw[o_focus_cascade_menu]);

  mw[o_focus_left_menu] = gtk_menu_item_new_with_label(_("window left edge"));
  ml[o_focus_left_menu] = _("window left edge");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_focus_cascade_menu]), mw[o_focus_left_menu]);
  gtk_widget_show(mw[o_focus_left_menu]);
  SG_SIGNAL_CONNECT(mw[o_focus_left_menu], "activate", options_focus_left_callback, NULL);

  mw[o_focus_right_menu] = gtk_menu_item_new_with_label(_("window right edge"));
  ml[o_focus_right_menu] = _("window right edge");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_focus_cascade_menu]), mw[o_focus_right_menu]);
  gtk_widget_show(mw[o_focus_right_menu]);
  SG_SIGNAL_CONNECT(mw[o_focus_right_menu], "activate", options_focus_right_callback, NULL);

  mw[o_focus_middle_menu] = gtk_menu_item_new_with_label(_("window midpoint"));
  ml[o_focus_middle_menu] = _("window midpoint");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_focus_cascade_menu]), mw[o_focus_middle_menu]);
  gtk_widget_show(mw[o_focus_middle_menu]);
  SG_SIGNAL_CONNECT(mw[o_focus_middle_menu], "activate", options_focus_middle_callback, NULL);

  mw[o_focus_active_menu] = gtk_menu_item_new_with_label(_("cursor or selection"));
  ml[o_focus_active_menu] = _("cursor or selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_focus_cascade_menu]), mw[o_focus_active_menu]);
  gtk_widget_show(mw[o_focus_active_menu]);
  SG_SIGNAL_CONNECT(mw[o_focus_active_menu], "activate", options_focus_active_callback, NULL);
  activate_focus_menu(zoom_focus_style(ss));

#if HAVE_EXTENSION_LANGUAGE
  mw[o_save_menu] = gtk_menu_item_new_with_label(_("Save options"));
  ml[o_save_menu] = _("Save options");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_save_menu]);
  gtk_widget_show(mw[o_save_menu]);
  SG_SIGNAL_CONNECT(mw[o_save_menu], "activate", options_save_callback, NULL);

  mw[o_save_state_menu] = gtk_menu_item_new_with_label(_("Save state"));
  ml[o_save_state_menu] = _("Save state");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_save_state_menu]);
  gtk_widget_show(mw[o_save_state_menu]);
  SG_SIGNAL_CONNECT(mw[o_save_state_menu], "activate", options_save_state_callback, NULL);
#endif


  /* HELP MENU */
  mw[help_menu] = gtk_menu_item_new_with_label(_("Help"));
  ml[help_menu] = _("Help");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[help_menu]);
  gtk_widget_show(mw[help_menu]);
  gtk_menu_item_set_right_justified(GTK_MENU_ITEM(mw[help_menu]), true);

  mw[h_cascade_menu] = gtk_menu_new();
  ml[h_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[help_menu]), mw[h_cascade_menu]);

  mw[h_about_snd_menu] = gtk_menu_item_new_with_label(_("About Snd"));
  ml[h_about_snd_menu] = _("About Snd");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_about_snd_menu]);
  gtk_widget_show(mw[h_about_snd_menu]);
  SG_SIGNAL_CONNECT(mw[h_about_snd_menu], "activate", help_about_snd_callback, NULL);

  mw[h_init_file_menu] = gtk_menu_item_new_with_label(_("Customization"));
  ml[h_init_file_menu] = _("Customization");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_init_file_menu]);
  gtk_widget_show(mw[h_init_file_menu]);
  SG_SIGNAL_CONNECT(mw[h_init_file_menu], "activate", help_init_file_callback, NULL);

  mw[h_controls_menu] = gtk_menu_item_new_with_label(_("Control Panel"));
  ml[h_controls_menu] = _("Control Panel");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_controls_menu]);
  gtk_widget_show(mw[h_controls_menu]);
  SG_SIGNAL_CONNECT(mw[h_controls_menu], "activate", help_controls_callback, NULL);

  mw[h_keys_menu] = gtk_menu_item_new_with_label(_("Key bindings"));
  ml[h_keys_menu] = _("Key bindings");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_keys_menu]);
  gtk_widget_show(mw[h_keys_menu]);
  SG_SIGNAL_CONNECT(mw[h_keys_menu], "activate", help_keys_callback, NULL);

  mw[h_recording_menu] = gtk_menu_item_new_with_label(_("Record"));
  ml[h_recording_menu] = _("Record");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_recording_menu]);
  gtk_widget_show(mw[h_recording_menu]);
  SG_SIGNAL_CONNECT(mw[h_recording_menu], "activate", help_recording_callback, NULL);

  mw[h_play_menu] = gtk_menu_item_new_with_label(_("Play"));
  ml[h_play_menu] = _("Play");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_play_menu]);
  gtk_widget_show(mw[h_play_menu]);
  SG_SIGNAL_CONNECT(mw[h_play_menu], "activate", help_play_callback, NULL);

  mw[h_save_menu] = gtk_menu_item_new_with_label(_("Save"));
  ml[h_save_menu] = _("Save");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_save_menu]);
  gtk_widget_show(mw[h_save_menu]);
  SG_SIGNAL_CONNECT(mw[h_save_menu], "activate", help_save_callback, NULL);

  mw[h_mix_menu] = gtk_menu_item_new_with_label(_("Mix"));
  ml[h_mix_menu] = _("Mix");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_mix_menu]);
  gtk_widget_show(mw[h_mix_menu]);
  SG_SIGNAL_CONNECT(mw[h_mix_menu], "activate", help_mix_callback, NULL);

  mw[h_track_menu] = gtk_menu_item_new_with_label(_("Track"));
  ml[h_track_menu] = _("Track");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_track_menu]);
  gtk_widget_show(mw[h_track_menu]);
  SG_SIGNAL_CONNECT(mw[h_track_menu], "activate", help_track_callback, NULL);

  mw[h_resample_menu] = gtk_menu_item_new_with_label(_("Resample"));
  ml[h_resample_menu] = _("Resample");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_resample_menu]);
  gtk_widget_show(mw[h_resample_menu]);
  SG_SIGNAL_CONNECT(mw[h_resample_menu], "activate", help_resample_callback, NULL);

  mw[h_fft_menu] = gtk_menu_item_new_with_label(_("FFT"));
  ml[h_fft_menu] = _("FFT");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_fft_menu]);
  gtk_widget_show(mw[h_fft_menu]);
  SG_SIGNAL_CONNECT(mw[h_fft_menu], "activate", help_fft_callback, NULL);

  mw[h_filter_menu] = gtk_menu_item_new_with_label(_("Filter"));
  ml[h_filter_menu] = _("Filter");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_filter_menu]);
  gtk_widget_show(mw[h_filter_menu]);
  SG_SIGNAL_CONNECT(mw[h_filter_menu], "activate", help_filter_callback, NULL);

  mw[h_reverb_menu] = gtk_menu_item_new_with_label(_("Reverb"));
  ml[h_reverb_menu] = _("Reverb");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_reverb_menu]);
  gtk_widget_show(mw[h_reverb_menu]);
  SG_SIGNAL_CONNECT(mw[h_reverb_menu], "activate", help_reverb_callback, NULL);

  mw[h_env_menu] = gtk_menu_item_new_with_label(_("Envelope"));
  ml[h_env_menu] = _("Envelope");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_env_menu]);
  gtk_widget_show(mw[h_env_menu]);
  SG_SIGNAL_CONNECT(mw[h_env_menu], "activate", help_env_callback, NULL);

  mw[h_marks_menu] = gtk_menu_item_new_with_label(_("Mark"));
  ml[h_marks_menu] = _("Mark");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_marks_menu]);
  gtk_widget_show(mw[h_marks_menu]);
  SG_SIGNAL_CONNECT(mw[h_marks_menu], "activate", help_marks_callback, NULL);

  mw[h_insert_menu] = gtk_menu_item_new_with_label(_("Insert"));
  ml[h_insert_menu] = _("Insert");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_insert_menu]);
  gtk_widget_show(mw[h_insert_menu]);
  SG_SIGNAL_CONNECT(mw[h_insert_menu], "activate", help_insert_callback, NULL);

  mw[h_delete_menu] = gtk_menu_item_new_with_label(_("Delete"));
  ml[h_delete_menu] = _("Delete");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_delete_menu]);
  gtk_widget_show(mw[h_delete_menu]);
  SG_SIGNAL_CONNECT(mw[h_delete_menu], "activate", help_delete_callback, NULL);

  mw[h_undo_menu] = gtk_menu_item_new_with_label(_("Undo and redo"));
  ml[h_undo_menu] = _("Undo and redo");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_undo_menu]);
  gtk_widget_show(mw[h_undo_menu]);
  SG_SIGNAL_CONNECT(mw[h_undo_menu], "activate", help_undo_callback, NULL);

  mw[h_find_menu] = gtk_menu_item_new_with_label(_("Search"));
  ml[h_find_menu] = _("Search");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_find_menu]);
  gtk_widget_show(mw[h_find_menu]);
  SG_SIGNAL_CONNECT(mw[h_find_menu], "activate", help_find_callback, NULL);

  mw[h_sync_menu] = gtk_menu_item_new_with_label(_("Sync"));
  ml[h_sync_menu] = _("Sync");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_sync_menu]);
  gtk_widget_show(mw[h_sync_menu]);
  SG_SIGNAL_CONNECT(mw[h_sync_menu], "activate", help_sync_callback, NULL);

  mw[h_sound_files_menu] = gtk_menu_item_new_with_label(_("Headers and Data"));
  ml[h_sound_files_menu] = _("Headers and Data");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_sound_files_menu]);
  gtk_widget_show(mw[h_sound_files_menu]);
  SG_SIGNAL_CONNECT(mw[h_sound_files_menu], "activate", help_sound_files_callback, NULL);

  mw[h_debug_menu] = gtk_menu_item_new_with_label(_("Debugging"));
  ml[h_debug_menu] = _("Debugging");
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_debug_menu]);
  gtk_widget_show(mw[h_debug_menu]);
  SG_SIGNAL_CONNECT(mw[h_debug_menu], "activate", help_debug_callback, NULL);

#ifndef SND_AS_WIDGET
  gtk_menu_set_accel_group(GTK_MENU(mw[f_cascade_menu]), accel_group);
  gtk_widget_add_accelerator(mw[f_open_menu], "activate", accel_group, GDK_O, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_close_menu], "activate", accel_group, GDK_C, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_save_menu], "activate", accel_group, GDK_S, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_save_as_menu], "activate", accel_group, GDK_A, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_revert_menu], "activate", accel_group, GDK_R, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_mix_menu], "activate", accel_group, GDK_M, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_update_menu], "activate", accel_group, GDK_U, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  gtk_widget_add_accelerator(mw[f_view_menu], "activate", accel_group, GDK_V, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
  /* M-p and M-n are for the line history handlers */
  /* gtk_widget_add_accelerator(mw[f_new_menu], "activate", accel_group, GDK_N, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE); */
  /* gtk_widget_add_accelerator(mw[f_print_menu], "activate", accel_group, GDK_P, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE); */
#if HAVE_EXTENSION_LANGUAGE
  gtk_widget_add_accelerator(mw[e_find_menu], "activate", accel_group, GDK_F, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
#endif
  gtk_widget_add_accelerator(mw[o_transform_menu], "activate", accel_group, GDK_T, GDK_MOD1_MASK, GTK_ACCEL_VISIBLE);
#endif

  return(mw[menu_menu]);
}

static GtkWidget *added_menus[MAX_MAIN_MENUS];
static int new_menu = 5;
static GtkWidget **added_options = NULL;
static char **added_options_names = NULL;
static int *added_options_menus = NULL;
static int added_options_size = 0;
static int added_options_pos = 0;
static int *added_options_callb = NULL;
enum {FILE_MENU, EDIT_MENU, VIEW_MENU, OPTIONS_MENU, HELP_MENU, POPUP_MENU};
#define INVALID_MENU -1

static int callb2option(int callb)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if (added_options_callb[i] == callb)
      return(i);
  return(-1);
}

static void SND_callback(GtkWidget *w, gpointer info) 
{
  int callb, opt;
  callb = get_user_int_data(G_OBJECT(w));
  opt = callb2option(callb);
  if (opt != -1)
    g_snd_callback(callb);
}

GtkWidget *menu_widget(int which_menu)
{
  switch (which_menu)
    {
    case FILE_MENU:    return(mw[file_menu]); break;
    case EDIT_MENU:    return(mw[edit_menu]); break;
    case VIEW_MENU:    return(mw[view_menu]); break;
    case OPTIONS_MENU: return(mw[option_menu]); break;
    case HELP_MENU:    return(mw[help_menu]); break;
    case POPUP_MENU:   return(popup_menu); break;
    default:           return(added_menus[which_menu]); break;
    }
  return(NULL);
}

static void add_option(GtkWidget *w, int which_menu, char *label, int callb)
{
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
	  int i;
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
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(added_options_names[i]) &&
	(strcmp(label, added_options_names[i]) == 0) && 
	(added_options[i]))
      {
	unprotect_callback(added_options_callb[i]);
	gtk_widget_hide(added_options[i]); /* destroy here causes segfault in gtk2? */
	added_options[i] = NULL;
	added_options_menus[i] = -1;
	FREE(added_options_names[i]);
	added_options_names[i] = NULL;
	return(0);
      }
  for (i = 0; i < NUM_MENU_WIDGETS; i++)
    if (ml[i])
      if (strcmp(label, ml[i]) == 0)
	{
	  gtk_widget_hide(mw[i]);
	  return(0);
	}
  for (i = 0; i < NUM_POPUP_CHILDREN; i++)
    if (pl[i])
      if (strcmp(label, pl[i]) == 0)
	{
	  gtk_widget_hide(popup_children[i]);
	  return(0);
	}
  return(INVALID_MENU);
}

int g_add_to_main_menu(char *label, int slot)
{
  GtkWidget *m, *mc;
  if (new_menu >= MAX_MAIN_MENUS) return(INVALID_MENU);
  m = gtk_menu_item_new_with_label(label);
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), m);
  gtk_widget_show(m);
  set_user_int_data(G_OBJECT(m), slot);
  if (slot >= 0) 
    {
      SG_SIGNAL_CONNECT(m, "activate", SND_callback, NULL);
      add_option(m, new_menu + 1, label, slot);
    }
  mc = gtk_menu_new();
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(m), mc);
  new_menu++;
  added_menus[new_menu] = mc;
  return(new_menu);
}

GtkWidget *get_help_menu_widget(void)
{
  return(mw[h_cascade_menu]);
}

GtkWidget *g_add_to_menu(int which_menu, char *label, int callb, int position)
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
      else return(NULL);
      break;
    }
   if (label)
     m = gtk_menu_item_new_with_label(label);
   else m = gtk_menu_item_new();
   if (position >= 0)
     gtk_menu_shell_insert(GTK_MENU_SHELL(menw), m, position);
   else gtk_menu_shell_append(GTK_MENU_SHELL(menw), m);
   gtk_widget_show(m);
   if (label)
     {
       set_user_int_data(G_OBJECT(m), callb);
       SG_SIGNAL_CONNECT(m, "activate", SND_callback, NULL);
       add_option(m, which_menu, label, callb);
     }
  return(m);
}

int g_remove_from_menu(int which_menu, char *label)
{
  return(remove_option(which_menu, label));
}


/* -------------------------------- POPUP MENU -------------------------------- */

static bool stopping = false;

static void popup_play_callback(GtkWidget *w, gpointer info) 
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
	  play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
	  stopping = true;
	  set_button_label(w, _("Stop playing"));
	}
    }
  gtk_widget_hide(popup_menu);
}

void reflect_play_stop_in_popup_menu(void)
{
  stopping = false;
  if (popup_menu)
    set_button_label(popup_children[W_pop_play], _("Play"));
}

static void popup_save_callback(GtkWidget *w, gpointer info) 
{
  save_edits(any_selected_sound(), NULL);
  gtk_widget_hide(popup_menu);
}

static void popup_undo_callback(GtkWidget *w, gpointer info) 
{
  undo_edit_with_sync(current_channel(), 1);
  gtk_widget_hide(popup_menu);
}

static void popup_redo_callback(GtkWidget *w, gpointer info) 
{
  redo_edit_with_sync(current_channel(), 1);
  gtk_widget_hide(popup_menu);
}

static void popup_info_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) display_info(sp);
  gtk_widget_hide(popup_menu);
}

static void create_popup_menu(guint button, Tempus time)
{
  if (!popup_menu)
    {
      bool undo_possible = false, redo_possible = false;
      chan_info *selcp = NULL;
      selcp = selected_channel();
      if (selcp)
	{
	  undo_possible = (selcp->edit_ctr > 0);
	  redo_possible = ((selcp->edit_size > (selcp->edit_ctr + 1)) && 
			   (selcp->edits[selcp->edit_ctr + 1]));
	}
      popup_menu = gtk_menu_new();
      gtk_widget_show(popup_menu);

      popup_children[W_pop_play] = gtk_menu_item_new_with_label(_("Play"));
      pl[W_pop_play] = _("Play");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_play]);
      SG_SIGNAL_CONNECT(popup_children[W_pop_play], "activate", popup_play_callback, NULL);
      set_sensitive(popup_children[W_pop_play], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_play]);

      popup_children[W_pop_undo] = gtk_menu_item_new_with_label(_("Undo"));
      pl[W_pop_undo] = _("Undo");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_undo]);
      SG_SIGNAL_CONNECT(popup_children[W_pop_undo], "activate", popup_undo_callback, NULL);
      set_sensitive(popup_children[W_pop_undo], undo_possible);
      gtk_widget_show(popup_children[W_pop_undo]);
      
      popup_children[W_pop_redo] = gtk_menu_item_new_with_label(_("Redo"));
      pl[W_pop_redo] = _("Redo");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_redo]);
      SG_SIGNAL_CONNECT(popup_children[W_pop_redo], "activate", popup_redo_callback, NULL);
      set_sensitive(popup_children[W_pop_redo], redo_possible);
      gtk_widget_show(popup_children[W_pop_redo]);
      
      popup_children[W_pop_save] = gtk_menu_item_new_with_label(_("Save"));
      pl[W_pop_save] = _("Save");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_save]);
      SG_SIGNAL_CONNECT(popup_children[W_pop_save], "activate", popup_save_callback, NULL);
      set_sensitive(popup_children[W_pop_save], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_save]);

      popup_children[W_pop_info] = gtk_menu_item_new_with_label(_("Info"));
      pl[W_pop_info] = _("Info");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_info]);
      SG_SIGNAL_CONNECT(popup_children[W_pop_info], "activate", popup_info_callback, NULL);
      set_sensitive(popup_children[W_pop_info], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_info]);
    }
  gtk_menu_popup(GTK_MENU(popup_menu), NULL, NULL, NULL, NULL, button, time);
}

static XEN gtk_popup_hook;

void popup_menu_from(GtkWidget *w, GdkEventButton *ev, gpointer data, int snd, int chn)
{
  if (XEN_HOOKED(gtk_popup_hook))
    {
      XEN result;
      result = run_progn_hook(gtk_popup_hook,
			      XEN_LIST_5(XEN_WRAP_WIDGET(w),
					 XEN_WRAP_EVENT((GdkEvent *)ev),
					 C_TO_XEN_ULONG(data),
					 C_TO_XEN_INT(snd),
					 C_TO_XEN_INT(chn)),
				  "gtk-popup-hook");
      if (XEN_TRUE_P(result)) return;
    }
  create_popup_menu(ev->button, ev->time);
}

static XEN g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets "): a list of the top level menu widgets: ((0)main (1)file (2)edit (3)view (4)options (5)help)"
  return(XEN_CONS(XEN_WRAP_WIDGET(mw[menu_menu]),
	  XEN_CONS(XEN_WRAP_WIDGET(mw[file_menu]),
           XEN_CONS(XEN_WRAP_WIDGET(mw[edit_menu]),
            XEN_CONS(XEN_WRAP_WIDGET(mw[view_menu]),
             XEN_CONS(XEN_WRAP_WIDGET(mw[option_menu]),
              XEN_CONS(XEN_WRAP_WIDGET(mw[help_menu]),
	       XEN_EMPTY_LIST)))))));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_menu_widgets_w, g_menu_widgets)
#else
#define g_menu_widgets_w g_menu_widgets
#endif

void g_init_gxmenu(void)
{
  #define H_gtk_popup_hook "gtk-popup-hook (widget event data snd chn): called upon middle button click. \
If it returns other than #t, the normal Snd popup menu is posted."

  XEN_DEFINE_HOOK(gtk_popup_hook, "gtk-popup-hook", 5, H_gtk_popup_hook);
  XEN_DEFINE_PROCEDURE(S_menu_widgets, g_menu_widgets_w, 0, 0, 0, H_menu_widgets);
}


