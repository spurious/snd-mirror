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

#define NUM_MENU_WIDGETS 97
static GtkWidget *mw[NUM_MENU_WIDGETS];
static const char *ml[NUM_MENU_WIDGETS];

enum {W_pop_play, W_pop_undo, W_pop_redo, W_pop_save, W_pop_info};
#define NUM_POPUP_CHILDREN 6
static GtkWidget *popup_menu = NULL;
static GtkWidget *popup_children[NUM_POPUP_CHILDREN];
static const char *pl[NUM_POPUP_CHILDREN];

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
    res = run_and_hook(menu_hook, 
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
  IF_MENU_HOOK("File", "Open") make_open_file_dialog((snd_state *)cD, FALSE, TRUE);
}

static void file_view_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "View") make_open_file_dialog((snd_state *)cD, TRUE, TRUE);
}

static void file_new_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "New") new_file_from_menu((snd_state *)cD);
}

static void file_record_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Record") snd_record_file((snd_state *)cD);
}

static void file_close_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Close") close_file_from_menu((snd_state *)cD);
}

static void file_save_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Save") save_file_from_menu((snd_state *)cD);
}

static void file_update_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Update") update_file_from_menu((snd_state *)cD);
}

static void file_save_as_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Save as") make_file_save_as_dialog((snd_state *)cD);
}

static void file_revert_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Revert") revert_file_from_menu((snd_state *)cD);
}

static void file_exit_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Exit") exit_from_menu((snd_state *)cD);
}

static void file_mix_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Mix") make_mix_file_dialog((snd_state *)cD, TRUE);
}

static void file_print_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("File", "Print") file_print_callback(w, cD);
}


/* -------------------------------- EDIT MENU -------------------------------- */


static void edit_mix_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Edit", "Mix Selection") add_selection_or_region((snd_state *)cD, 0, selected_channel(((snd_state *)cD)), "Edit: mix");
}

static void edit_envelope_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Edit", "Edit Envelope") create_envelope_editor((snd_state *)cD);
}

static void edit_cut_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Edit", "Delete Selection") delete_selection("Edit: Cut", UPDATE_DISPLAY);
}

static void edit_paste_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Edit", "Insert Selection") insert_selection_from_menu((snd_state *)cD);
}

static void edit_save_as_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Edit", "Save Selection") make_edit_save_as_dialog((snd_state *)cD);
}

static void edit_select_all_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Edit", "Select all") select_all(current_channel((snd_state *)cD));
}

static void edit_undo_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Edit", "Undo") undo_edit_with_sync(current_channel((snd_state *)cD), 1);
}

static void edit_redo_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Edit", "Redo") redo_edit_with_sync(current_channel((snd_state *)cD), 1);
}

static int selection_play_stop = FALSE;

static void edit_play_callback(GtkWidget *w, gpointer cD) 
{
  if (selection_play_stop)
    {
      stop_playing_all_sounds();
    }
  else
    {
      IF_MENU_HOOK("Edit", "Play Selection") 
	{
	  set_menu_label(edit_play_menu(), "Stop");
	  selection_play_stop = TRUE;
	  play_selection(IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "play selection", 0);
	}
    }
}

void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu(), "Play Selection");
  selection_play_stop = FALSE;
}

static void edit_header_callback_1(GtkWidget *w, gpointer cD)
{
  snd_state *ss = (snd_state *)cD;
  snd_info *sp;
  IF_MENU_HOOK("Edit", "Edit Header") 
    {
      sp = selected_sound(ss);
      if (sp) edit_header(sp);
    }
}

#if HAVE_EXTENSION_LANGUAGE
static void edit_find_callback_1(GtkWidget *w, gpointer cD)
{
  IF_MENU_HOOK("Edit", "Find") edit_find_callback(w, cD);
}
#endif


/* -------------------------------- VIEW MENU -------------------------------- */

static void view_separate_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "separate") set_channel_style((snd_state *)cD, CHANNELS_SEPARATE);
}

static void view_combined_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "combined") set_channel_style((snd_state *)cD, CHANNELS_COMBINED);
}

static void view_superimposed_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "superimposed") set_channel_style((snd_state *)cD, CHANNELS_SUPERIMPOSED);
}

static void view_dots_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "dots") set_graph_style((snd_state *)cD, GRAPH_DOTS);
}

static void view_lines_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "lines") set_graph_style((snd_state *)cD, GRAPH_LINES);
}

static void view_filled_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "filled") set_graph_style((snd_state *)cD, GRAPH_FILLED);
}

static void view_dots_and_lines_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "dots and lines") set_graph_style((snd_state *)cD, GRAPH_DOTS_AND_LINES);
}

static void view_lollipops_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "lollipops") set_graph_style((snd_state *)cD, GRAPH_LOLLIPOPS);
}

static void view_zero_callback(GtkWidget *w, gpointer cD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("View", "Show Y = 0") set_show_y_zero(ss, (!(show_y_zero(ss))));
}

static void view_cursor_callback(GtkWidget *w, gpointer cD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("View", "Verbose cursor") set_verbose_cursor(ss, (!(verbose_cursor(ss))));
}

static void view_ctrls_callback(GtkWidget *w, gpointer cD)
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("View", "Show controls") 
    {
      if (ss->ctrls_height < 100) 
	show_controls(ss); 
      else hide_controls(ss); /* snd-xmain.c */
    }
}

#if HAVE_EXTENSION_LANGUAGE
static void view_listener_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "Show listener") handle_listener((snd_state *)cD, listener_height() < 5);
}
#endif

static void view_mix_panel_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "Mix Panel") make_mix_panel((snd_state *)cD);
}

static void view_error_history_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "Error History") show_snd_errors((snd_state *)cD);
}

static void view_region_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "Regions") view_region_callback(w, cD);
}

static void view_orientation_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "Orientation") view_orientation_callback(w, cD);
}

static void view_color_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "Color") view_color_callback(w, cD);
}

static void view_files_callback_1(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("View", "Files") view_files_callback(w, cD);
}


/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_transform_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Options", "Transform Options") fire_up_transform_dialog((snd_state *)cD, TRUE);
}

#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Options", "Save options") save_options_from_menu((snd_state *)cD);
}
#endif

static void options_focus_right_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK("Options", "window right edge") activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_RIGHT);
}

static void options_focus_left_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK("Options", "window left edge") activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_LEFT);
}

static void options_focus_middle_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK("Options", "window midpoint") activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_MIDDLE);
}

static void options_focus_active_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK("Options", "cursor or selection") activate_focus_menu((snd_state *)cD, ZOOM_FOCUS_ACTIVE);
}

static void options_speed_float_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK("Options", "float") activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_FLOAT);
}

static void options_speed_ratio_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK("Options", "ratio") activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_RATIO);
}

static void options_speed_semitone_callback(GtkWidget *w, gpointer cD, gpointer Data) 
{
  IF_MENU_HOOK("Options", "semitones") activate_speed_in_menu((snd_state *)cD, SPEED_CONTROL_AS_SEMITONE);
}

static void options_x_axis_seconds_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Options", "seconds") set_x_axis_style((snd_state *)cD, X_AXIS_IN_SECONDS);
}

static void options_x_axis_beats_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Options", "beats") set_x_axis_style((snd_state *)cD, X_AXIS_IN_BEATS);
}

static void options_x_axis_samples_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Options", "samples") set_x_axis_style((snd_state *)cD, X_AXIS_IN_SAMPLES);
}

static void options_x_axis_percentage_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Options", "percentage") set_x_axis_style((snd_state *)cD, X_AXIS_AS_PERCENTAGE);
}

#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(GtkWidget *w, gpointer cD) 
{
  IF_MENU_HOOK("Options", "Save state") save_state_from_menu((snd_state *)cD);
}
#endif



/* -------------------------------- HELP MENU -------------------------------- */

static void help_about_snd_callback(GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Overview") about_snd_help((snd_state *)cD);}
static void help_fft_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "FFT") fft_help((snd_state *)cD);}
static void help_find_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Find") find_help((snd_state *)cD);}
static void help_undo_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Undo and redo") undo_help((snd_state *)cD);}
static void help_sync_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Sync") sync_help((snd_state *)cD);}
static void help_speed_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Speed") speed_help((snd_state *)cD);}
static void help_expand_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Expand") expand_help((snd_state *)cD);}
static void help_reverb_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Reverb") reverb_help((snd_state *)cD);}
static void help_contrast_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Contrast") contrast_help((snd_state *)cD);}
static void help_env_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Envelope") env_help((snd_state *)cD);}
static void help_marks_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Marks") marks_help((snd_state *)cD);}
static void help_mix_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Mixing") mix_help((snd_state *)cD);}
static void help_sound_files_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Formats") sound_files_help((snd_state *)cD);}
static void help_init_file_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Customization") init_file_help((snd_state *)cD);}
static void help_recording_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "Recording") recording_help((snd_state *)cD);}

static void help_clm_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "CLM") clm_help((snd_state *)cD);}

static void help_news_callback (GtkWidget *w, gpointer cD) {IF_MENU_HOOK("Help", "News") news_help((snd_state *)cD);}


void check_menu_labels(int key, int state, int extended) {}


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
  ml[menu_menu] = NULL;
  gtk_box_pack_start(GTK_BOX(MAIN_PANE(ss)), mw[menu_menu], FALSE, TRUE, 0);
  gtk_widget_show(mw[menu_menu]);


  /* FILE MENU */
  mw[file_menu] = gtk_menu_item_new_with_label("File");
  ml[file_menu] = "File";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[file_menu]);
  gtk_widget_show(mw[file_menu]);

  mw[f_cascade_menu] = gtk_menu_new();
  ml[f_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[file_menu]), mw[f_cascade_menu]);

  mw[f_open_menu] = gtk_menu_item_new_with_label("Open");
  ml[f_open_menu] = "Open";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_open_menu]);
  gtk_widget_show(mw[f_open_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_open_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_open_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_open_callback), (gpointer)ss, 0),
				 0);

  mw[f_close_menu] = gtk_menu_item_new_with_label("Close");
  ml[f_close_menu] = "Close";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_close_menu]);
  gtk_widget_show(mw[f_close_menu]);
  set_sensitive(mw[f_close_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_close_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_close_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_close_callback), (gpointer)ss, 0),
				 0);
  
  mw[f_save_menu] = gtk_menu_item_new_with_label("Save");
  ml[f_save_menu] = "Save";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_save_menu]);
  gtk_widget_show(mw[f_save_menu]);
  set_sensitive(mw[f_save_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_save_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_save_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_save_callback), (gpointer)ss, 0),
				 0);
  
  mw[f_save_as_menu] = gtk_menu_item_new_with_label("Save as");
  ml[f_save_as_menu] = "Save as";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_save_as_menu]);
  gtk_widget_show(mw[f_save_as_menu]);
  set_sensitive(mw[f_save_as_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_save_as_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_save_as_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_save_as_callback), (gpointer)ss, 0),
				 0);
  
  mw[f_revert_menu] = gtk_menu_item_new_with_label("Revert");
  ml[f_revert_menu] = "Revert";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_revert_menu]);
  gtk_widget_show(mw[f_revert_menu]);
  set_sensitive(mw[f_revert_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_revert_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_revert_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_revert_callback), (gpointer)ss, 0),
				 0);
  
  mw[f_mix_menu] = gtk_menu_item_new_with_label("Mix");
  ml[f_mix_menu] = "Mix";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_mix_menu]);
  gtk_widget_show(mw[f_mix_menu]);
  set_sensitive(mw[f_mix_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_mix_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_mix_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_mix_callback_1), (gpointer)ss, 0),
				 0);

  mw[f_update_menu] = gtk_menu_item_new_with_label("Update");
  ml[f_update_menu] = "Update";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_update_menu]);
  gtk_widget_show(mw[f_update_menu]);
  set_sensitive(mw[f_update_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_update_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_update_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_update_callback), (gpointer)ss, 0),
				 0);

  mw[f_new_menu] = gtk_menu_item_new_with_label("New");
  ml[f_new_menu] = "New";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_new_menu]);
  gtk_widget_show(mw[f_new_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_new_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_new_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_new_callback), (gpointer)ss, 0),
				 0);

  mw[f_record_menu] = gtk_menu_item_new_with_label("Record");
  ml[f_record_menu] = "Record";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_record_menu]);
  gtk_widget_show(mw[f_record_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_record_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_record_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_record_callback), (gpointer)ss, 0),
				 0);

  mw[f_view_menu] = gtk_menu_item_new_with_label("View");
  ml[f_view_menu] = "View";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_view_menu]);
  gtk_widget_show(mw[f_view_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_view_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_view_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_view_callback), (gpointer)ss, 0),
				 0);

  mw[f_print_menu] = gtk_menu_item_new_with_label("Print");
  ml[f_print_menu] = "Print";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_print_menu]);
  gtk_widget_show(mw[f_print_menu]);
  set_sensitive(mw[f_print_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_print_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_print_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_print_callback_1), (gpointer)ss, 0),
				 0);

  mw[f_sep_menu] = gtk_menu_item_new();
  ml[f_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_sep_menu]);
  gtk_widget_show(mw[f_sep_menu]);

  mw[f_exit_menu] = gtk_menu_item_new_with_label("Exit");
  ml[f_exit_menu] = "Exit";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[f_cascade_menu]), mw[f_exit_menu]);
  gtk_widget_show(mw[f_exit_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[f_exit_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[f_exit_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(file_exit_callback), (gpointer)ss, 0),
				 0);

  /* EDIT MENU */
  mw[edit_menu] = gtk_menu_item_new_with_label("Edit");
  ml[edit_menu] = "Edit";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[edit_menu]);
  gtk_widget_show(mw[edit_menu]);

  mw[e_cascade_menu] = gtk_menu_new();
  ml[e_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[edit_menu]), mw[e_cascade_menu]);
  
  mw[e_undo_menu] = gtk_menu_item_new_with_label("Undo");
  ml[e_undo_menu] = "Undo";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_undo_menu]);
  gtk_widget_show(mw[e_undo_menu]);
  set_sensitive(mw[e_undo_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_undo_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_undo_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_undo_callback), (gpointer)ss, 0),
				 0);

  mw[e_redo_menu] = gtk_menu_item_new_with_label("Redo");
  ml[e_redo_menu] = "Redo";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_redo_menu]);
  gtk_widget_show(mw[e_redo_menu]);
  set_sensitive(mw[e_redo_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_redo_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_redo_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_redo_callback), (gpointer)ss, 0),
				 0);

#if HAVE_EXTENSION_LANGUAGE
  mw[e_find_menu] = gtk_menu_item_new_with_label("Find");
  ml[e_find_menu] = "Find";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_find_menu]);
  gtk_widget_show(mw[e_find_menu]);
  set_sensitive(mw[e_find_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_find_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_find_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_find_callback_1), (gpointer)ss, 0),
				 0);
#endif

  mw[e_select_sep_menu] = gtk_menu_item_new();
  ml[e_select_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_select_sep_menu]);
  gtk_widget_show(mw[e_select_sep_menu]);

  mw[e_cut_menu] = gtk_menu_item_new_with_label("Delete Selection");
  ml[e_cut_menu] = "Delete Selection";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_cut_menu]);
  gtk_widget_show(mw[e_cut_menu]);
  set_sensitive(mw[e_cut_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_cut_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_cut_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_cut_callback), (gpointer)ss, 0),
				 0);

  mw[e_paste_menu] = gtk_menu_item_new_with_label("Insert Selection");
  ml[e_paste_menu] = "Insert Selection";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_paste_menu]);
  gtk_widget_show(mw[e_paste_menu]);
  set_sensitive(mw[e_paste_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_paste_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_paste_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_paste_callback), (gpointer)ss, 0),
				 0);

  mw[e_mix_menu] = gtk_menu_item_new_with_label("Mix Selection");
  ml[e_mix_menu] = "Mix Selection";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_mix_menu]);
  gtk_widget_show(mw[e_mix_menu]);
  set_sensitive(mw[e_mix_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_mix_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_mix_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_mix_callback), (gpointer)ss, 0),
				 0);

  mw[e_play_menu] = gtk_menu_item_new_with_label("Play Selection");
  ml[e_play_menu] = "Play Selection";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_play_menu]);
  gtk_widget_show(mw[e_play_menu]);
  set_sensitive(mw[e_play_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_play_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_play_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_play_callback), (gpointer)ss, 0),
				 0);

  mw[e_save_as_menu] = gtk_menu_item_new_with_label("Save Selection");
  ml[e_save_as_menu] = "Save Selection";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_save_as_menu]);
  gtk_widget_show(mw[e_save_as_menu]);
  set_sensitive(mw[e_save_as_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_save_as_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_save_as_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_save_as_callback), (gpointer)ss, 0),
				 0);

  mw[e_select_all_menu] = gtk_menu_item_new_with_label("Select all");
  ml[e_select_all_menu] = "Select all";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_select_all_menu]);
  gtk_widget_show(mw[e_select_all_menu]);
  set_sensitive(mw[e_select_all_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_select_all_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_select_all_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_select_all_callback), (gpointer)ss, 0),
				 0);

  mw[e_edit_sep_menu] = gtk_menu_item_new();
  ml[e_edit_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_edit_sep_menu]);
  gtk_widget_show(mw[e_edit_sep_menu]);

  mw[e_edenv_menu] = gtk_menu_item_new_with_label("Edit Envelope");
  ml[e_edenv_menu] = "Edit Envelope";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_edenv_menu]);
  gtk_widget_show(mw[e_edenv_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_edenv_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_edenv_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_envelope_callback), (gpointer)ss, 0),
				 0);

  mw[e_header_menu] = gtk_menu_item_new_with_label("Edit Header");
  ml[e_header_menu] = "Edit Header";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[e_cascade_menu]), mw[e_header_menu]);
  gtk_widget_show(mw[e_header_menu]);
  set_sensitive(mw[e_header_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[e_header_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[e_header_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(edit_header_callback_1), (gpointer)ss, 0),
				 0);


  /* VIEW MENU */
  mw[view_menu] = gtk_menu_item_new_with_label("View");
  ml[view_menu] = "View";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[view_menu]);
  gtk_widget_show(mw[view_menu]);

  mw[v_cascade_menu] = gtk_menu_new();
  ml[v_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[view_menu]), mw[v_cascade_menu]);

  mw[v_ctrls_menu] = gtk_menu_item_new_with_label("Show controls");
  ml[v_ctrls_menu] = "Show controls";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_ctrls_menu]);
  gtk_widget_show(mw[v_ctrls_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_ctrls_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_ctrls_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_ctrls_callback), (gpointer)ss, 0),
				 0);

#if 0
  mw[v_equalize_panes_menu] = gtk_menu_item_new_with_label("Equalize Panes");
  ml[v_equalize_panes_menu] = "Equalize Panes";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_equalize_panes_menu]);
  gtk_widget_show(mw[v_equalize_panes_menu]);
  set_sensitive(mw[v_equalize_panes_menu], FALSE);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_equalize_panes_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_equalize_panes_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_equalize_panes_callback), (gpointer)ss, 0),
				 0);
#endif

#if HAVE_EXTENSION_LANGUAGE
  mw[v_listener_menu] = gtk_menu_item_new_with_label("Open listener");
  ml[v_listener_menu] = "Open listener";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_listener_menu]);
  gtk_widget_show(mw[v_listener_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_listener_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_listener_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_listener_callback), (gpointer)ss, 0),
				 0);
#endif

  mw[v_mix_panel_menu] = gtk_menu_item_new_with_label("Mix Panel");
  ml[v_mix_panel_menu] = "Mix Panel";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_mix_panel_menu]);
  gtk_widget_show(mw[v_mix_panel_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_mix_panel_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_mix_panel_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_mix_panel_callback), (gpointer)ss, 0),
				 0);
  set_sensitive(mw[v_mix_panel_menu], FALSE);

  mw[v_region_menu] = gtk_menu_item_new_with_label("Regions");
  ml[v_region_menu] = "Regions";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_region_menu]);
  gtk_widget_show(mw[v_region_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_region_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_region_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_region_callback_1), (gpointer)ss, 0),
				 0);
  set_sensitive(mw[v_region_menu], FALSE);

  mw[v_files_menu] = gtk_menu_item_new_with_label("Files");
  ml[v_files_menu] = "Files";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_files_menu]);
  gtk_widget_show(mw[v_files_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_files_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_files_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_files_callback_1), (gpointer)ss, 0),
				 0);

  mw[v_color_menu] = gtk_menu_item_new_with_label("Color");
  ml[v_color_menu] = "Color";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_color_menu]);
  gtk_widget_show(mw[v_color_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_color_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_color_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_color_callback_1), (gpointer)ss, 0),
				 0);

  mw[v_orientation_menu] = gtk_menu_item_new_with_label("Orientation");
  ml[v_orientation_menu] = "Orientation";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_orientation_menu]);
  gtk_widget_show(mw[v_orientation_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_orientation_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_orientation_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_orientation_callback_1), (gpointer)ss, 0),
				 0);

  mw[v_sep2_menu] = gtk_menu_item_new();
  ml[v_sep2_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_sep2_menu]);
  gtk_widget_show(mw[v_sep2_menu]);



  mw[v_graph_style_menu] = gtk_menu_item_new_with_label("Graph style");
  ml[v_graph_style_menu] = "Graph style";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_graph_style_menu]);
  gtk_widget_show(mw[v_graph_style_menu]);

  mw[v_graph_style_cascade_menu] = gtk_menu_new();
  ml[v_graph_style_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_graph_style_menu]), mw[v_graph_style_cascade_menu]);

  mw[v_lines_menu] = gtk_menu_item_new_with_label("lines");
  ml[v_lines_menu] = "lines";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_lines_menu]);
  gtk_widget_show(mw[v_lines_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_lines_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_lines_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_lines_callback), (gpointer)ss, 0),
				 0);
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(mw[v_lines_menu], FALSE);

  mw[v_dots_menu] = gtk_menu_item_new_with_label("dots");
  ml[v_dots_menu] = "dots";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_dots_menu]);
  gtk_widget_show(mw[v_dots_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_dots_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_dots_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_dots_callback), (gpointer)ss, 0),
				 0);
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(mw[v_dots_menu], FALSE);

  mw[v_filled_menu] = gtk_menu_item_new_with_label("filled");
  ml[v_filled_menu] = "filled";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_filled_menu]);
  gtk_widget_show(mw[v_filled_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_filled_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_filled_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_filled_callback), (gpointer)ss, 0),
				 0);
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(mw[v_filled_menu], FALSE);

  mw[v_dots_and_lines_menu] = gtk_menu_item_new_with_label("dots and lines");
  ml[v_dots_and_lines_menu] = "dots and lines";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_dots_and_lines_menu]);
  gtk_widget_show(mw[v_dots_and_lines_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_dots_and_lines_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_dots_and_lines_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_dots_and_lines_callback), (gpointer)ss, 0),
				 0);
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(mw[v_dots_and_lines_menu], FALSE);

  mw[v_lollipops_menu] = gtk_menu_item_new_with_label("lollipops");
  ml[v_lollipops_menu] = "lollipops";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_graph_style_cascade_menu]), mw[v_lollipops_menu]);
  gtk_widget_show(mw[v_lollipops_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_lollipops_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_lollipops_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_lollipops_callback), (gpointer)ss, 0),
				 0);
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(mw[v_lollipops_menu], FALSE);

  mw[v_cursor_menu] = gtk_menu_item_new_with_label("Verbose cursor");
  ml[v_cursor_menu] = "Verbose cursor";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_cursor_menu]);
  gtk_widget_show(mw[v_cursor_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_cursor_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_cursor_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_cursor_callback), (gpointer)ss, 0),
				 0);


  mw[v_combine_menu] = gtk_menu_item_new_with_label("Channel style");
  ml[v_combine_menu] = "Channel style";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_combine_menu]);
  gtk_widget_show(mw[v_combine_menu]);

  mw[v_combine_cascade_menu] = gtk_menu_new();
  ml[v_combine_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_combine_menu]), mw[v_combine_cascade_menu]);

  mw[v_combine_separate_menu] = gtk_menu_item_new_with_label("separate");
  ml[v_combine_separate_menu] = "separate";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_combine_cascade_menu]), mw[v_combine_separate_menu]);
  gtk_widget_show(mw[v_combine_separate_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_combine_separate_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_combine_separate_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_separate_callback), (gpointer)ss, 0),
				 0);
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(mw[v_combine_separate_menu], FALSE);

  mw[v_combine_combined_menu] = gtk_menu_item_new_with_label("combined");
  ml[v_combine_combined_menu] = "combined";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_combine_cascade_menu]), mw[v_combine_combined_menu]);
  gtk_widget_show(mw[v_combine_combined_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_combine_combined_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_combine_combined_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_combined_callback), (gpointer)ss, 0),
				 0);
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(mw[v_combine_combined_menu], FALSE);

  mw[v_combine_superimposed_menu] = gtk_menu_item_new_with_label("superimposed");
  ml[v_combine_superimposed_menu] = "superimposed";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_combine_cascade_menu]), mw[v_combine_superimposed_menu]);
  gtk_widget_show(mw[v_combine_superimposed_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_combine_superimposed_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_combine_superimposed_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_superimposed_callback), (gpointer)ss, 0),
				 0);
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(mw[v_combine_superimposed_menu], FALSE);


  mw[v_zero_menu] = gtk_menu_item_new_with_label("Show Y = 0");
  ml[v_zero_menu] = "Show Y = 0";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_zero_menu]);
  gtk_widget_show(mw[v_zero_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_zero_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_zero_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_zero_callback), (gpointer)ss, 0),
				 0);

  mw[v_x_axis_menu] = gtk_menu_item_new_with_label("X axis units");
  ml[v_x_axis_menu] = "X axis units";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_x_axis_menu]);
  gtk_widget_show(mw[v_x_axis_menu]);

  mw[v_x_axis_cascade_menu] = gtk_menu_new();
  ml[v_x_axis_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_x_axis_menu]), mw[v_x_axis_cascade_menu]);

  mw[v_x_axis_seconds_menu] = gtk_menu_item_new_with_label("seconds");
  ml[v_x_axis_seconds_menu] = "seconds";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_x_axis_cascade_menu]), mw[v_x_axis_seconds_menu]);
  gtk_widget_show(mw[v_x_axis_seconds_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_x_axis_seconds_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_x_axis_seconds_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_x_axis_seconds_callback), (gpointer)ss, 0),
				 0);
  set_sensitive(mw[v_x_axis_seconds_menu], FALSE);

  mw[v_x_axis_samples_menu] = gtk_menu_item_new_with_label("samples");
  ml[v_x_axis_samples_menu] = "samples";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_x_axis_cascade_menu]), mw[v_x_axis_samples_menu]);
  gtk_widget_show(mw[v_x_axis_samples_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_x_axis_samples_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_x_axis_samples_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_x_axis_samples_callback), (gpointer)ss, 0),
				 0);

  mw[v_x_axis_percentage_menu] = gtk_menu_item_new_with_label("percentage");
  ml[v_x_axis_percentage_menu] = "percentage";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_x_axis_cascade_menu]), mw[v_x_axis_percentage_menu]);
  gtk_widget_show(mw[v_x_axis_percentage_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_x_axis_percentage_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_x_axis_percentage_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_x_axis_percentage_callback), (gpointer)ss, 0),
				 0);

  mw[v_x_axis_beats_menu] = gtk_menu_item_new_with_label("beats");
  ml[v_x_axis_beats_menu] = "beats";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_x_axis_cascade_menu]), mw[v_x_axis_beats_menu]);
  gtk_widget_show(mw[v_x_axis_beats_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_x_axis_beats_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_x_axis_beats_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_x_axis_beats_callback), (gpointer)ss, 0),
				 0);

  mw[v_error_history_menu] = gtk_menu_item_new_with_label("Error History");
  ml[v_error_history_menu] = "Error History";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[v_cascade_menu]), mw[v_error_history_menu]);
  gtk_widget_show(mw[v_error_history_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[v_error_history_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[v_error_history_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(view_error_history_callback), (gpointer)ss, 0),
				 0);



  /* OPTIONS MENU */
  mw[option_menu] = gtk_menu_item_new_with_label("Options");
  ml[option_menu] = "Options";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[option_menu]);
  gtk_widget_show(mw[option_menu]);

  mw[o_cascade_menu] = gtk_menu_new();
  ml[o_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[option_menu]), mw[o_cascade_menu]);

  mw[o_transform_menu] = gtk_menu_item_new_with_label("Transform Options");
  ml[o_transform_menu] = "Transform Options";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_transform_menu]);
  gtk_widget_show(mw[o_transform_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_transform_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_transform_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_transform_callback), (gpointer)ss, 0),
				 0);

  mw[o_speed_menu] = gtk_menu_item_new_with_label("Speed style");
  ml[o_speed_menu] = "Speed style";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_speed_menu]);
  gtk_widget_show(mw[o_speed_menu]);

  mw[o_speed_cascade_menu] = gtk_menu_new();
  ml[o_speed_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[o_speed_menu]), mw[o_speed_cascade_menu]);

  mw[o_speed_float_menu] = gtk_menu_item_new_with_label("float");
  ml[o_speed_float_menu] = "float";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_speed_cascade_menu]), mw[o_speed_float_menu]);
  gtk_widget_show(mw[o_speed_float_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_speed_float_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_speed_float_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_speed_float_callback), (gpointer)ss, 0),
				 0);

  mw[o_speed_semitone_menu] = gtk_menu_item_new_with_label("semitones");
  ml[o_speed_semitone_menu] = "semitones";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_speed_cascade_menu]), mw[o_speed_semitone_menu]);
  gtk_widget_show(mw[o_speed_semitone_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_speed_semitone_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_speed_semitone_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_speed_semitone_callback), (gpointer)ss, 0),
				 0);

  mw[o_speed_ratio_menu] = gtk_menu_item_new_with_label("ratio");
  ml[o_speed_ratio_menu] = "ratio";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_speed_cascade_menu]), mw[o_speed_ratio_menu]);
  gtk_widget_show(mw[o_speed_ratio_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_speed_ratio_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_speed_ratio_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_speed_ratio_callback), (gpointer)ss, 0),
				 0);


  mw[o_focus_style_menu] = gtk_menu_item_new_with_label("Zoom focus");
  ml[o_focus_style_menu] = "Zoom focus";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_focus_style_menu]);
  gtk_widget_show(mw[o_focus_style_menu]);

  mw[o_focus_cascade_menu] = gtk_menu_new();
  ml[o_focus_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[o_focus_style_menu]), mw[o_focus_cascade_menu]);

  mw[o_focus_left_menu] = gtk_menu_item_new_with_label("window left edge");
  ml[o_focus_left_menu] = "window left edge";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_focus_cascade_menu]), mw[o_focus_left_menu]);
  gtk_widget_show(mw[o_focus_left_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_focus_left_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_focus_left_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_focus_left_callback), (gpointer)ss, 0),
				 0);

  mw[o_focus_right_menu] = gtk_menu_item_new_with_label("window right edge");
  ml[o_focus_right_menu] = "window right edge";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_focus_cascade_menu]), mw[o_focus_right_menu]);
  gtk_widget_show(mw[o_focus_right_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_focus_right_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_focus_right_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_focus_right_callback), (gpointer)ss, 0),
				 0);

  mw[o_focus_middle_menu] = gtk_menu_item_new_with_label("window midpoint");
  ml[o_focus_middle_menu] = "window midpoint";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_focus_cascade_menu]), mw[o_focus_middle_menu]);
  gtk_widget_show(mw[o_focus_middle_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_focus_middle_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_focus_middle_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_focus_middle_callback), (gpointer)ss, 0),
				 0);

  mw[o_focus_active_menu] = gtk_menu_item_new_with_label("cursor or selection");
  ml[o_focus_active_menu] = "cursor or selection";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_focus_cascade_menu]), mw[o_focus_active_menu]);
  gtk_widget_show(mw[o_focus_active_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_focus_active_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_focus_active_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_focus_active_callback), (gpointer)ss, 0),
				 0);
  activate_focus_menu(ss, zoom_focus_style(ss));

#if HAVE_EXTENSION_LANGUAGE
  mw[o_save_menu] = gtk_menu_item_new_with_label("Save options");
  ml[o_save_menu] = "Save options";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_save_menu]);
  gtk_widget_show(mw[o_save_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_save_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_save_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_save_callback), (gpointer)ss, 0),
				 0);

  mw[o_save_state_menu] = gtk_menu_item_new_with_label("Save state");
  ml[o_save_state_menu] = "Save state";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[o_cascade_menu]), mw[o_save_state_menu]);
  gtk_widget_show(mw[o_save_state_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[o_save_state_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[o_save_state_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(options_save_state_callback), (gpointer)ss, 0),
				 0);
#endif



  /* HELP MENU */
  mw[help_menu] = gtk_menu_item_new_with_label("Help");
  ml[help_menu] = "Help";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), mw[help_menu]);
  gtk_widget_show(mw[help_menu]);
  gtk_menu_item_set_right_justified(GTK_MENU_ITEM(mw[help_menu]), TRUE);

  mw[h_cascade_menu] = gtk_menu_new();
  ml[h_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[help_menu]), mw[h_cascade_menu]);

  mw[h_about_snd_menu] = gtk_menu_item_new_with_label("Overview");
  ml[h_about_snd_menu] = "Overview";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_about_snd_menu]);
  gtk_widget_show(mw[h_about_snd_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_about_snd_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_about_snd_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_about_snd_callback), (gpointer)ss, 0),
				 0);

  mw[h_fft_menu] = gtk_menu_item_new_with_label("FFT");
  ml[h_fft_menu] = "FFT";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_fft_menu]);
  gtk_widget_show(mw[h_fft_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_fft_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_fft_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_fft_callback), (gpointer)ss, 0),
				 0);

  mw[h_find_menu] = gtk_menu_item_new_with_label("Find");
  ml[h_find_menu] = "Find";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_find_menu]);
  gtk_widget_show(mw[h_find_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_find_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_find_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_find_callback), (gpointer)ss, 0),
				 0);

  mw[h_undo_menu] = gtk_menu_item_new_with_label("Undo and redo");
  ml[h_undo_menu] = "Undo and redo";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_undo_menu]);
  gtk_widget_show(mw[h_undo_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_undo_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_undo_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_undo_callback), (gpointer)ss, 0),
				 0);

  mw[h_sync_menu] = gtk_menu_item_new_with_label("Sync");
  ml[h_sync_menu] = "Sync";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_sync_menu]);
  gtk_widget_show(mw[h_sync_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_sync_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_sync_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_sync_callback), (gpointer)ss, 0),
				 0);

  mw[h_speed_menu] = gtk_menu_item_new_with_label("Speed");
  ml[h_speed_menu] = "Speed";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_speed_menu]);
  gtk_widget_show(mw[h_speed_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_speed_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_speed_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_speed_callback), (gpointer)ss, 0),
				 0);

  mw[h_expand_menu] = gtk_menu_item_new_with_label("Expand");
  ml[h_expand_menu] = "Expand";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_expand_menu]);
  gtk_widget_show(mw[h_expand_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_expand_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_expand_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_expand_callback), (gpointer)ss, 0),
				 0);

  mw[h_reverb_menu] = gtk_menu_item_new_with_label("Reverb");
  ml[h_reverb_menu] = "Reverb";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_reverb_menu]);
  gtk_widget_show(mw[h_reverb_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_reverb_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_reverb_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_reverb_callback), (gpointer)ss, 0),
				 0);

  mw[h_contrast_menu] = gtk_menu_item_new_with_label("Contrast");
  ml[h_contrast_menu] = "Contrast";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_contrast_menu]);
  gtk_widget_show(mw[h_contrast_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_contrast_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_contrast_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_contrast_callback), (gpointer)ss, 0),
				 0);

  mw[h_env_menu] = gtk_menu_item_new_with_label("Envelope");
  ml[h_env_menu] = "Envelope";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_env_menu]);
  gtk_widget_show(mw[h_env_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_env_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_env_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_env_callback), (gpointer)ss, 0),
				 0);

  mw[h_marks_menu] = gtk_menu_item_new_with_label("Marks");
  ml[h_marks_menu] = "Marks";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_marks_menu]);
  gtk_widget_show(mw[h_marks_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_marks_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_marks_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_marks_callback), (gpointer)ss, 0),
				 0);

  mw[h_mix_menu] = gtk_menu_item_new_with_label("Mixing");
  ml[h_mix_menu] = "Mixing";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_mix_menu]);
  gtk_widget_show(mw[h_mix_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_mix_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_mix_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_mix_callback), (gpointer)ss, 0),
				 0);

  mw[h_sound_files_menu] = gtk_menu_item_new_with_label("Formats");
  ml[h_sound_files_menu] = "Formats";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_sound_files_menu]);
  gtk_widget_show(mw[h_sound_files_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_sound_files_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_sound_files_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_sound_files_callback), (gpointer)ss, 0),
				 0);

  mw[h_init_file_menu] = gtk_menu_item_new_with_label("Customization");
  ml[h_init_file_menu] = "Customization";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_init_file_menu]);
  gtk_widget_show(mw[h_init_file_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_init_file_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_init_file_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_init_file_callback), (gpointer)ss, 0),
				 0);

  mw[h_recording_menu] = gtk_menu_item_new_with_label("Recording");
  ml[h_recording_menu] = "Recording";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_recording_menu]);
  gtk_widget_show(mw[h_recording_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_recording_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_recording_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_recording_callback), (gpointer)ss, 0),
				 0);

  mw[h_clm_menu] = gtk_menu_item_new_with_label("CLM");
  ml[h_clm_menu] = "CLM";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_clm_menu]);
  gtk_widget_show(mw[h_clm_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_clm_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_clm_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_clm_callback), (gpointer)ss, 0),
				 0);

  mw[h_news_menu] = gtk_menu_item_new_with_label("News");
  ml[h_news_menu] = "News";
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[h_cascade_menu]), mw[h_news_menu]);
  gtk_widget_show(mw[h_news_menu]);
  g_signal_connect_closure_by_id(GTK_OBJECT(mw[h_news_menu]),
				 g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(mw[h_news_menu]))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(help_news_callback), (gpointer)ss, 0),
				 0);

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
    case FILE_MENU:    return("File"); break;
    case EDIT_MENU:    return("Edit"); break;
    case VIEW_MENU:    return("View"); break;
    case OPTIONS_MENU: return("Options"); break;
    case HELP_MENU:    return("Help"); break;
    case POPUP_MENU:   return("Popup"); break;
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
  callb = get_user_int_data(G_OBJECT(w));
  opt = callb2option(callb);
  if (opt != -1)
    {
      IF_MENU_HOOK(main_menu_name(opt), added_options_names[(opt < 0) ? 0 : opt])
	g_snd_callback(callb);
    }
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
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(added_options_names[i]) &&
	(strcmp(label, added_options_names[i]) == 0) && 
	(added_options[i]))
      {
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
  gtk_menu_shell_append(GTK_MENU_SHELL(mw[menu_menu]), m);
  gtk_widget_show(m);
  set_user_int_data(G_OBJECT(m), slot);
  if (slot >= 0) 
    {
      g_signal_connect_closure_by_id(GTK_OBJECT(m),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(m))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(SND_callback), (gpointer)ss, 0),
				     0);
      add_option(m, new_menu + 1, label, slot);
    }

  mc = gtk_menu_new();
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
     gtk_menu_shell_insert(GTK_MENU_SHELL(menw), m, position);
   else gtk_menu_shell_append(GTK_MENU_SHELL(menw), m);
   gtk_widget_show(m);
   if (label)
     {
       set_user_int_data(G_OBJECT(m), callb);
       g_signal_connect_closure_by_id(GTK_OBJECT(m),
				      g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(m))),
				      0,
				      g_cclosure_new(GTK_SIGNAL_FUNC(SND_callback), (gpointer)ss, 0),
				      0);
       add_option(m, which_menu, label, callb);
     }
  return(0);
}

int g_remove_from_menu(int which_menu, char *label)
{
  return(remove_option(which_menu, label));
}


/* -------------------------------- POPUP MENU -------------------------------- */

static int stopping = FALSE;

static void popup_play_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  snd_info *sp;
  IF_MENU_HOOK("Popup", "Play")
    {
      sp = any_selected_sound(ss);
      if (stopping)
	{
	  stop_playing_all_sounds();
	  stopping = FALSE;
	  set_button_label(w, "Play");
	  if (sp) set_play_button(sp, 0);
	}
      else
	{
	  if (sp)
	    {
	      play_sound(sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "popup play", 0);
	      set_play_button(sp, 1);
	      stopping = TRUE;
	      set_button_label(w, "Stop playing");
	    }
	}
    }
  gtk_widget_hide(popup_menu);
}

void reflect_play_stop_in_popup_menu(void)
{
  stopping = FALSE;
  if (popup_menu)
    set_button_label(popup_children[W_pop_play], "Play");
}

static void popup_save_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", "Save") save_edits(any_selected_sound(ss), NULL);
  gtk_widget_hide(popup_menu);
}

static void popup_undo_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", "Undo") undo_edit_with_sync(current_channel(ss), 1);
  gtk_widget_hide(popup_menu);
}

static void popup_redo_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  IF_MENU_HOOK("Popup", "Redo") redo_edit_with_sync(current_channel(ss), 1);
  gtk_widget_hide(popup_menu);
}

static void popup_info_callback(GtkWidget *w, gpointer cD) 
{
  snd_state *ss = (snd_state *)cD;
  snd_info *sp;
  IF_MENU_HOOK("Popup", "Info")
    {
      sp = selected_sound(ss);
      if (sp) display_info(sp);
    }
  gtk_widget_hide(popup_menu);
}

static void create_popup_menu(snd_state *ss, guint button, Tempus time)
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

      popup_children[W_pop_play] = gtk_menu_item_new_with_label("Play");
      pl[W_pop_play] = "Play";
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_play]);
      g_signal_connect_closure_by_id(GTK_OBJECT(popup_children[W_pop_play]),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(popup_children[W_pop_play]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(popup_play_callback), (gpointer)ss, 0),
				     0);
      set_sensitive(popup_children[W_pop_play], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_play]);

      popup_children[W_pop_undo] = gtk_menu_item_new_with_label("Undo");
      pl[W_pop_undo] = "Undo";
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_undo]);
      g_signal_connect_closure_by_id(GTK_OBJECT(popup_children[W_pop_undo]),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(popup_children[W_pop_undo]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(popup_undo_callback), (gpointer)ss, 0),
				     0);
      set_sensitive(popup_children[W_pop_undo], undo_possible);
      gtk_widget_show(popup_children[W_pop_undo]);
      
      popup_children[W_pop_redo] = gtk_menu_item_new_with_label("Redo");
      pl[W_pop_redo] = "Redo";
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_redo]);
      g_signal_connect_closure_by_id(GTK_OBJECT(popup_children[W_pop_redo]),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(popup_children[W_pop_redo]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(popup_redo_callback), (gpointer)ss, 0),
				     0);
      set_sensitive(popup_children[W_pop_redo], redo_possible);
      gtk_widget_show(popup_children[W_pop_redo]);
      
      popup_children[W_pop_save] = gtk_menu_item_new_with_label("Save");
      pl[W_pop_save] = "Save";
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_save]);
      g_signal_connect_closure_by_id(GTK_OBJECT(popup_children[W_pop_save]),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(popup_children[W_pop_save]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(popup_save_callback), (gpointer)ss, 0),
				     0);
      set_sensitive(popup_children[W_pop_save], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_save]);

      popup_children[W_pop_info] = gtk_menu_item_new_with_label("Info");
      pl[W_pop_info] = "Info";
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_children[W_pop_info]);
      g_signal_connect_closure_by_id(GTK_OBJECT(popup_children[W_pop_info]),
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(popup_children[W_pop_info]))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(popup_info_callback), (gpointer)ss, 0),
				     0);
      set_sensitive(popup_children[W_pop_info], (ss->active_sounds > 0));
      gtk_widget_show(popup_children[W_pop_info]);
    }
  gtk_widget_show(popup_menu);
  gtk_menu_popup(GTK_MENU(popup_menu), NULL, NULL, NULL, NULL, button, time);
}

static XEN gtk_popup_hook = XEN_FALSE;

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
  create_popup_menu(get_global_state(), ev->button, ev->time);
}

static XEN g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets ") returns list of top level menu widgets ((0)main (1)file (2)edit (3)view (4)options (5)help)"
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

  #define H_gtk_popup_hook "gtk-popup-hook (widget event data snd chn) is called upon middle button click. \
If it returns other than #t, the normal Snd popup menu is posted."

  XEN_DEFINE_HOOK(menu_hook, S_menu_hook, 2, H_menu_hook);
  XEN_DEFINE_HOOK(gtk_popup_hook, "gtk-popup-hook", 5, H_gtk_popup_hook);
  XEN_DEFINE_PROCEDURE(S_menu_widgets, g_menu_widgets_w, 0, 0, 0, H_menu_widgets);
}


