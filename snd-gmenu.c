#include "snd.h"

/* DIFFS: click for help is called tipquery or something like that (and isn't implemented)
 *        no normalize menus (not using paned windows here, so no use for them)
 */
/* TODO: popup doesn't work right 
 *       finish guile-gtk connection (popup and decide about inner cascades)
 */

enum {menu_menu,
        file_menu,f_cascade_menu,
          f_open_menu,f_close_menu,f_save_menu,f_save_as_menu,f_revert_menu,f_exit_menu,f_new_menu,
          f_view_menu,f_print_menu,f_mix_menu,f_update_menu,f_record_menu,f_sep_menu,
        edit_menu,e_cascade_menu,
          e_cut_menu,e_paste_menu,e_mix_menu,e_play_menu,e_save_as_menu,e_undo_menu,
          e_redo_menu,e_find_menu,e_edenv_menu,e_header_menu,
          e_select_all_menu,
          e_select_sep_menu,e_edit_sep_menu,
        help_menu,h_cascade_menu,
          h_about_snd_menu,h_fft_menu,h_find_menu,h_undo_menu,h_sync_menu,h_speed_menu,
          h_expand_menu,h_contrast_menu,h_reverb_menu,h_env_menu,h_marks_menu,h_sound_files_menu,h_init_file_menu,
          h_mix_menu,h_recording_menu,h_clm_menu,h_news_menu,
        option_menu,o_cascade_menu,
          o_transform_menu,
          o_focus_style_menu,o_focus_cascade_menu,
            o_focus_right_menu,o_focus_left_menu,o_focus_middle_menu,o_focus_active_menu,
          o_save_menu,o_save_state_menu,
          o_speed_menu,o_speed_cascade_menu,
            o_speed_float_menu,o_speed_ratio_menu,o_speed_semitone_menu,
          o_stats_menu,
        view_menu,v_cascade_menu,
#if 0
          v_normalize_menu, 
#endif
          v_graph_style_menu, v_graph_style_cascade_menu,
            v_lines_menu,v_dots_menu,v_filled_menu,v_dots_and_lines_menu,v_lollipops_menu,
          v_marks_menu, v_zero_menu, v_cursor_menu, v_ctrls_menu, v_listener_menu,
          v_region_menu,
          v_combine_menu, v_combine_cascade_menu,
            v_combine_separate_menu,v_combine_combined_menu,v_combine_superimposed_menu,
          v_color_menu, v_orientation_menu, 
          v_files_menu, v_consoles_menu,
          v_x_axis_menu,v_x_axis_cascade_menu,
            v_x_axis_seconds_menu,v_x_axis_samples_menu,v_x_axis_percentage_menu,
          v_error_history_menu,
          v_sep1_menu,v_sep2_menu
};

#define NUM_MENU_WIDGETS 99

static GtkWidget *mw[NUM_MENU_WIDGETS];

enum {W_pop_menu,W_pop_sep,W_pop_play,W_pop_undo,W_pop_redo,W_pop_save,W_pop_normalize,W_pop_info};
#define NUM_POPUP_CHILDREN 8
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

/* GtkWidget *view_normalize_menu(void) {return(mw[v_normalize_menu]);} */
GtkWidget *view_normalize_menu(void) {return(NULL);}

GtkWidget *view_consoles_menu(void) {return(mw[v_consoles_menu]);}
GtkWidget *view_region_menu(void) {return(mw[v_region_menu]);}
GtkWidget *view_combine_separate_menu(void) {return(mw[v_combine_separate_menu]);}
GtkWidget *view_combine_combined_menu(void) {return(mw[v_combine_combined_menu]);}
GtkWidget *view_combine_superimposed_menu(void) {return(mw[v_combine_superimposed_menu]);}
GtkWidget *view_lines_menu(void) {return(mw[v_lines_menu]);}
GtkWidget *view_dots_menu(void) {return(mw[v_dots_menu]);}
GtkWidget *view_dots_and_lines_menu(void) {return(mw[v_dots_and_lines_menu]);}
GtkWidget *view_filled_menu(void) {return(mw[v_filled_menu]);}
GtkWidget *view_lollipops_menu(void) {return(mw[v_lollipops_menu]);}
GtkWidget *view_marks_menu(void) {return(mw[v_marks_menu]);}
GtkWidget *view_zero_menu(void) {return(mw[v_zero_menu]);}
GtkWidget *view_ctrls_menu(void) {return(mw[v_ctrls_menu]);}
GtkWidget *view_listener_menu(void) {return(mw[v_listener_menu]);}
GtkWidget *view_cursor_menu(void) {return(mw[v_cursor_menu]);}
GtkWidget *view_x_axis_seconds_menu(void) {return(mw[v_x_axis_seconds_menu]);}
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
GtkWidget *popup_normalize_menu(void) {return(NULL);}
GtkWidget *popup_info_menu(void) {return(popup_children[W_pop_info]);}

void set_menu_label(GtkWidget *w, char *label) {set_button_label(w,label);}



/* -------------------------------- FILE MENU -------------------------------- */

static void File_Open_Callback(GtkWidget *w,gpointer clientData) {make_open_file_dialog((snd_state *)clientData);}

static void File_View_Callback(GtkWidget *w,gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  ss->viewing = 1;
  make_open_file_dialog(ss);
}

static void File_New_Callback(GtkWidget *w,gpointer clientData) {new_file_from_menu((snd_state *)clientData);}
static void File_Record_Callback(GtkWidget *w,gpointer clientData) {snd_record_file((snd_state *)clientData);}
static void File_Close_Callback(GtkWidget *w,gpointer clientData) {close_file_from_menu((snd_state *)clientData);}
static void File_Save_Callback(GtkWidget *w,gpointer clientData) {save_file_from_menu((snd_state *)clientData);}
static void File_Update_Callback(GtkWidget *w,gpointer clientData) {update_file_from_menu((snd_state *)clientData);}
static void File_Save_As_Callback(GtkWidget *w,gpointer clientData) {make_file_save_as_dialog((snd_state *)clientData);}
static void File_Revert_Callback(GtkWidget *w,gpointer clientData) {revert_file_from_menu((snd_state *)clientData);}
static void File_Exit_Callback(GtkWidget *w,gpointer clientData) {exit_from_menu((snd_state *)clientData);}


/* -------------------------------- EDIT MENU -------------------------------- */


static void Edit_Mix_Callback(GtkWidget *w,gpointer clientData) {mix_selection_from_menu((snd_state *)clientData);}
static void Edit_Envelope_Callback(GtkWidget *w,gpointer clientData) {create_envelope_editor((snd_state *)clientData);}
static void Edit_Cut_Callback(GtkWidget *w,gpointer clientData) {cut_selection_from_menu();}
static void Edit_Paste_Callback(GtkWidget *w,gpointer clientData) {paste_selection_from_menu((snd_state *)clientData);}
static void Edit_Save_As_Callback(GtkWidget *w,gpointer clientData) {make_edit_save_as_dialog((snd_state *)clientData);}
static void Edit_Select_All_Callback(GtkWidget *w,gpointer clientData) {select_all_from_menu((snd_state *)clientData);}

static void Edit_Undo_Callback(GtkWidget *w,gpointer clientData) 
{
  finish_keyboard_selection();
  undo_EDIT((void *)clientData,1);
}

static void Edit_Redo_Callback(GtkWidget *w,gpointer clientData) 
{
  finish_keyboard_selection();
  redo_EDIT((void *)clientData,1);
}

static void Edit_Header_Callback(GtkWidget *w,gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  snd_info *sp;
  sp = selected_sound(ss);
  if (sp) edit_header(sp);
}

static void Edit_Play_Callback(GtkWidget *w,gpointer clientData) 
{
  finish_keyboard_selection();
  if (region_ok(0)) play_region((snd_state *)clientData,0,NULL,FALSE);
}


/* -------------------------------- VIEW MENU -------------------------------- */

static void View_Separate_Callback(GtkWidget *w,gpointer clientData) {set_channel_style((snd_state *)clientData,CHANNELS_SEPARATE);}
static void View_Combined_Callback(GtkWidget *w,gpointer clientData) {set_channel_style((snd_state *)clientData,CHANNELS_COMBINED);}
static void View_Superimposed_Callback(GtkWidget *w,gpointer clientData) {set_channel_style((snd_state *)clientData,CHANNELS_SUPERIMPOSED);}
static void View_Dots_Callback(GtkWidget *w,gpointer clientData) {set_graph_style((snd_state *)clientData,GRAPH_DOTS);}
static void View_Lines_Callback(GtkWidget *w,gpointer clientData) {set_graph_style((snd_state *)clientData,GRAPH_LINES);}
static void View_Filled_Callback(GtkWidget *w,gpointer clientData) {set_graph_style((snd_state *)clientData,GRAPH_FILLED);}
static void View_Dots_and_Lines_Callback(GtkWidget *w,gpointer clientData) {set_graph_style((snd_state *)clientData,GRAPH_DOTS_AND_LINES);}
static void View_Lollipops_Callback(GtkWidget *w,gpointer clientData) {set_graph_style((snd_state *)clientData,GRAPH_LOLLIPOPS);}

static void View_Marks_Callback(GtkWidget *w,gpointer clientData)
{
   /* similar to dots menus */
  snd_state *ss = (snd_state *)clientData;
  set_show_marks(ss,(!(show_marks(ss))));
}

static void View_Zero_Callback(GtkWidget *w,gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  set_show_y_zero(ss,(!(show_y_zero(ss))));
}

static void View_Cursor_Callback(GtkWidget *w,gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  set_verbose_cursor(ss,(!(verbose_cursor(ss))));
}

static void View_Ctrls_Callback(GtkWidget *w,gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  if (ss->ctrls_height < 100) show_controls(ss); else hide_controls(ss); /* snd-xmain.c */
}

static void View_Listener_Callback(GtkWidget *w,gpointer clientData)
{
  handle_listener((snd_state *)clientData,LISTENER_OPEN);
}

static void View_Consoles_Callback(GtkWidget *w,gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  set_show_mix_consoles(ss,!(show_mix_consoles(ss)));
}

static void View_Error_History_Callback(GtkWidget *w,gpointer clientData) {show_snd_errors((snd_state *)clientData);}



/* -------------------------------- OPTIONS MENU -------------------------------- */

static void Options_Transform_Callback(GtkWidget *w,gpointer clientData) {fire_up_transform_dialog((snd_state *)clientData);}
static void Options_Save_Callback(GtkWidget *w,gpointer clientData) {save_options_from_menu((snd_state *)clientData);}

static void Options_Focus_Right_Callback(GtkWidget *w,gpointer clientData,gpointer Data) {activate_focus_menu((snd_state *)clientData,FOCUS_RIGHT);}
static void Options_Focus_Left_Callback(GtkWidget *w,gpointer clientData,gpointer Data) {activate_focus_menu((snd_state *)clientData,FOCUS_LEFT);}
static void Options_Focus_Middle_Callback(GtkWidget *w,gpointer clientData,gpointer Data) {activate_focus_menu((snd_state *)clientData,FOCUS_MIDDLE);}
static void Options_Focus_Active_Callback(GtkWidget *w,gpointer clientData,gpointer Data) {activate_focus_menu((snd_state *)clientData,FOCUS_ACTIVE);}

static void Options_Speed_Float_Callback(GtkWidget *w,gpointer cD,gpointer Data) {activate_speed_in_menu((snd_state *)cD,SPEED_AS_FLOAT);}
static void Options_Speed_Ratio_Callback(GtkWidget *w,gpointer cD,gpointer Data) {activate_speed_in_menu((snd_state *)cD,SPEED_AS_RATIO);}
static void Options_Speed_Semitone_Callback(GtkWidget *w,gpointer cD,gpointer Data) {activate_speed_in_menu((snd_state *)cD,SPEED_AS_SEMITONE);}

static void Options_X_Axis_Seconds_Callback(GtkWidget *w,gpointer clientData) {set_x_axis_style((snd_state *)clientData,X_IN_SECONDS);}
static void Options_X_Axis_Samples_Callback(GtkWidget *w,gpointer clientData) {set_x_axis_style((snd_state *)clientData,X_IN_SAMPLES);}
static void Options_X_Axis_Percentage_Callback(GtkWidget *w,gpointer clientData) {set_x_axis_style((snd_state *)clientData,X_TO_ONE);}

static void Options_Save_State_Callback(GtkWidget *w,gpointer clientData) {save_state_from_menu((snd_state *)clientData);}

static void Options_Stats_Callback(GtkWidget *w,gpointer clientData)
{
  snd_state *ss = (snd_state *)clientData;
  set_show_usage_stats(ss,(!(show_usage_stats(ss))));
}


/* -------------------------------- HELP MENU -------------------------------- */

static void Help_About_Snd_Callback(GtkWidget *w,gpointer clientData) {about_snd_help((snd_state *)clientData);}
static void Help_FFT_Callback (GtkWidget *w,gpointer clientData) {fft_help((snd_state *)clientData);}
static void Help_Find_Callback (GtkWidget *w,gpointer clientData) {find_help((snd_state *)clientData);}
static void Help_Undo_Callback (GtkWidget *w,gpointer clientData) {undo_help((snd_state *)clientData);}
static void Help_Sync_Callback (GtkWidget *w,gpointer clientData) {sync_help((snd_state *)clientData);}
static void Help_Speed_Callback (GtkWidget *w,gpointer clientData) {speed_help((snd_state *)clientData);}
static void Help_Expand_Callback (GtkWidget *w,gpointer clientData) {expand_help((snd_state *)clientData);}
static void Help_Reverb_Callback (GtkWidget *w,gpointer clientData) {reverb_help((snd_state *)clientData);}
static void Help_Contrast_Callback (GtkWidget *w,gpointer clientData) {contrast_help((snd_state *)clientData);}
static void Help_Env_Callback (GtkWidget *w,gpointer clientData) {env_help((snd_state *)clientData);}
static void Help_Marks_Callback (GtkWidget *w,gpointer clientData) {marks_help((snd_state *)clientData);}
static void Help_Mix_Callback (GtkWidget *w,gpointer clientData) {mix_help((snd_state *)clientData);}
static void Help_Sound_Files_Callback (GtkWidget *w,gpointer clientData) {sound_files_help((snd_state *)clientData);}
static void Help_Init_File_Callback (GtkWidget *w,gpointer clientData) {init_file_help((snd_state *)clientData);}
static void Help_Recording_Callback (GtkWidget *w,gpointer clientData) {recording_help((snd_state *)clientData);}

#if HAVE_GUILE
static void Help_CLM_Callback (GtkWidget *w,gpointer clientData) {clm_help((snd_state *)clientData);}
#endif

static void Help_News_Callback (GtkWidget *w,gpointer clientData) {news_help((snd_state *)clientData);}


/* -------------------------------- MAIN MENU -------------------------------- */

GtkWidget *get_menubar(void) {return(mw[menu_menu]);}

GtkWidget *add_menu(snd_state *ss)
{
  /* this mainly passes the global data pointer (ss) to all the menu-related callbacks */

  /* TODO: SND_AS_WIDGET here?
   *       figure out why accelerator doesn't get displayed
   */

  GtkAccelGroup *accel_group;
  accel_group = gtk_accel_group_new();

#ifndef SND_AS_WIDGET
  gtk_window_add_accel_group(GTK_WINDOW(MAIN_SHELL(ss)),accel_group);
#endif

  mw[menu_menu] = gtk_menu_bar_new();
  set_background(mw[menu_menu],(ss->sgx)->highlight_color);
  gtk_box_pack_start(GTK_BOX(MAIN_PANE(ss)),mw[menu_menu],FALSE,TRUE,0);
  gtk_widget_show(mw[menu_menu]);

  /* gtk_signal_connect(GTK_OBJECT(mw[menu_menu]),"button_press_event",GTK_SIGNAL_FUNC(middle_button_press),(gpointer)ss); */

  /* FILE MENU */
  mw[file_menu] = gtk_menu_item_new_with_label(STR_File);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]),mw[file_menu]);
  set_background(mw[file_menu],(ss->sgx)->highlight_color);
  gtk_widget_show(mw[file_menu]);
  gtk_widget_add_accelerator(mw[file_menu],"activate",accel_group,GDK_F,GDK_MOD1_MASK,GTK_ACCEL_VISIBLE);
  /* why doesn't this show up?? */

  mw[f_cascade_menu] = gtk_menu_new();
  set_background(mw[f_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[file_menu]),mw[f_cascade_menu]);

  mw[f_open_menu] = gtk_menu_item_new_with_label(STR_Open);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_open_menu]);
  set_background(mw[f_open_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_open_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_open_menu]),"activate",GTK_SIGNAL_FUNC(File_Open_Callback),(gpointer)ss);

  mw[f_close_menu] = gtk_menu_item_new_with_label(STR_Close);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_close_menu]);
  set_background(mw[f_close_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_close_menu]);
  set_sensitive(mw[f_close_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_close_menu]),"activate",GTK_SIGNAL_FUNC(File_Close_Callback),(gpointer)ss);
  
  mw[f_save_menu] = gtk_menu_item_new_with_label(STR_Save);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_save_menu]);
  set_background(mw[f_save_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_save_menu]);
  set_sensitive(mw[f_save_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_save_menu]),"activate",GTK_SIGNAL_FUNC(File_Save_Callback),(gpointer)ss);
  
  mw[f_save_as_menu] = gtk_menu_item_new_with_label(STR_Save_as);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_save_as_menu]);
  set_background(mw[f_save_as_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_save_as_menu]);
  set_sensitive(mw[f_save_as_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_save_as_menu]),"activate",GTK_SIGNAL_FUNC(File_Save_As_Callback),(gpointer)ss);
  
  mw[f_revert_menu] = gtk_menu_item_new_with_label(STR_Revert);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_revert_menu]);
  set_background(mw[f_revert_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_revert_menu]);
  set_sensitive(mw[f_revert_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_revert_menu]),"activate",GTK_SIGNAL_FUNC(File_Revert_Callback),(gpointer)ss);
  
  mw[f_mix_menu] = gtk_menu_item_new_with_label(STR_Mix);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_mix_menu]);
  set_background(mw[f_mix_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_mix_menu]);
  set_sensitive(mw[f_mix_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_mix_menu]),"activate",GTK_SIGNAL_FUNC(File_Mix_Callback),(gpointer)ss);

  mw[f_update_menu] = gtk_menu_item_new_with_label(STR_Update);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_update_menu]);
  set_background(mw[f_update_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_update_menu]);
  set_sensitive(mw[f_update_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_update_menu]),"activate",GTK_SIGNAL_FUNC(File_Update_Callback),(gpointer)ss);

  mw[f_new_menu] = gtk_menu_item_new_with_label(STR_New);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_new_menu]);
  set_background(mw[f_new_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_new_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_new_menu]),"activate",GTK_SIGNAL_FUNC(File_New_Callback),(gpointer)ss);

  mw[f_record_menu] = gtk_menu_item_new_with_label(STR_Record);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_record_menu]);
  set_background(mw[f_record_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_record_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_record_menu]),"activate",GTK_SIGNAL_FUNC(File_Record_Callback),(gpointer)ss);

  mw[f_view_menu] = gtk_menu_item_new_with_label(STR_View);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_view_menu]);
  set_background(mw[f_view_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_view_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_view_menu]),"activate",GTK_SIGNAL_FUNC(File_View_Callback),(gpointer)ss);

  mw[f_print_menu] = gtk_menu_item_new_with_label(STR_Print);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_print_menu]);
  set_background(mw[f_print_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_print_menu]);
  set_sensitive(mw[f_print_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[f_print_menu]),"activate",GTK_SIGNAL_FUNC(File_Print_Callback),(gpointer)ss);

  mw[f_sep_menu] = gtk_menu_item_new();
  set_background(mw[f_sep_menu],(ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_sep_menu]);
  gtk_widget_show(mw[f_sep_menu]);

  mw[f_exit_menu] = gtk_menu_item_new_with_label(STR_Exit);
  gtk_menu_append(GTK_MENU(mw[f_cascade_menu]),mw[f_exit_menu]);
  set_background(mw[f_exit_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[f_exit_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[f_exit_menu]),"activate",GTK_SIGNAL_FUNC(File_Exit_Callback),(gpointer)ss);


  /* EDIT MENU */
  mw[edit_menu] = gtk_menu_item_new_with_label(STR_Edit);
  set_background(mw[edit_menu],(ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]),mw[edit_menu]);
  gtk_widget_show(mw[edit_menu]);

  mw[e_cascade_menu] = gtk_menu_new();
  set_background(mw[e_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[edit_menu]),mw[e_cascade_menu]);
  
  mw[e_undo_menu] = gtk_menu_item_new_with_label(STR_Undo);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_undo_menu]);
  set_background(mw[e_undo_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_undo_menu]);
  set_sensitive(mw[e_undo_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_undo_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Undo_Callback),(gpointer)ss);

  mw[e_redo_menu] = gtk_menu_item_new_with_label(STR_Redo);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_redo_menu]);
  set_background(mw[e_redo_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_redo_menu]);
  set_sensitive(mw[e_redo_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_redo_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Redo_Callback),(gpointer)ss);

  mw[e_find_menu] = gtk_menu_item_new_with_label(STR_Find);
  set_background(mw[e_find_menu],(ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_find_menu]);
  gtk_widget_show(mw[e_find_menu]);
  set_sensitive(mw[e_find_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_find_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Find_Callback),(gpointer)ss);

  mw[e_select_sep_menu] = gtk_menu_item_new();
  set_background(mw[e_select_sep_menu],(ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_select_sep_menu]);
  gtk_widget_show(mw[e_select_sep_menu]);

  mw[e_cut_menu] = gtk_menu_item_new_with_label(STR_Delete_Selection);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_cut_menu]);
  set_background(mw[e_cut_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_cut_menu]);
  set_sensitive(mw[e_cut_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_cut_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Cut_Callback),(gpointer)ss);

  mw[e_paste_menu] = gtk_menu_item_new_with_label(STR_Insert_Selection);
  set_background(mw[e_paste_menu],(ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_paste_menu]);
  gtk_widget_show(mw[e_paste_menu]);
  set_sensitive(mw[e_paste_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_paste_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Paste_Callback),(gpointer)ss);

  mw[e_mix_menu] = gtk_menu_item_new_with_label(STR_Mix_Selection);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_mix_menu]);
  set_background(mw[e_mix_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_mix_menu]);
  set_sensitive(mw[e_mix_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_mix_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Mix_Callback),(gpointer)ss);

  mw[e_play_menu] = gtk_menu_item_new_with_label(STR_Play_selection);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_play_menu]);
  set_background(mw[e_play_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_play_menu]);
  set_sensitive(mw[e_play_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_play_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Play_Callback),(gpointer)ss);

  mw[e_save_as_menu] = gtk_menu_item_new_with_label(STR_Save_selection);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_save_as_menu]);
  set_background(mw[e_save_as_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_save_as_menu]);
  set_sensitive(mw[e_save_as_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_save_as_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Save_As_Callback),(gpointer)ss);

  mw[e_select_all_menu] = gtk_menu_item_new_with_label(STR_Select_all);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_select_all_menu]);
  set_background(mw[e_select_all_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_select_all_menu]);
  set_sensitive(mw[e_select_all_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_select_all_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Select_All_Callback),(gpointer)ss);

  mw[e_edit_sep_menu] = gtk_menu_item_new();
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_edit_sep_menu]);
  set_background(mw[e_edit_sep_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_edit_sep_menu]);

  mw[e_edenv_menu] = gtk_menu_item_new_with_label(STR_Edit_Envelope);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_edenv_menu]);
  set_background(mw[e_edenv_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_edenv_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[e_edenv_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Envelope_Callback),(gpointer)ss);

  mw[e_header_menu] = gtk_menu_item_new_with_label(STR_Edit_Header);
  gtk_menu_append(GTK_MENU(mw[e_cascade_menu]),mw[e_header_menu]);
  set_background(mw[e_header_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[e_header_menu]);
  set_sensitive(mw[e_header_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[e_header_menu]),"activate",GTK_SIGNAL_FUNC(Edit_Header_Callback),(gpointer)ss);


  /* VIEW MENU */
  mw[view_menu] = gtk_menu_item_new_with_label(STR_View);
  set_background(mw[view_menu],(ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]),mw[view_menu]);
  gtk_widget_show(mw[view_menu]);

  mw[v_cascade_menu] = gtk_menu_new();
  set_background(mw[v_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[view_menu]),mw[v_cascade_menu]);

  mw[v_ctrls_menu] = gtk_menu_item_new_with_label(STR_Show_controls);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_ctrls_menu]);
  set_background(mw[v_ctrls_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_ctrls_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_ctrls_menu]),"activate",GTK_SIGNAL_FUNC(View_Ctrls_Callback),(gpointer)ss);

#if 0
  mw[v_normalize_menu] = gtk_menu_item_new_with_label(STR_Normalize);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_normalize_menu]);
  set_background(mw[v_normalize_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_normalize_menu]);
  set_sensitive(mw[v_normalize_menu],FALSE);
  gtk_signal_connect(GTK_OBJECT(mw[v_normalize_menu]),"activate",GTK_SIGNAL_FUNC(View_Normalize_Callback),(gpointer)ss);
#endif

  mw[v_listener_menu] = gtk_menu_item_new_with_label(STR_Open_listener);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_listener_menu]);
  set_background(mw[v_listener_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_listener_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_listener_menu]),"activate",GTK_SIGNAL_FUNC(View_Listener_Callback),(gpointer)ss);

  mw[v_combine_menu] = gtk_menu_item_new_with_label(STR_Channel_style);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_combine_menu]);
  set_background(mw[v_combine_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_combine_menu]);

  mw[v_combine_cascade_menu] = gtk_menu_new();
  set_background(mw[v_combine_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_combine_menu]),mw[v_combine_cascade_menu]);

  mw[v_combine_separate_menu] = gtk_menu_item_new_with_label(STR_separate);
  gtk_menu_append(GTK_MENU(mw[v_combine_cascade_menu]),mw[v_combine_separate_menu]);
  set_background(mw[v_combine_separate_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_combine_separate_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_combine_separate_menu]),"activate",GTK_SIGNAL_FUNC(View_Separate_Callback),(gpointer)ss);
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(mw[v_combine_separate_menu],FALSE);

  mw[v_combine_combined_menu] = gtk_menu_item_new_with_label(STR_combined);
  gtk_menu_append(GTK_MENU(mw[v_combine_cascade_menu]),mw[v_combine_combined_menu]);
  set_background(mw[v_combine_combined_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_combine_combined_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_combine_combined_menu]),"activate",GTK_SIGNAL_FUNC(View_Combined_Callback),(gpointer)ss);
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(mw[v_combine_combined_menu],FALSE);

  mw[v_combine_superimposed_menu] = gtk_menu_item_new_with_label(STR_superimposed);
  gtk_menu_append(GTK_MENU(mw[v_combine_cascade_menu]),mw[v_combine_superimposed_menu]);
  set_background(mw[v_combine_superimposed_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_combine_superimposed_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_combine_superimposed_menu]),"activate",GTK_SIGNAL_FUNC(View_Superimposed_Callback),(gpointer)ss);
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(mw[v_combine_superimposed_menu],FALSE);


  mw[v_graph_style_menu] = gtk_menu_item_new_with_label(STR_Graph_style);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_graph_style_menu]);
  set_background(mw[v_graph_style_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_graph_style_menu]);

  mw[v_graph_style_cascade_menu] = gtk_menu_new();
  set_background(mw[v_graph_style_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_graph_style_menu]),mw[v_graph_style_cascade_menu]);

  mw[v_lines_menu] = gtk_menu_item_new_with_label(STR_lines);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]),mw[v_lines_menu]);
  set_background(mw[v_lines_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_lines_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_lines_menu]),"activate",GTK_SIGNAL_FUNC(View_Lines_Callback),(gpointer)ss);
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(mw[v_lines_menu],FALSE);

  mw[v_dots_menu] = gtk_menu_item_new_with_label(STR_dots);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]),mw[v_dots_menu]);
  set_background(mw[v_dots_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_dots_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_dots_menu]),"activate",GTK_SIGNAL_FUNC(View_Dots_Callback),(gpointer)ss);
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(mw[v_dots_menu],FALSE);

  mw[v_filled_menu] = gtk_menu_item_new_with_label(STR_filled);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]),mw[v_filled_menu]);
  set_background(mw[v_filled_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_filled_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_filled_menu]),"activate",GTK_SIGNAL_FUNC(View_Filled_Callback),(gpointer)ss);
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(mw[v_filled_menu],FALSE);

  mw[v_dots_and_lines_menu] = gtk_menu_item_new_with_label(STR_dots_and_lines);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]),mw[v_dots_and_lines_menu]);
  set_background(mw[v_dots_and_lines_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_dots_and_lines_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_dots_and_lines_menu]),"activate",GTK_SIGNAL_FUNC(View_Dots_and_Lines_Callback),(gpointer)ss);
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(mw[v_dots_and_lines_menu],FALSE);

  mw[v_lollipops_menu] = gtk_menu_item_new_with_label(STR_lollipops);
  gtk_menu_append(GTK_MENU(mw[v_graph_style_cascade_menu]),mw[v_lollipops_menu]);
  set_background(mw[v_lollipops_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_lollipops_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_lollipops_menu]),"activate",GTK_SIGNAL_FUNC(View_Lollipops_Callback),(gpointer)ss);
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(mw[v_lollipops_menu],FALSE);

  mw[v_cursor_menu] = gtk_menu_item_new_with_label(STR_Verbose_cursor);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_cursor_menu]);
  set_background(mw[v_cursor_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_cursor_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_cursor_menu]),"activate",GTK_SIGNAL_FUNC(View_Cursor_Callback),(gpointer)ss);

  mw[v_sep1_menu] = gtk_menu_item_new();
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_sep1_menu]);
  set_background(mw[v_sep1_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_sep1_menu]);

  mw[v_region_menu] = gtk_menu_item_new_with_label(STR_Regions);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_region_menu]);
  set_background(mw[v_region_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_region_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_region_menu]),"activate",GTK_SIGNAL_FUNC(View_Region_Callback),(gpointer)ss);
  set_sensitive(mw[v_region_menu],FALSE);

  mw[v_files_menu] = gtk_menu_item_new_with_label(STR_Files);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_files_menu]);
  set_background(mw[v_files_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_files_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_files_menu]),"activate",GTK_SIGNAL_FUNC(View_Files_Callback),(gpointer)ss);

  mw[v_color_menu] = gtk_menu_item_new_with_label(STR_Color);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_color_menu]);
  set_background(mw[v_color_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_color_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_color_menu]),"activate",GTK_SIGNAL_FUNC(View_Color_Callback),(gpointer)ss);

  mw[v_orientation_menu] = gtk_menu_item_new_with_label(STR_Orientation);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_orientation_menu]);
  set_background(mw[v_orientation_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_orientation_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_orientation_menu]),"activate",GTK_SIGNAL_FUNC(View_Orientation_Callback),(gpointer)ss);

  mw[v_sep2_menu] = gtk_menu_item_new();
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_sep2_menu]);
  set_background(mw[v_sep2_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_sep2_menu]);

  mw[v_marks_menu] = gtk_menu_item_new_with_label((show_marks(ss)) ? STR_Hide_marks : STR_Show_marks);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_marks_menu]);
  set_background(mw[v_marks_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_marks_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_marks_menu]),"activate",GTK_SIGNAL_FUNC(View_Marks_Callback),(gpointer)ss);

  mw[v_zero_menu] = gtk_menu_item_new_with_label(STR_Show_Y0);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_zero_menu]);
  set_background(mw[v_zero_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_zero_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_zero_menu]),"activate",GTK_SIGNAL_FUNC(View_Zero_Callback),(gpointer)ss);

  mw[v_consoles_menu] = gtk_menu_item_new_with_label(STR_Hide_consoles);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_consoles_menu]);
  set_background(mw[v_consoles_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_consoles_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_consoles_menu]),"activate",GTK_SIGNAL_FUNC(View_Consoles_Callback),(gpointer)ss);
  set_sensitive(mw[v_consoles_menu],FALSE);

  mw[v_x_axis_menu] = gtk_menu_item_new_with_label(STR_X_axis_units);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_x_axis_menu]);
  set_background(mw[v_x_axis_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_menu]);

  mw[v_x_axis_cascade_menu] = gtk_menu_new();
  set_background(mw[v_x_axis_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[v_x_axis_menu]),mw[v_x_axis_cascade_menu]);

  mw[v_x_axis_seconds_menu] = gtk_menu_item_new_with_label(STR_seconds);
  gtk_menu_append(GTK_MENU(mw[v_x_axis_cascade_menu]),mw[v_x_axis_seconds_menu]);
  set_background(mw[v_x_axis_seconds_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_seconds_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_x_axis_seconds_menu]),"activate",GTK_SIGNAL_FUNC(Options_X_Axis_Seconds_Callback),(gpointer)ss);
  set_sensitive(mw[v_x_axis_seconds_menu],FALSE);

  mw[v_x_axis_samples_menu] = gtk_menu_item_new_with_label(STR_samples);
  gtk_menu_append(GTK_MENU(mw[v_x_axis_cascade_menu]),mw[v_x_axis_samples_menu]);
  set_background(mw[v_x_axis_samples_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_samples_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_x_axis_samples_menu]),"activate",GTK_SIGNAL_FUNC(Options_X_Axis_Samples_Callback),(gpointer)ss);

  mw[v_x_axis_percentage_menu] = gtk_menu_item_new_with_label(STR_percentage);
  gtk_menu_append(GTK_MENU(mw[v_x_axis_cascade_menu]),mw[v_x_axis_percentage_menu]);
  set_background(mw[v_x_axis_percentage_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_x_axis_percentage_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_x_axis_percentage_menu]),"activate",GTK_SIGNAL_FUNC(Options_X_Axis_Percentage_Callback),(gpointer)ss);

  mw[v_error_history_menu] = gtk_menu_item_new_with_label(STR_Error_History);
  gtk_menu_append(GTK_MENU(mw[v_cascade_menu]),mw[v_error_history_menu]);
  set_background(mw[v_error_history_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[v_error_history_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[v_error_history_menu]),"activate",GTK_SIGNAL_FUNC(View_Error_History_Callback),(gpointer)ss);



  /* OPTIONS MENU */
  mw[option_menu] = gtk_menu_item_new_with_label(STR_Options);
  set_background(mw[option_menu],(ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]),mw[option_menu]);
  gtk_widget_show(mw[option_menu]);

  mw[o_cascade_menu] = gtk_menu_new();
  set_background(mw[o_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[option_menu]),mw[o_cascade_menu]);

  mw[o_transform_menu] = gtk_menu_item_new_with_label(STR_Transform_Options);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]),mw[o_transform_menu]);
  set_background(mw[o_transform_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_transform_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_transform_menu]),"activate",GTK_SIGNAL_FUNC(Options_Transform_Callback),(gpointer)ss);


  mw[o_speed_menu] = gtk_menu_item_new_with_label(STR_Speed_style);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]),mw[o_speed_menu]);
  set_background(mw[o_speed_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_speed_menu]);

  mw[o_speed_cascade_menu] = gtk_menu_new();
  set_background(mw[o_speed_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[o_speed_menu]),mw[o_speed_cascade_menu]);

  mw[o_speed_float_menu] = gtk_menu_item_new_with_label(STR_float);
  gtk_menu_append(GTK_MENU(mw[o_speed_cascade_menu]),mw[o_speed_float_menu]);
  set_background(mw[o_speed_float_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_speed_float_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_speed_float_menu]),"activate",GTK_SIGNAL_FUNC(Options_Speed_Float_Callback),(gpointer)ss);

  mw[o_speed_semitone_menu] = gtk_menu_item_new_with_label(STR_semitone);
  gtk_menu_append(GTK_MENU(mw[o_speed_cascade_menu]),mw[o_speed_semitone_menu]);
  set_background(mw[o_speed_semitone_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_speed_semitone_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_speed_semitone_menu]),"activate",GTK_SIGNAL_FUNC(Options_Speed_Semitone_Callback),(gpointer)ss);

  mw[o_speed_ratio_menu] = gtk_menu_item_new_with_label(STR_ratio);
  gtk_menu_append(GTK_MENU(mw[o_speed_cascade_menu]),mw[o_speed_ratio_menu]);
  set_background(mw[o_speed_ratio_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_speed_ratio_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_speed_ratio_menu]),"activate",GTK_SIGNAL_FUNC(Options_Speed_Ratio_Callback),(gpointer)ss);


  mw[o_focus_style_menu] = gtk_menu_item_new_with_label(STR_Focus_style);
  set_background(mw[o_focus_style_menu],(ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]),mw[o_focus_style_menu]);
  gtk_widget_show(mw[o_focus_style_menu]);

  mw[o_focus_cascade_menu] = gtk_menu_new();
  set_background(mw[o_focus_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[o_focus_style_menu]),mw[o_focus_cascade_menu]);

  mw[o_focus_left_menu] = gtk_menu_item_new_with_label(STR_focus_left);
  gtk_menu_append(GTK_MENU(mw[o_focus_cascade_menu]),mw[o_focus_left_menu]);
  set_background(mw[o_focus_left_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_focus_left_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_focus_left_menu]),"activate",GTK_SIGNAL_FUNC(Options_Focus_Left_Callback),(gpointer)ss);

  mw[o_focus_right_menu] = gtk_menu_item_new_with_label(STR_focus_right);
  gtk_menu_append(GTK_MENU(mw[o_focus_cascade_menu]),mw[o_focus_right_menu]);
  set_background(mw[o_focus_right_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_focus_right_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_focus_right_menu]),"activate",GTK_SIGNAL_FUNC(Options_Focus_Right_Callback),(gpointer)ss);

  mw[o_focus_middle_menu] = gtk_menu_item_new_with_label(STR_focus_middle);
  gtk_menu_append(GTK_MENU(mw[o_focus_cascade_menu]),mw[o_focus_middle_menu]);
  set_background(mw[o_focus_middle_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_focus_middle_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_focus_middle_menu]),"activate",GTK_SIGNAL_FUNC(Options_Focus_Middle_Callback),(gpointer)ss);

  mw[o_focus_active_menu] = gtk_menu_item_new_with_label(STR_focus_active);
  gtk_menu_append(GTK_MENU(mw[o_focus_cascade_menu]),mw[o_focus_active_menu]);
  set_background(mw[o_focus_active_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_focus_active_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_focus_active_menu]),"activate",GTK_SIGNAL_FUNC(Options_Focus_Active_Callback),(gpointer)ss);
  activate_focus_menu(ss,zoom_focus_style(ss));


  mw[o_save_menu] = gtk_menu_item_new_with_label(STR_Save_options);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]),mw[o_save_menu]);
  set_background(mw[o_save_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_save_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_save_menu]),"activate",GTK_SIGNAL_FUNC(Options_Save_Callback),(gpointer)ss);

  mw[o_save_state_menu] = gtk_menu_item_new_with_label(STR_Save_state);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]),mw[o_save_state_menu]);
  set_background(mw[o_save_state_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_save_state_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_save_state_menu]),"activate",GTK_SIGNAL_FUNC(Options_Save_State_Callback),(gpointer)ss);

  mw[o_stats_menu] = gtk_menu_item_new_with_label(STR_Show_stats);
  gtk_menu_append(GTK_MENU(mw[o_cascade_menu]),mw[o_stats_menu]);
  set_background(mw[o_stats_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[o_stats_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[o_stats_menu]),"activate",GTK_SIGNAL_FUNC(Options_Stats_Callback),(gpointer)ss);



  /* HELP MENU */
  mw[help_menu] = gtk_menu_item_new_with_label(STR_Help);
  set_background(mw[help_menu],(ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]),mw[help_menu]);
  gtk_widget_show(mw[help_menu]);
  gtk_menu_item_right_justify(GTK_MENU_ITEM(mw[help_menu]));

  mw[h_cascade_menu] = gtk_menu_new();
  set_background(mw[h_cascade_menu],(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(mw[help_menu]),mw[h_cascade_menu]);

  mw[h_about_snd_menu] = gtk_menu_item_new_with_label(STR_Overview);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_about_snd_menu]);
  set_background(mw[h_about_snd_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_about_snd_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_about_snd_menu]),"activate",GTK_SIGNAL_FUNC(Help_About_Snd_Callback),(gpointer)ss);

  mw[h_fft_menu] = gtk_menu_item_new_with_label(STR_FFT);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_fft_menu]);
  set_background(mw[h_fft_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_fft_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_fft_menu]),"activate",GTK_SIGNAL_FUNC(Help_FFT_Callback),(gpointer)ss);

  mw[h_find_menu] = gtk_menu_item_new_with_label(STR_Find);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_find_menu]);
  set_background(mw[h_find_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_find_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_find_menu]),"activate",GTK_SIGNAL_FUNC(Help_Find_Callback),(gpointer)ss);

  mw[h_undo_menu] = gtk_menu_item_new_with_label(STR_Undo_and_redo);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_undo_menu]);
  set_background(mw[h_undo_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_undo_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_undo_menu]),"activate",GTK_SIGNAL_FUNC(Help_Undo_Callback),(gpointer)ss);

  mw[h_sync_menu] = gtk_menu_item_new_with_label(STR_Sync);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_sync_menu]);
  set_background(mw[h_sync_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_sync_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_sync_menu]),"activate",GTK_SIGNAL_FUNC(Help_Sync_Callback),(gpointer)ss);

  mw[h_speed_menu] = gtk_menu_item_new_with_label(STR_Speed);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_speed_menu]);
  set_background(mw[h_speed_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_speed_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_speed_menu]),"activate",GTK_SIGNAL_FUNC(Help_Speed_Callback),(gpointer)ss);

  mw[h_expand_menu] = gtk_menu_item_new_with_label(STR_Expand);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_expand_menu]);
  set_background(mw[h_expand_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_expand_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_expand_menu]),"activate",GTK_SIGNAL_FUNC(Help_Expand_Callback),(gpointer)ss);

  mw[h_reverb_menu] = gtk_menu_item_new_with_label(STR_Reverb);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_reverb_menu]);
  set_background(mw[h_reverb_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_reverb_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_reverb_menu]),"activate",GTK_SIGNAL_FUNC(Help_Reverb_Callback),(gpointer)ss);

  mw[h_contrast_menu] = gtk_menu_item_new_with_label(STR_Contrast);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_contrast_menu]);
  set_background(mw[h_contrast_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_contrast_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_contrast_menu]),"activate",GTK_SIGNAL_FUNC(Help_Contrast_Callback),(gpointer)ss);

  mw[h_env_menu] = gtk_menu_item_new_with_label(STR_Envelope);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_env_menu]);
  set_background(mw[h_env_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_env_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_env_menu]),"activate",GTK_SIGNAL_FUNC(Help_Env_Callback),(gpointer)ss);

  mw[h_marks_menu] = gtk_menu_item_new_with_label(STR_Marks);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_marks_menu]);
  set_background(mw[h_marks_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_marks_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_marks_menu]),"activate",GTK_SIGNAL_FUNC(Help_Marks_Callback),(gpointer)ss);

  mw[h_mix_menu] = gtk_menu_item_new_with_label(STR_Mixing);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_mix_menu]);
  set_background(mw[h_mix_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_mix_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_mix_menu]),"activate",GTK_SIGNAL_FUNC(Help_Mix_Callback),(gpointer)ss);

  mw[h_sound_files_menu] = gtk_menu_item_new_with_label(STR_Formats);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_sound_files_menu]);
  set_background(mw[h_sound_files_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_sound_files_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_sound_files_menu]),"activate",GTK_SIGNAL_FUNC(Help_Sound_Files_Callback),(gpointer)ss);

  mw[h_init_file_menu] = gtk_menu_item_new_with_label(STR_Customization);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_init_file_menu]);
  set_background(mw[h_init_file_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_init_file_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_init_file_menu]),"activate",GTK_SIGNAL_FUNC(Help_Init_File_Callback),(gpointer)ss);

  mw[h_recording_menu] = gtk_menu_item_new_with_label(STR_Recording);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_recording_menu]);
  set_background(mw[h_recording_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_recording_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_recording_menu]),"activate",GTK_SIGNAL_FUNC(Help_Recording_Callback),(gpointer)ss);

#if HAVE_GUILE
  mw[h_clm_menu] = gtk_menu_item_new_with_label(STR_CLM);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_clm_menu]);
  set_background(mw[h_clm_menu],(ss->sgx)->basic_color);
  gtk_widget_show(mw[h_clm_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_clm_menu]),"activate",GTK_SIGNAL_FUNC(Help_CLM_Callback),(gpointer)ss);
#endif

  mw[h_news_menu] = gtk_menu_item_new_with_label(STR_News);
  set_background(mw[h_news_menu],(ss->sgx)->basic_color);
  gtk_menu_append(GTK_MENU(mw[h_cascade_menu]),mw[h_news_menu]);
  gtk_widget_show(mw[h_news_menu]);
  gtk_signal_connect(GTK_OBJECT(mw[h_news_menu]),"activate",GTK_SIGNAL_FUNC(Help_News_Callback),(gpointer)ss);

  return(mw[menu_menu]);
}

static void GH_Callback(GtkWidget *w, gpointer clientData) 
{
  g_snd_callback((int)gtk_object_get_user_data(GTK_OBJECT(w)));
}

#define MAX_MAIN_MENUS 12
static GtkWidget *added_menus[MAX_MAIN_MENUS];
static int new_menu = 4;
static GtkWidget **added_options = NULL;
static char **added_options_names = NULL;
static int *added_options_menus = NULL;
static int added_options_size = 0;
static int added_options_pos = 0;

static void add_option(GtkWidget *w,int which_menu,char *label)
{
  int i;
  if (added_options_pos == added_options_size)
    {
      added_options_size += 8;
      if (added_options_pos == 0)
	{
	  added_options = (GtkWidget **)CALLOC(added_options_size,sizeof(GtkWidget *));
	  added_options_names = (char **)CALLOC(added_options_size,sizeof(char *));
	  added_options_menus = (int *)CALLOC(added_options_size,sizeof(int));
	}
      else
	{
	  added_options = (GtkWidget **)REALLOC(added_options,added_options_size * sizeof(GtkWidget *));
	  added_options_names = (char **)REALLOC(added_options_names,added_options_size * sizeof(char *));
	  added_options_menus = (int *)REALLOC(added_options_menus,added_options_size * sizeof(int));
	  for (i=added_options_pos;i<added_options_size;i++) added_options[i] = NULL;
	}
    }
  added_options[added_options_pos] = w;
  added_options_menus[added_options_pos] = which_menu;
  added_options_names[added_options_pos] = copy_string(label);
  added_options_pos++;
}

static int remove_option(int which_menu,char *label)
{
  int i;
  for (i=0;i<added_options_pos;i++)
    {
      if ((added_options_menus[i] == which_menu) && (strcmp(label,added_options_names[i]) == 0) && (added_options[i]))
	{
	  gtk_widget_destroy(added_options[i]);
	  added_options[i] = NULL;
	  added_options_menus[i] = -1;
	  FREE(added_options_names[i]);
	  added_options_names[i] = NULL;
	  return(0);
	}
    }
  return(-1);
}

int gh_change_menu_label(int which_menu,char *old_label, char *new_label)
{
  int i;
  for (i=0;i<added_options_pos;i++)
    {
      if ((added_options_menus[i] == which_menu) && (strcmp(old_label,added_options_names[i]) == 0) && (added_options[i]))
	{
	  set_button_label(added_options[i],new_label);
	  if (added_options_names[i]) FREE(added_options_names[i]);
	  added_options_names[i] = copy_string(new_label);
	  return(0);
	}
    }
  return(-1);
}

int gh_set_menu_sensitive(int which_menu,char *old_label, int on)
{
  int i;
  for (i=0;i<added_options_pos;i++)
    {
      if ((added_options_menus[i] == which_menu) && (strcmp(old_label,added_options_names[i]) == 0) && (added_options[i]))
	{
	  set_sensitive(added_options[i],on);
	  return(0);
	}
    }
  return(-1);
}

int gh_add_to_main_menu(snd_state *ss, char *label)
{
  GtkWidget *m,*mc;
  m = gtk_menu_item_new_with_label(label);
  set_background(m,(ss->sgx)->highlight_color);
  gtk_menu_bar_append(GTK_MENU_BAR(mw[menu_menu]),m);
  gtk_widget_show(m);

  mc = gtk_menu_new();
  set_background(mc,(ss->sgx)->basic_color);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(m),mc);

  new_menu++;
  if (new_menu < MAX_MAIN_MENUS)
    {
      added_menus[new_menu] = mc;
      return(new_menu);
    }
  else return(-1);
}

int gh_add_to_menu(snd_state *ss, int which_menu, char *label, int callb)
{
  GtkWidget *m,*menw;
   switch (which_menu)
    {
    case 0: menw = mw[f_cascade_menu]; break;
    case 1: menw = mw[e_cascade_menu]; break;
    case 2: menw = mw[v_cascade_menu]; break;
    case 3: menw = mw[o_cascade_menu]; break;
    case 4: menw = mw[h_cascade_menu]; break;
    default: 
      if (which_menu < MAX_MAIN_MENUS)
	menw = added_menus[which_menu]; 
      else return(-1);
      break;
    }
  m = gtk_menu_item_new_with_label(label);
  gtk_menu_append(GTK_MENU(menw),m);
  set_background(m,(ss->sgx)->basic_color);
  gtk_widget_show(m);
  gtk_object_set_user_data(GTK_OBJECT(m),(gpointer)callb);
  gtk_signal_connect(GTK_OBJECT(m),"activate",GTK_SIGNAL_FUNC(GH_Callback),(gpointer)ss);
  add_option(m,which_menu,label);
  return(0);
}

int gh_remove_from_menu(int which_menu, char *label)
{
  return(remove_option(which_menu,label));
}


#if 0
/* -------------------------------- POPUP MENU -------------------------------- */

static void Popup_Play_Callback(GtkWidget *w,gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_info *sp;
  start_playing(sp=any_selected_sound(ss),0,NO_END_SPECIFIED);
  set_play_button(sp,1);
  gtk_widget_hide(popup_menu);
}

static void Popup_Save_Callback(GtkWidget *w,gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  save_edits(any_selected_sound(ss),NULL);
  gtk_widget_hide(popup_menu);
}

static void Popup_Undo_Callback(GtkWidget *w,gpointer clientData) 
{
  undo_EDIT((void *)clientData,1);
  gtk_widget_hide(popup_menu);
}

static void Popup_Redo_Callback(GtkWidget *w,gpointer clientData) 
{
  redo_EDIT((void *)clientData,1);
  gtk_widget_hide(popup_menu);
}

static void Popup_Info_Callback(GtkWidget *w,gpointer clientData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_info *sp;
  sp = selected_sound(ss);
  if (sp) display_info(sp);
  gtk_widget_hide(popup_menu);
}

  GtkWidget *m1,*m5;
static gint Popup_Dismiss_Callback(GtkWidget *w, GdkEvent *event, gpointer clientData)
{
fprintf(stderr,"released");
  gtk_widget_destroy(popup_menu);
  popup_menu = NULL;
  return(FALSE);
}

static gint Popup_Destroy_Callback(GtkWidget *w, GdkEvent *event, gpointer clientData)
{
  gtk_widget_destroy(popup_menu);
  popup_menu = NULL;
  return(FALSE);
}

#endif
void create_popup_menu(snd_state *ss, guint button, guint32 time)
{
#if 0
  if (!popup_menu)
    {
      popup_menu = gtk_window_new(GTK_WINDOW_POPUP);
      gtk_window_set_position (GTK_WINDOW(popup_menu), GTK_WIN_POS_MOUSE);
      gtk_signal_connect(GTK_OBJECT(popup_menu),"button_release_event",GTK_SIGNAL_FUNC(Popup_Dismiss_Callback),(gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(popup_menu),"delete_event",GTK_SIGNAL_FUNC(Popup_Destroy_Callback),(gpointer)ss);
      /*
      gtk_widget_set_app_paintable (GTK_WIDGET (popup_menu), TRUE);
      gtk_widget_realize (popup_menu);
      */
      m5 = gtk_vbox_new(FALSE,0); /* not homogenous, spacing 0 */
      gtk_container_add(GTK_CONTAINER(popup_menu),m5);
      set_background(m5,(ss->sgx)->basic_color);
      gtk_widget_show(m5);

      m1 = gtk_menu_bar_new();
      gtk_box_pack_start(GTK_BOX(m5),m1,FALSE,TRUE,0);
      set_background(m1,(ss->sgx)->basic_color);
      gtk_widget_show(m1);
      gtk_signal_connect(GTK_OBJECT(m1),"button_release_event",GTK_SIGNAL_FUNC(Popup_Dismiss_Callback),(gpointer)ss);

      popup_children[W_pop_menu] = gtk_menu_item_new_with_label("snd");
      set_background(popup_children[W_pop_menu],(ss->sgx)->basic_color);
      gtk_menu_bar_append(GTK_MENU_BAR(m1),popup_children[W_pop_menu]);
      gtk_widget_show(popup_children[W_pop_menu]);

      popup_children[W_pop_sep] = gtk_menu_new();
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(popup_children[W_pop_menu]),popup_children[W_pop_sep]);
      set_background(popup_children[W_pop_sep],(ss->sgx)->basic_color);

      popup_children[W_pop_play] = gtk_menu_item_new_with_label(STR_Play);
      gtk_menu_append(GTK_MENU(popup_children[W_pop_sep]),popup_children[W_pop_play]);
      set_background(popup_children[W_pop_play],(ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_play]),"activate",GTK_SIGNAL_FUNC(Popup_Play_Callback),(gpointer)ss);
      gtk_widget_show(popup_children[W_pop_play]);

      popup_children[W_pop_undo] = gtk_menu_item_new_with_label(STR_Undo);
      gtk_menu_append(GTK_MENU(popup_children[W_pop_sep]),popup_children[W_pop_undo]);
      set_background(popup_children[W_pop_undo],(ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_undo]),"activate",GTK_SIGNAL_FUNC(Popup_Undo_Callback),(gpointer)ss);
      set_sensitive(popup_children[W_pop_undo],FALSE);
      gtk_widget_show(popup_children[W_pop_undo]);
      
      popup_children[W_pop_redo] = gtk_menu_item_new_with_label(STR_Redo);
      gtk_menu_append(GTK_MENU(popup_children[W_pop_sep]),popup_children[W_pop_redo]);
      set_background(popup_children[W_pop_redo],(ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_redo]),"activate",GTK_SIGNAL_FUNC(Popup_Redo_Callback),(gpointer)ss);
      set_sensitive(popup_children[W_pop_redo],FALSE);
      gtk_widget_show(popup_children[W_pop_redo]);
      
      popup_children[W_pop_save] = gtk_menu_item_new_with_label(STR_Save);
      gtk_menu_append(GTK_MENU(popup_children[W_pop_sep]),popup_children[W_pop_save]);
      set_background(popup_children[W_pop_save],(ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_save]),"activate",GTK_SIGNAL_FUNC(Popup_Save_Callback),(gpointer)ss);
      gtk_widget_show(popup_children[W_pop_save]);

      popup_children[W_pop_info] = gtk_menu_item_new_with_label(STR_Info);
      gtk_menu_append(GTK_MENU(popup_children[W_pop_sep]),popup_children[W_pop_info]);
      set_background(popup_children[W_pop_info],(ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(popup_children[W_pop_info]),"activate",GTK_SIGNAL_FUNC(Popup_Info_Callback),(gpointer)ss);
      set_sensitive(popup_children[W_pop_info],FALSE);
      gtk_widget_show(popup_children[W_pop_info]);
    }
  gtk_widget_show(popup_menu);
#endif
}

#if 0
static gint middle_button_press (GtkWidget *widget, GdkEvent *bevent, gpointer data)
{
  GdkEventButton *event = (GdkEventButton *) bevent; 
  if ((event->type == GDK_BUTTON_PRESS) && (event->button == 2))
    {
      create_popup_menu((snd_state *)data,event->button,event->time);
      /* gtk_menu_popup(GTK_MENU(popup_children[W_pop_sep]),m1,popup_children[W_pop_menu],NULL,NULL,event->button,event->time); */
      return(TRUE);
    }
  return(FALSE);
}

static void init_popup(void)
{
  snd_state *ss;
  ss = get_global_state();
  gtk_widget_add_events (MAIN_SHELL(ss),gtk_widget_get_events(MAIN_SHELL(ss)) | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
  gtk_signal_connect(GTK_OBJECT(MAIN_SHELL(ss)),"button_press_event",GTK_SIGNAL_FUNC(middle_button_press),(gpointer)ss); 
}
#endif

#if HAVE_GUILE_GTK
#include <guile-gtk.h>

#define Sg_menu_bar_widget                 "sg-menu-bar-widget"
#define Sg_file_menu_widget                "sg-file-menu-widget"
#define Sg_edit_menu_widget                "sg-edit-menu-widget"
#define Sg_view_menu_widget                "sg-view-menu-widget"
#define Sg_help_menu_widget                "sg-help-menu-widget"
#define Sg_options_menu_widget             "sg-options-menu-widget"

#define Sg_file_open_menu_widget           "sg-file-open-menu-widget"
#define Sg_file_close_menu_widget          "sg-file-close-menu-widget"
#define Sg_file_save_menu_widget           "sg-file-save-menu-widget"
#define Sg_file_save_as_menu_widget        "sg-file-save-as-menu-widget"
#define Sg_file_revert_menu_widget         "sg-file-revert-menu-widget"
#define Sg_file_exit_menu_widget           "sg-file-exit-menu-widget"
#define Sg_file_new_menu_widget            "sg-file-new-menu-widget"
#define Sg_file_view_menu_widget           "sg-file-view-menu-widget"
#define Sg_file_print_menu_widget          "sg-file-print-menu-widget"
#define Sg_file_mix_menu_widget            "sg-file-mix-menu-widget"
#define Sg_file_update_menu_widget         "sg-file-update-menu-widget"
#define Sg_file_record_menu_widget         "sg-file-record-menu-widget"
#define Sg_edit_cut_menu_widget            "sg-edit-cut-menu-widget"
#define Sg_edit_paste_menu_widget          "sg-edit-paste-menu-widget"
#define Sg_edit_mix_menu_widget            "sg-edit-mix-menu-widget"
#define Sg_edit_play_menu_widget           "sg-edit-play-menu-widget"
#define Sg_edit_save_as_menu_widget        "sg-edit-save-as-menu-widget"
#define Sg_edit_undo_menu_widget           "sg-edit-undo-menu-widget"
#define Sg_edit_redo_menu_widget           "sg-edit-redo-menu-widget"
#define Sg_edit_find_menu_widget           "sg-edit-find-menu-widget"
#define Sg_edit_edenv_menu_widget          "sg-edit-edenv-menu-widget"
#define Sg_edit_header_menu_widget         "sg-edit-header-menu-widget"
#define Sg_edit_select_all_menu_widget     "sg-edit-select-all_menu"
#define Sg_help_about_snd_menu_widget      "sg-help-about-snd-menu-widget"
#define Sg_help_fft_menu_widget            "sg-help-fft-menu-widget"
#define Sg_help_find_menu_widget           "sg-help-find-menu-widget"
#define Sg_help_undo_menu_widget           "sg-help-undo-menu-widget"
#define Sg_help_sync_menu_widget           "sg-help-sync-menu-widget"
#define Sg_help_speed_menu_widget          "sg-help-speed-menu-widget"
#define Sg_help_expand_menu_widget         "sg-help-expand-menu-widget"
#define Sg_help_contrast_menu_widget       "sg-help-contrast-menu-widget"
#define Sg_help_reverb_menu_widget         "sg-help-reverb-menu-widget"
#define Sg_help_env_menu_widget            "sg-help-env-menu-widget"
#define Sg_help_marks_menu_widget          "sg-help-marks-menu-widget"
#define Sg_help_sound_files_menu_widget    "sg-help-sound-files-menu-widget"
#define Sg_help_init_file_menu_widget      "sg-help-init-file-menu-widget"
#define Sg_help_mix_menu_widget            "sg-help-mix-menu-widget"
#define Sg_help_recording_menu_widget      "sg-help-recording-menu-widget"
#define Sg_help_clm_menu_widget            "sg-help-clm-menu-widget"
#define Sg_help_news_menu_widget           "sg-help-news-menu-widget"
#define Sg_options_transform_menu_widget   "sg-options-transform-menu-widget"
#define Sg_options_focus_style_menu_widget "sg-options-focus-style-menu-widget"
#define Sg_options_focus_cascade_menu_widget "sg-options-focus-cascade-menu-widget"
#define Sg_options_save_menu_widget        "sg-options-save-menu-widget"
#define Sg_options_save_state_menu_widget  "sg-options-save-state-menu-widget"
#define Sg_options_speed_menu_widget       "sg-options-speed-menu-widget"
#define Sg_options_speed_cascade_menu_widget "sg-options-speed-cascade-menu-widget"
#define Sg_options_stats_menu_widget       "sg-options-stats-menu-widget"
#define Sg_view_graph_style_menu_widget    "sg-view-graph-style-menu-widget"
#define Sg_view_graph_style_cascade_menu_widget "sg-view-graph-style-cascade-menu-widget"
#define Sg_view_marks_menu_widget          "sg-view-marks-menu-widget"
#define Sg_view_zero_menu_widget           "sg-view-zero-menu-widget"
#define Sg_view_cursor_menu_widget         "sg-view-cursor-menu-widget"
#define Sg_view_ctrls_menu_widget          "sg-view-ctrls-menu-widget"
#define Sg_view_listener_menu_widget       "sg-view-listener-menu-widget"
#define Sg_view_region_menu_widget         "sg-view-region-menu-widget"
#define Sg_view_combine_menu_widget        "sg-view-combine-menu-widget"
#define Sg_view_combine_cascade_menu_widget "sg-view-combine-cascade-menu-widget"
#define Sg_view_color_menu_widget          "sg-view-color-menu-widget"
#define Sg_view_orientation_menu_widget "sg-view-orientation-menu-widget"
#define Sg_view_files_menu_widget          "sg-view-files-menu-widget"
#define Sg_view_consoles_menu_widget       "sg-view-consoles-menu-widget"
#define Sg_view_x_axis_menu_widget         "sg-view-x-axis-menu-widget"
#define Sg_view_x_axis_cascade_menu_widget "sg-view-x-axis-cascade-menu-widget"
#define Sg_view_error_history_menu_widget  "sg-view-error-history-menu-widget"


static SCM sg_menu_bar_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[menu_menu])));}
static SCM sg_file_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_cascade_menu])));}
static SCM sg_help_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_cascade_menu])));}
static SCM sg_edit_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_cascade_menu])));}
static SCM sg_view_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_cascade_menu])));}
static SCM sg_options_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_cascade_menu])));}
static SCM sg_file_open_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_open_menu])));}
static SCM sg_file_close_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_close_menu])));}
static SCM sg_file_save_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_save_menu])));}
static SCM sg_file_save_as_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_save_as_menu])));}
static SCM sg_file_revert_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_revert_menu])));}
static SCM sg_file_exit_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_exit_menu])));}
static SCM sg_file_new_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_new_menu])));}
static SCM sg_file_view_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_view_menu])));}
static SCM sg_file_print_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_print_menu])));}
static SCM sg_file_mix_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_mix_menu])));}
static SCM sg_file_update_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_update_menu])));}
static SCM sg_file_record_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[f_record_menu])));}
static SCM sg_edit_cut_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_cut_menu])));}
static SCM sg_edit_paste_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_paste_menu])));}
static SCM sg_edit_mix_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_mix_menu])));}
static SCM sg_edit_play_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_play_menu])));}
static SCM sg_edit_save_as_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_save_as_menu])));}
static SCM sg_edit_undo_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_undo_menu])));}
static SCM sg_edit_redo_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_redo_menu])));}
static SCM sg_edit_find_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_find_menu])));}
static SCM sg_edit_edenv_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_edenv_menu])));}
static SCM sg_edit_header_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_header_menu])));}
static SCM sg_edit_select_all_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[e_select_all_menu])));}
static SCM sg_help_about_snd_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_about_snd_menu])));}
static SCM sg_help_fft_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_fft_menu])));}
static SCM sg_help_find_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_find_menu])));}
static SCM sg_help_undo_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_undo_menu])));}
static SCM sg_help_sync_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_sync_menu])));}
static SCM sg_help_speed_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_speed_menu])));}
static SCM sg_help_expand_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_expand_menu])));}
static SCM sg_help_contrast_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_contrast_menu])));}
static SCM sg_help_reverb_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_reverb_menu])));}
static SCM sg_help_env_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_env_menu])));}
static SCM sg_help_marks_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_marks_menu])));}
static SCM sg_help_sound_files_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_sound_files_menu])));}
static SCM sg_help_init_file_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_init_file_menu])));}
static SCM sg_help_mix_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_mix_menu])));}
static SCM sg_help_recording_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_recording_menu])));}
static SCM sg_help_clm_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_clm_menu])));}
static SCM sg_help_news_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[h_news_menu])));}
static SCM sg_options_transform_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_transform_menu])));}
static SCM sg_options_focus_style_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_focus_style_menu])));}
static SCM sg_options_focus_cascade_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_focus_cascade_menu])));}
static SCM sg_options_save_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_save_menu])));}
static SCM sg_options_save_state_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_save_state_menu])));}
static SCM sg_options_speed_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_speed_menu])));}
static SCM sg_options_speed_cascade_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_speed_cascade_menu])));}
static SCM sg_options_stats_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[o_stats_menu])));}
static SCM sg_view_graph_style_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_graph_style_menu])));}
static SCM sg_view_graph_style_cascade_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_graph_style_cascade_menu])));}
static SCM sg_view_marks_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_marks_menu])));}
static SCM sg_view_zero_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_zero_menu])));}
static SCM sg_view_cursor_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_cursor_menu])));}
static SCM sg_view_ctrls_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_ctrls_menu])));}
static SCM sg_view_listener_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_listener_menu])));}
static SCM sg_view_region_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_region_menu])));}
static SCM sg_view_combine_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_combine_menu])));}
static SCM sg_view_combine_cascade_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_combine_cascade_menu])));}
static SCM sg_view_color_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_color_menu])));}
static SCM sg_view_orientation_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_orientation_menu])));}
static SCM sg_view_files_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_files_menu])));}
static SCM sg_view_consoles_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_consoles_menu])));}
static SCM sg_view_x_axis_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_x_axis_menu])));}
static SCM sg_view_x_axis_cascade_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_x_axis_cascade_menu])));}
static SCM sg_view_error_history_menu_widget(void) {return(sgtk_wrap_gtkobj((GtkObject *)(mw[v_error_history_menu])));}


void init_menu_widgets(SCM local_doc)
{
  gh_new_procedure0_0(Sg_menu_bar_widget,sg_menu_bar_widget);
  gh_new_procedure0_0(Sg_file_menu_widget,sg_file_menu_widget);
  gh_new_procedure0_0(Sg_edit_menu_widget,sg_edit_menu_widget);
  gh_new_procedure0_0(Sg_help_menu_widget,sg_help_menu_widget);
  gh_new_procedure0_0(Sg_view_menu_widget,sg_view_menu_widget);
  gh_new_procedure0_0(Sg_options_menu_widget,sg_options_menu_widget);

  gh_new_procedure0_0(Sg_file_open_menu_widget,sg_file_open_menu_widget);
  gh_new_procedure0_0(Sg_file_close_menu_widget,sg_file_close_menu_widget);
  gh_new_procedure0_0(Sg_file_save_menu_widget,sg_file_save_menu_widget);
  gh_new_procedure0_0(Sg_file_save_as_menu_widget,sg_file_save_as_menu_widget);
  gh_new_procedure0_0(Sg_file_revert_menu_widget,sg_file_revert_menu_widget);
  gh_new_procedure0_0(Sg_file_exit_menu_widget,sg_file_exit_menu_widget);
  gh_new_procedure0_0(Sg_file_new_menu_widget,sg_file_new_menu_widget);
  gh_new_procedure0_0(Sg_file_view_menu_widget,sg_file_view_menu_widget);
  gh_new_procedure0_0(Sg_file_print_menu_widget,sg_file_print_menu_widget);
  gh_new_procedure0_0(Sg_file_mix_menu_widget,sg_file_mix_menu_widget);
  gh_new_procedure0_0(Sg_file_update_menu_widget,sg_file_update_menu_widget);
  gh_new_procedure0_0(Sg_file_record_menu_widget,sg_file_record_menu_widget);
  gh_new_procedure0_0(Sg_edit_cut_menu_widget,sg_edit_cut_menu_widget);
  gh_new_procedure0_0(Sg_edit_paste_menu_widget,sg_edit_paste_menu_widget);
  gh_new_procedure0_0(Sg_edit_mix_menu_widget,sg_edit_mix_menu_widget);
  gh_new_procedure0_0(Sg_edit_play_menu_widget,sg_edit_play_menu_widget);
  gh_new_procedure0_0(Sg_edit_save_as_menu_widget,sg_edit_save_as_menu_widget);
  gh_new_procedure0_0(Sg_edit_undo_menu_widget,sg_edit_undo_menu_widget);
  gh_new_procedure0_0(Sg_edit_redo_menu_widget,sg_edit_redo_menu_widget);
  gh_new_procedure0_0(Sg_edit_find_menu_widget,sg_edit_find_menu_widget);
  gh_new_procedure0_0(Sg_edit_edenv_menu_widget,sg_edit_edenv_menu_widget);
  gh_new_procedure0_0(Sg_edit_header_menu_widget,sg_edit_header_menu_widget);
  gh_new_procedure0_0(Sg_edit_select_all_menu_widget,sg_edit_select_all_menu_widget);
  gh_new_procedure0_0(Sg_help_about_snd_menu_widget,sg_help_about_snd_menu_widget);
  gh_new_procedure0_0(Sg_help_fft_menu_widget,sg_help_fft_menu_widget);
  gh_new_procedure0_0(Sg_help_find_menu_widget,sg_help_find_menu_widget);
  gh_new_procedure0_0(Sg_help_undo_menu_widget,sg_help_undo_menu_widget);
  gh_new_procedure0_0(Sg_help_sync_menu_widget,sg_help_sync_menu_widget);
  gh_new_procedure0_0(Sg_help_speed_menu_widget,sg_help_speed_menu_widget);
  gh_new_procedure0_0(Sg_help_expand_menu_widget,sg_help_expand_menu_widget);
  gh_new_procedure0_0(Sg_help_contrast_menu_widget,sg_help_contrast_menu_widget);
  gh_new_procedure0_0(Sg_help_reverb_menu_widget,sg_help_reverb_menu_widget);
  gh_new_procedure0_0(Sg_help_env_menu_widget,sg_help_env_menu_widget);
  gh_new_procedure0_0(Sg_help_marks_menu_widget,sg_help_marks_menu_widget);
  gh_new_procedure0_0(Sg_help_sound_files_menu_widget,sg_help_sound_files_menu_widget);
  gh_new_procedure0_0(Sg_help_init_file_menu_widget,sg_help_init_file_menu_widget);
  gh_new_procedure0_0(Sg_help_mix_menu_widget,sg_help_mix_menu_widget);
  gh_new_procedure0_0(Sg_help_recording_menu_widget,sg_help_recording_menu_widget);
  gh_new_procedure0_0(Sg_help_clm_menu_widget,sg_help_clm_menu_widget);
  gh_new_procedure0_0(Sg_help_news_menu_widget,sg_help_news_menu_widget);
  gh_new_procedure0_0(Sg_options_transform_menu_widget,sg_options_transform_menu_widget);
  gh_new_procedure0_0(Sg_options_focus_style_menu_widget,sg_options_focus_style_menu_widget);
  gh_new_procedure0_0(Sg_options_focus_cascade_menu_widget,sg_options_focus_cascade_menu_widget);
  gh_new_procedure0_0(Sg_options_save_menu_widget,sg_options_save_menu_widget);
  gh_new_procedure0_0(Sg_options_speed_menu_widget,sg_options_speed_menu_widget);
  gh_new_procedure0_0(Sg_options_speed_cascade_menu_widget,sg_options_speed_cascade_menu_widget);
  gh_new_procedure0_0(Sg_options_save_state_menu_widget,sg_options_save_state_menu_widget);
  gh_new_procedure0_0(Sg_options_stats_menu_widget,sg_options_stats_menu_widget);
  gh_new_procedure0_0(Sg_view_graph_style_menu_widget,sg_view_graph_style_menu_widget);
  gh_new_procedure0_0(Sg_view_graph_style_cascade_menu_widget,sg_view_graph_style_cascade_menu_widget);
  gh_new_procedure0_0(Sg_view_marks_menu_widget,sg_view_marks_menu_widget);
  gh_new_procedure0_0(Sg_view_zero_menu_widget,sg_view_zero_menu_widget);
  gh_new_procedure0_0(Sg_view_cursor_menu_widget,sg_view_cursor_menu_widget);
  gh_new_procedure0_0(Sg_view_ctrls_menu_widget,sg_view_ctrls_menu_widget);
  gh_new_procedure0_0(Sg_view_listener_menu_widget,sg_view_listener_menu_widget);
  gh_new_procedure0_0(Sg_view_region_menu_widget,sg_view_region_menu_widget);
  gh_new_procedure0_0(Sg_view_combine_menu_widget,sg_view_combine_menu_widget);
  gh_new_procedure0_0(Sg_view_combine_cascade_menu_widget,sg_view_combine_cascade_menu_widget);
  gh_new_procedure0_0(Sg_view_color_menu_widget,sg_view_color_menu_widget);
  gh_new_procedure0_0(Sg_view_orientation_menu_widget,sg_view_orientation_menu_widget);
  gh_new_procedure0_0(Sg_view_files_menu_widget,sg_view_files_menu_widget);
  gh_new_procedure0_0(Sg_view_consoles_menu_widget,sg_view_consoles_menu_widget);
  gh_new_procedure0_0(Sg_view_x_axis_menu_widget,sg_view_x_axis_menu_widget);
  gh_new_procedure0_0(Sg_view_x_axis_cascade_menu_widget,sg_view_x_axis_cascade_menu_widget);
  gh_new_procedure0_0(Sg_view_error_history_menu_widget,sg_view_error_history_menu_widget);
}

#endif
