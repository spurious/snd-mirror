#include "snd.h"
#include "snd-menu.h"

static const char *ml[NUM_MENU_WIDGETS];

void set_menu_label(GtkWidget *w, const char *label) {if (w) set_button_label(w, label);}

/* -------------------------------- FILE MENU -------------------------------- */

static void file_menu_update_1(GtkWidget *w, gpointer info) {file_menu_update();}
static void file_open_callback(GtkWidget *w, gpointer info) {make_open_file_dialog(false, true);}
static void file_view_callback(GtkWidget *w, gpointer info) {make_open_file_dialog(true, true);}
static void file_new_callback(GtkWidget *w, gpointer info) {make_new_file_dialog(true);}
static void file_record_callback(GtkWidget *w, gpointer info) {snd_record_file();}
static void file_close_callback(GtkWidget *w, gpointer info) {if (any_selected_sound()) snd_close_file(any_selected_sound());}
static void file_save_callback(GtkWidget *w, gpointer info) {if (any_selected_sound()) save_edits_with_prompt(any_selected_sound());}
static void file_update_callback(GtkWidget *w, gpointer info) {update_file_from_menu();}
static void file_save_as_callback(GtkWidget *w, gpointer info) {make_sound_save_as_dialog(true);}
static void file_revert_callback(GtkWidget *w, gpointer info) {revert_file_from_menu();}
static void file_exit_callback(GtkWidget *w, gpointer info) {if (snd_exit_cleanly(EXIT_NOT_FORCED)) snd_exit(1);}
static void file_mix_callback_1(GtkWidget *w, gpointer info) {make_mix_file_dialog(true);}
static void file_insert_callback_1(GtkWidget *w, gpointer info) {make_insert_file_dialog(true);}
static void file_print_callback_1(GtkWidget *w, gpointer info) {file_print_callback(w, info);}


/* -------------------------------- EDIT MENU -------------------------------- */

static void edit_menu_update_1(GtkWidget *w, gpointer info) {edit_menu_update();}
static void edit_mix_callback(GtkWidget *w, gpointer info) {add_selection_or_region(0, selected_channel());}
static void edit_envelope_callback(GtkWidget *w, gpointer info) {create_envelope_editor();}
static void edit_cut_callback(GtkWidget *w, gpointer info) {delete_selection(UPDATE_DISPLAY);}
static void edit_paste_callback(GtkWidget *w, gpointer info) {insert_selection_from_menu();}
static void edit_save_as_callback(GtkWidget *w, gpointer info) {make_selection_save_as_dialog(true);}
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

static void view_menu_update_1(GtkWidget *w, gpointer info) {view_menu_update();}
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

static void view_controls_callback(GtkWidget *w, gpointer info)
{
  in_set_show_controls(ss, !in_show_controls(ss));
  if (in_show_controls(ss))
    show_all_controls();
  else hide_all_controls(); 

}

#if HAVE_EXTENSION_LANGUAGE
static void view_listener_callback(GtkWidget *w, gpointer info) {handle_listener(!(listener_is_visible()));}
#endif

static void view_mix_dialog_callback(GtkWidget *w, gpointer info) {make_mix_dialog();}
static void view_track_dialog_callback(GtkWidget *w, gpointer info) {make_track_dialog();}
static void view_region_callback_1(GtkWidget *w, gpointer info) {view_region_callback(w, info);}
static void view_orientation_callback_1(GtkWidget *w, gpointer info) {view_orientation_callback(w, info);}
static void view_color_callback_1(GtkWidget *w, gpointer info) {view_color_callback(w, info);}
static void view_files_callback(GtkWidget *w, gpointer info) {start_view_files_dialog(true, false);}

static void view_x_axis_seconds_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_IN_SECONDS);}
static void view_x_axis_clock_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_AS_CLOCK);}
static void view_x_axis_beats_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_IN_BEATS);}
static void view_x_axis_measures_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_IN_MEASURES);}
static void view_x_axis_samples_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_IN_SAMPLES);}
static void view_x_axis_percentage_callback(GtkWidget *w, gpointer info) {set_x_axis_style(X_AXIS_AS_PERCENTAGE);}

static void view_no_axes_callback(GtkWidget *w, gpointer info) {set_show_axes(SHOW_NO_AXES);}
static void view_all_axes_callback(GtkWidget *w, gpointer info) {set_show_axes(SHOW_ALL_AXES);}
static void view_just_x_axis_callback(GtkWidget *w, gpointer info) {set_show_axes(SHOW_X_AXIS);}
static void view_all_axes_unlabelled_callback(GtkWidget *w, gpointer info) {set_show_axes(SHOW_ALL_AXES_UNLABELLED);}
static void view_just_x_axis_unlabelled_callback(GtkWidget *w, gpointer info) {set_show_axes(SHOW_X_AXIS_UNLABELLED);}
static void view_bare_x_axis_callback(GtkWidget *w, gpointer info) {set_show_axes(SHOW_BARE_X_AXIS);}


/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_menu_update_1(GtkWidget *w, gpointer info) {options_menu_update();}
static void options_transform_callback(GtkWidget *w, gpointer info) {fire_up_transform_dialog(true);}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(GtkWidget *w, gpointer info) {save_options_from_menu();}
#endif
static void options_focus_right_callback(GtkWidget *w, gpointer info, gpointer data) {set_zoom_focus_style(ZOOM_FOCUS_RIGHT);}
static void options_focus_left_callback(GtkWidget *w, gpointer info, gpointer data) {set_zoom_focus_style(ZOOM_FOCUS_LEFT);}
static void options_focus_middle_callback(GtkWidget *w, gpointer info, gpointer data) {set_zoom_focus_style(ZOOM_FOCUS_MIDDLE);}
static void options_focus_active_callback(GtkWidget *w, gpointer info, gpointer data) {set_zoom_focus_style(ZOOM_FOCUS_ACTIVE);}

#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(GtkWidget *w, gpointer info) {save_state_from_menu();}
#endif

static void options_preferences_callback(GtkWidget *w, gpointer info, gpointer data) {start_preferences_dialog();}


/* -------------------------------- HELP MENU -------------------------------- */

static void help_about_snd_callback(GtkWidget *w, gpointer info) {about_snd_help();}
static void help_fft_callback(GtkWidget *w, gpointer info) {fft_help();}
#if HAVE_EXTENSION_LANGUAGE
static void help_find_callback(GtkWidget *w, gpointer info) {find_help();}
static void help_init_file_callback(GtkWidget *w, gpointer info) {init_file_help();}
#endif
static void help_undo_callback(GtkWidget *w, gpointer info) {undo_help();}
static void help_sync_callback(GtkWidget *w, gpointer info) {sync_help();}
static void help_debug_callback(GtkWidget *w, gpointer info) {debug_help();}
static void help_controls_callback(GtkWidget *w, gpointer info) {controls_help();}
static void help_env_callback(GtkWidget *w, gpointer info) {env_help();}
static void help_marks_callback(GtkWidget *w, gpointer info) {marks_help();}
static void help_mix_callback(GtkWidget *w, gpointer info) {mix_help();}
static void help_track_callback(GtkWidget *w, gpointer info) {track_help();}
static void help_sound_files_callback(GtkWidget *w, gpointer info) {sound_files_help();}
static void help_recording_callback(GtkWidget *w, gpointer info) {recording_help();}
static void help_keys_callback(GtkWidget *w, gpointer info) {key_binding_help();}
static void help_play_callback(GtkWidget *w, gpointer info) {play_help();}
static void help_resample_callback(GtkWidget *w, gpointer info) {resample_help();}
static void help_reverb_callback(GtkWidget *w, gpointer info) {reverb_help();}
static void help_insert_callback(GtkWidget *w, gpointer info) {insert_help();}
static void help_delete_callback(GtkWidget *w, gpointer info) {delete_help();}
static void help_filter_callback(GtkWidget *w, gpointer info) {filter_help();}
static void help_save_callback(GtkWidget *w, gpointer info) {save_help();}
static void help_region_callback(GtkWidget *w, gpointer info) {region_help();}
static void help_selection_callback(GtkWidget *w, gpointer info) {selection_help();}
static void help_colors_callback(GtkWidget *w, gpointer info) {colors_help();}

void check_menu_labels(int key, int state, bool extended) {}

static void menu_drop_watcher(GtkWidget *w, const char *filename, int x, int y, void *data)
{
  snd_info *sp = NULL;
  ss->open_requestor = FROM_DRAG_AND_DROP;
  sp = snd_open_file(filename, FILE_READ_WRITE);
  if (sp) select_channel(sp, 0);
}

static bool have_drag_title = false;
static void menu_drag_watcher(GtkWidget *w, const char *str, int x, int y, drag_style_t dtype, void *data)
{
  char *new_title;
  switch (dtype)
    {
    case DRAG_MOTION:
    case DRAG_ENTER:
      if (!have_drag_title)
	{
	  new_title = mus_format("%s: drop to open file", ss->startup_title);
	  gtk_window_set_title(GTK_WINDOW(MAIN_SHELL(ss)), new_title);
	  have_drag_title = true;
	  FREE(new_title);
	}
      break;
    case DRAG_LEAVE:
      reflect_file_change_in_title();
      have_drag_title = false;
      break;
    }
}



/* -------------------------------- MAIN MENU -------------------------------- */

GtkWidget *add_menu(void)
{
  ss->sgx->mw = (GtkWidget **)calloc(NUM_MENU_WIDGETS, sizeof(GtkWidget *));

  main_menu = gtk_menu_bar_new();
  ml[m_menu] = NULL;
  add_drag_and_drop(main_menu, menu_drop_watcher, menu_drag_watcher, NULL);
  gtk_box_pack_start(GTK_BOX(MAIN_PANE(ss)), main_menu, false, true, 0);
  gtk_widget_show(main_menu);

  /* FILE MENU */
  file_menu = gtk_menu_item_new_with_label(_("File"));
  ml[f_menu] = _("File");
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), file_menu);
  gtk_widget_show(file_menu);

  file_cascade_menu = gtk_menu_new();
  ml[f_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_menu), file_cascade_menu);

  file_open_menu = gtk_image_menu_item_new_with_label(_("Open"));
  ml[f_open_menu] = _("Open");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_open_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_open_menu), gtk_image_new_from_stock(GTK_STOCK_OPEN, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_open_menu);
  SG_SIGNAL_CONNECT(file_open_menu, "activate", file_open_callback, NULL);

  file_close_menu = gtk_image_menu_item_new_with_label(_("Close"));
  ml[f_close_menu] = _("Close");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_close_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_close_menu), gtk_image_new_from_stock(GTK_STOCK_CLOSE, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_close_menu);
  set_sensitive(file_close_menu, false);
  SG_SIGNAL_CONNECT(file_close_menu, "activate", file_close_callback, NULL);
  
  file_save_menu = gtk_image_menu_item_new_with_label(_("Save"));
  ml[f_save_menu] = _("Save");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_save_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_save_menu), gtk_image_new_from_stock(GTK_STOCK_SAVE, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_save_menu);
  set_sensitive(file_save_menu, false);
  SG_SIGNAL_CONNECT(file_save_menu, "activate", file_save_callback, NULL);
  
  file_save_as_menu = gtk_image_menu_item_new_with_label(_("Save as"));
  ml[f_save_as_menu] = _("Save as");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_save_as_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_save_as_menu), gtk_image_new_from_stock(GTK_STOCK_SAVE_AS, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_save_as_menu);
  set_sensitive(file_save_as_menu, false);
  SG_SIGNAL_CONNECT(file_save_as_menu, "activate", file_save_as_callback, NULL);
  
  file_revert_menu = gtk_image_menu_item_new_with_label(_("Revert"));
  ml[f_revert_menu] = _("Revert");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_revert_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_revert_menu), gtk_image_new_from_stock(GTK_STOCK_REVERT_TO_SAVED, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_revert_menu);
  set_sensitive(file_revert_menu, false);
  SG_SIGNAL_CONNECT(file_revert_menu, "activate", file_revert_callback, NULL);
  
  file_mix_menu = gtk_image_menu_item_new_with_label(_("Mix"));
  ml[f_mix_menu] = _("Mix");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_mix_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_mix_menu), gtk_image_new_from_stock(GTK_STOCK_ADD, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_mix_menu);
  set_sensitive(file_mix_menu, false);
  SG_SIGNAL_CONNECT(file_mix_menu, "activate", file_mix_callback_1, NULL);

  file_insert_menu = gtk_image_menu_item_new_with_label(_("Insert"));
  ml[f_insert_menu] = _("Insert");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_insert_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_insert_menu), gtk_image_new_from_stock(GTK_STOCK_PASTE, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_insert_menu);
  set_sensitive(file_insert_menu, false);
  SG_SIGNAL_CONNECT(file_insert_menu, "activate", file_insert_callback_1, NULL);

  file_update_menu = gtk_image_menu_item_new_with_label(_("Update"));
  ml[f_update_menu] = _("Update");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_update_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_update_menu), gtk_image_new_from_stock(GTK_STOCK_REFRESH, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_update_menu);
  set_sensitive(file_update_menu, false);
  SG_SIGNAL_CONNECT(file_update_menu, "activate", file_update_callback, NULL);

  file_new_menu = gtk_image_menu_item_new_with_label(_("New"));
  ml[f_new_menu] = _("New");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_new_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_new_menu), gtk_image_new_from_stock(GTK_STOCK_NEW, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_new_menu);
  SG_SIGNAL_CONNECT(file_new_menu, "activate", file_new_callback, NULL);

  file_record_menu = gtk_image_menu_item_new_with_label(_("Record"));
  ml[f_record_menu] = _("Record");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_record_menu);
#ifdef GTK_STOCK_MEDIA_RECORD
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_record_menu), gtk_image_new_from_stock(GTK_STOCK_MEDIA_RECORD, GTK_ICON_SIZE_MENU));
#else
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_record_menu), gtk_image_new_from_stock(GTK_STOCK_EXECUTE, GTK_ICON_SIZE_MENU));
#endif
  gtk_widget_show(file_record_menu);
  SG_SIGNAL_CONNECT(file_record_menu, "activate", file_record_callback, NULL);

  file_view_menu = gtk_image_menu_item_new_with_label(_("View"));
  ml[f_view_menu] = _("View");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_view_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_view_menu), gtk_image_new_from_stock(GTK_STOCK_OPEN, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_view_menu);
  SG_SIGNAL_CONNECT(file_view_menu, "activate", file_view_callback, NULL);

  file_print_menu = gtk_image_menu_item_new_with_label(_("Print"));
  ml[f_print_menu] = _("Print");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_print_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_print_menu), gtk_image_new_from_stock(GTK_STOCK_PRINT, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_print_menu);
  set_sensitive(file_print_menu, false);
  SG_SIGNAL_CONNECT(file_print_menu, "activate", file_print_callback_1, NULL);

  file_sep_menu = gtk_menu_item_new();
  ml[f_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_sep_menu);
  gtk_widget_show(file_sep_menu);
  gtk_widget_modify_bg(file_sep_menu, GTK_STATE_NORMAL, ss->sgx->black);

  file_exit_menu = gtk_image_menu_item_new_with_label(_("Exit"));
  ml[f_exit_menu] = _("Exit");
  gtk_menu_shell_append(GTK_MENU_SHELL(file_cascade_menu), file_exit_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(file_exit_menu), gtk_image_new_from_stock(GTK_STOCK_QUIT, GTK_ICON_SIZE_MENU));
  gtk_widget_show(file_exit_menu);
  SG_SIGNAL_CONNECT(file_exit_menu, "activate", file_exit_callback, NULL);


  /* EDIT MENU */
  edit_menu = gtk_menu_item_new_with_label(_("Edit"));
  ml[e_menu] = _("Edit");
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), edit_menu);
  gtk_widget_show(edit_menu);

  edit_cascade_menu = gtk_menu_new();
  ml[e_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(edit_menu), edit_cascade_menu);
  
  edit_undo_menu = gtk_image_menu_item_new_with_label(_("Undo"));
  ml[e_undo_menu] = _("Undo");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_undo_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_undo_menu), gtk_image_new_from_stock(GTK_STOCK_UNDO, GTK_ICON_SIZE_MENU));
  gtk_widget_show(edit_undo_menu);
  set_sensitive(edit_undo_menu, false);
  SG_SIGNAL_CONNECT(edit_undo_menu, "activate", edit_undo_callback, NULL);

  edit_redo_menu = gtk_image_menu_item_new_with_label(_("Redo"));
  ml[e_redo_menu] = _("Redo");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_redo_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_redo_menu), gtk_image_new_from_stock(GTK_STOCK_REDO, GTK_ICON_SIZE_MENU));
  gtk_widget_show(edit_redo_menu);
  set_sensitive(edit_redo_menu, false);
  SG_SIGNAL_CONNECT(edit_redo_menu, "activate", edit_redo_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  edit_find_menu = gtk_image_menu_item_new_with_label(_("Find"));
  ml[e_find_menu] = _("Find");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_find_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_find_menu), gtk_image_new_from_stock(GTK_STOCK_FIND, GTK_ICON_SIZE_MENU));
  gtk_widget_show(edit_find_menu);
  set_sensitive(edit_find_menu, false);
  SG_SIGNAL_CONNECT(edit_find_menu, "activate", edit_find_callback_1, NULL);
#endif

  edit_select_sep_menu = gtk_menu_item_new();
  ml[e_select_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_select_sep_menu);
  gtk_widget_show(edit_select_sep_menu);
  gtk_widget_modify_bg(edit_select_sep_menu, GTK_STATE_NORMAL, ss->sgx->black);

  edit_cut_menu = gtk_image_menu_item_new_with_label(_("Delete Selection"));
  ml[e_cut_menu] = _("Delete Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_cut_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_cut_menu), gtk_image_new_from_stock(GTK_STOCK_CUT, GTK_ICON_SIZE_MENU));
  gtk_widget_show(edit_cut_menu);
  set_sensitive(edit_cut_menu, false);
  SG_SIGNAL_CONNECT(edit_cut_menu, "activate", edit_cut_callback, NULL);

  edit_paste_menu = gtk_image_menu_item_new_with_label(_("Insert Selection"));
  ml[e_paste_menu] = _("Insert Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_paste_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_paste_menu), gtk_image_new_from_stock(GTK_STOCK_PASTE, GTK_ICON_SIZE_MENU));
  gtk_widget_show(edit_paste_menu);
  set_sensitive(edit_paste_menu, false);
  SG_SIGNAL_CONNECT(edit_paste_menu, "activate", edit_paste_callback, NULL);

  edit_mix_menu = gtk_image_menu_item_new_with_label(_("Mix Selection"));
  ml[e_mix_menu] = _("Mix Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_mix_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_mix_menu), gtk_image_new_from_stock(GTK_STOCK_ADD, GTK_ICON_SIZE_MENU));
  gtk_widget_show(edit_mix_menu);
  set_sensitive(edit_mix_menu, false);
  SG_SIGNAL_CONNECT(edit_mix_menu, "activate", edit_mix_callback, NULL);

  edit_play_menu = gtk_image_menu_item_new_with_label(_("Play Selection"));
  ml[e_play_menu] = _("Play Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_play_menu);
#ifdef GTK_STOCK_MEDIA_PLAY
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_play_menu), gtk_image_new_from_stock(GTK_STOCK_MEDIA_PLAY, GTK_ICON_SIZE_MENU));
#else
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_undo_menu), gtk_image_new_from_stock(GTK_STOCK_EXECUTE, GTK_ICON_SIZE_MENU));
#endif
  gtk_widget_show(edit_play_menu);
  set_sensitive(edit_play_menu, false);
  SG_SIGNAL_CONNECT(edit_play_menu, "activate", edit_play_callback, NULL);

  edit_save_as_menu = gtk_image_menu_item_new_with_label(_("Save Selection"));
  ml[e_save_as_menu] = _("Save Selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_save_as_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_save_as_menu), gtk_image_new_from_stock(GTK_STOCK_SAVE_AS, GTK_ICON_SIZE_MENU));
  gtk_widget_show(edit_save_as_menu);
  set_sensitive(edit_save_as_menu, false);
  SG_SIGNAL_CONNECT(edit_save_as_menu, "activate", edit_save_as_callback, NULL);

  edit_select_all_menu = gtk_image_menu_item_new_with_label(_("Select all"));
  ml[e_select_all_menu] = _("Select all");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_select_all_menu);
#ifdef GTK_STOCK_SELECT_ALL
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_select_all_menu), gtk_image_new_from_stock(GTK_STOCK_SELECT_ALL, GTK_ICON_SIZE_MENU));
#else
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_select_all_menu), gtk_image_new_from_stock(GTK_STOCK_BOLD, GTK_ICON_SIZE_MENU));
#endif
  gtk_widget_show(edit_select_all_menu);
  set_sensitive(edit_select_all_menu, false);
  SG_SIGNAL_CONNECT(edit_select_all_menu, "activate", edit_select_all_callback, NULL);

  edit_edit_sep_menu = gtk_menu_item_new();
  ml[e_edit_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_edit_sep_menu);
  gtk_widget_show(edit_edit_sep_menu);
  gtk_widget_modify_bg(edit_edit_sep_menu, GTK_STATE_NORMAL, ss->sgx->black);

  edit_env_menu = gtk_image_menu_item_new_with_label(_("Edit Envelope"));
  ml[e_env_menu] = _("Edit Envelope");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_env_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_env_menu), gtk_image_new_from_stock(GTK_STOCK_APPLY, GTK_ICON_SIZE_MENU));
  gtk_widget_show(edit_env_menu);
  SG_SIGNAL_CONNECT(edit_env_menu, "activate", edit_envelope_callback, NULL);

  edit_header_menu = gtk_image_menu_item_new_with_label(_("Edit Header"));
  ml[e_header_menu] = _("Edit Header");
  gtk_menu_shell_append(GTK_MENU_SHELL(edit_cascade_menu), edit_header_menu);
#ifdef GTK_STOCK_EDIT
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_header_menu), gtk_image_new_from_stock(GTK_STOCK_EDIT, GTK_ICON_SIZE_MENU));
#else
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(edit_header_menu), gtk_image_new_from_stock(GTK_STOCK_CONVERT, GTK_ICON_SIZE_MENU));
#endif
  gtk_widget_show(edit_header_menu);
  set_sensitive(edit_header_menu, false);
  SG_SIGNAL_CONNECT(edit_header_menu, "activate", edit_header_callback_1, NULL);


  /* VIEW MENU */
  view_menu = gtk_menu_item_new_with_label(_("View"));
  ml[v_menu] = _("View");
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), view_menu);
  gtk_widget_show(view_menu);

  view_cascade_menu = gtk_menu_new();
  ml[v_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_menu), view_cascade_menu);

  view_controls_menu = gtk_image_menu_item_new_with_label(_("Show controls"));
  ml[v_controls_menu] = _("Show controls");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_controls_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_controls_menu), gtk_image_new_from_stock(GTK_STOCK_APPLY, GTK_ICON_SIZE_MENU));
  gtk_widget_show(view_controls_menu);
  SG_SIGNAL_CONNECT(view_controls_menu, "activate", view_controls_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  view_listener_menu = gtk_image_menu_item_new_with_label(_("Open listener"));
  ml[v_listener_menu] = _("Open listener");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_listener_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_listener_menu), gtk_image_new_from_stock(GTK_STOCK_EXECUTE, GTK_ICON_SIZE_MENU));
  gtk_widget_show(view_listener_menu);
  SG_SIGNAL_CONNECT(view_listener_menu, "activate", view_listener_callback, NULL);
#endif

  view_mix_dialog_menu = gtk_image_menu_item_new_with_label(_("Mixes"));
  ml[v_mix_dialog_menu] = _("Mixes");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_mix_dialog_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_mix_dialog_menu), gtk_image_new_from_stock(GTK_STOCK_ADD, GTK_ICON_SIZE_MENU));
  gtk_widget_show(view_mix_dialog_menu);
  SG_SIGNAL_CONNECT(view_mix_dialog_menu, "activate", view_mix_dialog_callback, NULL);

  view_track_dialog_menu = gtk_image_menu_item_new_with_label(_("Tracks"));
  ml[v_track_dialog_menu] = _("Tracks");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_track_dialog_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_track_dialog_menu), gtk_image_new_from_stock(GTK_STOCK_BOLD, GTK_ICON_SIZE_MENU));
  gtk_widget_show(view_track_dialog_menu);
  SG_SIGNAL_CONNECT(view_track_dialog_menu, "activate", view_track_dialog_callback, NULL);

  view_region_menu = gtk_image_menu_item_new_with_label(_("Regions"));
  ml[v_region_menu] = _("Regions");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_region_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_region_menu), gtk_image_new_from_stock(GTK_STOCK_CUT, GTK_ICON_SIZE_MENU));
  gtk_widget_show(view_region_menu);
  SG_SIGNAL_CONNECT(view_region_menu, "activate", view_region_callback_1, NULL);
  set_sensitive(view_region_menu, false);

  view_files_menu = gtk_image_menu_item_new_with_label(_("Files"));
  ml[v_files_menu] = _("Files");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_files_menu);
#ifdef GTK_STOCK_FILE
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_files_menu), gtk_image_new_from_stock(GTK_STOCK_FILE, GTK_ICON_SIZE_MENU));
#else
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_files_menu), gtk_image_new_from_stock(GTK_STOCK_HOME, GTK_ICON_SIZE_MENU));
#endif
  gtk_widget_show(view_files_menu);
  SG_SIGNAL_CONNECT(view_files_menu, "activate", view_files_callback, NULL);

  view_color_menu = gtk_image_menu_item_new_with_label(_("Color"));
  ml[v_color_menu] = _("Color");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_color_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_color_menu), gtk_image_new_from_stock(GTK_STOCK_SELECT_COLOR, GTK_ICON_SIZE_MENU));
  gtk_widget_show(view_color_menu);
  SG_SIGNAL_CONNECT(view_color_menu, "activate", view_color_callback_1, NULL);

  view_orientation_menu = gtk_image_menu_item_new_with_label(_("Orientation"));
  ml[v_orientation_menu] = _("Orientation");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_orientation_menu);
  gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(view_orientation_menu), gtk_image_new_from_stock(GTK_STOCK_PREFERENCES, GTK_ICON_SIZE_MENU));
  gtk_widget_show(view_orientation_menu);
  SG_SIGNAL_CONNECT(view_orientation_menu, "activate", view_orientation_callback_1, NULL);

  view_sep2_menu = gtk_menu_item_new();
  ml[v_sep2_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_sep2_menu);
  gtk_widget_show(view_sep2_menu);
  gtk_widget_modify_bg(view_sep2_menu, GTK_STATE_NORMAL, ss->sgx->black);


  view_graph_style_menu = gtk_menu_item_new_with_label(_("Graph style"));
  ml[v_graph_style_menu] = _("Graph style");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_graph_style_menu);
  gtk_widget_show(view_graph_style_menu);

  view_graph_style_cascade_menu = gtk_menu_new();
  ml[v_graph_style_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_graph_style_menu), view_graph_style_cascade_menu);

  view_lines_menu = gtk_menu_item_new_with_label(_("lines"));
  ml[v_lines_menu] = _("lines");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_graph_style_cascade_menu), view_lines_menu);
  gtk_widget_show(view_lines_menu);
  SG_SIGNAL_CONNECT(view_lines_menu, "activate", view_lines_callback, NULL);
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(view_lines_menu, false);

  view_dots_menu = gtk_menu_item_new_with_label(_("dots"));
  ml[v_dots_menu] = _("dots");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_graph_style_cascade_menu), view_dots_menu);
  gtk_widget_show(view_dots_menu);
  SG_SIGNAL_CONNECT(view_dots_menu, "activate", view_dots_callback, NULL);
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(view_dots_menu, false);

  view_filled_menu = gtk_menu_item_new_with_label(_("filled"));
  ml[v_filled_menu] = _("filled");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_graph_style_cascade_menu), view_filled_menu);
  gtk_widget_show(view_filled_menu);
  SG_SIGNAL_CONNECT(view_filled_menu, "activate", view_filled_callback, NULL);
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(view_filled_menu, false);

  view_dots_and_lines_menu = gtk_menu_item_new_with_label(_("dots and lines"));
  ml[v_dots_and_lines_menu] = _("dots and lines");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_graph_style_cascade_menu), view_dots_and_lines_menu);
  gtk_widget_show(view_dots_and_lines_menu);
  SG_SIGNAL_CONNECT(view_dots_and_lines_menu, "activate", view_dots_and_lines_callback, NULL);
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(view_dots_and_lines_menu, false);

  view_lollipops_menu = gtk_menu_item_new_with_label(_("lollipops"));
  ml[v_lollipops_menu] = _("lollipops");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_graph_style_cascade_menu), view_lollipops_menu);
  gtk_widget_show(view_lollipops_menu);
  SG_SIGNAL_CONNECT(view_lollipops_menu, "activate", view_lollipops_callback, NULL);
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(view_lollipops_menu, false);

  view_cursor_menu = gtk_menu_item_new_with_label(_("Verbose cursor"));
  ml[v_cursor_menu] = _("Verbose cursor");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_cursor_menu);
  gtk_widget_show(view_cursor_menu);
  SG_SIGNAL_CONNECT(view_cursor_menu, "activate", view_cursor_callback, NULL);


  view_combine_menu = gtk_menu_item_new_with_label(_("Channel style"));
  ml[v_combine_menu] = _("Channel style");
  gtk_widget_show(view_combine_menu);

  view_combine_cascade_menu = gtk_menu_new();
  ml[v_combine_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_combine_menu), view_combine_cascade_menu);

  view_combine_separate_menu = gtk_menu_item_new_with_label(_("separate"));
  ml[v_combine_separate_menu] = _("separate");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_combine_cascade_menu), view_combine_separate_menu);
  gtk_widget_show(view_combine_separate_menu);
  SG_SIGNAL_CONNECT(view_combine_separate_menu, "activate", view_separate_callback, NULL);
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(view_combine_separate_menu, false);

  view_combine_combined_menu = gtk_menu_item_new_with_label(_("combined"));
  ml[v_combine_combined_menu] = _("combined");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_combine_cascade_menu), view_combine_combined_menu);
  gtk_widget_show(view_combine_combined_menu);
  SG_SIGNAL_CONNECT(view_combine_combined_menu, "activate", view_combined_callback, NULL);
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(view_combine_combined_menu, false);

  view_combine_superimposed_menu = gtk_menu_item_new_with_label(_("superimposed"));
  ml[v_combine_superimposed_menu] = _("superimposed");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_combine_cascade_menu), view_combine_superimposed_menu);
  gtk_widget_show(view_combine_superimposed_menu);
  SG_SIGNAL_CONNECT(view_combine_superimposed_menu, "activate", view_superimposed_callback, NULL);
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(view_combine_superimposed_menu, false);


  view_zero_menu = gtk_menu_item_new_with_label(_("Show Y = 0"));
  ml[v_zero_menu] = _("Show Y = 0");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_zero_menu);
  gtk_widget_show(view_zero_menu);
  SG_SIGNAL_CONNECT(view_zero_menu, "activate", view_zero_callback, NULL);

  view_x_axis_menu = gtk_menu_item_new_with_label(_("X axis units"));
  ml[v_x_axis_menu] = _("X axis units");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_x_axis_menu);
  gtk_widget_show(view_x_axis_menu);

  view_x_axis_cascade_menu = gtk_menu_new();
  ml[v_x_axis_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_x_axis_menu), view_x_axis_cascade_menu);

  view_x_axis_seconds_menu = gtk_menu_item_new_with_label(_("seconds"));
  ml[v_x_axis_seconds_menu] = _("seconds");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_x_axis_cascade_menu), view_x_axis_seconds_menu);
  gtk_widget_show(view_x_axis_seconds_menu);
  SG_SIGNAL_CONNECT(view_x_axis_seconds_menu, "activate", view_x_axis_seconds_callback, NULL);
  set_sensitive(view_x_axis_seconds_menu, false);

  view_x_axis_samples_menu = gtk_menu_item_new_with_label(_("samples"));
  ml[v_x_axis_samples_menu] = _("samples");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_x_axis_cascade_menu), view_x_axis_samples_menu);
  gtk_widget_show(view_x_axis_samples_menu);
  SG_SIGNAL_CONNECT(view_x_axis_samples_menu, "activate", view_x_axis_samples_callback, NULL);

  view_x_axis_clock_menu = gtk_menu_item_new_with_label(_("clock"));
  ml[v_x_axis_clock_menu] = _("clock");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_x_axis_cascade_menu), view_x_axis_clock_menu);
  gtk_widget_show(view_x_axis_clock_menu);
  SG_SIGNAL_CONNECT(view_x_axis_clock_menu, "activate", view_x_axis_clock_callback, NULL);

  view_x_axis_percentage_menu = gtk_menu_item_new_with_label(_("percentage"));
  ml[v_x_axis_percentage_menu] = _("percentage");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_x_axis_cascade_menu), view_x_axis_percentage_menu);
  gtk_widget_show(view_x_axis_percentage_menu);
  SG_SIGNAL_CONNECT(view_x_axis_percentage_menu, "activate", view_x_axis_percentage_callback, NULL);

  view_x_axis_beats_menu = gtk_menu_item_new_with_label(_("beats"));
  ml[v_x_axis_beats_menu] = _("beats");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_x_axis_cascade_menu), view_x_axis_beats_menu);
  gtk_widget_show(view_x_axis_beats_menu);
  SG_SIGNAL_CONNECT(view_x_axis_beats_menu, "activate", view_x_axis_beats_callback, NULL);

  view_x_axis_measures_menu = gtk_menu_item_new_with_label(_("measures"));
  ml[v_x_axis_measures_menu] = _("measures");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_x_axis_cascade_menu), view_x_axis_measures_menu);
  gtk_widget_show(view_x_axis_measures_menu);
  SG_SIGNAL_CONNECT(view_x_axis_measures_menu, "activate", view_x_axis_measures_callback, NULL);


  view_axes_menu = gtk_menu_item_new_with_label(_("Axes"));
  ml[v_axes_menu] = _("Axes");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), view_axes_menu);
  gtk_widget_show(view_axes_menu);

  view_axes_cascade_menu = gtk_menu_new();
  ml[v_axes_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_axes_menu), view_axes_cascade_menu);

  view_no_axes_menu = gtk_menu_item_new_with_label(_("no axes"));
  ml[v_no_axes_menu] = _("no axes");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_axes_cascade_menu), view_no_axes_menu);
  gtk_widget_show(view_no_axes_menu);
  SG_SIGNAL_CONNECT(view_no_axes_menu, "activate", view_no_axes_callback, NULL);

  view_all_axes_menu = gtk_menu_item_new_with_label(_("both axes"));
  ml[v_all_axes_menu] = _("both axes");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_axes_cascade_menu), view_all_axes_menu);
  gtk_widget_show(view_all_axes_menu);
  SG_SIGNAL_CONNECT(view_all_axes_menu, "activate", view_all_axes_callback, NULL);

  view_just_x_axis_menu = gtk_menu_item_new_with_label(_("just x axis"));
  ml[v_just_x_axis_menu] = _("just x axis");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_axes_cascade_menu), view_just_x_axis_menu);
  gtk_widget_show(view_just_x_axis_menu);
  SG_SIGNAL_CONNECT(view_just_x_axis_menu, "activate", view_just_x_axis_callback, NULL);

  view_all_axes_unlabelled_menu = gtk_menu_item_new_with_label(_("both axes, no labels"));
  ml[v_all_axes_unlabelled_menu] = _("both axes, no labels");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_axes_cascade_menu), view_all_axes_unlabelled_menu);
  gtk_widget_show(view_all_axes_unlabelled_menu);
  SG_SIGNAL_CONNECT(view_all_axes_unlabelled_menu, "activate", view_all_axes_unlabelled_callback, NULL);

  view_just_x_axis_unlabelled_menu = gtk_menu_item_new_with_label(_("just x axis, no label"));
  ml[v_just_x_axis_unlabelled_menu] = _("just x axis, no label");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_axes_cascade_menu), view_just_x_axis_unlabelled_menu);
  gtk_widget_show(view_just_x_axis_unlabelled_menu);
  SG_SIGNAL_CONNECT(view_just_x_axis_unlabelled_menu, "activate", view_just_x_axis_unlabelled_callback, NULL);

  view_bare_x_axis_menu = gtk_menu_item_new_with_label(_("bare x axis"));
  ml[v_bare_x_axis_menu] = _("base x axis");
  gtk_menu_shell_append(GTK_MENU_SHELL(view_axes_cascade_menu), view_bare_x_axis_menu);
  gtk_widget_show(view_bare_x_axis_menu);
  SG_SIGNAL_CONNECT(view_bare_x_axis_menu, "activate", view_bare_x_axis_callback, NULL);



  /* OPTIONS MENU */
  options_menu = gtk_menu_item_new_with_label(_("Options"));
  ml[o_menu] = _("Options");
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), options_menu);
  gtk_widget_show(options_menu);

  options_cascade_menu = gtk_menu_new();
  ml[o_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(options_menu), options_cascade_menu);

  options_transform_menu = gtk_menu_item_new_with_label(_("Transform Options"));
  ml[o_transform_menu] = _("Transform Options");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_cascade_menu), options_transform_menu);
  gtk_widget_show(options_transform_menu);
  SG_SIGNAL_CONNECT(options_transform_menu, "activate", options_transform_callback, NULL);

  options_focus_style_menu = gtk_menu_item_new_with_label(_("Zoom focus"));
  ml[o_focus_style_menu] = _("Zoom focus");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_cascade_menu), options_focus_style_menu);
  gtk_widget_show(options_focus_style_menu);

  options_focus_cascade_menu = gtk_menu_new();
  ml[o_focus_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(options_focus_style_menu), options_focus_cascade_menu);

  options_focus_left_menu = gtk_menu_item_new_with_label(_("window left edge"));
  ml[o_focus_left_menu] = _("window left edge");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_focus_cascade_menu), options_focus_left_menu);
  gtk_widget_show(options_focus_left_menu);
  SG_SIGNAL_CONNECT(options_focus_left_menu, "activate", options_focus_left_callback, NULL);

  options_focus_right_menu = gtk_menu_item_new_with_label(_("window right edge"));
  ml[o_focus_right_menu] = _("window right edge");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_focus_cascade_menu), options_focus_right_menu);
  gtk_widget_show(options_focus_right_menu);
  SG_SIGNAL_CONNECT(options_focus_right_menu, "activate", options_focus_right_callback, NULL);

  options_focus_middle_menu = gtk_menu_item_new_with_label(_("window midpoint"));
  ml[o_focus_middle_menu] = _("window midpoint");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_focus_cascade_menu), options_focus_middle_menu);
  gtk_widget_show(options_focus_middle_menu);
  SG_SIGNAL_CONNECT(options_focus_middle_menu, "activate", options_focus_middle_callback, NULL);

  options_focus_active_menu = gtk_menu_item_new_with_label(_("cursor or selection"));
  ml[o_focus_active_menu] = _("cursor or selection");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_focus_cascade_menu), options_focus_active_menu);
  gtk_widget_show(options_focus_active_menu);
  SG_SIGNAL_CONNECT(options_focus_active_menu, "activate", options_focus_active_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  options_save_menu = gtk_menu_item_new_with_label(_("Save options"));
  ml[o_save_menu] = _("Save options");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_cascade_menu), options_save_menu);
  gtk_widget_show(options_save_menu);
  SG_SIGNAL_CONNECT(options_save_menu, "activate", options_save_callback, NULL);

  options_save_state_menu = gtk_menu_item_new_with_label(_("Save session"));
  ml[o_save_state_menu] = _("Save session");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_cascade_menu), options_save_state_menu);
  gtk_widget_show(options_save_state_menu);
  SG_SIGNAL_CONNECT(options_save_state_menu, "activate", options_save_state_callback, NULL);
#endif

  options_sep_menu = gtk_menu_item_new();
  ml[o_sep_menu] = NULL;
  gtk_menu_shell_append(GTK_MENU_SHELL(view_cascade_menu), options_sep_menu);
  gtk_widget_show(options_sep_menu);
  gtk_widget_modify_bg(options_sep_menu, GTK_STATE_NORMAL, ss->sgx->black);

  options_preferences_menu = gtk_menu_item_new_with_label(_("Preferences"));
  ml[o_preferences_menu] = _("Preferences");
  gtk_menu_shell_append(GTK_MENU_SHELL(options_cascade_menu), options_preferences_menu);
  gtk_widget_show(options_preferences_menu);
  SG_SIGNAL_CONNECT(options_preferences_menu, "activate", options_preferences_callback, NULL);



  /* HELP MENU */
  help_menu = gtk_menu_item_new_with_label(_("Help"));
  ml[h_menu] = _("Help");
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), help_menu);
  gtk_widget_show(help_menu);
  gtk_menu_item_set_right_justified(GTK_MENU_ITEM(help_menu), true);

  help_cascade_menu = gtk_menu_new();
  ml[h_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(help_menu), help_cascade_menu);

  help_about_snd_menu = gtk_menu_item_new_with_label(_("About Snd"));
  ml[h_about_snd_menu] = _("About Snd");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_about_snd_menu);
  gtk_widget_show(help_about_snd_menu);
  SG_SIGNAL_CONNECT(help_about_snd_menu, "activate", help_about_snd_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  help_init_file_menu = gtk_menu_item_new_with_label(_("Customization"));
  ml[h_init_file_menu] = _("Customization");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_init_file_menu);
  gtk_widget_show(help_init_file_menu);
  SG_SIGNAL_CONNECT(help_init_file_menu, "activate", help_init_file_callback, NULL);
#endif

  help_controls_menu = gtk_menu_item_new_with_label(_("Control Panel"));
  ml[h_controls_menu] = _("Control Panel");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_controls_menu);
  gtk_widget_show(help_controls_menu);
  SG_SIGNAL_CONNECT(help_controls_menu, "activate", help_controls_callback, NULL);

  help_keys_menu = gtk_menu_item_new_with_label(_("Key bindings"));
  ml[h_keys_menu] = _("Key bindings");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_keys_menu);
  gtk_widget_show(help_keys_menu);
  SG_SIGNAL_CONNECT(help_keys_menu, "activate", help_keys_callback, NULL);

  help_recording_menu = gtk_menu_item_new_with_label(_("Record"));
  ml[h_recording_menu] = _("Record");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_recording_menu);
  gtk_widget_show(help_recording_menu);
  SG_SIGNAL_CONNECT(help_recording_menu, "activate", help_recording_callback, NULL);

  help_play_menu = gtk_menu_item_new_with_label(_("Play"));
  ml[h_play_menu] = _("Play");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_play_menu);
  gtk_widget_show(help_play_menu);
  SG_SIGNAL_CONNECT(help_play_menu, "activate", help_play_callback, NULL);

  help_save_menu = gtk_menu_item_new_with_label(_("Save"));
  ml[h_save_menu] = _("Save");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_save_menu);
  gtk_widget_show(help_save_menu);
  SG_SIGNAL_CONNECT(help_save_menu, "activate", help_save_callback, NULL);

  help_mix_menu = gtk_menu_item_new_with_label(_("Mix"));
  ml[h_mix_menu] = _("Mix");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_mix_menu);
  gtk_widget_show(help_mix_menu);
  SG_SIGNAL_CONNECT(help_mix_menu, "activate", help_mix_callback, NULL);

  help_track_menu = gtk_menu_item_new_with_label(_("Track"));
  ml[h_track_menu] = _("Track");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_track_menu);
  gtk_widget_show(help_track_menu);
  SG_SIGNAL_CONNECT(help_track_menu, "activate", help_track_callback, NULL);

  help_resample_menu = gtk_menu_item_new_with_label(_("Resample"));
  ml[h_resample_menu] = _("Resample");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_resample_menu);
  gtk_widget_show(help_resample_menu);
  SG_SIGNAL_CONNECT(help_resample_menu, "activate", help_resample_callback, NULL);

  help_fft_menu = gtk_menu_item_new_with_label(_("FFT"));
  ml[h_fft_menu] = _("FFT");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_fft_menu);
  gtk_widget_show(help_fft_menu);
  SG_SIGNAL_CONNECT(help_fft_menu, "activate", help_fft_callback, NULL);

  help_filter_menu = gtk_menu_item_new_with_label(_("Filter"));
  ml[h_filter_menu] = _("Filter");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_filter_menu);
  gtk_widget_show(help_filter_menu);
  SG_SIGNAL_CONNECT(help_filter_menu, "activate", help_filter_callback, NULL);

  help_reverb_menu = gtk_menu_item_new_with_label(_("Reverb"));
  ml[h_reverb_menu] = _("Reverb");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_reverb_menu);
  gtk_widget_show(help_reverb_menu);
  SG_SIGNAL_CONNECT(help_reverb_menu, "activate", help_reverb_callback, NULL);

  help_env_menu = gtk_menu_item_new_with_label(_("Envelope"));
  ml[h_env_menu] = _("Envelope");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_env_menu);
  gtk_widget_show(help_env_menu);
  SG_SIGNAL_CONNECT(help_env_menu, "activate", help_env_callback, NULL);

  help_marks_menu = gtk_menu_item_new_with_label(_("Mark"));
  ml[h_marks_menu] = _("Mark");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_marks_menu);
  gtk_widget_show(help_marks_menu);
  SG_SIGNAL_CONNECT(help_marks_menu, "activate", help_marks_callback, NULL);

  help_insert_menu = gtk_menu_item_new_with_label(_("Insert"));
  ml[h_insert_menu] = _("Insert");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_insert_menu);
  gtk_widget_show(help_insert_menu);
  SG_SIGNAL_CONNECT(help_insert_menu, "activate", help_insert_callback, NULL);

  help_delete_menu = gtk_menu_item_new_with_label(_("Delete"));
  ml[h_delete_menu] = _("Delete");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_delete_menu);
  gtk_widget_show(help_delete_menu);
  SG_SIGNAL_CONNECT(help_delete_menu, "activate", help_delete_callback, NULL);

  help_undo_menu = gtk_menu_item_new_with_label(_("Undo and redo"));
  ml[h_undo_menu] = _("Undo and redo");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_undo_menu);
  gtk_widget_show(help_undo_menu);
  SG_SIGNAL_CONNECT(help_undo_menu, "activate", help_undo_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  help_find_menu = gtk_menu_item_new_with_label(_("Search"));
  ml[h_find_menu] = _("Search");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_find_menu);
  gtk_widget_show(help_find_menu);
  SG_SIGNAL_CONNECT(help_find_menu, "activate", help_find_callback, NULL);
#endif

  help_sync_menu = gtk_menu_item_new_with_label(_("Sync and Unite"));
  ml[h_sync_menu] = _("Sync and Unite");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_sync_menu);
  gtk_widget_show(help_sync_menu);
  SG_SIGNAL_CONNECT(help_sync_menu, "activate", help_sync_callback, NULL);

  help_sound_files_menu = gtk_menu_item_new_with_label(_("Headers and Data"));
  ml[h_sound_files_menu] = _("Headers and Data");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_sound_files_menu);
  gtk_widget_show(help_sound_files_menu);
  SG_SIGNAL_CONNECT(help_sound_files_menu, "activate", help_sound_files_callback, NULL);

  help_debug_menu = gtk_menu_item_new_with_label(_("Debugging"));
  ml[h_debug_menu] = _("Debugging");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_debug_menu);
  gtk_widget_show(help_debug_menu);
  SG_SIGNAL_CONNECT(help_debug_menu, "activate", help_debug_callback, NULL);

  help_region_menu = gtk_menu_item_new_with_label(_("Regions"));
  ml[h_region_menu] = _("Regions");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_region_menu);
  gtk_widget_show(help_region_menu);
  SG_SIGNAL_CONNECT(help_region_menu, "activate", help_region_callback, NULL);

  help_selection_menu = gtk_menu_item_new_with_label(_("Selections"));
  ml[h_selection_menu] = _("Selections");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_selection_menu);
  gtk_widget_show(help_selection_menu);
  SG_SIGNAL_CONNECT(help_selection_menu, "activate", help_selection_callback, NULL);

  help_colors_menu = gtk_menu_item_new_with_label(_("Colors"));
  ml[h_colors_menu] = _("Colors");
  gtk_menu_shell_append(GTK_MENU_SHELL(help_cascade_menu), help_colors_menu);
  gtk_widget_show(help_colors_menu);
  SG_SIGNAL_CONNECT(help_colors_menu, "activate", help_colors_callback, NULL);

  SG_SIGNAL_CONNECT(file_menu, "activate", file_menu_update_1, NULL);
  SG_SIGNAL_CONNECT(edit_menu, "activate", edit_menu_update_1, NULL);
  SG_SIGNAL_CONNECT(view_menu, "activate", view_menu_update_1, NULL);
  SG_SIGNAL_CONNECT(options_menu, "activate", options_menu_update_1, NULL);

  return(main_menu);
}

/* -------------------------------- POPUP MENU -------------------------------- */

static GtkWidget *popup_menu = NULL;
static const char *pl[NUM_POPUP_WIDGETS];
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
	  play_sound(sp, 0, NO_END_SPECIFIED);
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
    set_button_label(popup_play_menu, _("Play"));
}

static void popup_save_callback(GtkWidget *w, gpointer info) 
{
  save_edits_with_prompt(any_selected_sound());
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

static void popup_apply_callback(GtkWidget *w, gpointer info) 
{
  menu_apply_controls(any_selected_sound());
  gtk_widget_hide(popup_menu);
}

static void popup_reset_callback(GtkWidget *w, gpointer info) 
{
  menu_reset_controls(any_selected_sound());
  gtk_widget_hide(popup_menu);
}

static void create_popup_menu(guint button, Tempus time)
{
  if (!popup_menu)
    {
      bool undo_possible = false, redo_possible = false;
      chan_info *selcp = NULL;

      ss->sgx->pw = (GtkWidget **)calloc(NUM_POPUP_WIDGETS, sizeof(GtkWidget *));

      selcp = selected_channel();
      if (selcp)
	{
	  undo_possible = (selcp->edit_ctr > 0);
	  redo_possible = ((selcp->edit_size > (selcp->edit_ctr + 1)) && 
			   (selcp->edits[selcp->edit_ctr + 1]));
	}
      popup_menu = gtk_menu_new();
      gtk_widget_show(popup_menu);

      popup_play_menu = gtk_image_menu_item_new_with_label(_("Play"));
      pl[W_pop_play] = _("Play");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_play_menu);
#ifdef GTK_STOCK_MEDIA_PLAY
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(popup_play_menu), gtk_image_new_from_stock(GTK_STOCK_MEDIA_PLAY, GTK_ICON_SIZE_MENU));
#else
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(popup_play_menu), gtk_image_new_from_stock(GTK_STOCK_EXECUTE, GTK_ICON_SIZE_MENU));
#endif
      SG_SIGNAL_CONNECT(popup_play_menu, "activate", popup_play_callback, NULL);
      set_sensitive(popup_play_menu, (ss->active_sounds > 0));
      gtk_widget_show(popup_play_menu);

      popup_undo_menu = gtk_image_menu_item_new_with_label(_("Undo"));
      pl[W_pop_undo] = _("Undo");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_undo_menu);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(popup_undo_menu), gtk_image_new_from_stock(GTK_STOCK_UNDO, GTK_ICON_SIZE_MENU));
      SG_SIGNAL_CONNECT(popup_undo_menu, "activate", popup_undo_callback, NULL);
      set_sensitive(popup_undo_menu, undo_possible);
      gtk_widget_show(popup_undo_menu);
      
      popup_redo_menu = gtk_image_menu_item_new_with_label(_("Redo"));
      pl[W_pop_redo] = _("Redo");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_redo_menu);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(popup_redo_menu), gtk_image_new_from_stock(GTK_STOCK_REDO, GTK_ICON_SIZE_MENU));
      SG_SIGNAL_CONNECT(popup_redo_menu, "activate", popup_redo_callback, NULL);
      set_sensitive(popup_redo_menu, redo_possible);
      gtk_widget_show(popup_redo_menu);
      
      popup_save_menu = gtk_image_menu_item_new_with_label(_("Save"));
      pl[W_pop_save] = _("Save");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_save_menu);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(popup_save_menu), gtk_image_new_from_stock(GTK_STOCK_SAVE, GTK_ICON_SIZE_MENU));
      SG_SIGNAL_CONNECT(popup_save_menu, "activate", popup_save_callback, NULL);
      set_sensitive(popup_save_menu, (ss->active_sounds > 0));
      gtk_widget_show(popup_save_menu);

      popup_info_menu = gtk_image_menu_item_new_with_label(_("Info"));
      pl[W_pop_info] = _("Info");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_info_menu);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(popup_info_menu), gtk_image_new_from_stock(GTK_STOCK_HELP, GTK_ICON_SIZE_MENU));
      SG_SIGNAL_CONNECT(popup_info_menu, "activate", popup_info_callback, NULL);
      set_sensitive(popup_info_menu, (ss->active_sounds > 0));
      gtk_widget_show(popup_info_menu);

      popup_apply_menu = gtk_image_menu_item_new_with_label(_("Apply controls"));
      pl[W_pop_apply] = _("Apply controls");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_apply_menu);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(popup_apply_menu), gtk_image_new_from_stock(GTK_STOCK_APPLY, GTK_ICON_SIZE_MENU));
      SG_SIGNAL_CONNECT(popup_apply_menu, "activate", popup_apply_callback, NULL);
      set_sensitive(popup_apply_menu, true);
      gtk_widget_show(popup_apply_menu);

      popup_reset_menu = gtk_image_menu_item_new_with_label(_("Reset controls"));
      pl[W_pop_reset] = _("Reset controls");
      gtk_menu_shell_append(GTK_MENU_SHELL(popup_menu), popup_reset_menu);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(popup_reset_menu), gtk_image_new_from_stock(GTK_STOCK_REVERT_TO_SAVED, GTK_ICON_SIZE_MENU));
      SG_SIGNAL_CONNECT(popup_reset_menu, "activate", popup_reset_callback, NULL);
      set_sensitive(popup_reset_menu, true);
      gtk_widget_show(popup_reset_menu);

    }
  popup_menu_update();
  gtk_menu_popup(GTK_MENU(popup_menu), NULL, NULL, NULL, NULL, button, time);
}

static XEN gtk_popup_hook;

void popup_menu_from(GtkWidget *w, GdkEventButton *ev, gpointer data, int snd, int chn)
{
  /* set up in snd-gchn.c (graph_button_press) */
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


/* ---------------- tie in menu stuff to extlang ---------------- */

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
    case FILE_MENU:    return(file_menu);           break;
    case EDIT_MENU:    return(edit_menu);           break;
    case VIEW_MENU:    return(view_menu);           break;
    case OPTIONS_MENU: return(options_menu);        break;
    case HELP_MENU:    return(help_menu);           break;
    case POPUP_MENU:   return(popup_menu);              break;
    default:           return(added_menus[which_menu]); break;
    }
  return(NULL);
}

static void add_option(GtkWidget *w, int which_menu, const char *label, int callb)
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

static int remove_option(int which_menu, const char *label)
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
	  gtk_widget_hide(ss->sgx->mw[i]);
	  return(0);
	}
  for (i = 0; i < NUM_POPUP_WIDGETS; i++)
    if (pl[i])
      if (strcmp(label, pl[i]) == 0)
	{
	  gtk_widget_hide(ss->sgx->pw[i]);
	  return(0);
	}
  return(INVALID_MENU);
}

int g_add_to_main_menu(char *label, int slot)
{
  GtkWidget *m, *mc;
  if (new_menu >= MAX_MAIN_MENUS) return(INVALID_MENU);
  m = gtk_menu_item_new_with_label(label);
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), m);
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
  added_menus[new_menu] = m; /* was mc -- 1-Mar-06 */
  return(new_menu);
}

GtkWidget *get_help_menu_widget(void)
{
  return(help_cascade_menu);
}

GtkWidget *g_add_to_menu(int which_menu, const char *label, int callb, int position)
{
  GtkWidget *m, *menw;
   switch (which_menu)
    {
    case FILE_MENU:    menw = file_cascade_menu; break;
    case EDIT_MENU:    menw = edit_cascade_menu; break;
    case VIEW_MENU:    menw = view_cascade_menu; break;
    case OPTIONS_MENU: menw = options_cascade_menu; break;
    case HELP_MENU:    menw = help_cascade_menu; break;
    case POPUP_MENU:   menw = popup_menu; break;
    default: 
      if (which_menu < MAX_MAIN_MENUS)
	menw = gtk_menu_item_get_submenu(GTK_MENU_ITEM(added_menus[which_menu])); 
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

int g_remove_from_menu(int which_menu, const char *label)
{
  return(remove_option(which_menu, label));
}


static XEN g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets "): a list of the top level menu widgets: ((0)main (1)file (2)edit (3)view (4)options (5)help (6)popup)"
  return(XEN_CONS(XEN_WRAP_WIDGET(main_menu),
	  XEN_CONS(XEN_WRAP_WIDGET(file_menu),
           XEN_CONS(XEN_WRAP_WIDGET(edit_menu),
            XEN_CONS(XEN_WRAP_WIDGET(view_menu),
             XEN_CONS(XEN_WRAP_WIDGET(options_menu),
              XEN_CONS(XEN_WRAP_WIDGET(help_menu),
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
  #define H_gtk_popup_hook "gtk-popup-hook (widget event data snd chn): called upon middle button click. \
If it returns other than " PROC_TRUE ", the normal Snd popup menu is posted."

  gtk_popup_hook = XEN_DEFINE_HOOK("gtk-popup-hook", 5, H_gtk_popup_hook);
  XEN_DEFINE_PROCEDURE(S_menu_widgets, g_menu_widgets_w, 0, 0, 0, H_menu_widgets);
}


