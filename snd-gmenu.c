#include "snd.h"
#include "snd-menu.h"


static const char *ml[NUM_MENU_WIDGETS];

void set_menu_label(GtkWidget *w, const char *label) {if (w) set_button_label(w, label);}


/* -------------------------------- FILE MENU -------------------------------- */

static GtkWidget **recent_file_items = NULL;
static int recent_file_items_size = 0;

static char *get_item_label(GtkWidget *w)
{
  return((char *)gtk_label_get_text(GTK_LABEL(gtk_bin_get_child(GTK_BIN(w)))));
}


static void set_item_label(GtkWidget *w, const char *label)
{
  gtk_label_set_text(GTK_LABEL(gtk_bin_get_child(GTK_BIN(w))), label);
}


static void open_recent_file_callback(GtkWidget *w, gpointer info)
{
  char *filename;
  snd_info *sp;
  filename = get_item_label(w);
  ss->open_requestor = FROM_OPEN_RECENT_MENU;
  ss->open_requestor_data = NULL;
  sp = snd_open_file(filename, FILE_READ_WRITE);
  if (sp) select_channel(sp, 0);
}


static void file_open_recent_callback(GtkWidget *w, gpointer info)
{
  int size;
  size = recent_files_size();
  if (size > 0)
    {
      int i;
      char **recent_file_names;

      if (size > recent_file_items_size)
	{
	  if (recent_file_items_size == 0)
	    recent_file_items = (GtkWidget **)calloc(size, sizeof(GtkWidget *));
	  else
	    {
	      recent_file_items = (GtkWidget **)realloc(recent_file_items, size * sizeof(GtkWidget *));
	      for (i = recent_file_items_size; i < size; i++)
		recent_file_items[i] = NULL;
	    }
	  recent_file_items_size = size;
	}

      recent_file_names = recent_files();

      for (i = 0; i < size; i++)
	{
	  if (recent_file_items[i] == NULL)
	    {
	      recent_file_items[i] = gtk_menu_item_new_with_label(recent_file_names[i]);
	      gtk_menu_shell_append(GTK_MENU_SHELL(file_open_recent_cascade_menu), recent_file_items[i]);
	      gtk_widget_show(recent_file_items[i]);
	      SG_SIGNAL_CONNECT(recent_file_items[i], "activate", open_recent_file_callback, NULL);
	    }
	  else
	    {
	      set_item_label(recent_file_items[i], recent_file_names[i]);
	      gtk_widget_show(recent_file_items[i]);
	    }
	}

      for (i = size; i < recent_file_items_size; i++) /* maybe previous file was deleted */
	if (recent_file_items[i])
	  gtk_widget_hide(recent_file_items[i]);
    }
}


static void file_menu_update_1(GtkWidget *w, gpointer info)
{
  if (recent_files_size() > 0)
    gtk_widget_show(file_open_recent_menu);
  else gtk_widget_hide(file_open_recent_menu);
  file_menu_update();
}


static void file_open_callback(GtkWidget *w, gpointer info) {make_open_file_dialog(FILE_READ_WRITE, true);}
static void file_view_callback(GtkWidget *w, gpointer info) {make_open_file_dialog(FILE_READ_ONLY, true);}
static void file_new_callback(GtkWidget *w, gpointer info) {make_new_file_dialog(true);}
static void file_record_callback(GtkWidget *w, gpointer info) {record_file();}
static void file_close_callback(GtkWidget *w, gpointer info) {if (any_selected_sound()) snd_close_file(any_selected_sound());}
static void file_close_all_callback(GtkWidget *w, gpointer info) {for_each_sound(snd_close_file);}
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
static void edit_unselect_callback(GtkWidget *w, gpointer info) {deactivate_selection();}
static void edit_undo_callback(GtkWidget *w, gpointer info) {undo_edit_with_sync(current_channel(), 1);}
static void edit_redo_callback(GtkWidget *w, gpointer info) {redo_edit_with_sync(current_channel(), 1);}


static void edit_play_callback(GtkWidget *w, gpointer info) 
{
  if (ss->selection_play_stop)
    {
      stop_playing_all_sounds(PLAY_BUTTON_UNSET);
      reflect_play_selection_stop();
    }
  else
    {
      set_menu_label(edit_play_menu, "Stop");
      ss->selection_play_stop = true;
      play_selection(IN_BACKGROUND);
    }
}


void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu, "Play Selection");
  ss->selection_play_stop = false;
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

static GtkWidget **view_files_items = NULL, *view_files_cascade_menu = NULL;
static int view_files_items_size = 0;

static void view_files_item_callback(GtkWidget *w, gpointer info)
{
  char *dirname;
  dirname = get_item_label(w);
  if (mus_strcmp(dirname, "new viewer"))
    make_view_files_dialog(true, true); /* managed and empty (brand-new) */
  else view_files_start_dialog_with_title(dirname);
}


static void view_files_callback(GtkWidget *w, gpointer info)
{
  int size;

  size = view_files_dialog_list_length();
  if (size == 0)
    make_view_files_dialog(true, true); /* managed and empty (brand-new) */
  else
    {
      int i;
      char **view_files_names;

      view_files_names = view_files_dialog_titles();
      view_files_names[size++] = mus_strdup("new viewer");

      if (size > view_files_items_size)
	{
	  if (view_files_items_size == 0)
	    view_files_items = (GtkWidget **)calloc(size, sizeof(GtkWidget *));
	  else
	    {
	      view_files_items = (GtkWidget **)realloc(view_files_items, size * sizeof(GtkWidget *));
	      for (i = view_files_items_size; i < size; i++)
		view_files_items[i] = NULL;
	    }
	  view_files_items_size = size;
	}

      for (i = 0; i < size; i++)
	{
	  if (view_files_items[i] == NULL)
	    {
	      view_files_items[i] = gtk_menu_item_new_with_label(view_files_names[i]);
	      gtk_menu_shell_append(GTK_MENU_SHELL(view_files_cascade_menu), view_files_items[i]);
	      gtk_widget_show(view_files_items[i]);
	      SG_SIGNAL_CONNECT(view_files_items[i], "activate", view_files_item_callback, NULL);
	    }
	  else
	    {
	      set_item_label(view_files_items[i], view_files_names[i]);
	      gtk_widget_show(view_files_items[i]);
	    }
	  free(view_files_names[i]);
	}
      free(view_files_names);
    }
}


static void view_menu_update_1(GtkWidget *w, gpointer info)
{
  if ((view_files_dialog_list_length() > 0) &&
      (!view_files_cascade_menu))
    {
      view_files_cascade_menu = gtk_menu_new();
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_files_menu), view_files_cascade_menu);
    }
    
  view_menu_update();
}


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
static void view_controls_callback(GtkWidget *w, gpointer info) {set_show_controls(!in_show_controls(ss));}

#if HAVE_EXTENSION_LANGUAGE
static void view_inset_callback(GtkWidget *w, gpointer info) 
{
  set_with_inset_graph((!(with_inset_graph(ss))));
  for_each_chan(update_graph);
}
#endif

#if HAVE_EXTENSION_LANGUAGE
static void view_listener_callback(GtkWidget *w, gpointer info) {handle_listener(!(listener_is_visible()));}
#endif

static void view_mix_dialog_callback(GtkWidget *w, gpointer info) {make_mix_dialog();}
static void view_region_callback_1(GtkWidget *w, gpointer info) {view_region_callback(w, info);}
static void view_color_orientation_callback_1(GtkWidget *w, gpointer info) {view_color_orientation_callback(w, info);}

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

static void view_focus_right_callback(GtkWidget *w, gpointer info, gpointer data) {set_zoom_focus_style(ZOOM_FOCUS_RIGHT);}
static void view_focus_left_callback(GtkWidget *w, gpointer info, gpointer data) {set_zoom_focus_style(ZOOM_FOCUS_LEFT);}
static void view_focus_middle_callback(GtkWidget *w, gpointer info, gpointer data) {set_zoom_focus_style(ZOOM_FOCUS_MIDDLE);}
static void view_focus_active_callback(GtkWidget *w, gpointer info, gpointer data) {set_zoom_focus_style(ZOOM_FOCUS_ACTIVE);}


/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_transform_callback(GtkWidget *w, gpointer info) {make_transform_dialog(true);}
static void options_controls_callback(GtkWidget *w, gpointer info) {make_controls_dialog();}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(GtkWidget *w, gpointer info) {save_options_from_menu();}
#endif

#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(GtkWidget *w, gpointer info) {save_state_from_menu();}
#endif

static void options_preferences_callback(GtkWidget *w, gpointer info, gpointer data) {make_preferences_dialog();}


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
	  free(new_title);
	}
      break;
    case DRAG_LEAVE:
      reflect_file_change_in_title();
      have_drag_title = false;
      break;
    }
}



/* -------------------------------- MAIN MENU -------------------------------- */

static GtkWidget *add_menu_item(GtkWidget *menu, const char *label, const char *icon, GCallback callback)
{
  GtkWidget *w;
  if (icon)
    w = gtk_image_menu_item_new_with_label(label);
  else w = gtk_menu_item_new_with_label(label);
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), w);
  if (icon)
    gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(w), gtk_image_new_from_stock(icon, GTK_ICON_SIZE_MENU));
  if (callback)
    SG_SIGNAL_CONNECT(w, "activate", callback, NULL);
  gtk_widget_show(w);
  return(w);
}


static GtkWidget *add_insensitive_menu_item(GtkWidget *menu, const char *label, const char *icon, GCallback callback)
{
  GtkWidget *w;
  w = add_menu_item(menu, label, icon, callback);
  set_sensitive(w, false);
  return(w);
}


static GtkWidget *add_menu_separator(GtkWidget *menu)
{
  GtkWidget *w;
  w = gtk_menu_item_new();
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), w);
  gtk_widget_show(w);
  widget_modify_bg(w, GTK_STATE_NORMAL, ss->black);
  return(w);
}


#define WITH_MENU_ACCELERATORS false

GtkWidget *add_menu(void)
{
#if WITH_MENU_ACCELERATORS
  /* these take precedence over everything, even in the listener? 
   *   also what are MOD1...5? MOD1->meta MOD2->?? SUPER->"windoze key" and HYPER also
   * but these look stupid -- maybe add with-menu-accelerators but then add_menu has to be smarter (not at startup)
   *   and we need a way to unaccellerate if set to #f
   */
  GtkAccelGroup *accel_group;
  accel_group = gtk_accel_group_new();
  gtk_window_add_accel_group(GTK_WINDOW(MAIN_SHELL(ss)), accel_group);
#endif

  ss->mw = (GtkWidget **)calloc(NUM_MENU_WIDGETS, sizeof(GtkWidget *));

  main_menu = gtk_menu_bar_new();
  ml[m_menu] = NULL;
  add_drag_and_drop(main_menu, menu_drop_watcher, menu_drag_watcher, NULL);
  gtk_box_pack_start(GTK_BOX(MAIN_PANE(ss)), main_menu, false, true, 0);
  gtk_widget_show(main_menu);


  /* -------- FILE MENU -------- */

  file_menu = gtk_menu_item_new_with_label("File");
  ml[f_menu] = "File";
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), file_menu);
  gtk_widget_show(file_menu);

  file_cascade_menu = gtk_menu_new();
  ml[f_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_menu), file_cascade_menu);

  file_open_menu = add_menu_item(file_cascade_menu, "Open", GTK_STOCK_OPEN, (GCallback)file_open_callback);
  ml[f_open_menu] = "Open";
#if WITH_MENU_ACCELERATORS
  gtk_widget_add_accelerator (file_open_menu, "activate", accel_group, GDK_O, GDK_HYPER_MASK | GDK_SHIFT_MASK, GTK_ACCEL_VISIBLE);
#endif

  file_open_recent_menu = add_menu_item(file_cascade_menu, "Open recent", GTK_STOCK_OPEN, (GCallback)file_open_recent_callback);
  ml[f_open_recent_menu] = "Open recent";
  gtk_widget_hide(file_open_recent_menu);
  
  file_open_recent_cascade_menu = gtk_menu_new();
  ml[f_open_recent_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_open_recent_menu), file_open_recent_cascade_menu);

  file_close_menu = add_insensitive_menu_item(file_cascade_menu, "Close", GTK_STOCK_CLOSE, (GCallback)file_close_callback);
  ml[f_close_menu] = "Close";
  
  file_close_all_menu = add_menu_item(file_cascade_menu, "Close all", GTK_STOCK_CLOSE, (GCallback)file_close_all_callback);
  ml[f_close_all_menu] = "Close all";
  gtk_widget_hide(file_close_all_menu);
  
  file_save_menu = add_insensitive_menu_item(file_cascade_menu, "Save", GTK_STOCK_SAVE, (GCallback)file_save_callback);
  ml[f_save_menu] = "Save";
  
  file_save_as_menu = add_insensitive_menu_item(file_cascade_menu, "Save as", GTK_STOCK_SAVE_AS, (GCallback)file_save_as_callback);
  ml[f_save_as_menu] = "Save as";
  
  file_revert_menu = add_insensitive_menu_item(file_cascade_menu, "Revert", GTK_STOCK_REVERT_TO_SAVED, (GCallback)file_revert_callback);
  ml[f_revert_menu] = "Revert";
  
  file_mix_menu = add_insensitive_menu_item(file_cascade_menu, "Mix", GTK_STOCK_ADD, (GCallback)file_mix_callback_1);
  ml[f_mix_menu] = "Mix";

  file_insert_menu = add_insensitive_menu_item(file_cascade_menu, "Insert", GTK_STOCK_PASTE, (GCallback)file_insert_callback_1);
  ml[f_insert_menu] = "Insert";

  file_update_menu = add_insensitive_menu_item(file_cascade_menu, "Update", GTK_STOCK_REFRESH, (GCallback)file_update_callback);
  ml[f_update_menu] = "Update";

  file_new_menu = add_menu_item(file_cascade_menu, "New", GTK_STOCK_NEW, (GCallback)file_new_callback);
  ml[f_new_menu] = "New";

  file_record_menu = add_menu_item(file_cascade_menu, "Record", GTK_STOCK_MEDIA_RECORD, (GCallback)file_record_callback);
  ml[f_record_menu] = "Record";

  file_view_menu = add_menu_item(file_cascade_menu, "View", GTK_STOCK_OPEN, (GCallback)file_view_callback);
  ml[f_view_menu] = "View";

  file_print_menu = add_insensitive_menu_item(file_cascade_menu, "Print", GTK_STOCK_PRINT, (GCallback)file_print_callback_1);
  ml[f_print_menu] = "Print";

  file_sep_menu = add_menu_separator(file_cascade_menu);
  ml[f_sep_menu] = NULL;

  file_exit_menu = add_menu_item(file_cascade_menu, "Exit", GTK_STOCK_QUIT, (GCallback)file_exit_callback);
  ml[f_exit_menu] = "Exit";


  /* -------- EDIT MENU -------- */

  edit_menu = gtk_menu_item_new_with_label("Edit");
  ml[e_menu] = "Edit";
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), edit_menu);
  gtk_widget_show(edit_menu);

  edit_cascade_menu = gtk_menu_new();
  ml[e_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(edit_menu), edit_cascade_menu);
  
  edit_undo_menu = add_insensitive_menu_item(edit_cascade_menu, "Undo", GTK_STOCK_UNDO, (GCallback)edit_undo_callback);
  ml[e_undo_menu] = "Undo";

  edit_redo_menu = add_insensitive_menu_item(edit_cascade_menu, "Redo", GTK_STOCK_REDO, (GCallback)edit_redo_callback);
  ml[e_redo_menu] = "Redo";

#if HAVE_EXTENSION_LANGUAGE
  edit_find_menu = add_insensitive_menu_item(edit_cascade_menu, "Find", GTK_STOCK_FIND, (GCallback)edit_find_callback_1);
  ml[e_find_menu] = "Find";
#endif

  edit_select_sep_menu = add_menu_separator(edit_cascade_menu);
  ml[e_select_sep_menu] = NULL;

  edit_cut_menu = add_insensitive_menu_item(edit_cascade_menu, "Delete selection", GTK_STOCK_CUT, (GCallback)edit_cut_callback);
  ml[e_cut_menu] = "Delete Selection";

  edit_paste_menu = add_insensitive_menu_item(edit_cascade_menu, "Insert selection", GTK_STOCK_PASTE, (GCallback)edit_paste_callback);
  ml[e_paste_menu] = "Insert Selection";

  edit_mix_menu = add_insensitive_menu_item(edit_cascade_menu, "Mix selection", GTK_STOCK_ADD, (GCallback)edit_mix_callback);
  ml[e_mix_menu] = "Mix Selection";

  edit_play_menu = add_insensitive_menu_item(edit_cascade_menu, "Play selection", GTK_STOCK_MEDIA_PLAY, (GCallback)edit_play_callback);
  ml[e_play_menu] = "Play Selection";

  edit_save_as_menu = add_insensitive_menu_item(edit_cascade_menu, "Save selection", GTK_STOCK_SAVE_AS, (GCallback)edit_save_as_callback);
  ml[e_save_as_menu] = "Save Selection";

  edit_select_all_menu = add_insensitive_menu_item(edit_cascade_menu, "Select all", GTK_STOCK_SELECT_ALL, (GCallback)edit_select_all_callback);
  ml[e_select_all_menu] = "Select all";

  edit_unselect_menu = add_insensitive_menu_item(edit_cascade_menu, "Unselect all", NULL, (GCallback)edit_unselect_callback);
  ml[e_unselect_menu] = "Unselect all";

  edit_edit_sep_menu = add_menu_separator(edit_cascade_menu);
  ml[e_edit_sep_menu] = NULL;

  edit_env_menu = add_menu_item(edit_cascade_menu, "Edit envelope", NULL, (GCallback)edit_envelope_callback);
  ml[e_env_menu] = "Edit Envelope";

  edit_header_menu = add_insensitive_menu_item(edit_cascade_menu, "Edit header", GTK_STOCK_EDIT, (GCallback)edit_header_callback_1);
  ml[e_header_menu] = "Edit Header";


  /* -------- VIEW MENU -------- */

  view_menu = gtk_menu_item_new_with_label("View");
  ml[v_menu] = "View";
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), view_menu);
  gtk_widget_show(view_menu);

  view_cascade_menu = gtk_menu_new();
  ml[v_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_menu), view_cascade_menu);

  view_controls_menu = add_menu_item(view_cascade_menu, "Show controls", NULL, (GCallback)view_controls_callback);
  ml[v_controls_menu] = "Show controls";

#if HAVE_EXTENSION_LANGUAGE
  view_listener_menu = add_menu_item(view_cascade_menu, "Open listener", NULL, (GCallback)view_listener_callback);
  ml[v_listener_menu] = "Open listener";
#endif

  view_files_menu = add_menu_item(view_cascade_menu, "Files", NULL, (GCallback)view_files_callback);
  ml[v_files_menu] = "Files";

  view_mix_dialog_menu = add_menu_item(view_cascade_menu, "Mixes", NULL, (GCallback)view_mix_dialog_callback);
  ml[v_mix_dialog_menu] = "Mixes";

  view_region_menu = add_insensitive_menu_item(view_cascade_menu, "Regions", NULL, (GCallback)view_region_callback_1);
  ml[v_region_menu] = "Regions";

  view_color_orientation_menu = add_menu_item(view_cascade_menu, "Color/Orientation", NULL, (GCallback)view_color_orientation_callback_1);
  ml[v_color_orientation_menu] = "Color/Orientation";

  view_sep2_menu = add_menu_separator(view_cascade_menu);
  ml[v_sep2_menu] = NULL;


  view_graph_style_menu = add_menu_item(view_cascade_menu, "Graph style", NULL, NULL);
  ml[v_graph_style_menu] = "Graph style";

  view_graph_style_cascade_menu = gtk_menu_new();
  ml[v_graph_style_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_graph_style_menu), view_graph_style_cascade_menu);

  view_lines_menu = add_menu_item(view_graph_style_cascade_menu, "lines", NULL, (GCallback)view_lines_callback);
  ml[v_lines_menu] = "lines";
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(view_lines_menu, false);

  view_dots_menu = add_menu_item(view_graph_style_cascade_menu, "dots", NULL, (GCallback)view_dots_callback);
  ml[v_dots_menu] = "dots";
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(view_dots_menu, false);

  view_filled_menu = add_menu_item(view_graph_style_cascade_menu, "filled", NULL, (GCallback)view_filled_callback);
  ml[v_filled_menu] = "filled";
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(view_filled_menu, false);

  view_dots_and_lines_menu = add_menu_item(view_graph_style_cascade_menu, "dots and lines", NULL, (GCallback)view_dots_and_lines_callback);
  ml[v_dots_and_lines_menu] = "dots and lines";
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(view_dots_and_lines_menu, false);

  view_lollipops_menu = add_menu_item(view_graph_style_cascade_menu, "lollipops", NULL, (GCallback)view_lollipops_callback);
  ml[v_lollipops_menu] = "lollipops";
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(view_lollipops_menu, false);


  view_cursor_menu = add_menu_item(view_cascade_menu, "Verbose cursor", NULL, (GCallback)view_cursor_callback);
  ml[v_cursor_menu] = "Verbose cursor";

#if HAVE_EXTENSION_LANGUAGE
  view_inset_menu = add_menu_item(view_cascade_menu, "With inset graph", NULL, (GCallback)view_inset_callback);
  ml[v_inset_menu] = "With inset graph";
#endif


  view_combine_menu = add_menu_item(view_cascade_menu, "Channel style", NULL, NULL);
  ml[v_combine_menu] = "Channel style";

  view_combine_cascade_menu = gtk_menu_new();
  ml[v_combine_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_combine_menu), view_combine_cascade_menu);

  view_combine_separate_menu = add_menu_item(view_combine_cascade_menu, "separate", NULL, (GCallback)view_separate_callback);
  ml[v_combine_separate_menu] = "separate";
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(view_combine_separate_menu, false);

  view_combine_combined_menu = add_menu_item(view_combine_cascade_menu, "combined", NULL, (GCallback)view_combined_callback);
  ml[v_combine_combined_menu] = "combined";
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(view_combine_combined_menu, false);

  view_combine_superimposed_menu = add_menu_item(view_combine_cascade_menu, "superimposed", NULL, (GCallback)view_superimposed_callback);
  ml[v_combine_superimposed_menu] = "superimposed";
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(view_combine_superimposed_menu, false);

  view_zero_menu = add_menu_item(view_cascade_menu, "Show y = 0", NULL, (GCallback)view_zero_callback);
  ml[v_zero_menu] = "Show y = 0";


  view_x_axis_menu = add_menu_item(view_cascade_menu, "X axis units", NULL, NULL);
  ml[v_x_axis_menu] = "X axis units";

  view_x_axis_cascade_menu = gtk_menu_new();
  ml[v_x_axis_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_x_axis_menu), view_x_axis_cascade_menu);

  view_x_axis_seconds_menu = add_insensitive_menu_item(view_x_axis_cascade_menu, "seconds", NULL, (GCallback)view_x_axis_seconds_callback);
  ml[v_x_axis_seconds_menu] = "seconds";

  view_x_axis_samples_menu = add_menu_item(view_x_axis_cascade_menu, "samples", NULL, (GCallback)view_x_axis_samples_callback);
  ml[v_x_axis_samples_menu] = "samples";

  view_x_axis_clock_menu = add_menu_item(view_x_axis_cascade_menu, "clock", NULL, (GCallback)view_x_axis_clock_callback);
  ml[v_x_axis_clock_menu] = "clock";

  view_x_axis_percentage_menu = add_menu_item(view_x_axis_cascade_menu, "percentage", NULL, (GCallback)view_x_axis_percentage_callback);
  ml[v_x_axis_percentage_menu] = "percentage";

  view_x_axis_beats_menu = add_menu_item(view_x_axis_cascade_menu, "beats", NULL, (GCallback)view_x_axis_beats_callback);
  ml[v_x_axis_beats_menu] = "beats";

  view_x_axis_measures_menu = add_menu_item(view_x_axis_cascade_menu, "measures", NULL, (GCallback)view_x_axis_measures_callback);
  ml[v_x_axis_measures_menu] = "measures";


  view_axes_menu = add_menu_item(view_cascade_menu, "Axes", NULL, NULL);
  ml[v_axes_menu] = "Axes";

  view_axes_cascade_menu = gtk_menu_new();
  ml[v_axes_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_axes_menu), view_axes_cascade_menu);

  view_no_axes_menu = add_menu_item(view_axes_cascade_menu, "no axes", NULL, (GCallback)view_no_axes_callback);
  ml[v_no_axes_menu] = "no axes";

  view_all_axes_menu = add_menu_item(view_axes_cascade_menu, "both axes", NULL, (GCallback)view_all_axes_callback);
  ml[v_all_axes_menu] = "both axes";

  view_just_x_axis_menu = add_menu_item(view_axes_cascade_menu, "just x axis", NULL, (GCallback)view_just_x_axis_callback);
  ml[v_just_x_axis_menu] = "just x axis";

  view_all_axes_unlabelled_menu = add_menu_item(view_axes_cascade_menu, "both axes, no labels", NULL, (GCallback)view_all_axes_unlabelled_callback);
  ml[v_all_axes_unlabelled_menu] = "both axes, no labels";

  view_just_x_axis_unlabelled_menu = add_menu_item(view_axes_cascade_menu, "just x axis, no label", NULL, (GCallback)view_just_x_axis_unlabelled_callback);
  ml[v_just_x_axis_unlabelled_menu] = "just x axis, no label";

  view_bare_x_axis_menu = add_menu_item(view_axes_cascade_menu, "bare x axis", NULL, (GCallback)view_bare_x_axis_callback);
  ml[v_bare_x_axis_menu] = "base x axis";


  view_focus_style_menu = add_menu_item(view_cascade_menu, "Zoom focus", NULL, NULL);
  ml[v_focus_style_menu] = "Zoom focus";

  view_focus_cascade_menu = gtk_menu_new();
  ml[v_focus_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(view_focus_style_menu), view_focus_cascade_menu);

  view_focus_left_menu = add_menu_item(view_focus_cascade_menu, "window left edge", NULL, (GCallback)view_focus_left_callback);
  ml[v_focus_left_menu] = "window left edge";

  view_focus_right_menu = add_menu_item(view_focus_cascade_menu, "window right edge", NULL, (GCallback)view_focus_right_callback);
  ml[v_focus_right_menu] = "window right edge";

  view_focus_middle_menu = add_menu_item(view_focus_cascade_menu, "window midpoint", NULL, (GCallback)view_focus_middle_callback);
  ml[v_focus_middle_menu] = "window midpoint";

  view_focus_active_menu = add_menu_item(view_focus_cascade_menu, "cursor or selection", NULL, (GCallback)view_focus_active_callback);
  ml[v_focus_active_menu] = "cursor or selection";



  /* -------- OPTIONS MENU -------- */

  options_menu = gtk_menu_item_new_with_label("Options");
  ml[o_menu] = "Options";
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), options_menu);
  gtk_widget_show(options_menu);

  options_cascade_menu = gtk_menu_new();
  ml[o_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(options_menu), options_cascade_menu);

  options_transform_menu = add_menu_item(options_cascade_menu, "Transform options", NULL, (GCallback)options_transform_callback);
  ml[o_transform_menu] = "Transform Options";

  options_controls_menu = add_menu_item(options_cascade_menu, "Controls", NULL, (GCallback)options_controls_callback);
  ml[o_controls_menu] = "Controls";

#if HAVE_EXTENSION_LANGUAGE
  options_save_menu = add_menu_item(options_cascade_menu, "Save options", NULL, (GCallback)options_save_callback);
  ml[o_save_menu] = "Save options";

  options_save_state_menu = add_menu_item(options_cascade_menu, "Save session", NULL, (GCallback)options_save_state_callback);
  ml[o_save_state_menu] = "Save session";
#endif

  add_menu_separator(options_cascade_menu);
  ml[o_sep_menu] = NULL;

  options_preferences_menu = add_menu_item(options_cascade_menu, "Preferences", NULL, (GCallback)options_preferences_callback);
  ml[o_preferences_menu] = "Preferences";



  /* -------- HELP MENU -------- */

  help_menu = gtk_menu_item_new_with_label("Help");
  ml[h_menu] = "Help";
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), help_menu);
  gtk_widget_show(help_menu);
  /* this is no longer supported in Gtk -- they say "use gtk_widget_set_hexpand() and gtk_widget_set_halign()" */
  /* gtk_menu_item_set_right_justified(GTK_MENU_ITEM(help_menu), true); */

  help_cascade_menu = gtk_menu_new();
  ml[h_cascade_menu] = NULL;
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(help_menu), help_cascade_menu);

  help_about_snd_menu = add_menu_item(help_cascade_menu, "About Snd", NULL, (GCallback)help_about_snd_callback);
  ml[h_about_snd_menu] = "About Snd";

#if HAVE_EXTENSION_LANGUAGE
  help_init_file_menu = add_menu_item(help_cascade_menu, "Customization", NULL, (GCallback)help_init_file_callback);
  ml[h_init_file_menu] = "Customization";
#endif

  help_controls_menu = add_menu_item(help_cascade_menu, "Control panel", NULL, (GCallback)help_controls_callback);
  ml[h_controls_menu] = "Control Panel";

  help_keys_menu = add_menu_item(help_cascade_menu, "Key bindings", NULL, (GCallback)help_keys_callback);
  ml[h_keys_menu] = "Key bindings";

  help_recording_menu = add_menu_item(help_cascade_menu, "Record", NULL, (GCallback)help_recording_callback);
  ml[h_recording_menu] = "Record";

  help_play_menu = add_menu_item(help_cascade_menu, "Play", NULL, (GCallback)help_play_callback);
  ml[h_play_menu] = "Play";

  help_save_menu = add_menu_item(help_cascade_menu, "Save", NULL, (GCallback)help_save_callback);
  ml[h_save_menu] = "Save";

  help_mix_menu = add_menu_item(help_cascade_menu, "Mix", NULL, (GCallback)help_mix_callback);
  ml[h_mix_menu] = "Mix";

  help_resample_menu = add_menu_item(help_cascade_menu, "Resample", NULL, (GCallback)help_resample_callback);
  ml[h_resample_menu] = "Resample";

  help_fft_menu = add_menu_item(help_cascade_menu, "FFT", NULL, (GCallback)help_fft_callback);
  ml[h_fft_menu] = "FFT";

  help_filter_menu = add_menu_item(help_cascade_menu, "Filter", NULL, (GCallback)help_filter_callback);
  ml[h_filter_menu] = "Filter";

  help_reverb_menu = add_menu_item(help_cascade_menu, "Reverb", NULL, (GCallback)help_reverb_callback);
  ml[h_reverb_menu] = "Reverb";

  help_env_menu = add_menu_item(help_cascade_menu, "Envelope", NULL, (GCallback)help_env_callback);
  ml[h_env_menu] = "Envelope";

  help_marks_menu = add_menu_item(help_cascade_menu, "Marks", NULL, (GCallback)help_marks_callback);
  ml[h_marks_menu] = "Marks";

  help_insert_menu = add_menu_item(help_cascade_menu, "Insert", NULL, (GCallback)help_insert_callback);
  ml[h_insert_menu] = "Insert";

  help_delete_menu = add_menu_item(help_cascade_menu, "Delete", NULL, (GCallback)help_delete_callback);
  ml[h_delete_menu] = "Delete";

  help_undo_menu = add_menu_item(help_cascade_menu, "Undo and redo", NULL, (GCallback)help_undo_callback);
  ml[h_undo_menu] = "Undo and redo";

#if HAVE_EXTENSION_LANGUAGE
  help_find_menu = add_menu_item(help_cascade_menu, "Find", NULL, (GCallback)help_find_callback);
  ml[h_find_menu] = "Find";
#endif

  help_sync_menu = add_menu_item(help_cascade_menu, "Sync and unite", NULL, (GCallback)help_sync_callback);
  ml[h_sync_menu] = "Sync and unite";

  help_sound_files_menu = add_menu_item(help_cascade_menu, "Headers and data", NULL, (GCallback)help_sound_files_callback);
  ml[h_sound_files_menu] = "Headers and data";

  help_debug_menu = add_menu_item(help_cascade_menu, "Debugging", NULL, (GCallback)help_debug_callback);
  ml[h_debug_menu] = "Debugging";

  help_region_menu = add_menu_item(help_cascade_menu, "Regions", NULL, (GCallback)help_region_callback);
  ml[h_region_menu] = "Regions";

  help_selection_menu = add_menu_item(help_cascade_menu, "Selection", NULL, (GCallback)help_selection_callback);
  ml[h_selection_menu] = "Selection";

  help_colors_menu = add_menu_item(help_cascade_menu, "Colors", NULL, (GCallback)help_colors_callback);
  ml[h_colors_menu] = "Colors";

  SG_SIGNAL_CONNECT(file_menu, "activate", file_menu_update_1, NULL);
  SG_SIGNAL_CONNECT(edit_menu, "activate", edit_menu_update_1, NULL);
  SG_SIGNAL_CONNECT(view_menu, "activate", view_menu_update_1, NULL);

  return(main_menu);
}


/* -------------------------------- POPUP MENUS -------------------------------- */

static GtkWidget *basic_popup_menu = NULL, *selection_popup_menu = NULL;


/* -------- basic popup -------- */

static void popup_info_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) display_info(sp);
}

static gboolean popup_menu_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  gtk_widget_hide(w);
  return(false);
}

static void popup_normalize_callback(GtkWidget *w, gpointer info) 
{
  mus_float_t scl[1];
  scl[0] = 1.0;
  scale_to(any_selected_sound(), current_channel(), scl, 1, OVER_SOUND);
}


static void popup_apply_controls_callback(GtkWidget *w, gpointer info) 
{
  menu_apply_controls(any_selected_sound());
}


static void popup_reset_controls_callback(GtkWidget *w, gpointer info) 
{
  menu_reset_controls(any_selected_sound());
}

static void popup_reverse_callback(GtkWidget *w, gpointer info)
{
  reverse_sound(current_channel(), OVER_SOUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
}


static void stop_everything_callback(GtkWidget *w, gpointer info)
{
  control_g(any_selected_sound());
}



void post_basic_popup_menu(void *e)
{
  GdkEventButton *ev = (GdkEventButton *)e;
  if (!basic_popup_menu)
    {
      basic_popup_menu = gtk_menu_new();
      gtk_widget_set_events(basic_popup_menu, GDK_ALL_EVENTS_MASK);
      gtk_widget_show(basic_popup_menu);

      SG_SIGNAL_CONNECT(basic_popup_menu, "button_release_event", popup_menu_button_release, NULL);

      add_menu_item(basic_popup_menu, "Info",           NULL, (GCallback)popup_info_callback);
      add_menu_item(basic_popup_menu, "Select all",     NULL, (GCallback)edit_select_all_callback);
      add_menu_item(basic_popup_menu, "Stop!",          NULL, (GCallback)stop_everything_callback);
      add_menu_item(basic_popup_menu, "-> 1.0",         NULL, (GCallback)popup_normalize_callback);
      add_menu_item(basic_popup_menu, "Reverse",        NULL, (GCallback)popup_reverse_callback);
      add_menu_item(basic_popup_menu, "Apply controls", NULL, (GCallback)popup_apply_controls_callback);
      add_menu_item(basic_popup_menu, "Reset controls", NULL, (GCallback)popup_reset_controls_callback);
    }

  gtk_menu_popup(GTK_MENU(basic_popup_menu), NULL, NULL, NULL, NULL, POPUP_BUTTON, EVENT_TIME(ev));
}


/* -------- selection popup -------- */

static void popup_show_selection_callback(GtkWidget *w, gpointer info) 
{
  show_selection();
}

static void popup_zero_selection_callback(GtkWidget *w, gpointer info) 
{
  mus_float_t scl[1];
  scl[0] = 0.0;
  scale_by(NULL, scl, 1, OVER_SELECTION);
}

static void popup_normalize_selection_callback(GtkWidget *w, gpointer info) 
{
  mus_float_t scl[1];
  scl[0] = 1.0;
  scale_to(NULL, NULL, scl, 1, OVER_SELECTION);
}

static void popup_error_handler(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  report_in_minibuffer(any_selected_sound(), "%s: %s", (char *)data, msg);
}

static void popup_cut_to_new_callback_1(bool cut) 
{
  char *temp_file;
  io_error_t io_err = IO_NO_ERROR;

  temp_file = snd_tempnam();
  io_err = save_selection(temp_file, default_output_header_type(ss), default_output_data_format(ss), selection_srate(), NULL, SAVE_ALL_CHANS);
  if (io_err == IO_NO_ERROR)
    {
      if (cut) delete_selection(UPDATE_DISPLAY);

      ss->open_requestor = FROM_POPUP_CUT_TO_NEW;
      redirect_snd_error_to(popup_error_handler, (void *)"popup cut->new");
      snd_open_file(temp_file, FILE_READ_WRITE);
      redirect_snd_error_to(NULL, NULL);

      free(temp_file);
    }
}

static void popup_cut_to_new_callback(GtkWidget *w, gpointer info) {popup_cut_to_new_callback_1(true);}
static void popup_copy_to_new_callback(GtkWidget *w, gpointer info) {popup_cut_to_new_callback_1(false);}

static void crop(chan_info *cp)
{
  if (selection_is_active_in_channel(cp))
    {
      mus_long_t beg, end, frames;
      frames = CURRENT_SAMPLES(cp);
      beg = selection_beg(cp);
      end = selection_end(cp);
      if (beg > 0)
	delete_samples(0, beg, cp, cp->edit_ctr);
      if (end < (frames - 1))
	delete_samples(end + 1, frames - end, cp, cp->edit_ctr);
    }
}

static void popup_crop_callback(GtkWidget *w, gpointer info)
{
  for_each_chan(crop);
}


static void popup_cut_and_smooth_callback(GtkWidget *w, gpointer info)
{
  for_each_chan(cut_and_smooth);
}

static void mark_selection(chan_info *cp)
{
  if (selection_is_active_in_channel(cp))
    {
      add_mark(selection_beg(cp), NULL, cp);
      add_mark(selection_end(cp), NULL, cp);
    }
}

static void popup_mark_selection_callback(GtkWidget *w, gpointer info)
{
  for_each_chan(mark_selection);
}

static void popup_reverse_selection_callback(GtkWidget *w, gpointer info)
{
  reverse_sound(current_channel(), OVER_SELECTION, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
}

static mus_float_t selection_max = 0.0;

static void selection_info(chan_info *cp)
{
  if ((selection_is_active_in_channel(cp)) &&
      (selection_maxamp(cp) > selection_max))
    selection_max = selection_maxamp(cp);
}

static void popup_selection_info_callback(GtkWidget *w, gpointer info)
{
  selection_max = 0.0;
  for_each_chan(selection_info);
  report_in_minibuffer(any_selected_sound(), "selection max: %f", selection_max);
}

static void popup_selection_apply_controls_callback(GtkWidget *w, gpointer info)
{
  ss->apply_choice = APPLY_TO_SELECTION;
  menu_apply_controls(any_selected_sound());
}


static void popup_loop_play_callback(GtkWidget *w, gpointer info) 
{
  if (ss->selection_play_stop)
    {
      stop_playing_all_sounds(PLAY_BUTTON_UNSET);
      reflect_play_selection_stop();
    }
  else
    {
      set_menu_label(edit_play_menu, "Stop");
      ss->selection_play_stop = true;
      loop_play_selection();
    }
}


void post_selection_popup_menu(void *e) 
{
  GdkEventButton *ev = (GdkEventButton *)e;
  if (!selection_popup_menu)
    {
      selection_popup_menu = gtk_menu_new();
      gtk_widget_set_events(selection_popup_menu, GDK_ALL_EVENTS_MASK);
      gtk_widget_show(selection_popup_menu);

      SG_SIGNAL_CONNECT(selection_popup_menu, "button_release_event", popup_menu_button_release, NULL);

      add_menu_item(selection_popup_menu, "Fill Window",    NULL, (GCallback)popup_show_selection_callback);
      add_menu_item(selection_popup_menu, "Cut",            NULL, (GCallback)edit_cut_callback);
      add_menu_item(selection_popup_menu, "Cut and smooth", NULL, (GCallback)popup_cut_and_smooth_callback);
      add_menu_item(selection_popup_menu, "Cut -> new",     NULL, (GCallback)popup_cut_to_new_callback);
      add_menu_item(selection_popup_menu, "Save as",        NULL, (GCallback)edit_save_as_callback);
      add_menu_item(selection_popup_menu, "Play",           NULL, (GCallback)edit_play_callback);
      add_menu_item(selection_popup_menu, "Play looping",   NULL, (GCallback)popup_loop_play_callback);
      add_menu_item(selection_popup_menu, "Crop",           NULL, (GCallback)popup_crop_callback);
      add_menu_item(selection_popup_menu, "Unselect",       NULL, (GCallback)edit_unselect_callback);
      add_menu_item(selection_popup_menu, "Copy -> new",    NULL, (GCallback)popup_copy_to_new_callback);
      add_menu_item(selection_popup_menu, "-> 0.0",         NULL, (GCallback)popup_zero_selection_callback);
      add_menu_item(selection_popup_menu, "-> 1.0",         NULL, (GCallback)popup_normalize_selection_callback);
      add_menu_item(selection_popup_menu, "Paste",          NULL, (GCallback)edit_paste_callback);
      add_menu_item(selection_popup_menu, "Mix",            NULL, (GCallback)edit_mix_callback);
      add_menu_item(selection_popup_menu, "Mark",           NULL, (GCallback)popup_mark_selection_callback);
      add_menu_item(selection_popup_menu, "Reverse",        NULL, (GCallback)popup_reverse_selection_callback);
      add_menu_item(selection_popup_menu, "Apply controls", NULL, (GCallback)popup_selection_apply_controls_callback);
      add_menu_item(selection_popup_menu, "Info",           NULL, (GCallback)popup_selection_info_callback);
    }

  gtk_menu_popup(GTK_MENU(selection_popup_menu), NULL, NULL, NULL, NULL, POPUP_BUTTON, EVENT_TIME(ev));
}


/* -------- fft popup -------- */

static GtkWidget *fft_popup_menu = NULL;

static void popup_peaks_callback(GtkWidget *w, gpointer info) 
{
  FILE *peaks_fd;
  peaks_fd = FOPEN("fft.txt", "w");
  if (peaks_fd)
    {
      write_transform_peaks(peaks_fd, current_channel()); /* follows sync */
      fclose(peaks_fd);
    }
}

static void fft_size_16_callback(GtkWidget *w, gpointer info) {set_transform_size(16);}
static void fft_size_64_callback(GtkWidget *w, gpointer info) {set_transform_size(64);}
static void fft_size_256_callback(GtkWidget *w, gpointer info) {set_transform_size(256);}
static void fft_size_1024_callback(GtkWidget *w, gpointer info) {set_transform_size(1024);}
static void fft_size_4096_callback(GtkWidget *w, gpointer info) {set_transform_size(4096);}
static void fft_size_16384_callback(GtkWidget *w, gpointer info) {set_transform_size(16384);}
static void fft_size_65536_callback(GtkWidget *w, gpointer info) {set_transform_size(65536);}
static void fft_size_262144_callback(GtkWidget *w, gpointer info) {set_transform_size(262144);}
static void fft_size_1048576_callback(GtkWidget *w, gpointer info) {set_transform_size(1048576);}


static void fft_window_rectangular_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_RECTANGULAR_WINDOW);}
static void fft_window_hann_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_HANN_WINDOW);}
static void fft_window_welch_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_WELCH_WINDOW);}
static void fft_window_parzen_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_PARZEN_WINDOW);}
static void fft_window_bartlett_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_BARTLETT_WINDOW);}
static void fft_window_blackman2_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_BLACKMAN2_WINDOW);}
static void fft_window_blackman3_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_BLACKMAN3_WINDOW);}
static void fft_window_blackman4_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_BLACKMAN4_WINDOW);}
static void fft_window_hamming_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_HAMMING_WINDOW);}
static void fft_window_exponential_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_EXPONENTIAL_WINDOW);}
static void fft_window_riemann_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_RIEMANN_WINDOW);}
static void fft_window_kaiser_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_KAISER_WINDOW);}
static void fft_window_cauchy_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_CAUCHY_WINDOW);}
static void fft_window_poisson_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_POISSON_WINDOW);}
static void fft_window_gaussian_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_GAUSSIAN_WINDOW);}
static void fft_window_tukey_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_TUKEY_WINDOW);}
static void fft_window_dolph_chebyshev_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_DOLPH_CHEBYSHEV_WINDOW);}
static void fft_window_blackman6_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_BLACKMAN6_WINDOW);}
static void fft_window_blackman8_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_BLACKMAN8_WINDOW);}
static void fft_window_blackman10_callback(GtkWidget *w, gpointer info) {set_fft_window(MUS_BLACKMAN10_WINDOW);}

static void fft_type_fourier_callback(GtkWidget *w, gpointer info) {set_transform_type(FOURIER);}
static void fft_type_wavelet_callback(GtkWidget *w, gpointer info) {set_transform_type(WAVELET);}
static void fft_type_autocorrelation_callback(GtkWidget *w, gpointer info) {set_transform_type(AUTOCORRELATION);}
static void fft_type_cepstrum_callback(GtkWidget *w, gpointer info) {set_transform_type(CEPSTRUM);}


static void fft_graph_once_callback(GtkWidget *w, gpointer info) {set_transform_graph_type(GRAPH_ONCE);}
static void fft_graph_sonogram_callback(GtkWidget *w, gpointer info) {set_transform_graph_type(GRAPH_AS_SONOGRAM);}
static void fft_graph_spectrogram_callback(GtkWidget *w, gpointer info) {set_transform_graph_type(GRAPH_AS_SPECTROGRAM);}


static void fft_gray_callback(GtkWidget *w, gpointer info) {set_color_map(GRAY_COLORMAP);}
static void fft_hot_callback(GtkWidget *w, gpointer info) {set_color_map(HOT_COLORMAP);}
static void fft_cool_callback(GtkWidget *w, gpointer info) {set_color_map(COOL_COLORMAP);}
static void fft_bone_callback(GtkWidget *w, gpointer info) {set_color_map(BONE_COLORMAP);}
static void fft_copper_callback(GtkWidget *w, gpointer info) {set_color_map(COPPER_COLORMAP);}
static void fft_pink_callback(GtkWidget *w, gpointer info) {set_color_map(PINK_COLORMAP);}
static void fft_jet_callback(GtkWidget *w, gpointer info) {set_color_map(JET_COLORMAP);}
static void fft_prism_callback(GtkWidget *w, gpointer info) {set_color_map(PRISM_COLORMAP);}
static void fft_autumn_callback(GtkWidget *w, gpointer info) {set_color_map(AUTUMN_COLORMAP);}
static void fft_winter_callback(GtkWidget *w, gpointer info) {set_color_map(WINTER_COLORMAP);}
static void fft_spring_callback(GtkWidget *w, gpointer info) {set_color_map(SPRING_COLORMAP);}
static void fft_summer_callback(GtkWidget *w, gpointer info) {set_color_map(SUMMER_COLORMAP);}
static void fft_rainbow_callback(GtkWidget *w, gpointer info) {set_color_map(RAINBOW_COLORMAP);}
static void fft_flag_callback(GtkWidget *w, gpointer info) {set_color_map(FLAG_COLORMAP);}
static void fft_phases_callback(GtkWidget *w, gpointer info) {set_color_map(PHASES_COLORMAP);}
static void fft_black_and_white_callback(GtkWidget *w, gpointer info) {set_color_map(BLACK_AND_WHITE_COLORMAP);}


void post_fft_popup_menu(void *e)
{
  GdkEventButton *ev = (GdkEventButton *)e;
  if (!fft_popup_menu)
    {
      GtkWidget *outer_menu, *cascade_menu;
      fft_popup_menu = gtk_menu_new();
      gtk_widget_set_events(fft_popup_menu, GDK_ALL_EVENTS_MASK);
      gtk_widget_show(fft_popup_menu);

      SG_SIGNAL_CONNECT(fft_popup_menu, "button_release_event", popup_menu_button_release, NULL);

      outer_menu = add_menu_item(fft_popup_menu, "Size", NULL, NULL);
      cascade_menu = gtk_menu_new();
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(outer_menu), cascade_menu);

      add_menu_item(cascade_menu, "16",      NULL, (GCallback)fft_size_16_callback);
      add_menu_item(cascade_menu, "64",      NULL, (GCallback)fft_size_64_callback);
      add_menu_item(cascade_menu, "256",     NULL, (GCallback)fft_size_256_callback);
      add_menu_item(cascade_menu, "1024",    NULL, (GCallback)fft_size_1024_callback);
      add_menu_item(cascade_menu, "4096",    NULL, (GCallback)fft_size_4096_callback);
      add_menu_item(cascade_menu, "16384",   NULL, (GCallback)fft_size_16384_callback);
      add_menu_item(cascade_menu, "65536",   NULL, (GCallback)fft_size_65536_callback);
      add_menu_item(cascade_menu, "262144",  NULL, (GCallback)fft_size_262144_callback);
      add_menu_item(cascade_menu, "1048576", NULL, (GCallback)fft_size_1048576_callback);


      outer_menu = add_menu_item(fft_popup_menu, "Window", NULL, NULL);
      cascade_menu = gtk_menu_new();
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(outer_menu), cascade_menu);

      add_menu_item(cascade_menu, "rectangular",     NULL, (GCallback)fft_window_rectangular_callback);
      add_menu_item(cascade_menu, "hann",            NULL, (GCallback)fft_window_hann_callback);
      add_menu_item(cascade_menu, "welch",           NULL, (GCallback)fft_window_welch_callback);
      add_menu_item(cascade_menu, "parzen",          NULL, (GCallback)fft_window_parzen_callback);
      add_menu_item(cascade_menu, "bartlett",        NULL, (GCallback)fft_window_bartlett_callback);
      add_menu_item(cascade_menu, "hamming",         NULL, (GCallback)fft_window_hamming_callback);
      add_menu_item(cascade_menu, "blackman2",       NULL, (GCallback)fft_window_blackman2_callback);
      add_menu_item(cascade_menu, "blackman3",       NULL, (GCallback)fft_window_blackman3_callback);
      add_menu_item(cascade_menu, "blackman4",       NULL, (GCallback)fft_window_blackman4_callback);
      add_menu_item(cascade_menu, "exponential",     NULL, (GCallback)fft_window_exponential_callback);
      add_menu_item(cascade_menu, "riemann",         NULL, (GCallback)fft_window_riemann_callback);
      add_menu_item(cascade_menu, "kaiser",          NULL, (GCallback)fft_window_kaiser_callback);
      add_menu_item(cascade_menu, "cauchy",          NULL, (GCallback)fft_window_cauchy_callback);
      add_menu_item(cascade_menu, "poisson",         NULL, (GCallback)fft_window_poisson_callback);
      add_menu_item(cascade_menu, "gaussian",        NULL, (GCallback)fft_window_gaussian_callback);
      add_menu_item(cascade_menu, "tukey",           NULL, (GCallback)fft_window_tukey_callback);
      add_menu_item(cascade_menu, "dolph-chebyshev", NULL, (GCallback)fft_window_dolph_chebyshev_callback);
      add_menu_item(cascade_menu, "blackman6",       NULL, (GCallback)fft_window_blackman6_callback);
      add_menu_item(cascade_menu, "blackman8",       NULL, (GCallback)fft_window_blackman8_callback);
      add_menu_item(cascade_menu, "blackman10" ,     NULL, (GCallback)fft_window_blackman10_callback);


      outer_menu = add_menu_item(fft_popup_menu, "Graph type", NULL, NULL);
      cascade_menu = gtk_menu_new();
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(outer_menu), cascade_menu);

      add_menu_item(cascade_menu, "one fft",     NULL, (GCallback)fft_graph_once_callback);
      add_menu_item(cascade_menu, "sonogram",    NULL, (GCallback)fft_graph_sonogram_callback);
      add_menu_item(cascade_menu, "spectrogram", NULL, (GCallback)fft_graph_spectrogram_callback);


      outer_menu = add_menu_item(fft_popup_menu, "Transform type", NULL, NULL);
      cascade_menu = gtk_menu_new();
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(outer_menu), cascade_menu);

      add_menu_item(cascade_menu, "fourier",         NULL, (GCallback)fft_type_fourier_callback);
      add_menu_item(cascade_menu, "wavelet",         NULL, (GCallback)fft_type_wavelet_callback);
      add_menu_item(cascade_menu, "autocorrelation", NULL, (GCallback)fft_type_autocorrelation_callback);
      add_menu_item(cascade_menu, "cepstrum",        NULL, (GCallback)fft_type_cepstrum_callback);


      outer_menu = add_menu_item(fft_popup_menu, "Colormap", NULL, NULL);
      cascade_menu = gtk_menu_new();
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(outer_menu), cascade_menu);

      add_menu_item(cascade_menu, "gray",    NULL, (GCallback)fft_gray_callback);
      add_menu_item(cascade_menu, "autumn",  NULL, (GCallback)fft_autumn_callback);
      add_menu_item(cascade_menu, "spring",  NULL, (GCallback)fft_spring_callback);
      add_menu_item(cascade_menu, "winter",  NULL, (GCallback)fft_winter_callback);
      add_menu_item(cascade_menu, "summer",  NULL, (GCallback)fft_summer_callback);
      add_menu_item(cascade_menu, "cool",    NULL, (GCallback)fft_cool_callback);
      add_menu_item(cascade_menu, "copper",  NULL, (GCallback)fft_copper_callback);
      add_menu_item(cascade_menu, "flag",    NULL, (GCallback)fft_flag_callback);
      add_menu_item(cascade_menu, "prism",   NULL, (GCallback)fft_prism_callback);
      add_menu_item(cascade_menu, "bone",    NULL, (GCallback)fft_bone_callback);
      add_menu_item(cascade_menu, "hot",     NULL, (GCallback)fft_hot_callback);
      add_menu_item(cascade_menu, "jet",     NULL, (GCallback)fft_jet_callback);
      add_menu_item(cascade_menu, "pink",    NULL, (GCallback)fft_pink_callback);
      add_menu_item(cascade_menu, "rainbow", NULL, (GCallback)fft_rainbow_callback);
      add_menu_item(cascade_menu, "phases",  NULL, (GCallback)fft_phases_callback);
      add_menu_item(cascade_menu, "black and white", NULL, (GCallback)fft_black_and_white_callback);

      add_menu_item(fft_popup_menu, "Peaks->fft.txt", NULL, (GCallback)popup_peaks_callback);
    }

  gtk_menu_popup(GTK_MENU(fft_popup_menu), NULL, NULL, NULL, NULL, POPUP_BUTTON, EVENT_TIME(ev));
}



void post_lisp_popup_menu(void *e) {}



/* ---------------- toolbar ---------------- */

void add_tooltip(GtkWidget *w, const char *tip)
{
#if (!HAVE_GTK_3)
  gtk_widget_set_tooltip_text(w, tip);
#else
  char *str;
  int i, len;
  len = mus_strlen(tip);
  str = (char *)calloc(len + 1, sizeof(char));
  for (i = 0; i < len; i++)
    {
      if (tip[i] == '\n') 
	str[i] = ' ';
      else str[i] = tip[i];
    }
  gtk_widget_set_tooltip_text(w, str);
  free(str);
#endif
}


static void add_to_toolbar(GtkWidget *bar, const gchar *stock, const char *tip, GCallback callback)
{
  GtkToolItem *w;
  w = gtk_tool_button_new_from_stock(stock);
  gtk_toolbar_insert(GTK_TOOLBAR(bar), w, -1); /* -1 = at end */
  add_tooltip(GTK_WIDGET(w), tip);
  gtk_widget_show(GTK_WIDGET(w));
  g_signal_connect(GTK_WIDGET(w), "clicked", callback, NULL);
}


static void add_separator_to_toolbar(GtkWidget *bar)
{
  GtkToolItem *w;
  w = gtk_separator_tool_item_new();
  gtk_toolbar_insert(GTK_TOOLBAR(bar), w, -1);
  gtk_widget_show(GTK_WIDGET(w));
}


static void play_from_start_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    play_sound(sp, 0, NO_END_SPECIFIED);
}


static void play_from_cursor_callback(GtkWidget *w, gpointer info)
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      chan_info *cp;
      cp = any_selected_channel(sp);
      if (cp)
	play_sound(sp, CURSOR(cp), NO_END_SPECIFIED);
    }
}


static void stop_playing_callback(GtkWidget *w, gpointer info) 
{
  stop_playing_all_sounds(PLAY_C_G);
  reflect_play_selection_stop(); /* this sets ss->selection_play_stop = false; */
}


static void full_dur_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      int i;
      for (i = 0; i < sp->nchans; i++)
	set_x_axis_x0x1(sp->chans[i], 0.0, sp->chans[i]->axis->xmax);
    }
}


static void zoom_out_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      int i;
      for (i = 0; i < sp->nchans; i++)
	zx_incremented(sp->chans[i], 2.0);
    }
}


static void zoom_in_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      int i;
      for (i = 0; i < sp->nchans; i++)
	zx_incremented(sp->chans[i], 0.5);
    }
}    


static void goto_start_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      int i;
      for (i = 0; i < sp->nchans; i++)
	set_x_axis_x0x1(sp->chans[i], 0.0, sp->chans[i]->axis->x1 - sp->chans[i]->axis->x0);
    }
}

static void go_back_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      int i;
      for (i = 0; i < sp->nchans; i++)
	sx_incremented(sp->chans[i], -1.0);
    }
}


static void go_forward_callback(GtkWidget *w, gpointer info)
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      int i;
      for (i = 0; i < sp->nchans; i++)
	sx_incremented(sp->chans[i], 1.0);
    }
}

static void goto_end_callback(GtkWidget *w, gpointer info) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    {
      int i;
      for (i = 0; i < sp->nchans; i++)
	set_x_axis_x0x1(sp->chans[i], sp->chans[i]->axis->xmax - sp->chans[i]->axis->x1 + sp->chans[i]->axis->x0, sp->chans[i]->axis->xmax);
    }
}


static GtkWidget *toolbar = NULL;

void show_toolbar(void)
{
  if (!toolbar)
    {
      toolbar = gtk_toolbar_new();
      gtk_box_pack_start(GTK_BOX(MAIN_PANE(ss)), toolbar, false, false, 0); /* MAIN_PANE = top level vbox */
      gtk_box_reorder_child(GTK_BOX(MAIN_PANE(ss)), toolbar, 1);            /* put toolbar just under the top level menubar */

      add_to_toolbar(toolbar, GTK_STOCK_NEW,             "new sound",                  (GCallback)file_new_callback);
      add_to_toolbar(toolbar, GTK_STOCK_OPEN,            "open sound",                 (GCallback)file_open_callback);
      add_to_toolbar(toolbar, GTK_STOCK_SAVE_AS,         "save selected sound",        (GCallback)file_save_as_callback);
      add_to_toolbar(toolbar, GTK_STOCK_REVERT_TO_SAVED, "revert to saved",            (GCallback)file_revert_callback);
      add_to_toolbar(toolbar, GTK_STOCK_UNDO,            "undo edit",                  (GCallback)edit_undo_callback);
      add_to_toolbar(toolbar, GTK_STOCK_REDO,            "redo last (undone) edit",    (GCallback)edit_redo_callback);
      add_to_toolbar(toolbar, GTK_STOCK_CLOSE,           "close selected sound",       (GCallback)file_close_callback);
      add_separator_to_toolbar(toolbar);

      add_to_toolbar(toolbar, GTK_STOCK_MEDIA_PLAY,      "play from the start",        (GCallback)play_from_start_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_MEDIA_FORWARD,   "play from the cursor",       (GCallback)play_from_cursor_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_MEDIA_STOP,      "stop playing",               (GCallback)stop_playing_callback);      
      add_separator_to_toolbar(toolbar);
 
      add_to_toolbar(toolbar, GTK_STOCK_FULLSCREEN,      "show full sound",            (GCallback)full_dur_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_ZOOM_OUT,        "zoom out",                   (GCallback)zoom_out_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_ZOOM_IN,         "zoom in",                    (GCallback)zoom_in_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_GOTO_FIRST,      "go to start of sound",       (GCallback)goto_start_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_GO_BACK,         "go back a window",           (GCallback)go_back_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_GO_FORWARD,      "go forward a window",        (GCallback)go_forward_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_GOTO_LAST,       "go to end of sound",         (GCallback)goto_end_callback);      
      add_separator_to_toolbar(toolbar);

#if 0
      add_to_toolbar(toolbar, GTK_STOCK_SELECT_ALL,      "select all of sound",        (GCallback)edit_select_all_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_CLEAR,           "unselect everything",        (GCallback)edit_unselect_callback);  
#endif    
      add_to_toolbar(toolbar, GTK_STOCK_CUT,             "delete selection",           (GCallback)edit_cut_callback);      
      add_to_toolbar(toolbar, GTK_STOCK_PASTE,           "insert selection at cursor", (GCallback)edit_paste_callback);      
#if 0
      add_separator_to_toolbar(toolbar);
#endif

      add_to_toolbar(toolbar, GTK_STOCK_PREFERENCES,     "open preferences dialog",    (GCallback)options_preferences_callback);
      add_to_toolbar(toolbar, GTK_STOCK_CANCEL,          "stop the current operation", (GCallback)stop_everything_callback);
      add_to_toolbar(toolbar, GTK_STOCK_QUIT,            "exit Snd",                   (GCallback)file_exit_callback);
    }

  gtk_widget_show(toolbar);
}


void hide_toolbar(void)
{
  if (toolbar)
    gtk_widget_hide(toolbar);
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
enum {FILE_MENU, EDIT_MENU, VIEW_MENU, OPTIONS_MENU, HELP_MENU};
#define INVALID_MENU -1

static int callb2option(int callb)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if (added_options_callb[i] == callb)
      return(i);
  return(-1);
}


static void menu_callback(GtkWidget *w, gpointer info) 
{
  int callb, opt;
  callb = get_user_int_data(G_OBJECT(w));
  opt = callb2option(callb);
  if (opt != -1)
    g_menu_callback(callb);
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
	  added_options = (GtkWidget **)calloc(added_options_size, sizeof(GtkWidget *));
	  added_options_names = (char **)calloc(added_options_size, sizeof(char *));
	  added_options_menus = (int *)calloc(added_options_size, sizeof(int));
	  added_options_callb = (int *)calloc(added_options_size, sizeof(int));
	}
      else
	{
	  int i;
	  added_options = (GtkWidget **)realloc(added_options, added_options_size * sizeof(GtkWidget *));
	  added_options_names = (char **)realloc(added_options_names, added_options_size * sizeof(char *));
	  added_options_menus = (int *)realloc(added_options_menus, added_options_size * sizeof(int));
	  added_options_callb = (int *)realloc(added_options_callb, added_options_size * sizeof(int));
	  for (i = added_options_pos; i < added_options_size; i++) 
	    {
	      added_options[i] = NULL;
	      added_options_callb[i] = 0;
	    }
	}
    }
  added_options[added_options_pos] = w;
  added_options_menus[added_options_pos] = which_menu;
  added_options_names[added_options_pos] = mus_strdup(label);
  added_options_callb[added_options_pos] = callb;
  added_options_pos++;
}


static int remove_option(int which_menu, const char *label)
{
  int i;
  for (i = 0; i < added_options_pos; i++)
    if ((added_options_menus[i] == which_menu) && 
	(mus_strcmp(label, added_options_names[i])) && 
	(added_options[i]))
      {
	unprotect_callback(added_options_callb[i]);
	gtk_widget_hide(added_options[i]); /* destroy here causes segfault in gtk2? */
	added_options[i] = NULL;
	added_options_menus[i] = -1;
	free(added_options_names[i]);
	added_options_names[i] = NULL;
	return(0);
      }
  for (i = 0; i < NUM_MENU_WIDGETS; i++)
    if ((ml[i]) &&
	(strcmp(label, ml[i]) == 0))
      {
	gtk_widget_hide(ss->mw[i]);
	return(0);
      }
  return(INVALID_MENU);
}


int g_add_to_main_menu(const char *label, int slot)
{
  GtkWidget *m, *mc;
  if (new_menu >= MAX_MAIN_MENUS) return(INVALID_MENU);
  m = gtk_menu_item_new_with_label(label);
  gtk_menu_shell_append(GTK_MENU_SHELL(main_menu), m);
  gtk_widget_show(m);
  set_user_int_data(G_OBJECT(m), slot);
  if (slot >= 0) 
    {
      SG_SIGNAL_CONNECT(m, "activate", menu_callback, NULL);
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
       SG_SIGNAL_CONNECT(m, "activate", menu_callback, NULL);
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
  #define H_menu_widgets "(" S_menu_widgets "): a list of the top level menu widgets: ((0)main (1)file (2)edit (3)view (4)options (5)help)"
  return(XEN_CONS(XEN_WRAP_WIDGET(main_menu),
	  XEN_CONS(XEN_WRAP_WIDGET(file_menu),
           XEN_CONS(XEN_WRAP_WIDGET(edit_menu),
            XEN_CONS(XEN_WRAP_WIDGET(view_menu),
             XEN_CONS(XEN_WRAP_WIDGET(options_menu),
              XEN_CONS(XEN_WRAP_WIDGET(help_menu),
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


