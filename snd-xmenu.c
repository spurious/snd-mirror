#include "snd.h"
#include "snd-menu.h"
#include <X11/cursorfont.h>

void set_menu_label(Widget w, const char *label) {if (w) set_button_label(w, label);}


/* -------------------------------- FILE MENU -------------------------------- */

static Widget *recent_file_items = NULL;
static int recent_file_items_size = 0;

static void open_recent_file_callback(Widget w, XtPointer context, XtPointer info)
{
  char *filename;
  snd_info *sp;
  filename = get_label(w);
  ss->open_requestor = FROM_OPEN_RECENT_MENU;
  ss->open_requestor_data = NULL;
  sp = snd_open_file(filename, FILE_READ_WRITE);
  if (sp) select_channel(sp, 0);
}


static void file_open_recent_callback(Widget w, XtPointer info, XtPointer context) 
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
	    recent_file_items = (Widget *)calloc(size, sizeof(Widget));
	  else
	    {
	      recent_file_items = (Widget *)realloc(recent_file_items, size * sizeof(Widget));
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
	      int n = 0;
	      Arg args[6];

	      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;

	      recent_file_items[i] = XtCreateManagedWidget(recent_file_names[i], xmPushButtonWidgetClass, file_open_recent_menu, args, n);
	      XtAddCallback(recent_file_items[i], XmNactivateCallback, open_recent_file_callback, NULL); 
	    }
	  else
	    {
	      set_label(recent_file_items[i], recent_file_names[i]);
	      XtManageChild(recent_file_items[i]);
	    }
	}

      for (i = size; i < recent_file_items_size; i++) /* maybe previous file was deleted */
	if ((recent_file_items[i]) &&
	    (XtIsManaged(recent_file_items[i])))
	  XtUnmanageChild(recent_file_items[i]);
    }
}


static void make_open_recent_menu(void)
{
  int n = 0;
  Arg args[6];
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  XtSetArg(args[n], XmNpositionIndex, 1); n++;  /* just after "Open" menu */
  
  file_open_recent_menu = XmCreatePulldownMenu(file_menu, (char *)"open-recent", args, n);
	  
  XtSetArg(args[n], XmNsubMenuId, file_open_recent_menu); n++;

  file_open_recent_cascade_menu = XtCreateManagedWidget("Open recent", xmCascadeButtonWidgetClass, file_menu, args, n);
  XtAddCallback(file_open_recent_cascade_menu, XmNcascadingCallback, file_open_recent_callback, NULL);
}


static void file_menu_update_1(Widget w, XtPointer info, XtPointer context) 
{
  if (recent_files_size() > 0)
    {
      if (file_open_recent_menu == NULL)
	make_open_recent_menu();
      else set_sensitive(file_open_recent_cascade_menu, true);
    }
  else
    {
      if (file_open_recent_menu)
	set_sensitive(file_open_recent_cascade_menu, false);
    }
    
  file_menu_update();
}


static void file_open_callback(Widget w, XtPointer info, XtPointer context) {make_open_file_dialog(FILE_READ_WRITE, true);}
static void file_view_callback(Widget w, XtPointer info, XtPointer context) {make_open_file_dialog(FILE_READ_ONLY, true);}
static void file_new_callback(Widget w, XtPointer info, XtPointer context) {make_new_file_dialog(true);}
static void file_close_callback(Widget w, XtPointer info, XtPointer context) {if (any_selected_sound()) snd_close_file(any_selected_sound());}
static void file_close_all_callback(Widget w, XtPointer info, XtPointer context) {for_each_sound(snd_close_file);}
static void file_save_callback(Widget w, XtPointer info, XtPointer context) {if (any_selected_sound()) save_edits_from_kbd(any_selected_sound());}
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
static void edit_unselect_callback(Widget w, XtPointer info, XtPointer context) {deactivate_selection();}
static void edit_undo_callback(Widget w, XtPointer info, XtPointer context) {undo_edit_with_sync(current_channel(), 1);}
static void edit_redo_callback(Widget w, XtPointer info, XtPointer context) {redo_edit_with_sync(current_channel(), 1);}

static void edit_menu_update_1(Widget w, XtPointer info, XtPointer context) {edit_menu_update();}


#if WITH_AUDIO
static void edit_play_callback(Widget w, XtPointer info, XtPointer context) 
{
  if (ss->selection_play_stop)
    {
      stop_playing_all_sounds(PLAY_BUTTON_UNSET);
      reflect_play_selection_stop(); /* if there was an error, stop_playing might not remember to clear this */
    }
  else
    {
      set_menu_label(edit_play_menu, I_STOP);
      ss->selection_play_stop = true;
      play_selection(IN_BACKGROUND);
    }
}


void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu, "Play Selection");
  ss->selection_play_stop = false;
}

#else

void reflect_play_selection_stop(void)
{
}

#endif


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

static Widget *view_files_items = NULL;
static Widget view_files_cascade_menu = NULL;
static int view_files_items_size = 0;


static void view_files_item_callback(Widget w, XtPointer context, XtPointer info)
{
  view_files_start_dialog_with_title(get_label(w));
}


static void view_files_callback(Widget w, XtPointer info, XtPointer context) 
{
  int size;

  size = view_files_dialog_list_length();
  if (size == 0)
    make_view_files_dialog(true, true); /* managed and empty (brand-new) */
  else
    {
      if (size == 1)
	make_view_files_dialog(true, false); /* raise current */
      else
	{
	  int i;
	  char **view_files_names;
	  
	  if ((XmIsPushButton(view_files_menu)) && /* autotest check */
	      (!view_files_cascade_menu))
	    return;
	  
	  view_files_names = view_files_dialog_titles();
	  if (size > view_files_items_size)
	    {
	      if (view_files_items_size == 0)
		view_files_items = (Widget *)calloc(size, sizeof(Widget));
	      else
		{
		  view_files_items = (Widget *)realloc(view_files_items, size * sizeof(Widget));
		  for (i = view_files_items_size; i < size; i++)
		    view_files_items[i] = NULL;
		}
	      view_files_items_size = size;
	    }
	  
	  for (i = 0; i < size; i++)
	    {
	      if (view_files_items[i] == NULL)
		{
		  int n = 0;
		  Arg args[6];
		  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
		  
		  view_files_items[i] = XtCreateManagedWidget(view_files_names[i], xmPushButtonWidgetClass, view_files_menu, args, n);
		  XtAddCallback(view_files_items[i], XmNactivateCallback, view_files_item_callback, NULL); 
		}
	      else
		{
		  set_label(view_files_items[i], view_files_names[i]);
		  XtManageChild(view_files_items[i]);
		}
	      free(view_files_names[i]);
	    }
	  free(view_files_names);
	}
    }
}


static void make_view_files_list_menu(void)
{
  int n = 0, pos = 2;
  Arg args[6];

  if ((view_files_menu) &&
      (XmIsPushButton(view_files_menu)))
    {
      XtVaGetValues(view_files_menu, XmNpositionIndex, &pos, NULL);
      XtUnmanageChild(view_files_menu);
    }
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  XtSetArg(args[n], XmNpositionIndex, pos); n++;
  
  view_files_menu = XmCreatePulldownMenu(view_menu, (char *)"view-files", args, n);
  XtSetArg(args[n], XmNsubMenuId, view_files_menu); n++;

  view_files_cascade_menu = XtCreateManagedWidget("Files", xmCascadeButtonWidgetClass, view_menu, args, n);
  XtAddCallback(view_files_cascade_menu, XmNcascadingCallback, view_files_callback, NULL);
}


static void view_menu_update_1(Widget w, XtPointer info, XtPointer context) 
{
  if ((view_files_dialog_list_length() > 1) &&
      (!view_files_cascade_menu))
    make_view_files_list_menu();
    
  view_menu_update();
}



static void view_separate_callback(Widget w, XtPointer info, XtPointer context) {set_channel_style(CHANNELS_SEPARATE);}
static void view_combined_callback(Widget w, XtPointer info, XtPointer context) {set_channel_style(CHANNELS_COMBINED);}
static void view_superimposed_callback(Widget w, XtPointer info, XtPointer context) {set_channel_style(CHANNELS_SUPERIMPOSED);}
static void view_dots_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_DOTS);}
static void view_lines_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_LINES);}
static void view_filled_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_FILLED);}
static void view_dots_and_lines_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_DOTS_AND_LINES);}
static void view_lollipops_callback(Widget w, XtPointer info, XtPointer context) {set_graph_style(GRAPH_LOLLIPOPS);}
#if HAVE_EXTENSION_LANGUAGE
static void view_listener_callback(Widget w, XtPointer info, XtPointer context) {handle_listener(!(listener_is_visible()));}
#endif
static void view_mix_dialog_callback(Widget w, XtPointer info, XtPointer context) {make_mix_dialog();}
static void view_zero_callback(Widget w, XtPointer info, XtPointer context){set_show_y_zero((!(show_y_zero(ss))));}
static void view_cursor_callback(Widget w, XtPointer info, XtPointer context){set_with_verbose_cursor((!(with_verbose_cursor(ss))));}

#if HAVE_EXTENSION_LANGUAGE
static void view_inset_callback(Widget w, XtPointer info, XtPointer context)
{
  set_with_inset_graph((!(with_inset_graph(ss))));
  for_each_chan(update_graph);
}
#endif

static void view_controls_callback(Widget w, XtPointer info, XtPointer context) {set_show_controls(!in_show_controls(ss));}
static void view_region_callback_1(Widget w, XtPointer info, XtPointer context) {view_region_callback(w, info, context);}
static void view_color_orientation_callback_1(Widget w, XtPointer info, XtPointer context) {view_color_orientation_callback(w, info, context);}

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

static void view_focus_right_callback(Widget w, XtPointer info, XtPointer context) {set_zoom_focus_style(ZOOM_FOCUS_RIGHT);}
static void view_focus_left_callback(Widget w, XtPointer info, XtPointer context) {set_zoom_focus_style(ZOOM_FOCUS_LEFT);}
static void view_focus_middle_callback(Widget w, XtPointer info, XtPointer context) {set_zoom_focus_style(ZOOM_FOCUS_MIDDLE);}
static void view_focus_active_callback(Widget w, XtPointer info, XtPointer context) {set_zoom_focus_style(ZOOM_FOCUS_ACTIVE);}

static void view_grid_callback(Widget w, XtPointer info, XtPointer context)
{
  if (show_grid(ss) == NO_GRID)
    set_show_grid(WITH_GRID);
  else set_show_grid(NO_GRID);
}



/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_transform_callback(Widget w, XtPointer info, XtPointer context) {make_transform_dialog(true);}
static void options_controls_callback(Widget w, XtPointer info, XtPointer context) {make_controls_dialog();}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(Widget w, XtPointer info, XtPointer context) {save_state_from_menu();}
#endif
static void options_preferences_callback(Widget w, XtPointer info, XtPointer context) {make_preferences_dialog();}



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
static void help_sound_files_callback(Widget w, XtPointer info, XtPointer context) {sound_files_help();}
static void help_keys_callback(Widget w, XtPointer info, XtPointer context) {key_help();}
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
	  if (key == snd_K_f) set_label(file_open_menu, "Open"); else
	  if (key == snd_K_s) set_label(file_save_menu, "Save"); else
	  if (key == snd_K_q) set_label(file_mix_menu, "Mix"); else
	  if (key == snd_K_i) set_label(file_insert_menu, "Insert"); else
	  if (key == snd_K_u) set_label(edit_undo_menu, "Undo"); else
	  if (key == snd_K_r) set_label(edit_redo_menu, "Redo");
	}
      else
	{
	  if (key == snd_K_k) set_label(file_close_menu, "Close"); else
	  if (key == snd_K_i) set_label(edit_paste_menu, "Insert Selection"); else	  
	  if (key == snd_K_q) set_label(edit_mix_menu, "Mix Selection"); else
#if WITH_AUDIO	  
	  if (key == snd_K_p) set_label(edit_play_menu, "Play Selection"); else	  
#endif
	  if (key == snd_K_w) set_label(edit_save_as_menu, "Save Selection");
	}
    }
#if HAVE_EXTENSION_LANGUAGE
  else 
    {
      if ((key == snd_K_s) && (state == snd_ControlMask))
	set_label(edit_find_menu, I_FIND);
    }
#endif
}


/* -------------------------------- MAIN MENU -------------------------------- */

static void menu_drag_watcher(Widget w, const char *str, Position x, Position y, drag_style_t dtype, void *data)
{
  char *new_title;
  switch (dtype)
    {
    case DRAG_ENTER:
      new_title = mus_format("%s: drop to open file", ss->startup_title);
      XtVaSetValues(MAIN_SHELL(ss), XmNtitle, (char *)new_title, NULL);
      XmChangeColor(w, ss->selection_color);
      free(new_title);
      break;

    case DRAG_LEAVE:
      reflect_file_change_in_title();
      XmChangeColor(w, ss->highlight_color);
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

  ss->mw = (Widget *)calloc(NUM_MENU_WIDGETS, sizeof(Widget));

  XtSetArg(main_args[main_n], XmNbackground, ss->basic_color); main_n++;
  XtSetArg(high_args[high_n], XmNbackground, ss->highlight_color); high_n++;
  XtSetArg(in_args[in_n], XmNbackground, ss->basic_color); in_n++;

  start_high_n = high_n;
  XtSetArg(in_args[in_n], XmNsensitive, false); in_n++;
  
  n = high_n;
  XtSetArg(high_args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(high_args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(high_args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(high_args[n], XmNrightAttachment, XmATTACH_FORM); n++;

  main_menu = XmCreateMenuBar(MAIN_PANE(ss), (char *)"menuBar", high_args, n);


  /* FILE MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)0);
  file_menu = XmCreatePulldownMenu(main_menu, (char *)"File", main_args, main_n + 1);
  
  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, file_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'F'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)0); high_n++;
  file_cascade_menu = XtCreateManagedWidget("File", xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  file_open_menu = XtCreateManagedWidget("Open", xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_open_menu, XmNactivateCallback, file_open_callback, NULL);
  XtVaSetValues(file_open_menu, XmNmnemonic, 'O', NULL);

  file_close_menu = XtCreateManagedWidget("Close", xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_close_menu, XmNactivateCallback, file_close_callback, NULL);
  XtVaSetValues(file_close_menu, XmNmnemonic, 'C', NULL);

  file_close_all_menu = XtCreateWidget("Close all", xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_close_all_menu, XmNactivateCallback, file_close_all_callback, NULL);
  
  file_save_menu = XtCreateManagedWidget("Save", xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_save_menu, XmNactivateCallback, file_save_callback, NULL);
  XtVaSetValues(file_save_menu, XmNmnemonic, 'S', NULL);
  
  file_save_as_menu = XtCreateManagedWidget("Save as", xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_save_as_menu, XmNactivateCallback, file_save_as_callback, NULL);
  XtVaSetValues(file_save_as_menu, XmNmnemonic, 'a', NULL);
  
  file_revert_menu = XtCreateManagedWidget("Revert", xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_revert_menu, XmNactivateCallback, file_revert_callback, NULL);
  XtVaSetValues(file_revert_menu, XmNmnemonic, 'R', NULL);
  
  file_mix_menu = XtCreateManagedWidget("Mix", xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_mix_menu, XmNactivateCallback, file_mix_callback_1, NULL);
  XtVaSetValues(file_mix_menu, XmNmnemonic, 'M', NULL);

  file_insert_menu = XtCreateManagedWidget("Insert", xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_insert_menu, XmNactivateCallback, file_insert_callback_1, NULL);
  XtVaSetValues(file_insert_menu, XmNmnemonic, 'I', NULL);

  file_update_menu = XtCreateManagedWidget("Update", xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_update_menu, XmNactivateCallback, file_update_callback, NULL);
  XtVaSetValues(file_update_menu, XmNmnemonic, 'U', NULL);

  file_new_menu = XtCreateManagedWidget("New", xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_new_menu, XmNactivateCallback, file_new_callback, NULL);
  XtVaSetValues(file_new_menu, XmNmnemonic, 'N', NULL);

  file_view_menu = XtCreateManagedWidget("View", xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_view_menu, XmNactivateCallback, file_view_callback, NULL);
  XtVaSetValues(file_view_menu, XmNmnemonic, 'V', NULL);

  file_print_menu = XtCreateManagedWidget("Print", xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_print_menu, XmNactivateCallback, file_print_callback_1, NULL);
  XtVaSetValues(file_print_menu, XmNmnemonic, 'P', NULL);

  j = 0;
  XtSetArg(sep_args[j], XmNbackground, ss->basic_color); j++;
  XtSetArg(sep_args[j], XmNseparatorType, XmSHADOW_ETCHED_IN); j++;
  file_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, file_menu, sep_args, j);

  file_exit_menu = XtCreateManagedWidget("Exit", xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_exit_menu, XmNactivateCallback, file_exit_callback, NULL);
  XtVaSetValues(file_exit_menu, XmNmnemonic, 'E', NULL);


  /* EDIT MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)1);
  edit_menu = XmCreatePulldownMenu(main_menu, (char *)"Edit", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, edit_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'E'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)1); high_n++;
  edit_cascade_menu = XtCreateManagedWidget("Edit", xmCascadeButtonWidgetClass, main_menu, high_args, high_n);
  
  edit_undo_menu = XtCreateManagedWidget("Undo", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_undo_menu, XmNactivateCallback, edit_undo_callback, NULL);
  XtVaSetValues(edit_undo_menu, XmNmnemonic, 'U', NULL);

  edit_redo_menu = XtCreateManagedWidget("Redo", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_redo_menu, XmNactivateCallback, edit_redo_callback, NULL);
  XtVaSetValues(edit_redo_menu, XmNmnemonic, 'R', NULL);

#if HAVE_EXTENSION_LANGUAGE
  edit_find_menu = XtCreateManagedWidget(I_FIND, xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_find_menu, XmNactivateCallback, edit_find_callback_1, NULL);
  XtVaSetValues(edit_find_menu, XmNmnemonic, 'F', NULL);
#endif

  edit_select_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, edit_menu, sep_args, j);

  edit_cut_menu = XtCreateManagedWidget("Delete selection", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_cut_menu, XmNactivateCallback, edit_cut_callback, NULL);
  XtVaSetValues(edit_cut_menu, XmNmnemonic, 'C', NULL);

  edit_paste_menu = XtCreateManagedWidget("Insert selection", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_paste_menu, XmNactivateCallback, edit_paste_callback, NULL);
  XtVaSetValues(edit_paste_menu, XmNmnemonic, 'P', NULL);

  edit_mix_menu = XtCreateManagedWidget("Mix selection", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_mix_menu, XmNactivateCallback, edit_mix_callback, NULL);
  XtVaSetValues(edit_mix_menu, XmNmnemonic, 'M', NULL);

#if WITH_AUDIO
  edit_play_menu = XtCreateManagedWidget("Play selection", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_play_menu, XmNactivateCallback, edit_play_callback, NULL);
  XtVaSetValues(edit_play_menu, XmNmnemonic, 'P', NULL);
#endif

  edit_save_as_menu = XtCreateManagedWidget("Save selection", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_save_as_menu, XmNactivateCallback, edit_save_as_callback, NULL);
  XtVaSetValues(edit_save_as_menu, XmNmnemonic, 'S', NULL);

  edit_select_all_menu = XtCreateManagedWidget("Select all", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_select_all_menu, XmNactivateCallback, edit_select_all_callback, NULL);

  edit_unselect_menu = XtCreateManagedWidget("Unselect all", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_unselect_menu, XmNactivateCallback, edit_unselect_callback, NULL);

  edit_edit_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, edit_menu, sep_args, j);

  edit_env_menu = XtCreateManagedWidget("Edit envelope", xmPushButtonWidgetClass, edit_menu, main_args, main_n);
  XtAddCallback(edit_env_menu, XmNactivateCallback, edit_envelope_callback, NULL);
  XtVaSetValues(edit_env_menu, XmNmnemonic, 'E', NULL);

  edit_header_menu = XtCreateManagedWidget("Edit header", xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_header_menu, XmNactivateCallback, edit_header_callback_1, NULL);
  XtVaSetValues(edit_header_menu, XmNmnemonic, 'H', NULL);


  /* VIEW MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)2);
  view_menu = XmCreatePulldownMenu(main_menu, (char *)"View", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, view_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'V'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)2); high_n++;
  view_cascade_menu = XtCreateManagedWidget("View", xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

#if HAVE_EXTENSION_LANGUAGE
  view_listener_menu = XtCreateManagedWidget("Open listener", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_listener_menu, XmNactivateCallback, view_listener_callback, NULL);
  XtVaSetValues(view_listener_menu, XmNmnemonic, 'L', NULL);
#endif

  view_files_menu = XtCreateManagedWidget("Files", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_files_menu, XmNactivateCallback, view_files_callback, NULL);
  XtVaSetValues(view_files_menu, XmNmnemonic, 'F', NULL);

  view_mix_dialog_menu = XtCreateManagedWidget("Mixes", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_mix_dialog_menu, XmNactivateCallback, view_mix_dialog_callback, NULL);

  view_region_menu = XtCreateManagedWidget("Regions", xmPushButtonWidgetClass, view_menu, in_args, in_n);
  XtAddCallback(view_region_menu, XmNactivateCallback, view_region_callback_1, NULL);
  XtVaSetValues(view_region_menu, XmNmnemonic, 'R', NULL);

  view_color_orientation_menu = XtCreateManagedWidget("Color/Orientation", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_color_orientation_menu, XmNactivateCallback, view_color_orientation_callback_1, NULL);

  view_controls_menu = XtCreateManagedWidget("Show controls", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_controls_menu, XmNactivateCallback, view_controls_callback, NULL);
  XtVaSetValues(view_controls_menu, XmNmnemonic, 'S', NULL);

  view_sep2_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, view_menu, sep_args, j);

  view_graph_style_menu = XmCreatePulldownMenu(view_menu, (char *)"graph-style", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_graph_style_menu); k++;
  view_graph_style_cascade_menu = XtCreateManagedWidget(I_LINES_OR_DOTS, xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_lines_menu = XtCreateManagedWidget("lines", xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_lines_menu, XmNactivateCallback, view_lines_callback, NULL); 
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(view_lines_menu, false);

  view_dots_menu = XtCreateManagedWidget("dots", xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_dots_menu, XmNactivateCallback, view_dots_callback, NULL);  
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(view_dots_menu, false);

  view_filled_menu = XtCreateManagedWidget("filled", xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_filled_menu, XmNactivateCallback, view_filled_callback, NULL);  
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(view_filled_menu, false);

  view_dots_and_lines_menu = XtCreateManagedWidget("dots and lines", xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_dots_and_lines_menu, XmNactivateCallback, view_dots_and_lines_callback, NULL);  
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(view_dots_and_lines_menu, false);

  view_lollipops_menu = XtCreateManagedWidget("lollipops", xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_lollipops_menu, XmNactivateCallback, view_lollipops_callback, NULL);  
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(view_lollipops_menu, false);

  view_cursor_menu = XtCreateManagedWidget("Verbose cursor", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_cursor_menu, XmNactivateCallback, view_cursor_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  view_inset_menu = XtCreateManagedWidget("With inset graph", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_inset_menu, XmNactivateCallback, view_inset_callback, NULL);
#endif

  view_combine_menu = XmCreatePulldownMenu(view_menu, (char *)"combine", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_combine_menu); k++;
  view_combine_cascade_menu = XtCreateManagedWidget(I_CHANNEL_LAYOUT, xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_combine_separate_menu = XtCreateManagedWidget("separate", xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_separate_menu, XmNactivateCallback, view_separate_callback, NULL); 
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(view_combine_separate_menu, false);

  view_combine_combined_menu = XtCreateManagedWidget("combined", xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_combined_menu, XmNactivateCallback, view_combined_callback, NULL);  
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(view_combine_combined_menu, false);

  view_combine_superimposed_menu = XtCreateManagedWidget("superimposed", xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_superimposed_menu, XmNactivateCallback, view_superimposed_callback, NULL);  
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(view_combine_superimposed_menu, false);

  view_zero_menu = XtCreateManagedWidget("Show y = 0", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_zero_menu, XmNactivateCallback, view_zero_callback, NULL);
  XtVaSetValues(view_zero_menu, XmNmnemonic, 'y', NULL);

  view_x_axis_menu = XmCreatePulldownMenu(view_menu, (char *)"xaxis", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_x_axis_menu); k++;
  view_x_axis_cascade_menu = XtCreateManagedWidget("X axis units", xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_x_axis_seconds_menu = XtCreateManagedWidget("seconds", xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_seconds_menu, XmNactivateCallback, view_x_axis_seconds_callback, NULL);  
  set_sensitive(view_x_axis_seconds_menu, false);

  view_x_axis_samples_menu = XtCreateManagedWidget("samples", xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_samples_menu, XmNactivateCallback, view_x_axis_samples_callback, NULL);  

  view_x_axis_clock_menu = XtCreateManagedWidget("clock", xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_clock_menu, XmNactivateCallback, view_x_axis_clock_callback, NULL);  

  view_x_axis_percentage_menu = XtCreateManagedWidget("percentage", xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_percentage_menu, XmNactivateCallback, view_x_axis_percentage_callback, NULL);  

  view_x_axis_beats_menu = XtCreateManagedWidget("beats", xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_beats_menu, XmNactivateCallback, view_x_axis_beats_callback, NULL);  

  view_x_axis_measures_menu = XtCreateManagedWidget("measures", xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_measures_menu, XmNactivateCallback, view_x_axis_measures_callback, NULL);  


  view_axes_menu = XmCreatePulldownMenu(view_menu, (char *)"axes", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_axes_menu); k++;
  view_axes_cascade_menu = XtCreateManagedWidget(I_AXIS_LAYOUT, xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_no_axes_menu = XtCreateManagedWidget("no axes", xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_no_axes_menu, XmNactivateCallback, view_no_axes_callback, NULL);  
  if (show_axes(ss) == SHOW_NO_AXES) set_sensitive(view_no_axes_menu, false); /* false because it is already chosen */

  view_all_axes_menu = XtCreateManagedWidget("both axes", xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_all_axes_menu, XmNactivateCallback, view_all_axes_callback, NULL);  
  if (show_axes(ss) == SHOW_ALL_AXES) set_sensitive(view_all_axes_menu, false);

  view_just_x_axis_menu = XtCreateManagedWidget("just x axis", xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_just_x_axis_menu, XmNactivateCallback, view_just_x_axis_callback, NULL);  
  if (show_axes(ss) == SHOW_X_AXIS) set_sensitive(view_just_x_axis_menu, false);

  view_all_axes_unlabelled_menu = XtCreateManagedWidget("both axes, no labels", xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_all_axes_unlabelled_menu, XmNactivateCallback, view_all_axes_unlabelled_callback, NULL);  
  if (show_axes(ss) == SHOW_ALL_AXES_UNLABELLED) set_sensitive(view_all_axes_unlabelled_menu, false);

  view_just_x_axis_unlabelled_menu = XtCreateManagedWidget("just x axis, no label", xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_just_x_axis_unlabelled_menu, XmNactivateCallback, view_just_x_axis_unlabelled_callback, NULL);  
  if (show_axes(ss) == SHOW_X_AXIS_UNLABELLED) set_sensitive(view_just_x_axis_unlabelled_menu, false);

  view_bare_x_axis_menu = XtCreateManagedWidget("bare x axis", xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_bare_x_axis_menu, XmNactivateCallback, view_bare_x_axis_callback, NULL);  
  if (show_axes(ss) == SHOW_BARE_X_AXIS) set_sensitive(view_bare_x_axis_menu, false);

  view_focus_style_menu = XmCreatePulldownMenu(view_menu, (char *)"focusstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_focus_style_menu); k++;
  view_focus_cascade_menu = XtCreateManagedWidget(I_ZOOM_CENTERS_ON, xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_focus_left_menu = XtCreateManagedWidget("window left edge", xmPushButtonWidgetClass, view_focus_style_menu, main_args, main_n);
  XtAddCallback(view_focus_left_menu, XmNactivateCallback, view_focus_left_callback, NULL);  

  view_focus_right_menu = XtCreateManagedWidget("window right edge", xmPushButtonWidgetClass, view_focus_style_menu, main_args, main_n);
  XtAddCallback(view_focus_right_menu, XmNactivateCallback, view_focus_right_callback, NULL);  

  view_focus_middle_menu = XtCreateManagedWidget("window midpoint", xmPushButtonWidgetClass, view_focus_style_menu, main_args, main_n);
  XtAddCallback(view_focus_middle_menu, XmNactivateCallback, view_focus_middle_callback, NULL);  

  view_focus_active_menu = XtCreateManagedWidget("cursor or selection", xmPushButtonWidgetClass, view_focus_style_menu, main_args, main_n);
  XtAddCallback(view_focus_active_menu, XmNactivateCallback, view_focus_active_callback, NULL);  


  view_grid_menu = XtCreateManagedWidget("With grid", xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_grid_menu, XmNactivateCallback, view_grid_callback, NULL);


  /* OPTIONS MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)3);
  options_menu = XmCreatePulldownMenu(main_menu, (char *)"Option", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, options_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'O'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)3); high_n++;
  options_cascade_menu = XtCreateManagedWidget("Options", xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  options_transform_menu = XtCreateManagedWidget("Transform options", xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_transform_menu, XmNactivateCallback, options_transform_callback, NULL);
  XtVaSetValues(options_transform_menu, XmNmnemonic, 't', NULL);

  options_controls_menu = XtCreateManagedWidget("Control panel options", xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_controls_menu, XmNactivateCallback, options_controls_callback, NULL);
  XtVaSetValues(options_controls_menu, XmNmnemonic, 'c', NULL);


#if HAVE_EXTENSION_LANGUAGE
  options_save_state_menu = XtCreateManagedWidget("Save session", xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_save_state_menu, XmNactivateCallback, options_save_state_callback, NULL);
#endif

  options_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, options_menu, sep_args, j);

  options_preferences_menu = XtCreateManagedWidget("Preferences", xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_preferences_menu, XmNactivateCallback, options_preferences_callback, NULL);


  /* HELP MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)4);
  help_menu = XmCreatePulldownMenu(main_menu, (char *)I_HELP, main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, help_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'H'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)4); high_n++;
  help_cascade_menu = XtCreateManagedWidget(I_HELP, xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  help_about_snd_menu = XtCreateManagedWidget("About Snd", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_about_snd_menu, XmNactivateCallback, help_about_snd_callback, NULL);
  XtVaSetValues(help_about_snd_menu, XmNmnemonic, 'O', NULL);

#if HAVE_EXTENSION_LANGUAGE
  help_init_file_menu = XtCreateManagedWidget("Customization", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_init_file_menu, XmNactivateCallback, help_init_file_callback, NULL);
#endif

  help_controls_menu = XtCreateManagedWidget("Control panel", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_controls_menu, XmNactivateCallback, help_controls_callback, NULL);

  help_keys_menu = XtCreateManagedWidget("Key bindings", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_keys_menu, XmNactivateCallback, help_keys_callback, NULL);

  help_play_menu = XtCreateManagedWidget("Play", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_play_menu, XmNactivateCallback, help_play_callback, NULL);

  help_save_menu = XtCreateManagedWidget("Save", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_save_menu, XmNactivateCallback, help_save_callback, NULL);

  help_mix_menu = XtCreateManagedWidget("Mix", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_mix_menu, XmNactivateCallback, help_mix_callback, NULL);

  help_resample_menu = XtCreateManagedWidget("Resample", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_resample_menu, XmNactivateCallback, help_resample_callback, NULL);

  help_fft_menu = XtCreateManagedWidget("FFT", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_fft_menu, XmNactivateCallback, help_fft_callback, NULL);

  help_filter_menu = XtCreateManagedWidget("Filter", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_filter_menu, XmNactivateCallback, help_filter_callback, NULL);

  help_reverb_menu = XtCreateManagedWidget("Reverb", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_reverb_menu, XmNactivateCallback, help_reverb_callback, NULL);

  help_env_menu = XtCreateManagedWidget("Envelope", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_env_menu, XmNactivateCallback, help_env_callback, NULL);

  help_marks_menu = XtCreateManagedWidget("Mark", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_marks_menu, XmNactivateCallback, help_marks_callback, NULL);

  help_insert_menu = XtCreateManagedWidget("Insert", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_insert_menu, XmNactivateCallback, help_insert_callback, NULL);

  help_delete_menu = XtCreateManagedWidget("Delete", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_delete_menu, XmNactivateCallback, help_delete_callback, NULL);

  help_undo_menu = XtCreateManagedWidget("Undo and redo", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_undo_menu, XmNactivateCallback, help_undo_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  help_find_menu = XtCreateManagedWidget("Search", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_find_menu, XmNactivateCallback, help_find_callback, NULL);
#endif

  help_sync_menu = XtCreateManagedWidget("Sync and unite", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_sync_menu, XmNactivateCallback, help_sync_callback, NULL);

  help_sound_files_menu = XtCreateManagedWidget("Headers and data", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_sound_files_menu, XmNactivateCallback, help_sound_files_callback, NULL);

  help_debug_menu = XtCreateManagedWidget("Debugging", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_debug_menu, XmNactivateCallback, help_debug_callback, NULL);

  help_region_menu = XtCreateManagedWidget("Regions", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_region_menu, XmNactivateCallback, help_region_callback, NULL);

  help_selection_menu = XtCreateManagedWidget("Selections", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_selection_menu, XmNactivateCallback, help_selection_callback, NULL);

  help_colors_menu = XtCreateManagedWidget("Colors", xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_colors_menu, XmNactivateCallback, help_colors_callback, NULL);

  XtVaSetValues(main_menu, XmNmenuHelpWidget, help_cascade_menu, NULL);

  XtAddCallback(file_cascade_menu, XmNcascadingCallback, file_menu_update_1, NULL);
  XtAddCallback(edit_cascade_menu, XmNcascadingCallback, edit_menu_update_1, NULL);
  XtAddCallback(view_cascade_menu, XmNcascadingCallback, view_menu_update_1, NULL);

  XtManageChild(main_menu);
  return(main_menu);
}


/* -------------------------------- POPUP MENU -------------------------------- */

static Widget basic_popup_menu = NULL, selection_popup_menu = NULL, fft_popup_menu = NULL;

static Widget add_menu_item(Widget menu, const char *label, void (*callback)(Widget w, XtPointer info, XtPointer context))
{
  Arg args[20];
  int n;
  Widget w;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
  w = XtCreateManagedWidget(label, xmPushButtonWidgetClass, menu, args, n);
  XtAddCallback(w, XmNactivateCallback, callback, NULL);
  return(w);
}

static void popup_info_callback(Widget w, XtPointer info, XtPointer context) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) display_info(sp);
}

static void popup_normalize_callback(Widget w, XtPointer info, XtPointer context) 
{
  mus_float_t scl[1];
  scl[0] = 1.0;
  scale_to(any_selected_sound(), current_channel(), scl, 1, OVER_SOUND);
}


static void popup_reverse_callback(Widget w, XtPointer info, XtPointer context) 
{
  reverse_sound(current_channel(), OVER_SOUND, C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), 0);
}


static void stop_everything_callback(Widget w, XtPointer info, XtPointer context) 
{
  control_g(any_selected_sound());
}


void post_basic_popup_menu(void *e)
{
  XButtonPressedEvent *event = (XButtonPressedEvent *)e;
  if (!basic_popup_menu)
    {
      Arg args[20];
      int n;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNpopupEnabled, false); n++;      /* this was XmPOPUP_AUTOMATIC_RECURSIVE */
      basic_popup_menu = XmCreatePopupMenu(MAIN_PANE(ss), (char *)"basic-popup-menu", args, n);

      add_menu_item(basic_popup_menu, "Info",           popup_info_callback);
      add_menu_item(basic_popup_menu, "Select all",     edit_select_all_callback);
      add_menu_item(basic_popup_menu, "Stop!",          stop_everything_callback);
      add_menu_item(basic_popup_menu, "-> 1.0",         popup_normalize_callback);
      add_menu_item(basic_popup_menu, "Reverse",        popup_reverse_callback);
    }

  XmMenuPosition(basic_popup_menu, event);
  XtManageChild(basic_popup_menu);
}


/* -------- selection popup -------- */

static void popup_show_selection_callback(Widget w, XtPointer info, XtPointer context) 
{
  show_selection();
}

static void popup_zero_selection_callback(Widget w, XtPointer info, XtPointer context) 
{
  mus_float_t scl[1];
  scl[0] = 0.0;
  scale_by(NULL, scl, 1, OVER_SELECTION);
}

static void popup_normalize_selection_callback(Widget w, XtPointer info, XtPointer context) 
{
  mus_float_t scl[1];
  scl[0] = 1.0;
  scale_to(NULL, NULL, scl, 1, OVER_SELECTION);
}

static void popup_error_handler(const char *msg, void *data)
{
  redirect_snd_error_to(NULL, NULL);
  redirect_snd_warning_to(NULL, NULL);
  status_report(any_selected_sound(), "%s: %s", (char *)data, msg);
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

static void popup_cut_to_new_callback(Widget w, XtPointer info, XtPointer context) {popup_cut_to_new_callback_1(true);}
static void popup_copy_to_new_callback(Widget w, XtPointer info, XtPointer context) {popup_cut_to_new_callback_1(false);}

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

static void popup_crop_callback(Widget w, XtPointer info, XtPointer context)
{
  for_each_chan(crop);
}


static void popup_cut_and_smooth_callback(Widget w, XtPointer info, XtPointer context)
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

static void popup_mark_selection_callback(Widget w, XtPointer info, XtPointer context)
{
  for_each_chan(mark_selection);
}

static void popup_reverse_selection_callback(Widget w, XtPointer info, XtPointer context)
{
  reverse_sound(current_channel(), OVER_SELECTION, C_int_to_Xen_integer(AT_CURRENT_EDIT_POSITION), 0);
}

static mus_float_t selection_max = 0.0;

static void selection_info(chan_info *cp)
{
  if ((selection_is_active_in_channel(cp)) &&
      (selection_maxamp(cp) > selection_max))
    selection_max = selection_maxamp(cp);
}

static void popup_selection_info_callback(Widget w, XtPointer info, XtPointer context)
{
  selection_max = 0.0;
  for_each_chan(selection_info);
  status_report(any_selected_sound(), "selection max: %f", selection_max);
}

#if WITH_AUDIO
static void popup_loop_play_callback(Widget w, XtPointer info, XtPointer context) 
{
  if (ss->selection_play_stop)
    {
      stop_playing_all_sounds(PLAY_BUTTON_UNSET);
      reflect_play_selection_stop();
    }
  else
    {
      set_menu_label(edit_play_menu, I_STOP);
      ss->selection_play_stop = true;
      loop_play_selection();
    }
}
#endif


void post_selection_popup_menu(void *e) 
{
  XButtonPressedEvent *event = (XButtonPressedEvent *)e;
  if (!selection_popup_menu)
    {
      Arg args[20];
      int n;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNpopupEnabled, false); n++;      /* this was XmPOPUP_AUTOMATIC_RECURSIVE */
      selection_popup_menu = XmCreatePopupMenu(MAIN_PANE(ss), (char *)"selection-popup-menu", args, n);

      add_menu_item(selection_popup_menu, "Fill window",    popup_show_selection_callback);
      add_menu_item(selection_popup_menu, "Cut",            edit_cut_callback);
      add_menu_item(selection_popup_menu, "Cut and smooth", popup_cut_and_smooth_callback);
      add_menu_item(selection_popup_menu, "Cut -> new",     popup_cut_to_new_callback);
      add_menu_item(selection_popup_menu, "Save as",        edit_save_as_callback);
#if WITH_AUDIO
      add_menu_item(selection_popup_menu, "Play",           edit_play_callback);
      add_menu_item(selection_popup_menu, "Play looping",   popup_loop_play_callback);
#endif
      add_menu_item(selection_popup_menu, "Crop",           popup_crop_callback);
      add_menu_item(selection_popup_menu, "Unselect all",   edit_unselect_callback);
      add_menu_item(selection_popup_menu, "-> 0.0",         popup_zero_selection_callback);
      add_menu_item(selection_popup_menu, "-> 1.0",         popup_normalize_selection_callback);
      add_menu_item(selection_popup_menu, "Copy -> new",    popup_copy_to_new_callback);
      add_menu_item(selection_popup_menu, "Paste",          edit_paste_callback);
      add_menu_item(selection_popup_menu, "Mix",            edit_mix_callback);
      add_menu_item(selection_popup_menu, "Mark",           popup_mark_selection_callback);
      add_menu_item(selection_popup_menu, "Reverse",        popup_reverse_selection_callback);
      add_menu_item(selection_popup_menu, "Info",           popup_selection_info_callback);
    }

  XmMenuPosition(selection_popup_menu, event);
  XtManageChild(selection_popup_menu);
}


/* -------- fft popup -------- */

static void popup_peaks_callback(Widget w, XtPointer info, XtPointer context) 
{
  FILE *peaks_fd;
  peaks_fd = FOPEN("fft.txt", "w");
  if (peaks_fd)
    {
      write_transform_peaks(peaks_fd, current_channel()); /* follows sync */
      fclose(peaks_fd);
    }
}

static void fft_size_16_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(16);}
static void fft_size_64_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(64);}
static void fft_size_256_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(256);}
static void fft_size_1024_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(1024);}
static void fft_size_4096_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(4096);}
static void fft_size_16384_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(16384);}
static void fft_size_65536_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(65536);}
static void fft_size_262144_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(262144);}
static void fft_size_1048576_callback(Widget w, XtPointer info, XtPointer context) {set_transform_size(1048576);}


static void fft_window_rectangular_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_RECTANGULAR_WINDOW);}
static void fft_window_hann_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_HANN_WINDOW);}
static void fft_window_welch_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_WELCH_WINDOW);}
static void fft_window_parzen_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_PARZEN_WINDOW);}
static void fft_window_bartlett_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_BARTLETT_WINDOW);}
static void fft_window_blackman2_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_BLACKMAN2_WINDOW);}
static void fft_window_blackman3_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_BLACKMAN3_WINDOW);}
static void fft_window_blackman4_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_BLACKMAN4_WINDOW);}
static void fft_window_hamming_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_HAMMING_WINDOW);}
static void fft_window_exponential_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_EXPONENTIAL_WINDOW);}
static void fft_window_riemann_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_RIEMANN_WINDOW);}
static void fft_window_kaiser_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_KAISER_WINDOW);}
static void fft_window_cauchy_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_CAUCHY_WINDOW);}
static void fft_window_poisson_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_POISSON_WINDOW);}
static void fft_window_gaussian_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_GAUSSIAN_WINDOW);}
static void fft_window_tukey_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_TUKEY_WINDOW);}
static void fft_window_dolph_chebyshev_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_DOLPH_CHEBYSHEV_WINDOW);}
static void fft_window_blackman6_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_BLACKMAN6_WINDOW);}
static void fft_window_blackman8_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_BLACKMAN8_WINDOW);}
static void fft_window_blackman10_callback(Widget w, XtPointer info, XtPointer context) {set_fft_window(MUS_BLACKMAN10_WINDOW);}

static void fft_type_fourier_callback(Widget w, XtPointer info, XtPointer context) {set_transform_type(FOURIER);}
static void fft_type_wavelet_callback(Widget w, XtPointer info, XtPointer context) {set_transform_type(WAVELET);}
static void fft_type_autocorrelation_callback(Widget w, XtPointer info, XtPointer context) {set_transform_type(AUTOCORRELATION);}
static void fft_type_cepstrum_callback(Widget w, XtPointer info, XtPointer context) {set_transform_type(CEPSTRUM);}


static void fft_graph_once_callback(Widget w, XtPointer info, XtPointer context) {set_transform_graph_type(GRAPH_ONCE);}
static void fft_graph_sonogram_callback(Widget w, XtPointer info, XtPointer context) {set_transform_graph_type(GRAPH_AS_SONOGRAM);}
static void fft_graph_spectrogram_callback(Widget w, XtPointer info, XtPointer context) {set_transform_graph_type(GRAPH_AS_SPECTROGRAM);}


static void fft_gray_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(GRAY_COLORMAP);}
static void fft_hot_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(HOT_COLORMAP);}
static void fft_cool_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(COOL_COLORMAP);}
static void fft_bone_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(BONE_COLORMAP);}
static void fft_copper_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(COPPER_COLORMAP);}
static void fft_pink_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(PINK_COLORMAP);}
static void fft_jet_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(JET_COLORMAP);}
static void fft_prism_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(PRISM_COLORMAP);}
static void fft_autumn_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(AUTUMN_COLORMAP);}
static void fft_winter_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(WINTER_COLORMAP);}
static void fft_spring_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(SPRING_COLORMAP);}
static void fft_summer_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(SUMMER_COLORMAP);}
static void fft_rainbow_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(RAINBOW_COLORMAP);}
static void fft_flag_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(FLAG_COLORMAP);}
static void fft_phases_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(PHASES_COLORMAP);}
static void fft_black_and_white_callback(Widget w, XtPointer info, XtPointer context) {set_color_map(BLACK_AND_WHITE_COLORMAP);}

void post_fft_popup_menu(void *e)
{
  XButtonPressedEvent *event = (XButtonPressedEvent *)e;
  if (!fft_popup_menu)
    {
      Widget outer_menu;
      Arg args[20];
      int n;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNpopupEnabled, false); n++;      /* this was XmPOPUP_AUTOMATIC_RECURSIVE */
      fft_popup_menu = XmCreatePopupMenu(MAIN_PANE(ss), (char *)"fft-popup-menu", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      outer_menu = XmCreatePulldownMenu(fft_popup_menu, (char *)"Size", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNsubMenuId, outer_menu); n++;
      XtCreateManagedWidget("Size", xmCascadeButtonWidgetClass, fft_popup_menu, args, n);

      add_menu_item(outer_menu, "16",      fft_size_16_callback);
      add_menu_item(outer_menu, "64",      fft_size_64_callback);
      add_menu_item(outer_menu, "256",     fft_size_256_callback);
      add_menu_item(outer_menu, "1024",    fft_size_1024_callback);
      add_menu_item(outer_menu, "4096",    fft_size_4096_callback);
      add_menu_item(outer_menu, "16384",   fft_size_16384_callback);
      add_menu_item(outer_menu, "65536",   fft_size_65536_callback);
      add_menu_item(outer_menu, "262144",  fft_size_262144_callback);
      add_menu_item(outer_menu, "1048576", fft_size_1048576_callback);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      outer_menu = XmCreatePulldownMenu(fft_popup_menu, (char *)"Window", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNsubMenuId, outer_menu); n++;
      XtCreateManagedWidget("Window", xmCascadeButtonWidgetClass, fft_popup_menu, args, n);

      add_menu_item(outer_menu, "rectangular",     fft_window_rectangular_callback);
      add_menu_item(outer_menu, "hann",            fft_window_hann_callback);
      add_menu_item(outer_menu, "welch",           fft_window_welch_callback);
      add_menu_item(outer_menu, "parzen",          fft_window_parzen_callback);
      add_menu_item(outer_menu, "bartlett",        fft_window_bartlett_callback);
      add_menu_item(outer_menu, "hamming",         fft_window_hamming_callback);
      add_menu_item(outer_menu, "blackman2",       fft_window_blackman2_callback);
      add_menu_item(outer_menu, "blackman3",       fft_window_blackman3_callback);
      add_menu_item(outer_menu, "blackman4",       fft_window_blackman4_callback);
      add_menu_item(outer_menu, "exponential",     fft_window_exponential_callback);
      add_menu_item(outer_menu, "riemann",         fft_window_riemann_callback);
      add_menu_item(outer_menu, "kaiser",          fft_window_kaiser_callback);
      add_menu_item(outer_menu, "cauchy",          fft_window_cauchy_callback);
      add_menu_item(outer_menu, "poisson",         fft_window_poisson_callback);
      add_menu_item(outer_menu, "gaussian",        fft_window_gaussian_callback);
      add_menu_item(outer_menu, "tukey",           fft_window_tukey_callback);
      add_menu_item(outer_menu, "dolph-chebyshev", fft_window_dolph_chebyshev_callback);
      add_menu_item(outer_menu, "blackman6",       fft_window_blackman6_callback);
      add_menu_item(outer_menu, "blackman8",       fft_window_blackman8_callback);
      add_menu_item(outer_menu, "blackman10" ,     fft_window_blackman10_callback);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      outer_menu = XmCreatePulldownMenu(fft_popup_menu, (char *)"Graph type", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNsubMenuId, outer_menu); n++;
      XtCreateManagedWidget("Graph type", xmCascadeButtonWidgetClass, fft_popup_menu, args, n);

      add_menu_item(outer_menu, "one fft",     fft_graph_once_callback);
      add_menu_item(outer_menu, "sonogram",    fft_graph_sonogram_callback);
      add_menu_item(outer_menu, "spectrogram", fft_graph_spectrogram_callback);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      outer_menu = XmCreatePulldownMenu(fft_popup_menu, (char *)"Transform type", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNsubMenuId, outer_menu); n++;
      XtCreateManagedWidget("Transform type", xmCascadeButtonWidgetClass, fft_popup_menu, args, n);

      add_menu_item(outer_menu, "fourier",         fft_type_fourier_callback);
      add_menu_item(outer_menu, "wavelet",         fft_type_wavelet_callback);
      add_menu_item(outer_menu, "autocorrelation", fft_type_autocorrelation_callback);
      add_menu_item(outer_menu, "cepstrum",        fft_type_cepstrum_callback);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      outer_menu = XmCreatePulldownMenu(fft_popup_menu, (char *)"Colormap", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNsubMenuId, outer_menu); n++;
      XtCreateManagedWidget("Colormap", xmCascadeButtonWidgetClass, fft_popup_menu, args, n);

      add_menu_item(outer_menu, "gray",    fft_gray_callback);
      add_menu_item(outer_menu, "autumn",  fft_autumn_callback);
      add_menu_item(outer_menu, "spring",  fft_spring_callback);
      add_menu_item(outer_menu, "winter",  fft_winter_callback);
      add_menu_item(outer_menu, "summer",  fft_summer_callback);
      add_menu_item(outer_menu, "cool",    fft_cool_callback);
      add_menu_item(outer_menu, "copper",  fft_copper_callback);
      add_menu_item(outer_menu, "flag",    fft_flag_callback);
      add_menu_item(outer_menu, "prism",   fft_prism_callback);
      add_menu_item(outer_menu, "bone",    fft_bone_callback);
      add_menu_item(outer_menu, "hot",     fft_hot_callback);
      add_menu_item(outer_menu, "jet",     fft_jet_callback);
      add_menu_item(outer_menu, "pink",    fft_pink_callback);
      add_menu_item(outer_menu, "rainbow", fft_rainbow_callback);
      add_menu_item(outer_menu, "phases",  fft_phases_callback);
      add_menu_item(outer_menu, "black and white", fft_black_and_white_callback);


      add_menu_item(fft_popup_menu, "Peaks->fft.txt", popup_peaks_callback);
    }

  XmMenuPosition(fft_popup_menu, event);
  XtManageChild(fft_popup_menu);
}



void post_lisp_popup_menu(void *e) {}




/* ---------------- tooltips ---------------- */

static Widget tooltip_shell = NULL;
static Widget tooltip_label = NULL;
static timeout_result_t tool_proc = 0, quit_proc = 0;
static Time tool_last_time = 0;
static Position tool_x, tool_y;
static Widget tool_w;

static void leave_tooltip(XtPointer tooltip, XtIntervalId *id)
{
  XtUnmanageChild(tooltip_shell);
  quit_proc = 0;
}


static void handle_tooltip(XtPointer tooltip, XtIntervalId *id)
{
  char *tip = (char *)tooltip;
  Position rx, ry;
  XmString str;
  int lines = 0;

  if (!tooltip_shell)
    {
      tooltip_shell = XtVaCreatePopupShell(tip, overrideShellWidgetClass, MAIN_SHELL(ss), 
					   XmNallowShellResize, true, 
					   NULL);
      tooltip_label = XtVaCreateManagedWidget("tooltip", xmLabelWidgetClass, tooltip_shell,
					      XmNrecomputeSize, true,
					      XmNbackground, ss->lighter_blue,
					      NULL);
    }
  str = multi_line_label(tip, &lines);
  XtVaSetValues(tooltip_label, XmNlabelString, str, NULL);
  XmStringFree(str);

  XtTranslateCoords(tool_w, tool_x, tool_y, &rx, &ry);
  XtVaSetValues(tooltip_shell, XmNx, rx, XmNy, ry, NULL);
  XtManageChild(tooltip_shell);
  quit_proc = XtAppAddTimeOut(MAIN_APP(ss), (unsigned long)10000, (XtTimerCallbackProc)leave_tooltip, NULL);
}


static void tool_starter(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XEnterWindowEvent *ev = (XEnterWindowEvent *)event;
  char *tip = (char *)context;
  if ((with_tooltips(ss)) &&
      ((ev->time - tool_last_time) > 2))
    {
      tool_x = ev->x;
      tool_y = ev->y;
      tool_w = w;
      tool_last_time = ev->time;
      tool_proc = XtAppAddTimeOut(MAIN_APP(ss), (unsigned long)300, (XtTimerCallbackProc)handle_tooltip, (XtPointer)tip);
    }
}


static void tool_stopper(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XLeaveWindowEvent *ev = (XLeaveWindowEvent *)event;
  tool_last_time = ev->time;
  if (tool_proc != 0)
    {
      XtRemoveTimeOut(tool_proc);
      tool_proc = 0;
    }
  if (quit_proc != 0)
    {
      XtRemoveTimeOut(quit_proc);
      quit_proc = 0;
    }
  if ((tooltip_shell) && (XtIsManaged(tooltip_shell)))
    XtUnmanageChild(tooltip_shell);
}


void add_tooltip(Widget w, const char *tip)
{
  XtAddEventHandler(w, EnterWindowMask, false, tool_starter, (XtPointer)tip);
  XtAddEventHandler(w, LeaveWindowMask, false, tool_stopper, NULL);
}



/* ---------------- toolbar ---------------- */

static void add_to_toolbar(Widget bar, Pixmap icon, const char *tip, void (*callback)(Widget w, XtPointer info, XtPointer context))
{
  Widget w;
  w = XtVaCreateManagedWidget("icon", xmPushButtonWidgetClass, bar,
			      XmNlabelPixmap, icon,
			      XmNlabelType, XmPIXMAP,
			      XmNwidth, 24,
			      XmNheight, 24,
			      XmNshadowThickness, 0,
			      XmNhighlightThickness, 0,
			      /* XmNmarginHeight, 0, */
			      XmNbackground, ss->basic_color,
			      NULL);
  XtAddCallback(w, XmNactivateCallback, callback, NULL);
  add_tooltip(w, tip);
}


static void add_separator_to_toolbar(Widget bar)
{
  XtVaCreateManagedWidget("icon", xmPushButtonWidgetClass, bar,
			  XmNlabelPixmap, toolbar_icon(SND_XPM_SEPARATOR),
			  XmNlabelType, XmPIXMAP,
			  XmNwidth, 8,
			  XmNheight, 24,
			  XmNshadowThickness, 0,
			  XmNhighlightThickness, 0,
			  XmNbackground, ss->basic_color,
			  NULL);
}


#if WITH_AUDIO
static void play_from_start_callback(Widget w, XtPointer info, XtPointer context) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp)
    play_sound(sp, 0, NO_END_SPECIFIED);
}


static void play_from_cursor_callback(Widget w, XtPointer info, XtPointer context) 
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


static void stop_playing_callback(Widget w, XtPointer info, XtPointer context) 
{
  stop_playing_all_sounds(PLAY_C_G);
  reflect_play_selection_stop(); /* this sets ss->selection_play_stop = false; */
}
#endif


static void full_dur_callback(Widget w, XtPointer info, XtPointer context) 
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


static void zoom_out_callback(Widget w, XtPointer info, XtPointer context) 
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


static void zoom_in_callback(Widget w, XtPointer info, XtPointer context) 
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


static void goto_start_callback(Widget w, XtPointer info, XtPointer context) 
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

static void go_back_callback(Widget w, XtPointer info, XtPointer context) 
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


static void go_forward_callback(Widget w, XtPointer info, XtPointer context) 
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

static void goto_end_callback(Widget w, XtPointer info, XtPointer context) 
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



static Widget toolbar = NULL;

void show_toolbar(void)
{
  if (!toolbar)
    {
      #define ICON_HEIGHT 28
      Arg args[32];
      int n;

      XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, false, NULL);

      n = attach_all_sides(args, 0);
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNheight, ICON_HEIGHT); n++;
      XtSetArg(args[n], XmNpaneMaximum, ICON_HEIGHT); n++; /* Xm/Paned initializes each pane max to 1000 apparently! */
      XtSetArg(args[n], XmNpositionIndex, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;

      if ((sound_style(ss) == SOUNDS_IN_NOTEBOOK) || 
	  (sound_style(ss) == SOUNDS_HORIZONTAL))
	toolbar = XtCreateManagedWidget("toolbar", xmRowColumnWidgetClass, SOUND_PANE_BOX(ss), args, n);
      else toolbar = XtCreateManagedWidget("toolbar", xmRowColumnWidgetClass, SOUND_PANE(ss), args, n);
      ss->toolbar = toolbar;

      if (auto_resize(ss))
	XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL);

      make_toolbar_icons(MAIN_SHELL(ss));

      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_NEW),           "new sound",                  file_new_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_OPEN),          "open sound",                 file_open_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_SAVE),          "save current sound, overwriting it", file_save_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_SAVE_AS),       "save current sound in a new file", file_save_as_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_REVERT),        "revert to saved",            file_revert_callback); 
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_UNDO),          "undo edit",                  edit_undo_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_REDO),          "redo last (undone) edit",    edit_redo_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_CLOSE),         "close selected sound",       file_close_callback);
      add_separator_to_toolbar(toolbar); 

#if WITH_AUDIO
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_PLAY),          "play from the start",        play_from_start_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_CURSOR_PLAY),   "play from the cursor",       play_from_cursor_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_STOP_PLAY),     "stop playing",               stop_playing_callback);      
      add_separator_to_toolbar(toolbar);
#endif
 
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_UP),            "show full sound",            full_dur_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_ZOOM_OUT),      "zoom out",                   zoom_out_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_ZOOM_IN),       "zoom in",                    zoom_in_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_BACK_ARROW),    "go to start of sound",       goto_start_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_BACK),          "go back a window",           go_back_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_NEXT),          "go forward a window",        go_forward_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_FORWARD_ARROW), "go to end of sound",         goto_end_callback);      
      add_separator_to_toolbar(toolbar);

      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_CUT),           "delete selection",           edit_cut_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_PASTE),         "insert selection at cursor", edit_paste_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_PREFERENCES),   "open preferences dialog",    options_preferences_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_STOP),          "stop the current operation", stop_everything_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_EXIT),          "exit Snd",                   file_exit_callback);

    }
  else
    {
      XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, false, NULL);
      XtManageChild(toolbar);
      if (auto_resize(ss))
	XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL);
    }
}


void hide_toolbar(void)
{
  if (toolbar)
    {
      XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, false, NULL);
      XtUnmanageChild(toolbar);
      if (auto_resize(ss))
	XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL);
    }
}




/* ---------------- ext lang tie-ins ---------------- */

#define INVALID_MENU -1
#define CALL_INDEX(Data) (Data >> 16)
#define PACK_MENU_DATA(Slot, Menu) ((Slot << 16) | (Menu))

static void menu_callback(Widget w, XtPointer info, XtPointer context) 
{
  pointer_or_int_t callb;
  XtVaGetValues(w, XmNuserData, &callb, NULL);
  g_menu_callback(CALL_INDEX(callb)); /* menu option activate callback */
}


static void GHC_callback(Widget w, XtPointer info, XtPointer context) 
{
  pointer_or_int_t slot;
  XtVaGetValues(w, XmNuserData, &slot, NULL);
  g_menu_callback(CALL_INDEX(slot)); /* main menu cascading callback */
}


static Widget *main_menus = NULL;
static int main_menus_size = 0;
/* fancy code here looping through the main menus children hangs or segfaults in 86_64 unoptimized cases! */

Widget menu_widget(int which_menu)
{
  switch (which_menu)
    {
    case 0: return(file_menu);    break;
    case 1: return(edit_menu);    break;
    case 2: return(view_menu);    break;
    case 3: return(options_menu); break;
    case 4: return(help_menu);    break;
      
    default:
      if (which_menu < main_menus_size)
	return(main_menus[which_menu]);
      break;
    }
  return(NULL);
}


#include <X11/IntrinsicP.h>

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
      (mus_strcmp(name, wname)) &&
      (XtIsManaged(w)))
    {
      pointer_or_int_t slot;
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

int g_add_to_main_menu(const char *label, int slot)
{
  static Arg args[12];
  Widget m, cas;
  int n;

  if (auto_resize(ss)) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, false, NULL);
  new_menu++;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(slot, new_menu)); n++;
  m = XmCreatePulldownMenu(main_menu, (char *)label, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
  XtSetArg(args[n], XmNsubMenuId, m); n++;
  XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(slot, new_menu)); n++;
  cas = XtCreateManagedWidget(label, xmCascadeButtonWidgetClass, main_menu, args, n);
  if (slot >= 0) XtAddCallback(cas, XmNcascadingCallback, GHC_callback, NULL);

  if (auto_resize(ss)) XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL);
  
  if (main_menus_size == 0)
    {
      main_menus_size = 8;
      main_menus = (Widget *)calloc(main_menus_size, sizeof(Widget));
    }
  else
    {
      if (new_menu >= main_menus_size)
	{
	  main_menus_size = new_menu + 8;
	  main_menus = (Widget *)realloc(main_menus, main_menus_size * sizeof(Widget));
	}
    }
  main_menus[new_menu] = m;
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
      /*   but close-all and open-recent should be left alone! */
      CompositeWidget cw = (CompositeWidget)menw;
      for (i = 0; i < cw->composite.num_children; i++)
	{
	  m = cw->composite.children[i];
	  if ((m) && 
	      (!(XtIsManaged(m))) &&
	      (m != file_close_all_menu) &&
	      (m != file_open_recent_menu) &&
	      (m != file_open_recent_cascade_menu))
	    {
	      if (!(mus_strcmp(XtName(m), label)))
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
      XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(callb, which_menu)); n++;
      m = XtCreateManagedWidget(label, xmPushButtonWidgetClass, menw, args, n);
      XtAddCallback(m, XmNactivateCallback, menu_callback, NULL);
    }
  else
    {
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
      m = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, menw, args, n);
    }
  return(m);
}


static Xen g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets "): a list of top level menu widgets: ((0)main (1)file (2)edit (3)view (4)options (5)help)"
  return(Xen_cons(Xen_wrap_widget(main_menu),
	  Xen_cons(Xen_wrap_widget(file_cascade_menu),
           Xen_cons(Xen_wrap_widget(edit_cascade_menu),
            Xen_cons(Xen_wrap_widget(view_cascade_menu),
             Xen_cons(Xen_wrap_widget(options_cascade_menu),
              Xen_cons(Xen_wrap_widget(help_cascade_menu),
	       Xen_empty_list)))))));
}


Xen_wrap_no_args(g_menu_widgets_w, g_menu_widgets)

void g_init_gxmenu(void)
{
  Xen_define_procedure(S_menu_widgets, g_menu_widgets_w, 0, 0, 0, H_menu_widgets);
}

/* Motif bug: the button backgrounds remain in the original highlight color? but the widget (if it is one) is not the child of any obvious widget
 */
