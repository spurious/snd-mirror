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
	      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;

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
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNpositionIndex, 1); n++;  /* just after "Open" menu */
  
  file_open_recent_menu = XmCreatePulldownMenu(file_menu, (char *)"open-recent", args, n);
	  
  XtSetArg(args[n], XmNsubMenuId, file_open_recent_menu); n++;

  file_open_recent_cascade_menu = XtCreateManagedWidget(_("Open recent"), xmCascadeButtonWidgetClass, file_menu, args, n);
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
static void file_record_callback(Widget w, XtPointer info, XtPointer context) {record_file();}
static void file_close_callback(Widget w, XtPointer info, XtPointer context) {if (any_selected_sound()) snd_close_file(any_selected_sound());}
static void file_close_all_callback(Widget w, XtPointer info, XtPointer context) {for_each_sound(snd_close_file);}
static void file_save_callback(Widget w, XtPointer info, XtPointer context) {if (any_selected_sound()) save_edits_with_prompt(any_selected_sound());}
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


static void edit_play_callback(Widget w, XtPointer info, XtPointer context) 
{
  if (ss->selection_play_stop)
    {
      stop_playing_all_sounds(PLAY_BUTTON_UNSET);
      reflect_play_selection_stop(); /* if there was an error, stop_playing might not remember to clear this */
    }
  else
    {
      set_menu_label(edit_play_menu, _("Stop"));
      ss->selection_play_stop = true;
      play_selection(IN_BACKGROUND);
    }
}


void reflect_play_selection_stop(void)
{
  set_menu_label(edit_play_menu, _("Play Selection"));
  ss->selection_play_stop = false;
}


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
  char *dirname;
  dirname = get_label(w);
  if (mus_strcmp(dirname, _("new viewer")))
    start_view_files_dialog(true, true); /* managed and empty (brand-new) */
  else view_files_start_dialog_with_title(dirname);
}


static void view_files_callback(Widget w, XtPointer info, XtPointer context) 
{
  int size;

  size = view_files_dialog_list_length();
  if (size == 0)
    {
      start_view_files_dialog(true, true); /* managed and empty (brand-new) */
    }
  else
    {
      int i;
      char **view_files_names;

      if ((XmIsPushButton(view_files_menu)) && /* autotest check */
	  (!view_files_cascade_menu))
	return;

      view_files_names = view_files_dialog_titles();
      view_files_names[size++] = mus_strdup(_("new viewer"));

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
	      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;

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
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNpositionIndex, pos); n++;
  
  view_files_menu = XmCreatePulldownMenu(view_menu, (char *)"view-files", args, n);
  XtSetArg(args[n], XmNsubMenuId, view_files_menu); n++;

  view_files_cascade_menu = XtCreateManagedWidget(_("Files"), xmCascadeButtonWidgetClass, view_menu, args, n);
  XtAddCallback(view_files_cascade_menu, XmNcascadingCallback, view_files_callback, NULL);
}


static void view_menu_update_1(Widget w, XtPointer info, XtPointer context) 
{
  if ((view_files_dialog_list_length() > 0) &&
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
static void view_cursor_callback(Widget w, XtPointer info, XtPointer context){set_verbose_cursor((!(verbose_cursor(ss))));}

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



/* -------------------------------- OPTIONS MENU -------------------------------- */

static void options_transform_callback(Widget w, XtPointer info, XtPointer context) {fire_up_transform_dialog(true);}
static void options_controls_callback(Widget w, XtPointer info, XtPointer context) {make_controls_dialog();}
#if HAVE_EXTENSION_LANGUAGE
static void options_save_callback(Widget w, XtPointer info, XtPointer context) {save_options_from_menu();}
#endif
#if HAVE_EXTENSION_LANGUAGE
static void options_save_state_callback(Widget w, XtPointer info, XtPointer context) {save_state_from_menu();}
#endif
static void options_preferences_callback(Widget w, XtPointer info, XtPointer context) {start_preferences_dialog();}



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
static void help_recording_callback(Widget w, XtPointer info, XtPointer context) {recording_help();}
static void help_keys_callback(Widget w, XtPointer info, XtPointer context) {key_binding_help();}
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
	  if (key == snd_K_f) set_label(file_open_menu, _("Open")); else
	  if (key == snd_K_s) set_label(file_save_menu, _("Save")); else
	  if (key == snd_K_q) set_label(file_mix_menu, _("Mix")); else
	  if (key == snd_K_i) set_label(file_insert_menu, _("Insert")); else
	  if (key == snd_K_u) set_label(edit_undo_menu, _("Undo")); else
	  if (key == snd_K_r) set_label(edit_redo_menu, _("Redo"));
	}
      else
	{
	  if (key == snd_K_k) set_label(file_close_menu, _("Close")); else
	  if (key == snd_K_i) set_label(edit_paste_menu, _("Insert Selection")); else	  
	  if (key == snd_K_q) set_label(edit_mix_menu, _("Mix Selection")); else	  
	  if (key == snd_K_p) set_label(edit_play_menu, _("Play Selection")); else	  
	  if (key == snd_K_w) set_label(edit_save_as_menu, _("Save Selection"));
	}
    }
  else 
    {
      if ((key == snd_K_s) && (state == snd_ControlMask))
	set_label(edit_find_menu, _("Find"));
    }
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
      XmChangeColor(w, ss->sgx->selection_color);
      free(new_title);
      break;

    case DRAG_LEAVE:
      reflect_file_change_in_title();
      XmChangeColor(w, ss->sgx->highlight_color);
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

  ss->sgx->mw = (Widget *)calloc(NUM_MENU_WIDGETS, sizeof(Widget));

  XtSetArg(main_args[main_n], XmNbackground, ss->sgx->basic_color); main_n++;
  XtSetArg(high_args[high_n], XmNbackground, ss->sgx->highlight_color); high_n++;
  XtSetArg(in_args[in_n], XmNbackground, ss->sgx->basic_color); in_n++;

  start_high_n = high_n;
  XtSetArg(in_args[in_n], XmNsensitive, false); in_n++;
  
  n = high_n;
  XtSetArg(high_args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(high_args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(high_args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(high_args[n], XmNrightAttachment, XmATTACH_FORM); n++;

#ifdef SND_AS_WIDGET
  main_menu = XtCreateWidget("mb", xmRowColumnWidgetClass, MAIN_PANE(ss), high_args, n);
#else
  main_menu = XmCreateMenuBar(MAIN_PANE(ss), (char *)"menuBar", high_args, n);
#endif

  /* FILE MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)0);
  file_menu = XmCreatePulldownMenu(main_menu, (char *)"File", main_args, main_n + 1);
  
  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, file_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'F'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)0); high_n++;
  file_cascade_menu = XtCreateManagedWidget(_("File"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  file_open_menu = XtCreateManagedWidget(_("Open"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_open_menu, XmNactivateCallback, file_open_callback, NULL);
  XtVaSetValues(file_open_menu, XmNmnemonic, 'O', NULL);

  file_close_menu = XtCreateManagedWidget(_("Close"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_close_menu, XmNactivateCallback, file_close_callback, NULL);
  XtVaSetValues(file_close_menu, XmNmnemonic, 'C', NULL);

  file_close_all_menu = XtCreateWidget(_("Close all"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_close_all_menu, XmNactivateCallback, file_close_all_callback, NULL);
  
  file_save_menu = XtCreateManagedWidget(_("Save"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_save_menu, XmNactivateCallback, file_save_callback, NULL);
  XtVaSetValues(file_save_menu, XmNmnemonic, 'S', NULL);
  
  file_save_as_menu = XtCreateManagedWidget(_("Save as"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_save_as_menu, XmNactivateCallback, file_save_as_callback, NULL);
  XtVaSetValues(file_save_as_menu, XmNmnemonic, 'a', NULL);
  
  file_revert_menu = XtCreateManagedWidget(_("Revert"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_revert_menu, XmNactivateCallback, file_revert_callback, NULL);
  XtVaSetValues(file_revert_menu, XmNmnemonic, 'R', NULL);
  
  file_mix_menu = XtCreateManagedWidget(_("Mix"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_mix_menu, XmNactivateCallback, file_mix_callback_1, NULL);
  XtVaSetValues(file_mix_menu, XmNmnemonic, 'M', NULL);

  file_insert_menu = XtCreateManagedWidget(_("Insert"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_insert_menu, XmNactivateCallback, file_insert_callback_1, NULL);
  XtVaSetValues(file_insert_menu, XmNmnemonic, 'I', NULL);

  file_update_menu = XtCreateManagedWidget(_("Update"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_update_menu, XmNactivateCallback, file_update_callback, NULL);
  XtVaSetValues(file_update_menu, XmNmnemonic, 'U', NULL);

  file_new_menu = XtCreateManagedWidget(_("New"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_new_menu, XmNactivateCallback, file_new_callback, NULL);
  XtVaSetValues(file_new_menu, XmNmnemonic, 'N', NULL);

  file_record_menu = XtCreateManagedWidget(_("Record"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_record_menu, XmNactivateCallback, file_record_callback, NULL);

  file_view_menu = XtCreateManagedWidget(_("View"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_view_menu, XmNactivateCallback, file_view_callback, NULL);
  XtVaSetValues(file_view_menu, XmNmnemonic, 'V', NULL);

  file_print_menu = XtCreateManagedWidget(_("Print"), xmPushButtonWidgetClass, file_menu, in_args, in_n);
  XtAddCallback(file_print_menu, XmNactivateCallback, file_print_callback_1, NULL);
  XtVaSetValues(file_print_menu, XmNmnemonic, 'P', NULL);

  j = 0;
  XtSetArg(sep_args[j], XmNbackground, ss->sgx->basic_color); j++;
  XtSetArg(sep_args[j], XmNseparatorType, XmSHADOW_ETCHED_IN); j++;
  file_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, file_menu, sep_args, j);

  file_exit_menu = XtCreateManagedWidget(_("Exit"), xmPushButtonWidgetClass, file_menu, main_args, main_n);
  XtAddCallback(file_exit_menu, XmNactivateCallback, file_exit_callback, NULL);
  XtVaSetValues(file_exit_menu, XmNmnemonic, 'E', NULL);


  /* EDIT MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)1);
  edit_menu = XmCreatePulldownMenu(main_menu, (char *)"Edit", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, edit_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'E'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)1); high_n++;
  edit_cascade_menu = XtCreateManagedWidget(_("Edit"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);
  
  edit_undo_menu = XtCreateManagedWidget(_("Undo"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_undo_menu, XmNactivateCallback, edit_undo_callback, NULL);
  XtVaSetValues(edit_undo_menu, XmNmnemonic, 'U', NULL);

  edit_redo_menu = XtCreateManagedWidget(_("Redo"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_redo_menu, XmNactivateCallback, edit_redo_callback, NULL);
  XtVaSetValues(edit_redo_menu, XmNmnemonic, 'R', NULL);

#if HAVE_EXTENSION_LANGUAGE
  edit_find_menu = XtCreateManagedWidget(_("Find"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_find_menu, XmNactivateCallback, edit_find_callback_1, NULL);
  XtVaSetValues(edit_find_menu, XmNmnemonic, 'F', NULL);
#endif

  edit_select_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, edit_menu, sep_args, j);

  edit_cut_menu = XtCreateManagedWidget(_("Delete Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_cut_menu, XmNactivateCallback, edit_cut_callback, NULL);
  XtVaSetValues(edit_cut_menu, XmNmnemonic, 'C', NULL);

  edit_paste_menu = XtCreateManagedWidget(_("Insert Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_paste_menu, XmNactivateCallback, edit_paste_callback, NULL);
  XtVaSetValues(edit_paste_menu, XmNmnemonic, 'P', NULL);

  edit_mix_menu = XtCreateManagedWidget(_("Mix Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_mix_menu, XmNactivateCallback, edit_mix_callback, NULL);
  XtVaSetValues(edit_mix_menu, XmNmnemonic, 'M', NULL);

  edit_play_menu = XtCreateManagedWidget(_("Play Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_play_menu, XmNactivateCallback, edit_play_callback, NULL);
  XtVaSetValues(edit_play_menu, XmNmnemonic, 'P', NULL);

  edit_save_as_menu = XtCreateManagedWidget(_("Save Selection"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_save_as_menu, XmNactivateCallback, edit_save_as_callback, NULL);
  XtVaSetValues(edit_save_as_menu, XmNmnemonic, 'S', NULL);

  edit_select_all_menu = XtCreateManagedWidget(_("Select all"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_select_all_menu, XmNactivateCallback, edit_select_all_callback, NULL);

  edit_unselect_menu = XtCreateManagedWidget(_("Unselect all"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_unselect_menu, XmNactivateCallback, edit_unselect_callback, NULL);

  edit_edit_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, edit_menu, sep_args, j);

  edit_env_menu = XtCreateManagedWidget(_("Edit Envelope"), xmPushButtonWidgetClass, edit_menu, main_args, main_n);
  XtAddCallback(edit_env_menu, XmNactivateCallback, edit_envelope_callback, NULL);
  XtVaSetValues(edit_env_menu, XmNmnemonic, 'E', NULL);

  edit_header_menu = XtCreateManagedWidget(_("Edit Header"), xmPushButtonWidgetClass, edit_menu, in_args, in_n);
  XtAddCallback(edit_header_menu, XmNactivateCallback, edit_header_callback_1, NULL);
  XtVaSetValues(edit_header_menu, XmNmnemonic, 'H', NULL);


  /* VIEW MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)2);
  view_menu = XmCreatePulldownMenu(main_menu, (char *)"View", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, view_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'V'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)2); high_n++;
  view_cascade_menu = XtCreateManagedWidget(_("View"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  view_controls_menu = XtCreateManagedWidget(_("Show controls"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_controls_menu, XmNactivateCallback, view_controls_callback, NULL);
  XtVaSetValues(view_controls_menu, XmNmnemonic, 'S', NULL);

#if HAVE_EXTENSION_LANGUAGE
  view_listener_menu = XtCreateManagedWidget(_("Open listener"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_listener_menu, XmNactivateCallback, view_listener_callback, NULL);
  XtVaSetValues(view_listener_menu, XmNmnemonic, 'L', NULL);
#endif

  view_files_menu = XtCreateManagedWidget(_("Files"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_files_menu, XmNactivateCallback, view_files_callback, NULL);
  XtVaSetValues(view_files_menu, XmNmnemonic, 'F', NULL);

  view_mix_dialog_menu = XtCreateManagedWidget(_("Mixes"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_mix_dialog_menu, XmNactivateCallback, view_mix_dialog_callback, NULL);

  view_region_menu = XtCreateManagedWidget(_("Regions"), xmPushButtonWidgetClass, view_menu, in_args, in_n);
  XtAddCallback(view_region_menu, XmNactivateCallback, view_region_callback_1, NULL);
  XtVaSetValues(view_region_menu, XmNmnemonic, 'R', NULL);

  view_color_orientation_menu = XtCreateManagedWidget(_("Color/Orientation"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_color_orientation_menu, XmNactivateCallback, view_color_orientation_callback_1, NULL);

  view_sep2_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, view_menu, sep_args, j);

  view_graph_style_menu = XmCreatePulldownMenu(view_menu, (char *)"graph-style", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_graph_style_menu); k++;
  view_graph_style_cascade_menu = XtCreateManagedWidget(_("Graph style"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_lines_menu = XtCreateManagedWidget(_("lines"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_lines_menu, XmNactivateCallback, view_lines_callback, NULL); 
  if (graph_style(ss) == GRAPH_LINES) set_sensitive(view_lines_menu, false);

  view_dots_menu = XtCreateManagedWidget(_("dots"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_dots_menu, XmNactivateCallback, view_dots_callback, NULL);  
  if (graph_style(ss) == GRAPH_DOTS) set_sensitive(view_dots_menu, false);

  view_filled_menu = XtCreateManagedWidget(_("filled"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_filled_menu, XmNactivateCallback, view_filled_callback, NULL);  
  if (graph_style(ss) == GRAPH_FILLED) set_sensitive(view_filled_menu, false);

  view_dots_and_lines_menu = XtCreateManagedWidget(_("dots and lines"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_dots_and_lines_menu, XmNactivateCallback, view_dots_and_lines_callback, NULL);  
  if (graph_style(ss) == GRAPH_DOTS_AND_LINES) set_sensitive(view_dots_and_lines_menu, false);

  view_lollipops_menu = XtCreateManagedWidget(_("lollipops"), xmPushButtonWidgetClass, view_graph_style_menu, main_args, main_n);
  XtAddCallback(view_lollipops_menu, XmNactivateCallback, view_lollipops_callback, NULL);  
  if (graph_style(ss) == GRAPH_LOLLIPOPS) set_sensitive(view_lollipops_menu, false);

  view_cursor_menu = XtCreateManagedWidget(_("Verbose cursor"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_cursor_menu, XmNactivateCallback, view_cursor_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  view_inset_menu = XtCreateManagedWidget(_("With inset graph"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_inset_menu, XmNactivateCallback, view_inset_callback, NULL);
#endif

  view_combine_menu = XmCreatePulldownMenu(view_menu, (char *)"combine", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_combine_menu); k++;
  view_combine_cascade_menu = XtCreateManagedWidget(_("Channel style"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_combine_separate_menu = XtCreateManagedWidget(_("separate"), xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_separate_menu, XmNactivateCallback, view_separate_callback, NULL); 
  if (channel_style(ss) == CHANNELS_SEPARATE) set_sensitive(view_combine_separate_menu, false);

  view_combine_combined_menu = XtCreateManagedWidget(_("combined"), xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_combined_menu, XmNactivateCallback, view_combined_callback, NULL);  
  if (channel_style(ss) == CHANNELS_COMBINED) set_sensitive(view_combine_combined_menu, false);

  view_combine_superimposed_menu = XtCreateManagedWidget(_("superimposed"), xmPushButtonWidgetClass, view_combine_menu, main_args, main_n);
  XtAddCallback(view_combine_superimposed_menu, XmNactivateCallback, view_superimposed_callback, NULL);  
  if (channel_style(ss) == CHANNELS_SUPERIMPOSED) set_sensitive(view_combine_superimposed_menu, false);

  view_zero_menu = XtCreateManagedWidget(_("Show Y = 0"), xmPushButtonWidgetClass, view_menu, main_args, main_n);
  XtAddCallback(view_zero_menu, XmNactivateCallback, view_zero_callback, NULL);
  XtVaSetValues(view_zero_menu, XmNmnemonic, 'y', NULL);

  view_x_axis_menu = XmCreatePulldownMenu(view_menu, (char *)"xaxis", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_x_axis_menu); k++;
  view_x_axis_cascade_menu = XtCreateManagedWidget(_("X axis units"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_x_axis_seconds_menu = XtCreateManagedWidget(_("seconds"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_seconds_menu, XmNactivateCallback, view_x_axis_seconds_callback, NULL);  
  set_sensitive(view_x_axis_seconds_menu, false);

  view_x_axis_samples_menu = XtCreateManagedWidget(_("samples"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_samples_menu, XmNactivateCallback, view_x_axis_samples_callback, NULL);  

  view_x_axis_clock_menu = XtCreateManagedWidget(_("clock"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_clock_menu, XmNactivateCallback, view_x_axis_clock_callback, NULL);  

  view_x_axis_percentage_menu = XtCreateManagedWidget(_("percentage"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_percentage_menu, XmNactivateCallback, view_x_axis_percentage_callback, NULL);  

  view_x_axis_beats_menu = XtCreateManagedWidget(_("beats"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_beats_menu, XmNactivateCallback, view_x_axis_beats_callback, NULL);  

  view_x_axis_measures_menu = XtCreateManagedWidget(_("measures"), xmPushButtonWidgetClass, view_x_axis_menu, main_args, main_n);
  XtAddCallback(view_x_axis_measures_menu, XmNactivateCallback, view_x_axis_measures_callback, NULL);  


  view_axes_menu = XmCreatePulldownMenu(view_menu, (char *)"axes", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_axes_menu); k++;
  view_axes_cascade_menu = XtCreateManagedWidget(_("Axes"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_no_axes_menu = XtCreateManagedWidget(_("no axes"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_no_axes_menu, XmNactivateCallback, view_no_axes_callback, NULL);  
  if (show_axes(ss) == SHOW_NO_AXES) set_sensitive(view_no_axes_menu, false); /* false because it is already chosen */

  view_all_axes_menu = XtCreateManagedWidget(_("both axes"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_all_axes_menu, XmNactivateCallback, view_all_axes_callback, NULL);  
  if (show_axes(ss) == SHOW_ALL_AXES) set_sensitive(view_all_axes_menu, false);

  view_just_x_axis_menu = XtCreateManagedWidget(_("just x axis"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_just_x_axis_menu, XmNactivateCallback, view_just_x_axis_callback, NULL);  
  if (show_axes(ss) == SHOW_X_AXIS) set_sensitive(view_just_x_axis_menu, false);

  view_all_axes_unlabelled_menu = XtCreateManagedWidget(_("both axes, no labels"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_all_axes_unlabelled_menu, XmNactivateCallback, view_all_axes_unlabelled_callback, NULL);  
  if (show_axes(ss) == SHOW_ALL_AXES_UNLABELLED) set_sensitive(view_all_axes_unlabelled_menu, false);

  view_just_x_axis_unlabelled_menu = XtCreateManagedWidget(_("just x axis, no label"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_just_x_axis_unlabelled_menu, XmNactivateCallback, view_just_x_axis_unlabelled_callback, NULL);  
  if (show_axes(ss) == SHOW_X_AXIS_UNLABELLED) set_sensitive(view_just_x_axis_unlabelled_menu, false);

  view_bare_x_axis_menu = XtCreateManagedWidget(_("bare x axis"), xmPushButtonWidgetClass, view_axes_menu, main_args, main_n);
  XtAddCallback(view_bare_x_axis_menu, XmNactivateCallback, view_bare_x_axis_callback, NULL);  
  if (show_axes(ss) == SHOW_BARE_X_AXIS) set_sensitive(view_bare_x_axis_menu, false);

  view_focus_style_menu = XmCreatePulldownMenu(view_menu, (char *)"focusstyle", main_args, main_n);

  k = main_n;
  XtSetArg(main_args[k], XmNsubMenuId, view_focus_style_menu); k++;
  view_focus_cascade_menu = XtCreateManagedWidget(_("Zoom focus"), xmCascadeButtonWidgetClass, view_menu, main_args, k);

  view_focus_left_menu = XtCreateManagedWidget(_("window left edge"), xmPushButtonWidgetClass, view_focus_style_menu, main_args, main_n);
  XtAddCallback(view_focus_left_menu, XmNactivateCallback, view_focus_left_callback, NULL);  

  view_focus_right_menu = XtCreateManagedWidget(_("window right edge"), xmPushButtonWidgetClass, view_focus_style_menu, main_args, main_n);
  XtAddCallback(view_focus_right_menu, XmNactivateCallback, view_focus_right_callback, NULL);  

  view_focus_middle_menu = XtCreateManagedWidget(_("window midpoint"), xmPushButtonWidgetClass, view_focus_style_menu, main_args, main_n);
  XtAddCallback(view_focus_middle_menu, XmNactivateCallback, view_focus_middle_callback, NULL);  

  view_focus_active_menu = XtCreateManagedWidget(_("cursor or selection"), xmPushButtonWidgetClass, view_focus_style_menu, main_args, main_n);
  XtAddCallback(view_focus_active_menu, XmNactivateCallback, view_focus_active_callback, NULL);  


  /* OPTIONS MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)3);
  options_menu = XmCreatePulldownMenu(main_menu, (char *)"Option", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, options_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'O'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)3); high_n++;
  options_cascade_menu = XtCreateManagedWidget(_("Options"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  options_transform_menu = XtCreateManagedWidget(_("Transform Options"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_transform_menu, XmNactivateCallback, options_transform_callback, NULL);
  XtVaSetValues(options_transform_menu, XmNmnemonic, 't', NULL);

  options_controls_menu = XtCreateManagedWidget(_("Controls"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_controls_menu, XmNactivateCallback, options_controls_callback, NULL);
  XtVaSetValues(options_controls_menu, XmNmnemonic, 'c', NULL);


#if HAVE_EXTENSION_LANGUAGE
  options_save_menu = XtCreateManagedWidget(_("Save options"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_save_menu, XmNactivateCallback, options_save_callback, NULL);
  XtVaSetValues(options_save_menu, XmNmnemonic, 'a', NULL);

  options_save_state_menu = XtCreateManagedWidget(_("Save session"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_save_state_menu, XmNactivateCallback, options_save_state_callback, NULL);
#endif

  options_sep_menu = XtCreateManagedWidget("", xmSeparatorWidgetClass, options_menu, sep_args, j);

  options_preferences_menu = XtCreateManagedWidget(_("Preferences"), xmPushButtonWidgetClass, options_menu, main_args, main_n);
  XtAddCallback(options_preferences_menu, XmNactivateCallback, options_preferences_callback, NULL);


  /* HELP MENU */
  XtSetArg(main_args[main_n], XmNuserData, (XtPointer)4);
  help_menu = XmCreatePulldownMenu(main_menu, (char *)"Help", main_args, main_n + 1);

  high_n = start_high_n;
  XtSetArg(high_args[high_n], XmNsubMenuId, help_menu); high_n++;
  XtSetArg(high_args[high_n], XmNmnemonic, 'H'); high_n++;
  XtSetArg(high_args[high_n], XmNuserData, (XtPointer)4); high_n++;
  help_cascade_menu = XtCreateManagedWidget(_("Help"), xmCascadeButtonWidgetClass, main_menu, high_args, high_n);

  help_about_snd_menu = XtCreateManagedWidget(_("About Snd"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_about_snd_menu, XmNactivateCallback, help_about_snd_callback, NULL);
  XtVaSetValues(help_about_snd_menu, XmNmnemonic, 'O', NULL);

#if HAVE_EXTENSION_LANGUAGE
  help_init_file_menu = XtCreateManagedWidget(_("Customization"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_init_file_menu, XmNactivateCallback, help_init_file_callback, NULL);
#endif

  help_controls_menu = XtCreateManagedWidget(_("Control Panel"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_controls_menu, XmNactivateCallback, help_controls_callback, NULL);

  help_keys_menu = XtCreateManagedWidget(_("Key bindings"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_keys_menu, XmNactivateCallback, help_keys_callback, NULL);

  help_recording_menu = XtCreateManagedWidget(_("Record"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_recording_menu, XmNactivateCallback, help_recording_callback, NULL);

  help_play_menu = XtCreateManagedWidget(_("Play"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_play_menu, XmNactivateCallback, help_play_callback, NULL);

  help_save_menu = XtCreateManagedWidget(_("Save"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_save_menu, XmNactivateCallback, help_save_callback, NULL);

  help_mix_menu = XtCreateManagedWidget(_("Mix"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_mix_menu, XmNactivateCallback, help_mix_callback, NULL);

  help_resample_menu = XtCreateManagedWidget(_("Resample"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_resample_menu, XmNactivateCallback, help_resample_callback, NULL);

  help_fft_menu = XtCreateManagedWidget(_("FFT"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_fft_menu, XmNactivateCallback, help_fft_callback, NULL);

  help_filter_menu = XtCreateManagedWidget(_("Filter"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_filter_menu, XmNactivateCallback, help_filter_callback, NULL);

  help_reverb_menu = XtCreateManagedWidget(_("Reverb"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_reverb_menu, XmNactivateCallback, help_reverb_callback, NULL);

  help_env_menu = XtCreateManagedWidget(_("Envelope"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_env_menu, XmNactivateCallback, help_env_callback, NULL);

  help_marks_menu = XtCreateManagedWidget(_("Mark"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_marks_menu, XmNactivateCallback, help_marks_callback, NULL);

  help_insert_menu = XtCreateManagedWidget(_("Insert"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_insert_menu, XmNactivateCallback, help_insert_callback, NULL);

  help_delete_menu = XtCreateManagedWidget(_("Delete"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_delete_menu, XmNactivateCallback, help_delete_callback, NULL);

  help_undo_menu = XtCreateManagedWidget(_("Undo and redo"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_undo_menu, XmNactivateCallback, help_undo_callback, NULL);

#if HAVE_EXTENSION_LANGUAGE
  help_find_menu = XtCreateManagedWidget(_("Search"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_find_menu, XmNactivateCallback, help_find_callback, NULL);
#endif

  help_sync_menu = XtCreateManagedWidget(_("Sync and Unite"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_sync_menu, XmNactivateCallback, help_sync_callback, NULL);

  help_sound_files_menu = XtCreateManagedWidget(_("Headers and Data"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_sound_files_menu, XmNactivateCallback, help_sound_files_callback, NULL);

  help_debug_menu = XtCreateManagedWidget(_("Debugging"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_debug_menu, XmNactivateCallback, help_debug_callback, NULL);

  help_region_menu = XtCreateManagedWidget(_("Regions"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_region_menu, XmNactivateCallback, help_region_callback, NULL);

  help_selection_menu = XtCreateManagedWidget(_("Selections"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_selection_menu, XmNactivateCallback, help_selection_callback, NULL);

  help_colors_menu = XtCreateManagedWidget(_("Colors"), xmPushButtonWidgetClass, help_menu, main_args, main_n);
  XtAddCallback(help_colors_menu, XmNactivateCallback, help_colors_callback, NULL);

  XtVaSetValues(main_menu, XmNmenuHelpWidget, help_cascade_menu, NULL);

  XtAddCallback(file_cascade_menu, XmNcascadingCallback, file_menu_update_1, NULL);
  XtAddCallback(edit_cascade_menu, XmNcascadingCallback, edit_menu_update_1, NULL);
  XtAddCallback(view_cascade_menu, XmNcascadingCallback, view_menu_update_1, NULL);

#ifndef SND_AS_WIDGET
  XtManageChild(main_menu);
#endif
  return(main_menu);
}


/* -------------------------------- POPUP MENU -------------------------------- */

static Widget popup_menu = NULL;

static void popup_menu_update_1(Widget w, XtPointer info, XtPointer context) {popup_menu_update();}

static void popup_info_callback(Widget w, XtPointer info, XtPointer context) 
{
  snd_info *sp;
  sp = any_selected_sound();
  if (sp) display_info(sp);
}

void post_popup(XButtonPressedEvent *event)
{
  XmMenuPosition(popup_menu, event);
  XtManageChild(popup_menu);
}


void create_popup_menu(void)
{
  /* make it a child of the main window */
  if (!popup_menu)
    {
      Widget mainp, sep, pop;
      Arg args[20];
      int n;

      ss->sgx->pw = (Widget *)calloc(NUM_POPUP_WIDGETS, sizeof(Widget));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      mainp = MAIN_PANE(ss);
      XtSetArg(args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC_RECURSIVE); n++;
      XtSetArg(args[n], XmNuserData, (XtPointer)5);
      popup_menu = XmCreatePopupMenu(mainp, (char *)"popup-menu", args, n + 1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;

      pop = XtCreateManagedWidget("Snd", xmLabelWidgetClass, popup_menu, args, n);
      sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, popup_menu, args, n);

      popup_undo_menu = XtCreateManagedWidget(_("Undo"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_undo_menu, XmNsensitive, false, NULL);
      XtAddCallback(popup_undo_menu, XmNactivateCallback, edit_undo_callback, NULL);

      popup_redo_menu = XtCreateManagedWidget(_("Redo"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_redo_menu, XmNsensitive, false, NULL);
      XtAddCallback(popup_redo_menu, XmNactivateCallback, edit_redo_callback, NULL);

      popup_save_menu = XtCreateManagedWidget(_("Save"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtAddCallback(popup_save_menu, XmNactivateCallback, file_save_callback, NULL);
      XtVaSetValues(popup_save_menu, XmNsensitive, false, NULL);

      popup_revert_menu = XtCreateManagedWidget(_("Revert"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_revert_menu, XmNsensitive, true, NULL);
      XtAddCallback(popup_revert_menu, XmNactivateCallback, file_revert_callback, NULL);

      popup_info_menu = XtCreateManagedWidget(_("Info"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_info_menu, XmNsensitive, false, NULL);
      XtAddCallback(popup_info_menu, XmNactivateCallback, popup_info_callback, NULL);

      popup_close_menu = XtCreateManagedWidget(_("Close"), xmPushButtonWidgetClass, popup_menu, args, n);
      XtVaSetValues(popup_close_menu, XmNsensitive, true, NULL);
      XtAddCallback(popup_close_menu, XmNactivateCallback, file_close_callback, NULL);

      XtAddCallback(popup_menu, XmNmapCallback, popup_menu_update_1, NULL);
    }
}

/* TODO: context sensitive popups everywhere (not this stupid thing)
 * PERHAPS: remove panic.scm, edit123.scm, what is kmenu.scm? -- it's adding accelerators
 *          perhaps add them -- we're using them above
 * PERHAPS: incorporate the special-menu stuff? 
 * PERHAPS: use the yin/yang icon for looped play of the whole file?
 */


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
  if (!tooltip_shell)
    {
      tooltip_shell = XtVaCreatePopupShell(tip, overrideShellWidgetClass, MAIN_SHELL(ss), 
					   XmNallowShellResize, true, 
					   NULL);
      tooltip_label = XtVaCreateManagedWidget(tip, xmLabelWidgetClass, tooltip_shell,
					      XmNrecomputeSize, true,
					      XmNbackground, ss->sgx->highlight_color,
					      NULL);
    }
  else 
    {
      XmString str;
      str = XmStringCreateLocalized(tip);
      XtVaSetValues(tooltip_label, XmNlabelString, str, NULL);
      XmStringFree(str);
    }

  XtTranslateCoords(tool_w, tool_x, tool_y, &rx, &ry);
  XtVaSetValues(tooltip_shell, XmNx, rx, XmNy, ry, NULL);
  XtManageChild(tooltip_shell);
  quit_proc = XtAppAddTimeOut(MAIN_APP(ss), (unsigned long)2000, (XtTimerCallbackProc)leave_tooltip, NULL);
}


static void tool_starter(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  XEnterWindowEvent *ev = (XEnterWindowEvent *)event;
  char *tip = (char *)context;
  if ((ev->time - tool_last_time) > 2)
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


static void add_tooltip(Widget w, const char *tip)
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
			      XmNbackground, ss->sgx->basic_color,
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
			  XmNbackground, ss->sgx->basic_color,
			  NULL);
}


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



static void stop_everything_callback(Widget w, XtPointer info, XtPointer context) 
{
  control_g(any_selected_sound());
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNheight, ICON_HEIGHT); n++;
      XtSetArg(args[n], XmNpaneMaximum, ICON_HEIGHT); n++; /* Xm/Paned initializes each pane max to 1000 apparently! */
      XtSetArg(args[n], XmNpositionIndex, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;

      if ((sound_style(ss) == SOUNDS_IN_NOTEBOOK) || (sound_style(ss) == SOUNDS_HORIZONTAL))
	toolbar = XtCreateManagedWidget("toolbar", xmRowColumnWidgetClass, SOUND_PANE_BOX(ss), args, n);
      else toolbar = XtCreateManagedWidget("toolbar", xmRowColumnWidgetClass, SOUND_PANE(ss), args, n);
      ss->sgx->toolbar = toolbar;

      if (auto_resize(ss))
	XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, true, NULL);

      make_toolbar_icons(MAIN_SHELL(ss));

      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_NEW),           "new sound",                  file_new_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_OPEN),          "open sound",                 file_open_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_SAVE),          "save selected sound",        file_save_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_REVERT),        "revert to saved",            file_revert_callback); 
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_UNDO),          "undo edit",                  edit_undo_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_REDO),          "redo last (undone) edit",    edit_redo_callback);
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_CLOSE),         "close selected sound",       file_close_callback);
      add_separator_to_toolbar(toolbar); 

      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_PLAY),          "play from the start",        play_from_start_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_CURSOR_PLAY),   "play from the cursor",       play_from_cursor_callback);      
      add_to_toolbar(toolbar, toolbar_icon(SND_XPM_STOP_PLAY),     "stop playing",               stop_playing_callback);      
      add_separator_to_toolbar(toolbar);
 
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
      add_separator_to_toolbar(toolbar);

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

static void SND_callback(Widget w, XtPointer info, XtPointer context) 
{
  pointer_or_int_t callb;
  XtVaGetValues(w, XmNuserData, &callb, NULL);
  g_snd_callback(CALL_INDEX(callb)); /* menu option activate callback */
}


static void GHC_callback(Widget w, XtPointer info, XtPointer context) 
{
  pointer_or_int_t slot;
  XtVaGetValues(w, XmNuserData, &slot, NULL);
  g_snd_callback(CALL_INDEX(slot)); /* main menu cascading callback */
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
    case 5: return(popup_menu);   break;
      
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
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(slot, new_menu)); n++;
  m = XmCreatePulldownMenu(main_menu, (char *)label, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
      XtSetArg(args[n], XmNuserData, PACK_MENU_DATA(callb, which_menu)); n++;
      m = XtCreateManagedWidget(label, xmPushButtonWidgetClass, menw, args, n);
      XtAddCallback(m, XmNactivateCallback, SND_callback, NULL);
    }
  else
    {
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      if (position >= 0) {XtSetArg(args[n], XmNpositionIndex, position); n++;}
      m = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, menw, args, n);
    }
  return(m);
}


static XEN g_menu_widgets(void)
{
  #define H_menu_widgets "(" S_menu_widgets "): a list of top level menu widgets: ((0)main (1)file (2)edit (3)view (4)options (5)help (6)popup)"
  return(XEN_CONS(XEN_WRAP_WIDGET(main_menu),
	  XEN_CONS(XEN_WRAP_WIDGET(file_cascade_menu),
           XEN_CONS(XEN_WRAP_WIDGET(edit_cascade_menu),
            XEN_CONS(XEN_WRAP_WIDGET(view_cascade_menu),
             XEN_CONS(XEN_WRAP_WIDGET(options_cascade_menu),
              XEN_CONS(XEN_WRAP_WIDGET(help_cascade_menu),
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
  XEN_DEFINE_PROCEDURE(S_menu_widgets, g_menu_widgets_w, 0, 0, 0, H_menu_widgets);
}

/* Motif bug: the button backgrounds remain in the original highlight color? but the widget (if it is one) is not the child of any obvious widget
 */
