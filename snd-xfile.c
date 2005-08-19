#include "snd.h"
/* #include "snd-file.h" */

/* various file-related dialogs:
   File|Edit:Save-as, 
   File:Open|View, 
   File|Edit:Mix, 
   File:Insert,
   File:Edit-Header,
   File:New
   Info and Raw
   View:Files
*/


/* TODO: pull-down list of recent files
 * TODO: new file: find some way to get around the hidden label bug (unmanage error text etc)
 * PERHAPS: if user changes raw file with dialog up -- adding header for example, should we automatically open it? or reflect in panel?
 * PERHAPS: (alert_new_file): handle all directory update decisions through FAM
 * TODO: various file/directory lists: tie into fam/gamin (also previous files list) -- add xen call?
 * TODO: if directory loaded into previous files list via -p, add any new sound files as they appear
 *
 * TODO: scheme option for error history menu?
 * TODO: what if running src, uses its check-event to open raw data -- where is control?
 *       or similarly, stops at "ok", starts src, clicks ok?
 * TODO: need array of dialogs for mix/insert, save-as?
 * TODO: edit-properties dialog (extension of edit-header)
 * TODO: option:settings dialog (global vars)
 * PERHAPS: audio:settings for display, perhaps reset -- as opposed to using the recorder
 * TODO: check that xen-errors are redirected locally (save-as-hook etc)
 * TODO: open no such file goes to post it? -- from startup args!
 * TODO: in mix/insert: panel for mix at cursor/beginning/end/mark/sample (num)
 * TODO: c-x c-n for new, or should c-x c-f behave as in emacs
 * TODO: add|delete-file-filter, file-filters tied to all file dialogs (panel of radio buttons where just sounds is now)
 *
 * PERHAPS: DnD (multi-select etc) to view-files list: ../test/openmotif-2.2.4/demos/programs/filemanager/filemanager
 *    need drop-watchers here, set up on vdat to get possible multi-file string, load each
 *    similarly for the menus -- why doesn't the drag context stuff work?
 *    need same stuff on gtk side -- how do they send multi-names? Qt? etc...
 * TODO: New[Copy?] Viewer button in vf
 * TODO: in nb.scm, get the info dialog out of the line of sight and unmanage it if view-files is unmanaged
 * TODO: horizontal pane in view-files
 * TODO: report-in-minibuffer extended to go to any dialog
 * TODO: always show bg wave in vf
 * TODO: will need at least a reset button for the vf env, perhaps reset for entire vf
 * TODO: snd-file.h + entire split
 */


/* ---------------- open/mix/insert dialogs ---------------- */

static void color_file_selection_box(Widget w)
{
  /* overwrite most Motif-default colors */
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  if (!(ss->using_schemes)) 	
    {
      Widget wtmp = NULL, ftmp = NULL, ltmp = NULL;
      map_over_children(w, set_main_color_of_widget, NULL);
      XtVaSetValues(XmFileSelectionBoxGetChild(w, XmDIALOG_DIR_LIST), 
		    XmNbackground, ss->sgx->white, 
		    XmNforeground, ss->sgx->black, 
		    NULL);
      XtVaSetValues(XmFileSelectionBoxGetChild(w, XmDIALOG_LIST), 
		    XmNbackground, ss->sgx->white, 
		    XmNforeground, ss->sgx->black, 
		    NULL);

      XtVaSetValues(XtNameToWidget(w, "Apply"),  XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "Cancel"), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "Help"),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "OK"),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "Apply"),  XmNbackground, ss->sgx->reset_button_color,  NULL);
      XtVaSetValues(XtNameToWidget(w, "Cancel"), XmNbackground, ss->sgx->quit_button_color,   NULL);
      XtVaSetValues(XtNameToWidget(w, "Help"),   XmNbackground, ss->sgx->help_button_color,   NULL);
      XtVaSetValues(XtNameToWidget(w, "OK"),     XmNbackground, ss->sgx->doit_button_color,   NULL);

      ltmp = XtNameToWidget(w, "FilterLabel");
      if (!ltmp) ltmp = XmFileSelectionBoxGetChild(w, XmDIALOG_FILTER_LABEL);
      XtVaSetValues(ltmp, XmNbackground, ss->sgx->reset_button_color, NULL);

      ltmp = XtNameToWidget(w, "Selection");
      if (!ltmp) ltmp = XmFileSelectionBoxGetChild(w, XmDIALOG_SELECTION_LABEL);
      XtVaSetValues(ltmp, XmNbackground, ss->sgx->help_button_color, NULL);

      ltmp = XtNameToWidget(w, "Dir");
      if (!ltmp) ltmp = XmFileSelectionBoxGetChild(w, XmDIALOG_DIR_LIST_LABEL);
      XtVaSetValues(ltmp, XmNbackground, ss->sgx->doit_button_color, NULL);

      ltmp = XtNameToWidget(w, "Items");
      if (!ltmp) ltmp = XmFileSelectionBoxGetChild(w, XmDIALOG_LIST_LABEL);
      XtVaSetValues(ltmp, XmNbackground, ss->sgx->quit_button_color, NULL);

      wtmp = XtNameToWidget(w, "Text");
      if (!wtmp) wtmp = XmFileSelectionBoxGetChild(w, XmDIALOG_TEXT);
      if (wtmp)
	{
	  XtVaSetValues(wtmp,     XmNhighlightThickness,  1,                          NULL);
	  XtAddCallback(wtmp,     XmNfocusCallback,       textfield_focus_callback,   NULL);
	  XtAddCallback(wtmp,     XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
	  XtAddEventHandler(wtmp, EnterWindowMask, false, mouse_enter_text_callback,  NULL);
	  XtAddEventHandler(wtmp, LeaveWindowMask, false, mouse_leave_text_callback,  NULL);
	}

      ftmp = XtNameToWidget(w, "FilterText");
      if (!ftmp) ftmp = XmFileSelectionBoxGetChild(w, XmDIALOG_FILTER_TEXT);	
      if (ftmp)
	{
	  XtVaSetValues(ftmp,     XmNhighlightThickness,  1,                          NULL);
	  XtAddCallback(ftmp,     XmNfocusCallback,       textfield_focus_callback,   NULL);
	  XtAddCallback(ftmp,     XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
	  XtAddEventHandler(ftmp, EnterWindowMask, false, mouse_enter_text_callback,  NULL);
	  XtAddEventHandler(ftmp, LeaveWindowMask, false, mouse_leave_text_callback,  NULL);
	}
    }
}

/* -------- just-sounds file list handlers -------- */

typedef struct file_pattern_info {
  /* just-sounds file lists */
  bool need_update, new_file_written, in_just_sounds_update;
  Widget dialog, just_sounds_button;
  XmSearchProc default_search_proc;
  char *save_dir,*last_dir;
  dir *sound_files, *current_files;
  char *last_pattern, *full_pathname;
} file_pattern_info;

static int string_compare(const void *ss1, const void *ss2)
{
  return(strcmp((*((char **)ss1)), (*((char **)ss2))));
}

static void sound_file_search(Widget dialog, XmFileSelectionBoxCallbackStruct *info)
{
  /* generate list of sound files, set XmNfileListItems, XmNfileListItemCount, XmNlistUpdated
   * the latter if new file list generated -- if no files, XmNfileListItems NULL, Count 0
   *
   * the pattern (file name mask) only matters if the filter button is hit, 
   * it appears to be "*" until the filter is invoked.
   */
  char *pattern = NULL, *our_dir = NULL;
  dir *cdp;
  file_pattern_info *fp;
  XmFileSelectionBoxCallbackStruct *data = (XmFileSelectionBoxCallbackStruct *)info;
  int i;
  bool filter_callback;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(dialog), dialog);

  XtVaGetValues(dialog, XmNuserData, &fp, NULL);
  pattern = (char *)XmStringUnparse(data->pattern, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  our_dir = (char *)XmStringUnparse(data->dir,     NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

  if (fp->full_pathname == NULL) 
    fp->full_pathname = (char *)CALLOC(512, sizeof(char));
  filter_callback = (strcmp(pattern, "*") != 0);
  if (!filter_callback)
    {
      if ((fp->last_dir == NULL) || 
	  (strcmp(our_dir, fp->last_dir) != 0) || 
	  (fp->new_file_written))
	{
	  if (fp->current_files) 
	    fp->current_files = free_dir(fp->current_files);
	  if (fp->last_dir) 
	    {
	      FREE(fp->last_dir); 
	      fp->last_dir = NULL;
	    }
	  fp->last_dir = copy_string(our_dir);
	  strcpy(fp->full_pathname, our_dir);
	  fp->save_dir = (char *)(fp->full_pathname + snd_strlen(our_dir));
	  if (fp->sound_files) 
	    free_dir(fp->sound_files);
	  fp->sound_files = find_sound_files_in_dir(our_dir);
	  fp->need_update = true;
	}
      if (fp->last_pattern)
	{
	  FREE(fp->last_pattern);
	  fp->last_pattern = NULL;
	}
      cdp = fp->sound_files;
    }
  else 
    {
      if ((fp->last_pattern == NULL) || 
	  (strcmp(pattern, fp->last_pattern) != 0) || 
	  (fp->new_file_written))
	  {
	    if (fp->last_pattern) 
	      {
		FREE(fp->last_pattern); 
		fp->last_pattern = NULL;
	      }
	    fp->last_pattern = copy_string(pattern);
	    if (fp->current_files)  
	      fp->current_files = free_dir(fp->current_files);
	    if ((fp->sound_files) && 
		(fp->sound_files->len > 0)) 
	      fp->current_files = filter_sound_files(fp->sound_files, pattern);
	    fp->need_update = true;
	  }
      cdp = fp->current_files;
    }  
  fp->new_file_written = false;
  if (fp->need_update)
    {
      XmString *names = NULL;
      if ((cdp) && (cdp->len > 0))
	{
	  qsort((void *)(cdp->files), cdp->len, sizeof(char *), string_compare);
	  names = (XmString *)CALLOC(cdp->len, sizeof(XmString));

#ifdef MUS_SGI
	  /* this is true only if the SGI "enhanced FSB" is in use, I hope */
	  if (!(XtNameToWidget(dialog, "Text"))) 
	    fp->save_dir = fp->full_pathname;
	  /* can't use SgDIALOG_FINDER here as suggested by SGI "Integration Guide" because
	   * XmFileSelectionBoxGetChild(dialog, SgDIALOG_FINDER)) generates an error if
	   * snd was loaded without -lSgm.
	   */
#endif

	  for (i = 0; i < cdp->len; i++) 
	    {
	      char *sp, *sn;
	      for (sp = fp->save_dir, sn = cdp->files[i]; ((*sp) = (*sn)) != '\0'; sp++, sn++);
	      /* save_dir is a pointer into fullpathname after the directory portion */
	      /*   this is unreadable code! -- it's basically sprintf(fullpathname, "%s%s", our_dir, cdp->files[i]) I think */
	      names[i] = XmStringCreate(fp->full_pathname, XmFONTLIST_DEFAULT_TAG);
	    }
	}
      else names = NULL;
      if (cdp) 
	XtVaSetValues(dialog, 
		      XmNfileListItems, names, 
		      XmNfileListItemCount, cdp->len, 
		      XmNlistUpdated, true, 
		      NULL);
      if (names)
	{
	  for (i = 0; i < cdp->len; i++) 
	    if (names[i]) 
	      XmStringFree(names[i]);
	  FREE(names);
	}
    }
  if (our_dir) XtFree(our_dir);
  if (pattern) XtFree(pattern);
  fp->need_update = false;
}

static void force_directory_reread(Widget dialog)
{
  /* force update, but make sure the filename is not reset to its (dumb) default */
  XmString dirmask;
  Widget name_field;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(dialog), dialog);
  name_field = XtNameToWidget(dialog, "Text");
  if (!name_field) 
    name_field = XmFileSelectionBoxGetChild(dialog, XmDIALOG_TEXT);
  filename = XmTextGetString(name_field);
  XtVaGetValues(dialog, XmNdirMask, &dirmask, NULL);
  XmFileSelectionDoSearch(dialog, dirmask);
  XmStringFree(dirmask);
  XmTextSetString(name_field, filename);
  if (filename) XtFree(filename);
}

static void just_sounds_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  file_pattern_info *fp = (file_pattern_info *)context;
  XmString lab;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(fp->dialog), fp->dialog);
  lab = XmStringCreate((char *)((cb->set) ? _("Sound Files") : _("Files")), XmFONTLIST_DEFAULT_TAG);
  if (cb->set)
    {
      XtVaGetValues(fp->dialog, 
		    XmNfileSearchProc, &(fp->default_search_proc), 
		    NULL);
      XtVaSetValues(fp->dialog, 
		    XmNfileSearchProc, sound_file_search,
		    XmNfileListLabelString, lab,
		    NULL);
    }
  else XtVaSetValues(fp->dialog, 
		     XmNfileSearchProc, fp->default_search_proc, 
		     XmNfileListLabelString, lab,
		     NULL);
  XmStringFree(lab);
  fp->need_update = true;
  fp->in_just_sounds_update = true;
  force_directory_reread(fp->dialog);
  fp->in_just_sounds_update = false;
}


/* -------- play selected file handlers -------- */

typedef struct dialog_play_info {
  Widget dialog, play_button;
  snd_info *player;
} dialog_play_info;

static void file_dialog_stop_playing(dialog_play_info *dp)
{
  if ((dp->player) && 
      (dp->player->playing)) 
    {
      stop_playing_sound(dp->player, PLAY_BUTTON_UNSET);
      dp->player = NULL;
    }
}

void clear_deleted_snd_info(struct dialog_play_info *dp)
{
  dp->player = NULL;
}

static void play_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  dialog_play_info *dp = (dialog_play_info *)context;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  if (cb->set)
    {
      Widget wtmp;
      char *filename = NULL;
      if ((dp->player) && 
	  (dp->player->playing)) 
	stop_playing_sound(dp->player, PLAY_BUTTON_UNSET);
      wtmp = XtNameToWidget(dp->dialog, "Text");
      if (!wtmp) 
	wtmp = XmFileSelectionBoxGetChild(dp->dialog, XmDIALOG_TEXT);
      filename = XmTextGetString(wtmp);
      if (filename)
	{
	  if (mus_file_probe(filename))
	    {
	      dp->player = make_sound_readable(filename, false);
	      dp->player->delete_me = dp;
	      if (dp->player)
		play_sound(dp->player, 0, NO_END_SPECIFIED);
	    }
	  XtFree(filename);
	}
    }
  else file_dialog_stop_playing(dp);
}

static void add_play_and_just_sounds_buttons(Widget dialog, Widget parent, file_pattern_info *fp, dialog_play_info *dp)
{
  Widget rc;
  rc = XtVaCreateManagedWidget("filebuttons-rc", 
			       xmRowColumnWidgetClass, parent,
			       XmNorientation, XmHORIZONTAL,
			       NULL);
  fp->just_sounds_button = XtVaCreateManagedWidget(_("sound files only"), xmToggleButtonWidgetClass, rc,
						   XmNset, just_sounds(ss),
						   XmNalignment, XmALIGNMENT_BEGINNING,
						   NULL);
  dp->play_button = XtVaCreateWidget(_("play selected sound"), xmToggleButtonWidgetClass, rc,
				     XmNalignment, XmALIGNMENT_END,
				     NULL);

  XtAddCallback(dp->play_button, XmNvalueChangedCallback, play_selected_callback, (XtPointer)dp);
  XtAddCallback(fp->just_sounds_button, XmNvalueChangedCallback, just_sounds_callback, (XtPointer)fp);
}



/* -------- File Open/View/Mix Dialogs -------- */

typedef struct file_dialog_info {
  bool file_dialog_read_only;
  Widget dialog;
  Widget info_frame, info1, info2; /* labels giving info on selected file, or an error message */
  file_pattern_info *fp;
  dialog_play_info *dp;
  int open_file_watcher_loc;
} file_dialog_info;

static void open_file_help_callback (Widget w, XtPointer context, XtPointer info) 
{
  open_file_dialog_help();
}

static void file_cancel_callback (Widget w, XtPointer context, XtPointer info) 
{
  file_dialog_stop_playing((dialog_play_info *)context);
  XtUnmanageChild (w);
}

static void post_sound_info(Widget info1, Widget info2, const char *filename, bool with_filename)
{
  /* filename is known[strongly believed] to be a sound file, etc */
  XmString label;
  char *buf;
  char timestr[64];
  time_t date;

  buf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s%s%d chan%s, %d Hz, %.3f secs",
	       (with_filename) ? filename_without_home_directory(filename) : "",
	       (with_filename) ? ": " : "",
	       mus_sound_chans(filename),
	       (mus_sound_chans(filename) > 1) ? "s" : "",
	       mus_sound_srate(filename),
	       mus_sound_duration(filename));
  label = XmStringCreateLocalized(buf);
  XtVaSetValues(info1, 
		XmNlabelString, label, 
		NULL);
  XmStringFree(label);

  date = mus_sound_write_date(filename);
#if HAVE_STRFTIME
  strftime(timestr, 64, ", %d-%b-%Y", localtime(&date));
#else
  sprintf(timestr, "");
#endif
  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s, %s%s",
	       mus_header_type_name(mus_sound_header_type(filename)),
	       short_data_format_name(mus_sound_data_format(filename), filename),
	       timestr);
  label = XmStringCreateLocalized(buf);
  XtVaSetValues(info2, XmNlabelString, label, NULL);
  XmStringFree(label);
  FREE(buf);
}

static void post_file_info(file_dialog_info *fd, const char *filename)
{
  XtManageChild(fd->dp->play_button);
  post_sound_info(fd->info1, fd->info2, filename, true);
  if (!(XtIsManaged(fd->info1))) 
    XtManageChild(fd->info1);
  if (!(XtIsManaged(fd->info2))) 
    XtManageChild(fd->info2);
  if (!(XtIsManaged(fd->info_frame)))
    XtManageChild(fd->info_frame);
}

static void unpost_file_info(file_dialog_info *fd)
{
  if (XtIsManaged(fd->dp->play_button)) 
    XtUnmanageChild(fd->dp->play_button);
  if (XtIsManaged(fd->info_frame))
    XtUnmanageChild(fd->info_frame);
}

static void file_dialog_select_callback(Widget w, XtPointer context, XtPointer info)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmString *strs;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  XtVaGetValues(w, XmNselectedItems, &strs, NULL);
  filename = (char *)XmStringUnparse(strs[0], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((filename) && 
      (plausible_sound_file_p(filename))) /* forces header read to avoid later unwanted error possibility */
    post_file_info(fd, filename);
  else unpost_file_info(fd);
  if (filename) XtFree(filename);
}

static void unpost_if_filter_changed(Widget w, XtPointer context, XtPointer info)
{
  unpost_file_info((file_dialog_info *)context);
}

static void watch_filename_change(Widget w, XtPointer context, XtPointer info)
{
  /* try to move file list to show possible matches,
   *   if a sound file, show info
   */
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename = NULL;
  filename = XmTextGetString(w);
  if ((filename) && (*filename))
    {
      XmStringTable files;
      Widget file_list;
      int num_files = 0, i, top, visible, pos = -1, l, u;
      char *file_list_file = NULL;

      file_list = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_LIST);
      XtVaGetValues(fd->dialog,
		    XmNfileListItemCount, &num_files,
		    XmNfileListItems, &files, /* do not free */
		    NULL);
      XtVaGetValues(file_list,
		    XmNtopItemPosition, &top,
		    XmNvisibleItemCount, &visible,
		    NULL);
      l = 0; /* hooray for Knuth... */
      u = num_files - 1;
      while (true)
	{
	  int comp;
	  if (u < l) break;
	  i = (l + u) / 2;
	  file_list_file = (char *)XmStringUnparse(files[i], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL); /* p453 */
	  comp = strcmp(file_list_file, filename);
	  XtFree(file_list_file);
	  if (comp == 0)
	    {
	      pos = i + 1;
	      break;
	    }
	  if (comp < 0) /* files[i] less than filename */
	    l = i + 1;
	  else u = i - 1;
	}
      if (pos >= 0)
	{
	  if (pos < top)
	    XmListSetPos(file_list, pos);
	  else
	    {
	      if (pos >= (top + visible))
		{
		  if ((pos + visible) > num_files)
		    XmListSetBottomPos(file_list, pos);
		  else XmListSetPos(file_list, pos);
		}
	    }
	}
      if ((mus_file_probe(filename)) && 
	  (!directory_p(filename)))
	{
	  if (sound_file_p(filename))
	    post_file_info(fd, filename);
	}
    }
  if (filename) XtFree(filename);
}

static void focus_filename_text_callback(Widget w, XtPointer context, XtPointer info)
{
  XtAddCallback(w, XmNvalueChangedCallback, watch_filename_change, context);
}

static void unfocus_filename_text_callback(Widget w, XtPointer context, XtPointer info)
{
  XtRemoveCallback(w, XmNvalueChangedCallback, watch_filename_change, context);
}

static file_dialog_info *make_file_dialog(bool read_only, char *title, char *select_title, 
					  XtCallbackProc file_ok_proc, XtCallbackProc file_help_proc)
{
  Widget w;
  file_dialog_info *fd;
  Arg args[20];
  int n;
  XmString s1, s2, ok_label, filter_list_label, cancel_label, filter_button_label;
  Widget wtmp = NULL, rc1, rc2;
  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->fp = (file_pattern_info *)CALLOC(1, sizeof(file_pattern_info));
  fd->fp->in_just_sounds_update = false;
  fd->dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));
  fd->file_dialog_read_only = read_only;
  /* file selection dialog box with added "Just Sound Files" and "Play selected" toggle buttons and info area */
  w = MAIN_SHELL(ss);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  s1 = XmStringCreate(select_title, XmFONTLIST_DEFAULT_TAG);
  s2 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
  ok_label = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
  filter_list_label = XmStringCreate(_("files listed:"), XmFONTLIST_DEFAULT_TAG);
  filter_button_label = XmStringCreate(_("Update"), XmFONTLIST_DEFAULT_TAG);
  cancel_label = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);

  XtSetArg(args[n], XmNokLabelString, ok_label); n++;
  XtSetArg(args[n], XmNselectionLabelString, s1); n++;
  XtSetArg(args[n], XmNdialogTitle, s2); n++;
  XtSetArg(args[n], XmNfilterLabelString, filter_list_label); n++;
  XtSetArg(args[n], XmNapplyLabelString, filter_button_label); n++;
  XtSetArg(args[n], XmNcancelLabelString, cancel_label); n++;
  XtSetArg(args[n], XmNuserData, (XtPointer)(fd->fp)); n++;

  if (just_sounds(ss))
    {
      XmString lab;
      lab = XmStringCreate(_("Sound Files"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNfileListLabelString, lab); n++;
      XmStringFree(lab);
    }

  fd->dialog = XmCreateFileSelectionDialog(w, title, args, n);
  fd->fp->dialog = fd->dialog;
  fd->dp->dialog = fd->dialog;

  XmStringFree(s1);
  XmStringFree(s2);
  XmStringFree(ok_label);
  XmStringFree(filter_list_label);
  XmStringFree(filter_button_label);
  XmStringFree(cancel_label);

  rc1 = XtVaCreateManagedWidget("filebuttons-rc1", 
				xmRowColumnWidgetClass, fd->dialog,
				XmNorientation, XmVERTICAL,
				NULL);
  add_play_and_just_sounds_buttons(fd->dialog, rc1, fd->fp, fd->dp);

  fd->info_frame = XtVaCreateWidget("", xmFrameWidgetClass, rc1, NULL);
  rc2 = XtVaCreateManagedWidget("info-rc2", 
				xmRowColumnWidgetClass, fd->info_frame,
				XmNorientation, XmVERTICAL,
				XmNbackground, ss->sgx->highlight_color,
				NULL);
  fd->info1 = XtVaCreateManagedWidget("", xmLabelWidgetClass, rc2, XmNbackground, ss->sgx->highlight_color, NULL);
  fd->info2 = XtVaCreateManagedWidget("", xmLabelWidgetClass, rc2, XmNbackground, ss->sgx->highlight_color, NULL);
  color_file_selection_box(fd->dialog);

  wtmp = XtNameToWidget(fd->dialog, "Text");
  if (!wtmp) 
    wtmp = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_TEXT);
  if (wtmp) 
    {
      add_completer_to_textfield(wtmp, add_completer_func(sound_filename_completer));
      XtAddCallback(wtmp, XmNfocusCallback, focus_filename_text_callback, (XtPointer)fd);
      XtAddCallback(wtmp, XmNlosingFocusCallback, unfocus_filename_text_callback, (XtPointer)fd);
    }

  wtmp = XtNameToWidget(fd->dialog, "FilterText");
  if (!wtmp) 
    wtmp = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_FILTER_TEXT);
  if (wtmp)
    {
      add_completer_to_textfield(wtmp, add_completer_func(filename_completer));
      XtAddCallback(wtmp, XmNvalueChangedCallback, unpost_if_filter_changed, (XtPointer)fd);
    }

  if (!(ss->using_schemes)) 
    {
      XtVaSetValues(fd->fp->just_sounds_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(fd->dp->play_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
    }
  XtVaGetValues(fd->dialog, XmNfileSearchProc, &(fd->fp->default_search_proc), NULL);

  XtAddCallback(fd->dialog, XmNokCallback, file_ok_proc, (XtPointer)fd);
  XtAddCallback(fd->dialog, XmNcancelCallback, file_cancel_callback, (XtPointer)(fd->dp));
  XtAddCallback(fd->dialog, XmNhelpCallback, file_help_proc, NULL);
  XtAddCallback(XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_LIST),
		XmNbrowseSelectionCallback, file_dialog_select_callback, (XtPointer)fd);
  return(fd);
}


/* -------- File:Open/View dialogs -------- */

static void file_open_error(const char *error_msg, void *ufd)
{
  /* called from snd_error, redirecting error handling to the dialog */
  file_dialog_info *fd = (file_dialog_info *)ufd;
  XmString msg;
  msg = XmStringCreate((char *)error_msg, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(fd->info1, 
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);

  if (XtIsManaged(fd->info2))
    XtUnmanageChild(fd->info2);
  if (!(XtIsManaged(fd->info_frame))) 
    XtManageChild(fd->info_frame);
}

static void open_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  Widget dialog_filename_text;
  if (!(fd->fp->in_just_sounds_update)) /* auto trigger from just_sounds button -- unwanted! */
    {
      if (XtIsManaged(fd->info_frame))
	XtUnmanageChild(fd->info_frame);
      dialog_filename_text = XtNameToWidget(fd->dialog, "Text");
      if (!dialog_filename_text) 
	dialog_filename_text = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_TEXT);
      if (dialog_filename_text) 
	XtRemoveCallback(dialog_filename_text, XmNmodifyVerifyCallback, open_modify_callback, context);
    }
  cbs->doit = true; /* fixup filename elsewhere -- returning false here makes the thing beep! */
}

static void clear_error_if_open_changes(Widget dialog, void *data)
{
  Widget dialog_filename_text;
  dialog_filename_text = XtNameToWidget(dialog, "Text");
  if (!dialog_filename_text) 
    dialog_filename_text = XmFileSelectionBoxGetChild(dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) 
    XtAddCallback(dialog_filename_text, XmNmodifyVerifyCallback, open_modify_callback, (XtPointer)data);
}

static void file_open_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)info;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  filename = (char *)XmStringUnparse(cbs->value, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((!filename) || (!(*filename)))
    {
      file_open_error(_("no filename given"), (void *)fd);
      clear_error_if_open_changes(fd->dialog, (void *)fd);
    }
  else
    {
      file_dialog_stop_playing(fd->dp);
      if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
	{
	  snd_info *sp;
	  redirect_snd_error_to(file_open_error, (void *)fd);
	  ss->sgx->requestor_dialog = w;
	  ss->open_requestor = FROM_OPEN_DIALOG;
	  sp = snd_open_file(filename, fd->file_dialog_read_only);
	  redirect_snd_error_to(NULL, NULL);
	  if (sp) 
	    {
	      XtUnmanageChild(w);
	      select_channel(sp, 0);
	    }
	  else
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		clear_error_if_open_changes(fd->dialog, (void *)fd);
	      /* TODO: if no such file, fam here? */
	    }
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format(_("%s is a directory"), filename);
	  file_open_error(str, (void *)fd);
	  clear_error_if_open_changes(fd->dialog, (void *)fd);
	  FREE(str);
	}
    }
}

static file_dialog_info *odat = NULL;

widget_t make_open_file_dialog(bool read_only, bool managed)
{
  char *title, *select_title;
  if (read_only)
    {
      title = _("View");
      select_title = _("open read-only:");
    }
  else
    {
      title = _("Open");
      select_title = _("open:");
    }
  if (!odat)
    {
      odat = make_file_dialog(read_only, title, select_title, file_open_ok_callback, open_file_help_callback);
      set_dialog_widget(FILE_OPEN_DIALOG, odat->dialog);
      if (just_sounds(ss)) 
	{
	  XtVaSetValues(odat->dialog, 
			XmNfileSearchProc, sound_file_search, 
			NULL);
	  odat->fp->need_update = true;
	  force_directory_reread(odat->dialog);
	}
    }
  else
    {
      if (odat->file_dialog_read_only != read_only)
	{
	  XmString s1, s2;
	  s1 = XmStringCreate(select_title, XmFONTLIST_DEFAULT_TAG);
	  s2 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(odat->dialog, 
			XmNselectionLabelString, s1, 
			XmNdialogTitle, s2, 
			NULL);
	  XmStringFree(s1);
	  XmStringFree(s2);
	  odat->file_dialog_read_only = read_only;
	}
    }
  if (odat->fp->new_file_written) 
    {
      force_directory_reread(odat->dialog);
      odat->fp->new_file_written = false;
    }
  if ((managed) && (!(XtIsManaged(odat->dialog))))
    XtManageChild(odat->dialog);
  return(odat->dialog);
}



/* -------- File:Mix dialog -------- */

static void file_mix_ok_callback(Widget w, XtPointer context, XtPointer info)
{
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)info;
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  filename = (char *)XmStringUnparse(cbs->value, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((!filename) || (!(*filename)))
    {
      file_open_error(_("no filename given"), (void *)fd);
      clear_error_if_open_changes(fd->dialog, (void *)fd);
    }
  else
    {
      file_dialog_stop_playing(fd->dp);
      if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
	{
	  int id_or_error;
	  snd_info *sp;
	  sp = any_selected_sound();
	  redirect_snd_error_to(file_open_error, (void *)fd);
	  ss->sgx->requestor_dialog = w;
	  ss->open_requestor = FROM_MIX_DIALOG;
	  id_or_error = mix_complete_file_at_cursor(sp, filename, with_mix_tags(ss), 0);
	  /* "id_or_error" here is either one of the mix id's or an error indication such as MIX_FILE_NO_MIX */
	  /*    the possible error conditions have been checked alreay, or go through snd_error */
	  redirect_snd_error_to(NULL, NULL);
	  if (id_or_error < 0) /* actually -1 .. -3 */
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		clear_error_if_open_changes(fd->dialog, (void *)fd);
	    }
	  else report_in_minibuffer(sp, _("%s mixed in at cursor"), filename);
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format(_("%s is a directory"), filename);
	  file_open_error(str, (void *)fd);
	  clear_error_if_open_changes(fd->dialog, (void *)fd);
	  FREE(str);
	}
    }
}
  
static void mix_file_help_callback (Widget w, XtPointer context, XtPointer info) 
{
  mix_file_dialog_help();
}

static void file_open_file_watcher(ss_watcher_reason_t reason, int loc)
{
  ss_watcher *data;
  file_dialog_info *fdat;
  data = ss->watchers[loc];
  fdat = (file_dialog_info *)(data->context);
  if ((fdat->dialog) &&
      (XtIsManaged(fdat->dialog)))
    set_sensitive(XmFileSelectionBoxGetChild(fdat->dialog, XmDIALOG_OK_BUTTON), (bool)any_selected_sound());
}

static file_dialog_info *mdat = NULL;

widget_t make_mix_file_dialog(bool managed)
{
  /* called from the menu */
  if (mdat == NULL)
    {
      mdat = make_file_dialog(true, _("Mix Sound"), _("mix in:"), file_mix_ok_callback, mix_file_help_callback);
      set_dialog_widget(FILE_MIX_DIALOG, mdat->dialog);
      if (just_sounds(ss)) 
	{
	  XtVaSetValues(mdat->dialog, 
			XmNfileSearchProc, sound_file_search, 
			NULL);
	  mdat->fp->need_update = true;
	  force_directory_reread(mdat->dialog);
	}
      mdat->open_file_watcher_loc = add_ss_watcher(SS_FILE_OPEN_WATCHER, file_open_file_watcher, (void *)mdat);
    }
  if ((managed) && (!XtIsManaged(mdat->dialog)))
    XtManageChild(mdat->dialog);
  return(mdat->dialog);
}


/* -------- File:Insert dialog -------- */

static void file_insert_ok_callback(Widget w, XtPointer context, XtPointer info)
{
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)info;
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  filename = (char *)XmStringUnparse(cbs->value, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((!filename) || (!(*filename)))
    {
      file_open_error(_("no filename given"), (void *)fd);
      clear_error_if_open_changes(fd->dialog, (void *)fd);
    }
  else
    {
      file_dialog_stop_playing(fd->dp);
      if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
	{
	  bool ok = false;
	  snd_info *sp;
	  sp = any_selected_sound();
	  ss->sgx->requestor_dialog = w;
	  ss->open_requestor = FROM_INSERT_DIALOG;
	  redirect_snd_error_to(file_open_error, (void *)fd);
	  ok = insert_complete_file_at_cursor(sp, filename);
	  redirect_snd_error_to(NULL, NULL);
	  if (!ok)
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		clear_error_if_open_changes(fd->dialog, (void *)fd);
	    }
	  else report_in_minibuffer(sp, _("%s inserted at cursor"), filename);
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format(_("%s is a directory"), filename);
	  file_open_error(str, (void *)fd);
	  clear_error_if_open_changes(fd->dialog, (void *)fd);
	  FREE(str);
	}
    }
}
  
static void insert_file_help_callback (Widget w, XtPointer context, XtPointer info) 
{
  insert_file_dialog_help();
}

static file_dialog_info *idat = NULL;

widget_t make_insert_file_dialog(bool managed)
{
  /* called from the menu */
  if (idat == NULL)
    {
      idat = make_file_dialog(true, _("Insert Sound"), _("insert:"), file_insert_ok_callback, insert_file_help_callback);
      set_dialog_widget(FILE_INSERT_DIALOG, idat->dialog);
      if (just_sounds(ss)) 
	{
	  XtVaSetValues(idat->dialog, 
			XmNfileSearchProc, sound_file_search, 
			NULL);
	  idat->fp->need_update = true;
	  force_directory_reread(idat->dialog);
	}
      idat->open_file_watcher_loc = add_ss_watcher(SS_FILE_OPEN_WATCHER, file_open_file_watcher, (void *)idat);
    }
  if ((managed) && (!XtIsManaged(idat->dialog)))
    XtManageChild(idat->dialog);
  return(idat->dialog);
}


/* -------- reflect outside changes -------- */

void set_open_file_play_button(bool val)
{
  if ((odat) && (odat->dp->play_button))
    XmToggleButtonSetState(odat->dp->play_button, (Boolean)val, false);
  if ((mdat) && (mdat->dp->play_button))
    XmToggleButtonSetState(mdat->dp->play_button, (Boolean)val, false);
  if ((idat) && (idat->dp->play_button))
    XmToggleButtonSetState(idat->dp->play_button, (Boolean)val, false);
}

void alert_new_file(void) 
{
  if (odat)
    odat->fp->new_file_written = true;
  if (mdat)
    mdat->fp->new_file_written = true;
  if (idat)
    idat->fp->new_file_written = true;
}

void reflect_just_sounds(void)
{
  if ((odat) && (odat->fp->just_sounds_button))
    XmToggleButtonSetState(odat->fp->just_sounds_button, just_sounds(ss), true);
  if ((mdat) && (mdat->fp->just_sounds_button))
    XmToggleButtonSetState(mdat->fp->just_sounds_button, just_sounds(ss), true);
  if ((idat) && (idat->fp->just_sounds_button))
    XmToggleButtonSetState(idat->fp->just_sounds_button, just_sounds(ss), true);
}



/* ---------------- file data panel ---------------- */

#define NUM_VISIBLE_HEADERS 5

char *get_file_dialog_sound_attributes(file_data *fdat, 
				       int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples, 
				       int min_chan)
{
  char *str;
  int n;
  int res;
  int *ns = NULL;
  char *comment = NULL;
  fdat->error_widget = NOT_A_SCANF_WIDGET;
  fdat->scanf_widget = NOT_A_SCANF_WIDGET;

  if ((srate) && (fdat->srate_text))
    {
      str = XmTextGetString(fdat->srate_text); 
      fdat->scanf_widget = SRATE_WIDGET;
      if ((str) && (*str))
	{
	  (*srate) = string_to_int_with_error(str, 1, "srate"); 
	  XtFree(str);
	}
      else snd_error("no srate?");
    }

  if ((chans) && (fdat->chans_text))
    {
      str = XmTextGetString(fdat->chans_text); 
      fdat->scanf_widget = CHANS_WIDGET;
      if ((str) && (*str))
	{
	  (*chans) = string_to_int_with_error(str, min_chan, "chans"); 
	  XtFree(str);
	}
      else
	{
	  if (min_chan > 0)
	    snd_error("no chans?");
	}
    }
  
  if ((location) && (fdat->location_text))
    {
      str = XmTextGetString(fdat->location_text); 
      fdat->scanf_widget = DATA_LOCATION_WIDGET;
      if ((str) && (*str))
	{
	  (*location) = string_to_off_t_with_error(str, 0, "data location"); 
	  XtFree(str);
	}
      else snd_error("no data location?");
    }

  if ((samples) && (fdat->samples_text))
    {
      str = XmTextGetString(fdat->samples_text); 
      fdat->scanf_widget = SAMPLES_WIDGET;
      if ((str) && (*str))
	{
	  (*samples) = string_to_off_t_with_error(str, 0, "samples"); 
	  XtFree(str);
	}
      else snd_error("no samples?");
    }
  fdat->scanf_widget = SAMPLES_WIDGET;

  if ((type) && (fdat->header_list))
    {
      res = XmListGetSelectedPos(fdat->header_list, &ns, &n);
      if (res)
	{
	  (*type) = header_type_from_position(ns[0] - 1);
	  fdat->current_type = (*type);
	  free(ns); 
	  ns = NULL;
	}
    }

  if ((format) && (fdat->format_list))
    {
      res = XmListGetSelectedPos(fdat->format_list, &ns, &n);
      if (res)
	{
	  (*format) = data_format_from_position(fdat->current_type, ns[0] - 1);
	  fdat->current_format = (*format);
	  free(ns); 
	  ns = NULL;
	}
    }

  if (fdat->comment_text) 
    {
      comment = XmTextGetString(fdat->comment_text);
      if (comment)
	{
	  str = copy_string(comment);
	  XtFree(comment);
	  return(str);
	}
    }

  return(NULL);
}

#define IGNORE_DATA_LOCATION -1
#define IGNORE_SAMPLES -1
#define IGNORE_CHANS -1
#define IGNORE_SRATE -1
#define IGNORE_HEADER_TYPE -1

static void set_file_dialog_sound_attributes(file_data *fdat, 
					     int type, int format, int srate, int chans, off_t location, off_t samples, char *comment)
{
  int i;
  char **fl = NULL;
  XmString *strs;

  if (type != IGNORE_HEADER_TYPE)
    fdat->current_type = type;
  else fdat->current_type = MUS_RAW;
  fdat->current_format = format;
  fl = set_header_and_data_positions(fdat, fdat->current_type, fdat->current_format);
  if (fl == NULL) return;
  
  if ((type != IGNORE_HEADER_TYPE) &&
      (fdat->header_list))
    XmListSelectPos(fdat->header_list, fdat->header_pos + 1, false);

  strs = (XmString *)MALLOC(fdat->formats * sizeof(XmString)); 
  for (i = 0; i < fdat->formats; i++) 
    strs[i] = XmStringCreate(fl[i], 
			     XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(fdat->format_list, 
		XmNitems, strs, 
		XmNitemCount, fdat->formats, 
		NULL);
  for (i = 0; i < fdat->formats; i++)
    XmStringFree(strs[i]);
  FREE(strs); 
  XmListSelectPos(fdat->format_list, fdat->format_pos + 1, false);

  if ((srate != IGNORE_SRATE) && 
      (fdat->srate_text))
    widget_int_to_text(fdat->srate_text, srate);

  if ((chans != IGNORE_CHANS) && 
      (fdat->chans_text))
    widget_int_to_text(fdat->chans_text, chans);

  if (fdat->comment_text) 
    XmTextSetString(fdat->comment_text, comment);

  if ((location != IGNORE_DATA_LOCATION) && 
      (fdat->location_text))
    widget_off_t_to_text(fdat->location_text, location);

  if ((samples != IGNORE_SAMPLES) && 
      (fdat->samples_text))
    widget_off_t_to_text(fdat->samples_text, samples);
}


/* -------- error handling -------- */

/* if an error occurs, a callback is added to the offending text widget, and an error is
 *   posted in the error_text label.  When the user modifies the bad entry, the callback
 *   erases the error message, and removes itself from the text widget.
 */

static void clear_dialog_error(file_data *fd)
{
  if (XtIsManaged(fd->error_text))
    {
      XtUnmanageChild(fd->error_text);
      if (fd->comment_text)
	{
	  XtVaSetValues(fd->comment_text, 
			XmNbottomAttachment, XmATTACH_FORM,
			NULL);
	}
    }
}

static void show_dialog_error(file_data *fd)
{
  if (!(XtIsManaged(fd->error_text))) 
    {
      if (fd->comment_text)
	{
	  XtVaSetValues(fd->comment_text, 
			XmNbottomAttachment, XmATTACH_WIDGET,
			XmNbottomWidget, fd->error_text,
			NULL);
	}
      XtManageChild(fd->error_text);
    }
}

static void post_file_dialog_error(const char *error_msg, void *ufd)
{
  XmString msg;
  file_data *fd = (file_data *)ufd;
  msg = XmStringCreate((char *)error_msg, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(fd->error_text, 
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);
  show_dialog_error(fd);
}

static void filename_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  Widget dialog_filename_text;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(fd->dialog), fd->dialog);
  clear_dialog_error(fd);
  dialog_filename_text = XtNameToWidget(fd->dialog, "Text");
  if (!dialog_filename_text) dialog_filename_text = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) XtRemoveCallback(dialog_filename_text, XmNmodifyVerifyCallback, filename_modify_callback, context);
  cbs->doit = true;
}

static void clear_error_if_filename_changes(Widget dialog, void *data)
{
  Widget dialog_filename_text;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(dialog), dialog);
  dialog_filename_text = XtNameToWidget(dialog, "Text");
  if (!dialog_filename_text) dialog_filename_text = XmFileSelectionBoxGetChild(dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) 
    XtAddCallback(dialog_filename_text, XmNmodifyVerifyCallback, filename_modify_callback, (XtPointer)data);
}

static void chans_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  clear_dialog_error(fd);
  XtRemoveCallback(fd->chans_text, XmNmodifyVerifyCallback, chans_modify_callback, context);
  cbs->doit = true;
}

static void clear_error_if_chans_changes(Widget dialog, void *data)
{
  file_data *fd = (file_data *)data;
  if (fd->chans_text) XtAddCallback(fd->chans_text, XmNmodifyVerifyCallback, chans_modify_callback, (XtPointer)data);
}

static void panel_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  clear_dialog_error(fd);
  XtRemoveCallback(w, XmNmodifyVerifyCallback, panel_modify_callback, context);
  cbs->doit = true;
}

static void clear_error_if_panel_changes(Widget dialog, void *data)
{
  file_data *fd = (file_data *)data;
  Widget baddy;
  switch (fd->error_widget)
    {
    case SRATE_WIDGET:         baddy = fd->srate_text;    break;
    case DATA_LOCATION_WIDGET: baddy = fd->location_text; break;
    case SAMPLES_WIDGET:       baddy = fd->samples_text;  break;
    default:                   baddy = fd->chans_text;    break;
    }
  if (baddy) XtAddCallback(baddy, XmNmodifyVerifyCallback, panel_modify_callback, (XtPointer)data);
}

static void post_file_panel_error(const char *error_msg, void *ufd)
{
  file_data *fd = (file_data *)ufd;
  fd->error_widget = fd->scanf_widget;
  post_file_dialog_error(error_msg, ufd);
}

static void file_data_type_callback(Widget w, XtPointer context, XtPointer info) 
{
  int pos;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  file_data *fd;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  XtVaGetValues(w, XmNuserData, &fd, NULL);
  pos = cbs->item_position - 1;
  if (header_type_from_position(pos) != fd->current_type)
    {
      set_header_type_and_format_from_position(fd, pos);
      set_file_dialog_sound_attributes(fd,
				       fd->current_type,
				       fd->current_format,
				       IGNORE_SRATE, IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				       NULL);
    }
}

static void file_data_format_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  file_data *fd;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  XtVaGetValues(w, XmNuserData, &fd, NULL);
  fd->current_format = data_format_from_position(fd->current_type, cbs->item_position - 1);
}


/* ---------------- File Data Panel ---------------- */

static void srate_drop(Widget w, XtPointer context, XtPointer info) 
{
  file_data *fd = (file_data *)context;
  char *sr;
  sr = XtName(w);
  XmTextSetString(fd->srate_text, sr);
}

static void chans_drop(Widget w, XtPointer context, XtPointer info) 
{
  file_data *fd = (file_data *)context;
  char *sr;
  sr = XtName(w);
  XmTextSetString(fd->chans_text, sr);
}

#define PANEL_COMMENT_SPACE 8

file_data *make_file_data_panel(Widget parent, char *name, Arg *in_args, int in_n, 
				dialog_channels_t with_chan, 
				int header_type, int data_format,
				dialog_data_location_t with_loc, dialog_samples_t with_samples,
				dialog_error_t with_error, dialog_header_type_t with_header_type,
				dialog_comment_t with_comment,
				header_choice_t header_choice)
{
  Widget form, header_label, data_label, srate_label, chans_label, sep1, sep2 = NULL, sep3, sep4;
  Widget comment_label = NULL, location_label, samples_label;
  Widget smenu, s8, s22, s44, s48, cmenu, c1 = NULL, c2 = NULL, c4 = NULL, c8 = NULL;
  file_data *fdat;
  Arg args[32];
  int i, n;
  XmString *strs;
  int nformats = 0, nheaders = 0;
  char **formats = NULL, **headers = NULL;

  switch (header_choice)
    {
    case WITH_READABLE_HEADERS: headers = short_readable_headers(&nheaders); break;
    case WITH_WRITABLE_HEADERS: headers = short_writable_headers(&nheaders); break;
    case WITH_BUILTIN_HEADERS:  headers = short_builtin_headers(&nheaders);  break;
    }

  fdat = (file_data *)CALLOC(1, sizeof(file_data));
  fdat->current_type = header_type;
  fdat->current_format = data_format;
  formats = set_header_positions_from_type(fdat, header_type, data_format);
  nformats = fdat->formats;

  /* pick up all args from caller */
  form = XtCreateManagedWidget(name, xmFormWidgetClass, parent, in_args, in_n);

  if (with_header_type == WITH_HEADER_TYPE_FIELD)
    {
      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNwidth, 5); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, form, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->doit_button_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep1); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      header_label = XtCreateManagedWidget(_("header"), xmLabelWidgetClass, form, args, n);
      
      /* what is selected depends on current type */
      strs = (XmString *)CALLOC(nheaders, sizeof(XmString)); 
      for (i = 0; i < nheaders; i++) 
	strs[i] = XmStringCreate(headers[i], XmFONTLIST_DEFAULT_TAG);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, header_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep1); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlistMarginWidth, 1); n++;
      XtSetArg(args[n], XmNuserData, (XtPointer)fdat); n++;
      XtSetArg(args[n], XmNitems, strs); n++;
      XtSetArg(args[n], XmNitemCount, nheaders); n++;
      XtSetArg(args[n], XmNvisibleItemCount, NUM_VISIBLE_HEADERS); n++;
      fdat->header_list = XmCreateScrolledList(form, "header-type", args, n);
      XtManageChild(fdat->header_list);

      for (i = 0; i < nheaders; i++) 
	XmStringFree(strs[i]);
      FREE(strs);
      XmListSelectPos(fdat->header_list, fdat->header_pos + 1, false);
      XtAddCallback(fdat->header_list, XmNbrowseSelectionCallback, file_data_type_callback, NULL);
      
      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, fdat->header_list); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNwidth, 15); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep2 = XtCreateManagedWidget("sep2", xmSeparatorWidgetClass, form, args, n);
    }

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->help_button_color); n++;}
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  if (with_header_type == WITH_HEADER_TYPE_FIELD)
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep2); n++;
    }
  else
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    }
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  data_label = XtCreateManagedWidget(_("data"), xmLabelWidgetClass, form, args, n);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, data_label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  if (with_header_type == WITH_HEADER_TYPE_FIELD)
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep2); n++;
    }
  else
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    }
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNuserData, (XtPointer)fdat); n++;
  fdat->format_list = XmCreateScrolledList(form, "data-format", args, n);

  strs = (XmString *)CALLOC(nformats, sizeof(XmString)); 
  for (i = 0; i < nformats; i++) 
    strs[i] = XmStringCreate(formats[i], XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(fdat->format_list, 
		XmNitems, strs, 
		XmNitemCount, nformats, 
		NULL);
  for (i = 0; i < nformats; i++) 
    XmStringFree(strs[i]);
  FREE(strs);

  XmListSelectPos(fdat->format_list, fdat->format_pos + 1, false);
  XtManageChild(fdat->format_list);
  XtAddCallback(fdat->format_list, XmNbrowseSelectionCallback, file_data_format_callback, NULL);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, fdat->format_list); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, 15); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep3 = XtCreateManagedWidget("sep3", xmSeparatorWidgetClass, form, args, n);

  /* srate: text field, label is actually a drop-down menu that sets text */
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep3); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNmarginHeight, 0); n++;
  srate_label = XmCreateMenuBar(form, "menuBar", args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
  smenu = XmCreatePulldownMenu(srate_label, _("srate:"), args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
  XtSetArg(args[n], XmNsubMenuId, smenu); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtCreateManagedWidget(_("srate:"), xmCascadeButtonWidgetClass, srate_label, args, n);
      
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
  s8 = XtCreateManagedWidget("8000",  xmPushButtonWidgetClass, smenu, args, n);
  s22 = XtCreateManagedWidget("22050",  xmPushButtonWidgetClass, smenu, args, n);
  s44 = XtCreateManagedWidget("44100",  xmPushButtonWidgetClass, smenu, args, n);
  s48 = XtCreateManagedWidget("48000", xmPushButtonWidgetClass, smenu, args, n);
  XtManageChild(srate_label);

  n = 0;
  XtSetArg(args[n], XmNcolumns, 6); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, srate_label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep3); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
  fdat->srate_text = make_textfield_widget("srate-text", form, args, n, NOT_ACTIVATABLE, add_completer_func(srate_completer));

  if (with_chan != WITHOUT_CHANNELS_FIELD)
    {
      /* chans: text field, label is actually a drop-down menu that sets text */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, fdat->srate_text); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      chans_label = XmCreateMenuBar(form, "menuBar1", args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      cmenu = XmCreatePulldownMenu(chans_label, (char *)((with_chan == WITH_CHANNELS_FIELD) ? _("channels:") : _("extract channel:")), 
				   args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNsubMenuId, cmenu); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      XtCreateManagedWidget((char *)((with_chan == WITH_CHANNELS_FIELD) ? _("channels:") : _("extract channel:")), 
			    xmCascadeButtonWidgetClass, chans_label, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      c1 = XtCreateManagedWidget("1",  xmPushButtonWidgetClass, cmenu, args, n);
      c2 = XtCreateManagedWidget("2",  xmPushButtonWidgetClass, cmenu, args, n);
      if (with_chan == WITH_CHANNELS_FIELD)
	{
	  c4 = XtCreateManagedWidget("4",  xmPushButtonWidgetClass, cmenu, args, n);
	  c8 = XtCreateManagedWidget("8", xmPushButtonWidgetClass, cmenu, args, n);
	}
      else
	{
	  /* extract case */
	  c4 = XtCreateManagedWidget("3",  xmPushButtonWidgetClass, cmenu, args, n);
	  c8 = XtCreateManagedWidget("4", xmPushButtonWidgetClass, cmenu, args, n);
	}
      XtManageChild(chans_label);

      n = 0;
      XtSetArg(args[n], XmNcolumns, 6); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, chans_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      fdat->chans_text = make_textfield_widget("chans-text", form, args, n, NOT_ACTIVATABLE, NO_COMPLETER);

      if (with_loc == WITH_DATA_LOCATION_FIELD)
	{
	  n = 0;
	  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, fdat->chans_text); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, sep3); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  location_label = XtCreateManagedWidget(_("data location:"), xmLabelWidgetClass, form, args, n);

	  n = 0;
	  XtSetArg(args[n], XmNcolumns, 6); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, location_label); n++;
	  XtSetArg(args[n], XmNbottomAttachment, (with_samples == WITHOUT_SAMPLES_FIELD) ? XmATTACH_FORM : XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, sep3); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
	  fdat->location_text = make_textfield_widget("location-text", form, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
	}
    }

  if (with_samples == WITH_SAMPLES_FIELD)
    {
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ((fdat->location_text) ? fdat->location_text : 
				       ((fdat->chans_text) ? fdat->chans_text : fdat->srate_text))); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      samples_label = XtCreateManagedWidget(_("samples:"), xmLabelWidgetClass, form, args, n);

      n = 0;
      XtSetArg(args[n], XmNcolumns, 16); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, samples_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      fdat->samples_text = make_textfield_widget("samples-text", form, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
    }

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, form); n++; /* form is the internal XmForm widget holding the lists etc */
  XtSetArg(args[n], XmNbottomAttachment, ((with_comment != WITHOUT_COMMENT_FIELD) || 
					  (with_error == WITH_ERROR_FIELD)) ? XmATTACH_NONE : XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNheight, (with_comment != WITHOUT_COMMENT_FIELD) ? PANEL_COMMENT_SPACE : 2); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep4 = XtCreateManagedWidget("sep4", xmSeparatorWidgetClass, parent, args, n);

  /* try to make the comment field the one that grows */
  if (with_error == WITH_ERROR_FIELD)
    {
      n = 0;
      if (with_comment == WITHOUT_COMMENT_FIELD)
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, sep4); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
	}
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->black); n++;
      XtSetArg(args[n], XmNborderWidth, 2); n++;
      XtSetArg(args[n], XmNmarginWidth, 10); n++;
      XtSetArg(args[n], XmNmarginHeight, 10); n++;
      fdat->error_text = XtCreateWidget("", xmLabelWidgetClass, parent, args, n);
      /* XtUnmanageChild(fdat->error_text); */
    }

  if (with_comment != WITHOUT_COMMENT_FIELD)
    {
      if (with_comment == WITH_COMMENT_FIELD) /* i.e. not WITH_UNLABELLED_COMMENT_FIELD */
	{
	  n = 0;
	  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, sep4); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  comment_label = XtCreateManagedWidget(_("comment:"), xmLabelWidgetClass, parent, args, n);
	}

      n = 0;
      if (with_comment == WITH_COMMENT_FIELD)
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, comment_label); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, comment_label); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, sep4); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	}
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrows, 2); n++;
      XtSetArg(args[n], XmNcolumns, 40); n++;
      fdat->comment_text = make_text_widget("comment-text", parent, args, n);
    }
  else fdat->comment_text = NULL;

  XtAddCallback(s8, XmNactivateCallback, srate_drop, (XtPointer)fdat);
  XtAddCallback(s22, XmNactivateCallback, srate_drop, (XtPointer)fdat);
  XtAddCallback(s44, XmNactivateCallback, srate_drop, (XtPointer)fdat);
  XtAddCallback(s48, XmNactivateCallback, srate_drop, (XtPointer)fdat);

  if (with_chan != WITHOUT_CHANNELS_FIELD)
    {
      XtAddCallback(c1, XmNactivateCallback, chans_drop, (XtPointer)fdat);
      XtAddCallback(c2, XmNactivateCallback, chans_drop, (XtPointer)fdat);
      XtAddCallback(c4, XmNactivateCallback, chans_drop, (XtPointer)fdat);
      XtAddCallback(c8, XmNactivateCallback, chans_drop, (XtPointer)fdat);
    }
  return(fdat);
}

static void reflect_file_data_panel_change(file_data *fd, void *data, void (*change_action)(Widget w, XtPointer context, XtPointer info))
{
  if (fd->srate_text)
    XtAddCallback(fd->srate_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->chans_text)
    XtAddCallback(fd->chans_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->samples_text)
    XtAddCallback(fd->samples_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->location_text)
    XtAddCallback(fd->location_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->comment_text)
    XtAddCallback(fd->comment_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->format_list)
    XtAddCallback(fd->format_list, XmNbrowseSelectionCallback, change_action, (XtPointer)data);
  if (fd->header_list)
    XtAddCallback(fd->header_list, XmNbrowseSelectionCallback, change_action, (XtPointer)data);
}

static void unreflect_file_data_panel_change(file_data *fd, void *data, void (*change_action)(Widget w, XtPointer context, XtPointer info))
{
  if (fd->srate_text)
    XtRemoveCallback(fd->srate_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->chans_text)
    XtRemoveCallback(fd->chans_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->samples_text)
    XtRemoveCallback(fd->samples_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->location_text)
    XtRemoveCallback(fd->location_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->comment_text)
    XtRemoveCallback(fd->comment_text, XmNvalueChangedCallback, change_action, (XtPointer)data);
  if (fd->format_list)
    XtRemoveCallback(fd->format_list, XmNbrowseSelectionCallback, change_action, (XtPointer)data);
  if (fd->header_list)
    XtRemoveCallback(fd->header_list, XmNbrowseSelectionCallback, change_action, (XtPointer)data);
}


/* -------- save as dialog (file and edit menus) -------- */

typedef struct {
  file_data *panel_data;
  Widget dialog, filename_widget;
  char *filename;
  save_dialog_t type;
  file_pattern_info *fp;
  dialog_play_info *dp;
  fam_info *file_watcher;
  int selection_watcher_loc;
} save_as_dialog_info;

static save_as_dialog_info *save_sound_as = NULL, *save_selection_as = NULL;

static save_as_dialog_info *new_save_as_dialog_info(save_dialog_t type)
{
  save_as_dialog_info *sd;
  sd = (save_as_dialog_info *)CALLOC(1, sizeof(save_as_dialog_info));
  sd->type = type;
  sd->selection_watcher_loc = -1;
  return(sd);
}

static void save_as_selection_watcher(selection_watcher_reason_t reason, void *data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  if ((reason == SELECTION_ACTIVE) ||
      (selection_is_active()))
    {
      clear_dialog_error(sd->panel_data);
      remove_selection_watcher(sd->selection_watcher_loc);
      sd->selection_watcher_loc = -1;
    }
}

static void save_as_filename_modify_callback(Widget w, XtPointer context, XtPointer info);

static void save_as_undoit(save_as_dialog_info *sd)
{
  XmString ok_label;
  ok_label = XmStringCreate(_("Save"), XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(sd->dialog,
		XmNokLabelString, ok_label, 
		NULL);
  XmStringFree(ok_label);
  clear_dialog_error(sd->panel_data);
  XtRemoveCallback(sd->filename_widget, XmNmodifyVerifyCallback, save_as_filename_modify_callback, (XtPointer)(sd->panel_data));
  sd->file_watcher = fam_unmonitor_file(sd->filename, sd->file_watcher);
}

static void save_as_filename_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  save_as_undoit((save_as_dialog_info *)context);
  cbs->doit = true;
}

static void clear_error_if_save_as_filename_changes(Widget dialog, void *data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  XtAddCallback(sd->filename_widget, XmNmodifyVerifyCallback, save_as_filename_modify_callback, (XtPointer)data);
}

static void watch_save_as_file(struct fam_info *fp, FAMEvent *fe)
{
#if HAVE_FAM
  /* if file is deleted, respond in some debonair manner */
  switch (fe->code)
    {
    case FAMChanged:
    case FAMDeleted:
    case FAMCreated:
    case FAMMoved:
      save_as_undoit((save_as_dialog_info *)(fp->data));
      break;

    default:
      /* ignore the rest */
      break;
    }
#endif
}

static void save_as_watch_user_read_only(struct snd_info *sp, sp_watcher_reason_t reason, int loc)
{
  file_data *pdat = (file_data *)(sp->watchers[loc]->context);
  clear_dialog_error(pdat);
  remove_sp_watcher(sp, loc);
}

/* TODO: merge save and extract callback duplicated code */

static void save_as_ok_callback(Widget w, XtPointer context, XtPointer info)
{ 
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  char *str = NULL, *comment = NULL, *msg = NULL, *fullname = NULL, *tmpfile = NULL;
  snd_info *sp = NULL;
  int type, format, srate, chans, output_type;
  bool file_exists;
  off_t location, samples;
  io_error_t io_err = IO_NO_ERROR;

  clear_dialog_error(sd->panel_data);

  if ((sd->type == SELECTION_SAVE_AS) &&
      (!(selection_is_active())))
    {
      msg = _("no selection to save");
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      if (sd->selection_watcher_loc < 0)
	sd->selection_watcher_loc = add_selection_watcher(save_as_selection_watcher, (void *)sd);
      return;
    }

  sp = any_selected_sound();
  if (!sp)
    {
      msg = _("nothing to save");
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));
      return;
    }

  /* get output filename */
  str = XmTextGetString(sd->filename_widget);
  if ((!str) || (!*str))
    {
      msg = _("can't save: no file name given");
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));
      return;
    }

  /* get output file attributes */
  redirect_snd_error_to(post_file_panel_error, (void *)(sd->panel_data));
  comment = get_file_dialog_sound_attributes(sd->panel_data, &srate, &chans, &type, &format, &location, &samples, 0);
  output_type = type;
  redirect_snd_error_to(NULL, NULL);
  if (sd->panel_data->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(sd->dialog, (void *)(sd->panel_data));
      if (comment) FREE(comment);
      XtFree(str);
      return;
    }

  if (sd->type == SOUND_SAVE_AS)
    clear_minibuffer(sp);

  fullname = mus_expand_filename(str);
  if (run_before_save_as_hook(sp, fullname, sd->type != SOUND_SAVE_AS, srate, type, format, comment))
    {
      msg = mus_format(_("save cancelled by %s"), S_before_save_as_hook);
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));      
      FREE(msg);
      FREE(fullname);
      if (comment) FREE(comment);
      XtFree(str);
      return;
    }

  file_exists = mus_file_probe(fullname);
  if ((sd->type == SOUND_SAVE_AS) &&
      (strcmp(fullname, sp->filename) == 0))
    {
      /* save-as here is the same as save */
      if ((sp->user_read_only) || 
	  (sp->file_read_only))
	{
	  msg = mus_format(_("can't overwrite %s (it is write-protected)"), sp->short_filename);
	  post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
	  clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data)); 
	  if (sp->user_read_only)
	    add_sp_watcher(sp, SP_READ_ONLY_WATCHER, save_as_watch_user_read_only, (void *)(sd->panel_data));
	  FREE(msg);
	  FREE(fullname);
	  if (comment) FREE(comment);
	  XtFree(str);
	  return;
	}
    }
  else
    {
      if (!(sd->file_watcher))
	{
	  /* check for overwrites that are questionable -- DoIt click will return here with sd->file_watcher active */
	  snd_info *parlous_sp = NULL;
	  if ((file_exists) &&
	      ((ask_before_overwrite(ss)) ||
	       ((sd->type == SOUND_SAVE_AS) &&
		(parlous_sp = file_is_open_elsewhere_and_has_unsaved_edits(sp, fullname)))))	   
	    {
	      XmString ok_label;
	      msg = mus_format(_("%s exists%s. To overwrite it, click 'DoIt'"), 
			       str,
			       (parlous_sp) ? ", and has unsaved edits" : "");
	      sd->file_watcher = fam_monitor_file(fullname, (void *)sd, watch_save_as_file);
	      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
	      clear_error_if_save_as_filename_changes(sd->dialog, (void *)(sd->panel_data));
	      ok_label = XmStringCreate(_("DoIt"), XmFONTLIST_DEFAULT_TAG);
	      XtVaSetValues(sd->dialog, 
			    XmNokLabelString, ok_label, 
			    NULL);
	      XmStringFree(ok_label);
	      FREE(msg);
	      FREE(fullname);
	      if (comment) FREE(comment);
	      XtFree(str);
	      return;
	    }
	}
    }

  /* try to save... if it exists already, first write as temp, then move */
  if (sd->file_watcher)
    save_as_undoit(sd);
  ss->local_errno = 0;

  if (encoded_header_p(type))
    {
      output_type = type;
      format = MUS_LSHORT;
      type = MUS_RIFF;
      tmpfile = snd_tempnam();
    }
  else
    {
      tmpfile = fullname;
    }

  redirect_snd_error_to(post_file_dialog_error, (void *)(sd->panel_data));
  if (sd->type == SOUND_SAVE_AS)
    io_err = save_edits_without_display(sp, tmpfile, type, format, srate, comment, AT_CURRENT_EDIT_POSITION);
  else
    {
      char *ofile;
      if (file_exists) /* file won't exist if we're encoding, so this isn't as wasteful as it looks */
	ofile = snd_tempnam();
      else ofile = copy_string(tmpfile);
      io_err = save_selection(ofile, type, format, srate, comment, SAVE_ALL_CHANS);
      if (io_err == IO_NO_ERROR)
	io_err = move_file(ofile, fullname);
      FREE(ofile);
    }
  redirect_snd_error_to(NULL, NULL);

  if (io_err == IO_NO_ERROR)
    {
      if (encoded_header_p(output_type))
	{
	  snd_encode(output_type, tmpfile, fullname);
	  snd_remove(tmpfile, REMOVE_FROM_CACHE);
	  FREE(tmpfile);
	}

      if (!file_exists)
	force_directory_reread(sd->dialog);
      if (sd->type == SOUND_SAVE_AS)
	report_in_minibuffer(sp, "%s saved as %s", sp->short_filename, str);
      else report_in_minibuffer(sp, "selection saved as %s", str);
      run_after_save_as_hook(sp, str, true); /* true => from dialog */
      XtUnmanageChild(sd->dialog);
    }
  else
    {
      msg = mus_format("save as %s error: %s", str, io_error_name(io_err));
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));
      FREE(msg);
    }

  FREE(fullname);
  XtFree(str);
  if (comment) FREE(comment);
} 

static void save_as_extract_callback(Widget w, XtPointer context, XtPointer info) 
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  char *str = NULL, *comment, *msg = NULL, *fullname = NULL, *tmpfile = NULL;
  snd_info *sp = NULL;
  int type, format, srate, chan = 0, extractable_chans = 0, output_type;
  bool file_exists = false;
  off_t location, samples;
  io_error_t io_err;

  clear_dialog_error(sd->panel_data);

  if ((sd->type == SELECTION_SAVE_AS) &&
      (!(selection_is_active())))
    {
      msg = _("can't extract: no selection");
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      if (sd->selection_watcher_loc < 0)
	sd->selection_watcher_loc = add_selection_watcher(save_as_selection_watcher, (void *)sd);
      return;
    }

  /* get output filename */
  str = XmTextGetString(sd->filename_widget);
  if ((!str) || (!*str))
    {
      msg = _("can't extract: no file name given");
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));
      return;
    }

  /* get output file attributes */
  redirect_snd_error_to(post_file_panel_error, (void *)(sd->panel_data));
  comment = get_file_dialog_sound_attributes(sd->panel_data, &srate, &chan, &type, &format, &location, &samples, 0);
  output_type = type;
  redirect_snd_error_to(NULL, NULL);
  if (sd->panel_data->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(sd->dialog, (void *)(sd->panel_data));
      if (comment) FREE(comment);
      XtFree(str);
      return;
    }

  /* check that chan-to-extract choice makes sense */
  sp = any_selected_sound();
  if (sd->type == SOUND_SAVE_AS)
    {
      clear_minibuffer(sp);
      extractable_chans = sp->nchans;
    }
  else extractable_chans = selection_chans();
  if ((chan > extractable_chans) ||
      (((extractable_chans > 1) && (chan == extractable_chans)) ||
       (chan < 0)))
    {
      if (chan > extractable_chans)
	msg = mus_format("can't extract chan %d (%s has %d chan%s)", 
			 chan, 
			 (sd->type == SOUND_SAVE_AS) ? "sound" : "selection",
			 extractable_chans, 
			 (extractable_chans > 1) ? "s" : "");
      else msg = mus_format("can't extract chan %d (first chan is numbered 0)", chan);
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_chans_changes(sd->dialog, (void *)(sd->panel_data));
      FREE(msg);
      if (comment) FREE(comment);
      XtFree(str);
      return;
    }

  /* check before-save-as-hook (non-#f return -> do not save) */
  fullname = mus_expand_filename(str);
  if (run_before_save_as_hook(sp, fullname, sd->type != SOUND_SAVE_AS, srate, type, format, comment))
    {
      msg = mus_format(_("extract cancelled by %s"), S_before_save_as_hook);
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));      
      FREE(msg);
      FREE(fullname);
      if (comment) FREE(comment);
      XtFree(str);
      return;
    }

  file_exists = mus_file_probe(fullname);
  if ((sd->type == SOUND_SAVE_AS) &&
      (strcmp(fullname, sp->filename) == 0))
    {
      /* save-as here is the same as save */
      if ((sp->user_read_only) || 
	  (sp->file_read_only))
	{
	  msg = mus_format(_("can't overwrite %s (it is write-protected)"), sp->short_filename);
	  post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
	  clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data)); 
	  if (sp->user_read_only)
	    add_sp_watcher(sp, SP_READ_ONLY_WATCHER, save_as_watch_user_read_only, (void *)(sd->panel_data));
	  FREE(msg);
	  FREE(fullname);
	  if (comment) FREE(comment);
	  XtFree(str);
	  return;
	}
    }
  else
    {
      if (!(sd->file_watcher))
	{
	  /* check for overwrites that are questionable -- DoIt click will return here with sd->file_watcher active */
	  snd_info *parlous_sp = NULL;
	  if ((file_exists) &&
	      ((ask_before_overwrite(ss)) ||
	       ((sd->type == SOUND_SAVE_AS) &&
		(parlous_sp = file_is_open_elsewhere_and_has_unsaved_edits(sp, fullname)))))	   
	    {
	      XmString ok_label;
	      msg = mus_format(_("%s exists%s. To overwrite it, click 'DoIt'"), 
			       str,
			       (parlous_sp) ? ", and has unsaved edits" : "");
	      sd->file_watcher = fam_monitor_file(fullname, (void *)sd, watch_save_as_file);
	      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
	      clear_error_if_save_as_filename_changes(sd->dialog, (void *)(sd->panel_data));
	      ok_label = XmStringCreate(_("DoIt"), XmFONTLIST_DEFAULT_TAG);
	      XtVaSetValues(sd->dialog, 
			    XmNokLabelString, ok_label, 
			    NULL);
	      XmStringFree(ok_label);
	      FREE(msg);
	      FREE(fullname);
	      if (comment) FREE(comment);
	      XtFree(str);
	      return;
	    }
	}
    }

  /* try to save... if it exists already (or needs encoding), first write as temp, then move (or encode) */
  if (sd->file_watcher)
    save_as_undoit(sd);
  ss->local_errno = 0;

  if (encoded_header_p(type))
    {
      output_type = type;
      format = MUS_LSHORT;
      type = MUS_RIFF;
      tmpfile = snd_tempnam();
    }
  else
    {
      tmpfile = fullname;
    }

  redirect_snd_error_to(post_file_dialog_error, (void *)(sd->panel_data));
  if (sd->type == SOUND_SAVE_AS)
    io_err = save_channel_edits(sp->chans[chan], tmpfile, AT_CURRENT_EDIT_POSITION); /* protects if same name */
  else 
    {
      char *ofile;
      if (file_exists)
	ofile = snd_tempnam();
      else ofile = copy_string(fullname);
      io_err = save_selection(ofile, type, format, srate, comment, chan);
      if (io_err == IO_NO_ERROR)
	io_err = move_file(ofile, fullname);
      FREE(ofile);
    }
  redirect_snd_error_to(NULL, NULL);
  if (io_err == IO_NO_ERROR)
    {
      if (encoded_header_p(output_type))
	{
	  snd_encode(output_type, tmpfile, fullname);
	  snd_remove(tmpfile, REMOVE_FROM_CACHE);
	  FREE(tmpfile);
	}

      if (!file_exists)
	force_directory_reread(sd->dialog);
      if (sd->type == SOUND_SAVE_AS)
	report_in_minibuffer(sp, "%s chan %d saved as %s", sp->short_filename, chan, str);
      else report_in_minibuffer(sp, "selection chan %d saved as %s", chan, str);
      run_after_save_as_hook(sp, str, true); /* true => from dialog */
      XtUnmanageChild(sd->dialog);
    }
  else
    {
      msg = mus_format("extract chan as %s error: %s", str, io_error_name(io_err));
      /* TODO: drag up the actual error here! */
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));
      FREE(msg);
    }
  FREE(fullname);
  XtFree(str);
  if (comment) FREE(comment);
}

static void save_as_dialog_select_callback(Widget w, XtPointer context, XtPointer info)
{
  dialog_play_info *dp = (dialog_play_info *)context;
  char *filename = NULL;
  XmString *strs;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  XtVaGetValues(w, XmNselectedItems, &strs, NULL);
  filename = (char *)XmStringUnparse(strs[0], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((filename) && (sound_file_p(filename)))
    XtManageChild(dp->play_button);
  else
    {
      if (XtIsManaged(dp->play_button)) 
	XtUnmanageChild(dp->play_button);
    }
  if (filename) XtFree(filename);
}

static void save_as_cancel_callback(Widget w, XtPointer context, XtPointer info)
{ 
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  XtUnmanageChild(sd->dialog);
} 

static void save_as_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  save_as_dialog_help();
}

static void save_as_file_exists_check(Widget w, XtPointer context, XtPointer info)
{
  Widget dialog = (Widget)context;
  char *filename = NULL;
  XmString s1;
  filename = XmTextGetString(w);
  if ((filename) && (*filename))
    {
      if ((mus_file_probe(filename)) && 
	  (!directory_p(filename)))
	{
#if HAVE_ACCESS
	  if (access(filename, W_OK) < 0)
	    s1 = XmStringCreate(_("save as (file write-protected?):"), XmFONTLIST_DEFAULT_TAG);
	  else
#endif
	  s1 = XmStringCreate(_("save as (overwriting):"), XmFONTLIST_DEFAULT_TAG);
	}
      else
	{
	  if (!(directory_exists(filename)))
	    s1 = XmStringCreate(_("save as (no such directory?):"), XmFONTLIST_DEFAULT_TAG);
	  else s1 = XmStringCreate(_("save as:"), XmFONTLIST_DEFAULT_TAG);
	}
    }
  else s1 = XmStringCreate(_("save as:"), XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(dialog, 
		XmNselectionLabelString, s1, 
		NULL);
  if (filename) XtFree(filename);
}

static void make_save_as_dialog(save_as_dialog_info *sd, char *sound_name, int header_type, int format_type)
{
  char *file_string;

  if (!(sd->dialog))
    {
      Arg args[32];
      int n;
      XmString xmstr1, xmstr2, s1;
      XmString filter_list_label, cancel_label, filter_button_label;
      Widget extractB, mainform;

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      s1 = XmStringCreate(_("save as:"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNselectionLabelString, s1); n++;

      xmstr1 = XmStringCreate(_("Save"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNokLabelString, xmstr1); n++;

      file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, _("save %s"), sound_name);

      xmstr2 = XmStringCreate(file_string, XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNdialogTitle, xmstr2); n++;

      filter_list_label = XmStringCreate(_("files listed:"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNfilterLabelString, filter_list_label); n++;

      filter_button_label = XmStringCreate(_("Update"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNapplyLabelString, filter_button_label); n++;

      cancel_label = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNcancelLabelString, cancel_label); n++;

      sd->fp = (file_pattern_info *)CALLOC(1, sizeof(file_pattern_info));
      sd->fp->in_just_sounds_update = false;
      sd->dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));

      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNchildPlacement, XmPLACE_ABOVE_SELECTION); n++;
      XtSetArg(args[n], XmNallowOverlap, false); n++;
      XtSetArg(args[n], XmNheight, 600); n++;
      XtSetArg(args[n], XmNuserData, (XtPointer)sd->fp); n++;
      sd->dialog = XmCreateFileSelectionDialog(MAIN_SHELL(ss), "save-as", args, n); /* TODO: why does this stay on top? */
      FREE(file_string);

      XmStringFree(s1);
      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      XmStringFree(filter_list_label);
      XmStringFree(filter_button_label);
      XmStringFree(cancel_label);

      sd->filename_widget = XtNameToWidget(sd->dialog, "Text");
      if (!(sd->filename_widget)) sd->filename_widget = XmFileSelectionBoxGetChild(sd->dialog, XmDIALOG_TEXT);

      XtAddCallback(sd->dialog, XmNhelpCallback, save_as_help_callback, (XtPointer)sd);
      XtAddCallback(sd->dialog, XmNcancelCallback, save_as_cancel_callback, (XtPointer)sd);
      XtAddCallback(sd->dialog, XmNokCallback, save_as_ok_callback, (XtPointer)sd);

      sd->fp->dialog = sd->dialog;
      sd->dp->dialog = sd->dialog;

      mainform = XtVaCreateManagedWidget("filebuttons-mainform", xmFormWidgetClass, sd->dialog, NULL);
      add_play_and_just_sounds_buttons(sd->dialog, mainform, sd->fp, sd->dp);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sd->fp->just_sounds_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sd->panel_data = make_file_data_panel(mainform, "data-form", args, n, 
					    WITH_EXTRACT_CHANNELS_FIELD, 
					    header_type, format_type, 
					    WITHOUT_DATA_LOCATION_FIELD, 
					    WITHOUT_SAMPLES_FIELD,
					    WITH_ERROR_FIELD, 
					    WITH_HEADER_TYPE_FIELD, 
					    WITH_COMMENT_FIELD,
					    WITH_WRITABLE_HEADERS);
      sd->panel_data->dialog = sd->dialog;

      color_file_selection_box(sd->dialog);
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(sd->panel_data->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(sd->panel_data->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(sd->fp->just_sounds_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(sd->dp->play_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
	}
      XtAddCallback(XmFileSelectionBoxGetChild(sd->dialog, XmDIALOG_LIST),
		    XmNbrowseSelectionCallback, save_as_dialog_select_callback, (XtPointer)(sd->dp));
      XtAddCallback(sd->filename_widget, XmNvalueChangedCallback, save_as_file_exists_check, (XtPointer)(sd->dialog));

      /* this must come after the file data panel so that Motif puts it in the button box, not the main work area */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
	  XtSetArg(args[n], XmNarmColor,   ss->sgx->pushed_button_color); n++;
	}
      extractB = XtCreateManagedWidget(_("Extract"), xmPushButtonGadgetClass, sd->dialog, args, n);
      XtAddCallback(extractB, XmNactivateCallback, save_as_extract_callback, (XtPointer)sd);

      XtManageChild(sd->dialog);
      if (sd->type == SOUND_SAVE_AS)
	set_dialog_widget(SOUND_SAVE_AS_DIALOG, sd->dialog);
      else set_dialog_widget(SELECTION_SAVE_AS_DIALOG, sd->dialog);
    }
  else
    {
      XmString xmstr2;
      file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, _("save %s"), sound_name);
      xmstr2 = XmStringCreate(file_string, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(sd->dialog, 
		    XmNdialogTitle, xmstr2, 
		    NULL);
      XmStringFree(xmstr2);
      FREE(file_string);
    }
}

widget_t make_sound_save_as_dialog(bool managed)
{
  snd_info *sp = NULL;
  char *com = NULL;
  file_info *hdr = NULL;
  save_as_dialog_info *sd;

  if (!save_sound_as)
    save_sound_as = new_save_as_dialog_info(SOUND_SAVE_AS);
  sd = save_sound_as;

  sp = any_selected_sound();
  if (sp) hdr = sp->hdr;
  make_save_as_dialog(sd,
		      (char *)((sp) ? sp->short_filename : ""),
		      default_output_header_type(ss),
		      default_output_data_format(ss));
  set_file_dialog_sound_attributes(sd->panel_data,
				   sd->panel_data->current_type,
				   sd->panel_data->current_format,
				   (hdr) ? hdr->srate : selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES,
				   com = output_comment(hdr));
  if (com) FREE(com);
  if ((managed) && (!XtIsManaged(sd->dialog))) 
    XtManageChild(sd->dialog);
  return(sd->dialog);
}

widget_t make_selection_save_as_dialog(bool managed)
{
  save_as_dialog_info *sd;

  if (!save_selection_as)
    save_selection_as = new_save_as_dialog_info(SELECTION_SAVE_AS);
  sd = save_selection_as;

  make_save_as_dialog(sd,
		      _("current selection"),
		      default_output_header_type(ss),
		      default_output_data_format(ss));
  set_file_dialog_sound_attributes(sd->panel_data,
				   sd->panel_data->current_type,
				   sd->panel_data->current_format,
				   selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   NULL);
  if ((managed) && (!XtIsManaged(sd->dialog))) 
    XtManageChild(sd->dialog);
  return(sd->dialog);
}



/* -------- save/restore for all these dialogs -------- */

void save_file_dialog_state(FILE *fd)
{
  if ((odat) && (XtIsManaged(odat->dialog)))
    {
      /* odat->file_dialog_read_only -> "view-sound" dialog -- this distinction currently ignored */
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_open_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_open_file_dialog));
#endif
    }
  if ((mdat) && (XtIsManaged(mdat->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_mix_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_mix_file_dialog));
#endif
    }
  if ((idat) && (XtIsManaged(idat->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_insert_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_insert_file_dialog));
#endif
    }
  if ((save_sound_as) && (XtIsManaged(save_sound_as->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_save_sound_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_save_sound_dialog));
#endif
    }
  if ((save_selection_as) && (XtIsManaged(save_selection_as->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_save_selection_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_save_selection_dialog));
#endif
    }
}



/* -------------------------------- New File -------------------------------- */

static Widget new_file_dialog = NULL;
static file_data *ndat = NULL;
static off_t initial_samples = 1;
static Widget new_file_text = NULL;
static char *new_file_filename = NULL;
static fam_info *new_file_watcher = NULL;

void cleanup_new_file_watcher(void)
{
  if (new_file_watcher)
    new_file_watcher = fam_unmonitor_file(new_file_filename, new_file_watcher);
}

static void new_filename_modify_callback(Widget w, XtPointer context, XtPointer info);

static void new_file_undoit(void)
{
  XmString ok_label;
  ok_label = XmStringCreate(_("Ok"), XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(new_file_dialog, 
		XmNokLabelString, ok_label, 
		NULL);
  XmStringFree(ok_label);
  clear_dialog_error(ndat);
  XtRemoveCallback(new_file_text, XmNmodifyVerifyCallback, new_filename_modify_callback, (XtPointer)ndat);
  new_file_watcher = fam_unmonitor_file(new_file_filename, new_file_watcher);
}

static void new_filename_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  new_file_undoit();
  cbs->doit = true;
}

static void clear_error_if_new_filename_changes(Widget dialog, void *data)
{
  XtAddCallback(new_file_text, XmNmodifyVerifyCallback, new_filename_modify_callback, (XtPointer)data);
}

static void watch_new_file(struct fam_info *fp, FAMEvent *fe)
{
#if HAVE_FAM
  /* if file is deleted, respond in some debonair manner */
  switch (fe->code)
    {
    case FAMChanged:
    case FAMDeleted:
    case FAMCreated:
    case FAMMoved:
      new_file_undoit();
      break;

    default:
      /* ignore the rest */
      break;
    }
#endif
}

static void new_file_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  off_t loc;
  char *comment = NULL, *newer_name = NULL, *msg;
  int header_type, data_format, srate, chans;
  newer_name = XmTextGetString(new_file_text);
  if ((!newer_name) || (!(*newer_name)))
    {
      msg = _("new sound needs a file name ('New file:' field is empty)");
      post_file_dialog_error((const char *)msg, (void *)ndat);
      clear_error_if_new_filename_changes(new_file_dialog, (void *)ndat);
    }
  else
    {
      redirect_snd_error_to(post_file_panel_error, (void *)ndat);
      comment = get_file_dialog_sound_attributes(ndat, &srate, &chans, &header_type, &data_format, &loc, &initial_samples, 1);
      redirect_snd_error_to(NULL, NULL);
      if (ndat->error_widget != NOT_A_SCANF_WIDGET)
	{
	  clear_error_if_panel_changes(new_file_dialog, (void *)ndat);
	}
      else
	{
	  snd_info *sp;
	  /* handle the overwrite hook directly */
	  if (new_file_filename) FREE(new_file_filename);
	  new_file_filename = mus_expand_filename(newer_name); /* need full filename for fam */
	  if ((!new_file_watcher) &&
	      (ask_before_overwrite(ss)) && 
	      (mus_file_probe(new_file_filename)))
	    {
	      XmString ok_label;
	      msg = mus_format(_("%s exists. If you want to overwrite it, click 'DoIt'"), newer_name);
	      new_file_watcher = fam_monitor_file(new_file_filename, NULL, watch_new_file);
	      post_file_dialog_error((const char *)msg, (void *)ndat);
	      clear_error_if_new_filename_changes(new_file_dialog, (void *)ndat);
	      ok_label = XmStringCreate(_("DoIt"), XmFONTLIST_DEFAULT_TAG);
	      XtVaSetValues(new_file_dialog, 
			    XmNokLabelString, ok_label, 
			    NULL);
	      XmStringFree(ok_label);
	      FREE(msg);
	    }
	  else
	    {
	      if (new_file_watcher)
		new_file_undoit();
	      ss->local_errno = 0;
	      redirect_snd_error_to(post_file_dialog_error, (void *)ndat);
	      sp = snd_new_file(new_file_filename, header_type, data_format, srate, chans, comment, initial_samples);
	      redirect_snd_error_to(NULL, NULL);
	      if (!sp)
		{
		  if ((ss->local_errno) &&
		    /* some sort of file system error -- this is confusing because fam sends
		     *   a "deleted" event if we don't have write permission -- as if we had
		     *   created it, then immediately deleted it.  So, if the file doesn't
		     *   already exist, I can't monitor for some relevant change (except
		     *   perhaps at the directory level, but that's getting ridiculous).
		     */
		      (mus_file_probe(new_file_filename)))
		    /* that is, the thing exists, so user could delete it or change its permission bits;
		     *  in any case, we won't be confused by an immediate irrelevant delete event
		     */
		    new_file_watcher = fam_monitor_file(new_file_filename, NULL, watch_new_file);
		  clear_error_if_new_filename_changes(new_file_dialog, (void *)ndat);
		}
	      else
		{
		  XtUnmanageChild(new_file_dialog);
		}
	    }
	}
      XtFree(newer_name);
      if (comment) FREE(comment);
    }
}

static void load_new_file_defaults(char *newname)
{
  static int new_ctr = 1;
  char *filename = NULL, *extension = NULL, *new_comment = NULL;
  int header_type, data_format, chans, srate;

  header_type = default_output_header_type(ss);
  chans =       default_output_chans(ss);
  data_format = default_output_data_format(ss);
  srate =       default_output_srate(ss);
  new_comment = output_comment(NULL);

  if ((newname) && (!(*newname))) newname = NULL;
  filename = output_name(newname); /* calls output-name-hook, always free */
  if (filename == NULL)
    {
      filename = (char *)CALLOC(64, sizeof(char));
      switch (header_type)
	{
	case MUS_AIFC: extension = "aiff"; break;
	case MUS_AIFF: extension = "aiff"; break;
	case MUS_RIFF: extension = "wav";  break;
	default:       extension = "snd";  break;
	}
      mus_snprintf(filename, 64, _("new-%d.%s"), new_ctr++, extension);
    }
  XmTextSetString(new_file_text, filename);  
  mus_sound_forget(filename);

  set_file_dialog_sound_attributes(ndat, header_type, data_format, srate, chans, IGNORE_DATA_LOCATION, initial_samples, new_comment);

  if (new_comment) FREE(new_comment);
  if (filename) FREE(filename);
}

static void new_file_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  char *current_name;
  current_name = XmTextGetString(new_file_text);
  load_new_file_defaults(current_name);
  if (current_name) XtFree(current_name);
  if (new_file_watcher)
    new_file_undoit();
}

static void new_file_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(w);
}

static void new_file_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  new_file_dialog_help();
}

void make_new_file_dialog(void)
{
  if (!new_file_dialog)
    {
      Arg args[20];
      int n;
      XmString xok, xcancel, xhelp;
      Widget name_label, form;
      XmString titlestr;
      Widget sep, reset_button;

      titlestr = XmStringCreate(_("New file"), XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      xcancel = XmStringCreate(_("Cancel"), XmFONTLIST_DEFAULT_TAG);
      xok = XmStringCreate(_("Ok"), XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNcancelLabelString, xcancel); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xok); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      new_file_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), "new", args, n);

      XmStringFree(titlestr);
      XmStringFree(xok);
      XmStringFree(xcancel);
      XmStringFree(xhelp);

      XtAddCallback(new_file_dialog, XmNhelpCallback,   new_file_help_callback,   NULL);
      XtAddCallback(new_file_dialog, XmNcancelCallback, new_file_cancel_callback, NULL);
      XtAddCallback(new_file_dialog, XmNokCallback,     new_file_ok_callback,     NULL);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
	  XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
	}
      reset_button = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, new_file_dialog, args, n);
      XtAddCallback(reset_button, XmNactivateCallback, new_file_reset_callback, NULL);

      n = 0;
      form = XtCreateManagedWidget("newfile", xmFormWidgetClass, new_file_dialog, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;}
      XtSetArg(args[n], XmNforeground, ss->sgx->black); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      name_label = XtCreateManagedWidget(_("New file:"), xmLabelWidgetClass, form, args, n);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, name_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      new_file_text = make_textfield_widget("newtext", form, args, n, ACTIVATABLE, add_completer_func(filename_completer));

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, new_file_text); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, form, args, n);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      ndat = make_file_data_panel(form, "data-form", args, n, 
				  WITH_CHANNELS_FIELD, 
				  default_output_header_type(ss), 
				  default_output_data_format(ss), 
				  WITHOUT_DATA_LOCATION_FIELD, 
				  WITH_SAMPLES_FIELD,
				  WITH_ERROR_FIELD, 
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD,
				  WITH_BUILTIN_HEADERS);
      ndat->dialog = new_file_dialog;
      XtManageChild(ndat->error_text);
      XtManageChild(new_file_dialog);

      if (!(ss->using_schemes)) map_over_children(new_file_dialog, set_main_color_of_widget, NULL);
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(ndat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(ndat->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	}
      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(new_file_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_file_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_file_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_file_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_file_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_file_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
	}
      set_dialog_widget(NEW_FILE_DIALOG, new_file_dialog);
      XtUnmanageChild(ndat->error_text); 

      load_new_file_defaults(NULL);
    }
#if (!HAVE_FAM)
  else
    {
      /* if overwrite question pends, but file has been deleted in the meantime, go back to normal state */
      if (new_file_watcher)
	{
	  char *new_name;
	  new_name = XmTextGetString(new_file_text);
	  if ((!new_name) || (!(*new_name)) ||
	      (!(mus_file_probe(new_name))))
	    new_file_undoit();
	  if (new_name) XtFree(new_name);
	}
    }
#endif
  if (!(XtIsManaged(new_file_dialog))) 
    XtManageChild(new_file_dialog);
}



/* ---------------- Edit Header ---------------- */

typedef struct edhead_info {
  Widget dialog;
  file_data *edat;
  snd_info *sp;
  bool panel_changed;
  fam_info *file_ro_watcher;
  int sp_ro_watcher_loc;
} edhead_info;

static int edhead_info_size = 0;
static edhead_info **edhead_infos = NULL;

static edhead_info *new_edhead_dialog(void)
{
  int loc = -1;
  if (edhead_info_size == 0)
    {
      loc = 0;
      edhead_info_size = 4;
      edhead_infos = (edhead_info **)CALLOC(edhead_info_size, sizeof(edhead_info *));
    }
  else
    {
      int i;
      for (i = 0; i < edhead_info_size; i++)
	if ((!edhead_infos[i]) ||
	    (!(XtIsManaged(edhead_infos[i]->dialog))))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = edhead_info_size;
	  edhead_info_size += 4;
	  edhead_infos = (edhead_info **)REALLOC(edhead_infos, edhead_info_size * sizeof(edhead_info *));
	  for (i = loc; i < edhead_info_size; i++) edhead_infos[i] = NULL;
	}
    }
  if (!edhead_infos[loc])
    {
      edhead_infos[loc] = (edhead_info *)CALLOC(1, sizeof(edhead_info));
      edhead_infos[loc]->dialog = NULL;
      edhead_infos[loc]->panel_changed = false;
    }
  edhead_infos[loc]->sp = NULL;
  edhead_infos[loc]->file_ro_watcher = NULL;
  return(edhead_infos[loc]);
}

void cleanup_edit_header_watcher(void)
{
  int i;
  for (i = 0; i < edhead_info_size; i++)
    if (edhead_infos[i])
      {
	edhead_info *ep;
	ep = edhead_infos[i];
	if (ep->file_ro_watcher)
	  ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
      }
}

static XmString make_header_dialog_title(edhead_info *ep, snd_info *sp)
{
  /* dialog may not yet exist */
  char *str;
  XmString xstr;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (sp->user_read_only || sp->file_read_only)
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, _("Add header to (write-protected) %s"), sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of (write-protected) %s"), sp->short_filename);
      if (ep->dialog)
	set_sensitive(XmMessageBoxGetChild(ep->dialog, XmDIALOG_OK_BUTTON), (sp->hdr->type == MUS_RAW));
    }
  else 
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, _("Add header to %s"), sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of %s"), sp->short_filename);
      if (ep->dialog)
	set_sensitive(XmMessageBoxGetChild(ep->dialog, XmDIALOG_OK_BUTTON), ep->panel_changed);
    }
  xstr = XmStringCreate(str, XmFONTLIST_DEFAULT_TAG);
  FREE(str);
  return(xstr);
}

static void edit_header_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  edit_header_dialog_help();
}

static void edit_header_set_ok_sensitive(Widget w, XtPointer context, XtPointer info)
{
  edhead_info *ep = (edhead_info *)context;
  if (!(ep->sp->file_read_only))
    set_sensitive(XmMessageBoxGetChild(ep->dialog, XmDIALOG_OK_BUTTON), true);
  ep->panel_changed = true;
}

static void edit_header_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  edhead_info *ep = (edhead_info *)context;
  XtUnmanageChild(ep->dialog);
  unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
  remove_sp_watcher(ep->sp, ep->sp_ro_watcher_loc);
  ep->panel_changed = false;
  ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
}

static void edit_header_watch_user_read_only(struct snd_info *sp, sp_watcher_reason_t reason, int loc)
{
  edhead_info *ep;
  ep = (edhead_info *)(sp->watchers[loc]->context);
  if ((ep->dialog) && 
      (XtIsManaged(ep->dialog)) &&
      (sp == ep->sp))
    {
      if (reason == SP_READ_ONLY_CHANGED) /* SP_IS_CLOSING is other choice */
	{
	  XmString title;
	  if ((!(sp->file_read_only)) && (!(sp->user_read_only)))
	    clear_dialog_error(ep->edat);
	  title = make_header_dialog_title(ep, sp);
	  XtVaSetValues(ep->dialog, 
			XmNmessageString, title, 
			NULL);
	  XmStringFree(title);
	}
      else /* sound closing, so we shouldn't sit around offering to edit its header -- watcher is null around update's close */
	{
	  clear_dialog_error(ep->edat);
	  if (ep->panel_changed)
	    unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
	  XtUnmanageChild(ep->dialog);
	  ep->panel_changed = false;
	  ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
	  remove_sp_watcher(ep->sp, loc);
	  ep->sp = NULL;
	}
    }
}

static void watch_file_read_only(struct fam_info *fp, FAMEvent *fe)
{
#if HAVE_FAM
  /* if file is deleted or permissions change, respond in some debonair manner */
  edhead_info *ep = (edhead_info *)(fp->data);
  snd_info *sp = NULL;
  sp = ep->sp;
  if (sp->writing) return;
  switch (fe->code)
    {
    case FAMChanged:
#if HAVE_ACCESS
      {
	int err;
	XmString title;
	if (mus_file_probe(sp->filename))
	  {
	    err = access(sp->filename, W_OK);
	    sp->file_read_only = (err < 0);
	    if ((!(sp->file_read_only)) && (!(sp->user_read_only)))
	      clear_dialog_error(ep->edat);
	    title = make_header_dialog_title(ep, sp);
	    XtVaSetValues(ep->dialog, 
			  XmNmessageString, title, 
			  NULL);
	    XmStringFree(title);
	    return;
	  }
      }
#endif
      /* else fall through */
    case FAMDeleted:
    case FAMCreated:
    case FAMMoved:
      /* I don't think it makes sense to continue the dialog at this point */
      clear_dialog_error(ep->edat);
      XtUnmanageChild(ep->dialog);
      if (ep->panel_changed)
	unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
      ep->panel_changed = false;
      ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
      remove_sp_watcher(ep->sp, ep->sp_ro_watcher_loc);
      break;

    default:
      /* ignore the rest */
      break;
    }
#endif
}

static void edit_header_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  edhead_info *ep = (edhead_info *)context;
  if ((ep->sp) && (ep->sp->active))
    {
      if (XmGetFocusWidget(ep->dialog) == XmMessageBoxGetChild(ep->dialog, XmDIALOG_OK_BUTTON))
	{
	  bool ok;
	  redirect_snd_error_to(post_file_dialog_error, (void *)(ep->edat));
	  ok = edit_header_callback(ep->sp, ep->edat, post_file_dialog_error, post_file_panel_error);
	  /* edit_header_callback, if all goes well, writes the header, recopies the data,
	   *   then calls snd_update which closes the sound and reopens it, to force the
	   *   new_header to take effect.  The read-only watcher is disabled during that
	   *   process to keep it from getting a SOUND_IS_CLOSING message from close.
	   */
	  redirect_snd_error_to(NULL, NULL);
	  if (ep->edat->error_widget != NOT_A_SCANF_WIDGET)
	    {
	      clear_error_if_panel_changes(ep->dialog, (void *)(ep->edat));
	      return;
	    }
	  else
	    {
	      if (!ok)
		{
		  set_sensitive(XmMessageBoxGetChild(ep->dialog, XmDIALOG_OK_BUTTON), false);
		  return;
		}
	    }
	  remove_sp_watcher(ep->sp, ep->sp_ro_watcher_loc);
	  ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
	  XtUnmanageChild(ep->dialog);
	  unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
	}
    }
}

Widget edit_header(snd_info *sp)
{
  file_info *hdr;
  XmString xstr4;
  Widget main_w;
  int i;
  edhead_info *ep = NULL;

  if (!sp) return(NULL);

  /* look for a dialog already editing this sound, raise if found, else make a new one */
  if (edhead_info_size > 0)
    {
      for (i = 0; i < edhead_info_size; i++)
	if ((edhead_infos[i]) &&
	    ((edhead_infos[i]->sp == sp) ||
	     ((edhead_infos[i]->sp) && /* maybe same sound open twice -- only one edit header dialog for it */
	      (edhead_infos[i]->sp->inuse == SOUND_NORMAL) &&
	      (edhead_infos[i]->sp->filename) &&
	      (strcmp(sp->filename, edhead_infos[i]->sp->filename) == 0))))
	  {
	    ep = edhead_infos[i];
	    break;
	  }
    }
  if (!ep)
    ep = new_edhead_dialog();

  ep->sp = sp;
  hdr = sp->hdr;
  ep->panel_changed = (hdr->type == MUS_RAW);
  xstr4 = make_header_dialog_title(ep, sp);

  if (!ep->dialog)
    {
      int n;
      Arg args[20];
      XmString xstr1, xstr2, xstr3, titlestr;

      n = 0;
      xstr1 = XmStringCreate(_("Cancel"), XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
      xstr2 = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      xstr3 = XmStringCreate(_("Save"), XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(_("Edit Header"), XmFONTLIST_DEFAULT_TAG);

      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNcancelLabelString, xstr1); n++;
      XtSetArg(args[n], XmNhelpLabelString, xstr2); n++;
      XtSetArg(args[n], XmNokLabelString, xstr3); n++;
      XtSetArg(args[n], XmNmessageString, xstr4); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      ep->dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), "Edit Header", args, n);

      XtAddCallback(ep->dialog, XmNcancelCallback, edit_header_cancel_callback, (XtPointer)ep);
      XtAddCallback(ep->dialog, XmNhelpCallback,   edit_header_help_callback,   (XtPointer)ep);
      XtAddCallback(ep->dialog, XmNokCallback,     edit_header_ok_callback,     (XtPointer)ep);

      XmStringFree(xstr1);
      XmStringFree(xstr2);
      XmStringFree(xstr3);
      XmStringFree(titlestr);

      n = 0;
      main_w = XtCreateManagedWidget("eh-main", xmFormWidgetClass, ep->dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      ep->edat = make_file_data_panel(main_w, "Edit Header", args, n, 
				      WITH_CHANNELS_FIELD, 
				      hdr->type, 
				      hdr->format, 
				      WITH_DATA_LOCATION_FIELD, 
				      WITH_SAMPLES_FIELD,
				      WITH_ERROR_FIELD, 
				      WITH_HEADER_TYPE_FIELD, 
				      WITH_COMMENT_FIELD,
				      WITH_BUILTIN_HEADERS);
      ep->edat->dialog = ep->dialog;

      if (hdr->type == MUS_RAW)
	set_file_dialog_sound_attributes(ep->edat, 
					 default_output_header_type(ss), 
					 hdr->format, hdr->srate, hdr->chans, 
					 hdr->data_location, hdr->samples, hdr->comment);
      else set_file_dialog_sound_attributes(ep->edat, 
					    hdr->type, hdr->format, hdr->srate, hdr->chans, 
					    hdr->data_location, hdr->samples, hdr->comment);
      XtManageChild(ep->edat->error_text);
      XtManageChild(ep->dialog);

      if (!(ss->using_schemes)) 
	{
	  map_over_children(ep->dialog, set_main_color_of_widget, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(ep->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(ep->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(ep->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(ep->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(ep->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(ep->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
	  XtVaSetValues(ep->edat->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(ep->edat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	}
      set_dialog_widget(EDIT_HEADER_DIALOG, ep->dialog);
      XtUnmanageChild(ep->edat->error_text);
    }
  else 
    {
      XtVaSetValues(ep->dialog, 
		    XmNmessageString, xstr4, 
		    NULL);
      if (hdr->type == MUS_RAW)
	set_file_dialog_sound_attributes(ep->edat, 
					 default_output_header_type(ss), 
					 hdr->format, hdr->srate, hdr->chans, 
					 hdr->data_location, hdr->samples, hdr->comment);
      else set_file_dialog_sound_attributes(ep->edat, 
					    hdr->type, hdr->format, hdr->srate, hdr->chans, 
					    hdr->data_location, hdr->samples, hdr->comment);
      raise_dialog(ep->dialog);
      clear_dialog_error(ep->edat);
    }
  set_sensitive(XmMessageBoxGetChild(ep->dialog, XmDIALOG_OK_BUTTON), (hdr->type == MUS_RAW)); /* nothing needs to be saved when we start */
  XmStringFree(xstr4);
  if (!(XtIsManaged(ep->dialog))) XtManageChild(ep->dialog);
  reflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
  ep->sp_ro_watcher_loc = add_sp_watcher(ep->sp, SP_READ_ONLY_WATCHER, edit_header_watch_user_read_only, (void *)ep);
  ep->file_ro_watcher = fam_monitor_file(ep->sp->filename, (void *)ep, watch_file_read_only);
  return(ep->dialog);
}

void save_edit_header_dialog_state(FILE *fd)
{
  /* save one of them anyway... */
  int i;
  for (i = 0; i < edhead_info_size; i++)
    if (edhead_infos[i])
      {
	edhead_info *ep;
	ep = edhead_infos[i];
	if ((ep->dialog) && 
	    (XtIsManaged(ep->dialog)) && 
	    (snd_ok(ep->sp)))
	  {
#if HAVE_SCHEME
	    fprintf(fd, "(%s (%s \"%s\"))\n", S_edit_header_dialog, S_find_sound, ep->sp->short_filename);
#endif
#if HAVE_RUBY
	    fprintf(fd, "%s(%s(\"%s\"))\n", TO_PROC_NAME(S_edit_header_dialog), TO_PROC_NAME(S_find_sound), ep->sp->short_filename);
#endif
	    break;
	  }
    }
}

#if DEBUGGING && HAVE_GUILE
static XEN g_apply_edit_header(void)
{
  /* apply one of them anyway... -- we called edit-header-dialog earlier to set things up for this */
  int i;
  for (i = 0; i < edhead_info_size; i++)
    if (edhead_infos[i])
      {
	edhead_info *ep;
	ep = edhead_infos[i];
	if ((ep->sp) && (ep->sp->active))
	  {
	    if ((!(ep->sp->user_read_only)) && (!(ep->sp->file_read_only)))
	      edit_header_callback(ep->sp, ep->edat, NULL, NULL);
	    else snd_error(_("%s is write-protected"), ep->sp->short_filename);
	  }
	XtUnmanageChild(ep->dialog);
	unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
	ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
      }
  return(XEN_FALSE);
}
#endif



/* -------------------------------- Raw Data Dialog -------------------------------- */

/* we keep an array of raw data dialogs so that any number can be active at once */

typedef struct raw_info {
  Widget dialog;
  off_t location;
  file_data *rdat;
  bool read_only;
  bool selected;
  char *filename;
  char *help;
  open_requestor_t requestor;
  snd_info *sp;
  Widget requestor_dialog;
} raw_info;

static int raw_info_size = 0;
static raw_info **raw_infos = NULL;

static raw_info *new_raw_dialog(void)
{
  int loc = -1;
  if (raw_info_size == 0)
    {
      loc = 0;
      raw_info_size = 4;
      raw_infos = (raw_info **)CALLOC(raw_info_size, sizeof(raw_info *));
    }
  else
    {
      int i;
      for (i = 0; i < raw_info_size; i++)
	if ((!raw_infos[i]) ||
	    (!(XtIsManaged(raw_infos[i]->dialog))))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = raw_info_size;
	  raw_info_size += 4;
	  raw_infos = (raw_info **)REALLOC(raw_infos, raw_info_size * sizeof(raw_info *));
	  for (i = loc; i < raw_info_size; i++) raw_infos[i] = NULL;
	}
    }
  if (!raw_infos[loc])
    {
      raw_infos[loc] = (raw_info *)CALLOC(1, sizeof(raw_info));
      raw_infos[loc]->dialog = NULL;
      raw_infos[loc]->filename = NULL;
      raw_infos[loc]->help = NULL;
    }
  raw_infos[loc]->requestor = NO_REQUESTOR;
  raw_infos[loc]->sp = NULL;
  raw_infos[loc]->location = 0;
  return(raw_infos[loc]);
}

static void raw_data_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_info *rp = (raw_info *)context;
  int raw_srate, raw_chans, raw_data_format;
  redirect_snd_error_to(post_file_panel_error, (void *)(rp->rdat));
  get_file_dialog_sound_attributes(rp->rdat, &raw_srate, &raw_chans, NULL, &raw_data_format, &(rp->location), NULL, 1);
  redirect_snd_error_to(NULL, NULL);
  if (rp->rdat->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(rp->dialog, (void *)(rp->rdat));
    }
  else
    {
      mus_header_set_raw_defaults(raw_srate, raw_chans, raw_data_format);
      mus_sound_override_header(rp->filename, raw_srate, raw_chans, 
				raw_data_format, MUS_RAW, rp->location,
				mus_bytes_to_samples(raw_data_format, 
						     mus_sound_length(rp->filename) - rp->location));
      /* choose action based on how we got here */
      if ((rp->requestor_dialog) &&
	  ((rp->requestor == FROM_MIX_DIALOG) ||
	   (rp->requestor == FROM_INSERT_DIALOG)))
	{
	  ss->reloading_updated_file = true; /* don't reread lack-of-header! */
	  /* redirection may be still set here, but I'll make it obvious */
	  if (rp->requestor == FROM_MIX_DIALOG)
	    {
	      redirect_snd_error_to(file_open_error, (void *)mdat);
	      mix_complete_file_at_cursor(any_selected_sound(), rp->filename, with_mix_tags(ss), 0);
	    }
	  else
	    {
	      redirect_snd_error_to(file_open_error, (void *)idat);
	      insert_complete_file_at_cursor(any_selected_sound(), rp->filename);
	    }
	  redirect_snd_error_to(NULL, NULL);
	  ss->reloading_updated_file = false;
	}
      else
	{
	  /* FROM_OPEN_DIALOG (has requestor_dialog)
	   * FROM_KEYBOARD (has requestor_sp)
	   * FROM_DRAG_AND_DROP (just open, no needed side effects)
	   * FROM_VIEW_FILES (ditto)
	   */
	  file_info *hdr;
	  hdr = (file_info *)CALLOC(1, sizeof(file_info));
	  hdr->name = copy_string(rp->filename);
	  hdr->type = MUS_RAW;
	  hdr->srate = raw_srate;
	  hdr->chans = raw_chans;
	  hdr->format = raw_data_format;
	  hdr->samples = mus_bytes_to_samples(raw_data_format, 
					      mus_sound_length(rp->filename) - rp->location);
	  hdr->data_location = rp->location;
	  hdr->comment = NULL;
	  if (rp->requestor == FROM_KEYBOARD)
	    {
	      clear_minibuffer(rp->sp);
	      rp->selected = true;
	    }
	  finish_opening_sound(add_sound_window(rp->filename, rp->read_only, hdr), rp->selected);
	}
      XtUnmanageChild(rp->dialog);
    }
}

static void raw_data_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_info *rp = (raw_info *)context;
  XtUnmanageChild(rp->dialog);
  if ((rp->requestor_dialog) && 
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    XtManageChild(rp->requestor_dialog);
}

static void raw_data_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_info *rp = (raw_info *)context;
  int raw_srate, raw_chans, raw_data_format;
  rp->location = 0;
  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */  
  set_file_dialog_sound_attributes(rp->rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, rp->location, 
				   IGNORE_SAMPLES, NULL);
  if (XtIsManaged(rp->rdat->error_text))
    XtUnmanageChild(rp->rdat->error_text);
}

static void raw_data_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_info *rp = (raw_info *)context;
  raw_data_dialog_help(rp->help);
}

static void make_raw_data_dialog(raw_info *rp, const char *filename, const char *title)
{
  XmString xstr1, xstr2, xstr3, xstr4, titlestr;
  int n;
  int raw_srate, raw_chans, raw_data_format;
  Arg args[20];
  Widget reset_button, main_w;

  xstr1 = XmStringCreate(_("Cancel"), XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
  xstr2 = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
  xstr3 = XmStringCreate(_("Ok"), XmFONTLIST_DEFAULT_TAG);
  if (!title)
    titlestr = XmStringCreate(_("No header on file"), XmFONTLIST_DEFAULT_TAG);
  else titlestr = XmStringCreate((char *)title, XmFONTLIST_DEFAULT_TAG);
  xstr4 = XmStringCreate((char *)title, XmFONTLIST_DEFAULT_TAG);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  XtSetArg(args[n], XmNcancelLabelString, xstr1); n++;
  XtSetArg(args[n], XmNhelpLabelString, xstr2); n++;
  XtSetArg(args[n], XmNokLabelString, xstr3); n++;
  XtSetArg(args[n], XmNmessageString, xstr4); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNallowShellResize, true); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNautoUnmanage, false); n++;
  /* not transient -- we want this window to remain visible if possible */
  rp->dialog = XmCreateWarningDialog(MAIN_SHELL(ss), "raw data", args, n);

  XtAddCallback(rp->dialog, XmNcancelCallback, raw_data_cancel_callback, (XtPointer)rp);
  XtAddCallback(rp->dialog, XmNhelpCallback,   raw_data_help_callback,   (XtPointer)rp);
  XtAddCallback(rp->dialog, XmNokCallback,     raw_data_ok_callback,     (XtPointer)rp);
  XmStringFree(xstr1);
  XmStringFree(xstr2);
  XmStringFree(xstr3);
  XmStringFree(xstr4);
  XmStringFree(titlestr);

  n = 0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
    }
  reset_button = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, rp->dialog, args, n);
  XtAddCallback(reset_button, XmNactivateCallback, raw_data_reset_callback, (XtPointer)rp);

  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */

  n = 0;
  XtSetArg(args[n], XmNallowResize, true); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_NONE); n++;
  main_w = XtCreateManagedWidget("raw-main", xmFormWidgetClass, rp->dialog, args, n);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  rp->rdat = make_file_data_panel(main_w, "data-form", args, n, 
				  WITH_CHANNELS_FIELD, 
				  MUS_RAW, raw_data_format, 
				  WITH_DATA_LOCATION_FIELD, 
				  WITHOUT_SAMPLES_FIELD,
				  WITH_ERROR_FIELD, 
				  WITHOUT_HEADER_TYPE_FIELD, 
				  WITHOUT_COMMENT_FIELD,
				  WITH_READABLE_HEADERS);
  rp->rdat->dialog = rp->dialog;

  set_file_dialog_sound_attributes(rp->rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, rp->location, 
				   IGNORE_SAMPLES, NULL);

  map_over_children(rp->dialog, set_main_color_of_widget, NULL);
  if (!(ss->using_schemes)) 
    {
      XtVaSetValues(XmMessageBoxGetChild(rp->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(rp->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(rp->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(rp->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
      XtVaSetValues(XmMessageBoxGetChild(rp->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
      XtVaSetValues(XmMessageBoxGetChild(rp->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
      XtVaSetValues(reset_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(rp->rdat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
    }

  XtManageChild(rp->rdat->error_text);
  XtManageChild(rp->dialog);
  XtUnmanageChild(rp->rdat->error_text); 
  set_dialog_widget(RAW_DATA_DIALOG, rp->dialog);
}

void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, bool read_only, bool selected)
{
  /* put up dialog for srate, chans, data format */
  raw_info *rp;
  rp = new_raw_dialog();
  rp->read_only = read_only;
  rp->selected = selected;
  if (rp->filename) FREE(rp->filename);
  rp->filename = copy_string(filename);
  rp->requestor = ss->open_requestor;
  rp->requestor_dialog = ss->sgx->requestor_dialog;
  rp->sp = ss->open_requestor_sp;
  ss->open_requestor = NO_REQUESTOR;
  ss->sgx->requestor_dialog = NULL;
  ss->open_requestor_sp = NULL;
  if ((rp->requestor_dialog) &&
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    XtUnmanageChild(rp->requestor_dialog);
  if (!title) 
    title = mus_format("no header found on %s\n", filename);
  if (!(rp->dialog))
    make_raw_data_dialog(rp, filename, title);
  else
    {
      XmString xstr4;
      xstr4 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(rp->dialog, 
		    XmNmessageString, xstr4, 
		    NULL);
      XmStringFree(xstr4);
    }
  FREE(title);
  if (rp->help) FREE(rp->help);
  if (info)
    {
      XtVaSetValues(XmMessageBoxGetChild(rp->dialog, XmDIALOG_HELP_BUTTON), 
		    XmNbackground, ss->sgx->green, 
		    NULL);
      rp->help = copy_string(info);
      FREE(info);
    }
  else
    {
      XtVaSetValues(XmMessageBoxGetChild(rp->dialog, XmDIALOG_HELP_BUTTON), 
		    XmNbackground, ss->sgx->help_button_color, 
		    NULL);
      rp->help = NULL;
    }
  raise_dialog(rp->dialog);
  if (!XtIsManaged(rp->dialog)) 
    XtManageChild(rp->dialog);
}


/* ---------------- POST-IT MONOLOG ---------------- */

#define POST_IT_ROWS 12
#define POST_IT_COLUMNS 56

static Widget post_it_dialog = NULL;
static Widget post_it_text = NULL;

static void create_post_it_monolog(void)
{
  /* create scrollable but not editable text window; used for fft peaks, sp info, and raw data help displays */
  Arg args[20];
  int n;

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  post_it_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "info", args, n);

  XtUnmanageChild(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_HELP_BUTTON));
  XtUnmanageChild(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_SYMBOL_LABEL));

  if (!(ss->using_schemes))
    XtVaSetValues(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->sgx->help_button_color, NULL);
      
  n = 0;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable, false); n++;
  XtSetArg(args[n], XmNcolumns, POST_IT_COLUMNS); n++;
  XtSetArg(args[n], XmNrows, POST_IT_ROWS); n++;
  if (!(ss->using_schemes))
    {
      XtSetArg(args[n], XmNforeground, ss->sgx->black); n++; /* needed if color allocation fails completely */
      XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    }
  post_it_text = XmCreateScrolledText(post_it_dialog, "post-it-text", args, n);
  XtManageChild(post_it_text);
  XtManageChild(post_it_dialog);

  if (!(ss->using_schemes))
    {
      map_over_children(post_it_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(post_it_text, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
    }
  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}

widget_t post_it(const char *subject, const char *str)
{
  /* place string in scrollable help window */
  XmString xstr1, xstr2;
  if ((ss == NULL) || (ss->sgx == NULL)) return(NULL); /* an attempt to call this before X/Motif is ready */
  if (!(post_it_dialog)) 
    create_post_it_monolog(); 
  else raise_dialog(post_it_dialog);
  xstr1 = XmStringCreate((char *)subject, XmFONTLIST_DEFAULT_TAG);
  xstr2 = XmStringCreate((char *)subject, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(post_it_dialog, 
		XmNmessageString, xstr1, 
		XmNdialogTitle, xstr2,
		NULL);
  XmTextSetString(post_it_text, (char *)str);
  if (!XtIsManaged(post_it_dialog)) 
    XtManageChild(post_it_dialog);
  XmStringFree(xstr1);
  XmStringFree(xstr2);
  return(post_it_dialog);
}

void save_post_it_dialog_state(FILE *fd)
{
  if ((post_it_dialog) && (XtIsManaged(post_it_dialog)))
    {
      char *subject, *text;
      XmString title;
      text = XmTextGetString(post_it_text);
      XtVaGetValues(post_it_dialog, XmNdialogTitle, &title, NULL);
      subject = (char *)XmStringUnparse(title, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\" \"%s\")\n", S_info_dialog, subject, text);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\", \"%s\")\n", TO_PROC_NAME(S_info_dialog), subject, text);
#endif
      if (subject) XtFree(subject);
      if (text) XtFree(text);
    }
}


/* ---------------- view files dialog ---------------- */

typedef enum {VF_AT_CURSOR, VF_AT_END, VF_AT_BEGINNING, VF_AT_MARK, VF_AT_SAMPLE} vf_location_t;

typedef struct {
  Widget rw, nm, pl;
  int pos;
  file_viewer_t parent;
  void *vdat;
} vf_row;

/* PERHAPS: how to present filters -- another button alongside sort? -- should sort have margins? */

typedef struct {
  Widget dialog, file_list, file_list_holder, left_title;
  vf_row **file_list_entries;
  int size;
  char **names;
  char **full_names;
  int *times;
  int end;
  int curtime, sorter;
  int *selected_files;
  int selected_files_size;
  int currently_selected_files;
  Widget info1, info2, openB, mixB, removeB, insertB, clearB, updateB;
  Widget at_cursor_button, at_end_button, at_beginning_button, at_mark_button, at_sample_button, at_sample_text, at_mark_text;
  Float amp;
  Widget amp_number, amp_scrollbar;
  vf_location_t location_choice;
  Float speed;
  Widget speed_number, speed_scrollbar;
  Widget env_drawer;
  axis_context *env_ax;
  GC env_gc;
  env_editor *spf;
  env *amp_env;
  int open_file_watcher_loc;
  bool error_p;
  Widget by_name, by_date, by_size, by_entry;
  Widget *sort_items;
  int sort_items_size;
  Widget smenu, current_play_button;
  speed_style_t speed_style;
} view_files_info;

static void view_files_clear_selected_files(view_files_info *vdat);
static void vf_mix_insert_buttons_set_sensitive(view_files_info *vdat, bool sensitive);
static void vf_open_remove_buttons_set_sensitive(view_files_info *vdat, bool sensitive);
static void view_files_clear_list(view_files_info *vdat);
static void view_files_update_list(view_files_info *vdat);
static Widget start_view_files_dialog_1(view_files_info *vdat, bool managed);
static void vf_clear_sample(view_files_info *vdat);
static void view_files_add_file_or_directory(view_files_info *vdat, const char *file_or_dir);
static int view_files_add_selected_file(view_files_info *vdat, vf_row *r);
static int view_files_find_row(view_files_info *vdat, const char *name);
static void view_files_display_list(view_files_info *vdat);
static void vf_reflect_sort_choice_in_menu(view_files_info *vdat);
static void vf_post_error(const char *error_msg, void *data);
static void vf_post_location_error(const char *error_msg, void *data);



/* -------- mouse-enter|leave-label hooks -------- */

static XEN mouse_enter_label_hook;
static XEN mouse_leave_label_hook;

static void mouse_leave_label_or_enter(vf_row *r, XEN hook, const char *caller)
{
  if ((r) &&
      (XEN_HOOKED(hook)))
    {
      char *label = NULL;
      bool need_free = false;
      if (r->parent == FILE_VIEWER)
	label = ((view_files_info *)(r->vdat))->full_names[r->pos];
      else
	{
	  XmString s1 = NULL;
	  /* it's a bit tedious to get the current button label... */
	  XtVaGetValues(r->nm, XmNlabelString, &s1, NULL);
	  if (XmStringEmpty(s1)) return;
	  label = (char *)XmStringUnparse(s1, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	  if (label) need_free = true;
	  XmStringFree(s1);
	}
      if (label)
	run_hook(hook,
		 XEN_LIST_3(C_TO_XEN_INT(r->parent),
			    C_TO_XEN_INT(r->pos),
			    C_TO_XEN_STRING(label)),
		 caller);
      if (need_free) XtFree(label);
    }
}

void mouse_enter_label(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  mouse_leave_label_or_enter((vf_row *)context, mouse_enter_label_hook, S_mouse_enter_label_hook);
}

void mouse_leave_label(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  mouse_leave_label_or_enter((vf_row *)context, mouse_leave_label_hook, S_mouse_leave_label_hook);
}

static vf_row *make_vf_row(view_files_info *vdat, 
			   Widget last_row, 
			   XtCallbackProc play_callback, XtCallbackProc name_callback)
{
  int n;
  Arg args[32];
  vf_row *r;
  XmString s1;
  XtCallbackList n1, n3;

  s1 = XmStringCreate("", XmFONTLIST_DEFAULT_TAG);
  r = (vf_row *)CALLOC(1, sizeof(vf_row));
  r->vdat = (void *)vdat;
  r->parent = FILE_VIEWER;

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, (last_row) ? XmATTACH_WIDGET : XmATTACH_FORM); n++;
  if (last_row) {XtSetArg(args[n], XmNtopWidget, last_row); n++;}
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNheight, 18); n++; 
  r->rw = XtCreateWidget("rw", xmFormWidgetClass, vdat->file_list_holder, args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, ss->sgx->pushed_button_color); n++;}
  XtSetArg(args[n], XmNlabelString, s1); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n1 = make_callback_list(play_callback, (XtPointer)r)); n++;
  if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
  XtSetArg(args[n], XmNmarginWidth, 8); n++;
  r->pl = make_togglebutton_widget("pl", r->rw, args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, r->pl); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  XtSetArg(args[n], XmNfillOnArm, false); n++;
  XtSetArg(args[n], XmNrecomputeSize, false); n++;
  XtSetArg(args[n], XmNwidth, 300); n++;
  XtSetArg(args[n], XmNactivateCallback, n3 = make_callback_list(name_callback, (XtPointer)r)); n++;
  r->nm = XtCreateManagedWidget("nm", xmPushButtonWidgetClass, r->rw, args, n);
  XmStringFree(s1);

  XtAddEventHandler(r->nm, EnterWindowMask, false, mouse_enter_label, (XtPointer)r);
  XtAddEventHandler(r->nm, LeaveWindowMask, false, mouse_leave_label, (XtPointer)r);

  FREE(n1);
  FREE(n3);
  return(r);
}

static void vf_open_file_watcher(ss_watcher_reason_t reason, int loc)
{
  ss_watcher *data;
  view_files_info *vdat;
  data = ss->watchers[loc];
  vdat = (view_files_info *)(data->context);
  if ((vdat->dialog) &&
      (XtIsManaged(vdat->dialog)))
    {
      /* reasons are SS_FILE_OPENED|CLOSED, but it's not worth the trouble of splitting them out here */
      vf_mix_insert_buttons_set_sensitive(vdat, 
					  ((vdat->currently_selected_files > 0) &&
					   (any_selected_sound())));
      vf_open_remove_buttons_set_sensitive(vdat, 
					   (vdat->currently_selected_files > 0));
    }
}

static int view_files_info_size = 0;
static view_files_info **view_files_infos = NULL;

static view_files_info *new_view_files_dialog(void)
{
  int loc = -1;
  if (view_files_info_size == 0)
    {
      loc = 0;
      view_files_info_size = 4;
      view_files_infos = (view_files_info **)CALLOC(view_files_info_size, sizeof(view_files_info *));
    }
  else
    {
      int i;
      for (i = 0; i < view_files_info_size; i++)
	if ((!view_files_infos[i]) ||
	    (!(view_files_infos[i]->dialog)) ||
	    (!(XtIsManaged(view_files_infos[i]->dialog))))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = view_files_info_size;
	  view_files_info_size += 4;
	  view_files_infos = (view_files_info **)REALLOC(view_files_infos, view_files_info_size * sizeof(view_files_info *));
	  for (i = loc; i < view_files_info_size; i++) view_files_infos[i] = NULL;
	}
    }
  if (!view_files_infos[loc])
    {
      view_files_info *vdat;
      view_files_infos[loc] = (view_files_info *)CALLOC(1, sizeof(view_files_info));
      vdat = view_files_infos[loc];
      vdat->dialog = NULL;
      vdat->file_list = NULL;
      vdat->file_list_holder = NULL;
      vdat->file_list_entries = NULL;
      vdat->size = 0;
      vdat->end = -1;
      vdat->names = NULL;
      vdat->full_names = NULL;
      vdat->curtime = 0;
      vdat->times = NULL;
      vdat->selected_files = NULL;
      vdat->selected_files_size = 0;
      vdat->location_choice = VF_AT_CURSOR;
      vdat->error_p = false;
      vdat->amp = 1.0;
      vdat->speed = 1.0;
      vdat->amp_env = default_env(1.0, 1.0);
      vdat->open_file_watcher_loc = add_ss_watcher(SS_FILE_OPEN_WATCHER, vf_open_file_watcher, (void *)vdat);
      vdat->sort_items_size = 0;
      vdat->sort_items = NULL;
      vdat->speed_style = speed_control_style(ss);
    }
  /* don't clear at this point! */
  view_files_infos[loc]->currently_selected_files = 0;
  view_files_infos[loc]->sorter = view_files_sort(ss);
  return(view_files_infos[loc]);
}

static int vf_dialog_to_index(Widget dialog)
{
  int i;
  if ((!dialog) &&
      (view_files_infos[0]) &&
      (view_files_infos[0]->dialog))
    return(0);
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog == dialog))
      return(i);
  return(-1);
}

static view_files_info *vf_dialog_to_info(Widget dialog)
{
  int index;
  index = vf_dialog_to_index(dialog);
  if (index >= 0)
    return(view_files_infos[index]);
  return(NULL);
}

static char **vf_selected_files(view_files_info *vdat)
{
  int len;
  char **files = NULL;
  len = vdat->currently_selected_files;
  if (len > 0)
    {
      int i;
      files = (char **)CALLOC(len, sizeof(char *));
      for (i = 0; i < len; i++) 
	files[i] = copy_string(vdat->full_names[vdat->selected_files[i]]);
    }
  return(files);
}

char **view_files_selected_files(widget_t dialog, int *len)
{
  /* free result */
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      (*len) = vdat->currently_selected_files;
      return(vf_selected_files(vdat));
    }
  (*len) = 0;
  return(NULL);
}

char **view_files_set_selected_files(widget_t dialog, char **files, int len)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      int i;
      view_files_clear_selected_files(vdat);
      for (i = 0; i < len; i++)
	if (files[i])
	  {
	    int loc;
	    loc = view_files_find_row(vdat, (const char *)(files[i]));
	    if (loc >= 0)
	      {
		view_files_add_selected_file(vdat, vdat->file_list_entries[loc]);
		view_files_run_select_hook(vdat->dialog, (const char *)(files[i]));
	      }
	  }
    }
  /* display not needed, I think */
  return(files);
}

char **view_files_files(widget_t dialog, int *len)
{
  /*don't free result! */
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      (*len) = vdat->end + 1;
      return(vdat->full_names);
    }
  (*len) = 0;
  return(NULL);
}

char **view_files_set_files(widget_t dialog, char **files, int len)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      int i;
      view_files_clear_selected_files(vdat);
      view_files_clear_list(vdat);
      for (i = 0; i < len; i++)
	if (files[i])
	  view_files_add_file_or_directory(vdat, (const char *)(files[i]));
    }
  view_files_display_list(vdat);
  return(files);
}

static void vf_mix_insert_buttons_set_sensitive(view_files_info *vdat, bool sensitive)
{
  if (vdat->mixB)
    {
      XtSetSensitive(vdat->mixB, sensitive);
      XtSetSensitive(vdat->insertB, sensitive);
    }
}

static void vf_open_remove_buttons_set_sensitive(view_files_info *vdat, bool sensitive)
{
  if (vdat->openB)
    {
      XtSetSensitive(vdat->removeB, sensitive);
      XtSetSensitive(vdat->openB, sensitive);
    }
}

static void vf_clear_button_set_sensitive(view_files_info *vdat, bool sensitive)
{
  if (vdat->clearB)
    XtSetSensitive(vdat->clearB, sensitive);
}

static void view_files_clear_selected_files(view_files_info *vdat)
{
  int len;
  len = vdat->currently_selected_files;
  if (len > 0)
    {
      int i;
      for (i = 0; i < len; i++)
	{
	  vf_row *r;
	  r = vdat->file_list_entries[vdat->selected_files[i]];
	  if (r)
	    {
	      XtVaSetValues(r->rw, XmNbackground, ss->sgx->highlight_color, NULL);
	      XtVaSetValues(r->nm, XmNbackground, ss->sgx->highlight_color, NULL);
	    }
	}
    }
  vdat->currently_selected_files = 0;
  vf_mix_insert_buttons_set_sensitive(vdat, false);
  vf_open_remove_buttons_set_sensitive(vdat, false);
}

static void vf_post_info(view_files_info *vdat, int pos)
{
  char *title;
  XmString s3;
  title = mus_format("%s:", vdat->names[pos]);
  s3 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(vdat->left_title,
		XmNlabelString, s3,
		NULL);
  XmStringFree(s3);
  FREE(title);
  post_sound_info(vdat->info1, vdat->info2, vdat->full_names[pos], false);
}

static void vf_post_selected_files_list(view_files_info *vdat)
{
  int len;
  char *msg1 = NULL, *msg2 = NULL, *title;
  XmString s1, s2, s3;
  len = vdat->currently_selected_files;

  title = copy_string("selected files:");
  s3 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(vdat->left_title,
		XmNlabelString, s3,
		NULL);
  XmStringFree(s3);
  FREE(title);

  if (len == 2)
    {
      msg1 = copy_string(vdat->names[vdat->selected_files[0]]);
      msg2 = copy_string(vdat->names[vdat->selected_files[1]]);
    }
  else
    {
      if (len == 3)
	{
	  msg1 = mus_format("%s, %s", vdat->names[vdat->selected_files[0]], vdat->names[vdat->selected_files[1]]);
	  msg2 = copy_string(vdat->names[vdat->selected_files[2]]);
	}
      else
	{
	  msg1 = mus_format("%s, %s", vdat->names[vdat->selected_files[0]], vdat->names[vdat->selected_files[1]]);
	  msg2 = mus_format("%s, %s%s", vdat->names[vdat->selected_files[2]], vdat->names[vdat->selected_files[3]],
			    (len == 4) ? "" : "...");
	}
    }

  s1 = XmStringCreate(msg1, XmFONTLIST_DEFAULT_TAG);
  s2 = XmStringCreate(msg2, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(vdat->info1, XmNlabelString, s1, NULL);
  XtVaSetValues(vdat->info2, XmNlabelString, s2, NULL);
  XmStringFree(s1);
  XmStringFree(s2);
  FREE(msg1);
  FREE(msg2);
}

static void vf_unpost_info(view_files_info *vdat)
{
  XmString s1, s2, s3;
  char *title;

  title = copy_string("(no files selected)");
  s3 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(vdat->left_title,
		XmNlabelString, s3,
		NULL);
  XmStringFree(s3);
  FREE(title);

  s1 = XmStringCreate("|", XmFONTLIST_DEFAULT_TAG);
  s2 = XmStringCreate("|", XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(vdat->info1, XmNlabelString, s1, NULL);
  XtVaSetValues(vdat->info2, XmNlabelString, s2, NULL);
  XmStringFree(s1);
  XmStringFree(s2);
}

static void view_files_unselect_file(view_files_info *vdat, vf_row *r)
{
  XtVaSetValues(r->rw, XmNbackground, ss->sgx->highlight_color, NULL);
  XtVaSetValues(r->nm, XmNbackground, ss->sgx->highlight_color, NULL);
  if (vdat->currently_selected_files > 1)
    {
      /* need to fixup selected_files list */
      int i, new_loc = 0;
      for (i = 0; i < vdat->currently_selected_files; i++)
	if (vdat->selected_files[i] != r->pos)
	  vdat->selected_files[new_loc++] = vdat->selected_files[i];
    }
  vdat->currently_selected_files--;
  if (vdat->currently_selected_files < 0) 
    vdat->currently_selected_files = 0;
}

static int view_files_add_selected_file(view_files_info *vdat, vf_row *r)
{
  /* returns how many are now selected (counting new) */
  if (vdat->selected_files_size == 0)
    {
      vdat->selected_files_size = 4;
      vdat->selected_files = (int *)CALLOC(vdat->selected_files_size, sizeof(int));
      vdat->selected_files[0] = r->pos;
      vdat->currently_selected_files = 1;
    }
  else
    {
      if (vdat->currently_selected_files >= vdat->selected_files_size)
	{
	  vdat->selected_files_size += 4;
	  vdat->selected_files = (int *)REALLOC(vdat->selected_files, vdat->selected_files_size * sizeof(int));
	  vdat->selected_files[vdat->currently_selected_files++] = r->pos;
	}
      else 
	{
	  vdat->selected_files[vdat->currently_selected_files++] = r->pos;
	}
    }
  XtVaSetValues(r->rw, XmNbackground, ss->sgx->zoom_color, NULL);
  XtVaSetValues(r->nm, XmNbackground, ss->sgx->zoom_color, NULL);
  return(vdat->currently_selected_files);
}

static void vf_fixup_selected_files(view_files_info *vdat, char **saved_selected_files, int len)
{
  /* various things change the order or contents of the files list, so the selected locs list needs to reflect that */
  int i, newly_selected = 0;
  for (i = 0; i < len; i++)
    {
      int j;
      for (j = 0; j <= vdat->end; j++)
	if ((vdat->full_names[j]) &&
	    (strcmp(vdat->full_names[j], saved_selected_files[i]) == 0))
	  {
	    vf_row *old_r, *new_r;
	    /* fprintf(stderr,"old %d at %d -> %d at %d\n", vdat->selected_files[i], i, j, newly_selected); */
	    old_r = vdat->file_list_entries[vdat->selected_files[i]];
	    vdat->selected_files[newly_selected++] = j;
	    new_r = vdat->file_list_entries[j];
	    if (new_r != old_r)
	      {
		XtVaSetValues(new_r->rw, XmNbackground, ss->sgx->zoom_color, NULL);
		XtVaSetValues(new_r->nm, XmNbackground, ss->sgx->zoom_color, NULL);
		XtVaSetValues(old_r->rw, XmNbackground, ss->sgx->highlight_color, NULL);
		XtVaSetValues(old_r->nm, XmNbackground, ss->sgx->highlight_color, NULL);
	      }
	    break;
	  }
    }
  vdat->currently_selected_files = newly_selected;
}


static int view_files_find_row(view_files_info *vdat, const char *name)
{
  int i;
  if (vdat->names)
    for (i = 0; i <= vdat->end; i++)
      if ((vdat->names[i]) && 
	  (strcmp(vdat->names[i], name) == 0))
  	return(i);
  if (vdat->full_names)
    for (i = 0; i <= vdat->end; i++)
      if ((vdat->full_names[i]) && 
	  (strcmp(vdat->full_names[i], name) == 0))
	return(i);
  return(-1);
}

/* TODO: clip/db buttons for graph?? */

static void view_files_select(vf_row *r, bool add_to_selected)
{
  view_files_info *vdat = (view_files_info *)(r->vdat);
  int i, curloc = -1;

  for (i = 0; i < vdat->currently_selected_files; i++)
    if (vdat->selected_files[i] == r->pos)
      {
	curloc = r->pos;
	break;
      }
  if (curloc == -1)
    {
      /* file not currently selected */
      if (!add_to_selected)         /* not shift click, so remove all currently selected files first */
	view_files_clear_selected_files(vdat);
      view_files_add_selected_file(vdat, r);
      view_files_run_select_hook(vdat->dialog, vdat->full_names[r->pos]);
    }
  else
    {
      /* file already selected, so remove from selected files list */
      view_files_unselect_file(vdat, r);
    }

  if ((vdat->currently_selected_files == 0) ||
      ((vdat->currently_selected_files == 1) &&
       (!(plausible_sound_file_p(vdat->full_names[vdat->selected_files[0]])))))
    vf_unpost_info(vdat);
  else
    {
      if (vdat->currently_selected_files == 1)
	vf_post_info(vdat, vdat->selected_files[0]);
      else vf_post_selected_files_list(vdat);
    }
  vf_mix_insert_buttons_set_sensitive(vdat, 
				      ((vdat->currently_selected_files > 0) &&
				       (any_selected_sound())));
  vf_open_remove_buttons_set_sensitive(vdat, 
				       (vdat->currently_selected_files > 0));
}

void view_files_unplay(void)
{
  int k;
  for (k = 0; k < view_files_info_size; k++)
    if ((view_files_infos[k]) &&
	(view_files_infos[k]->dialog) &&
	(XtIsManaged(view_files_infos[k]->dialog)))
      {
	view_files_info *vdat;
	vdat = view_files_infos[k];
	if ((vdat->current_play_button) &&
	    (XmIsToggleButton(vdat->current_play_button)) &&
	    (XmToggleButtonGetState(vdat->current_play_button) != XmUNSET))
	  {
	    XmToggleButtonSetState(vdat->current_play_button, false, true);
	    vdat->current_play_button = NULL;
	  }
      }
}

static bool view_files_play(view_files_info *vdat, int pos, bool play)
{
  static snd_info *play_sp;
  if (play)
    {
      if (play_sp)
	{
	  if (play_sp->playing) return(true); /* can't play two of these at once */
	  if ((vdat->names[pos] == NULL) || 
	      (strcmp(play_sp->short_filename, vdat->names[pos]) != 0))
	    {
	      completely_free_snd_info(play_sp);
	      play_sp = NULL;
	    }
	}
      if ((!play_sp) && 
	  (vdat->full_names[pos]))
	play_sp = make_sound_readable(vdat->full_names[pos], false);
      if (play_sp)
	{
	  play_sp->short_filename = vdat->names[pos];
	  play_sp->filename = NULL;
	  play_sound(play_sp, 0, NO_END_SPECIFIED);
	}
      else return(true); /* can't find or setup file */
    }
  else
    { /* play toggled off */
      if ((play_sp) && (play_sp->playing)) 
	{
	  stop_playing_sound(play_sp, PLAY_BUTTON_UNSET);
	  vdat->current_play_button = NULL;
	}
    }
  return(false);
}

static void add_file_to_view_files_list(view_files_info *vdat, const char *filename, const char *fullname)
{
  if (view_files_find_row(vdat, filename) != -1) return;
  if (!(mus_file_probe(fullname))) return;
  vdat->end++;
  if (vdat->end >= vdat->size)
    {
      int new_size;
      new_size = vdat->size + 32;
      if (vdat->size == 0)
	{
	  vdat->names = (char **)CALLOC(new_size, sizeof(char *));
	  vdat->full_names = (char **)CALLOC(new_size, sizeof(char *));
	  vdat->times = (int *)CALLOC(new_size, sizeof(int));
	}
      else
	{
	  int i;
	  vdat->names = (char **)REALLOC(vdat->names, new_size * sizeof(char *));
	  vdat->full_names = (char **)REALLOC(vdat->full_names, new_size * sizeof(char *));
	  vdat->times = (int *)REALLOC(vdat->times, new_size * sizeof(int));
	  for (i = vdat->size; i < new_size; i++) 
	    {
	      vdat->names[i] = NULL; 
	      vdat->full_names[i] = NULL; 
	      vdat->times[i] = 0;
	    }
	}
      if (vdat->file_list_entries == NULL)
	vdat->file_list_entries = (vf_row **)CALLOC(new_size, sizeof(vf_row *));
      else 
	{
	  int i;
	  vdat->file_list_entries = (vf_row **)REALLOC(vdat->file_list_entries, new_size * sizeof(vf_row *));
	  for (i = vdat->size; i < new_size; i++) vdat->file_list_entries[i] = NULL;
	}
      vdat->size = new_size;
    }
  if (mus_file_probe(fullname))
    {
      vdat->names[vdat->end] = copy_string(filename);
      vdat->full_names[vdat->end] = copy_string(fullname);
      vdat->times[vdat->end] = vdat->curtime++;
      if ((vdat->dialog) && (XtIsManaged(vdat->dialog)))
	vf_clear_button_set_sensitive(vdat, true);
    }
}

static void add_directory_to_view_files_list(view_files_info *vdat, const char *dirname)
{
  dir *sound_files = NULL;
  sound_files = find_sound_files_in_dir(dirname);
  if ((sound_files) && (sound_files->len > 0))
    {
      char *fullpathname = NULL;
      char **fullnames;
      int i, end;

      /* fprintf(stderr,"path: %s ->", dirname); */
      fullpathname = (char *)CALLOC(FILENAME_MAX, sizeof(char));
      strcpy(fullpathname, dirname);
      if (dirname[strlen(dirname) - 1] != '/') 
	strcat(fullpathname, "/");
      /* fprintf(stderr," %s (%d)\n", fullpathname, sound_files->len); */

      end = strlen(fullpathname);
      fullnames = (char **)CALLOC(sound_files->len, sizeof(char *));
      for (i = 0; i < sound_files->len; i++) 
	{
	  fullnames[i] = copy_string(strcat(fullpathname, sound_files->files[i]));
	  fullpathname[end] = '\0';
	}
      for (i = 0; i < sound_files->len; i++) 
	{
	  add_file_to_view_files_list(vdat, sound_files->files[i], fullnames[i]);
	  FREE(fullnames[i]); 
	  fullnames[i] = NULL;
	}
      FREE(fullnames);
      free_dir(sound_files);
      FREE(fullpathname);
    }
}

/* sort view_files list by name (aphabetical), or some number (date written, size, entry order, srate? type?) */

typedef struct {
  int vals, times;
  off_t samps;
  char *a1, *a2;
} heapdata;

static int alphabet_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  return(strcmp(d1->a1, d2->a1));
}

static int greater_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  if (d1->samps > d2->samps) 
    return(1); 
  else 
    {
      if (d1->samps == d2->samps) 
	return(0); 
      else return(-1);
    }
}

static int less_compare(const void *a, const void *b)
{
  heapdata *d1 = *(heapdata **)a;
  heapdata *d2 = *(heapdata **)b;
  if (d1->vals < d2->vals) 
    return(1); 
  else 
    {
      if (d1->vals == d2->vals) 
	return(0); 
      else return(-1);
    }
}

static void ignore_mus_error(int type, char *msg)
{
  /* squelch error */
}

static void view_files_sort_list(view_files_info *vdat)
{
  int i, old_len;
  char **old_names = NULL;
  old_len = vdat->currently_selected_files;
  if (old_len > 0)
    old_names = vf_selected_files(vdat);
  if (vdat->end >= 0)
    {
      heapdata **data;
      int i, len;
      len = vdat->end + 1;
      data = (heapdata **)CALLOC(len, sizeof(heapdata *));
      for (i = 0; i < len; i++)
	{
	  data[i] = (heapdata *)CALLOC(1, sizeof(heapdata));
	  data[i]->a1 = vdat->names[i];
	  data[i]->a2 = vdat->full_names[i];
	  data[i]->times = vdat->times[i];
	}
      switch (vdat->sorter)
	{
	case SORT_BY_NAME: 
	  qsort((void *)data, vdat->end + 1, sizeof(heapdata *), alphabet_compare);
	  break;
	case SORT_BY_DATE:
	  for (i = 0; i <= vdat->end; i++) 
	    data[i]->vals = file_write_date(vdat->full_names[i]);
	  qsort((void *)data, vdat->end + 1, sizeof(heapdata *), less_compare);
	  break;
	case SORT_BY_SIZE:
	  mus_error_set_handler(ignore_mus_error);
	  for (i = 0; i <= vdat->end; i++)
	    {
	      data[i]->samps = mus_sound_samples(vdat->full_names[i]);
	      if (data[i]->samps < 0) data[i]->samps = 0;
	    }
	  mus_error_set_handler(mus_error_to_snd);
	  qsort((void *)data, vdat->end + 1, sizeof(heapdata *), greater_compare);
	  break;
	case SORT_BY_ENTRY:
	  for (i = 0; i <= vdat->end; i++) 
	    data[i]->vals = vdat->times[i];
	  qsort((void *)data, vdat->end + 1, sizeof(heapdata *), less_compare);
	  break;
	default:
	  {
	    int sorter_pos;
	    /* sorter is SORT_BY_PROC + index into file_sorters list */
	    sorter_pos = vdat->sorter - SORT_BY_PROC;
	    if ((sorter_pos >= 0) &&
		(sorter_pos < ss->file_sorters_size))
	      {
		XEN proc;
		proc = XEN_VECTOR_REF(ss->file_sorters, sorter_pos);
		if (XEN_PAIR_P(proc))
		  {
		    XEN file_list;
		    int j, gc_loc;
		    char *name;
		    proc = XEN_CDR(proc);
		    file_list = XEN_EMPTY_LIST;
		    for (i = vdat->end; i >= 0; i--) 
		      file_list = XEN_CONS(C_TO_XEN_STRING(vdat->full_names[i]), file_list);
		    gc_loc = snd_protect(file_list);
		    file_list = XEN_COPY_ARG(XEN_CALL_1(proc, file_list, "view files sort"));
		    snd_unprotect_at(gc_loc);         /* unprotect old version */
		    gc_loc = snd_protect(file_list);  /* protect new */
		    if (XEN_LIST_P(file_list))
		      {
			for (i = 0; (i < len) && (XEN_NOT_NULL_P(file_list)); i++, file_list = XEN_CDR(file_list))
			  {
			    name = XEN_TO_C_STRING(XEN_CAR(file_list));
			    for (j = 0; j < len; j++)
			      if (strcmp(data[j]->a2, name) == 0)
				{
				  vdat->names[i] = data[j]->a1;
				  vdat->full_names[i] = data[j]->a2;
				  vdat->times[i] = data[j]->times;
				}
			  }
		      }
		    snd_unprotect_at(gc_loc);
		  }
	      }
	    for (i = 0; i < len; i++) FREE(data[i]);
	    break;
	  }
	}

      if (vdat->sorter < SORT_BY_PROC)
	for (i = 0; i < len; i++)
	  {
	    vdat->names[i] = data[i]->a1;
	    vdat->full_names[i] = data[i]->a2;
	    vdat->times[i] = data[i]->times;
	    FREE(data[i]);
	  }

      FREE(data);
    }
  if (old_names)
    {
      vf_fixup_selected_files(vdat, old_names, old_len);
      for (i = 0; i < old_len; i++) FREE(old_names[i]);
      FREE(old_names);
    }
}

static void view_files_select_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  XButtonEvent *ev;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  ev = (XButtonEvent *)(cb->event);
  view_files_select((vf_row *)context, ev->state & snd_ShiftMask);
}

static void view_files_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* open and play -- close at end or when button off toggled */
  vf_row *r = (vf_row *)context;
  view_files_info *vdat;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  vdat = (view_files_info *)(r->vdat);
  if (view_files_play(vdat, r->pos, cb->set))
    XmToggleButtonSetState(w, false, false);
  else vdat->current_play_button = w;
}

static void view_files_display_list(view_files_info *vdat)
{
  int i;
  Widget last_row = NULL;
  vf_row *r;
  if (!vdat) return;
  if (!(vdat->dialog)) return;
  if (vdat->end >= 0)
    {
      view_files_sort_list(vdat);
      for (i = 0; i <= vdat->end; i++)
	{
	  r = vdat->file_list_entries[i];
	  if (!r)
	    {
	      r = make_vf_row(vdat, last_row, view_files_play_callback, view_files_select_callback);
	      vdat->file_list_entries[i] = r;
	      r->pos = i;
	    }
	  set_button_label(r->nm, vdat->names[r->pos]);
	  XmToggleButtonSetState(r->pl, false, false);
	  if (!(XtIsManaged(r->rw))) XtManageChild(r->rw);
	  last_row = r->rw;
	}
    }
  for (i = vdat->end + 1; i < vdat->size; i++)
    {
      r = vdat->file_list_entries[i];
      if (r)
	{
	  if (XtIsManaged(r->rw)) 
	    XtUnmanageChild(r->rw);
	}
    }
  if (!(XtIsManaged(vdat->file_list))) 
    XtManageChild(vdat->file_list);
}

static void view_files_clear_list(view_files_info *vdat)
{
  int i;
  if (vdat->names)
    {
      for (i = 0; i < vdat->size; i++)
	if (vdat->names[i]) 
	  {
	    FREE(vdat->names[i]); 
	    vdat->names[i] = NULL;
	    FREE(vdat->full_names[i]); 
	    vdat->full_names[i] = NULL;
	  }
      vdat->end = -1;
      vdat->curtime = 0;
      vf_clear_button_set_sensitive(vdat, false);
    }
}

static void view_files_update_list(view_files_info *vdat)
{
  /* here we need the file's full name */
  int i, old_len;
  char **old_names = NULL;
  old_len = vdat->currently_selected_files;
  if (old_len > 0) 
    old_names = vf_selected_files(vdat);
  if (vdat->names)
    {
      int i, j;
      for (i = 0; i <= vdat->end; i++)
	if (vdat->names[i]) 
	  {
	    if (!(mus_file_probe(vdat->full_names[i])))
	      {
		FREE(vdat->names[i]); 
		vdat->names[i] = NULL;
		FREE(vdat->full_names[i]); 
		vdat->full_names[i] = NULL;
	      }
	  }
      for (i = 0, j = 0; i <= vdat->end; i++)
	if (vdat->names[i])
	  {
	    if (i != j) 
	      {
		vdat->names[j] = vdat->names[i]; 
		vdat->names[i] = NULL;
		vdat->full_names[j] = vdat->full_names[i];
		vdat->full_names[i] = NULL;
		vdat->times[j] = vdat->times[i];
	      }
	    j++;
	  }
      vdat->end = j - 1;
      vf_clear_button_set_sensitive(vdat, vdat->end >= 0);
    }
  if (old_names)
    {
      vf_fixup_selected_files(vdat, old_names, old_len);
      for (i = 0; i < old_len; i++) FREE(old_names[i]);
      FREE(old_names);
    }
}


static void view_files_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_dialog_help();
}

static void view_files_dismiss_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  Widget active_widget;
  active_widget = XmGetFocusWidget(vdat->dialog);
  if (active_widget == XmMessageBoxGetChild(vdat->dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(vdat->dialog);
}

static void view_files_clear_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  view_files_clear_list(vdat);
  view_files_display_list(vdat);
}

static void view_files_new_viewer_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat;
  vdat = new_view_files_dialog();
  start_view_files_dialog_1(vdat, true);
  /* TODO: copy caller? */
  /* TODO: place somewhere visible (not right under the current one!) */
}

static void view_files_update_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  /* run through view files list looking for any that have been deleted behind our back */
  view_files_update_list(vdat);
  view_files_sort_list(vdat);
  view_files_display_list(vdat);
}

static void sort_view_files_by_name(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  vdat->sorter = SORT_BY_NAME;
  vf_reflect_sort_choice_in_menu(vdat);
  view_files_sort_list(vdat); 
  view_files_display_list(vdat);
}

static void sort_view_files_by_date(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  vdat->sorter = SORT_BY_DATE;
  vf_reflect_sort_choice_in_menu(vdat);
  view_files_sort_list(vdat);
  view_files_display_list(vdat);
}

static void sort_view_files_by_size(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  vdat->sorter = SORT_BY_SIZE;
  vf_reflect_sort_choice_in_menu(vdat);
  view_files_sort_list(vdat);
  view_files_display_list(vdat);
}

static void sort_view_files_by_entry_order(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  vdat->sorter = SORT_BY_ENTRY;
  vf_reflect_sort_choice_in_menu(vdat);
  view_files_sort_list(vdat);
  view_files_display_list(vdat);
}

static void sort_view_files_by_procedure(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  int index;
  XtVaGetValues(w, XmNuserData, &index, NULL); /* index is location in list of file-sorters */
  vdat->sorter = index;
  vf_reflect_sort_choice_in_menu(vdat);
  view_files_sort_list(vdat);
  view_files_display_list(vdat);
}

static void view_files_add_file_or_directory(view_files_info *vdat, const char *file_or_dir)
{
  char *filename;
  filename = mus_expand_filename((const char *)file_or_dir);
  if (directory_p(filename))
    add_directory_to_view_files_list(vdat, (const char *)filename);
  else add_file_to_view_files_list(vdat, file_or_dir, filename);
  FREE(filename);
}

static void view_files_add_files(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  char *file_or_dir;
  file_or_dir = XmTextFieldGetString(w);
  if ((file_or_dir) && (*file_or_dir))
    {
      view_files_add_file_or_directory(vdat, (const char *)file_or_dir);
      XtFree(file_or_dir);
      view_files_sort_list(vdat);
      view_files_display_list(vdat);
    }
}

static void view_files_drop_watcher(Widget w, const char *str, Position x, Position y, void *context)
{
  view_files_info *vdat = (view_files_info *)context;
  char *filename;
  filename = mus_expand_filename(str);
  add_file_to_view_files_list(vdat, str, filename);
  FREE(filename);
  view_files_sort_list(vdat);
  view_files_display_list(vdat);
}

static void view_files_open_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  snd_info *sp;
  ss->open_requestor = FROM_VIEW_FILES;
  if (vdat->currently_selected_files > 0)
    {
      int i;
      for (i = 0; i < vdat->currently_selected_files; i++)
	sp = snd_open_file(vdat->full_names[vdat->selected_files[i]], FILE_READ_WRITE);
      if (sp) select_channel(sp, 0); 
    }
}

static void view_files_remove_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  int i, loc;
  view_files_info *vdat = (view_files_info *)context;
  for (i = 0; i < vdat->currently_selected_files; i++)
    {
      loc = vdat->selected_files[i];
      if (vdat->names[loc])
	{
	  FREE(vdat->names[loc]); 
	  vdat->names[loc] = NULL;
	  FREE(vdat->full_names[loc]); 
	  vdat->full_names[loc] = NULL;
	}
    }
  vdat->currently_selected_files = 0;
  view_files_update_list(vdat);
  view_files_display_list(vdat);
}

static off_t vf_location(view_files_info *vdat)
{
  off_t pos = 0;
  snd_info *sp;
  chan_info *cp;
  char *str;
  switch (vdat->location_choice)
    {
    case VF_AT_CURSOR:
      sp = any_selected_sound();
      if (sp)
	{
	  cp = any_selected_channel(sp);
	  return(CURSOR(cp));
	}
      break;
    case VF_AT_END:
      sp = any_selected_sound();
      if (sp)
	{
	  cp = any_selected_channel(sp);
	  return(CURRENT_SAMPLES(cp));
	}
      break;
    case VF_AT_BEGINNING:
      return(0);
      break;
    case VF_AT_MARK:
      str = XmTextGetString(vdat->at_mark_text);
      if ((str) && (*str))
	{
	  pos = mark_id_to_sample(string_to_int_with_error(str, 0, "mark"));
	  XtFree(str);
	  if (pos < 0)
	    snd_error("no such mark");
	}
      else snd_error("no mark?");
      break;
    case VF_AT_SAMPLE:
      str = XmTextGetString(vdat->at_sample_text);
      if ((str) && (*str))
	{
	  pos = string_to_off_t_with_error(str, 0, "sample"); 
	  XtFree(str);
	  /* pos already checked for lower bound */
	}
      else snd_error("no sample number?");
      break;
    }
  return(pos);
}

static void vf_clear_error(view_files_info *vdat)
{
  if (vdat->currently_selected_files == 1)
    vf_post_info(vdat, vdat->selected_files[0]);
  else
    {
      if (vdat->currently_selected_files == 0)
	vf_unpost_info(vdat);
      else vf_post_selected_files_list(vdat);
    }
  vdat->error_p = false;
}

static void vf_sample_button_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  vf_clear_sample((view_files_info *)context);
} 

static void vf_sample_text_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  vf_clear_sample((view_files_info *)context);
  cbs->doit = true;
} 

static void vf_clear_sample(view_files_info *vdat)
{
  vf_clear_error(vdat);
  XtRemoveCallback(vdat->at_sample_text, XmNmodifyVerifyCallback, vf_sample_text_modify_callback, (XtPointer)vdat);
  XtRemoveCallback(vdat->at_sample_button, XmNvalueChangedCallback, vf_sample_button_modify_callback, (XtPointer)vdat);
}

static void vf_clear_mark(view_files_info *vdat);

static void vf_mark_button_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  vf_clear_mark((view_files_info *)context);
}

static void vf_mark_text_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  vf_clear_mark((view_files_info *)context);
  cbs->doit = true;
}

static void vf_clear_mark(view_files_info *vdat)
{
  vf_clear_error(vdat);
  XtRemoveCallback(vdat->at_mark_text, XmNmodifyVerifyCallback, vf_mark_text_modify_callback, (XtPointer)vdat);
  XtRemoveCallback(vdat->at_mark_button, XmNvalueChangedCallback, vf_mark_button_modify_callback, (XtPointer)vdat);
}

static void vf_post_error(const char *error_msg, void *data)
{
  view_files_info *vdat = (view_files_info *)data;
  XmString msg;
  vdat->error_p = true;
  msg = XmStringCreate((char *)error_msg, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(vdat->info1,
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);
  msg = XmStringCreate("|", XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(vdat->info2,
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);
}

static void vf_post_location_error(const char *error_msg, void *data)
{
  view_files_info *vdat = (view_files_info *)data;
  vf_post_error(error_msg, data);
  if (vdat->location_choice == VF_AT_SAMPLE)
    {
      /* watch at_sample_text or button (undo) */
      XtAddCallback(vdat->at_sample_text, XmNmodifyVerifyCallback, vf_sample_text_modify_callback, (XtPointer)vdat);
      XtAddCallback(vdat->at_sample_button, XmNvalueChangedCallback, vf_sample_button_modify_callback, (XtPointer)vdat);
    }
  else
    {
      /* watch at_mark_text or button */
      XtAddCallback(vdat->at_mark_text, XmNmodifyVerifyCallback, vf_mark_text_modify_callback, (XtPointer)vdat);
      XtAddCallback(vdat->at_mark_button, XmNvalueChangedCallback, vf_mark_button_modify_callback, (XtPointer)vdat);
    }
}

static void view_files_mix_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  off_t beg;
  vdat->error_p = false;
  redirect_snd_error_to(vf_post_location_error, (void *)vdat);
  beg = vf_location(vdat);
  redirect_snd_error_to(NULL, NULL);
  if (!(vdat->error_p))
    {
      int i, len;
      int id_or_error = 0;
      snd_info *sp;
      sp = any_selected_sound();

      redirect_snd_error_to(vf_post_error, (void *)vdat);
      ss->sgx->requestor_dialog = w;
      ss->open_requestor = FROM_VIEW_FILES_MIX_DIALOG;

      len = vdat->currently_selected_files;
      if ((len == 1) &&
	  (snd_feq(vdat->amp, 1.0)) &&
	  (snd_feq(vdat->speed, 1.0)) &&
	  (default_env_p(vdat->amp_env)))
	id_or_error = mix_complete_file(sp, beg, 
					vdat->full_names[vdat->selected_files[0]], 
					with_mix_tags(ss), 
					DONT_DELETE_ME, 0, true); /* all-chans = true */
      else
	{
	  char *tempfile;
	  char **selected_files;
	  selected_files = vf_selected_files(vdat);
	  tempfile = scale_and_src(selected_files, len, sp->nchans, vdat->amp, vdat->speed, vdat->amp_env);
	  id_or_error = mix_complete_file(sp, beg, 
					  tempfile,
					  with_mix_tags(ss), 
					  (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
					  0, true); /* all-chans = true */
	  FREE(tempfile);
	  for (i = 0; i < len; i++)
	    FREE(selected_files[i]);
	  FREE(selected_files);
	}

      /* "id_or_error" here is either one of the mix id's or an error indication such as MIX_FILE_NO_MIX */
      /*    the possible error conditions have been checked already, or go through snd_error */

      redirect_snd_error_to(NULL, NULL);
      if (id_or_error < 0) /* actually -1 .. -3 */
	{
	  /* TODO: handle errors here
	  if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
	    clear_error_if_open_changes(fd->dialog, (void *)fd);
	  */
	}
      /*
      else 
	report_in_minibuffer(sp, _("%s mixed in at " OFF_TD), <file>, beg);
      */
    }
  else
    {
    }
}

static void view_files_insert_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  off_t beg;
  vdat->error_p = false;
  redirect_snd_error_to(vf_post_location_error, (void *)vdat);
  beg = vf_location(vdat);
  redirect_snd_error_to(NULL, NULL);
  if (!(vdat->error_p))
    {
      int i, len;
      bool ok = false;
      snd_info *sp;
      sp = any_selected_sound();

      redirect_snd_error_to(vf_post_error, (void *)vdat);
      ss->sgx->requestor_dialog = w;
      ss->open_requestor = FROM_VIEW_FILES_INSERT_DIALOG;

      len = vdat->currently_selected_files;
      if ((len == 1) &&
	  (snd_feq(vdat->amp, 1.0)) &&
	  (snd_feq(vdat->speed, 1.0)) &&
	  (default_env_p(vdat->amp_env)))
	ok = insert_complete_file(sp, 
				  vdat->full_names[vdat->selected_files[0]], 
				  beg,
				  DONT_DELETE_ME);
      else
	{
	  char *tempfile;
	  char **selected_files;
	  selected_files = vf_selected_files(vdat);
	  tempfile = scale_and_src(selected_files, len, sp->nchans, vdat->amp, vdat->speed, vdat->amp_env);
	  ok = insert_complete_file(sp, 
				    tempfile,
				    beg,
				    (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME);
	  FREE(tempfile);
	  for (i = 0; i < len; i++)
	    FREE(selected_files[i]);
	  FREE(selected_files);
	}

      redirect_snd_error_to(NULL, NULL);
      if (!ok)
	{
	  /* TODO: if raw, we need to complete the circle here
	  if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
	    clear_error_if_open_changes(fd->dialog, (void *)fd);
	  */
	}
      /*
      else 
        -- this can be a list of names
	report_in_minibuffer(sp, _("%s inserted at " OFF_TD), <file>, beg);
      */

    }
  else
    {
      /* is there anything to do here? */
    }
}

static void view_files_at_cursor_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  if (vdat->error_p)
    {
      if (vdat->location_choice == VF_AT_SAMPLE)
	vf_clear_sample(vdat);
      else vf_clear_mark(vdat);
    }
  XmToggleButtonSetState(vdat->at_cursor_button, true, false);
  XmToggleButtonSetState(vdat->at_end_button, false, false);
  XmToggleButtonSetState(vdat->at_beginning_button, false, false);
  XmToggleButtonSetState(vdat->at_mark_button, false, false);
  XmToggleButtonSetState(vdat->at_sample_button, false, false);
  vdat->location_choice = VF_AT_CURSOR;
}

static void view_files_at_end_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  if (vdat->error_p)
    {
      if (vdat->location_choice == VF_AT_SAMPLE)
	vf_clear_sample(vdat);
      else vf_clear_mark(vdat);
    }
  XmToggleButtonSetState(vdat->at_cursor_button, false, false);
  XmToggleButtonSetState(vdat->at_end_button, true, false);
  XmToggleButtonSetState(vdat->at_beginning_button, false, false);
  XmToggleButtonSetState(vdat->at_mark_button, false, false);
  XmToggleButtonSetState(vdat->at_sample_button, false, false);
  vdat->location_choice = VF_AT_END;
}

static void view_files_at_beginning_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  if (vdat->error_p)
    {
      if (vdat->location_choice == VF_AT_SAMPLE)
	vf_clear_sample(vdat);
      else vf_clear_mark(vdat);
    }
  XmToggleButtonSetState(vdat->at_cursor_button, false, false);
  XmToggleButtonSetState(vdat->at_end_button, false, false);
  XmToggleButtonSetState(vdat->at_beginning_button, true, false);
  XmToggleButtonSetState(vdat->at_mark_button, false, false);
  XmToggleButtonSetState(vdat->at_sample_button, false, false);
  vdat->location_choice = VF_AT_BEGINNING;
}

static void view_files_at_sample_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  if ((vdat->error_p) && 
      (vdat->location_choice == VF_AT_MARK))
      vf_clear_mark(vdat);
  XmToggleButtonSetState(vdat->at_cursor_button, false, false);
  XmToggleButtonSetState(vdat->at_end_button, false, false);
  XmToggleButtonSetState(vdat->at_beginning_button, false, false);
  XmToggleButtonSetState(vdat->at_mark_button, false, false);
  XmToggleButtonSetState(vdat->at_sample_button, true, false);
  vdat->location_choice = VF_AT_SAMPLE;
}

static void view_files_at_mark_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  if ((vdat->error_p) &&
      (vdat->location_choice == VF_AT_SAMPLE))
    vf_clear_sample(vdat);
  XmToggleButtonSetState(vdat->at_cursor_button, false, false);
  XmToggleButtonSetState(vdat->at_end_button, false, false);
  XmToggleButtonSetState(vdat->at_beginning_button, false, false);
  XmToggleButtonSetState(vdat->at_mark_button, true, false);
  XmToggleButtonSetState(vdat->at_sample_button, false, false);
  vdat->location_choice = VF_AT_MARK;
}


/* -------- speed -------- */

static int vf_speed_to_scroll(Float minval, Float val, Float maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * ((log(val) - log(minval)) / (log(maxval) - log(minval)))));
}

static void vf_set_speed(view_files_info *vdat, Float val)
{
  char speed_number_buffer[6];
  vdat->speed = speed_changed(val,
			      speed_number_buffer,
			      vdat->speed_style,
			      speed_control_tones(ss),
			      6);
  set_label(vdat->speed_number, speed_number_buffer);
  XtVaSetValues(vdat->speed_scrollbar, 
		XmNvalue, vf_speed_to_scroll(speed_control_min(ss), val, speed_control_max(ss)), 
		NULL);
}

static void vf_speed_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  vf_set_speed(vdat, 1.0);
  XtVaSetValues(vdat->speed_scrollbar, 
		XmNvalue, vf_speed_to_scroll(speed_control_min(ss), 1.0, speed_control_max(ss)), 
		NULL);
}

Float view_files_speed(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->speed);
  return(1.0);
}

Float view_files_set_speed(widget_t dialog, Float new_speed)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    vf_set_speed(vdat, new_speed);
  return(new_speed);
}

static void vf_speed_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  vf_set_speed(vdat, exp((cb->value * 
			  (log(speed_control_max(ss)) - log(speed_control_min(ss))) / 
			  (0.9 * SCROLLBAR_MAX)) + log(speed_control_min(ss))));
}

static void vf_speed_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  XmScrollBarCallbackStruct *cb = (XmScrollBarCallbackStruct *)info;
  vf_set_speed(vdat, exp((cb->value * 
			  (log(speed_control_max(ss)) - log(speed_control_min(ss))) / 
			  (0.9 * SCROLLBAR_MAX)) + log(speed_control_min(ss))));
}



/* -------- amp -------- */

static Float vf_scroll_to_amp(int val)
{
  if (val <= 0) 
    return(amp_control_min(ss));
  if (val >= (0.9 * SCROLLBAR_MAX)) 
    return(amp_control_max(ss));
  if (val > (0.5 * 0.9 * SCROLLBAR_MAX))
    return((((val / (0.5 * 0.9 * SCROLLBAR_MAX)) - 1.0) * (amp_control_max(ss) - 1.0)) + 1.0);
  else return((val * (1.0 - amp_control_min(ss)) / (0.5 * 0.9 * SCROLLBAR_MAX)) + amp_control_min(ss));
}

static int vf_amp_to_scroll(Float amp)
{
  return(amp_to_scroll(amp_control_min(ss), amp, amp_control_max(ss)));
}

static void vf_set_amp(view_files_info *vdat, Float val)
{
  char sfs[6];
  vdat->amp = val;
  mus_snprintf(sfs, 6, "%.2f", val);
  set_label(vdat->amp_number, sfs);
  XtVaSetValues(vdat->amp_scrollbar, 
		XmNvalue, amp_to_scroll(amp_control_min(ss), val, amp_control_max(ss)), 
		NULL);
}

Float view_files_amp(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->amp);
  return(0.0);
}

Float view_files_set_amp(widget_t dialog, Float new_amp)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    vf_set_amp(vdat, new_amp);
  return(new_amp);
}


static void vf_amp_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  vf_set_amp((view_files_info *)context, 1.0);
}

static void vf_amp_valuechanged_callback(Widget w, XtPointer context, XtPointer info) 
{
  vf_set_amp((view_files_info *)context, 
	     vf_scroll_to_amp(((XmScrollBarCallbackStruct *)info)->value));
}

static void vf_amp_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  vf_set_amp((view_files_info *)context, 
	     vf_scroll_to_amp(((XmScrollBarCallbackStruct *)info)->value));
}




/* -------- amp-envs -------- */

#if 0
/* g_channel_amp_envs gets the data (snd-snd) */
static void vf_show_background_wave(void)
{
  env_editor *e;
  int pts;
  bool two_sided = false;
  e = spf;
  if (e == NULL) return;
  /*
  pts = prepare_mix_id_waveform(mix_id, e->axis, &two_sided);
  if (pts > 0)
    {
      XSetForeground(MAIN_DISPLAY(ss), ax->gc, ss->sgx->enved_waveform_color);
      if (two_sided)
	draw_both_grf_points(1, ax, pts, GRAPH_LINES);
      else draw_grf_points(1, ax, pts, e->axis, ungrf_y(e->axis, 0.0), GRAPH_LINES);
      XSetForeground(MAIN_DISPLAY(ss), ax->gc, ss->sgx->black);
    }
  */
}
#endif

static void vf_amp_env_resize(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  if (vdat->env_ax == NULL)
    {
      XGCValues gv;
      gv.function = GXcopy;
      XtVaGetValues(vdat->env_drawer, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
      vdat->env_gc = XtGetGC(vdat->env_drawer, GCForeground | GCFunction, &gv);
      vdat->env_ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      vdat->env_ax->wn = XtWindow(vdat->env_drawer);
      vdat->env_ax->dp = XtDisplay(vdat->env_drawer);
      vdat->env_ax->gc = vdat->env_gc;
      if (!(vdat->env_ax->wn)) return;
    }
  else 
    {
      if (!(vdat->env_ax->wn))
	{
	  vdat->env_ax->wn = XtWindow(vdat->env_drawer); /* sometimes the dialog window is not ready when display_env gets called */
	  if (!(vdat->env_ax->wn)) return;
	}
      clear_window(vdat->env_ax);
    }
  vdat->spf->with_dots = true;
  env_editor_display_env(vdat->spf, vdat->amp_env, vdat->env_ax, _("amp env"), 
			 0, 0,
			 widget_width(w), widget_height(w), 
			 NOT_PRINTING);
  /* show_mix_background_wave(mix_dialog_id, chan); */
}

env *view_files_amp_env(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->amp_env);
  return(NULL);
}

env *view_files_set_amp_env(widget_t dialog, env *new_e)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      if (vdat->amp_env) free_env(vdat->amp_env);
      vdat->amp_env = copy_env(new_e);
      if ((vdat->dialog) &&
	  (XtIsManaged(vdat->dialog)))
	vf_amp_env_resize(vdat->env_drawer, (void *)vdat, NULL);
    }
  return(new_e);
}

speed_style_t view_files_speed_style(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->speed_style);
  return(SPEED_CONTROL_AS_FLOAT);
}

speed_style_t view_files_set_speed_style(widget_t dialog, speed_style_t speed_style)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    vdat->speed_style = speed_style;
  return(speed_style);
}


#ifdef MUS_MAC_OSX
static int press_x, press_y;
#endif

static void vf_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  view_files_info *vdat = (view_files_info *)context;
  XMotionEvent *ev = (XMotionEvent *)event;
  Float pos;

#ifdef MUS_MAC_OSX
  if ((press_x == ev->x) && (press_y == ev->y)) return;
#endif

  pos = (Float)(ev->x) / (Float)widget_width(w);
  env_editor_button_motion(vdat->spf, ev->x, ev->y, ev->time, vdat->amp_env);
  vf_amp_env_resize(w, context, NULL);
}

static void vf_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  view_files_info *vdat = (view_files_info *)context;
  XButtonEvent *ev = (XButtonEvent *)event;
  Float pos;

#ifdef MUS_MAC_OSX
  press_x = ev->x;
  press_y = ev->y;
#endif

  pos = (Float)(ev->x) / (Float)widget_width(w);
  if (env_editor_button_press(vdat->spf, ev->x, ev->y, ev->time, vdat->amp_env))
    vf_amp_env_resize(w, context, NULL);
}

static void vf_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  view_files_info *vdat = (view_files_info *)context;
  XButtonEvent *ev = (XButtonEvent *)event;
  Float pos;

  pos = (Float)(ev->x) / (Float)widget_width(w);
  env_editor_button_release(vdat->spf, vdat->amp_env);
  vf_amp_env_resize(w, context, NULL);
}

static void blue_textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info)
{
  if (!(ss->using_schemes)) 
    XtVaSetValues(w, XmNbackground, ss->sgx->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}

static void blue_mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (!(ss->using_schemes)) 
    XtVaSetValues(w, XmNbackground, ss->sgx->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}

static void white_mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  if (!(ss->using_schemes)) 
    XtVaSetValues(w, XmNbackground, ss->sgx->text_focus_color, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, true, NULL);
}

static void vf_reflect_sort_choice_in_menu(view_files_info *vdat)
{
  int i;
  set_sensitive(vdat->by_name, vdat->sorter != SORT_BY_NAME);
  set_sensitive(vdat->by_date, vdat->sorter != SORT_BY_DATE);
  set_sensitive(vdat->by_size, vdat->sorter != SORT_BY_SIZE);
  set_sensitive(vdat->by_entry, vdat->sorter != SORT_BY_ENTRY);
  for (i = 0; i < vdat->sort_items_size; i++)
    if (XtIsManaged(vdat->sort_items[i]))
      set_sensitive(vdat->sort_items[i], vdat->sorter != (SORT_BY_PROC + i));
}

void view_files_reflect_sort_items(void)
{
  view_files_info *vdat;

  int i, j = 0, k;
  if (view_files_info_size == 0) return;
  for (i = 0; i < ss->file_sorters_size; i++)
    {
      XEN ref;
      ref = XEN_VECTOR_REF(ss->file_sorters, i);
      if (XEN_PAIR_P(ref))
	{
	  XmString s1;
	  s1 = XmStringCreate(XEN_TO_C_STRING(XEN_CAR(ref)), XmFONTLIST_DEFAULT_TAG);
	  for (k = 0; k < view_files_info_size; k++)
	    if ((view_files_infos[k]) &&
		(view_files_infos[k]->dialog))
	      {
		vdat = view_files_infos[k];
		XtVaSetValues(vdat->sort_items[j], 
			      XmNlabelString, s1,
			      XmNuserData, i + SORT_BY_PROC, /* this is an index into the file_sorters list, not the widget list */
			      NULL);
		XtManageChild(vdat->sort_items[j]);
	      }
	  j++;
	  XmStringFree(s1);
	}
    }

  for (k = 0; k < view_files_info_size; k++)
    if ((view_files_infos[k]) &&
	(view_files_infos[k]->dialog))
      {
	vdat = view_files_infos[k];
	for (i = j; i < vdat->sort_items_size; i++)
	  XtUnmanageChild(vdat->sort_items[i]);
	vf_reflect_sort_choice_in_menu(vdat);
      }
}

/* (add-file-sorter "duration" 
		(lambda (lst)
		  (sort lst 
			(lambda (a b)
			  (> (mus-sound-duration a) (mus-sound-duration b))))))

 */


int view_files_local_sort(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->sorter);
  return(-1);
}

int view_files_set_local_sort(widget_t dialog, int sort_choice)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      vdat->sorter = sort_choice;
      view_files_sort_list(vdat);
      view_files_display_list(vdat);
      vf_reflect_sort_choice_in_menu(vdat);
    }
  return(sort_choice);
}

static Widget start_view_files_dialog_1(view_files_info *vdat, bool managed)
{
  if (!(vdat->dialog))
    {
      int n;
      Arg args[20];
      XmString xdismiss, xhelp, titlestr, new_viewer_str, s1, bstr;
      Widget mainform, viewform, vertical_sep, leftform;
      Widget left_title_sep, add_text, add_label, sep1, sep2, sep3, sep4, sep5, sep6, sep7, sort_cascade_menu;
      Widget plw, rlw, sbar;
      XtCallbackList n1, n2, n3, n4;
      Widget amp_label, speed_label, env_frame;
      Widget bframe, bform;

      xdismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(_("Files"), XmFONTLIST_DEFAULT_TAG);
      new_viewer_str = XmStringCreate(_("New Viewer"), XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNcancelLabelString, new_viewer_str); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      vdat->dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), "Files", args, n);

      XtAddCallback(vdat->dialog, XmNhelpCallback,   view_files_help_callback,       (XtPointer)vdat);
      XtAddCallback(vdat->dialog, XmNokCallback,     view_files_dismiss_callback,    (XtPointer)vdat);
      XtAddCallback(vdat->dialog, XmNcancelCallback, view_files_new_viewer_callback, (XtPointer)vdat);

      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);
      XmStringFree(new_viewer_str);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(vdat->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(vdat->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(vdat->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->reset_button_color,  NULL);
	  XtVaSetValues(XmMessageBoxGetChild(vdat->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->quit_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(vdat->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(vdat->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->reset_button_color,  NULL);
	}

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(vdat->dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, vdat->dialog, args, n);


      /* -------- left side controls -------- */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 49); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      leftform = XtCreateManagedWidget("leftform", xmFormWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      vdat->left_title = XtCreateManagedWidget("(no files selected)", xmLabelWidgetClass, leftform, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->left_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmDOUBLE_LINE); n++;
      left_title_sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, leftform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, left_title_sep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      vdat->info1 = XtCreateManagedWidget("|", xmLabelWidgetClass, leftform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->info1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      vdat->info2 = XtCreateManagedWidget("|", xmLabelWidgetClass, leftform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->info2); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep6 = XtCreateManagedWidget("dialog-sep1", xmSeparatorWidgetClass, leftform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep6); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      vdat->openB = XtCreateManagedWidget(_("Open"), xmPushButtonGadgetClass, leftform, args, n);
      XtAddCallback(vdat->openB, XmNactivateCallback, view_files_open_selected_callback, (XtPointer)vdat);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->openB); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->openB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      vdat->removeB = XtCreateManagedWidget(_("Remove"), xmPushButtonGadgetClass, leftform, args, n);
      XtAddCallback(vdat->removeB, XmNactivateCallback, view_files_remove_selected_callback, (XtPointer)vdat);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->openB); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep5 = XtCreateManagedWidget("dialog-sep1", xmSeparatorWidgetClass, leftform, args, n);


      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->zoom_color); n++;}
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNborderColor, ss->sgx->zoom_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep5); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 2); n++;
      bframe = XtCreateManagedWidget("bframe", xmFrameWidgetClass, leftform, args, n);      

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      bform = XtCreateManagedWidget("bform", xmFormWidgetClass, bframe, args, n);      

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      vdat->mixB = XtCreateManagedWidget(_("Mix"), xmPushButtonGadgetClass, bform, args, n);
      XtAddCallback(vdat->mixB, XmNactivateCallback, view_files_mix_selected_callback, (XtPointer)vdat);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->mixB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      vdat->insertB = XtCreateManagedWidget(_("Insert"), xmPushButtonGadgetClass, bform, args, n);
      XtAddCallback(vdat->insertB, XmNactivateCallback, view_files_insert_selected_callback, (XtPointer)vdat);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
	}
      bstr = XmStringCreate(_("at cursor"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->mixB); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      XtSetArg(args[n], XmNset, XmSET); n++;
      vdat->at_cursor_button = make_togglebutton_widget("at-cursor-button", bform, args, n);
      XtAddCallback(vdat->at_cursor_button, XmNdisarmCallback, view_files_at_cursor_callback, (XtPointer)vdat);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
	}
      bstr = XmStringCreate(_("at end"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->at_cursor_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      vdat->at_end_button = make_togglebutton_widget("at_end-button", bform, args, n);
      XtAddCallback(vdat->at_end_button, XmNdisarmCallback, view_files_at_end_callback, (XtPointer)vdat);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
	}
      bstr = XmStringCreate(_("at beginning"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->at_end_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      vdat->at_beginning_button = make_togglebutton_widget("at-beginning-button", bform, args, n);
      XtAddCallback(vdat->at_beginning_button, XmNdisarmCallback, view_files_at_beginning_callback, (XtPointer)vdat);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
	}
      bstr = XmStringCreate(_("at sample"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->at_beginning_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      vdat->at_sample_button = make_togglebutton_widget("at-sample-button", bform, args, n);
      XtAddCallback(vdat->at_sample_button, XmNdisarmCallback, view_files_at_sample_callback, (XtPointer)vdat);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;}
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->at_beginning_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, vdat->at_sample_button); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->at_sample_button); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      vdat->at_sample_text = make_textfield_widget("at-sample-text", bform, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
      XtRemoveCallback(vdat->at_sample_text, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
      XtAddCallback(vdat->at_sample_text, XmNlosingFocusCallback, blue_textfield_unfocus_callback, NULL);
      XtAddEventHandler(vdat->at_sample_text, LeaveWindowMask, false, blue_mouse_leave_text_callback, NULL);
      XtAddEventHandler(vdat->at_sample_text, EnterWindowMask, false, white_mouse_enter_text_callback, NULL);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
	}
      bstr = XmStringCreate(_("at mark"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->at_sample_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      vdat->at_mark_button = make_togglebutton_widget("at-beginning-button", bform, args, n);
      XtAddCallback(vdat->at_mark_button, XmNdisarmCallback, view_files_at_mark_callback, (XtPointer)vdat);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->at_sample_text); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->at_mark_button); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      vdat->at_mark_text = make_textfield_widget("at-mark-text", bform, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
      XtRemoveCallback(vdat->at_mark_text, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
      XtAddCallback(vdat->at_mark_text, XmNlosingFocusCallback, blue_textfield_unfocus_callback, NULL);
      XtAddEventHandler(vdat->at_mark_text, LeaveWindowMask, false, blue_mouse_leave_text_callback, NULL);
      XtAddEventHandler(vdat->at_mark_text, EnterWindowMask, false, white_mouse_enter_text_callback, NULL);


      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, bframe); n++;

      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      sep4 = XtCreateManagedWidget("sep4", xmSeparatorWidgetClass, leftform, args, n);

      n = 0;      
      /* AMP */
      s1 = XmStringCreate(_("amp:"), XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep4); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      /* XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; */
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      amp_label = make_pushbutton_widget ("amp-label", leftform, args, n);
      XtAddCallback(amp_label, XmNactivateCallback, vf_amp_click_callback, (XtPointer)vdat);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreate("1.0   ", XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep4); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, amp_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      /* XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; */
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      vdat->amp_number = XtCreateManagedWidget ("amp-number", xmLabelWidgetClass, leftform, args, n);
      XmStringFree(s1);

      n = 0;      
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep4); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->amp_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, vf_amp_to_scroll(1.0)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(vf_amp_valuechanged_callback, (XtPointer)vdat)); n++;
      XtSetArg(args[n], XmNdragCallback, n3 = make_callback_list(vf_amp_drag_callback, (XtPointer)vdat)); n++;
      vdat->amp_scrollbar = XtCreateManagedWidget("amp", xmScrollBarWidgetClass, leftform, args, n);

      n = 0;
      /* SPEED */
      s1 = XmStringCreate(_("speed:"), XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, amp_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      /* XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++;  */
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      speed_label = make_pushbutton_widget ("speed-label", leftform, args, n);
      XtAddCallback(speed_label, XmNactivateCallback, vf_speed_click_callback, (XtPointer)vdat);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(speed_control_style(ss));
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, speed_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, speed_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      /* XtSetArg(args[n], XmNmarginHeight, CONTROLS_MARGIN); n++; */
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNmarginRight, 3); n++;
      vdat->speed_number = XtCreateManagedWidget ("speed-number", xmLabelWidgetClass, leftform, args, n);
      XmStringFree(s1);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->speed_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->speed_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, SCROLLBAR_MAX); n++;
      XtSetArg(args[n], XmNvalue, vf_speed_to_scroll(speed_control_min(ss), 1.0, speed_control_max(ss))); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n4 = make_callback_list(vf_speed_valuechanged_callback, (XtPointer)vdat)); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(vf_speed_drag_callback, (XtPointer)vdat)); n++;
      vdat->speed_scrollbar = XtCreateManagedWidget("speed-scroll", xmScrollBarWidgetClass, leftform, args, n);


      /* separator before envelope */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, speed_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 8); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep7 = XtCreateManagedWidget("dialog-sep1", xmSeparatorWidgetClass, leftform, args, n);


      /* amp env */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep7); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 4); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 98); n++;
      XtSetArg(args[n], XmNheight, 100); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 4); n++;
      env_frame = XtCreateManagedWidget("amp-env-frame", xmFrameWidgetClass, leftform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNheight, 100); n++;
      vdat->env_drawer = XtCreateManagedWidget("amp-env-window", xmDrawingAreaWidgetClass, env_frame, args, n);



      /* -------- middle vertical separator -------- */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, leftform); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNwidth, 4); n++;
      vertical_sep = XtCreateManagedWidget("vertical-sep", xmSeparatorWidgetClass, mainform, args, n);


      /* -------- right side file box -------- */

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vertical_sep); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      viewform = XtCreateManagedWidget("viewform", xmFormWidgetClass, mainform, args, n);

      /* Add dir/file text entry at bottom */
      n = 0;
      s1 = XmStringCreate(_("add:"), XmFONTLIST_DEFAULT_TAG);
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      add_label = XtCreateManagedWidget("add", xmLabelWidgetClass, viewform, args, n);
      XmStringFree(s1);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, add_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      add_text = make_textfield_widget("add-text", viewform, args, n, ACTIVATABLE, add_completer_func(filename_completer));
      XtAddCallback(add_text, XmNactivateCallback, view_files_add_files, (XtPointer)vdat);
      
      /* SOMEDAY: file filters here also */

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, add_text); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNheight, 4); n++;
      sep2 = XtCreateManagedWidget("sep2", xmSeparatorWidgetClass, viewform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sep2); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      vdat->updateB = XtCreateManagedWidget(_("Update"), xmPushButtonGadgetClass, viewform, args, n);
      /* need Gadget if we want a subsequent XmNbackgroundPixmap change to be reflected in the button */
      XtAddCallback(vdat->updateB, XmNactivateCallback, view_files_update_callback, (XtPointer)vdat);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sep2); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->updateB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      vdat->clearB = XtCreateManagedWidget(_("Clear"), xmPushButtonGadgetClass, viewform, args, n);
      XtAddCallback(vdat->clearB, XmNactivateCallback, view_files_clear_callback, (XtPointer)vdat);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, vdat->updateB); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNheight, 4); n++;
      sep3 = XtCreateManagedWidget("sep3", xmSeparatorWidgetClass, viewform, args, n);


      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      rlw = XtCreateManagedWidget(_("files"), xmLabelWidgetClass, viewform, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, rlw); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmDOUBLE_LINE); n++;
      sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, viewform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 5); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      plw = XtCreateManagedWidget(_("play"), xmLabelWidgetClass, viewform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      sbar = XmCreateMenuBar(viewform, "menuBar", args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      vdat->smenu = XmCreatePulldownMenu(sbar, _("sort"), args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNsubMenuId, vdat->smenu); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      sort_cascade_menu = XtCreateManagedWidget("sort", xmCascadeButtonWidgetClass, sbar, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      vdat->by_name =  XtCreateManagedWidget(_("name"),  xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->by_date =  XtCreateManagedWidget(_("date"),  xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->by_size =  XtCreateManagedWidget(_("size"),  xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->by_entry = XtCreateManagedWidget(_("entry"), xmPushButtonWidgetClass, vdat->smenu, args, n);

      /* TODO: sort_items list growth */
      {
	/* for now... */
	int i;
	vdat->sort_items_size = 4;
	vdat->sort_items = (Widget *)CALLOC(vdat->sort_items_size, sizeof(Widget));
	for (i = 0; i < vdat->sort_items_size; i++)
	  {
	    vdat->sort_items[i] = XtCreateWidget("unused", xmPushButtonWidgetClass, vdat->smenu, args, n);
	  }
      }

      XtManageChild(sbar);

      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 5); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, plw); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sep3); n++;
      XtSetArg(args[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
      XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
      vdat->file_list = XmCreateScrolledWindow(viewform, "file_list", args, n);

      n = attach_all_sides(args, 0);
      vdat->file_list_holder = XtCreateManagedWidget("file_list_holder", xmRowColumnWidgetClass, vdat->file_list, args, n);
      XtVaSetValues(vdat->file_list, 
		    XmNworkWindow, vdat->file_list_holder, 
		    NULL);
      add_drop(vdat->file_list, view_files_drop_watcher, (void *)vdat);

      if (managed) view_files_display_list(vdat);

      XtAddCallback(vdat->by_name,     XmNactivateCallback, sort_view_files_by_name,        (XtPointer)vdat);
      XtAddCallback(vdat->by_date,     XmNactivateCallback, sort_view_files_by_date,        (XtPointer)vdat);
      XtAddCallback(vdat->by_size,     XmNactivateCallback, sort_view_files_by_size,        (XtPointer)vdat);
      XtAddCallback(vdat->by_entry,    XmNactivateCallback, sort_view_files_by_entry_order, (XtPointer)vdat);
      {
	int i;
	for (i = 0; i < vdat->sort_items_size; i++)
	  XtAddCallback(vdat->sort_items[i], XmNactivateCallback, sort_view_files_by_procedure, (XtPointer)vdat);
      }
      /* XtAddCallback(sort_cascade_menu, XmNcascadingCallback, vf_display_sort_items, (XtPointer)vdat); -- segfaults */

      if (!(ss->using_schemes)) 
	map_over_children(vdat->file_list, set_main_color_of_widget, NULL);

      set_dialog_widget(VIEW_FILES_DIALOG, vdat->dialog);

      if (managed)
	XtManageChild(vdat->dialog);

      XtAddCallback(vdat->env_drawer, XmNresizeCallback, vf_amp_env_resize, (XtPointer)vdat);
      XtAddCallback(vdat->env_drawer, XmNexposeCallback, vf_amp_env_resize, (XtPointer)vdat);

      vdat->spf = new_env_editor(); /* one global amp env */

      XtAddEventHandler(vdat->env_drawer, ButtonPressMask, false, vf_drawer_button_press, (XtPointer)vdat);
      XtAddEventHandler(vdat->env_drawer, ButtonMotionMask, false, vf_drawer_button_motion, (XtPointer)vdat);
      XtAddEventHandler(vdat->env_drawer, ButtonReleaseMask, false, vf_drawer_button_release, (XtPointer)vdat);

      FREE(n1);
      FREE(n2);
      FREE(n3);
      FREE(n4);

      vf_mix_insert_buttons_set_sensitive(vdat, false);
      vf_open_remove_buttons_set_sensitive(vdat, false); /* need selection */
      vf_clear_button_set_sensitive(vdat, vdat->end > 0);

    }
  else
    {
      if (managed) 
	{
	  if (!XtIsManaged(vdat->dialog)) 
	    XtManageChild(vdat->dialog);
	  raise_dialog(vdat->dialog);
	  view_files_sort_list(vdat);
	  view_files_display_list(vdat);
	}
    }
  if (managed)
    {
      vf_amp_env_resize(vdat->env_drawer, (XtPointer)vdat, NULL);
      view_files_reflect_sort_items();      
    }
  return(vdat->dialog);
}

static view_files_info *view_files_find_dialog(widget_t dialog)
{
  int i;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog == dialog))
      return(view_files_infos[i]);
  return(NULL);
}

Widget start_view_files_dialog(bool managed, bool make_new)
{
  int i;
  view_files_info *vdat = NULL;
  if (make_new)
    return(start_view_files_dialog_1(new_view_files_dialog(), managed));
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog))
      {
	vdat = view_files_infos[i];
	if (XtIsManaged(vdat->dialog))
	  break;
      }
  if (vdat)
    return(start_view_files_dialog_1(vdat, managed));
  return(start_view_files_dialog_1(new_view_files_dialog(), managed));
}

void save_view_files_dialogs(FILE *fd) 
{
#if HAVE_EXTENSION_LANGUAGE
  int i;
  view_files_info *vdat;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog) &&
	(XtIsManaged(view_files_infos[i]->dialog)))
      {
	int k;
	vdat = view_files_infos[i];
#if HAVE_SCHEME
	fprintf(fd, "(let ((vf (view-files-dialog #t #t)))\n");
	if (vdat->full_names)
	  {
	    fprintf(fd, "  (set! (view-files-files vf) (list");
	    for (k = 0; k <= vdat->end; k++)
	      fprintf(fd, " \"%s\"", vdat->full_names[k]);
	    fprintf(fd, "))\n");
	    if (vdat->currently_selected_files > 0)
	      {
		fprintf(fd, "  (set! (view-files-selected-files vf) (list");
		for (k = 0; k < vdat->currently_selected_files; k++)
		  fprintf(fd, " \"%s\"", vdat->full_names[vdat->selected_files[k]]);
		fprintf(fd, "))\n");
	      }
	  }
	if (!(snd_feq(vdat->amp, 1.0)))
	  {
	    fprintf(fd, "  (set! (view-files-amp vf) %.3f)\n", vdat->amp);
	  }
	if (!(snd_feq(vdat->speed, 1.0)))
	  {
	    fprintf(fd, "  (set! (view-files-speed vf) %.3f)\n", vdat->speed);
	  }
	if (!(default_env_p(vdat->amp_env)))
	  {
	    fprintf(fd, "  (set! (view-files-amp-env vf) %s)\n", env_to_string(vdat->amp_env));
	  }
	/* assume file-sorters are set up already */
	fprintf(fd, "  (set! (view-files-sort vf) %s)\n", view_files_sort_name(vdat->sorter));	    
	fprintf(fd, ")\n");
#endif
#if HAVE_RUBY
	fprintf(fd, "vf = view_files_dialog(true, true)\n");
	if (vdat->full_names)
	  {
	    fprintf(fd, "  set_view_files_files(vf, [");
	    for (k = 0; k < vdat->end; k++)
	      fprintf(fd, "\"%s\", ", vdat->full_names[k]);
	    fprintf(fd, "\"%s\"])\n", vdat->full_names[vdat->end]);
	    if (vdat->currently_selected_files > 0)
	      {
		fprintf(fd, "  set_view_files_selected_files(vf, [");
		for (k = 0; k < vdat->currently_selected_files - 1; k++)
		  fprintf(fd, "\"%s\", ", vdat->full_names[vdat->selected_files[k]]);
		fprintf(fd, "\"%s\"])\n", vdat->full_names[vdat->selected_files[vdat->currently_selected_files]]);
	      }
	  }
	if (!(snd_feq(vdat->amp, 1.0)))
	  {
	    fprintf(fd, "  set_view_files_amp(vf, %.3f)\n", vdat->amp);
	  }
	if (!(snd_feq(vdat->speed, 1.0)))
	  {
	    fprintf(fd, "  set_view_files_speed(vf, %.3f)\n", vdat->speed);
	  }
	if (!(default_env_p(vdat->amp_env)))
	  {
	    fprintf(fd, "  set_view_files_amp_env(vf, %s)\n", env_to_string(vdat->amp_env));
	  }
	/* assume file-sorters are set up already */
	fprintf(fd, "  set_view_files_sort(vf, %s)\n", TO_VAR_NAME(view_files_sort_name(vdat->sorter)));	    
	fprintf(fd, "\n");
#endif
      }
#endif
}

void view_files_add_directory(widget_t dialog, const char *dirname) 
{
  view_files_info *vdat = NULL;
  char *full_filename;
  if (dialog)
    vdat = view_files_find_dialog(dialog);
  else 
    {
      if (view_files_info_size > 0)
	vdat = view_files_infos[0];
      else 
	{
	  vdat = new_view_files_dialog();
	  start_view_files_dialog_1(vdat, false);
	}
    }
  if (vdat)
    {
      full_filename = mus_expand_filename((const char *)dirname);
      add_directory_to_view_files_list(vdat, full_filename);
      FREE(full_filename);
    }
}

void view_files_add_file(widget_t dialog, const char *filename)
{
  view_files_info *vdat = NULL;
  char *full_filename;
  if (dialog)
    vdat = view_files_find_dialog(dialog);
  else 
    {
      if (view_files_info_size > 0)
	vdat = view_files_infos[0];
      else 
	{
	  vdat = new_view_files_dialog();
	  start_view_files_dialog_1(vdat, false);
	}
    }
  if (vdat)
    {
      full_filename = mus_expand_filename((const char *)filename);
      add_file_to_view_files_list(vdat, filename, full_filename);
      FREE(full_filename);
    }
}


void g_init_gxfile(void)
{
#if HAVE_SCHEME
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.scm \
to popup file info as follows: \n\
(add-hook! " S_mouse_enter_label_hook "\n\
  (lambda (type position name)\n\
    (if (not (= type 2))\n\
        (" S_info_dialog " name (finfo name)))))\n\
See also nb.scm."
#else
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'."
#endif

  #define H_mouse_leave_label_hook S_mouse_leave_label_hook " (type position label): called when the mouse leaves a file viewer or region label"

  mouse_enter_label_hook = XEN_DEFINE_HOOK(S_mouse_enter_label_hook, 3, H_mouse_enter_label_hook);
  mouse_leave_label_hook = XEN_DEFINE_HOOK(S_mouse_leave_label_hook, 3, H_mouse_leave_label_hook);

#if DEBUGGING && HAVE_GUILE
  XEN_DEFINE_PROCEDURE("apply-edit-header", g_apply_edit_header, 0, 0, 0, "internal testing function");
#endif
}

/* TODO: always have a selected sound
 * TODO: vf fam + remove if file deleted = no need for update button? -> overall reset?
 */
