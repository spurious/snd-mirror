#include "snd.h"

/* various file-related dialogs:
   File|Edit-Save-as, 
   File:Open|View, 
   File|Edit-Mix, 
   File:Edit-Header, Raw, New
   View:Files and region lists 
*/


/* TODO: if file write-protected, fam to get rid of errmsg if user chmods it?
 * TODO: if edit-header of write-protected sound, should we add an 'Unprotect' button?  Is there are "right way" to implement it?
 * PERHAPS: location to mix at in File:Mix? amp/srate sliders?
 * TODO: pull-down list of recent files
 * TODO: add a sound filename completer to specialize filename_completer (if just_sounds, or perhaps restrict to sound if any are found)
 * TODO: extract overwrite question from save_as_dialog_save_sound
 * TODO: extract snd_overwrite_ok case: incorporate overwrite here
 * TODO: new file ok: if existing file by new name and it's write protected, we need some better explanation
 * TODO: new file: find some way to get around the hidden label bug (unmanage error text etc)
 * TODO: new file: mus_file_probe: use FAM to handle this
 * TODO: check out other paths to raw data dialog
 * PERHAPS: if user changes raw file with dialog up -- adding header for example, should we automatically open it?
 * TODO: should previous files list be monitored via FAM?
 * TODO: replace "current files" section with something useful --
 *   perhaps a menu for actions on previous files such as insert/mix/open/play
 *   or a grouping thing as in the (unimplemented) regions dialog
 * PERHAPS: (alert_new_file): handle all directory update decisions through FAM
 * PERHAPS: region save-as as button in region browser?
 * PERHAPS: raw_data: caller can pass continuation and callback funcs
 * SOMEDAY: new file ok: would be nice to cancel 'DoIt' if user deletes file by hand -- use FAM
 * TODO: raw give OGG/Mpeg/Speex choices if the progs can be found [needs configure support] -- why not translate in snd_translate?
 * TODO: various file/directory lists: tie into fam/gamin (also previous files list) -- add xen call?
 */


/* ---------------- open/mix dialogs ---------------- */

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
	  XtAddCallback(wtmp,     XmNfocusCallback,       textfield_focus_callback,   NULL);
	  XtAddCallback(wtmp,     XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
	  XtAddEventHandler(wtmp, EnterWindowMask, false, mouse_enter_text_callback,  NULL);
	  XtAddEventHandler(wtmp, LeaveWindowMask, false, mouse_leave_text_callback,  NULL);
	}

      ftmp = XtNameToWidget(w, "FilterText");
      if (!ftmp) ftmp = XmFileSelectionBoxGetChild(w, XmDIALOG_FILTER_TEXT);	
      if (ftmp)
	{
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
  our_dir = (char *)XmStringUnparse(data->dir, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

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

#ifdef SGI
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
		play_sound(dp->player, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
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
  Widget dialog_frame, dialog_info1, dialog_info2; /* labels giving info on selected file, or an error message */
  file_pattern_info *fp;
  dialog_play_info *dp;
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

static void post_file_info(file_dialog_info *fd, const char *filename)
{
  /* filename is known[strongly believed] to be a sound file, etc */
  XmString label;
  char *buf;
  char timestr[64];
  time_t date;
  XtManageChild(fd->dp->play_button);

  buf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s: %d chan%s, %d Hz, %.3f secs",
	       filename_without_home_directory(filename),
	       mus_sound_chans(filename),
	       (mus_sound_chans(filename) > 1) ? "s" : "",
	       mus_sound_srate(filename),
	       mus_sound_duration(filename));
  label = XmStringCreateLocalized(buf);
  XtVaSetValues(fd->dialog_info1, 
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
  XtVaSetValues(fd->dialog_info2, XmNlabelString, label, NULL);
  XmStringFree(label);
  FREE(buf);

  if (!(XtIsManaged(fd->dialog_info2))) 
    XtManageChild(fd->dialog_info2);
  if (!(XtIsManaged(fd->dialog_frame))) 
    XtManageChild(fd->dialog_frame);
}

static void unpost_file_info(file_dialog_info *fd)
{
  if (XtIsManaged(fd->dp->play_button)) 
    XtUnmanageChild(fd->dp->play_button);
  if (XtIsManaged(fd->dialog_frame)) 
    XtUnmanageChild(fd->dialog_frame);
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
  XmString s1, s2, ok_label;
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

  XtSetArg(args[n], XmNokLabelString, ok_label); n++;
  XtSetArg(args[n], XmNselectionLabelString, s1); n++;
  XtSetArg(args[n], XmNdialogTitle, s2); n++;
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

  rc1 = XtVaCreateManagedWidget("filebuttons-rc1", 
				xmRowColumnWidgetClass, fd->dialog,
				XmNorientation, XmVERTICAL,
				NULL);
  add_play_and_just_sounds_buttons(fd->dialog, rc1, fd->fp, fd->dp);

  fd->dialog_frame = XtVaCreateWidget("", xmFrameWidgetClass, rc1, NULL);
  rc2 = XtVaCreateManagedWidget("info-rc2", 
				xmRowColumnWidgetClass, fd->dialog_frame,
				XmNorientation, XmVERTICAL,
				XmNbackground, ss->sgx->highlight_color,
				NULL);
  fd->dialog_info1 = XtVaCreateManagedWidget("", xmLabelWidgetClass, rc2, XmNbackground, ss->sgx->highlight_color, NULL);
  fd->dialog_info2 = XtVaCreateManagedWidget("", xmLabelWidgetClass, rc2, XmNbackground, ss->sgx->highlight_color, NULL);
  color_file_selection_box(fd->dialog);

  wtmp = XtNameToWidget(fd->dialog, "Text");
  if (!wtmp) 
    wtmp = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_TEXT);
  if (wtmp) 
    {
      add_completer_to_textfield(wtmp, add_completer_func(filename_completer));
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
  XtVaSetValues(fd->dialog_info1, 
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);

  if (XtIsManaged(fd->dialog_info2))
    XtUnmanageChild(fd->dialog_info2);
  if (!(XtIsManaged(fd->dialog_frame))) 
    XtManageChild(fd->dialog_frame);
}

static void open_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  Widget dialog_filename_text;
  if (!(fd->fp->in_just_sounds_update)) /* auto trigger from just_sounds button -- unwanted! */
    {
      if (XtIsManaged(fd->dialog_frame))
	XtUnmanageChild(fd->dialog_frame);
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
	  /* the possible snd_error calls are so deeply buried here that it's easiest just to redirect the error handler to us */
	  redirect_snd_error_to(file_open_error, (void *)fd);
	  ss->sgx->requestor_dialog = w;
	  ss->open_requestor = FROM_OPEN_DIALOG;
	  sp = snd_open_file(filename, fd->file_dialog_read_only);
	  redirect_snd_error_to(NULL, NULL);
	  /* now snd_error is back to normal */
	  
	  if (sp) 
	    {
	      XtUnmanageChild(w);
	      select_channel(sp, 0);
	    }
	  else
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		clear_error_if_open_changes(fd->dialog, (void *)fd);
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
	  int err;
	  redirect_snd_error_to(file_open_error, (void *)fd);
	  ss->sgx->requestor_dialog = w;
	  ss->open_requestor = FROM_MIX_DIALOG;
	  err = mix_complete_file_at_cursor(any_selected_sound(), filename, with_mix_tags(ss), 0);
	  redirect_snd_error_to(NULL, NULL);
	  if (err == 0) 
	    XtUnmanageChild(w);
	  else
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		clear_error_if_open_changes(fd->dialog, (void *)fd);
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
  
static void mix_file_help_callback (Widget w, XtPointer context, XtPointer info) 
{
  mix_file_dialog_help();
}

static file_dialog_info *mdat = NULL;

widget_t make_mix_file_dialog(bool managed)
{
  /* called from the menu */
  if (mdat == NULL)
    {
      mdat = make_file_dialog(true, _("Mix"), _("mix in:"), file_mix_ok_callback, mix_file_help_callback);
      set_dialog_widget(FILE_MIX_DIALOG, mdat->dialog);
      if (just_sounds(ss)) 
	{
	  XtVaSetValues(mdat->dialog, 
			XmNfileSearchProc, sound_file_search, 
			NULL);
	  mdat->fp->need_update = true;
	  force_directory_reread(mdat->dialog);
	}
    }
  if ((managed) && (!XtIsManaged(mdat->dialog)))
    XtManageChild(mdat->dialog);
  return(mdat->dialog);
}


/* -------- reflect outside changes -------- */

void set_open_file_play_button(bool val)
{
  if ((odat) && (odat->dp->play_button))
    XmToggleButtonSetState(odat->dp->play_button, (Boolean)val, false);
  if ((mdat) && (mdat->dp->play_button))
    XmToggleButtonSetState(mdat->dp->play_button, (Boolean)val, false);
}

void alert_new_file(void) 
{
  if (odat)
    odat->fp->new_file_written = true;
  if (mdat)
    mdat->fp->new_file_written = true;
}

void reflect_just_sounds(void)
{
  if ((odat) && (odat->fp->just_sounds_button))
    XmToggleButtonSetState(odat->fp->just_sounds_button, just_sounds(ss), true);
  if ((mdat) && (mdat->fp->just_sounds_button))
    XmToggleButtonSetState(mdat->fp->just_sounds_button, just_sounds(ss), true);
}



/* ---------------- file data panel ---------------- */

#define NUM_VISIBLE_HEADERS 5

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples, int min_chan)
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
      if ((str) && (*str))
	{
	  fdat->scanf_widget = SRATE_WIDGET;
	  (*srate) = string_to_int_with_error(str, 1, "srate"); 
	  XtFree(str);
	}
    }
  if ((chans) && (fdat->chans_text))
    {
      str = XmTextGetString(fdat->chans_text); 
      if ((str) && (*str))
	{
	  fdat->scanf_widget = CHANS_WIDGET;
	  (*chans) = string_to_int_with_error(str, min_chan, "chans"); 
	  XtFree(str);
	}
    }
  if ((location) && (fdat->location_text))
    {
      str = XmTextGetString(fdat->location_text); 
      if ((str) && (*str))
	{
	  fdat->scanf_widget = DATA_LOCATION_WIDGET;
	  (*location) = string_to_off_t_with_error(str, 0, "data location"); 
	  XtFree(str);
	}
    }
  if ((samples) && (fdat->samples_text))
    {
      str = XmTextGetString(fdat->samples_text); 
      if ((str) && (*str))
	{
	  fdat->scanf_widget = SAMPLES_WIDGET;
	  (*samples) = string_to_off_t_with_error(str, 0, "samples"); 
	  XtFree(str);
	}
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

static void set_file_dialog_sound_attributes(file_data *fdat, int type, int format, int srate, int chans, off_t location, off_t samples, char *comment)
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

#define NUM_HEADER_TYPES 7
static char *header_short_names[NUM_HEADER_TYPES] = {"sun  ", "aifc ", "wave ", "raw  ", "aiff ", "ircam", "nist "};

file_data *make_file_data_panel(Widget parent, char *name, Arg *in_args, int in_n, 
				dialog_channels_t with_chan, 
				int header_type, int data_format,
				dialog_data_location_t with_loc, dialog_samples_t with_samples,
				dialog_error_t with_error, dialog_header_type_t with_header_type,
				dialog_comment_t with_comment)
{
  Widget form, header_label, data_label, srate_label, chans_label, sep1, sep2 = NULL, sep3, sep4;
  Widget comment_label = NULL, location_label, samples_label;
  Widget smenu, s8, s22, s44, s48, cmenu, c1 = NULL, c2 = NULL, c4 = NULL, c8 = NULL;
  file_data *fdat;
  Arg args[32];
  int i, n;
  XmString *strs;
  int dformats = 0;
  char **formats = NULL;

  fdat = (file_data *)CALLOC(1, sizeof(file_data));
  fdat->current_type = header_type;
  fdat->current_format = data_format;
  formats = set_header_positions_from_type(fdat, header_type, data_format);
  dformats = fdat->formats;

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
      strs = (XmString *)CALLOC(NUM_HEADER_TYPES, sizeof(XmString)); 
      for (i = 0; i < NUM_HEADER_TYPES; i++) 
	strs[i] = XmStringCreate(header_short_names[i], XmFONTLIST_DEFAULT_TAG);

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
      XtSetArg(args[n], XmNitemCount, NUM_HEADER_TYPES); n++;
      XtSetArg(args[n], XmNvisibleItemCount, NUM_VISIBLE_HEADERS); n++;
      fdat->header_list = XmCreateScrolledList(form, "header-type", args, n);
      XtManageChild(fdat->header_list);

      for (i = 0; i < NUM_HEADER_TYPES; i++) 
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

  strs = (XmString *)CALLOC(dformats, sizeof(XmString)); 
  for (i = 0; i < dformats; i++) 
    strs[i] = XmStringCreate(formats[i], XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(fdat->format_list, 
		XmNitems, strs, 
		XmNitemCount, dformats, 
		NULL);
  for (i = 0; i < dformats; i++) 
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
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->black); n++;
      XtSetArg(args[n], XmNborderWidth, 2); n++;
      XtSetArg(args[n], XmNmarginWidth, 10); n++;
      XtSetArg(args[n], XmNmarginHeight, 10); n++;
      fdat->error_text = XtCreateManagedWidget("", xmLabelWidgetClass, parent, args, n);
      XtUnmanageChild(fdat->error_text);
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

static void reflect_file_data_panel_change(file_data *fd, void (*change_action)(Widget w, XtPointer context, XtPointer info))
{
  if (fd->srate_text)
    XtAddCallback(fd->srate_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->chans_text)
    XtAddCallback(fd->chans_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->samples_text)
    XtAddCallback(fd->samples_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->location_text)
    XtAddCallback(fd->location_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->comment_text)
    XtAddCallback(fd->comment_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->format_list)
    XtAddCallback(fd->format_list, XmNbrowseSelectionCallback, change_action, (XtPointer)fd);
  if (fd->header_list)
    XtAddCallback(fd->header_list, XmNbrowseSelectionCallback, change_action, (XtPointer)fd);
}

static void unreflect_file_data_panel_change(file_data *fd, void (*change_action)(Widget w, XtPointer context, XtPointer info))
{
  if (fd->srate_text)
    XtRemoveCallback(fd->srate_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->chans_text)
    XtRemoveCallback(fd->chans_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->samples_text)
    XtRemoveCallback(fd->samples_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->location_text)
    XtRemoveCallback(fd->location_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->comment_text)
    XtRemoveCallback(fd->comment_text, XmNvalueChangedCallback, change_action, (XtPointer)fd);
  if (fd->format_list)
    XtRemoveCallback(fd->format_list, XmNbrowseSelectionCallback, change_action, (XtPointer)fd);
  if (fd->header_list)
    XtRemoveCallback(fd->header_list, XmNbrowseSelectionCallback, change_action, (XtPointer)fd);
}


/* -------- save as dialog (file and edit menus) -------- */
/* 
 * 19-June-97 simply save the current state under the new name and return
 *    to the current state/file (different from emacs) -- this keeps mix console intact
 *    across backups and so on, and seems more useful to me than switching to the new file.
 * 12-Nov-01 make this choice settable via emacs-style-save-as.
 * 6-Dec-04 need a hook here since gtk changes constantly
 * 20-Jun-05 improve error handling (redirect snd_error etc), add "Extract" option
 */


static file_data *sdat = NULL;
static Widget save_as_dialog = NULL, file_save_as_file_name;
static save_dialog_t save_as_dialog_type = FILE_SAVE_AS;
static file_pattern_info *save_as_fp;
static dialog_play_info *save_as_dp;

static void save_as_ok_callback(Widget w, XtPointer context, XtPointer info)
{ 
  char *str = NULL, *comment, *msg = NULL;
  snd_info *sp;
  int type, format, srate, chans;
  bool need_directory_update = false;
  off_t location, samples;

  clear_dialog_error(sdat);
  redirect_snd_error_to(post_file_panel_error, (void *)sdat);
  comment = get_file_dialog_sound_attributes(sdat, &srate, &chans, &type, &format, &location, &samples, 0);
  redirect_snd_error_to(NULL, NULL);
  if (sdat->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(save_as_dialog, (void *)sdat);
      if (comment) FREE(comment);
      return;
    }
  
  str = XmTextGetString(file_save_as_file_name);
  sp = any_selected_sound();
  clear_minibuffer(sp);
  if ((str) && (*str))
    {
      redirect_snd_error_to(post_file_dialog_error, (void *)sdat);
      msg = save_as_dialog_save_sound(sp, str, save_as_dialog_type, srate, type, format, comment, &need_directory_update);
      redirect_snd_error_to(NULL, NULL);
      if (msg)
	{
	  post_file_dialog_error((const char *)msg, (void *)sdat);
	  clear_error_if_filename_changes(save_as_dialog, (void *)sdat);
	  FREE(msg);
	}
      else
	{
	  if (need_directory_update) 
	    force_directory_reread(save_as_dialog);
	  XtUnmanageChild(save_as_dialog);
	  report_in_minibuffer(sp, "%s saved as %s", sp->short_filename, str);
	  if ((save_as_dialog_type == FILE_SAVE_AS) && 
	      (need_directory_update))
	    run_after_save_as_hook(sp, str, true); /* true => from dialog */
	}
      XtFree(str);
    }
  else 
    {
      msg = _("not saved (no file name given)");
      post_file_dialog_error((const char *)msg, (void *)sdat);
      clear_error_if_filename_changes(save_as_dialog, (void *)sdat);
    }
  if (comment) FREE(comment);
} 

static void save_as_extract_callback(Widget w, XtPointer context, XtPointer info) 
{
  char *str = NULL, *comment, *msg = NULL;
  snd_info *sp;
  int type, format, srate, chan = 0, err = 0;
  bool need_directory_update = false;
  off_t location, samples;
  clear_dialog_error(sdat);
  redirect_snd_error_to(post_file_panel_error, (void *)sdat);
  comment = get_file_dialog_sound_attributes(sdat, &srate, &chan, &type, &format, &location, &samples, 0);
  redirect_snd_error_to(NULL, NULL);
  if (sdat->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(save_as_dialog, (void *)sdat);
      if (comment) FREE(comment);
      return;
    }

  str = XmTextGetString(file_save_as_file_name);
  sp = any_selected_sound();
  clear_minibuffer(sp);
  if ((chan > sp->nchans) ||
      (((sp->nchans > 1) && (chan == sp->nchans)) ||
       (chan < 0)))
    {
      if (chan > sp->nchans)
	msg = mus_format("can't extract channel %d (sound has %d chan%s)", 
			 chan, sp->nchans, 
			 (sp->nchans > 1) ? "s" : "");
      else msg = mus_format("can't extract channel %d (first chan is numbered 0)", chan);
      post_file_dialog_error((const char *)msg, (void *)sdat);
      clear_error_if_chans_changes(save_as_dialog, (void *)sdat);
      FREE(msg);
    }
  else
    {
      if ((!str) || (!*str))
	{
	  msg = _("not saved (no file name given)");
	  post_file_dialog_error((const char *)msg, (void *)sdat);
	  clear_error_if_filename_changes(save_as_dialog, (void *)sdat);
	}
      else
	{
	  bool unmanage = true;
	  msg = NULL;
	  err = MUS_NO_ERROR;
	  if (sp->nchans == 1)
	    {
	      redirect_snd_error_to(post_file_dialog_error, (void *)sdat);
	      msg = save_as_dialog_save_sound(sp, str, save_as_dialog_type, srate, type, format, comment, &need_directory_update);
	      redirect_snd_error_to(NULL, NULL);
	    }
	  else 
	    {
	      char *fullname = NULL;
	      fullname = mus_expand_filename(str);
	      if (!(snd_overwrite_ok(fullname))) 
		{
		  FREE(fullname);
		  need_directory_update = false;
		  msg = mus_format(_("%s not overwritten"), str);
		  post_file_dialog_error((const char *)msg, (void *)sdat);
		  clear_error_if_filename_changes(save_as_dialog, (void *)sdat);
		  unmanage = false;
		}
	      else
		{
		  redirect_snd_error_to(post_file_dialog_error, (void *)sdat);
		  err = save_channel_edits(sp->chans[chan], str, AT_CURRENT_EDIT_POSITION);
		  redirect_snd_error_to(NULL, NULL);
		  /* returns MUS_NO_ERROR but nothing written if not overwrite_ok */
		  need_directory_update = true;
		}
	    }
	  if (unmanage)
	    {
	      if ((msg) || (err != MUS_NO_ERROR))
		{
		  post_file_dialog_error((const char *)msg, (void *)sdat);
		  clear_error_if_filename_changes(save_as_dialog, (void *)sdat);
		  if (msg) FREE(msg);
		}
	      else
		{
		  if (need_directory_update) force_directory_reread(save_as_dialog);
		  XtUnmanageChild(save_as_dialog);
		  report_in_minibuffer(sp, "%s channel %d saved as %s", sp->short_filename, chan, str);
		  if ((sp) && (str) && 
		      (save_as_dialog_type == FILE_SAVE_AS) && 
		      (need_directory_update))
		    run_after_save_as_hook(sp, str, true); /* true => from dialog */
		}
	    }
	  XtFree(str);
	}
    }
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
  XtUnmanageChild(save_as_dialog);
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
  if ((filename) && (*filename) && 
      (mus_file_probe(filename)) && 
      (!directory_p(filename)))
    s1 = XmStringCreate(_("save as (overwriting):"), XmFONTLIST_DEFAULT_TAG);
  else s1 = XmStringCreate(_("save as:"), XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(dialog, 
		XmNselectionLabelString, s1, 
		NULL);
  if (filename) XtFree(filename);
}

static void make_save_as_dialog(char *sound_name, int header_type, int format_type)
{
  char *file_string;
  if (!save_as_dialog)
    {
      Arg args[32];
      int n;
      XmString xmstr1, xmstr2, s1;
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

      save_as_fp = (file_pattern_info *)CALLOC(1, sizeof(file_pattern_info));
      save_as_fp->in_just_sounds_update = false;
      save_as_dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));

      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNchildPlacement, XmPLACE_ABOVE_SELECTION); n++;
      XtSetArg(args[n], XmNallowOverlap, false); n++;
      XtSetArg(args[n], XmNheight, 600); n++;
      XtSetArg(args[n], XmNuserData, (XtPointer)save_as_fp); n++;
      save_as_dialog = XmCreateFileSelectionDialog(MAIN_SHELL(ss), "save-as", args, n);
      FREE(file_string);

      XmStringFree(s1);
      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      file_save_as_file_name = XtNameToWidget(save_as_dialog, "Text");
      if (!(file_save_as_file_name)) file_save_as_file_name = XmFileSelectionBoxGetChild(save_as_dialog, XmDIALOG_TEXT);

      XtAddCallback(save_as_dialog, XmNhelpCallback, save_as_help_callback, NULL);
      XtAddCallback(save_as_dialog, XmNcancelCallback, save_as_cancel_callback, NULL);
      XtAddCallback(save_as_dialog, XmNokCallback, save_as_ok_callback, NULL);

      save_as_fp->dialog = save_as_dialog;
      save_as_dp->dialog = save_as_dialog;

      mainform = XtVaCreateManagedWidget("filebuttons-mainform", xmFormWidgetClass, save_as_dialog, NULL);
      add_play_and_just_sounds_buttons(save_as_dialog, mainform, save_as_fp, save_as_dp);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, save_as_fp->just_sounds_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sdat = make_file_data_panel(mainform, "data-form", args, n, 
				  WITH_EXTRACT_CHANNELS_FIELD, 
				  header_type, format_type, 
				  WITHOUT_DATA_LOCATION_FIELD, 
				  WITHOUT_SAMPLES_FIELD,
				  WITH_ERROR_FIELD, 
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD);
      sdat->dialog = save_as_dialog;

      color_file_selection_box(save_as_dialog);
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(sdat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(sdat->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(save_as_fp->just_sounds_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(save_as_dp->play_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
	}
      XtAddCallback(XmFileSelectionBoxGetChild(save_as_dialog, XmDIALOG_LIST),
		    XmNbrowseSelectionCallback, save_as_dialog_select_callback, (XtPointer)save_as_dp);
      XtAddCallback(file_save_as_file_name, XmNvalueChangedCallback, save_as_file_exists_check, (XtPointer)save_as_dialog);

      /* this must come after the file data panel so that Motif puts it in the button box, not the main work area */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
	  XtSetArg(args[n], XmNarmColor,   ss->sgx->pushed_button_color); n++;
	}
      extractB = XtCreateManagedWidget(_("Extract"), xmPushButtonGadgetClass, save_as_dialog, args, n);
      XtAddCallback(extractB, XmNactivateCallback, save_as_extract_callback, NULL);

      XtManageChild(save_as_dialog);
      set_dialog_widget(FILE_SAVE_AS_DIALOG, save_as_dialog);
    }
  else
    {
      XmString xmstr2;
      file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, _("save %s"), sound_name);
      xmstr2 = XmStringCreate(file_string, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(save_as_dialog, 
		    XmNdialogTitle, xmstr2, 
		    NULL);
      XmStringFree(xmstr2);
      FREE(file_string);
    }
}

widget_t make_file_save_as_dialog(bool managed)
{
  snd_info *sp = NULL;
  char *com = NULL;
  file_info *hdr = NULL;
  save_as_dialog_type = FILE_SAVE_AS;
  sp = any_selected_sound();
  if (sp) hdr = sp->hdr;
  make_save_as_dialog((char *)((sp) ? sp->short_filename : ""),
		      default_output_header_type(ss),
		      default_output_data_format(ss));
  set_file_dialog_sound_attributes(sdat,
				   sdat->current_type,
				   sdat->current_format,
				   (hdr) ? hdr->srate : selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES,
				   com = output_comment(hdr));
  if (com) FREE(com);
  if ((managed) && (!XtIsManaged(save_as_dialog))) XtManageChild(save_as_dialog);
  return(save_as_dialog);
}

widget_t make_edit_save_as_dialog(bool managed)
{
  save_as_dialog_type = EDIT_SAVE_AS;
  make_save_as_dialog(_("current selection"),
		      default_output_header_type(ss),
		      default_output_data_format(ss));
  set_file_dialog_sound_attributes(sdat,
				   sdat->current_type,
				   sdat->current_format,
				   selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   NULL);
  if ((managed) && (!XtIsManaged(save_as_dialog))) XtManageChild(save_as_dialog);
  return(save_as_dialog);
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
  if ((save_as_dialog) && (XtIsManaged(save_as_dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", (save_as_dialog_type == FILE_SAVE_AS) ? S_save_sound_dialog : S_save_selection_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME((save_as_dialog_type == FILE_SAVE_AS) ? S_save_sound_dialog : S_save_selection_dialog));
#endif
    }
}


/* -------------------------------- New File -------------------------------- */

static Widget new_file_dialog = NULL;
static file_data *ndat = NULL;
static off_t initial_samples = 1;
static Widget new_file_name = NULL;
static bool new_file_doit = false;
#if HAVE_FAM
  FAMRequest *new_file_doit_watcher = NULL;
#endif

static void new_file_undoit(void);

static void new_filename_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  if (new_file_doit)
    new_file_undoit();
  else
    {
      clear_dialog_error(fd);
      XtRemoveCallback(new_file_name, XmNmodifyVerifyCallback, new_filename_modify_callback, context);
    }
  cbs->doit = true;
}

static void new_file_undoit(void)
{
  XmString ok_label;
  new_file_doit = false;
  ok_label = XmStringCreate(_("Ok"), XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(new_file_dialog, 
		XmNokLabelString, ok_label, 
		NULL);
  XmStringFree(ok_label);
  clear_dialog_error(ndat);
  XtRemoveCallback(new_file_name, XmNmodifyVerifyCallback, new_filename_modify_callback, (XtPointer)ndat);
}

static void clear_error_if_new_filename_changes(Widget dialog, void *data)
{
  XtAddCallback(new_file_name, XmNmodifyVerifyCallback, new_filename_modify_callback, (XtPointer)data);
}

static void new_file_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  off_t loc;
  char *comment = NULL, *newer_name = NULL, *msg;
  int header_type, data_format, srate, chans;
  newer_name = XmTextGetString(new_file_name);
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
	  if ((!new_file_doit) &&
	      (ask_before_overwrite(ss)) && 
	      (mus_file_probe(newer_name)))
	    {
	      XmString ok_label;
	      msg = mus_format(_("%s exists. If you want to overwrite it, click 'DoIt'"), newer_name);
	      /* new_file_doit_watcher = fam_monitor_file(newer_name, NULL); */

	      post_file_dialog_error((const char *)msg, (void *)ndat);
	      clear_error_if_new_filename_changes(new_file_dialog, (void *)ndat);
	      ok_label = XmStringCreate(_("DoIt"), XmFONTLIST_DEFAULT_TAG);
	      XtVaSetValues(new_file_dialog, 
			    XmNokLabelString, ok_label, 
			    NULL);
	      XmStringFree(ok_label);
	      FREE(msg);
	      new_file_doit = true;
	    }
	  else
	    {
	      if (new_file_doit)
		new_file_undoit();
	      redirect_snd_error_to(post_file_dialog_error, (void *)ndat);
	      sp = snd_new_file(newer_name, header_type, data_format, srate, chans, comment, initial_samples);
	      redirect_snd_error_to(NULL, NULL);
	      if (!sp)
		{
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
  XmTextSetString(new_file_name, filename);  
  mus_sound_forget(filename);

  set_file_dialog_sound_attributes(ndat, header_type, data_format, srate, chans, IGNORE_DATA_LOCATION, initial_samples, new_comment);

  if (new_comment) FREE(new_comment);
  if (filename) FREE(filename);
}

static void new_file_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  char *current_name;
  current_name = XmTextGetString(new_file_name);
  load_new_file_defaults(current_name);
  if (current_name) XtFree(current_name);
  if (new_file_doit)
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
      new_file_name = make_textfield_widget("newtext", form, args, n, ACTIVATABLE, add_completer_func(filename_completer));

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, new_file_name); n++;
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
				  WITH_COMMENT_FIELD);
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
  else
    {
      /* if overwrite question pends, but file has been deleted in the meantime, go back to normal state */
      if (new_file_doit)
	{
	  char *new_name;
	  new_name = XmTextGetString(new_file_name);
	  if ((!new_name) || (!(*new_name)) ||
	      (!(mus_file_probe(new_name))))
	    new_file_undoit();
	  if (new_name) XtFree(new_name);
	}
    }
  if (!(XtIsManaged(new_file_dialog))) 
    XtManageChild(new_file_dialog);
}



/* ---------------- Edit Header ---------------- */

static Widget edit_header_dialog = NULL;
static file_data *edat;
static snd_info *edit_header_sp = NULL;

static void watch_read_only(struct snd_info *sp);

static XmString make_edit_header_dialog_title(snd_info *sp)
{
  char *str;
  XmString xstr;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (sp->read_only)
    {
      mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of (write-protected) %s"), sp->short_filename);
      sp->read_only_watcher = &watch_read_only;
    }
  else mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of %s"), sp->short_filename);
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
  set_sensitive(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_OK_BUTTON), true);
}

static void edit_header_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(edit_header_dialog);
  unreflect_file_data_panel_change(edat, edit_header_set_ok_sensitive);
  edit_header_sp->read_only_watcher = NULL;
}

#if DEBUGGING && HAVE_GUILE
static XEN g_apply_edit_header(void)
{
  if ((edit_header_sp) && (edit_header_sp->active))
    {
      if (!(edit_header_sp->read_only))
	edit_header_callback(edit_header_sp, edat, NULL, NULL);
      else snd_error(_("%s is write-protected"), edit_header_sp->short_filename);
    }
  XtUnmanageChild(edit_header_dialog);
  unreflect_file_data_panel_change(edat, edit_header_set_ok_sensitive);
  return(XEN_FALSE);
}
#endif

static void watch_read_only(struct snd_info *sp)
{
  if ((edit_header_dialog) && 
      (XtIsManaged(edit_header_dialog)) &&
      (!(sp->read_only)))
    {
      XmString title;
      clear_dialog_error(edat);
      title = make_edit_header_dialog_title(sp);
      XtVaSetValues(edit_header_dialog, 
		    XmNmessageString, title, 
		    NULL);
      sp->read_only_watcher = NULL;
    }
}

static void edit_header_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  if ((edit_header_sp) && (edit_header_sp->active))
    {
      if (XmGetFocusWidget(edit_header_dialog) == XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_OK_BUTTON))
	{
	  bool ok;
	  redirect_snd_error_to(post_file_dialog_error, (void *)edat);
	  ok = edit_header_callback(edit_header_sp, edat, post_file_dialog_error, post_file_panel_error);
	  redirect_snd_error_to(NULL, NULL);
	  if (edat->error_widget != NOT_A_SCANF_WIDGET)
	    {
	      clear_error_if_panel_changes(edit_header_dialog, (void *)edat);
	      return;
	    }
	  else
	    {
	      if (!ok)
		{
		  if (edit_header_sp->read_only)
		    edit_header_sp->read_only_watcher = &watch_read_only;
		  return;
		}
	    }
	}
    }
  XtUnmanageChild(edit_header_dialog);
  unreflect_file_data_panel_change(edat, edit_header_set_ok_sensitive);
}

Widget edit_header(snd_info *sp)
{
  file_info *hdr;
  XmString xstr4;
  Widget main_w;

  if (!sp) return(NULL);
  edit_header_sp = sp;
  hdr = sp->hdr;

  xstr4 = make_edit_header_dialog_title(sp);
  if (!edit_header_dialog)
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
      edit_header_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), "Edit Header", args, n);

      XtAddCallback(edit_header_dialog, XmNcancelCallback, edit_header_cancel_callback, NULL);
      XtAddCallback(edit_header_dialog, XmNhelpCallback,   edit_header_help_callback,   NULL);
      XtAddCallback(edit_header_dialog, XmNokCallback,     edit_header_ok_callback,     NULL);

      XmStringFree(xstr1);
      XmStringFree(xstr2);
      XmStringFree(xstr3);
      XmStringFree(titlestr);

      n = 0;
      main_w = XtCreateManagedWidget("eh-main", xmFormWidgetClass, edit_header_dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      edat = make_file_data_panel(main_w, "Edit Header", args, n, 
				  WITH_CHANNELS_FIELD, 
				  hdr->type, 
				  hdr->format, 
				  WITH_DATA_LOCATION_FIELD, 
				  WITH_SAMPLES_FIELD,
				  WITH_ERROR_FIELD, 
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD);
      edat->dialog = edit_header_dialog;

      set_file_dialog_sound_attributes(edat, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->samples, hdr->comment);
      XtManageChild(edat->error_text);
      XtManageChild(edit_header_dialog);

      if (!(ss->using_schemes)) 
	{
	  map_over_children(edit_header_dialog, set_main_color_of_widget, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
	  XtVaSetValues(edat->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(edat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	}
      set_dialog_widget(EDIT_HEADER_DIALOG, edit_header_dialog);
      XtUnmanageChild(edat->error_text);
    }
  else 
    {
      XtVaSetValues(edit_header_dialog, 
		    XmNmessageString, xstr4, 
		    NULL);
      set_file_dialog_sound_attributes(edat, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->samples, hdr->comment);
      raise_dialog(edit_header_dialog);
    }
  set_sensitive(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_OK_BUTTON), false); /* nothing needs to be saved when we start */
  XmStringFree(xstr4);
  if (hdr->type == MUS_RAW)
    post_file_dialog_error("this file has no header!", (void *)edat);
  else clear_dialog_error(edat);
  if (!(XtIsManaged(edit_header_dialog))) XtManageChild(edit_header_dialog);
  reflect_file_data_panel_change(edat, edit_header_set_ok_sensitive);
  return(edit_header_dialog);
}

void save_edit_header_dialog_state(FILE *fd)
{
  if ((edit_header_dialog) && 
      (XtIsManaged(edit_header_dialog)) && 
      (snd_ok(edit_header_sp)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s (%s \"%s\"))\n", S_edit_header_dialog, S_find_sound, edit_header_sp->short_filename);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(%s(\"%s\"))\n", TO_PROC_NAME(S_edit_header_dialog), TO_PROC_NAME(S_find_sound), edit_header_sp->short_filename);
#endif
    }
}



/* -------------------------------- Raw Data Dialog -------------------------------- */

static Widget raw_data_dialog = NULL;
static off_t raw_data_location = 0;
static file_data *rdat = NULL;
static bool raw_data_read_only = false, raw_data_sound_selected = false;
static char *raw_data_filename = NULL, *raw_data_info = NULL;

#if 0
typedef enum {NO_REQUESTOR, {FROM_UPDATE}, FROM_VIEW_PREVIOUS_FILES, {FROM_SAVE_AS_DIALOG}, [FROM_DRAG_AND_DROP], [FROM_OPEN_DIALOG],
	      {FROM_RECORDER}, [FROM_KEYBOARD], 

	      FROM_STARTUP -- how to get back to the startup args loop?

	      FROM_REGION_EDIT, FROM_SND_NEW_FILE, [FROM_OPEN_SOUND]
	      [FROM_OPEN_RAW_SOUND], [FROM_VIEW_SOUND], FROM_NEW_SOUND, {FROM_RAW_DATA_DIALOG}, [FROM_MIX_DIALOG]} open_requestor_t;
#endif

static void raw_data_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  int raw_srate, raw_chans, raw_data_format;
  redirect_snd_error_to(post_file_panel_error, (void *)rdat);
  get_file_dialog_sound_attributes(rdat, &raw_srate, &raw_chans, NULL, &raw_data_format, &raw_data_location, NULL, 1);
  redirect_snd_error_to(NULL, NULL);
  if (rdat->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(raw_data_dialog, (void *)rdat);
    }
  else
    {
      mus_header_set_raw_defaults(raw_srate, raw_chans, raw_data_format);
      mus_sound_override_header(raw_data_filename, raw_srate, raw_chans, 
				raw_data_format, MUS_RAW, raw_data_location,
				mus_bytes_to_samples(raw_data_format, 
						     mus_sound_length(raw_data_filename) - raw_data_location));
      /* choose action based on how we got here */
      if ((ss->sgx->requestor_dialog) &&
	  (ss->open_requestor == FROM_MIX_DIALOG))
	{
	  /* TODO: what is this about? ss->open_requestor = FROM_RAW_DATA_DIALOG;*/
	  ss->reloading_updated_file = true; /* don't reread lack-of-header! */
	  mix_complete_file_at_cursor(any_selected_sound(), raw_data_filename, with_mix_tags(ss), 0);
	  ss->reloading_updated_file = false;
	}
      else
	{
	  /* FROM_OPEN_DIALOG (has requestor_dialog)
	   * FROM_KEYBOARD (has requestor_sp)
	   * FROM_DRAG_AND_DROP (just open, no needed side effects)
	   */
	  file_info *hdr;
	  hdr = (file_info *)CALLOC(1, sizeof(file_info));
	  hdr->name = copy_string(raw_data_filename);
	  hdr->type = MUS_RAW;
	  hdr->srate = raw_srate;
	  hdr->chans = raw_chans;
	  hdr->format = raw_data_format;
	  hdr->samples = mus_bytes_to_samples(raw_data_format, 
					      mus_sound_length(raw_data_filename) - raw_data_location);
	  hdr->data_location = raw_data_location;
	  hdr->comment = NULL;
	  if (ss->open_requestor == FROM_KEYBOARD)
	    {
	      clear_minibuffer(ss->open_requestor_sp);
	      raw_data_sound_selected = true;
	    }
	  finish_opening_sound(add_sound_window(raw_data_filename, raw_data_read_only, hdr), raw_data_sound_selected);
	}
      XtUnmanageChild(raw_data_dialog);
    }
}

static void raw_data_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(raw_data_dialog);
  if ((ss->sgx->requestor_dialog) && 
      ((ss->open_requestor == FROM_OPEN_DIALOG) ||
       (ss->open_requestor == FROM_MIX_DIALOG)))
    XtManageChild(ss->sgx->requestor_dialog);
}

static void raw_data_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  int raw_srate, raw_chans, raw_data_format;
  raw_data_location = 0;
  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */  
  set_file_dialog_sound_attributes(rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, raw_data_location, 
				   IGNORE_SAMPLES, NULL);
  if (XtIsManaged(rdat->error_text))
    XtUnmanageChild(rdat->error_text);
}

static void raw_data_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_data_dialog_help(raw_data_info);
}

static Widget main_w;
static void make_raw_data_dialog(const char *filename, const char *title)
{
  XmString xstr1, xstr2, xstr3, xstr4, titlestr;
  int n;
  int raw_srate, raw_chans, raw_data_format;
  Arg args[20];
  Widget reset_button;

  xstr1 = XmStringCreate(_("Cancel"), XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
  xstr2 = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
  xstr3 = XmStringCreate(_("Ok"), XmFONTLIST_DEFAULT_TAG);
  titlestr = XmStringCreate(_("No Header on File"), XmFONTLIST_DEFAULT_TAG);
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
  raw_data_dialog = XmCreateWarningDialog(MAIN_SHELL(ss), "raw data", args, n);

  XtAddCallback(raw_data_dialog, XmNcancelCallback, raw_data_cancel_callback, NULL);
  XtAddCallback(raw_data_dialog, XmNhelpCallback,   raw_data_help_callback,   NULL);
  XtAddCallback(raw_data_dialog, XmNokCallback,     raw_data_ok_callback,     NULL);
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
  reset_button = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, raw_data_dialog, args, n);
  XtAddCallback(reset_button, XmNactivateCallback, raw_data_reset_callback, NULL);

  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */

  n = 0;
  XtSetArg(args[n], XmNallowResize, true); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_NONE); n++;
  main_w = XtCreateManagedWidget("raw-main", xmFormWidgetClass, raw_data_dialog, args, n);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  rdat = make_file_data_panel(main_w, "data-form", args, n, 
			      WITH_CHANNELS_FIELD, 
			      MUS_RAW, raw_data_format, 
			      WITH_DATA_LOCATION_FIELD, 
			      WITHOUT_SAMPLES_FIELD,
			      WITH_ERROR_FIELD, 
			      WITHOUT_HEADER_TYPE_FIELD, 
			      WITHOUT_COMMENT_FIELD);
  rdat->dialog = raw_data_dialog;

  set_file_dialog_sound_attributes(rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, raw_data_location, 
				   IGNORE_SAMPLES, NULL);

  map_over_children(raw_data_dialog, set_main_color_of_widget, NULL);
  if (!(ss->using_schemes)) 
    {
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
      XtVaSetValues(reset_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(rdat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
    }

  XtManageChild(rdat->error_text);
  XtManageChild(raw_data_dialog);
  XtUnmanageChild(rdat->error_text); 
  set_dialog_widget(RAW_DATA_DIALOG, raw_data_dialog);
}

void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, bool read_only, bool selected)
{
  /* put up dialog for srate, chans, data format */
  raw_data_read_only = read_only;
  raw_data_sound_selected = selected;
  if (raw_data_filename) FREE(raw_data_filename);
  raw_data_filename = copy_string(filename);
  if ((ss->sgx->requestor_dialog) &&
      ((ss->open_requestor == FROM_OPEN_DIALOG) ||
       (ss->open_requestor == FROM_MIX_DIALOG)))
    XtUnmanageChild(ss->sgx->requestor_dialog);
  if (!title) 
    title = mus_format("no header found on %s\n", filename);
  if (!raw_data_dialog) 
    make_raw_data_dialog(filename, title);
  else
    {
      XmString xstr4;
      xstr4 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(raw_data_dialog, 
		    XmNmessageString, xstr4, 
		    NULL);
      XmStringFree(xstr4);
    }
  FREE(title);
  if (raw_data_info) FREE(raw_data_info);
  if (info)
    {
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_HELP_BUTTON), 
		    XmNbackground, ss->sgx->green, 
		    NULL);
      raw_data_info = copy_string(info);
      FREE(info);
    }
  else
    {
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_HELP_BUTTON), 
		    XmNbackground, ss->sgx->help_button_color, 
		    NULL);
      raw_data_info = NULL;
    }
  raise_dialog(raw_data_dialog);
  if (!XtIsManaged(raw_data_dialog)) 
    XtManageChild(raw_data_dialog);
}



/* -------- mouse-enter|leave-label hooks (used in View:Files) -------- */
static XEN mouse_enter_label_hook;
static XEN mouse_leave_label_hook;

static void mouse_leave_label_or_enter(regrow *r, XEN hook, const char *caller)
{
  if ((r) &&
      (XEN_HOOKED(hook)))
    {
      char *label = NULL;
      bool need_free = false;
      if (r->parent == CURRENT_FILE_VIEWER)
	label = get_curfullname(r->pos);
      else
	{
	  if (r->parent == PREVIOUS_FILE_VIEWER)
	    label = get_prevfullname(r->pos);
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

static void mouse_enter_label(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  mouse_leave_label_or_enter((regrow *)context, mouse_enter_label_hook, S_mouse_enter_label_hook);
}

static void mouse_leave_label(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  mouse_leave_label_or_enter((regrow *)context, mouse_leave_label_hook, S_mouse_leave_label_hook);
}


/* -------- files browser and regions list widgetry -------- */
/*
 * the region and file browsers share much widgetry -- they are supposed to look the same
 */

ww_info *make_title_row(Widget formw, char *top_str, char *main_str, dialog_pad_t pad, dialog_sort_t with_sort, dialog_paned_t with_pane)
{
  int n;
  Arg args[32];
  Widget plw, rlw, sep1 = NULL;
  Widget smenu, sbar;
  ww_info *wwi;
  wwi = (ww_info *)CALLOC(1, sizeof(ww_info));
  
  if (main_str)
    {
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      rlw = XtCreateManagedWidget(main_str, xmLabelWidgetClass, formw, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, rlw); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmDOUBLE_LINE); n++;
      sep1 = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, formw, args, n);
    }

  if (with_pane == WITH_PANED_WINDOW)
    {
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      if (main_str)
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, sep1); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	}
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      wwi->panes = XtCreateManagedWidget("panes", xmPanedWindowWidgetClass, formw, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      n = attach_all_sides(args, n);
      XtSetArg(args[n], XmNpaneMinimum, 40); n++;
      wwi->toppane = XtCreateManagedWidget("toppane", xmFormWidgetClass, wwi->panes, args, n);
      formw = wwi->toppane;
    }
  else 
    {
      wwi->panes = formw;
      wwi->toppane = formw;
    }

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  if (pad == PAD_TITLE_ON_LEFT)
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 5); n++;
    }
  else
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    }
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  if (with_pane == WITH_PANED_WINDOW)
    {
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
    }
  else
    {
      if (main_str)
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, sep1); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	}
    }
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  plw = XtCreateManagedWidget(top_str, xmLabelWidgetClass, formw, args, n);
  wwi->plw = plw;

  if (with_sort == WITH_SORT_BUTTON)
    {
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      if (main_str)
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, sep1); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	}
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      sbar = XmCreateMenuBar(formw, "menuBar", args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      smenu = XmCreatePulldownMenu(sbar, _("sort"), args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNsubMenuId, smenu); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      XtCreateManagedWidget("sort", xmCascadeButtonWidgetClass, sbar, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      wwi->byname =  XtCreateManagedWidget(_("name"),  xmPushButtonWidgetClass, smenu, args, n);
      wwi->bydate =  XtCreateManagedWidget(_("date"),  xmPushButtonWidgetClass, smenu, args, n);
      wwi->bysize =  XtCreateManagedWidget(_("size"),  xmPushButtonWidgetClass, smenu, args, n);
      wwi->byentry = XtCreateManagedWidget(_("entry"), xmPushButtonWidgetClass, smenu, args, n);
      wwi->byproc =  XtCreateManagedWidget(_("proc"),  xmPushButtonWidgetClass, smenu, args, n);
      XtSetSensitive(wwi->byproc, XEN_PROCEDURE_P(ss->file_sort_proc));

      XtManageChild(sbar);
    }
  
  n = 0;
  if (pad == PAD_TITLE_ON_LEFT) 
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 5); n++;
    }
  else
    {
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    }
  if (pad == PAD_TITLE_ON_RIGHT)
    {
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 95); n++;
    }
  else
    {
      if (pad == PAD_TITLE_ON_LEFT)
	{
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
	  XtSetArg(args[n], XmNrightPosition, 70); n++;
	}
    }
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, plw); n++;
  if (pad == DONT_PAD_TITLE)
    {
      if (with_pane == WITH_PANED_WINDOW)
	{
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
	  XtSetArg(args[n], XmNbottomPosition, 40); n++;
	}
    }
  else
    {
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    }
  XtSetArg(args[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
  XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
  wwi->list = XmCreateScrolledWindow(formw, "reglist", args, n);

  n = attach_all_sides(args, 0);
  wwi->ww = XtCreateManagedWidget("ww", xmFormWidgetClass, wwi->list, args, n);
  XtVaSetValues(wwi->list, 
		XmNworkWindow, wwi->ww, 
		NULL);
  
  return(wwi);
}

regrow *make_regrow(Widget ww, Widget last_row, XtCallbackProc play_callback, XtCallbackProc name_callback)
{
  int n;
  Arg args[32];
  regrow *r;
  XmString s1;
  XtCallbackList n1, n3;

  s1 = XmStringCreate("", XmFONTLIST_DEFAULT_TAG);
  r = (regrow *)CALLOC(1, sizeof(regrow));

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;}
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, (last_row) ? XmATTACH_WIDGET : XmATTACH_FORM); n++;
  if (last_row) {XtSetArg(args[n], XmNtopWidget, last_row); n++;}
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNheight, 18); n++; 
  r->rw = XtCreateWidget("rw", xmFormWidgetClass, ww, args, n);

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



/* -------- view files dialog -------- */

static Widget view_files_dialog = NULL;
static int vf_selected_file = -1;
static Widget vf_curww, vf_prevlst, vf_curlst, vf_prevww;

static regrow **cur_name_row = NULL;
static regrow **prev_name_row = NULL;

void make_cur_name_row(int old_size, int new_size)
{
  if (cur_name_row == NULL)
    cur_name_row = (regrow **)CALLOC(new_size, sizeof(regrow *));
  else 
    {
      int i;
      cur_name_row = (regrow **)REALLOC(cur_name_row, new_size * sizeof(regrow *));
      for (i = old_size; i < new_size; i++) cur_name_row[i] = NULL;
    }
}

void make_prev_name_row(int old_size, int new_size)
{
  if (prev_name_row == NULL)
    prev_name_row = (regrow **)CALLOC(new_size, sizeof(regrow *));
  else 
    {
      int i;
      prev_name_row = (regrow **)REALLOC(prev_name_row, new_size * sizeof(regrow *));
      for (i = old_size; i < new_size; i++) prev_name_row[i] = NULL;
    }
}

static void view_files_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_dialog_help();
}

static void view_files_dismiss_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(view_files_dialog);
}

static void view_files_clear_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* clear previous files list and associated widget list */
  clear_prevlist();
}

static void view_files_update_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* run through previous files list looking for any that have been deleted behind our back */
  update_prevlist();
  if (view_files_dialog_is_active()) make_prevfiles_list();
}

void set_file_browser_play_button(char *name, int state)
{
  if (view_files_dialog_is_active())
    {
      int i, list = 0;
      i = find_curfile_regrow(name); 
      if (i != -1) list = 1; else i = find_prevfile_regrow(name);
      if (i != -1)
	{
	  regrow *r;
	  if (list) r = cur_name_row[i]; else r = prev_name_row[i];
	  XmToggleButtonSetState(r->pl, (Boolean)state, false);
	}
    }
}

static void view_curfiles_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  regrow *r = (regrow *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  view_curfiles_play(r->pos, cb->set);
}

static void curfile_unhighlight(void)
{
  if ((view_files_dialog_is_active()) && 
      (!(ss->using_schemes)) && 
      (vf_selected_file != -1))
    {
      regrow *r;
      r = cur_name_row[vf_selected_file];
      XtVaSetValues(r->rw, XmNbackground, ss->sgx->highlight_color, NULL);
      XtVaSetValues(r->nm, XmNbackground, ss->sgx->highlight_color, NULL);
      vf_selected_file = -1;
    }
}

void curfile_highlight(int i)
{
  if ((view_files_dialog_is_active()) && 
      (!(ss->using_schemes)))
    {
      regrow *r;
      if (vf_selected_file != -1) curfile_unhighlight();
      r = cur_name_row[i];
      XtVaSetValues(r->rw, XmNbackground, ss->sgx->zoom_color, NULL);
      XtVaSetValues(r->nm, XmNbackground, ss->sgx->zoom_color, NULL);
      vf_selected_file = i;
    }
}

static void view_curfiles_select_callback(Widget w, XtPointer context, XtPointer info) 
{
  regrow *r = (regrow *)context;
  view_curfiles_select(r->pos);
}

static void view_prevfiles_play_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* open and play -- close at end or when button off toggled */
  regrow *r = (regrow *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  if (view_prevfiles_play(r->pos, cb->set))
    XmToggleButtonSetState(w, false, false);
}

static void view_prevfiles_select_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* open and set as selected */
  regrow *r = (regrow *)context;
  view_prevfiles_select(r->pos);
}

void highlight_selected_sound(void)
{
  snd_info *sp;
  sp = selected_sound();
  if (sp)
    {
      int i;
      i = find_curfile_regrow(sp->short_filename);
      if (i != -1) 
	curfile_highlight(i); 
      else curfile_unhighlight();
    }
  else curfile_unhighlight();
}

void make_curfiles_list (void)
{
  int i, lim;
  Widget last_row = NULL;
  regrow *r;
  lim = get_curfile_end();
  for (i = 0; i < lim; i++)
    {
      r = cur_name_row[i];
      if (r == NULL)
	{
	  r = make_regrow(vf_curww, last_row, view_curfiles_play_callback, view_curfiles_select_callback);
	  cur_name_row[i] = r;
	  r->pos = i;
	  r->parent = CURRENT_FILE_VIEWER;
	}
      set_button_label(r->nm, view_curfiles_name(r->pos));
      XmToggleButtonSetState(r->pl, false, false);
      if (!(XtIsManaged(r->rw))) XtManageChild(r->rw);
      last_row = r->rw;
    }
  lim = get_max_curfile_end();
  for (i = get_curfile_end(); i < lim; i++)
    if ((r = cur_name_row[i]))
      if (XtIsManaged(r->rw)) 
	XtUnmanageChild(r->rw);
  set_max_curfile_end(get_curfile_end());
  highlight_selected_sound();
  XtManageChild(vf_curlst);
}

static void sort_prevfiles_by_name(Widget w, XtPointer context, XtPointer info) 
{
  set_previous_files_sort(1);
  make_prevfiles_list();
}

static void sort_prevfiles_by_date(Widget w, XtPointer context, XtPointer info) 
{
  set_previous_files_sort(2);
  make_prevfiles_list();
}

static void sort_prevfiles_by_size(Widget w, XtPointer context, XtPointer info) 
{
  set_previous_files_sort(3);
  make_prevfiles_list();
}

static void sort_prevfiles_by_entry_order(Widget w, XtPointer context, XtPointer info) 
{
  set_previous_files_sort(4);
  make_prevfiles_list();
}

static void sort_prevfiles_by_user_procedure(Widget w, XtPointer context, XtPointer info) 
{
  set_previous_files_sort(5);
  make_prevfiles_list();
}

void make_prevfiles_list (void)
{
  int i, lim;
  Widget last_row = NULL;
  regrow *r;
  if (get_prevfile_end() >= 0)
    {
      make_prevfiles_list_1();
      lim = get_prevfile_end();
      for (i = 0; i <= lim; i++)
	{
	  if (!((r = prev_name_row[i])))
	    {
	      r = make_regrow(vf_prevww, last_row, view_prevfiles_play_callback, view_prevfiles_select_callback);
	      prev_name_row[i] = r;
	      r->pos = i;
	      r->parent = PREVIOUS_FILE_VIEWER;
	    }
	  set_button_label(r->nm, get_prevname(r->pos));
	  XmToggleButtonSetState(r->pl, false, false);
	  if (!(XtIsManaged(r->rw))) XtManageChild(r->rw);
	  last_row = r->rw;
	}
    }
  lim = get_max_prevfile_end();
  for (i = get_prevfile_end() + 1; i <= lim; i++)
    if ((r = prev_name_row[i]))
      if (XtIsManaged(r->rw)) 
	XtUnmanageChild(r->rw);
  set_max_prevfile_end(get_prevfile_end());
  if (!(XtIsManaged(vf_prevlst))) 
    XtManageChild(vf_prevlst);
}

static Widget byproc = NULL;
void set_file_sort_sensitive(bool sensitive)
{
  if (byproc)
    XtSetSensitive(byproc, sensitive);
}

/* play open for prevfile, play save select for curfile, preload process for prevfile (snd-clm) */

static void start_view_files_dialog(bool managed)
{
  /* fire up a dialog window with a list of currently open files, 
   * currently selected file also selected in list --
   * if user selects one (browse mode), so does Snd (via equalize_sound_panes etc)
   * use snd_info label as is (short-form with '*' etc)
   * secondary list of previously edited files (if still in existence) --
   * click here re-opens the file.  (The overall form is similar to the regions browser).
   * The previous files list requires that we keep such a list as we go along, on the
   * off-chance this browser will be fired up.  (Such files may be subsequently moved or deleted).
   */
  bool new_dialog = false;
  if (!view_files_dialog)
    {
      int n;
      Arg args[20];
      ww_info *wwl;
      regrow *r;
      XmString xdismiss, xhelp, xclear, titlestr;
      Widget mainform, curform, prevform, updateB, sep;

      new_dialog = true;
      vf_selected_file = -1;
      xdismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      xclear = XmStringCreate(_("Clear"), XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(_("Files"), XmFONTLIST_DEFAULT_TAG);
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNcancelLabelString, xclear); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      view_files_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), "File Browser", args, n);

      XtAddCallback(view_files_dialog, XmNcancelCallback, view_files_clear_callback,   NULL);
      XtAddCallback(view_files_dialog, XmNhelpCallback,   view_files_help_callback,    NULL);
      XtAddCallback(view_files_dialog, XmNokCallback,     view_files_dismiss_callback, NULL);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xclear);
      XmStringFree(titlestr);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->quit_button_color,   NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->reset_button_color,  NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
	}

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->doit_button_color); n++;
	  XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
	}
      updateB = XtCreateManagedWidget(_("Update"), xmPushButtonGadgetClass, view_files_dialog, args, n);
      /* need Gadget if we want a subsequent XmNbackgroundPixmap change to be reflected in the button */
      XtAddCallback(updateB, XmNactivateCallback, view_files_update_callback, NULL);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(view_files_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, view_files_dialog, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 49); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      curform = XtCreateManagedWidget("curform", xmFormWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, curform); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
      sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      prevform = XtCreateManagedWidget("prevform", xmFormWidgetClass, mainform, args, n);

      /* current files section: save play current files | files */
      wwl = make_title_row(curform, _("play"), _("current files"),
			   PAD_TITLE_ON_RIGHT, WITHOUT_SORT_BUTTON, WITHOUT_PANED_WINDOW);
      vf_curww = wwl->ww;
      vf_curlst = wwl->list;
      if (!(ss->using_schemes)) 
	map_over_children(vf_curlst, set_main_color_of_widget, NULL);
      FREE(wwl); 
      wwl = NULL;
      if (get_curfile_size() == 0) /* apparently we need at least one row to get Motif to allocate the outer widgets correctly */
	{                    /* not curfile_end here since it is tracking currently active files before this dialog is created */
	  init_curfiles(4);
	  cur_name_row = (regrow **)CALLOC(4, sizeof(regrow *));
	  r = make_regrow(vf_curww, NULL, view_curfiles_play_callback, view_curfiles_select_callback);
	  cur_name_row[0] = r;
	  r->pos = 0;
	  r->parent = CURRENT_FILE_VIEWER;
	}

      /* previous files section: play previous files | files */
      wwl = make_title_row(prevform, _("play"), _("previous files"), PAD_TITLE_ON_LEFT, WITH_SORT_BUTTON, WITHOUT_PANED_WINDOW);

      XtAddCallback(wwl->byname,  XmNactivateCallback, sort_prevfiles_by_name,           NULL);
      XtAddCallback(wwl->bydate,  XmNactivateCallback, sort_prevfiles_by_date,           NULL);
      XtAddCallback(wwl->bysize,  XmNactivateCallback, sort_prevfiles_by_size,           NULL);
      XtAddCallback(wwl->byentry, XmNactivateCallback, sort_prevfiles_by_entry_order,    NULL);
      XtAddCallback(wwl->byproc,  XmNactivateCallback, sort_prevfiles_by_user_procedure, NULL);

      byproc = wwl->byproc;
      vf_prevww = wwl->ww;
      vf_prevlst = wwl->list;
      if (!(ss->using_schemes)) 
	map_over_children(vf_prevlst, set_main_color_of_widget, NULL);
      FREE(wwl); 
      wwl = NULL;
      if (get_prevfile_size() == 0)
	{
	  init_prevfiles(4);
	  prev_name_row = (regrow **)CALLOC(4, sizeof(regrow *));
	  r = make_regrow(vf_prevww, NULL, view_prevfiles_play_callback, view_prevfiles_select_callback);
	  prev_name_row[0] = r;
	  r->pos = 0;
	  r->parent = PREVIOUS_FILE_VIEWER;
	}
      set_dialog_widget(VIEW_FILES_DIALOG, view_files_dialog);
    }
  make_curfiles_list();
  make_prevfiles_list();
  if (managed)
    {
      if (new_dialog)
	{
	  XtManageChild(view_files_dialog);
	}
      else 
	{
	  if (!XtIsManaged(view_files_dialog)) 
	    XtManageChild(view_files_dialog);
	  raise_dialog(view_files_dialog);
	}
    }
  highlight_selected_sound();
}

void view_files_callback(Widget w, XtPointer context, XtPointer info)
{
  start_view_files_dialog(true);
}

Widget start_file_dialog(bool managed)
{
  start_view_files_dialog(managed);
  return(view_files_dialog);
}

bool view_files_dialog_is_active(void)
{
  return((view_files_dialog) && (XtIsManaged(view_files_dialog)));
}



#if DEBUGGING && HAVE_GUILE
static XEN g_new_file_dialog(void)
{
  make_new_file_dialog();
  return(XEN_FALSE);
}
#endif

void g_init_gxfile(void)
{
#if HAVE_SCHEME
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 0 for the current files list, 1 for previous files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.scm \
to popup file info as follows: \n\
(add-hook! " S_mouse_enter_label_hook "\n\
  (lambda (type position name)\n\
    (if (not (= type 2))\n\
        (" S_info_dialog " name (finfo name)))))\n\
See also nb.scm."
#else
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 0 for the current files list, 1 for previous files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'."
#endif

  #define H_mouse_leave_label_hook S_mouse_leave_label_hook " (type position label): called when the mouse leaves a file viewer or region label"

  mouse_enter_label_hook = XEN_DEFINE_HOOK(S_mouse_enter_label_hook, 3, H_mouse_enter_label_hook);
  mouse_leave_label_hook = XEN_DEFINE_HOOK(S_mouse_leave_label_hook, 3, H_mouse_leave_label_hook);

#if DEBUGGING && HAVE_GUILE
  XEN_DEFINE_PROCEDURE("new-file-dialog", g_new_file_dialog, 0, 0, 0, "internal testing function");
  XEN_DEFINE_PROCEDURE("apply-edit-header", g_apply_edit_header, 0, 0, 0, "internal testing function");
#endif
}

