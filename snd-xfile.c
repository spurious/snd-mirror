#include "snd.h"

/* TODO: the view:files menu might be better as a container widget.  Not sure the "current files" list is useful.
 */

#define NUM_VISIBLE_HEADERS 4

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples)
{
  char *str;
  int n;
  int res, val;
  off_t oval;
  int *ns = NULL;
  char *comment = NULL;
  if (fdat->srate_text) 
    {
      str = XmTextGetString(fdat->srate_text); 
      if (str) 
	{
	  val = string_to_int(str); 
	  if (val > 0) (*srate) = val;
	  XtFree(str);
	}
    }
  if (fdat->chans_text) 
    {
      str = XmTextGetString(fdat->chans_text); 
      if (str) 
	{
	  val = string_to_int(str); 
	  if (val > 0) (*chans) = val;
	  XtFree(str);
	}
    }
  if (fdat->location_text) 
    {
      str = XmTextGetString(fdat->location_text); 
      if (str) 
	{
	  oval = string_to_off_t(str); 
	  if (oval >= 0) (*location) = oval;
	  XtFree(str);
	}
    }
  if (fdat->samples_text) 
    {
      str = XmTextGetString(fdat->samples_text); 
      if (str) 
	{
	  oval = string_to_off_t(str); 
	  if (oval >= 0) (*samples) = oval;
	  XtFree(str);
	}
    }
  if (fdat->comment_text) 
    {
      comment = XmTextGetString(fdat->comment_text);
    }
  if (fdat->header_list)
    {
      res = XmListGetSelectedPos(fdat->header_list, &ns, &n);
      if (res)
	{
	  (*type) = header_type_from_position(ns[0] - 1);
	  fdat->current_type = (*type);
	  free(ns); ns = NULL;
	}
    }
  if (fdat->format_list)
    {
      res = XmListGetSelectedPos(fdat->format_list, &ns, &n);
      if (res)
	{
	  (*format) = data_format_from_position(fdat->current_type, ns[0] - 1);
	  fdat->current_format = (*format);
	  free(ns); ns = NULL;
	}
    }
  if (comment)
    {
      str = copy_string(comment);
      XtFree(comment);
      return(str);
    }
  return(NULL);
}

static void load_header_and_data_lists(file_data *fdat, int type, int format, int srate, int chans, off_t location, off_t samples, char *comment)
{
  int i;
  char **fl = NULL;
  XmString *strs;
  fdat->current_type = type;
  fdat->current_format = format;
  fl = set_header_and_data_positions(fdat, type, format); 
  if (fl == NULL) return;
  XmListSelectPos(fdat->header_list, fdat->header_pos + 1, false);
  strs = (XmString *)MALLOC(fdat->formats * sizeof(XmString)); 
  for (i = 0; i < fdat->formats; i++) 
    strs[i] = XmStringCreate(fl[i], XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(fdat->format_list, 
		XmNitems, strs, 
		XmNitemCount, fdat->formats, 
		NULL);
  for (i = 0; i < fdat->formats; i++)
    XmStringFree(strs[i]);
  FREE(strs); 
  XmListSelectPos(fdat->format_list, fdat->format_pos + 1, false);
  if ((srate > 0) && 
      (fdat->srate_text))
    widget_int_to_text(fdat->srate_text, srate);
  if ((chans > 0) && 
      (fdat->chans_text))
    widget_int_to_text(fdat->chans_text, chans);
  if (fdat->comment_text) 
    XmTextSetString(fdat->comment_text, comment);
  if ((location >= 0) && 
      (fdat->location_text))
    widget_off_t_to_text(fdat->location_text, location);
  if ((samples >= 0) && 
      (fdat->samples_text))
    widget_off_t_to_text(fdat->samples_text, samples);
}

static void color_file_selection_box(Widget w)
{
  if (!(ss->using_schemes)) 	
    {
      Widget wtmp = NULL, ftmp = NULL, ltmp = NULL;
      map_over_children(w, set_main_color_of_widget, NULL);
      XtVaSetValues(XmFileSelectionBoxGetChild(w, XmDIALOG_DIR_LIST), 
		    XmNbackground, (ss->sgx)->white, 
		    XmNforeground, (ss->sgx)->black, 
		    NULL);
      XtVaSetValues(XmFileSelectionBoxGetChild(w, XmDIALOG_LIST), 
		    XmNbackground, (ss->sgx)->white, 
		    XmNforeground, (ss->sgx)->black, 
		    NULL);

      XtVaSetValues(XtNameToWidget(w, "Apply"), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "Cancel"), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "Help"), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "OK"), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "Apply"), XmNbackground, (ss->sgx)->reset_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "Cancel"), XmNbackground, (ss->sgx)->quit_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "Help"), XmNbackground, (ss->sgx)->help_button_color, NULL);
      XtVaSetValues(XtNameToWidget(w, "OK"), XmNbackground, (ss->sgx)->doit_button_color, NULL);

      ltmp = XtNameToWidget(w, "FilterLabel");
      if (!ltmp) ltmp = XmFileSelectionBoxGetChild(w, XmDIALOG_FILTER_LABEL);
      XtVaSetValues(ltmp, XmNbackground, (ss->sgx)->reset_button_color, NULL);

      ltmp = XtNameToWidget(w, "Selection");
      if (!ltmp) ltmp = XmFileSelectionBoxGetChild(w, XmDIALOG_SELECTION_LABEL);
      XtVaSetValues(ltmp, XmNbackground, (ss->sgx)->help_button_color, NULL);

      ltmp = XtNameToWidget(w, "Dir");
      if (!ltmp) ltmp = XmFileSelectionBoxGetChild(w, XmDIALOG_DIR_LIST_LABEL);
      XtVaSetValues(ltmp, XmNbackground, (ss->sgx)->doit_button_color, NULL);

      ltmp = XtNameToWidget(w, "Items");
      if (!ltmp) ltmp = XmFileSelectionBoxGetChild(w, XmDIALOG_LIST_LABEL);
      XtVaSetValues(ltmp, XmNbackground, (ss->sgx)->quit_button_color, NULL);

      wtmp = XtNameToWidget(w, "Text");
      if (!wtmp) wtmp = XmFileSelectionBoxGetChild(w, XmDIALOG_TEXT);
      if (wtmp)
	{
	  XtAddCallback(wtmp, XmNfocusCallback, textfield_focus_callback, NULL);
	  XtAddCallback(wtmp, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
	  XtAddEventHandler(wtmp, EnterWindowMask, false, mouse_enter_text_callback, NULL);
	  XtAddEventHandler(wtmp, LeaveWindowMask, false, mouse_leave_text_callback, NULL);
	}

      ftmp = XtNameToWidget(w, "FilterText");
      if (!ftmp) ftmp = XmFileSelectionBoxGetChild(w, XmDIALOG_FILTER_TEXT);	
      if (ftmp)
	{
	  XtAddCallback(ftmp, XmNfocusCallback, textfield_focus_callback, NULL);
	  XtAddCallback(ftmp, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
	  XtAddEventHandler(ftmp, EnterWindowMask, false, mouse_enter_text_callback, NULL);
	  XtAddEventHandler(ftmp, LeaveWindowMask, false, mouse_leave_text_callback, NULL);
	}
    }
}


/* -------- File Open/Mix Dialogs -------- */

typedef struct {
  bool file_dialog_read_only, need_update, new_file_written;
  Widget dialog, play_selected_button, just_sounds_button, dialog_frame, dialog_info1, dialog_info2;
  XmSearchProc default_search_proc;
  char *save_dir,*last_dir;
  dir *sound_files, *current_files;
  char *last_pattern, *fullpathname;
  snd_info *file_play_sp;
} file_dialog_info;

static void open_file_help_callback (Widget w, XtPointer context, XtPointer info) 
{
  open_file_dialog_help();
}

static void mix_file_help_callback (Widget w, XtPointer context, XtPointer info) 
{
  mix_file_dialog_help();
}

static void file_dialog_stop_playing(file_dialog_info *fd)
{
  if ((fd->file_play_sp) && 
      (fd->file_play_sp->playing)) 
    {
      stop_playing_sound(fd->file_play_sp, PLAY_BUTTON_UNSET);
      fd->file_play_sp = NULL;
    }
}

static void file_cancel_callback (Widget w, XtPointer context, XtPointer info) 
{
  file_dialog_stop_playing((file_dialog_info *)context);
  XtUnmanageChild (w);
}

static int string_compare(const void *ss1, const void *ss2)
{
  return(strcmp((*((char **)ss1)), (*((char **)ss2))));
}

void clear_deleted_snd_info(void *data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  fd->file_play_sp = NULL;
}

static void play_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  file_dialog_info *fd = (file_dialog_info *)context;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  if (cb->set)
    {
      Widget wtmp;
      char *filename;
      if ((fd->file_play_sp) && (fd->file_play_sp->playing)) 
	stop_playing_sound(fd->file_play_sp, PLAY_BUTTON_UNSET);
      wtmp = XtNameToWidget(fd->dialog, "Text");
      if (!wtmp) 
	wtmp = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_TEXT);
      filename = XmTextGetString(wtmp);
      if (filename)
	{
	  if (mus_file_probe(filename))
	    {
	      fd->file_play_sp = make_sound_readable(filename, false);
	      fd->file_play_sp->delete_me = (void *)fd;
	      if (fd->file_play_sp)
		play_sound(fd->file_play_sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
	    }
	  XtFree(filename);
	}
    }
  else file_dialog_stop_playing(fd);
}

static file_dialog_info *open_dialog = NULL;
static file_dialog_info *mix_dialog = NULL;

static void sound_file_search(Widget FSB_w, XmFileSelectionBoxCallbackStruct *info)
{
  /* generate list of sound files, set XmNfileListItems, XmNfileListItemCount, XmNlistUpdated
   * the latter if new file list generated -- if no files, XmNfileListItems NULL, Count 0
   * can also set XmNdirSpec to full file spec of dir.  The callbackstruct is:
   *    typedef struct
   *    {
   *      int      reason;         Why called
   *      XEvent   * event;  
   *      XmString value;          current value of XmNdirSpec
   *      int      length;         number of bytes in value
   *      XmString mask;           current value of XmNdirMask
   *      int      mask_length;    number of bytes in mask
   *      XmString dir;            current base directory
   *      int      dir_length;     number of bytes in dir
   *      XmString pattern;        current search pattern
   *      int      pattern_length; number of bytes in pattern
   *    } XmFileSelectionBoxCallbackStruct;
   *
   * proc should stick to XmNfileTypeMask (type unsigned char): 
   *   XmFILE_REGULAR -- regular files
   *   XmFILE_DIRECTORY -- directories
   *   XmFILE_ANY_TYPE 
   *
   * the pattern (file name mask) only matters if the filter button is hit, 
   * it appears to be "*" until the filter is invoked.
   */
  char *pattern = NULL, *our_dir = NULL;
  dir *cdp;
  file_dialog_info *fd;
  XmFileSelectionBoxCallbackStruct *data = (XmFileSelectionBoxCallbackStruct *)info;
  int i, which_dialog;
  bool filter_callback;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(FSB_w), FSB_w);
  XtVaGetValues(FSB_w, XmNuserData, &which_dialog, NULL);
  if (which_dialog == FILE_OPEN_DIALOG)
    fd = open_dialog;
  else fd = mix_dialog;
  XmStringGetLtoR (data->pattern, XmFONTLIST_DEFAULT_TAG, &pattern);
  XmStringGetLtoR (data->dir, XmFONTLIST_DEFAULT_TAG, &our_dir);

  if (fd->fullpathname == NULL) 
    fd->fullpathname = (char *)CALLOC(512, sizeof(char));
  filter_callback = (strcmp(pattern, "*") != 0);
  if (!filter_callback)
    {
      if ((fd->last_dir == NULL) || 
	  (strcmp(our_dir, fd->last_dir) != 0) || 
	  (fd->new_file_written))
	{
	  if (fd->current_files) 
	    fd->current_files = free_dir(fd->current_files);
	  if (fd->last_dir) 
	    {
	      FREE(fd->last_dir); 
	      fd->last_dir = NULL;
	    }
	  fd->last_dir = copy_string(our_dir);
	  strcpy(fd->fullpathname, our_dir);
	  fd->save_dir = (char *)(fd->fullpathname + snd_strlen(our_dir));
	  if (fd->sound_files) free_dir(fd->sound_files);
	  fd->sound_files = find_sound_files_in_dir(our_dir);
	  fd->need_update = true;
	}
      if (fd->last_pattern)
	{
	  FREE(fd->last_pattern);
	  fd->last_pattern = NULL;
	}
      cdp = fd->sound_files;
    }
  else 
    {
      if ((fd->last_pattern == NULL) || 
	  (strcmp(pattern, fd->last_pattern) != 0) || 
	  (fd->new_file_written))
	  {
	    if (fd->last_pattern) 
	      {
		FREE(fd->last_pattern); 
		fd->last_pattern = NULL;
	      }
	    fd->last_pattern = copy_string(pattern);
	    if (fd->current_files)  
	      fd->current_files = free_dir(fd->current_files);
	    if ((fd->sound_files) && 
		(fd->sound_files->len > 0)) 
	      fd->current_files = filter_sound_files(fd->sound_files, pattern);
	    fd->need_update = true;
	  }
      cdp = fd->current_files;
    }  
  fd->new_file_written = false;
  if (fd->need_update)
    {
      XmString *names = NULL;
      if ((cdp) && (cdp->len > 0))
	{
	  qsort((void *)(cdp->files), cdp->len, sizeof(char *), string_compare);
	  names = (XmString *)CALLOC(cdp->len, sizeof(XmString));

#ifdef SGI
	  /* this is true only if the SGI "enhanced FSB" is in use, I hope */
	  if (!(XtNameToWidget(FSB_w, "Text"))) 
	    fd->save_dir = fd->fullpathname;
	  /* can't use SgDIALOG_FINDER here as suggested by SGI "Integration Guide" because
	   * XmFileSelectionBoxGetChild(FSB_w, SgDIALOG_FINDER)) generates an error if
	   * snd was loaded without -lSgm.
	   */
#endif

	  for (i = 0; i < cdp->len; i++) 
	    {
	      char *sp, *sn;
	      for (sp = fd->save_dir, sn = cdp->files[i]; ((*sp) = (*sn)) != '\0'; sp++, sn++);
	      /* save_dir is a pointer into fullpathname after the directory portion */
	      /*   this is unreadable code! -- it's basically sprintf(fullpathname, "%s%s", our_dir, cdp->files[i]) I think */
	      names[i] = XmStringCreate(fd->fullpathname, XmFONTLIST_DEFAULT_TAG);
	    }
	}
      else names = NULL;
      if (cdp) 
	XtVaSetValues(FSB_w, 
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
  fd->need_update = false;
}

static void force_directory_reread(Widget dialog)
{
  XmString dirmask;
  XtVaGetValues(dialog, XmNdirMask, &dirmask, NULL);
  XmFileSelectionDoSearch(dialog, dirmask);
  XmStringFree(dirmask);
}

static void just_sounds_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  file_dialog_info *fd = (file_dialog_info *)context;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  if (cb->set)
    {
      XtVaGetValues(fd->dialog, XmNfileSearchProc, &(fd->default_search_proc), NULL);
      XtVaSetValues(fd->dialog, XmNfileSearchProc, sound_file_search, NULL);
    }
  else XtVaSetValues(fd->dialog, XmNfileSearchProc, fd->default_search_proc, NULL);
  fd->need_update = true;
  force_directory_reread(fd->dialog);
}

#if (XmVERSION > 1)
static void file_dialog_select_callback(Widget w, XtPointer context, XtPointer info)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmString *strs;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  XtVaGetValues(w, XmNselectedItems, &strs, NULL);
  filename = (char *)XmStringUnparse(strs[0], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if (filename)
    {
      if (sound_file_p(filename))
	{
	  XmString label;
	  char *buf;
	  char timestr[64];
	  time_t date;
	  XtManageChild(fd->play_selected_button);
	  buf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s: %d chan%s, %d Hz, %.3f secs",
		       filename_without_home_directory(filename),
		       mus_sound_chans(filename),
		       (mus_sound_chans(filename) > 1) ? "s" : "",
		       mus_sound_srate(filename),
		       mus_sound_duration(filename));
	  label = XmStringCreateLocalized(buf);
	  XtVaSetValues(fd->dialog_info1, XmNlabelString, label, NULL);
	  XmStringFree(label);
	  date = mus_sound_write_date(filename);
#if HAVE_STRFTIME
	  strftime(timestr, 64, ", %d-%b-%Y", localtime(&date));
#else
	  sprintf(timestr, "");
#endif
	  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s %s%s",
		       mus_header_type_name(mus_sound_header_type(filename)),
		       mus_short_data_format_name(mus_sound_data_format(filename)),
		       timestr);
	  label = XmStringCreateLocalized(buf);
	  XtVaSetValues(fd->dialog_info2, XmNlabelString, label, NULL);
	  XmStringFree(label);
	  FREE(buf);
	  if (!(XtIsManaged(fd->dialog_frame))) 
	    XtManageChild(fd->dialog_frame);
	}
      XtFree(filename);
    }
  else
    {
      if (XtIsManaged(fd->play_selected_button)) 
	XtUnmanageChild(fd->play_selected_button);
      if (XtIsManaged(fd->dialog_frame)) 
	XtUnmanageChild(fd->dialog_frame);
    }
}
#endif

static file_dialog_info *make_file_dialog(bool read_only, char *title, char *select_title, snd_dialog_t which_dialog, 
					  XtCallbackProc file_ok_proc, XtCallbackProc file_help_proc)
{
  Widget w;
  file_dialog_info *fd;
  Arg args[20];
  int n;
  XmString s1, s2;
  Widget wtmp = NULL, rc, rc1, rc2;
  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->file_dialog_read_only = read_only;
  /* file selection dialog box with added "Just Sound Files" toggle button */
  w = MAIN_SHELL(ss);
  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  s1 = XmStringCreate(select_title, XmFONTLIST_DEFAULT_TAG);
  s2 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
  XtSetArg(args[n], XmNselectionLabelString, s1); n++;
  XtSetArg(args[n], XmNuserData, which_dialog); n++;
  XtSetArg(args[n], XmNdialogTitle, s2); n++;
  fd->dialog = XmCreateFileSelectionDialog(w, title, args, n);
  XmStringFree(s1);
  XmStringFree(s2);

  rc1 = XtVaCreateManagedWidget("filebuttons", 
				xmRowColumnWidgetClass, fd->dialog,
				XmNorientation, XmVERTICAL,
				NULL);
  rc = XtVaCreateManagedWidget("filebuttons", 
			       xmRowColumnWidgetClass, rc1,
			       XmNorientation, XmHORIZONTAL,
			       NULL);
  fd->just_sounds_button = XtVaCreateManagedWidget(_("sound files only"), xmToggleButtonWidgetClass, rc,
						   XmNset, ss->just_sounds_state,
						   XmNalignment, XmALIGNMENT_BEGINNING,
						   NULL);
  fd->play_selected_button = XtVaCreateWidget(_("play selected sound"), xmToggleButtonWidgetClass, rc,
					      XmNalignment, XmALIGNMENT_END,
					      NULL);
#if (XmVERSION > 1)
  fd->dialog_frame = XtVaCreateWidget("", xmFrameWidgetClass, rc1, NULL);
  rc2 = XtVaCreateManagedWidget("info-rc2", 
				xmRowColumnWidgetClass, fd->dialog_frame,
				XmNorientation, XmVERTICAL,
				XmNbackground, ss->sgx->highlight_color,
				NULL);
  fd->dialog_info1 = XtVaCreateManagedWidget("", xmLabelWidgetClass, rc2, XmNbackground, ss->sgx->highlight_color, NULL);
  fd->dialog_info2 = XtVaCreateManagedWidget("", xmLabelWidgetClass, rc2, XmNbackground, ss->sgx->highlight_color, NULL);
#endif
  color_file_selection_box(fd->dialog);

  wtmp = XtNameToWidget(fd->dialog, "Text");
  if (!wtmp) 
    wtmp = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_TEXT);
  if (wtmp) 
    add_completer_to_textfield(wtmp, add_completer_func(filename_completer));
  wtmp = XtNameToWidget(fd->dialog, "FilterText");
  if (!wtmp) 
    wtmp = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_FILTER_TEXT);
  if (wtmp) 
    add_completer_to_textfield(wtmp, add_completer_func(filename_completer));

  if (!(ss->using_schemes)) 
    {
      XtVaSetValues(fd->just_sounds_button, XmNselectColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(fd->play_selected_button, XmNselectColor, (ss->sgx)->pushed_button_color, NULL);
    }
  XtVaGetValues(fd->dialog, XmNfileSearchProc, &(fd->default_search_proc), NULL);
  XtAddCallback(fd->dialog, XmNokCallback, file_ok_proc, (XtPointer)fd);
  XtAddCallback(fd->dialog, XmNcancelCallback, file_cancel_callback, (XtPointer)fd);
  XtAddCallback(fd->dialog, XmNhelpCallback, file_help_proc, NULL);
  XtAddCallback(fd->just_sounds_button, XmNvalueChangedCallback, just_sounds_callback, (XtPointer)fd);
  XtAddCallback(fd->play_selected_button, XmNvalueChangedCallback, play_selected_callback, (XtPointer)fd);
#if (XmVERSION > 1)
  XtAddCallback(XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_LIST),
		XmNbrowseSelectionCallback, file_dialog_select_callback, (XtPointer)fd);
#endif
  set_dialog_widget(which_dialog, fd->dialog);
  return(fd);
}


/* -------- open file dialog -------- */

static void file_open_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)info;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  XtUnmanageChild(w);
  XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename);
  file_dialog_stop_playing(fd);
  if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
    {
      snd_info *sp;
      sp = snd_open_file(filename, 
			 fd->file_dialog_read_only);
      if (sp) select_channel(sp, 0);           /* add_sound_window (snd-xsnd.c) will report reason for error, if any */
      if (filename) XtFree(filename);
    }
  else snd_error(_("%s is a directory"), filename);
}

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
  if (open_dialog == NULL)
    {
      open_dialog = make_file_dialog(read_only, title, select_title, FILE_OPEN_DIALOG, file_open_ok_callback, open_file_help_callback);
      if (ss->just_sounds_state) 
	{
	  XtVaSetValues(open_dialog->dialog, XmNfileSearchProc, sound_file_search, NULL);
	  open_dialog->need_update = true;
	  force_directory_reread(open_dialog->dialog);
	}
    }
  else
    {
      if (open_dialog->file_dialog_read_only != read_only)
	{
	  XmString s1, s2;
	  s1 = XmStringCreate(select_title, XmFONTLIST_DEFAULT_TAG);
	  s2 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
	  XtVaSetValues(open_dialog->dialog, XmNselectionLabelString, s1, XmNdialogTitle, s2, NULL);
	  XmStringFree(s1);
	  XmStringFree(s2);
	  open_dialog->file_dialog_read_only = read_only;
	}
    }
  if (open_dialog->new_file_written) 
    {
      force_directory_reread(open_dialog->dialog);
      open_dialog->new_file_written = false;
    }
  if ((managed) && (!(XtIsManaged(open_dialog->dialog))))
    XtManageChild(open_dialog->dialog);
  return(open_dialog->dialog);
}


/* -------- mix file dialog -------- */

static void file_mix_ok_callback(Widget w, XtPointer context, XtPointer info)
{
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)info;
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  XtUnmanageChild(w);
  XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename);
  file_dialog_stop_playing(fd);
  if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
    {
      mix_complete_file_at_cursor(any_selected_sound(), filename, with_mix_tags(ss), 0);
      if (filename) XtFree(filename);
    }
  else snd_error(_("%s is a directory"), filename);
}

widget_t make_mix_file_dialog(bool managed)
{
  /* called from the menu */
  if (mix_dialog == NULL)
    {
      mix_dialog = make_file_dialog(true, _("Mix"), _("mix in:"), FILE_MIX_DIALOG, file_mix_ok_callback, mix_file_help_callback);
      if (ss->just_sounds_state) 
	{
	  XtVaSetValues(mix_dialog->dialog, XmNfileSearchProc, sound_file_search, NULL);
	  mix_dialog->need_update = true;
	  force_directory_reread(mix_dialog->dialog);
	}
    }
  if ((managed) && (!XtIsManaged(mix_dialog->dialog)))
    XtManageChild(mix_dialog->dialog);
  return(mix_dialog->dialog);
}

void set_open_file_play_button(bool val)
{
  if ((open_dialog) && (open_dialog->play_selected_button))
    XmToggleButtonSetState(open_dialog->play_selected_button, (Boolean)val, false);
  if ((mix_dialog) && (mix_dialog->play_selected_button))
    XmToggleButtonSetState(mix_dialog->play_selected_button, (Boolean)val, false);
}

void alert_new_file(void) 
{
  if (open_dialog)
    open_dialog->new_file_written = true;
  if (mix_dialog)
    mix_dialog->new_file_written = true;
}




/* -------- save as dialog (file and edit menus) -------- */
/* 
 * changed 19-June-97 to simply save the current state under the new name and return
 * to the current state/file (different from emacs) -- this keeps mix console intact
 * across backups and so on, and seems more useful to me than switching to the new file.
 * changed again 12-Nov-01 to make this choice settable via emacs-style-save-as.
 * changed again 6-Dec-04 -- need a hook here since gtk changes constantly
 */

static file_data *save_as_file_data = NULL;
static Widget save_as_dialog = NULL, file_save_as_file_name;
static save_dialog_t save_as_dialog_type = FILE_SAVE_AS;

static void save_as_ok_callback(Widget w, XtPointer context, XtPointer info)
{ 
  char *str = NULL, *comment;
  snd_info *sp;
  int type, format, srate, chans;
  bool need_update = false;
  off_t location, samples;
  comment = read_file_data_choices(save_as_file_data, &srate, &chans, &type, &format, &location, &samples);
  str = XmTextGetString(file_save_as_file_name);
  sp = any_selected_sound();
  if ((str) && (*str))
    {
      need_update = saved_file_needs_update(sp, str, save_as_dialog_type, srate, type, format, comment);
      force_directory_reread(save_as_dialog);
    }
  else 
    if (sp) 
      report_in_minibuffer(sp, _("not saved (no file name given)"));
  if (comment) FREE(comment);
  XtUnmanageChild(save_as_dialog);
  if ((sp) && (str) && 
      (save_as_dialog_type == FILE_SAVE_AS) && 
      (need_update))
    run_after_save_as_hook(sp, str, true); /* true => from dialog */
  if (str) XtFree(str);
} 

static void save_as_cancel_callback(Widget w, XtPointer context, XtPointer info)
{ 
  XtUnmanageChild(save_as_dialog);
} 

static void file_data_type_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* this can be called from any header list created by make_file_data_panel */
  int pos;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  file_data *fd;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  XtVaGetValues(w, XmNuserData, &fd, NULL);
  pos = cbs->item_position - 1;
  if (header_type_from_position(pos) != fd->current_type)
    {
      set_header_type_and_format_from_position(fd, pos);
      load_header_and_data_lists(fd,
				 fd->current_type,
				 fd->current_format,
				 0, 0, -1, -1, NULL);
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

#define NUM_HEADER_TYPES 7
static char *header_short_names[NUM_HEADER_TYPES] = {"sun  ", "aifc ", "wave ", "raw  ", "aiff ", "ircam", "nist "};

file_data *make_file_data_panel(Widget parent, char *name, Arg *in_args, int in_n, 
				bool with_chan, int header_type, int data_format, bool with_loc, bool with_comment, bool with_samples)
{
  Widget mainform, form, sep1, hlab, hlist, dlab, dlist, slab, stext, clab, ctext = NULL, sep2, sep3;
  Widget comment_label, comment_text, sep4, loclab, loctext = NULL, samplab, samptext;
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
  mainform = XtCreateManagedWidget(name, xmFormWidgetClass, parent, in_args, in_n);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  form = XtCreateManagedWidget(name, xmFormWidgetClass, mainform, args, n);

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
  hlab = XtCreateManagedWidget(_("header"), xmLabelWidgetClass, form, args, n);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, hlab); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep1); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNlistMarginWidth, 1); n++;
  XtSetArg(args[n], XmNuserData, (XtPointer)fdat); n++;
  /* what is selected depends on current type */
  strs = (XmString *)CALLOC(NUM_HEADER_TYPES, sizeof(XmString)); 
  for (i = 0; i < NUM_HEADER_TYPES; i++) 
    strs[i] = XmStringCreate(header_short_names[i], XmFONTLIST_DEFAULT_TAG);
  XtSetArg(args[n], XmNitems, strs); n++;
  XtSetArg(args[n], XmNitemCount, NUM_HEADER_TYPES); n++;
  XtSetArg(args[n], XmNvisibleItemCount, NUM_VISIBLE_HEADERS); n++;
  hlist = XmCreateScrolledList(form, "header type", args, n);
  XtManageChild(hlist);
  for (i = 0; i < NUM_HEADER_TYPES; i++) XmStringFree(strs[i]);
  FREE(strs);
  XmListSelectPos(hlist, fdat->header_pos + 1, false);
  fdat->header_list = hlist;
  XtAddCallback(fdat->header_list, XmNbrowseSelectionCallback, file_data_type_callback, NULL);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, hlist); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, 15); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep2 = XtCreateManagedWidget("sep2", xmSeparatorWidgetClass, form, args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->help_button_color); n++;}
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep2); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  dlab = XtCreateManagedWidget(_("data"), xmLabelWidgetClass, form, args, n);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, dlab); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep2); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNuserData, (XtPointer)fdat); n++;
  dlist = XmCreateScrolledList(form, "data format", args, n);
  /* what is displayed and selected depends on current type */
  strs = (XmString *)CALLOC(dformats, sizeof(XmString)); 
  for (i = 0; i < dformats; i++) 
    strs[i] = XmStringCreate(formats[i], XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(dlist, XmNitems, strs, XmNitemCount, dformats, NULL);
  for (i = 0; i < dformats; i++) XmStringFree(strs[i]);
  FREE(strs);
  XmListSelectPos(dlist, fdat->format_pos + 1, false);
  XtManageChild(dlist);
  fdat->format_list = dlist;
  XtAddCallback(fdat->format_list, XmNbrowseSelectionCallback, file_data_format_callback, NULL);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, dlist); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, 15); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep3 = XtCreateManagedWidget("sep3", xmSeparatorWidgetClass, form, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep3); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  slab = XtCreateManagedWidget(_("srate:"), xmLabelWidgetClass, form, args, n);

  n = 0;
  XtSetArg(args[n], XmNcolumns, 6); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, slab); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep3); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
  stext = make_textfield_widget("srate-text", form, args, n, NOT_ACTIVATABLE, add_completer_func(srate_completer));
  fdat->srate_text = stext;

  if (with_chan)
    {
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, stext); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      clab = XtCreateManagedWidget(_("chans:"), xmLabelWidgetClass, form, args, n);

      n = 0;
      XtSetArg(args[n], XmNcolumns, 6); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, clab); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      ctext = make_textfield_widget("chans-text", form, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
      fdat->chans_text = ctext;

      if (with_loc)
	{
	  n = 0;
	  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, ctext); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, sep3); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  loclab = XtCreateManagedWidget(_("location:"), xmLabelWidgetClass, form, args, n);

	  n = 0;
	  XtSetArg(args[n], XmNcolumns, 6); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, loclab); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, sep3); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
	  loctext = make_textfield_widget("location-text", form, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
	  fdat->location_text = loctext;
	}
    }

  if (with_samples)
    {
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ((loctext) ? loctext : ((ctext) ? ctext : stext))); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      samplab = XtCreateManagedWidget(_("samples:"), xmLabelWidgetClass, form, args, n);

      n = 0;
      XtSetArg(args[n], XmNcolumns, 16); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, samplab); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      samptext = make_textfield_widget("samples-text", form, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
      fdat->samples_text = samptext;
    }

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, form); n++;
  XtSetArg(args[n], XmNbottomAttachment, (with_comment) ? XmATTACH_NONE : XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNheight, 5); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep4 = XtCreateManagedWidget("sep4", xmSeparatorWidgetClass, mainform, args, n);

  if (with_comment)
    {
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep4); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      comment_label = XtCreateManagedWidget(_("comment:"), xmLabelWidgetClass, mainform, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, comment_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, comment_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrows, 2); n++;
#if (HAVE_OSS || HAVE_ALSA)
      XtSetArg(args[n], XmNcolumns, 40); n++;
      /* this pushes the right boundary over a ways -- otherwise the button holder box takes up all available space */
#endif
      comment_text = make_text_widget("comment-text", mainform, args, n);
      fdat->comment_text = comment_text;
    }
  return(fdat);
}

static void save_as_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  save_as_dialog_help();
}

static void make_save_as_dialog(char *sound_name, int header_type, int format_type)
{
  /* save old as new, close old, open new */
  char *file_string;
  if (!save_as_dialog)
    {
      Arg args[32];
      int n;
      XmString xmstr1, xmstr2, s1;

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      s1 = XmStringCreate(_("save as:"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNselectionLabelString, s1); n++;
      xmstr1 = XmStringCreate(_("Save"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNokLabelString, xmstr1); n++;
      file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, _("save %s"), sound_name);
      xmstr2 = XmStringCreate(file_string, XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNdialogTitle, xmstr2); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNchildPlacement, XmPLACE_ABOVE_SELECTION); n++;
      XtSetArg(args[n], XmNallowOverlap, false); n++;
      save_as_dialog = XmCreateFileSelectionDialog(MAIN_SHELL(ss), "save-as", args, n);
      FREE(file_string);

      XtManageChild(save_as_dialog);
      XmStringFree(s1);
      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      file_save_as_file_name = XtNameToWidget(save_as_dialog, "Text");
      if (!(file_save_as_file_name)) file_save_as_file_name = XmFileSelectionBoxGetChild(save_as_dialog, XmDIALOG_TEXT);

      XtAddCallback(save_as_dialog, XmNhelpCallback, save_as_help_callback, NULL);
      XtAddCallback(save_as_dialog, XmNcancelCallback, save_as_cancel_callback, NULL);
      XtAddCallback(save_as_dialog, XmNokCallback, save_as_ok_callback, NULL);
      
      n = attach_all_sides(args, 0);
      save_as_file_data = make_file_data_panel(save_as_dialog, "data-form", args, n, false, header_type, format_type, false, true, false);

      color_file_selection_box(save_as_dialog);
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(save_as_file_data->format_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
	  XtVaSetValues(save_as_file_data->header_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
	}
      set_dialog_widget(FILE_SAVE_AS_DIALOG, save_as_dialog);
    }
  else
    {
      XmString xmstr2;
      file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, _("save %s"), sound_name);
      xmstr2 = XmStringCreate(file_string, XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(save_as_dialog, XmNdialogTitle, xmstr2, NULL);
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
		      default_output_type(ss),
		      default_output_format(ss));
  load_header_and_data_lists(save_as_file_data,
			     save_as_file_data->current_type,
			     save_as_file_data->current_format,
			     (hdr) ? hdr->srate : selection_srate(), 
			     0, -1, -1,
			     com = output_comment(hdr));
  if (com) FREE(com);
  if ((managed) && (!XtIsManaged(save_as_dialog))) XtManageChild(save_as_dialog);
  return(save_as_dialog);
}

widget_t make_edit_save_as_dialog(bool managed)
{
  save_as_dialog_type = EDIT_SAVE_AS;
  make_save_as_dialog(_("current selection"),
		      default_output_type(ss),
		      default_output_format(ss));
  load_header_and_data_lists(save_as_file_data,
			     save_as_file_data->current_type,
			     save_as_file_data->current_format,
			     selection_srate(), 
			     0, -1, -1, NULL);
  if ((managed) && (!XtIsManaged(save_as_dialog))) XtManageChild(save_as_dialog);
  return(save_as_dialog);
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      rlw = XtCreateManagedWidget(main_str, xmLabelWidgetClass, formw, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->white); n++;}
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      smenu = XmCreatePulldownMenu(sbar, _("sort"), args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNsubMenuId, smenu); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      XtCreateManagedWidget("sort", xmCascadeButtonWidgetClass, sbar, args, n);
      
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      wwi->byname = XtCreateManagedWidget(_("name"), xmPushButtonWidgetClass, smenu, args, n);
      wwi->bydate = XtCreateManagedWidget(_("date"), xmPushButtonWidgetClass, smenu, args, n);
      wwi->bysize = XtCreateManagedWidget(_("size"), xmPushButtonWidgetClass, smenu, args, n);
      wwi->byentry = XtCreateManagedWidget(_("entry"), xmPushButtonWidgetClass, smenu, args, n);
      wwi->byproc = XtCreateManagedWidget(_("proc"), xmPushButtonWidgetClass, smenu, args, n);
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
  XtVaSetValues(wwi->list, XmNworkWindow, wwi->ww, NULL);
  
  return(wwi);
}

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
	      XmStringGetLtoR(s1, XmFONTLIST_DEFAULT_TAG, &label);
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
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, (last_row) ? XmATTACH_WIDGET : XmATTACH_FORM); n++;
  if (last_row) {XtSetArg(args[n], XmNtopWidget, last_row); n++;}
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNheight, 18); n++; 
  r->rw = XtCreateWidget("rw", xmFormWidgetClass, ww, args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNselectColor, (ss->sgx)->pushed_button_color); n++;}
  XtSetArg(args[n], XmNlabelString, s1); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n1 = make_callback_list(play_callback, (XtPointer)r)); n++;
  if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
  XtSetArg(args[n], XmNmarginWidth, 8); n++;
  r->pl = make_togglebutton_widget("pl", r->rw, args, n);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
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

void view_curfiles_set_row_name(int pos)
{
  regrow *r;
  r = cur_name_row[pos];
  set_button_label(r->nm, view_curfiles_name(r->pos));
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
      XtVaSetValues(r->rw, XmNbackground, (ss->sgx)->highlight_color, NULL);
      XtVaSetValues(r->nm, XmNbackground, (ss->sgx)->highlight_color, NULL);
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
      XtVaSetValues(r->rw, XmNbackground, (ss->sgx)->zoom_color, NULL);
      XtVaSetValues(r->nm, XmNbackground, (ss->sgx)->zoom_color, NULL);
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNcancelLabelString, xclear); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      view_files_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), "File Browser", args, n);

      XtAddCallback(view_files_dialog, XmNcancelCallback, view_files_clear_callback, NULL);
      XtAddCallback(view_files_dialog, XmNhelpCallback, view_files_help_callback, NULL);
      XtAddCallback(view_files_dialog, XmNokCallback, view_files_dismiss_callback, NULL);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xclear);
      XmStringFree(titlestr);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_OK_BUTTON), XmNbackground, (ss->sgx)->quit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, (ss->sgx)->reset_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, (ss->sgx)->help_button_color, NULL);
	}

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->doit_button_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      updateB = XtCreateManagedWidget(_("Update"), xmPushButtonGadgetClass, view_files_dialog, args, n);
      /* need Gadget if we want a subsequent XmNbackgroundPixmap change to be reflected in the button */
      XtAddCallback(updateB, XmNactivateCallback, view_files_update_callback, NULL);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(view_files_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("formd", xmFormWidgetClass, view_files_dialog, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 49); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      curform = XtCreateManagedWidget("curform", xmFormWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, curform); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
      sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      XtAddCallback(wwl->byname, XmNactivateCallback, sort_prevfiles_by_name, NULL);
      XtAddCallback(wwl->bydate, XmNactivateCallback, sort_prevfiles_by_date, NULL);
      XtAddCallback(wwl->bysize, XmNactivateCallback, sort_prevfiles_by_size, NULL);
      XtAddCallback(wwl->byentry, XmNactivateCallback, sort_prevfiles_by_entry_order, NULL);
      XtAddCallback(wwl->byproc, XmNactivateCallback, sort_prevfiles_by_user_procedure, NULL);
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



/* -------------------------------- Raw Data Dialog -------------------------------- */

static Widget raw_data_dialog = NULL;
static Widget raw_srate_text, raw_chans_text, raw_location_text;
static off_t raw_data_location = 0;
static bool raw_cancelled = false;

static void raw_data_ok_callback(Widget w, XtPointer context, XtPointer info) {raw_cancelled = false;}
static void raw_data_cancel_callback(Widget w, XtPointer context, XtPointer info) {raw_cancelled = true;}

static void raw_data_default_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_data_ok_callback(w, context, info);
}

static void raw_data_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  int sr, oc, fr;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  mus_header_raw_defaults(&sr, &oc, &fr);
  fr = cbs->item_position;
  mus_header_set_raw_defaults(sr, oc, fr);
}

static void raw_data_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_data_dialog_help();
}

static void make_raw_data_dialog(const char *filename)
{
  XmString *formats;
  XmString xstr1, xstr2, xstr3, xstr4, titlestr;
  int i, n;
  int sr, oc, fr;
  char *str;
  Arg args[20];
  Widget lst, defw, dls, rform, dlab, dloclab, chnlab;

  mus_header_raw_defaults(&sr, &oc, &fr);

  n = 0;
  xstr1 = XmStringCreate(_("Cancel"), XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
  xstr2 = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
  xstr3 = XmStringCreate(_("Ok"), XmFONTLIST_DEFAULT_TAG);
  titlestr = XmStringCreate(_("No Header on File"), XmFONTLIST_DEFAULT_TAG);
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, PRINT_BUFFER_SIZE, 
	       _("No header found for %s"),
	       filename_without_home_directory(filename));
  xstr4 = XmStringCreate(str, XmFONTLIST_DEFAULT_TAG);
  FREE(str);
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  XtSetArg(args[n], XmNcancelLabelString, xstr1); n++;
  XtSetArg(args[n], XmNhelpLabelString, xstr2); n++;
  XtSetArg(args[n], XmNokLabelString, xstr3); n++;
  XtSetArg(args[n], XmNmessageString, xstr4); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  /* not transient -- we want this window to remain visible if possible */
  raw_data_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), "raw data", args, n);

  XtAddCallback(raw_data_dialog, XmNcancelCallback, raw_data_cancel_callback, NULL);
  XtAddCallback(raw_data_dialog, XmNhelpCallback, raw_data_help_callback, NULL);
  XtAddCallback(raw_data_dialog, XmNokCallback, raw_data_ok_callback, NULL);
  XmStringFree(xstr1);
  XmStringFree(xstr2);
  XmStringFree(xstr3);
  XmStringFree(xstr4);
  XmStringFree(titlestr);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
  defw = XtCreateManagedWidget(_("Default"), xmPushButtonGadgetClass, raw_data_dialog, args, n);
  XtAddCallback(defw, XmNactivateCallback, raw_data_default_callback, NULL);

  rform = XtCreateManagedWidget("sretc", xmFormWidgetClass, raw_data_dialog, NULL, 0);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNmarginTop, 6); n++;
  dls = XtCreateManagedWidget(_("srate:"), xmLabelWidgetClass, rform, args, n);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, dls); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNcolumns, 6); n++;
  XtSetArg(args[n], XmNresizeWidth, false); n++;
  raw_srate_text = make_textfield_widget("text", rform, args, n, NOT_ACTIVATABLE, add_completer_func(srate_completer));
  widget_int_to_text(raw_srate_text, sr);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, raw_srate_text); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;	
  XtSetArg(args[n], XmNmarginTop, 6); n++;
  chnlab = XtCreateManagedWidget(_("chans:"), xmLabelWidgetClass, rform, args, n);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, chnlab); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNcolumns, 3); n++;
  XtSetArg(args[n], XmNresizeWidth, false); n++;
  raw_chans_text = make_textfield_widget("text", rform, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
  widget_int_to_text(raw_chans_text, oc);
  
  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, raw_srate_text); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  XtSetArg(args[n], XmNmarginTop, 6); n++;
  dloclab = XtCreateManagedWidget(_("data location:"), xmLabelWidgetClass, rform, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, dloclab); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, raw_srate_text); n++;
  XtSetArg(args[n], XmNcolumns, 8); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  raw_location_text = make_textfield_widget("text", rform, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
  XmTextSetString(raw_location_text, "0");

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, raw_location_text); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  dlab = XtCreateManagedWidget(_("data format:"), xmLabelWidgetClass, rform, args, n);

  n = 0;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, dlab); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  formats = (XmString *)CALLOC(MUS_LAST_DATA_FORMAT, sizeof(XmString));
  for (i = 0; i < MUS_LAST_DATA_FORMAT; i++) 
    formats[i] = XmStringCreate((char *)mus_data_format_name(i + 1), XmFONTLIST_DEFAULT_TAG);
  lst = XmCreateScrolledList(rform, "raw-data-format-list", args, n);
  XtVaSetValues(lst, XmNitems, formats, 
		     XmNitemCount, MUS_LAST_DATA_FORMAT, 
		     XmNvisibleItemCount, 6, NULL);
  XtManageChild(lst); 
  XmListSelectPos(lst, fr, false);
  for (i = 0; i < MUS_LAST_DATA_FORMAT; i++) 
    XmStringFree(formats[i]);
  FREE(formats);
  XtAddCallback(lst, XmNbrowseSelectionCallback, raw_data_browse_callback, NULL);

  XtManageChild(raw_data_dialog);

  if (!(ss->using_schemes)) 
    {
      map_over_children(rform, set_main_color_of_widget, NULL);
      XtVaSetValues(lst, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_OK_BUTTON), XmNbackground, (ss->sgx)->doit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, (ss->sgx)->quit_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, (ss->sgx)->help_button_color, NULL);
      XtVaSetValues(defw, XmNselectColor, (ss->sgx)->pushed_button_color, NULL);
    }
  set_dialog_widget(RAW_DATA_DIALOG, raw_data_dialog);
}

file_info *raw_data_dialog_to_file_info(const char *filename, const char *title)
{
  /* put up dialog for srate, chans, data format */
  XmString xstr;
  char *str;
  int sr, oc, fr;
  file_info *hdr = NULL;
  if (!raw_data_dialog) 
    make_raw_data_dialog(filename);
  xstr = XmStringCreate((char *)title, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(raw_data_dialog, XmNmessageString, xstr, NULL);
  XmStringFree(xstr);
  raise_dialog(raw_data_dialog);
  reflect_raw_pending_in_menu();
  if (!XtIsManaged(raw_data_dialog)) 
    XtManageChild(raw_data_dialog);
#if DEBUGGING
  if (with_background_processes(ss))
#endif
    while (XtIsManaged(raw_data_dialog)) 
      check_for_event();
  reflect_raw_open_in_menu();
  if (raw_cancelled) return(NULL);
  mus_header_raw_defaults(&sr, &oc, &fr);
  str = XmTextGetString(raw_srate_text);
  if ((str) && (*str)) 
    {
      sr = string_to_int(str); 
      if (sr <= 0) sr = 1;
      XtFree(str);
    }
  str = XmTextGetString(raw_chans_text);
  if ((str) && (*str)) 
    {
      oc = string_to_int(str); 
      if (oc <= 0) oc = 1;
      XtFree(str);
    }
  str = XmTextGetString(raw_location_text);
  if ((str) && (*str)) 
    {
      raw_data_location = string_to_int(str); 
      if (raw_data_location < 0) raw_data_location = 0;
      XtFree(str);
    }
  mus_header_set_raw_defaults(sr, oc, fr);
  mus_sound_override_header(filename, sr, oc, fr, MUS_RAW, raw_data_location,
			    mus_bytes_to_samples(fr, mus_sound_length(filename) - raw_data_location));
  hdr = (file_info *)CALLOC(1, sizeof(file_info));
  hdr->name = copy_string(filename);
  hdr->type = MUS_RAW;
  hdr->srate = mus_sound_srate(filename);
  hdr->chans = mus_sound_chans(filename);
  hdr->format = mus_sound_data_format(filename);
  hdr->samples = mus_sound_samples(filename); /* total samples, not per channel */
  hdr->data_location = mus_sound_data_location(filename);
  hdr->comment = NULL;
  return(hdr);
}



/* -------------------------------- New File -------------------------------- */

static bool new_file_cancelled = false;
static Widget new_dialog = NULL;
static file_data *new_dialog_data = NULL;
static Widget new_file_name = NULL;
static off_t initial_samples = 1;

static void new_file_ok_callback(Widget w, XtPointer context, XtPointer info) {new_file_cancelled = false;}
static void new_file_cancel_callback(Widget w, XtPointer context, XtPointer info) {new_file_cancelled = true;}

static void new_file_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  new_file_dialog_help();
}

snd_info *make_new_file_dialog(char *newname, int header_type, int data_format, int srate, int chans, char *comment)
{
  snd_info *sp = NULL;
  XmString titlestr;

  {
    char *title;
    new_file_cancelled = false;
    title = (char *)CALLOC(snd_strlen(newname) + 32, sizeof(char));
    sprintf(title, _("create new sound: %s"), newname);
    titlestr = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
    FREE(title);
  }

  if (!new_dialog)
    {
      Arg args[20];
      int n;
      XmString xok, xcancel, xhelp;
      Widget name_label, form;

      xhelp = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      xcancel = XmStringCreate(_("Cancel"), XmFONTLIST_DEFAULT_TAG);
      xok = XmStringCreate(_("Ok"), XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNcancelLabelString, xcancel); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xok); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      new_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), "new", args, n);

      XmStringFree(titlestr);
      XmStringFree(xok);
      XmStringFree(xcancel);
      XmStringFree(xhelp);

      XtAddCallback(new_dialog, XmNhelpCallback, new_file_help_callback, NULL);
      XtAddCallback(new_dialog, XmNcancelCallback, new_file_cancel_callback, NULL);
      XtAddCallback(new_dialog, XmNokCallback, new_file_ok_callback, NULL);

      n = attach_all_sides(args, 0);
      form = XtCreateManagedWidget("newfile", xmFormWidgetClass, new_dialog, args, n);

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
      XtSetArg(args[n], XmNvalue, newname); n++;
      new_file_name = make_textfield_widget("newtext", form, args, n, NOT_ACTIVATABLE, add_completer_func(filename_completer));

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, new_file_name); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      new_dialog_data = make_file_data_panel(form, "data-form", args, n, true, default_output_type(ss), default_output_format(ss), false, true, true);

      XtManageChild(new_dialog);
      if (!(ss->using_schemes)) map_over_children(new_dialog, set_main_color_of_widget, NULL);
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(new_dialog_data->format_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
	  XtVaSetValues(new_dialog_data->header_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
	}
      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog, XmDIALOG_OK_BUTTON), XmNbackground, (ss->sgx)->doit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, (ss->sgx)->quit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, (ss->sgx)->help_button_color, NULL);
	}
      set_dialog_widget(NEW_FILE_DIALOG, new_dialog);
    }
  else 
    {
      XtVaSetValues(new_dialog, XmNdialogTitle, titlestr, NULL);
      XmStringFree(titlestr);
      XmTextSetString(new_file_name, newname);
    }

  load_header_and_data_lists(new_dialog_data, header_type, data_format, srate, chans, -1, initial_samples, comment);
  if (!(XtIsManaged(new_dialog))) XtManageChild(new_dialog);
#if DEBUGGING
  if (with_background_processes(ss))
    {
      while (XtIsManaged(new_dialog)) 
	check_for_event();
    }
  else XtUnmanageChild(new_dialog);
#else
  while (XtIsManaged(new_dialog)) check_for_event();
#endif
  if (new_file_cancelled)
    return(NULL);
  else
    {
      off_t loc;
      char *tmpstr, *newer_name = NULL;
      newer_name = XmTextGetString(new_file_name);
      if (newer_name == NULL) return(NULL);
      tmpstr = read_file_data_choices(new_dialog_data, &srate, &chans, &header_type, &data_format, &loc, &initial_samples);
      sp = snd_new_file(newer_name, header_type, data_format, srate, chans, tmpstr, initial_samples);
      XtFree(newer_name);
      if (tmpstr) FREE(tmpstr);
    }
  return(sp);
}


/* ---------------- EDIT_HEADER ---------------- */

static Widget edit_header_dialog = NULL;
static file_data *edit_header_data;
static snd_info *edit_header_sp = NULL;

static void edit_header_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  edit_header_dialog_help();
}

static void edit_header_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(edit_header_dialog);
}

#if DEBUGGING && HAVE_GUILE
static XEN g_apply_edit_header(void)
{
  if ((edit_header_sp) && (edit_header_sp->active))
    {
      if (!(edit_header_sp->read_only))
	edit_header_callback(edit_header_sp, edit_header_data);
      else snd_error(_("%s is write-protected"), edit_header_sp->short_filename);
    }
  XtUnmanageChild(edit_header_dialog);
  return(XEN_FALSE);
}
#endif

static void edit_header_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  if ((edit_header_sp) && (edit_header_sp->active))
    {
      if (XmGetFocusWidget(edit_header_dialog) == XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_OK_BUTTON))
	{
	  if (!(edit_header_sp->read_only))
	    edit_header_callback(edit_header_sp, edit_header_data);
	  else snd_error(_("%s is write-protected"), edit_header_sp->short_filename);
	}
    }
  XtUnmanageChild(edit_header_dialog);
}

Widget edit_header(snd_info *sp)
{
  /* like display-info, but writable.
   * need fields for srate, channels, type, format, data location, comment
   * if any are changed, need save button, cancel button, dismiss (leave unsaved but pending), reflect (change Snd display, not file)
   * this means the Snd-effective header is separate from the in-file header even across saves??
   */
  file_info *hdr;
  XmString xstr4;
  if (!sp) return(NULL);
  edit_header_sp = sp;
  hdr = sp->hdr;

  {
    char *str;
    str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
    mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of %s"), sp->short_filename);
    xstr4 = XmStringCreate(str, XmFONTLIST_DEFAULT_TAG);
    FREE(str);
  }

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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      XtAddCallback(edit_header_dialog, XmNhelpCallback, edit_header_help_callback, NULL);
      XtAddCallback(edit_header_dialog, XmNokCallback, edit_header_ok_callback, NULL);
      XmStringFree(xstr1);
      XmStringFree(xstr2);
      XmStringFree(xstr3);
      XmStringFree(titlestr);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      edit_header_data = make_file_data_panel(edit_header_dialog, "Edit Header", args, n, true, hdr->type, hdr->format, true, true, true);
      load_header_and_data_lists(edit_header_data, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->samples, hdr->comment);

      XtManageChild(edit_header_dialog);

      if (!(ss->using_schemes)) 
	{
	  map_over_children(edit_header_dialog, set_main_color_of_widget, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_OK_BUTTON), XmNbackground, (ss->sgx)->doit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, (ss->sgx)->quit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, (ss->sgx)->help_button_color, NULL);
	  XtVaSetValues(edit_header_data->header_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
	  XtVaSetValues(edit_header_data->format_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
	}
      set_dialog_widget(EDIT_HEADER_DIALOG, edit_header_dialog);
    }
  else 
    {
      XtVaSetValues(edit_header_dialog, XmNmessageString, xstr4, NULL);
      load_header_and_data_lists(edit_header_data, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->samples, hdr->comment);
      raise_dialog(edit_header_dialog);
    }
  XmStringFree(xstr4);
  if (!(XtIsManaged(edit_header_dialog))) XtManageChild(edit_header_dialog);
  return(edit_header_dialog);
}


/* ---------------- POST-IT MONOLOG ---------------- */

#define POST_IT_ROWS 12
#define POST_IT_COLUMNS 56

static Widget post_it_dialog = NULL;
static Widget post_it_text = NULL;

static void create_post_it_monolog(void)
{
  /* create scrollable but not editable text window */
  Arg args[20];
  int n;

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
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
      XtSetArg(args[n], XmNforeground, (ss->sgx)->black); n++; /* needed if color allocation fails completely */
      XtSetArg(args[n], XmNbackground, (ss->sgx)->white); n++;
    }
  post_it_text = XmCreateScrolledText(post_it_dialog, "post-it-text", args, n);
  XtManageChild(post_it_text);
  XtManageChild(post_it_dialog);

  if (!(ss->using_schemes))
    {
      map_over_children(post_it_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(post_it_text, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      XtVaSetValues(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(post_it_dialog, XmDIALOG_OK_BUTTON), XmNbackground, (ss->sgx)->quit_button_color, NULL);
    }
  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}

widget_t post_it(const char *subject, const char *str)
{
  /* place string in scrollable help window */
  XmString xstr1, xstr2;
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

void post_it_append(char *text)
{
  XmTextInsert(post_it_text, XmTextGetLastPosition(post_it_text), text);
}

void reflect_just_sounds_state(void)
{
  if ((open_dialog) && (open_dialog->just_sounds_button))
    XmToggleButtonSetState(open_dialog->just_sounds_button, ss->just_sounds_state, true);
  if ((mix_dialog) && (mix_dialog->just_sounds_button))
    XmToggleButtonSetState(mix_dialog->just_sounds_button, ss->just_sounds_state, true);
}


#if DEBUGGING && HAVE_GUILE
static XEN g_new_file_dialog(void)
{
  snd_info *sp;
  sp = new_file_from_menu();
  if (sp)
    return(C_TO_XEN_INT(sp->index));
  return(XEN_FALSE);
}
#endif

void g_init_gxfile(void)
{
#if HAVE_GUILE
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

  XEN_DEFINE_HOOK(mouse_enter_label_hook, S_mouse_enter_label_hook, 3, H_mouse_enter_label_hook);
  XEN_DEFINE_HOOK(mouse_leave_label_hook, S_mouse_leave_label_hook, 3, H_mouse_leave_label_hook);

#if DEBUGGING && HAVE_GUILE
  XEN_DEFINE_PROCEDURE("new-file-dialog", g_new_file_dialog, 0, 0, 0, "internal testing function");
  XEN_DEFINE_PROCEDURE("apply-edit-header", g_apply_edit_header, 0, 0, 0, "internal testing function");
#endif
}

