#include "snd.h"

/* various file-related dialogs:
   File|Edit-Save-as, Open|View, File|Edit-Mix, Edit-Header, Raw, New, View:Files and region lists 
*/

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

static void color_file_selection_box(Widget w)
{
  /* overwrite most Motif-default colors */
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
  file_pattern_info *fp;
  XmFileSelectionBoxCallbackStruct *data = (XmFileSelectionBoxCallbackStruct *)info;
  int i;
  bool filter_callback;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(dialog), dialog);

  XtVaGetValues(dialog, XmNuserData, &fp, NULL);
  XmStringGetLtoR (data->pattern, XmFONTLIST_DEFAULT_TAG, &pattern);
  XmStringGetLtoR (data->dir, XmFONTLIST_DEFAULT_TAG, &our_dir);

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
	  if (fp->sound_files) free_dir(fp->sound_files);
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
  XmString dirmask;
  XtVaGetValues(dialog, XmNdirMask, &dirmask, NULL);
  XmFileSelectionDoSearch(dialog, dirmask);
  XmStringFree(dirmask);
}

static void just_sounds_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  file_pattern_info *fp = (file_pattern_info *)context;
  XmString lab;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
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

  /* force_directory_reread below calls XmFileSelectionDoSearch which "reinitializes" the selection box, resetting
   *   the current filename field and thereby triggering a modify callback to clear any error message as well.
   *   So, we add a flag telling a possible open_modify_callback not to clear anything, and then reset the
   *   filename by hand.
   */
  {
    Widget wtmp;
    char *filename;
    fp->in_just_sounds_update = true;
    wtmp = XtNameToWidget(fp->dialog, "Text");
    if (!wtmp) 
      wtmp = XmFileSelectionBoxGetChild(fp->dialog, XmDIALOG_TEXT);
    filename = XmTextGetString(wtmp);
    force_directory_reread(fp->dialog);
    XmTextSetString(wtmp, filename);
    fp->in_just_sounds_update = false;
  }
}


/* -------- File Open/View/Mix Dialogs -------- */

typedef struct file_dialog_info {
  bool file_dialog_read_only;
  Widget dialog, play_selected_button;
  Widget dialog_frame, dialog_info1, dialog_info2; /* labels giving info on selected file, or an error message */
  snd_info *file_play_sp;
  file_pattern_info *fp;
} file_dialog_info;

static void open_file_help_callback (Widget w, XtPointer context, XtPointer info) 
{
  open_file_dialog_help();
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

void clear_deleted_snd_info(struct file_dialog_info *fd)
{
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
      if ((fd->file_play_sp) && 
	  (fd->file_play_sp->playing)) 
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
	      fd->file_play_sp->delete_me = fd;
	      if (fd->file_play_sp)
		play_sound(fd->file_play_sp, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
	    }
	  XtFree(filename);
	}
    }
  else file_dialog_stop_playing(fd);
}

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
		       mus_header_type_to_string(mus_sound_header_type(filename)),
		       mus_data_format_to_string(mus_sound_data_format(filename)),
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

static file_dialog_info *make_file_dialog(bool read_only, char *title, char *select_title, 
					  XtCallbackProc file_ok_proc, XtCallbackProc file_help_proc)
{
  Widget w;
  file_dialog_info *fd;
  Arg args[20];
  int n;
  XmString s1, s2;
  Widget wtmp = NULL, rc, rc1, rc2;
  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->fp = (file_pattern_info *)CALLOC(1, sizeof(file_pattern_info));
  fd->fp->in_just_sounds_update = false;
  fd->file_dialog_read_only = read_only;
  /* file selection dialog box with added "Just Sound Files" and "Play selected" toggle buttons and info area */
  w = MAIN_SHELL(ss);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  s1 = XmStringCreate(select_title, XmFONTLIST_DEFAULT_TAG);
  s2 = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);

  XtSetArg(args[n], XmNselectionLabelString, s1); n++;
  XtSetArg(args[n], XmNuserData, (XtPointer)(fd->fp)); n++;
  XtSetArg(args[n], XmNdialogTitle, s2); n++;

  if (just_sounds(ss))
    {
      XmString lab;
      lab = XmStringCreate(_("Sound Files"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNfileListLabelString, lab); n++;
      XmStringFree(lab);
    }
  fd->dialog = XmCreateFileSelectionDialog(w, title, args, n);
  fd->fp->dialog = fd->dialog;
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
  fd->fp->just_sounds_button = XtVaCreateManagedWidget(_("sound files only"), xmToggleButtonWidgetClass, rc,
						   XmNset, just_sounds(ss),
						   XmNalignment, XmALIGNMENT_BEGINNING,
						   NULL);
  fd->play_selected_button = XtVaCreateWidget(_("play selected sound"), xmToggleButtonWidgetClass, rc,
					      XmNalignment, XmALIGNMENT_END,
					      NULL);

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
    add_completer_to_textfield(wtmp, add_completer_func(filename_completer));

  wtmp = XtNameToWidget(fd->dialog, "FilterText");
  if (!wtmp) 
    wtmp = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_FILTER_TEXT);
  if (wtmp) 
    add_completer_to_textfield(wtmp, add_completer_func(filename_completer));

  if (!(ss->using_schemes)) 
    {
      XtVaSetValues(fd->fp->just_sounds_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(fd->play_selected_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
    }

  XtVaGetValues(fd->dialog, XmNfileSearchProc, &(fd->fp->default_search_proc), NULL);
  XtAddCallback(fd->fp->just_sounds_button, XmNvalueChangedCallback, just_sounds_callback, (XtPointer)(fd->fp));

  XtAddCallback(fd->dialog, XmNokCallback, file_ok_proc, (XtPointer)fd);
  XtAddCallback(fd->dialog, XmNcancelCallback, file_cancel_callback, (XtPointer)fd);
  XtAddCallback(fd->dialog, XmNhelpCallback, file_help_proc, NULL);
  XtAddCallback(fd->play_selected_button, XmNvalueChangedCallback, play_selected_callback, (XtPointer)fd);
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
  XtVaSetValues(fd->dialog_info1, XmNlabelString, msg, NULL);
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
      dialog_filename_text = XtNameToWidget(w, "Text");
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
  XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &filename);
  file_dialog_stop_playing(fd);
  if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
    {
      snd_info *sp;
      /* the possible snd_error calls are so deeply buried here that it's easiest just to redirect the error handler to us */
      redirect_snd_error_to(file_open_error, (void *)fd);
      sp = snd_open_file(filename, fd->file_dialog_read_only);
      redirect_snd_error_to(NULL, NULL);
      /* now snd_error is back to normal */

      if (sp) 
	{
	  XtUnmanageChild(w);
	  select_channel(sp, 0); /* add_sound_window (snd-xsnd.c) -> make_file_info (snd-file) will report reason for error, if any */
	}
      else
	{
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

static file_dialog_info *open_dialog = NULL;

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
      open_dialog = make_file_dialog(read_only, title, select_title, file_open_ok_callback, open_file_help_callback);
      set_dialog_widget(FILE_OPEN_DIALOG, open_dialog->dialog);
      if (just_sounds(ss)) 
	{
	  XtVaSetValues(open_dialog->dialog, XmNfileSearchProc, sound_file_search, NULL);
	  open_dialog->fp->need_update = true;
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
  if (open_dialog->fp->new_file_written) 
    {
      force_directory_reread(open_dialog->dialog);
      open_dialog->fp->new_file_written = false;
    }
  if ((managed) && (!(XtIsManaged(open_dialog->dialog))))
    XtManageChild(open_dialog->dialog);
  return(open_dialog->dialog);
}



/* -------- File:Mix dialog -------- */

static void file_mix_ok_callback(Widget w, XtPointer context, XtPointer info)
{
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)info;
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename);
  file_dialog_stop_playing(fd);
  if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
    {
      int err;
      redirect_snd_error_to(file_open_error, (void *)fd);
      err = mix_complete_file_at_cursor(any_selected_sound(), filename, with_mix_tags(ss), 0);
      redirect_snd_error_to(NULL, NULL);
      if (err == 0) 
	XtUnmanageChild(w);
      else
	{
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

static void mix_file_help_callback (Widget w, XtPointer context, XtPointer info) 
{
  mix_file_dialog_help();
}

static file_dialog_info *mix_dialog = NULL;

widget_t make_mix_file_dialog(bool managed)
{
  /* called from the menu */
  if (mix_dialog == NULL)
    {
      mix_dialog = make_file_dialog(true, _("Mix"), _("mix in:"), file_mix_ok_callback, mix_file_help_callback);
      set_dialog_widget(FILE_MIX_DIALOG, mix_dialog->dialog);
      if (just_sounds(ss)) 
	{
	  XtVaSetValues(mix_dialog->dialog, XmNfileSearchProc, sound_file_search, NULL);
	  mix_dialog->fp->need_update = true;
	  force_directory_reread(mix_dialog->dialog);
	}
    }
  if ((managed) && (!XtIsManaged(mix_dialog->dialog)))
    XtManageChild(mix_dialog->dialog);
  return(mix_dialog->dialog);
}


/* -------- reflect outside changes -------- */

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
    open_dialog->fp->new_file_written = true;
  if (mix_dialog)
    mix_dialog->fp->new_file_written = true;
}

void reflect_just_sounds(void)
{
  if ((open_dialog) && (open_dialog->fp->just_sounds_button))
    XmToggleButtonSetState(open_dialog->fp->just_sounds_button, just_sounds(ss), true);
  if ((mix_dialog) && (mix_dialog->fp->just_sounds_button))
    XmToggleButtonSetState(mix_dialog->fp->just_sounds_button, just_sounds(ss), true);
}



/* -------- error handling -------- */

/* if an error occurs, a callback is added to the offending text widget, and an error is
 *   posted in the error_text label.  When the user modifies the bad entry, the callback
 *   erases the error message, and removes itself from the text widget.
 */

static void post_file_dialog_error(const char *error_msg, void *ufd)
{
  XmString msg;
  file_data *fd = (file_data *)ufd;
  msg = XmStringCreate((char *)error_msg, XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(fd->error_text, XmNlabelString, msg, NULL);
  XmStringFree(msg);
  if (!(XtIsManaged(fd->error_text))) 
    XtManageChild(fd->error_text);
}

static void filename_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  Widget dialog_filename_text;
  if (XtIsManaged(fd->error_text))
    XtUnmanageChild(fd->error_text);
  dialog_filename_text = XtNameToWidget(w, "Text");
  if (!dialog_filename_text) dialog_filename_text = XmFileSelectionBoxGetChild(fd->dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) XtRemoveCallback(dialog_filename_text, XmNmodifyVerifyCallback, filename_modify_callback, context);
  cbs->doit = true;
}

static void clear_error_if_filename_changes(Widget dialog, void *data)
{
  Widget dialog_filename_text;
  dialog_filename_text = XtNameToWidget(dialog, "Text");
  if (!dialog_filename_text) dialog_filename_text = XmFileSelectionBoxGetChild(dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) XtAddCallback(dialog_filename_text, XmNmodifyVerifyCallback, filename_modify_callback, (XtPointer)data);
}


/* -------- file data panel -------- */

static void chans_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  if (XtIsManaged(fd->error_text))
    XtUnmanageChild(fd->error_text);
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
  if (XtIsManaged(fd->error_text))
    XtUnmanageChild(fd->error_text);
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

#define NUM_HEADER_TYPES 7
static char *header_short_names[NUM_HEADER_TYPES] = {"sun  ", "aifc ", "wave ", "raw  ", "aiff ", "ircam", "nist "};

file_data *make_file_data_panel(Widget parent, char *name, Arg *in_args, int in_n, 
				dialog_channels_t with_chan, int header_type, int data_format,
				dialog_data_location_t with_loc, dialog_samples_t with_samples,
				dialog_error_t with_error, dialog_header_type_t with_header_type,
				dialog_comment_t with_comment)
{
  Widget mainform, form, sep1, hlab, hlist, dlab, dlist, slab, stext, clab, ctext = NULL, sep2 = NULL, sep3;
  Widget comment_label, comment_text = NULL, sep4, loclab, loctext = NULL, samplab, samptext;
  file_data *fdat;
  Arg args[32];
  int i, n;
  XmString *strs;
  int dformats = 0;
  char **formats = NULL;
  fdat = (file_data *)CALLOC(1, sizeof(file_data));
  fdat->current_type = header_type;
  fdat->current_format = data_format;
  fdat->dialog = parent;
  formats = set_header_positions_from_type(fdat, header_type, data_format);
  dformats = fdat->formats;
  mainform = XtCreateManagedWidget(name, xmFormWidgetClass, parent, in_args, in_n);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  form = XtCreateManagedWidget(name, xmFormWidgetClass, mainform, args, n);

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
      hlab = XtCreateManagedWidget(_("header type"), xmLabelWidgetClass, form, args, n);
      
      /* what is selected depends on current type */
      strs = (XmString *)CALLOC(NUM_HEADER_TYPES, sizeof(XmString)); 
      for (i = 0; i < NUM_HEADER_TYPES; i++) 
	strs[i] = XmStringCreate(header_short_names[i], XmFONTLIST_DEFAULT_TAG);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, hlab); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep1); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlistMarginWidth, 1); n++;
      XtSetArg(args[n], XmNuserData, (XtPointer)fdat); n++;
      XtSetArg(args[n], XmNitems, strs); n++;
      XtSetArg(args[n], XmNitemCount, NUM_HEADER_TYPES); n++;
      XtSetArg(args[n], XmNvisibleItemCount, NUM_VISIBLE_HEADERS); n++;
      hlist = XmCreateScrolledList(form, "header-type", args, n);
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
  dlab = XtCreateManagedWidget(_("data format"), xmLabelWidgetClass, form, args, n);

  n = 0;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, dlab); n++;
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
  dlist = XmCreateScrolledList(form, "data-format", args, n);

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

  if (with_chan != WITHOUT_CHANNELS_FIELD)
    {
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, stext); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      clab = XtCreateManagedWidget((with_chan == WITH_CHANNELS_FIELD) ? _("channels:") : _("extract channel:"), xmLabelWidgetClass, form, args, n);

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

      if (with_loc == WITH_DATA_LOCATION_FIELD)
	{
	  n = 0;
	  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, ctext); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, sep3); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  loclab = XtCreateManagedWidget(_("data location:"), xmLabelWidgetClass, form, args, n);

	  n = 0;
	  XtSetArg(args[n], XmNcolumns, 6); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, loclab); n++;
	  XtSetArg(args[n], XmNbottomAttachment, (with_samples == WITHOUT_SAMPLES_FIELD) ? XmATTACH_FORM : XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, sep3); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
	  loctext = make_textfield_widget("location-text", form, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
	  fdat->location_text = loctext;
	}
    }

  if (with_samples == WITH_SAMPLES_FIELD)
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
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
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
  XtSetArg(args[n], XmNbottomAttachment, ((with_comment == WITH_COMMENT_FIELD) || 
					  (with_error == WITH_ERROR_FIELD)) ? XmATTACH_NONE : XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNheight, 8); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep4 = XtCreateManagedWidget("sep4", xmSeparatorWidgetClass, mainform, args, n);

  if (with_comment == WITH_COMMENT_FIELD)
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
      XtSetArg(args[n], XmNbottomAttachment, (with_error == WITHOUT_ERROR_FIELD) ? XmATTACH_FORM : XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, comment_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrows, 2); n++;
#if (HAVE_OSS || HAVE_ALSA)
      XtSetArg(args[n], XmNcolumns, 40); n++;
      /* this pushes the right boundary over a ways -- otherwise the button holder box takes up all available space */
#endif
      comment_text = make_text_widget("comment-text", mainform, args, n);
    }
  fdat->comment_text = comment_text;

  if (with_error == WITH_ERROR_FIELD)
    {
      Widget esep;
      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, (with_comment == WITH_COMMENT_FIELD) ? comment_text : sep4); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 4); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      esep = XtCreateManagedWidget("esep", xmSeparatorWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, esep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->black); n++;
      XtSetArg(args[n], XmNborderWidth, 2); n++;
      XtSetArg(args[n], XmNmarginWidth, 10); n++;
      XtSetArg(args[n], XmNmarginHeight, 10); n++;
      fdat->error_text = XtCreateManagedWidget("", xmLabelWidgetClass, mainform, args, n);
      XtUnmanageChild(fdat->error_text);
    }

  return(fdat);
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


static file_data *save_as_file_data = NULL;
static Widget save_as_dialog = NULL, file_save_as_file_name;
static save_dialog_t save_as_dialog_type = FILE_SAVE_AS;

static void save_as_ok_callback(Widget w, XtPointer context, XtPointer info)
{ 
  char *str = NULL, *comment, *msg = NULL;
  snd_info *sp;
  int type, format, srate, chans;
  bool need_directory_update = false;
  off_t location, samples;
  if (XtIsManaged(save_as_file_data->error_text))
    XtUnmanageChild(save_as_file_data->error_text);

  /* TODO: should this remove any leftover error cleaning callbacks? */

  redirect_snd_error_to(post_file_panel_error, (void *)save_as_file_data);
  comment = get_file_dialog_sound_attributes(save_as_file_data, &srate, &chans, &type, &format, &location, &samples, 0);
  redirect_snd_error_to(NULL, NULL);
  if (save_as_file_data->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(save_as_dialog, (void *)save_as_file_data);
      if (comment) FREE(comment);
      return;
    }
  
  str = XmTextGetString(file_save_as_file_name);
  sp = any_selected_sound();
  clear_minibuffer(sp);
  if ((str) && (*str))
    {
      redirect_snd_error_to(post_file_dialog_error, (void *)save_as_file_data);
      msg = save_as_dialog_save_sound(sp, str, save_as_dialog_type, srate, type, format, comment, &need_directory_update);
      redirect_snd_error_to(NULL, NULL);
      if (msg)
	{
	  post_file_dialog_error((const char *)msg, (void *)save_as_file_data);
	  clear_error_if_filename_changes(save_as_dialog, (void *)save_as_file_data);
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
      post_file_dialog_error((const char *)msg, (void *)save_as_file_data);
      clear_error_if_filename_changes(save_as_dialog, (void *)save_as_file_data);
    }
  if (comment) FREE(comment);
} 

static void save_as_extract_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* TODO: merge save-as/extract code */
  char *str = NULL, *comment, *msg = NULL;
  snd_info *sp;
  int type, format, srate, chan = 0, err = 0;
  bool need_directory_update = false;
  off_t location, samples;
  if (XtIsManaged(save_as_file_data->error_text))
    XtUnmanageChild(save_as_file_data->error_text);

  redirect_snd_error_to(post_file_panel_error, (void *)save_as_file_data);
  comment = get_file_dialog_sound_attributes(save_as_file_data, &srate, &chan, &type, &format, &location, &samples, 0);
  redirect_snd_error_to(NULL, NULL);
  if (save_as_file_data->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(save_as_dialog, (void *)save_as_file_data);
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
      post_file_dialog_error((const char *)msg, (void *)save_as_file_data);
      clear_error_if_chans_changes(save_as_dialog, (void *)save_as_file_data);
      FREE(msg);
    }
  else
    {
      if ((!str) || (!*str))
	{
	  msg = _("not saved (no file name given)");
	  post_file_dialog_error((const char *)msg, (void *)save_as_file_data);
	  clear_error_if_filename_changes(save_as_dialog, (void *)save_as_file_data);
	}
      else
	{
	  msg = NULL;
	  err = MUS_NO_ERROR;
	  redirect_snd_error_to(post_file_dialog_error, (void *)save_as_file_data);
	  if (sp->nchans == 1)
	    msg = save_as_dialog_save_sound(sp, str, save_as_dialog_type, srate, type, format, comment, &need_directory_update);
	  else 
	    {
	      err = save_channel_edits(sp->chans[chan], str, AT_CURRENT_EDIT_POSITION); /* TODO: rationalize this! */
	      need_directory_update = true;
	    }
	  redirect_snd_error_to(NULL, NULL);
	  if ((msg) || (err != MUS_NO_ERROR))
	    {
	      post_file_dialog_error((const char *)msg, (void *)save_as_file_data);
	      clear_error_if_filename_changes(save_as_dialog, (void *)save_as_file_data);
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
	  XtFree(str);
	}
    }
  /* TODO: doc that extraction ignores srate etc? or fix this */
  /* PERHAPS: if srate is different, have a "perform src" button? */
  /* TODO: what about just-sounds here?  The file dir list currently lists every file! */
  /* TODO: gtk all dialogs */
  if (comment) FREE(comment);
}

static void save_as_cancel_callback(Widget w, XtPointer context, XtPointer info)
{ 
  XtUnmanageChild(save_as_dialog);
} 

static void save_as_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  save_as_dialog_help();
}

static void make_save_as_dialog(char *sound_name, int header_type, int format_type)
{
  char *file_string;
  if (!save_as_dialog)
    {
      Arg args[32];
      int n;
      XmString xmstr1, xmstr2, s1;
      Widget extractB;

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

      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNchildPlacement, XmPLACE_ABOVE_SELECTION); n++;
      XtSetArg(args[n], XmNallowOverlap, false); n++;
      XtSetArg(args[n], XmNheight, 600); n++;
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
      
      n = attach_all_sides(args, 0);
      save_as_file_data = make_file_data_panel(save_as_dialog, "data-form", args, n, 
					       WITH_EXTRACT_CHANNELS_FIELD, 
					       header_type, format_type, 
					       WITHOUT_DATA_LOCATION_FIELD, 
					       WITHOUT_SAMPLES_FIELD,
					       WITH_ERROR_FIELD, 
					       WITH_HEADER_TYPE_FIELD, 
					       WITH_COMMENT_FIELD);
      color_file_selection_box(save_as_dialog);
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(save_as_file_data->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(save_as_file_data->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	}

      /* this must come after the file data panel so that Motif puts it in the button box, not the main work area */
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
	  XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
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
  set_file_dialog_sound_attributes(save_as_file_data,
				   save_as_file_data->current_type,
				   save_as_file_data->current_format,
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
		      default_output_type(ss),
		      default_output_format(ss));
  set_file_dialog_sound_attributes(save_as_file_data,
				   save_as_file_data->current_type,
				   save_as_file_data->current_format,
				   selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   NULL);
  if ((managed) && (!XtIsManaged(save_as_dialog))) XtManageChild(save_as_dialog);
  return(save_as_dialog);
}


/* -------- save/restore for all these dialogs -------- */

void save_file_dialog_state(FILE *fd)
{
  if ((open_dialog) && (XtIsManaged(open_dialog->dialog)))
    {
      /* open_dialog->file_dialog_read_only -> "view-sound" dialog -- this distinction currently ignored */
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_open_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_open_file_dialog));
#endif
    }
  if ((mix_dialog) && (XtIsManaged(mix_dialog->dialog)))
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
static file_data *new_dialog_data = NULL;
static off_t initial_samples = 1;
static Widget new_file_name = NULL;

static void new_filename_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  if (XtIsManaged(fd->error_text))
    XtUnmanageChild(fd->error_text);
  XtRemoveCallback(new_file_name, XmNmodifyVerifyCallback, new_filename_modify_callback, context);
  cbs->doit = true;
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
      fprintf(stderr,"post error");
      msg = _("new sound needs a file name ('New file:' field is empty)");
      post_file_dialog_error((const char *)msg, (void *)new_dialog_data);
      clear_error_if_new_filename_changes(new_file_dialog, (void *)new_dialog_data);
    }
  else
    {
      redirect_snd_error_to(post_file_panel_error, (void *)new_dialog_data);
      comment = get_file_dialog_sound_attributes(new_dialog_data, &srate, &chans, &header_type, &data_format, &loc, &initial_samples, 1);
      redirect_snd_error_to(NULL, NULL);
      if (new_dialog_data->error_widget != NOT_A_SCANF_WIDGET)
	{
	  clear_error_if_panel_changes(new_file_dialog, (void *)new_dialog_data);
	}
      else
	{
	  snd_info *sp;
	  redirect_snd_error_to(post_file_dialog_error, (void *)new_dialog_data);
	  sp = snd_new_file(newer_name, header_type, data_format, srate, chans, comment, initial_samples);
	  redirect_snd_error_to(NULL, NULL);
	  if (!sp)
	    {
	      clear_error_if_new_filename_changes(new_file_dialog, (void *)new_dialog_data);
	    }
	  else
	    {
	      XtUnmanageChild(new_file_dialog);
	    }
	}
      XtFree(newer_name);
      if (comment) FREE(comment);
    }
}

static void new_file_default_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* TODO: load defaults, including name if it fits the format */
  new_file_ok_callback(w, context, info);
  XtUnmanageChild(w);
}

static void new_file_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(w);
}

static void new_file_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  new_file_dialog_help();
}

/* TODO: newname should be previous if nothing else overrides that */
/* similarly all other fields! -- perhaps a Default button to pick up defaults,
 *  including ticking the name -- otherwise leave it alone.
 */
void make_new_file_dialog(char *newname, int header_type, int data_format, int srate, int chans, char *comment)
{
  XmString titlestr;
  char *title;
  Widget sep, default_button;
  
  title = (char *)CALLOC(snd_strlen(newname) + 32, sizeof(char));
  sprintf(title, _("create new sound: %s"), newname);
  titlestr = XmStringCreate(title, XmFONTLIST_DEFAULT_TAG);
  FREE(title);

  if (!new_file_dialog)
    {
      Arg args[20];
      int n;
      XmString xok, xcancel, xhelp;
      Widget name_label, form;

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
      default_button = XtCreateManagedWidget(_("Default"), xmPushButtonGadgetClass, new_file_dialog, args, n);
      XtAddCallback(default_button, XmNactivateCallback, new_file_default_callback, NULL);

      n = attach_all_sides(args, 0);
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
      XtSetArg(args[n], XmNvalue, newname); n++;
      new_file_name = make_textfield_widget("newtext", form, args, n, NOT_ACTIVATABLE, add_completer_func(filename_completer));

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, new_file_name); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNheight, 12); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, form, args, n);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      new_dialog_data = make_file_data_panel(form, "data-form", args, n, 
					     WITH_CHANNELS_FIELD, 
					     default_output_type(ss), default_output_format(ss), 
					     WITHOUT_DATA_LOCATION_FIELD, 
					     WITH_SAMPLES_FIELD,
					     WITH_ERROR_FIELD, 
					     WITH_HEADER_TYPE_FIELD, 
					     WITH_COMMENT_FIELD);
      XtManageChild(new_dialog_data->error_text);
      XtManageChild(new_file_dialog);
      if (!(ss->using_schemes)) map_over_children(new_file_dialog, set_main_color_of_widget, NULL);
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(new_dialog_data->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(new_dialog_data->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
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
      XtUnmanageChild(new_dialog_data->error_text); /* TODO: find some way to get around this hidden label bug */
    }
  else 
    {
      XtVaSetValues(new_file_dialog, XmNdialogTitle, titlestr, NULL);
      XmStringFree(titlestr);
      XmTextSetString(new_file_name, newname);
    }

  set_file_dialog_sound_attributes(new_dialog_data, header_type, data_format, srate, chans, IGNORE_DATA_LOCATION, initial_samples, comment);
  if (!(XtIsManaged(new_file_dialog))) 
    XtManageChild(new_file_dialog);
}


/* ---------------- EDIT_HEADER ---------------- */

static Widget edit_header_dialog = NULL;
static file_data *edit_header_data;
static snd_info *edit_header_sp = NULL;

static void watch_read_only(struct snd_info *sp);

static XmString make_edit_header_dialog_title(snd_info *sp)
{
  char *str;
  XmString xstr;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (sp->read_only)
    {
      mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of %s (currently write-protected)"), sp->short_filename);
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

static void edit_header_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  XtUnmanageChild(edit_header_dialog);
  edit_header_sp->read_only_watcher = NULL;
}

#if DEBUGGING && HAVE_GUILE
static XEN g_apply_edit_header(void)
{
  if ((edit_header_sp) && (edit_header_sp->active))
    {
      if (!(edit_header_sp->read_only))
	edit_header_callback(edit_header_sp, edit_header_data, NULL, NULL);
      else snd_error(_("%s is write-protected"), edit_header_sp->short_filename);
    }
  XtUnmanageChild(edit_header_dialog);
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
      XtUnmanageChild(edit_header_data->error_text);
      title = make_edit_header_dialog_title(sp);
      XtVaSetValues(edit_header_dialog, XmNmessageString, title, NULL);
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
	  redirect_snd_error_to(post_file_dialog_error, (void *)edit_header_data);
	  ok = edit_header_callback(edit_header_sp, edit_header_data, post_file_dialog_error, post_file_panel_error);

	  fprintf(stderr,"edited: %d\n", ok);
	  redirect_snd_error_to(NULL, NULL);
	  if (edit_header_data->error_widget != NOT_A_SCANF_WIDGET)
	    {
	      clear_error_if_panel_changes(edit_header_dialog, (void *)edit_header_data);
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
}

Widget edit_header(snd_info *sp)
{
  file_info *hdr;
  XmString xstr4;
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
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
      n = attach_all_sides(args, n);
      edit_header_data = make_file_data_panel(edit_header_dialog, "Edit Header", args, n, 
					      WITH_CHANNELS_FIELD, 
					      hdr->type, 
					      hdr->format, 
					      WITH_DATA_LOCATION_FIELD, 
					      WITH_SAMPLES_FIELD,
					      WITH_ERROR_FIELD, 
					      WITH_HEADER_TYPE_FIELD, 
					      WITH_COMMENT_FIELD);
      set_file_dialog_sound_attributes(edit_header_data, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->samples, hdr->comment);
      XtManageChild(edit_header_data->error_text);
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
	  XtVaSetValues(edit_header_data->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	  XtVaSetValues(edit_header_data->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
	}
      set_dialog_widget(EDIT_HEADER_DIALOG, edit_header_dialog);
      XtUnmanageChild(edit_header_data->error_text);
    }
  else 
    {
      XtVaSetValues(edit_header_dialog, XmNmessageString, xstr4, NULL);
      set_file_dialog_sound_attributes(edit_header_data, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->samples, hdr->comment);
      raise_dialog(edit_header_dialog);
    }
  XmStringFree(xstr4);
  if (!(XtIsManaged(edit_header_dialog))) XtManageChild(edit_header_dialog);
  return(edit_header_dialog);
}

void save_edit_header_dialog_state(FILE *fd)
{
  if ((edit_header_dialog) && (XtIsManaged(edit_header_dialog)) && (snd_ok(edit_header_sp)))
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
static bool raw_cancelled = false;
static file_data *raw_dialog_data = NULL;

static void raw_data_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  int raw_srate, raw_chans, raw_data_format;
  raw_cancelled = false;

  redirect_snd_error_to(post_file_panel_error, (void *)raw_dialog_data);
  get_file_dialog_sound_attributes(raw_dialog_data, &raw_srate, &raw_chans, NULL, &raw_data_format, &raw_data_location, NULL, 1);
  redirect_snd_error_to(NULL, NULL);
  if (raw_dialog_data->error_widget != NOT_A_SCANF_WIDGET)
    clear_error_if_panel_changes(raw_data_dialog, (void *)raw_dialog_data);
  else
    {
      mus_header_set_raw_defaults(raw_srate, raw_chans, raw_data_format);
      XtUnmanageChild(raw_data_dialog);
    }
}

static void raw_data_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_cancelled = true;
  XtUnmanageChild(raw_data_dialog);
}

static void raw_data_default_callback(Widget w, XtPointer context, XtPointer info) 
{
  int raw_srate, raw_chans, raw_data_format;
  raw_data_location = 0;
  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */  
  set_file_dialog_sound_attributes(raw_dialog_data, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, raw_data_location, 
				   IGNORE_SAMPLES, NULL);
  /* TODO: if error_text active, unmanage */
  raw_data_ok_callback(w, context, info);
}

static void raw_data_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  raw_data_dialog_help();
}

static void make_raw_data_dialog(const char *filename)
{
  XmString xstr1, xstr2, xstr3, xstr4, titlestr;
  int n;
  int raw_srate, raw_chans, raw_data_format;
  char *str;
  Arg args[20];
  Widget default_button;

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

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  XtSetArg(args[n], XmNcancelLabelString, xstr1); n++;
  XtSetArg(args[n], XmNhelpLabelString, xstr2); n++;
  XtSetArg(args[n], XmNokLabelString, xstr3); n++;
  XtSetArg(args[n], XmNmessageString, xstr4); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
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
  default_button = XtCreateManagedWidget(_("Default"), xmPushButtonGadgetClass, raw_data_dialog, args, n);
  XtAddCallback(default_button, XmNactivateCallback, raw_data_default_callback, NULL);

  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */

  n = attach_all_sides(args, 0);
  raw_dialog_data = make_file_data_panel(raw_data_dialog, "data-form", args, n, 
					 WITH_CHANNELS_FIELD, 
					 MUS_RAW, raw_data_format, 
					 WITH_DATA_LOCATION_FIELD, 
					 WITHOUT_SAMPLES_FIELD,
					 WITH_ERROR_FIELD, 
					 WITHOUT_HEADER_TYPE_FIELD, 
					 WITHOUT_COMMENT_FIELD);
  set_file_dialog_sound_attributes(raw_dialog_data, 
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
      XtVaSetValues(default_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(raw_dialog_data->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
    }

  XtManageChild(raw_dialog_data->error_text); /* TODO: figure out why this is needed! */
  XtManageChild(raw_data_dialog);
  XtUnmanageChild(raw_dialog_data->error_text); 
  set_dialog_widget(RAW_DATA_DIALOG, raw_data_dialog);
}

file_info *raw_data_dialog_to_file_info(const char *filename, const char *title)
{
  /* put up dialog for srate, chans, data format */
  XmString xstr;
  int raw_srate, raw_data_format, raw_chans;
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

  if (raw_cancelled) return(NULL);
  reflect_raw_open_in_menu();
  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format);
  mus_sound_override_header(filename, raw_srate, raw_chans, raw_data_format, MUS_RAW, raw_data_location,
			    mus_bytes_to_samples(raw_data_format, 
						 mus_sound_length(filename) - raw_data_location));
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

      XtAddCallback(view_files_dialog, XmNcancelCallback, view_files_clear_callback, NULL);
      XtAddCallback(view_files_dialog, XmNhelpCallback, view_files_help_callback, NULL);
      XtAddCallback(view_files_dialog, XmNokCallback, view_files_dismiss_callback, NULL);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xclear);
      XmStringFree(titlestr);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->reset_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);
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



#if DEBUGGING && HAVE_GUILE
static XEN g_new_file_dialog(void)
{
  new_file_from_menu();
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

