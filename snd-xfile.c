#include "snd.h"

/* most the dialogs present a view of the various file header possibilities */

#define NUM_VISIBLE_HEADERS 4

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, int *location)
{
  char *str;
  int n;
  int res;
  int *ns = NULL;
  char *comment = NULL;
  if (fdat->srate_text) {str = XmTextGetString(fdat->srate_text); if (str) {(*srate) = string2int(str); XtFree(str);}}
  if (fdat->chans_text) {str = XmTextGetString(fdat->chans_text); if (str) {(*chans) = string2int(str); XtFree(str);}}
  if (fdat->location_text) {str = XmTextGetString(fdat->location_text); if (str) {(*location) = string2int(str); XtFree(str);}}
  if (fdat->comment_text) {comment = XmTextGetString(fdat->comment_text);}
  if (fdat->header_list)
    {
      res = XmListGetSelectedPos(fdat->header_list,&ns,&n);
      if (res == True)
	{
	  (*type) = header_type_from_position(ns[0]-1);
	  fdat->current_type = (*type);
	  free(ns); ns = NULL;
	}
    }
  if (fdat->format_list)
    {
      res = XmListGetSelectedPos(fdat->format_list,&ns,&n);
      if (res == True)
	{
	  (*format) = data_format_from_position(fdat->current_type,ns[0]-1);
	  fdat->current_format = (*format);
	  free(ns); ns = NULL;
	}
    }
  return(comment);
}

static void load_header_and_data_lists(file_data *fdat, int type, int format, int srate, int chans, int location, char *comment)
{
  int i;
  char **fl = NULL;
  char *str;
  XmString *strs;
  fdat->current_type = type;
  fdat->current_format = format;
  fl = set_header_and_data_positions(fdat,type,format); 
  XmListSelectPos(fdat->header_list,fdat->header_pos+1,FALSE);
  strs = (XmString *)CALLOC(fdat->formats,sizeof(XmString)); 
  for (i=0;i<fdat->formats;i++) strs[i] = XmStringCreate(fl[i],XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(fdat->format_list,XmNitems,strs,XmNitemCount,fdat->formats,NULL);
  for (i=0;i<fdat->formats;i++) XmStringFree(strs[i]);
  FREE(strs); 
  XmListSelectPos(fdat->format_list,fdat->format_pos+1,FALSE);
  if ((srate > 0) && (fdat->srate_text))
    {
      str = (char *)CALLOC(32,sizeof(char));
      sprintf(str,"%d",srate);
      XmTextSetString(fdat->srate_text,str);
    }
  if ((chans > 0) && (fdat->chans_text))
    {
      str = (char *)CALLOC(8,sizeof(char));
      sprintf(str,"%d",chans);
      XmTextSetString(fdat->chans_text,str);
    }
  if (fdat->comment_text) XmTextSetString(fdat->comment_text,comment);
  if ((location >= 0) && (fdat->location_text))
    {
      str = (char *)CALLOC(32,sizeof(char));
      sprintf(str,"%d",location);
      XmTextSetString(fdat->location_text,str);
    }
}

static void color_file_selection_box(Widget w, snd_state *ss)
{
  Widget wtmp = NULL,ftmp = NULL;
  if (!(ss->using_schemes)) 	
    {
      map_over_children(w,set_main_color_of_widget,(void *)ss);
      XtVaSetValues(XmFileSelectionBoxGetChild(w,XmDIALOG_DIR_LIST),XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      XtVaSetValues(XmFileSelectionBoxGetChild(w,XmDIALOG_LIST),XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
#ifndef LESSTIF_VERSION
      XtVaSetValues(XtNameToWidget(w,"Apply"),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XtNameToWidget(w,"Cancel"),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XtNameToWidget(w,"Help"),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XtNameToWidget(w,"OK"),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      wtmp = XtNameToWidget(w,"Text");
      if (!wtmp) wtmp = XmFileSelectionBoxGetChild(w,XmDIALOG_TEXT);
      ftmp = XtNameToWidget(w,"FilterText");
      if (!ftmp) ftmp = XmFileSelectionBoxGetChild(w,XmDIALOG_FILTER_TEXT);	
#else
      XtVaSetValues(XmFileSelectionBoxGetChild(w,XmDIALOG_APPLY_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XmFileSelectionBoxGetChild(w,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XmFileSelectionBoxGetChild(w,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XmFileSelectionBoxGetChild(w,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      wtmp = XmFileSelectionBoxGetChild(w,XmDIALOG_TEXT);
      ftmp = XmFileSelectionBoxGetChild(w,XmDIALOG_FILTER_TEXT);	
#endif
      if (wtmp)
	{
	  XtAddCallback(wtmp,XmNfocusCallback,textfield_focus_Callback,ss);
	  XtAddCallback(wtmp,XmNlosingFocusCallback,textfield_unfocus_Callback,ss);
	}
      if (ftmp)
	{
	  XtAddCallback(ftmp,XmNfocusCallback,textfield_focus_Callback,ss);
	  XtAddCallback(ftmp,XmNlosingFocusCallback,textfield_unfocus_Callback,ss);
	}
    }
}


/* -------- File Open Dialog -------- */

static void File_Open_OK_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_info *sp;
  snd_state *ss = (snd_state *)clientData;
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) callData;
  char *fileName;
  XtUnmanageChild(w);
  XmStringGetLtoR (cbs->value,XmFONTLIST_DEFAULT_TAG,&fileName);
  /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
  if (!(is_directory(fileName)))
    {
      sp = snd_open_file(fileName,ss);
      if (sp) select_channel(sp,0);           /* add_sound_window (snd-xsnd.c) will report reason for error, if any */
    }
  else
    snd_error(STR_is_a_directory,fileName);
}

static void File_Open_Help_Callback (Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
       "File Open",
"If you click the 'Sound Files Only'\n\
button, only those files in the current\n\
directory that look vaguely like sound\n\
files will be displayed.\n\
");
}

static void File_Open_Cancel_Callback (Widget w,XtPointer clientData,XtPointer callData) 
{
  XtUnmanageChild (w);
}

static Widget open_dialog = NULL; 
static XmSearchProc default_search_proc;

static int string_compare(const void *ss1, const void *ss2)
{
  return(strcmp((*((char **)ss1)),(*((char **)ss2)))); /* sweet baby jesus... ain't C grand? */
}

static void just_sounds_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "Sound Files Only",
"If you click the 'Sound Files Only'\n\
button, only those files in the current\n\
directory that look vaguely like sound\n\
files will be displayed.  The decision\n\
is based on the file's extension.\n\
");
}

static char *fullpathname = NULL;
static int file_SB_list_needs_update = 0; /* browser list needs update (new 'filter' or whatever) */
static int new_file_written = 0;          /* sound file list needs update because we wrote a new file */

void alert_new_file(void) {new_file_written = 1;}

static void sound_file_search(Widget FSB_w, XmFileSelectionBoxCallbackStruct *callData)
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
  char *pattern,*our_dir,*sp,*sn;
  static char *save_dir = NULL;
  static char *last_dir = NULL;
  static dir *sound_files,*current_files;
  static char *last_pattern = NULL;
  dir *cdp;
  XmFileSelectionBoxCallbackStruct *data = (XmFileSelectionBoxCallbackStruct *)callData;
  XmString *names = NULL;
  int i,filter_callback,need_update;

  XmStringGetLtoR (data->pattern,XmFONTLIST_DEFAULT_TAG,&pattern);
  XmStringGetLtoR (data->dir,XmFONTLIST_DEFAULT_TAG,&our_dir);

  if (!fullpathname) fullpathname = (char *)CALLOC(FILENAME_MAX,sizeof(char));
  filter_callback = (strcmp(pattern,"*") != 0);
  need_update = file_SB_list_needs_update;
  file_SB_list_needs_update = 0;
  if (!filter_callback)
    {
      if ((!last_dir) || (strcmp(our_dir,last_dir) != 0) || (new_file_written))
	{
	  if (current_files) current_files = free_dir(current_files);
	  if (last_dir) {FREE(last_dir); last_dir = NULL;}
	  last_dir = copy_string(our_dir);
	  strcpy(fullpathname,our_dir);
	  save_dir = (char *)(fullpathname+snd_strlen(our_dir));
	  sound_files = find_sound_files_in_dir(our_dir);
	  need_update = 1;
	}
      if (last_pattern)
	{
	  FREE(last_pattern);
	  last_pattern = NULL;
	}
      cdp = sound_files;
    }
  else 
    {
      if ((!last_pattern) || (strcmp(pattern,last_pattern) != 0) || (new_file_written))
	  {
	    if (last_pattern) {FREE(last_pattern); last_pattern = NULL;}
	    last_pattern = copy_string(pattern);
	    if (current_files)  current_files = free_dir(current_files);
	    if ((sound_files) && (sound_files->len > 0)) current_files = filter_sound_files(sound_files,pattern);
	    need_update = 1;
	  }
      cdp = current_files;
    }  
  new_file_written = 0;
  if (need_update)
    {
      if ((cdp) && (cdp->len > 0))
	{
	  qsort((void *)(cdp->files),cdp->len,sizeof(char *),string_compare);
	  names = (XmString *)CALLOC(cdp->len,sizeof(XmString));

#ifdef SGI
	  /* this is true only if the SGI "enhanced FSB" is in use, I hope */
	  if (!(XtNameToWidget(FSB_w,"Text"))) save_dir = fullpathname;
	  /* can't use SgDIALOG_FINDER here as suggested by SGI "Integration Guide" because
	   * XmFileSelectionBoxGetChild(FSB_w,SgDIALOG_FINDER)) generates an error if
	   * snd was loaded without -lSgm.
	   */
#endif

	  for (i=0;i<cdp->len;i++) 
	    {
	      for (sp=save_dir,sn=cdp->files[i];((*sp)=(*sn)) != '\0';sp++,sn++);
	      /* save_dir is a pointer into fullpathname after the directory portion */
	      names[i] = XmStringCreate(fullpathname,XmFONTLIST_DEFAULT_TAG);
	    }
	}
      else names=NULL;
      if (cdp) XtVaSetValues(FSB_w,XmNfileListItems,names,XmNfileListItemCount,cdp->len,XmNlistUpdated,TRUE,NULL);
      if (names)
	{
	  for (i=0;i<cdp->len;i++) if (names[i]) XmStringFree(names[i]);
	  FREE(names);
	}
    }
}

static void force_directory_reread(void)
{
  XmString dirmask;
  XtVaGetValues(open_dialog,XmNdirMask,&dirmask,NULL);
  XmFileSelectionDoSearch(open_dialog,dirmask);
  XmStringFree(dirmask);
}

static void just_sounds_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  if (cb->set)
    XtVaSetValues(open_dialog,XmNfileSearchProc,sound_file_search,NULL);
  else XtVaSetValues(open_dialog,XmNfileSearchProc,default_search_proc,NULL);
  file_SB_list_needs_update = 1;
  force_directory_reread();
}

static Widget just_sounds_button = NULL;
static int just_sounds_state = FALSE;

void toggle_just_sounds(int n)
{
  if (just_sounds_button)
    XmToggleButtonSetState(just_sounds_button,n,TRUE);
  just_sounds_state = n;
}

void CreateOpenDialog(Widget w,XtPointer clientData)
{
  /* file selection dialog box with added "Just Sound Files" toggle button */
  Arg args[20];
  int n;
  XmString s1;
  Widget wtmp = NULL;
  snd_state *ss = (snd_state *)clientData;
  n=0;
  if (!open_dialog)
    {
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      if (just_sounds_state)
	{
	  XtSetArg(args[n],XmNfileSearchProc,sound_file_search); n++;
	}
      s1 = XmStringCreate(STR_open_p,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNselectionLabelString,s1); n++;
      open_dialog = XmCreateFileSelectionDialog(w,STR_File,args,n);
      XmStringFree(s1);
#ifdef LESSTIF_VERSION
      XtManageChild(open_dialog);
#endif
#if OVERRIDE_TOGGLE
      override_form_translation(open_dialog);
#endif

      just_sounds_button = XtVaCreateManagedWidget(STR_Sound_Files_Only,xmToggleButtonWidgetClass,open_dialog,
						   XmNset,just_sounds_state,
						   XmNalignment,XmALIGNMENT_BEGINNING,
						   NULL);
      color_file_selection_box(open_dialog,ss);

#ifndef LESSTIF_VERSION
      wtmp = XtNameToWidget(open_dialog,"Text");
      if (!wtmp) wtmp = XmFileSelectionBoxGetChild(open_dialog,XmDIALOG_TEXT);
#else
      wtmp = XmFileSelectionBoxGetChild(open_dialog,XmDIALOG_TEXT);
#endif
      if (wtmp) add_completer_to_textfield(ss,wtmp,add_completer_func(filename_completer));
      
#ifndef LESSTIF_VERSION
      wtmp = XtNameToWidget(open_dialog,"FilterText");
      if (!wtmp) wtmp = XmFileSelectionBoxGetChild(open_dialog,XmDIALOG_FILTER_TEXT);
#else
      wtmp = XmFileSelectionBoxGetChild(open_dialog,XmDIALOG_FILTER_TEXT);
#endif
      if (wtmp) add_completer_to_textfield(ss,wtmp,add_completer_func(filename_completer));

      if (!(ss->using_schemes)) XtVaSetValues(just_sounds_button,XmNselectColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaGetValues(open_dialog,XmNfileSearchProc,&default_search_proc,NULL);
      XtAddCallback(open_dialog,XmNokCallback,File_Open_OK_Callback,clientData);
      XtAddCallback(open_dialog,XmNcancelCallback,File_Open_Cancel_Callback,clientData);
      XtAddCallback(open_dialog,XmNhelpCallback,File_Open_Help_Callback,clientData);
      XtAddCallback(just_sounds_button,XmNvalueChangedCallback,just_sounds_Callback,NULL);
      XtAddCallback(just_sounds_button,XmNhelpCallback,just_sounds_help_Callback,clientData);
    }
}

void make_open_file_dialog(snd_state *ss)
{
  finish_keyboard_selection();
  if (!open_dialog) CreateOpenDialog(MAIN_SHELL(ss),(XtPointer)ss);
  if (new_file_written) 
    {
      force_directory_reread();
      new_file_written = 0;
    }
  if (!(XtIsManaged(open_dialog))) XtManageChild(open_dialog);
}


/* -------- save as dialog (file and edit menus) -------- */
/* 
 * changed 19-June-97 to simply save the current state under the new name and return
 * to the current state/file (different from emacs) -- this keeps mix console intact
 * across backups and so on, and seems more useful to me than switching to the new file.
 */

static file_data *save_as_file_data = NULL;
static Widget save_as_dialog = NULL, file_save_as_file_name;
static int save_as_dialog_type = FILE_SAVE_AS;

static void save_as_ok_callback(Widget w,XtPointer clientData,XtPointer callData)
{ 
  snd_state *ss = (snd_state *)clientData;
  char *str = NULL,*comment;
  snd_info *sp;
  int type,format,srate;
  str = XmTextGetString(save_as_file_data->srate_text);
  srate = string2int(str);
  if (str) XtFree(str);
  comment = XmTextGetString(save_as_file_data->comment_text);
  type = save_as_file_data->current_type;
  format = save_as_file_data->current_format;
  str = XmTextGetString(file_save_as_file_name);
  sp = any_selected_sound(ss);
  if ((str) && (*str))
    {
      check_for_filename_collisions_and_save(ss,sp,str,save_as_dialog_type,srate,type,format,comment);
      XtFree(str);
    }
  else if (sp) report_in_minibuffer(sp,STR_not_saved_no_name_given);
  XtUnmanageChild(save_as_dialog);
} 

static void save_as_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  click_for_save_as_help((snd_state *)clientData);
}

static void save_as_cancel_callback(Widget w,XtPointer clientData,XtPointer callData)
{ 
  XtUnmanageChild(save_as_dialog);
} 

static void file_data_Type_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* this can be called from any header list created by sndCreateFileDataForm */
  int pos;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)callData;
  file_data *fd;
  XtVaGetValues(w,XmNuserData,&fd,NULL);
  pos = cbs->item_position-1;
  if (header_type_from_position(pos) != fd->current_type)
    {
      set_header_type_and_format_from_position(fd,pos);
      load_header_and_data_lists(fd,
				 fd->current_type,
				 fd->current_format,
				 0,0,-1,NULL);
    }
}

static void file_data_Format_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)callData;
  file_data *fd;
  XtVaGetValues(w,XmNuserData,&fd,NULL);
  fd->current_format = data_format_from_position(fd->current_type,cbs->item_position-1);
}

static void file_header_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "File Header Type",
"This list shows the output header types that\n\
Snd is willing to write.\n\
");
}

static void file_data_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "File Data Format",
"This list shows the data format choices\n\
available with the currently selected header\n\
choice.  'Short' means 16-bit two's complement\n\
integer.  'Mulaw' and 'Alaw' are common 8-bit\n\
compression schemes. 'Long' is 32-bit integer.\n\
'Float' is 32-bit float.  In each case, the\n\
decision as to byte order ('endianess') depends\n\
on the header type.\n\
");
}

static void file_data_location_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_help((snd_state *)clientData,
	   "File Data Location",
"If you know the data location (in bytes)\n\
you can set it in this field.\n\
");
}

static void file_srate_help_callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_help((snd_state *)clientData,
	   "File Srate",
"This field sets the nominal file sampling\n\
rate.\n\
");
} 

static void file_chans_help_callback(Widget w,XtPointer clientData,XtPointer callData)
{
  snd_help((snd_state *)clientData,
	   "File Srate",
"This field sets the number of channels\n\
in the output file.\n\
");
} 

static void file_comment_label_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  snd_help(ss,
	   "Output File Comment",
"This optional field provides any comments\n\
that you want saved in the output file's\n\
header.\n\
");	   
}

file_data *sndCreateFileDataForm(snd_state *ss, Widget parent, char *name, Arg *in_args, int in_n, int with_chan, int header_type, int data_format, int with_loc)
{
  Widget mainform,form,
    sep1,hlab,hlist,dlab,dlist,
    slab,stext,clab,ctext,sep2,sep3, 
    comment_label,comment_text,sep4,
    loclab,loctext;
  file_data *fdat;
  Arg args[32];
  int i,n;
  XmString *strs;
  int dformats = 0;
  char **formats = NULL;
  fdat = (file_data *)CALLOC(1,sizeof(file_data));
  fdat->current_type = header_type;
  fdat->current_format = data_format;
  formats = set_header_positions_from_type(fdat,header_type,data_format);
  dformats = fdat->formats;
  mainform = sndCreateFormWidget(name,parent,in_args,in_n);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  form = sndCreateFormWidget(name,mainform,args,n);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  XtSetArg(args[n],XmNwidth,5); n++;
  XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
  sep1 = XtCreateManagedWidget("sep1",xmSeparatorWidgetClass,form,args,n);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,sep1); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  hlab = XtCreateManagedWidget(STR_header,xmLabelWidgetClass,form,args,n);
  XtAddCallback(hlab,XmNhelpCallback,file_header_help_callback,ss);

  n=0;
#ifdef LESSTIF_VERSION
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#endif
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,hlab); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,sep1); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNlistMarginWidth,1); n++;
  XtSetArg(args[n],XmNuserData,(XtPointer)fdat); n++;
  /* what is selected depends on current type */
  strs = (XmString *)CALLOC(num_header_types(),sizeof(XmString)); 
  for (i=0;i<num_header_types();i++) strs[i] = XmStringCreate(header_short_name(i),XmFONTLIST_DEFAULT_TAG);
  XtSetArg(args[n],XmNitems,strs); n++;
  XtSetArg(args[n],XmNitemCount,num_header_types()); n++;
  XtSetArg(args[n],XmNvisibleItemCount,NUM_VISIBLE_HEADERS); n++;
  hlist = XmCreateScrolledList(form,"header type",args,n);
  XtAddCallback(hlist,XmNhelpCallback,file_header_help_callback,ss);
  XtManageChild(hlist);
  for (i=0;i<num_header_types();i++) XmStringFree(strs[i]);
  FREE(strs);
  XmListSelectPos(hlist,fdat->header_pos+1,FALSE);
  fdat->header_list = hlist;
  XtAddCallback(fdat->header_list,XmNbrowseSelectionCallback,file_data_Type_Callback,ss);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,hlist); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  XtSetArg(args[n],XmNwidth,15); n++;
  XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
  sep2 = XtCreateManagedWidget("sep2",xmSeparatorWidgetClass,form,args,n);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,sep2); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  dlab = XtCreateManagedWidget(STR_data,xmLabelWidgetClass,form,args,n);
  XtAddCallback(dlab,XmNhelpCallback,file_data_help_callback,ss);

  n=0;
#ifdef LESSTIF_VERSION
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#endif
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,dlab); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,sep2); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNuserData,(XtPointer)fdat); n++;
  dlist = XmCreateScrolledList(form,"data format",args,n);
  /* what is displayed and selected depends on current type */
  XtAddCallback(dlist,XmNhelpCallback,file_data_help_callback,ss);
  strs = (XmString *)CALLOC(dformats,sizeof(XmString)); 
  for (i=0;i<dformats;i++) strs[i] = XmStringCreate(formats[i],XmFONTLIST_DEFAULT_TAG);
  XtVaSetValues(dlist,XmNitems,strs,XmNitemCount,dformats,NULL);
  for (i=0;i<dformats;i++) XmStringFree(strs[i]);
  FREE(strs);
  XmListSelectPos(dlist,fdat->format_pos+1,FALSE);
  XtManageChild(dlist);
  fdat->format_list = dlist;
  XtAddCallback(fdat->format_list,XmNbrowseSelectionCallback,file_data_Format_Callback,ss);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
#ifndef LESSTIF_VERSION
  XtSetArg(args[n],XmNleftWidget,dlist); n++;
#else
  XtSetArg(args[n],XmNleftWidget,XtParent(dlist)); n++;
#endif
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  XtSetArg(args[n],XmNwidth,15); n++;
  XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
  sep3 = XtCreateManagedWidget("sep3",xmSeparatorWidgetClass,form,args,n);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,sep3); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  slab = XtCreateManagedWidget(STR_srate_p,xmLabelWidgetClass,form,args,n);
  XtAddCallback(slab,XmNhelpCallback,file_srate_help_callback,ss);

  n=0;
  XtSetArg(args[n],XmNcolumns,6); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,slab); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,sep3); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
  stext = sndCreateTextFieldWidget(ss,"text",form,args,n,NOT_ACTIVATABLE,add_completer_func(srate_completer));
  XtAddCallback(stext,XmNhelpCallback,file_srate_help_callback,ss);
  fdat->srate_text = stext;

  if (with_chan)
    {
      n=0;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,stext); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,sep3); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      clab = XtCreateManagedWidget(STR_chans_p,xmLabelWidgetClass,form,args,n);
      XtAddCallback(clab,XmNhelpCallback,file_chans_help_callback,ss);

      n=0;
      XtSetArg(args[n],XmNcolumns,6); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,clab); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,sep3); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
      ctext = sndCreateTextFieldWidget(ss,"text",form,args,n,NOT_ACTIVATABLE,NO_COMPLETER);
      XtAddCallback(ctext,XmNhelpCallback,file_chans_help_callback,ss);
      fdat->chans_text = ctext;

      if (with_loc)
	{
	  n=0;
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNtopWidget,ctext); n++;
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNleftWidget,sep3); n++;
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
	  loclab = XtCreateManagedWidget(STR_location_p,xmLabelWidgetClass,form,args,n);
	  XtAddCallback(loclab,XmNhelpCallback,file_data_location_help_callback,ss);

	  n=0;
	  XtSetArg(args[n],XmNcolumns,6); n++;
	  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNtopWidget,loclab); n++;
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
	  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	  XtSetArg(args[n],XmNleftWidget,sep3); n++;
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;	
	  loctext = sndCreateTextFieldWidget(ss,"text",form,args,n,NOT_ACTIVATABLE,NO_COMPLETER);
	  XtAddCallback(loctext,XmNhelpCallback,file_data_location_help_callback,ss);
	  fdat->location_text = loctext;
	}
    }

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,form); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNheight,5); n++;
  XtSetArg(args[n],XmNseparatorType,XmNO_LINE); n++;
  sep4 = XtCreateManagedWidget("sep4",xmSeparatorWidgetClass,mainform,args,n);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,sep4); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  comment_label = XtCreateManagedWidget(STR_comment_p,xmLabelWidgetClass,mainform,args,n);
  XtAddCallback(comment_label,XmNhelpCallback,file_comment_label_help_callback,ss);

  n=0;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,comment_label); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,comment_label); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrows,2); n++;
#if (HAVE_OSS || HAVE_ALSA)
  XtSetArg(args[n],XmNcolumns,40); n++;
  /* this pushes the right boundary over a ways -- otherwise the button holder box takes up all available space */
#endif
  comment_text = sndCreateTextWidget(ss,"comment",mainform,args,n);
  XtAddCallback(comment_text,XmNhelpCallback,file_comment_label_help_callback,ss);
  fdat->comment_text = comment_text;

  return(fdat);
}

static void make_save_as_dialog(snd_state *ss, char *sound_name, int save_type, int header_type, int format_type)
{
  /* save old as new, close old, open new */
  Arg args[32];
  int n;
  XmString xmstr1,xmstr2,s1;
  char *file_string;
  finish_keyboard_selection();
  if (!save_as_dialog)
    {
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      s1 = XmStringCreate(STR_save_as_p,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNselectionLabelString,s1); n++;
      xmstr1=XmStringCreate(STR_Save,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNokLabelString,xmstr1); n++;
      file_string = (char *)CALLOC(256,sizeof(char));
      sprintf(file_string,STR_saving,sound_name);
      xmstr2 = XmStringCreate(file_string,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNdialogTitle,xmstr2); n++;
#if RESIZE_DIALOG
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      XtSetArg(args[n],XmNautoUnmanage,FALSE); n++;
      XtSetArg(args[n],XmNchildPlacement,XmPLACE_ABOVE_SELECTION); n++;
      XtSetArg(args[n],XmNallowOverlap,FALSE); n++;
      save_as_dialog = XmCreateFileSelectionDialog(MAIN_SHELL(ss),"save-as",args,n);
#if OVERRIDE_TOGGLE
      override_form_translation(save_as_dialog);
#endif
      FREE(file_string);

      XtManageChild(save_as_dialog);
      XmStringFree(s1);
      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
#ifndef LESSTIF_VERSION
      file_save_as_file_name = XtNameToWidget(save_as_dialog,"Text");
      if (!(file_save_as_file_name)) file_save_as_file_name = XmFileSelectionBoxGetChild(save_as_dialog,XmDIALOG_TEXT);
#else
      file_save_as_file_name = XmFileSelectionBoxGetChild(save_as_dialog,XmDIALOG_TEXT);
#endif

      XtAddCallback(save_as_dialog,XmNhelpCallback,save_as_help_callback,ss);
      XtAddCallback(save_as_dialog,XmNcancelCallback,save_as_cancel_callback,ss);
      XtAddCallback(save_as_dialog,XmNokCallback,save_as_ok_callback,ss);
      
      n=0;
#ifdef LESSTIF_VERSION
      XtSetArg(args[n],XmNheight,100); n++;
#endif
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      save_as_file_data = sndCreateFileDataForm(ss,save_as_dialog,"data-form",args,n,FALSE,header_type,format_type,FALSE);

      color_file_selection_box(save_as_dialog,ss);
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(save_as_file_data->format_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
	  XtVaSetValues(save_as_file_data->header_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
	}
    }
  else
    {
      file_string = (char *)CALLOC(256,sizeof(char));
      sprintf(file_string,STR_saving,sound_name);
      xmstr2 = XmStringCreate(file_string,XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(save_as_dialog,XmNdialogTitle,xmstr2,NULL);
      XmStringFree(xmstr2);
      FREE(file_string);
    }
}

void make_file_save_as_dialog(snd_state *ss)
{
  snd_info *sp = NULL;
  file_info *hdr = NULL;
  save_as_dialog_type = FILE_SAVE_AS;
  sp = any_selected_sound(ss);
  if (sp) hdr = sp->hdr;
  make_save_as_dialog(ss,(sp) ? sp->shortname : "",FILE_SAVE_AS,
		      default_output_type(ss),
		      default_output_format(ss));

  load_header_and_data_lists(save_as_file_data,
			     save_as_file_data->current_type,
			     save_as_file_data->current_format,
			     (hdr) ? hdr->srate : region_srate(0),0,-1,output_comment(hdr));
  if (!XtIsManaged(save_as_dialog)) XtManageChild(save_as_dialog);
}

/* -------- edit save as dialog -------- */

void make_edit_save_as_dialog(snd_state *ss)
{
  save_as_dialog_type = EDIT_SAVE_AS;
  make_save_as_dialog(ss,STR_current_selection,EDIT_SAVE_AS,-1,-1);
  load_header_and_data_lists(save_as_file_data,
			     save_as_file_data->current_type,
			     save_as_file_data->current_format,
			     region_srate(0),0,-1,NULL);
  if (!XtIsManaged(save_as_dialog)) XtManageChild(save_as_dialog);
}




/* -------- files browser and regions list widgetry -------- */
/*
 * the region and file browsers share much widgetry -- they are supposed to look the same
 */

ww_info *make_title_row(snd_state *ss, Widget formw, char *first_str, char *second_str, char *main_str, int pad, int with_sort, int with_pane)
{
  int n;
  Arg args[32];
  Widget plw,svw,rlw,sep1;
  Widget smenu,sbar;
  ww_info *wwi;

  wwi = (ww_info *)CALLOC(1,sizeof(ww_info));
  
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_CENTER); n++;	
  rlw = XtCreateManagedWidget(main_str,xmLabelWidgetClass,formw,args,n);
      
  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,rlw); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNseparatorType,XmDOUBLE_LINE); n++;
  sep1 = XtCreateManagedWidget("sep",xmSeparatorWidgetClass,formw,args,n);
  wwi->dbline = sep1;

  if (with_pane == WITH_PANED_WINDOW)
    {
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,sep1); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNallowResize,TRUE); n++;
      wwi->panes = sndCreatePanedWindowWidget("panes",formw,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNpaneMinimum,40); n++;
      wwi->toppane = sndCreateFormWidget("toppane",wwi->panes,args,n);
      formw = wwi->toppane;
    }
  else 
    {
      wwi->panes = formw;
      wwi->toppane = formw;
    }

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  if (pad == PAD_TITLE_ON_LEFT)
    {
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_POSITION); n++;
      XtSetArg(args[n],XmNleftPosition,5); n++;
    }
  else
    {
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
    }
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  if (with_pane == WITH_PANED_WINDOW)
    {
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
    }
  else
    {
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,sep1); n++;
    }
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
  svw = XtCreateManagedWidget(first_str,xmLabelWidgetClass,formw,args,n);
  set_button_label_normal(svw,first_str);
  wwi->svw = svw;

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,svw); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  if (with_pane == WITH_PANED_WINDOW)
    {
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_NONE); n++;
    }
  else
    {
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,sep1); n++;
    }
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
  plw = XtCreateManagedWidget(second_str,xmLabelWidgetClass,formw,args,n);
  set_button_label_normal(plw,second_str);
  wwi->plw = plw;

  if (with_sort == WITH_SORT_BUTTON)
    {
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,sep1); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNshadowThickness,0); n++;
      XtSetArg(args[n],XmNhighlightThickness,0); n++;
      XtSetArg(args[n],XmNmarginHeight,0); n++;
      sbar = XmCreateMenuBar(formw,"menuBar",args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      smenu = XmCreatePulldownMenu(sbar,STR_sort,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNsubMenuId,smenu); n++;
      XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
      XtSetArg(args[n],XmNshadowThickness,0); n++;
      XtSetArg(args[n],XmNhighlightThickness,0); n++;
      XtSetArg(args[n],XmNmarginHeight,1); n++;
      XtCreateManagedWidget(STR_sort,xmCascadeButtonWidgetClass,sbar,args,n);
      
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XM_FONT_RESOURCE,BUTTON_FONT(ss)); n++;
      wwi->byname = XtCreateManagedWidget(STR_name,xmPushButtonWidgetClass,smenu,args,n);
      wwi->bydate = XtCreateManagedWidget(STR_date,xmPushButtonWidgetClass,smenu,args,n);
      wwi->bysize = XtCreateManagedWidget(STR_size,xmPushButtonWidgetClass,smenu,args,n);
      wwi->byentry = XtCreateManagedWidget(STR_entry,xmPushButtonWidgetClass,smenu,args,n);

      XtManageChild(sbar);
    }
  
  n=0;
  if (pad == PAD_TITLE_ON_LEFT) 
    {
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_POSITION); n++;
      XtSetArg(args[n],XmNleftPosition,5); n++;
    }
  else
    {
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
    }
  if (pad == PAD_TITLE_ON_RIGHT)
    {
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
      XtSetArg(args[n],XmNrightPosition,95); n++;
    }
  else
    {
      if (pad == PAD_TITLE_ON_LEFT)
	{
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	}
      else
	{
	  XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	  XtSetArg(args[n],XmNrightPosition,70); n++;
	}
    }
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,svw); n++;
  if (pad == DONT_PAD_TITLE)
    {
      if (with_pane == WITH_PANED_WINDOW)
	{
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	}
      else
	{
	  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
	  XtSetArg(args[n],XmNbottomPosition,40); n++;
	}
    }
  else
    {
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
    }
  XtSetArg(args[n],XmNscrollingPolicy,XmAUTOMATIC); n++;
  XtSetArg(args[n],XmNscrollBarDisplayPolicy,XmSTATIC); n++;
  wwi->list = XmCreateScrolledWindow(formw,"reglist",args,n);

  n=0;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  wwi->ww = sndCreateFormWidget("ww",wwi->list,args,n);
  XtVaSetValues(wwi->list,XmNworkWindow,wwi->ww,NULL);
  
  return(wwi);
}

regrow *make_regrow(snd_state *ss, Widget ww, Widget last_row, 
			   XtCallbackProc first_callback, XtCallbackProc second_callback, XtCallbackProc third_callback)
{
  int n;
  Arg args[32];
  regrow *r;
  XmString s1;
  s1 = XmStringCreate("",XmFONTLIST_DEFAULT_TAG);
  r = (regrow *)CALLOC(1,sizeof(regrow));

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,(last_row) ? XmATTACH_WIDGET : XmATTACH_FORM); n++;
  if (last_row) {XtSetArg(args[n],XmNtopWidget,last_row); n++;}
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
#if defined(LINUX) || defined(SCO5) || defined(UW2) || defined(SUN)
  XtSetArg(args[n],XmNheight,17); n++; 
#else
  XtSetArg(args[n],XmNheight,20); n++; 
#endif
  r->rw = XtCreateWidget("rw",xmFormWidgetClass,ww,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNselectColor,(ss->sgx)->pushed_button_color); n++;}
  XtSetArg(args[n],XmNlabelString,s1); n++;
  XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(first_callback,(XtPointer)r)); n++;
  if (ss->toggle_size > 0) {XtSetArg(args[n],XmNindicatorSize,ss->toggle_size); n++;}
  r->sv = sndCreateToggleButtonWidget("sv",r->rw,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,r->sv); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNselectColor,(ss->sgx)->pushed_button_color); n++;}
  XtSetArg(args[n],XmNlabelString,s1); n++;
  XtSetArg(args[n],XmNvalueChangedCallback,make_callback_list(second_callback,(XtPointer)r)); n++;
  if (ss->toggle_size > 0) {XtSetArg(args[n],XmNindicatorSize,ss->toggle_size); n++;}
  r->pl = sndCreateToggleButtonWidget("pl",r->rw,args,n);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->highlight_color); n++;}
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,r->pl); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNshadowThickness,0); n++;
  XtSetArg(args[n],XmNhighlightThickness,0); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
  XtSetArg(args[n],XmNfillOnArm,FALSE); n++;
  XtSetArg(args[n],XM_FONT_RESOURCE,BOLD_BUTTON_FONT(ss)); n++;
  XtSetArg(args[n],XmNrecomputeSize,FALSE); n++;
  XtSetArg(args[n],XmNwidth,300); n++;
  XtSetArg(args[n],XmNactivateCallback,make_callback_list(third_callback,(XtPointer)r)); n++;
  r->nm = XtCreateManagedWidget("nm",xmPushButtonWidgetClass,r->rw,args,n);
  XmStringFree(s1);
  return(r);
}

static void make_row_name(regrow *r, char *name, int big_star)
{
  char *str;
  if (big_star)
    {
      str = (char *)CALLOC(128,sizeof(char));
      sprintf(str,"%s*",name);
      set_button_label_bold(r->nm,str);
      FREE(str);
    }
  else set_button_label_bold(r->nm,name);
}

static void fill_in_row(regrow *r, char *name, int big_star)
{
  XtUnmanageChild(r->rw);
  make_row_name(r,name,big_star);
  XmToggleButtonSetState(r->sv,FALSE,FALSE);
  XmToggleButtonSetState(r->pl,FALSE,FALSE);
  XtManageChild(r->rw);
}


/* -------- view files dialog -------- */

static Widget view_files_dialog = NULL;
static int vf_selected_file = -1;
static Widget vf_curww,vf_prevlst,vf_curlst,vf_prevww;

static void make_prevfiles_list (snd_state *ss);
static void make_curfiles_list (snd_state *ss);

static regrow **cur_name_row = NULL;
static regrow **prev_name_row = NULL;

void add_files_to_prevlist(snd_state *ss, char **shortnames, char **longnames, int len)
{
  int i;
  for (i=0;i<len;i++) file_prevlist(shortnames[i],longnames[i]);
  if ((view_files_dialog) && (XtIsManaged(view_files_dialog))) make_prevfiles_list(ss);
}

void update_prevfiles(snd_state *ss)
{
  if ((view_files_dialog) && (XtIsManaged(view_files_dialog))) make_prevfiles_list(ss);
}

void add_directory_to_prevlist(snd_state *ss, char *dirname)
{
  add_directory_to_prevlist_1(ss,dirname);
  if ((view_files_dialog) && (XtIsManaged(view_files_dialog))) make_prevfiles_list(ss);
}

void remember_me(snd_state *ss, char *shortname,char *fullname)
{
  file_prevlist(shortname,fullname);
  file_uncurlist(shortname);
  if ((view_files_dialog) && (XtIsManaged(view_files_dialog)))
    {
      make_curfiles_list(ss);
      make_prevfiles_list(ss);
    }
}

void make_cur_name_row(int old_size, int new_size)
{
  int i;
  if (cur_name_row == NULL)
    cur_name_row = (regrow **)CALLOC(new_size,sizeof(regrow *));
  else 
    {
      cur_name_row = (regrow **)REALLOC(cur_name_row,new_size * sizeof(regrow *));
      for (i=old_size;i<new_size;i++) cur_name_row[i] = NULL;
    }
}

void make_prev_name_row(int old_size, int new_size)
{
  int i;
  if (prev_name_row == NULL)
    prev_name_row = (regrow **)CALLOC(new_size,sizeof(regrow *));
  else 
    {
      prev_name_row = (regrow **)REALLOC(prev_name_row, new_size * sizeof(regrow *));
      for (i=old_size;i<new_size;i++) prev_name_row[i] = NULL;
    }
}

void greet_me(snd_state *ss, char *shortname)
{
  file_curlist(shortname);
  file_unprevlist(shortname);
  if ((view_files_dialog) && (XtIsManaged(view_files_dialog)))
    {
      make_curfiles_list(ss);
      make_prevfiles_list(ss);
    }
}

void make_a_big_star_outa_me(char *shortname, int big_star)
{
  int i;
  regrow *r;
  i = find_curfile_regrow(shortname);
  if ((i != -1) && (get_a_big_star(i) != big_star))
    {
      if ((view_files_dialog) && (XtIsManaged(view_files_dialog)))
	{
	  r = cur_name_row[i];
	  make_row_name(r,get_curnames(i),big_star);
	}
      set_a_big_star(i,big_star);
    }
}

static void View_Files_Help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  view_files_dialog_help((snd_state *)clientData);
}

static void View_Files_Dismiss_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XtUnmanageChild(view_files_dialog);
}

static void View_Files_Clear_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* clear previous files list and associated widget list */
  clear_prevlist();
  make_prevfiles_list((snd_state *)clientData);
}

static void View_Files_Update_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* run through previous files list looking for any that have been deleted behind our back */
  update_prevlist();
  make_prevfiles_list((snd_state *)clientData);
}

static void View_CurFiles_Save_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  regrow *r = (regrow *)clientData;
  snd_info *sp;
  sp = find_sound(r->ss,get_curnames(r->pos));
  if (sp)
    {
      finish_keyboard_selection();
      save_edits(sp,NULL);
    }
  XmToggleButtonSetState(r->sv,FALSE,FALSE);
}

void set_file_browser_play_button(char *name, int state)
{
  int i,list;
  regrow *r;
  list = 0;
  if ((view_files_dialog) && (XtIsManaged(view_files_dialog)))
    {
      i=find_curfile_regrow(name); 
      if (i != -1) list = 1; else i=find_prevfile_regrow(name);
      if (i != -1)
	{
	  if (list) r=cur_name_row[i]; else r=prev_name_row[i];
	  XmToggleButtonSetState(r->pl,state,FALSE);
	}
    }
}

static void View_CurFiles_Play_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  regrow *r = (regrow *)clientData;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  snd_info *sp;
  sp = find_sound(r->ss,get_curnames(r->pos));
  if (sp)
    {
      if (sp->playing) stop_playing_sound(sp);
      if (cb->set)
	{
	  start_playing(sp,0);
	  set_play_button(sp,1);
	}
      else set_play_button(sp,0);
    }
}

static void curfile_unhighlight(snd_state *ss)
{
  regrow *r;
  if ((view_files_dialog) && (XtIsManaged(view_files_dialog)))
    {
      if ((!(ss->using_schemes)) && (vf_selected_file != -1))
	{
	  r = cur_name_row[vf_selected_file];
	  XtVaSetValues(r->rw,XmNbackground,(ss->sgx)->highlight_color,NULL);
	  XtVaSetValues(r->nm,XmNbackground,(ss->sgx)->highlight_color,NULL);
	  vf_selected_file = -1;
	}
    }
}

static void curfile_highlight(snd_state *ss, int i)
{
  regrow *r;
  if ((view_files_dialog) && (XtIsManaged(view_files_dialog)))
    {
      if (!(ss->using_schemes)) 
	{
	  if (vf_selected_file != -1) curfile_unhighlight(ss);
	  r = cur_name_row[i];
	  XtVaSetValues(r->rw,XmNbackground,(ss->sgx)->zoom_color,NULL);
	  XtVaSetValues(r->nm,XmNbackground,(ss->sgx)->zoom_color,NULL);
	  vf_selected_file = i;
	}
    }
}

static void View_CurFiles_Select_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  regrow *r = (regrow *)clientData;
  snd_info *sp,*osp;
  snd_state *ss;
  ss = r->ss;
  curfile_highlight(ss,r->pos);
  sp = find_sound(ss,get_curnames(r->pos));
  osp = any_selected_sound(ss);
  if (sp != osp)
    {
      select_channel(sp,0);
      normalize_sound(ss,sp,osp,sp->chans[0]);
      /* goto_graph(sp->chans[0]); */
    }
}

static void View_PrevFiles_Unlist_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  regrow *r = (regrow *)clientData;
  file_unprevlist(get_prevnames(r->pos));
  make_prevfiles_list(r->ss);
}

static void View_PrevFiles_Play_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* open and play -- close at end or when button off toggled */
  regrow *r = (regrow *)clientData;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)callData;
  static snd_info *play_sp;
  int play;
  play = cb->set;
  if (play)
    {
      if (play_sp)
	{
	  if (play_sp->playing) {XmToggleButtonSetState(w,FALSE,FALSE); return;} /* can't play two of these at once */
	  if (strcmp(play_sp->shortname,get_prevnames(r->pos)) != 0)
	    {
	      completely_free_snd_info(play_sp);
	      play_sp = NULL;
	    }
	}
      if (!play_sp) play_sp = make_sound_readable(r->ss,get_prevfullnames(r->pos),FALSE);
      if (play_sp)
	{
	  play_sp->shortname = get_prevnames(r->pos);
	  play_sp->fullname = NULL;
	  start_playing(play_sp,0);
	}
      else
	{ /* can't find or setup file */
	  XmToggleButtonSetState(w,FALSE,FALSE);
	  play = 0;
	}
    }
  else
    { /* play toggled off */
      if ((play_sp) && (play_sp->playing)) stop_playing_sound(play_sp);
    }
}

static void View_PrevFiles_Select_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  /* open and set as selected */
  regrow *r = (regrow *)clientData;
  snd_info *sp;
  sp = snd_open_file(get_prevfullnames(r->pos),r->ss);
  if (sp) select_channel(sp,0); 
}

void highlight_selected_sound(snd_state *ss)
{
  snd_info *sp;
  int i;
  sp = selected_sound(ss);
  if (sp)
    {
      i = find_curfile_regrow(sp->shortname);
      if (i != -1) curfile_highlight(ss,i); else curfile_unhighlight(ss);
    }
  else curfile_unhighlight(ss);
}

static void make_curfiles_list (snd_state *ss)
{
  int i;
  Widget last_row = NULL;
  regrow *r;
  for (i=0;i<get_curfile_end();i++)
    {
      r = cur_name_row[i];
      if (r == NULL)
	{
	  r = make_regrow(ss,vf_curww,last_row,View_CurFiles_Save_Callback,View_CurFiles_Play_Callback,View_CurFiles_Select_Callback);
	  cur_name_row[i] = r;
	  r->pos = i;
	  r->ss = ss;
	}
      fill_in_row(r,get_curnames(i),get_a_big_star(i));
      last_row = r->rw;
    }
  for (i=get_curfile_end();i<get_max_curfile_end();i++)
    {
      if ((r = cur_name_row[i]))
	{
	  if (XtIsManaged(r->rw)) XtUnmanageChild(r->rw);
	}
    }
  set_max_curfile_end(get_curfile_end());
  highlight_selected_sound(ss);
  XtManageChild(vf_curlst);
}

static void sort_prevfiles_by_name(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  set_previous_files_sort(ss,1);
  make_prevfiles_list(ss);
}

static void sort_prevfiles_by_date(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  set_previous_files_sort(ss,2);
  make_prevfiles_list(ss);
}

static void sort_prevfiles_by_size(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  set_previous_files_sort(ss,3);
  make_prevfiles_list(ss);
}

static void sort_prevfiles_by_entry_order(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  set_previous_files_sort(ss,4);
  make_prevfiles_list(ss);
}

static void make_prevfiles_list (snd_state *ss)
{
  int i;
  Widget last_row = NULL;
  regrow *r;
  if (get_prevfile_end() >= 0)
    {
      make_prevfiles_list_1(ss);
      for (i=0;i<=get_prevfile_end();i++)
	{
	  if (!((r = prev_name_row[i])))
	    {
	      r = make_regrow(ss,vf_prevww,last_row,View_PrevFiles_Unlist_Callback,View_PrevFiles_Play_Callback,View_PrevFiles_Select_Callback);
	      prev_name_row[i] = r;
	      r->pos = i;
	      r->ss = ss;
	    }
	  fill_in_row(r,get_prevnames(i),0);
	  last_row = r->rw;
	}
    }
  for (i=get_prevfile_end()+1;i<=get_max_prevfile_end();i++)
    {
      if ((r = prev_name_row[i]))
	{
	  if (XtIsManaged(r->rw)) XtUnmanageChild(r->rw);
	}
    }
  set_max_prevfile_end(get_prevfile_end());
  if (!(XtIsManaged(vf_prevlst))) XtManageChild(vf_prevlst);
}


/* play open unlist for prevfile, play save select for curfile, preload process for prevfile (snd-clm) */

void View_Files_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  /* fire up a dialog window with a list of currently open files, 
   * currently selected file also selected in list --
   * if user selects one (browse mode), so does Snd (via normalize_sound etc)
   * use snd_info label as is (short-form with '*' etc)
   * secondary list of previously edited files (if still in existence) --
   * click here re-opens the file.  (The overall form is similar to the regions browser).
   * The previous files list requires that we keep such a list as we go along, on the
   * off-chance this browser will be fired up.  (Such files may be subsequently moved or deleted).
   */
  snd_state *ss = (snd_state *)clientData;
  int n;
  Arg args[20];
  ww_info *wwl;
  regrow *r;
  XmString xdismiss,xhelp,xclear,titlestr;
  Widget mainform,curform,prevform,updateB,sep;
  if (!view_files_dialog)
    {
      vf_selected_file = -1;
      xdismiss = XmStringCreate(STR_Dismiss,XmFONTLIST_DEFAULT_TAG);
      xhelp = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
      xclear = XmStringCreate(STR_Clear,XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Files,XmFONTLIST_DEFAULT_TAG);
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNcancelLabelString,xclear); n++;
      XtSetArg(args[n],XmNhelpLabelString,xhelp); n++;
      XtSetArg(args[n],XmNokLabelString,xdismiss); n++;
      XtSetArg(args[n],XmNautoUnmanage,FALSE); n++;
      XtSetArg(args[n],XmNdialogTitle,titlestr); n++;
#if RESIZE_DIALOG
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      XtSetArg(args[n],XmNtransient,FALSE); n++;
      view_files_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss),STR_File_Browser,args,n);
      add_dialog(ss,view_files_dialog);
#if OVERRIDE_TOGGLE
      override_form_translation(view_files_dialog);
#endif

      XtAddCallback(view_files_dialog,XmNcancelCallback,View_Files_Clear_Callback,ss);
      XtAddCallback(view_files_dialog,XmNhelpCallback,View_Files_Help_Callback,ss);
      XtAddCallback(view_files_dialog,XmNokCallback,View_Files_Dismiss_Callback,ss);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xclear);
      XmStringFree(titlestr);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(view_files_dialog,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	}

      n=0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;
	  XtSetArg(args[n],XmNarmColor,(ss->sgx)->pushed_button_color); n++;
	}
      updateB = XtCreateManagedWidget(STR_Update,xmPushButtonWidgetClass,view_files_dialog,args,n);
      XtAddCallback(updateB,XmNactivateCallback,View_Files_Update_Callback,ss);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNbottomWidget,XmMessageBoxGetChild(view_files_dialog,XmDIALOG_SEPARATOR)); n++;
      mainform = sndCreateFormWidget("formd",view_files_dialog,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
      XtSetArg(args[n],XmNrightPosition,49); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      curform = sndCreateFormWidget("curform",mainform,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,curform); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
      XtSetArg(args[n],XmNseparatorType,XmSHADOW_ETCHED_IN); n++;
      sep = XtCreateManagedWidget("sep",xmSeparatorWidgetClass,mainform,args,n);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,sep); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      prevform = sndCreateFormWidget("prevform",mainform,args,n);


      /* current files section: save play current files | files */
      wwl = make_title_row(ss,curform,STR_save,STR_play,STR_current_files,
			   PAD_TITLE_ON_RIGHT,WITHOUT_SORT_BUTTON,WITHOUT_PANED_WINDOW);
      vf_curww = wwl->ww;
      vf_curlst = wwl->list;
      if (!(ss->using_schemes)) map_over_children(vf_curlst,set_main_color_of_widget,(void *)clientData);
      FREE(wwl); 
      wwl=NULL;
      if (get_curfile_size() == 0) /* apparently we need at least one row to get Motif to allocate the outer widgets correctly */
	{                    /* not curfile_end here since it is tracking currently active files before this dialog is created */
	  init_curfiles(4);
	  cur_name_row = (regrow **)CALLOC(4,sizeof(regrow *));
	  r = make_regrow(ss,vf_curww,NULL,View_CurFiles_Save_Callback,View_CurFiles_Play_Callback,View_CurFiles_Select_Callback);
	  cur_name_row[0] = r;
	  r->pos = 0;
	  r->ss = ss;
	}

      /* previous files section: unlist play previous files | files */
      wwl = make_title_row(ss,prevform,STR_unlist,STR_play,STR_previous_files,
			   PAD_TITLE_ON_LEFT,WITH_SORT_BUTTON,WITHOUT_PANED_WINDOW);
      XtAddCallback(wwl->byname,XmNactivateCallback,sort_prevfiles_by_name,ss);
      XtAddCallback(wwl->bydate,XmNactivateCallback,sort_prevfiles_by_date,ss);
      XtAddCallback(wwl->bysize,XmNactivateCallback,sort_prevfiles_by_size,ss);
      XtAddCallback(wwl->byentry,XmNactivateCallback,sort_prevfiles_by_entry_order,ss);

      vf_prevww = wwl->ww;
      vf_prevlst = wwl->list;
      if (!(ss->using_schemes)) map_over_children(vf_prevlst,set_main_color_of_widget,(void *)clientData);
      FREE(wwl); 
      wwl=NULL;
      if (get_prevfile_size() == 0)
	{
	  init_prevfiles(4);
	  prev_name_row = (regrow **)CALLOC(4,sizeof(regrow *));
	  r = make_regrow(ss,vf_prevww,NULL,View_PrevFiles_Unlist_Callback,View_PrevFiles_Play_Callback,View_PrevFiles_Select_Callback);
	  prev_name_row[0] = r;
	  r->pos = 0;
	  r->ss = ss;
	}
    }
  else raise_dialog(view_files_dialog);
  make_curfiles_list(ss);
  make_prevfiles_list(ss);
  if (!XtIsManaged(view_files_dialog)) XtManageChild(view_files_dialog);
  highlight_selected_sound(ss);
}

void start_file_dialog(snd_state *ss, int width, int height)
{
  View_Files_Callback(NULL,(XtPointer)ss,NULL);
  if (width > 0) XtVaSetValues(view_files_dialog,XmNwidth,(Dimension)width,XmNheight,(Dimension)height,NULL);
}

int file_dialog_is_active(void)
{
  return((view_files_dialog) && (XtIsManaged(view_files_dialog)));
}



/* -------------------------------- Raw Data Dialog -------------------------------- */

static Widget raw_data_dialog = NULL;
static Widget raw_srate_text,raw_chans_text,raw_location_text;
static int raw_data_location = 0;
static int raw_cancelled = 0;

static void raw_data_ok_Callback(Widget w,XtPointer clientData,XtPointer callData) {raw_cancelled = 0;}
static void raw_data_cancel_Callback(Widget w,XtPointer clientData,XtPointer callData) {raw_cancelled = 1;}

static void raw_data_default_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  set_use_raw_defaults(ss,1);
  raw_data_ok_Callback(w,clientData,callData);
}

static void raw_data_browse_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss = (snd_state *)clientData;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)callData;
  set_raw_format(ss,cbs->item_position);
}

static void raw_data_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  raw_data_dialog_help((snd_state *)clientData);
}

static char dfs_str[8];
static char dfc_str[4];

static void make_raw_data_dialog(char *filename, snd_state *ss)
{
  XmString *formats;
  XmString xstr1,xstr2,xstr3,xstr4,titlestr;
  int i,n;
  char *str;
  Arg args[20];
  Widget lst,defw,dls,dfs,dfc,rform,dlab,dloc,dloclab,chnlab;
  n=0;
  xstr1 = XmStringCreate(STR_Cancel,XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
  xstr2 = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
  xstr3 = XmStringCreate(STR_Ok,XmFONTLIST_DEFAULT_TAG);
  titlestr = XmStringCreate(STR_No_Header_on_File,XmFONTLIST_DEFAULT_TAG);
  str = (char *)CALLOC(256,sizeof(char));
  sprintf(str,STR_No_header_found_for,filename_without_home_directory(filename));
  xstr4 = XmStringCreate(str,XmFONTLIST_DEFAULT_TAG);
  FREE(str);
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  XtSetArg(args[n],XmNcancelLabelString,xstr1); n++;
  XtSetArg(args[n],XmNhelpLabelString,xstr2); n++;
  XtSetArg(args[n],XmNokLabelString,xstr3); n++;
  XtSetArg(args[n],XmNmessageString,xstr4); n++;
  XtSetArg(args[n],XmNdialogTitle,titlestr); n++;
#if RESIZE_DIALOG
  XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
  XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
  /* not transient -- we want this window to remain visible if possible */
  raw_data_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss),STR_raw_data,args,n);
#if OVERRIDE_TOGGLE
  override_form_translation(raw_data_dialog);
#endif

  XtAddCallback(raw_data_dialog,XmNcancelCallback,raw_data_cancel_Callback,ss);
  XtAddCallback(raw_data_dialog,XmNhelpCallback,raw_data_help_Callback,ss);
  XtAddCallback(raw_data_dialog,XmNokCallback,raw_data_ok_Callback,ss);
  XmStringFree(xstr1);
  XmStringFree(xstr2);
  XmStringFree(xstr3);
  XmStringFree(xstr4);
  XmStringFree(titlestr);

  n=0;
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
  defw = XtCreateManagedWidget(STR_Default,xmPushButtonWidgetClass,raw_data_dialog,args,n);
  XtAddCallback(defw,XmNactivateCallback,raw_data_default_Callback,ss);

  rform = sndCreateFormWidget("sretc",raw_data_dialog,NULL,0);

  n=0;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNmarginTop,6); n++;
  dls = XtCreateManagedWidget(STR_srate_p,xmLabelWidgetClass,rform,args,n);

  n=0;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,dls); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNcolumns,6); n++;
  XtSetArg(args[n],XmNresizeWidth,FALSE); n++;
  dfs = sndCreateTextFieldWidget(ss,"text",rform,args,n,NOT_ACTIVATABLE,add_completer_func(srate_completer));
  if (raw_srate(ss) < 100000) sprintf(dfs_str," %d",raw_srate(ss)); else sprintf(dfs_str,"%d",raw_srate(ss));
  XmTextSetString(dfs,dfs_str);

  n=0;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,dfs); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_END); n++;	
  XtSetArg(args[n],XmNmarginTop,6); n++;
  chnlab = XtCreateManagedWidget(STR_chans_p,xmLabelWidgetClass,rform,args,n);

  n=0;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,chnlab); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNcolumns,3); n++;
  XtSetArg(args[n],XmNresizeWidth,FALSE); n++;
  dfc = sndCreateTextFieldWidget(ss,"text",rform,args,n,NOT_ACTIVATABLE,NO_COMPLETER);
  if (raw_chans(ss) < 10) sprintf(dfc_str,"  %d",raw_chans(ss)); else sprintf(dfc_str," %d",raw_chans(ss));
  XmTextSetString(dfc,dfc_str);
  
  n=0;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,dfs); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNalignment,XmALIGNMENT_END); n++;
  XtSetArg(args[n],XmNmarginTop,6); n++;
  dloclab = XtCreateManagedWidget(STR_data_location_p,xmLabelWidgetClass,rform,args,n);
  
  n=0;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNleftWidget,dloclab); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,dfs); n++;
  XtSetArg(args[n],XmNcolumns,8); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  dloc = sndCreateTextFieldWidget(ss,"text",rform,args,n,NOT_ACTIVATABLE,NO_COMPLETER);
  XmTextSetString(dloc,"0");

  n=0;
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,dloc); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
  dlab = XtCreateManagedWidget(STR_data_format_p,xmLabelWidgetClass,rform,args,n);

  n=0;
#ifdef LESSTIF_VERSION
  if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->white); n++;}
#endif
  XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
  XtSetArg(args[n],XmNtopWidget,dlab); n++;
  XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
  formats = (XmString *)CALLOC(num_data_formats(),sizeof(XmString));
  for (i=1;i<num_data_formats();i++) formats[i-1] = XmStringCreate(data_format_name(i),XmFONTLIST_DEFAULT_TAG);
  lst = XmCreateScrolledList(rform,"raw-data-format-list",args,n);
  XtVaSetValues(lst,XmNitems,formats,XmNitemCount,num_data_formats()-1,XmNvisibleItemCount,6,NULL);
  XtManageChild(lst); 
  XmListSelectPos(lst,raw_format(ss),FALSE);
  for (i=1;i<num_data_formats();i++) XmStringFree(formats[i]);
  FREE(formats);
  XtAddCallback(lst,XmNbrowseSelectionCallback,raw_data_browse_Callback,ss);

#if MANAGE_DIALOG
  XtManageChild(raw_data_dialog);
#endif

  if (!(ss->using_schemes)) 
    {
      map_over_children(rform,set_main_color_of_widget,(void *)ss);
      XtVaSetValues(lst,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(XmMessageBoxGetChild(raw_data_dialog,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
      XtVaSetValues(defw,XmNselectColor,(ss->sgx)->pushed_button_color,NULL);
    }

  raw_srate_text = dfs;
  raw_chans_text = dfc;
  raw_location_text = dloc;
}

static file_info *read_raw_dialog(char *filename, snd_state *ss)
{
  char *str;
  file_info *hdr = NULL;
  reflect_raw_pending_in_menu();
  if (!XtIsManaged(raw_data_dialog)) XtManageChild(raw_data_dialog);
  while (XtIsManaged(raw_data_dialog)) check_for_event(ss);
  reflect_raw_open_in_menu();
  if (raw_cancelled) return(NULL);
  str = XmTextGetString(raw_srate_text);
  if ((str) && (*str)) {set_raw_srate(ss,string2int(str)); XtFree(str);}
  str = XmTextGetString(raw_chans_text);
  if ((str) && (*str)) {set_raw_chans(ss,string2int(str)); XtFree(str);}
  str = XmTextGetString(raw_location_text);
  if ((str) && (*str)) {raw_data_location = string2int(str); XtFree(str);}
  mus_header_set_raw_defaults(raw_srate(ss),raw_chans(ss),raw_format(ss));
  mus_sound_override_header(filename, raw_srate(ss), raw_chans(ss), raw_format(ss), MUS_RAW, raw_data_location, 
			mus_bytes_to_samples(raw_format(ss),mus_sound_length(filename) - raw_data_location));
  hdr = (file_info *)CALLOC(1,sizeof(file_info));
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

file_info *get_raw_file_info(char *filename, snd_state *ss)
{
  /* put up dialog for srate, chans, data format */
  XmString xstr;
  char *str;
  if (use_raw_defaults(ss))
    {
      /* choices already made, so just send back a header that reflects those choices */
      return(make_file_info_1(filename,ss));
    }
  if (!raw_data_dialog) 
    make_raw_data_dialog(filename,ss);
  else
    {
      /* set filename in label */
      str = (char *)CALLOC(256,sizeof(char));
      sprintf(str,STR_No_header_found_for,filename_without_home_directory(filename));
      xstr = XmStringCreate(str,XmFONTLIST_DEFAULT_TAG);
      FREE(str);
      XtVaSetValues(raw_data_dialog,XmNmessageString,xstr,NULL);
      XmStringFree(xstr);
      raise_dialog(raw_data_dialog);
    }
  return(read_raw_dialog(filename,ss));
}

file_info *get_reasonable_file_info(char *filename, snd_state *ss, file_info *hdr)
{
  XmString xstr;
  char *tmp;
  tmp = raw_data_explanation(filename,ss,hdr);
  xstr = XmStringCreate(tmp,XmFONTLIST_DEFAULT_TAG);
  if (!raw_data_dialog) make_raw_data_dialog(filename,ss);
  XtVaSetValues(raw_data_dialog,XmNmessageString,xstr,NULL);
  XmStringFree(xstr);
  FREE(tmp);
  raise_dialog(raw_data_dialog);
  return(read_raw_dialog(filename,ss));
}


/* -------------------------------- New File -------------------------------- */

/* no longer shares with raw data -- 11-Nov-99 */

static int new_file_cancelled = 0;
static Widget new_dialog = NULL;
static file_data *new_dialog_data = NULL;
static Widget new_file_name = NULL;

static void NewFileOkCallback(Widget w,XtPointer clientData,XtPointer callData) {new_file_cancelled = 0;}
static void NewFileCancelCallback(Widget w,XtPointer clientData,XtPointer callData) {new_file_cancelled = 1;}

static void new_file_help_callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  new_file_dialog_help((snd_state *)clientData);
}

snd_info *make_new_file_dialog(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *comment)
{
  Arg args[20];
  int n,loc;
  XmString titlestr,xok,xcancel,xhelp;
  char *tmpstr,*title,*newer_name = NULL;
  snd_info *sp = NULL;
  Widget name_label,form;
  new_file_cancelled = 0;
  if (!new_dialog)
    {
      title = (char *)CALLOC(snd_strlen(newname) + 32,sizeof(char));
      sprintf(title,STR_create_new_sound_p,newname);
      titlestr = XmStringCreate(title,XmFONTLIST_DEFAULT_TAG);
      FREE(title);
      xhelp = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
      xcancel = XmStringCreate(STR_Cancel,XmFONTLIST_DEFAULT_TAG);
      xok = XmStringCreate(STR_Ok,XmFONTLIST_DEFAULT_TAG);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
      XtSetArg(args[n],XmNcancelLabelString,xcancel); n++;
      XtSetArg(args[n],XmNhelpLabelString,xhelp); n++;
      XtSetArg(args[n],XmNokLabelString,xok); n++;
      XtSetArg(args[n],XmNdialogTitle,titlestr); n++;
#if RESIZE_DIALOG
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      new_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss),"new",args,n);
#if OVERRIDE_TOGGLE
      override_form_translation(new_dialog);
#endif
      add_dialog(ss,new_dialog);
      XmStringFree(titlestr);
      XmStringFree(xok);
      XmStringFree(xcancel);
      XmStringFree(xhelp);

      XtAddCallback(new_dialog,XmNhelpCallback,new_file_help_callback,ss);
      XtAddCallback(new_dialog,XmNcancelCallback,NewFileCancelCallback,NULL);
      XtAddCallback(new_dialog,XmNokCallback,NewFileOkCallback,NULL);

      n=0;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      form = sndCreateFormWidget("newfile",new_dialog,args,n);

      n=0;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_NONE); n++;
      name_label = XtCreateManagedWidget("New file:",xmLabelWidgetClass,form,args,n);

      n=0;
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNleftWidget,name_label); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNvalue,newname); n++;
      new_file_name = sndCreateTextFieldWidget(ss,"newtext",form,args,n,NOT_ACTIVATABLE,add_completer_func(filename_completer));

      n=0;
#ifdef LESSTIF_VERSION
      XtSetArg(args[n],XmNheight,100); n++;
#endif
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
      XtSetArg(args[n],XmNtopWidget,new_file_name); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      new_dialog_data = sndCreateFileDataForm(ss,form,"data-form",args,n,TRUE,default_output_type(ss),default_output_format(ss),FALSE);

#if MANAGE_DIALOG
      XtManageChild(new_dialog);
#endif
      if (!(ss->using_schemes))	
	{
	  XtVaSetValues(new_dialog_data->format_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
	  XtVaSetValues(new_dialog_data->header_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
	}
      if (!(ss->using_schemes)) map_over_children(new_dialog,set_main_color_of_widget,(void *)ss);

      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(new_dialog,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	}
    }

  load_header_and_data_lists(new_dialog_data,header_type,data_format,srate,chans,-1,comment);
  if (!(XtIsManaged(new_dialog))) XtManageChild(new_dialog);
  while (XtIsManaged(new_dialog)) check_for_event(ss);
  if (new_file_cancelled)
    return(NULL);
  else
    {
      tmpstr = read_file_data_choices(new_dialog_data,&srate, &chans, &header_type, &data_format, &loc);
      newer_name = XmTextGetString(new_file_name);
      if (newer_name == NULL) return(NULL);
      sp = finish_new_file(ss,newer_name,header_type,data_format,srate,chans,tmpstr);
      XtFree(newer_name);
      if (tmpstr) XtFree(tmpstr);
    }
  return(sp);
}



/* -------- mix file dialog -------- */

static Widget file_mix_dialog = NULL;
static Widget file_mix_name = NULL;

static void file_mix_help_callback(Widget w,XtPointer clientData,XtPointer callData)
{
  file_mix_dialog_help((snd_state *)clientData);
}

static void file_mix_cancel_callback(Widget w,XtPointer clientData,XtPointer callData)
{
  XtUnmanageChild(w);
}

static void file_mix_ok_callback(Widget w,XtPointer clientData,XtPointer callData)
{
  char *str = NULL;
  snd_state *ss = (snd_state *)clientData;
  XtUnmanageChild(w);
  mix_complete_file(any_selected_sound(ss),str=XmTextGetString(file_mix_name),"File: mix",with_mix_consoles(ss));
  if (str) XtFree(str);
}

void File_Mix_Callback(Widget w,XtPointer clientData,XtPointer callData)
{
  Arg args[20];
  int n;
  XmString s1;
  Widget wtmp;
  snd_state *ss = (snd_state *)clientData;
  if (!file_mix_dialog)
    {
      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
#if RESIZE_DIALOG
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      s1 = XmStringCreate(STR_mix_in_p,XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n],XmNselectionLabelString,s1); n++;
      file_mix_dialog = XmCreateFileSelectionDialog(w,STR_mix_file_p,args,n);
#if OVERRIDE_TOGGLE
      override_form_translation(file_mix_dialog);
#endif
      XmStringFree(s1);
      XtAddCallback(file_mix_dialog,XmNhelpCallback,file_mix_help_callback,ss);
      XtAddCallback(file_mix_dialog,XmNcancelCallback,file_mix_cancel_callback,ss);
      XtAddCallback(file_mix_dialog,XmNokCallback,file_mix_ok_callback,ss);
      XtManageChild(file_mix_dialog);
      color_file_selection_box(file_mix_dialog,ss);
#ifndef LESSTIF_VERSION
      file_mix_name = XtNameToWidget(file_mix_dialog,"Text");
      if (!file_mix_name) file_mix_name = XmFileSelectionBoxGetChild(file_mix_dialog,XmDIALOG_TEXT);
#else
      file_mix_name = XmFileSelectionBoxGetChild(file_mix_dialog,XmDIALOG_TEXT);
#endif
      if (file_mix_name) add_completer_to_textfield(ss,file_mix_name,add_completer_func(filename_completer));

#ifndef LESSTIF_VERSION
      wtmp = XtNameToWidget(file_mix_dialog,"FilterText");
      if (!wtmp) wtmp = XmFileSelectionBoxGetChild(file_mix_dialog,XmDIALOG_FILTER_TEXT);
#else
      wtmp = XmFileSelectionBoxGetChild(file_mix_dialog,XmDIALOG_FILTER_TEXT);
#endif
      if (wtmp) add_completer_to_textfield(ss,wtmp,add_completer_func(filename_completer));

    }
  if (!XtIsManaged(file_mix_dialog)) XtManageChild(file_mix_dialog);
}


/* ---------------- EDIT_HEADER ---------------- */

static Widget edit_header_dialog = NULL;
static file_data *edit_header_data;

static void edit_header_help_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  edit_header_dialog_help((snd_state *)clientData);
}

static void edit_header_cancel_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  XtUnmanageChild(edit_header_dialog);
}

static void edit_header_ok_Callback(Widget w,XtPointer clientData,XtPointer callData) 
{
  snd_state *ss;
  snd_info *sp = (snd_info *)clientData;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)callData;
  ss = sp->state;
  if (cb->event == ((ss->sgx)->text_activate_event)) return; /* <cr> in one of text fields */
  if (!(sp->read_only))
    edit_header_callback(ss,sp,edit_header_data);
  else snd_error(STR_is_write_protected,sp->shortname);
  XtUnmanageChild(edit_header_dialog);
}

void edit_header(snd_info *sp)
{
  /* like display-info, but writable.
   * need fields for srate, channels, type, format, data location, comment
   * if any are changed, need save button, cancel button, dismiss (leave unsaved but pending), reflect (change Snd display, not file)
   * this means the Snd-effective header is separate from the in-file header even across saves??
   */
  XmString xstr1,xstr2,xstr3,xstr4,titlestr;
  int n;
  Arg args[20];
  snd_state *ss;
  file_info *hdr;
  char *str;
  if (!sp) return;
  ss = sp->state;
  hdr = sp->hdr;

  if (!edit_header_dialog)
    {
      n=0;
      xstr1 = XmStringCreate(STR_Cancel,XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
      xstr2 = XmStringCreate(STR_Help,XmFONTLIST_DEFAULT_TAG);
      xstr3 = XmStringCreate(STR_Save,XmFONTLIST_DEFAULT_TAG);
      titlestr = XmStringCreate(STR_Edit_Header,XmFONTLIST_DEFAULT_TAG);
      str = (char *)CALLOC(128,sizeof(char));
      sprintf(str,STR_Edit_header_of,sp->shortname);
      xstr4 = XmStringCreate(str,XmFONTLIST_DEFAULT_TAG);
      FREE(str);
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNcancelLabelString,xstr1); n++;
      XtSetArg(args[n],XmNhelpLabelString,xstr2); n++;
      XtSetArg(args[n],XmNokLabelString,xstr3); n++;
      XtSetArg(args[n],XmNmessageString,xstr4); n++;
      XtSetArg(args[n],XmNdialogTitle,titlestr); n++;
      XtSetArg(args[n],XmNautoUnmanage,FALSE); n++;
#if RESIZE_DIALOG
      XtSetArg(args[n],XmNresizePolicy,XmRESIZE_GROW); n++;
      XtSetArg(args[n],XmNnoResize,FALSE); n++;
#endif
      XtSetArg(args[n],XmNtransient,FALSE); n++;
      edit_header_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss),STR_Edit_Header,args,n);
      add_dialog(ss,edit_header_dialog);
#if OVERRIDE_TOGGLE
      override_form_translation(edit_header_dialog);
#endif

      XtAddCallback(edit_header_dialog,XmNcancelCallback,edit_header_cancel_Callback,ss);
      XtAddCallback(edit_header_dialog,XmNhelpCallback,edit_header_help_Callback,ss);
      XtAddCallback(edit_header_dialog,XmNokCallback,edit_header_ok_Callback,sp);
      XmStringFree(xstr1);
      XmStringFree(xstr2);
      XmStringFree(xstr3);
      XmStringFree(xstr4);
      XmStringFree(titlestr);

      n=0;
      if (!(ss->using_schemes)) {XtSetArg(args[n],XmNbackground,(ss->sgx)->basic_color); n++;}
      XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
      XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
      edit_header_data = sndCreateFileDataForm(ss,edit_header_dialog,STR_Edit_Header,args,n,TRUE,hdr->type,hdr->format,TRUE);
      load_header_and_data_lists(edit_header_data,hdr->type,hdr->format,hdr->srate,hdr->chans,hdr->data_location,hdr->comment);

#if MANAGE_DIALOG
      XtManageChild(edit_header_dialog);
#endif
      if (!(ss->using_schemes)) 
	{
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog,XmDIALOG_OK_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog,XmDIALOG_CANCEL_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  XtVaSetValues(XmMessageBoxGetChild(edit_header_dialog,XmDIALOG_HELP_BUTTON),XmNarmColor,(ss->sgx)->pushed_button_color,NULL);
	  map_over_children(edit_header_dialog,set_main_color_of_widget,ss);
	  XtVaSetValues(edit_header_data->header_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
	  XtVaSetValues(edit_header_data->format_list,XmNbackground,(ss->sgx)->white,XmNforeground,(ss->sgx)->black,NULL);
	}
    }
  else raise_dialog(edit_header_dialog);
  if (!(XtIsManaged(edit_header_dialog))) XtManageChild(edit_header_dialog);
}


