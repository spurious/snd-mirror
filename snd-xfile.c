#include "snd.h"
#include "snd-file.h"

/* various file-related dialogs:
   File|Edit:Save-as
   File:Open|View
   File|Edit:Mix
   File:Insert
   File:Edit-Header
   File:New
   Info and Raw
   View:Files
*/

#define FSB_BOX(Dialog, Child) XmFileSelectionBoxGetChild(Dialog, Child)
#define MSG_BOX(Dialog, Child) XmMessageBoxGetChild(Dialog, Child)


/* ---------------- open/mix/insert/save-as dialogs ---------------- */

static void color_file_selection_box(Widget w)
{
  /* overwrite most Motif-default colors */
  Widget wtmp;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  
  map_over_children(w, set_main_color_of_widget);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_DIR_LIST), 
		XmNbackground, ss->sgx->white, 
		XmNforeground, ss->sgx->black, 
		NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_LIST), 
		XmNbackground, ss->sgx->white, 
		XmNforeground, ss->sgx->black, 
		NULL);
  
  XtVaSetValues(FSB_BOX(w, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
  
  wtmp = FSB_BOX(w, XmDIALOG_TEXT);
  if (wtmp)
    {
      XtVaSetValues(wtmp,     XmNhighlightThickness,  1,                          NULL);
      XtAddCallback(wtmp,     XmNfocusCallback,       textfield_focus_callback,   NULL);
      XtAddCallback(wtmp,     XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
      XtAddEventHandler(wtmp, EnterWindowMask, false, mouse_enter_text_callback,  NULL);
      XtAddEventHandler(wtmp, LeaveWindowMask, false, mouse_leave_text_callback,  NULL);
    }
  
  wtmp = FSB_BOX(w, XmDIALOG_FILTER_TEXT);	
  if (wtmp)
    {
      XtVaSetValues(wtmp,     XmNhighlightThickness,  1,                          NULL);
      XtAddCallback(wtmp,     XmNfocusCallback,       textfield_focus_callback,   NULL);
      XtAddCallback(wtmp,     XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
      XtAddEventHandler(wtmp, EnterWindowMask, false, mouse_enter_text_callback,  NULL);
      XtAddEventHandler(wtmp, LeaveWindowMask, false, mouse_leave_text_callback,  NULL);
    }
}

static void force_directory_reread(Widget dialog)
{
  /* force update, but make sure the filename is not reset to its (dumb) default */
  XmString dirmask;
  Widget name_field;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(dialog), dialog);
  name_field = FSB_BOX(dialog, XmDIALOG_TEXT);
  filename = XmTextGetString(name_field);
  XtVaGetValues(dialog, XmNdirMask, &dirmask, NULL);
  XmFileSelectionDoSearch(dialog, dirmask);
  XmStringFree(dirmask);
  XmTextSetString(name_field, filename);
  if (filename) 
    {
      XmTextSetCursorPosition(name_field, snd_strlen(filename));
      XtFree(filename);
    }
}

static void force_directory_reread_and_let_filename_change(Widget dialog)
{
  XmString dirmask;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(dialog), dialog);
  XtVaGetValues(dialog, XmNdirMask, &dirmask, NULL);
  XmFileSelectionDoSearch(dialog, dirmask);
  XmStringFree(dirmask);
}



/* -------- popups -------- */

/* I think there is no way to get a key action to popup one of these menus -- Xm/RowColumn.c
 *   appears to insist on a button event, and any change to that via XmNmenuPost gets an
 *   error.  Perhaps we should notice the POPUP_BUTTON setting however?
 */

typedef struct file_pattern_info {
  /* just-sounds file lists */
  bool reread_directory;
  bool in_just_sounds_update;
  Widget dialog, just_sounds_button;
  char *last_dir;
  dir_info *current_files;
  fam_info *directory_watcher;
  int filter_choice, sorter_choice;
} file_pattern_info;

/* popups:
 *   text:    history of previous choices,
 *   list:    sort and filter choices
 *   dir:     higher level dir choices
 *   filter:  history of previous choices
 */

typedef struct file_popup_info {
  Widget dialog;
  Widget file_text_popup, file_list_popup, file_dir_popup, file_filter_popup;
  Widget file_text_popup_label, file_filter_popup_label, file_dir_popup_label, file_list_popup_label;
  /* file_filter here refers to the dialog filter field, not file-filters */
  char **file_text_names, **file_filter_names;                   /* history of choices as array of strings */
  Widget *file_text_items, *file_filter_items, *file_dir_items, *file_list_items;  /* menu items */
  int file_list_items_size;
  file_pattern_info *fp;
} file_popup_info;

/* file popup */
static void file_text_item_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  char *filename;
  filename = get_label(w);
  XmTextFieldSetString(FSB_BOX(fd->dialog, XmDIALOG_TEXT), filename);
  if (filename) XtFree(filename);
}

#define FILE_TEXT_POPUP_LABEL "previous files"

static void file_text_popup_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  XmPopupHandlerCallbackStruct *cb = (XmPopupHandlerCallbackStruct *)info;
  XEvent *e;
  e = cb->event;
  if (e->type == ButtonPress)
    {
      /* position menu to match current text widget, show previous choices, if any else "[no previous choices]" */
      /*     XmMenuPosition(popup_menu, event) happens automatically */
      /* should max len be user-var? history-length? (replace minibuffer-history-length?) */

      char *current_filename;
      int i, filenames_to_display = 0;

      if (fd->file_text_items == NULL)
	{
	  int n = 0;
	  Arg args[12];
	  fd->file_text_items = (Widget *)CALLOC(FILENAME_LIST_SIZE, sizeof(Widget));
	  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
	  for (i = 0; i < FILENAME_LIST_SIZE; i++)
	    {
	      fd->file_text_items[i] = XtCreateWidget("", xmPushButtonWidgetClass, fd->file_text_popup, args, n);
	      XtAddCallback(fd->file_text_items[i], XmNactivateCallback, file_text_item_activate_callback, (void *)fd);
	    }
	}

      current_filename = XmTextFieldGetString(FSB_BOX(fd->dialog, XmDIALOG_TEXT)); 
      /* w is probably ok here (assumes only text triggers this) */

      for (i = 0; i < FILENAME_LIST_SIZE; i++)
	if ((fd->file_text_names[i]) &&
	    (mus_file_probe(fd->file_text_names[i])) &&
	    ((current_filename == NULL) || (strcmp(fd->file_text_names[i], current_filename) != 0)))
	  {
	    set_label(fd->file_text_items[filenames_to_display], fd->file_text_names[i]);
	    XtManageChild(fd->file_text_items[filenames_to_display]);
	    filenames_to_display++;
	  }

      for (i = filenames_to_display; i < FILENAME_LIST_SIZE; i++)
	if ((fd->file_text_items[i]) &&
	    (XtIsManaged(fd->file_text_items[i])))
	  XtUnmanageChild(fd->file_text_items[i]);
      XtFree(current_filename);
      /*
      if (filenames_to_display == 0)
	set_label(fd->file_text_popup_label, "no " FILE_TEXT_POPUP_LABEL);
      else set_label(fd->file_text_popup_label, FILE_TEXT_POPUP_LABEL);
      */
      cb->menuToPost = fd->file_text_popup;
    }
}

/* filter popup */
static void file_filter_text_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  char *filter;
  filter = XmTextFieldGetString(w);
  if (filter)
    {
      remember_filename(filter, fd->file_filter_names);
      XtFree(filter);
      force_directory_reread_and_let_filename_change(fd->dialog);
    }
}

static void file_filter_item_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  Widget text;
  char *filtername;
  filtername = get_label(w);
  text = FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT);
  XmTextFieldSetString(text, filtername);
  force_directory_reread(fd->dialog);
  if (filtername) XtFree(filtername);
}

#define FILE_FILTER_POPUP_LABEL "previous filters"

static void file_filter_popup_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  XmPopupHandlerCallbackStruct *cb = (XmPopupHandlerCallbackStruct *)info;
  XEvent *e;
  e = cb->event;
  if (e->type == ButtonPress)
    {
      char *current_filtername;
      int i, filternames_to_display = 0;

      if (fd->file_filter_items == NULL)
	{
	  int n = 0;
	  Arg args[12];
	  fd->file_filter_items = (Widget *)CALLOC(FILENAME_LIST_SIZE, sizeof(Widget));
	  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
	  for (i = 0; i < FILENAME_LIST_SIZE; i++)
	    {
	      fd->file_filter_items[i] = XtCreateWidget("", xmPushButtonWidgetClass, fd->file_filter_popup, args, n);
	      XtAddCallback(fd->file_filter_items[i], XmNactivateCallback, file_filter_item_activate_callback, (void *)fd);
	    }
	}

      current_filtername = XmTextFieldGetString(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT)); 

      for (i = 0; i < FILENAME_LIST_SIZE; i++)
	if ((fd->file_filter_names[i]) &&
	    ((current_filtername == NULL) || (strcmp(fd->file_filter_names[i], current_filtername) != 0)))
	  {
	    set_label(fd->file_filter_items[filternames_to_display], fd->file_filter_names[i]);
	    XtManageChild(fd->file_filter_items[filternames_to_display]);
	    filternames_to_display++;
	  }

      for (i = filternames_to_display; i < FILENAME_LIST_SIZE; i++)
	if ((fd->file_filter_items[i]) &&
	    (XtIsManaged(fd->file_filter_items[i])))
	  XtUnmanageChild(fd->file_filter_items[i]);
      XtFree(current_filtername);
      /*
      if (filternames_to_display == 0)
	set_label(fd->file_filter_popup_label, "no " FILE_FILTER_POPUP_LABEL);
      else set_label(fd->file_filter_popup_label, FILE_FILTER_POPUP_LABEL);
      */
      cb->menuToPost = fd->file_filter_popup;
    }
}

/* dir list popup */

static void update_dir_list(Widget dialog, char *filter)
{
  Widget text;
  text = FSB_BOX(dialog, XmDIALOG_FILTER_TEXT);
  XmTextFieldSetString(text, filter);
  force_directory_reread(dialog);
}

static void file_dir_item_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  char *name, *filter;
  name = get_label(w);
  filter = mus_format("%s/*", name);
  update_dir_list(fd->dialog, filter);
  if (name) XtFree(name);
  FREE(filter);
}

#define FILE_DIR_POPUP_LABEL "dirs"

/* dir_items, but strs generated on the fly, current in filter text */

static void file_dir_popup_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  XmPopupHandlerCallbackStruct *cb = (XmPopupHandlerCallbackStruct *)info;
  XEvent *e;
  e = cb->event;
  if (e->type == ButtonPress)
    {
      char *current_filename = NULL;
      int i, dirs_to_display = 0, len = 0;

      if (fd->file_dir_items == NULL)
	{
	  int n = 0;
	  Arg args[12];
	  fd->file_dir_items = (Widget *)CALLOC(FILENAME_LIST_SIZE, sizeof(Widget));
	  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
	  for (i = 0; i < FILENAME_LIST_SIZE; i++)
	    {
	      fd->file_dir_items[i] = XtCreateWidget("", xmPushButtonWidgetClass, fd->file_dir_popup, args, n);
	      XtAddCallback(fd->file_dir_items[i], XmNactivateCallback, file_dir_item_activate_callback, (void *)fd);
	    }
	}

      {
	XmStringTable items;
	int num_dirs;
	XtVaGetValues(fd->dialog, XmNdirListItems, &items, XmNdirListItemCount, &num_dirs, NULL);
	if (num_dirs > 0)
	  current_filename = (char *)XmStringUnparse(items[0], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
      }
      if (!current_filename)
	{
	  current_filename = XmTextFieldGetString(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT));
	  if (!current_filename) 
	    current_filename = XmTextFieldGetString(FSB_BOX(fd->dialog, XmDIALOG_TEXT));
	}

      if (current_filename)
	{
	  len = strlen(current_filename);
	  for (i = 0; i < len; i++)
	    if (current_filename[i] == '/')
	      dirs_to_display++;

	  if (dirs_to_display > FILENAME_LIST_SIZE)
	    dirs_to_display = FILENAME_LIST_SIZE;

	  if (dirs_to_display > 0)
	    {
	      char **dirs;
	      int j = 1;
	      dirs = (char **)CALLOC(dirs_to_display, sizeof(char *));
	      dirs[0] = strdup("/");
	      for (i = 1; i < len; i++)
		if (current_filename[i] == '/')
		  {
		    dirs[j] = (char *)calloc(i + 1, sizeof(char));
		    strncpy(dirs[j], (const char *)current_filename, i);
		    j++;
		  }

	      for (i = 0; i < dirs_to_display; i++)
		{
		  set_label(fd->file_dir_items[i], dirs[i]);
		  XtManageChild(fd->file_dir_items[i]);
		  free(dirs[i]);
		}
	      FREE(dirs);
	    }
	}

      for (i = dirs_to_display; i < FILENAME_LIST_SIZE; i++)
	if ((fd->file_dir_items[i]) &&
	    (XtIsManaged(fd->file_dir_items[i])))
	  XtUnmanageChild(fd->file_dir_items[i]);
      XtFree(current_filename);

      cb->menuToPost = fd->file_dir_popup;
    }
}

#define FILE_LIST_POPUP_LABEL "sort/filter"
#define NO_FILTER_LABEL "no filter"

#define FILE_FILTER_OFFSET 1024
#define NO_FILE_FILTER_OFFSET 2048

static void sort_files_and_redisplay(file_pattern_info *fp);

static void file_list_item_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  int choice = 0;
  XtVaGetValues(w, XmNuserData, &choice, NULL);
  if (choice >= FILE_FILTER_OFFSET)
    {
      XmToggleButtonSetState(fd->fp->just_sounds_button, false, false);
      if (choice == NO_FILE_FILTER_OFFSET)
	fd->fp->filter_choice = NO_FILE_FILTER;
      else fd->fp->filter_choice = choice - FILE_FILTER_OFFSET + 2;
      fd->fp->in_just_sounds_update = true;
      force_directory_reread(fd->fp->dialog);
      fd->fp->in_just_sounds_update = false;
    }
  else
    {
      fd->fp->sorter_choice = choice;
      sort_files_and_redisplay(fd->fp);
    }
}

static Widget make_file_list_item(file_popup_info *fd, int choice)
{
  int n;
  Arg args[12];
  char *item_label;
  Widget w;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;

  switch (choice)
    {
    case 0: item_label = "a..z";       break;
    case 1: item_label = "z..a";       break;
    case 2: item_label = "new..old";   break;
    case 3: item_label = "old..new";   break;
    case 4: item_label = "small..big"; break;
    case 5: item_label = "big..small"; break;
    default: item_label = "unused";    break;
    }

  XtSetArg(args[n], XmNuserData, choice);           /* userData is index into sorters list */
  w = XtCreateManagedWidget(item_label, xmPushButtonWidgetClass, fd->file_list_popup, args, n + 1);
  XtAddCallback(w, XmNactivateCallback, file_list_item_activate_callback, (void *)fd);
  return(w);
}

static void file_list_popup_callback(Widget w, XtPointer context, XtPointer info)
{
  file_popup_info *fd = (file_popup_info *)context;
  XmPopupHandlerCallbackStruct *cb = (XmPopupHandlerCallbackStruct *)info;
  XEvent *e;
  e = cb->event;
  if (e->type == ButtonPress)
    {
      int i, items_len;
      if (fd->file_list_items == NULL)
	{
	  /* set up the default menu items */

	  fd->file_list_items = (Widget *)CALLOC(SORT_XEN, sizeof(Widget));
	  fd->file_list_items_size = SORT_XEN;

	  for (i = 0; i < SORT_XEN; i++)
	    fd->file_list_items[i] = make_file_list_item(fd, i);
	}

      /* clear any trailers just in case */
      if (fd->file_list_items_size > SORT_XEN)
	for (i = SORT_XEN; i < fd->file_list_items_size; i++)
	  XtUnmanageChild(fd->file_list_items[i]);

      /* check for added sort and filter functions (allocate more items if needed) */
      {
	int extra_sorters = 0, extra_filters = 0;
	for (i = 0; i < ss->file_sorters_size; i++)
	  if (!(XEN_FALSE_P(XEN_VECTOR_REF(ss->file_sorters, i))))
	    extra_sorters++;
	for (i = 0; i < ss->file_filters_size; i++)
	  if (!(XEN_FALSE_P(XEN_VECTOR_REF(ss->file_filters, i))))
	    extra_filters++;

	items_len = SORT_XEN + extra_sorters + extra_filters;
	if (fd->fp->filter_choice != NO_FILE_FILTER) items_len++;

	if (items_len > fd->file_list_items_size)
	  {
	    fd->file_list_items = (Widget *)REALLOC(fd->file_list_items, items_len * sizeof(Widget));
	    for (i = fd->file_list_items_size; i < items_len; i++)
	      fd->file_list_items[i] = make_file_list_item(fd, i);
	    fd->file_list_items_size = items_len;
	  }
      }

      /* make sure all the added sorter labels are correct, bg blue, and items active */
      if (fd->file_list_items_size > SORT_XEN)
	{
	  int k = SORT_XEN;

	  /* sorters */
	  for (i = 0; i < ss->file_sorters_size; i++)
	    {
	      if (!(XEN_FALSE_P(XEN_VECTOR_REF(ss->file_sorters, i))))
		{
		  set_label(fd->file_list_items[k], XEN_TO_C_STRING(XEN_CAR(XEN_VECTOR_REF(ss->file_sorters, i))));
		  XtVaSetValues(fd->file_list_items[k], 
				XmNbackground, ss->sgx->lighter_blue,
				XmNuserData, SORT_XEN + i,
				NULL);
		  if (!(XtIsManaged(fd->file_list_items[k])))
		    XtManageChild(fd->file_list_items[k]);
		  k++;
		}
	    }
	  
	  for (i = 0; i < ss->file_filters_size; i++)
	    {
	      if (!(XEN_FALSE_P(XEN_VECTOR_REF(ss->file_filters, i))))
		{
		  set_label(fd->file_list_items[k], XEN_TO_C_STRING(XEN_CAR(XEN_VECTOR_REF(ss->file_filters, i))));
		  XtVaSetValues(fd->file_list_items[k], XmNbackground, ss->sgx->light_blue, 
				XmNuserData, i + FILE_FILTER_OFFSET,
				NULL);
		  if (!(XtIsManaged(fd->file_list_items[k])))
		    XtManageChild(fd->file_list_items[k]);
		  k++;
		}
	    }

	  /* add "no filter" item if currently filtered */
	  if (fd->fp->filter_choice != NO_FILE_FILTER)
	    {
	      set_label(fd->file_list_items[k], NO_FILTER_LABEL);
	      XtVaSetValues(fd->file_list_items[k], XmNbackground, ss->sgx->light_blue, 
			    XmNuserData, NO_FILE_FILTER_OFFSET,
			    NULL);
	      if (!(XtIsManaged(fd->file_list_items[k])))
		XtManageChild(fd->file_list_items[k]);
	    }
	  
	}
      cb->menuToPost = fd->file_list_popup;
    }
}

static void add_file_popups(file_popup_info *fd)
{
  int n;
  Arg args[20];

  /* from lib/Xm.RCPopup.c:
   * When a user creates a new popup menu then we will install a particular
   * event handler on the menu's widget parent. Along with this we install
   * a grab on the button specified in XmNmenuPost or XmNwhichButton.   [XmNmenuPost is a string = translation table syntax, <Btn3Down> is default]
   *                                                                    [XmNwhichButton is obsolete]
   * The posting algorithm is as follows: 
   * 
   * 1. On receipt of a posting event, the handler will search the child
   * list for a candidate widget or gadget, and track the most specific
   * popup menu available (these can be found in the popup list). The
   * criteria for a match includes matching the XmNmenuPost information.
   * 
   * 2. Matching criteria include: 
   * 
   *    * The menu must have XmNpopupEnabled set to either
   *      XmPOPUP_AUTOMATIC or XmPOPUP_AUTOMATIC_RECURSIVE.  
   * 
   *    * The popup menu is chosen according to creation order. If there is
   *      more than one, the first correct match is chosen.  
   * 
   *    * If the popup menu is found in a parent of the target widget, and
   *      the popup menu must also have XmNpopupEnabled set to 
   *      XmPOPUP_AUTOMATIC_RECURSIVE to match.                         [sigh -- no one actually reads comments...]
   * 
   * 3. Once a selection is made, if the menu's parent widget has a
   * popupHandlerCallback, it is invoked. The callback allows the user to
   * determine if a more specific menu is necessary, such as would be the
   * case in a graphical manipulation environment, and includes all the
   * necessary information.  
   * 
   */

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC); n++;

  /* file text */
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_TEXT), XmNpopupHandlerCallback, file_text_popup_callback, (void *)fd);
  fd->file_text_popup = XmCreatePopupMenu(FSB_BOX(fd->dialog, XmDIALOG_TEXT), "file-text-popup", args, n);
  fd->file_text_names = make_filename_list();
  fd->file_text_popup_label = XtCreateManagedWidget(FILE_TEXT_POPUP_LABEL, xmLabelWidgetClass, fd->file_text_popup, args, n);
  XtCreateManagedWidget("sep", xmSeparatorWidgetClass, fd->file_text_popup, args, n);

  /* filter text */
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT), XmNpopupHandlerCallback, file_filter_popup_callback, (void *)fd);
  fd->file_filter_popup = XmCreatePopupMenu(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT), "file-filter-popup", args, n);
  fd->file_filter_names = make_filename_list();
  fd->file_filter_popup_label = XtCreateManagedWidget(FILE_FILTER_POPUP_LABEL, xmLabelWidgetClass, fd->file_filter_popup, args, n);
  XtCreateManagedWidget("sep", xmSeparatorWidgetClass, fd->file_filter_popup, args, n);
  {
    char *startup_filter;
    startup_filter = XmTextFieldGetString(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT));
    if (startup_filter) 
      {
	remember_filename(startup_filter, fd->file_filter_names);
	XtFree(startup_filter);
      }
  }
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT), XmNactivateCallback, file_filter_text_activate_callback, (void *)fd);

  /* file directory */
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_DIR_LIST), XmNpopupHandlerCallback, file_dir_popup_callback, (void *)fd);
  fd->file_dir_popup = XmCreatePopupMenu(FSB_BOX(fd->dialog, XmDIALOG_DIR_LIST), "file-dir-popup", args, n);
  fd->file_dir_popup_label = XtCreateManagedWidget(FILE_DIR_POPUP_LABEL, xmLabelWidgetClass, fd->file_dir_popup, args, n);
  XtCreateManagedWidget("sep", xmSeparatorWidgetClass, fd->file_dir_popup, args, n);

  /* file list */
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_LIST), XmNpopupHandlerCallback, file_list_popup_callback, (void *)fd);
  fd->file_list_popup = XmCreatePopupMenu(FSB_BOX(fd->dialog, XmDIALOG_LIST), "file-list-popup", args, n);
  fd->file_list_popup_label = XtCreateManagedWidget(FILE_LIST_POPUP_LABEL, xmLabelWidgetClass, fd->file_list_popup, args, n);
  XtCreateManagedWidget("sep", xmSeparatorWidgetClass, fd->file_list_popup, args, n);
}


/* ---------------- just-sounds (file-filters) ---------------- */

static void file_change_directory_callback(Widget w, XtPointer context, XtPointer info) 
{
  file_pattern_info *fp = (file_pattern_info *)context;
  fp->reread_directory = true;
  force_directory_reread_and_let_filename_change(fp->dialog);
  fp->reread_directory = false;
}

#if HAVE_FAM
static void watch_current_directory_contents(struct fam_info *famp, FAMEvent *fe)
{
  switch (fe->code)
    {
    case FAMDeleted:
      /* we could simply edit the current_files list here, rather than requesting a full directory read,
       *   but I doubt it matters -- directory reads are apparently very fast, and the simplicity is a blessing.
       */
    case FAMCreated:
    case FAMMoved:
      if ((!(just_sounds(ss))) ||
	  (sound_file_p(fe->filename)))
	{
	  file_pattern_info *fp = (file_pattern_info *)(famp->data);
	  fp->reread_directory = true;
	  if ((fp->dialog) &&
	      (XtIsManaged(fp->dialog)))
	    {
	      force_directory_reread(fp->dialog);
	      fp->reread_directory = false;
	    }
	}
      break;
    default:
      /* ignore the rest */
      break;
    }
}
#endif

static void sort_files_and_redisplay(file_pattern_info *fp)
{
  /* if just sorting, no need to read the directory */
  dir_info *cur_dir;
  cur_dir = fp->current_files;
  if (cur_dir->len > 0)
    {
      XmString *names;
      int i;

      snd_sort(fp->sorter_choice, cur_dir->files, cur_dir->len);

      /* here we could use colored text to mark sound files, perhaps different colors for
       *   different chans (as in install-searcher-with-colors), but I would rather have
       *   used different background colors (less intrusive I think).  As far as I can tell,
       *   this is impossible given the current XmList widget -- each item is an internal
       *   "Element", not a label widget or whatever, and the selection color, for example,
       *   is done by hand.
       */

      names = (XmString *)CALLOC(cur_dir->len, sizeof(XmString));
      for (i = 0; i < cur_dir->len; i++) 
	names[i] = XmStringCreateLocalized(cur_dir->files[i]->full_filename);

      XtVaSetValues(fp->dialog, 
		    XmNfileListItems, names, 
		    XmNfileListItemCount, cur_dir->len, 
		    XmNlistUpdated, true, 
		    NULL);

      for (i = 0; i < cur_dir->len; i++) 
	if (names[i]) 
	  XmStringFree(names[i]);
      FREE(names);
    }
  else
    {
      /* nothing to sort, but make sure the files list is actually empty */
      XtVaSetValues(fp->dialog, 
		    XmNfileListItems, NULL, 
		    XmNfileListItemCount, 0, 
		    XmNlistUpdated, true, 
		    NULL);
    }
}

static void snd_directory_reader(Widget dialog, XmFileSelectionBoxCallbackStruct *info)
{
  /* replaces the FSB searchProc */
  file_pattern_info *fp;
  dir_info *cur_dir = NULL;
  char *pattern = NULL, *our_dir = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(dialog), dialog);

  XtVaGetValues(dialog, XmNuserData, &fp, NULL);
  if (!(fp->dialog)) fp->dialog = dialog; /* can be null at initialization */

  pattern = (char *)XmStringUnparse(info->pattern, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  our_dir = (char *)XmStringUnparse(info->dir,     NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

  /* get current directory contents, given filter and pattern */
  if (strcmp(pattern, "*") == 0)
    {
      if (fp->filter_choice == NO_FILE_FILTER)
	cur_dir = find_files_in_dir(our_dir);
      else cur_dir = find_filtered_files_in_dir(our_dir, fp->filter_choice);
    }
  else cur_dir = find_filtered_files_in_dir_with_pattern(our_dir, fp->filter_choice, pattern);

  if (fp->current_files) free_dir_info(fp->current_files);
  fp->current_files = cur_dir;
  if (pattern) XtFree(pattern);

  /* post the sorted list in the dialog -- alphabetize by default */
  sort_files_and_redisplay(fp);

#if HAVE_FAM
  /* make sure fam knows which directory to watch */
  if ((fp->last_dir == NULL) ||
      (strcmp(our_dir, fp->last_dir) != 0))
    {
      if (fp->directory_watcher)
	fam_unmonitor_file(fp->last_dir, fp->directory_watcher); /* filename normally ignored */

      fp->directory_watcher = fam_monitor_directory(our_dir, (void *)fp, watch_current_directory_contents);

      if (fp->last_dir) FREE(fp->last_dir);
      fp->last_dir = copy_string(our_dir);
      fp->reread_directory = false;
    }
#endif
  if (our_dir) XtFree(our_dir);
}

static void just_sounds_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  file_pattern_info *fp = (file_pattern_info *)context;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(fp->dialog), fp->dialog);
  if (cb->set)
    fp->filter_choice = JUST_SOUNDS_FILTER;
  else fp->filter_choice = NO_FILE_FILTER;
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
      wtmp = FSB_BOX(dp->dialog, XmDIALOG_TEXT);
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
  int n;
  Arg args[12];

  n = 0;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  rc = XtCreateManagedWidget("filebuttons-rc", xmRowColumnWidgetClass, parent, args, n);

  n = 0;
  XtSetArg(args[n], XmNset, just_sounds(ss)); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  fp->just_sounds_button = XtCreateManagedWidget(_("sound files only"), xmToggleButtonWidgetClass, rc, args, n);

  n = 0;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  dp->play_button = XtCreateWidget(_("play selected sound"), xmToggleButtonWidgetClass, rc, args, n);

  XtAddCallback(dp->play_button, XmNvalueChangedCallback, play_selected_callback, (XtPointer)dp);
  XtAddCallback(fp->just_sounds_button, XmNvalueChangedCallback, just_sounds_callback, (XtPointer)fp);
}



/* -------- File Open/View/Mix Dialogs -------- */

typedef struct file_dialog_info {
  bool file_dialog_read_only;
  Widget dialog, mkdirB;
  Widget info_frame, info1, info2;     /* labels giving info on selected file, or an error message */
  file_pattern_info *fp;
  dialog_play_info *dp;
  int open_file_watcher_loc;           /* write-only currently (the watcher is permanent) */
  fam_info *unsound_directory_watcher; /* started if file doesn't exist, not a sound file, bogus header, etc (clears error msg if problem changed) */
  char *unsound_dirname, *unsound_filename;
  fam_info *info_filename_watcher;     /* watch for change in selected file and repost info */
  char *info_filename;
  file_popup_info *fpop;
} file_dialog_info;

static void open_file_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  open_file_dialog_help();
}

static void file_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  file_dialog_stop_playing((dialog_play_info *)context);
  XtUnmanageChild (w);
}

static void file_wm_delete_callback(Widget w, XtPointer context, XtPointer info) 
{
  file_dialog_stop_playing((dialog_play_info *)context);
}

static void post_sound_info(Widget info1, Widget info2, const char *filename, bool with_filename)
{
  /* filename is known[strongly believed] to be a sound file, etc */
  XmString label;
  char *buf;
  buf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s%s%d chan%s, %d Hz, %.3f secs",
	       (with_filename) ? filename_without_directory(filename) : "",
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
  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s, %s%s",
	       mus_header_type_name(mus_sound_header_type(filename)),
	       short_data_format_name(mus_sound_data_format(filename), filename),
	       snd_strftime(", %d-%b-%Y", mus_sound_write_date(filename)));
  label = XmStringCreateLocalized(buf);
  XtVaSetValues(info2, XmNlabelString, label, NULL);
  XmStringFree(label);
  FREE(buf);
}

#if HAVE_FAM
static void unpost_file_info(file_dialog_info *fd);

static void repost_sound_info(file_dialog_info *fd)
{
  if ((mus_file_probe(fd->info_filename)) &&
      (plausible_sound_file_p(fd->info_filename)))
    post_sound_info(fd->info1, fd->info2, fd->info_filename, true);
  else unpost_file_info(fd);
}

static void watch_info_file(struct fam_info *fp, FAMEvent *fe)
{
  switch (fe->code)
    {
    case FAMChanged:
    case FAMDeleted:
    case FAMCreated:
    case FAMMoved:
      repost_sound_info((file_dialog_info *)(fp->data));
      break;
    default:
      /* ignore the rest */
      break;
    }
}
#endif

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
#if HAVE_FAM
  if (fd->info_filename_watcher)
    {
      fd->info_filename_watcher = fam_unmonitor_file(fd->info_filename, fd->info_filename_watcher);
      if (fd->info_filename) {FREE(fd->info_filename); fd->info_filename = NULL;}
    }
  fd->info_filename = copy_string(filename);
  fd->info_filename_watcher = fam_monitor_file(fd->info_filename, (void *)fd, watch_info_file);
#endif
}

static void unpost_file_info(file_dialog_info *fd)
{
  if (XtIsManaged(fd->dp->play_button)) 
    XtUnmanageChild(fd->dp->play_button);
  if (XtIsManaged(fd->info_frame))
    XtUnmanageChild(fd->info_frame);
#if HAVE_FAM
  if (fd->info_filename_watcher)
    {
      fd->info_filename_watcher = fam_unmonitor_file(fd->info_filename, fd->info_filename_watcher);
      if (fd->info_filename) {FREE(fd->info_filename); fd->info_filename = NULL;}
    }
#endif
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
      int num_files = 0, i, pos = -1, l, u;
      char *file_list_file = NULL;

      file_list = FSB_BOX(fd->dialog, XmDIALOG_LIST);
      XtVaGetValues(fd->dialog,
		    XmNfileListItemCount, &num_files,
		    XmNfileListItems, &files, /* do not free */
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
      ensure_list_row_visible(file_list, pos);
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

static bool file_is_directory(Widget dialog)
{
  char *filename = NULL;
  bool is_dir = false;
  filename = XmTextGetString(FSB_BOX(dialog, XmDIALOG_TEXT));
  if (filename)
    {
      is_dir = directory_p(filename);
      XtFree(filename);
    }
  return(is_dir);
}

static bool file_is_nonexistent_directory(Widget dialog)
{
  char *filename = NULL;
  bool is_nonexistent_dir = false;
  filename = XmTextGetString(FSB_BOX(dialog, XmDIALOG_TEXT));
  if (filename)
    {
      int i, len;
      len = strlen(filename);
      if ((!mus_file_probe(filename)) && 
	  (filename[len - 1] == '/'))
	{
	  /* check that there's some hope of making this directory */
	  for (i = len - 2; i > 0; i--)
	    if (filename[i] == '/')
	      {
		filename[i] = '\0';
		is_nonexistent_dir = directory_p(filename);
		break;
	      }
	}
      XtFree(filename);
    }
  return(is_nonexistent_dir);
}

static void reflect_text_in_open_button(Widget w, XtPointer context, XtPointer info)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  /* w here is the text widget, not the button */
  XtSetSensitive(FSB_BOX(fd->dialog, XmDIALOG_OK_BUTTON), (!(file_is_directory(fd->dialog))));
  if (fd->mkdirB) XtSetSensitive(fd->mkdirB, file_is_nonexistent_directory(fd->dialog));
}

static file_dialog_info *make_file_dialog(bool read_only, char *title, char *select_title, 
					  XtCallbackProc file_ok_proc, XtCallbackProc file_help_proc)
{
  /* file selection dialog box with added "Just Sound Files" and "Play selected" toggle buttons and info area,
   *   fam support, popups, and so on.  This applies to the Open, Mix, and Insert dialogs.  The save-as
   *   dialogs are handled by make_save_as_dialog below
   */
  Widget w;
  file_dialog_info *fd;
  Arg args[20];
  int n;
  XmString s1, s2, ok_label, filter_list_label, cancel_label;
  Widget wtmp = NULL, rc1, rc2;

  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->fp = (file_pattern_info *)CALLOC(1, sizeof(file_pattern_info));
  fd->fp->in_just_sounds_update = false;
  if (just_sounds(ss))
    fd->fp->filter_choice = JUST_SOUNDS_FILTER;
  else fd->fp->filter_choice = NO_FILE_FILTER;

  fd->dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));
  fd->file_dialog_read_only = read_only;
  fd->fpop = (file_popup_info *)CALLOC(1, sizeof(file_popup_info));
  fd->fpop->fp = fd->fp;

  w = MAIN_SHELL(ss);

  /* -------- titles -- 'Dismiss' is changed later to 'Cancel' in some cases */
  s1 = XmStringCreateLocalized(select_title);
  s2 = XmStringCreateLocalized(title);
  ok_label = XmStringCreateLocalized(title);
  filter_list_label = XmStringCreateLocalized(_("files listed:"));
  cancel_label = XmStringCreateLocalized(_("Dismiss"));

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNokLabelString, ok_label); n++;
  XtSetArg(args[n], XmNselectionLabelString, s1); n++;                    /* "open", "mix", "insert" */
  XtSetArg(args[n], XmNdialogTitle, s2); n++;
  XtSetArg(args[n], XmNfilterLabelString, filter_list_label); n++;        /* default label 'Filter' is confusing in this context */
  XtSetArg(args[n], XmNfileFilterStyle, XmFILTER_HIDDEN_FILES); n++;      /* the dot files mostly just get in the way */
  XtSetArg(args[n], XmNcancelLabelString, cancel_label); n++;
  XtSetArg(args[n], XmNuserData, (XtPointer)(fd->fp)); n++;
  XtSetArg(args[n], XmNfileSearchProc, snd_directory_reader); n++;        /* over-ride Motif's directory reader altogether */      

  fd->dialog = XmCreateFileSelectionDialog(w, title, args, n);
  fd->fp->dialog = fd->dialog;
  fd->dp->dialog = fd->dialog;
  fd->fpop->dialog = fd->dialog;

  XtUnmanageChild(FSB_BOX(fd->dialog, XmDIALOG_DIR_LIST_LABEL)); /* these are obvious */
  XtUnmanageChild(FSB_BOX(fd->dialog, XmDIALOG_LIST_LABEL));
  XtUnmanageChild(FSB_BOX(fd->dialog, XmDIALOG_APPLY_BUTTON));   /* "Filter" button is useless */

  XmStringFree(s1);
  XmStringFree(s2);
  XmStringFree(ok_label);
  XmStringFree(filter_list_label);
  XmStringFree(cancel_label);

  /* -------- play and just-sounds buttons and info area */
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

  /* -------- Snd-like color schemes */
  color_file_selection_box(fd->dialog);
  XtVaSetValues(fd->fp->just_sounds_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(fd->dp->play_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);

  /* -------- completions */
  wtmp = FSB_BOX(fd->dialog, XmDIALOG_TEXT);
  add_completer_to_textfield(wtmp, add_completer_func(sound_filename_completer, NULL));
  XtAddCallback(wtmp, XmNfocusCallback, focus_filename_text_callback, (XtPointer)fd);
  XtAddCallback(wtmp, XmNlosingFocusCallback, unfocus_filename_text_callback, (XtPointer)fd);

  wtmp = FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT);
  add_completer_to_textfield(wtmp, add_completer_func(filename_completer, NULL));
  XtAddCallback(wtmp, XmNvalueChangedCallback, unpost_if_filter_changed, (XtPointer)fd);

  /* -------- fam/gamin */
#if HAVE_FAM
  {
    char *our_dir;
    XmString cur_dir;
    XtVaGetValues(fd->dialog, XmNdirectory, &cur_dir, NULL);
    our_dir = (char *)XmStringUnparse(cur_dir, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    fd->fp->directory_watcher = fam_monitor_directory(our_dir, (void *)(fd->fp), watch_current_directory_contents);
    /* don't set last_dir yet */
    XtFree(our_dir);
  }
#endif

  /* -------- base button callbacks */
  XtAddCallback(fd->dialog, XmNokCallback, file_ok_proc, (XtPointer)fd);
  XtAddCallback(fd->dialog, XmNcancelCallback, file_cancel_callback, (XtPointer)(fd->dp));
  XtAddCallback(fd->dialog, XmNhelpCallback, file_help_proc, NULL);
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_LIST), XmNbrowseSelectionCallback, file_dialog_select_callback, (XtPointer)fd);

  /* -------- single click in directory list */
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_DIR_LIST), XmNbrowseSelectionCallback, file_change_directory_callback, (XtPointer)(fd->fp));

  /* -------- the WM 'close' button */
  {
    Atom wm_delete_window;
    wm_delete_window = XmInternAtom(MAIN_DISPLAY(ss), "WM_DELETE_WINDOW", false);
    XmAddWMProtocolCallback(XtParent(fd->dialog), wm_delete_window, file_wm_delete_callback, (XtPointer)(fd->dp));
  }

  /* -------- special popups */
  add_file_popups(fd->fpop);

  XtSetSensitive(FSB_BOX(fd->dialog, XmDIALOG_OK_BUTTON), (!(file_is_directory(fd->dialog))));
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_TEXT), XmNvalueChangedCallback, reflect_text_in_open_button, (void *)fd);

  return(fd);
}


/* -------- File:Open/View dialogs -------- */

static void file_open_error(const char *error_msg, file_dialog_info *fd)
{
  XmString msg;
  msg = XmStringCreateLocalized((char *)error_msg);
  XtVaSetValues(fd->info1, 
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);

  if (XtIsManaged(fd->info2))
    XtUnmanageChild(fd->info2);
  if (!(XtIsManaged(fd->info_frame))) 
    XtManageChild(fd->info_frame);
}

static void redirect_file_open_error(const char *error_msg, void *ufd)
{
  /* called from snd_error, redirecting error handling to the dialog */
  file_open_error(error_msg, (file_dialog_info *)ufd);
}

static void open_modify_callback(Widget w, XtPointer context, XtPointer info);

static void unpost_open_modify_error(file_dialog_info *fd)
{
  Widget dialog_filename_text;
  if (XtIsManaged(fd->info_frame))
    XtUnmanageChild(fd->info_frame);
  dialog_filename_text = FSB_BOX(fd->dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) 
    XtRemoveCallback(dialog_filename_text, XmNmodifyVerifyCallback, open_modify_callback, (XtPointer)fd);
#if HAVE_FAM
  if (fd->unsound_directory_watcher)
    {
      fd->unsound_directory_watcher = fam_unmonitor_file(fd->unsound_dirname, fd->unsound_directory_watcher);
      if (fd->unsound_dirname) {FREE(fd->unsound_dirname); fd->unsound_dirname = NULL;}
      if (fd->unsound_filename) {FREE(fd->unsound_filename); fd->unsound_filename = NULL;}
    }
#endif
}

static void open_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  if (!(fd->fp->in_just_sounds_update)) /* auto trigger from just_sounds button -- unwanted! */
    unpost_open_modify_error(fd);
  cbs->doit = true; /* fixup filename elsewhere -- returning false here makes the thing beep! */
}

static void clear_error_if_open_changes(Widget dialog, file_dialog_info *data)
{
  Widget dialog_filename_text;
  dialog_filename_text = FSB_BOX(dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) 
    XtAddCallback(dialog_filename_text, XmNmodifyVerifyCallback, open_modify_callback, (XtPointer)data);
}

#if HAVE_FAM
static void unpost_unsound_error(struct fam_info *fp, FAMEvent *fe)
{
  file_dialog_info *fd;
  switch (fe->code)
    {
    case FAMChanged:
    case FAMCreated:
      fd = (file_dialog_info *)(fp->data);
      if ((fd) &&
	  (fe->filename) &&
	  (fd->unsound_filename) &&
	  (strcmp(fe->filename, fd->unsound_filename) == 0))
	unpost_open_modify_error(fd);
      break;
    default:
      /* ignore the rest */
      break;
    }
}

static void start_unsound_watcher(file_dialog_info *fd, const char *filename)
{
  if (fd->unsound_directory_watcher)
    {
      fd->unsound_directory_watcher = fam_unmonitor_file(fd->unsound_dirname, fd->unsound_directory_watcher);
      if (fd->unsound_dirname) FREE(fd->unsound_dirname);
      if (fd->unsound_filename) FREE(fd->unsound_filename);
    }
  fd->unsound_filename = mus_expand_filename(filename);
  fd->unsound_dirname = just_directory(fd->unsound_filename);
  fd->unsound_directory_watcher = fam_monitor_directory(fd->unsound_dirname, (void *)fd, unpost_unsound_error);
}
#else
static void start_unsound_watcher(file_dialog_info *fd, const char *filename) {}
#endif

static void file_open_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)info;
  char *filename = NULL;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(w), w);
  if (XmGetFocusWidget(fd->dialog) == FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT)) return;

  filename = (char *)XmStringUnparse(cbs->value, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((!filename) || (!(*filename)))
    {
      file_open_error(_("no filename given"), fd);
      clear_error_if_open_changes(fd->dialog, fd);
    }
  else
    {
      file_dialog_stop_playing(fd->dp);
      if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
	{
	  snd_info *sp;
	  redirect_snd_error_to(redirect_file_open_error, (void *)fd);
	  ss->sgx->requestor_dialog = w;
	  ss->open_requestor = FROM_OPEN_DIALOG;
	  ss->open_requestor_data = NULL;
	  sp = snd_open_file(filename, fd->file_dialog_read_only);
	  redirect_snd_error_to(NULL, NULL);
	  if (sp) 
	    {
	      XtUnmanageChild(w);
	      remember_filename(filename, fd->fpop->file_text_names);
	      select_channel(sp, 0);
	    }
	  else
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  clear_error_if_open_changes(fd->dialog, fd);
		  /* whatever the error was, I think it is correct here to unpost the error
		   *   if the underlying file is either changed or created.
		   */
		  start_unsound_watcher(fd, filename);
		}
	    }
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format(_("%s is a directory"), filename);
	  file_open_error(str, fd);
	  clear_error_if_open_changes(fd->dialog, fd);
	  FREE(str);
	}
    }
}

static void file_mkdir_callback(Widget w, XtPointer context, XtPointer info)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename = NULL;
  filename = XmTextGetString(FSB_BOX(fd->dialog, XmDIALOG_TEXT));
  if (mkdir(filename, 0777) < 0)
    {
      /* could not make the directory */
      char *str;
      str = mus_format(_("can't make %s: %s"), filename, strerror(errno));
      file_open_error(str, fd);
      clear_error_if_open_changes(fd->dialog, fd);
      FREE(str);
    }
  else
    {
      /* set FSB to new dir and force update */
      char *filter;
      filter = mus_format("%s*", filename); /* already has the "/" at the end */
      update_dir_list(fd->dialog, filter);
      FREE(filter);
      XtSetSensitive(w, false);
    }
  XtFree(filename);
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
      XmString cancel_label;
      odat = make_file_dialog(read_only, title, select_title, file_open_ok_callback, open_file_help_callback);
      set_dialog_widget(FILE_OPEN_DIALOG, odat->dialog);

      /* add "Mkdir" button */
      {
	int n;
	Arg args[12];
	n = 0;
	XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
	XtSetArg(args[n], XmNarmColor,   ss->sgx->pushed_button_color); n++;
	odat->mkdirB = XtCreateManagedWidget(_("Mkdir"), xmPushButtonGadgetClass, odat->dialog, args, n);
	XtAddCallback(odat->mkdirB, XmNactivateCallback, file_mkdir_callback, (XtPointer)odat);
	XtSetSensitive(odat->mkdirB, false);
      }

      cancel_label = XmStringCreateLocalized(_("Cancel"));
      XtVaSetValues(odat->dialog, XmNcancelLabelString, cancel_label, NULL);
      XmStringFree(cancel_label);
    }
  else
    {
      if (odat->file_dialog_read_only != read_only)
	{
	  XmString s1, s2;
	  s1 = XmStringCreateLocalized(select_title);
	  s2 = XmStringCreateLocalized(title);
	  XtVaSetValues(odat->dialog, 
			XmNselectionLabelString, s1, 
			XmNdialogTitle, s2, 
			NULL);
	  XmStringFree(s1);
	  XmStringFree(s2);
	  odat->file_dialog_read_only = read_only;
	}
      if (odat->fp->reread_directory) 
	{
	  force_directory_reread(odat->dialog);
	  odat->fp->reread_directory = false;
	}
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
      file_open_error(_("no filename given"), fd);
      clear_error_if_open_changes(fd->dialog, fd);
    }
  else
    {
      file_dialog_stop_playing(fd->dp);
      if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
	{
	  int id_or_error;
	  snd_info *sp;
	  sp = any_selected_sound();
	  redirect_snd_error_to(redirect_file_open_error, (void *)fd);
	  ss->sgx->requestor_dialog = w;
	  ss->open_requestor = FROM_MIX_DIALOG;
	  ss->open_requestor_data = NULL;
	  id_or_error = mix_complete_file_at_cursor(sp, filename, with_mix_tags(ss), 0);
	  /* "id_or_error" here is either one of the mix id's or an error indication such as MIX_FILE_NO_MIX */
	  /*    the possible error conditions have been checked already, or go through snd_error */
	  redirect_snd_error_to(NULL, NULL);
	  if (id_or_error < 0) /* actually -1 .. -3 */
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  clear_error_if_open_changes(fd->dialog, fd);
		  if (id_or_error == MIX_FILE_NO_FILE)
		    start_unsound_watcher(fd, filename);
		}
	    }
	  else 
	    {
	      report_in_minibuffer(sp, _("%s mixed in at cursor"), filename);
	      remember_filename(filename, fd->fpop->file_text_names);
	    }
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format(_("%s is a directory"), filename);
	  file_open_error(str, fd);
	  clear_error_if_open_changes(fd->dialog, fd);
	  FREE(str);
	}
    }
}
  
static void mix_file_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  mix_file_dialog_help();
}

static void file_open_file_watcher(ss_watcher_reason_t reason, void *data)
{
  file_dialog_info *fdat = (file_dialog_info *)data;
  if ((fdat->dialog) &&
      (XtIsManaged(fdat->dialog)))
    set_sensitive(FSB_BOX(fdat->dialog, XmDIALOG_OK_BUTTON), (bool)any_selected_sound());
}

static file_dialog_info *mdat = NULL;

widget_t make_mix_file_dialog(bool managed)
{
  /* called from the menu */
  if (!mdat)
    {
      mdat = make_file_dialog(true, _("Mix Sound"), _("mix in:"), file_mix_ok_callback, mix_file_help_callback);
      set_dialog_widget(FILE_MIX_DIALOG, mdat->dialog);
      mdat->open_file_watcher_loc = add_ss_watcher(SS_FILE_OPEN_WATCHER, file_open_file_watcher, (void *)mdat);
    }
  else
    {
      if (mdat->fp->reread_directory) 
	{
	  force_directory_reread(mdat->dialog);
	  mdat->fp->reread_directory = false;
	}
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
      file_open_error(_("no filename given"), fd);
      clear_error_if_open_changes(fd->dialog, fd);
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
	  ss->open_requestor_data = NULL;
	  redirect_snd_error_to(redirect_file_open_error, (void *)fd);
	  ok = insert_complete_file_at_cursor(sp, filename);
	  redirect_snd_error_to(NULL, NULL);
	  if (!ok)
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  char *fullname;
		  clear_error_if_open_changes(fd->dialog, fd);
		  /* ideally insert_complete_file would return an indication of what the error was... */
		  fullname = mus_expand_filename(filename);
		  if (!(mus_file_probe(fullname)))
		    start_unsound_watcher(fd, filename);
		  FREE(fullname);
		}
	    }
	  else 
	    {
	      report_in_minibuffer(sp, _("%s inserted at cursor"), filename);
	      remember_filename(filename, fd->fpop->file_text_names);
	    }
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format(_("%s is a directory"), filename);
	  file_open_error(str, fd);
	  clear_error_if_open_changes(fd->dialog, fd);
	  FREE(str);
	}
    }
}
  
static void insert_file_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  insert_file_dialog_help();
}

static file_dialog_info *idat = NULL;

widget_t make_insert_file_dialog(bool managed)
{
  if (!idat)
    {
      idat = make_file_dialog(true, _("Insert Sound"), _("insert:"), file_insert_ok_callback, insert_file_help_callback);
      set_dialog_widget(FILE_INSERT_DIALOG, idat->dialog);
      idat->open_file_watcher_loc = add_ss_watcher(SS_FILE_OPEN_WATCHER, file_open_file_watcher, (void *)idat);
    }
  else
    {
      if (idat->fp->reread_directory) 
	{
	  force_directory_reread(idat->dialog);
	  idat->fp->reread_directory = false;
	}
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
  if (ss->fam_ok) return;
  /* ideally this would include the save-as dialogs */
  if (odat)
    {
      odat->fp->reread_directory = true;
      if (XtIsManaged(odat->dialog))
	{
	  force_directory_reread(odat->dialog);
	  odat->fp->reread_directory = false;
	}
    }
  if (mdat)
    {
      mdat->fp->reread_directory = true;
      if (XtIsManaged(mdat->dialog))
	{
	  force_directory_reread(mdat->dialog);
	  mdat->fp->reread_directory = false;
	}
    }
  if (idat)
    {
      idat->fp->reread_directory = true;
      if (XtIsManaged(idat->dialog))
	{
	  force_directory_reread(idat->dialog);
	  idat->fp->reread_directory = false;
	}
    }
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
	  (*srate) = string_to_int(str, 1, "srate"); 
	  XtFree(str);
	}
      else snd_error_without_format("no srate?");
    }

  if ((chans) && (fdat->chans_text))
    {
      str = XmTextGetString(fdat->chans_text); 
      fdat->scanf_widget = CHANS_WIDGET;
      if ((str) && (*str))
	{
	  (*chans) = string_to_int(str, min_chan, "chans"); 
	  XtFree(str);
	}
      else
	{
	  if (min_chan > 0)
	    snd_error_without_format("no chans?");
	}
    }
  
  if ((location) && (fdat->location_text))
    {
      str = XmTextGetString(fdat->location_text); 
      fdat->scanf_widget = DATA_LOCATION_WIDGET;
      if ((str) && (*str))
	{
	  (*location) = string_to_off_t(str, 0, "data location"); 
	  XtFree(str);
	}
      else snd_error_without_format("no data location?");
    }

  if ((samples) && (fdat->samples_text))
    {
      str = XmTextGetString(fdat->samples_text); 
      fdat->scanf_widget = SAMPLES_WIDGET;
      if ((str) && (*str))
	{
	  (*samples) = string_to_off_t(str, 0, "samples"); 
	  XtFree(str);
	}
      else snd_error_without_format("no samples?");
    }
  fdat->scanf_widget = SAMPLES_WIDGET;

  if ((type) && (fdat->header_list))
    {
      res = XmListGetSelectedPos(fdat->header_list, &ns, &n);
      if (res)
	{
	  (*type) = position_to_type(ns[0] - 1);
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
	  (*format) = position_to_format(fdat->current_type, ns[0] - 1);
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
  fl = type_and_format_to_position(fdat, fdat->current_type, fdat->current_format);
  if (fl == NULL) return;
  
  if ((type != IGNORE_HEADER_TYPE) &&
      (fdat->header_list))
    {
      XmListSelectPos(fdat->header_list, fdat->header_pos + 1, false);
      ensure_list_row_visible(fdat->header_list, fdat->header_pos + 1);
    }

  strs = (XmString *)MALLOC(fdat->formats * sizeof(XmString)); 
  for (i = 0; i < fdat->formats; i++) 
    strs[i] = XmStringCreateLocalized(fl[i]);
  XtVaSetValues(fdat->format_list, 
		XmNitems, strs, 
		XmNitemCount, fdat->formats, 
		NULL);
  for (i = 0; i < fdat->formats; i++)
    XmStringFree(strs[i]);
  FREE(strs); 
  XmListSelectPos(fdat->format_list, fdat->format_pos + 1, false);
  ensure_list_row_visible(fdat->format_list, fdat->format_pos + 1);

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

static void post_file_dialog_error(const char *error_msg, file_data *fd)
{
  XmString msg;
  msg = XmStringCreateLocalized((char *)error_msg);
  XtVaSetValues(fd->error_text, 
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);
  show_dialog_error(fd);
}

static void redirect_post_file_dialog_error(const char *error_msg, void *ufd)
{
  post_file_dialog_error(error_msg, (file_data *)ufd);
}

static void filename_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  Widget dialog_filename_text;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(fd->dialog), fd->dialog);
  clear_dialog_error(fd);
  dialog_filename_text = FSB_BOX(fd->dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) XtRemoveCallback(dialog_filename_text, XmNmodifyVerifyCallback, filename_modify_callback, context);
  cbs->doit = true;
}

static void clear_error_if_filename_changes(Widget dialog, file_data *data)
{
  Widget dialog_filename_text;
  ASSERT_WIDGET_TYPE(XmIsFileSelectionBox(dialog), dialog);
  dialog_filename_text = FSB_BOX(dialog, XmDIALOG_TEXT);
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

static void clear_error_if_chans_changes(Widget dialog, file_data *fd)
{
  if (fd->chans_text) XtAddCallback(fd->chans_text, XmNmodifyVerifyCallback, chans_modify_callback, (XtPointer)fd);
}

static void panel_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  clear_dialog_error(fd);
  XtRemoveCallback(w, XmNmodifyVerifyCallback, panel_modify_callback, context);
  cbs->doit = true;
}

static void clear_error_if_panel_changes(Widget dialog, file_data *fd)
{
  Widget baddy;
  switch (fd->error_widget)
    {
    case SRATE_WIDGET:         baddy = fd->srate_text;    break;
    case DATA_LOCATION_WIDGET: baddy = fd->location_text; break;
    case SAMPLES_WIDGET:       baddy = fd->samples_text;  break;
    default:                   baddy = fd->chans_text;    break;
    }
  if (baddy) XtAddCallback(baddy, XmNmodifyVerifyCallback, panel_modify_callback, (XtPointer)fd);
}

static void post_file_panel_error(const char *error_msg, void *ufd)
{
  file_data *fd = (file_data *)ufd;
  fd->error_widget = fd->scanf_widget;
  post_file_dialog_error(error_msg, fd);
}

static void file_data_type_callback(Widget w, XtPointer context, XtPointer info) 
{
  int pos;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  file_data *fd;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  XtVaGetValues(w, XmNuserData, &fd, NULL);
  pos = cbs->item_position - 1;
  if (position_to_type(pos) != fd->current_type)
    {
      position_to_type_and_format(fd, pos);
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
  fd->current_format = position_to_format(fd->current_type, cbs->item_position - 1);
}


/* ---------------- File Data Panel ---------------- */

static void srate_drop(Widget w, XtPointer context, XtPointer info) 
{
  file_data *fd = (file_data *)context;
  char *sr;
  sr = XtName(w);
  XmTextSetString(fd->srate_text, sr);
}

static void add_srate_menu(file_data *fd, const char *srate_name)
{
  int n;
  Arg args[12];
  if (fd->srates_size == 0)
    {
      fd->srates_size = 8;
      fd->srates = (Widget *)CALLOC(fd->srates_size, sizeof(Widget));
    }
  else
    {
      if (fd->srates_size == fd->num_srates)
	{
	  fd->srates_size += 8;
	  fd->srates = (Widget *)REALLOC(fd->srates, fd->srates_size * sizeof(Widget));
	}
    }
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  fd->srates[fd->num_srates] = XtCreateManagedWidget(srate_name, xmPushButtonWidgetClass, fd->smenu, args, n);
  XtAddCallback(fd->srates[fd->num_srates++], XmNactivateCallback, srate_drop, (XtPointer)fd);
}

static void make_srate_menu(Widget w, XtPointer context, XtPointer info) 
{
  file_data *fd = (file_data *)context;
  list_completer_info *cur_srates;
  cur_srates = srate_list();
  if (fd->num_srates < cur_srates->num_values)
    {
      int i;
      for (i = fd->num_srates; i < cur_srates->num_values; i++)
	add_srate_menu(fd, cur_srates->values[i]);
    }
}

static void chans_drop(Widget w, XtPointer context, XtPointer info) 
{
  file_data *fd = (file_data *)context;
  char *sr;
  sr = XtName(w);
  XmTextSetString(fd->chans_text, sr);
}

#define PANEL_COMMENT_SPACE 8

file_data *make_file_data_panel(Widget parent, const char *name, Arg *in_args, int in_n, 
				dialog_channels_t with_chan, 
				int header_type, int data_format,
				dialog_data_location_t with_loc, dialog_samples_t with_samples,
				dialog_error_t with_error, dialog_header_type_t with_header_type,
				dialog_comment_t with_comment,
				header_choice_t header_choice)
{
  Widget form, header_label, data_label, srate_label, chans_label, sep1, sep2 = NULL, sep3, sep4;
  Widget comment_label = NULL, location_label, samples_label;
  Widget scascade, cmenu, c1 = NULL, c2 = NULL, c4 = NULL, c8 = NULL;
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
  formats = type_and_format_to_position(fdat, header_type, data_format);
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
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_button_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep1); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      header_label = XtCreateManagedWidget(_("header"), xmLabelWidgetClass, form, args, n);
      
      /* what is selected depends on current type */
      strs = (XmString *)CALLOC(nheaders, sizeof(XmString)); 
      for (i = 0; i < nheaders; i++) 
	strs[i] = XmStringCreateLocalized(headers[i]);

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
  XtSetArg(args[n], XmNbackground, ss->sgx->help_button_color); n++;
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
    strs[i] = XmStringCreateLocalized(formats[i]);
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
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  fdat->smenu = XmCreatePulldownMenu(srate_label, _("srate:"), args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNsubMenuId, fdat->smenu); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  scascade = XtCreateManagedWidget(_("srate:"), xmCascadeButtonWidgetClass, srate_label, args, n);
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
  fdat->srate_text = make_textfield_widget("srate-text", form, args, n, NOT_ACTIVATABLE, add_completer_func(srate_completer, NULL));

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
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      cmenu = XmCreatePulldownMenu(chans_label, (char *)((with_chan == WITH_CHANNELS_FIELD) ? _("channels:") : _("extract channel:")), 
				   args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNsubMenuId, cmenu); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      XtCreateManagedWidget((char *)((with_chan == WITH_CHANNELS_FIELD) ? _("channels:") : _("extract channel:")), 
			    xmCascadeButtonWidgetClass, chans_label, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
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
      XtSetArg(args[n], XmNcolumns, 8); n++;
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
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep4); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      comment_label = XtCreateManagedWidget(_("comment:"), xmLabelWidgetClass, parent, args, n);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, comment_label); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, comment_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrows, 2); n++;
      XtSetArg(args[n], XmNcolumns, 16); n++; /* this sets the lower size, so we don't want it too big */
      fdat->comment_text = make_text_widget("comment-text", parent, args, n);
    }
  else fdat->comment_text = NULL;

  XtAddCallback(scascade, XmNcascadingCallback, make_srate_menu, (XtPointer)fdat);

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
  Widget dialog, filename_widget, extractB, mkdirB;
  char *filename;
  save_dialog_t type;
  file_pattern_info *fp;
  dialog_play_info *dp;
  fam_info *file_watcher;
  int selection_watcher_loc;
  file_popup_info *fpop;
} save_as_dialog_info;

static save_as_dialog_info *save_sound_as = NULL, *save_selection_as = NULL, *save_region_as = NULL;

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

void reflect_region_in_save_as_dialog(void)
{
  if ((save_region_as) &&
      (save_region_as->dialog) &&
      (XtIsManaged(save_region_as->dialog)) &&
      (region_ok(region_dialog_region())))
    clear_dialog_error(save_region_as->panel_data);
}

static void save_as_filename_modify_callback(Widget w, XtPointer context, XtPointer info);

static void save_as_undoit(save_as_dialog_info *sd)
{
  XmString ok_label;
  ok_label = XmStringCreateLocalized(_("Save"));
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

static void clear_error_if_save_as_filename_changes(Widget dialog, save_as_dialog_info *sd)
{
  XtAddCallback(sd->filename_widget, XmNmodifyVerifyCallback, save_as_filename_modify_callback, (XtPointer)sd);
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

static void save_or_extract(save_as_dialog_info *sd, bool saving)
{
  char *str = NULL, *comment = NULL, *msg = NULL, *fullname = NULL, *tmpfile = NULL;
  snd_info *sp = NULL;
  int type = MUS_NEXT, format = MUS_BSHORT, srate = 22050, chans = 1, output_type, chan = 0, extractable_chans = 0;
  bool file_exists = false;
  off_t location = 28, samples = 0;
  io_error_t io_err = IO_NO_ERROR;

  clear_dialog_error(sd->panel_data);

  if ((sd->type == SELECTION_SAVE_AS) &&
      (!(selection_is_active())))
    {
      if (saving)
	msg = _("no selection to save");
      else msg = _("can't extract: no selection");
      post_file_dialog_error((const char *)msg, sd->panel_data);
      if (sd->selection_watcher_loc < 0)
	sd->selection_watcher_loc = add_selection_watcher(save_as_selection_watcher, (void *)sd);
      return;
    }

  if ((sd->type == REGION_SAVE_AS) &&
      (!(region_ok(region_dialog_region()))))
    {
      post_file_dialog_error(_("no region to save"), sd->panel_data);
      return;
    }

  sp = any_selected_sound();
  if ((!sp) && 
      (sd->type != REGION_SAVE_AS))
    {
      if (saving)
	msg = _("nothing to save");
      else msg = _("nothing to extract");
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data);
      return;
    }

  /* get output filename */
  str = XmTextGetString(sd->filename_widget);
  if ((!str) || (!*str))
    {
      if (saving)
	msg = _("can't save: no file name given");
      else msg = _("can't extract: no file name given");
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data);
      return;
    }

  /* get output file attributes */
  redirect_snd_error_to(post_file_panel_error, (void *)(sd->panel_data));
  if (saving)
    comment = get_file_dialog_sound_attributes(sd->panel_data, &srate, &chans, &type, &format, &location, &samples, 0);
  else comment = get_file_dialog_sound_attributes(sd->panel_data, &srate, &chan, &type, &format, &location, &samples, 0);
  output_type = type;
  redirect_snd_error_to(NULL, NULL);
  if (sd->panel_data->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(sd->dialog, sd->panel_data);
      if (comment) FREE(comment);
      XtFree(str);
      return;
    }

  switch (sd->type)
    {
    case SOUND_SAVE_AS:
      clear_minibuffer(sp);
      if (!saving)
	extractable_chans = sp->nchans;
      break;
    case SELECTION_SAVE_AS:
      if (!saving)
	extractable_chans = selection_chans();
      break;
    default:
      break;
    }

  if (!saving)
    {
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
	  post_file_dialog_error((const char *)msg, sd->panel_data);
	  clear_error_if_chans_changes(sd->dialog, sd->panel_data);
	  FREE(msg);
	  if (comment) FREE(comment);
	  XtFree(str);
	  return;
	}
    }

  fullname = mus_expand_filename(str);
  if (run_before_save_as_hook(sp, fullname, sd->type != SOUND_SAVE_AS, srate, type, format, comment))
    {
      msg = mus_format(_("%s cancelled by %s"), (saving) ? "save" : "extract", S_before_save_as_hook);
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data);      
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
	  post_file_dialog_error((const char *)msg, sd->panel_data);
	  clear_error_if_filename_changes(sd->dialog, sd->panel_data); 
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
	      post_file_dialog_error((const char *)msg, sd->panel_data);
	      clear_error_if_save_as_filename_changes(sd->dialog, sd);
	      ok_label = XmStringCreateLocalized(_("DoIt"));
	      XtVaSetValues(sd->dialog, 
			    XmNokLabelString, ok_label, 
			    NULL);
	      XmUpdateDisplay(FSB_BOX(sd->dialog, XmDIALOG_OK_BUTTON));
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

  redirect_snd_error_to(redirect_post_file_dialog_error, (void *)(sd->panel_data));
  switch (sd->type)
    {
    case SOUND_SAVE_AS:
      if (saving)
	io_err = save_edits_without_display(sp, tmpfile, type, format, srate, comment, AT_CURRENT_EDIT_POSITION);
      else io_err = save_channel_edits(sp->chans[chan], tmpfile, AT_CURRENT_EDIT_POSITION); /* protects if same name */
      break;
    case SELECTION_SAVE_AS:
      {
	char *ofile;
	if (file_exists) /* file won't exist if we're encoding, so this isn't as wasteful as it looks */
	  ofile = snd_tempnam();
	else ofile = copy_string(tmpfile);
	io_err = save_selection(ofile, type, format, srate, comment, (saving) ? SAVE_ALL_CHANS : chan);
	if (io_err == IO_NO_ERROR)
	  io_err = move_file(ofile, fullname);
	FREE(ofile);
	break;
      }
    case REGION_SAVE_AS:
      {
	char *ofile;
	if (region_ok(region_dialog_region()))
	  {
	    if (file_exists)
	      ofile = snd_tempnam();
	    else ofile = copy_string(tmpfile);
	    io_err = save_region(region_dialog_region(), ofile, type, format, comment);
	    if (io_err == IO_NO_ERROR)
	      io_err = move_file(ofile, fullname);
	    FREE(ofile);
	  }
	break;
      default:
	snd_error("internal screw up");
	break;
      }
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
      remember_filename(fullname, sd->fpop->file_text_names);

      if (!file_exists)
	force_directory_reread(sd->dialog);
      if (saving)
	{
	  if (sd->type == SOUND_SAVE_AS)
	    report_in_minibuffer(sp, "%s saved as %s", sp->short_filename, str);
	  else report_in_minibuffer(sp, "%s saved as %s", (sd->type == SELECTION_SAVE_AS) ? "selection" : "region", str);
	}
      else
	{
	  if (sd->type == SOUND_SAVE_AS)
	    report_in_minibuffer(sp, "%s chan %d saved as %s", sp->short_filename, chan, str);
	  else report_in_minibuffer(sp, "selection chan %d saved as %s", chan, str);
	}
      run_after_save_as_hook(sp, str, true); /* true => from dialog */
      XtUnmanageChild(sd->dialog);
    }
  else
    {
      msg = mus_format("%s as %s: %s (%s)", (saving) ? "save" : "extract chan", str, io_error_name(io_err), snd_io_strerror());
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data);
      FREE(msg);
    }

  FREE(fullname);
  XtFree(str);
  if (comment) FREE(comment);
}


static void save_as_ok_callback(Widget w, XtPointer context, XtPointer info)
{ 
  save_or_extract((save_as_dialog_info *)context, true);
}

static void save_as_extract_callback(Widget w, XtPointer context, XtPointer info) 
{
  save_or_extract((save_as_dialog_info *)context, false);
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
	    s1 = XmStringCreateLocalized(_("save as (file write-protected?):"));
	  else
#endif
	  s1 = XmStringCreateLocalized(_("save as (overwriting):"));
	}
      else
	{
	  if (!(directory_exists(filename)))
	    s1 = XmStringCreateLocalized(_("save as (no such directory?):"));
	  else s1 = XmStringCreateLocalized(_("save as:"));
	}
    }
  else s1 = XmStringCreateLocalized(_("save as:"));
  XtVaSetValues(dialog, 
		XmNselectionLabelString, s1, 
		NULL);
  if (filename) XtFree(filename);
}

static void save_as_mkdir_callback(Widget w, XtPointer context, XtPointer info)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  char *filename = NULL;
  filename = XmTextGetString(FSB_BOX(sd->dialog, XmDIALOG_TEXT));
  if (mkdir(filename, 0777) < 0)
    {
      /* could not make the directory */
      char *str;
      str = mus_format(_("can't make %s: %s"), filename, strerror(errno));
      post_file_dialog_error((const char *)str, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data); 
      FREE(str);
    }
  else
    {
      /* set FSB to new dir and force update */
      char *filter;
      filter = mus_format("%s*", filename); /* already has the "/" at the end */
      update_dir_list(sd->dialog, filter);
      FREE(filter);
      XtSetSensitive(w, false);
    }
  XtFree(filename);
}

static void reflect_text_in_save_button(Widget w, XtPointer context, XtPointer info)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  /* w here is text widget, not button */
  XtSetSensitive(FSB_BOX(sd->dialog, XmDIALOG_OK_BUTTON), (!(file_is_directory(sd->dialog))));
  if (sd->mkdirB) XtSetSensitive(sd->mkdirB, file_is_nonexistent_directory(sd->dialog));
}

static void reflect_text_in_extract_button(Widget w, XtPointer context, XtPointer info)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  /* w here is text widget, not button */
  XtSetSensitive(sd->extractB, (!(file_is_directory(sd->dialog))));
}

static void save_as_filter_text_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  force_directory_reread_and_let_filename_change(sd->dialog);
}


static void make_save_as_dialog(save_as_dialog_info *sd, char *sound_name, int header_type, int format_type)
{
  char *file_string;

  if (!(sd->dialog))
    {
      Arg args[32];
      int n;
      XmString xmstr1, xmstr2, s1;
      XmString filter_list_label, cancel_label;
      Widget extractB, mainform;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      s1 = XmStringCreateLocalized(_("save as:"));
      XtSetArg(args[n], XmNselectionLabelString, s1); n++;

      xmstr1 = XmStringCreateLocalized(_("Save"));
      XtSetArg(args[n], XmNokLabelString, xmstr1); n++;

      file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, _("save %s"), sound_name);

      xmstr2 = XmStringCreateLocalized(file_string);
      XtSetArg(args[n], XmNdialogTitle, xmstr2); n++;

      filter_list_label = XmStringCreateLocalized(_("files listed:"));
      XtSetArg(args[n], XmNfilterLabelString, filter_list_label); n++;

      cancel_label = XmStringCreateLocalized(_("Cancel"));
      XtSetArg(args[n], XmNcancelLabelString, cancel_label); n++;

      sd->fp = (file_pattern_info *)CALLOC(1, sizeof(file_pattern_info));
      sd->fp->in_just_sounds_update = false;
      if (just_sounds(ss))
	sd->fp->filter_choice = JUST_SOUNDS_FILTER;
      else sd->fp->filter_choice = NO_FILE_FILTER;

      sd->dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));
      sd->fpop = (file_popup_info *)CALLOC(1, sizeof(file_popup_info));
      sd->fpop->fp = sd->fp;

      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNchildPlacement, XmPLACE_ABOVE_SELECTION); n++;
      XtSetArg(args[n], XmNallowOverlap, false); n++;
      XtSetArg(args[n], XmNheight, 600); n++;
      XtSetArg(args[n], XmNuserData, (XtPointer)sd->fp); n++;
      XtSetArg(args[n], XmNfileFilterStyle, XmFILTER_HIDDEN_FILES); n++;
      XtSetArg(args[n], XmNfileSearchProc, snd_directory_reader); n++;        /* over-ride Motif's directory reader altogether */      

      sd->dialog = XmCreateFileSelectionDialog(MAIN_SHELL(ss), "save-as", args, n);
      sd->fp->dialog = sd->dialog;
      sd->dp->dialog = sd->dialog;
      sd->fpop->dialog = sd->dialog;

      FREE(file_string);

      XtUnmanageChild(FSB_BOX(sd->dialog, XmDIALOG_DIR_LIST_LABEL));
      XtUnmanageChild(FSB_BOX(sd->dialog, XmDIALOG_LIST_LABEL));
      XtUnmanageChild(FSB_BOX(sd->dialog, XmDIALOG_APPLY_BUTTON));

      XmStringFree(s1);
      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      XmStringFree(filter_list_label);
      XmStringFree(cancel_label);

#if HAVE_FAM
      {
	char *our_dir;
	XmString cur_dir;
	XtVaGetValues(sd->dialog, XmNdirectory, &cur_dir, NULL);
	our_dir = (char *)XmStringUnparse(cur_dir, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	sd->fp->directory_watcher = fam_monitor_directory(our_dir, (void *)(sd->fp), watch_current_directory_contents);
	/* don't set last_dir yet */
	XtFree(our_dir);
      }
#endif

      sd->filename_widget = FSB_BOX(sd->dialog, XmDIALOG_TEXT);
      XtAddCallback(sd->dialog, XmNhelpCallback, save_as_help_callback, (XtPointer)sd);
      XtAddCallback(sd->dialog, XmNcancelCallback, save_as_cancel_callback, (XtPointer)sd);
      XtAddCallback(sd->dialog, XmNokCallback, save_as_ok_callback, (XtPointer)sd);

      mainform = XtVaCreateManagedWidget("filebuttons-mainform", xmFormWidgetClass, sd->dialog, NULL);
      add_play_and_just_sounds_buttons(sd->dialog, mainform, sd->fp, sd->dp);

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sd->fp->just_sounds_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      sd->panel_data = make_file_data_panel(mainform, "data-form", args, n, 
					    (sd->type == REGION_SAVE_AS) ? WITHOUT_CHANNELS_FIELD : WITH_EXTRACT_CHANNELS_FIELD, 
					    header_type, format_type, 
					    WITHOUT_DATA_LOCATION_FIELD, 
					    WITHOUT_SAMPLES_FIELD,
					    WITH_ERROR_FIELD, 
					    WITH_HEADER_TYPE_FIELD, 
					    WITH_COMMENT_FIELD,
					    WITH_WRITABLE_HEADERS);

      sd->panel_data->dialog = sd->dialog;

      color_file_selection_box(sd->dialog);
      XtVaSetValues(sd->panel_data->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(sd->panel_data->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(sd->fp->just_sounds_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(sd->dp->play_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);

      XtAddCallback(FSB_BOX(sd->dialog, XmDIALOG_LIST),
		    XmNbrowseSelectionCallback, save_as_dialog_select_callback, (XtPointer)(sd->dp));
      XtAddCallback(sd->filename_widget, XmNvalueChangedCallback, save_as_file_exists_check, (XtPointer)(sd->dialog));
      XtAddCallback(FSB_BOX(sd->dialog, XmDIALOG_FILTER_TEXT), XmNactivateCallback, save_as_filter_text_activate_callback, (void *)sd);

      {
	Widget wtmp;
	wtmp = FSB_BOX(sd->dialog, XmDIALOG_DIR_LIST);
	if (wtmp) XtAddCallback(wtmp, XmNbrowseSelectionCallback, file_change_directory_callback, (XtPointer)(sd->fp));
      }

      add_file_popups(sd->fpop);

      /* this must come after the file data panel so that Motif puts it in the button box, not the main work area */
      if (sd->type != REGION_SAVE_AS)
	{
	  /* add "Extract" button */
	  n = 0;
	  XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
	  XtSetArg(args[n], XmNarmColor,   ss->sgx->pushed_button_color); n++;
	  extractB = XtCreateManagedWidget(_("Extract"), xmPushButtonGadgetClass, sd->dialog, args, n);
	  XtAddCallback(extractB, XmNactivateCallback, save_as_extract_callback, (XtPointer)sd);
	  sd->extractB = extractB;

	  XtSetSensitive(extractB, (!(file_is_directory(sd->dialog))));
	  XtAddCallback(FSB_BOX(sd->dialog, XmDIALOG_TEXT), XmNvalueChangedCallback, reflect_text_in_extract_button, (void *)sd);
	}
	 
      /* add "Mkdir" button */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
      XtSetArg(args[n], XmNarmColor,   ss->sgx->pushed_button_color); n++;
      sd->mkdirB = XtCreateManagedWidget(_("Mkdir"), xmPushButtonGadgetClass, sd->dialog, args, n);
      XtAddCallback(sd->mkdirB, XmNactivateCallback, save_as_mkdir_callback, (XtPointer)sd);
      XtSetSensitive(sd->mkdirB, false);

      XtSetSensitive(FSB_BOX(sd->dialog, XmDIALOG_OK_BUTTON), (!(file_is_directory(sd->dialog))));
      XtAddCallback(FSB_BOX(sd->dialog, XmDIALOG_TEXT), XmNvalueChangedCallback, reflect_text_in_save_button, (void *)sd);

      XtManageChild(sd->dialog);
      switch (sd->type)
	{
	case SOUND_SAVE_AS:
	  set_dialog_widget(SOUND_SAVE_AS_DIALOG, sd->dialog);
	  break;
	case SELECTION_SAVE_AS:
	  set_dialog_widget(SELECTION_SAVE_AS_DIALOG, sd->dialog);
	  break;
	case REGION_SAVE_AS:
	  set_dialog_widget(REGION_SAVE_AS_DIALOG, sd->dialog);
	  break;
	default:
	  snd_error("internal screw up");
	  break;
	}
    }
  else
    {
      XmString xmstr2;
      file_string = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, _("save %s"), sound_name);
      xmstr2 = XmStringCreateLocalized(file_string);
      XtVaSetValues(sd->dialog, 
		    XmNdialogTitle, xmstr2, 
		    NULL);
      XmStringFree(xmstr2);
      FREE(file_string);
    }
}

widget_t make_sound_save_as_dialog(bool managed)
{
  /* should the save-as dialog, at least in the file case, reflect the current file attributes/comment?
   *          or should we have a save-as-hook that can set up the dialog fields? 
   */

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
  if (sd->fp->reread_directory) 
    {
      force_directory_reread(sd->dialog);
      sd->fp->reread_directory = false;
    }

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
  if (sd->fp->reread_directory) 
    {
      force_directory_reread(sd->dialog);
      sd->fp->reread_directory = false;
    }
  if ((managed) && (!XtIsManaged(sd->dialog))) 
    XtManageChild(sd->dialog);
  return(sd->dialog);
}

widget_t make_region_save_as_dialog(bool managed)
{
  save_as_dialog_info *sd;
  char *comment = NULL;

  if (!save_region_as)
    save_region_as = new_save_as_dialog_info(REGION_SAVE_AS);
  sd = save_region_as;

  make_save_as_dialog(sd,
		      _("selected region"),
		      default_output_header_type(ss),
		      default_output_data_format(ss));
  comment = region_description(region_dialog_region());
  set_file_dialog_sound_attributes(sd->panel_data,
				   sd->panel_data->current_type,
				   sd->panel_data->current_format,
				   region_srate(region_dialog_region()), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   comment);
  if (sd->fp->reread_directory) 
    {
      force_directory_reread(sd->dialog);
      sd->fp->reread_directory = false;
    }
  if ((managed) && (!XtIsManaged(sd->dialog))) 
    XtManageChild(sd->dialog);
  if (comment) FREE(comment);
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
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_open_file_dialog);
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
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_mix_file_dialog);
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
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_insert_file_dialog);
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
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_save_sound_dialog);
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
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_save_selection_dialog);
#endif
    }
  if ((save_region_as) && (XtIsManaged(save_region_as->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_save_region_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_save_region_dialog));
#endif
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_save_region_dialog);
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
  ok_label = XmStringCreateLocalized(_("Ok"));
  XtVaSetValues(new_file_dialog, 
		XmNokLabelString, ok_label, 
		NULL);
  XmStringFree(ok_label);
  clear_dialog_error(ndat);
  XtRemoveCallback(new_file_text, XmNmodifyVerifyCallback, new_filename_modify_callback, NULL);
  new_file_watcher = fam_unmonitor_file(new_file_filename, new_file_watcher);
}

static void new_filename_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  new_file_undoit();
  cbs->doit = true;
}

static void clear_error_if_new_filename_changes(Widget dialog)
{
  XtAddCallback(new_file_text, XmNmodifyVerifyCallback, new_filename_modify_callback, NULL);
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
      post_file_dialog_error((const char *)msg, ndat);
      clear_error_if_new_filename_changes(new_file_dialog);
    }
  else
    {
      redirect_snd_error_to(post_file_panel_error, (void *)ndat);
      comment = get_file_dialog_sound_attributes(ndat, &srate, &chans, &header_type, &data_format, &loc, &initial_samples, 1);
      redirect_snd_error_to(NULL, NULL);
      if (ndat->error_widget != NOT_A_SCANF_WIDGET)
	{
	  clear_error_if_panel_changes(new_file_dialog, ndat);
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
	      post_file_dialog_error((const char *)msg, ndat);
	      clear_error_if_new_filename_changes(new_file_dialog);
	      ok_label = XmStringCreateLocalized(_("DoIt"));
	      XtVaSetValues(new_file_dialog, 
			    XmNokLabelString, ok_label, 
			    NULL);
	      XmUpdateDisplay(MSG_BOX(new_file_dialog, XmDIALOG_OK_BUTTON));
	      XmStringFree(ok_label);
	      FREE(msg);
	    }
	  else
	    {
	      if (new_file_watcher)
		new_file_undoit();
	      ss->local_errno = 0;
	      redirect_snd_error_to(redirect_post_file_dialog_error, (void *)ndat);
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
		  clear_error_if_new_filename_changes(new_file_dialog);
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

static char *new_file_dialog_filename(int header_type)
{
  static int new_file_dialog_file_ctr = 1;
  char *filename = NULL, *extension = NULL;
  filename = (char *)CALLOC(64, sizeof(char));
  switch (header_type)
    {
    case MUS_AIFC: extension = "aiff"; break;
    case MUS_AIFF: extension = "aiff"; break;
    case MUS_RIFF: extension = "wav";  break;
    case MUS_RF64: extension = "wav";  break;
    default:       extension = "snd";  break;
    }
  mus_snprintf(filename, 64, _("new-%d.%s"), new_file_dialog_file_ctr++, extension);
  return(filename);
}

static void load_new_file_defaults(char *newname)
{
  char *filename = NULL, *new_comment = NULL;
  int header_type, data_format, chans, srate;

  header_type = default_output_header_type(ss);
  chans =       default_output_chans(ss);
  data_format = default_output_data_format(ss);
  srate =       default_output_srate(ss);
  new_comment = output_comment(NULL);

  if ((newname) && (!(*newname))) newname = NULL;
  filename = output_name(newname); /* calls output-name-hook, always free */
  if (filename == NULL)
    filename = new_file_dialog_filename(header_type);
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

widget_t make_new_file_dialog(bool managed)
{
  if (!new_file_dialog)
    {
      Arg args[20];
      int n;
      XmString xok, xcancel, xhelp;
      Widget name_label, form;
      XmString titlestr;
      Widget sep, reset_button;

      titlestr = XmStringCreateLocalized(_("New file"));
      xhelp = XmStringCreateLocalized(_("Help"));
      xcancel = XmStringCreateLocalized(_("Cancel"));
      xok = XmStringCreateLocalized(_("Ok"));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      reset_button = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, new_file_dialog, args, n);
      XtAddCallback(reset_button, XmNactivateCallback, new_file_reset_callback, NULL);

      n = 0;
      form = XtCreateManagedWidget("newfile", xmFormWidgetClass, new_file_dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
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
      new_file_text = make_textfield_widget("newtext", form, args, n, ACTIVATABLE, add_completer_func(filename_completer, NULL));

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

      map_over_children(new_file_dialog, set_main_color_of_widget);
      XtVaSetValues(ndat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(ndat->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);

      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);

      set_dialog_widget(NEW_FILE_DIALOG, new_file_dialog);
      XtUnmanageChild(ndat->error_text); 

      load_new_file_defaults(NULL);
    }
  else
    {
      char *new_name;
      new_name = XmTextGetString(new_file_text);
#if (!HAVE_FAM)
      if (new_file_watcher)
	{
	  /* if overwrite question pends, but file has been deleted in the meantime, go back to normal state */
	  if ((!new_name) || (!(*new_name)) ||
	      (!(mus_file_probe(new_name))))
	    new_file_undoit();
	}
#endif
      if (strncmp(new_name, "new-", 4) == 0)
	{
	  /* if file is open with currently posted new-file dialog name, and it's our name (new-%d), then tick the counter */
	  snd_info *sp;
	  sp = find_sound(new_name, 0);
	  if (sp)
	    {
	      char *filename;
	      filename = new_file_dialog_filename(default_output_header_type(ss));
	      XmTextSetString(new_file_text, filename);  
	      mus_sound_forget(filename);
	      FREE(filename);
	    }
	}
      if (new_name) XtFree(new_name);
    }
  if ((managed) && 
      (!(XtIsManaged(new_file_dialog))))
    XtManageChild(new_file_dialog);
  return(new_file_dialog);
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
	set_sensitive(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON), (sp->hdr->type == MUS_RAW));
    }
  else 
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, _("Add header to %s"), sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of %s"), sp->short_filename);
      if (ep->dialog)
	set_sensitive(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON), ep->panel_changed);
    }
  xstr = XmStringCreateLocalized(str);
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
    set_sensitive(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON), true);
  ep->panel_changed = true;
}

static void eh_cancel(edhead_info *ep)
{
  unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
  remove_sp_watcher(ep->sp, ep->sp_ro_watcher_loc);
  ep->panel_changed = false;
  if ((ep->file_ro_watcher) &&
      (ep->sp) &&
      (ep->sp->active) &&
      (ep->sp->filename))
    ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
}

static void edit_header_cancel_callback(Widget w, XtPointer context, XtPointer info) 
{
  edhead_info *ep = (edhead_info *)context;
  XtUnmanageChild(ep->dialog);
  eh_cancel(ep);
}

static void edit_header_wm_delete_callback(Widget w, XtPointer context, XtPointer info) 
{
  eh_cancel((edhead_info *)context);
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
      if (XmGetFocusWidget(ep->dialog) == MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON))
	{
	  bool ok;
	  redirect_snd_error_to(redirect_post_file_dialog_error, (void *)(ep->edat));
	  ok = edit_header_callback(ep->sp, ep->edat, redirect_post_file_dialog_error, post_file_panel_error);
	  /* edit_header_callback, if all goes well, writes the header, recopies the data,
	   *   then calls snd_update which closes the sound and reopens it, to force the
	   *   new_header to take effect.  The read-only watcher is disabled during that
	   *   process to keep it from getting a SOUND_IS_CLOSING message from close.
	   */
	  redirect_snd_error_to(NULL, NULL);
	  if (ep->edat->error_widget != NOT_A_SCANF_WIDGET)
	    {
	      clear_error_if_panel_changes(ep->dialog, ep->edat);
	      return;
	    }
	  else
	    {
	      if (!ok)
		{
		  set_sensitive(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON), false);
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
      xstr1 = XmStringCreateLocalized(_("Cancel")); /* needed by template dialog */
      xstr2 = XmStringCreateLocalized(_("Help"));
      xstr3 = XmStringCreateLocalized(_("Save"));
      titlestr = XmStringCreateLocalized(_("Edit Header"));

      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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

      map_over_children(ep->dialog, set_main_color_of_widget);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
      XtVaSetValues(ep->edat->header_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      XtVaSetValues(ep->edat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);

      set_dialog_widget(EDIT_HEADER_DIALOG, ep->dialog);

      {
	Atom wm_delete_window;
	wm_delete_window = XmInternAtom(MAIN_DISPLAY(ss), "WM_DELETE_WINDOW", false);
	XmAddWMProtocolCallback(XtParent(ep->dialog), wm_delete_window, edit_header_wm_delete_callback, (XtPointer)ep);
      }

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
  set_sensitive(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON), (hdr->type == MUS_RAW)); /* nothing needs to be saved when we start */
  XmStringFree(xstr4);
  if (!(XtIsManaged(ep->dialog))) XtManageChild(ep->dialog);
  reflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
  ep->sp_ro_watcher_loc = add_sp_watcher(ep->sp, SP_READ_ONLY_WATCHER, edit_header_watch_user_read_only, (void *)ep);
  ep->file_ro_watcher = fam_monitor_file(ep->sp->filename, (void *)ep, watch_file_read_only);
  return(ep->dialog);
}

void save_edit_header_dialog_state(FILE *fd)
{
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
#if HAVE_FORTH
	    fprintf(fd, "\"%s\" %s %s drop\n", ep->sp->short_filename, S_find_sound, S_edit_header_dialog);
#endif
	    break;
	  }
    }
}

#if MUS_DEBUGGING && HAVE_SCHEME
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
  void *requestor_data;
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
  raw_infos[loc]->requestor_data = NULL;
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
      clear_error_if_panel_changes(rp->dialog, rp->rdat);
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
	   (rp->requestor == FROM_INSERT_DIALOG) ||
	   (rp->requestor == FROM_VIEW_FILES_MIX_DIALOG) ||
	   (rp->requestor == FROM_VIEW_FILES_INSERT_DIALOG)))
	{
	  ss->reloading_updated_file = true; /* don't reread lack-of-header! */
	  /* redirection may be still set here, but I'll make it obvious */
	  switch (rp->requestor)
	    {
	    case FROM_MIX_DIALOG:
	      redirect_snd_error_to(redirect_file_open_error, (void *)mdat);
	      mix_complete_file_at_cursor(any_selected_sound(), rp->filename, with_mix_tags(ss), 0);
	      break;
	    case FROM_INSERT_DIALOG:
	      redirect_snd_error_to(redirect_file_open_error, (void *)idat);
	      insert_complete_file_at_cursor(any_selected_sound(), rp->filename);
	      break;
	    case FROM_VIEW_FILES_MIX_DIALOG:
	      {
		int err;
		view_files_info *vdat = (view_files_info *)(rp->requestor_data);
		redirect_snd_error_to(redirect_vf_post_error, rp->requestor_data);
		err = vf_mix(vdat);
	      }
	      break;
	    case FROM_VIEW_FILES_INSERT_DIALOG:
	      {
		int err;
		view_files_info *vdat = (view_files_info *)(rp->requestor_data);
		redirect_snd_error_to(redirect_vf_post_error, rp->requestor_data);
		err = vf_insert(vdat);
	      }
	      break;
	    default:
	      snd_error("wrong requestor type in raw data dialog? %d\n", (int)(rp->requestor));
	      break;
	    }
	  redirect_snd_error_to(NULL, NULL);
	  ss->reloading_updated_file = false;
	}
      else
	{
	  /* FROM_OPEN_DIALOG (has requestor_dialog)
	   * FROM_KEYBOARD (has sp = requestor_data)
	   * FROM_DRAG_AND_DROP (just open, no needed side effects)
	   * FROM_VIEW_FILES (ditto)
	   * FROM_VIEW_FILES_MIX_DIALOG or INSERT -- requestor_data contains needed info to complete the action
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
	      clear_minibuffer((snd_info *)(rp->requestor_data));
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

static void make_raw_data_dialog(raw_info *rp, const char *title)
{
  XmString xstr1, xstr2, xstr3, xstr4, titlestr;
  int n;
  int raw_srate, raw_chans, raw_data_format;
  Arg args[20];
  Widget reset_button, main_w;

  xstr1 = XmStringCreateLocalized(_("Cancel")); /* needed by template dialog */
  xstr2 = XmStringCreateLocalized(_("Help"));
  xstr3 = XmStringCreateLocalized(_("Ok"));
  if (!title)
    titlestr = XmStringCreateLocalized(_("No header on file"));
  else titlestr = XmStringCreateLocalized((char *)title);
  xstr4 = XmStringCreateLocalized((char *)title);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
  XtSetArg(args[n], XmNbackground, ss->sgx->doit_again_button_color); n++;
  XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
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

  map_over_children(rp->dialog, set_main_color_of_widget);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->doit_button_color,   NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->quit_button_color,   NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
  XtVaSetValues(reset_button, XmNselectColor, ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(rp->rdat->format_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);

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
  rp->requestor_data = ss->open_requestor_data;
  rp->requestor_dialog = ss->sgx->requestor_dialog;
  ss->open_requestor = NO_REQUESTOR;
  ss->sgx->requestor_dialog = NULL;
  ss->open_requestor_data = NULL;
  if ((rp->requestor_dialog) &&
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    XtUnmanageChild(rp->requestor_dialog);
  if (!title) 
    title = mus_format("no header found on %s\n", filename);
  if (!(rp->dialog))
    make_raw_data_dialog(rp, title);
  else
    {
      XmString xstr4;
      xstr4 = XmStringCreateLocalized(title);
      XtVaSetValues(rp->dialog, 
		    XmNmessageString, xstr4, 
		    NULL);
      XmStringFree(xstr4);
    }
  FREE(title);
  if (rp->help) FREE(rp->help);
  if (info)
    {
      XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_HELP_BUTTON), 
		    XmNbackground, ss->sgx->green, 
		    NULL);
      rp->help = copy_string(info);
      FREE(info);
    }
  else
    {
      XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_HELP_BUTTON), 
		    XmNbackground, ss->sgx->help_button_color, 
		    NULL);
      rp->help = NULL;
    }
  raise_dialog(rp->dialog);
  if (!XtIsManaged(rp->dialog)) 
    XtManageChild(rp->dialog);
}


/* ---------------- INFO MONOLOG ---------------- */

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
  XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  post_it_dialog = XmCreateMessageDialog(MAIN_PANE(ss), "info", args, n);

  XtUnmanageChild(MSG_BOX(post_it_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(MSG_BOX(post_it_dialog, XmDIALOG_HELP_BUTTON));
  XtUnmanageChild(MSG_BOX(post_it_dialog, XmDIALOG_SYMBOL_LABEL));

  XtVaSetValues(MSG_BOX(post_it_dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->sgx->help_button_color, NULL);
      
  n = 0;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable, false); n++;
  XtSetArg(args[n], XmNcolumns, POST_IT_COLUMNS); n++;
  XtSetArg(args[n], XmNrows, POST_IT_ROWS); n++;
  XtSetArg(args[n], XmNforeground, ss->sgx->black); n++; /* needed if color allocation fails completely */
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  post_it_text = XmCreateScrolledText(post_it_dialog, "post-it-text", args, n);
  XtManageChild(post_it_text);
  XtManageChild(post_it_dialog);

  map_over_children(post_it_dialog, set_main_color_of_widget);
  XtVaSetValues(post_it_text, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
  XtVaSetValues(MSG_BOX(post_it_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
  XtVaSetValues(MSG_BOX(post_it_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);

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
  xstr1 = XmStringCreateLocalized((char *)subject);
  xstr2 = XmStringCreateLocalized((char *)subject);
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
#if HAVE_FORTH
      fprintf(fd, "\"%s\" \"%s\" %s drop\n", subject, text, S_info_dialog);
#endif
      if (subject) XtFree(subject);
      if (text) XtFree(text);
    }
}


/* ---------------- view files dialog ---------------- */

static XEN mouse_enter_label_hook;
static XEN mouse_leave_label_hook;

static char *vf_row_get_label(void *ur)
{
  vf_row *r = (vf_row *)ur;
  return(((view_files_info *)(r->vdat))->full_names[r->pos]);
}

static int vf_row_get_pos(void *ur)
{
  vf_row *r = (vf_row *)ur;
  return(r->pos);
}

static void mouse_enter_or_leave_label(void *r, int type, XEN hook, const char *caller)
{
  if ((r) &&
      (XEN_HOOKED(hook)))
    {
      char *label = NULL;
      bool need_free = false;
      if (type == FILE_VIEWER)
	label = vf_row_get_label(r);
      else
	{
	  label = regrow_get_label(r);
	  if (label) need_free = true;
	}
      if (label)
	run_hook(hook,
		 XEN_LIST_3(C_TO_XEN_INT(type),
			    C_TO_XEN_INT((type == FILE_VIEWER) ? (vf_row_get_pos(r)) : (regrow_get_pos(r))),
			    C_TO_XEN_STRING(label)),
		 caller);
      if (need_free) XtFree(label);
    }
}

void mouse_leave_label(void *r, int type)
{
  mouse_enter_or_leave_label(r, type, mouse_leave_label_hook, S_mouse_leave_label_hook);
}

void mouse_enter_label(void *r, int type)
{
  mouse_enter_or_leave_label(r, type, mouse_enter_label_hook, S_mouse_enter_label_hook);
}

static void vf_mouse_enter_label(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  mouse_enter_label(context, FILE_VIEWER);
}

static void vf_mouse_leave_label(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  mouse_leave_label(context, FILE_VIEWER);
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

  s1 = XmStringCreateLocalized("");
  r = (vf_row *)CALLOC(1, sizeof(vf_row));
  r->vdat = (void *)vdat;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, (last_row) ? XmATTACH_WIDGET : XmATTACH_FORM); n++;
  if (last_row) {XtSetArg(args[n], XmNtopWidget, last_row); n++;}
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNheight, 18); n++; 
  r->rw = XtCreateWidget("rw", xmFormWidgetClass, vdat->file_list_holder, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNselectColor, ss->sgx->pushed_button_color); n++;
  XtSetArg(args[n], XmNlabelString, s1); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n1 = make_callback_list(play_callback, (XtPointer)r)); n++;
  if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
  XtSetArg(args[n], XmNmarginWidth, 8); n++;
  r->pl = make_togglebutton_widget("pl", r->rw, args, n);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
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

  XtAddEventHandler(r->nm, EnterWindowMask, false, vf_mouse_enter_label, (XtPointer)r);
  XtAddEventHandler(r->nm, LeaveWindowMask, false, vf_mouse_leave_label, (XtPointer)r);

  FREE(n1);
  FREE(n3);
  return(r);
}

void vf_unhighlight_row(widget_t nm, widget_t rw)
{
  XtVaSetValues(rw, XmNbackground, ss->sgx->highlight_color, NULL);
  XtVaSetValues(nm, XmNbackground, ss->sgx->highlight_color, NULL);
}

void vf_highlight_row(widget_t nm, widget_t rw)
{
  XtVaSetValues(rw, XmNbackground, ss->sgx->zoom_color, NULL);
  XtVaSetValues(nm, XmNbackground, ss->sgx->zoom_color, NULL);
}

typedef struct {
  vf_row *r;
  color_t old_color;
} vf_flash_data;

static void vf_unflash_row(XtPointer data, XtIntervalId *id)
{
  vf_flash_data *v = (vf_flash_data *)data;
  XtVaSetValues(v->r->rw, XmNbackground, v->old_color, NULL);
  XtVaSetValues(v->r->nm, XmNbackground, v->old_color, NULL);
  FREE(v);
}

void vf_flash_row(vf_row *r)
{
  vf_flash_data *v;
  v = (vf_flash_data *)CALLOC(1, sizeof(vf_flash_data));
  v->r = r;
  XtVaGetValues(r->rw, XmNbackground, &(v->old_color), NULL);
  XtVaSetValues(r->rw, XmNbackground, ss->sgx->light_blue, NULL);
  XtVaSetValues(r->nm, XmNbackground, ss->sgx->light_blue, NULL);
  XtAppAddTimeOut(MAIN_APP(ss),
		  500,
		  (XtTimerCallbackProc)vf_unflash_row,
		  (void *)v);
}

void vf_post_info(view_files_info *vdat, int pos)
{
  char *title;
  XmString s3;
  title = mus_format("%s:", vdat->names[pos]);
  s3 = XmStringCreateLocalized(title);
  XtVaSetValues(vdat->left_title,
		XmNlabelString, s3,
		NULL);
  XmStringFree(s3);
  FREE(title);
  post_sound_info(vdat->info1, vdat->info2, vdat->full_names[pos], false);
}

void vf_post_selected_files_list(view_files_info *vdat)
{
  int len;
  char *msg1 = NULL, *msg2 = NULL, *title;
  XmString s1, s2, s3;
  len = vdat->currently_selected_files;

  title = copy_string("selected files:");
  s3 = XmStringCreateLocalized(title);
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

  s1 = XmStringCreateLocalized(msg1);
  s2 = XmStringCreateLocalized(msg2);
  XtVaSetValues(vdat->info1, XmNlabelString, s1, NULL);
  XtVaSetValues(vdat->info2, XmNlabelString, s2, NULL);
  XmStringFree(s1);
  XmStringFree(s2);
  FREE(msg1);
  FREE(msg2);
}

void vf_unpost_info(view_files_info *vdat)
{
  XmString s1, s2, s3;
  char *title;

  title = copy_string("(no files selected)");
  s3 = XmStringCreateLocalized(title);
  XtVaSetValues(vdat->left_title,
		XmNlabelString, s3,
		NULL);
  XmStringFree(s3);
  FREE(title);

  s1 = XmStringCreateLocalized("|");
  s2 = XmStringCreateLocalized("|");
  XtVaSetValues(vdat->info1, XmNlabelString, s1, NULL);
  XtVaSetValues(vdat->info2, XmNlabelString, s2, NULL);
  XmStringFree(s1);
  XmStringFree(s2);
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

vf_row *view_files_make_row(view_files_info *vdat, widget_t last_row)
{
  return(make_vf_row(vdat, last_row, view_files_play_callback, view_files_select_callback));
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
  if (active_widget == MSG_BOX(vdat->dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(vdat->dialog);
}

static void view_files_new_viewer_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  if ((vdat) && 
      (vdat->dialog) &&
      (XtIsManaged(vdat->dialog)))
    {
      Position x = 0, y = 0;
      /* jog the current one over a bit -- otherwise the new one lands exactly on top of the old! */
      XtVaGetValues(vdat->dialog, XmNx, &x, XmNy, &y, NULL);
      XtVaSetValues(vdat->dialog, XmNx, x + 30, XmNy, y - 30, NULL);
    }
  vdat = new_view_files_dialog();
  start_view_files_dialog_1(vdat, true);

}

#if (!HAVE_FAM)
static void view_files_clear_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  view_files_clear_list(vdat);
  view_files_display_list(vdat);
}

static void view_files_update_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  /* run through view files list looking for any that have been deleted behind our back */
  view_files_update_list(vdat);
  view_files_display_list(vdat);
}
#endif

static void sort_vf(view_files_info *vdat, int sort_choice)
{
  vdat->sorter = sort_choice;
  vf_reflect_sort_choice_in_menu(vdat);
  view_files_display_list(vdat);
}

static void sort_view_files_a_to_z(Widget w, XtPointer context, XtPointer info) 
{
  sort_vf((view_files_info *)context, SORT_A_TO_Z);
}

static void sort_view_files_z_to_a(Widget w, XtPointer context, XtPointer info) 
{
  sort_vf((view_files_info *)context, SORT_Z_TO_A);
}

static void sort_view_files_new_to_old(Widget w, XtPointer context, XtPointer info) 
{
  sort_vf((view_files_info *)context, SORT_NEW_TO_OLD);
}

static void sort_view_files_old_to_new(Widget w, XtPointer context, XtPointer info) 
{
  sort_vf((view_files_info *)context, SORT_OLD_TO_NEW);
}

static void sort_view_files_big_to_small(Widget w, XtPointer context, XtPointer info) 
{
  sort_vf((view_files_info *)context, SORT_BIG_TO_SMALL);
}

static void sort_view_files_small_to_big(Widget w, XtPointer context, XtPointer info) 
{
  sort_vf((view_files_info *)context, SORT_SMALL_TO_BIG);
}

static void sort_view_files_xen(Widget w, XtPointer context, XtPointer info) 
{
  int index;
  XtVaGetValues(w, XmNuserData, &index, NULL); /* index is location in list of file-sorters */
  sort_vf((view_files_info *)context, index);
}

void vf_reflect_sort_choice_in_menu(view_files_info *vdat)
{
  int i;
  set_sensitive(vdat->a_to_z, vdat->sorter != SORT_A_TO_Z);
  set_sensitive(vdat->z_to_a, vdat->sorter != SORT_Z_TO_A);
  set_sensitive(vdat->new_to_old, vdat->sorter != SORT_NEW_TO_OLD);
  set_sensitive(vdat->old_to_new, vdat->sorter != SORT_OLD_TO_NEW);
  set_sensitive(vdat->small_to_big, vdat->sorter != SORT_SMALL_TO_BIG);
  set_sensitive(vdat->big_to_small, vdat->sorter != SORT_BIG_TO_SMALL);
  for (i = 0; i < vdat->sort_items_size; i++)
    if (XtIsManaged(vdat->sort_items[i]))
      set_sensitive(vdat->sort_items[i], vdat->sorter != (SORT_XEN + i));
}


void view_files_add_file_or_directory(view_files_info *vdat, const char *file_or_dir)
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
      view_files_display_list(vdat);
    }
}

static void view_files_drop_watcher(Widget w, const char *str, Position x, Position y, void *context)
{
  view_files_info *vdat = (view_files_info *)context;
  /* incoming str is a single filename (drop watcher code splits the possible list and calls us on each one) */
  view_files_add_file_or_directory(vdat, str);
  view_files_display_list(vdat);
}

static void view_files_drag_watcher(Widget w, const char *str, Position x, Position y, drag_style_t dtype, void *context)
{
  view_files_info *vdat = (view_files_info *)context;
  switch (dtype)
    {
    case DRAG_ENTER:
      XmChangeColor(vdat->file_list, ss->sgx->pushed_button_color);
      break;
    case DRAG_LEAVE:
      XmChangeColor(vdat->file_list, ss->sgx->basic_color);
      break;
    default:
      break;
    }
}

static void view_files_open_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_open_selected_files((view_files_info *)context);
}

static void view_files_remove_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_remove_selected_files((view_files_info *)context);
}

off_t vf_location(view_files_info *vdat)
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
	  pos = mark_id_to_sample(string_to_int(str, 0, "mark"));
	  XtFree(str);
	  if (pos < 0)
	    snd_error_without_format("no such mark");
	}
      else snd_error_without_format("no mark?");
      break;
    case VF_AT_SAMPLE:
      str = XmTextGetString(vdat->at_sample_text);
      if ((str) && (*str))
	{
	  pos = string_to_off_t(str, 0, "sample"); 
	  XtFree(str);
	  /* pos already checked for lower bound */
	}
      else snd_error_without_format("no sample number?");
      break;
    }
  return(pos);
}

static void vf_clear_sample(view_files_info *vdat);

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

static void vf_add_text_modify_callback(Widget w, XtPointer context, XtPointer info);

static void remove_all_pending_clear_callbacks(view_files_info *vdat)
{
  /* docs say this is a no-op if the indicated callback does not exist (i.e. not an error or segfault) */
  XtRemoveCallback(vdat->at_mark_text, XmNmodifyVerifyCallback, vf_mark_text_modify_callback, (XtPointer)vdat);
  XtRemoveCallback(vdat->at_mark_button, XmNvalueChangedCallback, vf_mark_button_modify_callback, (XtPointer)vdat);
  XtRemoveCallback(vdat->at_sample_text, XmNmodifyVerifyCallback, vf_sample_text_modify_callback, (XtPointer)vdat);
  XtRemoveCallback(vdat->at_sample_button, XmNvalueChangedCallback, vf_sample_button_modify_callback, (XtPointer)vdat);
  XtRemoveCallback(vdat->add_text, XmNmodifyVerifyCallback, vf_add_text_modify_callback, (XtPointer)vdat);
}

void vf_post_error(const char *error_msg, view_files_info *vdat)
{
  XmString msg;
  remove_all_pending_clear_callbacks(vdat);
  vdat->error_p = true;
  msg = XmStringCreateLocalized((char *)error_msg);
  XtVaSetValues(vdat->info1,
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);
  msg = XmStringCreateLocalized("");
  XtVaSetValues(vdat->info2,
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);
}

void redirect_vf_post_error(const char *error_msg, void *vdat)
{
  vf_post_error(error_msg, (view_files_info *)vdat);
}

void redirect_vf_post_location_error(const char *error_msg, void *data)
{
  view_files_info *vdat = (view_files_info *)data;
  vf_post_error(error_msg, vdat);
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

static void vf_add_text_modify_callback(Widget w, XtPointer context, XtPointer info)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)info;
  view_files_info *vdat = (view_files_info *)context;
  vf_clear_error(vdat);
  XtRemoveCallback(vdat->add_text, XmNmodifyVerifyCallback, vf_add_text_modify_callback, (XtPointer)vdat);
  cbs->doit = true;
}

void vf_post_add_error(const char *error_msg, view_files_info *vdat)
{
  vf_post_error(error_msg, vdat);
  XtAddCallback(vdat->add_text, XmNmodifyVerifyCallback, vf_add_text_modify_callback, (XtPointer)vdat);
  /* what about other clearing actions? */
}

static void view_files_mix_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_mix_selected_files(w, (view_files_info *)context);
}

static void view_files_insert_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_insert_selected_files(w, (view_files_info *)context);
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

void vf_set_speed(view_files_info *vdat, Float val)
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

static void speed_label_click_callback(Widget w, XtPointer context, XtPointer info) 
{
  char speed_number_buffer[6];
  view_files_info *vdat = (view_files_info *)context;
  ASSERT_WIDGET_TYPE(XmIsPushButton(w), w);
  switch (vdat->speed_style)
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    vdat->speed_style = SPEED_CONTROL_AS_RATIO;    break;
    case SPEED_CONTROL_AS_RATIO:    vdat->speed_style = SPEED_CONTROL_AS_SEMITONE; break;
    case SPEED_CONTROL_AS_SEMITONE: vdat->speed_style = SPEED_CONTROL_AS_FLOAT;    break;
    }
  speed_changed(vdat->speed,
		speed_number_buffer,
		vdat->speed_style,
		speed_control_tones(ss),
		6);
  set_label(vdat->speed_number, speed_number_buffer);
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

void vf_set_amp(view_files_info *vdat, Float val)
{
  char sfs[6];
  vdat->amp = val;
  mus_snprintf(sfs, 6, "%.2f", val);
  set_label(vdat->amp_number, sfs);
  XtVaSetValues(vdat->amp_scrollbar, 
		XmNvalue, amp_to_scroll(amp_control_min(ss), val, amp_control_max(ss)), 
		NULL);
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
  /* it might be nice to show the sound data in the background, but there are
   *   complications involving multichannel and multiselection cases, also
   *   how to get the "peak-func" and how to call g_channel_amp_envs.
   * Too many problems...
   *   but perhaps something like the region browser display would work:
   *   label saying file+chan and up/down arrows to see the rest + off button
   */
}

static void vf_amp_env_redraw(Widget w, view_files_info *vdat)
{
  vf_amp_env_resize(w, (void *)vdat, NULL);
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

void vf_set_amp_env(view_files_info *vdat, env *new_e)
{
  if (!vdat) return;
  if (vdat->amp_env) free_env(vdat->amp_env);
  vdat->amp_env = copy_env(new_e);
  if ((vdat->dialog) &&
      (widget_is_active(vdat->dialog)))
    vf_amp_env_redraw(vdat->env_drawer, vdat);
}

static void blue_textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info)
{
  XtVaSetValues(w, XmNbackground, ss->sgx->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}

static void blue_mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  XtVaSetValues(w, XmNbackground, ss->sgx->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}

static void white_mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  XtVaSetValues(w, XmNbackground, ss->sgx->text_focus_color, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, true, NULL);
}

static void view_files_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  env *e;
  vf_set_amp(vdat, 1.0);
  vf_set_speed(vdat, 1.0);
  vf_set_amp_env(vdat, e = default_env(1.0, 1.0)); /* vf_set_amp_env copies the envelope */
  free_env(e);
  sort_vf(vdat, view_files_sort(ss));
}

widget_t start_view_files_dialog_1(view_files_info *vdat, bool managed)
{
  if (!(vdat->dialog))
    {
      int i, n;
      Arg args[20];
      XmString xdismiss, xhelp, titlestr, new_viewer_str, s1, bstr;
      Widget mainform, viewform, leftform, reset_button;
      Widget left_title_sep, add_label, sep1, sep3, sep4, sep5, sep6, sep7, sort_cascade_menu;
#if (!HAVE_FAM)
      Widget sep2;
#endif
      Widget plw, rlw, sbar;
      XtCallbackList n1, n2, n3, n4;
      Widget amp_label, speed_label, env_frame;
      Widget bframe, bform;

      xdismiss = XmStringCreateLocalized(_("Dismiss"));
      xhelp = XmStringCreateLocalized(_("Help"));
      titlestr = XmStringCreateLocalized(_("Files"));
      new_viewer_str = XmStringCreateLocalized(_("New Viewer"));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      reset_button = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, vdat->dialog, args, n);
      XtAddCallback(reset_button, XmNactivateCallback, view_files_reset_callback, (XtPointer)vdat);

      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(titlestr);
      XmStringFree(new_viewer_str);

      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color,  NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->quit_button_color,   NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_button_color,  NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, MSG_BOX(vdat->dialog, XmDIALOG_SEPARATOR)); n++;
      XtSetArg(args[n], XmNsashIndent, 2); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNspacing, 24); n++;
      mainform = XtCreateManagedWidget("formd", xmPanedWindowWidgetClass, vdat->dialog, args, n);

      /* -------- left side controls -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      leftform = XtCreateManagedWidget("leftform", xmFormWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      vdat->left_title = XtCreateManagedWidget("(no files selected)", xmLabelWidgetClass, leftform, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->left_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmDOUBLE_LINE); n++;
      left_title_sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, leftform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, left_title_sep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      vdat->info1 = XtCreateManagedWidget("|", xmLabelWidgetClass, leftform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->info1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      vdat->info2 = XtCreateManagedWidget("|", xmLabelWidgetClass, leftform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      vdat->removeB = XtCreateManagedWidget(_("Unlist"), xmPushButtonGadgetClass, leftform, args, n);
      XtAddCallback(vdat->removeB, XmNactivateCallback, view_files_remove_selected_callback, (XtPointer)vdat);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->zoom_color); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->zoom_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep5); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 2); n++;
      bframe = XtCreateManagedWidget("bframe", xmFrameWidgetClass, leftform, args, n);      

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      bform = XtCreateManagedWidget("bform", xmFormWidgetClass, bframe, args, n);      

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      vdat->mixB = XtCreateManagedWidget(_("Mix"), xmPushButtonWidgetClass, bform, args, n);
      XtAddCallback(vdat->mixB, XmNactivateCallback, view_files_mix_selected_callback, (XtPointer)vdat);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->mixB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      vdat->insertB = XtCreateManagedWidget(_("Insert"), xmPushButtonWidgetClass, bform, args, n);
      XtAddCallback(vdat->insertB, XmNactivateCallback, view_files_insert_selected_callback, (XtPointer)vdat);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("at cursor"));
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
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("at end"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->at_cursor_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      vdat->at_end_button = make_togglebutton_widget("at-end-button", bform, args, n);
      XtAddCallback(vdat->at_end_button, XmNdisarmCallback, view_files_at_end_callback, (XtPointer)vdat);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("at beginning"));
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
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("at sample"));
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
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("at mark"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->at_sample_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      vdat->at_mark_button = make_togglebutton_widget("at-mark-button", bform, args, n);
      XtAddCallback(vdat->at_mark_button, XmNdisarmCallback, view_files_at_mark_callback, (XtPointer)vdat);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      s1 = XmStringCreateLocalized(_("amp:"));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      amp_label = make_pushbutton_widget("amp-label", leftform, args, n);
      XtAddCallback(amp_label, XmNactivateCallback, vf_amp_click_callback, (XtPointer)vdat);
      XmStringFree(s1);

      n = 0;
      s1 = XmStringCreateLocalized("1.0 ");
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      /* XtSetArg(args[n], XmNmarginRight, 3); n++; */
      vdat->amp_number = XtCreateManagedWidget("amp-number", xmLabelWidgetClass, leftform, args, n);
      XmStringFree(s1);

      n = 0;      
      XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;
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
      vdat->amp_scrollbar = XtCreateManagedWidget("amp-scroll", xmScrollBarWidgetClass, leftform, args, n);

      n = 0;
      /* SPEED */
      s1 = XmStringCreateLocalized(_("speed:"));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      speed_label = make_pushbutton_widget("speed-label", leftform, args, n);
      XtAddCallback(speed_label, XmNactivateCallback, vf_speed_click_callback, (XtPointer)vdat);
      XmStringFree(s1);

      n = 0;
      s1 = initial_speed_label(speed_control_style(ss));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      /* XtSetArg(args[n], XmNmarginRight, 3); n++; */
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNfillOnArm, false); n++;
      vdat->speed_number = make_pushbutton_widget("speed-number", leftform, args, n);
      XtAddCallback(vdat->speed_number, XmNactivateCallback, speed_label_click_callback, (XtPointer)vdat);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->position_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNheight, 100); n++;
      vdat->env_drawer = XtCreateManagedWidget("amp-env-window", xmDrawingAreaWidgetClass, env_frame, args, n);

      /* right side */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      viewform = XtCreateManagedWidget("viewform", xmFormWidgetClass, mainform, args, n);

      /* Add dir/file text entry at bottom */
      n = 0;
      s1 = XmStringCreateLocalized(_("add:"));
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, add_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      vdat->add_text = make_textfield_widget("add-text", viewform, args, n, ACTIVATABLE, add_completer_func(filename_completer, NULL));
      XtAddCallback(vdat->add_text, XmNactivateCallback, view_files_add_files, (XtPointer)vdat);
      
#if (!HAVE_FAM)
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, vdat->add_text); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNheight, 4); n++;
      sep2 = XtCreateManagedWidget("sep2", xmSeparatorWidgetClass, viewform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
#endif
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
#if (!HAVE_FAM)
      XtSetArg(args[n], XmNbottomWidget, vdat->updateB); n++;
#else
      XtSetArg(args[n], XmNbottomWidget, vdat->add_text); n++;
#endif
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNheight, 4); n++;
      sep3 = XtCreateManagedWidget("sep3", xmSeparatorWidgetClass, viewform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      rlw = XtCreateManagedWidget(_("files"), xmLabelWidgetClass, viewform, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, rlw); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmDOUBLE_LINE); n++;
      sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, viewform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 5); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      plw = XtCreateManagedWidget(_("play"), xmLabelWidgetClass, viewform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      vdat->smenu = XmCreatePulldownMenu(sbar, "sort-menu", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNsubMenuId, vdat->smenu); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      sort_cascade_menu = XtCreateManagedWidget(_("sort"), xmCascadeButtonWidgetClass, sbar, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      vdat->a_to_z =        XtCreateManagedWidget(_("a..z"),       xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->z_to_a =        XtCreateManagedWidget(_("z..a"),       xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->new_to_old =    XtCreateManagedWidget(_("new..old"),   xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->old_to_new =    XtCreateManagedWidget(_("old..new"),   xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->small_to_big =  XtCreateManagedWidget(_("small..big"), xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->big_to_small =  XtCreateManagedWidget(_("big..small"), xmPushButtonWidgetClass, vdat->smenu, args, n);

      vdat->sort_items_size = 4;
      vdat->sort_items = (Widget *)CALLOC(vdat->sort_items_size, sizeof(Widget));
      for (i = 0; i < vdat->sort_items_size; i++)
	vdat->sort_items[i] = XtCreateWidget("unused", xmPushButtonWidgetClass, vdat->smenu, args, n);

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
      add_drag_and_drop(vdat->file_list, view_files_drop_watcher, view_files_drag_watcher, (void *)vdat);

      if (managed) view_files_display_list(vdat);

      XtAddCallback(vdat->a_to_z,       XmNactivateCallback, sort_view_files_a_to_z,       (XtPointer)vdat);
      XtAddCallback(vdat->z_to_a,       XmNactivateCallback, sort_view_files_z_to_a,       (XtPointer)vdat);
      XtAddCallback(vdat->new_to_old,   XmNactivateCallback, sort_view_files_new_to_old,   (XtPointer)vdat);
      XtAddCallback(vdat->old_to_new,   XmNactivateCallback, sort_view_files_old_to_new,   (XtPointer)vdat);
      XtAddCallback(vdat->small_to_big, XmNactivateCallback, sort_view_files_small_to_big, (XtPointer)vdat);
      XtAddCallback(vdat->big_to_small, XmNactivateCallback, sort_view_files_big_to_small, (XtPointer)vdat);
      vf_reflect_sort_choice_in_menu(vdat);

      {
	int i;
	for (i = 0; i < vdat->sort_items_size; i++)
	  XtAddCallback(vdat->sort_items[i], XmNactivateCallback, sort_view_files_xen, (XtPointer)vdat);
      }
      /* XtAddCallback(sort_cascade_menu, XmNcascadingCallback, vf_display_sort_items, (XtPointer)vdat); -- segfaults */

      map_over_children(vdat->file_list, set_main_color_of_widget);

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
#if (!HAVE_FAM)
      vf_clear_button_set_sensitive(vdat, vdat->end > 0);
#endif
    }
  else
    {
      if (managed) 
	{
	  if (!XtIsManaged(vdat->dialog)) 
	    XtManageChild(vdat->dialog);
	  raise_dialog(vdat->dialog);
	  view_files_display_list(vdat);
	}
    }
  if (managed)
    {
      vf_amp_env_resize(vdat->env_drawer, (XtPointer)vdat, NULL);
      view_files_reflect_sort_items();
#if (!HAVE_FAM)
      vf_clear_button_set_sensitive(vdat, vdat->end >= 0);
#endif
    }
  return(vdat->dialog);
}

#if MUS_DEBUGGING && HAVE_SCHEME
#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_apply_edit_header_w, g_apply_edit_header)
#else
#define g_apply_edit_header_w g_apply_edit_header
#endif
#endif

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

#if MUS_DEBUGGING && HAVE_SCHEME
  XEN_DEFINE_PROCEDURE("apply-edit-header", g_apply_edit_header_w, 0, 0, 0, "internal testing function");
#endif
}
