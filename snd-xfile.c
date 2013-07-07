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

static void snd_sort(int sorter, sort_info **data, int len);


/* -------------------------------- sorters -------------------------------- */


static mus_long_t file_bytes(const char *filename)
{
#if HAVE_LSTAT
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return(statbuf.st_size);
  return(0);
#else
  int chan;
  mus_long_t bytes;
  chan = mus_file_open_read(filename);
  if (chan == -1) return(0);
  bytes = lseek(chan, 0L, SEEK_END);
  snd_close(chan, filename);
  return(bytes);
#endif
}

/* sort files list by name (aphabetical), or some number (date written, size), or by xen proc */

static int sort_a_to_z(const void *a, const void *b)
{
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  return(strcmp(d1->filename, d2->filename));
}


static int sort_z_to_a(const void *a, const void *b)
{
  return(-sort_a_to_z(a, b));
}


static int sort_small_to_big(const void *a, const void *b)
{
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  if (d1->samps > d2->samps) 
    return(1); 
  else 
    {
      if (d1->samps == d2->samps) 
	return(0); 
      else return(-1);
    }
}


static int sort_big_to_small(const void *a, const void *b)
{
  return(-sort_small_to_big(a, b));
}


static int sort_new_to_old(const void *a, const void *b)
{
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  if (d1->time < d2->time) 
    return(1); 
  else 
    {
      if (d1->time == d2->time) 
	return(0); 
      else return(-1);
    }
}


static int sort_old_to_new(const void *a, const void *b)
{
  return(-sort_new_to_old(a, b));
}


static XEN sorter_func;

static int sort_xen(const void *a, const void *b)
{
  /* sorter function gets two names, returns -1, 0, or 1 just like the other comparators */
  sort_info *d1 = *(sort_info **)a;
  sort_info *d2 = *(sort_info **)b;
  return(XEN_TO_C_INT(XEN_CALL_2(sorter_func, C_TO_XEN_STRING(d1->full_filename), C_TO_XEN_STRING(d2->full_filename), "sort func")));
}


static void snd_sort(int sorter, sort_info **data, int len)
{
  int i, sorter_pos;
  switch (sorter)
    {
    case SORT_A_TO_Z: 
      qsort((void *)data, len, sizeof(sort_info *), sort_a_to_z);
      break;

    case SORT_Z_TO_A: 
      qsort((void *)data, len, sizeof(sort_info *), sort_z_to_a);
      break;

    case SORT_NEW_TO_OLD:
      for (i = 0; i < len; i++) 
	data[i]->time = file_write_date(data[i]->full_filename);
      qsort((void *)data, len, sizeof(sort_info *), sort_new_to_old);
      break;

    case SORT_OLD_TO_NEW:
      for (i = 0; i < len; i++) 
	data[i]->time = file_write_date(data[i]->full_filename);
      qsort((void *)data, len, sizeof(sort_info *), sort_old_to_new);
      break;

    case SORT_SMALL_TO_BIG:
      for (i = 0; i < len; i++)
	data[i]->samps = file_bytes(data[i]->full_filename);
      qsort((void *)data, len, sizeof(sort_info *), sort_small_to_big);
      break;

    case SORT_BIG_TO_SMALL:
      for (i = 0; i < len; i++)
	data[i]->samps = file_bytes(data[i]->full_filename);
      qsort((void *)data, len, sizeof(sort_info *), sort_big_to_small);
      break;

    default:
    case SORT_XEN:
      /* sorter is SORT_XEN + index into file_sorters list */
      /*   that list is a vector of pairs (name proc) */
      sorter_pos = sorter - SORT_XEN;
      if ((sorter_pos >= 0) &&
	  (sorter_pos < ss->file_sorters_size))
	{
	  if (XEN_LIST_P(XEN_VECTOR_REF(ss->file_sorters, sorter_pos)))
	    {
	      sorter_func = XEN_CADR(XEN_VECTOR_REF(ss->file_sorters, sorter_pos));
	      qsort((void *)data, len, sizeof(sort_info *), sort_xen);
	      return;
	    }
	}
      snd_warning("no such file-sorter (%d)", sorter_pos);
      break;
    }
}






typedef enum {VF_AT_CURSOR, VF_AT_END, VF_AT_BEGINNING, VF_AT_MARK, VF_AT_SAMPLE} vf_location_t;

typedef struct {
  widget_t rw;
  widget_t nm;
#if WITH_AUDIO
  widget_t pl;
#endif
  int pos;
  void *vdat;
} vf_row;

typedef struct {
  vf_row **file_list_entries;
  int index, size;
  char **names;
  char **full_names;
  int end;
  int sorter;
  int *selected_files;
  int selected_files_size;
  int currently_selected_files;
  mus_float_t amp;
  vf_location_t location_choice;
  mus_float_t speed;
  graphics_context *env_ax;
  env_editor *spf;
  env *amp_env;
  bool error_p;
  int sort_items_size;
  speed_style_t speed_style;
  mus_long_t beg;

  int dirs_size;
#if HAVE_G_FILE_MONITOR_DIRECTORY
  GFileMonitor **dirs;
#else
  void *dirs;
#endif
  char **dir_names;
  bool need_update;

  widget_t dialog;
  widget_t file_list;
  widget_t file_list_holder;
  widget_t left_title;
  widget_t info1; 
  widget_t info2; 
  widget_t mixB; 
  widget_t insertB; 
  widget_t at_cursor_button; 
  widget_t at_end_button; 
  widget_t at_beginning_button; 
  widget_t at_mark_button; 
  widget_t at_sample_button; 
  widget_t at_sample_text; 
  widget_t at_mark_text;
  widget_t amp_number; 
  widget_t amp_scrollbar;
  widget_t speed_number; 
  widget_t speed_scrollbar;
  widget_t env_drawer;
  widget_t a_to_z; 
  widget_t z_to_a; 
  widget_t new_to_old; 
  widget_t old_to_new; 
  widget_t small_to_big; 
  widget_t big_to_small; 
  widget_t smenu; 
  widget_t current_play_button;
  widget_t amp_event; 
  widget_t speed_event;
  widget_t speed_label_event;
  widget_t add_text;
  widget_t* sort_items;

  GC env_gc;
} view_files_info;


static void vf_unhighlight_row(widget_t nm, widget_t rw);
static void vf_highlight_row(widget_t nm, widget_t rw);
static void vf_post_info(view_files_info *vdat, int pos);
static void vf_unpost_info(view_files_info *vdat);
static mus_long_t vf_location(view_files_info *vdat);
static void vf_post_error(const char *error_msg, view_files_info *data);
static void redirect_vf_post_error(const char *error_msg, void *data);
static void redirect_vf_post_location_error(const char *error_msg, void *data);
static void vf_post_add_error(const char *error_msg, view_files_info *data);
static widget_t make_view_files_dialog_1(view_files_info *vdat, bool managed);
static void vf_post_selected_files_list(view_files_info *vdat);
static void view_files_add_file_or_directory(view_files_info *vdat, const char *file_or_dir);
static void vf_reflect_sort_choice_in_menu(view_files_info *vdat);
static vf_row *view_files_make_row(view_files_info *vdat, widget_t last_row);
static void vf_flash_row(vf_row *r);
static void vf_set_amp(view_files_info *vdat, mus_float_t val);
static void vf_set_speed(view_files_info *vdat, mus_float_t val);
static void vf_set_amp_env(view_files_info *vdat, env *new_e);
static void vf_clear_error(view_files_info *vdat);
static void vf_mix_insert_buttons_set_sensitive(view_files_info *vdat, bool sensitive);
static int vf_mix(view_files_info *vdat);
static bool vf_insert(view_files_info *vdat);

static void view_files_display_list(view_files_info *vdat);
static void view_files_mix_selected_files(widget_t w, view_files_info *vdat);
static void view_files_insert_selected_files(widget_t w, view_files_info *vdat);
static void view_files_open_selected_files(view_files_info *vdat);
static void view_files_select(vf_row *r, bool add_to_selected);
static bool view_files_play(view_files_info *vdat, int pos, bool play);
static view_files_info *new_view_files_dialog(void);
static void add_directory_to_view_files_list(view_files_info *vdat, const char *dirname);
static void add_file_to_view_files_list(view_files_info *vdat, const char *filename, const char *fullname);
static void view_files_reflect_sort_items(void);
static void view_files_unmonitor_directories(view_files_info *vdat);
static void view_files_monitor_directory(view_files_info *vdat, const char *dirname);


static void dialog_set_title(widget_t dialog, const char *titlestr)
{
  XmString title;
  title = XmStringCreateLocalized((char *)titlestr);
  XtVaSetValues(dialog, XmNdialogTitle, title, NULL);
  XmStringFree(title);
}




void cleanup_file_monitor(void) {}
bool initialize_file_monitor(void) {return(false);}
void *unmonitor_file(void *watcher) {return(NULL);}
void monitor_sound(snd_info *sp) {}

/* -------------------------------------------------------------------------------- */



#define FSB_BOX(Dialog, Child) XmFileSelectionBoxGetChild(Dialog, Child)
#define MSG_BOX(Dialog, Child) XmMessageBoxGetChild(Dialog, Child)


/* ---------------- open/mix/insert/save-as dialogs ---------------- */

static void color_file_selection_box(Widget w)
{
  /* overwrite most Motif-default colors */
  Widget wtmp;
  
  map_over_children(w, set_main_color_of_widget);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_DIR_LIST), 
		XmNbackground, ss->white, 
		XmNforeground, ss->black, 
		NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_LIST), 
		XmNbackground, ss->white, 
		XmNforeground, ss->black, 
		NULL);
  
  XtVaSetValues(FSB_BOX(w, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->selection_color, NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->selection_color, NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->selection_color, NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->highlight_color, NULL);
  XtVaSetValues(FSB_BOX(w, XmDIALOG_OK_BUTTON),     XmNbackground, ss->highlight_color, NULL);
  
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
  name_field = FSB_BOX(dialog, XmDIALOG_TEXT);
  filename = XmTextGetString(name_field);
  XtVaGetValues(dialog, XmNdirMask, &dirmask, NULL);
  XmFileSelectionDoSearch(dialog, dirmask);
  XmStringFree(dirmask);
  XmTextSetString(name_field, filename);
  if (filename) 
    {
      XmTextSetCursorPosition(name_field, mus_strlen(filename));
      XtFree(filename);
    }
}


static void force_directory_reread_and_let_filename_change(Widget dialog)
{
  XmString dirmask;
  XtVaGetValues(dialog, XmNdirMask, &dirmask, NULL);
  XmFileSelectionDoSearch(dialog, dirmask);
  XmStringFree(dirmask);
}




/* -------------------------------- file list positioning -------------------------------- */

typedef struct {
  char *directory_name;
  position_t list_top;
} dirpos_info;

typedef struct {
  dirpos_info **dirs;
  int size, top;
} dirpos_list;

void dirpos_update(dirpos_list *dl, const char *dir, position_t pos);
position_t dirpos_list_top(dirpos_list *dl, const char *dirname);
dirpos_list *make_dirpos_list(void);


static dirpos_info *make_dirpos_info(const char *dir, position_t pos)
{
  dirpos_info *dp;
  dp = (dirpos_info *)calloc(1, sizeof(dirpos_info));
  dp->directory_name = mus_strdup(dir);
  dp->list_top = pos;
  return(dp);
}


dirpos_list *make_dirpos_list(void)
{
  dirpos_list *dl;
  dl = (dirpos_list *)calloc(1, sizeof(dirpos_list));
  dl->size = 8;
  dl->top = 0;
  dl->dirs = (dirpos_info **)calloc(dl->size, sizeof(dirpos_info *));
  return(dl);
}


void dirpos_update(dirpos_list *dl, const char *dir, position_t pos)
{
  int i;
  if (!dl) return;
  for (i = 0; i < dl->top; i++)
    {
      if ((dl->dirs[i]) && 
	  (strcmp(dir, dl->dirs[i]->directory_name) == 0))
	{
	  dirpos_info *dp;
	  dp = dl->dirs[i];
	  dp->list_top = pos;
	  return;
	}
    }
  if (dl->top >= dl->size)
    {
      int old_size;
      old_size = dl->size;
      dl->size += 8;
      dl->dirs = (dirpos_info **)realloc(dl->dirs, dl->size * sizeof(dirpos_info *));
      for (i = old_size; i < dl->size; i++) dl->dirs[i] = NULL;
    }
  dl->dirs[dl->top++] = make_dirpos_info(dir, pos);
}


position_t dirpos_list_top(dirpos_list *dl, const char *dirname)
{
  int i;
  if (dl)
    for (i = 0; i < dl->top; i++)
      if ((dl->dirs[i]) && 
	  (strcmp(dirname, dl->dirs[i]->directory_name) == 0))
	return(dl->dirs[i]->list_top);
  return(POSITION_UNKNOWN);
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
  void *directory_watcher;
  int filter_choice, sorter_choice;
  dirpos_list *dir_list;
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
  snd_info *sp;
  filename = get_label(w);
  XmTextFieldSetString(FSB_BOX(fd->dialog, XmDIALOG_TEXT), filename);

  ss->open_requestor = FROM_OPEN_DIALOG_POPUP;
  ss->open_requestor_data = NULL;
  sp = snd_open_file(filename, FILE_READ_WRITE);
  if (sp) select_channel(sp, 0);

  XtUnmanageChild(fd->dialog);
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

      char *current_filename;
      int i, filenames_to_display = 0;

      if (fd->file_text_items == NULL)
	{
	  int n = 0;
	  Arg args[12];
	  fd->file_text_items = (Widget *)calloc(FILENAME_LIST_SIZE, sizeof(Widget));
	  XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
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
	    (!(mus_strcmp(fd->file_text_names[i], current_filename))))
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

      /* why was this commented out? */
      if (filenames_to_display == 0)
	set_label(fd->file_text_popup_label, "no " FILE_TEXT_POPUP_LABEL);
      else set_label(fd->file_text_popup_label, FILE_TEXT_POPUP_LABEL);

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
	  fd->file_filter_items = (Widget *)calloc(FILENAME_LIST_SIZE, sizeof(Widget));
	  XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
	  for (i = 0; i < FILENAME_LIST_SIZE; i++)
	    {
	      fd->file_filter_items[i] = XtCreateWidget("", xmPushButtonWidgetClass, fd->file_filter_popup, args, n);
	      XtAddCallback(fd->file_filter_items[i], XmNactivateCallback, file_filter_item_activate_callback, (void *)fd);
	    }
	}

      current_filtername = XmTextFieldGetString(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT)); 

      for (i = 0; i < FILENAME_LIST_SIZE; i++)
	if ((fd->file_filter_names[i]) &&
	    (!(mus_strcmp(fd->file_filter_names[i], current_filtername))))
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
  free(filter);
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
	  fd->file_dir_items = (Widget *)calloc(FILENAME_LIST_SIZE, sizeof(Widget));
	  XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
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
	      dirs = (char **)calloc(dirs_to_display, sizeof(char *));
	      dirs[0] = mus_strdup("/");
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
	      free(dirs);
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
  pointer_or_int_t data;
  int choice;
  XtVaGetValues(w, XmNuserData, &data, NULL);
  choice = (int)data;
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
  const char *item_label;
  Widget w;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;

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

	  fd->file_list_items = (Widget *)calloc(SORT_XEN, sizeof(Widget));
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
	    fd->file_list_items = (Widget *)realloc(fd->file_list_items, items_len * sizeof(Widget));
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
				XmNbackground, ss->lighter_blue,
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
		  XtVaSetValues(fd->file_list_items[k], XmNbackground, ss->light_blue, 
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
	      XtVaSetValues(fd->file_list_items[k], XmNbackground, ss->light_blue, 
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
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
  XtSetArg(args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC); n++;

  /* file text */
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_TEXT), XmNpopupHandlerCallback, file_text_popup_callback, (void *)fd);
  fd->file_text_popup = XmCreatePopupMenu(FSB_BOX(fd->dialog, XmDIALOG_TEXT), (char *)"file-text-popup", args, n);
  fd->file_text_names = make_filename_list();
  fd->file_text_popup_label = XtCreateManagedWidget(FILE_TEXT_POPUP_LABEL, xmLabelWidgetClass, fd->file_text_popup, args, n);
  XtCreateManagedWidget("sep", xmSeparatorWidgetClass, fd->file_text_popup, args, n);

  /* filter text */
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT), XmNpopupHandlerCallback, file_filter_popup_callback, (void *)fd);
  fd->file_filter_popup = XmCreatePopupMenu(FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT), (char *)"file-filter-popup", args, n);
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
  fd->file_dir_popup = XmCreatePopupMenu(FSB_BOX(fd->dialog, XmDIALOG_DIR_LIST), (char *)"file-dir-popup", args, n);
  fd->file_dir_popup_label = XtCreateManagedWidget(FILE_DIR_POPUP_LABEL, xmLabelWidgetClass, fd->file_dir_popup, args, n);
  XtCreateManagedWidget("sep", xmSeparatorWidgetClass, fd->file_dir_popup, args, n);

  /* file list */
  XtAddCallback(FSB_BOX(fd->dialog, XmDIALOG_LIST), XmNpopupHandlerCallback, file_list_popup_callback, (void *)fd);
  fd->file_list_popup = XmCreatePopupMenu(FSB_BOX(fd->dialog, XmDIALOG_LIST), (char *)"file-list-popup", args, n);
  fd->file_list_popup_label = XtCreateManagedWidget(FILE_LIST_POPUP_LABEL, xmLabelWidgetClass, fd->file_list_popup, args, n);
  XtCreateManagedWidget("sep", xmSeparatorWidgetClass, fd->file_list_popup, args, n);
}



/* ---------------- just-sounds (file-filters) ---------------- */

static void file_change_directory_callback(Widget w, XtPointer context, XtPointer info) 
{
  /* click in directory list */
  file_pattern_info *fp = (file_pattern_info *)context;
  char *leaving_dir;

  {
    /* save current directory list position */
    position_t position = 0;
    XmString *strs;
    char *filename = NULL;
    XtVaGetValues(w, 
		  XmNtopItemPosition, &position,
		  XmNselectedItems, &strs, 
		  NULL);
    if (position > 1) /* 1 = .. */
      {
	filename = (char *)XmStringUnparse(strs[0], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
	dirpos_update(fp->dir_list, filename, position);
	XtFree(filename);
      }
  }

  leaving_dir = mus_strdup(fp->last_dir);
  if ((leaving_dir) &&
      (leaving_dir[strlen(leaving_dir) - 1] == '/'))
    leaving_dir[strlen(leaving_dir) - 1] = 0;
  
  fp->reread_directory = true;
  force_directory_reread_and_let_filename_change(fp->dialog);
  fp->reread_directory = false;

  if (leaving_dir)
    {
      position_t pos;
      pos = dirpos_list_top(fp->dir_list, leaving_dir);
      if (pos != POSITION_UNKNOWN)
	XmListSetPos(w, pos);
      free(leaving_dir);
    }
}


static void sort_files_and_redisplay(file_pattern_info *fp)
{
  /* if just sorting, no need to read the directory */
  dir_info *cur_dir;

  cur_dir = fp->current_files;
  if (cur_dir->len > 0)
    {
      XmString *names;
      int i, new_selected_position = -1;
      char *selected_filename = NULL;

      {
	XmString *strs;
	int selections = 0;
	XtVaGetValues(XmFileSelectionBoxGetChild(fp->dialog, XmDIALOG_LIST), 
		      XmNselectedItems, &strs, 
		      XmNselectedItemCount, &selections,
		      NULL);
	if ((selections > 0) && (strs[0]))
	  selected_filename = (char *)XmStringUnparse(strs[0], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
      }

      snd_sort(fp->sorter_choice, cur_dir->files, cur_dir->len);

      /* here we could use colored text to mark sound files, perhaps different colors for
       *   different chans (as in install-searcher-with-colors), but I would rather have
       *   used different background colors (less intrusive I think).  As far as I can tell,
       *   this is impossible given the current XmList widget -- each item is an internal
       *   "Element", not a label widget or whatever, and the selection color, for example,
       *   is done by hand.
       */

      names = (XmString *)calloc(cur_dir->len, sizeof(XmString));
      for (i = 0; i < cur_dir->len; i++)
	{
	  names[i] = XmStringCreateLocalized(cur_dir->files[i]->full_filename);
	  if ((new_selected_position == -1) &&
	      (mus_strcmp(selected_filename, cur_dir->files[i]->full_filename)))
	    new_selected_position = i;
	}

      XtVaSetValues(fp->dialog, 
		    XmNfileListItems, names, 
		    XmNfileListItemCount, cur_dir->len, 
		    XmNlistUpdated, true, 
		    NULL);

      if (new_selected_position >= 0)
	ensure_list_row_visible(XmFileSelectionBoxGetChild(fp->dialog, XmDIALOG_LIST), new_selected_position);

      for (i = 0; i < cur_dir->len; i++) 
	if (names[i]) 
	  XmStringFree(names[i]);
      free(names);
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

  /* set file_pattern_info->selected_filename list_slider_position from history */
  {
    position_t list_pos;
    Widget file_list;
    file_list = XmFileSelectionBoxGetChild(dialog, XmDIALOG_LIST);
    list_pos = dirpos_list_top(fp->dir_list, our_dir);

    /* post the sorted list in the dialog -- alphabetize by default */
    sort_files_and_redisplay(fp);

    if (list_pos != POSITION_UNKNOWN)
      XmListSetPos(file_list, list_pos);
  }

  if ((fp->last_dir == NULL) ||
      (strcmp(our_dir, fp->last_dir) != 0))
    {
      if (fp->directory_watcher)
	unmonitor_file(fp->directory_watcher);
      fp->directory_watcher = NULL;
      if (fp->last_dir) free(fp->last_dir);
      fp->last_dir = mus_strdup(our_dir);
      fp->reread_directory = false;
    }

  if (our_dir) XtFree(our_dir);
}


static void just_sounds_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  file_pattern_info *fp = (file_pattern_info *)context;
  if (cb->set)
    fp->filter_choice = JUST_SOUNDS_FILTER;
  else fp->filter_choice = NO_FILE_FILTER;
  fp->in_just_sounds_update = true;
  force_directory_reread(fp->dialog);
  fp->in_just_sounds_update = false;
}


/* -------- play selected file handlers -------- */

typedef struct {
  Widget dialog, play_button;
  snd_info *player;
} dialog_play_info;


static void file_dialog_stop_playing(dialog_play_info *dp)
{
#if WITH_AUDIO
  if ((dp->player) && 
      (dp->player->playing)) 
    {
      stop_playing_sound(dp->player, PLAY_BUTTON_UNSET);
      dp->player = NULL;
    }
#endif
}


void clear_deleted_snd_info(void *udp)
{
  dialog_play_info *dp = (dialog_play_info *)udp;
#if WITH_AUDIO
  dp->player = NULL;
#endif
}


#if WITH_AUDIO
static void play_selected_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  dialog_play_info *dp = (dialog_play_info *)context;
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
	      dp->player->delete_me = (void *)dp;
	      if (dp->player)
		play_sound(dp->player, 0, NO_END_SPECIFIED);
	    }
	  XtFree(filename);
	}
    }
  else file_dialog_stop_playing(dp);
}
#endif


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
  fp->just_sounds_button = XtCreateManagedWidget("sound files only", xmToggleButtonWidgetClass, rc, args, n);
  XtAddCallback(fp->just_sounds_button, XmNvalueChangedCallback, just_sounds_callback, (XtPointer)fp);

#if WITH_AUDIO
  n = 0;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, 20); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, rc, args, n);

  n = 0;
  /* XmNmarginLeft here refers to the space between the button and its label! */
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  dp->play_button = XtCreateWidget("play selected sound", xmToggleButtonWidgetClass, rc, args, n);

  XtAddCallback(dp->play_button, XmNvalueChangedCallback, play_selected_callback, (XtPointer)dp);
#endif
}



/* -------- File Open/View/Mix Dialogs -------- */

typedef struct file_dialog_info {
  read_only_t file_dialog_read_only;
  Widget dialog;
  Widget info_frame, info1, info2;     /* labels giving info on selected file, or an error message */
  file_pattern_info *fp;
  dialog_play_info *dp;
  void *unsound_directory_watcher; /* started if file doesn't exist, not a sound file, bogus header, etc (clears error msg if problem changed) */
  void *info_filename_watcher;     /* watch for change in selected file and repost info */
  char *unsound_dirname, *unsound_filename;
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
  buf = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
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
  free(buf);
}


static void post_file_info(file_dialog_info *fd, const char *filename)
{
#if WITH_AUDIO
  XtManageChild(fd->dp->play_button);
#endif
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
#if WITH_AUDIO
  if (XtIsManaged(fd->dp->play_button)) 
    XtUnmanageChild(fd->dp->play_button);
#endif
  if (XtIsManaged(fd->info_frame))
    XtUnmanageChild(fd->info_frame);

  if (fd->info_filename_watcher)
    {
      fd->info_filename_watcher = unmonitor_file(fd->info_filename_watcher);
      if (fd->info_filename) {free(fd->info_filename); fd->info_filename = NULL;}
    }
}


static bool empty_file_p(const char *filename)
{
#if HAVE_LSTAT
  struct stat statbuf;
  if (lstat(filename, &statbuf) >= 0) 
    return(statbuf.st_size == (mus_long_t)0);
#endif
  return(false);
}


static int local_error = MUS_NO_ERROR;
static char *local_error_msg = NULL;
static mus_error_handler_t *old_error_handler;

static void local_error2snd(int type, char *msg) 
{
  local_error = type;
  if (local_error_msg) free(local_error_msg);
  if (msg)
    local_error_msg = mus_strdup(msg);
  else local_error_msg = NULL;
}


static bool plausible_sound_file_p(const char *name)
{
  int err = MUS_NO_ERROR;
  if (empty_file_p(name)) return(false);
  old_error_handler = mus_error_set_handler(local_error2snd);
  err = mus_header_read(name);
  mus_error_set_handler(old_error_handler);
  return((err == MUS_NO_ERROR) &&
	 (mus_header_type() != MUS_RAW));
}


static void file_dialog_select_callback(Widget w, XtPointer context, XtPointer info)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmString *strs;
  char *filename = NULL;
  XtVaGetValues(w, XmNselectedItems, &strs, NULL);
  filename = (char *)XmStringUnparse(strs[0], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if (filename)
    {
      if (plausible_sound_file_p(filename)) /* forces header read to avoid later unwanted error possibility */
	post_file_info(fd, filename);
      XtFree(filename);      
    }
  else unpost_file_info(fd);

  {
    /* save current list position */
    position_t position = 0;
    XtVaGetValues(w, XmNtopItemPosition, &position, NULL);
    dirpos_update(fd->fp->dir_list, fd->fp->current_files->dir_name, position);
  }
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
	  pos = i + 1;
	  if (comp == 0)
	    break;
	  if (comp < 0) /* files[i] less than filename */
	    l = i + 1;
	  else u = i - 1;
	}
      if (pos > 0)
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
      int len;
      len = strlen(filename);
      if ((!mus_file_probe(filename)) && 
	  (filename[len - 1] == '/'))
	{
	  int i;
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
}


static void multifile_completer(widget_t w, void *data)
{
  watch_filename_change(w, (XtPointer)data, NULL);
}


#define FILE_DIALOG_WIDTH 500
#define FILE_DIALOG_HEIGHT 500

static file_dialog_info *make_file_dialog(read_only_t read_only, char *title, char *select_title, 
					  XtCallbackProc file_ok_proc, XtCallbackProc file_help_proc)
{
  /* file selection dialog box with added "Just Sound Files" and "Play selected" toggle buttons and info area,
   *   popups, and so on.  This applies to the Open, Mix, and Insert dialogs.  The save-as
   *   dialogs are handled by make_save_as_dialog below
   */
  Widget w;
  file_dialog_info *fd;
  Arg args[32];
  int n;
  XmString s1, s2, ok_label, filter_list_label, cancel_label;
  Widget wtmp = NULL, rc1, rc2;

  fd = (file_dialog_info *)calloc(1, sizeof(file_dialog_info));
  fd->fp = (file_pattern_info *)calloc(1, sizeof(file_pattern_info));
  fd->fp->in_just_sounds_update = false;
  if (just_sounds(ss))
    fd->fp->filter_choice = JUST_SOUNDS_FILTER;
  else fd->fp->filter_choice = NO_FILE_FILTER;

  fd->dp = (dialog_play_info *)calloc(1, sizeof(dialog_play_info));
  fd->file_dialog_read_only = read_only;
  fd->fpop = (file_popup_info *)calloc(1, sizeof(file_popup_info));
  fd->fpop->fp = fd->fp;

  fd->fp->dir_list = make_dirpos_list();

  w = MAIN_SHELL(ss);

  s1 = XmStringCreateLocalized(select_title);
  s2 = XmStringCreateLocalized(title);
  ok_label = XmStringCreateLocalized(title);
  filter_list_label = XmStringCreateLocalized((char *)"files listed:");
  cancel_label = XmStringCreateLocalized((char *)I_GO_AWAY);

  n = 0;
  if (open_file_dialog_directory(ss))
    {
      XmString dirstr;
      dirstr = XmStringCreateLocalized(open_file_dialog_directory(ss));
      XtSetArg(args[n], XmNdirectory, dirstr); n++;
    }
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  XtSetArg(args[n], XmNokLabelString, ok_label); n++;
  XtSetArg(args[n], XmNselectionLabelString, s1); n++;                    /* "open", "mix", "insert", "open read-only:" */
  XtSetArg(args[n], XmNdialogTitle, s2); n++;
  XtSetArg(args[n], XmNfilterLabelString, filter_list_label); n++;        /* default label 'Filter' is confusing in this context */
  XtSetArg(args[n], XmNfileFilterStyle, XmFILTER_HIDDEN_FILES); n++;      /* the dot files mostly just get in the way */
  XtSetArg(args[n], XmNcancelLabelString, cancel_label); n++;
  XtSetArg(args[n], XmNuserData, (XtPointer)(fd->fp)); n++;
  XtSetArg(args[n], XmNfileSearchProc, snd_directory_reader); n++;        /* over-ride Motif's directory reader altogether */  
  XtSetArg(args[n], XmNwidth, FILE_DIALOG_WIDTH); n++;

  XtSetArg(args[n], XmNheight, FILE_DIALOG_HEIGHT); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;

  fd->dialog = XmCreateFileSelectionDialog(w, title, args, n);
  fd->fp->dialog = fd->dialog;
  fd->dp->dialog = fd->dialog;
  fd->fpop->dialog = fd->dialog;

  XtUnmanageChild(FSB_BOX(fd->dialog, XmDIALOG_DIR_LIST_LABEL)); /* these are obvious */
  XtUnmanageChild(FSB_BOX(fd->dialog, XmDIALOG_LIST_LABEL));
  XtUnmanageChild(FSB_BOX(fd->dialog, XmDIALOG_APPLY_BUTTON));   /* "Filter" button is useless */

  XtVaSetValues(FSB_BOX(fd->dialog, XmDIALOG_FILTER_LABEL), XmNbackground, ss->basic_color, NULL);
  XtVaSetValues(FSB_BOX(fd->dialog, XmDIALOG_SELECTION_LABEL), XmNbackground, ss->basic_color, NULL);

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
				XmNbackground, ss->highlight_color,
				NULL);
  fd->info1 = XtVaCreateManagedWidget("", xmLabelWidgetClass, rc2, XmNbackground, ss->highlight_color, NULL);
  fd->info2 = XtVaCreateManagedWidget("", xmLabelWidgetClass, rc2, XmNbackground, ss->highlight_color, NULL);


  /* -------- Snd-like color schemes */
  color_file_selection_box(fd->dialog);
  XtVaSetValues(fd->fp->just_sounds_button, XmNselectColor, ss->selection_color, NULL);
#if WITH_AUDIO
  XtVaSetValues(fd->dp->play_button, XmNselectColor, ss->selection_color, NULL);
#endif

  /* -------- completions */

  wtmp = FSB_BOX(fd->dialog, XmDIALOG_TEXT);
  add_completer_to_builtin_textfield(wtmp, add_completer_func_with_multicompleter(sound_filename_completer, (void *)fd, multifile_completer));

  XtAddCallback(wtmp, XmNfocusCallback, focus_filename_text_callback, (XtPointer)fd);
  XtAddCallback(wtmp, XmNlosingFocusCallback, unfocus_filename_text_callback, (XtPointer)fd);

  wtmp = FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT);
  add_completer_to_builtin_textfield(wtmp, add_completer_func(filename_completer, NULL));

  XtAddCallback(wtmp, XmNvalueChangedCallback, unpost_if_filter_changed, (XtPointer)fd);

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
    wm_delete_window = XmInternAtom(MAIN_DISPLAY(ss), (char *)"WM_DELETE_WINDOW", false);
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

  if (fd->unsound_directory_watcher)
    {
      fd->unsound_directory_watcher = unmonitor_file(fd->unsound_directory_watcher);
      if (fd->unsound_dirname) {free(fd->unsound_dirname); fd->unsound_dirname = NULL;}
      if (fd->unsound_filename) {free(fd->unsound_filename); fd->unsound_filename = NULL;}
    }
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


static void file_open_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  file_dialog_info *fd = (file_dialog_info *)context;
  XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)info;
  char *filename = NULL;
  if (XmGetFocusWidget(fd->dialog) == FSB_BOX(fd->dialog, XmDIALOG_FILTER_TEXT)) return;

  filename = (char *)XmStringUnparse(cbs->value, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((!filename) || (!(*filename)))
    {
      file_open_error("no filename given", fd);
      clear_error_if_open_changes(fd->dialog, fd);
    }
  else
    {
      file_dialog_stop_playing(fd->dp);
      if (!(directory_p(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
	{
	  snd_info *sp;
	  redirect_snd_error_to(redirect_file_open_error, (void *)fd);
	  ss->requestor_dialog = w;
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
		}
	    }
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format("%s is a directory", filename);
	  file_open_error(str, fd);
	  clear_error_if_open_changes(fd->dialog, fd);
	  free(str);
	}
    }
}


static file_dialog_info *odat = NULL;

widget_t make_open_file_dialog(read_only_t read_only, bool managed)
{
  char *title, *select_title;
  if (read_only == FILE_READ_ONLY)  
    {
      title = (char *)"View";
      select_title = (char *)"open read-only:";
    }
  else
    {
      title = (char *)"Open";
      select_title = (char *)"open:";
    }
  if (!odat)
    {
      XmString cancel_label;
      odat = make_file_dialog(read_only, title, select_title, file_open_ok_callback, open_file_help_callback);
      set_dialog_widget(FILE_OPEN_DIALOG, odat->dialog);

      /* now preload last n files opened before this point */
      preload_filenames(odat->fpop->file_text_names);

      cancel_label = XmStringCreateLocalized((char *)I_GO_AWAY);
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
			XmNokLabelString, s2, /* "ok" button label can be either "View" or "Open" */
			NULL);
	  XmStringFree(s1);
	  XmStringFree(s2);
	}
      odat->file_dialog_read_only = read_only;
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
  
  filename = (char *)XmStringUnparse(cbs->value, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((!filename) || (!(*filename)))
    {
      file_open_error("no filename given", fd);
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
	  ss->requestor_dialog = w;
	  ss->open_requestor = FROM_MIX_DIALOG;
	  ss->open_requestor_data = NULL;
	  id_or_error = mix_complete_file_at_cursor(sp, filename);
	  /* "id_or_error" here is either one of the mix id's or an error indication such as MIX_FILE_NO_MIX */
	  /*    the possible error conditions have been checked already, or go through snd_error */
	  redirect_snd_error_to(NULL, NULL);
	  if (id_or_error < 0) /* actually -1 .. -3 */
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  clear_error_if_open_changes(fd->dialog, fd);
		}
	    }
	  else 
	    {
	      status_report(sp, "%s mixed in at cursor", filename);
	      remember_filename(filename, fd->fpop->file_text_names);
	    }
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format("%s is a directory", filename);
	  file_open_error(str, fd);
	  clear_error_if_open_changes(fd->dialog, fd);
	  free(str);
	}
    }
}
  

static void mix_file_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  mix_file_dialog_help();
}


static file_dialog_info *mdat = NULL;

static XEN mix_open_file_watcher(XEN hook_or_reason)
{
  if ((mdat->dialog) &&
      (XtIsManaged(mdat->dialog)))
    set_sensitive(FSB_BOX(mdat->dialog, XmDIALOG_OK_BUTTON), (bool)any_selected_sound());
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_1(mix_open_file_watcher_w, mix_open_file_watcher)
#else
  #define mix_open_file_watcher_w mix_open_file_watcher
#endif



widget_t make_mix_file_dialog(bool managed)
{
  /* called from the menu */
  if (!mdat)
    {
      mdat = make_file_dialog(FILE_READ_ONLY, (char *)"Mix Sound", (char *)"mix in:", file_mix_ok_callback, mix_file_help_callback);
      set_dialog_widget(FILE_MIX_DIALOG, mdat->dialog);
      XEN_ADD_HOOK(ss->snd_open_file_hook, mix_open_file_watcher_w, "mix-dialog-open-file-watcher", "mix dialog's open-file-hook handler");
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
  
  filename = (char *)XmStringUnparse(cbs->value, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
  if ((!filename) || (!(*filename)))
    {
      file_open_error("no filename given", fd);
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
	  ss->requestor_dialog = w;
	  ss->open_requestor = FROM_INSERT_DIALOG;
	  ss->open_requestor_data = NULL;
	  redirect_snd_error_to(redirect_file_open_error, (void *)fd);
	  ok = insert_complete_file_at_cursor(sp, filename);
	  redirect_snd_error_to(NULL, NULL);
	  if (!ok)
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  clear_error_if_open_changes(fd->dialog, fd);
		  /* ideally insert_complete_file would return an indication of what the error was... */
		}
	    }
	  else 
	    {
	      status_report(sp, "%s inserted at cursor", filename);
	      remember_filename(filename, fd->fpop->file_text_names);
	    }
	  if (filename) XtFree(filename);
	}
      else 
	{
	  char *str;
	  str = mus_format("%s is a directory", filename);
	  file_open_error(str, fd);
	  clear_error_if_open_changes(fd->dialog, fd);
	  free(str);
	}
    }
}
  

static void insert_file_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  insert_file_dialog_help();
}


static file_dialog_info *idat = NULL;

static XEN insert_open_file_watcher(XEN hook_or_reason)
{
  if ((idat->dialog) &&
      (XtIsManaged(idat->dialog)))
    set_sensitive(FSB_BOX(idat->dialog, XmDIALOG_OK_BUTTON), (bool)any_selected_sound());
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
  XEN_ARGIFY_1(insert_open_file_watcher_w, insert_open_file_watcher)
#else
  #define insert_open_file_watcher_w insert_open_file_watcher
#endif

widget_t make_insert_file_dialog(bool managed)
{
  if (!idat)
    {
      idat = make_file_dialog(FILE_READ_ONLY, (char *)"Insert Sound", (char *)"insert:", file_insert_ok_callback, insert_file_help_callback);
      set_dialog_widget(FILE_INSERT_DIALOG, idat->dialog);
      XEN_ADD_HOOK(ss->snd_open_file_hook, insert_open_file_watcher_w, "insert-dialog-open-file-watcher", "insert dialog's open-file-hook handler");
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
#if WITH_AUDIO
  if ((odat) && (odat->dp->play_button))
    XmToggleButtonSetState(odat->dp->play_button, (Boolean)val, false);
  if ((mdat) && (mdat->dp->play_button))
    XmToggleButtonSetState(mdat->dp->play_button, (Boolean)val, false);
  if ((idat) && (idat->dp->play_button))
    XmToggleButtonSetState(idat->dp->play_button, (Boolean)val, false);
#endif
}


void alert_new_file(void) 
{
  if (ss->file_monitor_ok) return;
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
				       int *srate, int *chans, int *type, int *format, mus_long_t *location, mus_long_t *samples, 
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
	  (*location) = string_to_mus_long_t(str, 0, "data location"); 
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
	  (*samples) = string_to_mus_long_t(str, 0, "samples"); 
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
	  str = mus_strdup(comment);
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
					     int type, int format, int srate, int chans, mus_long_t location, mus_long_t samples, char *comment)
{
  int i;
  const char **fl = NULL;
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

  strs = (XmString *)malloc(fdat->formats * sizeof(XmString)); 
  for (i = 0; i < fdat->formats; i++) 
    strs[i] = XmStringCreateLocalized((char *)fl[i]);
  XtVaSetValues(fdat->format_list, 
		XmNitems, strs, 
		XmNitemCount, fdat->formats, 
		NULL);
  for (i = 0; i < fdat->formats; i++)
    XmStringFree(strs[i]);
  free(strs); 
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
    widget_mus_long_t_to_text(fdat->location_text, location);

  if ((samples != IGNORE_SAMPLES) && 
      (fdat->samples_text))
    widget_mus_long_t_to_text(fdat->samples_text, samples);
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
		XmNbackground, ss->yellow,
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
  clear_dialog_error(fd);
  dialog_filename_text = FSB_BOX(fd->dialog, XmDIALOG_TEXT);
  if (dialog_filename_text) XtRemoveCallback(dialog_filename_text, XmNmodifyVerifyCallback, filename_modify_callback, context);
  cbs->doit = true;
}


static void clear_error_if_filename_changes(Widget dialog, file_data *data)
{
  Widget dialog_filename_text;
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
  XtVaGetValues(w, XmNuserData, &fd, NULL);
  fd->current_format = position_to_format(fd->current_type, cbs->item_position - 1);
}


static void file_data_src_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  fd->src = cb->set;
}


static void file_data_auto_comment_callback(Widget w, XtPointer context, XtPointer info)
{
  file_data *fd = (file_data *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  fd->auto_comment = cb->set;
}



/* ---------------- File Data Panel ---------------- */


#define PANEL_COMMENT_SPACE 16

#define WITH_AUTO_COMMENT true
#define WITHOUT_AUTO_COMMENT false

#define WITH_SRATE true
#define WITHOUT_SRATE false


static file_data *make_file_data_panel(Widget parent, const char *name, Arg *in_args, int in_n, 
				       dialog_channels_t with_chan, 
				       int header_type, int data_format,
				       dialog_data_location_t with_loc, 
				       dialog_samples_t with_samples,
				       dialog_header_type_t with_header_type,
				       dialog_comment_t with_comment,
				       header_choice_t header_choice,
				       bool with_src, bool with_auto_comment)
{
  Widget form, header_label, data_label, srate_label, chans_label, sep1, sep2 = NULL, sep3, sep4;
  Widget comment_label = NULL, location_label, samples_label;
  file_data *fdat;
  Arg args[32];
  int i, n;
  XmString *strs;
  int nformats = 0, nheaders = 0;
  const char **formats = NULL, **headers = NULL;

  switch (header_choice)
    {
    case WITH_READABLE_HEADERS: headers = short_readable_headers(&nheaders); break;
    case WITH_WRITABLE_HEADERS: headers = short_writable_headers(&nheaders); break;
    case WITH_BUILTIN_HEADERS:  headers = short_builtin_headers(&nheaders);  break;
    }

  fdat = (file_data *)calloc(1, sizeof(file_data));
  fdat->src = save_as_dialog_src(ss);
  fdat->auto_comment = save_as_dialog_auto_comment(ss);
  fdat->saved_comment = NULL;
  fdat->current_type = header_type;
  fdat->current_format = data_format;
  formats = type_and_format_to_position(fdat, header_type, data_format);
  nformats = fdat->formats;

  /* pick up all args from caller -- args here are attachment points */
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
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep1); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      header_label = XtCreateManagedWidget("header", xmLabelWidgetClass, form, args, n);
      
      /* what is selected depends on current type */
      strs = (XmString *)calloc(nheaders, sizeof(XmString)); 
      for (i = 0; i < nheaders; i++) 
	strs[i] = XmStringCreateLocalized((char *)headers[i]);

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
      fdat->header_list = XmCreateScrolledList(form, (char *)"header-type", args, n);
      XtManageChild(fdat->header_list);

      for (i = 0; i < nheaders; i++) 
	XmStringFree(strs[i]);
      free(strs);
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
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
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
  data_label = XtCreateManagedWidget("data", xmLabelWidgetClass, form, args, n);

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
  fdat->format_list = XmCreateScrolledList(form, (char *)"data-format", args, n);

  strs = (XmString *)calloc(nformats, sizeof(XmString)); 
  for (i = 0; i < nformats; i++) 
    strs[i] = XmStringCreateLocalized((char *)formats[i]);
  XtVaSetValues(fdat->format_list, 
		XmNitems, strs, 
		XmNitemCount, nformats, 
		NULL);
  for (i = 0; i < nformats; i++) 
    XmStringFree(strs[i]);
  free(strs);

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


  /* srate */
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep3); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  srate_label = XtCreateManagedWidget("srate", xmLabelWidgetClass, form, args, n);

  n = 0;
  XtSetArg(args[n], XmNcolumns, 8); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, srate_label); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep3); n++;
  if (with_src)
    {
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
    }
  else
    {
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    }
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
  fdat->srate_text = make_textfield_widget("srate-text", form, args, n, NOT_ACTIVATABLE, add_completer_func(srate_completer, NULL));

  if (with_src)
    {
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, srate_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, fdat->srate_text); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNselectColor, ss->selection_color); n++; /* this is probably clobbered by color_file_selection_box */
      fdat->src_button = make_togglebutton_widget("src", form, args, n);
      XtAddCallback(fdat->src_button, XmNvalueChangedCallback, file_data_src_callback, (XtPointer)fdat);
      XmToggleButtonSetState(fdat->src_button, fdat->src, false);
    }

  if (with_chan != WITHOUT_CHANNELS_FIELD)
    {
      /* chans */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, fdat->srate_text); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      chans_label = XtCreateManagedWidget((char *)((with_chan == WITH_CHANNELS_FIELD) ? "channels" : "extract channel"), xmLabelWidgetClass, form, args, n);

      n = 0;
      XtSetArg(args[n], XmNcolumns, 6); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, chans_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      fdat->chans_text = make_textfield_widget("chans-text", form, args, n, NOT_ACTIVATABLE, NO_COMPLETER);
      XmTextFieldSetString(fdat->chans_text, (char *)"0");

      if (with_loc == WITH_DATA_LOCATION_FIELD)
	{
	  n = 0;
	  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, fdat->chans_text); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, sep3); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
	  location_label = XtCreateManagedWidget("data location", xmLabelWidgetClass, form, args, n);

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
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ((fdat->location_text) ? fdat->location_text : 
				       ((fdat->chans_text) ? fdat->chans_text : fdat->srate_text))); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, sep3); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      samples_label = XtCreateManagedWidget("samples", xmLabelWidgetClass, form, args, n);

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
  XtSetArg(args[n], XmNbottomAttachment, (with_comment != WITHOUT_COMMENT_FIELD) ? XmATTACH_NONE : XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(args[n], XmNheight, (with_comment != WITHOUT_COMMENT_FIELD) ? PANEL_COMMENT_SPACE : 2); n++;
  XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
  sep4 = XtCreateManagedWidget("sep4", xmSeparatorWidgetClass, parent, args, n);

  /* try to make the comment field the one that grows */
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
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++; /* overridden later -> yellow */
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNborderColor, ss->black); n++;
  XtSetArg(args[n], XmNborderWidth, 2); n++;
  XtSetArg(args[n], XmNmarginWidth, 10); n++;
  XtSetArg(args[n], XmNmarginHeight, 10); n++;
  fdat->error_text = XtCreateWidget("", xmLabelWidgetClass, parent, args, n);
  /* XtUnmanageChild(fdat->error_text); */

  if (with_comment != WITHOUT_COMMENT_FIELD)
    {
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep4); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      comment_label = XtCreateManagedWidget("comment", xmLabelWidgetClass, parent, args, n);

      if (with_auto_comment)
	{
	  n = 0;
	  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNtopWidget, sep4); n++;
	  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
	  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
	  fdat->auto_comment_button = make_togglebutton_widget("auto", parent, args, n);
	  XtAddCallback(fdat->auto_comment_button, XmNvalueChangedCallback, file_data_auto_comment_callback, (XtPointer)fdat);
	  XmToggleButtonSetState(fdat->auto_comment_button, fdat->auto_comment, false);
	}

      n = 0;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      if (with_auto_comment)
	{
	  XtSetArg(args[n], XmNtopWidget, fdat->auto_comment_button); n++;
	}
      else
	{
	  XtSetArg(args[n], XmNtopWidget, comment_label); n++;
	}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrows, 4); n++;
      /* XtSetArg(args[n], XmNcolumns, 16); n++; */ /* this sets the lower size, so we don't want it too big */
      fdat->comment_text = make_text_widget("comment-text", parent, args, n);
    }
  else fdat->comment_text = NULL;

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
  char *filename; /* output name (?) */
  save_dialog_t type;
  file_pattern_info *fp;
  dialog_play_info *dp;
  void *file_watcher;
  file_popup_info *fpop;
  const char *original_filename;
} save_as_dialog_info;

static save_as_dialog_info *save_sound_as = NULL, *save_selection_as = NULL, *save_region_as = NULL;


static save_as_dialog_info *new_save_as_dialog_info(save_dialog_t type)
{
  save_as_dialog_info *sd;
  sd = (save_as_dialog_info *)calloc(1, sizeof(save_as_dialog_info));
  sd->type = type;
  return(sd);
}


static void make_auto_comment(save_as_dialog_info *sd)
{
  if ((sd == save_sound_as) &&
      (XtIsManaged(sd->dialog)))
    {
      file_data *fd;
      fd = sd->panel_data;

      if (!(fd->auto_comment))
	{
	  /* don't erase typed-in comment, if any */
	  XmTextSetString(fd->comment_text, fd->saved_comment);
	}
      else
	{
	  snd_info *sp;
	  bool edits = false;
	  int i;
	  char *original_sound_comment, *comment, *orig_comment = NULL;

	  sp = any_selected_sound();

	  original_sound_comment = mus_sound_comment(sp->filename);
	  if (original_sound_comment)
	    {
	      if (*original_sound_comment)
		orig_comment = mus_format("\n%s comment:\n%s\n", sp->short_filename, original_sound_comment);
	      free(original_sound_comment);
	      original_sound_comment = NULL;
	    }

	  if (fd->saved_comment) XtFree(fd->saved_comment);
	  fd->saved_comment = XmTextGetString(fd->comment_text);
	  if ((fd->saved_comment) &&
	      (!(*(fd->saved_comment))))
	    {
	      /* this is the norm in Motif */
	      XtFree(fd->saved_comment);
	      fd->saved_comment = NULL;
	    }

	  for (i = 0; i < sp->nchans; i++)
	    if (sp->chans[i]->edit_ctr != 0)
	      {
		edits = true;
		break;
	      }

	  if (!edits)
	    comment = mus_format("%s%ssaved %s from %s (no edits)\n%s", 
				 (fd->saved_comment) ? fd->saved_comment : "",
				 (fd->saved_comment) ? "\n" : "",
				 snd_local_time(),
				 sp->filename,
				 (orig_comment) ? orig_comment : "");
	  else 
	    {
	      int len;
	      char **edit_strs;
	      char *time;
	  
	      time = snd_local_time();
	      len = 2 * mus_strlen(sp->filename) + 
		    mus_strlen(time) + 
		    32 * sp->nchans + 
		    mus_strlen(fd->saved_comment) + 
		    mus_strlen(original_sound_comment);

	      edit_strs = (char **)malloc(sp->nchans * sizeof(char *));
	      for (i = 0; i < sp->nchans; i++)
		{
		  edit_strs[i] = edit_list_to_function(sp->chans[i], 1, sp->chans[i]->edit_ctr);
		  len += mus_strlen(edit_strs[i]);
		}

	      comment = (char *)calloc(len, sizeof(char));
	      mus_snprintf(comment, len, "%s%ssaved %s from %s with edits:\n", 
			   (fd->saved_comment) ? fd->saved_comment : "",
			   (fd->saved_comment) ? "\n" : "",
			   snd_local_time(),
			   sp->filename);
	      
	      for (i = 0; i < sp->nchans; i++)
		{
		  if (sp->nchans > 1)
		    {
		      char buf[32];
		      snprintf(buf, 32, "\n-------- channel %d --------\n", i);
		      strcat(comment, buf);
		    }
		  strcat(comment, edit_strs[i]);
		}

	      if (orig_comment)
		strcat(comment, orig_comment);
	    }

	  XmTextSetString(fd->comment_text, comment);
	  if (comment) free(comment);
	  if (orig_comment) free(orig_comment);
	}
    }
}


static void auto_comment_callback(Widget w, XtPointer context, XtPointer info)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  sd->panel_data->auto_comment = (cb->set);
  make_auto_comment(sd);
}


void reflect_save_as_src(bool val)
{
  if (save_sound_as)
    XmToggleButtonSetState(save_sound_as->panel_data->src_button, val, true);
  if (save_selection_as)
    XmToggleButtonSetState(save_selection_as->panel_data->src_button, val, true);
  if (save_region_as)
    XmToggleButtonSetState(save_region_as->panel_data->src_button, val, true);
}


void reflect_save_as_auto_comment(bool val)
{
  if (save_sound_as)
    XmToggleButtonSetState(save_sound_as->panel_data->auto_comment_button, val, true);
}


void reflect_save_as_sound_selection(const char *sound_name)
{
  if ((save_sound_as) &&
      (XtIsManaged(save_sound_as->dialog)))
    {
      XmString xmstr2;
      char *file_string;
      file_string = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
      if (sound_name)
	mus_snprintf(file_string, PRINT_BUFFER_SIZE, "save %s", sound_name);
      else 
	{
	  snd_info *sp;
	  sp = any_selected_sound();
	  if (sp)
	    mus_snprintf(file_string, PRINT_BUFFER_SIZE, "save %s", sp->short_filename);
	  else mus_snprintf(file_string, PRINT_BUFFER_SIZE, "nothing to save!");
	}
      xmstr2 = XmStringCreateLocalized(file_string);
      XtVaSetValues(save_sound_as->dialog, XmNdialogTitle, xmstr2, NULL);
      XmStringFree(xmstr2);
      free(file_string);
    }
}


void reflect_selection_in_save_as_dialog(bool on)
{
  if ((on) &&
      (save_selection_as) &&
      (save_selection_as->panel_data))
    clear_dialog_error(save_selection_as->panel_data);
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
  ok_label = XmStringCreateLocalized((char *)"Save");
  XtVaSetValues(sd->dialog,
		XmNokLabelString, ok_label, 
		NULL);
  XmStringFree(ok_label);
  clear_dialog_error(sd->panel_data);
  XtRemoveCallback(sd->filename_widget, XmNmodifyVerifyCallback, save_as_filename_modify_callback, (XtPointer)(sd->panel_data));
  sd->file_watcher = unmonitor_file(sd->file_watcher);
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


static bool srates_differ(int srate, save_as_dialog_info *sd)
{
  switch (sd->type)
    {
    case SOUND_SAVE_AS:
      return(SND_SRATE(any_selected_sound()) != srate);
      
    case SELECTION_SAVE_AS:
      return(selection_srate() != srate);
      
    case REGION_SAVE_AS:
      return(region_srate(region_dialog_region()) != srate);
    }

  return(false);
}


static double srate_ratio(int srate, save_as_dialog_info *sd)
{
  switch (sd->type)
    {
    case SOUND_SAVE_AS:
      return((double)(SND_SRATE(any_selected_sound())) / (double)srate);
      
    case SELECTION_SAVE_AS:
      return((double)selection_srate() / (double)srate);
      
    case REGION_SAVE_AS:
      return((double)region_srate(region_dialog_region()) / (double)srate);
    }

  return(1.0);
}


static void save_or_extract(save_as_dialog_info *sd, bool saving)
{
  char *str = NULL, *comment = NULL, *msg = NULL, *fullname = NULL, *tmpfile = NULL;
  snd_info *sp = NULL;
  int type = MUS_NEXT, format = DEFAULT_OUTPUT_DATA_FORMAT, srate = DEFAULT_OUTPUT_SRATE;
  int output_type, chan = 0, extractable_chans = 0;
  bool file_exists = false;
  io_error_t io_err = IO_NO_ERROR;

  clear_dialog_error(sd->panel_data);

  if ((sd->type == SELECTION_SAVE_AS) &&
      (!(selection_is_active())))
    {
      if (saving)
	msg = (char *)"no selection to save";
      else msg = (char *)"can't extract: no selection";
      post_file_dialog_error((const char *)msg, sd->panel_data);
      return;
    }

  if ((sd->type == REGION_SAVE_AS) &&
      (!(region_ok(region_dialog_region()))))
    {
      post_file_dialog_error("no region to save", sd->panel_data);
      return;
    }

  sp = any_selected_sound();
  if ((!sp) && 
      (sd->type != REGION_SAVE_AS))
    {
      if (saving)
	msg = (char *)"nothing to save";
      else msg = (char *)"nothing to extract";
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data);
      return;
    }

  /* get output filename */
  str = XmTextGetString(sd->filename_widget);
  if ((!str) || (!*str))
    {
      if (saving)
	msg = (char *)"can't save: no file name given";
      else msg = (char *)"can't extract: no file name given";
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data);
      return;
    }

  /* get output file attributes */
  redirect_snd_error_to(post_file_panel_error, (void *)(sd->panel_data));
  {
    mus_long_t location = 28, samples = 0;
    int chans = 1;
    if (saving)
      comment = get_file_dialog_sound_attributes(sd->panel_data, &srate, &chans, &type, &format, &location, &samples, 0);
    else comment = get_file_dialog_sound_attributes(sd->panel_data, &srate, &chan, &type, &format, &location, &samples, 0);
  }
  output_type = type;
  redirect_snd_error_to(NULL, NULL);

  if (sd->panel_data->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(sd->dialog, sd->panel_data);
      if (comment) free(comment);
      XtFree(str);
      return;
    }

  switch (sd->type)
    {
    case SOUND_SAVE_AS:
      clear_status_area(sp);
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
	  free(msg);
	  if (comment) free(comment);
	  XtFree(str);
	  return;
	}
    }

  fullname = mus_expand_filename(str);
  if (run_before_save_as_hook(sp, fullname, sd->type != SOUND_SAVE_AS, srate, type, format, comment))
    {
      msg = mus_format("%s cancelled by %s", (saving) ? "save" : "extract", S_before_save_as_hook);
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data);      
      free(msg);
      free(fullname);
      if (comment) free(comment);
      XtFree(str);
      return;
    }

  file_exists = mus_file_probe(fullname);
  if ((sd->type == SOUND_SAVE_AS) &&
      (mus_strcmp(fullname, sp->filename)))
    {
      /* save-as here is the same as save */
      if ((sp->user_read_only == FILE_READ_ONLY) || 
	  (sp->file_read_only == FILE_READ_ONLY))
	{
	  msg = mus_format("can't overwrite %s (it is write-protected)", sp->short_filename);
	  post_file_dialog_error((const char *)msg, sd->panel_data);
	  clear_error_if_filename_changes(sd->dialog, sd->panel_data); 
	  free(msg);
	  free(fullname);
	  if (comment) free(comment);
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
	      msg = mus_format("%s exists%s. To overwrite it, click 'DoIt'", 
			       str,
			       (parlous_sp) ? ", and has unsaved edits" : "");
	      post_file_dialog_error((const char *)msg, sd->panel_data);
	      clear_error_if_save_as_filename_changes(sd->dialog, sd);
	      ok_label = XmStringCreateLocalized((char *)"DoIt");
	      XtVaSetValues(sd->dialog, 
			    XmNokLabelString, ok_label, 
			    NULL);
	      XmUpdateDisplay(FSB_BOX(sd->dialog, XmDIALOG_OK_BUTTON));
	      XmStringFree(ok_label);
	      free(msg);
	      free(fullname);
	      if (comment) free(comment);
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
	else ofile = mus_strdup(tmpfile);
	io_err = save_selection(ofile, type, format, srate, comment, (saving) ? SAVE_ALL_CHANS : chan);
	if (io_err == IO_NO_ERROR)
	  io_err = move_file(ofile, fullname);
	free(ofile);
      }
      break;

    case REGION_SAVE_AS:
      {
	char *ofile;
	if (region_ok(region_dialog_region()))
	  {
	    if (file_exists)
	      ofile = snd_tempnam();
	    else ofile = mus_strdup(tmpfile);
	    io_err = save_region(region_dialog_region(), ofile, type, format, comment);
	    if (io_err == IO_NO_ERROR)
	      io_err = move_file(ofile, fullname);
	    free(ofile);
	  }
      }
      break;
    }
  redirect_snd_error_to(NULL, NULL);

  /* check for possible srate conversion */
  if ((sd->panel_data->src) &&
      (srates_differ(srate, sd)))
    {
      /* if src, and srates differ, do the sampling rate conversion.
       *    this needs to happen before the snd_encode (->OGG etc) below
       *    if we do it before the save-as above, then undo it later, it messes up the user's edit history list
       *    so do it here to tmpfile (tmpfile is fullname unless we're doing a translation to something like OGG)
       */
      src_file(tmpfile, srate_ratio(srate, sd));
    }

  if (io_err == IO_NO_ERROR)
    {
      if (encoded_header_p(output_type))
	{
	  snd_encode(output_type, tmpfile, fullname);
	  snd_remove(tmpfile, REMOVE_FROM_CACHE);
	  free(tmpfile);
	}
      remember_filename(fullname, sd->fpop->file_text_names);

      if (!file_exists)
	force_directory_reread(sd->dialog);
      if (saving)
	{
	  if (sd->type == SOUND_SAVE_AS)
	    status_report(sp, "%s saved as %s", sp->short_filename, str);
	  else status_report(sp, "%s saved as %s", (sd->type == SELECTION_SAVE_AS) ? "selection" : "region", str);
	}
      else
	{
	  if (sd->type == SOUND_SAVE_AS)
	    status_report(sp, "%s chan %d saved as %s", sp->short_filename, chan, str);
	  else status_report(sp, "selection chan %d saved as %s", chan, str);
	}
      run_after_save_as_hook(sp, str, true); /* true => from dialog */
      XtUnmanageChild(sd->dialog);
    }
  else
    {
      msg = mus_format("%s as %s: %s (%s)", (saving) ? "save" : "extract chan", str, io_error_name(io_err), snd_io_strerror());
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data);
      free(msg);
    }

  free(fullname);
  XtFree(str);
  if (comment) free(comment);
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
#if WITH_AUDIO
  dialog_play_info *dp = (dialog_play_info *)context;
  char *filename = NULL;
  XmString *strs;
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
#endif
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


static bool directory_exists(char *name)
{
  char temp;
  bool result;
  int i, len, last_slash = -1;
  len = strlen(name);
  for (i = 0; i < len; i++) 
    if (name[i] == '/') 
      last_slash = i;
  if (last_slash <= 0)
    return(true);
  if (last_slash >= len - 1) /* can't be > */
    return(directory_p(name));
  temp = name[last_slash + 1];
  name[last_slash + 1] = '\0';
  result = directory_p(name);
  name[last_slash + 1] = temp;
  return(result);
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
	    s1 = XmStringCreateLocalized((char *)"save as (file write-protected?):");
	  else
#endif
	    s1 = XmStringCreateLocalized((char *)"save as (overwriting):");
	}
      else
	{
	  if (!(directory_exists(filename)))
	    s1 = XmStringCreateLocalized((char *)"save as (no such directory?):");
	  else s1 = XmStringCreateLocalized((char *)"save as:");
	}
    }
  else s1 = XmStringCreateLocalized((char *)"save as:");
  XtVaSetValues(dialog, 
		XmNselectionLabelString, s1, 
		NULL);
  if (filename) XtFree(filename);
}


static int snd_mkdir(const char *filename)
{
#ifdef __MINGW32__ 
  return(mkdir(filename));
#else 
  return(mkdir(filename, 0777));
#endif 
}


static void save_as_mkdir_callback(Widget w, XtPointer context, XtPointer info)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  char *filename = NULL;
  filename = XmTextGetString(FSB_BOX(sd->dialog, XmDIALOG_TEXT));
  if (snd_mkdir(filename) < 0)
    {
      /* could not make the directory */
      char *str;
      str = mus_format("can't make %s: %s", filename, strerror(errno));
      post_file_dialog_error((const char *)str, sd->panel_data);
      clear_error_if_filename_changes(sd->dialog, sd->panel_data); 
      free(str);
    }
  else
    {
      /* set FSB to new dir and force update */
      char *filter;
      filter = mus_format("%s*", filename); /* already has the "/" at the end */
      update_dir_list(sd->dialog, filter);
      free(filter);
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

  sd->original_filename = sound_name;
  if (!(sd->dialog))
    {
      Arg args[32];
      int n;
      XmString xmstr1, xmstr2, s1;
      XmString filter_list_label, cancel_label;
      Widget extractB, mainform;

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      s1 = XmStringCreateLocalized((char *)"save as:");
      XtSetArg(args[n], XmNselectionLabelString, s1); n++;

      xmstr1 = XmStringCreateLocalized((char *)"Save");
      XtSetArg(args[n], XmNokLabelString, xmstr1); n++;

      file_string = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, "save %s", sound_name);

      xmstr2 = XmStringCreateLocalized(file_string);
      XtSetArg(args[n], XmNdialogTitle, xmstr2); n++;

      filter_list_label = XmStringCreateLocalized((char *)"files listed:");
      XtSetArg(args[n], XmNfilterLabelString, filter_list_label); n++;

      cancel_label = XmStringCreateLocalized((char *)I_GO_AWAY);
      XtSetArg(args[n], XmNcancelLabelString, cancel_label); n++;

      sd->fp = (file_pattern_info *)calloc(1, sizeof(file_pattern_info));
      sd->fp->in_just_sounds_update = false;
      if (just_sounds(ss))
	sd->fp->filter_choice = JUST_SOUNDS_FILTER;
      else sd->fp->filter_choice = NO_FILE_FILTER;

      sd->dp = (dialog_play_info *)calloc(1, sizeof(dialog_play_info));
      sd->fpop = (file_popup_info *)calloc(1, sizeof(file_popup_info));
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

      sd->dialog = XmCreateFileSelectionDialog(MAIN_SHELL(ss), (char *)"save-as", args, n);
      sd->fp->dialog = sd->dialog;
      sd->dp->dialog = sd->dialog;
      sd->fpop->dialog = sd->dialog;

      free(file_string);

      XtUnmanageChild(FSB_BOX(sd->dialog, XmDIALOG_DIR_LIST_LABEL));
      XtUnmanageChild(FSB_BOX(sd->dialog, XmDIALOG_LIST_LABEL));
      XtUnmanageChild(FSB_BOX(sd->dialog, XmDIALOG_APPLY_BUTTON));

      XtVaSetValues(FSB_BOX(sd->dialog, XmDIALOG_FILTER_LABEL), XmNbackground, ss->basic_color, NULL);
      XtVaSetValues(FSB_BOX(sd->dialog, XmDIALOG_SELECTION_LABEL), XmNbackground, ss->basic_color, NULL);

      XmStringFree(s1);
      XmStringFree(xmstr1);
      XmStringFree(xmstr2);
      XmStringFree(filter_list_label);
      XmStringFree(cancel_label);

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
					    WITH_HEADER_TYPE_FIELD, 
					    WITH_COMMENT_FIELD,
					    WITH_WRITABLE_HEADERS,
					    WITH_SRATE,
					    sd->type == SOUND_SAVE_AS); /* auto comment */

      sd->panel_data->dialog = sd->dialog;

      color_file_selection_box(sd->dialog);

      XtVaSetValues(sd->panel_data->format_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
      XtVaSetValues(sd->panel_data->header_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
      XtVaSetValues(sd->fp->just_sounds_button, XmNselectColor, ss->selection_color, NULL);
#if WITH_AUDIO
      XtVaSetValues(sd->dp->play_button, XmNselectColor, ss->selection_color, NULL);
#endif

      XtVaSetValues(sd->panel_data->src_button, XmNselectColor, ss->selection_color, NULL);
      if (sd->type == SOUND_SAVE_AS)
	{
	  XtVaSetValues(sd->panel_data->auto_comment_button, XmNselectColor, ss->selection_color, NULL);
	  XtAddCallback(sd->panel_data->auto_comment_button, XmNvalueChangedCallback, auto_comment_callback, (XtPointer)sd);
	}
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
	  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
	  XtSetArg(args[n], XmNarmColor,   ss->selection_color); n++;
	  extractB = XtCreateManagedWidget("Extract", xmPushButtonGadgetClass, sd->dialog, args, n);
	  XtAddCallback(extractB, XmNactivateCallback, save_as_extract_callback, (XtPointer)sd);
	  sd->extractB = extractB;

	  XtSetSensitive(extractB, (!(file_is_directory(sd->dialog))));
	  XtAddCallback(FSB_BOX(sd->dialog, XmDIALOG_TEXT), XmNvalueChangedCallback, reflect_text_in_extract_button, (void *)sd);
	}
	 
      /* add "Mkdir" button */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor,   ss->selection_color); n++;
      sd->mkdirB = XtCreateManagedWidget("Mkdir", xmPushButtonGadgetClass, sd->dialog, args, n);
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
      file_string = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(file_string, PRINT_BUFFER_SIZE, "save %s", sound_name);
      xmstr2 = XmStringCreateLocalized(file_string);
      XtVaSetValues(sd->dialog, 
		    XmNdialogTitle, xmstr2, 
		    NULL);
      XmStringFree(xmstr2);
      free(file_string);
    }
}


static save_as_dialog_info *make_sound_save_as_dialog_1(bool managed, int chan)
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
  if (com) free(com);

  if (chan >= 0)
    {
      char *chan_str;  
      chan_str = (char *)calloc(8, sizeof(char));
      snprintf(chan_str, 8, "%d", chan);
      XmTextFieldSetString(sd->panel_data->chans_text, chan_str);
      free(chan_str);
    }

  if (sd->fp->reread_directory) 
    {
      force_directory_reread(sd->dialog);
      sd->fp->reread_directory = false;
    }

  if ((managed) && (!XtIsManaged(sd->dialog))) 
    XtManageChild(sd->dialog);

  make_auto_comment(sd);
  return(sd);
}


widget_t make_sound_save_as_dialog(bool managed)
{
  save_as_dialog_info *sd;
  sd = make_sound_save_as_dialog_1(managed, -1);
  return(sd->dialog);
}


void make_channel_extract_dialog(int chan)
{
  make_sound_save_as_dialog_1(true, chan);
}


widget_t make_selection_save_as_dialog(bool managed)
{
  save_as_dialog_info *sd;

  if (!save_selection_as)
    save_selection_as = new_save_as_dialog_info(SELECTION_SAVE_AS);
  sd = save_selection_as;

  make_save_as_dialog(sd,
		      (char *)"current selection",
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
		      (char *)"selected region",
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
  if (comment) free(comment);
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
static mus_long_t initial_samples = 1;
static Widget new_file_text = NULL;
static char *new_file_filename = NULL;
static void *new_file_watcher = NULL;


static void new_filename_modify_callback(Widget w, XtPointer context, XtPointer info);

static void new_file_undoit(void)
{
  XmString ok_label;
  ok_label = XmStringCreateLocalized((char *)"Ok");
  XtVaSetValues(new_file_dialog, 
		XmNokLabelString, ok_label, 
		NULL);
  XmStringFree(ok_label);
  clear_dialog_error(ndat);
  XtRemoveCallback(new_file_text, XmNmodifyVerifyCallback, new_filename_modify_callback, NULL);
  new_file_watcher = unmonitor_file(new_file_watcher);
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


static void new_file_ok_callback(Widget w, XtPointer context, XtPointer info) 
{
  mus_long_t loc;
  char *comment = NULL, *newer_name = NULL, *msg;
  int header_type, data_format, srate, chans;
  newer_name = XmTextGetString(new_file_text);
  if ((!newer_name) || (!(*newer_name)))
    {
      msg = (char *)"new sound needs a file name ('New file:' field is empty)";
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
	  if (new_file_filename) free(new_file_filename);
	  new_file_filename = mus_expand_filename(newer_name); 
	  if ((!new_file_watcher) &&
	      (ask_before_overwrite(ss)) && 
	      (mus_file_probe(new_file_filename)))
	    {
	      XmString ok_label;
	      msg = mus_format("%s exists. If you want to overwrite it, click 'DoIt'", newer_name);
	      post_file_dialog_error((const char *)msg, ndat);
	      clear_error_if_new_filename_changes(new_file_dialog);
	      ok_label = XmStringCreateLocalized((char *)"DoIt");
	      XtVaSetValues(new_file_dialog, 
			    XmNokLabelString, ok_label, 
			    NULL);
	      XmUpdateDisplay(MSG_BOX(new_file_dialog, XmDIALOG_OK_BUTTON));
	      XmStringFree(ok_label);
	      free(msg);
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
		  clear_error_if_new_filename_changes(new_file_dialog);
		}
	      else
		{
		  XtUnmanageChild(new_file_dialog);
		}
	    }
	}
      XtFree(newer_name);
      if (comment) free(comment);
    }
}


static char *new_file_dialog_filename(int header_type)
{
  static int new_file_dialog_file_ctr = 1;
  char *filename = NULL;
  const char *extension = NULL;
  filename = (char *)calloc(64, sizeof(char));
  switch (header_type)
    {
    case MUS_AIFC: extension = "aiff"; break;
    case MUS_AIFF: extension = "aiff"; break;
    case MUS_RIFF: extension = "wav";  break;
    case MUS_RF64: extension = "wav";  break;
    case MUS_CAFF: extension = "caf";  break;
    default:       extension = "snd";  break;
    }
  mus_snprintf(filename, 64, "new-%d.%s", new_file_dialog_file_ctr++, extension);
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

  if (new_comment) free(new_comment);
  if (filename) free(filename);
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

      titlestr = XmStringCreateLocalized((char *)"New file");
      xhelp = XmStringCreateLocalized((char *)I_HELP);
      xcancel = XmStringCreateLocalized((char *)I_GO_AWAY);
      xok = XmStringCreateLocalized((char *)"Ok");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNcancelLabelString, xcancel); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, xok); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      new_file_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"new", args, n);

      XmStringFree(titlestr);
      XmStringFree(xok);
      XmStringFree(xcancel);
      XmStringFree(xhelp);

      XtAddCallback(new_file_dialog, XmNhelpCallback,   new_file_help_callback,   NULL);
      XtAddCallback(new_file_dialog, XmNcancelCallback, new_file_cancel_callback, NULL);
      XtAddCallback(new_file_dialog, XmNokCallback,     new_file_ok_callback,     NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      reset_button = XtCreateManagedWidget("Reset", xmPushButtonGadgetClass, new_file_dialog, args, n);
      XtAddCallback(reset_button, XmNactivateCallback, new_file_reset_callback, NULL);

      n = 0;
      form = XtCreateManagedWidget("newfile", xmFormWidgetClass, new_file_dialog, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNforeground, ss->black); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      name_label = XtCreateManagedWidget("New file:", xmLabelWidgetClass, form, args, n);

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
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD,
				  WITH_BUILTIN_HEADERS,
				  WITHOUT_SRATE, 
				  WITHOUT_AUTO_COMMENT);
      ndat->dialog = new_file_dialog;
      XtManageChild(ndat->error_text);
      XtManageChild(new_file_dialog);

      map_over_children(new_file_dialog, set_main_color_of_widget);
      XtVaSetValues(ndat->format_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
      XtVaSetValues(ndat->header_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);

      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->highlight_color,   NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color,   NULL);
      XtVaSetValues(MSG_BOX(new_file_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->highlight_color,   NULL);

      set_dialog_widget(NEW_FILE_DIALOG, new_file_dialog);
      XtUnmanageChild(ndat->error_text); 

      load_new_file_defaults(NULL);
    }
  else
    {
      char *new_name;
      new_name = XmTextGetString(new_file_text);

      if (new_file_watcher)
	{
	  /* if overwrite question pends, but file has been deleted in the meantime, go back to normal state */
	  if ((!new_name) || (!(*new_name)) ||
	      (!(mus_file_probe(new_name))))
	    new_file_undoit();
	}

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
	      free(filename);
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
  void *file_ro_watcher;
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
      edhead_infos = (edhead_info **)calloc(edhead_info_size, sizeof(edhead_info *));
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
	  edhead_infos = (edhead_info **)realloc(edhead_infos, edhead_info_size * sizeof(edhead_info *));
	  for (i = loc; i < edhead_info_size; i++) edhead_infos[i] = NULL;
	}
    }
  if (!edhead_infos[loc])
    {
      edhead_infos[loc] = (edhead_info *)calloc(1, sizeof(edhead_info));
      edhead_infos[loc]->dialog = NULL;
      edhead_infos[loc]->panel_changed = false;
    }
  edhead_infos[loc]->sp = NULL;
  edhead_infos[loc]->file_ro_watcher = NULL;
  return(edhead_infos[loc]);
}


static XmString make_header_dialog_title(edhead_info *ep, snd_info *sp)
{
  /* dialog may not yet exist */
  char *str;
  XmString xstr;
  str = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, "Add header to (write-protected) %s", sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, "Edit header of (write-protected) %s", sp->short_filename);
      if (ep->dialog)
	set_sensitive(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON), (sp->hdr->type == MUS_RAW));
    }
  else 
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, "Add header to %s", sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, "Edit header of %s", sp->short_filename);
      if (ep->dialog)
	set_sensitive(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON), ep->panel_changed);
    }
  xstr = XmStringCreateLocalized(str);
  free(str);
  return(xstr);
}


static void edit_header_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  edit_header_dialog_help();
}


static void edit_header_set_ok_sensitive(Widget w, XtPointer context, XtPointer info)
{
  edhead_info *ep = (edhead_info *)context;
  if (ep->sp->file_read_only == FILE_READ_WRITE)
    set_sensitive(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON), true);
  ep->panel_changed = true;
}


static void eh_cancel(edhead_info *ep)
{
  unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
  ep->panel_changed = false;
  if ((ep->file_ro_watcher) &&
      (ep->sp) &&
      (ep->sp->active) &&
      (ep->sp->filename))
    ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
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
	  ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
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
	      (mus_strcmp(sp->filename, edhead_infos[i]->sp->filename)))))
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
      xstr1 = XmStringCreateLocalized((char *)I_GO_AWAY); /* needed by template dialog */
      xstr2 = XmStringCreateLocalized((char *)I_HELP);
      xstr3 = XmStringCreateLocalized((char *)"Save");
      titlestr = XmStringCreateLocalized((char *)"Edit Header");

      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xstr1); n++;
      XtSetArg(args[n], XmNhelpLabelString, xstr2); n++;
      XtSetArg(args[n], XmNokLabelString, xstr3); n++;
      XtSetArg(args[n], XmNmessageString, xstr4); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      ep->dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"Edit Header", args, n);

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
				      WITH_HEADER_TYPE_FIELD, 
				      WITH_COMMENT_FIELD,
				      WITH_BUILTIN_HEADERS,
				      WITHOUT_SRATE, 
				      WITHOUT_AUTO_COMMENT);
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
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->highlight_color,   NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color,   NULL);
      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->highlight_color,   NULL);
      XtVaSetValues(ep->edat->header_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
      XtVaSetValues(ep->edat->format_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);

      XtVaSetValues(MSG_BOX(ep->dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->basic_color, NULL);

      set_dialog_widget(EDIT_HEADER_DIALOG, ep->dialog);

      {
	Atom wm_delete_window;
	wm_delete_window = XmInternAtom(MAIN_DISPLAY(ss), (char *)"WM_DELETE_WINDOW", false);
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




/* -------------------------------- Raw Data Dialog -------------------------------- */

/* we keep an array of raw data dialogs so that any number can be active at once */

typedef struct raw_info {
  Widget dialog;
  mus_long_t location;
  file_data *rdat;
  read_only_t read_only;
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
      raw_infos = (raw_info **)calloc(raw_info_size, sizeof(raw_info *));
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
	  raw_infos = (raw_info **)realloc(raw_infos, raw_info_size * sizeof(raw_info *));
	  for (i = loc; i < raw_info_size; i++) raw_infos[i] = NULL;
	}
    }
  if (!raw_infos[loc])
    {
      raw_infos[loc] = (raw_info *)calloc(1, sizeof(raw_info));
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
	      mix_complete_file_at_cursor(any_selected_sound(), rp->filename);
	      break;

	    case FROM_INSERT_DIALOG:
	      redirect_snd_error_to(redirect_file_open_error, (void *)idat);
	      insert_complete_file_at_cursor(any_selected_sound(), rp->filename);
	      break;

	    case FROM_VIEW_FILES_MIX_DIALOG:
	      {
		view_files_info *vdat = (view_files_info *)(rp->requestor_data);
		redirect_snd_error_to(redirect_vf_post_error, rp->requestor_data);
		vf_mix(vdat);
	      }
	      break;

	    case FROM_VIEW_FILES_INSERT_DIALOG:
	      {
		view_files_info *vdat = (view_files_info *)(rp->requestor_data);
		redirect_snd_error_to(redirect_vf_post_error, rp->requestor_data);
		vf_insert(vdat);
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
	  hdr = (file_info *)calloc(1, sizeof(file_info));
	  hdr->name = mus_strdup(rp->filename);
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
	      clear_status_area((snd_info *)(rp->requestor_data));
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
  XmString go_away, xhelp, xok, xtitle, titlestr;
  int n;
  int raw_srate, raw_chans, raw_data_format;
  Arg args[20];
  Widget reset_button, main_w;

  go_away = XmStringCreateLocalized((char *)I_GO_AWAY); /* needed by template dialog */
  xhelp = XmStringCreateLocalized((char *)I_HELP);
  xok = XmStringCreateLocalized((char *)"Ok");
  if (!title)
    titlestr = XmStringCreateLocalized((char *)"No header on file");
  else titlestr = XmStringCreateLocalized((char *)title);
  xtitle = XmStringCreateLocalized((char *)title);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  XtSetArg(args[n], XmNcancelLabelString, go_away); n++;
  XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
  XtSetArg(args[n], XmNokLabelString, xok); n++;
  XtSetArg(args[n], XmNmessageString, xtitle); n++;
  XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNallowShellResize, true); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNautoUnmanage, false); n++;
  /* not transient -- we want this window to remain visible if possible */
  rp->dialog = XmCreateWarningDialog(MAIN_SHELL(ss), (char *)"raw data", args, n);

  XtAddCallback(rp->dialog, XmNcancelCallback, raw_data_cancel_callback, (XtPointer)rp);
  XtAddCallback(rp->dialog, XmNhelpCallback,   raw_data_help_callback,   (XtPointer)rp);
  XtAddCallback(rp->dialog, XmNokCallback,     raw_data_ok_callback,     (XtPointer)rp);
  XmStringFree(go_away);
  XmStringFree(xhelp);
  XmStringFree(xok);
  XmStringFree(xtitle);
  XmStringFree(titlestr);

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
  XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
  reset_button = XtCreateManagedWidget("Reset", xmPushButtonGadgetClass, rp->dialog, args, n);
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
				  WITHOUT_HEADER_TYPE_FIELD, 
				  WITHOUT_COMMENT_FIELD,
				  WITH_READABLE_HEADERS,
				  WITHOUT_SRATE, 
				  WITHOUT_AUTO_COMMENT);
  rp->rdat->dialog = rp->dialog;

  set_file_dialog_sound_attributes(rp->rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, rp->location, 
				   IGNORE_SAMPLES, NULL);

  map_over_children(rp->dialog, set_main_color_of_widget);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->selection_color, NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->selection_color, NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->selection_color, NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->highlight_color,   NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color,   NULL);
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->highlight_color,   NULL);
  XtVaSetValues(reset_button, XmNselectColor, ss->selection_color, NULL);
  XtVaSetValues(rp->rdat->format_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
  /*
   * this line makes the dialog take up all vertical space on the screen
   * XtManageChild(rp->rdat->error_text);
  */
  XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->basic_color, NULL);

  XtManageChild(rp->dialog);
  XtUnmanageChild(rp->rdat->error_text); 
  set_dialog_widget(RAW_DATA_DIALOG, rp->dialog);
}


void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, read_only_t read_only, bool selected)
{
  /* put up dialog for srate, chans, data format */
  raw_info *rp;
  rp = new_raw_dialog();
  rp->read_only = read_only;
  rp->selected = selected;
  if (rp->filename) free(rp->filename);
  rp->filename = mus_strdup(filename);
  rp->requestor = ss->open_requestor;
  rp->requestor_data = ss->open_requestor_data;
  rp->requestor_dialog = ss->requestor_dialog;
  ss->open_requestor = NO_REQUESTOR;
  ss->requestor_dialog = NULL;
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
  free(title);
  if (rp->help) free(rp->help);
  if (info)
    {
      XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_HELP_BUTTON), 
		    XmNbackground, ss->green, 
		    NULL);
      rp->help = mus_strdup(info);
      free(info);
    }
  else
    {
      XtVaSetValues(MSG_BOX(rp->dialog, XmDIALOG_HELP_BUTTON), 
		    XmNbackground, ss->highlight_color, 
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
  XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  post_it_dialog = XmCreateMessageDialog(MAIN_PANE(ss), (char *)"info", args, n);

  XtUnmanageChild(MSG_BOX(post_it_dialog, XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild(MSG_BOX(post_it_dialog, XmDIALOG_HELP_BUTTON));
  XtUnmanageChild(MSG_BOX(post_it_dialog, XmDIALOG_SYMBOL_LABEL));

  XtVaSetValues(MSG_BOX(post_it_dialog, XmDIALOG_MESSAGE_LABEL), XmNbackground, ss->highlight_color, NULL);
      
  n = 0;
  XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable, false); n++;
  XtSetArg(args[n], XmNcolumns, POST_IT_COLUMNS); n++;
  XtSetArg(args[n], XmNrows, POST_IT_ROWS); n++;
  XtSetArg(args[n], XmNforeground, ss->black); n++; /* needed if color allocation fails completely */
  XtSetArg(args[n], XmNbackground, ss->white); n++;
  post_it_text = XmCreateScrolledText(post_it_dialog, (char *)"post-it-text", args, n);
  XtManageChild(post_it_text);
  XtManageChild(post_it_dialog);

  map_over_children(post_it_dialog, set_main_color_of_widget);
  XtVaSetValues(post_it_text, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
  XtVaSetValues(MSG_BOX(post_it_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->selection_color, NULL);
  XtVaSetValues(MSG_BOX(post_it_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->highlight_color, NULL);

  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}


widget_t post_it(const char *subject, const char *str)
{
  /* place string in scrollable help window */
  XmString xstr1, xstr2;
  if (ss == NULL) return(NULL); /* an attempt to call this before X/Motif is ready */
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


void post_it_append(const char *str)
{
  if (post_it_dialog)
    XmTextInsert(post_it_text, XmTextGetLastPosition(post_it_text), (char *)str);
}


void save_post_it_dialog_state(FILE *fd)
{
  if ((post_it_dialog) && (XtIsManaged(post_it_dialog)))
    {
      char *subject = NULL, *text = NULL;
      subject = dialog_get_title(post_it_dialog);
      text = XmTextGetString(post_it_text);
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\" \"%s\")\n", S_info_dialog, subject, text);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\", \"%s\")\n", TO_PROC_NAME(S_info_dialog), subject, text);
#endif
#if HAVE_FORTH
      fprintf(fd, "\"%s\" \"%s\" %s drop\n", subject, text, S_info_dialog);
#endif
      if (subject) free(subject);
      if (text) XtFree(text);
    }
}


/* ---------------- unsaved edits dialog ---------------- */

static int num_unsaved_edits_dialogs = 0;
static Widget *unsaved_edits_dialogs = NULL;
static snd_info **unsaved_edits_sounds = NULL;

static Widget unsaved_edits_dialog(snd_info *sp)
{
  int i;
  /* are there any such dialogs? */
  if (num_unsaved_edits_dialogs == 0)
    return(NULL);

  /* now see if we've already prompted about this sound */
  for (i = 0; i < num_unsaved_edits_dialogs; i++)
    if (unsaved_edits_sounds[i] == sp)
      return(unsaved_edits_dialogs[i]);

  /* try to find a free unmanaged dialog */
  for (i = 0; i < num_unsaved_edits_dialogs; i++)
    if ((unsaved_edits_dialogs[i]) &&
	(!XtIsManaged(unsaved_edits_dialogs[i])))
      return(unsaved_edits_dialogs[i]);

  return(NULL);
}

static void save_unsaved_edits_dialog(Widget d, snd_info *sp)
{
  if (num_unsaved_edits_dialogs == 0)
    {
      unsaved_edits_dialogs = (Widget *)calloc(1, sizeof(Widget));
      unsaved_edits_sounds = (snd_info **)calloc(1, sizeof(snd_info *));
    }
  else
    {
      unsaved_edits_dialogs = (Widget *)realloc(unsaved_edits_dialogs, (num_unsaved_edits_dialogs + 1) * sizeof(Widget));
      unsaved_edits_sounds = (snd_info **)realloc(unsaved_edits_sounds, (num_unsaved_edits_dialogs + 1) * sizeof(snd_info *));
    }

  unsaved_edits_dialogs[num_unsaved_edits_dialogs] = d;
  unsaved_edits_sounds[num_unsaved_edits_dialogs] = sp;
  num_unsaved_edits_dialogs++;
}


void unpost_unsaved_edits_if_any(snd_info *sp)
{
  int i;
  for (i = 0; i < num_unsaved_edits_dialogs; i++)
    if (((unsaved_edits_sounds[i] == sp) ||
	 (!snd_ok(unsaved_edits_sounds[i]))) &&
	(XtIsManaged(unsaved_edits_dialogs[i])))
      XtUnmanageChild(unsaved_edits_dialogs[i]);
}


static void zero_edits(chan_info *cp)
{
  cp->edit_ctr = 0;
}

static void unsaved_edits_no_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  for_each_sound_chan(sp, zero_edits);
  snd_close_file(sp);
}

static void unsaved_edits_yes_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  save_edits(sp);
  snd_close_file(sp);
}

static void unsaved_edits_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help("save edits?", 
	   "You have set " S_ask_about_unsaved_edits " to " PROC_TRUE ", so you will be asked whether you want \
to save edits if you try to close a sound that has unsaved edits.",
	   WITH_WORD_WRAP);
}

void save_edits_now(snd_info *sp)
{
  char *question;
  XmString q;
  Widget dialog;

  question = mus_format("%s has unsaved edits.  Save them?", sp->short_filename);
  q = XmStringCreateLocalized(question);

  dialog = unsaved_edits_dialog(sp);
  if (!dialog)
    {
      Arg args[20];
      int n;
      XmString yes, no;

      yes = XmStringCreateLocalized((char *)"yes");
      no = XmStringCreateLocalized((char *)"no");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNokLabelString, yes); n++;
      XtSetArg(args[n], XmNcancelLabelString, no); n++;
      XtSetArg(args[n], XmNmessageString, q); n++;
      dialog = XmCreateQuestionDialog(MAIN_PANE(ss), sp->filename, args, n);
      save_unsaved_edits_dialog(dialog, sp);

      XtAddCallback(dialog, XmNhelpCallback,   unsaved_edits_help_callback, (XtPointer)sp);
      XtAddCallback(dialog, XmNokCallback,     unsaved_edits_yes_callback,  (XtPointer)sp);
      XtAddCallback(dialog, XmNcancelCallback, unsaved_edits_no_callback,   (XtPointer)sp);

      XmStringFree(yes);
      XmStringFree(no);

      map_over_children(dialog, set_main_color_of_widget);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->highlight_color, NULL);
    }
  else
    {
      XtVaSetValues(dialog, XmNmessageString, q, NULL);
    }

  XmStringFree(q);
  free(question);
  XtManageChild(dialog);
}



/* ---------------- file has changed dialog ---------------- */

static int num_file_has_changed_dialogs = 0;
static Widget *file_has_changed_dialogs = NULL;
static snd_info **file_has_changed_sounds = NULL;

static Widget file_has_changed_dialog(snd_info *sp)
{
  int i;
  /* are there any such dialogs? */
  if (num_file_has_changed_dialogs == 0)
    return(NULL);

  /* now see if we've already prompted about this sound */
  for (i = 0; i < num_file_has_changed_dialogs; i++)
    if (file_has_changed_sounds[i] == sp)
      return(file_has_changed_dialogs[i]);

  /* try to find a free unmanaged dialog */
  for (i = 0; i < num_file_has_changed_dialogs; i++)
    if ((file_has_changed_dialogs[i]) &&
	(!XtIsManaged(file_has_changed_dialogs[i])))
      return(file_has_changed_dialogs[i]);

  return(NULL);
}

static void save_file_has_changed_dialog(Widget d, snd_info *sp)
{
  if (num_file_has_changed_dialogs == 0)
    {
      file_has_changed_dialogs = (Widget *)calloc(1, sizeof(Widget));
      file_has_changed_sounds = (snd_info **)calloc(1, sizeof(snd_info *));
    }
  else
    {
      file_has_changed_dialogs = (Widget *)realloc(file_has_changed_dialogs, (num_file_has_changed_dialogs + 1) * sizeof(Widget));
      file_has_changed_sounds = (snd_info **)realloc(file_has_changed_sounds, (num_file_has_changed_dialogs + 1) * sizeof(snd_info *));
    }

  file_has_changed_dialogs[num_file_has_changed_dialogs] = d;
  file_has_changed_sounds[num_file_has_changed_dialogs] = sp;
  num_file_has_changed_dialogs++;
}


void unpost_file_has_changed_if_any(snd_info *sp)
{
  int i;
  for (i = 0; i < num_file_has_changed_dialogs; i++)
    if (((file_has_changed_sounds[i] == sp) ||
	 (!snd_ok(file_has_changed_sounds[i]))) &&
	(XtIsManaged(file_has_changed_dialogs[i])))
      XtUnmanageChild(file_has_changed_dialogs[i]);
}


static void file_has_changed_no_callback(Widget w, XtPointer context, XtPointer info)
{
}

static void file_has_changed_yes_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_info *sp = (snd_info *)context;
  save_edits_without_asking(sp);
  sp->need_update = false;
  stop_bomb(sp);                  /* in case Snd already noticed the problem */
  clear_status_area(sp);
}

static void file_has_changed_help_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_help("save edits?", 
	   "The file has changed unexpectedly, so in most cases, saving the current edits can't do anything useful.  But you can try anyway!",
	   WITH_WORD_WRAP);
}

void changed_file_dialog(snd_info *sp)
{
  char *question;
  XmString q;
  Widget dialog;

  question = mus_format("%s has changed!  Save edits anyway?", sp->short_filename);
  q = XmStringCreateLocalized(question);

  dialog = file_has_changed_dialog(sp);
  if (!dialog)
    {
      Arg args[20];
      int n;
      XmString yes, no;

      yes = XmStringCreateLocalized((char *)"yes");
      no = XmStringCreateLocalized((char *)"no");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNokLabelString, yes); n++;
      XtSetArg(args[n], XmNcancelLabelString, no); n++;
      XtSetArg(args[n], XmNmessageString, q); n++;
      dialog = XmCreateQuestionDialog(MAIN_PANE(ss), sp->filename, args, n);
      save_file_has_changed_dialog(dialog, sp);

      XtAddCallback(dialog, XmNhelpCallback,   file_has_changed_help_callback, (XtPointer)sp);
      XtAddCallback(dialog, XmNokCallback,     file_has_changed_yes_callback,  (XtPointer)sp);
      XtAddCallback(dialog, XmNcancelCallback, file_has_changed_no_callback,   (XtPointer)sp);

      XmStringFree(yes);
      XmStringFree(no);

      map_over_children(dialog, set_main_color_of_widget);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->highlight_color, NULL);
    }
  else
    {
      XtVaSetValues(dialog, XmNmessageString, q, NULL);
    }

  XmStringFree(q);
  free(question);
  XtManageChild(dialog);
}



/* ---------------- view files dialog ---------------- */


/* -------- view files shared code -------- */

static void view_files_clear_selected_files(view_files_info *vdat);
static int view_files_add_selected_file(view_files_info *vdat, vf_row *r);
static int view_files_find_row(view_files_info *vdat, const char *name);
static int view_files_info_size = 0;
static view_files_info **view_files_infos = NULL;

static XEN vf_open_file_watcher(XEN hook_or_reason)
{
  int k;
  /* reasons are FILE_OPENED|CLOSED, but it's not worth the trouble of splitting them out here */
  
  /* loop through all vf dialogs ... */
  for (k = 0; k < view_files_info_size; k++)
    if ((view_files_infos[k]) &&
	(view_files_infos[k]->dialog) &&
	(widget_is_active(view_files_infos[k]->dialog)))
      {
	view_files_info *vdat;
	vdat = view_files_infos[k];
	vf_mix_insert_buttons_set_sensitive(vdat, 
					    ((vdat->currently_selected_files > 0) &&
					     (any_selected_sound())));
      }
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
  XEN_NARGIFY_1(vf_open_file_watcher_w, vf_open_file_watcher)
#else
  #define vf_open_file_watcher_w vf_open_file_watcher
#endif


int view_files_dialog_list_length(void)
{
  int i, n = 0;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog))
      n++;
  return(n);
}


char **view_files_dialog_titles(void)
{
  int n;
  n = view_files_dialog_list_length();
  if (n > 0)
    {
      char **titles;
      int i, j = 0;
      titles = (char **)calloc(n + 1, sizeof(char *));
      for (i = 0; i < view_files_info_size; i++)
	if ((view_files_infos[i]) &&
	    (view_files_infos[i]->dialog))
	  titles[j++] = dialog_get_title(view_files_infos[i]->dialog);
      return(titles);
    }
  return(NULL);
}


void view_files_start_dialog_with_title(const char *title)
{
  int i;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog))
      {
	char *dialog_title = NULL;
	dialog_title = dialog_get_title(view_files_infos[i]->dialog);
	if (mus_strcmp(title, dialog_title)) /* this includes NULL == NULL */
	  {
	    if (dialog_title) free(dialog_title);
	    make_view_files_dialog_1(view_files_infos[i], true);
	    return;
	  }
	if (dialog_title) free(dialog_title);
      }
}


static view_files_info *new_view_files_dialog(void)
{
  /* always returns a new (empty) file viewer -- changed 8-Jan-08 */
  int loc = -1;
  view_files_info *vdat;

  if (view_files_info_size == 0)
    {
      loc = 0;
      view_files_info_size = 4;
      view_files_infos = (view_files_info **)calloc(view_files_info_size, sizeof(view_files_info *));
    }
  else
    {
      int i;
      for (i = 0; i < view_files_info_size; i++)
	if (!view_files_infos[i])
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = view_files_info_size;
	  view_files_info_size += 4;
	  view_files_infos = (view_files_info **)realloc(view_files_infos, view_files_info_size * sizeof(view_files_info *));
	  for (i = loc; i < view_files_info_size; i++) view_files_infos[i] = NULL;
	}
    }

  view_files_infos[loc] = (view_files_info *)calloc(1, sizeof(view_files_info));
  vdat = view_files_infos[loc];
  vdat->index = loc;
  vdat->dialog = NULL_WIDGET;
  vdat->file_list = NULL_WIDGET;
  vdat->file_list_holder = NULL_WIDGET;
  vdat->file_list_entries = NULL;
  vdat->size = 0;
  vdat->end = -1;
  vdat->names = NULL;
  vdat->full_names = NULL;
  vdat->selected_files = NULL;
  vdat->selected_files_size = 0;
  vdat->location_choice = VF_AT_CURSOR;
  vdat->error_p = false;
  vdat->need_update = false;
  vdat->dirs_size = 0;
  vdat->dirs = NULL;
  vdat->dir_names = NULL;
  vdat->amp = 1.0;
  vdat->speed = 1.0;
  vdat->amp_env = default_env(1.0, 1.0);
  vdat->sort_items_size = 0;
  vdat->sort_items = NULL;
  vdat->speed_style = speed_control_style(ss);

  /* don't clear at this point! */
  view_files_infos[loc]->currently_selected_files = 0;
  view_files_infos[loc]->sorter = view_files_sort(ss);
  return(view_files_infos[loc]);
}


static int vf_dialog_to_index(widget_t dialog)
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


static view_files_info *vf_dialog_to_info(widget_t dialog)
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
      files = (char **)calloc(len, sizeof(char *));
      for (i = 0; i < len; i++) 
	files[i] = mus_strdup(vdat->full_names[vdat->selected_files[i]]);
    }
  return(files);
}


static char **view_files_selected_files(widget_t dialog, int *len)
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


static void view_files_run_select_hook(widget_t dialog, const char *selected_file);

static char **view_files_set_selected_files(widget_t dialog, char **files, int len)
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
      vf_mix_insert_buttons_set_sensitive(vdat, 
					  ((vdat->currently_selected_files > 0) &&
					   (any_selected_sound())));
    }
  return(files);
}


static char **view_files_files(widget_t dialog, int *len)
{
  /* don't free result! */
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


static void view_files_clear_list(view_files_info *vdat);

static char **view_files_set_files(widget_t dialog, char **files, int len)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      int i;
      view_files_clear_selected_files(vdat);
      view_files_clear_list(vdat);
      if (len > 0)
	{
	  for (i = 0; i < len; i++)
	    if (files[i])
	      view_files_add_file_or_directory(vdat, (const char *)(files[i]));
	}
      view_files_display_list(vdat);
    }
  return(files);
}


static void vf_mix_insert_buttons_set_sensitive(view_files_info *vdat, bool sensitive)
{
  if (vdat->mixB)
    {
      set_sensitive(vdat->mixB, sensitive);
      set_sensitive(vdat->insertB, sensitive);
    }
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
	    vf_unhighlight_row(r->nm, r->rw);
	}
    }
  vdat->currently_selected_files = 0;
  vf_mix_insert_buttons_set_sensitive(vdat, false);
}


static void view_files_unselect_file(view_files_info *vdat, vf_row *r)
{
  vf_unhighlight_row(r->nm, r->rw);
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
  if (vdat->currently_selected_files == 0)
    {
      vf_mix_insert_buttons_set_sensitive(vdat, false);
      vf_unpost_info(vdat);
    }
}


static int view_files_add_selected_file(view_files_info *vdat, vf_row *r)
{
  /* returns how many are now selected (counting new) */
  if (vdat->selected_files_size == 0)
    {
      vdat->selected_files_size = 4;
      vdat->selected_files = (int *)calloc(vdat->selected_files_size, sizeof(int));
      vdat->selected_files[0] = r->pos;
      vdat->currently_selected_files = 1;
    }
  else
    {
      if (vdat->currently_selected_files >= vdat->selected_files_size)
	{
	  vdat->selected_files_size += 4;
	  vdat->selected_files = (int *)realloc(vdat->selected_files, vdat->selected_files_size * sizeof(int));
	  vdat->selected_files[vdat->currently_selected_files++] = r->pos;
	}
      else 
	{
	  vdat->selected_files[vdat->currently_selected_files++] = r->pos;
	}
    }
  vf_highlight_row(r->nm, r->rw);
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
		vf_highlight_row(new_r->nm, new_r->rw);
		vf_unhighlight_row(old_r->nm, old_r->rw);
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
	  /* pass view files dialog settings to play */
	  play_sp->speed_control = vdat->speed;
	  play_sp->amp_control = vdat->amp;
	  play_sound(play_sp, 0, NO_END_SPECIFIED);
	}
      else return(true); /* can't find or setup file */
    }
  else
    { /* play toggled off */
      if ((play_sp) && (play_sp->playing)) 
	{
	  stop_playing_sound(play_sp, PLAY_BUTTON_UNSET);
	  vdat->current_play_button = NULL_WIDGET;
	}
    }
  return(false);
}


void view_files_unplay(void)
{
  int k;
  for (k = 0; k < view_files_info_size; k++)
    if ((view_files_infos[k]) &&
	(view_files_infos[k]->dialog) &&
	(widget_is_active(view_files_infos[k]->dialog)))
      {
	view_files_info *vdat;
	vdat = view_files_infos[k];
	if ((vdat->current_play_button) &&
	    (XmToggleButtonGetState(vdat->current_play_button) != XmUNSET))
	  {
	    set_toggle_button(vdat->current_play_button, false, true, (void *)vdat);
	    vdat->current_play_button = NULL_WIDGET;
	  }
      }
}


static void view_files_reflect_sort_items(void)
{
  int i;
  view_files_info *vdat;
  int j = 0, k;

  if (view_files_info_size == 0) return;
  for (i = 0; i < ss->file_sorters_size; i++)
    {
      XEN ref;
      ref = XEN_VECTOR_REF(ss->file_sorters, i);
      if (XEN_PAIR_P(ref))
	{
	  XmString s1;
	  s1 = XmStringCreateLocalized((char *)XEN_TO_C_STRING(XEN_CAR(ref)));
	  for (k = 0; k < view_files_info_size; k++)
	    if ((view_files_infos[k]) &&
		(view_files_infos[k]->dialog))
	      {
		vdat = view_files_infos[k];
		if (j >= vdat->sort_items_size)
		  {
		    int n = 0, k, old_size;
		    Arg args[20];
		    old_size = vdat->sort_items_size;
		    XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
		    vdat->sort_items_size += 4;
		    vdat->sort_items = (Widget *)realloc(vdat->sort_items, vdat->sort_items_size * sizeof(Widget));
		    for (k = old_size; k < vdat->sort_items_size; k++)
		      vdat->sort_items[k] = XtCreateWidget("unused", xmPushButtonWidgetClass, vdat->smenu, args, n);
		  }
		XtVaSetValues(vdat->sort_items[j], 
			      XmNlabelString, s1,
			      XmNuserData, i + SORT_XEN, /* this is an index into the file_sorters list, not the widget list */
			      NULL);
		XtManageChild(vdat->sort_items[j]);
	      }
	  j++;
	  XmStringFree(s1);
	}
    }
}


/* (add-file-sorter "duration" 
		(lambda (lst)
		  (sort lst 
			(lambda (a b)
			  (> (mus-sound-duration a) (mus-sound-duration b))))))

 */


static void vf_add_file(view_files_info *vdat, const char *filename, const char *fullname)
{
  vdat->end++;
  if (vdat->end >= vdat->size)
    {
      int new_size;
      new_size = vdat->size + 32;
      if (vdat->size == 0)
	{
	  vdat->names = (char **)calloc(new_size, sizeof(char *));
	  vdat->full_names = (char **)calloc(new_size, sizeof(char *));
	}
      else
	{
	  int i;
	  vdat->names = (char **)realloc(vdat->names, new_size * sizeof(char *));
	  vdat->full_names = (char **)realloc(vdat->full_names, new_size * sizeof(char *));
	  for (i = vdat->size; i < new_size; i++) 
	    {
	      vdat->names[i] = NULL; 
	      vdat->full_names[i] = NULL; 
	    }
	}
      if (vdat->file_list_entries == NULL)
	vdat->file_list_entries = (vf_row **)calloc(new_size, sizeof(vf_row *));
      else 
	{
	  int i;
	  vdat->file_list_entries = (vf_row **)realloc(vdat->file_list_entries, new_size * sizeof(vf_row *));
	  for (i = vdat->size; i < new_size; i++) vdat->file_list_entries[i] = NULL;
	}
      vdat->size = new_size;
    }
  vdat->names[vdat->end] = mus_strdup(filename);
  vdat->full_names[vdat->end] = mus_strdup(fullname);
}


static void add_file_to_view_files_list(view_files_info *vdat, const char *filename, const char *fullname)
{
  int row;

  row = view_files_find_row(vdat, filename);
  if (row != -1)
    {
      if ((vdat->dialog) &&
	  (widget_is_active(vdat->dialog)) &&
	  (vdat->file_list_entries[row]))
	{
	  ensure_scrolled_window_row_visible(vdat->file_list, row, vdat->end + 1);
	  vf_flash_row(vdat->file_list_entries[row]);
	}
      return;
    }

  errno = 0;
  if (!(mus_file_probe(fullname)))
    {
      char *msg;
      if ((vdat->dialog) &&
	  (widget_is_active(vdat->dialog)))
	{
	  if (errno != 0)
	    msg = mus_format("%s: %s", filename, strerror(errno));
	  else msg = mus_format("%s does not exist", filename);
	  vf_post_add_error(msg, vdat);
	  free(msg);
	}
      return;
    }

  vf_add_file(vdat, filename, fullname);
}



/* what about temps coming and going -- should we just add a need-update switch for later remanage? */
/*   remanagement only through make_view_files_dialog -- this file */
/*   perhaps ss->making|deleting_temp_file -> ignore this fam event? */

static void add_directory_to_view_files_list(view_files_info *vdat, const char *dirname)
{
  /* I think all directory additions come through here */
  dir_info *sound_files = NULL;

  if ((dirname) && (dirname[strlen(dirname) - 1] != '/'))
    {
      char *add_slash;
      add_slash = mus_format("%s/", dirname);
      add_directory_to_view_files_list(vdat, add_slash);
      free(add_slash);
    }
  else
    {
#if (!USE_NO_GUI)
      view_files_monitor_directory(vdat, dirname);
#endif
      sound_files = find_sound_files_in_dir(dirname);
      if ((sound_files) && 
	  (sound_files->len > 0))
	{
	  int i, dirs = 0, len = 16;
	  for (i = 0; i < sound_files->len; i++) 
	    add_file_to_view_files_list(vdat, sound_files->files[i]->filename, sound_files->files[i]->full_filename);
	  sound_files = free_dir_info(sound_files);

	  /* fixup title */
	  for (i = 0; i < vdat->dirs_size; i++)
	    if (vdat->dir_names[i])
	      {
		dirs++;
		len += mus_strlen(just_filename(vdat->dir_names[i]));
	      }
	  if ((dirs < 4) &&
	      (len < 512))
	    {
	      int cur_dir = 0;
	      char *titlestr = NULL, *dirstr;
	      titlestr = (char *)calloc(len + dirs * 8, sizeof(char));
	      strcat(titlestr, "Files: ");
	      for (i = 0; i < vdat->dirs_size; i++)
		if (vdat->dir_names[i])
		  {
		    dirstr = just_filename(vdat->dir_names[i]);
		    strcat(titlestr, dirstr);
		    free(dirstr);
		    cur_dir++;
		    if (cur_dir < dirs)
		      strcat(titlestr, ", ");
		  }
	      dialog_set_title(vdat->dialog, titlestr);
	      free(titlestr);
	    }
	}
    }
}


static void view_files_sort_list(view_files_info *vdat)
{
  if (vdat->end >= 0)
    {
      sort_info **data;
      int i, len;

      len = vdat->end + 1;
      data = (sort_info **)calloc(len, sizeof(sort_info *));

      for (i = 0; i < len; i++)
	{
	  data[i] = (sort_info *)calloc(1, sizeof(sort_info));
	  data[i]->filename = vdat->names[i];
	  data[i]->full_filename = vdat->full_names[i];
	}

      snd_sort(vdat->sorter, data, len);

      for (i = 0; i < len; i++)
	{
	  vdat->names[i] = data[i]->filename;
	  vdat->full_names[i] = data[i]->full_filename;
	  free(data[i]);
	}
      free(data);
    }
}


static void view_files_display_list(view_files_info *vdat)
{
  int i;
  widget_t last_row = NULL_WIDGET; /* ignored in gtk version */
  vf_row *r;

  if (!vdat) return;
  if (!(vdat->dialog)) return;

  if (vdat->end >= 0)
    {
      int i, old_len;
      char **old_names = NULL;

      old_len = vdat->currently_selected_files;
      if (old_len > 0)
	old_names = vf_selected_files(vdat);

      view_files_sort_list(vdat);
      for (i = 0; i <= vdat->end; i++)
	{
	  r = vdat->file_list_entries[i];
	  if (!r)
	    {
	      r = view_files_make_row(vdat, last_row);
	      vdat->file_list_entries[i] = r;
	      r->pos = i;
	    }
	  set_button_label(r->nm, vdat->names[r->pos]);
#if WITH_AUDIO
	  set_toggle_button(r->pl, false, false, (void *)vdat);
#endif
	  if (!(widget_is_active(r->rw))) activate_widget(r->rw);
	  last_row = r->rw;
	}

      if (old_names)
	{
	  vf_fixup_selected_files(vdat, old_names, old_len);
	  for (i = 0; i < old_len; i++) free(old_names[i]);
	  free(old_names);
	}
    }

  for (i = vdat->end + 1; i < vdat->size; i++)
    {
      r = vdat->file_list_entries[i];
      if (r)
	{
	  if (widget_is_active(r->rw)) 
	    deactivate_widget(r->rw);
	}
    }

  if (!(widget_is_active(vdat->file_list))) 
    activate_widget(vdat->file_list);
}


static void view_files_clear_list(view_files_info *vdat)
{
  int i;
#if (!USE_NO_GUI)
  view_files_unmonitor_directories(vdat);
#endif
  if (vdat->names)
    {
      for (i = 0; i < vdat->size; i++)
	if (vdat->names[i]) 
	  {
	    free(vdat->names[i]); 
	    vdat->names[i] = NULL;
	    free(vdat->full_names[i]); 
	    vdat->full_names[i] = NULL;
	  }
      vdat->end = -1;
      vdat->currently_selected_files = 0;
    }
}

#if 0
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
		free(vdat->names[i]); 
		vdat->names[i] = NULL;
		free(vdat->full_names[i]); 
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
	      }
	    j++;
	  }
      vdat->end = j - 1;
    }

  if (old_names)
    {
      vf_fixup_selected_files(vdat, old_names, old_len);
      for (i = 0; i < old_len; i++) free(old_names[i]);
      free(old_names);
    }
}
#endif


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


static int vf_mix(view_files_info *vdat)
{
  int len, id_or_error = 0;
  snd_info *sp;

  sp = any_selected_sound();
  len = vdat->currently_selected_files;

  if ((len == 1) &&
      (snd_feq(vdat->amp, 1.0)) &&
      (snd_feq(vdat->speed, 1.0)) &&
      (default_env_p(vdat->amp_env)))
    id_or_error = mix_complete_file(sp, vdat->beg, 
				    vdat->full_names[vdat->selected_files[0]], 
				    with_mix_tags(ss), DONT_DELETE_ME, MIX_SETS_SYNC_LOCALLY, NULL);
  else
    {
      int i;
      bool err = false;
      char *tempfile;
      char **selected_files;

      selected_files = vf_selected_files(vdat);
      tempfile = scale_and_src(selected_files, len, sp->nchans, vdat->amp, vdat->speed, vdat->amp_env, &err);

      if (err)
	{
	  vf_post_error(tempfile, vdat);
	  id_or_error = MIX_FILE_NO_TEMP_FILE;
	}
      else
	{ 
	  if (sp->nchans > 1)
	    remember_temp(tempfile, sp->nchans);
	  id_or_error = mix_complete_file(sp, vdat->beg, tempfile,
					  with_mix_tags(ss), 
					  (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
					  MIX_SETS_SYNC_LOCALLY, NULL);
	}
      free(tempfile);
      for (i = 0; i < len; i++)
	free(selected_files[i]);
      free(selected_files);
    }
  return(id_or_error);
}


static void view_files_mix_selected_files(widget_t w, view_files_info *vdat)
{
  vdat->error_p = false;
  redirect_snd_error_to(redirect_vf_post_location_error, (void *)vdat);
  vdat->beg = vf_location(vdat);
  redirect_snd_error_to(NULL, NULL);

  if (!(vdat->error_p))
    {
      int id_or_error = 0;

      redirect_snd_error_to(redirect_vf_post_error, (void *)vdat);
      ss->requestor_dialog = w;
      ss->open_requestor_data = (void *)vdat;
      ss->open_requestor = FROM_VIEW_FILES_MIX_DIALOG;
      id_or_error = vf_mix(vdat);

      /* "id_or_error" here is either one of the mix id's or an error indication such as MIX_FILE_NO_MIX */
      /*    the possible error conditions have been checked already, or go through snd_error */

      redirect_snd_error_to(NULL, NULL);
      if (id_or_error >= 0)
	{
	  char *msg;
	  if (vdat->currently_selected_files == 1)
	    msg = mus_format("%s mixed in at %lld", vdat->names[vdat->selected_files[0]], vdat->beg);
	  else msg = mus_format("selected files mixed in at %lld", vdat->beg);
	  vf_post_error(msg, vdat);
	  vdat->error_p = false;
	  free(msg);
	}
    }
}


static bool vf_insert(view_files_info *vdat)
{
  int len;
  bool ok = false;
  snd_info *sp;
  sp = any_selected_sound();

  len = vdat->currently_selected_files;
  if ((len == 1) &&
      (snd_feq(vdat->amp, 1.0)) &&
      (snd_feq(vdat->speed, 1.0)) &&
      (default_env_p(vdat->amp_env)))
    ok = insert_complete_file(sp, 
			      vdat->full_names[vdat->selected_files[0]], 
			      vdat->beg,
			      DONT_DELETE_ME);
  else
    {
      int i;
      bool err = false;
      char *tempfile;
      char **selected_files;

      selected_files = vf_selected_files(vdat);
      tempfile = scale_and_src(selected_files, len, sp->nchans, vdat->amp, vdat->speed, vdat->amp_env, &err);

      if (err)
	{
	  vf_post_error(tempfile, vdat);
	  ok = false;
	}
      else
	{
	  vf_clear_error(vdat);
	  if (sp->nchans > 1)
	    remember_temp(tempfile, sp->nchans);
	  ok = insert_complete_file(sp, 
				    tempfile,
				    vdat->beg,
				    (sp->nchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME);
	}
      free(tempfile);
      for (i = 0; i < len; i++)
	free(selected_files[i]);
      free(selected_files);
    }
  return(ok);
}


static void view_files_insert_selected_files(widget_t w, view_files_info *vdat)
{
  vdat->error_p = false;
  redirect_snd_error_to(redirect_vf_post_location_error, (void *)vdat);
  vdat->beg = vf_location(vdat);
  redirect_snd_error_to(NULL, NULL);

  if (!(vdat->error_p))
    {
      bool ok = false;

      redirect_snd_error_to(redirect_vf_post_error, (void *)vdat);
      redirect_snd_warning_to(redirect_vf_post_error, (void *)vdat);
      ss->requestor_dialog = w;
      ss->open_requestor = FROM_VIEW_FILES_INSERT_DIALOG;
      ss->open_requestor_data = (void *)vdat;
      ok = vf_insert(vdat);
      redirect_snd_error_to(NULL, NULL);
      redirect_snd_warning_to(NULL, NULL);

      if (ok)
	{
	  char *msg;
	  if (vdat->currently_selected_files == 1)
	    msg = mus_format("%s inserted at %lld", vdat->names[vdat->selected_files[0]], vdat->beg);
	  else msg = mus_format("selected files inserted at %lld", vdat->beg);
	  vf_post_error(msg, vdat);
	  vdat->error_p = false;
	  free(msg);
	}
      /* else we've already posted whatever went wrong (make_file_info etc) */
    }
}


static mus_float_t view_files_amp(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->amp);
  return(0.0);
}


static mus_float_t view_files_set_amp(widget_t dialog, mus_float_t new_amp)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    vf_set_amp(vdat, new_amp);
  return(new_amp);
}


static mus_float_t view_files_speed(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->speed);
  return(1.0);
}


static mus_float_t view_files_set_speed(widget_t dialog, mus_float_t new_speed)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    vf_set_speed(vdat, new_speed);
  return(new_speed);
}


static speed_style_t view_files_speed_style(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->speed_style);
  return(SPEED_CONTROL_AS_FLOAT);
}


static speed_style_t view_files_set_speed_style(widget_t dialog, speed_style_t speed_style)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      vdat->speed_style = speed_style;
      vf_set_speed(vdat, vdat->speed); /* update label etc */
    }
  return(speed_style);
}


static env *view_files_amp_env(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->amp_env);
  return(NULL);
}


static void view_files_set_amp_env(widget_t dialog, env *new_e)
{
  vf_set_amp_env(vf_dialog_to_info(dialog), new_e);
}


static int view_files_local_sort(widget_t dialog)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    return(vdat->sorter);
  return(-1);
}


static int view_files_set_local_sort(widget_t dialog, int sort_choice)
{
  view_files_info *vdat;
  vdat = vf_dialog_to_info(dialog);
  if (vdat)
    {
      vdat->sorter = sort_choice;
      view_files_display_list(vdat);
      vf_reflect_sort_choice_in_menu(vdat);
    }
  return(sort_choice);
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


widget_t make_view_files_dialog(bool managed, bool make_new)
{
  int i;
  view_files_info *vdat = NULL;

  if (make_new)
    return(make_view_files_dialog_1(new_view_files_dialog(), managed));

  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog))
      {
	vdat = view_files_infos[i];
	if (widget_is_active(vdat->dialog))
	  break;
      }

  if (vdat)
    return(make_view_files_dialog_1(vdat, managed));
  return(make_view_files_dialog_1(new_view_files_dialog(), managed));
}


void save_view_files_dialogs(FILE *fd) 
{
#if HAVE_EXTENSION_LANGUAGE
  int i;
  view_files_info *vdat;
  for (i = 0; i < view_files_info_size; i++)
    if ((view_files_infos[i]) &&
	(view_files_infos[i]->dialog) &&
	(widget_is_active(view_files_infos[i]->dialog)))
      {
	int k;
	vdat = view_files_infos[i];

#if HAVE_SCHEME
	fprintf(fd, "(let ((vf (" S_view_files_dialog " #t #t)))\n");

	if (vdat->full_names)
	  {
	    fprintf(fd, "  (set! (" S_view_files_files " vf) (list");
	    for (k = 0; k <= vdat->end; k++)
	      fprintf(fd, " \"%s\"", vdat->full_names[k]);
	    fprintf(fd, "))\n");
	    if (vdat->currently_selected_files > 0)
	      {
		fprintf(fd, "  (set! (" S_view_files_selected_files " vf) (list");
		for (k = 0; k < vdat->currently_selected_files; k++)
		  fprintf(fd, " \"%s\"", vdat->full_names[vdat->selected_files[k]]);
		fprintf(fd, "))\n");
	      }
	  }
	if (!(snd_feq(vdat->amp, 1.0)))
	  fprintf(fd, "  (set! (" S_view_files_amp " vf) %.3f)\n", vdat->amp);

	if (!(snd_feq(vdat->speed, 1.0)))
	  fprintf(fd, "  (set! (" S_view_files_speed " vf) %.3f)\n", vdat->speed);

	if (!(default_env_p(vdat->amp_env)))
	  fprintf(fd, "  (set! (" S_view_files_amp_env " vf) %s)\n", env_to_string(vdat->amp_env));

	/* assume file-sorters are set up already */
	fprintf(fd, "  (set! (" S_view_files_sort " vf) %d)\n", vdat->sorter);	    
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
	  fprintf(fd, "  set_view_files_amp(vf, %.3f)\n", vdat->amp);

	if (!(snd_feq(vdat->speed, 1.0)))
	  fprintf(fd, "  set_view_files_speed(vf, %.3f)\n", vdat->speed);

	if (!(default_env_p(vdat->amp_env)))
	  fprintf(fd, "  set_view_files_amp_env(vf, %s)\n", env_to_string(vdat->amp_env));

	/* assume file-sorters are set up already */
	fprintf(fd, "  set_view_files_sort(vf, %d)\n", vdat->sorter);	    
	fprintf(fd, "\n");
#endif

#if HAVE_FORTH
	fprintf(fd, "#t #t view-files-dialog value vf\n");

	if (vdat->full_names)
	  {
	    fprintf(fd, "  vf '(");
	    for (k = 0; k <= vdat->end; k++)
	      fprintf(fd, " \"%s\"", vdat->full_names[k]);
	    fprintf(fd, " ) set-view-files-files drop\n");
	    if (vdat->currently_selected_files > 0)
	      {
		fprintf(fd, "  vf '(");
		for (k = 0; k <= vdat->currently_selected_files; k++)
		  fprintf(fd, " \"%s\"", vdat->full_names[vdat->selected_files[k]]);
		fprintf(fd, " ) set-view-files-selected-files drop\n");
	      }
	  }
	if (!(snd_feq(vdat->amp, 1.0)))
	  fprintf(fd, "  vf %.3f set-view-files-amp drop\n", vdat->amp);

	if (!(snd_feq(vdat->speed, 1.0)))
	  fprintf(fd, "  vf %.3f set-view-files-speed drop\n", vdat->speed);

	if (!(default_env_p(vdat->amp_env)))
	  fprintf(fd, "  vf %s set-view-files-amp-env drop\n", env_to_string(vdat->amp_env));

	/* assume file-sorters are set up already */
	fprintf(fd, "  vf %d set-view-files-sort drop\n\n", vdat->sorter);
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
	  make_view_files_dialog_1(vdat, false);
	}
    }

  if (vdat)
    {
      full_filename = mus_expand_filename((const char *)dirname);
      if (!(mus_file_probe(full_filename)))
	{
	  char *msg;
	  if ((vdat->dialog) &&
	      (widget_is_active(vdat->dialog)))
	    {
	      if (errno != 0)
		msg = mus_format("%s: %s", full_filename, strerror(errno));
	      else msg = mus_format("%s does not exist", full_filename);
	      vf_post_add_error(msg, vdat);
	      free(msg);
	    }
	}
      else
	{
	  add_directory_to_view_files_list(vdat, full_filename);
	}
      free(full_filename);
    }
}


static void view_files_add_file(widget_t dialog, const char *filename)
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
	  make_view_files_dialog_1(vdat, false);
	}
    }

  if (vdat)
    {
      full_filename = mus_expand_filename((const char *)filename);
      add_file_to_view_files_list(vdat, filename, full_filename);
      free(full_filename);
    }
}


static void view_files_open_selected_files(view_files_info *vdat)
{
  snd_info *sp = NULL;
  ss->open_requestor = FROM_VIEW_FILES;

  if (vdat->currently_selected_files > 0)
    {
      int i;
      for (i = 0; i < vdat->currently_selected_files; i++)
	sp = snd_open_file(vdat->full_names[vdat->selected_files[i]], FILE_READ_WRITE);
      if (sp) select_channel(sp, 0); 
    }
}


char *view_files_find_any_directory(void)
{
  /* find any active directory in any vf dialog */
  if (view_files_info_size > 0)
    {
      int j;
      for (j = 0; j < view_files_info_size; j++)
	{
	  view_files_info *vdat;
	  vdat = view_files_infos[j];
	  if ((vdat) && 
	      (vdat->dir_names))
	    {
	      int i;
	      for (i = 0; i < vdat->dirs_size; i++)
		if (vdat->dir_names[i])
		  return(vdat->dir_names[i]);
	    }
	}
    }
  return(NULL);
}



static XEN mouse_enter_label_hook;
static XEN mouse_leave_label_hook;


static void view_files_unmonitor_directories(view_files_info *vdat) {}
static void view_files_monitor_directory(view_files_info *vdat, const char *dirname) {}


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
#if WITH_AUDIO
  XtCallbackList n1;
#endif
  XtCallbackList n3;

  s1 = XmStringCreateLocalized((char *)"");
  r = (vf_row *)calloc(1, sizeof(vf_row));
  r->vdat = (void *)vdat;

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, (last_row) ? XmATTACH_WIDGET : XmATTACH_FORM); n++;
  if (last_row) {XtSetArg(args[n], XmNtopWidget, last_row); n++;}
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNheight, 18); n++; 
  r->rw = XtCreateWidget("rw", xmFormWidgetClass, vdat->file_list_holder, args, n);

#if WITH_AUDIO
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNselectColor, ss->selection_color); n++;
  XtSetArg(args[n], XmNlabelString, s1); n++;
  XtSetArg(args[n], XmNvalueChangedCallback, n1 = make_callback_list(play_callback, (XtPointer)r)); n++;
  if (ss->toggle_size > 0) {XtSetArg(args[n], XmNindicatorSize, ss->toggle_size); n++;}
  XtSetArg(args[n], XmNmarginWidth, 8); n++;
  r->pl = make_togglebutton_widget("pl", r->rw, args, n);
#endif

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
#if WITH_AUDIO
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, r->pl); n++;
#else
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
#endif
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNhighlightThickness, 0); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
  XtSetArg(args[n], XmNfillOnArm, false); n++;
  XtSetArg(args[n], XmNrecomputeSize, false); n++;
  XtSetArg(args[n], XmNwidth, 500); n++;                /* this sets the max name length indirectly -- was 300 which truncates some long file names (29-Oct-07) */
  XtSetArg(args[n], XmNactivateCallback, n3 = make_callback_list(name_callback, (XtPointer)r)); n++;
  r->nm = XtCreateManagedWidget("nm", xmPushButtonWidgetClass, r->rw, args, n);
  XmStringFree(s1);

  XtAddEventHandler(r->nm, EnterWindowMask, false, vf_mouse_enter_label, (XtPointer)r);
  XtAddEventHandler(r->nm, LeaveWindowMask, false, vf_mouse_leave_label, (XtPointer)r);

#if WITH_AUDIO
  free(n1);
#endif
  free(n3);
  return(r);
}


static void vf_unhighlight_row(widget_t nm, widget_t rw)
{
  XtVaSetValues(rw, XmNbackground, ss->highlight_color, NULL);
  XtVaSetValues(nm, XmNbackground, ss->highlight_color, NULL);
}


static void vf_highlight_row(widget_t nm, widget_t rw)
{
  XtVaSetValues(rw, XmNbackground, ss->zoom_color, NULL);
  XtVaSetValues(nm, XmNbackground, ss->zoom_color, NULL);
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
  free(v);
}


static void vf_flash_row(vf_row *r)
{
  vf_flash_data *v;
  v = (vf_flash_data *)calloc(1, sizeof(vf_flash_data));
  v->r = r;
  XtVaGetValues(r->rw, XmNbackground, &(v->old_color), NULL);
  XtVaSetValues(r->rw, XmNbackground, ss->light_blue, NULL);
  XtVaSetValues(r->nm, XmNbackground, ss->light_blue, NULL);
  XtAppAddTimeOut(MAIN_APP(ss),
		  500,
		  (XtTimerCallbackProc)vf_unflash_row,
		  (void *)v);
}


static void vf_post_info(view_files_info *vdat, int pos)
{
  char *title;
  XmString s3;
  title = mus_format("%s:", vdat->names[pos]);
  s3 = XmStringCreateLocalized(title);
  XtVaSetValues(vdat->left_title,
		XmNlabelString, s3,
		NULL);
  XmStringFree(s3);
  free(title);
  post_sound_info(vdat->info1, vdat->info2, vdat->full_names[pos], false);
}


static void vf_post_selected_files_list(view_files_info *vdat)
{
  int len;
  char *msg1 = NULL, *msg2 = NULL, *title;
  XmString s1, s2, s3;
  len = vdat->currently_selected_files;

  title = mus_strdup("selected files:");
  s3 = XmStringCreateLocalized(title);
  XtVaSetValues(vdat->left_title,
		XmNlabelString, s3,
		NULL);
  XmStringFree(s3);
  free(title);

  if (len == 2)
    {
      msg1 = mus_strdup(vdat->names[vdat->selected_files[0]]);
      msg2 = mus_strdup(vdat->names[vdat->selected_files[1]]);
    }
  else
    {
      if (len == 3)
	{
	  msg1 = mus_format("%s, %s", vdat->names[vdat->selected_files[0]], vdat->names[vdat->selected_files[1]]);
	  msg2 = mus_strdup(vdat->names[vdat->selected_files[2]]);
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
  free(msg1);
  free(msg2);
}


static void vf_unpost_info(view_files_info *vdat)
{
  XmString s1, s2, s3;
  char *title;

  title = mus_strdup("(no files selected)");
  s3 = XmStringCreateLocalized(title);
  XtVaSetValues(vdat->left_title,
		XmNlabelString, s3,
		NULL);
  XmStringFree(s3);
  free(title);

  s1 = XmStringCreateLocalized((char *)"|");
  s2 = XmStringCreateLocalized((char *)"|");
  XtVaSetValues(vdat->info1, XmNlabelString, s1, NULL);
  XtVaSetValues(vdat->info2, XmNlabelString, s2, NULL);
  XmStringFree(s1);
  XmStringFree(s2);
}


static void view_files_select_callback(Widget w, XtPointer context, XtPointer info) 
{
  static oclock_t mouse_down_time = 0;
  XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)info;
  XButtonEvent *ev;
  ev = (XButtonEvent *)(cb->event);

  /* cb->click_count is always 1 so we can't detect a double-click that way
   *   ss->click_time has the (very short!) multiclick time
   */

  if (mouse_down_time != 0)
    {
      if ((ev->time - mouse_down_time) < ss->click_time) /* open file if double clicked */
	{
	  mouse_down_time = ev->time;
	  view_files_open_selected_files((view_files_info *)(((vf_row *)context)->vdat));
	  return;
	}
    }
  mouse_down_time = ev->time;
  view_files_select((vf_row *)context, ev->state & snd_ShiftMask);
}


static void view_files_play_callback(Widget w, XtPointer context, XtPointer info) 
{
#if WITH_AUDIO
  /* open and play -- close at end or when button off toggled */
  vf_row *r = (vf_row *)context;
  view_files_info *vdat;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  vdat = (view_files_info *)(r->vdat);
  if (view_files_play(vdat, r->pos, cb->set))
    XmToggleButtonSetState(w, false, false);
  else vdat->current_play_button = w;
#endif
}


static vf_row *view_files_make_row(view_files_info *vdat, widget_t last_row)
{
  return(make_vf_row(vdat, last_row, view_files_play_callback, view_files_select_callback));
}


static void view_files_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_dialog_help();
}


static void view_files_quit_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  XtUnmanageChild(vdat->dialog);
}


static void view_files_new_viewer_callback(Widget w, XtPointer context, XtPointer info) 
{
  view_files_info *vdat = (view_files_info *)context;
  if ((vdat) && 
      (vdat->dialog) &&
      (XtIsManaged(vdat->dialog)) &&
      (XmGetFocusWidget(vdat->dialog) == XmMessageBoxGetChild(vdat->dialog, XmDIALOG_OK_BUTTON)))
    {
      Position x = 0, y = 0;

      /* jog the current one over a bit -- otherwise the new one lands exactly on top of the old! */
      XtVaGetValues(vdat->dialog, XmNx, &x, XmNy, &y, NULL);
      XtVaSetValues(vdat->dialog, XmNx, x + 30, XmNy, y - 30, NULL);

      vdat = new_view_files_dialog();
      make_view_files_dialog_1(vdat, true);
    }
}


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
  pointer_or_int_t index;
  XtVaGetValues(w, XmNuserData, &index, NULL); /* index is location in list of file-sorters */
  sort_vf((view_files_info *)context, (int)index);
}


static void vf_reflect_sort_choice_in_menu(view_files_info *vdat)
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


static void view_files_add_file_or_directory(view_files_info *vdat, const char *file_or_dir)
{
  char *filename;
  filename = mus_expand_filename((const char *)file_or_dir);
  if (filename)
    {
      int len;
      len = strlen(filename);
      if (filename[len - 1] == '*')
	filename[len - 1] = 0;
      if (directory_p(filename))
	add_directory_to_view_files_list(vdat, (const char *)filename);
      else add_file_to_view_files_list(vdat, file_or_dir, filename);
      free(filename);
    }
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
      XmChangeColor(vdat->file_list, ss->selection_color);
      break;

    case DRAG_LEAVE:
      XmChangeColor(vdat->file_list, ss->basic_color);
      break;

    default:
      break;
    }
}


static mus_long_t vf_location(view_files_info *vdat)
{
  mus_long_t pos = 0;
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
	  pos = string_to_mus_long_t(str, 0, "sample"); 
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


static void vf_post_error(const char *error_msg, view_files_info *vdat)
{
  XmString msg;
  remove_all_pending_clear_callbacks(vdat);
  vdat->error_p = true;
  msg = XmStringCreateLocalized((char *)error_msg);
  XtVaSetValues(vdat->info1,
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);
  msg = XmStringCreateLocalized((char *)"");
  XtVaSetValues(vdat->info2,
		XmNlabelString, msg, 
		NULL);
  XmStringFree(msg);
}


static void redirect_vf_post_error(const char *error_msg, void *vdat)
{
  vf_post_error(error_msg, (view_files_info *)vdat);
}


static void redirect_vf_post_location_error(const char *error_msg, void *data)
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


static void vf_post_add_error(const char *error_msg, view_files_info *vdat)
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

static int vf_speed_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0);
  if (val >= maxval) return((int)(0.9 * SCROLLBAR_MAX));
  return(snd_round(0.9 * SCROLLBAR_MAX * ((log(val) - log(minval)) / (log(maxval) - log(minval)))));
}


static void vf_set_speed(view_files_info *vdat, mus_float_t val)
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

static mus_float_t vf_scroll_to_amp(int val)
{
  if (val <= 0) 
    return(amp_control_min(ss));
  if (val >= (0.9 * SCROLLBAR_MAX)) 
    return(amp_control_max(ss));
  if (val > (0.5 * 0.9 * SCROLLBAR_MAX))
    return((((val / (0.5 * 0.9 * SCROLLBAR_MAX)) - 1.0) * (amp_control_max(ss) - 1.0)) + 1.0);
  else return((val * (1.0 - amp_control_min(ss)) / (0.5 * 0.9 * SCROLLBAR_MAX)) + amp_control_min(ss));
}


static int vf_amp_to_scroll(mus_float_t amp)
{
  return(amp_to_scroll(amp_control_min(ss), amp, amp_control_max(ss)));
}


static void vf_set_amp(view_files_info *vdat, mus_float_t val)
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
      vdat->env_ax = (graphics_context *)calloc(1, sizeof(graphics_context));
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
  env_editor_display_env(vdat->spf, vdat->amp_env, vdat->env_ax, "amp env", 
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


#ifdef __APPLE__
static int press_x, press_y;
#endif

static void vf_drawer_button_motion(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  view_files_info *vdat = (view_files_info *)context;
  XMotionEvent *ev = (XMotionEvent *)event;
  /* mus_float_t pos; */

#ifdef __APPLE__
  if ((press_x == ev->x) && (press_y == ev->y)) return;
#endif

  /* pos = (mus_float_t)(ev->x) / (mus_float_t)widget_width(w); */
  env_editor_button_motion(vdat->spf, ev->x, ev->y, ev->time, vdat->amp_env);
  vf_amp_env_resize(w, context, NULL);
}


static void vf_drawer_button_press(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  view_files_info *vdat = (view_files_info *)context;
  XButtonEvent *ev = (XButtonEvent *)event;
  /* mus_float_t pos; */

#ifdef __APPLE__
  press_x = ev->x;
  press_y = ev->y;
#endif

  /* pos = (mus_float_t)(ev->x) / (mus_float_t)widget_width(w); */
  if (env_editor_button_press(vdat->spf, ev->x, ev->y, ev->time, vdat->amp_env))
    vf_amp_env_resize(w, context, NULL);
}


static void vf_drawer_button_release(Widget w, XtPointer context, XEvent *event, Boolean *cont) 
{
  view_files_info *vdat = (view_files_info *)context;
  /* XButtonEvent *ev = (XButtonEvent *)event; */
  /* mus_float_t pos; */

  /* pos = (mus_float_t)(ev->x) / (mus_float_t)widget_width(w); */
  env_editor_button_release(vdat->spf, vdat->amp_env);
  vf_amp_env_resize(w, context, NULL);
}


static void vf_set_amp_env(view_files_info *vdat, env *new_e)
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
  XtVaSetValues(w, XmNbackground, ss->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}


static void blue_mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  XtVaSetValues(w, XmNbackground, ss->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}


static void white_mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  XtVaSetValues(w, XmNbackground, ss->text_focus_color, NULL);
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



static widget_t make_view_files_dialog_1(view_files_info *vdat, bool managed)
{
  if (!(vdat->dialog))
    {
      int i, n;
      Arg args[20];
      XmString go_away, xhelp, titlestr, new_viewer_str, s1, bstr;
      Widget mainform, viewform, leftform, reset_button, new_viewer_button;
      Widget left_title_sep, add_label, sep1, sep3, sep4, sep6, sep7;
#if WITH_AUDIO
      Widget plw;
#endif
      Widget rlw, sbar;
      XtCallbackList n1, n2, n3, n4;
      Widget amp_label, speed_label, env_frame;
      Widget bframe, bform;

      go_away = XmStringCreateLocalized((char *)I_GO_AWAY);
      xhelp = XmStringCreateLocalized((char *)I_HELP);
      new_viewer_str = XmStringCreateLocalized((char *)"New Viewer");

      {
	char *filestr = NULL;
	filestr = mus_format("%s %d", "Files", vdat->index + 1);
	titlestr = XmStringCreateLocalized(filestr);
	free(filestr);
      }

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNokLabelString, new_viewer_str); n++;
      XtSetArg(args[n], XmNcancelLabelString, go_away); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, titlestr); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      vdat->dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"Files", args, n);
      new_viewer_button = MSG_BOX(vdat->dialog, XmDIALOG_OK_BUTTON);

      XtAddCallback(vdat->dialog, XmNhelpCallback,   view_files_help_callback,       (XtPointer)vdat);
      /* XtAddCallback(vdat->dialog, XmNokCallback,     view_files_new_viewer_callback,    (XtPointer)vdat); */
      XtAddCallback(new_viewer_button, XmNactivateCallback, view_files_new_viewer_callback, (XtPointer)vdat);
      XtAddCallback(vdat->dialog, XmNcancelCallback, view_files_quit_callback, (XtPointer)vdat);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->selection_color); n++;
      reset_button = XtCreateManagedWidget("Reset", xmPushButtonGadgetClass, vdat->dialog, args, n);
      XtAddCallback(reset_button, XmNactivateCallback, view_files_reset_callback, (XtPointer)vdat);

      XmStringFree(xhelp);
      XmStringFree(go_away);
      XmStringFree(titlestr);
      XmStringFree(new_viewer_str);

      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->selection_color, NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->selection_color,  NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->highlight_color,   NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->highlight_color,   NULL);
      XtVaSetValues(MSG_BOX(vdat->dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color,  NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, MSG_BOX(vdat->dialog, XmDIALOG_SEPARATOR)); n++;
      XtSetArg(args[n], XmNsashIndent, 2); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNspacing, 24); n++;
      XtSetArg(args[n], XmNpaneMaximum, LOTSA_PIXELS); n++; 
      mainform = XtCreateManagedWidget("formd", xmPanedWindowWidgetClass, vdat->dialog, args, n);

      /* -------- left side controls -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      leftform = XtCreateManagedWidget("leftform", xmFormWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      vdat->left_title = XtCreateManagedWidget("(no files selected)", xmLabelWidgetClass, leftform, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->white); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->left_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmDOUBLE_LINE); n++;
      left_title_sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, leftform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, left_title_sep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      vdat->info1 = XtCreateManagedWidget("|", xmLabelWidgetClass, leftform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, vdat->info1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      vdat->info2 = XtCreateManagedWidget("|", xmLabelWidgetClass, leftform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->zoom_color); n++;
      XtSetArg(args[n], XmNborderColor, ss->zoom_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep6); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 2); n++;
      bframe = XtCreateManagedWidget("bframe", xmFrameWidgetClass, leftform, args, n);      

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->position_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      bform = XtCreateManagedWidget("bform", xmFormWidgetClass, bframe, args, n);      

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 50); n++;
      vdat->mixB = XtCreateManagedWidget("Mix", xmPushButtonWidgetClass, bform, args, n);
      XtAddCallback(vdat->mixB, XmNactivateCallback, view_files_mix_selected_callback, (XtPointer)vdat);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNmarginTop, 0); n++;
      XtSetArg(args[n], XmNmarginBottom, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 1); n++;
      XtSetArg(args[n], XmNhighlightThickness, 1); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, vdat->mixB); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      vdat->insertB = XtCreateManagedWidget("Insert", xmPushButtonWidgetClass, bform, args, n);
      XtAddCallback(vdat->insertB, XmNactivateCallback, view_files_insert_selected_callback, (XtPointer)vdat);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"at cursor");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"at end");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"at beginning");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"at sample");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"at mark");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      s1 = XmStringCreateLocalized((char *)"amp:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      s1 = XmStringCreateLocalized((char *)"1.0 ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->position_color); n++;
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
      s1 = XmStringCreateLocalized((char *)"speed:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->position_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNheight, 100); n++;
      vdat->env_drawer = XtCreateManagedWidget("amp-env-window", xmDrawingAreaWidgetClass, env_frame, args, n);

      /* right side */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      viewform = XtCreateManagedWidget("viewform", xmFormWidgetClass, mainform, args, n);

      /* Add dir/file text entry at bottom */
      n = 0;
      s1 = XmStringCreateLocalized((char *)"add:");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
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
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, add_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      vdat->add_text = make_textfield_widget("add-text", viewform, args, n, ACTIVATABLE, add_completer_func(filename_completer, NULL));
      XtAddCallback(vdat->add_text, XmNactivateCallback, view_files_add_files, (XtPointer)vdat);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, vdat->add_text); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmNO_LINE); n++;
      XtSetArg(args[n], XmNheight, 4); n++;
      sep3 = XtCreateManagedWidget("sep3", xmSeparatorWidgetClass, viewform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;	
      rlw = XtCreateManagedWidget("files", xmLabelWidgetClass, viewform, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->white); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, rlw); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNseparatorType, XmDOUBLE_LINE); n++;
      sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass, viewform, args, n);

#if WITH_AUDIO
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 5); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      plw = XtCreateManagedWidget("play", xmLabelWidgetClass, viewform, args, n);
#endif

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 0); n++;
      sbar = XmCreateMenuBar(viewform, (char *)"menuBar", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      vdat->smenu = XmCreatePulldownMenu(sbar, (char *)"sort-menu", args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNsubMenuId, vdat->smenu); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      XtSetArg(args[n], XmNmarginHeight, 1); n++;
      XtCreateManagedWidget("sort", xmCascadeButtonWidgetClass, sbar, args, n);
      
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      vdat->a_to_z =        XtCreateManagedWidget("a..z",       xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->z_to_a =        XtCreateManagedWidget("z..a",       xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->new_to_old =    XtCreateManagedWidget("new..old",   xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->old_to_new =    XtCreateManagedWidget("old..new",   xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->small_to_big =  XtCreateManagedWidget("small..big", xmPushButtonWidgetClass, vdat->smenu, args, n);
      vdat->big_to_small =  XtCreateManagedWidget("big..small", xmPushButtonWidgetClass, vdat->smenu, args, n);

      vdat->sort_items_size = 4;
      vdat->sort_items = (Widget *)calloc(vdat->sort_items_size, sizeof(Widget));
      for (i = 0; i < vdat->sort_items_size; i++)
	vdat->sort_items[i] = XtCreateWidget("unused", xmPushButtonWidgetClass, vdat->smenu, args, n);

      XtManageChild(sbar);

      n = 0;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNleftPosition, 5); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
#if WITH_AUDIO
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, plw); n++;
#else
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sep1); n++;
#endif
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, sep3); n++;
      XtSetArg(args[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
      XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
      vdat->file_list = XmCreateScrolledWindow(viewform, (char *)"file_list", args, n);

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

      free(n1);
      free(n2);
      free(n3);
      free(n4);

      vf_mix_insert_buttons_set_sensitive(vdat, false);
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
    }
  return(vdat->dialog);
}


/* -------- view-files variables -------- */

static XEN g_view_files_dialog(XEN managed, XEN make_new)
{
  #define H_view_files_dialog "(" S_view_files_dialog " :optional managed create-new-dialog): start the View Files dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ARG_1, S_view_files_dialog, "a boolean");
  return(XEN_WRAP_WIDGET(make_view_files_dialog(XEN_TO_C_BOOLEAN(managed), XEN_TRUE_P(make_new))));
}


static XEN g_add_directory_to_view_files_list(XEN directory, XEN dialog) 
{
  #define H_add_directory_to_view_files_list "(" S_add_directory_to_view_files_list " dir :optional w): adds any sound files in 'dir' to the View:Files dialog"
  
  XEN_ASSERT_TYPE(XEN_STRING_P(directory), directory, XEN_ARG_1, S_add_directory_to_view_files_list, "a string");
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog) || XEN_NOT_BOUND_P(dialog), dialog, XEN_ARG_2, S_add_directory_to_view_files_list, "a view-files dialog widget"); 

  if (XEN_NOT_BOUND_P(dialog))
    view_files_add_directory(NULL_WIDGET, XEN_TO_C_STRING(directory));
  else view_files_add_directory((widget_t)(XEN_UNWRAP_WIDGET(dialog)), XEN_TO_C_STRING(directory));
  return(directory);
}


static XEN g_add_file_to_view_files_list(XEN file, XEN dialog) 
{
  #define H_add_file_to_view_files_list "(" S_add_file_to_view_files_list " file :optional w): adds file to the View:Files dialog's list"
  char *name = NULL;

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_add_file_to_view_files_list, "a string");
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog) || XEN_NOT_BOUND_P(dialog), dialog, XEN_ARG_2, S_add_file_to_view_files_list, "a view-files dialog widget"); 

  name = mus_expand_filename(XEN_TO_C_STRING(file));
  if (mus_file_probe(name))
    {
      if (XEN_NOT_BOUND_P(dialog))
	view_files_add_file(NULL_WIDGET, name);
      else view_files_add_file((widget_t)(XEN_UNWRAP_WIDGET(dialog)), name);
    }
  if (name) free(name);
  return(file);
}


static XEN g_view_files_sort(XEN dialog) 
{
  #define H_view_files_sort "(" S_view_files_sort " :optional dialog): sort choice in View:files dialog."
  if (XEN_BOUND_P(dialog))
    {
      XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_sort, "a view-files dialog widget"); 
      return(C_TO_XEN_INT(view_files_local_sort((widget_t)(XEN_UNWRAP_WIDGET(dialog)))));
    }
  return(C_TO_XEN_INT(view_files_sort(ss)));
}


static XEN g_set_view_files_sort(XEN dialog, XEN val) 
{
  int choice;
  XEN sort_choice;

  if (XEN_BOUND_P(val)) sort_choice = val; else sort_choice = dialog;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(sort_choice), sort_choice, XEN_ARG_1, S_setB S_view_files_sort, "an integer"); 

  choice = XEN_TO_C_INT(sort_choice);
  if ((choice < 0) ||
      (choice >= (ss->file_sorters_size + SORT_XEN)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_view_files_sort, 2, sort_choice, "must be a valid file-sorter index");

  if (XEN_BOUND_P(val))
    {
      widget_t w;
      XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ARG_1, S_setB S_view_files_sort, "a view-files dialog widget"); 
      w = (widget_t)(XEN_UNWRAP_WIDGET(dialog));
      view_files_set_local_sort(w, choice);
      return(C_TO_XEN_INT((int)view_files_sort(ss)));
    }
  /* else set global (default) sort choice */
  set_view_files_sort(choice);
  return(C_TO_XEN_INT((int)view_files_sort(ss)));
}


static XEN g_view_files_amp(XEN dialog)
{
  #define H_view_files_amp "(" S_view_files_amp " dialog): amp setting in the given View:Files dialog"
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_amp, "a view-files dialog widget"); 
  return(C_TO_XEN_DOUBLE(view_files_amp((widget_t)(XEN_UNWRAP_WIDGET(dialog)))));
}


static XEN g_view_files_set_amp(XEN dialog, XEN amp)
{
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ARG_1, S_setB S_view_files_amp, "a view-files dialog widget"); 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(amp), amp, XEN_ARG_2, S_setB S_view_files_amp, "a number");
  view_files_set_amp((widget_t)(XEN_UNWRAP_WIDGET(dialog)), XEN_TO_C_DOUBLE(amp));
  return(amp);
}


static XEN g_view_files_speed(XEN dialog)
{
  #define H_view_files_speed "(" S_view_files_speed " dialog): speed setting in the given View:Files dialog"
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_speed, "a view-files dialog widget"); 
  return(C_TO_XEN_DOUBLE(view_files_speed((widget_t)(XEN_UNWRAP_WIDGET(dialog)))));
}


static XEN g_view_files_set_speed(XEN dialog, XEN speed)
{
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_setB S_view_files_speed, "a view-files dialog widget"); 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(speed), speed, XEN_ARG_2, S_setB S_view_files_speed, "a number");
  view_files_set_speed((widget_t)(XEN_UNWRAP_WIDGET(dialog)), XEN_TO_C_DOUBLE(speed));
  return(speed);
}


static XEN g_view_files_amp_env(XEN dialog)
{
  #define H_view_files_amp_env "(" S_view_files_amp_env " dialog): amp env breakpoints in the given View:Files dialog"
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_amp_env, "a view-files dialog widget"); 
  return(env_to_xen(view_files_amp_env((widget_t)(XEN_UNWRAP_WIDGET(dialog)))));
}


static XEN g_view_files_set_amp_env(XEN dialog, XEN amp_env)
{
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_setB S_view_files_amp_env, "a view-files dialog widget"); 
  XEN_ASSERT_TYPE(XEN_LIST_P(amp_env), amp_env, XEN_ARG_2, S_setB S_view_files_amp_env, "an envelope");
  view_files_set_amp_env((widget_t)(XEN_UNWRAP_WIDGET(dialog)), xen_to_env(amp_env));
  return(amp_env);
}


static XEN g_view_files_speed_style(XEN dialog)
{
  #define H_view_files_speed_style "(" S_view_files_speed_style " dialog): speed_style in use in the given View:Files dialog"
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_speed_style, "a view-files dialog widget"); 
  return(C_TO_XEN_INT((int)(view_files_speed_style((widget_t)(XEN_UNWRAP_WIDGET(dialog))))));
}


static XEN g_view_files_set_speed_style(XEN dialog, XEN speed_style)
{
  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_setB S_view_files_speed_style, "a view-files dialog widget"); 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speed_style), speed_style, XEN_ARG_2, S_setB S_view_files_speed_style, "an int");
  view_files_set_speed_style((widget_t)(XEN_UNWRAP_WIDGET(dialog)), (speed_style_t)(XEN_TO_C_INT(speed_style)));
  return(speed_style);
}


static XEN g_view_files_selected_files(XEN dialog)
{
  #define H_view_files_selected_files "(" S_view_files_selected_files " dialog): list of files currently selected in the given View:Files dialog"
  XEN result = XEN_EMPTY_LIST;
  char **selected_files;
  int i, len = 0;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_selected_files, "a view-files dialog widget"); 

  selected_files = view_files_selected_files((widget_t)(XEN_UNWRAP_WIDGET(dialog)), &len);
  if ((selected_files) && (len > 0))
    {
      for (i = 0; i < len; i++)
	{
	  result = XEN_CONS(C_TO_XEN_STRING(selected_files[i]), result);
	  free(selected_files[i]);
	}
      free(selected_files);
    }
  return(result);
}


static XEN g_view_files_set_selected_files(XEN dialog, XEN files)
{
  int i, len;
  char **cfiles = NULL;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ARG_1, S_setB S_view_files_selected_files, "a view-files dialog widget");   
  XEN_ASSERT_TYPE(XEN_LIST_P(files), files, XEN_ARG_2, S_setB S_view_files_selected_files, "a list of files or directories");

  len = XEN_LIST_LENGTH(files);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	if (!(XEN_STRING_P(XEN_LIST_REF(files, i))))
	  {
	    XEN_ASSERT_TYPE(0, XEN_LIST_REF(files, i), i, S_setB S_view_files_selected_files, "a filename (string)");
	    return(XEN_FALSE);
	  }
      cfiles = (char **)calloc(len, sizeof(char *));
      for (i = 0; i < len; i++)
	cfiles[i] = (char *)XEN_TO_C_STRING(XEN_LIST_REF(files, i));
      view_files_set_selected_files((widget_t)(XEN_UNWRAP_WIDGET(dialog)), cfiles, len);
      free(cfiles);
    }
  return(files);
}


static XEN g_view_files_files(XEN dialog)
{
  #define H_view_files_files "(" S_view_files_files " dialog): list of files currently available in the given View:Files dialog"
  XEN result = XEN_EMPTY_LIST;
  char **files;
  int i, len = 0;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ONLY_ARG, S_view_files_files, "a view-files dialog widget"); 

  files = view_files_files((widget_t)(XEN_UNWRAP_WIDGET(dialog)), &len);
  if ((files) && (len > 0))
    for (i = 0; i < len; i++)
      result = XEN_CONS(C_TO_XEN_STRING(files[i]), result);
  return(result);
}


static XEN g_view_files_set_files(XEN dialog, XEN files)
{
  int i, len = 0;
  char **cfiles = NULL;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(dialog), dialog, XEN_ARG_1, S_setB S_view_files_files, "a view-files dialog widget");   
  XEN_ASSERT_TYPE(XEN_LIST_P(files), files, XEN_ARG_2, S_setB S_view_files_files, "a list of files or directories");

  len = XEN_LIST_LENGTH(files);
  if (len > 0)
    {
      for (i = 0; i < len; i++)
	if (!(XEN_STRING_P(XEN_LIST_REF(files, i))))
	  {
	    XEN_ASSERT_TYPE(0, XEN_LIST_REF(files, i), i, S_setB S_view_files_files, "a filename (string)");
	    return(XEN_FALSE);
	  }
      cfiles = (char **)calloc(len, sizeof(char *));
      for (i = 0; i < len; i++)
	cfiles[i] = (char *)XEN_TO_C_STRING(XEN_LIST_REF(files, i));
    }
  view_files_set_files((widget_t)(XEN_UNWRAP_WIDGET(dialog)), cfiles, len);
  if (cfiles) free(cfiles);
  return(files);
}


static XEN view_files_select_hook;

static void view_files_run_select_hook(widget_t dialog, const char *selected_file)
{
  if (XEN_HOOKED(view_files_select_hook))
    run_hook(view_files_select_hook,
	     XEN_LIST_2(XEN_WRAP_WIDGET(dialog),
			C_TO_XEN_STRING(selected_file)),
	     S_view_files_select_hook);
}

/* -------- file-filters and file-sorters -------- */

#define INITIAL_FILE_SORTERS_SIZE 4

static bool file_sorter_ok(XEN name, XEN proc, const char *caller)
{
  char *errmsg;
  XEN errstr;
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, caller, "a string");   
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc), proc, XEN_ARG_2, caller, "a procedure of 2 args (file1 and file2)");
  errmsg = procedure_ok(proc, 2, caller, "function", 2);
  if (errmsg)
    {
      errstr = C_TO_XEN_STRING(errmsg);
      free(errmsg);
      snd_bad_arity_error(caller, errstr, proc);
      return(false);
    }
  return(true);
}


static XEN g_add_file_sorter(XEN name, XEN proc)
{
  #define H_add_file_sorter "(" S_add_file_sorter " name proc) -- add proc with identifier name to file sorter list, returns its index"
  int i, len, choice = -1;
  /* type checks are redundant here */

  if (file_sorter_ok(name, proc, S_add_file_sorter))
    {
      len = ss->file_sorters_size;
      for (i = 0; i < len; i++)
	{
	  if (XEN_FALSE_P(XEN_VECTOR_REF(ss->file_sorters, i)))
	    {
	      XEN_VECTOR_SET(ss->file_sorters, i, XEN_LIST_2(name, proc));
	      choice = i;
	      break;
	    }
	}
      if (choice == -1)
	{
	  ss->file_sorters_size = len * 2;
	  ss->file_sorters = g_expand_vector(ss->file_sorters, ss->file_sorters_size);
	  XEN_VECTOR_SET(ss->file_sorters, len, XEN_LIST_2(name, proc));
	  choice = len;
	}
      view_files_reflect_sort_items();
    }
  return(C_TO_XEN_INT(choice + SORT_XEN));
}


static XEN g_delete_file_sorter(XEN index)
{
  #define H_delete_file_sorter "(" S_delete_file_sorter " index) -- delete proc with identifier name from file sorter list"
  int pos;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(index), index, XEN_ONLY_ARG, S_delete_file_sorter, "a file-sorter index");   
  pos = XEN_TO_C_INT(index);
  if ((pos >= SORT_XEN) &&
      ((pos - SORT_XEN) < ss->file_sorters_size))
    XEN_VECTOR_SET(ss->file_sorters, pos - SORT_XEN, XEN_FALSE);
  view_files_reflect_sort_items();
  return(index);
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_1(g_view_files_sort_w, g_view_files_sort)
XEN_ARGIFY_2(g_set_view_files_sort_w, g_set_view_files_sort)
XEN_ARGIFY_2(g_add_directory_to_view_files_list_w, g_add_directory_to_view_files_list)
XEN_ARGIFY_2(g_add_file_to_view_files_list_w, g_add_file_to_view_files_list)
XEN_ARGIFY_2(g_view_files_dialog_w, g_view_files_dialog)
XEN_NARGIFY_1(g_view_files_amp_w, g_view_files_amp)
XEN_NARGIFY_2(g_view_files_set_amp_w, g_view_files_set_amp)
XEN_NARGIFY_1(g_view_files_speed_w, g_view_files_speed)
XEN_NARGIFY_2(g_view_files_set_speed_w, g_view_files_set_speed)
XEN_NARGIFY_1(g_view_files_amp_env_w, g_view_files_amp_env)
XEN_NARGIFY_2(g_view_files_set_amp_env_w, g_view_files_set_amp_env)
XEN_NARGIFY_1(g_view_files_speed_style_w, g_view_files_speed_style)
XEN_NARGIFY_2(g_view_files_set_speed_style_w, g_view_files_set_speed_style)
XEN_NARGIFY_1(g_view_files_selected_files_w, g_view_files_selected_files)
XEN_NARGIFY_1(g_view_files_files_w, g_view_files_files)
XEN_NARGIFY_2(g_view_files_set_selected_files_w, g_view_files_set_selected_files)
XEN_NARGIFY_2(g_view_files_set_files_w, g_view_files_set_files)
XEN_NARGIFY_1(g_delete_file_sorter_w, g_delete_file_sorter)
XEN_NARGIFY_2(g_add_file_sorter_w, g_add_file_sorter)
#else
#define g_view_files_sort_w g_view_files_sort
#define g_set_view_files_sort_w g_set_view_files_sort
#define g_add_directory_to_view_files_list_w g_add_directory_to_view_files_list
#define g_add_file_to_view_files_list_w g_add_file_to_view_files_list
#define g_view_files_dialog_w g_view_files_dialog
#define g_view_files_amp_w g_view_files_amp
#define g_view_files_set_amp_w g_view_files_set_amp
#define g_view_files_amp_env_w g_view_files_amp_env
#define g_view_files_set_amp_env_w g_view_files_set_amp_env
#define g_view_files_speed_style_w g_view_files_speed_style
#define g_view_files_set_speed_style_w g_view_files_set_speed_style
#define g_view_files_speed_w g_view_files_speed
#define g_view_files_set_speed_w g_view_files_set_speed
#define g_view_files_selected_files_w g_view_files_selected_files
#define g_view_files_files_w g_view_files_files
#define g_view_files_set_selected_files_w g_view_files_set_selected_files
#define g_view_files_set_files_w g_view_files_set_files
#define g_delete_file_sorter_w g_delete_file_sorter
#define g_add_file_sorter_w g_add_file_sorter

#endif




void g_init_gxfile(void)
{
#if HAVE_SCHEME
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.scm \
to popup file info as follows: \n\
(hook-push " S_mouse_enter_label_hook "\n\
  (lambda (type position name)\n\
    (if (not (= type 2))\n\
        (" S_info_dialog " name (finfo name)))))\n\
See also nb.scm."
#endif
#if HAVE_RUBY
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.rb \
to popup file info as follows: \n\
$mouse_enter_label_hook.add_hook!(\"finfo\") do |type, position, name|\n\
  if type != 2\n\
    " S_info_dialog "(name, finfo(name))\n\
  end\n\
end\n\
See also nb.rb."
#endif
#if HAVE_FORTH
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.fs \
to popup file info as follows: \n\
" S_mouse_enter_label_hook " lambda: <{ type position name }>\n\
  type 2 <> if\n\
    name name finfo info-dialog\n\
  else\n\
    #f\n\
  then\n\
; add-hook!"
#endif

  #define H_mouse_leave_label_hook S_mouse_leave_label_hook " (type position label): called when the mouse leaves a file viewer or region label"

  mouse_enter_label_hook = XEN_DEFINE_HOOK(S_mouse_enter_label_hook, "(make-hook 'type 'position 'label)", 3, H_mouse_enter_label_hook);
  mouse_leave_label_hook = XEN_DEFINE_HOOK(S_mouse_leave_label_hook, "(make-hook 'type 'position 'label)", 3, H_mouse_leave_label_hook);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_amp, g_view_files_amp_w, H_view_files_amp,
				   S_setB S_view_files_amp, g_view_files_set_amp_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_amp_env, g_view_files_amp_env_w, H_view_files_amp_env,
				   S_setB S_view_files_amp_env, g_view_files_set_amp_env_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_speed_style, g_view_files_speed_style_w, H_view_files_speed_style,
				   S_setB S_view_files_speed_style, g_view_files_set_speed_style_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_speed, g_view_files_speed_w, H_view_files_speed,
				   S_setB S_view_files_speed, g_view_files_set_speed_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_files, g_view_files_files_w, H_view_files_files,
				   S_setB S_view_files_files, g_view_files_set_files_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_selected_files, g_view_files_selected_files_w, H_view_files_selected_files,
				   S_setB S_view_files_selected_files, g_view_files_set_selected_files_w,  1, 0, 2, 0);

  XEN_DEFINE_SAFE_PROCEDURE(S_add_directory_to_view_files_list, g_add_directory_to_view_files_list_w, 1, 1, 0, H_add_directory_to_view_files_list);
  XEN_DEFINE_SAFE_PROCEDURE(S_add_file_to_view_files_list,      g_add_file_to_view_files_list_w,      1, 1, 0, H_add_file_to_view_files_list);
  XEN_DEFINE_PROCEDURE(S_view_files_dialog,                g_view_files_dialog_w,                0, 2, 0, H_view_files_dialog);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_view_files_sort, g_view_files_sort_w, H_view_files_sort,
				   S_setB S_view_files_sort, g_set_view_files_sort_w,  0, 1, 1, 1);

  XEN_ADD_HOOK(ss->snd_open_file_hook, vf_open_file_watcher_w, "view-files-dialog-open-file-handler", "view-files dialog open-file handler");

  #define H_view_files_select_hook S_view_files_select_hook "(dialog name): called when a file is selected in the \
files list of the View Files dialog.  If it returns " PROC_TRUE ", the default action, opening the file, is omitted."

  view_files_select_hook = XEN_DEFINE_HOOK(S_view_files_select_hook, "(make-hook 'dialog 'name)", 2, H_view_files_select_hook);

  /* file-filters and file-sorters are lists from user's point of view, but I want to
   *   make sure they're gc-protected through add/delete/set, and want such code compatible
   *   with current Ruby xen macros, so I'll use an array internally.
   */
  ss->file_sorters_size = INITIAL_FILE_SORTERS_SIZE;
  ss->file_sorters = XEN_MAKE_VECTOR(ss->file_sorters_size, XEN_FALSE);
  XEN_PROTECT_FROM_GC(ss->file_sorters);

  XEN_DEFINE_SAFE_PROCEDURE(S_add_file_sorter,    g_add_file_sorter_w,    2, 0, 0, H_add_file_sorter);
  XEN_DEFINE_SAFE_PROCEDURE(S_delete_file_sorter, g_delete_file_sorter_w, 1, 0, 0, H_delete_file_sorter);

}
