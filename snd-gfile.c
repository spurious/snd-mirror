#include "snd.h"

/* various file-related dialogs:
   File|Edit-Save-as, 
   File:Open|View, 
   File|Edit-Mix,
   File:Insert,
   File:Edit-Header, Raw, New
   View:Files and region lists 
*/

/* it would be nice if there were some way in gtk to...
 *   filter the file list in the FileSelectionDialog
 *   change the label (to "Sound Files") of the FileSelection Files list
 *   filter out local directories in the file_filter (FileChooser)
 *   get some indication that the user clicked 'Save' when there's no filename (FileChooser) 
 *   get a text entry widget for the file name in Open mode in the FileChooser (I'll have to add this by hand)
 */


/* TODO: play button and file filters in save-as
 * TODO: separate funcs to add play/just-sounds buttons (if the latter is possible)
 * PERHAPS: make file_chooser/file_selection a run-time choice somehow? -- file_chooser is really ugly and stupid...
 * TODO: FileSelection: reflect filename change in selection list and in info
 * TODO: FileSelection (+chooser if entry): cancel info in mix/open if filename in textfield doesn't match selected file 
 *       FileSelection (+chooser if entry): update info as file name is typed, as in write-protected case 
 * TODO: GTK_STOCK_STOP -> C-G? as menu item? -- stop anything
 *         or post a Stop sign whenever the dac is in progress (or any long computation)
 *         similarly for Update
 *         This could be in the sound's pixmap area except that none of the small stop icons looks good.
 *         (and who on earth thinks a phillips screw head means "stop"!)
 * TODO: FileChooser: add entry for filename in Open case (with completion)
 * TODO: many of the button labels are wrong: cancel->quit, open->mix etc
 */



#ifndef HAVE_GFCDN
  #if DEBUGGING
     #define HAVE_GFCDN 0
  #else
     #define HAVE_GFCDN HAVE_GTK_FILE_CHOOSER_DIALOG_NEW
  #endif
#endif
/* -------- just-sounds file list handlers -------- */

#if HAVE_GFCDN
static GtkFileFilter *all_files_filter, *sound_files_filter;

static int sound_files_only_filter(const GtkFileFilterInfo *filter_info, gpointer data)
{
  /* apparently I never see the folders in the filter, and can't select "files only" mode. */
  return((int)((sound_file_p((char *)(filter_info->display_name))) && 
	       (run_just_sounds_hook(filter_info->filename))));
}
#endif


/* -------- play selected file handlers -------- */

typedef struct dialog_play_info {
  GtkWidget *dialog, *play_button;
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

static char *snd_filer_get_filename(GtkWidget *dialog);

static void play_selected_callback(GtkWidget *w, gpointer data)
{
  dialog_play_info *dp = (dialog_play_info *)data;
  if (GTK_TOGGLE_BUTTON(w)->active)
    {
      char *filename;
      if ((dp->player) && (dp->player->playing)) 
	stop_playing_sound(dp->player, PLAY_BUTTON_UNSET);
      filename = snd_filer_get_filename(dp->dialog);
      if (filename)
	{
	  if (mus_file_probe(filename))
	    {
	      dp->player = make_sound_readable(filename, false);
	      dp->player->delete_me = dp;
	      if (dp->player)
		play_sound(dp->player, 0, NO_END_SPECIFIED, IN_BACKGROUND, AT_CURRENT_EDIT_POSITION);
	    }
	  FREE(filename);
	}
    }
  else file_dialog_stop_playing(dp);
}


/* ---------------- file dialogs ---------------- */

typedef struct file_dialog_info {
  int file_dialog_read_only;
  GtkWidget *dialog, *dialog_frame, *dialog_info1, *dialog_info2, *dialog_vbox;
  dialog_play_info *dp;
} file_dialog_info;

#define USE_LABEL_FOR_INFO 0
#if USE_LABEL_FOR_INFO
  #define NEW_INFO() gtk_label_new(NULL)
  #define CHANGE_INFO(Widget, Message) gtk_label_set_text(GTK_LABEL(Widget), Message)
  #define INFO_MARGIN 2
  #define SET_INFO_SIZE(Widget, Size)
#else
  #define NEW_INFO() snd_gtk_label_new(NULL, ss->sgx->highlight_color)
  #define CHANGE_INFO(Widget, Message) gtk_entry_set_text(GTK_ENTRY(Widget), Message)
  #define INFO_MARGIN 0
  #define SET_INFO_SIZE(Widget, Size) gtk_entry_set_width_chars(GTK_ENTRY(Widget), Size)
#endif

#if HAVE_GFCDN

static char *snd_filer_get_filename(GtkWidget *dialog)
{
  gchar *tmp;
  char *str;
  tmp = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
  str = copy_string((char *)tmp);
  g_free(tmp);
  /* copy then g_free here so that we can use FREE in both cases */
  return(str);
}

typedef void (*Callback_Func)(GtkWidget *w, gpointer ptr);

typedef struct {
  Callback_Func ok;
  Callback_Func cancel;
  Callback_Func help;
  Callback_Func extract;
  void *fd;
} filer_response_t;

static filer_response_t *wrap_filer_callbacks(Callback_Func ok, Callback_Func cancel, Callback_Func help, Callback_Func extract,
					      void *fd)
{
  filer_response_t *fr;
  fr = (filer_response_t *)CALLOC(1, sizeof(filer_response_t));
  fr->ok = ok;
  fr->cancel = cancel;
  fr->help = help;
  fr->extract = extract;
  fr->fd = fd;
  return(fr);
}

static void chooser_response_callback(GtkDialog *dialog, gint response_id)
{
  filer_response_t *fr;
  fr = (filer_response_t *)get_user_data(G_OBJECT(dialog));
  /*
  fprintf(stderr, "reponse: %p %d (%d)\n", fr, response_id, GTK_RESPONSE_OK);
  */
  if (response_id == GTK_RESPONSE_OK)
    (*(fr->ok))(GTK_WIDGET(dialog), (gpointer)(fr->fd));
  else
    {
      if (response_id == GTK_RESPONSE_APPLY)
	(*(fr->extract))(GTK_WIDGET(dialog), (gpointer)(fr->fd));
      else
	{
	  if (response_id == GTK_RESPONSE_CANCEL)
	    (*(fr->cancel))(GTK_WIDGET(dialog), (gpointer)(fr->fd));
	  else
	    {
	      if (response_id == GTK_RESPONSE_HELP)
		(*(fr->help))(GTK_WIDGET(dialog), (gpointer)(fr->fd));
	    }
	}
    }
}

static GtkWidget *file_chooser_button(GtkFileChooserDialog *dialog, int response)
{
  /* find a given goddamn button -- this code borrowed from gtk/gtkfilechooserdialog.c */
  /*    dialog uses button_new_from_stock to create these */
  GtkWidget *result = NULL;

#if HAVE_GTK_DIALOG_GET_RESPONSE_FOR_WIDGET
  GList *children, *l;
  children = gtk_container_get_children(GTK_CONTAINER(GTK_DIALOG(dialog)->action_area));
  for (l = children; l; l = l->next)
    {
      GtkWidget *widget;
      int response_id;
      widget = GTK_WIDGET(l->data);
      response_id = gtk_dialog_get_response_for_widget(GTK_DIALOG(dialog), widget);
      if (response_id == response)
	{
	  result = widget;
	  break;
	}
    }
  g_list_free(children);
#endif

  return(result);
}

static GtkWidget *snd_filer_new(char *title, bool saving, 
				GtkSignalFunc gdelete, Callback_Func ok, Callback_Func cancel, Callback_Func help, Callback_Func extract,
				gpointer fd)
{
  GtkWidget *new_dialog;
  if (extract)
    {
      GtkWidget *button;
      new_dialog = gtk_file_chooser_dialog_new(title, NULL, 
					       GTK_FILE_CHOOSER_ACTION_SAVE,
					       GTK_STOCK_SAVE,   GTK_RESPONSE_OK,
					       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					       GTK_STOCK_HELP,   GTK_RESPONSE_HELP,
					       NULL);
      /* can't use varargs above for "extract" because they assume the label is in the gtk-stock */
      /* this code from gtk_dialog_add_button with "with_label" in place of "from_stock" */
      button = gtk_button_new_with_label(_("Extract"));
      gtk_widget_set_name(button, "doit_again_button");
      GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
      gtk_widget_show(button);
      gtk_dialog_add_action_widget(GTK_DIALOG(new_dialog), button, GTK_RESPONSE_APPLY);
    }
  else new_dialog = gtk_file_chooser_dialog_new(title, NULL, 
						(saving) ? GTK_FILE_CHOOSER_ACTION_SAVE : GTK_FILE_CHOOSER_ACTION_OPEN,
						(saving) ? GTK_STOCK_SAVE : GTK_STOCK_OPEN, GTK_RESPONSE_OK,
						GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
						GTK_STOCK_HELP,   GTK_RESPONSE_HELP,
						NULL);
  gtk_window_set_default_size(GTK_WINDOW(new_dialog), 600, 400);
  gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(new_dialog), false);

  set_user_data(G_OBJECT(new_dialog), (gpointer)wrap_filer_callbacks(ok, cancel, help, extract, (void *)fd));
  SG_SIGNAL_CONNECT(new_dialog, "response", chooser_response_callback, NULL); /* why include the pointer here? it is dropped by gtk! */

  if (gdelete)
    {
      /* this has to be separate (not handled as a "response" because the latter deletes the goddamn widget!! */
      SG_SIGNAL_CONNECT(new_dialog, "delete_event", gdelete, fd);
    }

  {
    GtkWidget *button;
    button = file_chooser_button(GTK_FILE_CHOOSER_DIALOG(new_dialog), GTK_RESPONSE_OK);
    if (button) gtk_widget_set_name(button, "doit_button");
    button = file_chooser_button(GTK_FILE_CHOOSER_DIALOG(new_dialog), GTK_RESPONSE_CANCEL);
    if (button) gtk_widget_set_name(button, "quit_button");
    button = file_chooser_button(GTK_FILE_CHOOSER_DIALOG(new_dialog), GTK_RESPONSE_HELP);
    if (button) gtk_widget_set_name(button, "help_button");
  }
  return(new_dialog);
}

#else

#define Callback_Func GtkSignalFunc
static void unpad(gpointer w, gpointer data)
{
  if (GTK_IS_CELL_RENDERER_TEXT(w))
    GTK_CELL_RENDERER(w)->ypad = 0;
}

static gboolean filer_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  if (event->keyval == GDK_Tab)
    {
      gtk_entry_set_text(GTK_ENTRY(w), sound_filename_completer((char *)gtk_entry_get_text(GTK_ENTRY(w))));
      return(true);
    }
  return(false);
}

static char *snd_filer_get_filename(GtkWidget *dialog)
{
  return(copy_string((char *)gtk_file_selection_get_filename(GTK_FILE_SELECTION(dialog))));
}

static GtkWidget *snd_filer_new(char *title, bool saving, 
				GtkSignalFunc gdelete, Callback_Func ok, Callback_Func cancel, Callback_Func help, Callback_Func extract,
				gpointer fd)
{
  GtkWidget *new_dialog, *entry, *helpB;
  GList *cells;
  GtkTreeViewColumn *dirl;
  GtkFileSelection *filer;
  new_dialog = gtk_file_selection_new(title);
  filer = GTK_FILE_SELECTION(new_dialog);

  if (extract)
    {
      GtkWidget *button;
      button = gtk_button_new_with_label(_("Extract"));
      GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
      gtk_widget_show(button);
      gtk_widget_set_name(button, "doit_again_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_dialog)->action_area), button, false, true, 10);
      SG_SIGNAL_CONNECT(button, "clicked", extract, fd);
    }

  helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
  gtk_widget_set_name(helpB, "help_button");
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(new_dialog)->action_area), helpB, false, true, 10);
  SG_SIGNAL_CONNECT(helpB, "clicked", help, fd);
  gtk_widget_show(helpB);

  /* get rid of ridiculous padding in lists */
  dirl = gtk_tree_view_get_column(GTK_TREE_VIEW(filer->dir_list), 0);
  cells = gtk_tree_view_column_get_cell_renderers(dirl);
  g_list_foreach(cells, unpad, NULL);
  g_list_free(cells);
  dirl = gtk_tree_view_get_column(GTK_TREE_VIEW(filer->file_list), 0);
  cells = gtk_tree_view_column_get_cell_renderers(dirl);
  g_list_foreach(cells, unpad, NULL);
  g_list_free(cells);

  /* make entry widget look like one of ours */
  entry = filer->selection_entry;
  gtk_widget_modify_bg(entry, GTK_STATE_NORMAL, ss->sgx->white);
  connect_mouse_to_text(entry);
  SG_SIGNAL_CONNECT(entry, "key_press_event", filer_key_press, NULL);

  if (gdelete)
    SG_SIGNAL_CONNECT(new_dialog, "delete_event", gdelete, fd);
  SG_SIGNAL_CONNECT(filer->ok_button, "clicked", ok, fd);
  SG_SIGNAL_CONNECT(filer->cancel_button, "clicked", cancel, fd);

  gtk_widget_set_name(filer->ok_button, "doit_button");
  gtk_widget_set_name(filer->cancel_button, "quit_button");
  if (filer->fileop_c_dir) gtk_widget_set_name(filer->fileop_c_dir, "help_button");
  if (filer->fileop_del_file) gtk_widget_set_name(filer->fileop_del_file, "doit_again_button");
  if (filer->fileop_ren_file) gtk_widget_set_name(filer->fileop_ren_file, "reset_button");
  return(new_dialog);
}
#endif


/* -------- File Open/View/Mix/ Dialogs -------- */

void alert_new_file(void) {}

#if HAVE_GFCDN
static void update_info_callback(GtkFileChooser *chooser)
{
  char *filename;
  file_dialog_info *fd;
  fd = (file_dialog_info *)g_object_get_data(G_OBJECT(chooser), "snd-dialog");
  filename = (char *)gtk_file_chooser_get_filename(chooser);

#else

static void dialog_select_callback(GtkTreeSelection *selection, gpointer context)
{
  char *filename = NULL;
  file_dialog_info *fd = (file_dialog_info *)context;
  filename = snd_filer_get_filename(fd->dialog);
#endif

  if ((filename) && 
      (!(directory_p(filename))) &&
      (plausible_sound_file_p(filename)))
    {
      char *buf;
      char timestr[64];
      time_t date;
      buf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s: %d chan%s, %d Hz, %.3f secs",
		   filename_without_home_directory(filename),
		   mus_sound_chans(filename),
		   (mus_sound_chans(filename) > 1) ? "s" : "",
		   mus_sound_srate(filename),
		   mus_sound_duration(filename));
      CHANGE_INFO(fd->dialog_info1, buf);
      SET_INFO_SIZE(fd->dialog_info1, 1 + strlen(buf));
      date = mus_sound_write_date(filename);
#if HAVE_STRFTIME
      strftime(timestr, 64, ", %d-%b-%Y", localtime(&date));
#else
      sprintf(timestr, "");
#endif
      mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s %s%s",
		   mus_header_type_name(mus_sound_header_type(filename)),
		   short_data_format_name(mus_sound_data_format(filename), filename),
		   timestr);
      CHANGE_INFO(fd->dialog_info2, buf);
      SET_INFO_SIZE(fd->dialog_info2, 1 + strlen(buf));
      FREE(buf);
      gtk_widget_show(fd->dialog_frame);
      gtk_widget_show(fd->dialog_vbox);
      gtk_widget_show(fd->dialog_info1);
      gtk_widget_show(fd->dialog_info2);
      gtk_widget_show(fd->dp->play_button);
    }
  else
    {
      gtk_widget_hide(fd->dialog_frame);
      gtk_widget_hide(fd->dialog_vbox);
      gtk_widget_hide(fd->dialog_info1);
      gtk_widget_hide(fd->dialog_info2);
      gtk_widget_hide(fd->dp->play_button);
    }
#if (!HAVE_GFCDN)
  if (filename) FREE(filename);
#endif
}

#if HAVE_GFCDN
static file_dialog_info *make_file_dialog(int read_only, char *title, snd_dialog_t which_dialog, 
					  Callback_Func file_ok_proc,
					  GtkSignalFunc file_delete_proc,
					  Callback_Func file_dismiss_proc,
					  Callback_Func file_help_proc)
{
  file_dialog_info *fd;
  GtkWidget *center_info;
  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));
  fd->file_dialog_read_only = read_only;
  fd->dialog = snd_filer_new(title, false, file_delete_proc, file_ok_proc, file_dismiss_proc, file_help_proc, NULL, (gpointer)fd);
  fd->dp->dialog = fd->dialog;
  g_object_set_data(G_OBJECT(fd->dialog), "snd-dialog", (gpointer)fd);

  center_info = gtk_hbox_new(true, 10);
  gtk_widget_show(center_info);

  fd->dialog_frame = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(fd->dialog_frame), GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(fd->dialog_frame), 1);
  gtk_widget_modify_bg(fd->dialog_frame, GTK_STATE_NORMAL, ss->sgx->black);
  gtk_widget_show(fd->dialog_frame);
  gtk_box_pack_start(GTK_BOX(center_info), fd->dialog_frame, false, false, 10 + INFO_MARGIN);

  gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(fd->dialog), center_info);

  fd->dialog_vbox = gtk_vbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(fd->dialog_frame), fd->dialog_vbox);

  fd->dialog_info1 = NEW_INFO();
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info1, false, true, INFO_MARGIN);

  fd->dialog_info2 = NEW_INFO();
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info2, false, true, INFO_MARGIN);

  fd->dp->play_button = gtk_check_button_new_with_label(_("play selected sound"));
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dp->play_button, false, true, 2);
  SG_SIGNAL_CONNECT(fd->dp->play_button, "toggled", play_selected_callback, fd->dp);
  SG_SIGNAL_CONNECT(fd->dialog, "selection-changed", update_info_callback, fd);

  all_files_filter = gtk_file_filter_new();
  gtk_file_filter_set_name(all_files_filter, "All Files");
  gtk_file_filter_add_pattern(all_files_filter, "*");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(fd->dialog), all_files_filter);
  
  sound_files_filter = gtk_file_filter_new();
  gtk_file_filter_set_name(sound_files_filter, "sound files only");
  gtk_file_filter_add_custom(sound_files_filter, 
			     (GtkFileFilterFlags)(GTK_FILE_FILTER_DISPLAY_NAME | GTK_FILE_FILTER_FILENAME), 
			     sound_files_only_filter, 
			     NULL, NULL);
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(fd->dialog), sound_files_filter);
  if (just_sounds(ss))
    gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(fd->dialog), sound_files_filter);
  else gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(fd->dialog), all_files_filter);

  set_dialog_widget(which_dialog, fd->dialog);
  return(fd);
}

#else

static file_dialog_info *make_file_dialog(int read_only, char *title, snd_dialog_t which_dialog, 
					  Callback_Func file_ok_proc,
					  GtkSignalFunc file_delete_proc,
					  Callback_Func file_dismiss_proc,
					  Callback_Func file_help_proc)
{
  file_dialog_info *fd;
  GtkWidget *center_info;
  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));
  fd->file_dialog_read_only = read_only;
  fd->dialog = snd_filer_new(title, false, file_delete_proc, file_ok_proc, file_dismiss_proc, file_help_proc, NULL, fd);
  fd->dp->dialog = fd->dialog;

  center_info = gtk_hbox_new(true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(fd->dialog)->main_vbox), center_info, true, true, 0);
  gtk_widget_show(center_info);

  fd->dialog_frame = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(fd->dialog_frame), GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(fd->dialog_frame), 1);
  gtk_widget_modify_bg(fd->dialog_frame, GTK_STATE_NORMAL, ss->sgx->black);
  gtk_widget_show(fd->dialog_frame);
  gtk_box_pack_start(GTK_BOX(center_info), fd->dialog_frame, false, false, 10 + INFO_MARGIN);

  fd->dialog_vbox = gtk_vbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(fd->dialog_frame), fd->dialog_vbox);

  fd->dialog_info1 = NEW_INFO();
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info1, true, true, INFO_MARGIN);
	
  fd->dialog_info2 = NEW_INFO();
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info2, true, true, INFO_MARGIN);

  SG_SIGNAL_CONNECT(gtk_tree_view_get_selection(GTK_TREE_VIEW(GTK_FILE_SELECTION(fd->dialog)->file_list)), "changed", dialog_select_callback, fd);

  fd->dp->play_button = gtk_check_button_new_with_label(_("play selected sound"));
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dp->play_button, true, true, 2);
  SG_SIGNAL_CONNECT(fd->dp->play_button, "toggled", play_selected_callback, fd->dp);
  set_dialog_widget(which_dialog, fd->dialog);
  return(fd);
}

#endif

static void file_open_error(const char *error_msg, void *ufd)
{
  /* called from snd_error, redirecting error handling to the dialog */
  file_dialog_info *fd = (file_dialog_info *)ufd;
  CHANGE_INFO(fd->dialog_info1, error_msg);
  SET_INFO_SIZE(fd->dialog_info1, strlen(error_msg));
  gtk_widget_show(fd->dialog_frame);
  gtk_widget_show(fd->dialog_vbox);
  gtk_widget_show(fd->dialog_info1);
  gtk_widget_hide(fd->dialog_info2);
  gtk_widget_hide(fd->dp->play_button);
}

static void clear_file_error_label(file_dialog_info *fd)
{
  CHANGE_INFO(fd->dialog_info1, "");
  gtk_widget_hide(fd->dialog_frame);
  gtk_widget_hide(fd->dialog_vbox);
  gtk_widget_hide(fd->dialog_info1);
  gtk_widget_hide(fd->dialog_info2);
  gtk_widget_hide(fd->dp->play_button);
}

#if HAVE_GFCDN
static gulong file_error_handler_id = 0;
static void open_modify_callback(GtkFileChooser *dialog, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  clear_file_error_label(fd);
  if (file_error_handler_id)
    {
      g_signal_handler_disconnect(dialog, file_error_handler_id);
      file_error_handler_id = 0;
    }
}
#else
/* key press event here, not key release -- the latter is triggered by the <return> release
 *   that triggered the error, so our error is immediately erased
 */
static gulong key_press_handler_id = 0, selection_changed_handler_id = 0;
static void clear_open_handlers(GtkWidget *dialog)
{
  if (key_press_handler_id)
    {
      g_signal_handler_disconnect(GTK_FILE_SELECTION(dialog)->selection_entry, key_press_handler_id);
      key_press_handler_id = 0;
    }
  if (selection_changed_handler_id)
    {
      g_signal_handler_disconnect(gtk_tree_view_get_selection(GTK_TREE_VIEW(GTK_FILE_SELECTION(dialog)->file_list)), selection_changed_handler_id);
      selection_changed_handler_id = 0;
    }
}

static gboolean open_modify_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;  
  clear_file_error_label(fd);
  clear_open_handlers(fd->dialog);
  return(false);
}
static void open_modify_selection_changed(GtkTreeSelection *selection, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  clear_file_error_label(fd);
  clear_open_handlers(fd->dialog);
}
#endif

static void clear_error_if_open_changes(GtkWidget *dialog, void *data)
{
#if HAVE_GFCDN
  file_error_handler_id = g_signal_connect(dialog, "selection-changed", G_CALLBACK(open_modify_callback), (gpointer)data);
#else
  selection_changed_handler_id = g_signal_connect(gtk_tree_view_get_selection(GTK_TREE_VIEW(GTK_FILE_SELECTION(dialog)->file_list)), "changed",
						  G_CALLBACK(open_modify_selection_changed), data);
  key_press_handler_id = SG_SIGNAL_CONNECT(GTK_FILE_SELECTION(dialog)->selection_entry, "key_press_event", open_modify_key_press, data);
#endif
}

static void file_open_dialog_ok(GtkWidget *w, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  char *filename = NULL;
  gpointer hide_me = 0;
  filename = snd_filer_get_filename(fd->dialog);
  if ((!filename) || (!(*filename)))
    {
      file_open_error(_("no filename given"), (void *)fd);
      clear_error_if_open_changes(fd->dialog, (void *)fd);
    }
  else
    {
      file_dialog_stop_playing(fd->dp);
      if (!(directory_p(filename)))
	{
	  snd_info *sp;
	  redirect_snd_error_to(file_open_error, (void *)fd);
	  ss->sgx->requestor_dialog = fd->dialog;
	  ss->open_requestor = FROM_OPEN_DIALOG;
	  sp = snd_open_file(filename, fd->file_dialog_read_only);
	  redirect_snd_error_to(NULL, NULL);
	  if (sp) 
	    {
	      hide_me = g_object_get_data(G_OBJECT(fd->dialog), "hide-me"); /* see snd-gtk.scm where this is set */
	      if (hide_me == 0)
		gtk_widget_hide(fd->dialog);
	      select_channel(sp, 0); /* add_sound_window (snd-xsnd.c) -> make_file_info (snd-file) will report reason for error, if any */
	    }
	  else
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		clear_error_if_open_changes(fd->dialog, (void *)fd);
	    }
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
  if (filename) FREE(filename);
}

static void file_open_dialog_dismiss(GtkWidget *w, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  file_dialog_stop_playing(fd->dp);
  gtk_widget_hide(fd->dialog);
}

static void file_open_dialog_help(GtkWidget *w, gpointer context)
{
  open_file_dialog_help();
}

static gint file_open_dialog_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  file_dialog_stop_playing(fd->dp);
  gtk_widget_hide(fd->dialog);
  return(true);
}

static file_dialog_info *odat = NULL;

widget_t make_open_file_dialog(bool read_only, bool managed)
{
  if (!odat)
    odat = make_file_dialog(read_only, (char *)((read_only) ? _("View") : _("Open")), FILE_OPEN_DIALOG,
			    (Callback_Func)file_open_dialog_ok,				     
			    (GtkSignalFunc)file_open_dialog_delete,
			    (Callback_Func)file_open_dialog_dismiss,
			    (Callback_Func)file_open_dialog_help);
  else
    {
      if (read_only != odat->file_dialog_read_only)
	{
	  gtk_window_set_title(GTK_WINDOW(odat->dialog), (char *)((read_only) ? _("View") : _("Open")));
	  odat->file_dialog_read_only = read_only;
	}
    }
  if (managed) gtk_widget_show(odat->dialog);
  return(odat->dialog);
}


/* -------- mix file dialog -------- */

static file_dialog_info *mdat = NULL;

static void file_mix_cancel_callback(GtkWidget *w, gpointer context)
{
  file_dialog_stop_playing(mdat->dp);
  gtk_widget_hide(mdat->dialog);
}

static void file_mix_help_callback(GtkWidget *w, gpointer context)
{
  mix_file_dialog_help();
}

static gint file_mix_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(mdat->dp);
  gtk_widget_hide(mdat->dialog);
  return(true);
}

static void file_mix_ok_callback(GtkWidget *w, gpointer context)
{
  char *filename = NULL;
  filename = snd_filer_get_filename(mdat->dialog);
  if ((!filename) || (!(*filename)))
    {
      file_open_error(_("no filename given"), (void *)mdat);
      clear_error_if_open_changes(mdat->dialog, (void *)mdat);
    }
  else
    {
      file_dialog_stop_playing(mdat->dp);
      if (!(directory_p(filename)))
	{
	  snd_info *sp;
	  int err;
	  sp = any_selected_sound();
	  redirect_snd_error_to(file_open_error, (void *)mdat);
	  ss->sgx->requestor_dialog = mdat->dialog;
	  ss->open_requestor = FROM_MIX_DIALOG;
	  err = mix_complete_file_at_cursor(sp, filename, with_mix_tags(ss), 0);
	  redirect_snd_error_to(NULL, NULL);
	  if (err < 0) 
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		clear_error_if_open_changes(mdat->dialog, (void *)mdat);
	    }
	  else report_in_minibuffer(sp, _("%s mixed in at cursor"), filename);
	}
      else 
	{
	  char *str;
	  str = mus_format(_("%s is a directory"), filename);
	  file_open_error(str, (void *)mdat);
	  clear_error_if_open_changes(mdat->dialog, (void *)mdat);
	  FREE(str);
	}
    }
  if (filename) FREE(filename);
}

widget_t make_mix_file_dialog(bool managed)
{
  if (mdat == NULL)
    mdat = make_file_dialog(true, _("mix file:"), FILE_MIX_DIALOG,
			    (Callback_Func)file_mix_ok_callback,
			    (GtkSignalFunc)file_mix_delete_callback,
			    (Callback_Func)file_mix_cancel_callback,
			    (Callback_Func)file_mix_help_callback);
  if (managed) gtk_widget_show(mdat->dialog);
  /* the ok button is special (gtk_dialog_add_button -> gtk_button_new_from_stock), so we can't change it */
  return(mdat->dialog);
}


/* -------- File:Insert dialog -------- */

static file_dialog_info *idat = NULL;

static void file_insert_cancel_callback(GtkWidget *w, gpointer context)
{
  file_dialog_stop_playing(idat->dp);
  gtk_widget_hide(idat->dialog);
}

static void file_insert_help_callback(GtkWidget *w, gpointer context)
{
  insert_file_dialog_help();
}

static gint file_insert_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(idat->dp);
  gtk_widget_hide(idat->dialog);
  return(true);
}

static void file_insert_ok_callback(GtkWidget *w, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename;
  filename = snd_filer_get_filename(idat->dialog);
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
  if (filename) FREE(filename);
}
  
widget_t make_insert_file_dialog(bool managed)
{
  if (idat == NULL)
    idat = make_file_dialog(true, _("insert file:"), FILE_INSERT_DIALOG,
			    (Callback_Func)file_insert_ok_callback,
			    (GtkSignalFunc)file_insert_delete_callback,
			    (Callback_Func)file_insert_cancel_callback,
			    (Callback_Func)file_insert_help_callback);
  if (managed) gtk_widget_show(idat->dialog);
  return(idat->dialog);
}



void set_open_file_play_button(bool val) 
{
  if ((odat) && (odat->dp->play_button))
    set_toggle_button(odat->dp->play_button, val, false, (gpointer)odat);
  if ((mdat) && (mdat->dp->play_button))
    set_toggle_button(mdat->dp->play_button, val, false, (gpointer)mdat);
  if ((idat) && (idat->dp->play_button))
    set_toggle_button(idat->dp->play_button, val, false, (gpointer)mdat);
}

void reflect_just_sounds(void)
{
#if HAVE_GFCDN
  if (odat)
    gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(odat->dialog), (just_sounds(ss)) ? sound_files_filter : all_files_filter);
  if (mdat)
    gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(mdat->dialog), (just_sounds(ss)) ? sound_files_filter : all_files_filter);
  if (idat)
    gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(idat->dialog), (just_sounds(ss)) ? sound_files_filter : all_files_filter);
#endif
}



/* ---------------- file data panel ---------------- */

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples, int min_chan)
{
  char *str;
  int res;
  char *comment = NULL;
  fdat->error_widget = NOT_A_SCANF_WIDGET;
  fdat->scanf_widget = NOT_A_SCANF_WIDGET;

  if ((srate) && (fdat->srate_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->srate_text)); 
      fdat->scanf_widget = SRATE_WIDGET;
      if ((str) && (*str))
	(*srate) = string_to_int_with_error(str, 1, "srate"); 
      else snd_error("no srate?");
    }

  if ((chans) && (fdat->chans_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->chans_text)); 
      fdat->scanf_widget = CHANS_WIDGET;
      if ((str) && (*str))
	(*chans) = string_to_int_with_error(str, min_chan, "chans"); 
       else
 	{
 	  if (min_chan > 0)
 	    snd_error("no chans?");
 	}
    }

  if ((location) && (fdat->location_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->location_text)); 
      fdat->scanf_widget = DATA_LOCATION_WIDGET;
      if ((str) && (*str))
	(*location) = string_to_off_t_with_error(str, 0, "data location"); 
      else snd_error("no data location?");
    }

  if ((samples) && (fdat->samples_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->samples_text)); 
      fdat->scanf_widget = SAMPLES_WIDGET;
      if ((str) && (*str))
	(*samples) = string_to_off_t_with_error(str, 0, "samples"); 
      else snd_error("no samples?");
    }
  fdat->scanf_widget = SAMPLES_WIDGET;

  if (fdat->header_list)
    {
      res = fdat->header_pos;
      if (res != NO_SELECTION)
	{
	  (*type) = header_type_from_position(res);
	  fdat->current_type = (*type);
	}
    }
  if (fdat->format_list)
    {
      res = fdat->format_pos;
      if (res != NO_SELECTION)
	{
	  (*format) = data_format_from_position(fdat->current_type, res);
	  fdat->current_format = (*format);
	}
    }
  if (fdat->comment_text) 
    {
      if (GTK_IS_TEXT_VIEW(fdat->comment_text))
	comment = sg_get_text(fdat->comment_text, 0, -1);
      else comment = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->comment_text)); 
      str = copy_string(comment);
      return(str);
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
  char *str;
  if (!(fdat->format_list)) return;

  if (type != IGNORE_HEADER_TYPE)
    fdat->current_type = type;
  else fdat->current_type = MUS_RAW;
  fdat->current_format = format;
  fl = set_header_and_data_positions(fdat, fdat->current_type, fdat->current_format);
  if (fl == NULL) return;

  if ((type != IGNORE_HEADER_TYPE) &&
      (fdat->header_list))
    {
      g_signal_handlers_block_matched(GTK_OBJECT(fdat->header_list), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)fdat);
      sg_list_select(fdat->header_list, fdat->header_pos);
      g_signal_handlers_unblock_matched(GTK_OBJECT(fdat->header_list), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)fdat);
    }

  gtk_list_store_clear(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(fdat->format_list))));
  for (i = 0; i < fdat->formats; i++) 
    {
      str = fl[i];
      sg_list_insert(fdat->format_list, i, str);
    }
  g_signal_handlers_block_matched(GTK_OBJECT(fdat->format_list), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)fdat);
  sg_list_select(fdat->format_list, fdat->format_pos);
  g_signal_handlers_unblock_matched(GTK_OBJECT(fdat->format_list), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)fdat);

  if ((srate != IGNORE_SRATE) && 
      (fdat->srate_text))
    widget_int_to_text(fdat->srate_text, srate);

  if ((chans != IGNORE_CHANS) && 
      (fdat->chans_text))
    widget_int_to_text(fdat->chans_text, chans);

  if (fdat->comment_text) 
    {
      if (GTK_IS_TEXT_VIEW(fdat->comment_text))
	{
	  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(fdat->comment_text)), "", 0);
	  sg_text_insert(fdat->comment_text, comment);
	}
      else gtk_entry_set_text(GTK_ENTRY(fdat->comment_text), comment);
    }

  if ((location != IGNORE_DATA_LOCATION) && 
      (fdat->location_text))
    widget_off_t_to_text(fdat->location_text, location);

  if ((samples != IGNORE_SAMPLES) && 
      (fdat->samples_text))
    widget_off_t_to_text(fdat->samples_text, samples);
}

static gboolean data_panel_srate_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  if (event->keyval == GDK_Tab)
    {
      gtk_entry_set_text(GTK_ENTRY(w), srate_completer((char *)gtk_entry_get_text(GTK_ENTRY(w))));
      return(true);
    }
  return(false);
}


#define NUM_REFLECTION_IDS 7
enum {REFLECT_SRATE_ID, REFLECT_CHANS_ID, REFLECT_SAMPLES_ID, REFLECT_LOCATION_ID, 
      REFLECT_COMMENT_ID, REFLECT_FORMAT_ID, REFLECT_HEADER_ID};

static void reflect_file_data_panel_change(file_data *fd, void *data, void (*change_action)(GtkWidget *w, gpointer context))
{
  if (!(fd->reflection_ids))
    fd->reflection_ids = (gulong *)CALLOC(NUM_REFLECTION_IDS, sizeof(gulong));
  if (fd->srate_text)
    fd->reflection_ids[REFLECT_SRATE_ID] = SG_SIGNAL_CONNECT(fd->srate_text, "changed", change_action, (gpointer)data);
  if (fd->chans_text)
    fd->reflection_ids[REFLECT_CHANS_ID] = SG_SIGNAL_CONNECT(fd->chans_text, "changed", change_action, (gpointer)data);
  if (fd->samples_text)
    fd->reflection_ids[REFLECT_SAMPLES_ID] = SG_SIGNAL_CONNECT(fd->samples_text, "changed", change_action, (gpointer)data);
  if (fd->location_text)
    fd->reflection_ids[REFLECT_LOCATION_ID] = SG_SIGNAL_CONNECT(fd->location_text, "changed", change_action, (gpointer)data);
  if (fd->comment_text)
    {
      if (GTK_IS_TEXT_VIEW(fd->comment_text))
	fd->reflection_ids[REFLECT_COMMENT_ID] = 
          SG_SIGNAL_CONNECT(gtk_text_view_get_buffer(GTK_TEXT_VIEW(fd->comment_text)), "changed", change_action, (gpointer)data);
      else fd->reflection_ids[REFLECT_COMMENT_ID] = SG_SIGNAL_CONNECT(fd->comment_text, "changed", change_action, (gpointer)data);
    }
  /* these are technically the wrong callback type, but we ignore the widget */
  if (fd->format_list)
    fd->reflection_ids[REFLECT_FORMAT_ID] = 
      SG_SIGNAL_CONNECT(gtk_tree_view_get_selection(GTK_TREE_VIEW(fd->format_list)), "changed", change_action, (gpointer)data);
  if (fd->header_list)
    fd->reflection_ids[REFLECT_HEADER_ID] = 
      SG_SIGNAL_CONNECT(gtk_tree_view_get_selection(GTK_TREE_VIEW(fd->header_list)), "changed", change_action, (gpointer)data);
}

static void unreflect_file_data_panel_change(file_data *fd, void *data, void (*change_action)(GtkWidget *w, gpointer context))
{
  int i;
  if (!(fd->reflection_ids)) return;
  if ((fd->srate_text) && (fd->reflection_ids[REFLECT_SRATE_ID] > 0))
    g_signal_handler_disconnect(fd->srate_text, fd->reflection_ids[REFLECT_SRATE_ID]);
  if ((fd->chans_text) && (fd->reflection_ids[REFLECT_CHANS_ID] > 0))
    g_signal_handler_disconnect(fd->chans_text, fd->reflection_ids[REFLECT_CHANS_ID]);
  if ((fd->samples_text) && (fd->reflection_ids[REFLECT_SAMPLES_ID] > 0))
    g_signal_handler_disconnect(fd->samples_text, fd->reflection_ids[REFLECT_SAMPLES_ID]);
  if ((fd->location_text) && (fd->reflection_ids[REFLECT_LOCATION_ID] > 0))
    g_signal_handler_disconnect(fd->location_text, fd->reflection_ids[REFLECT_LOCATION_ID]);
  if ((fd->comment_text) && (fd->reflection_ids[REFLECT_COMMENT_ID] > 0))
    {
      if (GTK_IS_TEXT_VIEW(fd->comment_text))
	g_signal_handler_disconnect(gtk_text_view_get_buffer(GTK_TEXT_VIEW(fd->comment_text)), fd->reflection_ids[REFLECT_COMMENT_ID]);
      else g_signal_handler_disconnect(fd->comment_text, fd->reflection_ids[REFLECT_COMMENT_ID]);
    }
  if ((fd->format_list) && (fd->reflection_ids[REFLECT_FORMAT_ID] > 0))
    g_signal_handler_disconnect(gtk_tree_view_get_selection(GTK_TREE_VIEW(fd->format_list)), fd->reflection_ids[REFLECT_FORMAT_ID]);
  if ((fd->header_list) && (fd->reflection_ids[REFLECT_HEADER_ID] > 0))
    g_signal_handler_disconnect(gtk_tree_view_get_selection(GTK_TREE_VIEW(fd->header_list)), fd->reflection_ids[REFLECT_HEADER_ID]);
  for (i = 0; i < NUM_REFLECTION_IDS; i++) fd->reflection_ids[i] = 0;
}

/* -------- panel error handling -------- */

/* if an error occurs, a callback is added to the offending text widget, and an error is
 *   posted in the error_text label.  When the user modifies the bad entry, the callback
 *   erases the error message, and removes itself from the text widget.
 */

static void clear_dialog_error(file_data *fd)
{
  gtk_widget_hide(fd->error_text);
}

static void show_dialog_error(file_data *fd)
{
  gtk_widget_show(fd->error_text);
}

static void post_file_dialog_error(const char *error_msg, void *ufd)
{
  file_data *fd = (file_data *)ufd;
  gtk_entry_set_text(GTK_ENTRY(fd->error_text), (gchar *)error_msg);
  show_dialog_error(fd);
}

#if HAVE_GFCDN
static gulong filename_modify_handler_id = 0;
static void filename_modify_callback(GtkFileChooser *dialog, gpointer data)
{
  file_data *fd = (file_data *)data;
  clear_dialog_error(fd);
  if (filename_modify_handler_id)
    {
      g_signal_handler_disconnect(dialog, filename_modify_handler_id);
      filename_modify_handler_id = 0;
    }
}
#else
/* key press event here, not key release -- the latter is triggered by the <return> release
 *   that triggered the error, so our error is immediately erased
 */
static gulong key_press_filename_handler_id = 0, selection_changed_filename_handler_id = 0;
static void clear_filename_handlers(GtkWidget *dialog)
{
  if (key_press_filename_handler_id)
    {
      g_signal_handler_disconnect(GTK_FILE_SELECTION(dialog)->selection_entry, key_press_filename_handler_id);
      key_press_filename_handler_id = 0;
    }
  if (selection_changed_filename_handler_id)
    {
      g_signal_handler_disconnect(gtk_tree_view_get_selection(GTK_TREE_VIEW(GTK_FILE_SELECTION(dialog)->file_list)), selection_changed_filename_handler_id);
      selection_changed_filename_handler_id = 0;
    }
}

static gboolean filename_modify_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  file_data *fd = (file_data *)data;  
  clear_dialog_error(fd);
  clear_filename_handlers(fd->dialog);
  return(false);
}
static void filename_modify_selection_changed(GtkTreeSelection *selection, gpointer data)
{
  file_data *fd = (file_data *)data;
  clear_dialog_error(fd);
  clear_filename_handlers(fd->dialog);
}
#endif

static void clear_error_if_filename_changes(GtkWidget *dialog, void *data)
{
#if HAVE_GFCDN
  filename_modify_handler_id = g_signal_connect(dialog, "selection-changed", G_CALLBACK(filename_modify_callback), (gpointer)data);
#else
  selection_changed_filename_handler_id = g_signal_connect(gtk_tree_view_get_selection(GTK_TREE_VIEW(GTK_FILE_SELECTION(dialog)->file_list)), "changed",
							   G_CALLBACK(filename_modify_selection_changed), data);
  key_press_filename_handler_id = SG_SIGNAL_CONNECT(GTK_FILE_SELECTION(dialog)->selection_entry, "key_press_event", filename_modify_key_press, data);
#endif
}

static gulong chans_key_press_handler_id = 0;

static gboolean chans_key_press_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  file_data *fd = (file_data *)data;
  clear_dialog_error(fd);
  if (chans_key_press_handler_id)
    {
      g_signal_handler_disconnect(fd->chans_text, chans_key_press_handler_id);
      chans_key_press_handler_id = 0;
    }
  return(false);
}

static void clear_error_if_chans_changes(GtkWidget *dialog, void *data)
{
  file_data *fd = (file_data *)data;
  if (fd->chans_text) 
    chans_key_press_handler_id = SG_SIGNAL_CONNECT(fd->chans_text, "key_press_event", chans_key_press_callback, data);
}

static gulong panel_modify_handler_id = 0;

static gboolean panel_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  file_data *fd = (file_data *)data;
  clear_dialog_error(fd);
  if (panel_modify_handler_id)
    {
      g_signal_handler_disconnect(w, panel_modify_handler_id);
      panel_modify_handler_id = 0;
    }
  return(false);
}

static void clear_error_if_panel_changes(GtkWidget *dialog, void *data)
{
  file_data *fd = (file_data *)data;
  GtkWidget *baddy;
  switch (fd->error_widget)
    {
    case SRATE_WIDGET:         baddy = fd->srate_text;    break;
    case DATA_LOCATION_WIDGET: baddy = fd->location_text; break;
    case SAMPLES_WIDGET:       baddy = fd->samples_text;  break;
    default:                   baddy = fd->chans_text;    break;
    }
  if (baddy) 
    panel_modify_handler_id = SG_SIGNAL_CONNECT(baddy, "key_press_event", panel_modify_callback, data);
}

static void post_file_panel_error(const char *error_msg, void *ufd)
{
  file_data *fd = (file_data *)ufd;
  fd->error_widget = fd->scanf_widget;
  post_file_dialog_error(error_msg, ufd);
}



/* -------- file data choices -------- */

static void update_header_type_list(GtkTreeSelection *selection, gpointer gp)
{
  /* needed to reflect type selection in format list */
  file_data *fd = (file_data *)gp;
  GtkTreeIter iter;
  gchar *value = NULL;
  int i;
  GtkTreeModel *model;

  if ((!(fd->format_list)) || (!(fd->header_list))) return;
  /* sg_list_select in make_file_data_panel can trigger this before we have finished making the file data panel */
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;

  gtk_tree_model_get(model, &iter, 0, &value, -1);
  for (i = 0; i < fd->num_header_types; i++)
    if (strcmp(value, fd->header_short_names[i]) == 0)
      {
	fd->header_pos = i;
	if (fd->current_type != i)
	  {
	    set_header_type_and_format_from_position(fd, i);
	    set_file_dialog_sound_attributes(fd,
					     fd->current_type,
					     fd->current_format,
					     IGNORE_SRATE, IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
					     NULL);
	  }
	g_free(value);
	return;
      }
  if (value) g_free(value);
}

static void update_data_format_list(GtkTreeSelection *selection, gpointer gp)
{
  GtkTreeIter iter;
  gchar *value = NULL;
  int i;
  GtkTreeModel *model;
  int nformats = 0;
  char **formats = NULL;
  file_data *fd = (file_data *)gp;

  if (!(fd->format_list)) return;
  /* sg_list_select in make_file_data_panel can trigger this before we have finished making the file data panel */
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;

  gtk_tree_model_get(model, &iter, 0, &value, -1);
  formats = set_header_positions_from_type(fd, fd->current_type, fd->current_format);
  nformats = fd->formats;
  for (i = 0; i < nformats; i++)
    if (strcmp(value, formats[i]) == 0)
      {
	fd->format_pos = i;
	fd->current_format = data_format_from_position(fd->header_pos, i);
      }
  if (value) g_free(value);
}

static void s8_callback(GtkWidget *w, gpointer context) 
{
  file_data *fd = (file_data *)context;
  if (fd->srate_text)
    gtk_entry_set_text(GTK_ENTRY(fd->srate_text), "8000");
}

static void s22_callback(GtkWidget *w, gpointer context) 
{
  file_data *fd = (file_data *)context;
  if (fd->srate_text)
    gtk_entry_set_text(GTK_ENTRY(fd->srate_text), "22050");
}

static void s44_callback(GtkWidget *w, gpointer context) 
{
  file_data *fd = (file_data *)context;
  if (fd->srate_text)
    gtk_entry_set_text(GTK_ENTRY(fd->srate_text), "41000");
}

static void s48_callback(GtkWidget *w, gpointer context) 
{
  file_data *fd = (file_data *)context;
  if (fd->srate_text)
    gtk_entry_set_text(GTK_ENTRY(fd->srate_text), "48000");
}

static void c1_callback(GtkWidget *w, gpointer context) 
{
  file_data *fd = (file_data *)context;
  if (fd->chans_text)
    gtk_entry_set_text(GTK_ENTRY(fd->chans_text), "1");
}

static void c2_callback(GtkWidget *w, gpointer context) 
{
  file_data *fd = (file_data *)context;
  if (fd->chans_text)
    gtk_entry_set_text(GTK_ENTRY(fd->chans_text), "2");
}

static void c3_callback(GtkWidget *w, gpointer context) 
{
  file_data *fd = (file_data *)context;
  if (fd->chans_text)
    gtk_entry_set_text(GTK_ENTRY(fd->chans_text), (fd->extracting) ? "3" : "4");
}

static void c4_callback(GtkWidget *w, gpointer context) 
{
  file_data *fd = (file_data *)context;
  if (fd->chans_text)
    gtk_entry_set_text(GTK_ENTRY(fd->chans_text), (fd->extracting) ? "4" : "8");
}

file_data *make_file_data_panel(GtkWidget *parent, char *name, 
				dialog_channels_t with_chan, 
				int header_type, int data_format,
				dialog_data_location_t with_loc, 
				dialog_samples_t with_samples,
				dialog_error_t with_error, 
				dialog_header_type_t with_header_type,
				dialog_comment_t with_comment, 
				header_choice_t header_choice)
{
  GtkWidget *form, *scbox, *combox = NULL;
  file_data *fdat;
  int nformats = 0, nheaders = 0;
  char **formats = NULL, **headers = NULL;
  GtkWidget *s8, *s22, *s44, *s48;
  GtkWidget *sbar, *sitem, *smenu;

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
  fdat->header_short_names = headers;
  fdat->num_header_types = nheaders;

  form = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(parent), form, false, false, 4); /* ??? */
  gtk_widget_show(form);

  /* header type */
  if (with_header_type == WITH_HEADER_TYPE_FIELD)
    {
      fdat->header_list = sg_make_list(_("header"), form, BOX_PACK, (gpointer)fdat, 
				       nheaders, headers, 
				       GTK_SIGNAL_FUNC(update_header_type_list), 0, 0, 0, 0);
      /* header_list is a gtk list */
      sg_list_select(fdat->header_list, fdat->header_pos);
      gtk_widget_show(fdat->header_list);
    }

  /* data format */
  fdat->format_list = sg_make_list(_("data"), form, BOX_PACK, (gpointer)fdat, 
				   nformats, formats, 
				   GTK_SIGNAL_FUNC(update_data_format_list), 0, 0, 0, 0);
  sg_list_select(fdat->format_list, fdat->format_pos);
  gtk_widget_show(fdat->format_list);

  /* srate */
  scbox = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(form), scbox, false, false, 4);
  gtk_widget_show(scbox);

  /* srate label is drop-down menu */
  sbar = gtk_menu_bar_new();
  gtk_box_pack_start(GTK_BOX(scbox), sbar, false, false, 0);
  gtk_widget_show(sbar);

  smenu = gtk_menu_new();
  s8 = gtk_menu_item_new_with_label("8000");
  s22 = gtk_menu_item_new_with_label("22050");
  s44 = gtk_menu_item_new_with_label("44100");
  s48 = gtk_menu_item_new_with_label("48000");

  gtk_menu_shell_append(GTK_MENU_SHELL(smenu), s8);
  gtk_menu_shell_append(GTK_MENU_SHELL(smenu), s22);
  gtk_menu_shell_append(GTK_MENU_SHELL(smenu), s44);
  gtk_menu_shell_append(GTK_MENU_SHELL(smenu), s48);

  gtk_widget_show(s8);
  gtk_widget_show(s22);
  gtk_widget_show(s44);
  gtk_widget_show(s48);

  sitem = gtk_menu_item_new_with_label(_("srate:"));
  gtk_widget_show(sitem);
  gtk_menu_item_set_submenu(GTK_MENU_ITEM(sitem), smenu);
  gtk_menu_shell_append(GTK_MENU_SHELL(sbar), sitem);

  fdat->srate_text = snd_entry_new(scbox, true);
  SG_SIGNAL_CONNECT(fdat->srate_text, "key_press_event", data_panel_srate_key_press, NULL);

  SG_SIGNAL_CONNECT(s8,  "activate", s8_callback, (gpointer)fdat);
  SG_SIGNAL_CONNECT(s22,  "activate", s22_callback, (gpointer)fdat);
  SG_SIGNAL_CONNECT(s44,  "activate", s44_callback, (gpointer)fdat);
  SG_SIGNAL_CONNECT(s48,  "activate", s48_callback, (gpointer)fdat);

  /* chans */
  /* chan also a drop-down menu */
  if (with_chan != WITHOUT_CHANNELS_FIELD)
    {
      GtkWidget *c1, *c2, *c3, *c4;
      GtkWidget *cbar, *citem, *cmenu;

      cbar = gtk_menu_bar_new();
      gtk_box_pack_start(GTK_BOX(scbox), cbar, false, false, 0);
      gtk_widget_show(cbar);

      cmenu = gtk_menu_new();
      c1 = gtk_menu_item_new_with_label("1");
      c2 = gtk_menu_item_new_with_label("2");
      if (with_chan == WITH_CHANNELS_FIELD)
	{
	  c3 = gtk_menu_item_new_with_label("4");
	  c4 = gtk_menu_item_new_with_label("8");
	  fdat->extracting = false;
	}
      else
	{
	  c3 = gtk_menu_item_new_with_label("3");
	  c4 = gtk_menu_item_new_with_label("4");
	  fdat->extracting = true;
	}

      gtk_menu_shell_append(GTK_MENU_SHELL(cmenu), c1);
      gtk_menu_shell_append(GTK_MENU_SHELL(cmenu), c2);
      gtk_menu_shell_append(GTK_MENU_SHELL(cmenu), c3);
      gtk_menu_shell_append(GTK_MENU_SHELL(cmenu), c4);

      gtk_widget_show(c1);
      gtk_widget_show(c2);
      gtk_widget_show(c3);
      gtk_widget_show(c4);

      citem = gtk_menu_item_new_with_label((gchar *)((with_chan == WITH_CHANNELS_FIELD) ? _("channels:") : _("extract channel:")));
      gtk_widget_show(citem);
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(citem), cmenu);
      gtk_menu_shell_append(GTK_MENU_SHELL(cbar), citem);

      fdat->chans_text = snd_entry_new(scbox, true);

      SG_SIGNAL_CONNECT(c1,  "activate", c1_callback, (gpointer)fdat);
      SG_SIGNAL_CONNECT(c2,  "activate", c2_callback, (gpointer)fdat);
      SG_SIGNAL_CONNECT(c3,  "activate", c3_callback, (gpointer)fdat);
      SG_SIGNAL_CONNECT(c4,  "activate", c4_callback, (gpointer)fdat);
      
      if (with_loc == WITH_DATA_LOCATION_FIELD)
	{
	  GtkWidget *loclab;
	  loclab = snd_gtk_label_new(_("location:"), ss->sgx->highlight_color);
	  gtk_box_pack_start(GTK_BOX(scbox), loclab, false, false, 0);
	  gtk_widget_show(loclab);

	  fdat->location_text = snd_entry_new(scbox, true);
	}
    }

  /* samples */
  if (with_samples == WITH_SAMPLES_FIELD)
    {
      GtkWidget *samplab;
      samplab = snd_gtk_label_new(_("samples:"), ss->sgx->highlight_color);
      gtk_box_pack_start(GTK_BOX(scbox), samplab, false, false, 0);
      gtk_widget_show(samplab);

      fdat->samples_text = snd_entry_new(scbox, true);
    }
  else
    {
      /* need a spacer to force the lists to have room */
      GtkWidget *spacer;
      spacer = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(scbox), spacer, false, false, 40);
      gtk_widget_show(spacer);
    }

  /* comment */
  if (with_comment != WITHOUT_COMMENT_FIELD)
    {
      GtkWidget *frame, *comment_label;
      if (with_comment == WITH_COMMENT_FIELD) /* i.e. not WITH_UNLABELLED_COMMENT_FIELD */
	{
	  combox = gtk_hbox_new(false, 0);
	  gtk_box_pack_start(GTK_BOX(parent), combox, true, true, 4);
	  gtk_widget_show(combox);

	  comment_label = snd_gtk_label_new(_("comment:"), ss->sgx->highlight_color);
	  gtk_entry_set_width_chars(GTK_ENTRY(comment_label), 2 + strlen(_("comment:")));
	  gtk_box_pack_start(GTK_BOX(combox), comment_label, false, false, 0);
	  gtk_widget_show(comment_label);
	}

      frame = gtk_frame_new(NULL);
      if (with_comment == WITH_COMMENT_FIELD) /* i.e. not WITH_UNLABELLED_COMMENT_FIELD */
	gtk_box_pack_start(GTK_BOX(combox), frame, true, true, 4);  
      else gtk_box_pack_start(GTK_BOX(parent), frame, true, true, 4);  
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
      gtk_widget_show(frame);
      fdat->comment_text = make_scrolled_text(frame, true, NULL, NULL);
      connect_mouse_to_text(fdat->comment_text);
    }

  /* error */
  if (with_error == WITH_ERROR_FIELD)
    {
      fdat->error_text = snd_gtk_label_new(NULL, ss->sgx->highlight_color);
      gtk_box_pack_end(GTK_BOX(parent), fdat->error_text, false, false, 0);
      gtk_widget_hide(fdat->error_text);
    }

  return(fdat);
}


/* -------- save as dialog (file and edit menus) -------- */

typedef struct {
  file_data *panel_data;
  GtkWidget *dialog, *filename_widget;
  char *filename;
  save_dialog_t type;
  /*  file_pattern_info *fp; */
  dialog_play_info *dp;
  fam_info *file_watcher;
  int selection_watcher_loc;
  gulong filename_watcher_id;
} save_as_dialog_info;

static save_as_dialog_info *save_sound_as = NULL, *save_selection_as = NULL;

static save_as_dialog_info *new_save_as_dialog_info(save_dialog_t type)
{
  save_as_dialog_info *sd;
  sd = (save_as_dialog_info *)CALLOC(1, sizeof(save_as_dialog_info));
  sd->type = type;
  sd->selection_watcher_loc = -1;
  sd->filename_watcher_id = 0;
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

static void save_as_undoit(save_as_dialog_info *sd)
{
#if HAVE_GFCDN
  {
    GtkWidget *ok_button;
    ok_button = file_chooser_button(GTK_FILE_CHOOSER_DIALOG(sd->dialog), GTK_RESPONSE_OK);
    if (ok_button)
      gtk_button_set_label(GTK_BUTTON(ok_button), GTK_STOCK_SAVE);
  }
#else
  set_button_label((GTK_FILE_SELECTION(sd->dialog))->ok_button, _("Save"));
#endif
  if ((sd->filename_watcher_id > 0) && (sd->filename_widget))
    {
      g_signal_handler_disconnect(sd->filename_widget, sd->filename_watcher_id);
      sd->filename_watcher_id = 0;
    }
  clear_dialog_error(sd->panel_data);
  sd->file_watcher = fam_unmonitor_file(sd->filename, sd->file_watcher);
}

static void save_as_filename_modify_callback(GtkWidget *w, gpointer context)
{
  save_as_undoit((save_as_dialog_info *)context);
}

static void clear_error_if_save_as_filename_changes(GtkWidget *dialog, gpointer data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  if (sd->filename_widget)
    sd->filename_watcher_id = SG_SIGNAL_CONNECT(sd->filename_widget, "changed", save_as_filename_modify_callback, data);
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
/* TODO: in gtk filesel if out name exists, you have to click save twice! -- how to disable this stupidity? */

static void save_as_ok_callback(GtkWidget *w, gpointer data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
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
  str = snd_filer_get_filename(sd->dialog);
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
      if (str) FREE(str);
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
      if (str) FREE(str);
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
	  if (str) FREE(str);
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
	      msg = mus_format(_("%s exists%s. To overwrite it, click '%s'"), 
			       str,
			       (parlous_sp) ? ", and has unsaved edits" : "",
#if HAVE_GFCDN
			       "Yes"
#else
			       "DoIt"
#endif
			       );
	      sd->file_watcher = fam_monitor_file(fullname, (void *)sd, watch_save_as_file);
	      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
	      clear_error_if_save_as_filename_changes(sd->dialog, (void *)(sd->panel_data));
#if HAVE_GFCDN
	      {
		GtkWidget *ok_button;
		ok_button = file_chooser_button(GTK_FILE_CHOOSER_DIALOG(sd->dialog), GTK_RESPONSE_OK);
		if (ok_button)
		  gtk_button_set_label(GTK_BUTTON(ok_button), GTK_STOCK_YES);
	      }
#else
	      set_button_label((GTK_FILE_SELECTION(sd->dialog))->ok_button, _("DoIt"));
#endif
	      FREE(msg);
	      FREE(fullname);
	      if (comment) FREE(comment);
	      if (str) FREE(str);
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
      if (file_exists)
	ofile = snd_tempnam();
      else ofile = copy_string(fullname);
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

      if (sd->type == SOUND_SAVE_AS)
	report_in_minibuffer(sp, "%s saved as %s", sp->short_filename, str);
      else report_in_minibuffer(sp, "selection saved as %s", str);
      run_after_save_as_hook(sp, str, true); /* true => from dialog */
      gtk_widget_hide(sd->dialog);
    }
  else
    {
      msg = mus_format("save as %s error: %s", str, io_error_name(io_err));
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));
      FREE(msg);
    }
  FREE(fullname);
  if (comment) FREE(comment);
  if (str) FREE(str);
} 

static void save_as_extract_callback(GtkWidget *w, gpointer data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
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
  str = snd_filer_get_filename(sd->dialog);
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
      if (str) FREE(str);
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
      if (str) FREE(str);
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
      if (str) FREE(str);
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
	  if (str) FREE(str);
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
	      msg = mus_format(_("%s exists%s. To overwrite it, click '%s'"), 
			       str,
			       (parlous_sp) ? ", and has unsaved edits" : "",
#if HAVE_GFCDN
			       "Yes"
#else
			       "DoIt"
#endif
			       );
	      sd->file_watcher = fam_monitor_file(fullname, (void *)sd, watch_save_as_file);
	      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
	      clear_error_if_save_as_filename_changes(sd->dialog, (void *)(sd->panel_data));
#if HAVE_GFCDN
	      {
		GtkWidget *ok_button;
		ok_button = file_chooser_button(GTK_FILE_CHOOSER_DIALOG(sd->dialog), GTK_RESPONSE_OK);
		if (ok_button)
		  gtk_button_set_label(GTK_BUTTON(ok_button), GTK_STOCK_YES);
	      }
#else
	      set_button_label((GTK_FILE_SELECTION(sd->dialog))->ok_button, _("DoIt"));
#endif
	      FREE(msg);
	      FREE(fullname);
	      if (comment) FREE(comment);
	      if (str) FREE(str);
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

      if (sd->type == SOUND_SAVE_AS)
	report_in_minibuffer(sp, "%s chan %d saved as %s", sp->short_filename, chan, str);
      else report_in_minibuffer(sp, "selection chan %d saved as %s", chan, str);
      run_after_save_as_hook(sp, str, true); /* true => from dialog */
      gtk_widget_hide(sd->dialog);
    }
  else
    {
      msg = mus_format("extract chan as %s error: %s", str, io_error_name(io_err));
      post_file_dialog_error((const char *)msg, (void *)(sd->panel_data));
      clear_error_if_filename_changes(sd->dialog, (void *)(sd->panel_data));
      FREE(msg);
    }
  FREE(fullname);
  if (comment) FREE(comment);
  if (str) FREE(str);
}

static void save_as_cancel_callback(GtkWidget *w, gpointer data)
{ 
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  gtk_widget_hide(sd->dialog);
} 

static void save_as_help_callback(GtkWidget *w, gpointer data)
{
  save_as_dialog_help();
}

static gint save_as_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  gtk_widget_hide(sd->dialog);
  return(true);
}

/* TODO: if "not found" error, use fam to watch for creation and erase message */

#if (!HAVE_GFCDN)
static void save_as_file_exists_check(GtkWidget *w, gpointer context)
{
  /* a "changed" callback for the text field */
  char *msg;
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  GtkWidget *label, *entry;
  entry = (GTK_FILE_SELECTION(sd->dialog))->selection_entry;
  if (entry)
    {
      label = (GTK_FILE_SELECTION(sd->dialog))->selection_text;
      char *filename = NULL;
      filename = (char *)gtk_file_selection_get_filename(GTK_FILE_SELECTION(sd->dialog));
      if ((filename) && (*filename))
	{
	  if ((mus_file_probe(filename)) && 
	      (!directory_p(filename)))
	    {
#if HAVE_ACCESS
	      if (access(filename, W_OK) < 0)
		msg = _("save as (file write-protected?):");
	      else
#endif
		msg = _("save as (overwriting):");
	    }
	  else
	    {
	      if (!(directory_exists(filename)))
		msg = _("save as (no such directory?):");
	      else msg = _("save as:");
	    }
	}
      else msg = _("save as:");
      gtk_label_set_text(GTK_LABEL(label), msg);
    }
}
#endif

static void make_save_as_dialog(save_as_dialog_info *sd, char *sound_name, int header_type, int format_type)
{
  char *file_string;

  file_string = mus_format(_("save %s"), sound_name);
  if (!(sd->dialog))
    {
      GtkWidget *fbox;
      sd->dialog = snd_filer_new(file_string, true,
				 NULL, /* handle delete later */
				 (Callback_Func)save_as_ok_callback,
				 (Callback_Func)save_as_cancel_callback,
				 (Callback_Func)save_as_help_callback,
				 (Callback_Func)save_as_extract_callback,
				 (gpointer)sd); /* needs fixup below */
      fbox = gtk_vbox_new(false, 0);
#if HAVE_GFCDN
      gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(sd->dialog), fbox);
#else
      gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(sd->dialog)->main_vbox), fbox, true, true, 0);
      SG_SIGNAL_CONNECT((GTK_FILE_SELECTION(sd->dialog))->selection_entry, "changed", save_as_file_exists_check, (gpointer)sd);
      sd->filename_widget = (GTK_FILE_SELECTION(sd->dialog))->selection_entry;
#endif

      gtk_widget_show(fbox);
      sd->panel_data = make_file_data_panel(fbox, "data-form", 
					    WITH_EXTRACT_CHANNELS_FIELD, 
					    header_type, format_type, 
					    WITHOUT_DATA_LOCATION_FIELD, 
					    WITHOUT_SAMPLES_FIELD,
					    WITH_ERROR_FIELD, 
					    WITH_HEADER_TYPE_FIELD, 
					    WITH_COMMENT_FIELD,
					    WITH_WRITABLE_HEADERS);
      sd->panel_data->dialog = sd->dialog;
      if (sd->type == SOUND_SAVE_AS)
	set_dialog_widget(SOUND_SAVE_AS_DIALOG, sd->dialog);
      else set_dialog_widget(SELECTION_SAVE_AS_DIALOG, sd->dialog);
#if HAVE_GFCDN
      {
	filer_response_t *fr;
	fr = (filer_response_t *)get_user_data(G_OBJECT(sd->dialog));
	fr->fd = (void *)sd;
      }
#else

#endif
      SG_SIGNAL_CONNECT(sd->dialog, "delete_event", save_as_delete_callback, (void *)sd);
    }
  else
    {
      gtk_window_set_title(GTK_WINDOW(sd->dialog), file_string);
    }
  FREE(file_string);
#if (!HAVE_GFCDN)
  {
    GtkWidget *label, *entry;
    entry = (GTK_FILE_SELECTION(sd->dialog))->selection_entry;
    if (entry)
      {
	label = (GTK_FILE_SELECTION(sd->dialog))->selection_text;
	file_string = mus_format(_("save %s as:"), sound_name);
	gtk_label_set_text(GTK_LABEL(label), file_string);
	FREE(file_string);
      }
  }
#endif
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
  if (managed) gtk_widget_show(sd->dialog);
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
  if (managed) gtk_widget_show(sd->dialog);
  return(sd->dialog);
}

void save_file_dialog_state(FILE *fd)
{
  if ((odat) && (GTK_WIDGET_VISIBLE(odat->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_open_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_open_file_dialog));
#endif
    }
  if ((mdat) && (GTK_WIDGET_VISIBLE(mdat->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_mix_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_mix_file_dialog));
#endif
    }
  if ((idat) && (GTK_WIDGET_VISIBLE(idat->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_insert_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_insert_file_dialog));
#endif
    }
  if ((save_sound_as) && (GTK_WIDGET_VISIBLE(save_sound_as->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_save_sound_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_save_sound_dialog));
#endif
    }
  if ((save_selection_as) && (GTK_WIDGET_VISIBLE(save_selection_as->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_save_selection_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME(S_save_selection_dialog));
#endif
    }
}



/* -------------------------------- Raw Data Dialog -------------------------------- */

typedef struct raw_info {
  GtkWidget *dialog;
  off_t location;
  file_data *rdat;
  bool read_only;
  bool selected;
  char *filename;
  char *help;
  open_requestor_t requestor;
  snd_info *sp;
  GtkWidget *requestor_dialog;
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
	    (!(GTK_WIDGET_VISIBLE(raw_infos[i]->dialog))))
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

static void raw_data_ok_callback(GtkWidget *w, gpointer context)
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
      gtk_widget_hide(rp->dialog);
    }
}

static void raw_data_cancel_callback(GtkWidget *w, gpointer context)
{
  raw_info *rp = (raw_info *)context;
  gtk_widget_hide(rp->dialog);
  if ((rp->requestor_dialog) && 
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    gtk_widget_show(rp->requestor_dialog);
}

static gint raw_data_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  raw_info *rp = (raw_info *)context;
  if ((rp->requestor_dialog) && 
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    gtk_widget_show(rp->requestor_dialog);
  return(true);
}

static void raw_data_reset_callback(GtkWidget *w, gpointer context) 
{
  raw_info *rp = (raw_info *)context;
  int raw_srate, raw_chans, raw_data_format;
  rp->location = 0;
  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */  
  set_file_dialog_sound_attributes(rp->rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, rp->location, 
				   IGNORE_SAMPLES, NULL);
  gtk_widget_hide(rp->rdat->error_text);
}

static void raw_data_help_callback(GtkWidget *w, gpointer context) 
{
  raw_info *rp = (raw_info *)context;
  raw_data_dialog_help(rp->help);
}

static void make_raw_data_dialog(raw_info *rp, const char *filename, const char *title)
{
  GtkWidget *resetB, *helpB, *cancelB, *okB;
  int raw_srate, raw_chans, raw_data_format;
 
  rp->dialog = snd_gtk_dialog_new();
  if (!title)
    gtk_window_set_title(GTK_WINDOW(rp->dialog), _("No header on file"));
  else gtk_window_set_title(GTK_WINDOW(rp->dialog), title);
  sg_make_resizable(rp->dialog);
  gtk_container_set_border_width(GTK_CONTAINER(rp->dialog), 10);
  gtk_window_resize(GTK_WINDOW(rp->dialog), 350, 260);
  gtk_widget_realize(rp->dialog);

  helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
  gtk_widget_set_name(helpB, "help_button");

  cancelB = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
  gtk_widget_set_name(cancelB, "quit_button");

  resetB = gtk_button_new_with_label(_("Reset"));
  gtk_widget_set_name(resetB, "reset_button");

  okB = gtk_button_new_from_stock(GTK_STOCK_OK);
  gtk_widget_set_name(okB, "doit_button");

  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(rp->dialog)->action_area), okB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(rp->dialog)->action_area), resetB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(rp->dialog)->action_area), cancelB, true, true, 10);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(rp->dialog)->action_area), helpB, true, true, 10);

  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */

  rp->rdat = make_file_data_panel(GTK_DIALOG(rp->dialog)->vbox, "data-form", 
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

  SG_SIGNAL_CONNECT(rp->dialog, "delete_event", raw_data_delete_callback, rp);
  SG_SIGNAL_CONNECT(okB, "clicked", raw_data_ok_callback, rp);

  SG_SIGNAL_CONNECT(helpB, "clicked", raw_data_help_callback, rp);
  SG_SIGNAL_CONNECT(resetB, "clicked", raw_data_reset_callback, rp);
  SG_SIGNAL_CONNECT(cancelB, "clicked", raw_data_cancel_callback, rp);

  gtk_widget_show(okB);
  gtk_widget_show(cancelB);
  gtk_widget_show(helpB);
  gtk_widget_show(resetB);

  set_dialog_widget(RAW_DATA_DIALOG, rp->dialog);
}

void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, bool read_only, bool selected)
{
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
    gtk_widget_hide(ss->sgx->requestor_dialog);
  if (!title) 
    title = mus_format("no header found on %s\n", filename);
  if (!rp->dialog) 
    make_raw_data_dialog(rp, filename, title);
  else gtk_window_set_title(GTK_WINDOW(rp->dialog), title);
  FREE(title);
  if (rp->help) FREE(rp->help);
  if (info)
    {
      rp->help = copy_string(info);
      FREE(info);
    }
  else rp->help = NULL;
  raise_dialog(rp->dialog);
  gtk_widget_show(rp->dialog);
}


/* -------------------------------- New File -------------------------------- */

static GtkWidget *new_file_dialog = NULL, *new_file_text = NULL, *new_file_ok_button = NULL;
static file_data *ndat = NULL;
static off_t initial_samples = 1;
static char *new_file_filename = NULL;
static fam_info *new_file_watcher = NULL;

void cleanup_new_file_watcher(void)
{
  new_file_watcher = fam_unmonitor_file(new_file_filename, new_file_watcher);
}

static gulong new_file_handler_id = 0;
static gboolean new_filename_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data);

static void new_file_undoit(void)
{
  clear_dialog_error(ndat);
  if (new_file_handler_id)
    {
      if (new_file_handler_id)
	{
	  g_signal_handler_disconnect(new_file_text, new_file_handler_id);
	  new_file_handler_id = 0;
	}
    }
  set_button_label(new_file_ok_button, _("Ok"));
  new_file_watcher = fam_unmonitor_file(new_file_filename, new_file_watcher);
}

static gboolean new_filename_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  new_file_undoit();
  return(false);
}

static void clear_error_if_new_filename_changes(GtkWidget *dialog, void *data)
{
  if (new_file_text)
    new_file_handler_id = SG_SIGNAL_CONNECT(new_file_text, "key_press_event", new_filename_modify_callback, data);
}

static void watch_new_file(struct fam_info *fp, FAMEvent *fe)
{
  /* if file is deleted, respond in some debonair manner */
#if HAVE_FAM
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

static void new_file_ok_callback(GtkWidget *w, gpointer context) 
{
  off_t loc;
  char *comment = NULL, *newer_name = NULL, *msg;
  int header_type, data_format, srate, chans;
  newer_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_text));
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
	      msg = mus_format(_("%s exists. If you want to overwrite it, click 'DoIt'"), newer_name);
	      new_file_watcher = fam_monitor_file(new_file_filename, NULL, watch_new_file);
	      set_button_label(new_file_ok_button, _("DoIt"));
	      post_file_dialog_error((const char *)msg, (void *)ndat);
	      clear_error_if_new_filename_changes(new_file_dialog, (void *)ndat);
	      FREE(msg);
	    }
	  else
	    {
	      if (new_file_watcher)
		new_file_undoit();
	      ss->local_errno = 0;
	      redirect_snd_error_to(post_file_dialog_error, (void *)ndat);
	      sp = snd_new_file(newer_name, header_type, data_format, srate, chans, comment, initial_samples);
	      redirect_snd_error_to(NULL, NULL);
	      if (!sp)
		{
		  if ((ss->local_errno) &&
		      (mus_file_probe(new_file_filename))) /* see comment in snd-xfile.c */
		    new_file_watcher = fam_monitor_file(new_file_filename, NULL, watch_new_file);
		  clear_error_if_new_filename_changes(new_file_dialog, (void *)ndat);
		}
	      else
		{
		  gtk_widget_hide(new_file_dialog);
		}
	    }
	}
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
  gtk_entry_set_text(GTK_ENTRY(new_file_text), filename);  
  mus_sound_forget(filename);

  set_file_dialog_sound_attributes(ndat, header_type, data_format, srate, chans, IGNORE_DATA_LOCATION, initial_samples, new_comment);

  if (new_comment) FREE(new_comment);
  if (filename) FREE(filename);
}

static void new_file_cancel_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(new_file_dialog);
}

static void new_file_reset_callback(GtkWidget *w, gpointer context)
{
  char *current_name;
  current_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_text));
  load_new_file_defaults(current_name);
  if (new_file_watcher)
    new_file_undoit();
}

static gint new_file_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  gtk_widget_hide(new_file_dialog);
  return(true);
}

static void new_file_help_callback(GtkWidget *w, gpointer context) 
{
  new_file_dialog_help();
}

void make_new_file_dialog(void)
{
  char *newname;
  if (!new_file_dialog)
    {
      GtkWidget *name_label, *hform, *help_button, *cancel_button, *reset_button;
      new_file_dialog = snd_gtk_dialog_new();
      gtk_window_set_title(GTK_WINDOW(new_file_dialog), _("New file"));
      sg_make_resizable(new_file_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(new_file_dialog), 10);
      gtk_window_resize(GTK_WINDOW(new_file_dialog), 400, 250);
      gtk_widget_realize(new_file_dialog);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
      gtk_widget_set_name(cancel_button, "quit_button");

      new_file_ok_button = gtk_button_new_with_label(_("Ok"));
      gtk_widget_set_name(new_file_ok_button, "doit_button");

      reset_button = gtk_button_new_with_label(_("Reset"));
      gtk_widget_set_name(reset_button, "reset_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_file_dialog)->action_area), new_file_ok_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_file_dialog)->action_area), reset_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_file_dialog)->action_area), cancel_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(new_file_dialog)->action_area), help_button, true, true, 10);

      hform = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_file_dialog)->vbox), hform, false, false, 4);
      gtk_widget_show(hform);

      name_label = gtk_label_new(_("New file:"));
      gtk_box_pack_start(GTK_BOX(hform), name_label, false, false, 2);
      gtk_widget_show(name_label);

      new_file_text = snd_entry_new(hform, true);

      newname = output_name(NULL); /* fix later */
      if ((newname) && (*newname))
	gtk_entry_set_text(GTK_ENTRY(new_file_text), newname); /* output_name?? fix later */

      ndat = make_file_data_panel(GTK_DIALOG(new_file_dialog)->vbox, "data-form", 
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

      SG_SIGNAL_CONNECT(new_file_dialog, "delete_event", new_file_delete_callback, ndat);
      SG_SIGNAL_CONNECT(cancel_button, "clicked", new_file_cancel_callback, ndat);
      SG_SIGNAL_CONNECT(help_button, "clicked", new_file_help_callback, ndat);
      SG_SIGNAL_CONNECT(new_file_ok_button, "clicked", new_file_ok_callback, ndat);
      SG_SIGNAL_CONNECT(reset_button, "clicked", new_file_reset_callback, ndat);
      SG_SIGNAL_CONNECT(new_file_text, "activate", new_file_ok_callback, ndat);

      gtk_widget_show(cancel_button);
      gtk_widget_show(new_file_ok_button);
      gtk_widget_show(reset_button);
      gtk_widget_show(help_button);

      set_dialog_widget(NEW_FILE_DIALOG, new_file_dialog);
      load_new_file_defaults(NULL);
    }
#if (!HAVE_FAM)
  else
    {
      /* if overwrite question pends, but file has been deleted in the meantime, go back to normal state */
      if (new_file_watcher)
	{
	  char *new_name;
	  new_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_text));
	  if ((!new_name) || (!(*new_name)) ||
	      (!(mus_file_probe(new_name))))
	    new_file_undoit();
	}
    }
#endif
  gtk_widget_show(new_file_dialog);
}



/* ---------------- Edit Header ---------------- */

typedef struct edhead_info {
  GtkWidget *dialog, *save_button;
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
	    (!(GTK_WIDGET_VISIBLE(edhead_infos[i]->dialog))))
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

static char *make_header_dialog_title(edhead_info *ep, snd_info *sp)
{
  /* dialog may not yet exist */
  char *str;
  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (sp->user_read_only || sp->file_read_only)
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, _("Add header to (write-protected) %s"), sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of (write-protected) %s"), sp->short_filename);
      if (ep->dialog)
	set_sensitive(ep->save_button, (sp->hdr->type == MUS_RAW));
    }
  else 
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, _("Add header to %s"), sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of %s"), sp->short_filename);
      if (ep->dialog)
	set_sensitive(ep->save_button, ep->panel_changed);
    }
  return(str);
}

static void edit_header_help_callback(GtkWidget *w, gpointer context) 
{
  edit_header_dialog_help();
}

static void edit_header_set_ok_sensitive(GtkWidget *w, gpointer context) 
{
  edhead_info *ep = (edhead_info *)context;
  if (!(ep->sp->file_read_only))
    gtk_widget_set_sensitive(ep->save_button, true);
  ep->panel_changed = true;
}

static void edit_header_done(edhead_info *ep)
{
  gtk_widget_hide(ep->dialog);
  unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
  remove_sp_watcher(ep->sp, ep->sp_ro_watcher_loc);
  ep->panel_changed = false;
  ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
}

static void edit_header_cancel_callback(GtkWidget *w, gpointer context) 
{
  edit_header_done((edhead_info *)context);
}

static gint edit_header_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  edit_header_done((edhead_info *)context);
  return(true);
}

static void edit_header_watch_user_read_only(struct snd_info *sp, sp_watcher_reason_t reason, int loc)
{
  edhead_info *ep;
  ep = (edhead_info *)(sp->watchers[loc]->context);
  if ((ep->dialog) && 
      (GTK_WIDGET_VISIBLE(ep->dialog)) &&
      (sp == ep->sp))
    {
      if (reason == SP_READ_ONLY_CHANGED)
	{
	  char *title;
	  if ((!(sp->file_read_only)) && (!(sp->user_read_only)))
	    clear_dialog_error(ep->edat);
	  title = make_header_dialog_title(ep, sp);
	  gtk_window_set_title(GTK_WINDOW(ep->dialog), title);
	  FREE(title);
	}
      else /* sound closing, so we shouldn't sit around offering to edit its header */
	{
	  clear_dialog_error(ep->edat);
	  gtk_widget_hide(ep->dialog);
	  if (ep->panel_changed)
	    unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
	  ep->panel_changed = false;
 	  ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
	  remove_sp_watcher(ep->sp, ep->sp_ro_watcher_loc);
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
	char *title;
	if (mus_file_probe(sp->filename))
	  {
	    err = access(sp->filename, W_OK);
	    sp->file_read_only = (err < 0);
	    if ((!(sp->file_read_only)) && (!(sp->user_read_only)))
	      clear_dialog_error(ep->edat);
	    title = make_header_dialog_title(ep, sp);
	    gtk_window_set_title(GTK_WINDOW(ep->dialog), title);
	    FREE(title);
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
      gtk_widget_hide(ep->dialog);
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

static void edit_header_ok_callback(GtkWidget *w, gpointer context) 
{
  edhead_info *ep = (edhead_info *)context;
  if ((ep->sp) && (ep->sp->active))
    {
      bool ok;
      redirect_snd_error_to(post_file_dialog_error, (void *)(ep->edat));
      ok = edit_header_callback(ep->sp, ep->edat, post_file_dialog_error, post_file_panel_error);
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
	      set_sensitive(ep->save_button, false);
	      return;
	    }
	}
      remove_sp_watcher(ep->sp, ep->sp_ro_watcher_loc);
      ep->file_ro_watcher = fam_unmonitor_file(ep->sp->filename, ep->file_ro_watcher);
      gtk_widget_hide(ep->dialog);
      unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
    }
}

GtkWidget *edit_header(snd_info *sp)
{
  char *str;
  file_info *hdr;
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

  if (!ep->dialog)
    {
      GtkWidget *help_button, *cancel_button;
      ep->dialog = snd_gtk_dialog_new();
      /* gtk_window_set_title(GTK_WINDOW(ep->dialog), _("Edit Header")); */
      sg_make_resizable(ep->dialog);
      gtk_container_set_border_width (GTK_CONTAINER(ep->dialog), 10);
      gtk_window_resize(GTK_WINDOW(ep->dialog), 400, 250);
      gtk_widget_realize(ep->dialog);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
      gtk_widget_set_name(cancel_button, "quit_button");

      ep->save_button = gtk_button_new_from_stock(GTK_STOCK_SAVE);
      gtk_widget_set_name(ep->save_button, "doit_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(ep->dialog)->action_area), cancel_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(ep->dialog)->action_area), ep->save_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(ep->dialog)->action_area), help_button, true, true, 10);

      ep->edat = make_file_data_panel(GTK_DIALOG(ep->dialog)->vbox, _("Edit Header"), 
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

      SG_SIGNAL_CONNECT(ep->dialog, "delete_event", edit_header_delete_callback, ep);
      SG_SIGNAL_CONNECT(cancel_button, "clicked", edit_header_cancel_callback, ep);
      SG_SIGNAL_CONNECT(help_button, "clicked", edit_header_help_callback, ep);
      SG_SIGNAL_CONNECT(ep->save_button, "clicked", edit_header_ok_callback, ep);

      gtk_widget_show(cancel_button);
      gtk_widget_show(ep->save_button);
      gtk_widget_show(help_button);

      set_dialog_widget(EDIT_HEADER_DIALOG, ep->dialog);
    }
  else clear_dialog_error(ep->edat);

  str = make_header_dialog_title(ep, sp);
  gtk_window_set_title(GTK_WINDOW(ep->dialog), str);
  FREE(str);

  gtk_widget_set_sensitive(ep->save_button, (hdr->type == MUS_RAW));

  if (hdr->type == MUS_RAW)
    set_file_dialog_sound_attributes(ep->edat, 
				     default_output_header_type(ss), 
				     hdr->format, hdr->srate, hdr->chans, 
				     hdr->data_location, hdr->samples, hdr->comment);
  else set_file_dialog_sound_attributes(ep->edat, 
					hdr->type, hdr->format, hdr->srate, hdr->chans, 
					hdr->data_location, hdr->samples, hdr->comment);

  gtk_widget_show(ep->dialog);
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
	    (GTK_WIDGET_VISIBLE(ep->dialog)) && 
	    (snd_ok(ep->sp)))
	  {
#if HAVE_SCHEME
	    fprintf(fd, "(%s (%s \"%s\"))\n", S_edit_header_dialog, S_find_sound, ep->sp->short_filename);
#endif
#if HAVE_RUBY
	    fprintf(fd, "%s(%s(\"%s\"))\n", TO_PROC_NAME(S_edit_header_dialog), TO_PROC_NAME(S_find_sound), ep->sp->short_filename);
#endif
	  }
      }
}



/* ---------------- POST-IT MONOLOG ---------------- */

#define POST_IT_ROWS 12
#define POST_IT_COLUMNS 56

static GtkWidget *post_it_text = NULL, *post_it_dialog = NULL;

static void dismiss_post_it(GtkWidget *w, gpointer context) {gtk_widget_hide(post_it_dialog);}

static gint delete_post_it(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  gtk_widget_hide(post_it_dialog);
  return(true);
}

static void create_post_it_monolog(void)
{
  /* create scrollable but not editable text window */
  GtkWidget *ok_button;
  post_it_dialog = gtk_dialog_new();
  SG_SIGNAL_CONNECT(post_it_dialog, "delete_event", delete_post_it, NULL);

  gtk_window_set_title(GTK_WINDOW(post_it_dialog), _("Info"));
  sg_make_resizable(post_it_dialog);
  gtk_container_set_border_width(GTK_CONTAINER(post_it_dialog), 10);
  gtk_window_resize(GTK_WINDOW(post_it_dialog), POST_IT_COLUMNS * 9, POST_IT_ROWS * 20);
  gtk_widget_realize(post_it_dialog);

  ok_button = gtk_button_new_from_stock(GTK_STOCK_OK);
  gtk_widget_set_name(ok_button, "quit_button");

  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(post_it_dialog)->action_area), ok_button, false, true, 20);
  SG_SIGNAL_CONNECT(ok_button, "clicked", dismiss_post_it, NULL);
  gtk_widget_show(ok_button);

  post_it_text = make_scrolled_text(GTK_DIALOG(post_it_dialog)->vbox, false, NULL, NULL);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(post_it_text), 10);
  gtk_widget_show(post_it_dialog);
  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}

widget_t post_it(const char *subject, const char *str)
{
  if ((ss == NULL) || (ss->sgx == NULL)) return(NULL);
  if (!(post_it_dialog)) create_post_it_monolog(); else raise_dialog(post_it_dialog);
  gtk_window_set_title(GTK_WINDOW(post_it_dialog), subject);
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(post_it_text)), "", 0);
  sg_text_insert(post_it_text, (char *)str);
  return(post_it_dialog);
}

void save_post_it_dialog_state(FILE *fd)
{
  if ((post_it_dialog) && (GTK_WIDGET_VISIBLE(post_it_dialog)))
    {
      const gchar *subject;
      gchar *text;
      subject = gtk_window_get_title(GTK_WINDOW(post_it_dialog)); /* don't free subject! */
      text = sg_get_text(post_it_text, 0, -1);
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\" \"%s\")\n", S_info_dialog, subject, text);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\", \"%s\")\n", TO_PROC_NAME(S_info_dialog), subject, text);
#endif
      if (text) g_free(text);
    }
}



/* ---------------- mouse enter|leave-label ---------------- */

static XEN mouse_enter_label_hook;
static XEN mouse_leave_label_hook;

static gboolean mouse_name(XEN hook, GtkWidget *w, const char *caller)
{
  if (XEN_HOOKED(hook))
    {
      regrow *r;
      r = (regrow *)get_user_data(G_OBJECT(w));
      if (r)
	{
	  char *label = NULL;
	  if (r->parent == FILE_VIEWER)
	    label = get_view_files_full_name(r->pos);
	  else label = (char *)gtk_label_get_text(GTK_LABEL(GTK_BIN(w)->child));
	  if (label)
	    run_hook(hook,
		     XEN_LIST_3(C_TO_XEN_INT(r->parent),
				C_TO_XEN_INT(r->pos),
				C_TO_XEN_STRING(label)),
		     caller);
	}
    }
  return(false);
}

static gboolean label_enter_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer gp)
{
  return(mouse_name(mouse_enter_label_hook, w, S_mouse_enter_label_hook));
}

static gboolean label_leave_callback(GtkWidget *w, GdkEventCrossing *ev, gpointer gp)
{
  return(mouse_name(mouse_leave_label_hook, w, S_mouse_leave_label_hook));
}



/* -------- files browser and regions list widgetry -------- */
/*
 * the region and file browsers share much widgetry -- they are supposed to look the same
 */

static GtkWidget *byproc = NULL;
void set_view_files_sort_sensitive(bool sensitive)
{
  if (byproc)
    set_sensitive(byproc, sensitive);
}

ww_info *make_title_row(GtkWidget *formw, char *top_str, char *main_str, dialog_pad_t pad, dialog_sort_t with_sort, dialog_paned_t with_pane)
{
  GtkWidget *sep1, *cww;
  ww_info *wwi;
  wwi = (ww_info *)CALLOC(1, sizeof(ww_info));
  /* assuming "formw" is a vbox */
  if (main_str)
    {
      GtkWidget *rlw;
      rlw = snd_gtk_label_new(main_str, ss->sgx->highlight_color);
      gtk_box_pack_start(GTK_BOX(formw), rlw, false, false, 0);
      gtk_widget_show(rlw);

      sep1 = gtk_hseparator_new();
      gtk_box_pack_start(GTK_BOX(formw), sep1, false, false, 2);
      gtk_widget_modify_bg(sep1, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      gtk_widget_show(sep1);
    }
  
  if (with_pane == WITH_PANED_WINDOW)
    {
      GtkWidget *phbox;
      wwi->panes = gtk_vpaned_new();
      gtk_box_pack_start(GTK_BOX(formw), wwi->panes, true, true, 0);
      gtk_widget_show(wwi->panes);

      wwi->toppane = gtk_hbox_new(false, 0);
      gtk_paned_add1(GTK_PANED(wwi->panes), wwi->toppane);
      gtk_widget_show(wwi->toppane);

      phbox = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(wwi->toppane), phbox, true, true, 4);
      gtk_widget_show(phbox);

      formw = phbox;
    }
  else 
    {
      wwi->panes = formw;
      wwi->toppane = formw;
    }

  if (with_sort != WITH_SORT_BUTTON)
    {
      sep1 = gtk_vseparator_new(); /* not hsep -- damned thing insists on drawing a line */
      gtk_box_pack_start(GTK_BOX(formw), sep1, false, false, 2);
      gtk_widget_show(sep1);
    }

  wwi->tophbox = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(formw), wwi->tophbox, false, false, 4);
  gtk_widget_show(wwi->tophbox);

  wwi->plw = gtk_label_new(top_str); 
  gtk_box_pack_start(GTK_BOX(wwi->tophbox), wwi->plw, false, false, (pad == PAD_TITLE_ON_LEFT) ? 5 : 2);
  gtk_widget_show(wwi->plw);

  if (with_sort == WITH_SORT_BUTTON)
    {
      GtkWidget *sbar, *sitem, *smenu;
      sbar = gtk_menu_bar_new();
      gtk_box_pack_end(GTK_BOX(wwi->tophbox), sbar, false, false, 0);
      gtk_widget_show(sbar);

      smenu = gtk_menu_new();
      wwi->byname = gtk_menu_item_new_with_label(_("name"));
      wwi->bydate = gtk_menu_item_new_with_label(_("date"));
      wwi->bysize = gtk_menu_item_new_with_label(_("size"));
      wwi->byentry = gtk_menu_item_new_with_label(_("entry"));
      wwi->byproc = gtk_menu_item_new_with_label(_("proc"));

      gtk_menu_shell_append(GTK_MENU_SHELL(smenu), wwi->byname);
      gtk_menu_shell_append(GTK_MENU_SHELL(smenu), wwi->bydate);
      gtk_menu_shell_append(GTK_MENU_SHELL(smenu), wwi->bysize);
      gtk_menu_shell_append(GTK_MENU_SHELL(smenu), wwi->byentry);
      gtk_menu_shell_append(GTK_MENU_SHELL(smenu), wwi->byproc);

      gtk_widget_show(wwi->byname);
      gtk_widget_show(wwi->bydate);
      gtk_widget_show(wwi->bysize);
      gtk_widget_show(wwi->byentry);
      gtk_widget_show(wwi->byproc);
      byproc = wwi->byproc;

      sitem = gtk_menu_item_new_with_label(_("sort"));
      gtk_widget_show(sitem);
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(sitem), smenu);
      gtk_menu_shell_append(GTK_MENU_SHELL(sbar), sitem);
    }
  else
    {
      sep1 = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(formw), sep1, false, false, 2);
      gtk_widget_show(sep1);
    }

  wwi->list = gtk_vbox_new(false, 0);

  cww = gtk_scrolled_window_new(NULL, NULL);
  gtk_box_pack_start(GTK_BOX(formw), cww, true, true, 0);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cww), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(cww), wwi->list);

  gtk_widget_show(wwi->list);
  gtk_widget_show(cww);

  return(wwi);
}

regrow *make_regrow(GtkWidget *ww, GtkSignalFunc play_callback, GtkSignalFunc name_callback)
{
  regrow *r;
  r = (regrow *)CALLOC(1, sizeof(regrow));

  /* assume "ww" is a vbox widget in this case */
  r->rw = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(ww), r->rw, false, false, 0);
  gtk_widget_show(r->rw);

  r->pl = gtk_check_button_new();
  gtk_box_pack_start(GTK_BOX(r->rw), r->pl, false, false, 2);
  SG_SIGNAL_CONNECT(r->pl, "toggled", play_callback, r);
  gtk_widget_show(r->pl);

  r->nm = gtk_button_new_with_label("");
  sg_left_justify_button(r->nm);
  gtk_box_pack_start(GTK_BOX(r->rw), r->nm, true, true, 2);
  SG_SIGNAL_CONNECT(r->nm, "clicked", name_callback, r);
  SG_SIGNAL_CONNECT(r->nm, "enter_notify_event", label_enter_callback, r);
  SG_SIGNAL_CONNECT(r->nm, "leave_notify_event", label_leave_callback, r);
  set_user_data(G_OBJECT(r->nm), (gpointer)r);
  gtk_widget_show(r->nm);

  return(r);
}

/* -------- view files dialog -------- */

static GtkWidget *view_files_dialog = NULL;
static GtkWidget *vf_lst, *vf_ww;
static regrow **view_files_row = NULL;

void make_vf_row(int old_size, int new_size)
{
  if (view_files_row == NULL)
    view_files_row = (regrow **)CALLOC(new_size, sizeof(regrow *));
  else 
    {
      int i;
      view_files_row = (regrow **)REALLOC(view_files_row, new_size * sizeof(regrow *));
      for (i = old_size; i < new_size; i++) view_files_row[i] = NULL;
    }
}

static void view_files_help_callback(GtkWidget *w, gpointer context) 
{
  view_files_dialog_help();
}

static void view_files_dismiss_callback(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(view_files_dialog);
}

static gint view_files_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(view_files_dialog);
  return(true);
}

static void view_files_clear_callback(GtkWidget *w, gpointer context) 
{
  /* clear files list and associated widget list */
  clear_view_files_list();
}

static void view_files_update_callback(GtkWidget *w, gpointer context) 
{
  /* run through files list looking for any that have been deleted behind our back */
  update_view_files_list();
  if (view_files_dialog_is_active()) make_view_files_list();
}

void set_view_files_play_button(char *name, int state)
{
  if (view_files_dialog_is_active())
    {
      int i;
      i = find_view_files_regrow(name);
      if (i != -1)
	{
	  regrow *r;
	  r = view_files_row[i];
	  set_toggle_button(r->pl, state, false, (void *)r);
	}
    }
}

static void view_files_play_callback(GtkWidget *w, gpointer context) 
{
  /* open and play -- close at end or when button off toggled */
  regrow *r = (regrow *)context;
  if (view_files_play(r->pos, GTK_TOGGLE_BUTTON(w)->active))
    set_toggle_button(w, false, false, (void *)r);
}

static void view_files_select_callback(GtkWidget *w, gpointer context) 
{
  /* open and set as selected */
  regrow *r = (regrow *)context;
  view_files_select(r->pos);
}

static void sort_view_files_by_name(GtkWidget *w, gpointer context) 
{
  set_view_files_sort(SORT_BY_NAME);
  make_view_files_list();
}

static void sort_view_files_by_date(GtkWidget *w, gpointer context) 
{
  set_view_files_sort(SORT_BY_DATE);
  make_view_files_list();
}

static void sort_view_files_by_size(GtkWidget *w, gpointer context) 
{
  set_view_files_sort(SORT_BY_SIZE);
  make_view_files_list();
}

static void sort_view_files_by_entry(GtkWidget *w, gpointer context) 
{
  set_view_files_sort(SORT_BY_ENTRY);
  make_view_files_list();
}

static void sort_view_files_by_user_procedure(GtkWidget *w, gpointer context) 
{
  set_view_files_sort(SORT_BY_PROC);
  make_view_files_list();
}

void make_view_files_list (void)
{
  int i, lim;
  regrow *r;
  if (get_view_files_end() >= 0)
    {
      make_view_files_list_1();
      lim = get_view_files_end();
      for (i = 0; i <= lim; i++)
	{
	  if (!((r = view_files_row[i])))
	    {
	      r = make_regrow(vf_ww, (void (*)())view_files_play_callback, (void (*)())view_files_select_callback);
	      view_files_row[i] = r;
	      r->pos = i;
	      r->parent = FILE_VIEWER;
	    }
	  set_button_label(r->nm, get_view_files_name(r->pos));
	  set_toggle_button(r->pl, false, false, (void *)r);
	  gtk_widget_show(r->rw);
	}
    }
  lim = get_max_view_files_end();
  for (i = get_view_files_end() + 1; i <= lim; i++)
    if ((r = view_files_row[i]))
      if (GTK_WIDGET_VISIBLE(r->rw)) 
	gtk_widget_hide(r->rw);
  set_max_view_files_end(get_view_files_end());
  if (!(GTK_WIDGET_VISIBLE(vf_lst))) gtk_widget_show(vf_lst);
}

/* play open for view_files, preload */

GtkWidget *start_view_files_dialog(bool managed)
{
  if (!view_files_dialog)
    {
      ww_info *wwl;
      GtkWidget *mainform, *leftform, *fileform, *updateB, *helpB, *dismissB, *clearB, *sep;
      view_files_dialog = snd_gtk_dialog_new();
      gtk_window_set_title(GTK_WINDOW(view_files_dialog), _("Files"));
      sg_make_resizable(view_files_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(view_files_dialog), 10);
      gtk_window_resize(GTK_WINDOW(view_files_dialog), 400, 200);
      gtk_widget_realize(view_files_dialog);

      helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(helpB, "help_button");

      dismissB = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismissB, "quit_button");

      updateB = gtk_button_new_from_stock(GTK_STOCK_REFRESH);
      gtk_widget_set_name(updateB, "doit_button");

      clearB = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
      gtk_widget_set_name(clearB, "reset_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), dismissB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), updateB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), clearB, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), helpB, true, true, 10);

      SG_SIGNAL_CONNECT(view_files_dialog, "delete_event", view_files_delete_callback, NULL);
      SG_SIGNAL_CONNECT(dismissB, "clicked", view_files_dismiss_callback, NULL);
      SG_SIGNAL_CONNECT(helpB, "clicked", view_files_help_callback, NULL);
      SG_SIGNAL_CONNECT(updateB, "clicked", view_files_update_callback, NULL);
      SG_SIGNAL_CONNECT(clearB, "clicked", view_files_clear_callback, NULL);

      gtk_widget_show(dismissB);
      gtk_widget_show(updateB);
      gtk_widget_show(helpB);
      gtk_widget_show(clearB);

      mainform = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->vbox), mainform, true, true, 0);
      gtk_widget_show(mainform);
      
      leftform = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(mainform), leftform, true, true, 0);
      gtk_widget_show(leftform);

      sep = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(mainform), sep, false, false, 0);
      gtk_widget_show(sep);

      fileform = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(mainform), fileform, true, true, 0);
      gtk_widget_show(fileform);

      /* files section: play files | files */
      wwl = make_title_row(fileform, _("play"), _("files"), PAD_TITLE_ON_LEFT, WITH_SORT_BUTTON, WITHOUT_PANED_WINDOW);

      SG_SIGNAL_CONNECT(wwl->byname,  "activate", sort_view_files_by_name, NULL);
      SG_SIGNAL_CONNECT(wwl->bydate,  "activate", sort_view_files_by_date, NULL);
      SG_SIGNAL_CONNECT(wwl->bysize,  "activate", sort_view_files_by_size, NULL);
      SG_SIGNAL_CONNECT(wwl->byentry,  "activate", sort_view_files_by_entry, NULL);
      SG_SIGNAL_CONNECT(wwl->byproc,  "activate", sort_view_files_by_user_procedure, NULL);
      vf_ww = wwl->list;
      vf_lst = wwl->list;
      FREE(wwl); 
      wwl = NULL;
      set_dialog_widget(VIEW_FILES_DIALOG, view_files_dialog);
    }
  make_view_files_list();
  if (managed) gtk_widget_show(view_files_dialog);
  return(view_files_dialog);
}

void view_files_callback(GtkWidget *w, gpointer context)
{
  start_view_files_dialog(true);
}

bool view_files_dialog_is_active(void)
{
  return((view_files_dialog) && (GTK_WIDGET_VISIBLE(view_files_dialog)));
}



void g_init_gxfile(void)
{
#if HAVE_SCHEME
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.scm \
to popup file info as follows: \n\
(add-hook! mouse-enter-label-hook\n\
  (lambda (type position name)\n\
    (if (not (= type 2))\n\
        (info-dialog name (finfo name)))))\n\
See also nb.scm."
#endif
#if HAVE_RUBY
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 1 for view-files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'."
#endif

  #define H_mouse_leave_label_hook S_mouse_leave_label_hook " (type position label): called when the mouse leaves a file viewer or region label"

  mouse_enter_label_hook = XEN_DEFINE_HOOK(S_mouse_enter_label_hook, 3, H_mouse_enter_label_hook);
  mouse_leave_label_hook = XEN_DEFINE_HOOK(S_mouse_leave_label_hook, 3, H_mouse_leave_label_hook);
}


