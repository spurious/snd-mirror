#include "snd.h"

/* take a tranquillizer... (or maybe "abandon hope...") */

/* various file-related dialogs:
   File|Edit-Save-as, 
   File:Open|View, 
   File|Edit-Mix, 
   File:Edit-Header, Raw, New
   View:Files and region lists 
*/

/* TODO: merge all Motif dialog changes into Gtk */
/* TODO: compsnd have_fam=0 */
/* PERHAPS: click bomb: update, or some easy way to update (temp File: item?) */
/* TODO: highlight error message in open_info */
/* PERHAPS: make file_chooser/file_selection a run-time choice somehow? -- file_chooser is really ugly and stupid... */

#ifndef HAVE_GFCDN
/* #define HAVE_GFCDN HAVE_GTK_FILE_CHOOSER_DIALOG_NEW */
 #define HAVE_GFCDN 0
#endif

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
      if (str) 
	{
	  fdat->scanf_widget = SRATE_WIDGET;
	  (*srate) = string_to_int_with_error(str, 1, "srate"); 
	}
    }

  if ((chans) && (fdat->chans_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->chans_text)); 
      if (str) 
	{
	  fdat->scanf_widget = CHANS_WIDGET;
	  (*chans) = string_to_int_with_error(str, min_chan, "chans"); 
	}
    }

  if ((location) && (fdat->location_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->location_text)); 
      if (str)
	{
	  fdat->scanf_widget = DATA_LOCATION_WIDGET;
	  (*location) = string_to_off_t_with_error(str, 0, "data location"); 
	}
    }

  if ((samples) && (fdat->samples_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->samples_text)); 
      if (str)
	{
	  fdat->scanf_widget = SAMPLES_WIDGET;
	  (*samples) = string_to_off_t_with_error(str, 0, "samples"); 
	}
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
  fl = set_header_and_data_positions(fdat, type, format); 
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

/* TODO: separate funcs to add play/just-sounds buttons (if the latter is possible) */


/* ---------------- file dialogs ---------------- */

typedef struct file_dialog_info {
  int file_dialog_read_only;
  GtkWidget *dialog, *dialog_frame, *dialog_info1, *dialog_info2, *dialog_vbox;
  dialog_play_info *dp;
} file_dialog_info;

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
  file_dialog_info *fd;
} filer_response_t;

static filer_response_t *wrap_filer_callbacks(Callback_Func ok, Callback_Func cancel, Callback_Func help, file_dialog_info *fd)
{
  filer_response_t *fr;
  fr = (filer_response_t *)CALLOC(1, sizeof(filer_response_t));
  fr->ok = ok;
  fr->cancel = cancel;
  fr->help = help;
  fr->fd = fd;
  return(fr);
}

static void chooser_response_callback(GtkDialog *dialog, gint response_id)
{
  filer_response_t *fr;
  fr = (filer_response_t *)get_user_data(G_OBJECT(dialog));
  if (response_id == GTK_RESPONSE_OK)
    (*(fr->ok))(GTK_WIDGET(dialog), (gpointer)(fr->fd));
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

static GtkWidget *snd_filer_new(char *title, bool saving, 
				GtkSignalFunc gdelete, Callback_Func ok, Callback_Func cancel, Callback_Func help,
				gpointer fd)
{
  GtkWidget *new_dialog;
  new_dialog = gtk_file_chooser_dialog_new(title, NULL, 
					   (saving) ? GTK_FILE_CHOOSER_ACTION_SAVE : GTK_FILE_CHOOSER_ACTION_OPEN,
					   (saving) ? GTK_STOCK_SAVE : GTK_STOCK_OPEN, GTK_RESPONSE_OK,
					   GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					   GTK_STOCK_HELP, GTK_RESPONSE_HELP,
					   NULL);
  gtk_window_set_default_size(GTK_WINDOW(new_dialog), 600, 400);
  gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(new_dialog), false);

  set_user_data(G_OBJECT(new_dialog), (gpointer)wrap_filer_callbacks(ok, cancel, help, fd));
  SG_SIGNAL_CONNECT(new_dialog, "response", chooser_response_callback, fd);

  /* this has to be separate (not handled as a "response" because the latter deletes the goddamn widget!! */
  SG_SIGNAL_CONNECT(new_dialog, "delete_event", gdelete, NULL);
  return(new_dialog);
}

#else

#define Callback_Func GtkSignalFunc
static void unpad(gpointer w, gpointer data)
{
  if (GTK_IS_CELL_RENDERER_TEXT(w))
    GTK_CELL_RENDERER(w)->ypad = 0;
}

#define SND_FILER(Widget) GTK_FILE_SELECTION(Widget)

static char *snd_filer_get_filename(GtkWidget *dialog)
{
  return(copy_string((char *)gtk_file_selection_get_filename(GTK_FILE_SELECTION(dialog))));
}

static GtkWidget *snd_filer_new(char *title, bool saving, 
				GtkSignalFunc gdelete, Callback_Func ok, Callback_Func cancel, Callback_Func help,
				gpointer fd)
{
  GtkWidget *new_dialog, *entry, *helpB;
  GList *cells;
  GtkTreeViewColumn *dirl;
  GtkFileSelection *filer;
  new_dialog = gtk_file_selection_new(title);
  filer = SND_FILER(new_dialog);

  /* TODO: check button_new -> from stock where possible
   *    [help -> GTK_STOCK_HELP]
   *    [cancel -> GTK_STOCK_CANCEL]
   *    apply -> GTK_STOCK_APPLY
   *    clear -> GTK_STOCK_CLEAR
   *    print -> GTK_STOCK_PRINT
   *    reset -> GTK_STOCK_REFRESH? or update?
   *    save/as -> GTK_STOCK_SAVE/AS
   *    color -> GTK_STOCK_SELECT_COLOR
   *    open -> GTK_STOCK_OPEN
   *    undo -> GTK_STOCK_UNDO
   *    delete -> GTK_STOCK_DELETE
   *    dismiss -> ? quit?
   *    orientation -> ?
   *    previous -> GTK_STOCK_GO_BACK?
   *    next -> GTK_STOCK_GO_FORWARD?
   *    find -> GTK_STOCK_FIND
   *    ok -> GTK_STOCK_OK
   *    yes/no if yes_or_no_p survives
   */

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

  SG_SIGNAL_CONNECT(new_dialog, "delete_event", gdelete, NULL);
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
      (sound_file_p(filename)))
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
      gtk_label_set_text(GTK_LABEL(fd->dialog_info1), buf);
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
      gtk_label_set_text(GTK_LABEL(fd->dialog_info2), buf);
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
  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));
  fd->file_dialog_read_only = read_only;
  fd->dialog = snd_filer_new(title, false, file_delete_proc, file_ok_proc, file_dismiss_proc, file_help_proc, (gpointer)fd);
  fd->dp->dialog = fd->dialog;
  g_object_set_data(G_OBJECT(fd->dialog), "snd-dialog", (gpointer)fd);

  fd->dialog_frame = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(fd->dialog_frame), GTK_SHADOW_ETCHED_IN);
  gtk_widget_show(fd->dialog_frame);
  gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(fd->dialog), fd->dialog_frame);
    
  fd->dialog_vbox = gtk_vbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(fd->dialog_frame), fd->dialog_vbox);

  fd->dialog_info1 = gtk_label_new(NULL);
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info1, false, true, 2);

  fd->dialog_info2 = gtk_label_new(NULL);
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info2, false, true, 2);

  fd->dp->play_button = gtk_check_button_new_with_label(_("play selected sound"));
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dp->play_button, false, true, 2);
  SG_SIGNAL_CONNECT(fd->dp->play_button, "toggled", play_selected_callback, fd->dp);
  SG_SIGNAL_CONNECT(fd->dialog, "selection-changed", update_info_callback, NULL);

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
  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->dp = (dialog_play_info *)CALLOC(1, sizeof(dialog_play_info));
  fd->file_dialog_read_only = read_only;
  fd->dialog = snd_filer_new(title, false, file_delete_proc, file_ok_proc, file_dismiss_proc, file_help_proc, fd);
  fd->dp->dialog = fd->dialog;

  /* these start out hidden -- are shown only when a file is selected */
  fd->dialog_frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(SND_FILER(fd->dialog)->main_vbox), fd->dialog_frame, true, true, 0);
  gtk_frame_set_shadow_type(GTK_FRAME(fd->dialog_frame), GTK_SHADOW_ETCHED_IN);
  
  fd->dialog_vbox = gtk_vbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(fd->dialog_frame), fd->dialog_vbox);

  fd->dialog_info1 = gtk_label_new(NULL);
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info1, true, true, 2);
	
  fd->dialog_info2 = gtk_label_new(NULL);
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info2, true, true, 2);

  SG_SIGNAL_CONNECT(gtk_tree_view_get_selection(GTK_TREE_VIEW(SND_FILER(fd->dialog)->file_list)), "changed", dialog_select_callback, fd);

  fd->dp->play_button = gtk_check_button_new_with_label(_("play selected sound"));
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dp->play_button, true, true, 2);
  SG_SIGNAL_CONNECT(fd->dp->play_button, "toggled", play_selected_callback, fd->dp);
  set_dialog_widget(which_dialog, fd->dialog);
  return(fd);
}

#endif

static file_dialog_info *open_dialog = NULL;
static file_dialog_info *mdat = NULL;

void set_open_file_play_button(bool val) 
{
  if ((open_dialog) && (open_dialog->dp->play_button))
    set_toggle_button(open_dialog->dp->play_button, val, false, (gpointer)open_dialog);
  if ((mdat) && (mdat->dp->play_button))
    set_toggle_button(mdat->dp->play_button, val, false, (gpointer)mdat);
}

static void file_open_error(const char *error_msg, void *ufd)
{
  /* called from snd_error, redirecting error handling to the dialog */
  file_dialog_info *fd = (file_dialog_info *)ufd;
  /* fprintf(stderr,"set up open error: %s\n", error_msg); */
  set_label(fd->dialog_info1, error_msg);
  gtk_widget_show(fd->dialog_frame);
  gtk_widget_show(fd->dialog_vbox);
  gtk_widget_show(fd->dialog_info1);
  gtk_widget_hide(fd->dialog_info2);
  gtk_widget_hide(fd->dp->play_button);
}

static void clear_file_error_label(file_dialog_info *fd)
{
  set_label(fd->dialog_info1, "");
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
  /* fprintf(stderr,"clear %ld %ld\n", key_press_handler_id, selection_changed_handler_id); */
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
  /* fprintf(stderr, "call key_press\n"); */
  clear_file_error_label(fd);
  clear_open_handlers(fd->dialog);
  return(false);
}
static void open_modify_selection_changed(GtkTreeSelection *selection, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  /* fprintf(stderr, "call selection_changed\n"); */
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
  fprintf(stderr, "post %ld %ld\n", key_press_handler_id, selection_changed_handler_id);
#endif
}

/* TODO: vector to raw data and back */

static void file_open_dialog_ok(GtkWidget *w, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  char *filename = NULL;
  gpointer hide_me = 0;
  filename = snd_filer_get_filename(open_dialog->dialog);
  if ((!filename) || (!(*filename)))
    {
      file_open_error(_("no filename given"), (void *)fd);
      clear_error_if_open_changes(fd->dialog, (void *)fd);
    }
  else
    {
      file_dialog_stop_playing(open_dialog->dp);
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
	      hide_me = g_object_get_data(G_OBJECT(w), "hide-me"); /* see snd-gtk.scm where this is set */
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
  file_dialog_stop_playing(open_dialog->dp);
  gtk_widget_hide(open_dialog->dialog);
}

static void file_open_dialog_help(GtkWidget *w, gpointer context)
{
  open_file_dialog_help();
}

static gint file_open_dialog_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(open_dialog->dp);
  gtk_widget_hide(open_dialog->dialog);
  return(true);
}

widget_t make_open_file_dialog(bool read_only, bool managed)
{
  if (!open_dialog)
    open_dialog = make_file_dialog(read_only, (char *)((read_only) ? _("View") : _("Open")), FILE_OPEN_DIALOG,
				   (Callback_Func)file_open_dialog_ok,				     
				   (GtkSignalFunc)file_open_dialog_delete,
				   (Callback_Func)file_open_dialog_dismiss,
				   (Callback_Func)file_open_dialog_help);
  else
    {
      if (read_only != open_dialog->file_dialog_read_only)
	{
	  gtk_window_set_title(GTK_WINDOW(open_dialog->dialog), (char *)((read_only) ? _("View") : _("Open")));
	  open_dialog->file_dialog_read_only = read_only;
	}
    }
  if (managed) gtk_widget_show(open_dialog->dialog);
  return(open_dialog->dialog);
}


/* -------- mix file dialog -------- */

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
  gpointer hide_me = 0;
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
	  int err;
	  redirect_snd_error_to(file_open_error, (void *)mdat);
	  err = mix_complete_file_at_cursor(any_selected_sound(), filename, with_mix_tags(ss), 0);
	  redirect_snd_error_to(NULL, NULL);
	  if (err == 0) 
	    {
	      hide_me = g_object_get_data(G_OBJECT(w), "hide-me");
	      if (hide_me == 0)
		gtk_widget_hide(mdat->dialog);
	    }
	  else
	    {
	      clear_error_if_open_changes(mdat->dialog, (void *)mdat);
	    }
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



/* -------- file data choices -------- */

#define NUM_HEADER_TYPES 7
static char *header_short_names[NUM_HEADER_TYPES] = {"snd  ", "aifc ", "wave ", "raw  ", "aiff ", "ircam", "nist "};

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
  for (i = 0; i < NUM_HEADER_TYPES; i++)
    if (strcmp(value, header_short_names[i]) == 0)
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
  int dformats = 0;
  char **formats = NULL;
  file_data *fd = (file_data *)gp;

  if (!(fd->format_list)) return;
  /* sg_list_select in make_file_data_panel can trigger this before we have finished making the file data panel */
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;

  gtk_tree_model_get(model, &iter, 0, &value, -1);
  formats = set_header_positions_from_type(fd, fd->current_type, fd->current_format);
  dformats = fd->formats;
  for (i = 0; i < dformats; i++)
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
				dialog_comment_t with_comment)
{
  GtkWidget *form, *scbox, *combox = NULL;
  file_data *fdat;
  int dformats = 0;
  char **formats = NULL;
  GtkWidget *s8, *s22, *s44, *s48;
  GtkWidget *sbar, *sitem, *smenu;

  fdat = (file_data *)CALLOC(1, sizeof(file_data));
  fdat->current_type = header_type;
  fdat->current_format = data_format;
  formats = set_header_positions_from_type(fdat, header_type, data_format);
  dformats = fdat->formats;

  form = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(parent), form, false, false, 4); /* ??? */
  gtk_widget_show(form);

  /* header type */
  if (with_header_type == WITH_HEADER_TYPE_FIELD)
    {
      fdat->header_list = sg_make_list(_("header"), form, BOX_PACK, (gpointer)fdat, NUM_HEADER_TYPES, header_short_names, 
				       GTK_SIGNAL_FUNC(update_header_type_list), 0, 0, 0, 0);
      /* header_list is a gtk list */
      sg_list_select(fdat->header_list, fdat->header_pos);
      gtk_widget_show(fdat->header_list);
    }

  /* data format */
  fdat->format_list = sg_make_list(_("data"), form, BOX_PACK, (gpointer)fdat, dformats, formats, 
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

      citem = gtk_menu_item_new_with_label(_("chans:"));
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

static file_data *sdat = NULL;
static GtkWidget *save_as_dialog = NULL;
static save_dialog_t save_as_dialog_type = FILE_SAVE_AS;
static char *last_save_as_filename = NULL;

/* TODO: extract callback, ok error checks */

static void save_as_ok_callback(GtkWidget *w, gpointer data)
{
  char *comment = NULL, *msg;
  int type, format, srate, chans;
  bool need_directory_update = false;
  off_t location, samples;
  gpointer hide_me = 0;
  snd_info *sp;
  comment = get_file_dialog_sound_attributes(sdat, &srate, &chans, &type, &format, &location, &samples, 0);
  if (last_save_as_filename) 
    FREE(last_save_as_filename);
  last_save_as_filename = snd_filer_get_filename(save_as_dialog);
  sp = any_selected_sound();
  if (last_save_as_filename)
    msg = save_as_dialog_save_sound(sp, last_save_as_filename, save_as_dialog_type, srate, type, format, comment, &need_directory_update);
  else 
    {
      if (sp) 
	report_in_minibuffer(sp, _("not saved (no name given)"));
    }
  if (comment) 
    FREE(comment);
  hide_me = g_object_get_data(G_OBJECT(w), "hide-me");
  if (hide_me == 0)
    gtk_widget_hide(save_as_dialog);
  if ((sp) && (last_save_as_filename) &&
      (save_as_dialog_type == FILE_SAVE_AS) && 
      (need_directory_update))
    run_after_save_as_hook(sp, last_save_as_filename, true); /* true => from dialog */
} 

static void save_as_cancel_callback(GtkWidget *w, gpointer data)
{ 
  gtk_widget_hide(save_as_dialog);
} 

static void save_as_help_callback(GtkWidget *w, gpointer data)
{
  save_as_dialog_help();
}

static gint save_as_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(save_as_dialog);
  return(true);
}

static void make_save_as_dialog(char *sound_name, int header_type, int format_type)
{
  /* save old as new, close old, open new */
  char *file_string;
  file_string = mus_format(_("save %s"), sound_name);
  if (!save_as_dialog)
    {
      GtkWidget *fbox;
      save_as_dialog = snd_filer_new(file_string, true,
				     (GtkSignalFunc)save_as_delete_callback,
				     (Callback_Func)save_as_ok_callback,
				     (Callback_Func)save_as_cancel_callback,
				     (Callback_Func)save_as_help_callback,
				     NULL); /* fix later */
      fbox = gtk_vbox_new(false, 0);
#if HAVE_GFCDN
      gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(save_as_dialog), fbox);
#else
      gtk_box_pack_start(GTK_BOX(SND_FILER(save_as_dialog)->main_vbox), fbox, true, true, 0);
#endif
      gtk_widget_show(fbox);
      sdat = make_file_data_panel(fbox, "data-form", 
				  WITH_EXTRACT_CHANNELS_FIELD, 
				  header_type, format_type, 
				  WITHOUT_DATA_LOCATION_FIELD, 
				  WITHOUT_SAMPLES_FIELD,
				  WITH_ERROR_FIELD, 
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD);
      set_dialog_widget(FILE_SAVE_AS_DIALOG, save_as_dialog);
    }
  else
    {
      gtk_window_set_title(GTK_WINDOW(save_as_dialog), file_string);
    }
  FREE(file_string);
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
  set_file_dialog_sound_attributes(sdat,
				   sdat->current_type,
				   sdat->current_format,
				   (hdr) ? hdr->srate : selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES,
				   com = output_comment(hdr));
  if (com) FREE(com);
  if (managed) gtk_widget_show(save_as_dialog);
  return(save_as_dialog);
}

widget_t make_edit_save_as_dialog(bool managed)
{
  save_as_dialog_type = EDIT_SAVE_AS;
  make_save_as_dialog(_("current selection"),
		      default_output_type(ss),
		      default_output_format(ss));
  set_file_dialog_sound_attributes(sdat,
				   sdat->current_type,
				   sdat->current_format,
				   selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   NULL);
  if (managed) gtk_widget_show(save_as_dialog);
  return(save_as_dialog);
}

void reflect_just_sounds(void)
{
#if HAVE_GFCDN
  if (open_dialog)
    gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(open_dialog->dialog), (just_sounds(ss)) ? sound_files_filter : all_files_filter);
  if (mdat)
    gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(mdat->dialog), (just_sounds(ss)) ? sound_files_filter : all_files_filter);
#endif
}

void save_file_dialog_state(FILE *fd)
{
  if ((open_dialog) && (GTK_WIDGET_VISIBLE(open_dialog->dialog)))
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
  if ((save_as_dialog) && (GTK_WIDGET_VISIBLE(save_as_dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", (save_as_dialog_type == FILE_SAVE_AS) ? S_save_sound_dialog : S_save_selection_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", TO_PROC_NAME((save_as_dialog_type == FILE_SAVE_AS) ? S_save_sound_dialog : S_save_selection_dialog));
#endif
    }
}



/* -------------------------------- Raw Data Dialog -------------------------------- */

static GtkWidget *raw_data_dialog = NULL;
static file_data *rdat = NULL;
static off_t raw_data_location = 0;
static bool raw_data_read_only = false, raw_data_sound_selected = false;
static char *raw_data_filename = NULL, *raw_data_info = NULL;

static void raw_data_ok_callback(GtkWidget *w, gpointer context)
{
  int raw_srate, raw_chans, raw_data_format;
  get_file_dialog_sound_attributes(rdat, &raw_srate, &raw_chans, NULL, &raw_data_format, &raw_data_location, NULL, 1);
  if (rdat->error_widget != NOT_A_SCANF_WIDGET)
    {
      fprintf(stderr,"ooops"); /* TODO: error handling */
    }
  else
    {
      file_info *hdr;
      mus_header_set_raw_defaults(raw_srate, raw_chans, raw_data_format);
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
      mus_sound_override_header(raw_data_filename, raw_srate, raw_chans, raw_data_format, MUS_RAW, raw_data_location,
				mus_bytes_to_samples(raw_data_format, 
						     mus_sound_length(raw_data_filename) - raw_data_location));
      finish_opening_sound(add_sound_window(raw_data_filename, raw_data_read_only, hdr), raw_data_sound_selected);
      gtk_widget_hide(raw_data_dialog);
    }
}

static void raw_data_cancel_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(raw_data_dialog);
}

static gint raw_data_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  return(true);
}

static void raw_data_reset_callback(GtkWidget *w, gpointer context) 
{
  int raw_srate, raw_chans, raw_data_format;
  raw_data_location = 0;
  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */  
  set_file_dialog_sound_attributes(rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, raw_data_location, 
				   IGNORE_SAMPLES, NULL);
  gtk_widget_hide(rdat->error_text);
}

static void raw_data_help_callback(GtkWidget *w, gpointer context) 
{
  raw_data_dialog_help(raw_data_info);
}

static void make_raw_data_dialog(const char *filename, const char *title)
{
  GtkWidget *resetB, *helpB, *cancelB, *okB;
  int raw_srate, raw_chans, raw_data_format;
 
  raw_data_dialog = snd_gtk_dialog_new();
  SG_SIGNAL_CONNECT(raw_data_dialog, "delete_event", raw_data_delete_callback, NULL);
  gtk_window_set_title(GTK_WINDOW(raw_data_dialog), _("No Header on File"));
  sg_make_resizable(raw_data_dialog);
  gtk_container_set_border_width(GTK_CONTAINER(raw_data_dialog), 10);
  gtk_window_resize(GTK_WINDOW(raw_data_dialog), 350, 260);
  gtk_widget_realize(raw_data_dialog);

  helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
  gtk_widget_set_name(helpB, "help_button");

  cancelB = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
  gtk_widget_set_name(cancelB, "quit_button");

  resetB = gtk_button_new_with_label(_("Reset"));
  gtk_widget_set_name(resetB, "reset_button");

  okB = gtk_button_new_from_stock(GTK_STOCK_OK);
  gtk_widget_set_name(okB, "doit_button");

  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), okB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), resetB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), cancelB, true, true, 10);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), helpB, true, true, 10);

  SG_SIGNAL_CONNECT(okB, "clicked", raw_data_ok_callback, NULL);
  SG_SIGNAL_CONNECT(helpB, "clicked", raw_data_help_callback, NULL);
  SG_SIGNAL_CONNECT(resetB, "clicked", raw_data_reset_callback, NULL);
  SG_SIGNAL_CONNECT(cancelB, "clicked", raw_data_cancel_callback, NULL);

  gtk_widget_show(okB);
  gtk_widget_show(cancelB);
  gtk_widget_show(helpB);
  gtk_widget_show(resetB);

  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */

  rdat = make_file_data_panel(GTK_DIALOG(raw_data_dialog)->vbox, "data-form", 
			      WITH_CHANNELS_FIELD, 
			      MUS_RAW, raw_data_format, 
			      WITH_DATA_LOCATION_FIELD, 
			      WITHOUT_SAMPLES_FIELD,
			      WITH_ERROR_FIELD, 
			      WITHOUT_HEADER_TYPE_FIELD, 
			      WITHOUT_COMMENT_FIELD);
  set_file_dialog_sound_attributes(rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_data_format, raw_srate, raw_chans, raw_data_location, 
				   IGNORE_SAMPLES, NULL);

  set_dialog_widget(RAW_DATA_DIALOG, raw_data_dialog);
}

void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, bool read_only, bool selected)
{
  raw_data_read_only = read_only;
  raw_data_sound_selected = selected;
  if (raw_data_filename) FREE(raw_data_filename);
  raw_data_filename = copy_string(filename);
  if ((ss->sgx->requestor_dialog) &&
      (ss->open_requestor == FROM_OPEN_DIALOG))
    gtk_widget_hide(ss->sgx->requestor_dialog);
  if (!title) 
    title = mus_format("no header found on %s\n", filename);
  if (!raw_data_dialog) 
    make_raw_data_dialog(filename, title);
  else gtk_window_set_title(GTK_WINDOW(raw_data_dialog), title);
  FREE(title);
  if (raw_data_info) FREE(raw_data_info);
  if (info)
    {
      raw_data_info = copy_string(info);
      FREE(info);
    }
  else raw_data_info = NULL;
  raise_dialog(raw_data_dialog);
  gtk_widget_show(raw_data_dialog);
}


/* -------------------------------- New File -------------------------------- */

static GtkWidget *new_file_dialog = NULL, *new_file_name = NULL;
static file_data *ndat = NULL;
static off_t initial_samples = 1;
static bool new_file_doit = false;

static void new_file_undoit(void)
{
}

static void new_file_ok_callback(GtkWidget *w, gpointer context) 
{
  off_t loc;
  char *comment = NULL, *newer_name = NULL, *msg;
  int header_type, data_format, srate, chans;
  newer_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_name));
  if ((!newer_name) || (!(*newer_name)))
    {
      msg = _("new sound needs a file name ('New file:' field is empty)");
      snd_error(msg); /* fix later */
    }
  else
    {
      comment = get_file_dialog_sound_attributes(ndat, &srate, &chans, &header_type, &data_format, &loc, &initial_samples, 1);
      if (ndat->error_widget != NOT_A_SCANF_WIDGET)
	{
	  /* fix later */
	}
      else
	{
	  snd_info *sp;
	  /* handle the overwrite hook directly */
	  if ((!new_file_doit) &&
	      (ask_before_overwrite(ss)) && 
	      (mus_file_probe(newer_name)))
	    {
	      /* SOMEDAY: would be nice to cancel 'DoIt' if user deletes file by hand */
	      msg = mus_format(_("%s exists. If you want to overwrite it, click 'DoIt'"), newer_name);
	      /* fix later */
	      FREE(msg);
	      new_file_doit = true;
	    }
	  else
	    {
	      if (new_file_doit)
		new_file_undoit();

	      sp = snd_new_file(newer_name, header_type, data_format, srate, chans, comment, initial_samples);

	      if (!sp)
		{
		  /* fix */
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

  header_type = default_output_type(ss);
  chans =       default_output_chans(ss);
  data_format = default_output_format(ss);
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
  if (!filename) fprintf(stderr,"no filename?");
  gtk_entry_set_text(GTK_ENTRY(new_file_name), filename);  
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
  current_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_name));
  load_new_file_defaults(current_name);
  if (new_file_doit)
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
      GtkWidget *name_label, *hform, *help_button, *cancel_button, *ok_button, *reset_button;
      new_file_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(new_file_dialog, "delete_event", new_file_delete_callback, NULL);
      gtk_window_set_title(GTK_WINDOW(new_file_dialog), _("New file"));
      sg_make_resizable(new_file_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(new_file_dialog), 10);
      gtk_window_resize(GTK_WINDOW(new_file_dialog), 400, 250);
      gtk_widget_realize(new_file_dialog);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
      gtk_widget_set_name(cancel_button, "quit_button");

      ok_button = gtk_button_new_from_stock(GTK_STOCK_OK);
      gtk_widget_set_name(ok_button, "doit_button");

      reset_button = gtk_button_new_with_label(_("Reset"));
      gtk_widget_set_name(reset_button, "reset_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_file_dialog)->action_area), ok_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_file_dialog)->action_area), reset_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_file_dialog)->action_area), cancel_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(new_file_dialog)->action_area), help_button, true, true, 10);

      SG_SIGNAL_CONNECT(cancel_button, "clicked", new_file_cancel_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", new_file_help_callback, NULL);
      SG_SIGNAL_CONNECT(ok_button, "clicked", new_file_ok_callback, NULL);
      SG_SIGNAL_CONNECT(reset_button, "clicked", new_file_reset_callback, NULL);

      gtk_widget_show(cancel_button);
      gtk_widget_show(ok_button);
      gtk_widget_show(reset_button);
      gtk_widget_show(help_button);

      hform = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_file_dialog)->vbox), hform, false, false, 4);
      gtk_widget_show(hform);

      name_label = gtk_label_new(_("New file:"));
      gtk_box_pack_start(GTK_BOX(hform), name_label, false, false, 2);
      gtk_widget_show(name_label);

      new_file_name = snd_entry_new(hform, true);

      newname = output_name(NULL); /* fix later */
      if ((newname) && (*newname))
	gtk_entry_set_text(GTK_ENTRY(new_file_name), newname); /* output_name?? fix later */

      ndat = make_file_data_panel(GTK_DIALOG(new_file_dialog)->vbox, "data-form", 
				  WITH_CHANNELS_FIELD, 
				  default_output_type(ss), 
				  default_output_format(ss), 
				  WITHOUT_DATA_LOCATION_FIELD, 
				  WITH_SAMPLES_FIELD,
				  WITH_ERROR_FIELD, 
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD);
      set_dialog_widget(NEW_FILE_DIALOG, new_file_dialog);
      load_new_file_defaults(NULL);
    }
  else
    {
      /* if overwrite question pends, but file has been deleted in the meantime, go back to normal state */
      if (new_file_doit)
	{
	  char *new_name;
	  new_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_name));
	  if ((!new_name) || (!(*new_name)) ||
	      (!(mus_file_probe(new_name))))
	    new_file_undoit();
	}
    }
  gtk_widget_show(new_file_dialog);
}



/* ---------------- Edit Header ---------------- */

static GtkWidget *edit_header_dialog = NULL;
static file_data *edat;
static snd_info *edit_header_sp = NULL;

static void edit_header_help_callback(GtkWidget *w, gpointer context) 
{
  edit_header_dialog_help();
}

static void edit_header_cancel_callback(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(edit_header_dialog);
}

static gint edit_header_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(edit_header_dialog);
  return(true);
}

static void edit_header_ok_callback(GtkWidget *w, gpointer context) 
{
  if (!(edit_header_sp->read_only))
    edit_header_callback(edit_header_sp, edat, NULL, NULL);
  else snd_error(_("%s is write-protected"), edit_header_sp->short_filename);
  gtk_widget_hide(edit_header_dialog);
}

GtkWidget *edit_header(snd_info *sp)
{
  /* like display-info, but writable.
   * need fields for srate, channels, type, format, data location, comment
   * if any are changed, need save button, cancel button, dismiss (leave unsaved but pending), reflect (change Snd display, not file)
   * this means the Snd-effective header is separate from the in-file header even across saves??
   */
  char *str;
  file_info *hdr;
  if (!sp) return(NULL);
  edit_header_sp = sp;
  hdr = sp->hdr;
  if (!edit_header_dialog)
    {
      GtkWidget *help_button, *cancel_button, *save_button;
      edit_header_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(edit_header_dialog, "delete_event", edit_header_delete_callback, NULL);
      /* gtk_window_set_title(GTK_WINDOW(edit_header_dialog), _("Edit Header")); */
      sg_make_resizable(edit_header_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(edit_header_dialog), 10);
      gtk_window_resize(GTK_WINDOW(edit_header_dialog), 400, 250);
      gtk_widget_realize(edit_header_dialog);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "help_button");

      cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
      gtk_widget_set_name(cancel_button, "quit_button");

      save_button = gtk_button_new_from_stock(GTK_STOCK_SAVE);
      gtk_widget_set_name(save_button, "doit_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), cancel_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), save_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), help_button, true, true, 10);

      SG_SIGNAL_CONNECT(cancel_button, "clicked", edit_header_cancel_callback, NULL);
      SG_SIGNAL_CONNECT(help_button, "clicked", edit_header_help_callback, NULL);
      SG_SIGNAL_CONNECT(save_button, "clicked", edit_header_ok_callback, NULL);

      gtk_widget_show(cancel_button);
      gtk_widget_show(save_button);
      gtk_widget_show(help_button);

      edat = make_file_data_panel(GTK_DIALOG(edit_header_dialog)->vbox, _("Edit Header"), 
				  WITH_CHANNELS_FIELD, 
				  hdr->type, 
				  hdr->format, 
				  WITH_DATA_LOCATION_FIELD, 
				  WITH_SAMPLES_FIELD,
				  WITH_ERROR_FIELD, 
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD);
      set_dialog_widget(EDIT_HEADER_DIALOG, edit_header_dialog);
    }
  str = mus_format(_("Edit header of %s"), sp->short_filename);
  gtk_window_set_title(GTK_WINDOW(edit_header_dialog), str);
  FREE(str);

  gtk_widget_show(edit_header_dialog);
  set_file_dialog_sound_attributes(edat, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->samples, hdr->comment);
  return(edit_header_dialog);
}

void save_edit_header_dialog_state(FILE *fd)
{
  if ((edit_header_dialog) && (GTK_WIDGET_VISIBLE(edit_header_dialog)) && (snd_ok(edit_header_sp)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s (%s \"%s\"))\n", S_edit_header_dialog, S_find_sound, edit_header_sp->short_filename);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(%s(\"%s\"))\n", TO_PROC_NAME(S_edit_header_dialog), TO_PROC_NAME(S_find_sound), edit_header_sp->short_filename);
#endif
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
	  if (r->parent == CURRENT_FILE_VIEWER)
	    label = get_curfullname(r->pos);
	  else
	    {
	      if (r->parent == PREVIOUS_FILE_VIEWER)
		label = get_prevfullname(r->pos);
	      else
		label = (char *)gtk_label_get_text(GTK_LABEL(GTK_BIN(w)->child));
	    }
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
void set_file_sort_sensitive(bool sensitive)
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
static int vf_selected_file = -1;
static GtkWidget *vf_curww, *vf_prevlst, *vf_curlst, *vf_prevww;

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
  /* clear previous files list and associated widget list */
  clear_prevlist();
}

static void view_files_update_callback(GtkWidget *w, gpointer context) 
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
	  set_toggle_button(r->pl, state, false, (void *)r);
	}
    }
}

static void view_curfiles_play_callback(GtkWidget *w, gpointer context) 
{
  regrow *r = (regrow *)context;
  view_curfiles_play(r->pos, GTK_TOGGLE_BUTTON(w)->active);
}

static void curfile_unhighlight(void)
{
  if (view_files_dialog_is_active())
    {
      if (vf_selected_file != -1)
	{
	  regrow *r;
	  r = cur_name_row[vf_selected_file];
	  gtk_widget_modify_bg(r->nm, GTK_STATE_NORMAL, ss->sgx->basic_color);
	  gtk_widget_modify_base(r->nm, GTK_STATE_NORMAL, ss->sgx->basic_color);
	  gtk_widget_modify_bg(r->rw, GTK_STATE_NORMAL, ss->sgx->basic_color);
	  gtk_widget_modify_base(r->rw, GTK_STATE_NORMAL, ss->sgx->basic_color);
	  vf_selected_file = -1;
	}
    }
}

void curfile_highlight(int i)
{
  if (view_files_dialog_is_active())
    {
      regrow *r;
      if (vf_selected_file != -1) curfile_unhighlight();
      r = cur_name_row[i];
      gtk_widget_modify_bg(r->nm, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      gtk_widget_modify_base(r->nm, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      gtk_widget_modify_bg(r->rw, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      gtk_widget_modify_base(r->rw, GTK_STATE_NORMAL, ss->sgx->zoom_color);
      vf_selected_file = i;
    }
}

static void view_curfiles_select_callback(GtkWidget *w, gpointer context) 
{
  regrow *r = (regrow *)context;
  view_curfiles_select(r->pos);
}

static void view_prevfiles_play_callback(GtkWidget *w, gpointer context) 
{
  /* open and play -- close at end or when button off toggled */
  regrow *r = (regrow *)context;
  if (view_prevfiles_play(r->pos, GTK_TOGGLE_BUTTON(w)->active))
    set_toggle_button(w, false, false, (void *)r);
}

static void view_prevfiles_select_callback(GtkWidget *w, gpointer context) 
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
  regrow *r;
  lim = get_curfile_end();
  for (i = 0; i < lim; i++)
    {
      r = cur_name_row[i];
      if (r == NULL)
	{
	  r = make_regrow(vf_curww, (void (*)())view_curfiles_play_callback, (void (*)())view_curfiles_select_callback);
	  cur_name_row[i] = r;
	  r->pos = i;
	  r->parent = CURRENT_FILE_VIEWER;
	}
      set_button_label(r->nm, view_curfiles_name(r->pos));
      set_toggle_button(r->pl, false, false, (void *)r);
      gtk_widget_show(r->rw);
    }
  lim = get_max_curfile_end();
  for (i = get_curfile_end(); i < lim; i++)
    if ((r = cur_name_row[i]))
      if (GTK_WIDGET_VISIBLE(r->rw)) 
	gtk_widget_hide(r->rw);
  set_max_curfile_end(get_curfile_end());
  highlight_selected_sound();
  gtk_widget_show(vf_curlst);
}

static void sort_prevfiles_by_name(GtkWidget *w, gpointer context) 
{
  set_previous_files_sort(1);
  make_prevfiles_list();
}

static void sort_prevfiles_by_date(GtkWidget *w, gpointer context) 
{
  set_previous_files_sort(2);
  make_prevfiles_list();
}

static void sort_prevfiles_by_size(GtkWidget *w, gpointer context) 
{
  set_previous_files_sort(3);
  make_prevfiles_list();
}

static void sort_prevfiles_by_entry(GtkWidget *w, gpointer context) 
{
  set_previous_files_sort(4);
  make_prevfiles_list();
}

static void sort_prevfiles_by_user_procedure(GtkWidget *w, gpointer context) 
{
  set_previous_files_sort(5);
  make_prevfiles_list();
}

void make_prevfiles_list (void)
{
  int i, lim;
  regrow *r;
  if (get_prevfile_end() >= 0)
    {
      make_prevfiles_list_1();
      lim = get_prevfile_end();
      for (i = 0; i <= lim; i++)
	{
	  if (!((r = prev_name_row[i])))
	    {
	      r = make_regrow(vf_prevww, (void (*)())view_prevfiles_play_callback, (void (*)())view_prevfiles_select_callback);
	      prev_name_row[i] = r;
	      r->pos = i;
	      r->parent = PREVIOUS_FILE_VIEWER;
	    }
	  set_button_label(r->nm, get_prevname(r->pos));
	  set_toggle_button(r->pl, false, false, (void *)r);
	  gtk_widget_show(r->rw);
	}
    }
  lim = get_max_prevfile_end();
  for (i = get_prevfile_end() + 1; i <= lim; i++)
    if ((r = prev_name_row[i]))
      if (GTK_WIDGET_VISIBLE(r->rw)) 
	gtk_widget_hide(r->rw);
  set_max_prevfile_end(get_prevfile_end());
  if (!(GTK_WIDGET_VISIBLE(vf_prevlst))) gtk_widget_show(vf_prevlst);
}

/* play open for prevfile, play select for curfile, preload process for prevfile (snd-clm) */

static GtkWidget *fs1, *fs3;

static void start_view_files_dialog(bool managed)
{
  /* fire up a dialog window with a list of currently open files, 
   * currently selected file also selected in list
   * use snd_info label as is (short-form with '*' etc)
   * secondary list of previously edited files (if still in existence) --
   * click here re-opens the file.  (The overall form is similar to the regions browser).
   * The previous files list requires that we keep such a list as we go along, on the
   * off-chance this browser will be fired up.  (Such files may be subsequently moved or deleted).
   */
  if (!view_files_dialog)
    {
      ww_info *wwl;
      GtkWidget *mainform, *curform, *prevform, *updateB, *helpB, *dismissB, *clearB, *sep;
      vf_selected_file = -1;
      view_files_dialog = snd_gtk_dialog_new();
      SG_SIGNAL_CONNECT(view_files_dialog, "delete_event", view_files_delete_callback, NULL);
      gtk_window_set_title(GTK_WINDOW(view_files_dialog), _("Files"));
      sg_make_resizable(view_files_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(view_files_dialog), 10);
      gtk_window_resize(GTK_WINDOW(view_files_dialog), 400, 200);
      gtk_widget_realize(view_files_dialog);

      helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(helpB, "help_button");

      dismissB = gtk_button_new_with_label(_("Dismiss"));
      gtk_widget_set_name(dismissB, "quit_button");

      updateB = gtk_button_new_with_label(_("Update"));
      gtk_widget_set_name(updateB, "doit_button");

      clearB = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
      gtk_widget_set_name(clearB, "reset_button");

      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), dismissB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), updateB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), clearB, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), helpB, true, true, 10);
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
      
      curform = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(mainform), curform, true, true, 0);
      gtk_widget_show(curform);

      sep = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(mainform), sep, false, false, 0);
      gtk_widget_show(sep);

      prevform = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(mainform), prevform, true, true, 0);
      gtk_widget_show(prevform);

      /* current files section: save play current files | files */
      wwl = make_title_row(curform, _("play"), _("current files"), PAD_TITLE_ON_RIGHT, WITHOUT_SORT_BUTTON, WITHOUT_PANED_WINDOW);
      fs1 = wwl->tophbox;
      
      vf_curww = wwl->list; /* different from Motif */
      vf_curlst = wwl->list;
      FREE(wwl); 
      wwl = NULL;

      /* previous files section: play previous files | files */
      wwl = make_title_row(prevform, _("play"), _("previous files"), PAD_TITLE_ON_LEFT, WITH_SORT_BUTTON, WITHOUT_PANED_WINDOW);
      fs3 = wwl->tophbox;

      SG_SIGNAL_CONNECT(wwl->byname,  "activate", sort_prevfiles_by_name, NULL);
      SG_SIGNAL_CONNECT(wwl->bydate,  "activate", sort_prevfiles_by_date, NULL);
      SG_SIGNAL_CONNECT(wwl->bysize,  "activate", sort_prevfiles_by_size, NULL);
      SG_SIGNAL_CONNECT(wwl->byentry,  "activate", sort_prevfiles_by_entry, NULL);
      SG_SIGNAL_CONNECT(wwl->byproc,  "activate", sort_prevfiles_by_user_procedure, NULL);
      vf_prevww = wwl->list;
      vf_prevlst = wwl->list;
      FREE(wwl); 
      wwl = NULL;
      set_dialog_widget(VIEW_FILES_DIALOG, view_files_dialog);
    }
  make_curfiles_list();
  make_prevfiles_list();
  if (managed) gtk_widget_show(view_files_dialog);
  highlight_selected_sound();
}

void view_files_callback(GtkWidget *w, gpointer context)
{
  start_view_files_dialog(true);
}

GtkWidget *start_file_dialog(bool managed)
{
  start_view_files_dialog(managed);
  return(view_files_dialog);
}

bool view_files_dialog_is_active(void)
{
  return((view_files_dialog) && (GTK_WIDGET_VISIBLE(view_files_dialog)));
}



void g_init_gxfile(void)
{
#if HAVE_SCHEME
  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 0 for the current files list, 1 for previous files, and 2 for regions. The 'position' \
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
The 'type' is 0 for the current files list, 1 for previous files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'."
#endif

  #define H_mouse_leave_label_hook S_mouse_leave_label_hook " (type position label): called when the mouse leaves a file viewer or region label"

  mouse_enter_label_hook = XEN_DEFINE_HOOK(S_mouse_enter_label_hook, 3, H_mouse_enter_label_hook);
  mouse_leave_label_hook = XEN_DEFINE_HOOK(S_mouse_leave_label_hook, 3, H_mouse_leave_label_hook);
}


