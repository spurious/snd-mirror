#include "snd.h"

/* most of these dialogs present a view of the various file header possibilities */

#define NUM_VISIBLE_HEADERS 4

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples)
{
  char *str;
  int res, val;
  off_t oval;
  char *comment = NULL;
  if (fdat->srate_text) 
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->srate_text)); 
      if (str) 
	{
	  val = string2int(str);
	  if (val > 0) (*srate) = val;
	}
    }
  if (fdat->chans_text) 
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->chans_text)); 
      if (str) 
	{
	  val = string2int(str);
	  if (val > 0) (*chans) = val;
	}
    }
  if (fdat->location_text) 
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->location_text)); 
      if (str)
	{
	  oval = string2off_t(str);
	  if (oval >= 0) (*location) = oval;
	}
    }
  if (fdat->samples_text) 
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->samples_text)); 
      if (str)
	{
	  oval = string2off_t(str);
	  if (oval >= 0) (*samples) = oval;
	}
    }
  if (fdat->comment_text) 
    {
      if (GTK_IS_TEXT_VIEW(fdat->comment_text))
	comment = sg_get_text(fdat->comment_text, 0, -1);
      else comment = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->comment_text)); 
    }
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
  if (comment)
    {
      str = copy_string(comment);
      return(str);
    }
  return(NULL);
}

static void load_header_and_data_lists(file_data *fdat, int type, int format, int srate, int chans, off_t location, off_t samples, char *comment)
{
  int i;
  char **fl = NULL;
  char *str;
  fdat->current_type = type;
  fdat->current_format = format;
  fl = set_header_and_data_positions(fdat, type, format); 
  g_signal_handlers_block_matched(GTK_OBJECT(fdat->header_list), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)fdat);
  sg_list_select(fdat->header_list, fdat->header_pos);
  g_signal_handlers_unblock_matched(GTK_OBJECT(fdat->header_list), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)fdat);
  gtk_list_store_clear(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(fdat->format_list))));
  for (i = 0; i < fdat->formats; i++) 
    {
      str = fl[i];
      sg_list_insert(fdat->format_list, i, str);
    }
  g_signal_handlers_block_matched(GTK_OBJECT(fdat->format_list), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)fdat);
  sg_list_select(fdat->format_list, fdat->format_pos);
  g_signal_handlers_unblock_matched(GTK_OBJECT(fdat->format_list), G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)fdat);
  if ((srate > 0) && (fdat->srate_text))
    {
      str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(str, LABEL_BUFFER_SIZE, "%d", srate);
      gtk_entry_set_text(GTK_ENTRY(fdat->srate_text), str);
      FREE(str);
    }
  if ((chans > 0) && (fdat->chans_text))
    {
      str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(str, LABEL_BUFFER_SIZE, "%d", chans);
      gtk_entry_set_text(GTK_ENTRY(fdat->chans_text), str);
      FREE(str);
    }
  if (fdat->comment_text) 
    {
      if (GTK_IS_TEXT_VIEW(fdat->comment_text))
	{
	  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(fdat->comment_text)), "", 0);
	  sg_text_insert(fdat->comment_text, comment);
	}
      else gtk_entry_set_text(GTK_ENTRY(fdat->comment_text), comment);
    }
  if ((location >= 0) && (fdat->location_text))
    {
      str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(str, LABEL_BUFFER_SIZE, OFF_TD, location);
      gtk_entry_set_text(GTK_ENTRY(fdat->location_text), str);
      FREE(str);
    }
  if ((samples >= 0) && (fdat->samples_text))
    {
      str = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(str, LABEL_BUFFER_SIZE, OFF_TD, samples);
      gtk_entry_set_text(GTK_ENTRY(fdat->samples_text), str);
      FREE(str);
    }
}

static void unpad(gpointer w, gpointer data)
{
  if (GTK_IS_CELL_RENDERER_TEXT(w))
    GTK_CELL_RENDERER(w)->ypad = 0;
}

static GtkWidget *snd_gtk_file_selection_new(char *title, GtkSignalFunc gdelete, GtkSignalFunc ok, GtkSignalFunc cancel)
{
  GtkWidget *new_dialog, *entry;
  GList *cells;
  GtkTreeViewColumn *dirl;
  GtkFileSelection *filer;
  new_dialog = gtk_file_selection_new(title);
  filer = GTK_FILE_SELECTION(new_dialog);

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

  g_signal_connect_closure_by_id(GTK_OBJECT(new_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(new_dialog))),
				 0,
				 g_cclosure_new(gdelete, NULL, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(filer->ok_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(filer->ok_button))),
				 0,
				 g_cclosure_new(ok, NULL, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(filer->cancel_button), 
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(filer->cancel_button))),
				 0,
				 g_cclosure_new_swap(cancel, NULL, 0),
				 0);

  gtk_widget_set_name(filer->ok_button, "doit_button");
  gtk_widget_set_name(filer->cancel_button, "quit_button");
  if (filer->fileop_c_dir) gtk_widget_set_name(filer->fileop_c_dir, "help_button");
  if (filer->fileop_del_file) gtk_widget_set_name(filer->fileop_del_file, "doit_again_button");
  if (filer->fileop_ren_file) gtk_widget_set_name(filer->fileop_ren_file, "reset_button");
  return(new_dialog);
}


/* -------- Open/Mix File Dialogs -------- */

typedef struct {
  int file_dialog_read_only, need_update, new_file_written;
  GtkWidget *dialog, *play_selected_button, *dialog_frame, *dialog_info1, *dialog_info2, *dialog_vbox, *playb, *just_sounds_button;
  snd_info *file_play_sp;
} file_dialog_info;

static void file_dialog_stop_playing(file_dialog_info *fd)
{
  if ((fd->file_play_sp) && 
      (fd->file_play_sp->playing)) 
    {
      stop_playing_sound(fd->file_play_sp);
      fd->file_play_sp = NULL;
    }
}

void alert_new_file(void) {}

static void dialog_select_callback(GtkTreeSelection *selection, gpointer context)
{
  char *filename;
  char *buf;
  char timestr[64];
  time_t date;
  file_dialog_info *fd = (file_dialog_info *)context;
  filename = (char *)gtk_file_selection_get_filename(GTK_FILE_SELECTION(fd->dialog));
  if ((filename) && (sound_file_p(filename)))
    {
      buf = (char *)CALLOC(LABEL_BUFFER_SIZE, sizeof(char));
      mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s: %d chan%s, %d Hz, %.3f secs",
		   filename_without_home_directory(filename),
		   mus_sound_chans(filename),
		   (mus_sound_chans(filename) > 1) ? "s" : "",
		   mus_sound_srate(filename),
		   mus_sound_duration(filename));
      gtk_label_set_text(GTK_LABEL(fd->dialog_info1), buf);
      date = mus_sound_write_date(filename);
#if (!defined(HAVE_CONFIG_H)) || HAVE_STRFTIME
      strftime(timestr, 64, ", %d-%b-%Y", localtime(&date));
#else
      sprintf(timestr, "");
#endif
      mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s %s%s",
		   mus_header_type_name(mus_sound_header_type(filename)),
		   mus_short_data_format_name(mus_sound_data_format(filename)),
		   timestr);
      gtk_label_set_text(GTK_LABEL(fd->dialog_info2), buf);
      FREE(buf);
      gtk_widget_show(fd->dialog_frame);
      gtk_widget_show(fd->dialog_vbox);
      gtk_widget_show(fd->dialog_info1);
      gtk_widget_show(fd->dialog_info2);
      gtk_widget_show(fd->playb);
    }
  else
    {
      gtk_widget_hide(fd->dialog_frame);
      gtk_widget_hide(fd->dialog_vbox);
      gtk_widget_hide(fd->dialog_info1);
      gtk_widget_hide(fd->dialog_info2);
      gtk_widget_hide(fd->playb);
    }
}

void clear_deleted_snd_info(void *data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  fd->file_play_sp = NULL;
}

static bool just_sounds_state = false;

static char *just_directory(const char *uname)
{
  int i, len, last_slash;
  char *name;
  last_slash = 0;
  name = copy_string(uname);
  len = strlen(name);
  if (name[len - 1] == '/') 
    return(name);
  for (i = len - 2; i > 0; i--)
    if (name[i] == '/') 
      {
	name[i + 1] = '\0';
	return(name);
      }
  return(NULL);
}

static void prune_file_list(GtkFileSelection *filer)
{
  GtkListStore *list_store;  
  GtkTreeIter iter;
  GtkTreeModel *tree_model;
  char *dirname;
  bool more_files = true;
  tree_model = gtk_tree_view_get_model(GTK_TREE_VIEW(filer->file_list));
  list_store = GTK_LIST_STORE(tree_model);
  dirname = just_directory((char *)gtk_file_selection_get_filename(filer)); /* unused but perhaps hook or proc? */
  if (gtk_tree_model_get_iter_first(tree_model, &iter))
    {
      while (more_files)
	{
	  gchar *name;
	  gtk_tree_model_get(tree_model, &iter, 0, &name, -1);
	  if (!(sound_file_p(name)))
	    more_files = gtk_list_store_remove(list_store, &iter);
	  else more_files = gtk_tree_model_iter_next(tree_model, &iter);
	}
    }
}

/* TODO: for just-sounds, how to get notification that the file list has changed? */

static void just_sounds_callback(GtkWidget *w, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  if (GTK_TOGGLE_BUTTON(w)->active)
    prune_file_list(GTK_FILE_SELECTION(fd->dialog));
  else gtk_file_selection_complete(GTK_FILE_SELECTION(fd->dialog), ""); /* i.e. re-read dir, make a new file list */
}

static void play_selected_callback(GtkWidget *w, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  char *filename;
  if (GTK_TOGGLE_BUTTON(w)->active)
    {
      if ((fd->file_play_sp) && (fd->file_play_sp->playing)) 
	stop_playing_sound(fd->file_play_sp);
      filename = (char *)gtk_file_selection_get_filename(GTK_FILE_SELECTION(fd->dialog));
      if (mus_file_probe(filename))
	{
	  fd->file_play_sp = make_sound_readable(filename, false);
	  fd->file_play_sp->delete_me = (void *)fd;
	  if (fd->file_play_sp)
	    play_sound(fd->file_play_sp, 0, 
		       NO_END_SPECIFIED, IN_BACKGROUND, 
		       C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 
		       "selected file play", 0);
	}
    }
  else file_dialog_stop_playing(fd);
}

static file_dialog_info *make_file_dialog(int read_only, char *title, snd_dialog_t which_dialog, 
					  GtkSignalFunc file_ok_proc,
					  GtkSignalFunc file_delete_proc,
					  GtkSignalFunc file_dismiss_proc)
{
  file_dialog_info *fd;
  fd = (file_dialog_info *)CALLOC(1, sizeof(file_dialog_info));
  fd->file_dialog_read_only = read_only;
  fd->dialog = snd_gtk_file_selection_new(title,
					  file_delete_proc,
					  file_ok_proc,
					  file_dismiss_proc);

  fd->just_sounds_button = gtk_check_button_new_with_label(_("sound files only"));
  gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(fd->dialog)->main_vbox), fd->just_sounds_button, false, false, 6);
  gtk_widget_show(fd->just_sounds_button);
  g_signal_connect_closure_by_id(GTK_OBJECT(fd->just_sounds_button),
				 g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(fd->just_sounds_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(just_sounds_callback), (gpointer)fd, 0),
				 0);

  /* these start out hidden -- are shown only when a file is selected */
  fd->dialog_frame = gtk_frame_new(NULL);
  gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(fd->dialog)->main_vbox), fd->dialog_frame, true, true, 0);
  gtk_frame_set_shadow_type(GTK_FRAME(fd->dialog_frame), GTK_SHADOW_ETCHED_IN);
  
  fd->dialog_vbox = gtk_vbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(fd->dialog_frame), fd->dialog_vbox);

  fd->dialog_info1 = gtk_label_new(NULL);
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info1, true, true, 2);
	
  fd->dialog_info2 = gtk_label_new(NULL);
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->dialog_info2, true, true, 2);
  g_signal_connect(gtk_tree_view_get_selection(GTK_TREE_VIEW(GTK_FILE_SELECTION(fd->dialog)->file_list)), "changed",
		   G_CALLBACK (dialog_select_callback), fd);

  fd->playb = gtk_check_button_new_with_label(_("play selected sound"));
  gtk_box_pack_start(GTK_BOX(fd->dialog_vbox), fd->playb, true, true, 2);
  g_signal_connect_closure_by_id(GTK_OBJECT(fd->playb),
				 g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(fd->playb))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(play_selected_callback), (gpointer)fd, 0),
				 0);

  set_dialog_widget(which_dialog, fd->dialog);
  return(fd);
}

static file_dialog_info *open_dialog = NULL;
static file_dialog_info *mix_dialog = NULL;

void set_open_file_play_button(bool val) 
{
  if ((open_dialog) && (open_dialog->playb))
    set_toggle_button(open_dialog->playb, val, false, (gpointer)open_dialog);
  if ((mix_dialog) && (mix_dialog->playb))
    set_toggle_button(mix_dialog->playb, val, false, (gpointer)mix_dialog);
}

static void file_open_dialog_ok(GtkWidget *w, gpointer data)
{
  snd_info *sp;
  char *filename;
  filename = (char *)gtk_file_selection_get_filename(GTK_FILE_SELECTION(open_dialog->dialog));
  gtk_widget_hide(open_dialog->dialog);
  file_dialog_stop_playing(open_dialog);
  if (!(directory_p(filename)))
    {
      sp = snd_open_file(filename, open_dialog->file_dialog_read_only);
      if (sp) select_channel(sp, 0);           /* add_sound_window (snd-xsnd.c) will report reason for error, if any */
    }
  else snd_error(_("%s is a directory"), filename);
}

static void file_open_dialog_dismiss(GtkWidget *w, gpointer context)
{
  file_dialog_stop_playing(open_dialog);
  gtk_widget_hide(open_dialog->dialog);
}

static void file_open_dialog_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(open_dialog);
  gtk_widget_hide(open_dialog->dialog);
}

void make_open_file_dialog(bool read_only, bool managed)
{
  if (!open_dialog)
    open_dialog = make_file_dialog(read_only, _("File"), FILE_OPEN_DIALOG,
				   (GtkSignalFunc)file_open_dialog_ok,				     
				   (GtkSignalFunc)file_open_dialog_delete,
				   (GtkSignalFunc)file_open_dialog_dismiss);
  if (managed) gtk_widget_show(open_dialog->dialog);
}


/* -------- mix file dialog -------- */

static void file_mix_cancel_callback(GtkWidget *w, gpointer context)
{
  file_dialog_stop_playing(mix_dialog);
  gtk_widget_hide(mix_dialog->dialog);
}

static void file_mix_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(mix_dialog);
  gtk_widget_hide(mix_dialog->dialog);
}

static void file_mix_ok_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(mix_dialog->dialog);
  file_dialog_stop_playing(mix_dialog);
  mix_complete_file_at_cursor(any_selected_sound(),
			      (char *)gtk_file_selection_get_filename(GTK_FILE_SELECTION(mix_dialog->dialog)),
			      "File: mix", with_mix_tags(ss));
}

void make_mix_file_dialog(bool managed)
{
  if (mix_dialog == NULL)
    mix_dialog = make_file_dialog(false, _("mix file:"), FILE_MIX_DIALOG,
				  (GtkSignalFunc)file_mix_ok_callback,
				  (GtkSignalFunc)file_mix_delete_callback,
				  (GtkSignalFunc)file_mix_cancel_callback);
  if (managed) gtk_widget_show(mix_dialog->dialog);
}



/* -------- file data choices -------- */

#define NUM_HEADER_TYPES 7
static char *header_short_names[NUM_HEADER_TYPES] = {"sun  ", "aifc ", "wave ", "raw  ", "aiff ", "ircam", "nist "};

static void gfile_header_type(file_data *fd, int row)
{
  fd->header_pos = row;
  if (fd->current_type != row)
    {
      set_header_type_and_format_from_position(fd, row);
      load_header_and_data_lists(fd,
				 fd->current_type,
				 fd->current_format,
				 0, 0, -1, -1, NULL);
    }
}

static void save_as_header_type_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value;
  int i;
  GtkTreeModel *model;
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  for (i = 0; i < NUM_HEADER_TYPES; i++)
    if (strcmp(value, header_short_names[i]) == 0)
      {
	gfile_header_type((file_data *)gp, i);
	return;
      }
}

static void save_as_data_format_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value;
  int i;
  GtkTreeModel *model;
  int dformats = 0;
  char **formats = NULL;
  file_data *fd = (file_data *)gp;
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
}

file_data *make_file_data_panel(GtkWidget *parent, char *name, 
				bool with_chan, int header_type, int data_format, bool with_loc, bool comment_as_entry, bool with_samples)
{
  GtkWidget *form, *slab, *clab, *comment_label, *loclab, *scbox, *combox, *samplab;
  file_data *fdat;
  int dformats = 0;
  char **formats = NULL;
  fdat = (file_data *)CALLOC(1, sizeof(file_data));
  fdat->current_type = header_type;
  fdat->current_format = data_format;
  formats = set_header_positions_from_type(fdat, header_type, data_format);
  dformats = fdat->formats;

  form = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(parent), form, false, false, 4); /* ??? */
  gtk_widget_show(form);

  fdat->header_list = sg_make_list(_("header"), form, BOX_PACK, (gpointer)fdat, NUM_HEADER_TYPES, header_short_names, 
				   GTK_SIGNAL_FUNC(save_as_header_type_callback), 0, 0, 0, 0);
  fdat->format_list = sg_make_list(_("data"), form, BOX_PACK, (gpointer)fdat, dformats, formats, 
				   GTK_SIGNAL_FUNC(save_as_data_format_callback), 0, 0, 0, 0);
  sg_list_select(fdat->header_list, fdat->header_pos);
  sg_list_select(fdat->format_list, fdat->format_pos);

  gtk_widget_show(fdat->header_list);
  gtk_widget_show(fdat->format_list);

  scbox = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(form), scbox, false, false, 4);
  gtk_widget_show(scbox);

  slab = snd_gtk_label_new(_("srate:"), ss->sgx->highlight_color);
  gtk_box_pack_start(GTK_BOX(scbox), slab, false, false, 0);
  gtk_widget_show(slab);

  fdat->srate_text = snd_entry_new(scbox, true);

  if (with_chan)
    {
      clab = snd_gtk_label_new(_("chans:"), ss->sgx->highlight_color);
      gtk_box_pack_start(GTK_BOX(scbox), clab, false, false, 0);
      gtk_widget_show(clab);

      fdat->chans_text = snd_entry_new(scbox, true);
      
      if (with_loc)
	{
	  loclab = snd_gtk_label_new(_("location:"), ss->sgx->highlight_color);
	  gtk_box_pack_start(GTK_BOX(scbox), loclab, false, false, 0);
	  gtk_widget_show(loclab);

	  fdat->location_text = snd_entry_new(scbox, true);
	}
    }

  if (with_samples)
    {
      samplab = snd_gtk_label_new(_("samples:"), ss->sgx->highlight_color);
      gtk_box_pack_start(GTK_BOX(scbox), samplab, false, false, 0);
      gtk_widget_show(samplab);

      fdat->samples_text = snd_entry_new(scbox, true);
    }

  combox = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(parent), combox, true, true, 4);
  gtk_widget_show(combox);

  comment_label = snd_gtk_label_new(_("comment:"), ss->sgx->highlight_color);
  gtk_entry_set_width_chars(GTK_ENTRY(comment_label), 2 + strlen(_("comment:")));
  gtk_box_pack_start(GTK_BOX(combox), comment_label, false, false, 0);
  gtk_widget_show(comment_label);

  if (comment_as_entry)
    {
      /* try to kludge around a gtk bug -- this is not needed in motif */
      fdat->comment_text = snd_entry_new(combox, true);
    }
  else
    {
      GtkWidget *frame;
      frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(combox), frame, true, true, 4);  
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
      gtk_widget_show(frame);
      fdat->comment_text = make_scrolled_text(frame, true, NULL, NULL);
      connect_mouse_to_text(fdat->comment_text);
    }

  return(fdat);
}


/* -------- save as dialog (file and edit menus) -------- */

static file_data *save_as_file_data = NULL;
static GtkWidget *save_as_dialog = NULL;
static save_dialog_t save_as_dialog_type = FILE_SAVE_AS;
static char *last_save_as_filename = NULL;

static void save_as_ok_callback(GtkWidget *w, gpointer data)
{
  char *str = NULL, *comment = NULL, *fullname = NULL;
  int i, type, format, srate, opened = -1;
  snd_info *sp;
  str = (char *)gtk_entry_get_text(GTK_ENTRY(save_as_file_data->srate_text));
  srate = string2int(str);
  str = NULL;
  if (GTK_IS_TEXT_VIEW(save_as_file_data->comment_text))
    comment = sg_get_text(save_as_file_data->comment_text, 0, -1);
  else comment = (char *)gtk_entry_get_text(GTK_ENTRY(save_as_file_data->comment_text)); 
  type = save_as_file_data->current_type;
  format = save_as_file_data->current_format;
  last_save_as_filename = (char *)gtk_file_selection_get_filename(GTK_FILE_SELECTION(save_as_dialog));
  sp = any_selected_sound();
  if (last_save_as_filename)
    opened = check_for_filename_collisions_and_save(sp, last_save_as_filename, save_as_dialog_type, srate, type, format, comment);
  else 
    if (sp) 
      report_in_minibuffer(sp, _("not saved (no name given)"));
  gtk_widget_hide(save_as_dialog);
  if ((sp) && (last_save_as_filename) && (emacs_style_save_as(ss)) &&
      (save_as_dialog_type == FILE_SAVE_AS) && 
      (opened == 0))
    {
      for (i = 0; i < sp->nchans; i++) 
	sp->chans[i]->edit_ctr = 0; /* don't trigger close-hook unsaved-edit checks */
      snd_close_file(sp);
      fullname = mus_expand_filename(last_save_as_filename);
      snd_open_file(fullname, false); /* false = not read_only */
      FREE(fullname);
    }
} 

static void save_as_cancel_callback(GtkWidget *w, gpointer data)
{ 
  gtk_widget_hide(save_as_dialog);
} 

static void save_as_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(save_as_dialog);
}

static void make_save_as_dialog(char *sound_name, int header_type, int format_type)
{
  /* save old as new, close old, open new */
  GtkWidget *fbox;
  if (!save_as_dialog)
    {
      save_as_dialog = snd_gtk_file_selection_new(_("save as:"),
						  (GtkSignalFunc)save_as_delete_callback,
						  (GtkSignalFunc)save_as_ok_callback,
						  (GtkSignalFunc)save_as_cancel_callback);
      fbox = gtk_vbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(save_as_dialog)->main_vbox), fbox, true, true, 0);
      gtk_widget_show(fbox);

      save_as_file_data = make_file_data_panel(fbox, "data-form", false, header_type, format_type, false, false, true);
      set_dialog_widget(FILE_SAVE_AS_DIALOG, save_as_dialog);
    }
}

void make_file_save_as_dialog(void)
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
			     (hdr) ? hdr->srate : selection_srate(), 0, -1,
			     (hdr) ? hdr->samples : selection_len(),
			     com = output_comment(hdr));
  if (com) FREE(com);
  gtk_widget_show(save_as_dialog);
}

void make_edit_save_as_dialog(void)
{
  save_as_dialog_type = EDIT_SAVE_AS;
  make_save_as_dialog(_("current selection"), -1, -1);
  load_header_and_data_lists(save_as_file_data,
			     save_as_file_data->current_type,
			     save_as_file_data->current_format,
			     selection_srate(), 0, -1,
			     selection_len(),
			     NULL);
  gtk_widget_show(save_as_dialog);
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
  GtkWidget *rlw, *sep1, *cww, *phbox;
  GtkWidget *smenu, *sbar, *sitem;
  ww_info *wwi;
  wwi = (ww_info *)CALLOC(1, sizeof(ww_info));

  /* assuming "formw" is a vbox */
  if (main_str)
    {
      
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

static XEN mouse_enter_label_hook;
static XEN mouse_leave_label_hook;

static gint mouse_name(XEN hook, GtkWidget *w, const char *caller)
{
  char *label = NULL;
  regrow *r;
  if (XEN_HOOKED(hook))
    {
      r = (regrow *)get_user_data(G_OBJECT(w));
      if (r)
	{
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
		     XEN_LIST_3(C_TO_SMALL_XEN_INT(r->parent),
				C_TO_SMALL_XEN_INT(r->pos),
				C_TO_XEN_STRING(label)),
		     caller);
	}
    }
  return(0);
}

static gint label_enter_callback(GtkWidget *w, GdkEventCrossing *ev)
{
  return(mouse_name(mouse_enter_label_hook, w, S_mouse_enter_label_hook));
}

static gint label_leave_callback(GtkWidget *w, GdkEventCrossing *ev)
{
  return(mouse_name(mouse_leave_label_hook, w, S_mouse_leave_label_hook));
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
  g_signal_connect_closure_by_id(GTK_OBJECT(r->pl),
				 g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(r->pl))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(play_callback), (gpointer)r, 0),
				 0);
  gtk_widget_show(r->pl);

  r->nm = gtk_button_new_with_label("");
  sg_left_justify_button(r->nm);
  gtk_box_pack_start(GTK_BOX(r->rw), r->nm, true, true, 2);
  g_signal_connect_closure_by_id(GTK_OBJECT(r->nm),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(r->nm))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(name_callback), (gpointer)r, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(r->nm),
				 g_signal_lookup("enter_notify_event", G_OBJECT_TYPE(GTK_OBJECT(r->nm))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(label_enter_callback), (gpointer)r, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(r->nm),
				 g_signal_lookup("leave_notify_event", G_OBJECT_TYPE(GTK_OBJECT(r->nm))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(label_leave_callback), (gpointer)r, 0),
				 0);
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
  int i;
  if (cur_name_row == NULL)
    cur_name_row = (regrow **)CALLOC(new_size, sizeof(regrow *));
  else 
    {
      cur_name_row = (regrow **)REALLOC(cur_name_row, new_size * sizeof(regrow *));
      for (i = old_size; i < new_size; i++) cur_name_row[i] = NULL;
    }
}

void make_prev_name_row(int old_size, int new_size)
{
  int i;
  if (prev_name_row == NULL)
    prev_name_row = (regrow **)CALLOC(new_size, sizeof(regrow *));
  else 
    {
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

static void view_files_help_callback(GtkWidget *w, gpointer context) 
{
  view_files_dialog_help();
}

static void view_files_dismiss_callback(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(view_files_dialog);
}

static void view_files_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(view_files_dialog);
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
  if (file_dialog_is_active()) make_prevfiles_list();
}

void set_file_browser_play_button(char *name, int state)
{
  int i, list;
  regrow *r;
  list = 0;
  if (file_dialog_is_active())
    {
      i = find_curfile_regrow(name); 
      if (i != -1) list = 1; else i = find_prevfile_regrow(name);
      if (i != -1)
	{
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
  regrow *r;
  if (file_dialog_is_active())
    {
      if (vf_selected_file != -1)
	{
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
  regrow *r;
  if (file_dialog_is_active())
    {
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
  int i;
  sp = selected_sound();
  if (sp)
    {
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
void view_files_callback(GtkWidget *w, gpointer context)
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

  ww_info *wwl;
  GtkWidget *mainform, *curform, *prevform, *updateB, *helpB, *dismissB, *clearB, *sep;
  if (!view_files_dialog)
    {
      vf_selected_file = -1;

      view_files_dialog = snd_gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(view_files_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(view_files_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(view_files_delete_callback), NULL, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(view_files_dialog), _("Files"));
      sg_make_resizable(view_files_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(view_files_dialog), 10);
      gtk_window_resize(GTK_WINDOW(view_files_dialog), 400, 200);
      gtk_widget_realize(view_files_dialog);

      helpB = gtk_button_new_with_label(_("Help"));
      gtk_widget_set_name(helpB, "help_button");
      dismissB = gtk_button_new_with_label(_("Dismiss"));
      gtk_widget_set_name(dismissB, "quit_button");
      updateB = gtk_button_new_with_label(_("Update"));
      gtk_widget_set_name(updateB, "doit_button");
      clearB = gtk_button_new_with_label(_("Clear"));
      gtk_widget_set_name(clearB, "reset_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), dismissB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), updateB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), clearB, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), helpB, true, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(dismissB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(dismissB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(view_files_dismiss_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(helpB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(helpB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(view_files_help_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(updateB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(updateB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(view_files_update_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(clearB),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(clearB))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(view_files_clear_callback), NULL, 0),
				     0);
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

      g_signal_connect_closure_by_id(GTK_OBJECT(wwl->byname), 
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(wwl->byname))), 
				     0,
				     g_cclosure_new_swap(GTK_SIGNAL_FUNC(sort_prevfiles_by_name), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(wwl->bydate), 
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(wwl->bydate))), 
				     0,
				     g_cclosure_new_swap(GTK_SIGNAL_FUNC(sort_prevfiles_by_date), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(wwl->bysize), 
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(wwl->bysize))),
				     0,
				     g_cclosure_new_swap(GTK_SIGNAL_FUNC(sort_prevfiles_by_size), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(wwl->byentry), 
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(wwl->byentry))), 
				     0,
				     g_cclosure_new_swap(GTK_SIGNAL_FUNC(sort_prevfiles_by_entry), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(wwl->byproc), 
				     g_signal_lookup("activate", G_OBJECT_TYPE(GTK_OBJECT(wwl->byproc))), 
				     0,
				     g_cclosure_new_swap(GTK_SIGNAL_FUNC(sort_prevfiles_by_user_procedure), NULL, 0),
				     0);

      vf_prevww = wwl->list;
      vf_prevlst = wwl->list;
      FREE(wwl); 
      wwl = NULL;
      set_dialog_widget(VIEW_FILES_DIALOG, view_files_dialog);
    }
  gtk_widget_show(view_files_dialog);
  make_curfiles_list();
  make_prevfiles_list();
  highlight_selected_sound();
}

GtkWidget *start_file_dialog(int width, int height)
{
  view_files_callback(NULL, NULL);
  if (width > 0) set_widget_size(view_files_dialog, width, height);
  return(view_files_dialog);
}

bool file_dialog_is_active(void)
{
  return((view_files_dialog) && (GTK_WIDGET_VISIBLE(view_files_dialog)));
}


/* -------------------------------- Raw Data Dialog -------------------------------- */

static GtkWidget *raw_data_dialog = NULL;
static GtkWidget *raw_srate_text, *raw_chans_text, *raw_location_text, *raw_data_label;
static off_t raw_data_location = 0;
static bool raw_cancelled = false, raw_done = false;

static void raw_data_ok_callback(GtkWidget *w, gpointer context) {raw_cancelled = false; raw_done = true;}
static void raw_data_cancel_callback(GtkWidget *w, gpointer context) {raw_cancelled = true; raw_done = true;}
static void raw_data_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context) {raw_cancelled = true; raw_done = true;}

static void raw_data_default_callback(GtkWidget *w, gpointer context) 
{
  raw_cancelled = false; 
  raw_done = true;
}

static char **data_formats = NULL;
static char **data_format_names(void)
{
  int i;
  if (data_formats == NULL)
    {
      data_formats = (char **)CALLOC(MUS_LAST_DATA_FORMAT, sizeof(char *));
      for (i = 0; i < MUS_LAST_DATA_FORMAT; i++)
	data_formats[i] = (char *)mus_data_format_name(i + 1);
    }
  return(data_formats);
}

/* must parallel sndlib.h definitions */


static void raw_data_browse_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value;
  int i;
  GtkTreeModel *model;
  char **names;
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  names = data_format_names();
  for (i = 0; i < MUS_LAST_DATA_FORMAT; i++)
    if (strcmp(value, names[i]) == 0)
      {
	int sr, oc, fr;
	mus_header_raw_defaults(&sr, &oc, &fr);
	fr = i + 1;
	mus_header_set_raw_defaults(sr, oc, fr);
	return;
      }
}

static void raw_data_help_callback(GtkWidget *w, gpointer context) 
{
  raw_data_dialog_help();
}

static void make_raw_data_dialog(void)
{
  char dfs_str[LABEL_BUFFER_SIZE];
  int sr, oc, fr;
  GtkWidget *lst, *dls, *dloclab, *chnlab;
  GtkWidget *defaultB, *helpB, *cancelB, *okB, *sratehbox, *lochbox;
  mus_header_raw_defaults(&sr, &oc, &fr);
  raw_data_dialog = snd_gtk_dialog_new();
  g_signal_connect_closure_by_id(GTK_OBJECT(raw_data_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(raw_data_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(raw_data_delete_callback), NULL, 0),
				 0);
  gtk_window_set_title(GTK_WINDOW(raw_data_dialog), _("No Header on File"));
  sg_make_resizable(raw_data_dialog);
  gtk_container_set_border_width(GTK_CONTAINER(raw_data_dialog), 10);
  gtk_window_resize(GTK_WINDOW(raw_data_dialog), 350, 260);
  gtk_widget_realize(raw_data_dialog);

  helpB = gtk_button_new_with_label(_("Help"));
  gtk_widget_set_name(helpB, "help_button");
  cancelB = gtk_button_new_with_label(_("Cancel"));
  gtk_widget_set_name(cancelB, "quit_button");
  defaultB = gtk_button_new_with_label(_("Default"));
  gtk_widget_set_name(defaultB, "reset_button");
  okB = gtk_button_new_with_label(_("Ok"));
  gtk_widget_set_name(okB, "doit_button");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), okB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), defaultB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), cancelB, true, true, 10);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), helpB, true, true, 10);
  g_signal_connect_closure_by_id(GTK_OBJECT(okB),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(okB))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(raw_data_ok_callback), NULL, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(helpB),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(helpB))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(raw_data_help_callback), NULL, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(defaultB),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(defaultB))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(raw_data_default_callback), NULL, 0),
				 0);
  g_signal_connect_closure_by_id(GTK_OBJECT(cancelB),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(cancelB))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(raw_data_cancel_callback), NULL, 0),
				 0);
  gtk_widget_show(okB);
  gtk_widget_show(cancelB);
  gtk_widget_show(helpB);
  gtk_widget_show(defaultB);

  raw_data_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->vbox), raw_data_label, false, false, 6);
  gtk_widget_show(raw_data_label);

  sratehbox = gtk_hbox_new(false, 2);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->vbox), sratehbox, false, false, 6);
  gtk_widget_show(sratehbox);

  dls = gtk_label_new(_("srate:"));
  gtk_box_pack_start(GTK_BOX(sratehbox), dls, false, false, 4);
  gtk_widget_show(dls);

  raw_srate_text = snd_entry_new(sratehbox, true);
  mus_snprintf(dfs_str, LABEL_BUFFER_SIZE, "%d", sr);
  gtk_entry_set_text(GTK_ENTRY(raw_srate_text), dfs_str);

  chnlab = gtk_label_new(_("chans:"));
  gtk_box_pack_start(GTK_BOX(sratehbox), chnlab, false, false, 4);
  gtk_widget_show(chnlab);

  raw_chans_text = snd_entry_new(sratehbox, true);
  mus_snprintf(dfs_str, LABEL_BUFFER_SIZE, "%d", oc);
  gtk_entry_set_text(GTK_ENTRY(raw_chans_text), dfs_str);

  lochbox = gtk_hbox_new(false, 2);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->vbox), lochbox, false, false, 6);
  gtk_widget_show(lochbox);

  dloclab = gtk_label_new(_("data location:"));
  gtk_box_pack_start(GTK_BOX(lochbox), dloclab, false, false, 4);
  gtk_widget_show(dloclab);

  raw_location_text = snd_entry_new(lochbox, true);
  gtk_entry_set_text(GTK_ENTRY(raw_location_text), "0");
  lst = sg_make_list(_("data format:"), GTK_DIALOG(raw_data_dialog)->vbox, BOX_PACK, NULL, MUS_LAST_DATA_FORMAT, data_format_names(),
		     GTK_SIGNAL_FUNC(raw_data_browse_callback), 0, 0, 0, 0);
  gtk_widget_show(lst);
  set_dialog_widget(RAW_DATA_DIALOG, raw_data_dialog);
}

file_info *raw_data_dialog_to_file_info(const char *filename, const char *title)
{
  char *str;
  file_info *hdr = NULL;
  int sr, oc, fr;
  if (!raw_data_dialog) make_raw_data_dialog();
  gtk_label_set_text(GTK_LABEL(raw_data_label), title);
  reflect_raw_pending_in_menu();
  raw_done = false;
  gtk_widget_show(raw_data_dialog);
  while (!raw_done) gtk_main_iteration();
  gtk_widget_hide(raw_data_dialog);
  reflect_raw_open_in_menu();
  if (raw_cancelled) return(NULL);
  mus_header_raw_defaults(&sr, &oc, &fr);
  str = (char *)gtk_entry_get_text(GTK_ENTRY(raw_srate_text));
  if ((str) && (*str)) 
    {
      sr = string2int(str);
      if (sr <= 0) sr = 1;
    }
  str = (char *)gtk_entry_get_text(GTK_ENTRY(raw_chans_text));
  if ((str) && (*str)) 
    {
      oc = string2int(str);
      if (oc <= 0) oc = 1;
    }
  str = (char *)gtk_entry_get_text(GTK_ENTRY(raw_location_text));
  if ((str) && (*str)) 
    {
      raw_data_location = string2int(str);
      if (raw_data_location < 0) raw_data_location = 0;
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

/* no longer shares with raw data -- 11-Nov-99 */

static bool new_file_cancelled = false, new_file_done = false;
static GtkWidget *new_dialog = NULL, *new_file_name;
static file_data *new_dialog_data = NULL;

static void new_file_ok_callback(GtkWidget *w, gpointer context) {new_file_cancelled = false; new_file_done = true;}
static void new_file_cancel_callback(GtkWidget *w, gpointer context) {new_file_cancelled = true; new_file_done = true;}
static void new_file_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context) {new_file_cancelled = true; new_file_done = true;}

static void new_file_help_callback(GtkWidget *w, gpointer context) 
{
  new_file_dialog_help();
}

static off_t initial_samples = 1;

snd_info *make_new_file_dialog(char *newname, int header_type, int data_format, int srate, int chans, char *comment)
{
  off_t loc;
  char *tmpstr, *title, *newer_name = NULL;
  snd_info *sp = NULL;
  GtkWidget *name_label, *hform;
  GtkWidget *help_button, *cancel_button, *ok_button;
  new_file_cancelled = false;
  title = (char *)CALLOC(snd_strlen(newname) + 32, sizeof(char));
  sprintf(title, _("create new sound: %s"), newname);
  if (!new_dialog)
    {
      new_dialog = snd_gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(new_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(new_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(new_file_delete_callback), NULL, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(new_dialog), title);
      sg_make_resizable(new_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(new_dialog), 10);
      gtk_window_resize(GTK_WINDOW(new_dialog), 400, 250);
      gtk_widget_realize(new_dialog);

      help_button = gtk_button_new_with_label(_("Help"));
      gtk_widget_set_name(help_button, "help_button");
      cancel_button = gtk_button_new_with_label(_("Cancel"));
      gtk_widget_set_name(cancel_button, "quit_button");
      ok_button = gtk_button_new_with_label(_("Ok"));
      gtk_widget_set_name(ok_button, "doit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_dialog)->action_area), ok_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_dialog)->action_area), cancel_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(new_dialog)->action_area), help_button, true, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(cancel_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(cancel_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(new_file_cancel_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(new_file_help_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(ok_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(ok_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(new_file_ok_callback), NULL, 0),
				     0);
      gtk_widget_show(cancel_button);
      gtk_widget_show(ok_button);
      gtk_widget_show(help_button);

      hform = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_dialog)->vbox), hform, false, false, 4);
      gtk_widget_show(hform);

      name_label = gtk_label_new(_("New file:"));
      gtk_box_pack_start(GTK_BOX(hform), name_label, false, false, 2);
      gtk_widget_show(name_label);

      new_file_name = snd_entry_new(hform, true);
      gtk_entry_set_text(GTK_ENTRY(new_file_name), newname);

      new_dialog_data = make_file_data_panel(GTK_DIALOG(new_dialog)->vbox, "data-form", true, 
					     default_output_type(ss), default_output_format(ss), false, false, true);
      set_dialog_widget(NEW_FILE_DIALOG, new_dialog);
    }
  else
    {
      gtk_window_set_title(GTK_WINDOW(new_dialog), title);
      gtk_entry_set_text(GTK_ENTRY(new_file_name), newname);
    }
  load_header_and_data_lists(new_dialog_data, header_type, data_format, srate, chans, -1, initial_samples, comment);
  new_file_done = false;
  gtk_widget_show(new_dialog);
  while (!new_file_done) gtk_main_iteration();
  gtk_widget_hide(new_dialog);
  if (new_file_cancelled)
    return(NULL);
  else
    {
      newer_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_name));
      if (newer_name == NULL) return(NULL);
      tmpstr = read_file_data_choices(new_dialog_data, &srate, &chans, &header_type, &data_format, &loc, &initial_samples);
      sp = snd_new_file(newer_name, header_type, data_format, srate, chans, tmpstr, initial_samples);
      if (tmpstr) FREE(tmpstr);
    }
  return(sp);
}



/* ---------------- EDIT_HEADER ---------------- */

static GtkWidget *edit_header_dialog = NULL;
static file_data *edit_header_data;
static snd_info *edit_header_sp = NULL;

static void edit_header_help_callback(GtkWidget *w, gpointer context) 
{
  edit_header_dialog_help();
}

static void edit_header_cancel_callback(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(edit_header_dialog);
}

static void edit_header_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(edit_header_dialog);
}

static void edit_header_ok_callback(GtkWidget *w, gpointer context) 
{
  if (!(edit_header_sp->read_only))
    edit_header_callback(edit_header_sp, edit_header_data);
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
  GtkWidget *help_button, *cancel_button, *save_button;
  char *str;
  file_info *hdr;
  if (!sp) return(NULL);
  edit_header_sp = sp;
  hdr = sp->hdr;

  if (!edit_header_dialog)
    {
      edit_header_dialog = snd_gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(edit_header_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(edit_header_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_header_delete_callback), NULL, 0),
				     0);
      /* gtk_window_set_title(GTK_WINDOW(edit_header_dialog), _("Edit Header")); */
      sg_make_resizable(edit_header_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(edit_header_dialog), 10);
      gtk_window_resize(GTK_WINDOW(edit_header_dialog), 360, 250);
      gtk_widget_realize(edit_header_dialog);

      help_button = gtk_button_new_with_label(_("Help"));
      gtk_widget_set_name(help_button, "help_button");
      cancel_button = gtk_button_new_with_label(_("Cancel"));
      gtk_widget_set_name(cancel_button, "quit_button");
      save_button = gtk_button_new_with_label(_("Save"));
      gtk_widget_set_name(save_button, "doit_button");
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), cancel_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), save_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), help_button, true, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(cancel_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(cancel_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_header_cancel_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_header_help_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(save_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(save_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(edit_header_ok_callback), NULL, 0),
				     0);
      gtk_widget_show(cancel_button);
      gtk_widget_show(save_button);
      gtk_widget_show(help_button);

      edit_header_data = make_file_data_panel(GTK_DIALOG(edit_header_dialog)->vbox, _("Edit Header"), true, 
					      hdr->type, hdr->format, true, false, true);
      set_dialog_widget(EDIT_HEADER_DIALOG, edit_header_dialog);
    }

  str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(str, PRINT_BUFFER_SIZE, _("Edit header of %s"), sp->short_filename);
  gtk_window_set_title(GTK_WINDOW(edit_header_dialog), str);
  FREE(str);

  gtk_widget_show(edit_header_dialog);
  load_header_and_data_lists(edit_header_data, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->samples, hdr->comment);

  return(edit_header_dialog);
}


/* ---------------- POST-IT MONOLOG ---------------- */

#define POST_IT_ROWS 12
#define POST_IT_COLUMNS 56

static GtkWidget *post_it_text = NULL, *post_it_dialog = NULL;

static void dismiss_post_it(GtkWidget *w, gpointer context) {gtk_widget_hide(post_it_dialog);}
static void delete_post_it(GtkWidget *w, GdkEvent *event, gpointer context) {gtk_widget_hide(post_it_dialog);}

static void create_post_it_monolog(void)
{
  /* create scrollable but not editable text window */
  GtkWidget *ok_button;
  post_it_dialog = gtk_dialog_new();
  g_signal_connect_closure_by_id(GTK_OBJECT(post_it_dialog),
				 g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(post_it_dialog))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(delete_post_it), NULL, 0),
				 0);

  gtk_window_set_title(GTK_WINDOW(post_it_dialog), _("Info"));
  sg_make_resizable(post_it_dialog);
  gtk_container_set_border_width(GTK_CONTAINER(post_it_dialog), 10);
  gtk_window_resize(GTK_WINDOW(post_it_dialog), POST_IT_COLUMNS * 9, POST_IT_ROWS * 20);
  gtk_widget_realize(post_it_dialog);

  ok_button = gtk_button_new_with_label(_("Ok"));
  gtk_widget_set_name(ok_button, "quit_button");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(post_it_dialog)->action_area), ok_button, false, true, 20);
  g_signal_connect_closure_by_id(GTK_OBJECT(ok_button),
				 g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(ok_button))),
				 0,
				 g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_post_it), NULL, 0),
				 0);
  gtk_widget_show(ok_button);

  post_it_text = make_scrolled_text(GTK_DIALOG(post_it_dialog)->vbox, false, NULL, NULL);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(post_it_text), 10);
  gtk_widget_show(post_it_dialog);
  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}

void post_it(const char *subject, const char *str)
{
  if (!(post_it_dialog)) create_post_it_monolog(); else raise_dialog(post_it_dialog);
  gtk_window_set_title(GTK_WINDOW(post_it_dialog), subject);
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(post_it_text)), "", 0);
  sg_text_insert(post_it_text, (char *)str);
}


static XEN g_just_sounds(void)
{
  #define H_just_sounds "(" S_just_sounds "): the 'just sounds' button in the file chooser dialog"
  return(C_TO_XEN_BOOLEAN(just_sounds_state));
}

static XEN g_set_just_sounds(XEN on) 
{
  bool n;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_just_sounds, "a boolean");
  n = XEN_TO_C_BOOLEAN(on);
  if (open_dialog) prune_file_list(GTK_FILE_SELECTION(open_dialog->dialog));

  /* TODO: finish tying just-sounds variable into gtk, also just-sounds-hook? */
#if 0
  if ((open_dialog) && (open_dialog->just_sounds_button))
    XmToggleButtonSetState(open_dialog->just_sounds_button, n, true);
  if ((mix_dialog) && (mix_dialog->just_sounds_button))
    XmToggleButtonSetState(mix_dialog->just_sounds_button, n, true);
#endif
  just_sounds_state = n;
  return(C_TO_XEN_BOOLEAN(n));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_just_sounds_w, g_just_sounds)
XEN_NARGIFY_1(g_set_just_sounds_w, g_set_just_sounds)
#else
#define g_just_sounds_w g_just_sounds
#define g_set_just_sounds_w g_set_just_sounds
#endif



void g_init_gxfile(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_just_sounds, g_just_sounds_w, H_just_sounds,
				   S_setB S_just_sounds, g_set_just_sounds_w,  0, 0, 1, 0);

  #define H_mouse_enter_label_hook S_mouse_enter_label_hook " (type position label): called when the mouse enters a file viewer or region label. \
The 'type' is 0 for the current files list, 1 for previous files, and 2 for regions. The 'position' \
is the scrolled list position of the label. The label itself is 'label'. We could use the 'finfo' procedure in examp.scm \
to popup file info as follows: \n\
(add-hook! mouse-enter-label-hook\n\
  (lambda (type position name)\n\
    (if (not (= type 2))\n\
        (info-dialog name (finfo name)))))\n\
See also nb.scm."

  #define H_mouse_leave_label_hook S_mouse_leave_label_hook " (type position label): called when the mouse leaves a file viewer or region label"

  XEN_DEFINE_HOOK(mouse_enter_label_hook, S_mouse_enter_label_hook, 3, H_mouse_enter_label_hook);
  XEN_DEFINE_HOOK(mouse_leave_label_hook, S_mouse_leave_label_hook, 3, H_mouse_leave_label_hook);
}


