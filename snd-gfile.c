/* DIFFS: no "just-sounds" button since the search proc is apparently a simple string
 *          depending on built-in filename completion routine
 *        no forced re-read of directory when file is written (update button?)
 */

#include "snd.h"
#if HAVE_GTKEXTRA
  #include <gtkextra/gtkextra.h>
#endif

/* most of these dialogs present a view of the various file header possibilities */

#define NUM_VISIBLE_HEADERS 4

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, int *location)
{
  char *str;
  int res;
  char *comment = NULL;
  if (fdat->srate_text) 
    {
      str = gtk_entry_get_text(GTK_ENTRY(fdat->srate_text)); 
      if (str) 
	(*srate) = string2int(str);
    }
  if (fdat->chans_text) 
    {
      str = gtk_entry_get_text(GTK_ENTRY(fdat->chans_text)); 
      if (str) 
	(*chans) = string2int(str);
    }
  if (fdat->location_text) 
    {
      str = gtk_entry_get_text(GTK_ENTRY(fdat->location_text)); 
      if (str) 
	(*location) = string2int(str);
    }
  if (fdat->comment_text) 
    {
      comment = gtk_editable_get_chars(GTK_EDITABLE(fdat->comment_text), 0, -1);
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

static void load_header_and_data_lists(file_data *fdat, int type, int format, int srate, int chans, int location, char *comment)
{
  int i, chars;
  char **fl = NULL;
  char *str;
  snd_state *ss;
  ss = get_global_state();
  fdat->current_type = type;
  fdat->current_format = format;
  fl = set_header_and_data_positions(fdat, type, format); 
  gtk_signal_handler_block_by_data(GTK_OBJECT(fdat->header_list), (gpointer)ss);
  gtk_clist_select_row(GTK_CLIST(fdat->header_list), fdat->header_pos, 0);
  gtk_signal_handler_unblock_by_data(GTK_OBJECT(fdat->header_list), (gpointer)ss);
  gtk_clist_clear(GTK_CLIST(fdat->format_list));
  for (i = 0; i < fdat->formats; i++) 
    {
      str = fl[i];
      gtk_clist_insert(GTK_CLIST(fdat->format_list), i, &str);
    }
  gtk_signal_handler_block_by_data(GTK_OBJECT(fdat->format_list), (gpointer)ss);
  gtk_clist_select_row(GTK_CLIST(fdat->format_list), fdat->format_pos, 0);
  gtk_signal_handler_unblock_by_data(GTK_OBJECT(fdat->format_list), (gpointer)ss);
  if ((srate > 0) && (fdat->srate_text))
    {
      str = (char *)CALLOC(32, sizeof(char));
      sprintf(str, "%d", srate);
      gtk_entry_set_text(GTK_ENTRY(fdat->srate_text), str);
      FREE(str);
    }
  if ((chans > 0) && (fdat->chans_text))
    {
      str = (char *)CALLOC(8, sizeof(char));
      sprintf(str, "%d", chans);
      gtk_entry_set_text(GTK_ENTRY(fdat->chans_text), str);
      FREE(str);
    }
  if (fdat->comment_text) 
    {
      chars = gtk_text_get_length(GTK_TEXT(fdat->comment_text));
      if (chars > 0) gtk_editable_delete_text(GTK_EDITABLE(fdat->comment_text), 0, -1);
      gtk_text_insert(GTK_TEXT(fdat->comment_text), (ss->sgx)->help_text_fnt, (ss->sgx)->black, (ss->sgx)->white, comment, -1);
    }
  if ((location >= 0) && (fdat->location_text))
    {
      str = (char *)CALLOC(32, sizeof(char));
      sprintf(str, "%d", location);
      gtk_entry_set_text(GTK_ENTRY(fdat->location_text), str);
      FREE(str);
    }
}


/* some utilities to make gtkextra's file selector look more like gtk's */

static char *last_filename = NULL;

static char *snd_gtk_get_filename(GtkWidget *dialog)
{
#if HAVE_GTKEXTRA
  gchar *path;
  gchar *file;
  path = gtk_file_list_get_path(GTK_FILE_LIST(GTK_ICON_FILESEL(dialog)->file_list));
  file = gtk_file_list_get_filename(GTK_FILE_LIST(GTK_ICON_FILESEL(dialog)->file_list));
  if (last_filename == NULL) last_filename = (char *)CALLOC(256, sizeof(char));
  sprintf(last_filename, "%s%s", path, file);
#else
  last_filename = gtk_file_selection_get_filename(GTK_FILE_SELECTION(dialog));
#endif
  return(last_filename);
}

static GtkWidget *snd_gtk_file_selection_new(snd_state *ss, char *title, GtkSignalFunc gdelete, GtkSignalFunc ok, GtkSignalFunc cancel)
{
  GtkWidget *new_dialog;
#if HAVE_GTKEXTRA
  new_dialog = (GtkWidget *)gtk_icon_file_selection_new(title);
  add_dialog(ss, new_dialog);
  set_background(new_dialog, (ss->sgx)->basic_color);
  gtk_signal_connect(GTK_OBJECT(new_dialog), "delete_event", gdelete, (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(GTK_ICON_FILESEL(new_dialog)->ok_button), "clicked", ok, (gpointer)ss);
  gtk_signal_connect_object(GTK_OBJECT(GTK_ICON_FILESEL(new_dialog)->cancel_button), "clicked", cancel, (gpointer)ss);
  set_pushed_button_colors(GTK_ICON_FILESEL(new_dialog)->ok_button, ss);
  set_pushed_button_colors(GTK_ICON_FILESEL(new_dialog)->cancel_button, ss);
  #if HAVE_GETCWD
    if (last_filename == NULL) last_filename = (char *)CALLOC(256, sizeof(char));
    getcwd(last_filename, 256);
    gtk_icon_file_selection_open_dir(GTK_ICON_FILESEL(new_dialog), last_filename);
  #endif
#else
  new_dialog = gtk_file_selection_new(title);
  add_dialog(ss, new_dialog);
  set_background(new_dialog, (ss->sgx)->basic_color);
  gtk_signal_connect(GTK_OBJECT(new_dialog), "delete_event", gdelete, (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(new_dialog)->ok_button), "clicked", ok, (gpointer)ss);
  gtk_signal_connect_object(GTK_OBJECT(GTK_FILE_SELECTION(new_dialog)->cancel_button), "clicked", cancel, (GtkObject *)ss);
  if (last_filename) gtk_file_selection_set_filename(GTK_FILE_SELECTION(new_dialog), last_filename);
  gtk_file_selection_hide_fileop_buttons(GTK_FILE_SELECTION(new_dialog));
  set_pushed_button_colors(GTK_FILE_SELECTION(new_dialog)->ok_button, ss);
  set_pushed_button_colors(GTK_FILE_SELECTION(new_dialog)->cancel_button, ss);
#endif
  return(new_dialog);
}


/* -------- Open File Dialog -------- */

static GtkWidget *open_dialog = NULL;
void alert_new_file(void) {}

#if HAVE_GUILE

static SCM g_just_sounds(void)
{
  #define H_just_sounds "not implemented in Gtk+ version of Snd"
  return(SCM_UNSPECIFIED);
}

static SCM g_set_just_sounds(SCM on) 
{
  return(SCM_UNSPECIFIED);
}

void g_initialize_xgfile(SCM local_doc)
{
  define_procedure_with_setter(S_just_sounds, SCM_FNC g_just_sounds, H_just_sounds,
			       "set-" S_just_sounds, SCM_FNC g_set_just_sounds, local_doc, 0, 0, 0, 1);
}

#endif

static void file_open_dialog_ok(GtkWidget *w, gpointer data)
{
  snd_info *sp;
  snd_state *ss = (snd_state *)data;
  last_filename = snd_gtk_get_filename(open_dialog);
  gtk_widget_hide(open_dialog);
  if (!(is_directory(last_filename)))
    {
      sp = snd_open_file(last_filename, ss);
      if (sp) select_channel(sp, 0);           /* add_sound_window (snd-xsnd.c) will report reason for error, if any */
    }
  else
    {
      snd_error("%s is a directory", last_filename);
      last_filename = NULL;
    }
}

static void file_open_dialog_dismiss(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(open_dialog);
}

static void file_open_dialog_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(open_dialog);
}

void make_open_file_dialog(snd_state *ss)
{
  if (!open_dialog)
    open_dialog = snd_gtk_file_selection_new(ss, STR_File,
					     (GtkSignalFunc)file_open_dialog_delete,
					     (GtkSignalFunc)file_open_dialog_ok,
					     (GtkSignalFunc)file_open_dialog_dismiss);
  gtk_widget_show(open_dialog);
}


/* -------- file data choices -------- */

static void save_as_header_type_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  file_data *fd;
  fd = (file_data *)gtk_object_get_user_data(GTK_OBJECT(w));
  fd->header_pos = row;
  if (fd->current_type != row)
    {
      set_header_type_and_format_from_position(fd, row);
      load_header_and_data_lists(fd,
				 fd->current_type,
				 fd->current_format,
				 0, 0, -1, NULL);
    }
}

static void save_as_data_format_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  file_data *fd;
  fd = (file_data *)gtk_object_get_user_data(GTK_OBJECT(w));
  fd->format_pos = row;
  fd->current_format = data_format_from_position(fd->header_pos, row);
}

file_data *sndCreateFileDataForm(snd_state *ss, GtkWidget *parent, char *name, int with_chan, int header_type, int data_format, int with_loc, int comment_as_entry)
{
  GtkWidget *form, *hlab, *dlab, *slab, *clab, *comment_label, *loclab,
    *hscroll, *dscroll, *scbox, *combox, *table, *hscrollbar, *vscrollbar;
  file_data *fdat;
  int i;
  int dformats = 0;
  char **formats = NULL;
  char *str;
  fdat = (file_data *)CALLOC(1, sizeof(file_data));
  fdat->current_type = header_type;
  fdat->current_format = data_format;
  formats = set_header_positions_from_type(fdat, header_type, data_format);
  dformats = fdat->formats;

  form = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(parent), form, FALSE, FALSE, 4); /* ??? */
  gtk_widget_show(form);

  hlab = gtk_frame_new(STR_header);
  gtk_box_pack_start(GTK_BOX(form), hlab, TRUE, TRUE, 2);
  gtk_frame_set_label_align(GTK_FRAME(hlab), 0.5, 0.0);
  gtk_frame_set_shadow_type(GTK_FRAME(hlab), GTK_SHADOW_ETCHED_IN);

  fdat->header_list = gtk_clist_new(1);
  gtk_object_set_user_data(GTK_OBJECT(fdat->header_list), (gpointer)fdat);
  gtk_clist_set_selection_mode(GTK_CLIST(fdat->header_list), GTK_SELECTION_SINGLE);
  gtk_clist_set_shadow_type(GTK_CLIST(fdat->header_list), GTK_SHADOW_ETCHED_IN);
  gtk_clist_column_titles_passive(GTK_CLIST(fdat->header_list));

  for (i = 0; i < num_header_types(); i++) 
    {
      str = header_short_name(i);
      gtk_clist_append(GTK_CLIST(fdat->header_list), &str);
    }

  hscroll = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(hscroll), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(hscroll), fdat->header_list);
  gtk_container_add(GTK_CONTAINER(hlab), hscroll);

  gtk_clist_select_row(GTK_CLIST(fdat->header_list), fdat->header_pos, 0);
  gtk_signal_connect(GTK_OBJECT(fdat->header_list), "select_row", GTK_SIGNAL_FUNC(save_as_header_type_Callback), (gpointer)ss);
  gtk_widget_show(hlab);
  gtk_widget_show(fdat->header_list);
  gtk_widget_show(hscroll);

  dlab = gtk_frame_new(STR_data);
  gtk_box_pack_start(GTK_BOX(form), dlab, TRUE, TRUE, 2);
  gtk_frame_set_label_align(GTK_FRAME(dlab), 0.5, 0.0);
  gtk_frame_set_shadow_type(GTK_FRAME(dlab), GTK_SHADOW_ETCHED_IN);

  fdat->format_list = gtk_clist_new(1);
  gtk_object_set_user_data(GTK_OBJECT(fdat->format_list), (gpointer)fdat);
  gtk_clist_set_selection_mode(GTK_CLIST(fdat->format_list), GTK_SELECTION_SINGLE);
  gtk_clist_set_shadow_type(GTK_CLIST(fdat->format_list), GTK_SHADOW_ETCHED_IN);
  gtk_clist_column_titles_passive(GTK_CLIST(fdat->format_list));

  for (i = 0; i < dformats; i++) 
    {
      str = formats[i];
      gtk_clist_append(GTK_CLIST(fdat->format_list), &str);
    }

  dscroll = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(dscroll), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(dscroll), fdat->format_list);
  gtk_container_add(GTK_CONTAINER(dlab), dscroll);

  gtk_clist_select_row(GTK_CLIST(fdat->format_list), fdat->format_pos, 0);
  gtk_signal_connect(GTK_OBJECT(fdat->format_list), "select_row", GTK_SIGNAL_FUNC(save_as_data_format_Callback), (gpointer)ss);
  gtk_widget_show(dlab);
  gtk_widget_show(fdat->format_list);
  gtk_widget_show(dscroll);

  scbox = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(form), scbox, FALSE, FALSE, 4);
  gtk_widget_show(scbox);

  slab = gtk_label_new(STR_srate_p);
  gtk_box_pack_start(GTK_BOX(scbox), slab, FALSE, FALSE, 0);
  gtk_widget_show(slab);

  fdat->srate_text = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(fdat->srate_text), TRUE);
  gtk_box_pack_start(GTK_BOX(scbox), fdat->srate_text, TRUE, TRUE, 2);
  set_background(fdat->srate_text, (ss->sgx)->white);
  gtk_widget_show(fdat->srate_text);

  if (with_chan)
    {
      clab = gtk_label_new(STR_chans_p);
      gtk_box_pack_start(GTK_BOX(scbox), clab, FALSE, FALSE, 0);
      gtk_widget_show(clab);

      fdat->chans_text = gtk_entry_new();
      gtk_entry_set_editable(GTK_ENTRY(fdat->chans_text), TRUE);
      gtk_box_pack_start(GTK_BOX(scbox), fdat->chans_text, TRUE, TRUE, 2);
      set_background(fdat->chans_text, (ss->sgx)->white);
      gtk_widget_show(fdat->chans_text);
      
      if (with_loc)
	{
	  loclab = gtk_label_new(STR_location_p);
	  gtk_box_pack_start(GTK_BOX(scbox), loclab, FALSE, FALSE, 0);
	  gtk_widget_show(loclab);

	  fdat->location_text = gtk_entry_new();
	  gtk_entry_set_editable(GTK_ENTRY(fdat->location_text), TRUE);
	  gtk_box_pack_start(GTK_BOX(scbox), fdat->location_text, TRUE, TRUE, 2);
	  set_background(fdat->location_text, (ss->sgx)->white);
	  gtk_widget_show(fdat->location_text);
	}
    }

  combox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(parent), combox, TRUE, TRUE, 4);
  gtk_widget_show(combox);

  comment_label = gtk_label_new(STR_comment_p);
  gtk_box_pack_start(GTK_BOX(combox), comment_label, FALSE, FALSE, 0);
  gtk_widget_show(comment_label);

  if (comment_as_entry)
    {
      /* try to kludge around a gtk bug -- this is not needed in motif */
      fdat->comment_text = gtk_entry_new();
      gtk_entry_set_editable(GTK_ENTRY(fdat->comment_text), TRUE);
      gtk_box_pack_start(GTK_BOX(combox), fdat->comment_text, TRUE, TRUE, 2);
      gtk_widget_show(fdat->comment_text);
      set_background(fdat->comment_text, (ss->sgx)->white);
    }
  else
    {
      table = gtk_table_new (2, 2, FALSE);
      gtk_box_pack_start(GTK_BOX(combox), table, TRUE, TRUE, 4);
      fdat->comment_text = gtk_text_new(NULL, NULL);
      gtk_table_attach(GTK_TABLE(table), fdat->comment_text, 0, 1, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      gtk_text_set_editable(GTK_TEXT(fdat->comment_text), TRUE);
      gtk_text_set_word_wrap(GTK_TEXT(fdat->comment_text), FALSE);
      gtk_widget_show (fdat->comment_text);
      hscrollbar = gtk_hscrollbar_new (GTK_TEXT (fdat->comment_text)->hadj);
      set_background(hscrollbar, (ss->sgx)->position_color);
      gtk_table_attach (GTK_TABLE (table), hscrollbar, 0, 1, 1, 2, 
			(GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
			(GtkAttachOptions)(GTK_FILL), 
			0, 0);
      gtk_widget_show (hscrollbar);
      vscrollbar = gtk_vscrollbar_new (GTK_TEXT (fdat->comment_text)->vadj);
      set_background(vscrollbar, (ss->sgx)->position_color);
      gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1, 
			(GtkAttachOptions)(GTK_FILL), 
			(GtkAttachOptions)(GTK_EXPAND | GTK_FILL | GTK_SHRINK), 
			0, 0);
      gtk_widget_show (vscrollbar);
      gtk_widget_show(table);
    }

  return(fdat);
}


/* -------- save as dialog (file and edit menus) -------- */

static file_data *save_as_file_data = NULL;
static GtkWidget *save_as_dialog = NULL;
static int save_as_dialog_type = FILE_SAVE_AS;
static char *last_save_as_filename = NULL;

static void save_as_ok_callback(GtkWidget *w, gpointer data)
{
  char *str = NULL, *comment = NULL;
  int result;
  int type, format, srate;
  snd_info *sp;
  snd_state *ss = (snd_state *)data;
  str = gtk_entry_get_text(GTK_ENTRY(save_as_file_data->srate_text));
  srate = string2int(str);
  comment = gtk_editable_get_chars(GTK_EDITABLE(save_as_file_data->comment_text), 0, -1);
  type = save_as_file_data->current_type;
  format = save_as_file_data->current_format;
  last_save_as_filename = gtk_file_selection_get_filename(GTK_FILE_SELECTION(save_as_dialog));
  sp = any_selected_sound(ss);
  if (last_save_as_filename)
    result = check_for_filename_collisions_and_save(ss, sp, last_save_as_filename, save_as_dialog_type, srate, type, format, comment);
  else if (sp) report_in_minibuffer(sp, "not saved (no name given)");
  gtk_widget_hide(save_as_dialog);
} 

static void save_as_cancel_callback(GtkWidget *w, gpointer data)
{ 
  gtk_widget_hide(save_as_dialog);
} 

static void save_as_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(save_as_dialog);
}

static void make_save_as_dialog(snd_state *ss, char *sound_name, int save_type, int header_type, int format_type)
{
  /* save old as new, close old, open new */
  GtkWidget *fbox;
  if (!save_as_dialog)
    {

      /* can't use the gtkextra file selector here because they've hidden "main_vbox" so there's
       *   no way to add our own widgets to the dialog
       */
      save_as_dialog = gtk_file_selection_new(STR_save_as_p);
      add_dialog(ss, save_as_dialog);
      set_background(save_as_dialog, (ss->sgx)->basic_color);
      gtk_signal_connect(GTK_OBJECT(save_as_dialog), "delete_event", (GtkSignalFunc)save_as_delete_callback, NULL);
      gtk_signal_connect(GTK_OBJECT(GTK_FILE_SELECTION(save_as_dialog)->ok_button), "clicked", (GtkSignalFunc)save_as_ok_callback, (GtkObject *)ss);
      gtk_signal_connect_object(GTK_OBJECT(GTK_FILE_SELECTION(save_as_dialog)->cancel_button), "clicked", (GtkSignalFunc)save_as_cancel_callback, (GtkObject *)ss);
#if (!HAVE_GTKEXTRA)
      if (last_filename) gtk_file_selection_set_filename(GTK_FILE_SELECTION(open_dialog), last_filename);
#endif
      gtk_file_selection_hide_fileop_buttons(GTK_FILE_SELECTION(save_as_dialog));
      set_pushed_button_colors(GTK_FILE_SELECTION(save_as_dialog)->ok_button, ss);
      set_pushed_button_colors(GTK_FILE_SELECTION(save_as_dialog)->cancel_button, ss);

      fbox = gtk_vbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(save_as_dialog)->main_vbox), fbox, TRUE, TRUE, 0);
      gtk_widget_show(fbox);

      save_as_file_data = sndCreateFileDataForm(ss, fbox, "data-form", FALSE, header_type, format_type, FALSE, FALSE);
      gtk_widget_set_usize(save_as_file_data->comment_text, 100, 20);
    }
}

void make_file_save_as_dialog(snd_state *ss)
{
  snd_info *sp = NULL;
  file_info *hdr = NULL;
  save_as_dialog_type = FILE_SAVE_AS;
  sp = any_selected_sound(ss);
  if (sp) hdr = sp->hdr;
  make_save_as_dialog(ss,
		      (char *)((sp) ? sp->shortname : ""),
		      FILE_SAVE_AS,
		      default_output_type(ss),
		      default_output_format(ss));
  load_header_and_data_lists(save_as_file_data,
			     save_as_file_data->current_type,
			     save_as_file_data->current_format,
			     (hdr) ? hdr->srate : region_srate(0), 0, -1, output_comment(hdr));
  gtk_widget_show(save_as_dialog);
}


/* -------- edit save as dialog -------- */

void make_edit_save_as_dialog(snd_state *ss)
{
  save_as_dialog_type = EDIT_SAVE_AS;
  make_save_as_dialog(ss, STR_current_selection, EDIT_SAVE_AS, -1, -1);
  load_header_and_data_lists(save_as_file_data,
			     save_as_file_data->current_type,
			     save_as_file_data->current_format,
			     region_srate(0), 0, -1, NULL);
  gtk_widget_show(save_as_dialog);
}


/* -------- files browser and regions list widgetry -------- */
/*
 * the region and file browsers share much widgetry -- they are supposed to look the same
 */

ww_info *make_title_row(snd_state *ss, GtkWidget *formw, char *first_str, char *second_str, char *main_str, int pad, int with_sort, int with_pane)
{
  GtkWidget *rlw, *sep1, *cww, *phbox;
  GtkWidget *smenu, *sbar, *sitem;
  ww_info *wwi;

  wwi = (ww_info *)CALLOC(1, sizeof(ww_info));
  
  /* assuming "formw" is a vbox */

  rlw = gtk_label_new(main_str);
  gtk_box_pack_start(GTK_BOX(formw), rlw, FALSE, FALSE, 0);
  set_background(rlw, (ss->sgx)->highlight_color);
  gtk_widget_show(rlw);

  sep1 = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(formw), sep1, FALSE, FALSE, 2);
  gtk_widget_show(sep1);
  
  if (with_pane == WITH_PANED_WINDOW)
    {
      wwi->panes = gtk_vpaned_new();
      gtk_box_pack_start(GTK_BOX(formw), wwi->panes, TRUE, TRUE, 0);
      gtk_widget_show(wwi->panes);

      wwi->toppane = gtk_hbox_new(FALSE, 0);
      gtk_paned_add1(GTK_PANED(wwi->panes), wwi->toppane);
      gtk_widget_show(wwi->toppane);

      phbox = gtk_vbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(wwi->toppane), phbox, TRUE, TRUE, 4);
      gtk_widget_show(phbox);

      formw = phbox;
    }
  else 
    {
      wwi->panes = formw;
      wwi->toppane = formw;
    }

  wwi->tophbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(formw), wwi->tophbox, FALSE, FALSE, 4);
  gtk_widget_show(wwi->tophbox);

  wwi->svw = gtk_label_new(first_str); 
  gtk_box_pack_start(GTK_BOX(wwi->tophbox), wwi->svw, FALSE, FALSE, (pad == PAD_TITLE_ON_LEFT) ? 5 : 2);
  gtk_widget_show(wwi->svw);

  wwi->plw = gtk_label_new(second_str);
  gtk_box_pack_start(GTK_BOX(wwi->tophbox), wwi->plw, FALSE, FALSE, 2);
  gtk_widget_show(wwi->plw);

  if (with_sort == WITH_SORT_BUTTON)
    {
      sbar = gtk_menu_bar_new();
      gtk_box_pack_end(GTK_BOX(wwi->tophbox), sbar, FALSE, FALSE, 0);
      set_background(sbar, (ss->sgx)->basic_color);
      gtk_widget_show(sbar);

      smenu = gtk_menu_new();
      wwi->byname = gtk_menu_item_new_with_label(STR_name);
      set_background(wwi->byname, (ss->sgx)->basic_color);
      wwi->bydate = gtk_menu_item_new_with_label(STR_date);
      set_background(wwi->bydate, (ss->sgx)->basic_color);
      wwi->bysize = gtk_menu_item_new_with_label(STR_size);
      set_background(wwi->bysize, (ss->sgx)->basic_color);
      wwi->byentry = gtk_menu_item_new_with_label(STR_entry);
      set_background(wwi->byentry, (ss->sgx)->basic_color);

      gtk_menu_append(GTK_MENU(smenu), wwi->byname);
      gtk_menu_append(GTK_MENU(smenu), wwi->bydate);
      gtk_menu_append(GTK_MENU(smenu), wwi->bysize);
      gtk_menu_append(GTK_MENU(smenu), wwi->byentry);

      gtk_widget_show(wwi->byname);
      gtk_widget_show(wwi->bydate);
      gtk_widget_show(wwi->bysize);
      gtk_widget_show(wwi->byentry);
      
      sitem = gtk_menu_item_new_with_label(STR_sort);
      set_background(sitem, (ss->sgx)->basic_color);
      gtk_widget_show(sitem);
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(sitem), smenu);
      gtk_menu_bar_append(GTK_MENU_BAR(sbar), sitem);
    }

  wwi->list = gtk_vbox_new(FALSE, 0);
  set_background(wwi->list, (ss->sgx)->basic_color);

  cww = gtk_scrolled_window_new(NULL, NULL);
  set_background(cww, (ss->sgx)->basic_color);
  gtk_box_pack_start(GTK_BOX(formw), cww, TRUE, TRUE, 0);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cww), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(cww), wwi->list);

  gtk_widget_show(wwi->list);
  gtk_widget_show(cww);

  return(wwi);
}

regrow *make_regrow(snd_state *ss, GtkWidget *ww,
		    GtkSignalFunc first_callback, GtkSignalFunc second_callback, GtkSignalFunc third_callback)
{
  regrow *r;
  r = (regrow *)CALLOC(1, sizeof(regrow));

  /* assume "ww" is a vbox widget in this case */

  r->rw = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(ww), r->rw, FALSE, FALSE, 0);
  set_background(r->rw, (ss->sgx)->zoom_color);
  gtk_widget_show(r->rw);

  r->sv = gtk_check_button_new();
  set_backgrounds(r->rw, (ss->sgx)->highlight_color);
  gtk_box_pack_start(GTK_BOX(r->rw), r->sv, FALSE, FALSE, 2);
  gtk_signal_connect(GTK_OBJECT(r->sv), "toggled", GTK_SIGNAL_FUNC(first_callback), (gpointer)r);
  gtk_widget_show(r->sv);

  r->pl = gtk_check_button_new();
  set_backgrounds(r->pl, (ss->sgx)->highlight_color);
  gtk_box_pack_start(GTK_BOX(r->rw), r->pl, FALSE, FALSE, 2);
  gtk_signal_connect(GTK_OBJECT(r->pl), "toggled", GTK_SIGNAL_FUNC(second_callback), (gpointer)r);
  gtk_widget_show(r->pl);

  r->nm = gtk_button_new_with_label("");
  set_backgrounds(r->nm, (ss->sgx)->highlight_color);
  gtk_box_pack_start(GTK_BOX(r->rw), r->nm, TRUE, TRUE, 2);
  gtk_signal_connect(GTK_OBJECT(r->nm), "clicked", GTK_SIGNAL_FUNC(third_callback), (gpointer)r);
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
  char *str;
  r = cur_name_row[pos];
  str = view_curfiles_name(r->pos);
  set_button_label_bold(r->nm, str);
  FREE(str);
}

static void View_Files_Help_Callback(GtkWidget *w, gpointer context) 
{
  view_files_dialog_help((snd_state *)context);
}

static void View_Files_Dismiss_Callback(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(view_files_dialog);
}

static void View_Files_Delete_Callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(view_files_dialog);
}

static void View_Files_Clear_Callback(GtkWidget *w, gpointer context) 
{
  /* clear previous files list and associated widget list */
  clear_prevlist((snd_state *)context);
}

static void View_Files_Update_Callback(GtkWidget *w, gpointer context) 
{
  /* run through previous files list looking for any that have been deleted behind our back */
  update_prevlist((snd_state *)context);
}

static void View_CurFiles_Save_Callback(GtkWidget *w, gpointer context) 
{
  regrow *r = (regrow *)context;
  view_curfiles_save(r->ss, r->pos);
  set_toggle_button(r->sv, FALSE, FALSE, (void *)r);
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
	  set_toggle_button(r->pl, state, FALSE, (void *)r);
	}
    }
}

static void View_CurFiles_Play_Callback(GtkWidget *w, gpointer context) 
{
  regrow *r = (regrow *)context;
  view_curfiles_play(r->ss, r->pos, GTK_TOGGLE_BUTTON(w)->active);
}

static void curfile_unhighlight(snd_state *ss)
{
  regrow *r;
  if (file_dialog_is_active())
    {
      if (vf_selected_file != -1)
	{
	  r = cur_name_row[vf_selected_file];
	  set_backgrounds(r->rw, (ss->sgx)->highlight_color);
	  set_backgrounds(r->nm, (ss->sgx)->highlight_color);
	  vf_selected_file = -1;
	}
    }
}

void curfile_highlight(snd_state *ss, int i)
{
  regrow *r;
  if (file_dialog_is_active())
    {
      if (vf_selected_file != -1) curfile_unhighlight(ss);
      r = cur_name_row[i];
      set_backgrounds(r->rw, (ss->sgx)->zoom_color);
      set_backgrounds(r->nm, (ss->sgx)->zoom_color);
      vf_selected_file = i;
    }
}

static void View_CurFiles_Select_Callback(GtkWidget *w, gpointer context) 
{
  regrow *r = (regrow *)context;
  view_curfiles_select(r->ss, r->pos);
}

static void View_PrevFiles_Unlist_Callback(GtkWidget *w, gpointer context) 
{
  regrow *r = (regrow *)context;
  file_unprevlist(get_prevnames(r->pos));
  make_prevfiles_list(r->ss);
}

static void View_PrevFiles_Play_Callback(GtkWidget *w, gpointer context) 
{
  /* open and play -- close at end or when button off toggled */
  regrow *r = (regrow *)context;
  if (view_prevfiles_play(r->ss, r->pos, GTK_TOGGLE_BUTTON(w)->active))
    set_toggle_button(w, FALSE, FALSE, (void *)r);
}

static void View_PrevFiles_Select_Callback(GtkWidget *w, gpointer context) 
{
  /* open and set as selected */
  regrow *r = (regrow *)context;
  view_prevfiles_select(r->ss, r->pos);
}

void highlight_selected_sound(snd_state *ss)
{
  snd_info *sp;
  int i;
  sp = selected_sound(ss);
  if (sp)
    {
      i = find_curfile_regrow(sp->shortname);
      if (i != -1) curfile_highlight(ss, i); else curfile_unhighlight(ss);
    }
  else curfile_unhighlight(ss);
}

void make_curfiles_list (snd_state *ss)
{
  int i;
  char *str;
  regrow *r;
  for (i = 0; i < get_curfile_end(); i++)
    {
      r = cur_name_row[i];
      if (r == NULL)
	{
	  r = make_regrow(ss, vf_curww, (void (*)())View_CurFiles_Save_Callback, (void (*)())View_CurFiles_Play_Callback, (void (*)())View_CurFiles_Select_Callback);
	  cur_name_row[i] = r;
	  r->pos = i;
	  r->ss = ss;
	}
      str = view_curfiles_name(r->pos);
      set_button_label_bold(r->nm, str);
      FREE(str);
      set_toggle_button(r->sv, FALSE, FALSE, (void *)r);
      set_toggle_button(r->pl, FALSE, FALSE, (void *)r);
      gtk_widget_show(r->rw);
    }
  for (i = get_curfile_end(); i < get_max_curfile_end(); i++)
    {
      if ((r = cur_name_row[i]))
	{
	  if (GTK_WIDGET_VISIBLE(r->rw)) gtk_widget_hide(r->rw);
	}
    }
  set_max_curfile_end(get_curfile_end());
  highlight_selected_sound(ss);
  gtk_widget_show(vf_curlst);
}

static void sort_prevfiles_by_name(GtkWidget *w, gpointer context) 
{
  snd_state *ss;
  ss = get_global_state();
  set_previous_files_sort(ss, 1);
  make_prevfiles_list(ss);
}

static void sort_prevfiles_by_date(GtkWidget *w, gpointer context) 
{
  snd_state *ss;
  ss = get_global_state();
  set_previous_files_sort(ss, 2);
  make_prevfiles_list(ss);
}

static void sort_prevfiles_by_size(GtkWidget *w, gpointer context) 
{
  snd_state *ss;
  ss = get_global_state();
  set_previous_files_sort(ss, 3);
  make_prevfiles_list(ss);
}

static void sort_prevfiles_by_entry(GtkWidget *w, gpointer context) 
{
  snd_state *ss;
  ss = get_global_state();
  set_previous_files_sort(ss, 4);
  make_prevfiles_list(ss);
}

void make_prevfiles_list (snd_state *ss)
{
  int i;
  regrow *r;
  if (get_prevfile_end() >= 0)
    {
      make_prevfiles_list_1(ss);
      for (i = 0; i <= get_prevfile_end(); i++)
	{
	  if (!((r = prev_name_row[i])))
	    {
	      r = make_regrow(ss, vf_prevww, (void (*)())View_PrevFiles_Unlist_Callback, (void (*)())View_PrevFiles_Play_Callback, (void (*)())View_PrevFiles_Select_Callback);
	      prev_name_row[i] = r;
	      r->pos = i;
	      r->ss = ss;
	    }
	  set_button_label_bold(r->nm, get_prevnames(r->pos));
	  set_toggle_button(r->sv, FALSE, FALSE, (void *)r);
	  set_toggle_button(r->pl, FALSE, FALSE, (void *)r);
	  gtk_widget_show(r->rw);
	}
    }
  for (i = get_prevfile_end()+1; i <= get_max_prevfile_end(); i++)
    {
      if ((r = prev_name_row[i]))
	{
	  if (GTK_WIDGET_VISIBLE(r->rw)) gtk_widget_hide(r->rw);
	}
    }
  set_max_prevfile_end(get_prevfile_end());
  if (!(GTK_WIDGET_VISIBLE(vf_prevlst))) gtk_widget_show(vf_prevlst);
}

/* play open unlist for prevfile, play save select for curfile, preload process for prevfile (snd-clm) */

static GtkWidget *fs1, *fs3;
void View_Files_Callback(GtkWidget *w, gpointer context)
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
  snd_state *ss = (snd_state *)context;
  ww_info *wwl;
  GtkWidget *mainform, *curform, *prevform, *updateB, *helpB, *dismissB, *clearB, *sep;
  if (!view_files_dialog)
    {
      vf_selected_file = -1;

      view_files_dialog = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(view_files_dialog), "delete_event", GTK_SIGNAL_FUNC(View_Files_Delete_Callback), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(view_files_dialog), STR_Files);
      gtk_window_set_policy(GTK_WINDOW(view_files_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
      set_backgrounds(view_files_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(view_files_dialog), 10);
      gtk_widget_set_usize(GTK_WIDGET(view_files_dialog), 400, 200);
      gtk_widget_realize(view_files_dialog);
      add_dialog(ss, view_files_dialog);

      helpB = gtk_button_new_with_label(STR_Help);
      dismissB = gtk_button_new_with_label(STR_Dismiss);
      updateB = gtk_button_new_with_label(STR_Update);
      clearB = gtk_button_new_with_label(STR_Clear);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), dismissB, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), updateB, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), clearB, TRUE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(view_files_dialog)->action_area), helpB, TRUE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(dismissB), "clicked", GTK_SIGNAL_FUNC(View_Files_Dismiss_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(helpB), "clicked", GTK_SIGNAL_FUNC(View_Files_Help_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(updateB), "clicked", GTK_SIGNAL_FUNC(View_Files_Update_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(clearB), "clicked", GTK_SIGNAL_FUNC(View_Files_Clear_Callback), (gpointer)ss);
      set_pushed_button_colors(helpB, ss);
      set_pushed_button_colors(dismissB, ss);
      set_pushed_button_colors(updateB, ss);
      set_pushed_button_colors(clearB, ss);
      gtk_widget_show(dismissB);
      gtk_widget_show(updateB);
      gtk_widget_show(helpB);
      gtk_widget_show(clearB);

      mainform = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(view_files_dialog)->vbox), mainform, TRUE, TRUE, 0);
      gtk_widget_show(mainform);
      
      curform = gtk_vbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(mainform), curform, TRUE, TRUE, 0);
      gtk_widget_show(curform);

      sep = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(mainform), sep, FALSE, FALSE, 0);
      gtk_widget_show(sep);

      prevform = gtk_vbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(mainform), prevform, TRUE, TRUE, 0);
      gtk_widget_show(prevform);

      /* current files section: save play current files | files */
      wwl = make_title_row(ss, curform, STR_save, STR_play, STR_current_files, PAD_TITLE_ON_RIGHT, WITHOUT_SORT_BUTTON, WITHOUT_PANED_WINDOW);
      fs1 = wwl->tophbox;
      vf_curww = wwl->list; /* different from Motif */
      vf_curlst = wwl->list;
      FREE(wwl); 
      wwl = NULL;

      /* previous files section: unlist play previous files | files */
      wwl = make_title_row(ss, prevform, STR_unlist, STR_play, STR_previous_files, PAD_TITLE_ON_LEFT, WITH_SORT_BUTTON, WITHOUT_PANED_WINDOW);
      fs3 = wwl->tophbox;

      gtk_signal_connect_object(GTK_OBJECT(wwl->byname), "activate", GTK_SIGNAL_FUNC(sort_prevfiles_by_name), NULL);
      gtk_signal_connect_object(GTK_OBJECT(wwl->bydate), "activate", GTK_SIGNAL_FUNC(sort_prevfiles_by_date), NULL);
      gtk_signal_connect_object(GTK_OBJECT(wwl->bysize), "activate", GTK_SIGNAL_FUNC(sort_prevfiles_by_size), NULL);
      gtk_signal_connect_object(GTK_OBJECT(wwl->byentry), "activate", GTK_SIGNAL_FUNC(sort_prevfiles_by_entry), NULL);

      vf_prevww = wwl->list;
      vf_prevlst = wwl->list;
      FREE(wwl); 
      wwl = NULL;
    }
  gtk_widget_show(view_files_dialog);
  make_curfiles_list(ss);
  make_prevfiles_list(ss);
  highlight_selected_sound(ss);

  set_widget_height(fs1, widget_height(fs3));
}

void start_file_dialog(snd_state *ss, int width, int height)
{
  View_Files_Callback(NULL, (gpointer)ss);
  if (width > 0) set_widget_size(view_files_dialog, width, height);
}

int file_dialog_is_active(void)
{
  return((view_files_dialog) && (GTK_WIDGET_VISIBLE(view_files_dialog)));
}


/* -------------------------------- Raw Data Dialog -------------------------------- */

static GtkWidget *raw_data_dialog = NULL;
static GtkWidget *raw_srate_text, *raw_chans_text, *raw_location_text, *raw_data_label;
static int raw_data_location = 0;
static int raw_cancelled = 0, raw_done = 0;

static void raw_data_ok_Callback(GtkWidget *w, gpointer context) {raw_cancelled = 0; raw_done = 1;}
static void raw_data_cancel_Callback(GtkWidget *w, gpointer context) {raw_cancelled = 1; raw_done = 1;}
static void raw_data_delete_Callback(GtkWidget *w, GdkEvent *event, gpointer context) {raw_cancelled = 1; raw_done = 1;}

static void raw_data_default_Callback(GtkWidget *w, gpointer context) 
{
  snd_state *ss = (snd_state *)context;
  set_use_raw_defaults(ss, 1);
  raw_cancelled = 0; raw_done = 1;
}

static void raw_data_browse_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  set_raw_format(ss, row+1);
}

static void raw_data_help_Callback(GtkWidget *w, gpointer context) 
{
  raw_data_dialog_help((snd_state *)context);
}

static void make_raw_data_dialog(snd_state *ss)
{
  char dfs_str[16];
  int i;
  char *str;
  GtkWidget *lst, *dls, *dlab, *dloclab, *chnlab;
  GtkWidget *defaultB, *helpB, *cancelB, *okB, *sratehbox, *lochbox, *scroller;

  raw_data_dialog = gtk_dialog_new();
  gtk_signal_connect(GTK_OBJECT(raw_data_dialog), "delete_event", GTK_SIGNAL_FUNC(raw_data_delete_Callback), (gpointer)ss);
  gtk_window_set_title(GTK_WINDOW(raw_data_dialog), STR_No_Header_on_File);
  gtk_window_set_policy(GTK_WINDOW(raw_data_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
  set_background(raw_data_dialog, (ss->sgx)->basic_color);
  gtk_container_set_border_width(GTK_CONTAINER(raw_data_dialog), 10);
  gtk_widget_set_usize(GTK_WIDGET(raw_data_dialog), 350, 260);
  gtk_widget_realize(raw_data_dialog);
  add_dialog(ss, raw_data_dialog);

  helpB = gtk_button_new_with_label(STR_Help);
  cancelB = gtk_button_new_with_label(STR_Cancel);
  defaultB = gtk_button_new_with_label(STR_Default);
  okB = gtk_button_new_with_label(STR_Ok);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), okB, TRUE, TRUE, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), defaultB, TRUE, TRUE, 10);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), cancelB, TRUE, TRUE, 10);
  gtk_box_pack_end(GTK_BOX(GTK_DIALOG(raw_data_dialog)->action_area), helpB, TRUE, TRUE, 10);
  gtk_signal_connect(GTK_OBJECT(okB), "clicked", GTK_SIGNAL_FUNC(raw_data_ok_Callback), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(helpB), "clicked", GTK_SIGNAL_FUNC(raw_data_help_Callback), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(defaultB), "clicked", GTK_SIGNAL_FUNC(raw_data_default_Callback), (gpointer)ss);
  gtk_signal_connect(GTK_OBJECT(cancelB), "clicked", GTK_SIGNAL_FUNC(raw_data_cancel_Callback), (gpointer)ss);
  set_pushed_button_colors(helpB, ss);
  set_pushed_button_colors(okB, ss);
  set_pushed_button_colors(defaultB, ss);
  set_pushed_button_colors(cancelB, ss);
  gtk_widget_show(okB);
  gtk_widget_show(cancelB);
  gtk_widget_show(helpB);
  gtk_widget_show(defaultB);

  raw_data_label = gtk_label_new("");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->vbox), raw_data_label, FALSE, FALSE, 6);
  gtk_widget_show(raw_data_label);

  sratehbox = gtk_hbox_new(FALSE, 2);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->vbox), sratehbox, FALSE, FALSE, 6);
  gtk_widget_show(sratehbox);

  dls = gtk_label_new(STR_srate_p);
  gtk_box_pack_start(GTK_BOX(sratehbox), dls, FALSE, FALSE, 4);
  gtk_widget_show(dls);

  raw_srate_text = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(raw_srate_text), TRUE);
  gtk_box_pack_start(GTK_BOX(sratehbox), raw_srate_text, TRUE, TRUE, 2);
  set_background(raw_srate_text, (ss->sgx)->white);
  sprintf(dfs_str, "%d", raw_srate(ss));
  gtk_entry_set_text(GTK_ENTRY(raw_srate_text), dfs_str);
  gtk_widget_show(raw_srate_text);

  chnlab = gtk_label_new(STR_chans_p);
  gtk_box_pack_start(GTK_BOX(sratehbox), chnlab, FALSE, FALSE, 4);
  gtk_widget_show(chnlab);

  raw_chans_text = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(raw_chans_text), TRUE);
  gtk_box_pack_start(GTK_BOX(sratehbox), raw_chans_text, TRUE, TRUE, 2);
  set_background(raw_chans_text, (ss->sgx)->white);
  sprintf(dfs_str, "%d", raw_chans(ss));
  gtk_entry_set_text(GTK_ENTRY(raw_chans_text), dfs_str);
  gtk_widget_show(raw_chans_text);

  lochbox = gtk_hbox_new(FALSE, 2);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->vbox), lochbox, FALSE, FALSE, 6);
  gtk_widget_show(lochbox);

  dloclab = gtk_label_new(STR_data_location_p);
  gtk_box_pack_start(GTK_BOX(lochbox), dloclab, FALSE, FALSE, 4);
  gtk_widget_show(dloclab);

  raw_location_text = gtk_entry_new();
  gtk_entry_set_editable(GTK_ENTRY(raw_location_text), TRUE);
  gtk_box_pack_start(GTK_BOX(lochbox), raw_location_text, TRUE, TRUE, 2);
  set_background(raw_location_text, (ss->sgx)->white);
  gtk_entry_set_text(GTK_ENTRY(raw_location_text), "0");
  gtk_widget_show(raw_location_text);

  dlab = gtk_label_new(STR_data_format_p);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->vbox), dlab, FALSE, FALSE, 6);
  gtk_widget_show(dlab);

  lst = gtk_clist_new(1);
  gtk_clist_set_selection_mode(GTK_CLIST(lst), GTK_SELECTION_SINGLE);
  gtk_clist_set_shadow_type(GTK_CLIST(lst), GTK_SHADOW_ETCHED_IN);
  gtk_clist_column_titles_passive(GTK_CLIST(lst));
  for (i = 1; i < num_data_formats(); i++) 
    {
      str = data_format_name(i);
      gtk_clist_append(GTK_CLIST(lst), &str);
    }
  gtk_signal_connect(GTK_OBJECT(lst), "select_row", GTK_SIGNAL_FUNC(raw_data_browse_Callback), (gpointer)ss);

  scroller = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scroller), lst);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(raw_data_dialog)->vbox), scroller, TRUE, TRUE, 0);

  gtk_widget_show(lst);
  gtk_widget_show(scroller);
}

static file_info *read_raw_dialog(char *filename, snd_state *ss)
{
  char *str;
  file_info *hdr = NULL;
  reflect_raw_pending_in_menu();
  raw_done = 0;
  gtk_widget_show(raw_data_dialog);
  while (!raw_done) gtk_main_iteration();
  gtk_widget_hide(raw_data_dialog);
  reflect_raw_open_in_menu();
  if (raw_cancelled) return(NULL);
  str = gtk_entry_get_text(GTK_ENTRY(raw_srate_text));
  if ((str) && (*str)) set_raw_srate(ss, string2int(str));
  str = gtk_entry_get_text(GTK_ENTRY(raw_chans_text));
  if ((str) && (*str)) set_raw_chans(ss, string2int(str));
  str = gtk_entry_get_text(GTK_ENTRY(raw_location_text));
  if ((str) && (*str)) raw_data_location = string2int(str);
  mus_header_set_raw_defaults(raw_srate(ss), raw_chans(ss), raw_format(ss));
  mus_sound_override_header(filename, raw_srate(ss), raw_chans(ss), raw_format(ss), MUS_RAW, raw_data_location, 
			mus_bytes_to_samples(raw_format(ss), mus_sound_length(filename) - raw_data_location));
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

file_info *get_raw_file_info(char *filename, snd_state *ss)
{
  /* put up dialog for srate, chans, data format */
  char *str;
  if (use_raw_defaults(ss))
    {
      /* choices already made, so just send back a header that reflects those choices */
      if (mus_file_probe(filename))
	return(make_file_info_1(filename, ss));
      else
	{
	  snd_error("can't find raw (headerless) file %s: %s", filename, strerror(errno));
	  return(NULL);
	}
    }
  if (!raw_data_dialog) make_raw_data_dialog(ss);
  str = (char *)CALLOC(256, sizeof(char));
  sprintf(str, "No header found for %s", filename_without_home_directory(filename));
  gtk_label_set_text(GTK_LABEL(raw_data_label), str);
  FREE(str);
  return(read_raw_dialog(filename, ss));
}

file_info *get_reasonable_file_info(char *filename, snd_state *ss, file_info *hdr)
{
  char *tmp;
  tmp = raw_data_explanation(filename, ss, hdr);
  if (!raw_data_dialog) make_raw_data_dialog(ss);
  gtk_label_set_text(GTK_LABEL(raw_data_label), tmp);
  FREE(tmp);
  return(read_raw_dialog(filename, ss));
}



/* -------------------------------- New File -------------------------------- */

/* no longer shares with raw data -- 11-Nov-99 */

static int new_file_cancelled = 0, new_file_done = 0;
static GtkWidget *new_dialog = NULL, *new_file_name;
static file_data *new_dialog_data = NULL;

static void NewFileOkCallback(GtkWidget *w, gpointer context) {new_file_cancelled = 0; new_file_done = 1;}
static void NewFileCancelCallback(GtkWidget *w, gpointer context) {new_file_cancelled = 1; new_file_done = 1;}
static void NewFileDeleteCallback(GtkWidget *w, GdkEvent *event, gpointer context) {new_file_cancelled = 1; new_file_done = 1;}

static void new_file_help_callback(GtkWidget *w, gpointer context) 
{
  new_file_dialog_help((snd_state *)context);
}

snd_info *make_new_file_dialog(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *comment)
{
  int loc;
  char *tmpstr, *title, *newer_name = NULL;
  snd_info *sp = NULL;
  GtkWidget *name_label, *hform;
  GtkWidget *help_button, *cancel_button, *ok_button;
  new_file_cancelled = 0;
  title = (char *)CALLOC(snd_strlen(newname) + 32, sizeof(char));
  sprintf(title, "create new sound: %s", newname);
  if (!new_dialog)
    {
      new_dialog = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(new_dialog), "delete_event", GTK_SIGNAL_FUNC(NewFileDeleteCallback), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(new_dialog), title);
      gtk_window_set_policy(GTK_WINDOW(new_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
      set_background(new_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(new_dialog), 10);
      gtk_widget_set_usize(GTK_WIDGET(new_dialog), 400, 250);
      gtk_widget_realize(new_dialog);
      add_dialog(ss, new_dialog);

      help_button = gtk_button_new_with_label(STR_Help);
      cancel_button = gtk_button_new_with_label(STR_Cancel);
      ok_button = gtk_button_new_with_label(STR_Ok);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_dialog)->action_area), ok_button, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_dialog)->action_area), cancel_button, TRUE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(new_dialog)->action_area), help_button, TRUE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(cancel_button), "clicked", GTK_SIGNAL_FUNC(NewFileCancelCallback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(new_file_help_callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(ok_button), "clicked", GTK_SIGNAL_FUNC(NewFileOkCallback), (gpointer)ss);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(cancel_button, ss);
      set_pushed_button_colors(ok_button, ss);
      gtk_widget_show(cancel_button);
      gtk_widget_show(ok_button);
      gtk_widget_show(help_button);

      hform = gtk_hbox_new(FALSE, 0);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(new_dialog)->vbox), hform, FALSE, FALSE, 4);
      gtk_widget_show(hform);

      name_label = gtk_label_new("New file:");
      gtk_box_pack_start(GTK_BOX(hform), name_label, FALSE, FALSE, 2);
      gtk_widget_show(name_label);

      new_file_name = gtk_entry_new();
      gtk_entry_set_editable(GTK_ENTRY(new_file_name), TRUE);
      gtk_box_pack_start(GTK_BOX(hform), new_file_name, TRUE, TRUE, 2);
      set_background(new_file_name, (ss->sgx)->white);
      gtk_entry_set_text(GTK_ENTRY(new_file_name), newname);
      gtk_widget_show(new_file_name);

      new_dialog_data = sndCreateFileDataForm(ss, GTK_DIALOG(new_dialog)->vbox, "data-form", TRUE, default_output_type(ss), default_output_format(ss), FALSE, FALSE);
    }
  else
    {
      gtk_window_set_title(GTK_WINDOW(new_dialog), title);
      gtk_entry_set_text(GTK_ENTRY(new_file_name), newname);
    }
  load_header_and_data_lists(new_dialog_data, header_type, data_format, srate, chans, -1, comment);
  new_file_done = 0;
  gtk_widget_show(new_dialog);
  while (!new_file_done) gtk_main_iteration();
  gtk_widget_hide(new_dialog);
  if (new_file_cancelled)
    return(NULL);
  else
    {
      tmpstr = read_file_data_choices(new_dialog_data, &srate, &chans, &header_type, &data_format, &loc);
      newer_name = gtk_entry_get_text(GTK_ENTRY(new_file_name));
      if (newer_name == NULL) return(NULL);
      sp = finish_new_file(ss, newer_name, header_type, data_format, srate, chans, tmpstr);
      if (tmpstr) FREE(tmpstr);
    }
  return(sp);
}



/* -------- mix file dialog -------- */

static GtkWidget *file_mix_dialog = NULL;

static void file_mix_cancel_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(file_mix_dialog);
}

static void file_mix_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(file_mix_dialog);
}

static void file_mix_ok_callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  gtk_widget_hide(file_mix_dialog);
  mix_complete_file(any_selected_sound(ss),
		    snd_gtk_get_filename(file_mix_dialog),
		    "File: mix", with_mix_tags(ss));
}

void File_Mix_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  if (!file_mix_dialog)
    {
      file_mix_dialog = snd_gtk_file_selection_new(ss, STR_mix_file_p,
						   (GtkSignalFunc)file_mix_delete_callback,
						   (GtkSignalFunc)file_mix_ok_callback,
						   (GtkSignalFunc)file_mix_cancel_callback);
      add_dialog(ss, file_mix_dialog);
    }
  gtk_widget_show(file_mix_dialog);
}


/* ---------------- EDIT_HEADER ---------------- */

static GtkWidget *edit_header_dialog = NULL;
static file_data *edit_header_data;

static void edit_header_help_Callback(GtkWidget *w, gpointer context) 
{
  edit_header_dialog_help((snd_state *)context);
}

static void edit_header_cancel_Callback(GtkWidget *w, gpointer context) 
{
  gtk_widget_hide(edit_header_dialog);
}

static void edit_header_delete_Callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(edit_header_dialog);
}

static void edit_header_ok_Callback(GtkWidget *w, gpointer context) 
{
  snd_state *ss;
  snd_info *sp = (snd_info *)context;
  ss = sp->state;
  if (!(sp->read_only))
    edit_header_callback(ss, sp, edit_header_data);
  else snd_error("%s is write-protected", sp->shortname);
  gtk_widget_hide(edit_header_dialog);
}

void edit_header(snd_info *sp)
{
  /* like display-info, but writable.
   * need fields for srate, channels, type, format, data location, comment
   * if any are changed, need save button, cancel button, dismiss (leave unsaved but pending), reflect (change Snd display, not file)
   * this means the Snd-effective header is separate from the in-file header even across saves??
   */
  GtkWidget *help_button, *cancel_button, *save_button;
  snd_state *ss;
  char *str;
  file_info *hdr;
  if (!sp) return;
  ss = sp->state;
  hdr = sp->hdr;

  if (!edit_header_dialog)
    {
      edit_header_dialog = gtk_dialog_new();
      gtk_signal_connect(GTK_OBJECT(edit_header_dialog), "delete_event", GTK_SIGNAL_FUNC(edit_header_delete_Callback), (gpointer)ss);
      /* gtk_window_set_title(GTK_WINDOW(edit_header_dialog), STR_Edit_Header); */
      gtk_window_set_policy(GTK_WINDOW(edit_header_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
      set_background(edit_header_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width (GTK_CONTAINER(edit_header_dialog), 10);
      gtk_widget_set_usize(GTK_WIDGET(edit_header_dialog), 360, 250);
      gtk_widget_realize(edit_header_dialog);
      add_dialog(ss, edit_header_dialog);

      help_button = gtk_button_new_with_label(STR_Help);
      cancel_button = gtk_button_new_with_label(STR_Cancel);
      save_button = gtk_button_new_with_label(STR_Save);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), cancel_button, TRUE, TRUE, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), save_button, TRUE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(edit_header_dialog)->action_area), help_button, TRUE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(cancel_button), "clicked", GTK_SIGNAL_FUNC(edit_header_cancel_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(edit_header_help_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(save_button), "clicked", GTK_SIGNAL_FUNC(edit_header_ok_Callback), (gpointer)sp);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(cancel_button, ss);
      set_pushed_button_colors(save_button, ss);
      gtk_widget_show(cancel_button);
      gtk_widget_show(save_button);
      gtk_widget_show(help_button);

      edit_header_data = sndCreateFileDataForm(ss, GTK_DIALOG(edit_header_dialog)->vbox, STR_Edit_Header, TRUE, hdr->type, hdr->format, TRUE, FALSE);
      load_header_and_data_lists(edit_header_data, hdr->type, hdr->format, hdr->srate, hdr->chans, hdr->data_location, hdr->comment);
    }

  str = (char *)CALLOC(128, sizeof(char));
  sprintf(str, STR_Edit_header_of, sp->shortname);
  gtk_window_set_title(GTK_WINDOW(edit_header_dialog), str);
  FREE(str);
  gtk_widget_show(edit_header_dialog);
}

#if HAVE_GUILE_GTK
#include <guile-gtk.h>
 
#define Sg_open_file_dialog_widget    "sg-open-file-dialog-widget"
#define Sg_save_file_as_dialog_widget "sg-save-file-as-dialog-widget"
#define Sg_view_files_dialog_widget   "sg-view-files-dialog-widget"
#define Sg_mix_file_dialog_widget     "sg-mix-file-dialog-widget"
#define Sg_raw_data_dialog_widget     "sg-raw-data-dialog-widget"
#define Sg_new_file_dialog_widget     "sg-new-file-dialog-widget"
#define Sg_edit_header_dialog_widget  "sg-edit-header-dialog-widget"

static SCM sg_open_file_dialog_widget(void) 
{
  #define H_open_file_dialog_widget "(" Sg_open_file_dialog_widget ") -> gtk file selection widget used by File:Open"
  return(sgtk_wrap_gtkobj((GtkObject *)(open_dialog)));
}

static SCM sg_view_files_dialog_widget(void) 
{
  #define H_view_files_dialog_widget "(" Sg_view_files_dialog_widget ") -> gtk dialog widget for the View:Files option"
  return(sgtk_wrap_gtkobj((GtkObject *)(view_files_dialog)));
}

static SCM sg_save_file_as_dialog_widget(void) 
{
  #define H_save_file_as_dialog_widget "(" Sg_save_file_as_dialog_widget ") -> gtk file selection widget used by File:Save as"
  return(sgtk_wrap_gtkobj((GtkObject *)(save_as_dialog)));
}

static SCM sg_mix_file_dialog_widget(void) 
{
  #define H_mix_file_dialog_widget "(" Sg_mix_file_dialog_widget ") -> gtk file selection widget used by File:Mix"
  return(sgtk_wrap_gtkobj((GtkObject *)(file_mix_dialog)));
}

static SCM sg_edit_header_dialog_widget(void) 
{
  #define H_edit_header_dialog_widget "(" Sg_edit_header_dialog_widget ") -> gtk dialog used by Edit:Edit Header"
  return(sgtk_wrap_gtkobj((GtkObject *)(edit_header_dialog)));
}

static SCM sg_raw_data_dialog_widget(void) 
{
  #define H_raw_data_dialog_widget "(" Sg_raw_data_dialog_widget ") -> gtk dialog used to get info about a raw sound file"
  return(sgtk_wrap_gtkobj((GtkObject *)(raw_data_dialog)));
}

static SCM sg_new_file_dialog_widget(void) 
{
  #define H_new_file_dialog_widget "(" Sg_new_file_dialog_widget ") -> gtk dialog used to get info about a new file (File:New)"
  return(sgtk_wrap_gtkobj((GtkObject *)(new_dialog)));
}

void init_file_widgets(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure0_0(Sg_open_file_dialog_widget, sg_open_file_dialog_widget), H_open_file_dialog_widget);
  DEFINE_PROC(gh_new_procedure0_0(Sg_mix_file_dialog_widget, sg_mix_file_dialog_widget), H_mix_file_dialog_widget);
  DEFINE_PROC(gh_new_procedure0_0(Sg_save_file_as_dialog_widget, sg_save_file_as_dialog_widget), H_save_file_as_dialog_widget);
  DEFINE_PROC(gh_new_procedure0_0(Sg_view_files_dialog_widget, sg_view_files_dialog_widget), H_view_files_dialog_widget);
  DEFINE_PROC(gh_new_procedure0_0(Sg_edit_header_dialog_widget, sg_edit_header_dialog_widget), H_edit_header_dialog_widget);
  DEFINE_PROC(gh_new_procedure0_0(Sg_raw_data_dialog_widget, sg_raw_data_dialog_widget), H_raw_data_dialog_widget);
  DEFINE_PROC(gh_new_procedure0_0(Sg_new_file_dialog_widget, sg_new_file_dialog_widget), H_new_file_dialog_widget);

}
#endif
