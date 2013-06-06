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



/* file monitor */

/* glib now has a "gio" module that provides a fam-like service, but it requires using gio style file handlers throughout. 
*/

#if HAVE_G_FILE_MONITOR_DIRECTORY

bool initialize_file_monitor(void)
{
  return(true);
}

static void cleanup_new_file_watcher(void);
static void cleanup_edit_header_watcher(void);

void cleanup_file_monitor(void)
{
  cleanup_edit_header_watcher();
  cleanup_new_file_watcher();
  ss->file_monitor_ok = false;
}

void *unmonitor_file(void *watcher) 
{
  if (G_IS_FILE_MONITOR(watcher))
    g_file_monitor_cancel((GFileMonitor *)watcher);
  return(NULL);
}

static void *unmonitor_directory(void *watcher) 
{
  if (G_IS_FILE_MONITOR(watcher))
    g_file_monitor_cancel((GFileMonitor *)watcher);
  return(NULL);
}


static void sp_file_changed(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (sp->writing) return;
  
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
      /* this includes cp overwriting old etc */
      if (file_write_date(sp->filename) != sp->write_date) /* otherwise chmod? */
	{
	  sp->need_update = true;
	  if (auto_update(ss))
	    snd_update(sp);
	  else start_bomb(sp);
	}
#if HAVE_ACCESS
      else
	{
	  int err;
	  err = access(sp->filename, R_OK);
	  if (err < 0)
	    {
	      char *msg;
	      msg = mus_format("%s is read-protected!", sp->short_filename);
	      status_report(sp, "%s", msg);
	      free(msg);
	      sp->file_unreadable = true;
	      start_bomb(sp);
	    }
	  else
	    {
	      sp->file_unreadable = false;
	      clear_status_area(sp);
	      err = access(sp->filename, W_OK);
	      if (err < 0)   /* if err < 0, then we can't write (W_OK -> error ) */
		sp->file_read_only = FILE_READ_ONLY; 
	      else sp->file_read_only = FILE_READ_WRITE;
	      if ((sp->user_read_only == FILE_READ_ONLY) || 
		  (sp->file_read_only == FILE_READ_ONLY)) 
		show_lock(sp); 
	      else hide_lock(sp);
	    }
	}
#endif
      break;

    case G_FILE_MONITOR_EVENT_DELETED:
      /* snd_update will post a complaint in this case, but I like it explicit */
      if (mus_file_probe(sp->filename) == 0)
	{
	  /* user deleted file while editing it? */
	  status_report(sp, "%s no longer exists!", sp->short_filename);
	  sp->file_unreadable = true;
	  start_bomb(sp);
	  return;
	}

    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      if (sp->write_date != file_write_date(sp->filename))
	{
	  sp->file_unreadable = false;
	  sp->need_update = true;
	  if (auto_update(ss))
	    snd_update(sp);
	  else start_bomb(sp);
	}
      break;

    default:
      /* ignore the rest */
      break;
    }
}


void monitor_sound(snd_info *sp)
{
  GFile *file;
  GError *err = NULL;

  file = g_file_new_for_path(sp->filename);

  sp->file_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
  if (err != NULL)
    snd_warning("%s", err->message);
  else g_signal_connect(G_OBJECT(sp->file_watcher), "changed", G_CALLBACK(sp_file_changed), (gpointer)sp);   

  g_object_unref(file); /* is this safe? */
}



void view_files_unmonitor_directories(view_files_info *vdat) 
{
  if (vdat->dirs)
    {
      int i;
      for (i = 0; i < vdat->dirs_size; i++)
	if (vdat->dirs[i])
	  {
	    vdat->dirs[i] = (GFileMonitor *)unmonitor_directory(vdat->dirs[i]);
	    free(vdat->dir_names[i]);
	    vdat->dir_names[i] = NULL;
	  }
      free(vdat->dirs);
      vdat->dirs = NULL;
      free(vdat->dir_names);
      vdat->dir_names = NULL;
      vdat->dirs_size = 0;
    }
}


static void vf_watch_directory(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  view_files_info *vdat;
  char *filename;

  vdat = (view_files_info *)data;
  filename = g_file_get_path(file);

  fprintf(stderr, "%s: %d\n", filename, (int)ev);

  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
      if (access(filename, R_OK) == 0)
	vf_add_file_if_absent(vdat, filename);
      else vf_remove_file_if_present(vdat, filename);
      break;

    case G_FILE_MONITOR_EVENT_DELETED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      /* it's an existing file that is moved? -- I see the old name?? */
      vf_remove_file_if_present(vdat, filename);
      break;

    case G_FILE_MONITOR_EVENT_CREATED:
      vf_add_file_if_absent(vdat, filename);
      break;

    default:
      /* ignore the rest */
      break;
    }
  if (vdat->need_update)
    {
      view_files_update_list(vdat);
      vdat->need_update = false;
      if ((vdat->dialog) &&
	  (widget_is_active(vdat->dialog)))
	view_files_display_list(vdat);
    }
}


void view_files_monitor_directory(view_files_info *vdat, const char *dirname)
{
  GFile *file;
  GError *err = NULL;
  int i, loc = -1;

  if (vdat->dir_names)
    {
      for (i = 0; i < vdat->dirs_size; i++)
	if (vdat->dirs[i])
	  {
	    if (mus_strcmp(vdat->dir_names[i], dirname)) /* this was reversed?? */
	      return;
	  }
	else
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = vdat->dirs_size;
	  vdat->dirs_size += 4;
	  vdat->dirs = (GFileMonitor **)realloc(vdat->dirs, vdat->dirs_size * sizeof(GFileMonitor *));
	  vdat->dir_names = (char **)realloc(vdat->dir_names, vdat->dirs_size * sizeof(char *));
	  for (i = loc; i < vdat->dirs_size; i++)
	    {
	      vdat->dirs[i] = NULL;
	      vdat->dir_names[i] = NULL;
	    }
	}
    }
  else
    {
      vdat->dirs_size = 4;
      loc = 0;
      vdat->dirs = (GFileMonitor **)calloc(vdat->dirs_size, sizeof(GFileMonitor *));
      vdat->dir_names = (char **)calloc(vdat->dirs_size, sizeof(char *));
    }

  redirect_snd_error_to(redirect_vf_post_error, (void *)vdat);
  file = g_file_new_for_path(dirname);
  vdat->dirs[loc] = g_file_monitor_directory(file, G_FILE_MONITOR_NONE, NULL, &err);
  if (err != NULL)
    snd_warning("%s", err->message);
  else g_signal_connect(G_OBJECT(vdat->dirs[loc]), "changed", G_CALLBACK(vf_watch_directory), (gpointer)vdat);   
  g_object_unref(file); 
  redirect_snd_error_to(NULL, NULL);

  if (vdat->dirs[loc])
    vdat->dir_names[loc] = mus_strdup(dirname);
}


#else

void cleanup_file_monitor(void) {}
bool initialize_file_monitor(void) {return(false);}
void *unmonitor_file(void *watcher) {return(NULL);}
static void *unmonitor_directory(void *watcher) {return(NULL);}
void monitor_sound(snd_info *sp) {}

void view_files_unmonitor_directories(view_files_info *vdat) {}
void view_files_monitor_directory(view_files_info *vdat, const char *dirname) {}

#endif


/* -------------------------------------------------------------------------------- */



/* if icons do not get displayed, check the system preferences menu+toolbar dialog */

/* ---------------- file selector replacement ---------------- */

typedef struct fsb {

  /* base dialog */
  GtkWidget *dialog, *filter_text, *filter_label, *just_sounds_button;
  GtkWidget *file_label, *file_text, *ok_button, *mkdir_button, *cancel_button, *help_button, *extract_button;
  GtkWidget *panes, *dirs_menu, *files_menu;

  char *directory_name, *file_name;
  slist *directory_list, *file_list;
  void (*file_select_callback)(const char *filename, void *data);
  void (*file_double_click_callback)(const char *filename, void *data);
  void *file_select_data;
  void (*directory_select_callback)(const char *filename, void *data);
  void *directory_select_data;

  /* fam stuff */
  bool reread_directory;
  char *last_dir;
  dir_info *current_files;
  void *directory_watcher;
  int filter_choice, sorter_choice;

  /* popup info */
  GtkWidget **file_dir_items, **file_list_items;
  int file_list_items_size;

  dirpos_list *dir_list;
} fsb;


#define NO_MATCHING_FILES "[no matching files]"

static char *fsb_filter_text(fsb *fs)
{
  return((char *)gtk_entry_get_text(GTK_ENTRY(fs->filter_text)));
}


static void fsb_filter_set_text_with_directory(fsb *fs, const char *filter)
{
  char *name;
  int cur_dir_len;
  cur_dir_len = mus_strlen(fs->directory_name);
  name = (char *)calloc(cur_dir_len + 3, sizeof(char));
  mus_snprintf(name, cur_dir_len + 3, "%s%s", fs->directory_name, filter);
  gtk_entry_set_text(GTK_ENTRY(fs->filter_text), name);
  free(name);
}


static char *fsb_file_text(fsb *fs)
{
  return((char *)gtk_entry_get_text(GTK_ENTRY(fs->file_text)));
}


static void fsb_file_set_text(fsb *fs, const char *file)
{
  if (fs->file_name) free(fs->file_name);
  fs->file_name = mus_strdup(file);
  gtk_entry_set_text(GTK_ENTRY(fs->file_text), fs->file_name);
}


#if HAVE_G_FILE_MONITOR_DIRECTORY
static void force_directory_reread(fsb *fs);
static void watch_current_directory_contents(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_DELETED:
    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      if ((!(just_sounds(ss))) ||
	  (sound_file_p(g_file_get_path(file))))
	{
	  fsb *fs = (fsb *)data;
	  fs->reread_directory = true;
	  if ((fs->dialog) &&
	      (widget_is_active(fs->dialog)))
	    {
	      force_directory_reread(fs);
	      fs->reread_directory = false;
	    }
	}
      break;

    default:
      /* ignore the rest */
      break;
    }
}
#endif


static void fsb_update_lists(fsb *fs)
{
  dir_info *files;
  int i;
  char *pattern;

  /* reload directory list */
  slist_clear(fs->directory_list);

  files = find_directories_in_dir(fs->directory_name);
  if (files->len > 1) 
    snd_sort(0, files->files, files->len);

  for (i = 0; i < files->len; i++) 
    slist_append(fs->directory_list, files->files[i]->filename);

  /* set directory list position */
  {
    position_t list_top;
    char *dir_case;

    dir_case = mus_format("dir:%s", fs->directory_name);
    list_top = dirpos_list_top(fs->dir_list, dir_case);

    if (list_top != POSITION_UNKNOWN)
      {
	GtkAdjustment *adj;
	adj = gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->directory_list->scroller));
	if (ADJUSTMENT_UPPER(adj) < files->len * 16)
	  ADJUSTMENT_SET_UPPER(adj, files->len * 16);
	ADJUSTMENT_SET_VALUE(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->directory_list->scroller)), list_top);
      }
    else slist_moveto(fs->directory_list, 0);

    free(dir_case);
  }

  files = free_dir_info(files);

  /* reload file list */
  if (fs->current_files) fs->current_files = free_dir_info(fs->current_files);
  slist_clear(fs->file_list);

  pattern = filename_without_directory(fsb_filter_text(fs)); /* a pointer into the text */
  if ((!pattern) ||
      (strcmp(pattern, "*") == 0))
    {
      if (fs->filter_choice == NO_FILE_FILTER)
	files = find_files_in_dir(fs->directory_name);
      else files = find_filtered_files_in_dir(fs->directory_name, fs->filter_choice);
    }
  else files = find_filtered_files_in_dir_with_pattern(fs->directory_name, fs->filter_choice, pattern);

  if (files->len > 1)
    snd_sort(fs->sorter_choice, files->files, files->len);

  if (files->len == 0)
    slist_append(fs->file_list, NO_MATCHING_FILES);
  else
    {
      for (i = 0; i < files->len; i++) 
	slist_append(fs->file_list, files->files[i]->filename);
    }

  /* set file list position */
  {
    position_t list_top;
    list_top = dirpos_list_top(fs->dir_list, fs->directory_name);
    if (list_top != POSITION_UNKNOWN)
      {
	GtkAdjustment *adj;
	adj = gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->file_list->scroller));
	/* this is unbelievable -- there's no way to force the scrolled window to update its notion of the viewport size
	 *    so that the vertical adjustment is more than its (dumb) default size, so unless I set it by hand here,
	 *    I can't position the list!  I suppose I could have a timed call one second from here that would set
	 *    the position, but then I have to worry about the user clicking before I get to it.
	 */
	if (ADJUSTMENT_UPPER(adj) < files->len * 16)
	  ADJUSTMENT_SET_UPPER(adj, files->len * 16);
	ADJUSTMENT_SET_VALUE(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->file_list->scroller)), list_top);
      }
    else slist_moveto(fs->file_list, 0);
  }

  fs->current_files = files;

#if HAVE_G_FILE_MONITOR_DIRECTORY
  /* make sure fam knows which directory to watch */
  if ((fs->last_dir == NULL) ||
      (strcmp(fs->directory_name, fs->last_dir) != 0))
    {
      GFile *file;
      GError *err = NULL;
      if (fs->directory_watcher)
	unmonitor_directory(fs->directory_watcher); /* filename normally ignored */

      file = g_file_new_for_path(fs->directory_name);
      fs->directory_watcher = (void *)g_file_monitor_directory(file, G_FILE_MONITOR_NONE, NULL, &err);
      if (err != NULL)
	snd_warning("%s", err->message);
      else g_signal_connect(G_OBJECT(fs->directory_watcher), "changed", G_CALLBACK(watch_current_directory_contents), (gpointer)fs);
      g_object_unref(file);

      if (fs->last_dir) free(fs->last_dir);
      fs->last_dir = mus_strdup(fs->directory_name);
      fs->reread_directory = false;
    }
#endif
}


static void fsb_directory_select_callback(const char *dir_name, int row, void *data)
{
  fsb *fs = (fsb *)data;

  if (strcmp(dir_name, PARENT_DIRECTORY) == 0)
    {
      int i, slash_loc = 0, len;
      len = strlen(fs->directory_name);
      for (i = 1; i < len - 1; i++)
	if (fs->directory_name[i] == '/')
	  slash_loc = i;
      fs->directory_name[slash_loc + 1] = '\0';
    }
  else
    {
      char *old_name;

      if (row > 0) /* not ".." */
	/* save current directory list position */
	{
	  position_t position;
	  char *dir_case;
	  dir_case = mus_format("dir:%s", fs->directory_name);
	  position = ADJUSTMENT_VALUE(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->directory_list->scroller)));
	  dirpos_update(fs->dir_list, dir_case, position);
	  free(dir_case);
	}

      old_name = fs->directory_name;
      fs->directory_name = (char *)calloc(strlen(old_name) + strlen(dir_name) + 3, sizeof(char));
      strcpy(fs->directory_name, old_name);
      strcat(fs->directory_name, dir_name);
      strcat(fs->directory_name, "/");

      free(old_name);
    }

  fsb_filter_set_text_with_directory(fs, "*");
  fsb_file_set_text(fs, fs->directory_name);
  fsb_update_lists(fs);
  if (fs->directory_select_callback)
    (*(fs->directory_select_callback))((const char *)(fs->directory_name), fs->directory_select_data);
}


static char *fsb_fullname(fsb *fs, const char *filename)
{
  if (filename)
    {
      char *fullname;
      fullname = (char *)calloc(strlen(fs->directory_name) + strlen(filename) + 2, sizeof(char));
      strcpy(fullname, fs->directory_name);
      strcat(fullname, filename);
      return(fullname);
    }
  return(NULL);
}


static void fsb_file_select_callback(const char *file_name, int row, void *data)
{
  fsb *fs = (fsb *)data;
  if (strcmp(file_name, NO_MATCHING_FILES) != 0)
    {
      char *fullname;
      fullname = fsb_fullname(fs, file_name);
      fsb_file_set_text(fs, fullname);
      if (fs->file_select_callback)
	(*(fs->file_select_callback))((const char *)fullname, fs->file_select_data);

      /* save current file list position */
      {
	position_t position;
	position = ADJUSTMENT_VALUE(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->file_list->scroller)));
	dirpos_update(fs->dir_list, fs->directory_name, position);
      }

      free(fullname);
    }
}


#if WITH_AUDIO
static char *fsb_selected_file(fsb *fs)
{
  return(fsb_fullname(fs, slist_selection(fs->file_list)));
}
#endif


static void fsb_filter_activate(GtkWidget *w, gpointer context) 
{
  fsb *fs = (fsb *)context;
  char *filter;
  filter = fsb_filter_text(fs);
  if (filter)
    {
      if (fs->directory_name) free(fs->directory_name);
      fs->directory_name = just_directory(filter); /* this allocates */
      fsb_update_lists(fs);
    }
}


static void fsb_file_activate(GtkWidget *w, gpointer context) 
{
  fsb *fs = (fsb *)context;
  char *file;
  file = fsb_file_text(fs);
  if (file)
    {
      if ((strcmp(file, NO_MATCHING_FILES) != 0) &&
	  (fs->file_select_callback))
	(*(fs->file_select_callback))(file, fs->file_select_data);
    }
}




static bool fsb_directory_button_press_callback(GdkEventButton *ev, void *data);
static bool fsb_files_button_press_callback(GdkEventButton *ev, void *data);

static fsb *make_fsb(const char *title, const char *file_lab, const char *ok_lab,
		     void (*add_innards)(GtkWidget *vbox, void *data), void *data, /* add_innards data can be either file_dialog_info or save_as_dialog_info */
		     const gchar *stock, bool with_extract, bool with_mkdir)
{
  fsb *fs;
  char *cur_dir = NULL, *pwd = NULL;
  
  fs = (fsb *)calloc(1, sizeof(fsb));
  if (just_sounds(ss))
    fs->filter_choice = JUST_SOUNDS_FILTER;
  else fs->filter_choice = NO_FILE_FILTER;
  fs->dir_list = make_dirpos_list();


  /* -------- current working directory -------- */
  if (open_file_dialog_directory(ss))
    pwd = open_file_dialog_directory(ss);
  else pwd = mus_getcwd();
  cur_dir = (char *)calloc(strlen(pwd) + 2, sizeof(char));
  strcpy(cur_dir, pwd);
  if ((cur_dir) && (cur_dir[strlen(cur_dir) - 1] != '/'))
    strcat(cur_dir, "/");
  fs->directory_name = cur_dir;


  /* -------- base dialog -------- */
  fs->dialog = snd_gtk_dialog_new();
  gtk_window_set_title(GTK_WINDOW(fs->dialog), title);
  sg_make_resizable(fs->dialog);
  gtk_container_set_border_width(GTK_CONTAINER(fs->dialog), 10);
  gtk_widget_realize(fs->dialog);


  /* -------- buttons -------- */
  fs->help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
  gtk_widget_set_name(fs->help_button, "dialog_button");
  if (!with_mkdir) widget_set_margin_left(fs->help_button, 16);

  fs->cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
  gtk_widget_set_name(fs->cancel_button, "dialog_button");
  set_stock_button_label(fs->cancel_button, I_GO_AWAY);
  if (!with_mkdir) widget_set_margin_left(fs->cancel_button, 16);

  if (with_mkdir)
    {
      fs->mkdir_button = sg_button_new_from_stock_with_label("Mkdir", GTK_STOCK_REFRESH);
      gtk_widget_set_name(fs->mkdir_button, "dialog_button");
    }

  if (with_extract)
    {
      fs->extract_button = sg_button_new_from_stock_with_label("Extract", GTK_STOCK_CUT);
      gtk_widget_set_name(fs->extract_button, "dialog_button");
    }

  if (ok_lab)
    {
      if (stock)
	fs->ok_button = sg_button_new_from_stock_with_label(ok_lab, stock);
      else fs->ok_button = gtk_button_new_with_label(ok_lab);
    }
  else fs->ok_button = gtk_button_new_from_stock(stock);
  gtk_widget_set_name(fs->ok_button, "dialog_button");
  if (!with_mkdir) widget_set_margin_left(fs->ok_button, 40);

#if HAVE_GTK_3
  add_highlight_button_style(fs->ok_button);
  add_highlight_button_style(fs->cancel_button);
  if (with_mkdir) add_highlight_button_style(fs->mkdir_button);
  add_highlight_button_style(fs->help_button);
  if (with_extract) add_highlight_button_style(fs->extract_button);
#endif

  gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(fs->dialog)), fs->ok_button, true, true, 10);
  if (with_mkdir) gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(fs->dialog)), fs->mkdir_button, true, true, 10);
  if (with_extract) gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(fs->dialog)), fs->extract_button, true, true, 10);
  gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(fs->dialog)), fs->cancel_button, true, true, 10);
  gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(fs->dialog)), fs->help_button, true, true, 10);

  gtk_widget_show(fs->ok_button);
  gtk_widget_show(fs->cancel_button);
  gtk_widget_show(fs->help_button);
  if (with_mkdir) gtk_widget_show(fs->mkdir_button);
  if (with_extract) gtk_widget_show(fs->extract_button);


  /* -------- file -------- */
  {
    GtkWidget *row;
    char *str;

    row = gtk_hbox_new(false, 10);
    gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(fs->dialog)), row, false, false, 10);
    gtk_widget_show(row);

    /* file text entry */
    fs->file_label = gtk_label_new(file_lab);

    str = mus_format("<b>%s</b>", file_lab);
    gtk_label_set_markup(GTK_LABEL(fs->file_label), str);
    gtk_label_set_use_markup(GTK_LABEL(fs->file_label), true);
    free(str);

    gtk_box_pack_start(GTK_BOX(row), fs->file_label, false, false, 0);
    sg_left_justify_label(fs->file_label);
    gtk_widget_show(fs->file_label);

    fs->file_text = snd_entry_new(row, NULL, WITH_WHITE_BACKGROUND);
    gtk_entry_set_text(GTK_ENTRY(fs->file_text), fs->directory_name);
    if (fs->directory_name)
      gtk_editable_set_position(GTK_EDITABLE(fs->file_text), mus_strlen((char *)(fs->directory_name)));
    SG_SIGNAL_CONNECT(fs->file_text, "activate", fsb_file_activate, (gpointer)fs);
  }


  /* -------- directory and file lists -------- */
  fs->panes = gtk_hpaned_new();
  add_paned_style(fs->panes);
  gtk_container_set_border_width(GTK_CONTAINER(fs->panes), 2);
  gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(fs->dialog)), fs->panes, true, true, 10);
  gtk_widget_show(fs->panes);

  fs->directory_list = slist_new(fs->panes, NULL, 0, PANED_ADD1);
  fs->directory_list->select_callback = fsb_directory_select_callback;
  fs->directory_list->select_callback_data = (void *)fs;
  fs->directory_list->button_press_callback = fsb_directory_button_press_callback;
  fs->directory_list->button_press_callback_data = (void *)fs;

  fs->file_list = slist_new(fs->panes, NULL, 0, PANED_ADD2);
  fs->file_list->select_callback = fsb_file_select_callback;
  fs->file_list->select_callback_data = (void *)fs;
  fs->file_list->button_press_callback = fsb_files_button_press_callback;
  fs->file_list->button_press_callback_data = (void *)fs;

  gtk_widget_set_size_request(fs->panes, -1, 150);


  /* -------- special case box -------- */
  add_innards(DIALOG_CONTENT_AREA(fs->dialog), data);

  /* -------- filter -------- */
  {
    GtkWidget *row;
    row = gtk_hbox_new(false, 8);
    gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(fs->dialog)), row, false, false, 16);
    gtk_widget_show(row);

    /* filter text entry */
    fs->filter_label = gtk_label_new("files listed:");
    gtk_box_pack_start(GTK_BOX(row), fs->filter_label, false, false, 0);
    sg_left_justify_label(fs->filter_label);
    gtk_widget_show(fs->filter_label);

    fs->filter_text = snd_entry_new(row, NULL, WITH_WHITE_BACKGROUND);
    fsb_filter_set_text_with_directory(fs, "*");
    if (fs->directory_name)
      gtk_editable_set_position(GTK_EDITABLE(fs->filter_text), 1 + mus_strlen((char *)(fs->directory_name)));
    SG_SIGNAL_CONNECT(fs->filter_text, "activate", fsb_filter_activate, (gpointer)fs);
  }

  fsb_update_lists(fs);

  gtk_widget_show(fs->dialog);
  gtk_paned_set_position(GTK_PANED(fs->panes), 120);

  return(fs);
}


static void force_directory_reread(fsb *fs)
{
  /* protect the current selection and file name entry values across an update, also try to maintain window position */
  char *filename = NULL, *selected;
  gdouble scroller_position;
  int i;

  selected = mus_strdup(slist_selection(fs->file_list));
  filename = mus_strdup(fsb_file_text(fs));

  scroller_position = ADJUSTMENT_VALUE(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->file_list->scroller)));
  fsb_update_lists(fs);
  fsb_file_set_text(fs, filename);

  if (selected)
    {
      for (i = 0; i < fs->current_files->len; i++)
	if (mus_strcmp(selected, fs->current_files->files[i]->filename))
	  {
	    slist_select(fs->file_list, i); /* doesn't call select callback */
	    break;
	  }
      free(selected);
    }
  if (filename) free(filename);
  ADJUSTMENT_SET_VALUE(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->file_list->scroller)), scroller_position);
}


/* ---------------- popups ---------------- */

/* dir list popup */

static void file_dir_item_activate_callback(GtkWidget *w, gpointer context)
{
  /* set fs->directory_name, and filter text ("*", then fsb_update_lists */
  fsb *fs = (fsb *)context;
  if (fs->directory_name) free(fs->directory_name);
  fs->directory_name = mus_format("%s/", gtk_label_get_text(GTK_LABEL(gtk_bin_get_child(GTK_BIN(w)))));
  fsb_filter_set_text_with_directory(fs, "*");
  fsb_update_lists(fs);
}

/* dir_items, but strs generated on the fly, current in filter text */

static bool fsb_directory_button_press_callback(GdkEventButton *ev, void *data)
{
  fsb *fs = (fsb *)data;
  if ((NO_BUCKY_BITS_P(EVENT_STATE(ev))) && 
      (EVENT_TYPE(ev) == GDK_BUTTON_PRESS) && 
      (EVENT_BUTTON(ev) == POPUP_BUTTON))
    {
      char *current_filename = NULL;
      int i, dirs_to_display = 0, len = 0;

      if (fs->file_dir_items == NULL)
	{
	  fs->dirs_menu = gtk_menu_new();
	  add_menu_style(fs->dirs_menu);
	  fs->file_dir_items = (GtkWidget **)calloc(FILENAME_LIST_SIZE, sizeof(GtkWidget *));
	  for (i = 0; i < FILENAME_LIST_SIZE; i++)
	    {
	      fs->file_dir_items[i] = gtk_menu_item_new_with_label("oops");
	      set_user_int_data(G_OBJECT(fs->file_dir_items[i]), i);
	      gtk_menu_shell_append(GTK_MENU_SHELL(fs->dirs_menu), fs->file_dir_items[i]);
	      gtk_widget_show(fs->file_dir_items[i]);
	      SG_SIGNAL_CONNECT(fs->file_dir_items[i], "activate", file_dir_item_activate_callback, (gpointer)fs);	      
	    }
	}
      current_filename = fs->directory_name;
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
	      gtk_label_set_text(GTK_LABEL(gtk_bin_get_child(GTK_BIN(fs->file_dir_items[i]))), dirs[i]);
	      gtk_widget_show(fs->file_dir_items[i]);
	      free(dirs[i]);
	    }
	  free(dirs);
	}

      for (i = dirs_to_display; i < FILENAME_LIST_SIZE; i++)
	if ((fs->file_dir_items[i]) &&
	    (widget_is_active(fs->file_dir_items[i])))
	  gtk_widget_hide(fs->file_dir_items[i]);

      gtk_menu_popup(GTK_MENU(fs->dirs_menu), NULL, NULL, NULL, NULL, EVENT_BUTTON(ev), EVENT_TIME(ev));
    }
  return(false);
}


#define NO_FILTER_LABEL "no filter"

#define FILE_FILTER_OFFSET 1024
#define NO_FILE_FILTER_OFFSET 2048

static void sort_files_and_redisplay(fsb *fs);

static void file_list_item_activate_callback(GtkWidget *w, gpointer context)
{
  fsb *fs = (fsb *)context;
  int choice = 0;
  choice = get_user_int_data(G_OBJECT(w));
  if (choice >= FILE_FILTER_OFFSET)
    {
      set_toggle_button(fs->just_sounds_button, false, false, (void *)fs);
      if (choice == NO_FILE_FILTER_OFFSET)
	fs->filter_choice = NO_FILE_FILTER;
      else fs->filter_choice = choice - FILE_FILTER_OFFSET + 2;
      force_directory_reread(fs);
    }
  else
    {
      fs->sorter_choice = choice;
      sort_files_and_redisplay(fs);
    }
}

static GtkWidget *make_file_list_item(fsb *fs, int choice)
{
  const char *item_label[7] = {"a..z", "z..a", "new..old", "old..new", "small..big", "big..small", "unused"};
  GtkWidget *w;

  w = gtk_menu_item_new_with_label(item_label[choice]);
  set_user_int_data(G_OBJECT(w), choice);
  gtk_menu_shell_append(GTK_MENU_SHELL(fs->files_menu), w);
  gtk_widget_show(w);
  SG_SIGNAL_CONNECT(w, "activate", file_list_item_activate_callback, (gpointer)fs);	      
  return(w);
}


static bool fsb_files_button_press_callback(GdkEventButton *ev, void *data)
{
  fsb *fs = (fsb *)data;
  if ((NO_BUCKY_BITS_P(EVENT_STATE(ev))) && 
      (EVENT_TYPE(ev) == GDK_BUTTON_PRESS) && 
      (EVENT_BUTTON(ev) == POPUP_BUTTON))
    {
      int i, items_len;
      if (fs->file_list_items == NULL)
	{
	  /* set up the default menu items */
	  fs->files_menu = gtk_menu_new();
	  add_menu_style(fs->files_menu);
	  fs->file_list_items = (GtkWidget **)calloc(SORT_XEN, sizeof(GtkWidget *));
	  fs->file_list_items_size = SORT_XEN;
	  for (i = 0; i < SORT_XEN; i++)
	    fs->file_list_items[i] = make_file_list_item(fs, i);
	}

      /* clear any trailers just in case */
      if (fs->file_list_items_size > SORT_XEN)
	for (i = SORT_XEN; i < fs->file_list_items_size; i++)
	  gtk_widget_hide(fs->file_list_items[i]);

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
	if (fs->filter_choice != NO_FILE_FILTER) items_len++;

	if (items_len > fs->file_list_items_size)
	  {
	    fs->file_list_items = (GtkWidget **)realloc(fs->file_list_items, items_len * sizeof(GtkWidget *));
	    for (i = fs->file_list_items_size; i < items_len; i++)
	      fs->file_list_items[i] = make_file_list_item(fs, i);
	    fs->file_list_items_size = items_len;
	  }
      }

      /* make sure all the added sorter labels are correct, bg blue, and items active */
      if (fs->file_list_items_size > SORT_XEN)
	{
	  int k = SORT_XEN;

	  /* sorters */
	  for (i = 0; i < ss->file_sorters_size; i++)
	    {
	      if (!(XEN_FALSE_P(XEN_VECTOR_REF(ss->file_sorters, i))))
		{
		  gtk_label_set_text(GTK_LABEL(gtk_bin_get_child(GTK_BIN(fs->file_list_items[k]))),
				     XEN_TO_C_STRING(XEN_CAR(XEN_VECTOR_REF(ss->file_sorters, i))));
		  reset_user_int_data(G_OBJECT(fs->file_list_items[k]), SORT_XEN + i);
		  widget_modify_bg(fs->file_list_items[k], GTK_STATE_NORMAL, ss->lighter_blue);
		  if (!(widget_is_active(fs->file_list_items[k])))
		    gtk_widget_show(fs->file_list_items[k]);
		  k++;
		}
	    }
	  
	  for (i = 0; i < ss->file_filters_size; i++)
	    {
	      if (!(XEN_FALSE_P(XEN_VECTOR_REF(ss->file_filters, i))))
		{
		  gtk_label_set_text(GTK_LABEL(gtk_bin_get_child(GTK_BIN(fs->file_list_items[k]))),
				     XEN_TO_C_STRING(XEN_CAR(XEN_VECTOR_REF(ss->file_filters, i))));
		  reset_user_int_data(G_OBJECT(fs->file_list_items[k]), i + FILE_FILTER_OFFSET);
		  widget_modify_bg(fs->file_list_items[k], GTK_STATE_NORMAL, ss->light_blue);
		  if (!(widget_is_active(fs->file_list_items[k])))
		    gtk_widget_show(fs->file_list_items[k]);
		  k++;
		}
	    }

	  /* add "no filter" item if currently filtered */
	  if (fs->filter_choice != NO_FILE_FILTER)
	    {
	      gtk_label_set_text(GTK_LABEL(gtk_bin_get_child(GTK_BIN(fs->file_list_items[k]))), NO_FILTER_LABEL);
	      reset_user_int_data(G_OBJECT(fs->file_list_items[k]), NO_FILE_FILTER_OFFSET);
	      widget_modify_bg(fs->file_list_items[k], GTK_STATE_NORMAL, ss->light_blue);
	      if (!(widget_is_active(fs->file_list_items[k])))
		gtk_widget_show(fs->file_list_items[k]);
	    }

	}
      gtk_menu_popup(GTK_MENU(fs->files_menu), NULL, NULL, NULL, NULL, EVENT_BUTTON(ev), EVENT_TIME(ev));
    }
  else
    {
      if ((EVENT_TYPE(ev) == GDK_2BUTTON_PRESS) && /* double-click */

#ifdef GDK_BUTTON_PRIMARY
	  (EVENT_BUTTON(ev) == GDK_BUTTON_PRIMARY)) /* gdkevents.h: "since 3.4" which hasn't happened yet! */
#else
	  (EVENT_BUTTON(ev) == 1))
#endif

	{
	  if (fs->file_double_click_callback)
	    (*(fs->file_double_click_callback))((const char *)(fs->file_name), fs->file_select_data);
	}
    }

  return(false);
}


/* ---------------- just-sounds (file-filters) ---------------- */

static void sort_files_and_redisplay(fsb *fs)
{
  /* if just sorting, no need to read the directory */
  dir_info *cur_dir;
  char *selected;
  gdouble scroller_position;

  cur_dir = fs->current_files;
  scroller_position = ADJUSTMENT_VALUE(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->file_list->scroller)));
  selected = mus_strdup(slist_selection(fs->file_list));
  slist_clear(fs->file_list);

  if (cur_dir->len == 0)
    slist_append(fs->file_list, NO_MATCHING_FILES);
  else
    {
      int i;
      snd_sort(fs->sorter_choice, cur_dir->files, cur_dir->len);
      for (i = 0; i < cur_dir->len; i++) 
	slist_append(fs->file_list, cur_dir->files[i]->filename);
    }

  if (selected)
    {
      int i;
      for (i = 0; i < cur_dir->len; i++)
	if (mus_strcmp(selected, cur_dir->files[i]->filename))
	  {
	    slist_select(fs->file_list, i); /* doesn't call select callback */
	    scroller_position = i * 16;
	    break;
	  }
      free(selected);
    }

  ADJUSTMENT_SET_VALUE(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(fs->file_list->scroller)), scroller_position);
}


static void just_sounds_callback(GtkWidget *w, gpointer data)
{
  fsb *fs = (fsb *)data;
  if (TOGGLE_BUTTON_ACTIVE(w))
    fs->filter_choice = JUST_SOUNDS_FILTER;
  else fs->filter_choice = NO_FILE_FILTER;
  force_directory_reread(fs);
}



/* -------- play selected file handlers -------- */

typedef struct dialog_play_info {
  GtkWidget *play_button;
  snd_info *player;
  fsb *fs;
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


#if WITH_AUDIO
static void play_selected_callback(GtkWidget *w, gpointer data)
{
  dialog_play_info *dp = (dialog_play_info *)data;
  if (TOGGLE_BUTTON_ACTIVE(w))
    {
      char *filename;
      if ((dp->player) && 
	  (dp->player->playing)) 
	stop_playing_sound(dp->player, PLAY_BUTTON_UNSET);
      filename = fsb_selected_file(dp->fs); /* allocates, so free below */
      if (filename)
	{
	  if (mus_file_probe(filename))
	    {
	      dp->player = make_sound_readable(filename, false);
	      dp->player->delete_me = dp;
	      if (dp->player)
		play_sound(dp->player, 0, NO_END_SPECIFIED);
	    }
	  free(filename);
	}
    }
  else file_dialog_stop_playing(dp);
}
#endif


static bool file_is_directory(fsb *fs)
{
  char *filename;
  filename = fsb_file_text(fs);
  return((!filename) || (directory_p(filename)));
}


static bool file_is_nonexistent_directory(fsb *fs)
{
  char *filename = NULL;
  filename = mus_strdup(fsb_file_text(fs));
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
		bool result;
		filename[i] = '\0';
		result = directory_p(filename);
		free(filename);
		return(result);
	      }
	}
    }
  free(filename);
  return(false);
}


static void post_sound_info(GtkWidget *info1, GtkWidget *info2, const char *filename, bool with_filename)
{
  /* filename is known[strongly believed] to be a sound file, etc */
  char *buf;

  buf = (char *)calloc(LABEL_BUFFER_SIZE, sizeof(char));
  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s%s%d chan%s, %d Hz, %.3f secs",
	       (with_filename) ? filename_without_directory(filename) : "",
	       (with_filename) ? ": " : "",
	       mus_sound_chans(filename),
	       (mus_sound_chans(filename) > 1) ? "s" : "",
	       mus_sound_srate(filename),
	       mus_sound_duration(filename));

  info_widget_display(info1, buf);
  info_widget_set_size(info1, 1 + strlen(buf));

  mus_snprintf(buf, LABEL_BUFFER_SIZE, "%s, %s%s",
	       mus_header_type_name(mus_sound_header_type(filename)),
	       short_data_format_name(mus_sound_data_format(filename), filename),
	       snd_strftime(", %d-%b-%Y", mus_sound_write_date(filename)));

  info_widget_display(info2, buf);
  info_widget_set_size(info2, 1 + strlen(buf));

  free(buf);
}


/* ---------------- file dialogs ---------------- */

typedef struct file_dialog_info {
  fsb *fs;
  read_only_t file_dialog_read_only;
  GtkWidget *frame, *info1, *info2, *vbox;
  dialog_play_info *dp;
  void *unsound_directory_watcher; /* doesn't exist, not a sound file, bogus header, etc */
  void *info_filename_watcher;     /* watch for change in selected file and repost info */
  char *unsound_dirname, *unsound_filename;
  char *info_filename;
} file_dialog_info;

static void clear_open_handlers(fsb *fs);

static void unpost_file_info(file_dialog_info *fd)
{
  info_widget_display(fd->info1, "");
  info_widget_display(fd->info2, "");
  clear_open_handlers(fd->fs);

  if (fd->info_filename_watcher)
    {
      fd->info_filename_watcher = unmonitor_file(fd->info_filename_watcher);
      if (fd->info_filename) {free(fd->info_filename); fd->info_filename = NULL;}
    }
}


#if HAVE_G_FILE_MONITOR_DIRECTORY
static void repost_sound_info(file_dialog_info *fd)
{
  if ((mus_file_probe(fd->info_filename)) &&
      (plausible_sound_file_p(fd->info_filename)))
    post_sound_info(fd->info1, fd->info2, fd->info_filename, true);
  else unpost_file_info(fd);
}


static void watch_info_file(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
    case G_FILE_MONITOR_EVENT_DELETED:
    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      repost_sound_info((file_dialog_info *)data);
      break;

    default:
      /* ignore the rest */
      break;
    }
}
#endif


static gboolean filer_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  if (EVENT_KEYVAL(event) == snd_K_Tab)
    {
      gtk_entry_set_text(GTK_ENTRY(w), sound_filename_completer(w, (char *)gtk_entry_get_text(GTK_ENTRY(w)), NULL));
      gtk_editable_set_position(GTK_EDITABLE(w), mus_strlen((char *)gtk_entry_get_text(GTK_ENTRY(w))));
      return(true);
    }
  return(false);
}


/* -------- File Open/View/Mix/ Dialogs -------- */

static file_dialog_info *odat = NULL; /* open file */
static file_dialog_info *mdat = NULL; /* mix file */
static file_dialog_info *idat = NULL; /* insert file */

void alert_new_file(void) 
{
  if (ss->file_monitor_ok) return;
  if (odat)
    {
      odat->fs->reread_directory = true;
      if (widget_is_active(odat->fs->dialog))
	{
	  force_directory_reread(odat->fs);
	  odat->fs->reread_directory = false;
	}
    }
  if (mdat)
    {
      mdat->fs->reread_directory = true;
      if (widget_is_active(mdat->fs->dialog))
	{
	  force_directory_reread(mdat->fs);
	  mdat->fs->reread_directory = false;
	}
    }
  if (idat)
    {
      idat->fs->reread_directory = true;
      if (widget_is_active(idat->fs->dialog))
	{
	  force_directory_reread(idat->fs);
	  idat->fs->reread_directory = false;
	}
    }
}


void reflect_just_sounds(void)
{
  if ((odat) && (odat->fs->just_sounds_button))
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(odat->fs->just_sounds_button), just_sounds(ss));
  if ((mdat) && (mdat->fs->just_sounds_button))
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(mdat->fs->just_sounds_button), just_sounds(ss));
  if ((idat) && (idat->fs->just_sounds_button))
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(idat->fs->just_sounds_button), just_sounds(ss));
}


static void dialog_directory_select_callback(const char *dirname, void *context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  set_sensitive(fd->fs->ok_button, (!(file_is_directory(fd->fs))));
  unpost_file_info(fd);
}


static void dialog_select_callback(const char *filename, void *context)
{
  file_dialog_info *fd = (file_dialog_info *)context;

  unpost_file_info(fd);
  if ((filename) && 
      (!(directory_p(filename))) &&
      (plausible_sound_file_p(filename)))
    {
      post_sound_info(fd->info1, fd->info2, filename, true);
      gtk_widget_show(fd->frame);
      gtk_widget_show(fd->vbox);
      gtk_widget_show(fd->info1);
      gtk_widget_show(fd->info2);
#if WITH_AUDIO
      gtk_widget_show(fd->dp->play_button);
#endif
#if HAVE_G_FILE_MONITOR_DIRECTORY
      {
	GFile *file;
	GError *err = NULL;

	fd->info_filename = mus_strdup(filename);
	file = g_file_new_for_path(filename);
	fd->info_filename_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
	if (err != NULL)
	  snd_warning("%s", err->message);
	else g_signal_connect(G_OBJECT(fd->info_filename_watcher), "changed", G_CALLBACK(watch_info_file), (gpointer)fd);
	g_object_unref(file);
      }
#endif
    }
  else
    {
      unpost_file_info(fd);
    }
  set_sensitive(fd->fs->ok_button, (!(file_is_directory(fd->fs))));
}


static void open_innards(GtkWidget *vbox, void *data)
{
  GtkWidget *center_info;
  file_dialog_info *fd = (file_dialog_info *)data;

  center_info = gtk_hbox_new(true, 10);
  gtk_box_pack_start(GTK_BOX(vbox), center_info, false, false, 0);
  gtk_widget_show(center_info);

  fd->frame = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(fd->frame), GTK_SHADOW_ETCHED_IN);
  gtk_container_set_border_width(GTK_CONTAINER(fd->frame), 1);
  widget_modify_bg(fd->frame, GTK_STATE_NORMAL, ss->zoom_color);
  gtk_box_pack_start(GTK_BOX(center_info), fd->frame, false, false, 10);

  fd->vbox = gtk_vbox_new(false, 0);
  gtk_container_set_border_width(GTK_CONTAINER(fd->vbox), 10);
  gtk_container_add(GTK_CONTAINER(fd->frame), fd->vbox);

  fd->info1 = make_info_widget();
  gtk_box_pack_start(GTK_BOX(fd->vbox), fd->info1, true, true, 0);
	
  fd->info2 = make_info_widget();
  gtk_box_pack_start(GTK_BOX(fd->vbox), fd->info2, true, true, 0);

  gtk_widget_show(fd->frame);
  gtk_widget_show(fd->vbox);
  gtk_widget_show(fd->info1);
  gtk_widget_show(fd->info2);
}


static gboolean reflect_text_in_open_button(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  fsb *fs;
  char *filename = NULL;
  fs = fd->fs;

  set_sensitive(fs->ok_button, (!(file_is_directory(fs))));
  if (fs->mkdir_button) set_sensitive(fs->mkdir_button, file_is_nonexistent_directory(fs));

  /* try to find the current partial filename in the files list, and move to it if found */
  /* try to move file list to show possible matches,
   *   if a sound file, show info
   */
  filename = fsb_file_text(fs);
  if ((filename) && (*filename))
    {
      int i, pos = -1, l = 0, u;
      dir_info *cur_dir;
      cur_dir = fs->current_files;
      u = cur_dir->len - 1;
      while (true)
	{
	  int comp;
	  if (u < l) break;
	  i = (l + u) / 2;
	  comp = strcmp(cur_dir->files[i]->full_filename, filename);
	  if (comp == 0)
	    {
	      pos = i;
	      break;
	    }
	  if (comp < 0) /* files[i]->full_filename less than filename */
	    l = i + 1;
	  else u = i - 1;
	}
      slist_moveto(fs->file_list, pos);
      if ((mus_file_probe(filename)) && 
	  (!directory_p(filename)))
	{
	  if (sound_file_p(filename))
	    post_sound_info(fd->info1, fd->info2, filename, true);
	}
    }

  return(false);
}


static file_dialog_info *make_file_dialog(read_only_t read_only, const char *title, const char *file_title, const char *ok_title,
					  snd_dialog_t which_dialog, 
					  GCallback file_ok_proc,
					  GCallback file_mkdir_proc,
					  GCallback file_delete_proc,
					  GCallback file_dismiss_proc,
					  GCallback file_help_proc,
					  const gchar *stock)
{
  file_dialog_info *fd;
  fsb *fs;

  fd = (file_dialog_info *)calloc(1, sizeof(file_dialog_info));
  fd->file_dialog_read_only = read_only;
  fd->dp = (dialog_play_info *)calloc(1, sizeof(dialog_play_info));

  fd->fs = make_fsb(title, file_title, ok_title, open_innards, (void *)fd, stock, false, false);
  fs = fd->fs;
  fd->dp->fs = fs;

  SG_SIGNAL_CONNECT(fs->help_button, "clicked", file_help_proc, (gpointer)fd);
  SG_SIGNAL_CONNECT(fs->file_text, "key_press_event", filer_key_press, NULL);
  SG_SIGNAL_CONNECT(fs->ok_button, "clicked", file_ok_proc, (gpointer)fd);
  SG_SIGNAL_CONNECT(fs->cancel_button, "clicked", file_dismiss_proc, (gpointer)fd);
  if (file_delete_proc) 
    SG_SIGNAL_CONNECT(fs->dialog, "delete_event", file_delete_proc, (gpointer)fd);

  fs->file_select_data = (void *)fd;
  fs->file_select_callback = dialog_select_callback;
  fs->file_double_click_callback = NULL;
  fs->directory_select_data = (void *)fd;
  fs->directory_select_callback = dialog_directory_select_callback;

  /* this needs fs, so it can't be in open_innards */
  {
    GtkWidget *hbox, *spacer;

    hbox = gtk_hbox_new(true, 0);
    gtk_box_pack_end(GTK_BOX(fd->vbox), hbox, false, false, 0);
    gtk_widget_show(hbox);

    fs->just_sounds_button = gtk_check_button_new_with_label("sound files only");
    gtk_box_pack_start(GTK_BOX(hbox), fs->just_sounds_button, true, true, 2);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(fs->just_sounds_button), just_sounds(ss));
    SG_SIGNAL_CONNECT(fs->just_sounds_button, "toggled", just_sounds_callback, (void *)fs);
    gtk_widget_show(fs->just_sounds_button);

#if WITH_AUDIO
    fd->dp->play_button = gtk_check_button_new_with_label("play selected sound");
    gtk_box_pack_end(GTK_BOX(hbox), fd->dp->play_button, true, true, 2);
    SG_SIGNAL_CONNECT(fd->dp->play_button, "toggled", play_selected_callback, fd->dp);
    gtk_widget_show(fd->dp->play_button);
#endif

    /* this order of box_pack_end calls puts the spacer before (above) the buttons */
    spacer = gtk_vseparator_new();
    gtk_box_pack_end(GTK_BOX(fd->vbox), spacer, false, false, 6);
    gtk_widget_show(spacer);
  }

  info_widget_display(fd->info1, "");
  info_widget_display(fd->info2, "");

  SG_SIGNAL_CONNECT(fs->file_text, "activate", file_ok_proc, (gpointer)fd);

  set_sensitive(fs->ok_button, (!(file_is_directory(fs))));
  SG_SIGNAL_CONNECT(fs->file_text, "key_release_event", reflect_text_in_open_button, (gpointer)fd);

  set_dialog_widget(which_dialog, fs->dialog);

  return(fd);
}


static void file_open_error(const char *error_msg, file_dialog_info *fd)
{
  info_widget_display(fd->info1, error_msg);
  info_widget_set_size(fd->info1, strlen(error_msg));
  gtk_widget_show(fd->frame);
  gtk_widget_show(fd->vbox);
  gtk_widget_show(fd->info1);
  info_widget_display(fd->info2, "");
}


static void redirect_file_open_error(const char *error_msg, void *ufd)
{
  /* called from snd_error, redirecting error handling to the dialog */
  file_open_error(error_msg, (file_dialog_info *)ufd);
}


static void clear_file_error_label(file_dialog_info *fd)
{
  info_widget_display(fd->info1, "");

  if (fd->unsound_directory_watcher)
    {
      fd->unsound_directory_watcher = unmonitor_directory(fd->unsound_directory_watcher);
      if (fd->unsound_dirname) {free(fd->unsound_dirname); fd->unsound_dirname = NULL;}
      if (fd->unsound_filename) {free(fd->unsound_filename); fd->unsound_filename = NULL;}
    }
}


/* key press event here, not key release -- the latter is triggered by the <return> release
 *   that triggered the error, so our error is immediately erased
 */
static gulong key_press_handler_id = 0;

static void clear_open_handlers(fsb *fs)
{
  if (key_press_handler_id)
    {
      g_signal_handler_disconnect(fs->file_text, key_press_handler_id);
      key_press_handler_id = 0;
    }
}


static gboolean open_modify_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;  
  clear_file_error_label(fd);
  clear_open_handlers(fd->fs);
  return(false);
}


static void clear_error_if_open_changes(fsb *fs, file_dialog_info *fd)
{
  if (!key_press_handler_id)
    key_press_handler_id = SG_SIGNAL_CONNECT(fs->file_text, "key_press_event", open_modify_key_press, (void *)fd);
}


#if HAVE_G_FILE_MONITOR_DIRECTORY 
static void unpost_unsound_error(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  file_dialog_info *fd;
  char *filename;
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
    case G_FILE_MONITOR_EVENT_CREATED:
      filename = g_file_get_path(file);
      fd = (file_dialog_info *)data;
      if ((fd) &&
	  (filename) &&
	  (mus_strcmp(filename, fd->unsound_filename)))
	clear_file_error_label(fd);
      break;

    default:
      /* ignore the rest */
      break;
    }
}


static void start_unsound_watcher(file_dialog_info *fd, const char *filename)
{
  GFile *file;
  GError *err = NULL;

  if (fd->unsound_directory_watcher)
    {
      fd->unsound_directory_watcher = unmonitor_directory(fd->unsound_directory_watcher);
      if (fd->unsound_dirname) free(fd->unsound_dirname);
      if (fd->unsound_filename) free(fd->unsound_filename);
    }

  fd->unsound_filename = mus_expand_filename(filename);
  fd->unsound_dirname = just_directory(fd->unsound_filename);

  file = g_file_new_for_path(fd->unsound_dirname);
  fd->unsound_directory_watcher = (void *)g_file_monitor_directory(file, G_FILE_MONITOR_NONE, NULL, &err);
  if (err != NULL)
    snd_warning("%s", err->message);
  else g_signal_connect(G_OBJECT(fd->unsound_directory_watcher), "changed", G_CALLBACK(unpost_unsound_error), (gpointer)fd);
  g_object_unref(file);
}
#else
static void start_unsound_watcher(file_dialog_info *fd, const char *filename) {}
#endif


static void file_open_dialog_ok(GtkWidget *w, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  char *filename = NULL;
  gpointer hide_me = 0;

  filename = fsb_file_text(fd->fs);
  if ((!filename) || (!(*filename)))
    {
      file_open_error("no filename given", fd);
      clear_error_if_open_changes(fd->fs, fd);
    }
  else
    {
      file_dialog_stop_playing(fd->dp);
      if (!(directory_p(filename)))
	{
	  snd_info *sp;
	  redirect_snd_error_to(redirect_file_open_error, (void *)fd);
	  ss->requestor_dialog = fd->fs->dialog;
	  ss->open_requestor = FROM_OPEN_DIALOG;
	  sp = snd_open_file(filename, fd->file_dialog_read_only);
	  redirect_snd_error_to(NULL, NULL);
	  if (sp) 
	    {
	      hide_me = g_object_get_data(G_OBJECT(fd->fs->dialog), "hide-me"); /* see snd-gtk.scm where this is set */
	      if (hide_me == 0)
		gtk_widget_hide(fd->fs->dialog);
	      select_channel(sp, 0); /* add_sound_window (snd-xsnd.c) -> make_file_info (snd-file) will report reason for error, if any */
	    }
	  else
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  clear_error_if_open_changes(fd->fs, fd);
		  start_unsound_watcher(fd, filename);
		}
	    }
	}
      else
	{
	  char *str;
	  str = mus_format("%s is a directory", filename);
	  file_open_error(str, fd);
	  clear_error_if_open_changes(fd->fs, fd);
	  free(str);
	}
    }
}


static void file_open_dialog_dismiss(GtkWidget *w, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  file_dialog_stop_playing(fd->dp);
  gtk_widget_hide(fd->fs->dialog);
}


static void file_open_dialog_help(GtkWidget *w, gpointer context)
{
  open_file_dialog_help();
}


static gint file_open_dialog_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  file_dialog_stop_playing(fd->dp);
  gtk_widget_hide(fd->fs->dialog);
  return(true);
}


static void file_open_double_click_callback(const char *filename, void *context)
{
  if ((filename) &&
      (!(directory_p(filename))))
    {
      file_dialog_info *fd;
      snd_info *sp;
      
      fd = odat;
      redirect_snd_error_to(redirect_file_open_error, (void *)fd);
      ss->requestor_dialog = fd->fs->dialog;
      ss->open_requestor = FROM_OPEN_DIALOG;
      sp = snd_open_file(filename, fd->file_dialog_read_only);
      redirect_snd_error_to(NULL, NULL);
      if (sp) 
	{
	  gpointer hide_me;
	  hide_me = g_object_get_data(G_OBJECT(fd->fs->dialog), "hide-me"); /* see snd-gtk.scm where this is set */
	  if (hide_me == 0)
	    gtk_widget_hide(fd->fs->dialog);
	  select_channel(sp, 0); /* add_sound_window (snd-xsnd.c) -> make_file_info (snd-file) will report reason for error, if any */
	}
      else
	{
	  clear_error_if_open_changes(fd->fs, fd);
	  start_unsound_watcher(fd, filename);
	}
    }
}


widget_t make_open_file_dialog(read_only_t read_only, bool managed)
{
  if (!odat)
    {
      odat = make_file_dialog(read_only, 
			      (char *)((read_only == FILE_READ_ONLY) ? "View" : "Open"), 
			      (char *)((read_only == FILE_READ_ONLY) ? "view:" : "open:"),
			      NULL,
			      FILE_OPEN_DIALOG,
			      (GCallback)file_open_dialog_ok,	
			      NULL, /* no mkdir */
			      (GCallback)file_open_dialog_delete,
			      (GCallback)file_open_dialog_dismiss,
			      (GCallback)file_open_dialog_help,
			      GTK_STOCK_OPEN);
      odat->fs->file_double_click_callback = file_open_double_click_callback;
#if 0
      preload_filenames(odat->fs->file_text_names);
#endif
    }
  else
    {
      if (read_only != odat->file_dialog_read_only)
	{
	  set_stock_button_label(odat->fs->ok_button, (char *)((read_only == FILE_READ_ONLY) ? "View" : "Open"));
	  gtk_window_set_title(GTK_WINDOW(odat->fs->dialog), (char *)((read_only == FILE_READ_ONLY) ? "View" : "Open"));
	  odat->file_dialog_read_only = read_only;
	}
      if (odat->fs->reread_directory) 
	{
	  force_directory_reread(odat->fs);
	  odat->fs->reread_directory = false;
	}
    }
  if (managed) 
    gtk_widget_show(odat->fs->dialog);

  return(odat->fs->dialog);
}


/* -------- mix file dialog -------- */

static void file_mix_cancel_callback(GtkWidget *w, gpointer context)
{
  file_dialog_stop_playing(mdat->dp);
  gtk_widget_hide(mdat->fs->dialog);
}


static void file_mix_help_callback(GtkWidget *w, gpointer context)
{
  mix_file_dialog_help();
}


static gint file_mix_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(mdat->dp);
  gtk_widget_hide(mdat->fs->dialog);
  return(true);
}


static void file_mix_ok_callback(GtkWidget *w, gpointer context)
{
  char *filename = NULL;
  filename = fsb_file_text(mdat->fs);
  if ((!filename) || (!(*filename)))
    {
      file_open_error("no filename given", mdat);
      clear_error_if_open_changes(mdat->fs, mdat);
    }
  else
    {
      file_dialog_stop_playing(mdat->dp);
      if (!(directory_p(filename)))
	{
	  snd_info *sp;
	  int err;
	  sp = any_selected_sound();
	  redirect_snd_error_to(redirect_file_open_error, (void *)mdat);
	  ss->requestor_dialog = mdat->fs->dialog;
	  ss->open_requestor = FROM_MIX_DIALOG;
	  err = mix_complete_file_at_cursor(sp, filename);
	  redirect_snd_error_to(NULL, NULL);
	  if (err < 0) 
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  if (err == MIX_FILE_NO_FILE)
		    start_unsound_watcher(mdat, filename);
		  clear_error_if_open_changes(mdat->fs, mdat);
		}
	    }
	  else 
	    {
	      status_report(sp, "%s mixed in at cursor", filename);
	    }
	}
      else 
	{
	  char *str;
	  str = mus_format("%s is a directory", filename);
	  file_open_error(str, mdat);
	  clear_error_if_open_changes(mdat->fs, mdat);
	  free(str);
	}
    }
}


widget_t make_mix_file_dialog(bool managed)
{
  if (mdat == NULL)
    {
      mdat = make_file_dialog(FILE_READ_ONLY, "Mix", "mix:", "Mix", FILE_MIX_DIALOG,
			      (GCallback)file_mix_ok_callback,
			      NULL, /* no mkdir */
			      (GCallback)file_mix_delete_callback,
			      (GCallback)file_mix_cancel_callback,
			      (GCallback)file_mix_help_callback,
			      GTK_STOCK_ADD);
    }
  else
    {
      if (mdat->fs->reread_directory) 
	{
	  force_directory_reread(mdat->fs);
	  mdat->fs->reread_directory = false;
	}
    }
  if (managed) gtk_widget_show(mdat->fs->dialog);
  return(mdat->fs->dialog);
}


/* -------- File:Insert dialog -------- */

static void file_insert_cancel_callback(GtkWidget *w, gpointer context)
{
  file_dialog_stop_playing(idat->dp);
  gtk_widget_hide(idat->fs->dialog);
}


static void file_insert_help_callback(GtkWidget *w, gpointer context)
{
  insert_file_dialog_help();
}


static gint file_insert_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(idat->dp);
  gtk_widget_hide(idat->fs->dialog);
  return(true);
}


static void file_insert_ok_callback(GtkWidget *w, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename;
  filename = fsb_file_text(fd->fs);
  if ((!filename) || (!(*filename)))
    {
      file_open_error("no filename given", fd);
      clear_error_if_open_changes(fd->fs, fd);
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
	  redirect_snd_error_to(redirect_file_open_error, (void *)fd);
	  ok = insert_complete_file_at_cursor(sp, filename);
	  redirect_snd_error_to(NULL, NULL);
	  if (!ok)
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  char *fullname;
		  clear_error_if_open_changes(fd->fs, fd);
		  fullname = mus_expand_filename(filename);
		  if (!(mus_file_probe(fullname)))
		    start_unsound_watcher(fd, filename);
		  free(fullname);
		}
	    }
	  else 
	    {
	      status_report(sp, "%s inserted at cursor", filename);
	    }
	}
      else 
	{
	  char *str;
	  str = mus_format("%s is a directory", filename);
	  file_open_error(str, fd);
	  clear_error_if_open_changes(fd->fs, fd);
	  free(str);
	}
    }
}

  
widget_t make_insert_file_dialog(bool managed)
{
  if (idat == NULL)
    idat = make_file_dialog(FILE_READ_ONLY, "Insert", "insert:", "Insert", FILE_INSERT_DIALOG,
			    (GCallback)file_insert_ok_callback,
			    NULL, /* no mkdir */
			    (GCallback)file_insert_delete_callback,
			    (GCallback)file_insert_cancel_callback,
			    (GCallback)file_insert_help_callback,
			    GTK_STOCK_PASTE);
  else
    {
      if (idat->fs->reread_directory) 
	{
	  force_directory_reread(idat->fs);
	  idat->fs->reread_directory = false;
	}
    }
  if (managed) gtk_widget_show(idat->fs->dialog);
  return(idat->fs->dialog);
}



void set_open_file_play_button(bool val) 
{
#if WITH_AUDIO
  if ((odat) && (odat->dp->play_button))
    set_toggle_button(odat->dp->play_button, val, false, (gpointer)odat);
  if ((mdat) && (mdat->dp->play_button))
    set_toggle_button(mdat->dp->play_button, val, false, (gpointer)mdat);
  if ((idat) && (idat->dp->play_button))
    set_toggle_button(idat->dp->play_button, val, false, (gpointer)mdat);
#endif
}


/* ---------------- file data panel ---------------- */

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, int *type, int *format, mus_long_t *location, mus_long_t *samples, int min_chan)
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
	(*srate) = string_to_int(str, 1, "srate"); 
      else snd_error_without_format("no srate?");
    }

  if ((chans) && (fdat->chans_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->chans_text)); 
      fdat->scanf_widget = CHANS_WIDGET;
      if ((str) && (*str))
	(*chans) = string_to_int(str, min_chan, "chans"); 
       else
 	{
 	  if (min_chan > 0)
 	    snd_error_without_format("no chans?");
 	}
    }

  if ((location) && (fdat->location_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->location_text)); 
      fdat->scanf_widget = DATA_LOCATION_WIDGET;
      if ((str) && (*str))
	(*location) = string_to_mus_long_t(str, 0, "data location"); 
      else snd_error_without_format("no data location?");
    }

  if ((samples) && (fdat->samples_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->samples_text)); 
      fdat->scanf_widget = SAMPLES_WIDGET;
      if ((str) && (*str))
	(*samples) = string_to_mus_long_t(str, 0, "samples"); 
      else snd_error_without_format("no samples?");
    }
  fdat->scanf_widget = SAMPLES_WIDGET;

  if (fdat->header_list)
    {
      res = fdat->header_list->selected_item;
      if (res == SLIST_NO_ITEM_SELECTED)
	res = fdat->header_pos;
      if (res != NO_SELECTION)
	{
	  (*type) = position_to_type(res);
	  fdat->current_type = (*type);
	}
    }

  if (fdat->format_list)
    {
      res = fdat->format_list->selected_item;
      if (res == SLIST_NO_ITEM_SELECTED) /* can this happen? */
	res = fdat->format_pos;
      if (res != NO_SELECTION)
	{
	  (*format) = position_to_format(fdat->current_type, res);
	  fdat->current_format = (*format);
	}
    }

  if (fdat->comment_text) 
    {
      if (GTK_IS_TEXT_VIEW(fdat->comment_text))
	comment = sg_get_text(fdat->comment_text, 0, -1);
      else comment = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->comment_text)); 
      str = mus_strdup(comment);
      return(str);
    }

  return(NULL);
}


#define IGNORE_DATA_LOCATION -1
#define IGNORE_SAMPLES -1
#define IGNORE_CHANS -1
#define IGNORE_SRATE -1
#define IGNORE_HEADER_TYPE -1

static void set_file_dialog_sound_attributes(file_data *fdat, int type, int format, int srate, int chans, mus_long_t location, mus_long_t samples, char *comment)
{
  int i;
  const char **fl = NULL;
  const char *str;
  if (!(fdat->format_list)) return;

  if (type != IGNORE_HEADER_TYPE)
    fdat->current_type = type;
  else fdat->current_type = MUS_RAW;
  fdat->current_format = format;
  fl = type_and_format_to_position(fdat, fdat->current_type, fdat->current_format);
  if (fl == NULL) return;

  if ((type != IGNORE_HEADER_TYPE) &&
      (fdat->header_list))
    {
      slist_select(fdat->header_list, fdat->header_pos);
      slist_moveto(fdat->header_list, fdat->header_pos);
    }

  slist_clear(fdat->format_list);
  for (i = 0; i < fdat->formats; i++) 
    {
      str = fl[i];
      slist_append(fdat->format_list, str);
    }
  slist_select(fdat->format_list, fdat->format_pos);
  slist_moveto(fdat->format_list, fdat->format_pos);

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
    widget_mus_long_t_to_text(fdat->location_text, location);

  if ((samples != IGNORE_SAMPLES) && 
      (fdat->samples_text))
    widget_mus_long_t_to_text(fdat->samples_text, samples);
}


static gboolean data_panel_srate_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  if (EVENT_KEYVAL(event) == snd_K_Tab)
    {
      gtk_entry_set_text(GTK_ENTRY(w), srate_completer(w, (char *)gtk_entry_get_text(GTK_ENTRY(w)), NULL));
      gtk_editable_set_position(GTK_EDITABLE(w), mus_strlen((char *)gtk_entry_get_text(GTK_ENTRY(w))));
      return(true);
    }
  return(false);
}


#define NUM_REFLECTION_IDS 5
enum {REFLECT_SRATE_ID, REFLECT_CHANS_ID, REFLECT_SAMPLES_ID, REFLECT_LOCATION_ID, REFLECT_COMMENT_ID};

static void reflect_file_data_panel_change(file_data *fd, void *data, void (*change_action)(GtkWidget *w, gpointer context))
{
  if (!(fd->reflection_ids))
    fd->reflection_ids = (gulong *)calloc(NUM_REFLECTION_IDS, sizeof(gulong));
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


static void post_file_dialog_error(const char *error_msg, file_data *fd)
{
  gtk_entry_set_text(GTK_ENTRY(fd->error_text), (gchar *)error_msg);
  show_dialog_error(fd);
}


static void redirect_post_file_dialog_error(const char *error_msg, void *ufd)
{
  post_file_dialog_error(error_msg, (file_data *)ufd);
}


/* key press event here, not key release -- the latter is triggered by the <return> release
 *   that triggered the error, so our error is immediately erased
 */
static gulong key_press_filename_handler_id = 0;

static void clear_filename_handlers(fsb *fs)
{
  if (key_press_filename_handler_id)
    {
      g_signal_handler_disconnect(fs->file_text, key_press_filename_handler_id);
      key_press_filename_handler_id = 0;
    }
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


static void clear_error_if_panel_changes(GtkWidget *dialog, file_data *fd)
{
  GtkWidget *baddy;
  switch (fd->error_widget)
    {
    case SRATE_WIDGET:         baddy = fd->srate_text;    break;
    case DATA_LOCATION_WIDGET: baddy = fd->location_text; break;
    case SAMPLES_WIDGET:       baddy = fd->samples_text;  break;
    default:                   baddy = fd->chans_text;    break;
    }
  if (baddy) 
    panel_modify_handler_id = SG_SIGNAL_CONNECT(baddy, "key_press_event", panel_modify_callback, (void *)fd);
}


static void post_file_panel_error(const char *error_msg, file_data *fd)
{
  fd->error_widget = fd->scanf_widget;
  post_file_dialog_error(error_msg, fd);
}


static void redirect_post_file_panel_error(const char *error_msg, void *ufd)
{
  post_file_panel_error(error_msg, (file_data *)ufd);
}


/* -------- file data choices -------- */

static void update_header_type_list(const char *name, int row, void *data)
{
  /* needed to reflect type selection in format list */
  file_data *fd = (file_data *)data;
  if (position_to_type(row) != fd->current_type)
    {
      position_to_type_and_format(fd, row);
      set_file_dialog_sound_attributes(fd,
				       fd->current_type,
				       fd->current_format,
				       IGNORE_SRATE, IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				       NULL);
    }
}


static void update_data_format_list(const char *name, int row, void *data)
{
  file_data *fd = (file_data *)data;
  fd->current_format = position_to_format(fd->current_type, row);
}


static void file_data_src_callback(GtkWidget *w, gpointer context)
{
  file_data *fd = (file_data *)context;
  fd->src = (bool)(TOGGLE_BUTTON_ACTIVE(w));
}

#define WITH_SRATE_FIELD true
#define WITHOUT_SRATE_FIELD false

#define WITH_AUTO_COMMENT true
#define WITHOUT_AUTO_COMMENT false


static file_data *make_file_data_panel(GtkWidget *parent, const char *name, 
				       dialog_channels_t with_chan, 
				       int header_type, int data_format,
				       dialog_data_location_t with_loc, 
				       dialog_samples_t with_samples,
				       dialog_header_type_t with_header_type,
				       dialog_comment_t with_comment, 
				       header_choice_t header_choice,
				       bool with_src, 
				       bool with_auto_comment)
{
  GtkWidget *form, *scbox, *combox = NULL, *frame_box;
  file_data *fdat;
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

  frame_box = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(parent), frame_box, true, true, 8);
  gtk_widget_show(frame_box);

  form = gtk_hbox_new(true, 8);
  gtk_box_pack_start(GTK_BOX(frame_box), form, false, false, 4);
  gtk_widget_show(form);

  /* header type */
  if (with_header_type == WITH_HEADER_TYPE_FIELD)
    {
      fdat->header_list = slist_new_with_title("header type", form, (const char **)headers, nheaders, BOX_PACK); /* BOX_PACK widget_add_t (snd-g0.h) */
      fdat->header_list->select_callback = update_header_type_list;
      fdat->header_list->select_callback_data = (void *)fdat;
      slist_select(fdat->header_list, fdat->header_pos);
    }

  /* data format */ 
#if HAVE_GTK_3
  fdat->format_list = slist_new_with_title("     data type     ", form, (const char **)formats, nformats, BOX_PACK);
#else
  fdat->format_list = slist_new_with_title("data type", form, (const char **)formats, nformats, BOX_PACK);
#endif
  fdat->format_list->select_callback = update_data_format_list;
  fdat->format_list->select_callback_data = (void *)fdat;
  slist_select(fdat->format_list, fdat->format_pos);

  scbox = gtk_vbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(form), scbox, false, false, 4);
  gtk_widget_show(scbox);

  /* srate */
  {
    GtkWidget *srate_label;
    srate_label = snd_gtk_highlight_label_new("srate");
    gtk_box_pack_start(GTK_BOX(scbox), srate_label, false, false, 0);
    gtk_widget_show(srate_label);

    if (with_src)
      {
	GtkWidget *src_box;
	src_box = gtk_hbox_new(false, 0);
	gtk_box_pack_start(GTK_BOX(scbox), src_box, false, false, 0);
	gtk_widget_show(src_box);
	
	fdat->srate_text = snd_entry_new(src_box, NULL, WITH_WHITE_BACKGROUND);
	gtk_entry_set_width_chars(GTK_ENTRY(fdat->srate_text), 8);
	SG_SIGNAL_CONNECT(fdat->srate_text, "key_press_event", data_panel_srate_key_press, NULL); /* srate completer */
	
	fdat->src_button = gtk_check_button_new_with_label("src");
	gtk_box_pack_end(GTK_BOX(src_box), fdat->src_button, false, false, 4);
	SG_SIGNAL_CONNECT(fdat->src_button, "toggled", file_data_src_callback, fdat);
	gtk_widget_show(fdat->src_button);
	set_toggle_button(fdat->src_button, fdat->src, false, (void *)fdat);
      }
    else
      {
	fdat->srate_text = snd_entry_new(scbox, NULL, WITH_WHITE_BACKGROUND);
	SG_SIGNAL_CONNECT(fdat->srate_text, "key_press_event", data_panel_srate_key_press, NULL); /* srate completer */
      }
  }

  /* chans */
  if (with_chan != WITHOUT_CHANNELS_FIELD)
    {
      GtkWidget *chans_label;
      chans_label = snd_gtk_highlight_label_new((with_chan == WITH_CHANNELS_FIELD) ? "channels" : "extract channel");
      gtk_box_pack_start(GTK_BOX(scbox), chans_label, false, false, 0);
      gtk_widget_show(chans_label);

      fdat->chans_text = snd_entry_new(scbox, NULL, WITH_WHITE_BACKGROUND);
      
      if (with_loc == WITH_DATA_LOCATION_FIELD)
	{
	  GtkWidget *loclab;
	  loclab = snd_gtk_highlight_label_new("location");
	  gtk_box_pack_start(GTK_BOX(scbox), loclab, false, false, 0);
	  gtk_widget_show(loclab);

	  fdat->location_text = snd_entry_new(scbox, NULL, WITH_WHITE_BACKGROUND);
	}
    }

  /* samples */
  if (with_samples == WITH_SAMPLES_FIELD)
    {
      GtkWidget *samplab;
      samplab = snd_gtk_highlight_label_new("samples");
      gtk_box_pack_start(GTK_BOX(scbox), samplab, false, false, 0);
      gtk_widget_show(samplab);

      fdat->samples_text = snd_entry_new(scbox, NULL, WITH_WHITE_BACKGROUND);
    }
  else
    {
      /* need a spacer to force the lists to have room */
      GtkWidget *spacer;
      spacer = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(scbox), spacer, false, false, 10);
      gtk_widget_show(spacer);
    }

  /* comment */
  if (with_comment != WITHOUT_COMMENT_FIELD)
    {
      GtkWidget *frame, *comment_label;
      GtkWidget *w1;

      w1 = gtk_vseparator_new();
      gtk_box_pack_start(GTK_BOX(frame_box), w1, false, false, 4);
      gtk_widget_show(w1);

      combox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(frame_box), combox, true, true, 4);
      gtk_widget_show(combox);

      if (with_auto_comment)
	{
	  GtkWidget *cbox;
	  cbox = gtk_vbox_new(false, 0);
	  gtk_box_pack_start(GTK_BOX(combox), cbox, false, false, 0);
	  gtk_widget_show(cbox);
	  
	  comment_label = snd_gtk_highlight_label_new("comment");
	  gtk_box_pack_start(GTK_BOX(cbox), comment_label, false, false, 0);
	  gtk_widget_show(comment_label);
	  
	  fdat->auto_comment_button = gtk_check_button_new_with_label("auto");
	  gtk_box_pack_end(GTK_BOX(cbox), fdat->auto_comment_button, false, false, 4);
	  gtk_widget_show(fdat->auto_comment_button);
	  set_toggle_button(fdat->auto_comment_button, fdat->auto_comment, false, (void *)fdat);
	}
      else
	{
	  comment_label = snd_gtk_highlight_label_new("comment");
	  gtk_box_pack_start(GTK_BOX(combox), comment_label, false, false, 0);
	  gtk_widget_show(comment_label);
	}

      frame = gtk_frame_new(NULL);
      gtk_box_pack_start(GTK_BOX(combox), frame, true, true, 4);  
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
      gtk_widget_show(frame);
      fdat->comment_text = make_scrolled_text(frame, true, CONTAINER_ADD, false); /* this returns a text_view widget */
      connect_mouse_to_text(fdat->comment_text);
    }

  /* error */
  fdat->error_text = make_info_widget();
  gtk_box_pack_end(GTK_BOX(parent), fdat->error_text, false, false, 0);
  gtk_widget_hide(fdat->error_text);

  return(fdat);
}


/* -------- save as dialog (file and edit menus) -------- */

typedef struct {
  fsb *fs;
  file_data *panel_data;
  char *filename;
  save_dialog_t type;
  dialog_play_info *dp;
  void *file_watcher;
  gulong filename_watcher_id;
  int header_type, format_type;
} save_as_dialog_info;

static save_as_dialog_info *save_sound_as = NULL, *save_selection_as = NULL, *save_region_as = NULL;

static gboolean filename_modify_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  clear_dialog_error(sd->panel_data);
  clear_filename_handlers(sd->fs);
  return(false);
}


void reflect_save_as_sound_selection(const char *sound_name)
{
  if (save_sound_as)
    {
      char *file_string;
      if (sound_name)
	file_string = mus_format("save %s", sound_name);
      else
	{
	  snd_info *sp;
	  sp = any_selected_sound();
	  if (sp)
	    file_string = mus_format("save %s", sp->short_filename);
	  else file_string = mus_strdup("nothing to save!");
	}
      gtk_window_set_title(GTK_WINDOW(save_sound_as->fs->dialog), file_string);
      free(file_string);
    }
}


void reflect_save_as_src(bool val)
{
  if (save_sound_as)
    {
      set_toggle_button(save_sound_as->panel_data->src_button, val, false, (void *)save_sound_as);
      save_sound_as->panel_data->src = val;
    }
  if (save_selection_as)
    {
      set_toggle_button(save_selection_as->panel_data->src_button, val, false, (void *)save_selection_as);
      save_selection_as->panel_data->src = val;
    }
  if (save_region_as)
    {
      set_toggle_button(save_region_as->panel_data->src_button, val, false, (void *)save_region_as);
      save_region_as->panel_data->src = val;
    }
}


void reflect_save_as_auto_comment(bool val)
{
  if (save_sound_as)
    {
      set_toggle_button(save_sound_as->panel_data->auto_comment_button, val, false, (void *)save_sound_as);
      save_sound_as->panel_data->auto_comment = val;
    }
}


static void make_auto_comment(save_as_dialog_info *sd)
{
  if ((sd == save_sound_as) &&
      (widget_is_active(sd->fs->dialog)))
    {
      file_data *fd;
      fd = sd->panel_data;

      if (!(fd->auto_comment))
	{
	  /* don't erase typed-in comment, if any */
	  sg_text_insert(fd->comment_text, fd->saved_comment);
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

	  fd->saved_comment = sg_get_text(fd->comment_text, 0, -1);
	  if ((fd->saved_comment) &&
	      (!(*(fd->saved_comment))))
	    fd->saved_comment = NULL;

	  for (i = 0; i < sp->nchans; i++)
	    if (sp->chans[i]->edit_ctr != 0)
	      {
		edits = true;
		break;
	      }

	  if (!edits)
	    comment = mus_format("%ssaved %s from %s (no edits)\n%s", 
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
	      mus_snprintf(comment, len, "%ssaved %s from %s with edits:\n", 
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

	  sg_text_insert(fd->comment_text, comment);
	  if (comment) free(comment);
	  if (orig_comment) free(orig_comment);
	}
    }
}


static void file_data_auto_comment_callback(GtkWidget *w, gpointer context)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  sd->panel_data->auto_comment = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  make_auto_comment(sd);
}


static void clear_error_if_filename_changes(fsb *fs, save_as_dialog_info *sd)
{
  key_press_filename_handler_id = SG_SIGNAL_CONNECT(fs->file_text, "key_press_event", filename_modify_key_press, (void *)sd);
}



static save_as_dialog_info *new_save_as_dialog_info(save_dialog_t type)
{
  save_as_dialog_info *sd;
  sd = (save_as_dialog_info *)calloc(1, sizeof(save_as_dialog_info));
  sd->type = type;
  sd->filename_watcher_id = 0;
  return(sd);
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
      (save_region_as->fs->dialog) &&
      (widget_is_active(save_region_as->fs->dialog)) &&
      (region_ok(region_dialog_region())))
    clear_dialog_error(save_region_as->panel_data);
}


static void save_as_undoit(save_as_dialog_info *sd)
{
  set_stock_button_label(sd->fs->ok_button, "Save");
  if ((sd->filename_watcher_id > 0) && (sd->fs->file_text))
    {
      g_signal_handler_disconnect(sd->fs->file_text, sd->filename_watcher_id);
      sd->filename_watcher_id = 0;
    }
  clear_dialog_error(sd->panel_data);
  sd->file_watcher = unmonitor_file(sd->file_watcher);
}


static void save_as_filename_modify_callback(GtkWidget *w, gpointer context)
{
  save_as_undoit((save_as_dialog_info *)context);
}


static void clear_error_if_save_as_filename_changes(GtkWidget *dialog, gpointer data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  if (sd->fs->file_text)
    sd->filename_watcher_id = SG_SIGNAL_CONNECT(sd->fs->file_text, "changed", save_as_filename_modify_callback, data);
}


#if HAVE_G_FILE_MONITOR_DIRECTORY
static void watch_save_as_file(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  /* if file is deleted, respond in some debonair manner */
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
    case G_FILE_MONITOR_EVENT_DELETED:
    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      save_as_undoit((save_as_dialog_info *)data);
      break;

    default:
      /* ignore the rest */
      break;
    }
}
#endif


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
  int type = MUS_NEXT, format = DEFAULT_OUTPUT_DATA_FORMAT, srate = DEFAULT_OUTPUT_SRATE, chans = DEFAULT_OUTPUT_CHANS;
  int output_type, chan = 0, extractable_chans = 0;
  bool file_exists = false;
  mus_long_t location = 28, samples = 0;
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
      clear_error_if_filename_changes(sd->fs, sd);
      return;
    }

  /* get output filename */
  str = fsb_file_text(sd->fs);
  if ((!str) || (!*str))
    {
      if (saving)
	msg = (char *)"can't save: no file name given";
      else msg = (char *)"can't extract: no file name given";
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->fs, sd);
      return;
    }

  /* get output file attributes */
  redirect_snd_error_to(redirect_post_file_panel_error, (void *)(sd->panel_data));
  if (saving)
    comment = get_file_dialog_sound_attributes(sd->panel_data, &srate, &chans, &type, &format, &location, &samples, 0);
  else comment = get_file_dialog_sound_attributes(sd->panel_data, &srate, &chan, &type, &format, &location, &samples, 0);
  output_type = type;
  redirect_snd_error_to(NULL, NULL);
  if (sd->panel_data->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(sd->fs->dialog, sd->panel_data);
      if (comment) free(comment);
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
	  clear_error_if_chans_changes(sd->fs->dialog, (void *)(sd->panel_data));
	  free(msg);
	  if (comment) free(comment);
	  return;
	}
    }

  fullname = mus_expand_filename(str);
  if (run_before_save_as_hook(sp, fullname, sd->type != SOUND_SAVE_AS, srate, type, format, comment))
    {
      msg = mus_format("%s cancelled by %s", (saving) ? "save" : "extract", S_before_save_as_hook);
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->fs, sd);      
      free(msg);
      free(fullname);
      if (comment) free(comment);
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
	  clear_error_if_filename_changes(sd->fs, sd); 
	  free(msg);
	  free(fullname);
	  if (comment) free(comment);
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
	      msg = mus_format("%s exists%s. To overwrite it, click '%s'", 
			       str,
			       (parlous_sp) ? ", and has unsaved edits" : "",
			       "DoIt"
			       );
#if HAVE_G_FILE_MONITOR_DIRECTORY
	      {
		GFile *file;
		GError *err = NULL;
		file = g_file_new_for_path(fullname);
		sd->file_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
		if (err != NULL)
		  snd_warning("%s", err->message);
		else g_signal_connect(G_OBJECT(sd->file_watcher), "changed", G_CALLBACK(watch_save_as_file), (gpointer)sd);
		g_object_unref(file);
	      }
#endif
	      post_file_dialog_error((const char *)msg, sd->panel_data);
	      clear_error_if_save_as_filename_changes(sd->fs->dialog, (void *)sd);
	      set_stock_button_label(sd->fs->ok_button, "DoIt");
	      free(msg);
	      free(fullname);
	      if (comment) free(comment);
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
	break;
      }

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
      gtk_widget_hide(sd->fs->dialog);
    }
  else
    {
      msg = mus_format("%s as %s: %s (%s)", (saving) ? "save" : "extract chan", str, io_error_name(io_err), snd_io_strerror());
      post_file_dialog_error((const char *)msg, sd->panel_data);
      clear_error_if_filename_changes(sd->fs, sd);
      free(msg);
    }
  free(fullname);
  if (comment) free(comment);
}


static void save_as_ok_callback(GtkWidget *w, gpointer data)
{ 
  save_or_extract((save_as_dialog_info *)data, true);
}


static void save_as_extract_callback(GtkWidget *w, gpointer data)
{
  save_or_extract((save_as_dialog_info *)data, false);
}


static void save_as_cancel_callback(GtkWidget *w, gpointer data)
{ 
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  gtk_widget_hide(sd->fs->dialog);
} 


static void save_as_help_callback(GtkWidget *w, gpointer data)
{
  save_as_dialog_help();
}


static void save_as_dialog_select_callback(const char *filename, void *data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  clear_filename_handlers(sd->fs);
  clear_dialog_error(sd->panel_data);
  set_sensitive(sd->fs->ok_button, (!(file_is_directory(sd->fs))));
  if (sd->fs->extract_button) set_sensitive(sd->fs->extract_button, (!(file_is_directory(sd->fs))));
}


static gint save_as_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  gtk_widget_hide(sd->fs->dialog);
  return(true);
}


static void save_as_mkdir_callback(GtkWidget *w, gpointer context)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  char *filename = NULL, *str;
  fsb *fs;
  fs = sd->fs;
  filename = fsb_file_text(fs);
  if (snd_mkdir(filename) < 0)
    {
      /* could not make the directory */
      str = mus_format("can't make %s: %s", filename, strerror(errno));
      post_file_dialog_error((const char *)str, sd->panel_data);
      clear_error_if_filename_changes(fs, sd);
      free(str);
    }
  else
    {
      if (fs->directory_name) free(fs->directory_name);
      fs->directory_name = mus_strdup(filename);
      fsb_filter_set_text_with_directory(fs, "*");
      fsb_update_lists(fs);
      set_sensitive(w, false);
      str = (char *)"save as:";
      gtk_label_set_text(GTK_LABEL(sd->fs->file_label), str);
    }
}


static void save_as_file_exists_check(GtkWidget *w, gpointer context)
{
  /* a "changed" callback for the text field */
  char *msg, *filename = NULL;
  save_as_dialog_info *sd = (save_as_dialog_info *)context;
  filename = fsb_file_text(sd->fs);
  if ((filename) && (*filename))
    {
      if ((mus_file_probe(filename)) && 
	  (!directory_p(filename)))
	{
#if HAVE_ACCESS
	  if (access(filename, W_OK) < 0)
	    msg = (char *)"save as (file write-protected?):";
	  else
#endif
	    msg = (char *)"save as (overwriting):";
	}
      else
	{
	  if (!(directory_exists(filename)))
	    msg = (char *)"save as (no such directory?):";
	  else msg = (char *)"save as:";
	}
    }
  else msg = (char *)"save as:";
  gtk_label_set_text(GTK_LABEL(sd->fs->file_label), msg);
}


static void save_innards(GtkWidget *vbox, void *data)
{
  save_as_dialog_info *sd = (save_as_dialog_info *)data;
  sd->panel_data = make_file_data_panel(vbox, "data-form", 
					(sd->type == REGION_SAVE_AS) ? WITHOUT_CHANNELS_FIELD : WITH_EXTRACT_CHANNELS_FIELD, 
					sd->header_type, sd->format_type, 
					WITHOUT_DATA_LOCATION_FIELD, 
					WITHOUT_SAMPLES_FIELD,
					WITH_HEADER_TYPE_FIELD, 
					WITH_COMMENT_FIELD,
					WITH_WRITABLE_HEADERS,
					WITH_SRATE_FIELD,
					sd->type == SOUND_SAVE_AS);
  widget_modify_base(sd->panel_data->error_text, GTK_STATE_NORMAL, ss->yellow);
  widget_modify_base(sd->panel_data->error_text, GTK_STATE_ACTIVE, ss->yellow);
}


static gboolean reflect_text_in_save_button(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  fsb *fs = (fsb *)data;
  set_sensitive(fs->ok_button, (!(file_is_directory(fs))));
  if (fs->mkdir_button) set_sensitive(fs->mkdir_button, file_is_nonexistent_directory(fs));
  return(false);
}


static gboolean reflect_text_in_extract_button(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  fsb *fs = (fsb *)data;
  set_sensitive(fs->extract_button, (!(file_is_directory(fs))));
  return(false);
}


static void make_save_as_dialog(save_as_dialog_info *sd, const char *sound_name, int header_type, int format_type)
{
  char *file_string;

  file_string = mus_format("save %s", sound_name);
  if (!(sd->fs))
    {
      fsb *fs;
      sd->header_type = header_type;
      sd->format_type = format_type;
      sd->fs = make_fsb(file_string, "save as:", "Save as", save_innards, (void *)sd, GTK_STOCK_SAVE_AS, (sd->type != REGION_SAVE_AS), true);
      fs = sd->fs;

      if (sd->type != REGION_SAVE_AS)
	SG_SIGNAL_CONNECT(fs->extract_button, "clicked", save_as_extract_callback, (void *)sd);
      SG_SIGNAL_CONNECT(fs->mkdir_button, "clicked", save_as_mkdir_callback, (void *)sd);

      SG_SIGNAL_CONNECT(fs->help_button, "clicked", save_as_help_callback, (gpointer)sd);
      SG_SIGNAL_CONNECT(fs->file_text, "key_press_event", filer_key_press, NULL);
      SG_SIGNAL_CONNECT(fs->ok_button, "clicked", save_as_ok_callback, (gpointer)sd);
      SG_SIGNAL_CONNECT(fs->cancel_button, "clicked", save_as_cancel_callback, (gpointer)sd);
      SG_SIGNAL_CONNECT(fs->file_text, "changed", save_as_file_exists_check, (gpointer)sd);

      fs->file_select_data = (void *)sd;
      fs->file_select_callback = save_as_dialog_select_callback;
      fs->directory_select_data = (void *)sd;
      fs->directory_select_callback = save_as_dialog_select_callback;

      sd->panel_data->dialog = fs->dialog;
      switch (sd->type)
	{
	case SOUND_SAVE_AS:
	  set_dialog_widget(SOUND_SAVE_AS_DIALOG, fs->dialog);
	  break;

	case SELECTION_SAVE_AS:
	  set_dialog_widget(SELECTION_SAVE_AS_DIALOG, fs->dialog);
	  break;

	case REGION_SAVE_AS:
	  set_dialog_widget(REGION_SAVE_AS_DIALOG, fs->dialog);
	  break;

	default:
	  snd_error("internal screw up");
	  break;
	}
      SG_SIGNAL_CONNECT(fs->dialog, "delete_event", save_as_delete_callback, (void *)sd);

      set_sensitive(fs->ok_button, (!(file_is_directory(fs))));
      SG_SIGNAL_CONNECT(fs->file_text, "key_release_event", reflect_text_in_save_button, (gpointer)fs);

      if (sd->type != REGION_SAVE_AS)
	{
	  set_sensitive(fs->extract_button, (!(file_is_directory(fs))));
	  SG_SIGNAL_CONNECT(fs->file_text, "key_release_event", reflect_text_in_extract_button, (gpointer)fs);
	  if (sd->type == SOUND_SAVE_AS)
	    SG_SIGNAL_CONNECT(sd->panel_data->auto_comment_button, "toggled", file_data_auto_comment_callback, sd);
	}
    }
  else
    {
      gtk_window_set_title(GTK_WINDOW(sd->fs->dialog), file_string);
    }

  /* gtk_label_set_text(GTK_LABEL(sd->fs->file_label), file_string); */
  free(file_string);
}


static save_as_dialog_info *make_sound_save_as_dialog_1(bool managed, int chan)
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
  if (com) free(com);
  if (chan >= 0)
    {
      char *chan_str;  
      chan_str = (char *)calloc(8, sizeof(char));
      snprintf(chan_str, 8, "%d", chan);
      gtk_entry_set_text(GTK_ENTRY(sd->panel_data->chans_text), chan_str);
      free(chan_str);
    }

  if (sd->fs->reread_directory) 
    {
      force_directory_reread(sd->fs);
      sd->fs->reread_directory = false;
    }
  if (managed) gtk_widget_show(sd->fs->dialog);
  make_auto_comment(sd);
  return(sd);
}


widget_t make_sound_save_as_dialog(bool managed)
{
  save_as_dialog_info *sd;
  sd = make_sound_save_as_dialog_1(managed, -1);
  return(sd->fs->dialog);
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
		      "current selection",
		      default_output_header_type(ss),
		      default_output_data_format(ss));
  set_file_dialog_sound_attributes(sd->panel_data,
				   sd->panel_data->current_type,
				   sd->panel_data->current_format,
				   selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   NULL);
  if (sd->fs->reread_directory) 
    {
      force_directory_reread(sd->fs);
      sd->fs->reread_directory = false;
    }
  if (managed) gtk_widget_show(sd->fs->dialog);
  return(sd->fs->dialog);
}


widget_t make_region_save_as_dialog(bool managed)
{
  save_as_dialog_info *sd;
  char *comment = NULL;

  if (!save_region_as)
    save_region_as = new_save_as_dialog_info(REGION_SAVE_AS);
  sd = save_region_as;

  make_save_as_dialog(sd,
		      "current region",
		      default_output_header_type(ss),
		      default_output_data_format(ss));
  comment = region_description(region_dialog_region());
  set_file_dialog_sound_attributes(sd->panel_data,
				   sd->panel_data->current_type,
				   sd->panel_data->current_format,
				   region_srate(region_dialog_region()), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   comment);
  if (comment) free(comment);
  if (sd->fs->reread_directory) 
    {
      force_directory_reread(sd->fs);
      sd->fs->reread_directory = false;
    }
  if (managed) gtk_widget_show(sd->fs->dialog);
  return(sd->fs->dialog);
}


void save_file_dialog_state(FILE *fd)
{
  if ((odat) && (widget_is_active(odat->fs->dialog)))
    {
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
  if ((mdat) && (widget_is_active(mdat->fs->dialog)))
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
  if ((idat) && (widget_is_active(idat->fs->dialog)))
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
  if ((save_sound_as) && (widget_is_active(save_sound_as->fs->dialog)))
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
  if ((save_selection_as) && (widget_is_active(save_selection_as->fs->dialog)))
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
  if ((save_region_as) && (widget_is_active(save_region_as->fs->dialog)))
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


/* -------------------------------- Raw Data Dialog -------------------------------- */

typedef struct raw_info {
  GtkWidget *dialog;
  mus_long_t location;
  file_data *rdat;
  read_only_t read_only;
  bool selected;
  char *filename;
  char *help;
  open_requestor_t requestor;
  void *requestor_data;
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
      raw_infos = (raw_info **)calloc(raw_info_size, sizeof(raw_info *));
    }
  else
    {
      int i;
      for (i = 0; i < raw_info_size; i++)
	if ((!raw_infos[i]) ||
	    (!(widget_is_active(raw_infos[i]->dialog))))
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


static void raw_data_ok_callback(GtkWidget *w, gpointer context)
{
  raw_info *rp = (raw_info *)context;
  int raw_srate, raw_chans, raw_data_format;
  redirect_snd_error_to(redirect_post_file_panel_error, (void *)(rp->rdat));
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
    gtk_window_set_title(GTK_WINDOW(rp->dialog), "No header on file");
  else gtk_window_set_title(GTK_WINDOW(rp->dialog), title);
  sg_make_resizable(rp->dialog);
  gtk_container_set_border_width(GTK_CONTAINER(rp->dialog), 10);
  gtk_window_resize(GTK_WINDOW(rp->dialog), 350, 260);
  gtk_widget_realize(rp->dialog);

  helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
  gtk_widget_set_name(helpB, "dialog_button");

  cancelB = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
  gtk_widget_set_name(cancelB, "dialog_button");
  set_stock_button_label(cancelB, I_GO_AWAY);

  resetB = sg_button_new_from_stock_with_label("Reset", GTK_STOCK_REFRESH);
  gtk_widget_set_name(resetB, "dialog_button");

  okB = gtk_button_new_from_stock(GTK_STOCK_OK);
  gtk_widget_set_name(okB, "dialog_button");

  gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(rp->dialog)), okB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(rp->dialog)), resetB, true, true, 10);
  gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(rp->dialog)), cancelB, true, true, 10);
  gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(rp->dialog)), helpB, true, true, 10);

#if HAVE_GTK_3
  add_highlight_button_style(cancelB);
  add_highlight_button_style(helpB);
  add_highlight_button_style(resetB);
  add_highlight_button_style(okB);
#endif

  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_data_format); /* pick up defaults */

  rp->rdat = make_file_data_panel(DIALOG_CONTENT_AREA(rp->dialog), "data-form", 
				  WITH_CHANNELS_FIELD, 
				  MUS_RAW, raw_data_format, 
				  WITH_DATA_LOCATION_FIELD, 
				  WITHOUT_SAMPLES_FIELD,
				  WITHOUT_HEADER_TYPE_FIELD, 
				  WITHOUT_COMMENT_FIELD,
				  WITH_READABLE_HEADERS,
				  WITHOUT_SRATE_FIELD, 
				  WITHOUT_AUTO_COMMENT);
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


void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, read_only_t read_only, bool selected)
{
  raw_info *rp;
  rp = new_raw_dialog();
  rp->read_only = read_only;
  rp->selected = selected;
  if (rp->filename) free(rp->filename);
  rp->filename = mus_strdup(filename);
  rp->requestor = ss->open_requestor;
  rp->requestor_dialog = ss->requestor_dialog;
  ss->open_requestor = NO_REQUESTOR;
  ss->requestor_dialog = NULL;
  if ((rp->requestor_dialog) &&
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    gtk_widget_hide(ss->requestor_dialog);
  if (!title) 
    title = mus_format("no header found on %s\n", filename);
  if (!rp->dialog) 
    make_raw_data_dialog(rp, filename, title);
  else gtk_window_set_title(GTK_WINDOW(rp->dialog), title);
  free(title);
  if (rp->help) free(rp->help);
  if (info)
    {
      rp->help = mus_strdup(info);
      free(info);
    }
  else rp->help = NULL;
  raise_dialog(rp->dialog);
  gtk_widget_show(rp->dialog);
}


/* -------------------------------- New File -------------------------------- */

static GtkWidget *new_file_dialog = NULL, *new_file_text = NULL, *new_file_ok_button = NULL;
static file_data *ndat = NULL;
static mus_long_t initial_samples = 1;
static char *new_file_filename = NULL;
static void *new_file_watcher = NULL;

static void cleanup_new_file_watcher(void)
{
  new_file_watcher = unmonitor_file(new_file_watcher);
}


static gulong new_file_handler_id = 0;
static gboolean new_filename_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer ignored);

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
  set_stock_button_label(new_file_ok_button, "Ok");
  new_file_watcher = unmonitor_file(new_file_watcher);
}


static gboolean new_filename_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer ignored)
{
  new_file_undoit();
  return(false);
}


static void clear_error_if_new_filename_changes(GtkWidget *dialog)
{
  if (new_file_text)
    new_file_handler_id = SG_SIGNAL_CONNECT(new_file_text, "key_press_event", new_filename_modify_callback, NULL);
}


#if HAVE_G_FILE_MONITOR_DIRECTORY
static void watch_new_file(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  /* if file is deleted, respond in some debonair manner */
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
    case G_FILE_MONITOR_EVENT_DELETED:
    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      new_file_undoit();
      break;

    default:
      /* ignore the rest */
      break;
    }
}
#endif


static void new_file_ok_callback(GtkWidget *w, gpointer context) 
{
  mus_long_t loc;
  char *comment = NULL, *newer_name = NULL, *msg;
  int header_type, data_format, srate, chans;
  newer_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_text));
  if ((!newer_name) || (!(*newer_name)))
    {
      msg = (char *)"new sound needs a file name ('New file:' field is empty)";
      post_file_dialog_error((const char *)msg, ndat);
      clear_error_if_new_filename_changes(new_file_dialog);
    }
  else
    {
      redirect_snd_error_to(redirect_post_file_panel_error, (void *)ndat);
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
	  new_file_filename = mus_expand_filename(newer_name); /* need full filename for fam */
	  if ((!new_file_watcher) &&
	      (ask_before_overwrite(ss)) && 
	      (mus_file_probe(new_file_filename)))
	    {
	      msg = mus_format("%s exists. If you want to overwrite it, click 'DoIt'", newer_name);
#if HAVE_G_FILE_MONITOR_DIRECTORY
	      {
		GFile *file;
		GError *err = NULL;
		file = g_file_new_for_path(new_file_filename);
		new_file_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
		if (err != NULL)
		  snd_warning("%s", err->message);
		else g_signal_connect(G_OBJECT(new_file_watcher), "changed", G_CALLBACK(watch_new_file), NULL);
		g_object_unref(file);
	      }
#endif
	      set_stock_button_label(new_file_ok_button, "DoIt");
	      post_file_dialog_error((const char *)msg, ndat);
	      clear_error_if_new_filename_changes(new_file_dialog);
	      free(msg);
	    }
	  else
	    {
	      if (new_file_watcher)
		new_file_undoit();

	      ss->local_errno = 0;
	      redirect_snd_error_to(redirect_post_file_dialog_error, (void *)ndat);
	      sp = snd_new_file(newer_name, header_type, data_format, srate, chans, comment, initial_samples);
	      redirect_snd_error_to(NULL, NULL);
	      if (!sp)
		{
#if HAVE_G_FILE_MONITOR_DIRECTORY
		  if ((ss->local_errno) &&
		      (mus_file_probe(new_file_filename))) /* see comment in snd-xfile.c */
		    {
		      GFile *file;
		      GError *err = NULL;
		      file = g_file_new_for_path(new_file_filename);
		      new_file_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
		      if (err != NULL)
			snd_warning("%s", err->message);
		      else g_signal_connect(G_OBJECT(new_file_watcher), "changed", G_CALLBACK(watch_new_file), NULL);
		      g_object_unref(file);
		    }
#endif
		  clear_error_if_new_filename_changes(new_file_dialog);
		}
	      else
		{
		  gtk_widget_hide(new_file_dialog);
		}
	    }
	}
      if (comment) free(comment);
    }
}


static char *new_file_dialog_filename(int header_type)
{
  static int new_file_dialog_file_ctr = 1;
  char *filename = NULL;
  const char *extensions[6] = {"aiff", "aiff", "wav", "wav", "caf", "snd"};
  int extension = 0;

  filename = (char *)calloc(64, sizeof(char));
  switch (header_type)
    {
    case MUS_AIFC: extension = 0; break;
    case MUS_AIFF: extension = 1; break;
    case MUS_RF64: extension = 2;  break;
    case MUS_RIFF: extension = 3;  break;
    case MUS_CAFF: extension = 4;  break;
    default:       extension = 5;  break;
    }
  mus_snprintf(filename, 64, "new-%d.%s", new_file_dialog_file_ctr++, extensions[extension]);

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
  gtk_entry_set_text(GTK_ENTRY(new_file_text), filename);  
  mus_sound_forget(filename);

  set_file_dialog_sound_attributes(ndat, header_type, data_format, srate, chans, IGNORE_DATA_LOCATION, initial_samples, new_comment);

  if (new_comment) free(new_comment);
  if (filename) free(filename);
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


widget_t make_new_file_dialog(bool managed)
{
  char *newname;
  if (!new_file_dialog)
    {
      GtkWidget *name_label, *hform, *help_button, *cancel_button, *reset_button;
      new_file_dialog = snd_gtk_dialog_new();
      gtk_window_set_title(GTK_WINDOW(new_file_dialog), "New file");
      sg_make_resizable(new_file_dialog);
      gtk_container_set_border_width (GTK_CONTAINER(new_file_dialog), 10);
      gtk_window_resize(GTK_WINDOW(new_file_dialog), 400, 250);
      gtk_widget_realize(new_file_dialog);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "dialog_button");

      cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
      gtk_widget_set_name(cancel_button, "dialog_button");
      set_stock_button_label(cancel_button, I_GO_AWAY);

      new_file_ok_button = sg_button_new_from_stock_with_label("Ok", GTK_STOCK_NEW);
      gtk_widget_set_name(new_file_ok_button, "dialog_button");

      reset_button = sg_button_new_from_stock_with_label("Reset", GTK_STOCK_REFRESH);
      gtk_widget_set_name(reset_button, "dialog_button");

      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(new_file_dialog)), new_file_ok_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(new_file_dialog)), reset_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(new_file_dialog)), cancel_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(new_file_dialog)), help_button, true, true, 10);

#if HAVE_GTK_3
      add_highlight_button_style(help_button);
      add_highlight_button_style(cancel_button);
      add_highlight_button_style(reset_button);
      add_highlight_button_style(new_file_ok_button);
#endif

      hform = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(new_file_dialog)), hform, false, false, 4);
      gtk_widget_show(hform);

      name_label = gtk_label_new("New file:");
      gtk_box_pack_start(GTK_BOX(hform), name_label, false, false, 2);
      gtk_widget_show(name_label);

      new_file_text = snd_entry_new(hform, NULL, WITH_WHITE_BACKGROUND);

      newname = output_name(NULL); /* fix later */
      if ((newname) && (*newname))
	gtk_entry_set_text(GTK_ENTRY(new_file_text), newname); /* output_name?? fix later */

      ndat = make_file_data_panel(DIALOG_CONTENT_AREA(new_file_dialog), "data-form", 
				  WITH_CHANNELS_FIELD, 
				  default_output_header_type(ss), 
				  default_output_data_format(ss), 
				  WITHOUT_DATA_LOCATION_FIELD, 
				  WITH_SAMPLES_FIELD,
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD,
				  WITH_BUILTIN_HEADERS,
				  WITHOUT_SRATE_FIELD, 
				  WITHOUT_AUTO_COMMENT);
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
  else
    {
      char *new_name;
      new_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_text));

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
	      gtk_entry_set_text(GTK_ENTRY(new_file_text), filename);  
	      mus_sound_forget(filename);
	      free(filename);
	    }
	}
    }
  if (managed)
    gtk_widget_show(new_file_dialog);
  return(new_file_dialog);
}



/* ---------------- Edit Header ---------------- */

typedef struct edhead_info {
  GtkWidget *dialog, *save_button;
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
	    (!(widget_is_active(edhead_infos[i]->dialog))))
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


static void cleanup_edit_header_watcher(void)
{
  int i;
  for (i = 0; i < edhead_info_size; i++)
    if (edhead_infos[i])
      {
	edhead_info *ep;
	ep = edhead_infos[i];
	if (ep->file_ro_watcher)
	  ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
      }
}


static char *make_header_dialog_title(edhead_info *ep, snd_info *sp)
{
  /* dialog may not yet exist */
  char *str;
  str = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, "Add header to (write-protected) %s", sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, "Edit header of (write-protected) %s", sp->short_filename);
      if (ep->dialog)
	set_sensitive(ep->save_button, (sp->hdr->type == MUS_RAW));
    }
  else 
    {
      if (sp->hdr->type == MUS_RAW)
	mus_snprintf(str, PRINT_BUFFER_SIZE, "Add header to %s", sp->short_filename);
      else mus_snprintf(str, PRINT_BUFFER_SIZE, "Edit header of %s", sp->short_filename);
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
  if (ep->sp->file_read_only == FILE_READ_WRITE)
    gtk_widget_set_sensitive(ep->save_button, true);
  ep->panel_changed = true;
}


static void edit_header_done(edhead_info *ep)
{
  gtk_widget_hide(ep->dialog);
  unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
  ep->panel_changed = false;
  ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
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


#if HAVE_G_FILE_MONITOR_DIRECTORY
static void watch_file_read_only(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  /* if file is deleted or permissions change, respond in some debonair manner */
  edhead_info *ep = (edhead_info *)data;
  snd_info *sp = NULL;
  sp = ep->sp;
  if (sp->writing) return;

  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
#if HAVE_ACCESS
      {
	int err;
	char *title;
	if (mus_file_probe(sp->filename))
	  {
	    err = access(sp->filename, W_OK);
	    sp->file_read_only = ((err < 0) ? FILE_READ_ONLY : FILE_READ_WRITE);
	    if ((sp->file_read_only == FILE_READ_WRITE) && 
		(sp->user_read_only == FILE_READ_WRITE))
	      clear_dialog_error(ep->edat);
	    title = make_header_dialog_title(ep, sp);
	    gtk_window_set_title(GTK_WINDOW(ep->dialog), title);
	    free(title);
	    return;
	  }
      }
#endif

      /* else fall through */
    case G_FILE_MONITOR_EVENT_DELETED:
    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      /* I don't think it makes sense to continue the dialog at this point */
      clear_dialog_error(ep->edat);
      gtk_widget_hide(ep->dialog);
      if (ep->panel_changed)
	unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
      ep->panel_changed = false;
      ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
      break;

    default:
      /* ignore the rest */
      break;
    }
}
#endif


static void edit_header_ok_callback(GtkWidget *w, gpointer context) 
{
  edhead_info *ep = (edhead_info *)context;
  if ((ep->sp) && (ep->sp->active))
    {
      bool ok;
      redirect_snd_error_to(redirect_post_file_dialog_error, (void *)(ep->edat));
      ok = edit_header_callback(ep->sp, ep->edat, redirect_post_file_dialog_error, redirect_post_file_panel_error);
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
	      set_sensitive(ep->save_button, false);
	      return;
	    }
	}
      ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
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

  if (!ep->dialog)
    {
      GtkWidget *help_button, *cancel_button;
      ep->dialog = snd_gtk_dialog_new();
      /* gtk_window_set_title(GTK_WINDOW(ep->dialog), "Edit Header"); */
      sg_make_resizable(ep->dialog);
      gtk_container_set_border_width (GTK_CONTAINER(ep->dialog), 10);
      gtk_window_resize(GTK_WINDOW(ep->dialog), 400, 250);
      gtk_widget_realize(ep->dialog);

      help_button = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(help_button, "dialog_button");

      cancel_button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
      gtk_widget_set_name(cancel_button, "dialog_button");
      set_stock_button_label(cancel_button, I_GO_AWAY);

      ep->save_button = gtk_button_new_from_stock(GTK_STOCK_SAVE);
      gtk_widget_set_name(ep->save_button, "dialog_button");

      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(ep->dialog)), ep->save_button, true, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(ep->dialog)), cancel_button, true, true, 10);
      gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(ep->dialog)), help_button, true, true, 10);

#if HAVE_GTK_3
      add_highlight_button_style(help_button);
      add_highlight_button_style(cancel_button);
      add_highlight_button_style(ep->save_button);
#endif

      ep->edat = make_file_data_panel(DIALOG_CONTENT_AREA(ep->dialog), "Edit Header", 
				      WITH_CHANNELS_FIELD, 
				      hdr->type, 
				      hdr->format, 
				      WITH_DATA_LOCATION_FIELD, 
				      WITH_SAMPLES_FIELD,
				      WITH_HEADER_TYPE_FIELD, 
				      WITH_COMMENT_FIELD,
				      WITH_BUILTIN_HEADERS,
				      WITHOUT_SRATE_FIELD, 
				      WITHOUT_AUTO_COMMENT);
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
  free(str);

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
#if HAVE_G_FILE_MONITOR_DIRECTORY
  {
    GFile *file;
    GError *err = NULL;
    file = g_file_new_for_path(ep->sp->filename);
    ep->file_ro_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
    if (err != NULL)
      snd_warning("%s", err->message);
    else g_signal_connect(G_OBJECT(ep->file_ro_watcher), "changed", G_CALLBACK(watch_file_read_only), (gpointer)ep);
    g_object_unref(file);
  }
#endif
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
	    (widget_is_active(ep->dialog)) && 
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
	  }
      }
}



/* ---------------- Post-it Monolog ---------------- */

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

  gtk_window_set_title(GTK_WINDOW(post_it_dialog), "Info");
  sg_make_resizable(post_it_dialog);
  gtk_container_set_border_width(GTK_CONTAINER(post_it_dialog), 10);
  gtk_window_resize(GTK_WINDOW(post_it_dialog), POST_IT_COLUMNS * 9, POST_IT_ROWS * 20);
  gtk_widget_realize(post_it_dialog);

  ok_button = gtk_button_new_from_stock(GTK_STOCK_OK);
  gtk_widget_set_name(ok_button, "dialog_button");

  gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(post_it_dialog)), ok_button, false, true, 20);
  SG_SIGNAL_CONNECT(ok_button, "clicked", dismiss_post_it, NULL);
  gtk_widget_show(ok_button);

#if HAVE_GTK_3
  add_highlight_button_style(ok_button);
#endif

  post_it_text = make_scrolled_text(DIALOG_CONTENT_AREA(post_it_dialog), false, BOX_PACK, true);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(post_it_text), 10);
  gtk_widget_show(post_it_dialog);
  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}


widget_t post_it(const char *subject, const char *str)
{
  if (ss == NULL) return(NULL);
  if (!(post_it_dialog)) create_post_it_monolog(); else raise_dialog(post_it_dialog);
  gtk_window_set_title(GTK_WINDOW(post_it_dialog), subject);
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(post_it_text)), "", 0);
  sg_text_insert(post_it_text, (char *)str);
  return(post_it_dialog);
}


void post_it_append(const char *str)
{
  if (post_it_text)
    sg_text_insert(post_it_text, (char *)str);
}


void save_post_it_dialog_state(FILE *fd)
{
  if ((post_it_dialog) && (widget_is_active(post_it_dialog)))
    {
      char *subject;
      gchar *text;
      subject = dialog_get_title(post_it_dialog);
      text = sg_get_text(post_it_text, 0, -1);
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\" \"%s\")\n", S_info_dialog, subject, text);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\", \"%s\")\n", TO_PROC_NAME(S_info_dialog), subject, text);
#endif
#if HAVE_FORTH
      fprintf(fd, "\"%s\" \"%s\" %s drop\n", subject, text, S_info_dialog);
#endif
      if (text) g_free(text);
      if (subject) free(subject);
    }
}



/* ---------------- unsaved edits dialog ---------------- */

static int num_unsaved_edits_dialogs = 0;
static GtkWidget **unsaved_edits_dialogs = NULL;
static snd_info **unsaved_edits_sounds = NULL;

static GtkWidget *unsaved_edits_dialog(snd_info *sp)
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
	(!widget_is_active(unsaved_edits_dialogs[i])))
      return(unsaved_edits_dialogs[i]);

  return(NULL);
}

static void save_unsaved_edits_dialog(GtkWidget *d, snd_info *sp)
{
  if (num_unsaved_edits_dialogs == 0)
    {
      unsaved_edits_dialogs = (GtkWidget **)calloc(1, sizeof(GtkWidget *));
      unsaved_edits_sounds = (snd_info **)calloc(1, sizeof(snd_info *));
    }
  else
    {
      unsaved_edits_dialogs = (GtkWidget **)realloc(unsaved_edits_dialogs, (num_unsaved_edits_dialogs + 1) * sizeof(GtkWidget *));
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
	(widget_is_active(unsaved_edits_dialogs[i])))
      gtk_widget_hide(unsaved_edits_dialogs[i]);
}


static void zero_edits(chan_info *cp)
{
  cp->edit_ctr = 0;
}


static void unsaved_edits_activate(GtkDialog *w, gint id, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  if (id == GTK_RESPONSE_NO)
    for_each_sound_chan(sp, zero_edits);
  else save_edits(sp);
  snd_close_file(sp);
  gtk_widget_hide(GTK_WIDGET(w));
}


void save_edits_now(snd_info *sp)
{
  char *question;
  GtkWidget *dialog;

  question = mus_format("%s has unsaved edits.  Save them?", sp->short_filename);
  dialog = unsaved_edits_dialog(sp);
  if (!dialog)
    {
      dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "%s", question);
      SG_SIGNAL_CONNECT(dialog, "response", unsaved_edits_activate, (gpointer)sp);
      save_unsaved_edits_dialog(dialog, sp);
    }
  else
    {
      g_object_set(dialog, "text", question, NULL);
    }

  free(question);
  gtk_widget_show(dialog);
}



/* ---------------- file has changed dialog ---------------- */

static int num_file_has_changed_dialogs = 0;
static GtkWidget **file_has_changed_dialogs = NULL;
static snd_info **file_has_changed_sounds = NULL;

static GtkWidget *file_has_changed_dialog(snd_info *sp)
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
	(!widget_is_active(file_has_changed_dialogs[i])))
      return(file_has_changed_dialogs[i]);

  return(NULL);
}

static void save_file_has_changed_dialog(GtkWidget *d, snd_info *sp)
{
  if (num_file_has_changed_dialogs == 0)
    {
      file_has_changed_dialogs = (GtkWidget **)calloc(1, sizeof(GtkWidget *));
      file_has_changed_sounds = (snd_info **)calloc(1, sizeof(snd_info *));
    }
  else
    {
      file_has_changed_dialogs = (GtkWidget **)realloc(file_has_changed_dialogs, (num_file_has_changed_dialogs + 1) * sizeof(GtkWidget *));
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
	(widget_is_active(file_has_changed_dialogs[i])))
      gtk_widget_hide(file_has_changed_dialogs[i]);
}


static void file_has_changed_activate(GtkDialog *w, gint id, gpointer context)
{
  if (id == GTK_RESPONSE_YES)
    {
      snd_info *sp = (snd_info *)context;
      save_edits_without_asking(sp);
      sp->need_update = false;
      stop_bomb(sp);                  /* in case Snd already noticed the problem */
      clear_status_area(sp);
    }
  gtk_widget_hide(GTK_WIDGET(w));
}


void changed_file_dialog(snd_info *sp)
{
  char *question;
  GtkWidget *dialog;

  question = mus_format("%s has changed!  Save edits anyway?", sp->short_filename);
  dialog = file_has_changed_dialog(sp);
  if (!dialog)
    {
      dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "%s", question);
      SG_SIGNAL_CONNECT(dialog, "response", file_has_changed_activate, (gpointer)sp);
      save_file_has_changed_dialog(dialog, sp);
    }
  else
    {
      g_object_set(dialog, "text", question, NULL);
    }

  free(question);
  gtk_widget_show(dialog);
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
      if (type == FILE_VIEWER)
	label = vf_row_get_label(r);
      else label = regrow_get_label(r);
      if (label)
	run_hook(hook,
		 XEN_LIST_3(C_TO_XEN_INT(type),
			    C_TO_XEN_INT((type == FILE_VIEWER) ? (vf_row_get_pos(r)) : (regrow_get_pos(r))),
			    C_TO_XEN_STRING(label)),
		 caller);
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


static gboolean vf_mouse_enter_label(GtkWidget *w, GdkEventCrossing *ev, gpointer gp)
{
  mouse_enter_label((void *)gp, FILE_VIEWER);
  return(false);
}


static gboolean vf_mouse_leave_label(GtkWidget *w, GdkEventCrossing *ev, gpointer gp)
{
  mouse_leave_label((void *)gp, FILE_VIEWER);
  return(false);
}


static int vf_last_select_state = 0;
static oclock_t mouse_down_time = 0, ev_mouse_down_time = 0;

static gboolean select_event_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  vf_last_select_state = EVENT_STATE(ev);
  ev_mouse_down_time = EVENT_TIME(ev);
  return(false);
}


static vf_row *make_vf_row(view_files_info *vdat, GCallback play_callback, GCallback name_callback)
{
  vf_row *r;
  r = (vf_row *)calloc(1, sizeof(vf_row));
  r->vdat = (void *)vdat;

  r->rw = gtk_hbox_new(false, 0);
  gtk_box_pack_start(GTK_BOX(vdat->file_list), r->rw, false, false, 0);
  gtk_widget_show(r->rw);

#if WITH_AUDIO
  r->pl = gtk_check_button_new();
  gtk_box_pack_start(GTK_BOX(r->rw), r->pl, false, false, 2);
  widget_modify_bg(r->rw, GTK_STATE_NORMAL, ss->white);
  widget_modify_base(r->rw, GTK_STATE_NORMAL, ss->white);
  SG_SIGNAL_CONNECT(r->pl, "toggled", play_callback, r);
  gtk_widget_show(r->pl);
#endif

  r->nm = gtk_button_new_with_label("");
  widget_modify_bg(r->nm, GTK_STATE_NORMAL, ss->white);
  widget_modify_base(r->nm, GTK_STATE_NORMAL, ss->white);
  sg_left_justify_button(r->nm);
  gtk_box_pack_start(GTK_BOX(r->rw), r->nm, true, true, 2);
  add_white_button_style(r->nm);

  SG_SIGNAL_CONNECT(r->nm, "clicked", name_callback, r);
  SG_SIGNAL_CONNECT(r->nm, "enter_notify_event", vf_mouse_enter_label, r);
  SG_SIGNAL_CONNECT(r->nm, "leave_notify_event", vf_mouse_leave_label, r);
  SG_SIGNAL_CONNECT(r->nm, "button_press_event", select_event_callback, (gpointer)vdat);

  set_user_data(G_OBJECT(r->nm), (gpointer)r);
  gtk_widget_show(r->nm);

  vf_unhighlight_row(r->nm, r->rw);
  return(r);
}


void vf_unhighlight_row(GtkWidget *nm, GtkWidget *rw)
{
  widget_modify_bg(nm, GTK_STATE_NORMAL, ss->white);
  widget_modify_base(nm, GTK_STATE_NORMAL, ss->white);
  widget_modify_bg(rw, GTK_STATE_NORMAL, ss->white);
  widget_modify_base(rw, GTK_STATE_NORMAL, ss->white);
}


void vf_highlight_row(GtkWidget *nm, GtkWidget *rw)
{
  widget_modify_bg(nm, GTK_STATE_NORMAL, ss->light_blue);
  widget_modify_base(nm, GTK_STATE_NORMAL, ss->light_blue);
  widget_modify_bg(rw, GTK_STATE_NORMAL, ss->light_blue);
  widget_modify_base(rw, GTK_STATE_NORMAL, ss->light_blue);
}


static gint vf_unflash_row(gpointer data)
{
  vf_row *r = (vf_row *)data;
  view_files_info *vdat;
  int i;
  vdat = (view_files_info *)(r->vdat);
  for (i = 0; i < vdat->currently_selected_files; i++)
    if (vdat->selected_files[i] == r->pos)
      {
	vf_highlight_row(r->nm, r->rw);
	return(0);
      }
  vf_unhighlight_row(r->nm, r->rw);
  return(0);
}


void vf_flash_row(vf_row *r)
{
  widget_modify_bg(r->nm, GTK_STATE_NORMAL, ss->light_blue);
  widget_modify_base(r->nm, GTK_STATE_NORMAL, ss->light_blue);
  g_timeout_add_full(0, (guint32)500, vf_unflash_row, (gpointer)r, NULL);
}


void vf_post_info(view_files_info *vdat, int pos)
{
  char *title;
  title = mus_format("%s:", vdat->names[pos]);
  info_widget_display(vdat->left_title, title);
  free(title);
  post_sound_info(vdat->info1, vdat->info2, vdat->full_names[pos], false);
}


void vf_post_selected_files_list(view_files_info *vdat)
{
  int len;
  char *msg1 = NULL, *msg2 = NULL, *title;
  len = vdat->currently_selected_files;

  title = mus_strdup("selected files:");
  info_widget_display(vdat->left_title, title);
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

  info_widget_display(vdat->info1, msg1);
  info_widget_display(vdat->info2, msg2);

  free(msg1);
  free(msg2);

  set_sensitive(vdat->mixB, true);
  set_sensitive(vdat->insertB, true);
}


void vf_unpost_info(view_files_info *vdat)
{
  char *title;

  title = mus_strdup("(no files selected)");
  info_widget_display(vdat->left_title, title);
  free(title);

  info_widget_display(vdat->info1, "");
  info_widget_display(vdat->info2, "");

  set_sensitive(vdat->mixB, false);
  set_sensitive(vdat->insertB, false);
}


static void view_files_select_callback(GtkWidget *w, gpointer context) 
{
  if (mouse_down_time != 0)
    {
      if ((ev_mouse_down_time - mouse_down_time) < 200)
	{
	  mouse_down_time = ev_mouse_down_time;
	  view_files_open_selected_files((view_files_info *)(((vf_row *)context)->vdat));
	  return;
	}
    }
  mouse_down_time = ev_mouse_down_time;
  view_files_select((vf_row *)context, vf_last_select_state & snd_ShiftMask);
}


static void view_files_play_callback(GtkWidget *w, gpointer context) 
{
#if WITH_AUDIO
  /* open and play -- close at end or when button off toggled */
  vf_row *r = (vf_row *)context;
  view_files_info *vdat;
  vdat = (view_files_info *)(r->vdat);
  if (view_files_play(vdat, r->pos, TOGGLE_BUTTON_ACTIVE(w)))
    set_toggle_button(w, false, false, (void *)vdat);
  else vdat->current_play_button = w;
#endif
}


vf_row *view_files_make_row(view_files_info *vdat, widget_t ignored)
{
  return(make_vf_row(vdat, (GCallback)view_files_play_callback, (GCallback)view_files_select_callback));
}


static void view_files_help_callback(GtkWidget *w, gpointer context) 
{
  view_files_dialog_help();
}


static gint view_files_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  view_files_info *vdat = (view_files_info *)context;
  gtk_widget_hide(vdat->dialog);
  return(true);
}


static void view_files_dismiss_callback(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat = (view_files_info *)context;
  gtk_widget_hide(vdat->dialog);
}


static void view_files_new_viewer_callback(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat;
  vdat = new_view_files_dialog();
  make_view_files_dialog_1(vdat, true);
}


static void sort_vf(view_files_info *vdat, int sort_choice)
{
  vdat->sorter = sort_choice;
  vf_reflect_sort_choice_in_menu(vdat);
  view_files_display_list(vdat);
}


static void sort_view_files_a_to_z(GtkWidget *w, gpointer context)
{
  sort_vf((view_files_info *)context, SORT_A_TO_Z);
}

static void sort_view_files_z_to_a(GtkWidget *w, gpointer context)
{
  sort_vf((view_files_info *)context, SORT_Z_TO_A);
}

static void sort_view_files_new_to_old(GtkWidget *w, gpointer context)
{
  sort_vf((view_files_info *)context, SORT_NEW_TO_OLD);
}

static void sort_view_files_old_to_new(GtkWidget *w, gpointer context)
{
  sort_vf((view_files_info *)context, SORT_OLD_TO_NEW);
}

static void sort_view_files_big_to_small(GtkWidget *w, gpointer context)
{
  sort_vf((view_files_info *)context, SORT_BIG_TO_SMALL);
}

static void sort_view_files_small_to_big(GtkWidget *w, gpointer context)
{
  sort_vf((view_files_info *)context, SORT_SMALL_TO_BIG);
}

static void sort_view_files_xen(GtkWidget *w, gpointer context)
{
  long index;
  index = (long)get_user_data(G_OBJECT(w));
  sort_vf((view_files_info *)context, (int)index);
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
    if (widget_is_active(vdat->sort_items[i]))
      set_sensitive(vdat->sort_items[i], vdat->sorter != (SORT_XEN + i));
}


void view_files_add_file_or_directory(view_files_info *vdat, const char *file_or_dir)
{
  char *filename;
  filename = mus_expand_filename((const char *)file_or_dir);
  if ((filename) && (filename[strlen(filename) - 1] == '*'))
    filename[strlen(filename) - 1] = 0;
  if (directory_p(filename))
    add_directory_to_view_files_list(vdat, (const char *)filename);
  else add_file_to_view_files_list(vdat, file_or_dir, filename);
  free(filename);
}


static void view_files_add_files(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat = (view_files_info *)context;
  char *file_or_dir;
  file_or_dir = (char *)gtk_entry_get_text(GTK_ENTRY(w));
  if ((file_or_dir) && (*file_or_dir))
    {
      view_files_add_file_or_directory(vdat, (const char *)file_or_dir);
      view_files_display_list(vdat);
    }
}


static void view_files_drop_watcher(GtkWidget *w, const char *str, int x, int y, void *context)
{
  view_files_info *vdat = (view_files_info *)context;
  char *filename;
  filename = mus_expand_filename(str);
  add_file_to_view_files_list(vdat, str, filename);
  free(filename);
  view_files_display_list(vdat);
}


mus_long_t vf_location(view_files_info *vdat)
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
      str = (char *)gtk_entry_get_text(GTK_ENTRY(vdat->at_mark_text));
      if ((str) && (*str))
	{
	  pos = mark_id_to_sample(string_to_int(str, 0, "mark"));
	  if (pos < 0)
	    snd_error_without_format("no such mark");
	}
      else snd_error_without_format("no mark?");
      break;

    case VF_AT_SAMPLE:
      str = (char *)gtk_entry_get_text(GTK_ENTRY(vdat->at_sample_text));
      if ((str) && (*str))
	{
	  pos = string_to_mus_long_t(str, 0, "sample"); 
	  /* pos already checked for lower bound */
	}
      else snd_error_without_format("no sample number?");
      break;
    }
  return(pos);
}


static void vf_clear_sample(view_files_info *vdat);

static void vf_sample_button_modify_callback(GtkWidget *w, gpointer context)
{
  vf_clear_sample((view_files_info *)context);
} 


static void vf_sample_text_modify_callback(GtkWidget *w, gpointer context)
{
  vf_clear_sample((view_files_info *)context);
} 


static void vf_clear_sample(view_files_info *vdat)
{
  vf_clear_error(vdat);
  if (vdat->at_sample_text_handler_id != 0)
    {
      g_signal_handler_disconnect(vdat->at_sample_text, vdat->at_sample_text_handler_id);
      g_signal_handler_disconnect(vdat->at_sample_button, vdat->at_sample_button_handler_id);
      vdat->at_sample_text_handler_id = 0;
      vdat->at_sample_button_handler_id = 0;
    }
}


static void vf_clear_mark(view_files_info *vdat);

static void vf_mark_button_modify_callback(GtkWidget *w, gpointer context)
{
  vf_clear_mark((view_files_info *)context);
}


static void vf_mark_text_modify_callback(GtkWidget *w, gpointer context)
{
  vf_clear_mark((view_files_info *)context);
}


static void vf_clear_mark(view_files_info *vdat)
{
  vf_clear_error(vdat);
  if (vdat->at_mark_text_handler_id != 0)
    {
      g_signal_handler_disconnect(vdat->at_mark_text, vdat->at_mark_text_handler_id);
      g_signal_handler_disconnect(vdat->at_mark_button, vdat->at_mark_button_handler_id);
      vdat->at_mark_text_handler_id = 0;
      vdat->at_mark_button_handler_id = 0;
    }
}


void vf_post_error(const char *error_msg, view_files_info *vdat)
{
  vdat->error_p = true;
  info_widget_display(vdat->info1, error_msg);
  info_widget_display(vdat->info2, "|");
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
      vdat->at_sample_text_handler_id = g_signal_connect(vdat->at_sample_text, "changed", G_CALLBACK(vf_sample_text_modify_callback), (gpointer)data);
      vdat->at_sample_button_handler_id = g_signal_connect(vdat->at_sample_button, "activated", G_CALLBACK(vf_sample_button_modify_callback), (gpointer)data);
    }
  else
    {
      /* watch at_mark_text or button */
      vdat->at_mark_text_handler_id = g_signal_connect(vdat->at_mark_text, "changed", G_CALLBACK(vf_mark_text_modify_callback), (gpointer)data);
      vdat->at_mark_button_handler_id = g_signal_connect(vdat->at_mark_button, "activated", G_CALLBACK(vf_mark_button_modify_callback), (gpointer)data);
    }
}


static gboolean vf_add_text_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  view_files_info *vdat = (view_files_info *)data;
  vf_clear_error(vdat);
  if (vdat->add_text_handler_id)
    {
      g_signal_handler_disconnect(w, vdat->add_text_handler_id);
      vdat->add_text_handler_id = 0;
    }
  return(false);
}


void vf_post_add_error(const char *error_msg, view_files_info *vdat)
{
  vf_post_error(error_msg, vdat);
  vdat->add_text_handler_id = SG_SIGNAL_CONNECT(vdat->add_text, "key_press_event", vf_add_text_modify_callback, (gpointer)vdat);
}


static void view_files_mix_selected_callback(GtkWidget *w, gpointer context) 
{
  view_files_mix_selected_files(w, (view_files_info *)context);
}


static void view_files_insert_selected_callback(GtkWidget *w, gpointer context) 
{
  view_files_insert_selected_files(w, (view_files_info *)context);
}


static void view_files_at_cursor_callback(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat = (view_files_info *)context;
  if (vdat->error_p)
    {
      if (vdat->location_choice == VF_AT_SAMPLE)
	vf_clear_sample(vdat);
      else vf_clear_mark(vdat);
    }
  vdat->location_choice = VF_AT_CURSOR;
}


static void view_files_at_end_callback(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat = (view_files_info *)context;
  if (vdat->error_p)
    {
      if (vdat->location_choice == VF_AT_SAMPLE)
	vf_clear_sample(vdat);
      else vf_clear_mark(vdat);
    }
  vdat->location_choice = VF_AT_END;
}


static void view_files_at_beginning_callback(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat = (view_files_info *)context;
  if (vdat->error_p)
    {
      if (vdat->location_choice == VF_AT_SAMPLE)
	vf_clear_sample(vdat);
      else vf_clear_mark(vdat);
    }
  vdat->location_choice = VF_AT_BEGINNING;
}


static void view_files_at_sample_callback(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat = (view_files_info *)context;
  if ((vdat->error_p) && 
      (vdat->location_choice == VF_AT_MARK))
      vf_clear_mark(vdat);
  vdat->location_choice = VF_AT_SAMPLE;
}


static void view_files_at_mark_callback(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat = (view_files_info *)context;
  if ((vdat->error_p) &&
      (vdat->location_choice == VF_AT_SAMPLE))
    vf_clear_sample(vdat);
  vdat->location_choice = VF_AT_MARK;
}



/* -------- speed -------- */

static bool speed_pressed = false, speed_dragged = false;

static mus_float_t vf_speed_to_scroll(mus_float_t minval, mus_float_t val, mus_float_t maxval)
{
  if (val <= minval) return(0.0);
  if (val >= maxval) return(0.9);
  return(0.9 * ((log(val) - log(minval)) / (log(maxval) - log(minval))));
}


static mus_float_t vf_scroll_to_speed(mus_float_t scroll)
{
  return(exp((scroll * (log(speed_control_max(ss)) - log(speed_control_min(ss))) / 0.9) + log(speed_control_min(ss))));
}


static void vf_set_speed_label(view_files_info *vdat, mus_float_t val)
{
  char speed_number_buffer[6];
  vdat->speed = speed_changed(val,
			      speed_number_buffer,
			      vdat->speed_style,
			      speed_control_tones(ss),
			      6);
  set_label(vdat->speed_number, speed_number_buffer);
}


void vf_set_speed(view_files_info *vdat, mus_float_t val)
{
  vf_set_speed_label(vdat, val);
  ADJUSTMENT_SET_VALUE(vdat->speed_adj, vf_speed_to_scroll(speed_control_min(ss), vdat->speed, speed_control_max(ss)));
  /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(vdat->speed_adj)); */
}


static gboolean vf_speed_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer context)
{
  view_files_info *vdat = (view_files_info *)context;
  speed_dragged = false;
  speed_pressed = false;
  vf_set_speed(vdat, 1.0);
  return(false);
}


static gboolean vf_speed_label_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer context)
{
  view_files_info *vdat = (view_files_info *)context;
  speed_dragged = false;
  speed_pressed = false;

  switch (vdat->speed_style)
    {
    default:
    case SPEED_CONTROL_AS_FLOAT:    vdat->speed_style = SPEED_CONTROL_AS_RATIO;    break;
    case SPEED_CONTROL_AS_RATIO:    vdat->speed_style = SPEED_CONTROL_AS_SEMITONE; break;
    case SPEED_CONTROL_AS_SEMITONE: vdat->speed_style = SPEED_CONTROL_AS_FLOAT;    break;
    }

  vf_set_speed_label(vdat, vf_scroll_to_speed(ADJUSTMENT_VALUE(vdat->speed_adj)));
  return(false);
}


static gboolean vf_speed_motion_callback(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  view_files_info *vdat = (view_files_info *)data;
  if (!speed_pressed) {speed_dragged = false; return(false);}
  speed_dragged = true;
  vf_set_speed_label(vdat, vf_scroll_to_speed(ADJUSTMENT_VALUE(vdat->speed_adj)));
  return(false);
}


static gboolean vf_speed_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  view_files_info *vdat = (view_files_info *)data;
  speed_pressed = false;
  speed_dragged = false;
  vf_set_speed_label(vdat, vf_scroll_to_speed(ADJUSTMENT_VALUE(vdat->speed_adj)));
  return(false);
}


static gboolean vf_speed_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  speed_pressed = true;
  speed_dragged = false;
  return(false);
}


/* -------- amp -------- */

static bool amp_pressed = false, amp_dragged = false;

static mus_float_t vf_scroll_to_amp(mus_float_t val)
{
  if (val <= 0.0) 
    return(amp_control_min(ss));
  if (val >= 0.9) 
    return(amp_control_max(ss));
  if (val > (0.5 * 0.9))
    return((((val / (0.5 * 0.9)) - 1.0) * (amp_control_max(ss) - 1.0)) + 1.0);
  else return((val * (1.0 - amp_control_min(ss)) / (0.5 * 0.9)) + amp_control_min(ss));
}


static mus_float_t vf_amp_to_scroll(mus_float_t amp)
{
  return(amp_to_scroll(amp_control_min(ss), amp, amp_control_max(ss)));
}


void vf_set_amp(view_files_info *vdat, mus_float_t val)
{
  char sfs[6];
  vdat->amp = val;
  mus_snprintf(sfs, 6, "%.2f", val);
  set_label(vdat->amp_number, sfs);
  ADJUSTMENT_SET_VALUE(vdat->amp_adj, vf_amp_to_scroll(vdat->amp));
  /* gtk_adjustment_value_changed(GTK_ADJUSTMENT(vdat->amp_adj)); */
}


static gboolean vf_amp_click_callback(GtkWidget *w, GdkEventButton *ev, gpointer context)
{
  amp_dragged = false;
  amp_pressed = false;
  vf_set_amp((view_files_info *)context, 1.0);
  return(false);
}


static gboolean vf_amp_motion_callback(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{
  mus_float_t scrollval;
  char sfs[6];
  view_files_info *vdat = (view_files_info *)data;

  if (!amp_pressed) {amp_dragged = false; return(false);}
  amp_dragged = true;

  scrollval = ADJUSTMENT_VALUE(vdat->amp_adj);
  vdat->amp = vf_scroll_to_amp(scrollval);
  mus_snprintf(sfs, 6, "%.2f", vdat->amp);
  set_label(vdat->amp_number, sfs);

  return(false);
}


static gboolean vf_amp_release_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  mus_float_t scrollval;
  char sfs[6];
  view_files_info *vdat = (view_files_info *)data;

  amp_pressed = false;
  /* if (!amp_dragged) return(false); */
  amp_dragged = false;

  scrollval = ADJUSTMENT_VALUE(vdat->amp_adj);
  vdat->amp = vf_scroll_to_amp(scrollval);
  mus_snprintf(sfs, 6, "%.2f", vdat->amp);
  set_label(vdat->amp_number, sfs);

  return(false);
}


static gboolean vf_amp_press_callback(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  amp_pressed = true;
  amp_dragged = false;
  return(false);
}



/* -------- amp-envs -------- */

static void vf_amp_env_resize(view_files_info *vdat, GtkWidget *w)
{
  if (vdat->env_ax == NULL)
    {
      vdat->env_gc = gc_new();
      gc_set_background(vdat->env_gc, ss->graph_color);
      gc_set_foreground(vdat->env_gc, ss->data_color);
      vdat->env_ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      vdat->env_ax->wn = WIDGET_TO_WINDOW(w);
      vdat->env_ax->w = w;
      vdat->env_ax->gc = vdat->env_gc;
    }
  ss->cr = make_cairo(WIDGET_TO_WINDOW(w));
  cairo_push_group(ss->cr);

  /* erase previous */
  cairo_set_source_rgba(ss->cr, vdat->env_gc->bg_color->red, vdat->env_gc->bg_color->green, vdat->env_gc->bg_color->blue, vdat->env_gc->bg_color->alpha);
  cairo_rectangle(ss->cr, 0, 0, widget_width(w), widget_height(w));
  cairo_fill(ss->cr);

  vdat->spf->with_dots = true;
  env_editor_display_env(vdat->spf, vdat->amp_env, vdat->env_ax, NULL, 0, 0, widget_width(w), widget_height(w), NOT_PRINTING);

  cairo_pop_group_to_source(ss->cr);
  cairo_paint(ss->cr);
  free_cairo(ss->cr);
  ss->cr = NULL;
}


void vf_set_amp_env(view_files_info *vdat, env *new_e)
{
  if (!vdat) return;
  if (vdat->amp_env) free_env(vdat->amp_env);
  vdat->amp_env = copy_env(new_e);
  if ((vdat->dialog) &&
      (widget_is_active(vdat->dialog)))
    vf_amp_env_resize(vdat, vdat->env_drawer);
}


static gboolean vf_drawer_button_press(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  view_files_info *vdat = (view_files_info *)data;
  vdat->spf->with_dots = false;
  if (env_editor_button_press(vdat->spf, (int)(EVENT_X(ev)), (int)(EVENT_Y(ev)), EVENT_TIME(ev), vdat->amp_env))
    vf_amp_env_resize(vdat, w);
  return(false);
}


static gboolean vf_drawer_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  view_files_info *vdat = (view_files_info *)data;
  env_editor_button_release(vdat->spf, vdat->amp_env);
  vf_amp_env_resize(vdat, w);
  return(false);
}


static gboolean vf_drawer_button_motion(GtkWidget *w, GdkEventMotion *ev, gpointer data)
{ 
  view_files_info *vdat = (view_files_info *)data;
  if (BUTTON1_PRESSED(EVENT_STATE(ev)))
    {
      int x, y;
      GdkModifierType state;
      if (EVENT_IS_HINT(ev))
	window_get_pointer(ev, &x, &y, &state);
      else
	{
	  x = (int)(EVENT_X(ev));
	  y = (int)(EVENT_Y(ev));
	}
      vdat->spf->with_dots = false;
      env_editor_button_motion(vdat->spf, x, y, EVENT_TIME(ev), vdat->amp_env);
      vf_amp_env_resize(vdat, w);
    }
  return(false);
}


static gboolean vf_amp_env_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  view_files_info *vdat = (view_files_info *)data;
  vf_amp_env_resize(vdat, w);
  return(false);
}


static gboolean vf_amp_env_resize_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  view_files_info *vdat = (view_files_info *)data;
  vf_amp_env_resize(vdat, w);
  return(false);
}


static void view_files_reset_callback(GtkWidget *w, gpointer context) 
{
  view_files_info *vdat = (view_files_info *)context;
  env *e;
  vf_set_amp(vdat, 1.0);
  vf_set_speed(vdat, 1.0);
  vf_set_amp_env(vdat, e = default_env(1.0, 1.0));
  free_env(e);
  sort_vf(vdat, view_files_sort(ss));
}


GtkWidget *make_view_files_dialog_1(view_files_info *vdat, bool managed)
{
  if (!(vdat->dialog))
    {
      int i;
      GtkWidget *sep1, *cww, *rlw, *tophbox, *add_label, *addbox;
#if WITH_AUDIO
      GtkWidget *plw;
#endif
      GtkWidget *sbar, *sitem, *newB;
      GtkWidget *mainform, *leftform, *fileform, *helpB, *dismissB, *resetB;

      vdat->dialog = snd_gtk_dialog_new();
      add_dialog_style(vdat->dialog);
      widget_modify_bg(vdat->dialog, GTK_STATE_NORMAL, ss->basic_color);
      {
	char *filestr = NULL;
	filestr = mus_format("%s %d", "Files", vdat->index + 1);
	gtk_window_set_title(GTK_WINDOW(vdat->dialog), filestr);
	free(filestr);
      }

      sg_make_resizable(vdat->dialog);
      gtk_container_set_border_width (GTK_CONTAINER(vdat->dialog), 10);
      gtk_widget_realize(vdat->dialog);
      gtk_window_resize(GTK_WINDOW(vdat->dialog), 500, 540);

      helpB = gtk_button_new_from_stock(GTK_STOCK_HELP);
      gtk_widget_set_name(helpB, "dialog_button");

      newB = sg_button_new_from_stock_with_label("New Viewer", GTK_STOCK_NEW);
      gtk_widget_set_name(newB, "dialog_button");

      dismissB = gtk_button_new_from_stock(GTK_STOCK_QUIT);
      gtk_widget_set_name(dismissB, "dialog_button");
      set_stock_button_label(dismissB, I_GO_AWAY);

      resetB = sg_button_new_from_stock_with_label("Reset", GTK_STOCK_REFRESH);
      gtk_widget_set_name(resetB, "dialog_button");

      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(vdat->dialog)), newB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(vdat->dialog)), resetB, true, true, 10);
      gtk_box_pack_start(GTK_BOX(DIALOG_ACTION_AREA(vdat->dialog)), dismissB, true, true, 10);
      gtk_box_pack_end(GTK_BOX(DIALOG_ACTION_AREA(vdat->dialog)), helpB, true, true, 10);

#if HAVE_GTK_3
      add_highlight_button_style(helpB);
      add_highlight_button_style(newB);
      add_highlight_button_style(dismissB);
      add_highlight_button_style(resetB);
#endif

      SG_SIGNAL_CONNECT(vdat->dialog, "delete_event", view_files_delete_callback, (gpointer)vdat);
      SG_SIGNAL_CONNECT(dismissB, "clicked", view_files_dismiss_callback, (gpointer)vdat);
      SG_SIGNAL_CONNECT(newB, "clicked", view_files_new_viewer_callback, (gpointer)vdat);
      SG_SIGNAL_CONNECT(helpB, "clicked", view_files_help_callback, (gpointer)vdat);
      SG_SIGNAL_CONNECT(resetB, "clicked", view_files_reset_callback, (gpointer)vdat);

      gtk_widget_show(dismissB);
      gtk_widget_show(newB);
      gtk_widget_show(helpB);
      gtk_widget_show(resetB);

      mainform = gtk_hpaned_new();
      gtk_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(vdat->dialog)), mainform, true, true, 8);
      gtk_widget_set_name(mainform, "the_unpane");
      add_paned_style(mainform);
      gtk_widget_show(mainform);

      {
	GtkWidget *lmargin, *rmargin;
	/* these exist solely to put some blank space around the handle */
#if (!HAVE_GTK_3)
	lmargin = gtk_hbox_new(false, 0); 
	gtk_paned_add1(GTK_PANED(mainform), lmargin);	
	gtk_widget_show(lmargin);

	rmargin = gtk_hbox_new(false, 0);
	gtk_paned_add2(GTK_PANED(mainform), rmargin);	
	gtk_widget_show(rmargin);

	leftform = gtk_vbox_new(false, 0);
	gtk_box_pack_start(GTK_BOX(lmargin), leftform, true, true, 4);
	gtk_widget_show(leftform);

	fileform = gtk_vbox_new(false, 0);
	gtk_box_pack_start(GTK_BOX(rmargin), fileform, true, true, 4);
	gtk_widget_show(fileform);
#else
	lmargin = gtk_event_box_new();
	gtk_paned_add1(GTK_PANED(mainform), lmargin);	
	gtk_widget_show(lmargin);

	rmargin = gtk_event_box_new();
	gtk_paned_add2(GTK_PANED(mainform), rmargin);	
	gtk_widget_show(rmargin);

	leftform = gtk_vbox_new(false, 0);
	gtk_container_add(GTK_CONTAINER(lmargin), leftform);
	gtk_widget_set_hexpand(GTK_WIDGET(lmargin), true);
	gtk_widget_set_vexpand(GTK_WIDGET(lmargin), true);
	gtk_widget_show(leftform);

	fileform = gtk_vbox_new(false, 0);
	gtk_container_add(GTK_CONTAINER(rmargin), fileform);
	gtk_widget_set_hexpand(GTK_WIDGET(rmargin), true);
	gtk_widget_set_vexpand(GTK_WIDGET(rmargin), true);
	gtk_widget_show(fileform);
#endif
      }

      /* files section: play files | files */

      rlw = snd_gtk_highlight_label_new("files");
      gtk_box_pack_start(GTK_BOX(fileform), rlw, false, false, 0);
      gtk_widget_show(rlw);

      sep1 = gtk_hseparator_new();
      gtk_box_pack_start(GTK_BOX(fileform), sep1, false, false, 2);
      widget_modify_bg(sep1, GTK_STATE_NORMAL, ss->zoom_color);
      gtk_widget_show(sep1);

      tophbox = gtk_hbox_new(false, 0);
      gtk_box_pack_start(GTK_BOX(fileform), tophbox, false, false, 4);
      gtk_widget_show(tophbox);

#if WITH_AUDIO
#if (!HAVE_GTK_3)
      plw = gtk_label_new("play"); 
#else
      plw = gtk_button_new_with_label("play");
      add_highlight_button_style(plw);
#endif
      gtk_box_pack_start(GTK_BOX(tophbox), plw, false, false, 5);
      gtk_widget_show(plw);
#endif

      sbar = gtk_menu_bar_new();
      gtk_box_pack_end(GTK_BOX(tophbox), sbar, false, false, 0);
      add_menu_style(sbar);
      gtk_widget_show(sbar);

      vdat->smenu = gtk_menu_new();
      add_menu_style(vdat->smenu);
      vdat->a_to_z = gtk_menu_item_new_with_label("a..z");
      vdat->z_to_a = gtk_menu_item_new_with_label("z..a");
      vdat->new_to_old = gtk_menu_item_new_with_label("new..old");
      vdat->old_to_new = gtk_menu_item_new_with_label("old..new");
      vdat->small_to_big = gtk_menu_item_new_with_label("small..big");
      vdat->big_to_small = gtk_menu_item_new_with_label("big..small");

      gtk_menu_shell_append(GTK_MENU_SHELL(vdat->smenu), vdat->a_to_z);
      gtk_menu_shell_append(GTK_MENU_SHELL(vdat->smenu), vdat->z_to_a);
      gtk_menu_shell_append(GTK_MENU_SHELL(vdat->smenu), vdat->new_to_old);
      gtk_menu_shell_append(GTK_MENU_SHELL(vdat->smenu), vdat->old_to_new);
      gtk_menu_shell_append(GTK_MENU_SHELL(vdat->smenu), vdat->small_to_big);
      gtk_menu_shell_append(GTK_MENU_SHELL(vdat->smenu), vdat->big_to_small);

      vdat->sort_items_size = 4;
      vdat->sort_items = (GtkWidget **)calloc(vdat->sort_items_size, sizeof(GtkWidget *));
      for (i = 0; i < vdat->sort_items_size; i++)
	{
	  vdat->sort_items[i] = gtk_menu_item_new_with_label("unused");
	  gtk_menu_shell_append(GTK_MENU_SHELL(vdat->smenu), vdat->sort_items[i]);
	}

      gtk_widget_show(vdat->a_to_z);
      gtk_widget_show(vdat->z_to_a);
      gtk_widget_show(vdat->new_to_old);
      gtk_widget_show(vdat->old_to_new);
      gtk_widget_show(vdat->small_to_big);
      gtk_widget_show(vdat->big_to_small);

      sitem = gtk_menu_item_new_with_label("sort");
      gtk_widget_show(sitem);
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(sitem), vdat->smenu);
      gtk_menu_shell_append(GTK_MENU_SHELL(sbar), sitem);

      SG_SIGNAL_CONNECT(vdat->a_to_z,        "activate", sort_view_files_a_to_z, (gpointer)vdat);
      SG_SIGNAL_CONNECT(vdat->z_to_a,        "activate", sort_view_files_z_to_a, (gpointer)vdat);
      SG_SIGNAL_CONNECT(vdat->new_to_old,    "activate", sort_view_files_new_to_old, (gpointer)vdat);
      SG_SIGNAL_CONNECT(vdat->old_to_new,    "activate", sort_view_files_old_to_new, (gpointer)vdat);
      SG_SIGNAL_CONNECT(vdat->small_to_big,  "activate", sort_view_files_small_to_big, (gpointer)vdat);
      SG_SIGNAL_CONNECT(vdat->big_to_small,  "activate", sort_view_files_big_to_small, (gpointer)vdat);

      {
	int i;
	for (i = 0; i < vdat->sort_items_size; i++)
	  SG_SIGNAL_CONNECT(vdat->sort_items[i],  "activate", sort_view_files_xen, (gpointer)vdat);
      }

      vdat->file_list = gtk_vbox_new(false, 0);
#if HAVE_GTK_3
      gtk_widget_set_hexpand(GTK_WIDGET(vdat->file_list), true);
      gtk_widget_set_vexpand(GTK_WIDGET(vdat->file_list), true);
#endif

      cww = gtk_scrolled_window_new(NULL, NULL);
      gtk_box_pack_start(GTK_BOX(fileform), cww, true, true, 0);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(cww), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
#if HAVE_GTK_HEADER_BAR_NEW
      /* actually 3.8 -- do we need a flag for this? */
      gtk_container_add(GTK_CONTAINER(cww), vdat->file_list);
#else
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(cww), vdat->file_list);
#endif

      gtk_widget_show(vdat->file_list);
      gtk_widget_show(cww);
      add_drop(vdat->file_list, view_files_drop_watcher, (void *)vdat);

      addbox = gtk_hbox_new(false, 0);
      gtk_box_pack_end(GTK_BOX(fileform), addbox, false, false, 0);
      gtk_widget_show(addbox);

#if HAVE_GTK_3
      add_label = gtk_button_new_with_label("add:");
      add_highlight_button_style(add_label);
#else
      add_label = gtk_label_new("add:");
#endif
      gtk_box_pack_start(GTK_BOX(addbox), add_label, false, false, 4);
      gtk_widget_show(add_label);

      vdat->add_text = snd_entry_new(addbox, NULL, WITH_WHITE_BACKGROUND);
      SG_SIGNAL_CONNECT(vdat->add_text, "activate", view_files_add_files, (gpointer)vdat);


      /* left side */
      {
	GtkWidget *ltop_sep, *frame;
	GtkWidget *lbox1, *lbox2, *lbox3, *lbox4, *lbox5;

	vdat->left_title = snd_gtk_entry_label_new("(no files selected)", ss->highlight_color);
	gtk_box_pack_start(GTK_BOX(leftform), vdat->left_title, false, false, 0);

	gtk_entry_set_alignment(GTK_ENTRY(vdat->left_title), 0.5);

	gtk_widget_show(vdat->left_title);

	ltop_sep = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(leftform), ltop_sep, false, false, 2);
	widget_modify_bg(ltop_sep, GTK_STATE_NORMAL, ss->zoom_color);
	gtk_widget_show(ltop_sep);

	vdat->info1 = make_info_widget();
	gtk_box_pack_start(GTK_BOX(leftform), vdat->info1, false, true, 4);
	gtk_widget_show(vdat->info1);

	vdat->info2 = make_info_widget();
	gtk_box_pack_start(GTK_BOX(leftform), vdat->info2, false, true, 4);
	gtk_widget_show(vdat->info2);

	{
	  GtkWidget *spacer;
	  spacer = gtk_vseparator_new();
	  gtk_box_pack_start(GTK_BOX(leftform), spacer, false, false, 2);
	  gtk_widget_show(spacer);
	}

#if (!HAVE_GTK_3)
	/* framed stuff */
	frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_ETCHED_IN);
	gtk_container_set_border_width(GTK_CONTAINER(frame), 3);
	widget_modify_bg(frame, GTK_STATE_NORMAL, ss->zoom_color);
	gtk_widget_show(frame);
	gtk_box_pack_start(GTK_BOX(leftform), frame, false, false, 0);
#else
	{
	  GtkWidget *top_sep;
	  top_sep = gtk_vseparator_new();
	  gtk_box_pack_start(GTK_BOX(leftform), top_sep, false, false, 4);
	  gtk_widget_show(top_sep);
	}
	frame = gtk_event_box_new();
	add_highlight_button_style(frame);
	gtk_widget_show(frame);
	gtk_box_pack_start(GTK_BOX(leftform), frame, false, false, 0);
#endif
	
	lbox2 = gtk_vbox_new(false, 2);
	gtk_container_add(GTK_CONTAINER(frame), lbox2);
	gtk_widget_show(lbox2);

	lbox1 = gtk_hbox_new(false, 0);
	gtk_box_pack_start(GTK_BOX(lbox2), lbox1, false, false, 8);
	gtk_widget_show(lbox1);

	vdat->mixB = sg_button_new_from_stock_with_label("Mix", GTK_STOCK_ADD);
	vdat->insertB = sg_button_new_from_stock_with_label("Insert", GTK_STOCK_PASTE);

	gtk_box_pack_start(GTK_BOX(lbox1), vdat->mixB, true, true, 1);
	gtk_box_pack_end(GTK_BOX(lbox1), vdat->insertB, true, true, 1);
	add_highlight_button_style(vdat->mixB);
	add_highlight_button_style(vdat->insertB);

	SG_SIGNAL_CONNECT(vdat->mixB, "clicked", view_files_mix_selected_callback, (gpointer)vdat);
	SG_SIGNAL_CONNECT(vdat->insertB, "clicked", view_files_insert_selected_callback, (gpointer)vdat);

	set_sensitive(vdat->mixB, false);
	set_sensitive(vdat->insertB, false);

	gtk_widget_show(vdat->mixB);
	gtk_widget_show(vdat->insertB);

	#define LEFT_MARGIN 4

	vdat->at_cursor_button = gtk_radio_button_new_with_label(NULL, "at cursor");
	widget_modify_bg(vdat->at_cursor_button, GTK_STATE_PRELIGHT, ss->lighter_blue);
	widget_set_margin_left(vdat->at_cursor_button, LEFT_MARGIN);
	gtk_box_pack_start(GTK_BOX(lbox2), vdat->at_cursor_button, false, false, 0);
	add_check_button_style(vdat->at_cursor_button);
	gtk_widget_show(vdat->at_cursor_button);
	SG_SIGNAL_CONNECT(vdat->at_cursor_button, "clicked", view_files_at_cursor_callback, (gpointer)vdat);

	vdat->at_end_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(vdat->at_cursor_button)), "at end");
	widget_modify_bg(vdat->at_end_button, GTK_STATE_PRELIGHT, ss->lighter_blue);
	widget_set_margin_left(vdat->at_end_button, LEFT_MARGIN);
	gtk_box_pack_start(GTK_BOX(lbox2), vdat->at_end_button, false, false, 0);
	add_check_button_style(vdat->at_end_button);
	gtk_widget_show(vdat->at_end_button);
	SG_SIGNAL_CONNECT(vdat->at_end_button, "clicked", view_files_at_end_callback, (gpointer)vdat);

	vdat->at_beginning_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(vdat->at_cursor_button)), "at beginning");
	widget_modify_bg(vdat->at_beginning_button, GTK_STATE_PRELIGHT, ss->lighter_blue);
	widget_set_margin_left(vdat->at_beginning_button, LEFT_MARGIN);
	add_check_button_style(vdat->at_beginning_button);
	gtk_box_pack_start(GTK_BOX(lbox2), vdat->at_beginning_button, false, false, 0);
	gtk_widget_show(vdat->at_beginning_button);
	SG_SIGNAL_CONNECT(vdat->at_beginning_button, "clicked", view_files_at_beginning_callback, (gpointer)vdat);

	lbox5 = gtk_hbox_new(false, 0);
	gtk_box_pack_start(GTK_BOX(lbox2), lbox5, false, false, 0);
	gtk_widget_show(lbox5);

	lbox3 = gtk_vbox_new(true, 0);
	gtk_box_pack_start(GTK_BOX(lbox5), lbox3, false, false, 0);
	gtk_widget_show(lbox3);

	lbox4 = gtk_vbox_new(true, 0);
	gtk_box_pack_start(GTK_BOX(lbox5), lbox4, true, true, 10);
	gtk_widget_show(lbox4);

	vdat->at_sample_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(vdat->at_cursor_button)), "at sample");
	widget_modify_bg(vdat->at_sample_button, GTK_STATE_PRELIGHT, ss->lighter_blue);
	widget_set_margin_left(vdat->at_sample_button, LEFT_MARGIN);
	add_check_button_style(vdat->at_sample_button);
	gtk_box_pack_start(GTK_BOX(lbox3), vdat->at_sample_button, false, false, 0);
	gtk_widget_show(vdat->at_sample_button);
	SG_SIGNAL_CONNECT(vdat->at_sample_button, "clicked", view_files_at_sample_callback, (gpointer)vdat);

	vdat->at_sample_text = snd_entry_new(lbox4, NULL, WITH_WHITE_BACKGROUND);
	gtk_widget_show(vdat->at_sample_text);

	vdat->at_mark_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(vdat->at_cursor_button)), "at mark");
	widget_modify_bg(vdat->at_mark_button, GTK_STATE_PRELIGHT, ss->lighter_blue);
	widget_set_margin_left(vdat->at_mark_button, LEFT_MARGIN);
	add_check_button_style(vdat->at_mark_button);
	gtk_box_pack_end(GTK_BOX(lbox3), vdat->at_mark_button, false, false, 0);
	gtk_widget_show(vdat->at_mark_button);
	SG_SIGNAL_CONNECT(vdat->at_mark_button, "clicked", view_files_at_mark_callback, (gpointer)vdat);

	vdat->at_sample_text = snd_entry_new(lbox4, NULL, WITH_WHITE_BACKGROUND);
	gtk_widget_show(vdat->at_sample_text);

	{
	  GtkWidget *ampH, *ampL, *speedH, *speedL, *gframe;
#if HAVE_GTK_3
	  {
	    GtkWidget *top_sep;
	    top_sep = gtk_vseparator_new();
	    gtk_box_pack_start(GTK_BOX(leftform), top_sep, false, false, 8);
	    gtk_widget_show(top_sep);
	  }
#endif
	  /* AMP */
	  ampH = gtk_hbox_new(false, 2);
	  gtk_box_pack_start(GTK_BOX(leftform), ampH, false, false, 4);
      
	  vdat->amp_event = gtk_event_box_new();
	  widget_set_margin_left(vdat->amp_event, LEFT_MARGIN);
	  add_highlight_button_style(vdat->amp_event);
	  gtk_box_pack_start(GTK_BOX(ampH), vdat->amp_event, false, false, 4);
	  gtk_widget_show(vdat->amp_event);
	  SG_SIGNAL_CONNECT(vdat->amp_event, "button_press_event", vf_amp_click_callback, (gpointer)vdat);
      
	  ampL = gtk_label_new("amp:");
	  gtk_container_add(GTK_CONTAINER(vdat->amp_event), ampL);
	  gtk_widget_show(ampL);

	  vdat->amp_number = gtk_label_new("1.00");
	  gtk_box_pack_start(GTK_BOX(ampH), vdat->amp_number, false, false, 0);
	  gtk_widget_show(vdat->amp_number);
	  
	  vdat->amp_adj = (GtkAdjustment *)gtk_adjustment_new(0.5, 0.0, 1.0, 0.001, 0.01, .1);
	  vdat->amp_scrollbar = gtk_hscrollbar_new(GTK_ADJUSTMENT(vdat->amp_adj));
	  gtk_box_pack_start(GTK_BOX(ampH), vdat->amp_scrollbar, true, true, 4);

	  SG_SIGNAL_CONNECT(vdat->amp_scrollbar, "motion_notify_event", vf_amp_motion_callback, (gpointer)vdat);
	  SG_SIGNAL_CONNECT(vdat->amp_scrollbar, "button_release_event", vf_amp_release_callback, (gpointer)vdat);
	  SG_SIGNAL_CONNECT(vdat->amp_scrollbar, "button_press_event", vf_amp_press_callback, (gpointer)vdat);
	  gtk_widget_show(vdat->amp_scrollbar);
      
	  gtk_widget_show(ampH);

	  /* SPEED */
	  speedH = gtk_hbox_new(false, 2);
	  gtk_box_pack_start(GTK_BOX(leftform), speedH, false, false, 4);
      
	  vdat->speed_event = gtk_event_box_new();
	  add_highlight_button_style(vdat->speed_event);
	  widget_set_margin_left(vdat->speed_event, LEFT_MARGIN);
	  gtk_box_pack_start(GTK_BOX(speedH), vdat->speed_event, false, false, 4);
	  gtk_widget_show(vdat->speed_event);
	  SG_SIGNAL_CONNECT(vdat->speed_event, "button_press_event", vf_speed_click_callback, (gpointer)vdat);
      
	  speedL = gtk_label_new("speed:");
	  gtk_container_add(GTK_CONTAINER(vdat->speed_event), speedL);
	  gtk_widget_show(speedL);

	  vdat->speed_label_event = gtk_event_box_new();
	  add_highlight_button_style(vdat->speed_label_event);
	  gtk_box_pack_start(GTK_BOX(speedH), vdat->speed_label_event, false, false, 4);
	  gtk_widget_show(vdat->speed_label_event);
	  SG_SIGNAL_CONNECT(vdat->speed_label_event, "button_press_event", vf_speed_label_click_callback, (gpointer)vdat);
      
	  switch (vdat->speed_style)
	    {
	    case SPEED_CONTROL_AS_RATIO:    vdat->speed_number = gtk_label_new("1/1"); break;
	    case SPEED_CONTROL_AS_SEMITONE: vdat->speed_number = gtk_label_new("1"); break;
	    default:                        vdat->speed_number = gtk_label_new("1.00"); break;
	    }
	  gtk_container_add(GTK_CONTAINER(vdat->speed_label_event), vdat->speed_number);
	  gtk_widget_show(vdat->speed_number);
      
	  vdat->speed_adj = (GtkAdjustment *)gtk_adjustment_new(0.45, 0.0, 1.0, 0.001, 0.01, .1);
	  vdat->speed_scrollbar = gtk_hscrollbar_new(GTK_ADJUSTMENT(vdat->speed_adj));
	  gtk_box_pack_start(GTK_BOX(speedH), vdat->speed_scrollbar, true, true, 4);
	  SG_SIGNAL_CONNECT(vdat->speed_scrollbar, "button_release_event", vf_speed_release_callback, (gpointer)vdat);
	  SG_SIGNAL_CONNECT(vdat->speed_scrollbar, "motion_notify_event", vf_speed_motion_callback, (gpointer)vdat);
	  SG_SIGNAL_CONNECT(vdat->speed_scrollbar, "button_press_event", vf_speed_press_callback, (gpointer)vdat);

	  gtk_widget_show(vdat->speed_scrollbar);
	  gtk_widget_show(speedH);

#if HAVE_GTK_3
	  {
	    GtkWidget *top_sep;
	    top_sep = gtk_vseparator_new();
	    gtk_box_pack_start(GTK_BOX(leftform), top_sep, false, false, 4);
	    gtk_widget_show(top_sep);
	  }
#endif

	  /* GRAPH (frame) */
	  gframe = gtk_frame_new(NULL);
	  gtk_box_pack_end(GTK_BOX(leftform), gframe, true, true, 10);

	  /* GRAPH (drawing area) */
	  vdat->env_drawer = gtk_drawing_area_new();
	  gtk_widget_set_events(vdat->env_drawer, GDK_ALL_EVENTS_MASK);
	  gtk_container_add(GTK_CONTAINER(gframe), vdat->env_drawer);
	  widget_modify_bg(vdat->env_drawer, GTK_STATE_NORMAL, ss->white);
	  gtk_widget_show(vdat->env_drawer);

	  SG_SIGNAL_CONNECT(vdat->env_drawer, DRAW_SIGNAL, vf_amp_env_expose_callback, (gpointer)vdat);
	  SG_SIGNAL_CONNECT(vdat->env_drawer, "configure_event", vf_amp_env_resize_callback, (gpointer)vdat);
	  SG_SIGNAL_CONNECT(vdat->env_drawer, "button_press_event", vf_drawer_button_press, (gpointer)vdat);
	  SG_SIGNAL_CONNECT(vdat->env_drawer, "button_release_event", vf_drawer_button_release, (gpointer)vdat);
	  SG_SIGNAL_CONNECT(vdat->env_drawer, "motion_notify_event", vf_drawer_button_motion, (gpointer)vdat);

	  gtk_widget_show(gframe);

	  vdat->spf = new_env_editor(); /* one global amp env */

	}
      }

      set_dialog_widget(VIEW_FILES_DIALOG, vdat->dialog);
    }
  if (managed) 
    {
      view_files_display_list(vdat);
      raise_dialog(vdat->dialog);
    }
  return(vdat->dialog);
}


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
}
