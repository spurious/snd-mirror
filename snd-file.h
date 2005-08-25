#ifndef SND_FILE_H
#define SND_FILE_H

typedef enum {VF_AT_CURSOR, VF_AT_END, VF_AT_BEGINNING, VF_AT_MARK, VF_AT_SAMPLE} vf_location_t;

typedef struct {
  widget_t rw;
  widget_t nm;
  widget_t pl;
  int pos;
  file_viewer_t parent;
  void *vdat;
} vf_row;

typedef struct {
  vf_row **file_list_entries;
  int size;
  char **names;
  char **full_names;
  int *times;
  int end;
  int curtime, sorter;
  int *selected_files;
  int selected_files_size;
  int currently_selected_files;
  Float amp;
  vf_location_t location_choice;
  Float speed;
  axis_context *env_ax;
  env_editor *spf;
  env *amp_env;
  int open_file_watcher_loc;
  bool error_p;
  int sort_items_size;
  speed_style_t speed_style;
  off_t beg;

  widget_t dialog;
  widget_t file_list;
  widget_t file_list_holder;
  widget_t left_title;
  widget_t info1; 
  widget_t info2; 
  widget_t openB; 
  widget_t mixB; 
  widget_t removeB; 
  widget_t insertB; 
  widget_t clearB; 
  widget_t updateB;
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
  widget_t by_name; 
  widget_t by_date; 
  widget_t by_size; 
  widget_t by_entry;
  widget_t smenu; 
  widget_t current_play_button;
  widget_t amp_event; 
  widget_t speed_event;
  widget_t add_text;

  widget_t* sort_items;

  g_adj_t amp_adj;
  g_adj_t speed_adj;

  gc_t env_gc;
#if USE_GTK
  gulong at_sample_text_handler_id, at_mark_text_handler_id;
  gulong at_sample_button_handler_id, at_mark_button_handler_id;
#endif
} view_files_info;

void vf_unhighlight_row(widget_t nm, widget_t rw);
void vf_highlight_row(widget_t nm, widget_t rw);
void vf_post_info(view_files_info *vdat, int pos);
void vf_unpost_info(view_files_info *vdat);
off_t vf_location(view_files_info *vdat);
void vf_post_error(const char *error_msg, void *data);
void vf_post_location_error(const char *error_msg, void *data);
void vf_post_add_error(const char *error_msg, void *data);
widget_t start_view_files_dialog_1(view_files_info *vdat, bool managed);
void vf_post_selected_files_list(view_files_info *vdat);
void view_files_add_file_or_directory(view_files_info *vdat, const char *file_or_dir);
void vf_reflect_sort_choice_in_menu(view_files_info *vdat);
vf_row *view_files_make_row(view_files_info *vdat, widget_t last_row);

void view_files_set_sort_proc_name(const char *name);
void vf_set_amp(view_files_info *vdat, Float val);
void vf_set_speed(view_files_info *vdat, Float val);
void vf_amp_env_redraw(widget_t w, view_files_info *vdat);
void vf_flash_row(vf_row *r);

void view_files_display_list(view_files_info *vdat);
void view_files_mix_selected_files(widget_t w, view_files_info *vdat);
void view_files_insert_selected_files(widget_t w, view_files_info *vdat);
void view_files_open_selected_files(widget_t w, view_files_info *vdat);
void view_files_remove_selected_files(widget_t w, view_files_info *vdat);
void view_files_select(vf_row *r, bool add_to_selected);
bool view_files_play(view_files_info *vdat, int pos, bool play);
void vf_clear_error(view_files_info *vdat);
void view_files_clear_list(view_files_info *vdat);
view_files_info *new_view_files_dialog(void);
void view_files_update_list(view_files_info *vdat);
void add_directory_to_view_files_list(view_files_info *vdat, const char *dirname);
void add_file_to_view_files_list(view_files_info *vdat, const char *filename, const char *fullname);
view_files_info *vf_dialog_to_info(widget_t dialog);
void vf_mix_insert_buttons_set_sensitive(view_files_info *vdat, bool sensitive);
void vf_open_remove_buttons_set_sensitive(view_files_info *vdat, bool sensitive);
void vf_clear_button_set_sensitive(view_files_info *vdat, bool sensitive);
void view_files_reflect_sort_items(void);
int vf_mix(view_files_info *vdat);
bool vf_insert(view_files_info *vdat);

#endif
