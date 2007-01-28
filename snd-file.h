#ifndef SND_FILE_H
#define SND_FILE_H

typedef enum {VF_AT_CURSOR, VF_AT_END, VF_AT_BEGINNING, VF_AT_MARK, VF_AT_SAMPLE} vf_location_t;

typedef struct {
  widget_t rw;
  widget_t nm;
  widget_t pl;
  int pos;
  void *vdat;
} vf_row;

typedef struct {
  vf_row **file_list_entries;
  int size;
  char **names;
  char **full_names;
  int end;
  int sorter;
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

  int dirs_size;
  fam_info **dirs;
  char **dir_names;
  bool need_update;

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
#if (!HAVE_FAM)
  widget_t clearB;
  widget_t updateB;
#endif
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

  gc_t env_gc;
#if USE_GTK
  g_adj_t amp_adj;
  g_adj_t speed_adj;

  gulong at_sample_text_handler_id, at_mark_text_handler_id;
  gulong at_sample_button_handler_id, at_mark_button_handler_id;
  gulong add_text_handler_id;
#endif
} view_files_info;

void vf_unhighlight_row(widget_t nm, widget_t rw);
void vf_highlight_row(widget_t nm, widget_t rw);
void vf_post_info(view_files_info *vdat, int pos);
void vf_unpost_info(view_files_info *vdat);
off_t vf_location(view_files_info *vdat);
void vf_post_error(const char *error_msg, view_files_info *data);
void redirect_vf_post_error(const char *error_msg, void *data);
void redirect_vf_post_location_error(const char *error_msg, void *data);
void vf_post_add_error(const char *error_msg, view_files_info *data);
widget_t start_view_files_dialog_1(view_files_info *vdat, bool managed);
void vf_post_selected_files_list(view_files_info *vdat);
void view_files_add_file_or_directory(view_files_info *vdat, const char *file_or_dir);
void vf_reflect_sort_choice_in_menu(view_files_info *vdat);
vf_row *view_files_make_row(view_files_info *vdat, widget_t last_row);
void vf_flash_row(vf_row *r);

void vf_set_amp(view_files_info *vdat, Float val);
void vf_set_speed(view_files_info *vdat, Float val);
void vf_set_amp_env(view_files_info *vdat, env *new_e);

void view_files_display_list(view_files_info *vdat);
void view_files_mix_selected_files(widget_t w, view_files_info *vdat);
void view_files_insert_selected_files(widget_t w, view_files_info *vdat);
void view_files_open_selected_files(view_files_info *vdat);
void view_files_remove_selected_files(view_files_info *vdat);
void view_files_select(vf_row *r, bool add_to_selected);
bool view_files_play(view_files_info *vdat, int pos, bool play);
void vf_clear_error(view_files_info *vdat);
void view_files_clear_list(view_files_info *vdat);
view_files_info *new_view_files_dialog(void);
void view_files_update_list(view_files_info *vdat);
void add_directory_to_view_files_list(view_files_info *vdat, const char *dirname);
void add_file_to_view_files_list(view_files_info *vdat, const char *filename, const char *fullname);
void vf_mix_insert_buttons_set_sensitive(view_files_info *vdat, bool sensitive);
void vf_open_remove_buttons_set_sensitive(view_files_info *vdat, bool sensitive);
#if (!HAVE_FAM)
  void vf_clear_button_set_sensitive(view_files_info *vdat, bool sensitive);
#endif
void view_files_reflect_sort_items(void);
int vf_mix(view_files_info *vdat);
bool vf_insert(view_files_info *vdat);


char **type_and_format_to_position(file_data *fdat, int type, int format);
void position_to_type_and_format(file_data *fdat, int pos);
int position_to_format(int header, int pos);
int position_to_type(int pos);
char **short_writable_headers(int *len);
char **short_readable_headers(int *len);
char **short_builtin_headers(int *len);
bool encoded_header_p(int header_type);
void snd_encode(int type, const char *input_filename, const char *output_filename);
snd_info *file_is_open_elsewhere_and_has_unsaved_edits(snd_info *sp, const char *fullname);
bool plausible_sound_file_p(const char *name);
snd_info *finish_opening_sound(snd_info *sp, bool selected);

bool edit_header_callback(snd_info *sp, file_data *edit_header_data, 
			  void (*outer_handler)(const char *error_msg, void *ufd),
			  void (*inner_handler)(const char *error_msg, void *ufd));

void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, bool read_only, bool selected);

typedef struct {
  time_t time;
  off_t samps;
  char *filename, *full_filename;
} sort_info;

void snd_sort(int sorter, sort_info **data, int len);
sort_info *free_sort_info(sort_info *ptr);
sort_info *make_sort_info(const char *filename, const char *full_filename);

typedef struct {
  sort_info **files;
  char *dir_name;
  int len;
  int size;
} dir_info;

enum {NO_FILE_FILTER, JUST_SOUNDS_FILTER};
#define PARENT_DIRECTORY ".."

dir_info *free_dir_info (dir_info *dp);
dir_info *find_files_in_dir(const char *name);
dir_info *find_filtered_files_in_dir(const char *name, int filter_choice);
dir_info *find_filtered_files_in_dir_with_pattern(const char *name, int filter_choice, const char *pattern);
#if USE_GTK
  dir_info *find_directories_in_dir(const char *name);
#endif

#define FILENAME_LIST_SIZE 16

void forget_filename(const char *filename, char **names);
void remember_filename(const char *filename, char **names);
char **make_filename_list(void);

#endif
