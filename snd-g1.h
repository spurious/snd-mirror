#ifndef SND_G1_H
#define SND_G1_H

typedef struct {
  GtkWidget *rw, *nm, *pl;
  int pos;
  file_viewer_t parent;
} regrow;

#define SOUND_ENV_EDITOR(Sp) ((env_editor *)(sp->sgx->flt))

/* -------- snd-ghelp.c -------- */

GtkWidget *snd_help(const char *subject, const char *help, with_word_wrap_t with_wrap);
GtkWidget *snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, char **xrefs, char **urls);
int help_text_width(const char *txt, int start, int end);
void snd_help_append(char *text);
void snd_help_back_to_top(void);


/* -------- snd-gerror.c -------- */

void add_to_error_history(char *msg, bool popup);
void post_error_dialog(char *msg);
void show_snd_errors(void);

#ifdef __GNUC__
  bool snd_yes_or_no_p(const char *format, ...) __attribute__ ((format (printf, 1, 2)));
#else
  bool snd_yes_or_no_p(const char *format, ...);
#endif



/* -------- snd-gdraw.c -------- */

void draw_line (axis_context *ax, int x0, int y0, int x1, int y1);
void draw_lines (axis_context *ax, GdkPoint *points, int num);
void draw_points (axis_context *ax, GdkPoint *points, int num, int size);
void fill_rectangle (axis_context *ax, int x0, int y0, int width, int height);
void erase_rectangle (chan_info *cp, axis_context *ax, int x0, int y0, int width, int height);
void fill_polygon(axis_context *ax, int points, ...);
void draw_polygon(axis_context *ax, int points, ...);
void draw_string (axis_context *ax, int x0, int y0, char *str, int len);
void draw_arc(axis_context *ax, int x, int y, int size);
void set_grf_points(Locus xi, int j, Locus ymin, Locus ymax);
void set_grf_point(Locus xi, int j, Locus yi);
void draw_grf_points(int dot_size, axis_context *ax, int j, axis_info *ap, Float y0, graph_style_t graph_style);
void draw_both_grf_points(int dot_size, axis_context *ax, int j, graph_style_t graph_style);
void mix_save_graph(mix_context *ms, int j);
void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int j);
void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int j);
void setup_axis_context(chan_info *cp, axis_context *ax);
GtkWidget *start_color_dialog(bool managed);
GtkWidget *start_orientation_dialog(bool managed);
void set_color_scale(Float val);
void set_color_inverted(bool val);
void set_color_cutoff(Float val);
void set_color_map(int val);
void set_spectro_hop(int val);
void set_spectro_x_angle(Float val);
void set_spectro_y_angle(Float val);
void set_spectro_z_angle(Float val);
void set_spectro_x_scale(Float val);
void set_spectro_y_scale(Float val);
void set_spectro_z_scale(Float val);
void set_spectro_cutoff(Float val);
void view_orientation_callback(GtkWidget *w, gpointer info);
void view_color_callback(GtkWidget * w, gpointer info);
bool color_dialog_is_active(void);
bool orientation_dialog_is_active(void);
void reflect_spectro(void);
void allocate_sono_rects(int size);
void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height);
void draw_sono_rectangles(axis_context *ax, int color, int jmax);
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(int colormap);
void initialize_colormap(void);
bool set_with_gl(bool val);
void g_init_gxdraw(void);



/* -------- snd-glistener.c -------- */

void color_listener(GdkColor *pix);
void color_listener_text(GdkColor *pix);
void handle_listener(bool new_state);
void snd_completion_help(int matches, char **buffer);
int listener_height(void);
int listener_width(void);
void goto_listener(void);
void save_listener_text(FILE *fp);
void listener_delete_text(int new_end);
void append_listener_text(int end, char *msg);
void listener_append(char *msg);
void listener_append_and_prompt(char *msg);
void clear_listener(void);
void set_listener_text_font(void);
void g_init_gxlistener(void);
GtkWidget *snd_entry_new(GtkWidget *container, bool with_white_background);
bool highlight_unbalanced_paren(void);
void connect_mouse_to_text(GtkWidget *text);



/* -------- snd-gmain.c -------- */

color_t get_in_between_color(color_t fg, color_t bg);
void snd_doit(int argc, char **argv);
#ifdef SND_AS_WIDGET
  GtkWidget *snd_as_widget(int argc, char **argv, GtkWidget *parent, void (*error_func)(const char *));
#endif

void g_init_gxmain(void);



/* -------- snd-gmenu.c -------- */

GtkWidget *file_open_menu(void);
GtkWidget *file_close_menu(void);
GtkWidget *file_save_menu(void);
GtkWidget *file_save_as_menu(void);
GtkWidget *file_print_menu(void);
GtkWidget *file_revert_menu(void);
GtkWidget *file_update_menu(void);
GtkWidget *file_mix_menu(void);
GtkWidget *file_view_menu(void);
GtkWidget *file_new_menu(void);
GtkWidget *edit_cut_menu(void);
GtkWidget *edit_paste_menu(void);
GtkWidget *edit_mix_menu(void);
GtkWidget *edit_play_menu(void);
GtkWidget *edit_save_as_menu(void);
GtkWidget *edit_undo_menu(void);
GtkWidget *edit_redo_menu(void);
GtkWidget *edit_find_menu(void);
GtkWidget *edit_select_all_menu(void);
GtkWidget *edit_header_menu(void);
GtkWidget *view_region_menu(void);
GtkWidget *view_combine_separate_menu(void);
GtkWidget *view_combine_combined_menu(void);
GtkWidget *view_combine_superimposed_menu(void);
GtkWidget *view_lines_menu(void);
GtkWidget *view_dots_menu(void);
GtkWidget *view_dots_and_lines_menu(void);
GtkWidget *view_filled_menu(void);
GtkWidget *view_lollipops_menu(void);
GtkWidget *view_zero_menu(void);
GtkWidget *view_ctrls_menu(void);
GtkWidget *view_listener_menu(void);
GtkWidget *view_cursor_menu(void);
GtkWidget *view_x_axis_seconds_menu(void);
GtkWidget *view_x_axis_beats_menu(void);
GtkWidget *view_x_axis_samples_menu(void);
GtkWidget *view_x_axis_percentage_menu(void);
GtkWidget *options_save_state_menu(void);
GtkWidget *options_focus_left_menu(void);
GtkWidget *options_focus_right_menu(void);
GtkWidget *options_focus_middle_menu(void);
GtkWidget *options_focus_active_menu(void);
GtkWidget *popup_play_menu(void);
GtkWidget *popup_undo_menu(void);
GtkWidget *popup_redo_menu(void);
GtkWidget *popup_save_menu(void);
GtkWidget *popup_info_menu(void);
bool popup_menu_exists(void);
void set_menu_label(GtkWidget *w, const char *label);
int g_add_to_main_menu(char *label, int slot);
GtkWidget *g_add_to_menu(int which_menu, char *label, int callb, int position);
int g_remove_from_menu(int which_menu, char *label);
GtkWidget *add_menu(void);
void reflect_play_stop_in_popup_menu(void);
void reflect_play_selection_stop(void);
void g_init_gxmenu(void);
GtkWidget *menu_widget(int which_menu);
void check_menu_labels(int key, int state, bool extended);
void popup_menu_from(GtkWidget *w, GdkEventButton *ev, gpointer data, int snd, int chn);
GtkWidget *get_help_menu_widget(void);


/* -------- snd-gfft.c -------- */

void set_fft_window_beta(Float val);
void set_transform_size(int val);
void set_fft_window(mus_fft_window_t val);
void set_transform_type(int val);
void set_wavelet_type(int val);
GtkWidget *fire_up_transform_dialog(bool managed);
bool transform_dialog_is_active(void);

char *transform_type_name(int choice);
int add_transform_to_list(char *name);
int max_transform_type(void);

void set_show_transform_peaks(bool val);
void set_fft_log_magnitude(bool val);
void set_fft_log_frequency(bool val);
void set_transform_normalization(fft_normalize_t val);
void set_show_selection_transform(bool show);
void set_transform_graph_type(graph_type_t val);



/* -------- snd-gdrop.c -------- */

void add_drop(GtkWidget *w);

void g_init_gxdrop(void);



/* -------- snd-gregion.c -------- */

void update_region_browser(bool grf_too);
void reflect_play_region_stop(int n);
bool region_browser_is_active(void);
void delete_region_and_update_browser(int n);
void view_region_callback(GtkWidget *w, gpointer info);
void allocate_region_rows(int n);
bool region_dialog_is_active(void);
void reflect_regions_in_region_browser(void);
void reflect_no_regions_in_region_browser(void);
void reflect_region_graph_style(void);
void g_init_gxregion(void);


/* -------- snd-gxutils -------- */

bool send_netscape(const char *html_viewer, const char *url);
void g_init_gxutils(void);



/* -------- snd-gxbitmaps.c -------- */

char **onlabel_bits(void);
char **offlabel_bits(void);
char **cliplabel_bits(void);
char **mini_bomb_bits(int n);
char **mini_glass_bits(int n);
char **mini_lock_bits(void);
char **blank_bits(void);
char **snd_icon_bits(void);
char **speed_l_bits(void);
char **speed_r_bits(void);

char **line_in_bits(void);
char **cd_bits(void);
char **mic_bits(void);
char **speaker_bits(void);
char **blue_speaker_bits(void);
char **pan_bits(void);
char **yellow_pan_bits(void);
void make_icons_transparent(char *basic_color);


/* -------- snd-gxcolormaps.c -------- */

char **colormap_names(void);
void get_current_color(int colormap, int j, unsigned short *r, unsigned short *g, unsigned short *b);


/* -------- snd-gchn.c -------- */

GtkWidget *channel_sx(chan_info *cp);
GtkWidget *channel_sy(chan_info *cp);
GtkWidget *channel_zx(chan_info *cp);
GtkWidget *channel_zy(chan_info *cp);
GtkWidget *channel_w(chan_info *cp);
GtkWidget *channel_f(chan_info *cp);
GtkWidget *channel_graph(chan_info *cp);
GtkWidget *channel_up_arrow(chan_info *cp);
GtkWidget *channel_down_arrow(chan_info *cp);
bool channel_open_pane(chan_info *cp, void *ptr);
void set_zx_scrollbar_value(chan_info *cp, Float value);
void resize_sx(chan_info *cp);
void resize_zx(chan_info *cp);
void resize_sy(chan_info *cp);
void resize_zy(chan_info *cp);
void initialize_scrollbars(chan_info *cp);
Float gsy_value(chan_info *cp);
Float gsy_size(chan_info *cp);
bool fixup_cp_cgx_ax_wn(chan_info *cp);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
gboolean graph_key_press(GtkWidget *w, GdkEventKey *event, gpointer data);
int add_channel_window(snd_info *sound, int channel, int chan_y, int insertion, GtkWidget *main, fw_button_t arrows, bool with_events);
void set_peak_numbers_font(chan_info *cp);
void set_bold_peak_numbers_font(chan_info *cp);
void set_tiny_numbers_font(chan_info *cp);
color_t get_foreground_color(chan_info *cp, axis_context *ax);
color_t get_background_color(chan_info *cp, axis_context *ax);
void set_foreground_color(chan_info *cp, axis_context *ax, GdkColor *color);
GdkGC *copy_GC(chan_info *cp);
GdkGC *erase_GC(chan_info *cp);
void cleanup_cw(chan_info *cp);
void change_channel_style(snd_info *sp, channel_style_t new_style);

void g_init_gxchn(void);



/* -------- snd-gfind.c -------- */

void edit_find_callback(GtkWidget *w, gpointer info);
void set_find_dialog_label(const char *str);
void g_init_gxfind(void);



/* -------- snd-gutils.c -------- */

bool set_tiny_font(char *font);
bool set_listener_font(char *font);
bool set_peaks_font(char *font);
bool set_bold_peaks_font(char *font);
bool set_axis_label_font(char *font);
bool set_axis_numbers_font(char *font);
int label_width(char *txt);
int number_width(char *num);
int number_height(void);
int label_height(void);
int sg_text_width(char *txt, PangoFontDescription *font);
int mark_name_width(char *txt);
void clear_window(axis_context *ax);
void highlight_color(GtkWidget *w);
void raise_dialog(GtkWidget *w);
void set_button_label(GtkWidget *label, const char *str);
void set_label(GtkWidget *label, const char *str);
void sg_left_justify_button(GtkWidget *button);
void sg_left_justify_label(GtkWidget *label);
void check_for_event(void);
void force_update(GtkWidget *wid);
bool event_pending(void);
void set_title(const char *title);
void goto_window(GtkWidget *text);
void gc_set_foreground_xor(GdkGC *gc, GdkColor *col1, GdkColor *col2);
void color_cursor(GdkColor *color);
void color_marks(GdkColor *color);
void color_selection(GdkColor *color);
void color_data(GdkColor *color);
void color_selected_data(GdkColor *color);
void color_graph(GdkColor *color);
void color_selected_graph(GdkColor *color);
void set_mix_color(GdkColor *color);
void recolor_graph(chan_info *cp, bool selected);
void set_sensitive(GtkWidget *wid, bool val);
void set_toggle_button(GtkWidget *wid, bool val, bool passed, void *data);
guint16 widget_height(GtkWidget *w);
guint16 widget_width(GtkWidget *w);
void set_widget_height(GtkWidget *w, guint16 height);
void set_widget_width(GtkWidget *w, guint16 width);
gint16 widget_x(GtkWidget *w);
gint16 widget_y(GtkWidget *w);
void set_widget_x(GtkWidget *w, gint16 x);
void set_widget_y(GtkWidget *w, gint16 y);
void set_widget_size(GtkWidget *w, guint16 width, guint16 height);
void set_widget_position(GtkWidget *w, gint16 x, gint16 y);
void fixup_axis_context(axis_context *ax, GtkWidget *w, GdkGC *gc);
void set_user_data(GObject *obj, gpointer data);
void set_user_int_data(GObject *obj, int data);
gpointer get_user_data(GObject *obj);
int get_user_int_data(GObject *obj);

char *sg_get_text(GtkWidget *w, int start, int end);
void sg_set_cursor(GtkWidget *w, int position);
void sg_text_insert(GtkWidget *w, char *text);
int sg_cursor_position(GtkWidget *w);
void sg_list_append(GtkWidget *w, char *val);
void sg_list_insert(GtkWidget *w, int row, char *val);
void sg_list_select(GtkWidget *lst, int row);
void sg_list_moveto(GtkWidget *lst, int row);

GtkWidget *make_scrolled_text(GtkWidget *parent, bool editable, GtkWidget *boxer, GtkWidget *paner);
GtkWidget *sg_make_list(const char *title, GtkWidget *parent, widget_add_t paned, gpointer gp, int num_items, char **items, 
			GtkSignalFunc callback, int t1, int t2, int t3, int t4);
void sg_text_delete(GtkWidget *w, int start, int end);
void sg_make_resizable(GtkWidget *w);

Cessator add_work_proc(GtkFunction func, gpointer data);
GtkWidget *snd_gtk_dialog_new(void);
GtkWidget *snd_gtk_label_new(const char *label, GdkColor *color);


/* -------- snd-gsnd.c -------- */

int control_panel_height(snd_info *sp);
GtkWidget *w_snd_pane(snd_info *sp);
GtkWidget *w_snd_pane_box(snd_info *sp);
GtkWidget *w_snd_name(snd_info *sp);
GtkWidget *unite_button(snd_info *sp);
void set_control_panel_play_button(snd_info *sp, bool val);
GtkWidget *filter_graph(snd_info *sp);
void snd_file_lock_icon(snd_info *sp, bool on);
void snd_file_bomb_icon(snd_info *sp, bool on);
void x_bomb(snd_info *sp, bool on);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
void make_minibuffer_label(snd_info *sp, char *str);
void set_play_button(snd_info *sp, bool val);
void play_button_pause(bool pausing);
void syncb(snd_info *sp, int on);
void set_amp(snd_info *sp, Float val);
Float amp_to_scroll(Float minval, Float val, Float maxval);
void set_expand(snd_info *sp, Float val);
void set_contrast(snd_info *sp, Float val);
void set_speed(snd_info *sp, Float val);
void set_revlen(snd_info *sp, Float val);
void set_revscl(snd_info *sp, Float val);
void set_filter_order(snd_info *sp, int order);
void set_filter_text(snd_info *sp, char *str);
void display_filter_env(snd_info *sp);
void toggle_expand_button(snd_info *sp, bool state);
void toggle_contrast_button(snd_info *sp, bool state);
void toggle_reverb_button(snd_info *sp, bool state);
void toggle_filter_button(snd_info *sp, bool state);
void toggle_direction_arrow(snd_info *sp, bool state);
void set_filter_in_dB(snd_info *sp, bool val);
void set_filter_in_hz(snd_info *sp, bool val);
void filter_env_changed(snd_info *sp, env *e);
void color_filter_waveform(GdkColor *color);
void lock_apply(snd_info *sp);
void unlock_apply(snd_info *sp);
void reflect_amp_env_completion(snd_info *sp);
void reflect_amp_env_in_progress(snd_info *sp);
snd_info *add_sound_window (char *filename, bool read_only);
void set_sound_pane_file_label(snd_info *sp, char *str);
void snd_info_cleanup(snd_info *sp);
void sound_show_ctrls(snd_info *sp);
void sound_hide_ctrls(snd_info *sp);
bool control_panel_open(snd_info *sp);
void show_controls(void);
void hide_controls(void);
void start_progress_report(snd_info *sp, enved_progress_t from_enved);
void finish_progress_report(snd_info *sp, enved_progress_t from_enved);
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, enved_progress_t from_enved);
void set_apply_button(snd_info *sp, bool val);
void g_init_gxsnd(void);


/* -------- snd-gmix.c -------- */

void reflect_mix_or_track_change(int mix_id, int track_id, bool forced);
GtkWidget *make_mix_dialog(void);
GtkWidget *make_track_dialog(void);
bool mix_play_stopped(void);
bool track_play_stopped(void);
void reflect_mix_play_stop(void);
void reflect_track_play_stop(void);
int mix_dialog_mix(void);
int mix_dialog_set_mix(int id);
int track_dialog_track(void);
int track_dialog_set_track(int id);
void show_track_background_wave(int pts, bool two_sided);


/* -------- snd-genv.c -------- */

axis_info *enved_make_axis(const char *name, axis_context *ax, int ex0, int ey0, int width, int height, 
			   Float xmin, Float xmax, Float ymin, Float ymax, bool printing);
void display_enved_env_with_selection(env *e, char *name, int x0, int y0, int width, int height, bool dots, bool printing);
void set_enved_redo_sensitive(bool val);
void set_enved_revert_sensitive(bool val);
void set_enved_undo_sensitive(bool val);
void set_enved_save_sensitive(bool val);
void set_enved_show_sensitive(bool val);
void make_scrolled_env_list (void);
void alert_enved_amp_env(snd_info *sp);
void new_active_channel_alert(void);
void env_redisplay(void);
void env_redisplay_with_print(void);
void enved_display_point_label(Float x, Float y);
void display_enved_progress(char *str, GdkPixmap *pix);
void enved_print(char *name);
GtkWidget *create_envelope_editor (void);
void set_enved_clip_p(bool val);
void reflect_enved_style(void);
void set_enved_base(Float val);
void set_enved_target(enved_target_t val);
void set_enved_wave_p(bool val);
void set_enved_in_dB(bool val);
bool enved_dialog_is_active(void);
void enved_reflect_selection(bool on);
void set_enved_filter_order(int order);
void color_enved_waveform(GdkColor *pix);
void g_init_gxenv(void);



/* -------- snd-gxen.c -------- */

void color_chan_components(color_t color, slider_choice_t which_component);
void color_unselected_graphs(color_t color);
void g_init_gxen(void);



/* -------- snd-grec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording (void);
widget_t snd_record_file(void);
bool record_dialog_is_active(void);
void recorder_error(char *msg);
void reflect_amp_control_bounds_change_in_recorder(void);
void g_init_gxrec(void);



/* -------- snd-gfile.c -------- */

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples);
void alert_new_file(void);
widget_t make_open_file_dialog(bool read_only, bool managed);
file_data *make_file_data_panel(GtkWidget *parent, char *name, bool with_chan, 
				int header_type, int data_format, bool with_loc, bool comment_as_entry, bool with_samples);
widget_t make_file_save_as_dialog(bool managed);
widget_t make_edit_save_as_dialog(bool managed);
ww_info *make_title_row(GtkWidget *formw, char *top_str, char *main_str, dialog_pad_t pad, dialog_sort_t with_sort, dialog_paned_t with_pane);
regrow *make_regrow(GtkWidget *ww, GtkSignalFunc play_callback, GtkSignalFunc name_callback);
void make_cur_name_row(int old_size, int new_size);
void make_prev_name_row(int old_size, int new_size);
void make_prevfiles_list (void);
void make_curfiles_list (void);
void curfile_highlight(int i);
void view_curfiles_set_row_name(int pos);
void set_file_browser_play_button(char *name, int state);
void highlight_selected_sound(void);
void set_file_sort_sensitive(bool sensitive);
void view_files_callback(GtkWidget *w, gpointer info);
GtkWidget *start_file_dialog(bool managed);
bool view_files_dialog_is_active(void);
file_info *raw_data_dialog_to_file_info(const char *filename, const char *title);
snd_info *make_new_file_dialog(char *newname, int header_type, int data_format, int srate, int chans, char *comment);
widget_t make_mix_file_dialog(bool managed);
GtkWidget *edit_header(snd_info *sp);
void set_open_file_play_button(bool val);
void g_init_gxfile(void);
void clear_deleted_snd_info(void *fd);
widget_t post_it(const char *subject, const char *str);
void reflect_just_sounds_state(void);


/* -------- snd-gprint.c -------- */

void file_print_callback(GtkWidget *w, gpointer info);
widget_t make_file_print_dialog(bool managed);


#endif

