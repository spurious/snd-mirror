#ifndef SND_NOGUI1_H
#define SND_NOGUI1_H

#define SOUND_ENV_EDITOR(Sp) NULL

/* -------- snd-xhelp.c -------- */

int snd_help(const char *subject, const char *help, with_word_wrap_t with_wrap);
int snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, char **xrefs, char **urls);
int help_text_width(const char *txt, int start, int end);
void snd_help_append(char *text);
void snd_help_back_to_top(void);
void save_help_dialog_state(FILE *fd);


/* -------- snd-xerror.c -------- */

void add_to_error_history(char *msg, bool popup);
void post_error_dialog(char *msg);
bool snd_yes_or_no_p(char *format, ...);


/* -------- snd-xdraw.c -------- */

void draw_line (axis_context *ax, int x0, int y0, int x1, int y1);
void fill_rectangle (axis_context *ax, int x0, int y0, int width, int height);
void erase_rectangle (chan_info *cp, axis_context *ax, int x0, int y0, int width, int height);
void fill_polygon(axis_context *ax, int points, ...);
void draw_polygon(axis_context *ax, int points, ...);
void draw_string (axis_context *ax, int x0, int y0, char *str, int len);
void draw_arc(axis_context *ax, int x, int y, int size);
void set_grf_points(int xi, int j, int ymin, int ymax);
void set_grf_point(int xi, int j, int yi);
void draw_grf_points(int dot_size, axis_context *ax, int j, axis_info *ap, Float y0, graph_style_t graph_style);
void draw_both_grf_points(int dot_size, axis_context *ax, int j, graph_style_t graph_style);
void mix_save_graph(mix_context *ms, int j);
void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int j);
void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int j);
void setup_axis_context(chan_info *cp, axis_context *ax);
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(int colormap);
void allocate_sono_rects(int size);
void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height);
void draw_sono_rectangles(axis_context *ax, int color, int jmax);
widget_t start_color_dialog(bool managed);
widget_t start_orientation_dialog(bool managed);
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
bool color_dialog_is_active(void);
bool orientation_dialog_is_active(void);
void reflect_spectro(void);
void set_with_gl(bool val);
void recorder_error(char *msg);


/* -------- snd-xfind.c -------- */

void set_find_dialog_label(const char *str);
void save_find_dialog_state(FILE *fd);


/* -------- snd-xlistener.c -------- */

void append_listener_text(int end, char *msg);
void save_listener_text(FILE *fp);
void listener_delete_text(int new_end);
void listener_append_and_prompt(char *msg);
void goto_listener(void);
void listener_append(char *msg);
void handle_listener(bool new_state);
int listener_height(void);
int listener_width(void);
void clear_listener(void);
bool highlight_unbalanced_paren(void);
void snd_completion_help(int matches, char **buffer);


/* -------- snd-xmenu.c -------- */

int file_open_menu(void);
int file_close_menu(void);
int file_save_menu(void);
int file_save_as_menu(void);
int file_print_menu(void);
int file_revert_menu(void);
int file_update_menu(void);
int file_mix_menu(void);
int file_view_menu(void);
int file_new_menu(void);
int edit_cut_menu(void);
int edit_paste_menu(void);
int edit_mix_menu(void);
int edit_play_menu(void);
int edit_save_as_menu(void);
int edit_undo_menu(void);
int edit_redo_menu(void);
int edit_find_menu(void);
int edit_select_all_menu(void);
int edit_header_menu(void);
int view_region_menu(void);
int view_combine_separate_menu(void);
int view_combine_combined_menu(void);
int view_combine_superimposed_menu(void);
int view_lines_menu(void);
int view_dots_menu(void);
int view_dots_and_lines_menu(void);
int view_filled_menu(void);
int view_lollipops_menu(void);
int view_zero_menu(void);
int view_ctrls_menu(void);
int view_listener_menu(void);
int view_cursor_menu(void);
int view_x_axis_seconds_menu(void);
int view_x_axis_beats_menu(void);
int view_x_axis_measures_menu(void);
int view_x_axis_samples_menu(void);
int view_x_axis_percentage_menu(void);
int view_no_axes_menu(void);
int view_all_axes_menu(void);
int view_all_axes_unlabelled_menu(void);
int view_just_x_axis_menu(void);
int view_just_x_axis_unlabelled_menu(void);
int options_save_state_menu(void);
int options_focus_left_menu(void);
int options_focus_right_menu(void);
int options_focus_middle_menu(void);
int options_focus_active_menu(void);
int popup_play_menu(void);
int popup_undo_menu(void);
int popup_redo_menu(void);
int popup_save_menu(void);
int popup_info_menu(void);
bool popup_menu_exists(void);
void set_menu_label(int w, const char *label);
void check_menu_labels(int key, int state, bool extended);
int g_add_to_main_menu(char *label, int slot);
widget_t g_add_to_menu(int which_menu, char *label, int callb, int position);
int g_remove_from_menu(int which_menu, char *label);
void reflect_play_stop_in_popup_menu(void);
void reflect_play_selection_stop(void);
int menu_widget(int which_menu);


/* -------- snd-xmain.c -------- */

color_t get_in_between_color(color_t fg, color_t bg);
void snd_doit(int argc, char **argv);
void auto_update_restart(void);
#ifdef SND_AS_WIDGET
  void snd_as_widget(int argc, char **argv, int app, int parent, int *caller_args, int caller_argn);
#endif


/* -------- snd-xfft.c -------- */

void set_fft_window_beta(Float val);
void set_transform_size(int val);
void set_fft_window(mus_fft_window_t val);
void set_wavelet_type(int val);
int fire_up_transform_dialog(bool managed);
bool transform_dialog_is_active(void);
void set_transform_type(int val);
void set_show_transform_peaks(bool val);
void set_fft_log_magnitude(bool val);
void set_fft_log_frequency(bool val);
void set_transform_normalization(fft_normalize_t val);
void set_show_selection_transform(bool show);
void set_transform_graph_type(graph_type_t val);

void reflect_peaks_in_transform_dialog(void);
void reflect_log_freq_start_in_transform_dialog(void);
void reflect_min_db_in_transform_dialog(void);


/* -------- snd-xregion.c -------- */

void update_region_browser(bool grf_too);
bool region_browser_is_active(void);
void delete_region_and_update_browser(int n);
void reflect_play_region_stop(int n);
bool region_dialog_is_active(void);
void allocate_region_rows(int n);
void reflect_regions_in_region_browser(void);
void reflect_no_regions_in_region_browser(void);
void reflect_region_graph_style(void);


/* -------- snd-xutils.c -------- */

void goto_window(int text);
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
int mark_name_width(char *txt);
void clear_window(axis_context *ax);
void set_title(const char *title);
void check_for_event(void);
void recolor_graph(chan_info *cp, bool selected);
void set_sensitive(int wid, bool val);
void set_toggle_button(int wid, bool val, bool passed, void *data);
int widget_height(int w);
int widget_width(int w);
void set_widget_size(int w, int width, int height);
int widget_x(int w);
int widget_y(int w);
void set_widget_x(int w, int x);
void set_widget_y(int w, int y);
void set_mix_color(int color);



/* -------- snd-xchn.c -------- */

int channel_w(chan_info *cp);
int channel_f(chan_info *cp);
int channel_graph(chan_info *cp);
bool channel_graph_is_visible(chan_info *cp);
void set_zx_scrollbar_value(chan_info *cp, Float value);
Float gsy_value(chan_info *cp);
Float gsy_size(chan_info *cp);
void initialize_scrollbars(chan_info *cp);
void resize_sx(chan_info *cp);
void resize_zx(chan_info *cp);
void resize_sy(chan_info *cp);
void resize_zy(chan_info *cp);
bool channel_open_pane(chan_info *cp, void *ptr);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
void set_peak_numbers_font(chan_info *cp);
void set_bold_peak_numbers_font(chan_info *cp);
void set_tiny_numbers_font(chan_info *cp);
color_t get_foreground_color(axis_context *ax);
color_t get_background_color(axis_context *ax);
void set_foreground_color(axis_context *ax, int color);
void cleanup_cw(chan_info *cp);
bool fixup_cp_cgx_ax_wn(chan_info *cp);
void get_current_color(int colormap, int j, unsigned short *r, unsigned short *g, unsigned short *b);
void change_channel_style(snd_info *sp, channel_style_t new_style);
int add_channel_window(snd_info *sound, int channel, int chan_y, int insertion, widget_t main, fw_button_t arrows, bool with_events);


/* -------- snd-xsnd.c -------- */

void make_minibuffer_label(snd_info *sp, char *str);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str, bool update);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
void snd_info_cleanup(snd_info *sp);
void set_amp(snd_info *sp, Float val);
void set_expand(snd_info *sp, Float val);
void set_contrast(snd_info *sp, Float val);
void set_speed(snd_info *sp, Float val);
void set_revlen(snd_info *sp, Float val);
void set_revscl(snd_info *sp, Float val);
void set_filter_order(snd_info *sp, int val);
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
void set_play_button(snd_info *sp, bool val);
void play_button_pause(bool pausing);
void syncb(snd_info *sp, int on);
void snd_file_lock_icon(snd_info *sp, bool on);
void snd_file_bomb_icon(snd_info *sp, bool on);
void x_bomb(snd_info *sp, bool on);
snd_info *add_sound_window (char *filename, bool read_only);
void set_sound_pane_file_label(snd_info *sp, char *str);
void reflect_amp_env_completion(snd_info *sp);
void sound_show_ctrls(snd_info *sp);
void sound_hide_ctrls(snd_info *sp);
bool control_panel_open(snd_info *sp);
void start_progress_report(snd_info *sp, enved_progress_t from_enved);
void finish_progress_report(snd_info *sp, enved_progress_t from_enved);
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, enved_progress_t from_enved);
void reflect_amp_env_in_progress(snd_info *sp);
void reflect_sound_selection(snd_info *sp);


/* -------- snd-xfile.c -------- */

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples, int min_chan);
void alert_new_file(void);
snd_info *make_new_file_dialog(char *newname, int header_type, int data_format, int srate, int chans, char *comment);
void make_cur_name_row(int old_size, int new_size);
void make_prev_name_row(int old_size, int new_size);
void make_prevfiles_list (void);
void make_curfiles_list (void);
void curfile_highlight(int i);
void set_file_browser_play_button(char *name, int state);
void highlight_selected_sound(void);
void set_file_sort_sensitive(bool sensitive);
int start_file_dialog(bool managed);
bool view_files_dialog_is_active(void);
int edit_header(snd_info *sp);
void save_edit_header_dialog_state(FILE *fd);
widget_t make_open_file_dialog(bool read_only, bool managed);
void set_open_file_play_button(bool val);
widget_t make_edit_save_as_dialog(bool managed);
widget_t make_file_save_as_dialog(bool managed);
widget_t make_mix_file_dialog(bool managed);
void clear_deleted_snd_info(struct file_dialog_info *fd);
widget_t post_it(const char *subject, const char *str);
void save_post_it_dialog_state(FILE *fd);
void reflect_just_sounds(void);
void save_file_dialog_state(FILE *fd);


/* -------- snd-xenv.c -------- */

axis_info *enved_make_axis(const char *name, axis_context *ax, int ex0, int ey0, int width, int height, 
			   Float xmin, Float xmax, Float ymin, Float ymax, printing_t printing);
void display_enved_env_with_selection(env *e, char *name, int x0, int y0, int width, int height, bool dots, printing_t printing);
void set_enved_redo_sensitive(bool val);
void set_enved_revert_sensitive(bool val);
void set_enved_undo_sensitive(bool val);
void set_enved_save_sensitive(bool val);
void set_enved_show_sensitive(bool val);
void make_scrolled_env_list (void);
void new_active_channel_alert(void);
void env_redisplay(void);
void env_redisplay_with_print(void);
int create_envelope_editor (void);
void set_enved_clip_p(bool val);
void reflect_enved_style(void);
void set_enved_base(Float val);
void set_enved_target(enved_target_t val);
void set_enved_wave_p(bool val);
void set_enved_in_dB(bool val);
bool enved_dialog_is_active(void);
void set_enved_filter_order(int order);
void enved_reflect_selection(bool on);



/* -------- snd-xmix.c -------- */

void reflect_mix_or_track_change(int mix_id, int track_id, bool forced);
int make_mix_dialog(void);
int make_track_dialog(void);
bool mix_play_stopped(void);
bool track_play_stopped(void);
void reflect_mix_play_stop(void);
void reflect_track_play_stop(void);
int mix_dialog_mix(void);
void mix_dialog_set_mix(int id);
int track_dialog_track(void);
void track_dialog_set_track(int id);
void show_track_background_wave(int pts, bool two_sided);


/* -------- snd-xrec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording (void);
widget_t snd_record_file(void);
bool record_dialog_is_active(void);
void reflect_amp_control_bounds_change_in_recorder(void);


/* -------- snd-gxutils.c -------- */
bool send_mozilla(const char *html_viewer, const char *url);


/* -------- snd-xprint.c -------- */
widget_t make_file_print_dialog(bool managed, bool direct_to_printer);
void save_print_dialog_state(FILE *fd);


#endif


