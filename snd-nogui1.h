#ifndef SND_NOGUI1_H_LOADED
#define SND_NOGUI1_H_LOADED

/* -------- snd-xhelp.c -------- */

int snd_help(snd_state *ss, char *subject, char *help);
int snd_help_with_wrap(snd_state *ss, char *subject, char *help);
void snd_completion_help(snd_state *ss, int matches, char **buffer);


/* -------- snd-xerror.c -------- */

void add_to_error_history(snd_state *ss, char *msg, int popup);
void post_error_dialog(snd_state *ss, char *msg);
int snd_yes_or_no_p(snd_state *ss, char *format, ...);


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
void allocate_grf_points(void);
void draw_grf_points(chan_info *cp, axis_context *ax, int j, axis_info *ap, Float y0, int graph_style);
void draw_both_grf_points(chan_info *cp, axis_context *ax, int j, int graph_style);
void draw_both_grfs(axis_context *ax, int j);
void mix_save_graph(snd_state *ss, mix_context *ms, int j);
void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int j);
void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int j);
void setup_axis_context(chan_info *cp, axis_context *ax);
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(snd_state *ss, int colormap);
void allocate_sono_rects(snd_state *ss, int size);
void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height);
void draw_sono_rectangles(axis_context *ax, int color, int jmax);
int start_color_dialog(snd_state *ss, int width, int height);
int start_orientation_dialog(snd_state *ss, int width, int height);
void set_color_scale(snd_state *ss, Float val);
void set_color_inverted(snd_state *ss, int val);
void set_color_cutoff(snd_state *ss, Float val);
void set_color_map(snd_state *ss, int val);
void set_spectro_hop(snd_state *ss, int val);
void set_spectro_x_angle(snd_state *ss, Float val);
void set_spectro_y_angle(snd_state *ss, Float val);
void set_spectro_z_angle(snd_state *ss, Float val);
void set_spectro_x_scale(snd_state *ss, Float val);
void set_spectro_y_scale(snd_state *ss, Float val);
void set_spectro_z_scale(snd_state *ss, Float val);
void set_spectro_cutoff(snd_state *ss, Float val);
int color_dialog_is_active(void);
int orientation_dialog_is_active(void);
void reflect_spectro(snd_state *ss);


char *clm2snd_help(void);
void recorder_error(char *msg);
void reflect_record_size(int val);
void unsensitize_control_buttons(void);
void reflect_recorder_duration(Float new_dur);
void reflect_recorder_mixer_gain(int ind, Float val);
void reflect_recorder_out_amp(int ind, Float val);
void reflect_recorder_in_amp(int in, int out, Float val);




/* -------- snd-xlistener.c -------- */

void append_listener_text(int end, char *msg);
void save_listener_text(FILE *fp);
void listener_append_and_prompt(snd_state *ss, char *msg);
void goto_listener(void);
void listener_append(snd_state *ss, char *msg);
void handle_listener(snd_state *ss, int new_state);
int listener_height(void);
int listener_width(void);



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
int view_equalize_panes_menu(void);
int view_mix_panel_menu(void);
int view_region_menu(void);
int view_combine_separate_menu(void);
int view_combine_combined_menu(void);
int view_combine_superimposed_menu(void);
int view_lines_menu(void);
int view_dots_menu(void);
int view_dots_and_lines_menu(void);
int view_filled_menu(void);
int view_lollipops_menu(void);
int view_marks_menu(void);
int view_zero_menu(void);
int view_ctrls_menu(void);
int view_listener_menu(void);
int view_cursor_menu(void);
int view_x_axis_seconds_menu(void);
int view_x_axis_beats_menu(void);
int view_x_axis_samples_menu(void);
int view_x_axis_percentage_menu(void);
int options_save_state_menu(void);
int options_focus_left_menu(void);
int options_focus_right_menu(void);
int options_focus_middle_menu(void);
int options_focus_active_menu(void);
int options_speed_ratio_menu(void);
int options_speed_float_menu(void);
int options_speed_semitone_menu(void);
int popup_play_menu(void);
int popup_undo_menu(void);
int popup_redo_menu(void);
int popup_save_menu(void);
int popup_equalize_panes_menu(void);
int popup_info_menu(void);
int popup_menu_exists(void);
void set_menu_label(int w, const char *label);

int g_change_menu_label(int which_menu, char *old_label, char *new_label);
int g_set_menu_sensitive(int which_menu, char *old_label, int on);
int g_menu_is_sensitive(int which_menu, char *old_label);
int g_add_to_main_menu(snd_state *ss, char *label, int slot);
int g_add_to_menu(snd_state *ss, int which_menu, char *label, int callb, int position);
int g_remove_from_menu(int which_menu, char *label);
void reflect_play_stop_in_popup_menu(void);
void reflect_play_selection_stop(void);
int menu_widget(int which_menu);


/* -------- snd-xmain.c -------- */

void dismiss_all_dialogs(snd_state *ss);
void snd_doit(snd_state *state, int argc, char **argv);
#ifdef SND_AS_WIDGET
  void snd_as_widget(int argc, char **argv, int app, int parent, int *caller_args, int caller_argn);
#endif


/* -------- snd-xfft.c -------- */

void set_fft_window_beta(snd_state *ss, Float val);
void set_transform_size(snd_state *ss, int val);
void set_fft_window(snd_state *ss, int val);
void set_transform_type(snd_state *ss, int val);
void set_wavelet_type(snd_state *ss, int val);
int fire_up_transform_dialog(snd_state *ss, int managed);
int transform_dialog_is_active(void);

char *transform_type_name(int choice);
int add_transform_to_list(char *name);
int max_transform_type(void);

void set_show_transform_peaks(snd_state *ss, int val);
void set_fft_log_magnitude(snd_state *ss, int val);
void set_fft_log_frequency(snd_state *ss, int val);
void set_transform_normalization(snd_state *ss, int val);
void set_show_selection_transform(snd_state *ss, int show);
void set_transform_graph_type(snd_state *ss, int val);



/* -------- snd-xregion.c -------- */

void intern_atoms (snd_state *ss);
void update_region_browser(snd_state *ss, int grf_too);
int region_browser_is_active(void);
void delete_region_and_update_browser(snd_state *ss, int n);
void reflect_play_region_stop(int n);
void set_region_protect(int reg, int protect);
int region_dialog_is_active(void);
void allocate_region_rows(snd_state *ss, int n);
void reflect_regions_in_region_browser(void);
void reflect_no_regions_in_region_browser(void);
void reflect_region_graph_style(snd_state *ss);


/* -------- snd-xutils.c -------- */

void goto_window(int text);
int set_help_text_font(snd_state *ss, char *font);
int set_tiny_font(snd_state *ss, char *font);
int set_listener_font(snd_state *ss, char *font);
int set_button_font(snd_state *ss, char *font);
int set_bold_button_font(snd_state *ss, char *font);
int set_axis_label_font(snd_state *ss, char *font);
int set_axis_numbers_font(snd_state *ss, char *font);
void activate_numbers_font(axis_context *ax);
void activate_label_font(axis_context *ax);
void activate_button_font(axis_context *ax, snd_state *ss);
int label_width(axis_context *ax, char *txt);
int number_width(axis_context *ax, char *num);
int number_height(axis_context *ax);
int label_height(axis_context *ax);
int mark_name_width(snd_state *ss, char *txt);
void clear_window(axis_context *ax);
void highlight_color(snd_state *ss, int w);
void white_color(snd_state *ss, int w);
void set_title(snd_state *ss, const char *title);
void check_for_event(snd_state *ss);
int event_pending(snd_state *ss);
void recolor_graph(chan_info *cp, int selected);
void reflect_resize(snd_state *ss);
void set_sensitive(int wid, int val);
void set_toggle_button(int wid, int val, int passed, void *data);
int widget_height(int w);
int widget_width(int w);
void set_widget_height(int w, int height);
void set_widget_width(int w, int width);
void set_widget_size(int w, int width, int height);
int widget_x(int w);
int widget_y(int w);
void set_widget_x(int w, int x);
void set_widget_y(int w, int y);
void fixup_axis_context(axis_context *ax, int w, int gc);


/* -------- snd-xchn.c -------- */

int channel_w(chan_info *cp);
int channel_f(chan_info *cp);
int channel_graph(chan_info *cp);
void set_zx_scrollbar_value(chan_info *cp, Float value);
Float gsy_value(chan_info *cp);
Float gsy_size(chan_info *cp);
void initialize_scrollbars(chan_info *cp);
void resize_sx(chan_info *cp);
void resize_zx(chan_info *cp);
void resize_sy(chan_info *cp);
void resize_zy(chan_info *cp);
int channel_open_pane(chan_info *cp, void *ptr);
int channel_unlock_pane(chan_info *cp, void *ptr);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
void reflect_save_as_in_edit_history(chan_info *cp, char *filename);
void set_peak_numbers_font(chan_info *cp);
void set_bold_peak_numbers_font(chan_info *cp);
void set_tiny_numbers_font(chan_info *cp);
COLOR_TYPE get_foreground_color(chan_info *cp, axis_context *ax);
COLOR_TYPE get_background_color(chan_info *cp, axis_context *ax);
void set_foreground_color(chan_info *cp, axis_context *ax, int color);
void cleanup_cw(chan_info *cp);
void combine_sound(snd_info *sp);
void separate_sound(snd_info *sp);
void superimpose_sound(snd_info *sp);
int fixup_cp_cgx_ax_wn(chan_info *cp);
void get_current_color(int colormap, int j, unsigned short *r, unsigned short *g, unsigned short *b);
void change_channel_style(snd_info *sp, int new_style);
void add_channel_window(snd_info *sound, int channel, snd_state *ss, int chan_y, int insertion, GUI_WIDGET main, int arrows);


/* -------- snd-xsnd.c -------- */

int w_snd_name(snd_info *sp);
int filter_graph(snd_info *sp);
void make_minibuffer_label(snd_info *sp, char *str);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
void snd_info_cleanup(snd_info *sp);
void set_snd_amp(snd_info *sp, Float val);
void set_snd_expand(snd_info *sp, Float val);
void set_snd_contrast(snd_info *sp, Float val);
void set_snd_srate(snd_info *sp, Float val);
void set_snd_revlen(snd_info *sp, Float val);
void set_snd_revscl(snd_info *sp, Float val);
void set_snd_filter_order(snd_info *sp, int val);
void set_filter_text(snd_info *sp, char *str);
void toggle_expand_button(snd_info *sp, int state);
void toggle_contrast_button(snd_info *sp, int state);
void toggle_reverb_button(snd_info *sp, int state);
void toggle_filter_button(snd_info *sp, int state);
void toggle_direction_arrow(snd_info *sp, int state);
void sp_display_env(snd_info *sp);
void set_filter_in_dB(snd_info *sp, int val);
void filter_env_changed(snd_info *sp, env *e);
void set_play_button(snd_info *sp, int val);
void play_button_pause(snd_state *ss, int pausing);
void syncb(snd_info *sp, int on);
void combineb(snd_info *sp, int val);
void lock_apply(snd_state *ss, snd_info *sp);
void unlock_apply(snd_state *ss, snd_info *sp);
void set_apply_button(snd_info *sp, int val);
void snd_file_lock_icon(snd_info *sp, int on);
void snd_file_bomb_icon(snd_info *sp, int on);
void x_bomb(snd_info *sp, int on);
snd_info *add_sound_window (char *filename, snd_state *state, int read_only);
void set_sound_pane_file_label(snd_info *sp, char *str);
void equalize_sound_panes(snd_state *ss, snd_info *sp, chan_info *ncp, int all_panes);
void reflect_amp_env_completion(snd_info *sp);
void equalize_all_panes(snd_state *ss);
void sound_show_ctrls(snd_info *sp);
void sound_hide_ctrls(snd_info *sp);
int control_panel_open(snd_info *sp);
void start_progress_report(snd_info *sp, int from_enved);
void finish_progress_report(snd_info *sp, int from_enved);
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, int from_enved);
void reflect_amp_env_in_progress(snd_info *sp);


/* -------- snd-xfile.c -------- */

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, int *location);
void alert_new_file(void);
snd_info *make_new_file_dialog(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *comment);
void make_cur_name_row(int old_size, int new_size);
void make_prev_name_row(int old_size, int new_size);
void make_prevfiles_list (snd_state *ss);
void make_curfiles_list (snd_state *ss);
void view_curfiles_set_row_name(int pos);
void curfile_highlight(snd_state *ss, int i);
void set_file_browser_play_button(char *name, int state);
void highlight_selected_sound(snd_state *ss);
void set_file_sort_sensitive(int sensitive);
int start_file_dialog(snd_state *ss, int width, int height);
int file_dialog_is_active(void);
int edit_header(snd_info *sp);
void make_a_big_star_outa_me(char *shortname, int big_star);
void make_open_file_dialog(snd_state *ss, int read_only, int managed);
void set_open_file_play_button(int val);
void make_edit_save_as_dialog(snd_state *ss);
void make_file_save_as_dialog(snd_state *ss);
void make_mix_file_dialog(snd_state *ss, int managed);


/* -------- snd-xenv.c -------- */

chan_info *enved_make_axis_cp(snd_state *ss, char *name, axis_context *ax, int ex0, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax);
void display_enved_env_with_selection(snd_state *ss, env *e, char *name, int x0, int y0, int width, int height, int dots, Float base);
void set_enved_redo_sensitive(int val);
void set_enved_revert_sensitive(int val);
void set_enved_undo_sensitive(int val);
void set_enved_save_sensitive(int val);
void set_enved_show_sensitive(int val);
void make_scrolled_env_list (snd_state *ss);
void new_active_channel_alert(snd_state *ss);
void env_redisplay(snd_state *ss);
void enved_display_point_label(snd_state *ss, Float x, Float y);
void set_enved_click_to_delete(int n);
int create_envelope_editor (snd_state *ss);
void set_enved_clip_p(snd_state *ss, int val);
void set_enved_exp_p(snd_state *ss, int val);
void set_enved_base(snd_state *ss, Float val);
void set_enved_target(snd_state *ss, int val);
void set_enved_wave_p(snd_state *ss, int val);
void set_enved_in_dB(snd_state *ss, int val);
int enved_dialog_is_active(void);
void set_enved_filter_order(snd_state *ss, int order);
void enved_reflect_selection(int on);
void reflect_mix_in_enved(void);
void enved_fft_update(void);



/* -------- snd-xmix.c -------- */

void reflect_mix_in_mix_panel(int mix_id);
int make_mix_panel(snd_state *ss);
int mix_play_stopped(void);
void reflect_mix_play_stop(void);
void reflect_no_mix_in_mix_panel(void);


/* -------- snd-xrec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording (void);
void snd_record_file(snd_state *ss);
int record_dialog_is_active(void);


#endif


