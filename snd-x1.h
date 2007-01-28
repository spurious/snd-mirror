#ifndef SND_X1_H
#define SND_X1_H

#define SOUND_ENV_EDITOR(Sp) ((env_editor *)(sp->sgx->flt))

/* -------- snd-xhelp.c -------- */

Widget snd_help(const char *subject, const char *help, with_word_wrap_t with_wrap);
Widget snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, char **xrefs, char **urls);
int help_text_width(const char *txt, int start, int end);
void snd_help_append(const char *text);
void snd_help_back_to_top(void);
bool help_dialog_is_active(void);


/* -------- snd-xdraw.c -------- */

void draw_line(axis_context *ax, int x0, int y0, int x1, int y1);
void draw_lines(axis_context *ax, XPoint *points, int num);
void draw_points(axis_context *ax, XPoint *points, int num, int size);
void fill_rectangle(axis_context *ax, int x0, int y0, int width, int height);
void erase_rectangle(chan_info *cp, axis_context *ax, int x0, int y0, int width, int height);
void fill_polygon(axis_context *ax, int points, ...);
void draw_polygon(axis_context *ax, int points, ...);
void draw_string(axis_context *ax, int x0, int y0, const char *str, int len);
void gtk_style_draw_string(axis_context *ax, int x0, int y0, const char *str, int len);
void draw_arc(axis_context *ax, int x, int y, int size);
void set_grf_points(Locus xi, int j, Locus ymin, Locus ymax);
void set_grf_point(Locus xi, int j, Locus yi);
void draw_grf_points(int dot_size, axis_context *ax, int j, axis_info *ap, Float y0, graph_style_t graph_style);
void draw_both_grf_points(int dot_size, axis_context *ax, int j, graph_style_t graph_style);
void mix_save_graph(mix_context *ms, int j);
void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int j);
void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int j);
void setup_axis_context(chan_info *cp, axis_context *ax);
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(int colormap);
void check_colormap_sizes(int size);
void initialize_colormap(void);
void reflect_color_list(bool setup_time);
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
void view_orientation_callback(Widget w, XtPointer context, XtPointer info);
void view_color_callback(Widget w, XtPointer context, XtPointer info);
bool color_dialog_is_active(void);
bool orientation_dialog_is_active(void);
void reflect_spectro(void);
void set_with_gl(bool val);
void g_init_gxdraw(void);



/* -------- snd-xlistener.c -------- */

void textfield_focus_callback(Widget w, XtPointer context, XtPointer info);
void textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info);
void mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag);
void mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag);
void add_completer_to_textfield(Widget w, int completer);
void snd_completion_help(int matches, char **buffer);
void listener_delete_text(int new_end);
void listener_append_and_prompt(const char *msg);
int save_listener_text(FILE *fp);
void append_listener_text(int end, const char *msg);
void goto_listener(void);
void color_listener(Pixel pix);
void color_listener_text(Pixel pix);
void listener_append(const char *msg);
void handle_listener(bool new_state);
bool listener_exists(void);
int listener_height(void);
int listener_width(void);
Widget make_togglebutton_widget(char *name, Widget parent, Arg *args, int n);
Widget make_pushbutton_widget(char *name, Widget parent, Arg *args, int n);
Widget make_text_widget(char *name, Widget parent, Arg *args, int n);
Widget make_textfield_widget(char *name, Widget parent, Arg *args, int n, text_cr_t activatable, int completer);
void clear_listener(void);
void set_listener_text_font(void);
void lock_listener_pane(void);
void unlock_listener_pane(void);
void g_init_gxlistener(void);
bool highlight_unbalanced_paren(void);


/* -------- snd-xmenu.c -------- */

Widget add_menu(void);
void create_popup_menu(void);
void add_menu_drop(void);
void post_popup(XButtonPressedEvent *event);
Widget menu_widget(int which_menu);
void check_menu_labels(int key, int state, bool extended);
void reflect_play_stop_in_popup_menu(void);
void reflect_play_selection_stop(void);
void g_init_gxmenu(void);
void set_button_label(Widget label, const char *str);


/* -------- snd-xmain.c -------- */

void snd_doit(int argc, char **argv);
color_t get_in_between_color(color_t fg, color_t bg);
void auto_update_restart(void);
void save_colors(FILE *Fp);

#ifdef SND_AS_WIDGET
  void snd_as_widget(int argc, char **argv, XtAppContext app, Widget parent, Arg *caller_args, int caller_argn);
#endif

void g_init_gxmain(void);



/* -------- snd-xfft.c -------- */

void set_fft_window_beta(Float val);
void set_fft_window_alpha(Float val);
void set_transform_size(int val);
void set_fft_window(mus_fft_window_t val);
void set_wavelet_type(int val);
Widget fire_up_transform_dialog(bool managed);
bool transform_dialog_is_active(void);

void set_transform_type(int val);
void make_transform_type_list(void);

void set_show_transform_peaks(bool val);
void set_fft_log_magnitude(bool val);
void set_fft_log_frequency(bool val);
void set_transform_normalization(fft_normalize_t val);
void set_show_selection_transform(bool show);
void set_transform_graph_type(graph_type_t val);
void reflect_peaks_in_transform_dialog(void);
void reflect_log_freq_start_in_transform_dialog(void);
void reflect_min_db_in_transform_dialog(void);


/* -------- snd-xdrop.c -------- */

void add_drag_and_drop(Widget w, 
		       void (*drop_watcher)(Widget w, const char *message, Position x, Position y, void *data), 
		       void (*drag_watcher)(Widget w, const char *message, Position x, Position y, drag_style_t dtype, void *data), 
		       void *context);
void g_init_gxdrop(void);



/* -------- snd-xregion.c -------- */

void update_region_browser(bool grf_too);
bool region_browser_is_active(void);
void delete_region_and_update_browser(int n);
void reflect_play_region_stop(int n);
void view_region_callback(Widget w, XtPointer context, XtPointer info);
bool region_dialog_is_active(void);
void allocate_region_rows(int n);
void reflect_regions_in_region_browser(void);
void reflect_no_regions_in_region_browser(void);
void reflect_region_graph_style(void);
int region_dialog_region(void);
char *regrow_get_label(void *ur);
int regrow_get_pos(void *ur);
void g_init_gxregion(void);


/* -------- snd-gxutils -------- */

bool send_mozilla(const char *html_viewer, const char *url);
void g_init_gxutils(void);



/* -------- snd-gxbitmaps.c -------- */

char **mini_bomb_bits(int n);
char **mini_glass_bits(int n);
char **mini_lock_bits(void);
char **snd_icon_bits(void);
unsigned char *snd_plain_icon_bits(void);
char **blank_bits(void);
char **speed_l_bits(void);
char **speed_r_bits(void);
void make_icons_transparent(char *basic_color);
char **stop_sign_bits(void);


/* -------- snd-gxcolormaps.c -------- */

char *colormap_name(int n);
bool is_colormap(int n);
int num_colormaps(void);
void get_current_color(int colormap, int n, unsigned short *r, unsigned short *g, unsigned short *b);
void g_init_gxcolormaps(void);


/* -------- snd-xfind.c -------- */

void edit_find_callback(Widget w, XtPointer context, XtPointer info);
void set_find_dialog_label(const char *str);
void save_find_dialog_state(FILE *fd);
void g_init_gxfind(void);


/* -------- snd-xutils.c -------- */

bool set_tiny_font(const char *font);
bool set_listener_font(const char *font);
bool set_peaks_font(const char *font);
bool set_bold_peaks_font(const char *font);
bool set_axis_label_font(const char *font);
bool set_axis_numbers_font(const char *font);
int label_width(const char *txt, bool use_tiny_font);
int number_width(const char *num, bool use_tiny_font);
int number_height(bool use_tiny_font);
int label_height(bool use_tiny_font);
int mark_name_width(const char *txt);
void map_over_children(Widget w, void (*func)(Widget w));
void map_over_children_with_color(Widget w, void (*func)(Widget uw, color_t color), color_t color);
void clear_window(axis_context *ax);
void raise_dialog(Widget w);
void set_main_color_of_widget(Widget w);
char *get_label(Widget label);
void set_label(Widget label, const char *str);
void set_title(const char *title);
void goto_window(Widget text);
XtCallbackList make_callback_list(XtCallbackProc callback, XtPointer closure);
void color_sashes(Widget w);
void check_for_event(void);
void color_cursor(Pixel color);
void color_marks(Pixel color);
void color_selection(Pixel color);
void color_data(Pixel color);
void color_selected_data(Pixel color);
void color_graph(Pixel color);
void color_selected_graph(Pixel color);
void set_mix_color(Pixel color);
void recolor_graph(chan_info *cp, bool selected);
void set_sensitive(Widget wid, bool val);
void set_toggle_button(Widget wid, bool val, bool passed, void *data);
Dimension widget_height(Widget w);
Dimension widget_width(Widget w);
void set_widget_height(Widget w, Dimension height);
void set_widget_width(Widget w, Dimension width);
Position widget_x(Widget w);
Position widget_y(Widget w);
void set_widget_x(Widget w, Position x);
void set_widget_y(Widget w, Position y);
void set_widget_size(Widget w, Dimension width, Dimension height);
void set_widget_position(Widget w, Position x, Position y);
void fixup_axis_context(axis_context *ax, Widget w, GC gc);
Pixmap make_pixmap(unsigned char *bits, int width, int height, int depth, GC gc);
Cessator add_work_proc(XtWorkProc func, XtPointer data);
int attach_all_sides(Arg *args, int n);
void widget_int_to_text(Widget w, int val);
void widget_float_to_text(Widget w, Float val);
void widget_off_t_to_text(Widget w, off_t val);
Pixmap rotate_text(Widget w, const char *str, XFontStruct *font, Float angle_in_degrees, int *nw, int *nh, Pixel bg, Pixel fg, GC d_gc);
void draw_rotated_axis_label(chan_info *cp, GC gc, const char *text, int x0, int y0);
void ensure_list_row_visible(widget_t list, int row);
void ensure_scrolled_window_row_visible(widget_t list, int pos, int num_rows);
XmString multi_line_label(const char *s, int *lines);


/* -------- snd-xchn.c -------- */

Widget channel_main_pane(chan_info *cp);
Widget channel_sx(chan_info *cp);
Widget channel_sy(chan_info *cp);
Widget channel_zx(chan_info *cp);
Widget channel_zy(chan_info *cp);
Widget channel_w(chan_info *cp);
Widget channel_f(chan_info *cp);
Widget channel_graph(chan_info *cp);
bool channel_graph_is_visible(chan_info *cp);
void set_zx_scrollbar_value(chan_info *cp, Float value);
void fixup_gsy(chan_info *cp, Float low, Float high);
Float gsy_value(chan_info *cp);
Float gsy_size(chan_info *cp);
void initialize_scrollbars(chan_info *cp);
void resize_sx(chan_info *cp);
void resize_zx(chan_info *cp);
void resize_sy(chan_info *cp);
void resize_zy(chan_info *cp);
bool channel_open_pane(chan_info *cp);
bool channel_unlock_pane(chan_info *cp);
bool channel_lock_pane(chan_info *cp, int height);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
int add_channel_window(snd_info *sound, int channel, int chan_y, int insertion, Widget main, fw_button_t arrows, bool with_events);
void set_peak_numbers_font(chan_info *cp);
void set_bold_peak_numbers_font(chan_info *cp);
void set_tiny_numbers_font(chan_info *cp);
color_t get_foreground_color(axis_context *ax);
color_t get_background_color(axis_context *ax);
void set_foreground_color(axis_context *ax, Pixel color);
GC copy_GC(chan_info *cp);
GC erase_GC(chan_info *cp);
void graph_key_press(Widget w, XtPointer context, XEvent *event, Boolean *cont);
void cleanup_cw(chan_info *cp);
void free_fft_pix(chan_info *cp);
bool restore_fft_pix(chan_info *cp, axis_context *ax);
void save_fft_pix(chan_info *cp, axis_context *ax, int fwidth, int fheight, int x0, int y1);
bool fixup_cp_cgx_ax_wn(chan_info *cp);
void change_channel_style(snd_info *sp, channel_style_t new_style);

void g_init_gxchn(void);


/* -------- snd-xsnd.c -------- */

int control_panel_height(snd_info *sp);
Widget w_snd_pane(snd_info *sp);
Widget unite_button(snd_info *sp);
void set_control_panel_play_button(snd_info *sp);
void make_minibuffer_label(snd_info *sp, char *str);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str, bool update);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
void snd_info_cleanup(snd_info *sp);
void set_amp(snd_info *sp, Float val);
int amp_to_scroll(Float minval, Float val, Float maxval);
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
void show_lock(snd_info *sp);
void hide_lock(snd_info *sp);
void start_bomb(snd_info *sp);
void stop_bomb(snd_info *sp);
void show_bomb(snd_info *sp);
void hide_bomb(snd_info *sp);
snd_info *add_sound_window(char *filename, bool read_only, file_info *hdr);
void set_sound_pane_file_label(snd_info *sp, char *str);
void equalize_sound_panes(snd_info *sp, chan_info *ncp, bool all_panes);
void color_filter_waveform(Pixel color);
void equalize_all_panes(void);
void show_controls(snd_info *sp);
void hide_controls(snd_info *sp);
bool showing_controls(snd_info *sp);
void show_all_controls(void);
void hide_all_controls(void);
void start_progress_report(snd_info *sp, enved_progress_t from_enved);
void finish_progress_report(snd_info *sp, enved_progress_t from_enved);
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, enved_progress_t from_enved);
XmString initial_speed_label(speed_style_t style);
void g_init_gxsnd(void);
void make_sound_icons_transparent_again(Pixel old_color, Pixel new_color);
void reflect_sound_selection(snd_info *sp);
void display_minibuffer_error(snd_info *sp, const char *str);
void clear_minibuffer_error(snd_info *sp);


/* -------- snd-xfile.c -------- */

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples, int min_chan);
file_data *make_file_data_panel(Widget parent, const char *name, Arg *args, int n, dialog_channels_t with_chan, 
				int header_type, int data_format, dialog_data_location_t with_loc, 
				dialog_samples_t with_samples, dialog_error_t with_error, 
				dialog_header_type_t with_header_type, dialog_comment_t with_comment,
				header_choice_t header_choice);
void alert_new_file(void);
widget_t make_open_file_dialog(bool read_only, bool managed);
widget_t make_sound_save_as_dialog(bool managed);
widget_t make_selection_save_as_dialog(bool managed);
widget_t make_region_save_as_dialog(bool managed);
widget_t make_new_file_dialog(bool managed);
void mouse_enter_label(void *r, int type);
void mouse_leave_label(void *r, int type);
Widget edit_header(snd_info *sp);
void save_edit_header_dialog_state(FILE *fd);
void cleanup_edit_header_watcher(void);
void cleanup_new_file_watcher(void);
void set_open_file_play_button(bool val);
widget_t make_mix_file_dialog(bool managed);
widget_t make_insert_file_dialog(bool managed);
void g_init_gxfile(void);
void clear_deleted_snd_info(struct dialog_play_info *dp);
void reflect_just_sounds(void);
void save_file_dialog_state(FILE *fd);
widget_t post_it(const char *subject, const char *str);
void save_post_it_dialog_state(FILE *fd);
void reflect_region_in_save_as_dialog(void);


/* -------- snd-xenv.c -------- */

axis_info *enved_make_axis(const char *name, axis_context *ax, int ex0, int ey0, int width, int height, 
			   Float xmin, Float xmax, Float ymin, Float ymax, printing_t printing);
void display_enved_env_with_selection(env *e, const char *name, int x0, int y0, int width, int height, bool dots, printing_t printing);
void set_enved_redo_sensitive(bool val);
void set_enved_revert_sensitive(bool val);
void set_enved_undo_sensitive(bool val);
void set_enved_save_sensitive(bool val);
void set_enved_show_sensitive(bool val);
void make_scrolled_env_list(void);
void enved_reflect_amp_env_completion(snd_info *sp);
void new_active_channel_alert(void);
void env_redisplay(void);
void env_redisplay_with_print(void);
void enved_display_point_label(Float x, Float y);
void display_enved_progress(char *str, Pixmap pix);
void enved_print(char *name);
Widget create_envelope_editor(void);
void set_enved_clip_p(bool val);
void reflect_enved_style(void);
void set_enved_base(Float val);
void set_enved_target(enved_target_t val);
void set_enved_wave_p(bool val);
void set_enved_in_dB(bool val);
bool enved_dialog_is_active(void);
void set_enved_filter_order(int order);
void color_enved_waveform(Pixel pix);
void g_init_gxenv(void);



/* -------- snd-xmix.c -------- */

void reflect_mix_or_track_change(int mix_id, int track_id, bool forced);
Widget make_mix_dialog(void);
Widget make_track_dialog(void);
bool mix_play_stopped(void);
bool track_play_stopped(void);
void reflect_mix_play_stop(void);
void reflect_track_play_stop(void);
void make_mixer_icons_transparent_again(Pixel old_color, Pixel new_color);
int mix_dialog_mix(void);
void mix_dialog_set_mix(int id);
int track_dialog_track(void);
void track_dialog_set_track(int id);
void show_track_background_wave(int pts, bool two_sided);



/* -------- snd-xrec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording(void);
widget_t snd_record_file(void);
bool record_dialog_is_active(void);
void make_recorder_icons_transparent_again(Pixel new_color);
void reflect_amp_control_bounds_change_in_recorder(void);
void g_init_gxrec(void);


/* -------- snd-xprint.c -------- */

void file_print_callback(Widget w, XtPointer context, XtPointer info);
widget_t make_file_print_dialog(bool managed, bool direct_to_printer);
void save_print_dialog_state(FILE *fd);


/* -------- snd-xxen.c -------- */

void color_chan_components(color_t color, slider_choice_t which_component);
void color_unselected_graphs(color_t color);
void g_init_gxen(void);


/* -------- snd-xprefs.c -------- */

widget_t start_preferences_dialog(void);

#endif


