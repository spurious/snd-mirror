#ifndef SND_X1_H
#define SND_X1_H

typedef struct {
  Widget rw, nm, pl;
  int pos;
  file_viewer_t parent;
} regrow;


/* -------- snd-xhelp.c -------- */

Widget snd_help(const char *subject, const char *help, bool with_wrap);
Widget snd_help_with_xrefs(const char *subject, const char *helpstr, bool with_wrap, char **xrefs, char **urls);
int help_text_width(const char *txt, int start, int end);


/* -------- snd-xerror.c -------- */

void add_to_error_history(char *msg, bool popup);
void post_error_dialog(char *msg);
void show_snd_errors(void);

#ifdef __GNUC__
  bool snd_yes_or_no_p(const char *format, ...) __attribute__ ((format (printf, 1, 2)));
#else
  bool snd_yes_or_no_p(const char *format, ...);
#endif



/* -------- snd-xdraw.c -------- */

void draw_line (axis_context *ax, int x0, int y0, int x1, int y1);
void draw_lines (axis_context *ax, XPoint *points, int num);
void draw_points (axis_context *ax, XPoint *points, int num, int size);
void fill_rectangle (axis_context *ax, int x0, int y0, int width, int height);
void erase_rectangle (chan_info *cp, axis_context *ax, int x0, int y0, int width, int height);
void fill_polygon(axis_context *ax, int points, ...);
void draw_polygon(axis_context *ax, int points, ...);
void draw_string (axis_context *ax, int x0, int y0, char *str, int len);
void draw_arc(axis_context *ax, int x, int y, int size);
void set_grf_points(Locus xi, int j, Locus ymin, Locus ymax);
void set_grf_point(Locus xi, int j, Locus yi);
void draw_grf_points(chan_info *cp, axis_context *ax, int j, axis_info *ap, Float y0, graph_style_t graph_style);
void draw_both_grf_points(chan_info *cp, axis_context *ax, int j, graph_style_t graph_style);
void draw_both_grfs(axis_context *ax, int j);
void mix_save_graph(mix_context *ms, int j);
void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int j);
void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int j);
void setup_axis_context(chan_info *cp, axis_context *ax);
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(int colormap);
void initialize_colormap(void);
void allocate_sono_rects(int size);
void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height);
void draw_sono_rectangles(axis_context *ax, int color, int jmax);
Widget start_color_dialog(void);
Widget start_orientation_dialog(void);
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
bool set_with_gl(bool val);
void g_init_gxdraw(void);



/* -------- snd-xlistener.c -------- */

void textfield_focus_callback(Widget w, XtPointer context, XtPointer info);
void textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info);
void mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag);
void mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag);
void add_completer_to_textfield(Widget w, int completer);
void snd_completion_help(int matches, char **buffer);
void listener_append_and_prompt(char *msg);
void save_listener_text(FILE *fp);
void append_listener_text(int end, char *msg);
void goto_listener(void);
void color_listener(Pixel pix);
void color_listener_text(Pixel pix);
void listener_append(char *msg);
void handle_listener(bool new_state);
int listener_height(void);
int listener_width(void);
Widget make_togglebutton_widget(char *name, Widget parent, Arg *args, int n);
Widget make_pushbutton_widget(char *name, Widget parent, Arg *args, int n);
Widget make_text_widget(char *name, Widget parent, Arg *args, int n);
Widget make_textfield_widget(char *name, Widget parent, Arg *args, int n, text_cr_t activatable, int completer);
void clear_listener(void);
void lock_listener_pane(void);
void unlock_listener_pane(void);
void g_init_gxlistener(void);
bool highlight_unbalanced_paren(void);


/* -------- snd-xmenu.c -------- */

Widget file_open_menu(void);
Widget file_close_menu(void);
Widget file_save_menu(void);
Widget file_save_as_menu(void);
Widget file_print_menu(void);
Widget file_revert_menu(void);
Widget file_update_menu(void);
Widget file_mix_menu(void);
Widget file_view_menu(void);
Widget file_new_menu(void);
Widget edit_cut_menu(void);
Widget edit_paste_menu(void);
Widget edit_mix_menu(void);
Widget edit_play_menu(void);
Widget edit_save_as_menu(void);
Widget edit_undo_menu(void);
Widget edit_redo_menu(void);
Widget edit_find_menu(void);
Widget edit_select_all_menu(void);
Widget edit_header_menu(void);
Widget view_equalize_panes_menu(void);
Widget view_mix_dialog_menu(void);
Widget view_track_dialog_menu(void);
Widget view_region_menu(void);
Widget view_combine_separate_menu(void);
Widget view_combine_combined_menu(void);
Widget view_combine_superimposed_menu(void);
Widget view_lines_menu(void);
Widget view_dots_menu(void);
Widget view_dots_and_lines_menu(void);
Widget view_filled_menu(void);
Widget view_lollipops_menu(void);
Widget view_zero_menu(void);
Widget view_ctrls_menu(void);
Widget view_listener_menu(void);
Widget view_cursor_menu(void);
Widget view_x_axis_seconds_menu(void);
Widget view_x_axis_beats_menu(void);
Widget view_x_axis_samples_menu(void);
Widget view_x_axis_percentage_menu(void);
Widget options_save_state_menu(void);
Widget options_focus_left_menu(void);
Widget options_focus_right_menu(void);
Widget options_focus_middle_menu(void);
Widget options_focus_active_menu(void);
Widget options_speed_ratio_menu(void);
Widget options_speed_float_menu(void);
Widget options_speed_semitone_menu(void);
Widget popup_play_menu(void);
Widget popup_undo_menu(void);
Widget popup_redo_menu(void);
Widget popup_save_menu(void);
Widget popup_equalize_panes_menu(void);
Widget popup_info_menu(void);
bool popup_menu_exists(void);
void set_menu_label(Widget w, const char *label);
Widget add_menu(void);
void create_popup_menu(void);
void post_popup(XButtonPressedEvent *event);
void add_menu_drop(void);

int g_change_menu_label(int which_menu, char *old_label, char *new_label);
int g_set_menu_sensitive(int which_menu, char *old_label, bool on);
int g_add_to_main_menu(char *label, int slot);
int g_add_to_menu(int which_menu, char *label, int callb, int position);
int g_remove_from_menu(int which_menu, char *label);
int g_menu_is_sensitive(int which_menu, char *old_label);
void reflect_play_stop_in_popup_menu(void);
void reflect_play_selection_stop(void);
void g_init_gxmenu(void);
Widget menu_widget(int which_menu);
void check_menu_labels(int key, int state, bool extended);


/* -------- snd-xmain.c -------- */

void snd_doit(int argc, char **argv);
#ifdef SND_AS_WIDGET
  void snd_as_widget(int argc, char **argv, XtAppContext app, Widget parent, Arg *caller_args, int caller_argn);
#endif

void g_init_gxmain(void);



/* -------- snd-xfft.c -------- */

void set_fft_window_beta(Float val);
void set_transform_size(int val);
void set_fft_window(mus_fft_window_t val);
void set_transform_type(int val);
void set_wavelet_type(int val);
Widget fire_up_transform_dialog(bool managed);
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


/* -------- snd-xdrop.c -------- */

void add_drop(Widget w);
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
char **snd_icon_bits(void);
unsigned char *snd_plain_icon_bits(void);
char **blank_bits(void);
char **speed_l_bits(void);
char **speed_r_bits(void);
void make_icons_transparent(char *basic_color);


/* -------- snd-gxcolormaps.c -------- */

char **colormap_names(void);
void get_current_color(int colormap, int j, unsigned short *r, unsigned short *g, unsigned short *b);


/* -------- snd-xfind.c -------- */

void edit_find_callback(Widget w, XtPointer context, XtPointer info);
#if DEBUGGING
  void g_init_gxfind(void);
#endif


/* -------- snd-xutils.c -------- */

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
void map_over_children (Widget w, void (*func)(Widget w, void *ptr), void *userptr);
void clear_window(axis_context *ax);
void raise_dialog(Widget w);
void set_main_color_of_widget (Widget w, void *userptr);
void set_button_label(Widget label, const char *str);
void set_label(Widget label, const char *str);
void set_title(const char *title);
void goto_window(Widget text);
XtCallbackList make_callback_list(XtCallbackProc callback, XtPointer closure);
void color_sashes(Widget w, void *ptr);
void check_for_event(void);
bool event_pending(void);
void color_cursor(Pixel color);
void color_marks(Pixel color);
void color_selection(Pixel color);
void color_data(Pixel color);
void color_selected_data(Pixel color);
void color_graph(Pixel color);
void color_selected_graph(Pixel color);
void set_mix_color(Pixel color);
void recolor_graph(chan_info *cp, bool selected);
void reflect_resize(void);
void set_sensitive(Widget wid, bool val);
bool is_sensitive(Widget wid);
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


/* -------- snd-xchn.c -------- */

Widget channel_main_pane(chan_info *cp);
Widget channel_sx(chan_info *cp);
Widget channel_sy(chan_info *cp);
Widget channel_zx(chan_info *cp);
Widget channel_zy(chan_info *cp);
Widget channel_w(chan_info *cp);
Widget channel_f(chan_info *cp);
Widget channel_graph(chan_info *cp);
void set_zx_scrollbar_value(chan_info *cp, Float value);
void fixup_gsy(chan_info *cp, Float low, Float high);
Float gsy_value(chan_info *cp);
Float gsy_size(chan_info *cp);
void initialize_scrollbars(chan_info *cp);
void resize_sx(chan_info *cp);
void resize_zx(chan_info *cp);
void resize_sy(chan_info *cp);
void resize_zy(chan_info *cp);
bool channel_open_pane(chan_info *cp, void *ptr);
bool channel_unlock_pane(chan_info *cp, void *ptr);
bool channel_lock_pane(chan_info *cp, void *ptr);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
int add_channel_window(snd_info *sound, int channel, int chan_y, int insertion, Widget main, fw_button_t arrows, bool with_events);
void set_peak_numbers_font(chan_info *cp);
void set_bold_peak_numbers_font(chan_info *cp);
void set_tiny_numbers_font(chan_info *cp);
color_t get_foreground_color(chan_info *cp, axis_context *ax);
color_t get_background_color(chan_info *cp, axis_context *ax);
void set_foreground_color(chan_info *cp, axis_context *ax, Pixel color);
GC copy_GC(chan_info *cp);
GC erase_GC(chan_info *cp);
void graph_key_press(Widget w, XtPointer context, XEvent *event, Boolean *cont);
void cleanup_cw(chan_info *cp);
bool fixup_cp_cgx_ax_wn(chan_info *cp);
void change_channel_style(snd_info *sp, channel_style_t new_style);

void g_init_gxchn(void);


/* -------- snd-xsnd.c -------- */

int control_panel_height(snd_info *sp);
Widget w_snd_pane(snd_info *sp);
Widget w_snd_name(snd_info *sp);
Widget unite_button(snd_info *sp);
void set_control_panel_play_button(snd_info *sp, bool val);
Widget filter_graph(snd_info *sp);
void make_minibuffer_label(snd_info *sp, char *str);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
void sound_unlock_control_panel(snd_info *sp, void *ptr);
void sound_lock_control_panel(snd_info *sp, void *ptr);
void snd_info_cleanup(snd_info *sp);
void set_snd_amp(snd_info *sp, Float val);
void set_snd_expand(snd_info *sp, Float val);
void set_snd_contrast(snd_info *sp, Float val);
void set_snd_srate(snd_info *sp, Float val);
void set_snd_revlen(snd_info *sp, Float val);
void set_snd_revscl(snd_info *sp, Float val);
void set_snd_filter_order(snd_info *sp, int val);
void set_filter_text(snd_info *sp, char *str);
void toggle_expand_button(snd_info *sp, bool state);
void toggle_contrast_button(snd_info *sp, bool state);
void toggle_reverb_button(snd_info *sp, bool state);
void toggle_filter_button(snd_info *sp, bool state);
void toggle_direction_arrow(snd_info *sp, bool state);
void set_filter_in_dB(snd_info *sp, bool val);
void filter_env_changed(snd_info *sp, env *e);
void set_play_button(snd_info *sp, bool val);
void play_button_pause(bool pausing);
void syncb(snd_info *sp, int on);
void lock_apply(snd_info *sp);
void unlock_apply(snd_info *sp);
void set_apply_button(snd_info *sp, bool val);
void snd_file_lock_icon(snd_info *sp, bool on);
void snd_file_bomb_icon(snd_info *sp, bool on);
void x_bomb(snd_info *sp, bool on);
snd_info *add_sound_window (char *filename, bool read_only);
void set_sound_pane_file_label(snd_info *sp, char *str);
void unlock_control_panel(snd_info *sp);
void equalize_sound_panes(snd_info *sp, chan_info *ncp, bool all_panes);
void color_filter_waveform(Pixel color);
void reflect_amp_env_completion(snd_info *sp);
void reflect_amp_env_in_progress(snd_info *sp);
void equalize_all_panes(void);
void sound_show_ctrls(snd_info *sp);
void sound_hide_ctrls(snd_info *sp);
bool control_panel_open(snd_info *sp);
void show_controls(void);
void hide_controls(void);
void start_progress_report(snd_info *sp, enved_progress_t from_enved);
void finish_progress_report(snd_info *sp, enved_progress_t from_enved);
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, enved_progress_t from_enved);
XmString initial_speed_label(void);
void g_init_gxsnd(void);
void make_sound_icons_transparent_again(Pixel old_color, Pixel new_color);


/* -------- snd-xfile.c -------- */

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples);
file_data *make_file_data_panel(Widget parent, char *name, Arg *args, int n, bool with_chan, 
				int header_type, int data_format, bool with_loc, bool with_comment, bool with_samples);
void alert_new_file(void);
void make_open_file_dialog(bool read_only, bool managed);
void make_file_save_as_dialog(void);
void make_edit_save_as_dialog(void);
snd_info *make_new_file_dialog(char *newname, int header_type, int data_format, int srate, int chans, char *comment);
ww_info *make_title_row(Widget formw, char *top_str, char *main_str, dialog_pad_t pad, dialog_sort_t with_sort, dialog_paned_t with_pane);
regrow *make_regrow(Widget ww, Widget last_row, XtCallbackProc play_callback, XtCallbackProc name_callback);
void make_prevfiles_list (void);
void make_curfiles_list (void);
void curfile_highlight(int i);
void view_curfiles_set_row_name(int pos);
void make_cur_name_row(int old_size, int new_size);
void make_prev_name_row(int old_size, int new_size);
void set_file_browser_play_button(char *name, int state);
void set_file_sort_sensitive(bool sensitive);
void highlight_selected_sound(void);
void view_files_callback(Widget w, XtPointer context, XtPointer info);
Widget start_file_dialog(void);
bool file_dialog_is_active(void);
file_info *raw_data_dialog_to_file_info(const char *filename, const char *title);
Widget edit_header(snd_info *sp);
void set_open_file_play_button(bool val);
void make_mix_file_dialog(bool managed);
void g_init_gxfile(void);
void clear_deleted_snd_info(void *fd);
void post_it(const char *subject, const char *str);
void reflect_just_sounds_state(void);



/* -------- snd-xenv.c -------- */

axis_info *enved_make_axis(char *name, axis_context *ax, int ex0, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax, bool printing);
void display_enved_env_with_selection(env *e, char *name, 
				      int x0, int y0, int width, int height, bool dots, Float base, bool printing);
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
void display_enved_progress(char *str, Pixmap pix);
void set_enved_click_to_delete(bool n);
void enved_print(char *name);
Widget create_envelope_editor (void);
void set_enved_clip_p(bool val);
void set_enved_exp_p(bool val);
void set_enved_base(Float val);
void set_enved_target(enved_target_t val);
void set_enved_wave_p(bool val);
void set_enved_in_dB(bool val);
bool enved_dialog_is_active(void);
void set_enved_filter_order(int order);
void enved_reflect_selection(bool on);
void color_enved_waveform(Pixel pix);
void enved_fft_update(void);
void g_init_gxenv(void);



/* -------- snd-xmix.c -------- */

void reflect_mix_in_mix_dialog(int mix_id);
Widget make_mix_dialog(void);
Widget make_track_dialog(void);
bool mix_play_stopped(void);
void reflect_mix_play_stop(void);
void reflect_no_mix_in_mix_dialog(void);
void make_mixer_icons_transparent_again(Pixel old_color, Pixel new_color);
void reflect_undo_in_mix_dialog(void);
void reflect_track_in_track_dialog(int track_id);
void reflect_undo_in_track_dialog(void);
void reflect_no_track_in_track_dialog(void);
void reflect_track_play_stop(void);
void g_init_gxmix(void);



/* -------- snd-xrec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording (void);
void snd_record_file(void);
bool record_dialog_is_active(void);
void make_recorder_icons_transparent_again(Pixel old_color, Pixel new_color);
void recorder_error(char *msg);
void g_init_gxrec(void);


/* -------- snd-xprint.c -------- */

void file_print_callback(Widget w, XtPointer context, XtPointer info);


/* -------- snd-xxen.c -------- */


void recolor_button(widget_t w, void *ptr);
void color_chan_components(color_t color, slider_choice_t which_component);
void color_unselected_graphs(color_t color);
void g_init_gxen(void);

#endif


