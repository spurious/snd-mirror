#ifndef SND_X1_H_LOADED
#define SND_X1_H_LOADED

typedef struct {
  Widget rw, nm, pl, sv;
  int pos, parent;
  snd_state *ss;
} regrow;


/* -------- snd-xhelp.c -------- */

Widget snd_help(snd_state *ss, char *subject, char *help);
Widget snd_help_with_wrap(snd_state *ss, char *subject, char *helpstr);


/* -------- snd-xerror.c -------- */

void add_to_error_history(snd_state *ss, char *msg, int popup);
void post_error_dialog(snd_state *ss, char *msg);
void show_snd_errors(snd_state *ss);

#ifdef __GNUC__
  int snd_yes_or_no_p(snd_state *ss, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
#else
  int snd_yes_or_no_p(snd_state *ss, const char *format, ...);
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
void initialize_colormap(snd_state *ss);
void allocate_sono_rects(snd_state *ss, int size);
void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height);
void draw_sono_rectangles(axis_context *ax, int color, int jmax);
Widget start_color_dialog(snd_state *ss, int width, int height);
Widget start_orientation_dialog(snd_state *ss, int width, int height);
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
void view_orientation_callback(Widget w, XtPointer clientData, XtPointer callData);
void view_color_callback(Widget w, XtPointer clientData, XtPointer callData);
int color_dialog_is_active(void);
int orientation_dialog_is_active(void);
void reflect_spectro(snd_state *ss);



/* -------- snd-xlistener.c -------- */

void textfield_focus_callback(Widget w, XtPointer clientData, XtPointer callData);
void textfield_unfocus_callback(Widget w, XtPointer clientData, XtPointer callData);
void add_completer_to_textfield(snd_state *ss, Widget w, int completer);
void snd_completion_help(snd_state *ss, int matches, char **buffer);
void listener_append_and_prompt(snd_state *ss, char *msg);
void save_listener_text(FILE *fp);
void append_listener_text(int end, char *msg);
void goto_listener(void);
void color_listener(Pixel pix);
void color_listener_text(Pixel pix);
void listener_append(snd_state *ss, char *msg);
void handle_listener(snd_state *ss, int new_state);
int listener_height(void);
int listener_width(void);
Widget make_togglebutton_widget(char *name, Widget parent, Arg *args, int n);
Widget make_pushbutton_widget(char *name, Widget parent, Arg *args, int n);
Widget make_text_widget(snd_state *ss, char *name, Widget parent, Arg *args, int n);
Widget make_textfield_widget(snd_state *ss, char *name, Widget parent, Arg *args, int n, int activatable, int completer);
void clear_listener(void);
void g_init_gxlistener(void);



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
Widget view_mix_panel_menu(void);
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
int popup_menu_exists(void);
void set_menu_label(Widget w, const char *label);
Widget get_menubar(void);
Widget add_menu(snd_state *state);
void create_popup_menu(snd_state *ss);
void post_popup(XButtonPressedEvent *event);

int g_change_menu_label(int which_menu, char *old_label, char *new_label);
int g_set_menu_sensitive(int which_menu, char *old_label, int on);
int g_add_to_main_menu(snd_state *ss, char *label, int slot);
int g_add_to_menu(snd_state *ss, int which_menu, char *label, int callb, int position);
int g_remove_from_menu(int which_menu, char *label);
int g_menu_is_sensitive(int which_menu, char *old_label);
void reflect_play_stop_in_popup_menu(void);
void reflect_play_selection_stop(void);
void g_init_gxmenu(void);
Widget menu_widget(int which_menu);


/* -------- snd-xmain.c -------- */

void dismiss_all_dialogs(snd_state *ss);
void snd_doit(snd_state *state, int argc, char **argv);
#ifdef SND_AS_WIDGET
  void snd_as_widget(int argc, char **argv, XtAppContext app, Widget parent, Arg *caller_args, int caller_argn);
#endif

void g_init_gxmain(void);



/* -------- snd-xfft.c -------- */

void set_fft_window_beta(snd_state *ss, Float val);
void set_transform_size(snd_state *ss, int val);
void set_fft_window(snd_state *ss, int val);
void set_transform_type(snd_state *ss, int val);
void set_wavelet_type(snd_state *ss, int val);
Widget fire_up_transform_dialog(snd_state *ss, int managed);
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


/* -------- snd-xdrop.c -------- */

void initialize_drop(snd_state *ss);
void handle_drop(Widget w, XtPointer context, XtPointer info);
void g_init_gxdrop(void);



/* -------- snd-xregion.c -------- */

void update_region_browser(snd_state *ss, int grf_too);
int region_browser_is_active(void);
void delete_region_and_update_browser(snd_state *ss, int n);
void reflect_play_region_stop(int n);
void set_region_protect(int reg, int protect);
void view_region_callback(Widget w, XtPointer clientData, XtPointer callData);
int region_dialog_is_active(void);
void allocate_region_rows(int n);
void reflect_regions_in_region_browser(void);
void reflect_no_regions_in_region_browser(void);
void reflect_region_graph_style(snd_state *ss);
void g_init_gxregion(void);


/* -------- snd-gxutils -------- */

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


/* -------- snd-gxcolormaps.c -------- */

char **colormap_names(void);
unsigned short *snd_colormap(int n);
void get_current_color(int colormap, int j, unsigned short *r, unsigned short *g, unsigned short *b);


/* -------- snd-xfind.c -------- */

void edit_find_callback(Widget w, XtPointer clientData, XtPointer callData);
#if DEBUGGING
  void g_init_gxfind(void);
#endif


/* -------- snd-xutils.c -------- */

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
void map_over_children (Widget w, void (*func)(Widget w, void *ptr), void *userptr);
void clear_window(axis_context *ax);
void raise_dialog(Widget w);
void set_main_color_of_widget (Widget w, void *userptr);
void highlight_color(snd_state *ss, Widget w);
void white_color(snd_state *ss, Widget w);
void set_button_label_normal(Widget button, const char *str);
void set_button_label_bold(Widget button, const char *str);
void set_button_label(Widget label, const char *str);
void set_label(Widget label, const char *str);
void set_title(snd_state *ss, const char *title);
void goto_window(Widget text);
XtCallbackList make_callback_list(XtCallbackProc callback, XtPointer closure);
void color_sashes(Widget w, void *ptr);
void check_for_event(snd_state *ss);
int event_pending(snd_state *ss);
void color_cursor(snd_state *ss, Pixel color);
void color_marks(snd_state *ss, Pixel color);
void color_selection(snd_state *ss, Pixel color);
void color_data(snd_state *ss, Pixel color);
void color_selected_data(snd_state *ss, Pixel color);
void color_graph(snd_state *ss, Pixel color);
void color_selected_graph(snd_state *ss, Pixel color);
void set_mix_color(snd_state *ss, Pixel color);
void set_selected_mix_color(snd_state *ss, Pixel color);
void recolor_graph(chan_info *cp, int selected);
void reflect_resize(snd_state *ss);
void set_sensitive(Widget wid, int val);
int is_sensitive(Widget wid);
void set_toggle_button(Widget wid, int val, int passed, void *data);
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
Pixmap make_pixmap(snd_state *ss, unsigned char *bits, int width, int height, int depth, GC gc);
BACKGROUND_FUNCTION_TYPE add_work_proc(snd_state *ss, XtWorkProc func, XtPointer data);
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
int channel_open_pane(chan_info *cp, void *ptr);
int channel_unlock_pane(chan_info *cp, void *ptr);
int channel_lock_pane(chan_info *cp, void *ptr);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
void reflect_save_as_in_edit_history(chan_info *cp, char *filename);
void add_channel_window(snd_info *sound, int channel, snd_state *ss, int chan_y, int insertion, Widget main, int arrows);
void set_peak_numbers_font(chan_info *cp);
void set_bold_peak_numbers_font(chan_info *cp);
void set_tiny_numbers_font(chan_info *cp);
COLOR_TYPE get_foreground_color(chan_info *cp, axis_context *ax);
COLOR_TYPE get_background_color(chan_info *cp, axis_context *ax);
void set_foreground_color(chan_info *cp, axis_context *ax, Pixel color);
GC copy_GC(chan_info *cp);
GC erase_GC(chan_info *cp);
void graph_key_press(Widget w, XtPointer clientData, XEvent *event, Boolean *cont);
void cleanup_cw(chan_info *cp);
int fixup_cp_cgx_ax_wn(chan_info *cp);
void change_channel_style(snd_info *sp, int new_style);

void g_init_gxchn(void);


/* -------- snd-xsnd.c -------- */

int control_panel_height(snd_info *sp);
Widget w_snd_pane(snd_info *sp);
Widget w_snd_name(snd_info *sp);
Widget unite_button(snd_info *sp);
void set_control_panel_play_button(snd_info *sp, int val);
Widget filter_graph(snd_info *sp);
void make_minibuffer_label(snd_info *sp, char *str);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
int sound_unlock_control_panel(snd_info *sp, void *ptr);
int sound_lock_control_panel(snd_info *sp, void *ptr);
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
void lock_apply(snd_state *ss, snd_info *sp);
void unlock_apply(snd_state *ss, snd_info *sp);
void set_apply_button(snd_info *sp, int val);
void snd_file_lock_icon(snd_info *sp, int on);
void snd_file_bomb_icon(snd_info *sp, int on);
void x_bomb(snd_info *sp, int on);
snd_info *add_sound_window (char *filename, snd_state *state, int read_only);
void set_sound_pane_file_label(snd_info *sp, char *str);
void unlock_control_panel(snd_info *sp);
void equalize_sound_panes(snd_state *ss, snd_info *sp, chan_info *ncp, int all_panes);
void color_filter_waveform(snd_state *ss, Pixel color);
void reflect_amp_env_completion(snd_info *sp);
void reflect_amp_env_in_progress(snd_info *sp);
void equalize_all_panes(snd_state *ss);
void sound_show_ctrls(snd_info *sp);
void sound_hide_ctrls(snd_info *sp);
int control_panel_open(snd_info *sp);
void show_controls(snd_state *ss);
void hide_controls(snd_state *ss);
void start_progress_report(snd_info *sp, int from_enved);
void finish_progress_report(snd_info *sp, int from_enved);
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, int from_enved);
XmString initial_speed_label(snd_state *ss);
void g_init_gxsnd(void);


/* -------- snd-xfile.c -------- */

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, int *location);
file_data *make_file_data_panel(snd_state *ss, Widget parent, char *name, Arg *args, int n, int with_chan, int header_type, int data_format, int with_loc);
void alert_new_file(void);
void make_open_file_dialog(snd_state *ss, int read_only, int managed);
void make_file_save_as_dialog(snd_state *ss);
void make_edit_save_as_dialog(snd_state *ss);
snd_info *make_new_file_dialog(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *comment);
ww_info *make_title_row(snd_state *ss, Widget formw, char *first_str, char *second_str, char *main_str, int pad, int with_sort, int with_pane);
regrow *make_regrow(snd_state *ss, Widget ww, Widget last_row, 
		    XtCallbackProc first_callback, XtCallbackProc second_callback, XtCallbackProc third_callback);
void make_prevfiles_list (snd_state *ss);
void make_curfiles_list (snd_state *ss);
void curfile_highlight(snd_state *ss, int i);
void view_curfiles_set_row_name(int pos);
void make_cur_name_row(int old_size, int new_size);
void make_prev_name_row(int old_size, int new_size);
void make_a_big_star_outa_me(char *shortname, int big_star);
void set_file_browser_play_button(char *name, int state);
void set_file_sort_sensitive(int sensitive);
void highlight_selected_sound(snd_state *ss);
void view_files_callback(Widget w, XtPointer clientData, XtPointer callData);
Widget start_file_dialog(snd_state *ss, int width, int height);
int file_dialog_is_active(void);
file_info *raw_data_dialog_to_file_info(char *filename, snd_state *ss, const char *title);
Widget edit_header(snd_info *sp);
void set_open_file_play_button(int val);
void make_mix_file_dialog(snd_state *ss, int managed);

void g_initialize_xgfile(void);



/* -------- snd-xenv.c -------- */

chan_info *enved_make_axis_cp(snd_state *ss, char *name, axis_context *ax, int ex0, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax);
void display_enved_env_with_selection(snd_state *ss, env *e, char *name, int x0, int y0, int width, int height, int dots, Float base);
void set_enved_redo_sensitive(int val);
void set_enved_revert_sensitive(int val);
void set_enved_undo_sensitive(int val);
void set_enved_save_sensitive(int val);
void set_enved_show_sensitive(int val);
void make_scrolled_env_list (snd_state *ss);
void alert_enved_amp_env(snd_info *sp);
void new_active_channel_alert(snd_state *ss);
void env_redisplay(snd_state *ss);
void enved_display_point_label(snd_state *ss, Float x, Float y);
void display_enved_progress(char *str, Pixmap pix);
void set_enved_click_to_delete(int n);
void enved_print(char *name);
Widget create_envelope_editor (snd_state *ss);
void set_enved_clip_p(snd_state *ss, int val);
void set_enved_exp_p(snd_state *ss, int val);
void set_enved_base(snd_state *ss, Float val);
void set_enved_target(snd_state *ss, int val);
void set_enved_wave_p(snd_state *ss, int val);
void set_enved_in_dB(snd_state *ss, int val);
int enved_dialog_is_active(void);
void set_enved_filter_order(snd_state *ss, int order);
void enved_reflect_selection(int on);
void color_enved_waveform(Pixel pix);
void reflect_mix_in_enved(void);
void enved_fft_update(void);
void g_init_gxenv(void);



/* -------- snd-xmix.c -------- */

void reflect_mix_in_mix_panel(int mix_id);
Widget make_mix_panel(snd_state *ss);
int mix_play_stopped(void);
void reflect_mix_play_stop(void);
void reflect_no_mix_in_mix_panel(void);



/* -------- snd-xrec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording (void);
void snd_record_file(snd_state *ss);
int record_dialog_is_active(void);


/* -------- snd-xprint.c -------- */

void file_print_callback(Widget w, XtPointer clientData, XtPointer callData);


/* -------- snd-xxen.c -------- */


int snd_color_p(XEN obj);
#define COLOR_P(Obj) snd_color_p(Obj)
snd_color *get_snd_color(XEN arg);
#define TO_SND_COLOR(Arg) get_snd_color(Arg)
XEN pixel2color(COLOR_TYPE pix);
COLOR_TYPE color2pixel(XEN color);
void recolor_button(GUI_WIDGET w, GUI_POINTER ptr);
void color_chan_components(COLOR_TYPE color, int which_component);
void color_unselected_graphs(COLOR_TYPE color);
void recolor_everything(GUI_WIDGET w, GUI_POINTER ptr);
void g_initialize_xgh(void);

#endif


