#ifndef SND_G1_H_LOADED
#define SND_G1_H_LOADED

typedef struct {
  GtkWidget *rw, *nm, *pl, *sv;
  int pos, parent;
  snd_state *ss;
} regrow;

/* -------- snd-ghelp.c -------- */

GtkWidget *snd_help(snd_state *ss, char *subject, char *help);
GtkWidget *snd_help_with_wrap(snd_state *ss, char *subject, char *help);
void move_help_dialog_to(int x, int y);
int help_dialog_is_active(void);


/* -------- snd-gerror.c -------- */

void add_to_error_history(snd_state *ss, char *msg, int popup);
void post_error_dialog(snd_state *ss, char *msg);
void show_snd_errors(snd_state *ss);

#ifdef __GNUC__
  int snd_yes_or_no_p(snd_state *ss, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
#else
  int snd_yes_or_no_p(snd_state *ss, const char *format, ...);
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
void draw_lines (axis_context *ax, GdkPoint *points, int num);
void draw_arc(axis_context *ax, int x, int y, int size);
void set_grf_points(Locus xi, int j, Locus ymin, Locus ymax);
void set_grf_point(Locus xi, int j, Locus yi);
void draw_grf_points(chan_info *cp, axis_context *ax, int j, axis_info *ap, Float y0, int graph_style);
void draw_both_grf_points(chan_info *cp, axis_context *ax, int j, int graph_style);
void draw_both_grfs(axis_context *ax, int j);
void mix_save_graph(snd_state *ss, mix_context *ms, int j);
void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int j);
void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int j);
void setup_axis_context(chan_info *cp, axis_context *ax);
GtkWidget *start_color_dialog(snd_state *ss, int width, int height);
GtkWidget *start_orientation_dialog(snd_state *ss, int width, int height);
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
void view_orientation_callback(GtkWidget *w, gpointer clientData);
void view_color_callback(GtkWidget * w, gpointer clientData);
int color_dialog_is_active(void);
int orientation_dialog_is_active(void);
void reflect_spectro(snd_state *ss);
void allocate_sono_rects(int size);
void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height);
void draw_sono_rectangles(axis_context *ax, int color, int jmax);
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(snd_state *ss, int colormap);
void initialize_colormap(snd_state *ss);



/* -------- snd-glistener.c -------- */

void color_listener(GdkColor *pix);
void color_listener_text(GdkColor *pix);
void handle_listener(snd_state *ss, int new_state);
void snd_completion_help(snd_state *ss, int matches, char **buffer);
int listener_height(void);
int listener_width(void);
void goto_listener(void);
void save_listener_text(FILE *fp);
void append_listener_text(int end, char *msg);
void listener_append(snd_state *ss, char *msg);
void listener_append_and_prompt(snd_state *ss, char *msg);
void clear_listener(void);
void g_init_gxlistener(void);
GtkWidget *snd_entry_new(snd_state *ss, GtkWidget *container, int with_white_background);


/* -------- snd-gmain.c -------- */

void dismiss_all_dialogs(snd_state *ss);
void snd_doit(snd_state *state, int argc, char **argv);
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
GtkWidget *view_equalize_panes_menu(void);
GtkWidget *view_mix_panel_menu(void);
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
GtkWidget *options_speed_ratio_menu(void);
GtkWidget *options_speed_float_menu(void);
GtkWidget *options_speed_semitone_menu(void);
GtkWidget *popup_play_menu(void);
GtkWidget *popup_undo_menu(void);
GtkWidget *popup_redo_menu(void);
GtkWidget *popup_save_menu(void);
GtkWidget *popup_equalize_panes_menu(void);
GtkWidget *popup_info_menu(void);
int popup_menu_exists(void);
void create_popup_menu(snd_state *ss, guint button, TIME_TYPE time);
void set_menu_label(GtkWidget *w, const char *label);
int g_change_menu_label(int which_menu, char *old_label, char *new_label);
int g_set_menu_sensitive(int which_menu, char *old_label, int on);
int g_menu_is_sensitive(int which_menu, char *old_label);
int g_add_to_main_menu(snd_state *ss, char *label, long slot);
int g_add_to_menu(snd_state *ss, int which_menu, char *label, int callb, int position);
int g_remove_from_menu(int which_menu, char *label);
GtkWidget *add_menu(snd_state *state);
GtkWidget *get_menubar(void);
void reflect_play_stop_in_popup_menu(void);
void reflect_play_selection_stop(void);
void g_init_gxmenu(void);
GtkWidget *menu_widget(int which_menu);
void check_menu_labels(int key, int state, int extended);


/* -------- snd-gfft.c -------- */

void set_fft_window_beta(snd_state *ss, Float val);
void set_transform_size(snd_state *ss, int val);
void set_fft_window(snd_state *ss, int val);
void set_transform_type(snd_state *ss, int val);
void set_wavelet_type(snd_state *ss, int val);
GtkWidget *fire_up_transform_dialog(snd_state *ss, int managed);
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



/* -------- snd-gdrop.c -------- */

void initialize_drop(snd_state *ss);

void g_init_gxdrop(void);



/* -------- snd-gregion.c -------- */

void update_region_browser(snd_state *ss, int grf_too);
void reflect_play_region_stop(int n);
int region_browser_is_active(void);
void delete_region_and_update_browser(snd_state *ss, int n);
void set_region_protect(int reg, int protect);
void view_region_callback(GtkWidget *w, gpointer clientData);
void allocate_region_rows(int n);
int region_dialog_is_active(void);
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
char **blank_bits(void);
char **snd_icon_bits(void);
char **speed_l_bits(void);
char **speed_r_bits(void);

char **line_in_bits(void);
char **cd_bits(void);
char **mic_bits(void);
char **speaker_bits(void);


/* -------- snd-gxcolormaps.c -------- */

char **colormap_names(void);
unsigned short *snd_colormap(int n);
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
int channel_open_pane(chan_info *cp, void *ptr);
void set_zx_scrollbar_value(chan_info *cp, Float value);
void resize_sx(chan_info *cp);
void resize_zx(chan_info *cp);
void resize_sy(chan_info *cp);
void resize_zy(chan_info *cp);
void initialize_scrollbars(chan_info *cp);
Float gsy_value(chan_info *cp);
Float gsy_size(chan_info *cp);
int fixup_cp_cgx_ax_wn(chan_info *cp);
void reflect_edit_history_change(chan_info *cp);
void reflect_edit_counter_change(chan_info *cp);
void reflect_save_as_in_edit_history(chan_info *cp, char *filename);
gboolean graph_key_press(GtkWidget *w, GdkEventKey *event, gpointer data);
void add_channel_window(snd_info *sound, int channel, snd_state *ss, int chan_y, int insertion, GtkWidget *main, int arrows);
void set_peak_numbers_font(chan_info *cp);
void set_bold_peak_numbers_font(chan_info *cp);
void set_tiny_numbers_font(chan_info *cp);
COLOR_TYPE get_foreground_color(chan_info *cp, axis_context *ax);
COLOR_TYPE get_background_color(chan_info *cp, axis_context *ax);
void set_foreground_color(chan_info *cp, axis_context *ax, GdkColor *color);
GdkGC *copy_GC(chan_info *cp);
GdkGC *erase_GC(chan_info *cp);
void cleanup_cw(chan_info *cp);
int channel_unlock_pane(chan_info *cp, void *ptr);
void change_channel_style(snd_info *sp, int new_style);

void g_init_gxchn(void);



/* -------- snd-gfind.c -------- */

void edit_find_callback(GtkWidget *w, gpointer clientData);
#if DEBUGGING
  void g_init_gxfind(void);
#endif



/* -------- snd-gutils.c -------- */

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
int label_width(snd_state *ss, char *txt);
int number_width(snd_state *ss, char *num);
int number_height(snd_state *ss);
int label_height(snd_state *ss);
int mark_name_width(snd_state *ss, char *txt);
void clear_window(axis_context *ax);
void set_background(GtkWidget *w, GdkColor *col);
void set_backgrounds(GtkWidget *w, GdkColor *col);
void set_active_color(GtkWidget *w, GdkColor *col);
void set_background_and_redraw(GtkWidget *w, GdkColor *col);
void set_foreground(GtkWidget *w, GdkColor *col);
void set_text_background(GtkWidget *w, GdkColor *col);
void set_pushed_button_colors(GtkWidget *w, snd_state *ss);
void highlight_color(snd_state *ss, GtkWidget *w);
void white_color(snd_state *ss, GtkWidget *w);
void raise_dialog(GtkWidget *w);
void set_button_label_bold(GtkWidget *button, const char *str);
void set_button_label(GtkWidget *label, const char *str);
void set_label(GtkWidget *label, const char *str);
void check_for_event(snd_state *ss);
int event_pending(snd_state *ss);
void set_title(snd_state *ss, const char *title);
void goto_window(GtkWidget *text);
void gc_set_foreground_xor(GdkGC *gc, GdkColor *col1, GdkColor *col2);
void color_cursor(snd_state *ss, GdkColor *color);
void color_marks(snd_state *ss, GdkColor *color);
void color_selection(snd_state *ss, GdkColor *color);
void color_data(snd_state *ss, GdkColor *color);
void color_selected_data(snd_state *ss, GdkColor *color);
void color_graph(snd_state *ss, GdkColor *color);
void color_selected_graph(snd_state *ss, GdkColor *color);
void set_mix_color(snd_state *ss, GdkColor *color);
void set_selected_mix_color(snd_state *ss, GdkColor *color);
void recolor_graph(chan_info *cp, int selected);
void reflect_resize(snd_state *ss);
void set_sensitive(GtkWidget *wid, int val);
int is_sensitive(GtkWidget *wid);
void set_toggle_button(GtkWidget *wid, int val, int passed, void *data);
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
void set_user_data(GtkObject *obj, gpointer data);
gpointer get_user_data(GtkObject *obj);
#if HAVE_GTK2
  char *sg_get_text(GtkWidget *w, int start, int end);
  void sg_set_cursor(GtkWidget *w, int position);
  void sg_text_insert(GtkWidget *w, char *text);
  int sg_cursor_position(GtkWidget *w);
  void sg_select_text(GtkWidget *w, int s0, int s1);
  void sg_unselect_text(GtkWidget *w);
  void sg_list_append(GtkWidget *w, char *val);
  void sg_list_insert(GtkWidget *w, int row, char *val);
  void sg_list_set_text(GtkWidget *w, int row, char *val);
  void sg_pixmap_set(GtkWidget *holder, GdkPixbuf *pix);
  GtkWidget *sg_pixmap_new(GdkPixbuf *pix);
#else
  char *sg_label_text(GtkLabel *w);
#endif
GtkWidget *make_scrolled_text(snd_state *ss, GtkWidget *parent, int editable, GtkWidget *boxer, GtkWidget *paner);
GtkWidget *sg_make_list(gpointer gp, int num_items, char **items, GtkSignalFunc callback);
void sg_text_delete(GtkWidget *w, int start, int end);


/* -------- snd-gsnd.c -------- */

int control_panel_height(snd_info *sp);
GtkWidget *w_snd_pane(snd_info *sp);
GtkWidget *w_snd_pane_box(snd_info *sp);
GtkWidget *w_snd_name(snd_info *sp);
GtkWidget *unite_button(snd_info *sp);
void set_control_panel_play_button(snd_info *sp, int val);
GtkWidget *filter_graph(snd_info *sp);
void snd_file_lock_icon(snd_info *sp, int on);
void snd_file_bomb_icon(snd_info *sp, int on);
void x_bomb(snd_info *sp, int on);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
void make_minibuffer_label(snd_info *sp, char *str);
void set_play_button(snd_info *sp, int val);
void play_button_pause(snd_state *ss, int pausing);
void syncb(snd_info *sp, int on);
void set_snd_amp(snd_info *sp, Float val);
void set_snd_expand(snd_info *sp, Float val);
void set_snd_contrast(snd_info *sp, Float val);
void set_snd_srate(snd_info *sp, Float val);
void set_snd_revlen(snd_info *sp, Float val);
void set_snd_revscl(snd_info *sp, Float val);
void set_snd_filter_order(snd_info *sp, int order);
void set_filter_text(snd_info *sp, char *str);
void sp_display_env(snd_info *sp);
void toggle_expand_button(snd_info *sp, int state);
void toggle_contrast_button(snd_info *sp, int state);
void toggle_reverb_button(snd_info *sp, int state);
void toggle_filter_button(snd_info *sp, int state);
void toggle_direction_arrow(snd_info *sp, int state);
void set_filter_in_dB(snd_info *sp, int val);
void filter_env_changed(snd_info *sp, env *e);
void color_filter_waveform(snd_state *ss, GdkColor *color);
void lock_apply(snd_state *ss, snd_info *sp);
void unlock_apply(snd_state *ss, snd_info *sp);
void reflect_amp_env_completion(snd_info *sp);
void reflect_amp_env_in_progress(snd_info *sp);
snd_info *add_sound_window (char *filename, snd_state *state, int read_only);
void set_sound_pane_file_label(snd_info *sp, char *str);
int sound_unlock_control_panel(snd_info *sp, void *ptr);
int sound_lock_control_panel(snd_info *sp, void *ptr);
void snd_info_cleanup(snd_info *sp);
void unlock_control_panel(snd_info *sp);
void equalize_sound_panes(snd_state *ss, snd_info *sp, chan_info *ncp, int all_panes);
void equalize_all_panes(snd_state *ss);
void sound_show_ctrls(snd_info *sp);
void sound_hide_ctrls(snd_info *sp);
int control_panel_open(snd_info *sp);
void show_controls(snd_state *ss);
void hide_controls(snd_state *ss);
void sound_check_control_panel(snd_info *sp, int height);
void start_progress_report(snd_info *sp, int from_enved);
void finish_progress_report(snd_info *sp, int from_enved);
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, int from_enved);
void set_apply_button(snd_info *sp, int val);
void g_init_gxsnd(void);


/* -------- snd-gmix.c -------- */

void reflect_mix_in_mix_panel(int mix_id);
GtkWidget *make_mix_panel(snd_state *ss);
int mix_play_stopped(void);
void reflect_mix_play_stop(void);
void reflect_no_mix_in_mix_panel(void);



/* -------- snd-genv.c -------- */

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
void display_enved_progress(char *str, SG_PIXMAP *pix, SG_BITMAP *mask);
void set_enved_click_to_delete(int n);
void enved_print(char *name);
GtkWidget *create_envelope_editor (snd_state *ss);
void set_enved_clip_p(snd_state *ss, int val);
void set_enved_exp_p(snd_state *ss, int val);
void set_enved_base(snd_state *ss, Float val);
void set_enved_target(snd_state *ss, int val);
void set_enved_wave_p(snd_state *ss, int val);
void set_enved_in_dB(snd_state *ss, int val);
int enved_dialog_is_active(void);
void enved_reflect_selection(int on);
void set_enved_filter_order(snd_state *ss, int order);
void color_enved_waveform(GdkColor *pix);
void reflect_mix_in_enved(void);
void enved_fft_update(void);
void g_init_gxenv(void);



/* -------- snd-gxen.c -------- */


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



/* -------- snd-grec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording (void);
void snd_record_file(snd_state *ss);
int record_dialog_is_active(void);



/* -------- snd-gfile.c -------- */

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location);
void alert_new_file(void);
void make_open_file_dialog(snd_state *ss, int read_only, int managed);
file_data *make_file_data_panel(snd_state *ss, GtkWidget *parent, char *name, int with_chan, 
				int header_type, int data_format, int with_loc, int comment_as_entry);
void make_file_save_as_dialog(snd_state *ss);
void make_edit_save_as_dialog(snd_state *ss);
ww_info *make_title_row(snd_state *ss, GtkWidget *formw, char *first_str, char *second_str, char *main_str, int pad, int with_sort, int with_pane);
regrow *make_regrow(snd_state *ss, GtkWidget *ww, GtkSignalFunc first_callback, GtkSignalFunc second_callback, GtkSignalFunc third_callback);
void make_cur_name_row(int old_size, int new_size);
void make_prev_name_row(int old_size, int new_size);
void make_prevfiles_list (snd_state *ss);
void make_curfiles_list (snd_state *ss);
void curfile_highlight(snd_state *ss, int i);
void view_curfiles_set_row_name(int pos);
void make_a_big_star_outa_me(char *shortname, int big_star);
void set_file_browser_play_button(char *name, int state);
void highlight_selected_sound(snd_state *ss);
void set_file_sort_sensitive(int sensitive);
void view_files_callback(GtkWidget *w, gpointer clientData);
GtkWidget *start_file_dialog(snd_state *ss, int width, int height);
int file_dialog_is_active(void);
file_info *raw_data_dialog_to_file_info(char *filename, snd_state *ss, const char *title);
snd_info *make_new_file_dialog(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *comment);
void make_mix_file_dialog(snd_state *ss, int managed);
GtkWidget *edit_header(snd_info *sp);
void set_open_file_play_button(int val);
void g_initialize_xgfile(void);



/* -------- snd-gprint.c -------- */

void file_print_callback(GtkWidget *w, gpointer clientData);


#endif

