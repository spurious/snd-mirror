#ifndef SND_GTK1_H_LOADED
#define SND_GTK1_H_LOADED

typedef struct {
  GtkWidget *rw,*nm,*pl,*sv;
  int pos;
  snd_state *ss;
} regrow;

/* -------- snd-ghelp.c -------- */

void snd_help(snd_state *ss, char *subject, char *help);
void move_help_dialog_to(int x, int y);
int help_dialog_is_active(void);
#if HAVE_GUILE_GTK
  void init_help_widgets(SCM local_doc);
#endif


/* -------- snd-gerror.c -------- */

void add_to_error_history(snd_state *ss, char *msg);
void post_error_dialog(snd_state *ss, char *msg);
void show_snd_errors(snd_state *ss);
int snd_yes_or_no_p(snd_state *ss,char *question);
#if HAVE_GUILE_GTK
  void init_error_widgets(SCM local_doc);
#endif


/* -------- snd-gdraw.c -------- */

void draw_line (axis_context *ax,int x0,int y0,int x1,int y1);
void fill_rectangle (axis_context *ax,int x0, int y0, int width, int height);
void erase_rectangle (chan_info *cp, axis_context *ax,int x0, int y0, int width, int height);
void fill_polygon(axis_context *ax,int points, ...);
void draw_polygon(axis_context *ax,int points, ...);
void draw_string (axis_context *ax, int x0, int y0, char *str, int len);
void draw_lines (axis_context *ax,GdkPoint *points,int num);
void draw_arc(axis_context *ax, int x, int y, int size);
void set_grf_points(int xi, int j, int ymin, int ymax);
void set_grf_point(int xi, int j, int yi);
void allocate_grf_points(void);
void draw_grf_points(chan_info *cp, axis_context *ax, int j, axis_info *ap, Float y0);
void draw_both_grf_points(chan_info *cp, axis_context *ax, int j);
void draw_both_grfs(axis_context *ax, int j);
void mix_save_graph(snd_state *ss, mix_context *ms,int j);
void erase_and_draw_grf_points(mix_context *ms,chan_info *cp, int j);
void erase_and_draw_both_grf_points(mix_context *ms,chan_info *cp, int j);
void setup_axis_context(chan_info *cp, axis_context *ax);
void start_color_dialog(snd_state *ss, int width, int height);
void start_orientation_dialog(snd_state *ss, int width, int height);
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
void View_Orientation_Callback(GtkWidget *w,gpointer clientData);
void View_Color_Callback(GtkWidget * w,gpointer clientData);
int color_dialog_is_active(void);
int orientation_dialog_is_active(void);
void reflect_spectro(snd_state *ss);
#if HAVE_GUILE
  void x_load_colormap(GdkColor **colors);
#endif
void allocate_sono_rects(snd_state *ss, int size);
void set_sono_rectangle(int j, int color, int x, int y, int width, int height);
void draw_sono_rectangles(axis_context *ax, int color, int jmax);
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(snd_state *ss, int colormap);
void initialize_colormap(snd_state *ss);



/* -------- snd-glistener.c -------- */

void color_listener(GdkColor *pix);
void handle_listener(snd_state *ss, int new_state);
int listener_height(void);
void goto_listener(void);
void append_listener_text(int end, char *msg);
void snd_append_char(snd_state *ss, char *msg);
void snd_append_command(snd_state *ss, char *msg);
#if HAVE_GUILE_GTK
  void init_listener_widgets(SCM local_doc);
#endif



/* -------- snd-gmain.c -------- */

void add_dialog(snd_state *ss, GtkWidget *dialog);
void dismiss_all_dialogs(snd_state *ss);
void snd_doit(snd_state *state,int argc, char **argv);
#ifdef SND_AS_WIDGET
  GtkWidget *snd_as_widget(int argc, char **argv, GtkWidget *parent, void (*error_func)(const char *));
#endif


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
GtkWidget *view_normalize_menu(void);
GtkWidget *view_consoles_menu(void);
GtkWidget *view_region_menu(void);
GtkWidget *view_combine_separate_menu(void);
GtkWidget *view_combine_combined_menu(void);
GtkWidget *view_combine_superimposed_menu(void);
GtkWidget *view_lines_menu(void);
GtkWidget *view_dots_menu(void);
GtkWidget *view_dots_and_lines_menu(void);
GtkWidget *view_filled_menu(void);
GtkWidget *view_lollipops_menu(void);
GtkWidget *view_marks_menu(void);
GtkWidget *view_zero_menu(void);
GtkWidget *view_ctrls_menu(void);
GtkWidget *view_listener_menu(void);
GtkWidget *view_cursor_menu(void);
GtkWidget *view_x_axis_seconds_menu(void);
GtkWidget *view_x_axis_samples_menu(void);
GtkWidget *view_x_axis_percentage_menu(void);
GtkWidget *options_save_state_menu(void);
GtkWidget *options_stats_menu(void);
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
GtkWidget *popup_normalize_menu(void);
GtkWidget *popup_info_menu(void);
int popup_menu_exists(void);
void create_popup_menu(snd_state *ss, guint button, TIME_TYPE time);
void set_menu_label(GtkWidget *w, char *label);
int gh_change_menu_label(int which_menu,char *old_label, char *new_label);
int gh_set_menu_sensitive(int which_menu,char *old_label, int on);
int gh_add_to_main_menu(snd_state *ss, char *label);
int gh_add_to_menu(snd_state *ss, int which_menu, char *label, int callb);
int gh_remove_from_menu(int which_menu, char *label);
GtkWidget *add_menu(snd_state *state);
GtkWidget *get_menubar(void);
#if HAVE_GUILE_GTK
  void init_menu_widgets(SCM local_doc);
#endif


/* -------- snd-gfft.c -------- */

void set_fft_beta(snd_state *ss, Float val);
void set_fft_size(snd_state *ss, int val);
void set_fft_window(snd_state *ss, int val);
void set_transform_type(snd_state *ss, int val);
void set_wavelet_type(snd_state *ss, int val);
void fire_up_transform_dialog(snd_state *ss);
int transform_dialog_is_active(void);
#if HAVE_GUILE
char *transform_type_name(int choice);
int add_transform_to_list(char *name);
int max_transform_type(void);
#endif
void set_show_fft_peaks(snd_state *ss, int val);
void set_fft_log_magnitude(snd_state *ss, int val);
void set_fft_log_frequency(snd_state *ss, int val);
void set_normalize_fft(snd_state *ss, int val);
void set_show_selection_transform(snd_state *ss, int show);
void set_fft_style(snd_state *ss, int val);
#if HAVE_GUILE_GTK
  void init_fft_widgets(SCM local_doc);
#endif


/* -------- snd-gdrop.c -------- */

void InitializeDrop(snd_state *ss);



/* -------- snd-gregion.c -------- */

void update_region_browser(snd_state *ss, int grf_too);
void reflect_play_region_stop(region_info *r);
int region_browser_is_active(void);
void delete_region_and_update_browser(snd_state *ss, int n);
void select_region_and_update_browser(snd_state *ss, int n);
void set_region_protect(int reg, int protect);
void View_Region_Callback(GtkWidget *w,gpointer clientData);
void allocate_region_rows(snd_state *ss, int n);
int region_dialog_is_active(void);
#if HAVE_GUILE_GTK
  void init_region_widgets(SCM local_doc);
#endif


/* -------- snd-gxutils -------- */
#if HAVE_GUILE
  void g_init_gxutils(void);
#endif



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
char **mini_bits(void);
char **mixer_bits(void);
char **mic_bits(void);
char **speaker_bits(void);
char **cross_bits(void);


/* -------- snd-gxcolormaps.c -------- */

char *colormap_name(int n);
unsigned short *snd_colormap(int n);
void get_current_color(int colormap, int j, int *r, int *g, int *b);


/* -------- snd-gchn.c -------- */

GtkWidget *channel_sx(chan_info *cp);
GtkWidget *channel_sy(chan_info *cp);
GtkWidget *channel_zx(chan_info *cp);
GtkWidget *channel_zy(chan_info *cp);
GtkWidget *channel_w(chan_info *cp);
GtkWidget *channel_f(chan_info *cp);
GtkWidget *channel_graph(chan_info *cp);
GtkWidget *channel_graph_parent(chan_info *cp);
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
gint graph_key_press(GtkWidget *w, GdkEventKey *event, gpointer data);
void add_channel_window(snd_info *sound, int channel, snd_state *ss, int chan_y, int insertion, GtkWidget *main, int arrows);
void set_peak_numbers_font(chan_info *cp);
void set_bold_peak_numbers_font(chan_info *cp);
void set_tiny_numbers_font(chan_info *cp);
COLOR_TYPE get_foreground_color(chan_info *cp, axis_context *ax);
void set_foreground_color(chan_info *cp, axis_context *ax, GdkColor *color);
GdkGC *copy_GC(chan_info *cp);
GdkGC *erase_GC(chan_info *cp);
void cleanup_cw(chan_info *cp);
int channel_unlock_pane(chan_info *cp, void *ptr);
#if HAVE_GUILE_GTK
  void init_chn_widgets(SCM local_doc);
#endif
void change_channel_style(snd_info *sp, int new_style);


/* -------- snd-gfind.c -------- */

void Edit_Find_Callback(GtkWidget *w,gpointer clientData);
#if HAVE_GUILE_GTK
  void init_find_widgets(SCM local_doc);
#endif


/* -------- snd-gutils.c -------- */

axis_context *free_axis_context(axis_context *ax);
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
void map_over_children (GtkWidget *w, void (*func)(GtkWidget *wid, gpointer data), void *userptr);
void clear_window(axis_context *ax);
void set_background(GtkWidget *w,GdkColor *col);
void set_backgrounds(GtkWidget *w,GdkColor *col);
void set_active_color(GtkWidget *w,GdkColor *col);
void set_background_and_redraw(GtkWidget *w,GdkColor *col);
void set_foreground(GtkWidget *w,GdkColor *col);
void set_text_background(GtkWidget *w, GdkColor *col);
void set_pushed_button_colors(GtkWidget *w, snd_state *ss);
void highlight_color(snd_state *ss, GtkWidget *w);
void white_color(snd_state *ss, GtkWidget *w);
void raise_dialog(GtkWidget *w);
void raise_widget(GtkWidget *w);
void set_button_label_bold(GtkWidget *button,char *str);
void set_button_label(GtkWidget *label,char *str);
void set_label(GtkWidget *label,char *str);
void check_for_event(snd_state *ss);
void work_wait(snd_state *ss);
void set_title(snd_state *ss, char *title);
void goto_window(GtkWidget *text);
void gc_set_foreground_xor(GdkGC *gc,GdkColor *col1, GdkColor *col2);
void color_cursor(snd_state *ss, GdkColor *color);
void color_marks(snd_state *ss, GdkColor *color);
void color_selection(snd_state *ss, GdkColor *color);
void color_data(snd_state *ss, GdkColor *color);
void color_selected_data(snd_state *ss, GdkColor *color);
void color_graph(snd_state *ss, GdkColor *color);
void color_selected_graph(snd_state *ss, GdkColor *color);
void color_mix_waveform(snd_state *ss, GdkColor *color);
void recolor_graph(chan_info *cp, int selected);
void reflect_resize(snd_state *ss);
void set_sensitive(GtkWidget *wid, int val);
void set_toggle_button(GtkWidget *wid, int val, int passed, void *data);
int widget_height(GtkWidget *w);
int widget_width(GtkWidget *w);
void set_widget_height(GtkWidget *w, int height);
void set_widget_width(GtkWidget *w, int width);
int widget_x(GtkWidget *w);
int widget_y(GtkWidget *w);
void set_widget_x(GtkWidget *w, int x);
void set_widget_y(GtkWidget *w, int y);
void set_widget_size(GtkWidget *w, int width, int height);
void set_widget_position(GtkWidget *w, int x, int y);
void set_pixmap(GtkWidget *w, GdkPixmap *pix, GdkBitmap *mask);
void fixup_axis_context(axis_context *ax, GtkWidget *w, GdkGC *gc);


/* -------- snd-gsnd.c -------- */

GtkWidget *w_snd_ctrls(snd_info *sp);
GtkWidget *w_snd_pane(snd_info *sp);
GtkWidget *w_snd_pane_box(snd_info *sp);
GtkWidget *w_snd_name(snd_info *sp);
GtkWidget *w_snd_combine(snd_info *sp);
GtkWidget *w_snd_play(snd_info *sp);
GtkWidget *w_snd_filter_env(snd_info *sp);
void snd_file_lock_icon(snd_info *sp, int on);
void snd_file_bomb_icon(snd_info *sp, int on);
void x_bomb(snd_info *sp, int on);
void set_blank_pixmap(GtkWidget *w);
GtkWidget *get_blank_pixmap(snd_state *ss);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
void make_minibuffer_label(snd_info *sp,char *str);
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
void set_filter_dBing(snd_info *sp, int val);
void filter_env_changed(snd_info *sp, env *e);
void color_filter_waveform(snd_state *ss, GdkColor *color);
void lock_apply(snd_state *ss, snd_info *sp);
void unlock_apply(snd_state *ss,snd_info *sp);
void reflect_amp_env_completion(snd_info *sp);
void reflect_amp_env_in_progress(snd_info *sp);
snd_info *add_sound_window (char *filename, snd_state *state);
void set_sound_pane_file_label(snd_info *sp, char *str);
int sound_unlock_ctrls(snd_info *sp, void *ptr);
int sound_lock_ctrls(snd_info *sp, void *ptr);
void snd_info_cleanup(snd_info *sp);
void unlock_ctrls(snd_info *sp);
void normalize_sound(snd_state *ss, snd_info *sp, chan_info *ncp);
void normalize_all_sounds(snd_state *ss);
void sound_show_ctrls(snd_info *sp);
void sound_hide_ctrls(snd_info *sp);
int control_panel_open(snd_info *sp);
void show_controls(snd_state *ss);
void hide_controls(snd_state *ss);
void sound_check_control_panel(snd_info *sp, int height);
void start_progress_report(snd_info *sp, int from_enved);
void finish_progress_report(snd_info *sp, int from_enved);
void progress_report(snd_info *sp, char *funcname, int curchan, int chans, Float pct, int from_enved);
void set_apply_button(snd_info *sp, int val);
#if HAVE_GUILE_GTK
  void init_sound_widgets(SCM local_doc);
#endif


/* -------- snd-gmix.c -------- */

void release_mixmark_widgets(mixmark *m);
void select_mix(snd_state *ss, mixdata *md);
void color_selected_mix(snd_state *ss);
void color_unselected_mixes(snd_state *ss);
void color_one_mix(mixdata *md, GdkColor *color);
int color_mix(mixdata *md, void *ptr);
void reflect_mix_stop_playing(snd_state *ss, mixmark *m);
void set_mix_console_amp_scaler(Float amp);
Float get_mix_console_amp_scaler(void);
void set_mix_console_speed_scaler(Float amp);
Float get_mix_console_speed_scaler(void);
void mix_set_title_beg(mixdata *md, mixmark *m);
void reamp(mixdata *md, int chan, Float amp);
void respeed(mixdata *md, Float spd);
void fixup_mixmark(mixdata *md);
int mix_dragging(void);
void set_mix_track_button_color(mixdata *md, int track);
void move_mixmark(mixmark *m, int x, int y);
void use_mixmark(mixdata *md, int x, int y);
void move_mix_x(mixmark *m, int xspot);
void move_mix_y(mixmark *m, int yspot);
void mix_set_minimal_title(mixdata *md, mixmark *m);
void mix_set_title_name(mixdata *md, mixmark *m);
void mix_set_console(mixdata *md, mixmark *m);
void mix_open_console(mixmark *m);
void mix_close_console(mixmark *m);
void mix_open_title(mixmark *m);
void mix_close_title(mixmark *m);
int move_mix_console(mixmark *m, int *nx);
void mix_raise_console(mixmark *m);



/* -------- snd-genv.c -------- */

void enved_make_axis_cp(snd_state *ss, char *name, axis_context *ax, int ex0, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax);
void display_enved_env_with_selection(snd_state *ss, env *e, char *name, int x0, int y0, int width, int height, int dots);
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
void display_enved_progress(char *str, GdkPixmap *pix, GdkBitmap *mask);
void set_enved_click_to_delete(int n);
void enved_print(char *name);
void create_envelope_editor (snd_state *ss);
void set_enved_clipping(snd_state *ss, int val);
void set_enved_exping(snd_state *ss, int val);
void set_enved_base(snd_state *ss,Float val);
void set_enved_target(snd_state *ss, int val);
void set_enved_waving(snd_state *ss, int val);
void set_enved_dBing(snd_state *ss, int val);
int enved_dialog_is_active(void);
void enved_reflect_selection(int on);
void set_filter_env_order(snd_state *ss, int order);
void color_enved_waveform(GdkColor *pix);
void reflect_mix_in_enved(void);
#if HAVE_GUILE_GTK
  void init_enved_widgets(SCM local_doc);
#endif



/* -------- snd-gscm.c -------- */

#if HAVE_GUILE
  void g_initialize_xgh(snd_state *ss, SCM local_doc);
#endif


/* -------- snd-grec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording (void);
void snd_record_file(snd_state *ss);
int record_dialog_is_active(void);
#if HAVE_GUILE_GTK
  void init_recorder_widgets(SCM local_doc);
#endif



/*-------- snd-gstats.c -------- */

void update_stats_display(snd_state *ss, int all);
void check_stats_window(snd_state *ss, int val);
void update_stats(snd_state *ss);
#if HAVE_GUILE_GTK
  void init_stats_widgets(SCM local_doc);
#endif



/* -------- snd-gfile.c -------- */

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, int *location);
void alert_new_file(void);
void toggle_just_sounds(int n);
void make_open_file_dialog(snd_state *ss);
file_data *sndCreateFileDataForm(snd_state *ss, GtkWidget *parent, char *name, int with_chan, int header_type, int data_format, int with_loc, int comment_as_entry);
void make_file_save_as_dialog(snd_state *ss);
void make_edit_save_as_dialog(snd_state *ss);
ww_info *make_title_row(snd_state *ss, GtkWidget *formw, char *first_str, char *second_str, char *main_str, int pad, int with_sort, int with_pane);
regrow *make_regrow(snd_state *ss, GtkWidget *ww,GtkSignalFunc first_callback,GtkSignalFunc second_callback,GtkSignalFunc third_callback);
void make_cur_name_row(int old_size, int new_size);
void make_prev_name_row(int old_size, int new_size);
void make_prevfiles_list (snd_state *ss);
void make_curfiles_list (snd_state *ss);
void curfile_highlight(snd_state *ss, int i);
void view_curfiles_set_row_name(int pos);
void make_a_big_star_outa_me(char *shortname, int big_star);
void set_file_browser_play_button(char *name, int state);
void highlight_selected_sound(snd_state *ss);
void View_Files_Callback(GtkWidget *w,gpointer clientData);
void start_file_dialog(snd_state *ss, int width, int height);
int file_dialog_is_active(void);
file_info *get_raw_file_info(char *filename, snd_state *ss);
file_info *get_reasonable_file_info(char *filename, snd_state *ss, file_info *hdr);
snd_info *make_new_file_dialog(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *comment);
void File_Mix_Callback(GtkWidget *w,gpointer clientData);
void edit_header(snd_info *sp);
#if HAVE_GUILE_GTK
  void init_file_widgets(SCM local_doc);
#endif


/* -------- snd-gprint.c -------- */

void File_Print_Callback(GtkWidget *w, gpointer clientData);
char *ps_rgb(snd_state *ss, int pchan);
#if HAVE_GUILE_GTK
  void init_print_widgets(SCM local_doc);
#endif


#endif

