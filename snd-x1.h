#ifndef SND_X1_H_LOADED
#define SND_X1_H_LOADED

typedef struct {
  Widget rw,nm,pl,sv;
  int pos;
  snd_state *ss;
} regrow;


/* -------- snd-xhelp.c -------- */

void snd_help(snd_state *ss, char *subject, char *help);
void move_help_dialog_to(int x, int y);
int help_dialog_is_active(void);


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

void draw_line (axis_context *ax,int x0,int y0,int x1,int y1);
void fill_rectangle (axis_context *ax,int x0, int y0, int width, int height);
void erase_rectangle (chan_info *cp, axis_context *ax,int x0, int y0, int width, int height);
void fill_polygon(axis_context *ax,int points, ...);
void draw_polygon(axis_context *ax,int points, ...);
void draw_string (axis_context *ax, int x0, int y0, char *str, int len);
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
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1);
void allocate_color_map(snd_state *ss, int colormap);
void initialize_colormap(snd_state *ss);
void allocate_sono_rects(snd_state *ss, int size);
void set_sono_rectangle(int j, int color, int x, int y, int width, int height);
void draw_sono_rectangles(axis_context *ax, int color, int jmax);
#if HAVE_GUILE
void x_load_colormap(Pixel *colors);
#endif
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
void View_Orientation_Callback(Widget w,XtPointer clientData,XtPointer callData);
void View_Color_Callback(Widget w,XtPointer clientData,XtPointer callData);
int color_dialog_is_active(void);
int orientation_dialog_is_active(void);
void reflect_spectro(snd_state *ss);



/* -------- snd-xlistener.c -------- */

void textfield_focus_Callback(Widget w,XtPointer clientData,XtPointer callData);
void textfield_unfocus_Callback(Widget w,XtPointer clientData,XtPointer callData);
void add_completer_to_textfield(snd_state *ss, Widget w, int completer);
void snd_append_command(snd_state *ss, char *msg);
void append_listener_text(int end, char *msg);
void goto_listener(void);
void color_listener(Pixel pix);
void snd_append_char(snd_state *ss, char *msg);
void handle_listener(snd_state *ss, int new_state);
int listener_height(void);
Widget sndCreateFormWidget(char *name, Widget parent, Arg *args, int n);
Widget sndCreateDrawingAreaWidget(char *name, Widget parent, Arg *args, int n);
Widget sndCreateFrameWidget(char *name, Widget parent, Arg *args, int n);
Widget sndCreateToggleButtonWidget(char *name, Widget parent, Arg *args, int n);
Widget sndCreatePushButtonWidget(char *name, Widget parent, Arg *args, int n);
Widget sndCreateRowColumnWidget(char *name, Widget parent, Arg *args, int n);
Widget sndCreateTextWidget(snd_state *ss, char *name, Widget parent, Arg *args, int n);
Widget sndCreateTextFieldWidget(snd_state *ss, char *name, Widget parent, Arg *args, int n, int activatable, int completer);
Widget sndCreatePanedWindowWidget(char *name, Widget parent, Arg *args, int n);
#if OVERRIDE_TOGGLE
  void override_form_translation(Widget w);
#endif



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
Widget view_normalize_menu(void);
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
Widget view_x_axis_samples_menu(void);
Widget view_x_axis_percentage_menu(void);
Widget options_save_state_menu(void);
Widget options_stats_menu(void);
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
Widget popup_normalize_menu(void);
Widget popup_info_menu(void);
int popup_menu_exists(void);
void set_menu_label(Widget w, const char *label);
Widget get_menubar(void);
Widget add_menu(snd_state *state);
void create_popup_menu(snd_state *ss);
void add_popup_handler(Widget w);

int gh_change_menu_label(int which_menu,char *old_label, char *new_label);
int gh_set_menu_sensitive(int which_menu,char *old_label, int on);
int gh_add_to_main_menu(snd_state *ss, char *label, int slot);
int gh_add_to_menu(snd_state *ss, int which_menu, char *label, int callb);
int gh_remove_from_menu(int which_menu, char *label);
int gh_menu_is_sensitive(int which_menu,char *old_label);

#if HAVE_HOOKS
  void g_init_gxmenu(void);
#endif


/* -------- snd-xmain.c -------- */

void add_dialog(snd_state *ss, Widget dialog);
void dismiss_all_dialogs(snd_state *ss);
void snd_doit(snd_state *state,int argc, char **argv);
#ifdef SND_AS_WIDGET
  void snd_as_widget(int argc, char **argv, XtAppContext app, Widget parent, Arg *caller_args, int caller_argn);
#endif


/* -------- snd-xfft.c -------- */

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


/* -------- snd-xdrop.c -------- */

void InitializeDrop(snd_state *ss);


/* -------- snd-xregion.c -------- */

void update_region_browser(snd_state *ss, int grf_too);
int region_browser_is_active(void);
void delete_region_and_update_browser(snd_state *ss, int n);
void select_region_and_update_browser(snd_state *ss, int n);
void reflect_play_region_stop(int n);
void set_region_protect(int reg, int protect);
void View_Region_Callback(Widget w,XtPointer clientData,XtPointer callData);
int region_dialog_is_active(void);
void allocate_region_rows(snd_state *ss, int n);



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
char **snd_icon_bits(void);
unsigned char *snd_plain_icon_bits(void);
char **blank_bits(void);
char **speed_l_bits(void);
char **speed_r_bits(void);


/* -------- snd-gxcolormaps.c -------- */

char *colormap_name(int n);
unsigned short *snd_colormap(int n);
void get_current_color(int colormap, int j, int *r, int *g, int *b);


/* -------- snd-xfind.c -------- */

void Edit_Find_Callback(Widget w,XtPointer clientData,XtPointer callData);


/* -------- snd-xutils.c -------- */

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
void map_over_children (Widget w, void (*func)(Widget w,void *ptr), void *userptr);
void clear_window(axis_context *ax);
void raise_dialog(Widget w);
void raise_widget(Widget w);
void set_main_color_of_widget (Widget w,void *userptr);
void highlight_color(snd_state *ss, Widget w);
void white_color(snd_state *ss, Widget w);
void set_button_label_normal(Widget button,const char *str);
void set_button_label_bold(Widget button,const char *str);
void set_button_label(Widget label,const char *str);
void set_label(Widget label,const char *str);
void set_title(snd_state *ss, const char *title);
void goto_window(Widget text);
XtCallbackList make_callback_list(XtCallbackProc callback, XtPointer closure);
void color_sashes(Widget w, void *ptr);
void check_for_event(snd_state *ss);
void work_wait(snd_state *ss);
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
int widget_height(Widget w);
int widget_width(Widget w);
void set_widget_height(Widget w, int height);
void set_widget_width(Widget w, int width);
int widget_x(Widget w);
int widget_y(Widget w);
void set_widget_x(Widget w, int x);
void set_widget_y(Widget w, int y);
void set_widget_size(Widget w, int width, int height);
void set_widget_position(Widget w, int x, int y);
void set_pixmap(Widget w, Pixmap pix, void *ignore);
void fixup_axis_context(axis_context *ax, Widget w, GC gc);
Pixmap make_pixmap(snd_state *ss, unsigned char *bits, int width, int height, int depth, GC gc);



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
void set_foreground_color(chan_info *cp, axis_context *ax, Pixel color);
GC copy_GC(chan_info *cp);
GC erase_GC(chan_info *cp);
void graph_key_press(Widget w,XtPointer clientData,XEvent *event,Boolean *cont);
void cleanup_cw(chan_info *cp);
int fixup_cp_cgx_ax_wn(chan_info *cp);
void change_channel_style(snd_info *sp, int new_style);


/* -------- snd-xsnd.c -------- */

Widget w_snd_ctrls(snd_info *sp);
Widget w_snd_pane(snd_info *sp);
Widget w_snd_name(snd_info *sp);
Widget w_snd_combine(snd_info *sp);
Widget w_snd_play(snd_info *sp);
Widget w_snd_filter_env(snd_info *sp);
void make_minibuffer_label(snd_info *sp,char *str);
void goto_minibuffer(snd_info *sp);
void set_minibuffer_string(snd_info *sp, char *str);
void set_minibuffer_cursor_position(snd_info *sp, int pos);
char *get_minibuffer_string(snd_info *sp);
int sound_unlock_ctrls(snd_info *sp, void *ptr);
int sound_lock_ctrls(snd_info *sp, void *ptr);
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
void set_filter_dBing(snd_info *sp, int val);
void filter_env_changed(snd_info *sp, env *e);
void set_play_button(snd_info *sp, int val);
void play_button_pause(snd_state *ss, int pausing);
void syncb(snd_info *sp, int on);
void lock_apply(snd_state *ss, snd_info *sp);
void unlock_apply(snd_state *ss,snd_info *sp);
void set_apply_button(snd_info *sp, int val);
void snd_file_lock_icon(snd_info *sp, int on);
void snd_file_bomb_icon(snd_info *sp, int on);
void x_bomb(snd_info *sp, int on);
snd_info *add_sound_window (char *filename, snd_state *state);
void set_sound_pane_file_label(snd_info *sp, char *str);
void unlock_ctrls(snd_info *sp);
void normalize_sound(snd_state *ss, snd_info *sp, chan_info *ncp);
void color_filter_waveform(snd_state *ss, Pixel color);
void reflect_amp_env_completion(snd_info *sp);
void reflect_amp_env_in_progress(snd_info *sp);
void normalize_all_sounds(snd_state *ss);
void sound_show_ctrls(snd_info *sp);
void sound_hide_ctrls(snd_info *sp);
int control_panel_open(snd_info *sp);
void show_controls(snd_state *ss);
void hide_controls(snd_state *ss);
void start_progress_report(snd_info *sp, int from_enved);
void finish_progress_report(snd_info *sp, int from_enved);
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, int from_enved);
XmString initial_speed_label(snd_state *ss);


/* -------- snd-xfile.c -------- */

char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, int *location);
file_data *sndCreateFileDataForm(snd_state *ss, Widget parent, char *name, Arg *args, int n, int with_chan, int header_type, int data_format, int with_loc);
void alert_new_file(void);
void CreateOpenDialog(Widget w,XtPointer clientData);
void make_open_file_dialog(snd_state *ss);
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
void highlight_selected_sound(snd_state *ss);
void View_Files_Callback(Widget w,XtPointer clientData,XtPointer callData);
void start_file_dialog(snd_state *ss, int width, int height);
int file_dialog_is_active(void);
file_info *get_raw_file_info(char *filename, snd_state *ss);
file_info *get_reasonable_file_info(char *filename, snd_state *ss, file_info *hdr);
void File_Mix_Callback(Widget w,XtPointer clientData,XtPointer callData);
void edit_header(snd_info *sp);
#if HAVE_GUILE
  void g_initialize_xgfile(SCM local_doc);
#endif


/* -------- snd-xenv.c -------- */

chan_info *enved_make_axis_cp(snd_state *ss, char *name, axis_context *ax, int ex0, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax);
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
void display_enved_progress(char *str, Pixmap pix);
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
void set_filter_env_order(snd_state *ss, int order);
void enved_reflect_selection(int on);
void color_enved_waveform(Pixel pix);
void reflect_mix_in_enved(void);
#if HAVE_GUILE
  void g_init_gxenv(SCM local_doc);
#endif


/* -------- snd-xmix.c -------- */

void reflect_mix_in_mix_panel(int mix_id);
void make_mix_panel(snd_state *ss);
int mix_play_stopped(void);
void reflect_mix_play_stop(void);



/* -------- snd-xrec.c -------- */

void lock_recording_audio(void);
void unlock_recording_audio(void);
void cleanup_recording (void);
void snd_record_file(snd_state *ss);
int record_dialog_is_active(void);


/* -------- snd-xprint.c -------- */

void File_Print_Callback(Widget w,XtPointer clientData,XtPointer callData);
char *ps_rgb(snd_state *ss, int pchan);


/* -------- snd-xscm.c -------- */

#if HAVE_GUILE
void g_initialize_xgh(snd_state *ss, SCM local_doc);
#endif


/* -------- snd-xstats.c -------- */

void update_stats(snd_state *ss);
void update_stats_display(snd_state *ss, int all);
void check_stats_window(snd_state *ss, int val);

#endif


