#include "snd.h"

void reflect_amp_control_bounds_change_in_recorder(void) {}
void snd_help_back_to_top(void) {}
color_t get_in_between_color(color_t fg, color_t bg) {return(0);}
void set_find_dialog_label(const char *str) {}
bool send_mozilla(const char *html_viewer, const char *url) {return(false);}
void check_menu_labels(int key, int state, bool extended) {}
int add_channel_window(snd_info *sound, int channel, int chan_y, int insertion, widget_t main, fw_button_t arrows, bool with_events) {return(0);}
int snd_help(const char *subject, const char *help, with_word_wrap_t with_wrap) {fprintf(stdout, help); return(0);}
int snd_help_with_xrefs(const char *subject, const char *helpstr, with_word_wrap_t with_wrap, char **xrefs, char **urls) {return(0);}
void snd_help_append(char *text) {fprintf(stdout, text);}
int help_text_width(const char *txt, int start, int end) {return(0);}
widget_t post_it(const char *subject, const char *str) {fprintf(stdout, str); return(0);}
void reflect_just_sounds_state(void) {}
void add_to_error_history(char *msg, bool popup) {}
void post_error_dialog(char *msg) {}
bool snd_yes_or_no_p(char *format, ...) {return(false);}
void draw_line (axis_context *ax, int x0, int y0, int x1, int y1) {}
void fill_rectangle (axis_context *ax, int x0, int y0, int width, int height) {}
void fill_polygon(axis_context *ax, int points, ...) {}
void draw_polygon(axis_context *ax, int points, ...) {}
void draw_string (axis_context *ax, int x0, int y0, char *str, int len) {}
void draw_arc(axis_context *ax, int x, int y, int size) {}
void set_grf_points(Locus xi, int j, Locus ymin, Locus ymax) {}
void set_grf_point(Locus xi, int j, Locus yi) {}
void draw_grf_points(int dot_size, axis_context *ax, int j, axis_info *ap, Float y0, graph_style_t graph_style) {}
void draw_both_grf_points(int dot_size, axis_context *ax, int j, graph_style_t graph_style) {}
void mix_save_graph(mix_context *ms, int j) {}
void erase_rectangle (chan_info *cp, axis_context *ax, int x0, int y0, int width, int height) {}
void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int j) {}
void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int j) {}
void setup_axis_context(chan_info *cp, axis_context *ax) {}
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1) {}
void allocate_color_map(int colormap) {}
void allocate_sono_rects(int size) {}
bool set_with_gl(bool val) {return(false);}
void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height) {}
void draw_sono_rectangles(axis_context *ax, int color, int jmax) {}
widget_t start_color_dialog(bool managed) {return(0);}
widget_t start_orientation_dialog(bool managed) {return(0);}
void set_color_scale(Float val) {}
void set_color_inverted(bool val) {}
void set_color_cutoff(Float val) {}
void set_color_map(int val) {}
void set_spectro_hop(int val) {}
void set_spectro_x_angle(Float val) {}
void set_spectro_y_angle(Float val) {}
void set_spectro_z_angle(Float val) {}
void set_spectro_x_scale(Float val) {}
void set_spectro_y_scale(Float val) {}
void set_spectro_z_scale(Float val) {}
void set_spectro_cutoff(Float val) {}
bool color_dialog_is_active(void) {return(false);}
bool orientation_dialog_is_active(void) {return(false);}
void reflect_spectro(void) {}
void reflect_peaks_in_transform_dialog(void) {}
void reflect_log_freq_start_in_transform_dialog(void) {}
void reflect_min_db_in_transform_dialog(void) {}
void widget_int_to_text(widget_t w, int val) {}
void widget_float_to_text(widget_t w, Float val) {}
void widget_off_t_to_text(widget_t w, off_t val) {}
void listener_append_and_prompt(char *msg) {fprintf(stderr, "%s", msg);}
void goto_listener(void) {}
void save_listener_text(FILE *fp) {}
void append_listener_text(int end, char *msg) {}
void listener_delete_text(int new_end) {}
void listener_append(char *msg) {fprintf(stderr, "%s", msg);}
void handle_listener(bool new_state) {}
int listener_height(void) {return(0);}
int listener_width(void) {return(0);}
bool highlight_unbalanced_paren(void) {return(true);}
int file_open_menu(void) {return(0);}
int file_close_menu(void) {return(0);}
int file_save_menu(void) {return(0);}
int file_save_as_menu(void) {return(0);}
int file_print_menu(void) {return(0);}
int file_revert_menu(void) {return(0);}
int file_update_menu(void) {return(0);}
int file_mix_menu(void) {return(0);}
int file_view_menu(void) {return(0);}
int file_new_menu(void) {return(0);}
int edit_cut_menu(void) {return(0);}
int edit_paste_menu(void) {return(0);}
int edit_mix_menu(void) {return(0);}
int edit_play_menu(void) {return(0);}
int edit_save_as_menu(void) {return(0);}
int edit_undo_menu(void) {return(0);}
int edit_redo_menu(void) {return(0);}
int edit_find_menu(void) {return(0);}
int edit_select_all_menu(void) {return(0);}
int edit_header_menu(void) {return(0);}
int view_region_menu(void) {return(0);}
int view_combine_separate_menu(void) {return(0);}
int view_combine_combined_menu(void) {return(0);}
int view_combine_superimposed_menu(void) {return(0);}
int view_lines_menu(void) {return(0);}
int view_dots_menu(void) {return(0);}
int view_dots_and_lines_menu(void) {return(0);}
int view_filled_menu(void) {return(0);}
int view_lollipops_menu(void) {return(0);}
int view_zero_menu(void) {return(0);}
int view_ctrls_menu(void) {return(0);}
int view_listener_menu(void) {return(0);}
int view_cursor_menu(void) {return(0);}
int view_x_axis_seconds_menu(void) {return(0);}
int view_x_axis_beats_menu(void) {return(0);}
int view_x_axis_samples_menu(void) {return(0);}
int view_x_axis_percentage_menu(void) {return(0);}
int options_save_state_menu(void) {return(0);}
int options_focus_left_menu(void) {return(0);}
int options_focus_right_menu(void) {return(0);}
int options_focus_middle_menu(void) {return(0);}
int options_focus_active_menu(void) {return(0);}
int popup_play_menu(void) {return(0);}
int popup_undo_menu(void) {return(0);}
int popup_redo_menu(void) {return(0);}
int popup_save_menu(void) {return(0);}
int popup_info_menu(void) {return(0);}
bool popup_menu_exists(void) {return(false);}
void set_menu_label(int w, const char *label) {}
int g_add_to_main_menu(char *label, int slot) {return(0);}
widget_t g_add_to_menu(int which_menu, char *label, int callb, int position) {return(0);}
int g_remove_from_menu(int which_menu, char *label) {return(0);}
void reflect_play_stop_in_popup_menu(void) {}
void reflect_play_selection_stop(void) {}
int fire_up_transform_dialog(bool managed) {return(0);}
bool transform_dialog_is_active(void) {return(false);}
void set_show_transform_peaks(bool val) {}
void set_fft_log_magnitude(bool val) {}
void set_fft_log_frequency(bool val) {}
void set_transform_normalization(fft_normalize_t val) {}
void set_show_selection_transform(bool show) {}
void reflect_regions_in_region_browser(void) {}
void reflect_no_regions_in_region_browser(void) {}
void update_region_browser(bool grf_too) {}
bool region_browser_is_active(void) {return(false);}
void delete_region_and_update_browser(int n) {}
void reflect_play_region_stop(int n) {}
bool region_dialog_is_active(void) {return(false);}
void allocate_region_rows(int n) {}
void reflect_region_graph_style(void) {}
void snd_completion_help(int matches, char **buffer) {}
bool set_tiny_font(char *font) {if (ss->Tiny_Font) FREE(ss->Tiny_Font); ss->Tiny_Font = copy_string(font); return(false);}
bool set_listener_font(char *font) {if (ss->Listener_Font) FREE(ss->Listener_Font); ss->Listener_Font = copy_string(font); return(false);}
bool set_peaks_font(char *font) {if (ss->Peaks_Font) FREE(ss->Peaks_Font); ss->Peaks_Font = copy_string(font); return(false);}
bool set_bold_peaks_font(char *font) {if (ss->Bold_Peaks_Font) FREE(ss->Bold_Peaks_Font); ss->Bold_Peaks_Font = copy_string(font); return(false);}
bool set_axis_label_font(char *font) {if (ss->Axis_Label_Font) FREE(ss->Axis_Label_Font); ss->Axis_Label_Font = copy_string(font); return(false);}
bool set_axis_numbers_font(char *font) {if (ss->Axis_Numbers_Font) FREE(ss->Axis_Numbers_Font); ss->Axis_Numbers_Font = copy_string(font); return(false);}
int label_width(char *txt) {return(0);}
int number_width(char *num) {return(0);}
int number_height(void) {return(0);}
int label_height(void) {return(0);}
int mark_name_width(char *txt) {return(0);}
void clear_window(axis_context *ax) {}
void set_title(const char *title) {}
void goto_window(int text) {}
void check_for_event(void) {}
bool event_pending(void) {return(false);}
void recolor_graph(chan_info *cp, bool selected) {}
void set_sensitive(int wid, bool val) {}
void set_toggle_button(int wid, bool val, bool passed, void *data) {}
int widget_height(int w) {return(0);}
int widget_width(int w) {return(0);}
void set_widget_size(int w, int width, int height) {}
int widget_x(int w) {return(0);}
int widget_y(int w) {return(0);}
void set_widget_x(int w, int x) {}
void set_widget_y(int w, int y) {}
void set_open_file_play_button(bool val) {}
int channel_w(chan_info *cp) {return(0);}
int channel_f(chan_info *cp) {return(0);}
int channel_graph(chan_info *cp) {return(0);}
bool channel_graph_is_visible(chan_info *cp) {return(false);} /* maybe this should be true? */
void set_zx_scrollbar_value(chan_info *cp, Float value) {}
Float gsy_value(chan_info *cp) {return(0.0);}
Float gsy_size(chan_info *cp) {return(0.0);}
void initialize_scrollbars(chan_info *cp) {}
void resize_sx(chan_info *cp) {}
void resize_zx(chan_info *cp) {}
void resize_sy(chan_info *cp) {}
void resize_zy(chan_info *cp) {}
bool channel_open_pane(chan_info *cp, void *ptr) {return(false);}
void reflect_edit_history_change(chan_info *cp) {}
void reflect_edit_counter_change(chan_info *cp) {}
void set_peak_numbers_font(chan_info *cp) {}
void set_bold_peak_numbers_font(chan_info *cp) {}
void set_tiny_numbers_font(chan_info *cp) {}
color_t get_foreground_color(chan_info *cp, axis_context *ax) {return(0);}
color_t get_background_color(chan_info *cp, axis_context *ax) {return(0);}
void set_foreground_color(chan_info *cp, axis_context *ax, int color) {}
void change_channel_style(snd_info *sp, channel_style_t new_style) {}
void reflect_amp_env_in_progress(snd_info *sp) {}
void cleanup_cw(chan_info *cp) {}
void clear_deleted_snd_info(void *fd) {}
bool fixup_cp_cgx_ax_wn(chan_info *cp) {return(false);}
int w_snd_name(snd_info *sp) {return(0);}
int filter_graph(snd_info *sp) {return(0);}
void make_minibuffer_label(snd_info *sp, char *str) {}
void goto_minibuffer(snd_info *sp) {}
void set_minibuffer_string(snd_info *sp, char *str, bool update) {if ((str) && (*str)) fprintf(stderr, "%s", str);}
void set_minibuffer_cursor_position(snd_info *sp, int pos) {}
char *get_minibuffer_string(snd_info *sp) {return(NULL);}
void snd_info_cleanup(snd_info *sp) {}
void toggle_expand_button(snd_info *sp, bool state) {}
void toggle_contrast_button(snd_info *sp, bool state) {}
void toggle_reverb_button(snd_info *sp, bool state) {}
void toggle_filter_button(snd_info *sp, bool state) {}
void toggle_direction_arrow(snd_info *sp, bool state) {}
void filter_env_changed(snd_info *sp, env *e) {}
void set_play_button(snd_info *sp, bool val) {}
void play_button_pause(bool pausing) {}
void syncb(snd_info *sp, int on) {sp->sync = on;}
void snd_file_lock_icon(snd_info *sp, bool on) {}
void snd_file_bomb_icon(snd_info *sp, bool on) {}
void x_bomb(snd_info *sp, bool on) {}
void set_sound_pane_file_label(snd_info *sp, char *str) {}
void reflect_amp_env_completion(snd_info *sp) {}
void reflect_sound_selection(snd_info *sp) {}
void sound_show_ctrls(snd_info *sp) {}
void sound_hide_ctrls(snd_info *sp) {}
bool control_panel_open(snd_info *sp) {return(false);}
void start_progress_report(snd_info *sp, enved_progress_t from_enved) {}
void finish_progress_report(snd_info *sp, enved_progress_t from_enved) {}
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, enved_progress_t from_enved) {}
char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location, off_t *samples) {return(NULL);}
void alert_new_file(void) {}
snd_info *make_new_file_dialog(char *newname, int header_type, int data_format, int srate, int chans, char *comment) {return(NULL);}
void make_cur_name_row(int old_size, int new_size) {}
void make_prev_name_row(int old_size, int new_size) {}
void make_prevfiles_list (void) {}
void make_curfiles_list (void) {}
void curfile_highlight(int i) {}
void set_file_sort_sensitive(bool sensitive) {}
void view_curfiles_set_row_name(int pos) {}
void set_file_browser_play_button(char *name, int state) {}
void highlight_selected_sound(void) {}
int start_file_dialog(bool managed) {return(0);}
bool view_files_dialog_is_active(void) {return(false);}
int edit_header(snd_info *sp) {return(0);}
widget_t make_edit_save_as_dialog(bool managed) {return(0);}
widget_t make_file_save_as_dialog(bool managed) {return(0);}
widget_t make_file_print_dialog(bool managed) {return(0);}
axis_info *enved_make_axis(const char *name, axis_context *ax, int ex0, int ey0, int width, int height, 
			   Float xmin, Float xmax, Float ymin, Float ymax, printing_t printing) {return(NULL);}
void display_enved_env_with_selection(env *e, char *name, int x0, int y0, int width, int height, bool dots, printing_t printing) {}
void set_enved_redo_sensitive(bool val) {}
void set_enved_revert_sensitive(bool val) {}
void set_enved_undo_sensitive(bool val) {}
void set_enved_save_sensitive(bool val) {}
void set_enved_show_sensitive(bool val) {}
void make_scrolled_env_list (void) {}
void new_active_channel_alert(void) {}
void env_redisplay(void) {}
void env_redisplay_with_print(void) {}
void enved_display_point_label(Float x, Float y) {}
int create_envelope_editor (void) {return(0);}
void set_enved_clip_p(bool val) {}
void reflect_enved_style(void) {}
void set_enved_base(Float val) {}
void set_enved_target(enved_target_t val) {}
void set_enved_wave_p(bool val) {}
void set_enved_in_dB(bool val) {}
bool enved_dialog_is_active(void) {return(false);}
void set_enved_filter_order(int order) {}
void enved_reflect_selection(bool on) {}
void lock_recording_audio(void) {}
void unlock_recording_audio(void) {}
widget_t snd_record_file(void) {return(0);}
bool record_dialog_is_active(void) {return(false);}
void recorder_error(char *msg) {}
void reflect_record_size(int val) {}
void unsensitize_control_buttons(void) {}
void reflect_recorder_duration(Float new_dur) {}
widget_t make_open_file_dialog(bool read_only, bool managed) {return(0);}
widget_t make_mix_file_dialog(bool managed) {return(0);}
void clear_listener(void) {}
int menu_widget(int which_menu) {return(0);}
void get_current_color(int colormap, int j, unsigned short *r, unsigned short *g, unsigned short *b) {}
void set_filter_text(snd_info *sp, char *str) {}
void display_filter_env(snd_info *sp) {}
void reflect_mix_or_track_change(int mix_id, int track_id, bool forced) {}
int make_mix_dialog(void) {return(0);}
int make_track_dialog(void) {return(0);}
bool mix_play_stopped(void) {return(false);}
bool track_play_stopped(void) {return(false);}
void reflect_mix_play_stop(void) {}
void reflect_track_play_stop(void) {}
void set_mix_color(int color) {}
int mix_dialog_mix(void) {return(0);}
int mix_dialog_set_mix(int id) {return(0);}
int track_dialog_track(void) {return(0);}
int track_dialog_set_track(int id) {return(0);}
void show_track_background_wave(int pts, bool two_sided) {}
void set_fft_window_beta(Float val) {in_set_fft_window_beta(val);}
void set_transform_size(int val) {in_set_transform_size(val);}
void set_fft_window(mus_fft_window_t val) {in_set_fft_window(val);}
void set_transform_type(int val) {in_set_transform_type(val);}
void make_transform_type_list(void) {}
void set_wavelet_type(int val) {in_set_wavelet_type(val);}
void set_transform_graph_type(graph_type_t val) {in_set_transform_graph_type(val);}
void set_amp(snd_info *sp, Float val) {sp->amp_control = val;}
void set_expand(snd_info *sp, Float val) {sp->expand_control = val;}
void set_contrast(snd_info *sp, Float val) {sp->contrast_control = val;}
void set_speed(snd_info *sp, Float val) {sp->speed_control = val;}
void set_revlen(snd_info *sp, Float val) {sp->reverb_control_length = val;}
void set_revscl(snd_info *sp, Float val) {sp->reverb_control_scale = val;}
void set_filter_order(snd_info *sp, int val) {sp->filter_control_order = val;}
void set_filter_in_dB(snd_info *sp, bool val) {sp->filter_control_in_dB = val;}
void set_filter_in_hz(snd_info *sp, bool val) {sp->filter_control_in_hz = val;}

void reflect_recorder_mixer_gain(int ind, Float val) {}
void reflect_recorder_out_amp(int ind, Float val) {}
void reflect_recorder_in_amp(int in, int out, Float val) {}

void auto_update_restart(void) {}

snd_info *add_sound_window (char *filename, bool read_only)
{
  snd_info *sp;
  file_info *hdr;
  int snd_slot, nchans, i;
  bool free_filename = false;
  hdr = make_file_info(filename);
  if (!hdr) return(NULL);
  if (ss->pending_change) 
    {
      filename = ss->pending_change;
      free_filename = true;
      ss->pending_change = NULL;
    }
  nchans = hdr->chans;
  if (nchans <= 0) nchans = 1;
  if (nchans > 256)
    {
      /* either a screwed up header, or Snd was built with wrong endianess */
      /* this kind of error is trapped by raw_data_explanation in make_file_info in the motif/gtk cases */
      fprintf(stderr, _("%s has %d channels? "), filename, nchans);
      if (mus_char_to_bint((unsigned char *)&nchans) < 8)
	fprintf(stderr, _("byte swap problem: chans should be %d"), mus_char_to_bint((unsigned char *)&nchans));
      if (mus_char_to_lint((unsigned char *)&nchans) < 8)
	fprintf(stderr, _("byte swap problem: chans should be %d"), mus_char_to_lint((unsigned char *)&nchans));
      nchans = 1; /* ?? */
    }
  snd_slot = find_free_sound_slot(nchans); /* expands sound list if needed */
  ss->sounds[snd_slot] = make_snd_info(ss->sounds[snd_slot], filename, hdr, snd_slot, read_only);
  sp = ss->sounds[snd_slot];
  for (i = 0; i < nchans; i++) sp->chans[i] = make_chan_info(sp->chans[i], i, sp);
  add_sound_data(filename, sp, WITHOUT_GRAPH);
  after_open(sp->index);
  if (free_filename) FREE(filename);
  return(sp);
}

static char **auto_open_file_names = NULL;
static int auto_open_files = 0;
static bool noglob = false, noinit = false, nostdin = false;

#if HAVE_SETJMP_H
#include <setjmp.h>

#if TRAP_SEGFAULT
/* stolen from scwm.c */
static sigjmp_buf envHandleEventsLoop;

static RETSIGTYPE segv(int ignored)
{
  siglongjmp(envHandleEventsLoop, 1);
}
#endif

static jmp_buf top_level_jump;
RETSIGTYPE top_level_catch(int ignore);
RETSIGTYPE top_level_catch(int ignore)
{
  longjmp(top_level_jump, 1);
}
#endif

#define FALLBACK_FONT "9x15"

void snd_doit(int argc, char **argv)
{
  static int auto_open_ctr = 0;
  int i;
  ss->sgx = (state_context *)CALLOC(1, sizeof(state_context));

  ss->init_file = copy_string(getenv(SND_INIT_FILE_ENVIRONMENT_NAME));
  if (ss->init_file == NULL)
    ss->init_file = INIT_FILE_NAME;

#if HAVE_GUILE
  XEN_EVAL_C_STRING("(set! scm-repl-prompt \"snd> \")");

  XEN_EVAL_C_STRING("(define (" S_view_regions_dialog " . args) #f)\
               (define (" S_in " . args) #f)\
               (define (" S_make_color " . args) #f)\
               (define (" S_color_p " . args) #f)\
               (define (goto-listener-end) #f)\
               (define (" S_color_to_list " .args) #f)");

  XEN_EVAL_C_STRING("(define " S_mouse_enter_graph_hook " (make-hook 2))\
               (define " S_mouse_leave_graph_hook " (make-hook 2))\
               (define " S_mouse_enter_label_hook " (make-hook 3))\
               (define " S_mouse_leave_label_hook " (make-hook 3))\
               (define " S_mouse_enter_listener_hook " (make-hook 1))\
               (define " S_mouse_leave_listener_hook " (make-hook 1))\
               (define " S_mouse_enter_text_hook " (make-hook 1))\
               (define " S_mouse_leave_text_hook " (make-hook 1))\
               (define " S_new_widget_hook " (make-hook 1))\
               (define " S_drop_hook " (make-hook 1))\
               (define " S_color_hook " (make-hook 0))\
               (define " S_orientation_hook " (make-hook 0))\
               (define " S_listener_click_hook " (make-hook 1)) \
               (define " S_recorder_file_hook " (make-hook 1)) \
               (define " S_window_property_changed_hook " (make-hook 1))");

  XEN_EVAL_C_STRING("(define " S_enved_envelope " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_just_sounds " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_copy_context " 0)\
               (define " S_cursor_context " 3)\
               (define " S_mark_context " 4)\
               (define " S_selection_context " 2)\
               (define " S_time_graph " 0)\
               (define " S_transform_graph " 1)\
               (define " S_lisp_graph " 2)\
               (define (" S_axis_info " . args) #f)\
               (define (" S_dialog_widgets ") #f)\
               (define (" S_channel_widgets " . args) #f)\
               (define (" S_sound_widgets " . args) #f)\
               (define (" S_menu_widgets " . args) #f)\
               (define (" S_main_widgets " . args) #f)\
               (define (" S_current_font ") #f)\
               (define (" S_reset_listener_cursor ") #f)\
               (define (" S_graph_data " . args) #f)\
               (define (" S_make_graph_data " . args) #f)\
               (define (" S_widget_text " . args) \"\")\
               (define (" S_colormap_p " . args) #f)\
               (define " S_x_axis_label " (make-procedure-with-setter (lambda args \"\") (lambda args \"\")))\
               (define " S_basic_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_zoom_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_position_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_mark_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_listener_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_listener_text_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_enved_waveform_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_enved_filter " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_filter_control_waveform_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_highlight_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_graph_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_selected_graph_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_data_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_selected_data_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_cursor_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_selection_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_pushed_button_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_text_focus_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_sash_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_graph_cursor " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_colormap " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_colormap_size " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_mix_color " (make-procedure-with-setter (lambda args #f) (lambda args #f)))");
#endif
#if HAVE_RUBY
  XEN_EVAL_C_STRING("def view_regions_dialog () false end");
  XEN_EVAL_C_STRING("def make_color (r g b) false end");
  XEN_EVAL_C_STRING("def color_p (a) false end");
  XEN_EVAL_C_STRING("def color_to_list (a) false end");
  XEN_EVAL_C_STRING("def axis_info (s c a) false end");
  XEN_EVAL_C_STRING("def dialog_widgets () false end");
  XEN_EVAL_C_STRING("def channel_widgets (s c) false end");
  XEN_EVAL_C_STRING("def sound_widgets (s) false end");
  XEN_EVAL_C_STRING("def menu_widgets (s) false end");
  XEN_EVAL_C_STRING("def main_widgets (s) false end");
  XEN_EVAL_C_STRING("def current_font () false end");
  XEN_EVAL_C_STRING("def reset_listener_cursor () false end");
  XEN_EVAL_C_STRING("def enved_filter () false end");

  XEN_EVAL_C_STRING("$mouse_enter_graph_hook = false");
  XEN_EVAL_C_STRING("$mouse_leave_graph_hook = false");
  XEN_EVAL_C_STRING("$mouse_enter_label_hook = false");
  XEN_EVAL_C_STRING("$mouse_leave_label_hook = false");
  XEN_EVAL_C_STRING("$mouse_enter_listener_hook = false");
  XEN_EVAL_C_STRING("$mouse_leave_listener_hook = false");
  XEN_EVAL_C_STRING("$mouse_enter_text_hook = false");
  XEN_EVAL_C_STRING("$mouse_leave_text_hook = false");
  XEN_EVAL_C_STRING("$new_widget_hook = false");
  XEN_EVAL_C_STRING("$drop_hook = false");
  XEN_EVAL_C_STRING("$color_hook = false");
  XEN_EVAL_C_STRING("$orientation_hook = false");
  XEN_EVAL_C_STRING("$listener_click_hook = false");
  XEN_EVAL_C_STRING("$window_property_changed_hook = false");

  XEN_EVAL_C_STRING("Copy_context = 0");
  XEN_EVAL_C_STRING("Cursor_context = 3");
  XEN_EVAL_C_STRING("Mark_context = 4");
  XEN_EVAL_C_STRING("Selection_context = 2");
#endif
  set_peaks_font(FALLBACK_FONT);
  set_tiny_font(FALLBACK_FONT);
  set_bold_peaks_font(FALLBACK_FONT);
  set_axis_label_font(FALLBACK_FONT);
  set_axis_numbers_font(FALLBACK_FONT);
  set_listener_font(FALLBACK_FONT);
  set_html_dir(copy_string(DEFAULT_HTML_DIR));
  ss->startup_title = copy_string("snd");

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "-noglob") == 0)
	noglob = true;
      else
	if (strcmp(argv[i], "-noinit") == 0)
	  noinit = true;
	else
	  if (strcmp(argv[i], "-nostdin") == 0)
	    nostdin = true;
	  else
	    if ((strcmp(argv[i], "-b") == 0) || (strcmp(argv[i], "-batch") == 0))
	      ss->batch_mode = true;
    }
  snd_load_init_file(noglob, noinit);
#if HAVE_SIGNAL
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN);
#endif
  auto_open_files = argc - 1;
  if (argc > 1) auto_open_file_names = (char **)(argv + 1);
  while (auto_open_ctr < auto_open_files)
    auto_open_ctr = handle_next_startup_arg(auto_open_ctr, auto_open_file_names, false, auto_open_files);
#if TRAP_SEGFAULT
  if (trap_segfault(ss)) signal(SIGSEGV, segv);
#endif
  if ((ss->sounds) && (ss->sounds[0]) && ((ss->sounds[0])->inuse == SOUND_NORMAL))
    select_channel(ss->sounds[0], 0);

#if HAVE_SETJMP_H
#if TRAP_SEGFAULT
  if (sigsetjmp(envHandleEventsLoop, 1))
    {
      if (!(ss->exiting))
	snd_error(_("Caught seg fault (will try to continue):\n"));
      else
	{
	  snd_error(_("Caught seg fault while trying to exit.\n"));
	  exit(0);
	}
    }
#endif
  if (setjmp(top_level_jump))
    {
      if (!(ss->jump_ok))
	snd_error(_("Caught top level error (will try to continue):\n"));
      else ss->jump_ok = false;
    }
#endif

  xen_repl(1, argv);
}
