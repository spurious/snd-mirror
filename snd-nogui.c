#include "snd.h"

void check_menu_labels(int key, int state, int extended) {}
void add_channel_window(snd_info *sound, int channel, snd_state *ss, int chan_y, int insertion, GUI_WIDGET main, int arrows) {}
int snd_help(snd_state *ss, char *subject, char *help) {fprintf(stdout, help); return(0);}
int snd_help_with_wrap(snd_state *ss, char *subject, char *help) {fprintf(stdout, help); return(0);}
void add_to_error_history(snd_state *ss, char *msg, int popup) {}
void post_error_dialog(snd_state *ss, char *msg) {}
int snd_yes_or_no_p(snd_state *ss, char *format, ...) {return(0);}
void draw_line (axis_context *ax, int x0, int y0, int x1, int y1) {}
void fill_rectangle (axis_context *ax, int x0, int y0, int width, int height) {}
void fill_polygon(axis_context *ax, int points, ...) {}
void draw_polygon(axis_context *ax, int points, ...) {}
void draw_string (axis_context *ax, int x0, int y0, char *str, int len) {}
void draw_arc(axis_context *ax, int x, int y, int size) {}
void set_grf_points(Locus xi, int j, Locus ymin, Locus ymax) {}
void set_grf_point(Locus xi, int j, Locus yi) {}
void draw_grf_points(chan_info *cp, axis_context *ax, int j, axis_info *ap, Float y0, int graph_style) {}
void draw_both_grf_points(chan_info *cp, axis_context *ax, int j, int graph_style) {}
void draw_both_grfs(axis_context *ax, int j) {}
void mix_save_graph(snd_state *ss, mix_context *ms, int j) {}
void erase_rectangle (chan_info *cp, axis_context *ax, int x0, int y0, int width, int height) {}
void erase_and_draw_grf_points(mix_context *ms, chan_info *cp, int j) {}
void erase_and_draw_both_grf_points(mix_context *ms, chan_info *cp, int j) {}
void setup_axis_context(chan_info *cp, axis_context *ax) {}
void draw_spectro_line(axis_context *ax, int color, int x0, int y0, int x1, int y1) {}
void allocate_color_map(snd_state *ss, int colormap) {}
void allocate_sono_rects(int size) {}
int set_with_gl(snd_state *ss, int val) {return(0);}
void set_sono_rectangle(int j, int color, Locus x, Locus y, Latus width, Latus height) {}
void draw_sono_rectangles(axis_context *ax, int color, int jmax) {}
int start_color_dialog(snd_state *ss, int width, int height) {return(0);}
int start_orientation_dialog(snd_state *ss, int width, int height) {return(0);}
void set_color_scale(snd_state *ss, Float val) {}
void set_color_inverted(snd_state *ss, int val) {}
void set_color_cutoff(snd_state *ss, Float val) {}
void set_color_map(snd_state *ss, int val) {}
void set_spectro_hop(snd_state *ss, int val) {}
void set_spectro_x_angle(snd_state *ss, Float val) {}
void set_spectro_y_angle(snd_state *ss, Float val) {}
void set_spectro_z_angle(snd_state *ss, Float val) {}
void set_spectro_x_scale(snd_state *ss, Float val) {}
void set_spectro_y_scale(snd_state *ss, Float val) {}
void set_spectro_z_scale(snd_state *ss, Float val) {}
void set_spectro_cutoff(snd_state *ss, Float val) {}
int color_dialog_is_active(void) {return(0);}
int orientation_dialog_is_active(void) {return(0);}
void reflect_spectro(snd_state *ss) {}
void listener_append_and_prompt(snd_state *ss, char *msg) {fprintf(stderr, "%s", msg);}
void goto_listener(void) {}
void save_listener_text(FILE *fp) {}
void append_listener_text(int end, char *msg) {}
void listener_append(snd_state *ss, char *msg) {fprintf(stderr, "%s", msg);}
void handle_listener(snd_state *ss, int new_state) {}
int listener_height(void) {return(0);}
int listener_width(void) {return(0);}
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
int view_equalize_panes_menu(void) {return(0);}
int view_mix_panel_menu(void) {return(0);}
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
int options_speed_ratio_menu(void) {return(0);}
int options_speed_float_menu(void) {return(0);}
int options_speed_semitone_menu(void) {return(0);}
int popup_play_menu(void) {return(0);}
int popup_undo_menu(void) {return(0);}
int popup_redo_menu(void) {return(0);}
int popup_save_menu(void) {return(0);}
int popup_equalize_panes_menu(void) {return(0);}
int popup_info_menu(void) {return(0);}
int popup_menu_exists(void) {return(0);}
void set_menu_label(int w, const char *label) {}
int g_change_menu_label(int which_menu, char *old_label, char *new_label) {return(0);}
int g_set_menu_sensitive(int which_menu, char *old_label, int on) {return(0);}
int g_menu_is_sensitive(int which_menu, char *old_label) {return(0);}
int g_add_to_main_menu(snd_state *ss, char *label, long slot) {return(0);}
int g_add_to_menu(snd_state *ss, int which_menu, char *label, int callb, int position) {return(0);}
int g_remove_from_menu(int which_menu, char *label) {return(0);}
void reflect_play_stop_in_popup_menu(void) {}
void reflect_play_selection_stop(void) {}
void dismiss_all_dialogs(snd_state *ss) {}
int fire_up_transform_dialog(snd_state *ss, int managed) {return(0);}
int transform_dialog_is_active(void) {return(0);}
void set_show_transform_peaks(snd_state *ss, int val) {}
void set_fft_log_magnitude(snd_state *ss, int val) {}
void set_fft_log_frequency(snd_state *ss, int val) {}
void set_transform_normalization(snd_state *ss, int val) {}
void set_show_selection_transform(snd_state *ss, int show) {}
void reflect_regions_in_region_browser(void) {}
void reflect_no_regions_in_region_browser(void) {}
void update_region_browser(snd_state *ss, int grf_too) {}
int region_browser_is_active(void) {return(0);}
void delete_region_and_update_browser(snd_state *ss, int n) {}
void reflect_play_region_stop(int n) {}
void set_region_protect(int reg, int protect) {}
int region_dialog_is_active(void) {return(0);}
void allocate_region_rows(int n) {}
void reflect_region_graph_style(snd_state *ss) {}
void snd_completion_help(snd_state *ss, int matches, char **buffer) {}
int set_help_text_font(snd_state *ss, char *font) {ss->Help_Text_Font = font; return(0);}
int set_tiny_font(snd_state *ss, char *font) {ss->Tiny_Font = font; return(0);}
int set_listener_font(snd_state *ss, char *font) {ss->Listener_Font = font; return(0);}
int set_button_font(snd_state *ss, char *font) {ss->Button_Font = font; return(0);}
int set_bold_button_font(snd_state *ss, char *font) {ss->Bold_Button_Font = font; return(0);}
int set_axis_label_font(snd_state *ss, char *font) {ss->Axis_Label_Font = font; return(0);}
int set_axis_numbers_font(snd_state *ss, char *font) {ss->Axis_Numbers_Font = font; return(0);}
void activate_numbers_font(axis_context *ax, snd_state *ss) {}
void activate_label_font(axis_context *ax, snd_state *ss) {}
void activate_button_font(axis_context *ax, snd_state *ss) {}
int label_width(snd_state *ss, char *txt) {return(0);}
int number_width(snd_state *ss, char *num) {return(0);}
int number_height(snd_state *ss) {return(0);}
int label_height(snd_state *ss) {return(0);}
int mark_name_width(snd_state *ss, char *txt) {return(0);}
void clear_window(axis_context *ax) {}
void highlight_color(snd_state *ss, int w) {}
void white_color(snd_state *ss, int w) {}
void set_title(snd_state *ss, const char *title) {}
void goto_window(int text) {}
void check_for_event(snd_state *ss) {}
int event_pending(snd_state *ss) {return(0);}
void recolor_graph(chan_info *cp, int selected) {}
void reflect_resize(snd_state *ss) {}
void set_sensitive(int wid, int val) {}
void set_toggle_button(int wid, int val, int passed, void *data) {}
int widget_height(int w) {return(0);}
int widget_width(int w) {return(0);}
void set_widget_height(int w, int height) {}
void set_widget_width(int w, int width) {}
void set_widget_size(int w, int width, int height) {}
int widget_x(int w) {return(0);}
int widget_y(int w) {return(0);}
void set_widget_x(int w, int x) {}
void set_widget_y(int w, int y) {}
void set_open_file_play_button(int val) {}
int channel_w(chan_info *cp) {return(0);}
int channel_f(chan_info *cp) {return(0);}
int channel_graph(chan_info *cp) {return(0);}
void set_zx_scrollbar_value(chan_info *cp, Float value) {}
Float gsy_value(chan_info *cp) {return(0.0);}
Float gsy_size(chan_info *cp) {return(0.0);}
void initialize_scrollbars(chan_info *cp) {}
void resize_sx(chan_info *cp) {}
void resize_zx(chan_info *cp) {}
void resize_sy(chan_info *cp) {}
void resize_zy(chan_info *cp) {}
int channel_open_pane(chan_info *cp, void *ptr) {return(0);}
int channel_unlock_pane(chan_info *cp, void *ptr) {return(0);}
void reflect_edit_history_change(chan_info *cp) {}
void reflect_edit_counter_change(chan_info *cp) {}
void reflect_save_as_in_edit_history(chan_info *cp, char *filename) {}
void set_peak_numbers_font(chan_info *cp) {}
void set_bold_peak_numbers_font(chan_info *cp) {}
void set_tiny_numbers_font(chan_info *cp) {}
COLOR_TYPE get_foreground_color(chan_info *cp, axis_context *ax) {return(0);}
COLOR_TYPE get_background_color(chan_info *cp, axis_context *ax) {return(0);}
void set_foreground_color(chan_info *cp, axis_context *ax, int color) {}
void change_channel_style(snd_info *sp, int new_style) {}
void reflect_amp_env_in_progress(snd_info *sp) {}
void cleanup_cw(chan_info *cp) {}
int fixup_cp_cgx_ax_wn(chan_info *cp) {return(0);}
int w_snd_name(snd_info *sp) {return(0);}
int filter_graph(snd_info *sp) {return(0);}
void make_minibuffer_label(snd_info *sp, char *str) {}
void goto_minibuffer(snd_info *sp) {}
void set_minibuffer_string(snd_info *sp, char *str) {if ((str) && (*str)) fprintf(stderr, "%s", str);}
void set_minibuffer_cursor_position(snd_info *sp, int pos) {}
char *get_minibuffer_string(snd_info *sp) {return(NULL);}
void snd_info_cleanup(snd_info *sp) {}
void toggle_expand_button(snd_info *sp, int state) {}
void toggle_contrast_button(snd_info *sp, int state) {}
void toggle_reverb_button(snd_info *sp, int state) {}
void toggle_filter_button(snd_info *sp, int state) {}
void toggle_direction_arrow(snd_info *sp, int state) {}
void filter_env_changed(snd_info *sp, env *e) {}
void set_play_button(snd_info *sp, int val) {}
void play_button_pause(snd_state *ss, int pausing) {}
void syncb(snd_info *sp, int on) {sp->sync = on;}
void lock_apply(snd_state *ss, snd_info *sp) {}
void unlock_apply(snd_state *ss, snd_info *sp) {}
void set_apply_button(snd_info *sp, int val) {}
void snd_file_lock_icon(snd_info *sp, int on) {}
void snd_file_bomb_icon(snd_info *sp, int on) {}
void x_bomb(snd_info *sp, int on) {}
void set_sound_pane_file_label(snd_info *sp, char *str) {}
void equalize_sound_panes(snd_state *ss, snd_info *sp, chan_info *ncp, int all_panes) {}
void reflect_amp_env_completion(snd_info *sp) {}
void equalize_all_panes(snd_state *ss) {}
void sound_show_ctrls(snd_info *sp) {}
void sound_hide_ctrls(snd_info *sp) {}
int control_panel_open(snd_info *sp) {return(0);}
void start_progress_report(snd_info *sp, int from_enved) {}
void finish_progress_report(snd_info *sp, int from_enved) {}
void progress_report(snd_info *sp, const char *funcname, int curchan, int chans, Float pct, int from_enved) {}
char *read_file_data_choices(file_data *fdat, int *srate, int *chans, int *type, int *format, off_t *location) {return(NULL);}
void alert_new_file(void) {}
snd_info *make_new_file_dialog(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *comment) {return(NULL);}
void make_cur_name_row(int old_size, int new_size) {}
void make_prev_name_row(int old_size, int new_size) {}
void make_prevfiles_list (snd_state *ss) {}
void make_curfiles_list (snd_state *ss) {}
void curfile_highlight(snd_state *ss, int i) {}
void set_file_sort_sensitive(int sensitive) {}
void view_curfiles_set_row_name(int pos) {}
void set_file_browser_play_button(char *name, int state) {}
void highlight_selected_sound(snd_state *ss) {}
int start_file_dialog(snd_state *ss, int width, int height) {return(0);}
int file_dialog_is_active(void) {return(0);}
int edit_header(snd_info *sp) {return(0);}
void make_edit_save_as_dialog(snd_state *ss) {}
void make_file_save_as_dialog(snd_state *ss) {}
axis_info *enved_make_axis(snd_state *ss, char *name, axis_context *ax, int ex0, int ey0, int width, int height, 
			   Float xmin, Float xmax, Float ymin, Float ymax, int printing) {return(NULL);}
void display_enved_env_with_selection(snd_state *ss, env *e, char *name, int x0, int y0, int width, int height, int dots, Float base, int printing) {}
void set_enved_redo_sensitive(int val) {}
void set_enved_revert_sensitive(int val) {}
void set_enved_undo_sensitive(int val) {}
void set_enved_save_sensitive(int val) {}
void set_enved_show_sensitive(int val) {}
void enved_fft_update(void) {}
void make_scrolled_env_list (snd_state *ss) {}
void new_active_channel_alert(snd_state *ss) {}
void env_redisplay(snd_state *ss) {}
void env_redisplay_with_print(snd_state *ss) {}
void enved_display_point_label(snd_state *ss, Float x, Float y) {}
void set_enved_click_to_delete(int n) {}
int create_envelope_editor (snd_state *ss) {return(0);}
void set_enved_clip_p(snd_state *ss, int val) {}
void set_enved_exp_p(snd_state *ss, int val) {}
void set_enved_base(snd_state *ss, Float val) {}
void set_enved_target(snd_state *ss, int val) {}
void set_enved_wave_p(snd_state *ss, int val) {}
void set_enved_in_dB(snd_state *ss, int val) {}
int enved_dialog_is_active(void) {return(0);}
void set_enved_filter_order(snd_state *ss, int order) {}
void enved_reflect_selection(int on) {}
void reflect_mix_in_enved(void) {}
void lock_recording_audio(void) {}
void unlock_recording_audio(void) {}
void snd_record_file(snd_state *ss) {}
int record_dialog_is_active(void) {return(0);}
void recorder_error(char *msg) {}
void reflect_record_size(int val) {}
void unsensitize_control_buttons(void) {}
void reflect_recorder_duration(Float new_dur) {}
void make_open_file_dialog(snd_state *ss, int read_only, int managed) {}
void make_mix_file_dialog(snd_state *ss, int managed) {}
void clear_listener(void) {}
int menu_widget(int which_menu) {return(0);}
void get_current_color(int colormap, int j, unsigned short *r, unsigned short *g, unsigned short *b) {}

#define NUM_TRANSFORM_TYPES 9
static char *TRANSFORM_TYPE_CONSTANTS[NUM_TRANSFORM_TYPES] =
  {S_fourier_transform, S_wavelet_transform, S_hankel_transform, S_walsh_transform, 
   S_autocorrelation, S_chebyshev_transform, S_cepstrum, S_hadamard_transform, S_haar_transform};
char *transform_type_name(int choice) {return(TRANSFORM_TYPE_CONSTANTS[choice]);}

int add_transform_to_list(char *name) {return(0);}
void set_filter_text(snd_info *sp, char *str) {}
int max_transform_type(void) {return(19);}

void reflect_mix_in_mix_panel(int mix_id) {}
void reflect_no_mix_in_mix_panel(void) {}
int make_mix_panel(snd_state *ss) {return(0);}
int mix_play_stopped(void) {return(0);}
void reflect_mix_play_stop(void) {}
void set_fft_window_beta(snd_state *ss, Float val) {in_set_fft_window_beta(ss, val);}
void set_transform_size(snd_state *ss, int val) {in_set_transform_size(ss, val);}
void set_fft_window(snd_state *ss, int val) {in_set_fft_window(ss, val);}
void set_transform_type(snd_state *ss, int val) {in_set_transform_type(ss, val);}
void set_wavelet_type(snd_state *ss, int val) {in_set_wavelet_type(ss, val);}
void set_transform_graph_type(snd_state *ss, int val) {in_set_transform_graph_type(ss, val);}
void set_snd_amp(snd_info *sp, Float val) {sp->amp_control = val;}
void set_snd_expand(snd_info *sp, Float val) {sp->expand_control = val;}
void set_snd_contrast(snd_info *sp, Float val) {sp->contrast_control = val;}
void set_snd_srate(snd_info *sp, Float val) {sp->speed_control = val;}
void set_snd_revlen(snd_info *sp, Float val) {sp->reverb_control_length = val;}
void set_snd_revscl(snd_info *sp, Float val) {sp->reverb_control_scale = val;}
void set_snd_filter_order(snd_info *sp, int val) {sp->filter_control_order = val;}
void set_filter_in_dB(snd_info *sp, int val) {sp->filter_control_in_dB = val;}

void reflect_recorder_mixer_gain(int ind, Float val) {}
void reflect_recorder_out_amp(int ind, Float val) {}
void reflect_recorder_in_amp(int in, int out, Float val) {}

snd_info *add_sound_window (char *filename, snd_state *ss, int read_only)
{
  snd_info *sp;
  file_info *hdr;
  int snd_slot, nchans, i;
  hdr = make_file_info(filename, ss);
  if (!hdr) return(NULL);
  if (ss->pending_change) 
    {
      filename = ss->pending_change;
      ss->pending_change = NULL;
    }
  nchans = hdr->chans;
  if (nchans <= 0) nchans = 1;
  if (nchans > 256)
    {
      /* either a screwed up header, or Snd was built with wrong endianess */
      /* this kind of error is trapped by raw_data_explanation in make_file_info in the motif/gtk cases */
      fprintf(stderr, "%s has %d channels? ", filename, nchans);
      if (mus_char_to_bint((unsigned char *)&nchans) < 8)
	fprintf(stderr, "byte swap problem: chans should be %d ", mus_char_to_bint((unsigned char *)&nchans));
      if (mus_char_to_lint((unsigned char *)&nchans) < 8)
	fprintf(stderr, "byte swap problem: chans should be %d ", mus_char_to_lint((unsigned char *)&nchans));
      nchans = 1; /* ?? */
    }
  snd_slot = find_free_sound_slot(ss, nchans); /* expands sound list if needed */
  ss->sounds[snd_slot] = make_snd_info(ss->sounds[snd_slot], ss, filename, hdr, snd_slot, read_only);
  sp = ss->sounds[snd_slot];
  for (i = 0; i < nchans; i++) sp->chans[i] = make_chan_info(sp->chans[i], i, sp, ss);
  add_sound_data(filename, sp, ss, WITHOUT_GRAPH);
  after_open(sp->index);
  return(sp);
}

static XEN menu_hook;
static char **auto_open_file_names = NULL;
static int auto_open_files = 0;
static int noglob = 0, noinit = 0;

#if TRAP_SEGFAULT
#include <setjmp.h>
/* stolen from scwm.c */
static sigjmp_buf envHandleEventsLoop;

static RETSIGTYPE segv(int ignored)
{
  siglongjmp(envHandleEventsLoop, 1);
}
#endif

#define FALLBACK_FONT "9x15"

void snd_doit(snd_state *ss, int argc, char **argv)
{
  static int auto_open_ctr = 0;
  int i;
  ss->sgx = (state_context *)CALLOC(1, sizeof(state_context));

  ss->init_file = copy_string(getenv(SND_INIT_FILE_ENVIRONMENT_NAME));
  if (ss->init_file == NULL)
    ss->init_file = INIT_FILE_NAME;

#if HAVE_GUILE
  XEN_EVAL_C_STRING("(set! scm-repl-prompt \"snd> \")");

  XEN_EVAL_C_STRING("(define (" S_region_dialog " . args) #f)\
               (define (" S_in " . args) #f)\
               (define (" S_make_color " . args) #f)\
               (define (" S_color_p " . args) #f)\
               (define (" S_color2list " .args) #f)");

  XEN_EVAL_C_STRING("(define (set-" S_enved_active_env " obj) obj)\
               (define (set-" S_enved_selected_env " obj) obj)\
               (define (set-" S_just_sounds " obj) obj)\
               (define (set-" S_html_dir " obj) obj)\
               (define (set-" S_basic_color " obj) obj)\
               (define (set-" S_zoom_color " obj) obj)\
               (define (set-" S_position_color " obj) obj)\
               (define (set-" S_mark_color " obj) obj)\
               (define (set-" S_listener_color " obj) obj)\
               (define (set-" S_listener_text_color " obj) obj)\
               (define (set-" S_selected_mix_color " obj) obj)\
               (define (set-" S_enved_waveform_color " obj) obj)\
               (define (set-" S_filter_waveform_color " obj) obj)\
               (define (set-" S_highlight_color " obj) obj)\
               (define (set-" S_graph_color " obj) obj)\
               (define (set-" S_selected_graph_color " obj) obj)\
               (define (set-" S_data_color " obj) obj)\
               (define (set-" S_selected_data_color " obj) obj)\
               (define (set-" S_cursor_color " obj) obj)\
               (define (set-" S_selection_color " obj) obj)\
               (define (set-" S_pushed_button_color " obj) obj)\
               (define (set-" S_text_focus_color " obj) obj)\
               (define (set-" S_sash_color " obj) obj)\
               (define (set-" S_graph_cursor " obj) obj)\
               (define (set-" S_mix_color " . args) #f)\
               (define (set-" S_selected_mix_color " . args) #f)");

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
               (define " S_menu_hook " (make-hook 2))\
               (define " S_property_changed_hook " (make-hook 1))");

  XEN_EVAL_C_STRING("(define " S_enved_active_env " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_enved_selected_env " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_just_sounds " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_html_dir " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_copy_context " 0)\
               (define " S_cursor_context " 3)\
               (define " S_selection_context " 2)\
               (define (" S_axis_info " . args) #f)\
               (define (" S_change_property " . args) #f)\
               (define (" S_dialog_widgets ") #f)\
               (define (" S_channel_widgets " . args) #f)\
               (define (" S_sound_widgets " . args) #f)\
               (define (" S_menu_widgets " . args) #f)\
               (define (" S_main_widgets " . args) #f)\
               (define (" S_current_font ") #f)\
               (define (" S_reset_listener_cursor ") #f)\
               (define (set-" S_enved_filter " val) #f)\
               (define " S_basic_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_zoom_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_position_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_mark_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_listener_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_listener_text_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_selected_mix_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_enved_waveform_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_enved_filter " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
               (define " S_filter_waveform_color " (make-procedure-with-setter (lambda () #f) (lambda (val) val)))\
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
               (define " S_mix_color " (make-procedure-with-setter (lambda args #f) (lambda args #f)))\
               (define " S_selected_mix_color " (make-procedure-with-setter (lambda args #f) (lambda args #f)))");
#endif
#if HAVE_RUBY
  XEN_EVAL_C_STRING("def region_dialog () false end");
  XEN_EVAL_C_STRING("def make_color (r g b) false end");
  XEN_EVAL_C_STRING("def color_p (a) false end");
  XEN_EVAL_C_STRING("def color2list (a) false end");
  XEN_EVAL_C_STRING("def axis_info (s c a) false end");
  XEN_EVAL_C_STRING("def change_property (s) false end");
  XEN_EVAL_C_STRING("def dialog_widgets () false end");
  XEN_EVAL_C_STRING("def channel_widgets (s c) false end");
  XEN_EVAL_C_STRING("def sound_widgets (s) false end");
  XEN_EVAL_C_STRING("def menu_widgets (s) false end");
  XEN_EVAL_C_STRING("def main_widgets (s) false end");
  XEN_EVAL_C_STRING("def current_font () false end");
  XEN_EVAL_C_STRING("def reset_listener_cursor () false end");

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
  XEN_EVAL_C_STRING("$menu_hook = false");
  XEN_EVAL_C_STRING("$property_changed_hook = false");

  XEN_EVAL_C_STRING("Copy_context = 0");
  XEN_EVAL_C_STRING("Cursor_context = 3");
  XEN_EVAL_C_STRING("Selection_context = 2");
#endif
  set_button_font(ss, FALLBACK_FONT);
  set_tiny_font(ss, FALLBACK_FONT);
  set_bold_button_font(ss, FALLBACK_FONT);
  set_axis_label_font(ss, FALLBACK_FONT);
  set_axis_numbers_font(ss, FALLBACK_FONT);
  set_help_text_font(ss, FALLBACK_FONT);
  set_listener_font(ss, FALLBACK_FONT);

  XEN_DEFINE_HOOK(menu_hook, S_menu_hook, 2, NULL);

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "-noglob") == 0)
	noglob = 1;
      else
	if (strcmp(argv[i], "-noinit") == 0)
	  noinit = 1;
    }
  snd_load_init_file(ss, noglob, noinit);
#if HAVE_SIGNAL
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN);
#endif
  auto_open_files = argc - 1;
  if (argc > 1) auto_open_file_names = (char **)(argv + 1);
  while (auto_open_ctr < auto_open_files)
    auto_open_ctr = handle_next_startup_arg(ss, auto_open_ctr, auto_open_file_names, FALSE, auto_open_files);
#if TRAP_SEGFAULT
  if (trap_segfault(ss)) signal(SIGSEGV, segv);
#endif
  if ((ss->sounds) && (ss->sounds[0]) && ((ss->sounds[0])->inuse)) 
    select_channel(ss->sounds[0], 0);

#if TRAP_SEGFAULT
  if (sigsetjmp(envHandleEventsLoop, 1))
    {
      snd_error("Caught seg fault (will try to continue):\n");
      show_stack();
    }
#endif

  xen_repl(1, argv);
}

