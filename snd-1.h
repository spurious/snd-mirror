#ifndef SND_1_H
#define SND_1_H

#define ASSERT_SOUND(Origin, Snd, Offset) \
  if (!((XEN_INTEGER_P(Snd)) || (XEN_BOOLEAN_P(Snd)) || (XEN_NOT_BOUND_P(Snd)))) \
    XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset, Snd, "an integer (sound index), or a boolean");

#define ASSERT_CHANNEL(Origin, Snd, Chn, Offset) \
  if (!((XEN_INTEGER_P(Snd)) || (XEN_BOOLEAN_P(Snd)) || (XEN_NOT_BOUND_P(Snd)))) \
    XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset, Snd, "an integer (sound index), or a boolean"); \
  else \
    if (!((XEN_INTEGER_P(Chn)) || (XEN_BOOLEAN_P(Chn)) || (XEN_NOT_BOUND_P(Chn)))) \
      XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset + 1, Chn, "an integer (0-based channel number) or boolean");

#if HAVE_GUILE
#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name) \
static XEN name_reversed(XEN arg1, XEN arg2, XEN arg3) \
{ \
  if (XEN_NOT_BOUND_P(arg2)) \
    return(name(arg1, XEN_UNDEFINED, XEN_UNDEFINED)); \
  else { \
    if (XEN_NOT_BOUND_P(arg3)) \
      return(name(arg2, arg1, XEN_UNDEFINED)); \
    else return(name(arg3, arg1, arg2)); \
}}
#else
#define WITH_REVERSED_CHANNEL_ARGS(name_reversed, name)
#endif

#define ASSERT_SAMPLE_TYPE(Origin, Beg, Offset) \
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P(Beg), Beg, Offset, Origin, "a number or #f")

typedef struct {
  int samps_per_bin, amp_env_size;
  mus_sample_t fmax, fmin;
  mus_sample_t *data_max, *data_min;
  bool completed;
  int bin, top_bin;
} env_info;

typedef struct {
  int fd, chans, bufsize;
  off_t frames, beg, end;
  mus_sample_t **arrays;
} snd_io;

typedef struct {
  char *name;             /* full name */
  off_t samples;          /* total samples = chans * frames */
  off_t data_location;    /* bytes */
  int srate;
  int chans;
  int format;             /* data format (snd_16_linear etc) */
  int type;               /* header type (AIFF etc) */
  char *comment;          /* output case, not input */
  int *loops;
} file_info;

typedef struct {
  snd_data_file_t type;
  mus_sample_t *buffered_data;    
  snd_io *io;      
  char *filename;
  file_info *hdr;
  file_delete_t temporary;
  int edit_ctr;
  fd_open_t open;
  bool inuse;
  bool copy;
  int chan;
  off_t len;
  bool free_me;
} snd_data;

typedef struct {
  int size, allocated_size;
  void **fragments; /* ed_fragment** */
  off_t beg, len;
  char *origin;
  int edit_type, sound_location, ptree_location;
  off_t selection_beg, selection_end;  /* selection needs to follow edit list */
  Float maxamp, selection_maxamp;
  int edpos;
  bool ptree_env_too;
} ed_list;

typedef struct snd_fd {
  mus_sample_t (*run)(struct snd_fd *sf);
  Float (*runf)(struct snd_fd *sf);
  ed_list *current_state;
  void *cb; /* ed_fragment* */
  off_t loc, first, last;
  int cbi;
  read_direction_t direction;
  bool at_eof;
  mus_sample_t *data;
  snd_data *current_sound;
  off_t initial_samp; /* only real use (outside descriptions) is in apply-ladspa (sigh...) */
  struct chan_info *cp;
  struct snd_info *local_sp;
  Float fscaler, rscaler;
  int iscaler;
  off_t frag_pos;
  double incr, curval;
  void *ptree, *ptree1;
  XEN closure, closure1;
  int protect, protect1;
  double incr2, curval2, incr3, curval3;
  mus_sample_t (*rev_run)(struct snd_fd *sf);
  Float (*rev_runf)(struct snd_fd *sf);
  int edit_ctr, dangling_loc;
} snd_fd;

typedef struct {Float freq; Float amp;} fft_peak;

typedef struct {
  Float y0, y1;                         /* scroller-dependent axis bounds */
  double x0, x1;
  double xmin, xmax;
  Float ymin, ymax;                     /* data-dependent absolute limits */
  Float y_scale, y_base, y_ambit;
  double x_scale, x_base, x_ambit;
  char *xlabel;
  Locus y_axis_x0, x_axis_x0,
        y_axis_y0, x_axis_y0,
        x_axis_x1,
        y_axis_y1,
        x_label_x, x_label_y;
  bool graph_active;
  off_t losamp, hisamp;                 /* displayed x-axis bounds in terms of sound sample numbers */
  Locus graph_x0;                       /* x axis offset relative to window (for double graphs) */
  void *x_ticks, *y_ticks;              /* actual type is tick_descriptor local to snd-axis.c */
  axis_context *ax;
  Latus width, height;
  struct chan_info *cp;
  Float sy, zy;                         /* as set by user, 0.0 - 1.0 */
  double sx, zx;
  Locus y_offset;
  Latus window_width;
  bool no_data, changed;
#if HAVE_GL
  bool use_gl, used_gl;
#endif
} axis_info;

typedef struct {
  off_t samp;
  char *name;
  unsigned int id, sync;
} mark;

typedef struct {
  Float *data;
  int pts, data_size;
} env;

typedef struct {
  int size;
  int current_size;
  mus_fft_window_t window;
  bool ok;
  Float beta;
  Float scale;
  axis_info *axis;
  Float *data;
  char *xlabel;
  struct chan_info *chan;
} fft_info;

typedef struct {
  int total_slices;        /* size of the data array (max for allocation checks) */
  int total_bins;          /* size other axis data array */
  int active_slices;       /* how many slices contain current data */
  int target_bins;         /* this many bins Y-side */
  int target_slices;       /* how many slices in full display (current) */
  Float **data;            /* data[total_slices][bins] -> each is a spectral magnitude */
  off_t *begs;             /* associated begin times (for data reuse) */
  struct chan_info *cp;
  Float scale;
} sono_info;

typedef struct {
  int size;
  short *ids, *locs;
} track_info;

typedef struct chan_info {
  int chan;                /* which chan are we */
  off_t *samples;          /* current length */
  bool graph_transform_p;  /* f button state */
  bool graph_time_p;       /* w button state */
  bool graph_lisp_p;       /* is lisp graph active */
  void *lisp_info;
  bool cursor_on;          /* channel's cursor */
  bool cursor_visible;     /* for XOR decisions */
  off_t *cursors;          /* sample number (follows edit history) */
  int cursor_size;
  cursor_style_t cursor_style;
  int cx, cy;               /* graph-relative cursor loc (for XOR) */
  int edit_ctr;            /* channel's edit history */
  int edit_size;           /* current edit list size */
  ed_list **edits;         /* the edit list */
  int sound_size;          /* edit_list associated temp sound buffers */
  int sound_ctr;           /* current location in sounds list */
  snd_data **sounds;       /* the associated temp buffer/file/struct list */
  int ptree_size;          /* ditto for ptrees */
  int ptree_ctr;
  void **ptrees;
  XEN *ptree_inits, *xens;
  fft_info *fft;           /* possibly null fft data */
  struct snd_info *sound; /* containing sound */
  axis_info *axis;         /* time domain axis */
  mark ***marks;           /* current marks, indexed by edit_ctr, then mark_number, then the mark pointer */
  int marks_size;
  int *mark_size;
  int *mark_ctr;
  chan_context *cgx;       /* graphics/window context */
  chan_context *tcgx;      /* when combining chans, all should use chan[0]'s context */
  env_info **amp_envs;
  void *sonogram_data;
  void *last_sonogram, *temp_sonogram;
  void *fft_data;          /* parallels sonogram -- try to avoid repeating large ffts needlessly */
  bool printing;
  fft_change_t fft_changed;
  Float gsy, gzy;
  void *mix_dragging;
  int height, mixes;
  off_t original_cursor;   /* for cursor reset after cursor-moving play */
  bool hookable;
  int selection_transform_size;
  bool squelch_update, previous_squelch_update, waiting_to_make_graph, in_as_one_edit;
  /* moved from global to channel-local 4-Aug-00 */
  Float spectro_x_scale, spectro_y_scale, spectro_z_scale, spectro_z_angle, spectro_x_angle, spectro_y_angle, spectro_cutoff, spectro_start;
  Float lin_dB, min_dB, fft_window_beta, beats_per_minute;
  bool show_y_zero, show_marks, verbose_cursor;
  int wavo_hop, wavo_trace, zero_pad, wavelet_type, max_transform_peaks;
  x_axis_style_t x_axis_style;
  show_axes_t show_axes;
  int transform_size;
  mus_fft_window_t fft_window;
  graph_type_t transform_graph_type, time_graph_type;
  bool show_transform_peaks, fft_log_frequency, fft_log_magnitude;
  graph_style_t time_graph_style, lisp_graph_style, transform_graph_style;
  Latus dot_size;
  fft_normalize_t transform_normalization;
  int transform_type, spectro_hop, edhist_base;
  bool show_mix_waveforms, graphs_horizontal;
  XEN edit_hook;
  XEN undo_hook;
  XEN cursor_proc;
  XEN after_edit_hook;
  XEN properties;
  bool selection_visible, active;
  Locus old_x0, old_x1;
  Float *amp_control; /* local amp controls in snd-dac; should it be extended to other controls? */
  search_result_t last_search_result;
  bool just_zero;
  track_info **tracks;
#if HAVE_GL
  int gl_fft_list;
#endif
} chan_info;

#define CURRENT_SAMPLES(Cp) (Cp)->samples[(Cp)->edit_ctr]
#define CURSOR(Cp) (Cp)->cursors[(Cp)->edit_ctr]

typedef struct snd_info {
  sound_inuse_t inuse;
  int index;
  int playing;
  mark *playing_mark;
  int sync, previous_sync;
  bool expand_control_p;
  bool contrast_control_p;
  bool reverb_control_p;
  bool filter_control_p, filter_control_in_dB;
  Float amp_control;
  Float speed_control;
  int speed_control_direction, speed_control_tones;
  speed_style_t speed_control_style;
  Float last_speed_control, last_amp_control, last_expand_control, last_contrast_control;
  Float last_reverb_control_length, last_reverb_control_scale;
  Float saved_speed_control, saved_amp_control, saved_expand_control, saved_contrast_control;
  Float saved_reverb_control_length, saved_reverb_control_scale;
  Float expand_control, expand_control_length, expand_control_ramp, expand_control_hop;
  Float contrast_control, contrast_control_amp;
  Float reverb_control_length, reverb_control_scale, reverb_control_feedback, reverb_control_lowpass;
  Float reverb_control_decay, filter_control_env_xmax;
  int filter_control_order;
  bool filter_control_changed;
  env *filter_control_env;
  int selected_channel;
  char *filename;
  char *short_filename;
  int nchans;
  void *search_tree;
  XEN search_proc;
  XEN prompt_callback;
  XEN properties;
  bool raw_prompt;
  char *search_expr;
  off_t marking;
  int searching, amping;
  sp_filing_t filing;
  bool prompting, loading, finding_mark, printing, selectioning;
  off_t macroing;
  minibuffer_choice_t minibuffer_on;
  bool read_only;
  chan_info **chans;
  snd_context *sgx;
  file_info *hdr;             /* header of file that would be affected if we were to save current edits */
  int bomb_ctr;
  time_t write_date;          /* check for change behind back while editing */
  bool need_update;           /* current in-core data does not match actual file (someone wrote it behind our back) */
  channel_style_t channel_style;
  int allocated_chans;        /* snd_info widget tree is never pruned -- can only grow */
  tracking_cursor_t cursor_follows_play;
  void *edited_region;
  void *delete_me;
  chan_info *lacp;
  void *saved_controls;
  bool apply_ok, applying;
  /* moved from global to channel-local 4-Aug-00 */
  void *minibuffer_history, *filter_history;
  bool active;
  char *name_string;
} snd_info;

#define SND_SRATE(sp) (((sp)->hdr)->srate)

typedef struct snd_state {
  int selected_sound, selected_mix;         /* NO_SELECTION = none selected = which sound is currently receiving user's attention */
  int active_sounds;
  Latus ctrls_height, open_ctrls_height, channel_min_height;
  snd_info **sounds;
  char *search_expr, *startup_title;
  void *search_tree;
  XEN search_proc;
  XEN file_sort_proc;
  int catch_exists;
  char *catch_message;
#if (!USE_GTK)
  bool using_schemes;
#endif
  state_context *sgx;
  int position_slider_width, zoom_slider_width, toggle_size, enved_point_size, channel_sash_indent, sash_size, channel_sash_size, sash_indent;
  char *init_file;
  int max_sounds;
  snd_info *mx_sp;
  char *pending_change;
  print_choice_t print_choice;
  snd_apply_t apply_choice;
  bool gl_has_double_buffer, just_time;
  bool stopped_explicitly, checking_explicitly;
  int reloading_updated_file;
  Latus init_window_width, init_window_height;
  Locus init_window_x, init_window_y;
  bool graph_hook_active, lisp_graph_hook_active;
  bool Show_Transform_Peaks, Show_Y_Zero, Show_Marks;
  bool Fft_Log_Frequency, Fft_Log_Magnitude;
  channel_style_t Channel_Style;
  sound_style_t Sound_Style;
  show_axes_t Show_Axes;
  char *Eps_File, *Temp_Dir, *Save_Dir, *Ladspa_Dir;
  char *Listener_Font, *Axis_Label_Font, *Axis_Numbers_Font, *Tiny_Font, *Peaks_Font, *Bold_Peaks_Font;
  bool Verbose_Cursor, Trap_Segfault;
  int Enved_Filter_Order;
  bool Filter_Env_In_Hz;  /* for spectral envelopes from the envelope editor */
  Float Vu_Size, Vu_Font_Size, Eps_Left_Margin, Eps_Bottom_Margin, Eps_Size;
  char *Vu_Font;
  Float Spectro_X_Scale, Spectro_Y_Scale, Spectro_Z_Scale, Spectro_Z_Angle, Spectro_X_Angle, Spectro_Y_Angle, Spectro_Cutoff, Spectro_Start;
  int Default_Output_Type, Default_Output_Format, Default_Output_Chans, Default_Output_Srate;
  int Spectro_Hop, Color_Map, Wavelet_Type, Transform_Type, Optimization;
  Latus Dot_Size;
  int Transform_Size, Zero_Pad, Wavo_Hop, Wavo_Trace;
  mus_fft_window_t Fft_Window;
  graph_type_t Transform_Graph_Type, Time_Graph_Type;
  bool Ask_Before_Overwrite;
  Float Fft_Window_Beta, Reverb_Control_Decay;
  Float Color_Scale, Color_Cutoff, Beats_Per_Minute;
  bool Color_Inverted, Show_Mix_Waveforms;
  speed_style_t Speed_Control_Style;
  int Mix_Waveform_Height;
  fft_normalize_t Transform_Normalization;
  int Speed_Control_Tones, Sinc_Width;
  x_axis_style_t X_Axis_Style;
  zoom_focus_t Zoom_Focus_Style;
  graph_style_t Graph_Style, Region_Graph_Style;
  bool Auto_Resize, Auto_Update;
  int Max_Regions, Max_Transform_Peaks;
  int Audio_Output_Device, Audio_Input_Device;
  bool Show_Backtrace, Emacs_Style_Save_As, With_GL, With_Relative_Panes;
  int Print_Length, Dac_Size, Previous_Files_Sort;
  bool Dac_Combines_Channels, Show_Selection_Transform, With_Mix_Tags, Selection_Creates_Region;
  char *Save_State_File, *Listener_Prompt;
  Float Enved_Base, Enved_Power, Auto_Update_Interval;
  bool Enved_Clip_p, Enved_Exp_p, Enved_Wave_p, Enved_in_dB, Graphs_Horizontal, With_Background_Processes;
  int Graph_Cursor, Mix_Tag_Width, Mix_Tag_Height, Minibuffer_History_Length;
  enved_target_t Enved_Target;
  bool Data_Clipped, Show_Indices;
  int Cursor_Size;
  cursor_style_t Cursor_Style;
  XEN cursor_proc;
  Float min_dB, lin_dB;
  char *HTML_Dir, *HTML_Program;
  bool error_lock;
  int deferred_regions;
  bool batch_mode;
  bool jump_ok, exiting, just_sounds_state;
} snd_state;

extern snd_state *ss;

typedef struct {
  char **files;
  char *name;
  int len;
  int size;
} dir;

typedef struct {
  int chans;
  off_t *begs;
  chan_info **cps;
} sync_info;

typedef struct {
  int len;
  char **name;
} region_state;

typedef struct {
  mus_any *gen;
  snd_fd *sf;
  off_t sample;
  int dir;
} src_state;



/* -------- snd-io.c -------- */

int snd_open_read(const char *arg);
int snd_reopen_write(const char *arg);
int snd_write_header(const char *name, int type, int srate, int chans, off_t loc, off_t samples, int format, const char *comment, int len, int *loops);
bool snd_overwrite_ok(const char *ofile);
snd_io *make_file_state(int fd, file_info *hdr, int chan, int suggested_bufsize);
void file_buffers_forward(off_t ind0, off_t ind1, off_t indx, snd_fd *sf, snd_data *cur_snd);
void file_buffers_back(off_t ind0, off_t ind1, off_t indx, snd_fd *sf, snd_data *cur_snd);
int snd_remove(const char *name, cache_remove_t forget);
int snd_close(int fd, const char *name);
int snd_fclose(FILE *fd, const char *name);
void remember_temp(const char *filename, int chans);
void forget_temps(void);
snd_data *make_snd_data_file(const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int ctr, int temp_chan);
snd_data *copy_snd_data(snd_data *sd, int bufsize);
snd_data *free_snd_data(snd_data *sf);
snd_data *make_snd_data_buffer(mus_sample_t *data, int len, int ctr);
snd_data *make_snd_data_buffer_for_simple_channel(int len);
int open_temp_file(const char *ofile, int chans, file_info *hdr);
int close_temp_file(int ofd, file_info *hdr, off_t bytes, snd_info *sp);


/* -------- snd-help.c -------- */

void about_snd_help(void);
void controls_help(void);
void fft_help(void);
void find_help(void);
void undo_help(void);
void sync_help(void);
void debug_help(void);
void env_help(void);
void marks_help(void);
void mix_help(void);
void sound_files_help(void);
void init_file_help(void);
void recording_help(void);
char *version_info(void);
void transform_dialog_help(void);
void color_dialog_help(void);
void orientation_dialog_help(void);
void envelope_editor_dialog_help(void);
void region_dialog_help(void);
void raw_data_dialog_help(void);
void new_file_dialog_help(void);
void edit_header_dialog_help(void);
void print_dialog_help(void);
void view_files_dialog_help(void);
void mix_dialog_help(void);
void find_dialog_help(void);
void completion_dialog_help(void);
void open_file_dialog_help(void);
void save_as_dialog_help(void);
char* word_wrap(const char *text, int widget_len);
void g_init_help(void);
XEN g_snd_help(XEN text, int widget_wid);
char *snd_url(const char *name);
void set_html_dir(char *new_dir);
void key_binding_help(void);
void play_help(void);
void save_help(void);
void reverb_help(void);
void resample_help(void);
void filter_help(void);
void insert_help(void);
void delete_help(void);
void name_to_html_viewer(char *red_text);
void url_to_html_viewer(char *url);
bool snd_topic_help(const char *topic);
char **help_name_to_xrefs(const char *name);


/* -------- snd-menu.c -------- */

void reflect_file_open_in_menu (void);
void reflect_file_change_in_menu (void);
void reflect_file_lack_in_menu (void);
void reflect_mix_in_menu(void);
void reflect_equalize_panes_in_menu(bool on);
void reflect_file_revert_in_menu (void);
void reflect_file_save_in_menu (void);
void reflect_file_revert_in_label (snd_info *sp);
void reflect_no_more_redo_in_menu(void);
void reflect_edit_with_selection_in_menu (void);
void reflect_edit_without_selection_in_menu (void);
void reflect_undo_in_menu(void);
void reflect_redo_in_menu(void);
void reflect_undo_ok_in_menu(void);
void reflect_undo_or_redo_in_menu(chan_info *cp);
void reflect_regions_in_menu(void);
void reflect_no_regions_in_menu(void);
void reflect_raw_open_in_menu(void);
void reflect_raw_pending_in_menu(void);

void close_file_from_menu(void);
void save_file_from_menu(void);
void update_file_from_menu(void);
void revert_file_from_menu(void);
void exit_from_menu(void);
void save_options_from_menu(void);
void save_state_from_menu(void);
snd_info *new_file_from_menu(void);
void unprotect_callback(int slot);
void set_graph_style(graph_style_t val);
void set_show_marks(bool val);
void set_show_y_zero(bool val);
void set_verbose_cursor(bool val);
void set_view_ctrls_label(const char *lab);
void set_view_listener_label(const char *lab);
void activate_focus_menu(zoom_focus_t new_focus);
void activate_speed_in_menu(speed_style_t newval);
void set_x_axis_style(x_axis_style_t val);
void set_channel_style(channel_style_t val);
void chans_x_axis_style(chan_info *cp, void *ptr);

void g_init_menu(void);


/* -------- snd-main.c -------- */

void open_save_sound_block(snd_info *sp, FILE *fd, bool with_nth);
void close_save_sound_block(FILE *fd);
int snd_exit_cleanly(bool force_exit);
void sound_not_current(snd_info *sp, void *dat);
int save_options (void);
FILE *open_snd_init_file (void);
int save_state (char *save_state_name);
int handle_next_startup_arg(int auto_open_ctr, char **auto_open_file_names, bool with_title, int args);

void g_init_main(void);


/* --------- snd-error.c -------- */

#ifdef __GNUC__
  void snd_error(char *format, ...)  __attribute__ ((format (printf, 1, 2)));
  void snd_warning(char *format, ...)  __attribute__ ((format (printf, 1, 2)));
#else
  void snd_error(char *format, ...);
  void snd_warning(char *format, ...);
#endif

void g_init_errors(void);

#ifdef SND_AS_WIDGET
  void set_snd_error_display (void (*func)(const char *));
#endif


/* -------- snd-completion.c -------- */

char *command_completer(char *text);
int add_completer_func(char *(*func)(char *));
int get_completion_matches(void);
void set_completion_matches(int matches);
void set_save_completions(bool save);
void add_possible_completion(char *text);
void display_completions(void);
char *complete_text(char *text, int func);
void clear_possible_completions(void);
char *filename_completer(char *text);
char *srate_completer(char *text);
char *info_completer(char *text);
char *complete_listener_text(char *old_text, int end, bool *try_completion, char **to_file_text);
bool separator_char_p(char c);


/* -------- snd-print.c -------- */

void ps_set_grf_points(double x, int j, Float ymin, Float ymax);
void ps_set_grf_point(double x, int j, Float y);
void ps_allocate_grf_points(void);
void ps_draw_grf_points(axis_info *ap, int j, Float y0, graph_style_t graph_style, int dot_size);
void ps_draw_both_grf_points(axis_info *ap, int j, graph_style_t graph_style, int dot_size);
void ps_draw_sono_rectangle(axis_info *ap, int color, Float x, Float y, Float width, Float height);
void ps_reset_color(void);
void ps_bg(axis_info *ap, axis_context *ax);
void ps_fg(axis_info *ap, axis_context *ax);
void ps_draw_line (axis_info *ap, int x0, int y0, int x1, int y1);
void ps_draw_spectro_line(axis_info *ap, int color, Float x0, Float y0, Float x1, Float y1);
void ps_fill_rectangle (axis_info *ap, int x0, int y0, int width, int height);
void ps_draw_string (axis_info *ap, int x0, int y0, char *str);
void ps_set_number_font(void);
void ps_set_label_font(void);
void ps_set_bold_peak_numbers_font(void);
void ps_set_peak_numbers_font(void);
void ps_set_tiny_numbers_font(void);
void snd_print(char *output);
void region_print(char *output, char* title, chan_info *cp);
void print_enved(char *output, int y0);
void g_init_print(void);



/* -------- snd-marks.c -------- */

int mark_id(mark *m);
int mark_sync_max(void);
int set_mark_sync(mark *m, int val);
void marks_off(chan_info *cp);
mark *hit_mark(chan_info *cp, int x, int y, int key_state);
mark *hit_triangle(chan_info *cp, int x, int y);
void move_mark(chan_info *cp, mark *mp, int x);
void play_syncd_mark(chan_info *cp, mark *mp);
off_t move_play_mark(chan_info *cp, off_t *mc, Locus cx);
void finish_moving_play_mark(chan_info *cp);
void finish_moving_mark(chan_info *cp, mark *m);
mark *add_mark(off_t samp, const char *name, chan_info *cp);
void delete_mark_samp(off_t samp, chan_info *cp);
void free_mark_list(chan_info *cp, int ignore);
void collapse_marks (snd_info *sp);
void goto_mark(chan_info *cp, int count);
void goto_named_mark(chan_info *cp, const char *name);
mark *active_mark(chan_info *cp);
off_t mark_beg(chan_info *cp);
void display_channel_marks(chan_info *cp);
void release_pending_marks(chan_info *cp, int edit_ctr);
void ripple_marks(chan_info *cp, off_t beg, off_t change);
void mark_define_region(chan_info *cp, int count);
void save_mark_list(FILE *fd, chan_info *cp);
void reverse_marks(chan_info *cp, off_t beg, off_t dur);
void src_marks(chan_info *cp, Float ratio, off_t old_samps, off_t new_samps, off_t beg, bool over_selection);
void reset_marks(chan_info *cp, int cur_marks, off_t *samps, off_t end, off_t extension, bool over_selection);
void ripple_trailing_marks(chan_info *cp, off_t beg, off_t old_len, off_t new_len);
void swap_marks(chan_info *cp0, chan_info *cp1);
void g_init_marks(void);
void *sound_store_marks(snd_info *sp);
void sound_restore_marks(snd_info *sp, void *marks);
void backup_mark_list(chan_info *cp, int cur);



/* -------- snd-data.c -------- */

chan_info *make_chan_info(chan_info *cip, int chan, snd_info *sound);
snd_info *make_snd_info(snd_info *sip, const char *filename, file_info *hdr, int snd_slot, bool read_only);
snd_info *make_basic_snd_info(int chans);
void initialize_control_panel(snd_info *sp);
void free_snd_info(snd_info *sp);
snd_info *completely_free_snd_info(snd_info *sp);
bool map_over_sounds (bool (*func)(snd_info *, void *), void *userptr);
bool map_over_chans (bool (*func)(chan_info *, void *), void *userptr);
void for_each_chan(void (*func)(chan_info *));
void for_each_chan_1(void (*func)(chan_info *, void *), void *userptr);
bool map_over_sound_chans (snd_info *sp, bool (*func)(chan_info *, void *), void *userptr);
void for_each_sound_chan(snd_info *sp, void (*func)(chan_info *));
void for_each_sound(void (*func)(snd_info *, void *), void *userptr);
bool map_over_separate_chans(bool (*func)(chan_info *, void *), void *userptr);
bool snd_ok (snd_info *sp);
int active_channels (virtual_channels_t count_virtual_channels);
int find_free_sound_slot (int desired_chans);
int find_free_sound_slot_for_channel_display (void);
snd_info *selected_sound(void);
chan_info *selected_channel(void);
chan_info *color_selected_channel(snd_info *sp);
snd_info *any_selected_sound (void);
chan_info *any_selected_channel(snd_info *sp);
void select_channel(snd_info *sp, int chan);
chan_info *current_channel(void);
sync_info *free_sync_info (sync_info *si);
sync_info *snd_sync(int sync);
sync_info *sync_to_chan(chan_info *cp);
sync_info *make_simple_sync (chan_info *cp, off_t beg);
snd_info *find_sound(const char *name, int nth);
void display_info(snd_info *sp);

void g_init_data(void);



/* -------- snd-edits.c -------- */

void allocate_ed_list(chan_info *cp);
ed_list *initial_ed_list(off_t beg, off_t end);
off_t edit_changes_begin_at(chan_info *cp);
off_t edit_changes_end_at(chan_info *cp);
char *run_save_state_hook(char *filename);
void edit_history_to_file(FILE *fd, chan_info *cp);
char *edit_to_string(chan_info *cp, int edit);
void free_edit_list(chan_info *cp);
void backup_edit_list(chan_info *cp);
void as_one_edit(chan_info *cp, int one_edit, const char *one_edit_origin);
void free_sound_list (chan_info *cp);
void free_ptree_list(chan_info *cp);
void release_dangling_readers(chan_info *cp, int edit_ctr);
void extend_with_zeros(chan_info *cp, off_t beg, off_t num, const char *origin, int edpos);
void file_insert_samples(off_t beg, off_t num, char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin, int edpos);
void delete_samples(off_t beg, off_t num, chan_info *cp, const char *origin, int edpos);
void change_samples(off_t beg, off_t num, mus_sample_t *vals, chan_info *cp, lock_mix_t lock, const char *origin, int edpos);
void file_change_samples(off_t beg, off_t num, char *tempfile, chan_info *cp, int chan, 
			 file_delete_t auto_delete, lock_mix_t lock, const char *origin, int edpos);
void file_override_samples(off_t num, char *tempfile, chan_info *cp, int chan, 
			   file_delete_t auto_delete, lock_mix_t lock, const char *origin);
Float chn_sample(off_t samp, chan_info *cp, int pos);
snd_fd *free_snd_fd(snd_fd *sf);
char *sf_to_string(snd_fd *fd);
void release_region_readers(int reg);
bool sf_p(XEN obj);
snd_fd *get_sf(XEN obj);
snd_fd *free_snd_fd_almost(snd_fd *sf);
void scale_channel(chan_info *cp, Float scaler, off_t beg, off_t num, int pos, bool in_as_one_edit);
void ramp_channel(chan_info *cp, Float rmp0, Float rmp1, off_t beg, off_t num, int pos, bool in_as_one_edit);
void xramp_channel(chan_info *cp, Float rmp0, Float rmp1, Float scaler, Float offset, 
		   off_t beg, off_t num, int pos, bool in_as_one_edit, mus_any *e, int e_pos);
void ptree_channel(chan_info *cp, void *ptree, off_t beg, off_t num, int pos, bool env_it, XEN init_func, bool is_xen);
snd_fd *init_sample_read(off_t samp, chan_info *cp, read_direction_t direction);
snd_fd *init_sample_read_any(off_t samp, chan_info *cp, read_direction_t direction, int edit_position);
void read_sample_change_direction(snd_fd *sf, read_direction_t dir);
bool ramp_or_ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos, Float base);
bool ptree_or_sound_fragments_in_use(chan_info *cp, int pos);
bool ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos, bool is_xen);
#define read_sample(Sf) (*Sf->run)(Sf)
#define read_sample_to_float(Sf) (*Sf->runf)(Sf)
Float protected_next_sample_to_float(snd_fd *sf);
Float protected_previous_sample_to_float(snd_fd *sf);
Float local_maxamp(chan_info *cp, off_t beg, off_t num, int edpos);
void undo_edit_with_sync(chan_info *cp, int count);
void redo_edit_with_sync(chan_info *cp, int count);
void undo_edit(chan_info *cp, int count);
void redo_edit(chan_info *cp, int count);
int save_channel_edits(chan_info *cp, char *ofile, XEN edpos, const char *caller, int arg_pos);
void save_edits(snd_info *sp, void *ptr);
int save_edits_without_display(snd_info *sp, char *new_name, int type, int format, int srate, char *comment, XEN edpos, const char *caller, int arg_pos);
void revert_edits(chan_info *cp, void *ptr);
off_t current_location(snd_fd *sf);
void g_init_edits(void);
void set_ed_maxamp(chan_info *cp, int edpos, Float val);
Float ed_maxamp(chan_info *cp, int edpos);
void set_ed_selection_maxamp(chan_info *cp, Float val);
Float ed_selection_maxamp(chan_info *cp);
void copy_then_swap_channels(chan_info *cp0, chan_info *cp1, int pos0, int pos1);
void reflect_file_change_in_label(chan_info *cp);
void update_track_lists(chan_info *cp);

bool snd2sample_p(mus_any *ptr);
bool xen2sample_p(mus_any *ptr);
Float xen2sample_read(mus_any *ptr, off_t frame, int chan);
Float snd2sample_read(mus_any *ptr, off_t frame, int chan);


/* -------- snd-fft.c -------- */

int find_and_sort_transform_peaks(Float *buf, fft_peak *found, int num_peaks, int fftsize, int srate, Float samps_per_pixel, Float fft_scale);
int find_and_sort_peaks(Float *buf, fft_peak *found, int num_peaks, int size);
fft_info *free_fft_info(fft_info *fp);
void free_sonogram_fft_state(void *ptr);
bool fft_window_beta_in_use(mus_fft_window_t win);
void free_sono_info (chan_info *cp);
void sono_update(chan_info *cp);
void set_spectro_cutoff_and_redisplay(Float val);
void c_convolve(char *fname, Float amp, int filec, off_t filehdr, int filterc, off_t filterhdr, int filtersize,
		 int fftsize, int filter_chans, int filter_chan, int data_size, snd_info *gsp, enved_progress_t from_enved, int ip, int total_chans);
void *make_sonogram_state(chan_info *cp);
void single_fft(chan_info *cp, bool dpy);
Cessate sonogram_in_slices(void *sono);
char *added_transform_name(int type);
void clear_transform_edit_ctrs(chan_info *cp);
void make_fft_graph(chan_info *cp, axis_info *fap, axis_context *ax, bool with_hooks);
void g_init_fft(void);
Float fft_beta_max(mus_fft_window_t win);
void cp_free_fft_state(chan_info *cp);
void autocorrelation(Float *data, int n);
void set_fft_info_xlabel(chan_info *cp, char *new_label);



/* -------- snd-xen.c -------- */

XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller);
XEN snd_throw(XEN key, XEN args);
XEN snd_no_such_file_error(const char *caller, XEN filename);
XEN snd_no_such_channel_error(const char *caller, XEN snd, XEN chn);
XEN snd_bad_arity_error(const char *caller, XEN errstr, XEN proc);
XEN snd_no_active_selection_error(const char *caller);
void g_initialize_gh(void);
XEN eval_str_wrapper(void *data);
XEN eval_form_wrapper(void *data);
XEN string_to_form(void *data);
char *g_print_1(XEN obj);
chan_info *get_cp(XEN snd_n, XEN chn_n, const char *caller);
snd_info *get_sp(XEN snd_n, sp_sound_t accept_player);
XEN g_c_make_sample_reader(snd_fd *fd);
XEN g_call0(XEN proc, const char *caller);
XEN g_call1(XEN proc, XEN arg, const char *caller);
XEN g_call2(XEN proc, XEN arg1, XEN arg2, const char *caller);
XEN g_call3(XEN proc, XEN arg1, XEN arg2, XEN arg3, const char *caller);
XEN g_call_any(XEN proc, XEN arglist, const char *caller);
char *procedure_ok(XEN proc, int args, const char *caller, const char *arg_name, int argn);
int procedure_ok_with_error(XEN proc, int req_args, const char *caller, const char *arg_name, int argn);
int snd_protect(XEN obj);
void snd_unprotect(XEN obj);
void snd_unprotect_at(int loc);
XEN run_or_hook (XEN hook, XEN args, const char *caller);
XEN run_progn_hook (XEN hook, XEN args, const char *caller);
XEN run_hook(XEN hook, XEN args, const char *caller);
void during_open(int fd, char *file, open_reason_t reason);
void after_open(int index);
bool listener_print_p(char *msg);
#if (!USE_NO_GUI)
  Float check_color_range(const char *caller, XEN val);
#endif
int string2int(char *str);
Float string2Float(char *str);
off_t string2off_t(char *str);
char *output_comment(file_info *hdr);
void snd_load_init_file(bool nog, bool noi);
void snd_load_file(char *filename);
XEN snd_eval_str(char *buf);
XEN snd_report_result(XEN result, char *buf);
XEN snd_report_listener_result(XEN form);
void snd_eval_property_str(char *buf);
void snd_eval_stdin_str(char *buf);
void g_snd_callback(int callb);
void clear_stdin(void);
#if HAVE_RUBY
  void snd_rb_raise(XEN type, XEN info);
#endif


/* -------- snd-select.c -------- */

bool selection_is_active(void);
bool selection_is_active_in_channel(chan_info *cp);
bool selection_is_visible_in_channel(chan_info *cp);
off_t selection_beg(chan_info *cp);
off_t selection_end(chan_info *cp);
off_t selection_len(void);
int selection_chans(void);
int selection_srate(void);
Float selection_maxamp(chan_info *cp);
void deactivate_selection(void);
void reactivate_selection(chan_info *cp, off_t beg, off_t end);
void ripple_selection(ed_list *new_ed, off_t beg, off_t num);
sync_info *selection_sync(void);
void start_selection_creation(chan_info *cp, off_t samp);
void update_possible_selection_in_progress(off_t samp);
int make_region_from_selection(void);
void display_selection(chan_info *cp);
bool delete_selection(const char *origin, cut_selection_regraph_t regraph);
void move_selection(chan_info *cp, int x);
void finish_selection_creation(void);
int select_all(chan_info *cp);
int save_selection(char *ofile, int type, int format, int srate, const char *comment, int chan);
bool selection_creation_in_progress(void);
void cancel_selection_watch(void);
void add_selection_or_region(int reg, chan_info *cp, const char *origin);
void insert_selection_from_menu(void);
void insert_selection_or_region(int reg, chan_info *cp, const char *origin);

void g_init_selection(void);
  

/* -------- snd-region.c -------- */

void allocate_regions(int numreg);
bool region_ok(int n);
int region_chans(int n);
int region_srate(int n);
off_t region_len(int n);
Float region_maxamp(int n);
int region_list_position_to_id(int n);
int region_id_to_list_position(int id);
file_info *fixup_region_data(chan_info *cp, int chan, int n);
region_state *region_report(void);
void free_region_state (region_state *r);
int remove_region_from_list(int pos);
int save_region(int n, char *ofile, int data_format);
void paste_region(int n, chan_info *cp, const char *origin);
void add_region(int n, chan_info *cp, const char *origin);
int define_region(sync_info *si, off_t *ends);
snd_fd *init_region_read (off_t beg, int n, int chan, read_direction_t direction);
void cleanup_region_temp_files(void);
int snd_regions(void);
void save_regions(FILE *fd);
void region_edit(int reg);
void clear_region_backpointer(snd_info *sp);
void save_region_backpointer(snd_info *sp);
void sequester_deferred_regions(chan_info *cp, int edit_top);
void g_init_regions(void);
void for_each_region_chan(void (*func)(chan_info *, void *), void *userptr);


/* -------- snd-env.c -------- */

Float un_dB(Float py);
env *copy_env(env *e);
bool envs_equal(env *e1, env *e2);
env *free_env(env *e);
char *env_to_string(env *e);
int find_env(char *name);
env *make_envelope(Float *env_buffer, int len);
Float interp_env(env *e, Float x);
env *normalize_x_axis(env *e);
env *window_env(env *e, off_t local_beg, off_t local_dur, off_t e_beg, off_t e_dur);
env *multiply_envs(env *e1, env *e2, Float maxx);
void move_point (env *e, int pos, Float x, Float y);
void delete_point(env *e, int pos);
env *default_env(Float x1, Float y);
void *new_env_editor(void);
void edp_reset(void *spf);
axis_info *edp_ap(void *spf);
bool edp_display_graph(void *spf, const char *name, axis_context *ax, 
		       int x, int y, int width, int height, env *e, bool in_dB, bool with_dots);
void edp_handle_point(void *spf, int evx, int evy, Tempus motion_time, env *e, bool in_dB, Float xmax);
bool edp_handle_press(void *spf, int evx, int evy, Tempus time, env *e, bool in_dB, Float xmax);
void edp_handle_release(void *spf, env *e);
void edp_edited(void *spf);
void init_env_axes(axis_info *ap, const char *name, int x_offset, int ey0, int width, int height, 
		   Float xmin, Float xmax, Float ymin, Float ymax, bool printing);
void display_enved_env(env *e, axis_context *ax, char *name, 
		       int x0, int y0, int width, int height, bool dots, Float base, bool printing);
void view_envs(int env_window_width, int env_window_height, bool printing);
int hit_env(int xe, int ye, int env_window_width, int env_window_height);
void do_enved_edit(env *new_env);
void redo_env_edit(void);
void undo_env_edit(void);
void revert_env_edit(void);
int enved_all_envs_top(void);
char *enved_all_names(int n);
void set_enved_env_list_top(int n);
env *enved_all_envs(int pos);
void alert_envelope_editor(char *name, env *val);
void enved_show_background_waveform(axis_info *ap, axis_info *gray_ap, bool apply_to_selection, bool show_fft, bool printing);
int enved_button_press_display(axis_info *ap, env *active_env, int evx, int evy);
void save_envelope_editor_state(FILE *fd);
char *env_name_completer(char *text);
env *enved_next_env(void);
env *string2env(char *str);
void add_or_edit_symbol(char *name, env *val);
env* name_to_env(char *str);
void delete_envelope(char *name);

XEN env_to_xen (env *e);
env *xen_to_env(XEN res);
env *get_env(XEN e, const char *origin);
void g_init_env(void);
bool check_enved_hook(env *e, int pos, Float x, Float y, enved_point_t reason);



/* -------- snd-dac.c -------- */

void cleanup_dac(void);
Float list_interp(Float x, Float *e, int pts);
void stop_playing_sound(snd_info *sp);
void stop_playing_sound_no_toggle(snd_info *sp);
void stop_playing_all_sounds(void);
void stop_playing_region(int n);
void play_region(int n, play_process_t background);
void play_channel(chan_info *cp, off_t start, off_t end, play_process_t background, XEN edpos, const char *caller, int arg_pos);
void play_sound(snd_info *sp, off_t start, off_t end, play_process_t background, XEN edpos, const char *caller, int arg_pos);
void play_channels(chan_info **cps, int chans, off_t *starts, off_t *ends, play_process_t background, 
		   XEN edpos, const char *caller, int arg_pos, bool selection);
void play_selection(play_process_t background, XEN edpos, const char *caller, int arg_pos);
void toggle_dac_pausing(void); /* snd-dac.c */
bool play_in_progress(void);
void initialize_apply(snd_info *sp, int chans, off_t beg, off_t frames);
void finalize_apply(snd_info *sp);
int run_apply(int ofd);
Float *sample_linear_env(env *e, int order);

void g_init_dac(void);
snd_info *player(int index);
void clear_players(void);

void dac_set_expand(snd_info *sp, Float newval);
void dac_set_expand_length(snd_info *sp, Float newval);
void dac_set_expand_ramp(snd_info *sp, Float newval);
void dac_set_expand_hop(snd_info *sp, Float newval);
void dac_set_contrast_amp(snd_info *sp, Float newval);
void dac_set_reverb_feedback(snd_info *sp, Float newval);
void dac_set_reverb_lowpass(snd_info *sp, Float newval);



/* -------- snd-chn.c -------- */

snd_info *make_simple_channel_display(int srate, int initial_length, fw_button_t with_arrows, 
				      graph_style_t grf_style, widget_t container, bool with_events);
axis_info *lisp_info_axis(chan_info *cp);
void *free_lisp_info(chan_info *cp);
void zx_incremented(chan_info *cp, double amount);
kbd_cursor_t cursor_decision(chan_info *cp);
void reset_x_display(chan_info *cp, double sx, double zx);
void set_x_axis_x0x1 (chan_info *cp, double x0, double x1);
void cursor_move (chan_info *cp, off_t samps);
void cursor_moveto_without_verbosity(chan_info *cp, off_t samp);
void set_wavo_trace(int uval);
void set_dot_size(int val);
chan_info *virtual_selected_channel(chan_info *cp);
void handle_cursor(chan_info *cp, kbd_cursor_t redisplay);
void chans_field(fcp_t field, Float val);
void in_set_transform_graph_type(graph_type_t val);
void in_set_fft_window(mus_fft_window_t val);
void combine_sound(snd_info *sp);
void separate_sound(snd_info *sp);
void superimpose_sound(snd_info *sp);
void set_sound_channel_style(snd_info *sp, channel_style_t val);
void set_chan_fft_in_progress(chan_info *cp, Cessator fp);
void goto_graph(chan_info *cp);
void start_amp_env(chan_info *cp);
void stop_amp_env(chan_info *cp);
bool chan_fft_in_progress(chan_info *cp);
void force_fft_clear(chan_info *cp);
void chan_info_cleanup(chan_info *cp);
void update_graph(chan_info *cp);
void add_channel_data(char *filename, chan_info *cp, channel_graph_t graphed);
void add_channel_data_1(chan_info *cp, int srate, off_t frames, channel_graph_t graphed);
void set_x_bounds(axis_info *ap);
void display_channel_data (chan_info *cp);
void display_channel_fft_data (chan_info *cp);
void show_cursor_info(chan_info *cp);
void apply_x_axis_change(axis_info *ap, chan_info *cp);
void apply_y_axis_change (axis_info *ap, chan_info *cp);
void sx_incremented(chan_info *cp, double amount);
int move_axis(chan_info *cp, axis_info *ap, int x);
void set_axes(chan_info *cp, double x0, double x1, Float y0, Float y1);
void focus_x_axis_change(axis_info *ap, chan_info *cp, int focus_style);
bool key_press_callback(chan_info *ur_cp, int x, int y, int key_state, int keysym);
void graph_button_press_callback(chan_info *cp, int x, int y, int key_state, int button, Tempus time);
void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button);
void graph_button_motion_callback(chan_info *cp, int x, int y, Tempus time, Tempus click_time);
void channel_resize(chan_info *cp);
void edit_history_select(chan_info *cp, int row);
int make_graph(chan_info *cp);
int make_background_graph(chan_info *cp);
void reset_spectro(void);
void cursor_moveto (chan_info *cp, off_t samp);
chan_info *which_channel(snd_info *sp, int y);

void g_init_chn(void);
XEN make_graph_data(chan_info *cp, int edit_pos, off_t losamp, off_t hisamp);
void draw_graph_data(chan_info *cp, off_t losamp, off_t hisamp, int data_size, Float *data, Float *data1, axis_context *ax, graph_style_t style);

void fftb(chan_info *cp, bool on);
void waveb(chan_info *cp, bool on);
void f_button_callback(chan_info *cp, bool on, bool with_control);
void w_button_callback(chan_info *cp, bool on, bool with_control);
axis_context *set_context (chan_info *cp, chan_gc_t gc);
axis_context *copy_context (chan_info *cp);
axis_context *erase_context (chan_info *cp);
axis_context *selection_context (chan_info *cp);
axis_context *mark_context (chan_info *cp);
axis_context *mix_waveform_context (chan_info *cp);
axis_context *selected_mix_waveform_context (chan_info *cp);
void calculate_fft(chan_info *cp);


/* -------- snd-axis.c -------- */

axis_info *free_axis_info(axis_info *ap);
axis_context *free_axis_context(axis_context *ax);
Locus grf_x(double val, axis_info *ap);
Locus grf_y(Float val, axis_info *ap);
void init_axis_scales(axis_info *ap);
void make_axes_1(axis_info *ap, x_axis_style_t x_style, int srate, show_axes_t axes, bool printing, bool show_x_axis);

#define ungrf_x(AP, X) (((X) - (AP)->x_base) / (AP)->x_scale)
#define ungrf_y(AP, Y) (((Y) - (AP)->y_base) / (AP)->y_scale)

axis_info *make_axis_info (chan_info *cp, double xmin, double xmax, Float ymin, Float ymax, 
			   char *xlabel, double x0, double x1, Float y0, Float y1,
			   axis_info *old_ap);

#if (!USE_NO_GUI)
  void g_init_axis(void);
#endif
#if HAVE_GL
  void reload_label_font(void);
  void reload_number_font(void);
#endif



/* -------- snd-snd.c -------- */

env_info *free_amp_env(chan_info *cp, int pos);
void free_env_state(chan_info *cp);
env_info *free_env_info(env_info *ep);
void start_env_state(chan_info *cp);
env_info *make_mix_input_amp_env(chan_info *cp);
Cessate get_amp_env(Indicium ptr);
bool amp_env_maxamp_ok(chan_info *cp, int edpos);
Float amp_env_maxamp(chan_info *cp, int edpos);
bool amp_env_usable(chan_info *cp, Float samples_per_pixel, off_t hisamp, bool start_new, int edit_pos, bool finish_env);
int amp_env_graph(chan_info *cp, axis_info *ap, Float samples_per_pixel, int srate);
char *shortname(snd_info *sp);
char *shortname_indexed(snd_info *sp);
void add_sound_data(char *filename, snd_info *sp, channel_graph_t graphed);
Float srate_changed(Float ival, char *srcbuf, speed_style_t style, int tones);
void sp_name_click(snd_info *sp);
void free_controls(snd_info *sp);
void save_controls(snd_info *sp);
void restore_controls(snd_info *sp);
void reset_controls(snd_info *sp);
void stop_applying(snd_info *sp);
void remove_apply(snd_info *sp);
Cessate apply_controls(Indicium xp);
void *make_apply_state_with_implied_beg_and_dur(void *xp);
env_info *env_on_env(env *e, chan_info *cp);
void amp_env_env(chan_info *cp, Float *brkpts, int npts, int pos, Float base, Float scaler, Float offset);
void amp_env_env_selection_by(chan_info *cp, mus_any *e, off_t beg, off_t num, int pos);
void amp_env_ptree(chan_info *cp, void *pt, int pos, XEN init_func, bool is_xen);
void amp_env_ptree_selection(chan_info *cp, void *pt, off_t beg, off_t num, int pos, XEN init_func, bool is_xen);
void amp_env_insert_zeros(chan_info *cp, off_t beg, off_t num, int pos);
snd_info *snd_new_file(char *newname, int header_type, int data_format, int srate, int chans, char *new_comment, off_t samples);

void g_init_snd(void);
XEN snd_no_such_sound_error(const char *caller, XEN n);

void set_speed_style(speed_style_t val);
void amp_env_scale_by(chan_info *cp, Float scl, int pos);
void amp_env_scale_selection_by(chan_info *cp, Float scl, off_t beg, off_t num, int pos);
env_info *amp_env_copy(chan_info *cp, bool reversed, int edpos);
env_info *amp_env_section(chan_info *cp, off_t beg, off_t num, int edpos);
void pick_one_bin(env_info *ep, int bin, off_t cursamp, chan_info *cp, int edpos);
void remember_mini_string(snd_info *sp, char *str);
void restore_mini_string(snd_info *s, bool back);
void clear_mini_strings(snd_info *sp);
void remember_filter_string(snd_info *sp, char *str);
void restore_filter_string(snd_info *s, bool back);
void clear_filter_strings(snd_info *sp);
void remember_listener_string(char *str);
void restore_listener_string(bool back);


/* -------- snd-file -------- */

void *free_axes_data(void *sa);
void *make_axes_data(snd_info *sp);
bool restore_axes_data(snd_info *sp, void *sa, Float new_duration, bool need_edit_history_update);
off_t disk_kspace (const char *filename);
time_t file_write_date(const char *filename);
bool link_p(const char *filename);
bool directory_p(const char *filename);
file_info *make_file_info(const char *fullname);
file_info *free_file_info(file_info *hdr);
file_info *copy_header(const char *fullname, file_info *ohdr);
file_info *make_temp_header(const char *fullname, int srate, int chans, off_t samples, const char *caller);
dir *free_dir (dir *dp);
bool sound_file_p(char *name);
void init_sound_file_extensions(void);
dir *find_sound_files_in_dir (const char *name);
dir *filter_sound_files(dir *dp, char *pattern);
snd_info *snd_open_file (const char *filename, bool read_only);
snd_info *snd_open_file_unselected (const char *filename, bool read_only);
void snd_close_file(snd_info *sp);
int copy_file(const char *oldname, const char *newname);
int move_file(const char *oldfile, const char *newfile);
snd_info *make_sound_readable(const char *filename, bool post_close);
snd_info *snd_update(snd_info *sp);
char *view_curfiles_name(int pos);
void view_curfiles_play(int pos, bool play);
void view_curfiles_select(int pos);
void view_prevfiles_select(int pos);
int view_prevfiles_play(int pos, bool play);
char *get_prevname(int n);
char *get_prevfullname(int n);
char *get_curfullname(int pos);
int get_max_prevfile_end(void);
void set_max_prevfile_end(int n);
int get_prevfile_end(void);
int get_max_curfile_end(void);
void set_max_curfile_end(int n);
int get_curfile_end(void);
int get_curfile_size(void);
int get_prevfile_size(void);
void save_prevlist(FILE *fd);
int find_curfile_regrow(const char *shortname);
int find_prevfile_regrow(const char *shortname);
void clear_prevlist(void);
void update_prevlist(void);
void init_curfiles(int size);
void init_prevfiles(int size);
void add_directory_to_prevlist(const char *dirname);
void make_prevfiles_list_1(void);
char **set_header_and_data_positions(file_data *fdat, int type, int format);
int check_for_filename_collisions_and_save(snd_info *sp, char *str, save_dialog_t save_type, int srate, int type, int format, char *comment);
void edit_header_callback(snd_info *sp, file_data *edit_header_data);
void reflect_file_change_in_title(void);

int header_type_from_position(int pos);
int data_format_from_position(int header, int pos);
void set_header_type_and_format_from_position(file_data *fdat, int pos);
char **set_header_positions_from_type(file_data *fdat, int header_type, int data_format);

void g_init_file(void);
void initialize_format_lists(void);



/* -------- snd-utils -------- */

int snd_round(double x);
off_t snd_round_off_t(double x);
off_t snd_abs_off_t(off_t val);
int snd_ipow2(int n);
int snd_2pow2(int n);
Float in_dB(Float min_dB, Float lin_dB, Float py);
char *copy_string(const char *str);
int snd_strlen(const char *str);
char *snd_strcat(char *errmsg, const char *str, int *err_size);
char *filename_without_home_directory(const char *name);
char *just_filename(char *name);
char *prettyf(Float num, int tens);
char *shorter_tempnam(const char *dir, const char *prefix);
char *snd_tempnam(void);
void fill_number(char *fs, char *ps);
void snd_exit(int val);
char local_decimal_point(void);
void g_init_utils(void);
#ifdef DEBUG_MEMORY
  void set_encloser(char *name);
#endif
#if DEBUGGING && HAVE_CLOCK
  void start_timing(void);
  void stop_timing(void);
#endif



/* -------- snd-listener -------- */

void command_return(widget_t w, int last_prompt);
char *listener_prompt_with_cr(void);
int check_balance(char *expr, int start, int end, bool in_listener);
int find_matching_paren(char *str, int parens, int pos, char *prompt, int *highlight_pos);
XEN provide_listener_help(char *source);

void g_init_listener(void);



/* -------- snd-mix.c -------- */

disk_space_t disk_space_p(snd_info *sp, off_t bytes, off_t other_bytes, char *filename);
mix_context *cp_to_mix_context(chan_info *cp);
mix_context *make_mix_context(chan_info *cp);
mix_context *free_mix_context(mix_context *ms);
void free_mix_list(chan_info *cp);
void free_mixes(chan_info *cp);
void mix_complete_file_at_cursor(snd_info *sp, char *str, const char *origin, bool with_tag, int track_id);
int mix(off_t beg, off_t num, int chans, chan_info **cps, char *mixinfile, file_delete_t temp, const char *origin, bool with_tag, int track_id);
void backup_mix_list(chan_info *cp, int edit_ctr);
bool active_mix_p(chan_info *cp);
off_t mix_beg(chan_info *cp);
void reset_mix_graph_parent(chan_info *cp);
void display_channel_mixes(chan_info *cp);
void lock_affected_mixes(chan_info *cp, off_t beg, off_t end);
void release_pending_mixes(chan_info *cp, int edit_ctr);
void reset_mix_list(chan_info *cp);
void ripple_mixes(chan_info *cp, off_t beg, off_t change);
void goto_mix(chan_info *cp, int count);
off_t mix_frames(int n);
int any_mix_id(void);
bool mix_ok_and_unlocked(int n);
int set_mix_amp_env_from_gui(int n, int chan, env *val);
int set_mix_amp_env_without_edit(int n, int chan, env *val);
env *mix_amp_env_from_id(int n, int chan);
void g_init_mix(void);
snd_info *make_mix_readable_from_id(int id);
int mix_selected_channel(int id);
void clear_mix_tags(chan_info *cp);
void clear_mix_y(chan_info *cp);
void move_mix_tag(int mix_tag, int x);
void finish_moving_mix_tag(int mix_tag, int x);
int hit_mix(chan_info *cp, int x, int y);
void select_mix_from_id(int mix_id);
chan_info *mix_channel_from_id(int mix_id);
void mix_play_from_id(int mix_id);
void track_play_from_id(int mix_id);
int current_mix_id(void);
void start_mix_drag(int mix_id);
int set_mix_speed_from_id(int mix_id, Float val, bool dragging);
int set_mix_amp_from_id(int mix_id, int chan, Float val, bool dragging);
void set_mix_track_from_id(int mix_id, int track);
int mix_track_from_id(int mix_id);
Float mix_speed_from_id(int mix_id);
Float mix_amp_from_id(int mix_id, int chan);
off_t mix_position_from_id(int mix_id);
int mix_input_chans_from_id(int mix_id);
int set_mix_position(int mix_id, off_t beg);
bool mix_ok(int n);
env **mix_panel_envs(int n);
env *mix_panel_env(int n, int chan);
void mix_at_x_y(int data, char *filename, int x, int y);
int next_mix_id(int id);
int previous_mix_id(int id);
void reflect_edit_in_mix_panel_envs(int n);
void g_init_track(void);
#if 0
#define track_p(Arg) track_p_1(Arg, __FUNCTION__)
bool track_p_1(int trk, char *caller);
#else
bool track_p(int trk);
#endif
void free_track_info_list(chan_info *cp);
track_info *free_track_info(chan_info *cp, int loc);
void record_initial_track_info(chan_info *cp);


/* -------- snd-find.c -------- */

char *global_search(read_direction_t direction);
void cursor_search(chan_info *cp, int count);

void g_init_find(void);



/* -------- snd-trans.c -------- */

int snd_translate(const char *oldname, const char *newname, int type);


/* -------- snd-rec.c -------- */

bool record_in_progress(void);
void init_recorder(void);
void save_recorder_state(FILE *fd);
void close_recorder_audio(void);
void g_init_recorder(void);
void fire_up_recorder(void);


/* -------- snd.c -------- */

#if SND_AS_WIDGET
  snd_state *snd_main(int argc, char **argv);
#endif
void g_init_base(void);


/* -------- snd-kbd.c -------- */

void save_macro_state(FILE *fd);

#ifdef __GNUC__
  void report_in_minibuffer(snd_info *sp, char *format, ...)  __attribute__ ((format (printf, 2, 3)));
  void report_in_minibuffer_and_save(snd_info *sp, char *format, ...)  __attribute__ ((format (printf, 2, 3)));
#else
  void report_in_minibuffer(snd_info *sp, char *format, ...);
  void report_in_minibuffer_and_save(snd_info *sp, char *format, ...);
#endif
void clear_minibuffer(snd_info *sp);
void clear_minibuffer_prompt(snd_info *sp);
void snd_minibuffer_activate(snd_info *sp, int keysym, bool with_meta);
void keyboard_command (chan_info *cp, int keysym, int state);
void control_g(snd_info *sp);
void g_init_kbd(void);


/* -------- snd-sig.c -------- */

void scale_by(chan_info *cp, Float *scalers, int len, bool selection);
void scale_to(snd_info *sp, chan_info *cp, Float *scalers, int len, bool selection);
Float get_maxamp(snd_info *sp, chan_info *cp, int edpos);
src_state *make_src(Float srate, snd_fd *sf, Float initial_srate);
Float src_input_as_needed(void *arg, int dir);
src_state *free_src(src_state *sr);
void src_env_or_num(chan_info *cp, env *e, Float ratio, bool just_num, 
		    enved_progress_t from_enved, const char *origin, bool over_selection, mus_any *gen, XEN edpos, int arg_pos, Float e_base);
void apply_filter(chan_info *ncp, int order, env *e, enved_progress_t from_enved, const char *origin, 
		  bool over_selection, Float *ur_a, mus_any *gen, XEN edpos, int arg_pos);
void apply_env(chan_info *cp, env *e, off_t beg, off_t dur, bool regexpr, 
	       enved_progress_t from_enved, const char *origin, mus_any *gen, XEN edpos, int arg_pos, Float e_base);
void cos_smooth(chan_info *cp, off_t beg, off_t num, bool regexpr, const char *origin);
void display_frequency_response(env *e, axis_info *ap, axis_context *gax, int order, bool dBing);
void cursor_delete(chan_info *cp, off_t count, const char *origin);
void cursor_zeros(chan_info *cp, off_t count, bool regexpr);
void cursor_insert(chan_info *cp, off_t beg, off_t count, const char *origin);

void g_init_sig(void);
int to_c_edit_position(chan_info *cp, XEN edpos, const char *caller, int arg_pos);
off_t to_c_edit_samples(chan_info *cp, XEN edpos, const char *caller, int arg_pos);
off_t beg_to_sample(XEN beg, const char *caller);
off_t dur_to_samples(XEN dur, off_t beg, chan_info *cp, int edpos, int argn, const char *caller);


/* -------- snd-run.c -------- */

void *form_to_ptree_1_b(XEN code);
void *form_to_ptree_1_b_without_env(XEN code);
void *form_to_ptree_1_f(XEN code);
Float evaluate_ptree_0f2f(void *upt);
void *form_to_ptree_0_f(XEN code);
Float evaluate_ptree_1f2f(void *upt, Float arg);
int evaluate_ptree_1f2b(void *upt, Float arg);
void *free_ptree(void *upt);
void g_init_run(void);
XEN ptree_code(void *p);
Float evaluate_ptree_1f1v1b2f(void *upt, Float arg, vct *v, bool dir);
void *form_to_ptree_3_f(XEN code);
Float evaluate_ptreec(void *upt, Float arg, vct *v, bool dir);


/* -------- snd-draw.c -------- */

#if (!USE_NO_GUI)
  axis_info *get_ap(chan_info *cp, axis_info_t ap_id, const char *caller);
  void g_init_draw(void);
  void set_dialog_widget(snd_dialog_t which, widget_t wid);
  void run_new_widget_hook(widget_t w);
#endif
#if HAVE_GL
  void sgl_save_currents(void);
  void sgl_set_currents(void);
#endif


/* -------- snd-ladspa.c -------- */
#if HAVE_LADSPA
void g_ladspa_to_snd(void);
#endif

#endif

