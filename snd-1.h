#ifndef SND_1_H_LOADED
#define SND_1_H_LOADED

#define ASSERT_SOUND(Origin, Snd, Offset) \
  if (!((XEN_INTEGER_P(Snd)) || (XEN_FALSE_P(Snd)) || (XEN_NOT_BOUND_P(Snd)) || (XEN_LIST_P(Snd)))) \
    XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset, Snd, "an integer (sound index), boolean, or a list");

#define ASSERT_CHANNEL(Origin, Snd, Chn, Offset) \
  if (!((XEN_INTEGER_P(Snd)) || (XEN_FALSE_P(Snd)) || (XEN_NOT_BOUND_P(Snd)) || (XEN_LIST_P(Snd)))) \
    XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset, Snd, "an integer (sound index), boolean, or a list"); \
  else \
    if (!((XEN_INTEGER_P(Chn)) || (XEN_FALSE_P(Chn)) || (XEN_NOT_BOUND_P(Chn)))) \
      XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset + 1, Chn, "an integer (0-based channel number) or boolean");

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

typedef struct {
  int samps_per_bin, amp_env_size;
  MUS_SAMPLE_TYPE fmax, fmin;
  MUS_SAMPLE_TYPE *data_max, *data_min;
  int completed, bin, top_bin;
} env_info;

typedef struct {
  COLOR_TYPE color;
} snd_color;

typedef struct {
  char *name;             /* full name */
  int samples;            /* total samples = chans * samples/chan */
  int data_location;      /* bytes */
  int srate;
  int chans;
  int format;             /* data format (snd_16_linear etc) */
  int type;               /* header type (AIFF etc) */
  char *comment;          /* output case, not input */
  int *loops;
} file_info;

typedef struct {
  int type;
  MUS_SAMPLE_TYPE *buffered_data;    
  int *io;      
  char *filename;
  file_info *hdr;
  int temporary;
  int edit_ctr;
  int open;
  int inuse;
  int copy;
  int chan;
  int len;
  int just_zeros;
#if DEBUGGING
  const char *caller;
  int active;
#endif
} snd_data;

typedef struct {
  int size;
  int *fragments;
  int beg, len;
  char *origin;
  int sfnum;
  int selection_beg, selection_end;    /* added 11-Sep-00: selection needs to follow edit list */
  Float maxamp, selection_maxamp;      /* added 3-Oct-00 */
#if WITH_PARSE_TREES
  /* work-in-progress: embedded edit list fragment (composite) functions */
  MUS_SAMPLE_TYPE (*func)(void *cp, int pos, void *sf, void *env);
  void *environ;
#endif
} ed_list;

typedef struct snd_fd {
  ed_list *current_state;
  int *cb;
  int cbi;
  int eof;
  MUS_SAMPLE_TYPE *first;
  MUS_SAMPLE_TYPE *last;
  MUS_SAMPLE_TYPE *view_buffered_data;
  snd_data **sounds;
#if DEBUGGING
  const char *caller;
  char *filename;
  int edit_pos;
#endif
  snd_data *current_sound;
  int beg, end, initial_samp;
  int direction;
  struct chan__info *cp;
  struct snd__info *local_sp;          /* for local reads via make-sample-reader from Scheme */
  Float scaler;
} snd_fd;

typedef struct {Float freq; Float amp;} fft_peak;

typedef struct {
  Float y0, y1;                         /* scroller-dependent axis bounds */
  double x0, x1;
  Float xmin, xmax, ymin, ymax;         /* data-dependent absolute limits */
  Float y_scale, y_base, y_ambit;
  double x_scale, x_base, x_ambit;
  char *xlabel;
  Locus y_axis_x0, x_axis_x0,
        y_axis_y0, x_axis_y0,
        x_axis_x1,
        y_axis_y1,
        x_label_x, x_label_y;
  int graph_active;
  int losamp, hisamp;                   /* displayed x-axis bounds in terms of sound sample numbers */
  Locus graph_x0;                       /* x axis offset relative to window (for double graphs) */
  void *x_ticks, *y_ticks;              /* actual type is tick_descriptor local to snd-axis.c */
  axis_context *ax;
  Latus width, height;
  struct snd__state *ss;               /* back pointers for debugging and whatnot */
  struct chan__info *cp;
  Float sy, zy;                         /* as set by user, 0.0 - 1.0 */
  double sx, zx;
  Locus y_offset;
  Latus window_width;
  int no_data;
} axis_info;

typedef struct {
  int *len;
  Float **data;
  int graphs;
  axis_info *axis;
  int env_data;
} lisp_grf;

typedef struct {
  int samp;
  char *name;
  unsigned int id, sync;
} mark;

typedef struct {
  Float *data;
  int pts, data_size;
  int exponential;
} env;

typedef struct {
  int size;
  int current_size;
  int window;              /* fft window in this case, not a display window */
  int ok;
  Float beta;
  Float scale;
  axis_info *axis;
  Float *data;
  struct chan__info *chan;
} fft_info;

typedef struct {
  int total_slices;        /* size of the data array (max for allocation checks) */
  int total_bins;          /* size other axis data array */
  int active_slices;       /* how many slices contain current data */
  int target_bins;         /* this many bins Y-side */
  int target_slices;       /* how many slices in full display (current) */
  Float **data;            /* data[total_slices][bins] -> each is a spectral magnitude */
  int *begs;               /* associated begin times (for data reuse) */
  struct chan__info *cp;
  Float scale;
} sono_info;

typedef struct chan__info {
  int chan;                /* which chan are we */
  int *samples;            /* current length */
  int graph_transform_p;   /* f button state */
  int graph_time_p;        /* w button state */
  int graph_lisp_p;        /* is lisp graph active */
  lisp_grf *lisp_info;
  int cursor_on;           /* channel's cursor */
  int cursor_visible;      /* for XOR decisions */
  int cursor;              /* sample */
  int cursor_style, cursor_size;
  int cx, cy;               /* graph-relative cursor loc (for XOR) */
  int edit_ctr;            /* channel's edit history */
  int edit_size;           /* current edit list size */
  ed_list **edits;         /* the edit list */
  int sound_size;          /* edit_list associated temp sound buffers */
  int sound_ctr;           /* current location in sounds list */
  snd_data **sounds;       /* the associated temp buffer/file/struct list */
  fft_info *fft;           /* possibly null fft data */
  struct snd__info *sound; /* containing sound */
  struct snd__state *state;
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
  int printing;
  Float gsy, gzy;
  void *mix_dragging;
  int height, mixes;
  int original_cursor;     /* for cursor reset after cursor-moving play */
  int hookable;
  int selection_transform_size;
  int *stats;
  int squelch_update, waiting_to_make_graph;
  /* moved from global to channel-local 4-Aug-00 */
  Float spectro_x_scale, spectro_y_scale, spectro_z_scale, spectro_z_angle, spectro_x_angle, spectro_y_angle, spectro_cutoff, spectro_start;
  Float lin_dB, min_dB, fft_window_beta;
  int show_y_zero, show_marks, wavo_hop, wavo_trace, zero_pad, x_axis_style, wavelet_type, verbose_cursor, max_transform_peaks;
  int show_transform_peaks, show_axes, graph_style, fft_log_frequency, fft_log_magnitude, transform_size, transform_graph_type, fft_window, time_graph_type;
  Latus dot_size;
  int transform_normalization, transform_type, show_mix_waveforms, spectro_hop, graphs_horizontal;
  void *mix_md;
  XEN edit_hook;
  XEN undo_hook;
  XEN cursor_proc;
  int selection_visible, sync, active;
  Locus old_x0, old_x1;
} chan_info;

typedef struct snd__info {
  int inuse;
  int index;
  int playing;
  mark *playing_mark;
  int sync, previous_sync;
  int expand_control_p;
  int contrast_control_p;
  int reverb_control_p;
  int filter_control_p, filter_control_in_dB;
  Float amp_control;
  Float speed_control;
  int speed_control_direction, speed_control_style, speed_control_tones;
  Float last_speed_control, last_amp_control, last_expand_control, last_contrast_control;
  Float last_reverb_control_length, last_reverb_control_scale;
  Float saved_speed_control, saved_amp_control, saved_expand_control, saved_contrast_control;
  Float saved_reverb_control_length, saved_reverb_control_scale;
  Float expand_control, expand_control_length, expand_control_ramp, expand_control_hop;
  Float contrast_control, contrast_control_amp;
  Float reverb_control_length, reverb_control_scale, reverb_control_feedback, reverb_control_lowpass;
  Float reverb_control_decay, filter_control_env_xmax;
  int filter_control_order, filter_control_changed;
  env *filter_control_env;
  int selected_channel;
  char *filename;
  char *short_filename;
  int nchans;
  XEN search_proc;
  XEN prompt_callback;
  char *search_expr;
  int searching, marking, filing, finding_mark, amping, reging, printing, loading, macroing, prompting;
  int minibuffer_on, minibuffer_temp;
  int sx_scroll_max;
  int read_only;
  chan_info **chans;
  struct snd__state *state;
  snd_context *sgx;
  file_info *hdr;             /* header of file that would be affected if we were to save current edits */
  int env_anew, bomb_ctr;
  time_t write_date;          /* check for change behind back while editing */
  int need_update;            /* current in-core data does not match actual file (someone wrote it behind our back) */
  int channel_style;          /* 0:separate panes per chan, 1:all chans in one pane */
  int allocated_chans;        /* snd_info widget tree is never pruned -- can only grow */
  int cursor_follows_play;
  void *edited_region;
  int delete_me;
  chan_info *lacp;
  void *saved_controls;
  int apply_ok, applying;
  /* moved from global to channel-local 4-Aug-00 */
  void *minibuffer_history, *filter_history;
  int active;
} snd_info;

#define SND_SRATE(sp) (((sp)->hdr)->srate)

typedef struct snd__state {
  int selected_sound, selected_mix;         /* NO_SELECTION = none selected = which sound is currently receiving user's attention */
  int active_sounds;
  Latus ctrls_height, open_ctrls_height, channel_min_height;
  snd_info **sounds;
  char *search_expr, *startup_title;
  XEN search_proc;
  XEN file_sort_proc;
  int catch_exists;
  char *catch_message;
  int search_in_progress;
  int using_schemes;
  state_context *sgx;
  int position_slider_width, zoom_slider_width, toggle_size, enved_point_size, channel_sash_indent, sash_size, channel_sash_size, sash_indent;
  char *init_file;
  int max_sounds;
  snd_info *mx_sp;
  char *pending_change;
  int print_choice, apply_choice, just_time, memory_available;
  int stopped_explicitly, checking_explicitly, reloading_updated_file;
  int listening;
  Latus init_window_width, init_window_height;
  Locus init_window_x, init_window_y;
  int transform_hook_active, graph_hook_active;

  /* user-visible global variables
   *   all of these are accessed through macros in snd-0.h 
   *   each has a default defined in snd-0.h,
   *              name defined in snd-strings.h
   *              initialized in snd.c
   *              saved in snd-main.c
   *              included in snd-help.c's variable list
   *              documented in extsnd.html
   *              several styles of tests in snd-test.scm
   *              brought out to user via Guile
   */
  int Show_Transform_Peaks, Show_Y_Zero, Show_Marks, Fft_Log_Frequency, Fft_Log_Magnitude, Channel_Style, Sound_Style, Show_Axes;
  char *Eps_File, *Temp_Dir, *Audio_State_File, *Save_Dir;
  char *Listener_Font, *Help_Text_Font, *Axis_Label_Font, *Axis_Numbers_Font, *Bold_Button_Font, *Button_Font, *Tiny_Font;
  int Verbose_Cursor, Show_Usage_Stats, Trap_Segfault;
  int Enved_Filter_Order, Filter_Env_In_Hz;  /* for spectral envelopes from the envelope editor */
  Float Vu_Size, Vu_Font_Size, Eps_Left_Margin, Eps_Bottom_Margin;
  char *Vu_Font;
  Float Spectro_X_Scale, Spectro_Y_Scale, Spectro_Z_Scale, Spectro_Z_Angle, Spectro_X_Angle, Spectro_Y_Angle, Spectro_Cutoff, Spectro_Start;
  int Default_Output_Type, Default_Output_Format, Default_Output_Chans, Default_Output_Srate;
  int Spectro_Hop, Color_Map, Wavelet_Type, Transform_Type;
  Latus Dot_Size;
  int Transform_Size, Fft_Window, Transform_Graph_Type, Time_Graph_Type, Zero_Pad, Ask_Before_Overwrite, Wavo_Hop, Wavo_Trace;
  Float Fft_Window_Beta, Reverb_Control_Decay;
  Float Color_Scale, Color_Cutoff;
  int Color_Inverted, Speed_Control_Style, Movies, Transform_Normalization, Show_Mix_Waveforms, Mix_Waveform_Height;
  int Speed_Control_Tones, Sinc_Width, X_Axis_Style, Zoom_Focus_Style, Graph_Style, Region_Graph_Style;
  int Auto_Resize, Auto_Update, Max_Regions, Max_Transform_Peaks;
  int Audio_Output_Device, Audio_Input_Device, Show_Backtrace;
  int Print_Length, Dac_Size, Dac_Combines_Channels, Previous_Files_Sort, Show_Selection_Transform, With_Mix_Tags, Selection_Creates_Region;
  char *Save_State_File, *Listener_Prompt;
  Float Enved_Base, Enved_Power, Auto_Update_Interval;
  int Enved_Clip_p, Enved_Exp_p, Enved_Target, Enved_Wave_p, Enved_in_dB, Graphs_Horizontal;
  int Graph_Cursor, Use_Sinc_Interp, Data_Clipped, Show_Indices, Mix_Tag_Width, Mix_Tag_Height, Minibuffer_History_Length;
  Float min_dB, lin_dB, Hankel_Jn;
#if HAVE_HTML
  int HTML_Width, HTML_Height;
  char *HTML_Dir, *HTML_Font_Size_List, *HTML_Fixed_Font_Size_List;
#endif
} snd_state;

typedef struct {
  char **files;
  char *name;
  int len;
  int size;
} dir;

typedef struct {
  int chans;
  int *begs;
  chan_info **cps;
} sync_info;

typedef struct {
  int len;
  int *save;
  char **name;
} region_state;

typedef struct {
  mus_any *gen;
  snd_fd *sf;
  int sample;
} src_state;

typedef struct {
  int slice; 
  int samples;  
  env_info *ep; 
  snd_fd *sf;
  int m, amp_buffer_size;
} env_state;



/* -------- snd-io.c -------- */

int snd_open_read(snd_state *ss, const char *arg);
int snd_reopen_write(snd_state *ss, const char *arg);
int snd_write_header(snd_state *ss, const char *name, int type, int srate, int chans, int loc, int size, int format, const char *comment, int len, int *loops);
int snd_overwrite_ok(snd_state *ss, const char *ofile);
int file_state_channel_offset(int chan);
int *make_file_state(int fd, file_info *hdr, int chan, int suggested_bufsize);
int *free_file_state(int *datai);
void set_file_state_fd(int *datai, int fd);
void close_file_state_fd(int *datai);
void file_buffers_forward(int ind0, int ind1, int indx, snd_fd *sf, snd_data *cur_snd);
void file_buffers_back(int ind0, int ind1, int indx, snd_fd *sf, snd_data *cur_snd);
MUS_SAMPLE_TYPE snd_file_read_sample(snd_data *ur_sd, int index, chan_info *cp);
int file_state_buffer_size(int *datai);
int *make_zero_file_state(int size);
char *snd_remove_with_error(const char *name);
int snd_remove(const char *name);


/* -------- snd-help.c -------- */

void about_snd_help(snd_state *ss);
void fft_help(snd_state *ss);
void find_help(snd_state *ss);
void undo_help(snd_state *ss);
void sync_help(snd_state *ss);
void speed_help(snd_state *ss);
void expand_help(snd_state *ss);
void reverb_help(snd_state *ss);
void contrast_help(snd_state *ss);
void env_help(snd_state *ss);
void marks_help(snd_state *ss);
void mix_help(snd_state *ss);
void sound_files_help(snd_state *ss);
void init_file_help(snd_state *ss);
void recording_help(snd_state *ss);
void clm_help(snd_state *ss);
char *version_info(void);
void news_help(snd_state *ss);

#if HAVE_CLICK_FOR_HELP
void click_for_file_menu_help(snd_state *ss);
void click_for_edit_menu_help(snd_state *ss);
void click_for_view_menu_help(snd_state *ss);
void click_for_options_menu_help(snd_state *ss);
void click_for_help_menu_help(snd_state *ss);
void click_for_graph_help(snd_state *ss);
void click_for_history_help(snd_state *ss);
void click_for_minibuffer_help(snd_state *ss);
void click_for_play_help(snd_state *ss);
void click_for_speed_help(snd_state *ss);
void click_for_expand_help(snd_state *ss);
void click_for_name_separator_help(snd_state *ss);
void click_for_amp_help(snd_state *ss);
void click_for_srate_arrow_help(snd_state *ss);
void click_for_contrast_help(snd_state *ss);
void click_for_sound_help(snd_state *ss);
void click_for_reverb_scale_help(snd_state *ss);
void click_for_reverb_length_help(snd_state *ss);
void click_for_filter_help(snd_state *ss);
void click_for_filter_order_help(snd_state *ss);
void click_for_filter_envelope_help(snd_state *ss);
void click_for_save_as_help(snd_state *ss);
#endif

void help_dialog_help(snd_state *ss);
void transform_dialog_help(snd_state *ss);
void color_dialog_help(snd_state *ss);
void orientation_dialog_help(snd_state *ss);
void listener_dialog_help(snd_state *ss);
void record_dialog_help(snd_state *ss);
void envelope_editor_dialog_help(snd_state *ss);
void region_dialog_help(snd_state *ss);
void raw_data_dialog_help(snd_state *ss);
void new_file_dialog_help(snd_state *ss);
void file_mix_dialog_help(snd_state *ss);
void edit_header_dialog_help(snd_state *ss);
void print_dialog_help(snd_state *ss);
void view_files_dialog_help(snd_state *ss);
void stats_dialog_help(snd_state *ss);
void ssnd_help(snd_state *ss, char *subject, ...);

void g_init_help(void);
XEN g_help(XEN text, int widget_wid);


/* -------- snd-menu.c -------- */

void reflect_file_open_in_menu (void);
void reflect_file_change_in_menu (void);
void reflect_file_lack_in_menu (void);
void reflect_mix_active_in_menu(void);
void reflect_equalize_panes_in_menu(int on);
void reflect_file_revert_in_menu (snd_state *ss);
void reflect_file_save_in_menu (snd_state *ss);
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

void set_show_usage_stats(snd_state *ss, int val);
void close_file_from_menu(snd_state *ss);
void save_file_from_menu(snd_state *ss);
void update_file_from_menu(snd_state *ss);
void revert_file_from_menu(snd_state *ss);
void exit_from_menu(snd_state *ss);
void save_options_from_menu(snd_state *ss);
void save_state_from_menu(snd_state *ss);
void new_file_from_menu(snd_state *ss);

void set_graph_style(snd_state *ss, int val);
void set_show_marks(snd_state *ss, int val);
void set_show_y_zero(snd_state *ss, int val);
void set_verbose_cursor(snd_state *ss, int val);
void set_view_ctrls_label(char *lab);
void set_view_listener_label(char *lab);
void activate_focus_menu(snd_state *ss, int new_focus);
void activate_speed_in_menu(snd_state *ss, int newval);
void set_x_axis_style(snd_state *ss, int val);
void set_channel_style(snd_state *ss, int val);
int map_chans_x_axis_style(chan_info *cp, void *ptr);

void g_init_menu(void);


/* -------- snd-main.c -------- */

int snd_exit_cleanly(snd_state *ss, int force_exit);
int snd_not_current(snd_info *sp, void *dat);
int save_options (snd_state *ss);
FILE *open_snd_init_file (snd_state *ss);
int save_state (snd_state *ss, char *save_state_name);
int handle_next_startup_arg(snd_state *ss, int auto_open_ctr, char **auto_open_file_names, int with_title, int args);

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
void set_save_completions(int save);
void add_possible_completion(char *text);
void display_completions(snd_state *ss);
char *complete_text(char *text, int func);
void clear_possible_completions(void);
char *filename_completer(char *text);
char *srate_completer(char *text);
char *info_completer(char *text);
char *complete_listener_text(char *old_text, int end, int *try_completion, char **to_file_text);


/* -------- snd-print.c -------- */

void ps_set_grf_points(double x, int j, Float ymin, Float ymax);
void ps_set_grf_point(double x, int j, Float y);
void ps_allocate_grf_points(void);
void ps_draw_grf_points(axis_info *ap, int j, Float y0, int graph_style, int dot_size);
void ps_draw_both_grf_points(axis_info *ap, int j, int graph_style, int dot_size);
void ps_draw_sono_rectangle(axis_info *ap, int color, Float x, Float y, Float width, Float height);
void ps_reset_color(void);
void ps_recolor(chan_info *cp);
void ps_draw_line (axis_info *ap, int x0, int y0, int x1, int y1);
void ps_draw_spectro_line(axis_info *ap, int color, Float x0, Float y0, Float x1, Float y1);
void ps_fill_rectangle (axis_info *ap, int x0, int y0, int width, int height);
void ps_draw_string (axis_info *ap, int x0, int y0, char *str);
void ps_set_number_font(void);
void ps_set_label_font(void);
void ps_set_bold_peak_numbers_font(void);
void ps_set_peak_numbers_font(void);
void ps_set_tiny_numbers_font(void);
void snd_print(snd_state *ss, char *output);
void region_print(char *output, char* title, chan_info *cp);
void print_enved(char *output, chan_info *cp, int y0);

void g_init_print(void);



/* -------- snd-marks.c -------- */

int mark_id(mark *m);
int mark_sync_max(void);
int set_mark_sync(mark *m, int val);
void marks_off(chan_info *cp);
void draw_mark(chan_info *cp, axis_info *ap, mark *mp);
mark *hit_mark(chan_info *cp, int x, int y, int key_state);
mark *hit_triangle(chan_info *cp, int x, int y);
void move_mark(chan_info *cp, mark *mp, int x);
void play_syncd_mark(chan_info *cp, mark *mp);
int move_play_mark(chan_info *cp, int *mc, Locus cx);
void finish_moving_play_mark(chan_info *cp);
void finish_moving_mark(chan_info *cp, mark *m);
mark *add_mark(int samp, char *name, chan_info *cp);
void delete_mark_samp(int samp, chan_info *cp);
void free_mark_list(chan_info *cp, int ignore);
void collapse_marks (snd_info *sp);
int goto_mark(chan_info *cp, int count);
int goto_named_mark(chan_info *cp, char *name);
mark *active_mark(chan_info *cp);
int mark_beg(chan_info *cp);
void display_channel_marks (chan_info *cp);
void release_pending_marks(chan_info *cp, int edit_ctr);
void ripple_marks(chan_info *cp, int beg, int change);
void mark_define_region(chan_info *cp, int count);
void save_mark_list(FILE *fd, chan_info *cp);
void reverse_marks(chan_info *cp, int over_selection);
void src_marks(chan_info *cp, Float ratio, int old_samps, int new_samps, int beg, int over_selection);
void reset_marks(chan_info *cp, int num, int *samps, int end, int extension, int over_selection);
void ripple_trailing_marks(chan_info *cp, int beg, int old_len, int new_len);

void g_init_marks(void);



/* -------- snd-data.c -------- */

lisp_grf *free_lisp_info(chan_info *cp);
chan_info *make_chan_info(chan_info *cip, int chan, snd_info *sound, snd_state *state);
snd_info *make_snd_info(snd_info *sip, snd_state *state, char *filename, file_info *hdr, int snd_slot, int read_only);
void free_snd_info(snd_info *sp);
snd_info *completely_free_snd_info(snd_info *sp);
int map_over_sounds (snd_state *ss, int (*func)(snd_info *, void *), void *userptr);
int map_over_chans (snd_state *ss, int (*func)(chan_info *, void *), void *userptr);
int map_over_sound_chans (snd_info *sp, int (*func)(chan_info *, void *), void *userptr);
int map_over_separate_chans(snd_state *ss, int (*func)(chan_info *, void *), void *userptr);
int snd_ok (snd_info *sp);
int active_channels (snd_state *ss, int count_virtual_channels);
int find_free_sound_slot (snd_state *state, int desired_chans);
snd_info *selected_sound(snd_state *ss);
chan_info *selected_channel(snd_state *ss);
snd_info *any_selected_sound (snd_state *ss);
chan_info *any_selected_channel(snd_info *sp);
void select_channel(snd_info *sp, int chan);
chan_info *current_channel(snd_state *ss);
sync_info *free_sync_info (sync_info *si);
sync_info *snd_sync(snd_state *ss, int sync);
sync_info *make_simple_sync (chan_info *cp, int beg);
sync_info *sync_to_chan(chan_info *cp);
snd_info *find_sound(snd_state *ss, char *name);
void display_info(snd_info *sp);

void g_init_data(void);



/* -------- snd-edits.c -------- */

void allocate_ed_list(chan_info *cp);
void set_initial_ed_list(chan_info *cp, int len);
int edit_changes_begin_at(chan_info *cp);
int edit_changes_end_at(chan_info *cp);
void edit_history_to_file(FILE *fd, chan_info *cp);
char *edit_to_string(chan_info *cp, int edit);
void free_edit_list(chan_info *cp);
void backup_edit_list(chan_info *cp);
void remember_temp(char *filename, int chans);
void forget_temps(void);
snd_data *make_snd_data_file(char *name, int *io, MUS_SAMPLE_TYPE *data, file_info *hdr, int temp, int ctr, int temp_chan);
snd_data *make_snd_data_buffer(MUS_SAMPLE_TYPE *data, int len, int ctr);
void free_sound_list (chan_info *cp);
void gather_usage_stats(chan_info *cp);
void update_all_usage_stats(snd_state *ss);
int current_ed_samples(chan_info *cp);
void extend_with_zeros(chan_info *cp, int beg, int num, const char *origin);
void insert_samples(int beg, int num, MUS_SAMPLE_TYPE *vals, chan_info *cp, const char *origin);
void file_insert_samples(int beg, int num, char *tempfile, chan_info *cp, int chan, int auto_delete, const char *origin);
void delete_samples(int beg, int num, chan_info *cp, const char *origin);
void change_samples(int beg, int num, MUS_SAMPLE_TYPE *vals, chan_info *cp, int lock, const char *origin);
void file_change_samples(int beg, int num, char *tempfile, chan_info *cp, int chan, int auto_delete, int lock, const char *origin);
void file_override_samples(int num, char *tempfile, chan_info *cp, int chan, int auto_delete, int lock, const char *origin);
Float sample (int samp, chan_info *cp);
snd_fd *free_snd_fd(snd_fd *sf);
snd_fd *free_snd_fd_almost(snd_fd *sf);
MUS_SAMPLE_TYPE previous_sound (snd_fd *sf);
MUS_SAMPLE_TYPE next_sound (snd_fd *sf);

snd_fd *init_sample_read(int samp, chan_info *cp, int direction);
#if DEBUGGING
  snd_fd *init_sample_read_any_1(int samp, chan_info *cp, int direction, int edit_position, const char *caller);
  #define init_sample_read_any(Samp, Cp, Direction, Edit_position) init_sample_read_any_1(Samp, Cp, Direction, Edit_position, __FUNCTION__)
#else
  snd_fd *init_sample_read_any(int samp, chan_info *cp, int direction, int edit_position);
#endif

#define next_sample_to_float(SF) \
  ((SF->view_buffered_data > SF->last) ? \
     (MUS_SAMPLE_TO_FLOAT(next_sound(SF))) : \
     ((*SF->view_buffered_data++) * SF->scaler))

#define previous_sample_to_float(SF) \
  ((SF->view_buffered_data < SF->first) ? \
    (MUS_SAMPLE_TO_FLOAT(previous_sound(SF))) : \
    ((*SF->view_buffered_data--) * SF->scaler))

MUS_SAMPLE_TYPE next_sample(snd_fd *sf);
MUS_SAMPLE_TYPE previous_sample(snd_fd *sf);
int read_sample_eof (snd_fd *sf);
void undo_edit_with_sync(chan_info *cp, int count);
void redo_edit_with_sync(chan_info *cp, int count);
void undo_edit(chan_info *cp, int count);
void redo_edit(chan_info *cp, int count);
int save_channel_edits(chan_info *cp, char *ofile, XEN edpos, const char *caller, int arg_pos);
void save_edits(snd_info *sp, void *ptr);
int save_edits_without_display(snd_info *sp, char *new_name, int type, int format, int srate, char *comment, XEN edpos, const char *caller, int arg_pos);
void revert_edits(chan_info *cp, void *ptr);
int open_temp_file(char *ofile, int chans, file_info *hdr, snd_state *ss);
int close_temp_file(int ofd, file_info *hdr, long bytes, snd_info *sp);
int snd_make_file(char *ofile, int chans, file_info *hdr, snd_fd **sfs, int length, snd_state *ss);
int current_location(snd_fd *sf);

void g_init_edits(void);
MUS_SAMPLE_TYPE *g_floats_to_samples(XEN obj, int *size, const char *caller, int position);

snd_data *copy_snd_data(snd_data *sd, chan_info *cp, int bufsize);
snd_data *free_snd_data(snd_data *sf);

void parse_tree_scale_by(chan_info *cp, Float scl);
void parse_tree_selection_scale_by(chan_info *cp, Float scl, int beg, int num);
int no_ed_scalers(chan_info *cp);
void set_ed_maxamp(chan_info *cp, int edpos, Float val);
Float ed_maxamp(chan_info *cp, int edpos);
void set_ed_selection_maxamp(chan_info *cp, Float val);
Float ed_selection_maxamp(chan_info *cp);


/* -------- snd-fft.c -------- */

int make_fft_window_1(Float *window, int size, int type, Float beta);
int find_and_sort_transform_peaks(Float *buf, fft_peak *found, int num_peaks, int fftsize, int srate, Float samps_per_pixel, Float fft_scale);
int find_and_sort_peaks(Float *buf, fft_peak *found, int num_peaks, int size);
fft_info *free_fft_info(fft_info *fp);
void free_sonogram_fft_state(void *ptr);
int fft_window_beta_in_use(int win);
void *make_fft_state(chan_info *cp, int simple);
void free_sono_info (chan_info *cp);
int sono_update(chan_info *cp, void *ptr);
void set_spectro_cutoff_and_redisplay(snd_state *ss, Float val);
void c_convolve(char *fname, Float amp, int filec, int filehdr, int filterc, int filterhdr, int filtersize,
		 int fftsize, int filter_chans, int filter_chan, int data_size, snd_info *gsp, int from_enved, int ip, int total_chans);
void *make_sonogram_state(chan_info *cp);
BACKGROUND_TYPE safe_fft_in_slices(void *fftData);
BACKGROUND_TYPE sonogram_in_slices(void *sono);
char *added_transform_name(int type);

void g_init_fft(void);



/* -------- snd-xen.c -------- */

XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller);
XEN snd_create_hook(const char *name, int args, const char *help);
int ignore_mus_error(int type, char *msg);
XEN snd_no_such_file_error(const char *caller, XEN filename);
XEN snd_no_such_channel_error(const char *caller, XEN snd, XEN chn);
XEN snd_bad_arity_error(const char *caller, XEN errstr, XEN proc);
XEN snd_no_active_selection_error(const char *caller);
void g_initialize_gh(snd_state *ss);
XEN eval_str_wrapper(void *data);
XEN eval_form_wrapper(void *data);
char *g_print_1(XEN obj, const char *caller);
chan_info *get_cp(XEN snd_n, XEN chn_n, const char *caller);
snd_info *get_sp(XEN snd_n);
XEN g_c_make_sample_reader(snd_fd *fd);
XEN g_call0(XEN proc, const char *caller);
XEN g_call1(XEN proc, XEN arg, const char *caller);
XEN g_call2(XEN proc, XEN arg1, XEN arg2, const char *caller);
XEN g_call3(XEN proc, XEN arg1, XEN arg2, XEN arg3, const char *caller);
XEN g_call_any(XEN proc, XEN arglist, const char *caller);
char *procedure_ok(XEN proc, int args, const char *caller, const char *arg_name, int argn);
int procedure_ok_with_error(XEN proc, int req_args, const char *caller, const char *arg_name, int argn);
void snd_protect(XEN obj);
void snd_unprotect(XEN obj);
int to_c_int_or_else(XEN obj, int fallback, const char *origin);
XEN g_c_run_or_hook (XEN hook, XEN args, const char *caller);
XEN g_c_run_and_hook (XEN hook, XEN args, const char *caller);
XEN g_c_run_progn_hook (XEN hook, XEN args, const char *caller);
void during_open(int fd, char *file, int reason);
void after_open(int index);

Float check_color_range(const char *caller, XEN val);
int string2int(char *str);
Float string2Float(char *str);
char *output_comment(file_info *hdr);
void snd_load_init_file(snd_state *ss, int nog, int noi);
void snd_load_file(char *filename);
XEN snd_eval_str(snd_state *ss, char *buf);
XEN snd_report_result(snd_state *ss, XEN result, char *buf);
XEN snd_report_listener_result(snd_state *ss, XEN form);
void snd_eval_property_str(snd_state *ss, char *buf);
void snd_eval_stdin_str(snd_state *ss, char *buf);
void g_snd_callback(int callb);
void clear_listener(void);
char *gl_print(XEN result, const char *caller);



/* -------- snd-select.c -------- */

int selection_is_active(void);
int selection_is_active_in_channel(chan_info *cp);
int selection_is_visible_in_channel(chan_info *cp);
int selection_beg(chan_info *cp);
int selection_end(chan_info *cp);
int selection_len(void);
int selection_chans(void);
int selection_srate(void);
void deactivate_selection(void);
void reactivate_selection(chan_info *cp, int beg, int end);
void ripple_selection(ed_list *new_ed, int beg, int num);
sync_info *selection_sync(void);
void start_selection_creation(chan_info *cp, int samp);
void update_possible_selection_in_progress(int samp);
int make_region_from_selection(void);
void display_selection(chan_info *cp);
int delete_selection(const char *origin, int regraph);
void move_selection(chan_info *cp, int x);
void finish_selection_creation(void);
int select_all(chan_info *cp);
int save_selection(snd_state *ss, char *ofile, int type, int format, int srate, char *comment, int chan);
int selection_creation_in_progress(void);
void cancel_selection_watch(void);
void add_selection_or_region(snd_state *ss, int reg, chan_info *cp, const char *origin);
void mix_selection_from_menu(snd_state *ss);
void paste_selection_from_menu(snd_state *ss);
void paste_selection_or_region(snd_state *ss, int reg, chan_info *cp, const char *origin);

void g_init_selection(void);
  

/* -------- snd-region.c -------- */

void allocate_regions(snd_state *ss, int numreg);
int region_ok(int n);
int region_len(int n);
int region_chans(int n);
int region_srate(int n);
Float region_maxamp(int n);
int stack_position_to_id(int n);
int id_to_stack_position(int id);
file_info *fixup_region_data(chan_info *cp, int chan, int n);
region_state *region_report(void);
void free_region_state (region_state *r);
int remove_region_from_stack(int pos);
void protect_region(int n, int protect);
int save_region(snd_state *ss, int n, char *ofile, int data_format);
void paste_region(int n, chan_info *cp, const char *origin);
void add_region(int n, chan_info *cp, const char *origin);
void region_stats(int *vals);
int define_region(sync_info *si, int *ends);
snd_fd *init_region_read (snd_state *ss, int beg, int n, int chan, int direction);
void cleanup_region_temp_files(void);
int snd_regions(void);
void save_regions(snd_state *ss, FILE *fd);
void region_edit(snd_state *ss, int reg);
void clear_region_backpointer(snd_info *sp);
void save_region_backpointer(snd_info *sp);

void g_init_regions(void);



/* -------- snd-env.c -------- */

Float un_dB(snd_state *ss, Float py);
env *copy_env(env *e);
env *free_env(env *e);
char *env_to_string(env *e);
int find_env(char *name);
env *make_envelope(Float *env_buffer, int len);
void move_point (env *e, int pos, Float x, Float y);
void delete_point(env *e, int pos);
env *default_env(Float x1, Float y);
void new_flt(snd_info *sp);
void snd_filter_cleanup(snd_info *sp);
void display_filter_graph(snd_state *ss, snd_info *sp, axis_context *ax, int width, int height);
void handle_filter_point(snd_state *ss, snd_info *sp, int evx, int evy, TIME_TYPE motion_time);
void handle_filter_press(snd_info *sp, int evx, int evy, TIME_TYPE time);
void handle_filter_release(snd_info *sp);
void report_filter_edit(snd_info *sp);
chan_info *new_env_axis(snd_state *ss);
void init_env_axes(chan_info *acp, char *name, int x_offset, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax);
axis_info *new_wave_axis(snd_state *ss);
void display_enved_env(snd_state *ss, env *e, axis_context *ax, chan_info *axis_cp, char *name, int x0, int y0, int width, int height, int dots, Float base);
void view_envs(snd_state *ss, int env_window_width, int env_window_height);
int hit_env(int xe, int ye, int env_window_width, int env_window_height);
void do_enved_edit(env *new_env);
void redo_env_edit(void);
void undo_env_edit(void);
void revert_env_edit(void);
int enved_all_envs_top(void);
char *enved_all_names(int n);
void set_enved_env_list_top(int n);
env *enved_all_envs(int pos);
void alert_envelope_editor(snd_state *ss, char *name, env *val);
void enved_show_background_waveform(snd_state *ss, chan_info *axis_cp, axis_info *gray_ap, int apply_to_mix, int apply_to_selection);
int enved_button_press_display(snd_state *ss, axis_info *ap, env *active_env, int evx, int evy);
void save_envelope_editor_state(FILE *fd);
char *env_name_completer(char *text);
env *enved_next_env(void);
env *string2env(char *str);
void add_or_edit_symbol(char *name, env *val);
env* name_to_env(char *str);
void delete_envelope(snd_state *ss, char *name);

XEN env_to_xen (env *e);
env *xen_to_env(XEN res);
env *get_env(XEN e, char *origin);
void g_init_env(void);
int check_enved_hook(env *e, int pos, Float x, Float y, int reason);



/* -------- snd-dac.c -------- */

char *reverb_name(void);
int mus_audio_compatible_format(int dev);
void cleanup_dac(void);
Float list_interp(Float x, Float *e, int pts);
void stop_playing_sound(snd_info *sp);
void stop_playing_sound_no_toggle(snd_info *sp);
void stop_playing_all_sounds(void);
void stop_playing_region(int n);
void play_region(snd_state *ss, int n, int background);
void play_channel(chan_info *cp, int start, int end, int background, XEN edpos, const char *caller, int arg_pos);
void play_sound(snd_info *sp, int start, int end, int background, XEN edpos, const char *caller, int arg_pos);
void play_channels(chan_info **cps, int chans, int *starts, int *ends, int background, XEN edpos, const char *caller, int arg_pos, int selection);
void play_selection(int background, XEN edpos, const char *caller, int arg_pos);
void toggle_dac_pausing(snd_state *ss); /* snd-dac.c */
int play_in_progress(void);
void initialize_apply(snd_info *sp, int chans, int beg, int frames);
void finalize_apply(snd_info *sp);
int run_apply(int ofd);

void g_init_dac(void);
snd_info *player(int index);
void clear_players(void);

void dac_set_expand(snd_info *sp, Float newval);
void dac_set_expand_length(snd_info *sp, Float newval);
void dac_set_expand_ramp(snd_info *sp, Float newval);
void dac_set_expand_hop(snd_info *sp, Float newval);
void dac_set_expand_scaler(snd_info *sp, Float newval);
void dac_set_contrast_amp(snd_info *sp, Float newval);
void dac_set_reverb_feedback(snd_info *sp, Float newval);
void dac_set_reverb_lowpass(snd_info *sp, Float newval);



/* -------- snd-chn.c -------- */


void zx_incremented(chan_info *cp, double amount);
void handle_cursor_with_sync(chan_info *cp, int decision);
int cursor_decision(chan_info *cp);
void reset_x_display(chan_info *cp, double sx, double zx);
void set_x_axis_x0x1 (chan_info *cp, Float x0, Float x1);
int cursor_move (chan_info *cp, int samps);
void set_wavo_trace(snd_state *ss, int uval);
void set_dot_size(snd_state *ss, int val);
chan_info *virtual_selected_channel(chan_info *cp);

void map_chans_field(snd_state *ss, int field, Float val);
void in_set_transform_graph_type(snd_state *ss, int val);
void in_set_fft_window(snd_state *ss, int val);
void combine_sound(snd_info *sp);
void separate_sound(snd_info *sp);
void superimpose_sound(snd_info *sp);
void set_sound_channel_style(snd_info *sp, int val);
void set_chan_fft_in_progress(chan_info *cp, BACKGROUND_FUNCTION_TYPE fp);
void goto_graph(chan_info *cp);
void start_amp_env(chan_info *cp);
void stop_amp_env(chan_info *cp);
int chan_fft_in_progress(chan_info *cp);
int force_fft_clear(chan_info *cp, void *ptr);
void chan_info_cleanup(chan_info *cp);
int update_graph(chan_info *cp, void *ptr);
void add_channel_data(char *filename, chan_info *cp, file_info *hdr, snd_state *ss, int graphed);
void add_channel_data_1(chan_info *cp, snd_info *sp, int graphed);
void handle_cursor(chan_info *cp, int redisplay);
void set_x_bounds(axis_info *ap);
void display_channel_data (chan_info *cp, snd_info *sp, snd_state *ss);
void display_channel_fft_data (chan_info *cp, snd_info *sp, snd_state *ss);
void display_channel_lisp_data (chan_info *cp, snd_info *sp, snd_state *ss);
void show_cursor_info(chan_info *cp);
void apply_x_axis_change(axis_info *ap, chan_info *cp, snd_info *sp);
void apply_y_axis_change (axis_info *ap, chan_info *cp);
void sx_incremented(chan_info *cp, double amount);
int move_axis(chan_info *cp, axis_info *ap, int x);
void set_axes(chan_info *cp, Float x0, Float x1, Float y0, Float y1);
void focus_x_axis_change(axis_info *ap, chan_info *cp, snd_info *sp, int focus_style);
int key_press_callback(chan_info *ur_cp, int x, int y, int key_state, int keysym);
void graph_button_press_callback(chan_info *cp, int x, int y, int key_state, int button, TIME_TYPE time);
void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button);
void graph_button_motion_callback(chan_info *cp, int x, int y, TIME_TYPE time, TIME_TYPE click_time);
int make_graph(chan_info *cp, snd_info *sp, snd_state *ss);
void reset_spectro(snd_state *state);
int cursor_moveto (chan_info *cp, int samp);

void g_init_chn(void);
XEN make_graph_data(chan_info *cp, int edit_pos, int losamp, int hisamp);
void draw_graph_data(chan_info *cp, int losamp, int hisamp, int data_size, Float *data, Float *data1, axis_context *ax, int style);

void fftb(chan_info *cp, int on);
void waveb(chan_info *cp, int on);
void f_button_callback(chan_info *cp, int on, int with_control);
void w_button_callback(chan_info *cp, int on, int with_control);
void edit_select_callback(chan_info *cp, int ed, int with_control);
axis_context *set_context (chan_info *cp, int gc);
axis_context *copy_context (chan_info *cp);
axis_context *erase_context (chan_info *cp);
axis_context *selection_context (chan_info *cp);
axis_context *mark_context (chan_info *cp);
axis_context *mix_waveform_context (chan_info *cp);
axis_context *selected_mix_waveform_context (chan_info *cp);
int calculate_fft(chan_info *cp, void *ptr);


/* -------- snd-axis.c -------- */

axis_info *free_axis_info(axis_info *ap);
Locus grf_x(double val, axis_info *ap);
Locus grf_y(Float val, axis_info *ap);
void make_axes_1(axis_info *ap, int x_style, int srate, int axes, int printing, int show_x_axis);

#define ungrf_x(AP, X) (((X) - (AP)->x_base) / (AP)->x_scale)
#define ungrf_y(AP, Y) (((Y) - (AP)->y_base) / (AP)->y_scale)

axis_info *make_axis_info (chan_info *cp, Float xmin, Float xmax, Float ymin, Float ymax, 
			   char *xlabel, Float x0, Float x1, Float y0, Float y1,
			   axis_info *old_ap);

void g_init_axis(void);




/* -------- snd-snd.c -------- */

env_info *free_amp_env(chan_info *cp, int pos);
void free_env_state(chan_info *cp);
env_state *make_env_state(chan_info *cp, int samples);
int tick_amp_env(chan_info *cp, env_state *es);
BACKGROUND_TYPE get_amp_env(GUI_POINTER ptr);
int amp_env_maxamp_ok(chan_info *cp, int edpos);
Float amp_env_maxamp(chan_info *cp, int edpos);
int amp_env_usable(chan_info *cp, Float samples_per_pixel, int hisamp, int start_new, int edit_pos);
int amp_env_graph(chan_info *cp, axis_info *ap, Float samples_per_pixel, int srate);
char *shortname(snd_info *sp);
char *shortname_indexed(snd_info *sp);
void add_sound_data(char *filename, snd_info *sp, snd_state *ss, int graphed);
Float srate_changed(Float ival, char *srcbuf, int style, int tones);
void sp_name_click(snd_info *sp);
void free_controls(snd_info *sp);
void save_controls(snd_info *sp);
void restore_controls(snd_info *sp);
void reset_controls(snd_info *sp);
void stop_applying(snd_info *sp);
void remove_apply(snd_info *sp);
BACKGROUND_TYPE apply_controls(GUI_POINTER xp);
void *make_apply_state_with_implied_beg_and_dur(void *xp);

void g_init_snd(void);
XEN snd_no_such_sound_error(const char *caller, XEN n);

void set_speed_style(snd_state *ss, int val);
void amp_env_scale_by(chan_info *cp, Float scl);
void amp_env_scale_selection_by(chan_info *cp, Float scl, int beg, int num);
env_info *amp_env_copy(chan_info *cp, int reversed, int edpos);
void remember_mini_string(snd_info *sp, char *str);
void restore_mini_string(snd_info *s, int back);
void clear_mini_strings(snd_info *sp);
void remember_filter_string(snd_info *sp, char *str);
void restore_filter_string(snd_info *s, int back);
void clear_filter_strings(snd_info *sp);



/* -------- snd-file -------- */

int disk_kspace (char *filename);
time_t file_write_date(char *filename);
int is_link(char *filename);
int is_directory(char *filename);
file_info *make_file_info(char *fullname, snd_state *ss);
file_info *free_file_info(file_info *hdr);
file_info *copy_header(char *fullname, file_info *ohdr);
file_info *make_temp_header(char *fullname, int srate, int chans, int samples, char *caller);
dir *free_dir (dir *dp);
int is_sound_file(char *name);
void init_sound_file_extensions(void);
dir *find_sound_files_in_dir (char *name);
dir *filter_sound_files(dir *dp, char *pattern);
snd_info *snd_open_file (char *filename, snd_state *ss, int read_only);
snd_info *snd_open_file_unselected (char *filename, snd_state *ss, int read_only);
void snd_close_file(snd_info *sp, snd_state *ss);
int copy_file(char *oldname, char *newname);
int move_file(char *oldfile, char *newfile);
snd_info *make_sound_readable(snd_state *ss, char *filename, int post_close);
void snd_update(snd_state *ss, snd_info *sp);
char *update_chan_stats(chan_info *cp);
char *view_curfiles_name(int pos);
void view_curfiles_play(snd_state *ss, int pos, int play);
void view_curfiles_select(snd_state *ss, int pos);
void view_curfiles_save(snd_state *ss, int pos);
void view_prevfiles_select(snd_state *ss, int pos);
int view_prevfiles_play(snd_state *ss, int pos, int play);
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
int find_curfile_regrow(char *shortname);
int find_prevfile_regrow(char *shortname);
void clear_prevlist(snd_state *ss);
void update_prevlist(snd_state *ss);
void init_curfiles(int size);
void init_prevfiles(int size);
void file_unprevlist(char *filename);
void update_prevfiles(snd_state *ss);
void add_directory_to_prevlist(snd_state *ss, char *dirname);
void make_prevfiles_list_1(snd_state *ss);
int num_header_types(void);
int num_data_formats(void);
char *data_format_name(int i);
char **set_header_and_data_positions(file_data *fdat, int type, int format);
int check_for_filename_collisions_and_save(snd_state *ss, snd_info *sp, char *str, int save_type, int srate, int type, int format, char *comment);
char *header_short_name(int i);
void edit_header_callback(snd_state *ss, snd_info *sp, file_data *edit_header_data);
char *raw_data_explanation(char *filename, snd_state *ss, file_info *hdr);
snd_info *snd_new_file(snd_state *ss, char *newname, int header_type, int data_format, int srate, int chans, char *new_comment);

int header_type_from_position(int pos);
int data_format_from_position(int header, int pos);
void set_header_type_and_format_from_position(file_data *fdat, int pos);
char **set_header_positions_from_type(file_data *fdat, int header_type, int data_format);

void g_init_file(void);




/* -------- snd-utils -------- */

int snd_round(Float x);
char *copy_string(const char *str);
char *snd_strdup(const char *str);
int snd_strlen(char *str);
char *filename_without_home_directory(char *name);
char *just_filename(char *name);
char *prettyf(Float num, int tens);
char *shorter_tempnam(char *dir, char *prefix);
char *snd_tempnam(snd_state *ss);
void fill_number(char *fs, char *ps);
void snd_exit(int val);
char *kmg (int num);
#ifdef DEBUG_MEMORY
  void set_encloser(const char *name);
#endif
#if DEBUGGING && HAVE_CLOCK
  void start_timing(void);
  void stop_timing(void);
#endif
#if TIMING
  void g_init_timing(void);
  int new_time(char *name);
#endif
XEN show_stack(void);



/* -------- snd-listener -------- */

void command_return(GUI_WIDGET w, snd_state *ss, int last_prompt);
char *listener_prompt_with_cr(snd_state *ss);
int check_balance(char *expr, int start, int end);
void update_stats_with_widget(snd_state *ss, GUI_WIDGET stats_form);

void g_init_listener(void);



/* -------- snd-mix.c -------- */

int disk_space_p(snd_info *sp, int fd, int bytes, int other_bytes, char *filename);
mix_context *cp_to_mix_context(chan_info *cp);
mix_context *make_mix_context(chan_info *cp);
mix_context *free_mix_context(mix_context *ms);
void free_mix_list(chan_info *cp);
void free_mixes(chan_info *cp);
int mixes(void);
int mix_complete_file(snd_info *sp, char *str, const char *origin, int with_tag);
int mix_array(int beg, int num, MUS_SAMPLE_TYPE **data, chan_info **out_cps, int in_chans, int out_chans, int nominal_srate, const char *origin, int with_tag);
int mix_file_and_delete(int beg, int num, char *file, chan_info **cps, int out_chans, const char *origin, int with_tag);
int copy_file_and_mix(int beg, int num, char *file, chan_info **cps, int out_chans, const char *origin, int with_tag);
void backup_mix_list(chan_info *cp, int edit_ctr);
int active_mix_p(chan_info *cp);
int mix_beg(chan_info *cp);
void reset_mix_graph_parent(chan_info *cp);
void display_channel_mixes(chan_info *cp);
void lock_affected_mixes(chan_info *cp, int beg, int end);
void release_pending_mixes(chan_info *cp, int edit_ctr);
void reset_mix_list(chan_info *cp);
void ripple_mixes(chan_info *cp, int beg, int change);
int goto_mix(chan_info *cp, int count);
int mix_length(int n);
int any_mix_id(void);
int set_mix_amp_env(int n, int chan, env *val);
env *mix_amp_env_from_id(int n, int chan);
void display_mix_amp_envs(snd_state *ss, chan_info *axis_cp, axis_context *ax, int width, int height);
void reflect_mix_edit(chan_info *input_cp, const char *origin);

void g_init_mix(void);

int mix_dragging(void);

void color_one_mix_from_id(int mix_id, COLOR_TYPE color);
COLOR_TYPE mix_to_color_from_id(int ix_id);

int display_mix_waveform_at_zero(chan_info *cp, int mix_id);
snd_info *make_mix_readable_from_id(int id);
int mix_selected_channel(int id);

void clear_mix_tags(chan_info *cp);
void move_mix_tag(int mix_tag, int x);
void finish_moving_mix_tag(int mix_tag, int x);
int hit_mix(chan_info *cp, int x, int y);
void select_mix_from_id(int mix_id);
chan_info *mix_channel_from_id(int mix_id);
void mix_play_from_id(int mix_id);
int current_mix_id(snd_state *ss);
void start_mix_drag(int mix_id);
int set_mix_speed_from_id(int mix_id, Float val, int dragging);
int set_mix_amp_from_id(int mix_id, int chan, Float val, int dragging);
void set_mix_track_from_id(int mix_id, int track);
int mix_track_from_id(int mix_id);
Float mix_speed_from_id(int mix_id);
Float mix_amp_from_id(int mix_id, int chan);
int mix_position_from_id(int mix_id);
char *mix_name_from_id(int mix_id);
int mix_input_chans_from_id(int mix_id);
void set_mix_name_from_id(int mix_id, char *name);
void set_mix_position_from_id(int mix_id, int beg);
int mix_ok(int n);


/* -------- snd-find.c -------- */

char *global_search(snd_state *ss, int direction);
int cursor_search(chan_info *cp, int count);

void g_init_find(void);



/* -------- snd-trans.c -------- */

int snd_translate(char *oldname, char *newname, int type);


/* -------- snd-rec.c -------- */

int record_in_progress(void);
void init_recorder(void);
void save_recorder_state(FILE *fd);
void close_recorder_audio(void);
void recorder_error(char *msg);
void g_init_recorder(void);
void fire_up_recorder(snd_state *ss);


/* -------- snd.c -------- */

snd_state *get_global_state(void);
#if SND_AS_WIDGET
  snd_state *snd_main(int argc, char **argv);
#endif


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
void snd_minibuffer_activate(snd_info *sp, int keysym, int with_meta);
int use_filename_completer(int filing);
int keyboard_command (chan_info *cp, int keysym, int state);

void g_init_kbd(void);



/* -------- snd-sig.c -------- */

void scale_by(chan_info *cp, Float *scalers, int len, int selection);
void scale_to(snd_state *ss, snd_info *sp, chan_info *cp, Float *scalers, int len, int selection);
Float get_maxamp(snd_info *sp, chan_info *cp, int edpos);
src_state *make_src(snd_state *ss, Float srate, snd_fd *sf);
Float run_src(src_state *sr, Float sr_change);
src_state *free_src(src_state *sr);
void src_env_or_num(snd_state *ss, chan_info *cp, env *e, Float ratio, int just_num, 
		    int from_enved, const char *origin, int over_selection, mus_any *gen, XEN edpos, int arg_pos, Float e_base);
void apply_filter(chan_info *ncp, int order, env *e, int from_enved, const char *origin, 
		  int over_selection, Float *ur_a, mus_any *gen, XEN edpos, int arg_pos);
void apply_env(chan_info *cp, env *e, int beg, int dur, Float scaler, int regexpr, 
	       int from_enved, const char *origin, mus_any *gen, XEN edpos, int arg_pos, Float e_base);
void cos_smooth(chan_info *cp, int beg, int num, int regexpr, const char *origin);
void display_frequency_response(snd_state *ss, env *e, axis_info *ap, axis_context *gax, int order, int dBing);
int cursor_delete(chan_info *cp, int count, const char *origin);
int cursor_delete_previous(chan_info *cp, int count, const char *origin);
int cursor_zeros(chan_info *cp, int count, int regexpr);
int cursor_insert(chan_info *cp, int beg, int count, const char *origin);
void fht(int powerOfFour, Float *array);

void g_init_sig(void);
int to_c_edit_position(chan_info *cp, XEN edpos, const char *caller, int arg_pos);
int to_c_edit_samples(chan_info *cp, XEN edpos, const char *caller, int arg_pos);



/* -------- snd-draw.c -------- */

#if (!USE_NO_GUI)
  axis_info *get_ap(chan_info *cp, int ap_id, const char *caller);
  void g_init_draw(void);
  void set_dialog_widget(snd_state *ss, int which, GUI_WIDGET wid);
#endif

#endif

