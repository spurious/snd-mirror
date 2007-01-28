#ifndef SND_1_H
#define SND_1_H

#define ASSERT_SOUND(Origin, Snd, Offset) \
  if (!((XEN_INTEGER_P(Snd)) || (XEN_FALSE_P(Snd)) || (XEN_NOT_BOUND_P(Snd)))) \
    XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset, Snd, "an integer (sound index), or " PROC_FALSE);

#define ASSERT_CHANNEL(Origin, Snd, Chn, Offset) \
  if (!((XEN_INTEGER_P(Snd)) || (XEN_FALSE_P(Snd)) || (XEN_NOT_BOUND_P(Snd)))) \
    XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset, Snd, "an integer (sound index), or " PROC_FALSE); \
  else \
    if (!((XEN_INTEGER_P(Chn)) || (XEN_FALSE_P(Chn)) || (XEN_NOT_BOUND_P(Chn)))) \
      XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset + 1, Chn, "an integer (0-based channel number) or " PROC_FALSE);

/* these macros fix up argument order for setter procs in Scheme: (set! (proc a b) c) */
/*    snd-edits has a 5 and a 10 case */
#if HAVE_GUILE

#define WITH_TWO_SETTER_ARGS(name_reversed, name) \
static XEN name_reversed(XEN arg1, XEN arg2)		\
{							\
  if (XEN_NOT_BOUND_P(arg2))				\
    return(name(arg1, XEN_UNDEFINED));			\
  return(name(arg2, arg1));				\
}

#define WITH_THREE_SETTER_ARGS(name_reversed, name) \
static XEN name_reversed(XEN arg1, XEN arg2, XEN arg3) \
  {						       \
    if (XEN_NOT_BOUND_P(arg2))			       \
      return(name(arg1, XEN_UNDEFINED, XEN_UNDEFINED)); \
    else {						\
      if (XEN_NOT_BOUND_P(arg3))			\
	return(name(arg2, arg1, XEN_UNDEFINED));	\
      else return(name(arg3, arg1, arg2));		\
}}

#define WITH_FOUR_SETTER_ARGS(name_reversed, name) \
static XEN name_reversed(XEN arg1, XEN arg2, XEN arg3, XEN arg4)	\
{									\
  if (XEN_NOT_BOUND_P(arg2))						\
    return(name(arg1, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));	\
  else {								\
    if (XEN_NOT_BOUND_P(arg3))						\
      return(name(arg2, arg1, XEN_UNDEFINED, XEN_UNDEFINED));		\
    else {								\
      if (XEN_NOT_BOUND_P(arg4))					\
	return(name(arg3, arg1, arg2, XEN_UNDEFINED));			\
      else return(name(arg4, arg1, arg2, arg3));			\
}}}

#else

#if HAVE_GAUCHE

#define WITH_TWO_SETTER_ARGS(name_reversed, name)	  \
static XEN name_reversed(XEN *argv, int argc, void *self) \
{							  \
  XEN args[2];						  \
  xen_gauche_load_args(args, argc, 2, argv);		  \
  if (XEN_NOT_BOUND_P(args[1]))				   \
    return(name(args[0], XEN_UNDEFINED));		   \
  return(name(args[1], args[0]));			   \
}

#define WITH_THREE_SETTER_ARGS(name_reversed, name) \
static XEN name_reversed(XEN *argv, int argc, void *self) \
{							  \
  XEN args[3];						  \
  xen_gauche_load_args(args, argc, 3, argv);		  \
  if (XEN_NOT_BOUND_P(args[1]))				   \
    return(name(args[0], XEN_UNDEFINED, XEN_UNDEFINED));   \
  else {						   \
    if (XEN_NOT_BOUND_P(args[2]))			   \
      return(name(args[1], args[0], XEN_UNDEFINED));	   \
    else return(name(args[2], args[0], args[1]));	   \
}}

#define WITH_FOUR_SETTER_ARGS(name_reversed, name) \
static XEN name_reversed(XEN *argv, int argc, void *self) \
{							  \
  XEN args[4];						  \
  xen_gauche_load_args(args, argc, 4, argv);		  \
  if (XEN_NOT_BOUND_P(args[1]))				   \
    return(name(args[0], XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));	\
  else {								\
    if (XEN_NOT_BOUND_P(args[2]))					\
      return(name(args[1], args[0], XEN_UNDEFINED, XEN_UNDEFINED));	\
    else {								\
      if (XEN_NOT_BOUND_P(args[3]))					\
	return(name(args[2], args[0], args[1], XEN_UNDEFINED));		\
      else return(name(args[3], args[0], args[1], args[2]));		\
}}}

#else
#define WITH_TWO_SETTER_ARGS(name_reversed, name)
#define WITH_THREE_SETTER_ARGS(name_reversed, name)
#define WITH_FOUR_SETTER_ARGS(name_reversed, name)
#endif
#endif


#define ASSERT_SAMPLE_TYPE(Origin, Beg, Offset) \
  XEN_ASSERT_TYPE(XEN_NUMBER_P(Beg) || XEN_FALSE_P(Beg) || XEN_NOT_BOUND_P(Beg), Beg, Offset, Origin, "a number or " PROC_FALSE)

typedef struct {
  char **values;
  int num_values, values_size;
  bool exact_match;
} list_completer_info;

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
  int format;             /* data format (mus_bshort etc) */
  int type;               /* header type (mus_aifc etc) */
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
  off_t data_bytes;        /* used only for edit-history descriptions (snd-edits.c display_ed_list) */
  bool free_me;
} snd_data;

typedef struct {
  int size, allocated_size;
  struct ed_fragment **fragments; /* defined in snd-edits.c */
  off_t beg, len;
  char *origin;
  int edit_type, sound_location, ptree_location;
  off_t selection_beg, selection_end;  /* selection needs to follow edit list */
  Float maxamp, selection_maxamp;
  off_t maxamp_position, selection_maxamp_position;
  int edpos;
  bool ptree_env_too, backed_up;
} ed_list;

typedef struct snd_fd {
  mus_sample_t (*run)(struct snd_fd *sf);
  Float (*runf)(struct snd_fd *sf);
  ed_list *current_state;
  struct ed_fragment *cb;
  off_t loc, first, last;
  int cbi;
  read_direction_t direction;
  bool at_eof;
  mus_sample_t *data;
  snd_data *current_sound;
  off_t initial_samp;
  struct chan_info *cp;
  struct snd_info *local_sp;
  Float fscaler;
#if (!SNDLIB_USE_FLOATS)
  int iscaler;
#endif
  off_t frag_pos;
  struct ptree *ptree1, *ptree2, *ptree3;
  XEN closure1, closure2, closure3;
  int protect1, protect2, protect3;
  double incr1, curval1, incr2, curval2, incr3, curval3, incr4, curval4;
  bool zero, xramp2;
  int edit_ctr, dangling_loc, region, type;
  mus_sample_t (*rev_run)(struct snd_fd *sf);
  Float (*rev_runf)(struct snd_fd *sf);
  Float (*rampf)(struct snd_fd *sf);
  Float (*rev_rampf)(struct snd_fd *sf);
} snd_fd;

typedef struct {Float freq; Float amp;} fft_peak;

typedef struct {
  Float y0, y1;                         /* scroller-dependent axis bounds */
  double x0, x1;
  double xmin, xmax;
  Float ymin, ymax;                     /* data-dependent absolute limits */
  Float y_scale, y_base, y_ambit;
  double x_scale, x_base, x_ambit;
  char *xlabel, *ylabel, *default_xlabel;
  Locus y_axis_x0, x_axis_x0, y_axis_y0, x_axis_y0, x_axis_x1, y_axis_y1, x_label_x, x_label_y;
  bool graph_active;
  off_t losamp, hisamp;                 /* displayed x-axis bounds in terms of sound sample numbers */
  Locus graph_x0;                       /* x axis offset relative to window (for double graphs) */
  struct tick_descriptor *x_ticks, *y_ticks; 
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
  int id, sync;
  bool visible;
} mark;

typedef struct {
  Float *data;
  int pts, data_size; /* data_size is independent of actual number of points of data (can be much larger) */
  Float base;
} env;

typedef struct fam_info {
  FAMRequest *rp;
  void (*action)(struct fam_info *fp, FAMEvent *fe);
  void *data;
#if MUS_DEBUGGING
  char *filename;
#endif
} fam_info;

typedef struct env_editor {
  int *current_xs;
  int *current_ys;
  int current_size;
  axis_info *axis;
  Tempus down_time;
  bool env_dragged;
  int env_pos;
  bool click_to_delete, in_dB, with_dots, clip_p;
  bool edited;
} env_editor;

typedef struct {
  int size;
  int current_size;
  mus_fft_window_t window;
  Float alpha, beta;
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
  struct lisp_grf *lisp_info; /* defined in snd-chn.c */
  bool cursor_on;          /* channel's cursor */
  bool cursor_visible, fft_cursor_visible;     /* for XOR decisions */
  off_t *cursors;          /* sample number (follows edit history) */
  int cursor_size;
  cursor_style_t cursor_style, tracking_cursor_style;
  int cx, cy, fft_cx;      /* graph-relative cursor loc (for XOR) */
  int edit_ctr;            /* channel's edit history */
  int edit_size;           /* current edit list size */
  ed_list **edits;         /* the edit list */
  int sound_size;          /* edit_list associated temp sound buffers */
  int sound_ctr;           /* current location in sounds list */
  snd_data **sounds;       /* the associated temp buffer/file/struct list */
  int ptree_size;          /* ditto for ptrees */
  int ptree_ctr;
  struct ptree **ptrees;
  XEN *ptree_inits;
  int *init_locs;
  struct enved_ffts *enved_spectra;
  fft_info *fft;           /* possibly null fft data */
  struct snd_info *sound;  /* containing sound */
  axis_info *axis;         /* time domain axis */
  mark ***marks;           /* current marks, indexed by edit_ctr, then mark_number, then the mark pointer */
  int marks_size;
  int *mark_size;
  int *mark_ctr;
  chan_context *cgx;       /* graphics/window context */
  chan_context *tcgx;      /* when combining chans, all should use chan[0]'s context */
  env_info **amp_envs;
  sono_info *sonogram_data;
  struct sonogram_state *last_sonogram, *temp_sonogram; /* defined in snd-fft.c */
  bool show_sonogram_cursor;
  struct fft_state *fft_data;          /* parallels sonogram -- try to avoid repeating large ffts needlessly */
  printing_t printing;
  fft_change_t fft_changed;
  Float gsy, gzy;
  int height;
  bool have_mixes;
  off_t original_cursor, original_left_sample, original_window_size;   /* for cursor reset after cursor-moving play */
  with_hook_t hookable;
  off_t selection_transform_size;
  bool squelch_update, previous_squelch_update, waiting_to_make_graph;
  int in_as_one_edit, as_one_edit_positions_size;
  int *as_one_edit_positions;
  /* moved from global to channel-local 4-Aug-00 */
  Float spectro_x_scale, spectro_y_scale, spectro_z_scale, spectro_z_angle, spectro_x_angle, spectro_y_angle, spectro_cutoff, spectro_start;
  Float lin_dB, min_dB, fft_window_alpha, fft_window_beta, beats_per_minute, grid_density;
  bool show_y_zero, show_marks, verbose_cursor;
  with_grid_t show_grid;
  int wavo_hop, wavo_trace, zero_pad, wavelet_type, max_transform_peaks, beats_per_measure;
  x_axis_style_t x_axis_style;
  show_axes_t show_axes;
  off_t transform_size;
  mus_fft_window_t fft_window;
  graph_type_t transform_graph_type, time_graph_type;
  bool show_transform_peaks, fft_log_frequency, fft_log_magnitude;
  graph_style_t time_graph_style, lisp_graph_style, transform_graph_style;
  Latus dot_size;
  fft_normalize_t transform_normalization;
  int transform_type, spectro_hop, edhist_base;
  bool show_mix_waveforms, graphs_horizontal, edit_hook_checked;
  XEN edit_hook;
  XEN undo_hook;
  XEN cursor_proc;
  XEN after_edit_hook;
  XEN properties;
  int cursor_proc_loc, edit_hook_loc, undo_hook_loc, after_edit_hook_loc, properties_loc;
  bool selection_visible, active;
  Locus old_x0, old_x1;
  Float *amp_control; /* local amp controls in snd-dac; should it be extended to other controls? */
  search_result_t last_search_result;
  bool just_zero, new_peaks, editable, tracking;
  track_info **tracks;
#if HAVE_GL
  int gl_fft_list;
#endif
} chan_info;

#define CURRENT_SAMPLES(Cp) (Cp)->samples[(Cp)->edit_ctr]
#define CURSOR(Cp) (Cp)->cursors[(Cp)->edit_ctr]

typedef struct {
  void (*watcher)(struct snd_info *sp, sp_watcher_reason_t reason, int list_loc);
  void *context;
  int loc;
  sp_watcher_t type;
} sp_watcher;

typedef struct snd_info {
  sound_inuse_t inuse;
  int index;
  int playing;
  mark *playing_mark;
  int sync, previous_sync;
  bool expand_control_p;
  bool contrast_control_p;
  bool reverb_control_p;
  bool filter_control_p, filter_control_in_dB, filter_control_in_hz;
  Float amp_control;
  Float speed_control;
  int speed_control_direction, speed_control_tones, speed_control_numerator, speed_control_denominator;
  speed_style_t speed_control_style;
  Float last_speed_control, last_amp_control, last_expand_control, last_contrast_control;
  Float last_reverb_control_length, last_reverb_control_scale;
  Float saved_speed_control, saved_amp_control, saved_expand_control, saved_contrast_control;
  Float saved_reverb_control_length, saved_reverb_control_scale;
  Float expand_control, expand_control_length, expand_control_ramp, expand_control_hop, expand_control_jitter;
  Float contrast_control, contrast_control_amp;
  Float reverb_control_length, reverb_control_scale, reverb_control_feedback, reverb_control_lowpass;
  Float reverb_control_decay, filter_control_xmax;
  Float contrast_control_min, contrast_control_max, expand_control_min, expand_control_max, speed_control_min, speed_control_max;
  Float amp_control_min, amp_control_max, reverb_control_scale_min, reverb_control_scale_max;
  Float reverb_control_length_min, reverb_control_length_max;
  int filter_control_order;
  bool filter_control_changed;
  env *filter_control_envelope;
  int selected_channel;
  char *filename;
  char *short_filename;
  int nchans;
  struct ptree *search_tree;
  XEN search_proc;
  XEN prompt_callback;
  XEN properties;
  int search_proc_loc, prompt_callback_loc, properties_loc;
  bool raw_prompt;
  char *search_expr;
  off_t marking;
  int search_count, amp_count; /* search_count = number of times to search before return, amp_count = amp env samps if not 1 (= full dur) */
  sp_filing_t filing;
  char *filing_filename;
  bool prompting, loading, finding_mark, selectioning;
  printing_t printing;
  off_t macro_count;
  minibuffer_choice_t minibuffer_on;
  bool user_read_only, file_read_only;
  chan_info **chans;
  snd_context *sgx;
  file_info *hdr;             /* header of file that would be affected if we were to save current edits */
  int bomb_ctr;
  time_t write_date;          /* check for change behind back while editing */
  bool need_update, file_unreadable; /* current in-core data does not match actual file (someone wrote it behind our back) */
  channel_style_t channel_style;
  int allocated_chans, selectpos; 
  tracking_cursor_t with_tracking_cursor;
  struct region *edited_region;
  struct dialog_play_info *delete_me;
  chan_info *lacp;
  struct ctrl_state *saved_controls;
  bool apply_ok, applying;
  struct mini_history *minibuffer_history, *filter_history;
  bool active;
  char *name_string;
  fam_info *file_watcher;
  bool writing, bomb_in_progress;
  sp_watcher **watchers;
  int watchers_size;
} snd_info;

#define SND_SRATE(sp) (((sp)->hdr)->srate)

typedef struct {
  void (*watcher)(ss_watcher_reason_t reason, void *data);
  void *context;
  int loc;
  ss_watcher_t type;
} ss_watcher;

typedef struct snd_state {
  int selected_sound;         /* NO_SELECTION = none selected = which sound is currently receiving user's attention */
  int active_sounds;
  Latus channel_min_height;
  snd_info **sounds;
  char *search_expr, *startup_title, *startup_errors;
  struct ptree *search_tree;
  XEN search_proc;
  int file_sorters_size, file_filters_size;
  XEN file_sorters, file_filters;
  int catch_exists, search_proc_loc, local_errno, local_open_errno;
  state_context *sgx;
  int position_slider_width, zoom_slider_width, toggle_size, channel_sash_indent, sash_size, channel_sash_size, sash_indent;
  int max_sounds, sound_sync_max;
  char *translated_filename;
  print_choice_t print_choice;
  snd_apply_t apply_choice;
  bool gl_has_double_buffer;
  bool stopped_explicitly, checking_explicitly;
  int reloading_updated_file;
  Latus init_window_width, init_window_height;
  Locus init_window_x, init_window_y;
  bool graph_hook_active, lisp_graph_hook_active;
  bool Show_Transform_Peaks, Show_Y_Zero, Show_Marks;
  with_grid_t Show_Grid;
  bool Fft_Log_Frequency, Fft_Log_Magnitude;
  channel_style_t Channel_Style;
  sound_style_t Sound_Style;
  show_axes_t Show_Axes;
  char *Eps_File, *Temp_Dir, *Save_Dir, *Ladspa_Dir;
  char *Listener_Font, *Axis_Label_Font, *Axis_Numbers_Font, *Tiny_Font, *Peaks_Font, *Bold_Peaks_Font;
  char *orig_listener_font, *orig_axis_label_font, *orig_axis_numbers_font, *orig_tiny_font, *orig_peaks_font, *orig_bold_peaks_font;
  bool Verbose_Cursor, Trap_Segfault;
  int Enved_Filter_Order;
  Float Vu_Size, Eps_Left_Margin, Eps_Bottom_Margin, Eps_Size, Log_Freq_Start;
  Float Spectro_X_Scale, Spectro_Y_Scale, Spectro_Z_Scale, Spectro_Z_Angle, Spectro_X_Angle, Spectro_Y_Angle, Spectro_Cutoff, Spectro_Start;
  int Default_Output_Header_Type, Default_Output_Data_Format, Default_Output_Chans, Default_Output_Srate;
  int Spectro_Hop, Color_Map, Color_Map_Size, Wavelet_Type, Transform_Type, Optimization;
  Latus Dot_Size;
  int Zero_Pad, Wavo_Hop, Wavo_Trace;
  off_t Transform_Size;
  mus_fft_window_t Fft_Window;
  graph_type_t Transform_Graph_Type, Time_Graph_Type;
  bool Ask_Before_Overwrite;
  Float Fft_Window_Alpha, Fft_Window_Beta, Grid_Density;
  Float Color_Scale, Color_Cutoff, Beats_Per_Minute;
  bool Color_Inverted, Show_Mix_Waveforms;
  int Mix_Waveform_Height, Beats_Per_Measure;
  fft_normalize_t Transform_Normalization;
  int Sinc_Width;
  x_axis_style_t X_Axis_Style;
  zoom_focus_t Zoom_Focus_Style;
  graph_style_t Graph_Style, Region_Graph_Style;
  bool Auto_Resize, Auto_Update;
  int Max_Regions, Max_Transform_Peaks;
  int Audio_Output_Device, Audio_Input_Device;
  bool Show_Backtrace, With_GL, With_Relative_Panes;
  int Print_Length, Dac_Size, View_Files_Sort;
  bool Dac_Combines_Channels, Show_Selection_Transform, With_Mix_Tags, Selection_Creates_Region;
  char *Save_State_File, *Listener_Prompt;
  Float Enved_Base, Enved_Power, Auto_Update_Interval;
  bool Enved_Wave_p, Graphs_Horizontal, With_Background_Processes, With_File_Monitor;
  env_type_t Enved_Style;
  int Graph_Cursor, Mix_Tag_Width, Mix_Tag_Height, Mark_Tag_Height, Mark_Tag_Width, Minibuffer_History_Length;
  enved_target_t Enved_Target;
  bool Clipping, Show_Indices, Just_Sounds;
  int Cursor_Size;
  cursor_style_t Cursor_Style, Tracking_Cursor_Style;
  bool Filter_Control_In_Db, Filter_Control_In_Hz, Show_Sonogram_Cursor;
  int Speed_Control_Tones;
  speed_style_t Speed_Control_Style;
  Float Expand_Control_Length, Expand_Control_Ramp, Expand_Control_Hop, Expand_Control_Jitter;
  Float Contrast_Control_Amp;
  Float Reverb_Control_Feedback, Reverb_Control_Lowpass;
  Float Reverb_Control_Decay, Cursor_Update_Interval;
  Float Contrast_Control_Min, Contrast_Control_Max, Expand_Control_Min, Expand_Control_Max, Speed_Control_Min, Speed_Control_Max;
  Float Amp_Control_Min, Amp_Control_Max, Reverb_Control_Scale_Min, Reverb_Control_Scale_Max;
  Float Reverb_Control_Length_Min, Reverb_Control_Length_Max;
  int Filter_Control_Order, Cursor_Location_Offset;
  Float Tempo_Control_Min, Tempo_Control_Max, Min_dB;
  bool Show_Controls, Vu_In_dB;
  tracking_cursor_t With_Tracking_Cursor;
  XEN cursor_proc;
  int cursor_proc_loc, listener_prompt_length;
  XEN zoom_focus_proc;
  int zoom_focus_proc_loc;
  Float lin_dB;
  char *HTML_Dir, *HTML_Program;
  bool error_lock;
  char *io_error_info;
  int deferred_regions;
  open_requestor_t open_requestor;
  void *open_requestor_data;
  bool batch_mode;
  bool jump_ok, exiting;
  env_editor *enved;
  Tempus click_time;
  bool fam_ok;
  FAMConnection *fam_connection;
  void (*snd_error_handler)(const char *error_msg, void *data);
  void *snd_error_data;
  void (*snd_warning_handler)(const char *warning_msg, void *data);
  void *snd_warning_data;
  void (*xen_error_handler)(const char *error_msg, void *data);
  void *xen_error_data;
  void (*snd_print_handler)(const char *msg, void *data);
  void *snd_print_data;
  ss_watcher **watchers;
  int watchers_size;
#if HAVE_GL && MUS_WITH_GL2PS
  bool gl_printing;
#endif
} snd_state;

extern snd_state *ss;

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

typedef struct {
  char *key;
  bool c, m, x;
} key_info;



/* -------- snd-io.c -------- */

int snd_creat(const char *filename, mode_t mode);
FILE *snd_fopen(const char *filename, const char *modes);
int snd_open(const char *filename, int flags, mode_t mode);

void snd_remove(const char *name, cache_remove_t forget);
void snd_close(int fd, const char *name);
void snd_fclose(FILE *fd, const char *name);
io_error_t copy_file(const char *oldname, const char *newname);
io_error_t move_file(const char *oldfile, const char *newfile);

int snd_open_read(const char *arg);
int snd_reopen_write(const char *arg);
io_error_t snd_write_header(const char *name, int type, int srate, int chans, off_t samples, 
			    int format, const char *comment, int len, int *loops);
io_error_t sndlib_error_to_snd(int sndlib_err);
int snd_file_open_descriptors(int tfd, const char *name, int format, off_t location, int chans, int type);
snd_io *make_file_state(int fd, file_info *hdr, int chan, off_t beg, int suggested_bufsize);
void file_buffers_forward(off_t ind0, off_t ind1, off_t indx, snd_fd *sf, snd_data *cur_snd);
void file_buffers_back(off_t ind0, off_t ind1, off_t indx, snd_fd *sf, snd_data *cur_snd);
void remember_temp(const char *filename, int chans);
void forget_temps(void);
snd_data *make_snd_data_file(const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int ctr, int temp_chan);
snd_data *copy_snd_data(snd_data *sd, off_t beg, int bufsize);
snd_data *free_snd_data(snd_data *sf);
snd_data *make_snd_data_buffer(mus_sample_t *data, int len, int ctr);
snd_data *make_snd_data_buffer_for_simple_channel(int len);
int open_temp_file(const char *ofile, int chans, file_info *hdr, io_error_t *err);
io_error_t close_temp_file(const char *filename, int ofd, int type, off_t bytes);
#if MUS_DEBUGGING
  void mem_report(void);
#endif


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
void track_help(void);
void sound_files_help(void);
void init_file_help(void);
void recording_help(void);
void region_help(void);
void selection_help(void);
void colors_help(void);
char *version_info(void);
void transform_dialog_help(void);
void color_dialog_help(void);
void orientation_dialog_help(void);
void envelope_editor_dialog_help(void);
void region_dialog_help(void);
void raw_data_dialog_help(const char *info);
void new_file_dialog_help(void);
void edit_header_dialog_help(void);
void print_dialog_help(void);
void view_files_dialog_help(void);
void mix_dialog_help(void);
void track_dialog_help(void);
void find_dialog_help(void);
void completion_dialog_help(void);
void open_file_dialog_help(void);
void mix_file_dialog_help(void);
void insert_file_dialog_help(void);
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
void name_to_html_viewer(const char *red_text);
void url_to_html_viewer(const char *url);
bool snd_topic_help(const char *topic);
char **help_name_to_xrefs(const char *name);


/* -------- snd-menu.c -------- */

void reflect_file_revert_in_label(snd_info *sp);
void set_menu_label(widget_t w, const char *label);
void g_init_menu(void);


/* -------- snd-main.c -------- */

int add_ss_watcher(ss_watcher_t type, void (*watcher)(ss_watcher_reason_t reason, void *data), void *context);
/* bool remove_ss_watcher(int loc); */
void call_ss_watchers(ss_watcher_t type, ss_watcher_reason_t reason);
char *save_options_in_prefs(void);
void open_save_sound_block(snd_info *sp, FILE *fd, bool with_nth);
void close_save_sound_block(FILE *fd, bool need_f);
bool snd_exit_cleanly(bool force_exit);
void sound_not_current(snd_info *sp);
void save_state(const char *save_state_name);
void global_control_panel_state(void);
void global_fft_state(void);
int handle_next_startup_arg(int auto_open_ctr, char **auto_open_file_names, bool with_title, int args);

void g_init_main(void);


/* --------- snd-error.c -------- */

const char *io_error_name(io_error_t err);
#ifdef __GNUC__
  void snd_error(char *format, ...)  __attribute__ ((format (printf, 1, 2)));
  void snd_warning(char *format, ...)  __attribute__ ((format (printf, 1, 2)));
#else
  void snd_error(char *format, ...);
  void snd_warning(char *format, ...);
#endif
void snd_error_without_redirection_or_hook(const char *msg);
void snd_error_without_format(const char *msg);
void snd_warning_without_format(const char *msg);
bool run_snd_error_hook(const char *msg);
void g_init_errors(void);

#ifdef SND_AS_WIDGET
  void set_error_display(void (*func)(const char *msg));
#endif
void redirect_snd_error_to(void (*handler)(const char *error_msg, void *ufd), void *data);
void redirect_snd_warning_to(void (*handler)(const char *warning_msg, void *ufd), void *data);



/* -------- snd-completion.c -------- */

char *command_completer(char *text, void *data);
int add_completer_func(char *(*func)(char *text, void *context), void *data);
int get_completion_matches(void);
void set_completion_matches(int matches);
void set_save_completions(bool save);
void add_possible_completion(const char *text);
void display_completions(void);
char *complete_text(char *text, int func);
void clear_possible_completions(void);
char *filename_completer(char *text, void *data);
char *sound_filename_completer(char *text, void *data);
char *srate_completer(char *text, void *data);
char *list_completer(char *text, void *data);
char *info_completer(char *text, void *data);
char *complete_listener_text(char *old_text, int end, bool *try_completion, char **to_file_text);
bool separator_char_p(char c);
void add_srate_to_completion_list(int srate);
list_completer_info *srate_list(void);
char *srate_list_to_string(int row);


/* -------- snd-print.c -------- */

void ps_set_grf_points(double x, int j, Float ymin, Float ymax);
void ps_set_grf_point(double x, int j, Float y);
void ps_allocate_grf_points(void);
void ps_draw_grf_points(axis_info *ap, int j, Float y0, graph_style_t graph_style, int dot_size);
void ps_draw_both_grf_points(axis_info *ap, int j, graph_style_t graph_style, int dot_size);
void ps_draw_sono_rectangle(axis_info *ap, int color, Float x, Float y, Float width, Float height);
void ps_reset_color(void);
void ps_bg(axis_info *ap, axis_context *ax);
void ps_fg(axis_context *ax);
void ps_draw_line(axis_info *ap, int x0, int y0, int x1, int y1);
void ps_draw_spectro_line(axis_info *ap, int color, Float x0, Float y0, Float x1, Float y1);
void ps_fill_rectangle(axis_info *ap, int x0, int y0, int width, int height);
void ps_draw_string(axis_info *ap, int x0, int y0, const char *str);
void ps_set_number_font(void);
void ps_set_label_font(void);
void ps_set_bold_peak_numbers_font(void);
void ps_set_peak_numbers_font(void);
void ps_set_tiny_numbers_font(void);
bool snd_print(const char *output);
void region_print(const char *output, const char *title, chan_info *cp);
void print_enved(const char *output, int y0);
void g_init_print(void);



/* -------- snd-marks.c -------- */

int mark_sync_max(void);
void set_mark_sync(mark *m, int val);
void marks_off(chan_info *cp);
mark *hit_mark(chan_info *cp, int x, int y, int key_state);
mark *hit_triangle(chan_info *cp, int x, int y);
void move_mark(chan_info *cp, mark *mp, int x);
void play_syncd_mark(chan_info *cp, mark *mp);
off_t move_play_mark(chan_info *cp, off_t *mc, Locus cx);
void finish_moving_play_mark(chan_info *cp);
void finish_moving_mark(chan_info *cp, mark *m);
mark *add_mark(off_t samp, const char *name, chan_info *cp);
bool delete_mark_samp(off_t samp, chan_info *cp);
void free_mark_list(chan_info *cp, int ignore);
void collapse_marks(snd_info *sp);
bool goto_mark(chan_info *cp, int count);
void goto_named_mark(chan_info *cp, const char *name);
mark *active_mark(chan_info *cp);
off_t mark_beg(chan_info *cp);
void display_channel_marks(chan_info *cp);
void release_pending_marks(chan_info *cp, int edit_ctr);
void ripple_marks(chan_info *cp, off_t beg, off_t change);
bool mark_define_region(chan_info *cp, int count);
void save_mark_list(FILE *fd, chan_info *cp, bool all_chans);
void reverse_marks(chan_info *cp, off_t beg, off_t dur);
void src_marks(chan_info *cp, Float ratio, off_t old_samps, off_t new_samps, off_t beg, bool over_selection);
void reset_marks(chan_info *cp, int cur_marks, off_t *samps, off_t end, off_t extension, bool over_selection);
void ripple_trailing_marks(chan_info *cp, off_t beg, off_t old_len, off_t new_len);
void swap_marks(chan_info *cp0, chan_info *cp1);
void g_init_marks(void);
void *sound_store_marks(snd_info *sp);
void sound_restore_marks(snd_info *sp, void *marks);
void backup_mark_list(chan_info *cp, int cur);
off_t mark_id_to_sample(int id);



/* -------- snd-data.c -------- */

chan_info *make_chan_info(chan_info *cip, int chan, snd_info *sound);
snd_info *make_snd_info(snd_info *sip, const char *filename, file_info *hdr, int snd_slot, bool read_only);
snd_info *make_basic_snd_info(int chans);
void initialize_control_panel(snd_info *sp);
void free_snd_info(snd_info *sp);
snd_info *completely_free_snd_info(snd_info *sp);
void for_each_chan_with_int(void (*func)(chan_info *ncp, int val), int value);
void for_each_chan_with_bool(void (*func)(chan_info *ncp, bool val), bool value);
void for_each_chan_with_float(void (*func)(chan_info *ncp, Float val), Float value);
void for_each_chan(void (*func)(chan_info *ncp));
void for_each_normal_chan(void (*func)(chan_info *ncp));
void for_each_normal_chan_with_void(void (*func)(chan_info *ncp, void *ptr), void *userptr);
void for_each_normal_chan_with_int(void (*func)(chan_info *ncp, int val), int value);
void for_each_normal_chan_with_refint(void (*func)(chan_info *ncp, int *val), int *value);
bool map_over_sound_chans(snd_info *sp, bool (*func)(chan_info *ncp));
bool map_over_sound_chans_with_void(snd_info *sp, bool (*func)(chan_info *ncp, void *ptr), void *userptr);
bool map_over_sound_chans_with_int(snd_info *sp, bool (*func)(chan_info *ncp, int val1), int value);
void for_each_sound_chan(snd_info *sp, void (*func)(chan_info *ncp));
void for_each_sound(void (*func)(snd_info *usp));
void for_each_sound_with_void(void (*func)(snd_info *usp, void *ptr), void *userptr);
void for_each_sound_with_int(void (*func)(snd_info *usp, int val), int value);
bool map_over_separate_chans(bool (*func)(chan_info *ncp));
bool map_over_separate_chans_with_int(bool (*func)(chan_info *ncp, int val), int value);
bool snd_ok(snd_info *sp);
int active_channels(virtual_channels_t count_virtual_channels);
int find_free_sound_slot(int desired_chans);
int find_free_sound_slot_for_channel_display(void);
snd_info *selected_sound(void);
chan_info *selected_channel(void);
chan_info *color_selected_channel(snd_info *sp);
snd_info *any_selected_sound(void);
chan_info *any_selected_channel(snd_info *sp);
void select_channel(snd_info *sp, int chan);
chan_info *current_channel(void);
sync_info *free_sync_info(sync_info *si);
sync_info *snd_sync(int sync);
sync_info *sync_to_chan(chan_info *cp);
sync_info *make_simple_sync(chan_info *cp, off_t beg);
snd_info *find_sound(const char *name, int nth);
void display_info(snd_info *sp);

void g_init_data(void);



/* -------- snd-edits.c -------- */

void allocate_ed_list(chan_info *cp);
ed_list *initial_ed_list(off_t beg, off_t end);
snd_info *sound_is_silence(snd_info *sp);
off_t edit_changes_begin_at(chan_info *cp, int edpos);
off_t edit_changes_end_at(chan_info *cp, int edpos);
bool has_unsaved_edits(snd_info *sp);
char *run_save_state_hook(char *filename);
void edit_history_to_file(FILE *fd, chan_info *cp, bool with_save_state_hook);
char *edit_to_string(chan_info *cp, int edit);
void free_edit_list(chan_info *cp);
void backup_edit_list(chan_info *cp);
void as_one_edit(chan_info *cp, int one_edit);
void free_sound_list(chan_info *cp);
void free_ptree_list(chan_info *cp);
void release_dangling_readers(chan_info *cp, int edit_ctr);
void after_edit(chan_info *cp);
bool extend_with_zeros(chan_info *cp, off_t beg, off_t num, int edpos);
void extend_edit_list(chan_info *cp, int edpos);
bool insert_samples(off_t beg, off_t num, mus_sample_t *vals, chan_info *cp, const char *origin, int edpos);
bool file_insert_samples(off_t beg, off_t num, const char *tempfile, chan_info *cp, int chan, 
			 file_delete_t auto_delete, const char *origin, int edpos);
bool insert_complete_file_at_cursor(snd_info *sp, const char *filename);
bool insert_complete_file(snd_info *sp, const char *str, off_t chan_beg, file_delete_t auto_delete);
bool delete_samples(off_t beg, off_t num, chan_info *cp, int edpos);
bool change_samples(off_t beg, off_t num, mus_sample_t *vals, chan_info *cp, lock_mix_t lock, const char *origin, int edpos);
bool file_change_samples(off_t beg, off_t num, const char *tempfile, chan_info *cp, int chan, 
			 file_delete_t auto_delete, lock_mix_t lock, const char *origin, int edpos);
bool file_mix_change_samples(off_t beg, off_t num, const char *tempfile, chan_info *cp, int chan, 
			     file_delete_t auto_delete, lock_mix_t lock, const char *origin, int edpos, bool with_mix);
bool file_override_samples(off_t num, const char *tempfile, chan_info *cp, int chan, 
			   file_delete_t auto_delete, lock_mix_t lock, const char *origin);
Float chn_sample(off_t samp, chan_info *cp, int pos);
void check_saved_temp_file(const char *type, XEN filename, XEN date_and_length);
bool editable_p(chan_info *cp);
snd_fd *free_snd_fd(snd_fd *sf);
char *sf_to_string(snd_fd *fd);
void release_region_readers(int reg);
bool sf_p(XEN obj);
snd_fd *get_sf(XEN obj);
snd_fd *free_snd_fd_almost(snd_fd *sf);
bool scale_channel(chan_info *cp, Float scaler, off_t beg, off_t num, int pos, bool in_as_one_edit);
bool scale_channel_with_origin(chan_info *cp, Float scl, off_t beg, off_t num, int pos, bool in_as_one_edit, const char *origin);
bool ramp_channel(chan_info *cp, Float rmp0, Float rmp1, off_t beg, off_t num, int pos, bool in_as_one_edit);
bool xramp_channel(chan_info *cp, Float rmp0, Float rmp1, Float scaler, Float offset, 
		   off_t beg, off_t num, int pos, bool in_as_one_edit, mus_any *e, int e_pos);
void ptree_channel(chan_info *cp, struct ptree *tree, off_t beg, off_t num, int pos, bool env_it, XEN init_func, const char *origin);
snd_fd *init_sample_read(off_t samp, chan_info *cp, read_direction_t direction);
snd_fd *init_sample_read_any(off_t samp, chan_info *cp, read_direction_t direction, int edit_position);
void read_sample_change_direction(snd_fd *sf, read_direction_t dir);
bool ramp_or_ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos, bool is_xramp);
bool ptree_or_sound_fragments_in_use(chan_info *cp, int pos);
bool ptree_fragments_in_use(chan_info *cp, off_t beg, off_t dur, int pos);
#define read_sample(Sf) (*((Sf)->run))(Sf)
#define read_sample_to_float(Sf) (*((Sf)->runf))(Sf)
Float protected_next_sample_to_float(snd_fd *sf);
Float protected_previous_sample_to_float(snd_fd *sf);
Float channel_local_maxamp(chan_info *cp, off_t beg, off_t num, int edpos, off_t *maxpos);
bool undo_edit_with_sync(chan_info *cp, int count);
bool redo_edit_with_sync(chan_info *cp, int count);
bool undo_edit(chan_info *cp, int count);
bool redo_edit(chan_info *cp, int count);
io_error_t save_channel_edits(chan_info *cp, const char *ofile, int pos);
io_error_t save_edits(snd_info *sp);
io_error_t save_edits_without_asking(snd_info *sp);
void save_edits_with_prompt(snd_info *sp);
io_error_t save_edits_and_update_display(snd_info *sp);
io_error_t save_edits_without_display(snd_info *sp, const char *new_name, int type, int format, int srate, const char *comment, int pos);
void revert_edits(chan_info *cp);
off_t current_location(snd_fd *sf);
void g_init_edits(void);
void set_ed_maxamp(chan_info *cp, int edpos, Float val);
Float ed_maxamp(chan_info *cp, int edpos);
void set_ed_maxamp_position(chan_info *cp, int edpos, off_t val);
off_t ed_maxamp_position(chan_info *cp, int edpos);
void set_ed_selection_maxamp(chan_info *cp, Float val);
Float ed_selection_maxamp(chan_info *cp);
void set_ed_selection_maxamp_position(chan_info *cp, off_t val);
off_t ed_selection_maxamp_position(chan_info *cp);
void copy_then_swap_channels(chan_info *cp0, chan_info *cp1, int pos0, int pos1);
void reflect_file_change_in_label(chan_info *cp);
void update_track_lists(chan_info *cp, int top_ctr);
void reflect_file_change_in_title(void);

bool snd_to_sample_p(mus_any *ptr);
Float snd_to_sample_read(mus_any *ptr, off_t frame, int chan);


/* -------- snd-fft.c -------- */

int find_and_sort_transform_peaks(Float *buf, fft_peak *found, int num_peaks, int fftsize, int srate, Float samps_per_pixel, Float fft_scale);
int find_and_sort_peaks(Float *buf, fft_peak *found, int num_peaks, int size);
fft_info *free_fft_info(fft_info *fp);
void free_sonogram_fft_state(void *ptr);
bool fft_window_beta_in_use(mus_fft_window_t win);
bool fft_window_alpha_in_use(mus_fft_window_t win);
void free_sono_info(chan_info *cp);
void sono_update(chan_info *cp);
void set_spectro_cutoff_and_redisplay(Float val);
void c_convolve(const char *fname, Float amp, int filec, off_t filehdr, int filterc, off_t filterhdr, int filtersize,
		 int fftsize, int filter_chans, int filter_chan, int data_size, snd_info *gsp, enved_progress_t from_enved, int ip, int total_chans);
void *make_sonogram_state(chan_info *cp, bool force_recalc);
void single_fft(chan_info *cp, bool update_display, bool force_recalc);
Cessate sonogram_in_slices(void *sono);
void clear_transform_edit_ctrs(chan_info *cp);
void g_init_fft(void);
Float fft_beta_max(mus_fft_window_t win);
void cp_free_fft_state(chan_info *cp);
void autocorrelation(Float *data, int n);
void set_fft_info_xlabel(chan_info *cp, const char *new_label);
void fourier_spectrum(snd_fd *sf, Float *data, int fft_size, int data_len, Float *window);
char *wavelet_name(int i);
char **wavelet_names(void);
void set_log_freq_start(Float base);

char *transform_name(int type);
char *transform_program_name(int type);
int transform_position_to_type(int pos);
int transform_type_to_position(int type);
int max_transform_type(void);
void set_transform_position(int i, int j);
bool transform_p(int type);


/* -------- snd-xen.c -------- */

void redirect_xen_error_to(void (*handler)(const char *msg, void *ufd), void *data);
void redirect_snd_print_to(void (*handler)(const char *msg, void *ufd), void *data);
void redirect_errors_to(void (*handler)(const char *msg, void *ufd), void *data);
void redirect_everything_to(void (*handler)(const char *msg, void *ufd), void *data);
XEN snd_catch_any(XEN_CATCH_BODY_TYPE body, void *body_data, const char *caller);
XEN snd_throw(XEN key, XEN args);
XEN snd_no_such_file_error(const char *caller, XEN filename);
XEN snd_no_such_channel_error(const char *caller, XEN snd, XEN chn);
XEN snd_bad_arity_error(const char *caller, XEN errstr, XEN proc);
XEN snd_no_active_selection_error(const char *caller);
void g_xen_initialize(void);
XEN eval_str_wrapper(void *data);
XEN eval_form_wrapper(void *data);
XEN string_to_form(char *data);
char *g_print_1(XEN obj);
XEN g_c_make_sample_reader(snd_fd *fd);
#if HAVE_GUILE
  XEN g_call0(XEN proc, const char *caller);
  XEN g_call1(XEN proc, XEN arg, const char *caller);
  XEN g_call2(XEN proc, XEN arg1, XEN arg2, const char *caller);
  XEN g_call3(XEN proc, XEN arg1, XEN arg2, XEN arg3, const char *caller);
  XEN g_call_any(XEN proc, XEN arglist, const char *caller);
#endif
char *procedure_ok(XEN proc, int args, const char *caller, const char *arg_name, int argn);
bool procedure_arity_ok(XEN proc, int args);
#if MUS_DEBUGGING
  int snd_protect_1(XEN obj, const char *caller);
  #define snd_protect(Obj) snd_protect_1(Obj, c__FUNCTION__)
#else
  int snd_protect(XEN obj);
#endif
void snd_unprotect_at(int loc);
XEN snd_protected_at(int loc);
XEN run_or_hook(XEN hook, XEN args, const char *caller);
XEN run_progn_hook(XEN hook, XEN args, const char *caller);
XEN run_hook(XEN hook, XEN args, const char *caller);
bool listener_print_p(const char *msg);
void check_features_list(char *features);
#if (!USE_NO_GUI)
  Float check_color_range(const char *caller, XEN val);
#endif
void set_basic_color(color_t color);
void set_highlight_color(color_t color);
void set_position_color(color_t color);
void set_zoom_color(color_t color);
void set_data_color(color_t color);
void set_selected_data_color(color_t color);
void set_graph_color(color_t color);
void set_selected_graph_color(color_t color);
Float string_to_Float(char *str, Float lo, const char *file_name);
int string_to_int(char *str, int lo, const char *field_name);
off_t string_to_off_t(char *str, off_t lo, const char *field_name);
char *output_comment(file_info *hdr);
void snd_load_init_file(bool nog, bool noi);
void snd_load_file(char *filename);
void snd_report_result(XEN result, const char *buf);
void snd_report_listener_result(XEN form);
void snd_eval_stdin_str(char *buf);
void clear_stdin(void);
#if HAVE_RUBY
  void snd_rb_raise(XEN type, XEN info);
#endif
void run_watchers(void);
bool source_file_p(const char *name);
void save_added_source_file_extensions(FILE *fd);



/* -------- snd-select.c -------- */

int add_selection_watcher(void (*watcher)(selection_watcher_reason_t reason, void *data), void *context);
void remove_selection_watcher(int loc);
void call_selection_watchers(selection_watcher_reason_t reason);
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
bool delete_selection(cut_selection_regraph_t regraph);
void move_selection(chan_info *cp, int x);
void finish_selection_creation(void);
int select_all(chan_info *cp);
io_error_t save_selection(const char *ofile, int type, int format, int srate, const char *comment, int chan);
bool selection_creation_in_progress(void);
void cancel_selection_watch(void);
void add_selection_or_region(int reg, chan_info *cp);
void insert_selection_from_menu(void);
void insert_selection_or_region(int reg, chan_info *cp);

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
void free_region_state(region_state *r);
int remove_region_from_list(int pos);
io_error_t paste_region(int n, chan_info *cp);
io_error_t add_region(int n, chan_info *cp);
int define_region(sync_info *si, off_t *ends);
snd_fd *init_region_read(off_t beg, int n, int chan, read_direction_t direction);
void cleanup_region_temp_files(void);
int snd_regions(void);
void save_regions(FILE *fd);
io_error_t save_region(int rg, const char *name, int type, int format, const char *comment);
void region_edit(int reg);
void clear_region_backpointer(snd_info *sp);
void save_region_backpointer(snd_info *sp);
void sequester_deferred_regions(chan_info *cp, int edit_top);
void g_init_regions(void);
void for_each_region_chan_with_refint(void (*func)(chan_info *ncp, int *val), int *value);
off_t region_current_location(snd_fd *fd);
char *region_description(int rg);


/* -------- snd-env.c -------- */

env *copy_env(env *e);
bool envs_equal(env *e1, env *e2);
env *free_env(env *e);
char *env_to_string(env *e);
env *make_envelope(Float *env_buffer, int len);
env *make_envelope_with_offset_and_scaler(Float *env_buffer, int len, Float offset, Float scaler);
env *window_env(env *e, off_t local_beg, off_t local_dur, off_t e_beg, off_t e_dur, Float maxx);
env *multiply_envs(env *e1, env *e2, Float maxx);
env *invert_env(env *e);
env *default_env(Float x1, Float y);
bool default_env_p(env *e);
env_editor *new_env_editor(void);
void env_editor_button_motion(env_editor *edp, int evx, int evy, Tempus motion_time, env *e);
bool env_editor_button_press(env_editor *edp, int evx, int evy, Tempus time, env *e);
void env_editor_button_release(env_editor *edp, env *e);
double env_editor_ungrf_y_dB(env_editor *edp, int y);
void init_env_axes(axis_info *ap, const char *name, int x_offset, int ey0, int width, int height, 
		   Float xmin, Float xmax, Float ymin, Float ymax, printing_t printing);
void env_editor_display_env(env_editor *edp, env *e, axis_context *ax, const char *name, 
			    int x0, int y0, int width, int height, printing_t printing);
void view_envs(int env_window_width, int env_window_height, printing_t printing);
int hit_env(int xe, int ye, int env_window_width, int env_window_height);
void prepare_enved_edit(env *new_env);
void redo_env_edit(void);
void undo_env_edit(void);
void revert_env_edit(void);
int enved_all_envs_top(void);
char *enved_all_names(int n);
void set_enved_env_list_top(int n);
env *enved_all_envs(int pos);
void alert_envelope_editor(char *name, env *val);
void enved_show_background_waveform(axis_info *ap, axis_info *gray_ap, bool apply_to_selection, bool show_fft, printing_t printing);
void save_envelope_editor_state(FILE *fd);
char *env_name_completer(char *text, void *data);
env *enved_next_env(void);
env *string_to_env(char *str);
void add_or_edit_symbol(char *name, env *val);
env* name_to_env(const char *str);
env *position_to_env(int pos);
void delete_envelope(char *name);
void free_enved_spectra(chan_info *cp);
void release_dangling_enved_spectra(chan_info *cp, int edpt);
void reflect_enved_spectra_change(chan_info *cp);

XEN env_to_xen(env *e);
env *xen_to_env(XEN res);
env *get_env(XEN e, const char *origin);
void g_init_env(void);


/* -------- snd-dac.c -------- */

void cleanup_dac(void);
void stop_playing_sound(snd_info *sp, play_stop_t reason);
void stop_playing_sound_without_hook(snd_info *sp, play_stop_t reason);
void stop_playing_sound_no_toggle(snd_info *sp, play_stop_t reason);
void stop_playing_all_sounds(play_stop_t reason);
void stop_playing_region(int n, play_stop_t reason);
void play_region(int n, play_process_t background);
void play_region_1(int region, play_process_t background, XEN stop_proc);
void play_channel(chan_info *cp, off_t start, off_t end);
void play_sound(snd_info *sp, off_t start, off_t end);
void play_channels(chan_info **cps, int chans, off_t *starts, off_t *ur_ends, 
		   play_process_t background, XEN edpos, bool selection, const char *caller, int arg_pos);
void play_selection(play_process_t background);
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

chan_info *get_cp(XEN snd_n, XEN chn_n, const char *caller);
snd_info *make_simple_channel_display(int srate, int initial_length, fw_button_t with_arrows, 
				      graph_style_t grf_style, widget_t container, bool with_events);
axis_info *lisp_info_axis(chan_info *cp);
void free_lisp_info(chan_info *cp);
void zx_incremented(chan_info *cp, double amount);
kbd_cursor_t cursor_decision(chan_info *cp);
void reset_x_display(chan_info *cp, double sx, double zx);
void set_x_axis_x0x1(chan_info *cp, double x0, double x1);
void cursor_move(chan_info *cp, off_t samps);
void cursor_moveto_without_verbosity(chan_info *cp, off_t samp);
void cursor_moveto_with_window(chan_info *cp, off_t samp, off_t left_samp, off_t window_size);
void set_wavo_trace(int uval);
void set_dot_size(int val);
chan_info *virtual_selected_channel(chan_info *cp);
void handle_cursor(chan_info *cp, kbd_cursor_t redisplay);
void handle_cursor_with_sync(chan_info *cp, kbd_cursor_t redisplay);
void chans_field(fcp_t field, Float val);
void in_set_transform_graph_type(graph_type_t val);
void in_set_fft_window(mus_fft_window_t val);
void set_max_transform_peaks(int val);
void combine_sound(snd_info *sp);
void separate_sound(snd_info *sp);
void superimpose_sound(snd_info *sp);
void set_sound_channel_style(snd_info *sp, channel_style_t val);
void set_chan_fft_in_progress(chan_info *cp, Cessator fp);
void stop_fft_in_progress(chan_info *cp);
void goto_graph(chan_info *cp);
void start_amp_env(chan_info *cp);
void stop_amp_env(chan_info *cp);
bool chan_fft_in_progress(chan_info *cp);
void force_fft_clear(chan_info *cp);
void chan_info_cleanup(chan_info *cp);
void update_graph(chan_info *cp);
void update_graph_or_warn(chan_info *cp);
void add_channel_data(char *filename, chan_info *cp, channel_graph_t graphed);
void add_channel_data_1(chan_info *cp, int srate, off_t frames, channel_graph_t graphed);
void set_x_bounds(axis_info *ap);
void set_show_axes(show_axes_t val);
void display_channel_data(chan_info *cp);
void display_channel_fft_data(chan_info *cp);
void show_cursor_info(chan_info *cp);
void apply_x_axis_change(axis_info *ap, chan_info *cp);
void apply_y_axis_change(axis_info *ap, chan_info *cp);
void sx_incremented(chan_info *cp, double amount);
int move_axis(chan_info *cp, axis_info *ap, int x);
void set_axes(chan_info *cp, double x0, double x1, Float y0, Float y1);
void focus_x_axis_change(axis_info *ap, chan_info *cp, int focus_style);
bool key_press_callback(chan_info *ur_cp, int x, int y, int key_state, int keysym);
void graph_button_press_callback(chan_info *cp, int x, int y, int key_state, int button, Tempus time);
void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button);
void graph_button_motion_callback(chan_info *cp, int x, int y, Tempus time);
void channel_resize(chan_info *cp);
void edit_history_select(chan_info *cp, int row);
int make_graph(chan_info *cp);
int make_background_graph(chan_info *cp, int srate, bool *two_sided);
void reset_spectro(void);
void cursor_moveto(chan_info *cp, off_t samp);
chan_info *which_channel(snd_info *sp, int y);

void g_init_chn(void);
XEN make_graph_data(chan_info *cp, int edit_pos, off_t losamp, off_t hisamp);
void draw_graph_data(chan_info *cp, off_t losamp, off_t hisamp, int data_size, Float *data, Float *data1, axis_context *ax, graph_style_t style);

void fftb(chan_info *cp, bool on);
void waveb(chan_info *cp, bool on);
void f_button_callback(chan_info *cp, bool on, bool with_control);
void w_button_callback(chan_info *cp, bool on, bool with_control);
axis_context *set_context(chan_info *cp, chan_gc_t gc);
axis_context *copy_context(chan_info *cp);
axis_context *erase_context(chan_info *cp);
axis_context *selection_context(chan_info *cp);
axis_context *mark_context(chan_info *cp);
axis_context *mix_waveform_context(chan_info *cp);
void calculate_fft(chan_info *cp);
void set_min_db(Float db);
void set_x_axis_style(x_axis_style_t val);
void set_verbose_cursor(bool val);
void set_graph_style(graph_style_t val);
void set_show_marks(bool val);
void set_show_y_zero(bool val);



/* -------- snd-axis.c -------- */

axis_info *free_axis_info(axis_info *ap);
axis_context *free_axis_context(axis_context *ax);
char *x_axis_location_to_string(chan_info *cp, double loc);
Locus grf_x(double val, axis_info *ap);
Locus grf_y(Float val, axis_info *ap);
void init_axis_scales(axis_info *ap);
void make_axes_1(axis_info *ap, x_axis_style_t x_style, int srate, show_axes_t axes, printing_t printing, 
		 with_x_axis_t show_x_axis, with_grid_t grid, log_axis_t log_axes, Float grid_scale);

#define ungrf_x(AP, X) (((X) - (AP)->x_base) / (AP)->x_scale)
#define ungrf_y(AP, Y) (((Y) - (AP)->y_base) / (AP)->y_scale)

axis_info *make_axis_info(chan_info *cp, double xmin, double xmax, Float ymin, Float ymax, 
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

snd_info *get_sp(XEN snd_n, sp_sound_t accept_player);
env_info *free_amp_env(chan_info *cp, int pos);
void free_env_state(chan_info *cp);
env_info *free_env_info(env_info *ep);
void start_env_state(chan_info *cp);
env_info *make_mix_input_amp_env(chan_info *cp);
Cessate get_amp_env(Indicium ptr);
void finish_amp_env(chan_info *cp);
bool amp_env_maxamp_ok(chan_info *cp, int edpos);
Float amp_env_maxamp(chan_info *cp, int edpos);
bool amp_env_usable(chan_info *cp, Float samples_per_pixel, off_t hisamp, bool start_new, int edit_pos, bool finish_env);
int amp_env_graph(chan_info *cp, axis_info *ap, Float samples_per_pixel, int srate);
char *shortname(snd_info *sp);
char *shortname_indexed(snd_info *sp);
void add_sound_data(char *filename, snd_info *sp, channel_graph_t graphed);
Float speed_changed(Float ival, char *srcbuf, speed_style_t style, int tones, int srcbuf_size);
void sp_name_click(snd_info *sp);
void free_controls(snd_info *sp);
void save_controls(snd_info *sp);
void restore_controls(snd_info *sp);
void reset_controls(snd_info *sp);
void stop_applying(snd_info *sp);
void menu_apply_controls(snd_info *sp);
void menu_reset_controls(snd_info *sp);
env_info *env_on_env(env *e, chan_info *cp);
void amp_env_env(chan_info *cp, Float *brkpts, int npts, int pos, Float base, Float scaler, Float offset);
void amp_env_env_selection_by(chan_info *cp, mus_any *e, off_t beg, off_t num, int pos);
void amp_env_ptree(chan_info *cp, struct ptree *pt, int pos, XEN init_func);
void amp_env_ptree_selection(chan_info *cp, struct ptree *pt, off_t beg, off_t num, int pos, XEN init_func);
void amp_env_insert_zeros(chan_info *cp, off_t beg, off_t num, int pos);
snd_info *snd_new_file(const char *newname, int header_type, int data_format, int srate, int chans, const char *new_comment, off_t samples);
#if XEN_HAVE_RATIOS
  void snd_rationalize(Float a, int *num, int *den);
#endif
int add_sp_watcher(snd_info *sp, sp_watcher_t type, void (*watcher)(struct snd_info *sp, sp_watcher_reason_t reason, int list_loc), void *context);
void remove_sp_watcher(snd_info *sp, int loc);
void call_sp_watchers(snd_info *sp, sp_watcher_t type, sp_watcher_reason_t reason);

#if MUS_DEBUGGING
void clear_listener_strings(void);
#endif

void g_init_snd(void);
XEN snd_no_such_sound_error(const char *caller, XEN n);

void amp_env_scale_by(chan_info *cp, Float scl, int pos);
void amp_env_scale_selection_by(chan_info *cp, Float scl, off_t beg, off_t num, int pos);
env_info *amp_env_copy(chan_info *cp, bool reversed, int edpos);
env_info *amp_env_section(chan_info *cp, off_t beg, off_t num, int edpos);
void pick_one_bin(env_info *ep, int bin, off_t cursamp, chan_info *cp, int edpos);
void remember_mini_string(snd_info *sp, const char *str);
void restore_mini_string(snd_info *s, bool back);
void clear_mini_strings(snd_info *sp);
void remember_filter_string(snd_info *sp, const char *str);
void restore_filter_string(snd_info *s, bool back);
void clear_filter_strings(snd_info *sp);
void remember_listener_string(const char *str);
void restore_listener_string(bool back);
void set_channel_style(channel_style_t val);


/* -------- snd-file -------- */

void *free_axes_data(void *sa);
void *make_axes_data(snd_info *sp);
void restore_axes_data(snd_info *sp, void *sa, Float new_duration, bool need_edit_history_update);
off_t disk_kspace(const char *filename);
time_t file_write_date(const char *filename);
bool link_p(const char *filename);
bool directory_p(const char *filename);
file_info *make_file_info(const char *fullname, bool read_only, bool selected);
file_info *free_file_info(file_info *hdr);
file_info *copy_header(const char *fullname, file_info *ohdr);
file_info *make_temp_header(const char *fullname, int srate, int chans, off_t samples, const char *caller);
bool sound_file_p(const char *name);
void init_sound_file_extensions(void);
void save_added_sound_file_extensions(FILE *fd);
snd_info *snd_open_file(const char *filename, bool read_only);
void snd_close_file(snd_info *sp);
snd_info *make_sound_readable(const char *filename, bool post_close);
snd_info *snd_update(snd_info *sp);
snd_info *snd_update_within_xen(snd_info *sp, const char *caller);
int snd_decode(int type, const char *input_filename, const char *output_filename);
void set_fallback_srate(int sr);
void set_fallback_chans(int ch);
void set_fallback_format(int fr);

void run_after_save_as_hook(snd_info *sp, const char *already_saved_as_name, bool from_save_as_dialog);
bool run_before_save_as_hook(snd_info *sp, const char *save_as_filename, bool selection, int srate, int type, int format, const char *comment);
void during_open(int fd, const char *file, open_reason_t reason);
void after_open(int index);
char *output_name(const char *current_name);
void save_view_files_dialogs(FILE *fd);
widget_t start_view_files_dialog(bool managed, bool make_new);
void view_files_unplay(void);
void view_files_add_directory(widget_t dialog, const char *dirname);
char *view_files_find_any_directory(void);

void g_init_file(void);
void initialize_format_lists(void);



/* -------- snd-utils -------- */

int snd_round(double x);
off_t snd_round_off_t(double x);
off_t snd_abs_off_t(off_t val);
int snd_int_pow2(int n);
int snd_to_int_pow2(int n);
int snd_int_log2(int n);
bool snd_feq(Float val1, Float val2);

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define in_dB(Min_Db, Lin_Db, Val)  ({ Float _snd_1_h_1 = Val; (_snd_1_h_1 <= Lin_Db) ? Min_Db : (20.0 * log10(_snd_1_h_1)); })
#else
  Float in_dB(Float min_dB, Float lin_dB, Float val);
#endif

#if MUS_DEBUGGING
#define copy_string(Str) copy_string_1(Str, __FUNCTION__, __FILE__, __LINE__)
char *copy_string_1(const char *str, const char *func, const char *file, int line);
#else
char *copy_string(const char *str);
#endif

int snd_strlen(const char *str);
char *snd_strcat(char *errmsg, const char *str, int *err_size);
char *snd_local_time(void);
char *snd_io_strerror(void);
char *snd_open_strerror(void);
char *string_to_colon(char *val);
char *filename_without_directory(const char *name);
char *just_filename(char *name);
char *just_directory(const char *name);
bool directory_exists(char *name);
char *file_to_string(const char *filename);

#ifdef __GNUC__
  char *vstr(const char *format, va_list ap)  __attribute__ ((format (printf, 1, 0)));
  char *snd_strftime(const char *format, time_t date) __attribute__ ((format (strftime, 1, 0)));
#else
  char *vstr(const char *format, va_list ap);
  char *snd_strftime(const char *format, time_t date);
#endif

disk_space_t disk_space_p(off_t bytes, const char *filename);
const char *short_data_format_name(int sndlib_format, const char *filename);
char *prettyf(double num, int tens);
char *shorter_tempnam(const char *dir, const char *prefix);
char *snd_tempnam(void);
void snd_exit(int val);
void g_init_utils(void);

fam_info *fam_monitor_file(const char *filename, 
			   void *data, 
			   void (*action)(struct fam_info *fp, FAMEvent *fe));
fam_info *fam_monitor_directory(const char *dir_name,
				void *data, 
				void (*action)(struct fam_info *fp, FAMEvent *fe));
fam_info *fam_unmonitor_file(const char *filename, fam_info *fp);
fam_info *fam_unmonitor_directory(const char *filename, fam_info *fp);
char *fam_event_name(int code);

#if MUS_DEBUGGING
void set_printable(int val);
void check_pointer(void *ptr);
#endif

#if MUS_DEBUGGING && HAVE_CLOCK
  void start_timing(void);
  void stop_timing(void);
#endif



/* -------- snd-listener -------- */

void backup_listener_to_previous_command(void);
void command_return(widget_t w, int last_prompt);
bool is_prompt(const char *str, int beg);
bool within_prompt(const char *str, int beg, int end);
char *listener_prompt_with_cr(void);
void set_listener_prompt(const char *new_prompt);
int check_balance(char *expr, int start, int end, bool in_listener);
int find_matching_paren(char *str, int parens, int pos, int *highlight_pos);
void provide_listener_help(char *source);
bool listener_is_visible(void);
void g_init_listener(void);



/* -------- snd-mix.c -------- */

mix_context *make_mix_context(chan_info *cp);
mix_context *free_mix_context(mix_context *ms);
void free_mix_list(chan_info *cp);
void free_mixes(chan_info *cp);
int mix_complete_file(snd_info *sp, off_t beg, const char *fullname, bool with_tag, file_delete_t auto_delete, int track_id, bool all_chans);
int mix_complete_file_at_cursor(snd_info *sp, const char *str, bool with_tag, int track_id);
int mix_file(off_t beg, off_t num, int chans, chan_info **cps, const char *mixinfile, file_delete_t temp, const char *origin, bool with_tag, int track_id);
void backup_mix_list(chan_info *cp, int edit_ctr);
bool active_mix_p(chan_info *cp);
off_t mix_beg(chan_info *cp);
void reset_mix_graph_parent(chan_info *cp);
void display_channel_mixes(chan_info *cp);
void lock_affected_mixes(chan_info *cp, off_t beg, off_t end);
void release_pending_mixes(chan_info *cp, int edit_ctr);
void reset_mix_list(chan_info *cp);
void sync_mixes_with_edits(chan_info *cp);
void ripple_mixes(chan_info *cp, off_t beg, off_t change);
void goto_mix(chan_info *cp, int count);
off_t mix_frames(int n);
int any_mix_id(void);
bool mix_ok_and_unlocked(int n);
int set_mix_amp_env(int n, int chan, env *val);
void g_init_mix(void);
void clear_mix_tags(chan_info *cp);
void clear_mix_y(chan_info *cp);
void color_mixes(color_t color);
void move_mix_tag(int mix_tag, int x);
void finish_moving_mix_tag(int mix_tag, int x);
int hit_mix(chan_info *cp, int x, int y);
int prepare_mix_id_waveform(int mix_id, axis_info *ap, bool *two_sided);
chan_info *mix_dialog_mix_channel(int mix_id);
void mix_dialog_set_mix_amp_env_without_edit(int n, int chan, env *val);
void mix_dialog_mix_play(int mix_id);
void mix_dialog_track_play(int mix_id);
void mix_dialog_start_drag(int mix_id);
void mix_dialog_set_mix_speed(int mix_id, Float val, bool dragging);
void mix_dialog_set_mix_amp(int mix_id, int chan, Float val, bool dragging);
void mix_dialog_set_mix_track(int mix_id, int track);
int mix_dialog_mix_track(int mix_id);
Float mix_dialog_mix_speed(int mix_id);
Float mix_dialog_mix_amp(int mix_id, int chan);
off_t mix_dialog_mix_position(int mix_id);
env *mix_dialog_mix_amp_env(int n, int chan);
int mix_dialog_mix_input_chans(int mix_id);
int set_mix_position(int mix_id, off_t beg);
bool mix_ok(int n);
env **mix_dialog_envs(int n);
env *mix_dialog_env(int n, int chan);
void mix_at_x_y(int data, const char *filename, int x, int y);
int next_mix_id(int id);
int previous_mix_id(int id);
void reflect_edit_in_mix_dialog_envs(int n);
void g_init_track(void);
bool track_p(int trk);
void free_track_info_list(chan_info *cp);
track_info *free_track_info(chan_info *cp, int loc);
void record_initial_track_info(chan_info *cp);
off_t track_position(int id, int chan);
off_t track_frames(int id, int chan);
int track_chans(int id);
chan_info *track_channel(int id, int chn);
env *track_dialog_track_amp_env(int id);
Float track_dialog_track_amp(int id);
Float track_dialog_track_speed(int id);
Float track_dialog_track_tempo(int id);
int track_dialog_track_track(int id);
env *track_dialog_env(int n);
void track_dialog_play(int track_id);
void track_dialog_set_amp(int track_id, Float val);
void track_dialog_start_slider_drag(int id);
void track_dialog_set_speed(int id, Float val);
void track_dialog_set_tempo(int id, Float val, bool dragging);
void track_dialog_set_amp_env(int id, env *e);
bool track_dialog_track_color_set(int id);
color_t track_dialog_track_color(int id);
void reflect_edit_in_track_dialog_env(int n);
bool set_track_track(int id, int trk);
void set_track_position(int id, off_t pos);
int any_track_id(void);
int next_track_id(int id);
int previous_track_id(int id);
char *track_dialog_track_info(int id);
void release_pending_track_states(void);
void display_track_waveform(int track_id, axis_info *ap);
speed_style_t mix_speed_style(int id);
speed_style_t set_mix_speed_style(int id, speed_style_t choice, bool from_gui);
speed_style_t track_speed_style(int id);
speed_style_t set_track_speed_style(int id, speed_style_t choice, bool from_gui);

struct mix_fd;
struct track_fd;
Float mix_read_sample_to_float(struct mix_fd *ptr);
bool mf_p(XEN obj);
struct mix_fd *get_mf(XEN obj);
char *run_mix_sample_reader_to_string(struct mix_fd *mf);
void run_free_mix_fd(struct mix_fd *ptr);
struct mix_fd *run_make_mix_sample_reader(int id, off_t beg);
bool mix_sample_reader_at_end_p(struct mix_fd *mf);
Float track_read_sample_to_float(struct track_fd *ptr);
bool tf_p(XEN obj);
struct track_fd *get_tf(XEN obj);
char *run_track_sample_reader_to_string(struct track_fd *ptr);
void run_free_track_sample_reader(struct track_fd *ptr);
struct track_fd *run_make_track_sample_reader(int id, int chan, off_t beg);
bool track_sample_reader_at_end_p(struct track_fd *tf);
bool mix_dialog_mix_inverted(int id);
void mix_dialog_set_mix_inverted(int id, bool on);
char *edit_list_mix_and_track_init(chan_info *cp);

XEN g_copy_mix_sample_reader(XEN obj);
XEN g_mix_sample_reader_home(XEN obj);
XEN g_mix_sample_reader_at_end_p(XEN obj);
XEN g_mix_sample_reader_position(XEN obj);
XEN g_free_mix_sample_reader(XEN obj);
XEN g_copy_track_sample_reader(XEN obj);
XEN g_track_sample_reader_home(XEN obj);
XEN g_track_sample_reader_at_end_p(XEN obj);
XEN g_track_sample_reader_position(XEN obj);
XEN g_free_track_sample_reader(XEN obj);

char *track_name(int trk);
char *mix_name(int id);
int track_name_to_id(const char *name);
int mix_name_to_id(const char *name);



/* -------- snd-find.c -------- */

char *global_search(read_direction_t direction);
void cursor_search(chan_info *cp, int count);
void clear_sound_search_procedure(snd_info *sp, bool clear_expr_too);
void clear_global_search_procedure(bool clear_expr_too);

void g_init_find(void);



/* -------- snd-trans.c -------- */

int snd_translate(const char *oldname, const char *newname, int type);


/* -------- snd-rec.c -------- */

bool record_in_progress(void);
void save_recorder_state(FILE *fd);
void g_init_recorder(void);
bool rec_autoload(void);
bool rec_set_autoload(bool val);
int rec_buffer_size(void);
int rec_set_buffer_size(int size);
char *rec_filename(void);
void rec_set_filename(const char *filename);
int rec_output_chans(void);
void rec_set_output_chans(int chans);
int rec_output_data_format(void);
void rec_set_output_data_format(int f);
int rec_output_header_type(void);
void rec_set_output_header_type(int h);
int rec_srate(void);
int rec_set_srate(int s);


/* -------- snd.c -------- */

void mus_error_to_snd(int type, char *msg);
void snd_set_global_defaults(bool need_cleanup);
#if SND_AS_WIDGET
  snd_state *snd_main(int argc, char **argv);
#endif
#ifdef SND_AS_PD_EXTERNAL
  int snd_pd_main(void);
#endif
void g_init_base(void);


/* -------- snd-kbd.c -------- */

void save_macro_state(FILE *fd);

#ifdef __GNUC__
  void report_in_minibuffer(snd_info *sp, const char *format, ...)  __attribute__ ((format (printf, 2, 3)));
#else
  void report_in_minibuffer(snd_info *sp, const char *format, ...);
#endif
void string_to_minibuffer(snd_info *sp, const char *buf);
void errors_to_minibuffer(const char *msg, void *data);
void printout_to_minibuffer(const char *msg, void *data);
void clear_minibuffer(snd_info *sp);
void clear_minibuffer_prompt(snd_info *sp);
void snd_minibuffer_activate(snd_info *sp, int keysym, bool with_meta);
int in_user_keymap(int key, int state, bool cx_extended);
void set_keymap_entry(int key, int state, int args, XEN func, bool cx_extended, const char *origin, const char *prefs_info);
char *key_binding_description(int key, int state, bool cx_extended);
char *make_key_name(char *buf, int buf_size, int key, int state, bool extended);
void map_over_key_bindings(bool (*func)(int key, int state, bool cx, char *pinfo, XEN xf));
key_info *find_prefs_key_binding(const char *prefs_name);
void keyboard_command(chan_info *cp, int keysym, int state);
void control_g(snd_info *sp);
void g_init_kbd(void);


/* -------- snd-sig.c -------- */

void scale_by(chan_info *cp, Float *scalers, int len, bool selection);
bool scale_to(snd_info *sp, chan_info *cp, Float *scalers, int len, bool selection);
Float channel_maxamp(chan_info *cp, int edpos);
off_t channel_maxamp_position(chan_info *cp, int edpos);
src_state *make_src(Float srate, snd_fd *sf, Float initial_srate);
Float src_input_as_needed(void *arg, int dir);
src_state *free_src(src_state *sr);
void src_env_or_num(chan_info *cp, env *e, Float ratio, bool just_num, 
		    enved_progress_t from_enved, const char *origin, bool over_selection, mus_any *gen, XEN edpos, int arg_pos);
void apply_filter(chan_info *ncp, int order, env *e, enved_progress_t from_enved, const char *caller, const char *origin, 
		  bool over_selection, Float *ur_a, mus_any *gen, XEN edpos, int arg_pos, bool truncate);
void apply_env(chan_info *cp, env *e, off_t beg, off_t dur, bool over_selection, 
	       enved_progress_t from_enved, const char *origin, mus_any *gen, XEN edpos, int arg_pos);
void cos_smooth(chan_info *cp, off_t beg, off_t num, bool over_selection);
void display_frequency_response(env *e, axis_info *ap, axis_context *gax, int order, bool dBing);
void cursor_delete(chan_info *cp, off_t count);
void cursor_zeros(chan_info *cp, off_t count, bool over_selection);
void cursor_insert(chan_info *cp, off_t beg, off_t count);

void g_init_sig(void);
int to_c_edit_position(chan_info *cp, XEN edpos, const char *caller, int arg_pos);
off_t to_c_edit_samples(chan_info *cp, XEN edpos, const char *caller, int arg_pos);
off_t beg_to_sample(XEN beg, const char *caller);
off_t dur_to_samples(XEN dur, off_t beg, chan_info *cp, int edpos, int argn, const char *caller);
char *scale_and_src(char **files, int len, int max_chans, Float amp, Float speed, env *amp_env, bool *err);


/* -------- snd-run.c -------- */

struct ptree *form_to_ptree_1_b(XEN code);
struct ptree *form_to_ptree_1_b_without_env(XEN code);
struct ptree *form_to_ptree_1_f(XEN code);
Float evaluate_ptree_0f2f(struct ptree *pt);
struct ptree *form_to_ptree_0_f(XEN code);
Float evaluate_ptree_1f2f(struct ptree *pt, Float arg);
int evaluate_ptree_1f2b(struct ptree *pt, Float arg);
void free_ptree(struct ptree *pt);
void g_init_run(void);
XEN ptree_code(struct ptree *pt);
Float evaluate_ptree_1f1v1b2f(struct ptree *pt, Float arg, vct *v, bool dir);
struct ptree *form_to_ptree_3_f(XEN code);
Float evaluate_ptreec(struct ptree *pt, Float arg, vct *v, bool dir);


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

