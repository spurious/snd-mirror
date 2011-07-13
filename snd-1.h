#ifndef SND_1_H
#define SND_1_H

#define ASSERT_SOUND(Origin, Snd, Offset) \
  if (!((XEN_INTEGER_P(Snd)) || (XEN_SOUND_P(Snd)) || (XEN_FALSE_P(Snd)) || (XEN_NOT_BOUND_P(Snd)))) \
    XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset, Snd, "a sound object, an integer (sound index), or " PROC_FALSE);

#define ASSERT_CHANNEL(Origin, Snd, Chn, Offset) \
  if (!((XEN_INTEGER_P(Snd)) || (XEN_SOUND_P(Snd)) || (XEN_FALSE_P(Snd)) || (XEN_NOT_BOUND_P(Snd)))) \
    XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset, Snd, "a sound object, an integer (sound index), or " PROC_FALSE); \
  else \
    if (!((XEN_INTEGER_P(Chn)) || (XEN_FALSE_P(Chn)) || (XEN_NOT_BOUND_P(Chn)))) \
      XEN_WRONG_TYPE_ARG_ERROR(Origin, Offset + 1, Chn, "an integer (0-based channel number) or " PROC_FALSE);

/* these macros fix up argument order for setter procs in Scheme: (set! (proc a b) c) */
/*    snd-edits has a 5 and a 10 case */
#if HAVE_SCHEME

#define WITH_TWO_SETTER_ARGS(name_reversed, name)	   \
  static s7_pointer name_reversed(s7_scheme *sc, s7_pointer args)   \
  {                                                        \
    if (XEN_NULL_P(XEN_CDR(args)))		   \
      return(name(XEN_CAR(args), XEN_UNDEFINED));	   \
    return(name(XEN_CADR(args), XEN_CAR(args)));	   \
  }

#define WITH_THREE_SETTER_ARGS(name_reversed, name)                      \
  static s7_pointer name_reversed(s7_scheme *sc, s7_pointer args)                 \
  {							                 \
    if (XEN_NULL_P(XEN_CDR(args)))		                 \
      return(name(XEN_CAR(args), XEN_UNDEFINED, XEN_UNDEFINED));         \
    else {					 		         \
      if (XEN_NULL_P(XEN_CDDR(args)))				 \
	return(name(XEN_CADR(args), XEN_CAR(args), XEN_UNDEFINED));	\
      else return(name(XEN_CADDR(args), XEN_CAR(args), XEN_CADR(args))); \
  }}

#define WITH_FOUR_SETTER_ARGS(name_reversed, name)                                         \
  static s7_pointer name_reversed(s7_scheme *sc, s7_pointer args)                                   \
{							                                   \
  if (XEN_NULL_P(XEN_CDR(args)))					                   \
    return(name(XEN_CAR(args), XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));              \
  else {								                   \
    if (XEN_NULL_P(XEN_CDDR(args)))				                   \
      return(name(XEN_CADR(args), XEN_CAR(args), XEN_UNDEFINED, XEN_UNDEFINED));           \
    else {								                   \
      if (XEN_NULL_P(XEN_CDDDR(args)))				                   \
	return(name(XEN_CADDR(args), XEN_CAR(args), XEN_CADR(args), XEN_UNDEFINED));       \
      else return(name(XEN_CADDDR(args), XEN_CAR(args), XEN_CADR(args), XEN_CADDR(args))); \
  }}}

#else

/* 10 case in snd-edits for set-samples and a 5 case for set-sample */
#define WITH_TWO_SETTER_ARGS(name_reversed, name)
#define WITH_THREE_SETTER_ARGS(name_reversed, name)
#define WITH_FOUR_SETTER_ARGS(name_reversed, name)
#endif

#define ASSERT_SAMPLE_TYPE(Origin, Beg, Offset) \
  XEN_ASSERT_TYPE(XEN_NUMBER_P(Beg) || XEN_FALSE_P(Beg) || XEN_NOT_BOUND_P(Beg), Beg, Offset, Origin, "a number or " PROC_FALSE)

typedef struct {
  char **values;
  int num_values, values_size;
  bool exact_match;
} list_completer_info;

typedef struct {
  int samps_per_bin, peak_env_size;
  mus_float_t fmax, fmin;
  mus_float_t *data_max, *data_min;
  bool completed;
  int bin, top_bin;
} peak_env_info;

typedef struct snd_io snd_io;

typedef struct {
  char *name;             /* full name */
  mus_long_t samples;          /* total samples = chans * frames */
  mus_long_t data_location;    /* bytes */
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
  mus_long_t data_bytes;        /* used only for edit-history descriptions (snd-edits.c display_ed_list) */
  bool free_me;
} snd_data;

typedef struct mark mark;
typedef struct enved_fft enved_fft;

typedef struct {
  int size, allocated_size;
  struct ed_fragment **fragments;
  mus_long_t beg, len;                      /* beg and len of changed portion */
  char *origin;
  edit_t edit_type;
  int sound_location, ptree_location;
  mus_long_t selection_beg, selection_end;  /* selection needs to follow edit list */
  mus_float_t maxamp, selection_maxamp;
  mus_long_t maxamp_position, selection_maxamp_position;
  int edpos;
  bool ptree_env_too, backed_up;
  mus_long_t samples, cursor;
  int mark_size, mark_ctr;
  mark **marks;                        /* mark positions */
  peak_env_info *peak_env; 
  enved_fft *fft;                      /* overall fft data for envelope editor */
  void *readers;                       /* current readers of this edit (g++ stupidity forces us to use void* here -- type is sf_info, snd-edits.c) */
  void *mixes;
  XEN properties;
  int properties_gc_loc;
} ed_list;

typedef struct snd_fd {
  mus_float_t (*runf)(struct snd_fd *sf);
  mus_float_t (*rev_runf)(struct snd_fd *sf);
  ed_list *current_state;
  struct ed_fragment *cb;
  mus_long_t loc, first, last;
  int cbi;
  read_direction_t direction;
  bool at_eof, freed;
  mus_sample_t *data;
  snd_data *current_sound;
  mus_long_t initial_samp;
  struct chan_info *cp;
  struct snd_info *local_sp;
  mus_float_t fscaler;
  mus_long_t frag_pos;
  int edit_ctr, region;
  reader_t type;
  void *ptrees, *ramps, *mixes;
} snd_fd;

typedef struct {mus_float_t freq; mus_float_t amp;} fft_peak;

typedef struct {
  mus_float_t y0, y1;                         /* scroller-dependent axis bounds */
  double x0, x1;
  double xmin, xmax;
  mus_float_t ymin, ymax;                     /* data-dependent absolute limits */
  mus_float_t y_scale, y_base, y_ambit;
  double x_scale, x_base, x_ambit;
  char *xlabel, *ylabel, *default_xlabel;
  int y_axis_x0, x_axis_x0, y_axis_y0, x_axis_y0, x_axis_x1, y_axis_y1, x_label_x, x_label_y;
  bool graph_active;
  mus_long_t losamp, hisamp;                 /* displayed x-axis bounds in terms of sound sample numbers */
  int graph_x0;                         /* x axis offset relative to window (for double graphs) */
  struct tick_descriptor *x_ticks, *y_ticks; 
  graphics_context *ax;
  int width, height;
  struct chan_info *cp;
  mus_float_t sy, zy;                         /* as set by user, 0.0 - 1.0 */
  double sx, zx;
  int y_offset;
  int window_width;
  bool no_data, changed;
#if HAVE_GL
  bool use_gl, used_gl;
#endif
} axis_info;

typedef struct {
  mus_float_t *data;
  int pts, data_size; /* data_size is independent of actual number of points of data (can be much larger) */
  mus_float_t base;
} env;

typedef struct {
  int mix_id;
  mus_long_t beg, len;
  mus_float_t scaler, speed;
  env *amp_env;
  int index;           /* cp->sounds index (src writes a new temp file) */
} mix_state;

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
  oclock_t down_time;
  bool env_dragged;
  int env_pos;
  bool click_to_delete, in_dB, with_dots, clip_p;
  bool edited;
} env_editor;

typedef struct {
  mus_long_t size;
  mus_long_t current_size;
  mus_fft_window_t window;
  mus_float_t alpha, beta;
  mus_float_t scale;
  axis_info *axis;
  mus_float_t *data, *phases;
  char *xlabel;
  struct chan_info *chan;
} fft_info;

typedef struct {
  int total_slices;        /* size of the data array (max for allocation checks) */
  int total_bins;          /* size other axis data array */
  int active_slices;       /* how many slices contain current data */
  int target_bins;         /* this many bins Y-side */
  int target_slices;       /* how many slices in full display (current) */
  mus_float_t **data;            /* data[total_slices][bins] -> each is a spectral magnitude */
  mus_long_t *begs;             /* associated begin times (for data reuse) */
  struct chan_info *cp;
  mus_float_t scale;
} sono_info;

typedef struct {
  int chans, fields;
  double *axis_data;
  bool *fftp, *wavep;
} axes_data;

typedef struct chan_info {
  int chan;                /* which chan are we */
  bool graph_transform_p;  /* f button state */
  bool graph_time_p;       /* w button state */
  bool graph_lisp_p;       /* is lisp graph active */
  struct lisp_grf *lisp_info; /* defined in snd-chn.c */
  bool cursor_on;          /* channel's cursor */
  bool cursor_visible, fft_cursor_visible;     /* for XOR decisions */
  int cursor_size;
  cursor_style_t cursor_style, tracking_cursor_style;
  int cx, cy, fft_cx, old_cy; /* graph-relative cursor loc (for XOR in Motif, erase-via-overwrite in cairo) */
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
  int *init_locs, *init_args;
  fft_info *fft;           /* possibly null fft data */
  struct snd_info *sound;  /* containing sound */
  axis_info *axis;         /* time domain axis */

  idle_t fft_in_progress;
  idle_t peak_env_in_progress;
  struct env_state *peak_env_state;
  graphics_context *ax;
  bool selected;
  mus_float_t progress_pct;

#if USE_GTK
  GtkWidget **chan_widgets;
  GtkAdjustment **chan_adjs;
  GdkCursor *current_cursor;
  slist *edhist_list;
  color_info *combined_data_color;
#endif
#if USE_MOTIF
  Widget *chan_widgets;
  Pixmap fft_pix;
  unsigned int fft_pix_width, fft_pix_height;
  int fft_pix_x0, fft_pix_y0;
  bool fft_pix_ready;
  mus_float_t fft_pix_cutoff;
  Cursor current_cursor;
  Pixel combined_data_color;
#endif
#if USE_NO_GUI
  int current_cursor, combined_data_color;
#endif

  sono_info *sonogram_data;
  struct sonogram_state *last_sonogram, *temp_sonogram; /* defined in snd-fft.c */
  struct wavogram_state *last_wavogram;                 /* defined in snd-chn.c */
  bool show_sonogram_cursor;
  struct fft_state *fft_data;          /* parallels sonogram -- try to avoid repeating large ffts needlessly */
  printing_t printing;
  fft_change_t fft_changed;
  mus_float_t gsy, gzy;
  int height;
  mus_long_t original_cursor, original_left_sample, original_window_size;   /* for cursor reset after cursor-moving play */
  with_hook_t hookable;
  mus_long_t selection_transform_size;
  bool squelch_update, previous_squelch_update, waiting_to_make_graph;
  int in_as_one_edit, as_one_edit_positions_size;
  int *as_one_edit_positions;
  /* moved from global to channel-local 4-Aug-00 */
  mus_float_t spectro_x_scale, spectro_y_scale, spectro_z_scale, spectro_z_angle, spectro_x_angle, spectro_y_angle;
  mus_float_t spectrum_end, spectrum_start;
  mus_float_t lin_dB, min_dB, fft_window_alpha, fft_window_beta, beats_per_minute, grid_density;
  bool show_y_zero, show_marks, verbose_cursor;
  with_grid_t show_grid;
  int wavo_hop, wavo_trace, zero_pad, wavelet_type, max_transform_peaks, beats_per_measure;
  x_axis_style_t x_axis_style;
  show_axes_t show_axes;
  mus_long_t transform_size;
  mus_fft_window_t fft_window;
  graph_type_t transform_graph_type, time_graph_type;
  bool show_transform_peaks, fft_log_frequency, fft_log_magnitude, fft_with_phases;
  graph_style_t time_graph_style, lisp_graph_style, transform_graph_style;
  int dot_size;
  fft_normalize_t transform_normalization;
  int transform_type, spectro_hop, edhist_base;
  bool show_mix_waveforms, graphs_horizontal;
  XEN edit_hook;
  XEN undo_hook;
  XEN cursor_proc;
  XEN after_edit_hook;
  XEN properties;
  int cursor_proc_loc, edit_hook_loc, undo_hook_loc, after_edit_hook_loc, properties_loc;
  bool selection_visible;
  channel_state_t active;
  int old_x0, old_x1;
  mus_float_t *amp_control; /* local amp controls in snd-dac; should it be extended to other controls? */
  search_result_t last_search_result;
  bool just_zero, new_peaks, editable, updating;
  struct inset_graph_info_t *inset_graph; /* defined in snd-chn.c */
#if HAVE_GL
  int gl_fft_list, gl_wavo_list;
#endif
} chan_info;

#define CURRENT_SAMPLES(Cp) (Cp)->edits[(Cp)->edit_ctr]->samples
#define CURSOR(Cp) (Cp)->edits[(Cp)->edit_ctr]->cursor

typedef struct snd_info {
  sound_inuse_t inuse;
  int index;
  int playing;
  int sync, previous_sync;
  bool expand_control_p;
  bool contrast_control_p;
  bool reverb_control_p;
  bool filter_control_p, filter_control_in_dB, filter_control_in_hz;
  mus_float_t amp_control;
  mus_float_t speed_control;
  int speed_control_direction, speed_control_tones, speed_control_numerator, speed_control_denominator;
  speed_style_t speed_control_style;
  mus_float_t last_speed_control, last_amp_control, last_expand_control, last_contrast_control;
  mus_float_t last_reverb_control_length, last_reverb_control_scale;
  mus_float_t saved_speed_control, saved_amp_control, saved_expand_control, saved_contrast_control;
  mus_float_t saved_reverb_control_length, saved_reverb_control_scale;
  mus_float_t expand_control, expand_control_length, expand_control_ramp, expand_control_hop, expand_control_jitter;
  mus_float_t contrast_control, contrast_control_amp;
  mus_float_t reverb_control_length, reverb_control_scale, reverb_control_feedback, reverb_control_lowpass;
  mus_float_t reverb_control_decay, filter_control_xmax;
  mus_float_t contrast_control_min, contrast_control_max, expand_control_min, expand_control_max, speed_control_min, speed_control_max;
  mus_float_t amp_control_min, amp_control_max, reverb_control_scale_min, reverb_control_scale_max;
  mus_float_t reverb_control_length_min, reverb_control_length_max;
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
  bool raw_prompt, remembering;
  char *search_expr;
  mus_long_t marking;
  int search_count, amp_count; /* search_count = number of times to search before return, amp_count = amp env samps if not 1 (= full dur) */
  sp_filing_t filing;
  char *filing_filename;
  bool prompting, loading, finding_mark, selectioning;
  printing_t printing;
  mus_long_t macro_count;
  minibuffer_choice_t minibuffer_on;
  read_only_t user_read_only, file_read_only;
  chan_info **chans;

  struct env_editor *flt;
#if USE_MOTIF
  Widget *snd_widgets;
  Widget *progress_widgets;
  int num_progress_widgets;
  Widget tab;
  Widget dialog;
  Dimension minibuffer_height;
  bool minibuffer_watcher;
#endif
#if USE_GTK
  GtkWidget **snd_widgets;
  GtkAdjustment **snd_adjs;
  GtkWidget *dialog;
  int page;
  bool mini_active;
  gulong minibuffer_watcher;
  graphics_context *name_pix_ax, *stop_pix_ax, *speed_arrow_ax, *filter_ax;
  graphics_context **clock_pix_ax;
  GtkWidget **clock_widgets;
  int num_clock_widgets;
#endif
#if USE_NO_GUI
  bool snd_widgets;
#endif

  file_info *hdr;             /* header of file that would be affected if we were to save current edits */
  int bomb_ctr;
  time_t write_date, update_warning_write_date;   /* check for change behind back while editing */
  bool need_update, file_unreadable; /* current in-core data does not match actual file (someone wrote it behind our back) */
  channel_style_t channel_style;
  int allocated_chans, selectpos; 
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
} snd_info;

#define SND_SRATE(Sp) (((Sp)->hdr)->srate)
#define HAS_WIDGETS(Sp) ((Sp) && ((Sp)->snd_widgets))

typedef struct snd_state {
  int selected_sound;         /* NO_SELECTION = none selected = which sound is currently receiving user's attention */
  int active_sounds;
  int channel_min_height;
  snd_info **sounds;
  char *search_expr, *startup_title, *startup_errors;
  struct ptree *search_tree;
  XEN search_proc;
  int file_sorters_size, file_filters_size;
  XEN file_sorters, file_filters;
  int search_proc_loc, local_errno, local_open_errno;
  int position_slider_width, zoom_slider_width, toggle_size, channel_sash_indent, sash_size, channel_sash_size, sash_indent;
  int max_sounds, sound_sync_max;
  char *translated_filename;
  print_choice_t print_choice;
  snd_apply_t apply_choice;
  bool gl_has_double_buffer;
  bool stopped_explicitly, checking_explicitly, selection_play_stop;
  int reloading_updated_file;
  int init_window_width, init_window_height;
  int init_window_x, init_window_y;
  bool graph_hook_active, lisp_graph_hook_active;
  bool Show_Transform_Peaks, Show_Y_Zero, Show_Marks;
  with_grid_t Show_Grid;
  bool Fft_Log_Frequency, Fft_Log_Magnitude, Fft_With_Phases;
  channel_style_t Channel_Style;
  sync_style_t Sync_Style;
  sound_style_t Sound_Style;
  show_axes_t Show_Axes;
  char *Eps_File, *Temp_Dir, *Save_Dir, *Ladspa_Dir, *Peak_Env_Dir;
  char *Listener_Font, *Axis_Label_Font, *Axis_Numbers_Font, *Tiny_Font, *Peaks_Font, *Bold_Peaks_Font;
  char *orig_listener_font, *orig_axis_label_font, *orig_axis_numbers_font, *orig_tiny_font, *orig_peaks_font, *orig_bold_peaks_font;
  bool Verbose_Cursor, Trap_Segfault, With_Inset_Graph, With_Pointer_Focus, With_Smpte_Label, With_Interrupts;
  int Enved_Filter_Order;
  mus_float_t Eps_Left_Margin, Eps_Bottom_Margin, Eps_Size, Log_Freq_Start;
  mus_float_t Spectro_X_Scale, Spectro_Y_Scale, Spectro_Z_Scale, Spectro_Z_Angle, Spectro_X_Angle, Spectro_Y_Angle;
  mus_float_t Spectrum_End, Spectrum_Start;
  int Default_Output_Header_Type, Default_Output_Data_Format, Default_Output_Chans, Default_Output_Srate;
  int Spectro_Hop, Color_Map, Color_Map_Size, Wavelet_Type, Transform_Type, Optimization;
  int Dot_Size;
  int Zero_Pad, Wavo_Hop, Wavo_Trace;
  mus_long_t Transform_Size;
  mus_fft_window_t Fft_Window;
  graph_type_t Transform_Graph_Type, Time_Graph_Type;
  bool Ask_Before_Overwrite, Ask_About_Unsaved_Edits, Show_Full_Duration, Show_Full_Range, Remember_Sound_State;
  bool Save_As_Dialog_Src, Save_As_Dialog_Auto_Comment, With_Toolbar, With_Tooltips, With_Menu_Icons;
  mus_float_t Fft_Window_Alpha, Fft_Window_Beta, Grid_Density, Initial_Beg, Initial_Dur;
  mus_float_t Color_Scale, Color_Cutoff, Beats_Per_Minute;
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
  mus_float_t Enved_Base, Enved_Power, Auto_Update_Interval;
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
  mus_float_t Expand_Control_Length, Expand_Control_Ramp, Expand_Control_Hop, Expand_Control_Jitter;
  mus_float_t Contrast_Control_Amp;
  mus_float_t Reverb_Control_Feedback, Reverb_Control_Lowpass;
  mus_float_t Reverb_Control_Decay, Cursor_Update_Interval;
  mus_float_t Contrast_Control_Min, Contrast_Control_Max, Expand_Control_Min, Expand_Control_Max, Speed_Control_Min, Speed_Control_Max;
  mus_float_t Amp_Control_Min, Amp_Control_Max, Reverb_Control_Scale_Min, Reverb_Control_Scale_Max;
  mus_float_t Reverb_Control_Length_Min, Reverb_Control_Length_Max;
  int Filter_Control_Order, Cursor_Location_Offset, Play_Arrow_Size;
  mus_float_t Min_dB;
  bool Show_Controls;
  tracking_cursor_t With_Tracking_Cursor;
  bool tracking;
  XEN cursor_proc;
  int cursor_proc_loc, listener_prompt_length;
  XEN zoom_focus_proc;
  int zoom_focus_proc_loc;
  mus_float_t lin_dB;
  char *HTML_Dir, *HTML_Program, *Open_File_Dialog_Directory;
  char *io_error_info;
  int deferred_regions;
  open_requestor_t open_requestor;
  void *open_requestor_data;
  bool batch_mode;
  bool jump_ok, exiting;
  env_editor *enved;
  oclock_t click_time;
  bool fam_ok, C_g_typed;
  FAMConnection *fam_connection;
  void (*snd_error_handler)(const char *error_msg, void *data);
  void *snd_error_data;
  void (*snd_warning_handler)(const char *warning_msg, void *data);
  void *snd_warning_data;
  void (*xen_error_handler)(const char *error_msg, void *data);
  void *xen_error_data;
  void (*snd_print_handler)(const char *msg, void *data);
  void *snd_print_data;
  channel_style_t update_sound_channel_style;
#if HAVE_GL && WITH_GL2PS
  bool gl_printing;
#endif
  XEN mus_error_hook;
  XEN snd_error_hook; 
  XEN snd_warning_hook; 
  XEN snd_open_file_hook;
  XEN snd_selection_hook;
  XEN effects_hook;

#if USE_MOTIF
  XtAppContext mainapp;     
  Widget mainshell;
  Widget mainpane;
  Widget soundpane;
  Widget soundpanebox;
  Widget toolbar;
  Display *mdpy;
  xm_font_t peaks_fontlist;
  XFontStruct *peaks_fontstruct;
  xm_font_t bold_peaks_fontlist;
  XFontStruct *bold_peaks_fontstruct; 
  xm_font_t listener_fontlist;
  XFontStruct *listener_fontstruct;
  XFontStruct *axis_label_fontstruct;
  XFontStruct *axis_numbers_fontstruct;
  xm_font_t tiny_fontlist;
  XFontStruct *tiny_fontstruct;

  Pixel white, black, red, yellow, green, blue, light_blue, lighter_blue;
  Pixel data_color, selected_data_color, mark_color, graph_color, selected_graph_color, listener_color, listener_text_color;
  Pixel basic_color, selection_color, zoom_color, position_color, highlight_color, enved_waveform_color, cursor_color;
  Pixel text_focus_color, filter_control_waveform_color, mix_color, sash_color;
  Pixel selected_grid_color, grid_color, axis_color;
  Pixel orig_data_color, orig_selected_data_color, orig_mark_color, orig_mix_color;
  Pixel orig_graph_color, orig_selected_graph_color, orig_listener_color, orig_listener_text_color, orig_cursor_color;
  Pixel orig_basic_color, orig_selection_color, orig_zoom_color, orig_position_color, orig_highlight_color;

  GC basic_gc, selected_basic_gc, combined_basic_gc;        
  GC cursor_gc, selected_cursor_gc;      
  GC selection_gc, selected_selection_gc;
  GC erase_gc, selected_erase_gc;        
  GC mark_gc, selected_mark_gc;          
  GC mix_gc;
  GC fltenv_basic_gc, fltenv_data_gc;
  Widget listener_pane;
  Widget *dialogs;
  int num_dialogs, dialogs_size;
  Cursor graph_cursor, wait_cursor, bounds_cursor, play_cursor, loop_play_cursor, yaxis_cursor;
  Widget requestor_dialog;
#if HAVE_GL
  GLXContext cx;
#endif
  XtInputId fam_port;
  Widget *mw;
  bool axis_color_set;
#endif

#if USE_GTK
  cairo_t *cr;
  double line_width;
  GtkWidget *mainshell;
  GtkWidget *mainpane;
  GtkWidget *soundpane;
  GtkWidget *soundpanebox;
  GtkWidget *listener_pane;
  GdkWindow *mainwindow;

  PangoFontDescription *listener_fnt;
  PangoFontDescription *axis_label_fnt;
  PangoFontDescription *axis_numbers_fnt;
  PangoFontDescription *tiny_fnt;
  PangoFontDescription *peaks_fnt;
  PangoFontDescription *bold_peaks_fnt; 

  color_info *white, *black, *red, *yellow, *green, *blue, *light_blue, *lighter_blue;
  color_info *data_color, *selected_data_color, *mark_color, *graph_color, *selected_graph_color, *listener_color, *listener_text_color, *cursor_color;
  color_info *basic_color, *selection_color, *zoom_color, *position_color, *highlight_color, *enved_waveform_color;
  color_info *text_focus_color, *filter_control_waveform_color, *mix_color, *sash_color;
  color_info *selected_grid_color, *grid_color, *axis_color;
  color_info *orig_data_color, *orig_selected_data_color, *orig_mark_color, *orig_mix_color;
  color_info *orig_graph_color, *orig_selected_graph_color, *orig_listener_color, *orig_listener_text_color, *orig_cursor_color;
  color_info *orig_basic_color, *orig_selection_color, *orig_zoom_color, *orig_position_color, *orig_highlight_color;

  gc_t *basic_gc, *selected_basic_gc, *combined_basic_gc;        
  gc_t *cursor_gc, *selected_cursor_gc;      
  gc_t *selection_gc, *selected_selection_gc;
  gc_t *erase_gc, *selected_erase_gc;        
  gc_t *mark_gc, *selected_mark_gc;          
  gc_t *mix_gc;
  gc_t *fltenv_basic_gc, *fltenv_data_gc;

  GtkWidget **dialogs;
  int num_dialogs, dialogs_size;
  bool graph_is_active;
  GtkWidget *requestor_dialog;
  mus_float_t bg_gradient;
  
  GdkCursor *arrow_cursor, *wait_cursor, *graph_cursor, *bounds_cursor, *play_cursor, *loop_play_cursor, *yaxis_cursor;
  gint fam_port;
  GtkWidget **mw;
  bool axis_color_set;
#endif

#if USE_NO_GUI
  int data_color, selected_data_color, mix_color, basic_color, grid_color, selected_grid_color, mark_color, axis_color;
  int white, black, red, yellow, green, blue, light_blue;
  int fltenv_basic_gc, fltenv_data_gc;
  int basic_gc, selected_basic_gc, combined_basic_gc;        
  int cursor_gc, selected_cursor_gc;      
  int selection_gc, selected_selection_gc;
  int erase_gc, selected_erase_gc;        
  int mark_gc, selected_mark_gc;          
  int mix_gc;           
  struct dialog_play_info *ignore_me; /* for the compiler's benefit */
  int requestor_dialog;
  bool axis_color_set;
  int bounds_cursor, graph_cursor, play_cursor, loop_play_cursor, yaxis_cursor;
#endif

} snd_state;

extern snd_state *ss;

typedef struct {
  int chans;
  mus_long_t *begs;
  chan_info **cps;
} sync_info;

typedef struct {
  int len;
  char **name;
} region_state;

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
io_error_t snd_write_header(const char *name, int type, int srate, int chans, mus_long_t samples, int format, const char *comment, int *loops);
io_error_t sndlib_error_to_snd(int sndlib_err);
int snd_file_open_descriptors(int tfd, const char *name, int format, mus_long_t location, int chans, int type);
snd_io *make_file_state(int fd, file_info *hdr, int chan, mus_long_t beg, int suggested_bufsize);
void file_buffers_forward(mus_long_t ind0, mus_long_t ind1, mus_long_t indx, snd_fd *sf, snd_data *cur_snd);
void file_buffers_back(mus_long_t ind0, mus_long_t ind1, mus_long_t indx, snd_fd *sf, snd_data *cur_snd);

void remember_temp(const char *filename, int chans);
void forget_temp(const char *filename, int chan);
void forget_temps(void);

snd_data *make_snd_data_file(const char *name, snd_io *io, file_info *hdr, file_delete_t temp, int ctr, int temp_chan);
snd_data *copy_snd_data(snd_data *sd, mus_long_t beg, int bufsize);
snd_data *free_snd_data(snd_data *sf);
snd_data *make_snd_data_buffer(mus_sample_t *data, int len, int ctr);
snd_data *make_snd_data_buffer_for_simple_channel(int len);
int open_temp_file(const char *ofile, int chans, file_info *hdr, io_error_t *err);
io_error_t close_temp_file(const char *filename, int ofd, int type, mus_long_t bytes);

void set_up_snd_io(chan_info *cp, int i, int fd, const char *filename, file_info *hdr, bool post_close);
mus_long_t io_beg(snd_io *io);
mus_long_t io_end(snd_io *io);
int io_filed(snd_io *io); /* io_fd is in use already */



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
void region_help(void);
void selection_help(void);
void colors_help(void);
char *version_info(void);
void transform_dialog_help(void);
void color_orientation_dialog_help(void);
void envelope_editor_dialog_help(void);
void region_dialog_help(void);
void raw_data_dialog_help(const char *info);
void new_file_dialog_help(void);
void edit_header_dialog_help(void);
void print_dialog_help(void);
void view_files_dialog_help(void);
void mix_dialog_help(void);
void find_dialog_help(void);
void open_file_dialog_help(void);
void mix_file_dialog_help(void);
void insert_file_dialog_help(void);
void save_as_dialog_help(void);
char* word_wrap(const char *text, int widget_len);
void g_init_help(void);
XEN g_snd_help_with_search(XEN text, int widget_wid, bool search);
XEN g_snd_help(XEN text, int widget_wid);
const char *snd_url(const char *name);
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
const char **help_name_to_xrefs(const char *name);



/* -------- snd-menu.c -------- */

void reflect_file_revert_in_label(snd_info *sp);
void set_menu_label(widget_t w, const char *label);
void g_init_menu(void);


/* -------- snd-main.c -------- */

const char *save_options_in_prefs(void);
void open_save_sound_block(snd_info *sp, FILE *fd, bool with_nth);
void close_save_sound_block(FILE *fd, bool need_f);
void save_sound_state(snd_info *sp, void *ptr);
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
  void snd_error(const char *format, ...)  __attribute__ ((format (printf, 1, 2)));
  void snd_warning(const char *format, ...)  __attribute__ ((format (printf, 1, 2)));
#else
  void snd_error(const char *format, ...);
  void snd_warning(const char *format, ...);
#endif
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

char *expression_completer(widget_t w, const char *text, void *data);
void preload_best_completions(void);
void save_completion_choice(const char *selection);
int find_best_completion(char **choices, int num_choices);
int add_completer_func(char *(*func)(widget_t w, const char *text, void *context), void *data);
int add_completer_func_with_multicompleter(char *(*func)(widget_t w, const char *text, void *context), void *data, void (*multi_func)(widget_t w, void *data));
int get_completion_matches(void);
void set_completion_matches(int matches);
void set_save_completions(bool save);
void add_possible_completion(const char *text);
int get_possible_completions_size(void);
char **get_possible_completions(void);
void clear_possible_completions(void);
void handle_completions(widget_t w, int completer);
char *complete_text(widget_t w, const char *text, int func);
char *filename_completer(widget_t w, const char *text, void *data);
char *sound_filename_completer(widget_t w, const char *text, void *data);
char *srate_completer(widget_t w, const char *text, void *data);
char *list_completer(widget_t w, const char *text, void *data);
char *info_completer(widget_t w, const char *text, void *data);
char *complete_listener_text(char *old_text, int end, bool *try_completion, char **to_file_text);
void add_srate_to_completion_list(int srate);
char *direct_completions(const char *str);



/* -------- snd-print.c -------- */

void ps_set_grf_points(double x, int j, mus_float_t ymin, mus_float_t ymax);
void ps_set_grf_point(double x, int j, mus_float_t y);
void ps_allocate_grf_points(void);
void ps_draw_grf_points(axis_info *ap, int j, mus_float_t y0, graph_style_t graph_style, int dot_size);
void ps_draw_both_grf_points(axis_info *ap, int j, graph_style_t graph_style, int dot_size);
void ps_draw_sono_rectangle(axis_info *ap, int color, mus_float_t x, mus_float_t y, mus_float_t width, mus_float_t height);
void ps_reset_color(void);
void ps_bg(axis_info *ap, graphics_context *ax);
void ps_fg(chan_info *cp, graphics_context *ax);
void ps_draw_line(axis_info *ap, int x0, int y0, int x1, int y1);
void ps_draw_spectro_line(axis_info *ap, int color, mus_float_t x0, mus_float_t y0, mus_float_t x1, mus_float_t y1);
void ps_fill_rectangle(axis_info *ap, int x0, int y0, int width, int height);
void ps_draw_string(axis_info *ap, int x0, int y0, const char *str);
void ps_set_number_font(void);
void ps_set_label_font(void);
void ps_set_bold_peak_numbers_font(void);
void ps_set_peak_numbers_font(void);
void ps_set_tiny_numbers_font(void);
bool snd_print(const char *output);
void print_enved(const char *output, int y0);
void g_init_print(void);



/* -------- snd-marks.c -------- */

int mark_sync_max(void);
void set_mark_sync(mark *m, int val);
int mark_to_int(mark *m);
int mark_sync(mark *m);
mus_long_t mark_sample(mark *m);
void marks_off(chan_info *cp);
mark *hit_mark(chan_info *cp, int x, int y);
void set_mark_control(chan_info *cp, mark *mp, int key_state);
mark *hit_mark_triangle(chan_info *cp, int x, int y);
void move_mark(chan_info *cp, mark *mp, int x);
void play_syncd_mark(chan_info *cp, mark *mp);
void finish_moving_mark(chan_info *cp, mark *m);
mark *add_mark(mus_long_t samp, const char *name, chan_info *cp);
bool delete_mark_samp(mus_long_t samp, chan_info *cp);
void free_mark_list(ed_list *ed);
bool goto_mark(chan_info *cp, int count);
void goto_named_mark(chan_info *cp, const char *name);
mark *active_mark(chan_info *cp);
mus_long_t mark_beg(chan_info *cp);
void display_channel_marks(chan_info *cp);
void ripple_marks(chan_info *cp, mus_long_t beg, mus_long_t change);
bool mark_define_region(chan_info *cp, int count);
void save_mark_list(FILE *fd, chan_info *cp, bool all_chans);
void reverse_marks(chan_info *cp, mus_long_t beg, mus_long_t dur);
void src_marks(chan_info *cp, mus_float_t ratio, mus_long_t old_samps, mus_long_t new_samps, mus_long_t beg, bool over_selection);
void reset_marks(chan_info *cp, int cur_marks, mus_long_t *samps, mus_long_t end, mus_long_t extension, bool over_selection);
void ripple_trailing_marks(chan_info *cp, mus_long_t beg, mus_long_t old_len, mus_long_t new_len);
void swap_marks(chan_info *cp0, chan_info *cp1);
void g_init_marks(void);
void *sound_store_marks(snd_info *sp);
void sound_restore_marks(snd_info *sp, void *marks);
mus_long_t mark_id_to_sample(int id);

XEN new_xen_mark(int n);
bool xen_mark_p(XEN obj);
#define XEN_MARK_P(arg) xen_mark_p(arg)
int xen_mark_to_int(XEN n);
#define XEN_MARK_TO_C_INT(n) xen_mark_to_int(n)
XEN g_mark_sync(XEN mark_n);
XEN g_set_mark_sync(XEN mark_n, XEN sync_n);
 


/* -------- snd-data.c -------- */

chan_info *make_chan_info(chan_info *cip, int chan, snd_info *sound);
snd_info *make_snd_info(snd_info *sip, const char *filename, file_info *hdr, int snd_slot, read_only_t read_only);
snd_info *make_basic_snd_info(int chans);
void initialize_control_panel(snd_info *sp);
void free_snd_info(snd_info *sp);
snd_info *completely_free_snd_info(snd_info *sp);
void for_each_chan_with_int(void (*func)(chan_info *ncp, int val), int value);
void for_each_chan_with_mus_long_t(void (*func)(chan_info *ncp, mus_long_t val), mus_long_t value);
void for_each_chan_with_bool(void (*func)(chan_info *ncp, bool val), bool value);
void for_each_chan_with_float(void (*func)(chan_info *ncp, mus_float_t val), mus_float_t value);
void for_each_chan(void (*func)(chan_info *ncp));
void for_each_normal_chan(void (*func)(chan_info *ncp));
void for_each_normal_chan_with_void(void (*func)(chan_info *ncp, void *ptr), void *userptr);
void for_each_normal_chan_with_int(void (*func)(chan_info *ncp, int val), int value);
void for_each_normal_chan_with_refint(void (*func)(chan_info *ncp, int *val), int *value);
void for_each_sound_chan(snd_info *sp, void (*func)(chan_info *ncp));
void for_each_sound_chan_with_int(snd_info *sp, void (*func)(chan_info *ncp, int val1), int value);
void for_each_sound(void (*func)(snd_info *usp));
void for_each_sound_with_void(void (*func)(snd_info *usp, void *ptr), void *userptr);
void for_each_separate_chan(void (*func)(chan_info *ncp));
bool map_over_sound_chans(snd_info *sp, bool (*func)(chan_info *ncp));
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
sync_info *make_simple_sync(chan_info *cp, mus_long_t beg);
snd_info *find_sound(const char *name, int nth);
void mix_display_during_drag(int mix_id, mus_long_t drag_beg, mus_long_t drag_end);
void g_init_data(void);



/* -------- snd-edits.c -------- */

ed_list *initial_ed_list(mus_long_t beg, mus_long_t end);
snd_info *sound_is_silence(snd_info *sp);
mus_long_t edit_changes_begin_at(chan_info *cp, int edpos);
mus_long_t edit_changes_end_at(chan_info *cp, int edpos);
bool has_unsaved_edits(snd_info *sp);
char *run_save_state_hook(const char *filename);
void edit_history_to_file(FILE *fd, chan_info *cp, bool with_save_state_hook);
char *edit_to_string(chan_info *cp, int edit);
void free_edit_list(chan_info *cp);
void backup_edit_list(chan_info *cp);
void as_one_edit(chan_info *cp, int one_edit);
void free_sound_list(chan_info *cp);
void free_ptree_list(chan_info *cp);
void after_edit(chan_info *cp);
bool extend_with_zeros(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos, const char *origin);
bool insert_samples(mus_long_t beg, mus_long_t num, mus_sample_t *vals, chan_info *cp, const char *origin, int edpos);
bool file_insert_samples(mus_long_t beg, mus_long_t num, const char *tempfile, chan_info *cp, int chan, 
			 file_delete_t auto_delete, const char *origin, int edpos);
bool insert_complete_file_at_cursor(snd_info *sp, const char *filename);
bool insert_complete_file(snd_info *sp, const char *str, mus_long_t chan_beg, file_delete_t auto_delete);
bool delete_samples(mus_long_t beg, mus_long_t num, chan_info *cp, int edpos);
bool change_samples(mus_long_t beg, mus_long_t num, mus_sample_t *vals, chan_info *cp, const char *origin, int edpos);
bool file_change_samples(mus_long_t beg, mus_long_t num, const char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin, int edpos);
bool file_override_samples(mus_long_t num, const char *tempfile, chan_info *cp, int chan, file_delete_t auto_delete, const char *origin);
mus_float_t chn_sample(mus_long_t samp, chan_info *cp, int pos);
void check_saved_temp_file(const char *type, XEN filename, XEN date_and_length);
bool editable_p(chan_info *cp);
file_delete_t xen_to_file_delete_t(XEN auto_delete, const char *caller);
snd_fd *free_snd_fd(snd_fd *sf);
char *sampler_to_string(snd_fd *fd);
bool sampler_p(XEN obj);
snd_fd *xen_to_sampler(XEN obj);
snd_fd *free_snd_fd_almost(snd_fd *sf);
bool scale_channel(chan_info *cp, mus_float_t scaler, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit);
bool scale_channel_with_origin(chan_info *cp, mus_float_t scl, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit, const char *origin);
bool ramp_channel(chan_info *cp, double start, double incr, mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit);
bool xramp_channel(chan_info *cp, double start, double incr, double scaler, double offset, 
		   mus_long_t beg, mus_long_t num, int pos, bool in_as_one_edit, mus_any *e, int xramp_seg_loc);
void ptree_channel(chan_info *cp, struct ptree *tree, mus_long_t beg, mus_long_t num, int pos, bool env_it, XEN init_func, const char *origin);
snd_fd *init_sample_read(mus_long_t samp, chan_info *cp, read_direction_t direction);
snd_fd *init_sample_read_any(mus_long_t samp, chan_info *cp, read_direction_t direction, int edit_position);
snd_fd *init_sample_read_any_with_bufsize(mus_long_t samp, chan_info *cp, read_direction_t direction, int edit_position, int bufsize);
void read_sample_change_direction(snd_fd *sf, read_direction_t dir);
bool unrampable(chan_info *cp, mus_long_t beg, mus_long_t dur, int pos, bool is_xramp);
bool ptree_or_sound_fragments_in_use(chan_info *cp, int pos);
bool unptreeable(chan_info *cp, mus_long_t beg, mus_long_t dur, int pos);
#define read_sample_to_mus_sample(Sf) MUS_FLOAT_TO_SAMPLE((*((Sf)->runf))(Sf))
#define read_sample(Sf) (*((Sf)->runf))(Sf)
mus_float_t protected_next_sample(snd_fd *sf);
mus_float_t protected_previous_sample(snd_fd *sf);
mus_float_t channel_local_maxamp(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos, mus_long_t *maxpos);
bool undo_edit_with_sync(chan_info *cp, int count);
bool redo_edit_with_sync(chan_info *cp, int count);
bool undo_edit(chan_info *cp, int count);
bool redo_edit(chan_info *cp, int count);
io_error_t save_channel_edits(chan_info *cp, const char *ofile, int pos);
io_error_t channel_to_file_with_settings(chan_info *cp, const char *new_name, int type, int format, int srate, const char *comment, int pos);
io_error_t save_edits(snd_info *sp);
io_error_t save_edits_without_asking(snd_info *sp);
void save_edits_with_prompt(snd_info *sp);
io_error_t save_edits_and_update_display(snd_info *sp);
io_error_t save_edits_without_display(snd_info *sp, const char *new_name, int type, int format, int srate, const char *comment, int pos);
void revert_edits(chan_info *cp);
mus_long_t current_location(snd_fd *sf);
void g_init_edits(void);
void set_ed_maxamp(chan_info *cp, int edpos, mus_float_t val);
mus_float_t ed_maxamp(chan_info *cp, int edpos);
void set_ed_maxamp_position(chan_info *cp, int edpos, mus_long_t val);
mus_long_t ed_maxamp_position(chan_info *cp, int edpos);
void set_ed_selection_maxamp(chan_info *cp, mus_float_t val);
mus_float_t ed_selection_maxamp(chan_info *cp);
void set_ed_selection_maxamp_position(chan_info *cp, mus_long_t val);
mus_long_t ed_selection_maxamp_position(chan_info *cp);
void copy_then_swap_channels(chan_info *cp0, chan_info *cp1, int pos0, int pos1);
void reflect_file_change_in_label(chan_info *cp);
void reflect_file_change_in_title(void);

bool snd_to_sample_p(mus_any *ptr);
mus_float_t snd_to_sample_read(mus_any *ptr, mus_long_t frame, int chan);
int mix_buffer_with_tag(chan_info *cp, mus_sample_t *data, mus_long_t beg, mus_long_t num, const char *origin);

int mix_file_with_tag(chan_info *cp, const char *filename, int chan, mus_long_t beg, file_delete_t auto_delete, const char *origin);
void unmix(chan_info *cp, mix_state *ms);
void remix(chan_info *cp, mix_state *ms);
snd_fd *make_virtual_mix_reader(chan_info *cp, mus_long_t beg, mus_long_t len, int index, mus_float_t scl, read_direction_t direction);
bool virtual_mix_ok(chan_info *cp, int edpos);
bool begin_mix_op(chan_info *cp, mus_long_t old_beg, mus_long_t old_len, mus_long_t new_beg, mus_long_t new_len, int edpos, const char *caller);
void end_mix_op(chan_info *cp, mus_long_t old_beg, mus_long_t old_len);
void prepare_sound_list(chan_info *cp);
XEN g_sampler_file_name(XEN obj);

vct *run_samples_to_vct(mus_long_t beg, mus_long_t len, chan_info *cp, int pos);
char *edit_list_to_function(chan_info *cp, int start_pos, int end_pos);



/* -------- snd-fft.c -------- */

int find_and_sort_transform_peaks(mus_float_t *buf, fft_peak *found, int num_peaks, mus_long_t losamp, mus_long_t hisamp, mus_float_t samps_per_pixel, mus_float_t fft_scale);
int find_and_sort_peaks(mus_float_t *buf, fft_peak *found, int num_peaks, mus_long_t losamp, mus_long_t hisamp);
fft_info *free_fft_info(fft_info *fp);
void free_sonogram_fft_state(void *ptr);
bool fft_window_beta_in_use(mus_fft_window_t win);
bool fft_window_alpha_in_use(mus_fft_window_t win);
void free_sono_info(chan_info *cp);
void sono_update(chan_info *cp);
void c_convolve(const char *fname, mus_float_t amp, int filec, mus_long_t filehdr, int filterc, mus_long_t filterhdr, mus_long_t filtersize,
		mus_long_t fftsize, int filter_chans, int filter_chan, mus_long_t data_size, snd_info *gsp);
void *make_sonogram_state(chan_info *cp, bool force_recalc);
void single_fft(chan_info *cp, bool update_display, bool force_recalc);
idle_func_t sonogram_in_slices(void *sono);
void clear_transform_edit_ctrs(chan_info *cp);
void g_init_fft(void);
mus_float_t fft_beta_max(mus_fft_window_t win);
void cp_free_fft_state(chan_info *cp);
void set_fft_info_xlabel(chan_info *cp, const char *new_label);
void fourier_spectrum(snd_fd *sf, mus_float_t *data, mus_long_t fft_size, mus_long_t data_len, mus_float_t *window, chan_info *cp);
const char *wavelet_name(int i);
const char **wavelet_names(void);
void set_log_freq_start(mus_float_t base);

const char *transform_name(int type);
const char *transform_program_name(int type);
int transform_position_to_type(int pos);
int transform_type_to_position(int type);
int max_transform_type(void);
void set_transform_position(int i, int j);
bool transform_p(int type);

XEN new_xen_transform(int n);
bool xen_transform_p(XEN obj);
int xen_transform_to_int(XEN n);
#define XEN_TRANSFORM_P(arg) xen_transform_p(arg)
#define C_INT_TO_XEN_TRANSFORM(Val) new_xen_transform(Val)
#define XEN_TRANSFORM_TO_C_INT(n) xen_transform_to_int(n)



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
XEN string_to_form(const char *data);
XEN g_c_make_sampler(snd_fd *fd);
char *procedure_ok(XEN proc, int args, const char *caller, const char *arg_name, int argn);
bool procedure_arity_ok(XEN proc, int args);
int snd_protect(XEN obj);
void snd_unprotect_at(int loc);

XEN snd_protected_at(int loc);
XEN run_or_hook(XEN hook, XEN args, const char *caller);
XEN run_progn_hook(XEN hook, XEN args, const char *caller);
XEN run_hook(XEN hook, XEN args, const char *caller);
bool listener_print_p(const char *msg);
void check_features_list(const char *features);
#if (!USE_NO_GUI)
  mus_float_t check_color_range(const char *caller, XEN val);
#endif
void set_basic_color(color_t color);
void set_highlight_color(color_t color);
void set_position_color(color_t color);
void set_zoom_color(color_t color);
void set_data_color(color_t color);
void set_selected_data_color(color_t color);
void set_graph_color(color_t color);
void set_selected_graph_color(color_t color);
mus_float_t string_to_mus_float_t(const char *str, mus_float_t lo, const char *file_name);
int string_to_int(const char *str, int lo, const char *field_name);
mus_long_t string_to_mus_long_t(const char *str, mus_long_t lo, const char *field_name);
char *output_comment(file_info *hdr);
void snd_load_init_file(bool nog, bool noi);
void snd_load_file(const char *filename);
void snd_display_result(const char *str, const char *endstr);
void snd_report_result(XEN result, const char *buf);
void snd_report_listener_result(XEN form);
void snd_eval_stdin_str(const char *buf);
void clear_stdin(void);
#if HAVE_RUBY
  void snd_rb_raise(XEN type, XEN info);
#endif
bool source_file_p(const char *name);
void save_added_source_file_extensions(FILE *fd);



/* -------- snd-select.c -------- */

bool selection_is_active(void);
bool selection_is_active_in_channel(chan_info *cp);
bool selection_is_visible_in_channel(chan_info *cp);
mus_long_t selection_beg(chan_info *cp);
mus_long_t selection_end(chan_info *cp);
mus_long_t selection_len(void);
int selection_chans(void);
int selection_srate(void);
mus_float_t selection_maxamp(chan_info *cp);
void deactivate_selection(void);
void reactivate_selection(chan_info *cp, mus_long_t beg, mus_long_t end);
void ripple_selection(ed_list *new_ed, mus_long_t beg, mus_long_t num);
sync_info *selection_sync(void);
void start_selection_creation(chan_info *cp, mus_long_t samp);
void update_possible_selection_in_progress(mus_long_t samp);
void restart_selection_creation(chan_info *cp, bool right);
bool hit_selection_triangle(chan_info *cp, int x, int y);
bool hit_selection_loop_triangle(chan_info *cp, int x, int y);
void cp_delete_selection(chan_info *cp);

int make_region_from_selection(void);
void display_selection(chan_info *cp);
bool delete_selection(cut_selection_regraph_t regraph);
void move_selection(chan_info *cp, int x);
void finish_selection_creation(void);
int select_all(chan_info *cp);
io_error_t save_selection(const char *ofile, int type, int format, int srate, const char *comment, int chan);
bool selection_creation_in_progress(void);
void add_selection_or_region(int reg, chan_info *cp);
void insert_selection_from_menu(void);
void insert_selection_or_region(int reg, chan_info *cp);
void cancel_selection_watch(void);
void show_selection(void);
bool xen_selection_p(XEN obj);
#define XEN_SELECTION_P(arg) xen_selection_p(arg)
XEN g_selection_chans(void);
XEN g_selection_srate(void);
XEN g_selection_maxamp(XEN snd, XEN chn);
XEN g_selection_frames(XEN snd, XEN chn);

void g_init_selection(void);
  

/* -------- snd-region.c -------- */

void allocate_regions(int numreg);
bool region_ok(int n);
int region_chans(int n);
int region_srate(int n);
const char *region_file_name(int n);
mus_long_t region_len(int n);
mus_float_t region_maxamp(int n);
int region_list_position_to_id(int n);
int region_id_to_list_position(int id);
file_info *fixup_region_data(chan_info *cp, int chan, int n);
region_state *region_report(void);
void free_region_state(region_state *r);
int remove_region_from_list(int pos);
io_error_t paste_region(int n, chan_info *cp);
io_error_t add_region(int n, chan_info *cp);
int define_region(sync_info *si, mus_long_t *ends);
snd_fd *init_region_read(mus_long_t beg, int n, int chan, read_direction_t direction);
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
mus_long_t region_current_location(snd_fd *fd);
char *region_description(int rg);

XEN new_xen_region(int n);
bool xen_region_p(XEN obj);
int xen_region_to_int(XEN n);
#define XEN_REGION_P(arg) xen_region_p(arg)
#define C_INT_TO_XEN_REGION(Val) new_xen_region(Val)
#define XEN_REGION_TO_C_INT(n) xen_region_to_int(n)
XEN g_region_srate(XEN n);
XEN g_region_chans(XEN n);
XEN g_region_frames(XEN n, XEN chan);
XEN g_region_maxamp(XEN n);
XEN g_play_region(XEN n, play_process_t back, XEN stop_proc);



/* -------- snd-env.c -------- */

env *copy_env(env *e);
env *free_env(env *e);
char *env_to_string(env *e);
env *make_envelope_with_offset_and_scaler(mus_float_t *env_buffer, int len, mus_float_t offset, mus_float_t scaler);
env *default_env(mus_float_t x1, mus_float_t y);
bool default_env_p(env *e);
bool envs_equal(env *e1, env *e2);
env_editor *new_env_editor(void);
void env_editor_button_motion_with_xy(env_editor *edp, int evx, int evy, oclock_t motion_time, env *e, mus_float_t *new_x, mus_float_t *new_y);
void env_editor_button_motion(env_editor *edp, int evx, int evy, oclock_t motion_time, env *e);
bool env_editor_button_press(env_editor *edp, int evx, int evy, oclock_t time, env *e);
void env_editor_button_release(env_editor *edp, env *e);
double env_editor_ungrf_y_dB(env_editor *edp, int y);
void init_env_axes(axis_info *ap, const char *name, int x_offset, int ey0, int width, int height, 
		   mus_float_t xmin, mus_float_t xmax, mus_float_t ymin, mus_float_t ymax, printing_t printing);
void env_editor_display_env(env_editor *edp, env *e, graphics_context *ax, const char *name, 
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
void alert_envelope_editor(const char *name, env *val);
void enved_show_background_waveform(axis_info *ap, axis_info *gray_ap, bool apply_to_selection, bool show_fft, printing_t printing);
void save_envelope_editor_state(FILE *fd);
char *env_name_completer(widget_t w, const char *text, void *data);
env *enved_next_env(void);
env *string_to_env(const char *str);
void add_or_edit_symbol(const char *name, env *val);
env* name_to_env(const char *str);
env *position_to_env(int pos);
void delete_envelope(const char *name);
enved_fft *free_enved_fft(enved_fft *ef);
void reflect_enved_fft_change(chan_info *cp);

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
void play_channel(chan_info *cp, mus_long_t start, mus_long_t end);
void play_channel_with_sync(chan_info *cp, mus_long_t start, mus_long_t end);
void play_sound(snd_info *sp, mus_long_t start, mus_long_t end);
void play_channels(chan_info **cps, int chans, mus_long_t *starts, mus_long_t *ur_ends, 
		   play_process_t background, XEN edpos, bool selection, const char *caller, int arg_pos);
void play_selection(play_process_t background);
void loop_play_selection(void);
bool add_mix_to_play_list(mix_state *ms, chan_info *cp, mus_long_t beg_within_mix, bool start_playing);
void toggle_dac_pausing(void); /* snd-dac.c */
bool play_in_progress(void);
void initialize_apply(snd_info *sp, int chans, mus_long_t beg, mus_long_t frames);
void finalize_apply(snd_info *sp);
int run_apply(int ofd);
mus_float_t *sample_linear_env(env *e, int order);

void g_init_dac(void);
void clear_players(void);

bool xen_player_p(XEN obj);
#define XEN_PLAYER_P(arg) xen_player_p(arg)
#define IS_PLAYER_SOUND(Sp) ((Sp) && ((Sp)->index < 0))
snd_info *get_player_sound(XEN player);
XEN no_such_player_error(const char *caller, XEN player);

void dac_set_expand(snd_info *sp, mus_float_t newval);
void dac_set_expand_length(snd_info *sp, mus_float_t newval);
void dac_set_expand_ramp(snd_info *sp, mus_float_t newval);
void dac_set_expand_hop(snd_info *sp, mus_float_t newval);
void dac_set_contrast_amp(snd_info *sp, mus_float_t newval);
void dac_set_reverb_feedback(snd_info *sp, mus_float_t newval);
void dac_set_reverb_lowpass(snd_info *sp, mus_float_t newval);



/* -------- snd-chn.c -------- */

bool graph_style_p(int grf);
chan_info *get_cp(XEN snd_n, XEN chn_n, const char *caller);
snd_info *make_simple_channel_display(int srate, int initial_length, fw_button_t with_arrows, 
				      graph_style_t grf_style, widget_t container, bool with_events);
axis_info *lisp_info_axis(chan_info *cp);
void free_lisp_info(chan_info *cp);
void zx_incremented(chan_info *cp, double amount);
kbd_cursor_t cursor_decision(chan_info *cp);
void reset_x_display(chan_info *cp, double sx, double zx);
void set_x_axis_x0x1(chan_info *cp, double x0, double x1);
void cursor_move(chan_info *cp, mus_long_t samps);
void cursor_moveto_without_verbosity(chan_info *cp, mus_long_t samp);
void cursor_moveto_with_window(chan_info *cp, mus_long_t samp, mus_long_t left_samp, mus_long_t window_size);
void sync_cursors(chan_info *cp, mus_long_t samp);
void set_wavo_trace(int uval);
void set_dot_size(int val);
chan_info *virtual_selected_channel(chan_info *cp);
void handle_cursor(chan_info *cp, kbd_cursor_t redisplay);
void handle_cursor_with_sync(chan_info *cp, kbd_cursor_t redisplay);
void chans_field(fcp_t field, mus_float_t val);
void in_set_transform_graph_type(graph_type_t val);
void in_set_fft_window(mus_fft_window_t val);
void set_max_transform_peaks(int val);
void combine_sound(snd_info *sp);
void separate_sound(snd_info *sp);
void superimpose_sound(snd_info *sp);
void set_sound_channel_style(snd_info *sp, channel_style_t val);
void set_chan_fft_in_progress(chan_info *cp, idle_t fp);
void stop_fft_in_progress(chan_info *cp);
void goto_graph(chan_info *cp);
void start_peak_env(chan_info *cp);
void stop_peak_env(chan_info *cp);
void write_transform_peaks(FILE *fd, chan_info *ucp);
bool chan_fft_in_progress(chan_info *cp);
void force_fft_clear(chan_info *cp);
void chan_info_cleanup(chan_info *cp);
void display_channel_data_for_print(chan_info *cp);
void update_graph(chan_info *cp);
void update_graph_or_warn(chan_info *cp);
void make_partial_graph(chan_info *cp, mus_long_t beg, mus_long_t end);
void add_channel_data(char *filename, chan_info *cp, channel_graph_t graphed);
bool add_channel_data_1(chan_info *cp, int srate, mus_long_t frames, channel_graph_t graphed);
void set_x_bounds(axis_info *ap);
void set_show_axes(show_axes_t val);
void display_channel_data(chan_info *cp);
void display_channel_fft_data(chan_info *cp);
void display_channel_time_data(chan_info *cp);
void show_cursor_info(chan_info *cp);
void apply_x_axis_change(chan_info *cp);
void apply_y_axis_change(chan_info *cp);
void sx_incremented(chan_info *cp, double amount);
int move_axis(chan_info *cp, int x);
void set_axes(chan_info *cp, double x0, double x1, mus_float_t y0, mus_float_t y1);
void focus_x_axis_change(chan_info *cp, int focus_style);
bool key_press_callback(chan_info *ur_cp, int x, int y, int key_state, int keysym);
void graph_button_press_callback(chan_info *cp, void *ev, int x, int y, int key_state, int button, oclock_t time);
void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button);
void graph_button_motion_callback(chan_info *cp, int x, int y, oclock_t time);
void channel_resize(chan_info *cp);
void edit_history_select(chan_info *cp, int row);
int make_background_graph(chan_info *cp, int srate, bool *two_sided);
int make_dragged_marks_graph(chan_info *cp);
void reset_spectro(void);
void cursor_moveto(chan_info *cp, mus_long_t samp);
chan_info *which_channel(snd_info *sp, int y);
void set_show_grid(with_grid_t val);
void set_grid_density(mus_float_t val);
void set_cursor_size(int val);
void set_cursor_style(cursor_style_t val);
void set_show_mix_waveforms(bool val);
void clear_inset_graph(chan_info *cp);
void free_inset_graph(chan_info *cp);
void draw_inset_line_cursor(chan_info *cp, graphics_context *ax);

void g_init_chn(void);
XEN make_graph_data(chan_info *cp, int edit_pos, mus_long_t losamp, mus_long_t hisamp);
void draw_graph_data(chan_info *cp, mus_long_t losamp, mus_long_t hisamp, int data_size, mus_float_t *data, mus_float_t *data1, graphics_context *ax, graph_style_t style);

void fftb(chan_info *cp, bool on);
void waveb(chan_info *cp, bool on);
void f_button_callback(chan_info *cp, bool on, bool with_control);
void w_button_callback(chan_info *cp, bool on, bool with_control);
graphics_context *set_context(chan_info *cp, chan_gc_t gc);
graphics_context *copy_context(chan_info *cp);
graphics_context *erase_context(chan_info *cp);
graphics_context *selection_context(chan_info *cp);
graphics_context *mark_tag_context(chan_info *cp);
graphics_context *mix_waveform_context(chan_info *cp);
graphics_context *cursor_context(chan_info *cp);
void calculate_fft(chan_info *cp);
void set_min_db(mus_float_t db);
void set_x_axis_style(x_axis_style_t val);
void set_verbose_cursor(bool val);
void set_graph_style(graph_style_t val);
void set_show_marks(bool val);
void set_show_y_zero(bool val);

XEN g_frames(XEN snd_n, XEN chn_n, XEN edpos);
void check_cursor_shape(chan_info *cp, int x, int y);
widget_t channel_to_widget(chan_info *cp);
chan_info *channel_to_chan(chan_info *cp);



/* -------- snd-axis.c -------- */

bool x_axis_style_p(int n);
bool show_axes_p(int n);
axis_info *free_axis_info(axis_info *ap);
char *x_axis_location_to_string(chan_info *cp, double loc);
int grf_x(double val, axis_info *ap);
int grf_y(mus_float_t val, axis_info *ap);
void init_axis_scales(axis_info *ap);
void make_axes_1(axis_info *ap, x_axis_style_t x_style, int srate, show_axes_t axes, printing_t printing, 
		 with_x_axis_t show_x_axis, with_grid_t grid, log_axis_t log_axes, mus_float_t grid_scale);

#define ungrf_x(AP, X) (((X) - (AP)->x_base) / (AP)->x_scale)
#define ungrf_y(AP, Y) (((Y) - (AP)->y_base) / (AP)->y_scale)

axis_info *make_axis_info(chan_info *cp, double xmin, double xmax, mus_float_t ymin, mus_float_t ymax, 
			  const char *xlabel, double x0, double x1, mus_float_t y0, mus_float_t y1,
			  axis_info *old_ap);
void set_numbers_font(graphics_context *ax, printing_t printing, bool use_tiny_font);

#if (!USE_NO_GUI)
  void g_init_axis(void);
#endif
#if HAVE_GL
  void reload_label_font(void);
  void reload_number_font(void);
#endif



/* -------- snd-snd.c -------- */

snd_info *get_sp(XEN snd_n);
peak_env_info *free_peak_env(chan_info *cp, int pos);
void free_peak_env_state(chan_info *cp);
peak_env_info *free_peak_env_info(peak_env_info *ep);
void start_peak_env_state(chan_info *cp);
idle_func_t get_peak_env(any_pointer_t ptr);
void finish_peak_env(chan_info *cp);
bool peak_env_maxamp_ok(chan_info *cp, int edpos);
mus_float_t peak_env_maxamp(chan_info *cp, int edpos);
bool peak_env_usable(chan_info *cp, mus_float_t samples_per_pixel, mus_long_t hisamp, bool start_new, int edit_pos, bool finish_env);
int peak_env_graph(chan_info *cp, mus_float_t samples_per_pixel, int srate);
int peak_env_partial_graph(chan_info *cp, mus_long_t beg, mus_long_t end, mus_float_t samples_per_pixel, int srate);
char *shortname(snd_info *sp);
char *shortname_indexed(snd_info *sp);
void add_sound_data(char *filename, snd_info *sp, channel_graph_t graphed);
mus_float_t speed_changed(mus_float_t ival, char *srcbuf, speed_style_t style, int tones, int srcbuf_size);
void sp_name_click(snd_info *sp);
void free_controls(snd_info *sp);
void save_controls(snd_info *sp);
void restore_controls(snd_info *sp);
void reset_controls(snd_info *sp);
void set_show_controls(bool val);
void stop_applying(snd_info *sp);
void menu_apply_controls(snd_info *sp);
void menu_reset_controls(snd_info *sp);
void expand_control_set_hop(mus_float_t hop);
void expand_control_set_length(mus_float_t hop);
void expand_control_set_ramp(mus_float_t hop);
void expand_control_set_jitter(mus_float_t hop);
void contrast_control_set_amp(mus_float_t hop);
void reverb_control_set_lowpass(mus_float_t hop);
void reverb_control_set_feedback(mus_float_t hop);
void amp_env_env(chan_info *cp, mus_float_t *brkpts, int npts, int pos, mus_float_t base, mus_float_t scaler, mus_float_t offset);
peak_env_info *copy_peak_env_info(peak_env_info *old_ep, bool reversed);
void amp_env_env_selection_by(chan_info *cp, mus_any *e, mus_long_t beg, mus_long_t num, int pos);
void peak_env_ptree(chan_info *cp, struct ptree *pt, int pos, XEN init_func);
void peak_env_ptree_selection(chan_info *cp, struct ptree *pt, mus_long_t beg, mus_long_t num, int pos, XEN init_func);
void peak_env_insert_zeros(chan_info *cp, mus_long_t beg, mus_long_t num, int pos);
snd_info *snd_new_file(const char *newname, int header_type, int data_format, int srate, int chans, const char *new_comment, mus_long_t samples);
#if XEN_HAVE_RATIOS
  void snd_rationalize(mus_float_t a, int *num, int *den);
#endif

void g_init_snd(void);
XEN snd_no_such_sound_error(const char *caller, XEN n);

void peak_env_scale_by(chan_info *cp, mus_float_t scl, int pos);
void peak_env_scale_selection_by(chan_info *cp, mus_float_t scl, mus_long_t beg, mus_long_t num, int pos);
peak_env_info *peak_env_copy(chan_info *cp, bool reversed, int edpos);
peak_env_info *peak_env_section(chan_info *cp, mus_long_t beg, mus_long_t num, int edpos);
void pick_one_bin(peak_env_info *ep, int bin, mus_long_t cursamp, chan_info *cp, int edpos);
void remember_mini_string(snd_info *sp, const char *str);
void restore_mini_string(snd_info *s, bool back);
void clear_mini_strings(snd_info *sp);
void remember_filter_string(snd_info *sp, const char *str);
void restore_filter_string(snd_info *s, bool back);
void clear_filter_strings(snd_info *sp);
void remember_listener_string(const char *str);
void restore_listener_string(bool back);
void set_channel_style(channel_style_t val);

XEN new_xen_sound(int n);
bool xen_sound_p(XEN obj);
int xen_sound_to_int(XEN n);
#define XEN_SOUND_P(arg) xen_sound_p(arg)
#define C_INT_TO_XEN_SOUND(Val) new_xen_sound(Val)
#define XEN_SOUND_TO_C_INT(n) xen_sound_to_int(n)

const char *read_peak_env_info_file(chan_info *cp);
bool write_peak_env_info_file(chan_info *cp);
void delete_peak_env_info_file(chan_info *cp);



/* -------- snd-file -------- */

axes_data *free_axes_data(axes_data *sa);
axes_data *make_axes_data(snd_info *sp);
void restore_axes_data(snd_info *sp, axes_data *sa, mus_float_t new_duration, bool need_edit_history_update);
mus_long_t disk_kspace(const char *filename);
time_t file_write_date(const char *filename);
bool link_p(const char *filename);
int recent_files_size(void);
char **recent_files(void);
bool directory_p(const char *filename);
file_info *make_file_info(const char *fullname, read_only_t read_only, bool selected);
file_info *free_file_info(file_info *hdr);
file_info *copy_header(const char *fullname, file_info *ohdr);
file_info *make_temp_header(const char *fullname, int srate, int chans, mus_long_t samples, const char *caller);
bool sound_file_p(const char *name);
void init_sound_file_extensions(void);
void save_added_sound_file_extensions(FILE *fd);
snd_info *snd_open_file(const char *filename, read_only_t read_only);
void snd_close_file(snd_info *sp);
snd_info *make_sound_readable(const char *filename, bool post_close);
snd_info *snd_update(snd_info *sp);
snd_info *snd_update_within_xen(snd_info *sp, const char *caller);
int snd_decode(int type, const char *input_filename, const char *output_filename);
void set_fallback_srate(int sr);
void set_fallback_chans(int ch);
void set_fallback_format(int fr);
void set_with_tooltips(bool val);
void run_after_save_as_hook(snd_info *sp, const char *already_saved_as_name, bool from_save_as_dialog);
bool run_before_save_as_hook(snd_info *sp, const char *save_as_filename, bool selection, int srate, int type, int format, const char *comment);
void during_open(int fd, const char *file, open_reason_t reason);
void after_open(snd_info *sp);
char *output_name(const char *current_name);
void save_view_files_dialogs(FILE *fd);
widget_t make_view_files_dialog(bool managed, bool make_new);
void view_files_unplay(void);
void view_files_add_directory(widget_t dialog, const char *dirname);
char *view_files_find_any_directory(void);
int view_files_dialog_list_length(void);
char **view_files_dialog_titles(void);
void view_files_start_dialog_with_title(const char *title);
void set_with_toolbar_and_display(bool val);
#if (!USE_NO_GUI)
  void display_info(snd_info *sp);
#endif
void g_init_file(void);
void initialize_format_lists(void);
void set_with_menu_icons(bool val);



/* -------- snd-utils -------- */

int snd_round(double x);
mus_long_t snd_round_mus_long_t(double x);
mus_long_t snd_abs_mus_long_t(mus_long_t val);
int snd_int_pow2(int n);
mus_long_t snd_mus_long_t_pow2(int n);
int snd_to_int_pow2(int n);
int snd_int_log2(int n);
bool snd_feq(mus_float_t val1, mus_float_t val2);

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define in_dB(Min_Db, Lin_Db, Val)  ({ mus_float_t _snd_1_h_1 = Val; (_snd_1_h_1 <= Lin_Db) ? Min_Db : (20.0 * log10(_snd_1_h_1)); })
#else
  mus_float_t in_dB(mus_float_t min_dB, mus_float_t lin_dB, mus_float_t val);
#endif

int snd_mkdir(const char *filename);
char *snd_local_time(void);
char *snd_io_strerror(void);
char *snd_open_strerror(void);
char *string_to_colon(char *val);
char *filename_without_directory(const char *name);
char *just_filename(char *name);
char *just_directory(const char *name);
char *file_to_string(const char *filename);

#ifdef __GNUC__
  char *vstr(const char *format, va_list ap)  __attribute__ ((format (printf, 1, 0)));
  char *snd_strftime(const char *format, time_t date) __attribute__ ((format (strftime, 1, 0)));
#else
  char *vstr(const char *format, va_list ap);
  char *snd_strftime(const char *format, time_t date);
#endif

disk_space_t disk_space_p(mus_long_t bytes, const char *filename);
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



/* -------- snd-listener -------- */

void backup_listener_to_previous_expression(void);
void listener_return(widget_t w, int last_prompt);
bool is_prompt(const char *str, int beg);
bool within_prompt(const char *str, int beg, int end);
char *listener_prompt_with_cr(void);
void set_listener_prompt(const char *new_prompt);
int check_balance(const char *expr, int start, int end, bool in_listener);
int find_matching_paren(const char *str, int parens, int pos, int *highlight_pos);
bool listener_is_visible(void);
void g_init_listener(void);
char *trim(char *orig);
void listener_help_at_cursor(char *buf, int name_curpos, int len, int prompt_pos);



/* -------- snd-mix.c -------- */

void free_ed_mixes(void *ptr);
bool mix_exists(int n);
bool mix_is_active(int n);
bool channel_has_mixes(chan_info *cp);
bool channel_has_active_mixes(chan_info *cp);
const char *mix_name(int id);
const char *mix_file_name(int id);
int mix_name_to_id(const char *name);
void goto_mix(chan_info *cp, int count);
mus_long_t zoom_focus_mix_in_channel_to_position(chan_info *cp);
int any_mix_id(void);
int next_mix_id(int id);
int previous_mix_id(int id);
int lowest_mix_id(void);
int highest_mix_id(void);
void reset_mix_ctr(void);
void preload_mixes(mix_state **mixes, int low_id, ed_list *ed);
void free_channel_mixes(chan_info *cp);
void delete_any_remaining_mix_temp_files_at_exit(chan_info *cp);
int mix_sync_from_id(int id);
int mix_set_sync_from_id(int id, int new_sync);
void set_mix_waveform_height(int new_val);
XEN new_xen_mix(int n);
XEN g_make_mix_sampler(XEN mix_id, XEN ubeg);
bool xen_mix_p(XEN obj);
#define XEN_MIX_P(arg) xen_mix_p(arg)
snd_fd *xen_mix_to_snd_fd(XEN obj);
int xen_mix_to_int(XEN n);
#define XEN_MIX_TO_C_INT(n) xen_mix_to_int(n)
XEN g_mix_length(XEN n);
XEN g_mix_sync(XEN n);
XEN g_set_mix_sync(XEN n, XEN val);
XEN g_mix_maxamp(XEN mix_id);
double mix_maxamp(int mix_id);
 
mus_long_t mix_position_from_id(int id);
mus_long_t mix_length_from_id(int id);
mus_float_t mix_amp_from_id(int id);
mus_float_t mix_speed_from_id(int id);
env *mix_amp_env_from_id(int id);
chan_info *mix_chan_info_from_id(int id);
int copy_mix(int id);

mix_state *prepare_mix_state_for_channel(chan_info *cp, int mix_loc, mus_long_t beg, mus_long_t len);
void add_ed_mix(ed_list *ed, mix_state *ms);
mix_state *copy_mix_state(mix_state *old_ms);

void g_init_mix(void);

bool mix_set_position_edit(int id, mus_long_t pos);
bool mix_set_amp_env_edit(int id, env *e);
bool mix_set_amp_edit(int id, mus_float_t amp);
bool mix_set_speed_edit(int id, mus_float_t spd);
void after_mix_edit(int id);

void syncd_mix_set_color(int id, color_t col);
void syncd_mix_unset_color(int id);
void syncd_mix_set_amp(int id, mus_float_t amp);
void syncd_mix_set_speed(int id, mus_float_t amp);
void syncd_mix_set_amp_env(int id, env *e);
void syncd_mix_play(int id);
void mix_unset_color_from_id(int id);
color_t mix_color_from_id(int mix_id);
color_t mix_set_color_from_id(int id, color_t new_color);
void start_dragging_syncd_mixes(int mix_id);
void keep_dragging_syncd_mixes(int mix_id);
void stop_dragging_syncd_mixes(int mix_id);
void after_syncd_mix_edit(int id);
void syncd_mix_change_position(int mix_id, mus_long_t change);

int mix_complete_file(snd_info *sp, mus_long_t beg, const char *fullname, bool with_tag, file_delete_t auto_delete, mix_sync_t all_chans, int *out_chans);
int mix_complete_file_at_cursor(snd_info *sp, const char *str);
int mix_file(mus_long_t beg, mus_long_t num, int chans, chan_info **cps, const char *mixinfile, file_delete_t temp, const char *origin, bool with_tag, int start_chan);

bool mix_sampler_p(XEN obj);
XEN g_copy_mix_sampler(XEN obj);
XEN g_mix_sampler_home(XEN obj);
XEN g_mix_sampler_at_end_p(XEN obj);
XEN g_mix_sampler_position(XEN obj);
XEN g_free_mix_sampler(XEN obj);
char *edit_list_mix_init(chan_info *cp);
void channel_set_mix_tags_erased(chan_info *cp);
void color_mixes(color_t color);
void move_mix_tag(int mix_tag, int x, int y);
void finish_moving_mix_tag(int mix_tag, int x);
int hit_mix(chan_info *cp, int x, int y);
int hit_mix_triangle(chan_info *cp, int x, int y);
int prepare_mix_dialog_waveform(int mix_id, axis_info *ap, bool *two_sided);
void display_channel_mixes(chan_info *cp);

bool play_mix_from_id(int mix_id);
XEN g_play_mix(XEN num, mus_long_t beg);
void drag_and_drop_mix_at_x_y(int data, const char *filename, int x, int y);



/* -------- snd-find.c -------- */

char *global_search(read_direction_t direction);
void cursor_search(chan_info *cp, int count);
void clear_sound_search_procedure(snd_info *sp, bool clear_expr_too);
void clear_global_search_procedure(bool clear_expr_too);

void g_init_find(void);



/* -------- snd-trans.c -------- */

int snd_translate(const char *oldname, const char *newname, int type);


/* -------- snd.c -------- */

void mus_error_to_snd(int type, char *msg);
void snd_set_global_defaults(bool need_cleanup);
#if SND_AS_WIDGET
  snd_state *snd_main(int argc, char **argv);
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
void map_over_key_bindings(bool (*func)(int key, int state, bool cx, XEN xf));
key_info *find_prefs_key_binding(const char *prefs_name);
void keyboard_command(chan_info *cp, int keysym, int state);
void control_g(snd_info *sp);
void g_init_kbd(void);


/* -------- snd-sig.c -------- */

void scale_by(chan_info *cp, mus_float_t *scalers, int len, bool selection);
bool scale_to(snd_info *sp, chan_info *cp, mus_float_t *scalers, int len, bool selection);
mus_float_t channel_maxamp(chan_info *cp, int edpos);
mus_long_t channel_maxamp_position(chan_info *cp, int edpos);
void src_env_or_num(chan_info *cp, env *e, mus_float_t ratio, bool just_num, 
		    const char *origin, bool over_selection, mus_any *gen, XEN edpos, int arg_pos);
void apply_filter(chan_info *ncp, int order, env *e, const char *caller, const char *origin, 
		  bool over_selection, mus_float_t *ur_a, mus_any *gen, XEN edpos, int arg_pos, bool truncate);
void apply_env(chan_info *cp, env *e, mus_long_t beg, mus_long_t dur, bool over_selection, 
	       const char *origin, mus_any *gen, XEN edpos, int arg_pos);
void cos_smooth(chan_info *cp, mus_long_t beg, mus_long_t num, bool over_selection);
void display_frequency_response(env *e, axis_info *ap, graphics_context *gax, int order, bool dBing);
void cursor_delete(chan_info *cp, mus_long_t count);
void cursor_zeros(chan_info *cp, mus_long_t count, bool over_selection);
void cursor_insert(chan_info *cp, mus_long_t beg, mus_long_t count);
void cut_and_smooth(chan_info *cp);
void src_file(const char *file, double ratio);

void g_init_sig(void);
int to_c_edit_position(chan_info *cp, XEN edpos, const char *caller, int arg_pos);
mus_long_t to_c_edit_samples(chan_info *cp, XEN edpos, const char *caller, int arg_pos);
mus_long_t beg_to_sample(XEN beg, const char *caller);
mus_long_t dur_to_samples(XEN dur, mus_long_t beg, chan_info *cp, int edpos, int argn, const char *caller);
char *scale_and_src(char **files, int len, int max_chans, mus_float_t amp, mus_float_t speed, env *amp_env, bool *err);
XEN g_scale_selection_by(XEN scalers);
void reverse_sound(chan_info *ncp, bool over_selection, XEN edpos, int arg_pos);


/* -------- snd-draw.c -------- */

point_t *get_grf_points(void);
point_t *get_grf_points1(void);
void draw_cursor(chan_info *cp);
void set_grf_points(int xi, int j, int ymin, int ymax);
void set_grf_point(int xi, int j, int yi);
void draw_grf_points(int dot_size, graphics_context *ax, int j, axis_info *ap, mus_float_t y0, graph_style_t graph_style);
void draw_both_grf_points(int dot_size, graphics_context *ax, int j, graph_style_t graph_style);
void g_init_draw(void);
void set_dialog_widget(snd_dialog_t which, widget_t wid);
void run_new_widget_hook(widget_t w);
bool foreground_color_ok(XEN color, graphics_context *ax);

#if HAVE_GL
  void sgl_save_currents(void);
  void sgl_set_currents(bool with_dialogs);
#endif

#if USE_GTK
  void recolor_everything(widget_t w, gpointer color);
#endif


/* -------- snd-ladspa.c -------- */
#if HAVE_LADSPA
void g_ladspa_to_snd(void);
#endif

#endif

