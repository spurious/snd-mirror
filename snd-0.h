#ifndef SND_0_H
#define SND_0_H

#if defined(__bsdi__) && (!HAVE_DECL_ISNAN)
  #define HAVE_DECL_ISNAN 1
#endif

#if HAVE_RUBY
  #undef _
#endif

#if ENABLE_NLS && HAVE_GETTEXT
  #include "gettext.h"
  #define _(String) gettext (String)
  #define gettext_noop(String) String
  #define N_(String) gettext_noop (String)
#else
  #define _(String) (String)
  #define N_(String) String
  #define textdomain(Domain)
  #define bindtextdomain(Package, Directory)
#endif

#if (!HAVE_FAM)
  #define FAMRequest int
  #define FAMEvent int
  #define FAMConnection void*
#endif

#define NO_SUCH_TRACK    XEN_ERROR_TYPE("no-such-track")
#define NO_SUCH_ENVELOPE XEN_ERROR_TYPE("no-such-envelope")
#define NO_SUCH_SAMPLE   XEN_ERROR_TYPE("no-such-sample")
#define NO_SUCH_EDIT     XEN_ERROR_TYPE("no-such-edit")
#define CANNOT_SAVE      XEN_ERROR_TYPE("cannot-save")
#define CANT_UPDATE_FILE XEN_ERROR_TYPE("cant-update-file")

#ifndef Float
  #define Float float
#endif

#ifndef STRFTIME_FORMAT
  #define STRFTIME_FORMAT "%a %d-%b-%Y %H:%M %Z"
#endif

#if HAVE_STRCASECMP
  #define STRCMP(a, b) strcasecmp(a, b)
  #define STRNCMP(a, b, c) strncasecmp(a, b, c)
#else
  #define STRCMP(a, b) strcmp(a, b)
  #define STRNCMP(a, b, c) strncmp(a, b, c)
#endif

#define XOR(a, b) ((a) ^ (b))
/* can't get used to this operator -- in the good old days, ^ meant exponentiation */

#define UNPACK_SOUND(a) (a >> 16)
#define UNPACK_CHANNEL(a) (a & 0xff)
#define PACK_SOUND_AND_CHANNEL(a, b) ((a << 16) | b)

#define FILE_BUFFER_SIZE 8192
/* this shouldn't be too much larger than the average un-peak-env'd time domain window size:
 *   as a read crosses the in-core boundary, it jumps ahead to the next windowful (everything
 *   goes through read_sample, and it doesn't have local knowledge of the caller's intentions).
 *   But that means a subsequent redisplay (normally upon mouse release) has to jump back to
 *   ax->losamp, so at worst each crossing can cost us 1/4 or thereabouts of FILE_BUFFER_SIZE
 *   extra samples read. Not a big deal, but...
 */
#define MAX_BUFFER_SIZE 65536
#define MIX_FILE_BUFFER_SIZE 2048
#define PRINT_BUFFER_SIZE 512
#define LABEL_BUFFER_SIZE 64
#define REPORTING_SIZE (MAX_BUFFER_SIZE * 8)
/* progress bar (hourglass icon) is displayed if more than this many samples are being processed */
#define NUM_HOURGLASSES 15
#define NUM_BOMBS 15

#define AMP_ENV_CUTOFF 50000

#define DEFAULT_OUTPUT_CHANS 1
#define DEFAULT_OUTPUT_SRATE 22050
#define DEFAULT_OUTPUT_HEADER_TYPE MUS_NEXT
#define DEFAULT_OUTPUT_DATA_FORMAT MUS_BFLOAT
/* mus-next is probably best here since intermediate/temp files can be any length (> 2^32 bytes)
 *   and the next header-specified size, although 32 bits, is explicitly "advisory".
 */

#define NO_COMPLETER -1
#define NO_SELECTION -1
#define NO_END_SPECIFIED -1
#define NO_MIX_TAG -1
#define INVALID_MIX_ID -1
#define INVALID_MIX_CHANNEL -2
#define SAVE_ALL_CHANS -1
#define INVALID_TRACK_ID -1
#define ANY_MIX_ID -2
#define ANY_TRACK_ID -2
#define NO_COLOR -1
#define NOT_A_GC_LOC -1
#define NOT_A_SOUND -1

#define MIX_FILE_NO_MIX -1
#define MIX_FILE_NO_FILE -2
#define MIX_FILE_NO_SP -3
#define MIX_FILE_NO_TEMP_FILE -4

#ifndef POPUP_BUTTON
  #define POPUP_BUTTON 3
#endif

typedef enum {SOUND_SAVE_AS, SELECTION_SAVE_AS, REGION_SAVE_AS} save_dialog_t;
typedef enum {NOT_IN_BACKGROUND, IN_BACKGROUND} play_process_t;
typedef enum {WITHOUT_VIRTUAL_CHANNELS, WITH_VIRTUAL_CHANNELS} virtual_channels_t;
typedef enum {WITH_FW_BUTTONS, WITH_ARROWS} fw_button_t;
typedef enum {WITHOUT_HOOK, WITH_HOOK} with_hook_t;
typedef enum {WITHOUT_WORD_WRAP, WITH_WORD_WRAP} with_word_wrap_t;
typedef enum {DRAG_ENTER, DRAG_LEAVE, DRAG_MOTION} drag_style_t;
typedef enum {ENVED_AMPLITUDE, ENVED_SPECTRUM, ENVED_SRATE} enved_target_t;
typedef enum {ENVELOPE_LINEAR, ENVELOPE_EXPONENTIAL} env_type_t;
typedef enum {GRAPH_LINES, GRAPH_DOTS, GRAPH_FILLED, GRAPH_DOTS_AND_LINES, GRAPH_LOLLIPOPS, NUM_GRAPH_STYLES} graph_style_t;
#define GRAPH_STYLE_OK(Grf) (Grf < NUM_GRAPH_STYLES)
typedef enum {APPLY_TO_SOUND, APPLY_TO_CHANNEL, APPLY_TO_SELECTION} snd_apply_t;
typedef enum {GRAPH_ONCE, GRAPH_AS_SONOGRAM, GRAPH_AS_SPECTROGRAM, GRAPH_AS_WAVOGRAM} graph_type_t;
typedef enum {ZOOM_FOCUS_LEFT, ZOOM_FOCUS_RIGHT, ZOOM_FOCUS_ACTIVE, ZOOM_FOCUS_MIDDLE, ZOOM_FOCUS_PROC} zoom_focus_t;
typedef enum {DONT_LOCK_MIXES, LOCK_MIXES} lock_mix_t;
typedef enum {DONT_DELETE_ME, DELETE_ME, ALREADY_DELETED, MULTICHANNEL_DELETION} file_delete_t;
typedef enum {SND_REOPEN_CLOSED_FILE, SND_OPEN_CHANNEL, SND_COPY_READER, SND_INSERT_FILE, SND_CHANGE_FILE, SND_OVERRIDE_FILE, SND_MIX_FILE} open_reason_t;
typedef enum {CURSOR_CROSS, CURSOR_LINE, CURSOR_PROC} cursor_style_t;
typedef enum {SHOW_NO_AXES, SHOW_ALL_AXES, SHOW_X_AXIS, SHOW_ALL_AXES_UNLABELLED, SHOW_X_AXIS_UNLABELLED, SHOW_BARE_X_AXIS, NUM_SHOW_AXES} show_axes_t;
typedef enum {DONT_NORMALIZE, NORMALIZE_BY_CHANNEL, NORMALIZE_BY_SOUND, NORMALIZE_GLOBALLY, NUM_TRANSFORM_NORMALIZATIONS} fft_normalize_t;
typedef enum {NO_DISK_SPACE, NOT_ENOUGH_DISK_SPACE, DISK_SPACE_OK} disk_space_t;
typedef enum {CHAN_GC, CHAN_IGC, CHAN_SELGC, CHAN_CGC, CHAN_MGC, CHAN_MXGC, CHAN_TMPGC} chan_gc_t;
typedef enum {NOT_A_VIEWER, FILE_VIEWER, REGION_VIEWER} file_viewer_t; /* 1 and 2, according to docs for mouse-enter-label-hook */
typedef enum {COLOR_DIALOG, ORIENTATION_DIALOG, ENVED_DIALOG, UNUSED_DIALOG, ANOTHER_UNUSED_DIALOG, TRANSFORM_DIALOG,
	      FILE_OPEN_DIALOG, SOUND_SAVE_AS_DIALOG, VIEW_FILES_DIALOG, RAW_DATA_DIALOG, NEW_FILE_DIALOG,
	      FILE_MIX_DIALOG, EDIT_HEADER_DIALOG, FIND_DIALOG, HELP_DIALOG, COMPLETION_DIALOG, MIX_DIALOG,
	      PRINT_DIALOG, RECORDER_DIALOG, REGION_DIALOG, POST_IT_DIALOG, TRACK_DIALOG, SELECTION_SAVE_AS_DIALOG,
              FILE_INSERT_DIALOG, REGION_SAVE_AS_DIALOG, PREFERENCES_DIALOG, NUM_DIALOGS} snd_dialog_t;
typedef enum {SOUND_IDLE, SOUND_NORMAL, SOUND_WRAPPER, SOUND_REGION, SOUND_READER} sound_inuse_t;
typedef enum {NOT_FILING, INPUT_FILING, SELECTION_FILING, CHANNEL_FILING, TEMP_FILING, CHANGE_FILING, 
	      INSERT_FILING, MACRO_FILING, DOIT_SELECTION_FILING, DOIT_CHANNEL_FILING, SAVE_EDITS_FILING} sp_filing_t;
typedef enum {IO_NO_ERROR, IO_SAVE_HOOK_CANCELLATION, IO_BAD_CHANNEL, IO_CANT_REOPEN_FILE, IO_TOO_MANY_OPEN_FILES, 
	      IO_UNKNOWN_SNDLIB_ERROR, IO_NO_MEMORY, IO_CANT_OPEN_FILE, IO_NO_FILENAME, IO_BAD_DATA_FORMAT, IO_BAD_HEADER_TYPE,
	      IO_SNDLIB_UNINITIALIZED, IO_NOT_A_SOUND_FILE, IO_FILE_CLOSED, IO_WRITE_ERROR, IO_INTERRUPTED, IO_CANT_CLOSE_FILE, 
	      IO_BAD_HEADER, IO_DISK_FULL, IO_WRITE_PROTECTED, IO_CANT_READ_SELECTION_FILE, IO_NEED_WRITE_CONFIRMATION, 
	      IO_NO_CHANGES, IO_EDIT_HOOK_CANCELLATION, IO_CANT_CREATE_FILE, IO_ERROR_NUM} io_error_t;

#define SERIOUS_IO_ERROR(Err) ((Err != IO_NO_ERROR) && \
                               (Err != IO_EDIT_HOOK_CANCELLATION) && \
                               (Err != IO_SAVE_HOOK_CANCELLATION) && \
                               (Err != IO_INTERRUPTED) && \
                               (Err != IO_NEED_WRITE_CONFIRMATION) && \
                               (Err != IO_NO_CHANGES))

#if MUS_DEBUGGING
  #define ASSERT_IO_ERROR(Err, Location) if (Err >= IO_ERROR_NUM) {fprintf(stderr, "%s: bogus io error: %d\n", Location, Err); abort();}
#else
  #define ASSERT_IO_ERROR(Err, Location)
#endif

typedef enum {SS_ANY_WATCHER, SS_FILE_OPEN_WATCHER} ss_watcher_t;
typedef enum {SS_FILE_OPENED, SS_FILE_CLOSED} ss_watcher_reason_t;
typedef enum {SP_ANY_WATCHER, SP_READ_ONLY_WATCHER, SP_REVERT_WATCHER} sp_watcher_t;
typedef enum {SP_READ_ONLY_CHANGED, SP_IS_CLOSING, SP_REVERTED} sp_watcher_reason_t;
typedef enum {SELECTION_INACTIVE, SELECTION_ACTIVE, SELECTION_CHANGED, SELECTION_IN_DOUBT} selection_watcher_reason_t;
typedef enum {WITH_READABLE_HEADERS, WITH_WRITABLE_HEADERS, WITH_BUILTIN_HEADERS} header_choice_t;
enum {SORT_A_TO_Z, SORT_Z_TO_A, SORT_NEW_TO_OLD, SORT_OLD_TO_NEW, SORT_SMALL_TO_BIG, SORT_BIG_TO_SMALL, SORT_XEN};

#define FORCE_REFFT true
#define DONT_FORCE_REFFT false
#define FORCE_REDISPLAY true
#define DONT_FORCE_REDISPLAY false
#define WITH_EVENTS true
#define WITHOUT_EVENTS false
#define NOT_IN_AS_ONE_EDIT false
#define IN_AS_ONE_EDIT true
#define FILE_READ_ONLY true
#define FILE_READ_WRITE false
#define FILE_SELECTED true
#define FILE_NOT_SELECTED false
#define EXIT_FORCED true
#define EXIT_NOT_FORCED false

#if HAVE_RUBY
  #define TO_PROC_NAME(Str) xen_scheme_procedure_to_ruby(Str)
  #define TO_VAR_NAME(Str) xen_scheme_constant_to_ruby(Str)
  #define PROC_OPEN "("
  #define PROC_CLOSE ")"
  #define PROC_SEP ", "
  #define LIST_OPEN "["
  #define LIST_CLOSE "]"
  #define BPAREN ""
  #define EPAREN ""
  #define PROC_FALSE "false"
  #define PROC_TRUE "true"
  #define PROC_QUOTE ""
#endif
#if HAVE_FORTH
  #define TO_PROC_NAME(Str) Str
  #define TO_VAR_NAME(Str) Str
  #define PROC_OPEN " "
  #define PROC_CLOSE " "
  #define PROC_SEP " "
  #define LIST_OPEN " '( "
  #define LIST_CLOSE " ) "
  #define BPAREN ""
  #define EPAREN " drop"
  #define PROC_FALSE "#f"
  #define PROC_TRUE  "#t"
  #define PROC_QUOTE ""
#endif
#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
  #define TO_PROC_NAME(Str) Str
  #define TO_VAR_NAME(Str) Str
  #define PROC_OPEN " "
  #define PROC_CLOSE ""
  #define PROC_SEP " "
  #define LIST_OPEN "(list "
  #define LIST_CLOSE ")"
  #define BPAREN "("
  #define EPAREN ")"
  #define PROC_FALSE "#f"
  #define PROC_TRUE  "#t"
  #define PROC_QUOTE "'"
#endif

#define MAX_MAIN_MENUS 64
#define NO_REGIONS -2
#define INVALID_REGION -1
#define NO_LIST -1

typedef enum {READ_FORWARD, READ_BACKWARD} read_direction_t;
typedef enum {DONT_TRACK, ALWAYS_TRACK, TRACK_ONCE} tracking_cursor_t;
typedef enum {DONT_UPDATE_DISPLAY, UPDATE_DISPLAY} cut_selection_regraph_t;
typedef enum {NOT_FROM_ENVED, FROM_ENVED} enved_progress_t;
typedef enum {SEARCH_OK, SEARCH_FAILED} search_result_t;
typedef enum {IGNORE_CACHE, REMOVE_FROM_CACHE} cache_remove_t;
typedef enum {FFT_UNCHANGED, FFT_CHANGED, FFT_CHANGE_LOCKED} fft_change_t;
typedef enum {WITHOUT_GRAPH, WITH_GRAPH, WITHOUT_INITIAL_GRAPH_HOOK} channel_graph_t;
typedef enum {NOT_PRINTING, PRINTING} printing_t;
typedef enum {NO_X_AXIS, WITH_X_AXIS} with_x_axis_t;
typedef enum {NO_GRID, WITH_GRID} with_grid_t;
typedef enum {WITH_LINEAR_AXES, WITH_LOG_X_AXIS, WITH_LOG_Y_AXIS} log_axis_t;

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define IS_PLAYER(snd) ({ snd_info *_snd_0_h_1 = snd; ((_snd_0_h_1) && (_snd_0_h_1->index < 0)); })
#else
  #define IS_PLAYER(snd) ((snd) && (snd->index < 0))
#endif

#define PLAYER(snd) (-(snd->index))
typedef enum {NO_PLAYERS, PLAYERS_OK} sp_sound_t;

#define OVER_SELECTION true
#define OVER_SOUND false

#define AT_CURRENT_EDIT_POSITION -1

typedef enum {X_AXIS_IN_SECONDS, X_AXIS_IN_SAMPLES, X_AXIS_AS_PERCENTAGE, X_AXIS_IN_BEATS, 
	      X_AXIS_IN_MEASURES, X_AXIS_AS_CLOCK, NUM_X_AXIS_STYLES} x_axis_style_t;
typedef enum {SPEED_CONTROL_AS_FLOAT, SPEED_CONTROL_AS_RATIO, SPEED_CONTROL_AS_SEMITONE, NUM_SPEED_CONTROL_STYLES} speed_style_t;
typedef enum {CURSOR_IN_VIEW, CURSOR_ON_LEFT, CURSOR_ON_RIGHT, CURSOR_IN_MIDDLE, KEYBOARD_NO_ACTION} kbd_cursor_t;
typedef enum {CHANNELS_SEPARATE, CHANNELS_COMBINED, CHANNELS_SUPERIMPOSED, NUM_CHANNEL_STYLES} channel_style_t;
typedef enum {FD_CLOSED, FD_OPEN} fd_open_t;
typedef enum {PRINT_SND, PRINT_ENV} print_choice_t;
typedef enum {SND_DATA_NO_DATA, SND_DATA_FILE, SND_DATA_BUFFER} snd_data_file_t;
typedef enum {SOUNDS_VERTICAL, SOUNDS_HORIZONTAL, SOUNDS_IN_NOTEBOOK, SOUNDS_IN_SEPARATE_WINDOWS} sound_style_t;
enum {FOURIER, WAVELET, WALSH, AUTOCORRELATION, CEPSTRUM, HAAR, NUM_BUILTIN_TRANSFORM_TYPES}; /* not typedef'd -- grows as new ones are added */
#define NUM_WAVELETS 48

typedef enum {FCP_X_ANGLE, FCP_X_SCALE, FCP_Y_ANGLE, FCP_Y_SCALE, FCP_Z_ANGLE, FCP_Z_SCALE, FCP_CUTOFF, FCP_START, FCP_ALPHA, FCP_BETA, FCP_BEATS} fcp_t;
typedef enum {TIME_AXIS_INFO, TRANSFORM_AXIS_INFO, LISP_AXIS_INFO} axis_info_t;
typedef enum {COLOR_POSITION, COLOR_ZOOM} slider_choice_t;
typedef enum {MINI_OFF, MINI_CURSOR, MINI_FIND, MINI_PROMPT, MINI_REPORT, MINI_USER} minibuffer_choice_t;

typedef enum {PLAY_COMPLETE, PLAY_CLOSE, PLAY_BUTTON_UNSET, PLAY_STOP_CALLED, PLAY_C_G, PLAY_NO_CHANNEL,
	      PLAY_ERROR, PLAY_APPLY, PLAY_EDIT, PLAY_C_T} play_stop_t;

typedef enum {NO_REQUESTOR, FROM_UPDATE, FROM_VIEW_FILES, FROM_DRAG_AND_DROP, FROM_OPEN_DIALOG,
	      FROM_RECORDER, FROM_KEYBOARD, FROM_STARTUP, FROM_REGION_EDIT, FROM_NEW_FILE_DIALOG, FROM_OPEN_SOUND, 
	      FROM_OPEN_RAW_SOUND, FROM_VIEW_SOUND, FROM_NEW_SOUND, FROM_RAW_DATA_DIALOG, FROM_MIX_DIALOG,
              FROM_INSERT_DIALOG, FROM_VIEW_FILES_MIX_DIALOG, FROM_VIEW_FILES_INSERT_DIALOG} open_requestor_t;

#if MUS_DEBUGGING
enum {PRINT_NOT, PRINT_CHAR, PRINT_CLM, PRINT_REGION, PRINT_SYNC, PRINT_SND_FD, PRINT_FAM_INFO, PRINT_SOUND_DATA};
#endif

#define DEFAULT_AMP_CONTROL 1.0
#define DEFAULT_CONTRAST_CONTROL 0.0
#define DEFAULT_EXPAND_CONTROL 1.0
#define DEFAULT_REVERB_CONTROL_LENGTH 1.0
#define DEFAULT_REVERB_CONTROL_SCALE 0.0
#define DEFAULT_SPEED_CONTROL 1.0
#define DEFAULT_CONTRAST_CONTROL_P false
#define DEFAULT_EXPAND_CONTROL_P false
#define DEFAULT_FILTER_CONTROL_P false
#define DEFAULT_REVERB_CONTROL_P false


#define filter_control_in_dB(ss) ss->Filter_Control_In_Db
#define in_set_filter_control_in_dB(ss, val) ss->Filter_Control_In_Db = val
#define DEFAULT_FILTER_CONTROL_IN_DB false

#define filter_control_in_hz(ss) ss->Filter_Control_In_Hz
#define in_set_filter_control_in_hz(ss, val) ss->Filter_Control_In_Hz = val
#define DEFAULT_FILTER_CONTROL_IN_HZ false

#define speed_control_tones(ss) ss->Speed_Control_Tones
#define in_set_speed_control_tones(ss, val) ss->Speed_Control_Tones = val
#define DEFAULT_SPEED_CONTROL_TONES 12

#define speed_control_style(ss) ss->Speed_Control_Style
#define in_set_speed_control_style(ss, val) ss->Speed_Control_Style = val
#define DEFAULT_SPEED_CONTROL_STYLE SPEED_CONTROL_AS_FLOAT

#define expand_control_length(ss) ss->Expand_Control_Length
#define in_set_expand_control_length(ss, val) ss->Expand_Control_Length = val
#define DEFAULT_EXPAND_CONTROL_LENGTH 0.15

#define expand_control_ramp(ss) ss->Expand_Control_Ramp
#define in_set_expand_control_ramp(ss, val) ss->Expand_Control_Ramp = val
#define DEFAULT_EXPAND_CONTROL_RAMP 0.4

#define expand_control_hop(ss) ss->Expand_Control_Hop
#define in_set_expand_control_hop(ss, val) ss->Expand_Control_Hop = val
#define DEFAULT_EXPAND_CONTROL_HOP 0.05

#define expand_control_jitter(ss) ss->Expand_Control_Jitter
#define in_set_expand_control_jitter(ss, val) ss->Expand_Control_Jitter = val
#define DEFAULT_EXPAND_CONTROL_JITTER 0.1

#define contrast_control_amp(ss) ss->Contrast_Control_Amp
#define in_set_contrast_control_amp(ss, val) ss->Contrast_Control_Amp = val
#define DEFAULT_CONTRAST_CONTROL_AMP 1.0

#define reverb_control_feedback(ss) ss->Reverb_Control_Feedback
#define in_set_reverb_control_feedback(ss, val) ss->Reverb_Control_Feedback = val
#define DEFAULT_REVERB_CONTROL_FEEDBACK 1.09

#define reverb_control_lowpass(ss) ss->Reverb_Control_Lowpass
#define in_set_reverb_control_lowpass(ss, val) ss->Reverb_Control_Lowpass = val
#define DEFAULT_REVERB_CONTROL_LOWPASS 0.7

#define reverb_control_decay(ss) ss->Reverb_Control_Decay
#define in_set_reverb_control_decay(ss, val) ss->Reverb_Control_Decay = val
#define DEFAULT_REVERB_CONTROL_DECAY 1.0

#define contrast_control_min(ss) ss->Contrast_Control_Min
#define in_set_contrast_control_min(ss, val) ss->Contrast_Control_Min = val
#define DEFAULT_CONTRAST_CONTROL_MIN 0.0

#define contrast_control_max(ss) ss->Contrast_Control_Max
#define in_set_contrast_control_max(ss, val) ss->Contrast_Control_Max = val
#define DEFAULT_CONTRAST_CONTROL_MAX 10.0

#define expand_control_min(ss) ss->Expand_Control_Min
#define in_set_expand_control_min(ss, val) ss->Expand_Control_Min = val
#define DEFAULT_EXPAND_CONTROL_MIN 0.05

#define expand_control_max(ss) ss->Expand_Control_Max
#define in_set_expand_control_max(ss, val) ss->Expand_Control_Max = val
#define DEFAULT_EXPAND_CONTROL_MAX 20.0

#define speed_control_min(ss) ss->Speed_Control_Min
#define in_set_speed_control_min(ss, val) ss->Speed_Control_Min = val
#define DEFAULT_SPEED_CONTROL_MIN 0.05

#define speed_control_max(ss) ss->Speed_Control_Max
#define in_set_speed_control_max(ss, val) ss->Speed_Control_Max = val
#define DEFAULT_SPEED_CONTROL_MAX 20.0

#define amp_control_min(ss) ss->Amp_Control_Min
#define in_set_amp_control_min(ss, val) ss->Amp_Control_Min = val
#define DEFAULT_AMP_CONTROL_MIN 0.0

#define amp_control_max(ss) ss->Amp_Control_Max
#define in_set_amp_control_max(ss, val) ss->Amp_Control_Max = val
#define DEFAULT_AMP_CONTROL_MAX 8.0

#define reverb_control_scale_min(ss) ss->Reverb_Control_Scale_Min
#define in_set_reverb_control_scale_min(ss, val) ss->Reverb_Control_Scale_Min = val
#define DEFAULT_REVERB_CONTROL_SCALE_MIN 0.0

#define reverb_control_scale_max(ss) ss->Reverb_Control_Scale_Max
#define in_set_reverb_control_scale_max(ss, val) ss->Reverb_Control_Scale_Max = val
#define DEFAULT_REVERB_CONTROL_SCALE_MAX 4.0

#define reverb_control_length_min(ss) ss->Reverb_Control_Length_Min
#define in_set_reverb_control_length_min(ss, val) ss->Reverb_Control_Length_Min = val
#define DEFAULT_REVERB_CONTROL_LENGTH_MIN 0.0

#define reverb_control_length_max(ss) ss->Reverb_Control_Length_Max
#define in_set_reverb_control_length_max(ss, val) ss->Reverb_Control_Length_Max = val
#define DEFAULT_REVERB_CONTROL_LENGTH_MAX 5.0

#define filter_control_order(ss) ss->Filter_Control_Order
#define in_set_filter_control_order(ss, val) ss->Filter_Control_Order = val
#define DEFAULT_FILTER_CONTROL_ORDER 20

#define tempo_control_min(ss) ss->Tempo_Control_Min
#define in_set_tempo_control_min(ss, val) ss->Tempo_Control_Min = val
#define DEFAULT_TEMPO_CONTROL_MIN 0.0

#define tempo_control_max(ss) ss->Tempo_Control_Max
#define in_set_tempo_control_max(ss, val) ss->Tempo_Control_Max = val
#define DEFAULT_TEMPO_CONTROL_MAX 8.0

#define in_show_controls(ss) ss->Show_Controls
#define in_set_show_controls(ss, val) ss->Show_Controls = val
#define DEFAULT_SHOW_CONTROLS false

#define with_tracking_cursor(ss) ss->With_Tracking_Cursor
#define in_set_with_tracking_cursor(ss, val) ss->With_Tracking_Cursor = val
#define DEFAULT_WITH_TRACKING_CURSOR DONT_TRACK

#define just_sounds(ss) ss->Just_Sounds
#define set_just_sounds(val) ss->Just_Sounds = val
#define DEFAULT_JUST_SOUNDS false

#define DEFAULT_SYNC 0
#define DEFAULT_INIT_WINDOW_X -1
#define DEFAULT_INIT_WINDOW_Y -1
#define DEFAULT_INIT_WINDOW_WIDTH 0
#define DEFAULT_INIT_WINDOW_HEIGHT 0

#define default_output_header_type(ss) ss->Default_Output_Header_Type
#define set_default_output_header_type(a) ss->Default_Output_Header_Type = a

#define default_output_chans(ss) ss->Default_Output_Chans
#define set_default_output_chans(a) ss->Default_Output_Chans = a

#define default_output_srate(ss) ss->Default_Output_Srate
#define set_default_output_srate(a) ss->Default_Output_Srate = a

#define default_output_data_format(ss) ss->Default_Output_Data_Format
#define set_default_output_data_format(a) ss->Default_Output_Data_Format = a

#define dac_size(ss) ss->Dac_Size
#define set_dac_size(a) ss->Dac_Size = a
#if (HAVE_OSS || HAVE_ALSA)
  #ifdef PPC
     /* actually linuxppc */
     #define DEFAULT_DAC_SIZE 0
  #else
     #define DEFAULT_DAC_SIZE 256
  #endif
#else
  #if MUS_MAC_OSX
    #define DEFAULT_DAC_SIZE 64
  #else
    #define DEFAULT_DAC_SIZE 1024
  #endif
#endif

#define dac_combines_channels(ss) ss->Dac_Combines_Channels
#define set_dac_combines_channels(a) ss->Dac_Combines_Channels = a
#define DEFAULT_DAC_COMBINES_CHANNELS true

#define max_regions(ss) ss->Max_Regions
#define in_set_max_regions(a) ss->Max_Regions = a
#define DEFAULT_MAX_REGIONS 16

#define max_transform_peaks(ss) ss->Max_Transform_Peaks
#define in_set_max_transform_peaks(a) ss->Max_Transform_Peaks = a
#define DEFAULT_MAX_TRANSFORM_PEAKS 100

#define auto_resize(ss) ss->Auto_Resize
#define set_auto_resize(a) ss->Auto_Resize = a
#define DEFAULT_AUTO_RESIZE true

#define auto_update(ss) ss->Auto_Update
#define set_auto_update(a) ss->Auto_Update = a
#define DEFAULT_AUTO_UPDATE false

#define auto_update_interval(ss) ss->Auto_Update_Interval
#define set_auto_update_interval(a) ss->Auto_Update_Interval = a
#define DEFAULT_AUTO_UPDATE_INTERVAL 60.0

#define cursor_update_interval(ss) ss->Cursor_Update_Interval
#define set_cursor_update_interval(a) ss->Cursor_Update_Interval = a
#define DEFAULT_CURSOR_UPDATE_INTERVAL 0.05

#define cursor_location_offset(ss) ss->Cursor_Location_Offset
#define set_cursor_location_offset(a) ss->Cursor_Location_Offset = a
#define DEFAULT_CURSOR_LOCATION_OFFSET 0

#define color_cutoff(ss) ss->Color_Cutoff
#define in_set_color_cutoff(a) ss->Color_Cutoff = a
#define DEFAULT_COLOR_CUTOFF 0.003

#define color_inverted(ss) ss->Color_Inverted
#define in_set_color_inverted(a) ss->Color_Inverted = a
#define DEFAULT_COLOR_INVERTED true

#define color_scale(ss) ss->Color_Scale
#define in_set_color_scale(a) ss->Color_Scale = a
#define DEFAULT_COLOR_SCALE 1.0

#define fft_window_alpha(ss) ss->Fft_Window_Alpha
#define in_set_fft_window_alpha(a) ss->Fft_Window_Alpha = a
#define DEFAULT_FFT_WINDOW_ALPHA 0.0

#define fft_window_beta(ss) ss->Fft_Window_Beta
#define in_set_fft_window_beta(a) ss->Fft_Window_Beta = a
#define DEFAULT_FFT_WINDOW_BETA 0.0

#define transform_size(ss) ss->Transform_Size
#define in_set_transform_size(a) ss->Transform_Size = a
#define DEFAULT_TRANSFORM_SIZE 512

#define transform_graph_type(ss) ss->Transform_Graph_Type
#define in_set_transform_graph_type_1(a) ss->Transform_Graph_Type = a
#define DEFAULT_TRANSFORM_GRAPH_TYPE GRAPH_ONCE

#define time_graph_type(ss) ss->Time_Graph_Type
#define in_set_time_graph_type(a) ss->Time_Graph_Type = a
#define DEFAULT_TIME_GRAPH_TYPE GRAPH_ONCE

#define fft_window(ss) ss->Fft_Window
#define in_set_fft_window_1(a) ss->Fft_Window = a
#define DEFAULT_FFT_WINDOW MUS_BLACKMAN2_WINDOW

#define trap_segfault(ss) ss->Trap_Segfault
#define set_trap_segfault(a) ss->Trap_Segfault = a
#define DEFAULT_TRAP_SEGFAULT true

#define log_freq_start(ss) ss->Log_Freq_Start
#define in_set_log_freq_start(a) ss->Log_Freq_Start = a
#define DEFAULT_LOG_FREQ_START 32.0

#define optimization(ss) ss->Optimization
#define set_optimization(a) ss->Optimization = a
#define DEFAULT_OPTIMIZATION 0

#define dot_size(ss) ss->Dot_Size
#define in_set_dot_size(a) ss->Dot_Size = a
#define DEFAULT_DOT_SIZE 1
#define MIN_DOT_SIZE 0
#define MAX_DOT_SIZE 100

#define grid_density(ss) ss->Grid_Density
#define in_set_grid_density(a) ss->Grid_Density = a
#define DEFAULT_GRID_DENSITY 1.0

#define minibuffer_history_length(ss) ss->Minibuffer_History_Length
#define set_minibuffer_history_length(a) ss->Minibuffer_History_Length = a
#define DEFAULT_MINIBUFFER_HISTORY_LENGTH 8

#define transform_normalization(ss) ss->Transform_Normalization
#define in_set_transform_normalization(a) ss->Transform_Normalization = a
#define DEFAULT_TRANSFORM_NORMALIZATION NORMALIZE_BY_CHANNEL

#define ask_before_overwrite(ss) ss->Ask_Before_Overwrite
#define set_ask_before_overwrite(a) ss->Ask_Before_Overwrite = a
#define DEFAULT_ASK_BEFORE_OVERWRITE false

#define spectro_cutoff(ss) ss->Spectro_Cutoff
#define in_set_spectro_cutoff(a) ss->Spectro_Cutoff = a
#define DEFAULT_SPECTRO_CUTOFF 1.0

#define spectro_start(ss) ss->Spectro_Start
#define in_set_spectro_start(a) ss->Spectro_Start = a
#define DEFAULT_SPECTRO_START 0.0

#define spectro_x_angle(ss) ss->Spectro_X_Angle
#define in_set_spectro_x_angle(a) ss->Spectro_X_Angle = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_X_ANGLE 300.0
#else
  #define DEFAULT_SPECTRO_X_ANGLE 90.0
#endif

#define spectro_y_angle(ss) ss->Spectro_Y_Angle
#define in_set_spectro_y_angle(a) ss->Spectro_Y_Angle = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_Y_ANGLE 320.0
#else
  #define DEFAULT_SPECTRO_Y_ANGLE 0.0
#endif

#define spectro_z_angle(ss) ss->Spectro_Z_Angle
#define in_set_spectro_z_angle(a) ss->Spectro_Z_Angle = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_Z_ANGLE 0.0
#else
  #define DEFAULT_SPECTRO_Z_ANGLE 358.0
#endif

#define spectro_x_scale(ss) ss->Spectro_X_Scale
#define in_set_spectro_x_scale(a) ss->Spectro_X_Scale = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_X_SCALE 1.5
  #define SPECTRO_X_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_X_SCALE 1.0
  #define SPECTRO_X_SCALE_MAX 2.0
#endif

#define spectro_y_scale(ss) ss->Spectro_Y_Scale
#define in_set_spectro_y_scale(a) ss->Spectro_Y_Scale = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_Y_SCALE 1.0
  #define SPECTRO_Y_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_Y_SCALE 1.0
  #define SPECTRO_Y_SCALE_MAX 2.0
#endif

#define spectro_z_scale(ss) ss->Spectro_Z_Scale
#define in_set_spectro_z_scale(a) ss->Spectro_Z_Scale = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_Z_SCALE 1.0
  #define SPECTRO_Z_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_Z_SCALE 0.1
  #define SPECTRO_Z_SCALE_MAX 1.0
#endif

#define spectro_hop(ss) ss->Spectro_Hop
#define in_set_spectro_hop(a) ss->Spectro_Hop = a
#define DEFAULT_SPECTRO_HOP 4

#define color_map(ss) ss->Color_Map
#define in_set_color_map(a) ss->Color_Map = a
#if HAVE_GL
  #define DEFAULT_COLOR_MAP 2
#else
  #define DEFAULT_COLOR_MAP 0
#endif

#define color_map_size(ss) ss->Color_Map_Size
#define set_color_map_size(a) ss->Color_Map_Size = a
#define DEFAULT_COLOR_MAP_SIZE 512

#define graph_style(ss) ss->Graph_Style
#define in_set_graph_style(a) ss->Graph_Style = a
#define DEFAULT_GRAPH_STYLE GRAPH_LINES

#define region_graph_style(ss) ss->Region_Graph_Style
#define set_region_graph_style(a) ss->Region_Graph_Style = a

#define sinc_width(ss) ss->Sinc_Width
#define set_sinc_width(a) ss->Sinc_Width = a
#define DEFAULT_SINC_WIDTH 10

#define verbose_cursor(ss) ss->Verbose_Cursor
#define in_set_verbose_cursor(a) ss->Verbose_Cursor = a
#define DEFAULT_VERBOSE_CURSOR false

#define selection_creates_region(ss) ss->Selection_Creates_Region
#define set_selection_creates_region(a) ss->Selection_Creates_Region = a
#define DEFAULT_SELECTION_CREATES_REGION true

#define zoom_focus_style(ss) ss->Zoom_Focus_Style
#define set_zoom_focus_style(a) ss->Zoom_Focus_Style = a
#define DEFAULT_ZOOM_FOCUS_STYLE ZOOM_FOCUS_ACTIVE

#define eps_file(ss) ss->Eps_File
#define set_eps_file(a) ss->Eps_File = a
#define DEFAULT_EPS_FILE "snd.eps"

#define eps_left_margin(ss) ss->Eps_Left_Margin
#define set_eps_left_margin(a) ss->Eps_Left_Margin = a
#define DEFAULT_EPS_LEFT_MARGIN 0.0

#define eps_bottom_margin(ss) ss->Eps_Bottom_Margin
#define set_eps_bottom_margin(a) ss->Eps_Bottom_Margin = a
#define DEFAULT_EPS_BOTTOM_MARGIN 0.0

#define eps_size(ss) ss->Eps_Size
#define set_eps_size(a) ss->Eps_Size = a
#define DEFAULT_EPS_SIZE 1.0

#define tiny_font(ss) ss->Tiny_Font
#define in_set_tiny_font(a) ss->Tiny_Font = a

#define peaks_font(ss) ss->Peaks_Font
#define in_set_peaks_font(a) ss->Peaks_Font = a

#define bold_peaks_font(ss) ss->Bold_Peaks_Font
#define in_set_bold_peaks_font(a) ss->Bold_Peaks_Font = a

#define axis_label_font(ss) ss->Axis_Label_Font
#define in_set_axis_label_font(a) ss->Axis_Label_Font = a

#define axis_numbers_font(ss) ss->Axis_Numbers_Font
#define in_set_axis_numbers_font(a) ss->Axis_Numbers_Font = a

#define listener_font(ss) ss->Listener_Font
#define in_set_listener_font(a) ss->Listener_Font = a

#define save_state_file(ss) ss->Save_State_File
#define in_set_save_state_file(a) ss->Save_State_File = a
#define DEFAULT_SAVE_STATE_FILE "saved-snd." XEN_FILE_EXTENSION

#define temp_dir(ss) ss->Temp_Dir
#define set_temp_dir(a) ss->Temp_Dir = a
#ifndef MUS_DEFAULT_TEMP_DIR
  #define MUS_DEFAULT_TEMP_DIR NULL
#endif

#define save_dir(ss) ss->Save_Dir
#define set_save_dir(a) ss->Save_Dir = a
#ifndef MUS_DEFAULT_SAVE_DIR
  #define MUS_DEFAULT_SAVE_DIR NULL
#endif

#define ladspa_dir(ss) ss->Ladspa_Dir
#define set_ladspa_dir(a) ss->Ladspa_Dir = a
#ifndef DEFAULT_LADSPA_DIR
  #define DEFAULT_LADSPA_DIR NULL
#endif

#define vu_size(ss) ss->Vu_Size
#define set_vu_size(a) ss->Vu_Size = a
#define DEFAULT_VU_SIZE 1.0

#define vu_in_dB(ss) ss->Vu_In_dB
#define in_set_vu_in_dB(a) ss->Vu_In_dB = a
#define DEFAULT_VU_IN_DB false

#define wavelet_type(ss) ss->Wavelet_Type
#define in_set_wavelet_type(a) ss->Wavelet_Type = a
#define DEFAULT_WAVELET_TYPE 0

#define transform_type(ss) ss->Transform_Type
#define in_set_transform_type(a) ss->Transform_Type = a
#define DEFAULT_TRANSFORM_TYPE FOURIER

#define show_selection_transform(ss) ss->Show_Selection_Transform
#define in_set_show_selection_transform(a) ss->Show_Selection_Transform = a
#define DEFAULT_SHOW_SELECTION_TRANSFORM false

#define with_mix_tags(ss) ss->With_Mix_Tags
#define set_with_mix_tags(a) ss->With_Mix_Tags = a
#if USE_NO_GUI
  #define DEFAULT_WITH_MIX_TAGS false
#else
  #define DEFAULT_WITH_MIX_TAGS true
#endif

#define with_relative_panes(ss) ss->With_Relative_Panes
#define set_with_relative_panes(a) ss->With_Relative_Panes = a
#define DEFAULT_WITH_RELATIVE_PANES true

#define with_gl(ss) ss->With_GL
#define in_set_with_gl(a) ss->With_GL = a
#if HAVE_GL
  #define DEFAULT_WITH_GL true
#else
  #define DEFAULT_WITH_GL false
#endif

#define with_background_processes(ss) ss->With_Background_Processes
#define set_with_background_processes(a) ss->With_Background_Processes = a
#define DEFAULT_WITH_BACKGROUND_PROCESSES true

#define with_file_monitor(ss) ss->With_File_Monitor
#define set_with_file_monitor(a) ss->With_File_Monitor = a
#define DEFAULT_WITH_FILE_MONITOR true

#define wavo_hop(ss) ss->Wavo_Hop
#define in_set_wavo_hop(a) ss->Wavo_Hop = a
#define DEFAULT_WAVO_HOP 3

#define wavo_trace(ss) ss->Wavo_Trace
#define in_set_wavo_trace(a) ss->Wavo_Trace = a
#define DEFAULT_WAVO_TRACE 64

#define x_axis_style(ss) ss->X_Axis_Style
#define in_set_x_axis_style(a) ss->X_Axis_Style = a
#define DEFAULT_X_AXIS_STYLE X_AXIS_IN_SECONDS

#define beats_per_minute(ss) ss->Beats_Per_Minute
#define in_set_beats_per_minute(a) ss->Beats_Per_Minute = a
#define DEFAULT_BEATS_PER_MINUTE 60.0

#define beats_per_measure(ss) ss->Beats_Per_Measure
#define in_set_beats_per_measure(a) ss->Beats_Per_Measure = a
#define DEFAULT_BEATS_PER_MEASURE 4

#define zero_pad(ss) ss->Zero_Pad
#define in_set_zero_pad(a) ss->Zero_Pad = a
#define DEFAULT_ZERO_PAD 0
#define MAX_ZERO_PAD 1000

#define show_transform_peaks(ss) ss->Show_Transform_Peaks
#define in_set_show_transform_peaks(a) ss->Show_Transform_Peaks = a
#define DEFAULT_SHOW_TRANSFORM_PEAKS false

#define show_indices(ss) ss->Show_Indices
#define set_show_indices(a) ss->Show_Indices = a
#define DEFAULT_SHOW_INDICES false

#define show_backtrace(ss) ss->Show_Backtrace
#define set_show_backtrace(a) ss->Show_Backtrace = a
#define DEFAULT_SHOW_BACKTRACE false

#define show_y_zero(ss) ss->Show_Y_Zero
#define in_set_show_y_zero(a) ss->Show_Y_Zero = a
#define DEFAULT_SHOW_Y_ZERO false

#define show_grid(ss) ss->Show_Grid
#define in_set_show_grid(a) ss->Show_Grid = a
#define DEFAULT_SHOW_GRID NO_GRID

#define show_sonogram_cursor(ss) ss->Show_Sonogram_Cursor
#define in_set_show_sonogram_cursor(a) ss->Show_Sonogram_Cursor = a
#define DEFAULT_SHOW_SONOGRAM_CURSOR false

#define show_axes(ss) ss->Show_Axes
#define in_set_show_axes(a) ss->Show_Axes = a
#define DEFAULT_SHOW_AXES SHOW_ALL_AXES

#define show_mix_waveforms(ss) ss->Show_Mix_Waveforms
#define in_set_show_mix_waveforms(a) ss->Show_Mix_Waveforms = a
#define DEFAULT_SHOW_MIX_WAVEFORMS true

#define mix_waveform_height(ss) ss->Mix_Waveform_Height
#define in_set_mix_waveform_height(a) ss->Mix_Waveform_Height = a
#define DEFAULT_MIX_WAVEFORM_HEIGHT 20

#define show_marks(ss) ss->Show_Marks
#define in_set_show_marks(a) ss->Show_Marks = a
#define DEFAULT_SHOW_MARKS true

#define fft_log_magnitude(ss) ss->Fft_Log_Magnitude
#define in_set_fft_log_magnitude(a) ss->Fft_Log_Magnitude = a
#define DEFAULT_FFT_LOG_MAGNITUDE false

#define fft_log_frequency(ss) ss->Fft_Log_Frequency
#define in_set_fft_log_frequency(a) ss->Fft_Log_Frequency = a
#define DEFAULT_FFT_LOG_FREQUENCY false

#define cursor_style(ss) ss->Cursor_Style
#define in_set_cursor_style(a) ss->Cursor_Style = a
#define DEFAULT_CURSOR_STYLE CURSOR_CROSS

#define tracking_cursor_style(ss) ss->Tracking_Cursor_Style
#define in_set_tracking_cursor_style(a) ss->Tracking_Cursor_Style = a
#define DEFAULT_TRACKING_CURSOR_STYLE CURSOR_CROSS

#define cursor_size(ss) ss->Cursor_Size
#define in_set_cursor_size(a) ss->Cursor_Size = a
#define DEFAULT_CURSOR_SIZE 15

#define channel_style(ss) ss->Channel_Style
#define in_set_channel_style(a) ss->Channel_Style = a
#define DEFAULT_CHANNEL_STYLE CHANNELS_COMBINED

#define sound_style(ss) ss->Sound_Style
#define set_sound_style(a) ss->Sound_Style = a
#define DEFAULT_SOUND_STYLE SOUNDS_VERTICAL

#define listener_prompt(ss) ss->Listener_Prompt
#define in_set_listener_prompt(a) ss->Listener_Prompt = a
#define DEFAULT_LISTENER_PROMPT ">"

#define print_length(ss) ss->Print_Length
#define set_print_length(a) ss->Print_Length = a
#define DEFAULT_PRINT_LENGTH 12

#define view_files_sort(ss) ss->View_Files_Sort
#define set_view_files_sort(a) ss->View_Files_Sort = a
#define DEFAULT_VIEW_FILES_SORT SORT_A_TO_Z

#define enved_clip_p(ss) ss->enved->clip_p
#define in_set_enved_clip_p(a) ss->enved->clip_p = a
#define DEFAULT_ENVED_CLIP_P false

#define enved_wave_p(ss) ss->Enved_Wave_p
#define in_set_enved_wave_p(a) ss->Enved_Wave_p = a
#define DEFAULT_ENVED_WAVE_P false

#define enved_filter_order(ss) ss->Enved_Filter_Order
#define in_set_enved_filter_order(a) ss->Enved_Filter_Order = a
#define DEFAULT_ENVED_FILTER_ORDER 40

#define enved_in_dB(ss) ss->enved->in_dB
#define in_set_enved_in_dB(a) ss->enved->in_dB = a
#define DEFAULT_ENVED_IN_DB false

#define enved_target(ss) ss->Enved_Target
#define in_set_enved_target(a) ss->Enved_Target = a
#define DEFAULT_ENVED_TARGET ENVED_AMPLITUDE

#define enved_base(ss) ss->Enved_Base
#define in_set_enved_base(a) ss->Enved_Base = a
#define DEFAULT_ENVED_BASE 1.0

#define enved_power(ss) ss->Enved_Power
#define set_enved_power(a) ss->Enved_Power = a
#define DEFAULT_ENVED_POWER 3.0

#define enved_style(ss) ss->Enved_Style
#define set_enved_style(a) ss->Enved_Style = a
#define DEFAULT_ENVED_STYLE ENVELOPE_LINEAR

#define audio_output_device(ss) ss->Audio_Output_Device
#define set_audio_output_device(a) ss->Audio_Output_Device = a
#define DEFAULT_AUDIO_OUTPUT_DEVICE MUS_AUDIO_DEFAULT

#define audio_input_device(ss) ss->Audio_Input_Device
#define set_audio_input_device(a) ss->Audio_Input_Device = a
#define DEFAULT_AUDIO_INPUT_DEVICE MUS_AUDIO_DEFAULT

#define in_graph_cursor(ss) ss->Graph_Cursor

#define clipping(ss) ss->Clipping
#define set_clipping(a) ss->Clipping = a
#define DEFAULT_CLIPPING false

#define html_dir(ss) ss->HTML_Dir
#define set_html_dir_1(a) ss->HTML_Dir = a
#define DEFAULT_HTML_DIR "."

#define html_program(ss) ss->HTML_Program
#define set_html_program(a) ss->HTML_Program = a
#define DEFAULT_HTML_PROGRAM "mozilla"

#define graphs_horizontal(ss) ss->Graphs_Horizontal
#define in_set_graphs_horizontal(a) ss->Graphs_Horizontal = a
#define DEFAULT_GRAPHS_HORIZONTAL true

#define mix_tag_width(ss) ss->Mix_Tag_Width
#define set_mix_tag_width(a) ss->Mix_Tag_Width = a
#define DEFAULT_MIX_TAG_WIDTH 6

#define mix_tag_height(ss) ss->Mix_Tag_Height
#define set_mix_tag_height(a) ss->Mix_Tag_Height = a
#define DEFAULT_MIX_TAG_HEIGHT 14

#define mark_tag_width(ss) ss->Mark_Tag_Width
#define set_mark_tag_width(a) ss->Mark_Tag_Width = a
#define DEFAULT_MARK_TAG_WIDTH 10

#define mark_tag_height(ss) ss->Mark_Tag_Height
#define set_mark_tag_height(a) ss->Mark_Tag_Height = a
#define DEFAULT_MARK_TAG_HEIGHT 4

#define min_dB(ss) ss->Min_dB
#define set_min_dB(a) ss->Min_dB = a
#define DEFAULT_MIN_DB -60.0

#endif
