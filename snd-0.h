#ifndef SND_0_H
#define SND_0_H

#if (!defined(HAVE_CONFIG_H))
  /* we need statfs -- need its header and how many args it takes */
  #if defined(SGI) || defined(SCO5) || defined(UW2) || defined(SUN)
    #define HAVE_SYS_STATFS_H 1
    #define STATFS_ARGS 4
  #else
    #if defined(HPUX) || defined(LINUX)
      #define HAVE_SYS_VFS_H 1
      #define STATFS_ARGS 2
    #else
      #if (defined(ALPHA) || defined(__APPLE__) || defined(__bsdi__))
        #define HAVE_SYS_MOUNT_H 1
        #define STATFS_ARGS 2
      #else
        #if defined(WINDOZE) && defined(__CYGWIN__)
          #define HAVE_SYS_VFS_H 1
          #define STATFS_ARGS 2
        #endif
      #endif
    #endif
  #endif
  #define RETSIGTYPE void
  #ifdef __CYGWIN__
    #define CLOSEDIR_VOID 1
    /* apparently we can't trust the return value */
  #endif
  #define HAVE_VPRINTF 1
  #define HAVE_LOCALE_H 1
  #define HAVE_SETLOCALE 1
  #define HAVE_MEMMOVE 1
  #define HAVE_STRINGIZE 1
  #define HAVE_LSTAT 1
  #define HAVE_HYPOT 1
  #define HAVE_STRDUP 1
  #define HAVE_FILENO 1
  #if defined(LINUX) && (!(defined(HAVE_SCHED_H)))
    #define HAVE_SCHED_H 1
  #endif
  #if defined(LINUX) && (!(defined(HAVE_SETJMP_H)))
    #define HAVE_SETJMP_H 1
  #endif
  #ifdef SGI
    #define HAVE_SYS_FPU_H 1
  #endif
  #ifdef LINUX
    #define HAVE_VSNPRINTF 1
    #define HAVE_SNPRINTF 1
  #endif
  #ifndef _MSC_VER
    #ifndef TRAP_SEGFAULT
      #define TRAP_SEGFAULT 1
    #endif
    #define HAVE_READLINK 1
    #define HAVE_ACCESS 1
    #define HAVE_OPENDIR 1
    #define HAVE_SLEEP 1
    #define HAVE_SIGNAL 1
    #define HAVE_STATFS 1
    #define HAVE_DIRENT_H 1
    #define HAVE_STRFTIME 1
    #define HAVE_CLOCK 1
  #endif
  #if defined(LINUX) || defined(__bsdi__)
    #define HAVE_ISNAN 1
  #endif
  #define HAVE_X 1
#endif

#if defined(__bsdi__) && (!HAVE_ISNAN)
  #define HAVE_ISNAN 1
#endif

#ifndef STATFS_ARGS
  #define STATFS_ARGS 2
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

#ifdef PRId64
/* this is needed because guile's libguile.h->tags.h->inttypes.h picks up
   a version of PRId64 that doesn't work with gettext as advertised.
*/
  #undef PRId64
#endif
#if (defined(SIZEOF_OFF_T) && (SIZEOF_OFF_T > 4)) || (defined(_FILE_OFFSET_BITS) && (_FILE_OFFSET_BITS == 64))
  #define PRId64 "%lld"
#else
  #define PRId64 "%d"
#endif

#define NO_SUCH_SOUND    XEN_ERROR_TYPE("no-such-sound")
#define NO_SUCH_MIX      XEN_ERROR_TYPE("no-such-mix")
#define NO_SUCH_REGION   XEN_ERROR_TYPE("no-such-region")
#define NO_SUCH_ENVELOPE XEN_ERROR_TYPE("no-such-envelope")
#define NO_SUCH_SAMPLE   XEN_ERROR_TYPE("no-such-sample")
#define NO_SUCH_EDIT     XEN_ERROR_TYPE("no-such-edit")
#define CANNOT_SAVE      XEN_ERROR_TYPE("cannot-save")

#ifndef Float
  #define Float float
#endif

#define SND_INIT_FILE_ENVIRONMENT_NAME "SND_INIT_FILE"
#ifndef WINDOZE
  #define INIT_FILE_NAME "~/.snd"
#else
  #define INIT_FILE_NAME "snd-init"
#endif

#if HAVE_RUBY
  #define S_setB "set_"
#else
  #define S_setB "set! "
#endif

#ifndef STRFTIME_FORMAT
#define STRFTIME_FORMAT "%a %d-%b-%Y %H:%M %Z"
#endif

#define STR_decimal '.'
/* defines the decimal point character used where I'm making float representations by hand */
/* if nl_langinfo is available, its decimal point will override this */

#define XOR(a, b) ((a) ^ (b))

#define UNPACK_SOUND(a) (a >> 16)
#define UNPACK_CHANNEL(a) (a & 0xff)
#define PACK_SOUND_AND_CHANNEL(a, b) ((a << 16) | b)

#define FILE_BUFFER_SIZE 8192
#define MAX_BUFFER_SIZE 65536
#define MIX_FILE_BUFFER_SIZE 2048
#define TIME_STR_SIZE 64
#define PRINT_BUFFER_SIZE 512
#define LABEL_BUFFER_SIZE 64
#define REPORTING_SIZE (FILE_BUFFER_SIZE * 20)

#define AMP_ENV_CUTOFF 50000

#define FILE_SAVE_AS 1
#define EDIT_SAVE_AS 2

#define DEFAULT_OUTPUT_CHANS 1
#define DEFAULT_OUTPUT_SRATE 22050
#define DEFAULT_OUTPUT_TYPE MUS_NEXT
#define DEFAULT_OUTPUT_FORMAT MUS_BSHORT

#define NO_COMPLETER -1
#define NO_SELECTION -1
#define NO_END_SPECIFIED -1
#define NO_MIX_TAG -1
#define INVALID_MIX_ID -1
#define INVALID_MIX_CHANNEL -2
#define SAVE_ALL_CHANS -1

#define NOT_IN_BACKGROUND 0
#define IN_BACKGROUND 1

#define WITHOUT_VIRTUAL_CHANNELS 0
#define WITH_VIRTUAL_CHANNELS 1

#define WITH_ARROWS 1
#define WITH_FW_BUTTONS 0

#ifndef POPUP_BUTTON
  #define POPUP_BUTTON 3
#endif

typedef enum {ENVED_AMPLITUDE, ENVED_SPECTRUM, ENVED_SRATE} enved_target_t;
typedef enum {GRAPH_LINES, GRAPH_DOTS, GRAPH_FILLED, GRAPH_DOTS_AND_LINES, GRAPH_LOLLIPOPS} graph_style_t;
#define GRAPH_STYLE_OK(Grf) ((Grf >= GRAPH_LINES) && (Grf <= GRAPH_LOLLIPOPS))
 /* using lollipop rather than the suggested popsicle because 
  *   popsicle is a (capitalized) trade-name in the USA and I want lowercase,
  *   lollipop is about 200 years older as an English word (1724 vs 1923),
  *   and I think lollipop is more accurate -- in my experience lollipops have
  *     circular candies whereas popsicles have a sort of squared-off bottom.
  */
typedef enum {GRAPH_ONCE, GRAPH_AS_SONOGRAM, GRAPH_AS_SPECTROGRAM, GRAPH_AS_WAVOGRAM} graph_type_t;
typedef enum {ZOOM_FOCUS_LEFT, ZOOM_FOCUS_RIGHT, ZOOM_FOCUS_ACTIVE, ZOOM_FOCUS_MIDDLE} zoom_focus_t;
enum {DONT_LOCK_MIXES, LOCK_MIXES};
typedef enum {DONT_DELETE_ME, DELETE_ME, ALREADY_DELETED, MULTICHANNEL_DELETION} file_delete_t;
/* TODO: enum typedefs, rest of bools */
typedef enum {SND_REOPEN_CLOSED_FILE, SND_OPEN_CHANNEL, SND_COPY_READER, SND_INSERT_FILE, SND_CHANGE_FILE, SND_OVERRIDE_FILE, SND_MIX_FILE} open_reason_t;
typedef enum {CURSOR_CROSS, CURSOR_LINE, CURSOR_PROC} cursor_style_t;
typedef enum {SHOW_NO_AXES, SHOW_ALL_AXES, SHOW_X_AXIS} show_axes_t;
typedef enum {DONT_NORMALIZE, NORMALIZE_BY_CHANNEL, NORMALIZE_BY_SOUND, NORMALIZE_GLOBALLY} fft_normalize_t;
typedef enum {NO_PROBLEM, BLIND_LEAP, GIVE_UP, HUNKER_DOWN} disk_space_t;
typedef enum {ENVED_ADD_POINT,ENVED_DELETE_POINT,ENVED_MOVE_POINT} enved_point_t;
typedef enum {CHAN_GC, CHAN_IGC, CHAN_SELGC, CHAN_CGC, CHAN_MGC, CHAN_MXGC, CHAN_TMPGC, CHAN_SELMXGC} chan_gc_t;
#define MAX_MAIN_MENUS 64

typedef enum {CURRENT_FILE_VIEWER, PREVIOUS_FILE_VIEWER, REGION_VIEWER} file_viewer_t;
typedef enum {COLOR_DIALOG, ORIENTATION_DIALOG, ENVED_DIALOG, ERROR_DIALOG, YES_OR_NO_DIALOG, TRANSFORM_DIALOG,
	      FILE_OPEN_DIALOG, FILE_SAVE_AS_DIALOG, VIEW_FILES_DIALOG, RAW_DATA_DIALOG, NEW_FILE_DIALOG,
	      FILE_MIX_DIALOG, EDIT_HEADER_DIALOG, FIND_DIALOG, HELP_DIALOG, COMPLETION_DIALOG, MIX_PANEL_DIALOG,
	      PRINT_DIALOG, RECORDER_DIALOG, REGION_DIALOG} snd_dialog_t;
#define NUM_DIALOGS 20

typedef enum {SOUND_IDLE, SOUND_NORMAL, SOUND_WRAPPER, SOUND_REGION, SOUND_READER} sound_inuse_t;

#define NO_REGIONS -2
#define INVALID_REGION -1

#define READ_FORWARD 1
#define READ_BACKWARD -1

#define FOLLOW_ALWAYS 1
#define FOLLOW_ONCE 2
#define DONT_FOLLOW 0

#define UPDATE_DISPLAY 1
#define DONT_UPDATE_DISPLAY 0

#define NOT_FROM_ENVED 0
#define FROM_ENVED 1

#define SEARCH_OK 0
#define SEARCH_FAILED -1

#define REMOVE_FROM_CACHE true
#define IGNORE_CACHE false

typedef enum {FFT_UNCHANGED, FFT_CHANGED, FFT_CHANGE_LOCKED} fft_change_t;
typedef enum {WITHOUT_GRAPH, WITH_GRAPH, WITHOUT_INITIAL_GRAPH_HOOK} channel_graph_t;

#define IS_PLAYER(snd) ((snd) && (snd->index < 0))
#define PLAYER(snd) (-(snd->index))
#define NO_PLAYERS false
#define PLAYERS_OK true

#define AT_CURRENT_EDIT_POSITION -1

typedef enum {X_AXIS_IN_SECONDS, X_AXIS_IN_SAMPLES, X_AXIS_AS_PERCENTAGE, X_AXIS_IN_BEATS} x_axis_style_t;
typedef enum {SPEED_CONTROL_AS_FLOAT, SPEED_CONTROL_AS_RATIO, SPEED_CONTROL_AS_SEMITONE} speed_style_t;

typedef enum {CURSOR_IN_VIEW, CURSOR_ON_LEFT, CURSOR_ON_RIGHT, CURSOR_IN_MIDDLE, KEYBOARD_NO_ACTION} kbd_cursor_t;

typedef enum {CHANNELS_SEPARATE, CHANNELS_COMBINED, CHANNELS_SUPERIMPOSED} channel_style_t;
enum {FD_CLOSED, FD_OPEN};
enum {PRINT_SND, PRINT_ENV};

#define SND_DATA_FILE 0xfade
#define SND_DATA_BUFFER 0xbeef

typedef enum {SOUNDS_VERTICAL, SOUNDS_HORIZONTAL, SOUNDS_IN_NOTEBOOK, SOUNDS_IN_SEPARATE_WINDOWS} sound_style_t;
enum {FOURIER, WAVELET, WALSH, AUTOCORRELATION, CEPSTRUM, HADAMARD, HAAR}; /* not typedef'd -- grows as new ones are added */
#define NUM_FFT_WINDOWS 17
#define NUM_WAVELETS 20

typedef enum {APPLY_TO_SOUND, APPLY_TO_CHANNEL, APPLY_TO_SELECTION} snd_apply_t;
enum {FCP_X_ANGLE, FCP_X_SCALE, FCP_Y_ANGLE, FCP_Y_SCALE, FCP_Z_ANGLE, FCP_Z_SCALE, FCP_CUTOFF, FCP_START, FCP_BETA, FCP_BEATS};

enum {TIME_AXIS_INFO, TRANSFORM_AXIS_INFO, LISP_AXIS_INFO};
enum {COLOR_POSITION, COLOR_ZOOM};
enum {MINI_OFF, MINI_CURSOR, MINI_FIND, MINI_PROMPT, MINI_REPORT, MINI_USER};

#define DEFAULT_MIN_DB -60.0
#define NO_LIST -1
#define BLACK_AND_WHITE -1

#define DEFAULT_AMP_CONTROL 1.0
#define DEFAULT_CONTRAST_CONTROL 0.0
#define DEFAULT_CONTRAST_CONTROL_AMP 1.0
#define DEFAULT_CONTRAST_CONTROL_P false
#define DEFAULT_EXPAND_CONTROL 1.0
#define DEFAULT_EXPAND_CONTROL_P false
#define DEFAULT_EXPAND_CONTROL_HOP 0.05
#define DEFAULT_EXPAND_CONTROL_LENGTH 0.15
#define DEFAULT_EXPAND_CONTROL_RAMP 0.4
#define DEFAULT_FILTER_CONTROL_P false
#define DEFAULT_FILTER_CONTROL_ORDER 20
#define DEFAULT_FILTER_CONTROL_IN_DB 0
#define DEFAULT_REVERB_CONTROL_P false
#define DEFAULT_REVERB_CONTROL_FEEDBACK 1.09
#define DEFAULT_REVERB_CONTROL_LENGTH 1.0
#define DEFAULT_REVERB_CONTROL_LOWPASS 0.7
#define DEFAULT_REVERB_CONTROL_SCALE 0.0
#define DEFAULT_SPEED_CONTROL 1.0

#define DEFAULT_SYNC 0
#define DEFAULT_CURSOR_SIZE 15

#define DEFAULT_INIT_WINDOW_X -1
#define DEFAULT_INIT_WINDOW_Y -1
#define DEFAULT_INIT_WINDOW_WIDTH 0
#define DEFAULT_INIT_WINDOW_HEIGHT 0

#define reverb_control_decay(ss) ss->Reverb_Control_Decay
#define in_set_reverb_control_decay(ss, a) ss->Reverb_Control_Decay = a
#define DEFAULT_REVERB_CONTROL_DECAY 1.0

#define default_output_type(ss) ss->Default_Output_Type
#define set_default_output_type(ss, a) ss->Default_Output_Type = a

#define default_output_chans(ss) ss->Default_Output_Chans
#define set_default_output_chans(ss, a) ss->Default_Output_Chans = a

#define default_output_srate(ss) ss->Default_Output_Srate
#define set_default_output_srate(ss, a) ss->Default_Output_Srate = a

#define default_output_format(ss) ss->Default_Output_Format
#define set_default_output_format(ss, a) ss->Default_Output_Format = a

#define dac_size(ss) ss->Dac_Size
#define set_dac_size(ss, a) ss->Dac_Size = a
#if (HAVE_OSS || HAVE_ALSA)
  #ifdef PPC
     /* actually linuxppc */
     #define DEFAULT_DAC_SIZE 0
  #else
     #define DEFAULT_DAC_SIZE 256
  #endif
#else
  #define DEFAULT_DAC_SIZE 1024
#endif

#define dac_combines_channels(ss) ss->Dac_Combines_Channels
#define set_dac_combines_channels(ss, a) ss->Dac_Combines_Channels = a
#define DEFAULT_DAC_COMBINES_CHANNELS true

#define emacs_style_save_as(ss) ss->Emacs_Style_Save_As
#define set_emacs_style_save_as(ss, a) ss->Emacs_Style_Save_As = a
#define DEFAULT_EMACS_STYLE_SAVE_AS false

#define max_regions(ss) ss->Max_Regions
#define in_set_max_regions(ss, a) ss->Max_Regions = a
#define DEFAULT_MAX_REGIONS 16

#define max_transform_peaks(ss) ss->Max_Transform_Peaks
#define in_set_max_transform_peaks(ss, a) ss->Max_Transform_Peaks = a
#define DEFAULT_MAX_TRANSFORM_PEAKS 100

#define auto_resize(ss) ss->Auto_Resize
#define set_auto_resize(ss, a) ss->Auto_Resize = a
#define DEFAULT_AUTO_RESIZE true

#define auto_update(ss) ss->Auto_Update
#define set_auto_update(ss, a) ss->Auto_Update = a
#define DEFAULT_AUTO_UPDATE false

#define auto_update_interval(ss) ss->Auto_Update_Interval
#define set_auto_update_interval(ss, a) ss->Auto_Update_Interval = a
#define DEFAULT_AUTO_UPDATE_INTERVAL 60.0

#define color_cutoff(ss) ss->Color_Cutoff
#define in_set_color_cutoff(ss, a) ss->Color_Cutoff = a
#define DEFAULT_COLOR_CUTOFF 0.003

#define color_inverted(ss) ss->Color_Inverted
#define in_set_color_inverted(ss, a) ss->Color_Inverted = a
#define DEFAULT_COLOR_INVERTED true

#define color_scale(ss) ss->Color_Scale
#define in_set_color_scale(ss, a) ss->Color_Scale = a
#define DEFAULT_COLOR_SCALE 1.0

#define fft_window_beta(ss) ss->Fft_Window_Beta
#define in_set_fft_window_beta(ss, a) ss->Fft_Window_Beta = a
#define DEFAULT_FFT_WINDOW_BETA 0.0

#define transform_size(ss) ss->Transform_Size
#define in_set_transform_size(ss, a) ss->Transform_Size = a
#define DEFAULT_TRANSFORM_SIZE 256

#define transform_graph_type(ss) ss->Transform_Graph_Type
#define in_set_transform_graph_type_1(ss, a) ss->Transform_Graph_Type = a
#define DEFAULT_TRANSFORM_GRAPH_TYPE GRAPH_ONCE

#define time_graph_type(ss) ss->Time_Graph_Type
#define in_set_time_graph_type(ss, a) ss->Time_Graph_Type = a
#define DEFAULT_TIME_GRAPH_TYPE GRAPH_ONCE

#define fft_window(ss) ss->Fft_Window
#define in_set_fft_window_1(ss, a) ss->Fft_Window = a
#define DEFAULT_FFT_WINDOW MUS_BLACKMAN2_WINDOW

#define trap_segfault(ss) ss->Trap_Segfault
#define set_trap_segfault(ss, a) ss->Trap_Segfault = a
#define DEFAULT_TRAP_SEGFAULT true

#define optimization(ss) ss->Optimization
#define set_optimization(ss, a) ss->Optimization = a
#define DEFAULT_OPTIMIZATION 0

#define dot_size(ss) ss->Dot_Size
#define in_set_dot_size(ss, a) ss->Dot_Size = a
#define DEFAULT_DOT_SIZE 1

#define minibuffer_history_length(ss) ss->Minibuffer_History_Length
#define set_minibuffer_history_length(ss, a) ss->Minibuffer_History_Length = a
#define DEFAULT_MINIBUFFER_HISTORY_LENGTH 8

#define transform_normalization(ss) ss->Transform_Normalization
#define in_set_transform_normalization(ss, a) ss->Transform_Normalization = a
#define DEFAULT_TRANSFORM_NORMALIZATION NORMALIZE_BY_CHANNEL

#define ask_before_overwrite(ss) ss->Ask_Before_Overwrite
#define set_ask_before_overwrite(ss, a) ss->Ask_Before_Overwrite = a
#define DEFAULT_ASK_BEFORE_OVERWRITE false

#define spectro_cutoff(ss) ss->Spectro_Cutoff
#define in_set_spectro_cutoff(ss, a) ss->Spectro_Cutoff = a
#define DEFAULT_SPECTRO_CUTOFF 1.0

#define spectro_start(ss) ss->Spectro_Start
#define in_set_spectro_start(ss, a) ss->Spectro_Start = a
#define DEFAULT_SPECTRO_START 0.0

#define spectro_x_angle(ss) ss->Spectro_X_Angle
#define in_set_spectro_x_angle(ss, a) ss->Spectro_X_Angle = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_X_ANGLE 300.0
#else
  #define DEFAULT_SPECTRO_X_ANGLE 90.0
#endif

#define spectro_y_angle(ss) ss->Spectro_Y_Angle
#define in_set_spectro_y_angle(ss, a) ss->Spectro_Y_Angle = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_Y_ANGLE 320.0
#else
  #define DEFAULT_SPECTRO_Y_ANGLE 0.0
#endif

#define spectro_z_angle(ss) ss->Spectro_Z_Angle
#define in_set_spectro_z_angle(ss, a) ss->Spectro_Z_Angle = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_Z_ANGLE 0.0
#else
  #define DEFAULT_SPECTRO_Z_ANGLE 358.0
#endif

#define spectro_x_scale(ss) ss->Spectro_X_Scale
#define in_set_spectro_x_scale(ss, a) ss->Spectro_X_Scale = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_X_SCALE 1.5
  #define SPECTRO_X_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_X_SCALE 1.0
  #define SPECTRO_X_SCALE_MAX 2.0
#endif

#define spectro_y_scale(ss) ss->Spectro_Y_Scale
#define in_set_spectro_y_scale(ss, a) ss->Spectro_Y_Scale = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_Y_SCALE 1.0
  #define SPECTRO_Y_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_Y_SCALE 1.0
  #define SPECTRO_Y_SCALE_MAX 2.0
#endif

#define spectro_z_scale(ss) ss->Spectro_Z_Scale
#define in_set_spectro_z_scale(ss, a) ss->Spectro_Z_Scale = a
#if HAVE_GL
  #define DEFAULT_SPECTRO_Z_SCALE 1.0
  #define SPECTRO_Z_SCALE_MAX 4.0
#else
  #define DEFAULT_SPECTRO_Z_SCALE 0.1
  #define SPECTRO_Z_SCALE_MAX 1.0
#endif

#define spectro_hop(ss) ss->Spectro_Hop
#define in_set_spectro_hop(ss, a) ss->Spectro_Hop = a
#define DEFAULT_SPECTRO_HOP 4

#define color_map(ss) ss->Color_Map
#define in_set_color_map(ss, a) ss->Color_Map = a
#if HAVE_GL
  #define DEFAULT_COLOR_MAP 2
#else
  #define DEFAULT_COLOR_MAP BLACK_AND_WHITE
#endif

#define speed_control_tones(ss) ss->Speed_Control_Tones
#define in_set_speed_control_tones(ss, a) ss->Speed_Control_Tones = a
#define DEFAULT_SPEED_CONTROL_TONES 12

#define speed_control_style(ss) ss->Speed_Control_Style
#define in_set_speed_control_style(ss, a) ss->Speed_Control_Style = a
#define DEFAULT_SPEED_CONTROL_STYLE SPEED_CONTROL_AS_FLOAT

#define graph_style(ss) ss->Graph_Style
#define in_set_graph_style(ss, a) ss->Graph_Style = a
#define DEFAULT_GRAPH_STYLE GRAPH_LINES

#define region_graph_style(ss) ss->Region_Graph_Style
#define set_region_graph_style(ss, a) ss->Region_Graph_Style = a

#define sinc_width(ss) ss->Sinc_Width
#define set_sinc_width(ss, a) ss->Sinc_Width = a
#define DEFAULT_SINC_WIDTH 10

#define verbose_cursor(ss) ss->Verbose_Cursor
#define in_set_verbose_cursor(ss, a) ss->Verbose_Cursor = a
#define DEFAULT_VERBOSE_CURSOR false

#define selection_creates_region(ss) ss->Selection_Creates_Region

#define set_selection_creates_region(ss, a) ss->Selection_Creates_Region = a
#define DEFAULT_SELECTION_CREATES_REGION true

#define filter_env_in_hz(ss) ss->Filter_Env_In_Hz
#define set_filter_env_in_hz(ss, a) ss->Filter_Env_In_Hz = a
#define DEFAULT_FILTER_ENV_IN_HZ false

#define zoom_focus_style(ss) ss->Zoom_Focus_Style
#define set_zoom_focus_style(ss, a) ss->Zoom_Focus_Style = a
#define DEFAULT_ZOOM_FOCUS_STYLE ZOOM_FOCUS_ACTIVE

#define eps_file(ss) ss->Eps_File
#define set_eps_file(ss, a) ss->Eps_File = a
#define DEFAULT_EPS_FILE "snd.eps"

#define eps_left_margin(ss) ss->Eps_Left_Margin
#define set_eps_left_margin(ss, a) ss->Eps_Left_Margin = a
#define DEFAULT_EPS_LEFT_MARGIN 0.0

#define eps_bottom_margin(ss) ss->Eps_Bottom_Margin
#define set_eps_bottom_margin(ss, a) ss->Eps_Bottom_Margin = a
#define DEFAULT_EPS_BOTTOM_MARGIN 0.0

#define eps_size(ss) ss->Eps_Size
#define set_eps_size(ss, a) ss->Eps_Size = a
#define DEFAULT_EPS_SIZE 1.0

#define tiny_font(ss) ss->Tiny_Font
#define in_set_tiny_font(ss, a) ss->Tiny_Font = a

#define bold_button_font(ss) ss->Bold_Button_Font
#define in_set_bold_button_font(ss, a) ss->Bold_Button_Font = a

#define peaks_font(ss) ss->Peaks_Font
#define in_set_peaks_font(ss, a) ss->Peaks_Font = a

#define bold_peaks_font(ss) ss->Bold_Peaks_Font
#define in_set_bold_peaks_font(ss, a) ss->Bold_Peaks_Font = a

#define axis_label_font(ss) ss->Axis_Label_Font
#define in_set_axis_label_font(ss, a) ss->Axis_Label_Font = a

#define axis_numbers_font(ss) ss->Axis_Numbers_Font
#define in_set_axis_numbers_font(ss, a) ss->Axis_Numbers_Font = a

#define listener_font(ss) ss->Listener_Font
#define in_set_listener_font(ss, a) ss->Listener_Font = a

#define save_state_file(ss) ss->Save_State_File
#define in_set_save_state_file(ss, a) ss->Save_State_File = a
#define DEFAULT_SAVE_STATE_FILE "saved-snd." XEN_FILE_EXTENSION

#define temp_dir(ss) ss->Temp_Dir
#define set_temp_dir(ss, a) ss->Temp_Dir = a
#ifndef DEFAULT_TEMP_DIR
  #define DEFAULT_TEMP_DIR NULL
#endif

#define save_dir(ss) ss->Save_Dir
#define set_save_dir(ss, a) ss->Save_Dir = a
#ifndef DEFAULT_SAVE_DIR
  #define DEFAULT_SAVE_DIR NULL
#endif

#define ladspa_dir(ss) ss->Ladspa_Dir
#define set_ladspa_dir(ss, a) ss->Ladspa_Dir = a
#ifndef DEFAULT_LADSPA_DIR
  #define DEFAULT_LADSPA_DIR NULL
#endif

#define vu_font(ss) ss->Vu_Font
#define set_vu_font(ss, a) ss->Vu_Font = a
#define DEFAULT_VU_FONT NULL

#define vu_font_size(ss) ss->Vu_Font_Size
#define set_vu_font_size(ss, a) ss->Vu_Font_Size = a
#define DEFAULT_VU_FONT_SIZE 1.0

#define vu_size(ss) ss->Vu_Size
#define set_vu_size(ss, a) ss->Vu_Size = a
#define DEFAULT_VU_SIZE 1.0

#define wavelet_type(ss) ss->Wavelet_Type
#define in_set_wavelet_type(ss, a) ss->Wavelet_Type = a
#define DEFAULT_WAVELET_TYPE 0

#define transform_type(ss) ss->Transform_Type
#define in_set_transform_type(ss, a) ss->Transform_Type = a
#define DEFAULT_TRANSFORM_TYPE FOURIER

#define show_selection_transform(ss) ss->Show_Selection_Transform
#define in_set_show_selection_transform(ss, a) ss->Show_Selection_Transform = a
#define DEFAULT_SHOW_SELECTION_TRANSFORM 0

#define with_mix_tags(ss) ss->With_Mix_Tags
#define set_with_mix_tags(ss, a) ss->With_Mix_Tags = a
#define DEFAULT_WITH_MIX_TAGS true

#define with_relative_panes(ss) ss->With_Relative_Panes
#define set_with_relative_panes(ss, a) ss->With_Relative_Panes = a
#define DEFAULT_WITH_RELATIVE_PANES true

#define with_gl(ss) ss->With_GL
#define in_set_with_gl(ss, a) ss->With_GL = a
#if HAVE_GL
  #define DEFAULT_WITH_GL true
#else
  #define DEFAULT_WITH_GL false
#endif

#define with_background_processes(ss) ss->With_Background_Processes
#define set_with_background_processes(ss, a) ss->With_Background_Processes = a
#define DEFAULT_WITH_BACKGROUND_PROCESSES true
#define DISABLE_BACKGROUND_PROCESSES 1234

#define wavo_hop(ss) ss->Wavo_Hop
#define in_set_wavo_hop(ss, a) ss->Wavo_Hop = a
#define DEFAULT_WAVO_HOP 3

#define wavo_trace(ss) ss->Wavo_Trace
#define in_set_wavo_trace(ss, a) ss->Wavo_Trace = a
#define DEFAULT_WAVO_TRACE 64

#define x_axis_style(ss) ss->X_Axis_Style
#define in_set_x_axis_style(ss, a) ss->X_Axis_Style = a
#define DEFAULT_X_AXIS_STYLE X_AXIS_IN_SECONDS

#define beats_per_minute(ss) ss->Beats_Per_Minute
#define in_set_beats_per_minute(ss, a) ss->Beats_Per_Minute = a
#define DEFAULT_BEATS_PER_MINUTE 60.0

#define zero_pad(ss) ss->Zero_Pad
#define in_set_zero_pad(ss, a) ss->Zero_Pad = a
#define DEFAULT_ZERO_PAD 0

#define show_transform_peaks(ss) ss->Show_Transform_Peaks
#define in_set_show_transform_peaks(ss, a) ss->Show_Transform_Peaks = a
#define DEFAULT_SHOW_TRANSFORM_PEAKS false

#define show_indices(ss) ss->Show_Indices
#define set_show_indices(ss, a) ss->Show_Indices = a
#define DEFAULT_SHOW_INDICES false

#define show_backtrace(ss) ss->Show_Backtrace
#define set_show_backtrace(ss, a) ss->Show_Backtrace = a
#define DEFAULT_SHOW_BACKTRACE false

#define show_y_zero(ss) ss->Show_Y_Zero
#define in_set_show_y_zero(ss, a) ss->Show_Y_Zero = a
#define DEFAULT_SHOW_Y_ZERO false

#define show_axes(ss) ss->Show_Axes
#define in_set_show_axes(ss, a) ss->Show_Axes = a
#define DEFAULT_SHOW_AXES SHOW_ALL_AXES

#define show_mix_waveforms(ss) ss->Show_Mix_Waveforms
#define in_set_show_mix_waveforms(ss, a) ss->Show_Mix_Waveforms = a
#define DEFAULT_SHOW_MIX_WAVEFORMS true

#define mix_waveform_height(ss) ss->Mix_Waveform_Height
#define in_set_mix_waveform_height(ss, a) ss->Mix_Waveform_Height = a
#define DEFAULT_MIX_WAVEFORM_HEIGHT 20

#define show_marks(ss) ss->Show_Marks
#define in_set_show_marks(ss, a) ss->Show_Marks = a
#define DEFAULT_SHOW_MARKS true

#define fft_log_magnitude(ss) ss->Fft_Log_Magnitude
#define in_set_fft_log_magnitude(ss, a) ss->Fft_Log_Magnitude = a
#define DEFAULT_FFT_LOG_MAGNITUDE false

#define fft_log_frequency(ss) ss->Fft_Log_Frequency
#define in_set_fft_log_frequency(ss, a) ss->Fft_Log_Frequency = a
#define DEFAULT_FFT_LOG_FREQUENCY false

#define channel_style(ss) ss->Channel_Style
#define in_set_channel_style(ss, a) ss->Channel_Style = a
#define DEFAULT_CHANNEL_STYLE CHANNELS_SEPARATE

#define sound_style(ss) ss->Sound_Style
#define set_sound_style(ss, a) ss->Sound_Style = a
#define DEFAULT_SOUND_STYLE SOUNDS_VERTICAL

#define listener_prompt(ss) ss->Listener_Prompt
#define set_listener_prompt(ss, a) ss->Listener_Prompt = a
#define DEFAULT_LISTENER_PROMPT ">"

#define print_length(ss) ss->Print_Length
#define set_print_length(ss, a) ss->Print_Length = a
#define DEFAULT_PRINT_LENGTH 12

#define previous_files_sort(ss) ss->Previous_Files_Sort
#define set_previous_files_sort(ss, a) ss->Previous_Files_Sort = a
#define DEFAULT_PREVIOUS_FILES_SORT 0

#define enved_clip_p(ss) ss->Enved_Clip_p
#define in_set_enved_clip_p(ss, a) ss->Enved_Clip_p = a
#define DEFAULT_ENVED_CLIP_P false

#define enved_wave_p(ss) ss->Enved_Wave_p
#define in_set_enved_wave_p(ss, a) ss->Enved_Wave_p = a
#define DEFAULT_ENVED_WAVE_P false

#define enved_filter_order(ss) ss->Enved_Filter_Order
#define in_set_enved_filter_order(ss, a) ss->Enved_Filter_Order = a
#define DEFAULT_ENVED_FILTER_ORDER 40

#define enved_in_dB(ss) ss->Enved_in_dB
#define in_set_enved_in_dB(ss, a) ss->Enved_in_dB = a
#define DEFAULT_ENVED_IN_DB false

#define enved_target(ss) ss->Enved_Target
#define in_set_enved_target(ss, a) ss->Enved_Target = a
#define DEFAULT_ENVED_TARGET ENVED_AMPLITUDE

#define enved_base(ss) ss->Enved_Base
#define in_set_enved_base(ss, a) ss->Enved_Base = a
#define DEFAULT_ENVED_BASE 1.0

#define enved_power(ss) ss->Enved_Power
#define set_enved_power(ss, a) ss->Enved_Power = a
#define DEFAULT_ENVED_POWER 3.0

#define enved_exp_p(ss) ss->Enved_Exp_p
#define in_set_enved_exp_p(ss, a) ss->Enved_Exp_p = a
#define DEFAULT_ENVED_EXP_P false

#define audio_output_device(ss) ss->Audio_Output_Device
#define set_audio_output_device(ss, a) ss->Audio_Output_Device = a
#define DEFAULT_AUDIO_OUTPUT_DEVICE MUS_AUDIO_DEFAULT

#define audio_input_device(ss) ss->Audio_Input_Device
#define set_audio_input_device(ss, a) ss->Audio_Input_Device = a
#define DEFAULT_AUDIO_INPUT_DEVICE MUS_AUDIO_DEFAULT

#define in_graph_cursor(ss) ss->Graph_Cursor

#define data_clipped(ss) ss->Data_Clipped
#define set_data_clipped(ss, a) ss->Data_Clipped = a
#define DEFAULT_DATA_CLIPPED false

#define html_dir(ss) ss->HTML_Dir
#define set_html_dir_1(ss, a) ss->HTML_Dir = a
#define DEFAULT_HTML_DIR "."

#define html_program(ss) ss->HTML_Program
#define set_html_program(ss, a) ss->HTML_Program = a
#define DEFAULT_HTML_PROGRAM "netscape"

#define graphs_horizontal(ss) ss->Graphs_Horizontal
#define in_set_graphs_horizontal(ss, a) ss->Graphs_Horizontal = a
#define DEFAULT_GRAPHS_HORIZONTAL true

#define mix_tag_width(ss) ss->Mix_Tag_Width
#define set_mix_tag_width(ss, a) ss->Mix_Tag_Width = a
#define DEFAULT_MIX_TAG_WIDTH 6

#define mix_tag_height(ss) ss->Mix_Tag_Height
#define set_mix_tag_height(ss, a) ss->Mix_Tag_Height = a
#define DEFAULT_MIX_TAG_HEIGHT 14

#endif
