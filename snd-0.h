#ifndef SND_0_H_LOADED
#define SND_0_H_LOADED

#if (!defined(HAVE_CONFIG_H))
  /* we need fstatfs -- need its header and how many args it takes */
  #if defined(SGI) || defined(SCO5) || defined(UW2) || defined(SOLARIS)
    #define HAVE_SYS_STATFS_H
    #define FSTATFS_ARGS 4
  #else
    #if defined(HPUX) || defined(LINUX) || defined(NEXT)
      #define HAVE_SYS_VFS_H
      #define FSTATFS_ARGS 2
    #else
      #if defined(ALPHA)
        #define HAVE_SYS_MOUNT_H
        #define FSTATFS_ARGS 2
      #else
        #if defined(WINDOZE) && defined(__CYGWIN__)
          #define HAVE_SYS_VFS_H
          #define FSTATFS_ARGS 2
        #endif
      #endif
    #endif
  #endif
  #ifdef __CYGWIN__
    #define CLOSEDIR_VOID
    /* apparently we can't trust the return value */
  #endif
  #define HAVE_VPRINTF 1
  #define HAVE_SETLOCALE 1
  #if defined(LINUX) && (!(defined(HAVE_FPU_CONTROL_H)))
    #define HAVE_FPU_CONTROL_H 1
  #endif
  #if defined(LINUX) && (!(defined(HAVE_SCHED_H)))
    #define HAVE_SCHED_H 1
  #endif
  #ifdef LINUX
    #define HAVE_MALLINFO 1
  #endif
  #ifndef _MSC_VER
    #ifndef NEXT
      #define TRAP_SEGFAULT 1
      #define HAVE_TEMPNAM 1
    #endif
    #define HAVE_READLINK 1
    #define HAVE_ACCESS 1
    #define HAVE_OPENDIR 1
    #define HAVE_SLEEP 1
    #define HAVE_SIGNAL 1
    #define HAVE_MEMSET 1
  #endif
  #ifndef BEOS
    #define HAVE_X 1
  #endif
#endif

#ifdef NEXT
  #define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
  #define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)
  #ifndef O_NONBLOCK
    #define O_NONBLOCK 0
  #endif
#endif

#ifndef FSTATFS_ARGS
  #define FSTATFS_ARGS 2
#endif

#ifndef Float
  #define Float float
#endif

#if defined(CCRMA) && (!(defined(SND_CONF)))
  #define SND_CONF "/etc/snd.conf"
#endif

#define XOR(a,b) ((~((a) & (b))) & ((a) | (b)))

#define SND_IO_FD 1
#define SND_IO_CHANS 2
#define SND_IO_SIZE 3
#define SND_IO_BEG 4
#define SND_IO_END 5
#define SND_IO_BUFSIZ 6
#define SND_IO_DATA_START 7
#define SND_IO_DATA_END 8
#define SND_IO_DIR 10
#define SND_IO_LOC 11
#define SND_IO_HDR_END 12
#define SND_IO_DATS 14

#define SND_AREF_BLOCK 0
#define SND_AREF_SIZE 1
#define SND_AREF_HEADER_SIZE 2

#define SND_IO_IN_FILE 0
#define SND_IO_OUT_FILE 1

#define FILE_BUFFER_SIZE 8192
#define MAX_BUFFER_SIZE 65536
#define MIX_FILE_BUFFER_SIZE 2048

#define AMP_ENV_CUTOFF 100000

enum {AMPLITUDE_ENV,SPECTRUM_ENV,SRATE_ENV};

#define NO_COMPLETER -1

#define FILE_SAVE_AS 1
#define EDIT_SAVE_AS 2

#define DEFAULT_OUTPUT_CHANS 1
#define DEFAULT_OUTPUT_SRATE 22050
#define DEFAULT_OUTPUT_TYPE MUS_NEXT
#define DEFAULT_OUTPUT_FORMAT MUS_BSHORT

#define NO_SELECTION -1
#define NO_DEFAULT -1
#define NOT_AN_INDEX -2

enum {CHAN_INFO,SND_INFO,SND_STATE,REGION_INFO};
enum {GRAPH_LINES,GRAPH_DOTS,GRAPH_FILLED,GRAPH_DOTS_AND_LINES,GRAPH_LOLLIPOPS};
enum {NORMAL_FFT,SONOGRAM,SPECTROGRAM};
enum {NOGRAPH,WAVE,FFT,LISP,FFT_MAIN};    /* for marks, regions, mouse click detection */
enum {ARRAY_USAGE,TEMP_USAGE,FILE_USAGE,TEMPS_ACTIVE,TEMPS_OPEN,ARRAYS_ACTIVE,AMP_ENVS_ACTIVE,AMP_ENV_USAGE};
enum {FOCUS_LEFT,FOCUS_RIGHT,FOCUS_ACTIVE,FOCUS_MIDDLE};
enum {DONT_LOCK_MIXES,LOCK_MIXES};
enum {DONT_DELETE_ME,DELETE_ME,ALREADY_DELETED,MULTICHANNEL_DELETION};
enum {PLAIN_MESSAGE,MESSAGE_WITH_CARET,MESSAGE_WITHOUT_CARET};
enum {SND_REOPEN_CLOSED_FILE,SND_OPEN_CHANNEL,SND_COPY_READER,SND_INSERT_FILE,SND_CHANGE_FILE,SND_OVERRIDE_FILE,SND_MIX_FILE};
enum {CURSOR_CROSS,CURSOR_LINE};
enum {SHOW_NO_AXES,SHOW_ALL_AXES,SHOW_X_AXIS};

#define NO_REGIONS -1
#define INVALID_REGION -2

#define READ_FORWARD 1
#define READ_BACKWARD -1

#define FOLLOW_ALWAYS 1
#define FOLLOW_ONCE 2
#define DONT_FOLLOW 0

#define UPDATE_DISPLAY 1
#define DONT_UPDATE_DISPLAY 0

#define FROM_ENVED 1
#define NOT_FROM_ENVED 0

#define FILE_EXISTS 1
#define FILE_DOES_NOT_EXIST 0

enum {X_IN_SECONDS,X_IN_SAMPLES,X_TO_ONE,X_IN_LENGTH};
enum {SPEED_AS_FLOAT,SPEED_AS_RATIO,SPEED_AS_SEMITONE};

enum {MD_CS,MD_TITLE,MD_M};

enum {CURSOR_IN_VIEW,CURSOR_ON_LEFT,CURSOR_ON_RIGHT,CURSOR_IN_MIDDLE,
      CURSOR_UPDATE_DISPLAY,CURSOR_NO_ACTION,CURSOR_CLAIM_SELECTION,KEYBOARD_NO_ACTION};
enum {SEARCH_TREE,EVAL_TREE};

enum {NO_PROBLEM,BLIND_LEAP,GIVE_UP,HUNKER_DOWN};
enum {CHANNELS_SEPARATE,CHANNELS_COMBINED,CHANNELS_SUPERIMPOSED};
enum {FD_INITIALIZED,FD_OPEN,FD_CLOSED};
enum {PRINT_SND,PRINT_ENV};

#define SND_DATA_FILE 0
#define SND_DATA_BUFFER 1

enum {SOUNDS_VERTICAL,SOUNDS_HORIZONTAL,SOUNDS_IN_NOTEBOOK,SOUNDS_IN_SEPARATE_WINDOWS};
enum {FOURIER,WAVELET,HANKEL,WALSH,AUTOCORRELATION,CHEBYSHEV,CEPSTRUM,HADAMARD};

enum {AUTOLOAD,REC_IN_AMPS,REC_OUT_AMPS,AUDIO_GAINS,REC_SRATE,IN_FORMAT,OUT_CHANS,OUT_FORMAT};

enum {NOT_FILING,INPUT_FILING,REGION_FILING,CHANNEL_FILING,TEMP_FILING,CHANGE_FILING,INSERT_FILING,MACRO_FILING};
enum {APPLY_TO_SOUND,APPLY_TO_CHANNEL,APPLY_TO_SELECTION};
enum {LISTENER_CLOSED,LISTENER_OPEN,LISTENER_LISTENING};

enum {SND_NO_ERROR,SND_CANNOT_WRITE_HEADER,SND_CANNOT_OPEN_TEMP_FILE,
      SND_CANNOT_ALLOCATE_IO_BUFFERS,SND_CANNOT_WRITE_DATA,
      SND_CANNOT_REMOVE_FILE,SND_CANNOT_RENAME_FILE,
      SND_CANNOT_READ_HEADER,SND_UNSUPPORTED_HEADER_TYPE,
      SND_CANNOT_FIND_FILE,SND_UNSUPPORTED_DATA_FORMAT,SND_OUTPUT_INTERRUPTED};

#define NO_CHANGE 0
#define JUST_AMP 1
#define JUST_SPEED 2
#define ALL_CHANGES 3

enum {ALPHABET,VALS_GREATER,VALS_LESS};

enum {FILE_PER_SOUND,FILE_PER_CHANNEL};

enum {SCAN_CURRENT_CHAN,SCAN_SOUND_CHANS,SCAN_SYNCD_CHANS,SCAN_ALL_CHANS};
#define AUDIO_STATE_FILE ".snd-mixer"


#define DEFAULT_MIN_DB -60.0
#define DEFAULT_AMP 1.0
#define DEFAULT_CONTRAST 0.0
#define DEFAULT_CONTRAST_AMP 1.0
#define DEFAULT_CONTRASTING 0
#define DEFAULT_EXPAND 1.0
#define DEFAULT_EXPANDING 0
#define DEFAULT_EXPAND_HOP 0.05
#define DEFAULT_EXPAND_LENGTH 0.15
#define DEFAULT_EXPAND_RAMP 0.4
#define DEFAULT_FILTERING 0
#define DEFAULT_FILTER_ORDER 20
#define DEFAULT_FILTER_DBING 0
#define DEFAULT_REVERBING 0
#define DEFAULT_REVERB_FEEDBACK 1.09
#define DEFAULT_REVERB_LENGTH 1.0
#define DEFAULT_REVERB_LOWPASS 0.7
#define DEFAULT_REVERB_SCALE 0.0
#define DEFAULT_SPEED 1.0

#define reverb_decay(ss) ss->Reverb_Decay
#define set_reverb_decay(ss,a) ss->Reverb_Decay = a
#define DEFAULT_REVERB_DECAY 1.0

#define default_output_type(ss) ss->Default_Output_Type
#define set_default_output_type(ss,a) ss->Default_Output_Type = a

#define default_output_chans(ss) ss->Default_Output_Chans
#define set_default_output_chans(ss,a) ss->Default_Output_Chans = a

#define default_output_srate(ss) ss->Default_Output_Srate
#define set_default_output_srate(ss,a) ss->Default_Output_Srate = a

#define default_output_format(ss) ss->Default_Output_Format
#define set_default_output_format(ss,a) ss->Default_Output_Format = a

#define normalize_on_open(ss) ss->Normalize_On_Open
#define set_normalize_on_open(ss,a) ss->Normalize_On_Open = a
#define DEFAULT_NORMALIZE_ON_OPEN 1

#define dac_size(ss) ss->Dac_Size
#define set_dac_size(ss,a) ss->Dac_Size = a

#define dac_folding(ss) ss->Dac_Folding
#define set_dac_folding(ss,a) ss->Dac_Folding = a

#define max_regions(ss) ss->Max_Regions
#define in_set_max_regions(ss,a) ss->Max_Regions = a
#define DEFAULT_MAX_REGIONS 16

#define max_fft_peaks(ss) ss->Max_Fft_Peaks
#define in_set_max_fft_peaks(ss,a) ss->Max_Fft_Peaks = a
#define DEFAULT_MAX_FFT_PEAKS 100

#define auto_resize(ss) ss->Auto_Resize
#define set_auto_resize(ss,a) ss->Auto_Resize = a
#define DEFAULT_AUTO_RESIZE 1

#define auto_update(ss) ss->Auto_Update
#define set_auto_update(ss,a) ss->Auto_Update = a
#define DEFAULT_AUTO_UPDATE 0

#define corruption_time(ss) ss->Corruption_Time
#define set_corruption_time(ss,a) ss->Corruption_Time = a
#define DEFAULT_CORRUPTION_TIME 60.0

#define color_cutoff(ss) ss->Color_Cutoff
#define in_set_color_cutoff(ss,a) ss->Color_Cutoff = a
#define DEFAULT_COLOR_CUTOFF 0.003

#define color_inverted(ss) ss->Color_Inverted
#define in_set_color_inverted(ss,a) ss->Color_Inverted = a
#define DEFAULT_COLOR_INVERTED 1

#define color_scale(ss) ss->Color_Scale
#define in_set_color_scale(ss,a) ss->Color_Scale = a
#define DEFAULT_COLOR_SCALE 1.0

#define fft_beta(ss) ss->Fft_Beta
#define in_set_fft_beta(ss,a) ss->Fft_Beta = a
#define DEFAULT_FFT_BETA 0.0

#define fft_size(ss) ss->Fft_Size
#define in_set_fft_size(ss,a) ss->Fft_Size = a
#define DEFAULT_FFT_SIZE 256

#define fft_style(ss) ss->Fft_Style
#define in_set_fft_style(ss,a) ss->Fft_Style = a

#define fft_window(ss) ss->Fft_Window
#define in_set_fft_window(ss,a) ss->Fft_Window = a

#define trap_segfault(ss) ss->Trap_Segfault
#define set_trap_segfault(ss,a) ss->Trap_Segfault = a
#define DEFAULT_TRAP_SEGFAULT 1

#define initial_x0(ss) ss->Initial_X0
#define set_initial_x0(ss,a) ss->Initial_X0 = a
#define DEFAULT_INITIAL_X0 0.0

#define initial_x1(ss) ss->Initial_X1
#define set_initial_x1(ss,a) ss->Initial_X1 = a
#define DEFAULT_INITIAL_X1 0.1

#define initial_y0(ss) ss->Initial_Y0
#define set_initial_y0(ss,a) ss->Initial_Y0 = a
#define DEFAULT_INITIAL_Y0 -1.0

#define initial_y1(ss) ss->Initial_Y1
#define set_initial_y1(ss,a) ss->Initial_Y1 = a
#define DEFAULT_INITIAL_Y1 1.0

#define line_size(ss) ss->Line_Size
#define set_line_size(ss,a) ss->Line_Size = a
#define DEFAULT_LINE_SIZE 128

#define dot_size(ss) ss->Dot_Size
#define in_set_dot_size(ss,a) ss->Dot_Size = a
#define DEFAULT_DOT_SIZE 1

#define normalize_fft(ss) ss->Normalize_Fft
#define in_set_normalize_fft(ss,a) ss->Normalize_Fft = a
#define DEFAULT_NORMALIZE_FFT 1

#define ask_before_overwrite(ss) ss->Ask_Before_Overwrite
#define set_ask_before_overwrite(ss,a) ss->Ask_Before_Overwrite = a
#define DEFAULT_ASK_BEFORE_OVERWRITE 0

#define spectro_cutoff(ss) ss->Spectro_Cutoff
#define in_set_spectro_cutoff(ss,a) ss->Spectro_Cutoff = a
#define DEFAULT_SPECTRO_CUTOFF 1.0

#define spectro_start(ss) ss->Spectro_Start
#define set_spectro_start(ss,a) ss->Spectro_Start = a
#define DEFAULT_SPECTRO_START 0.0

#define spectro_x_angle(ss) ss->Spectro_X_Angle
#define in_set_spectro_x_angle(ss,a) ss->Spectro_X_Angle = a
#define DEFAULT_SPECTRO_X_ANGLE 90.0

#define spectro_y_angle(ss) ss->Spectro_Y_Angle
#define in_set_spectro_y_angle(ss,a) ss->Spectro_Y_Angle = a
#define DEFAULT_SPECTRO_Y_ANGLE 0.0

#define spectro_z_angle(ss) ss->Spectro_Z_Angle
#define in_set_spectro_z_angle(ss,a) ss->Spectro_Z_Angle = a
#define DEFAULT_SPECTRO_Z_ANGLE -2.0

#define spectro_x_scale(ss) ss->Spectro_X_Scale
#define in_set_spectro_x_scale(ss,a) ss->Spectro_X_Scale = a
#define DEFAULT_SPECTRO_X_SCALE 1.0

#define spectro_y_scale(ss) ss->Spectro_Y_Scale
#define in_set_spectro_y_scale(ss,a) ss->Spectro_Y_Scale = a
#define DEFAULT_SPECTRO_Y_SCALE 1.0

#define spectro_z_scale(ss) ss->Spectro_Z_Scale
#define in_set_spectro_z_scale(ss,a) ss->Spectro_Z_Scale = a
#define DEFAULT_SPECTRO_Z_SCALE 0.1

#define spectro_hop(ss) ss->Spectro_Hop
#define in_set_spectro_hop(ss,a) ss->Spectro_Hop = a
#define DEFAULT_SPECTRO_HOP 4

#define color_map(ss) ss->Color_Map
#define in_set_color_map(ss,a) ss->Color_Map = a
#define DEFAULT_COLOR_MAP -1

#define speed_tones(ss) ss->Speed_Tones
#define set_speed_tones(ss,a) ss->Speed_Tones = a
#define DEFAULT_SPEED_TONES 12

#define speed_style(ss) ss->Speed_Style
#define in_set_speed_style(ss,a) ss->Speed_Style = a

#define graph_style(ss) ss->Graph_Style
#define in_set_graph_style(ss,a) ss->Graph_Style = a

#define sinc_width(ss) ss->Sinc_Width
#define set_sinc_width(ss,a) ss->Sinc_Width = a
#define DEFAULT_SINC_WIDTH 10

#define verbose_cursor(ss) ss->Verbose_Cursor
#define in_set_verbose_cursor(ss,a) ss->Verbose_Cursor = a
#define DEFAULT_VERBOSE_CURSOR 0

#define movies(ss) ss->Movies
#define set_movies(ss,a) ss->Movies = a
#define DEFAULT_MOVIES 1

#define prefix_arg(ss) ss->Prefix_Arg
#define set_prefix_arg(ss,a) ss->Prefix_Arg = a

#define filter_env_order(ss) ss->Filter_Env_Order
#define in_set_filter_env_order(ss,a) ss->Filter_Env_Order = a
#define DEFAULT_FILTER_ENV_ORDER 40

#define save_state_on_exit(ss) ss->Save_State_On_Exit
#define set_save_state_on_exit(ss,a) ss->Save_State_On_Exit = a
#define DEFAULT_SAVE_STATE_ON_EXIT 0

#define zoom_focus_style(ss) ss->Zoom_Focus_Style
#define set_zoom_focus_style(ss,a) ss->Zoom_Focus_Style = a

#define fit_data_on_open(ss) ss->Fit_Data_On_Open
#define set_fit_data_on_open(ss,a) ss->Fit_Data_On_Open = a
#define DEFAULT_FIT_DATA_ON_OPEN 0

#define eps_file(ss) ss->Eps_File
#define set_eps_file(ss,a) ss->Eps_File = a

#define help_text_font(ss) ss->Help_Text_Font
#define in_set_help_text_font(ss,a) ss->Help_Text_Font = a

#define tiny_font(ss) ss->Tiny_Font
#define in_set_tiny_font(ss,a) ss->Tiny_Font = a

#define button_font(ss) ss->Button_Font
#define in_set_button_font(ss,a) ss->Button_Font = a

#define bold_button_font(ss) ss->Bold_Button_Font
#define in_set_bold_button_font(ss,a) ss->Bold_Button_Font = a

#define axis_label_font(ss) ss->Axis_Label_Font
#define in_set_axis_label_font(ss,a) ss->Axis_Label_Font = a

#define axis_numbers_font(ss) ss->Axis_Numbers_Font
#define in_set_axis_numbers_font(ss,a) ss->Axis_Numbers_Font = a

#define listener_font(ss) ss->Listener_Font
#define in_set_listener_font(ss,a) ss->Listener_Font = a

#define audio_state_file(ss) ss->Audio_State_File
#define set_audio_state_file(ss,a) ss->Audio_State_File = a

#define save_state_file(ss) ss->Save_State_File
#define in_set_save_state_file(ss,a) ss->Save_State_File = a

#define temp_dir(ss) ss->Temp_Dir
#define set_temp_dir(ss,a) ss->Temp_Dir = a

#define save_dir(ss) ss->Save_Dir
#define set_save_dir(ss,a) ss->Save_Dir = a

#define vu_font(ss) ss->Vu_Font
#define set_vu_font(ss,a) ss->Vu_Font = a

#define vu_font_size(ss) ss->Vu_Font_Size
#define set_vu_font_size(ss,a) ss->Vu_Font_Size = a
#define DEFAULT_VU_FONT_SIZE 1.0

#define vu_size(ss) ss->Vu_Size
#define set_vu_size(ss,a) ss->Vu_Size = a
#define DEFAULT_VU_SIZE 1.0

#define wavelet_type(ss) ss->Wavelet_Type
#define in_set_wavelet_type(ss,a) ss->Wavelet_Type = a
#define DEFAULT_WAVELET_TYPE 0

#define transform_type(ss) ss->Transform_Type
#define in_set_transform_type(ss,a) ss->Transform_Type = a

#define show_selection_transform(ss) ss->Show_Selection_Transform
#define in_set_show_selection_transform(ss,a) ss->Show_Selection_Transform = a
#define DEFAULT_SHOW_SELECTION_TRANSFORM 0

#define with_mix_consoles(ss) ss->With_Mix_Consoles
#define set_with_mix_consoles(ss,a) ss->With_Mix_Consoles = a
#define DEFAULT_WITH_MIX_CONSOLES 1

#define wavo_hop(ss) ss->Wavo_Hop
#define set_wavo_hop(ss,a) ss->Wavo_Hop = a
#define DEFAULT_WAVO_HOP 3

#define wavo(ss) ss->Wavo
#define set_wavo(ss,a) ss->Wavo = a
#define DEFAULT_WAVO 0

#define wavo_trace(ss) ss->Wavo_Trace
#define set_wavo_trace(ss,a) ss->Wavo_Trace = a
#define DEFAULT_WAVO_TRACE 64

#define x_axis_style(ss) ss->X_Axis_Style
#define in_set_x_axis_style(ss,a) ss->X_Axis_Style = a

#define xmax(ss) ss->Xmax
#define set_xmax(ss,a) ss->Xmax = a
#define DEFAULT_XMAX 0.0

#define xmin(ss) ss->Xmin
#define set_xmin(ss,a) ss->Xmin = a
#define DEFAULT_XMIN 0.0

#define ymax(ss) ss->Ymax
#define set_ymax(ss,a) ss->Ymax = a
#define DEFAULT_YMAX 1.0

#define ymin(ss) ss->Ymin
#define set_ymin(ss,a) ss->Ymin = a
#define DEFAULT_YMIN -1.0

#define zero_pad(ss) ss->Zero_Pad
#define set_zero_pad(ss,a) ss->Zero_Pad = a
#define DEFAULT_ZERO_PAD 0

#define show_fft_peaks(ss) ss->Show_Fft_Peaks
#define in_set_show_fft_peaks(ss,a) ss->Show_Fft_Peaks = a
#define DEFAULT_SHOW_FFT_PEAKS 0

#define show_y_zero(ss) ss->Show_Y_Zero
#define in_set_show_y_zero(ss,a) ss->Show_Y_Zero = a
#define DEFAULT_SHOW_Y_ZERO 0

#define erase_zeros(ss) ss->Erase_Zeros
#define set_erase_zeros(ss,a) ss->Erase_Zeros = a
#define DEFAULT_ERASE_ZEROS 0

#define show_axes(ss) ss->Show_Axes
#define set_show_axes(ss,a) ss->Show_Axes = a
#define DEFAULT_SHOW_AXES SHOW_ALL_AXES

#define show_usage_stats(ss) ss->Show_Usage_Stats
#define in_set_show_usage_stats(ss,a) ss->Show_Usage_Stats = a
#define DEFAULT_SHOW_USAGE_STATS 0

#define show_mix_consoles(ss) ss->Show_Mix_Consoles
#define in_set_show_mix_consoles(ss,a) ss->Show_Mix_Consoles = a
#define DEFAULT_SHOW_MIX_CONSOLES 1

#define show_mix_waveforms(ss) ss->Show_Mix_Waveforms
#define set_show_mix_waveforms(ss,a) ss->Show_Mix_Waveforms = a
#define DEFAULT_SHOW_MIX_WAVEFORMS 0

#define mix_waveform_height(ss) ss->Mix_Waveform_Height
#define in_set_mix_waveform_height(ss,a) ss->Mix_Waveform_Height = a
#define DEFAULT_MIX_WAVEFORM_HEIGHT 20

#define show_marks(ss) ss->Show_Marks
#define in_set_show_marks(ss,a) ss->Show_Marks = a
#define DEFAULT_SHOW_MARKS 1

#define fft_log_magnitude(ss) ss->Fft_Log_Magnitude
#define in_set_fft_log_magnitude(ss,a) ss->Fft_Log_Magnitude = a
#define DEFAULT_FFT_LOG_MAGNITUDE 0

#define fft_log_frequency(ss) ss->Fft_Log_Frequency
#define in_set_fft_log_frequency(ss,a) ss->Fft_Log_Frequency = a
#define DEFAULT_FFT_LOG_FREQUENCY 0

#define channel_style(ss) ss->Channel_Style
#define in_set_channel_style(ss,a) ss->Channel_Style = a

#define sound_style(ss) ss->Sound_Style
#define set_sound_style(ss,a) ss->Sound_Style = a

#define listener_prompt(ss) ss->Listener_Prompt
#define set_listener_prompt(ss,a) ss->Listener_Prompt = a

#define raw_srate(ss) ss->Raw_Srate
#define set_raw_srate(ss,a) ss->Raw_Srate = a
#define DEFAULT_RAW_SRATE 44100

#define raw_chans(ss) ss->Raw_Chans
#define set_raw_chans(ss,a) ss->Raw_Chans = a
#define DEFAULT_RAW_CHANS 1

#define raw_format(ss) ss->Raw_Format
#define set_raw_format(ss,a) ss->Raw_Format = a

#define use_raw_defaults(ss) ss->Use_Raw_Defaults
#define set_use_raw_defaults(ss,a) ss->Use_Raw_Defaults = a
#define DEFAULT_USE_RAW_DEFAULTS 0

#define use_sinc_interp(ss) ss->Use_Sinc_Interp
#define set_use_sinc_interp(ss,a) ss->Use_Sinc_Interp = a
#define DEFAULT_USE_SINC_INTERP 1

#define print_length(ss) ss->Print_Length
#define set_print_length(ss,a) ss->Print_Length = a
#define DEFAULT_PRINT_LENGTH 12

#define previous_files_sort(ss) ss->Previous_Files_Sort
#define set_previous_files_sort(ss,a) ss->Previous_Files_Sort = a
#define DEFAULT_PREVIOUS_FILES_SORT 0

#define enved_clipping(ss) ss->Enved_Clipping
#define in_set_enved_clipping(ss,a) ss->Enved_Clipping = a
#define DEFAULT_ENVED_CLIPPING 0

#define enved_waving(ss) ss->Enved_Waving
#define in_set_enved_waving(ss,a) ss->Enved_Waving = a
#define DEFAULT_ENVED_WAVING 0

#define enved_dBing(ss) ss->Enved_dBing
#define in_set_enved_dBing(ss,a) ss->Enved_dBing = a
#define DEFAULT_ENVED_DBING 0

#define enved_target(ss) ss->Enved_Target
#define in_set_enved_target(ss,a) ss->Enved_Target = a

#define enved_base(ss) ss->Enved_Base
#define in_set_enved_base(ss,a) ss->Enved_Base = a
#define DEFAULT_ENVED_BASE 1.0

#define enved_power(ss) ss->Enved_Power
#define set_enved_power(ss,a) ss->Enved_Power = a
#define DEFAULT_ENVED_POWER 3.0

#define enved_exping(ss) ss->Enved_Exping
#define in_set_enved_exping(ss,a) ss->Enved_Exping = a
#define DEFAULT_ENVED_EXPING 0

#define recorder_autoload(ss) ss->Recorder_Autoload
#define in_set_recorder_autoload(ss,a) ss->Recorder_Autoload = a
#define DEFAULT_RECORDER_AUTOLOAD 0

#define recorder_file(ss) ss->Recorder_File
#define in_set_recorder_file(ss,a) ss->Recorder_File = a

#define recorder_buffer_size(ss) ss->Recorder_Buffer_Size
#define in_set_recorder_buffer_size(ss,a) ss->Recorder_Buffer_Size = a
#define DEFAULT_RECORDER_BUFFER_SIZE 4096

#define recorder_in_format(ss) ss->Recorder_In_Format
#define in_set_recorder_in_format(ss,a) ss->Recorder_In_Format = a

#define recorder_srate(ss) ss->Recorder_Srate
#define in_set_recorder_srate(ss,a) ss->Recorder_Srate = a

#define recorder_trigger(ss) ss->Recorder_Trigger
#define in_set_recorder_trigger(ss,a) ss->Recorder_Trigger = a
#define DEFAULT_RECORDER_TRIGGER 0.0

#define recorder_max_duration(ss) ss->Recorder_Max_Duration
#define in_set_recorder_max_duration(ss,a) ss->Recorder_Max_Duration = a
#define DEFAULT_RECORDER_MAX_DURATION 1000000.0

#define recorder_out_format(ss) ss->Recorder_Out_Format
#define in_set_recorder_out_format(ss,a) ss->Recorder_Out_Format = a

#define recorder_out_chans(ss) ss->Recorder_Out_Chans
#define in_set_recorder_out_chans(ss,a) ss->Recorder_Out_Chans = a
#define DEFAULT_RECORDER_OUT_CHANS 2

#define audio_output_device(ss) ss->Audio_Output_Device
#define set_audio_output_device(ss,a) ss->Audio_Output_Device = a

#define in_graph_cursor(ss) ss->Graph_Cursor

#define data_clipped(ss) ss->Data_Clipped
#define set_data_clipped(ss,a) ss->Data_Clipped = a
#define DEFAULT_DATA_CLIPPED 0

#if HAVE_XmHTML
  #define html_width(ss) ss->HTML_Width
  #define set_html_width(ss,a) ss->HTML_Width = a

  #define html_height(ss) ss->HTML_Height
  #define set_html_height(ss,a) ss->HTML_Height = a

  #define html_dir(ss) ss->HTML_Dir
  #define set_html_dir(ss,a) ss->HTML_Dir = a

  #define html_font_size_list(ss) ss->HTML_Font_Size_List
  #define set_html_font_size_list(ss,a) ss->HTML_Font_Size_List = a

  #define html_fixed_font_size_list(ss) ss->HTML_Fixed_Font_Size_List
  #define set_html_fixed_font_size_list(ss,a) ss->HTML_Fixed_Font_Size_List = a
#endif

#endif

#define graphs_horizontal(ss) ss->Graphs_Horizontal
#define set_graphs_horizontal(ss,a) ss->Graphs_Horizontal = a
#define DEFAULT_GRAPHS_HORIZONTAL 1

#if NONINTERLEAVED_AUDIO
  #define DEFAULT_AUDIO_HW_CHANNELS 2
#endif

