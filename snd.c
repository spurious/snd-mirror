/* Sound display/edit/etc
 *
 * a re-implementation in summer 1996 of my much-missed dpysnd -- the Foonly/SAIL/E/Mus10/Grnlib sound editor from ca 1983.
 * with much help from Douglas Young's "The X Window System, Programming and Applications with Xt" Prentice-Hall 1994.
 */

#include "snd.h"

#ifdef HAVE_FPU_CONTROL_H
  #include <fpu_control.h>
#endif

#if DEBUGGING
  void check_snd_commands(void);
#endif

static snd_state *state;                      /* global state info, carried around via callData arg in callbacks */

static void mus_error2snd(int type, char *msg)
{
#if HAVE_GUILE
  if (!(ignore_mus_error(type,msg)))
#endif
  if (type != MUS_UNSUPPORTED_HEADER_TYPE)
    {
      if (type != MUS_NO_ERROR)
	{
	  snd_error(msg);
	  if (type == MUS_WRITE_ERROR)
	    set_snd_IO_error(SND_CANNOT_WRITE_DATA);
	}
      else fprintf(stderr,msg); /* scm_misc_error here causes segfaults for some reason */
    }
}

#ifdef SGI
  #include <sys/fpu.h>
#endif

#if SND_AS_WIDGET
  snd_state *snd_main(int argc, char **argv)
#else
  #if HAVE_GUILE
    static void snd_main(int argc, char **argv)
  #else
    int main (int argc, char **argv)
  #endif
#endif
{
  int i;
#ifdef SGI
  union fpc_csr f; f.fc_word = get_fpc_csr(); f.fc_struct.flush = 1; set_fpc_csr(f.fc_word);
#endif
#ifdef HAVE_FPU_CONTROL_H
  #if __GLIBC_MINOR__ < 1
    /* in linux there's <fpu_control.h> with __setfpucw which Clisp calls as __setfpucw(_FPU_IEEE); */
    /* this appears to be useful in getting rid of idiotic NaN's */
    __setfpucw(_FPU_IEEE);
  #else
    #ifndef __alpha__
      int __fpu_ieee = _FPU_IEEE;
      _FPU_SETCW(__fpu_ieee);
      /* this bugfix thanks to Paul Barton-Davis */
    #else
      int __fpu_ieee = 0;
    #endif
  #endif
#endif

  for (i=1;i<argc;i++)
    {
      if (strcmp(argv[i],"--version") == 0)
	{
	  fprintf(stdout,version_info());
	  snd_exit(0);
	}
      else
	if (strcmp(argv[i],"--help") == 0)
	  {
	    fprintf(stdout,"Snd is a sound editor.  Execute it and peruse the 'help' menu for details.\n");
	    fprintf(stdout,version_info());
	    snd_exit(0);
	  }
    }

  mus_sound_initialize();

  state=(snd_state *)CALLOC(1,sizeof(snd_state));
  state->s_type = SND_STATE;

  state->Fft_Size = DEFAULT_FFT_SIZE;
  state->Fft_Window = default_fft_window(NULL);
  state->Fft_Beta = DEFAULT_FFT_BETA;
  state->Fft_Style = NORMAL_FFT;
  state->Sinc_Width = DEFAULT_SINC_WIDTH;
  state->Speed_Tones = DEFAULT_SPEED_TONES;
  state->Default_Output_Chans = DEFAULT_OUTPUT_CHANS;
  state->Default_Output_Srate = DEFAULT_OUTPUT_SRATE;
  state->Default_Output_Type = DEFAULT_OUTPUT_TYPE;
  state->Default_Output_Format = DEFAULT_OUTPUT_FORMAT;
  state->Initial_X0 = DEFAULT_INITIAL_X0;
  state->Initial_X1 = DEFAULT_INITIAL_X1;
  state->Initial_Y0 = DEFAULT_INITIAL_Y0;
  state->Initial_Y1 = DEFAULT_INITIAL_Y1;
  state->Xmax = DEFAULT_XMAX;
  state->Xmin = DEFAULT_XMIN;
  state->Ymax = DEFAULT_YMAX;
  state->Ymin = DEFAULT_YMIN;
  state->Normalize_On_Open = DEFAULT_NORMALIZE_ON_OPEN;
  state->Auto_Resize = DEFAULT_AUTO_RESIZE; 
  state->Auto_Update = DEFAULT_AUTO_UPDATE; 
  state->Graphs_Horizontal = DEFAULT_GRAPHS_HORIZONTAL;
  state->Color_Cutoff = DEFAULT_COLOR_CUTOFF;
  state->Color_Scale = DEFAULT_COLOR_SCALE;
  state->Color_Inverted = DEFAULT_COLOR_INVERTED;
  state->Zero_Pad = DEFAULT_ZERO_PAD;
  state->Line_Size = DEFAULT_LINE_SIZE;
  state->Ask_Before_Overwrite = DEFAULT_ASK_BEFORE_OVERWRITE;
  state->X_Axis_Style = X_IN_SECONDS;
  state->Wavo = DEFAULT_WAVO;
  state->Wavo_Hop = DEFAULT_WAVO_HOP;
  state->Wavo_Trace = DEFAULT_WAVO_TRACE;
  state->Spectro_Hop = DEFAULT_SPECTRO_HOP;
  state->Spectro_X_Scale = DEFAULT_SPECTRO_X_SCALE;
  state->Spectro_Y_Scale = DEFAULT_SPECTRO_Y_SCALE;
  state->Spectro_Z_Scale = DEFAULT_SPECTRO_Z_SCALE;
  state->Spectro_Z_Angle = DEFAULT_SPECTRO_Z_ANGLE;
  state->Spectro_X_Angle = DEFAULT_SPECTRO_X_ANGLE;
  state->Spectro_Y_Angle = DEFAULT_SPECTRO_Y_ANGLE;
  state->Color_Map = DEFAULT_COLOR_MAP;
  state->Spectro_Cutoff = DEFAULT_SPECTRO_CUTOFF;
  state->Spectro_Start = DEFAULT_SPECTRO_START;
  state->Wavelet_Type = DEFAULT_WAVELET_TYPE;
  state->Transform_Type = FOURIER;
  state->Show_Selection_Transform = DEFAULT_SHOW_SELECTION_TRANSFORM;
  state->With_Mix_Consoles = DEFAULT_WITH_MIX_CONSOLES;
  state->Dot_Size = DEFAULT_DOT_SIZE;
  state->Vu_Size = DEFAULT_VU_SIZE;
  state->Vu_Font_Size = DEFAULT_VU_FONT_SIZE;
  state->Vu_Font = NULL;
  state->Speed_Style = SPEED_AS_FLOAT;
  state->Movies = DEFAULT_MOVIES;
  state->Normalize_Fft = DEFAULT_NORMALIZE_FFT;
  state->Fit_Data_On_Open = DEFAULT_FIT_DATA_ON_OPEN;
  state->Zoom_Focus_Style = FOCUS_ACTIVE;
  state->Save_State_On_Exit = DEFAULT_SAVE_STATE_ON_EXIT;
  state->Graph_Style = GRAPH_LINES;
  state->Use_Sinc_Interp = DEFAULT_USE_SINC_INTERP;
  state->Filter_Env_Order = DEFAULT_FILTER_ENV_ORDER;
  state->Verbose_Cursor = DEFAULT_VERBOSE_CURSOR;
  state->Temp_Dir = NULL;
  state->Save_Dir = NULL;
  state->Eps_File = NULL;
  state->Listener_Prompt = ">";
  state->Audio_State_File = AUDIO_STATE_FILE;
  state->Show_Fft_Peaks = DEFAULT_SHOW_FFT_PEAKS;
  state->Show_Y_Zero = DEFAULT_SHOW_Y_ZERO;
  state->Show_Axes = DEFAULT_SHOW_AXES;
  state->Show_Marks = DEFAULT_SHOW_MARKS;
  state->Data_Clipped = DEFAULT_DATA_CLIPPED;
#if ((USE_MOTIF) && (XmVERSION == 1))
  state->Show_Edit_History = 0;
  state->Edit_History_Width = 0;
#endif
  state->Fft_Log_Magnitude = DEFAULT_FFT_LOG_MAGNITUDE;
  state->Fft_Log_Frequency = DEFAULT_FFT_LOG_FREQUENCY;
  state->Channel_Style = CHANNELS_SEPARATE;
  state->Sound_Style = SOUNDS_VERTICAL;
  state->Audio_Output_Device = MUS_AUDIO_DEFAULT;
#if DEBUGGING
  state->Trap_Segfault = 0;
#else
  state->Trap_Segfault = DEFAULT_TRAP_SEGFAULT;
#endif
  state->Raw_Srate = DEFAULT_RAW_SRATE;
  state->Raw_Chans = DEFAULT_RAW_CHANS;
  state->Raw_Format = DEFAULT_OUTPUT_FORMAT;
  state->Use_Raw_Defaults = DEFAULT_USE_RAW_DEFAULTS;
  state->Print_Length = DEFAULT_PRINT_LENGTH;
  state->Previous_Files_Sort = DEFAULT_PREVIOUS_FILES_SORT;
  state->Show_Mix_Consoles = DEFAULT_SHOW_MIX_CONSOLES;
  state->Show_Mix_Waveforms = DEFAULT_SHOW_MIX_WAVEFORMS;
  state->Mix_Waveform_Height = DEFAULT_MIX_WAVEFORM_HEIGHT;
  state->Show_Usage_Stats = DEFAULT_SHOW_USAGE_STATS;
  state->Recorder_Autoload = DEFAULT_RECORDER_AUTOLOAD;
  state->Recorder_Buffer_Size = DEFAULT_RECORDER_BUFFER_SIZE;
  state->Recorder_Out_Chans = DEFAULT_RECORDER_OUT_CHANS;
  state->Recorder_Out_Format = MUS_COMPATIBLE_FORMAT;
  state->Reverb_Decay = DEFAULT_REVERB_DECAY;
#ifdef SUN
  state->Recorder_In_Format = MUS_MULAW;
  state->Recorder_Srate = 8000;
#else
  state->Recorder_In_Format = MUS_COMPATIBLE_FORMAT;
  state->Recorder_Srate = 22050;
#endif
  state->Recorder_Trigger = DEFAULT_RECORDER_TRIGGER;
  state->Recorder_Max_Duration = DEFAULT_RECORDER_MAX_DURATION;
  state->Recorder_File = NULL;
  state->Save_State_File = "saved-snd.scm";
  state->Enved_Base = DEFAULT_ENVED_BASE;
  state->Enved_Power = DEFAULT_ENVED_POWER;
  state->Enved_Waving = DEFAULT_ENVED_WAVING;
  state->Enved_dBing = DEFAULT_ENVED_DBING;
  state->Enved_Clipping = DEFAULT_ENVED_CLIPPING;
  state->Enved_Exping = DEFAULT_ENVED_EXPING;
  state->Enved_Target = AMPLITUDE_ENV;
  state->Prefix_Arg = 0;
#if (HAVE_OSS || HAVE_ALSA)
  #ifdef PPC
     /* actually linuxppc */
     state->Dac_Size = 0;	
  #else
     state->Dac_Size = 256;
  #endif
#else
  state->Dac_Size = 1024;
#endif
  state->Dac_Folding = TRUE;
  state->Corruption_Time = DEFAULT_CORRUPTION_TIME;
  state->Max_Regions = DEFAULT_MAX_REGIONS;
  state->Max_Fft_Peaks = DEFAULT_MAX_FFT_PEAKS;
  allocate_regions(state,max_regions(state));

  state->min_dB = DEFAULT_MIN_DB;
  state->lin_dB = 0.001;
  state->init_window_x = -1; 
  state->init_window_y = -1; 
  state->init_window_width = -1; 
  state->init_window_height = -1;

  init_sound_file_extensions();

  state->viewing = 0;
  state->max_sounds = 4;                 /* expands to accomodate any number of files */
  state->stopped_explicitly = 0;
  state->selected_sound = NO_SELECTION;
  state->selected_mix = NO_SELECTION;
  state->mx_sp = NULL;
  state->open_ctrls_height = 180;        /* just a first guess */
  state->sounds = (snd_info **)CALLOC(state->max_sounds,sizeof(snd_info *));
  state->print_choice = PRINT_SND;
  state->listening = 0;
  state->open_hook_active = 0;
  state->close_hook_active = 0;
  state->fft_hook_active = 0;
  state->graph_hook_active = 0;
  state->exit_hook_active = 0;
  state->start_hook_active = 0;
  state->search_in_progress = 0;
#if NONINTERLEAVED_AUDIO
  state->audio_hw_channels = DEFAULT_AUDIO_HW_CHANNELS;
#endif

  set_snd_IO_error(SND_NO_ERROR);

#if HAVE_GUILE
  g_initialize_gh(state);
  state->search_proc = SCM_UNDEFINED;
#endif
#if DEBUGGING
  check_snd_commands();
#endif
  mus_error_set_handler(mus_error2snd);

#ifdef SND_AS_WIDGET
  return(state);
#else
  snd_doit(state,argc,argv);

  #if (!HAVE_GUILE)
    return(0);
  #endif
#endif
}

#ifndef SND_AS_WIDGET
  #if HAVE_GUILE     
  int main(int argc, char *argv[])
   {
     gh_enter(argc,argv,snd_main);
     return(0);
   }
  #endif
#endif

snd_state *get_global_state(void) {return(state);} /* sigh */
