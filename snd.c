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

static snd_state *ss;                      /* global state info, carried around via callData arg in callbacks */

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

  ss=(snd_state *)CALLOC(1,sizeof(snd_state));
  ss->s_type = SND_STATE;

  ss->Fft_Size = DEFAULT_FFT_SIZE;
  ss->Fft_Window = default_fft_window(NULL);
  ss->Fft_Beta = DEFAULT_FFT_BETA;
  ss->Fft_Style = NORMAL_FFT;
  ss->Sinc_Width = DEFAULT_SINC_WIDTH;
  ss->Speed_Tones = DEFAULT_SPEED_TONES;
  ss->Default_Output_Chans = DEFAULT_OUTPUT_CHANS;
  ss->Default_Output_Srate = DEFAULT_OUTPUT_SRATE;
  ss->Default_Output_Type = DEFAULT_OUTPUT_TYPE;
  ss->Default_Output_Format = DEFAULT_OUTPUT_FORMAT;
  ss->Initial_X0 = DEFAULT_INITIAL_X0;
  ss->Initial_X1 = DEFAULT_INITIAL_X1;
  ss->Initial_Y0 = DEFAULT_INITIAL_Y0;
  ss->Initial_Y1 = DEFAULT_INITIAL_Y1;
  ss->Xmax = DEFAULT_XMAX;
  ss->Xmin = DEFAULT_XMIN;
  ss->Ymax = DEFAULT_YMAX;
  ss->Ymin = DEFAULT_YMIN;
  ss->Normalize_On_Open = DEFAULT_NORMALIZE_ON_OPEN;
  ss->Auto_Resize = DEFAULT_AUTO_RESIZE; 
  ss->Auto_Update = DEFAULT_AUTO_UPDATE; 
  ss->Graphs_Horizontal = DEFAULT_GRAPHS_HORIZONTAL;
  ss->Color_Cutoff = DEFAULT_COLOR_CUTOFF;
  ss->Color_Scale = DEFAULT_COLOR_SCALE;
  ss->Color_Inverted = DEFAULT_COLOR_INVERTED;
  ss->Zero_Pad = DEFAULT_ZERO_PAD;
  ss->Line_Size = DEFAULT_LINE_SIZE;
  ss->Ask_Before_Overwrite = DEFAULT_ASK_BEFORE_OVERWRITE;
  ss->X_Axis_Style = X_IN_SECONDS;
  ss->Wavo = DEFAULT_WAVO;
  ss->Wavo_Hop = DEFAULT_WAVO_HOP;
  ss->Wavo_Trace = DEFAULT_WAVO_TRACE;
  ss->Spectro_Hop = DEFAULT_SPECTRO_HOP;
  ss->Spectro_X_Scale = DEFAULT_SPECTRO_X_SCALE;
  ss->Spectro_Y_Scale = DEFAULT_SPECTRO_Y_SCALE;
  ss->Spectro_Z_Scale = DEFAULT_SPECTRO_Z_SCALE;
  ss->Spectro_Z_Angle = DEFAULT_SPECTRO_Z_ANGLE;
  ss->Spectro_X_Angle = DEFAULT_SPECTRO_X_ANGLE;
  ss->Spectro_Y_Angle = DEFAULT_SPECTRO_Y_ANGLE;
  ss->Color_Map = DEFAULT_COLOR_MAP;
  ss->Spectro_Cutoff = DEFAULT_SPECTRO_CUTOFF;
  ss->Spectro_Start = DEFAULT_SPECTRO_START;
  ss->Wavelet_Type = DEFAULT_WAVELET_TYPE;
  ss->Transform_Type = FOURIER;
  ss->Show_Selection_Transform = DEFAULT_SHOW_SELECTION_TRANSFORM;
  ss->With_Mix_Consoles = DEFAULT_WITH_MIX_CONSOLES;
  ss->Dot_Size = DEFAULT_DOT_SIZE;
  ss->Vu_Size = DEFAULT_VU_SIZE;
  ss->Vu_Font_Size = DEFAULT_VU_FONT_SIZE;
  ss->Vu_Font = NULL;
  ss->Speed_Style = SPEED_AS_FLOAT;
  ss->Movies = DEFAULT_MOVIES;
  ss->Normalize_Fft = DEFAULT_NORMALIZE_FFT;
  ss->Fit_Data_On_Open = DEFAULT_FIT_DATA_ON_OPEN;
  ss->Zoom_Focus_Style = FOCUS_ACTIVE;
  ss->Save_State_On_Exit = DEFAULT_SAVE_STATE_ON_EXIT;
  ss->Graph_Style = GRAPH_LINES;
  ss->Use_Sinc_Interp = DEFAULT_USE_SINC_INTERP;
  ss->Filter_Env_Order = DEFAULT_FILTER_ENV_ORDER;
  ss->Verbose_Cursor = DEFAULT_VERBOSE_CURSOR;
  ss->Temp_Dir = NULL;
  ss->Save_Dir = NULL;
  ss->Eps_File = NULL;
  ss->Listener_Prompt = ">";
  ss->Audio_State_File = AUDIO_STATE_FILE;
  ss->Show_Fft_Peaks = DEFAULT_SHOW_FFT_PEAKS;
  ss->Show_Y_Zero = DEFAULT_SHOW_Y_ZERO;
  ss->Erase_Zeros = DEFAULT_ERASE_ZEROS;
  ss->Show_Axes = DEFAULT_SHOW_AXES;
  ss->Show_Marks = DEFAULT_SHOW_MARKS;
  ss->Data_Clipped = DEFAULT_DATA_CLIPPED;
  ss->Fft_Log_Magnitude = DEFAULT_FFT_LOG_MAGNITUDE;
  ss->Fft_Log_Frequency = DEFAULT_FFT_LOG_FREQUENCY;
  ss->Channel_Style = CHANNELS_SEPARATE;
  ss->Sound_Style = SOUNDS_VERTICAL;
  ss->Audio_Output_Device = MUS_AUDIO_DEFAULT;
#if DEBUGGING
  ss->Trap_Segfault = 0;
#else
  ss->Trap_Segfault = DEFAULT_TRAP_SEGFAULT;
#endif
  ss->Raw_Srate = DEFAULT_RAW_SRATE;
  ss->Raw_Chans = DEFAULT_RAW_CHANS;
  ss->Raw_Format = DEFAULT_OUTPUT_FORMAT;
  ss->Use_Raw_Defaults = DEFAULT_USE_RAW_DEFAULTS;
  ss->Print_Length = DEFAULT_PRINT_LENGTH;
  ss->Previous_Files_Sort = DEFAULT_PREVIOUS_FILES_SORT;
  ss->Show_Mix_Consoles = DEFAULT_SHOW_MIX_CONSOLES;
  ss->Show_Mix_Waveforms = DEFAULT_SHOW_MIX_WAVEFORMS;
  ss->Mix_Waveform_Height = DEFAULT_MIX_WAVEFORM_HEIGHT;
  ss->Show_Usage_Stats = DEFAULT_SHOW_USAGE_STATS;
  ss->Recorder_Autoload = DEFAULT_RECORDER_AUTOLOAD;
  ss->Recorder_Buffer_Size = DEFAULT_RECORDER_BUFFER_SIZE;
  ss->Recorder_Out_Chans = DEFAULT_RECORDER_OUT_CHANS;
  ss->Recorder_Out_Format = MUS_COMPATIBLE_FORMAT;
  ss->Reverb_Decay = DEFAULT_REVERB_DECAY;
#ifdef SUN
  ss->Recorder_In_Format = MUS_MULAW;
  ss->Recorder_Srate = 8000;
#else
  ss->Recorder_In_Format = MUS_COMPATIBLE_FORMAT;
  ss->Recorder_Srate = 22050;
#endif
  ss->Recorder_Trigger = DEFAULT_RECORDER_TRIGGER;
  ss->Recorder_Max_Duration = DEFAULT_RECORDER_MAX_DURATION;
  ss->Recorder_File = NULL;
  ss->Save_State_File = "saved-snd.scm";
  ss->Enved_Base = DEFAULT_ENVED_BASE;
  ss->Enved_Power = DEFAULT_ENVED_POWER;
  ss->Enved_Waving = DEFAULT_ENVED_WAVING;
  ss->Enved_dBing = DEFAULT_ENVED_DBING;
  ss->Enved_Clipping = DEFAULT_ENVED_CLIPPING;
  ss->Enved_Exping = DEFAULT_ENVED_EXPING;
  ss->Enved_Target = AMPLITUDE_ENV;
  ss->Prefix_Arg = 0;
#if (HAVE_OSS || HAVE_ALSA)
  #ifdef PPC
     /* actually linuxppc */
     ss->Dac_Size = 0;	
  #else
     ss->Dac_Size = 256;
  #endif
#else
  ss->Dac_Size = 1024;
#endif
  ss->Dac_Folding = TRUE;
  ss->Corruption_Time = DEFAULT_CORRUPTION_TIME;
  ss->Max_Regions = DEFAULT_MAX_REGIONS;
  ss->Max_Fft_Peaks = DEFAULT_MAX_FFT_PEAKS;
  allocate_regions(ss,max_regions(ss));

  ss->min_dB = DEFAULT_MIN_DB;
  ss->lin_dB = 0.001;
  ss->init_window_x = -1; 
  ss->init_window_y = -1; 
  ss->init_window_width = -1; 
  ss->init_window_height = -1;

  init_sound_file_extensions();

  ss->viewing = 0;
  ss->max_sounds = 4;                 /* expands to accomodate any number of files */
  ss->stopped_explicitly = 0;
  ss->selected_sound = NO_SELECTION;
  ss->selected_mix = NO_SELECTION;
  ss->mx_sp = NULL;
  ss->open_ctrls_height = 180;        /* just a first guess */
  ss->sounds = (snd_info **)CALLOC(ss->max_sounds,sizeof(snd_info *));
  ss->print_choice = PRINT_SND;
  ss->listening = 0;
  ss->open_hook_active = 0;
  ss->close_hook_active = 0;
  ss->save_hook_active = 0;
  ss->fft_hook_active = 0;
  ss->graph_hook_active = 0;
  ss->exit_hook_active = 0;
  ss->start_hook_active = 0;
  ss->search_in_progress = 0;
#if NONINTERLEAVED_AUDIO
  ss->audio_hw_channels = DEFAULT_AUDIO_HW_CHANNELS;
#endif

  set_snd_IO_error(SND_NO_ERROR);

#if HAVE_GUILE
  g_initialize_gh(ss);
  ss->search_proc = SCM_UNDEFINED;
#endif
#if DEBUGGING
  check_snd_commands();
#endif
  mus_error_set_handler(mus_error2snd);

#ifdef SND_AS_WIDGET
  return(ss);
#else
  snd_doit(ss,argc,argv);

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

snd_state *get_global_state(void) {return(ss);} /* sigh */
