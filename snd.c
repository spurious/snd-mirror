/* Sound display/edit/etc
 *
 * a re-implementation in summer 1996 of my much-missed dpysnd -- the Foonly/SAIL/E/Mus10/Grnlib sound editor from ca 1983.
 * with much help from Douglas Young's "The X Window System, Programming and Applications with Xt" Prentice-Hall 1994.
 */

/* TODO: tie in configuration scripts to possible use of sndlib.so
 * TODO: make background as separate thread option (for multi-processor machines)
 */

#include "snd.h"

#if HAVE_FPU_CONTROL_H
  #include <fpu_control.h>
#endif

static snd_state *ss;                      /* global state info, carried around via callData arg in callbacks */

static void mus_error2snd(int type, char *msg)
{
  if (!(ignore_mus_error(type, msg)))
    {
      if (ss->catch_exists) /* damned thing aborts main program if throw to tag is not caught! */
	{
	  if (msg == NULL)
	    XEN_ERROR(MUS_MISC_ERROR,
		      XEN_LIST_1(C_TO_XEN_STRING((char *)mus_error_to_string(type))));
	  else XEN_ERROR(MUS_MISC_ERROR,
			 XEN_LIST_1(C_TO_XEN_STRING(msg)));
	}
      /* else we're not called from guile? */
    }
  else return;
  snd_error(msg);
}

static void mus_print2snd(char *msg)
{
  add_to_error_history(get_global_state(), msg, FALSE);
  if (record_dialog_is_active()) recorder_error(msg);
  listener_append(get_global_state(), msg);
}

#if HAVE_SYS_FPU_H
  #include <sys/fpu.h>
#endif

#if HAVE_GSL
  #include <gsl/gsl_ieee_utils.h>
#endif

#if WITH_MCHECK
  #include <mcheck.h>
#endif

#if SND_AS_WIDGET
  snd_state *snd_main(int argc, char **argv)
#else
  #if HAVE_GUILE
    static void snd_main(void *closure, int argc, char **argv)
  #else
    int main (int argc, char **argv)
  #endif
#endif
{
  int i, need_ieee_setup = 1;
#if HAVE_LONG_LONGS
  FILE *md;
#endif

#if HAVE_SYS_FPU_H
  union fpc_csr f; f.fc_word = get_fpc_csr(); f.fc_struct.flush = 1; set_fpc_csr(f.fc_word);
#endif

#if WITH_MCHECK
  mcheck_pedantic(NULL);
#endif

  /* if HAVE_GSL and the environment variable GSL_IEEE_MODE exists, use it */
#if HAVE_GSL
  /* GSL_IEEE_MODE=double-precision,mask-underflow,mask-denormalized */
  if (getenv("GSL_IEEE_MODE")) 
    {
      need_ieee_setup = 0;
      gsl_ieee_env_setup();
    }
#endif

#if HAVE_FPU_CONTROL_H
  #if __GLIBC_MINOR__ < 1
    /* in linux there's <fpu_control.h> with __setfpucw which Clisp calls as __setfpucw(_FPU_IEEE); */
    /* this appears to be useful in getting rid of idiotic NaN's */
  if (need_ieee_setup) {__setfpucw(_FPU_IEEE);}
  #else
    #ifndef __alpha__
      /* 22 in low bits = mask denormalized + mask underflow, both are on in _FPU_IEEE, so this looks right */
      if (need_ieee_setup) {int __fpu_ieee = _FPU_IEEE; _FPU_SETCW(__fpu_ieee);}
      /* this bugfix thanks to Paul Barton-Davis */
    #endif
  #endif
#endif

  mus_sound_initialize(); /* has to precede version check (mus_audio_moniker needs to be setup in Alsa/Oss) */

#if HAVE_RUBY
  ruby_init();
#endif

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "--version") == 0)
	{
	  fprintf(stdout, version_info());
	  snd_exit(0);
	}
      else
	if (strcmp(argv[i], "--help") == 0)
	  {
	    fprintf(stdout, "Snd is a sound editor.");
#if HAVE_EXTENSION_LANGUAGE
#if USE_NO_GUI
	    fprintf(stdout, "  Try the snd-help function for more help.\n");
#else
	    fprintf(stdout, "  Peruse the 'help' menu or try the snd-help function for help.\n");
#endif
#else
#if USE_NO_GUI
	    fprintf(stdout, "  Since you haven't loaded Guile or any interface code, there's not much it can do.\n");
#else
	    fprintf(stdout, "  Peruse the 'help' menu for help, but you should make a real effort to get Guile.\n");
#endif
#endif
	    fprintf(stdout, version_info());
	    snd_exit(0);
	  }
    }

  ss = (snd_state *)CALLOC(1, sizeof(snd_state));

  ss->Transform_Size = DEFAULT_TRANSFORM_SIZE;
  ss->Minibuffer_History_Length = DEFAULT_MINIBUFFER_HISTORY_LENGTH;
  ss->Fft_Window = DEFAULT_FFT_WINDOW;
  ss->Fft_Window_Beta = DEFAULT_FFT_WINDOW_BETA;
  ss->Transform_Graph_Type = DEFAULT_TRANSFORM_GRAPH_TYPE;
  ss->Sinc_Width = DEFAULT_SINC_WIDTH;
  ss->Speed_Control_Tones = DEFAULT_SPEED_CONTROL_TONES;
  ss->Default_Output_Chans = DEFAULT_OUTPUT_CHANS;
  ss->Default_Output_Srate = DEFAULT_OUTPUT_SRATE;
  ss->Default_Output_Type = DEFAULT_OUTPUT_TYPE;
  ss->Default_Output_Format = DEFAULT_OUTPUT_FORMAT;
  ss->Auto_Resize = DEFAULT_AUTO_RESIZE; 
  ss->Auto_Update = DEFAULT_AUTO_UPDATE; 
  ss->Graphs_Horizontal = DEFAULT_GRAPHS_HORIZONTAL;
  ss->Color_Cutoff = DEFAULT_COLOR_CUTOFF;
  ss->Color_Scale = DEFAULT_COLOR_SCALE;
  ss->Color_Inverted = DEFAULT_COLOR_INVERTED;
  ss->Zero_Pad = DEFAULT_ZERO_PAD;
  ss->Ask_Before_Overwrite = DEFAULT_ASK_BEFORE_OVERWRITE;
  ss->X_Axis_Style = DEFAULT_X_AXIS_STYLE;
  ss->Beats_Per_Minute = DEFAULT_BEATS_PER_MINUTE;
  ss->Time_Graph_Type = DEFAULT_TIME_GRAPH_TYPE;
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
  ss->Transform_Type = DEFAULT_TRANSFORM_TYPE;
  ss->Show_Selection_Transform = DEFAULT_SHOW_SELECTION_TRANSFORM;
  ss->With_Mix_Tags = DEFAULT_WITH_MIX_TAGS;
  ss->Dot_Size = DEFAULT_DOT_SIZE;
  ss->Vu_Size = DEFAULT_VU_SIZE;
  ss->Vu_Font_Size = DEFAULT_VU_FONT_SIZE;
  ss->Vu_Font = snd_strdup(DEFAULT_VU_FONT);
  ss->Speed_Control_Style = DEFAULT_SPEED_CONTROL_STYLE;
  ss->Movies = DEFAULT_MOVIES;
  ss->Selection_Creates_Region = DEFAULT_SELECTION_CREATES_REGION;
  ss->Transform_Normalization = DEFAULT_TRANSFORM_NORMALIZATION;
  ss->Zoom_Focus_Style = DEFAULT_ZOOM_FOCUS_STYLE;
  ss->Graph_Style = DEFAULT_GRAPH_STYLE;
  ss->Region_Graph_Style = DEFAULT_GRAPH_STYLE;
  ss->Use_Sinc_Interp = DEFAULT_USE_SINC_INTERP;
  ss->Enved_Filter_Order = DEFAULT_ENVED_FILTER_ORDER;
  ss->Filter_Env_In_Hz = DEFAULT_FILTER_ENV_IN_HZ;
  ss->Verbose_Cursor = DEFAULT_VERBOSE_CURSOR;
  ss->Temp_Dir = snd_strdup(DEFAULT_TEMP_DIR);
  ss->Save_Dir = snd_strdup(DEFAULT_SAVE_DIR);
  ss->Ladspa_Dir = snd_strdup(DEFAULT_LADSPA_DIR);
  ss->Eps_File = snd_strdup(DEFAULT_EPS_FILE);
  ss->Eps_Bottom_Margin = DEFAULT_EPS_BOTTOM_MARGIN;
  ss->Eps_Left_Margin = DEFAULT_EPS_LEFT_MARGIN;
  ss->Eps_Size = DEFAULT_EPS_SIZE;
  ss->Listener_Prompt = snd_strdup(DEFAULT_LISTENER_PROMPT);
  ss->Audio_State_File = snd_strdup(AUDIO_STATE_FILE);
  ss->Show_Transform_Peaks = DEFAULT_SHOW_TRANSFORM_PEAKS;
  ss->Show_Y_Zero = DEFAULT_SHOW_Y_ZERO;
  ss->Show_Axes = DEFAULT_SHOW_AXES;
  ss->Show_Marks = DEFAULT_SHOW_MARKS;
  ss->Show_Indices = DEFAULT_SHOW_INDICES;
  ss->Show_Backtrace = DEFAULT_SHOW_BACKTRACE;
  ss->Data_Clipped = DEFAULT_DATA_CLIPPED;
  ss->Fft_Log_Magnitude = DEFAULT_FFT_LOG_MAGNITUDE;
  ss->Fft_Log_Frequency = DEFAULT_FFT_LOG_FREQUENCY;
  ss->Channel_Style = DEFAULT_CHANNEL_STYLE;
  ss->Sound_Style = DEFAULT_SOUND_STYLE;
  ss->Audio_Input_Device = DEFAULT_AUDIO_INPUT_DEVICE;
  ss->Audio_Output_Device = DEFAULT_AUDIO_OUTPUT_DEVICE;
#if DEBUGGING
  ss->Trap_Segfault = 0;
#else
  ss->Trap_Segfault = DEFAULT_TRAP_SEGFAULT;
#endif
  ss->Print_Length = DEFAULT_PRINT_LENGTH;
  ss->Previous_Files_Sort = DEFAULT_PREVIOUS_FILES_SORT;
  ss->Show_Mix_Waveforms = DEFAULT_SHOW_MIX_WAVEFORMS;
  ss->Mix_Waveform_Height = DEFAULT_MIX_WAVEFORM_HEIGHT;
  ss->Mix_Tag_Width = DEFAULT_MIX_TAG_WIDTH;
  ss->Mix_Tag_Height = DEFAULT_MIX_TAG_HEIGHT;
  ss->Show_Usage_Stats = DEFAULT_SHOW_USAGE_STATS;
  ss->Save_State_File = snd_strdup(DEFAULT_SAVE_STATE_FILE);
  ss->Enved_Base = DEFAULT_ENVED_BASE;
  ss->Enved_Power = DEFAULT_ENVED_POWER;
  ss->Enved_Wave_p = DEFAULT_ENVED_WAVE_P;
  ss->Enved_in_dB = DEFAULT_ENVED_IN_DB;
  ss->Enved_Clip_p = DEFAULT_ENVED_CLIP_P;
  ss->Enved_Exp_p = DEFAULT_ENVED_EXP_P;
  ss->Enved_Target = DEFAULT_ENVED_TARGET;
  ss->Dac_Size = DEFAULT_DAC_SIZE;
  ss->Dac_Combines_Channels = DEFAULT_DAC_COMBINES_CHANNELS;
  ss->Auto_Update_Interval = DEFAULT_AUTO_UPDATE_INTERVAL;
  ss->Max_Regions = DEFAULT_MAX_REGIONS;
  ss->Max_Transform_Peaks = DEFAULT_MAX_TRANSFORM_PEAKS;
  ss->Reverb_Control_Decay = DEFAULT_REVERB_CONTROL_DECAY;
  allocate_regions(ss, max_regions(ss));

  ss->min_dB = DEFAULT_MIN_DB;
  ss->lin_dB = pow(10.0, DEFAULT_MIN_DB * 0.05);
  ss->init_window_x = DEFAULT_INIT_WINDOW_X; 
  ss->init_window_y = DEFAULT_INIT_WINDOW_Y; 
  ss->init_window_width = DEFAULT_INIT_WINDOW_WIDTH; 
  ss->init_window_height = DEFAULT_INIT_WINDOW_HEIGHT;

  init_sound_file_extensions();

  ss->max_sounds = 4;                 /* expands to accommodate any number of files */
  ss->stopped_explicitly = 0;
  ss->reloading_updated_file = FALSE;
  ss->selected_sound = NO_SELECTION;
  ss->selected_mix = NO_SELECTION;
  ss->mx_sp = NULL;
  ss->open_ctrls_height = 180;        /* just a first guess */
  ss->sounds = (snd_info **)CALLOC(ss->max_sounds, sizeof(snd_info *));
  ss->print_choice = PRINT_SND;
  ss->listening = 0;
  ss->graph_hook_active = 0;
  ss->just_time = 0;
  ss->error_lock = 0;
  ss->deferred_regions = 0;

#if HAVE_LONG_LONGS
  md = fopen("/proc/meminfo", "r");
  if (md)
    {
      long long mem;
      fscanf(md, "        total:    used:    free:  shared: buffers:  cached:\nMem:  %Ld", &mem); /* %lld on SGI? */
      fclose(md);
      ss->memory_available = mem / 1024;
    }
  else 
#endif
    ss->memory_available = 0;

  init_recorder();

  ss->catch_exists = 0;
  g_initialize_gh(ss);
  ss->search_proc = XEN_UNDEFINED;
  ss->file_sort_proc = XEN_UNDEFINED;
  mus_error_set_handler(mus_error2snd);
  mus_print_set_handler(mus_print2snd);

#ifdef SND_AS_WIDGET
  return(ss);
#else
  snd_doit(ss, argc, argv);
  #if (!HAVE_GUILE)
    return(0);
  #endif
#endif
}

#ifndef SND_AS_WIDGET
  #if HAVE_GUILE
  int main(int argc, char *argv[])
   {
     scm_boot_guile(argc, argv, snd_main, 0);
     return(0);
   }
  #endif
#endif

snd_state *get_global_state(void) {return(ss);} /* sigh */
