/* Sound display/edit/etc
 *
 * a re-implementation in summer 1996 of my much-missed dpysnd -- the Foonly/SAIL/E/Mus10/Grnlib sound editor from ca 1983.
 * with much help from Douglas Young's "The X Window System, Programming and Applications with Xt" Prentice-Hall 1994.
 */

#include "snd.h"

/* SOMEDAY: put Snd names in its own module
 * SOMEDAY: set up configure to handle libsndlib correctly
 */

static snd_state *ss;

static XEN mus_error_hook;

static int ignore_mus_error(int type, char *msg)
{
  XEN result = XEN_FALSE;
  if (XEN_HOOKED(mus_error_hook))
    result = g_c_run_or_hook(mus_error_hook, 
			     XEN_LIST_2(C_TO_XEN_INT(type), 
					C_TO_XEN_STRING(msg)),
			     S_mus_error_hook);
  return(XEN_NOT_FALSE_P(result));
}

static void mus_error2snd(int type, char *msg)
{
  if (!(ignore_mus_error(type, msg)))
    {
      if (msg == NULL)
	XEN_ERROR(MUS_MISC_ERROR,
		  XEN_LIST_1(C_TO_XEN_STRING((char *)mus_error_to_string(type))));
      else XEN_ERROR(MUS_MISC_ERROR,
		     XEN_LIST_1(C_TO_XEN_STRING(msg)));
    }
}

static void mus_print2snd(char *msg)
{
  add_to_error_history(ss, msg, FALSE);
  if (record_dialog_is_active()) recorder_error(msg);
  if (!(ignore_mus_error(MUS_NO_ERROR, msg)))
    if (msg)
      {
	listener_append(ss, ";");
	{
	  int i, len;
	  len = strlen(msg);
	  for (i = 1; i < len - 1; i++)
	    if ((msg[i] == '\n') && (msg[i + 1] == ' '))
	      msg[i + 1] = ';';
	}
	if (msg[0] == '\n')
	  listener_append(ss, (char *)(msg + 1));
	else listener_append(ss, msg);
	if (msg[strlen(msg) - 1] != '\n')
	  listener_append(ss, "\n");
      }
}

#if HAVE_SYS_FPU_H
  #include <sys/fpu.h>
#endif

#if HAVE_GSL
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_errno.h>
/* default gsl error handler apparently aborts main program! */
static void snd_gsl_error(const char *reason, const char *file, int line, int gsl_errno)
{
  XEN_ERROR(SND_GSL_ERROR,
	    XEN_LIST_5(C_TO_XEN_STRING(reason),
		       C_TO_XEN_STRING(file),
		       C_TO_XEN_INT(line),
		       C_TO_XEN_INT(gsl_errno),
		       C_TO_XEN_STRING(gsl_strerror(gsl_errno))));
}
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
  int i;
#if HAVE_LLONGS
  FILE *md;
#endif

#if HAVE_SYS_FPU_H
  union fpc_csr f; f.fc_word = get_fpc_csr(); f.fc_struct.flush = 1; set_fpc_csr(f.fc_word);
#endif

#if HAVE_GSL
  /* if HAVE_GSL and the environment variable GSL_IEEE_MODE exists, use it */
  /* GSL_IEEE_MODE=double-precision,mask-underflow,mask-denormalized */
  if (getenv("GSL_IEEE_MODE")) 
    gsl_ieee_env_setup();
  gsl_set_error_handler(snd_gsl_error);
#endif

  /* I think the old Linux problem with underflows has been fixed -- the default is now the
   *   same as _FPU_IEEE (_FPU_IEEE == _FPU_DEFAULT as of July-2000) so the original 
   *   (not very portable) fpu_control.h code is no longer needed:
   *   int __fpu_ieee = _FPU_IEEE; _FPU_SETCW(__fpu_ieee);
   */

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
  ss->With_Background_Processes = DEFAULT_WITH_BACKGROUND_PROCESSES;
  ss->Dot_Size = DEFAULT_DOT_SIZE;
  ss->Vu_Size = DEFAULT_VU_SIZE;
  ss->Vu_Font_Size = DEFAULT_VU_FONT_SIZE;
  if (DEFAULT_VU_FONT != (char *)NULL) ss->Vu_Font = copy_string(DEFAULT_VU_FONT); else ss->Vu_Font = NULL;
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
  if (DEFAULT_TEMP_DIR != (char *)NULL) ss->Temp_Dir = copy_string(DEFAULT_TEMP_DIR); else ss->Temp_Dir = NULL;
  if (DEFAULT_SAVE_DIR != (char *)NULL) ss->Save_Dir = copy_string(DEFAULT_SAVE_DIR); else ss->Save_Dir = NULL;
  if (DEFAULT_LADSPA_DIR != (char *)NULL) ss->Ladspa_Dir = copy_string(DEFAULT_LADSPA_DIR); else ss->Ladspa_Dir = NULL;
  if (DEFAULT_EPS_FILE != (char *)NULL) ss->Eps_File = copy_string(DEFAULT_EPS_FILE); else ss->Eps_File = NULL;
  ss->Eps_Bottom_Margin = DEFAULT_EPS_BOTTOM_MARGIN;
  ss->Eps_Left_Margin = DEFAULT_EPS_LEFT_MARGIN;
  ss->Eps_Size = DEFAULT_EPS_SIZE;
  ss->Listener_Prompt = copy_string(DEFAULT_LISTENER_PROMPT);
  if (DEFAULT_AUDIO_STATE_FILE != (char *)NULL) ss->Audio_State_File = copy_string(DEFAULT_AUDIO_STATE_FILE); else ss->Audio_State_File = NULL;
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
  if (DEFAULT_SAVE_STATE_FILE != (char *)NULL) ss->Save_State_File = copy_string(DEFAULT_SAVE_STATE_FILE); else ss->Save_State_File = NULL;
  ss->Enved_Base = DEFAULT_ENVED_BASE;
  ss->Enved_Power = DEFAULT_ENVED_POWER;
  ss->Enved_Wave_p = DEFAULT_ENVED_WAVE_P;
  ss->Enved_in_dB = DEFAULT_ENVED_IN_DB;
  ss->Enved_Clip_p = DEFAULT_ENVED_CLIP_P;
  ss->Enved_Exp_p = DEFAULT_ENVED_EXP_P;
  ss->Enved_Target = DEFAULT_ENVED_TARGET;
  ss->Dac_Size = DEFAULT_DAC_SIZE;
  ss->Dac_Combines_Channels = DEFAULT_DAC_COMBINES_CHANNELS;
  ss->Emacs_Style_Save_As = DEFAULT_EMACS_STYLE_SAVE_AS;
  ss->Auto_Update_Interval = DEFAULT_AUTO_UPDATE_INTERVAL;
  ss->Max_Regions = DEFAULT_MAX_REGIONS;
  ss->Max_Transform_Peaks = DEFAULT_MAX_TRANSFORM_PEAKS;
  ss->Reverb_Control_Decay = DEFAULT_REVERB_CONTROL_DECAY;
  allocate_regions(ss, max_regions(ss));

#if HAVE_HTML
  ss->HTML_Dir = NULL;
#endif

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
  ss->selected_mix = INVALID_MIX_ID;
  ss->mx_sp = NULL;
  ss->open_ctrls_height = 180;        /* just a first guess */
  ss->sounds = (snd_info **)CALLOC(ss->max_sounds, sizeof(snd_info *));
  ss->print_choice = PRINT_SND;
  ss->listening = 0;
  ss->graph_hook_active = 0;
  ss->just_time = 0;
  ss->error_lock = 0;
  ss->deferred_regions = 0;

#if HAVE_LLONGS
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


void g_init_base(void)
{
  #define H_mus_error_hook S_mus_error_hook " (error-type error-message) is called upon mus_error. \
If it returns #t, Snd ignores the error (it assumes you've handled it via the hook)."

  XEN_DEFINE_HOOK(mus_error_hook, S_mus_error_hook, 2, H_mus_error_hook);       /* arg = error-type error-message */
}
