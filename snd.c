/* Sound display/edit/etc
 *
 * a re-implementation in summer 1996 of my much-missed dpysnd -- the Foonly/SAIL/E/Mus10/Grnlib sound editor from ca 1983.
 * with much help from Douglas Young's "The X Window System, Programming and Applications with Xt" Prentice-Hall 1994.
 */

#include "snd.h"

snd_state *ss = NULL;
static XEN mus_error_hook;

static bool ignore_mus_error(int type, char *msg)
{
  XEN result = XEN_FALSE;
  if (XEN_HOOKED(mus_error_hook))
    result = run_or_hook(mus_error_hook, 
			 XEN_LIST_2(C_TO_XEN_INT(type), 
				    C_TO_XEN_STRING(msg)),
			 S_mus_error_hook);
  return(XEN_NOT_FALSE_P(result));
}

#if HAVE_SETJMP_H
  RETSIGTYPE top_level_catch(int ignore);
#endif

static void mus_error2snd(int type, char *msg)
{
  /* it's possible to get here outside any catch, and in Guile a throw in that case
   *   kills the main program!
   */
  if (!ss)
    {
      fprintf(stderr, msg);
      return;
    }
  if (!(ignore_mus_error(type, msg)))
    {
      if (ss->catch_exists)
	{
	  if (msg == NULL)
	    XEN_ERROR(MUS_MISC_ERROR,
		      XEN_LIST_1(C_TO_XEN_STRING((char *)mus_error_to_string(type))));
	  else XEN_ERROR(MUS_MISC_ERROR,
			 XEN_LIST_1(C_TO_XEN_STRING(msg)));
	}
      else
	{
	  snd_error("%s: %s", mus_error_to_string(type), msg);
#if HAVE_SETJMP_H
	  ss->jump_ok = true;
	  top_level_catch(1); /* sigh -- try to keep going */
#endif
	}
    }
}

static void mus_print2snd(char *msg)
{
  if (!ss)
    {
      fprintf(stderr, msg);
      return;
    }
  add_to_error_history(msg, false);
  if (record_dialog_is_active()) recorder_error(msg);
  if (!(ignore_mus_error(MUS_NO_ERROR, msg)))
    if (msg)
      {
	int i, len;
	listener_append(";");
	len = strlen(msg);
	for (i = 1; i < len - 1; i++)
	  if ((msg[i] == '\n') && (msg[i + 1] == ' '))
	    msg[i + 1] = ';';
	if (msg[0] == '\n')
	  listener_append((char *)(msg + 1));
	else listener_append(msg);
	if (msg[strlen(msg) - 1] != '\n')
	  listener_append("\n");
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
	    XEN_LIST_3(C_TO_XEN_STRING("GSL"),
		       C_TO_XEN_STRING("~A, ~A in ~A line ~A, gsl err: ~A"),
		       XEN_LIST_5(C_TO_XEN_STRING(gsl_strerror(gsl_errno)),
				  C_TO_XEN_STRING(reason),
				  C_TO_XEN_STRING(file),
				  C_TO_XEN_INT(line),
				  C_TO_XEN_INT(gsl_errno))));
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

#if HAVE_SYS_FPU_H
  union fpc_csr f; f.fc_word = get_fpc_csr(); f.fc_struct.flush = 1; set_fpc_csr(f.fc_word);
#endif

#if HAVE_GSL
  /* if HAVE_GSL and the environment variable GSL_IEEE_MODE exists, use it */
  /* GSL_IEEE_MODE=double-precision,mask-underflow,mask-denormalized */
  if (getenv("GSL_IEEE_MODE") != NULL) 
    gsl_ieee_env_setup();
  gsl_set_error_handler(snd_gsl_error);
#endif

#if ENABLE_NLS && HAVE_GETTEXT
  /* both flags needed to avoid idiotic confusion on the Sun */
  #if HAVE_SETLOCALE
    setlocale (LC_ALL, "");
  #endif
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
#endif

  ss = (snd_state *)CALLOC(1, sizeof(snd_state));
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
	    fprintf(stdout, _("Snd is a sound editor; see http://ccrma.stanford.edu/software/snd/."));
	    fprintf(stdout, version_info());
	    snd_exit(0);
	  }
    }

  initialize_format_lists();

  ss->Transform_Size = DEFAULT_TRANSFORM_SIZE;
  ss->Minibuffer_History_Length = DEFAULT_MINIBUFFER_HISTORY_LENGTH;
  ss->Fft_Window = DEFAULT_FFT_WINDOW;
  ss->Fft_Window_Beta = DEFAULT_FFT_WINDOW_BETA;
  ss->Transform_Graph_Type = DEFAULT_TRANSFORM_GRAPH_TYPE;
  ss->Sinc_Width = DEFAULT_SINC_WIDTH;
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
  ss->Color_Map_Size = DEFAULT_COLOR_MAP_SIZE;
  ss->Spectro_Cutoff = DEFAULT_SPECTRO_CUTOFF;
  ss->Spectro_Start = DEFAULT_SPECTRO_START;
  ss->Wavelet_Type = DEFAULT_WAVELET_TYPE;
  ss->Transform_Type = DEFAULT_TRANSFORM_TYPE;
  ss->Show_Selection_Transform = DEFAULT_SHOW_SELECTION_TRANSFORM;
  ss->With_Mix_Tags = DEFAULT_WITH_MIX_TAGS;
  ss->With_Relative_Panes = DEFAULT_WITH_RELATIVE_PANES;
  ss->With_GL = DEFAULT_WITH_GL;
  ss->With_Background_Processes = DEFAULT_WITH_BACKGROUND_PROCESSES;
  ss->Dot_Size = DEFAULT_DOT_SIZE;
  ss->Cursor_Size = DEFAULT_CURSOR_SIZE;
  ss->Cursor_Style = DEFAULT_CURSOR_STYLE;
  ss->cursor_proc = XEN_UNDEFINED;
  ss->cursor_proc_loc = -1;
  ss->Vu_Size = DEFAULT_VU_SIZE;
  ss->Vu_Font_Size = DEFAULT_VU_FONT_SIZE;
  if (DEFAULT_VU_FONT != (char *)NULL) ss->Vu_Font = copy_string(DEFAULT_VU_FONT); else ss->Vu_Font = NULL;
  ss->Selection_Creates_Region = DEFAULT_SELECTION_CREATES_REGION;
  ss->Transform_Normalization = DEFAULT_TRANSFORM_NORMALIZATION;
  ss->Zoom_Focus_Style = DEFAULT_ZOOM_FOCUS_STYLE;
  ss->Graph_Style = DEFAULT_GRAPH_STYLE;
  ss->Region_Graph_Style = DEFAULT_GRAPH_STYLE;
  ss->Enved_Filter_Order = DEFAULT_ENVED_FILTER_ORDER;
  ss->Verbose_Cursor = DEFAULT_VERBOSE_CURSOR;
  if (DEFAULT_TEMP_DIR != (char *)NULL) ss->Temp_Dir = copy_string(DEFAULT_TEMP_DIR); else ss->Temp_Dir = NULL;
  if (DEFAULT_SAVE_DIR != (char *)NULL) ss->Save_Dir = copy_string(DEFAULT_SAVE_DIR); else ss->Save_Dir = NULL;
  if (DEFAULT_LADSPA_DIR != (char *)NULL) ss->Ladspa_Dir = copy_string(DEFAULT_LADSPA_DIR); else ss->Ladspa_Dir = NULL;
  if (DEFAULT_EPS_FILE != (char *)NULL) ss->Eps_File = copy_string(DEFAULT_EPS_FILE); else ss->Eps_File = NULL;
  ss->Eps_Bottom_Margin = DEFAULT_EPS_BOTTOM_MARGIN;
  ss->Eps_Left_Margin = DEFAULT_EPS_LEFT_MARGIN;
  ss->Eps_Size = DEFAULT_EPS_SIZE;
  ss->Listener_Prompt = copy_string(DEFAULT_LISTENER_PROMPT);
  ss->Show_Transform_Peaks = DEFAULT_SHOW_TRANSFORM_PEAKS;
  ss->Show_Y_Zero = DEFAULT_SHOW_Y_ZERO;
  ss->Show_Grid = DEFAULT_SHOW_GRID;
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
  ss->Trap_Segfault = false;
#else
  ss->Trap_Segfault = DEFAULT_TRAP_SEGFAULT;
#endif
  ss->jump_ok = false;
  ss->Optimization = DEFAULT_OPTIMIZATION;
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
  ss->Enved_Style = DEFAULT_ENVED_STYLE;
  ss->Enved_Target = DEFAULT_ENVED_TARGET;
  ss->Dac_Size = DEFAULT_DAC_SIZE;
  ss->Dac_Combines_Channels = DEFAULT_DAC_COMBINES_CHANNELS;
  ss->Emacs_Style_Save_As = DEFAULT_EMACS_STYLE_SAVE_AS;
  ss->Auto_Update_Interval = DEFAULT_AUTO_UPDATE_INTERVAL;
  ss->Cursor_Update_Interval = DEFAULT_CURSOR_UPDATE_INTERVAL;
  ss->Max_Regions = DEFAULT_MAX_REGIONS;
  ss->Max_Transform_Peaks = DEFAULT_MAX_TRANSFORM_PEAKS;
  allocate_regions(max_regions(ss));
  ss->HTML_Dir = NULL;
  ss->HTML_Program = copy_string(DEFAULT_HTML_PROGRAM);
  ss->min_dB = DEFAULT_MIN_DB;
  ss->lin_dB = pow(10.0, DEFAULT_MIN_DB * 0.05);
  ss->init_window_x = DEFAULT_INIT_WINDOW_X; 
  ss->init_window_y = DEFAULT_INIT_WINDOW_Y; 
  ss->init_window_width = DEFAULT_INIT_WINDOW_WIDTH; 
  ss->init_window_height = DEFAULT_INIT_WINDOW_HEIGHT;
  ss->click_time = 100;

  ss->Expand_Control_Min = DEFAULT_EXPAND_CONTROL_MIN;
  ss->Expand_Control_Max = DEFAULT_EXPAND_CONTROL_MAX;
  ss->Amp_Control_Min = DEFAULT_AMP_CONTROL_MIN;
  ss->Amp_Control_Max = DEFAULT_AMP_CONTROL_MAX;
  ss->Speed_Control_Min = DEFAULT_SPEED_CONTROL_MIN;
  ss->Speed_Control_Max = DEFAULT_SPEED_CONTROL_MAX;
  ss->Contrast_Control_Min = DEFAULT_CONTRAST_CONTROL_MIN;
  ss->Contrast_Control_Max = DEFAULT_CONTRAST_CONTROL_MAX;
  ss->Contrast_Control_Amp = DEFAULT_CONTRAST_CONTROL_AMP;
  ss->Expand_Control_Length = DEFAULT_EXPAND_CONTROL_LENGTH;
  ss->Expand_Control_Ramp = DEFAULT_EXPAND_CONTROL_RAMP;
  ss->Expand_Control_Hop = DEFAULT_EXPAND_CONTROL_HOP;
  ss->Expand_Control_Jitter = DEFAULT_EXPAND_CONTROL_JITTER;
  ss->Reverb_Control_Feedback = DEFAULT_REVERB_CONTROL_FEEDBACK;
  ss->Reverb_Control_Lowpass = DEFAULT_REVERB_CONTROL_LOWPASS;
  ss->Reverb_Control_Scale_Min = DEFAULT_REVERB_CONTROL_SCALE_MIN;
  ss->Reverb_Control_Scale_Max = DEFAULT_REVERB_CONTROL_SCALE_MAX;
  ss->Reverb_Control_Decay = DEFAULT_REVERB_CONTROL_DECAY;
  ss->Speed_Control_Tones = DEFAULT_SPEED_CONTROL_TONES;
  ss->Speed_Control_Style = DEFAULT_SPEED_CONTROL_STYLE;
  ss->Reverb_Control_Length_Min = DEFAULT_REVERB_CONTROL_LENGTH_MIN;
  ss->Reverb_Control_Length_Max = DEFAULT_REVERB_CONTROL_LENGTH_MAX;
  ss->Filter_Control_Order = DEFAULT_FILTER_CONTROL_ORDER;
  ss->Filter_Control_In_Db = DEFAULT_FILTER_CONTROL_IN_DB;
  ss->Filter_Control_In_Hz = DEFAULT_FILTER_CONTROL_IN_HZ;
  ss->Tempo_Control_Min = DEFAULT_TEMPO_CONTROL_MIN;
  ss->Tempo_Control_Max = DEFAULT_TEMPO_CONTROL_MAX;
  ss->Show_Controls = DEFAULT_SHOW_CONTROLS;
  ss->Cursor_Follows_Play = DEFAULT_CURSOR_FOLLOWS_PLAY;

  init_sound_file_extensions();

  ss->max_sounds = 4;                 /* expands to accommodate any number of files */
  ss->stopped_explicitly = false;     /* C-g sets this flag so that we can interrupt various loops */
  ss->reloading_updated_file = 0;
  ss->selected_sound = NO_SELECTION;
  ss->mx_sp = NULL;
  ss->sounds = (snd_info **)CALLOC(ss->max_sounds, sizeof(snd_info *));
  ss->print_choice = PRINT_SND;
  ss->graph_hook_active = false;
  ss->lisp_graph_hook_active = false;
  ss->just_time = false;
  ss->error_lock = false;
  ss->exiting = false;
  ss->just_sounds_state = false;
  ss->deferred_regions = 0;

#if USE_NO_GUI
  ss->catch_exists = 1; /* scm_shell */
#else
  ss->catch_exists = 0;
#endif
  g_initialize_gh();
  ss->search_proc = XEN_UNDEFINED;
  ss->file_sort_proc = XEN_UNDEFINED;
  ss->search_tree = NULL;
  mus_error_set_handler(mus_error2snd);
  mus_print_set_handler(mus_print2snd);

#ifdef SND_AS_WIDGET
  return(ss);
#else
  snd_doit(argc, argv);
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


void g_init_base(void)
{
  #define H_mus_error_hook S_mus_error_hook " (error-type error-message):  called upon mus_error. \
If it returns #t, Snd ignores the error (it assumes you've handled it via the hook)."

  XEN_DEFINE_HOOK(mus_error_hook, S_mus_error_hook, 2, H_mus_error_hook);       /* arg = error-type error-message */
}
