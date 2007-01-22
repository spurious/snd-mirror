/* Sound display/edit/etc
 *
 * originally intended as a re-implementation of my much-missed dpysnd -- the Foonly/SAIL/E/Mus10/Grnlib sound editor from ca 1983.
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

void mus_error_to_snd(int type, char *msg)
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
	    XEN_ERROR(XEN_ERROR_TYPE("mus-error"),
		      XEN_LIST_1(C_TO_XEN_STRING((char *)mus_error_type_to_string(type))));
	  else XEN_ERROR(XEN_ERROR_TYPE("mus-error"),
			 XEN_LIST_1(C_TO_XEN_STRING(msg)));
	}
      else
	{
	  snd_error("%s: %s", mus_error_type_to_string(type), msg);
#if HAVE_SETJMP_H
	  ss->jump_ok = true;
	  top_level_catch(1); /* sigh -- try to keep going */
#endif
	}
    }
}

static void mus_print_to_snd(char *msg)
{
  if (!ss)
    {
      fprintf(stderr, msg);
      return;
    }
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

static void initialize_load_path(void)
{
  /* look for SND_PATH env var, add dirs to %load-path or load_path */
  char *path;
  path = getenv("SND_PATH");
  if (path)
    {
      /* colon-separated list of directory names, pushed on load-path in reverse order (hopefully = search order) */
      int i, len, dirs = 1, curdir = 0, start = 0;
      char **dirnames;
      len = strlen(path);
      for (i = 0; i < len; i++)
	if (path[i] == ':')
	  dirs++;
      dirnames = (char **)CALLOC(dirs, sizeof(char *));
      for (i = 0; i < len; i++)
	{
	  if ((path[i] == ':') ||
	      (i == len - 1))
	    {
	      if (i > start)
		{
		  int j, lim;
		  char *tmp;
		  if (i == (len - 1))
		    lim = i + 1;
		  else lim = i;
		  tmp = (char *)CALLOC(lim - start + 1, sizeof(char));
		  for (j = start; j < lim; j++)
		    tmp[j - start] = path[j];
		  dirnames[curdir++] = mus_expand_filename(tmp);
		  start = i + 1;
		  FREE(tmp);
		}
	    }
	}
      for (i = curdir - 1; i >= 0; i--)
	{
	  XEN_ADD_TO_LOAD_PATH(dirnames[i]);
	  FREE(dirnames[i]);
	}
      FREE(dirnames);
    }
}

void snd_set_global_defaults(bool need_cleanup)
{
  if (need_cleanup)
    {
      if (ss->HTML_Program) {FREE(ss->HTML_Program); ss->HTML_Program = NULL;}
      if (ss->HTML_Dir) {FREE(ss->HTML_Dir); ss->HTML_Dir = NULL;}
      if (ss->Temp_Dir) {FREE(ss->Temp_Dir); ss->Temp_Dir = NULL;}
      if (ss->Save_Dir) {FREE(ss->Save_Dir); ss->Save_Dir = NULL;}
      if (ss->Ladspa_Dir) {FREE(ss->Ladspa_Dir); ss->Ladspa_Dir = NULL;}
      if (ss->Save_State_File) {FREE(ss->Save_State_File); ss->Save_State_File = NULL;}
      if (ss->Eps_File) {FREE(ss->Eps_File); ss->Eps_File = NULL;}
      if (ss->Listener_Prompt) {FREE(ss->Listener_Prompt); ss->Listener_Prompt = NULL;}
      
      /* not sure about the next two... */
      if ((cursor_style(ss) == CURSOR_PROC) && (XEN_PROCEDURE_P(ss->cursor_proc)))
	snd_unprotect_at(ss->cursor_proc_loc);
      if ((zoom_focus_style(ss) == ZOOM_FOCUS_PROC) && (XEN_PROCEDURE_P(ss->zoom_focus_proc)))
	snd_unprotect_at(ss->zoom_focus_proc_loc);
    }
  ss->Transform_Size = DEFAULT_TRANSFORM_SIZE;
  ss->Minibuffer_History_Length = DEFAULT_MINIBUFFER_HISTORY_LENGTH;
  ss->Fft_Window = DEFAULT_FFT_WINDOW;
  ss->Fft_Window_Alpha = DEFAULT_FFT_WINDOW_ALPHA;
  ss->Fft_Window_Beta = DEFAULT_FFT_WINDOW_BETA;
  ss->Transform_Graph_Type = DEFAULT_TRANSFORM_GRAPH_TYPE;
  ss->Sinc_Width = DEFAULT_SINC_WIDTH;
  ss->Default_Output_Chans = DEFAULT_OUTPUT_CHANS;
  ss->Default_Output_Srate = DEFAULT_OUTPUT_SRATE;
  ss->Default_Output_Header_Type = DEFAULT_OUTPUT_HEADER_TYPE;
  ss->Default_Output_Data_Format = DEFAULT_OUTPUT_DATA_FORMAT;
  ss->Auto_Resize = DEFAULT_AUTO_RESIZE; 
  ss->Auto_Update = DEFAULT_AUTO_UPDATE; 
  ss->Graphs_Horizontal = DEFAULT_GRAPHS_HORIZONTAL;
  ss->Color_Cutoff = DEFAULT_COLOR_CUTOFF;
  ss->Color_Scale = DEFAULT_COLOR_SCALE;
  ss->Color_Inverted = DEFAULT_COLOR_INVERTED;
  ss->Color_Map = DEFAULT_COLOR_MAP;
  ss->Zero_Pad = DEFAULT_ZERO_PAD;
  ss->Ask_Before_Overwrite = DEFAULT_ASK_BEFORE_OVERWRITE;
  ss->X_Axis_Style = DEFAULT_X_AXIS_STYLE;
  ss->Beats_Per_Minute = DEFAULT_BEATS_PER_MINUTE;
  ss->Beats_Per_Measure = DEFAULT_BEATS_PER_MEASURE;
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
  ss->With_File_Monitor = DEFAULT_WITH_FILE_MONITOR;
  ss->Dot_Size = DEFAULT_DOT_SIZE;
  ss->Grid_Density = DEFAULT_GRID_DENSITY;
  ss->Cursor_Size = DEFAULT_CURSOR_SIZE;
  ss->Cursor_Style = DEFAULT_CURSOR_STYLE;
  ss->Tracking_Cursor_Style = DEFAULT_TRACKING_CURSOR_STYLE;
  ss->cursor_proc = XEN_UNDEFINED;
  ss->cursor_proc_loc = NOT_A_GC_LOC;
  ss->Vu_Size = DEFAULT_VU_SIZE;
  ss->Vu_In_dB = DEFAULT_VU_IN_DB;
  ss->Selection_Creates_Region = DEFAULT_SELECTION_CREATES_REGION;
  ss->Transform_Normalization = DEFAULT_TRANSFORM_NORMALIZATION;
  ss->Zoom_Focus_Style = DEFAULT_ZOOM_FOCUS_STYLE;
  ss->zoom_focus_proc = XEN_UNDEFINED;
  ss->zoom_focus_proc_loc = NOT_A_GC_LOC;
  ss->Graph_Style = DEFAULT_GRAPH_STYLE;
  ss->Region_Graph_Style = DEFAULT_GRAPH_STYLE;
  ss->Enved_Filter_Order = DEFAULT_ENVED_FILTER_ORDER;
  ss->Verbose_Cursor = DEFAULT_VERBOSE_CURSOR;
  if (MUS_DEFAULT_TEMP_DIR != (char *)NULL) ss->Temp_Dir = copy_string(MUS_DEFAULT_TEMP_DIR); else ss->Temp_Dir = NULL;
  if (MUS_DEFAULT_SAVE_DIR != (char *)NULL) ss->Save_Dir = copy_string(MUS_DEFAULT_SAVE_DIR); else ss->Save_Dir = NULL;
  if (DEFAULT_LADSPA_DIR != (char *)NULL) ss->Ladspa_Dir = copy_string(DEFAULT_LADSPA_DIR); else ss->Ladspa_Dir = NULL;
  if (DEFAULT_EPS_FILE != (char *)NULL) ss->Eps_File = copy_string(DEFAULT_EPS_FILE); else ss->Eps_File = NULL;
  ss->Eps_Bottom_Margin = DEFAULT_EPS_BOTTOM_MARGIN;
  ss->Eps_Left_Margin = DEFAULT_EPS_LEFT_MARGIN;
  ss->Eps_Size = DEFAULT_EPS_SIZE;
  ss->Listener_Prompt = copy_string(DEFAULT_LISTENER_PROMPT);
  ss->listener_prompt_length = snd_strlen(ss->Listener_Prompt);
  ss->Show_Transform_Peaks = DEFAULT_SHOW_TRANSFORM_PEAKS;
  ss->Show_Y_Zero = DEFAULT_SHOW_Y_ZERO;
  ss->Show_Grid = DEFAULT_SHOW_GRID;
  ss->Show_Sonogram_Cursor = DEFAULT_SHOW_SONOGRAM_CURSOR;
  ss->Show_Axes = DEFAULT_SHOW_AXES;
  ss->Show_Marks = DEFAULT_SHOW_MARKS;
  ss->Show_Indices = DEFAULT_SHOW_INDICES;
  ss->Show_Backtrace = DEFAULT_SHOW_BACKTRACE;
  ss->Clipping = DEFAULT_CLIPPING;
  ss->Fft_Log_Magnitude = DEFAULT_FFT_LOG_MAGNITUDE;
  ss->Fft_Log_Frequency = DEFAULT_FFT_LOG_FREQUENCY;
  ss->Channel_Style = DEFAULT_CHANNEL_STYLE;
  ss->Sound_Style = DEFAULT_SOUND_STYLE;
  ss->Audio_Input_Device = DEFAULT_AUDIO_INPUT_DEVICE;
  ss->Audio_Output_Device = DEFAULT_AUDIO_OUTPUT_DEVICE;
  ss->Optimization = DEFAULT_OPTIMIZATION;
  ss->Print_Length = DEFAULT_PRINT_LENGTH;
  ss->View_Files_Sort = DEFAULT_VIEW_FILES_SORT;
  ss->Show_Mix_Waveforms = DEFAULT_SHOW_MIX_WAVEFORMS;
  ss->Mix_Waveform_Height = DEFAULT_MIX_WAVEFORM_HEIGHT;
  ss->Mix_Tag_Width = DEFAULT_MIX_TAG_WIDTH;
  ss->Mix_Tag_Height = DEFAULT_MIX_TAG_HEIGHT;
  ss->Mark_Tag_Width = DEFAULT_MARK_TAG_WIDTH;
  ss->Mark_Tag_Height = DEFAULT_MARK_TAG_HEIGHT;
  if (DEFAULT_SAVE_STATE_FILE != (char *)NULL) ss->Save_State_File = copy_string(DEFAULT_SAVE_STATE_FILE); else ss->Save_State_File = NULL;
  ss->Enved_Base = DEFAULT_ENVED_BASE;
  ss->Enved_Power = DEFAULT_ENVED_POWER;
  ss->Enved_Wave_p = DEFAULT_ENVED_WAVE_P;
  ss->Enved_Style = DEFAULT_ENVED_STYLE;
  ss->Enved_Target = DEFAULT_ENVED_TARGET;
  ss->Dac_Size = DEFAULT_DAC_SIZE;
  ss->Dac_Combines_Channels = DEFAULT_DAC_COMBINES_CHANNELS;
  ss->Auto_Update_Interval = DEFAULT_AUTO_UPDATE_INTERVAL;
  ss->Cursor_Update_Interval = DEFAULT_CURSOR_UPDATE_INTERVAL;
  ss->Cursor_Location_Offset = DEFAULT_CURSOR_LOCATION_OFFSET;
  ss->Max_Regions = DEFAULT_MAX_REGIONS;
  ss->Max_Transform_Peaks = DEFAULT_MAX_TRANSFORM_PEAKS;
  ss->HTML_Dir = copy_string(DEFAULT_HTML_DIR);
  ss->HTML_Program = copy_string(DEFAULT_HTML_PROGRAM);
  ss->Log_Freq_Start = DEFAULT_LOG_FREQ_START;
  ss->Min_dB = DEFAULT_MIN_DB;
  ss->lin_dB = pow(10.0, DEFAULT_MIN_DB * 0.05);

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
  ss->With_Tracking_Cursor = DEFAULT_WITH_TRACKING_CURSOR;
  ss->Just_Sounds = DEFAULT_JUST_SOUNDS;
}

#if HAVE_GSL
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_errno.h>
/* default gsl error handler apparently aborts main program! */

static void snd_gsl_error(const char *reason, const char *file, int line, int gsl_errno)
{
  XEN_ERROR(XEN_ERROR_TYPE("gsl-error"),
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
    int main(int argc, char **argv)
  #endif
#endif
{
  int i;

#if HAVE_GSL
  /* if HAVE_GSL and the environment variable GSL_IEEE_MODE exists, use it */
  /* GSL_IEEE_MODE=double-precision,mask-underflow,mask-denormalized */
  if (getenv("GSL_IEEE_MODE") != NULL) 
    gsl_ieee_env_setup();
  gsl_set_error_handler(snd_gsl_error);
#endif

#if ENABLE_NLS && HAVE_GETTEXT && defined(LOCALE_DIR)
  /* both flags needed to avoid idiotic confusion on the Sun */
  #if HAVE_SETLOCALE
    setlocale (LC_ALL, "");
  #endif
  bindtextdomain (PACKAGE, LOCALE_DIR);
  textdomain (PACKAGE);
  /*
    (bindtextdomain "snd" "/usr/local/share/locale")
    (textdomain "snd")
    (define _ gettext)
    (display (_ "no selection"))

    but that is limited to Snd's messages
  */
#endif

  ss = (snd_state *)CALLOC(1, sizeof(snd_state));
  ss->fam_ok = false;
  ss->startup_errors = NULL;
  mus_sound_initialize(); /* has to precede version check (mus_audio_moniker needs to be setup in Alsa/Oss) */

#if HAVE_FORTH || HAVE_GAUCHE || HAVE_RUBY
  xen_initialize();
#endif

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "--version") == 0)
	{
	  fprintf(stdout, version_info());
	  snd_exit(0);
	}
      else
	{
	  if (strcmp(argv[i], "--help") == 0)
	    {
	      fprintf(stdout, _("Snd is a sound editor; see http://ccrma.stanford.edu/software/snd/.\n"));
	      fprintf(stdout, version_info());
	      snd_exit(0);
	    }
	}
    }

  initialize_format_lists();
  snd_set_global_defaults(false);
#if MUS_DEBUGGING
  ss->Trap_Segfault = false;
#else
  ss->Trap_Segfault = DEFAULT_TRAP_SEGFAULT;
#endif
  ss->jump_ok = false;
  allocate_regions(max_regions(ss));
  ss->init_window_x = DEFAULT_INIT_WINDOW_X; 
  ss->init_window_y = DEFAULT_INIT_WINDOW_Y; 
  ss->init_window_width = DEFAULT_INIT_WINDOW_WIDTH; 
  ss->init_window_height = DEFAULT_INIT_WINDOW_HEIGHT;
  ss->click_time = 100;
  init_sound_file_extensions();

  ss->max_sounds = 4;                 /* expands to accommodate any number of files */
  ss->sound_sync_max = 0;
  ss->stopped_explicitly = false;     /* C-g sets this flag so that we can interrupt various loops */
  ss->checking_explicitly = false;
  ss->reloading_updated_file = 0;
  ss->selected_sound = NO_SELECTION;
  ss->sounds = (snd_info **)CALLOC(ss->max_sounds, sizeof(snd_info *));
  ss->print_choice = PRINT_SND;
  ss->graph_hook_active = false;
  ss->lisp_graph_hook_active = false;
  ss->error_lock = false;
  ss->exiting = false;
  ss->deferred_regions = 0;
  ss->fam_connection = NULL;
  ss->snd_error_data = NULL;
  ss->snd_error_handler = NULL;
  ss->snd_warning_data = NULL;
  ss->snd_warning_handler = NULL;
  ss->xen_error_data = NULL;
  ss->xen_error_handler = NULL;

#if USE_NO_GUI || HAVE_RUBY || HAVE_FORTH || HAVE_GAUCHE
  ss->catch_exists = 1; /* scm_shell for USE_NO_GUI case */
#else
  ss->catch_exists = 0;
#endif
#if HAVE_GL && MUS_WITH_GL2PS
  ss->gl_printing = false;
#endif
  g_xen_initialize();
  ss->search_proc = XEN_UNDEFINED;
  ss->search_expr = NULL;
  ss->search_tree = NULL;
  mus_error_set_handler(mus_error_to_snd);
  mus_print_set_handler(mus_print_to_snd);

  initialize_load_path(); /* merge SND_PATH entries into the load-path */

#ifdef SND_AS_PD_EXTERNAL
  return;
#else
  #ifdef SND_AS_WIDGET
    return(ss); 
  #else
    snd_doit(argc, argv);
    #if (!HAVE_GUILE)
      return(0);
    #endif
  #endif
#endif
}


#ifndef SND_AS_WIDGET
  #ifndef SND_AS_PD_EXTERNAL
    #if HAVE_GUILE
    int main(int argc, char *argv[])
     {
       scm_boot_guile(argc, argv, snd_main, 0);
       return(0);
     }
    #endif
  #endif
#endif

#ifdef SND_AS_PD_EXTERNAL
int snd_pd_main(void){
  char *argv[]={"pd","-noglob","-noinit"};
  snd_main(NULL,3,argv);
  return 0;
}
#endif

void g_init_base(void)
{
  #define H_mus_error_hook S_mus_error_hook " (error-type error-message):  called upon mus_error. \
If it returns #t, Snd ignores the error (it assumes you've handled it via the hook)."

  mus_error_hook = XEN_DEFINE_HOOK(S_mus_error_hook, 2, H_mus_error_hook);       /* arg = error-type error-message */
}
