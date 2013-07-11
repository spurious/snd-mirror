/* Sound display/edit/etc
 *
 * originally intended as a re-implementation of my much-missed dpysnd -- the Foonly/SAIL/E/Mus10/Grnlib sound editor from ca 1983.
 */

#include "snd.h"

snd_state *ss = NULL;

static bool ignore_mus_error(int type, char *msg)
{
  XEN result = XEN_FALSE;

  if (XEN_HOOKED(ss->mus_error_hook))
    result = run_or_hook(ss->mus_error_hook, 
			 XEN_LIST_2(C_TO_XEN_INT(type), 
				    C_TO_XEN_STRING(msg)),
			 S_mus_error_hook);
  return(XEN_TRUE_P(result));
}

#if HAVE_SETJMP_H
  void top_level_catch(int ignore);
#endif


static void mus_error_to_snd(int type, char *msg)
{
  if (!ss)
    {
      fprintf(stderr, "%s", msg);
      return;
    }

  if (!(ignore_mus_error(type, msg)))
    {
#if HAVE_EXTENSION_LANGUAGE
      if (msg == NULL)
	XEN_ERROR(XEN_ERROR_TYPE("mus-error"),
		  XEN_LIST_1(C_TO_XEN_STRING((char *)mus_error_type_to_string(type))));
      else XEN_ERROR(XEN_ERROR_TYPE("mus-error"),
		     XEN_LIST_1(C_TO_XEN_STRING(msg)));
#endif
      snd_error("%s: %s", mus_error_type_to_string(type), msg);
#if HAVE_SETJMP_H
      ss->jump_ok = true;
      top_level_catch(1); /* sigh -- try to keep going */
#endif
    }
}


static void mus_print_to_snd(char *msg)
{
  if (!ss)
    {
      fprintf(stderr, "%s", msg);
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

      dirnames = (char **)calloc(dirs, sizeof(char *));
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

		  tmp = (char *)calloc(lim - start + 1, sizeof(char));
		  for (j = start; j < lim; j++)
		    tmp[j - start] = path[j];

		  dirnames[curdir++] = mus_expand_filename(tmp);
		  start = i + 1;
		  free(tmp);
		}
	    }
	}

      for (i = curdir - 1; i >= 0; i--)
	{
	  XEN_ADD_TO_LOAD_PATH(dirnames[i]);
	  free(dirnames[i]);
	}
      free(dirnames);
    }
}


void snd_set_global_defaults(bool need_cleanup)
{
  if (need_cleanup)
    {
      if (ss->HTML_Program) {free(ss->HTML_Program); ss->HTML_Program = NULL;}
      if (ss->HTML_Dir) {free(ss->HTML_Dir); ss->HTML_Dir = NULL;}
      if (ss->Temp_Dir) {free(ss->Temp_Dir); ss->Temp_Dir = NULL;}
      if (ss->Save_Dir) {free(ss->Save_Dir); ss->Save_Dir = NULL;}
      if (ss->Ladspa_Dir) {free(ss->Ladspa_Dir); ss->Ladspa_Dir = NULL;}
      if (ss->Save_State_File) {free(ss->Save_State_File); ss->Save_State_File = NULL;}
      if (ss->Eps_File) {free(ss->Eps_File); ss->Eps_File = NULL;}
      if (ss->Listener_Prompt) {free(ss->Listener_Prompt); ss->Listener_Prompt = NULL;}
      if (ss->Open_File_Dialog_Directory) {free(ss->Open_File_Dialog_Directory); ss->Open_File_Dialog_Directory = NULL;}
      
      /* not sure about the next two... */
      if ((cursor_style(ss) == CURSOR_PROC) && (XEN_PROCEDURE_P(ss->cursor_proc)))
	snd_unprotect_at(ss->cursor_proc_loc);
      if ((zoom_focus_style(ss) == ZOOM_FOCUS_PROC) && (XEN_PROCEDURE_P(ss->zoom_focus_proc)))
	snd_unprotect_at(ss->zoom_focus_proc_loc);
    }

  ss->Transform_Size =              DEFAULT_TRANSFORM_SIZE;
  ss->Fft_Window =                  DEFAULT_FFT_WINDOW;
  ss->Fft_Window_Alpha =            DEFAULT_FFT_WINDOW_ALPHA;
  ss->Fft_Window_Beta =             DEFAULT_FFT_WINDOW_BETA;
  ss->Transform_Graph_Type =        DEFAULT_TRANSFORM_GRAPH_TYPE;
  ss->Sinc_Width =                  DEFAULT_SINC_WIDTH;
  ss->Zero_Pad =                    DEFAULT_ZERO_PAD;
  ss->Wavelet_Type =                DEFAULT_WAVELET_TYPE;
  ss->Transform_Type =              DEFAULT_TRANSFORM_TYPE;
  ss->Transform_Normalization =     DEFAULT_TRANSFORM_NORMALIZATION;
  ss->Show_Transform_Peaks =        DEFAULT_SHOW_TRANSFORM_PEAKS;
  ss->Show_Sonogram_Cursor =        DEFAULT_SHOW_SONOGRAM_CURSOR;
  ss->Fft_Log_Magnitude =           DEFAULT_FFT_LOG_MAGNITUDE;
  ss->Fft_Log_Frequency =           DEFAULT_FFT_LOG_FREQUENCY;
  ss->Fft_With_Phases =             DEFAULT_FFT_WITH_PHASES;
  ss->Max_Transform_Peaks =         DEFAULT_MAX_TRANSFORM_PEAKS;
  ss->Log_Freq_Start =              DEFAULT_LOG_FREQ_START;
  ss->Min_dB =                      DEFAULT_MIN_DB;
  ss->lin_dB =                      pow(10.0, DEFAULT_MIN_DB * 0.05);
  ss->Show_Selection_Transform =    DEFAULT_SHOW_SELECTION_TRANSFORM;
  ss->Default_Output_Chans =        DEFAULT_OUTPUT_CHANS;
  ss->Default_Output_Srate =        DEFAULT_OUTPUT_SRATE;
  ss->Default_Output_Header_Type =  DEFAULT_OUTPUT_HEADER_TYPE;
  ss->Default_Output_Data_Format =  DEFAULT_OUTPUT_DATA_FORMAT;
  ss->Audio_Input_Device =          DEFAULT_AUDIO_INPUT_DEVICE;
  ss->Audio_Output_Device =         DEFAULT_AUDIO_OUTPUT_DEVICE;
  ss->Dac_Size =                    DEFAULT_DAC_SIZE;
  ss->Dac_Combines_Channels =       DEFAULT_DAC_COMBINES_CHANNELS;
  ss->Auto_Resize =                 DEFAULT_AUTO_RESIZE; 
  ss->Auto_Update =                 DEFAULT_AUTO_UPDATE; 
  ss->Auto_Update_Interval =        DEFAULT_AUTO_UPDATE_INTERVAL;
  ss->Ask_Before_Overwrite =        DEFAULT_ASK_BEFORE_OVERWRITE;
  ss->With_Toolbar =                DEFAULT_WITH_TOOLBAR;
  ss->With_Tooltips =               DEFAULT_WITH_TOOLTIPS;
  ss->Remember_Sound_State =        DEFAULT_REMEMBER_SOUND_STATE;
  ss->Ask_About_Unsaved_Edits =     DEFAULT_ASK_ABOUT_UNSAVED_EDITS;
  ss->Save_As_Dialog_Src =          DEFAULT_SAVE_AS_DIALOG_SRC;
  ss->Save_As_Dialog_Auto_Comment = DEFAULT_SAVE_AS_DIALOG_AUTO_COMMENT;
  ss->Show_Full_Duration =          DEFAULT_SHOW_FULL_DURATION;
  ss->Show_Full_Range =             DEFAULT_SHOW_FULL_RANGE;
  ss->Initial_Beg =                 DEFAULT_INITIAL_BEG;
  ss->Initial_Dur =                 DEFAULT_INITIAL_DUR;
  ss->With_Background_Processes =   DEFAULT_WITH_BACKGROUND_PROCESSES;
  ss->With_File_Monitor =           DEFAULT_WITH_FILE_MONITOR;
  ss->Selection_Creates_Region =    DEFAULT_SELECTION_CREATES_REGION;
  ss->Channel_Style =               DEFAULT_CHANNEL_STYLE;
  ss->Sound_Style =                 DEFAULT_SOUND_STYLE;
  ss->Graphs_Horizontal =           DEFAULT_GRAPHS_HORIZONTAL;
  ss->Graph_Style =                 DEFAULT_GRAPH_STYLE;
  ss->Region_Graph_Style =          DEFAULT_GRAPH_STYLE;
  ss->Time_Graph_Type =             DEFAULT_TIME_GRAPH_TYPE;
  ss->X_Axis_Style =                DEFAULT_X_AXIS_STYLE;
  ss->Beats_Per_Minute =            DEFAULT_BEATS_PER_MINUTE;
  ss->Beats_Per_Measure =           DEFAULT_BEATS_PER_MEASURE;
  ss->With_Relative_Panes =         DEFAULT_WITH_RELATIVE_PANES;
  ss->With_GL =                     DEFAULT_WITH_GL;
  ss->Dot_Size =                    DEFAULT_DOT_SIZE;
  ss->Grid_Density =                DEFAULT_GRID_DENSITY;
  ss->Zoom_Focus_Style =            DEFAULT_ZOOM_FOCUS_STYLE;
  ss->zoom_focus_proc =             XEN_UNDEFINED;
  ss->zoom_focus_proc_loc =         NOT_A_GC_LOC;
  ss->Max_Regions =                 DEFAULT_MAX_REGIONS;
  ss->Show_Y_Zero =                 DEFAULT_SHOW_Y_ZERO;
  ss->Show_Grid =                   DEFAULT_SHOW_GRID;
  ss->Show_Axes =                   DEFAULT_SHOW_AXES;
  ss->Show_Indices =                DEFAULT_SHOW_INDICES;
  ss->Show_Backtrace =              DEFAULT_SHOW_BACKTRACE;
  ss->With_Inset_Graph =            DEFAULT_WITH_INSET_GRAPH;
  ss->With_Interrupts =             DEFAULT_WITH_INTERRUPTS;
  ss->With_Menu_Icons =             DEFAULT_WITH_MENU_ICONS;
  ss->With_Smpte_Label =            DEFAULT_WITH_SMPTE_LABEL;
  ss->With_Pointer_Focus =          DEFAULT_WITH_POINTER_FOCUS;
  ss->Play_Arrow_Size =             DEFAULT_PLAY_ARROW_SIZE;
  ss->Sync_Style =                  DEFAULT_SYNC_STYLE;
  ss->Listener_Prompt =             mus_strdup(DEFAULT_LISTENER_PROMPT);
  ss->listener_prompt_length =      mus_strlen(ss->Listener_Prompt);
  ss->Clipping =                    DEFAULT_CLIPPING;
  ss->Print_Length =                DEFAULT_PRINT_LENGTH;
  ss->View_Files_Sort =             DEFAULT_VIEW_FILES_SORT;
  ss->Just_Sounds =                 DEFAULT_JUST_SOUNDS;
  ss->Open_File_Dialog_Directory =  NULL;
  ss->HTML_Dir =                    mus_strdup(DEFAULT_HTML_DIR);
  ss->HTML_Program =                mus_strdup(DEFAULT_HTML_PROGRAM);
  ss->Cursor_Size =                 DEFAULT_CURSOR_SIZE;
  ss->Cursor_Style =                DEFAULT_CURSOR_STYLE;
  ss->Tracking_Cursor_Style =       DEFAULT_TRACKING_CURSOR_STYLE;
  ss->With_Tracking_Cursor =        DEFAULT_WITH_TRACKING_CURSOR;
  ss->cursor_proc =                 XEN_UNDEFINED;
  ss->cursor_proc_loc =             NOT_A_GC_LOC;
  ss->Verbose_Cursor =              DEFAULT_VERBOSE_CURSOR;
  ss->Cursor_Update_Interval =      DEFAULT_CURSOR_UPDATE_INTERVAL;
  ss->Cursor_Location_Offset =      DEFAULT_CURSOR_LOCATION_OFFSET;
  ss->Show_Mix_Waveforms =          DEFAULT_SHOW_MIX_WAVEFORMS;
  ss->Mix_Waveform_Height =         DEFAULT_MIX_WAVEFORM_HEIGHT;
  ss->Mix_Tag_Width =               DEFAULT_MIX_TAG_WIDTH;
  ss->Mix_Tag_Height =              DEFAULT_MIX_TAG_HEIGHT;
  ss->With_Mix_Tags =               DEFAULT_WITH_MIX_TAGS;
  ss->Mark_Tag_Width =              DEFAULT_MARK_TAG_WIDTH;
  ss->Mark_Tag_Height =             DEFAULT_MARK_TAG_HEIGHT;
  ss->Show_Marks =                  DEFAULT_SHOW_MARKS;
  ss->Color_Map =                   DEFAULT_COLOR_MAP;
  ss->Color_Map_Size =              DEFAULT_COLOR_MAP_SIZE;
  ss->Color_Cutoff =                DEFAULT_COLOR_CUTOFF;
  ss->Color_Scale =                 DEFAULT_COLOR_SCALE;
  ss->Color_Inverted =              DEFAULT_COLOR_INVERTED;
  ss->Color_Map =                   DEFAULT_COLOR_MAP;
  ss->Wavo_Hop =                    DEFAULT_WAVO_HOP;
  ss->Wavo_Trace =                  DEFAULT_WAVO_TRACE;
  ss->Spectro_Hop =                 DEFAULT_SPECTRO_HOP;
  ss->Spectro_X_Scale =             DEFAULT_SPECTRO_X_SCALE;
  ss->Spectro_Y_Scale =             DEFAULT_SPECTRO_Y_SCALE;
  ss->Spectro_Z_Scale =             DEFAULT_SPECTRO_Z_SCALE;
  ss->Spectro_Z_Angle =             DEFAULT_SPECTRO_Z_ANGLE;
  ss->Spectro_X_Angle =             DEFAULT_SPECTRO_X_ANGLE;
  ss->Spectro_Y_Angle =             DEFAULT_SPECTRO_Y_ANGLE;
  ss->Spectrum_End =                DEFAULT_SPECTRUM_END;
  ss->Spectrum_Start =              DEFAULT_SPECTRUM_START;
  ss->Enved_Base =                  DEFAULT_ENVED_BASE;
  ss->Enved_Power =                 DEFAULT_ENVED_POWER;
  ss->Enved_Wave_p =                DEFAULT_ENVED_WAVE_P;
  ss->Enved_Style =                 DEFAULT_ENVED_STYLE;
  ss->Enved_Target =                DEFAULT_ENVED_TARGET;
  ss->Enved_Filter_Order =          DEFAULT_ENVED_FILTER_ORDER;
  ss->Eps_Bottom_Margin =           DEFAULT_EPS_BOTTOM_MARGIN;
  ss->Eps_Left_Margin =             DEFAULT_EPS_LEFT_MARGIN;
  ss->Eps_Size =                    DEFAULT_EPS_SIZE;
  ss->Expand_Control_Min =          DEFAULT_EXPAND_CONTROL_MIN;
  ss->Expand_Control_Max =          DEFAULT_EXPAND_CONTROL_MAX;
  ss->Amp_Control_Min =             DEFAULT_AMP_CONTROL_MIN;
  ss->Amp_Control_Max =             DEFAULT_AMP_CONTROL_MAX;
  ss->Speed_Control_Min =           DEFAULT_SPEED_CONTROL_MIN;
  ss->Speed_Control_Max =           DEFAULT_SPEED_CONTROL_MAX;
  ss->Contrast_Control_Min =        DEFAULT_CONTRAST_CONTROL_MIN;
  ss->Contrast_Control_Max =        DEFAULT_CONTRAST_CONTROL_MAX;
  ss->Contrast_Control_Amp =        DEFAULT_CONTRAST_CONTROL_AMP;
  ss->Expand_Control_Length =       DEFAULT_EXPAND_CONTROL_LENGTH;
  ss->Expand_Control_Ramp =         DEFAULT_EXPAND_CONTROL_RAMP;
  ss->Expand_Control_Hop =          DEFAULT_EXPAND_CONTROL_HOP;
  ss->Expand_Control_Jitter =       DEFAULT_EXPAND_CONTROL_JITTER;
  ss->Reverb_Control_Feedback =     DEFAULT_REVERB_CONTROL_FEEDBACK;
  ss->Reverb_Control_Lowpass =      DEFAULT_REVERB_CONTROL_LOWPASS;
  ss->Reverb_Control_Scale_Min =    DEFAULT_REVERB_CONTROL_SCALE_MIN;
  ss->Reverb_Control_Scale_Max =    DEFAULT_REVERB_CONTROL_SCALE_MAX;
  ss->Reverb_Control_Decay =        DEFAULT_REVERB_CONTROL_DECAY;
  ss->Speed_Control_Tones =         DEFAULT_SPEED_CONTROL_TONES;
  ss->Speed_Control_Style =         DEFAULT_SPEED_CONTROL_STYLE;
  ss->Reverb_Control_Length_Min =   DEFAULT_REVERB_CONTROL_LENGTH_MIN;
  ss->Reverb_Control_Length_Max =   DEFAULT_REVERB_CONTROL_LENGTH_MAX;
  ss->Filter_Control_Order =        DEFAULT_FILTER_CONTROL_ORDER;
  ss->Filter_Control_In_Db =        DEFAULT_FILTER_CONTROL_IN_DB;
  ss->Filter_Control_In_Hz =        DEFAULT_FILTER_CONTROL_IN_HZ;
  ss->Show_Controls =               DEFAULT_SHOW_CONTROLS;

  if (MUS_DEFAULT_TEMP_DIR != (char *)NULL) 
    ss->Temp_Dir = mus_strdup(MUS_DEFAULT_TEMP_DIR); 
  else ss->Temp_Dir = NULL;
  
  if (MUS_DEFAULT_SAVE_DIR != (char *)NULL) 
    ss->Save_Dir = mus_strdup(MUS_DEFAULT_SAVE_DIR); 
  else ss->Save_Dir = NULL;

  if (DEFAULT_LADSPA_DIR != (char *)NULL) 
    ss->Ladspa_Dir = mus_strdup(DEFAULT_LADSPA_DIR); 
  else ss->Ladspa_Dir = NULL;

  if (DEFAULT_SAVE_STATE_FILE != (char *)NULL) 
    ss->Save_State_File = mus_strdup(DEFAULT_SAVE_STATE_FILE); 
  else ss->Save_State_File = NULL;

  if (DEFAULT_PEAK_ENV_DIR != (char *)NULL) 
    ss->Peak_Env_Dir = mus_strdup(DEFAULT_PEAK_ENV_DIR); 
  else ss->Peak_Env_Dir = NULL;
  
  if (DEFAULT_EPS_FILE != (char *)NULL) 
    ss->Eps_File = mus_strdup(DEFAULT_EPS_FILE);
  else ss->Eps_File = NULL;
}


#if HAVE_GSL
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_errno.h>

/* default gsl error handler apparently aborts main program! */

static void snd_gsl_error(const char *reason, const char *file, int line, int gsl_errno)
{
  XEN_ERROR(XEN_ERROR_TYPE("gsl-error"),
	    XEN_LIST_6(C_TO_XEN_STRING("GSL: ~A, ~A in ~A line ~A, gsl err: ~A"),
		       C_TO_XEN_STRING(gsl_strerror(gsl_errno)),
		       C_TO_XEN_STRING(reason),
		       C_TO_XEN_STRING(file),
		       C_TO_XEN_INT(line),
		       C_TO_XEN_INT(gsl_errno)));
}
#endif


#if SND_AS_WIDGET
  snd_state *snd_main(int argc, char **argv)
#else
  int main(int argc, char **argv)
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

  ss = (snd_state *)calloc(1, sizeof(snd_state)); /* not calloc! */
  ss->startup_errors = NULL;

#if HAVE_GTK_3
#if !GLIB_CHECK_VERSION(2,35,0)
  g_type_init();
#endif
#endif

  mus_sound_initialize(); /* has to precede version check (mus_audio_moniker needs to be setup in Alsa/Oss) */
  xen_initialize();

  for (i = 1; i < argc; i++)
    {
      if (strcmp(argv[i], "--version") == 0)
	{
	  fprintf(stdout, "%s", version_info());
	  snd_exit(0);
	}
      else
	{
	  if (strcmp(argv[i], "--help") == 0)
	    {
	      fprintf(stdout, "%s", "Snd is a sound editor; see http://ccrma.stanford.edu/software/snd/.\n");
	      fprintf(stdout, "%s", version_info());
	      snd_exit(0);
	    }
	}
    }

  initialize_format_lists();
  snd_set_global_defaults(false);

  ss->jump_ok = false;
  ss->file_monitor_ok = false;
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
  ss->selection_play_stop = false;
  ss->reloading_updated_file = 0;
  ss->selected_sound = NO_SELECTION;
  ss->sounds = (snd_info **)calloc(ss->max_sounds, sizeof(snd_info *));
  ss->print_choice = PRINT_SND;
  ss->graph_hook_active = false;
  ss->lisp_graph_hook_active = false;
  ss->exiting = false;
  ss->deferred_regions = 0;
  ss->snd_error_data = NULL;
  ss->snd_error_handler = NULL;
  ss->snd_warning_data = NULL;
  ss->snd_warning_handler = NULL;
  ss->xen_error_data = NULL;
  ss->xen_error_handler = NULL;
  ss->update_sound_channel_style = NOT_A_CHANNEL_STYLE;
  ss->squelch_mark_drag_info = false;

#if HAVE_GL && WITH_GL2PS
  ss->gl_printing = false;
#endif
  g_xen_initialize();
  ss->search_proc = XEN_UNDEFINED;
  ss->search_expr = NULL;
  mus_error_set_handler(mus_error_to_snd);
  mus_print_set_handler(mus_print_to_snd);

  initialize_load_path(); /* merge SND_PATH entries into the load-path */

#ifdef SND_AS_WIDGET
  return(ss); 
#else
  snd_doit(argc, argv);
  return(0);
#endif
}


void g_init_base(void)
{
  #define H_mus_error_hook S_mus_error_hook " (type message):  called upon mus_error. \
If it returns " PROC_TRUE ", Snd ignores the error (it assumes you've handled it via the hook)."

  ss->mus_error_hook = XEN_DEFINE_HOOK(S_mus_error_hook, "(make-hook 'type 'message)", 2, H_mus_error_hook);
}
