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

#ifndef _MSC_VER
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
#ifndef _MSC_VER
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
  ss->With_Verbose_Cursor =         DEFAULT_WITH_VERBOSE_CURSOR;
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

#if HAVE_SCHEME
  ss->show_transform_peaks_symbol = s7_define_variable(s7, "*" S_show_transform_peaks "*", s7_make_boolean(s7, DEFAULT_SHOW_TRANSFORM_PEAKS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_transform_peaks "*) (list #f (lambda (s v) (set! (" S_show_transform_peaks ") v)) #f))");
  s7_symbol_set_documentation(s7, ss->show_transform_peaks_symbol, "*" S_show_transform_peaks "* determines whether fft displays include a peak list");

  ss->show_y_zero_symbol = s7_define_variable(s7, "*" S_show_y_zero "*", s7_make_boolean(s7, DEFAULT_SHOW_Y_ZERO));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_y_zero "*) (list #f (lambda (s v) (set! (" S_show_y_zero ") v)) #f))");

  ss->show_marks_symbol = s7_define_variable(s7, "*" S_show_marks "*", s7_make_boolean(s7, DEFAULT_SHOW_MARKS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_marks "*) (list #f (lambda (s v) (set! (" S_show_marks ") v)) #f))");

  ss->show_grid_symbol = s7_define_variable(s7, "*" S_show_grid "*", s7_make_boolean(s7, DEFAULT_SHOW_GRID));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_grid "*) (list #f (lambda (s v) (set! (" S_show_grid ") v)) #f))");

  ss->fft_log_frequency_symbol = s7_define_variable(s7, "*" S_fft_log_frequency "*", s7_make_real(s7, DEFAULT_FFT_LOG_FREQUENCY));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_log_frequency "*) (list #f (lambda (s v) (set! (" S_fft_log_frequency ") v)) #f))");

  ss->fft_log_magnitude_symbol = s7_define_variable(s7, "*" S_fft_log_magnitude "*", s7_make_real(s7, DEFAULT_FFT_LOG_MAGNITUDE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_log_magnitude "*) (list #f (lambda (s v) (set! (" S_fft_log_magnitude ") v)) #f))");

  ss->fft_with_phases_symbol = s7_define_variable(s7, "*" S_fft_with_phases "*", s7_make_boolean(s7, DEFAULT_FFT_WITH_PHASES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_with_phases "*) (list #f (lambda (s v) (set! (" S_fft_with_phases ") v)) #f))");

  ss->channel_style_symbol = s7_define_variable(s7, "*" S_channel_style "*", s7_make_integer(s7, DEFAULT_CHANNEL_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_channel_style "*) (list #f (lambda (s v) (set! (" S_channel_style ") v)) #f))");

  ss->sync_style_symbol = s7_define_variable(s7, "*" S_sync_style "*", s7_make_integer(s7, DEFAULT_SYNC_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_sync_style "*) (list #f (lambda (s v) (set! (" S_sync_style ") v)) #f))");

  ss->show_axes_symbol = s7_define_variable(s7, "*" S_show_axes "*", s7_make_integer(s7, DEFAULT_SHOW_AXES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_axes "*) (list #f (lambda (s v) (set! (" S_show_axes ") v)) #f))");

  ss->eps_file_symbol = s7_define_variable(s7, "*" S_eps_file "*", s7_make_string(s7, DEFAULT_EPS_FILE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_eps_file "*) (list #f (lambda (s v) (set! (" S_eps_file ") v)) #f))");

  ss->temp_dir_symbol = s7_define_variable(s7, "*" S_temp_dir "*", s7_make_string(s7, ss->Temp_Dir));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_temp_dir "*) (list #f (lambda (s v) (set! (" S_temp_dir ") v)) #f))");

  ss->save_dir_symbol = s7_define_variable(s7, "*" S_save_dir "*", s7_make_string(s7, ss->Save_Dir));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_save_dir "*) (list #f (lambda (s v) (set! (" S_save_dir ") v)) #f))");

  ss->ladspa_dir_symbol = s7_define_variable(s7, "*" S_ladspa_dir "*", s7_make_string(s7, DEFAULT_LADSPA_DIR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_ladspa_dir "*) (list #f (lambda (s v) (set! (" S_ladspa_dir ") v)) #f))");

  ss->peak_env_dir_symbol = s7_define_variable(s7, "*" S_peak_env_dir "*", s7_make_string(s7, DEFAULT_PEAK_ENV_DIR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_peak_env_dir "*) (list #f (lambda (s v) (set! (" S_peak_env_dir ") v)) #f))");

#if USE_MOTIF
  #define DEFAULT_LISTENER_FONT "9x15"
#endif
#if (!USE_NO_GUI)
  ss->listener_font_symbol = s7_define_variable(s7, "*" S_listener_font "*", s7_make_string(s7, DEFAULT_LISTENER_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_listener_font "*) (list #f (lambda (s v) (set! (" S_listener_font ") v)) #f))");
#else
  ss->listener_font_symbol = s7_define_variable(s7, "*" S_listener_font "*", s7_make_boolean(s7, s7_f(s7)));
#endif

  ss->axis_label_font_symbol = s7_define_variable(s7, "*" S_axis_label_font "*", s7_make_string(s7, DEFAULT_AXIS_LABEL_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_axis_label_font "*) (list #f (lambda (s v) (set! (" S_axis_label_font ") v)) #f))");

  ss->axis_numbers_font_symbol = s7_define_variable(s7, "*" S_axis_numbers_font "*", s7_make_string(s7, DEFAULT_AXIS_NUMBERS_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_axis_numbers_font "*) (list #f (lambda (s v) (set! (" S_axis_numbers_font ") v)) #f))");

  ss->tiny_font_symbol = s7_define_variable(s7, "*" S_tiny_font "*", s7_make_string(s7, DEFAULT_TINY_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_tiny_font "*) (list #f (lambda (s v) (set! (" S_tiny_font ") v)) #f))");

  ss->peaks_font_symbol = s7_define_variable(s7, "*" S_peaks_font "*", s7_make_string(s7, DEFAULT_PEAKS_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_peaks_font "*) (list #f (lambda (s v) (set! (" S_peaks_font ") v)) #f))");

  ss->bold_peaks_font_symbol = s7_define_variable(s7, "*" S_bold_peaks_font "*", s7_make_string(s7, DEFAULT_BOLD_PEAKS_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_bold_peaks_font "*) (list #f (lambda (s v) (set! (" S_bold_peaks_font ") v)) #f))");

  ss->with_verbose_cursor_symbol = s7_define_variable(s7, "*" S_with_verbose_cursor "*", s7_make_boolean(s7, DEFAULT_WITH_VERBOSE_CURSOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_verbose_cursor "*) (list #f (lambda (s v) (set! (" S_with_verbose_cursor ") v)) #f))");

  ss->with_inset_graph_symbol = s7_define_variable(s7, "*" S_with_inset_graph "*", s7_make_boolean(s7, DEFAULT_WITH_INSET_GRAPH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_inset_graph "*) (list #f (lambda (s v) (set! (" S_with_inset_graph ") v)) #f))");

  ss->with_pointer_focus_symbol = s7_define_variable(s7, "*" S_with_pointer_focus "*", s7_make_boolean(s7, DEFAULT_WITH_POINTER_FOCUS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_pointer_focus "*) (list #f (lambda (s v) (set! (" S_with_pointer_focus ") v)) #f))");

  ss->with_smpte_label_symbol = s7_define_variable(s7, "*" S_with_smpte_label "*", s7_make_boolean(s7, DEFAULT_WITH_SMPTE_LABEL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_smpte_label "*) (list #f (lambda (s v) (set! (" S_with_smpte_label ") v)) #f))");

  ss->with_interrupts_symbol = s7_define_variable(s7, "*" S_with_interrupts "*", s7_make_boolean(s7, DEFAULT_WITH_INTERRUPTS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_interrupts "*) (list #f (lambda (s v) (set! (" S_with_interrupts ") v)) #f))");

  ss->enved_filter_order_symbol = s7_define_variable(s7, "*" S_enved_filter_order "*", s7_make_integer(s7, DEFAULT_ENVED_FILTER_ORDER));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_filter_order "*) (list #f (lambda (s v) (set! (" S_enved_filter_order ") v)) #f))");

  ss->eps_left_margin_symbol = s7_define_variable(s7, "*" S_eps_left_margin "*", s7_make_real(s7, DEFAULT_EPS_LEFT_MARGIN));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_eps_left_margin "*) (list #f (lambda (s v) (set! (" S_eps_left_margin ") v)) #f))");

  ss->eps_bottom_margin_symbol = s7_define_variable(s7, "*" S_eps_bottom_margin "*", s7_make_real(s7, DEFAULT_EPS_BOTTOM_MARGIN));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_eps_bottom_margin "*) (list #f (lambda (s v) (set! (" S_eps_bottom_margin ") v)) #f))");

  ss->eps_size_symbol = s7_define_variable(s7, "*" S_eps_size "*", s7_make_real(s7, DEFAULT_EPS_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_eps_size "*) (list #f (lambda (s v) (set! (" S_eps_size ") v)) #f))");

  ss->log_freq_start_symbol = s7_define_variable(s7, "*" S_log_freq_start "*", s7_make_real(s7, DEFAULT_LOG_FREQ_START));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_log_freq_start "*) (list #f (lambda (s v) (set! (" S_log_freq_start ") v)) #f))");

  ss->spectro_x_scale_symbol = s7_define_variable(s7, "*" S_spectro_x_scale "*", s7_make_real(s7, DEFAULT_SPECTRO_X_SCALE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_x_scale "*) (list #f (lambda (s v) (set! (" S_spectro_x_scale ") v)) #f))");

  ss->spectro_y_scale_symbol = s7_define_variable(s7, "*" S_spectro_y_scale "*", s7_make_real(s7, DEFAULT_SPECTRO_Y_SCALE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_y_scale "*) (list #f (lambda (s v) (set! (" S_spectro_y_scale ") v)) #f))");

  ss->spectro_z_scale_symbol = s7_define_variable(s7, "*" S_spectro_z_scale "*", s7_make_real(s7, DEFAULT_SPECTRO_Z_SCALE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_z_scale "*) (list #f (lambda (s v) (set! (" S_spectro_z_scale ") v)) #f))");

  ss->spectro_z_angle_symbol = s7_define_variable(s7, "*" S_spectro_z_angle "*", s7_make_real(s7, DEFAULT_SPECTRO_Z_ANGLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_z_angle "*) (list #f (lambda (s v) (set! (" S_spectro_z_angle ") v)) #f))");

  ss->spectro_x_angle_symbol = s7_define_variable(s7, "*" S_spectro_x_angle "*", s7_make_real(s7, DEFAULT_SPECTRO_X_ANGLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_x_angle "*) (list #f (lambda (s v) (set! (" S_spectro_x_angle ") v)) #f))");

  ss->spectro_y_angle_symbol = s7_define_variable(s7, "*" S_spectro_y_angle "*", s7_make_real(s7, DEFAULT_SPECTRO_Y_ANGLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_y_angle "*) (list #f (lambda (s v) (set! (" S_spectro_y_angle ") v)) #f))");

  ss->spectrum_end_symbol = s7_define_variable(s7, "*" S_spectrum_end "*", s7_make_real(s7, DEFAULT_SPECTRUM_END));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectrum_end "*) (list #f (lambda (s v) (set! (" S_spectrum_end ") v)) #f))");

  ss->spectrum_start_symbol = s7_define_variable(s7, "*" S_spectrum_start "*", s7_make_real(s7, DEFAULT_SPECTRUM_START));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectrum_start "*) (list #f (lambda (s v) (set! (" S_spectrum_start ") v)) #f))");

  ss->default_output_header_type_symbol = s7_define_variable(s7, "*" S_default_output_header_type "*", s7_make_integer(s7, DEFAULT_OUTPUT_HEADER_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_default_output_header_type "*) (list #f (lambda (s v) (set! (" S_default_output_header_type ") v)) #f))");

  ss->default_output_data_format_symbol = s7_define_variable(s7, "*" S_default_output_data_format "*", s7_make_integer(s7, DEFAULT_OUTPUT_DATA_FORMAT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_default_output_data_format "*) (list #f (lambda (s v) (set! (" S_default_output_data_format ") v)) #f))");

  ss->default_output_chans_symbol = s7_define_variable(s7, "*" S_default_output_chans "*", s7_make_integer(s7, DEFAULT_OUTPUT_CHANS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_default_output_chans "*) (list #f (lambda (s v) (set! (" S_default_output_chans ") v)) #f))");

  ss->default_output_srate_symbol = s7_define_variable(s7, "*" S_default_output_srate "*", s7_make_integer(s7, DEFAULT_OUTPUT_SRATE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_default_output_srate "*) (list #f (lambda (s v) (set! (" S_default_output_srate ") v)) #f))");

  ss->spectro_hop_symbol = s7_define_variable(s7, "*" S_spectro_hop "*", s7_make_integer(s7, DEFAULT_SPECTRO_HOP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_hop "*) (list #f (lambda (s v) (set! (" S_spectro_hop ") v)) #f))");

  ss->color_map_symbol = s7_define_variable(s7, "*" S_colormap "*", s7_make_integer(s7, DEFAULT_COLOR_MAP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_colormap "*) (list #f (lambda (s v) (set! (" S_colormap ") v)) #f))");

  ss->color_map_size_symbol = s7_define_variable(s7, "*" S_colormap_size "*", s7_make_integer(s7, DEFAULT_COLOR_MAP_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_colormap_size "*) (list #f (lambda (s v) (set! (" S_colormap_size ") v)) #f))");

  ss->wavelet_type_symbol = s7_define_variable(s7, "*" S_wavelet_type "*", s7_make_integer(s7, DEFAULT_WAVELET_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_wavelet_type "*) (list #f (lambda (s v) (set! (" S_wavelet_type ") v)) #f))");

  ss->transform_type_symbol = s7_define_variable(s7, "*" S_transform_type "*", s7_make_integer(s7, DEFAULT_TRANSFORM_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_transform_type "*) (list #f (lambda (s v) (set! (" S_transform_type ") v)) #f))");

  ss->dot_size_symbol = s7_define_variable(s7, "*" S_dot_size "*", s7_make_integer(s7, DEFAULT_DOT_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_dot_size "*) (list #f (lambda (s v) (set! (" S_dot_size ") v)) #f))");

  ss->zero_pad_symbol = s7_define_variable(s7, "*" S_zero_pad "*", s7_make_integer(s7, DEFAULT_ZERO_PAD));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_zero_pad "*) (list #f (lambda (s v) (set! (" S_zero_pad ") v)) #f))");

  ss->wavo_hop_symbol = s7_define_variable(s7, "*" S_wavo_hop "*", s7_make_integer(s7, DEFAULT_WAVO_HOP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_wavo_hop "*) (list #f (lambda (s v) (set! (" S_wavo_hop ") v)) #f))");

  ss->wavo_trace_symbol = s7_define_variable(s7, "*" S_wavo_trace "*", s7_make_integer(s7, DEFAULT_WAVO_TRACE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_wavo_trace "*) (list #f (lambda (s v) (set! (" S_wavo_trace ") v)) #f))");

  ss->transform_size_symbol = s7_define_variable(s7, "*" S_transform_size "*", s7_make_integer(s7, DEFAULT_TRANSFORM_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_transform_size "*) (list #f (lambda (s v) (set! (" S_transform_size ") v)) #f))");

  ss->fft_window_symbol = s7_define_variable(s7, "*" S_fft_window "*", s7_make_integer(s7, DEFAULT_FFT_WINDOW));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_window "*) (list #f (lambda (s v) (set! (" S_fft_window ") v)) #f))");

  ss->transform_graph_type_symbol = s7_define_variable(s7, "*" S_transform_graph_type "*", s7_make_integer(s7, DEFAULT_TRANSFORM_GRAPH_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_transform_graph_type "*) (list #f (lambda (s v) (set! (" S_transform_graph_type ") v)) #f))");

  ss->time_graph_type_symbol = s7_define_variable(s7, "*" S_time_graph_type "*", s7_make_integer(s7, DEFAULT_TIME_GRAPH_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_time_graph_type "*) (list #f (lambda (s v) (set! (" S_time_graph_type ") v)) #f))");

  ss->ask_before_overwrite_symbol = s7_define_variable(s7, "*" S_ask_before_overwrite "*", s7_make_boolean(s7, DEFAULT_ASK_BEFORE_OVERWRITE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_ask_before_overwrite "*) (list #f (lambda (s v) (set! (" S_ask_before_overwrite ") v)) #f))");

  ss->ask_about_unsaved_edits_symbol = s7_define_variable(s7, "*" S_ask_about_unsaved_edits "*", s7_make_boolean(s7, DEFAULT_ASK_ABOUT_UNSAVED_EDITS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_ask_about_unsaved_edits "*) (list #f (lambda (s v) (set! (" S_ask_about_unsaved_edits ") v)) #f))");

  ss->show_full_duration_symbol = s7_define_variable(s7, "*" S_show_full_duration "*", s7_make_boolean(s7, DEFAULT_SHOW_FULL_DURATION));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_full_duration "*) (list #f (lambda (s v) (set! (" S_show_full_duration ") v)) #f))");

  ss->show_full_range_symbol = s7_define_variable(s7, "*" S_show_full_range "*", s7_make_boolean(s7, DEFAULT_SHOW_FULL_RANGE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_full_range "*) (list #f (lambda (s v) (set! (" S_show_full_range ") v)) #f))");

  ss->remember_sound_state_symbol = s7_define_variable(s7, "*" S_remember_sound_state "*", s7_make_boolean(s7, DEFAULT_REMEMBER_SOUND_STATE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_remember_sound_state "*) (list #f (lambda (s v) (set! (" S_remember_sound_state ") v)) #f))");

  ss->save_as_dialog_src_symbol = s7_define_variable(s7, "*" S_save_as_dialog_src "*", s7_make_boolean(s7, DEFAULT_SAVE_AS_DIALOG_SRC));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_save_as_dialog_src "*) (list #f (lambda (s v) (set! (" S_save_as_dialog_src ") v)) #f))");

  ss->save_as_dialog_auto_comment_symbol = s7_define_variable(s7, "*" S_save_as_dialog_auto_comment "*", s7_make_boolean(s7, DEFAULT_SAVE_AS_DIALOG_AUTO_COMMENT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_save_as_dialog_auto_comment "*) (list #f (lambda (s v) (set! (" S_save_as_dialog_auto_comment ") v)) #f))");

  ss->with_toolbar_symbol = s7_define_variable(s7, "*" S_with_toolbar "*", s7_make_boolean(s7, DEFAULT_WITH_TOOLBAR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_toolbar "*) (list #f (lambda (s v) (set! (" S_with_toolbar ") v)) #f))");

  ss->with_tooltips_symbol = s7_define_variable(s7, "*" S_with_tooltips "*", s7_make_boolean(s7, DEFAULT_WITH_TOOLTIPS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_tooltips "*) (list #f (lambda (s v) (set! (" S_with_tooltips ") v)) #f))");

  ss->with_menu_icons_symbol = s7_define_variable(s7, "*" S_with_menu_icons "*", s7_make_boolean(s7, DEFAULT_WITH_MENU_ICONS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_menu_icons "*) (list #f (lambda (s v) (set! (" S_with_menu_icons ") v)) #f))");

  ss->fft_window_alpha_symbol = s7_define_variable(s7, "*" S_fft_window_alpha "*", s7_make_real(s7, DEFAULT_FFT_WINDOW_ALPHA));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_window_alpha "*) (list #f (lambda (s v) (set! (" S_fft_window_alpha ") v)) #f))");

  ss->fft_window_beta_symbol = s7_define_variable(s7, "*" S_fft_window_beta "*", s7_make_real(s7, DEFAULT_FFT_WINDOW_BETA));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_window_beta "*) (list #f (lambda (s v) (set! (" S_fft_window_beta ") v)) #f))");

  ss->grid_density_symbol = s7_define_variable(s7, "*" S_grid_density "*", s7_make_real(s7, DEFAULT_GRID_DENSITY));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_grid_density "*) (list #f (lambda (s v) (set! (" S_grid_density ") v)) #f))");

  ss->initial_beg_symbol = s7_define_variable(s7, "*" S_initial_beg "*", s7_make_real(s7, DEFAULT_INITIAL_BEG));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_initial_beg "*) (list #f (lambda (s v) (set! (" S_initial_beg ") v)) #f))");

  ss->initial_dur_symbol = s7_define_variable(s7, "*" S_initial_dur "*", s7_make_real(s7, DEFAULT_INITIAL_DUR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_initial_dur "*) (list #f (lambda (s v) (set! (" S_initial_dur ") v)) #f))");

  ss->color_scale_symbol = s7_define_variable(s7, "*" S_color_scale "*", s7_make_real(s7, DEFAULT_COLOR_SCALE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_color_scale "*) (list #f (lambda (s v) (set! (" S_color_scale ") v)) #f))");

  ss->color_cutoff_symbol = s7_define_variable(s7, "*" S_color_cutoff "*", s7_make_real(s7, DEFAULT_COLOR_CUTOFF));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_color_cutoff "*) (list #f (lambda (s v) (set! (" S_color_cutoff ") v)) #f))");

  ss->beats_per_minute_symbol = s7_define_variable(s7, "*" S_beats_per_minute "*", s7_make_real(s7, DEFAULT_BEATS_PER_MINUTE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_beats_per_minute "*) (list #f (lambda (s v) (set! (" S_beats_per_minute ") v)) #f))");

  ss->color_inverted_symbol = s7_define_variable(s7, "*" S_color_inverted "*", s7_make_boolean(s7, DEFAULT_COLOR_INVERTED));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_color_inverted "*) (list #f (lambda (s v) (set! (" S_color_inverted ") v)) #f))");

  ss->show_mix_waveforms_symbol = s7_define_variable(s7, "*" S_show_mix_waveforms "*", s7_make_boolean(s7, DEFAULT_SHOW_MIX_WAVEFORMS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_mix_waveforms "*) (list #f (lambda (s v) (set! (" S_show_mix_waveforms ") v)) #f))");

  ss->mix_waveform_height_symbol = s7_define_variable(s7, "*" S_mix_waveform_height "*", s7_make_integer(s7, DEFAULT_MIX_WAVEFORM_HEIGHT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mix_waveform_height "*) (list #f (lambda (s v) (set! (" S_mix_waveform_height ") v)) #f))");

  ss->beats_per_measure_symbol = s7_define_variable(s7, "*" S_beats_per_measure "*", s7_make_integer(s7, DEFAULT_BEATS_PER_MEASURE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_beats_per_measure "*) (list #f (lambda (s v) (set! (" S_beats_per_measure ") v)) #f))");

  ss->transform_normalization_symbol = s7_define_variable(s7, "*" S_transform_normalization "*", s7_make_integer(s7, DEFAULT_TRANSFORM_NORMALIZATION));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_transform_normalization "*) (list #f (lambda (s v) (set! (" S_transform_normalization ") v)) #f))");

  ss->sinc_width_symbol = s7_define_variable(s7, "*" S_sinc_width "*", s7_make_integer(s7, DEFAULT_SINC_WIDTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_sinc_width "*) (list #f (lambda (s v) (set! (" S_sinc_width ") v)) #f))");

  ss->x_axis_style_symbol = s7_define_variable(s7, "*" S_x_axis_style "*", s7_make_integer(s7, DEFAULT_X_AXIS_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_x_axis_style "*) (list #f (lambda (s v) (set! (" S_x_axis_style ") v)) #f))");

  ss->zoom_focus_style_symbol = s7_define_variable(s7, "*" S_zoom_focus_style "*", s7_make_integer(s7, DEFAULT_ZOOM_FOCUS_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_zoom_focus_style "*) (list #f (lambda (s v) (set! (" S_zoom_focus_style ") v)) #f))");

  ss->graph_style_symbol = s7_define_variable(s7, "*" S_graph_style "*", s7_make_integer(s7, DEFAULT_GRAPH_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_graph_style "*) (list #f (lambda (s v) (set! (" S_graph_style ") v)) #f))");

  ss->region_graph_style_symbol = s7_define_variable(s7, "*" S_region_graph_style "*", s7_make_integer(s7, ss->Region_Graph_Style));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_region_graph_style "*) (list #f (lambda (s v) (set! (" S_region_graph_style ") v)) #f))");

  ss->auto_resize_symbol = s7_define_variable(s7, "*" S_auto_resize "*", s7_make_boolean(s7, DEFAULT_AUTO_RESIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_auto_resize "*) (list #f (lambda (s v) (set! (" S_auto_resize ") v)) #f))");

  ss->auto_update_symbol = s7_define_variable(s7, "*" S_auto_update "*", s7_make_boolean(s7, DEFAULT_AUTO_UPDATE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_auto_update "*) (list #f (lambda (s v) (set! (" S_auto_update ") v)) #f))");

  ss->max_regions_symbol = s7_define_variable(s7, "*" S_max_regions "*", s7_make_integer(s7, DEFAULT_MAX_REGIONS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_max_regions "*) (list #f (lambda (s v) (set! (" S_max_regions ") v)) #f))");

  ss->max_transform_peaks_symbol = s7_define_variable(s7, "*" S_max_transform_peaks "*", s7_make_integer(s7, DEFAULT_MAX_TRANSFORM_PEAKS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_max_transform_peaks "*) (list #f (lambda (s v) (set! (" S_max_transform_peaks ") v)) #f))");

  ss->audio_output_device_symbol = s7_define_variable(s7, "*" S_audio_output_device "*", s7_make_integer(s7, DEFAULT_AUDIO_OUTPUT_DEVICE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_audio_output_device "*) (list #f (lambda (s v) (set! (" S_audio_output_device ") v)) #f))");

  ss->audio_input_device_symbol = s7_define_variable(s7, "*" S_audio_input_device "*", s7_make_integer(s7, DEFAULT_AUDIO_INPUT_DEVICE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_audio_input_device "*) (list #f (lambda (s v) (set! (" S_audio_input_device ") v)) #f))");

  ss->with_gl_symbol = s7_define_variable(s7, "*" S_with_gl "*", s7_make_boolean(s7, DEFAULT_WITH_GL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_gl "*) (list #f (lambda (s v) (set! (" S_with_gl ") v)) #f))");

  ss->with_relative_panes_symbol = s7_define_variable(s7, "*" S_with_relative_panes "*", s7_make_boolean(s7, DEFAULT_WITH_RELATIVE_PANES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_relative_panes "*) (list #f (lambda (s v) (set! (" S_with_relative_panes ") v)) #f))");

  ss->print_length_symbol = s7_define_variable(s7, "*" S_print_length "*", s7_make_integer(s7, DEFAULT_PRINT_LENGTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_print_length "*) (list #f (lambda (s v) (set! (" S_print_length ") v)) #f))");

  ss->dac_size_symbol = s7_define_variable(s7, "*" S_dac_size "*", s7_make_integer(s7, DEFAULT_DAC_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_dac_size "*) (list #f (lambda (s v) (set! (" S_dac_size ") v)) #f))");

  ss->view_files_sort_symbol = s7_define_variable(s7, "*" S_view_files_sort "*", s7_make_integer(s7, DEFAULT_VIEW_FILES_SORT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_view_files_sort "*) (list #f (lambda (s v) (set! (" S_view_files_sort ") v)) #f))");

  ss->dac_combines_channels_symbol = s7_define_variable(s7, "*" S_dac_combines_channels "*", s7_make_boolean(s7, DEFAULT_DAC_COMBINES_CHANNELS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_dac_combines_channels "*) (list #f (lambda (s v) (set! (" S_dac_combines_channels ") v)) #f))");

  ss->show_selection_transform_symbol = s7_define_variable(s7, "*" S_show_selection_transform "*", s7_make_boolean(s7, DEFAULT_SHOW_SELECTION_TRANSFORM));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_selection_transform "*) (list #f (lambda (s v) (set! (" S_show_selection_transform ") v)) #f))");

  ss->with_mix_tags_symbol = s7_define_variable(s7, "*" S_with_mix_tags "*", s7_make_boolean(s7, DEFAULT_WITH_MIX_TAGS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_mix_tags "*) (list #f (lambda (s v) (set! (" S_with_mix_tags ") v)) #f))");

  ss->selection_creates_region_symbol = s7_define_variable(s7, "*" S_selection_creates_region "*", s7_make_boolean(s7, DEFAULT_SELECTION_CREATES_REGION));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_selection_creates_region "*) (list #f (lambda (s v) (set! (" S_selection_creates_region ") v)) #f))");

  ss->save_state_file_symbol = s7_define_variable(s7, "*" S_save_state_file "*", s7_make_string(s7, DEFAULT_SAVE_STATE_FILE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_save_state_file "*) (list #f (lambda (s v) (set! (" S_save_state_file ") v)) #f))");

  ss->listener_prompt_symbol = s7_define_variable(s7, "*" S_listener_prompt "*", s7_make_string(s7, DEFAULT_LISTENER_PROMPT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_listener_prompt "*) (list #f (lambda (s v) (set! (" S_listener_prompt ") v)) #f))");

  ss->enved_base_symbol = s7_define_variable(s7, "*" S_enved_base "*", s7_make_real(s7, DEFAULT_ENVED_BASE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_base "*) (list #f (lambda (s v) (set! (" S_enved_base ") v)) #f))");

  ss->enved_power_symbol = s7_define_variable(s7, "*" S_enved_power "*", s7_make_real(s7, DEFAULT_ENVED_POWER));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_power "*) (list #f (lambda (s v) (set! (" S_enved_power ") v)) #f))");

  ss->auto_update_interval_symbol = s7_define_variable(s7, "*" S_auto_update_interval "*", s7_make_real(s7, DEFAULT_AUTO_UPDATE_INTERVAL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_auto_update_interval "*) (list #f (lambda (s v) (set! (" S_auto_update_interval ") v)) #f))");

  ss->enved_wave_p_symbol = s7_define_variable(s7, "*" S_enved_wave_p "*", s7_make_boolean(s7, DEFAULT_ENVED_WAVE_P));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_wave_p "*) (list #f (lambda (s v) (set! (" S_enved_wave_p ") v)) #f))");

  ss->graphs_horizontal_symbol = s7_define_variable(s7, "*" S_graphs_horizontal "*", s7_make_boolean(s7, DEFAULT_GRAPHS_HORIZONTAL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_graphs_horizontal "*) (list #f (lambda (s v) (set! (" S_graphs_horizontal ") v)) #f))");

  ss->with_background_processes_symbol = s7_define_variable(s7, "*" S_with_background_processes "*", s7_make_boolean(s7, DEFAULT_WITH_BACKGROUND_PROCESSES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_background_processes "*) (list #f (lambda (s v) (set! (" S_with_background_processes ") v)) #f))");

  ss->with_file_monitor_symbol = s7_define_variable(s7, "*" S_with_file_monitor "*", s7_make_boolean(s7, DEFAULT_WITH_FILE_MONITOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_file_monitor "*) (list #f (lambda (s v) (set! (" S_with_file_monitor ") v)) #f))");

  ss->enved_style_symbol = s7_define_variable(s7, "*" S_enved_style "*", s7_make_integer(s7, DEFAULT_ENVED_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_style "*) (list #f (lambda (s v) (set! (" S_enved_style ") v)) #f))");

  ss->graph_cursor_symbol = s7_define_variable(s7, "*" S_graph_cursor "*", s7_make_integer(s7, DEFAULT_GRAPH_CURSOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_graph_cursor "*) (list #f (lambda (s v) (set! (" S_graph_cursor ") v)) #f))");

  ss->mix_tag_width_symbol = s7_define_variable(s7, "*" S_mix_tag_width "*", s7_make_integer(s7, DEFAULT_MIX_TAG_WIDTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mix_tag_width "*) (list #f (lambda (s v) (set! (" S_mix_tag_width ") v)) #f))");

  ss->mix_tag_height_symbol = s7_define_variable(s7, "*" S_mix_tag_height "*", s7_make_integer(s7, DEFAULT_MIX_TAG_HEIGHT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mix_tag_height "*) (list #f (lambda (s v) (set! (" S_mix_tag_height ") v)) #f))");

  ss->mark_tag_height_symbol = s7_define_variable(s7, "*" S_mark_tag_height "*", s7_make_integer(s7, DEFAULT_MARK_TAG_HEIGHT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mark_tag_height "*) (list #f (lambda (s v) (set! (" S_mark_tag_height ") v)) #f))");

  ss->mark_tag_width_symbol = s7_define_variable(s7, "*" S_mark_tag_width "*", s7_make_integer(s7, DEFAULT_MARK_TAG_WIDTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mark_tag_width "*) (list #f (lambda (s v) (set! (" S_mark_tag_width ") v)) #f))");

  ss->enved_target_symbol = s7_define_variable(s7, "*" S_enved_target "*", s7_make_integer(s7, DEFAULT_ENVED_TARGET));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_target "*) (list #f (lambda (s v) (set! (" S_enved_target ") v)) #f))");

  ss->clipping_symbol = s7_define_variable(s7, "*" S_clipping "*", s7_make_boolean(s7, DEFAULT_CLIPPING));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_clipping "*) (list #f (lambda (s v) (set! (" S_clipping ") v)) #f))");

  ss->show_indices_symbol = s7_define_variable(s7, "*" S_show_indices "*", s7_make_boolean(s7, DEFAULT_SHOW_INDICES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_indices "*) (list #f (lambda (s v) (set! (" S_show_indices ") v)) #f))");

  ss->just_sounds_symbol = s7_define_variable(s7, "*" S_just_sounds "*", s7_make_boolean(s7, DEFAULT_JUST_SOUNDS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_just_sounds "*) (list #f (lambda (s v) (set! (" S_just_sounds ") v)) #f))");

  ss->cursor_size_symbol = s7_define_variable(s7, "*" S_cursor_size "*", s7_make_integer(s7, DEFAULT_CURSOR_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_size "*) (list #f (lambda (s v) (set! (" S_cursor_size ") v)) #f))");

  ss->cursor_style_symbol = s7_define_variable(s7, "*" S_cursor_style "*", s7_make_integer(s7, DEFAULT_CURSOR_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_style "*) (list #f (lambda (s v) (set! (" S_cursor_style ") v)) #f))");

  ss->tracking_cursor_style_symbol = s7_define_variable(s7, "*" S_tracking_cursor_style "*", s7_make_integer(s7, DEFAULT_TRACKING_CURSOR_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_tracking_cursor_style "*) (list #f (lambda (s v) (set! (" S_tracking_cursor_style ") v)) #f))");

  ss->filter_control_in_db_symbol = s7_define_variable(s7, "*" S_filter_control_in_dB "*", s7_make_boolean(s7, DEFAULT_FILTER_CONTROL_IN_DB));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_filter_control_in_dB "*) (list #f (lambda (s v) (set! (" S_filter_control_in_dB ") v)) #f))");

  ss->filter_control_in_hz_symbol = s7_define_variable(s7, "*" S_filter_control_in_hz "*", s7_make_boolean(s7, DEFAULT_FILTER_CONTROL_IN_HZ));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_filter_control_in_hz "*) (list #f (lambda (s v) (set! (" S_filter_control_in_hz ") v)) #f))");

  ss->show_sonogram_cursor_symbol = s7_define_variable(s7, "*" S_show_sonogram_cursor "*", s7_make_integer(s7, DEFAULT_SHOW_SONOGRAM_CURSOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_sonogram_cursor "*) (list #f (lambda (s v) (set! (" S_show_sonogram_cursor ") v)) #f))");

  ss->speed_control_tones_symbol = s7_define_variable(s7, "*" S_speed_control_tones "*", s7_make_integer(s7, DEFAULT_SPEED_CONTROL_TONES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_speed_control_tones "*) (list #f (lambda (s v) (set! (" S_speed_control_tones ") v)) #f))");

  ss->speed_control_style_symbol = s7_define_variable(s7, "*" S_speed_control_style "*", s7_make_integer(s7, DEFAULT_SPEED_CONTROL_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_speed_control_style "*) (list #f (lambda (s v) (set! (" S_speed_control_style ") v)) #f))");

  ss->expand_control_length_symbol = s7_define_variable(s7, "*" S_expand_control_length "*", s7_make_real(s7, DEFAULT_EXPAND_CONTROL_LENGTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_expand_control_length "*) (list #f (lambda (s v) (set! (" S_expand_control_length ") v)) #f))");

  ss->expand_control_ramp_symbol = s7_define_variable(s7, "*" S_expand_control_ramp "*", s7_make_real(s7, DEFAULT_EXPAND_CONTROL_RAMP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_expand_control_ramp "*) (list #f (lambda (s v) (set! (" S_expand_control_ramp ") v)) #f))");

  ss->expand_control_hop_symbol = s7_define_variable(s7, "*" S_expand_control_hop "*", s7_make_real(s7, DEFAULT_EXPAND_CONTROL_HOP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_expand_control_hop "*) (list #f (lambda (s v) (set! (" S_expand_control_hop ") v)) #f))");

  ss->expand_control_jitter_symbol = s7_define_variable(s7, "*" S_expand_control_jitter "*", s7_make_real(s7, DEFAULT_EXPAND_CONTROL_JITTER));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_expand_control_jitter "*) (list #f (lambda (s v) (set! (" S_expand_control_jitter ") v)) #f))");

  ss->contrast_control_amp_symbol = s7_define_variable(s7, "*" S_contrast_control_amp "*", s7_make_real(s7, DEFAULT_CONTRAST_CONTROL_AMP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_contrast_control_amp "*) (list #f (lambda (s v) (set! (" S_contrast_control_amp ") v)) #f))");

  ss->reverb_control_feedback_symbol = s7_define_variable(s7, "*" S_reverb_control_feedback "*", s7_make_real(s7, DEFAULT_REVERB_CONTROL_FEEDBACK));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_reverb_control_feedback "*) (list #f (lambda (s v) (set! (" S_reverb_control_feedback ") v)) #f))");

  ss->reverb_control_lowpass_symbol = s7_define_variable(s7, "*" S_reverb_control_lowpass "*", s7_make_real(s7, DEFAULT_REVERB_CONTROL_LOWPASS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_reverb_control_lowpass "*) (list #f (lambda (s v) (set! (" S_reverb_control_lowpass ") v)) #f))");

  ss->reverb_control_decay_symbol = s7_define_variable(s7, "*" S_reverb_control_decay "*", s7_make_real(s7, DEFAULT_REVERB_CONTROL_DECAY));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_reverb_control_decay "*) (list #f (lambda (s v) (set! (" S_reverb_control_decay ") v)) #f))");

  ss->cursor_update_interval_symbol = s7_define_variable(s7, "*" S_cursor_update_interval "*", s7_make_real(s7, DEFAULT_CURSOR_UPDATE_INTERVAL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_update_interval "*) (list #f (lambda (s v) (set! (" S_cursor_update_interval ") v)) #f))");

  ss->filter_control_order_symbol = s7_define_variable(s7, "*" S_filter_control_order "*", s7_make_integer(s7, DEFAULT_FILTER_CONTROL_ORDER));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_filter_control_order "*) (list #f (lambda (s v) (set! (" S_filter_control_order ") v)) #f))");

  ss->cursor_location_offset_symbol = s7_define_variable(s7, "*" S_cursor_location_offset "*", s7_make_integer(s7, DEFAULT_CURSOR_LOCATION_OFFSET));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_location_offset "*) (list #f (lambda (s v) (set! (" S_cursor_location_offset ") v)) #f))");

  ss->play_arrow_size_symbol = s7_define_variable(s7, "*" S_play_arrow_size "*", s7_make_integer(s7, DEFAULT_PLAY_ARROW_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_play_arrow_size "*) (list #f (lambda (s v) (set! (" S_play_arrow_size ") v)) #f))");

  ss->min_db_symbol = s7_define_variable(s7, "*" S_min_dB "*", s7_make_real(s7, DEFAULT_MIN_DB));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_min_dB "*) (list #f (lambda (s v) (set! (" S_min_dB ") v)) #f))");

  ss->show_controls_symbol = s7_define_variable(s7, "*" S_show_controls "*", s7_make_boolean(s7, DEFAULT_SHOW_CONTROLS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_controls "*) (list #f (lambda (s v) (set! (" S_show_controls ") v)) #f))");

  ss->with_tracking_cursor_symbol = s7_define_variable(s7, "*" S_with_tracking_cursor "*", s7_make_integer(s7, (int)DEFAULT_WITH_TRACKING_CURSOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_tracking_cursor "*) (list #f (lambda (s v) (set! (" S_with_tracking_cursor ") v)) #f))");

  ss->html_dir_symbol = s7_define_variable(s7, "*" S_html_dir "*", s7_make_string(s7, DEFAULT_HTML_DIR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_html_dir "*) (list #f (lambda (s v) (set! (" S_html_dir ") v)) #f))");

  ss->html_program_symbol = s7_define_variable(s7, "*" S_html_program "*", s7_make_string(s7, DEFAULT_HTML_PROGRAM));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_html_program "*) (list #f (lambda (s v) (set! (" S_html_program ") v)) #f))");

  ss->open_file_dialog_directory_symbol = s7_define_variable(s7, "*" S_open_file_dialog_directory "*", s7_make_string(s7, ss->Open_File_Dialog_Directory));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_open_file_dialog_directory "*) (list #f (lambda (s v) (set! (" S_open_file_dialog_directory ") v)) #f))");

  ss->data_color_symbol           = s7_define_variable(s7, "*" S_data_color "*",           s7_f(s7));
  ss->selected_data_color_symbol  = s7_define_variable(s7, "*" S_selected_data_color "*",  s7_f(s7));
  ss->mark_color_symbol           = s7_define_variable(s7, "*" S_mark_color "*",           s7_f(s7));
  ss->graph_color_symbol          = s7_define_variable(s7, "*" S_graph_color "*",          s7_f(s7));
  ss->selected_graph_color_symbol = s7_define_variable(s7, "*" S_selected_graph_color "*", s7_f(s7));
  ss->listener_color_symbol       = s7_define_variable(s7, "*" S_listener_color "*",       s7_f(s7));
  ss->listener_text_color_symbol  = s7_define_variable(s7, "*" S_listener_text_color "*",  s7_f(s7));
  ss->basic_color_symbol          = s7_define_variable(s7, "*" S_basic_color "*",          s7_f(s7));
  ss->selection_color_symbol      = s7_define_variable(s7, "*" S_selection_color "*",      s7_f(s7));
  ss->zoom_color_symbol           = s7_define_variable(s7, "*" S_zoom_color "*",           s7_f(s7));
  ss->position_color_symbol       = s7_define_variable(s7, "*" S_position_color "*",       s7_f(s7));
  ss->highlight_color_symbol      = s7_define_variable(s7, "*" S_highlight_color "*",      s7_f(s7));
  ss->enved_waveform_color_symbol = s7_define_variable(s7, "*" S_enved_waveform_color "*", s7_f(s7));
  ss->cursor_color_symbol         = s7_define_variable(s7, "*" S_cursor_color "*",         s7_f(s7));
  ss->text_focus_color_symbol     = s7_define_variable(s7, "*" S_text_focus_color "*",     s7_f(s7));
  ss->filter_control_waveform_color_symbol = s7_define_variable(s7, "*" S_filter_control_waveform_color "*", s7_f(s7));
  ss->mix_color_symbol            = s7_define_variable(s7, "*" S_mix_color "*",            s7_f(s7));
  ss->sash_color_symbol           = s7_define_variable(s7, "*" S_sash_color "*",           s7_f(s7));
  ss->axis_color_symbol           = s7_define_variable(s7, "*" S_axis_color "*",           s7_f(s7));

#if (!USE_NO_GUI)
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_data_color "*) (list #f (lambda (s v) (set! (" S_data_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_selected_data_color "*) (list #f (lambda (s v) (set! (" S_selected_data_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mark_color "*) (list #f (lambda (s v) (set! (" S_mark_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_graph_color "*) (list #f (lambda (s v) (set! (" S_graph_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_selected_graph_color "*) (list #f (lambda (s v) (set! (" S_selected_graph_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_listener_color "*) (list #f (lambda (s v) (set! (" S_listener_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_listener_text_color "*) (list #f (lambda (s v) (set! (" S_listener_text_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_basic_color "*) (list #f (lambda (s v) (set! (" S_basic_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_selection_color "*) (list #f (lambda (s v) (set! (" S_selection_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_zoom_color "*) (list #f (lambda (s v) (set! (" S_zoom_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_position_color "*) (list #f (lambda (s v) (set! (" S_position_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_highlight_color "*) (list #f (lambda (s v) (set! (" S_highlight_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_waveform_color "*) (list #f (lambda (s v) (set! (" S_enved_waveform_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_color "*) (list #f (lambda (s v) (set! (" S_cursor_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_text_focus_color "*) (list #f (lambda (s v) (set! (" S_text_focus_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_filter_control_waveform_color "*) (list #f (lambda (s v) (set! (" S_filter_control_waveform_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mix_color "*) (list #f (lambda (s v) (set! (" S_mix_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_sash_color "*) (list #f (lambda (s v) (set! (" S_sash_color ") v)) #f))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_axis_color "*) (list #f (lambda (s v) (set! (" S_axis_color ") v)) #f))");
#endif
#endif
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


int main(int argc, char **argv)
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

  snd_doit(argc, argv);
  return(0);
}


void g_init_base(void)
{
  #define H_mus_error_hook S_mus_error_hook " (type message):  called upon mus_error. \
If it returns " PROC_TRUE ", Snd ignores the error (it assumes you've handled it via the hook)."

  ss->mus_error_hook = XEN_DEFINE_HOOK(S_mus_error_hook, "(make-hook 'type 'message)", 2, H_mus_error_hook);
}
