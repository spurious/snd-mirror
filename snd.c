/* Sound display/edit/etc
 *
 * originally intended as a re-implementation of my much-missed dpysnd -- the Foonly/SAIL/E/Mus10/Grnlib sound editor from ca 1983.
 */

#include "snd.h"

snd_state *ss = NULL;

static bool ignore_mus_error(int type, char *msg)
{
  Xen result = Xen_false;

  if (Xen_hook_has_list(ss->mus_error_hook))
    result = run_or_hook(ss->mus_error_hook, 
			 Xen_list_2(C_int_to_Xen_integer(type), 
				    C_string_to_Xen_string(msg)),
			 S_mus_error_hook);
  return(Xen_is_true(result));
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
	Xen_error(Xen_make_error_type("mus-error"),
		  Xen_list_1(C_string_to_Xen_string((char *)mus_error_type_to_string(type))));
      else Xen_error(Xen_make_error_type("mus-error"),
		     Xen_list_1(C_string_to_Xen_string(msg)));
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
	  Xen_add_to_load_path(dirnames[i]);
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
      if ((cursor_style(ss) == CURSOR_PROC) && (Xen_is_procedure(ss->cursor_proc)))
	snd_unprotect_at(ss->cursor_proc_loc);
      if ((zoom_focus_style(ss) == ZOOM_FOCUS_PROC) && (Xen_is_procedure(ss->zoom_focus_proc)))
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
  ss->zoom_focus_proc =             Xen_undefined;
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
  ss->cursor_proc =                 Xen_undefined;
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
  ss->Enved_With_Wave =             DEFAULT_ENVED_WITH_WAVE;
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
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_transform_peaks "*) (lambda (s v) (set! (" S_show_transform_peaks ") v)))");
  s7_symbol_set_documentation(s7, ss->show_transform_peaks_symbol, "*" S_show_transform_peaks "* determines whether fft displays include a peak list");

  ss->show_y_zero_symbol = s7_define_variable(s7, "*" S_show_y_zero "*", s7_make_boolean(s7, DEFAULT_SHOW_Y_ZERO));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_y_zero "*) (lambda (s v) (set! (" S_show_y_zero ") v)))");
  s7_symbol_set_documentation(s7, ss->show_y_zero_symbol, "*show-y-zero*: #t if Snd should include a line at y = 0.0");

  ss->show_marks_symbol = s7_define_variable(s7, "*" S_show_marks "*", s7_make_boolean(s7, DEFAULT_SHOW_MARKS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_marks "*) (lambda (s v) (set! (" S_show_marks ") v)))");
  s7_symbol_set_documentation(s7, ss->show_marks_symbol, "*show-marks*: #t if Snd should show marks");

  ss->show_grid_symbol = s7_define_variable(s7, "*" S_show_grid "*", s7_make_boolean(s7, DEFAULT_SHOW_GRID));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_grid "*) (lambda (s v) (set! (" S_show_grid ") v)))");
  s7_symbol_set_documentation(s7, ss->show_grid_symbol, "*show-grid*: #t if Snd should display a background grid in the graphs");

  ss->fft_log_frequency_symbol = s7_define_variable(s7, "*" S_fft_log_frequency "*", s7_make_boolean(s7, DEFAULT_FFT_LOG_FREQUENCY));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_log_frequency "*) (lambda (s v) (set! (" S_fft_log_frequency ") v)))");
  s7_symbol_set_documentation(s7, ss->fft_log_frequency_symbol, "*fft-log-frequency*: #t if fft displays use log on the frequency axis");

  ss->fft_log_magnitude_symbol = s7_define_variable(s7, "*" S_fft_log_magnitude "*", s7_make_boolean(s7, DEFAULT_FFT_LOG_MAGNITUDE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_log_magnitude "*) (lambda (s v) (set! (" S_fft_log_magnitude ") v)))");
  s7_symbol_set_documentation(s7, ss->fft_log_magnitude_symbol, "*fft-log-magnitude*: #t if fft displays use dB");

  ss->fft_with_phases_symbol = s7_define_variable(s7, "*" S_fft_with_phases "*", s7_make_boolean(s7, DEFAULT_FFT_WITH_PHASES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_with_phases "*) (lambda (s v) (set! (" S_fft_with_phases ") v)))");
  s7_symbol_set_documentation(s7, ss->fft_with_phases_symbol, "*fft-with-phases*: #t if fft displays include phase info");

  ss->channel_style_symbol = s7_define_variable(s7, "*" S_channel_style "*", s7_make_integer(s7, DEFAULT_CHANNEL_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_channel_style "*) (lambda (s v) (set! (" S_channel_style ") v)))");
  s7_symbol_set_documentation(s7, ss->channel_style_symbol, "*channel-style*: how multichannel sounds lay out the channels: channels-combined, channels-separate or channels-superimposed.");

  ss->sync_style_symbol = s7_define_variable(s7, "*" S_sync_style "*", s7_make_integer(s7, DEFAULT_SYNC_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_sync_style "*) (lambda (s v) (set! (" S_sync_style ") v)))");
  s7_symbol_set_documentation(s7, ss->sync_style_symbol, "*sync-style*: determines how channels are grouped when a sound is opened.");

  ss->show_axes_symbol = s7_define_variable(s7, "*" S_show_axes "*", s7_make_integer(s7, DEFAULT_SHOW_AXES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_axes "*) (lambda (s v) (set! (" S_show_axes ") v)))");
  s7_symbol_set_documentation(s7, ss->show_axes_symbol, "*show-axes*: If show-all-axes, display x and y axes; if show-x-axis, just one axis (the x axis) is displayed. The other choices are show-no-axes, show-all-axes-unlabelled, show-x-axis-unlabelled, and show-bare-x-axis.");

  ss->eps_file_symbol = s7_define_variable(s7, "*" S_eps_file "*", s7_make_string(s7, DEFAULT_EPS_FILE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_eps_file "*) (lambda (s v) (set! (" S_eps_file ") v)))");
  s7_symbol_set_documentation(s7, ss->eps_file_symbol, "*eps-file*: File:Print and graph->ps file name (snd.eps)");

  ss->temp_dir_symbol = s7_define_variable(s7, "*" S_temp_dir "*", s7_make_string(s7, ss->Temp_Dir));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_temp_dir "*) (lambda (s v) (set! (" S_temp_dir ") v)))");
  s7_symbol_set_documentation(s7, ss->temp_dir_symbol, "*temp-dir*: name of directory for temp files (or #f=null)"); 

  ss->save_dir_symbol = s7_define_variable(s7, "*" S_save_dir "*", s7_make_string(s7, ss->Save_Dir));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_save_dir "*) (lambda (s v) (set! (" S_save_dir ") v)))");
  s7_symbol_set_documentation(s7, ss->save_dir_symbol, "*save-dir*: name of directory for saved state data (or #f=null)");

  ss->ladspa_dir_symbol = s7_define_variable(s7, "*" S_ladspa_dir "*", s7_make_string(s7, DEFAULT_LADSPA_DIR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_ladspa_dir "*) (lambda (s v) (set! (" S_ladspa_dir ") v)))");
  s7_symbol_set_documentation(s7, ss->ladspa_dir_symbol, "*ladspa-dir*: name of directory for ladspa plugin libraries");

  ss->peak_env_dir_symbol = s7_define_variable(s7, "*" S_peak_env_dir "*", s7_make_string(s7, DEFAULT_PEAK_ENV_DIR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_peak_env_dir "*) (lambda (s v) (set! (" S_peak_env_dir ") v)))");
  s7_symbol_set_documentation(s7, ss->peak_env_dir_symbol, "*peak-env-dir*: name of directory for peak env files (or #f=null)");

#if USE_MOTIF
  #define DEFAULT_LISTENER_FONT "9x15"
#endif
#if (!USE_NO_GUI)
  ss->listener_font_symbol = s7_define_variable(s7, "*" S_listener_font "*", s7_make_string(s7, DEFAULT_LISTENER_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_listener_font "*) (lambda (s v) (set! (" S_listener_font ") v)))");
#else
  ss->listener_font_symbol = s7_define_variable(s7, "*" S_listener_font "*", s7_make_string(s7, ""));
#endif
  s7_symbol_set_documentation(s7, ss->listener_font_symbol, "*listener-font*: font used by the lisp listener");

  ss->axis_label_font_symbol = s7_define_variable(s7, "*" S_axis_label_font "*", s7_make_string(s7, DEFAULT_AXIS_LABEL_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_axis_label_font "*) (lambda (s v) (set! (" S_axis_label_font ") v)))");
  s7_symbol_set_documentation(s7, ss->axis_label_font_symbol, "*axis-label-font*: font used for axis labels");

  ss->axis_numbers_font_symbol = s7_define_variable(s7, "*" S_axis_numbers_font "*", s7_make_string(s7, DEFAULT_AXIS_NUMBERS_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_axis_numbers_font "*) (lambda (s v) (set! (" S_axis_numbers_font ") v)))");
  s7_symbol_set_documentation(s7, ss->axis_numbers_font_symbol, "*axis-numbers-font*: font used for axis numbers");

  ss->tiny_font_symbol = s7_define_variable(s7, "*" S_tiny_font "*", s7_make_string(s7, DEFAULT_TINY_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_tiny_font "*) (lambda (s v) (set! (" S_tiny_font ") v)))");
  s7_symbol_set_documentation(s7, ss->tiny_font_symbol, "*tiny-font*: font use for some info in the graphs");

  ss->peaks_font_symbol = s7_define_variable(s7, "*" S_peaks_font "*", s7_make_string(s7, DEFAULT_PEAKS_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_peaks_font "*) (lambda (s v) (set! (" S_peaks_font ") v)))");
  s7_symbol_set_documentation(s7, ss->peaks_font_symbol, "*peaks-font*: normal font used by fft peak display");

  ss->bold_peaks_font_symbol = s7_define_variable(s7, "*" S_bold_peaks_font "*", s7_make_string(s7, DEFAULT_BOLD_PEAKS_FONT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_bold_peaks_font "*) (lambda (s v) (set! (" S_bold_peaks_font ") v)))");
  s7_symbol_set_documentation(s7, ss->bold_peaks_font_symbol, "*bold-peaks-font*: bold font used by fft peak display");

  ss->with_verbose_cursor_symbol = s7_define_variable(s7, "*" S_with_verbose_cursor "*", s7_make_boolean(s7, DEFAULT_WITH_VERBOSE_CURSOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_verbose_cursor "*) (lambda (s v) (set! (" S_with_verbose_cursor ") v)))");
  s7_symbol_set_documentation(s7, ss->with_verbose_cursor_symbol, "*with-verbose-cursor*: #t if the cursor's position and so on is displayed in the status area");

  ss->with_inset_graph_symbol = s7_define_variable(s7, "*" S_with_inset_graph "*", s7_make_boolean(s7, DEFAULT_WITH_INSET_GRAPH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_inset_graph "*) (lambda (s v) (set! (" S_with_inset_graph ") v)))");
  s7_symbol_set_documentation(s7, ss->with_inset_graph_symbol, "*with-inset-graph*: if #t, display the inset graph in the time domain section.");

  ss->with_pointer_focus_symbol = s7_define_variable(s7, "*" S_with_pointer_focus "*", s7_make_boolean(s7, DEFAULT_WITH_POINTER_FOCUS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_pointer_focus "*) (lambda (s v) (set! (" S_with_pointer_focus ") v)))");
  s7_symbol_set_documentation(s7, ss->with_pointer_focus_symbol, "*with-pointer-focus*: if #t, activate the text or graph widget beneath the mouse.");

  ss->with_smpte_label_symbol = s7_define_variable(s7, "*" S_with_smpte_label "*", s7_make_boolean(s7, DEFAULT_WITH_SMPTE_LABEL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_smpte_label "*) (lambda (s v) (set! (" S_with_smpte_label ") v)))");
  s7_symbol_set_documentation(s7, ss->with_smpte_label_symbol, "*with-smpte-label*: if #t, display the SMPTE data in the time domain section.");

  ss->with_interrupts_symbol = s7_define_variable(s7, "*" S_with_interrupts "*", s7_make_boolean(s7, DEFAULT_WITH_INTERRUPTS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_interrupts "*) (lambda (s v) (set! (" S_with_interrupts ") v)))");
  s7_symbol_set_documentation(s7, ss->with_interrupts_symbol, "*with-interrupts*: if #t, check for GUI events during computations.");

  ss->enved_filter_order_symbol = s7_define_variable(s7, "*" S_enved_filter_order "*", s7_make_integer(s7, DEFAULT_ENVED_FILTER_ORDER));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_filter_order "*) (lambda (s v) (set! (" S_enved_filter_order ") v)))");
  s7_symbol_set_documentation(s7, ss->enved_filter_order_symbol, "*enved-filter-order*: envelope editor's FIR filter order (40)");

  ss->eps_left_margin_symbol = s7_define_variable(s7, "*" S_eps_left_margin "*", s7_make_real(s7, DEFAULT_EPS_LEFT_MARGIN));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_eps_left_margin "*) (lambda (s v) (set! (" S_eps_left_margin ") v)))");
  s7_symbol_set_documentation(s7, ss->eps_left_margin_symbol, "*eps-left-margin*: File:Print and graph->ps left margin");

  ss->eps_bottom_margin_symbol = s7_define_variable(s7, "*" S_eps_bottom_margin "*", s7_make_real(s7, DEFAULT_EPS_BOTTOM_MARGIN));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_eps_bottom_margin "*) (lambda (s v) (set! (" S_eps_bottom_margin ") v)))");
  s7_symbol_set_documentation(s7, ss->eps_bottom_margin_symbol, "*eps-bottom-margin*: File:Print and graph->ps bottom margin");

  ss->eps_size_symbol = s7_define_variable(s7, "*" S_eps_size "*", s7_make_real(s7, DEFAULT_EPS_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_eps_size "*) (lambda (s v) (set! (" S_eps_size ") v)))");
  s7_symbol_set_documentation(s7, ss->eps_size_symbol, "*eps-size*: File:Print and graph->ps output size scaler (1.0)");

  ss->log_freq_start_symbol = s7_define_variable(s7, "*" S_log_freq_start "*", s7_make_real(s7, DEFAULT_LOG_FREQ_START));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_log_freq_start "*) (lambda (s v) (set! (" S_log_freq_start ") v)))");
  s7_symbol_set_documentation(s7, ss->log_freq_start_symbol, "*log-freq-start*: log freq base (25.0)");

  ss->spectro_x_scale_symbol = s7_define_variable(s7, "*" S_spectro_x_scale "*", s7_make_real(s7, DEFAULT_SPECTRO_X_SCALE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_x_scale "*) (lambda (s v) (set! (" S_spectro_x_scale ") v)))");
  s7_symbol_set_documentation(s7, ss->spectro_x_scale_symbol, "*spectro-x-scale*: scaler (stretch) along the spectrogram x axis (1.0)");

  ss->spectro_y_scale_symbol = s7_define_variable(s7, "*" S_spectro_y_scale "*", s7_make_real(s7, DEFAULT_SPECTRO_Y_SCALE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_y_scale "*) (lambda (s v) (set! (" S_spectro_y_scale ") v)))");
  s7_symbol_set_documentation(s7, ss->spectro_y_scale_symbol, "*spectro-y-scale*: scaler (stretch) along the spectrogram y axis (1.0)");

  ss->spectro_z_scale_symbol = s7_define_variable(s7, "*" S_spectro_z_scale "*", s7_make_real(s7, DEFAULT_SPECTRO_Z_SCALE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_z_scale "*) (lambda (s v) (set! (" S_spectro_z_scale ") v)))");
  s7_symbol_set_documentation(s7, ss->spectro_z_scale_symbol, "*spectro-z-scale*: scaler (stretch) along the spectrogram z axis (0.1)");

  ss->spectro_z_angle_symbol = s7_define_variable(s7, "*" S_spectro_z_angle "*", s7_make_real(s7, DEFAULT_SPECTRO_Z_ANGLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_z_angle "*) (lambda (s v) (set! (" S_spectro_z_angle ") v)))");
  s7_symbol_set_documentation(s7, ss->spectro_z_angle_symbol, "*spectro-z-angle*: spectrogram z-axis viewing angle (-2.0)");

  ss->spectro_x_angle_symbol = s7_define_variable(s7, "*" S_spectro_x_angle "*", s7_make_real(s7, DEFAULT_SPECTRO_X_ANGLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_x_angle "*) (lambda (s v) (set! (" S_spectro_x_angle ") v)))");
  s7_symbol_set_documentation(s7, ss->spectro_x_angle_symbol, "*spectro-x-angle*: spectrogram x-axis viewing angle (90.0)");

  ss->spectro_y_angle_symbol = s7_define_variable(s7, "*" S_spectro_y_angle "*", s7_make_real(s7, DEFAULT_SPECTRO_Y_ANGLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_y_angle "*) (lambda (s v) (set! (" S_spectro_y_angle ") v)))");
  s7_symbol_set_documentation(s7, ss->spectro_y_angle_symbol, "*spectro-y-angle*: spectrogram y-axis viewing angle (0.0)");

  ss->spectrum_end_symbol = s7_define_variable(s7, "*" S_spectrum_end "*", s7_make_real(s7, DEFAULT_SPECTRUM_END));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectrum_end "*) (lambda (s v) (set! (" S_spectrum_end ") v)))");
  s7_symbol_set_documentation(s7, ss->spectrum_end_symbol, "*spectrum-end*: max frequency shown in spectra (1.0 = srate/2)");

  ss->spectrum_start_symbol = s7_define_variable(s7, "*" S_spectrum_start "*", s7_make_real(s7, DEFAULT_SPECTRUM_START));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectrum_start "*) (lambda (s v) (set! (" S_spectrum_start ") v)))");
  s7_symbol_set_documentation(s7, ss->spectrum_start_symbol, "*spectrum-start*: lower bound of frequency in spectral displays (0.0)");

  ss->default_output_header_type_symbol = s7_define_variable(s7, "*" S_default_output_header_type "*", s7_make_integer(s7, DEFAULT_OUTPUT_HEADER_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_default_output_header_type "*) (lambda (s v) (set! (" S_default_output_header_type ") v)))");
  s7_symbol_set_documentation(s7, ss->default_output_header_type_symbol, "*default-output-header-type*: header type when a new file is created (mus-next etc)");

  ss->default_output_data_format_symbol = s7_define_variable(s7, "*" S_default_output_data_format "*", s7_make_integer(s7, DEFAULT_OUTPUT_DATA_FORMAT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_default_output_data_format "*) (lambda (s v) (set! (" S_default_output_data_format ") v)))");
  s7_symbol_set_documentation(s7, ss->default_output_data_format_symbol, "*default-output-data-format*: data format when a new file is created (mus-ldouble etc)");

  ss->default_output_chans_symbol = s7_define_variable(s7, "*" S_default_output_chans "*", s7_make_integer(s7, DEFAULT_OUTPUT_CHANS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_default_output_chans "*) (lambda (s v) (set! (" S_default_output_chans ") v)))");
  s7_symbol_set_documentation(s7, ss->default_output_chans_symbol, "*default-output-chans*: number of channels when a new file is created (1)");

  ss->default_output_srate_symbol = s7_define_variable(s7, "*" S_default_output_srate "*", s7_make_integer(s7, DEFAULT_OUTPUT_SRATE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_default_output_srate "*) (lambda (s v) (set! (" S_default_output_srate ") v)))");
  s7_symbol_set_documentation(s7, ss->default_output_srate_symbol, "*default-output-srate*: sampling rate when a new file is created (44100)");

  ss->spectro_hop_symbol = s7_define_variable(s7, "*" S_spectro_hop "*", s7_make_integer(s7, DEFAULT_SPECTRO_HOP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_spectro_hop "*) (lambda (s v) (set! (" S_spectro_hop ") v)))");
  s7_symbol_set_documentation(s7, ss->spectro_hop_symbol, "*spectro-hop*: hop amount (pixels) in spectral displays");

  ss->color_map_symbol = s7_define_variable(s7, "*" S_colormap "*", s7_make_integer(s7, DEFAULT_COLOR_MAP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_colormap "*) (lambda (s v) (set! (" S_colormap ") v)))");
  s7_symbol_set_documentation(s7, ss->color_map_symbol, "*colormap*: current colormap choice.");

  ss->color_map_size_symbol = s7_define_variable(s7, "*" S_colormap_size "*", s7_make_integer(s7, DEFAULT_COLOR_MAP_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_colormap_size "*) (lambda (s v) (set! (" S_colormap_size ") v)))");
  s7_symbol_set_documentation(s7, ss->color_map_size_symbol, "*colormap-size*: current colormap size; default is 512.");

  ss->wavelet_type_symbol = s7_define_variable(s7, "*" S_wavelet_type "*", s7_make_integer(s7, DEFAULT_WAVELET_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_wavelet_type "*) (lambda (s v) (set! (" S_wavelet_type ") v)))");
  s7_symbol_set_documentation(s7, ss->wavelet_type_symbol, "*wavelet-type*: wavelet used in wavelet-transform (0)");

  ss->dot_size_symbol = s7_define_variable(s7, "*" S_dot_size "*", s7_make_integer(s7, DEFAULT_DOT_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_dot_size "*) (lambda (s v) (set! (" S_dot_size ") v)))");
  s7_symbol_set_documentation(s7, ss->dot_size_symbol, "*dot-size*: size in pixels of dots when graphing with dots (1)");

  ss->zero_pad_symbol = s7_define_variable(s7, "*" S_zero_pad "*", s7_make_integer(s7, DEFAULT_ZERO_PAD));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_zero_pad "*) (lambda (s v) (set! (" S_zero_pad ") v)))");
  s7_symbol_set_documentation(s7, ss->zero_pad_symbol, "*zero-pad*: zero padding used in fft as a multiple of fft size (0)");

  ss->wavo_hop_symbol = s7_define_variable(s7, "*" S_wavo_hop "*", s7_make_integer(s7, DEFAULT_WAVO_HOP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_wavo_hop "*) (lambda (s v) (set! (" S_wavo_hop ") v)))");
  s7_symbol_set_documentation(s7, ss->wavo_hop_symbol, "*wavo-hop*: wavogram spacing between successive traces");

  ss->wavo_trace_symbol = s7_define_variable(s7, "*" S_wavo_trace "*", s7_make_integer(s7, DEFAULT_WAVO_TRACE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_wavo_trace "*) (lambda (s v) (set! (" S_wavo_trace ") v)))");
  s7_symbol_set_documentation(s7, ss->wavo_trace_symbol, "*wavo-trace*: length (samples) of each trace in the wavogram (64)");

  ss->transform_size_symbol = s7_define_variable(s7, "*" S_transform_size "*", s7_make_integer(s7, DEFAULT_TRANSFORM_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_transform_size "*) (lambda (s v) (set! (" S_transform_size ") v)))");
  s7_symbol_set_documentation(s7, ss->transform_size_symbol, "*transform-size*: current fft size (512)");

  ss->fft_window_symbol = s7_define_variable(s7, "*" S_fft_window "*", s7_make_integer(s7, DEFAULT_FFT_WINDOW));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_window "*) (lambda (s v) (set! (" S_fft_window ") v)))");
  s7_symbol_set_documentation(s7, ss->fft_window_symbol, "*fft-window*: fft data window choice (blackman2-window etc)");

  ss->transform_graph_type_symbol = s7_define_variable(s7, "*" S_transform_graph_type "*", s7_make_integer(s7, DEFAULT_TRANSFORM_GRAPH_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_transform_graph_type "*) (lambda (s v) (set! (" S_transform_graph_type ") v)))");
  s7_symbol_set_documentation(s7, ss->transform_graph_type_symbol, "*transform-graph-type* can be graph-once, graph-as-sonogram, or graph-as-spectrogram.");

  ss->time_graph_type_symbol = s7_define_variable(s7, "*" S_time_graph_type "*", s7_make_integer(s7, DEFAULT_TIME_GRAPH_TYPE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_time_graph_type "*) (lambda (s v) (set! (" S_time_graph_type ") v)))");
  s7_symbol_set_documentation(s7, ss->time_graph_type_symbol, "*time-graph-type*: graph-once or graph-as-wavogram");

  ss->ask_before_overwrite_symbol = s7_define_variable(s7, "*" S_ask_before_overwrite "*", s7_make_boolean(s7, DEFAULT_ASK_BEFORE_OVERWRITE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_ask_before_overwrite "*) (lambda (s v) (set! (" S_ask_before_overwrite ") v)))");
  s7_symbol_set_documentation(s7, ss->ask_before_overwrite_symbol, "*ask-before-overwrite*: #t if you want Snd to ask before overwriting a file.");

  ss->ask_about_unsaved_edits_symbol = s7_define_variable(s7, "*" S_ask_about_unsaved_edits "*", s7_make_boolean(s7, DEFAULT_ASK_ABOUT_UNSAVED_EDITS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_ask_about_unsaved_edits "*) (lambda (s v) (set! (" S_ask_about_unsaved_edits ") v)))");
  s7_symbol_set_documentation(s7, ss->ask_about_unsaved_edits_symbol, "*ask-about-unsaved-edits*: #t if you want Snd to ask whether to save unsaved edits when a sound is closed.");

  ss->show_full_duration_symbol = s7_define_variable(s7, "*" S_show_full_duration "*", s7_make_boolean(s7, DEFAULT_SHOW_FULL_DURATION));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_full_duration "*) (lambda (s v) (set! (" S_show_full_duration ") v)))");
  s7_symbol_set_documentation(s7, ss->show_full_duration_symbol, "*show-full-duration*: #t if you want the entire sound displayed whn it is opened.");

  ss->show_full_range_symbol = s7_define_variable(s7, "*" S_show_full_range "*", s7_make_boolean(s7, DEFAULT_SHOW_FULL_RANGE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_full_range "*) (lambda (s v) (set! (" S_show_full_range ") v)))");
  s7_symbol_set_documentation(s7, ss->show_full_range_symbol, "*show-full-range*: #t if you want the graph y-bounds to accommodate the sound's max and min when it is opened.");

  ss->remember_sound_state_symbol = s7_define_variable(s7, "*" S_remember_sound_state "*", s7_make_boolean(s7, DEFAULT_REMEMBER_SOUND_STATE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_remember_sound_state "*) (lambda (s v) (set! (" S_remember_sound_state ") v)))");
  s7_symbol_set_documentation(s7, ss->remember_sound_state_symbol, "*remember-sound-state*: #t if you want a Snd to remember the current state of each sound when it is closed, restoring that state when it is opened again later.");

  ss->save_as_dialog_src_symbol = s7_define_variable(s7, "*" S_save_as_dialog_src "*", s7_make_boolean(s7, DEFAULT_SAVE_AS_DIALOG_SRC));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_save_as_dialog_src "*) (lambda (s v) (set! (" S_save_as_dialog_src ") v)))");
  s7_symbol_set_documentation(s7, ss->save_as_dialog_src_symbol, "*save-as-dialog-src*: #t if you want the 'src' button set by default in the various Save-as dialogs");

  ss->save_as_dialog_auto_comment_symbol = s7_define_variable(s7, "*" S_save_as_dialog_auto_comment "*", s7_make_boolean(s7, DEFAULT_SAVE_AS_DIALOG_AUTO_COMMENT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_save_as_dialog_auto_comment "*) (lambda (s v) (set! (" S_save_as_dialog_auto_comment ") v)))");
  s7_symbol_set_documentation(s7, ss->save_as_dialog_auto_comment_symbol, "*save-as-dialog-auto-comment*: #t if you want the 'auto' button set by default in the various Save-as dialogs");

  ss->with_toolbar_symbol = s7_define_variable(s7, "*" S_with_toolbar "*", s7_make_boolean(s7, DEFAULT_WITH_TOOLBAR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_toolbar "*) (lambda (s v) (set! (" S_with_toolbar ") v)))");
  s7_symbol_set_documentation(s7, ss->with_toolbar_symbol, "*with-toolbar*: #t if you want a toolbar");

  ss->with_tooltips_symbol = s7_define_variable(s7, "*" S_with_tooltips "*", s7_make_boolean(s7, DEFAULT_WITH_TOOLTIPS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_tooltips "*) (lambda (s v) (set! (" S_with_tooltips ") v)))");
  s7_symbol_set_documentation(s7, ss->with_tooltips_symbol, "*with-tooltips*: #t if you want tooltips");

  ss->with_menu_icons_symbol = s7_define_variable(s7, "*" S_with_menu_icons "*", s7_make_boolean(s7, DEFAULT_WITH_MENU_ICONS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_menu_icons "*) (lambda (s v) (set! (" S_with_menu_icons ") v)))");
  s7_symbol_set_documentation(s7, ss->with_menu_icons_symbol, "*with-menu-icons*: #t if you want icons in the menus (gtk only)");

  ss->fft_window_alpha_symbol = s7_define_variable(s7, "*" S_fft_window_alpha "*", s7_make_real(s7, DEFAULT_FFT_WINDOW_ALPHA));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_window_alpha "*) (lambda (s v) (set! (" S_fft_window_alpha ") v)))");
  s7_symbol_set_documentation(s7, ss->fft_window_alpha_symbol, "*fft-window-alpha*: fft window alpha parameter value");

  ss->fft_window_beta_symbol = s7_define_variable(s7, "*" S_fft_window_beta "*", s7_make_real(s7, DEFAULT_FFT_WINDOW_BETA));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_fft_window_beta "*) (lambda (s v) (set! (" S_fft_window_beta ") v)))");
  s7_symbol_set_documentation(s7, ss->fft_window_beta_symbol, "*fft-window-beta*: fft window beta parameter value");

  ss->grid_density_symbol = s7_define_variable(s7, "*" S_grid_density "*", s7_make_real(s7, DEFAULT_GRID_DENSITY));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_grid_density "*) (lambda (s v) (set! (" S_grid_density ") v)))");
  s7_symbol_set_documentation(s7, ss->grid_density_symbol, "*grid-density*: sets how closely axis ticks are spaced, default=1.0");

  ss->initial_beg_symbol = s7_define_variable(s7, "*" S_initial_beg "*", s7_make_real(s7, DEFAULT_INITIAL_BEG));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_initial_beg "*) (lambda (s v) (set! (" S_initial_beg ") v)))");
  s7_symbol_set_documentation(s7, ss->initial_beg_symbol, "*initial-beg*: the begin point (in seconds) for the initial graph of a sound.");

  ss->initial_dur_symbol = s7_define_variable(s7, "*" S_initial_dur "*", s7_make_real(s7, DEFAULT_INITIAL_DUR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_initial_dur "*) (lambda (s v) (set! (" S_initial_dur ") v)))");
  s7_symbol_set_documentation(s7, ss->initial_dur_symbol, "*initial-dur*: the duration (in seconds) for the initial graph of a sound.");

  ss->color_scale_symbol = s7_define_variable(s7, "*" S_color_scale "*", s7_make_real(s7, DEFAULT_COLOR_SCALE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_color_scale "*) (lambda (s v) (set! (" S_color_scale ") v)))");
  s7_symbol_set_documentation(s7, ss->color_scale_symbol, "*color-scale*: darkness setting for colormaps (0.5)");

  ss->color_cutoff_symbol = s7_define_variable(s7, "*" S_color_cutoff "*", s7_make_real(s7, DEFAULT_COLOR_CUTOFF));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_color_cutoff "*) (lambda (s v) (set! (" S_color_cutoff ") v)))");
  s7_symbol_set_documentation(s7, ss->color_cutoff_symbol, "*color-cutoff*: color map cutoff point (default .003).");

  ss->beats_per_minute_symbol = s7_define_variable(s7, "*" S_beats_per_minute "*", s7_make_real(s7, DEFAULT_BEATS_PER_MINUTE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_beats_per_minute "*) (lambda (s v) (set! (" S_beats_per_minute ") v)))");
  s7_symbol_set_documentation(s7, ss->beats_per_minute_symbol, "*beats-per-minute*: beats per minute if x-axis-style is x-axis-in-beats");

  ss->color_inverted_symbol = s7_define_variable(s7, "*" S_color_inverted "*", s7_make_boolean(s7, DEFAULT_COLOR_INVERTED));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_color_inverted "*) (lambda (s v) (set! (" S_color_inverted ") v)))");
  s7_symbol_set_documentation(s7, ss->color_inverted_symbol, "*color-inverted*: whether the colormap in operation should be inverted");

  ss->show_mix_waveforms_symbol = s7_define_variable(s7, "*" S_show_mix_waveforms "*", s7_make_boolean(s7, DEFAULT_SHOW_MIX_WAVEFORMS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_mix_waveforms "*) (lambda (s v) (set! (" S_show_mix_waveforms ") v)))");
  s7_symbol_set_documentation(s7, ss->show_mix_waveforms_symbol, "*show-mix-waveforms*: #t if Snd should display mix waveforms (above the main waveform)");

  ss->mix_waveform_height_symbol = s7_define_variable(s7, "*" S_mix_waveform_height "*", s7_make_integer(s7, DEFAULT_MIX_WAVEFORM_HEIGHT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mix_waveform_height "*) (lambda (s v) (set! (" S_mix_waveform_height ") v)))");
  s7_symbol_set_documentation(s7, ss->mix_waveform_height_symbol, "*mix-waveform-height*: max height (pixels) of mix waveforms (20)");

  ss->beats_per_measure_symbol = s7_define_variable(s7, "*" S_beats_per_measure "*", s7_make_integer(s7, DEFAULT_BEATS_PER_MEASURE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_beats_per_measure "*) (lambda (s v) (set! (" S_beats_per_measure ") v)))");
  s7_symbol_set_documentation(s7, ss->beats_per_measure_symbol, "*beats-per-measure*: beats per measure if x-axis-style is x-axis-in-measures");

  ss->transform_normalization_symbol = s7_define_variable(s7, "*" S_transform_normalization "*", s7_make_integer(s7, DEFAULT_TRANSFORM_NORMALIZATION));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_transform_normalization "*) (lambda (s v) (set! (" S_transform_normalization ") v)))");
  s7_symbol_set_documentation(s7, ss->transform_normalization_symbol, "*transform-normalization*: dont-normalize, normalize-by-channel, normalize-by-sound, or normalize-globally.");

  ss->sinc_width_symbol = s7_define_variable(s7, "*" S_sinc_width "*", s7_make_integer(s7, DEFAULT_SINC_WIDTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_sinc_width "*) (lambda (s v) (set! (" S_sinc_width ") v)))");
  s7_symbol_set_documentation(s7, ss->sinc_width_symbol, "*sinc-width*: sampling rate conversion sinc width (10).");

  ss->x_axis_style_symbol = s7_define_variable(s7, "*" S_x_axis_style "*", s7_make_integer(s7, DEFAULT_X_AXIS_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_x_axis_style "*) (lambda (s v) (set! (" S_x_axis_style ") v)))");
  s7_symbol_set_documentation(s7, ss->x_axis_style_symbol, "*x-axis-style*: The x axis labelling of the time domain waveform (x-axis-in-seconds etc)");

  ss->zoom_focus_style_symbol = s7_define_variable(s7, "*" S_zoom_focus_style "*", s7_make_integer(s7, DEFAULT_ZOOM_FOCUS_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_zoom_focus_style "*) (lambda (s v) (set! (" S_zoom_focus_style ") v)))");
  s7_symbol_set_documentation(s7, ss->zoom_focus_style_symbol, "*zoom-focus-style*: determines what zooming centers on (zoom-focus-active etc).");

  ss->graph_style_symbol = s7_define_variable(s7, "*" S_graph_style "*", s7_make_integer(s7, DEFAULT_GRAPH_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_graph_style "*) (lambda (s v) (set! (" S_graph_style ") v)))");
  s7_symbol_set_documentation(s7, ss->graph_style_symbol, "*graph-style*: graph style (graph-lines etc)");

  ss->region_graph_style_symbol = s7_define_variable(s7, "*" S_region_graph_style "*", s7_make_integer(s7, ss->Region_Graph_Style));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_region_graph_style "*) (lambda (s v) (set! (" S_region_graph_style ") v)))");
  s7_symbol_set_documentation(s7, ss->region_graph_style_symbol, "*region-graph-style*: graph style of the region dialog graph (graph-lines etc)");

  ss->auto_resize_symbol = s7_define_variable(s7, "*" S_auto_resize "*", s7_make_boolean(s7, DEFAULT_AUTO_RESIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_auto_resize "*) (lambda (s v) (set! (" S_auto_resize ") v)))");
  s7_symbol_set_documentation(s7, ss->auto_resize_symbol, "*auto-resize*: #t if Snd can change its main window size as it pleases");

  ss->auto_update_symbol = s7_define_variable(s7, "*" S_auto_update "*", s7_make_boolean(s7, DEFAULT_AUTO_UPDATE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_auto_update "*) (lambda (s v) (set! (" S_auto_update ") v)))");
  s7_symbol_set_documentation(s7, ss->auto_update_symbol, "*auto-update*: #t if Snd should automatically update a file if it changes unexpectedly");

  ss->max_regions_symbol = s7_define_variable(s7, "*" S_max_regions "*", s7_make_integer(s7, DEFAULT_MAX_REGIONS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_max_regions "*) (lambda (s v) (set! (" S_max_regions ") v)))");
  s7_symbol_set_documentation(s7, ss->max_regions_symbol, "*max-regions*: max number of regions saved on the region list");

  ss->max_transform_peaks_symbol = s7_define_variable(s7, "*" S_max_transform_peaks "*", s7_make_integer(s7, DEFAULT_MAX_TRANSFORM_PEAKS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_max_transform_peaks "*) (lambda (s v) (set! (" S_max_transform_peaks ") v)))");
  s7_symbol_set_documentation(s7, ss->max_transform_peaks_symbol, "*max-transform-peaks*: max number of fft peaks reported in fft display");

  ss->audio_output_device_symbol = s7_define_variable(s7, "*" S_audio_output_device "*", s7_make_integer(s7, DEFAULT_AUDIO_OUTPUT_DEVICE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_audio_output_device "*) (lambda (s v) (set! (" S_audio_output_device ") v)))");
  s7_symbol_set_documentation(s7, ss->audio_output_device_symbol, "*audio-output-device*: the current sndlib default output device (mus-audio-default)");

  ss->audio_input_device_symbol = s7_define_variable(s7, "*" S_audio_input_device "*", s7_make_integer(s7, DEFAULT_AUDIO_INPUT_DEVICE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_audio_input_device "*) (lambda (s v) (set! (" S_audio_input_device ") v)))");
  s7_symbol_set_documentation(s7, ss->audio_input_device_symbol, "*audio-input-device*: the current sndlib default input device (mus-audio-default)");

  ss->with_gl_symbol = s7_define_variable(s7, "*" S_with_gl "*", s7_make_boolean(s7, DEFAULT_WITH_GL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_gl "*) (lambda (s v) (set! (" S_with_gl ") v)))");
  s7_symbol_set_documentation(s7, ss->with_gl_symbol, "*with-gl*: #t if Snd should use GL graphics");

  ss->with_relative_panes_symbol = s7_define_variable(s7, "*" S_with_relative_panes "*", s7_make_boolean(s7, DEFAULT_WITH_RELATIVE_PANES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_relative_panes "*) (lambda (s v) (set! (" S_with_relative_panes ") v)))");
  s7_symbol_set_documentation(s7, ss->with_relative_panes_symbol, "*with-relative-panes*: #t if multichannel sounds should try to maintain relative pane sizes");

  ss->print_length_symbol = s7_define_variable(s7, "*" S_print_length "*", s7_make_integer(s7, DEFAULT_PRINT_LENGTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_print_length "*) (lambda (s v) (set! (" S_print_length ") v)))");
  s7_symbol_set_documentation(s7, ss->print_length_symbol, "*print-length*: number of vector elements to print in the listener (12)");

  ss->dac_size_symbol = s7_define_variable(s7, "*" S_dac_size "*", s7_make_integer(s7, DEFAULT_DAC_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_dac_size "*) (lambda (s v) (set! (" S_dac_size ") v)))");
  s7_symbol_set_documentation(s7, ss->dac_size_symbol, "*dac-size*: the current DAC buffer size in framples (256)");

  ss->view_files_sort_symbol = s7_define_variable(s7, "*" S_view_files_sort "*", s7_make_integer(s7, DEFAULT_VIEW_FILES_SORT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_view_files_sort "*) (lambda (s v) (set! (" S_view_files_sort ") v)))");

  ss->dac_combines_channels_symbol = s7_define_variable(s7, "*" S_dac_combines_channels "*", s7_make_boolean(s7, DEFAULT_DAC_COMBINES_CHANNELS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_dac_combines_channels "*) (lambda (s v) (set! (" S_dac_combines_channels ") v)))");
  s7_symbol_set_documentation(s7, ss->dac_combines_channels_symbol, "*dac-combines-channels*: #t if extra channels are to be mixed into available ones during playing.");

  ss->show_selection_transform_symbol = s7_define_variable(s7, "*" S_show_selection_transform "*", s7_make_boolean(s7, DEFAULT_SHOW_SELECTION_TRANSFORM));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_selection_transform "*) (lambda (s v) (set! (" S_show_selection_transform ") v)))");
  s7_symbol_set_documentation(s7, ss->show_selection_transform_symbol, "*show-selection-transform*: #t if transform display reflects selection, not time-domain window");

  ss->with_mix_tags_symbol = s7_define_variable(s7, "*" S_with_mix_tags "*", s7_make_boolean(s7, DEFAULT_WITH_MIX_TAGS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_mix_tags "*) (lambda (s v) (set! (" S_with_mix_tags ") v)))");
  s7_symbol_set_documentation(s7, ss->with_mix_tags_symbol, "*with-mix-tags*: #t if Snd should try to use virtual (tagged) mixing");

  ss->selection_creates_region_symbol = s7_define_variable(s7, "*" S_selection_creates_region "*", s7_make_boolean(s7, DEFAULT_SELECTION_CREATES_REGION));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_selection_creates_region "*) (lambda (s v) (set! (" S_selection_creates_region ") v)))");
  s7_symbol_set_documentation(s7, ss->selection_creates_region_symbol, "*selection-creates-region*: #t if a region should be created each time a selection is made.");

  ss->save_state_file_symbol = s7_define_variable(s7, "*" S_save_state_file "*", s7_make_string(s7, DEFAULT_SAVE_STATE_FILE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_save_state_file "*) (lambda (s v) (set! (" S_save_state_file ") v)))");
  s7_symbol_set_documentation(s7, ss->save_state_file_symbol, "*save-state-file*: the name of the saved state file (\"saved-snd.scm\")");

  ss->listener_prompt_symbol = s7_define_variable(s7, "*" S_listener_prompt "*", s7_make_string(s7, DEFAULT_LISTENER_PROMPT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_listener_prompt "*) (lambda (s v) (set! (" S_listener_prompt ") v)))");
  s7_symbol_set_documentation(s7, ss->listener_prompt_symbol, "*listener-prompt*: the current lisp listener prompt character ('>') ");

  ss->enved_base_symbol = s7_define_variable(s7, "*" S_enved_base "*", s7_make_real(s7, DEFAULT_ENVED_BASE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_base "*) (lambda (s v) (set! (" S_enved_base ") v)))");
  s7_symbol_set_documentation(s7, ss->enved_base_symbol, "*enved-base*: envelope editor exponential base value (1.0)");

  ss->enved_power_symbol = s7_define_variable(s7, "*" S_enved_power "*", s7_make_real(s7, DEFAULT_ENVED_POWER));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_power "*) (lambda (s v) (set! (" S_enved_power ") v)))");
  s7_symbol_set_documentation(s7, ss->enved_power_symbol, "*enved-power*: envelope editor base scale range (9.0^power)");

  ss->auto_update_interval_symbol = s7_define_variable(s7, "*" S_auto_update_interval "*", s7_make_real(s7, DEFAULT_AUTO_UPDATE_INTERVAL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_auto_update_interval "*) (lambda (s v) (set! (" S_auto_update_interval ") v)))");
  s7_symbol_set_documentation(s7, ss->auto_update_interval_symbol, "*auto-update-interval*: time (seconds) between background checks for changed file on disk (60)");

  ss->enved_with_wave_symbol = s7_define_variable(s7, "*" S_enved_with_wave "*", s7_make_boolean(s7, DEFAULT_ENVED_WITH_WAVE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_with_wave "*) (lambda (s v) (set! (" S_enved_with_wave ") v)))");
  s7_symbol_set_documentation(s7, ss->enved_with_wave_symbol, "*enved-wave?*: #t if the envelope editor is displaying the waveform to be edited");

  ss->graphs_horizontal_symbol = s7_define_variable(s7, "*" S_graphs_horizontal "*", s7_make_boolean(s7, DEFAULT_GRAPHS_HORIZONTAL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_graphs_horizontal "*) (lambda (s v) (set! (" S_graphs_horizontal ") v)))");
  s7_symbol_set_documentation(s7, ss->graphs_horizontal_symbol, "*graphs-horizontal*: #t if the time domain, fft, and lisp graphs are layed out horizontally");

  ss->with_background_processes_symbol = s7_define_variable(s7, "*" S_with_background_processes "*", s7_make_boolean(s7, DEFAULT_WITH_BACKGROUND_PROCESSES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_background_processes "*) (lambda (s v) (set! (" S_with_background_processes ") v)))");
  s7_symbol_set_documentation(s7, ss->with_background_processes_symbol, "*with-background-processes*: #t if Snd should use background (idle time) processing");

  ss->with_file_monitor_symbol = s7_define_variable(s7, "*" S_with_file_monitor "*", s7_make_boolean(s7, DEFAULT_WITH_FILE_MONITOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_file_monitor "*) (lambda (s v) (set! (" S_with_file_monitor ") v)))");
  s7_symbol_set_documentation(s7, ss->with_file_monitor_symbol, "*with-file-monitor*: #t if the file alteration monitor is active");

  ss->enved_style_symbol = s7_define_variable(s7, "*" S_enved_style "*", s7_make_integer(s7, DEFAULT_ENVED_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_style "*) (lambda (s v) (set! (" S_enved_style ") v)))");
  s7_symbol_set_documentation(s7, ss->enved_style_symbol, "*enved-style*: envelope editor breakpoint connection choice: envelope-linear or envelope-exponential");

  ss->graph_cursor_symbol = s7_define_variable(s7, "*" S_graph_cursor "*", s7_make_integer(s7, DEFAULT_GRAPH_CURSOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_graph_cursor "*) (lambda (s v) (set! (" S_graph_cursor ") v)))");
  s7_symbol_set_documentation(s7, ss->graph_cursor_symbol, "*graph-cursor*: current graph cursor shape");

  ss->mix_tag_width_symbol = s7_define_variable(s7, "*" S_mix_tag_width "*", s7_make_integer(s7, DEFAULT_MIX_TAG_WIDTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mix_tag_width "*) (lambda (s v) (set! (" S_mix_tag_width ") v)))");
  s7_symbol_set_documentation(s7, ss->mix_tag_width_symbol, "*mix-tag-width*: width (pixels) of mix tags (6)");

  ss->mix_tag_height_symbol = s7_define_variable(s7, "*" S_mix_tag_height "*", s7_make_integer(s7, DEFAULT_MIX_TAG_HEIGHT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mix_tag_height "*) (lambda (s v) (set! (" S_mix_tag_height ") v)))");
  s7_symbol_set_documentation(s7, ss->mix_tag_height_symbol, "*mix-tag-height*: height (pixels) of mix tags (14)");

  ss->mark_tag_height_symbol = s7_define_variable(s7, "*" S_mark_tag_height "*", s7_make_integer(s7, DEFAULT_MARK_TAG_HEIGHT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mark_tag_height "*) (lambda (s v) (set! (" S_mark_tag_height ") v)))");
  s7_symbol_set_documentation(s7, ss->mark_tag_height_symbol, "*mark-tag-height*: height (pixels) of mark tags (4)");

  ss->mark_tag_width_symbol = s7_define_variable(s7, "*" S_mark_tag_width "*", s7_make_integer(s7, DEFAULT_MARK_TAG_WIDTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mark_tag_width "*) (lambda (s v) (set! (" S_mark_tag_width ") v)))");
  s7_symbol_set_documentation(s7, ss->mark_tag_width_symbol, "*mark-tag-width*: width (pixels) of mark tags (10)");

  ss->enved_target_symbol = s7_define_variable(s7, "*" S_enved_target "*", s7_make_integer(s7, DEFAULT_ENVED_TARGET));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_target "*) (lambda (s v) (set! (" S_enved_target ") v)))");
  s7_symbol_set_documentation(s7, ss->enved_target_symbol, "*enved-target*: determines how the envelope edit envelope is applied; enved-amplitude etc");

  ss->clipping_symbol = s7_define_variable(s7, "*" S_clipping "*", s7_make_boolean(s7, DEFAULT_CLIPPING));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_clipping "*) (lambda (s v) (set! (" S_clipping ") v)))");
  s7_symbol_set_documentation(s7, ss->clipping_symbol, "*clipping*: #t if Snd should clip output values");

  ss->show_indices_symbol = s7_define_variable(s7, "*" S_show_indices "*", s7_make_boolean(s7, DEFAULT_SHOW_INDICES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_indices "*) (lambda (s v) (set! (" S_show_indices ") v)))");
  s7_symbol_set_documentation(s7, ss->show_indices_symbol, "*show-indices*: #t if sound name should be preceded by its index in the sound display.");

  ss->just_sounds_symbol = s7_define_variable(s7, "*" S_just_sounds "*", s7_make_boolean(s7, DEFAULT_JUST_SOUNDS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_just_sounds "*) (lambda (s v) (set! (" S_just_sounds ") v)))");
  s7_symbol_set_documentation(s7, ss->just_sounds_symbol, "*just-sounds*: the 'just sounds' choice in the file chooser dialog");

  ss->cursor_size_symbol = s7_define_variable(s7, "*" S_cursor_size "*", s7_make_integer(s7, DEFAULT_CURSOR_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_size "*) (lambda (s v) (set! (" S_cursor_size ") v)))");
  s7_symbol_set_documentation(s7, ss->cursor_size_symbol, "*cursor-size*: current cursor size");

  ss->cursor_style_symbol = s7_define_variable(s7, "*" S_cursor_style "*", s7_make_integer(s7, DEFAULT_CURSOR_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_style "*) (lambda (s v) (set! (" S_cursor_style ") v)))");
  s7_symbol_set_documentation(s7, ss->cursor_style_symbol, "*cursor-style*: current cursor shape (cursor-cross etc)");

  ss->tracking_cursor_style_symbol = s7_define_variable(s7, "*" S_tracking_cursor_style "*", s7_make_integer(s7, DEFAULT_TRACKING_CURSOR_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_tracking_cursor_style "*) (lambda (s v) (set! (" S_tracking_cursor_style ") v)))");
  s7_symbol_set_documentation(s7, ss->tracking_cursor_style_symbol, "*tracking-cursor-style*: current tracking cursor shape (cursor-cross, cursor-line)");

  ss->filter_control_in_db_symbol = s7_define_variable(s7, "*" S_filter_control_in_dB "*", s7_make_boolean(s7, DEFAULT_FILTER_CONTROL_IN_DB));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_filter_control_in_dB "*) (lambda (s v) (set! (" S_filter_control_in_dB ") v)))");
  s7_symbol_set_documentation(s7, ss->filter_control_in_db_symbol, "*filter-control-in-dB*: #t if snd's filter envelope is displayed in dB in control panel");

  ss->filter_control_in_hz_symbol = s7_define_variable(s7, "*" S_filter_control_in_hz "*", s7_make_boolean(s7, DEFAULT_FILTER_CONTROL_IN_HZ));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_filter_control_in_hz "*) (lambda (s v) (set! (" S_filter_control_in_hz ") v)))");
  s7_symbol_set_documentation(s7, ss->filter_control_in_hz_symbol, "*filter-control-in-hz*: #t if snd's filter envelope x axis should be in hz (control panel filter)");

  ss->show_sonogram_cursor_symbol = s7_define_variable(s7, "*" S_show_sonogram_cursor "*", s7_make_boolean(s7, DEFAULT_SHOW_SONOGRAM_CURSOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_sonogram_cursor "*) (lambda (s v) (set! (" S_show_sonogram_cursor ") v)))");
  s7_symbol_set_documentation(s7, ss->show_sonogram_cursor_symbol, "*show-sonogram-cursor*: #t if Snd should display a cursor in the sonogram");

  ss->speed_control_tones_symbol = s7_define_variable(s7, "*" S_speed_control_tones "*", s7_make_integer(s7, DEFAULT_SPEED_CONTROL_TONES));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_speed_control_tones "*) (lambda (s v) (set! (" S_speed_control_tones ") v)))");
  s7_symbol_set_documentation(s7, ss->speed_control_tones_symbol, "*speed-control-tones*: the speed-control octave divisions (12)");

  ss->speed_control_style_symbol = s7_define_variable(s7, "*" S_speed_control_style "*", s7_make_integer(s7, DEFAULT_SPEED_CONTROL_STYLE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_speed_control_style "*) (lambda (s v) (set! (" S_speed_control_style ") v)))");
  s7_symbol_set_documentation(s7, ss->speed_control_style_symbol, "*speed-control-style*: speed control choice (speed-control-as-float etc)");

  ss->expand_control_length_symbol = s7_define_variable(s7, "*" S_expand_control_length "*", s7_make_real(s7, DEFAULT_EXPAND_CONTROL_LENGTH));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_expand_control_length "*) (lambda (s v) (set! (" S_expand_control_length ") v)))");
  s7_symbol_set_documentation(s7, ss->expand_control_length_symbol, "*expand-control-length*: current expansion segment length in seconds (.15)");

  ss->expand_control_ramp_symbol = s7_define_variable(s7, "*" S_expand_control_ramp "*", s7_make_real(s7, DEFAULT_EXPAND_CONTROL_RAMP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_expand_control_ramp "*) (lambda (s v) (set! (" S_expand_control_ramp ") v)))");
  s7_symbol_set_documentation(s7, ss->expand_control_ramp_symbol, "*expand-control-ramp*: current expansion ramp time (.4)");

  ss->expand_control_hop_symbol = s7_define_variable(s7, "*" S_expand_control_hop "*", s7_make_real(s7, DEFAULT_EXPAND_CONTROL_HOP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_expand_control_hop "*) (lambda (s v) (set! (" S_expand_control_hop ") v)))");
  s7_symbol_set_documentation(s7, ss->expand_control_hop_symbol, "*expand-control-hop*: current expansion output grain spacing in seconds (0.05)");

  ss->expand_control_jitter_symbol = s7_define_variable(s7, "*" S_expand_control_jitter "*", s7_make_real(s7, DEFAULT_EXPAND_CONTROL_JITTER));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_expand_control_jitter "*) (lambda (s v) (set! (" S_expand_control_jitter ") v)))");
  s7_symbol_set_documentation(s7, ss->expand_control_jitter_symbol, "*expand-control-jitter*: current expansion output grain spacing jitter (0.1)");

  ss->contrast_control_amp_symbol = s7_define_variable(s7, "*" S_contrast_control_amp "*", s7_make_real(s7, DEFAULT_CONTRAST_CONTROL_AMP));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_contrast_control_amp "*) (lambda (s v) (set! (" S_contrast_control_amp ") v)))");
  s7_symbol_set_documentation(s7, ss->contrast_control_amp_symbol, "*contrast-control-amp*: contrast amp");

  ss->reverb_control_feedback_symbol = s7_define_variable(s7, "*" S_reverb_control_feedback "*", s7_make_real(s7, DEFAULT_REVERB_CONTROL_FEEDBACK));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_reverb_control_feedback "*) (lambda (s v) (set! (" S_reverb_control_feedback ") v)))");
  s7_symbol_set_documentation(s7, ss->reverb_control_feedback_symbol, "*reverb-control-feedback*: control-panel reverb feedback scaler");

  ss->reverb_control_lowpass_symbol = s7_define_variable(s7, "*" S_reverb_control_lowpass "*", s7_make_real(s7, DEFAULT_REVERB_CONTROL_LOWPASS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_reverb_control_lowpass "*) (lambda (s v) (set! (" S_reverb_control_lowpass ") v)))");
  s7_symbol_set_documentation(s7, ss->reverb_control_lowpass_symbol, "*reverb-control-lowpass*: control-panel reverb lowpass filter coefficient");

  ss->reverb_control_decay_symbol = s7_define_variable(s7, "*" S_reverb_control_decay "*", s7_make_real(s7, DEFAULT_REVERB_CONTROL_DECAY));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_reverb_control_decay "*) (lambda (s v) (set! (" S_reverb_control_decay ") v)))");
  s7_symbol_set_documentation(s7, ss->reverb_control_decay_symbol, "*reverb-control-decay*: control-panel reverb decay time (1.0 seconds)");

  ss->cursor_update_interval_symbol = s7_define_variable(s7, "*" S_cursor_update_interval "*", s7_make_real(s7, DEFAULT_CURSOR_UPDATE_INTERVAL));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_update_interval "*) (lambda (s v) (set! (" S_cursor_update_interval ") v)))");
  s7_symbol_set_documentation(s7, ss->cursor_update_interval_symbol, "*cursor-update-interval*: time (seconds) between cursor updates if with-tracking-cursor.");

  ss->filter_control_order_symbol = s7_define_variable(s7, "*" S_filter_control_order "*", s7_make_integer(s7, DEFAULT_FILTER_CONTROL_ORDER));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_filter_control_order "*) (lambda (s v) (set! (" S_filter_control_order ") v)))");
  s7_symbol_set_documentation(s7, ss->filter_control_order_symbol, "*filter-control-order*: control-panel filter order");

  ss->cursor_location_offset_symbol = s7_define_variable(s7, "*" S_cursor_location_offset "*", s7_make_integer(s7, DEFAULT_CURSOR_LOCATION_OFFSET));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_location_offset "*) (lambda (s v) (set! (" S_cursor_location_offset ") v)))");
  s7_symbol_set_documentation(s7, ss->cursor_location_offset_symbol, "*cursor-location-offset*: samples added to cursor location if cursor displayed during play.");

  ss->play_arrow_size_symbol = s7_define_variable(s7, "*" S_play_arrow_size "*", s7_make_integer(s7, DEFAULT_PLAY_ARROW_SIZE));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_play_arrow_size "*) (lambda (s v) (set! (" S_play_arrow_size ") v)))");
  s7_symbol_set_documentation(s7, ss->play_arrow_size_symbol, "*play-arrow-size*: the size of the play triangles");

  ss->min_db_symbol = s7_define_variable(s7, "*" S_min_dB "*", s7_make_real(s7, DEFAULT_MIN_DB));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_min_dB "*) (lambda (s v) (set! (" S_min_dB ") v)))");
  s7_symbol_set_documentation(s7, ss->min_db_symbol, "*min-dB*: min dB value displayed in fft graphs using dB scales (-60)");

  ss->show_controls_symbol = s7_define_variable(s7, "*" S_show_controls "*", s7_make_boolean(s7, DEFAULT_SHOW_CONTROLS));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_show_controls "*) (lambda (s v) (set! (" S_show_controls ") v)))");
  s7_symbol_set_documentation(s7, ss->show_controls_symbol, "*show-controls*: #t if snd's control panel is known to be open");

  ss->with_tracking_cursor_symbol = s7_define_variable(s7, "*" S_with_tracking_cursor "*", s7_make_integer(s7, (int)DEFAULT_WITH_TRACKING_CURSOR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_with_tracking_cursor "*) (lambda (s v) (set! (" S_with_tracking_cursor ") v)))");
  s7_symbol_set_documentation(s7, ss->with_tracking_cursor_symbol, "*with-tracking-cursor*: #t if cursor always moves along in waveform display as sound is played");

  ss->html_dir_symbol = s7_define_variable(s7, "*" S_html_dir "*", s7_make_string(s7, DEFAULT_HTML_DIR));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_html_dir "*) (lambda (s v) (set! (" S_html_dir ") v)))");
  s7_symbol_set_documentation(s7, ss->html_dir_symbol, "*html-dir*: location of Snd documentation");

  ss->html_program_symbol = s7_define_variable(s7, "*" S_html_program "*", s7_make_string(s7, DEFAULT_HTML_PROGRAM));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_html_program "*) (lambda (s v) (set! (" S_html_program ") v)))");
  s7_symbol_set_documentation(s7, ss->html_program_symbol, "*html-program*: name of documentation reader (firefox)");

  ss->open_file_dialog_directory_symbol = s7_define_variable(s7, "*" S_open_file_dialog_directory "*", s7_make_string(s7, ss->Open_File_Dialog_Directory));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_open_file_dialog_directory "*) (lambda (s v) (set! (" S_open_file_dialog_directory ") v)))");
  s7_symbol_set_documentation(s7, ss->open_file_dialog_directory_symbol, "*open-file-dialog-directory*: name of directory for initial open file dialog search");

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

  s7_symbol_set_documentation(s7, ss->axis_color_symbol, "*axis-color*: color of axis (defaults to current data color)");
  s7_symbol_set_documentation(s7, ss->basic_color_symbol, "*basic-color*: Snd's basic color");
  s7_symbol_set_documentation(s7, ss->cursor_color_symbol, "*cursor-color*: cursor color");
  s7_symbol_set_documentation(s7, ss->data_color_symbol, "*data-color*: color used to draw unselected data");
  s7_symbol_set_documentation(s7, ss->enved_waveform_color_symbol, "*enved-waveform-color*: color of the envelope editor wave display");
  s7_symbol_set_documentation(s7, ss->filter_control_waveform_color_symbol, "*filter-control-waveform-color*: color of the filter waveform");
  s7_symbol_set_documentation(s7, ss->graph_color_symbol, "*graph-color*: background color used for unselected data");
  s7_symbol_set_documentation(s7, ss->highlight_color_symbol, "*highlight-color*: color of highlighted text or buttons");
  s7_symbol_set_documentation(s7, ss->listener_color_symbol, "*listener-color*: background color of the lisp listener");
  s7_symbol_set_documentation(s7, ss->listener_text_color_symbol, "*listener-text-color*: text color in the lisp listener");
  s7_symbol_set_documentation(s7, ss->mark_color_symbol, "*mark-color*: mark color");
  s7_symbol_set_documentation(s7, ss->mix_color_symbol, "*mix-color*: color of mix tags");
  s7_symbol_set_documentation(s7, ss->position_color_symbol, "*position-color*: color of position sliders");
  s7_symbol_set_documentation(s7, ss->sash_color_symbol, "*sash-color*: color used to draw paned window sashes");
  s7_symbol_set_documentation(s7, ss->selected_data_color_symbol, "*selected-data-color*: color used for selected data");
  s7_symbol_set_documentation(s7, ss->selected_graph_color_symbol, "*selected-graph-color*: background color of selected data");
  s7_symbol_set_documentation(s7, ss->selection_color_symbol, "*selection-color*: selection color");
  s7_symbol_set_documentation(s7, ss->text_focus_color_symbol, "*text-focus-color*: color used to show a text field has focus");
  s7_symbol_set_documentation(s7, ss->zoom_color_symbol, "*zoom-color*: color of zoom sliders");

#if (!USE_NO_GUI)
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_data_color "*) (lambda (s v) (set! (" S_data_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_selected_data_color "*) (lambda (s v) (set! (" S_selected_data_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mark_color "*) (lambda (s v) (set! (" S_mark_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_graph_color "*) (lambda (s v) (set! (" S_graph_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_selected_graph_color "*) (lambda (s v) (set! (" S_selected_graph_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_listener_color "*) (lambda (s v) (set! (" S_listener_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_listener_text_color "*) (lambda (s v) (set! (" S_listener_text_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_basic_color "*) (lambda (s v) (set! (" S_basic_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_selection_color "*) (lambda (s v) (set! (" S_selection_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_zoom_color "*) (lambda (s v) (set! (" S_zoom_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_position_color "*) (lambda (s v) (set! (" S_position_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_highlight_color "*) (lambda (s v) (set! (" S_highlight_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_enved_waveform_color "*) (lambda (s v) (set! (" S_enved_waveform_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_cursor_color "*) (lambda (s v) (set! (" S_cursor_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_text_focus_color "*) (lambda (s v) (set! (" S_text_focus_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_filter_control_waveform_color "*) (lambda (s v) (set! (" S_filter_control_waveform_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mix_color "*) (lambda (s v) (set! (" S_mix_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_sash_color "*) (lambda (s v) (set! (" S_sash_color ") v)))");
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_axis_color "*) (lambda (s v) (set! (" S_axis_color ") v)))");
#endif
#endif
}


#if HAVE_GSL
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_errno.h>

/* default gsl error handler apparently aborts main program! */

static void snd_gsl_error(const char *reason, const char *file, int line, int gsl_errno)
{
  Xen_error(Xen_make_error_type("gsl-error"),
	    Xen_list_6(C_string_to_Xen_string("GSL: ~A, ~A in ~A line ~A, gsl err: ~A"),
		       C_string_to_Xen_string(gsl_strerror(gsl_errno)),
		       C_string_to_Xen_string(reason),
		       C_string_to_Xen_string(file),
		       C_int_to_Xen_integer(line),
		       C_int_to_Xen_integer(gsl_errno)));
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
  ss->search_proc = Xen_undefined;
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

  ss->mus_error_hook = Xen_define_hook(S_mus_error_hook, "(make-hook 'type 'message)", 2, H_mus_error_hook);
}
