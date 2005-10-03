#include "snd.h"

/* preferences dialog; layout design taken from webmail
 *
 * also add an html-style table of contents at top -- basically mimic a web-page
 */


/* start automatically if no .snd? -- create empty if not wanted?
 */


static Widget preferences_dialog = NULL;

static void preferences_help_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_help("preferences",
	   "This dialog is under construction...",
	   WITH_WORD_WRAP);
}

static void preferences_quit_callback(Widget w, XtPointer context, XtPointer info) 
{
  if (XmGetFocusWidget(preferences_dialog) == XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(preferences_dialog);
}

static void preferences_reset_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_set_global_defaults(true); /* need complete redisplay, what about snd/chn stuff? also max-regions et al need check */
}

static void preferences_save_callback(Widget w, XtPointer context, XtPointer info) 
{
  save_options_in_prefs(); /* returns filename, need redirect + msg */
}

typedef struct {
  Widget label, text, arrow_up, arrow_down, error;
  bool got_error;
} prefs_info;

static void clear_prefs_error(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  XtRemoveCallback(prf->text, XmNvalueChangedCallback, clear_prefs_error, context);
  set_label(prf->error, "");
}

static void post_prefs_error(const char *msg, void *data)
{
  prefs_info *prf = (prefs_info *)data;
  prf->got_error = true;
  set_label(prf->error, msg);
  XtAddCallback(prf->text, XmNvalueChangedCallback, clear_prefs_error, (void *)prf);
}

static void fft_size_up(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  off_t size;
  char *new_size;
  size = transform_size(ss) * 2;
  if (size >= 1073741824)
    XtSetSensitive(w, false);
  if (size > 2)
    XtSetSensitive(prf->arrow_down, true);
  in_set_transform_size(size);
  new_size = mus_format(OFF_TD, transform_size(ss));
  XmTextSetString(prf->text, new_size);
  FREE(new_size);
}

static void fft_size_down(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  off_t size;
  char *new_size;
  size = transform_size(ss) / 2;
  if (size <= 2)
    XtSetSensitive(w, false);
  if (size < 1073741824)
    XtSetSensitive(prf->arrow_up, true);
  in_set_transform_size(size);
  new_size = mus_format(OFF_TD, transform_size(ss));
  XmTextSetString(prf->text, new_size);
  FREE(new_size);
}

static void fft_size_from_text(Widget w, XtPointer context, XtPointer info) 
{
  prefs_info *prf = (prefs_info *)context;
  off_t size;
  char *str;
  str = XmTextGetString(w);
  if ((str) && (*str))
    {
      prf->got_error = false;
      redirect_errors_to(post_prefs_error, (void *)prf);
      size = string_to_off_t(str, 0, "size"); 
      redirect_errors_to(NULL, NULL);
      XtFree(str);
      if (!(prf->got_error))
	{
	  if (POWER_OF_2_P(size))
	    {
	      if (size >= 2)
		{
		  if (size <= 1073741824)
		    in_set_transform_size(size);
		  else post_prefs_error("size > 1073741824?", (void *)prf);
		}
	      else post_prefs_error("size < 2?", (void *)prf);
	    }
	  else post_prefs_error("size must be a power of 2", (void *)prf);
	}
      else prf->got_error = false;
    }
  else post_prefs_error("no size?", (void *)prf);
}

static prefs_info *prefs_row_with_number(const char *label, const char *value, 
					 Widget box, Widget top_widget, 
					 XtCallbackProc size_up, XtCallbackProc size_down, XtCallbackProc size_from_text)
{
  Arg args[20];
  int n;
  prefs_info *prf = NULL;
  Widget sep;
  prf = (prefs_info *)CALLOC(1, sizeof(prefs_info));

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg(args[n], XmNrightPosition, 49); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->label = XtCreateManagedWidget(label, xmLabelWidgetClass, box, args, n);
  
  /* ideally we'd use one of the Motif Combo|Spin boxes, but they are all execrable */
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, prf->label); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->label); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  XtSetArg(args[n], XmNwidth, 16); n++;
  XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_OUT); n++;
  sep = XtCreateManagedWidget("sep", xmSeparatorWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, sep); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNcolumns, 12); n++;
  XtSetArg(args[n], XmNvalue, value); n++;
  XtSetArg(args[n], XmNmarginHeight, 1); n++;
  XtSetArg(args[n], XmNborderWidth, 0); n++;
  XtSetArg(args[n], XmNborderColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNbottomShadowColor, ss->sgx->white); n++;
  XtSetArg(args[n], XmNshadowThickness, 0); n++;
  XtSetArg(args[n], XmNtopShadowColor, ss->sgx->white); n++;
  prf->text = make_textfield_widget("size-text", box, args, n, ACTIVATABLE_BUT_NOT_FOCUSED, NO_COMPLETER);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->text); n++;
  XtSetArg(args[n], XmNarrowDirection, XmARROW_DOWN); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  prf->arrow_down = XtCreateManagedWidget("arrow-down", xmArrowButtonWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->arrow_down); n++;
  XtSetArg(args[n], XmNarrowDirection, XmARROW_UP); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
  prf->arrow_up = XtCreateManagedWidget("arrow-up", xmArrowButtonWidgetClass, box, args, n);
  
  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, top_widget); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNleftWidget, prf->arrow_up); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
  prf->error = XtCreateManagedWidget("", xmLabelWidgetClass, box, args, n);
  
  XtAddCallback(prf->arrow_down, XmNactivateCallback, size_down, (XtPointer)prf);
  XtAddCallback(prf->arrow_up, XmNactivateCallback, size_up, (XtPointer)prf);
  /* SOMEDAY: held button here => scrolling */
  XtAddCallback(prf->text, XmNactivateCallback, size_from_text, (XtPointer)prf);
  return(prf);
}

void start_preferences_dialog(void)
{
  Arg args[20];
  int n;
  XmString title, help, reset, save, dismiss;
  Widget reset_button, scroller, topics;

  title = XmStringCreate(_("Preferences"), XmFONTLIST_DEFAULT_TAG);
  help = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
  reset = XmStringCreate(_("Reset"), XmFONTLIST_DEFAULT_TAG);
  save = XmStringCreate(_("Save"), XmFONTLIST_DEFAULT_TAG);
  dismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG);

  n = 0;
  if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;}
  XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
  XtSetArg(args[n], XmNnoResize, false); n++;
  XtSetArg(args[n], XmNtransient, false); n++;
  XtSetArg(args[n], XmNcancelLabelString, save); n++;
  XtSetArg(args[n], XmNhelpLabelString, help); n++;
  XtSetArg(args[n], XmNokLabelString, dismiss); n++;
  XtSetArg(args[n], XmNdialogTitle, title); n++;
  XtSetArg(args[n], XmNallowShellResize, true); n++;
  XtSetArg(args[n], XmNautoUnmanage, false); n++;
  preferences_dialog = XmCreateTemplateDialog(MAIN_PANE(ss), "preferences", args, n);

  n = 0;
  if (!(ss->using_schemes)) 
    {
      XtSetArg(args[n], XmNbackground, ss->sgx->reset_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
    }
  reset_button = XtCreateManagedWidget(_("Reset"), xmPushButtonGadgetClass, preferences_dialog, args, n);

  XtAddCallback(preferences_dialog, XmNcancelCallback, preferences_save_callback, NULL);
  XtAddCallback(preferences_dialog, XmNhelpCallback, preferences_help_callback, NULL);
  XtAddCallback(preferences_dialog, XmNokCallback, preferences_quit_callback, NULL);
  XtAddCallback(reset_button, XmNactivateCallback, preferences_reset_callback, NULL);

  XmStringFree(title);
  XmStringFree(help);
  XmStringFree(save);
  XmStringFree(dismiss);
  XmStringFree(reset);

  if (!(ss->using_schemes))
    {
      map_over_children(preferences_dialog, set_main_color_of_widget, NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON),     XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_HELP_BUTTON),   XmNarmColor,   ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_button_color,   NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_OK_BUTTON),     XmNbackground, ss->sgx->quit_button_color,   NULL);
      XtVaSetValues(XmMessageBoxGetChild(preferences_dialog, XmDIALOG_HELP_BUTTON),   XmNbackground, ss->sgx->help_button_color,   NULL);
    }

  n = 0;
  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(preferences_dialog, XmDIALOG_SEPARATOR)); n++;
  scroller = XmCreateScrolledWindow(preferences_dialog, "pref-scroller", args, n);

  XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
  n = attach_all_sides(args, 0);
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  topics = XtCreateManagedWidget("pref-topics", xmRowColumnWidgetClass, scroller, args, n);
  XtVaSetValues(scroller,
		XmNworkWindow, topics, 
		NULL);

  
  {
    Widget fft_frame, fft_box, fft_label;
    char *size;
    prefs_info *size_prf = NULL;

    /*
     *  fft choices
     */

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    fft_frame = XtCreateManagedWidget("pref-fft-frame", xmFrameWidgetClass, topics, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->white); n++;
    fft_box = XtCreateManagedWidget("pref-fft", xmFormWidgetClass, fft_frame, args, n);

    n = 0;
    XtSetArg(args[n], XmNbackground, ss->sgx->light_blue); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
    fft_label = XtCreateManagedWidget("transforms", xmLabelWidgetClass, fft_box, args, n);

    size = mus_format(OFF_TD, transform_size(ss));
    size_prf = prefs_row_with_number("size:", size, fft_box, fft_label, fft_size_up, fft_size_down, fft_size_from_text);
    FREE(size);
  }
  
  
    /* need drop-down menu tied in size/placement exactly to the text widget with sizes, centered on current choice */
    /* different arrow style if menu-related? */
    /* maybe need a button + pixmap here ... */



  XtManageChild(scroller);
  XtManageChild(preferences_dialog);
  set_dialog_widget(PREFERENCES_DIALOG, preferences_dialog);

}

#if 0

  ss->Minibuffer_History_Length = DEFAULT_MINIBUFFER_HISTORY_LENGTH;
  ss->Fft_Window = DEFAULT_FFT_WINDOW;
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
  ss->Dot_Size = DEFAULT_DOT_SIZE;
  ss->Grid_Density = DEFAULT_GRID_DENSITY;
  ss->Cursor_Size = DEFAULT_CURSOR_SIZE;
  ss->Cursor_Style = DEFAULT_CURSOR_STYLE;
  ss->cursor_proc = XEN_UNDEFINED;
  ss->cursor_proc_loc = NOT_A_GC_LOC;
  ss->Vu_Size = DEFAULT_VU_SIZE;
  ss->Vu_Font_Size = DEFAULT_VU_FONT_SIZE;
  if (DEFAULT_VU_FONT != (char *)NULL) ss->Vu_Font = copy_string(DEFAULT_VU_FONT); else ss->Vu_Font = NULL;
  ss->Selection_Creates_Region = DEFAULT_SELECTION_CREATES_REGION;
  ss->Transform_Normalization = DEFAULT_TRANSFORM_NORMALIZATION;
  ss->Zoom_Focus_Style = DEFAULT_ZOOM_FOCUS_STYLE;
  ss->zoom_focus_proc = XEN_UNDEFINED;
  ss->zoom_focus_proc_loc = NOT_A_GC_LOC;
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
  ss->Show_Sonogram_Cursor = DEFAULT_SHOW_SONOGRAM_CURSOR;
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
  ss->HTML_Dir = NULL;
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
  ss->Cursor_Follows_Play = DEFAULT_CURSOR_FOLLOWS_PLAY;
  ss->Just_Sounds = DEFAULT_JUST_SOUNDS;
#endif
