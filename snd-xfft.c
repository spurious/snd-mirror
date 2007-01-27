/* Transform settings dialog */

#include "snd.h"

static Widget transform_dialog = NULL; /* main dialog shell */
static Widget type_list, size_list, wavelet_list, window_list, window_beta_scale, window_alpha_scale;
static Widget db_button, peaks_button, logfreq_button, sono_button, spectro_button, normo_button, normalize_button, selection_button;
static Widget graph_label, graph_drawer;
static Widget peak_txt, db_txt, freq_base_txt;
static Widget error_frame, error_label;

static GC gc, fgc;

#define GRAPH_SIZE 128
static Float current_graph_data[GRAPH_SIZE]; /* fft window graph in transform options dialog */
static Float current_graph_fftr[GRAPH_SIZE * 2];
static Float current_graph_ffti[GRAPH_SIZE * 2];

#define NUM_TRANSFORM_SIZES 14
static char *TRANSFORM_SIZES[NUM_TRANSFORM_SIZES] = 
  {"32", "64", "128", "256", "512", "1024", "2048", "4096", "8192", "16384", "65536", "262144", "1048576", "4194304    "};
static int transform_sizes[NUM_TRANSFORM_SIZES] = 
  {32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 65536, 262144, 1048576, 4194304};

static char *FFT_WINDOWS[MUS_NUM_WINDOWS] = 
  {"Rectangular", "Hann", "Welch", "Parzen", "Bartlett", "Hamming", "Blackman2", "Blackman3", "Blackman4",
   "Exponential", "Riemann", "Kaiser", "Cauchy", "Poisson", "Gaussian", "Tukey", "Dolph-Chebyshev", "Hann-Poisson", "Connes",
   "Samaraki", "Ultraspherical"};

static Float fp_dB(Float py)
{
  return((py <= ss->lin_dB) ? 0.0 : (1.0 - (20.0 * log10(py) / min_dB(ss))));
}

static Locus local_grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((Locus)(ap->x_base + val * ap->x_scale));
}

static Locus local_grf_y(Float val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((Locus)(ap->y_base + val * ap->y_scale));
}

static axis_info *axis_ap = NULL;

static void graph_redisplay(void)
{
  int ix0, iy0, ix1, iy1, i;
  Float xincr, x;
  axis_context *ax;
  if (axis_ap == NULL) 
    {
      axis_ap = (axis_info *)CALLOC(1, sizeof(axis_info));
      ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      axis_ap->ax = ax;
      ax->dp = XtDisplay(graph_drawer);
      ax->wn = XtWindow(graph_drawer);
    }
  else 
    {
      if (axis_ap->xlabel) FREE(axis_ap->xlabel);
      ax = axis_ap->ax;
    }
  axis_ap->xmin = 0.0;
  axis_ap->xmax = 1.0;
  axis_ap->ymin = 0.0;
  axis_ap->ymax = 1.0;
  axis_ap->y_ambit = 1.0;
  axis_ap->x_ambit = 1.0;
  axis_ap->xlabel = NULL;
  axis_ap->x0 = 0.0;
  axis_ap->x1 = 1.0;
  axis_ap->y0 = 0.0;
  axis_ap->y1 = 1.0;
  axis_ap->width = widget_width(graph_drawer);
  axis_ap->window_width = axis_ap->width;
  axis_ap->y_offset = 0;
  axis_ap->height = widget_height(graph_drawer);
  axis_ap->graph_x0 = 0;
  clear_window(ax);
  ax->gc = gc;
  make_axes_1(axis_ap, X_AXIS_IN_SECONDS, 1 /* "srate" */, SHOW_ALL_AXES, NOT_PRINTING, WITH_X_AXIS, NO_GRID, WITH_LINEAR_AXES, grid_density(ss));
  ix1 = local_grf_x(0.0, axis_ap);
  iy1 = local_grf_y(current_graph_data[0], axis_ap);
  xincr = 1.0 / (Float)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x += xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = local_grf_x(x, axis_ap);
      iy1 = local_grf_y(current_graph_data[i], axis_ap);
      XDrawLine(ax->dp, ax->wn, gc, ix0, iy0, ix1, iy1);
    }

  ax->gc = fgc;
  ix1 = local_grf_x(0.0, axis_ap);
  iy1 = local_grf_y(current_graph_fftr[0], axis_ap);
  xincr = 1.0 / (Float)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x += xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = local_grf_x(x, axis_ap);
      if (fft_log_magnitude(ss))
	iy1 = local_grf_y(fp_dB(current_graph_fftr[i]), axis_ap);
      else iy1 = local_grf_y(current_graph_fftr[i], axis_ap);
      XDrawLine(ax->dp, ax->wn, fgc, ix0, iy0, ix1, iy1);
    }
}

static void get_fft_window_data(void)
{
  int i;
  mus_make_fft_window_with_window(fft_window(ss), GRAPH_SIZE, 
				  fft_window_beta(ss) * fft_beta_max(fft_window(ss)), 
				  fft_window_alpha(ss), current_graph_data);
  memset((void *)current_graph_fftr, 0, GRAPH_SIZE * 2 * sizeof(Float));
  memset((void *)current_graph_ffti, 0, GRAPH_SIZE * 2 * sizeof(Float));
  memcpy((void *)current_graph_fftr, (void *)current_graph_data, GRAPH_SIZE * sizeof(Float));
  mus_spectrum(current_graph_fftr, current_graph_ffti, NULL, GRAPH_SIZE * 2, 0);
  for (i = 0; i < GRAPH_SIZE; i++)
    current_graph_fftr[i] = (current_graph_fftr[i] + 80.0) / 80.0;
}

static void chans_transform_size(chan_info *cp, int size)
{
  cp->transform_size = size;
  if (cp->fft) 
    cp->fft->size = size;
}

static void size_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  int size;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  for_each_chan(force_fft_clear);
  in_set_transform_size(transform_sizes[cbs->item_position - 1]);
  size = transform_size(ss);
  for_each_chan_with_int(chans_transform_size, size);
  for_each_chan(calculate_fft);
  set_label(graph_label, FFT_WINDOWS[(int)fft_window(ss)]);
  get_fft_window_data();
  graph_redisplay();
}

static void chans_wavelet_type(chan_info *cp, int value)
{
  cp->wavelet_type = value;
}

static void wavelet_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  int val;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  in_set_wavelet_type(val = (cbs->item_position - 1)); /* make these numbers 0-based as in mus.lisp */
  for_each_chan_with_int(chans_wavelet_type, val);
  if (transform_type(ss) == WAVELET)
    for_each_chan(calculate_fft);
}


static void window_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  mus_fft_window_t fft_window_choice;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  fft_window_choice = (mus_fft_window_t)(cbs->item_position - 1); /* make these numbers 0-based as in mus.lisp */
  in_set_fft_window(fft_window_choice);
  for_each_chan(calculate_fft);
  set_label(graph_label, FFT_WINDOWS[(int)fft_window(ss)]);
  get_fft_window_data();
  graph_redisplay();
  if (fft_window_beta_in_use(fft_window(ss)))
    XtVaSetValues(window_beta_scale, XmNbackground, ss->sgx->highlight_color, NULL);
  else XtVaSetValues(window_beta_scale, XmNbackground, ss->sgx->basic_color, NULL);
  if (fft_window_alpha_in_use(fft_window(ss)))
    XtVaSetValues(window_alpha_scale, XmNbackground, ss->sgx->highlight_color, NULL);
  else XtVaSetValues(window_alpha_scale, XmNbackground, ss->sgx->basic_color, NULL);
}

static void chans_transform_type(chan_info *cp, int value) 
{
  cp->transform_type = value;
}

static void transform_type_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  int type;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  type = transform_position_to_type(cbs->item_position - 1);
  for_each_chan(force_fft_clear);
  in_set_transform_type(type);
  for_each_chan_with_int(chans_transform_type, type);
  for_each_chan(calculate_fft);
}


static void graph_transform_once_callback(Widget w, XtPointer context, XtPointer info)
{
  graph_type_t old_type;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  old_type = transform_graph_type(ss);
  XmToggleButtonSetState(normo_button, true, false);
  XmToggleButtonSetState(sono_button, false, false);
  XmToggleButtonSetState(spectro_button, false, false);
  in_set_transform_graph_type(GRAPH_ONCE);
  if (old_type != GRAPH_ONCE)
    for_each_chan(calculate_fft);
}

static void sonogram_callback(Widget w, XtPointer context, XtPointer info)
{
  graph_type_t old_type;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  old_type = transform_graph_type(ss);
  XmToggleButtonSetState(sono_button, true, false);
  XmToggleButtonSetState(normo_button, false, false);
  XmToggleButtonSetState(spectro_button, false, false);
  in_set_transform_graph_type(GRAPH_AS_SONOGRAM);
  if (old_type != GRAPH_AS_SONOGRAM)
    for_each_chan(calculate_fft);
}

static void spectrogram_callback(Widget w, XtPointer context, XtPointer info)
{
  graph_type_t old_type;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  old_type = transform_graph_type(ss);
  XmToggleButtonSetState(spectro_button, true, false);
  XmToggleButtonSetState(normo_button, false, false);
  XmToggleButtonSetState(sono_button, false, false);
  in_set_transform_graph_type(GRAPH_AS_SPECTROGRAM);
  if (old_type != GRAPH_AS_SPECTROGRAM)
    for_each_chan(calculate_fft);
}

static void map_show_transform_peaks(chan_info *cp, bool value) 
{
  cp->show_transform_peaks = value;
}

static void peaks_callback(Widget w, XtPointer context, XtPointer info)
{
  bool val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  val = (cb->set);
  in_set_show_transform_peaks(val);
  for_each_chan_with_bool(map_show_transform_peaks, val);
  for_each_chan(calculate_fft);
}

static void chans_fft_log_magnitude(chan_info *cp, bool value)
{
  cp->fft_log_magnitude = value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void db_callback(Widget w, XtPointer context, XtPointer info)
{
  bool val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  val = cb->set;
  in_set_fft_log_magnitude(val);
  graph_redisplay();
  for_each_chan_with_bool(chans_fft_log_magnitude, val);
  for_each_chan(calculate_fft);
}

static void chans_fft_log_frequency(chan_info *cp, bool value)
{
  cp->fft_log_frequency = value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void logfreq_callback(Widget w, XtPointer context, XtPointer info)
{
  bool val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  val = cb->set;
  in_set_fft_log_frequency(val);
  for_each_chan_with_bool(chans_fft_log_frequency, val);
  for_each_chan(calculate_fft);
}

static void chans_transform_normalization(chan_info *cp, int value)
{
  cp->transform_normalization = (fft_normalize_t)value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void normalize_callback(Widget w, XtPointer context, XtPointer info)
{
  fft_normalize_t choice;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  choice = (cb->set) ? NORMALIZE_BY_CHANNEL : DONT_NORMALIZE;
  in_set_transform_normalization(choice);
  for_each_chan_with_int(chans_transform_normalization, (int)choice);
  for_each_chan(calculate_fft);
}

static void selection_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  in_set_show_selection_transform(cb->set);
  for_each_chan(calculate_fft);
}


static void beta_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  in_set_fft_window_beta((Float)(cb->value) / 100.0);
  chans_field(FCP_BETA, (Float)(cb->value) / 100.0);
  if (fft_window_beta_in_use(fft_window(ss)))
    {
      get_fft_window_data();
      graph_redisplay();
      if (transform_type(ss) == FOURIER) 
	for_each_chan(calculate_fft);
    }
} 

#define FFT_WINDOW_ALPHA_SCALER 100.0

static void alpha_callback(Widget w, XtPointer context, XtPointer info)
{
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  in_set_fft_window_alpha((Float)(cb->value) / FFT_WINDOW_ALPHA_SCALER);
  chans_field(FCP_ALPHA, (Float)(cb->value) / FFT_WINDOW_ALPHA_SCALER);
  if (fft_window_alpha_in_use(fft_window(ss)))
    {
      get_fft_window_data();
      graph_redisplay();
      if (transform_type(ss) == FOURIER) 
	for_each_chan(calculate_fft);
    }
} 

static void graph_resize_callback(Widget w, XtPointer context, XtPointer info)
{
  graph_redisplay();
}

static void dismiss_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  Widget active_widget;
  active_widget = XmGetFocusWidget(transform_dialog);
  if (active_widget == XmMessageBoxGetChild(transform_dialog, XmDIALOG_OK_BUTTON))
    XtUnmanageChild(transform_dialog);
}

static void orient_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  start_orientation_dialog(true);
}

static void color_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  start_color_dialog(true);
}

static void help_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  transform_dialog_help();
}

static void blue_textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info)
{
  XtVaSetValues(w, XmNbackground, ss->sgx->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}

static void blue_mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  XtVaSetValues(w, XmNbackground, ss->sgx->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}

static void white_mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  XtVaSetValues(w, XmNbackground, ss->sgx->text_focus_color, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, true, NULL);
}


static void clear_fft_error(void)
{
  if ((error_frame) && (XtIsManaged(error_frame)))
    XtUnmanageChild(error_frame);
}

static void unpost_fft_error(XtPointer data, XtIntervalId *id)
{
  clear_fft_error();
}

static void errors_to_fft_text(const char *msg, void *data)
{
  int lines = 0;
  XmString label;
  label = multi_line_label(msg, &lines);
  XtVaSetValues(error_label, 
		XmNlabelString, label, 
		XmNheight, lines * 20,
		NULL);
  XtVaSetValues(error_frame, XmNheight, lines * 20, NULL);
  XmStringFree(label);
  XtManageChild(error_frame);
  /* since the offending text is automatically overwritten, we can't depend on subsequent text modify callbacks
   *   to clear things, so we'll just use a timer
   */
  XtAppAddTimeOut(MAIN_APP(ss),
		  5000,
		  (XtTimerCallbackProc)unpost_fft_error,
		  NULL);
}


void reflect_peaks_in_transform_dialog(void)
{
  if (transform_dialog)
    widget_int_to_text(peak_txt, max_transform_peaks(ss));
}

static void peaks_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  char *str;
  str = XmTextFieldGetString(w);
  if ((str) && (*str))
    {
      int new_peaks;
      redirect_errors_to(errors_to_fft_text, NULL);
      new_peaks = string_to_int(str, 1, "peaks");
      redirect_errors_to(NULL, NULL);
      if (new_peaks >= 1)
	{
	  set_max_transform_peaks(new_peaks);
	  for_each_chan(calculate_fft);
	}
      else widget_int_to_text(w, max_transform_peaks(ss));
      XtFree(str);
    }
}

void reflect_log_freq_start_in_transform_dialog(void)
{
  if (transform_dialog)
    widget_float_to_text(freq_base_txt, log_freq_start(ss));
}

static void log_freq_start_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  char *str;
  str = XmTextFieldGetString(w);
  if ((str) && (*str))
    {
      Float new_lfb;
      redirect_errors_to(errors_to_fft_text, NULL);
      new_lfb = string_to_Float(str, 0.0, "log freq start");
      redirect_errors_to(NULL, NULL);
      if (new_lfb > 0.0)
	set_log_freq_start(new_lfb);
      else widget_float_to_text(w, log_freq_start(ss));
      XtFree(str);
    }
}

void reflect_min_db_in_transform_dialog(void)
{
  if (transform_dialog)
    widget_float_to_text(db_txt, min_dB(ss));
}

static void min_db_activate_callback(Widget w, XtPointer context, XtPointer info)
{
  char *str;
  str = XmTextFieldGetString(w);
  if ((str) && (*str))
    {
      Float new_db;
      redirect_errors_to(errors_to_fft_text, NULL);
      new_db = string_to_Float(str, -10000.0, "dB");
      redirect_errors_to(NULL, NULL);
      if (new_db < 0.0)
	set_min_db(new_db);
      else widget_float_to_text(w, min_dB(ss));
      XtFree(str);
    }
}

static bool need_callback = true;
Widget fire_up_transform_dialog(bool managed)
{
  Widget mainform, type_frame, type_form, type_label, size_frame, size_form, size_label, display_frame, display_form, display_label;
  Widget window_frame, window_form, window_label, wavelet_frame, wavelet_form, wavelet_label, graph_frame, graph_form, color_button;
    
  if (!transform_dialog)
    {
      XmString xhelp, xdismiss, xtitle, bstr, xorient;
      Arg args[32];
      XmString sizes[NUM_TRANSFORM_SIZES];
      XmString wavelets[NUM_WAVELETS];
      XmString windows[MUS_NUM_WINDOWS];
      XGCValues gv;
      XtCallbackList n1, n2, n3, n4;
      int size_pos = 1;
      int n, i;

      for (i = 0; i < NUM_TRANSFORM_SIZES; i++)
	if (transform_sizes[i] == transform_size(ss))
	  {
	    size_pos = i + 1;
	    break;
	  }
      xdismiss = XmStringCreateLocalized(_("Dismiss")); /* needed by template dialog */
      xhelp = XmStringCreateLocalized(_("Help"));
      xtitle = XmStringCreateLocalized(_("Transform Options"));
      xorient = XmStringCreateLocalized(_("Orientation"));

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xorient); n++;
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      transform_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Transform Options"), args, n);

      XtAddCallback(transform_dialog, XmNcancelCallback, orient_transform_callback, NULL);
      XtAddCallback(transform_dialog, XmNokCallback, dismiss_transform_callback, NULL);
      XtAddCallback(transform_dialog, XmNhelpCallback, help_transform_callback, NULL);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xtitle);
      XmStringFree(xorient);

      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->sgx->pushed_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->sgx->doit_again_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->sgx->help_button_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->sgx->quit_button_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->doit_button_color); n++;
      XtSetArg(args[n], XmNarmColor, ss->sgx->pushed_button_color); n++;
      color_button = XtCreateManagedWidget(_("Color"), xmPushButtonGadgetClass, transform_dialog, args, n);
      XtAddCallback(color_button, XmNactivateCallback, color_transform_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(transform_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("mainform", xmFormWidgetClass, transform_dialog, args, n);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      error_frame = XtCreateManagedWidget("error-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      error_label = XtCreateManagedWidget("", xmLabelWidgetClass, error_frame, args, n);


      /* now 6 boxes within the main box:
	 
	 type (list)    |  size (list)        |  display (button column)
	 wavelet (list) |  window (list+beta) |  graph (fft?) of current window
	 
	 each box has a frame, label, and contents
      */

      /* TYPE */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 30); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNbottomPosition, 50); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      type_frame = XtCreateManagedWidget("type-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      n = attach_all_sides(args, n);
      type_form = XtCreateManagedWidget("type-form", xmFormWidgetClass, type_frame, args, n);
      /* needed because XmFrame only accepts one child */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      type_label = XtCreateManagedWidget(_("type"), xmLabelWidgetClass, type_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, type_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      type_list = XmCreateScrolledList(type_form, "type-list", args, n);
      XtVaSetValues(type_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      make_transform_type_list();
      XtManageChild(type_list); 
      XtAddCallback(type_list, XmNbrowseSelectionCallback, transform_type_browse_callback, NULL);

      /* SIZE */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, type_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 60); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, type_frame); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      size_frame = XtCreateManagedWidget("size-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      n = attach_all_sides(args, n);
      size_form = XtCreateManagedWidget("size-form", xmFormWidgetClass, size_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      size_label = XtCreateManagedWidget(_("size"), xmLabelWidgetClass, size_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, size_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopItemPosition, (size_pos > 2) ? (size_pos - 2) : size_pos); n++;
      size_list = XmCreateScrolledList(size_form, "size-list", args, n);
      XtVaSetValues(size_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++) 
	sizes[i] = XmStringCreateLocalized(TRANSFORM_SIZES[i]);
      XtVaSetValues(size_list, 
		    XmNitems, sizes, 
		    XmNitemCount, NUM_TRANSFORM_SIZES, 
		    XmNvisibleItemCount, 6, 
		    NULL);
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++) 
	XmStringFree(sizes[i]);
      XtManageChild(size_list); 
      XtAddCallback(size_list, XmNbrowseSelectionCallback, size_browse_callback, NULL);


      /* DISPLAY */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, size_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      display_frame = XtCreateManagedWidget("display-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->zoom_color); n++;
      n = attach_all_sides(args, n);
      display_form = XtCreateManagedWidget("display-form", xmFormWidgetClass, display_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      display_label = XtCreateManagedWidget(_("display"), xmLabelWidgetClass, display_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("single transform"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, display_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      normo_button = make_togglebutton_widget("normo-button", display_form, args, n);
      XtAddCallback(normo_button, XmNdisarmCallback, graph_transform_once_callback, NULL);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("sonogram"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, normo_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      sono_button = make_togglebutton_widget("sono-button", display_form, args, n);
      XtAddCallback(sono_button, XmNdisarmCallback, sonogram_callback, NULL);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("spectrogram"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sono_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      spectro_button = make_togglebutton_widget("spectro-button", display_form, args, n);
      XtAddCallback(spectro_button, XmNdisarmCallback, spectrogram_callback, NULL);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("peaks"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 67); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, spectro_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      peaks_button = make_togglebutton_widget("peaks-button", display_form, args, n);
      XtAddCallback(peaks_button, XmNvalueChangedCallback, peaks_callback, NULL);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, 6); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      /* XtSetArg(args[n], XmNmarginHeight, 1); n++; */
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, peaks_button); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, peaks_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, peaks_button); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      peak_txt = make_textfield_widget("max-peaks", display_form, args, n, ACTIVATABLE, NO_COMPLETER);
      XtRemoveCallback(peak_txt, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
      XtAddCallback(peak_txt, XmNlosingFocusCallback, blue_textfield_unfocus_callback, NULL);
      XtAddEventHandler(peak_txt, LeaveWindowMask, false, blue_mouse_leave_text_callback, NULL);
      XtAddEventHandler(peak_txt, EnterWindowMask, false, white_mouse_enter_text_callback, NULL);
      widget_int_to_text(peak_txt, max_transform_peaks(ss));
      XtAddCallback(peak_txt, XmNactivateCallback, peaks_activate_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("dB"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 67); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, peaks_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      db_button = make_togglebutton_widget("db-button", display_form, args, n);
      XtAddCallback(db_button, XmNvalueChangedCallback, db_callback, NULL);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, 6); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      /* XtSetArg(args[n], XmNmarginHeight, 1); n++; */
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, db_button); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, db_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, db_button); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      db_txt = make_textfield_widget("db", display_form, args, n, ACTIVATABLE, NO_COMPLETER);
      XtRemoveCallback(db_txt, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
      XtAddCallback(db_txt, XmNlosingFocusCallback, blue_textfield_unfocus_callback, NULL);
      XtAddEventHandler(db_txt, LeaveWindowMask, false, blue_mouse_leave_text_callback, NULL);
      XtAddEventHandler(db_txt, EnterWindowMask, false, white_mouse_enter_text_callback, NULL);
      widget_float_to_text(db_txt, min_dB(ss));
      XtAddCallback(db_txt, XmNactivateCallback, min_db_activate_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("log freq"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 67); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, db_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      logfreq_button = make_togglebutton_widget("logfreq-button", display_form, args, n);
      XtAddCallback(logfreq_button, XmNvalueChangedCallback, logfreq_callback, NULL);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNresizeWidth, false); n++;
      XtSetArg(args[n], XmNcolumns, 6); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      /* XtSetArg(args[n], XmNmarginHeight, 1); n++; */
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, logfreq_button); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, logfreq_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, logfreq_button); n++;
      XtSetArg(args[n], XmNborderWidth, 0); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      freq_base_txt = make_textfield_widget("lfb", display_form, args, n, ACTIVATABLE, NO_COMPLETER);
      XtRemoveCallback(freq_base_txt, XmNlosingFocusCallback, textfield_unfocus_callback, NULL);
      XtAddCallback(freq_base_txt, XmNlosingFocusCallback, blue_textfield_unfocus_callback, NULL);
      XtAddEventHandler(freq_base_txt, LeaveWindowMask, false, blue_mouse_leave_text_callback, NULL);
      XtAddEventHandler(freq_base_txt, EnterWindowMask, false, white_mouse_enter_text_callback, NULL);
      widget_float_to_text(freq_base_txt, log_freq_start(ss));
      XtAddCallback(freq_base_txt, XmNactivateCallback, log_freq_start_activate_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("normalize"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, logfreq_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      normalize_button = make_togglebutton_widget("normalize-button", display_form, args, n);
      XtAddCallback(normalize_button, XmNvalueChangedCallback, normalize_callback, NULL);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->sgx->red); n++;
      bstr = XmStringCreateLocalized(_("selection"));
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, normalize_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      selection_button = make_togglebutton_widget("selection-button", display_form, args, n);
      XtAddCallback(selection_button, XmNvalueChangedCallback, selection_callback, NULL);
      XmStringFree(bstr);


      
      /* WAVELET */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, type_frame); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, type_frame); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      wavelet_frame = XtCreateManagedWidget("wavelet-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      n = attach_all_sides(args, n);
      wavelet_form = XtCreateManagedWidget("wavelet-form", xmFormWidgetClass, wavelet_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      wavelet_label = XtCreateManagedWidget(_("wavelet"), xmLabelWidgetClass, wavelet_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, wavelet_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      wavelet_list = XmCreateScrolledList(wavelet_form, "wavelet-list", args, n);
      XtVaSetValues(wavelet_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      for (i = 0; i < NUM_WAVELETS; i++) 
	wavelets[i] = XmStringCreateLocalized(wavelet_name(i));
      XtVaSetValues(wavelet_list, 
		    XmNitems, wavelets, 
		    XmNitemCount, NUM_WAVELETS, 
		    XmNvisibleItemCount, 5, 
		    NULL);
      for (i = 0; i < NUM_WAVELETS; i++) 
	XmStringFree(wavelets[i]);
      XtManageChild(wavelet_list); 
      XtAddCallback(wavelet_list, XmNbrowseSelectionCallback, wavelet_browse_callback, NULL);


      /* WINDOW */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, wavelet_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, size_frame); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, size_frame); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      window_frame = XtCreateManagedWidget("window-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      n = attach_all_sides(args, n);
      window_form = XtCreateManagedWidget("window-form", xmFormWidgetClass, window_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshowValue, XmNEAR_SLIDER); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNmaximum, 1000); n++;
      XtSetArg(args[n], XmNvalue, (int)(FFT_WINDOW_ALPHA_SCALER * fft_window_alpha(ss))); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(alpha_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(alpha_callback, NULL)); n++;
      window_alpha_scale = XtCreateManagedWidget("alpha-scale", xmScaleWidgetClass, window_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, window_alpha_scale); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshowValue, XmNEAR_SLIDER); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNvalue, 100 * fft_window_beta(ss)); n++;
      XtSetArg(args[n], XmNdragCallback, n3 = make_callback_list(beta_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n4 = make_callback_list(beta_callback, NULL)); n++;
      window_beta_scale = XtCreateManagedWidget("beta-scale", xmScaleWidgetClass, window_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      window_label = XtCreateManagedWidget(_("window"), xmLabelWidgetClass, window_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, window_beta_scale); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, window_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopItemPosition, ((int)fft_window(ss) > 2) ? ((int)fft_window(ss) - 1) : ((int)fft_window(ss) + 1)); n++;
      window_list = XmCreateScrolledList(window_form, "window-list", args, n);
      XtVaSetValues(window_list, XmNbackground, ss->sgx->white, XmNforeground, ss->sgx->black, NULL);
      for (i = 0; i < MUS_NUM_WINDOWS; i++)
	windows[i] = XmStringCreateLocalized(FFT_WINDOWS[i]);

      XtVaSetValues(window_list, 
		    XmNitems, windows, 
		    XmNitemCount, MUS_NUM_WINDOWS, 
		    XmNvisibleItemCount, 5, 
		    NULL);
      for (i = 0; i < MUS_NUM_WINDOWS; i++) 
	XmStringFree(windows[i]);
      XtManageChild(window_list); 
      XtAddCallback(window_list, XmNbrowseSelectionCallback, window_browse_callback, NULL);


      /* GRAPH */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, window_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, display_frame); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      XtSetArg(args[n], XmNborderColor, ss->sgx->basic_color); n++;
      graph_frame = XtCreateManagedWidget("graph-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->basic_color); n++;
      n = attach_all_sides(args, n);
      graph_form = XtCreateManagedWidget("graph-form", xmFormWidgetClass, graph_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      graph_label = XtCreateManagedWidget(_("window"), xmLabelWidgetClass, graph_form, args, n);
      /* label should change according to what is being displayed */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->sgx->graph_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, graph_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      graph_drawer = XtCreateManagedWidget("graph-drawer", xmDrawingAreaWidgetClass, graph_form, args, n);


      gv.function = GXcopy;
      XtVaGetValues(graph_drawer, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
      gc = XtGetGC(graph_drawer, GCForeground | GCFunction, &gv);

      gv.foreground = ss->sgx->enved_waveform_color;
      fgc = XtGetGC(graph_drawer, GCForeground | GCFunction, &gv);

      XmToggleButtonSetState(normo_button, (Boolean)(transform_graph_type(ss) == GRAPH_ONCE), false);
      XmToggleButtonSetState(sono_button, (Boolean)(transform_graph_type(ss) == GRAPH_AS_SONOGRAM), false);
      XmToggleButtonSetState(spectro_button, (Boolean)(transform_graph_type(ss) == GRAPH_AS_SPECTROGRAM), false);
      XmToggleButtonSetState(peaks_button, (Boolean)(show_transform_peaks(ss)), false);
      XmToggleButtonSetState(db_button, (Boolean)(fft_log_magnitude(ss)), false);
      XmToggleButtonSetState(logfreq_button, (Boolean)(fft_log_frequency(ss)), false);
      XmToggleButtonSetState(normalize_button, (Boolean)(transform_normalization(ss) != DONT_NORMALIZE), false);
      XmToggleButtonSetState(selection_button, (Boolean)(show_selection_transform(ss)), false);

      /* select current list choices */
      /* display current windowing choice unless wavelet in force */

      XmListSelectPos(type_list, transform_type_to_position(transform_type(ss)) + 1, false);
      XmListSelectPos(wavelet_list, wavelet_type(ss) + 1, false);
      XmListSelectPos(size_list, size_pos, false);
      XmListSelectPos(window_list, (int)fft_window(ss) + 1, false);

      if (fft_window_beta_in_use(fft_window(ss))) 
	XtVaSetValues(window_beta_scale, XmNbackground, ss->sgx->highlight_color, NULL);
      if (fft_window_alpha_in_use(fft_window(ss))) 
	XtVaSetValues(window_alpha_scale, XmNbackground, ss->sgx->highlight_color, NULL);

      FREE(n1);
      FREE(n2);
      FREE(n3);
      FREE(n4);
      set_dialog_widget(TRANSFORM_DIALOG, transform_dialog);

      XtUnmanageChild(error_frame);
    }
  else
    {
      if (managed)
	raise_dialog(transform_dialog);
    }
  if (managed)
    {
      if (!XtIsManaged(transform_dialog)) 
	XtManageChild(transform_dialog);
    }
  else XtUnmanageChild(transform_dialog);
  if ((need_callback) && (XtIsManaged(transform_dialog)))
    {
      set_label(graph_label, FFT_WINDOWS[(int)fft_window(ss)]);
      get_fft_window_data();
      XtAddCallback(graph_drawer, XmNresizeCallback, graph_resize_callback, NULL);
      XtAddCallback(graph_drawer, XmNexposeCallback, graph_resize_callback, NULL);
      need_callback = false;
    }
  return(transform_dialog);
}

bool transform_dialog_is_active(void)
{
  return((transform_dialog) && (XtIsManaged(transform_dialog)));
}

/* various set- cases need to be reflected in the transform dialog */
void set_fft_window_beta(Float val)
{
  in_set_fft_window_beta(val);
  chans_field(FCP_BETA, val);
  if (transform_dialog) 
    {
      XmScaleSetValue(window_beta_scale, (int)(100 * val));
      get_fft_window_data();
      graph_redisplay();
    }
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_fft_window_alpha(Float val)
{
  in_set_fft_window_alpha(val);
  chans_field(FCP_ALPHA, val);
  if (transform_dialog) 
    {
      XmScaleSetValue(window_alpha_scale, (int)(FFT_WINDOW_ALPHA_SCALER * val));
      get_fft_window_data();
      graph_redisplay();
    }
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_transform_graph_type(graph_type_t val)
{
  in_set_transform_graph_type(val);
  if (transform_dialog)
    switch (val)
      {
      case GRAPH_ONCE:
	XmToggleButtonSetState(normo_button, true, false);
	XmToggleButtonSetState(spectro_button, false, false);
	XmToggleButtonSetState(sono_button, false, false);
	break;
      case GRAPH_AS_SONOGRAM:
	XmToggleButtonSetState(normo_button, false, false);
	XmToggleButtonSetState(spectro_button, false, false);
	XmToggleButtonSetState(sono_button, true, false);
	break;
      case GRAPH_AS_SPECTROGRAM:
	XmToggleButtonSetState(normo_button, false, false);
	XmToggleButtonSetState(spectro_button, true, false);
	XmToggleButtonSetState(sono_button, false, false);
	break;
      case GRAPH_AS_WAVOGRAM:
	break;
      }
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_transform_size(int val)
{
  for_each_chan(force_fft_clear);
  in_set_transform_size(val);
  for_each_chan_with_int(chans_transform_size, val);
  if (transform_dialog)
    {
      int i;
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++)
	if (transform_sizes[i] == val)
	  {
	    XmListSelectPos(size_list, i + 1, false);
	    break;
	  }
    }
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
}

void set_fft_window(mus_fft_window_t val)
{
  in_set_fft_window(val);
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
  if (transform_dialog)
    {
      XmListSelectPos(window_list, (int)val + 1, false);
      set_label(graph_label, FFT_WINDOWS[val]);
      get_fft_window_data();
      graph_redisplay();
      if (fft_window_beta_in_use(val))
	XtVaSetValues(window_beta_scale, XmNbackground, ss->sgx->highlight_color, NULL);
      else XtVaSetValues(window_beta_scale, XmNbackground, ss->sgx->basic_color, NULL);
      if (fft_window_alpha_in_use(val))
	XtVaSetValues(window_alpha_scale, XmNbackground, ss->sgx->highlight_color, NULL);
      else XtVaSetValues(window_alpha_scale, XmNbackground, ss->sgx->basic_color, NULL);
    }
}
  
void set_transform_type(int val)
{
  if (transform_p(val))
    {
      if (!(ss->graph_hook_active)) for_each_chan(force_fft_clear);
      in_set_transform_type(val);
      for_each_chan_with_int(chans_transform_type, val);
      if (!(ss->graph_hook_active)) 
	for_each_chan(calculate_fft);
      if (transform_dialog) XmListSelectPos(type_list, transform_type_to_position(val) + 1, false);
    }
}

void set_wavelet_type(int val)
{
  if (transform_dialog) XmListSelectPos(wavelet_list, val, false);
  in_set_wavelet_type(val);
  for_each_chan_with_int(chans_wavelet_type, val);
  if ((transform_type(ss) == WAVELET) && 
      (!(ss->graph_hook_active))) 
    for_each_chan(calculate_fft);
}

/* various set- cases need to be reflected in the transform dialog */
void set_show_transform_peaks(bool val)
{
  in_set_show_transform_peaks(val);
  for_each_chan_with_bool(map_show_transform_peaks, val);
  if (transform_dialog) 
    set_toggle_button(peaks_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_fft_log_frequency(bool val)
{
  in_set_fft_log_frequency(val);
  for_each_chan_with_bool(chans_fft_log_frequency, val);
  if (transform_dialog)
    set_toggle_button(logfreq_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_fft_log_magnitude(bool val)
{
  in_set_fft_log_magnitude(val);
  for_each_chan_with_bool(chans_fft_log_magnitude, val);
  if (transform_dialog) 
    set_toggle_button(db_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_transform_normalization(fft_normalize_t val)
{
  in_set_transform_normalization(val);
  for_each_chan_with_int(chans_transform_normalization, (int)val);
  if (transform_dialog) 
    set_toggle_button(normalize_button, (val != DONT_NORMALIZE), false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_show_selection_transform(bool show)
{
  in_set_show_selection_transform(show);
  if (transform_dialog)
    set_toggle_button(selection_button, show, false, NULL); 
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void make_transform_type_list(void)
{
  int num;
  num = max_transform_type();
  if (transform_dialog)
    {
      XmString *types;
      int i, j;
      types = (XmString *)CALLOC(num, sizeof(XmString));
      for (i = 0, j = 0; i < num; i++) 
	if (transform_p(i))
	  {
	    set_transform_position(i, j);
	    types[j++] = XmStringCreateLocalized(transform_name(i)); 
	  }
      XtVaSetValues(type_list, 
		    XmNitems, types, 
		    XmNitemCount, j,
		    XmNvisibleItemCount, 6, 
		    NULL);
      for (i = 0; i < j; i++) 
	XmStringFree(types[i]);
      FREE(types);
    }
}

