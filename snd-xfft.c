/* Transform settings dialog */

#include "snd.h"

static Widget transform_dialog = NULL; /* main dialog shell */
static Widget type_list, size_list, wavelet_list, window_list, window_beta_scale,
              db_button, peaks_button, logfreq_button, sono_button, spectro_button, normo_button, normalize_button, selection_button,
              graph_label, graph_drawer;
static GC gc, fgc;

#define GRAPH_SIZE 128
static Float current_graph_data[GRAPH_SIZE]; /* fft window graph in transform options dialog */
static Float current_graph_fftr[GRAPH_SIZE * 2];
static Float current_graph_ffti[GRAPH_SIZE * 2];

#define NUM_TRANSFORM_SIZES 14
static char *TRANSFORM_SIZES[NUM_TRANSFORM_SIZES] = {"16", "32", "64", "128", "256", "512", "1024", "2048", "4096", "8192", "16384", "65536", "262144", "1048576    "};
static int transform_sizes[NUM_TRANSFORM_SIZES] = {16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 65536, 262144, 1048576};

#if HAVE_GSL
  #define GUI_NUM_FFT_WINDOWS NUM_FFT_WINDOWS
#else
  #define GUI_NUM_FFT_WINDOWS (NUM_FFT_WINDOWS - 1)
#endif

static char *FFT_WINDOWS[NUM_FFT_WINDOWS] = 
     {"Rectangular", "Hann", "Welch", "Parzen", "Bartlett", "Hamming", "Blackman2", "Blackman3", "Blackman4",
      "Exponential", "Riemann", "Kaiser", "Cauchy", "Poisson", "Gaussian", "Tukey", "Dolph-Chebyshev"};

static char *WAVELETS[NUM_WAVELETS] = {
  "daub4", "daub6", "daub8", "daub10", "daub12", "daub14", "daub16", "daub18", "daub20",
  "battle_lemarie", "burt_adelson", "beylkin", "coif2", "coif4", "coif6",
  "sym2", "sym3", "sym4", "sym5", "sym6"};

#define NUM_TRANSFORM_TYPES 7
static char *TRANSFORM_TYPES[NUM_TRANSFORM_TYPES] = {"Fourier", "Wavelet", "Walsh", "Autocorrelate", "Cepstrum", "Hadamard", "Haar"};
static int num_transform_types = NUM_TRANSFORM_TYPES;

static char *TRANSFORM_TYPE_CONSTANTS[NUM_TRANSFORM_TYPES] = {
  S_fourier_transform, S_wavelet_transform, S_walsh_transform, S_autocorrelation, S_cepstrum, S_hadamard_transform, S_haar_transform};

char *transform_type_name(int choice) 
{
  if (choice < NUM_TRANSFORM_TYPES)
    return(TRANSFORM_TYPE_CONSTANTS[choice]);
  else return(added_transform_name(choice));
}

int max_transform_type(void) {return(num_transform_types - 1);}

static Float fp_dB(snd_state *ss, Float py)
{
  return((py <= ss->lin_dB) ? 0.0 : (1.0 - ((20.0 * (log10(py))) / ss->min_dB)));
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

static void graph_redisplay(snd_state *ss)
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
  make_axes_1(axis_ap, X_AXIS_IN_SECONDS, 1, SHOW_ALL_AXES, false, true);
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
	iy1 = local_grf_y(fp_dB(ss, current_graph_fftr[i]), axis_ap);
      else iy1 = local_grf_y(current_graph_fftr[i], axis_ap);
      XDrawLine(ax->dp, ax->wn, fgc, ix0, iy0, ix1, iy1);
    }
}

static void get_fft_window_data(snd_state *ss)
{
  int i;
  mus_make_fft_window_with_window(fft_window(ss), GRAPH_SIZE, fft_window_beta(ss) * fft_beta_max(fft_window(ss)), current_graph_data);
  memset((void *)current_graph_fftr, 0, GRAPH_SIZE * 2 * sizeof(Float));
  memset((void *)current_graph_ffti, 0, GRAPH_SIZE * 2 * sizeof(Float));
  memcpy((void *)current_graph_fftr, (void *)current_graph_data, GRAPH_SIZE * sizeof(Float));
  mus_spectrum(current_graph_fftr, current_graph_ffti, NULL, GRAPH_SIZE * 2, 0);
  for (i = 0; i < GRAPH_SIZE; i++)
    current_graph_fftr[i] = (current_graph_fftr[i] + 80.0) / 80.0;
}

static void chans_transform_size(chan_info *cp, void *ptr) 
{
  fft_info *fp;
  int size;
  size = (*((int *)ptr));
  cp->transform_size = size;
  if (cp->fft) 
    {
      fp = cp->fft;
      if (fp->size < size) fp->ok = false; /* "dirty" flag for fft data array = needs reallocation */
      fp->size = size;
    }
}

static void size_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss = (snd_state *)context;
  int size;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  for_each_chan(ss, force_fft_clear);
  in_set_transform_size(ss, transform_sizes[cbs->item_position - 1]);
  size = transform_size(ss);
  for_each_chan_1(ss, chans_transform_size, (void *)(&size));
  for_each_chan(ss, calculate_fft);
  set_label(graph_label, FFT_WINDOWS[(int)fft_window(ss)]);
  get_fft_window_data(ss);
  graph_redisplay(ss);
}

static void chans_wavelet_type(chan_info *cp, void *ptr) {cp->wavelet_type = (*((int *)ptr));}

static void wavelet_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss = (snd_state *)context;
  int val;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  in_set_wavelet_type(ss, val = (cbs->item_position - 1)); /* make these numbers 0-based as in mus.lisp */
  for_each_chan_1(ss, chans_wavelet_type, (void *)(&val));
  if (transform_type(ss) == WAVELET)
    for_each_chan(ss, calculate_fft);
}


static void window_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  mus_fft_window_t fft_window_choice;
  snd_state *ss = (snd_state *)context;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  fft_window_choice = (mus_fft_window_t)(cbs->item_position - 1); /* make these numbers 0-based as in mus.lisp */
  in_set_fft_window(ss, fft_window_choice);
  for_each_chan(ss, calculate_fft);
  set_label(graph_label, FFT_WINDOWS[(int)fft_window(ss)]);
  get_fft_window_data(ss);
  graph_redisplay(ss);
  if (!(ss->using_schemes))
    {
      if (fft_window_beta_in_use(fft_window(ss)))
	XtVaSetValues(window_beta_scale, XmNbackground, (ss->sgx)->highlight_color, NULL);
      else XtVaSetValues(window_beta_scale, XmNbackground, (ss->sgx)->basic_color, NULL);
    }
}

static void chans_transform_type(chan_info *cp, void *ptr) {cp->transform_type = (*((int *)ptr));}

static void transform_type_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  snd_state *ss = (snd_state *)context;
  int type;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsList(w), w);
  type = cbs->item_position - 1;
  for_each_chan(ss, force_fft_clear);
  in_set_transform_type(ss, type);
  for_each_chan_1(ss, chans_transform_type, (void *)(&type));
  for_each_chan(ss, calculate_fft);
}


static void graph_transform_once_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  if (cb->set)
    {
      XmToggleButtonSetState(sono_button, false, false);
      XmToggleButtonSetState(spectro_button, false, false);
      in_set_transform_graph_type(ss, GRAPH_ONCE);
    }
  else
    {
      XmToggleButtonSetState(sono_button, true, false);
      in_set_transform_graph_type(ss, GRAPH_AS_SONOGRAM);
    }
  for_each_chan(ss, calculate_fft);
}

static void sonogram_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  if (cb->set)
    {
      XmToggleButtonSetState(normo_button, false, false);
      XmToggleButtonSetState(spectro_button, false, false);
      in_set_transform_graph_type(ss, GRAPH_AS_SONOGRAM);
    }
  else
    {
      XmToggleButtonSetState(normo_button, true, false);
      in_set_transform_graph_type(ss, GRAPH_ONCE);
    }
  for_each_chan(ss, calculate_fft);
}

static void spectrogram_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  if (cb->set)
    {
      XmToggleButtonSetState(normo_button, false, false);
      XmToggleButtonSetState(sono_button, false, false);
      in_set_transform_graph_type(ss, GRAPH_AS_SPECTROGRAM);
    }
  else
    {
      XmToggleButtonSetState(normo_button, true, false);
      in_set_transform_graph_type(ss, GRAPH_ONCE);
    }
  for_each_chan(ss, calculate_fft);
}

static void map_show_transform_peaks(chan_info *cp, void *ptr) {cp->show_transform_peaks = (*((int *)ptr));}

static void peaks_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  int val = 0;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  val = (cb->set);
  in_set_show_transform_peaks(ss, val);
  for_each_chan_1(ss, map_show_transform_peaks, (void *)(&val));
  for_each_chan(ss, calculate_fft);
}

static void chans_fft_log_magnitude(chan_info *cp, void *ptr) 
{
  cp->fft_log_magnitude = (*((int *)ptr)); 
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void db_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  int val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  val = cb->set;
  in_set_fft_log_magnitude(ss, val);
  for_each_chan_1(ss, chans_fft_log_magnitude, (void *)(&val));
  for_each_chan(ss, calculate_fft);
}

static void chans_fft_log_frequency(chan_info *cp, void *ptr) 
{
  cp->fft_log_frequency = (*((int *)ptr)); 
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void logfreq_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  int val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  val = cb->set;
  in_set_fft_log_frequency(ss, val);
  for_each_chan_1(ss, chans_fft_log_frequency, (void *)(&val));
  for_each_chan(ss, calculate_fft);
}

static void chans_transform_normalization(chan_info *cp, void *ptr) 
{
  cp->transform_normalization = (*((fft_normalize_t *)ptr)); 
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void normalize_callback(Widget w, XtPointer context, XtPointer info)
{
  fft_normalize_t choice;
  snd_state *ss = (snd_state *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  choice = (cb->set) ? NORMALIZE_BY_CHANNEL : DONT_NORMALIZE;
  in_set_transform_normalization(ss, choice);
  for_each_chan_1(ss, chans_transform_normalization, (void *)(&choice));
  for_each_chan(ss, calculate_fft);
}

static void selection_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsToggleButton(w), w);
  in_set_show_selection_transform(ss, cb->set);
  for_each_chan(ss, calculate_fft);
}


static void beta_callback(Widget w, XtPointer context, XtPointer info)
{
  snd_state *ss = (snd_state *)context;
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *)info;
  ASSERT_WIDGET_TYPE(XmIsScale(w), w);
  in_set_fft_window_beta(ss, (Float)(cb->value) / 100.0);
  chans_field(ss, FCP_BETA, (Float)(cb->value) / 100.0);
  if (fft_window_beta_in_use(fft_window(ss)))
    {
      get_fft_window_data(ss);
      graph_redisplay(ss);
      if (transform_type(ss) == FOURIER) 
	for_each_chan(ss, calculate_fft);
    }
} 

static void graph_resize_callback(Widget w, XtPointer context, XtPointer info)
{
  graph_redisplay((snd_state *)context);
}

static void dismiss_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  XtUnmanageChild(transform_dialog);
}

static void orient_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  start_orientation_dialog((snd_state *)context, 0, 0);
}

static void color_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  start_color_dialog((snd_state *)context, 0, 0);
}

static void help_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  transform_dialog_help((snd_state *)context);
}

static bool need_callback = true;
Widget fire_up_transform_dialog(snd_state *ss, bool managed)
{
  XmString xhelp, xdismiss, xtitle, bstr, xorient;
  Arg args[32];
  XmString sizes[NUM_TRANSFORM_SIZES];
  XmString *types;
  XmString wavelets[NUM_WAVELETS];
  XmString windows[GUI_NUM_FFT_WINDOWS];
  XGCValues gv;
  XtCallbackList n1, n2;
  int size_pos = 1;
  int n, i;
  Widget mainform, type_frame, type_form, type_label,
                  size_frame, size_form, size_label,
                  display_frame, display_form, display_label,
                  window_frame, window_form, window_label,
                  wavelet_frame, wavelet_form, wavelet_label,
                  graph_frame, graph_form, color_button;
    
  if (!transform_dialog)
    {
      types = (XmString *)CALLOC(num_transform_types, sizeof(XmString));
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++)
	if (transform_sizes[i] == transform_size(ss))
	  {
	    size_pos = i + 1;
	    break;
	  }
      xdismiss = XmStringCreate(_("Dismiss"), XmFONTLIST_DEFAULT_TAG); /* needed by template dialog */
      xhelp = XmStringCreate(_("Help"), XmFONTLIST_DEFAULT_TAG);
      xtitle = XmStringCreate(_("Transform Options"), XmFONTLIST_DEFAULT_TAG);
      xorient = XmStringCreate(_("Orientation"), XmFONTLIST_DEFAULT_TAG);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNcancelLabelString, xorient); n++;
      XtSetArg(args[n], XmNokLabelString, xdismiss); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      transform_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), _("Transform Options"), args, n);

      XtAddCallback(transform_dialog, XmNcancelCallback, orient_transform_callback, ss);
      XtAddCallback(transform_dialog, XmNokCallback, dismiss_transform_callback, ss);
      XtAddCallback(transform_dialog, XmNhelpCallback, help_transform_callback, ss);
      XmStringFree(xhelp);
      XmStringFree(xdismiss);
      XmStringFree(xtitle);
      XmStringFree(xorient);
      if (!(ss->using_schemes))
	{
	  XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	  XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, (ss->sgx)->pushed_button_color, NULL);
	}
      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;
	  XtSetArg(args[n], XmNarmColor, (ss->sgx)->pushed_button_color); n++;
	}
      color_button = XtCreateManagedWidget(_("Color"), xmPushButtonGadgetClass, transform_dialog, args, n);
      XtAddCallback(color_button, XmNactivateCallback, color_transform_callback, ss);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(transform_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("mainform", xmFormWidgetClass, transform_dialog, args, n);


      /* now 6 boxes within the main box:
	 
	 type (list)    |  size (list)        |  display (button column)
	 wavelet (list) |  window (list+beta) |  graph (fft?) of current window
	 
	 each box has a frame, label, and contents
      */

      /* TYPE */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNbottomPosition, 50); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNborderColor, (ss->sgx)->basic_color); n++;}
      type_frame = XtCreateManagedWidget("type-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      type_form = XtCreateManagedWidget("type-form", xmFormWidgetClass, type_frame, args, n);
      /* needed because XmFrame only accepts one child */

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      type_label = XtCreateManagedWidget(_("type"), xmLabelWidgetClass, type_form, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, type_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      type_list = XmCreateScrolledList(type_form, "type-list", args, n);
      if (!(ss->using_schemes)) XtVaSetValues(type_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      for (i = 0; i < num_transform_types; i++) 
	{
	  if (i < NUM_TRANSFORM_TYPES)
	    types[i] = XmStringCreate(TRANSFORM_TYPES[i], XmFONTLIST_DEFAULT_TAG);
	  else types[i] = XmStringCreate(added_transform_name(i), XmFONTLIST_DEFAULT_TAG);
	}
      XtVaSetValues(type_list, 
		    XmNitems, types, 
		    XmNitemCount, num_transform_types, 
		    XmNvisibleItemCount, 6, 
		    NULL);
      for (i = 0; i < num_transform_types; i++) 
	XmStringFree(types[i]);
      XtManageChild(type_list); 
      XtAddCallback(type_list, XmNbrowseSelectionCallback, transform_type_browse_callback, ss);
      FREE(types);

      /* SIZE */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, type_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, type_frame); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNborderColor, (ss->sgx)->basic_color); n++;}
      size_frame = XtCreateManagedWidget("size-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      size_form = XtCreateManagedWidget("size-form", xmFormWidgetClass, size_frame, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      size_label = XtCreateManagedWidget(_("size"), xmLabelWidgetClass, size_form, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, size_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopItemPosition, (size_pos > 2) ? (size_pos - 2) : size_pos); n++;
      size_list = XmCreateScrolledList(size_form, "size-list", args, n);
      if (!(ss->using_schemes)) XtVaSetValues(size_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++) 
	sizes[i] = XmStringCreate(TRANSFORM_SIZES[i], XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(size_list, 
		    XmNitems, sizes, 
		    XmNitemCount, NUM_TRANSFORM_SIZES, 
		    XmNvisibleItemCount, 6, 
		    NULL);
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++) 
	XmStringFree(sizes[i]);
      XtManageChild(size_list); 
      XtAddCallback(size_list, XmNbrowseSelectionCallback, size_browse_callback, ss);


      /* DISPLAY */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, size_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNborderColor, (ss->sgx)->basic_color); n++;}
      display_frame = XtCreateManagedWidget("display-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->zoom_color); n++;}
      n = attach_all_sides(args, n);
      display_form = XtCreateManagedWidget("display-form", xmFormWidgetClass, display_frame, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      display_label = XtCreateManagedWidget(_("display"), xmLabelWidgetClass, display_form, args, n);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(_("single transform"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, display_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      normo_button = make_togglebutton_widget("normo-button", display_form, args, n);
      XtAddCallback(normo_button, XmNvalueChangedCallback, graph_transform_once_callback, ss);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(_("sonogram"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, normo_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      sono_button = make_togglebutton_widget("sono-button", display_form, args, n);
      XtAddCallback(sono_button, XmNvalueChangedCallback, sonogram_callback, ss);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(_("spectrogram"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, sono_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
      spectro_button = make_togglebutton_widget("spectro-button", display_form, args, n);
      XtAddCallback(spectro_button, XmNvalueChangedCallback, spectrogram_callback, ss);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(_("peaks"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, spectro_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      peaks_button = make_togglebutton_widget("peaks-button", display_form, args, n);
      XtAddCallback(peaks_button, XmNvalueChangedCallback, peaks_callback, ss);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(_("dB"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, peaks_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      db_button = make_togglebutton_widget("db-button", display_form, args, n);
      XtAddCallback(db_button, XmNvalueChangedCallback, db_callback, ss);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(_("log freq"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, db_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      logfreq_button = make_togglebutton_widget("logfreq-button", display_form, args, n);
      XtAddCallback(logfreq_button, XmNvalueChangedCallback, logfreq_callback, ss);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(_("normalize"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, logfreq_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      normalize_button = make_togglebutton_widget("normalize-button", display_form, args, n);
      XtAddCallback(normalize_button, XmNvalueChangedCallback, normalize_callback, ss);
      XmStringFree(bstr);

      n = 0;
      if (!(ss->using_schemes)) 
	{
	  XtSetArg(args[n], XmNbackground, (ss->sgx)->lighter_blue); n++;
	  XtSetArg(args[n], XmNselectColor, (ss->sgx)->red); n++;
	}
      bstr = XmStringCreate(_("selection"), XmFONTLIST_DEFAULT_TAG);
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, normalize_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      selection_button = make_togglebutton_widget("selection-button", display_form, args, n);
      XtAddCallback(selection_button, XmNvalueChangedCallback, selection_callback, ss);
      XmStringFree(bstr);


      
      /* WAVELET */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, type_frame); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, type_frame); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNborderColor, (ss->sgx)->basic_color); n++;}
      wavelet_frame = XtCreateManagedWidget("wavelet-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      wavelet_form = XtCreateManagedWidget("wavelet-form", xmFormWidgetClass, wavelet_frame, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      wavelet_label = XtCreateManagedWidget(_("wavelet"), xmLabelWidgetClass, wavelet_form, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, wavelet_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      wavelet_list = XmCreateScrolledList(wavelet_form, "wavelet-list", args, n);
      if (!(ss->using_schemes)) XtVaSetValues(wavelet_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      for (i = 0; i < NUM_WAVELETS; i++) 
	wavelets[i] = XmStringCreate(WAVELETS[i], XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(wavelet_list, 
		    XmNitems, wavelets, 
		    XmNitemCount, NUM_WAVELETS, 
		    XmNvisibleItemCount, 5, 
		    NULL);
      for (i = 0; i < NUM_WAVELETS; i++) 
	XmStringFree(wavelets[i]);
      XtManageChild(wavelet_list); 
      XtAddCallback(wavelet_list, XmNbrowseSelectionCallback, wavelet_browse_callback, ss);


      /* WINDOW */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, wavelet_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNrightWidget, size_frame); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, size_frame); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNborderColor, (ss->sgx)->basic_color); n++;}
      window_frame = XtCreateManagedWidget("window-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      window_form = XtCreateManagedWidget("window-form", xmFormWidgetClass, window_frame, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNshowValue, true); n++;
      XtSetArg(args[n], XmNdecimalPoints, 2); n++;
      XtSetArg(args[n], XmNvalue, 100 * fft_window_beta(ss)); n++;
      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(beta_callback, (XtPointer)ss)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2 = make_callback_list(beta_callback, (XtPointer)ss)); n++;
      window_beta_scale = XtCreateManagedWidget("beta-scale", xmScaleWidgetClass, window_form, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      window_label = XtCreateManagedWidget(_("window"), xmLabelWidgetClass, window_form, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, window_beta_scale); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, window_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopItemPosition, ((int)fft_window(ss) > 2) ? ((int)fft_window(ss) - 1) : ((int)fft_window(ss) + 1)); n++;
      window_list = XmCreateScrolledList(window_form, "window-list", args, n);
      if (!(ss->using_schemes)) XtVaSetValues(window_list, XmNbackground, (ss->sgx)->white, XmNforeground, (ss->sgx)->black, NULL);
      for (i = 0; i < GUI_NUM_FFT_WINDOWS; i++)
	windows[i] = XmStringCreate(FFT_WINDOWS[i], XmFONTLIST_DEFAULT_TAG);
      XtVaSetValues(window_list, 
		    XmNitems, windows, 
		    XmNitemCount, GUI_NUM_FFT_WINDOWS, 
		    XmNvisibleItemCount, 5, 
		    NULL);
      for (i = 0; i < GUI_NUM_FFT_WINDOWS; i++) 
	XmStringFree(windows[i]);
      XtManageChild(window_list); 
      XtAddCallback(window_list, XmNbrowseSelectionCallback, window_browse_callback, ss);



      /* GRAPH */
      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, window_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, display_frame); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, 4); n++;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNborderColor, (ss->sgx)->basic_color); n++;}
      graph_frame = XtCreateManagedWidget("graph-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->basic_color); n++;}
      n = attach_all_sides(args, n);
      graph_form = XtCreateManagedWidget("graph-form", xmFormWidgetClass, graph_frame, args, n);

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->highlight_color); n++;}
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      graph_label = XtCreateManagedWidget(_("window"), xmLabelWidgetClass, graph_form, args, n);
      /* label should change according to what is being displayed */

      n = 0;
      if (!(ss->using_schemes)) {XtSetArg(args[n], XmNbackground, (ss->sgx)->graph_color); n++;}
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

      gv.foreground = (ss->sgx)->enved_waveform_color;
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

      XmListSelectPos(type_list, transform_type(ss) + 1, false);
      XmListSelectPos(wavelet_list, wavelet_type(ss) + 1, false);
      XmListSelectPos(size_list, size_pos, false);
      XmListSelectPos(window_list, (int)fft_window(ss) + 1, false);

      if (fft_window_beta_in_use(fft_window(ss))) 
	XtVaSetValues(window_beta_scale, XmNbackground, (ss->sgx)->highlight_color, NULL);

      FREE(n1);
      FREE(n2);
      set_dialog_widget(ss, TRANSFORM_DIALOG, transform_dialog);
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
      get_fft_window_data(ss);
      XtAddCallback(graph_drawer, XmNresizeCallback, graph_resize_callback, ss);
      XtAddCallback(graph_drawer, XmNexposeCallback, graph_resize_callback, ss);
      need_callback = false;
    }
  return(transform_dialog);
}

bool transform_dialog_is_active(void)
{
  return((transform_dialog) && (XtIsManaged(transform_dialog)));
}

/* various set- cases need to be reflected in the transform dialog */
void set_fft_window_beta(snd_state *ss, Float val)
{
  in_set_fft_window_beta(ss, val);
  chans_field(ss, FCP_BETA, val);
  if (transform_dialog) 
    {
      XmScaleSetValue(window_beta_scale, (int)(100 * val));
      get_fft_window_data(ss);
      graph_redisplay(ss);
    }
  if (!(ss->graph_hook_active)) 
    for_each_chan(ss, calculate_fft);
}

void set_transform_graph_type(snd_state *ss, graph_type_t val)
{
  in_set_transform_graph_type(ss, val);
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
    for_each_chan(ss, calculate_fft);
}

void set_transform_size(snd_state *ss, int val)
{
  int i;
  for_each_chan(ss, force_fft_clear);
  in_set_transform_size(ss, val);
  for_each_chan_1(ss, chans_transform_size, (void *)(&val));
  if (transform_dialog)
    {
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++)
	if (transform_sizes[i] == val)
	  {
	    XmListSelectPos(size_list, i + 1, false);
	    break;
	  }
    }
  if (!(ss->graph_hook_active)) for_each_chan(ss, calculate_fft);
}

void set_fft_window(snd_state *ss, mus_fft_window_t val)
{
  in_set_fft_window(ss, val);
  if (!(ss->graph_hook_active)) for_each_chan(ss, calculate_fft);
  if (transform_dialog)
    {
      XmListSelectPos(window_list, (int)val + 1, false);
      set_label(graph_label, FFT_WINDOWS[val]);
      get_fft_window_data(ss);
      graph_redisplay(ss);
      if (fft_window_beta_in_use(val))
	XtVaSetValues(window_beta_scale, XmNbackground, (ss->sgx)->highlight_color, NULL);
      else XtVaSetValues(window_beta_scale, XmNbackground, (ss->sgx)->basic_color, NULL);
    }
}
  
void set_transform_type(snd_state *ss, int val)
{
  if (!(ss->graph_hook_active)) for_each_chan(ss, force_fft_clear);
  in_set_transform_type(ss, val);
  for_each_chan_1(ss, chans_transform_type, (void *)(&val));
  if (!(ss->graph_hook_active)) 
    for_each_chan(ss, calculate_fft);
  if (transform_dialog) XmListSelectPos(type_list, val + 1, false);
}

void set_wavelet_type(snd_state *ss, int val)
{
  if (transform_dialog) XmListSelectPos(wavelet_list, val, false);
  in_set_wavelet_type(ss, val);
  for_each_chan_1(ss, chans_wavelet_type, (void *)(&val));
  if ((transform_type(ss) == WAVELET) && 
      (!(ss->graph_hook_active))) 
    for_each_chan(ss, calculate_fft);
}

/* various set- cases need to be reflected in the transform dialog */
void set_show_transform_peaks(snd_state *ss, bool val)
{
  in_set_show_transform_peaks(ss, val);
  for_each_chan_1(ss, map_show_transform_peaks, (void *)(&val));
  if (transform_dialog) 
    set_toggle_button(peaks_button, val, false, (void *)ss);
  if (!(ss->graph_hook_active)) 
    for_each_chan(ss, calculate_fft);
}

void set_fft_log_frequency(snd_state *ss, bool val)
{
  in_set_fft_log_frequency(ss, val);
  for_each_chan_1(ss, chans_fft_log_frequency, (void *)(&val));
  if (transform_dialog)
    set_toggle_button(logfreq_button, val, false, (void *)ss);
  if (!(ss->graph_hook_active)) 
    for_each_chan(ss, calculate_fft);
}

void set_fft_log_magnitude(snd_state *ss, bool val)
{
  in_set_fft_log_magnitude(ss, val);
  for_each_chan_1(ss, chans_fft_log_magnitude, (void *)(&val));
  if (transform_dialog) 
    set_toggle_button(db_button, val, false, (void *)ss);
  if (!(ss->graph_hook_active)) 
    for_each_chan(ss, calculate_fft);
}

void set_transform_normalization(snd_state *ss, fft_normalize_t val)
{
  in_set_transform_normalization(ss, val);
  for_each_chan_1(ss, chans_transform_normalization, (void *)(&val));
  if (transform_dialog) 
    set_toggle_button(normalize_button, (val != DONT_NORMALIZE), false, (void *)ss);
  if (!(ss->graph_hook_active)) 
    for_each_chan(ss, calculate_fft);
}

void set_show_selection_transform(snd_state *ss, bool show)
{
  in_set_show_selection_transform(ss, show);
  if (transform_dialog)
    set_toggle_button(selection_button, show, false, (void *)ss); 
  if (!(ss->graph_hook_active)) 
    for_each_chan(ss, calculate_fft);
}

int add_transform_to_list(char *name)
{
  /* put at end of list and return associated browse callback row */
  XmString str;
  if (transform_dialog)
    {
      str = XmStringCreate(name, XmFONTLIST_DEFAULT_TAG);
      XmListAddItem(type_list, str, 0);
      XmStringFree(str);
    }
  return(num_transform_types++);
}
