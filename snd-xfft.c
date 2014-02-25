/* Transform settings dialog */

#include "snd.h"


static Widget transform_dialog = NULL; /* main dialog shell */
static Widget type_list, size_list, wavelet_list, window_list;
static Widget beta_scale, alpha_scale, start_scale, end_scale, alpha_number, beta_number, start_number, end_number;  
static Widget db_button, peaks_button, logfreq_button, sono_button, spectro_button, normo_button, normalize_button, selection_button, phases_button;
static Widget graph_label, graph_drawer;
static Widget peak_txt, db_txt, freq_base_txt;
static Widget error_frame, error_label;

#define NUM_TRANSFORM_SIZES 15
static const char *transform_size_names[NUM_TRANSFORM_SIZES] = 
  {"32", "64", "128", "256", "512", "1024", "2048", "4096", "8192", "16384", "65536", "262144", "1048576", "4194304    ", "16777216"};
static mus_long_t transform_sizes[NUM_TRANSFORM_SIZES] = 
  {32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 65536, 262144, 1048576, 4194304, 16777216};



/* ---------------- fft window graph ---------------- */

static GC gc, fgc;

#define GRAPH_SIZE 128
static mus_float_t graph_data[GRAPH_SIZE]; /* fft window graph in transform options dialog */
static mus_float_t graph_fftr[GRAPH_SIZE * 2];
static mus_float_t graph_ffti[GRAPH_SIZE * 2];
/* I goofed around with making the graph size dependent on the drawer's width, but there's really nothing gained */
/*   also tried linear/db+min-dB distinction, but linear looks dumb and min-dB is a bother */

static mus_float_t fp_dB(mus_float_t py)
{
  return((py <= ss->lin_dB) ? 0.0 : (1.0 - (20.0 * log10(py) / min_dB(ss))));
}


static int local_grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((int)(ap->x_base + val * ap->x_scale));
}


static int local_grf_y(mus_float_t val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((int)(ap->y_base + val * ap->y_scale));
}


static axis_info *axis_ap = NULL;

static void graph_redisplay(void)
{
  /* fft_window(ss) is the current choice */
  int ix0, iy0, ix1, iy1, i;
  mus_float_t xincr, x;
  graphics_context *ax;

  if (axis_ap == NULL) 
    {
      axis_ap = (axis_info *)calloc(1, sizeof(axis_info));
      ax = (graphics_context *)calloc(1, sizeof(graphics_context));
      axis_ap->ax = ax;
      ax->dp = XtDisplay(graph_drawer);
      ax->wn = XtWindow(graph_drawer);
    }
  else ax = axis_ap->ax;

  axis_ap->xmin = 0.0;
  axis_ap->xmax = 1.0;
  axis_ap->x_ambit = 1.0;
  axis_ap->x0 = 0.0;
  axis_ap->x1 = 1.0;

  if (axis_ap->xlabel) free(axis_ap->xlabel);
  if (fft_beta_max(fft_window(ss)) != 1.0)
    axis_ap->xlabel = mus_format("(%d, beta: %.2f)", GRAPH_SIZE, fft_beta_max(fft_window(ss)) * fft_window_beta(ss));
  else axis_ap->xlabel = mus_format("(%d)", GRAPH_SIZE);

  if (fft_window(ss) == MUS_FLAT_TOP_WINDOW)
    {
      axis_ap->ymin = -0.1;
      axis_ap->ymax = 1.0;
      axis_ap->y_ambit = 1.1;
      axis_ap->y0 = -0.1;
      axis_ap->y1 = 1.0;
    }
  else 
    {
      axis_ap->ymin = 0.0;
      axis_ap->ymax = 1.0;
      axis_ap->y_ambit = 1.0;
      axis_ap->y0 = 0.0;
      axis_ap->y1 = 1.0;
    }

  axis_ap->width = widget_width(graph_drawer);
  axis_ap->window_width = axis_ap->width;
  axis_ap->y_offset = 0;
  axis_ap->height = widget_height(graph_drawer);
  axis_ap->graph_x0 = 0;

  clear_window(ax);
  ax->gc = gc;
  make_axes_1(axis_ap, X_AXIS_IN_SECONDS, 1 /* "srate" */, SHOW_ALL_AXES, NOT_PRINTING, WITH_X_AXIS, NO_GRID, WITH_LINEAR_AXES, grid_density(ss));

  ix1 = local_grf_x(0.0, axis_ap);
  iy1 = local_grf_y(graph_data[0], axis_ap);
  xincr = 1.0 / (mus_float_t)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x += xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = local_grf_x(x, axis_ap);
      iy1 = local_grf_y(graph_data[i], axis_ap);
      XDrawLine(ax->dp, ax->wn, gc, ix0, iy0, ix1, iy1);
    }

  ax->gc = fgc;
  ix1 = local_grf_x(0.0, axis_ap);
  iy1 = local_grf_y(graph_fftr[0], axis_ap);
  xincr = 1.0 / (mus_float_t)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x += xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = local_grf_x(x, axis_ap);
      if (fft_log_magnitude(ss))
	iy1 = local_grf_y(fp_dB(graph_fftr[i]), axis_ap);
      else iy1 = local_grf_y(graph_fftr[i], axis_ap);
      XDrawLine(ax->dp, ax->wn, fgc, ix0, iy0, ix1, iy1);
    }
}


static void get_fft_window_data(void)
{
  int i;
  mus_make_fft_window_with_window(fft_window(ss), GRAPH_SIZE, 
				  fft_window_beta(ss) * fft_beta_max(fft_window(ss)), 
				  fft_window_alpha(ss), graph_data);
  memset((void *)graph_fftr, 0, GRAPH_SIZE * 2 * sizeof(mus_float_t));
  memset((void *)graph_ffti, 0, GRAPH_SIZE * 2 * sizeof(mus_float_t));
  memcpy((void *)graph_fftr, (void *)graph_data, GRAPH_SIZE * sizeof(mus_float_t));
  mus_spectrum(graph_fftr, graph_ffti, NULL, GRAPH_SIZE * 2, MUS_SPECTRUM_IN_DB);
  for (i = 0; i < GRAPH_SIZE; i++)
    graph_fftr[i] = (graph_fftr[i] + 80.0) / 80.0; /* min dB here is -80 */
}


static void widget_float_to_text(Widget w, mus_float_t val)
{
  char *str;
  str = (char *)calloc(16, sizeof(char));
  snprintf(str, 16, "%.2f", val);
  XmTextFieldSetString(w, str);
  free(str);
}



/* ---------------- errors ---------------- */

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



/* ---------------- transform size ---------------- */

static void chans_transform_size(chan_info *cp, mus_long_t size)
{
  cp->transform_size = size;
  if (cp->fft) 
    cp->fft->size = size;
}


void set_transform_size(mus_long_t val)
{
  for_each_chan(force_fft_clear);
  in_set_transform_size(val);
  for_each_chan_with_mus_long_t(chans_transform_size, val);
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


static void size_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  for_each_chan(force_fft_clear);
  in_set_transform_size(transform_sizes[cbs->item_position - 1]);
  for_each_chan_with_mus_long_t(chans_transform_size, transform_size(ss));
  for_each_chan(calculate_fft);
  set_label(graph_label, mus_fft_window_name(fft_window(ss)));
}


/* ---------------- wavelet choice ---------------- */

static void chans_wavelet_type(chan_info *cp, int value)
{
  cp->wavelet_type = value;
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


static void wavelet_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  int val;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  in_set_wavelet_type(val = (cbs->item_position - 1)); /* make these numbers 0-based as in mus.lisp */
  for_each_chan_with_int(chans_wavelet_type, val);
  if (transform_type(ss) == WAVELET)
    for_each_chan(calculate_fft);
}


/* ---------------- fft window choice ---------------- */

static void highlight_alpha_beta_scales(mus_fft_window_t val)
{
  if (fft_window_beta_in_use(val))
    {
      XtVaSetValues(beta_scale, XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(beta_number, XmNbackground, ss->highlight_color, NULL);
    }
  else 
    {
      XtVaSetValues(beta_scale, XmNbackground, ss->basic_color, NULL);
      XtVaSetValues(beta_number, XmNbackground, ss->basic_color, NULL);
    }

  if (fft_window_alpha_in_use(val))
    {
      XtVaSetValues(alpha_scale, XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(alpha_number, XmNbackground, ss->highlight_color, NULL);
    }
  else 
    {
      XtVaSetValues(alpha_scale, XmNbackground, ss->basic_color, NULL);
      XtVaSetValues(alpha_number, XmNbackground, ss->basic_color, NULL);
    }
}


void set_fft_window(mus_fft_window_t val)
{
  in_set_fft_window(val);
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
  if (transform_dialog)
    {
      XmListSelectPos(window_list, (int)val + 1, false);
      set_label(graph_label, mus_fft_window_name(val));
      get_fft_window_data();
      if (XtIsManaged(transform_dialog))
	graph_redisplay();
      highlight_alpha_beta_scales(val);
    }
}


static void window_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  mus_fft_window_t fft_window_choice;

  fft_window_choice = (mus_fft_window_t)(cbs->item_position - 1); /* make these numbers 0-based as in mus.lisp */

  in_set_fft_window(fft_window_choice);
  for_each_chan(calculate_fft);
  set_label(graph_label, mus_fft_window_name(fft_window(ss)));
  get_fft_window_data();
  graph_redisplay();
  highlight_alpha_beta_scales(fft_window_choice);
}



/* ---------------- transform choice ---------------- */

static void chans_transform_type(chan_info *cp, int value) 
{
  cp->transform_type = value;
}


void set_transform_type(int val)
{
  if (is_transform(val))
    {
      if (!(ss->graph_hook_active)) for_each_chan(force_fft_clear);
      in_set_transform_type(val);
      for_each_chan_with_int(chans_transform_type, val);
      if (!(ss->graph_hook_active)) 
	for_each_chan(calculate_fft);
      if (transform_dialog) XmListSelectPos(type_list, transform_type_to_position(val) + 1, false);
    }
}


static void transform_type_browse_callback(Widget w, XtPointer context, XtPointer info) 
{
  int type;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *)info;
  type = transform_position_to_type(cbs->item_position - 1);
  for_each_chan(force_fft_clear);
  in_set_transform_type(type);
  for_each_chan_with_int(chans_transform_type, type);
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
      types = (XmString *)calloc(num, sizeof(XmString));
      for (i = 0, j = 0; i < num; i++) 
	if (is_transform(i))
	  {
	    set_transform_position(i, j);
	    types[j++] = XmStringCreateLocalized((char *)transform_name(i)); 
	  }
      XtVaSetValues(type_list, 
		    XmNitems, types, 
		    XmNitemCount, j,
		    XmNvisibleItemCount, 6, 
		    NULL);
      for (i = 0; i < j; i++) 
	XmStringFree(types[i]);
      free(types);
    }
}



/* ---------------- transform "graph type" (i.e. sonogram etc) ---------------- */

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


static void graph_transform_once_callback(Widget w, XtPointer context, XtPointer info)
{
  graph_type_t old_type;
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
  old_type = transform_graph_type(ss);
  XmToggleButtonSetState(spectro_button, true, false);
  XmToggleButtonSetState(normo_button, false, false);
  XmToggleButtonSetState(sono_button, false, false);
  in_set_transform_graph_type(GRAPH_AS_SPECTROGRAM);
  if (old_type != GRAPH_AS_SPECTROGRAM)
    for_each_chan(calculate_fft);
}



/* ---------------- show peaks ---------------- */

static void map_show_transform_peaks(chan_info *cp, bool value) 
{
  cp->show_transform_peaks = value;
}


static void peaks_callback(Widget w, XtPointer context, XtPointer info)
{
  bool val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  val = (cb->set);
  in_set_show_transform_peaks(val);
  for_each_chan_with_bool(map_show_transform_peaks, val);
  for_each_chan(calculate_fft);
}


void set_show_transform_peaks(bool val)
{
  in_set_show_transform_peaks(val);
  for_each_chan_with_bool(map_show_transform_peaks, val);
  if (transform_dialog) 
    set_toggle_button(peaks_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
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



/* ---------------- log magnitude ---------------- */

static void chans_fft_log_magnitude(chan_info *cp, bool value)
{
  cp->fft_log_magnitude = value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
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



/* ---------------- dB ---------------- */

static void fft_db_callback(Widget w, XtPointer context, XtPointer info)
{
  bool val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  val = cb->set;
  in_set_fft_log_magnitude(val);
  graph_redisplay();
  for_each_chan_with_bool(chans_fft_log_magnitude, val);
  for_each_chan(calculate_fft);
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
      mus_float_t new_db;
      redirect_errors_to(errors_to_fft_text, NULL);
      new_db = string_to_mus_float_t(str, -10000.0, "dB");
      redirect_errors_to(NULL, NULL);
      if (new_db < 0.0)
	set_min_db(new_db);
      else widget_float_to_text(w, min_dB(ss));
      XtFree(str);
    }
}



/* ---------------- log frequency ---------------- */

static void chans_fft_log_frequency(chan_info *cp, bool value)
{
  cp->fft_log_frequency = value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}


static void logfreq_callback(Widget w, XtPointer context, XtPointer info)
{
  bool val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  val = cb->set;
  in_set_fft_log_frequency(val);
  for_each_chan_with_bool(chans_fft_log_frequency, val);
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
      mus_float_t new_lfb;
      redirect_errors_to(errors_to_fft_text, NULL);
      new_lfb = string_to_mus_float_t(str, 0.0, "log freq start");
      redirect_errors_to(NULL, NULL);
      if (new_lfb > 0.0)
	set_log_freq_start(new_lfb);
      else widget_float_to_text(w, log_freq_start(ss));
      XtFree(str);
    }
}




/* ---------------- normalization choice ---------------- */

static void chans_transform_normalization(chan_info *cp, int value)
{
  cp->transform_normalization = (fft_normalize_t)value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}


static void normalize_callback(Widget w, XtPointer context, XtPointer info)
{
  fft_normalize_t choice;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  choice = (cb->set) ? NORMALIZE_BY_CHANNEL : DONT_NORMALIZE;
  in_set_transform_normalization(choice);
  for_each_chan_with_int(chans_transform_normalization, (int)choice);
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



/* ---------------- show selection transform ---------------- */

static void selection_callback(Widget w, XtPointer context, XtPointer info)
{
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  in_set_show_selection_transform(cb->set);
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



/* ---------------- show phases (via color) ---------------- */

static void chans_fft_with_phases(chan_info *cp, bool value)
{
  cp->fft_with_phases = value;
  cp->fft_changed = FFT_CHANGE_LOCKED;
}


static void phases_callback(Widget w, XtPointer context, XtPointer info)
{
  bool val;
  XmToggleButtonCallbackStruct *cb = (XmToggleButtonCallbackStruct *)info;
  val = cb->set;
  in_set_fft_with_phases(val);
  graph_redisplay();
  for_each_chan_with_bool(chans_fft_with_phases, val);
  for_each_chan(calculate_fft);
}


void set_fft_with_phases(bool val)
{
  in_set_fft_with_phases(val);
  for_each_chan_with_bool(chans_fft_with_phases, val);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}



/* ---------------- window alpha parameter ---------------- */

static void alpha_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  char alpha_number_buffer[11];
  mus_float_t alpha;
  
  alpha = (((XmScrollBarCallbackStruct *)info)->value) / 90.0;
  in_set_fft_window_alpha(alpha);
  chans_field(FCP_ALPHA, alpha);

  snprintf(alpha_number_buffer, 11, "alpha:%.3f", alpha);
  set_label(alpha_number, alpha_number_buffer);

  if (fft_window_alpha_in_use(fft_window(ss)))
    {
      get_fft_window_data();
      graph_redisplay();
      if (transform_type(ss) == FOURIER) 
	for_each_chan(calculate_fft);
    }
}

static void set_alpha_scale(mus_float_t val)
{
  char alpha_number_buffer[11];
  XtVaSetValues(alpha_scale, XmNvalue, (int)(val * 90), NULL);
  snprintf(alpha_number_buffer, 11, "alpha:%.3f", val);
  set_label(alpha_number, alpha_number_buffer);
}


void set_fft_window_alpha(mus_float_t val)
{
  in_set_fft_window_alpha(val);
  chans_field(FCP_ALPHA, val);
  if (transform_dialog) 
    {
      set_alpha_scale(val);
      get_fft_window_data();
      if (XtIsManaged(transform_dialog))
	graph_redisplay();
    }
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}



/* ---------------- window beta parameter ---------------- */

static void beta_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  char beta_number_buffer[11];
  mus_float_t beta;
  
  beta = (((XmScrollBarCallbackStruct *)info)->value) / 90.0;
  in_set_fft_window_beta(beta);
  chans_field(FCP_BETA, beta);

  snprintf(beta_number_buffer, 11, "beta: %.3f", beta);
  set_label(beta_number, beta_number_buffer);

  if (fft_window_beta_in_use(fft_window(ss)))
    {
      get_fft_window_data();
      graph_redisplay();
      if (transform_type(ss) == FOURIER) 
	for_each_chan(calculate_fft);
    }
}


static void set_beta_scale(mus_float_t val)
{
  char beta_number_buffer[11];
  XtVaSetValues(beta_scale, XmNvalue, (int)(val * 90), NULL);
  snprintf(beta_number_buffer, 11, "beta: %.3f", val);
  set_label(beta_number, beta_number_buffer);
}


void set_fft_window_beta(mus_float_t val)
{
  in_set_fft_window_beta(val);
  chans_field(FCP_BETA, val);
  if (transform_dialog) 
    {
      set_beta_scale(val);
      get_fft_window_data();
      if (XtIsManaged(transform_dialog))
	graph_redisplay();
    }
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}



/* ---------------- spectrum start/end ---------------- */

static void chans_spectrum_changed(chan_info *cp) 
{
  cp->fft_changed = FFT_CHANGE_LOCKED;
  update_graph(cp);
}

static void set_spectrum_start_scale(mus_float_t val)
{
  char start_number_buffer[11];
  XtVaSetValues(start_scale, XmNvalue, (int)(val * 90), NULL);
  snprintf(start_number_buffer, 11, "start:%.3f", val);
  set_label(start_number, start_number_buffer);
}


static void check_spectrum_start(mus_float_t end)
{
  /* don't display chans, but do reset if necessary */
  if (spectrum_start(ss) > end)
    {
      in_set_spectrum_start(end);
      if (transform_dialog)
	set_spectrum_start_scale(end);
      chans_field(FCP_SPECTRUM_START, end);
    }
}

static void check_spectrum_end(mus_float_t start);

void set_spectrum_start(mus_float_t val) 
{
  if (transform_dialog)
    set_spectrum_start_scale(val);
  in_set_spectrum_start(val);
  check_spectrum_end(val);
  chans_field(FCP_SPECTRUM_START, val);
  for_each_chan(chans_spectrum_changed);
}


static void start_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  char start_number_buffer[11];
  mus_float_t start;
  
  start = (((XmScrollBarCallbackStruct *)info)->value) / 90.0;
  snprintf(start_number_buffer, 11, "start:%.3f", start);
  set_label(start_number, start_number_buffer);

  in_set_spectrum_start(start);
  check_spectrum_end(start);
  chans_field(FCP_SPECTRUM_START, start);
  for_each_chan(chans_spectrum_changed);
}


static void set_spectrum_end_scale(mus_float_t val)
{
  char end_number_buffer[11];
  XtVaSetValues(end_scale, XmNvalue, (int)(val * 90), NULL);
  snprintf(end_number_buffer, 11, "end:  %.3f", val);
  set_label(end_number, end_number_buffer);
}

static void check_spectrum_end(mus_float_t start)
{
  /* don't display chans, but do reset if necessary */
  if (spectrum_end(ss) < start)
    {
      in_set_spectrum_end(start);
      if (transform_dialog)
	set_spectrum_end_scale(start);
      chans_field(FCP_SPECTRUM_END, start);
    }
}


void set_spectrum_end(mus_float_t val)
{
  if (transform_dialog)
    set_spectrum_end_scale(val);
  in_set_spectrum_end(val);
  check_spectrum_start(val);
  chans_field(FCP_SPECTRUM_END, val);
  for_each_chan(chans_spectrum_changed);
}


static void end_drag_callback(Widget w, XtPointer context, XtPointer info) 
{
  char end_number_buffer[11];
  mus_float_t end;

  end = (((XmScrollBarCallbackStruct *)info)->value) / 90.0;
  snprintf(end_number_buffer, 11, "end:  %.3f", end);
  set_label(end_number, end_number_buffer);

  in_set_spectrum_end(end);
  check_spectrum_start(end);
  chans_field(FCP_SPECTRUM_END, end);
  for_each_chan(chans_spectrum_changed);
}



/* ---------------- dialog buttons etc ---------------- */

static void graph_resize_callback(Widget w, XtPointer context, XtPointer info)
{
  graph_redisplay();
}


static void dismiss_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  if (XmGetFocusWidget(transform_dialog) == XmMessageBoxGetChild(transform_dialog, XmDIALOG_CANCEL_BUTTON))
    XtUnmanageChild(transform_dialog);
}


static void color_orientation_callback(Widget w, XtPointer context, XtPointer info)
{
  make_color_orientation_dialog(true);
}


static void help_transform_callback(Widget w, XtPointer context, XtPointer info)
{
  transform_dialog_help();
}


static void fft_blue_textfield_unfocus_callback(Widget w, XtPointer context, XtPointer info)
{
  XtVaSetValues(w, XmNbackground, ss->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}


static void fft_blue_mouse_leave_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  XtVaSetValues(w, XmNbackground, ss->lighter_blue, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, false, NULL);
}


static void fft_white_mouse_enter_text_callback(Widget w, XtPointer context, XEvent *event, Boolean *flag)
{
  XtVaSetValues(w, XmNbackground, ss->text_focus_color, NULL);
  XtVaSetValues(w, XmNcursorPositionVisible, true, NULL);
}



/* ---------------- transform options dialog ---------------- */

#define FRAME_BORDER_WIDTH 6

static bool need_callback = true;

Widget make_transform_dialog(bool managed)
{
  if (!transform_dialog)
    {
      Widget mainform, type_frame, type_form, type_label, size_frame, size_form, size_label, display_frame, display_form, display_label;
      Widget window_frame, window_form, window_label, wavelet_frame, wavelet_form, wavelet_label, graph_frame, graph_form, gsep;
      Widget ab_form, ab_frame, ab_title, ab_sep;
      Widget se_form, se_frame, se_title, se_sep, ok_button;
      XmString s1;
      XmString xhelp, xgo_away, xtitle, bstr, xorient;
      Arg args[32];
      XmString sizes[NUM_TRANSFORM_SIZES];
      XmString wavelets[NUM_WAVELETS];
      XmString windows[MUS_NUM_FFT_WINDOWS];
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
      xgo_away = XmStringCreateLocalized((char *)I_GO_AWAY); /* needed by template dialog */
      xhelp = XmStringCreateLocalized((char *)I_HELP);
      xtitle = XmStringCreateLocalized((char *)"Transform Options");
      xorient = XmStringCreateLocalized((char *)"Color/Orientation");

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNcancelLabelString, xgo_away); n++;
      XtSetArg(args[n], XmNokLabelString, xorient); n++;
      XtSetArg(args[n], XmNhelpLabelString, xhelp); n++;
      XtSetArg(args[n], XmNautoUnmanage, false); n++;
      XtSetArg(args[n], XmNdialogTitle, xtitle); n++;
      XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
      XtSetArg(args[n], XmNnoResize, false); n++;
      XtSetArg(args[n], XmNtransient, false); n++;
      transform_dialog = XmCreateTemplateDialog(MAIN_SHELL(ss), (char *)"Transform Options", args, n);
      ok_button = XmMessageBoxGetChild(transform_dialog, XmDIALOG_OK_BUTTON);

      XtAddCallback(transform_dialog, XmNcancelCallback, dismiss_transform_callback, NULL);
      /* XtAddCallback(transform_dialog, XmNokCallback, color_orientation_callback, NULL); */ /* <cr> in dialog calls this! */
      XtAddCallback(ok_button, XmNactivateCallback, color_orientation_callback, NULL);
      XtAddCallback(transform_dialog, XmNhelpCallback, help_transform_callback, NULL);
      XmStringFree(xhelp);
      XmStringFree(xgo_away);
      XmStringFree(xtitle);
      XmStringFree(xorient);

      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_CANCEL_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_HELP_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_OK_BUTTON), XmNarmColor, ss->selection_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_CANCEL_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_HELP_BUTTON), XmNbackground, ss->highlight_color, NULL);
      XtVaSetValues(XmMessageBoxGetChild(transform_dialog, XmDIALOG_OK_BUTTON), XmNbackground, ss->highlight_color, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, XmMessageBoxGetChild(transform_dialog, XmDIALOG_SEPARATOR)); n++;
      mainform = XtCreateManagedWidget("mainform", xmFormWidgetClass, transform_dialog, args, n);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNshadowThickness, 2); n++;
      error_frame = XtCreateManagedWidget("error-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      error_label = XtCreateManagedWidget("", xmLabelWidgetClass, error_frame, args, n);


      /* now 7 or 8 boxes within the main box:
	 
	 type (list)    |  size (list)        |  display (button column)
	 wavelet (list) |  window (list)      |  graph (fft?) of current window
         alpha/beta ------------------------  |
         start/end -------------------------  |
	 
	 each box has a frame, label, and contents
      */

      /* -------- SPECTRUM START/END -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 60); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, FRAME_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      se_frame = XtCreateManagedWidget("se-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      n = attach_all_sides(args, n);
      se_form = XtCreateManagedWidget("se-form", xmFormWidgetClass, se_frame, args, n);
      /* needed because XmFrame only accepts one child */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      se_title = XtCreateManagedWidget("spectrum start/end", xmLabelWidgetClass, se_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, se_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      se_sep = XtCreateManagedWidget("se_sep", xmSeparatorWidgetClass, se_form, args, n);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"start:0.0  ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, se_sep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      start_number = XtCreateManagedWidget("start-number", xmLabelWidgetClass, se_form, args, n);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, start_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, start_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, 100); n++;
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n3 = make_callback_list(start_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n3); n++;
      start_scale = XtCreateManagedWidget("start-scale", xmScrollBarWidgetClass, se_form, args, n);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"end:  1.0  ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, start_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      end_number = XtCreateManagedWidget("end-number", xmLabelWidgetClass, se_form, args, n);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, end_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, end_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, 100); n++;
      XtSetArg(args[n], XmNvalue, 90); n++;
      XtSetArg(args[n], XmNheight, 16); n++;
      XtSetArg(args[n], XmNdragCallback, n4 = make_callback_list(end_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n4); n++;
      end_scale = XtCreateManagedWidget("end-scale", xmScrollBarWidgetClass, se_form, args, n);



      /* -------- WINDOW ALPHA/BETA -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 60); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, se_frame); n++;
      XtSetArg(args[n], XmNborderWidth, FRAME_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      ab_frame = XtCreateManagedWidget("ab-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      n = attach_all_sides(args, n);
      ab_form = XtCreateManagedWidget("ab-form", xmFormWidgetClass, ab_frame, args, n);
      /* needed because XmFrame only accepts one child */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      ab_title = XtCreateManagedWidget("window parameter", xmLabelWidgetClass, ab_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ab_title); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      ab_sep = XtCreateManagedWidget("ab_sep", xmSeparatorWidgetClass, ab_form, args, n);

      n = 0;
      s1 = XmStringCreateLocalized((char *)"alpha:0.0  ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, ab_sep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      alpha_number = XtCreateManagedWidget("alpha-number", xmLabelWidgetClass, ab_form, args, n);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, alpha_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, alpha_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, 100); n++;
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNheight, 16); n++;

      XtSetArg(args[n], XmNdragCallback, n1 = make_callback_list(alpha_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n1); n++;
      alpha_scale = XtCreateManagedWidget("alpha-scale", xmScrollBarWidgetClass, ab_form, args, n);


      n = 0;
      s1 = XmStringCreateLocalized((char *)"beta: 0.0  ");
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;	
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, alpha_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, s1); n++;
      XtSetArg(args[n], XmNrecomputeSize, false); n++;
      XtSetArg(args[n], XmNshadowThickness, 0); n++;
      XtSetArg(args[n], XmNhighlightThickness, 0); n++;
      beta_number = XtCreateManagedWidget("beta-number", xmLabelWidgetClass, ab_form, args, n);
      XmStringFree(s1);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, beta_number); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, beta_number); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      XtSetArg(args[n], XmNmaximum, 100); n++;
      XtSetArg(args[n], XmNvalue, 0); n++;
      XtSetArg(args[n], XmNheight, 16); n++;

      XtSetArg(args[n], XmNdragCallback, n2 = make_callback_list(beta_drag_callback, NULL)); n++;
      XtSetArg(args[n], XmNvalueChangedCallback, n2); n++;
      beta_scale = XtCreateManagedWidget("beta-scale", xmScrollBarWidgetClass, ab_form, args, n);


      /* -------- WINDOW -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 30); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNtopPosition, 35); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, ab_frame); n++;
      XtSetArg(args[n], XmNborderWidth, FRAME_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      window_frame = XtCreateManagedWidget("window-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      n = attach_all_sides(args, n);
      window_form = XtCreateManagedWidget("window-form", xmFormWidgetClass, window_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      window_label = XtCreateManagedWidget("window", xmLabelWidgetClass, window_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomWidget, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, window_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopItemPosition, ((int)fft_window(ss) > 2) ? ((int)fft_window(ss) - 1) : ((int)fft_window(ss) + 1)); n++;
      window_list = XmCreateScrolledList(window_form, (char *)"window-list", args, n);

      XtVaSetValues(window_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
      for (i = 0; i < MUS_NUM_FFT_WINDOWS; i++)
	windows[i] = XmStringCreateLocalized((char *)mus_fft_window_name((mus_fft_window_t)i));

      XtVaSetValues(window_list, 
		    XmNitems, windows, 
		    XmNitemCount, MUS_NUM_FFT_WINDOWS, 
		    XmNvisibleItemCount, 8, 
		    NULL);
      for (i = 0; i < MUS_NUM_FFT_WINDOWS; i++) 
	XmStringFree(windows[i]);

      XtManageChild(window_list); 
      XtAddCallback(window_list, XmNbrowseSelectionCallback, window_browse_callback, NULL);


      /* -------- WAVELET -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, window_frame); n++;

      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 60); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNtopPosition, 35); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, ab_frame); n++;
      XtSetArg(args[n], XmNborderWidth, FRAME_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      wavelet_frame = XtCreateManagedWidget("wavelet-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      n = attach_all_sides(args, n);
      wavelet_form = XtCreateManagedWidget("wavelet-form", xmFormWidgetClass, wavelet_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      wavelet_label = XtCreateManagedWidget("wavelet", xmLabelWidgetClass, wavelet_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomWidget, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, wavelet_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      wavelet_list = XmCreateScrolledList(wavelet_form, (char *)"wavelet-list", args, n);

      XtVaSetValues(wavelet_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
      for (i = 0; i < NUM_WAVELETS; i++) 
	wavelets[i] = XmStringCreateLocalized((char *)wavelet_name(i));

      XtVaSetValues(wavelet_list, 
		    XmNitems, wavelets, 
		    XmNitemCount, NUM_WAVELETS, 
		    XmNvisibleItemCount, 8, 
		    NULL);
      for (i = 0; i < NUM_WAVELETS; i++) 
	XmStringFree(wavelets[i]);

      XtManageChild(wavelet_list); 
      XtAddCallback(wavelet_list, XmNbrowseSelectionCallback, wavelet_browse_callback, NULL);


      /* -------- TRANSFORM TYPE -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 30); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNbottomPosition, 35); n++;
      XtSetArg(args[n], XmNborderWidth, FRAME_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      type_frame = XtCreateManagedWidget("type-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      n = attach_all_sides(args, n);
      type_form = XtCreateManagedWidget("type-form", xmFormWidgetClass, type_frame, args, n);
      /* needed because XmFrame only accepts one child */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      type_label = XtCreateManagedWidget("type", xmLabelWidgetClass, type_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, type_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      type_list = XmCreateScrolledList(type_form, (char *)"type-list", args, n);

      XtVaSetValues(type_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
      make_transform_type_list();

      XtManageChild(type_list); 
      XtAddCallback(type_list, XmNbrowseSelectionCallback, transform_type_browse_callback, NULL);


      /* -------- SIZE -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, type_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 60); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
      XtSetArg(args[n], XmNbottomWidget, type_frame); n++;
      XtSetArg(args[n], XmNborderWidth, FRAME_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      size_frame = XtCreateManagedWidget("size-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      n = attach_all_sides(args, n);
      size_form = XtCreateManagedWidget("size-form", xmFormWidgetClass, size_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      size_label = XtCreateManagedWidget("size", xmLabelWidgetClass, size_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, size_label); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopItemPosition, (size_pos > 2) ? (size_pos - 2) : size_pos); n++;
      size_list = XmCreateScrolledList(size_form, (char *)"size-list", args, n);

      XtVaSetValues(size_list, XmNbackground, ss->white, XmNforeground, ss->black, NULL);
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++) 
	sizes[i] = XmStringCreateLocalized((char *)transform_size_names[i]);

      XtVaSetValues(size_list, 
		    XmNitems, sizes, 
		    XmNitemCount, NUM_TRANSFORM_SIZES, 
		    XmNvisibleItemCount, 6, 
		    NULL);
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++) 
	XmStringFree(sizes[i]);

      XtManageChild(size_list); 
      XtAddCallback(size_list, XmNbrowseSelectionCallback, size_browse_callback, NULL);


      /* -------- DISPLAY BOX BUTTONS -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, size_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNborderWidth, FRAME_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      display_frame = XtCreateManagedWidget("display-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->zoom_color); n++;
      n = attach_all_sides(args, n);
      display_form = XtCreateManagedWidget("display-form", xmFormWidgetClass, display_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      display_label = XtCreateManagedWidget("display", xmLabelWidgetClass, display_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"single transform");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"sonogram");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"spectrogram");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"peaks");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
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
      XtAddCallback(peak_txt, XmNlosingFocusCallback, fft_blue_textfield_unfocus_callback, NULL);
      XtAddEventHandler(peak_txt, LeaveWindowMask, false, fft_blue_mouse_leave_text_callback, NULL);
      XtAddEventHandler(peak_txt, EnterWindowMask, false, fft_white_mouse_enter_text_callback, NULL);
      widget_int_to_text(peak_txt, max_transform_peaks(ss));
      XtAddCallback(peak_txt, XmNactivateCallback, peaks_activate_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"dB");
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
      XtSetArg(args[n], XmNrightPosition, 67); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, peaks_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      db_button = make_togglebutton_widget("db-button", display_form, args, n);
      XtAddCallback(db_button, XmNvalueChangedCallback, fft_db_callback, NULL);
      XmStringFree(bstr);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
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
      XtAddCallback(db_txt, XmNlosingFocusCallback, fft_blue_textfield_unfocus_callback, NULL);
      XtAddEventHandler(db_txt, LeaveWindowMask, false, fft_blue_mouse_leave_text_callback, NULL);
      XtAddEventHandler(db_txt, EnterWindowMask, false, fft_white_mouse_enter_text_callback, NULL);
      widget_float_to_text(db_txt, min_dB(ss));
      XtAddCallback(db_txt, XmNactivateCallback, min_db_activate_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"log freq");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
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
      XtAddCallback(freq_base_txt, XmNlosingFocusCallback, fft_blue_textfield_unfocus_callback, NULL);
      XtAddEventHandler(freq_base_txt, LeaveWindowMask, false, fft_blue_mouse_leave_text_callback, NULL);
      XtAddEventHandler(freq_base_txt, EnterWindowMask, false, fft_white_mouse_enter_text_callback, NULL);
      widget_float_to_text(freq_base_txt, log_freq_start(ss));
      XtAddCallback(freq_base_txt, XmNactivateCallback, log_freq_start_activate_callback, NULL);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"normalize");
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
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"selection");
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, normalize_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      selection_button = make_togglebutton_widget("selection-button", display_form, args, n);
      XtAddCallback(selection_button, XmNvalueChangedCallback, selection_callback, NULL);
      XmStringFree(bstr);


      n = 0;
      XtSetArg(args[n], XmNbackground, ss->lighter_blue); n++;
      XtSetArg(args[n], XmNselectColor, ss->red); n++;
      bstr = XmStringCreateLocalized((char *)"with phases");
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, selection_button); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNlabelString, bstr); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING); n++;
      phases_button = make_togglebutton_widget("phases-button", display_form, args, n);
      XtAddCallback(phases_button, XmNvalueChangedCallback, phases_callback, NULL);
      XmStringFree(bstr);


      
      /* -------- GRAPH -------- */
      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNleftWidget, wavelet_frame); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, display_frame); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNborderWidth, FRAME_BORDER_WIDTH); n++;
      XtSetArg(args[n], XmNborderColor, ss->basic_color); n++;
      graph_frame = XtCreateManagedWidget("graph-frame", xmFrameWidgetClass, mainform, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      n = attach_all_sides(args, n);
      graph_form = XtCreateManagedWidget("graph-form", xmFormWidgetClass, graph_frame, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->highlight_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNalignment, XmALIGNMENT_CENTER); n++;
      graph_label = XtCreateManagedWidget("window", xmLabelWidgetClass, graph_form, args, n);
      /* label should change according to what is being displayed */

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, graph_label); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_NONE); n++;
      XtSetArg(args[n], XmNseparatorType, XmSHADOW_ETCHED_IN); n++;
      XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
      gsep = XtCreateManagedWidget("gsep", xmSeparatorWidgetClass, graph_form, args, n);

      n = 0;
      XtSetArg(args[n], XmNbackground, ss->graph_color); n++;
      XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
      XtSetArg(args[n], XmNtopWidget, gsep); n++;
      XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
      XtSetArg(args[n], XmNallowResize, true); n++;
      graph_drawer = XtCreateManagedWidget("graph-drawer", xmDrawingAreaWidgetClass, graph_form, args, n);

      gv.function = GXcopy;
      XtVaGetValues(graph_drawer, XmNbackground, &gv.background, XmNforeground, &gv.foreground, NULL);
      gc = XtGetGC(graph_drawer, GCForeground | GCFunction, &gv);

      gv.foreground = ss->enved_waveform_color;
      fgc = XtGetGC(graph_drawer, GCForeground | GCFunction, &gv);

      XmToggleButtonSetState(normo_button, (Boolean)(transform_graph_type(ss) == GRAPH_ONCE), false);
      XmToggleButtonSetState(sono_button, (Boolean)(transform_graph_type(ss) == GRAPH_AS_SONOGRAM), false);
      XmToggleButtonSetState(spectro_button, (Boolean)(transform_graph_type(ss) == GRAPH_AS_SPECTROGRAM), false);
      XmToggleButtonSetState(peaks_button, (Boolean)(show_transform_peaks(ss)), false);
      XmToggleButtonSetState(db_button, (Boolean)(fft_log_magnitude(ss)), false);
      XmToggleButtonSetState(logfreq_button, (Boolean)(fft_log_frequency(ss)), false);
      XmToggleButtonSetState(normalize_button, (Boolean)(transform_normalization(ss) != DONT_NORMALIZE), false);
      XmToggleButtonSetState(selection_button, (Boolean)(show_selection_transform(ss)), false);
      XmToggleButtonSetState(phases_button, (Boolean)(fft_with_phases(ss)), false);

      /* select current list choices */
      /* display current windowing choice unless wavelet in force */

      XmListSelectPos(type_list, transform_type_to_position(transform_type(ss)) + 1, false);
      XmListSelectPos(wavelet_list, wavelet_type(ss) + 1, false);
      XmListSelectPos(size_list, size_pos, false);
      XmListSelectPos(window_list, (int)fft_window(ss) + 1, false);

      if (spectrum_start(ss) != 0.0) set_spectrum_start_scale(spectrum_start(ss));
      if (spectrum_end(ss) != 1.0) set_spectrum_end_scale(spectrum_end(ss));
      if (fft_window_alpha(ss) != 0.0) set_alpha_scale(fft_window_alpha(ss));
      if (fft_window_beta(ss) != 0.0) set_beta_scale(fft_window_beta(ss));

      highlight_alpha_beta_scales(fft_window(ss));

      free(n1);
      free(n2);
      free(n3);
      free(n4);

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
      set_label(graph_label, mus_fft_window_name(fft_window(ss)));
      get_fft_window_data();
      XtAddCallback(graph_drawer, XmNresizeCallback, graph_resize_callback, NULL);
      XtAddCallback(graph_drawer, XmNexposeCallback, graph_resize_callback, NULL);
      need_callback = false;
    }
  return(transform_dialog);
}


bool transform_dialog_is_active(void)
{
  return((transform_dialog) && 
	 (XtIsManaged(transform_dialog)));
}
