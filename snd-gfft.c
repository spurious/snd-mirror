/* transform settings dialog */

#include "snd.h"

static GtkWidget *transform_dialog = NULL; /* main dialog shell */
static GtkWidget *transform_list, *size_list, *window_list, *wavelet_list,
                 *db_button, *peaks_button, *logfreq_button, *sono_button, *spectro_button, *normal_fft_button, *normalize_button, *selection_button,
                 *window_beta_scale, *graph_drawer = NULL, *graph_frame = NULL;
static GtkObject *beta_adj;
static GdkGC *gc = NULL, *fgc = NULL;
static bool ignore_callbacks;

#define GRAPH_SIZE 128
static Float current_graph_data[GRAPH_SIZE]; /* fft window graph in transform options dialog */
static Float current_graph_fftr[GRAPH_SIZE * 2];
static Float current_graph_ffti[GRAPH_SIZE * 2];

#define NUM_TRANSFORM_SIZES 14
static char *TRANSFORM_SIZES[NUM_TRANSFORM_SIZES] = 
  {"16", "32", "64", "128", "256", "512", "1024", "2048", "4096", "8192", "16384", "65536", "262144", "1048576    "};
static int transform_sizes[NUM_TRANSFORM_SIZES] = 
  {16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 65536, 262144, 1048576};

#if HAVE_GSL || HAVE_COMPLEX_TRIG
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

static Float fp_dB(Float py)
{
  return((py <= ss->lin_dB) ? 0.0 : (1.0 - ((20.0 * (log10(py))) / ss->min_dB)));
}

static axis_info *axis_ap = NULL;

static void graph_redisplay(void)
{
  GdkDrawable *wn;
  int ix0, iy0, ix1, iy1, i;
  Float xincr, x;
  axis_context *ax;
  if (!(transform_dialog_is_active())) return;
  if (graph_drawer == NULL) return;
  wn = graph_drawer->window;
  if (wn == NULL) return;
  if (!axis_ap)
    {
      axis_ap = (axis_info *)CALLOC(1, sizeof(axis_info));
      ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      axis_ap->ax = ax;
    }
  else
    {
      ax = axis_ap->ax;
      if (axis_ap->xlabel) FREE(axis_ap->xlabel);
    }
  ax->wn = graph_drawer->window;
  ax->w = graph_drawer;
  ax->gc = gc;
  ax->current_font = AXIS_NUMBERS_FONT(ss);
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
  gdk_window_clear(ax->wn);
  make_axes_1(axis_ap, X_AXIS_IN_SECONDS, 1, SHOW_ALL_AXES, false, true);
  ax->gc = gc;
  ix1 = grf_x(0.0, axis_ap);
  iy1 = grf_y(current_graph_data[0], axis_ap);
  xincr = 1.0 / (Float)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x += xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = grf_x(x, axis_ap);
      iy1 = grf_y(current_graph_data[i], axis_ap);
      gdk_draw_line(wn, gc, ix0, iy0, ix1, iy1);
    }

  ax->gc = fgc;
  ix1 = grf_x(0.0, axis_ap);
  iy1 = grf_y(current_graph_fftr[0], axis_ap);
  xincr = 1.0 / (Float)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x += xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = grf_x(x, axis_ap);
      if (fft_log_magnitude(ss))
	iy1 = grf_y(fp_dB(current_graph_fftr[i]), axis_ap);
      else iy1 = grf_y(current_graph_fftr[i], axis_ap);
      gdk_draw_line(wn, fgc, ix0, iy0, ix1, iy1);
    }
}

static void get_fft_window_data(void)
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
      if (fp->size < size) fp->ok = false; /* "dirty" flag for fft data array = needs REALLOCation */
      fp->size = size;
    }
}

static void gfft_size(int row)
{
  int size;
  for_each_chan(force_fft_clear);
  in_set_transform_size(transform_sizes[row]);
  size = transform_size(ss);
  for_each_chan_1(chans_transform_size, (void *)(&size));
  for_each_chan(calculate_fft);
  if (graph_frame) 
    gtk_frame_set_label(GTK_FRAME(graph_frame), 
			FFT_WINDOWS[(int)fft_window(ss)]);
  get_fft_window_data();
  graph_redisplay();
}

static void size_browse_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gint *indices;
  GtkTreePath *path;
  GtkTreeModel *model;
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  path = gtk_tree_model_get_path(model, &iter);
  indices = gtk_tree_path_get_indices(path);
  gfft_size(indices[0]);
  gtk_tree_path_free(path);
}

static void chans_wavelet_type(chan_info *cp, void *ptr) {cp->wavelet_type = (*((int *)ptr));}

static void gfft_wavelet(int row)
{
  in_set_wavelet_type(row);
  for_each_chan_1(chans_wavelet_type, (void *)(&row));
  if (transform_type(ss) == WAVELET)
    for_each_chan(calculate_fft);
}

static void wavelet_browse_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value;
  int i;
  GtkTreeModel *model;
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  for (i = 0; i < NUM_WAVELETS; i++)
    if (strcmp(value, WAVELETS[i]) == 0)
      {
	gfft_wavelet(i);
	return;
      }
}

static void gfft_window(int row)
{
  in_set_fft_window((mus_fft_window_t)row);
  for_each_chan(calculate_fft);
  if (graph_frame) 
    gtk_frame_set_label(GTK_FRAME(graph_frame), 
			FFT_WINDOWS[(int)fft_window(ss)]);
  get_fft_window_data();
  graph_redisplay();
}

static void window_browse_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value;
  int i;
  GtkTreeModel *model;
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  for (i = 0; i < NUM_FFT_WINDOWS; i++)
    if (strcmp(value, FFT_WINDOWS[i]) == 0)
      {
	gfft_window(i);
	return;
      }
}

static void chans_transform_type(chan_info *cp, void *ptr) {cp->transform_type = (*((int *)ptr));}

static void gfft_transform(int row)
{
  for_each_chan(force_fft_clear);
  in_set_transform_type(row);
  for_each_chan_1(chans_transform_type, (void *)(&row));
  for_each_chan(calculate_fft);
}

static void transform_browse_callback(GtkTreeSelection *selection, gpointer *gp)
{
  GtkTreeIter iter;
  gchar *value;
  int i;
  GtkTreeModel *model;
  if (!(gtk_tree_selection_get_selected(selection, &model, &iter))) return;
  gtk_tree_model_get(model, &iter, 0, &value, -1);
  for (i = 0; i < NUM_TRANSFORM_TYPES; i++)
    if (strcmp(value, TRANSFORM_TYPES[i]) == 0)
      {
	gfft_transform(i);
	return;
      }
}

static void normal_fft_callback(GtkWidget *w, gpointer context)
{
  if (ignore_callbacks) return;
  if (GTK_TOGGLE_BUTTON(w)->active)
    in_set_transform_graph_type(GRAPH_ONCE);
  else in_set_transform_graph_type(GRAPH_AS_SONOGRAM);
  for_each_chan(calculate_fft);
}

static void sonogram_callback(GtkWidget *w, gpointer context)
{
  if (ignore_callbacks) return;
  if (GTK_TOGGLE_BUTTON(w)->active)
    in_set_transform_graph_type(GRAPH_AS_SONOGRAM);
  else in_set_transform_graph_type(GRAPH_ONCE);
  for_each_chan(calculate_fft);
}

static void spectrogram_callback(GtkWidget *w, gpointer context)
{
  if (ignore_callbacks) return;
  if (GTK_TOGGLE_BUTTON(w)->active)
    in_set_transform_graph_type(GRAPH_AS_SPECTROGRAM);
  else in_set_transform_graph_type(GRAPH_ONCE);
  for_each_chan(calculate_fft);
}

static void map_show_transform_peaks(chan_info *cp, void *ptr) {cp->show_transform_peaks = (*((bool *)ptr));}

static void peaks_callback(GtkWidget *w, gpointer context)
{
  bool val = false;
  val = (bool)(GTK_TOGGLE_BUTTON(w)->active);
  in_set_show_transform_peaks(val);
  for_each_chan_1(map_show_transform_peaks, (void *)(&val));
  for_each_chan(calculate_fft);
}

static void chans_fft_log_magnitude(chan_info *cp, void *ptr) 
{
  cp->fft_log_magnitude = (*((bool *)ptr)); 
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void db_callback(GtkWidget *w, gpointer context)
{
  bool val;
  val = (bool)(GTK_TOGGLE_BUTTON(w)->active);
  in_set_fft_log_magnitude(val);
  for_each_chan_1(chans_fft_log_magnitude, (void *)(&val));
  for_each_chan(calculate_fft);
}

static void chans_fft_log_frequency(chan_info *cp, void *ptr) 
{
  cp->fft_log_frequency = (*((bool *)ptr)); 
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void logfreq_callback(GtkWidget *w, gpointer context)
{
  bool val;
  val = (bool)(GTK_TOGGLE_BUTTON(w)->active);
  in_set_fft_log_frequency(val);
  for_each_chan_1(chans_fft_log_frequency, (void *)(&val));
  for_each_chan(calculate_fft);
}

static void chans_transform_normalization(chan_info *cp, void *ptr) 
{
  cp->transform_normalization = (*((fft_normalize_t *)ptr)); 
  cp->fft_changed = FFT_CHANGE_LOCKED;
}

static void normalize_callback(GtkWidget *w, gpointer context)
{
  fft_normalize_t choice;
  if (GTK_TOGGLE_BUTTON(w)->active) choice = NORMALIZE_BY_CHANNEL; else choice = DONT_NORMALIZE;
  in_set_transform_normalization(choice);
  for_each_chan_1(chans_transform_normalization, (void *)(&choice));
  for_each_chan(calculate_fft);
}

static void selection_callback(GtkWidget *w, gpointer context)
{
  in_set_show_selection_transform(GTK_TOGGLE_BUTTON(w)->active);
  for_each_chan(calculate_fft);
}

static void beta_callback(GtkAdjustment *adj, gpointer context)
{
  in_set_fft_window_beta((Float)(adj->value));
  chans_field(FCP_BETA, (Float)(adj->value));
  if (fft_window_beta_in_use(fft_window(ss)))
    {
      get_fft_window_data();
      graph_redisplay();
      if (transform_type(ss) == FOURIER) 
	for_each_chan(calculate_fft);
    }
} 

static void graph_configure_callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  graph_redisplay();
}

static void graph_expose_callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  graph_redisplay();
}

static void dismiss_transform_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(transform_dialog);
}

static void orient_transform_callback(GtkWidget *w, gpointer context)
{
  start_orientation_dialog();
}

static void color_transform_callback(GtkWidget *w, gpointer context)
{
  start_color_dialog();
}

static void delete_transform_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(transform_dialog);
}

static void help_transform_callback(GtkWidget *w, gpointer context)
{
  transform_dialog_help();
}

#define BUTTON_HEIGHT 20
#define BUTTON_WIDTH 40
/* for some reason gtk puts a mile and a half of padding around buttons */

GtkWidget *fire_up_transform_dialog(bool managed)
{
  GtkWidget *outer_table, *buttons;
  int i;
  bool need_callback = false;
  GtkWidget *display_frame, *help_button, *dismiss_button;
  GtkWidget *window_box, *orient_button, *color_button;

  if (!transform_dialog)
    {
      transform_dialog = gtk_dialog_new();
      g_signal_connect_closure_by_id(GTK_OBJECT(transform_dialog),
				     g_signal_lookup("delete_event", G_OBJECT_TYPE(GTK_OBJECT(transform_dialog))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(delete_transform_dialog), NULL, 0),
				     0);
      gtk_window_set_title(GTK_WINDOW(transform_dialog), _("Transform Options"));
      sg_make_resizable(transform_dialog);
      gtk_container_set_border_width(GTK_CONTAINER(transform_dialog), 4);
      gtk_widget_realize(transform_dialog);
      gtk_window_resize(GTK_WINDOW(transform_dialog), 400, 500);

      help_button = gtk_button_new_with_label(_("Help"));
      dismiss_button = gtk_button_new_with_label(_("Dismiss"));
      color_button = gtk_button_new_with_label(_("Color"));
      orient_button = gtk_button_new_with_label(_("Orientation"));
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(transform_dialog)->action_area), dismiss_button, false, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(transform_dialog)->action_area), color_button, false, true, 10);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(transform_dialog)->action_area), orient_button, false, true, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(transform_dialog)->action_area), help_button, false, true, 10);
      g_signal_connect_closure_by_id(GTK_OBJECT(dismiss_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(dismiss_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(dismiss_transform_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(color_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(color_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(color_transform_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(orient_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(orient_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(orient_transform_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(help_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(help_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(help_transform_callback), NULL, 0),
				     0);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(color_button);
      gtk_widget_show(orient_button);
      gtk_widget_show(help_button);

      outer_table = gtk_table_new(2, 3, false);
      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(transform_dialog)->vbox), outer_table);
      gtk_table_set_row_spacings(GTK_TABLE(outer_table), 4);
      gtk_table_set_col_spacings(GTK_TABLE(outer_table), 4);

      /* now 6 boxes within the main box:
	 type (list)    |  size (list)        |  display (button column)
	 wavelet (list) |  window (list+beta) |  graph (fft?) of current window
      */

      /* TYPE */
      {
	char **transform_names;
	transform_names = (char **)CALLOC(num_transform_types, sizeof(char *));
	for (i = 0; i < num_transform_types; i++) 
	  {
	    if (i < NUM_TRANSFORM_TYPES)
	      transform_names[i] = TRANSFORM_TYPES[i];
	    else transform_names[i] = added_transform_name(i);
	  }
	transform_list = sg_make_list(_("type"), outer_table, TABLE_ATTACH, NULL, num_transform_types, transform_names, 
				      GTK_SIGNAL_FUNC(transform_browse_callback), 0, 1, 0, 1);
	gtk_widget_show(transform_list);
	FREE(transform_names);
      }

      /* SIZE */
      size_list = sg_make_list(_("size"), outer_table, TABLE_ATTACH, NULL, NUM_TRANSFORM_SIZES, TRANSFORM_SIZES, 
			       GTK_SIGNAL_FUNC(size_browse_callback), 1, 2, 0, 1);
      gtk_widget_show(size_list);

      /* DISPLAY */
      display_frame = gtk_frame_new(_("display"));
      /* gtk_table_attach_defaults(GTK_TABLE(outer_table), display_frame, 2, 3, 0, 1); */
      gtk_table_attach(GTK_TABLE(outer_table), display_frame, 2, 3, 0, 1,
		       (GtkAttachOptions)(GTK_FILL | GTK_SHRINK), 
		       (GtkAttachOptions)(GTK_FILL | GTK_SHRINK),
		       0, 0);
      gtk_frame_set_label_align(GTK_FRAME(display_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(display_frame), GTK_SHADOW_ETCHED_IN);

      buttons = gtk_vbox_new(false, 0);
      gtk_container_add(GTK_CONTAINER(display_frame), buttons);

      normal_fft_button = gtk_radio_button_new_with_label(NULL, _("single transform"));
      gtk_box_pack_start(GTK_BOX(buttons), normal_fft_button, false, false, 0);
      gtk_widget_show(normal_fft_button);
      g_signal_connect_closure_by_id(GTK_OBJECT(normal_fft_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(normal_fft_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(normal_fft_callback), NULL, 0),
				     0);
      gtk_widget_set_size_request(GTK_WIDGET(normal_fft_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      sono_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(normal_fft_button)), _("sonogram"));
      gtk_box_pack_start(GTK_BOX(buttons), sono_button, false, false, 0);
      gtk_widget_show(sono_button);
      g_signal_connect_closure_by_id(GTK_OBJECT(sono_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(sono_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(sonogram_callback), NULL, 0),
				     0);
      gtk_widget_set_size_request(GTK_WIDGET(sono_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      spectro_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(normal_fft_button)), _("spectrogram"));
      gtk_box_pack_start(GTK_BOX(buttons), spectro_button, false, false, 0);
      gtk_widget_show(spectro_button);
      g_signal_connect_closure_by_id(GTK_OBJECT(spectro_button),
				     g_signal_lookup("clicked", G_OBJECT_TYPE(GTK_OBJECT(spectro_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(spectrogram_callback), NULL, 0),
				     0);
      gtk_widget_set_size_request(GTK_WIDGET(spectro_button), BUTTON_WIDTH, BUTTON_HEIGHT);
      
      peaks_button = gtk_check_button_new_with_label(_("peaks"));
      gtk_box_pack_start(GTK_BOX(buttons), peaks_button, false, false, 0);
      gtk_widget_show(peaks_button);
      g_signal_connect_closure_by_id(GTK_OBJECT(peaks_button),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(peaks_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(peaks_callback), NULL, 0),
				     0);
      gtk_widget_set_size_request(GTK_WIDGET(peaks_button), BUTTON_WIDTH, BUTTON_HEIGHT);
 
      db_button = gtk_check_button_new_with_label(_("dB"));
      gtk_box_pack_start(GTK_BOX(buttons), db_button, false, false, 0);
      gtk_widget_show(db_button);
      g_signal_connect_closure_by_id(GTK_OBJECT(db_button),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(db_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(db_callback), NULL, 0),
				     0);
      gtk_widget_set_size_request(GTK_WIDGET(db_button), BUTTON_WIDTH, BUTTON_HEIGHT);
 
      logfreq_button = gtk_check_button_new_with_label(_("log freq"));
      gtk_box_pack_start(GTK_BOX(buttons), logfreq_button, false, false, 0);
      gtk_widget_show(logfreq_button);
      g_signal_connect_closure_by_id(GTK_OBJECT(logfreq_button),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(logfreq_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(logfreq_callback), NULL, 0),
				     0);
      gtk_widget_set_size_request(GTK_WIDGET(logfreq_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      normalize_button = gtk_check_button_new_with_label(_("normalize"));
      gtk_box_pack_start(GTK_BOX(buttons), normalize_button, false, false, 0);
      gtk_widget_show(normalize_button);
      g_signal_connect_closure_by_id(GTK_OBJECT(normalize_button),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(normalize_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(normalize_callback), NULL, 0),
				     0);
      gtk_widget_set_size_request(GTK_WIDGET(normalize_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      selection_button = gtk_check_button_new_with_label(_("selection"));
      gtk_box_pack_start(GTK_BOX(buttons), selection_button, false, false, 0);
      gtk_widget_show(selection_button);
      g_signal_connect_closure_by_id(GTK_OBJECT(selection_button),
				     g_signal_lookup("toggled", G_OBJECT_TYPE(GTK_OBJECT(selection_button))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(selection_callback), NULL, 0),
				     0);
      gtk_widget_set_size_request(GTK_WIDGET(selection_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      gtk_widget_show(buttons);
      gtk_widget_show(display_frame);

      /* WAVELET */
      wavelet_list = sg_make_list(_("wavelet"), outer_table, TABLE_ATTACH, NULL, NUM_WAVELETS, WAVELETS, 
				  GTK_SIGNAL_FUNC(wavelet_browse_callback), 0, 1, 1, 2);
      gtk_widget_show(wavelet_list);

      /* WINDOW */
      window_box = gtk_table_new(2, 2, false);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), window_box, 1, 2, 1, 2);
      window_list = sg_make_list(_("window"), window_box, TABLE_ATTACH, NULL, GUI_NUM_FFT_WINDOWS, FFT_WINDOWS, 
				 GTK_SIGNAL_FUNC(window_browse_callback), 0, 1, 0, 1);

      beta_adj = gtk_adjustment_new(0.0, 0.0, 1.01, 0.001, 0.01, .01);
      window_beta_scale = gtk_hscale_new(GTK_ADJUSTMENT(beta_adj));
      GTK_WIDGET_UNSET_FLAGS(window_beta_scale, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(window_beta_scale)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(window_beta_scale), 2);
      gtk_scale_set_value_pos(GTK_SCALE(window_beta_scale), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(window_beta_scale), true);
      g_signal_connect_closure_by_id(GTK_OBJECT(beta_adj),
				     g_signal_lookup("value_changed", G_OBJECT_TYPE(GTK_OBJECT(beta_adj))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(beta_callback), NULL, 0),
				     0);
      gtk_table_attach(GTK_TABLE(window_box), window_beta_scale, 0, 1, 1, 2,
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		       (GtkAttachOptions)(GTK_FILL), 
		       0, 0);

      gtk_widget_show(window_beta_scale);
      gtk_widget_show(window_list);
      gtk_widget_show(window_box);


      /* GRAPH */
      graph_frame = gtk_frame_new(FFT_WINDOWS[(int)fft_window(ss)]);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), graph_frame, 2, 3, 1, 2);
      gtk_frame_set_label_align(GTK_FRAME(graph_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(graph_frame), GTK_SHADOW_ETCHED_IN);

      graph_drawer = gtk_drawing_area_new();
      gtk_container_add(GTK_CONTAINER(graph_frame), graph_drawer);
      gtk_widget_modify_bg(graph_drawer, GTK_STATE_NORMAL, (ss->sgx)->white);

      fgc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(fgc, (ss->sgx)->white);
      gdk_gc_set_foreground(fgc, (ss->sgx)->enved_waveform_color);

      gc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(gc, (ss->sgx)->white);
      gdk_gc_set_foreground(gc, (ss->sgx)->black);

      gtk_widget_show(graph_drawer);
      gtk_widget_show(graph_frame);

      sg_list_select(transform_list, transform_type(ss));
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++)
	if (transform_sizes[i] == transform_size(ss))
	  {
	    sg_list_select(size_list, i);
	    sg_list_moveto(size_list, i);
	    break;
	  }

      ignore_callbacks = true;
      if (transform_graph_type(ss) == GRAPH_ONCE) set_toggle_button(normal_fft_button, true, false, NULL);
      if (transform_graph_type(ss) == GRAPH_AS_SONOGRAM) set_toggle_button(sono_button, true, false, NULL);
      if (transform_graph_type(ss) == GRAPH_AS_SPECTROGRAM) set_toggle_button(spectro_button, true, false, NULL);
      ignore_callbacks = false;

      set_toggle_button(peaks_button, show_transform_peaks(ss), false, NULL);
      set_toggle_button(db_button, fft_log_magnitude(ss), false, NULL);
      set_toggle_button(logfreq_button, fft_log_frequency(ss), false, NULL);
      set_toggle_button(normalize_button, (transform_normalization(ss) != DONT_NORMALIZE), false, NULL);
      set_toggle_button(selection_button, show_selection_transform(ss), false, NULL);
      sg_list_select(window_list, (int)fft_window(ss));
      sg_list_moveto(window_list, (int)fft_window(ss));
      sg_list_select(wavelet_list, wavelet_type(ss));
      sg_list_moveto(wavelet_list, wavelet_type(ss));
      need_callback = true;
      gtk_widget_show(outer_table);
      set_dialog_widget(TRANSFORM_DIALOG, transform_dialog);
    }
  else raise_dialog(transform_dialog);
  gtk_widget_show(transform_dialog);

  if (need_callback)
    {
      get_fft_window_data();
      g_signal_connect_closure_by_id(GTK_OBJECT(graph_drawer),
				     g_signal_lookup("expose_event", G_OBJECT_TYPE(GTK_OBJECT(graph_drawer))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(graph_expose_callback), NULL, 0),
				     0);
      g_signal_connect_closure_by_id(GTK_OBJECT(graph_drawer),
				     g_signal_lookup("configure_event", G_OBJECT_TYPE(GTK_OBJECT(graph_drawer))),
				     0,
				     g_cclosure_new(GTK_SIGNAL_FUNC(graph_configure_callback), NULL, 0),
				     0);
      need_callback = false;
    }
  return(transform_dialog);
}

bool transform_dialog_is_active(void)
{
  return((transform_dialog) && 
	 (GTK_WIDGET_VISIBLE(transform_dialog)));
}

void set_fft_window_beta(Float val)
{
  in_set_fft_window_beta(val);
  chans_field(FCP_BETA, val);
  if (transform_dialog) 
    {
      gtk_adjustment_set_value(GTK_ADJUSTMENT(beta_adj), val);
      get_fft_window_data();
      graph_redisplay();
    }
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
}

void set_transform_size(int val)
{
  int i;
  for_each_chan(force_fft_clear);
  in_set_transform_size(val);
  for_each_chan_1(chans_transform_size, (void *)(&val));
  if (transform_dialog)
    {
      for (i = 0; i < NUM_TRANSFORM_SIZES; i++)
	if (transform_sizes[i] == val)
	  {
	    sg_list_select(size_list, i);
	    sg_list_moveto(size_list, i);
	    break;
	  }
    }
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
}

void set_fft_window(mus_fft_window_t val)
{
  in_set_fft_window(val);
  if (!(ss->graph_hook_active)) for_each_chan(calculate_fft);
  if ((transform_dialog) && (graph_drawer))
    {
      sg_list_select(window_list, (int)val);
      sg_list_moveto(window_list, (int)val);
      if (graph_frame) 
	gtk_frame_set_label(GTK_FRAME(graph_frame),
			    FFT_WINDOWS[val]);
      get_fft_window_data();
      graph_redisplay();
    }
}
  
void set_transform_type(int val)
{
  if (!(ss->graph_hook_active)) for_each_chan(force_fft_clear);
  in_set_transform_type(val);
  for_each_chan_1(chans_transform_type, (void *)(&val));
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
  if (transform_dialog) 
    sg_list_select(transform_list, val);
}

void set_wavelet_type(int val)
{
  if (transform_dialog) 
    {
      sg_list_select(wavelet_list, val);
      sg_list_moveto(wavelet_list, val);
    }
  in_set_wavelet_type(val);
  for_each_chan_1(chans_wavelet_type, (void *)(&val));
  if ((transform_type(ss) == WAVELET) && 
      (!(ss->graph_hook_active))) 
    for_each_chan(calculate_fft);
}

/* various set- cases need to be reflected in the transform dialog */
void set_show_transform_peaks(bool val)
{
  in_set_show_transform_peaks(val);
  for_each_chan_1(map_show_transform_peaks, (void *)(&val));
  if (transform_dialog) 
    set_toggle_button(peaks_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_fft_log_frequency(bool val)
{
  in_set_fft_log_frequency(val);
  for_each_chan_1(chans_fft_log_frequency, (void *)(&val));
  if (transform_dialog)
    set_toggle_button(logfreq_button, val, false, NULL);
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_fft_log_magnitude(bool val)
{
  in_set_fft_log_magnitude(val);
  for_each_chan_1(chans_fft_log_magnitude, (void *)(&val));
  if (transform_dialog) 
    set_toggle_button(db_button, val, false, NULL);
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
	set_toggle_button(normal_fft_button, true, false, NULL); 
	break;
      case GRAPH_AS_SONOGRAM:   
	set_toggle_button(sono_button, true, false, NULL); 
	break;
      case GRAPH_AS_SPECTROGRAM:
	set_toggle_button(spectro_button, true, false, NULL);
	break;
      case GRAPH_AS_WAVOGRAM:
	break;
      }
  if (!(ss->graph_hook_active)) 
    for_each_chan(calculate_fft);
}

void set_transform_normalization(fft_normalize_t val)
{
  in_set_transform_normalization(val);
  for_each_chan_1(chans_transform_normalization, (void *)(&val));
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


int add_transform_to_list(char *name)
{
  /* put at end of list and return associated browse callback row */
  if (transform_dialog)
    sg_list_append(transform_list, name);
  return(num_transform_types++);
}

