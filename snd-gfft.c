/* transform settings dialog */

#include "snd.h"

static GtkWidget *transform_dialog = NULL; /* main dialog shell */
static GtkWidget *transform_list, *size_list, *window_list, *wavelet_list,
                 *db_button, *peaks_button, *logfreq_button, *sono_button, *spectro_button, *normal_fft_button, *normalize_button, *selection_button,
                 *window_beta_scale, *graph_drawer = NULL, *graph_frame = NULL;
static GtkObject *beta_adj;
static GdkGC *gc, *fgc;

#define GRAPH_SIZE 128
static Float current_graph_data[GRAPH_SIZE]; /* fft window graph in transform options dialog */
static Float current_graph_fftr[GRAPH_SIZE*2];
static Float current_graph_ffti[GRAPH_SIZE*2];

#define NUM_FFT_SIZES 14
static char *FFT_SIZES[NUM_FFT_SIZES] = {"16", "32", "64", "128", "256", "512", "1024", "2048", "4096", "8192", "16384", "65536", "262144", "1048576    "};
static int fft_sizes[NUM_FFT_SIZES] = {16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 65536, 262144, 1048576};

#if HAVE_GSL
  #define GUI_NUM_FFT_WINDOWS NUM_FFT_WINDOWS
#else
  #define GUI_NUM_FFT_WINDOWS (NUM_FFT_WINDOWS - 1)
#endif

static char *FFT_WINDOWS[NUM_FFT_WINDOWS] = 
     {"Rectangular", "Hanning", "Welch", "Parzen", "Bartlett", "Hamming", "Blackman2", "Blackman3", "Blackman4",
      "Exponential", "Riemann", "Kaiser", "Cauchy", "Poisson", "Gaussian", "Tukey", "Dolph-Chebyshev"};

static char *WAVELETS[NUM_WAVELETS] ={
  "daub4", "daub6", "daub8", "daub10", "daub12", "daub14", "daub16", "daub18", "daub20",
  "battle_lemarie", "burt_adelson", "beylkin", "coif2", "coif4", "coif6",
  "sym2", "sym3", "sym4", "sym5", "sym6"};

#define NUM_TRANSFORM_TYPES 9
static char *TRANSFORM_TYPES[NUM_TRANSFORM_TYPES] ={"Fourier", "Wavelet", "Hankel", "Walsh", "Autocorrelate", "Chebychev", "Cepstrum", "Hadamard", "Haar"};
static int num_transform_types = NUM_TRANSFORM_TYPES;

static char *TRANSFORM_TYPE_CONSTANTS[NUM_TRANSFORM_TYPES] ={
  S_fourier_transform, S_wavelet_transform, S_hankel_transform, S_walsh_transform,
  S_autocorrelation, S_chebyshev_transform, S_cepstrum, S_hadamard_transform, S_haar_transform};

char *transform_type_name(int choice)
{
  if (choice < NUM_TRANSFORM_TYPES)
    return(TRANSFORM_TYPE_CONSTANTS[choice]);
  else return(added_transform_name(choice));
}

int max_transform_type(void) {return(num_transform_types - 1);}

static chan_info *axis_cp = NULL;
static axis_context *make_axis_cp(snd_state *ss, GtkWidget *w)
{
  /* conjure up minimal context for axis drawer in snd-axis.c */
  /* almost the same as the same-named function in snd-xenv.c, but doesn't seem worth the trouble of making one function for both */
  axis_info *ap;
  axis_context *ax;
  if (!axis_cp)
    {
      /* axis_cp is just a dummy chan_info struct to hold the axis_info pointer */
      axis_cp = (chan_info *)CALLOC(1, sizeof(chan_info));
      axis_cp->printing = 0;
      axis_cp->state = ss;
      ap = (axis_info *)CALLOC(1, sizeof(axis_info));
      axis_cp->axis = ap;
      ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      ap->ax = ax;
      ap->ss = ss;
      ap->cp = axis_cp;
      ax->ss = ss;
    }
  else
    {
      ap = axis_cp->axis;
      ax = ap->ax;
      if (ap->xlabel) FREE(ap->xlabel);
    }
  ax->wn = graph_drawer->window;
  ax->gc = gc;
  ax->current_font = AXIS_NUMBERS_FONT(ss);
  ap->xmin = 0.0;
  ap->xmax = 1.0;
  ap->ymin = 0.0;
  ap->ymax = 1.0;
  ap->y_ambit = 1.0;
  ap->x_ambit = 1.0;
  ap->xlabel = NULL;
  ap->x0 = 0.0;
  ap->x1 = 1.0;
  ap->y0 = 0.0;
  ap->y1 = 1.0;
  ap->width = widget_width(graph_drawer);
  ap->window_width = ap->width;
  ap->y_offset = 0;
  ap->height = widget_height(graph_drawer);
  ap->graph_x0 = 0;
  gdk_window_clear(ax->wn);
  make_axes_1(axis_cp, ap, X_IN_SECONDS, 1);
  return(ax);
}

static Float fp_dB(snd_state *ss, Float py)
{
  return((py <= ss->lin_dB) ? 0.0 : (1.0 - ((20.0 * (log10(py))) / ss->min_dB)));
}

static void graph_redisplay(snd_state *ss)
{
  GdkDrawable *wn;
  int ix0, iy0, ix1, iy1, i;
  Float xincr, x;
  axis_context *ax;
  if (!(transform_dialog_is_active())) return;
  if (graph_drawer == NULL) return;
  wn = graph_drawer->window;
  if (wn == NULL) return;
  ax = make_axis_cp(ss, graph_drawer);

  ax->gc = gc;
  ix1 = grf_x(0.0, axis_cp->axis);
  iy1 = grf_y(current_graph_data[0], axis_cp->axis);
  xincr = 1.0 / (Float)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x+=xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = grf_x(x, axis_cp->axis);
      iy1 = grf_y(current_graph_data[i], axis_cp->axis);
      gdk_draw_line(wn, gc, ix0, iy0, ix1, iy1);
    }

  ax->gc = fgc;
  ix1 = grf_x(0.0, axis_cp->axis);
  iy1 = grf_y(current_graph_fftr[0], axis_cp->axis);
  xincr = 1.0 / (Float)GRAPH_SIZE;

  for (i = 1, x = xincr; i < GRAPH_SIZE; i++, x+=xincr)
    {
      ix0 = ix1;
      iy0 = iy1;
      ix1 = grf_x(x, axis_cp->axis);
      if (fft_log_magnitude(ss))
	iy1 = grf_y(fp_dB(ss, current_graph_fftr[i]), axis_cp->axis);
      else iy1 = grf_y(current_graph_fftr[i], axis_cp->axis);
      gdk_draw_line(wn, fgc, ix0, iy0, ix1, iy1);
    }
}

static void get_fft_window_data(snd_state *ss)
{
  int i;
  make_fft_window_1(current_graph_data, GRAPH_SIZE, fft_window(ss), fft_beta(ss));
#if HAVE_MEMSET
  memset((void *)current_graph_fftr, 0, GRAPH_SIZE * 2 * sizeof(Float));
  memset((void *)current_graph_ffti, 0, GRAPH_SIZE * 2 * sizeof(Float));
#else
  for (i = 0; i < GRAPH_SIZE * 2; i++)
    {
      current_graph_fftr[i] = 0.0;
      current_graph_ffti[i] = 0.0;
    }
#endif
#if HAVE_MEMMOVE
  memmove((void *)current_graph_fftr, (void *)current_graph_data, GRAPH_SIZE * sizeof(Float));
#else
  for (i = 0; i < GRAPH_SIZE; i++)
    current_graph_fftr[i] = current_graph_data[i];
#endif
  mus_spectrum(current_graph_fftr, current_graph_ffti, NULL, GRAPH_SIZE * 2, 0);
  for (i = 0; i < GRAPH_SIZE; i++)
    current_graph_fftr[i] = (current_graph_fftr[i] + 80.0) / 80.0;
}

static int map_chans_fft_size(chan_info *cp, void *ptr) 
{
  fft_info *fp;
  cp->fft_size = (int)ptr; 
  if (cp->fft) 
    {
      fp = cp->fft;
      if (fp->size < (int)ptr) fp->ok = 0; /* "dirty" flag for fft data array = needs REALLOCation */
      fp->size = (int)ptr;
    }
  return(0);
}

static void size_browse_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  int size;
  in_set_fft_size(ss, fft_sizes[row]);
  size = fft_size(ss);
  map_over_chans(ss, map_chans_fft_size, (void *)size);
  map_over_chans(ss, calculate_fft, NULL);
  if (graph_frame) 
    gtk_frame_set_label(GTK_FRAME(graph_frame), 
			FFT_WINDOWS[fft_window(ss)]);
  get_fft_window_data(ss);
  graph_redisplay(ss);
}

static int map_chans_wavelet_type(chan_info *cp, void *ptr) {cp->wavelet_type = (int)ptr; return(0);}

static void wavelet_browse_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_wavelet_type(ss, row);
  map_over_chans(ss, map_chans_wavelet_type, (void *)row);
  if (transform_type(ss) == WAVELET)
    map_over_chans(ss, calculate_fft, NULL);
}

static void window_browse_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_fft_window(ss, row);
  map_over_chans(ss, calculate_fft, NULL);
  if (graph_frame) 
    gtk_frame_set_label(GTK_FRAME(graph_frame), 
			FFT_WINDOWS[fft_window(ss)]);
  get_fft_window_data(ss);
  graph_redisplay(ss);
  if (!(ss->using_schemes))
    {
      if (fft_window_beta_in_use(fft_window(ss)))
	set_background(window_beta_scale, (ss->sgx)->highlight_color);
      else set_background(window_beta_scale, (ss->sgx)->basic_color);
    }
}

static int map_chans_transform_type(chan_info *cp, void *ptr) {cp->transform_type = (int)ptr; return(0);}

static void transform_browse_Callback(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  map_over_chans(ss, force_fft_clear, NULL);
  in_set_transform_type(ss, row);
  map_over_chans(ss, map_chans_transform_type, (void *)row);
  map_over_chans(ss, calculate_fft, NULL);
}

static void normal_fft_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  if (GTK_TOGGLE_BUTTON(w)->active)
    in_set_fft_style(ss, NORMAL_FFT);
  else in_set_fft_style(ss, SONOGRAM);
  map_over_chans(ss, calculate_fft, NULL);
}

static void sonogram_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  if (GTK_TOGGLE_BUTTON(w)->active)
    in_set_fft_style(ss, SONOGRAM);
  else in_set_fft_style(ss, NORMAL_FFT);
  map_over_chans(ss, calculate_fft, NULL);
}

static void spectrogram_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  if (GTK_TOGGLE_BUTTON(w)->active)
    in_set_fft_style(ss, SPECTROGRAM);
  else in_set_fft_style(ss, NORMAL_FFT);
  map_over_chans(ss, calculate_fft, NULL);
}

static int map_show_fft_peaks(chan_info *cp, void *ptr) {cp->show_fft_peaks = (int)ptr; return(0);}

static void peaks_Callback(GtkWidget *w, gpointer context)
{
  int val = 0;
  snd_state *ss = (snd_state *)context;
  in_set_show_fft_peaks(ss, val = (GTK_TOGGLE_BUTTON(w)->active));
  map_over_chans(ss, map_show_fft_peaks, (void *)val);
  map_over_chans(ss, calculate_fft, NULL);
}

static int map_chans_fft_log_magnitude(chan_info *cp, void *ptr) {cp->fft_log_magnitude = (int)ptr; return(0);}

static void db_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  int val;
  in_set_fft_log_magnitude(ss, val = (GTK_TOGGLE_BUTTON(w)->active));
  map_over_chans(ss, map_chans_fft_log_magnitude, (void *)val);
  map_over_chans(ss, calculate_fft, NULL);
}

static int map_chans_fft_log_frequency(chan_info *cp, void *ptr) {cp->fft_log_frequency = (int)ptr; return(0);}

static void logfreq_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  int val;
  in_set_fft_log_frequency(ss, val = (GTK_TOGGLE_BUTTON(w)->active));
  map_over_chans(ss, map_chans_fft_log_frequency, (void *)val);
  map_over_chans(ss, calculate_fft, NULL);
}

static int map_chans_normalize_fft(chan_info *cp, void *ptr) {cp->normalize_fft = (int)ptr; return(0);}

static void normalize_Callback(GtkWidget *w, gpointer context)
{
  int choice;
  snd_state *ss = (snd_state *)context;
  choice = GTK_TOGGLE_BUTTON(w)->active;
  in_set_normalize_fft(ss, choice);
  map_over_chans(ss, map_chans_normalize_fft, (void *)choice);
  map_over_chans(ss, calculate_fft, NULL);
}

static void selection_Callback(GtkWidget *w, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_show_selection_transform(ss, GTK_TOGGLE_BUTTON(w)->active);
  map_over_chans(ss, calculate_fft, NULL);
}

static void beta_Callback(GtkAdjustment *adj, gpointer context)
{
  snd_state *ss = (snd_state *)context;
  in_set_fft_beta(ss, (Float)(adj->value));
  map_chans_field(ss, FCP_BETA, (Float)(adj->value));
  if (fft_window_beta_in_use(fft_window(ss)))
    {
      get_fft_window_data(ss);
      graph_redisplay(ss);
      if (transform_type(ss) == FOURIER) 
	map_over_chans(ss, calculate_fft, NULL);
    }
} 

static void graph_configure_Callback(GtkWidget *w, GdkEventConfigure *ev, gpointer data)
{
  graph_redisplay((snd_state *)data);
}

static void graph_expose_Callback(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  graph_redisplay((snd_state *)data);
}

static void Dismiss_Transform_Callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(transform_dialog);
}

static void delete_transform_dialog(GtkWidget *w, GdkEvent *event, gpointer context)
{
  gtk_widget_hide(transform_dialog);
}

static void Help_Transform_Callback(GtkWidget *w, gpointer context)
{
  transform_dialog_help((snd_state *)context);
}

static void button_pushed_red(GtkWidget *w, snd_state *ss)
{
  GtkStyle *style;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->bg[GTK_STATE_ACTIVE] = (*((ss->sgx)->red));
  style->bg[GTK_STATE_SELECTED] = (*((ss->sgx)->red));
  style->bg[GTK_STATE_NORMAL] = (*((ss->sgx)->basic_color));
  gtk_widget_set_style(w, style);
}

#define BUTTON_HEIGHT 20
#define BUTTON_WIDTH 40
/* for some reason gtk puts a mile and a half of padding around buttons */

void fire_up_transform_dialog(snd_state *ss)
{
  GtkWidget *outer_table, *buttons;
  char *str;
  int i, need_callback = 0;
  GtkWidget *type_frame, *size_frame, *display_frame, *window_frame, *wavelet_frame, *help_button, *dismiss_button;
  GtkWidget *type_scroller, *size_scroller, *window_scroller, *wavelet_scroller, *window_box;

  if (!transform_dialog)
    {
      transform_dialog = gtk_dialog_new();
      set_dialog_widget(TRANSFORM_DIALOG, transform_dialog);
      gtk_signal_connect(GTK_OBJECT(transform_dialog), "delete_event", GTK_SIGNAL_FUNC(delete_transform_dialog), (gpointer)ss);
      gtk_window_set_title(GTK_WINDOW(transform_dialog), STR_Transform_Options);
      gtk_window_set_policy(GTK_WINDOW(transform_dialog), TRUE, TRUE, FALSE); /* allow shrink or grow */
      set_background(transform_dialog, (ss->sgx)->basic_color);
      gtk_container_set_border_width(GTK_CONTAINER(transform_dialog), 4);
      gtk_widget_realize(transform_dialog);
      add_dialog(ss, transform_dialog);
      gtk_widget_set_usize(GTK_WIDGET(transform_dialog), 400, 350);

      help_button = gtk_button_new_with_label(STR_Help);
      dismiss_button = gtk_button_new_with_label(STR_Dismiss);
      gtk_box_pack_start(GTK_BOX(GTK_DIALOG(transform_dialog)->action_area), dismiss_button, FALSE, TRUE, 10);
      gtk_box_pack_end(GTK_BOX(GTK_DIALOG(transform_dialog)->action_area), help_button, FALSE, TRUE, 10);
      gtk_signal_connect(GTK_OBJECT(dismiss_button), "clicked", GTK_SIGNAL_FUNC(Dismiss_Transform_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(help_button), "clicked", GTK_SIGNAL_FUNC(Help_Transform_Callback), (gpointer)ss);
      set_pushed_button_colors(help_button, ss);
      set_pushed_button_colors(dismiss_button, ss);
      gtk_widget_show(dismiss_button);
      gtk_widget_show(help_button);

      outer_table = gtk_table_new(2, 3, FALSE);
      gtk_container_add(GTK_CONTAINER(GTK_DIALOG(transform_dialog)->vbox), outer_table);
      gtk_table_set_row_spacings(GTK_TABLE(outer_table), 4);
      gtk_table_set_col_spacings(GTK_TABLE(outer_table), 4);

      /* now 6 boxes within the main box:
	 
	 type (list)    |  size (list)        |  display (button column)
	 wavelet (list) |  window (list+beta) |  graph (fft?) of current window
	 
	 each box has a frame, label, and contents
      */

      /* TYPE */
      type_frame = gtk_frame_new(STR_type);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), type_frame, 0, 1, 0, 1);
      gtk_frame_set_label_align(GTK_FRAME(type_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(type_frame), GTK_SHADOW_ETCHED_IN);

      transform_list = gtk_clist_new(1);
      gtk_clist_set_selection_mode(GTK_CLIST(transform_list), GTK_SELECTION_SINGLE);
      gtk_clist_set_shadow_type(GTK_CLIST(transform_list), GTK_SHADOW_ETCHED_IN);
      gtk_clist_column_titles_passive(GTK_CLIST(transform_list));
      for (i = 0; i < num_transform_types; i++) 
	{
	  if (i < NUM_TRANSFORM_TYPES)
	    str = TRANSFORM_TYPES[i];
	  else str = added_transform_name(i);
	  gtk_clist_append(GTK_CLIST(transform_list), &str);
	}
      gtk_signal_connect(GTK_OBJECT(transform_list), "select_row", GTK_SIGNAL_FUNC(transform_browse_Callback), (gpointer)ss);

      type_scroller = gtk_scrolled_window_new(NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(type_scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(type_scroller), transform_list);
      gtk_container_add(GTK_CONTAINER(type_frame), type_scroller);

      gtk_widget_show(transform_list);
      gtk_widget_show(type_scroller);
      gtk_widget_show(type_frame);


      /* SIZE */
      size_frame = gtk_frame_new(STR_size);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), size_frame, 1, 2, 0, 1);
      gtk_frame_set_label_align(GTK_FRAME(size_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(size_frame), GTK_SHADOW_ETCHED_IN);

      size_list = gtk_clist_new(1);
      gtk_clist_set_selection_mode(GTK_CLIST(size_list), GTK_SELECTION_SINGLE);
      gtk_clist_set_shadow_type(GTK_CLIST(size_list), GTK_SHADOW_ETCHED_IN);
      gtk_clist_column_titles_passive(GTK_CLIST(size_list));
      for (i = 0; i < NUM_FFT_SIZES; i++) 
	{
	  str = FFT_SIZES[i];
	  gtk_clist_append(GTK_CLIST(size_list), &str);
	}
      gtk_signal_connect(GTK_OBJECT(size_list), "select_row", GTK_SIGNAL_FUNC(size_browse_Callback), (gpointer)ss);

      size_scroller = gtk_scrolled_window_new(NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(size_scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(size_scroller), size_list);
      gtk_container_add(GTK_CONTAINER(size_frame), size_scroller);

      gtk_widget_show(size_list);
      gtk_widget_show(size_scroller);
      gtk_widget_show(size_frame);


      /* DISPLAY */
      display_frame = gtk_frame_new(STR_display);
      /* gtk_table_attach_defaults(GTK_TABLE(outer_table), display_frame, 2, 3, 0, 1); */
      gtk_table_attach(GTK_TABLE(outer_table), display_frame, 2, 3, 0, 1,
		       (GtkAttachOptions)(GTK_FILL | GTK_SHRINK), 
		       (GtkAttachOptions)(GTK_FILL | GTK_SHRINK),
		       0, 0);
      gtk_frame_set_label_align(GTK_FRAME(display_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(display_frame), GTK_SHADOW_ETCHED_IN);

      buttons = gtk_vbox_new(FALSE, 0);
      gtk_container_add(GTK_CONTAINER(display_frame), buttons);
      set_background(buttons, (ss->sgx)->position_color);

      normal_fft_button = gtk_radio_button_new_with_label(NULL, STR_normal_fft);
      gtk_box_pack_start(GTK_BOX(buttons), normal_fft_button, FALSE, FALSE, 0);
      gtk_widget_show(normal_fft_button);
      gtk_signal_connect(GTK_OBJECT(normal_fft_button), "clicked", GTK_SIGNAL_FUNC(normal_fft_Callback), (gpointer)ss);
      button_pushed_red(normal_fft_button, ss);
      gtk_widget_set_usize(GTK_WIDGET(normal_fft_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      sono_button = gtk_radio_button_new_with_label(gtk_radio_button_group(GTK_RADIO_BUTTON(normal_fft_button)), STR_sonogram);
      gtk_box_pack_start(GTK_BOX(buttons), sono_button, FALSE, FALSE, 0);
      gtk_widget_show(sono_button);
      gtk_signal_connect(GTK_OBJECT(sono_button), "clicked", GTK_SIGNAL_FUNC(sonogram_Callback), (gpointer)ss);
      button_pushed_red(sono_button, ss);
      gtk_widget_set_usize(GTK_WIDGET(sono_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      spectro_button = gtk_radio_button_new_with_label(gtk_radio_button_group(GTK_RADIO_BUTTON(normal_fft_button)), STR_spectrogram);
      gtk_box_pack_start(GTK_BOX(buttons), spectro_button, FALSE, FALSE, 0);
      gtk_widget_show(spectro_button);
      gtk_signal_connect(GTK_OBJECT(spectro_button), "clicked", GTK_SIGNAL_FUNC(spectrogram_Callback), (gpointer)ss);
      button_pushed_red(spectro_button, ss);
      gtk_widget_set_usize(GTK_WIDGET(spectro_button), BUTTON_WIDTH, BUTTON_HEIGHT);
      
      peaks_button = gtk_check_button_new_with_label(STR_peaks);
      gtk_box_pack_start(GTK_BOX(buttons), peaks_button, FALSE, FALSE, 0);
      gtk_widget_show(peaks_button);
      gtk_signal_connect(GTK_OBJECT(peaks_button), "toggled", GTK_SIGNAL_FUNC(peaks_Callback), (gpointer)ss);
      button_pushed_red(peaks_button, ss);
      gtk_widget_set_usize(GTK_WIDGET(peaks_button), BUTTON_WIDTH, BUTTON_HEIGHT);
 
      db_button = gtk_check_button_new_with_label(STR_dB);
      gtk_box_pack_start(GTK_BOX(buttons), db_button, FALSE, FALSE, 0);
      gtk_widget_show(db_button);
      gtk_signal_connect(GTK_OBJECT(db_button), "toggled", GTK_SIGNAL_FUNC(db_Callback), (gpointer)ss);
      button_pushed_red(db_button, ss);
      gtk_widget_set_usize(GTK_WIDGET(db_button), BUTTON_WIDTH, BUTTON_HEIGHT);
 
      logfreq_button = gtk_check_button_new_with_label(STR_log_freq);
      gtk_box_pack_start(GTK_BOX(buttons), logfreq_button, FALSE, FALSE, 0);
      gtk_widget_show(logfreq_button);
      gtk_signal_connect(GTK_OBJECT(logfreq_button), "toggled", GTK_SIGNAL_FUNC(logfreq_Callback), (gpointer)ss);
      button_pushed_red(logfreq_button, ss);
      gtk_widget_set_usize(GTK_WIDGET(logfreq_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      normalize_button = gtk_check_button_new_with_label(STR_normalize);
      gtk_box_pack_start(GTK_BOX(buttons), normalize_button, FALSE, FALSE, 0);
      gtk_widget_show(normalize_button);
      gtk_signal_connect(GTK_OBJECT(normalize_button), "toggled", GTK_SIGNAL_FUNC(normalize_Callback), (gpointer)ss);
      button_pushed_red(normalize_button, ss);
      gtk_widget_set_usize(GTK_WIDGET(normalize_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      selection_button = gtk_check_button_new_with_label(STR_selection);
      gtk_box_pack_start(GTK_BOX(buttons), selection_button, FALSE, FALSE, 0);
      gtk_widget_show(selection_button);
      gtk_signal_connect(GTK_OBJECT(selection_button), "toggled", GTK_SIGNAL_FUNC(selection_Callback), (gpointer)ss);
      button_pushed_red(selection_button, ss);
      gtk_widget_set_usize(GTK_WIDGET(selection_button), BUTTON_WIDTH, BUTTON_HEIGHT);

      gtk_widget_show(buttons);
      gtk_widget_show(display_frame);

      /* WAVELET */
      wavelet_frame = gtk_frame_new(STR_wavelet);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), wavelet_frame, 0, 1, 1, 2);
      gtk_frame_set_label_align(GTK_FRAME(wavelet_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(wavelet_frame), GTK_SHADOW_ETCHED_IN);

      wavelet_list = gtk_clist_new(1);
      gtk_clist_set_selection_mode(GTK_CLIST(wavelet_list), GTK_SELECTION_SINGLE);
      gtk_clist_set_shadow_type(GTK_CLIST(wavelet_list), GTK_SHADOW_ETCHED_IN);
      gtk_clist_column_titles_passive(GTK_CLIST(wavelet_list));
      for (i = 0; i < NUM_WAVELETS; i++) 
	{
	  str = WAVELETS[i];
	  gtk_clist_append(GTK_CLIST(wavelet_list), &str);
	}
      gtk_signal_connect(GTK_OBJECT(wavelet_list), "select_row", GTK_SIGNAL_FUNC(wavelet_browse_Callback), (gpointer)ss);

      wavelet_scroller = gtk_scrolled_window_new(NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(wavelet_scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(wavelet_scroller), wavelet_list);
      gtk_container_add(GTK_CONTAINER(wavelet_frame), wavelet_scroller);

      gtk_widget_show(wavelet_list);
      gtk_widget_show(wavelet_scroller);
      gtk_widget_show(wavelet_frame);


      /* WINDOW */
      window_frame = gtk_frame_new(STR_window);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), window_frame, 1, 2, 1, 2);
      gtk_frame_set_label_align(GTK_FRAME(window_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(window_frame), GTK_SHADOW_ETCHED_IN);

      window_box = gtk_table_new(2, 2, FALSE);
      gtk_container_add(GTK_CONTAINER(window_frame), window_box);

      window_list = gtk_clist_new(1);
      gtk_clist_set_selection_mode(GTK_CLIST(window_list), GTK_SELECTION_SINGLE);
      gtk_clist_set_shadow_type(GTK_CLIST(window_list), GTK_SHADOW_ETCHED_IN);
      gtk_clist_column_titles_passive(GTK_CLIST(window_list));
      for (i = 0; i < GUI_NUM_FFT_WINDOWS; i++) 
	{
	  str = FFT_WINDOWS[i];
	  gtk_clist_append(GTK_CLIST(window_list), &str);
	}
      gtk_signal_connect(GTK_OBJECT(window_list), "select_row", GTK_SIGNAL_FUNC(window_browse_Callback), (gpointer)ss);

      window_scroller = gtk_scrolled_window_new(NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(window_scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(window_scroller), window_list);
      /* gtk_table_attach_defaults(GTK_TABLE(window_box), window_scroller, 0, 1, 0, 1); */
      gtk_table_attach(GTK_TABLE(window_box), window_scroller, 0, 1, 0, 1, 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);

      beta_adj = gtk_adjustment_new(0.0, 0.0, 1.01, 0.001, 0.01, .01);
      window_beta_scale = gtk_hscale_new(GTK_ADJUSTMENT(beta_adj));
      GTK_WIDGET_UNSET_FLAGS(window_beta_scale, GTK_CAN_FOCUS);
      gtk_range_set_update_policy(GTK_RANGE(GTK_SCALE(window_beta_scale)), GTK_UPDATE_CONTINUOUS);
      gtk_scale_set_digits(GTK_SCALE(window_beta_scale), 2);
      gtk_scale_set_value_pos(GTK_SCALE(window_beta_scale), GTK_POS_TOP);
      gtk_scale_set_draw_value(GTK_SCALE(window_beta_scale), TRUE);
      gtk_signal_connect(GTK_OBJECT(beta_adj), "value_changed", GTK_SIGNAL_FUNC(beta_Callback), (gpointer)ss);
      gtk_table_attach(GTK_TABLE(window_box), window_beta_scale, 0, 1, 1, 2,
		       (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		       (GtkAttachOptions)(GTK_FILL), 
		       0, 0);

      gtk_widget_show(window_beta_scale);
      gtk_widget_show(window_list);
      gtk_widget_show(window_scroller);
      gtk_widget_show(window_box);
      gtk_widget_show(window_frame);


      /* GRAPH */
      graph_frame = gtk_frame_new(FFT_WINDOWS[fft_window(ss)]);
      gtk_table_attach_defaults(GTK_TABLE(outer_table), graph_frame, 2, 3, 1, 2);
      gtk_frame_set_label_align(GTK_FRAME(graph_frame), 0.5, 0.0);
      gtk_frame_set_shadow_type(GTK_FRAME(graph_frame), GTK_SHADOW_ETCHED_IN);

      graph_drawer = gtk_drawing_area_new();
      gtk_container_add(GTK_CONTAINER(graph_frame), graph_drawer);
      gc = graph_drawer->style->black_gc;
      set_background(graph_drawer, (ss->sgx)->white);

      fgc = gdk_gc_new(MAIN_WINDOW(ss));
      gdk_gc_set_background(fgc, (ss->sgx)->white);
      gdk_gc_set_foreground(fgc, (ss->sgx)->enved_waveform_color);

      gtk_widget_show(graph_drawer);
      gtk_widget_show(graph_frame);


      gtk_clist_select_row(GTK_CLIST(transform_list), transform_type(ss), 0);
      for (i = 0; i < NUM_FFT_SIZES; i++)
	if (fft_sizes[i] == fft_size(ss))
	  {
	    gtk_clist_select_row(GTK_CLIST(size_list), i, 0);
	    gtk_clist_moveto(GTK_CLIST(size_list), i, 0, 0.5, 0.5);
	    break;
	  }
      if (fft_style(ss) == NORMAL_FFT) set_toggle_button(normal_fft_button, TRUE, FALSE, (gpointer)ss);
      if (fft_style(ss) == SONOGRAM) set_toggle_button(sono_button, TRUE, FALSE, (gpointer)ss);
      if (fft_style(ss) == SPECTROGRAM) set_toggle_button(spectro_button, TRUE, FALSE, (gpointer)ss);
      set_toggle_button(peaks_button, show_fft_peaks(ss), FALSE, (gpointer)ss);
      set_toggle_button(db_button, fft_log_magnitude(ss), FALSE, (gpointer)ss);
      set_toggle_button(logfreq_button, fft_log_frequency(ss), FALSE, (gpointer)ss);
      set_toggle_button(normalize_button, normalize_fft(ss), FALSE, (gpointer)ss);
      set_toggle_button(selection_button, show_selection_transform(ss), FALSE, (gpointer)ss);
      gtk_clist_select_row(GTK_CLIST(window_list), fft_window(ss), 0);
      gtk_clist_moveto(GTK_CLIST(window_list), fft_window(ss), 0, 0.5, 0.5);
      gtk_clist_select_row(GTK_CLIST(wavelet_list), wavelet_type(ss), 0);
      gtk_clist_moveto(GTK_CLIST(wavelet_list), wavelet_type(ss), 0, 0.5, 0.5);

      need_callback = 1;
      gtk_widget_show(outer_table);
    }
  else raise_dialog(transform_dialog);
  gtk_widget_show(transform_dialog);

  if (need_callback)
    {
      get_fft_window_data(ss);
      gtk_signal_connect(GTK_OBJECT(graph_drawer), "expose_event", GTK_SIGNAL_FUNC(graph_expose_Callback), (gpointer)ss);
      gtk_signal_connect(GTK_OBJECT(graph_drawer), "configure_event", GTK_SIGNAL_FUNC(graph_configure_Callback), (gpointer)ss);
      need_callback = 0;
    }
}

int transform_dialog_is_active(void)
{
  return((transform_dialog) && (GTK_WIDGET_VISIBLE(transform_dialog)));
}

void set_fft_beta(snd_state *ss, Float val)
{
  in_set_fft_beta(ss, val);
  map_chans_field(ss, FCP_BETA, val);
  if (transform_dialog) 
    {
      gtk_adjustment_set_value(GTK_ADJUSTMENT(beta_adj), val);
      get_fft_window_data(ss);
      graph_redisplay(ss);
    }
  if (!(ss->graph_hook_active)) map_over_chans(ss, calculate_fft, NULL);
}

void set_fft_size(snd_state *ss, int val)
{
  int i;
  in_set_fft_size(ss, val);
  map_over_chans(ss, map_chans_fft_size, (void *)val);
  if (transform_dialog)
    {
      for (i = 0; i < NUM_FFT_SIZES; i++)
	if (fft_sizes[i] == val)
	  {
	    gtk_clist_select_row(GTK_CLIST(size_list), i, 0);
	    gtk_clist_moveto(GTK_CLIST(size_list), i, 0, 0.5, 0.5);
	    break;
	  }
    }
  if (!(ss->graph_hook_active)) map_over_chans(ss, calculate_fft, NULL);
}

void set_fft_window(snd_state *ss, int val)
{
  in_set_fft_window(ss, val);
  if (!(ss->graph_hook_active)) map_over_chans(ss, calculate_fft, NULL);
  if ((transform_dialog) && (graph_drawer))
    {
      gtk_clist_select_row(GTK_CLIST(window_list), val, 0);
      gtk_clist_moveto(GTK_CLIST(window_list), val, 0, 0.5, 0.5);
      if (graph_frame) 
	gtk_frame_set_label(GTK_FRAME(graph_frame),
			    FFT_WINDOWS[val]);
      get_fft_window_data(ss);
      graph_redisplay(ss);
    }
}
  
void set_transform_type(snd_state *ss, int val)
{
  if (!(ss->graph_hook_active)) map_over_chans(ss, force_fft_clear, NULL);
  in_set_transform_type(ss, val);
  map_over_chans(ss, map_chans_transform_type, (void *)val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, calculate_fft, NULL);
  if (transform_dialog) 
    gtk_clist_select_row(GTK_CLIST(transform_list), val, 0);
}

void set_wavelet_type(snd_state *ss, int val)
{
  if (transform_dialog) 
    {
      gtk_clist_select_row(GTK_CLIST(wavelet_list), val, 0);
      gtk_clist_moveto(GTK_CLIST(wavelet_list), val, 0, 0.5, 0.5);
    }
  in_set_wavelet_type(ss, val);
  map_over_chans(ss, map_chans_wavelet_type, (void *)val);
  if ((transform_type(ss) == WAVELET) && 
      (!(ss->graph_hook_active))) 
    map_over_chans(ss, calculate_fft, NULL);
}

/* various set- cases need to be reflected in the transform dialog */
void set_show_fft_peaks(snd_state *ss, int val)
{
  in_set_show_fft_peaks(ss, val);
  map_over_chans(ss, map_show_fft_peaks, (void *)val);
  if (transform_dialog) 
    set_toggle_button(peaks_button, val, FALSE, (void *)ss);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, calculate_fft, NULL);
}

void set_fft_log_frequency(snd_state *ss, int val)
{
  in_set_fft_log_frequency(ss, val);
  map_over_chans(ss, map_chans_fft_log_frequency, (void *)val);
  if (transform_dialog)
    set_toggle_button(logfreq_button, val, FALSE, (void *)ss);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, calculate_fft, NULL);
}

void set_fft_log_magnitude(snd_state *ss, int val)
{
  in_set_fft_log_magnitude(ss, val);
  map_over_chans(ss, map_chans_fft_log_magnitude, (void *)val);
  if (transform_dialog) 
    set_toggle_button(db_button, val, FALSE, (void *)ss);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, calculate_fft, NULL);
}

void set_fft_style(snd_state *ss, int val)
{
  in_set_fft_style(ss, val);
  if (transform_dialog) 
    switch (val)
      {
      case NORMAL_FFT: 
	set_toggle_button(normal_fft_button, TRUE, FALSE, (void *)ss); 
	break;
      case SONOGRAM:   
	set_toggle_button(sono_button, TRUE, FALSE, (void *)ss); 
	break;
      case SPECTROGRAM:
	set_toggle_button(spectro_button, TRUE, FALSE, (void *)ss);
	break;
      }
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, calculate_fft, NULL);
}

void set_normalize_fft(snd_state *ss, int val)
{
  in_set_normalize_fft(ss, val);
  map_over_chans(ss, map_chans_normalize_fft, (void *)val);
  if (transform_dialog) 
    set_toggle_button(normalize_button, val, FALSE, (void *)ss);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, calculate_fft, NULL);
}

void set_show_selection_transform(snd_state *ss, int show)
{
  in_set_show_selection_transform(ss, show);
  if (transform_dialog)
    set_toggle_button(selection_button, show, FALSE, (void *)ss); 
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, calculate_fft, NULL);
}


int add_transform_to_list(char *name)
{
  /* put at end of list and return associated browse callback row */
  if (transform_dialog)
    gtk_clist_append(GTK_CLIST(transform_list), &name);
  return(num_transform_types++);
}

