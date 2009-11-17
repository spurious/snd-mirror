#include "snd.h"
#include "clm2xen.h"
#include "clm-strings.h"

/* TODO: the per-chan attributes (y-bounds, graph-type etc) are a mess when update, unite, sync etc happen
 * TODO: the default should be to show the full y-bounds if possible
 *   it might be nice to show the waveform(s) as-read-so-far updating as we get new points from the disk
 *   (with blank space after current read end point) -- peak_env_usable[snd-snd.c] and peak_env_graph
 *   would need to be smarter? or use peak_env_partial_graph?
 */

bool graph_style_p(int grf)
{
  switch (grf)
    {
    case GRAPH_LINES: case GRAPH_DOTS: case GRAPH_FILLED: case GRAPH_DOTS_AND_LINES: case GRAPH_LOLLIPOPS: 
      return(true);
      break;
    }
  return(false);
}


typedef struct lisp_grf {
  int *len;
  mus_float_t **data;
  int graphs;
  axis_info *axis;
  int env_data;
  show_axes_t show_axes;
} lisp_grf;


chan_info *get_cp(XEN snd, XEN x_chn_n, const char *caller)
{
  snd_info *sp;
  int chn_n;

  sp = get_sp(snd);

  if ((sp == NULL) || (!(sp->active)) || (sp->inuse == SOUND_IDLE))
    {
      snd_no_such_sound_error(caller, snd); 
      return(NULL); /* just in case our catch has been clobbered */
    }

  if (XEN_INTEGER_P(x_chn_n))
    chn_n = XEN_TO_C_INT(x_chn_n);
  else
    if (sp->selected_channel != NO_SELECTION) 
      chn_n = sp->selected_channel;
    else chn_n = 0;

  if ((chn_n >= 0) && (chn_n < sp->nchans) && (sp->chans[chn_n]))
    return(sp->chans[chn_n]);

  snd_no_such_channel_error(caller, snd, x_chn_n);
  return(NULL);
}


typedef enum {CLICK_NOGRAPH, CLICK_WAVE, CLICK_FFT_AXIS, CLICK_LISP, CLICK_MIX, CLICK_MARK,
	      CLICK_FFT_MAIN, CLICK_SELECTION_LEFT, CLICK_SELECTION_RIGHT} click_loc_t;
/* for marks, regions, mouse click detection */
/*
 * static char *click_detection_names[9] = {"no graph", "click wave", "click fft axis", "click lisp", "click mix", "click mark",
 *                                          "click fft graph", "click selection left", "click selection right"};
 */


static XEN lisp_graph_hook;
static XEN mouse_press_hook; 
static XEN mark_click_hook; 
static XEN mix_click_hook; 
static XEN mouse_click_hook;
static XEN mouse_drag_hook; 
static XEN key_press_hook; 
static XEN after_transform_hook;
static XEN graph_hook;
static XEN after_graph_hook;
static XEN after_lisp_graph_hook;
static XEN time_graph_hook;


static void after_transform(chan_info *cp, mus_float_t scaler)
{
  if (XEN_HOOKED(after_transform_hook))
    run_hook(after_transform_hook,
	     XEN_LIST_3(C_INT_TO_XEN_SOUND(cp->sound->index),
			C_TO_XEN_INT(cp->chan),
			C_TO_XEN_DOUBLE(scaler)),
	     S_after_transform_hook);
}


static void run_after_graph_hook(chan_info *cp)
{
  if ((cp->hookable == WITH_HOOK) &&
      (XEN_HOOKED(after_graph_hook)))
    run_hook(after_graph_hook,
	     XEN_LIST_2(C_INT_TO_XEN_SOUND(cp->sound->index),
			C_TO_XEN_INT(cp->chan)),
	     S_after_graph_hook);
  /* (add-hook! after-graph-hook (lambda (a b) (snd-print (format #f "~A ~A" a b)))) */
}


static void set_y_bounds(axis_info *ap);

static void chans_time_graph_type(chan_info *cp, int value)
{
  cp->time_graph_type = (graph_type_t)value;
  if (cp->time_graph_type == GRAPH_ONCE) 
    {
      set_y_bounds(cp->axis);
      resize_sy(cp);
      set_x_bounds(cp->axis);
      resize_sx_and_zx(cp);
    }
  update_graph(cp); 
}

static void set_time_graph_type(graph_type_t val) 
{
  in_set_time_graph_type(val); 
  for_each_chan_with_int(chans_time_graph_type, (int)val);
}


static void chans_wavo_hop(chan_info *cp, int hop)
{
  cp->wavo_hop = hop;
  update_graph(cp); 
}

static void set_wavo_hop(int uval) 
{
  int val; 
  if (uval < 1) 
    val = 1; 
  else val = uval; 
  in_set_wavo_hop(val); 
  for_each_chan_with_int(chans_wavo_hop, val);
}


static void chans_wavo_trace(chan_info *cp, int value)
{
  cp->wavo_trace = value;
  update_graph(cp);
}

void set_wavo_trace(int uval) 
{
  int val; 
  if (uval < 1) 
    val = 1; 
  else val = uval; 
  in_set_wavo_trace(val); 
  for_each_chan_with_int(chans_wavo_trace, val);
}


static void set_beats_per_minute(mus_float_t val) 
{
  if (val > 0.0) 
    {
      if (val > 10000.0) val = 10000.0;
      in_set_beats_per_minute(val); 
      chans_field(FCP_BEATS, val);
      if (!(ss->graph_hook_active)) 
	for_each_chan(update_graph);
    }
}

static void chans_beats_per_measure(chan_info *cp, int value)
{
  cp->beats_per_measure = value;
  update_graph(cp);
}

static void set_beats_per_measure(int val) 
{
  if (val > 0) 
    {
      if (val > 1000) val = 1000;
      in_set_beats_per_measure(val); 
      for_each_chan_with_int(chans_beats_per_measure, val);
    }
}


static void chans_max_transform_peaks(chan_info *cp, int value)
{
  cp->max_transform_peaks = value;
}

void set_max_transform_peaks(int val) 
{
  in_set_max_transform_peaks(val); 
  for_each_chan_with_int(chans_max_transform_peaks, val);
}


static void chans_zero_pad(chan_info *cp, int value)
{
  cp->zero_pad = value;
  calculate_fft(cp);
}

static void set_zero_pad(int val) 
{
  in_set_zero_pad(val); 
  for_each_chan_with_int(chans_zero_pad, val);
}


static void chans_show_grid(chan_info *cp, int value)
{
  cp->show_grid = (with_grid_t)value;
  update_graph(cp);
}

static void set_show_grid(with_grid_t val)
{
  in_set_show_grid(val);
  for_each_chan_with_int(chans_show_grid, (int)val);
}


static void chans_grid_density(chan_info *cp, mus_float_t value)
{
  cp->grid_density = value;
  update_graph(cp);
}

static void set_grid_density(mus_float_t val)
{
  in_set_grid_density(val);
  for_each_chan_with_float(chans_grid_density, val);
}


static void chans_show_sonogram_cursor(chan_info *cp, bool value)
{
  cp->show_sonogram_cursor = value;
  update_graph(cp);
}

static void set_show_sonogram_cursor(bool val)
{
  in_set_show_sonogram_cursor(val);
  for_each_chan_with_bool(chans_show_sonogram_cursor, val);
}


static void chans_transform_graph_type(chan_info *cp, int value)
{
  cp->transform_graph_type = (graph_type_t)value;
}

void in_set_transform_graph_type(graph_type_t uval) 
{
  graph_type_t val;
  val = (graph_type_t)mus_iclamp((int)GRAPH_ONCE, uval, (int)GRAPH_AS_SPECTROGRAM);
  in_set_transform_graph_type_1(val); 
  for_each_chan_with_int(chans_transform_graph_type, (int)val);
}


static void chans_show_mix_waveforms(chan_info *cp, bool value)
{
  cp->show_mix_waveforms = value;
  update_graph(cp); 
}

static void set_show_mix_waveforms(bool val) 
{
  in_set_show_mix_waveforms(val); 
  for_each_chan_with_bool(chans_show_mix_waveforms, val);
}


static void chans_show_axes(chan_info *cp, int value)
{
  cp->show_axes = (show_axes_t)value;
  if (cp->lisp_info)
    cp->lisp_info->show_axes = cp->show_axes;
  update_graph(cp); 
}

void set_show_axes(show_axes_t val) 
{
  in_set_show_axes(val); 
  for_each_chan_with_int(chans_show_axes, (int)val);
}


static void chans_graphs_horizontal(chan_info *cp, bool value)
{
  cp->graphs_horizontal = value;
  update_graph(cp); 
}

static void set_graphs_horizontal(bool val) 
{
  in_set_graphs_horizontal(val);
  for_each_chan_with_bool(chans_graphs_horizontal, val);
}


static void chans_fft_window(chan_info *cp, int value)
{
  cp->fft_window = (mus_fft_window_t)value;
  if (cp->fft) (cp->fft)->window = (mus_fft_window_t)value;
}

void in_set_fft_window(mus_fft_window_t val) 
{
  in_set_fft_window_1(val); 
  for_each_chan_with_int(chans_fft_window, (int)val);
}


void chans_field(fcp_t field, mus_float_t val)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      int j;
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && ((sp->inuse == SOUND_NORMAL) || (sp->inuse == SOUND_WRAPPER)))
	for (j = 0; j < sp->nchans; j++)
	  switch (field)
	    {
	    case FCP_X_ANGLE:        sp->chans[j]->spectro_x_angle = val;                         break; /* these are in-coming from user interface */
	    case FCP_X_SCALE:        sp->chans[j]->spectro_x_scale = val;                         break;
	    case FCP_Y_ANGLE:        sp->chans[j]->spectro_y_angle = val;                         break;
	    case FCP_Y_SCALE:        sp->chans[j]->spectro_y_scale = val;                         break;
	    case FCP_Z_ANGLE:        sp->chans[j]->spectro_z_angle = val;                         break;
	    case FCP_Z_SCALE:        sp->chans[j]->spectro_z_scale = val;                         break;
	    case FCP_SPECTRUM_START: sp->chans[j]->spectrum_start = mus_fclamp(0.0, val, 1.0);    break; 
	    case FCP_SPECTRUM_END:   sp->chans[j]->spectrum_end = mus_fclamp(0.0, val, 1.0);      break;
	    case FCP_ALPHA:          sp->chans[j]->fft_window_alpha = mus_fclamp(0.0, val, 10.0); break;
	    case FCP_BETA:           sp->chans[j]->fft_window_beta = mus_fclamp(0.0, val, 1.0);   break;
	    case FCP_BEATS:          if (val > 0.0) sp->chans[j]->beats_per_minute = val;         break;
	    }
    }
}


void combine_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_COMBINED);}
void superimpose_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_SUPERIMPOSED);}
void separate_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_SEPARATE);}


void set_sound_channel_style(snd_info *sp, channel_style_t val)
{
  switch (val)
    {
    case CHANNELS_SEPARATE:     separate_sound(sp);    break; /* snd-xchn.c -> change_channel_style */
    case CHANNELS_COMBINED:     combine_sound(sp);     break;
    case CHANNELS_SUPERIMPOSED: superimpose_sound(sp); break;
    default:
      break;
    }
}


bool chan_fft_in_progress(chan_info *cp)
{
  /* fft_in_progress is a background process only if sonogram/spectrogram */
  return((bool)(cp->cgx->fft_in_progress));
}


void set_chan_fft_in_progress(chan_info *cp, idle_t fp) 
{
  cp->cgx->fft_in_progress = fp;
}


void stop_fft_in_progress(chan_info *cp)
{
  if ((cp) && (cp->cgx))
    {
      chan_context *cx;
      cx = cp->cgx;
      if (cx->fft_in_progress) 
	{
	  BACKGROUND_REMOVE(cx->fft_in_progress);
	  finish_progress_report(cp);
	  cx->fft_in_progress = 0;
	}
    }
}


void stop_peak_env(chan_info *cp)
{
  chan_context *cgx;
  cgx = cp->cgx;
  if ((cgx) && (cgx->peak_env_in_progress))
    {
      BACKGROUND_REMOVE(cgx->peak_env_in_progress);
      free_peak_env_state(cp);
      cgx->peak_env_in_progress = 0; 
    }
}


void force_fft_clear(chan_info *cp)
{
  if ((cp->cgx) && (cp->cgx->fft_in_progress))
    {
      BACKGROUND_REMOVE(cp->cgx->fft_in_progress);
      finish_progress_report(cp);
      cp->cgx->fft_in_progress = 0;
    }
  if (cp->fft) cp->fft = free_fft_info(cp->fft);
  cp_free_fft_state(cp);
}


void chan_info_cleanup(chan_info *cp)
{
  if ((cp) && (cp->cgx))
    {
      chan_context *cx;
      cx = cp->cgx;
      cx->selected = false;
      if (cx->fft_in_progress) 
	{
	  BACKGROUND_REMOVE(cx->fft_in_progress);
	  cx->fft_in_progress = 0;
	}
      stop_peak_env(cp);
      cleanup_cw(cp);
    }
}


static void chans_dot_size(chan_info *cp, int value)
{
  cp->dot_size = value;
  update_graph(cp);
}

void set_dot_size(int val)
{
  if (val > 0)  /* -1 here can crash X! */
    {
      in_set_dot_size(val);
      for_each_chan_with_int(chans_dot_size, val);
    }
}


static void chans_cursor_size(chan_info *cp, int value)
{
  cp->cursor_size = value;
  update_graph(cp);
}

static void set_cursor_size(int val)
{
  if (val > 0)
    {
      in_set_cursor_size(val);
      for_each_chan_with_int(chans_cursor_size, val);
    }
}


static void chans_cursor_style(chan_info *cp, int value)
{
  cursor_style_t style;
  style = (cursor_style_t)value;
  if ((cp->cursor_style == CURSOR_PROC) && (XEN_PROCEDURE_P(cp->cursor_proc)))
    {
      snd_unprotect_at(cp->cursor_proc_loc);
      cp->cursor_proc = XEN_UNDEFINED;
      cp->cursor_proc_loc = NOT_A_GC_LOC;
    }
  cp->cursor_style = style;
  cp->just_zero = (style == CURSOR_LINE); /* no point in displaying y value in this case */
  update_graph(cp);
}

static void set_cursor_style(cursor_style_t val)
{
  in_set_cursor_style(val);
  for_each_chan_with_int(chans_cursor_style, (int)val);
}


static void chans_tracking_cursor_style(chan_info *cp, int value)
{
  cp->tracking_cursor_style = (cursor_style_t)value;
}

static void set_tracking_cursor_style(cursor_style_t val)
{
  in_set_tracking_cursor_style(val);
  for_each_chan_with_int(chans_tracking_cursor_style, (int)val);
}


chan_info *virtual_selected_channel(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp->channel_style == CHANNELS_SEPARATE) || (sp->selected_channel == NO_SELECTION)) 
    return(cp);
  else return(sp->chans[sp->selected_channel]);
}


static int calculate_fft_1(chan_info *cp, bool update_display)
{
  if ((cp->graph_transform_p) &&
      (!(chan_fft_in_progress(cp))))
    {
#if (!USE_NO_GUI)
      if (cp->transform_graph_type == GRAPH_ONCE)
	single_fft(cp, update_display, DONT_FORCE_REFFT);
      else set_chan_fft_in_progress(cp,
				    BACKGROUND_ADD(sonogram_in_slices,
						   make_sonogram_state(cp, 
								       DONT_FORCE_REFFT)));
#else
      single_fft(cp, update_display, DONT_FORCE_REFFT);
#endif
    }
  return(0);
}

void calculate_fft(chan_info *cp)
{
  calculate_fft_1(cp, FORCE_REDISPLAY);
}


static bool updating = false;

static void update_graph_1(chan_info *cp, bool warn)
{
  /* don't put display stuff here!  This is needed so that the fft display does not get caught in a loop */
  double cur_srate;
  snd_info *sp;
  axis_info *ap;

  if ((updating) || 
      (cp->active != CHANNEL_HAS_AXES) ||
      (cp->cgx == NULL) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL)) 
    return;

  sp = cp->sound;
  if (cp->squelch_update)
    {
#if (!USE_NO_GUI)
      if ((warn) && (sp))
	string_to_minibuffer(sp, "(update squelched)"); /* this has tripped me one too many times... */
#endif
      return;
    }

  updating = true;
  /* next two are needed by fft and lisp displays, but if put off until make_graph cause
   * the display to happen twice in some cases 
   */
  ap = cp->axis;
  if (ap)
    {
      cur_srate = (double)(SND_SRATE(sp));
      ap->losamp = snd_round_mus_long_t(ap->x0 * cur_srate); 
      if (ap->losamp < 0) ap->losamp = 0;
      ap->hisamp = (mus_long_t)(ap->x1 * cur_srate);
    }

  if (!(cp->cgx->ax->wn)) 
    if (!(fixup_cp_cgx_ax_wn(cp))) 
      {
	/* window not active yet (gtk) */
	updating = false;
	return;
      }

  if ((cp->graph_transform_p) && 
      (!(chan_fft_in_progress(cp)))) 
    calculate_fft_1(cp, DONT_FORCE_REDISPLAY);
  display_channel_data(cp);
  updating = false;
}


void update_graph(chan_info *cp)
{
  update_graph_1(cp, false);
}


void update_graph_or_warn(chan_info *cp)
{
  update_graph_1(cp, true);
}


#define INITIAL_EDIT_SIZE 8

static XEN initial_graph_hook;

void add_channel_data_1(chan_info *cp, int srate, mus_long_t frames, channel_graph_t graphed)
{
  /* initialize channel, including edit/sound lists */
  axis_info *ap;
  mus_float_t ymin = 0.0, ymax = 0.0, y0, y1;
  double xmax, x0, x1, dur, gdur;
  const char *label = NULL;
  char *hook_label = NULL;
  bool ymin_set = false, ymax_set = false;

  cp->edit_size = INITIAL_EDIT_SIZE;
  cp->edits = (ed_list **)calloc(cp->edit_size, sizeof(ed_list *));
  cp->edit_ctr = 0;
  cp->edits[0] = initial_ed_list(0, frames - 1);
  cp->sound_size = INITIAL_EDIT_SIZE;
  cp->sound_ctr = 0;
  cp->sounds = (snd_data **)calloc(cp->sound_size, sizeof(snd_data *));

  cp->active = CHANNEL_HAS_EDIT_LIST;

  x0 = 0.0;
  x1 = 0.1;
  y0 = -1.0;
  y1 = 1.0;
  switch (cp->x_axis_style)
    {
    case X_AXIS_IN_BEATS:      label = _("time (beats)");    break;
    case X_AXIS_IN_MEASURES:   label = _("time (measures)"); break;
    case X_AXIS_IN_SAMPLES:    label = _("time (samples)");  break;
    case X_AXIS_AS_PERCENTAGE: label = _("time (percent)");  break;
    default:                   label = _("time");            break;
    }
  dur = (double)frames / (double)(srate);
  if ((cp->hookable == WITH_HOOK) && 
      (graphed == WITH_GRAPH) &&    
      /* can also be WITHOUT_GRAPH and WITHOUT_INITIAL_GRAPH_HOOK
       *   the former is called in snd-nogui, and in the make_readable calls in snd-regions and snd-snd
       *   the latter is from snd-edits where we are updating an already displayed sound (and keeping its axis settings across the update)
       * this hook is replacing earlier "initial-x0" settings
       */
      (XEN_HOOKED(initial_graph_hook)))
    {
      XEN res;
      int len = 0;
      res = run_or_hook(initial_graph_hook,
			XEN_LIST_3(C_INT_TO_XEN_SOUND(cp->sound->index),
				   C_TO_XEN_INT(cp->chan),
				   C_TO_XEN_DOUBLE(dur)),
			S_initial_graph_hook);

      if (XEN_LIST_P_WITH_LENGTH(res, len))
	{
	  if (len > 0) x0 = XEN_TO_C_DOUBLE(XEN_CAR(res));
	  if (len > 1) x1 = XEN_TO_C_DOUBLE(XEN_CADR(res));
	  if (len > 2) y0 = XEN_TO_C_DOUBLE(XEN_CADDR(res));
	  if (len > 3) y1 = XEN_TO_C_DOUBLE(XEN_CADDDR(res));
	  if ((len > 4) && 
	      (XEN_STRING_P(XEN_LIST_REF(res, 4))))
	    hook_label = mus_strdup(XEN_TO_C_STRING(XEN_LIST_REF(res, 4)));
	  if (len > 5)
	    {
	      ymin = XEN_TO_C_DOUBLE(XEN_LIST_REF(res, 5));
	      ymin_set = true;
	    }
	  if (len > 6)
	    {
	      ymax = XEN_TO_C_DOUBLE(XEN_LIST_REF(res, 6));
	      ymax_set = true;
	    }
	  /* ymin/ymax for possible fit data hooks */
	}
    }

  if (dur == 0.0) gdur = .001; else gdur = dur;
  xmax = gdur;
  if (x1 == 0.0) x1 = gdur;
  
  if ((!ymax_set) && (!ymin_set) &&
      (peak_env_maxamp_ok(cp, 0))) /* peak-env.scm uses initial-graph-hook to read in this data, so it's not entirely hopeless */
    {
      ymax = peak_env_maxamp(cp, 0);
      if (ymax < 1.0) ymax = 1.0;
      ymin = -ymax;
    }
  else
    {
      if (!ymax_set) 
	{
	  if (y1 > 1.0) 
	    ymax = y1; 
	  else ymax = 1.0;
	}
      if (!ymin_set) 
	{if (y0 < -1.0) 
	    ymin = y0; 
	  else ymin = -1.0;
	}
    }
  if (dur <= 0.0)
    {
      /* empty sound */
      label = _("(no data)");
      xmax = .001;
    }
  else
    {
      if (xmax > dur) xmax = dur;
      if (x0 >= x1) x0 = x1 - .01;
    }
  if (xmax <= 0.0) xmax = .001;
  if (ymin >= ymax) ymin = ymax - .01;
  if (y0 >= y1) y0 = y1 - .01;
  ap = make_axis_info(cp, 0.0, xmax, ymin, ymax, (hook_label) ? hook_label : label, x0, x1, y0, y1, NULL);
  if (hook_label) free(hook_label);

  if (dur == 0)
    {
      ap->zx = 1.0;
      ap->sx = 1.0;
      ap->no_data = true;
    }
  else
    {
      if (ap->x_ambit != 0.0)
	{
	  ap->zx = (ap->x1 - ap->x0) / ap->x_ambit;
	  ap->sx = (ap->x0 - ap->xmin) / ap->x_ambit;
	}
      ap->no_data = false;
    }

  if (ap->y_ambit != 0.0)
    {
      ap->zy = (ap->y1 - ap->y0) / ap->y_ambit;
      ap->sy = (ap->y0 - ap->ymin) / ap->y_ambit;
    }

  cp->axis = ap;
  if (graphed == WITH_GRAPH) 
    initialize_scrollbars(cp);

  cp->active = CHANNEL_HAS_AXES;
}


void start_peak_env(chan_info *cp)
{
  chan_context *cgx;
  cgx = cp->cgx;
  if (cgx)
    {
      if (cgx->peak_env_in_progress) stop_peak_env(cp);
      start_peak_env_state(cp);
      cgx->peak_env_in_progress = BACKGROUND_ADD(get_peak_env, (any_pointer_t)cp);
    }
}


void add_channel_data(char *filename, chan_info *cp, channel_graph_t graphed)
{
  int chn = 0;
  mus_long_t frames;
  snd_info *sp;
  file_info *chdr, *hdr;

  sp = cp->sound;
  hdr = sp->hdr;
  frames = hdr->samples / hdr->chans;
  add_channel_data_1(cp, hdr->srate, frames, graphed);

  chdr = copy_header(filename, hdr); /* need one separate from snd_info case */
  chn = cp->chan;
  if (chdr)
    {
      int fd;
      fd = snd_open_read(filename);
      if (fd != -1)
	{
	  snd_io *io;
	  snd_file_open_descriptors(fd,
				    filename, chdr->format,
				    chdr->data_location,
				    chdr->chans,
				    chdr->type);
	  during_open(fd, filename, SND_OPEN_CHANNEL);
	  io = make_file_state(fd, chdr, chn, 0, FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(filename, io, chdr, DONT_DELETE_ME, cp->edit_ctr, chn);
	}
    }
#if (!USE_NO_GUI)
  if ((CURRENT_SAMPLES(cp) > PEAK_ENV_CUTOFF) &&
      (cp->edits[0]->peak_env == NULL) &&              /* perhaps created in initial-graph-hook by read-peak-env-info-file */
      (cp->sound->short_filename != NULL))             /* region browser jumped in too soon during autotest */
    start_peak_env(cp);
#endif
}


static void set_y_bounds(axis_info *ap)
{
  mus_float_t range;
  range = ap->zy * ap->y_ambit;
  ap->y0 = ap->ymin + ap->sy * ap->y_ambit;
  ap->y1 = (ap->y0 + range);
  if (ap->y1 > ap->ymax)
    {
      ap->y1 = ap->ymax;
      ap->y0 = ap->y1 - range;
    }
  if (ap->y0 < ap->ymin) ap->y0 = ap->ymin;
}


void set_x_bounds(axis_info *ap)
{
  double range;
  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }
  range = ap->zx * ap->x_ambit;
  ap->x0 = ap->xmin + ap->sx * ap->x_ambit;
#if HAVE_DECL_ISNAN
  if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
  ap->x1 = (ap->x0 + range);
  if (ap->x1 > ap->xmax)
    {
      ap->x1 = ap->xmax;
      ap->x0 = ap->x1 - range;
    }
  if (ap->x0 < ap->xmin) ap->x0 = ap->xmin;
  ap->changed = true;
}


void apply_y_axis_change(axis_info *ap, chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;

  set_y_bounds(ap);
  update_graph_or_warn(cp);
  update_enved_background_waveform(cp);

  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      int i;
      mus_float_t zy, sy;
      sy = ap->sy;
      zy = ap->zy;
      for (i = 0; i < sp->nchans; i++)
	{
	  chan_info *ncp;
	  ncp = sp->chans[i];
	  if (ncp != cp)
	    {
	      axis_info *nap;
	      nap = ncp->axis;
	      if (nap)
		{
		  nap->y_ambit = ap->y_ambit;
		  nap->ymin = ap->ymin;
		  nap->ymax = ap->ymax;
		  nap->zy = zy;
		  nap->sy = sy;
		  set_y_bounds(nap);
		  update_graph_or_warn(ncp);
		}
	    }
	}
    }
}


void set_x_axis_x0x1(chan_info *cp, double x0, double x1) 
{
  /* all callers are explicit (non-gui), so it should be safe to reset the z scroller here as well */
  axis_info *ap;

  ap = cp->axis;
  if (x0 >= 0.0) ap->x0 = x0; else ap->x0 = 0.0;
  ap->x1 = x1;
  if (x1 > ap->xmax) ap->xmax = x1;
  if (ap->xmax <= ap->xmin) ap->xmax = ap->xmin + .001;
  ap->x_ambit = ap->xmax - ap->xmin;
  if (ap->x_ambit != 0.0)
    {
      ap->zx = (x1 - x0) / ap->x_ambit;
      ap->sx = (x0 - ap->xmin) / ap->x_ambit;
      resize_sx_and_zx(cp);
    }
  apply_x_axis_change(ap, cp); /* this checks sync */
  ap->changed = true;
}


static void set_x_axis_x0(chan_info *cp, mus_long_t left)
{
  if (cp)
    {
      axis_info *ap;
      ap = cp->axis; 
      if (ap) 
	{
	  double x1x0;
	  x1x0 = ap->x1 - ap->x0; 
	  ap->x0 = (double)left / (double)SND_SRATE(cp->sound); 
#if HAVE_DECL_ISNAN
	  if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
	  set_x_axis_x0x1(cp, ap->x0, ap->x0 + x1x0);
	}
    }
}


static void set_x_axis_x1(chan_info *cp, mus_long_t right)
{
  if (cp)
    {
      axis_info *ap;
      ap = cp->axis; 
      if (ap) 
	{
	  double x1x0;
	  x1x0 = ap->x1 - ap->x0; 
	  ap->x1 = (double)right / (double)SND_SRATE(cp->sound); 
	  set_x_axis_x0x1(cp, ap->x1 - x1x0, ap->x1);
	}
    }
}


void reset_x_display(chan_info *cp, double sx, double zx)
{
  axis_info *ap;
  bool reset_zx = false;

  ap = cp->axis;
  ap->sx = sx;
  reset_zx = (zx != ap->zx);
  ap->zx = zx;
  set_x_bounds(ap);
  if (reset_zx)
    resize_sx_and_zx(cp);
  else resize_sx(cp);
  update_graph_or_warn(cp);
}


static void update_xs(chan_info *ncp, axis_info *ap)
{
  axis_info *nap;
  nap = ncp->axis;
  if ((nap) && (nap->xmax > 0.0))
    {
      double scl;
      /* ncp->axis can be NULL here and elsewhere if we're in initialize_scrollbars
       *   of the first channel of a (brand-new) multi-channel sound with sync set --
       *   chans after the first have not necessarily set up an axis_info struct yet.
       */
      scl = ap->xmax / nap->xmax;
      reset_x_display(ncp, ap->sx * scl, ap->zx * scl);
    }
}


void apply_x_axis_change(axis_info *ap, chan_info *cp)
{
  int i;
  snd_info *sp;
  sp = cp->sound;
  sp->lacp = cp;
  set_x_bounds(ap);
  update_graph_or_warn(cp);
  if (sp->sync != 0)
    {
      sync_info *si;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++) 
	if (cp != si->cps[i]) 
	  update_xs(si->cps[i], ap);
      si = free_sync_info(si);
    }
  else 
    {
      if (sp->channel_style != CHANNELS_SEPARATE)
	for (i = 0; i < sp->nchans; i++)  /* not 1 (25-Oct-07: 1 might be selected chan, but 0 needs to reflect changes as well */
	  update_xs(sp->chans[i], ap);
    }
}


#define NO_ZOOM_FOCUS_LOCATION -1

static mus_long_t zoom_focus_location(chan_info *cp)
{
  if (cp->cursor_visible)
    return(CURSOR(cp));

  if (selection_is_visible_in_channel(cp)) 
    /* this is complicated!  We want the relative position of the focussed-upon thing
     *   to stay the same, so that the zoom is smooth, but if we focus on just (for example)
     *   the selection start, we end up jumping unexpectedly if C-x v centered the selection.
     * So, return something in the current window related to the current selection.
     */
    {
      mus_long_t beg, end, mid, left, right;
      beg = selection_beg(cp);
      end = beg + selection_len();
      mid = (mus_long_t)(0.5 * (beg + end));
      left = cp->axis->losamp;
      right = cp->axis->hisamp;
      if ((mid > left) &&
	  (mid < right))
	return(mid);
      if ((beg > left) &&
	  (beg < right))
	return(beg);
      if ((end > left) &&
	  (end < right))
	return(end);
      /* return((mus_long_t)(0.5 * (left + right))); */
    }

  {
    mus_long_t pos;
    pos = zoom_focus_mix_in_channel_to_position(cp);
    if (pos >= 0)
      return(pos);
  }

  if (active_mark(cp))
    return(mark_beg(cp));

  return(NO_ZOOM_FOCUS_LOCATION);
}


void focus_x_axis_change(axis_info *ap, chan_info *cp, int focus_style)
{
  /* prepare for set_x_bounds given desired focus point, then drop into default
   * we need to set ap->sx to reflect the new zx value and the focus type
   * if focus_left - go on (nothing to do)
   *    focus_right - get old right, set sx to keep it as is
   *    focus_middle - ditto mid window
   *    focus_active - find the currently active entity, if none use focus_middle 
   *    focus_proc -- call proc
   */

  if (ap->xmax == 0.0) return;

  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }

  if (focus_style != ZOOM_FOCUS_LEFT)
    {
      chan_info *ncp;
      mus_long_t newf;
      switch (focus_style)
	{
	case ZOOM_FOCUS_PROC:
	  ap->x0 = XEN_TO_C_DOUBLE(XEN_CALL_6(ss->zoom_focus_proc,
					      C_INT_TO_XEN_SOUND(cp->sound->index),
					      C_TO_XEN_INT(cp->chan),
					      C_TO_XEN_DOUBLE(ap->zx),
					      C_TO_XEN_DOUBLE(ap->x0),
					      C_TO_XEN_DOUBLE(ap->x1),
					      C_TO_XEN_DOUBLE(ap->x_ambit),
					      S_zoom_focus_style " procedure"));
	  break;

	case ZOOM_FOCUS_RIGHT:   
	  ap->x0 = ap->x1 - ap->zx * ap->x_ambit; 
	  break;

	case ZOOM_FOCUS_MIDDLE:  
	  ap->x0 = 0.5 * ((ap->x1 + ap->x0) - ap->zx * ap->x_ambit); 
	  break;

	case ZOOM_FOCUS_ACTIVE:
	  ncp = virtual_selected_channel(cp);
	  /* axes should be the same, since all move together in this mode */
	  newf = zoom_focus_location(ncp);

	  if (newf == NO_ZOOM_FOCUS_LOCATION)
	    {
	      snd_info *sp;
	      int sync;
	      sp = cp->sound;
	      sync = sp->sync;
	      if (sync != 0)
		{
		  int i, j;
		  for (i = 0; i < sp->nchans; i++)
		    if (i != ncp->chan)
		      {
			newf = zoom_focus_location(sp->chans[i]);
			if (newf != NO_ZOOM_FOCUS_LOCATION)
			  break;
		      }
		  if (newf == NO_ZOOM_FOCUS_LOCATION)
		    {
		      /* geez, maybe it's in a separate syncd sound */
		      for (j = 0; j < ss->max_sounds; j++)
			{
			  sp = ss->sounds[j];
			  if ((sp) && (sp->inuse == SOUND_NORMAL) && (sp->sync == sync) && (sp != cp->sound))
			    for (i = 0; i < sp->nchans; i++)
			      {
				newf = zoom_focus_location(sp->chans[i]);
				if (newf != NO_ZOOM_FOCUS_LOCATION)
				  break;
			      }}}}}

	  if (newf != NO_ZOOM_FOCUS_LOCATION)
	    {
	      double loc, pos;
	      loc = (double)newf / (double)SND_SRATE(ncp->sound);
	      if ((loc > ap->x0) && 
		  (loc < ap->x1) && 
		  (ap->x1 > ap->x0))
		/* try to maintain current relative position in window */
		{
		  pos = (loc - ap->x0) / (ap->x1 - ap->x0);
		  ap->x0 = loc - pos * ap->zx * ap->x_ambit;
		}
	      else ap->x0 = loc - 0.5 * ap->zx * ap->x_ambit;
	    }
	  else ap->x0 = 0.5 * ((ap->x1 + ap->x0) - ap->zx * ap->x_ambit); 
	  break;
	}

#if HAVE_DECL_ISNAN
      if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
      if (ap->x0 < 0.0) ap->x0 = 0.0;
      if (ap->x_ambit != 0.0)
	ap->sx = (double)(ap->x0 - ap->xmin) / (double)(ap->x_ambit);
    }
  apply_x_axis_change(ap, cp);
}


void sx_incremented(chan_info *cp, double amount)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sx += (ap->zx * amount);
  if (ap->sx < 0.0) ap->sx = 0.0;
  if (ap->sx > (1.0 - ap->zx)) ap->sx = 1.0 - ap->zx;
  apply_x_axis_change(ap, cp);
  resize_sx(cp);
}


void zx_incremented(chan_info *cp, double amount)
{ /* kbd arrows etc -- needs to be able to return to original */
  axis_info *ap;
  mus_long_t samps;
  samps = CURRENT_SAMPLES(cp);
  ap = cp->axis;
  if ((amount >= 1.0) || ((samps > 0) && ((ap->zx * (double)samps) > (amount / 2.0))))
    {
      ap->zx *= amount;
      focus_x_axis_change(ap, cp, zoom_focus_style(ss));
      /* apply_x_axis_change(ap, cp); */
      resize_sx(cp);
    }
}


int move_axis(chan_info *cp, axis_info *ap, int x)
{
  /* need to scroll axis forward or backward -- distance per call depends on x distance from end of axis */
  double off;
  int nx;
  if (x > ap->x_axis_x1)
    {
      off = x - ap->x_axis_x1;
      ap->sx += (off * ap->zx / 1000.0);
      if (ap->sx > (1.0 - ap->zx)) ap->sx = 1.0 - ap->zx;
      nx = ap->x_axis_x1;
    }
  else
    {
      off = ap->x_axis_x0 - x;
      ap->sx -= (off * ap->zx / 1000.0);
      if (ap->sx < 0.0) ap->sx = 0.0;
      nx = ap->x_axis_x0;
    }
  apply_x_axis_change(ap, cp);
  resize_sx(cp);
  return(nx);
}


void set_axes(chan_info *cp, double x0, double x1, mus_float_t y0, mus_float_t y1)
{
  /* use to change channel_style to channels_separate, and restore axes upon update */
  axis_info *ap;
  ap = cp->axis;
  ap->x0 = x0;
  ap->x1 = x1;
  if (ap->x_ambit != 0.0)
    {
      ap->zx = (x1 - x0) / ap->x_ambit;
      ap->sx = (x0 - ap->xmin) / ap->x_ambit;
    }
  resize_sx(cp);
  ap->y0 = y0;
  ap->y1 = y1;
  if (ap->y_ambit != 0.0)
    {
      ap->zy = (y1 - y0) / ap->y_ambit;
      ap->sy = (y0 - ap->ymin) / ap->y_ambit;
    }
  resize_sy_and_zy(cp);
}


/* these are copies from snd-axis.c; didn't want to use macros here */

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


static void display_y_zero(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if ((ap->y0 < 0.0) && (ap->y1 > 0.0))
    {
      int zero;
      zero = local_grf_y(0.0, ap);
      draw_line(copy_context(cp), ap->x_axis_x0, zero, ap->x_axis_x1, zero);
      if (cp->printing) 
	ps_draw_line(ap, ap->x_axis_x0, 0, ap->x_axis_x1, 0);
    }
}


#if USE_MOTIF
  #define CHN_LABEL_OFFSET -5
#else
  #define CHN_LABEL_OFFSET -18
#endif

static char chn_id_str[LABEL_BUFFER_SIZE];


static void display_channel_id(chan_info *cp, int height, int chans)
{
  if (cp->show_axes == SHOW_NO_AXES) return;
  if ((chans > 1) || (cp->edit_ctr > 0))
    {
      int x0, y0;
      color_t old_color = 0;
      set_peak_numbers_font(cp);
      if (cp->printing) ps_set_peak_numbers_font();
      x0 = 5;
      y0 = height + CHN_LABEL_OFFSET;

      if (cp->edit_ctr == 0)
	mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%d]", cp->chan + 1);
      else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%d]: %d", cp->chan + 1, cp->edit_ctr);
      if (cp == selected_channel())
	{
	  old_color = get_foreground_color(copy_context(cp));
	  set_foreground_color(copy_context(cp), ss->sgx->red);
	}
      draw_string(copy_context(cp), x0, y0, chn_id_str, strlen(chn_id_str));
      if (cp->printing) 
	ps_draw_string(cp->axis, x0, y0, chn_id_str);
      if (cp == selected_channel())
	set_foreground_color(copy_context(cp), old_color);
    }
}


#if USE_MOTIF
  #define SELECTION_FFT_LABEL_OFFSET -3
#else
  #define SELECTION_FFT_LABEL_OFFSET -15
#endif


static void display_selection_transform_size(chan_info *cp, axis_info *fap)
{
  int x0, y0;
  if (fap->height < 60) return;
  set_tiny_numbers_font(cp);
  if (cp->printing) ps_set_tiny_numbers_font();
  y0 = fap->height + fap->y_offset + SELECTION_FFT_LABEL_OFFSET;
  x0 = fap->x_axis_x0 + 20;
  mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, 
	       _("(len: " MUS_LD "/" MUS_LD ")"), 
	       selection_len(), 
	       cp->selection_transform_size);
  draw_string(copy_context(cp), x0, y0, chn_id_str, strlen(chn_id_str));
  if (cp->printing) ps_draw_string(fap, x0, y0, chn_id_str);
}


snd_info *make_simple_channel_display(int srate, int initial_length, fw_button_t with_arrows, graph_style_t grf_style, widget_t container, bool with_events)
{
  snd_info *sp;
  chan_info *cp;
  file_info *hdr;
  int err;

  sp = make_basic_snd_info(1); /* 1 chan */
  sp->nchans = 1;
  sp->inuse = SOUND_WRAPPER;
  sp->active = true;
  sp->hdr = (file_info *)calloc(1, sizeof(file_info));
  sp->search_proc = XEN_UNDEFINED;
  sp->prompt_callback = XEN_UNDEFINED;
  hdr = sp->hdr;
  hdr->samples = initial_length;
  hdr->srate = srate;
  hdr->comment = NULL;
  hdr->chans = 1;
  err = add_channel_window(sp, 0, 0, 0, container, with_arrows, with_events);
  if (err == -1)
    {
      free(sp->chans);
      free(sp->hdr);
      free(sp);
      return(NULL);
    }
  cp = sp->chans[0];
  cp->editable = false;
  cp->sound = sp;
  cp->hookable = WITHOUT_HOOK;
  add_channel_data_1(cp, srate, initial_length, WITH_GRAPH);
  cp->time_graph_style = grf_style;
  cp->dot_size = dot_size(ss);
  return(sp);
}


static axis_context *combined_context(chan_info *cp);
static int make_wavogram(chan_info *cp);


static int make_graph_1(chan_info *cp, double cur_srate, bool normal, bool *two_sided)
{
  snd_info *sp;
  int j = 0;
  mus_long_t samps;
  int xi;
  axis_info *ap;
  mus_float_t samples_per_pixel, xf, pinc = 0.0;
  double x, incr;  

  /* in long files with small windows we can run into floating point errors that accumulate
   * in the loop (incrementing by a truncated amount each time), making the x amounts smaller
   * than they should be (so the graph appears squeezed).
   *
   * There is a similar problem with long files (say 1 hour at 44KHz), in which 
   * the x increment gets quantized making the graphs step-like, so the
   * axis_info fields x0, x1, and x_scale are doubles as well. The y axis is less problematic
   * since we are assuming sound files here.
   *
   * And if the data window itself is small (in the sense that we're looking at individual samples)
   * we need to make decisions about in-between sample mouse clicks, and the cursor placement
   * must match the sample placement (which means that the initial x-axis offset from the
   * first sample (similarly the last) must be taken into account).  Currently, the selection
   * definition (by mouse drag) has a sloppy rounding mechanism so that
   * within reason only samples within the selection-rectangle are in the final selection,
   * and cursor placement (mouse click) rounds to the nearest sample (snd-xchn.c cursor_round).
   *
   * And lastly, the axis x_scale double can be enormous whereas the between-sample choice can
   * be tiny, so we're pushing the arithmetic into dangerous realms.  This stuff has been tested
   * on some extreme cases (week-long 44KHz stereo), but there's always another special case...
   */

  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  axis_context *ax = NULL;

  sp = cp->sound;
  ap = cp->axis;

  /* check for no graph */
  if ((!ap) || 
      (!(ap->graph_active)) || 
      (ap->x0 == ap->x1)) 
    return(0);

  /* check for wavogram */
  if (cp->time_graph_type == GRAPH_AS_WAVOGRAM) 
    return(make_wavogram(cp)); 

  if (normal)
    {
      ap->losamp = snd_round_mus_long_t(ap->x0 * cur_srate); /* was ceil??? */
      if (ap->losamp < 0) ap->losamp = 0;
      ap->hisamp = (mus_long_t)((ap->x1 * cur_srate) + 0.5); /* + 0.5 for 1-sample case */
      if ((ap->losamp == 0) && (ap->hisamp == 0)) return(0);
    }

  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = ap->hisamp - ap->losamp + 1;
  if ((x_start == x_end) && (samps > 10)) return(0); /* must be too-tiny graph */

  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01; /* any non-zero value < 1.0 should be ok here */
  else samples_per_pixel = (mus_float_t)((double)(samps - 1) / (double)pixels);

  if (cp->printing) ps_allocate_grf_points();

  if (normal)
    {
      if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
	ax = combined_context(cp); 
      else ax = copy_context(cp);
      if (cp->printing) ps_fg(cp, ax);
    }

  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && 
       (samps < POINT_BUFFER_SIZE)) ||
      ((cp->time_graph_style == GRAPH_FILLED) && 
       (samples_per_pixel < 25.0) && 
       (samps < POINT_BUFFER_SIZE)))
    {
      int grfpts;
      /* i.e. individual samples are widely spaced, so we have to be careful about placement
       *   mouse uses grf_x so in this case we have to also (to make the cursor hit the dots etc) 
       */
      sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
      if (sf == NULL) return(0);
      incr = (double)1.0 / cur_srate;
      grfpts = (int)(ap->hisamp - ap->losamp + 1);
      if (cp->printing)
	{
	  for (j = 0, x = ((double)(ap->losamp) / cur_srate); j < grfpts; j++, x += incr)
	    {
	      mus_float_t fsamp;
	      fsamp = read_sample(sf);
	      set_grf_point(local_grf_x(x, ap), j, local_grf_y(fsamp, ap));
	      ps_set_grf_point(x, j, fsamp);
	    }
	}
      else
	{
	  for (j = 0, x = ((double)(ap->losamp) / cur_srate); j < grfpts; j++, x += incr)
	    set_grf_point(local_grf_x(x, ap), j, local_grf_y(read_sample(sf), ap));
	}
      if (normal)
	{
	  draw_grf_points(cp->dot_size, ax, j, ap, 0.0, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_grf_points(ap, j, 0.0, cp->time_graph_style, cp->dot_size);
	}
      else (*two_sided) = false;
    }
  else
    {
      /* take min, max */
      if (peak_env_usable(cp, samples_per_pixel, ap->hisamp, true, cp->edit_ctr, false)) /* true = start new background amp env process if needed */
	j = peak_env_graph(cp, ap, samples_per_pixel, (normal) ? ((int)SND_SRATE(sp)) : 1);
      else
	{
	  mus_float_t ymin, ymax;
	  mus_long_t ioff;
	  if ((ap->hisamp - ap->losamp) > (CURRENT_SAMPLES(cp) / 4))
	    {    
                                             /* we're trying to view a large portion of the (large) sound */
	      chan_context *cgx;
	      cgx = cp->cgx;
	      if (cgx->peak_env_in_progress)
		{                            /* is peak-env background process is still working on it */
		  peak_env_info *ep;
		  ep = cp->edits[cp->edit_ctr]->peak_env;
		  if ((ep) && samples_per_pixel >= (mus_float_t)(ep->samps_per_bin))
		    {                        /* and it will be useful when it finishes */
		      cp->waiting_to_make_graph = true;
		      return(0);             /* so don't run two enormous data readers in parallel */
		    }
		}
	    }
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (sf == NULL) return(0);
	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = local_grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = MIN_INIT;
	  ymax = MAX_INIT;
	  if (cp->printing) pinc = samples_per_pixel / cur_srate;
	  ap->changed = false;
	  ss->stopped_explicitly = false;
	  for (ioff = ap->losamp, xf = 0.0; ioff <= ap->hisamp; ioff++)
	    {
	      mus_float_t samp;
	      samp = read_sample(sf);
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j, 
				 local_grf_y(ymin, ap), 
				 local_grf_y(ymax, ap));
		  if (cp->printing) 
		    {
		      x += pinc; 
		      ps_set_grf_points(x, j, ymin, ymax);
		    }
		  xi++;
		  j++;
		  xf -= samples_per_pixel;
		  ymin = MIN_INIT;
		  ymax = MAX_INIT;
		  if (samps > 10000000)
		    {
		      check_for_event();
		      if ((ap->changed) || (ss->stopped_explicitly) || (cp->active < CHANNEL_HAS_EDIT_LIST))
			{
			  ss->stopped_explicitly = false;
			  ap->changed = false;
			  break;
			}
		    }
		}
	    }
	}
      if (normal)
	{
	  draw_both_grf_points(cp->dot_size, ax, j, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_both_grf_points(ap, j, cp->time_graph_style, cp->dot_size);
	}
      else (*two_sided) = true;
    }
  sf = free_snd_fd(sf);
  if ((normal) && (sp->channel_style == CHANNELS_SUPERIMPOSED))
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing)
	ps_reset_color();
    }
  if ((cp->verbose_cursor) && (cp->cursor_on) && (normal) && 
      (CURSOR(cp) >= ap->losamp) && (CURSOR(cp) <= ap->hisamp) && 
      ((sp->minibuffer_on == MINI_OFF) || (sp->minibuffer_on == MINI_CURSOR)))
    {
      show_cursor_info(cp); 
      sp->minibuffer_on = MINI_CURSOR;
    } 
  return(j);
}


int make_graph(chan_info *cp) 
{
  return(make_graph_1(cp, (double)(SND_SRATE(cp->sound)), true, NULL));
}


int make_background_graph(chan_info *cp, int srate, bool *two_sided) /* (for envelope editor) */
{
  return(make_graph_1(cp, (double)srate, false, two_sided));
}


void make_partial_graph(chan_info *cp, mus_long_t beg, mus_long_t end)
{
  /* assume here that everything is already checked and set up */
  snd_info *sp;
  int j = 0;
  mus_long_t samps;
  int xi;
  axis_info *ap;
  mus_float_t samples_per_pixel, xf;
  double x, incr, cur_srate, beg_in_seconds, end_in_seconds;
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  axis_context *ax = NULL;

  sp = cp->sound;
  cur_srate = (double)SND_SRATE(sp);
  ap = cp->axis;
  ax = copy_context(cp);
  if (!(ap->ax))
    ap->ax = cp->cgx->ax;
  if (beg > 0) beg--;

  /* get samps / pixel */
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = ap->hisamp - ap->losamp + 1;
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01; /* any non-zero value < 1.0 should be ok here */
  else samples_per_pixel = (mus_float_t)((double)(samps - 1) / (double)pixels);

  if (beg < ap->losamp) beg = ap->losamp;
  if (end > ap->hisamp) end = ap->hisamp;
  beg_in_seconds = (double)beg / cur_srate;
  end_in_seconds = (double)end / cur_srate;
  erase_rectangle(cp, ap->ax, 
		  local_grf_x(beg_in_seconds, ap), 
		  ap->y_axis_y1,
		  local_grf_x(end_in_seconds, ap) - local_grf_x(beg_in_seconds, ap) + 1, 
		  ap->y_axis_y0 - ap->y_axis_y1);

  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE)) ||
      ((cp->time_graph_style == GRAPH_FILLED) && (samples_per_pixel < 25.0) && (samps < POINT_BUFFER_SIZE)))
    {
      int grfpts;
      sf = init_sample_read(beg, cp, READ_FORWARD);
      incr = (double)1.0 / cur_srate;
      grfpts = (int)(end - beg + 1);

      for (j = 0, x = beg_in_seconds; j < grfpts; j++, x += incr)
	set_grf_point(local_grf_x(x, ap), j, local_grf_y(read_sample(sf), ap));

      draw_grf_points(cp->dot_size, ax, j, ap, 0.0, cp->time_graph_style);
    }
  else
    {
      if (peak_env_usable(cp, samples_per_pixel, ap->hisamp, true, cp->edit_ctr, false)) /* true = start new background amp env process if needed */
	j = peak_env_partial_graph(cp, ap, beg, end, samples_per_pixel, (int)SND_SRATE(sp));
      else
	{
	  mus_float_t ymin, ymax;
	  mus_long_t ioff;
	  if ((end - beg) > (CURRENT_SAMPLES(cp) / 4))
	    {    
                                             /* we're trying to view a large portion of the (large) sound */
	      chan_context *cgx;
	      cgx = cp->cgx;
	      if (cgx->peak_env_in_progress)
		{                            /* is peak-env background process is still working on it */
		  peak_env_info *ep;
		  ep = cp->edits[cp->edit_ctr]->peak_env;
		  if ((ep) && samples_per_pixel >= (mus_float_t)(ep->samps_per_bin))
		    {                        /* and it will be useful when it finishes */
		      cp->waiting_to_make_graph = true;
		      return;               /* so don't run two enormous data readers in parallel */
		    }
		}
	    }
	  sf = init_sample_read(beg, cp, READ_FORWARD);

	  j = 0;      /* graph point counter */
	  x = beg_in_seconds;
	  xi = local_grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = MIN_INIT;
	  ymax = MAX_INIT;
	  ap->changed = false;
	  for (ioff = beg, xf = 0.0; ioff <= end; ioff++)
	    {
	      mus_float_t samp;
	      samp = read_sample(sf);
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j, local_grf_y(ymin, ap), local_grf_y(ymax, ap));
		  xi++;
		  j++;
		  xf -= samples_per_pixel;
		  ymin = MIN_INIT;
		  ymax = MAX_INIT;
		}
	    }
	}
      draw_both_grf_points(cp->dot_size, ax, j, cp->time_graph_style);
    }

  sf = free_snd_fd(sf);

#if (!USE_CAIRO)
  display_selection(cp);
#endif
  display_channel_marks(cp);
  if (cp->show_y_zero) display_y_zero(cp);
      
  if ((cp->verbose_cursor) && (cp->cursor_on) &&
      (CURSOR(cp) >= beg) && (CURSOR(cp) <= end) && 
      ((sp->minibuffer_on == MINI_OFF) || (sp->minibuffer_on == MINI_CURSOR)))
    {
      show_cursor_info(cp); 
      sp->minibuffer_on = MINI_CURSOR;
    }
}


/* these next two procedures split "make_graph" into two pieces; the first
 *   gets the data to be graphed, using the amp envs and so on, and
 *   the second displays it.  (The first cp+pos may have no relation
 *   to the second cp -- allow arbitrary overlapping etc).
 */

XEN make_graph_data(chan_info *cp, int edit_pos, mus_long_t losamp, mus_long_t hisamp)
{
  int i, j = 0;
  mus_long_t samps, ioff;
  axis_info *ap;
  mus_float_t samples_per_pixel, xf;
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  mus_float_t *data = NULL, *data1 = NULL;
  int data_size = 0;

  ap = cp->axis;
  if (losamp == -1) losamp = ap->losamp;
  if (hisamp == -1) hisamp = ap->hisamp;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = hisamp - losamp + 1;
  if ((samps <= 0) || ((x_start == x_end) && (samps > 10))) return(XEN_FALSE);
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01;
  else samples_per_pixel = (mus_float_t)((double)(samps - 1) / (double)pixels);

  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && 
       (samps < POINT_BUFFER_SIZE)))
    {
      data_size = (int)samps;
      sf = init_sample_read_any(losamp, cp, READ_FORWARD, edit_pos);
      if (sf == NULL) return(XEN_FALSE); /* should this throw an error? (CHANNEL_BEING_DEALLOCATED) */
      data = (mus_float_t *)malloc(data_size * sizeof(mus_float_t));
      for (i = 0; i < data_size; i++)
	data[i] = read_sample(sf);
    }
  else
    {
      if (peak_env_usable(cp, samples_per_pixel, hisamp, false, edit_pos, true)) 
	{
	  double step, xk;
	  mus_float_t ymin, ymax;
	  int k, kk;
	  peak_env_info *ep;

	  data_size = pixels + 1;
	  data = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
	  data1 = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
	  ep = cp->edits[edit_pos]->peak_env;
	  step = samples_per_pixel / (mus_float_t)(ep->samps_per_bin);
	  xf = (double)(losamp) / (double)(ep->samps_per_bin);
	  j = 0;
	  ioff = losamp;
	  xk = (double)ioff;
	  ymin = ep->fmax;
	  ymax = ep->fmin;

	  while (ioff <= hisamp)
	    {
	      k = (int)xf;
	      xf += step;
	      kk = (int)xf;
	      if (kk >= ep->peak_env_size) 
		kk = ep->peak_env_size - 1;
	      for (; k <= kk; k++)
		{
		  if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
		  if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
		}
	      xk += samples_per_pixel;
	      ioff = (mus_long_t)xk;
	      data[j] = ymin;
	      data1[j] = ymax;
	      j++;
	      ymin = ep->fmax;
	      ymax = ep->fmin;
	    }
	}
      else
	{
	  mus_float_t ymin, ymax, samp;

	  data_size = pixels + 1;
	  sf = init_sample_read_any(losamp, cp, READ_FORWARD, edit_pos);
	  if (sf == NULL) return(XEN_FALSE);
	  data = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
	  data1 = (mus_float_t *)calloc(data_size, sizeof(mus_float_t));
	  j = 0;      /* graph point counter */
	  ymin = 100.0;
	  ymax = -100.0;
	  ss->stopped_explicitly = false;

	  for (ioff = losamp, xf = 0.0; ioff <= hisamp; ioff++)
	    {
	      samp = read_sample(sf);
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  data[j] = ymin;
		  data1[j] = ymax;
		  j++;
		  xf -= samples_per_pixel;
		  ymin = 100.0;
		  ymax = -100.0;
		  if (samps > 10000000)
		    {
		      if ((ap->changed) || (ss->stopped_explicitly))
			{
			  ss->stopped_explicitly = false;
			  ap->changed = false;
			  break;
			}
		    }
		}
	    }
	}
    }

  sf = free_snd_fd(sf); 
  if (data1)
    return(XEN_LIST_2(xen_make_vct(data_size, data),
		      xen_make_vct(data_size, data1)));
  else return(xen_make_vct(data_size, data));
}


void draw_graph_data(chan_info *cp, mus_long_t losamp, mus_long_t hisamp, int data_size, mus_float_t *data, mus_float_t *data1, axis_context *ax, graph_style_t style)
{
  mus_long_t i, samps;
  int xi;
  axis_info *ap;
  snd_info *sp;
  double x, incr;  

  ap = cp->axis;
  sp = cp->sound;
  if (data1 == NULL)
    {
      double start_time = 0.0, cur_srate = 1.0;

      cur_srate = (double)SND_SRATE(sp);
      if (losamp == -1)
	losamp = snd_round_mus_long_t(ap->x0 * cur_srate);
      start_time = (double)(losamp) / cur_srate;
      if (hisamp == -1)
	hisamp = (mus_long_t)(ap->x1 * cur_srate);
      samps = hisamp - losamp + 1;
      if (samps <= 0) return;
      if (samps > data_size) samps = data_size;
      incr = (double)1.0 / cur_srate;

      for (i = 0, x = start_time; i < samps; i++, x += incr)
	set_grf_point(local_grf_x(x, ap), 
		      i, 
		      local_grf_y(data[i], ap));
      draw_grf_points(cp->dot_size, ax, samps, ap, 0.0, style);
    }
  else
    {
      if (losamp == -1)
	losamp = 0;
      if (hisamp == -1)
	hisamp = data_size - 1;
      samps = hisamp - losamp + 1;
      if (samps <= 0) return;
      if (samps > data_size) samps = data_size;
      for (i = 0, xi = (ap->x_axis_x0 + losamp); i < samps; i++, xi++)
	set_grf_points(xi, i, 
		       local_grf_y(data[i], ap), 
		       local_grf_y(data1[i], ap));
      draw_both_grf_points(cp->dot_size, ax, samps, style);
    }
}


static int compare_peak_amps(const void *pk1, const void *pk2)
{
  if (((fft_peak *)pk1)->amp > ((fft_peak *)pk2)->amp) return(-1);
  else if (((fft_peak *)pk1)->amp == ((fft_peak *)pk2)->amp) return(0);
  return(1);
}


static char ampstr[LABEL_BUFFER_SIZE];
#define AMP_ROOM 45
#define AMP_ROOM_CUTOFF 3.0

static void display_peaks(chan_info *cp, axis_info *fap, mus_float_t *data, 
			  mus_long_t start, mus_long_t end,  /* start is not used? */
			  mus_long_t losamp, mus_long_t hisamp, 
			  mus_float_t samps_per_pixel, bool fft_data, mus_float_t fft_scale /* fourier scale factor or 0.0 */)
{
  int num_peaks, row, frq_col, frq_strlen, tens, i, amp_col, amp_strlen, row_height = 15;
  mus_long_t samps;
  bool with_amps;
  mus_float_t amp0;
  axis_context *ax;
  fft_peak *peak_freqs = NULL;
  fft_peak *peak_amps = NULL;

  /* "end" is the top displayed frequency in the FFT frequency axis 
   * "hisamp - losamp" is how many samples of data we have
   */

  /* "tens" is for prettyf in snd-utils, if -1 -> print int else sets decimals */
  samps = hisamp - losamp;
  if (samps > (end * 10)) 
    tens = 2; 
  else 
    if (samps > end) 
      tens = 1; 
    else
      if (samps > (end / 10)) 
	tens = 0; 
      else tens = -1;

#if (!USE_GTK)
  row_height = (int)(1.25 * number_height(PEAKS_FONT(ss)));
#else
  row_height = (int)(0.75 * number_height(PEAKS_FONT(ss)));
#endif
  if (row_height < 10) row_height = 10;

  num_peaks = (fap->y_axis_y0 - fap->y_axis_y1) / (row_height + 5);
  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    num_peaks /= cp->sound->nchans;
  if (num_peaks <= 0) return;
  if (num_peaks > cp->max_transform_peaks) num_peaks = cp->max_transform_peaks;

  peak_freqs = (fft_peak *)calloc(cp->max_transform_peaks, sizeof(fft_peak));
  peak_amps = (fft_peak *)calloc(cp->max_transform_peaks, sizeof(fft_peak));

  if (fft_data)
    num_peaks = find_and_sort_transform_peaks(data, peak_freqs, num_peaks, losamp, hisamp /* "fft size" */, samps_per_pixel, fft_scale); /* srate 1.0=>freqs between 0 and 1.0 */
  else num_peaks = find_and_sort_peaks(data, peak_freqs, num_peaks, losamp, hisamp);

  if ((num_peaks == 0) || 
      ((num_peaks == 1) && 
       ((peak_freqs[0].freq == 1.0) || (peak_freqs[0].freq == 0.0))))
    {
      free(peak_freqs); 
      free(peak_amps); 
      return;
    }

  frq_strlen = (int)ceil(log10(peak_freqs[num_peaks - 1].freq * end)) + ((tens <= 0) ? 0 : (tens + 1)) + 1;
  if (frq_strlen < 1) frq_strlen = 1;

  with_amps = (fap->width > ((30 + 5 * frq_strlen + AMP_ROOM) * AMP_ROOM_CUTOFF));
  amp_strlen = 3;

  frq_col = fap->x_axis_x1 - frq_strlen * 5; 
  amp_col = fap->x_axis_x1 - AMP_ROOM + 15;

  if (with_amps) 
    {
      frq_col -= AMP_ROOM;
      if ((fft_data) && 
	  ((cp->transform_normalization == DONT_NORMALIZE) ||
	   (cp->min_dB < -60)))
	{
	  frq_col -= 5;
	  amp_col -= 5;
	  amp_strlen = 4;
	}
    }

  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    ax = combined_context(cp);
  else ax = copy_context(cp);

  if (num_peaks > 6)
    {
      for (i = 0; i < num_peaks; i++) peak_amps[i] = peak_freqs[i];
      qsort((void *)peak_amps, num_peaks, sizeof(fft_peak), compare_peak_amps);
      if (num_peaks < 12) amp0 = peak_amps[2].amp; else amp0 = peak_amps[5].amp;
      set_bold_peak_numbers_font(cp);
      if (cp->printing) ps_set_bold_peak_numbers_font();

#if (!USE_GTK)
      row = fap->y_axis_y1 + row_height;
#else
      row = fap->y_axis_y1;
#endif
      if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
	row += row_height * num_peaks * cp->chan;

      for (i = 0; i < num_peaks; i++)
	{
	  if (peak_freqs[i].amp >= amp0)
	    {
	      char *fstr;
	      mus_float_t px;

	      px = peak_freqs[i].freq;
	      fstr = prettyf(px * end, tens);
	      draw_string(ax, frq_col, row, fstr, strlen(fstr));
	      if (cp->printing) ps_draw_string(fap, frq_col, row, fstr);
	      free(fstr);
	      fstr = NULL;

	      if (with_amps)
		{
		  if ((fft_data) && (cp->fft_log_magnitude))
		    mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.1f", in_dB(cp->min_dB, cp->lin_dB, peak_freqs[i].amp));
		  else mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.*f", amp_strlen, peak_freqs[i].amp);
		  draw_string(ax, amp_col, row, ampstr, strlen(ampstr));
		  if (cp->printing) 
		    ps_draw_string(fap, amp_col, row, ampstr);
		}
	    }
	  row += row_height;
	}
    }
  else amp0 = 100.0;

  set_peak_numbers_font(cp);
  if (cp->printing) ps_set_peak_numbers_font();
  /* choose a small font for these numbers */
#if (!USE_GTK)
  row = fap->y_axis_y1 + row_height;
#else
  row = fap->y_axis_y1;
#endif
  if (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)
    row += row_height * num_peaks * cp->chan;

  for (i = 0; i < num_peaks; i++)
    {
      if (peak_freqs[i].amp < amp0)
	{
	  mus_float_t px;
	  char *fstr;

	  px = peak_freqs[i].freq;
	  fstr = prettyf(px * end, tens);
	  draw_string(ax, frq_col, row, fstr, strlen(fstr));
	  if (cp->printing) ps_draw_string(fap, frq_col, row, fstr);
	  free(fstr);
	  fstr = NULL;

	  if (with_amps)
	    {
	      if ((fft_data) && (cp->fft_log_magnitude))
		mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.1f", in_dB(cp->min_dB, cp->lin_dB, peak_freqs[i].amp));
	      else mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.*f", amp_strlen, peak_freqs[i].amp);
	      draw_string(ax, amp_col, row, ampstr, strlen(ampstr));
	      if (cp->printing) 
		ps_draw_string(fap, amp_col, row, ampstr);
	    }
	}
      row += row_height;
    }

  /* superimposed context reset takes place in make_fft_graph */

  if (peak_freqs) free(peak_freqs); 
  if (peak_amps) free(peak_amps);
}


static void make_fft_graph(chan_info *cp, axis_info *fap, axis_context *ax, with_hook_t with_hook)
{
  /* axes are already set, data is in the fft_info struct -- don't reset here! */
  /* since the fft size menu callback can occur while we are calculating the next fft, we have to lock the current size until the graph goes out */
  fft_info *fp;
  snd_info *sp;
  mus_float_t *data;
  mus_float_t incr, x, scale;
  mus_long_t i, j = 0, hisamp, losamp = 0;
  int lines_to_draw = 0;
  mus_float_t samples_per_pixel, xf, ina, ymax;
  int logx, logy;
  mus_float_t pslogx, pslogy;
  mus_float_t saved_data = 0.0;
  mus_float_t minlx = 0.0, curlx = 0.0, fap_range, log_range, lscale = 1.0;
  mus_float_t *fft_phases = NULL;
  bool free_phases = false;

  sp = cp->sound;
  fp = cp->fft;
  if (chan_fft_in_progress(cp)) return;
  if (!fap->graph_active) return;
  data = fp->data;
  /* these lo..hi values are just for upcoming loops -- not axis info */

  if (cp->transform_type == FOURIER)
    {
      hisamp = fp->current_size * cp->spectrum_end / 2;
      if ((cp->fft_log_frequency) && 
	  ((SND_SRATE(sp) * 0.5 * cp->spectrum_start) < log_freq_start(ss)))
	losamp = (mus_long_t)(ceil(fp->current_size * log_freq_start(ss) / (mus_float_t)SND_SRATE(sp)));
      else losamp = fp->current_size * cp->spectrum_start / 2;
      incr = (mus_float_t)SND_SRATE(sp) / (mus_float_t)(fp->current_size);
    }
  else
    {
      /* hisamp here is in terms of transform values, not original sampled data values */
      hisamp = fp->current_size * cp->spectrum_end;
      losamp = fp->current_size * cp->spectrum_start;
      incr = 1.0;
    }
  if ((losamp < 0) || (losamp >= hisamp)) return;

  /* no scaling etc here!! see snd_display_fft in snd-fft.c */
  scale = fp->scale;
  if (cp->printing) ps_allocate_grf_points();
  samples_per_pixel = (mus_float_t)((double)(hisamp - losamp) / (mus_float_t)(fap->x_axis_x1 - fap->x_axis_x0));
  if (cp->printing) ps_fg(cp, ax);

  if (cp->fft_log_frequency)
    {
      mus_float_t maxlx, max_data;

      fap_range = fap->x1 - fap->x0;
      if (fap->x0 > 1.0) minlx = log(fap->x0); else minlx = 0.0;
      maxlx = log(fap->x1);
      log_range = (maxlx - minlx);
      lscale = fap_range / log_range;

      /* I think this block is getting the max of all the DC to losamp freqs to represent the 0th point of the graph?? */
      max_data = data[0];
      if ((spectrum_start(ss) == 0.0) && (losamp > 0))
	{
	  for (i = 1; i <= losamp; i++)
	    if (data[i] > max_data)
	      max_data = data[i];
	}
      saved_data = data[losamp];
      data[losamp] = max_data;
    }

  if ((samples_per_pixel < 4.0) &&
      ((hisamp - losamp) < POINT_BUFFER_SIZE)) /* point buffer size in snd-draw.c:
						*     hisamp - losamp can be anything (huge fft), and x1 - x0 can be > 2400 (very large screen) 
						*     so we have to check hisamp or make the buffer bigger 
						*/
    {
      /* save phases if needed */
      if ((cp->fft_with_phases) &&
	  (fp->phases) &&
	  (cp->transform_type == FOURIER))
	{
	  if (losamp == 0)
	    {
	      fft_phases = fp->phases;
	      free_phases = false;
	    }
	  else
	    {
	      mus_long_t size;
	      size = hisamp - losamp + 1;
	      fft_phases = (mus_float_t *)malloc(size * sizeof(mus_float_t));
	      memcpy((void *)fft_phases, (void *)(&(fp->phases[losamp])), size * sizeof(mus_float_t));
	      free_phases = true;
	    }
	}

      /* get fft graph points */
      if ((!(cp->fft_log_magnitude)) && 
	  (!(cp->fft_log_frequency)))
	{
	  if (losamp == 0)
	    {
	      for (i = 0, x = 0.0; i < hisamp; i++, x += incr)
		{
		  mus_float_t scaled_data;
		  scaled_data = data[i] * scale;
		  set_grf_point(local_grf_x(x, fap), i, local_grf_y(scaled_data, fap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i, scaled_data);
		}
	    }
	  else
	    {
	      for (i = losamp, x = fap->x0; i < hisamp; i++, x += incr)
		{
		  mus_float_t scaled_data;
		  scaled_data = data[i] * scale;
		  set_grf_point(local_grf_x(x, fap), 
				i - losamp, 
				local_grf_y(scaled_data, fap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i - losamp, scaled_data);
		}
	    }
	}
      else
	{
	  /* either log freq or log magnitude or both */
	  for (i = losamp, x = fap->x0; i < hisamp; i++, x += incr)
	    {
	      mus_float_t scaled_data;
	      scaled_data = data[i] * scale;

	      if (cp->fft_log_frequency) 
		{
		  if (x > 1.0) curlx = log(x); else curlx = 0.0;
		  logx = local_grf_x(fap->x0 + lscale * (curlx - minlx), fap);
		}
	      else logx = local_grf_x(x, fap);

	      if (cp->fft_log_magnitude) 
		logy = local_grf_y(in_dB(cp->min_dB, cp->lin_dB, scaled_data), fap); 
	      else logy = local_grf_y(scaled_data, fap);

	      set_grf_point(logx, i - losamp, logy);
	      if (cp->printing) 
		{
		  if (cp->fft_log_frequency) 
		    pslogx = fap->x0 + lscale * (curlx - minlx);
		  else pslogx = x;
		  if (cp->fft_log_magnitude) 
		    pslogy = in_dB(cp->min_dB, cp->lin_dB, scaled_data); 
		  else pslogy = scaled_data;
		  ps_set_grf_point(pslogx, i - losamp, pslogy);
		}
	    }
	}

      lines_to_draw = i - losamp;
    }
  else
    {
      if ((cp->fft_with_phases) &&
	  (fp->phases) &&
	  (cp->transform_type == FOURIER))
	{
	  fft_phases = (mus_float_t *)malloc(POINT_BUFFER_SIZE * sizeof(mus_float_t));
	  free_phases = true;
	}

      j = 0;      /* graph point counter */
      i = losamp;
      if (losamp == 0) 
	x = 0.0; 
      else x = fap->x0;
      xf = 0.0;     /* samples per pixel counter */
      ymax = MAX_INIT;

      if ((!(cp->fft_log_magnitude)) && 
	  (!(cp->fft_log_frequency)))
	{
	  while (i < hisamp)
	    {
	      ina = data[i];
	      if (ina > ymax) 
		{
		  ymax = ina;
		  if (fft_phases) 
		    fft_phases[j] = fp->phases[i];
		}
	      xf += 1.0;
	      i++;
	      if (xf > samples_per_pixel)
		{
		  mus_float_t scaled_data;
		  scaled_data = ymax * scale;

		  set_grf_point(local_grf_x(x, fap), j, local_grf_y(scaled_data, fap));
		  x += (incr * samples_per_pixel); 

		  if (cp->printing) 
		    ps_set_grf_point(x, j, scaled_data);

		  j++;
		  xf -= samples_per_pixel;
		  ymax = MAX_INIT;
		}
	    }
	}
      else
	{
	  while (i < hisamp)
	    {
	      ina = data[i];
	      if (ina > ymax) 
		{
		  ymax = ina;
		  if (fft_phases) 
		    fft_phases[j] = fp->phases[i];
		}

	      xf += 1.0;
	      i++;
	      if (xf > samples_per_pixel)
		{
		  mus_float_t scaled_data;
		  scaled_data = ymax * scale;

		  if (cp->fft_log_frequency) 
		    {
		      if (x > 1.0) curlx = log(x); else curlx = 0.0;
		      logx = local_grf_x(fap->x0 + lscale * (curlx - minlx), fap);
		    }
		  else logx = local_grf_x(x, fap);

		  if (cp->fft_log_magnitude) 
		    logy = local_grf_y(in_dB(cp->min_dB, cp->lin_dB, scaled_data), fap); 
		  else logy = local_grf_y(scaled_data, fap);

		  set_grf_point(logx, j, logy);

		  if (cp->printing) 
		    {
		      if (cp->fft_log_frequency) 
			pslogx = fap->x0 + lscale * (curlx - minlx);
		      else pslogx = x;
		      if (cp->fft_log_magnitude) 
			pslogy = in_dB(cp->min_dB, cp->lin_dB, scaled_data); 
		      else pslogy = scaled_data;
		      ps_set_grf_point(pslogx, j, pslogy);
		    }

		  x += (incr * samples_per_pixel);
		  j++;
		  xf -= samples_per_pixel;
		  ymax = MAX_INIT;
		}
	    }
	}
      lines_to_draw = j;
    }
    
  if ((cp->fft_with_phases) &&
      (fft_phases) &&
      (cp->transform_type == FOURIER))
    {
      int k;
      int *colors;
      color_t default_color;
      
      if (cp == selected_channel())
	default_color = ss->sgx->selected_data_color;
      else default_color = ss->sgx->data_color;
      
      /* if value is close to 0, use [selected-]data-color, else use fft_phases[i] */
      allocate_color_map(PHASES_COLORMAP);
      colors = (int *)malloc(lines_to_draw * sizeof(int));
      
      for (k = 0; k < lines_to_draw; k++)
	{
	  if (fft_phases[k] < 0.0) fft_phases[k] += 2.0 * M_PI;                    /* we want 0..2pi, not -pi..pi */
	  colors[k] = (int)((fft_phases[k] * color_map_size(ss)) / (2.0 * M_PI));
	}
      
      draw_colored_lines(cp, ax, get_grf_points(), lines_to_draw, colors, fap->y_axis_y0, default_color);
      
      free(colors);
      if (free_phases)
	free(fft_phases);
    }
  else draw_grf_points(cp->dot_size, ax, lines_to_draw, fap, 0.0, cp->transform_graph_style);
  
  if (cp->printing) 
    ps_draw_grf_points(fap, lines_to_draw, 0.0, cp->transform_graph_style, cp->dot_size);

  if (cp->fft_log_frequency)
    data[losamp] = saved_data;

  if (cp->show_transform_peaks) 
    {
      if (cp->transform_type == FOURIER)
	display_peaks(cp, fap, data, 
		      SND_SRATE(sp) * cp->spectrum_start / 2,
		      SND_SRATE(sp) * cp->spectrum_end / 2, 
		      losamp, hisamp, 
		      samples_per_pixel, true, scale);
      else display_peaks(cp, fap, data, 
			 fp->current_size * cp->spectrum_start, 
			 fp->current_size * cp->spectrum_end, 
			 losamp, hisamp, 
			 samples_per_pixel, true, 0.0);
    }

  if (cp->selection_transform_size != 0) 
    display_selection_transform_size(cp, fap);

  if (sp->channel_style == CHANNELS_SUPERIMPOSED)
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing) ps_reset_color();
    }

  if (with_hook == WITH_HOOK) after_transform(cp, scale);
}


static int skew_color(mus_float_t x)
{
  mus_float_t base, val;
  int pos;
  if (x < color_cutoff(ss)) return(NO_COLOR);
  if (color_inverted(ss))   
    val = 1.0 - x;
  else val = x;
  base = color_scale(ss);
  if ((base > 0.0) && (base != 1.0))
    val = (pow(base, val) - 1.0) / (base - 1.0);
  pos = (int)(val * color_map_size(ss));
  if (pos > color_map_size(ss)) return(color_map_size(ss) - 1);
  if (pos > 0)
    return(pos - 1);
  return(0);
}


static void make_sonogram(chan_info *cp)
{ 
  #define SAVE_FFT_SIZE 4096
  sono_info *si;
  static int *sono_js = NULL;
  static int sono_js_size = 0;

  if (chan_fft_in_progress(cp)) return;
  si = cp->sonogram_data;

  if ((si) && (si->scale > 0.0))
    {
      int i, slice, fwidth, fheight, j, bins;
      int rectw, recth;
      axis_info *fap;
      mus_float_t xf, xfincr, yf, yfincr, frectw, frecth, xscl, scl = 1.0;
      mus_float_t *hfdata;
      int *hidata;
      axis_context *ax;
      mus_float_t minlx = 0.0, curlx = 0.0, lscale = 1.0;

      ax = copy_context(cp);
      fap = cp->fft->axis;
      fwidth = fap->x_axis_x1 - fap->x_axis_x0;        /* these are the corners */
      fheight = fap->y_axis_y0 - fap->y_axis_y1;
      if ((fwidth == 0) || (fheight == 0)) return;

      bins = (int)(si->target_bins * cp->spectrum_end);
      if (bins == 0) return;

      if (cp->cgx->fft_pix)                            /* Motif None = 0 */
	{
	  if ((cp->fft_changed == FFT_UNCHANGED) &&
	      (cp->cgx->fft_pix_ready) &&
	      (cp->cgx->fft_pix_cutoff == cp->spectrum_end) &&
	      ((int)(cp->cgx->fft_pix_width) == fwidth) &&
	      ((int)(cp->cgx->fft_pix_height) == fheight) &&
	      (cp->cgx->fft_pix_x0 == fap->x_axis_x0) &&
	      (cp->cgx->fft_pix_y0 == fap->y_axis_y1)) /* X is upside down */
	    {
	      if (restore_fft_pix(cp, ax))             /* copy pix into drawing area and return */
		return;
	    }
	  else
	    {
	      free_fft_pix(cp);
	    }
	  cp->cgx->fft_pix_ready = false;
	}

      if (sono_js_size != color_map_size(ss))
	{
	  if (sono_js) free(sono_js);
	  sono_js_size = color_map_size(ss);
	  sono_js = (int *)calloc(sono_js_size, sizeof(int));
	}
      if (cp->printing) ps_allocate_grf_points();
      allocate_sono_rects(si->total_bins);
      allocate_color_map(color_map(ss));

      scl = si->scale; 
      frectw = (mus_float_t)fwidth / (mus_float_t)(si->target_slices);
      frecth = (mus_float_t)fheight / (mus_float_t)bins;
      xscl = (mus_float_t)(fap->x1 - fap->x0) / (mus_float_t)(si->target_slices);
      rectw = (int)(ceil(frectw));
      recth = (int)(ceil(frecth));
      if (rectw == 0) rectw = 1;
      if (recth == 0) recth = 1;

      hfdata = (mus_float_t *)malloc((bins + 1) * sizeof(mus_float_t));
      hidata = (int *)malloc((bins + 1) * sizeof(int));

      if (cp->transform_type == FOURIER)
	{
	  if (cp->fft_log_frequency)
	    {
	      mus_float_t maxlx, fap_range, log_range;
	      fap_range = fap->y1 - fap->y0;
	      if (fap->y0 > 1.0) minlx = log(fap->y0); else minlx = 0.0;
	      maxlx = log(fap->y1);
	      log_range = (maxlx - minlx);
	      lscale = fap_range / log_range;
	    }
	  yfincr = cp->spectrum_end * (mus_float_t)SND_SRATE(cp->sound) * 0.5 / (mus_float_t)bins;
	}
      else yfincr = 1.0;

      if (cp->fft_log_frequency)
	{
	  for (yf = 0.0, i = 0; i <= bins; i++, yf += yfincr)
	    {
	      if (yf > 1.0) curlx = log(yf); else curlx = 0.0;
	      hfdata[i] = fap->y0 + lscale * (curlx - minlx);
	      hidata[i] = local_grf_y(hfdata[i], fap);
	    }
	}
      else
	{
	  for (yf = 0.0, i = 0; i <= bins; i++, yf += yfincr)
	    {
	      hfdata[i] = yf;
	      hidata[i] = local_grf_y(yf, fap);
	    }
	}

      xfincr = ((mus_float_t)fwidth / (mus_float_t)(si->target_slices));
      xf = 2 + fap->x_axis_x0;
      ss->stopped_explicitly = false;

      for (slice = 0; slice < si->active_slices; slice++, xf += xfincr)
	{
	  mus_float_t *fdata;

	  memset((void *)sono_js, 0, color_map_size(ss) * sizeof(int));
	  fdata = si->data[slice];

	  for (i = 0; i < bins; i++)
	    {
	      mus_float_t binval;
	      /* above is fdata[i-1], left is si->data[slice-1][i] */
	      binval = fdata[i] / scl;
	      if (cp->fft_log_magnitude) binval = 1.0 - (in_dB(cp->min_dB, cp->lin_dB, binval)) / cp->min_dB;
	      j = skew_color(binval);
	      if (j != NO_COLOR)
		{
		  if (cp->fft_log_frequency)
		    set_sono_rectangle(sono_js[j], j, (int)xf, hidata[i + 1], rectw, (int)(hidata[i] - hidata[i + 1]));
		  else set_sono_rectangle(sono_js[j], j, (int)xf, hidata[i + 1], rectw, recth);
		  if (cp->printing)
		    {
		      if (cp->fft_log_frequency) 
			ps_draw_sono_rectangle(fap, j, fap->x0 + xscl * slice, hfdata[i + 1], frectw, hidata[i] - hidata[i + 1]);
		      else ps_draw_sono_rectangle(fap, j, fap->x0 + xscl * slice, hfdata[i + 1], frectw, -frecth);
		    }
		  sono_js[j]++;
		}
	    }

	  for (i = 0; i < color_map_size(ss); i++)
	    if (sono_js[i] > 0) 
	      draw_sono_rectangles(ax, i, sono_js[i]);

	  if (cp->printing)
	    {
	      check_for_event();
	      if ((ss->stopped_explicitly) || (cp->active < CHANNEL_HAS_EDIT_LIST)) /* user closed file while trying to print */
		{
		  ss->stopped_explicitly = false;
		  string_to_minibuffer(cp->sound, _("stopped"));
		  break;
		}
	    }
	}

      /* if size was wrong, we've already released pix above */
      if ((bins >= SAVE_FFT_SIZE) ||      /* transform size of twice that (8192) */
	  ((enved_dialog_is_active()) &&  /*   or we want to see the sonogram as the enved background */
	   (enved_target(ss) == ENVED_SPECTRUM) &&
	   (enved_wave_p(ss))))
	save_fft_pix(cp, ax, fwidth, fheight, fap->x_axis_x0, fap->y_axis_y1);

      if (cp->printing) ps_reset_color();
      free(hfdata);
      free(hidata);
      if (cp->hookable == WITH_HOOK) after_transform(cp, 1.0 / scl);
    }
}


static void rotate_matrix(mus_float_t xangle, mus_float_t yangle, mus_float_t zangle, mus_float_t xscl, mus_float_t yscl, mus_float_t zscl, mus_float_t *mat)
{
  /* return rotation matrix for rotation through angles xangle, yangle, then zangle with scaling by xscl, yscl, zscl */
  mus_float_t sinx, siny, sinz, cosx, cosy, cosz, deg;
  mus_float_t x, y, z;
  deg = 2.0 * M_PI / 360.0;
  x = xangle * deg;
  y = yangle * deg;
  z = zangle * deg;
  sinx = sin(x);
  siny = sin(y);
  sinz = sin(z);
  cosx = cos(x);
  cosy = cos(y);
  cosz = cos(z);
  mat[0] = cosy * cosz * xscl;
  mat[1] = (sinx * siny * cosz - cosx * sinz) * yscl;
  mat[2] = (cosx * siny * cosz + sinx * sinz) * zscl;
  mat[3] = cosy * sinz * xscl;
  mat[4] = (sinx * siny * sinz + cosx * cosz) * yscl;
  mat[5] = (cosx * siny * sinz - sinx * cosz) * zscl;
  mat[6] = -siny * xscl;
  mat[7] = sinx * cosy * yscl;
  mat[8] = cosx * cosy * zscl;
}


static void rotate(mus_float_t *xyz, mus_float_t *mat)
{ /* use rotation/scaling matrix set up by rotate_matrix to rotate and scale the vector xyz */
  mus_float_t x, y, z;
  x = xyz[0];
  y = xyz[1];
  z = xyz[2];
  xyz[0] = mat[0] * x + mat[1] * y + mat[2] * z;
  xyz[1] = mat[3] * x + mat[4] * y + mat[5] * z;
  xyz[2] = mat[6] * x + mat[7] * y + mat[8] * z;
}


#if HAVE_GL

void reset_spectro(void)
{
  /* only used in orientation dialog button and Enter key binding */
  set_spectrum_end(DEFAULT_SPECTRUM_END);
  set_spectrum_start(DEFAULT_SPECTRUM_START);
  set_spectro_hop(DEFAULT_SPECTRO_HOP);
  set_spectro_x_angle((with_gl(ss)) ? DEFAULT_SPECTRO_X_ANGLE : 90.0);
  set_spectro_y_angle((with_gl(ss)) ? DEFAULT_SPECTRO_Y_ANGLE : 0.0);
  set_spectro_z_angle((with_gl(ss)) ? DEFAULT_SPECTRO_Z_ANGLE : 358.0);
  set_spectro_x_scale((with_gl(ss)) ? DEFAULT_SPECTRO_X_SCALE : 1.0);
  set_spectro_y_scale((with_gl(ss)) ? DEFAULT_SPECTRO_Y_SCALE : 1.0);
  set_spectro_z_scale((with_gl(ss)) ? DEFAULT_SPECTRO_Z_SCALE : 0.1);
}

#if HAVE_GL
  #include <GL/glu.h>
#endif


static GLdouble unproject_to_x(int x, int y)
{
#if HAVE_GLU
  /* taken from GL doc p152 */
  GLint viewport[4];
  GLdouble mv[16], proj[16];
  GLint realy;
  GLdouble wx = 0.0, wy, wz;
  glGetIntegerv(GL_VIEWPORT, viewport);
  glGetDoublev(GL_MODELVIEW_MATRIX, mv);
  glGetDoublev(GL_PROJECTION_MATRIX, proj);
  realy = viewport[3] - (GLint)y - 1;
  gluUnProject((GLdouble)x, (GLdouble)realy, 0.0, mv, proj, viewport, &wx, &wy, &wz);
  return(wx);
#else
  return(0.0);
#endif
}


static GLdouble unproject_to_y(int x, int y)
{
#if HAVE_GLU
  /* taken from GL doc p152 */
  GLint viewport[4];
  GLdouble mv[16], proj[16];
  GLint realy;
  GLdouble wx, wy = 0.0, wz;
  glGetIntegerv(GL_VIEWPORT, viewport);
  glGetDoublev(GL_MODELVIEW_MATRIX, mv);
  glGetDoublev(GL_PROJECTION_MATRIX, proj);
  realy = viewport[3] - (GLint)y - 1;
  gluUnProject((GLdouble)x, (GLdouble)realy, 0.0, mv, proj, viewport, &wx, &wy, &wz);
  return(wy);
#else
  return(0.0);
#endif
}

#else

void reset_spectro(void)
{
  /* only used in orientation dialog button and Enter key binding */
  set_spectrum_end(DEFAULT_SPECTRUM_END);
  set_spectrum_start(DEFAULT_SPECTRUM_START);
  set_spectro_hop(DEFAULT_SPECTRO_HOP);
  set_spectro_x_angle(DEFAULT_SPECTRO_X_ANGLE);
  set_spectro_y_angle(DEFAULT_SPECTRO_Y_ANGLE);
  set_spectro_z_angle(DEFAULT_SPECTRO_Z_ANGLE);
  set_spectro_x_scale(DEFAULT_SPECTRO_X_SCALE);
  set_spectro_y_scale(DEFAULT_SPECTRO_Y_SCALE);
  set_spectro_z_scale(DEFAULT_SPECTRO_Z_SCALE);
}
#endif


static void display_channel_lisp_data(chan_info *cp);
static void make_axes(chan_info *cp, axis_info *ap, x_axis_style_t x_style, bool erase_first, with_grid_t grid, log_axis_t log_axes, show_axes_t axes);

#define DONT_CLEAR_GRAPH false
#define CLEAR_GRAPH true

#if HAVE_GL
static void set_up_for_gl(chan_info *cp)
{
  glXMakeCurrent(MAIN_DISPLAY(ss), XtWindow(channel_graph(cp)), ss->sgx->cx);
}


static void gl_display(chan_info *cp)
{
  if (ss->gl_has_double_buffer)
    glXSwapBuffers(MAIN_DISPLAY(ss), XtWindow(channel_graph(cp)));
  else glFlush();
}


#define GL_COLOR_SET(R, G, B) glColor3us(R, G, B)

static void gl_spectrogram(sono_info *si, int gl_fft_list, mus_float_t cutoff, bool use_dB, mus_float_t min_dB,
			   rgb_t br, rgb_t bg, rgb_t bb)
{
  mus_float_t lin_dB = 0.0;
  mus_float_t xincr, yincr, x0, y0, x1, y1;
  int bins = 0, slice, i, j;
  float inv_scl;
  int **js = NULL;

  inv_scl = 1.0 / si->scale;
  if (use_dB) lin_dB = pow(10.0, min_dB * 0.05);

  glNewList((GLuint)gl_fft_list, GL_COMPILE);

  bins = (int)(si->target_bins * cutoff);
  if (bins <= 0) bins = 1;

  js = (int **)calloc(si->active_slices, sizeof(int *));
  for (i = 0; i < si->active_slices; i++)
    {
      js[i] = (int *)calloc(bins, sizeof(int));
      if (use_dB) 
	{
	  for (j = 0; j < bins; j++)
	    js[i][j] = skew_color(1.0 - (in_dB(min_dB, lin_dB, si->data[i][j] * inv_scl)) / min_dB);
	}
      else
	{
	  for (j = 0; j < bins; j++)
	    js[i][j] = skew_color(si->data[i][j] * inv_scl); /* can be NO_COLOR (-1) */
	}
    }
  xincr = 1.0 / (float)(si->active_slices);
  yincr = 1.0 / (float)bins;
  x1 = -0.5;

  for (slice = 0; slice < si->active_slices - 1; slice++)
    {
      x0 = x1;
      x1 += xincr;
      y1 = -0.5;

      for (i = 0; i < bins - 1; i++)
	{
	  rgb_t r, g, b;
	  mus_float_t val00, val01, val11, val10;

	  glBegin(GL_POLYGON);

	  y0 = y1;
	  y1 += yincr;

	  val00 = si->data[slice][i] * inv_scl;
	  val01 = si->data[slice][i + 1] * inv_scl;
	  val10 = si->data[slice + 1][i] * inv_scl;
	  val11 = si->data[slice + 1][i + 1] * inv_scl;

	  if (use_dB) 
	    {
	      val00 = 1.0 - (in_dB(min_dB, lin_dB, val00)) / min_dB;
	      val01 = 1.0 - (in_dB(min_dB, lin_dB, val01)) / min_dB;
	      val10 = 1.0 - (in_dB(min_dB, lin_dB, val10)) / min_dB;
	      val11 = 1.0 - (in_dB(min_dB, lin_dB, val11)) / min_dB;
	    }
	  if (js[slice][i] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice][i], &r, &g, &b);
	      GL_COLOR_SET(r, g, b);
	    }
	  else GL_COLOR_SET(br, bg, bb);
	  glVertex3f(x0, val00, y0);
	  
	  if (js[slice + 1][i] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice + 1][i], &r, &g, &b);
	      GL_COLOR_SET(r, g, b);
	    }
	  else GL_COLOR_SET(br, bg, bb);
	  glVertex3f(x1, val10, y0);
	  
	  if (js[slice + 1][i + 1] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice + 1][i + 1], &r, &g, &b);
	      GL_COLOR_SET(r, g, b);
	    }
	  else GL_COLOR_SET(br, bg, bb);
	  glVertex3f(x1, val11, y1);
	  
	  if (js[slice][i + 1] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice][i + 1], &r, &g, &b);
	      GL_COLOR_SET(r, g, b);
	    }
	  else GL_COLOR_SET(br, bg, bb);
	  glVertex3f(x0, val01, y1);
	  
	  glEnd();
	}
    }
  for (i = 0; i < si->active_slices; i++) free(js[i]);
  free(js);
  js = NULL;
  glEndList();
}


static bool gl_warned_already = false;

static bool make_gl_spectrogram(chan_info *cp)
{
  sono_info *si;
  axis_info *fap;
  snd_info *sp;
  bool need_relist = false;
  rgb_t br = RGB_MAX, bg = RGB_MAX, bb = RGB_MAX;

  Colormap cmap;
  XColor tmp_color;
  Display *dpy;

  si = cp->sonogram_data;
  sp = cp->sound;

  /* experiments with lighting were a bust -- does not improve things (ditto fog, translucency, grid) */
  
  fap = cp->fft->axis; 
  if ((cp->printing) &&
      (!gl_warned_already))
    {
      gl_warned_already = true;
#if MUS_WITH_GL2PS
      snd_warning("use " S_gl_graph_to_ps " to print openGL graphs");
#else
      snd_warning("we need openGL and gl2ps to print openGL graphs");
#endif
    }
  set_up_for_gl(cp);
  if (cp->gl_fft_list == NO_LIST) 
    {
      need_relist = true;
      cp->gl_fft_list = (int)glGenLists(1);
    }
  else
    {
      if (cp->fft_changed == FFT_CHANGED)
	{
	  glDeleteLists((GLuint)(cp->gl_fft_list), 1);
	  cp->gl_fft_list = (int)glGenLists(1);
	  need_relist = true;
	}
    }

  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);
  glClearDepth(1.0);

  /* get the background color */
  dpy = XtDisplay(MAIN_SHELL(ss));
  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
  tmp_color.flags = DoRed | DoGreen | DoBlue;
  if (cp == selected_channel())
    tmp_color.pixel = ss->sgx->selected_graph_color;
  else tmp_color.pixel = ss->sgx->graph_color;
  XQueryColor(dpy, cmap, &tmp_color);
  br = tmp_color.red;
  bg = tmp_color.green;
  bb = tmp_color.blue;
  glClearColor(RGB_TO_FLOAT(tmp_color.red),
	       RGB_TO_FLOAT(tmp_color.green),
	       RGB_TO_FLOAT(tmp_color.blue),
	       0.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  if (need_relist)
    gl_spectrogram(si, cp->gl_fft_list, cp->spectrum_end, cp->fft_log_magnitude, cp->min_dB, br, bg, bb);

  glViewport(fap->graph_x0, 0, fap->width, fap->height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  /* glOrtho(-1.0, 1.0, -1.0, 1.0, 1.0, -1.0); */ /* this appears to be the default */
  glRotatef(cp->spectro_x_angle, 1.0, 0.0, 0.0);
  glRotatef(cp->spectro_y_angle, 0.0, 1.0, 0.0);
  glRotatef(cp->spectro_z_angle, 0.0, 0.0, 1.0);
  glScalef(cp->spectro_x_scale, cp->spectro_y_scale, cp->spectro_z_scale);

  glCallList((GLuint)(cp->gl_fft_list));

  fap->use_gl = true;

  {
    double frq0, frq1;
    frq0 = SND_SRATE(sp) * cp->spectrum_start / 2.0;
    frq1 = SND_SRATE(sp) * cp->spectrum_end / 2.0;
    make_axis_info(cp,
		   cp->axis->x0, cp->axis->x1,
		   frq0, frq1,
		   _("time"),
		   cp->axis->x0, cp->axis->x1,
		   frq0, frq1,
		   fap);
  }

  make_axes(cp, fap, X_AXIS_IN_SECONDS, DONT_CLEAR_GRAPH, NO_GRID, WITH_LINEAR_AXES, cp->show_axes);
  /* there's no support for GL log y axis labelling in snd-axis.c (line 1244), or in the axis bounds
   *   choice here (see snd-fft.c -- need to check spectrum_start etc), or in gl_spectrogram above, 
   *   so ignore the log_freq setting -- since log freq really doesn't add much (unlike dB for example),
   *   the extra work seems wasted.
   */
  fap->use_gl = false;
  gl_display(cp);
  
  /* a kludge to get the normal graph drawn (again...) */
  if (cp->graph_time_p)
    display_channel_time_data(cp); 
  if (cp->graph_lisp_p)
    display_channel_lisp_data(cp); 
  
  return(XtAppPending(MAIN_APP(ss)) == 0); /* return true if there are no pending events to force current buffer to be displayed */
}
#endif
/* HAVE_GL */


static bool make_spectrogram(chan_info *cp)
{
  sono_info *si;
  axis_info *fap;
  axis_context *ax;
  mus_float_t *fdata;
  mus_float_t matrix[9];
  mus_float_t xyz[3];
  mus_float_t xoff, yoff, x, y, xincr, yincr, x0, y0, binval, scl = 1.0;
  mus_float_t fwidth, fheight, zscl, yval, xval;
  int bins = 0, slice, i, j, xx = 0, yy = 0;
  bool old_with_gl = false;
  snd_info *sp;

  mus_float_t minlx = 0.0, lscale = 0.0, fap_incr = 0.0;

  if (chan_fft_in_progress(cp)) return(false);
  si = cp->sonogram_data;
  if ((!si) || (si->scale <= 0.0)) return(false);
  sp = cp->sound;

#if HAVE_GL
  if (((sp->nchans == 1) || 
       (sp->channel_style == CHANNELS_SEPARATE)) &&
      (color_map(ss) != BLACK_AND_WHITE_COLORMAP) &&
      (with_gl(ss)))
    return(make_gl_spectrogram(cp));
#endif
  
  old_with_gl = with_gl(ss);
  if (old_with_gl) set_with_gl(false, false); /* needed to fixup spectro angles/scales etc */
  if (cp->printing) ps_allocate_grf_points();

  scl = si->scale;                     /* unnormalized fft doesn't make much sense here (just washes out the graph) */
  fap = cp->fft->axis;
  bins = (int)(si->target_bins * cp->spectrum_end);
  fwidth = (fap->x_axis_x1 - fap->x_axis_x0);
  fheight = (fap->y_axis_y1 - fap->y_axis_y0); /* negative! */
  xincr = fwidth / (mus_float_t)bins;
  yincr = fheight / (mus_float_t)si->active_slices;
  x0 = (fap->x_axis_x0 + fap->x_axis_x1) * 0.5;
  y0 = (fap->y_axis_y0 + fap->y_axis_y1) * 0.5;

  if (!(cp->fft_log_magnitude))
    zscl = -(cp->spectro_z_scale * fheight / scl);
  else zscl = -(cp->spectro_z_scale * fheight);
  rotate_matrix(cp->spectro_x_angle, cp->spectro_y_angle, cp->spectro_z_angle,
		cp->spectro_x_scale, cp->spectro_y_scale, zscl,
		matrix);

  if (cp->fft_log_frequency) 
    {
      fap_incr = (fap->x1 - fap->x0) / bins;
      if (fap->x0 > 1.0) minlx = log(fap->x0); else minlx = 0.0;
      lscale = (fap->x_axis_x1 - fap->x_axis_x0) / (log(fap->x1) - minlx);
    }

  ax = copy_context(cp);
  if (color_map(ss) != BLACK_AND_WHITE_COLORMAP)
    allocate_color_map(color_map(ss));
  ss->stopped_explicitly = false;

  for (slice = 0, xoff = fap->x_axis_x0, yoff = fap->y_axis_y0; 
       slice < si->active_slices; 
       slice++, yoff += yincr)
    {
      fdata = si->data[slice];
      x = xoff;
      y = yoff;
      
      if (color_map(ss) != BLACK_AND_WHITE_COLORMAP)
	{
	  xyz[0] = x - x0; 
	  xyz[1] = y - y0; 
	  xyz[2] = fdata[0]; 
	  rotate(xyz, matrix);
	  xx = (int)(xyz[0] + x0); 
	  yy = (int)(xyz[1] + xyz[2] + y0);
	}
      
      for (i = 0; i < bins; i++, x += xincr)
	{
	  mus_float_t logx;
	  if (cp->fft_log_frequency) 
	    {
	      mus_float_t fx, curlx;
	      fx = fap->x0 +  fap_incr * i;
	      if (fx > 1.0) curlx = log(fx); else curlx = 0.0;
	      logx = fap->x_axis_x0 + (curlx - minlx) * lscale;
	    }
	  else logx = x;

	  xyz[0] = logx - x0; 
	  xyz[1] = y - y0;
	  binval = fdata[i] / scl;
	  if (!(cp->fft_log_magnitude)) 		  
	    xyz[2] = fdata[i];
	  else 
	    {
	      xyz[2] = 1.0 - (in_dB(cp->min_dB, cp->lin_dB, binval)) / cp->min_dB; 
	      binval = xyz[2];
	    }
	  rotate(xyz, matrix);
	  yval = xyz[1] + xyz[2];
	  xval = xyz[0];
	  
	  if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	    {
	      set_grf_point((int)(xval + x0), i, (int)(yval + y0));
	      if (cp->printing) 
		ps_set_grf_point(ungrf_x(fap, (int)(xval + x0)), i, 
				 ungrf_y(fap, (int)(yval + y0)));
	    }
	  else
	    {
	      j = skew_color(binval);
	      if (j != NO_COLOR)
		{
		  draw_spectro_line(ax, j, xx, yy, 
				    (int)(xval + x0), 
				    (int)(yval + y0));
		  if (cp->printing) 
		    ps_draw_spectro_line(fap, j, xx, yy, xval + x0, yval + y0);
		}
	      xx = (int)(xval + x0); 
	      yy = (int)(yval + y0);
	    }
	}
      
      if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	draw_grf_points(cp->dot_size, ax, bins, fap, 0.0, cp->transform_graph_style);
      
      if (cp->printing) 
	{
	  if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	    ps_draw_grf_points(fap, bins, 0.0, cp->transform_graph_style, cp->dot_size);
	  check_for_event();
	  if ((ss->stopped_explicitly) || (cp->active < CHANNEL_HAS_EDIT_LIST))
	    {
	      ss->stopped_explicitly = false;
	      string_to_minibuffer(sp, _("stopped"));
	      break;
	    }
	}
    }
  if ((cp->printing) && (color_map(ss) != BLACK_AND_WHITE_COLORMAP)) ps_reset_color();
  if (cp->hookable == WITH_HOOK) after_transform(cp, 1.0 / scl);
  if (old_with_gl) set_with_gl(true, false);
  return(false);
}


/* ---------------------------------------- wavograms ---------------------------------------- */

/* (with-sound () (do ((j 0 (+ j 1))) ((= j 500)) (do ((i 0 (+ i 1))) ((= i 100)) (outa (+ (* j 100) i) (* i j .002 .01)))))
 *
 * xdeg=45, ydeg=0, zdeg=0 is the "flat" display
 */

typedef struct wavogram_state {
  int hop, trace, width, height, graph_x0, cmap, cmap_size, edit_ctr;
  bool inverted;
  mus_float_t scale, cutoff;
  mus_long_t losamp;
  mus_float_t x_scale, y_scale, z_scale, z_angle, x_angle, y_angle; /* only used in non-GL case */
} wavogram_state;

   
static int make_wavogram(chan_info *cp)
{
  snd_info *sp;
  mus_float_t xoff, x, y, x0, y0, xincr;
  mus_float_t width, height, zscl, yval, xval, binval;
  int i, j, points = 0, yincr, yoff, xx, yy;
  mus_float_t matrix[9];
  mus_float_t xyz[3];
  axis_info *ap;
  axis_context *ax;
  snd_fd *sf = NULL;
  wavogram_state *lw;
  bool need_new_list = true, need_redraw = true;
#if HAVE_GL || USE_MOTIF
  bool use_gl = false;
#endif

  sp = cp->sound;
  ap = cp->axis;
  if (sp) ap->losamp = (mus_long_t)(ap->x0 * SND_SRATE(sp));

#if HAVE_GL
  if (((sp->nchans == 1) || (sp->channel_style == CHANNELS_SEPARATE)) &&
      (color_map(ss) != BLACK_AND_WHITE_COLORMAP) &&
      (with_gl(ss)))
    use_gl = true;
#endif

  lw = cp->last_wavogram;
  if (lw)
    {
      need_new_list = (!((lw->hop == wavo_hop(ss)) &&
			 (lw->trace == wavo_trace(ss)) &&
			 (lw->losamp == ap->losamp) &&
			 (lw->width == ap->width) &&
			 (lw->height == ap->height) &&
			 (lw->graph_x0 == ap->graph_x0) && 
			 (lw->cmap == color_map(ss)) &&
			 (lw->cmap_size == color_map_size(ss)) &&
			 (lw->edit_ctr == cp->edit_ctr) &&
			 (lw->inverted == color_inverted(ss)) &&
			 (lw->scale == color_scale(ss)) &&
			 (lw->cutoff == color_cutoff(ss))));
#if HAVE_GL
      if (use_gl)
	{
	  if (cp->gl_wavo_list == NO_LIST)
	    need_new_list = true;
	}
#endif
      need_redraw = (!((lw->x_scale == spectro_x_scale(ss)) &&
		       (lw->y_scale == spectro_y_scale(ss)) &&
		       (lw->z_scale == spectro_z_scale(ss)) &&
		       (lw->x_angle == spectro_x_angle(ss)) &&
		       (lw->y_angle == spectro_y_angle(ss)) &&
		       (lw->z_angle == spectro_z_angle(ss))));
    }

#if USE_MOTIF
  if ((!use_gl) && (!need_redraw) && (!need_new_list))
    return(-1); /* not 0 here because I think that would cancel fft graph */
#endif

  if (cp->last_wavogram) free(cp->last_wavogram);

  lw = (wavogram_state *)malloc(sizeof(wavogram_state));
  lw->hop = wavo_hop(ss);
  lw->trace = wavo_trace(ss);
  lw->losamp = ap->losamp;
  lw->width = ap->width;
  lw->height = ap->height;
  lw->graph_x0 = ap->graph_x0;
  lw->cmap = color_map(ss);
  lw->cmap_size = color_map_size(ss);
  lw->edit_ctr = cp->edit_ctr;
  lw->inverted = color_inverted(ss);
  lw->scale = color_scale(ss);
  lw->cutoff = color_cutoff(ss);

  lw->x_scale = spectro_x_scale(ss);
  lw->y_scale = spectro_y_scale(ss);
  lw->z_scale = spectro_z_scale(ss);
  lw->x_angle = spectro_x_angle(ss);
  lw->y_angle = spectro_y_angle(ss);
  lw->z_angle = spectro_z_angle(ss);

  cp->last_wavogram = lw;

#if HAVE_GL
  if (use_gl)
    {
      mus_float_t **samps = NULL;
      int **js = NULL;
      float x0, x1, y0, y1, x5inc, y5inc;
      mus_float_t xinc, yinc;
      rgb_t *rs = NULL, *gs = NULL, *bs = NULL;
      /* each line is wavo_trace samps, there are (height / wave_hop) of these? */
      int lines = 0, len = 0;
      
      if (need_new_list)
	{
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (sf == NULL) return(0);

	  lines = (int)(ap->height / cp->wavo_hop);
	  if (lines == 0) return(0);
	  len = cp->wavo_trace;
	  samps = (mus_float_t **)calloc(lines, sizeof(mus_float_t *));
	  js = (int **)calloc(lines, sizeof(int *));
	  for (i = 0; i < lines; i++)
	    {
	      samps[i] = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	      js[i] = (int *)calloc(len, sizeof(int));
	      for (j = 0; j < len; j++)
		{
		  samps[i][j] = read_sample(sf);
		  js[i][j] = skew_color(fabs(samps[i][j]));
		  if (js[i][j] < 0) js[i][j] = 0;
		}
	    }
	  free_snd_fd(sf);
	}

      set_up_for_gl(cp);

      glEnable(GL_DEPTH_TEST);
      glDepthFunc(GL_LEQUAL); 
      glClearDepth(1.0);
      glClearColor(1.0, 1.0, 1.0, 0.0);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      glViewport(ap->graph_x0, 0, ap->width, ap->height);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glRotatef(cp->spectro_x_angle, 1.0, 0.0, 0.0);
      glRotatef(cp->spectro_y_angle, 0.0, 1.0, 0.0);
      glRotatef(cp->spectro_z_angle, 0.0, 0.0, 1.0);
      glScalef(cp->spectro_x_scale, cp->spectro_y_scale, cp->spectro_z_scale);

      if (!need_new_list)
	{
	  glCallList((GLuint)(cp->gl_wavo_list));
	  gl_display(cp);
	  return((int)(lw->height / lw->hop));
	}

      if (cp->gl_wavo_list == NO_LIST) 
	cp->gl_wavo_list = (int)glGenLists(1);
      else
	{
	  glDeleteLists((GLuint)(cp->gl_wavo_list), 1);
	  cp->gl_wavo_list = (int)glGenLists(1);
	}

      xinc = 2.0 / (mus_float_t)len;
      yinc = 2.0 / (mus_float_t)lines;
      x5inc = 1.25 * xinc;
      y5inc = 1.25 * yinc;
      
      rs = color_map_reds(color_map(ss));
      
      glNewList((GLuint)(cp->gl_wavo_list), GL_COMPILE);

      if (rs)
	{
	  int len1, lines1;
	  mus_float_t xf, yf;

	  gs = color_map_greens(color_map(ss));
	  bs = color_map_blues(color_map(ss));
	  y1 = -1.0;
	  len1 = len - 1;
	  lines1 = lines - 1;

	  for (j = 0; j < lines1; j++)
	    {
	      x1 = -1.0;
	      y0 = y1;
	      y1 += yinc;
	      yf = y1 + yinc / 2.0;

	      for (i = 0; i < len1; i++)
		{
		  int c;
		  x0 = x1;
		  x1 += xinc;
		  xf = x1 + xinc / 2.0;
		  
		  glBegin(GL_QUADS);
		  c = js[j][i];
		  GL_COLOR_SET(rs[c], gs[c], bs[c]);
		  glVertex3f(x0, samps[j][i], y0);
		
		  c = js[j + 1][i];
		  GL_COLOR_SET(rs[c], gs[c], bs[c]);
		  glVertex3f(xf, samps[j+ 1][i], y0);
		
		  c = js[j + 1][i + 1];
		  GL_COLOR_SET(rs[c], gs[c], bs[c]);
		  glVertex3f(xf, samps[j + 1][i + 1], yf);
		
		  c = js[j][i + 1];
		  GL_COLOR_SET(rs[c], gs[c], bs[c]);
		  glVertex3f(x0, samps[j][i + 1], y1 + yf);
		  glEnd();
		}
	    }
	}
      else
	{
	  int len1, lines1;
	  y1 = -1.0;
	  len1 = len - 1;
	  lines1 = lines - 1;
	  for (j = 0; j < lines1; j++)
	    {
	      x1 = -1.0;
	      y0 = y1;
	      y1 += yinc;
	      for (i = 0; i < len1; i++)
		{
		  rgb_t r, g, b;
		  x0 = x1;
		  x1 += xinc;

		  glBegin(GL_QUADS);
		  get_current_color(color_map(ss), js[j][i], &r, &g, &b);
		  GL_COLOR_SET(r, g, b);
		  glVertex3f(x0, samps[j][i], y0);
		
		  get_current_color(color_map(ss), js[j + 1][i], &r, &g, &b);
		  GL_COLOR_SET(r, g, b);
		  glVertex3f(x1, samps[j+ 1][i], y0);
		
		  get_current_color(color_map(ss), js[j + 1][i + 1], &r, &g, &b);
		  GL_COLOR_SET(r, g, b);
		  glVertex3f(x1, samps[j + 1][i + 1], y1);
		
		  get_current_color(color_map(ss), js[j][i + 1], &r, &g, &b);
		  GL_COLOR_SET(r, g, b);
		  glVertex3f(x0, samps[j][i + 1], y1);
		  glEnd();
		}
	    }
	}

      glEndList();
      glCallList((GLuint)(cp->gl_wavo_list));
      gl_display(cp);

      for (i = 0; i < lines; i++) 
	{
	  free(samps[i]);
	  free(js[i]);
	}
      free(samps);
      free(js);
      return(j);
    }
#endif

  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
  if (sf == NULL) return(0);

  /* set up the graph width and so on */
  make_axes_1(ap, cp->x_axis_style, SND_SRATE(sp), SHOW_NO_AXES, NOT_PRINTING, NO_X_AXIS, NO_GRID, WITH_LINEAR_AXES, 1.0);
  erase_rectangle(cp, ap->ax, ap->graph_x0, ap->y_offset, ap->width, ap->height); 

  if (cp->printing) ps_allocate_grf_points();

  width = (ap->x_axis_x1 - ap->x_axis_x0);
  height = (ap->y_axis_y1 - ap->y_axis_y0); /* negative! */

  xincr = width / (mus_float_t)(cp->wavo_trace);
  yincr = -(cp->wavo_hop);
  if (yincr > 0) yincr = -yincr;
  if (yincr == 0) yincr = -1;

  x0 = (ap->x_axis_x0 + ap->x_axis_x1) * 0.5;
  y0 = (ap->y_axis_y0 + ap->y_axis_y1) * 0.5;

  zscl = -(cp->spectro_z_scale * height);
  rotate_matrix(cp->spectro_x_angle, cp->spectro_y_angle, cp->spectro_z_angle,
		cp->spectro_x_scale, cp->spectro_y_scale, zscl,
		matrix);

  ax = copy_context(cp);
  if (color_map(ss) != BLACK_AND_WHITE_COLORMAP)
    allocate_color_map(color_map(ss));

  for (xoff = ap->x_axis_x0, yoff = ap->y_axis_y0; 
       yoff > ap->y_axis_y1; 
       yoff += yincr)
    {
      xx = -1;
      yy = (int)y0; /* ? */
      x = xoff;
      y = yoff;
      for (i = 0; i < cp->wavo_trace; i++, x += xincr)
	{
	  binval = read_sample(sf);
	  xyz[0] = x - x0; 
	  xyz[1] = y - y0; 
	  xyz[2] = binval;
	  rotate(xyz, matrix);
	  yval = xyz[1] + xyz[2];
	  xval = xyz[0];
	  if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	    {
	      set_grf_point((int)(xval + x0), i, 
			    (int)(yval + y0));
	      if (cp->printing) 
		ps_set_grf_point(ungrf_x(ap, (int)(xval + x0)), i, 
				 ungrf_y(ap, (int)(y0 + yval)));
	    }
	  else
	    {
	      /* for color decision here we need absolute value of data */
	      if (binval < 0.0) binval = -binval;
	      if (xx != -1)
		{
		  j = skew_color(binval);
		  if (j != NO_COLOR)
		    {
		      draw_spectro_line(ax, j, xx, yy, 
					(int)(xval + x0), 
					(int)(yval + y0));
		      if (cp->printing) 
			ps_draw_spectro_line(ap, j, xx, yy, xval + x0, yval + y0);
		    }
		}
	      xx = (int)(xval + x0); 
	      yy = (int)(yval + y0);
	    }
	}
      points += cp->wavo_trace;
      
      if (color_map(ss) == BLACK_AND_WHITE_COLORMAP)
	{
	  draw_grf_points(cp->dot_size, ax, cp->wavo_trace, ap, 0.0, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_grf_points(ap, cp->wavo_trace, 0.0, cp->time_graph_style, cp->dot_size);
	}
    }
      
  if ((cp->printing) && (color_map(ss) != BLACK_AND_WHITE_COLORMAP))
    ps_reset_color();
  free_snd_fd(sf);
  return(points);
}


axis_info *lisp_info_axis(chan_info *cp) 
{
  return(cp->lisp_info->axis);
}


void free_lisp_info(chan_info *cp)
{
  if (cp)
    {
      lisp_grf *lg;
      lg = cp->lisp_info;
      if (lg)
	{
	  if (lg->axis) 
	    lg->axis = free_axis_info(lg->axis);
	  if (lg->data) 
	    {
	      int i;
	      for (i = 0; i < lg->graphs; i++) 
		if (lg->data[i]) 
		  free(lg->data[i]);
	      free(lg->data);
	    }
	  if (lg->len) 
	    free(lg->len);
	  free(lg);
	}
    }
}


static void make_lisp_graph(chan_info *cp, XEN pixel_list)
{
  snd_info *sp;
  lisp_grf *up;
  /* data can be evenly spaced data or an envelope (up->env_data) */
  axis_info *uap = NULL;
  int i;
  axis_context *ax;

  sp = cp->sound;
  up = cp->lisp_info;
  if (up) uap = up->axis; else return;
  if ((!uap) || (!uap->graph_active) || (up->len == NULL) || (up->len[0] <= 0)) return;

  if (cp->printing) ps_allocate_grf_points();
  if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
    ax = combined_context(cp); 
  else ax = copy_context(cp);
  if (cp->printing) ps_fg(cp, ax);

  if (up->env_data)
    {
      int x0, x1, y0, y1;
      int grf_len;
      grf_len = up->len[0];
      x0 = local_grf_x(up->data[0][0], uap);
      y0 = local_grf_y(up->data[0][1], uap);
      if (cp->dot_size > 0) 
	draw_dot(ax, x0, y0, cp->dot_size);

      for (i = 2; i < grf_len; i += 2)
	{
	  x1 = local_grf_x(up->data[0][i], uap);
	  y1 = local_grf_y(up->data[0][i + 1], uap);
	  draw_line(ax, x0, y0, x1, y1);
	  if (cp->dot_size > 0) 
	    draw_dot(ax, x1, y1, cp->dot_size);
	  x0 = x1;
	  y0 = y1;
	}
    }
  else
    {
      int j, graph, pixel_len = -1;
      color_t old_color = 0;
      mus_float_t x, y, samples_per_pixel = 1.0;
      if (XEN_LIST_P(pixel_list))
	pixel_len = XEN_LIST_LENGTH(pixel_list);
      if (up->graphs > 1) 
	old_color = get_foreground_color(ax);

      for (graph = 0; graph < up->graphs; graph++)
	{
	  if ((pixel_len > graph) &&
	      (XEN_PIXEL_P(XEN_LIST_REF(pixel_list, graph))))
	    set_foreground_color(ax, (color_t)XEN_UNWRAP_PIXEL(XEN_LIST_REF(pixel_list, graph)));
	  else
	    {
	      switch (graph)
		{
		case 0:  break;
		case 1:  set_foreground_color(ax, ss->sgx->red);        break;
		case 2:  set_foreground_color(ax, ss->sgx->green);      break;
		case 3:  set_foreground_color(ax, ss->sgx->light_blue); break;
		case 4:  set_foreground_color(ax, ss->sgx->yellow);     break;
		default: set_foreground_color(ax, ss->sgx->black);      break;
		}
	    }
	  /* check for up->len[graph] > pixels available and use ymin ymax if needed */
	  samples_per_pixel = (mus_float_t)(up->len[graph]) / (mus_float_t)(uap->x_axis_x1 - uap->x_axis_x0);
	  if (samples_per_pixel < 4.0)
	    {
	      mus_float_t start_x, xinc;
	      int grf_len;
	      grf_len = up->len[graph];
	      start_x = uap->x0;
	      if (grf_len <= 1) 
		xinc = 1.0;
	      else xinc = (uap->x1 - uap->x0) / (mus_float_t)(grf_len - 1);
	      for (i = 0, x = start_x; i < grf_len; i++, x += xinc)
		{
		  y = up->data[graph][i];
		  set_grf_point(local_grf_x(x, uap), i, 
				local_grf_y(y, uap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i, y);
		}
	      draw_grf_points(cp->dot_size, ax, grf_len, uap, 0.0, cp->lisp_graph_style);
	      if (cp->printing) 
		ps_draw_grf_points(uap, grf_len, 0.0, cp->lisp_graph_style, cp->dot_size);
	    }
	  else
	    {
	      int xi;
	      mus_float_t ymin, ymax, pinc, xf;
	      j = 0;
	      i = 0;
	      xf = 0.0;
	      x = 0.0;
	      pinc = samples_per_pixel / (mus_float_t)(up->len[graph]);
	      xi = local_grf_x(0.0, uap);
	      ymin = 32768.0;
	      ymax = -32768.0;
	      while (i < up->len[graph])
		{
		  y = up->data[graph][i];
		  if (y > ymax) ymax = y;
		  if (y < ymin) ymin = y;
		  xf += 1.0;
		  i++;
		  if (xf > samples_per_pixel)
		    {
		      set_grf_points(xi, j, 
				     local_grf_y(ymin, uap), 
				     local_grf_y(ymax, uap));
		      if (cp->printing) 
			{
			  x += pinc; 
			  ps_set_grf_points(x, j, ymin, ymax);
			}
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      ymin = 32768.0;
		      ymax = -32768.0;
		    }
		}
	      draw_both_grf_points(cp->dot_size, ax, j, cp->lisp_graph_style);
	      if (cp->printing) 
		ps_draw_both_grf_points(uap, j, cp->lisp_graph_style, cp->dot_size);
	    }
	}
      if (up->graphs > 1) set_foreground_color(ax, old_color);
      if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
	{
	  copy_context(cp); /* reset for axes etc */
	  if (cp->printing) ps_reset_color();
	}
    }
}


static void make_axes(chan_info *cp, axis_info *ap, x_axis_style_t x_style, bool erase_first, with_grid_t grid, log_axis_t log_axes, show_axes_t axes)
{
  snd_info *sp;
  color_t old_color = 0; /* make the compiler happy */

  if (!(ap->ax))
    ap->ax = cp->cgx->ax;
  sp = cp->sound;
  setup_axis_context(cp, ap->ax);

  /* here is where the graph is cleared (see also make_partial_graph and make_wavogram) */
  if (erase_first == CLEAR_GRAPH)
    erase_rectangle(cp, ap->ax, ap->graph_x0, ap->y_offset, ap->width, ap->height); 

  if (ss->sgx->axis_color_set)
    {
      old_color = get_foreground_color(ap->ax);
      /* if axis color has been set, use it, else use data color from preceding make_graph call */
      set_foreground_color(ap->ax, ss->sgx->axis_color);
    }

  make_axes_1(ap, 
	      x_style, 
	      SND_SRATE(sp), 
	      axes, 
	      cp->printing,
	      (((sp->channel_style != CHANNELS_COMBINED) || 
		(cp->show_axes == SHOW_ALL_AXES) || 
		(cp->show_axes == SHOW_ALL_AXES_UNLABELLED) || 
		(cp->show_axes == SHOW_BARE_X_AXIS) || 
		(cp->chan == (sp->nchans - 1))) ? WITH_X_AXIS : NO_X_AXIS),
	      grid, 
	      log_axes,
	      cp->grid_density);

  if (ss->sgx->axis_color_set)
    set_foreground_color(ap->ax, old_color);
}


static void draw_sonogram_cursor(chan_info *cp);
static void draw_graph_cursor(chan_info *cp);


static void display_channel_data_with_size(chan_info *cp, 
					   int width, int height, int offset, 
					   bool just_fft, bool just_lisp, bool just_time,
					   bool save_cr)
{
  /* this procedure is unnecessarily confusing! */
  snd_info *sp;
  bool with_fft = false, with_lisp = false, with_time = false;
  int displays = 0;
  bool grflsp = false;
  axis_info *ap = NULL;
  axis_info *fap = NULL;
  axis_info *uap = NULL;
  lisp_grf *up = NULL;

  sp = cp->sound;
  
  /* -------- graph-hook -------- */
  if ((cp->hookable == WITH_HOOK) && 
      (!(ss->graph_hook_active)) &&
      (XEN_HOOKED(graph_hook)))
    {
      XEN res = XEN_FALSE;
      ss->graph_hook_active = true;
      res = run_progn_hook(graph_hook,
			   XEN_LIST_4(C_INT_TO_XEN_SOUND(sp->index),
				      C_TO_XEN_INT(cp->chan),
				      C_TO_XEN_DOUBLE(cp->axis->y0),
				      C_TO_XEN_DOUBLE(cp->axis->y1)),
			   S_graph_hook);
      /* (add-hook! graph-hook (lambda (a b c d) (snd-print (format #f "~A ~A ~A ~A" a b c d)))) */
      ss->graph_hook_active = false;
      if (XEN_TRUE_P(res)) return;
    }

  ap = cp->axis;
  if (ap == NULL) return;

  /* -------- decide which graphs to draw -------- */
  ap->height = height;
  ap->window_width = width;
  ap->width = width;
  ap->y_offset = offset;
  if (cp->graph_time_p) 
    {
      displays++;
      with_time = true;
    }
  grflsp = ((cp->graph_lisp_p) || (XEN_HOOKED(lisp_graph_hook)));
  if (grflsp)
    {
      displays++;
      up = cp->lisp_info;
      if (up == NULL)
	{
	  /* this should only happen the first time such a graph is needed */
	  cp->lisp_info = (lisp_grf *)calloc(1, sizeof(lisp_grf));
	  up = cp->lisp_info;
	  up->show_axes = cp->show_axes;
	  up->axis = make_axis_info(cp, 0.0, 1.0, -1.0, 1.0, "dummy axis", 0.0, 1.0, -1.0, 1.0, NULL);
	}
      if (up)
	{
	  uap = up->axis;
	  if (uap)
	    {
	      with_lisp = true;
	      uap->height = height;
	      uap->y_offset = offset;
	      uap->width = width;
	      uap->window_width = width;
	    }
	}
    }

  if (cp->graph_transform_p) 
    {
      fft_info *fp;
      displays++;
      fp = cp->fft;
      if (fp)
	{
	  fap = fp->axis; 
	  if (fap)
	    {
	      with_fft = true;
	      fap->height = height;
	      fap->y_offset = offset;
	      fap->width = width;
	      fap->window_width = width;
	    }
	}
    }

#if USE_CAIRO
  if ((!save_cr) && (cp->cgx) && (cp->cgx->ax))
    {
      if (cp->cgx->ax->cr)
	cairo_destroy(cp->cgx->ax->cr);
      cp->cgx->ax->cr = gdk_cairo_create(cp->cgx->ax->wn);
    }
#endif

  if (displays == 0)
    {
      clear_window(ap->ax);
      return;
    }

  /* -------- set graph positions and sizes -------- */
  if (cp->graphs_horizontal)
    {
      if (with_time) ap->width = width / displays;
      if (with_fft) fap->width =  width / displays;
      if (with_lisp) uap->width = width / displays;

      /* now the x axis offsets for fap and uap */
      if (with_fft) 
	{
	  if (with_time) 
	    fap->graph_x0 = ap->width;
	  else fap->graph_x0 = 0;
	}
      if (with_lisp) 
	uap->graph_x0 = uap->width * (displays - 1);
    }
  else
    {
      height /= displays;
      if (with_time) ap->height = height;
      if (with_fft) fap->height =  height;
      if (with_lisp) uap->height = height;

      /* now the y axis offsets for fap and uap */
      if (with_fft)
	{
	  if (with_time) fap->y_offset += height; 
	  fap->graph_x0 = 0;
	}
      if (with_lisp) 
	{
	  uap->y_offset += (height * (displays - 1));
	  uap->graph_x0 = 0;
	}
    }

  /* -------- time domain graph -------- */
  if ((!just_fft) && (!just_lisp))
    {
      marks_off(cp);
      channel_set_mix_tags_erased(cp);
      if (with_time)
	{
	  int points;
	  if (cp->time_graph_type == GRAPH_AS_WAVOGRAM)
	    {
	      if (ap->y_axis_y0 == ap->y_axis_y1) 
		make_axes(cp, ap, cp->x_axis_style, DONT_CLEAR_GRAPH, cp->show_grid, WITH_LINEAR_AXES, cp->show_axes); /* first time needs setup */
	      ap->y0 = ap->x0;
	      ap->y1 = ap->y0 + (mus_float_t)(cp->wavo_trace * (ap->y_axis_y0 - ap->y_axis_y1)) / ((mus_float_t)(cp->wavo_hop) * SND_SRATE(sp));
	      ap->x1 = ap->x0 + (double)(cp->wavo_trace) / (double)SND_SRATE(sp);
	      points = make_wavogram(cp);
	    }
	  else
	    {
	      make_axes(cp, ap,
			cp->x_axis_style,
			(((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH),
			cp->show_grid,
			WITH_LINEAR_AXES,
			cp->show_axes);
	      cp->cursor_visible = false;
	      cp->selection_visible = false;

#if USE_CAIRO
	      if ((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) 
		display_selection(cp);
#endif

	      points = make_graph(cp);
	    }
	  if (points == 0) return;
	  if (cp->cursor_on) draw_graph_cursor(cp);
	}
    }

  /* -------- fft graph -------- */
  if ((with_fft) && 
      (!just_lisp) && (!just_time))
    {
      if ((!(with_gl(ss))) || 
	  (cp->transform_graph_type != GRAPH_AS_SPECTROGRAM) ||
	  (color_map(ss) == BLACK_AND_WHITE_COLORMAP) ||
	  ((sp->nchans > 1) && 
	   (sp->channel_style != CHANNELS_SEPARATE)))
	make_axes(cp, fap, X_AXIS_IN_SECONDS, /* x-axis-style only affects the time domain graph */
		  (((cp->chan == 0) ||
		    (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH),
		  ((cp->show_grid) && 
		   (cp->transform_graph_type != GRAPH_AS_SPECTROGRAM)) ? WITH_GRID : NO_GRID,
		  (!(cp->fft_log_frequency)) ? WITH_LINEAR_AXES :
		   ((cp->transform_graph_type == GRAPH_AS_SONOGRAM) ? WITH_LOG_Y_AXIS : WITH_LOG_X_AXIS),
		  cp->show_axes);
	  
      if ((!with_time) || (just_fft))
	{ /* make_graph does this -- sets losamp needed by fft to find its starting point */
	  ap->losamp = (mus_long_t)(ap->x0 * (double)SND_SRATE(sp));
	  ap->hisamp = (mus_long_t)(ap->x1 * (double)SND_SRATE(sp));
	}

      switch (cp->transform_graph_type)
	{
	case GRAPH_ONCE:
	  make_fft_graph(cp,
			 cp->fft->axis,
			 (sp->channel_style == CHANNELS_SUPERIMPOSED) ? combined_context(cp) : copy_context(cp),
			 cp->hookable);
	  break;

	case GRAPH_AS_SONOGRAM:
	  make_sonogram(cp);
	  break;

	case GRAPH_AS_WAVOGRAM:
	  break;

	case GRAPH_AS_SPECTROGRAM:
#if HAVE_GL
	  if (make_spectrogram(cp))
	    glDrawBuffer(GL_BACK);
#else
	  make_spectrogram(cp);
#endif
	  break;
	}
      if (cp->cursor_on) 
	{
	  cp->fft_cursor_visible = false; 
	  draw_sonogram_cursor(cp);
	}
    }

  /* -------- "lisp" (extlang) graph -------- */
  if ((with_lisp) && 
      (!just_fft) && (!just_time))
    {
      int pixel_loc = -1;
      XEN pixel_list = XEN_FALSE;
      if ((just_lisp) || ((!with_time) && (!(with_fft))))
	{
	  ap->losamp = (mus_long_t)(ap->x0 * (double)SND_SRATE(sp));
	  ap->hisamp = (mus_long_t)(ap->x1 * (double)SND_SRATE(sp));
	}
      if ((cp->hookable == WITH_HOOK) &&
	  (!(ss->lisp_graph_hook_active)) &&
	  (XEN_HOOKED(lisp_graph_hook)))
	{
	  ss->lisp_graph_hook_active = true;
	  /* inadvertent recursive call here can hang entire computer */
	  pixel_list = run_progn_hook(lisp_graph_hook,
				      XEN_LIST_2(C_INT_TO_XEN_SOUND(cp->sound->index),
						 C_TO_XEN_INT(cp->chan)),
				      S_lisp_graph_hook);
	  ss->lisp_graph_hook_active = false;
	  if (!(XEN_FALSE_P(pixel_list))) pixel_loc = snd_protect(pixel_list);
	}

      if (up != cp->lisp_info)
	up = cp->lisp_info;
      if (uap != up->axis)
	uap = up->axis;
      /* if these were changed in the hook function, the old fields should have been saved across the change (g_graph below) */
      make_axes(cp, uap, /* defined in this file l 2293 */
		X_AXIS_IN_SECONDS,
		(((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH),
		cp->show_grid,
		WITH_LINEAR_AXES,
		up->show_axes);

      if (XEN_PROCEDURE_P(pixel_list))
	XEN_CALL_0(pixel_list, S_lisp_graph);
      else make_lisp_graph(cp, pixel_list);
      if (!(XEN_FALSE_P(pixel_list))) snd_unprotect_at(pixel_loc);

      if ((cp->hookable == WITH_HOOK) &&
	  (XEN_HOOKED(after_lisp_graph_hook)))
	run_hook(after_lisp_graph_hook,
		 XEN_LIST_2(C_INT_TO_XEN_SOUND(cp->sound->index),
			    C_TO_XEN_INT(cp->chan)),
		 S_after_lisp_graph_hook);
    }
  
  /* -------- time domain extras -------- */
  if ((!just_lisp) && (!just_fft))
    {
      if (with_time)
	{
	  if ((cp->chan == 0) || 
	      (sp->channel_style != CHANNELS_SUPERIMPOSED)) 
	    {
#if (!USE_CAIRO)
	      display_selection(cp);
#endif
	      display_channel_marks(cp);
	    }
	  if (cp->show_y_zero) display_y_zero(cp);
	  if ((channel_has_active_mixes(cp))) display_channel_mixes(cp);
	}

      if ((sp->channel_style != CHANNELS_SUPERIMPOSED) && (height > 10))
	display_channel_id(cp, height + offset, sp->nchans);

      run_after_graph_hook(cp);
    } 
}


static void display_channel_data_1(chan_info *cp, bool just_fft, bool just_lisp, bool just_time, bool save_cr)
{
  snd_info *sp;
  int width, height;

  sp = cp->sound;
  if ((sp->inuse == SOUND_IDLE) ||
      (cp->active < CHANNEL_HAS_AXES) ||
      (!(sp->active)) ||
      (!(channel_graph_is_visible(cp))))
    return;

  if ((sp->channel_style == CHANNELS_SEPARATE) || 
      (sp->nchans == 1))
    {
      width = widget_width(channel_graph(cp));
      height = widget_height(channel_graph(cp));
      if ((height > 5) && (width > 5))
	display_channel_data_with_size(cp, width, height, 0, just_fft, just_lisp, just_time, save_cr);
    }
  else
    {
      /* all chans in one graph widget, sy->scroll entire set, zy->zoom entire set etc */
      /* updates are asynchronous (dependent on background ffts etc), so we can't do the whole window at once */
      /* complication here is that we're growing down from the top, causing endless confusion */

#if USE_GTK
      width = widget_width(channel_graph(sp->chans[0])); /* don't fixup for gsy width -- those sliders are in different table slots */
#else
      width = widget_width(channel_graph(sp->chans[0])) - (2 * ss->position_slider_width);
#endif
      if (width <= 0) return;
      height = widget_height(channel_graph(sp->chans[0]));
      cp->height = height;
      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
	display_channel_data_with_size(cp, width, height, 0, just_fft, just_lisp, just_time, save_cr);
      else
	{
	  int offset, full_height, y0, y1, bottom, top;
	  mus_float_t val, size, chan_height;
	  axis_info *ap;

	  /* CHANNELS_COMBINED case */
	  val = gsy_value(sp->chans[0]);
	  size = gsy_size(sp->chans[0]);
	  full_height = (int)((mus_float_t)height / size);
	  chan_height = (mus_float_t)full_height / (mus_float_t)(sp->nchans);
	  bottom = (int)(full_height * val);
	  top = (int)(full_height * (val + size));
	  y1 = (int)((sp->nchans - cp->chan) * chan_height);
	  y0 = y1 - (int)chan_height;
	  offset = top - y1;

	  if ((cp->chan == 0) && 
	      (offset > 0))
	    {
	      /* round off trouble can lead to garbage collecting at the top of the window (similarly at the bottom I suppose) */
	      chan_height += offset;
	      offset = 0;
	    }
#if USE_GTK
	  /* this only needed because Gtk signal blocking is screwed up */
	  if ((cp->chan > 0) && 
	      (channel_has_active_mixes(cp)) && 
	      (cp->axis->height != height)) 
	    channel_set_mix_tags_erased(cp);
#endif

	  if ((cp->chan == (sp->nchans - 1)) && 
	      ((offset + chan_height) < height))
	    chan_height = height - offset;
	  if (((y0 < top) && (y0 >= bottom)) || 
	      ((y1 > bottom) && (y1 <= top)))
	    display_channel_data_with_size(cp, width, (int)chan_height, offset, just_fft, just_lisp, just_time, save_cr);
	  else 
	    {
	      ap = cp->axis;
	      ap->y_offset = offset; /* needed for mouse click channel determination */
	    }
	}
    }
}


void display_channel_fft_data(chan_info *cp)
{
  display_channel_data_1(cp, true, false, false, false);
}


static void display_channel_lisp_data(chan_info *cp)
{
  display_channel_data_1(cp, false, true, false, false);
}


void display_channel_time_data(chan_info *cp)
{
  display_channel_data_1(cp, false, false, true, false);
}


void display_channel_data(chan_info *cp)
{
  display_channel_data_1(cp, false, false, false, false);
}


void display_channel_data_for_print(chan_info *cp)
{
  display_channel_data_1(cp, false, false, false, true);
}



/* ---------------- CHANNEL CURSOR ---------------- */

static void draw_graph_cursor(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if ((CURSOR(cp) < ap->losamp) || 
      (CURSOR(cp) > ap->hisamp)) 
    return;

  if ((cp->chan > 0) && 
      (cp->sound->channel_style == CHANNELS_SUPERIMPOSED)) 
    return;

  if (cp->cursor_visible) erase_cursor(cp);

  cp->cx = local_grf_x((double)(CURSOR(cp)) / (double)SND_SRATE(cp->sound), ap); /* not float -- this matters in very long files (i.e. > 40 minutes) */
  if (cp->just_zero)
    cp->cy = local_grf_y(0.0, ap);
  else cp->cy = local_grf_y(chn_sample(CURSOR(cp), cp, cp->edit_ctr), ap);
  draw_cursor(cp);
  cp->cursor_visible = true;
}


static void draw_sonogram_cursor_1(chan_info *cp)
{
  axis_info *fap;
  axis_context *fax;

  fap = cp->fft->axis;
  fax = cursor_context(cp);

  if ((fap) && (fax)) 
#if (!USE_CAIRO)
    draw_line(fax, cp->fft_cx, fap->y_axis_y0, cp->fft_cx, fap->y_axis_y1);
#else
  {
    fax = cp->cgx->ax; /* fap->ax does not work here?!? */
    if (fax->cr) cairo_destroy(fax->cr);
    fax->cr = gdk_cairo_create(fax->wn);
    if (cp->fft_cursor_visible)
      restore_sono_cursor_pix(cp, fax); /* returns true if old cursor was erased */
    else
      {
	color_t old_color;
	/* y_axis_y0 > y_axis_y1 (upside down coordinates) */
	free_sono_cursor_pix(cp);
	save_sono_cursor_pix(cp, fax, 2, fap->y_axis_y0 - fap->y_axis_y1, cp->fft_cx, fap->y_axis_y1);
	old_color = get_foreground_color(fax);
	set_foreground_color(fax, ss->sgx->cursor_color);
	draw_line(fax, cp->fft_cx, fap->y_axis_y0 - 1, cp->fft_cx, fap->y_axis_y1);
	set_foreground_color(fax, old_color);
      }
    cp->fft_cursor_visible = (!(cp->fft_cursor_visible));
  }
#endif
}


static void draw_sonogram_cursor(chan_info *cp)
{
  if ((cp->graph_transform_p) &&
      (cp->show_sonogram_cursor) &&
      (cp->fft) &&
      (cp->fft->axis) &&
      (cp->transform_graph_type == GRAPH_AS_SONOGRAM))
    {
      if (cp->fft_cursor_visible) draw_sonogram_cursor_1(cp);
      cp->fft_cx = local_grf_x((double)(CURSOR(cp)) / (double)SND_SRATE(cp->sound), cp->fft->axis);
      draw_sonogram_cursor_1(cp);
      cp->fft_cursor_visible = true;
    }
}


kbd_cursor_t cursor_decision(chan_info *cp)
{
  mus_long_t len;
  len = CURRENT_SAMPLES(cp);
  if (CURSOR(cp) >= len) CURSOR(cp) = len - 1; /* zero based, but in 0-length files, len = 0 */
  if (CURSOR(cp) < 0) CURSOR(cp) = 0;          /* perhaps the cursor should be forced off in empty files? */
  if (CURSOR(cp) < cp->axis->losamp)
    {
      if (CURSOR(cp) == 0) return(CURSOR_ON_LEFT);
      else 
	{
	  if (cp->sound->playing)
	    return(CURSOR_ON_RIGHT);
	  return(CURSOR_IN_MIDDLE);
	}
    }
  if (CURSOR(cp) > cp->axis->hisamp)
    {
      if (CURSOR(cp) >= (len - 1)) return(CURSOR_ON_RIGHT);
      else 
	{
	  if (cp->sound->playing)
	    return(CURSOR_ON_LEFT);
	  return(CURSOR_IN_MIDDLE);
	}
    }
  return(CURSOR_IN_VIEW);
}


void handle_cursor(chan_info *cp, kbd_cursor_t redisplay)
{
  /* no sync here */
  if (cp == NULL) return;
  if (redisplay != KEYBOARD_NO_ACTION)
    {
      snd_info *sp;
      sp = cp->sound;
      if (cp->verbose_cursor)
	{
	  show_cursor_info(cp); 
	  sp->minibuffer_on = MINI_CURSOR;
	} 
      if (redisplay != CURSOR_IN_VIEW)
	{
	  double gx = 0.0;
	  axis_info *ap;
	  ap = cp->axis;
	  switch (redisplay)
	    {
	    case CURSOR_ON_LEFT: 
	      gx = (double)(CURSOR(cp)) / (double)SND_SRATE(sp); 
	      break;

	    case CURSOR_ON_RIGHT: 
	      gx = (double)(CURSOR(cp)) / (double)SND_SRATE(sp) - ap->zx * ap->x_ambit; 
	      break;

	    case CURSOR_IN_MIDDLE: 
	      gx = (double)(CURSOR(cp)) / (double)SND_SRATE(sp) - ap->zx * 0.5 * ap->x_ambit; 
	      break;

	    default:
	      break;
	    }
	  if (gx < 0.0) gx = 0.0;
	  if (ap->x_ambit != 0.0)
	    reset_x_display(cp, (gx - ap->xmin) / ap->x_ambit, ap->zx);
	}
      else 
	{
	  if (cp->cursor_on) 
	    {
	      draw_graph_cursor(cp);
	      draw_sonogram_cursor(cp);
	    }
	}
    }
  {
    /* not sure about this */
    int i;
    for (i = 0; i < cp->edit_size; i++) 
      if (cp->edits[i]) 
	cp->edits[i]->cursor = CURSOR(cp);
  }
  update_possible_selection_in_progress(CURSOR(cp));
}


void handle_cursor_with_sync(chan_info *cp, kbd_cursor_t redisplay)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp) && (sp->sync != 0))
    {
      int i;
      sync_info *si;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++)
	handle_cursor(si->cps[i], redisplay);
      si = free_sync_info(si);
    }
  else handle_cursor(cp, redisplay);
}


void cursor_moveto(chan_info *cp, mus_long_t samp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp) && (sp->sync != 0))
    {
      int i;
      sync_info *si;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++)
	{
	  chan_info *ncp;
	  ncp = si->cps[i];
	  CURSOR(ncp) = samp;
	  handle_cursor(ncp, cursor_decision(ncp)); /* checks len */
	}
      si = free_sync_info(si);
    }
  else 
    {
      CURSOR(cp) = samp;
      handle_cursor(cp, cursor_decision(cp));
    }
}


void cursor_move(chan_info *cp, mus_long_t samps)
{
  cursor_moveto(cp, CURSOR(cp) + samps);
}


void cursor_moveto_without_verbosity(chan_info *cp, mus_long_t samp)
{
  bool old_verbose;
  int old_sync;
  old_verbose = cp->verbose_cursor;
  old_sync = cp->sound->sync;
  cp->verbose_cursor = false;
  cp->sound->sync = 0;
  cursor_moveto(cp, samp);
  cp->verbose_cursor = old_verbose;
  cp->sound->sync = old_sync;
}


void cursor_moveto_with_window(chan_info *cp, mus_long_t samp, mus_long_t left_samp, mus_long_t window_size)
{
  /* restore old window and cursor as much as possible */
  double gx;
  snd_info *sp;
  mus_long_t current_window_size;
  axis_info *ap;

  sp = cp->sound;
  ap = cp->axis;
  if (cp->cursor_visible)
    {
      if (cp->graph_time_p) erase_cursor(cp);
      cp->cursor_visible = false; /* don't redraw at old location */
    }
  if (cp->fft_cursor_visible)
    {
      if ((cp->fft) && (cp->graph_transform_p)) draw_sonogram_cursor_1(cp);
      cp->fft_cursor_visible = false; /* don't redraw at old location */
    }

  CURSOR(cp) = samp;
  current_window_size = ap->hisamp - ap->losamp;
  if (snd_abs_mus_long_t(current_window_size - window_size) < (mus_long_t)(0.1 * (double)window_size))
    gx = (double)(left_samp) / (double)SND_SRATE(sp);
  else gx = (double)(samp) / (double)SND_SRATE(sp) - ap->zx * 0.5 * ap->x_ambit; 
  if (gx < 0.0) gx = 0.0;
  if (ap->x_ambit != 0.0)
    reset_x_display(cp, (gx - ap->xmin) / ap->x_ambit, ap->zx);
}


void sync_cursors(chan_info *cp, mus_long_t samp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp) && (sp->sync != 0))
    {
      int i;
      sync_info *si;
      si = snd_sync(sp->sync);
      for (i = 0; i < si->chans; i++)
	CURSOR(si->cps[i]) = samp;
      si = free_sync_info(si);
    }
  else CURSOR(cp) = samp;
}



void show_cursor_info(chan_info *cp)
{
  char *expr_str;
  snd_info *sp;
  mus_float_t y, absy;
  int digits, i, len;
  mus_long_t samp;
  char *s1, *s2;

  sp = cp->sound;
  if ((sp->sync != 0) && (cp->chan != 0)) return;

  samp = CURSOR(cp);
  y = chn_sample(samp, cp, cp->edit_ctr);
  absy = fabs(y);
  if (absy < .0001) digits = 4;
  else if (absy<.001) digits = 3;
  else digits = 2;
  len = PRINT_BUFFER_SIZE;
  expr_str = (char *)calloc(len, sizeof(char));

  if (sp->nchans == 1)
    mus_snprintf(expr_str, PRINT_BUFFER_SIZE, _("cursor at %s (sample " MUS_LD ") = %s"),
		 s1 = x_axis_location_to_string(cp, (double)samp / (double)SND_SRATE(sp)),
		 samp,
		 s2 = prettyf(y, digits));
  else
    {
      if (sp->sync == 0)
	mus_snprintf(expr_str, PRINT_BUFFER_SIZE, _("chan %d, cursor at %s (sample " MUS_LD ") = %s"),
		     cp->chan + 1,
		     s1 = x_axis_location_to_string(cp, (double)samp / (double)SND_SRATE(sp)),
		     samp,
		     s2 = prettyf(y, digits));
      else
	{
	  /* in this case, assume we show all on chan 0 and ignore the call otherwise (see above) */
	  /* "cursor at..." then list of values */
	  mus_snprintf(expr_str, PRINT_BUFFER_SIZE, _("cursor at %s (sample " MUS_LD "): %s"),
		       s1 = x_axis_location_to_string(cp, (double)samp / (double)SND_SRATE(sp)),
		       samp,
		       s2 = prettyf(y, digits));
	  for (i = 1; i < sp->nchans; i++)
	    {
	      chan_info *ncp;

	      expr_str = mus_strcat(expr_str, ", ", &len);
	      free(s2);
	      ncp = sp->chans[i];
	      y = chn_sample(samp, ncp, ncp->edit_ctr);
	      absy = fabs(y);
	      if (absy < .0001) 
		digits = 4;
	      else 
		{
		  if (absy < .001) 
		    digits = 3;
		  else digits = 2;
		}
	      s2 = prettyf(y, digits);
	      expr_str = mus_strcat(expr_str, s2, &len);
	    }
	}
    }
  string_to_minibuffer(sp, expr_str);
  free(expr_str);
  free(s1);
  free(s2);
}


void goto_graph(chan_info *cp)
{
  if ((cp) && (!(cp->squelch_update)))
    {
      snd_info *sp;
      sp = cp->sound;
      if ((cp->chan == 0) || (sp->channel_style == CHANNELS_SEPARATE))
	goto_window(channel_graph(cp));
      else goto_window(channel_graph(sp->chans[0]));
    }
}



/* ---------------- graphics callbacks ---------------- */

#define SLOPPY_MOUSE 10

#if USE_NO_GUI
#define GUI_SET_CURSOR(w, cursor)
#else
#if USE_GTK
#define GUI_SET_CURSOR(w, cursor) gdk_window_set_cursor(WIDGET_TO_WINDOW(w), cursor)
#else
#define GUI_SET_CURSOR(w, cursor) XUndefineCursor(XtDisplay(w), XtWindow(w)); XDefineCursor(XtDisplay(w), XtWindow(w), cursor)
#endif
#endif

static click_loc_t within_graph(chan_info *cp, int x, int y)
{
  int x0, x1, y0, y1;
  axis_info *ap;
  #define SELECTION_DRAG_HEIGHT 30

  x0 = x - SLOPPY_MOUSE;
  x1 = x + SLOPPY_MOUSE;
  y0 = y - SLOPPY_MOUSE;
  y1 = y + SLOPPY_MOUSE;

  if (cp->graph_time_p)
    {
      ap = cp->axis;
      /* does (x, y) fall within the current axis bounds x_axis_x0|x1, y_axis_y0|y1 */
      if (((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0)) && 
	  ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1)))
	{
	  if ((y < (ap->y_offset + SELECTION_DRAG_HEIGHT)) &&
	      (selection_is_active_in_channel(cp)))
	    {
	      /* look for click at selection boundary */
	      mus_long_t pos;
	      double sr;
	      sr = (double)SND_SRATE(cp->sound);
	      pos = selection_beg(cp);
	      if ((pos >= ap->losamp) &&
		  (pos <= ap->hisamp))
		{
		  mus_long_t x0_pos, x1_pos;
		  x0_pos = snd_round_mus_long_t(ungrf_x(cp->axis, x0) * sr);
		  x1_pos = snd_round_mus_long_t(ungrf_x(cp->axis, x1) * sr);
		  if ((pos > x0_pos) && (pos < x1_pos))
		    return(CLICK_SELECTION_LEFT); /* "click" is a misnomer -- we have moved to the portion where we can grab the selection and resize it */
		}
	      pos = selection_end(cp);
	      if ((pos >= ap->losamp) &&
		  (pos <= ap->hisamp))
		{
		  mus_long_t x0_pos, x1_pos;
		  x0_pos = snd_round_mus_long_t(ungrf_x(cp->axis, x0) * sr);
		  x1_pos = snd_round_mus_long_t(ungrf_x(cp->axis, x1) * sr);
		  if ((pos > x0_pos) && (pos < x1_pos))
		    return(CLICK_SELECTION_RIGHT);
		}	      
	    }

	  if (hit_mix(cp, x, y) != NO_MIX_TAG)
	    return(CLICK_MIX);

	  if (hit_mark(cp, x, y, 0) != NULL)
	    return(CLICK_MARK);

	  return(CLICK_WAVE);
	}
    }

  if (((cp->graph_lisp_p) || 
       (XEN_HOOKED(lisp_graph_hook))) && 
      (cp->lisp_info))
    {
      ap = cp->lisp_info->axis;
      if (((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0)) && 
	  ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1)))
	return(CLICK_LISP);
    }

  if (cp->graph_transform_p)
    {
      ap = cp->fft->axis;
      if (!ap) return(CLICK_NOGRAPH); /* apparently can happen if fft is being redrawn when we click */
      /* look first for on-axis (axis drag) mouse */
#if HAVE_GL
      if ((cp->transform_graph_type == GRAPH_AS_SPECTROGRAM) && 
	  (ap->used_gl))
	{
	  GLdouble xx;
	  xx = unproject_to_x(x, y);
	  if ((xx > -0.7) && (xx < -0.49))
	    return(CLICK_FFT_AXIS);
	}
#endif
      if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
	{
	  if (((x >= ap->x_axis_x0) && (x <= ap->x_axis_x1)) && 
	      ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y0)))
	    return(CLICK_FFT_AXIS);
	}
      else
	{
	  if (((x0 <= ap->x_axis_x0) && (x1 >= ap->x_axis_x0)) && 
	      ((y <= ap->y_axis_y0) && (y >= ap->y_axis_y1)))
	    return(CLICK_FFT_AXIS);
	}
      /* now check within fft graph */
      if (((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0)) && 
	  ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1)))
	return(CLICK_FFT_MAIN);
    }

  return(CLICK_NOGRAPH);
}


void check_cursor_shape(chan_info *cp, int x, int y)
{
  click_loc_t where;
  chan_info *ncp;
  
  if ((!cp) || (!cp->sound) || (cp->active == CHANNEL_INACTIVE)) return;

  if (cp->sound->channel_style == CHANNELS_COMBINED) 
    ncp = which_channel(cp->sound, y);
  else ncp = cp;

  where = within_graph(ncp, x, y);

  if ((where == CLICK_SELECTION_LEFT) ||
      (where == CLICK_SELECTION_RIGHT) ||
      (where == CLICK_MIX) ||
      (where == CLICK_MARK) ||
      (where == CLICK_FFT_AXIS))
    {
      if (cp->cgx->current_cursor != ss->sgx->bounds_cursor)
	{
	  cp->cgx->current_cursor = ss->sgx->bounds_cursor;
	  GUI_SET_CURSOR(channel_graph(cp), ss->sgx->bounds_cursor);
	}
    }
  else
    {
      if (cp->cgx->current_cursor != ss->sgx->graph_cursor)
	{
	  cp->cgx->current_cursor = ss->sgx->graph_cursor;
	  GUI_SET_CURSOR(channel_graph(cp), ss->sgx->graph_cursor);
	}
    }
}


static char *describe_fft_point(chan_info *cp, int x, int y)
{
  mus_float_t xf, yf;
  axis_info *ap;
  fft_info *fp;
  sono_info *si;
  int ind, time, digits = 3;
  fp = cp->fft;
  ap = fp->axis;

  if ((ap->x_axis_x1 == ap->x_axis_x0) || (ap->y_axis_y1 == ap->y_axis_y0))
    return(mus_strdup("?"));

  x = mus_iclamp(ap->x_axis_x0, x, ap->x_axis_x1);
  xf = ap->x0 + (ap->x1 - ap->x0) * (mus_float_t)(x - ap->x_axis_x0) / (mus_float_t)(ap->x_axis_x1 - ap->x_axis_x0);

  digits = (int)ceil(fabs(cp->min_dB) / 20);
  if (digits < 2) digits = 2;

  if (cp->transform_graph_type == GRAPH_ONCE)
    {
      if (cp->transform_type == FOURIER)
	{
	  if (cp->fft_log_frequency)
	    {
	      mus_float_t minlx = 0.0;
	      if (ap->x0 > 1.0) minlx = log(ap->x0); else minlx = 0.0;
	      xf = exp(minlx + ((xf - ap->x0) * (log(ap->x1) - minlx) / (ap->x1 - ap->x0)));
	      ind = (int)((fp->current_size * xf) / (mus_float_t)SND_SRATE(cp->sound));
	    }
	  else ind = (int)((fp->current_size * xf) / (mus_float_t)SND_SRATE(cp->sound));
	}
      else ind = (int)xf;
      if (ind >= fp->current_size) ind = fp->current_size - 1;

      if ((cp->fft_with_phases) && 
	  (fp->phases) &&
	  (cp->transform_type == FOURIER))
	return(mus_format(_("(%.1f Hz: %.*f%s, %.*f radians (unscaled: %.*f)"),
			  xf,
			  digits,
			  (cp->fft_log_magnitude) ? in_dB(cp->min_dB, cp->lin_dB, (fp->data[ind] * fp->scale)) : (fp->data[ind] * fp->scale),
			  (cp->fft_log_magnitude) ? _("dB") : "",
			  digits,
			  fp->phases[ind],
			  digits,
			  fp->data[ind]));
      else
	return(mus_format(_("(%.1f%s: %.*f%s (unscaled: %.*f)"),
			  xf,
			  ((cp->transform_type == AUTOCORRELATION) ? _(" samps") : _(" Hz")),
			  digits,
			  (cp->fft_log_magnitude) ? in_dB(cp->min_dB, cp->lin_dB, (fp->data[ind] * fp->scale)) : (fp->data[ind] * fp->scale),
			  (cp->fft_log_magnitude) ? _("dB") : "",
			  digits,
			  fp->data[ind]));
    }
  else 
    {
      if (cp->transform_graph_type == GRAPH_AS_SONOGRAM)
	{
	  y = mus_iclamp(ap->y_axis_y1, y, ap->y_axis_y0);
	  yf = ap->y0 + (ap->y1 - ap->y0) * (mus_float_t)(y - ap->y_axis_y0) / (mus_float_t)(ap->y_axis_y1 - ap->y_axis_y0);

	  si = cp->sonogram_data;
	  if (cp->transform_type == FOURIER)
	    {
	      if (cp->fft_log_frequency)
		{
		  mus_float_t minlx = 0.0;
		  if (ap->y0 > 1.0) minlx = log(ap->y0); else minlx = 0.0;
		  yf = exp(minlx + ((yf - ap->y0) * (log(ap->y1) - minlx) / (ap->y1 - ap->y0)));
		  ind = (int)((fp->current_size * yf) / (mus_float_t)SND_SRATE(cp->sound));
		}
	      else ind = (int)((fp->current_size * yf) / (mus_float_t)SND_SRATE(cp->sound));
	    }
	  else ind = (int)yf;
	  if (ind >= si->total_bins) ind = si->total_bins - 1;
	  time = (int)(si->target_slices * (mus_float_t)(x - ap->x_axis_x0) / (mus_float_t)(ap->x_axis_x1 - ap->x_axis_x0));
	  if (time >= si->total_slices) time = si->total_slices - 1;
	  return(mus_format(_("(time: %.2f, freq: %.1f, val: %.*f%s (raw: %.*f))"),
			    xf, yf,
			    digits,
			    (cp->fft_log_magnitude) ? in_dB(cp->min_dB, cp->lin_dB, si->data[time][ind] / si->scale) : (si->data[time][ind] / si->scale),
			    (cp->fft_log_magnitude) ? _("dB") : "",
			    digits,
			    si->data[time][ind]));
	}
    }
  return(mus_strdup("?"));
}


void fftb(chan_info *cp, bool on)
{
  cp->graph_transform_p = on;
  set_toggle_button(channel_f(cp), on, false, (void *)cp);
  calculate_fft(cp);
}


void waveb(chan_info *cp, bool on)
{
  cp->graph_time_p = on;
  set_toggle_button(channel_w(cp), on, false, (void *)cp);
  update_graph_or_warn(cp);
}


static void propagate_wf_state(snd_info *sp)
{
  int i;
  bool w, f;
  chan_info *cp;

  cp = sp->chans[0];
  w = cp->graph_time_p;
  f = cp->graph_transform_p;

  for (i = 1; i < sp->nchans; i++) 
    {
      cp = sp->chans[i];
      cp->graph_time_p = w;
      cp->graph_transform_p = f;
      set_toggle_button(channel_f(cp), f, false, (void *)cp);
      set_toggle_button(channel_w(cp), w, false, (void *)cp);
    }
  for_each_sound_chan(sp, update_graph_or_warn);
}


void f_button_callback(chan_info *cp, bool on, bool with_control)
{
  snd_info *sp;
  cp->graph_transform_p = on;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    propagate_wf_state(sp);
  else
    {
      update_graph_or_warn(cp);
      if (with_control)
	{
	  int i;
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      chan_info *ncp;
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->graph_transform_p = on;
#if USE_GTK
		  set_toggle_button(channel_f(ncp), on, true, (void *)cp);
#else
		  set_toggle_button(channel_f(ncp), on, false, (void *)cp);
#endif
		  update_graph_or_warn(ncp);
		}
	    }
	}
      goto_graph(cp);
    }
}


void w_button_callback(chan_info *cp, bool on, bool with_control)
{
  snd_info *sp;
  cp->graph_time_p = on;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    propagate_wf_state(sp);
  else
    {
      update_graph_or_warn(cp);
      if (with_control)
	{
	  int i;
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      chan_info *ncp;
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->graph_time_p = on;
#if USE_GTK
		  set_toggle_button(channel_w(ncp), on, true, (void *)cp);
#else
		  set_toggle_button(channel_w(ncp), on, false, (void *)cp);
#endif
		  update_graph_or_warn(ncp);
		}
	    }
	}
      goto_graph(cp);
    }
}


bool key_press_callback(chan_info *ncp, int x, int y, int key_state, int keysym)
{
  /* called by every key-intercepting widget in the entire sound pane */
  chan_info *cp;
  snd_info *sp;

  if (!ncp) return(false);
  cp = virtual_selected_channel(ncp);
  sp = cp->sound;
  select_channel(sp, cp->chan);

  if (((cp->graph_lisp_p) || (XEN_HOOKED(lisp_graph_hook))) &&
      (within_graph(cp, x, y) == CLICK_LISP) &&
      (XEN_HOOKED(key_press_hook)))
    {
      /* return true to keep this key press from being passed to keyboard_command */
      XEN res = XEN_FALSE;
      res = run_or_hook(key_press_hook,
			XEN_LIST_4(C_INT_TO_XEN_SOUND(sp->index),
				   C_TO_XEN_INT(cp->chan),
				   C_TO_XEN_INT(keysym),
				   C_TO_XEN_INT(key_state)), /* this can have NumLock etc -- will be masked off in keyboard_command */
			S_key_press_hook);
      if (XEN_TRUE_P(res))
	return(false);
    }
  keyboard_command(cp, keysym, key_state);
  /* if lisp graph has cursor? */
  return(false);
}


chan_info *which_channel(snd_info *sp, int y)
{
  int i;
  chan_info *ncp = NULL;

  if (y <= 0) /* this can happen if we drag the mouse over the top of the Snd window, then release it */
    return(sp->chans[0]);

  for (i = 0; i < sp->nchans; i++)
    {
      axis_info *ap;
      chan_info *cp;
      cp = sp->chans[i];
      ap = cp->axis;
      if (y < ap->y_offset) 
	{
	  if (ncp)
	    return(ncp);
	  else return(cp);
	}
      ncp = cp;
    }
  return(ncp);
}


static mus_float_t fft_axis_extent(chan_info *cp)
{
  axis_info *ap;
  ap = cp->fft->axis;
  if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
    return((mus_float_t)(ap->x_axis_x1 - ap->x_axis_x0));
  else return((mus_float_t)(ap->y_axis_y0 - ap->y_axis_y1));
}


static void calculate_syncd_fft(chan_info *cp, int value)
{
  if (cp->sound->sync == value) calculate_fft(cp);
}


static bool dragged = false;
static oclock_t mouse_down_time;
static mark *mouse_mark = NULL;
static mark *play_mark = NULL;
static click_loc_t click_within_graph = CLICK_NOGRAPH;
static int fft_axis_start = 0;
#if HAVE_GL
  static mus_float_t fft_faxis_start = 0.0;
#endif
static int mix_tag = NO_MIX_TAG;
static chan_info *dragged_cp;

#ifdef MUS_MAC_OSX
static int press_x, press_y;
#endif


void graph_button_press_callback(chan_info *cp, int x, int y, int key_state, int button, oclock_t time)
{
  snd_info *sp;

  sp = cp->sound;
  if ((cp->active < CHANNEL_HAS_AXES) || (sp == NULL)) return; /* autotest silliness */

  /* if combining, figure out which virtual channel the mouse is in */
  if (sp->channel_style == CHANNELS_COMBINED) cp = which_channel(sp, y);

  mouse_down_time = time;
#ifdef MUS_MAC_OSX
  press_x = x;
  press_y = y;
#endif
  select_channel(sp, cp->chan);
  dragged_cp = cp;
  dragged = false;
  finish_selection_creation();

  if (cp->graph_time_p)
    {
      mouse_mark = hit_mark(cp, x, y, key_state);
      if (mouse_mark == NULL) 
	play_mark = hit_triangle(cp, x, y);
    }

  click_within_graph = within_graph(cp, x, y);

  switch (click_within_graph)
    {
    case CLICK_FFT_AXIS:
      if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
	{
#if HAVE_GL
	  if ((cp->transform_graph_type == GRAPH_AS_SPECTROGRAM) &&
	      (cp->fft->axis->used_gl))
	    fft_faxis_start = unproject_to_y(x, y);
#endif
	  fft_axis_start = x;
	}
      else fft_axis_start = y;
      break;

    case CLICK_LISP:
      if (XEN_HOOKED(mouse_press_hook))
	run_hook(mouse_press_hook,
		 XEN_LIST_6(C_INT_TO_XEN_SOUND(sp->index),
			    C_TO_XEN_INT(cp->chan),
			    C_TO_XEN_INT(button),
			    C_TO_XEN_INT(key_state),
			    C_TO_XEN_DOUBLE(ungrf_x(cp->lisp_info->axis, x)),
			    C_TO_XEN_DOUBLE(ungrf_y(cp->lisp_info->axis, y))),
		 S_mouse_press_hook);
      break;

    case CLICK_MIX:
    case CLICK_MARK:
    case CLICK_WAVE:
      if ((mouse_mark == NULL) && 
	  (play_mark == NULL))
	mix_tag = hit_mix(cp, x, y);
      break;

    case CLICK_SELECTION_LEFT:
    case CLICK_SELECTION_RIGHT:

#if 0      
      if ((mouse_mark == NULL) && 
	  (play_mark == NULL))
	mix_tag = hit_mix(cp, x, y);
      
      if (mix_tag == NO_MIX_TAG)
	{
	  dragged = true;
	  /* start adjustment of selection bounds */
	}
#endif
      dragged = true;
      restart_selection_creation(cp, click_within_graph == CLICK_SELECTION_RIGHT);
      break;

    case CLICK_NOGRAPH:
    case CLICK_FFT_MAIN:
      break;
    }
}


#if USE_MOTIF
  #define BUTTON_2 Button2
#else
  #define BUTTON_2 2
#endif


void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button)
{
  snd_info *sp;

  sp = cp->sound;
  if ((cp->active < CHANNEL_HAS_AXES) || (sp == NULL)) return; /* autotest silliness */

  if (sp->channel_style == CHANNELS_COMBINED)
    {
      if ((dragged) && (dragged_cp))
	cp = dragged_cp;
      else cp = which_channel(sp, y);
    }

  dragged_cp = NULL;
  if (!dragged)
    {
      if (play_mark)
	{
	  mark *old_mark;
	  old_mark = sp->playing_mark; /* needed because stop_playing clobbers sp->playing_mark */
	  if (sp->playing)
	    {
	      stop_playing_sound(sp, PLAY_BUTTON_UNSET);
	      set_play_button(sp, false);
	    }
	  if (play_mark != old_mark)
	    {
	      if (mark_sync(play_mark))
		play_syncd_mark(cp, play_mark);
	      else 
		{
		  if (key_state & snd_ControlMask)
		    play_sound(sp, mark_sample(play_mark), NO_END_SPECIFIED);
		  else play_channel(cp, mark_sample(play_mark), NO_END_SPECIFIED);
		}
	      sp->playing_mark = play_mark;
	    }
	  else sp->playing_mark = NULL;
	  play_mark = NULL;
	}
      else
	{
	  click_loc_t actax;
	  actax = within_graph(cp, x, y);

	  if ((XEN_HOOKED(mouse_click_hook)) &&
	      (XEN_TRUE_P(run_or_hook(mouse_click_hook,
				      XEN_LIST_7(C_INT_TO_XEN_SOUND(sp->index),
						 C_TO_XEN_INT(cp->chan),
						 C_TO_XEN_INT(button),
						 C_TO_XEN_INT(key_state),
						 C_TO_XEN_INT(x),
						 C_TO_XEN_INT(y),
						 C_TO_XEN_INT((int)(((actax == CLICK_FFT_AXIS) || (actax == CLICK_FFT_MAIN)) ? 
								     TRANSFORM_AXIS_INFO : ((actax == CLICK_LISP) ? 
											    LISP_AXIS_INFO : TIME_AXIS_INFO)))),
				      S_mouse_click_hook))))
	    return;

	  switch (actax)
	    {
	    case CLICK_SELECTION_LEFT:
	    case CLICK_SELECTION_RIGHT:
	    case CLICK_MIX:
	    case CLICK_MARK:
	    case CLICK_WAVE:
	      if (button == BUTTON_2) /* the middle button */
		{
		  cp->cursor_on = true;
		  cursor_moveto(cp, snd_round_mus_long_t(ungrf_x(cp->axis, x) * (double)SND_SRATE(sp)));
		  paste_region(region_list_position_to_id(0), cp);
		}
	      else 
		{
		  if (key_state & (snd_ShiftMask | snd_ControlMask | snd_MetaMask))
		    {
		      mus_long_t samps;
		      axis_info *ap;
		      /* zoom request -> each added key zooms closer, as does each successive click */
		      ap = cp->axis;
		      samps = CURRENT_SAMPLES(cp);
		      if ((samps > 0) && ((ap->zx * (double)samps) > 1.0))
			{
			  if (key_state & snd_ShiftMask) ap->zx *= .5;
			  if (key_state & snd_ControlMask) ap->zx *= .5;
			  if (key_state & snd_MetaMask) ap->zx *= .5;
			  if (ap->x_ambit != 0.0)
			    ap->sx = (((double)(CURSOR(cp)) / (double)SND_SRATE(sp) - 
				       ap->zx * 0.5 * (ap->xmax - ap->xmin)) - ap->xmin) / ap->x_ambit;
			  apply_x_axis_change(ap, cp);
			  resize_sx_and_zx(cp);
			}
		    }
		  else
		    {
		      cp->cursor_on = true;
		      cursor_moveto(cp, snd_round_mus_long_t(ungrf_x(cp->axis, x) * (double)SND_SRATE(sp)));
		      if (mouse_mark)
			{
			  XEN res = XEN_FALSE;
			  if (XEN_HOOKED(mark_click_hook))
			    res = run_progn_hook(mark_click_hook,
						 XEN_LIST_1(new_xen_mark(mark_to_int(mouse_mark))),
						 S_mark_click_hook);
			  if (!(XEN_TRUE_P(res)))
			    {
			      mus_long_t samp;
			      int sync;
			      samp = mark_sample(mouse_mark);
			      sync = mark_sync(mouse_mark);
			      if (sync == 0)
				report_in_minibuffer(sp, _("mark %d at sample " MUS_LD " (%3f secs): %3f"), 
						     mark_to_int(mouse_mark), 
						     samp,
						     (double)samp / (double)(SND_SRATE(sp)),
						     chn_sample(samp, cp, cp->edit_ctr));
			      else
				report_in_minibuffer(sp, _("mark %d at sample " MUS_LD " (%3f secs): %3f, (sync: %d)"), 
						     mark_to_int(mouse_mark), 
						     samp,
						     (double)samp / (double)(SND_SRATE(sp)),
						     chn_sample(samp, cp, cp->edit_ctr),
						     sync);
			    }
			}
		      else
			{
			  if (mix_tag != NO_MIX_TAG)
			    {
			      XEN res = XEN_FALSE;
			      /* the mix has already been selected by hit-mix above (to prepare drag) */
			      if (XEN_HOOKED(mix_click_hook))
				res = run_progn_hook(mix_click_hook,
						     XEN_LIST_1(new_xen_mix(mix_tag)),
						     S_mix_click_hook);
			      if (!(XEN_TRUE_P(res)))
				{
				  make_mix_dialog();
				  reflect_mix_change(mix_tag);
				}
			    }
			}
		    }
		}
	      break;
	      
	    case CLICK_FFT_AXIS: /* not dragged, so must have clicked close to axis */
	    case CLICK_FFT_MAIN:
	      {
		char *str;
		str = describe_fft_point(cp, x, y);
		string_to_minibuffer(sp, str);
		if (str) free(str);
	      }
	      break;

	    default:
	      break;
	    }
	}
    }
  else
    {
      if (mouse_mark)
	{
	  finish_moving_mark(cp, mouse_mark);
	  mouse_mark = NULL;
	  dragged = false;
	}
      else
	{
	  if (play_mark)
	    {
	      finish_moving_play_mark(cp);
	      stop_playing_sound(sp, PLAY_BUTTON_UNSET);
	      play_mark = NULL;
	      dragged = false;
	    }
	  else
	    {
	      if (mix_tag != NO_MIX_TAG)
		{
		  finish_moving_mix_tag(mix_tag, x);
		  dragged = false;
		  mix_tag = NO_MIX_TAG;
		}
	      else
		{
		  if ((click_within_graph == CLICK_WAVE) ||
		      (click_within_graph == CLICK_SELECTION_LEFT) ||
		      (click_within_graph == CLICK_SELECTION_RIGHT) ||
		      (click_within_graph == CLICK_MIX) ||
		      (click_within_graph == CLICK_MARK))
		    {
		      cancel_selection_watch();
		      finish_selection_creation();
		      dragged = false;
		      if (show_selection_transform(ss)) 
			{
			  if (sp->sync)
			    for_each_normal_chan_with_int(calculate_syncd_fft, sp->sync);
			  else calculate_fft(cp);
			}
		    }
		}
	    }
	}
    }
}


static oclock_t first_time = 0;
static mus_long_t mouse_cursor = 0;
static XEN mark_drag_triangle_hook;

void graph_button_motion_callback(chan_info *cp, int x, int y, oclock_t time)
{
  /* this refers to mouse drag, not just any motion */
  snd_info *sp;
  oclock_t mouse_time;
  /* this needs to be a little slow about deciding that we are dragging, as opposed to a slow click */

  mouse_time = time;
  if ((mouse_time - mouse_down_time) < ss->click_time) return;

#ifdef MUS_MAC_OSX
  /* on the Mac, we seem to get motion events even without any motion, and the times seem very short */
  if ((x == press_x) && (y == press_y)) return;
#endif

  sp = cp->sound;
  if ((cp->active < CHANNEL_HAS_AXES) || (sp == NULL)) return; /* autotest silliness */

  if (sp->channel_style == CHANNELS_COMBINED) /* in united chans, dragging mark shouldn't change channel */
    {
      if (dragged_cp)
	cp = dragged_cp;
      else cp = which_channel(sp, y);
    }

  if (mouse_mark)
    {
      move_mark(cp, mouse_mark, x);
      report_in_minibuffer(sp, "%.4f", ungrf_x(cp->axis, x));
      dragged = true;
    }
  else
    {
      if (play_mark)
	{
	  XEN drag_res = XEN_FALSE;
	  if (XEN_HOOKED(mark_drag_triangle_hook))
	    drag_res = run_progn_hook(mark_drag_triangle_hook,
				      XEN_LIST_4(new_xen_mark(mark_to_int(play_mark)),
						 C_TO_XEN_INT(x),
						 C_TO_XEN_INT((int)time),
						 C_TO_XEN_BOOLEAN(dragged)),
				      S_mark_drag_triangle_hook);
	  if (!dragged)
	    {
	      first_time = mouse_time;
	      dragged = true;
	      mouse_cursor = CURSOR(cp);
	      if (!(XEN_TRUE_P(drag_res)))
		{
		  sp->speed_control = 0.0;
		  play_channel(cp, mark_sample(play_mark), NO_END_SPECIFIED);
		}
	    }
	  else
	    {
	      mus_long_t samps;
	      oclock_t time_interval;
	      time_interval = mouse_time - first_time;
	      first_time = mouse_time;
	      samps = move_play_mark(cp, &mouse_cursor, x);
	      if (!(XEN_TRUE_P(drag_res)))
		{
		  if (time_interval != 0)
		    sp->speed_control = (mus_float_t)((double)(samps * 1000) / (double)(time_interval * SND_SRATE(sp)));
		  else sp->speed_control = 0.0;
		}
	    }
	}
      else
	{
	  mus_float_t old_cutoff;
	  switch (click_within_graph)
	    {
	    case CLICK_SELECTION_LEFT:
	    case CLICK_SELECTION_RIGHT:
	    case CLICK_MIX:
	    case CLICK_MARK:
	    case CLICK_WAVE:
	      if (dragged) 
		report_in_minibuffer(sp, "%.4f", ungrf_x(cp->axis, x));

	      if (mix_tag != NO_MIX_TAG)
		{
		  move_mix_tag(mix_tag, x, y);
		  dragged = true;
		  return;
		}
	      if (!dragged) 
		start_selection_creation(cp, snd_round_mus_long_t(ungrf_x(cp->axis, x) * SND_SRATE(sp)));
	      else 
		{
		  update_possible_selection_in_progress(snd_round_mus_long_t(ungrf_x(cp->axis, x) * SND_SRATE(sp)));
		  move_selection(cp, x);
		}
	      dragged = true;
	      break;

	    case CLICK_FFT_AXIS:
	      {
		/* change spectrum_end(ss) and redisplay fft */
		/*   changed 25-Oct-07 -- follow sync and separate chan */
		mus_float_t new_cutoff;
		old_cutoff = cp->spectrum_end;
		if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
		  {
#if HAVE_GL
		    if ((cp->transform_graph_type == GRAPH_AS_SPECTROGRAM) &&
			(cp->fft->axis->used_gl))
		      {
			mus_float_t ny;
			ny = unproject_to_y(x, y);
			new_cutoff = cp->spectrum_end + (fft_faxis_start - ny);
			fft_faxis_start = ny;
		      }
		    else
#endif 
		      new_cutoff = cp->spectrum_end + ((mus_float_t)(fft_axis_start - x) / fft_axis_extent(cp));
		    fft_axis_start = x;
		  }
		else 
		  {
		    new_cutoff = cp->spectrum_end + ((mus_float_t)(y - fft_axis_start) / fft_axis_extent(cp));
		    fft_axis_start = y;
		  }
		if (new_cutoff > 1.0) new_cutoff = 1.0;
		if (new_cutoff < 0.001) new_cutoff = 0.001;
		if (sp->sync != 0)
		  {
		    int i;
		    sync_info *si;
		    si = snd_sync(sp->sync);
		    for (i = 0; i < si->chans; i++) 
		      {
			si->cps[i]->spectrum_end = new_cutoff;
			if (cp->transform_graph_type != GRAPH_ONCE)
			  sono_update(si->cps[i]);
			else update_graph(si->cps[i]);
		      }
		    si = free_sync_info(si);
		  }
		else
		  {
		    cp->spectrum_end = new_cutoff;
		    if (cp->transform_graph_type != GRAPH_ONCE)
		      sono_update(cp);
		    else update_graph(cp);
		  }
	      }
	      break;

	    case CLICK_LISP:
	      if (XEN_HOOKED(mouse_drag_hook))
		run_hook(mouse_drag_hook,
			 XEN_LIST_6(C_INT_TO_XEN_SOUND(cp->sound->index),
				    C_TO_XEN_INT(cp->chan),
				    C_TO_XEN_INT(-1),
				    C_TO_XEN_INT(-1),
				    C_TO_XEN_DOUBLE(ungrf_x(cp->lisp_info->axis, x)),
				    C_TO_XEN_DOUBLE(ungrf_y(cp->lisp_info->axis, y))),
			 S_mouse_drag_hook);
	      break;

	    case CLICK_FFT_MAIN:
	      if (cp->verbose_cursor)
		{
		  char *str;
		  str = describe_fft_point(cp, x, y);
		  string_to_minibuffer(cp->sound, str);
		  if (str) free(str);
		}
	      break;

	    default:
	      break;
	    }
	}
    }
}


void channel_resize(chan_info *cp)
{
  snd_info *sp;
  if ((cp == NULL) || (cp->active < CHANNEL_HAS_AXES) || (cp->sound == NULL)) return;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      if (cp->chan == 0)
	for_each_sound_chan(sp, update_graph_or_warn);
    }
  else update_graph_or_warn(cp);
}


void edit_history_select(chan_info *cp, int row)
{
#if WITH_RELATIVE_PANES || USE_GTK
  if (cp->sound->channel_style != CHANNELS_SEPARATE)
    {
      int k;
      snd_info *sp;
      chan_info *ncp = NULL;
      sp = cp->sound;
      for (k = 1; k < sp->nchans; k++)
	if (sp->chans[k]->edhist_base > row)
	  {
	    ncp = sp->chans[k - 1];
	    break;
	  }
      if (ncp == NULL) ncp = sp->chans[sp->nchans - 1];
      undo_edit_with_sync(ncp, ncp->edit_ctr - row + ncp->edhist_base);
      goto_graph(ncp);
    }
  else 
#endif
    {
      undo_edit_with_sync(cp, cp->edit_ctr - row);
      goto_graph(cp);
    }
}


static bool run_time_graph_hook(chan_info *cp, axis_context *ax)
{
  XEN result = XEN_FALSE;
  if ((cp->hookable == WITH_HOOK) &&
      (XEN_HOOKED(time_graph_hook)))
    {
      result = run_progn_hook(time_graph_hook,
			      XEN_LIST_2(C_INT_TO_XEN_SOUND(cp->sound->index),
					 C_TO_XEN_INT(cp->chan)),
			      S_time_graph_hook);
      if (XEN_PIXEL_P(result))
	{
	  set_foreground_color(ax, (color_t)XEN_UNWRAP_PIXEL(result));
	  return(true);
	}
    }
  return(false);
}


axis_context *set_context(chan_info *cp, chan_gc_t gc)
{
  axis_context *ax;
  state_context *sx;
  chan_context *cx;
  cx = cp->tcgx;
  if (!cx) cx = cp->cgx;
  ax = cx->ax;
  sx = ss->sgx;

  if (cp->cgx->selected)
    {
      switch (gc)
	{
	case CHAN_GC: ax->gc = sx->selected_basic_gc;        break;
	case CHAN_IGC: ax->gc = sx->selected_erase_gc;       break;
	case CHAN_SELGC: ax->gc = sx->selected_selection_gc; break;
	case CHAN_CGC: ax->gc = sx->selected_cursor_gc;      break;
	case CHAN_MGC: ax->gc = sx->selected_mark_gc;        break;
	case CHAN_MXGC: ax->gc = sx->mix_gc;                 break;
	case CHAN_TMPGC: 
	  if (!(run_time_graph_hook(cp, ax)))
	    ax->gc = sx->selected_basic_gc;     
	  break;
	}
    }
  else
    {
      switch (gc)
	{
	case CHAN_GC: ax->gc = sx->basic_gc;             break;
	case CHAN_IGC: ax->gc = sx->erase_gc;            break;
	case CHAN_SELGC: ax->gc = sx->selection_gc;      break;
	case CHAN_CGC: ax->gc = sx->cursor_gc;           break;
	case CHAN_MGC: ax->gc = sx->mark_gc;             break;
	case CHAN_MXGC: ax->gc = sx->mix_gc;             break;
	case CHAN_TMPGC: 
	  if (!(run_time_graph_hook(cp,ax)))
	    {
	      ax->gc = sx->combined_basic_gc;
	      switch (cp->chan % 4)
		{
		case 0: set_foreground_color(ax, sx->black);      break;
		case 1: set_foreground_color(ax, sx->red);        break;
		case 2: set_foreground_color(ax, sx->green);      break;
		case 3: set_foreground_color(ax, sx->light_blue); break;
		}
	    }
	  break;
	}
    }
  return(ax);
}


axis_context *copy_context(chan_info *cp)            {return(set_context(cp, CHAN_GC));}
axis_context *erase_context(chan_info *cp)           {return(set_context(cp, CHAN_IGC));}
axis_context *selection_context(chan_info *cp)       {return(set_context(cp, CHAN_SELGC));}
axis_context *cursor_context(chan_info *cp)          {return(set_context(cp, CHAN_CGC));}
axis_context *mark_tag_context(chan_info *cp)        {return(set_context(cp, CHAN_MGC));}
axis_context *mix_waveform_context(chan_info *cp)    {return(set_context(cp, CHAN_MXGC));}
static axis_context *combined_context(chan_info *cp) {return(set_context(cp, CHAN_TMPGC));}



typedef enum {CP_GRAPH_TRANSFORM_P, CP_GRAPH_TIME_P, CP_FRAMES, CP_CURSOR, CP_GRAPH_LISP_P, CP_AP_LOSAMP, CP_AP_HISAMP, CP_SQUELCH_UPDATE,
	      CP_EDIT_CTR, CP_CURSOR_STYLE, CP_EDIT_HOOK, CP_UNDO_HOOK, CP_AFTER_EDIT_HOOK,
	      CP_SHOW_Y_ZERO, CP_SHOW_MARKS, CP_TIME_GRAPH_TYPE, CP_WAVO_HOP, CP_WAVO_TRACE, CP_MAX_TRANSFORM_PEAKS, 
	      CP_SHOW_TRANSFORM_PEAKS, CP_ZERO_PAD, CP_VERBOSE_CURSOR, CP_FFT_LOG_FREQUENCY, CP_FFT_LOG_MAGNITUDE,
	      CP_WAVELET_TYPE, CP_SPECTRO_HOP, CP_TRANSFORM_SIZE, CP_TRANSFORM_GRAPH_TYPE, CP_FFT_WINDOW, CP_TRANSFORM_TYPE,
	      CP_TRANSFORM_NORMALIZATION, CP_SHOW_MIX_WAVEFORMS, CP_TIME_GRAPH_STYLE, CP_LISP_GRAPH_STYLE, CP_TRANSFORM_GRAPH_STYLE, CP_DOT_SIZE,
	      CP_SHOW_AXES, CP_GRAPHS_HORIZONTAL, CP_CURSOR_SIZE, CP_CURSOR_POSITION,
	      CP_EDPOS_FRAMES, CP_X_AXIS_STYLE, CP_UPDATE_TIME, CP_UPDATE_TRANSFORM_GRAPH, CP_UPDATE_LISP, CP_PROPERTIES,
	      CP_MIN_DB, CP_SPECTRO_X_ANGLE, CP_SPECTRO_Y_ANGLE, CP_SPECTRO_Z_ANGLE, CP_SPECTRO_X_SCALE, CP_SPECTRO_Y_SCALE, CP_SPECTRO_Z_SCALE,
	      CP_SPECTRUM_END, CP_SPECTRUM_START, CP_FFT_WINDOW_BETA, CP_AP_SX, CP_AP_SY, CP_AP_ZX, CP_AP_ZY, CP_MAXAMP, CP_EDPOS_MAXAMP,
	      CP_BEATS_PER_MINUTE, CP_EDPOS_CURSOR, CP_SHOW_GRID, CP_SHOW_SONOGRAM_CURSOR, CP_GRID_DENSITY, CP_MAXAMP_POSITION,
	      CP_EDPOS_MAXAMP_POSITION, CP_BEATS_PER_MEASURE, CP_FFT_WINDOW_ALPHA, CP_TRACKING_CURSOR_STYLE, CP_FFT_WITH_PHASES
} cp_field_t;


static XEN cp_edpos;
static int cp_edpos_loc = NOT_A_GC_LOC;

static XEN channel_get(XEN snd, XEN chn_n, cp_field_t fld, const char *caller)
{
  chan_info *cp;
  snd_info *sp = NULL;
  int i;
  XEN res = XEN_EMPTY_LIST;
  if (XEN_TRUE_P(snd))
    {
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res = XEN_CONS(channel_get(C_TO_XEN_INT(i), chn_n, fld, caller), res);
	  /* I think not SOUND_WRAPPER here -- get/set would then be operating on sounds that
	   *   are not returned by 'sounds' and return #f from 'sound?', so it would almost
	   *   certainly cause confusion.  The globals fields should be reflected however.
	   */
	}
      return(res);
    }
  else
    {
      if (XEN_TRUE_P(chn_n))
	{
	  sp = get_sp(snd);
	  if (sp == NULL)
	    return(snd_no_such_sound_error(caller, snd));
	  for (i = sp->nchans - 1; i >= 0; i--)
	    res = XEN_CONS(channel_get(snd, C_TO_XEN_INT(i), fld, caller), res);
	  return(res);
	}
      else
	{
	  ASSERT_CHANNEL(caller, snd, chn_n, 1);
	  cp = get_cp(snd, chn_n, caller);
	  if (!cp) return(XEN_FALSE); /* perhaps snd-error-hook cancelled the error? */
	  switch (fld)
	    {
	    case CP_EDIT_CTR:                return(C_TO_XEN_INT(cp->edit_ctr));                               break;
	    case CP_GRAPH_TRANSFORM_P:       return(C_TO_XEN_BOOLEAN(cp->graph_transform_p));                  break;
	    case CP_GRAPH_TIME_P:            return(C_TO_XEN_BOOLEAN(cp->graph_time_p));                       break;
	    case CP_CURSOR:                  return(C_TO_XEN_INT64_T(CURSOR(cp)));                             break;
	    case CP_EDPOS_CURSOR:            return(C_TO_XEN_INT64_T(cp->edits[to_c_edit_position(cp, cp_edpos, S_cursor, 3)]->cursor)); break;
	    case CP_FRAMES:                  return(C_TO_XEN_INT64_T(CURRENT_SAMPLES(cp)));                    break;
	    case CP_GRAPH_LISP_P:            return(C_TO_XEN_BOOLEAN(cp->graph_lisp_p));                       break;
	    case CP_AP_LOSAMP:               if (cp->axis) return(C_TO_XEN_INT64_T(cp->axis->losamp));         break;
	    case CP_AP_HISAMP:               if (cp->axis) return(C_TO_XEN_INT64_T(cp->axis->hisamp));         break;
	    case CP_SQUELCH_UPDATE:          return(C_TO_XEN_BOOLEAN(cp->squelch_update));                     break;
	    case CP_CURSOR_SIZE:             return(C_TO_XEN_INT(cp->cursor_size));                            break;

	    case CP_CURSOR_STYLE:
	      if (cp->cursor_style != CURSOR_PROC)
		return(C_TO_XEN_INT((int)(cp->cursor_style)));
	      if (XEN_PROCEDURE_P(cp->cursor_proc))
		return(cp->cursor_proc);
	      return(ss->cursor_proc);
	      break;

	    case CP_TRACKING_CURSOR_STYLE:
	      return(C_TO_XEN_INT((int)(cp->tracking_cursor_style)));
	      break;

	    case CP_EDIT_HOOK:
	      if (!(XEN_HOOK_P(cp->edit_hook)))
		{
		  cp->edit_hook = XEN_DEFINE_SIMPLE_HOOK(0);
		  cp->edit_hook_loc = snd_protect(cp->edit_hook);
		}
	      return(cp->edit_hook);
	      break;

	    case CP_AFTER_EDIT_HOOK:
	      if (!(XEN_HOOK_P(cp->after_edit_hook)))
		{
		  cp->after_edit_hook = XEN_DEFINE_SIMPLE_HOOK(0);
		  cp->after_edit_hook_loc = snd_protect(cp->after_edit_hook);
		}
	      return(cp->after_edit_hook);
	      break;

	    case CP_UNDO_HOOK:               
	      if (!(XEN_HOOK_P(cp->undo_hook)))
		{
		  cp->undo_hook = XEN_DEFINE_SIMPLE_HOOK(0);
		  cp->undo_hook_loc = snd_protect(cp->undo_hook);
		}
	      return(cp->undo_hook);
	      break;

	    case CP_SHOW_Y_ZERO:             return(C_TO_XEN_BOOLEAN(cp->show_y_zero));                          break;
	    case CP_SHOW_GRID:               return(C_TO_XEN_BOOLEAN((bool)(cp->show_grid)));                    break;
	    case CP_GRID_DENSITY:            return(C_TO_XEN_DOUBLE(cp->grid_density));                          break;
	    case CP_SHOW_SONOGRAM_CURSOR:    return(C_TO_XEN_BOOLEAN((bool)(cp->show_sonogram_cursor)));         break;
	    case CP_SHOW_MARKS:              return(C_TO_XEN_BOOLEAN(cp->show_marks));                           break;
	    case CP_TIME_GRAPH_TYPE:         return(C_TO_XEN_INT((int)(cp->time_graph_type)));                   break;
	    case CP_WAVO_HOP:                return(C_TO_XEN_INT(cp->wavo_hop));                                 break;
	    case CP_WAVO_TRACE:              return(C_TO_XEN_INT(cp->wavo_trace));                               break;
	    case CP_MAX_TRANSFORM_PEAKS:     return(C_TO_XEN_INT(cp->max_transform_peaks));                      break;
	    case CP_ZERO_PAD:                return(C_TO_XEN_INT(cp->zero_pad));                                 break;
	    case CP_WAVELET_TYPE:            return(C_TO_XEN_INT(cp->wavelet_type));                             break;
	    case CP_SHOW_TRANSFORM_PEAKS:    return(C_TO_XEN_BOOLEAN(cp->show_transform_peaks));                 break;
	    case CP_VERBOSE_CURSOR:          return(C_TO_XEN_BOOLEAN(cp->verbose_cursor));                       break;
	    case CP_FFT_LOG_FREQUENCY:       return(C_TO_XEN_BOOLEAN(cp->fft_log_frequency));                    break;
	    case CP_FFT_LOG_MAGNITUDE:       return(C_TO_XEN_BOOLEAN(cp->fft_log_magnitude));                    break;
	    case CP_FFT_WITH_PHASES:         return(C_TO_XEN_BOOLEAN(cp->fft_with_phases));                      break;
	    case CP_SPECTRO_HOP:             return(C_TO_XEN_INT(cp->spectro_hop));                              break;
	    case CP_TRANSFORM_SIZE:          return(C_TO_XEN_INT64_T(cp->transform_size));                         break;
	    case CP_TRANSFORM_GRAPH_TYPE:    return(C_TO_XEN_INT((int)(cp->transform_graph_type)));              break;
	    case CP_FFT_WINDOW:              return(C_TO_XEN_INT((int)(cp->fft_window)));                        break;
	    case CP_TRANSFORM_TYPE:          return(C_TO_XEN_INT(cp->transform_type));                           break;
	    case CP_TRANSFORM_NORMALIZATION: return(C_TO_XEN_INT((int)(cp->transform_normalization)));           break;
	    case CP_SHOW_MIX_WAVEFORMS:      return(C_TO_XEN_BOOLEAN(cp->show_mix_waveforms));                   break;
	    case CP_TIME_GRAPH_STYLE:        return(C_TO_XEN_INT(cp->time_graph_style));                         break;
	    case CP_LISP_GRAPH_STYLE:        return(C_TO_XEN_INT(cp->lisp_graph_style));                         break;
	    case CP_TRANSFORM_GRAPH_STYLE:   return(C_TO_XEN_INT(cp->transform_graph_style));                    break;
	    case CP_X_AXIS_STYLE:            return(C_TO_XEN_INT((int)(cp->x_axis_style)));                      break;
	    case CP_DOT_SIZE:                return(C_TO_XEN_INT(cp->dot_size));                                 break;
	    case CP_SHOW_AXES:               return(C_TO_XEN_INT((int)(cp->show_axes)));                         break;
	    case CP_GRAPHS_HORIZONTAL:       return(C_TO_XEN_BOOLEAN(cp->graphs_horizontal));                    break;
	    case CP_CURSOR_POSITION:         return(XEN_LIST_2(C_TO_XEN_INT(cp->cx), C_TO_XEN_INT(cp->cy)));     break;
	    case CP_EDPOS_FRAMES:            return(C_TO_XEN_INT64_T(to_c_edit_samples(cp, cp_edpos, caller, 3))); break;

	    case CP_UPDATE_TIME:
#if USE_GTK
	      if (!(cp->cgx->ax->wn)) fixup_cp_cgx_ax_wn(cp);
#endif
	      /* any display-oriented background process must 1st be run to completion
	       *       display checks for waiting process and does not update display if one found!
	       */
	      finish_peak_env(cp);
	      cp->waiting_to_make_graph = false;
	      display_channel_time_data(cp);
	      break;

	    case CP_UPDATE_LISP:
	      display_channel_lisp_data(cp);
	      break;

	    case CP_UPDATE_TRANSFORM_GRAPH: 
	      if (cp->graph_transform_p)
		{
		  if (chan_fft_in_progress(cp)) 
		    force_fft_clear(cp);
		  
		  ss->checking_explicitly = true;  /* do not allow UI events to intervene here! */
		  if (cp->transform_graph_type == GRAPH_ONCE)
		    single_fft(cp, FORCE_REDISPLAY, FORCE_REFFT);
		  else
		    {
		      void *val;
		      val = (void *)make_sonogram_state(cp, FORCE_REFFT);
		      while (sonogram_in_slices(val) == BACKGROUND_CONTINUE) ;
		    }
		  ss->checking_explicitly = false;
		}
	      break;

	    case CP_PROPERTIES:
	      if (!(XEN_VECTOR_P(cp->properties)))
		{
		  cp->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
		  cp->properties_loc = snd_protect(cp->properties);
		}
	      return(XEN_VECTOR_REF(cp->properties, 0));
	      break;

	    case CP_AP_SX:            if (cp->axis) return(C_TO_XEN_DOUBLE(cp->axis->sx)); break;
	    case CP_AP_SY:            if (cp->axis) return(C_TO_XEN_DOUBLE(cp->axis->sy)); break;
	    case CP_AP_ZX:            if (cp->axis) return(C_TO_XEN_DOUBLE(cp->axis->zx)); break;
	    case CP_AP_ZY:            if (cp->axis) return(C_TO_XEN_DOUBLE(cp->axis->zy)); break;
	    case CP_MIN_DB:           return(C_TO_XEN_DOUBLE(cp->min_dB));                 break;
	    case CP_SPECTRO_X_ANGLE:  return(C_TO_XEN_DOUBLE(cp->spectro_x_angle));        break;
	    case CP_SPECTRO_Y_ANGLE:  return(C_TO_XEN_DOUBLE(cp->spectro_y_angle));        break;
	    case CP_SPECTRO_Z_ANGLE:  return(C_TO_XEN_DOUBLE(cp->spectro_z_angle));        break;
	    case CP_SPECTRO_X_SCALE:  return(C_TO_XEN_DOUBLE(cp->spectro_x_scale));        break;
	    case CP_SPECTRO_Y_SCALE:  return(C_TO_XEN_DOUBLE(cp->spectro_y_scale));        break;
	    case CP_SPECTRO_Z_SCALE:  return(C_TO_XEN_DOUBLE(cp->spectro_z_scale));        break;
	    case CP_SPECTRUM_END:     return(C_TO_XEN_DOUBLE(cp->spectrum_end));           break;
	    case CP_SPECTRUM_START:   return(C_TO_XEN_DOUBLE(cp->spectrum_start));         break;
	    case CP_FFT_WINDOW_ALPHA: return(C_TO_XEN_DOUBLE(cp->fft_window_alpha));       break;
	    case CP_FFT_WINDOW_BETA:  return(C_TO_XEN_DOUBLE(cp->fft_window_beta));        break;
	    case CP_BEATS_PER_MINUTE: return(C_TO_XEN_DOUBLE(cp->beats_per_minute));       break;
	    case CP_BEATS_PER_MEASURE: return(C_TO_XEN_INT(cp->beats_per_measure));        break;
	    case CP_MAXAMP:           return(C_TO_XEN_DOUBLE(channel_maxamp(cp, AT_CURRENT_EDIT_POSITION))); break;
	    case CP_EDPOS_MAXAMP:     return(C_TO_XEN_DOUBLE(channel_maxamp(cp, to_c_edit_position(cp, cp_edpos, S_maxamp, 3)))); break;
	    case CP_MAXAMP_POSITION:  return(C_TO_XEN_INT64_T(channel_maxamp_position(cp, AT_CURRENT_EDIT_POSITION))); break;
	    case CP_EDPOS_MAXAMP_POSITION: return(C_TO_XEN_INT64_T(channel_maxamp_position(cp, to_c_edit_position(cp, cp_edpos, S_maxamp_position, 3)))); break;
	    }
	}
    }
  return(XEN_FALSE);
}


static int g_imin(int mn, XEN val, int def)
{
  int nval;
  nval = XEN_TO_C_INT_OR_ELSE(val, def);
  if (nval >= mn) return(nval);
  return(mn);
}


static int g_omin(mus_long_t mn, XEN val, mus_long_t def)
{
  mus_long_t nval;
  nval = XEN_TO_C_INT64_T_OR_ELSE(val, def);
  if (nval >= mn) return(nval);
  return(mn);
}


static void reset_y_display(chan_info *cp, double sy, double zy)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sy = sy;
  ap->zy = zy;
  resize_sy(cp);
  apply_y_axis_change(ap, cp);
}


static void chans_x_axis_style(chan_info *cp, int value);

static bool call_update_graph = true;
#define MAX_SPECTRO_SCALE 1000.0
#define MAX_SPECTRO_ANGLE 360.0
#define MIN_SPECTRO_ANGLE -360.0


static XEN channel_set(XEN snd, XEN chn_n, XEN on, cp_field_t fld, const char *caller)
{
  chan_info *cp;
  int val = 0;
  bool bval = false;
  snd_info *sp;
  int i;
  mus_long_t curlen = 0, newlen = 0;
  char *error = NULL;
  mus_float_t curamp;
  mus_float_t newamp[1];
  XEN res = XEN_EMPTY_LIST, errstr;
  if (XEN_TRUE_P(snd))
    {
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res = XEN_CONS(channel_set(C_TO_XEN_INT(i), chn_n, on, fld, caller), res);
	}
      return(res);
    }
  if (XEN_TRUE_P(chn_n))
    {
      sp = get_sp(snd);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd));
      for (i = sp->nchans - 1; i >= 0; i--)
	res = XEN_CONS(channel_set(snd, C_TO_XEN_INT(i), on, fld, caller), res);
      return(res);
    }
  ASSERT_CHANNEL(caller, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, caller);
  if (!cp) return(XEN_FALSE);

  switch (fld)
    {
    case CP_EDIT_CTR:
      if (cp->editable)
	{
	  val = XEN_TO_C_INT_OR_ELSE(on, 0);
	  if (cp->edit_ctr < val)
	    redo_edit(cp, val - cp->edit_ctr);
	  else undo_edit(cp, cp->edit_ctr - val);
	}
      return(C_TO_XEN_INT(cp->edit_ctr));
      break;

    case CP_GRAPH_TRANSFORM_P:
      bval = XEN_TO_C_BOOLEAN(on); 
      fftb(cp, bval);
      update_graph(cp);
      return(on);
      break;

    case CP_GRAPH_TIME_P:
      bval = XEN_TO_C_BOOLEAN(on);
      waveb(cp, bval);
      update_graph(cp);
      return(on);
      break;

    case CP_CURSOR:
#if USE_CAIRO
      if (cp->cgx->ax->cr == NULL)
	{
	  if (cp->cgx->ax->wn)
	    cp->cgx->ax->cr = gdk_cairo_create(cp->cgx->ax->wn);
	}
#endif
      cp->cursor_on = true; 
      cursor_moveto(cp, beg_to_sample(on, caller));
      return(C_TO_XEN_INT64_T(CURSOR(cp)));
      break;

    case CP_EDPOS_CURSOR:
      {
	int pos;
	mus_long_t cpos;
	pos = to_c_edit_position(cp, cp_edpos, caller, 3);
	cpos = beg_to_sample(on, caller);
	if (pos == cp->edit_ctr)
	  {
	    cp->cursor_on = true; 
	    cursor_moveto(cp, cpos);
	  }
	else cp->edits[pos]->cursor = cpos;
	return(C_TO_XEN_INT64_T(cpos));
      }
      break;

    case CP_GRAPH_LISP_P:
      cp->graph_lisp_p = XEN_TO_C_BOOLEAN(on); 
      update_graph(cp);
      return(on);
      break;

    case CP_AP_LOSAMP:
      set_x_axis_x0(cp, beg_to_sample(on, caller));
      return(on);
      break;

    case CP_AP_HISAMP:
      set_x_axis_x1(cp, beg_to_sample(on, caller));
      return(on);
      break;

    case CP_SQUELCH_UPDATE:
      cp->squelch_update = XEN_TO_C_BOOLEAN(on);
      if (!(cp->squelch_update))
	{
	  /* make sure everything that was squelched before is now correct */
	  reflect_edit_history_change(cp);
	  if (cp->edit_ctr == 0)
	    reflect_file_revert_in_label(cp->sound);
	  else reflect_file_change_in_label(cp);
	  clear_minibuffer(cp->sound);
	}
      break;

    case CP_CURSOR_SIZE:
      cp->cursor_size = XEN_TO_C_INT_OR_ELSE(on, DEFAULT_CURSOR_SIZE);
      update_graph(cp); 
      return(C_TO_XEN_INT(cp->cursor_size));
      break;

    case CP_CURSOR_STYLE:
      if (XEN_PROCEDURE_P(on))
	{
	  error = procedure_ok(on, 3, S_setB S_cursor_style, "", 1);
	  if (error == NULL)
	    {
	      if ((cp->cursor_style == CURSOR_PROC) &&
		  (XEN_PROCEDURE_P(cp->cursor_proc)))
		snd_unprotect_at(cp->cursor_proc_loc);
	      cp->cursor_proc_loc = snd_protect(on);
	      cp->cursor_proc = on;
	      cp->cursor_style = CURSOR_PROC;
	      return(on);
	    }
	  else 
	    {
	      errstr = C_TO_XEN_STRING(error);
	      free(error);
	      return(snd_bad_arity_error(S_setB S_cursor_style, errstr, on));
	    }
	}
      else
	{
	  if ((cp->cursor_style == CURSOR_PROC) &&
	      (XEN_PROCEDURE_P(cp->cursor_proc)))
	    {
	      snd_unprotect_at(cp->cursor_proc_loc);
	      cp->cursor_proc = XEN_UNDEFINED;
	      cp->cursor_proc_loc = NOT_A_GC_LOC;
	    }
	  cp->cursor_style = (cursor_style_t)XEN_TO_C_INT(on); /* range already checked */
	}
      cp->just_zero = (cp->cursor_style == CURSOR_LINE); /* no point in displaying y value in this case */
      update_graph(cp); 
      return(C_TO_XEN_INT((int)(cp->cursor_style)));
      break;

    case CP_TRACKING_CURSOR_STYLE:
      cp->tracking_cursor_style = (cursor_style_t)XEN_TO_C_INT(on);
      return(C_TO_XEN_INT((int)(cp->tracking_cursor_style)));
      break;

    case CP_SHOW_Y_ZERO:
      cp->show_y_zero = XEN_TO_C_BOOLEAN(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_y_zero));
      break;

    case CP_SHOW_GRID:
      cp->show_grid = (with_grid_t)(XEN_TO_C_BOOLEAN(on)); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN((bool)(cp->show_grid)));
      break;

    case CP_GRID_DENSITY:
      cp->grid_density = XEN_TO_C_DOUBLE(on); 
      update_graph(cp); 
      return(C_TO_XEN_DOUBLE(cp->grid_density));
      break;

    case CP_SHOW_SONOGRAM_CURSOR:
      cp->show_sonogram_cursor = XEN_TO_C_BOOLEAN(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_sonogram_cursor));
      break;

    case CP_SHOW_MARKS:
      cp->show_marks = XEN_TO_C_BOOLEAN(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_marks));
      break;

    case CP_TIME_GRAPH_TYPE:
      cp->time_graph_type = (graph_type_t)XEN_TO_C_INT(on); /* checked already */
      update_graph(cp); 
      return(C_TO_XEN_INT((int)(cp->time_graph_type)));
      break;

    case CP_WAVO_HOP:
      cp->wavo_hop = g_imin(1, on, DEFAULT_WAVO_HOP); 
      update_graph(cp); 
      return(C_TO_XEN_INT(cp->wavo_hop));
      break;

    case CP_WAVO_TRACE:
      cp->wavo_trace = mus_iclamp(1, g_imin(1, on, DEFAULT_WAVO_TRACE), POINT_BUFFER_SIZE);
      update_graph(cp); 
      return(C_TO_XEN_INT(cp->wavo_trace));
      break;

    case CP_MAX_TRANSFORM_PEAKS:
      cp->max_transform_peaks = g_imin(1, on, DEFAULT_MAX_TRANSFORM_PEAKS); 
      return(C_TO_XEN_INT(cp->max_transform_peaks));
      break;

    case CP_ZERO_PAD:
      cp->zero_pad = mus_iclamp(0, g_imin(0, on, DEFAULT_ZERO_PAD), MAX_ZERO_PAD); 
      update_graph(cp);
      return(C_TO_XEN_INT(cp->zero_pad));
      break;

    case CP_WAVELET_TYPE:
      cp->wavelet_type = XEN_TO_C_INT(on); /* range checked already */
      update_graph(cp);
      return(C_TO_XEN_INT(cp->wavelet_type));
      break;

    case CP_SHOW_TRANSFORM_PEAKS:
      cp->show_transform_peaks = XEN_TO_C_BOOLEAN(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_transform_peaks));
      break;

    case CP_VERBOSE_CURSOR:
      cp->verbose_cursor = XEN_TO_C_BOOLEAN(on); 
      return(C_TO_XEN_BOOLEAN(cp->verbose_cursor));
      break;

    case CP_FFT_LOG_FREQUENCY:
      cp->fft_log_frequency = XEN_TO_C_BOOLEAN(on); 
      if (cp->graph_transform_p) calculate_fft(cp); 
      return(C_TO_XEN_BOOLEAN(cp->fft_log_frequency));
      break;

    case CP_FFT_LOG_MAGNITUDE:
      cp->fft_log_magnitude = XEN_TO_C_BOOLEAN(on); 
      if (cp->graph_transform_p) calculate_fft(cp); 
      return(C_TO_XEN_BOOLEAN(cp->fft_log_magnitude));
      break;

    case CP_FFT_WITH_PHASES:
      cp->fft_with_phases = XEN_TO_C_BOOLEAN(on); 
      if (cp->graph_transform_p) calculate_fft(cp); 
      return(C_TO_XEN_BOOLEAN(cp->fft_with_phases));
      break;

    case CP_SPECTRO_HOP:
      cp->spectro_hop = g_imin(1, on, DEFAULT_SPECTRO_HOP); 
      if (cp->graph_transform_p) calculate_fft(cp); 
      return(C_TO_XEN_INT(cp->spectro_hop));
      break;

    case CP_TRANSFORM_SIZE:
      cp->transform_size = g_omin(1, on, DEFAULT_TRANSFORM_SIZE); 
      calculate_fft(cp);
      return(C_TO_XEN_INT64_T(cp->transform_size));
      break;

    case CP_TRANSFORM_GRAPH_TYPE: 
      cp->transform_graph_type = (graph_type_t)XEN_TO_C_INT(on); /* checked already */
      calculate_fft(cp); 
      return(C_TO_XEN_INT((int)(cp->transform_graph_type))); 
      break;

    case CP_FFT_WINDOW:
      cp->fft_window = (mus_fft_window_t)XEN_TO_C_INT(on); /* checked */
      calculate_fft(cp); 
      return(C_TO_XEN_INT((int)(cp->fft_window)));
      break;

    case CP_TRANSFORM_TYPE:
      cp->transform_type = XEN_TO_C_INT(on); /* range already checked */
      calculate_fft(cp); 
      return(C_TO_XEN_INT(cp->transform_type));
      break;

    case CP_TRANSFORM_NORMALIZATION:      
      cp->transform_normalization = (fft_normalize_t)XEN_TO_C_INT(on); /* range already checked */
      calculate_fft(cp); 
      return(C_TO_XEN_INT((int)(cp->transform_normalization)));
      break;

    case CP_SHOW_MIX_WAVEFORMS: 
      cp->show_mix_waveforms = XEN_TO_C_BOOLEAN(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_mix_waveforms));
      break;

    case CP_TIME_GRAPH_STYLE:
      cp->time_graph_style = (graph_style_t)XEN_TO_C_INT_OR_ELSE(on, (int)DEFAULT_GRAPH_STYLE);
      if (call_update_graph) update_graph(cp);
      return(C_TO_XEN_INT((int)(cp->time_graph_style)));
      break;

    case CP_LISP_GRAPH_STYLE:
      cp->lisp_graph_style = (graph_style_t)XEN_TO_C_INT_OR_ELSE(on, (int)DEFAULT_GRAPH_STYLE);
      if (call_update_graph) update_graph(cp);
      return(C_TO_XEN_INT((int)(cp->lisp_graph_style)));
      break;

    case CP_TRANSFORM_GRAPH_STYLE:
      cp->transform_graph_style = (graph_style_t)XEN_TO_C_INT_OR_ELSE(on, (int)DEFAULT_GRAPH_STYLE);
      if (call_update_graph) update_graph(cp);
      return(C_TO_XEN_INT((int)(cp->transform_graph_style)));
      break;

    case CP_X_AXIS_STYLE:
      val = XEN_TO_C_INT(on); /* range already checked */
      chans_x_axis_style(cp, val);
      return(C_TO_XEN_INT((int)(cp->x_axis_style)));
      break;

    case CP_DOT_SIZE:
      cp->dot_size = mus_iclamp(MIN_DOT_SIZE, 
				XEN_TO_C_INT_OR_ELSE(on, DEFAULT_DOT_SIZE), 
				MAX_DOT_SIZE); /* size > 17000 -> X segfault! */
      update_graph(cp);
      return(C_TO_XEN_INT(cp->dot_size));
      break;

    case CP_SHOW_AXES:
      cp->show_axes = (show_axes_t)XEN_TO_C_INT(on); /* range checked already */
      update_graph(cp); 
      return(C_TO_XEN_INT((int)(cp->show_axes)));
      break;

    case CP_GRAPHS_HORIZONTAL:
      cp->graphs_horizontal = XEN_TO_C_BOOLEAN(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->graphs_horizontal));
      break;

    case CP_FRAMES:
      if (cp->editable)
	{
	  bool need_update = true;
	  /* if less than current, delete, else zero pad */
	  curlen = CURRENT_SAMPLES(cp);
	  newlen = XEN_TO_C_INT64_T_OR_ELSE(on, curlen);
	  if (newlen < 0)
	    XEN_OUT_OF_RANGE_ERROR(S_setB S_frames, 1, on, "frames (~A) < 0?");
	  if (curlen > newlen)
	    {
	      if (newlen > 0)
		need_update = delete_samples(newlen - 1, curlen - newlen, cp, cp->edit_ctr);
	      else need_update = delete_samples(0, curlen, cp, cp->edit_ctr);
	    }
	  else
	    {
	      if (newlen > curlen)
		extend_with_zeros(cp, curlen, newlen - curlen, cp->edit_ctr, S_setB S_frames);
	    }
	  if (need_update)
	    update_graph(cp);
	}
      break;

    case CP_PROPERTIES:
      if (!(XEN_VECTOR_P(cp->properties)))
	{
	  cp->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
	  cp->properties_loc = snd_protect(cp->properties);
	}
      XEN_VECTOR_SET(cp->properties, 0, on);
      return(XEN_VECTOR_REF(cp->properties, 0));
      break;

    case CP_AP_SX:
      reset_x_display(cp, mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), 1.0), cp->axis->zx);
      break;

    case CP_AP_ZX:
      reset_x_display(cp, cp->axis->sx, mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), 1.0));
      break;

    case CP_AP_SY:
      reset_y_display(cp, mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), 1.0), cp->axis->zy);
      break;

    case CP_AP_ZY:
      reset_y_display(cp, cp->axis->sy, mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), 1.0)); 
      break;

    case CP_MIN_DB:
      curamp = XEN_TO_C_DOUBLE(on); 
      if (curamp < 0.0)
	{
	  cp->min_dB = curamp;
	  cp->lin_dB = pow(10.0, cp->min_dB * 0.05); 
	  calculate_fft(cp); 
	}
      else XEN_OUT_OF_RANGE_ERROR(caller, 1, on, S_min_dB " (~A) must be < 0.0");
      break;

    case CP_SPECTRO_X_ANGLE: cp->spectro_x_angle = mus_fclamp(MIN_SPECTRO_ANGLE, XEN_TO_C_DOUBLE(on), MAX_SPECTRO_ANGLE); calculate_fft(cp); break;
    case CP_SPECTRO_Y_ANGLE: cp->spectro_y_angle = mus_fclamp(MIN_SPECTRO_ANGLE, XEN_TO_C_DOUBLE(on), MAX_SPECTRO_ANGLE); calculate_fft(cp); break;
    case CP_SPECTRO_Z_ANGLE: cp->spectro_z_angle = mus_fclamp(MIN_SPECTRO_ANGLE, XEN_TO_C_DOUBLE(on), MAX_SPECTRO_ANGLE); calculate_fft(cp); break;

    case CP_SPECTRO_X_SCALE: cp->spectro_x_scale = mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), MAX_SPECTRO_SCALE); calculate_fft(cp); break;
    case CP_SPECTRO_Y_SCALE: cp->spectro_y_scale = mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), MAX_SPECTRO_SCALE); calculate_fft(cp); break;
    case CP_SPECTRO_Z_SCALE: cp->spectro_z_scale = mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), MAX_SPECTRO_SCALE); calculate_fft(cp); break;

    case CP_SPECTRUM_END:  
      cp->spectrum_end = XEN_TO_C_DOUBLE(on); /* range checked already */
      calculate_fft(cp); 
      return(C_TO_XEN_DOUBLE(cp->spectrum_end)); 
      break;

    case CP_SPECTRUM_START:   
      cp->spectrum_start = XEN_TO_C_DOUBLE(on); /* range checked already */
      calculate_fft(cp); 
      return(C_TO_XEN_DOUBLE(cp->spectrum_start));   
      break;

    case CP_FFT_WINDOW_ALPHA:        
      cp->fft_window_alpha = XEN_TO_C_DOUBLE(on);
      calculate_fft(cp); 
      return(C_TO_XEN_DOUBLE(cp->fft_window_alpha));             
      break;

    case CP_FFT_WINDOW_BETA:        
      cp->fft_window_beta = XEN_TO_C_DOUBLE(on); /* range checked already */
      calculate_fft(cp); 
      return(C_TO_XEN_DOUBLE(cp->fft_window_beta));             
      break;

    case CP_MAXAMP:
      if (cp->editable)
	{
	  curamp = channel_maxamp(cp, AT_CURRENT_EDIT_POSITION);
	  newamp[0] = XEN_TO_C_DOUBLE(on);
	  if (curamp != newamp[0])
	    {
	      if (scale_to(cp->sound, cp, newamp, 1, false))
		update_graph(cp);
	    }
	}
      break;

    case CP_BEATS_PER_MINUTE:
      curamp = XEN_TO_C_DOUBLE(on);
      if (curamp > 0.0)
	{
	  cp->beats_per_minute = curamp;
	  update_graph(cp);
	}
      break;

    case CP_BEATS_PER_MEASURE:
      cp->beats_per_measure = XEN_TO_C_INT(on);
      update_graph(cp);
      return(on);
      break;
    default:
      break;
    }
  return(C_TO_XEN_BOOLEAN(val));
}


static XEN g_update_time_graph(XEN snd, XEN chn) 
{
  #define H_update_time_graph "(" S_update_time_graph " :optional snd chn): redraw snd channel chn's graphs"
  return(channel_get(snd, chn, CP_UPDATE_TIME, S_update_time_graph));
}


static XEN g_update_transform_graph(XEN snd, XEN chn) 
{
  #define H_update_transform_graph "(" S_update_transform_graph " :optional snd chn): redraw snd channel chn's fft display"
  return(channel_get(snd, chn, CP_UPDATE_TRANSFORM_GRAPH, S_update_transform_graph));
}


static XEN g_update_lisp_graph(XEN snd, XEN chn) 
{
  #define H_update_lisp_graph "(" S_update_lisp_graph " :optional snd chn): redraw snd channel chn's lisp graph"
  return(channel_get(snd, chn, CP_UPDATE_LISP, S_update_lisp_graph));
}


static XEN g_edit_position(XEN snd, XEN chn_n) 
{
  #define H_edit_position "(" S_edit_position " :optional snd chn): current edit history position in snd's channel chn"
  return(channel_get(snd, chn_n, CP_EDIT_CTR, S_edit_position));
}

static XEN g_set_edit_position(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, S_setB S_edit_position, "an integer");
  return(channel_set(snd, chn_n, on, CP_EDIT_CTR, S_setB S_edit_position));
}

WITH_THREE_SETTER_ARGS(g_set_edit_position_reversed, g_set_edit_position)



static XEN g_transform_graph_p(XEN snd, XEN chn_n) 
{
  #define H_transform_graph_p "(" S_transform_graph_p " :optional snd chn): " PROC_TRUE " if fft display is active in snd's channel chn"
  return(channel_get(snd, chn_n, CP_GRAPH_TRANSFORM_P, S_transform_graph_p));
}

static XEN g_set_transform_graph_p(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_transform_graph_p, "a boolean");
  return(channel_set(snd, chn_n, on, CP_GRAPH_TRANSFORM_P, S_setB S_transform_graph_p));
}

WITH_THREE_SETTER_ARGS(g_set_transform_graph_p_reversed, g_set_transform_graph_p)



static XEN g_time_graph_p(XEN snd, XEN chn_n) 
{
  #define H_time_graph_p "(" S_time_graph_p " :optional snd chn): " PROC_TRUE " if time domain display is active in snd's channel chn"
  return(channel_get(snd, chn_n, CP_GRAPH_TIME_P, S_time_graph_p));
}

static XEN g_set_time_graph_p(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_time_graph_p, "a boolean");
  return(channel_set(snd, chn_n, on, CP_GRAPH_TIME_P, S_setB S_time_graph_p));
}

WITH_THREE_SETTER_ARGS(g_set_time_graph_p_reversed, g_set_time_graph_p)



static XEN g_lisp_graph_p(XEN snd, XEN chn_n) 
{
  #define H_lisp_graph_p "(" S_lisp_graph_p " :optional snd chn): " PROC_TRUE " if lisp-generated data display is active in snd's channel chn"
  return(channel_get(snd, chn_n, CP_GRAPH_LISP_P, S_lisp_graph_p));
}

static XEN g_set_lisp_graph_p(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_lisp_graph_p, "a boolean");
  return(channel_set(snd, chn_n, on, CP_GRAPH_LISP_P, S_setB S_lisp_graph_p));
}

WITH_THREE_SETTER_ARGS(g_set_lisp_graph_p_reversed, g_set_lisp_graph_p)



static XEN g_cursor(XEN snd, XEN chn_n, XEN edpos) 
{
  #define H_cursor "(" S_cursor " :optional snd chn edpos): current cursor location in snd's channel chn"
  if (XEN_BOUND_P(edpos))
    {
      XEN res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd, chn_n, CP_EDPOS_CURSOR, S_cursor);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }
  return(channel_get(snd, chn_n, CP_CURSOR, S_cursor));
}

static XEN g_set_cursor(XEN on, XEN snd, XEN chn_n, XEN edpos) 
{
  XEN_ASSERT_TYPE(XEN_INT64_T_P(on) || XEN_NOT_BOUND_P(on), on, XEN_ARG_1, S_setB S_cursor, "an integer");
  if (XEN_BOUND_P(edpos))
    {
      XEN res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_set(snd, chn_n, on, CP_EDPOS_CURSOR, S_setB S_cursor);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }
  return(channel_set(snd, chn_n, on, CP_CURSOR, S_setB S_cursor));
}

WITH_FOUR_SETTER_ARGS(g_set_cursor_reversed, g_set_cursor)



static XEN g_cursor_style(XEN snd, XEN chn_n) 
{
  #define H_cursor_style "(" S_cursor_style " :optional snd chn): current cursor style in snd's channel chn. \
Possible values are " S_cursor_cross " (default), " S_cursor_line " (a vertical line), or a procedure of three arguments, the \
sound index, channel number, and graph (always " S_time_graph ").  The procedure \
should draw the cursor at the current cursor position using the " S_cursor_context "."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn_n, CP_CURSOR_STYLE, S_cursor_style));
  if (cursor_style(ss) == CURSOR_PROC)
    return(ss->cursor_proc);
  return(C_TO_XEN_INT((int)(cursor_style(ss))));
}

static XEN g_set_cursor_style(XEN on, XEN snd, XEN chn_n) 
{
  cursor_style_t val = CURSOR_PROC;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on) || XEN_PROCEDURE_P(on), on, XEN_ARG_1, S_setB S_cursor_style, "an integer or a function");
  if (XEN_INTEGER_P(on))
    {
      int choice;
      choice = XEN_TO_C_INT(on);
      if (choice < 0)
	XEN_OUT_OF_RANGE_ERROR(S_setB S_cursor_style, 1, on, "~A, but must be >= 0");
      val = (cursor_style_t)choice;
      if (val > CURSOR_LINE)
	XEN_OUT_OF_RANGE_ERROR(S_setB S_cursor_style, 1, on, "~A, but must be " S_cursor_cross " or " S_cursor_line ", or a procedure");
    }
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn_n, on, CP_CURSOR_STYLE, S_setB S_cursor_style));
  else
    {
      if (XEN_PROCEDURE_P(on))
	{
	  char *error;
	  XEN errstr;
	  error = procedure_ok(on, 3, S_setB S_cursor_style, "", 1);
	  if (error == NULL)
	    {	  
	      if ((cursor_style(ss) == CURSOR_PROC) && (XEN_PROCEDURE_P(ss->cursor_proc)))
		snd_unprotect_at(ss->cursor_proc_loc);
	      ss->cursor_proc_loc = snd_protect(on);
	      ss->cursor_proc = on;
	      in_set_cursor_style(CURSOR_PROC);
	    }
	  else 
	    {
	      errstr = C_TO_XEN_STRING(error);
	      free(error);
	      return(snd_bad_arity_error(S_setB S_cursor_style, errstr, on));
	    }
	}
      set_cursor_style(val);
      return(on);
    }
}

WITH_THREE_SETTER_ARGS(g_set_cursor_style_reversed, g_set_cursor_style)



static XEN g_tracking_cursor_style(XEN snd, XEN chn_n) 
{
  #define H_tracking_cursor_style "(" S_tracking_cursor_style " :optional snd chn): current tracking cursor style in snd's channel chn. \
Possible values are " S_cursor_cross " (default), and " S_cursor_line "."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn_n, CP_TRACKING_CURSOR_STYLE, S_tracking_cursor_style));
  return(C_TO_XEN_INT((int)(tracking_cursor_style(ss))));
}

static XEN g_set_tracking_cursor_style(XEN on, XEN snd, XEN chn_n) 
{
  int in_val;
  cursor_style_t val = CURSOR_CROSS;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, S_setB S_tracking_cursor_style, "an integer");
  in_val = XEN_TO_C_INT(on);
  if (in_val < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_tracking_cursor_style, 1, on, "~A, but must be >= 0");
  val = (cursor_style_t)in_val;
  if (val > CURSOR_LINE)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_tracking_cursor_style, 1, on, "~A, but must be " S_cursor_cross " or " S_cursor_line);
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn_n, on, CP_TRACKING_CURSOR_STYLE, S_setB S_tracking_cursor_style));
  else set_tracking_cursor_style(val);
  return(on);
}

WITH_THREE_SETTER_ARGS(g_set_tracking_cursor_style_reversed, g_set_tracking_cursor_style)



static XEN g_cursor_size(XEN snd, XEN chn_n) 
{
  #define H_cursor_size "(" S_cursor_size " :optional snd chn): current cursor size in snd's channel chn"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn_n, CP_CURSOR_SIZE, S_cursor_size));
  return(C_TO_XEN_INT(cursor_size(ss)));
}

static XEN g_set_cursor_size(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, S_setB S_cursor_size, "an integer");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn_n, on, CP_CURSOR_SIZE, S_setB S_cursor_size));
  set_cursor_size(XEN_TO_C_INT_OR_ELSE(on, DEFAULT_CURSOR_SIZE));
  return(C_TO_XEN_INT(cursor_size(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_cursor_size_reversed, g_set_cursor_size)



static XEN g_cursor_position(XEN snd, XEN chn)
{
  #define H_cursor_position "(" S_cursor_position " :optional snd chn): current cursor position (x y in pixels) in snd's channel chn"
  return(channel_get(snd, chn, CP_CURSOR_POSITION, S_cursor_position));
}


XEN g_frames(XEN snd, XEN chn, XEN edpos)
{
  #define H_frames "(" S_frames " :optional snd-or-object chn edpos): number of frames of data in the given object or channel"

  if (!(XEN_BOUND_P(chn)))
    {
      if ((XEN_SOUND_P(snd)) || (!XEN_BOUND_P(snd)))
	return(channel_get(snd, chn, CP_FRAMES, S_frames));

      if (XEN_STRING_P(snd))
	return(g_mus_sound_frames(snd));         /* mus-sound-frames */
      
      if (mus_xen_p(snd))                        /* mus-length */
	return(g_mus_length(snd));

      if (sound_data_p(snd))                     /* sound-data-length */
	return(C_TO_XEN_INT64_T((XEN_TO_SOUND_DATA(snd))->length));

      if (MUS_VCT_P(snd))                        /* vct-length */
	return(C_TO_XEN_INT64_T((XEN_TO_VCT(snd))->length));

      if (XEN_MIX_P(snd))                        /* mix-length */
	return(g_mix_length(snd));

      if (XEN_REGION_P(snd))                     /* region-frames */
	return(g_region_frames(snd, C_TO_XEN_INT(0)));

      if (XEN_PLAYER_P(snd))
	{
	  snd_info *sp;
	  sp = get_player_sound(snd);
	  return(C_TO_XEN_INT64_T(CURRENT_SAMPLES(sp->chans[0])));
	}
    }

  if (XEN_SELECTION_P(snd))
    return(g_selection_frames(chn, edpos));

  if (XEN_BOUND_P(edpos))
    {
      XEN res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd, chn, CP_EDPOS_FRAMES, S_frames);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }

  return(channel_get(snd, chn, CP_FRAMES, S_frames));
}


static XEN g_set_frames(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_frames, "a number");
  return(channel_set(snd, chn_n, on, CP_FRAMES, S_setB S_frames));
}

WITH_THREE_SETTER_ARGS(g_set_frames_reversed, g_set_frames)


static XEN g_vector_maxamp(XEN obj)
{
  double mx = 0.0;
  int i, len;
  len = XEN_VECTOR_LENGTH(obj);
  for (i = 0; i < len; i++)
    {
      XEN el;
      el = XEN_VECTOR_REF(obj, i);
      if (XEN_NUMBER_P(el))
	{
	  double temp;
	  temp = fabs(XEN_TO_C_DOUBLE(el));
	  if (temp > mx) mx = temp;
	}
      /* should we trigger an error here? */
    }
  return(C_TO_XEN_DOUBLE(mx));
}


static XEN g_list_maxamp(XEN obj)
{
  double mx = 0.0;
  int i, len;
  len = XEN_LIST_LENGTH(obj);
  for (i = 0; i < len; i++)
    {
      XEN el;
      el = XEN_LIST_REF(obj, i);
      if (XEN_NUMBER_P(el))
	{
	  double temp;
	  temp = fabs(XEN_TO_C_DOUBLE(el));
	  if (temp > mx) mx = temp;
	}
    }
  return(C_TO_XEN_DOUBLE(mx));
}


static XEN g_maxamp(XEN snd, XEN chn_n, XEN edpos) 
{
  #define H_maxamp "(" S_maxamp " :optional snd chn edpos): maxamp of data in the object 'snd', or in snd's channel chn if snd is a sound"

  if (!(XEN_BOUND_P(chn_n)))
    {
      if (XEN_STRING_P(snd))                     /* string => mus_sound_maxamp */
	return(XEN_CADR(g_mus_sound_maxamp(snd)));

      if (mus_xen_p(snd))                        /* maxamp of mus-data */
	{
	  XEN v;
	  v = g_mus_data(snd);
	  if (MUS_VCT_P(v))
	    return(g_vct_peak(v));
	}

      if (sound_data_p(snd))
	return(g_list_maxamp(g_sound_data_maxamp(snd)));

      if (MUS_VCT_P(snd))                        /* vct-peak */
	return(g_vct_peak(snd));

      if (XEN_MIX_P(snd))                        /* mix => sound maxamp of the mix data */
	return(g_mix_maxamp(snd));

      if (XEN_REGION_P(snd))                     /* region-maxamp */
	return(g_region_maxamp(snd));

      if (XEN_VECTOR_P(snd))
	return(g_vector_maxamp(snd));

      if (XEN_LIST_P(snd)) /* can this happen given def-clm-struct? */
	return(g_list_maxamp(snd));
    }

  if (XEN_SELECTION_P(snd))                      /* selection-maxamp where chn=snd and edpos=chn */
    return(g_selection_maxamp(chn_n, edpos));

  if (XEN_BOUND_P(edpos))
    {
      XEN res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd, chn_n, CP_EDPOS_MAXAMP, S_maxamp);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }
  
  if ((XEN_BOUND_P(chn_n)) ||
      (XEN_TRUE_P(snd)))
    return(channel_get(snd, chn_n, CP_MAXAMP, S_maxamp));

  ASSERT_SOUND(S_maxamp, snd, 0);
  {
    snd_info *sp;
    sp = get_sp(snd);
    if (sp)
      {
	int i;
	mus_float_t mx = 0.0;
	for (i = 0; i < sp->nchans; i++)
	  {
	    mus_float_t cur_mx;
	    cur_mx = channel_maxamp(sp->chans[i], sp->chans[i]->edit_ctr);
	    if (cur_mx > mx)
	      mx = cur_mx;
	  }
	return(C_TO_XEN_DOUBLE(mx));
      }
    else snd_no_such_sound_error(S_maxamp, snd); 
  }
  return(XEN_ZERO);
}

static XEN g_set_maxamp(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_maxamp, "a number");
  return(channel_set(snd, chn_n, on, CP_MAXAMP, S_setB S_maxamp));
}

WITH_THREE_SETTER_ARGS(g_set_maxamp_reversed, g_set_maxamp)



static XEN g_maxamp_position(XEN snd, XEN chn_n, XEN edpos) 
{
  #define H_maxamp_position "(" S_maxamp_position " :optional snd chn edpos): location of maxamp of data in snd's channel chn"
  if (XEN_BOUND_P(edpos))
    {
      XEN res;
      if (cp_edpos_loc != NOT_A_GC_LOC)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd, chn_n, CP_EDPOS_MAXAMP_POSITION, S_maxamp_position);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = NOT_A_GC_LOC;
      return(res);
    }
  return(channel_get(snd, chn_n, CP_MAXAMP_POSITION, S_maxamp_position));
}


static XEN g_squelch_update(XEN snd, XEN chn_n) 
{
  #define H_squelch_update "(" S_squelch_update " :optional snd chn): " PROC_TRUE " if updates (redisplays) are turned off in snd's channel chn"
  return(channel_get(snd, chn_n, CP_SQUELCH_UPDATE, S_squelch_update));
}

static XEN g_set_squelch_update(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_squelch_update, "a boolean");
  return(channel_set(snd, chn_n, on, CP_SQUELCH_UPDATE, S_setB S_squelch_update));
}

WITH_THREE_SETTER_ARGS(g_set_squelch_update_reversed, g_set_squelch_update)



static XEN g_ap_sx(XEN snd, XEN chn_n) 
{
  #define H_x_position_slider "(" S_x_position_slider " :optional snd chn): current x axis position slider of snd channel chn"
  return(channel_get(snd, chn_n, CP_AP_SX, S_x_position_slider));
}

static XEN g_set_ap_sx(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_x_position_slider, "a number");
  return(channel_set(snd, chn_n, on, CP_AP_SX, S_setB S_x_position_slider));
}

WITH_THREE_SETTER_ARGS(g_set_ap_sx_reversed, g_set_ap_sx)



static XEN g_ap_sy(XEN snd, XEN chn_n) 
{
  #define H_y_position_slider "(" S_y_position_slider " :optional snd chn): current y axis position slider of snd channel chn"
  return(channel_get(snd, chn_n, CP_AP_SY, S_y_position_slider));
}

static XEN g_set_ap_sy(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_y_position_slider, "a number");
  return(channel_set(snd, chn_n, on, CP_AP_SY, S_setB S_y_position_slider));
}

WITH_THREE_SETTER_ARGS(g_set_ap_sy_reversed, g_set_ap_sy)



static XEN g_ap_zx(XEN snd, XEN chn_n) 
{
  #define H_x_zoom_slider "(" S_x_zoom_slider " :optional snd chn): current x axis zoom slider of snd channel chn"
  return(channel_get(snd, chn_n, CP_AP_ZX, S_x_zoom_slider));
}

static XEN g_set_ap_zx(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_x_zoom_slider, "a number");
  return(channel_set(snd, chn_n, on, CP_AP_ZX, S_setB S_x_zoom_slider));
}

WITH_THREE_SETTER_ARGS(g_set_ap_zx_reversed, g_set_ap_zx)



static XEN g_ap_zy(XEN snd, XEN chn_n) 
{
  #define H_y_zoom_slider "(" S_y_zoom_slider " :optional snd chn): current y axis zoom slider of snd channel chn"
  return(channel_get(snd, chn_n, CP_AP_ZY, S_y_zoom_slider));
}

static XEN g_set_ap_zy(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_y_zoom_slider, "a number");
  return(channel_set(snd, chn_n, on, CP_AP_ZY, S_setB S_y_zoom_slider));
}

WITH_THREE_SETTER_ARGS(g_set_ap_zy_reversed, g_set_ap_zy)



static XEN g_edit_hook(XEN snd, XEN chn_n) 
{
  #if HAVE_SCHEME
    #define edit_hook_example "(add-hook! (" S_edit_hook " snd chn) (lambda () (" S_snd_print " \";about to edit\") #f))"
  #endif
  #if HAVE_RUBY
    #define edit_hook_example "edit_hook(snd, chn).add_hook!(\"hook-test\") do | | snd_print(\"about to edit\"); false end"
  #endif
  #if HAVE_FORTH
    #define edit_hook_example "snd chn edit-hook lambda: <{ }> \"about to edit\" snd-print drop #f ; add-hook!"
  #endif

  #define H_edit_hook "(" S_edit_hook " :optional snd chn): snd's channel chn's " S_edit_hook ". \
This is a channel-specific hook variable; the hook procedures are thunks -- they should take no \
arguments. " S_edit_hook " is called just before any attempt to edit the channel's data; if it returns " PROC_TRUE ", \
the edit is aborted. \n  " edit_hook_example

  return(channel_get(snd, chn_n, CP_EDIT_HOOK, S_edit_hook));
}


static XEN g_after_edit_hook(XEN snd, XEN chn_n) 
{
  #if HAVE_SCHEME
    #define after_edit_hook_example "(add-hook! (" S_after_edit_hook " snd chn) (lambda () (" S_snd_print " \";just edited\")))"
  #endif
  #if HAVE_RUBY
    #define after_edit_hook_example "after_edit_hook(snd, chn).add_hook!(\"hook-test\") do | | snd_print(\"just edited\") end"
  #endif
  #if HAVE_FORTH
    #define after_edit_hook_example "snd chn after-edit-hook lambda: <{ }> \"just edited\" snd-print drop ; add-hook!"
  #endif

  #define H_after_edit_hook "(" S_after_edit_hook " :optional snd chn): snd's channel chn's " S_after_edit_hook ". \
This is a channel-specific hook variable; the hook procedures are thunks -- they should take no \
arguments. " S_after_edit_hook " is called after an edit, but before " S_after_graph_hook ". \n  " after_edit_hook_example

  return(channel_get(snd, chn_n, CP_AFTER_EDIT_HOOK, S_after_edit_hook));
}


static XEN g_undo_hook(XEN snd, XEN chn_n) 
{
  #define H_undo_hook "(" S_undo_hook " :optional snd chn): snd's channel chn's " S_undo_hook ". \
This is a channel-specific hook variable; the hook procedures are thunks -- they should take no \
arguments. " S_undo_hook " is called just after any undo, redo, or revert that affects the channel."

  return(channel_get(snd, chn_n, CP_UNDO_HOOK, S_undo_hook));
}


static XEN g_show_y_zero(XEN snd, XEN chn)
{
  #define H_show_y_zero "(" S_show_y_zero " :optional snd chn): " PROC_TRUE " if Snd should include a line at y = 0.0"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_Y_ZERO, S_show_y_zero));
  return(C_TO_XEN_BOOLEAN(show_y_zero(ss)));
}

static void chans_zero(chan_info *cp, bool value)
{
  cp->show_y_zero = value;
  update_graph(cp);
}

void set_show_y_zero(bool val)
{
  in_set_show_y_zero(val);
  for_each_chan_with_bool(chans_zero, val);
}

static XEN g_set_show_y_zero(XEN on, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_show_y_zero, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_Y_ZERO, S_setB S_show_y_zero));
  set_show_y_zero(XEN_TO_C_BOOLEAN(on));
  return(C_TO_XEN_BOOLEAN(show_y_zero(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_show_y_zero_reversed, g_set_show_y_zero)



static XEN g_show_grid(XEN snd, XEN chn)
{
  #define H_show_grid "(" S_show_grid " :optional snd chn): " PROC_TRUE " if Snd should display a background grid in the graphs"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_GRID, S_show_grid));
  return(C_TO_XEN_BOOLEAN((bool)show_grid(ss)));
}

static XEN g_set_show_grid(XEN on, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_show_grid, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_GRID, S_setB S_show_grid));
  set_show_grid((with_grid_t)(XEN_TO_C_BOOLEAN(on)));
  return(C_TO_XEN_BOOLEAN((bool)show_grid(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_show_grid_reversed, g_set_show_grid)



static XEN g_grid_density(XEN snd, XEN chn)
{
  #define H_grid_density "(" S_grid_density " :optional snd chn): sets how closely axis ticks are spaced, default=1.0"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_GRID_DENSITY, S_grid_density));
  return(C_TO_XEN_DOUBLE(grid_density(ss)));
}

static XEN g_set_grid_density(XEN on, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_grid_density, "a number");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_GRID_DENSITY, S_setB S_grid_density));
  set_grid_density(XEN_TO_C_DOUBLE(on));
  return(C_TO_XEN_DOUBLE(grid_density(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_grid_density_reversed, g_set_grid_density)



static XEN g_show_sonogram_cursor(XEN snd, XEN chn)
{
  #define H_show_sonogram_cursor "(" S_show_sonogram_cursor " :optional snd chn): " PROC_TRUE " if Snd should display a cursor in the sonogram"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_SONOGRAM_CURSOR, S_show_sonogram_cursor));
  return(C_TO_XEN_BOOLEAN((bool)show_sonogram_cursor(ss)));
}

static XEN g_set_show_sonogram_cursor(XEN on, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_show_sonogram_cursor, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_SONOGRAM_CURSOR, S_setB S_show_sonogram_cursor));
  set_show_sonogram_cursor(XEN_TO_C_BOOLEAN(on));
  return(C_TO_XEN_BOOLEAN(show_sonogram_cursor(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_show_sonogram_cursor_reversed, g_set_show_sonogram_cursor)



static XEN g_min_dB(XEN snd, XEN chn) 
{
  #define H_min_dB "(" S_min_dB " :optional snd chn): min dB value displayed in fft graphs using dB scales (default: -60)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_MIN_DB, S_min_dB));
  return(C_TO_XEN_DOUBLE(min_dB(ss)));
}

static void update_db_graph(chan_info *cp, mus_float_t new_db)
{
  cp->min_dB = new_db;
  cp->lin_dB = pow(10.0, cp->min_dB * 0.05); 
  if ((cp->active < CHANNEL_HAS_AXES) ||
      (cp->cgx == NULL) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL) ||
      (!(cp->graph_transform_p)) ||
      (!(cp->fft_log_magnitude)) ||
      (chan_fft_in_progress(cp)))
    return;
  calculate_fft(cp);
}

void set_min_db(mus_float_t db)
{
  set_min_dB(db);
  ss->lin_dB = pow(10.0, db * 0.05);
  for_each_chan_with_float(update_db_graph, db);
}

static XEN g_set_min_dB(XEN val, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_min_dB, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_MIN_DB, S_setB S_min_dB));
  else
    {
      mus_float_t db;
      db = XEN_TO_C_DOUBLE(val);
      if (db < 0.0)
	{
	  set_min_db(db);
	  reflect_min_db_in_transform_dialog();
	}
      else XEN_OUT_OF_RANGE_ERROR(S_setB S_min_dB, 1, val, S_min_dB " (~A) must be < 0.0");
      return(C_TO_XEN_DOUBLE(min_dB(ss)));
    }
}

WITH_THREE_SETTER_ARGS(g_set_min_dB_reversed, g_set_min_dB)



static XEN g_fft_window_beta(XEN snd, XEN chn) 
{
  #define H_fft_window_beta "(" S_fft_window_beta " :optional snd chn): fft window beta parameter value"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_WINDOW_BETA, S_fft_window_beta));
  return(C_TO_XEN_DOUBLE(fft_window_beta(ss)));
}

static XEN g_set_fft_window_beta(XEN val, XEN snd, XEN chn) 
{
  mus_float_t beta;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_fft_window_beta, "a number"); 
  beta = XEN_TO_C_DOUBLE(val);
  if ((beta < 0.0) || (beta > 1.0))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_fft_window_beta, 1, val, "~A, must be between 0.0 and 1.0");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_FFT_WINDOW_BETA, S_setB S_fft_window_beta));
  set_fft_window_beta(beta);
  return(C_TO_XEN_DOUBLE(fft_window_beta(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_fft_window_beta_reversed, g_set_fft_window_beta)



static XEN g_fft_window_alpha(XEN snd, XEN chn) 
{
  #define H_fft_window_alpha "(" S_fft_window_alpha " :optional snd chn): fft window alpha parameter value"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_WINDOW_ALPHA, S_fft_window_alpha));
  return(C_TO_XEN_DOUBLE(fft_window_alpha(ss)));
}

static XEN g_set_fft_window_alpha(XEN val, XEN snd, XEN chn) 
{
  mus_float_t alpha;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_fft_window_alpha, "a number"); 
  alpha = XEN_TO_C_DOUBLE(val);
  if ((alpha < 0.0) || (alpha > 10.0))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_fft_window_alpha, 1, val, "~A, must be between 0.0 and 10.0");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_FFT_WINDOW_ALPHA, S_setB S_fft_window_alpha));
  set_fft_window_alpha(alpha);
  return(C_TO_XEN_DOUBLE(fft_window_alpha(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_fft_window_alpha_reversed, g_set_fft_window_alpha)



static XEN g_spectrum_end(XEN snd, XEN chn) 
{
  #define H_spectrum_end "(" S_spectrum_end " :optional snd chn): max frequency shown in spectra (1.0 = srate/2)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRUM_END, S_spectrum_end));
  return(C_TO_XEN_DOUBLE(spectrum_end(ss)));
}

static XEN g_set_spectrum_end(XEN val, XEN snd, XEN chn) 
{
  mus_float_t cutoff;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectrum_end, "a number"); 
  cutoff = XEN_TO_C_DOUBLE(val);
  if ((cutoff < 0.0) || (cutoff > 1.0))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_spectrum_end, 1, val, "~A, must be between 0.0 and 1.0");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRUM_END, S_setB S_spectrum_end));
  set_spectrum_end(cutoff);
  return(C_TO_XEN_DOUBLE(spectrum_end(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectrum_end_reversed, g_set_spectrum_end)



static XEN g_spectrum_start(XEN snd, XEN chn) 
{
  #define H_spectrum_start "(" S_spectrum_start " :optional snd chn): lower bound of frequency in spectral displays (0.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRUM_START, S_spectrum_start));
  return(C_TO_XEN_DOUBLE(spectrum_start(ss)));
}

static XEN g_set_spectrum_start(XEN val, XEN snd, XEN chn) 
{
  mus_float_t start;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectrum_start, "a number"); 
  start = XEN_TO_C_DOUBLE(val);
  if ((start < 0.0) || (start > 1.0))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_spectrum_start, 1, val, "~A, must be between 0.0 and 1.0");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRUM_START, S_setB S_spectrum_start));
  set_spectrum_start(start);
  return(C_TO_XEN_DOUBLE(spectrum_start(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectrum_start_reversed, g_set_spectrum_start)



static XEN g_spectro_x_angle(XEN snd, XEN chn) 
{
  #define H_spectro_x_angle "(" S_spectro_x_angle " :optional snd chn): spectrogram x-axis viewing angle (90.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_X_ANGLE, S_spectro_x_angle));
  return(C_TO_XEN_DOUBLE(spectro_x_angle(ss)));
}

static XEN g_set_spectro_x_angle(XEN val, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_x_angle, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_X_ANGLE, S_setB S_spectro_x_angle));
  set_spectro_x_angle(mus_fclamp(MIN_SPECTRO_ANGLE, XEN_TO_C_DOUBLE(val), MAX_SPECTRO_ANGLE));
  return(C_TO_XEN_DOUBLE(spectro_x_angle(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectro_x_angle_reversed, g_set_spectro_x_angle)



static XEN g_spectro_x_scale(XEN snd, XEN chn) 
{
  #define H_spectro_x_scale "(" S_spectro_x_scale " :optional snd chn): scaler (stretch) along the spectrogram x axis (1.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_X_SCALE, S_spectro_x_scale));
  return(C_TO_XEN_DOUBLE(spectro_x_scale(ss)));
}

static XEN g_set_spectro_x_scale(XEN val, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_x_scale, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_X_SCALE, S_setB S_spectro_x_scale));
  set_spectro_x_scale(mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), MAX_SPECTRO_SCALE));
  return(C_TO_XEN_DOUBLE(spectro_x_scale(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectro_x_scale_reversed, g_set_spectro_x_scale)



static XEN g_spectro_y_angle(XEN snd, XEN chn) 
{
  #define H_spectro_y_angle "(" S_spectro_y_angle " :optional snd chn): spectrogram y-axis viewing angle (0.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Y_ANGLE, S_spectro_y_angle));
  return(C_TO_XEN_DOUBLE(spectro_y_angle(ss)));
}

static XEN g_set_spectro_y_angle(XEN val, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_y_angle, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Y_ANGLE, S_setB S_spectro_y_angle));
  set_spectro_y_angle(mus_fclamp(MIN_SPECTRO_ANGLE, XEN_TO_C_DOUBLE(val), MAX_SPECTRO_ANGLE));
  return(C_TO_XEN_DOUBLE(spectro_y_angle(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectro_y_angle_reversed, g_set_spectro_y_angle)



static XEN g_spectro_y_scale(XEN snd, XEN chn) 
{
  #define H_spectro_y_scale "(" S_spectro_y_scale " :optional snd chn): scaler (stretch) along the spectrogram y axis (1.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Y_SCALE, S_spectro_y_scale));
  return(C_TO_XEN_DOUBLE(spectro_y_scale(ss)));
}

static XEN g_set_spectro_y_scale(XEN val, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_y_scale, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Y_SCALE, S_setB S_spectro_y_scale));
  set_spectro_y_scale(mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), MAX_SPECTRO_SCALE));
  return(C_TO_XEN_DOUBLE(spectro_y_scale(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectro_y_scale_reversed, g_set_spectro_y_scale)



static XEN g_spectro_z_angle(XEN snd, XEN chn) 
{
  #define H_spectro_z_angle "(" S_spectro_z_angle " :optional snd chn): spectrogram z-axis viewing angle (-2.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Z_ANGLE, S_spectro_z_angle));
  return(C_TO_XEN_DOUBLE(spectro_z_angle(ss)));
}

static XEN g_set_spectro_z_angle(XEN val, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_z_angle, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Z_ANGLE, S_setB S_spectro_z_angle));
  set_spectro_z_angle(mus_fclamp(MIN_SPECTRO_ANGLE, XEN_TO_C_DOUBLE(val), MAX_SPECTRO_ANGLE));
  return(C_TO_XEN_DOUBLE(spectro_z_angle(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectro_z_angle_reversed, g_set_spectro_z_angle)



static XEN g_spectro_z_scale(XEN snd, XEN chn) 
{
  #define H_spectro_z_scale "(" S_spectro_z_scale " :optional snd chn): scaler (stretch) along the spectrogram z axis (0.1)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Z_SCALE, S_spectro_z_scale));
  return(C_TO_XEN_DOUBLE(spectro_z_scale(ss)));
}

static XEN g_set_spectro_z_scale(XEN val, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_z_scale, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Z_SCALE, S_setB S_spectro_z_scale));
  set_spectro_z_scale(mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), MAX_SPECTRO_SCALE));
  return(C_TO_XEN_DOUBLE(spectro_z_scale(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectro_z_scale_reversed, g_set_spectro_z_scale)



static XEN g_spectro_hop(XEN snd, XEN chn)
{
  #define H_spectro_hop "(" S_spectro_hop " :optional snd chn): hop amount (pixels) in spectral displays"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_HOP, S_spectro_hop));
  return(C_TO_XEN_INT(spectro_hop(ss)));
}

static XEN g_set_spectro_hop(XEN val, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_hop, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_HOP, S_setB S_spectro_hop));
  set_spectro_hop(XEN_TO_C_INT_OR_ELSE(val, 0));
  return(C_TO_XEN_INT(spectro_hop(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_spectro_hop_reversed, g_set_spectro_hop)



static XEN g_show_marks(XEN snd, XEN chn)
{
  #define H_show_marks "(" S_show_marks " :optional snd chn): " PROC_TRUE " if Snd should show marks"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_MARKS, S_show_marks));
  return(C_TO_XEN_BOOLEAN(show_marks(ss)));
}

static void chans_marks(chan_info *cp, bool value)
{
  cp->show_marks = value;
  update_graph(cp);
}

void set_show_marks(bool val)
{
  in_set_show_marks(val);
  for_each_chan_with_bool(chans_marks, val);
}

static XEN g_set_show_marks(XEN on, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_show_marks, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_MARKS, S_setB S_show_marks));
  set_show_marks(XEN_TO_C_BOOLEAN(on));
  return(C_TO_XEN_BOOLEAN(show_marks(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_show_marks_reversed, g_set_show_marks)



static XEN g_show_transform_peaks(XEN snd, XEN chn)
{
  #define H_show_transform_peaks "(" S_show_transform_peaks " :optional snd chn): " PROC_TRUE " if fft display should include peak list"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_TRANSFORM_PEAKS, S_show_transform_peaks));
  return(C_TO_XEN_BOOLEAN(show_transform_peaks(ss)));
}

static XEN g_set_show_transform_peaks(XEN val, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ARG_1, S_setB S_show_transform_peaks, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SHOW_TRANSFORM_PEAKS, S_setB S_show_transform_peaks));
  set_show_transform_peaks(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(show_transform_peaks(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_show_transform_peaks_reversed, g_set_show_transform_peaks)



static XEN g_zero_pad(XEN snd, XEN chn)
{
  #define H_zero_pad "(" S_zero_pad " :optional snd chn): zero padding used in fft as a multiple of fft size (0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_ZERO_PAD, S_zero_pad));
  return(C_TO_XEN_INT(zero_pad(ss)));
}

static XEN g_set_zero_pad(XEN val, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_zero_pad, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_ZERO_PAD, S_setB S_zero_pad));
  set_zero_pad(mus_iclamp(0, g_imin(0, val, DEFAULT_ZERO_PAD), MAX_ZERO_PAD));
  return(C_TO_XEN_INT(zero_pad(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_zero_pad_reversed, g_set_zero_pad)



static XEN g_wavelet_type(XEN snd, XEN chn)
{
  #define H_wavelet_type "(" S_wavelet_type " :optional snd chn): wavelet used in wavelet-transform (0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_WAVELET_TYPE, S_wavelet_type));
  return(C_TO_XEN_INT(wavelet_type(ss)));
}

static XEN g_set_wavelet_type(XEN val, XEN snd, XEN chn)
{
  int wave;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_wavelet_type, "an integer"); 
  wave = XEN_TO_C_INT(val);
  if ((wave < 0) || (wave >= NUM_WAVELETS))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_wavelet_type, 1, val, "~A, unknown wavelet type");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_WAVELET_TYPE, S_setB S_wavelet_type));
  set_wavelet_type(wave);
  return(C_TO_XEN_INT(wavelet_type(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_wavelet_type_reversed, g_set_wavelet_type)



static XEN g_fft_log_frequency(XEN snd, XEN chn)
{
  #define H_fft_log_frequency "(" S_fft_log_frequency " :optional snd chn): " PROC_TRUE " if fft displays use log on the frequency axis"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_LOG_FREQUENCY, S_fft_log_frequency));
  return(C_TO_XEN_BOOLEAN(fft_log_frequency(ss)));
}

static XEN g_set_fft_log_frequency(XEN on, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_fft_log_frequency, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_FFT_LOG_FREQUENCY, S_setB S_fft_log_frequency));
  set_fft_log_frequency(XEN_TO_C_BOOLEAN(on)); 
  return(C_TO_XEN_BOOLEAN(fft_log_frequency(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_fft_log_frequency_reversed, g_set_fft_log_frequency)



static XEN g_fft_log_magnitude(XEN snd, XEN chn)
{
  #define H_fft_log_magnitude "(" S_fft_log_magnitude " :optional snd chn): " PROC_TRUE " if fft displays use dB"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_LOG_MAGNITUDE, S_fft_log_magnitude));
  return(C_TO_XEN_BOOLEAN(fft_log_magnitude(ss)));
}

static XEN g_set_fft_log_magnitude(XEN on, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_fft_log_magnitude, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_FFT_LOG_MAGNITUDE, S_setB S_fft_log_magnitude));
  set_fft_log_magnitude(XEN_TO_C_BOOLEAN(on)); 
  return(C_TO_XEN_BOOLEAN(fft_log_magnitude(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_fft_log_magnitude_reversed, g_set_fft_log_magnitude)



static XEN g_fft_with_phases(XEN snd, XEN chn)
{
  #define H_fft_with_phases "(" S_fft_with_phases " :optional snd chn): " PROC_TRUE " if fft displays include phase info"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_WITH_PHASES, S_fft_with_phases));
  return(C_TO_XEN_BOOLEAN(fft_with_phases(ss)));
}

static XEN g_set_fft_with_phases(XEN on, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_fft_with_phases, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_FFT_WITH_PHASES, S_setB S_fft_with_phases));
  set_fft_with_phases(XEN_TO_C_BOOLEAN(on)); 
  return(C_TO_XEN_BOOLEAN(fft_with_phases(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_fft_with_phases_reversed, g_set_fft_with_phases)



static XEN g_show_mix_waveforms(XEN snd, XEN chn)
{
  #define H_show_mix_waveforms "(" S_show_mix_waveforms " :optional snd chn): " PROC_TRUE " if Snd should display mix waveforms (above the main waveform)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_MIX_WAVEFORMS, S_show_mix_waveforms));
  return(C_TO_XEN_BOOLEAN(show_mix_waveforms(ss)));
}

static XEN g_set_show_mix_waveforms(XEN on, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_show_mix_waveforms, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_MIX_WAVEFORMS, S_setB S_show_mix_waveforms));
  set_show_mix_waveforms(XEN_TO_C_BOOLEAN(on));
  return(C_TO_XEN_BOOLEAN(show_mix_waveforms(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_show_mix_waveforms_reversed, g_set_show_mix_waveforms)



static XEN g_verbose_cursor(XEN snd, XEN chn)
{
  #define H_verbose_cursor "(" S_with_verbose_cursor " :optional snd chn): " PROC_TRUE " if the cursor's position and so on is displayed in the minibuffer"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_VERBOSE_CURSOR, S_with_verbose_cursor));
  return(C_TO_XEN_BOOLEAN(verbose_cursor(ss)));
}

static void chans_verbose_cursor(chan_info *cp, bool value)
{
  cp->verbose_cursor = value;
  update_graph(cp);
}

void set_verbose_cursor(bool val)
{
  in_set_verbose_cursor(val);
  if (val == 0) for_each_sound(clear_minibuffer);
  for_each_chan_with_bool(chans_verbose_cursor, val);
}

static XEN g_set_verbose_cursor(XEN on, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_with_verbose_cursor, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_VERBOSE_CURSOR, S_setB S_with_verbose_cursor));
  set_verbose_cursor(XEN_TO_C_BOOLEAN(on));
  return(C_TO_XEN_BOOLEAN(verbose_cursor(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_verbose_cursor_reversed, g_set_verbose_cursor)



static XEN g_time_graph_type(XEN snd, XEN chn)
{
  #define H_time_graph_type "(" S_time_graph_type " :optional snd chn): " S_graph_as_wavogram " if Snd's time domain display is a 'wavogram',\
otherwise " S_graph_once "."
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TIME_GRAPH_TYPE, S_time_graph_type));
  return(C_TO_XEN_INT((int)time_graph_type(ss)));
}

static XEN g_set_time_graph_type(XEN val, XEN snd, XEN chn) 
{
  graph_type_t on;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_time_graph_type, "an integer");
  on = (graph_type_t)XEN_TO_C_INT(val);
  if ((on != GRAPH_ONCE) && (on != GRAPH_AS_WAVOGRAM))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_time_graph_type, 1, val, "~A, must be " S_graph_once ", or " S_graph_as_wavogram);
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_TIME_GRAPH_TYPE, S_setB S_time_graph_type));
  set_time_graph_type(on);
  return(C_TO_XEN_INT((int)time_graph_type(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_time_graph_type_reversed, g_set_time_graph_type)



static XEN g_wavo_hop(XEN snd, XEN chn)
{
  #define H_wavo_hop "(" S_wavo_hop " :optional snd chn): wavogram spacing between successive traces"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_WAVO_HOP, S_wavo_hop));
  return(C_TO_XEN_INT(wavo_hop(ss)));
}

static XEN g_set_wavo_hop(XEN val, XEN snd, XEN chn) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_wavo_hop, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_WAVO_HOP, S_setB S_wavo_hop));
  set_wavo_hop(XEN_TO_C_INT_OR_ELSE(val, 0));
  return(C_TO_XEN_INT(wavo_hop(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_wavo_hop_reversed, g_set_wavo_hop)



static XEN g_wavo_trace(XEN snd, XEN chn)
{
  #define H_wavo_trace "(" S_wavo_trace " :optional snd chn): length (samples) of each trace in the wavogram (64)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_WAVO_TRACE, S_wavo_trace));
  return(C_TO_XEN_INT(wavo_trace(ss)));
}

static XEN g_set_wavo_trace(XEN val, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_wavo_trace, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_WAVO_TRACE, S_setB S_wavo_trace));
  set_wavo_trace(mus_iclamp(1, XEN_TO_C_INT_OR_ELSE(val, DEFAULT_WAVO_TRACE), POINT_BUFFER_SIZE));
  return(C_TO_XEN_INT(wavo_trace(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_wavo_trace_reversed, g_set_wavo_trace)



static XEN g_transform_size(XEN snd, XEN chn)
{
  #define H_transform_size "(" S_transform_size " :optional snd chn): current fft size (512)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_SIZE, S_transform_size));
  return(C_TO_XEN_INT64_T(transform_size(ss)));
}

static XEN g_set_transform_size(XEN val, XEN snd, XEN chn)
{
  mus_long_t len;

  XEN_ASSERT_TYPE(XEN_INT64_T_P(val), val, XEN_ARG_1, S_setB S_transform_size, "an integer"); 
  len = XEN_TO_C_INT64_T(val);
  if (len <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_transform_size, 1, val, "size ~A, but must be > 0");
  if (!(POWER_OF_2_P(len)))
    len = snd_mus_long_t_pow2((int)(log(len + 1) / log(2.0))); /* actually rounds down, despite appearances */
  if (len <= 0) return(XEN_FALSE);
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_SIZE, S_setB S_transform_size));
  set_transform_size(len);
  return(C_TO_XEN_INT64_T(transform_size(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_transform_size_reversed, g_set_transform_size)



static XEN g_transform_graph_type(XEN snd, XEN chn)
{
  #define H_transform_graph_type "(" S_transform_graph_type " :optional snd chn) can \
be " S_graph_once ", " S_graph_as_sonogram ", or " S_graph_as_spectrogram "."
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_GRAPH_TYPE, S_transform_graph_type));
  return(C_TO_XEN_INT((int)transform_graph_type(ss)));
}

static XEN g_set_transform_graph_type(XEN val, XEN snd, XEN chn)
{
  graph_type_t style;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_transform_graph_type, "an integer"); 
  style = (graph_type_t)XEN_TO_C_INT(val);
  if (style > GRAPH_AS_SPECTROGRAM)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_transform_graph_type, 1, val, "~A, but must be " S_graph_once ", " S_graph_as_sonogram ", or " S_graph_as_spectrogram);
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_GRAPH_TYPE, S_setB S_transform_graph_type));
  set_transform_graph_type(style);
  return(C_TO_XEN_INT((int)transform_graph_type(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_transform_graph_type_reversed, g_set_transform_graph_type)



static XEN g_fft_window(XEN snd, XEN chn)
{
  #define H_fft_window "(" S_fft_window " :optional snd chn): fft data window choice (e.g. " S_blackman2_window ").  The \
choices are: " S_rectangular_window ", " S_hann_window ", " S_welch_window ", " S_parzen_window ", \
" S_bartlett_window ", " S_hamming_window ", " S_blackman2_window ", " S_blackman3_window ", \
" S_blackman4_window ", " S_exponential_window ", " S_riemann_window ", " S_kaiser_window ", \
" S_cauchy_window ", " S_poisson_window ", " S_gaussian_window ", " S_tukey_window ", \
" S_dolph_chebyshev_window ", " S_hann_poisson_window ", " S_connes_window ", \
" S_samaraki_window ", " S_ultraspherical_window ", " S_bartlett_hann_window ", " S_bohman_window ", " S_flat_top_window ", and " S_blackman5_window " et al."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_WINDOW, S_fft_window));
  return(C_TO_XEN_INT(fft_window(ss)));
}

static XEN g_set_fft_window(XEN val, XEN snd, XEN chn)
{
  int in_win;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_fft_window, "an integer"); 

  in_win = XEN_TO_C_INT(val);
  if (!(mus_fft_window_p(in_win)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_fft_window, 1, val, "~A: unknown fft data window");

  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_FFT_WINDOW, S_setB S_fft_window));

  set_fft_window((mus_fft_window_t)in_win);
  return(C_TO_XEN_INT((int)fft_window(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_fft_window_reversed, g_set_fft_window)



static XEN g_transform_type(XEN snd, XEN chn)
{
  #define H_transform_type "(" S_transform_type " :optional snd chn): transform type; can be one of " S_fourier_transform ", \
" S_wavelet_transform ", " S_haar_transform ", " S_autocorrelation ", " S_walsh_transform ", \
" S_cepstrum ", or an index corresponding to an added transform."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_TYPE, S_transform_type));
  return(C_TO_XEN_INT(transform_type(ss)));
}

static XEN g_set_transform_type(XEN val, XEN snd, XEN chn)
{
  int type;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_transform_type, "an integer"); 
  type = XEN_TO_C_INT(val);
  if (!(transform_p(type)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_transform_type, 1, val, "~A: unknown transform type");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_TYPE, S_setB S_transform_type));
  set_transform_type(type);
  return(C_TO_XEN_INT(transform_type(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_transform_type_reversed, g_set_transform_type)



static XEN g_transform_normalization(XEN snd, XEN chn)
{
  #define H_transform_normalization "(" S_transform_normalization " :optional snd chn): one of " S_dont_normalize ", " S_normalize_by_channel \
", " S_normalize_by_sound ", or " S_normalize_globally ". \
decides whether spectral data is normalized before display (default: " S_normalize_by_channel ")"

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_NORMALIZATION, S_transform_normalization));
  return(C_TO_XEN_INT((int)transform_normalization(ss)));
}

static XEN g_set_transform_normalization(XEN val, XEN snd, XEN chn)
{
  fft_normalize_t norm;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_transform_normalization, "an integer");
  norm = (fft_normalize_t)XEN_TO_C_INT(val);
  if (norm >= NUM_TRANSFORM_NORMALIZATIONS)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_transform_normalization, 1, val, 
			   "~A, but must be " S_dont_normalize ", " S_normalize_by_channel ", " S_normalize_by_sound ", or " S_normalize_globally);
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_NORMALIZATION, S_setB S_transform_normalization));
  set_transform_normalization(norm);
  return(C_TO_XEN_INT((int)transform_normalization(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_transform_normalization_reversed, g_set_transform_normalization)



static XEN g_max_transform_peaks(XEN snd, XEN chn)
{
  #define H_max_transform_peaks "(" S_max_transform_peaks " :optional snd chn): max number of fft peaks reported in fft display"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_MAX_TRANSFORM_PEAKS, S_max_transform_peaks));
  return(C_TO_XEN_INT(max_transform_peaks(ss)));
}

static XEN g_set_max_transform_peaks(XEN n, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_max_transform_peaks, "an integer"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, n, CP_MAX_TRANSFORM_PEAKS, S_setB S_max_transform_peaks));
  else
    {
      int lim;
      lim = XEN_TO_C_INT(n);
      if (lim >= 1)
	{
	  set_max_transform_peaks(lim);
	  reflect_peaks_in_transform_dialog();
	}
      return(C_TO_XEN_INT(max_transform_peaks(ss)));
    }
}

WITH_THREE_SETTER_ARGS(g_set_max_transform_peaks_reversed, g_set_max_transform_peaks)



static XEN g_graph_style(XEN snd, XEN chn)
{
  #define H_graph_style "(" S_graph_style " :optional snd chn): graph style, one \
of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TIME_GRAPH_STYLE, S_time_graph_style));
  return(C_TO_XEN_INT(graph_style(ss)));
}

static void chans_graph_style(chan_info *cp, int value)
{
  graph_style_t style = (graph_style_t)value;
  cp->time_graph_style = style;
  cp->lisp_graph_style = style;
  cp->transform_graph_style = style;
  update_graph(cp);
}

void set_graph_style(graph_style_t val)
{
  in_set_graph_style(val);
  for_each_chan_with_int(chans_graph_style, (int)val);
}

static XEN g_set_graph_style(XEN style, XEN snd, XEN chn)
{
  int val;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_graph_style, "an integer"); 

  val = XEN_TO_C_INT(style);
  if (!(graph_style_p(val)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_graph_style, 1, style, "~A: unknown graph-style");

  if (XEN_BOUND_P(snd))
    {
      XEN xval;
      call_update_graph = false;
      xval = channel_set(snd, chn, style, CP_TIME_GRAPH_STYLE, S_setB S_graph_style);
      channel_set(snd, chn, style, CP_LISP_GRAPH_STYLE, S_setB S_graph_style);
      call_update_graph = true;
      channel_set(snd, chn, style, CP_TRANSFORM_GRAPH_STYLE, S_setB S_graph_style);
      return(xval);
    }
  set_graph_style((graph_style_t)val);
  return(C_TO_XEN_INT((int)(graph_style(ss))));
}

WITH_THREE_SETTER_ARGS(g_set_graph_style_reversed, g_set_graph_style)



static XEN g_time_graph_style(XEN snd, XEN chn)
{
  #define H_time_graph_style "(" S_time_graph_style " snd chn): time domain graph drawing style. \
one of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"
  ASSERT_SOUND(S_time_graph_style, snd, 0);
  return(channel_get(snd, chn, CP_TIME_GRAPH_STYLE, S_time_graph_style));
}

static XEN g_set_time_graph_style(XEN style, XEN snd, XEN chn)
{
  int val;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_time_graph_style, "an integer"); 
  ASSERT_SOUND(S_time_graph_style, snd, 0);

  val = XEN_TO_C_INT(style);
  if (!(graph_style_p(val)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_time_graph_style, 1, style, "~A: unknown " S_time_graph_style);

  return(channel_set(snd, chn, style, CP_TIME_GRAPH_STYLE, S_setB S_time_graph_style));
}

WITH_THREE_SETTER_ARGS(g_set_time_graph_style_reversed, g_set_time_graph_style)



static XEN g_lisp_graph_style(XEN snd, XEN chn)
{
  #define H_lisp_graph_style "(" S_lisp_graph_style " snd chn): lisp graph drawing style. \
one of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"
  ASSERT_SOUND(S_lisp_graph_style, snd, 0);
  return(channel_get(snd, chn, CP_LISP_GRAPH_STYLE, S_lisp_graph_style));
}

static XEN g_set_lisp_graph_style(XEN style, XEN snd, XEN chn)
{
  int val;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_lisp_graph_style, "an integer"); 
  ASSERT_SOUND(S_lisp_graph_style, snd, 0);

  val = XEN_TO_C_INT(style);
  if (!(graph_style_p(val)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_lisp_graph_style, 1, style, "~A: unknown " S_lisp_graph_style);

  return(channel_set(snd, chn, style, CP_LISP_GRAPH_STYLE, S_setB S_lisp_graph_style));
}

WITH_THREE_SETTER_ARGS(g_set_lisp_graph_style_reversed, g_set_lisp_graph_style)



static XEN g_transform_graph_style(XEN snd, XEN chn)
{
  #define H_transform_graph_style "(" S_transform_graph_style " snd chn): fft graph drawing style, one \
of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"
  ASSERT_SOUND(S_transform_graph_style, snd, 0);
  return(channel_get(snd, chn, CP_TRANSFORM_GRAPH_STYLE, S_transform_graph_style));
}

static XEN g_set_transform_graph_style(XEN style, XEN snd, XEN chn)
{
  int val;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_transform_graph_style, "an integer"); 
  ASSERT_SOUND(S_transform_graph_style, snd, 0);

  val = XEN_TO_C_INT(style);

  if (!(graph_style_p(val)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_transform_graph_style, 1, style, "~A: unknown " S_transform_graph_style);

  return(channel_set(snd, chn, style, CP_TRANSFORM_GRAPH_STYLE, S_setB S_transform_graph_style));
}

WITH_THREE_SETTER_ARGS(g_set_transform_graph_style_reversed, g_set_transform_graph_style)



static XEN g_dot_size(XEN snd, XEN chn)
{
  #define H_dot_size "(" S_dot_size " :optional snd chn): size in pixels of dots when graphing with dots (1)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_DOT_SIZE, S_dot_size));
  return(C_TO_XEN_INT(dot_size(ss)));
}

static XEN g_set_dot_size(XEN size, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_1, S_setB S_dot_size, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, size, CP_DOT_SIZE, S_setB S_dot_size));
  set_dot_size(mus_iclamp(MIN_DOT_SIZE, 
			  XEN_TO_C_INT_OR_ELSE(size, DEFAULT_DOT_SIZE), 
			  MAX_DOT_SIZE));
  return(C_TO_XEN_INT(dot_size(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_dot_size_reversed, g_set_dot_size)



static XEN g_x_axis_style(XEN snd, XEN chn)
{
  #define H_x_axis_style "(" S_x_axis_style " :optional snd chn): \
The x axis labelling of the time domain waveform can be in seconds (" S_x_axis_in_seconds "), in samples (" S_x_axis_in_samples "), expressed as a \
percentage of the overall duration (" S_x_axis_as_percentage "), as a beat number (" S_x_axis_in_beats "), as a measure \
number (" S_x_axis_in_measures "), or clock-style (dd:hh:mm:ss) (" S_x_axis_as_clock ")."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_X_AXIS_STYLE, S_x_axis_style));
  return(C_TO_XEN_INT((int)x_axis_style(ss)));
}

static void chans_x_axis_style(chan_info *cp, int value)
{
  axis_info *ap;
  x_axis_style_t new_style = (x_axis_style_t)value;
  ap = cp->axis;
  cp->x_axis_style = new_style;
  if (ap)
    {
      if (ap->xlabel) free(ap->xlabel);
      /* if user set x-axis-label (snd-axis.c) ap->default_xlabel is not null, so use it rather than normal choices */
      if (ap->default_xlabel)
	ap->xlabel = mus_strdup(ap->default_xlabel);
      else
	{
	  switch (new_style)
	    {
	    case X_AXIS_IN_BEATS:      ap->xlabel = mus_strdup(_("time (beats)"));    break;
	    case X_AXIS_IN_MEASURES:   ap->xlabel = mus_strdup(_("time (measures)")); break;
	    case X_AXIS_IN_SAMPLES:    ap->xlabel = mus_strdup(_("time (samples)"));  break;
	    case X_AXIS_AS_PERCENTAGE: ap->xlabel = mus_strdup(_("time (percent)"));  break;
	    default:                   ap->xlabel = mus_strdup(_("time"));            break;
	    }
	}
      update_graph(cp);
    }
} 

void set_x_axis_style(x_axis_style_t val)
{
  in_set_x_axis_style(val);
  for_each_chan_with_int(chans_x_axis_style, (int)val);
}

static XEN g_set_x_axis_style(XEN style, XEN snd, XEN chn)
{
  int val;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_x_axis_style, "an integer"); 

  val = XEN_TO_C_INT(style);
  if (!(x_axis_style_p(val)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_x_axis_style, 1, style, "~A is not an x-axis-style");

  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, style, CP_X_AXIS_STYLE, S_setB S_x_axis_style));

  set_x_axis_style((x_axis_style_t)val);
  /* snd-menu.c -- maps over chans */
  return(C_TO_XEN_INT((int)x_axis_style(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_x_axis_style_reversed, g_set_x_axis_style)



static XEN g_beats_per_minute(XEN snd, XEN chn)
{
  #define H_beats_per_minute "(" S_beats_per_minute " :optional snd chn): beats per minute if " S_x_axis_style " is " S_x_axis_in_beats
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_BEATS_PER_MINUTE, S_beats_per_minute));
  return(C_TO_XEN_DOUBLE(beats_per_minute(ss)));
}

static XEN g_set_beats_per_minute(XEN beats, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beats), beats, XEN_ARG_1, S_setB S_beats_per_minute, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, beats, CP_BEATS_PER_MINUTE, S_setB S_beats_per_minute));
  else
    {
      mus_float_t val;
      val = XEN_TO_C_DOUBLE(beats);
      if (val > 0.0)
	set_beats_per_minute(val);
      return(C_TO_XEN_DOUBLE(beats_per_minute(ss)));
    }
}

WITH_THREE_SETTER_ARGS(g_set_beats_per_minute_reversed, g_set_beats_per_minute)



static XEN g_beats_per_measure(XEN snd, XEN chn)
{
  #define H_beats_per_measure "(" S_beats_per_measure " :optional snd chn): beats per measure if " S_x_axis_style " is " S_x_axis_in_measures
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_BEATS_PER_MEASURE, S_beats_per_measure));
  return(C_TO_XEN_INT(beats_per_measure(ss)));
}

static XEN g_set_beats_per_measure(XEN beats, XEN snd, XEN chn)
{
  int val;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(beats), beats, XEN_ARG_1, S_setB S_beats_per_measure, "an int"); 
  val = XEN_TO_C_INT(beats);
  if ((val <= 0) || (val > 1000))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_beats_per_measure, 1, beats, "must be between 1 and 1000");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, beats, CP_BEATS_PER_MEASURE, S_setB S_beats_per_measure));
  set_beats_per_measure(val);
  return(C_TO_XEN_INT(beats_per_measure(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_beats_per_measure_reversed, g_set_beats_per_measure)



static XEN g_show_axes(XEN snd, XEN chn)
{
  #define H_show_axes "(" S_show_axes " :optional snd chn) \
If " S_show_all_axes ", display x and y axes; if " S_show_x_axis ", just one axis (the x axis) is displayed. \
The other choices are " S_show_no_axes ", " S_show_all_axes_unlabelled ", " S_show_x_axis_unlabelled ", and " S_show_bare_x_axis "."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_AXES, S_show_axes));
  return(C_TO_XEN_INT((int)show_axes(ss)));
}

static XEN g_set_show_axes(XEN on, XEN snd, XEN chn)
{
  int val;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, S_setB S_show_axes, "an integer");

  val = XEN_TO_C_INT(on);
  if (!(show_axes_p(val)))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_show_axes, 1, on, "~A must be a show-axes choice");

  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_AXES, S_setB S_show_axes));

  set_show_axes((show_axes_t)val);
  return(C_TO_XEN_INT((int)show_axes(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_show_axes_reversed, g_set_show_axes)



static XEN g_graphs_horizontal(XEN snd, XEN chn)
{
  #define H_graphs_horizontal "(" S_graphs_horizontal " :optional snd chn): " PROC_TRUE " if the time domain, fft, and lisp graphs are layed out horizontally"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_GRAPHS_HORIZONTAL, S_graphs_horizontal));
  return(C_TO_XEN_BOOLEAN(graphs_horizontal(ss)));
}

static XEN g_set_graphs_horizontal(XEN val, XEN snd, XEN chn)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ARG_1, S_setB S_graphs_horizontal, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_GRAPHS_HORIZONTAL, S_setB S_graphs_horizontal));
  set_graphs_horizontal(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(graphs_horizontal(ss)));
}

WITH_THREE_SETTER_ARGS(g_set_graphs_horizontal_reversed, g_set_graphs_horizontal)




static void write_transform_peaks(FILE *fd, chan_info *ucp)
{
  /* put (sync'd) peak info in (possibly temporary) file */
  int i, chn;
  sync_info *si = NULL;

  fprintf(fd, _("Snd: fft peaks (%s)\n\n"), snd_local_time());

  si = sync_to_chan(ucp);
  for (chn = 0; chn < si->chans; chn++)
    {
      snd_info *sp;
      chan_info *cp;
      fft_info *fp;

      cp = si->cps[chn];
      fp = cp->fft;
      if (fp)
	{
	  axis_info *fap;

	  fap = fp->axis;
	  if ((fap) && (fap->graph_active))
	    {
	      mus_float_t srate2;
	      axis_info *ap;
	      fft_peak *peak_freqs = NULL;
	      fft_peak *peak_amps = NULL;
	      mus_float_t *data;
	      int num_peaks,samps, cutoff_samps, tens, srate, digits = 5;
	      mus_float_t samples_per_pixel, cutoff;

	      ap = cp->axis;
	      sp = cp->sound;
	      data = fp->data;
	      cutoff = cp->spectrum_end;
	      samps = (int)(fp->current_size * 0.5);
	      cutoff_samps = (int)(samps * cutoff);
	      samples_per_pixel = (mus_float_t)cutoff_samps / (mus_float_t)(fap->x_axis_x1 - fap->x_axis_x0);

	      srate = SND_SRATE(sp);
	      srate2 = (mus_float_t)srate * .5;
	      if (samps > (5 * srate)) 
		tens = 2; 
	      else 
		if (samps > (int)srate2) 
		  tens = 1; 
		else 
		  if (samps > (srate / 20)) 
		    tens = 0; 
		  else tens = -1;
	      
	      if (ceil(log10(srate)) > 3.0)
		digits = (int)ceil(log10(srate)) + 2;

	      peak_freqs = (fft_peak *)calloc(cp->max_transform_peaks, sizeof(fft_peak));
	      peak_amps = (fft_peak *)calloc(cp->max_transform_peaks, sizeof(fft_peak));
	      num_peaks = find_and_sort_transform_peaks(data, peak_freqs, cp->max_transform_peaks, 0, samps, samples_per_pixel, fp->scale);

	      if ((num_peaks != 1) || 
		  (peak_freqs[0].freq != 0.0))
		{
		  fprintf(fd, "%s", sp->short_filename);
		  if (sp->nchans > 1) fprintf(fd, _(": chan %d"), cp->chan);
		  fprintf(fd, _(", fft " MUS_LD " points beginning at sample " MUS_LD " (%.3f secs), %s\n\n"),
			  fp->current_size, 
			  ap->losamp, 
			  (float)((double)(ap->losamp) / (double)srate),
			  mus_fft_window_name(cp->fft_window)); /* was XEN name */
		  for (i = 0; i < num_peaks; i++)
		    fprintf(fd, "  %.*f  %.*f\n",
			    tens, 
			    peak_freqs[i].freq * srate2, 
			    digits,
			    peak_freqs[i].amp); 
		  fprintf(fd, "\n");
		}
	      if (peak_freqs) {free(peak_freqs); peak_freqs = NULL;}
	      if (peak_amps) {free(peak_amps); peak_amps = NULL;}
	    }
	}
    }
  if (si) si = free_sync_info(si);
}


static XEN g_peaks(XEN filename, XEN snd, XEN chn_n)
{
  #define H_peaks "(" S_peaks " :optional filename snd chn): write current fft peaks data to filename, or \
to the info dialog if filename is omitted"

  char *name = NULL, *str;
  chan_info *cp;
  bool post_to_dialog = true;
  FILE *fd = NULL;

  XEN_ASSERT_TYPE((XEN_STRING_P(filename) || (XEN_FALSE_P(filename)) || (XEN_NOT_BOUND_P(filename))), filename, XEN_ARG_1, S_peaks, "a string or " PROC_FALSE);

  ASSERT_CHANNEL(S_peaks, snd, chn_n, 2);
  cp = get_cp(snd, chn_n, S_peaks);
  if (!cp) return(XEN_FALSE);

  if (XEN_STRING_P(filename))
    {
      name = mus_expand_filename(XEN_TO_C_STRING(filename));
      if ((name) && (mus_strlen(name) > 0))
	{
	  fd = FOPEN(name, "w");
	  post_to_dialog = false;
	}
    }
  else
    {
      name = snd_tempnam();
      fd = FOPEN(name, "w");
    }

  if (!fd)
    XEN_ERROR(XEN_ERROR_TYPE("cant-open-file"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_peaks),
			 C_TO_XEN_STRING(name),
			 C_TO_XEN_STRING(snd_io_strerror())));

  write_transform_peaks(fd, cp);
  snd_fclose(fd, name);

  if (post_to_dialog)
    {
      str = file_to_string(name);
      if (str)
	{
	  post_it("fft peaks", str);
	  free(str);
	}
      snd_remove(name, IGNORE_CACHE);
    }
  free(name);
  return(filename);
}


static XEN g_left_sample(XEN snd, XEN chn_n) 
{
  #define H_left_sample "(" S_left_sample " :optional snd chn): left sample number in time domain window"
  return(channel_get(snd, chn_n, CP_AP_LOSAMP, S_left_sample));
}

static XEN g_set_left_sample(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_INT64_T_P(on) || XEN_NOT_BOUND_P(on), on, XEN_ARG_1, S_setB S_left_sample, "an integer");
  return(channel_set(snd, chn_n, on, CP_AP_LOSAMP, S_setB S_left_sample));
}

WITH_THREE_SETTER_ARGS(g_set_left_sample_reversed, g_set_left_sample)



static XEN g_right_sample(XEN snd, XEN chn_n) 
{
  #define H_right_sample "(" S_right_sample " :optional snd chn): right sample number in time domain window"
  return(channel_get(snd, chn_n, CP_AP_HISAMP, S_right_sample));
}

static XEN g_set_right_sample(XEN on, XEN snd, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_INT64_T_P(on) || XEN_NOT_BOUND_P(on), on, XEN_ARG_1, S_setB S_right_sample, "an integer");
  return(channel_set(snd, chn_n, on, CP_AP_HISAMP, S_setB S_right_sample));
}

WITH_THREE_SETTER_ARGS(g_set_right_sample_reversed, g_set_right_sample)



static XEN g_channel_properties(XEN snd, XEN chn_n) 
{
  #define H_channel_properties "(" S_channel_properties " :optional snd chn): \
A property list associated with the given channel. It is set to '() at the time a sound is opened. \
The accessor channel-property is provided in extensions." XEN_FILE_EXTENSION "."

  return(channel_get(snd, chn_n, CP_PROPERTIES, S_channel_properties));
}

static XEN g_set_channel_properties(XEN on, XEN snd, XEN chn_n) 
{
  /* XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_channel_properties, "a property list"); */
  return(channel_set(snd, chn_n, on, CP_PROPERTIES, S_setB S_channel_properties));
}

WITH_THREE_SETTER_ARGS(g_set_channel_properties_reversed, g_set_channel_properties)



static XEN g_edit_properties(XEN snd, XEN chn_n, XEN pos) 
{
  #define H_edit_properties "(" S_edit_properties " :optional snd chn edpos): \
A property list associated with the given edit. It is set to '() at the time an edit is performed and cleared when that edit is no longer accessible. \
The accessor edit-property is provided in extensions." XEN_FILE_EXTENSION "."

  chan_info *cp;
  int edpos;

  ASSERT_CHANNEL(S_edit_properties, snd, chn_n, 1);

  cp = get_cp(snd, chn_n, S_edit_properties);
  if (!cp) return(XEN_FALSE);

  edpos = to_c_edit_position(cp, pos, S_edit_properties, 3);
  if (!(XEN_VECTOR_P(cp->edits[edpos]->properties)))
    {
      cp->edits[edpos]->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
      cp->edits[edpos]->properties_gc_loc = snd_protect(cp->edits[edpos]->properties);
    }
  return(XEN_VECTOR_REF(cp->edits[edpos]->properties, 0));
}

static XEN g_set_edit_properties(XEN on, XEN snd, XEN chn_n, XEN pos) 
{
  chan_info *cp;
  int edpos;

  ASSERT_CHANNEL(S_setB S_edit_properties, snd, chn_n, 1);

  cp = get_cp(snd, chn_n, S_setB S_edit_properties);
  if (!cp) return(XEN_FALSE);

  edpos = to_c_edit_position(cp, pos, S_setB S_edit_properties, 3);
  if (!(XEN_VECTOR_P(cp->edits[edpos]->properties)))
    {
      cp->edits[edpos]->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
      cp->edits[edpos]->properties_gc_loc = snd_protect(cp->edits[edpos]->properties);
    }
  XEN_VECTOR_SET(cp->edits[edpos]->properties, 0, on);
  return(XEN_VECTOR_REF(cp->edits[edpos]->properties, 0));
}

WITH_FOUR_SETTER_ARGS(g_set_edit_properties_reversed, g_set_edit_properties)



static XEN g_edits(XEN snd, XEN chn_n)
{
  #define H_edits "(" S_edits " :optional snd chn): -> (list undoable-edits redoable-edits) in snd's channel chn"
  chan_info *cp;
  int i;
  ASSERT_CHANNEL(S_edits, snd, chn_n, 1);
  cp = get_cp(snd, chn_n, S_edits);
  if (!cp) return(XEN_FALSE);
  for (i = cp->edit_ctr + 1; i < cp->edit_size; i++)
    if (!(cp->edits[i])) break;
  return(XEN_LIST_2(C_TO_XEN_INT(cp->edit_ctr),
		    C_TO_XEN_INT(i - cp->edit_ctr - 1)));
}


static XEN g_graph(XEN ldata, XEN xlabel, XEN x0, XEN x1, XEN y0, XEN y1, XEN snd, XEN chn_n, XEN force_display, XEN show_axes)
{
  #define H_graph "(" S_graph " data :optional xlabel (x0 0.0) (x1 1.0) y0 y1 snd chn (force-display " PROC_TRUE ") show-axes): \
displays 'data' as a graph with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1; 'data' can be a list or a vct. \
If 'data' is a list of numbers, it is treated as an envelope."

  chan_info *cp;
  lisp_grf *lg;
  XEN data = XEN_UNDEFINED, lst;
  char *label = NULL;
  vct *v = NULL;
  int i, len = 0, graph, graphs;
  bool need_update = false;
  mus_float_t ymin, ymax, val;
  double nominal_x0, nominal_x1;
  int old_height = 0, old_width = 0, ww = 0;
  int old_y_offset = 0, gx0 = 0;

  /* ldata can be a vct or a list of numbers or vcts */
  XEN_ASSERT_TYPE(((MUS_VCT_P(ldata)) || 
		   ((XEN_LIST_P(ldata)) && (XEN_LIST_LENGTH(ldata) > 0) && 
		    ((XEN_NUMBER_P(XEN_CAR(ldata))) || (MUS_VCT_P(XEN_CAR(ldata)))))),
		  ldata, XEN_ARG_1, S_graph, "a vct or a list");

  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(x0), x0, XEN_ARG_3, S_graph, "a number (x0)");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(x1), x1, XEN_ARG_4, S_graph, "a number (x1)");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(y0), y0, XEN_ARG_5, S_graph, "a number (y0)");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(y1), y1, XEN_ARG_6, S_graph, "a number (y1)");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(force_display), force_display, XEN_ARG_9, S_graph, "a boolean (force-display)");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(show_axes), show_axes, XEN_ARG_10, S_graph, "an integer (show-axes choice)");

  ASSERT_CHANNEL(S_graph, snd, chn_n, 7);
  cp = get_cp(snd, chn_n, S_graph);
  if (!cp) return(XEN_FALSE);

  ymin = 32768.0;
  ymax = -32768.0;
  if ((cp->sound_ctr == NOT_A_SOUND) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL) ||
      (cp->axis == NULL))
    return(XEN_FALSE);

  if (XEN_STRING_P(xlabel)) label = mus_strdup(XEN_TO_C_STRING(xlabel)); 
  if (XEN_NUMBER_P(x0)) nominal_x0 = XEN_TO_C_DOUBLE(x0); else nominal_x0 = 0.0;
  if (XEN_NUMBER_P(x1)) nominal_x1 = XEN_TO_C_DOUBLE(x1); else nominal_x1 = 1.0;
  if (XEN_NUMBER_P(y0)) ymin = XEN_TO_C_DOUBLE(y0);
  if (XEN_NUMBER_P(y1)) ymax = XEN_TO_C_DOUBLE(y1);

  if ((!(XEN_LIST_P(ldata))) || 
      (XEN_NUMBER_P(XEN_CAR(ldata))))
    graphs = 1; 
  else graphs = XEN_LIST_LENGTH(ldata);
  if (graphs == 0) return(XEN_FALSE);
  lg = cp->lisp_info;

  if (lg)
    {
      axis_info *uap = NULL;
      uap = cp->lisp_info->axis;
      if (uap)
	{
	  old_height = uap->height;
	  old_width = uap->width;
	  ww = uap->window_width;
	  gx0 = uap->graph_x0;
	  old_y_offset = uap->y_offset;
	}
      if (graphs != lg->graphs)
	{
	  free_lisp_info(cp);
	  cp->lisp_info = NULL;
	}
    }

  if (!(cp->lisp_info))
    {
      lg = (lisp_grf *)calloc(graphs, sizeof(lisp_grf));
      cp->lisp_info = lg;
      lg->len = (int *)calloc(graphs, sizeof(int));
      lg->graphs = graphs;
      lg->data = (mus_float_t **)calloc(graphs, sizeof(mus_float_t *));
      need_update = true;
    }
  
  cp->lisp_info->show_axes = cp->show_axes;
  if (XEN_INTEGER_P(show_axes))
    {
      show_axes_t val;
      val = (show_axes_t)XEN_TO_C_INT(show_axes);
      if (val < NUM_SHOW_AXES)
	cp->lisp_info->show_axes = val;
    }

  if ((XEN_LIST_P_WITH_LENGTH(ldata, len)) &&
      (XEN_NUMBER_P(XEN_CAR(ldata))))
    {
      /* just one graph to display */
      lg = cp->lisp_info;
      lg->env_data = 1;
      if (lg->len[0] != len)
	{
	  if (lg->data[0]) free(lg->data[0]);
	  lg->data[0] = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	  lg->len[0] = len;
	}
      for (i = 0, lst = XEN_COPY_ARG(ldata); i < len; i++, lst = XEN_CDR(lst))
	lg->data[0][i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
      if ((!XEN_NUMBER_P(y0)) || 
	  (!XEN_NUMBER_P(y1)))
	{
	  for (i = 1; i < len; i += 2)
	    {
	      val = lg->data[0][i];
	      if (ymin > val) ymin = val;
	      if (ymax < val) ymax = val;
	    }
	}
      if (!XEN_NUMBER_P(x0)) nominal_x0 = lg->data[0][0];
      if (!XEN_NUMBER_P(x1)) nominal_x1 = lg->data[0][len - 2];
    }
  else
    {
      lg = cp->lisp_info;
      lg->env_data = 0;
      for (graph = 0; graph < graphs; graph++)
	{
	  if (XEN_LIST_P(ldata))
	    data = XEN_LIST_REF(ldata, graph);
	  else data = ldata;
	  v = XEN_TO_VCT(data);
	  len = v->length;
	  if (lg->len[graph] != len)
	    {
	      if (lg->data[graph]) free(lg->data[graph]);
	      lg->data[graph] = (mus_float_t *)calloc(len, sizeof(mus_float_t));
	      lg->len[graph] = len;
	    }
	  memcpy((void *)(lg->data[graph]), (void *)(v->data), len * sizeof(mus_float_t));
	  if ((!XEN_NUMBER_P(y0)) || 
	      (!XEN_NUMBER_P(y1)))
	    {
	      for (i = 0; i < len; i++)
		{
		  val = lg->data[graph][i];
		  if (ymin > val) ymin = val;
		  if (ymax < val) ymax = val;
		}
	    }
	}
    }

  lg->axis = make_axis_info(cp, nominal_x0, nominal_x1, ymin, ymax, label, nominal_x0, nominal_x1, ymin, ymax, lg->axis);
  lg->axis->y_offset = old_y_offset;

  if (label) free(label);
  if (need_update)
    {
      axis_info *uap = NULL;
      uap = lg->axis;
      uap->height = old_height;
      uap->window_width = ww;
      uap->width = old_width;
      uap->graph_x0 = gx0;
    }
  cp->graph_lisp_p = true;
  if ((XEN_NOT_BOUND_P(force_display)) || 
      (XEN_NOT_FALSE_P(force_display)))
    {
      if (need_update)
	update_graph(cp);
      else display_channel_lisp_data(cp);
    }
  return(xen_return_first(XEN_FALSE, ldata, data, xlabel));
  /* returning #f here because graph might be last thing in lisp-graph-hook, and we
   *   don't want its return value mistaken for the "pixel_list" that that hook function
   *   can return.
   */
}


#if HAVE_GL
#define S_glSpectrogram "glSpectrogram"
static XEN g_gl_spectrogram(XEN data, XEN gl_list, XEN cutoff, XEN use_dB, XEN min_dB, XEN scale, XEN br, XEN bg, XEN bb)
{
  #define H_glSpectrogram "(" S_glSpectrogram " data gl-list cutoff use-dB min-dB scale br bg bb) takes spectrogram \
data and passes it to openGL.  See snd-gl.scm for an example."
  sono_info *si;
  int i;
  vct *v;

  XEN_ASSERT_TYPE(XEN_VECTOR_P(data), data, XEN_ARG_1, S_glSpectrogram, "a vector of vcts");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(gl_list), gl_list, XEN_ARG_2, S_glSpectrogram, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(cutoff), cutoff, XEN_ARG_3, S_glSpectrogram, "a number");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(use_dB), use_dB, XEN_ARG_4, S_glSpectrogram, "a boolean");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(min_dB), min_dB, XEN_ARG_5, S_glSpectrogram, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scale), scale, XEN_ARG_6, S_glSpectrogram, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(br), br, XEN_ARG_7, S_glSpectrogram, "a number (red value)");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(bg), bg, XEN_ARG_8, S_glSpectrogram, "a number (green value)");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(bb), bb, XEN_ARG_9, S_glSpectrogram, "a number (blue value)");

  si = (sono_info *)calloc(1, sizeof(sono_info));
  si->active_slices = XEN_VECTOR_LENGTH(data);
  si->data = (mus_float_t **)calloc(si->active_slices, sizeof(mus_float_t *));
  v = xen_to_vct(XEN_VECTOR_REF(data, 0));
  si->target_bins = v->length;
  si->scale = XEN_TO_C_DOUBLE(scale);
  for (i = 0; i < si->active_slices; i++)
    {
      v = xen_to_vct(XEN_VECTOR_REF(data, i));
      si->data[i] = v->data;
    }
  gl_spectrogram(si, 
		 XEN_TO_C_INT(gl_list),
		 XEN_TO_C_DOUBLE(cutoff),
		 XEN_TO_C_BOOLEAN(use_dB),
		 XEN_TO_C_DOUBLE(min_dB),
		 FLOAT_TO_RGB(XEN_TO_C_DOUBLE(br)),
		 FLOAT_TO_RGB(XEN_TO_C_DOUBLE(bg)),
		 FLOAT_TO_RGB(XEN_TO_C_DOUBLE(bb)));
  free(si->data);
  free(si);
  return(XEN_FALSE);
}
#endif


static XEN g_channel_data(XEN snd, XEN chn)
{
  #define H_channel_data "(" S_channel_data " :optional snd chn) returns the in-core samples associated with the \
given channel.  Currently, this must be a channel (sound) created by " S_make_variable_graph "."
  chan_info *cp;

  ASSERT_CHANNEL(S_channel_data, snd, chn, 1);

  cp = get_cp(snd, chn, S_channel_data);
  if ((cp) && (cp->sound) && (cp->sound->inuse == SOUND_WRAPPER))
#if SNDLIB_USE_FLOATS
    return(wrap_sound_data(1, cp->edits[0]->samples, &(cp->sounds[0]->buffered_data)));
#else
  {
    /* actually this can't work.  We expect that anything we write to the channel data (sound-data) object
     *   will be displayed in the associated variable graph, but in the int mus_sample_t case, the two
     *   arrays are separate.
     */
    XEN result;
    sound_data *sd;
    mus_long_t i;
    int loc;
    result = make_sound_data(1, cp->edits[0]->samples); /* need actual gc-able object here */
    loc = snd_protect(result);
    sd = XEN_TO_SOUND_DATA(result);
    for (i = 0; i < sd->length; i++)
      sd->data[0][i] = MUS_SAMPLE_TO_DOUBLE(cp->sounds[0]->buffered_data[i]);
    snd_unprotect_at(loc);
    return(result);
  }
#endif
  return(XEN_FALSE);
}


static XEN g_variable_graph_p(XEN index)
{
  #define H_variable_graph_p "(" S_variable_graph_p " :optional snd): " PROC_TRUE " if snd is a variable graph (from " S_make_variable_graph ")."
  snd_info *sp;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(index) || XEN_SOUND_P(index), index, XEN_ONLY_ARG, S_variable_graph_p, "a sound");

  sp = get_sp(index);
  if (sp)
    return(C_TO_XEN_BOOLEAN(sp->inuse == SOUND_WRAPPER));
  return(XEN_FALSE);
}


/* free-variable-graph is much too tricky because callbacks can be invoked after the
 *   controlling snd_info struct has been freed.  In Motif it appears to work to call
 *   XtHasCallbacks/XtRemoveAllCallbacks on all children of the parent widget, but
 *   I didn't try the gtk equivalent -- the whole thing is too messy to be trustworthy.
 */

static XEN g_make_variable_graph(XEN container, XEN name, XEN length, XEN srate)
{
  #define H_make_variable_graph "(" S_make_variable_graph " container :optional name length srate) returns a sound index referring \
to a standard Snd channel graph placed in the widget 'container'."

  snd_info *sp;
  chan_info *cp;
  int rate, initial_length;

  XEN_ASSERT_TYPE(XEN_WIDGET_P(container), container, XEN_ARG_1, S_make_variable_graph, "a widget");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(name), name, XEN_ARG_2, S_make_variable_graph, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(length), length, XEN_ARG_3, S_make_variable_graph, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(srate), srate, XEN_ARG_4, S_make_variable_graph, "an integer");

  rate = XEN_TO_C_INT_OR_ELSE(srate, (int)mus_srate());
  initial_length = XEN_TO_C_INT_OR_ELSE(length, 8192);

  sp = make_simple_channel_display(rate, initial_length, WITH_FW_BUTTONS, graph_style(ss), 
				   (widget_t)(XEN_UNWRAP_WIDGET(container)), WITH_EVENTS);

  if (sp == NULL) /* can only happen if "container" is not a form widget (or perhaps no container XtWindow) */
    XEN_ERROR(XEN_ERROR_TYPE("wrong-type-arg"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_make_variable_graph),
			 C_TO_XEN_STRING("container must be a Form widget with a legitimate window"),
			 container));

  sp->user_read_only = FILE_READ_ONLY;
  sp->index = find_free_sound_slot_for_channel_display();
  ss->sounds[sp->index] = sp;
  cp = sp->chans[0];
  if (XEN_STRING_P(name))
    {
      axis_info *ap;
      sp->filename = mus_strdup(XEN_TO_C_STRING(name));
      sp->short_filename = mus_strdup(sp->filename);
      ap = cp->axis;
      if (ap->xlabel) free(ap->xlabel);
      ap->xlabel = mus_strdup(sp->filename);
      if (ap->default_xlabel) free(ap->default_xlabel); 
      ap->default_xlabel = NULL;
      if (ap->ylabel) free(ap->ylabel);
      ap->ylabel = NULL;
    }
  else
    {
      sp->filename = mus_strdup("variable");
      sp->short_filename = mus_strdup("var");
    }

  cp->sounds[0] = make_snd_data_buffer_for_simple_channel(initial_length);
  cp->edits[0] = initial_ed_list(0, initial_length - 1);
  cp->edits[0]->origin = mus_strdup(S_make_variable_graph);

  return(C_INT_TO_XEN_SOUND(sp->index));
}


static XEN g_zoom_focus_style(void) 
{
  if (zoom_focus_style(ss) != ZOOM_FOCUS_PROC)
    return(C_TO_XEN_INT((int)zoom_focus_style(ss)));
  return(ss->zoom_focus_proc);
}


static XEN g_set_zoom_focus_style(XEN focus) 
{
  #define H_zoom_focus_style "(" S_zoom_focus_style "): one of " S_zoom_focus_left ", " S_zoom_focus_right ", " S_zoom_focus_middle \
", or " S_zoom_focus_active ". This determines what zooming centers on (default: " S_zoom_focus_active ").  It can also \
be a function of 6 args (snd chan zx x0 x1 range) that returns the new window left edge as a float."

  XEN_ASSERT_TYPE((XEN_INTEGER_P(focus)) || (XEN_PROCEDURE_P(focus)), focus, XEN_ONLY_ARG, S_setB S_zoom_focus_style, "an integer or a function");
  if ((XEN_PROCEDURE_P(focus)) && (!(procedure_arity_ok(focus, 6))))
    return(snd_bad_arity_error(S_setB S_zoom_focus_style, 
			       C_TO_XEN_STRING("zoom focus func should take 4 args"), 
			       focus));
  if (zoom_focus_style(ss) == ZOOM_FOCUS_PROC)
    {
      snd_unprotect_at(ss->zoom_focus_proc_loc);
      ss->zoom_focus_proc = XEN_UNDEFINED;
    }
  if (XEN_INTEGER_P(focus))
    {
      int in_choice;
      zoom_focus_t choice;
      in_choice = XEN_TO_C_INT(focus);
      if (in_choice < 0)
	XEN_OUT_OF_RANGE_ERROR(S_setB S_zoom_focus_style, 1, focus, "~A, but must be >= 0");
      choice = (zoom_focus_t)in_choice;
      if (choice > ZOOM_FOCUS_MIDDLE)
	XEN_OUT_OF_RANGE_ERROR(S_setB S_zoom_focus_style, 1, focus, 
			       "~A, but must be " S_zoom_focus_left ", " S_zoom_focus_right ", " S_zoom_focus_middle ", or " S_zoom_focus_active);
      set_zoom_focus_style(choice);
      return(C_TO_XEN_INT((int)zoom_focus_style(ss)));
    }
  set_zoom_focus_style(ZOOM_FOCUS_PROC);
  ss->zoom_focus_proc = focus;
  ss->zoom_focus_proc_loc = snd_protect(focus);
  return(focus);
}


static XEN g_with_gl(void) {return(C_TO_XEN_BOOLEAN(with_gl(ss)));}

static XEN g_set_with_gl(XEN val) 
{
  #define H_with_gl "(" S_with_gl "): " PROC_TRUE " if Snd should use GL graphics"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_gl, "a boolean");
#if HAVE_GL
  set_with_gl(XEN_TO_C_BOOLEAN(val), true);
  for_each_chan(update_graph);
#endif
  return(C_TO_XEN_BOOLEAN(with_gl(ss)));
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_variable_graph_p_w, g_variable_graph_p)
XEN_ARGIFY_4(g_make_variable_graph_w, g_make_variable_graph)
XEN_ARGIFY_2(g_channel_data_w, g_channel_data)

XEN_ARGIFY_10(g_graph_w, g_graph)
XEN_ARGIFY_2(g_edits_w, g_edits)
XEN_ARGIFY_3(g_peaks_w, g_peaks)
XEN_ARGIFY_2(g_edit_hook_w, g_edit_hook)
XEN_ARGIFY_2(g_after_edit_hook_w, g_after_edit_hook)
XEN_ARGIFY_2(g_undo_hook_w, g_undo_hook)
XEN_ARGIFY_2(g_ap_sx_w, g_ap_sx)
XEN_ARGIFY_3(g_set_ap_sx_w, g_set_ap_sx)
XEN_ARGIFY_2(g_ap_sy_w, g_ap_sy)
XEN_ARGIFY_3(g_set_ap_sy_w, g_set_ap_sy)
XEN_ARGIFY_2(g_ap_zx_w, g_ap_zx)
XEN_ARGIFY_3(g_set_ap_zx_w, g_set_ap_zx)
XEN_ARGIFY_2(g_ap_zy_w, g_ap_zy)
XEN_ARGIFY_3(g_set_ap_zy_w, g_set_ap_zy)
XEN_ARGIFY_3(g_frames_w, g_frames)
XEN_ARGIFY_3(g_set_frames_w, g_set_frames)
XEN_ARGIFY_3(g_maxamp_position_w, g_maxamp_position)
XEN_ARGIFY_3(g_maxamp_w, g_maxamp)
XEN_ARGIFY_3(g_set_maxamp_w, g_set_maxamp)
XEN_ARGIFY_2(g_cursor_position_w, g_cursor_position)
XEN_ARGIFY_2(g_edit_position_w, g_edit_position)
XEN_ARGIFY_3(g_set_edit_position_w, g_set_edit_position)
XEN_ARGIFY_2(g_transform_graph_p_w, g_transform_graph_p)
XEN_ARGIFY_3(g_set_transform_graph_p_w, g_set_transform_graph_p)
XEN_ARGIFY_2(g_time_graph_p_w, g_time_graph_p)
XEN_ARGIFY_3(g_set_time_graph_p_w, g_set_time_graph_p)
XEN_ARGIFY_2(g_lisp_graph_p_w, g_lisp_graph_p)
XEN_ARGIFY_3(g_set_lisp_graph_p_w, g_set_lisp_graph_p)
XEN_ARGIFY_2(g_squelch_update_w, g_squelch_update)
XEN_ARGIFY_3(g_set_squelch_update_w, g_set_squelch_update)
XEN_ARGIFY_3(g_cursor_w, g_cursor)
XEN_ARGIFY_4(g_set_cursor_w, g_set_cursor)
XEN_ARGIFY_2(g_cursor_style_w, g_cursor_style)
XEN_ARGIFY_3(g_set_cursor_style_w, g_set_cursor_style)
XEN_ARGIFY_2(g_tracking_cursor_style_w, g_tracking_cursor_style)
XEN_ARGIFY_3(g_set_tracking_cursor_style_w, g_set_tracking_cursor_style)
XEN_ARGIFY_2(g_cursor_size_w, g_cursor_size)
XEN_ARGIFY_3(g_set_cursor_size_w, g_set_cursor_size)
XEN_ARGIFY_2(g_left_sample_w, g_left_sample)
XEN_ARGIFY_3(g_set_left_sample_w, g_set_left_sample)
XEN_ARGIFY_2(g_right_sample_w, g_right_sample)
XEN_ARGIFY_3(g_set_right_sample_w, g_set_right_sample)
XEN_ARGIFY_2(g_channel_properties_w, g_channel_properties)
XEN_ARGIFY_3(g_set_channel_properties_w, g_set_channel_properties)
XEN_ARGIFY_3(g_edit_properties_w, g_edit_properties)
XEN_ARGIFY_4(g_set_edit_properties_w, g_set_edit_properties)
XEN_ARGIFY_2(g_max_transform_peaks_w, g_max_transform_peaks)
XEN_ARGIFY_3(g_set_max_transform_peaks_w, g_set_max_transform_peaks)
XEN_ARGIFY_2(g_show_y_zero_w, g_show_y_zero)
XEN_ARGIFY_3(g_set_show_y_zero_w, g_set_show_y_zero)
XEN_ARGIFY_2(g_show_grid_w, g_show_grid)
XEN_ARGIFY_3(g_set_show_grid_w, g_set_show_grid)
XEN_ARGIFY_2(g_grid_density_w, g_grid_density)
XEN_ARGIFY_3(g_set_grid_density_w, g_set_grid_density)
XEN_ARGIFY_2(g_show_sonogram_cursor_w, g_show_sonogram_cursor)
XEN_ARGIFY_3(g_set_show_sonogram_cursor_w, g_set_show_sonogram_cursor)
XEN_ARGIFY_2(g_show_marks_w, g_show_marks)
XEN_ARGIFY_3(g_set_show_marks_w, g_set_show_marks)
XEN_ARGIFY_2(g_time_graph_type_w, g_time_graph_type)
XEN_ARGIFY_3(g_set_time_graph_type_w, g_set_time_graph_type)
XEN_ARGIFY_2(g_wavo_hop_w, g_wavo_hop)
XEN_ARGIFY_3(g_set_wavo_hop_w, g_set_wavo_hop)
XEN_ARGIFY_2(g_wavo_trace_w, g_wavo_trace)
XEN_ARGIFY_3(g_set_wavo_trace_w, g_set_wavo_trace)
XEN_ARGIFY_2(g_show_transform_peaks_w, g_show_transform_peaks)
XEN_ARGIFY_3(g_set_show_transform_peaks_w, g_set_show_transform_peaks)
XEN_ARGIFY_2(g_zero_pad_w, g_zero_pad)
XEN_ARGIFY_3(g_set_zero_pad_w, g_set_zero_pad)
XEN_ARGIFY_2(g_verbose_cursor_w, g_verbose_cursor)
XEN_ARGIFY_3(g_set_verbose_cursor_w, g_set_verbose_cursor)
XEN_ARGIFY_2(g_fft_log_frequency_w, g_fft_log_frequency)
XEN_ARGIFY_3(g_set_fft_log_frequency_w, g_set_fft_log_frequency)
XEN_ARGIFY_2(g_fft_log_magnitude_w, g_fft_log_magnitude)
XEN_ARGIFY_3(g_set_fft_log_magnitude_w, g_set_fft_log_magnitude)
XEN_ARGIFY_2(g_fft_with_phases_w, g_fft_with_phases)
XEN_ARGIFY_3(g_set_fft_with_phases_w, g_set_fft_with_phases)
XEN_ARGIFY_2(g_min_dB_w, g_min_dB)
XEN_ARGIFY_3(g_set_min_dB_w, g_set_min_dB)
XEN_ARGIFY_2(g_wavelet_type_w, g_wavelet_type)
XEN_ARGIFY_3(g_set_wavelet_type_w, g_set_wavelet_type)
XEN_ARGIFY_2(g_spectrum_end_w, g_spectrum_end)
XEN_ARGIFY_3(g_set_spectrum_end_w, g_set_spectrum_end)
XEN_ARGIFY_2(g_spectrum_start_w, g_spectrum_start)
XEN_ARGIFY_3(g_set_spectrum_start_w, g_set_spectrum_start)
XEN_ARGIFY_2(g_spectro_x_angle_w, g_spectro_x_angle)
XEN_ARGIFY_3(g_set_spectro_x_angle_w, g_set_spectro_x_angle)
XEN_ARGIFY_2(g_spectro_x_scale_w, g_spectro_x_scale)
XEN_ARGIFY_3(g_set_spectro_x_scale_w, g_set_spectro_x_scale)
XEN_ARGIFY_2(g_spectro_y_angle_w, g_spectro_y_angle)
XEN_ARGIFY_3(g_set_spectro_y_angle_w, g_set_spectro_y_angle)
XEN_ARGIFY_2(g_spectro_y_scale_w, g_spectro_y_scale)
XEN_ARGIFY_3(g_set_spectro_y_scale_w, g_set_spectro_y_scale)
XEN_ARGIFY_2(g_spectro_z_angle_w, g_spectro_z_angle)
XEN_ARGIFY_3(g_set_spectro_z_angle_w, g_set_spectro_z_angle)
XEN_ARGIFY_2(g_spectro_z_scale_w, g_spectro_z_scale)
XEN_ARGIFY_3(g_set_spectro_z_scale_w, g_set_spectro_z_scale)
XEN_ARGIFY_2(g_fft_window_alpha_w, g_fft_window_alpha)
XEN_ARGIFY_3(g_set_fft_window_alpha_w, g_set_fft_window_alpha)
XEN_ARGIFY_2(g_fft_window_beta_w, g_fft_window_beta)
XEN_ARGIFY_3(g_set_fft_window_beta_w, g_set_fft_window_beta)
XEN_ARGIFY_2(g_spectro_hop_w, g_spectro_hop)
XEN_ARGIFY_3(g_set_spectro_hop_w, g_set_spectro_hop)
XEN_ARGIFY_2(g_transform_size_w, g_transform_size)
XEN_ARGIFY_3(g_set_transform_size_w, g_set_transform_size)
XEN_ARGIFY_2(g_transform_graph_type_w, g_transform_graph_type)
XEN_ARGIFY_3(g_set_transform_graph_type_w, g_set_transform_graph_type)
XEN_ARGIFY_2(g_fft_window_w, g_fft_window)
XEN_ARGIFY_3(g_set_fft_window_w, g_set_fft_window)
XEN_ARGIFY_2(g_transform_type_w, g_transform_type)
XEN_ARGIFY_3(g_set_transform_type_w, g_set_transform_type)
XEN_ARGIFY_2(g_transform_normalization_w, g_transform_normalization)
XEN_ARGIFY_3(g_set_transform_normalization_w, g_set_transform_normalization)
XEN_ARGIFY_2(g_show_mix_waveforms_w, g_show_mix_waveforms)
XEN_ARGIFY_3(g_set_show_mix_waveforms_w, g_set_show_mix_waveforms)
XEN_ARGIFY_2(g_graph_style_w, g_graph_style)
XEN_ARGIFY_3(g_set_graph_style_w, g_set_graph_style)
XEN_ARGIFY_2(g_time_graph_style_w, g_time_graph_style)
XEN_ARGIFY_3(g_set_time_graph_style_w, g_set_time_graph_style)
XEN_ARGIFY_2(g_lisp_graph_style_w, g_lisp_graph_style)
XEN_ARGIFY_3(g_set_lisp_graph_style_w, g_set_lisp_graph_style)
XEN_ARGIFY_2(g_transform_graph_style_w, g_transform_graph_style)
XEN_ARGIFY_3(g_set_transform_graph_style_w, g_set_transform_graph_style)
XEN_ARGIFY_2(g_dot_size_w, g_dot_size)
XEN_ARGIFY_3(g_set_dot_size_w, g_set_dot_size)
XEN_ARGIFY_2(g_x_axis_style_w, g_x_axis_style)
XEN_ARGIFY_3(g_set_x_axis_style_w, g_set_x_axis_style)
XEN_ARGIFY_2(g_beats_per_measure_w, g_beats_per_measure)
XEN_ARGIFY_3(g_set_beats_per_measure_w, g_set_beats_per_measure)
XEN_ARGIFY_2(g_beats_per_minute_w, g_beats_per_minute)
XEN_ARGIFY_3(g_set_beats_per_minute_w, g_set_beats_per_minute)
XEN_ARGIFY_2(g_show_axes_w, g_show_axes)
XEN_ARGIFY_3(g_set_show_axes_w, g_set_show_axes)
XEN_ARGIFY_2(g_graphs_horizontal_w, g_graphs_horizontal)
XEN_ARGIFY_3(g_set_graphs_horizontal_w, g_set_graphs_horizontal)
XEN_ARGIFY_2(g_update_time_graph_w, g_update_time_graph)
XEN_ARGIFY_2(g_update_lisp_graph_w, g_update_lisp_graph)
XEN_ARGIFY_2(g_update_transform_graph_w, g_update_transform_graph)
XEN_NARGIFY_0(g_zoom_focus_style_w, g_zoom_focus_style)
XEN_NARGIFY_1(g_set_zoom_focus_style_w, g_set_zoom_focus_style)
XEN_NARGIFY_0(g_with_gl_w, g_with_gl)
XEN_NARGIFY_1(g_set_with_gl_w, g_set_with_gl)
#if HAVE_GL
  XEN_NARGIFY_9(g_gl_spectrogram_w, g_gl_spectrogram)
#endif
#else
#define g_variable_graph_p_w g_variable_graph_p
#define g_make_variable_graph_w g_make_variable_graph
#define g_channel_data_w g_channel_data

#define g_graph_w g_graph
#define g_edits_w g_edits
#define g_peaks_w g_peaks
#define g_edit_hook_w g_edit_hook
#define g_after_edit_hook_w g_after_edit_hook
#define g_undo_hook_w g_undo_hook
#define g_ap_sx_w g_ap_sx
#define g_set_ap_sx_w g_set_ap_sx
#define g_ap_sy_w g_ap_sy
#define g_set_ap_sy_w g_set_ap_sy
#define g_ap_zx_w g_ap_zx
#define g_set_ap_zx_w g_set_ap_zx
#define g_ap_zy_w g_ap_zy
#define g_set_ap_zy_w g_set_ap_zy
#define g_frames_w g_frames
#define g_set_frames_w g_set_frames
#define g_maxamp_position_w g_maxamp_position
#define g_maxamp_w g_maxamp
#define g_set_maxamp_w g_set_maxamp
#define g_cursor_position_w g_cursor_position
#define g_edit_position_w g_edit_position
#define g_set_edit_position_w g_set_edit_position
#define g_transform_graph_p_w g_transform_graph_p
#define g_set_transform_graph_p_w g_set_transform_graph_p
#define g_time_graph_p_w g_time_graph_p
#define g_set_time_graph_p_w g_set_time_graph_p
#define g_lisp_graph_p_w g_lisp_graph_p
#define g_set_lisp_graph_p_w g_set_lisp_graph_p
#define g_squelch_update_w g_squelch_update
#define g_set_squelch_update_w g_set_squelch_update
#define g_cursor_w g_cursor
#define g_set_cursor_w g_set_cursor
#define g_cursor_style_w g_cursor_style
#define g_set_cursor_style_w g_set_cursor_style
#define g_tracking_cursor_style_w g_tracking_cursor_style
#define g_set_tracking_cursor_style_w g_set_tracking_cursor_style
#define g_cursor_size_w g_cursor_size
#define g_set_cursor_size_w g_set_cursor_size
#define g_left_sample_w g_left_sample
#define g_set_left_sample_w g_set_left_sample
#define g_right_sample_w g_right_sample
#define g_set_right_sample_w g_set_right_sample
#define g_channel_properties_w g_channel_properties
#define g_set_channel_properties_w g_set_channel_properties
#define g_edit_properties_w g_edit_properties
#define g_set_edit_properties_w g_set_edit_properties
#define g_max_transform_peaks_w g_max_transform_peaks
#define g_set_max_transform_peaks_w g_set_max_transform_peaks
#define g_show_y_zero_w g_show_y_zero
#define g_set_show_y_zero_w g_set_show_y_zero
#define g_show_grid_w g_show_grid
#define g_set_show_grid_w g_set_show_grid
#define g_grid_density_w g_grid_density
#define g_set_grid_density_w g_set_grid_density
#define g_show_sonogram_cursor_w g_show_sonogram_cursor
#define g_set_show_sonogram_cursor_w g_set_show_sonogram_cursor
#define g_show_marks_w g_show_marks
#define g_set_show_marks_w g_set_show_marks
#define g_time_graph_type_w g_time_graph_type
#define g_set_time_graph_type_w g_set_time_graph_type
#define g_wavo_hop_w g_wavo_hop
#define g_set_wavo_hop_w g_set_wavo_hop
#define g_wavo_trace_w g_wavo_trace
#define g_set_wavo_trace_w g_set_wavo_trace
#define g_show_transform_peaks_w g_show_transform_peaks
#define g_set_show_transform_peaks_w g_set_show_transform_peaks
#define g_zero_pad_w g_zero_pad
#define g_set_zero_pad_w g_set_zero_pad
#define g_verbose_cursor_w g_verbose_cursor
#define g_set_verbose_cursor_w g_set_verbose_cursor
#define g_fft_log_frequency_w g_fft_log_frequency
#define g_set_fft_log_frequency_w g_set_fft_log_frequency
#define g_fft_log_magnitude_w g_fft_log_magnitude
#define g_set_fft_log_magnitude_w g_set_fft_log_magnitude
#define g_fft_with_phases_w g_fft_with_phases
#define g_set_fft_with_phases_w g_set_fft_with_phases
#define g_min_dB_w g_min_dB
#define g_set_min_dB_w g_set_min_dB
#define g_wavelet_type_w g_wavelet_type
#define g_set_wavelet_type_w g_set_wavelet_type
#define g_spectrum_end_w g_spectrum_end
#define g_set_spectrum_end_w g_set_spectrum_end
#define g_spectrum_start_w g_spectrum_start
#define g_set_spectrum_start_w g_set_spectrum_start
#define g_spectro_x_angle_w g_spectro_x_angle
#define g_set_spectro_x_angle_w g_set_spectro_x_angle
#define g_spectro_x_scale_w g_spectro_x_scale
#define g_set_spectro_x_scale_w g_set_spectro_x_scale
#define g_spectro_y_angle_w g_spectro_y_angle
#define g_set_spectro_y_angle_w g_set_spectro_y_angle
#define g_spectro_y_scale_w g_spectro_y_scale
#define g_set_spectro_y_scale_w g_set_spectro_y_scale
#define g_spectro_z_angle_w g_spectro_z_angle
#define g_set_spectro_z_angle_w g_set_spectro_z_angle
#define g_spectro_z_scale_w g_spectro_z_scale
#define g_set_spectro_z_scale_w g_set_spectro_z_scale
#define g_fft_window_beta_w g_fft_window_beta
#define g_set_fft_window_beta_w g_set_fft_window_beta
#define g_fft_window_alpha_w g_fft_window_alpha
#define g_set_fft_window_alpha_w g_set_fft_window_alpha
#define g_spectro_hop_w g_spectro_hop
#define g_set_spectro_hop_w g_set_spectro_hop
#define g_transform_size_w g_transform_size
#define g_set_transform_size_w g_set_transform_size
#define g_transform_graph_type_w g_transform_graph_type
#define g_set_transform_graph_type_w g_set_transform_graph_type
#define g_fft_window_w g_fft_window
#define g_set_fft_window_w g_set_fft_window
#define g_transform_type_w g_transform_type
#define g_set_transform_type_w g_set_transform_type
#define g_transform_normalization_w g_transform_normalization
#define g_set_transform_normalization_w g_set_transform_normalization
#define g_show_mix_waveforms_w g_show_mix_waveforms
#define g_set_show_mix_waveforms_w g_set_show_mix_waveforms
#define g_graph_style_w g_graph_style
#define g_set_graph_style_w g_set_graph_style
#define g_time_graph_style_w g_time_graph_style
#define g_set_time_graph_style_w g_set_time_graph_style
#define g_lisp_graph_style_w g_lisp_graph_style
#define g_set_lisp_graph_style_w g_set_lisp_graph_style
#define g_transform_graph_style_w g_transform_graph_style
#define g_set_transform_graph_style_w g_set_transform_graph_style
#define g_dot_size_w g_dot_size
#define g_set_dot_size_w g_set_dot_size
#define g_x_axis_style_w g_x_axis_style
#define g_set_x_axis_style_w g_set_x_axis_style
#define g_beats_per_measure_w g_beats_per_measure
#define g_set_beats_per_measure_w g_set_beats_per_measure
#define g_beats_per_minute_w g_beats_per_minute
#define g_set_beats_per_minute_w g_set_beats_per_minute
#define g_show_axes_w g_show_axes
#define g_set_show_axes_w g_set_show_axes
#define g_graphs_horizontal_w g_graphs_horizontal
#define g_set_graphs_horizontal_w g_set_graphs_horizontal
#define g_update_time_graph_w g_update_time_graph
#define g_update_lisp_graph_w g_update_lisp_graph
#define g_update_transform_graph_w g_update_transform_graph
#define g_zoom_focus_style_w g_zoom_focus_style
#define g_set_zoom_focus_style_w g_set_zoom_focus_style
#define g_with_gl_w g_with_gl
#define g_set_with_gl_w g_set_with_gl
#if HAVE_GL
  #define g_gl_spectrogram_w g_gl_spectrogram
#endif
#endif


void g_init_chn(void)
{
  cp_edpos = XEN_UNDEFINED;

  XEN_DEFINE_PROCEDURE(S_variable_graph_p,        g_variable_graph_p_w,       1, 0, 0, H_variable_graph_p);
  XEN_DEFINE_PROCEDURE(S_make_variable_graph,     g_make_variable_graph_w,    1, 3, 0, H_make_variable_graph);
  XEN_DEFINE_PROCEDURE(S_channel_data,            g_channel_data_w,           0, 2, 0, H_channel_data);

  XEN_DEFINE_PROCEDURE(S_graph,                   g_graph_w,                  1, 9, 0, H_graph);
  XEN_DEFINE_PROCEDURE(S_edits,                   g_edits_w,                  0, 2, 0, H_edits);
  XEN_DEFINE_PROCEDURE(S_peaks,                   g_peaks_w,                  0, 3, 0, H_peaks);
  XEN_DEFINE_PROCEDURE(S_edit_hook,               g_edit_hook_w,              0, 2, 0, H_edit_hook);
  XEN_DEFINE_PROCEDURE(S_after_edit_hook,         g_after_edit_hook_w,        0, 2, 0, H_after_edit_hook);
  XEN_DEFINE_PROCEDURE(S_undo_hook,               g_undo_hook_w,              0, 2, 0, H_undo_hook);
  XEN_DEFINE_PROCEDURE(S_update_time_graph,       g_update_time_graph_w,      0, 2, 0, H_update_time_graph);
  XEN_DEFINE_PROCEDURE(S_update_lisp_graph,       g_update_lisp_graph_w,      0, 2, 0, H_update_lisp_graph);
  XEN_DEFINE_PROCEDURE(S_update_transform_graph,  g_update_transform_graph_w, 0, 2, 0, H_update_transform_graph);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_x_position_slider, g_ap_sx_w, H_x_position_slider,
					    S_setB S_x_position_slider, g_set_ap_sx_w, g_set_ap_sx_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_y_position_slider, g_ap_sy_w, H_y_position_slider,
					    S_setB S_y_position_slider, g_set_ap_sy_w, g_set_ap_sy_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_x_zoom_slider, g_ap_zx_w, H_x_zoom_slider,
					    S_setB S_x_zoom_slider, g_set_ap_zx_w, g_set_ap_zx_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_y_zoom_slider, g_ap_zy_w, H_y_zoom_slider,
					    S_setB S_y_zoom_slider, g_set_ap_zy_w, g_set_ap_zy_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_frames, g_frames_w, H_frames,
					    S_setB S_frames, g_set_frames_w, g_set_frames_reversed, 0, 3, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_maxamp, g_maxamp_w, H_maxamp,
					    S_setB S_maxamp, g_set_maxamp_w, g_set_maxamp_reversed, 0, 3, 1, 2);
  
  XEN_DEFINE_PROCEDURE(S_maxamp_position,   g_maxamp_position_w, 0, 3, 0,   H_maxamp_position);
  XEN_DEFINE_PROCEDURE(S_cursor_position,   g_cursor_position_w, 0, 2, 0,   H_cursor_position);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_edit_position, g_edit_position_w, H_edit_position,
					    S_setB S_edit_position, g_set_edit_position_w, g_set_edit_position_reversed, 0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_graph_p, g_transform_graph_p_w, H_transform_graph_p,
					    S_setB S_transform_graph_p, g_set_transform_graph_p_w, g_set_transform_graph_p_reversed, 0, 2, 1, 2);

  #define H_graph_once "The value for the various graph-type variables that displays the standard waveform"
  #define H_graph_as_wavogram "The value for " S_time_graph_type " to make a spectrogram-like form of the time-domain data"

  XEN_DEFINE_CONSTANT(S_graph_once,        GRAPH_ONCE,        H_graph_once);
  XEN_DEFINE_CONSTANT(S_graph_as_wavogram, GRAPH_AS_WAVOGRAM, H_graph_as_wavogram);

  #define H_graph_as_sonogram "The value for " S_transform_graph_type " that causes a sonogram to be displayed"
  #define H_graph_as_spectrogram "The value for " S_transform_graph_type " that causes a spectrogram to be displayed"
  /* #define H_graph_as_complex "The value for " S_transform_graph_type " to display FFT data in the complex plane" */

  XEN_DEFINE_CONSTANT(S_graph_as_sonogram,    GRAPH_AS_SONOGRAM,    H_graph_as_sonogram);
  XEN_DEFINE_CONSTANT(S_graph_as_spectrogram, GRAPH_AS_SPECTROGRAM, H_graph_as_spectrogram);
  /* XEN_DEFINE_CONSTANT(S_graph_as_complex,     GRAPH_AS_COMPLEX,     H_graph_as_complex); */

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_time_graph_p, g_time_graph_p_w, H_time_graph_p,
					    S_setB S_time_graph_p, g_set_time_graph_p_w, g_set_time_graph_p_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_lisp_graph_p, g_lisp_graph_p_w, H_lisp_graph_p,
					    S_setB S_lisp_graph_p, g_set_lisp_graph_p_w, g_set_lisp_graph_p_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_squelch_update, g_squelch_update_w, H_squelch_update,
					    S_setB S_squelch_update, g_set_squelch_update_w, g_set_squelch_update_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_cursor, g_cursor_w, H_cursor,
					    S_setB S_cursor, g_set_cursor_w, g_set_cursor_reversed, 0, 3, 1, 3);
  
  #define H_cursor_cross "The value for " S_cursor_style " that causes is to be a cross (the default)"
  #define H_cursor_line "The value for " S_cursor_style " that causes is to be a full vertical line"

  XEN_DEFINE_CONSTANT(S_cursor_cross,          CURSOR_CROSS, H_cursor_cross);
  XEN_DEFINE_CONSTANT(S_cursor_line,           CURSOR_LINE,  H_cursor_line);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_cursor_style, g_cursor_style_w, H_cursor_style,
					    S_setB S_cursor_style, g_set_cursor_style_w, g_set_cursor_style_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_tracking_cursor_style, g_tracking_cursor_style_w, H_tracking_cursor_style,
					    S_setB S_tracking_cursor_style, g_set_tracking_cursor_style_w, g_set_tracking_cursor_style_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_cursor_size, g_cursor_size_w, H_cursor_size,
					    S_setB S_cursor_size, g_set_cursor_size_w, g_set_cursor_size_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_left_sample, g_left_sample_w, H_left_sample,
					    S_setB S_left_sample, g_set_left_sample_w, g_set_left_sample_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_right_sample, g_right_sample_w, H_right_sample,
					    S_setB S_right_sample, g_set_right_sample_w, g_set_right_sample_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_channel_properties, g_channel_properties_w, H_channel_properties,
					    S_setB S_channel_properties, g_set_channel_properties_w, g_set_channel_properties_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_edit_properties, g_edit_properties_w, H_edit_properties,
					    S_setB S_edit_properties, g_set_edit_properties_w, g_set_edit_properties_reversed, 0, 3, 1, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_max_transform_peaks, g_max_transform_peaks_w, H_max_transform_peaks,
					    S_setB S_max_transform_peaks, g_set_max_transform_peaks_w, g_set_max_transform_peaks_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_y_zero, g_show_y_zero_w, H_show_y_zero,
					    S_setB S_show_y_zero, g_set_show_y_zero_w, g_set_show_y_zero_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_grid, g_show_grid_w, H_show_grid,
					    S_setB S_show_grid, g_set_show_grid_w, g_set_show_grid_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_grid_density, g_grid_density_w, H_grid_density,
					    S_setB S_grid_density, g_set_grid_density_w, g_set_grid_density_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_sonogram_cursor, g_show_sonogram_cursor_w, H_show_sonogram_cursor,
					    S_setB S_show_sonogram_cursor, g_set_show_sonogram_cursor_w, g_set_show_sonogram_cursor_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_marks, g_show_marks_w, H_show_marks,
					    S_setB S_show_marks, g_set_show_marks_w, g_set_show_marks_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_time_graph_type, g_time_graph_type_w, H_time_graph_type,
					    S_setB S_time_graph_type, g_set_time_graph_type_w, g_set_time_graph_type_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_wavo_hop, g_wavo_hop_w, H_wavo_hop,
					    S_setB S_wavo_hop, g_set_wavo_hop_w, g_set_wavo_hop_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_wavo_trace, g_wavo_trace_w, H_wavo_trace,
					    S_setB S_wavo_trace, g_set_wavo_trace_w, g_set_wavo_trace_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_transform_peaks, g_show_transform_peaks_w, H_show_transform_peaks,
					    S_setB S_show_transform_peaks, g_set_show_transform_peaks_w, g_set_show_transform_peaks_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_zero_pad, g_zero_pad_w, H_zero_pad,
					    S_setB S_zero_pad, g_set_zero_pad_w, g_set_zero_pad_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER("verbose-cursor", g_verbose_cursor_w, H_verbose_cursor,
					    S_setB "verbose-cursor", g_set_verbose_cursor_w, g_set_verbose_cursor_reversed, 0, 2, 1, 2);
  
  /* a synonym for now */
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_with_verbose_cursor, g_verbose_cursor_w, H_verbose_cursor,
					    S_setB S_with_verbose_cursor, g_set_verbose_cursor_w, g_set_verbose_cursor_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_log_frequency, g_fft_log_frequency_w, H_fft_log_frequency,
					    S_setB S_fft_log_frequency, g_set_fft_log_frequency_w, g_set_fft_log_frequency_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_log_magnitude, g_fft_log_magnitude_w, H_fft_log_magnitude,
					    S_setB S_fft_log_magnitude, g_set_fft_log_magnitude_w, g_set_fft_log_magnitude_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_with_phases, g_fft_with_phases_w, H_fft_with_phases,
					    S_setB S_fft_with_phases, g_set_fft_with_phases_w, g_set_fft_with_phases_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_min_dB, g_min_dB_w, H_min_dB,
					    S_setB S_min_dB, g_set_min_dB_w, g_set_min_dB_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_wavelet_type, g_wavelet_type_w, H_wavelet_type,
					    S_setB S_wavelet_type, g_set_wavelet_type_w, g_set_wavelet_type_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectrum_end, g_spectrum_end_w, H_spectrum_end,
					    S_setB S_spectrum_end, g_set_spectrum_end_w, g_set_spectrum_end_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectrum_start, g_spectrum_start_w, H_spectrum_start,
					    S_setB S_spectrum_start, g_set_spectrum_start_w, g_set_spectrum_start_reversed, 0, 2, 1, 2);
  
  /* for backwards compatibility */
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER("spectro-cutoff", g_spectrum_end_w, H_spectrum_end,
					    S_setB "spectro-cutoff", g_set_spectrum_end_w, g_set_spectrum_end_reversed, 0, 2, 1, 2);
  /* for backwards compatibility */  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER("spectro-start", g_spectrum_start_w, H_spectrum_start,
					    S_setB "spectro-start", g_set_spectrum_start_w, g_set_spectrum_start_reversed, 0, 2, 1, 2);
  

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_x_angle, g_spectro_x_angle_w, H_spectro_x_angle,
					    S_setB S_spectro_x_angle, g_set_spectro_x_angle_w, g_set_spectro_x_angle_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_x_scale, g_spectro_x_scale_w, H_spectro_x_scale,
					    S_setB S_spectro_x_scale, g_set_spectro_x_scale_w, g_set_spectro_x_scale_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_y_angle, g_spectro_y_angle_w, H_spectro_y_angle,
					    S_setB S_spectro_y_angle, g_set_spectro_y_angle_w, g_set_spectro_y_angle_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_y_scale, g_spectro_y_scale_w, H_spectro_y_scale,
					    S_setB S_spectro_y_scale, g_set_spectro_y_scale_w, g_set_spectro_y_scale_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_z_angle, g_spectro_z_angle_w, H_spectro_z_angle,
					    S_setB S_spectro_z_angle, g_set_spectro_z_angle_w, g_set_spectro_z_angle_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_z_scale, g_spectro_z_scale_w, H_spectro_z_scale,
					    S_setB S_spectro_z_scale, g_set_spectro_z_scale_w, g_set_spectro_z_scale_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_window_beta, g_fft_window_beta_w, H_fft_window_beta,
					    S_setB S_fft_window_beta, g_set_fft_window_beta_w, g_set_fft_window_beta_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_window_alpha, g_fft_window_alpha_w, H_fft_window_alpha,
					    S_setB S_fft_window_alpha, g_set_fft_window_alpha_w, g_set_fft_window_alpha_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_hop, g_spectro_hop_w, H_spectro_hop,
					    S_setB S_spectro_hop, g_set_spectro_hop_w, g_set_spectro_hop_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_size, g_transform_size_w, H_transform_size,
					    S_setB S_transform_size, g_set_transform_size_w, g_set_transform_size_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_graph_type, g_transform_graph_type_w, H_transform_graph_type,
					    S_setB S_transform_graph_type, g_set_transform_graph_type_w, g_set_transform_graph_type_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_window, g_fft_window_w, H_fft_window,
					    S_setB S_fft_window, g_set_fft_window_w, g_set_fft_window_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_type, g_transform_type_w, H_transform_type,
					    S_setB S_transform_type, g_set_transform_type_w, g_set_transform_type_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_normalization, g_transform_normalization_w, H_transform_normalization,
					    S_setB S_transform_normalization, g_set_transform_normalization_w, g_set_transform_normalization_reversed, 0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_mix_waveforms, g_show_mix_waveforms_w, H_show_mix_waveforms,
					    S_setB S_show_mix_waveforms, g_set_show_mix_waveforms_w, g_set_show_mix_waveforms_reversed, 0, 2, 1, 2);
  
  /* should these be named "graph-with-lines" etc? */
  #define H_graph_lines "The value for " S_graph_style " that causes graphs to use line-segments"
  #define H_graph_dots "The value for " S_graph_style " that causes graphs to use dots"
  #define H_graph_filled "The value for " S_graph_style " that causes graphs to use filled polygons"
  #define H_graph_dots_and_lines "The value for " S_graph_style " that causes graphs to use dots connected by lines"
  #define H_graph_lollipops "The value for " S_graph_style " that makes DSP engineers happy"
  
  XEN_DEFINE_CONSTANT(S_graph_lines,           GRAPH_LINES,          H_graph_lines);
  XEN_DEFINE_CONSTANT(S_graph_dots,            GRAPH_DOTS,           H_graph_dots);
  XEN_DEFINE_CONSTANT(S_graph_filled,          GRAPH_FILLED,         H_graph_filled);
  XEN_DEFINE_CONSTANT(S_graph_dots_and_lines,  GRAPH_DOTS_AND_LINES, H_graph_dots_and_lines);
  XEN_DEFINE_CONSTANT(S_graph_lollipops,       GRAPH_LOLLIPOPS,      H_graph_lollipops);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_time_graph_style, g_time_graph_style_w, H_time_graph_style,
					    S_setB S_time_graph_style, g_set_time_graph_style_w, g_set_time_graph_style_reversed,
					    1, 1, 1, 2);
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_lisp_graph_style, g_lisp_graph_style_w, H_lisp_graph_style,
					    S_setB S_lisp_graph_style, g_set_lisp_graph_style_w, g_set_lisp_graph_style_reversed,
					    1, 1, 1, 2);
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_graph_style, g_transform_graph_style_w, H_transform_graph_style,
					    S_setB S_transform_graph_style, g_set_transform_graph_style_w, g_set_transform_graph_style_reversed,
					    1, 1, 1, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_graph_style, g_graph_style_w, H_graph_style,
					    S_setB S_graph_style, g_set_graph_style_w, g_set_graph_style_reversed,
					    0, 2, 1, 2);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_dot_size, g_dot_size_w, H_dot_size,
					    S_setB S_dot_size, g_set_dot_size_w, g_set_dot_size_reversed,
					    0, 2, 1, 2);

  #define H_x_axis_in_seconds    "The value for " S_x_axis_style " that displays the x axis using seconds"
  #define H_x_axis_in_samples    "The value for " S_x_axis_style " that displays the x axis using sample numbers"
  #define H_x_axis_in_beats      "The value for " S_x_axis_style " that displays the x axis using beats (also " S_beats_per_minute ")"
  #define H_x_axis_in_measures   "The value for " S_x_axis_style " that displays the x axis using measure numbers"
  #define H_x_axis_as_percentage "The value for " S_x_axis_style " that displays the x axis using percentages"
  #define H_x_axis_as_clock      "The value for " S_x_axis_style " that displays the x axis using clock-like DD:HH:MM:SS syntax"

  XEN_DEFINE_CONSTANT(S_x_axis_in_seconds,     X_AXIS_IN_SECONDS,    H_x_axis_in_seconds);
  XEN_DEFINE_CONSTANT(S_x_axis_in_samples,     X_AXIS_IN_SAMPLES,    H_x_axis_in_samples);
  XEN_DEFINE_CONSTANT(S_x_axis_in_beats,       X_AXIS_IN_BEATS,      H_x_axis_in_beats);
  XEN_DEFINE_CONSTANT(S_x_axis_in_measures,    X_AXIS_IN_MEASURES,   H_x_axis_in_measures);
  XEN_DEFINE_CONSTANT(S_x_axis_as_percentage,  X_AXIS_AS_PERCENTAGE, H_x_axis_as_percentage);
  XEN_DEFINE_CONSTANT(S_x_axis_as_clock,       X_AXIS_AS_CLOCK,      H_x_axis_as_clock);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_x_axis_style, g_x_axis_style_w, H_x_axis_style,
					    S_setB S_x_axis_style, g_set_x_axis_style_w, g_set_x_axis_style_reversed,
					    0, 2, 1, 2);
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_beats_per_minute, g_beats_per_minute_w, H_beats_per_minute,
					    S_setB S_beats_per_minute, g_set_beats_per_minute_w, g_set_beats_per_minute_reversed,
					    0, 2, 1, 2);
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_beats_per_measure, g_beats_per_measure_w, H_beats_per_measure,
					    S_setB S_beats_per_measure, g_set_beats_per_measure_w, g_set_beats_per_measure_reversed,
					    0, 2, 1, 2);

  #define H_show_all_axes "The value for " S_show_axes " that causes both the x and y axes to be displayed"
  #define H_show_all_axes_unlabelled "The value for " S_show_axes " that causes both the x and y axes to be displayed, but without any label"
  #define H_show_no_axes "The value for " S_show_axes " that causes neither the x or y axes to be displayed"
  #define H_show_x_axis "The value for " S_show_axes " that causes only the x axis to be displayed"
  #define H_show_x_axis_unlabelled "The value for " S_show_axes " that causes only the x axis to be displayed, but without any label"
  #define H_show_bare_x_axis "The value for " S_show_axes " that causes x axis to be displayed without a label or tick marks"

  XEN_DEFINE_CONSTANT(S_show_all_axes,           SHOW_ALL_AXES,            H_show_all_axes);
  XEN_DEFINE_CONSTANT(S_show_all_axes_unlabelled,SHOW_ALL_AXES_UNLABELLED, H_show_all_axes_unlabelled);
  XEN_DEFINE_CONSTANT(S_show_no_axes,            SHOW_NO_AXES,             H_show_no_axes);
  XEN_DEFINE_CONSTANT(S_show_x_axis,             SHOW_X_AXIS,              H_show_x_axis);
  XEN_DEFINE_CONSTANT(S_show_x_axis_unlabelled,  SHOW_X_AXIS_UNLABELLED,   H_show_x_axis_unlabelled);
  XEN_DEFINE_CONSTANT(S_show_bare_x_axis,        SHOW_BARE_X_AXIS,         H_show_bare_x_axis);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_axes, g_show_axes_w, H_show_axes,
					    S_setB S_show_axes, g_set_show_axes_w, g_set_show_axes_reversed, 0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_graphs_horizontal, g_graphs_horizontal_w, H_graphs_horizontal,
					    S_setB S_graphs_horizontal, g_set_graphs_horizontal_w, g_set_graphs_horizontal_reversed, 0, 2, 1, 2);

  #define H_zoom_focus_left "The value for " S_zoom_focus_style " that causes zooming to maintain the left edge steady"
  #define H_zoom_focus_right "The value for " S_zoom_focus_style " that causes zooming to maintain the right edge steady"
  #define H_zoom_focus_middle "The value for " S_zoom_focus_style " that causes zooming to focus on the middle sample"
  #define H_zoom_focus_active "The value for " S_zoom_focus_style " that causes zooming to focus on the currently active object"

  XEN_DEFINE_CONSTANT(S_zoom_focus_left,       ZOOM_FOCUS_LEFT,   H_zoom_focus_left);
  XEN_DEFINE_CONSTANT(S_zoom_focus_right,      ZOOM_FOCUS_RIGHT,  H_zoom_focus_right);
  XEN_DEFINE_CONSTANT(S_zoom_focus_active,     ZOOM_FOCUS_ACTIVE, H_zoom_focus_active);
  XEN_DEFINE_CONSTANT(S_zoom_focus_middle,     ZOOM_FOCUS_MIDDLE, H_zoom_focus_middle);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_zoom_focus_style, g_zoom_focus_style_w, H_zoom_focus_style,
				   S_setB S_zoom_focus_style, g_set_zoom_focus_style_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_gl, g_with_gl_w, H_with_gl,
				   S_setB S_with_gl, g_set_with_gl_w,  0, 0, 1, 0);

#if HAVE_GL
  XEN_DEFINE_PROCEDURE(S_glSpectrogram, g_gl_spectrogram_w, 9, 0, 0, H_glSpectrogram);
#endif

  #define H_after_transform_hook S_after_transform_hook " (snd chn scaler): called just after a spectrum is calculated."
  #define H_graph_hook S_graph_hook " (snd chn y0 y1): called each time a graph is about to be updated. If it returns " PROC_TRUE ", the display is not updated."
  #define H_after_graph_hook S_after_graph_hook " (snd chn): called after a graph is updated."
  #define H_lisp_graph_hook S_lisp_graph_hook " (snd chn): called just before the lisp graph is updated. If it returns a list \
of pixels, these are used in order by the list of graphs (if any), rather than Snd's default set; \
this makes it possible to use different colors for the various graphs. \
If it returns a function (of no arguments), that function is called rather than the standard graph routine."
  #define H_time_graph_hook S_time_graph_hook " (snd chn): called just before the time graph is updated when " S_channel_style " \
is " S_channels_combined ". If it returns a pixel, that color is used to draw the data, rather than Snd's default choice."
  #define H_after_lisp_graph_hook S_after_lisp_graph_hook " (snd chn): called after a lisp graph is updated."
  #define H_mouse_press_hook S_mouse_press_hook " (snd chn button state x y): called upon mouse button press within the lisp graph."
  #define H_mouse_click_hook S_mouse_click_hook " (snd chn button state x y axis): called upon button click."
  #define H_mouse_drag_hook S_mouse_drag_hook " (snd chn button state x y): called upon mouse drag within the lisp graph."
  #define H_mark_click_hook S_mark_click_hook " (id): called when a mark is clicked; return " PROC_TRUE " to squelch the default message."
  #define H_mix_click_hook S_mix_click_hook " (id): called when a mix is clicked; return " PROC_TRUE " to squelch the default message."
  #define H_key_press_hook S_key_press_hook " (snd chn key state): called upon a key press if the mouse is in the lisp graph. \
If it returns " PROC_TRUE ", the key press is not passed to the main handler. 'state' refers to the control, meta, and shift keys."
  #define H_initial_graph_hook S_initial_graph_hook " (snd chn dur): called when a sound is displayed for the first time"
  #define H_mark_drag_triangle_hook S_mark_drag_triangle_hook " (id x time dragged-before): called when a mark play triangle \
is dragged.  'dragged-before' is " PROC_FALSE " when the drag starts and " PROC_TRUE " thereafter.  'x' is the mouse x location in the current \
graph. 'time' is the uninterpreted time at which the drag event was reported. 'id' is the mark id. If the hook returns " PROC_TRUE ", \
Snd takes no further action.  To set up to play, then interpret the motion yourself, return " PROC_FALSE " on the first call, \
and " PROC_TRUE " thereafter."
  
  after_transform_hook =  XEN_DEFINE_HOOK(S_after_transform_hook, 3,  H_after_transform_hook); /* args = sound channel scaler */
  graph_hook =            XEN_DEFINE_HOOK(S_graph_hook, 4,            H_graph_hook);           /* args = sound channel y0 y1 */
  after_graph_hook =      XEN_DEFINE_HOOK(S_after_graph_hook, 2,      H_after_graph_hook);     /* args = sound channel */
  after_lisp_graph_hook = XEN_DEFINE_HOOK(S_after_lisp_graph_hook, 2, H_after_lisp_graph_hook);  /* args = sound channel */
  initial_graph_hook =    XEN_DEFINE_HOOK(S_initial_graph_hook, 3,    H_initial_graph_hook);   /* args = sound channel duration */
  lisp_graph_hook =       XEN_DEFINE_HOOK(S_lisp_graph_hook, 2,       H_lisp_graph_hook);      /* args = sound channel */
  time_graph_hook =       XEN_DEFINE_HOOK(S_time_graph_hook, 2,       H_time_graph_hook);      /* args = sound channel */
  mouse_press_hook =      XEN_DEFINE_HOOK(S_mouse_press_hook, 6,      H_mouse_press_hook);     /* args = sound channel button state x y */
  mouse_click_hook =      XEN_DEFINE_HOOK(S_mouse_click_hook, 7,      H_mouse_click_hook);     /* args = sound channel button state x y axis */
  mouse_drag_hook =       XEN_DEFINE_HOOK(S_mouse_drag_hook, 6,       H_mouse_drag_hook);      /* args = sound channel button state x y */
  key_press_hook =        XEN_DEFINE_HOOK(S_key_press_hook, 4,        H_key_press_hook);       /* args = sound channel key state */
  mark_click_hook =       XEN_DEFINE_HOOK(S_mark_click_hook, 1,       H_mark_click_hook);      /* arg = id */
  mix_click_hook =        XEN_DEFINE_HOOK(S_mix_click_hook, 1,        H_mix_click_hook);       /* arg = id */
  mark_drag_triangle_hook = XEN_DEFINE_HOOK(S_mark_drag_triangle_hook, 4, H_mark_drag_triangle_hook); /* args = id x time dragged-before */
}


