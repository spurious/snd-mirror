#include "snd.h"

/* TODO: multichan case can get in state where initial expose updates are ignored in chan>0 */

enum {NOGRAPH, WAVE, FFT_AXIS, LISP, FFT_MAIN};    /* for marks, regions, mouse click detection */

static XEN lisp_graph_hook;
static XEN mouse_press_hook; 
static XEN mark_click_hook; 
static XEN mouse_click_hook;
static XEN mouse_release_hook; 
static XEN mouse_drag_hook; 
static XEN key_press_hook; 
static XEN transform_hook;
static XEN graph_hook;
static XEN after_graph_hook;

static void after_fft(chan_info *cp, Float scaler)
{
  if (XEN_HOOKED(transform_hook))
    run_hook(transform_hook,
	     XEN_LIST_3(C_TO_SMALL_XEN_INT((cp->sound)->index),
			C_TO_SMALL_XEN_INT(cp->chan),
			C_TO_XEN_DOUBLE(scaler)),
	     S_transform_hook);
}


static void set_y_bounds(axis_info *ap);
static void chans_time_graph_type(chan_info *cp, void *ptr) 
{
  cp->time_graph_type = (*((int *)ptr)); 
  if (cp->time_graph_type == GRAPH_ONCE) 
    {
      set_y_bounds(cp->axis);
      resize_sy(cp);
      set_x_bounds(cp->axis);
      resize_sx(cp);
    }
  update_graph(cp); 
}

static void set_time_graph_type(snd_state *ss, int val) 
{
  in_set_time_graph_type(ss, val); 
  for_each_chan_1(ss, chans_time_graph_type, (void *)(&val));
}

static void chans_wavo_hop(chan_info *cp, void *ptr) 
{
  cp->wavo_hop = (*((int *)ptr)); 
  update_graph(cp); 
}

static void set_wavo_hop(snd_state *ss, int uval) 
{
  int val; 
  if (uval < 1) 
    val = 1; 
  else val = uval; 
  in_set_wavo_hop(ss, val); 
  for_each_chan_1(ss, chans_wavo_hop, (void *)(&val));
}

static void chans_wavo_trace(chan_info *cp, void *ptr) 
{
  cp->wavo_trace = (*((int *)ptr)); 
  update_graph(cp);
}

void set_wavo_trace(snd_state *ss, int uval) 
{
  int val; 
  if (uval < 1) 
    val = 1; 
  else val = uval; 
  in_set_wavo_trace(ss, val); 
  for_each_chan_1(ss, chans_wavo_trace, (void *)(&val));
}

static void set_beats_per_minute(snd_state *ss, Float val) 
{
  if (val > 0.0) 
    {
      in_set_beats_per_minute(ss, val); 
      chans_field(ss, FCP_BEATS, val);
      if (!(ss->graph_hook_active)) 
	for_each_chan(ss, update_graph);
    }
}

static void chans_max_transform_peaks(chan_info *cp, void *ptr) 
{
  cp->max_transform_peaks = (*((int *)ptr)); 
}

static void set_max_transform_peaks(snd_state *ss, int val) 
{
  in_set_max_transform_peaks(ss, val); 
  for_each_chan_1(ss, chans_max_transform_peaks, (void *)(&val));
}

static void chans_zero_pad(chan_info *cp, void *ptr) 
{
  cp->zero_pad = (*((int *)ptr)); 
  calculate_fft(cp);
}

static void set_zero_pad(snd_state *ss, int val) 
{
  in_set_zero_pad(ss, val); 
  for_each_chan_1(ss, chans_zero_pad, (void *)(&val));
}

static void chans_transform_graph_type(chan_info *cp, void *ptr) 
{
  cp->transform_graph_type = (*((int *)ptr)); 
}

void in_set_transform_graph_type(snd_state *ss, int uval) 
{
  int val;
  val = mus_iclamp(0, uval, GRAPH_AS_SPECTROGRAM);
  in_set_transform_graph_type_1(ss, val); 
  for_each_chan_1(ss, chans_transform_graph_type, (void *)(&val));
}

static void chans_show_mix_waveforms(chan_info *cp, void *ptr) 
{
  cp->show_mix_waveforms = (*((int *)ptr));
}

static void set_show_mix_waveforms(snd_state *ss, int val) 
{
  in_set_show_mix_waveforms(ss, val); 
  for_each_chan_1(ss, chans_show_mix_waveforms, (void *)(&val));
}

static void chans_show_axes(chan_info *cp, void *ptr) 
{
  cp->show_axes = (*((int *)ptr)); 
  update_graph(cp); 
}

static void set_show_axes(snd_state *ss, int val) 
{
  in_set_show_axes(ss, val); 
  for_each_chan_1(ss, chans_show_axes, (void *)(&val));
}

static void chans_graphs_horizontal(chan_info *cp, void *ptr) 
{
  cp->graphs_horizontal = (*((int *)ptr)); 
  update_graph(cp); 
}

static void set_graphs_horizontal(snd_state *ss, int val) 
{
  in_set_graphs_horizontal(ss, val);
  for_each_chan_1(ss, chans_graphs_horizontal, (void *)(&val));
}

static void chans_fft_window(chan_info *cp, void *ptr) 
{
  cp->fft_window = (*((int *)ptr)); 
  if (cp->fft) (cp->fft)->window = (*((int *)ptr));
}

void in_set_fft_window(snd_state *ss, int val) 
{
  in_set_fft_window_1(ss, val); 
  for_each_chan_1(ss, chans_fft_window, (void *)(&val));
}

void chans_field(snd_state *ss, int field, Float val)
{
  int i, j;
  snd_info *sp;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse))
	for (j = 0; j < sp->nchans; j++)
	  switch (field)
	    {
	    case FCP_X_ANGLE: sp->chans[j]->spectro_x_angle = val;                       break;
	    case FCP_X_SCALE: sp->chans[j]->spectro_x_scale = val;                       break;
	    case FCP_Y_ANGLE: sp->chans[j]->spectro_y_angle = val;                       break;
	    case FCP_Y_SCALE: sp->chans[j]->spectro_y_scale = val;                       break;
	    case FCP_Z_ANGLE: sp->chans[j]->spectro_z_angle = val;                       break;
	    case FCP_Z_SCALE: sp->chans[j]->spectro_z_scale = val;                       break;
	    case FCP_START:   sp->chans[j]->spectro_start = mus_fclamp(0.0, val, 1.0);   break;
	    case FCP_CUTOFF:  sp->chans[j]->spectro_cutoff = mus_fclamp(0.0, val, 1.0);  break;
	    case FCP_BETA:    sp->chans[j]->fft_window_beta = mus_fclamp(0.0, val, 1.0); break;
	    case FCP_BEATS:   if (val > 0.0) sp->chans[j]->beats_per_minute = val;       break;
	    }
    }
}

void combine_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_COMBINED);}
void superimpose_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_SUPERIMPOSED);}
void separate_sound(snd_info *sp) {change_channel_style(sp, CHANNELS_SEPARATE);}

void set_sound_channel_style(snd_info *sp, int val)
{
  switch (val)
    {
    case CHANNELS_SEPARATE: separate_sound(sp); break; /* snd-xchn.c -> change_channel_style */
    case CHANNELS_COMBINED: combine_sound(sp); break;
    case CHANNELS_SUPERIMPOSED: superimpose_sound(sp); break;
    }
}

int chan_fft_in_progress(chan_info *cp)
{
  return((cp->cgx)->fft_in_progress);
}

void set_chan_fft_in_progress(chan_info *cp, Cessator fp) 
{
  (cp->cgx)->fft_in_progress = fp;
}

void stop_amp_env(chan_info *cp)
{
  chan_context *cgx;
  cgx = cp->cgx;
  if ((cgx) && (cgx->amp_env_in_progress))
    {
      BACKGROUND_REMOVE(cgx->amp_env_in_progress);
      free_env_state(cp);
      cgx->amp_env_in_progress = 0; 
    }
}

void force_fft_clear(chan_info *cp)
{
  if ((cp->cgx) && ((cp->cgx)->fft_in_progress))
    {
      BACKGROUND_REMOVE((cp->cgx)->fft_in_progress);
      finish_progress_report(cp->sound, NOT_FROM_ENVED);
      (cp->cgx)->fft_in_progress = 0;
    }
  if (cp->fft) cp->fft = free_fft_info(cp->fft);
  cp_free_fft_state(cp);
}

void chan_info_cleanup(chan_info *cp)
{
  chan_context *cx;
  if ((cp) && (cp->cgx))
    {
      cx = cp->cgx;
      cx->selected = FALSE;
      if (cx->fft_in_progress) 
	{
	  BACKGROUND_REMOVE(cx->fft_in_progress);
	  cx->fft_in_progress = 0;
	}
      stop_amp_env(cp);
      cleanup_cw(cp);
    }
}

static void set_spectro_start(snd_state *ss, Float val) 
{
  in_set_spectro_start(ss, val);
  chans_field(ss, FCP_START, val);
  if (!(ss->graph_hook_active)) 
    for_each_chan(ss, update_graph);
}

static void chans_dot_size(chan_info *cp, void *ptr) 
{
  cp->dot_size = (*((int *)ptr)); 
  update_graph(cp);
}

void set_dot_size(snd_state *ss, int val)
{
  if (val > 0)  /* -1 here can crash X! */
    {
      in_set_dot_size(ss, (Latus)val);
      for_each_chan_1(ss, chans_dot_size, (void *)(&val));
    }
}

chan_info *virtual_selected_channel(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  if ((sp->channel_style == CHANNELS_SEPARATE) || (sp->selected_channel == NO_SELECTION)) 
    return(cp);
  else return(sp->chans[sp->selected_channel]);
}

static int calculate_fft_1(chan_info *cp, int no_dpy)
{
  snd_state *ss;
  if ((cp->graph_transform_p) &&
      (!(chan_fft_in_progress(cp))))
    {
      ss = cp->state;
      if (cp->transform_graph_type == GRAPH_ONCE)
	single_fft(cp, no_dpy);
      else set_chan_fft_in_progress(cp,
				    BACKGROUND_ADD(ss,
						   sonogram_in_slices,
						   make_sonogram_state(cp)));
    }
  return(0);
}

void calculate_fft(chan_info *cp)
{
  calculate_fft_1(cp, FALSE);
}

static int updating = FALSE;

#if HAVE_DYNAMIC_WIND
static void before_dpy(void *ignore) {}
static void after_dpy(void *context) {updating = FALSE;}
static XEN dpy_body(void *context)
{
  chan_info *cp = (chan_info *)context;
  if ((cp->graph_transform_p) && 
      (!(chan_fft_in_progress(cp)))) 
    calculate_fft_1(cp, TRUE);
  display_channel_data(cp, cp->sound, cp->state);
  return(XEN_FALSE);
}
#endif

void update_graph(chan_info *cp)
{
  /* don't put display stuff here!  This is needed so that the fft display does not get caught in a loop */
  double cur_srate;
  snd_state *ss;
  snd_info *sp;
  axis_info *ap;
  if ((updating) || 
      (!(cp->active)) ||
      (cp->cgx == NULL) || 
      (cp->squelch_update) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL)) 
    return;
  updating = TRUE;
  ss = cp->state;
  sp = cp->sound;

  /* next two are needed by fft and lisp displays, but if put off until make_graph cause
   * the display to happen twice in some cases 
   */
  ap = cp->axis;
  if (ap)
    {
      cur_srate = (double)(SND_SRATE(sp));
      ap->losamp = snd_round_off_t(ap->x0 * cur_srate); 
      if (ap->losamp < 0) ap->losamp = 0;
      ap->hisamp = (off_t)(ap->x1 * cur_srate);
    }
  if (!(((cp->cgx)->ax)->wn)) 
    if (!(fixup_cp_cgx_ax_wn(cp))) 
      return;
#if HAVE_DYNAMIC_WIND
  scm_internal_dynamic_wind((scm_t_guard)before_dpy, 
			    (scm_t_inner)dpy_body, 
			    (scm_t_guard)after_dpy, 
			    (void *)cp,
			    (void *)cp);
#else
  if ((cp->graph_transform_p) && 
      (!(chan_fft_in_progress(cp)))) 
    calculate_fft_1(cp, TRUE);
  display_channel_data(cp, sp, ss);
  updating = FALSE;
#endif
}

#define INITIAL_EDIT_SIZE 8

static XEN initial_graph_hook;

void add_channel_data_1(chan_info *cp, snd_info *sp, int graphed)
{
  /* initialize channel, including edit/sound lists */
  axis_info *ap;
  Float ymin = 0.0, ymax = 0.0, y0, y1;
  double xmax, x0, x1, dur, gdur;
  char *label;
  file_info *hdr;
  off_t samples_per_channel;
  int ymin_set = FALSE, ymax_set = FALSE;
  hdr = sp->hdr;
  samples_per_channel = hdr->samples / hdr->chans;
  x0 = 0.0;
  x1 = 0.1;
  y0 = -1.0;
  y1 = 1.0;
  switch (cp->x_axis_style)
    {
    case X_AXIS_IN_BEATS:      label = _("time (beats)");   break;
    case X_AXIS_IN_SAMPLES:    label = _("time (samples)"); break;
    case X_AXIS_AS_PERCENTAGE: label = _("time (percent)"); break;
    default:                   label = _("time");         break;
    }
  dur = (double)samples_per_channel / (double)(hdr->srate);

  cp->edit_size = INITIAL_EDIT_SIZE;
  cp->edit_ctr = 0;
  allocate_ed_list(cp);
  cp->amp_envs = (env_info **)CALLOC(cp->edit_size, sizeof(env_info *));
  cp->samples = (off_t *)CALLOC(cp->edit_size, sizeof(off_t));
  cp->cursors = (off_t *)CALLOC(cp->edit_size, sizeof(off_t));
  cp->sound_size = INITIAL_EDIT_SIZE;
  cp->sound_ctr = 0;
  cp->sounds = (snd_data **)CALLOC(cp->sound_size, sizeof(snd_data *));
  cp->samples[0] = samples_per_channel;

  if ((graphed == WITH_GRAPH) &&    
      /* can also be WITHOUT_GRAPH and WITHOUT_INITIAL_GRAPH_HOOK
       *   the former is called in snd-nogui, and in the make_readable calls in snd-regions and snd-snd
       *   the latter is from snd-edits where we are updating an already displayed sound (and keeping its axis settings across the update)
       * this hook is replacing earlier "initial-x0" settings
       */
      (XEN_HOOKED(initial_graph_hook)))
    {
      XEN res;
      int len;
      res = run_or_hook(initial_graph_hook,
			XEN_LIST_3(C_TO_SMALL_XEN_INT(sp->index),
				   C_TO_SMALL_XEN_INT(cp->chan),
				   C_TO_XEN_DOUBLE(dur)),
			S_initial_graph_hook);
      if (XEN_LIST_P_WITH_LENGTH(res, len))
	{
	  if (len > 0) x0 = XEN_TO_C_DOUBLE(XEN_CAR(res));
	  if (len > 1) x1 = XEN_TO_C_DOUBLE(XEN_CADR(res));
	  if (len > 2) y0 = XEN_TO_C_DOUBLE(XEN_CADDR(res));
	  if (len > 3) y1 = XEN_TO_C_DOUBLE(XEN_CADDDR(res));
	  if (len > 4) label = XEN_TO_C_STRING(XEN_LIST_REF(res, 4));
	  if (len > 5)
	    {
	      ymin = XEN_TO_C_DOUBLE(XEN_LIST_REF(res, 5));
	      ymin_set = TRUE;
	    }
	  if (len > 6)
	    {
	      ymax = XEN_TO_C_DOUBLE(XEN_LIST_REF(res, 6));
	      ymax_set = TRUE;
	    }
	  /* ymin/ymax for possible fit data hooks */
	}
    }

  if (dur == 0.0) gdur = .001; else gdur = dur;
  xmax = gdur;
  if (x1 == 0.0) x1 = gdur;
  if (!ymax_set) {if (y1 > 1.0) ymax = y1; else ymax = 1.0;}
  if (!ymin_set) {if (y0 < -1.0) ymin = y0; else ymin = -1.0;}
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
  ap = make_axis_info(cp, 0.0, xmax, ymin, ymax, label, x0, x1, y0, y1, NULL);
  if (dur == 0)
    {
      ap->zx = 1.0;
      ap->sx = 1.0;
      ap->no_data = TRUE;
    }
  else
    {
      ap->zx = (ap->x1 - ap->x0) / ap->x_ambit;
      ap->sx = (ap->x0 - ap->xmin) / ap->x_ambit;
      ap->no_data = FALSE;
    }
  ap->zy = (ap->y1 - ap->y0) / ap->y_ambit;
  ap->sy = (ap->y0 - ap->ymin) / ap->y_ambit;
  cp->axis = ap;
  if (graphed == WITH_GRAPH) initialize_scrollbars(cp);
  /* our initial edit_list size will be relatively small */
}

void start_amp_env(chan_info *cp)
{
  chan_context *cgx;
  snd_state *ss;
  cgx = cp->cgx;
  if (cgx)
    {
      ss = cp->state;
      if (cgx->amp_env_in_progress) stop_amp_env(cp);
      start_env_state(cp);
      cgx->amp_env_in_progress = BACKGROUND_ADD(ss, get_amp_env, (Indicium)cp);
      reflect_amp_env_in_progress(cp->sound);
    }
}

void add_channel_data(char *filename, chan_info *cp, file_info *hdr, snd_state *ss, int graphed)
{
  int fd, chn = 0;
  snd_io *io;
  snd_info *sp;
  file_info *chdr;
  sp = cp->sound;
  add_channel_data_1(cp, sp, graphed);
  cp->edits[0] = initial_ed_list(0, (hdr->samples / hdr->chans) - 1);
  chdr = copy_header(filename, sp->hdr); /* need one separate from snd_info case */
  chn = cp->chan;
  if (chdr)
    {
      fd = snd_open_read(ss, filename);
      if (fd != -1)
	{
	  mus_file_open_descriptors(fd,
				    filename, chdr->format,
				    mus_bytes_per_sample(chdr->format),
				    chdr->data_location,
				    chdr->chans,
				    chdr->type);
	  during_open(fd, filename, SND_OPEN_CHANNEL);
	  io = make_file_state(fd, chdr, chn, FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(filename, io, chdr, DONT_DELETE_ME, cp->edit_ctr, chn);
	}
    }
  if ((CURRENT_SAMPLES(cp) > AMP_ENV_CUTOFF) &&
      (cp->amp_envs[0] == NULL) &&                     /* perhaps created in initial-graph-hook by read-peak-env-info-file */
      (cp->sound->short_filename != NULL))             /* region browser jumped in too soon during autotest */
    start_amp_env(cp);
}

static void set_y_bounds(axis_info *ap)
{
  Float range;
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
#if HAVE_ISNAN
  if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
  ap->x1 = (ap->x0 + range);
  if (ap->x1 > ap->xmax)
    {
      ap->x1 = ap->xmax;
      ap->x0 = ap->x1 - range;
    }
  if (ap->x0 < ap->xmin) ap->x0 = ap->xmin;
  ap->changed = TRUE;
}

void apply_y_axis_change (axis_info *ap, chan_info *cp)
{
  snd_info *sp;
  chan_info *ncp;
  axis_info *nap;
  int i;
  Float zy, sy;
  set_y_bounds(ap);
  update_graph(cp);
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      sy = ap->sy;
      zy = ap->zy;
      for (i = 0; i < sp->nchans; i++)
	{
	  ncp = sp->chans[i];
	  if (ncp != cp)
	    {
	      nap = ncp->axis;
	      if (nap)
		{
		  nap->zy = zy;
		  nap->sy = sy;
		  set_y_bounds(nap);
		  update_graph(ncp);
		}
	    }
	}
    }
}

void set_x_axis_x0x1 (chan_info *cp, double x0, double x1) 
{
  axis_info *ap;
  ap = cp->axis;
  if (x0 >= 0.0) ap->x0 = x0; else ap->x0 = 0.0;
  ap->x1 = x1;
  if (x1 > ap->xmax) ap->xmax = x1;
  if (ap->xmax <= ap->xmin) ap->xmax = ap->xmin + .001;
  ap->x_ambit = ap->xmax - ap->xmin;
  ap->zx = (x1 - x0) / ap->x_ambit;
  ap->sx = (x0 - ap->xmin) / ap->x_ambit;
  resize_sx(cp);
  resize_zx(cp);
  apply_x_axis_change(ap, cp, cp->sound); /* this checks sync */
  ap->changed = TRUE;
}

static void set_x_axis_x0(chan_info *cp, off_t left)
{
  double x1x0;
  axis_info *ap;
  if (cp)
    {
      ap = cp->axis; 
      if (ap) 
	{
	  x1x0 = ap->x1 - ap->x0; 
	  ap->x0 = (double)left / (double)SND_SRATE(cp->sound); 
#if HAVE_ISNAN
	  if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
	  set_x_axis_x0x1(cp, ap->x0, ap->x0 + x1x0);
	}
    }
}

static void set_x_axis_x1(chan_info *cp, off_t right)
{
  double x1x0;
  axis_info *ap;
  if (cp)
    {
      ap = cp->axis; 
      if (ap) 
	{
	  x1x0 = ap->x1 - ap->x0; 
	  ap->x1 = (double)right / (double)SND_SRATE(cp->sound); 
	  set_x_axis_x0x1(cp, ap->x1 - x1x0, ap->x1);
	}
    }
}

void reset_x_display(chan_info *cp, double sx, double zx)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sx = sx;
  ap->zx = zx;
  set_x_bounds(ap);
  resize_sx(cp);
  resize_zx(cp);
  update_graph(cp);
}

static void update_xs(chan_info *ncp, axis_info *ap)
{
  double scl;
  axis_info *nap;
  nap = ncp->axis;
  if ((nap) && (nap->xmax > 0.0))
    {
      /* ncp->axis can be NULL here and elsewhere if we're in initialize_scrollbars
       *   of the first channel of a (brand-new) multi-channel sound with sync set --
       *   chans after the first have not necessarily set up an axis_info struct yet.
       */
      scl = ap->xmax / nap->xmax;
      reset_x_display(ncp, ap->sx * scl, ap->zx * scl);
    }
}

void apply_x_axis_change(axis_info *ap, chan_info *cp, snd_info *sp)
{
  int i;
  sync_info *si;
  si = NULL;
  sp->lacp = cp;
  set_x_bounds(ap);
  update_graph(cp);
  if (sp->sync != 0)
    {
      si = snd_sync(cp->state, sp->sync);
      for (i = 0; i < si->chans; i++) 
	if (cp != si->cps[i]) 
	  update_xs(si->cps[i], ap);
      si = free_sync_info(si);
    }
  else 
    {
      if (sp->channel_style != CHANNELS_SEPARATE)
	for (i = 1; i < sp->nchans; i++)
	  update_xs(sp->chans[i], ap);
    }
}

static off_t visible_syncd_cursor(chan_info *cp)
{
  snd_info *sp;
  snd_state *ss;
  chan_info *ncp;
  int i, j, sync;
  sp = cp->sound;
  sync = sp->sync;
  if (sync != 0)
    {
      for (i = 0; i < sp->nchans; i++)
	{
	  ncp = sp->chans[i];
	  if (ncp->cursor_visible) return(CURSOR(ncp));
	}
      /* geez, maybe it's in a separate syncd sound */
      ss = cp->state;
      for (j = 0; j < ss->max_sounds; j++)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse) && (sp->sync == sync) && (sp != cp->sound))
	    for (i = 0; i < sp->nchans; i++)
	      {
		ncp = sp->chans[i];
		if (ncp->cursor_visible) return(CURSOR(ncp));
	      }
	}
    }
  return(-1);
}

void focus_x_axis_change(axis_info *ap, chan_info *cp, snd_info *sp, int focus_style)
{
  /* prepare for set_x_bounds given desired focus point, then drop into default
   * we need to set ap->sx to reflect the new zx value and the focus type
   * if focus_left - go on (nothing to do)
   *    focus_right - get old right, set sx to keep it as is
   *    focus_middle - ditto mid window
   *    focus_active - find the currently active entity, if none use focus_middle 
   */
  chan_info *ncp;
  off_t newf;
  double loc, pos;
  if (ap->xmax == 0.0) return;
  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }
  if (focus_style != ZOOM_FOCUS_LEFT)
    {
      switch (focus_style)
	{
	case ZOOM_FOCUS_RIGHT:   ap->x0 = ap->x1 - ap->zx * ap->x_ambit; break;
	case ZOOM_FOCUS_MIDDLE:  ap->x0 = 0.5 * ((ap->x1 + ap->x0) - ap->zx * ap->x_ambit); break;
	case ZOOM_FOCUS_ACTIVE:
	  ncp = virtual_selected_channel(cp);
	  /* axes should be the same, since all move together in this mode */
	  if (ncp->cursor_visible)
	    newf = CURSOR(ncp);
	  else
	    {
	      /* perhaps user has syncd chans (sounds), has cursor in only one, is zooming using some other channel's (sound's) slider */
	      newf = visible_syncd_cursor(cp);
	      if (newf == -1)
		{
		  if (selection_is_visible_in_channel(ncp)) 
		    newf = selection_beg(ncp);
		  else
		    if (active_mix_p(ncp))
		      newf = mix_beg(ncp);
		    else
		      if (active_mark(ncp))
			newf = mark_beg(ncp);
		      else newf = -1;
		}
	    }
	  if (newf != -1)
	    {
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
	  break;
	}
#if HAVE_ISNAN
      if (isnan(ap->x0)) ap->x0 = 0.0;
#endif
      if (ap->x0 < 0.0) ap->x0 = 0.0;
      ap->sx = (double)(ap->x0 - ap->xmin) / (double)(ap->x_ambit);
    }
  apply_x_axis_change(ap, cp, sp);
}

void sx_incremented(chan_info *cp, double amount)
{
  axis_info *ap;
  ap = cp->axis;
  ap->sx += (ap->zx * amount);
  if (ap->sx < 0.0) ap->sx = 0.0;
  if (ap->sx > (1.0 - ap->zx)) ap->sx = 1.0 - ap->zx;
  apply_x_axis_change(ap, cp, cp->sound);
  resize_sx(cp);
}

void zx_incremented(chan_info *cp, double amount)
{ /* kbd arrows etc -- needs to be able to return to original */
  axis_info *ap;
  off_t samps;
  samps = CURRENT_SAMPLES(cp);
  ap = cp->axis;
  if ((amount >= 1.0) || ((samps > 0) && ((ap->zx * (double)samps) > (amount / 2.0))))
    {
      ap->zx *= amount;
      focus_x_axis_change(ap, cp, cp->sound, zoom_focus_style(cp->state));
      /* apply_x_axis_change(ap, cp, cp->sound); */
      resize_sx(cp);
      resize_zx(cp);
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
  apply_x_axis_change(ap, cp, cp->sound);
  resize_sx(cp);
  return(nx);
}

void set_axes(chan_info *cp, double x0, double x1, Float y0, Float y1)
{
  axis_info *ap;
  ap = cp->axis;
  ap->x0 = x0;
  ap->x1 = x1;
  ap->zx = (x1 - x0) / ap->x_ambit;
  ap->sx = (x0 - ap->xmin) / ap->x_ambit;
  resize_sx(cp);
  resize_zx(cp);
  ap->y0 = y0;
  ap->y1 = y1;
  ap->zy = (y1 - y0) / ap->y_ambit;
  ap->sy = (y0 - ap->ymin) / ap->y_ambit;
  resize_sy(cp);
  resize_zy(cp);
}

/* these are copies from snd-axis.c; didn't want to use macros here */
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

static void display_zero(chan_info *cp)
{
  axis_info *ap;
  Locus zero;
  ap = cp->axis;
  if ((ap->y0 < 0.0) && (ap->y1 > 0.0))
    {
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
  int x0, y0;
  if ((chans > 1) || (cp->edit_ctr > 0))
    {
      set_peak_numbers_font(cp);
      if (cp->printing) ps_set_peak_numbers_font();
      x0 = 5;
      y0 = height + CHN_LABEL_OFFSET;
      if (cp == selected_channel(cp->state))
	{
	  if (chans > 1)
	    {
	      if (cp->edit_ctr == 0)
		mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%s%d]", 
			     _("chn"), (cp->chan + 1));                    /* cp chan numbers are 0 based to index sp->chans array */
	      else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%s%d: (%d)]", 
				_("chn"), (cp->chan + 1), cp->edit_ctr);
	    }
	  else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%d]", cp->edit_ctr);
	}
      else
	{
	  if (chans > 1)
	    {
	      if (cp->edit_ctr == 0)
		mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "%s%d", 
			     _("chn"), (cp->chan + 1));
	      else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "%s%d:(%d)", 
				_("chn"), (cp->chan + 1), cp->edit_ctr);
	    }
	  else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "(%d)", cp->edit_ctr);
	}
      draw_string(copy_context(cp), x0, y0, chn_id_str, strlen(chn_id_str));
      if (cp->printing) 
	ps_draw_string(cp->axis, x0, y0, chn_id_str);
    }
}

#if USE_MOTIF
  #define SELECTION_FFT_LABEL_OFFSET -3
#else
  #define SELECTION_FFT_LABEL_OFFSET -15
#endif

static void display_selection_transform_size (chan_info *cp, axis_info *fap)
{
  int x0, y0;
  if (fap->height < 60) return;
  set_tiny_numbers_font(cp);
  if (cp->printing) ps_set_tiny_numbers_font();
  y0 = fap->height + fap->y_offset + SELECTION_FFT_LABEL_OFFSET;
  x0 = fap->x_axis_x0 + 10;
  mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, 
	       _("(len: " PRId64 "/%d)"), 
	       selection_len(), 
	       cp->selection_transform_size);
  draw_string(copy_context(cp), x0, y0, chn_id_str, strlen(chn_id_str));
  if (cp->printing) ps_draw_string(fap, x0, y0, chn_id_str);
}

static void make_wavogram(chan_info *cp, snd_info *sp, snd_state *ss);
static axis_context *cursor_context(chan_info *cp);
static axis_context *combined_context(chan_info *cp);

int make_graph(chan_info *cp, snd_info *sp, snd_state *ss)
{
  int j = 0;
  off_t samps, ioff = 0;
  Locus xi;
  axis_info *ap;
  Float samples_per_pixel, xf, pinc = 0.0;
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
   * definition (by mouse drag) has a sloppy rounding mechanism (snd-clip.c round_up) so that
   * within reason only samples within the selection-rectangle are in the final selection,
   * and cursor placement (mouse click) rounds to the nearest sample (snd-xchn.c cursor_round).
   *
   * And lastly, the axis x_scale double can be enormous whereas the between-sample choice can
   * be tiny, so we're pushing the arithmetic into dangerous realms.  This stuff has been tested
   * on some extreme cases (hour-long 44KHz stereo), but there's always another special case...
   */
  mus_sample_t samp, ymin, ymax;
  Float fsamp;
  int pixels, grfpts;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double cur_srate = 1.0;
  axis_context *ax = NULL;
  chan_context *cgx;
  ap = cp->axis;
  /* check for no graph */
  if ((!ap) || (!(ap->graph_active)) || (ap->x0 == ap->x1)) return(0);
  if (cp->time_graph_type == GRAPH_AS_WAVOGRAM) 
    {
      make_wavogram(cp, sp, ss); 
      return(0);
    }
  if (sp)
    {
      cur_srate = (double)SND_SRATE(sp);
      ap->losamp = snd_round_off_t(ap->x0 * cur_srate); /* was ceil??? */
      if (ap->losamp < 0) ap->losamp = 0;
      ap->hisamp = (off_t)(ap->x1 * cur_srate);
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
  else samples_per_pixel = (Float)((double)(samps - 1) / (double)pixels);
  if (cp->printing) ps_allocate_grf_points();
  if (sp)
    {
      if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
	ax = combined_context(cp); 
      else ax = copy_context(cp);
      if (cp->printing) ps_fg(ap, ax);
    }
  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE)))
    {
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
	      fsamp = read_sample_to_float(sf);
	      set_grf_point(local_grf_x(x, ap), j, local_grf_y(fsamp, ap));
	      ps_set_grf_point(x, j, fsamp);
	    }
	}
      else
	{
#if (!SNDLIB_USE_FLOATS)
	  mus_sample_t ay0, ay1, isamp;
	  Locus yval;
	  Float yscl;
	  ay0 = MUS_FLOAT_TO_SAMPLE(ap->y0);
	  ay1 = MUS_FLOAT_TO_SAMPLE(ap->y1);
	  yscl = MUS_FIX_TO_FLOAT * ap->y_scale;
	  for (j = 0, x = ((double)(ap->losamp) / cur_srate); j < grfpts; j++, x += incr)
	    {
	      isamp = read_sample(sf);
	      if (isamp >= ay1) 
		yval = ap->y_axis_y1;
	      else 
		{
		  if (isamp <= ay0) 
		    yval = ap->y_axis_y0;
		else yval = (Locus)(ap->y_base + isamp * yscl);
		}
	      set_grf_point(local_grf_x(x, ap), j, yval);
	    }
#else
	  for (j = 0, x = ((double)(ap->losamp) / cur_srate); j < grfpts; j++, x += incr)
	    set_grf_point(local_grf_x(x, ap), j, local_grf_y(read_sample_to_float(sf), ap));
#endif
	}
      if (sp)
	{
	  draw_grf_points(cp, ax, j, ap, 0.0, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_grf_points(ap, j, 0.0, cp->time_graph_style, cp->dot_size);
	}
    }
  else
    {
      /* take min, max */
      if (amp_env_usable(cp, samples_per_pixel, ap->hisamp, TRUE, cp->edit_ctr)) /* true = start new background amp env process if needed */
	j = amp_env_graph(cp, ap, samples_per_pixel, (sp) ? ((int)SND_SRATE(sp)) : 1);
      else
	{
	  if ((ap->hisamp - ap->losamp) > (CURRENT_SAMPLES(cp) / 4))
	    {                                /* we're trying to view a large portion of the (large) sound */
	      cgx = cp->cgx;
	      if (cgx->amp_env_in_progress)
		{                            /* but the amp-env background process is still working on it */
		  env_info *ep;
		  ep = cp->amp_envs[cp->edit_ctr];
		  if ((ep) && samples_per_pixel >= (Float)(ep->samps_per_bin))
		    {                        /* and it will be useful when it finishes */
		      cp->waiting_to_make_graph = TRUE;
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
	  ymin = MUS_SAMPLE_MAX;
	  ymax = MUS_SAMPLE_MIN;
	  if (cp->printing) pinc = samples_per_pixel/cur_srate;
	  ap->changed = FALSE;
	  ss->stopped_explicitly = FALSE;
	  for (ioff = ap->losamp, xf = 0.0; ioff <= ap->hisamp; ioff++)
	    {
	      samp = read_sample(sf);
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j, 
				 local_grf_y(MUS_SAMPLE_TO_FLOAT(ymin), ap), 
				 local_grf_y(MUS_SAMPLE_TO_FLOAT(ymax), ap));
		  if (cp->printing) 
		    {
		      x += pinc; 
		      ps_set_grf_points(x, j, MUS_SAMPLE_TO_FLOAT(ymin), MUS_SAMPLE_TO_FLOAT(ymax));
		    }
		  xi++;
		  j++;
		  xf -= samples_per_pixel;
		  ymin = MUS_SAMPLE_MAX;
		  ymax = MUS_SAMPLE_MIN;
		  if (samps > 10000000)
		    {
		      check_for_event(ss);
		      if ((ap->changed) || (ss->stopped_explicitly))
			{
			  ss->stopped_explicitly = FALSE;
			  ap->changed = FALSE;
			  break;
			}
		    }
		}
	    }
	}
      if (sp)
	{
	  draw_both_grf_points(cp, ax, j, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_both_grf_points(ap, j, cp->time_graph_style, cp->dot_size);
	}
    }
  if (sf) {free_snd_fd(sf); sf = NULL;}
  if ((sp) && (sp->channel_style == CHANNELS_SUPERIMPOSED))
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing)
	ps_reset_color();
    }
  if ((cp->verbose_cursor) && (cp->cursor_on) && (CURSOR(cp) >= ap->losamp) && (CURSOR(cp) <= ap->hisamp) &&
      ((sp) && ((sp->minibuffer_on == MINI_OFF) || (sp->minibuffer_on == MINI_CURSOR))))
    {
      show_cursor_info(cp); 
      sp->minibuffer_on = MINI_CURSOR;
    } 
  return(j);
}


/* these two procedures split "make_graph" into two pieces; the first
 *   gets the data to be graphed, using the amp envs and so on, and
 *   the second displays it.  (The first cp+pos may have no relation
 *   to the second cp -- allow arbitrary overlapping etc).
 */

XEN make_graph_data(chan_info *cp, int edit_pos, off_t losamp, off_t hisamp)
{
  int i, j = 0;
  off_t samps, ioff;
  axis_info *ap;
  snd_info *sp;
  snd_state *ss;
  Float samples_per_pixel, xf;
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double cur_srate = 1.0;
  Float *data = NULL, *data1 = NULL;
  int data_size = 0;
  ap = cp->axis;
  sp = cp->sound;
  ss = get_global_state();
  cur_srate = (double)SND_SRATE(sp);
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
  else samples_per_pixel = (Float)((double)(samps - 1) / (double)pixels);
  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && 
       (samps < POINT_BUFFER_SIZE)))
    {
      data_size = (int)samps;
      sf = init_sample_read_any(losamp, cp, READ_FORWARD, edit_pos);
      if (sf == NULL) return(XEN_FALSE); /* should this throw an error? (CHANNEL_BEING_DEALLOCATED) */
      data = (Float *)MALLOC(data_size * sizeof(Float));
      for (i = 0; i < data_size; i++)
	data[i] = read_sample_to_float(sf);
    }
  else
    {
      if (amp_env_usable(cp, samples_per_pixel, hisamp, FALSE, edit_pos)) 
	{
	  double step, xk;
	  mus_sample_t ymin, ymax;
	  int k, kk;
	  env_info *ep;

	  data_size = pixels + 1;
	  data = (Float *)CALLOC(data_size, sizeof(Float));
	  data1 = (Float *)CALLOC(data_size, sizeof(Float));

	  ep = cp->amp_envs[edit_pos];
	  step = samples_per_pixel / (Float)(ep->samps_per_bin);
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
	      if (kk >= ep->amp_env_size) 
		kk = ep->amp_env_size - 1;
	      for (; k <= kk; k++)
		{
		  if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
		  if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
		}
	      xk += samples_per_pixel;
	      ioff = (off_t)xk;
	      data[j] = MUS_SAMPLE_TO_FLOAT(ymin);
	      data1[j] = MUS_SAMPLE_TO_FLOAT(ymax);
	      j++;
	      ymin = ep->fmax;
	      ymax = ep->fmin;
	    }
	}
      else
	{
	  Float ymin, ymax, samp;
	  data_size = pixels + 1;
	  sf = init_sample_read_any(losamp, cp, READ_FORWARD, edit_pos);
	  if (sf == NULL) return(XEN_FALSE);
	  data = (Float *)CALLOC(data_size, sizeof(Float));
	  data1 = (Float *)CALLOC(data_size, sizeof(Float));
	  j = 0;      /* graph point counter */
	  ymin = 100.0;
	  ymax = -100.0;
	  ss->stopped_explicitly = FALSE;
	  for (ioff = losamp, xf = 0.0; ioff <= hisamp; ioff++)
	    {
	      samp = read_sample_to_float(sf);
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
			  ss->stopped_explicitly = FALSE;
			  ap->changed = FALSE;
			  break;
			}
		    }
		}
	    }
	}
    }
  if (sf) 
    {
      free_snd_fd(sf); 
      sf = NULL;
    }
  if (data1)
    return(XEN_LIST_2(make_vct(data_size, data),
		      make_vct(data_size, data1)));
  else return(make_vct(data_size, data));
}

void draw_graph_data(chan_info *cp, off_t losamp, off_t hisamp, int data_size, 
		     Float *data, Float *data1, axis_context *ax, int style)
{
  off_t i, samps;
  Locus xi;
  axis_info *ap;
  snd_info *sp;
  double x, incr;  
  double start_time = 0.0, cur_srate = 1.0;
  ap = cp->axis;
  sp = cp->sound;
  if (data1 == NULL)
    {
      cur_srate = (double)SND_SRATE(sp);
      if (losamp == -1)
	losamp = snd_round_off_t(ap->x0 * cur_srate);
      start_time = (double)(losamp) / cur_srate;
      if (hisamp == -1)
	hisamp = (off_t)(ap->x1 * cur_srate);
      samps = hisamp - losamp + 1;
      if (samps <= 0) return;
      if (samps > data_size) samps = data_size;
      incr = (double)1.0 / cur_srate;
      for (i = 0, x = start_time; i < samps; i++, x += incr)
	set_grf_point(local_grf_x(x, ap), 
		      i, 
		      local_grf_y(data[i], ap));
      draw_grf_points(cp, ax, samps, ap, 0.0, style);
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
      draw_both_grf_points(cp, ax, samps, style);
    }
}


static int compare_peak_amps(const void *pk1, const void *pk2)
{
  if (((fft_peak *)pk1)->amp > ((fft_peak *)pk2)->amp) return(-1);
  else if (((fft_peak *)pk1)->amp == ((fft_peak *)pk2)->amp) return(0);
  return(1);
}

static char ampstr[LABEL_BUFFER_SIZE];
#define AMP_ROOM 35
#define AMP_ROOM_CUTOFF 3.0

#define LOG_FACTOR 25.0
/* determines how we view the log -- the higher the factor, the more we emphasize the lower octaves (not sure this is a good idea) */

static void display_peaks(chan_info *cp, axis_info *fap, Float *data, int scaler, off_t samps, Float samps_per_pixel, int fft_data, Float fft_scale)
{
  int num_peaks, row, col, tens, i, with_amps, acol, acols;
  Float amp0, px;
  char *fstr;
  axis_context *ax;
  fft_peak *peak_freqs = NULL;
  fft_peak *peak_amps = NULL;
  if (samps > (scaler * 10)) 
    tens = 2; 
  else 
    if (samps > scaler) 
      tens = 1; 
    else 
      if (samps > (scaler/10)) 
	tens = 0; 
      else tens = -1;
  num_peaks = (fap->y_axis_y0 - fap->y_axis_y1) / 20;
  if (num_peaks <= 0) return;
  peak_freqs = (fft_peak *)CALLOC(cp->max_transform_peaks, sizeof(fft_peak));
  peak_amps = (fft_peak *)CALLOC(cp->max_transform_peaks, sizeof(fft_peak));
  if (num_peaks > cp->max_transform_peaks) num_peaks = cp->max_transform_peaks;
  if (fft_data)
    num_peaks = find_and_sort_transform_peaks(data, peak_freqs, num_peaks, samps, 1, samps_per_pixel, fft_scale); /* srate 1.0=>freqs between 0 and 1.0 */
  else num_peaks = find_and_sort_peaks(data, peak_freqs, num_peaks, samps);
  if ((num_peaks == 1) && (peak_freqs[0].freq == 0.0)) 
    {
      FREE(peak_freqs); 
      FREE(peak_amps); 
      return;
    }
  with_amps = (fap->width > ((30 + 5 * tens + AMP_ROOM) * AMP_ROOM_CUTOFF));
  acols = 3;
  col = fap->x_axis_x1 - 30 - tens * 5; 
  /* in some cases (lisp graph with integer peak "freqs") this can move the freq column over too far */
  acol = fap->x_axis_x1 - AMP_ROOM + 15;
  if (with_amps) 
    {
      col -= AMP_ROOM;
      if ((fft_data) && 
	  (cp->transform_normalization == DONT_NORMALIZE))
	{
	  col -= 5;
	  acol -= 5;
	  acols = 4;
	}
    }
  ax = copy_context(cp);
  if (num_peaks > 6)
    {
      for (i = 0; i < num_peaks; i++) peak_amps[i] = peak_freqs[i];
      qsort((void *)peak_amps, num_peaks, sizeof(fft_peak), compare_peak_amps);
      if (num_peaks < 12) amp0 = peak_amps[2].amp; else amp0 = peak_amps[5].amp;
      set_bold_peak_numbers_font(cp);
      if (cp->printing) ps_set_bold_peak_numbers_font();
      row = fap->y_axis_y1 + 15;
      for (i = 0; i < num_peaks; i++)
	{
	  if (peak_freqs[i].amp >= amp0)
	    {
	      px = peak_freqs[i].freq;
	      fstr = prettyf(px * scaler, tens);
	      draw_string(ax, col, row, fstr, strlen(fstr));
	      if (cp->printing) ps_draw_string(fap, col, row, fstr);
	      FREE(fstr);
	      fstr = NULL;
	      if (with_amps)
		{
		  if ((fft_data) && (cp->fft_log_magnitude))
		    mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.1f", in_dB(cp->min_dB, cp->lin_dB, peak_freqs[i].amp));
		  else mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.*f", acols, peak_freqs[i].amp);
		  draw_string(ax, acol, row, ampstr, strlen(ampstr));
		  if (cp->printing) 
		    ps_draw_string(fap, acol, row, ampstr);
		}
	    }
	  row += 15;
	}
    }
  else amp0 = 100.0;
  set_peak_numbers_font(cp);
  if (cp->printing) ps_set_peak_numbers_font();
  /* choose a small font for these numbers */
  row = fap->y_axis_y1 + 15;
  for (i = 0; i < num_peaks; i++)
    {
      if (peak_freqs[i].amp < amp0)
	{
	  px = peak_freqs[i].freq;
	  fstr = prettyf(px * scaler, tens);
	  draw_string(ax, col, row, fstr, strlen(fstr));
	  if (cp->printing) ps_draw_string(fap, col, row, fstr);
	  FREE(fstr);
	  fstr = NULL;
	  if (with_amps)
	    {
	      if ((fft_data) && (cp->fft_log_magnitude))
		mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.1f", in_dB(cp->min_dB, cp->lin_dB, peak_freqs[i].amp));
	      else mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.*f", acols, peak_freqs[i].amp);
	      draw_string(ax, acol, row, ampstr, strlen(ampstr));
	      if (cp->printing) 
		ps_draw_string(fap, acol, row, ampstr);
	    }
	}
      row += 15;
    }
  if (peak_freqs) FREE(peak_freqs); 
  if (peak_amps) FREE(peak_amps);
}

void make_fft_graph(chan_info *cp, snd_info *sp, axis_info *fap, axis_context *ax, int with_hooks)
{
  /* axes are already set, data is in the fft_info struct -- don't reset here! */
  /* since the fft size menu callback can occur while we are calculating the next fft, we have to lock the current size until the graph goes out */
  fft_info *fp;
  Float *data;
  Float incr, x, scale;
  int i, j, hisamp, losamp = 0;
  Float samples_per_pixel, xf, ina, ymax, scaler;
  Locus logx, logy;
  Float pslogx, pslogy;
  fp = cp->fft;
  if (chan_fft_in_progress(cp)) return;
  if (!fap->graph_active) return;
  data = fp->data;
  /* these lo..hi values are just for upcoming loops -- not axis info */
  if (cp->transform_type == FOURIER)
    {
      hisamp = (int)(fp->current_size * cp->spectro_cutoff / 2);
      losamp = (int)(fp->current_size * cp->spectro_start / 2);
      incr = (Float)SND_SRATE(sp) / (Float)(fp->current_size);
    }
  else
    {
      /* hisamp here is in terms of transform values, not original sampled data values */
      hisamp = (int)(fp->current_size * cp->spectro_cutoff);
      losamp = (int)(fp->current_size * cp->spectro_start);
      incr = 1.0;
    }
  /* no scaling etc here!! see snd_display_fft in snd-fft.c */
  scale = fp->scale;
  if (cp->printing) ps_allocate_grf_points();
  samples_per_pixel = (Float)((double)(hisamp - losamp) / (Float)(fap->x_axis_x1 - fap->x_axis_x0));
  if (cp->printing) ps_fg(fap, ax);
  if (samples_per_pixel < 4.0)
    {
      if ((!(cp->fft_log_magnitude)) && 
	  (!(cp->fft_log_frequency)))
	{
	  if (losamp == 0)
	    {
	      for (i = 0, x = 0.0; i < hisamp; i++, x += incr)
		{
		  set_grf_point(local_grf_x(x, fap), 
				i, 
				local_grf_y(data[i] * scale, fap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i, data[i] * scale);
		}
	    }
	  else
	    {
	      for (i = losamp, x = fap->x0; i < hisamp; i++, x += incr)
		{
		  set_grf_point(local_grf_x(x, fap), 
				i - losamp, 
				local_grf_y(data[i] * scale, fap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i - losamp, data[i] * scale);
		}
	    }
	}
      else
	{
	  if (cp->fft_log_frequency) 
	    {
	      ymax = LOG_FACTOR;
	      incr = ymax / (Float)((double)(hisamp - losamp));
	      scaler = 1.0 / log(ymax + 1.0);
	    }
	  else scaler = 0.0;
	  for (i = losamp, x = fap->x0; i < hisamp; i++, x += incr)
	    {
	      if (cp->fft_log_frequency) 
		logx = local_grf_x(log(x + 1.0) * scaler, fap); 
	      else logx = local_grf_x(x, fap);
	      if (cp->fft_log_magnitude) 
		logy = local_grf_y(in_dB(cp->min_dB, cp->lin_dB, data[i] * scale), fap); 
	      else logy = local_grf_y(data[i] * scale, fap);
	      set_grf_point(logx, i - losamp, logy);
	      if (cp->printing) 
		{
		  if (cp->fft_log_frequency) pslogx = log(x + 1.0) * scaler; else pslogx = x;
		  if (cp->fft_log_magnitude) pslogy = in_dB(cp->min_dB, cp->lin_dB, data[i] * scale); else pslogy = data[i] * scale;
		  ps_set_grf_point(pslogx, i - losamp, pslogy);
		}
	    }
	}
      draw_grf_points(cp, ax, i - losamp, fap, 0.0, cp->transform_graph_style);
      if (cp->printing) 
	ps_draw_grf_points(fap, i - losamp, 0.0, cp->transform_graph_style, cp->dot_size);
    }
  else
    {
      j = 0;      /* graph point counter */
      i = losamp;
      if (losamp == 0) 
	x = 0.0; 
      else x = fap->x0;
      xf = 0.0;     /* samples per pixel counter */
      if (cp->fft_log_frequency) 
	{
	  ymax = LOG_FACTOR;
	  incr = ymax / (Float)((double)(hisamp - losamp));
	  scaler = 1.0 / log(ymax + 1.0);
	}
      else scaler = 0.0;
      ymax = -1.0;
      if ((!(cp->fft_log_magnitude)) && 
	  (!(cp->fft_log_frequency)))
	{
	  while (i < hisamp)
	    {
	      ina = data[i];
	      if (ina > ymax) ymax = ina;
	      xf += 1.0;
	      i++;
	      if (xf > samples_per_pixel)
		{
		  set_grf_point(local_grf_x(x, fap), j, local_grf_y(ymax * scale, fap));
		  x += (incr * samples_per_pixel); 
		  if (cp->printing) 
		    ps_set_grf_point(x, j, ymax * scale);
		  j++;
		  xf -= samples_per_pixel;
		  ymax = -1.0;
		}
	    }
	}
      else
	{
	  while (i < hisamp)
	    {
	      ina = data[i];
	      if (ina > ymax) ymax = ina;
	      xf += 1.0;
	      i++;
	      if (xf > samples_per_pixel)
		{
		  if (cp->fft_log_frequency) 
		    logx = local_grf_x(log(x + 1.0) * scaler, fap); 
		  else logx = local_grf_x(x, fap);
		  if (cp->fft_log_magnitude) 
		    logy = local_grf_y(in_dB(cp->min_dB, cp->lin_dB, ymax * scale), fap); 
		  else logy = local_grf_y(ymax * scale, fap);
		  set_grf_point(logx, j, logy);
		  if (cp->printing) 
		    {
		      if (cp->fft_log_frequency) 
			pslogx = log(x + 1.0) * scaler; 
		      else pslogx = x;
		      if (cp->fft_log_magnitude) 
			pslogy = in_dB(cp->min_dB, cp->lin_dB, ymax * scale); 
		      else pslogy = ymax * scale;
		      ps_set_grf_point(pslogx, j, pslogy);
		    }
		  x += (incr * samples_per_pixel);
		  j++;
		  xf -= samples_per_pixel;
		  ymax = -1.0;
		}
	    }
	}
      draw_grf_points(cp, ax, j, fap, 0.0, cp->transform_graph_style);
      if (cp->printing) 
	ps_draw_grf_points(fap, j, 0.0, cp->transform_graph_style, cp->dot_size);
    }
  if (sp->channel_style == CHANNELS_SUPERIMPOSED)
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing) ps_reset_color();
    }
  if (cp->show_transform_peaks) 
    {
      if (cp->transform_type == FOURIER)
	display_peaks(cp, fap, data, 
		      (int)(SND_SRATE(sp) * cp->spectro_cutoff / 2), 
		      hisamp, samples_per_pixel, TRUE, scale);
      else display_peaks(cp, fap, data, 
			 (int)(fp->current_size * cp->spectro_cutoff), 
			 hisamp, samples_per_pixel, TRUE, 0.0);
    }
  if (cp->selection_transform_size != 0) display_selection_transform_size(cp, fap);
  if (with_hooks) after_fft(cp, scale);
}

static int display_transform_peaks(chan_info *ucp, char *filename)
{
  /* put (sync'd) peak info in help window */
  char *str = NULL, *mcf = NULL;
  fft_info *fp;
  axis_info *fap, *ap;
  snd_state *ss;
  snd_info *sp;
  Float *data;
  Float srate2;
  time_t ts;
  char *timbuf;
  fft_peak *peak_freqs = NULL;
  fft_peak *peak_amps = NULL;
  FILE *fd = NULL;
  int i, chn, samps, num_peaks, tens, srate, err = 0, tmp_file = 1, chars;
  Float samples_per_pixel;
  sync_info *si = NULL;
  chan_info *cp;
  ss = ucp->state;
  sp = ucp->sound;
  si = sync_to_chan(ucp);
  if ((filename) && (snd_strlen(filename) > 0))
    {
      fd = FOPEN(mcf = mus_expand_filename(filename), "w");
      if (mcf) FREE(mcf);
      if (fd == NULL) 
	{
	  report_in_minibuffer_and_save(sp, _("can't write %s: %s"), filename, strerror(errno));
	  err = 1;
	}
      else tmp_file = 0;
    }
  if (tmp_file == 1)
    {
      filename = snd_tempnam(ss);
      fd = FOPEN(filename, "w");
    }
  if (fd) 
    {
#if HAVE_STRFTIME
      timbuf = (char *)CALLOC(TIME_STR_SIZE, sizeof(char));
      time(&ts);
      strftime(timbuf, TIME_STR_SIZE, STRFTIME_FORMAT, localtime(&ts));
      fprintf(fd, _("Snd: fft peaks (%s)\n\n"), timbuf);
      FREE(timbuf);
#else
      fprintf(fd, _("Snd: fft peaks (%s)\n\n"));
#endif
      for (chn = 0; chn < si->chans; chn++)
	{
	  cp = si->cps[chn];
	  fp = cp->fft;
	  if (fp)
	    {
	      fap = fp->axis;
	      if ((fap) && (fap->graph_active))
		{
		  ap = cp->axis;
		  sp = cp->sound;
		  data = fp->data;
		  samps = fp->current_size / 2;
		  samples_per_pixel = (Float)samps / (Float)(fap->x_axis_x1 - fap->x_axis_x0);
		  srate = SND_SRATE(sp);
		  srate2 = (Float)srate * .5;
		  if (samps > (5 * srate)) 
		    tens = 2; 
		  else 
		    if (samps > (int)srate2) 
		      tens = 1; 
		    else 
		      if (samps > (srate / 20)) 
			tens = 0; 
		      else tens = -1;
		  peak_freqs = (fft_peak *)CALLOC(cp->max_transform_peaks, sizeof(fft_peak));
		  peak_amps = (fft_peak *)CALLOC(cp->max_transform_peaks, sizeof(fft_peak));
		  num_peaks = find_and_sort_transform_peaks(data, peak_freqs, cp->max_transform_peaks, samps, 1, samples_per_pixel, fp->scale);
		  if ((num_peaks != 1) || 
		      (peak_freqs[0].freq != 0.0))
		    {
		      fprintf(fd, sp->short_filename);
		      if (sp->nchans > 1) fprintf(fd, _(": chan %d"), cp->chan);
		      fprintf(fd, _(", fft %d points beginning at sample " PRId64 " (%.3f secs)\n\n"),
			      fp->current_size, 
			      ap->losamp, 
			      (float)((double)(ap->losamp) / (double)srate));
		      for (i = 0; i < num_peaks; i++)
			fprintf(fd, "  %.*f  %.5f\n",
				tens, 
				peak_freqs[i].freq * srate2, 
				peak_freqs[i].amp); 
		      fprintf(fd, "\n");
		    }
		  if (peak_freqs) {FREE(peak_freqs); peak_freqs = NULL;}
		  if (peak_amps) {FREE(peak_amps); peak_amps = NULL;}
		}
	    }
	}
      if (FCLOSE(fd) != 0)
	report_in_minibuffer_and_save(sp, _("can't close %s: %s"), filename, strerror(errno));
      if (tmp_file)
	{
	  fd = FOPEN(filename, "r");
	  fseek(fd, 0, SEEK_END);
	  chars = ftell(fd);
	  rewind(fd);
	  str = (char *)CALLOC(chars + 1, sizeof(char));
	  fread(str, 1, chars, fd);
	  snd_fclose(fd, filename);
	  snd_help(ss, "fft peaks", str);
	  FREE(str);
	  err = snd_remove(filename, FALSE);
	  FREE(filename);
	}
    }
  if (si) si = free_sync_info(si);
  return(err);
}

#define NO_COLOR -1
static int skew_color(snd_state *ss, Float x)
{
  Float base, val;
  int pos;
  if (x < color_cutoff(ss)) return(NO_COLOR);
  if (color_inverted(ss))   
    val = 1.0 - x;
  else val = x;
  base = color_scale(ss);
  if ((base > 0.0) && (base != 1.0))
    val = (pow(base, val) - 1.0) / (base - 1.0);
  pos = (int)(val * COLORMAP_SIZE);
  if (pos > COLORMAP_SIZE) return(COLORMAP_SIZE - 1);
  if (pos > 0)
    return(pos - 1);
  return(0);
}

static int js[COLORMAP_SIZE];

static void make_sonogram(chan_info *cp, snd_info *sp, snd_state *ss)
{ 
  sono_info *si;
  int i, slice, fwidth, fheight, j, bins;
  Latus rectw, recth;
  fft_info *fp;
  axis_info *fap;
  Float *fdata;
  Float binval, xf, xfincr, yf, yfincr, scaler = 0.0, frectw, frecth, xscl, scl = 1.0;
  Float *hfdata;
  Locus *hidata;
  axis_context *ax;
  if (chan_fft_in_progress(cp)) return;
  si = (sono_info *)(cp->sonogram_data);
  if ((si) && (si->scale > 0.0))
    {
      bins = (int)(si->target_bins * cp->spectro_cutoff);
      if (cp->printing) ps_allocate_grf_points();
      allocate_sono_rects(si->total_bins);
      allocate_color_map(ss, color_map(ss));
      if (cp->fft_log_frequency) scaler = 1.0 / log(LOG_FACTOR + 1.0);
      scl = si->scale; 
      fp = cp->fft;
      fap = fp->axis;
      fwidth = fap->x_axis_x1 - fap->x_axis_x0;
      fheight = fap->y_axis_y0 - fap->y_axis_y1;
      frectw = (Float)fwidth / (Float)(si->target_slices);
      frecth = (Float)fheight / (Float)bins;
      xscl = (Float)(fap->x1 - fap->x0) / (Float)(si->target_slices);
      rectw = (Latus)(ceil(frectw));
      recth = (Latus)(ceil(frecth));
      if (rectw == 0) rectw = 1;
      if (recth == 0) recth = 1;
      hfdata = (Float *)MALLOC((bins + 1) * sizeof(Float));
      hidata = (Locus *)MALLOC((bins + 1) * sizeof(Locus));
      if (cp->transform_type == FOURIER)
	{
	  if (cp->fft_log_frequency)
	      yfincr = (cp->spectro_cutoff * LOG_FACTOR) / (Float)bins;
	  else yfincr = cp->spectro_cutoff * (Float)SND_SRATE(cp->sound) * 0.5 / (Float)bins;
	}
      else yfincr = 1.0;
      if (cp->fft_log_frequency)
	{
	  for (yf = 0.0, i = 0; i <= bins; i++, yf += yfincr)
	    {
	      hfdata[i] = log(yf + 1.0) * scaler;
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
      xfincr = ((Float)fwidth / (Float)(si->target_slices));
      xf = 2 + fap->x_axis_x0;
      ax = copy_context(cp);
      ss->stopped_explicitly = FALSE;
      for (slice = 0; slice < si->active_slices; slice++, xf += xfincr)
	{
	  memset((void *)js, 0, COLORMAP_SIZE * sizeof(int));
	  fdata = si->data[slice];
	  for (i = 0; i < bins; i++)
	    {
	      /* above is fdata[i-1], left is si->data[slice-1][i] */
	      binval = fdata[i] / scl;
	      if (cp->fft_log_magnitude) binval = 1.0 - (in_dB(cp->min_dB, cp->lin_dB, binval)) / cp->min_dB;
	      j = skew_color(ss, binval);
	      if (j != NO_COLOR)
		{
		  if (cp->fft_log_frequency)
		    set_sono_rectangle(js[j], j, (Locus)xf, hidata[i + 1], rectw, (Latus)(hidata[i] - hidata[i + 1]));
		  else set_sono_rectangle(js[j], j, (Locus)xf, hidata[i + 1], rectw, recth);
		  if (cp->printing)
		    {
		      if (cp->fft_log_frequency) 
			ps_draw_sono_rectangle(fap, j, fap->x0 + xscl * slice, hfdata[i + 1], frectw, hidata[i] - hidata[i + 1]);
		      else ps_draw_sono_rectangle(fap, j, fap->x0 + xscl * slice, hfdata[i + 1], frectw, -frecth);
		    }
		  js[j]++;
		}
	    }
	  for (i = 0; i < COLORMAP_SIZE; i++)
	    if (js[i] > 0) 
	      draw_sono_rectangles(ax, i, js[i]);
	  if (cp->printing)
	    {
	      check_for_event(ss);
	      if ((ss->stopped_explicitly) || (!(cp->active))) /* user closed file while trying to print */
		{
		  ss->stopped_explicitly = FALSE;
		  report_in_minibuffer(sp, _("stopped"));
		  break;
		}
	    }
	}
      if (cp->printing) ps_reset_color();
      FREE(hfdata);
      FREE(hidata);
      if (cp->hookable) after_fft(cp, 1.0 / scl);
    }
}

static void rotate_matrix(Float xangle, Float yangle, Float zangle, Float xscl, Float yscl, Float zscl, Float *mat)
{
  /* return rotation matrix for rotation through angles xangle, yangle, then zangle with scaling by xscl, yscl, zscl */
  Float sinx, siny, sinz, cosx, cosy, cosz, deg;
  Float x, y, z;
  deg = TWO_PI / 360.0;
  sinx = sin(x = (xangle * deg));
  siny = sin(y = (yangle * deg));
  sinz = sin(z = (zangle * deg));
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

static void rotate(Float *xyz, Float *mat)
{ /* use rotation/scaling matrix set up by rotate_matrix to rotate and scale the vector xyz */
  Float x, y, z;
  x = xyz[0];
  y = xyz[1];
  z = xyz[2];
  xyz[0] = mat[0] * x + mat[1] * y + mat[2] * z;
  xyz[1] = mat[3] * x + mat[4] * y + mat[5] * z;
  xyz[2] = mat[6] * x + mat[7] * y + mat[8] * z;
}

#if HAVE_GL

void reset_spectro(snd_state *ss)
{
  set_spectro_cutoff(ss, DEFAULT_SPECTRO_CUTOFF);
  set_spectro_hop(ss, DEFAULT_SPECTRO_HOP);
  set_spectro_x_angle(ss, (with_gl(ss)) ? DEFAULT_SPECTRO_X_ANGLE : 90.0);
  set_spectro_y_angle(ss, (with_gl(ss)) ? DEFAULT_SPECTRO_Y_ANGLE : 0.0);
  set_spectro_z_angle(ss, (with_gl(ss)) ? DEFAULT_SPECTRO_Z_ANGLE : 358.0);
  set_spectro_x_scale(ss, (with_gl(ss)) ? DEFAULT_SPECTRO_X_SCALE : 1.0);
  set_spectro_y_scale(ss, (with_gl(ss)) ? DEFAULT_SPECTRO_Y_SCALE : 1.0);
  set_spectro_z_scale(ss, (with_gl(ss)) ? DEFAULT_SPECTRO_Z_SCALE : 0.1);
}

static GLdouble unproject2x(int x, int y)
{
  /* taken from GL doc p152 */
  GLint viewport[4];
  GLdouble mv[16], proj[16];
  GLint realy;
  GLdouble wx, wy, wz;
  glGetIntegerv(GL_VIEWPORT, viewport);
  glGetDoublev(GL_MODELVIEW_MATRIX, mv);
  glGetDoublev(GL_PROJECTION_MATRIX, proj);
  realy = viewport[3] - (GLint)y - 1;
  gluUnProject((GLdouble)x, (GLdouble)realy, 0.0, mv, proj, viewport, &wx, &wy, &wz);
  return(wx);
}
static GLdouble unproject2y(int x, int y)
{
  /* taken from GL doc p152 */
  GLint viewport[4];
  GLdouble mv[16], proj[16];
  GLint realy;
  GLdouble wx, wy, wz;
  glGetIntegerv(GL_VIEWPORT, viewport);
  glGetDoublev(GL_MODELVIEW_MATRIX, mv);
  glGetDoublev(GL_PROJECTION_MATRIX, proj);
  realy = viewport[3] - (GLint)y - 1;
  gluUnProject((GLdouble)x, (GLdouble)realy, 0.0, mv, proj, viewport, &wx, &wy, &wz);
  return(wy);
}
#else
void reset_spectro(snd_state *ss)
{
  set_spectro_cutoff(ss, DEFAULT_SPECTRO_CUTOFF);
  set_spectro_hop(ss, DEFAULT_SPECTRO_HOP);
  set_spectro_x_angle(ss, DEFAULT_SPECTRO_X_ANGLE);
  set_spectro_y_angle(ss, DEFAULT_SPECTRO_Y_ANGLE);
  set_spectro_z_angle(ss, DEFAULT_SPECTRO_Z_ANGLE);
  set_spectro_x_scale(ss, DEFAULT_SPECTRO_X_SCALE);
  set_spectro_y_scale(ss, DEFAULT_SPECTRO_Y_SCALE);
  set_spectro_z_scale(ss, DEFAULT_SPECTRO_Z_SCALE);
}
#endif

static void display_channel_time_data(chan_info *cp, snd_info *sp, snd_state *ss);
static void display_channel_lisp_data(chan_info *cp, snd_info *sp, snd_state *ss);
static void make_axes(chan_info *cp, axis_info *ap, int x_style, int erase_first);
#define DONT_CLEAR_GRAPH FALSE
#define CLEAR_GRAPH TRUE

#if HAVE_GL && USE_GTK
#if HAVE_GTK_WIDGET_GL_MAKE_CURRENT
  #define GL_MAKE_CURRENT(Cp) gtk_widget_gl_make_current(channel_graph(Cp))
  #define GL_SWAP_BUFFERS(Cp) if (gtk_widget_gl_is_double_buffer(channel_graph(Cp))) \
	                        gtk_widget_gl_swap_buffers(channel_graph(Cp)); \
                              else glFlush()
#else
  #define GL_MAKE_CURRENT(Cp) gdk_gl_drawable_make_current(gtk_widget_get_gl_drawable(channel_graph(Cp)), \
		                                           gtk_widget_get_gl_context(channel_graph(Cp)))
  #define GL_SWAP_BUFFERS(Cp) if (gdk_gl_drawable_is_double_buffered(gtk_widget_get_gl_drawable(channel_graph(Cp)))) \
	                        gdk_gl_drawable_swap_buffers(gtk_widget_get_gl_drawable(channel_graph(Cp))); \
                              else glFlush()
#endif
#endif

#if HAVE_GL
static void gl_spectrogram(snd_state *ss, sono_info *si, int gl_fft_list, Float cutoff, int use_dB, Float min_dB,
			   unsigned short br, unsigned short bg, unsigned short bb)
{
  Float lin_dB = 0.0;
  Float xincr, yincr, x0, y0;
  int bins = 0, slice, i, j;
  float x1, y1, inv_scl;
  int **js = NULL;
  inv_scl = 1.0 / si->scale;
  if (use_dB) lin_dB = pow(10.0, min_dB * 0.05);
  glNewList((GLuint)gl_fft_list, GL_COMPILE);
  bins = (int)(si->target_bins * cutoff);
  if (bins <= 0) bins = 1;
  js = (int **)CALLOC(si->active_slices, sizeof(int *));
  for (i = 0; i < si->active_slices; i++)
    {
      js[i] = (int *)CALLOC(bins, sizeof(int));
      if (use_dB) 
	{
	  for (j = 0; j < bins; j++)
	    js[i][j] = skew_color(ss, 1.0 - (in_dB(min_dB, lin_dB, si->data[i][j] * inv_scl)) / min_dB);
	}
      else
	{
	  for (j = 0; j < bins; j++)
	    js[i][j] = skew_color(ss, si->data[i][j] * inv_scl); /* can be NO_COLOR (-1) */
	}
    }
  
  xincr = 1.0 / (float)(si->active_slices);
  yincr = 1.0 / (float)bins;
  
  for (x0 = -0.5, slice = 0; slice < si->active_slices - 1; slice++, x0 += xincr)
    {
      for (i = 0, y0 = -0.5; i < bins - 1; i++, y0 += yincr)
	{
	  unsigned short r, g, b;
	  Float val00, val01, val11, val10;
	  glBegin(GL_POLYGON);
	  x1 = x0 + xincr;
	  y1 = y0 + yincr;

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
	      glColor3us(r, g, b);
	    }
	  else glColor3us(br, bg, bb);
	  glVertex3f(x0, val00, y0);
	  
	  if (js[slice + 1][i] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice + 1][i], &r, &g, &b);
	      glColor3us(r, g, b);
	    }
	  else glColor3us(br, bg, bb);
	  glVertex3f(x1, val10, y0);
	  
	  if (js[slice + 1][i + 1] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice + 1][i + 1], &r, &g, &b);
	      glColor3us(r, g, b);
	    }
	  else glColor3us(br, bg, bb);
	  glVertex3f(x1, val11, y1);
	  
	  if (js[slice][i + 1] != NO_COLOR)
	    {
	      get_current_color(color_map(ss), js[slice][i + 1], &r, &g, &b);
	      glColor3us(r, g, b);
	    }
	  else glColor3us(br, bg, bb);
	  glVertex3f(x0, val01, y1);
	  
	  glEnd();
	}
    }
  for (i = 0; i < si->active_slices; i++) FREE(js[i]);
  FREE(js);
  js = NULL;
  glEndList();
}
#endif

static int make_spectrogram(chan_info *cp, snd_info *sp, snd_state *ss)
{
  sono_info *si;
  fft_info *fp;
  axis_info *fap;
  axis_context *ax;
  Float *fdata;
  Float matrix[9];
  Float xyz[3];
  Float xoff, yoff, x, y, xincr, yincr, x0, y0, binval, scl = 1.0;
  Float fwidth, fheight, zscl, yval, xval;
  int bins = 0, slice, i, j, xx, yy, old_with_gl = FALSE;
  if (chan_fft_in_progress(cp)) return(FALSE);
  si = (sono_info *)(cp->sonogram_data);
  if ((si) && (si->scale > 0.0))
    {
#if HAVE_GL
      /* experiments with lighting were a bust -- does not improve things (ditto fog, translucency, grid) 
       */
      /* can't figure out how to clear just the fft portion of the window
       *   tried: glScissor + glClear, 
       *          glRectf in bg color
       *          no action (something is still clearing the darn thing!)
       * TODO: get gl spectro to work in united channel cases
       */
      /* SOMEDAY: editable spectrogram (select, apply any selection-proc)
	 TODO: printing support (via pixmaps I guess) glReadBuffer(GL_BACK) then glReadPixels
	        glReadPixels(fap->graph_x0, 0, fap->width, fap->height, GL_RGB, GL_UNSIGNED_SHORT, array_for_pixels)
		then PS colorimage op [see gtkplotps.c, ps doc p 225]
	 TODO: multichannel resize: one chan ends up messed up until expose event; can't see why
      */
      if (((sp->nchans == 1) || (sp->channel_style == CHANNELS_SEPARATE)) &&
	  (color_map(ss) != BLACK_AND_WHITE) &&
	  (with_gl(ss)))
	{
	  unsigned short br = 65535, bg = 65535, bb = 65535;
#if USE_MOTIF
	  Colormap cmap;
	  XColor tmp_color;
	  Display *dpy;
#else
	  GdkColor *tmp_color;
#endif
	  fp = cp->fft;
	  fap = fp->axis; 
	  if (cp->printing) snd_warning(_("can't print openGL graphics yet"));
#if USE_MOTIF
	  glXMakeCurrent(MAIN_DISPLAY(ss), XtWindow(channel_graph(cp)), ss->sgx->cx);
#else
	  GL_MAKE_CURRENT(cp);
	  gdk_gl_drawable_wait_gdk(gtk_widget_get_gl_drawable(channel_graph(cp)));
#endif
	  if (cp->gl_fft_list == NO_LIST) 
	    cp->gl_fft_list = (int)glGenLists(1);
	  else
	    {
	      if (cp->fft_changed == FFT_CHANGED)
		{
		  glDeleteLists((GLuint)(cp->gl_fft_list), 1);
		  cp->gl_fft_list = (int)glGenLists(1);
		}
	    }
	  glEnable(GL_DEPTH_TEST);
	  glShadeModel(GL_SMOOTH);
	  glClearDepth(1.0);
#if USE_MOTIF
	  /* get the background color */
	  dpy = XtDisplay(MAIN_SHELL(ss));
	  cmap = DefaultColormap(dpy, DefaultScreen(dpy));
	  tmp_color.flags = DoRed | DoGreen | DoBlue;
	  if (cp == selected_channel(ss))
	    tmp_color.pixel = ss->sgx->selected_graph_color;
	  else tmp_color.pixel = ss->sgx->graph_color;
	  XQueryColor(dpy, cmap, &tmp_color);
	  br = tmp_color.red;
	  bg = tmp_color.green;
	  bb = tmp_color.blue;
	  glClearColor((float)(tmp_color.red) / 65535.0,
		       (float)(tmp_color.green) / 65535.0,
		       (float)(tmp_color.blue) / 65535.0,
		       0.0);
#else
	  if (cp == selected_channel(ss))
	    tmp_color = ss->sgx->selected_graph_color;
	  else tmp_color = ss->sgx->graph_color;
	  glClearColor((float)(tmp_color->red) / 65535.0,
		       (float)(tmp_color->green) / 65535.0,
		       (float)(tmp_color->blue) / 65535.0,
		       0.0);
#endif
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  if (cp->fft_changed == FFT_CHANGED)
	    {
	      gl_spectrogram(ss, si, cp->gl_fft_list, cp->spectro_cutoff, cp->fft_log_magnitude, cp->min_dB, br, bg, bb);
	    }
	  glViewport(fap->graph_x0, 0, fap->width, fap->height);
	  glMatrixMode(GL_PROJECTION);
	  glLoadIdentity();
	  /* glOrtho(-1.0, 1.0, -1.0, 1.0, 1.0, -1.0); */ /* this appears to be the default */
	  glRotatef(cp->spectro_x_angle, 1.0, 0.0, 0.0);
	  glRotatef(cp->spectro_y_angle, 0.0, 1.0, 0.0);
	  glRotatef(cp->spectro_z_angle, 0.0, 0.0, 1.0);
	  glScalef(cp->spectro_x_scale, cp->spectro_y_scale, cp->spectro_z_scale);
	  glCallList((GLuint)(cp->gl_fft_list));
	  fap->use_gl = TRUE;
	  make_axis_info(cp,
			 cp->axis->x0, cp->axis->x1,
			 SND_SRATE(sp) * cp->spectro_start / 2.0, SND_SRATE(sp) * cp->spectro_cutoff / 2.0,
			 _("time"),
			 cp->axis->x0, cp->axis->x1,
			 SND_SRATE(sp) * cp->spectro_start / 2.0, SND_SRATE(sp) * cp->spectro_cutoff / 2.0,
			 fap);
	  make_axes(cp, fap, X_AXIS_IN_SECONDS, DONT_CLEAR_GRAPH);
	  fap->use_gl = FALSE;
#if USE_MOTIF
	  if (ss->gl_has_double_buffer)
	    glXSwapBuffers(MAIN_DISPLAY(ss), XtWindow(channel_graph(cp)));
	  else glFlush();
#else
	  GL_SWAP_BUFFERS(cp);
	  gdk_gl_drawable_wait_gl(gtk_widget_get_gl_drawable(channel_graph(cp)));
#endif
#if DEBUGGING
	  {
	    GLenum errcode;
	    errcode = glGetError();
	    if (errcode != GL_NO_ERROR)
	      fprintf(stderr, "spectro GL: %s\n", gluErrorString(errcode));
	  }
#endif
	  /* if (cp->printing) make a pixmap of the graph and turn it into PS bits */

	  /* a kludge to get the normal graph drawn (again...) */
	  if (cp->graph_time_p)
	    display_channel_time_data(cp, cp->sound, cp->state); 
	  if (cp->graph_lisp_p)
	    display_channel_lisp_data(cp, cp->sound, cp->state); 
#if USE_MOTIF
	  return(XtAppPending(MAIN_APP(ss)) == 0); /* return true if there are no pending events to force current buffer to be displayed */
#else
	  return(TRUE);
#endif
	}
#endif
      old_with_gl = with_gl(ss);
      if (old_with_gl) set_with_gl(ss, FALSE); /* needed to fixup spectro angles/scales etc */
      if (cp->printing) ps_allocate_grf_points();
      scl = si->scale; /* unnormalized fft doesn't make much sense here (just washes out the graph) */
      fp = cp->fft;
      fap = fp->axis;
      bins = (int)(si->target_bins * cp->spectro_cutoff);
      fwidth = (fap->x_axis_x1 - fap->x_axis_x0);
      fheight = (fap->y_axis_y1 - fap->y_axis_y0); /* negative! */
      xincr = fwidth / (Float)bins;
      yincr = fheight / (Float)si->active_slices;
      x0 = (fap->x_axis_x0 + fap->x_axis_x1) * 0.5;
      y0 = (fap->y_axis_y0 + fap->y_axis_y1) * 0.5;
      if (!(cp->fft_log_magnitude))
	zscl = -(cp->spectro_z_scale * fheight / scl);
      else zscl = -(cp->spectro_z_scale * fheight);
      rotate_matrix(cp->spectro_x_angle, cp->spectro_y_angle, cp->spectro_z_angle,
		    cp->spectro_x_scale, cp->spectro_y_scale, zscl,
		    matrix);
      ax = copy_context(cp);
      if (color_map(ss) == BLACK_AND_WHITE)
	{
	  ss->stopped_explicitly = FALSE;
	  for (slice = 0, xoff = fap->x_axis_x0, yoff = fap->y_axis_y0; 
	       slice < si->active_slices;
	       slice++, yoff += yincr)
	    {
	      fdata = si->data[slice];
	      x = xoff;
	      y = yoff;
	      for (i = 0; i < bins; i++, x += xincr)
		{
		  xyz[0] = x - x0; 
		  xyz[1] = y - y0; 
		  if (!(cp->fft_log_magnitude))
		    xyz[2] = fdata[i];
		  else 
		    {
		      binval = fdata[i] / scl; 
		      xyz[2] = 1.0 - (in_dB(cp->min_dB, cp->lin_dB, binval)) / cp->min_dB;
		    }
		  rotate(xyz, matrix);
		  yval = xyz[1] + xyz[2];
		  xval = xyz[0];
		  set_grf_point((Locus)(xval + x0), i, 
				(Locus)(yval + y0));
		  if (cp->printing) 
		    ps_set_grf_point(ungrf_x(fap, (int)(xval + x0)), i, 
				     ungrf_y(fap, (int)(yval + y0)));
		}
	      draw_grf_points(cp, ax, bins, fap, 0.0, cp->transform_graph_style);
	      if (cp->printing) 
		{
		  ps_draw_grf_points(fap, bins, 0.0, cp->transform_graph_style, cp->dot_size);
		  check_for_event(ss);
		  if ((ss->stopped_explicitly) || (!(cp->active)))
		    {
		      ss->stopped_explicitly = FALSE;
		      report_in_minibuffer(sp, _("stopped"));
		      break;
		    }
		}
	    }
	}
      else
	{
	  /* spectrogram in various colors */
	  allocate_color_map(ss, color_map(ss));
	  ss->stopped_explicitly = FALSE;
	  for (slice = 0, xoff = fap->x_axis_x0, yoff = fap->y_axis_y0; 
	       slice < si->active_slices; 
	       slice++, yoff += yincr)
	    {
	      fdata = si->data[slice];
	      x = xoff;
	      y = yoff;
	      xyz[0] = x - x0; 
	      xyz[1] = y - y0; 
	      xyz[2] = fdata[0]; 
	      rotate(xyz, matrix);
	      xx = (int)(xyz[0] + x0); 
	      yy = (int)(xyz[1] + xyz[2] + y0);
	      for (i = 0; i < bins; i++, x += xincr)
		{
		  xyz[0] = x - x0; 
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
		  j = skew_color(ss, binval);
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
	      if (cp->printing) 
		{
		  check_for_event(ss);
		  if ((ss->stopped_explicitly) || (!(cp->active)))
		    {
		      ss->stopped_explicitly = FALSE;
		      report_in_minibuffer(sp, _("stopped"));
		      break;
		    }
		}
	    }
	  if (cp->printing) ps_reset_color();
	}
      if (cp->hookable) after_fft(cp, 1.0 / scl);
      if (old_with_gl) set_with_gl(ss, TRUE);
    }
  return(FALSE);
}

static void make_wavogram(chan_info *cp, snd_info *sp, snd_state *ss)
{
  Float xoff, x, y, x0, y0, xincr;
  Float width, height, zscl, yval, xval, binval;
  int i, j, yincr, yoff, xx, yy;
  Float matrix[9];
  Float xyz[3];
  snd_fd *sf;
  axis_info *ap;
  axis_context *ax;
  ap = cp->axis;
  if (sp) ap->losamp = (off_t)(ap->x0 * SND_SRATE(sp));
  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
  if (sf == NULL) return;
#if HAVE_GL
  if (((sp->nchans == 1) || (sp->channel_style == CHANNELS_SEPARATE)) &&
      (color_map(ss) != BLACK_AND_WHITE) &&
      (with_gl(ss)))
    {
      Float **samps;
      int **js;
      float x0, x1, y0, y1;
      Float xinc, yinc;
      /* each line is wavo_trace samps, there are (height / wave_hop) of these? */
      int lines, len;
      lines = (int)(ap->height / cp->wavo_hop);
      if (lines == 0) return;
      len = cp->wavo_trace;
      samps = (Float **)CALLOC(lines, sizeof(Float *));
      js = (int **)CALLOC(lines, sizeof(int *));
      for (i = 0; i < lines; i++)
	{
	  samps[i] = (Float *)CALLOC(len, sizeof(Float));
	  js[i] = (int *)CALLOC(len, sizeof(int));
	  for (j = 0; j < len; j++)
	    {
	      samps[i][j] = read_sample_to_float(sf);
	      js[i][j] = skew_color(ss, samps[i][j]);
	      if (js[i][j] < 0) js[i][j] = 0;
	    }
	}
#if USE_MOTIF
      glXMakeCurrent(MAIN_DISPLAY(ss), XtWindow(channel_graph(cp)), ss->sgx->cx);
#else
      GL_MAKE_CURRENT(cp);
      gdk_gl_drawable_wait_gdk(gtk_widget_get_gl_drawable(channel_graph(cp)));
#endif
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
      xinc = 2.0 / (float)len;
      yinc = 2.0 / (float)lines;
      for (j = 0, y0 = -1.0; j < lines - 1; j++, y0 += yinc)
	for (x0 = -1.0, i = 0; i < len - 1; i++, x0 += xinc)
	  {
	    unsigned short r, g, b;
	    glBegin(GL_POLYGON);
	    x1 = x0 + xinc;
	    y1 = y0 + yinc;

	    get_current_color(color_map(ss), js[j][i], &r, &g, &b);
	    glColor3us(r, g, b);
	    glVertex3f(x0, samps[j][i], y0);
		      
	    get_current_color(color_map(ss), js[j + 1][i], &r, &g, &b);
	    glColor3us(r, g, b);
	    glVertex3f(x1, samps[j+ 1][i], y0);
		      
	    get_current_color(color_map(ss), js[j + 1][i + 1], &r, &g, &b);
	    glColor3us(r, g, b);
	    glVertex3f(x1, samps[j + 1][i + 1], y1);
		      
	    get_current_color(color_map(ss), js[j][i + 1], &r, &g, &b);
	    glColor3us(r, g, b);
	    glVertex3f(x0, samps[j][i + 1], y1);
		      
	    glEnd();
	  }
#if USE_MOTIF
      if (ss->gl_has_double_buffer)
	glXSwapBuffers(MAIN_DISPLAY(ss), XtWindow(channel_graph(cp)));
      else glFlush();
#else
      GL_SWAP_BUFFERS(cp);
      gdk_gl_drawable_wait_gl(gtk_widget_get_gl_drawable(channel_graph(cp)));
#endif
      /* (set! (time-graph-type) graph-time-as-wavogram) */
      for (i = 0; i < lines; i++) 
	{
	  FREE(samps[i]);
	  FREE(js[i]);
	}
      FREE(samps);
      FREE(js);
      free_snd_fd(sf);
      return;
    }
#endif
  if (cp->printing) ps_allocate_grf_points();
  width = (ap->x_axis_x1 - ap->x_axis_x0);
  height = (ap->y_axis_y1 - ap->y_axis_y0); /* negative! */
  xincr = width / (Float)(cp->wavo_trace);
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
  if (color_map(ss) == BLACK_AND_WHITE)
    {
      for (xoff = ap->x_axis_x0, yoff = ap->y_axis_y0; 
	   yoff > ap->y_axis_y1; 
	   yoff += yincr)
	{
	  x = xoff;
	  y = yoff;
	  for (i = 0; i < cp->wavo_trace; i++, x += xincr)
	    {
	      xyz[0] = x - x0; 
	      xyz[1] = y - y0; 
	      xyz[2] = read_sample_to_float(sf);
	      rotate(xyz, matrix);
	      yval = xyz[1] + xyz[2];
	      xval = xyz[0];
	      set_grf_point((Locus)(xval + x0), i, 
			    (Locus)(yval + y0));
	      if (cp->printing) 
		ps_set_grf_point(ungrf_x(ap, (int)(xval + x0)), i, 
				 ungrf_y(ap, (int)(y0 + yval)));
	    }
	  draw_grf_points(cp, ax, cp->wavo_trace, ap, 0.0, cp->time_graph_style);
	  if (cp->printing) 
	    ps_draw_grf_points(ap, cp->wavo_trace, 0.0, cp->time_graph_style, cp->dot_size);
	}
    }
  else
    {
      allocate_color_map(ss, color_map(ss));
      for (xoff = ap->x_axis_x0, yoff = ap->y_axis_y0; 
	   yoff > ap->y_axis_y1; 
	   yoff += yincr)
	{
	  xx = -1;
	  x = xoff;
	  y = yoff;
	  yy = (int)y0; /* ? */
	  for (i = 0; i < cp->wavo_trace; i++, x += xincr)
	    {
	      binval = read_sample_to_float(sf);
	      xyz[0] = x - x0; 
	      xyz[1] = y - y0; 
	      xyz[2] = binval;
	      rotate(xyz, matrix);
	      yval = xyz[1] + xyz[2];
	      xval = xyz[0];
	      /* for color decision here we need absolute value of data */
	      if (binval < 0.0) binval = -binval;
	      if (xx != -1)
		{
		  j = skew_color(ss, binval);
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
      if (cp->printing) ps_reset_color();
    }
  free_snd_fd(sf);
}

static void make_lisp_graph(chan_info *cp, snd_info *sp, snd_state *ss, XEN pixel_list)
{
  lisp_grf *up;
  /* data can be evenly spaced data or an envelope (up->env_data) */
  axis_info *uap = NULL;
  int i, j, grf_len, graph, pixel_len = -1;
  axis_context *ax;
  color_t old_color = 0;
  Float x, y, samples_per_pixel = 1.0, xinc, start_x, xf, ymin, ymax, pinc;
  int x0, x1, y0, y1;
  Locus xi;
  up = (lisp_grf *)(cp->lisp_info);
  if (up) uap = up->axis; else return;
  if ((!uap) || (!uap->graph_active) || (up->len == NULL) || (up->len[0] <= 0)) return;
  if (cp->printing) ps_allocate_grf_points();
  if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
    ax = combined_context(cp); 
  else ax = copy_context(cp);
  if (cp->printing) ps_fg(uap, ax);
  if (up->env_data)
    {
      grf_len = up->len[0];
      x0 = local_grf_x(up->data[0][0], uap);
      y0 = local_grf_y(up->data[0][1], uap);
      if (cp->dot_size > 0) 
	draw_arc(ax, x0, y0, cp->dot_size);
      for (i = 2; i < grf_len; i += 2)
	{
	  x1 = local_grf_x(up->data[0][i], uap);
	  y1 = local_grf_y(up->data[0][i + 1], uap);
	  draw_line(ax, x0, y0, x1, y1);
	  if (cp->dot_size > 0) 
	    draw_arc(ax, x1, y1, cp->dot_size);
	  x0 = x1;
	  y0 = y1;
	}
    }
  else
    {
      if (XEN_LIST_P(pixel_list))
	pixel_len = XEN_LIST_LENGTH(pixel_list);
      if (up->graphs > 1) 
	old_color = get_foreground_color(cp, ax);
      for (graph = 0; graph < up->graphs; graph++)
	{
	  if ((pixel_len > graph) &&
	      (XEN_PIXEL_P(XEN_LIST_REF(pixel_list, graph))))
	    set_foreground_color(cp, ax, (color_t)XEN_UNWRAP_PIXEL(XEN_LIST_REF(pixel_list, graph)));
	  else
	    {
	      switch (graph)
		{
		case 0:  break;
		case 1:  set_foreground_color(cp, ax, (ss->sgx)->red);        break;
		case 2:  set_foreground_color(cp, ax, (ss->sgx)->green);      break;
		case 3:  set_foreground_color(cp, ax, (ss->sgx)->light_blue); break;
		case 4:  set_foreground_color(cp, ax, (ss->sgx)->yellow);     break;
		default: set_foreground_color(cp, ax, (ss->sgx)->black);      break;
		}
	    }
	  /* check for up->len[graph] > pixels available and use ymin ymax if needed */
	  samples_per_pixel = (Float)(up->len[graph]) / (Float)(uap->x_axis_x1 - uap->x_axis_x0);
	  if (samples_per_pixel < 4.0)
	    {
	      grf_len = up->len[graph];
	      start_x = uap->x0;
	      if (grf_len <= 1) 
		xinc = 1.0;
	      else xinc = (uap->x1 - uap->x0) / (Float)(grf_len - 1);
	      for (i = 0, x = start_x; i < grf_len; i++, x += xinc)
		{
		  y = up->data[graph][i];
		  set_grf_point(local_grf_x(x, uap), i, 
				local_grf_y(y, uap));
		  if (cp->printing) 
		    ps_set_grf_point(x, i, y);
		}
	      draw_grf_points(cp, ax, grf_len, uap, 0.0, cp->lisp_graph_style);
	      if (cp->printing) 
		ps_draw_grf_points(uap, grf_len, 0.0, cp->lisp_graph_style, cp->dot_size);
	    }
	  else
	    {
	      j = 0;
	      i = 0;
	      xf = 0.0;
	      x = 0.0;
	      pinc = samples_per_pixel / (Float)(up->len[graph]);
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
		      ymin = 32767.0;
		      ymax = -32768.0;
		    }
		}
	      draw_both_grf_points(cp, ax, j, cp->lisp_graph_style);
	      if (cp->printing) 
		ps_draw_both_grf_points(uap, j, cp->lisp_graph_style, cp->dot_size);
	    }
	}
      if (up->graphs > 1) set_foreground_color(cp, ax, old_color);
      if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
	{
	  copy_context(cp); /* reset for axes etc */
	  if (cp->printing) ps_reset_color();
	}
      if (cp->show_transform_peaks) 
	display_peaks(cp, uap, up->data[0], 1, up->len[0] - 1, samples_per_pixel, FALSE, 0.0);
    }
}

static void make_axes(chan_info *cp, axis_info *ap, int x_style, int erase_first)
{
  snd_state *ss;
  snd_info *sp;
  axis_context *ax;
  ss = cp->state;
  if (!(ap->ax))
    {
      ax = (axis_context *)CALLOC(1, sizeof(axis_context));
      ap->ax = ax;
    }
  else ax = ap->ax;
  sp = cp->sound;
  setup_axis_context(cp, ax);
  /* here is where the graph is cleared(!) -- only use of erase_rectangle */
  if (erase_first == CLEAR_GRAPH)
    erase_rectangle(cp, ap->ax, ap->graph_x0, ap->y_offset, ap->width, ap->height); 
  make_axes_1(ap, x_style, SND_SRATE(sp), cp->show_axes, cp->printing,
	      ((sp->channel_style != CHANNELS_COMBINED) || 
	       (cp->show_axes == SHOW_ALL_AXES) || 
	       (cp->chan == (sp->nchans - 1))));
}

static void draw_graph_cursor(chan_info *cp);

static void display_channel_data_with_size (chan_info *cp, snd_info *sp, snd_state *ss, 
					    int width, int height, int offset, 
					    int just_fft, int just_lisp, int just_time)
{
  /* this procedure is unnecessarily confusing! */
  int with_fft = FALSE, with_lisp = FALSE, with_time = FALSE, displays = 0, points, grflsp = 0;
  axis_info *ap = NULL;
  axis_info *fap = NULL;
  axis_info *uap = NULL;
  fft_info *fp = NULL;
  lisp_grf *up = NULL;
  XEN res = XEN_FALSE;
  if ((cp->hookable) && 
      (!(ss->graph_hook_active)) &&
      (XEN_HOOKED(graph_hook)))
    {
      ss->graph_hook_active = TRUE;
      res = run_progn_hook(graph_hook,
			   XEN_LIST_4(C_TO_SMALL_XEN_INT((cp->sound)->index),
				      C_TO_SMALL_XEN_INT(cp->chan),
				      C_TO_XEN_DOUBLE((cp->axis)->y0),
				      C_TO_XEN_DOUBLE((cp->axis)->y1)),
			   S_graph_hook);
      /* (add-hook! graph-hook (lambda (a b c d) (snd-print (format #f "~A ~A ~A ~A" a b c d)))) */
      ss->graph_hook_active = FALSE;
      if (XEN_TRUE_P(res)) return;
    }
  ap = cp->axis;
  if (ap == NULL) return;
  /* now check for fft/wave/user-function decisions */
  ap->height = height;
  ap->window_width = width;
  ap->width = width;
  ap->y_offset = offset;

  if (cp->graph_time_p) 
    {
      displays++;
      with_time = TRUE;
    }

  grflsp = ((cp->graph_lisp_p) || (XEN_HOOKED(lisp_graph_hook)));
  if (grflsp)
    {
      displays++;
      up = (lisp_grf *)(cp->lisp_info);
      if (up == NULL)
	{
	  /* this should only happen the first time such a graph is needed */
	  cp->lisp_info = (lisp_grf *)CALLOC(1, sizeof(lisp_grf));
	  up = (lisp_grf *)(cp->lisp_info);
	  up->axis = make_axis_info(cp, 0.0, 1.0, -1.0, 1.0, "dummy axis", 0.0, 1.0, -1.0, 1.0, NULL);
	}
      if (up)
	{
	  uap = up->axis;
	  if (uap)
	    {
	      with_lisp = TRUE;
	      uap->height = height;
	      uap->y_offset = offset;
	      uap->width = width;
	      uap->window_width = width;
	    }
	}
    }

  if (cp->graph_transform_p) 
    {
      displays++;
      fp = cp->fft;
      if (fp)
	{
	  fap = fp->axis; 
	  if (fap)
	    {
	      with_fft = TRUE;
	      fap->height = height;
	      fap->y_offset = offset;
	      fap->width = width;
	      fap->window_width = width;
	    }
	}
    }

  if (displays == 0)
    {
      clear_window(ap->ax);
      return;
    }
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

  if ((!just_fft) && (!just_lisp))
    {
      marks_off(cp);
      clear_mix_tags(cp);

      if (with_time)
	{
	  if (cp->time_graph_type == GRAPH_AS_WAVOGRAM)
	    {
	      if (ap->y_axis_y0 == ap->y_axis_y1) 
		make_axes(cp, ap, cp->x_axis_style, DONT_CLEAR_GRAPH); /* first time needs setup */
	      ap->y0 = ap->x0;
	      ap->y1 = ap->y0 + (Float)(cp->wavo_trace * (ap->y_axis_y0 - ap->y_axis_y1)) / ((Float)(cp->wavo_hop) * SND_SRATE(sp));
	      ap->x1 = ap->x0 + (double)(cp->wavo_trace) / (double)SND_SRATE(sp);
	    }
	  if ((with_gl(ss) == FALSE) || 
	      (cp->time_graph_type != GRAPH_AS_WAVOGRAM) ||
	      (color_map(ss) == BLACK_AND_WHITE) ||
	      ((sp->nchans > 1) && (sp->channel_style != CHANNELS_SEPARATE)))
	    make_axes(cp, ap,
		      cp->x_axis_style,
		      (((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH));
	  cp->cursor_visible = FALSE;
	  cp->selection_visible = FALSE;
	  points = make_graph(cp, sp, ss);
	  if (points == 0) return;
	  if ((cp->mixes) &&
	      (cp->mix_dragging)) 
	    mix_save_graph(cp_to_mix_context(cp), points);
	  if (cp->cursor_on) draw_graph_cursor(cp);
	}
    }

  if ((with_fft) && 
      (!just_lisp) && (!just_time))
    {
      if ((with_gl(ss) == FALSE) || 
	  (cp->transform_graph_type != GRAPH_AS_SPECTROGRAM) ||
	  (color_map(ss) == BLACK_AND_WHITE) ||
	  ((sp->nchans > 1) && (sp->channel_style != CHANNELS_SEPARATE)))
	make_axes(cp, fap,
		  ((cp->x_axis_style == X_AXIS_IN_SAMPLES) || 
		   (cp->x_axis_style == X_AXIS_IN_BEATS)) ? X_AXIS_IN_SECONDS : (cp->x_axis_style),
#if USE_MOTIF
		  (((cp->chan == (sp->nchans - 1)) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH)
		  /* Xt documentation says the most recently added work proc runs first, but we're
		   *   adding fft work procs in channel order, normally, so the channel background
		   *   clear for the superimposed case needs to happen on the highest-numbered 
		   *   channel, since it will complete its fft first.  It also needs to notice the
		   *   selected background, if any of the current sound's channels is selected.
		   */
#else
		  (((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH)
		  /* In Gtk+ (apparently) the first proc added is run, not the most recent */
#endif
		  );
	  
      if ((!with_time) || (just_fft))
	{ /* make_graph does this -- sets losamp needed by fft to find its starting point */
	  ap->losamp = (off_t)(ap->x0 * (double)SND_SRATE(sp));
	  ap->hisamp = (off_t)(ap->x1 * (double)SND_SRATE(sp));
	}
      switch (cp->transform_graph_type)
	{
	case GRAPH_ONCE:
	  make_fft_graph(cp, sp,
			 cp->fft->axis,
			 (sp->channel_style == CHANNELS_SUPERIMPOSED) ? combined_context(cp) : copy_context(cp),
			 cp->hookable);
	  break;
	case GRAPH_AS_SONOGRAM:
	  make_sonogram(cp, sp, ss);
	  break;
	case GRAPH_AS_SPECTROGRAM:
#if HAVE_GL
	  if (make_spectrogram(cp, sp, ss))
	    glDrawBuffer(GL_BACK); /* was make_spectrogram(cp, sp, ss); */
#else
	  make_spectrogram(cp, sp, ss);
#endif
	  break;
	}
    }

  if ((with_lisp) && 
      (!just_fft) && (!just_time))
    {
      int pixel_loc = -1;
      XEN pixel_list = XEN_FALSE;
      if ((just_lisp) || ((!with_time) && (!(with_fft))))
	{
	  ap->losamp = (off_t)(ap->x0 * (double)SND_SRATE(sp));
	  ap->hisamp = (off_t)(ap->x1 * (double)SND_SRATE(sp));
	}
      if ((cp->hookable) &&
	  (!(ss->lisp_graph_hook_active)) &&
	  (XEN_HOOKED(lisp_graph_hook)))
	{
	  ss->lisp_graph_hook_active = TRUE;
	  /* inadvertent recursive call here can hang entire computer */
	  pixel_list = run_progn_hook(lisp_graph_hook,
				      XEN_LIST_2(C_TO_SMALL_XEN_INT((cp->sound)->index),
						 C_TO_SMALL_XEN_INT(cp->chan)),
				      S_lisp_graph_hook);
	  ss->lisp_graph_hook_active = FALSE;
	  if (!(XEN_FALSE_P(pixel_list))) pixel_loc = snd_protect(pixel_list);
	}
      if (up != (lisp_grf *)(cp->lisp_info))
	up = (lisp_grf *)(cp->lisp_info);
      if (uap != up->axis)
	uap = up->axis;
      /* if these were changed in the hook function, the old fields should have been saved across the change (g_graph below) */
      make_axes(cp, uap, /* defined in this file l 2293 */
		X_AXIS_IN_LENGTH,
		(((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)) ? CLEAR_GRAPH : DONT_CLEAR_GRAPH));
      if (XEN_PROCEDURE_P(pixel_list))
	XEN_CALL_0(pixel_list, "lisp-graph");
      else make_lisp_graph(cp, sp, ss, pixel_list);
      if (!(XEN_FALSE_P(pixel_list))) snd_unprotect_at(pixel_loc);
    }
  
  if ((!just_lisp) && (!just_fft))
    {
      if (with_time)
	{
	  display_selection(cp);
	  display_channel_marks(cp);
	  if (cp->show_y_zero) display_zero(cp);
	  if ((cp->mixes)) display_channel_mixes(cp);
	}
      if ((sp->channel_style != CHANNELS_SUPERIMPOSED) && (height > 10))
	display_channel_id(cp, height + offset, sp->nchans);
      if ((cp->hookable) &&
	  (XEN_HOOKED(after_graph_hook)))
	run_hook(after_graph_hook,
		 XEN_LIST_2(C_TO_SMALL_XEN_INT((cp->sound)->index),
			    C_TO_SMALL_XEN_INT(cp->chan)),
		 S_after_graph_hook);
      /* (add-hook! after-graph-hook (lambda (a b) (snd-print (format #f "~A ~A" a b)))) */
    } 
}

static void display_channel_data_1 (chan_info *cp, snd_info *sp, snd_state *ss, int just_fft, int just_lisp, int just_time)
{
  int width, height, offset, full_height, y0, y1, bottom, top;
  Float val, size, chan_height;
  axis_info *ap;
  if ((!(sp->inuse)) ||
      (!(cp->active)) ||
      (!(sp->active)))
    return;
  if ((sp->channel_style == CHANNELS_SEPARATE) || (sp->nchans == 1))
    {
      width = widget_width(channel_graph(cp));
      height = widget_height(channel_graph(cp));
      if ((height > 5) && (width > 5))
	display_channel_data_with_size(cp, sp, ss, width, height, 0, just_fft, just_lisp, just_time);
    }
  else
    {
      /* all chans in one graph widget, sy->scroll entire set, zy->zoom entire set etc */
      /* updates are asynchronous (dependent on background ffts etc), so we can't do the whole window at once */
      /* complication here is that we're growing down from the top, causing endless confusion */
      width = widget_width(channel_graph(sp->chans[0])) - (2 * ss->position_slider_width);
      if (width <= 0) return;
      height = widget_height(channel_graph(sp->chans[0]));
      cp->height = height;
      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
	display_channel_data_with_size(cp, sp, ss, width, height, 0, just_fft, just_lisp, just_time);
      else
	{
	  val = gsy_value(sp->chans[0]);
	  size = gsy_size(sp->chans[0]);
	  full_height = (int)((Float)height / size);
	  chan_height = (Float)full_height / (Float)(sp->nchans);
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
	  if ((cp->chan == (sp->nchans - 1)) && 
	      ((offset + chan_height) < height))
	    chan_height = height - offset;
	  if (((y0 < top) && (y0 >= bottom)) || 
	      ((y1 > bottom) && (y1 <= top)))
	    display_channel_data_with_size(cp, sp, ss, width, (int)chan_height, offset, just_fft, just_lisp, just_time);
	  else 
	    {
	      ap = cp->axis;
	      ap->y_offset = offset; /* needed for mouse click channel determination */
	    }
	}
    }
}

void display_channel_fft_data(chan_info *cp, snd_info *sp, snd_state *ss)
{
  display_channel_data_1(cp, sp, ss, TRUE, FALSE, FALSE);
}

static void display_channel_lisp_data(chan_info *cp, snd_info *sp, snd_state *ss)
{
  display_channel_data_1(cp, sp, ss, FALSE, TRUE, FALSE);
}

static void display_channel_time_data(chan_info *cp, snd_info *sp, snd_state *ss)
{
  display_channel_data_1(cp, sp, ss, FALSE, FALSE, TRUE);
}

void display_channel_data(chan_info *cp, snd_info *sp, snd_state *ss)
{
  display_channel_data_1(cp, sp, ss, FALSE, FALSE, FALSE);
}


/* ---------------- CHANNEL CURSOR ---------------- */

static void draw_cursor(chan_info *cp)
{
  axis_info *ap;
  axis_context *ax;
  ap = cp->axis;
  ax = cursor_context(cp);
  switch (cp->cursor_style)
    {
    case CURSOR_CROSS:
      draw_line(ax, cp->cx, cp->cy - cp->cursor_size, cp->cx, cp->cy + cp->cursor_size);
      draw_line(ax, cp->cx - cp->cursor_size, cp->cy, cp->cx + cp->cursor_size, cp->cy);
      break;
    case CURSOR_LINE:
      draw_line(ax, cp->cx, ap->y_axis_y0, cp->cx, ap->y_axis_y1);
      break;
    case CURSOR_PROC:
      XEN_CALL_3(cp->cursor_proc,
		 C_TO_XEN_INT(cp->sound->index),
		 C_TO_XEN_INT(cp->chan),
		 C_TO_XEN_INT(TIME_AXIS_INFO),
		 "cursor-style procedure");
      break;
    }
}

static void draw_graph_cursor(chan_info *cp)
{
  axis_info *ap;
  ap = cp->axis;
  if ((CURSOR(cp) < ap->losamp) || (CURSOR(cp) > ap->hisamp)) return;
  if (cp->cursor_visible) draw_cursor(cp);
  cp->cx = local_grf_x((double)(CURSOR(cp)) / (double)SND_SRATE(cp->sound), ap); /* not float -- this matters in very long files (i.e. > 40 minutes) */
  if (cp->just_zero)
    cp->cy = local_grf_y(0.0, ap);
  else cp->cy = local_grf_y(chn_sample(CURSOR(cp), cp, cp->edit_ctr), ap);
  draw_cursor(cp);
  cp->cursor_visible = TRUE;
}

int cursor_decision(chan_info *cp)
{
  off_t len;
  len = CURRENT_SAMPLES(cp);
  if (CURSOR(cp) >= len) CURSOR(cp) = len - 1; /* zero based, but in 0-length files, len = 0 */
  if (CURSOR(cp) < 0) CURSOR(cp) = 0;        /* perhaps the cursor should be forced off in empty files? */
  if (CURSOR(cp) < (cp->axis)->losamp)
    {
      if (CURSOR(cp) == 0) return(CURSOR_ON_LEFT);
      else 
	{
	  if (cp->sound->playing)
	    return(CURSOR_ON_RIGHT);
	  return(CURSOR_IN_MIDDLE);
	}
    }
  if (CURSOR(cp) > (cp->axis)->hisamp)
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

void handle_cursor(chan_info *cp, int redisplay)
{
  axis_info *ap;
  axis_context *ax;
  snd_info *sp;
  double gx = 0.0;
  if (cp == NULL) return;
  if (redisplay != KEYBOARD_NO_ACTION)
    {
      sp = cp->sound;
      if (cp->verbose_cursor)
	{
	  show_cursor_info(cp); 
	  sp->minibuffer_on = MINI_CURSOR;
	} 
      if (redisplay != CURSOR_IN_VIEW)
	{
	  ap = cp->axis;
	  if (cp->cursor_visible)
	    {
	      ax = cursor_context(cp);
	      draw_line(ax, cp->cx, cp->cy - cp->cursor_size, cp->cx, cp->cy + cp->cursor_size);
	      draw_line(ax, cp->cx - cp->cursor_size, cp->cy, cp->cx + cp->cursor_size, cp->cy);
	      cp->cursor_visible = FALSE; /* don't redraw at old location */
	    }
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
	    }
	  if (gx < 0.0) gx = 0.0;
	  reset_x_display(cp, (gx - ap->xmin) / ap->x_ambit, ap->zx);
	}
      else {if (cp->cursor_on) draw_graph_cursor(cp);}
    }
  {
    /* not sure about this */
    int i;
    for (i = 0; i < cp->edit_size; i++) cp->cursors[i] = CURSOR(cp);
  }
  update_possible_selection_in_progress(CURSOR(cp));
}

void cursor_moveto(chan_info *cp, off_t samp)
{
  snd_info *sp;
  chan_info *ncp;
  sync_info *si;
  int i;
  sp = cp->sound;
  if ((sp) && (sp->sync != 0))
    {
      si = snd_sync(cp->state, sp->sync);
      for (i = 0; i < si->chans; i++)
	{
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

void cursor_move(chan_info *cp, off_t samps)
{
  cursor_moveto(cp, CURSOR(cp) + samps);
}

void cursor_moveto_without_verbosity(chan_info *cp, off_t samp)
{
  int old_verbose, old_sync;
  old_verbose = cp->verbose_cursor;
  old_sync = cp->sound->sync;
  cp->verbose_cursor = 0;
  cp->sound->sync = 0;
  cursor_moveto(cp, samp);
  cp->verbose_cursor = old_verbose;
  cp->sound->sync = old_sync;
}

void show_cursor_info(chan_info *cp)
{
  char *expr_str;
  snd_info *sp;
  chan_info *ncp;
  Float y, absy;
  int digits, i;
  off_t samp;
  char *s1, *s2;
  sp = cp->sound;
  if ((sp->sync != 0) && (cp->chan != 0)) return;
  samp = CURSOR(cp);
  y = chn_sample(samp, cp, cp->edit_ctr);
  absy = fabs(y);
  if (absy < .0001) digits = 4;
  else if (absy<.001) digits = 3;
  else digits = 2;
  expr_str = (char *)CALLOC(PRINT_BUFFER_SIZE,sizeof(char));
  if (sp->nchans == 1)
    mus_snprintf(expr_str, PRINT_BUFFER_SIZE, _("cursor at %s (sample " PRId64 ") = %s"),
		 s1 = prettyf((double)samp / (double)SND_SRATE(sp), 2),
		 samp,
		 s2 = prettyf(y, digits));
  else
    {
      if (sp->sync == 0)
	mus_snprintf(expr_str, PRINT_BUFFER_SIZE, _("chan %d, cursor at %s (sample " PRId64 ") = %s"),
		     cp->chan + 1,
		     s1 = prettyf((double)samp / (double)SND_SRATE(sp), 2),
		     samp,
		     s2 = prettyf(y, digits));
      else
	{
	  /* in this case, assume we show all on chan 0 and ignore the call otherwise (see above) */
	  /* "cursor at..." then list of values */
	  mus_snprintf(expr_str, PRINT_BUFFER_SIZE, _("cursor at %s (sample " PRId64 "): %s"),
		       s1 = prettyf((double)samp / (double)SND_SRATE(sp), 2),
		       samp,
		       s2 = prettyf(y, digits));
	  for (i = 1; i < sp->nchans; i++)
	    {
	      strcat(expr_str, ", ");
	      FREE(s2);
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
	      strcat(expr_str, s2);
	    }
	}
    }
  set_minibuffer_string(sp, expr_str);
  sp->minibuffer_on = MINI_CURSOR;
  FREE(expr_str);
  FREE(s1);
  FREE(s2);
}

void goto_graph(chan_info *cp)
{
  snd_info *sp;
  if ((cp) && (!(cp->squelch_update)))
    {
      sp = cp->sound;
      if ((cp->chan == 0) || (sp->channel_style == CHANNELS_SEPARATE))
	goto_window(channel_graph(cp));
      else goto_window(channel_graph(sp->chans[0]));
    }
}


/* ---------------- graphics callbacks ---------------- */

#define SLOPPY_MOUSE 10

static int within_graph(chan_info *cp, int x, int y)
{
  axis_info *ap;
  fft_info *fp;
  int x0, x1, y0, y1;
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
	return(WAVE);
    }
  if (cp->graph_transform_p)
    {
      fp = cp->fft;
      ap = fp->axis;
      /* look first for on-axis (axis drag) mouse */
#if HAVE_GL
      if ((cp->transform_graph_type == GRAPH_AS_SPECTROGRAM) && (ap->used_gl))
	{
	  GLdouble xx;
	  xx = unproject2x(x, y);
	  if ((xx > -0.7) && (xx < -0.49))
	    return(FFT_AXIS);
	}
#endif
      if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
	{
	  if (((x >= ap->x_axis_x0) && (x <= ap->x_axis_x1)) && 
	      ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y0)))
	    return(FFT_AXIS);
	}
      else
	{
	  if (((x0 <= ap->x_axis_x0) && (x1 >= ap->x_axis_x0)) && 
	      ((y <= ap->y_axis_y0) && (y >= ap->y_axis_y1)))
	    return(FFT_AXIS);
	}
      /* now check within fft graph */
      if (((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0)) && 
	  ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1)))
	return(FFT_MAIN);
    }
  if (((cp->graph_lisp_p) || (XEN_HOOKED(lisp_graph_hook))) && (cp->lisp_info))
    {
      ap = (cp->lisp_info)->axis;
      if (((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0)) && 
	  ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1)))
	return(LISP);
    }
  return(NOGRAPH);
}

static char *describe_fft_point(chan_info *cp, int x, int y)
{
  Float xf, yf;
  axis_info *ap;
  fft_info *fp;
  sono_info *si;
  int ind, time;
  fp = cp->fft;
  ap = fp->axis;
  if (x < ap->x_axis_x0) x = ap->x_axis_x0; else if (x > ap->x_axis_x1) x = ap->x_axis_x1;
  if (ap->x_axis_x1 == ap->x_axis_x0) return(copy_string("?"));
  xf = ap->x0 + (ap->x1 - ap->x0) * (Float)(x - ap->x_axis_x0) / (Float)(ap->x_axis_x1 - ap->x_axis_x0);
  if (cp->fft_log_frequency)                                /* map axis x1 = 1.0 to srate/2 */
    xf = ((exp(xf * log(LOG_FACTOR + 1.0)) - 1.0) / LOG_FACTOR) * SND_SRATE(cp->sound) * 0.5 * cp->spectro_cutoff; 
  if (cp->transform_graph_type == GRAPH_ONCE)                          /* fp->data[bins] */
    {
      if (cp->transform_type == FOURIER)
	ind = (int)((fp->current_size * xf) / (Float)SND_SRATE(cp->sound));
      else ind = (int)xf;
      if (ind >= fp->current_size) ind = fp->current_size - 1;
      return(mus_format(_("(%.1f%s, transform val: %.3f%s (raw: %.3f)"),
			xf,
			((cp->transform_type == AUTOCORRELATION) ? _(" samps") : _(" Hz")),
			(cp->fft_log_magnitude) ? in_dB(cp->min_dB, cp->lin_dB, (fp->data[ind] * fp->scale)) : (fp->data[ind] * fp->scale),
			(cp->fft_log_magnitude) ? _("dB") : "",
			fp->data[ind]));
    }
  else 
    {
      if (cp->transform_graph_type == GRAPH_AS_SONOGRAM) 	  /* si->data[slices][bins] */
	{
	  if (y < ap->y_axis_y1) y = ap->y_axis_y1; else if (y > ap->y_axis_y0) y = ap->y_axis_y0;
	  if (ap->y_axis_y1 == ap->y_axis_y0) return(copy_string("?"));
	  yf = ap->y0 + (ap->y1 - ap->y0) * (Float)(y - ap->y_axis_y0) / (Float)(ap->y_axis_y1 - ap->y_axis_y0);
	  si = (sono_info *)(cp->sonogram_data);
	  if (cp->transform_type == FOURIER)
	    ind = (int)((fp->current_size * yf) / (Float)SND_SRATE(cp->sound));
	  else ind = (int)yf;
	  if (ind >= si->total_bins) ind = si->total_bins - 1;
	  time = (int)(si->target_slices * (Float)(x - ap->x_axis_x0) / (Float)(ap->x_axis_x1 - ap->x_axis_x0));
	  if (time >= si->total_slices) time = si->total_slices - 1;
	  return(mus_format(_("(time: %.2f, freq: %.1f, val: %.3f%s (raw: %.3f))"),
			    xf, yf,
			    (cp->fft_log_magnitude) ? in_dB(cp->min_dB, cp->lin_dB, si->data[time][ind] / si->scale) : (si->data[time][ind] / si->scale),
			    (cp->fft_log_magnitude) ? _("dB") : "",
			    si->data[time][ind]));
	}
    }
  return(copy_string("?"));
}

void fftb(chan_info *cp, int on)
{
  cp->graph_transform_p = on;
  set_toggle_button(channel_f(cp), on, FALSE, (void *)cp);
  calculate_fft(cp);
}

void waveb(chan_info *cp, int on)
{
  cp->graph_time_p = on;
  set_toggle_button(channel_w(cp), on, FALSE, (void *)cp);
  update_graph(cp);
}

static void propagate_wf_state(snd_info *sp)
{
  int i, w, f;
  chan_info *cp;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      cp = sp->chans[0];
      w = cp->graph_time_p;
      f = cp->graph_transform_p;
      for (i = 1; i < sp->nchans; i++) 
	{
	  cp = sp->chans[i];
	  cp->graph_time_p = w;
	  cp->graph_transform_p = f;
	  set_toggle_button(channel_f(cp), f, FALSE, (void *)cp);
	  set_toggle_button(channel_w(cp), w, FALSE, (void *)cp);
	}
      for_each_sound_chan(sp, update_graph);
    }
}

void f_button_callback(chan_info *cp, int on, int with_control)
{
  snd_info *sp;
  int i;
  chan_info *ncp;
  cp->graph_transform_p = on;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    propagate_wf_state(sp);
  else
    {
      update_graph(cp);
      if (with_control)
	{
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->graph_transform_p = on;
#if USE_GTK
		  set_toggle_button(channel_f(ncp), on, TRUE, (void *)cp);
#else
		  set_toggle_button(channel_f(ncp), on, FALSE, (void *)cp);
#endif
		  update_graph(ncp);
		}
	    }
	}
      goto_graph(cp);
    }
}

void w_button_callback(chan_info *cp, int on, int with_control)
{
  snd_info *sp;
  int i;
  chan_info *ncp;
  cp->graph_time_p = on;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    propagate_wf_state(sp);
  else
    {
      update_graph(cp);
      if (with_control)
	{
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->graph_time_p = on;
#if USE_GTK
		  set_toggle_button(channel_w(ncp), on, TRUE, (void *)cp);
#else
		  set_toggle_button(channel_w(ncp), on, FALSE, (void *)cp);
#endif
		  update_graph(ncp);
		}
	    }
	}
      goto_graph(cp);
    }
}

void edit_select_callback(chan_info *cp, int ed, int with_control)
{
  if (ed != cp->edit_ctr)
    {
      if (cp->edit_ctr > ed)
	{
	  if (with_control)
	    undo_edit_with_sync(cp, cp->edit_ctr - ed);
	  else undo_edit(cp, cp->edit_ctr - ed);
	}
      else 
	{
	  if (with_control)
	    redo_edit_with_sync(cp, ed - cp->edit_ctr);	      
	  else redo_edit(cp, ed - cp->edit_ctr);
	}
    }
  /* don't let this list trap all subsequent clicks and keypresses! */
  goto_graph(cp);
}

int key_press_callback(chan_info *ncp, int x, int y, int key_state, int keysym)
{
  /* called by every key-intercepting widget in the entire sound pane */
  chan_info *cp;
  snd_info *sp;
  cp = virtual_selected_channel(ncp);
  sp = cp->sound;
  select_channel(sp, cp->chan);
  if (((cp->graph_lisp_p) || (XEN_HOOKED(lisp_graph_hook))) &&
      (within_graph(cp, x, y) == LISP) &&
      (XEN_HOOKED(key_press_hook)))
    {
      /* return TRUE to keep this key press from being passed to keyboard_command */
      XEN res = XEN_FALSE;
      res = run_or_hook(key_press_hook,
			XEN_LIST_4(C_TO_SMALL_XEN_INT(sp->index),
				   C_TO_SMALL_XEN_INT(cp->chan),
				   C_TO_XEN_INT(keysym),
				   C_TO_XEN_INT(key_state)),
			S_key_press_hook);
      if (XEN_TRUE_P(res))
	return(FALSE);
    }
  keyboard_command(cp, keysym, key_state);
  /* if lisp graph has cursor? */
  return(FALSE);
}

static chan_info *which_channel(snd_info *sp, int y)
{
  int i;
  chan_info *cp, *ncp;
  axis_info *ap;
  ncp = NULL;
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      ap = cp->axis;
      if (y < ap->y_offset) return(ncp);
      ncp = cp;
    }
  return(ncp);
}

static Float fft_axis_extent(chan_info *cp)
{
  axis_info *ap;
  fft_info *fp;
  fp = cp->fft;
  ap = fp->axis;
  if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
    return((Float)(ap->x_axis_x1 - ap->x_axis_x0));
  else return((Float)(ap->y_axis_y0 - ap->y_axis_y1));
}

static void calculate_syncd_fft(chan_info *cp, void *ptr)
{
  if (cp->sound->sync == (*((int *)ptr))) calculate_fft(cp);
}

static int dragged = FALSE;
static Tempus mouse_down_time;
static mark *mouse_mark = NULL;
static mark *play_mark = NULL;
static int click_within_graph = NOGRAPH;
static int fft_axis_start = 0;
#if HAVE_GL
  static Float fft_faxis_start = 0.0;
#endif
static int mix_tag = NO_MIX_TAG;
static chan_info *dragged_cp;

#ifdef MAC_OSX
static int press_x, press_y;
#endif

void graph_button_press_callback(chan_info *cp, int x, int y, int key_state, int button, Tempus time)
{
  snd_info *sp;
  sp = cp->sound;
  /* if combining, figure out which virtual channel the mouse is in */
  if (sp->channel_style == CHANNELS_COMBINED) cp = which_channel(sp, y);
  mouse_down_time = time;
#ifdef MAC_OSX
  press_x = x;
  press_y = y;
#endif
  select_channel(sp, cp->chan);
  dragged_cp = cp;
  dragged = FALSE;
  finish_selection_creation();
  mouse_mark = hit_mark(cp, x, y, key_state);
  if (mouse_mark == NULL) 
    play_mark = hit_triangle(cp, x, y);
  click_within_graph = within_graph(cp, x, y);
  if (click_within_graph == FFT_AXIS) 
    {
      if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
	{
#if HAVE_GL
	  if ((with_gl(cp->state)) && (cp->transform_graph_type == GRAPH_AS_SPECTROGRAM))
	    fft_faxis_start = unproject2y(x, y);
#endif
	  fft_axis_start = x;
	}
      else fft_axis_start = y;
    }
  else
    {
      if (click_within_graph == LISP)
	{
	  if (XEN_HOOKED(mouse_press_hook))
	    run_hook(mouse_press_hook,
		     XEN_LIST_6(C_TO_SMALL_XEN_INT(sp->index),
				C_TO_SMALL_XEN_INT(cp->chan),
				C_TO_XEN_INT(button),
				C_TO_XEN_INT(key_state),
				C_TO_XEN_DOUBLE(ungrf_x((cp->lisp_info)->axis, x)),
				C_TO_XEN_DOUBLE(ungrf_y((cp->lisp_info)->axis, y))),
		     S_mouse_press_hook);
	}
      else
	{
	  if ((mouse_mark == NULL) && 
	      (play_mark == NULL))
	    mix_tag = hit_mix(cp, x, y);
	}
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
  snd_state *ss;
  axis_info *ap;
  mark *old_mark;
  int actax;
  off_t samps;
  char *str = NULL;
  sp = cp->sound;
  ss = cp->state;
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
	  old_mark = sp->playing_mark; /* needed because stop_playing clobbers sp->playing_mark */
	  if (sp->playing)
	    {
	      stop_playing_sound(sp);
	      set_play_button(sp, FALSE);
	    }
	  if (play_mark != old_mark)
	    {
	      if (play_mark->sync)
		play_syncd_mark(cp, play_mark);
	      else 
		{
		  if (key_state & snd_ControlMask)
		    play_sound(sp, play_mark->samp, NO_END_SPECIFIED, TRUE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "play button", 0);
		  else play_channel(cp, play_mark->samp, NO_END_SPECIFIED, TRUE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "play button", 0);
		}
	      sp->playing_mark = play_mark;
	    }
	  else sp->playing_mark = NULL;
	  play_mark = NULL;
	}
      else
	{
	  actax = within_graph(cp, x, y);

	  if ((XEN_HOOKED(mouse_click_hook)) &&
	      (XEN_TRUE_P(run_or_hook(mouse_click_hook,
				      XEN_LIST_7(C_TO_SMALL_XEN_INT(sp->index),
						 C_TO_SMALL_XEN_INT(cp->chan),
						 C_TO_XEN_INT(button),
						 C_TO_XEN_INT(key_state),
						 C_TO_XEN_INT(x),
						 C_TO_XEN_INT(y),
						 C_TO_XEN_INT((actax == WAVE) ? 
							      TIME_AXIS_INFO : ((actax == LISP) ? 
										LISP_AXIS_INFO : TRANSFORM_AXIS_INFO))),
				      S_mouse_click_hook))))
	    return;

	  if (actax == WAVE)
	    {
	      if (button == BUTTON_2) /* the middle button */
		{
		  cp->cursor_on = TRUE;
		  cursor_moveto(cp, 
				snd_round_off_t(ungrf_x(cp->axis, x) * 
						(double)SND_SRATE(sp)));
		  /* draw_graph_cursor(cp); */
		  paste_region(stack_position_to_id(0), cp, "Btn2");
		}
	      else 
		{
		  if (key_state & (snd_ShiftMask | snd_ControlMask | snd_MetaMask))
		    {
		      /* zoom request -> each added key zooms closer, as does each successive click */
		      ap = cp->axis;
		      samps = CURRENT_SAMPLES(cp);
		      if ((samps > 0) && ((ap->zx * (double)samps) > 1.0))
			{
			  if (key_state & snd_ShiftMask) ap->zx *= .5;
			  if (key_state & snd_ControlMask) ap->zx *= .5;
			  if (key_state & snd_MetaMask) ap->zx *= .5;
			  ap->sx = (((double)(CURSOR(cp)) / (double)SND_SRATE(sp) - 
				     ap->zx * 0.5 * (ap->xmax - ap->xmin)) - 
				    ap->xmin) / 
			           ap->x_ambit;
			  apply_x_axis_change(ap, cp, sp);
			  resize_sx(cp);
			  set_zx_scrollbar_value(cp, sqrt(ap->zx));
			}
		    }
		  else
		    {
		      cp->cursor_on = TRUE;
		      cursor_moveto(cp, 
				    snd_round_off_t(ungrf_x(cp->axis, x) * 
						    (double)SND_SRATE(sp)));
		      if (mouse_mark)
			{
			  XEN res = XEN_FALSE;
			  if (XEN_HOOKED(mark_click_hook))
			    res = run_progn_hook(mark_click_hook,
						 XEN_LIST_1(C_TO_SMALL_XEN_INT(mark_id(mouse_mark))),
						 S_mark_click_hook);
			  if (!(XEN_TRUE_P(res)))
			    report_in_minibuffer(sp, _("mark %d at sample " PRId64), 
						 mark_id(mouse_mark), 
						 mouse_mark->samp);
			}
		      else
			{
			  if (mix_tag != NO_MIX_TAG)
			    report_in_minibuffer(sp, _("mix %d "), mix_tag);
			}
		    }
		}
	    }
	  else
	    {
	      if (actax == FFT_MAIN)
		{
		  str = describe_fft_point(cp, x, y);
		  report_in_minibuffer(sp, str);
		  if (str) FREE(str);
		}
	      else
		if ((actax == LISP) && 
		    (XEN_HOOKED(mouse_release_hook)))
		  run_hook(mouse_release_hook,
			   XEN_LIST_6(C_TO_SMALL_XEN_INT(sp->index),
				      C_TO_SMALL_XEN_INT(cp->chan),
				      C_TO_XEN_INT(button),
				      C_TO_XEN_INT(key_state),
				      C_TO_XEN_DOUBLE(ungrf_x((cp->lisp_info)->axis, x)),
				      C_TO_XEN_DOUBLE(ungrf_y((cp->lisp_info)->axis, y))),
			   S_mouse_release_hook);
	    }
	}
    }
  else
    {
      /* lisp graph dragged? */
      if (mouse_mark)
	{
	  finish_moving_mark(cp, mouse_mark);
	  mouse_mark = NULL;
	  dragged = FALSE;
	}
      else
	{
	  if (play_mark)
	    {
	      finish_moving_play_mark(cp);
	      stop_playing_sound(sp);
	      play_mark = NULL;
	      dragged = FALSE;
	    }
	  else
	    {
	      if (mix_tag != NO_MIX_TAG)
		{
		  finish_moving_mix_tag(mix_tag, x);
		  dragged = FALSE;
		  mix_tag = NO_MIX_TAG;
		}
	      else
		{
		  if (click_within_graph == WAVE)
		    {
		      cancel_selection_watch();
		      finish_selection_creation();
		      dragged = FALSE;
		      if (show_selection_transform(ss)) 
			{
			  if (sp->sync)
			    for_each_chan_1(ss, calculate_syncd_fft, (void *)(&(sp->sync)));
			  else calculate_fft(cp);
			}
		    }
		}
	    }
	}
    }
}

static Tempus first_time = 0;
static off_t mouse_cursor = 0;

void graph_button_motion_callback(chan_info *cp, int x, int y, Tempus time, Tempus click_time)
{
  snd_info *sp;
  snd_state *ss;
  Tempus mouse_time, time_interval;
  off_t samps;
  char *str;
  Float old_cutoff;
  /* this needs to be a little slow about deciding that we are dragging, as opposed to a slow click */
  mouse_time = time;
#ifdef MAC_OSX
  /* on the Mac, we seem to get motion events even without any motion, and the times seem very short */
  if ((mouse_time - mouse_down_time) < click_time) return;
  if ((x == press_x) && (y == press_y)) return;
#else
  if ((mouse_time - mouse_down_time) < (click_time / 2)) return;
#endif
  sp = cp->sound;
  if (sp->channel_style == CHANNELS_COMBINED) /* in united chans, dragging mark shouldn't change channel */
    {
      if (dragged_cp)
	cp = dragged_cp;
      else cp = which_channel(sp, y);
    }
  select_channel(sp, cp->chan);
  if (mouse_mark)
    {
      move_mark(cp, mouse_mark, x);
      dragged = TRUE;
    }
  else
    {
      if (play_mark)
	{
	  if (!dragged)
	    {
	      first_time = mouse_time;
	      dragged = TRUE;
	      sp->speed_control = 0.0;
	      mouse_cursor = CURSOR(cp);
	      play_channel(cp, play_mark->samp, NO_END_SPECIFIED, TRUE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "drag playing mark", 0);
	    }
	  else
	    {
	      time_interval = mouse_time - first_time;
	      first_time = mouse_time;
	      samps = move_play_mark(cp, &mouse_cursor, (Locus)x);
	      if (time_interval != 0)
		sp->speed_control = (Float)((double)(samps * 1000) / (double)(time_interval * SND_SRATE(sp)));
	      else sp->speed_control = 0.0;
	    }
	}
      else
	{
	  if (click_within_graph == WAVE)
	    {
	      if (mix_tag != NO_MIX_TAG)
		{
		  move_mix_tag(mix_tag, x);
		  dragged = TRUE;
		  return;
		}
	      if (!dragged) 
		start_selection_creation(cp, snd_round_off_t(ungrf_x(cp->axis, x) * SND_SRATE(sp)));
	      else 
		{
		  update_possible_selection_in_progress(snd_round_off_t(ungrf_x(cp->axis, x) * SND_SRATE(sp)));
		  move_selection(cp, x);
		}
	      dragged = TRUE;
	    }
	  else
	    {
	      ss = cp->state;
	      if (click_within_graph == FFT_AXIS)
		{
		  /* change spectro_cutoff(ss) and redisplay fft */
		  old_cutoff = cp->spectro_cutoff;
		  if (cp->transform_graph_type != GRAPH_AS_SONOGRAM)
		    {
#if HAVE_GL
		      if ((with_gl(ss)) && (cp->transform_graph_type == GRAPH_AS_SPECTROGRAM))
			{
			  Float ny;
			  ny = unproject2y(x, y);
			  set_spectro_cutoff(ss, cp->spectro_cutoff + (fft_faxis_start - ny));
			  fft_faxis_start = ny;
			}
		      else
#endif 
		      set_spectro_cutoff(ss, cp->spectro_cutoff + ((Float)(fft_axis_start - x) / fft_axis_extent(cp)));
		      fft_axis_start = x;
		    }
		  else 
		    {
		      set_spectro_cutoff(ss, cp->spectro_cutoff + ((Float)(y - fft_axis_start) / fft_axis_extent(cp)));
		      fft_axis_start = y;
		    }
		  if (spectro_cutoff(ss) > 1.0) set_spectro_cutoff(ss, 1.0);
		  if (spectro_cutoff(ss) < 0.001) set_spectro_cutoff(ss, 0.001);
		  if (old_cutoff != spectro_cutoff(ss)) 
		    {
		      reflect_spectro(ss);
		      if (cp->transform_graph_type != GRAPH_ONCE)
			for_each_chan(ss, sono_update);
		      else for_each_chan(ss, update_graph);
		    }
		}
	      else
		{
		  if (click_within_graph == LISP)
		    {
		      if (XEN_HOOKED(mouse_drag_hook))
			  run_hook(mouse_drag_hook,
				   XEN_LIST_6(C_TO_SMALL_XEN_INT(cp->sound->index),
					      C_TO_SMALL_XEN_INT(cp->chan),
					      C_TO_XEN_INT(-1),
					      C_TO_XEN_INT(-1),
					      C_TO_XEN_DOUBLE(ungrf_x((cp->lisp_info)->axis, x)),
					      C_TO_XEN_DOUBLE(ungrf_y((cp->lisp_info)->axis, y))),
				   S_mouse_drag_hook);
		      return;
		    }
		  if ((cp->verbose_cursor) && (within_graph(cp, x, y) == FFT_MAIN))
		    {
		      str = describe_fft_point(cp, x, y);
		      report_in_minibuffer(cp->sound, str);
		      if (str) FREE(str);
		    }
		}
	    }
	}
    }
}


axis_context *set_context (chan_info *cp, int gc)
{
  axis_context *ax;
  state_context *sx;
  chan_context *cx;
  snd_state *ss;
  cx = cp->tcgx;
  ss = cp->state;
  if (!cx) cx = cp->cgx;
  ax = cx->ax;
  sx = ss->sgx;
  if ((cp->cgx)->selected)
    {
      switch (gc)
	{
	case CHAN_GC: ax->gc = sx->selected_basic_gc;        break;
	case CHAN_IGC: ax->gc = sx->selected_erase_gc;       break;
	case CHAN_SELGC: ax->gc = sx->selected_selection_gc; break;
	case CHAN_CGC: ax->gc = sx->selected_cursor_gc;      break;
	case CHAN_MGC: ax->gc = sx->selected_mark_gc;        break;
	case CHAN_MXGC: ax->gc = sx->mix_gc;                 break;
	case CHAN_SELMXGC: ax->gc = sx->selected_mix_gc;     break;
	case CHAN_TMPGC: ax->gc = sx->selected_basic_gc;     break;
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
	case CHAN_SELMXGC: ax->gc = sx->selected_mix_gc; break;
	case CHAN_TMPGC: 
	  ax->gc = sx->combined_basic_gc;
	  switch (cp->chan % 4)
	    {
	    case 0: set_foreground_color(cp, ax, sx->black);      break;
	    case 1: set_foreground_color(cp, ax, sx->red);        break;
	    case 2: set_foreground_color(cp, ax, sx->green);      break;
	    case 3: set_foreground_color(cp, ax, sx->light_blue); break;
	    }
	  break;
	}
    }
  return(ax);
}

axis_context *copy_context (chan_info *cp)                  {return(set_context(cp, CHAN_GC));}
axis_context *erase_context (chan_info *cp)                 {return(set_context(cp, CHAN_IGC));}
axis_context *selection_context (chan_info *cp)             {return(set_context(cp, CHAN_SELGC));}
static axis_context *cursor_context (chan_info *cp)         {return(set_context(cp, CHAN_CGC));}
axis_context *mark_context (chan_info *cp)                  {return(set_context(cp, CHAN_MGC));}
axis_context *mix_waveform_context (chan_info *cp)          {return(set_context(cp, CHAN_MXGC));}
axis_context *selected_mix_waveform_context (chan_info *cp) {return(set_context(cp, CHAN_SELMXGC));}
static axis_context *combined_context (chan_info *cp)       {return(set_context(cp, CHAN_TMPGC));}

enum {CP_GRAPH_TRANSFORM_P, CP_GRAPH_TIME_P, CP_FRAMES, CP_CURSOR, CP_GRAPH_LISP_P, CP_AP_LOSAMP, CP_AP_HISAMP, CP_SQUELCH_UPDATE,
      CP_EDIT_CTR, CP_CURSOR_STYLE, CP_EDIT_HOOK, CP_UNDO_HOOK, CP_AFTER_EDIT_HOOK,
      CP_SHOW_Y_ZERO, CP_SHOW_MARKS, CP_TIME_GRAPH_TYPE, CP_WAVO_HOP, CP_WAVO_TRACE, CP_MAX_TRANSFORM_PEAKS, 
      CP_SHOW_TRANSFORM_PEAKS, CP_ZERO_PAD, CP_VERBOSE_CURSOR, CP_FFT_LOG_FREQUENCY, CP_FFT_LOG_MAGNITUDE,
      CP_WAVELET_TYPE, CP_SPECTRO_HOP, CP_TRANSFORM_SIZE, CP_TRANSFORM_GRAPH_TYPE, CP_FFT_WINDOW, CP_TRANSFORM_TYPE,
      CP_TRANSFORM_NORMALIZATION, CP_SHOW_MIX_WAVEFORMS, CP_TIME_GRAPH_STYLE, CP_LISP_GRAPH_STYLE, CP_TRANSFORM_GRAPH_STYLE, CP_DOT_SIZE,
      CP_SHOW_AXES, CP_GRAPHS_HORIZONTAL, CP_CURSOR_SIZE, CP_CURSOR_POSITION,
      CP_EDPOS_FRAMES, CP_X_AXIS_STYLE, CP_UPDATE_TIME, CP_UPDATE_TRANSFORM_GRAPH, CP_UPDATE_LISP, CP_PROPERTIES,
      CP_MIN_DB, CP_SPECTRO_X_ANGLE, CP_SPECTRO_Y_ANGLE, CP_SPECTRO_Z_ANGLE, CP_SPECTRO_X_SCALE, CP_SPECTRO_Y_SCALE, CP_SPECTRO_Z_SCALE,
      CP_SPECTRO_CUTOFF, CP_SPECTRO_START, CP_FFT_WINDOW_BETA, CP_AP_SX, CP_AP_SY, CP_AP_ZX, CP_AP_ZY, CP_MAXAMP, CP_EDPOS_MAXAMP,
      CP_BEATS_PER_MINUTE, CP_EDPOS_CURSOR
};

#define EDPOS_NOT_PROTECTED -1
static XEN cp_edpos;
static int cp_edpos_loc = EDPOS_NOT_PROTECTED;

static XEN channel_get(XEN snd_n, XEN chn_n, int fld, char *caller)
{
  chan_info *cp;
  snd_info *sp = NULL;
  snd_state *ss;
  int i;
  XEN res = XEN_EMPTY_LIST;
  if (XEN_TRUE_P(snd_n))
    {
      ss = get_global_state();
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = XEN_CONS(channel_get(C_TO_SMALL_XEN_INT(i), chn_n, fld, caller), res);
	}
      return(res);
    }
  else
    {
      if (XEN_TRUE_P(chn_n))
	{
	  sp = get_sp(snd_n, NO_PLAYERS);
	  if (sp == NULL)
	    return(snd_no_such_sound_error(caller, snd_n));
	  for (i = sp->nchans - 1; i >= 0; i--)
	    res = XEN_CONS(channel_get(snd_n, C_TO_SMALL_XEN_INT(i), fld, caller), res);
	  return(res);
	}
      else
	{
	  ASSERT_CHANNEL(caller, snd_n, chn_n, 1);
	  cp = get_cp(snd_n, chn_n, caller);
	  switch(fld)
	    {
	    case CP_EDIT_CTR:                return(C_TO_XEN_INT(cp->edit_ctr));                               break;
	    case CP_GRAPH_TRANSFORM_P:       return(C_TO_XEN_BOOLEAN(cp->graph_transform_p));                  break;
	    case CP_GRAPH_TIME_P:            return(C_TO_XEN_BOOLEAN(cp->graph_time_p));                       break;
	    case CP_CURSOR:                  return(C_TO_XEN_OFF_T(CURSOR(cp)));                               break;
	    case CP_EDPOS_CURSOR:            return(C_TO_XEN_OFF_T(cp->cursors[to_c_edit_position(cp, cp_edpos, S_cursor, 3)])); break;
	    case CP_FRAMES:                  return(C_TO_XEN_OFF_T(CURRENT_SAMPLES(cp)));                      break;
	    case CP_GRAPH_LISP_P:            return(C_TO_XEN_BOOLEAN(cp->graph_lisp_p));                       break;
	    case CP_AP_LOSAMP:               if (cp->axis) return(C_TO_XEN_OFF_T((cp->axis)->losamp));         break;
	    case CP_AP_HISAMP:               if (cp->axis) return(C_TO_XEN_OFF_T((cp->axis)->hisamp));         break;
	    case CP_SQUELCH_UPDATE:          return(C_TO_XEN_BOOLEAN(cp->squelch_update));                     break;
	    case CP_CURSOR_SIZE:             return(C_TO_XEN_INT(cp->cursor_size));                            break;
	    case CP_CURSOR_STYLE:            return(C_TO_XEN_INT(cp->cursor_style));                           break;
	    case CP_EDIT_HOOK:               return(cp->edit_hook);                                            break;
	    case CP_AFTER_EDIT_HOOK:         return(cp->after_edit_hook);                                      break;
	    case CP_UNDO_HOOK:               return(cp->undo_hook);                                            break;
	    case CP_SHOW_Y_ZERO:             return(C_TO_XEN_BOOLEAN(cp->show_y_zero));                        break;
	    case CP_SHOW_MARKS:              return(C_TO_XEN_BOOLEAN(cp->show_marks));                         break;
	    case CP_TIME_GRAPH_TYPE:         return(C_TO_XEN_INT(cp->time_graph_type));                        break;
	    case CP_WAVO_HOP:                return(C_TO_XEN_INT(cp->wavo_hop));                               break;
	    case CP_WAVO_TRACE:              return(C_TO_XEN_INT(cp->wavo_trace));                             break;
	    case CP_MAX_TRANSFORM_PEAKS:     return(C_TO_XEN_INT(cp->max_transform_peaks));                    break;
	    case CP_ZERO_PAD:                return(C_TO_XEN_INT(cp->zero_pad));                               break;
	    case CP_WAVELET_TYPE:            return(C_TO_XEN_INT(cp->wavelet_type));                           break;
	    case CP_SHOW_TRANSFORM_PEAKS:    return(C_TO_XEN_BOOLEAN(cp->show_transform_peaks));               break;
	    case CP_VERBOSE_CURSOR:          return(C_TO_XEN_BOOLEAN(cp->verbose_cursor));                     break;
	    case CP_FFT_LOG_FREQUENCY:       return(C_TO_XEN_BOOLEAN(cp->fft_log_frequency));                  break;
	    case CP_FFT_LOG_MAGNITUDE:       return(C_TO_XEN_BOOLEAN(cp->fft_log_magnitude));                  break;
	    case CP_SPECTRO_HOP:             return(C_TO_XEN_INT(cp->spectro_hop));                            break;
	    case CP_TRANSFORM_SIZE:          return(C_TO_XEN_INT(cp->transform_size));                         break;
	    case CP_TRANSFORM_GRAPH_TYPE:    return(C_TO_XEN_INT(cp->transform_graph_type));                   break;
	    case CP_FFT_WINDOW:              return(C_TO_XEN_INT(cp->fft_window));                             break;
	    case CP_TRANSFORM_TYPE:          return(C_TO_XEN_INT(cp->transform_type));                         break;
	    case CP_TRANSFORM_NORMALIZATION: return(C_TO_XEN_INT(cp->transform_normalization));                break;
	    case CP_SHOW_MIX_WAVEFORMS:      return(C_TO_XEN_BOOLEAN(cp->show_mix_waveforms));                 break;
	    case CP_TIME_GRAPH_STYLE:        return(C_TO_XEN_INT(cp->time_graph_style));                       break;
	    case CP_LISP_GRAPH_STYLE:        return(C_TO_XEN_INT(cp->lisp_graph_style));                       break;
	    case CP_TRANSFORM_GRAPH_STYLE:   return(C_TO_XEN_INT(cp->transform_graph_style));                  break;
	    case CP_X_AXIS_STYLE:            return(C_TO_XEN_INT(cp->x_axis_style));                           break;
	    case CP_DOT_SIZE:                return(C_TO_XEN_INT(cp->dot_size));                               break;
	    case CP_SHOW_AXES:               return(C_TO_XEN_INT(cp->show_axes));                              break;
	    case CP_GRAPHS_HORIZONTAL:       return(C_TO_XEN_BOOLEAN(cp->graphs_horizontal));                  break;
	    case CP_CURSOR_POSITION:         return(XEN_LIST_2(C_TO_XEN_INT(cp->cx), C_TO_XEN_INT(cp->cy)));   break;
	    case CP_EDPOS_FRAMES:            return(C_TO_XEN_OFF_T(to_c_edit_samples(cp, cp_edpos, caller, 3))); break;
	    case CP_UPDATE_TIME:             display_channel_time_data(cp, cp->sound, cp->state);              break;
	    case CP_UPDATE_LISP:             display_channel_lisp_data(cp, cp->sound, cp->state);              break;
	    case CP_UPDATE_TRANSFORM_GRAPH: 
	      if (cp->graph_transform_p)
		{
		  void *val;
		  if (chan_fft_in_progress(cp)) 
		    force_fft_clear(cp);
		  
		  (cp->state)->checking_explicitly = TRUE;  /* do not allow UI events to intervene here! */
		  if (cp->transform_graph_type == GRAPH_ONCE)
		    single_fft(cp, 0);
		  else
		    {
		      val = (void *)make_sonogram_state(cp);
		      while (sonogram_in_slices(val) == BACKGROUND_CONTINUE);
		    }
		  (cp->state)->checking_explicitly = FALSE;
		}
	      break;
	    case CP_PROPERTIES:
	      if (!(XEN_VECTOR_P(cp->properties)))
		{
		  cp->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
		  snd_protect(cp->properties);
		}
	      return(XEN_VECTOR_REF(cp->properties, 0));
	      break;

	    case CP_AP_SX:            if (cp->axis) return(C_TO_XEN_DOUBLE((cp->axis)->sx));     break;
	    case CP_AP_SY:            if (cp->axis) return(C_TO_XEN_DOUBLE((cp->axis)->sy));     break;
	    case CP_AP_ZX:            if (cp->axis) return(C_TO_XEN_DOUBLE((cp->axis)->zx));     break;
	    case CP_AP_ZY:            if (cp->axis) return(C_TO_XEN_DOUBLE((cp->axis)->zy));     break;
	    case CP_MIN_DB:           return(C_TO_XEN_DOUBLE(cp->min_dB));                       break;
	    case CP_SPECTRO_X_ANGLE:  return(C_TO_XEN_DOUBLE(cp->spectro_x_angle));              break;
	    case CP_SPECTRO_Y_ANGLE:  return(C_TO_XEN_DOUBLE(cp->spectro_y_angle));              break;
	    case CP_SPECTRO_Z_ANGLE:  return(C_TO_XEN_DOUBLE(cp->spectro_z_angle));              break;
	    case CP_SPECTRO_X_SCALE:  return(C_TO_XEN_DOUBLE(cp->spectro_x_scale));              break;
	    case CP_SPECTRO_Y_SCALE:  return(C_TO_XEN_DOUBLE(cp->spectro_y_scale));              break;
	    case CP_SPECTRO_Z_SCALE:  return(C_TO_XEN_DOUBLE(cp->spectro_z_scale));              break;
	    case CP_SPECTRO_CUTOFF:   return(C_TO_XEN_DOUBLE(cp->spectro_cutoff));               break;
	    case CP_SPECTRO_START:    return(C_TO_XEN_DOUBLE(cp->spectro_start));                break;
	    case CP_FFT_WINDOW_BETA:  return(C_TO_XEN_DOUBLE(cp->fft_window_beta));              break;
	    case CP_MAXAMP:           return(C_TO_XEN_DOUBLE(get_maxamp(cp->sound, cp, AT_CURRENT_EDIT_POSITION))); break;
	    case CP_EDPOS_MAXAMP:     return(C_TO_XEN_DOUBLE(get_maxamp(cp->sound, cp, to_c_edit_position(cp, cp_edpos, S_maxamp, 3)))); break;
	    case CP_BEATS_PER_MINUTE: return(C_TO_XEN_DOUBLE(cp->beats_per_minute));             break;

	    }
	}
    }
  return(XEN_FALSE);
}

static int g_mus_iclamp(int mn, XEN val, int def, int mx)
{
  return(mus_iclamp(mn, XEN_TO_C_INT_OR_ELSE(val, def), mx));
}

static int g_imin(int mn, XEN val, int def)
{
  int nval;
  nval = XEN_TO_C_INT_OR_ELSE(val, def);
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
  resize_zy(cp);
  apply_y_axis_change(ap, cp);
}

static int call_update_graph = TRUE;

static XEN channel_set(XEN snd_n, XEN chn_n, XEN on, int fld, char *caller)
{
  chan_info *cp;
  int val = 0;
  snd_info *sp;
  snd_state *ss;
  int i;
  off_t curlen = 0, newlen = 0;
  char *error = NULL;
  Float curamp;
  Float newamp[1];
  XEN res = XEN_EMPTY_LIST; XEN errstr;
  if (XEN_TRUE_P(snd_n))
    {
      ss = get_global_state();
      for (i = ss->max_sounds - 1; i >= 0; i--)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = XEN_CONS(channel_set(C_TO_SMALL_XEN_INT(i), chn_n, on, fld, caller), res);
	}
      return(res);
    }
  if (XEN_TRUE_P(chn_n))
    {
      sp = get_sp(snd_n, NO_PLAYERS);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      for (i = sp->nchans - 1; i >= 0; i--)
	res = XEN_CONS(channel_set(snd_n, C_TO_SMALL_XEN_INT(i), on, fld, caller), res);
      return(res);
    }
  ASSERT_CHANNEL(caller, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, caller);
  switch (fld)
    {
    case CP_EDIT_CTR:
      val = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(on, 0, caller);
      if (cp->edit_ctr < val)
	redo_edit(cp, val - cp->edit_ctr);
      else undo_edit(cp, cp->edit_ctr - val);
      return(C_TO_XEN_INT(cp->edit_ctr));
      break;
    case CP_GRAPH_TRANSFORM_P:
      fftb(cp, val = XEN_TO_C_BOOLEAN_OR_TRUE(on)); 
      update_graph(cp);
      break;
    case CP_GRAPH_TIME_P:
      waveb(cp, val = XEN_TO_C_BOOLEAN_OR_TRUE(on)); 
      update_graph(cp);
      break;
    case CP_CURSOR:
      cp->cursor_on = TRUE; 
      cursor_moveto(cp, XEN_TO_C_OFF_T_OR_ELSE(on, 0));
      return(C_TO_XEN_OFF_T(CURSOR(cp)));
      break;
    case CP_EDPOS_CURSOR:
      {
	int pos;
	off_t cpos;
	pos = to_c_edit_position(cp, cp_edpos, caller, 3);
	cpos = XEN_TO_C_OFF_T_OR_ELSE(on, 0);
	if (pos == cp->edit_ctr)
	  {
	    cp->cursor_on = TRUE; 
	    cursor_moveto(cp, cpos);
	  }
	else cp->cursors[pos] = cpos;
	return(C_TO_XEN_OFF_T(cpos));
      }
      break;
    case CP_GRAPH_LISP_P:
      cp->graph_lisp_p = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      val = cp->graph_lisp_p; 
      update_graph(cp);
      break;
    case CP_AP_LOSAMP:
      set_x_axis_x0(cp, XEN_TO_C_OFF_T_OR_ELSE(on, 0)); 
      return(on);
      break;
    case CP_AP_HISAMP:
      set_x_axis_x1(cp, XEN_TO_C_OFF_T_OR_ELSE(on, 1)); 
      return(on);
      break;
    case CP_SQUELCH_UPDATE:
      cp->squelch_update = XEN_TO_C_BOOLEAN_OR_TRUE(on);
      if (!(cp->squelch_update))
	{
	  /* make sure everything that was squelched before is now correct */
	  reflect_edit_history_change(cp);
	  if (cp->edit_ctr == 0)
	    reflect_file_revert_in_label(cp->sound);
	  else reflect_file_change_in_label(cp);
	}
      break;
    case CP_CURSOR_SIZE:
      cp->cursor_size = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(on, DEFAULT_CURSOR_SIZE, caller);
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
		snd_unprotect(cp->cursor_proc);
	      snd_protect(on);
	      cp->cursor_proc = on;
	      cp->cursor_style = CURSOR_PROC;
	    }
	  else 
	    {
	      errstr = C_TO_XEN_STRING(error);
	      FREE(error);
	      return(snd_bad_arity_error(S_setB S_cursor_style, errstr, on));
	    }
	}
      else
	{
	  if ((cp->cursor_style == CURSOR_PROC) &&
	      (XEN_PROCEDURE_P(cp->cursor_proc)))
	    {
	      snd_unprotect(cp->cursor_proc);
	      cp->cursor_proc = XEN_UNDEFINED;
	    }
	  cp->cursor_style = g_mus_iclamp(CURSOR_CROSS, on, CURSOR_CROSS, CURSOR_LINE);
	}
      cp->just_zero = (cp->cursor_style == CURSOR_LINE); /* no point in displaying y value in this case */
      update_graph(cp); 
      return(C_TO_XEN_INT(cp->cursor_style));
      break;
    case CP_SHOW_Y_ZERO:
      cp->show_y_zero = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_y_zero));
      break;
    case CP_SHOW_MARKS:
      cp->show_marks = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_marks));
      break;
    case CP_TIME_GRAPH_TYPE:
      cp->time_graph_type = XEN_TO_C_INT_OR_ELSE(on, GRAPH_ONCE);
      update_graph(cp); 
      return(C_TO_XEN_INT(cp->time_graph_type));
      break;
    case CP_WAVO_HOP:
      cp->wavo_hop = g_imin(1, on, DEFAULT_WAVO_HOP); 
      update_graph(cp); 
      return(C_TO_XEN_INT(cp->wavo_hop));
      break;
    case CP_WAVO_TRACE:
      cp->wavo_trace = g_imin(1, on, DEFAULT_WAVO_TRACE); 
      update_graph(cp); 
      return(C_TO_XEN_INT(cp->wavo_trace));
      break;
    case CP_MAX_TRANSFORM_PEAKS:
      cp->max_transform_peaks = g_imin(1, on, DEFAULT_MAX_TRANSFORM_PEAKS); 
      return(C_TO_XEN_INT(cp->max_transform_peaks));
      break;
    case CP_ZERO_PAD:
      cp->zero_pad = g_imin(0, on, DEFAULT_ZERO_PAD); 
      update_graph(cp);
      return(C_TO_XEN_INT(cp->zero_pad));
      break;
    case CP_WAVELET_TYPE:
      cp->wavelet_type = g_mus_iclamp(0, on, DEFAULT_WAVELET_TYPE, NUM_WAVELETS - 1); 
      update_graph(cp);
      return(C_TO_XEN_INT(cp->wavelet_type));
      break;
    case CP_SHOW_TRANSFORM_PEAKS:
      cp->show_transform_peaks = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_transform_peaks));
      break;
    case CP_VERBOSE_CURSOR:
      cp->verbose_cursor = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      return(C_TO_XEN_BOOLEAN(cp->verbose_cursor));
      break;
    case CP_FFT_LOG_FREQUENCY:
      cp->fft_log_frequency = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      if (cp->graph_transform_p) calculate_fft(cp); 
      return(C_TO_XEN_BOOLEAN(cp->fft_log_frequency));
      break;
    case CP_FFT_LOG_MAGNITUDE:
      cp->fft_log_magnitude = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      if (cp->graph_transform_p) calculate_fft(cp); 
      return(C_TO_XEN_BOOLEAN(cp->fft_log_magnitude));
      break;
    case CP_SPECTRO_HOP:
      cp->spectro_hop = g_imin(1, on, DEFAULT_SPECTRO_HOP); 
      if (cp->graph_transform_p) calculate_fft(cp); 
      return(C_TO_XEN_INT(cp->spectro_hop));
      break;
    case CP_TRANSFORM_SIZE:
      cp->transform_size = g_imin(1, on, DEFAULT_TRANSFORM_SIZE); 
      calculate_fft(cp);
      return(C_TO_XEN_INT(cp->transform_size));
      break;
    case CP_TRANSFORM_GRAPH_TYPE: 
      cp->transform_graph_type = g_mus_iclamp(0, on, DEFAULT_TRANSFORM_GRAPH_TYPE, GRAPH_AS_SPECTROGRAM);
      calculate_fft(cp); 
      return(C_TO_XEN_INT(cp->transform_graph_type)); 
      break;
    case CP_FFT_WINDOW:
      cp->fft_window = g_mus_iclamp(0, on, DEFAULT_FFT_WINDOW, NUM_FFT_WINDOWS - 1); 
      calculate_fft(cp); 
      return(C_TO_XEN_INT(cp->fft_window));
      break;
    case CP_TRANSFORM_TYPE:
      cp->transform_type = g_imin(0, on, DEFAULT_TRANSFORM_TYPE); 
      calculate_fft(cp); 
      return(C_TO_XEN_INT(cp->transform_type));
      break;
    case CP_TRANSFORM_NORMALIZATION:      
      if (XEN_INTEGER_P(on))
	cp->transform_normalization = mus_iclamp(DONT_NORMALIZE, XEN_TO_SMALL_C_INT(on), NORMALIZE_GLOBALLY);
      else
	if (XEN_FALSE_P(on))
	  cp->transform_normalization = DONT_NORMALIZE;
	else 
	  if (XEN_TRUE_P(on))
	    cp->transform_normalization = NORMALIZE_BY_CHANNEL;
	  else cp->transform_normalization = DEFAULT_TRANSFORM_NORMALIZATION;
      calculate_fft(cp); 
      return(C_TO_XEN_INT(cp->transform_normalization));
      break;
    case CP_SHOW_MIX_WAVEFORMS: 
      cp->show_mix_waveforms = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->show_mix_waveforms));
      break;
    case CP_TIME_GRAPH_STYLE:
      val = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(on, DEFAULT_GRAPH_STYLE, caller);
      if (!(GRAPH_STYLE_OK(val)))
	XEN_OUT_OF_RANGE_ERROR(caller, 1, on, "unknown " S_time_graph_style);
      cp->time_graph_style = val;
      if (call_update_graph) update_graph(cp);
      return(C_TO_XEN_INT(cp->time_graph_style));
      break;
    case CP_LISP_GRAPH_STYLE:
      val = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(on, DEFAULT_GRAPH_STYLE, caller);
      if (!(GRAPH_STYLE_OK(val)))
	XEN_OUT_OF_RANGE_ERROR(caller, 1, on, "unknown " S_lisp_graph_style);
      cp->lisp_graph_style = val;
      if (call_update_graph) update_graph(cp);
      return(C_TO_XEN_INT(cp->lisp_graph_style));
      break;
    case CP_TRANSFORM_GRAPH_STYLE:
      val = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(on, DEFAULT_GRAPH_STYLE, caller);
      if (!(GRAPH_STYLE_OK(val)))
	XEN_OUT_OF_RANGE_ERROR(caller, 1, on, "unknown " S_transform_graph_style);
      cp->transform_graph_style = val;
      if (call_update_graph) update_graph(cp);
      return(C_TO_XEN_INT(cp->transform_graph_style));
      break;
    case CP_X_AXIS_STYLE:
      val = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(on, DEFAULT_X_AXIS_STYLE, caller);
      if ((val < X_AXIS_IN_SECONDS) ||
	  (val > X_AXIS_IN_BEATS))
	XEN_OUT_OF_RANGE_ERROR(caller, 1, on, "unknown x axis style");
      chans_x_axis_style(cp, (void *)(&val));
      return(C_TO_XEN_INT(cp->x_axis_style));
      break;
    case CP_DOT_SIZE:
      cp->dot_size = g_imin(0, on, DEFAULT_DOT_SIZE); 
      update_graph(cp);
      return(C_TO_XEN_INT(cp->dot_size));
      break;
    case CP_SHOW_AXES:
      val = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(on, DEFAULT_SHOW_AXES, caller);
      if ((val < SHOW_NO_AXES) ||
	  (val > SHOW_X_AXIS))
	XEN_OUT_OF_RANGE_ERROR(caller, 1, on, "unknown show-axes value");
      cp->show_axes = val;
      update_graph(cp); 
      return(C_TO_XEN_INT(cp->show_axes));
      break;
    case CP_GRAPHS_HORIZONTAL:
      cp->graphs_horizontal = XEN_TO_C_BOOLEAN_OR_TRUE(on); 
      update_graph(cp); 
      return(C_TO_XEN_BOOLEAN(cp->graphs_horizontal));
      break;
    case CP_FRAMES:
      /* if less than current, delete, else zero pad */
      curlen = CURRENT_SAMPLES(cp);
      newlen = XEN_TO_C_OFF_T_OR_ELSE(on, curlen);
      if (newlen < 0)
	XEN_OUT_OF_RANGE_ERROR(S_setB S_frames, 1, on, "frames < 0?");
      if (curlen > newlen)
	{
	  if (newlen > 0)
	    delete_samples(newlen - 1, curlen - newlen, cp, "(set-frames)", cp->edit_ctr);
	  else delete_samples(0, curlen, cp, "(set-frames)", cp->edit_ctr);
	}
      else
	{
	  if (newlen > curlen)
	    extend_with_zeros(cp, curlen, newlen - curlen, "(set-frames)", cp->edit_ctr);
	}
      update_graph(cp);
      break;
    case CP_PROPERTIES:
      if (!(XEN_VECTOR_P(cp->properties)))
	{
	  cp->properties = XEN_MAKE_VECTOR(1, XEN_EMPTY_LIST);
	  snd_protect(cp->properties);
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
      cp->min_dB = XEN_TO_C_DOUBLE(on); 
      cp->lin_dB = pow(10.0, cp->min_dB * 0.05); 
      calculate_fft(cp); 
      break;
    case CP_SPECTRO_X_ANGLE: cp->spectro_x_angle = XEN_TO_C_DOUBLE(on); calculate_fft(cp); break;
    case CP_SPECTRO_Y_ANGLE: cp->spectro_y_angle = XEN_TO_C_DOUBLE(on); calculate_fft(cp); break;
    case CP_SPECTRO_Z_ANGLE: cp->spectro_z_angle = XEN_TO_C_DOUBLE(on); calculate_fft(cp); break;
    case CP_SPECTRO_X_SCALE: cp->spectro_x_scale = XEN_TO_C_DOUBLE(on); calculate_fft(cp); break;
    case CP_SPECTRO_Y_SCALE: cp->spectro_y_scale = XEN_TO_C_DOUBLE(on); calculate_fft(cp); break;
    case CP_SPECTRO_Z_SCALE: cp->spectro_z_scale = XEN_TO_C_DOUBLE(on); calculate_fft(cp); break;
    case CP_SPECTRO_CUTOFF:  
      cp->spectro_cutoff = mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), 1.0); 
      calculate_fft(cp); 
      return(C_TO_XEN_DOUBLE(cp->spectro_cutoff)); 
      break;
    case CP_SPECTRO_START:   
      cp->spectro_start = mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), 1.0); 
      calculate_fft(cp); 
      return(C_TO_XEN_DOUBLE(cp->spectro_start));   
      break;
    case CP_FFT_WINDOW_BETA:        
      cp->fft_window_beta = mus_fclamp(0.0, XEN_TO_C_DOUBLE(on), 1.0); 
      calculate_fft(cp); 
      return(C_TO_XEN_DOUBLE(cp->fft_window_beta));             
      break;
    case CP_MAXAMP:
      curamp = get_maxamp(cp->sound, cp, AT_CURRENT_EDIT_POSITION);
      newamp[0] = XEN_TO_C_DOUBLE(on);
      if (curamp != newamp[0])
	{
	  scale_to(cp->state, cp->sound, cp, newamp, 1, FALSE);
	  update_graph(cp);
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
    }
  return(C_TO_XEN_BOOLEAN(val));
}

static XEN g_update_time_graph(XEN snd, XEN chn) 
{
  #define H_update_time_graph "(" S_update_time_graph " (snd #f) (chn #f)): redraw snd channel chn's graphs"
  return(channel_get(snd, chn, CP_UPDATE_TIME, S_update_time_graph));
}

static XEN g_update_transform_graph(XEN snd, XEN chn) 
{
  #define H_update_transform_graph "(" S_update_transform_graph " (snd #f) (chn #f)): redraw snd channel chn's fft display"
  return(channel_get(snd, chn, CP_UPDATE_TRANSFORM_GRAPH, S_update_time_graph));
}

static XEN g_update_lisp_graph(XEN snd, XEN chn) 
{
  #define H_update_lisp_graph "(" S_update_lisp_graph " (snd #f) (chn #f)): redraw snd channel chn's lisp graph"
  return(channel_get(snd, chn, CP_UPDATE_LISP, S_update_time_graph));
}



#define WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(name_reversed, name) \
static XEN name_reversed(XEN arg1, XEN arg2, XEN arg3) \
{ \
  if (XEN_NOT_BOUND_P(arg1)) \
    return(name(XEN_TRUE, XEN_UNDEFINED, XEN_UNDEFINED)); \
  else { \
    if (XEN_NOT_BOUND_P(arg2)) \
      return(name(arg1, XEN_UNDEFINED, XEN_UNDEFINED)); \
    else { \
      if (XEN_NOT_BOUND_P(arg3)) \
        return(name(arg2, arg1, XEN_UNDEFINED)); \
      else return(name(arg3, arg1, arg2)); \
}}}

static XEN g_edit_position(XEN snd_n, XEN chn_n) 
{
  #define H_edit_position "(" S_edit_position " (snd #f) (chn #f)): current edit history position in snd's channel chn"
  return(channel_get(snd_n, chn_n, CP_EDIT_CTR, S_edit_position));
}

static XEN g_set_edit_position(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, S_setB S_edit_position, "an integer");
  return(channel_set(snd_n, chn_n, on, CP_EDIT_CTR, S_setB S_edit_position));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_edit_position_reversed, g_set_edit_position)

static XEN g_transform_graph_p(XEN snd_n, XEN chn_n) 
{
  #define H_transform_graph_p "(" S_transform_graph_p " (snd #f) (chn #f)): #t if fft display is active in snd's channel chn"
  return(channel_get(snd_n, chn_n, CP_GRAPH_TRANSFORM_P, S_transform_graph_p));
}

static XEN g_set_transform_graph_p(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_transform_graph_p, "a boolean");
  return(channel_set(snd_n, chn_n, on, CP_GRAPH_TRANSFORM_P, S_setB S_transform_graph_p));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_transform_graph_p_reversed, g_set_transform_graph_p)

static XEN g_time_graph_p(XEN snd_n, XEN chn_n) 
{
  #define H_time_graph_p "(" S_time_graph_p " (snd #f) (chn #f)): #t if time domain display is active in snd's channel chn"
  return(channel_get(snd_n, chn_n, CP_GRAPH_TIME_P, S_time_graph_p));
}

static XEN g_set_time_graph_p(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_time_graph_p, "a boolean");
  return(channel_set(snd_n, chn_n, on, CP_GRAPH_TIME_P, S_setB S_time_graph_p));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_time_graph_p_reversed, g_set_time_graph_p)

static XEN g_lisp_graph_p(XEN snd_n, XEN chn_n) 
{
  #define H_lisp_graph_p "(" S_lisp_graph_p " (snd #f) (chn #f)): #t if lisp-generated data display is active in snd's channel chn"
  return(channel_get(snd_n, chn_n, CP_GRAPH_LISP_P, S_lisp_graph_p));
}

static XEN g_set_lisp_graph_p(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_lisp_graph_p, "a boolean");
  return(channel_set(snd_n, chn_n, on, CP_GRAPH_LISP_P, S_setB S_lisp_graph_p));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_lisp_graph_p_reversed, g_set_lisp_graph_p)

static XEN g_cursor(XEN snd_n, XEN chn_n, XEN edpos) 
{
  #define H_cursor "(" S_cursor " (snd #f) (chn #f) edpos): current cursor location in snd's channel chn"
  if (XEN_BOUND_P(edpos))
    {
      XEN res;
      if (cp_edpos_loc != EDPOS_NOT_PROTECTED)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd_n, chn_n, CP_EDPOS_CURSOR, S_cursor);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = EDPOS_NOT_PROTECTED;
      return(res);
    }
  return(channel_get(snd_n, chn_n, CP_CURSOR, S_cursor));
}

static XEN g_set_cursor(XEN on, XEN snd_n, XEN chn_n, XEN edpos) 
{
  XEN_ASSERT_TYPE(XEN_OFF_T_P(on) || XEN_NOT_BOUND_P(on), on, XEN_ARG_1, S_setB S_cursor, "an integer");
  if (XEN_BOUND_P(edpos))
    {
      XEN res;
      if (cp_edpos_loc != EDPOS_NOT_PROTECTED)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_set(snd_n, chn_n, on, CP_EDPOS_CURSOR, S_setB S_cursor);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = EDPOS_NOT_PROTECTED;
      return(res);
    }
  return(channel_set(snd_n, chn_n, on, CP_CURSOR, S_setB S_cursor));
}

static XEN g_set_cursor_reversed(XEN arg1, XEN arg2, XEN arg3, XEN arg4)
{
  if (XEN_NOT_BOUND_P(arg2))
    return(g_set_cursor(arg1, XEN_UNDEFINED, XEN_UNDEFINED, XEN_UNDEFINED));
  else {
    if (XEN_NOT_BOUND_P(arg3))
      return(g_set_cursor(arg2, arg1, XEN_UNDEFINED, XEN_UNDEFINED));
    else {
      if (XEN_NOT_BOUND_P(arg4))
	return(g_set_cursor(arg3, arg1, arg2, XEN_UNDEFINED));
      else return(g_set_cursor(arg4, arg1, arg2, arg3));
    }}
}

static XEN g_cursor_style(XEN snd_n, XEN chn_n) 
{
  #define H_cursor_style "(" S_cursor_style " (snd #f) (chn #f)): current cursor style in snd's channel chn. \
Possible values are " S_cursor_cross " (default), " S_cursor_line " (a vertical line), or a procedure of three arguments, the \
sound index, channel number, and graph (always " S_time_graph ").  The procedure \
should draw the cursor at the current cursor position using the \
cursor-context whenever it is called."

  return(channel_get(snd_n, chn_n, CP_CURSOR_STYLE, S_cursor_style));
}

static XEN g_set_cursor_style(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on) || XEN_PROCEDURE_P(on), on, XEN_ARG_1, S_setB S_cursor_style, "an integer");
  return(channel_set(snd_n, chn_n, on, CP_CURSOR_STYLE, S_setB S_cursor_style));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_cursor_style_reversed, g_set_cursor_style)

static XEN g_cursor_size(XEN snd_n, XEN chn_n) 
{
  #define H_cursor_size "(" S_cursor_size " (snd #f) (chn #f)): current cursor size in snd's channel chn"
  return(channel_get(snd_n, chn_n, CP_CURSOR_SIZE, S_cursor_size));
}

static XEN g_set_cursor_size(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(on), on, XEN_ARG_1, S_setB S_cursor_size, "an integer");
  return(channel_set(snd_n, chn_n, on, CP_CURSOR_SIZE, S_setB S_cursor_size));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_cursor_size_reversed, g_set_cursor_size)

static XEN g_cursor_position(XEN snd, XEN chn)
{
  #define H_cursor_position "(" S_cursor_position " (snd #f) (chn #f)): current cursor position (x y in pixels) in snd's channel chn"
  return(channel_get(snd, chn, CP_CURSOR_POSITION, S_cursor_position));
}

static XEN g_frames(XEN snd_n, XEN chn_n, XEN edpos)
{
  #define H_frames "(" S_frames " (snd #f) (chn #f) (edpos #f)): number of frames of data in snd's channel chn"
  XEN res;
  if (XEN_BOUND_P(edpos))
    {
      if (cp_edpos_loc != EDPOS_NOT_PROTECTED)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd_n, chn_n, CP_EDPOS_FRAMES, S_frames);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = EDPOS_NOT_PROTECTED;
      return(res);
    }
  return(channel_get(snd_n, chn_n, CP_FRAMES, S_frames));
}

static XEN g_set_frames(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_cursor_size, "a number");
  return(channel_set(snd_n, chn_n, on, CP_FRAMES, S_setB S_frames));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_frames_reversed, g_set_frames)

static XEN g_maxamp(XEN snd_n, XEN chn_n, XEN edpos) 
{
  #define H_maxamp "(" S_maxamp " (snd #f) (chn #f) (edpos #f)): maxamp of data in snd's channel chn"
  XEN res;
  if (XEN_BOUND_P(edpos))
    {
      if (cp_edpos_loc != EDPOS_NOT_PROTECTED)
	snd_unprotect_at(cp_edpos_loc);
      cp_edpos = edpos;
      cp_edpos_loc = snd_protect(cp_edpos);
      res = channel_get(snd_n, chn_n, CP_EDPOS_MAXAMP, S_maxamp);
      snd_unprotect_at(cp_edpos_loc);
      cp_edpos_loc = EDPOS_NOT_PROTECTED;
      return(res);
    }
  return(channel_get(snd_n, chn_n, CP_MAXAMP, S_maxamp));
}

static XEN g_set_maxamp(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_maxamp, "a number");
  return(channel_set(snd_n, chn_n, on, CP_MAXAMP, S_setB S_maxamp));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_maxamp_reversed, g_set_maxamp)

static XEN g_squelch_update(XEN snd_n, XEN chn_n) 
{
  #define H_squelch_update "(" S_squelch_update " (snd #f) (chn #f)): #t if updates (redisplays) are turned off in snd's channel chn"
  return(channel_get(snd_n, chn_n, CP_SQUELCH_UPDATE, S_squelch_update));
}

static XEN g_set_squelch_update(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_squelch_update, "a boolean");
  return(channel_set(snd_n, chn_n, on, CP_SQUELCH_UPDATE, S_setB S_squelch_update));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_squelch_update_reversed, g_set_squelch_update)

static XEN g_ap_sx(XEN snd_n, XEN chn_n) 
{
  #define H_x_position_slider "(" S_x_position_slider " (snd #f) (chn #f)): current x axis position slider of snd channel chn"
  return(channel_get(snd_n, chn_n, CP_AP_SX, S_x_position_slider));
}

static XEN g_set_ap_sx(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_x_position_slider, "a number");
  return(channel_set(snd_n, chn_n, on, CP_AP_SX, S_setB S_x_position_slider));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_ap_sx_reversed, g_set_ap_sx)

static XEN g_ap_sy(XEN snd_n, XEN chn_n) 
{
  #define H_y_position_slider "(" S_y_position_slider " (snd #f) (chn #f)): current y axis position slider of snd channel chn"
  return(channel_get(snd_n, chn_n, CP_AP_SY, S_y_position_slider));
}

static XEN g_set_ap_sy(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_y_position_slider, "a number");
  return(channel_set(snd_n, chn_n, on, CP_AP_SY, S_setB S_y_position_slider));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_ap_sy_reversed, g_set_ap_sy)

static XEN g_ap_zx(XEN snd_n, XEN chn_n) 
{
  #define H_x_zoom_slider "(" S_x_zoom_slider " (snd #f) (chn #f)): current x axis zoom slider of snd channel chn"
  return(channel_get(snd_n, chn_n, CP_AP_ZX, S_x_zoom_slider));
}

static XEN g_set_ap_zx(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_x_zoom_slider, "a number");
  return(channel_set(snd_n, chn_n, on, CP_AP_ZX, S_setB S_x_zoom_slider));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_ap_zx_reversed, g_set_ap_zx)


static XEN g_ap_zy(XEN snd_n, XEN chn_n) 
{
  #define H_y_zoom_slider "(" S_y_zoom_slider " (snd #f) (chn #f)): current y axis zoom slider of snd channel chn"
  return(channel_get(snd_n, chn_n, CP_AP_ZY, S_y_zoom_slider));
}

static XEN g_set_ap_zy(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(on), on, XEN_ARG_1, S_setB S_y_zoom_slider, "a number");
  return(channel_set(snd_n, chn_n, on, CP_AP_ZY, S_setB S_y_zoom_slider));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_ap_zy_reversed, g_set_ap_zy)

static XEN g_edit_hook(XEN snd_n, XEN chn_n) 
{
  #define H_edit_hook "(" S_edit_hook " (snd #f) (chn #f)): snd's channel chn's edit-hook. \
This is a channel-specific hook variable; the hook procedures are thunks -- they should take no \
arguments. " S_edit_hook " is called just before any attempt to edit the channel's data; if it returns #t, \
the edit is aborted."
  return(channel_get(snd_n, chn_n, CP_EDIT_HOOK, S_edit_hook));
}

static XEN g_after_edit_hook(XEN snd_n, XEN chn_n) 
{
  #define H_after_edit_hook "(" S_after_edit_hook " (snd #f) (chn #f)): snd's channel chn's after-edit-hook. \
This is a channel-specific hook variable; the hook procedures are thunks -- they should take no \
arguments. " S_after_edit_hook " is called after an edit, but before " S_after_graph_hook "."

  return(channel_get(snd_n, chn_n, CP_AFTER_EDIT_HOOK, S_after_edit_hook));
}

static XEN g_undo_hook(XEN snd_n, XEN chn_n) 
{
  #define H_undo_hook "(" S_undo_hook " (snd #f) (chn #f)): snd's channel chn's undo-hook. \
This is a channel-specific hook variable; the hook procedures are thunks -- they should take no \
arguments. " S_undo_hook " is called just after any undo, redo, or revert that affects the channel."

  return(channel_get(snd_n, chn_n, CP_UNDO_HOOK, S_undo_hook));
}

static XEN g_show_y_zero(XEN snd, XEN chn)
{
  #define H_show_y_zero "(" S_show_y_zero " (snd #t) (chn #t)): #t if Snd should include a line at y = 0.0"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_Y_ZERO, S_show_y_zero));
  return(C_TO_XEN_BOOLEAN(show_y_zero(get_global_state())));
}

static XEN g_set_show_y_zero(XEN on, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_show_y_zero, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_Y_ZERO, S_setB S_show_y_zero));
  else
    {
      ss = get_global_state();
      set_show_y_zero(ss, XEN_TO_C_BOOLEAN_OR_TRUE(on));
      return(C_TO_XEN_BOOLEAN(show_y_zero(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_y_zero_reversed, g_set_show_y_zero)

static XEN g_min_dB(XEN snd, XEN chn) 
{
  #define H_min_dB "(" S_min_dB " (snd #t) (chn #t)): min dB value displayed in fft graphs using dB scales (default: -60)"
  snd_state *ss;
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_MIN_DB, S_min_dB));
  ss = get_global_state();
  return(C_TO_XEN_DOUBLE(ss->min_dB));
}

static XEN g_set_min_dB(XEN val, XEN snd, XEN chn) 
{
  Float db;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_min_dB, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_MIN_DB, S_setB S_min_dB));
  else
    {
      db = XEN_TO_C_DOUBLE(val);
      ss = get_global_state();
      ss->min_dB = db;
      ss->lin_dB = pow(10.0, db * 0.05);
      channel_set(XEN_TRUE, XEN_TRUE, val, CP_MIN_DB, S_setB S_min_dB);
      return(C_TO_XEN_DOUBLE(ss->min_dB));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_min_dB_reversed, g_set_min_dB)

static XEN g_fft_window_beta(XEN snd, XEN chn) 
{
  #define H_fft_window_beta "(" S_fft_window_beta " *optional (snd #t) (chn #t)): fft window beta parameter value"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_WINDOW_BETA, S_fft_window_beta));
  return(C_TO_XEN_DOUBLE(fft_window_beta(get_global_state())));
}

static XEN g_set_fft_window_beta(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_fft_window_beta, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_FFT_WINDOW_BETA, S_setB S_fft_window_beta));
  else
    {
      ss = get_global_state();
      set_fft_window_beta(ss, mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), 1.0));
      return(C_TO_XEN_DOUBLE(fft_window_beta(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_window_beta_reversed, g_set_fft_window_beta)

static XEN g_spectro_cutoff(XEN snd, XEN chn) 
{
  #define H_spectro_cutoff "(" S_spectro_cutoff " *optional (snd #t) (chn #t)): max frequency shown in spectra (1.0 = srate/2)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_CUTOFF, S_spectro_cutoff));
  return(C_TO_XEN_DOUBLE(spectro_cutoff(get_global_state())));
}

static XEN g_set_spectro_cutoff(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_cutoff, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_CUTOFF, S_setB S_spectro_cutoff));
  else
    {
      ss = get_global_state();
      set_spectro_cutoff(ss, mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), 1.0));
      return(C_TO_XEN_DOUBLE(spectro_cutoff(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_cutoff_reversed, g_set_spectro_cutoff)

static XEN g_spectro_start(XEN snd, XEN chn) 
{
  #define H_spectro_start "(" S_spectro_start " *optional (snd #t) (chn #t)): lower bound of frequency in spectral displays (0.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_START, S_spectro_start));
  return(C_TO_XEN_DOUBLE(spectro_start(get_global_state())));
}

static XEN g_set_spectro_start(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_start, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_START, S_setB S_spectro_start));
  else
    {
      ss = get_global_state();
      set_spectro_start(ss, mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), 1.0));
      return(C_TO_XEN_DOUBLE(spectro_start(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_start_reversed, g_set_spectro_start)

static XEN g_spectro_x_angle(XEN snd, XEN chn) 
{
  #define H_spectro_x_angle "(" S_spectro_x_angle " *optional (snd #t) (chn #t)): spectrogram x-axis viewing angle (90.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_X_ANGLE, S_spectro_x_angle));
  return(C_TO_XEN_DOUBLE(spectro_x_angle(get_global_state())));
}

static XEN g_set_spectro_x_angle(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_x_angle, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_X_ANGLE, S_setB S_spectro_x_angle));
  else
    {
      ss = get_global_state();
      set_spectro_x_angle(ss, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(spectro_x_angle(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_x_angle_reversed, g_set_spectro_x_angle)

static XEN g_spectro_x_scale(XEN snd, XEN chn) 
{
  #define H_spectro_x_scale "(" S_spectro_x_scale " *optional (snd #t) (chn #t)): scaler (stretch) along the spectrogram x axis (1.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_X_SCALE, S_spectro_x_scale));
  return(C_TO_XEN_DOUBLE(spectro_x_scale(get_global_state())));
}

static XEN g_set_spectro_x_scale(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_x_scale, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_X_SCALE, S_setB S_spectro_x_scale));
  else
    {
      ss = get_global_state();
      set_spectro_x_scale(ss, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(spectro_x_scale(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_x_scale_reversed, g_set_spectro_x_scale)

static XEN g_spectro_y_angle(XEN snd, XEN chn) 
{
  #define H_spectro_y_angle "(" S_spectro_y_angle " *optional (snd #t) (chn #t)): spectrogram y-axis viewing angle (0.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Y_ANGLE, S_spectro_y_angle));
  return(C_TO_XEN_DOUBLE(spectro_y_angle(get_global_state())));
}

static XEN g_set_spectro_y_angle(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_y_angle, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Y_ANGLE, S_setB S_spectro_y_angle));
  else
    {
      ss = get_global_state();
      set_spectro_y_angle(ss, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(spectro_y_angle(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_y_angle_reversed, g_set_spectro_y_angle)

static XEN g_spectro_y_scale(XEN snd, XEN chn) 
{
  #define H_spectro_y_scale "(" S_spectro_y_scale " *optional (snd #t) (chn #t)): scaler (stretch) along the spectrogram y axis (1.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Y_SCALE, S_spectro_y_scale));
  return(C_TO_XEN_DOUBLE(spectro_y_scale(get_global_state())));
}

static XEN g_set_spectro_y_scale(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_y_scale, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Y_SCALE, S_setB S_spectro_y_scale));
  else
    {
      ss = get_global_state();
      set_spectro_y_scale(ss, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(spectro_y_scale(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_y_scale_reversed, g_set_spectro_y_scale)

static XEN g_spectro_z_angle(XEN snd, XEN chn) 
{
  #define H_spectro_z_angle "(" S_spectro_z_angle " *optional (snd #t) (chn #t)): spectrogram z-axis viewing angle (-2.0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Z_ANGLE, S_spectro_z_angle));
  return(C_TO_XEN_DOUBLE(spectro_z_angle(get_global_state())));
}

static XEN g_set_spectro_z_angle(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_z_angle, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Z_ANGLE, S_setB S_spectro_z_angle));
  else
    {
      ss = get_global_state();
      set_spectro_z_angle(ss, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(spectro_z_angle(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_z_angle_reversed, g_set_spectro_z_angle)

static XEN g_spectro_z_scale(XEN snd, XEN chn) 
{
  #define H_spectro_z_scale "(" S_spectro_z_scale " *optional (snd #t) (chn #t)): scaler (stretch) along the spectrogram z axis (0.1)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_Z_SCALE, S_spectro_z_scale));
  return(C_TO_XEN_DOUBLE(spectro_z_scale(get_global_state())));
}

static XEN g_set_spectro_z_scale(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_z_scale, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_Z_SCALE, S_setB S_spectro_z_scale));
  else
    {
      ss = get_global_state();
      set_spectro_z_scale(ss, XEN_TO_C_DOUBLE(val));
      return(C_TO_XEN_DOUBLE(spectro_z_scale(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_z_scale_reversed, g_set_spectro_z_scale)

static XEN g_spectro_hop(XEN snd, XEN chn)
{
  #define H_spectro_hop "(" S_spectro_hop " (snd #t) (chn #t)): hop amount (pixels) in spectral displays"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SPECTRO_HOP, S_spectro_hop));
  return(C_TO_XEN_INT(spectro_hop(get_global_state())));
}

static XEN g_set_spectro_hop(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_spectro_hop, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SPECTRO_HOP, S_setB S_spectro_hop));
  else
    {
      ss = get_global_state();
      set_spectro_hop(ss, XEN_TO_C_INT_OR_ELSE(val, 0));
      return(C_TO_XEN_INT(spectro_hop(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_hop_reversed, g_set_spectro_hop)


static XEN g_show_marks(XEN snd, XEN chn)
{
  #define H_show_marks "(" S_show_marks " (snd #t) (chn #t)): #t if Snd should show marks"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_MARKS, S_show_marks));
  return(C_TO_XEN_BOOLEAN(show_marks(get_global_state())));
}

static XEN g_set_show_marks(XEN on, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_show_marks, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_MARKS, S_setB S_show_marks));
  else
    {
      ss = get_global_state();
      set_show_marks(ss, XEN_TO_C_BOOLEAN_OR_TRUE(on));
      return(C_TO_XEN_BOOLEAN(show_marks(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_marks_reversed, g_set_show_marks)

static XEN g_show_transform_peaks(XEN snd, XEN chn)
{
  #define H_show_transform_peaks "(" S_show_transform_peaks " (snd #t) (chn #t)): #t if fft display should include peak list"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_TRANSFORM_PEAKS, S_show_transform_peaks));
  return(C_TO_XEN_BOOLEAN(show_transform_peaks(get_global_state())));
}

static XEN g_set_show_transform_peaks(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ARG_1, S_setB S_show_transform_peaks, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_SHOW_TRANSFORM_PEAKS, S_setB S_show_transform_peaks));
  else
    {
      ss = get_global_state();
      set_show_transform_peaks(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val));
      return(C_TO_XEN_BOOLEAN(show_transform_peaks(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_transform_peaks_reversed, g_set_show_transform_peaks)

static XEN g_zero_pad(XEN snd, XEN chn)
{
  #define H_zero_pad "(" S_zero_pad " (snd #t) (chn #t)): zero padding used in fft as a multiple of fft size (0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_ZERO_PAD, S_zero_pad));
  return(C_TO_XEN_INT(zero_pad(get_global_state())));
}

static XEN g_set_zero_pad(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_zero_pad, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_ZERO_PAD, S_setB S_zero_pad));
  else
    {
      ss = get_global_state();
      set_zero_pad(ss, g_imin(0, val, DEFAULT_ZERO_PAD));
      return(C_TO_XEN_INT(zero_pad(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_zero_pad_reversed, g_set_zero_pad)

static XEN g_wavelet_type(XEN snd, XEN chn)
{
  #define H_wavelet_type "(" S_wavelet_type " (snd #t) (chn #t)): wavelet used in wavelet-transform (0)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_WAVELET_TYPE, S_wavelet_type));
  return(C_TO_XEN_INT(wavelet_type(get_global_state())));
}

static XEN g_set_wavelet_type(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_wavelet_type, "an integer"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_WAVELET_TYPE, S_setB S_wavelet_type));
  else
    {
      ss = get_global_state();
      set_wavelet_type(ss, mus_iclamp(0, XEN_TO_C_INT(val), NUM_WAVELETS - 1));
      return(C_TO_XEN_INT(wavelet_type(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_wavelet_type_reversed, g_set_wavelet_type)

static XEN g_fft_log_frequency(XEN snd, XEN chn)
{
  #define H_fft_log_frequency "(" S_fft_log_frequency " (snd #t) (chn #t)): #t if fft displays use log on the frequency axis (#f)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_LOG_FREQUENCY, S_fft_log_frequency));
  return(C_TO_XEN_BOOLEAN(fft_log_frequency(get_global_state())));
}

static XEN g_set_fft_log_frequency(XEN on, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_fft_log_frequency, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_FFT_LOG_FREQUENCY, S_setB S_fft_log_frequency));
  else
    {
      ss = get_global_state();
      set_fft_log_frequency(ss, XEN_TO_C_BOOLEAN_OR_TRUE(on)); 
      return(C_TO_XEN_BOOLEAN(fft_log_frequency(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_log_frequency_reversed, g_set_fft_log_frequency)

static XEN g_fft_log_magnitude(XEN snd, XEN chn)
{
  #define H_fft_log_magnitude "(" S_fft_log_magnitude " (snd #t) (chn #t)): #t if fft displays use dB (#f)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_LOG_MAGNITUDE, S_fft_log_magnitude));
  return(C_TO_XEN_BOOLEAN(fft_log_magnitude(get_global_state())));
}

static XEN g_set_fft_log_magnitude(XEN on, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_fft_log_magnitude, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_FFT_LOG_MAGNITUDE, S_setB S_fft_log_magnitude));
  else
    {
      ss = get_global_state();
      set_fft_log_magnitude(ss, XEN_TO_C_BOOLEAN_OR_TRUE(on)); 
      return(C_TO_XEN_BOOLEAN(fft_log_magnitude(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_log_magnitude_reversed, g_set_fft_log_magnitude)

static XEN g_show_mix_waveforms(XEN snd, XEN chn)
{
  #define H_show_mix_waveforms "(" S_show_mix_waveforms " (snd #t) (chn #t)): #t if Snd should display mix waveforms (above the main waveform)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_MIX_WAVEFORMS, S_show_mix_waveforms));
  return(C_TO_XEN_BOOLEAN(show_mix_waveforms(get_global_state())));
}

static XEN g_set_show_mix_waveforms(XEN on, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_show_mix_waveforms, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_MIX_WAVEFORMS, S_setB S_show_mix_waveforms));
  else
    {
      ss = get_global_state();
      set_show_mix_waveforms(ss, XEN_TO_C_BOOLEAN_OR_TRUE(on));
      return(C_TO_XEN_BOOLEAN(show_mix_waveforms(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_mix_waveforms_reversed, g_set_show_mix_waveforms)

static XEN g_verbose_cursor(XEN snd, XEN chn)
{
  #define H_verbose_cursor "(" S_verbose_cursor " (snd #t) (chn #t)): #t if the cursor's position and so on is displayed in the minibuffer"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_VERBOSE_CURSOR, S_verbose_cursor));
  return(C_TO_XEN_BOOLEAN(verbose_cursor(get_global_state())));
}

static XEN g_set_verbose_cursor(XEN on, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_verbose_cursor, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_VERBOSE_CURSOR, S_setB S_verbose_cursor));
  else
    {
      ss = get_global_state();
      set_verbose_cursor(ss, XEN_TO_C_BOOLEAN_OR_TRUE(on));
      return(C_TO_XEN_BOOLEAN(verbose_cursor(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_verbose_cursor_reversed, g_set_verbose_cursor)


static XEN g_time_graph_type(XEN snd, XEN chn)
{
  #define H_time_graph_type "(" S_time_graph_type " (snd #t) (chn #t)): " S_graph_as_wavogram " if Snd's time domain display is a 'wavogram',\
otherwise " S_graph_once "."
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TIME_GRAPH_TYPE, S_time_graph_type));
  return(C_TO_XEN_INT(time_graph_type(get_global_state())));
}

static XEN g_set_time_graph_type(XEN val, XEN snd, XEN chn) 
{
  int on;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(val), val, XEN_ARG_1, S_setB S_time_graph_type, "an integer (default: " S_graph_once ")");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_TIME_GRAPH_TYPE, S_setB S_time_graph_type));
  else
    {
      ss = get_global_state();
      on = XEN_TO_C_INT_OR_ELSE(val, GRAPH_ONCE);
      set_time_graph_type(ss, on);
      return(C_TO_XEN_BOOLEAN(time_graph_type(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_time_graph_type_reversed, g_set_time_graph_type)

static XEN g_wavo_hop(XEN snd, XEN chn)
{
  #define H_wavo_hop "(" S_wavo_hop " (snd #t) (chn #t)): wavogram spacing between successive traces"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_WAVO_HOP, S_wavo_hop));
  return(C_TO_XEN_INT(wavo_hop(get_global_state())));
}

static XEN g_set_wavo_hop(XEN val, XEN snd, XEN chn) 
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_wavo_hop, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_WAVO_HOP, S_setB S_wavo_hop));
  else
    {
      ss = get_global_state();
      set_wavo_hop(ss, XEN_TO_C_INT_OR_ELSE(val, 0));
      return(C_TO_XEN_INT(wavo_hop(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_wavo_hop_reversed, g_set_wavo_hop)

static XEN g_wavo_trace(XEN snd, XEN chn)
{
  #define H_wavo_trace "(" S_wavo_trace " (snd #t) (chn #t)): length (samples) of each trace in the wavogram (64)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_WAVO_TRACE, S_wavo_trace));
  return(C_TO_XEN_INT(wavo_trace(get_global_state())));
}

static XEN g_set_wavo_trace(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_wavo_trace, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_WAVO_TRACE, S_setB S_wavo_trace));
  else
    {
      ss = get_global_state();
      set_wavo_trace(ss, XEN_TO_C_INT_OR_ELSE(val, 0));
      return(C_TO_XEN_INT(wavo_trace(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_wavo_trace_reversed, g_set_wavo_trace)

static XEN g_transform_size(XEN snd, XEN chn)
{
  #define H_transform_size "(" S_transform_size " (snd #t) (chn #t)): current fft size (256)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_SIZE, S_transform_size));
  return(C_TO_XEN_INT(transform_size(get_global_state())));
}

static XEN g_set_transform_size(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  int len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_transform_size, "an integer"); 
  len = XEN_TO_C_INT(val);
  if (!(POWER_OF_2_P(len)))
    len = snd_ipow2((int)(log(len + 1) / log(2.0)));
  if (len <= 0) return(XEN_FALSE);
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_SIZE, S_setB S_transform_size));
  else
    {
      ss = get_global_state();
      set_transform_size(ss, len);
      return(C_TO_XEN_INT(transform_size(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_transform_size_reversed, g_set_transform_size)

static XEN g_transform_graph_type(XEN snd, XEN chn)
{
  #define H_transform_graph_type "(" S_transform_graph_type " (snd #t) (chn #t)) can \
be " S_graph_once ", " S_graph_as_sonogram ", or " S_graph_as_spectrogram "."
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_GRAPH_TYPE, S_transform_graph_type));
  return(C_TO_XEN_INT(transform_graph_type(get_global_state())));
}

static XEN g_set_transform_graph_type(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  int style;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_transform_graph_type, "an integer"); 
  style = mus_iclamp(GRAPH_ONCE, XEN_TO_C_INT(val), GRAPH_AS_SPECTROGRAM);
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, C_TO_SMALL_XEN_INT(style), CP_TRANSFORM_GRAPH_TYPE, S_setB S_transform_graph_type));
  else
    {
      ss = get_global_state();
      set_transform_graph_type(ss, style);
      return(C_TO_XEN_INT(transform_graph_type(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_transform_graph_type_reversed, g_set_transform_graph_type)

static XEN g_fft_window(XEN snd, XEN chn)
{
  #define H_fft_window "(" S_fft_window " (snd #t) (chn #t)): fft data window choice (e.g. blackman2-window).  The \
choices are: rectangular-window, hann(ing)-window, welch-window, parzen-window, \
bartlett-window, hamming-window, blackman2-window, blackman3-window, \
blackman4-window, exponential-window, riemann-window, kaiser-window, \
cauchy-window, poisson-window, gaussian-window, tukey-window, \
dolph-chebyshev-window (if GSL is loaded)"

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_FFT_WINDOW, S_fft_window));
  return(C_TO_XEN_INT(fft_window(get_global_state())));
}

static XEN g_set_fft_window(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  int win;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_fft_window, "an integer"); 
  win = mus_iclamp(0, XEN_TO_C_INT(val), NUM_FFT_WINDOWS - 1);
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, C_TO_SMALL_XEN_INT(win), CP_FFT_WINDOW, S_setB S_fft_window));
  else
    {
      ss = get_global_state();
      set_fft_window(ss, win);
      return(C_TO_XEN_INT(fft_window(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_window_reversed, g_set_fft_window)

static XEN g_transform_type(XEN snd, XEN chn)
{
  #define H_transform_type "(" S_transform_type " (snd #t) (chn #t)): transform type; can be one of fourier-transform, \
wavelet-transform, haar-transform, autocorrelation, walsh-transform, hadamard-transform, cepstrum, or an index corresponding \
to an added transform."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_TYPE, S_transform_type));
  return(C_TO_XEN_INT(transform_type(get_global_state())));
}

static XEN g_set_transform_type(XEN val, XEN snd, XEN chn)
{
  int type;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_1, S_setB S_transform_type, "an integer"); 
  type = mus_iclamp(0, XEN_TO_C_INT(val), max_transform_type());
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, C_TO_SMALL_XEN_INT(type), CP_TRANSFORM_TYPE, S_setB S_transform_type));
  else
    {
      ss = get_global_state();
      set_transform_type(ss, type);
      return(C_TO_XEN_INT(transform_type(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_transform_type_reversed, g_set_transform_type)

static XEN g_transform_normalization(XEN snd, XEN chn)
{
  #define H_transform_normalization "(" S_transform_normalization " (snd #t) (chn #t)): one of '(" S_dont_normalize " " S_normalize_by_channel " " S_normalize_by_sound " " S_normalize_globally ") \
decides whether spectral data is normalized before display (default: " S_normalize_by_channel ")"

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TRANSFORM_NORMALIZATION, S_transform_normalization));
  return(C_TO_XEN_INT(transform_normalization(get_global_state())));
}

static XEN g_set_transform_normalization(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(val), val, XEN_ARG_1, S_setB S_transform_normalization, "an integer");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_TRANSFORM_NORMALIZATION, S_setB S_transform_normalization));
  else
    {
      ss = get_global_state();
      set_transform_normalization(ss, mus_iclamp(DONT_NORMALIZE,
						 XEN_TO_C_INT_OR_ELSE(val, DEFAULT_TRANSFORM_NORMALIZATION),
						 NORMALIZE_GLOBALLY));
      return(C_TO_XEN_INT(transform_normalization(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_transform_normalization_reversed, g_set_transform_normalization)

static XEN g_max_transform_peaks(XEN snd, XEN chn)
{
  #define H_max_transform_peaks "(" S_max_transform_peaks " (snd #t) (chn #t)): max number of fft peaks reported in fft display"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_MAX_TRANSFORM_PEAKS, S_max_transform_peaks));
  return(C_TO_XEN_INT(max_transform_peaks(get_global_state())));
}

static XEN g_set_max_transform_peaks(XEN n, XEN snd, XEN chn)
{
  int lim;
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_1, S_setB S_max_transform_peaks, "an integer"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, n, CP_MAX_TRANSFORM_PEAKS, S_setB S_max_transform_peaks));
  else
    {
      lim = XEN_TO_C_INT(n);
      ss = get_global_state();
      if (lim >= 1)
	set_max_transform_peaks(ss, lim);
      return(C_TO_XEN_INT(max_transform_peaks(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_max_transform_peaks_reversed, g_set_max_transform_peaks)


static XEN g_graph_style(XEN snd, XEN chn)
{
  #define H_graph_style "(" S_graph_style " (snd #t) (chn #t)): graph style, one \
of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ")"

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_TIME_GRAPH_STYLE, S_time_graph_style));
  return(C_TO_XEN_INT(graph_style(get_global_state())));
}

static XEN g_set_graph_style(XEN style, XEN snd, XEN chn)
{
  snd_state *ss;
  int val;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_graph_style, "an integer"); 
  val = XEN_TO_C_INT(style);
  if (XEN_BOUND_P(snd))
    {
      if (GRAPH_STYLE_OK(val))
	{
	  XEN val;
	  call_update_graph = FALSE;
	  val = channel_set(snd, chn, style, CP_TIME_GRAPH_STYLE, S_setB S_graph_style);
	  channel_set(snd, chn, style, CP_LISP_GRAPH_STYLE, S_setB S_graph_style);
	  call_update_graph = TRUE;
	  channel_set(snd, chn, style, CP_TRANSFORM_GRAPH_STYLE, S_setB S_graph_style);
	  return(val);
	}
    }
  else
    {
      if (GRAPH_STYLE_OK(val))
	{
	  ss = get_global_state();
	  set_graph_style(ss, val);
	  return(C_TO_XEN_INT(graph_style(ss)));
	}
    }
  XEN_OUT_OF_RANGE_ERROR(S_setB S_graph_style, 1, style, "unknown graph-style");
  return(XEN_FALSE);
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_graph_style_reversed, g_set_graph_style)

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
  if (GRAPH_STYLE_OK(val))
    return(channel_set(snd, chn, style, CP_TIME_GRAPH_STYLE, S_setB S_time_graph_style));
  XEN_OUT_OF_RANGE_ERROR(S_setB S_time_graph_style, 1, style, "unknown " S_time_graph_style);
  return(XEN_FALSE);
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_time_graph_style_reversed, g_set_time_graph_style)

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
  if (GRAPH_STYLE_OK(val))
    return(channel_set(snd, chn, style, CP_LISP_GRAPH_STYLE, S_setB S_lisp_graph_style));
  XEN_OUT_OF_RANGE_ERROR(S_setB S_lisp_graph_style, 1, style, "unknown " S_lisp_graph_style);
  return(XEN_FALSE);
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_lisp_graph_style_reversed, g_set_lisp_graph_style)

static XEN g_transform_graph_style(XEN snd, XEN chn)
{
  #define H_transform_graph_style "(" S_lisp_graph_style " snd chn): fft graph drawing style, one \
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
  if (GRAPH_STYLE_OK(val))
    return(channel_set(snd, chn, style, CP_TRANSFORM_GRAPH_STYLE, S_setB S_transform_graph_style));
  XEN_OUT_OF_RANGE_ERROR(S_setB S_transform_graph_style, 1, style, "unknown " S_transform_graph_style);
  return(XEN_FALSE);
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_transform_graph_style_reversed, g_set_transform_graph_style)


static XEN g_dot_size(XEN snd, XEN chn)
{
  #define H_dot_size "(" S_dot_size " (snd #t) (chn #t)): size in pixels of dots when graphing with dots (1)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_DOT_SIZE, S_dot_size));
  return(C_TO_XEN_INT(dot_size(get_global_state())));
}

static XEN g_set_dot_size(XEN size, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_1, S_setB S_dot_size, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, size, CP_DOT_SIZE, S_setB S_dot_size));
  else
    {
      ss = get_global_state();
      set_dot_size(ss, XEN_TO_C_INT_OR_ELSE(size, 0));
      return(C_TO_XEN_INT(dot_size(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_dot_size_reversed, g_set_dot_size)

static XEN g_x_axis_style(XEN snd, XEN chn)
{
  #define H_x_axis_style "(" S_x_axis_style " (snd #t) (chn #t)) \
The x axis labelling of the time domain waveform can be in seconds (" S_x_axis_in_seconds "), in samples (" S_x_axis_in_samples "), expressed as a \
percentage of the overall duration (" S_x_axis_as_percentage "), or as a beat number (" S_x_axis_in_beats ")."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_X_AXIS_STYLE, S_x_axis_style));
  return(C_TO_XEN_INT(x_axis_style(get_global_state())));
}

static XEN g_set_x_axis_style(XEN style, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(style), style, XEN_ARG_1, S_setB S_x_axis_style, "an integer"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, style, CP_X_AXIS_STYLE, S_setB S_x_axis_style));
  else
    {
      ss = get_global_state();
      set_x_axis_style(ss, mus_iclamp(X_AXIS_IN_SECONDS, XEN_TO_C_INT(style), X_AXIS_IN_BEATS));
      /* snd-menu.c -- maps over chans */
      return(C_TO_XEN_INT(x_axis_style(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_x_axis_style_reversed, g_set_x_axis_style)

static XEN g_beats_per_minute(XEN snd, XEN chn)
{
  #define H_beats_per_minute "(" S_beats_per_minute " (snd #t) (chn #t)): beats per minute if " S_x_axis_style " is " S_x_axis_in_beats
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_BEATS_PER_MINUTE, S_beats_per_minute));
  return(C_TO_XEN_DOUBLE(beats_per_minute(get_global_state())));
}

static XEN g_set_beats_per_minute(XEN beats, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beats), beats, XEN_ARG_1, S_setB S_beats_per_minute, "a number"); 
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, beats, CP_BEATS_PER_MINUTE, S_setB S_beats_per_minute));
  else
    {
      Float val;
      ss = get_global_state();
      val = XEN_TO_C_DOUBLE(beats);
      if (val > 0.0)
	set_beats_per_minute(ss, val);
      return(C_TO_XEN_DOUBLE(beats_per_minute(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_beats_per_minute_reversed, g_set_beats_per_minute)

static XEN g_show_axes(XEN snd, XEN chn)
{
  #define H_show_axes "(" S_show_axes "(snd #t) (chn #t)) \
If " S_show_all_axes ", display x and y axes; if " S_show_x_axis ", just one axis (the x axis) is displayed. \
The other choice is " S_show_no_axes "."

  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_SHOW_AXES, S_show_axes));
  return(C_TO_XEN_INT(show_axes(get_global_state())));
}

static XEN g_set_show_axes(XEN on, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(on), on, XEN_ARG_1, S_setB S_show_axes, "an integer");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, on, CP_SHOW_AXES, S_setB S_show_axes));
  else
    {
      ss = get_global_state();
      set_show_axes(ss, mus_iclamp(SHOW_NO_AXES, XEN_TO_C_INT_OR_ELSE(on, SHOW_ALL_AXES), SHOW_X_AXIS));
      return(C_TO_XEN_INT(show_axes(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_axes_reversed, g_set_show_axes)

static XEN g_graphs_horizontal(XEN snd, XEN chn)
{
  #define H_graphs_horizontal "(" S_graphs_horizontal " (snd #t) (chn #t)): #t if the time domain, fft, and lisp graphs are layed out horizontally (#t)"
  if (XEN_BOUND_P(snd))
    return(channel_get(snd, chn, CP_GRAPHS_HORIZONTAL, S_graphs_horizontal));
  return(C_TO_XEN_BOOLEAN(graphs_horizontal(get_global_state())));
}

static XEN g_set_graphs_horizontal(XEN val, XEN snd, XEN chn)
{
  snd_state *ss;
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ARG_1, S_setB S_graphs_horizontal, "a boolean");
  if (XEN_BOUND_P(snd))
    return(channel_set(snd, chn, val, CP_GRAPHS_HORIZONTAL, S_setB S_graphs_horizontal));
  else
    {
      ss = get_global_state();
      set_graphs_horizontal(ss, XEN_TO_C_BOOLEAN_OR_TRUE(val)); 
      return(C_TO_XEN_BOOLEAN(graphs_horizontal(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_graphs_horizontal_reversed, g_set_graphs_horizontal)


static XEN g_peaks(XEN filename, XEN snd_n, XEN chn_n)
{
  #define H_peaks "(" S_peaks " (filename #f) (snd #f) (chn #f)): write current fft peaks data to filename, or \
to the help dialog if filename is omitted"

  chan_info *cp;
  char *name = NULL;
  int err;
  XEN_ASSERT_TYPE((XEN_STRING_P(filename) || (XEN_FALSE_P(filename)) || (XEN_NOT_BOUND_P(filename))), filename, XEN_ARG_1, S_peaks, "a string or #f");
  ASSERT_CHANNEL(S_peaks, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_peaks);
  if (XEN_STRING_P(filename))
    name = mus_expand_filename(XEN_TO_C_STRING(filename));
  else name = NULL;
  err = display_transform_peaks(cp, name);
  if (name) FREE(name);
  if ((XEN_STRING_P(filename)) && (err == 0)) 
    return(filename);
  return(XEN_FALSE);
}

static XEN g_left_sample(XEN snd_n, XEN chn_n) 
{
  #define H_left_sample "(" S_left_sample " (snd #f) (chn #f)): left sample number in time domain window"
  return(channel_get(snd_n, chn_n, CP_AP_LOSAMP, S_left_sample));
}

static XEN g_set_left_sample(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_OFF_T_P(on) || XEN_NOT_BOUND_P(on), on, XEN_ARG_1, S_setB S_left_sample, "an integer");
  return(channel_set(snd_n, chn_n, on, CP_AP_LOSAMP, S_setB S_left_sample));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_left_sample_reversed, g_set_left_sample)

static XEN g_right_sample(XEN snd_n, XEN chn_n) 
{
  #define H_right_sample "(" S_right_sample " (snd #f) (chn #f)): right sample number in time domain window"
  return(channel_get(snd_n, chn_n, CP_AP_HISAMP, S_right_sample));
}

static XEN g_set_right_sample(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_OFF_T_P(on) || XEN_NOT_BOUND_P(on), on, XEN_ARG_1, S_setB S_right_sample, "an integer");
  return(channel_set(snd_n, chn_n, on, CP_AP_HISAMP, S_setB S_right_sample));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_right_sample_reversed, g_set_right_sample)

static XEN g_channel_properties(XEN snd_n, XEN chn_n) 
{
  #define H_channel_properties "(" S_channel_properties " (snd #f) (chn #f)): \
A property list associated with the given channel. It is set to '() at the time a sound is opened. \
The accessor channel-property is provided in extensions.scm."

  return(channel_get(snd_n, chn_n, CP_PROPERTIES, S_channel_properties));
}

static XEN g_set_channel_properties(XEN on, XEN snd_n, XEN chn_n) 
{
  XEN_ASSERT_TYPE(XEN_LIST_P(on), on, XEN_ARG_1, S_setB S_channel_properties, "a property list");
  return(channel_set(snd_n, chn_n, on, CP_PROPERTIES, S_setB S_channel_properties));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_channel_properties_reversed, g_set_channel_properties)


static XEN g_edits(XEN snd_n, XEN chn_n)
{
  #define H_edits "(" S_edits " (snd #f) (chn #f)): a list of undoable and redoable edits in snd's channel chn"
  chan_info *cp;
  int i;
  ASSERT_CHANNEL(S_edits, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_edits);
  for (i = cp->edit_ctr + 1; i < cp->edit_size; i++)
    if (!(cp->edits[i])) break;
  return(XEN_LIST_2(C_TO_XEN_INT(cp->edit_ctr),
		    C_TO_XEN_INT(i - cp->edit_ctr - 1)));
}

static XEN g_x_bounds(XEN snd_n, XEN chn_n)
{
  #define H_x_bounds "(" S_x_bounds " (snd #f) (chn #f)): a list (x0 x1) giving the current x axis bounds of snd channel chn"
  chan_info *cp;
  ASSERT_CHANNEL(S_x_bounds, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_x_bounds);
  return(XEN_LIST_2(C_TO_XEN_DOUBLE(cp->axis->x0),
		    C_TO_XEN_DOUBLE(cp->axis->x1)));
  /* wavogram settings depend on context -- no easy way to map back to user's notion of bounds */
}

static XEN g_set_x_bounds(XEN bounds, XEN snd_n, XEN chn_n)
{
  chan_info *cp;
  Float x0, x1;
  ASSERT_CHANNEL(S_setB S_x_bounds, snd_n, chn_n, 2);
  XEN_ASSERT_TYPE(XEN_LIST_P(bounds) && (XEN_LIST_LENGTH(bounds) == 2), bounds, XEN_ARG_1, S_setB S_x_bounds, "a list: (x0 x1)");
  cp = get_cp(snd_n, chn_n, S_setB S_x_bounds);
  if (cp->time_graph_type == GRAPH_ONCE) 
    {
      x0 = XEN_TO_C_DOUBLE(XEN_CAR(bounds));
      x1 = XEN_TO_C_DOUBLE(XEN_CADR(bounds));
      if (x1 > x0)
	set_x_axis_x0x1(cp, x0, x1);
      else XEN_OUT_OF_RANGE_ERROR(S_setB S_x_bounds, 1, bounds, "x1 > x0?");
    }
  return(bounds);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_x_bounds_reversed, g_set_x_bounds)

static XEN g_set_y_bounds(XEN bounds, XEN snd_n, XEN chn_n)
{
  chan_info *cp;
  Float low, hi;
  int len = 0;
  XEN y0 = XEN_UNDEFINED; XEN y1 = XEN_UNDEFINED;
  ASSERT_CHANNEL(S_setB S_y_bounds, snd_n, chn_n, 2);
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(bounds, len), bounds, XEN_ARG_1, S_setB S_y_bounds, "a list");
  cp = get_cp(snd_n, chn_n, S_setB S_y_bounds);
  if (len > 0)
    {
      y0 = XEN_CAR(bounds);
      if (len > 1)
	y1 = XEN_CADR(bounds);
    }
  if (XEN_NUMBER_P(y0))
    {
      low = XEN_TO_C_DOUBLE(y0);
      if (XEN_NUMBER_P(y1))
	hi = XEN_TO_C_DOUBLE(y1);
      else
	{
	  if (low < 0.0)
	    hi = -low;
	  else
	    {
	      hi = low;
	      low = -low;
	    }
	}
    }
  else
    {
      /* if no bounds given, use maxamp */
      hi = get_maxamp(cp->sound, cp, AT_CURRENT_EDIT_POSITION);
      if (hi < 0.0) hi = -hi;
      if (hi == 0.0) hi = .001;
      low = -hi;
    }
  if (hi > low)
    {
      axis_info *ap;
      ap = cp->axis;
      ap->ymin = low;
      ap->ymax = hi;
      ap->y_ambit = (ap->ymax - ap->ymin);
      ap->y0 = low;
      ap->y1 = hi;
      ap->zy = 1.0;
      ap->sy = 0.0;
      resize_sy(cp);
      resize_zy(cp);
      apply_y_axis_change(ap, cp);
    }
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_y_bounds, 1, bounds, "y1 < y0?");
  return(bounds);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_y_bounds_reversed, g_set_y_bounds)

static XEN g_y_bounds(XEN snd_n, XEN chn_n)
{
  #define H_y_bounds "(" S_y_bounds " (snd #f) (chn #f)): a list (y0 y1) giving the current y axis bounds of snd channel chn"
  chan_info *cp;
  ASSERT_CHANNEL(S_y_bounds, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_y_bounds);
  return(XEN_LIST_2(C_TO_XEN_DOUBLE(cp->axis->y0),
		    C_TO_XEN_DOUBLE(cp->axis->y1)));
}

static XEN g_graph(XEN ldata, XEN xlabel, XEN x0, XEN x1, XEN y0, XEN y1, XEN snd_n, XEN chn_n, XEN force_display)
{
  #define H_graph "(" S_graph " data (xlabel #f) (x0 0.0) (x1 1.0) (y0 #f) (y1 #f) (snd #f) (chn #f) (force-display #t)): \
displays 'data' as a graph with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1; 'data' can be a list, vct, or vector. \
If 'data' is a list of numbers, it is treated as an envelope."

  chan_info *cp;
  lisp_grf *lg;
  XEN data = XEN_UNDEFINED; XEN lst;
  char *label = NULL;
  vct *v = NULL;
  XEN *vdata;
  int i, len, graph, graphs, need_update = FALSE;
  Float ymin, ymax, val;
  double nominal_x0, nominal_x1;
  lisp_grf *old_lp = NULL;
  Latus h = 0, w = 0, ww = 0;
  Locus o = 0, gx0 = 0;
  axis_info *uap = NULL;
  /* ldata can be a vct object, a vector, or a list of either */
  XEN_ASSERT_TYPE(((VCT_P(ldata)) || 
		   (XEN_VECTOR_P(ldata) && (XEN_VECTOR_LENGTH(ldata) > 0)) ||
		   ((XEN_LIST_P(ldata)) && (XEN_LIST_LENGTH(ldata) > 0) && 
		    ((XEN_NUMBER_P(XEN_CAR(ldata))) || (XEN_VECTOR_P(XEN_CAR(ldata))) || (VCT_P(XEN_CAR(ldata)))))),
		  ldata, XEN_ARG_1, S_graph, "a vct, vector, or list");
  ASSERT_CHANNEL(S_graph, snd_n, chn_n, 7);
  cp = get_cp(snd_n, chn_n, S_graph);
  ymin = 32768.0;
  ymax = -32768.0;
  if ((cp->sound_ctr == -1) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL) ||
      (cp->axis == NULL))
    return(XEN_FALSE);
  if (XEN_STRING_P(xlabel)) label = XEN_TO_C_STRING(xlabel); 
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
  if ((lg) && (graphs != lg->graphs)) 
    {
      old_lp = (lisp_grf *)(cp->lisp_info);
      uap = old_lp->axis;
      if (uap)
	{
	  h = uap->height;
	  w = uap->width;
	  ww = uap->window_width;
	  o = uap->y_offset;
	  gx0 = uap->graph_x0;
	}
      cp->lisp_info = free_lisp_info(cp);
    }
  if (!(cp->lisp_info))
    {
      cp->lisp_info = (lisp_grf *)CALLOC(graphs, sizeof(lisp_grf));
      lg = cp->lisp_info;
      lg->len = (int *)CALLOC(graphs, sizeof(int));
      lg->graphs = graphs;
      lg->data = (Float **)CALLOC(graphs, sizeof(Float *));
      need_update = TRUE;
    }
  if ((XEN_LIST_P_WITH_LENGTH(ldata, len)) &&
      (XEN_NUMBER_P(XEN_CAR(ldata))))
    {
      lg = cp->lisp_info;
      lg->env_data = 1;
      if (lg->len[0] != len)
	{
	  if (lg->data[0]) FREE(lg->data[0]);
	  lg->data[0] = (Float *)CALLOC(len, sizeof(Float));
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
	  if (VCT_P(data))
	    {
	      v = (vct *)XEN_OBJECT_REF(data);
	      len = v->length;
	    }
	  else 
	    {
	      if (XEN_VECTOR_P(data))
		len = XEN_VECTOR_LENGTH(data);
	      else XEN_WRONG_TYPE_ARG_ERROR(S_graph, 1, data, "data list element must be vector or vct?");
	    }
	  if (lg->len[graph] != len)
	    {
	      if (lg->data[graph]) FREE(lg->data[graph]);
	      lg->data[graph] = (Float *)CALLOC(len, sizeof(Float));
	      lg->len[graph] = len;
	    }
	  if (v)
	    memcpy((void *)(lg->data[graph]), (void *)(v->data), len * sizeof(Float));
	  else 
	    {
	      vdata = XEN_VECTOR_ELEMENTS(data);
	      for (i = 0; i < len; i++) 
		lg->data[graph][i] = XEN_TO_C_DOUBLE(vdata[i]);
	    }
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
  if (need_update)
    {
      uap = lg->axis;
      uap->height = h;
      uap->window_width = ww;
      uap->y_offset = o;
      uap->width = w;
      uap->graph_x0 = gx0;
    }
  cp->graph_lisp_p = TRUE;
  if ((XEN_NOT_BOUND_P(force_display)) || 
      (XEN_NOT_FALSE_P(force_display)))
    {
      if (need_update)
	update_graph(cp);
      else display_channel_lisp_data(cp, cp->sound, cp->state);
    }
  return(xen_return_first(XEN_FALSE, data));
}


static XEN g_colormap_ref(XEN map, XEN pos)
{
  #define H_colormap_ref "(colormap-ref map (pos #f)): (list r g b). map can be a number \
between 0.0 and 1.0 with pos omitted -- in this case the color_map and so on comes from the color \
dialog.  Colormap names can be found in rgb.scm"
  unsigned short r, g, b;
  snd_state *ss;
  if (XEN_NOT_BOUND_P(pos))
    {
      ss = get_global_state();
      XEN_ASSERT_TYPE(XEN_NUMBER_P(map), map, XEN_ARG_1, "colormap-ref", "a number");
      get_current_color(color_map(ss), skew_color(ss, XEN_TO_C_DOUBLE(map)), &r, &g, &b);
    }
  else
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_P(map), map, XEN_ARG_1, "colormap-ref", "an integer");
      XEN_ASSERT_TYPE(XEN_INTEGER_P(pos), pos, XEN_ARG_2, "colormap-ref", "an integer");
      get_current_color(XEN_TO_C_INT(map), XEN_TO_C_INT(pos), &r, &g, &b);
    }
  return(XEN_LIST_3(C_TO_XEN_DOUBLE((float)r / 65535.0),
		    C_TO_XEN_DOUBLE((float)g / 65535.0),
		    C_TO_XEN_DOUBLE((float)b / 65535.0)));
}

#if HAVE_GL
static XEN g_gl_spectrogram(XEN data, XEN gl_list, XEN cutoff, XEN use_dB, XEN min_dB, XEN scale, XEN br, XEN bg, XEN bb)
{
  #define H_glSpectrogram "(glSpectrogram data gl-list cutoff use-dB min-dB scale br bg bb) takes spectrogram \
data and passes it to openGL.  See snd-gl.scm for an example."
  sono_info *si;
  vct *v;
  int i;
  XEN_ASSERT_TYPE(XEN_VECTOR_P(data), data, XEN_ARG_1, "glSpectrogram", "a vector of vcts");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(gl_list), gl_list, XEN_ARG_2, "glSpectrogram", "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(cutoff), cutoff, XEN_ARG_3, "glSpectrogram", "a number");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(use_dB), use_dB, XEN_ARG_4, "glSpectrogram", "a boolean");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(min_dB), min_dB, XEN_ARG_5, "glSpectrogram", "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scale), scale, XEN_ARG_6, "glSpectrogram", "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(br), br, XEN_ARG_7, "glSpectrogram", "an integer (pixel value)");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(bg), bg, XEN_ARG_8, "glSpectrogram", "an integer (pixel value)");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(bb), bb, XEN_ARG_9, "glSpectrogram", "an integer (pixel value)");
  si = (sono_info *)CALLOC(1, sizeof(sono_info));
  si->active_slices = XEN_VECTOR_LENGTH(data);
  si->data = (Float **)CALLOC(si->active_slices, sizeof(Float *));
  v = get_vct(XEN_VECTOR_REF(data, 0));
  si->target_bins = v->length;
  si->scale = XEN_TO_C_DOUBLE(scale);
  for (i = 0; i < si->active_slices; i++)
    {
      v = get_vct(XEN_VECTOR_REF(data, i));
      si->data[i] = v->data;
    }
  gl_spectrogram(get_global_state(),
		 si, 
		 XEN_TO_C_INT(gl_list),
		 XEN_TO_C_DOUBLE(cutoff),
		 XEN_TO_C_BOOLEAN(use_dB),
		 XEN_TO_C_DOUBLE(min_dB),
		 (unsigned short)(XEN_TO_C_INT(br)),
		 (unsigned short)(XEN_TO_C_INT(bg)),
		 (unsigned short)(XEN_TO_C_INT(bb)));
  FREE(si->data);
  FREE(si);
  return(XEN_FALSE);
}
#endif


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_9(g_graph_w, g_graph)
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
XEN_ARGIFY_2(g_cursor_size_w, g_cursor_size)
XEN_ARGIFY_3(g_set_cursor_size_w, g_set_cursor_size)
XEN_ARGIFY_2(g_left_sample_w, g_left_sample)
XEN_ARGIFY_3(g_set_left_sample_w, g_set_left_sample)
XEN_ARGIFY_2(g_right_sample_w, g_right_sample)
XEN_ARGIFY_3(g_set_right_sample_w, g_set_right_sample)
XEN_ARGIFY_2(g_channel_properties_w, g_channel_properties)
XEN_ARGIFY_3(g_set_channel_properties_w, g_set_channel_properties)
XEN_ARGIFY_2(g_max_transform_peaks_w, g_max_transform_peaks)
XEN_ARGIFY_3(g_set_max_transform_peaks_w, g_set_max_transform_peaks)
XEN_ARGIFY_2(g_show_y_zero_w, g_show_y_zero)
XEN_ARGIFY_3(g_set_show_y_zero_w, g_set_show_y_zero)
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
XEN_ARGIFY_2(g_min_dB_w, g_min_dB)
XEN_ARGIFY_3(g_set_min_dB_w, g_set_min_dB)
XEN_ARGIFY_2(g_wavelet_type_w, g_wavelet_type)
XEN_ARGIFY_3(g_set_wavelet_type_w, g_set_wavelet_type)
XEN_ARGIFY_2(g_spectro_cutoff_w, g_spectro_cutoff)
XEN_ARGIFY_3(g_set_spectro_cutoff_w, g_set_spectro_cutoff)
XEN_ARGIFY_2(g_spectro_start_w, g_spectro_start)
XEN_ARGIFY_3(g_set_spectro_start_w, g_set_spectro_start)
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
XEN_ARGIFY_2(g_beats_per_minute_w, g_beats_per_minute)
XEN_ARGIFY_3(g_set_beats_per_minute_w, g_set_beats_per_minute)
XEN_ARGIFY_2(g_show_axes_w, g_show_axes)
XEN_ARGIFY_3(g_set_show_axes_w, g_set_show_axes)
XEN_ARGIFY_2(g_graphs_horizontal_w, g_graphs_horizontal)
XEN_ARGIFY_3(g_set_graphs_horizontal_w, g_set_graphs_horizontal)
XEN_ARGIFY_2(g_x_bounds_w, g_x_bounds)
XEN_ARGIFY_3(g_set_x_bounds_w, g_set_x_bounds)
XEN_ARGIFY_2(g_y_bounds_w, g_y_bounds)
XEN_ARGIFY_3(g_set_y_bounds_w, g_set_y_bounds)
XEN_ARGIFY_2(g_update_time_graph_w, g_update_time_graph)
XEN_ARGIFY_2(g_update_lisp_graph_w, g_update_lisp_graph)
XEN_ARGIFY_2(g_update_transform_graph_w, g_update_transform_graph)
XEN_ARGIFY_2(g_colormap_ref_w, g_colormap_ref)
#if HAVE_GL
  XEN_NARGIFY_9(g_gl_spectrogram_w, g_gl_spectrogram)
#endif
#else
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
#define g_cursor_size_w g_cursor_size
#define g_set_cursor_size_w g_set_cursor_size
#define g_left_sample_w g_left_sample
#define g_set_left_sample_w g_set_left_sample
#define g_right_sample_w g_right_sample
#define g_set_right_sample_w g_set_right_sample
#define g_channel_properties_w g_channel_properties
#define g_set_channel_properties_w g_set_channel_properties
#define g_max_transform_peaks_w g_max_transform_peaks
#define g_set_max_transform_peaks_w g_set_max_transform_peaks
#define g_show_y_zero_w g_show_y_zero
#define g_set_show_y_zero_w g_set_show_y_zero
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
#define g_min_dB_w g_min_dB
#define g_set_min_dB_w g_set_min_dB
#define g_wavelet_type_w g_wavelet_type
#define g_set_wavelet_type_w g_set_wavelet_type
#define g_spectro_cutoff_w g_spectro_cutoff
#define g_set_spectro_cutoff_w g_set_spectro_cutoff
#define g_spectro_start_w g_spectro_start
#define g_set_spectro_start_w g_set_spectro_start
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
#define g_beats_per_minute_w g_beats_per_minute
#define g_set_beats_per_minute_w g_set_beats_per_minute
#define g_show_axes_w g_show_axes
#define g_set_show_axes_w g_set_show_axes
#define g_graphs_horizontal_w g_graphs_horizontal
#define g_set_graphs_horizontal_w g_set_graphs_horizontal
#define g_x_bounds_w g_x_bounds
#define g_set_x_bounds_w g_set_x_bounds
#define g_y_bounds_w g_y_bounds
#define g_set_y_bounds_w g_set_y_bounds
#define g_update_time_graph_w g_update_time_graph
#define g_update_lisp_graph_w g_update_lisp_graph
#define g_update_transform_graph_w g_update_transform_graph
#define g_colormap_ref_w g_colormap_ref
#if HAVE_GL
  #define g_gl_spectrogram_w g_gl_spectrogram
#endif
#endif

void g_init_chn(void)
{
  cp_edpos = XEN_UNDEFINED;

  XEN_DEFINE_PROCEDURE(S_graph,                   g_graph_w, 1, 8, 0,                   H_graph);
  XEN_DEFINE_PROCEDURE(S_edits,                   g_edits_w, 0, 2, 0,                   H_edits);
  XEN_DEFINE_PROCEDURE(S_peaks,                   g_peaks_w, 0, 3, 0,                   H_peaks);
  XEN_DEFINE_PROCEDURE(S_edit_hook,               g_edit_hook_w, 0, 2, 0,               H_edit_hook);
  XEN_DEFINE_PROCEDURE(S_after_edit_hook,         g_after_edit_hook_w, 0, 2, 0,         H_after_edit_hook);
  XEN_DEFINE_PROCEDURE(S_undo_hook,               g_undo_hook_w, 0, 2, 0,               H_undo_hook);
  XEN_DEFINE_PROCEDURE(S_update_time_graph,       g_update_time_graph_w, 0, 2, 0,       H_update_time_graph);
  XEN_DEFINE_PROCEDURE(S_update_lisp_graph,       g_update_lisp_graph_w, 0, 2, 0,       H_update_lisp_graph);
  XEN_DEFINE_PROCEDURE(S_update_transform_graph,  g_update_transform_graph_w, 0, 2, 0,  H_update_transform_graph);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_x_position_slider, g_ap_sx_w, H_x_position_slider,
					    S_setB S_x_position_slider, g_set_ap_sx_w, g_set_ap_sx_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_y_position_slider, g_ap_sy_w, H_y_position_slider,
					    S_setB S_y_position_slider, g_set_ap_sy_w, g_set_ap_sy_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_x_zoom_slider, g_ap_zx_w, H_x_zoom_slider,
					    S_setB S_x_zoom_slider, g_set_ap_zx_w, g_set_ap_zx_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_y_zoom_slider, g_ap_zy_w, H_y_zoom_slider,
					    S_setB S_y_zoom_slider, g_set_ap_zy_w, g_set_ap_zy_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_frames, g_frames_w, H_frames,
					    S_setB S_frames, g_set_frames_w, g_set_frames_reversed, 0, 3, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_maxamp, g_maxamp_w, H_maxamp,
					    S_setB S_maxamp, g_set_maxamp_w, g_set_maxamp_reversed, 0, 3, 0, 3);
  
  XEN_DEFINE_PROCEDURE(S_cursor_position,   g_cursor_position_w, 0, 2, 0,   H_cursor_position);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_edit_position, g_edit_position_w, H_edit_position,
					    S_setB S_edit_position, g_set_edit_position_w, g_set_edit_position_reversed, 0, 2, 0, 3);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_graph_p, g_transform_graph_p_w, H_transform_graph_p,
					    S_setB S_transform_graph_p, g_set_transform_graph_p_w, g_set_transform_graph_p_reversed, 0, 2, 0, 3);

  #define H_graph_once "The value for the various graph-type variables that displays the standard waveform"
  #define H_graph_as_wavogram "The value for " S_time_graph_type " to make a spectrogram-like form of the time-domain data"

  XEN_DEFINE_CONSTANT(S_graph_once,        GRAPH_ONCE,        H_graph_once);
  XEN_DEFINE_CONSTANT(S_graph_as_wavogram, GRAPH_AS_WAVOGRAM, H_graph_as_wavogram);

  #define H_graph_as_sonogram "The value for " S_transform_graph_type " that causes a sonogram to be displayed"
  #define H_graph_as_spectrogram "The value for " S_transform_graph_type " that causes a spectrogram to be displayed"

  XEN_DEFINE_CONSTANT(S_graph_as_sonogram,    GRAPH_AS_SONOGRAM,    H_graph_as_sonogram);
  XEN_DEFINE_CONSTANT(S_graph_as_spectrogram, GRAPH_AS_SPECTROGRAM, H_graph_as_spectrogram);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_time_graph_p, g_time_graph_p_w, H_time_graph_p,
					    S_setB S_time_graph_p, g_set_time_graph_p_w, g_set_time_graph_p_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_lisp_graph_p, g_lisp_graph_p_w, H_lisp_graph_p,
					    S_setB S_lisp_graph_p, g_set_lisp_graph_p_w, g_set_lisp_graph_p_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_squelch_update, g_squelch_update_w, H_squelch_update,
					    S_setB S_squelch_update, g_set_squelch_update_w, g_set_squelch_update_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_cursor, g_cursor_w, H_cursor,
					    S_setB S_cursor, g_set_cursor_w, g_set_cursor_reversed, 0, 3, 0, 4);
  
  #define H_cursor_cross "The value for " S_cursor_style " that causes is to be a cross (the default)"
  #define H_cursor_line "The value for " S_cursor_style " that causes is to be a full vertical line"

  XEN_DEFINE_CONSTANT(S_cursor_cross,          CURSOR_CROSS, H_cursor_cross);
  XEN_DEFINE_CONSTANT(S_cursor_line,           CURSOR_LINE,  H_cursor_line);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_cursor_style, g_cursor_style_w, H_cursor_style,
					    S_setB S_cursor_style, g_set_cursor_style_w, g_set_cursor_style_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_cursor_size, g_cursor_size_w, H_cursor_size,
					    S_setB S_cursor_size, g_set_cursor_size_w, g_set_cursor_size_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_left_sample, g_left_sample_w, H_left_sample,
					    S_setB S_left_sample, g_set_left_sample_w, g_set_left_sample_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_right_sample, g_right_sample_w, H_right_sample,
					    S_setB S_right_sample, g_set_right_sample_w, g_set_right_sample_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_channel_properties, g_channel_properties_w, H_channel_properties,
					    S_setB S_channel_properties, g_set_channel_properties_w, g_set_channel_properties_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_max_transform_peaks, g_max_transform_peaks_w, H_max_transform_peaks,
					    S_setB S_max_transform_peaks, g_set_max_transform_peaks_w, g_set_max_transform_peaks_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_y_zero, g_show_y_zero_w, H_show_y_zero,
					    S_setB S_show_y_zero, g_set_show_y_zero_w, g_set_show_y_zero_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_marks, g_show_marks_w, H_show_marks,
					    S_setB S_show_marks, g_set_show_marks_w, g_set_show_marks_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_time_graph_type, g_time_graph_type_w, H_time_graph_type,
					    S_setB S_time_graph_type, g_set_time_graph_type_w, g_set_time_graph_type_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_wavo_hop, g_wavo_hop_w, H_wavo_hop,
					    S_setB S_wavo_hop, g_set_wavo_hop_w, g_set_wavo_hop_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_wavo_trace, g_wavo_trace_w, H_wavo_trace,
					    S_setB S_wavo_trace, g_set_wavo_trace_w, g_set_wavo_trace_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_transform_peaks, g_show_transform_peaks_w, H_show_transform_peaks,
					    S_setB S_show_transform_peaks, g_set_show_transform_peaks_w, g_set_show_transform_peaks_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_zero_pad, g_zero_pad_w, H_zero_pad,
					    S_setB S_zero_pad, g_set_zero_pad_w, g_set_zero_pad_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_verbose_cursor, g_verbose_cursor_w, H_verbose_cursor,
					    S_setB S_verbose_cursor, g_set_verbose_cursor_w, g_set_verbose_cursor_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_log_frequency, g_fft_log_frequency_w, H_fft_log_frequency,
					    S_setB S_fft_log_frequency, g_set_fft_log_frequency_w, g_set_fft_log_frequency_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_log_magnitude, g_fft_log_magnitude_w, H_fft_log_magnitude,
					    S_setB S_fft_log_magnitude, g_set_fft_log_magnitude_w, g_set_fft_log_magnitude_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_min_dB, g_min_dB_w, H_min_dB,
					    S_setB S_min_dB, g_set_min_dB_w, g_set_min_dB_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_wavelet_type, g_wavelet_type_w, H_wavelet_type,
					    S_setB S_wavelet_type, g_set_wavelet_type_w, g_set_wavelet_type_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_cutoff, g_spectro_cutoff_w, H_spectro_cutoff,
					    S_setB S_spectro_cutoff, g_set_spectro_cutoff_w, g_set_spectro_cutoff_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_start, g_spectro_start_w, H_spectro_start,
					    S_setB S_spectro_start, g_set_spectro_start_w, g_set_spectro_start_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_x_angle, g_spectro_x_angle_w, H_spectro_x_angle,
					    S_setB S_spectro_x_angle, g_set_spectro_x_angle_w, g_set_spectro_x_angle_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_x_scale, g_spectro_x_scale_w, H_spectro_x_scale,
					    S_setB S_spectro_x_scale, g_set_spectro_x_scale_w, g_set_spectro_x_scale_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_y_angle, g_spectro_y_angle_w, H_spectro_y_angle,
					    S_setB S_spectro_y_angle, g_set_spectro_y_angle_w, g_set_spectro_y_angle_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_y_scale, g_spectro_y_scale_w, H_spectro_y_scale,
					    S_setB S_spectro_y_scale, g_set_spectro_y_scale_w, g_set_spectro_y_scale_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_z_angle, g_spectro_z_angle_w, H_spectro_z_angle,
					    S_setB S_spectro_z_angle, g_set_spectro_z_angle_w, g_set_spectro_z_angle_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_z_scale, g_spectro_z_scale_w, H_spectro_z_scale,
					    S_setB S_spectro_z_scale, g_set_spectro_z_scale_w, g_set_spectro_z_scale_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_window_beta, g_fft_window_beta_w, H_fft_window_beta,
					    S_setB S_fft_window_beta, g_set_fft_window_beta_w, g_set_fft_window_beta_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_spectro_hop, g_spectro_hop_w, H_spectro_hop,
					    S_setB S_spectro_hop, g_set_spectro_hop_w, g_set_spectro_hop_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_size, g_transform_size_w, H_transform_size,
					    S_setB S_transform_size, g_set_transform_size_w, g_set_transform_size_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_graph_type, g_transform_graph_type_w, H_transform_graph_type,
					    S_setB S_transform_graph_type, g_set_transform_graph_type_w, g_set_transform_graph_type_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_fft_window, g_fft_window_w, H_fft_window,
					    S_setB S_fft_window, g_set_fft_window_w, g_set_fft_window_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_type, g_transform_type_w, H_transform_type,
					    S_setB S_transform_type, g_set_transform_type_w, g_set_transform_type_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_transform_normalization, g_transform_normalization_w, H_transform_normalization,
					    S_setB S_transform_normalization, g_set_transform_normalization_w, g_set_transform_normalization_reversed, 0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_mix_waveforms, g_show_mix_waveforms_w, H_show_mix_waveforms,
					    S_setB S_show_mix_waveforms, g_set_show_mix_waveforms_w, g_set_show_mix_waveforms_reversed, 0, 2, 0, 3);
  
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
					    0, 2, 0, 3);
  
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_dot_size, g_dot_size_w, H_dot_size,
					    S_setB S_dot_size, g_set_dot_size_w, g_set_dot_size_reversed,
					    0, 2, 0, 3);

  #define H_x_axis_in_seconds    "The value for " S_x_axis_style " that displays the x axis using seconds"
  #define H_x_axis_in_samples    "The value for " S_x_axis_style " that displays the x axis using sample numbers"
  #define H_x_axis_in_beats      "The value for " S_x_axis_style " that displays the x axis using beats (also beats-per-minute)"
  #define H_x_axis_as_percentage "The value for " S_x_axis_style " that displays the x axis using percentages"
  /* what about X_AXIS_IN_LENGTH? */

  XEN_DEFINE_CONSTANT(S_x_axis_in_seconds,     X_AXIS_IN_SECONDS,    H_x_axis_in_seconds);
  XEN_DEFINE_CONSTANT(S_x_axis_in_samples,     X_AXIS_IN_SAMPLES,    H_x_axis_in_samples);
  XEN_DEFINE_CONSTANT(S_x_axis_in_beats,       X_AXIS_IN_BEATS,      H_x_axis_in_beats);
  XEN_DEFINE_CONSTANT(S_x_axis_as_percentage,  X_AXIS_AS_PERCENTAGE, H_x_axis_as_percentage);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_x_axis_style, g_x_axis_style_w, H_x_axis_style,
					    S_setB S_x_axis_style, g_set_x_axis_style_w, g_set_x_axis_style_reversed,
					    0, 2, 0, 3);
  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_beats_per_minute, g_beats_per_minute_w, H_beats_per_minute,
					    S_setB S_beats_per_minute, g_set_beats_per_minute_w, g_set_beats_per_minute_reversed,
					    0, 2, 0, 3);

  #define H_show_all_axes "The value for " S_show_axes " that causes both the x and y axes to be displayed"
  #define H_show_no_axes "The value for " S_show_axes " that causes neither the x or y axes to be displayed"
  #define H_show_x_axis "The value for " S_show_axes " that causes only the x axis to be displayed"

  XEN_DEFINE_CONSTANT(S_show_all_axes,         SHOW_ALL_AXES, H_show_all_axes);
  XEN_DEFINE_CONSTANT(S_show_no_axes,          SHOW_NO_AXES,  H_show_no_axes);
  XEN_DEFINE_CONSTANT(S_show_x_axis,           SHOW_X_AXIS,   H_show_x_axis);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_show_axes, g_show_axes_w, H_show_axes,
					    S_setB S_show_axes, g_set_show_axes_w, g_set_show_axes_reversed, 0, 2, 0, 3);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_graphs_horizontal, g_graphs_horizontal_w, H_graphs_horizontal,
					    S_setB S_graphs_horizontal, g_set_graphs_horizontal_w, g_set_graphs_horizontal_reversed, 0, 2, 0, 3);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_x_bounds, g_x_bounds_w, H_x_bounds,
					    S_setB S_x_bounds, g_set_x_bounds_w, g_set_x_bounds_reversed, 0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(S_y_bounds, g_y_bounds_w, H_y_bounds,
					    S_setB S_y_bounds, g_set_y_bounds_w, g_set_y_bounds_reversed, 0, 2, 1, 2);

  XEN_DEFINE_PROCEDURE("colormap-ref", g_colormap_ref_w, 1, 1, 0, H_colormap_ref);
#if HAVE_GL
  XEN_DEFINE_PROCEDURE("glSpectrogram", g_gl_spectrogram_w, 9, 0, 0, H_glSpectrogram);
#endif

  #define H_transform_hook S_transform_hook " (snd chn scaler): called just after a spectrum is calculated."
  #define H_graph_hook S_graph_hook " (snd chn y0 y1): called each time a graph is about to be updated. If it returns #t, the display is not updated."
  #define H_after_graph_hook S_after_graph_hook " (snd chn): called after a graph is updated."
  #define H_lisp_graph_hook S_lisp_graph_hook " (snd chn): called just before the lisp graph is updated. If it returns a list \
of pixels, these are used in order by the list of graphs (if any), rather than Snd's default set; \
this makes it possible to use different colors for the various graphs. \
If it returns a function (of no arguments), that function is called rather than the standard graph routine."
  #define H_mouse_press_hook S_mouse_press_hook " (snd chn button state x y): called upon mouse button press within the lisp graph."
  #define H_mouse_click_hook S_mouse_click_hook " (snd chn button state x y axis): called upon button click."
  #define H_mouse_release_hook S_mouse_release_hook " (snd chn button state x y): called upon mouse button release within the lisp graph."
  #define H_mouse_drag_hook S_mouse_drag_hook " (snd chn button state x y): called upon mouse drag within the lisp graph."
  #define H_mark_click_hook S_mark_click_hook " (id): called when a mark is clicked; return #t to squelch the default message."
  #define H_key_press_hook S_key_press_hook " (snd chn key state): called upon a key press if the mouse is in the lisp graph. \
If it returns #t, the key press is not passed to the main handler. 'state' refers to the control, meta, and shift keys."
  #define H_initial_graph_hook S_initial_graph_hook " (snd chn dur): called when a sound is displayed for the first time"

  
  XEN_DEFINE_HOOK(transform_hook,     S_transform_hook, 3,     H_transform_hook);     /* args = sound channel scaler */
  XEN_DEFINE_HOOK(graph_hook,         S_graph_hook, 4,         H_graph_hook);         /* args = sound channel y0 y1 */
  XEN_DEFINE_HOOK(after_graph_hook,   S_after_graph_hook, 2,   H_after_graph_hook);   /* args = sound channel */
  XEN_DEFINE_HOOK(lisp_graph_hook,    S_lisp_graph_hook, 2,    H_lisp_graph_hook);    /* args = sound channel */
  XEN_DEFINE_HOOK(mouse_press_hook,   S_mouse_press_hook, 6,   H_mouse_press_hook);   /* args = sound channel button state x y */
  XEN_DEFINE_HOOK(mouse_click_hook,   S_mouse_click_hook, 7,   H_mouse_click_hook);   /* args = sound channel button state x y axis */
  XEN_DEFINE_HOOK(mouse_release_hook, S_mouse_release_hook, 6, H_mouse_release_hook); /* args = sound channel button state x y */
  XEN_DEFINE_HOOK(mouse_drag_hook,    S_mouse_drag_hook, 6,    H_mouse_drag_hook);    /* args = sound channel button state x y */
  XEN_DEFINE_HOOK(key_press_hook,     S_key_press_hook, 4,     H_key_press_hook);     /* args = sound channel key state */
  XEN_DEFINE_HOOK(mark_click_hook,    S_mark_click_hook, 1,    H_mark_click_hook);    /* arg = id */
  XEN_DEFINE_HOOK(initial_graph_hook, S_initial_graph_hook, 3, H_initial_graph_hook); /* args = sound channel duration */
}


