#include "snd.h"

enum {NOGRAPH, WAVE, FFT_AXIS, LISP, FFT_MAIN};    /* for marks, regions, mouse click detection */

static void after_fft(snd_state *ss, chan_info *cp, Float scaler);

static SCM lisp_graph_hook;
static SCM mouse_press_hook, mark_click_hook;
static SCM mouse_release_hook, mouse_drag_hook, key_press_hook, fft_hook;
static SCM graph_hook, after_graph_hook;

static void set_y_bounds(axis_info *ap);
static int map_chans_wavo(chan_info *cp, void *ptr) 
{
  cp->wavo = (*((int *)ptr)); 
  if (cp->wavo == 0) 
    {
      set_y_bounds(cp->axis);
      resize_sy(cp);
      set_x_bounds(cp->axis);
      resize_sx(cp);
    }
  update_graph(cp, NULL); 
  return(0);
}
static void set_wavo(snd_state *ss, int val) {in_set_wavo(ss, val); map_over_chans(ss, map_chans_wavo, (void *)(&val));}

static int map_chans_wavo_hop(chan_info *cp, void *ptr) {cp->wavo_hop = (*((int *)ptr)); update_graph(cp, NULL); return(0);}
static void set_wavo_hop(snd_state *ss, int uval) 
{
  int val; 
  if (uval < 1) 
    val = 1; 
  else val = uval; 
  in_set_wavo_hop(ss, val); 
  map_over_chans(ss, map_chans_wavo_hop, (void *)(&val));
}

static int map_chans_wavo_trace(chan_info *cp, void *ptr) {cp->wavo_trace = (*((int *)ptr)); update_graph(cp, NULL); return(0);}
void set_wavo_trace(snd_state *ss, int uval) 
{
  int val; 
  if (uval < 1) 
    val = 1; 
  else val = uval; 
  in_set_wavo_trace(ss, val); 
  map_over_chans(ss, map_chans_wavo_trace, (void *)(&val));
}

static int map_chans_max_fft_peaks(chan_info *cp, void *ptr) {cp->max_fft_peaks = (*((int *)ptr)); return(0);}
static void set_max_fft_peaks(snd_state *ss, int uval) 
{
  int val; 
  if (uval < 0) 
    val = 0; 
  else val = uval; 
  in_set_max_fft_peaks(ss, val); 
  map_over_chans(ss, map_chans_max_fft_peaks, (void *)(&val));
}

static int map_chans_zero_pad(chan_info *cp, void *ptr) {cp->zero_pad = (*((int *)ptr)); return(0);}
static void set_zero_pad(snd_state *ss, int uval) 
{
  int val; 
  if (uval < 0) 
    val = 0; 
  else val = uval; 
  in_set_zero_pad(ss, val); 
  map_over_chans(ss, map_chans_zero_pad, (void *)(&val));
}

static int map_chans_fft_style(chan_info *cp, void *ptr) {cp->fft_style = (*((int *)ptr)); return(0);}
void in_set_fft_style(snd_state *ss, int uval) 
{
  int val;
  val = mus_iclamp(0, uval, MAX_FFT_STYLE);
  in_set_fft_style_1(ss, val); 
  map_over_chans(ss, map_chans_fft_style, (void *)(&val));
}

static int map_chans_show_mix_waveforms(chan_info *cp, void *ptr) {cp->show_mix_waveforms = (*((int *)ptr)); return(0);}
static void set_show_mix_waveforms(snd_state *ss, int val) {in_set_show_mix_waveforms(ss, val); map_over_chans(ss, map_chans_show_mix_waveforms, (void *)(&val));}

static int map_chans_show_axes(chan_info *cp, void *ptr) {cp->show_axes = (*((int *)ptr)); update_graph(cp, NULL); return(0);}
static void set_show_axes(snd_state *ss, int val) {in_set_show_axes(ss, val); map_over_chans(ss, map_chans_show_axes, (void *)(&val));}

static int map_chans_graphs_horizontal(chan_info *cp, void *ptr) {cp->graphs_horizontal = (*((int *)ptr)); update_graph(cp, NULL); return(0);}
static void set_graphs_horizontal(snd_state *ss, int val) {in_set_graphs_horizontal(ss, val); map_over_chans(ss, map_chans_graphs_horizontal, (void *)(&val));}

static int map_chans_fft_window(chan_info *cp, void *ptr) 
{
  cp->fft_window = (*((int *)ptr)); 
  if (cp->fft) (cp->fft)->window = (*((int *)ptr));
  return(0);
}
void in_set_fft_window(snd_state *ss, int val) {in_set_fft_window_1(ss, val); map_over_chans(ss, map_chans_fft_window, (void *)(&val));}

void map_chans_field(snd_state *ss, int field, Float val)
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
	    case FCP_X_ANGLE: sp->chans[j]->spectro_x_angle = val;                   break;
	    case FCP_X_SCALE: sp->chans[j]->spectro_x_scale = val;                   break;
	    case FCP_Y_ANGLE: sp->chans[j]->spectro_y_angle = val;                   break;
	    case FCP_Y_SCALE: sp->chans[j]->spectro_y_scale = val;                   break;
	    case FCP_Z_ANGLE: sp->chans[j]->spectro_z_angle = val;                   break;
	    case FCP_Z_SCALE: sp->chans[j]->spectro_z_scale = val;                   break;
	    case FCP_START:   sp->chans[j]->spectro_start = mus_fclamp(0.0, val, 1.0);   break;
	    case FCP_CUTOFF:  sp->chans[j]->spectro_cutoff = mus_fclamp(0.0, val, 1.0);  break;
	    case FCP_BETA:    sp->chans[j]->fft_beta = mus_fclamp(0.0, val, 1.0);        break;
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

void set_chan_fft_in_progress(chan_info *cp, BACKGROUND_FUNCTION_TYPE fp) 
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

int force_fft_clear(chan_info *cp, void *ptr)
{
  if ((cp->cgx) && ((cp->cgx)->fft_in_progress))
    {
      BACKGROUND_REMOVE((cp->cgx)->fft_in_progress);
      finish_progress_report(cp->sound, NOT_FROM_ENVED);
      (cp->cgx)->fft_in_progress = 0;
    }
  if (cp->fft) cp->fft = free_fft_info(cp->fft);
  if (cp->fft_data) {FREE(cp->fft_data); cp->fft_data = NULL;}
  /* this may leave ->wp window unfreed? -- see snd-fft.c free_fft_state */
  return(0);
}

void chan_info_cleanup(chan_info *cp)
{
  chan_context *cx;
  if ((cp) && (cp->cgx))
    {
      cx = cp->cgx;
      cx->selected = 0;
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
  map_chans_field(ss, FCP_START, val);
  if (!(ss->graph_hook_active)) 
    map_over_chans(ss, update_graph, NULL);
}

static int map_chans_dot_size(chan_info *cp, void *ptr) 
{
  cp->dot_size = (*((int *)ptr)); 
  if ((cp->graph_style != GRAPH_LINES) && (cp->graph_style != GRAPH_FILLED))
    update_graph(cp, NULL);
  return(0);
}

void set_dot_size(snd_state *ss, int val)
{
  if (val > 0)  /* -1 here can crash X! */
    {
      in_set_dot_size(ss, (Latus)val);
      map_over_chans(ss, map_chans_dot_size, (void *)(&val));
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

int calculate_fft(chan_info *cp, void *ptr)
{
  snd_state *ss;
  if ((cp->ffting) &&
      (!(chan_fft_in_progress(cp))))
    {
      ss = cp->state;
      if (cp->fft_style == NORMAL_FFT)
	{
	  if (cp->fft_size >= 65536) 
	    start_progress_report(cp->sound, NOT_FROM_ENVED);
	  set_chan_fft_in_progress(cp,
				   BACKGROUND_ADD(ss,
						  safe_fft_in_slices,
						  make_fft_state(cp, 1)));
	}
      else set_chan_fft_in_progress(cp,
				    BACKGROUND_ADD(ss,
						   sonogram_in_slices,
						   make_sonogram_state(cp)));
    }
  return(0);
}

static int updating = 0;

int update_graph(chan_info *cp, void *ptr)
{
  /* don't put display stuff here!  This is needed so that the fft display does not get caught in a loop */
  double cur_srate;
  snd_state *ss;
  snd_info *sp;
  axis_info *ap;
  if ((updating) || 
      (cp->active != 1) ||
      (cp->cgx == NULL) || 
      (cp->squelch_update) || 
      (cp->sounds == NULL) || 
      (cp->sounds[cp->sound_ctr] == NULL)) 
    return(0);
  updating = 1;
  ss = cp->state;
  sp = cp->sound;

  /* next two are needed by fft and lisp displays, but if put off until make_graph cause
   * the display to happen twice in some cases 
   */
  ap = cp->axis;
  if (ap)
    {
      cur_srate = (double)(SND_SRATE(sp));
      ap->losamp = (int)(ceil(ap->x0 * cur_srate)); 
      if (ap->losamp < 0) ap->losamp = 0;
      ap->hisamp = (int)(ap->x1 * cur_srate);
    }
  if (!(((cp->cgx)->ax)->wn)) 
    if (!(fixup_cp_cgx_ax_wn(cp))) 
      return(0);
  if ((cp->ffting) && 
      (!(chan_fft_in_progress(cp)))) 
    calculate_fft(cp, NULL);
  display_channel_data(cp, sp, ss);
  updating = 0;
  return(0);
}

#define INITIAL_EDIT_SIZE 8

static SCM initial_graph_hook;

void add_channel_data_1(chan_info *cp, snd_info *sp, snd_state *ss, int graphed)
{
  /* initialize channel, including edit/sound lists */
  axis_info *ap;
  Float ymin = 0.0, ymax = 0.0, xmax, y0, y1, x0, x1, dur, gdur;
  char *label;
  file_info *hdr;
  int samples_per_channel, ymin_set = 0, ymax_set = 0;
  hdr = sp->hdr;
  samples_per_channel = hdr->samples / hdr->chans;
  x0 = DEFAULT_INITIAL_X0;
  x1 = DEFAULT_INITIAL_X1;
  y0 = DEFAULT_INITIAL_Y0;
  y1 = DEFAULT_INITIAL_Y1;
  label = STR_time;
  dur = (Float)samples_per_channel / (Float)hdr->srate;

  cp->edit_size = INITIAL_EDIT_SIZE;
  cp->edit_ctr = 0;
  allocate_ed_list(cp);
  cp->amp_envs = (env_info **)CALLOC(cp->edit_size, sizeof(env_info *));
  cp->samples = (int *)CALLOC(cp->edit_size, sizeof(int));
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
      (HOOKED(initial_graph_hook)))
    {
      SCM res;
      int len;
      res = g_c_run_or_hook(initial_graph_hook,
			    SCM_LIST3(TO_SMALL_SCM_INT(sp->index),
				      TO_SMALL_SCM_INT(cp->chan),
				      TO_SCM_DOUBLE(dur)),
			    S_initial_graph_hook);
      if (LIST_P_WITH_LENGTH(res, len))
	{
	  if (len > 0) x0 = TO_C_DOUBLE(SCM_CAR(res));
	  if (len > 1) x1 = TO_C_DOUBLE(SCM_CADR(res));
	  if (len > 2) y0 = TO_C_DOUBLE(SCM_CADDR(res));
	  if (len > 3) y1 = TO_C_DOUBLE(SCM_CADDDR(res));
	  if (len > 4) label = TO_C_STRING(LIST_REF(res, 4));
	  if (len > 5)
	    {
	      ymin = TO_C_DOUBLE(LIST_REF(res, 5));
	      ymin_set = 1;
	    }
	  if (len > 6)
	    {
	      ymax = TO_C_DOUBLE(LIST_REF(res, 6));
	      ymax_set = 1;
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
      label = STR_no_data;
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
      ap->no_data = 1;
    }
  else
    {
      ap->zx = (ap->x1 - ap->x0) / ap->x_ambit;
      ap->sx = (ap->x0 - ap->xmin) / ap->x_ambit;
      ap->no_data = 0;
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
      cgx->amp_env_state = make_env_state(cp, current_ed_samples(cp));
      cgx->amp_env_in_progress = BACKGROUND_ADD(ss, get_amp_env, (GUI_POINTER)cp);
      reflect_amp_env_in_progress(cp->sound);
    }
}

void add_channel_data(char *filename, chan_info *cp, file_info *hdr, snd_state *ss, int graphed)
{
  int fd, chn = 0;
  int *datai;
  snd_info *sp;
  file_info *chdr;
  sp = cp->sound;
  add_channel_data_1(cp, sp, ss, graphed);
  set_initial_ed_list(cp, (hdr->samples/hdr->chans) - 1);
  chdr = copy_header(filename, sp->hdr); /* need one separate from snd_info case */
  chn = cp->chan;
  if (chdr)
    {
      fd = snd_open_read(ss, filename);
      if (fd != -1)
	{
	  mus_file_open_descriptors(fd,
				    filename, chdr->format,
				    mus_data_format_to_bytes_per_sample(chdr->format),
				    chdr->data_location,
				    chdr->chans,
				    chdr->type);
	  during_open(fd, filename, SND_OPEN_CHANNEL);
	  datai = make_file_state(fd, chdr, chn, FILE_BUFFER_SIZE);
	  cp->sounds[0] = make_snd_data_file(filename, datai,
					     MUS_SAMPLE_ARRAY(datai[file_state_channel_offset(chn)]),
					     chdr, DONT_DELETE_ME, cp->edit_ctr, chn);
	  if (show_usage_stats(ss)) gather_usage_stats(cp);
	}
    }
  if ((current_ed_samples(cp) > AMP_ENV_CUTOFF) &&
      (cp->amp_envs[0] == NULL))                      /* perhaps created in initial-graph-hook by read-peak-env-info-file */
    start_amp_env(cp);
}

static void set_y_bounds(axis_info *ap)
{
  Float range;
  range = ap->zy*ap->y_ambit;
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
  Float range;
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
}

void apply_y_axis_change (axis_info *ap, chan_info *cp)
{
  snd_info *sp;
  chan_info *ncp;
  axis_info *nap;
  int i;
  Float zy, sy;
  set_y_bounds(ap);
  update_graph(cp, NULL);
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
		  update_graph(ncp, NULL);
		}
	    }
	}
    }
}

void set_x_axis_x0x1 (chan_info *cp, Float x0, Float x1) 
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
}

static void set_x_axis_x0(chan_info *cp, int left)
{
  Float x1x0;
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

static void set_x_axis_x1(chan_info *cp, int right)
{
  Float x1x0;
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

static void set_y_axis_y0y1 (chan_info *cp, Float y0, Float y1) 
{
  axis_info *ap;
  ap = cp->axis;
  if (y0 < ap->ymin) ap->ymin = y0;
  if (y1 > ap->ymax) ap->ymax = y1;
  ap->y_ambit = (ap->ymax - ap->ymin);
  ap->y0 = y0;
  ap->y1 = y1;
  ap->zy = (y1 - y0) / ap->y_ambit;
  ap->sy = (y0 - ap->ymin) / ap->y_ambit;
  resize_sy(cp);
  resize_zy(cp);
  apply_y_axis_change(ap, cp);
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
  update_graph(cp, NULL);
}

static void update_xs(chan_info *ncp, axis_info *ap)
{
  Float scl;
  axis_info *nap;
  nap = ncp->axis;
  if (nap->xmax > 0.0)
    {
      scl = ap->xmax/nap->xmax;
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
  update_graph(cp, NULL);
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

static int visible_syncd_cursor(chan_info *cp)
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
	  if (ncp->cursor_visible) return(ncp->cursor);
	}
      /* geez, maybe it's in a separate syncd sound */
      ss = cp->state;
      for (j = 0; j < ss->max_sounds; j++)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse) && (sp->sync == sync) && (sp != cp->sound))
	    {
	      for (i = 0; i < sp->nchans; i++)
		{
		  ncp = sp->chans[i];
		  if (ncp->cursor_visible) return(ncp->cursor);
		}
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
  int newf;
  Float loc, pos;
  if (ap->xmax == 0.0) return;
  if (ap->xmax <= ap->xmin) 
    {
      ap->xmax = ap->xmin + .001;
      ap->x_ambit = .001;
    }
  if (focus_style != FOCUS_LEFT)
    {
      switch (focus_style)
	{
	case FOCUS_RIGHT:   ap->x0 = ap->x1 - ap->zx * ap->x_ambit; break;
	case FOCUS_MIDDLE:  ap->x0 = 0.5 * ((ap->x1 + ap->x0) - ap->zx * ap->x_ambit); break;
	case FOCUS_ACTIVE:
	  ncp = virtual_selected_channel(cp);
	  /* axes should be the same, since all move together in this mode */
	  if (ncp->cursor_visible)
	    newf = ncp->cursor;
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
	      if ((loc > ap->x0) && (loc < ap->x1) && (ap->x1 > ap->x0))
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
      ap->sx = (Float)(ap->x0 - ap->xmin) / (Float)ap->x_ambit;
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
  int samps;
  samps = current_ed_samples(cp);
  ap = cp->axis;
  if ((amount >= 1.0) || ((samps > 0) && (ap->zx > (1.0 / (double)samps))))
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
  Float off;
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

void set_axes(chan_info *cp, Float x0, Float x1, Float y0, Float y1)
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



/* ---------------- CHANNEL GRAPHICS ---------------- */


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

static void display_zero (chan_info *cp)
{
  axis_info *ap;
  Locus zero;
  ap = cp->axis;
  if ((ap->y0 < 0.0) && (ap->y1 > 0.0))
    {
      zero = local_grf_y(0.0, ap);
      draw_line(copy_context(cp), ap->x_axis_x0, zero, ap->x_axis_x1, zero);
      if (cp->printing) 
	ps_draw_line(cp, ap->x_axis_x0, 0, ap->x_axis_x1, 0);
    }
}

static char chn_id_str[LABEL_BUFFER_SIZE];

static void display_channel_id (chan_info *cp, int height, int chans)
{
  int x0, y0;
  if ((chans > 1) || (cp->edit_ctr > 0))
    {
      set_peak_numbers_font(cp);
      if (cp->printing) ps_set_peak_numbers_font(cp);
      x0 = 5;
      y0 = height - 5;
      if (cp == selected_channel(cp->state))
	{
	  if (chans > 1)
	    {
	      if (cp->edit_ctr == 0)
		mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%s%d]", 
			STR_channel_id, (cp->chan + 1));                    /* cp chan numbers are 0 based to index sp->chans array */
	      else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%s%d: (%d)]", 
			   STR_channel_id, (cp->chan + 1), cp->edit_ctr);
	    }
	  else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "[%d]", cp->edit_ctr);
	}
      else
	{
	  if (chans > 1)
	    {
	      if (cp->edit_ctr == 0)
		mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "%s%d", 
			STR_channel_id, (cp->chan + 1));
	      else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "%s%d:(%d)", 
			   STR_channel_id, (cp->chan + 1), cp->edit_ctr);
	    }
	  else mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "(%d)", cp->edit_ctr);
	}
      draw_string(copy_context(cp), x0, y0, chn_id_str, strlen(chn_id_str));
      if (cp->printing) 
	ps_draw_string(cp, x0, y0, chn_id_str);
    }
}

static void display_selection_fft_size (chan_info *cp, axis_info *fap)
{
  int x0, y0;
  if (fap->height < 60) return;
  set_tiny_numbers_font(cp);
  if (cp->printing) ps_set_tiny_numbers_font(cp);
  y0 = fap->height + fap->y_offset - 3;
  x0 = fap->x_axis_x0 + 10;
  mus_snprintf(chn_id_str, LABEL_BUFFER_SIZE, "(len: %d/%d)", selection_len(), cp->selection_transform_size);
  draw_string(copy_context(cp), x0, y0, chn_id_str, strlen(chn_id_str));
  if (cp->printing) ps_draw_string(cp, x0, y0, chn_id_str);
}

static void make_wavogram(chan_info *cp, snd_info *sp, snd_state *ss);
static axis_context *cursor_context(chan_info *cp);
static axis_context *combined_context(chan_info *cp);

int make_graph(chan_info *cp, snd_info *sp, snd_state *ss)
{
  /* axes are already set, determining the data we will show -- do we need explicit clipping ? */
  int i = 0, j = 0, samps;
  Locus xi;
  axis_info *ap;
  Float samples_per_pixel, xf, pinc = 0.0;
  double x, incr;  
  /* in long files with small windows we can run into floating point errors that accumulate
   * in the loop (incrementing by a truncated amount each time), making the x amounts smaller
   * than they should be (so the graph appears squeezed).
   *
   * There is a similar problem with exceedingly long files (say 1 hour at 44KHz), in which 
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
  Float samp, ymin, ymax;
  int pixels, grfpts;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double start_time = 0.0, cur_srate = 1.0;
  axis_context *ax = NULL;
  chan_context *cgx;
  env_info *ep;
  ap = cp->axis;
  /* check for no graph */
  if ((!ap) || (!(ap->graph_active)) || (ap->x0 == ap->x1)) return(0);
  if (cp->wavo) 
    {
      make_wavogram(cp, sp, ss); 
      return(0);
    }
  if (sp)
    {
      cur_srate = (double)SND_SRATE(sp);
      ap->losamp = (int)(ceil(ap->x0 * cur_srate));
      if (ap->losamp < 0) ap->losamp = 0;
      start_time = (double)(ap->losamp) / cur_srate;
      ap->hisamp = (int)(ap->x1 * cur_srate);
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
  else samples_per_pixel = (Float)(samps - 1) / (Float)pixels;
  allocate_grf_points();
  if (cp->printing) ps_allocate_grf_points();
  if (sp)
    {
      if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
	{
	  ax = combined_context(cp); 
	  if (cp->printing) ps_recolor(cp);
	}
      else ax = copy_context(cp);
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
      grfpts = ap->hisamp - ap->losamp + 1;
      for (j = 0, x = ((double)(ap->losamp) / cur_srate); j < grfpts; j++, x += incr)
	{
	  samp = next_sample_to_float(sf);
	  set_grf_point(local_grf_x(x, ap), 
			j, 
			local_grf_y(samp, ap));
	  if (cp->printing) 
	    ps_set_grf_point(x, j, samp);
	}
      if (sp)
	{
	  draw_grf_points(cp, ax, j, ap, 0.0, MAIN_GRAPH_STYLE(cp));
	  if (cp->printing) 
	    ps_draw_grf_points(cp, ap, j, 0.0, MAIN_GRAPH_STYLE(cp));
	}
    }
  else
    {
      /* take min, max */
      if (amp_env_usable(cp, samples_per_pixel, ap->hisamp,TRUE, cp->edit_ctr)) /* true = start new background amp env process if needed */
	j = amp_env_graph(cp, ap, samples_per_pixel, (sp) ? ((int)SND_SRATE(sp)) : 1);
      else
	{
	  if ((ap->hisamp - ap->losamp) > (current_ed_samples(cp)/4))
	    {                                /* we're trying to view a large portion of the (large) sound */
	      cgx = cp->cgx;
	      if (cgx->amp_env_in_progress)
		{                            /* but the amp-env background process is still working on it */
		  ep = cp->amp_envs[cp->edit_ctr];
		  if ((ep) && samples_per_pixel >= (Float)(ep->samps_per_bin))
		    {                        /* and it will be useful when it finishes */
		      cp->waiting_to_make_graph = 1;
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
	  ymin = 100.0;
	  ymax = -100.0;
	  if (cp->printing) pinc = samples_per_pixel/cur_srate;
	  for (i = ap->losamp, xf = 0.0; i <= ap->hisamp; i++)
	    {
	      samp = next_sample_to_float(sf);
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
		  ymin = 100.0;
		  ymax = -100.0;
		}
	    }
	}
      if (sp)
	{
	  draw_both_grf_points(cp, ax, j, MAIN_GRAPH_STYLE(cp));
	  if (cp->printing) 
	    ps_draw_both_grf_points(cp, ap, j, MAIN_GRAPH_STYLE(cp));
	}
    }
  if (sf) {free_snd_fd(sf); sf = NULL;}
  if ((sp) && (sp->channel_style == CHANNELS_SUPERIMPOSED))
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing)
	ps_reset_color(cp);
    }
  return(j);
}


/* these two procedures split "make_graph" into two pieces; the first
 *   gets the data to be graphed, using the amp envs and so on, and
 *   the second displays it.  (The first cp+pos may have no relation
 *   to the second cp -- allow arbitrary overlapping etc).
 */

#include "vct.h"

SCM make_graph_data(chan_info *cp, int edit_pos, int losamp, int hisamp)
{
  int i, j = 0, samps;
  axis_info *ap;
  snd_info *sp;
  Float samples_per_pixel, xf;
  Float samp, ymin, ymax;
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double start_time = 0.0, cur_srate = 1.0;
  Float *data = NULL, *data1 = NULL;
  int data_size = 0;
  ap = cp->axis;
  sp = cp->sound;
  cur_srate = (double)SND_SRATE(sp);
  if (losamp == -1)
    losamp = (int)(ceil(ap->x0 * cur_srate));
  start_time = (double)(losamp) / cur_srate;
  if (hisamp < 0)
    hisamp = (int)(ap->x1 * cur_srate);

  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = hisamp - losamp + 1;
  if ((x_start == x_end) && (samps > 10)) return(SCM_BOOL_F);
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01;
  else samples_per_pixel = (Float)(samps - 1) / (Float)pixels;
  if ((samples_per_pixel < 1.0) ||
      ((samples_per_pixel < 5.0) && 
       (samps < POINT_BUFFER_SIZE)))
    {
      data_size = samps;
      sf = init_sample_read_any(losamp, cp, READ_FORWARD, edit_pos);
      if (sf == NULL) return(SCM_BOOL_F); /* should this throw an error? (CHANNEL_BEING_DEALLOCATED) */
      data = (Float *)MALLOC(data_size * sizeof(Float));
      for (i = 0; i < data_size; i++)
	data[i] = next_sample_to_float(sf);
    }
  else
    {
      if (amp_env_usable(cp, samples_per_pixel, hisamp, FALSE, edit_pos)) 
	{
	  Float step, xk;
	  MUS_SAMPLE_TYPE ymin, ymax;
	  int k, kk;
	  env_info *ep;

	  data_size = pixels + 1;
	  data = (Float *)CALLOC(data_size, sizeof(Float));
	  data1 = (Float *)CALLOC(data_size, sizeof(Float));

	  ep = cp->amp_envs[edit_pos];
	  step = samples_per_pixel / (Float)(ep->samps_per_bin);
	  xf = (Float)(losamp) / (Float)(ep->samps_per_bin);
	  j = 0;
	  i = losamp;
	  xk = i;
	  ymin = ep->fmax;
	  ymax = ep->fmin;
	  while (i <= hisamp)
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
	      i = (int)xk;
	      data[j] = MUS_SAMPLE_TO_FLOAT(ymin);
	      data1[j] = MUS_SAMPLE_TO_FLOAT(ymax);
	      j++;
	      ymin = ep->fmax;
	      ymax = ep->fmin;
	    }
	}
      else
	{
	  data_size = pixels + 1;
	  sf = init_sample_read_any(losamp, cp, READ_FORWARD, edit_pos);
	  if (sf == NULL) return(SCM_BOOL_F);
	  data = (Float *)CALLOC(data_size, sizeof(Float));
	  data1 = (Float *)CALLOC(data_size, sizeof(Float));
	  j = 0;      /* graph point counter */
	  ymin = 100.0;
	  ymax = -100.0;
	  for (i = losamp, xf = 0.0; i <= hisamp; i++)
	    {
	      samp = next_sample_to_float(sf);
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
    return(SCM_LIST2(make_vct(data_size, data),
		     make_vct(data_size, data1)));
  else return(make_vct(data_size, data));
}

void draw_graph_data(chan_info *cp, int losamp, int hisamp, int data_size, 
		     Float *data, Float *data1, axis_context *ax, int style)
{
  int i, samps;
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
	losamp = (int)(ceil(ap->x0 * cur_srate));
      start_time = (double)(losamp) / cur_srate;
      if (hisamp == -1)
	hisamp = (int)(ap->x1 * cur_srate);
      samps = hisamp - losamp + 1;
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

static Float cp_dB(chan_info *cp, Float py)
{
  return((py <= cp->lin_dB) ? cp->min_dB : (20.0 * (log10(py))));
}

static void display_peaks(chan_info *cp, axis_info *fap, Float *data, int scaler, int samps, Float samps_per_pixel, int fft_data, Float fft_scale)
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
  peak_freqs = (fft_peak *)CALLOC(cp->max_fft_peaks, sizeof(fft_peak));
  peak_amps = (fft_peak *)CALLOC(cp->max_fft_peaks, sizeof(fft_peak));
  if (num_peaks > cp->max_fft_peaks) num_peaks = cp->max_fft_peaks;
  if (fft_data)
    num_peaks = find_and_sort_fft_peaks(data, peak_freqs, num_peaks, samps, 1, samps_per_pixel, fft_scale); /* srate 1.0=>freqs between 0 and 1.0 */
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
	  (cp->normalize_fft == DONT_NORMALIZE))
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
      if (cp->printing) ps_set_bold_peak_numbers_font(cp);
      row = fap->y_axis_y1 + 15;
      for (i = 0; i < num_peaks; i++)
	{
	  if (peak_freqs[i].amp >= amp0)
	    {
	      px = peak_freqs[i].freq;
	      fstr = prettyf(px * scaler, tens);
	      draw_string(ax, col, row, fstr, strlen(fstr));
	      if (cp->printing) ps_draw_string(cp, col, row, fstr);
	      FREE(fstr);
	      fstr = NULL;
	      if (with_amps)
		{
		  if ((fft_data) && (cp->fft_log_magnitude))
		    mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.1f", cp_dB(cp, peak_freqs[i].amp));
		  else mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.*f", acols, peak_freqs[i].amp);
		  draw_string(ax, acol, row, ampstr, strlen(ampstr));
		  if (cp->printing) 
		    ps_draw_string(cp, acol, row, ampstr);
		}
	    }
	  row += 15;
	}
    }
  else amp0 = 100.0;
  set_peak_numbers_font(cp);
  if (cp->printing) ps_set_peak_numbers_font(cp);
  /* choose a small font for these numbers */
  row = fap->y_axis_y1 + 15;
  for (i = 0; i < num_peaks; i++)
    {
      if (peak_freqs[i].amp < amp0)
	{
	  px = peak_freqs[i].freq;
	  fstr = prettyf(px * scaler, tens);
	  draw_string(ax, col, row, fstr, strlen(fstr));
	  if (cp->printing) ps_draw_string(cp, col, row, fstr);
	  FREE(fstr);
	  fstr = NULL;
	  if (with_amps)
	    {
	      if ((fft_data) && (cp->fft_log_magnitude))
		mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.1f", cp_dB(cp, peak_freqs[i].amp));
	      else mus_snprintf(ampstr, LABEL_BUFFER_SIZE, "%.*f", acols, peak_freqs[i].amp);
	      draw_string(ax, acol, row, ampstr, strlen(ampstr));
	      if (cp->printing) 
		ps_draw_string(cp, acol, row, ampstr);
	    }
	}
      row += 15;
    }
  if (peak_freqs) FREE(peak_freqs); 
  if (peak_amps) FREE(peak_amps);
}

static void make_fft_graph(chan_info *cp, snd_info *sp, snd_state *ss)
{
  /* axes are already set, data is in the fft_info struct -- don't reset here! */
  /* since the fft size menu callback can occur while we are calculating the next fft, we have to lock the current size until the graph goes out */
  fft_info *fp;
  axis_info *fap;
  axis_context *ax;
  Float *data;
  Float incr, x, scale;
  int i, j, hisamp, losamp = 0;
  Float samples_per_pixel, xf, ina, ymax, scaler;
  Locus logx, logy;
  Float pslogx, pslogy;
  fp = cp->fft;
  if (chan_fft_in_progress(cp)) return;
  fap = fp->axis;
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
  allocate_grf_points();
  if (cp->printing) ps_allocate_grf_points();
  samples_per_pixel = (Float)(hisamp - losamp) / (Float)(fap->x_axis_x1 - fap->x_axis_x0);
  if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
    {
      ax = combined_context(cp); 
      if (cp->printing) ps_recolor(cp);
    }
  else ax = copy_context(cp);
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
	      incr = ymax / (Float)(hisamp - losamp);
	      scaler = 1.0 / log(ymax + 1.0);
	    }
	  else scaler = 0.0;
	  for (i = losamp, x = fap->x0; i < hisamp; i++, x += incr)
	    {
	      if (cp->fft_log_frequency) 
		logx = local_grf_x(log(x + 1.0) * scaler, fap); 
	      else logx = local_grf_x(x, fap);
	      if (cp->fft_log_magnitude) 
		logy = local_grf_y(cp_dB(cp, data[i] * scale), fap); 
	      else logy = local_grf_y(data[i] * scale, fap);
	      set_grf_point(logx, i - losamp, logy);
	      if (cp->printing) 
		{
		  if (cp->fft_log_frequency) pslogx = log(x + 1.0) * scaler; else pslogx = x;
		  if (cp->fft_log_magnitude) pslogy = cp_dB(cp, data[i] * scale); else pslogy = data[i] * scale;
		  ps_set_grf_point(pslogx, i - losamp, pslogy);
		}
	    }
	}
      draw_grf_points(cp, ax, i - losamp, fap, 0.0, FFT_GRAPH_STYLE(cp));
      if (cp->printing) 
	ps_draw_grf_points(cp, fap, i - losamp, 0.0, FFT_GRAPH_STYLE(cp));
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
	  incr = ymax / (Float)(hisamp - losamp);
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
		    logy = local_grf_y(cp_dB(cp, ymax * scale), fap); 
		  else logy = local_grf_y(ymax * scale, fap);
		  set_grf_point(logx, j, logy);
		  if (cp->printing) 
		    {
		      if (cp->fft_log_frequency) 
			pslogx = log(x + 1.0) * scaler; 
		      else pslogx = x;
		      if (cp->fft_log_magnitude) 
			pslogy = cp_dB(cp, ymax * scale); 
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
      draw_grf_points(cp, ax, j, fap, 0.0, FFT_GRAPH_STYLE(cp));
      if (cp->printing) 
	ps_draw_grf_points(cp, fap, j, 0.0, FFT_GRAPH_STYLE(cp));
    }
  if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
    {
      copy_context(cp); /* reset for axes etc */
      if (cp->printing) ps_reset_color(cp);
    }
  if (cp->show_fft_peaks) 
    {
      if (cp->transform_type == FOURIER)
	display_peaks(cp, fap, data, 
		      (int)(SND_SRATE(sp) * cp->spectro_cutoff / 2), 
		      hisamp, samples_per_pixel, 1, scale);
      else display_peaks(cp, fap, data, 
			 (int)(fp->current_size * cp->spectro_cutoff), 
			 hisamp, samples_per_pixel, 1, 0.0);
    }
  if (cp->selection_transform_size != 0) display_selection_fft_size(cp, fap);
  if (cp->hookable) after_fft(ss, cp, scale);
}

static int display_fft_peaks(chan_info *ucp, char *filename)
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
      fd = fopen(mcf = mus_expand_filename(filename), "w");
      if (mcf) FREE(mcf);
      if (fd == NULL) 
	{
	  report_in_minibuffer_and_save(sp, "can't write %s: %s", filename, strerror(errno));
	  err = 1;
	}
      else tmp_file = 0;
    }
  if (tmp_file == 1)
    {
      filename = snd_tempnam(ss);
      fd = fopen(filename, "w");
    }
  if (fd) 
    {
#if HAVE_STRFTIME
      timbuf = (char *)CALLOC(TIME_STR_SIZE, sizeof(char));
      time(&ts);
      strftime(timbuf, TIME_STR_SIZE, STRFTIME_FORMAT, localtime(&ts));
      fprintf(fd, "Snd: fft peaks (%s)\n\n", timbuf);
      FREE(timbuf);
#else
      fprintf(fd, "Snd: fft peaks (%s)\n\n");
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
		  peak_freqs = (fft_peak *)CALLOC(cp->max_fft_peaks, sizeof(fft_peak));
		  peak_amps = (fft_peak *)CALLOC(cp->max_fft_peaks, sizeof(fft_peak));
		  num_peaks = find_and_sort_fft_peaks(data, peak_freqs, cp->max_fft_peaks, samps, 1, samples_per_pixel, fp->scale);
		  if ((num_peaks != 1) || 
		      (peak_freqs[0].freq != 0.0))
		    {
		      fprintf(fd, sp->shortname);
		      if (sp->nchans > 1) fprintf(fd, ": chan %d", cp->chan);
		      fprintf(fd, ", fft %d points beginning at sample %d (%.3f secs)\n\n",
			      fp->current_size, 
			      ap->losamp, 
			      (Float)(ap->losamp) / (Float)srate);
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
      if (fclose(fd) != 0)
	report_in_minibuffer_and_save(sp, "can't close %s: %s", filename, strerror(errno));
      if (tmp_file)
	{
	  fd = fopen(filename, "r");
	  fseek(fd, 0, SEEK_END);
	  chars = ftell(fd);
	  rewind(fd);
	  str = (char *)CALLOC(chars + 1, sizeof(char));
	  fread(str, 1, chars, fd);
	  fclose(fd);
	  snd_help(ss, "fft peaks", str);
	  FREE(str);
	  if (remove(filename) == -1)
	    snd_error("can't remove %s: %s", filename, strerror(errno));
	  FREE(filename);
	}
    }
  if (si) si = free_sync_info(si);
  return(err);
}

static Float skew_color(Float x, Float base)
{
  if ((base <= 0.0) || (base == 1.0)) return(x);
  return((pow(base, x) - 1.0) / (base - 1.0));
}

static int js[GRAY_SCALES];

static void make_sonogram(chan_info *cp, snd_info *sp, snd_state *ss)
{ /* colormap allocated in snd-fft via allocate_sono_rects in snd-xchn */
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
      allocate_grf_points();
      if (cp->printing) ps_allocate_grf_points();
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
      for (slice = 0; slice < si->active_slices; slice++, xf += xfincr)
	{
	  memset((void *)js, 0, GRAY_SCALES * sizeof(int));
	  fdata = si->data[slice];
	  for (i = 0; i < bins; i++)
	    {
	      /* above is fdata[i-1], left is si->data[slice-1][i] */
	      binval = fdata[i] / scl;
	      if (cp->fft_log_magnitude) binval = 1.0 - (cp_dB(cp, binval)) / cp->min_dB;
	      if (binval >= color_cutoff(ss))
		{
		  if (color_inverted(ss)) 
		    j = (int)(skew_color((1.0 - binval), color_scale(ss)) * GRAY_SCALES); 
		  else j = (int)(skew_color(binval, color_scale(ss)) * GRAY_SCALES);
		  if (j > 0) j--; else j = 0;
		  if (cp->fft_log_frequency)
		    set_sono_rectangle(js[j], j, (Locus)xf, hidata[i + 1], rectw, hidata[i] - hidata[i + 1]);
		  else set_sono_rectangle(js[j], j, (Locus)xf, hidata[i + 1], rectw, recth);
		  if (cp->printing)
		    {
		      if (cp->fft_log_frequency) 
			ps_draw_sono_rectangle(cp, fap, j, fap->x0 + xscl * slice, hfdata[i + 1], frectw, hfdata[i] - hfdata[i + 1]);
		      else ps_draw_sono_rectangle(cp, fap, j, fap->x0 + xscl * slice, hfdata[i + 1], frectw, frecth);
		    }
		  js[j]++;
		}
	    }
	  for (i = 0; i < GRAY_SCALES; i++)
	    if (js[i] > 0) 
	      draw_sono_rectangles(ax, i, js[i]);
	  if (cp->printing)
	    {
	      check_for_event(ss);
	      if ((ss->stopped_explicitly) || (!(cp->active))) /* user closed file while trying to print */
		{
		  ss->stopped_explicitly = 0;
		  report_in_minibuffer(sp, "stopped");
		  break;
		}
	    }
	}
      if (cp->printing) ps_reset_color(cp);
      FREE(hfdata);
      FREE(hidata);
      if (cp->hookable) after_fft(ss, cp, 1.0/scl);
    }
}

static void rotate_matrix(Float xangle, Float yangle, Float zangle, Float xscl, Float yscl, Float zscl, Float *mat)
{
  /* return rotation matrix for rotation through angles xangle, yangle, then zangle with scaling by xscl, yscl, zscl */
  Float sinx, siny, sinz, cosx, cosy, cosz, deg;
  Float x, y, z;
  deg = TWO_PI / 360.0;
  /* might be nice to use sincosf here, but it segfaults in GnuC */
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

void reset_spectro(snd_state *state)
{
  set_spectro_cutoff(state, DEFAULT_SPECTRO_CUTOFF);
  set_spectro_hop(state, DEFAULT_SPECTRO_HOP);
  set_spectro_x_scale(state, DEFAULT_SPECTRO_X_SCALE);
  set_spectro_y_scale(state, DEFAULT_SPECTRO_Y_SCALE);
  set_spectro_z_scale(state, DEFAULT_SPECTRO_Z_SCALE);
  set_spectro_z_angle(state, DEFAULT_SPECTRO_Z_ANGLE);
  set_spectro_x_angle(state, DEFAULT_SPECTRO_X_ANGLE);
  set_spectro_y_angle(state, DEFAULT_SPECTRO_Y_ANGLE);
  set_color_map(state, -1);
  set_color_cutoff(state, DEFAULT_COLOR_CUTOFF);
  set_color_scale(state, DEFAULT_COLOR_SCALE);
  set_color_inverted(state, DEFAULT_COLOR_INVERTED);
  set_wavo_hop(state, DEFAULT_WAVO_HOP);
  set_wavo_trace(state, DEFAULT_WAVO_TRACE);
}

static void make_spectrogram(chan_info *cp, snd_info *sp, snd_state *ss)
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
  int bins, slice, i, j, xx, yy;
  if (chan_fft_in_progress(cp)) return;
  si = (sono_info *)(cp->sonogram_data);
  if ((si) && (si->scale > 0.0))
    {
      allocate_grf_points();
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
	zscl = -(cp->spectro_z_scale * fheight/scl);
      else zscl = -(cp->spectro_z_scale * fheight);
      rotate_matrix(cp->spectro_x_angle, cp->spectro_y_angle, cp->spectro_z_angle,
		    cp->spectro_x_scale, cp->spectro_y_scale, zscl,
		    matrix);
      ax = copy_context(cp);
      if (color_map(ss) == -1)
	{
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
		      xyz[2] = 1.0 - (cp_dB(cp, binval)) / cp->min_dB;
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
	      draw_grf_points(cp, ax, bins, fap, 0.0, FFT_GRAPH_STYLE(cp));
	      if (cp->printing) 
		{
		  ps_draw_grf_points(cp, fap, bins, 0.0, FFT_GRAPH_STYLE(cp));
		  check_for_event(ss);
		  if ((ss->stopped_explicitly) || (!(cp->active)))
		    {
		      ss->stopped_explicitly = 0;
		      report_in_minibuffer(sp, "stopped");
		      break;
		    }
		}
	    }
	}
      else
	{
	  /* spectrogram in various colors */
	  allocate_color_map(ss, color_map(ss));
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
		      xyz[2] = 1.0 - (cp_dB(cp, binval)) / cp->min_dB; 
		      binval = xyz[2];
		    }
		  rotate(xyz, matrix);
		  yval = xyz[1] + xyz[2];
		  xval = xyz[0];
		  if (binval >= color_cutoff(ss))
		    {
		      if (color_inverted(ss)) 
			j = (int)(skew_color((1.0 - binval), color_scale(ss)) * GRAY_SCALES); 
		      else j = (int)(skew_color(binval, color_scale(ss)) * GRAY_SCALES);
		      if (j > 0) j--;
		      if (j >= GRAY_SCALES) j = GRAY_SCALES - 1;
		      draw_spectro_line(ax, j, xx, yy, 
					(int)(xval + x0), 
					(int)(yval + y0));
		      if (cp->printing) 
			ps_draw_spectro_line(cp, j, xx, yy, xval + x0, yval + y0);
		    }
		  xx = (int)(xval + x0); 
		  yy = (int)(yval + y0);
		}
	      if (cp->printing) 
		{
		  check_for_event(ss);
		  if ((ss->stopped_explicitly) || (!(cp->active)))
		    {
		      ss->stopped_explicitly = 0;
		      report_in_minibuffer(sp, "stopped");
		      break;
		    }
		}
	    }
	  if (cp->printing) ps_reset_color(cp);
	}
      if (cp->hookable) after_fft(ss, cp, 1.0 / scl);
    }
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
  if (sp) ap->losamp = (int)(ap->x0 * SND_SRATE(sp));
  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
  if (sf == NULL) return;
  allocate_grf_points();
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
  if (color_map(ss) == -1)
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
	      xyz[2] = next_sample_to_float(sf);
	      rotate(xyz, matrix);
	      yval = xyz[1] + xyz[2];
	      xval = xyz[0];
	      set_grf_point((Locus)(xval + x0), i, 
			    (Locus)(yval + y0));
	      if (cp->printing) 
		ps_set_grf_point(ungrf_x(ap, (int)(xval + x0)), i, 
				 ungrf_y(ap, (int)(y0 + yval)));
	    }
	  draw_grf_points(cp, ax, cp->wavo_trace, ap, 0.0, MAIN_GRAPH_STYLE(cp));
	  if (cp->printing) 
	    ps_draw_grf_points(cp, ap, cp->wavo_trace, 0.0, MAIN_GRAPH_STYLE(cp));
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
	      binval = next_sample_to_float(sf);
	      xyz[0] = x - x0; 
	      xyz[1] = y - y0; 
	      xyz[2] = binval;
	      rotate(xyz, matrix);
	      yval = xyz[1] + xyz[2];
	      xval = xyz[0];
	      /* for color decision here we need absolute value of data */
	      if (binval < 0.0) binval = -binval;
	      if ((binval >= color_cutoff(ss)) && (xx != -1))
		{
		  if (color_inverted(ss)) 
		    j = (int)(skew_color((1.0 - binval), color_scale(ss)) * GRAY_SCALES); 
		  else j = (int)(skew_color(binval, color_scale(ss)) * GRAY_SCALES);
		  if (j > 0) j--;
		  if (j >= GRAY_SCALES) j = GRAY_SCALES - 1;
		  draw_spectro_line(ax, j, xx, yy, 
				    (int)(xval + x0), 
				    (int)(yval + y0));
		  if (cp->printing) 
		    ps_draw_spectro_line(cp, j, xx, yy, xval + x0, yval + y0);
		}
	      xx = (int)(xval + x0); 
	      yy = (int)(yval + y0);
	    }
	}
      if (cp->printing) ps_reset_color(cp);
    }
  free_snd_fd(sf);
}

static void make_lisp_graph(chan_info *cp, snd_info *sp, snd_state *ss)
{
  lisp_grf *up;
  /* data can be evenly spaced data or an envelope (up->env_data) */
  axis_info *uap = NULL;
  int i, j, grf_len, graph;
  axis_context *ax;
  COLOR_TYPE old_color = 0;
  Float x, y, samples_per_pixel = 1.0, xinc, start_x, xf, ymin, ymax, pinc;
  int x0, x1, y0, y1;
  Locus xi;
  up = (lisp_grf *)(cp->lisp_info);
  if (up) uap = up->axis; else return;
  if ((!uap) || (!uap->graph_active) || (up->len == NULL) || (up->len[0] <= 0)) return;
  allocate_grf_points();
  if (cp->printing) ps_allocate_grf_points();
  if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
    {
      ax = combined_context(cp); 
      if (cp->printing) ps_recolor(cp);
    }
  else ax = copy_context(cp);
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
      if (up->graphs > 1) 
	old_color = get_foreground_color(cp, ax);
      for (graph = 0; graph < up->graphs; graph++)
	{
	  /* check for up->len[graph] > pixels available and use ymin ymax if needed */
	  /* TODO: data-color-hook: provide a way to turn this (lisp graph color) off */
	  /*         (lambda (snd chn grf overlay-num)) */
	  /*  would need to save/restore old_color after copy_context */
	  switch (graph)
	    {
	    case 0:  break;
	    case 1:  set_foreground_color(cp, ax, (ss->sgx)->red);        break;
	    case 2:  set_foreground_color(cp, ax, (ss->sgx)->green);      break;
	    case 3:  set_foreground_color(cp, ax, (ss->sgx)->light_blue); break;
	    case 4:  set_foreground_color(cp, ax, (ss->sgx)->yellow);     break;
	    default: set_foreground_color(cp, ax, (ss->sgx)->black);      break;
	    }
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
	      draw_grf_points(cp, ax, grf_len, uap, 0.0, LISP_GRAPH_STYLE(cp));
	      if (cp->printing) 
		ps_draw_grf_points(cp, uap, grf_len, 0.0, LISP_GRAPH_STYLE(cp));
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
	      draw_both_grf_points(cp, ax, j, LISP_GRAPH_STYLE(cp));
	      if (cp->printing) 
		ps_draw_both_grf_points(cp, uap, j, LISP_GRAPH_STYLE(cp));
	    }
	}
      if (up->graphs > 1) set_foreground_color(cp, ax, old_color);
      if (sp->channel_style == CHANNELS_SUPERIMPOSED) 
	{
	  copy_context(cp); /* reset for axes etc */
	  if (cp->printing) ps_reset_color(cp);
	}
      if (cp->show_fft_peaks) 
	display_peaks(cp, uap, up->data[0], 1, up->len[0] - 1, samples_per_pixel, 0, 0.0);
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
      ax->ss = ss;
    }
  else ax = ap->ax;
  sp = cp->sound;
  setup_axis_context(cp, ax);
  if (erase_first)
    erase_rectangle(cp, ap->ax, ap->graph_x0, ap->y_offset, ap->width, ap->height); 
  make_axes_1(cp, ap, x_style, SND_SRATE(sp));
}

static void draw_graph_cursor(chan_info *cp);

static void display_channel_data_with_size (chan_info *cp, snd_info *sp, snd_state *ss, 
					    int width, int height, int offset, 
					    int just_fft, int just_lisp)
{
  int with_fft, with_lisp, displays, points;
  axis_info *ap = NULL;
  axis_info *fap = NULL;
  axis_info *uap = NULL;
  fft_info *fp = NULL;
  lisp_grf *up = NULL;
  SCM res = SCM_BOOL_F;
  if ((cp->hookable) && 
      (!(ss->graph_hook_active)) &&
      (HOOKED(graph_hook)))
    {
      ss->graph_hook_active = 1;
      res = g_c_run_progn_hook(graph_hook,
			       SCM_LIST4(TO_SMALL_SCM_INT((cp->sound)->index),
					 TO_SMALL_SCM_INT(cp->chan),
					 TO_SCM_DOUBLE((cp->axis)->y0),
					 TO_SCM_DOUBLE((cp->axis)->y1)),
			       S_graph_hook);
      /* (add-hook! graph-hook (lambda (a b c d) (snd-print (format #f "~A ~A ~A ~A" a b c d)))) */
      ss->graph_hook_active = 0;
      if (TRUE_P(res)) return;
    }
  ap = cp->axis;
  if (ap == NULL) return;
  /* now check for fft/wave/user-function decisions */
  ap->height = height;
  ap->window_width = width;
  ap->width = width;
  ap->y_offset = offset;
  if (cp->waving) displays = 1; else displays = 0;
  if (cp->lisp_graphing) displays++;
  up = (lisp_grf *)(cp->lisp_info);

  if ((up == NULL) && 
      (HOOKED(lisp_graph_hook)))
    {
      /* this should only happen the first time such a graph is needed */
      cp->lisp_info = (lisp_grf *)CALLOC(1, sizeof(lisp_grf));
      up = (lisp_grf *)(cp->lisp_info);
      up->axis = make_axis_info(cp, 0.0, 1.0, -1.0, 1.0, "dummy axis", 0.0, 1.0, -1.0, 1.0, NULL);
      cp->lisp_graphing = 1;
    }
  if (up)
    {
      uap = up->axis;
      if (uap)
	{
	  with_lisp = 1;
	  uap->height = height;
	  uap->y_offset = offset;
	  uap->width = width;
	  uap->window_width = width;
	}
      else with_lisp = 0;
    }
  else with_lisp = 0;

  if (cp->ffting) displays++;
  fp = cp->fft;
  if (fp)
    {
      fap = fp->axis; 
      if (fap)
	{
	  with_fft = 1;
	  fap->height = height;
	  fap->y_offset = offset;
	  fap->width = width;
	  fap->window_width = width;
	}
      else with_fft = 0;
    }
  else with_fft = 0;

  if (displays == 0)
    {
      clear_window(ap->ax);
      return;
    }

  if (cp->graphs_horizontal)
    {
      if (cp->waving) ap->width = width / displays;
      if (with_fft) fap->width =  width / displays;
      if (with_lisp) uap->width = width / displays;

      /* now the x axis offsets for fap and uap */
      if (with_fft) 
	{
	  if (cp->waving) 
	    fap->graph_x0 = ap->width;
	  else fap->graph_x0 = 0;
	}
      if (with_lisp) 
	uap->graph_x0 = uap->width * (displays - 1);
    }
  else
    {
      height /= displays;
      if (cp->waving) ap->height = height;
      if (with_fft) fap->height =  height;
      if (with_lisp) uap->height = height;

      /* now the y axis offsets for fap and uap */
      if (with_fft)
	{
	  if (cp->waving) fap->y_offset += height; 
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

      if (cp->waving)
	{
	  if (cp->wavo)
	    {
	      if (ap->y_axis_y0 == ap->y_axis_y1) 
		make_axes(cp, ap, x_axis_style(ss), FALSE); /* first time needs setup */
	      ap->y0 = ap->x0;
	      ap->y1 = ap->y0 + (Float)(cp->wavo_trace * (ap->y_axis_y0 - ap->y_axis_y1)) / ((Float)(cp->wavo_hop) * SND_SRATE(sp));
	      ap->x1 = ap->x0 + (Float)(cp->wavo_trace) / (Float)SND_SRATE(sp);
	    }
	  make_axes(cp, ap,
		    x_axis_style(ss),
		    ((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)));
  
	  cp->cursor_visible = 0;
	  cp->selection_visible = 0;
	  points = make_graph(cp, sp, ss);
	  if (points == 0) return;
	  if ((cp->mixes) &&
	      (cp->mix_dragging)) 
	    mix_save_graph(ss, cp_to_mix_context(cp), points);
	  if (cp->cursor_on) draw_graph_cursor(cp);
	}
    }

  if (!just_lisp)
    {
      if (with_fft)
	{
	  make_axes(cp, fap,
		    (x_axis_style(ss) == X_IN_SAMPLES) ? X_IN_SECONDS : (x_axis_style(ss)),
#if USE_MOTIF
		    ((cp->chan == sp->nchans-1) || (sp->channel_style != CHANNELS_SUPERIMPOSED)));
	            /* Xt documentation says the most recently added work proc runs first, but we're
		     *   adding fft work procs in channel order, normally, so the channel background
		     *   clear for the superimposed case needs to happen on the highest-numbered 
		     *   channel, since it will complete its fft first.  It also needs to notice the
		     *   selected background, if any of the current sound's channels is selected.
		     */
#else
		    ((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)));
                    /* In Gtk+ (apparently) the first proc added is run, not the most recent */
#endif

        if ((!cp->waving) || (just_fft))
	    { /* make_graph does this -- sets losamp needed by fft to find its starting point */
	      ap->losamp = (int)(ap->x0 * (double)SND_SRATE(sp));
	      ap->hisamp = (int)(ap->x1 * (double)SND_SRATE(sp));
	    }
	  switch (cp->fft_style)
	    {
	    case NORMAL_FFT:  make_fft_graph(cp, sp, ss);                        break;
	    case SONOGRAM:    make_sonogram(cp, sp, ss);                         break;
	    case SPECTROGRAM: make_spectrogram(cp, sp, ss);                      break;
	    default:          snd_error("unknown fft style: %d", cp->fft_style); break;
	    }
	}
    }

  if (!just_fft)
    {
      if (with_lisp)
	{
	  if ((just_lisp) || ((!(cp->waving)) && (!(with_fft))))
	    {
	      ap->losamp = (int)(ap->x0 * (double)SND_SRATE(sp));
	      ap->hisamp = (int)(ap->x1 * (double)SND_SRATE(sp));
	    }
	  if ((cp->hookable) &&
	      (HOOKED(lisp_graph_hook)))
	    g_c_run_progn_hook(lisp_graph_hook,
			       SCM_LIST2(TO_SMALL_SCM_INT((cp->sound)->index),
					 TO_SMALL_SCM_INT(cp->chan)),
			       S_lisp_graph_hook);
	  if (up != (lisp_grf *)(cp->lisp_info))
	    up = (lisp_grf *)(cp->lisp_info);
	  if (uap != up->axis)
	    uap = up->axis;
	  /* if these were changed in the hook function, the old fields should have been saved across the change (g_graph in snd-scm.c) */
	  make_axes(cp, uap, /* this file l 2229 */
		    X_IN_LENGTH,
		    ((cp->chan == 0) || (sp->channel_style != CHANNELS_SUPERIMPOSED)));

	  make_lisp_graph(cp, sp, ss);
	}

      if (!just_lisp)
	{
	  if (cp->waving)
	    {
	      display_selection(cp);
	      if ((cp->marks) && (cp->show_marks)) display_channel_marks(cp);
	      if (cp->show_y_zero) display_zero(cp);
	      if ((cp->mixes)) display_channel_mixes(cp);
	    }
	  if ((sp->channel_style != CHANNELS_SUPERIMPOSED) && (height > 10))
	    display_channel_id(cp, height + offset, sp->nchans);
	  if ((cp->hookable) &&
	      (HOOKED(after_graph_hook)))
	    g_c_run_progn_hook(after_graph_hook,
			       SCM_LIST2(TO_SMALL_SCM_INT((cp->sound)->index),
					 TO_SMALL_SCM_INT(cp->chan)),
			       S_after_graph_hook);
	  /* (add-hook! after-graph-hook (lambda (a b) (snd-print (format #f "~A ~A" a b)))) */
	}
    } 
}

static void display_channel_data_1 (chan_info *cp, snd_info *sp, snd_state *ss, int just_fft, int just_lisp)
{
  int width, height, offset, full_height, chan_height, y0, y1, bottom, top;
  Float val, size;
  axis_info *ap;
  if ((sp->channel_style == CHANNELS_SEPARATE) || (sp->nchans == 1))
    {
      width = widget_width(channel_graph(cp));
      height = widget_height(channel_graph(cp));
      if ((height > 5) && (width > 5))
	display_channel_data_with_size(cp, sp, ss, width, height, 0, just_fft, just_lisp);
    }
  else
    {
      /* all chans in one graph widget, sy->scroll entire set, zy->zoom entire set etc */
      /* updates are asynchronous (dependent on background ffts etc), so we can't do the whole window at once */
      /* complication here is that we're growing down from the top, causing endless confusion */
      width = widget_width(channel_graph(sp->chans[0])) - (2 * ss->position_slider_width);
      height = widget_height(channel_graph(sp->chans[0]));
      cp->height = height;
      if (sp->channel_style == CHANNELS_SUPERIMPOSED)
	display_channel_data_with_size(cp, sp, ss, width, height, 0, just_fft, just_lisp);
      else
	{
	  val = gsy_value(sp->chans[0]);
	  size = gsy_size(sp->chans[0]);
	  full_height = (int)((Float)height / size);
	  chan_height = full_height / sp->nchans;
	  bottom = (int)(full_height * val);
	  top = (int)(full_height * (val + size));
	  y1 = (sp->nchans - cp->chan) * chan_height;
	  y0 = y1 - chan_height;
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
	    display_channel_data_with_size(cp, sp, ss, width, chan_height, offset, just_fft, just_lisp);
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
  display_channel_data_1(cp, sp, ss, TRUE, FALSE);
}

void display_channel_lisp_data(chan_info *cp, snd_info *sp, snd_state *ss)
{
  display_channel_data_1(cp, sp, ss, FALSE, TRUE);
}

void display_channel_data(chan_info *cp, snd_info *sp, snd_state *ss)
{
  display_channel_data_1(cp, sp, ss, FALSE, FALSE);
}


/* ---------------- CHANNEL CURSOR ---------------- */


static void draw_graph_cursor(chan_info *cp)
{
  axis_info *ap;
  axis_context *ax;
  ap = cp->axis;
  if ((cp->cursor < ap->losamp) || (cp->cursor > ap->hisamp)) return;
  ax = cursor_context(cp);
  if (cp->cursor_visible)
    {
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
	  CALL3(cp->cursor_proc,
		TO_SCM_INT(cp->sound->index),
		TO_SCM_INT(cp->chan),
		TO_SCM_INT(WAVE_AXIS_INFO),
		__FUNCTION__);
	  break;
	}
    }
  cp->cx = local_grf_x((double)(cp->cursor) / (double)SND_SRATE(cp->sound), ap); /* not float -- this matters in very long files (i.e. > 40 minutes) */
  cp->cy = local_grf_y(sample(cp->cursor, cp), ap);
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
      CALL3(cp->cursor_proc,
	    TO_SCM_INT(cp->sound->index),
	    TO_SCM_INT(cp->chan),
	    TO_SCM_INT(WAVE_AXIS_INFO),
	    __FUNCTION__);
      break;
    }
  cp->cursor_visible = 1;
}

int cursor_decision(chan_info *cp)
{
  int len;
  len = current_ed_samples(cp);
  if (cp->cursor >= len) cp->cursor = len - 1; /* zero based, but in 0-length files, len = 0 */
  if (cp->cursor < 0) cp->cursor = 0;        /* perhaps the cursor should be forced off in empty files? */
  if (cp->cursor < (cp->axis)->losamp)
    {
      if (cp->cursor == 0) return(CURSOR_ON_LEFT);
      else return(CURSOR_IN_MIDDLE);
    }
  if (cp->cursor > (cp->axis)->hisamp)
    {
      if (cp->cursor >= (len - 1)) return(CURSOR_ON_RIGHT);
      else return(CURSOR_IN_MIDDLE);
    }
  return(CURSOR_IN_VIEW);
}

void handle_cursor_with_sync(chan_info *cp, int decision)
{
  snd_info *sp;
  sync_info *si = NULL;
  int i;
  if (cp)
    {
      sp = cp->sound;
      if ((sp) && (sp->sync != 0))
	{
	  si = snd_sync(cp->state, sp->sync);
	  for (i = 0; i < si->chans; i++)
	    handle_cursor(si->cps[i], decision);
	  si = free_sync_info(si);
	}
      else handle_cursor(cp, decision);
    }
}

int cursor_moveto (chan_info *cp, int samp)
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
	  ncp->cursor = samp;
	  if (ncp != cp) 
	    handle_cursor(ncp, cursor_decision(ncp)); /* checks len */
	}
      si = free_sync_info(si);
    }
  else cp->cursor = samp;
  return(cursor_decision(cp));
}

int cursor_move (chan_info *cp, int samps)
{
  return(cursor_moveto(cp, cp->cursor + samps));
}


void show_cursor_info(chan_info *cp)
{
  char *expr_str;
  snd_info *sp;
  chan_info *ncp;
  Float y, absy;
  int digits, i, samp;
  char *s1, *s2;
  sp = cp->sound;
  if ((sp->sync != 0) && (cp->chan != 0)) return;
  samp = cp->cursor;
  y = sample(samp, cp);
  absy = fabs(y);
  if (absy < .0001) digits = 4;
  else if (absy<.001) digits = 3;
  else digits = 2;
  expr_str = (char *)CALLOC(PRINT_BUFFER_SIZE,sizeof(char));
  if (sp->nchans == 1)
    mus_snprintf(expr_str, PRINT_BUFFER_SIZE, "cursor at %s (sample %d) = %s",
	    s1 = prettyf((double)samp/(double)SND_SRATE(sp), 2),
	    samp,
	    s2 = prettyf(y, digits));
  else
    {
      if (sp->sync == 0)
	mus_snprintf(expr_str, PRINT_BUFFER_SIZE, "chan %d, cursor at %s (sample %d) = %s",
		cp->chan + 1,
		s1 = prettyf((double)samp/(double)SND_SRATE(sp), 2),
		samp,
		s2 = prettyf(y, digits));
      else
	{
	  /* in this case, assume we show all on chan 0 and ignore the call otherwise (see above) */
	  /* "cursor at..." then list of values */
	  mus_snprintf(expr_str, PRINT_BUFFER_SIZE, "cursor at %s (sample %d): %s",
		  s1 = prettyf((double)samp/(double)SND_SRATE(sp), 2),
		  samp,
		  s2 = prettyf(y, digits));
	  for (i = 1; i < sp->nchans; i++)
	    {
	      strcat(expr_str, ", ");
	      FREE(s2);
	      ncp = sp->chans[i];
	      y = sample(samp, ncp);
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
  sp->minibuffer_on = 1;
  FREE(expr_str);
  FREE(s1);
  FREE(s2);
}

void handle_cursor(chan_info *cp, int redisplay)
{
  axis_info *ap;
  axis_context *ax;
  snd_info *sp;
  Float gx = 0.0;
  if (cp == NULL) return;
  if ((redisplay != CURSOR_NO_ACTION) && (redisplay != KEYBOARD_NO_ACTION))
    {
      sp = cp->sound;
      if ((cp->verbose_cursor) && (sp->minibuffer_on == 0)) /* don't overwrite M-X results with cursor garbage! */
	{
	  show_cursor_info(cp); 
	  sp->minibuffer_on = 0;
	} 
      if (redisplay == CURSOR_UPDATE_DISPLAY)
	{
	  update_graph(cp, NULL);
	}
      else
	{
	  if (redisplay != CURSOR_IN_VIEW)
	    {
	      ap = cp->axis;
	      if (cp->cursor_visible)
		{
		  ax = cursor_context(cp);
		  draw_line(ax, cp->cx, cp->cy - cp->cursor_size, cp->cx, cp->cy + cp->cursor_size);
		  draw_line(ax, cp->cx - cp->cursor_size, cp->cy, cp->cx + cp->cursor_size, cp->cy);
		  cp->cursor_visible = 0; /* don't redraw at old location */
		}
	      switch (redisplay)
		{
		case CURSOR_ON_LEFT: 
		  gx = (double)(cp->cursor) / (double)SND_SRATE(sp); 
		  break;
		case CURSOR_ON_RIGHT: 
		  gx = (double)(cp->cursor) / (double)SND_SRATE(sp) - ap->zx * ap->x_ambit; 
		  break;
		case CURSOR_IN_MIDDLE: 
		  gx = (double)(cp->cursor) / (double)SND_SRATE(sp) - ap->zx * 0.5 * ap->x_ambit; 
		  break;
		}
	      if (gx < 0.0) gx = 0.0;
	      reset_x_display(cp, (gx - ap->xmin) / ap->x_ambit, ap->zx);
	    }
	  else {if (cp->cursor_on) draw_graph_cursor(cp);}
	}
    }
  update_possible_selection_in_progress(cp->cursor);
}

void goto_graph(chan_info *cp)
{
  snd_info *sp;
  if (cp)
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
  if (cp->waving)
    {
      ap = cp->axis;
      /* does (x, y) fall within the current axis bounds x_axis_x0|x1, y_axis_y0|y1 */
      if (((x0 <= ap->x_axis_x1) && (x1 >= ap->x_axis_x0)) && 
	  ((y0 <= ap->y_axis_y0) && (y1 >= ap->y_axis_y1)))
	return(WAVE);
    }
  if (cp->ffting)
    {
      fp = cp->fft;
      ap = fp->axis;
      /* look first for on-axis (axis drag) mouse */
      if (cp->fft_style != SONOGRAM)
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
  if ((cp->lisp_graphing) && (cp->lisp_info))
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
  if (x < ap->x_axis_x0) 
    x = ap->x_axis_x0; 
  else 
    if (x > ap->x_axis_x1) 
      x = ap->x_axis_x1;
  xf = ap->x0 + (ap->x1 - ap->x0) * (Float)(x - ap->x_axis_x0) / (Float)(ap->x_axis_x1 - ap->x_axis_x0);
  if (cp->fft_log_frequency)                                /* map axis x1 = 1.0 to srate/2 */
    xf = ((exp(xf * log(LOG_FACTOR + 1.0)) - 1.0) / LOG_FACTOR) * SND_SRATE(cp->sound) * 0.5 * cp->spectro_cutoff; 
  if (cp->fft_style == NORMAL_FFT)                          /* fp->data[bins] */
    {
      if (cp->transform_type == FOURIER)
	ind = (int)((fp->current_size * xf) / (Float)SND_SRATE(cp->sound));
      else ind = (int)xf;
      return(mus_format("(%.1f%s, transform val: %.3f%s (raw: %.3f)",
			xf,
			((cp->transform_type == AUTOCORRELATION) ? " samps" : " Hz"),
			(cp->fft_log_magnitude) ? cp_dB(cp, (fp->data[ind] * fp->scale)) : (fp->data[ind] * fp->scale),
			(cp->fft_log_magnitude) ? "dB" : "",
			fp->data[ind]));
    }
  else 
    {
      if (cp->fft_style == SONOGRAM) 	  /* si->data[slices][bins] */
	{
	  yf = ap->y0 + (ap->y1 - ap->y0) * (Float)(y - ap->y_axis_y0) / (Float)(ap->y_axis_y1 - ap->y_axis_y0);
	  si = (sono_info *)(cp->sonogram_data);
	  if (cp->transform_type == FOURIER)
	    ind = (int)((fp->current_size * yf) / (Float)SND_SRATE(cp->sound));
	  else ind = (int)yf;
	  time = (int)(si->target_slices * (Float)(x - ap->x_axis_x0) / (Float)(ap->x_axis_x1 - ap->x_axis_x0));
	  return(mus_format("(time: %.2f, freq: %.1f, val: %.3f%s (raw: %.3f))",
			    xf, yf,
			    (cp->fft_log_magnitude) ? cp_dB(cp, si->data[time][ind] / si->scale) : (si->data[time][ind] / si->scale),
			    (cp->fft_log_magnitude) ? "dB" : "",
			    si->data[time][ind]));
	}
    }
  return(copy_string("?"));
}

void fftb(chan_info *cp, int on)
{
  cp->ffting = on;
  set_toggle_button(channel_f(cp), on, FALSE, (void *)cp);
  calculate_fft(cp, NULL);
}

void waveb(chan_info *cp, int on)
{
  cp->waving = on;
  set_toggle_button(channel_w(cp), on, FALSE, (void *)cp);
  update_graph(cp, NULL);
}

static void propogate_wf_state(snd_info *sp)
{
  int i, w, f;
  chan_info *cp;
  if (sp->channel_style != CHANNELS_SEPARATE)
    {
      cp = sp->chans[0];
      w = cp->waving;
      f = cp->ffting;
      for (i = 1; i < sp->nchans; i++) 
	{
	  cp = sp->chans[i];
	  cp->waving = w;
	  cp->ffting = f;
	  set_toggle_button(channel_f(cp), (f) ? TRUE : FALSE, FALSE, (void *)cp);
	  set_toggle_button(channel_w(cp), (w) ? TRUE : FALSE, FALSE, (void *)cp);
	}
      map_over_sound_chans(sp, update_graph, NULL);
    }
}

void f_button_callback(chan_info *cp, int on, int with_control)
{
  snd_info *sp;
  int i;
  chan_info *ncp;
  cp->ffting = on;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    propogate_wf_state(sp);
  else
    {
      update_graph(cp, NULL);
      if (with_control)
	{
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->ffting = on;
		  set_toggle_button(channel_f(ncp), (on) ? TRUE : FALSE, FALSE, (void *)cp);
		  update_graph(ncp, NULL);
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
  cp->waving = on;
  sp = cp->sound;
  if (sp->channel_style != CHANNELS_SEPARATE)
    propogate_wf_state(sp);
  else
    {
      update_graph(cp, NULL);
      if (with_control)
	{
	  for (i = 0; i < sp->nchans; i++) 
	    {
	      ncp = sp->chans[i];
	      if (cp != ncp)
		{
		  ncp->waving = on;
		  set_toggle_button(channel_w(ncp), (on) ? TRUE : FALSE, FALSE, (void *)cp);
		  update_graph(ncp, NULL);
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
  int redisplay;
  cp = virtual_selected_channel(ncp);
  sp = cp->sound;
  select_channel(sp, cp->chan);
  if ((cp->lisp_graphing) && 
      (within_graph(cp, x, y) == LISP) &&
      (HOOKED(key_press_hook)))
    {
      /* return TRUE to keep this key press from being passed to keyboard_command */
      SCM res = SCM_BOOL_F;
      res = g_c_run_or_hook(key_press_hook,
			    SCM_LIST4(TO_SMALL_SCM_INT(sp->index),
				      TO_SMALL_SCM_INT(cp->chan),
				      TO_SCM_INT(keysym),
				      TO_SCM_INT(key_state)),
			    S_key_press_hook);
      if (TRUE_P(res))
	return(FALSE);
    }
  redisplay = keyboard_command(cp, keysym, key_state);
  /* if lisp graph has cursor? */
  handle_cursor_with_sync(cp, redisplay);
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
  if (cp->fft_style != SONOGRAM)
    return((Float)(ap->x_axis_x1 - ap->x_axis_x0));
  else return((Float)(ap->y_axis_y0 - ap->y_axis_y1));
}

static int cursor_round(double x)
{
  int xint;
  Float xfrac;
  xint = (int)x;
  xfrac = x-xint;
  if (xfrac > .5) return(xint + 1);
  return(xint);
}

static int calculate_syncd_fft(chan_info *cp, void *ptr)
{
  snd_info *sp;
  int sync = (*((int *)ptr));
  if (cp)
    {
      sp = cp->sound;
      if (sp->sync == sync) calculate_fft(cp, NULL);
    }
  return(0);
}

static int dragged = 0;
static TIME_TYPE mouse_down_time;
static mark *mouse_mark = NULL;
static mark *play_mark = NULL;
static int click_within_graph = NOGRAPH;
static int fft_axis_start = 0;
static int mix_tag = NO_MIX_TAG;

void graph_button_press_callback(chan_info *cp, int x, int y, int key_state, int button, TIME_TYPE time)
{
  snd_info *sp;
  sp = cp->sound;
  /* if combining, figure out which virtual channel the mouse is in */
  if (sp->channel_style == CHANNELS_COMBINED) cp = which_channel(sp, y);
  mouse_down_time = time;
  select_channel(sp, cp->chan);
  dragged = 0;
  finish_selection_creation();
  mouse_mark = hit_mark(cp, x, y, key_state);
  if (mouse_mark == NULL) 
    play_mark = hit_triangle(cp, x, y);
  click_within_graph = within_graph(cp, x, y);
  if (click_within_graph == FFT_AXIS) 
    {
      if (cp->fft_style != SONOGRAM)
	fft_axis_start = x;
      else fft_axis_start = y;
    }
  else
    {
      if (click_within_graph == LISP)
	{
	  if (HOOKED(mouse_press_hook))
	    g_c_run_progn_hook(mouse_press_hook,
			       SCM_LIST6(TO_SMALL_SCM_INT(sp->index),
					 TO_SMALL_SCM_INT(cp->chan),
					 TO_SCM_INT(button),
					 TO_SCM_INT(key_state),
					 TO_SCM_DOUBLE(ungrf_x((cp->lisp_info)->axis, x)),
					 TO_SCM_DOUBLE(ungrf_y((cp->lisp_info)->axis, y))),
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

void graph_button_release_callback(chan_info *cp, int x, int y, int key_state, int button)
{
  snd_info *sp;
  snd_state *ss;
  axis_info *ap;
  mark *old_mark;
  int actax, samps;
  char *str;
  sp = cp->sound;
  ss = cp->state;
  if (sp->channel_style == CHANNELS_COMBINED) cp = which_channel(sp, y);
  if (!dragged)
    {
      if (play_mark)
	{
	  old_mark = sp->playing_mark; /* needed because stop_playing clobbers sp->playing_mark */
	  if (sp->playing)
	    {
	      stop_playing_sound(sp);
	      set_play_button(sp, 0);
	    }
	  if (play_mark != old_mark)
	    {
	      if (play_mark->sync)
		play_syncd_mark(cp, play_mark);
	      else play_channel(cp, play_mark->samp, NO_END_SPECIFIED, TRUE, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), "play button");
	      sp->playing_mark = play_mark;
	      set_play_button(sp, 1);
	    }
	  else sp->playing_mark = NULL;
	  play_mark = NULL;
	}
      else
	{
	  actax = within_graph(cp, x, y);
	  if (actax == WAVE)
	    {
	      if (button == BUTTON_2) /* the middle button */
		{
		  cp->cursor_on = 1;
		  cursor_moveto(cp, 
				cursor_round(ungrf_x(cp->axis, x) * 
					     (double)SND_SRATE(sp)));
		  draw_graph_cursor(cp);
		  paste_region(0, cp, "Btn2");
		}
	      else 
		{
		  if (key_state & (snd_ShiftMask | snd_ControlMask | snd_MetaMask))
		    {
		      /* zoom request -> each added key zooms closer, as does each successive click */
		      ap = cp->axis;
		      samps = current_ed_samples(cp);
		      if ((samps > 0) && (ap->zx > (1.0 / (double)samps)))
			{
			  if (key_state & snd_ShiftMask) ap->zx *= .5;
			  if (key_state & snd_ControlMask) ap->zx *= .5;
			  if (key_state & snd_MetaMask) ap->zx *= .5;
			  ap->sx = (((double)(cp->cursor) / (double)SND_SRATE(sp) - ap->zx * 0.5 * (ap->xmax - ap->xmin)) - ap->xmin) / ap->x_ambit;
			  apply_x_axis_change(ap, cp, sp);
			  resize_sx(cp);
			  set_zx_scrollbar_value(cp, sqrt(ap->zx));
			}
		    }
		  else
		    {
		      cp->cursor_on = 1;
		      handle_cursor(cp, 
				    cursor_moveto(cp, 
						  cursor_round(ungrf_x(cp->axis, x) * 
							       (double)SND_SRATE(sp))));
		      if (mouse_mark)
			{
			  SCM res = SCM_BOOL_F;
			  if (HOOKED(mark_click_hook))
			    res = g_c_run_progn_hook(mark_click_hook,
						     SCM_LIST1(TO_SMALL_SCM_INT(mark_id(mouse_mark))),
						     S_mark_click_hook);
			  if (!(TRUE_P(res)))
			    report_in_minibuffer(sp, "mark %d at sample %d", 
						 mark_id(mouse_mark), 
						 mouse_mark->samp);
			}
		      else
			{
			  if (mix_tag != NO_MIX_TAG)
			    report_in_minibuffer(sp, "mix %d ", mix_tag);
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
		    (HOOKED(mouse_release_hook)))
		  g_c_run_progn_hook(mouse_release_hook,
				     SCM_LIST6(TO_SMALL_SCM_INT(sp->index),
					       TO_SMALL_SCM_INT(cp->chan),
					       TO_SCM_INT(button),
					       TO_SCM_INT(key_state),
					       TO_SCM_DOUBLE(ungrf_x((cp->lisp_info)->axis, x)),
					       TO_SCM_DOUBLE(ungrf_y((cp->lisp_info)->axis, y))),
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
	  dragged = 0;
	}
      else
	{
	  if (play_mark)
	    {
	      finish_moving_play_mark(cp);
	      stop_playing_sound(sp);
	      play_mark = NULL;
	      dragged = 0;
	    }
	  else
	    {
	      if (mix_tag != NO_MIX_TAG)
		{
		  finish_moving_mix_tag(mix_tag, x);
		  dragged = 0;
		  mix_tag = NO_MIX_TAG;
		}
	      else
		{
		  if (click_within_graph == WAVE)
		    {
		      cancel_selection_watch();
		      finish_selection_creation();
		      dragged = 0;
		      if (show_selection_transform(ss)) 
			{
			  if (sp->sync)
			    map_over_chans(ss, calculate_syncd_fft, (void *)(&(sp->sync)));
			  else calculate_fft(cp, NULL);
			}
		    }
		}
	    }
	}
    }
}

static TIME_TYPE first_time = 0;
static int mouse_cursor = 0;

void graph_button_motion_callback(chan_info *cp, int x, int y, TIME_TYPE time, TIME_TYPE click_time)
{
  snd_info *sp;
  snd_state *ss;
  TIME_TYPE mouse_time, time_interval;
  int samps;
  char *str;
  Float old_cutoff;
  /* this needs to be a little slow about deciding that we are dragging, as opposed to a slow click */
  mouse_time = time;
  if ((mouse_time - mouse_down_time) < (click_time / 2)) return;
  sp = cp->sound;
  if (sp->channel_style == CHANNELS_COMBINED) cp = which_channel(sp, y);
  select_channel(sp, cp->chan);
  if (mouse_mark)
    {
      move_mark(cp, mouse_mark, x);
      dragged = 1;
    }
  else
    {
      if (play_mark)
	{
	  if (!dragged)
	    {
	      first_time = mouse_time;
	      dragged = 1;
	      sp->speed_control = 0.0;
	      mouse_cursor = cp->cursor;
	      play_channel(cp, play_mark->samp, NO_END_SPECIFIED, TRUE, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), "drag playing mark");
	      set_play_button(sp, 1);
	    }
	  else
	    {
	      time_interval = mouse_time - first_time;
	      first_time = mouse_time;
	      samps = move_play_mark(cp, &mouse_cursor, (Locus)x);
	      if (time_interval != 0)
		sp->speed_control = (Float)(samps * 1000) / (Float)(time_interval * SND_SRATE(sp));
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
		  dragged = 1;
		  return;
		}
	      if (!dragged) 
		start_selection_creation(cp, (int)snd_round(ungrf_x(cp->axis, x) * SND_SRATE(sp)));
	      else 
		{
		  update_possible_selection_in_progress((int)snd_round(ungrf_x(cp->axis, x) * SND_SRATE(sp)));
		  move_selection(cp, x);
		}
	      dragged = 1;
	    }
	  else
	    {
	      ss = cp->state;
	      if (click_within_graph == FFT_AXIS)
		{
		  /* change spectro_cutoff(ss) and redisplay fft */
		  old_cutoff = cp->spectro_cutoff;
		  if (cp->fft_style != SONOGRAM)
		    {
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
		      if (cp->fft_style != NORMAL_FFT)
			map_over_chans(ss, sono_update, NULL);
		      else map_over_chans(ss, update_graph, NULL);
		    }
		}
	      else
		{
		  if (click_within_graph == LISP)
		    {
		      if (HOOKED(mouse_drag_hook))
			  g_c_run_progn_hook(mouse_drag_hook,
					     SCM_LIST6(TO_SMALL_SCM_INT(cp->sound->index),
						       TO_SMALL_SCM_INT(cp->chan),
						       TO_SCM_INT(-1),
						       TO_SCM_INT(-1),
						       TO_SCM_DOUBLE(ungrf_x((cp->lisp_info)->axis, x)),
						       TO_SCM_DOUBLE(ungrf_y((cp->lisp_info)->axis, y))),
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
	  /* if this changes, see snd-xprint.c ps_rgb */
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

#include "vct.h"

enum {CP_FFTING, CP_WAVING, CP_FRAMES, CP_CURSOR, CP_LISP_GRAPHING, CP_AP_LOSAMP, CP_AP_HISAMP, CP_SQUELCH_UPDATE,
      CP_EDIT_CTR, CP_CURSOR_STYLE, CP_EDIT_HOOK, CP_UNDO_HOOK,
      CP_SHOW_Y_ZERO, CP_SHOW_MARKS, CP_WAVO, CP_WAVO_HOP, CP_WAVO_TRACE, CP_MAX_FFT_PEAKS, 
      CP_SHOW_FFT_PEAKS, CP_ZERO_PAD, CP_VERBOSE_CURSOR, CP_FFT_LOG_FREQUENCY, CP_FFT_LOG_MAGNITUDE,
      CP_WAVELET_TYPE, CP_SPECTRO_HOP, CP_FFT_SIZE, CP_FFT_STYLE, CP_FFT_WINDOW, CP_TRANSFORM_TYPE,
      CP_NORMALIZE_FFT, CP_SHOW_MIX_WAVEFORMS, CP_GRAPH_STYLE, CP_DOT_SIZE,
      CP_SHOW_AXES, CP_GRAPHS_HORIZONTAL, CP_SYNC, CP_CURSOR_SIZE, CP_CURSOR_POSITION,
      CP_EDPOS_FRAMES
};

static SCM cp_edpos;

static SCM cp_iread(SCM snd_n, SCM chn_n, int fld, char *caller)
{
  chan_info *cp;
  snd_info *sp = NULL;
  snd_state *ss;
  int i;
  SCM res = SCM_EOL;
  if (TRUE_P(snd_n))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = CONS(cp_iread(TO_SMALL_SCM_INT(i), chn_n, fld, caller), res);
	}
      return(REVERSE_LIST(res));
    }
  else
    {
      if (TRUE_P(chn_n))
	{
	  sp = get_sp(snd_n);
	  if (sp == NULL)
	    return(snd_no_such_sound_error(caller, snd_n));
	  for (i = 0; i < sp->nchans; i++)
	    res = CONS(cp_iread(snd_n, TO_SMALL_SCM_INT(i), fld, caller), res);
	  return(REVERSE_LIST(res));
	}
      else
	{
	  SND_ASSERT_CHAN(caller, snd_n, chn_n, 1);
	  cp = get_cp(snd_n, chn_n, caller);
	  switch(fld)
	    {
	    case CP_EDIT_CTR:           return(TO_SCM_INT(cp->edit_ctr));                          break;
	    case CP_FFTING:             return(TO_SCM_BOOLEAN(cp->ffting));                        break;
	    case CP_WAVING:             return(TO_SCM_BOOLEAN(cp->waving));                        break;
	    case CP_CURSOR:             return(TO_SCM_INT(cp->cursor));                            break;
	    case CP_FRAMES:             return(TO_SCM_INT(current_ed_samples(cp)));                break;
	    case CP_LISP_GRAPHING:      return(TO_SCM_BOOLEAN(cp->lisp_graphing));                 break;
	    case CP_AP_LOSAMP:          if (cp->axis) return(TO_SCM_INT((cp->axis)->losamp));      break;
	    case CP_AP_HISAMP:          if (cp->axis) return(TO_SCM_INT((cp->axis)->hisamp));      break;
	    case CP_SQUELCH_UPDATE:     return(TO_SCM_BOOLEAN(cp->squelch_update));                break;
	    case CP_CURSOR_SIZE:        return(TO_SCM_INT(cp->cursor_size));                       break;
	    case CP_CURSOR_STYLE:       return(TO_SCM_INT(cp->cursor_style));                      break;
	    case CP_EDIT_HOOK:          return(cp->edit_hook);                                     break;
	    case CP_UNDO_HOOK:          return(cp->undo_hook);                                     break;
	    case CP_SHOW_Y_ZERO:        return(TO_SCM_BOOLEAN(cp->show_y_zero));                   break;
	    case CP_SHOW_MARKS:         return(TO_SCM_BOOLEAN(cp->show_marks));                    break;
	    case CP_WAVO:               return(TO_SCM_BOOLEAN(cp->wavo));                          break;
	    case CP_WAVO_HOP:           return(TO_SCM_INT(cp->wavo_hop));                          break;
	    case CP_WAVO_TRACE:         return(TO_SCM_INT(cp->wavo_trace));                        break;
	    case CP_MAX_FFT_PEAKS:      return(TO_SCM_INT(cp->max_fft_peaks));                     break;
	    case CP_ZERO_PAD:           return(TO_SCM_INT(cp->zero_pad));                          break;
	    case CP_WAVELET_TYPE:       return(TO_SCM_INT(cp->wavelet_type));                      break;
	    case CP_SHOW_FFT_PEAKS:     return(TO_SCM_BOOLEAN(cp->show_fft_peaks));                break;
	    case CP_VERBOSE_CURSOR:     return(TO_SCM_BOOLEAN(cp->verbose_cursor));                break;
	    case CP_FFT_LOG_FREQUENCY:  return(TO_SCM_BOOLEAN(cp->fft_log_frequency));             break;
	    case CP_FFT_LOG_MAGNITUDE:  return(TO_SCM_BOOLEAN(cp->fft_log_magnitude));             break;
	    case CP_SPECTRO_HOP:        return(TO_SCM_INT(cp->spectro_hop));                       break;
	    case CP_FFT_SIZE:           return(TO_SCM_INT(cp->fft_size));                          break;
	    case CP_FFT_STYLE:          return(TO_SCM_INT(cp->fft_style));                         break;
	    case CP_FFT_WINDOW:         return(TO_SCM_INT(cp->fft_window));                        break;
	    case CP_TRANSFORM_TYPE:     return(TO_SCM_INT(cp->transform_type));                    break;
	    case CP_NORMALIZE_FFT:      return(TO_SCM_INT(cp->normalize_fft));                     break;
	    case CP_SHOW_MIX_WAVEFORMS: return(TO_SCM_BOOLEAN(cp->show_mix_waveforms));            break;
	    case CP_GRAPH_STYLE:        return(TO_SCM_INT(cp->graph_style));                       break;
	    case CP_DOT_SIZE:           return(TO_SCM_INT(cp->dot_size));                          break;
	    case CP_SHOW_AXES:          return(TO_SCM_INT(cp->show_axes));                         break;
	    case CP_GRAPHS_HORIZONTAL:  return(TO_SCM_BOOLEAN(cp->graphs_horizontal));             break;
	    case CP_SYNC:               return(TO_SCM_INT(cp->sync));                              break;
	    case CP_CURSOR_POSITION:    return(SCM_LIST2(TO_SCM_INT(cp->cx), TO_SCM_INT(cp->cy))); break;
	    case CP_EDPOS_FRAMES:       return(TO_SCM_INT(to_c_edit_samples(cp, cp_edpos, caller))); break;
	    }
	}
    }
  return(SCM_BOOL_F);
}

static int g_mus_iclamp(int mn, SCM val, int def, int mx)
{
  return(mus_iclamp(mn, TO_C_INT_OR_ELSE(val, def), mx));
}

static int g_imin(int mn, SCM val, int def)
{
  int nval;
  nval = TO_C_INT_OR_ELSE(val, def);
  if (nval >= mn) return(nval);
  return(mn);
}

static SCM cp_iwrite(SCM snd_n, SCM chn_n, SCM on, int fld, char *caller)
{
  chan_info *cp;
  int val = 0;
  snd_info *sp;
  snd_state *ss;
  int i, curlen, newlen;
  char *error = NULL;
  SCM res = SCM_EOL, errstr;
  if (SCM_EQ_P(snd_n, SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = CONS(cp_iwrite(TO_SMALL_SCM_INT(i), chn_n, on, fld, caller), res);
	}
      return(REVERSE_LIST(res));
    }
  if (SCM_EQ_P(chn_n, SCM_BOOL_T))
    {
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      for (i = 0; i < sp->nchans; i++)
	res = CONS(cp_iwrite(snd_n, TO_SMALL_SCM_INT(i), on, fld, caller), res);
      return(REVERSE_LIST(res));
    }
  SND_ASSERT_CHAN(caller, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, caller);
  switch (fld)
    {
    case CP_EDIT_CTR:
      val = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, 0, caller);
      if (cp->edit_ctr < val)
	redo_edit(cp, val - cp->edit_ctr);
      else undo_edit(cp, cp->edit_ctr - val);
      return(TO_SCM_INT(cp->edit_ctr));
      break;
    case CP_FFTING:
      fftb(cp, val = TO_C_BOOLEAN_OR_T(on)); 
      update_graph(cp, NULL);
      break;
    case CP_WAVING:
      waveb(cp, val = TO_C_BOOLEAN_OR_T(on)); 
      update_graph(cp, NULL);
      break;
    case CP_CURSOR:
      cp->cursor_on = 1; 
      handle_cursor(cp, cursor_moveto(cp, val = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, 1, caller)));
      break;
    case CP_LISP_GRAPHING:
      cp->lisp_graphing = TO_C_BOOLEAN_OR_T(on); 
      val = cp->lisp_graphing; 
      update_graph(cp, NULL);
      break;
    case CP_AP_LOSAMP:
      set_x_axis_x0(cp, val = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, 0, caller)); 
      return(on);
      break;
    case CP_AP_HISAMP:
      set_x_axis_x1(cp, val = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, 1, caller)); 
      return(on);
      break;
    case CP_SQUELCH_UPDATE:
      cp->squelch_update = TO_C_BOOLEAN_OR_T(on);
      break;
    case CP_CURSOR_SIZE:
      cp->cursor_size = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, DEFAULT_CURSOR_SIZE, caller);
      update_graph(cp, NULL); 
      return(TO_SCM_INT(cp->cursor_size));
      break;
    case CP_CURSOR_STYLE:
      if (PROCEDURE_P(on))
	{
	  error = procedure_ok(on, 3, "set-" S_cursor_style, "", 1);
	  if (error == NULL)
	    {
	      if ((cp->cursor_style == CURSOR_PROC) &&
		  (PROCEDURE_P(cp->cursor_proc)))
		snd_unprotect(cp->cursor_proc);
	      snd_protect(on);
	      cp->cursor_proc = on;
	      cp->cursor_style = CURSOR_PROC;
	    }
	  else 
	    {
	      errstr = TO_SCM_STRING(error);
	      FREE(error);
	      return(snd_bad_arity_error("set-" S_cursor_style, errstr, on));
	    }
	}
      else
	{
	  if ((cp->cursor_style == CURSOR_PROC) &&
	      (PROCEDURE_P(cp->cursor_proc)))
	    {
	      snd_unprotect(cp->cursor_proc);
	      cp->cursor_proc = SCM_UNDEFINED;
	    }
	  cp->cursor_style = g_mus_iclamp(CURSOR_CROSS, on, CURSOR_CROSS, CURSOR_LINE);
	}
      update_graph(cp, NULL); 
      return(TO_SCM_INT(cp->cursor_style));
      break;
    case CP_SHOW_Y_ZERO:
      cp->show_y_zero = TO_C_BOOLEAN_OR_T(on); 
      update_graph(cp, NULL); 
      return(TO_SCM_BOOLEAN(cp->show_y_zero));
      break;
    case CP_SHOW_MARKS:
      cp->show_marks = TO_C_BOOLEAN_OR_T(on); 
      update_graph(cp, NULL); 
      return(TO_SCM_BOOLEAN(cp->show_marks));
      break;
    case CP_WAVO:
      cp->wavo = TO_C_BOOLEAN_OR_T(on); 
      update_graph(cp, NULL); 
      return(TO_SCM_BOOLEAN(cp->wavo));
      break;
    case CP_WAVO_HOP:
      cp->wavo_hop = g_imin(1, on, DEFAULT_WAVO_HOP); 
      update_graph(cp, NULL); 
      return(TO_SCM_INT(cp->wavo_hop));
      break;
    case CP_WAVO_TRACE:
      cp->wavo_trace = g_imin(1, on, DEFAULT_WAVO_TRACE); 
      update_graph(cp, NULL); 
      return(TO_SCM_INT(cp->wavo_trace));
      break;
    case CP_MAX_FFT_PEAKS:
      cp->max_fft_peaks = g_imin(0, on, DEFAULT_MAX_FFT_PEAKS); 
      return(TO_SCM_INT(cp->max_fft_peaks));
      break;
    case CP_ZERO_PAD:
      cp->zero_pad = g_imin(0, on, DEFAULT_ZERO_PAD); 
      update_graph(cp, NULL);
      return(TO_SCM_INT(cp->zero_pad));
      break;
    case CP_WAVELET_TYPE:
      cp->wavelet_type = g_mus_iclamp(0, on, DEFAULT_WAVELET_TYPE, NUM_WAVELETS - 1); 
      update_graph(cp, NULL);
      return(TO_SCM_INT(cp->wavelet_type));
      break;
    case CP_SHOW_FFT_PEAKS:
      cp->show_fft_peaks = TO_C_BOOLEAN_OR_T(on); 
      update_graph(cp, NULL); 
      return(TO_SCM_BOOLEAN(cp->show_fft_peaks));
      break;
    case CP_VERBOSE_CURSOR:
      cp->verbose_cursor = TO_C_BOOLEAN_OR_T(on); 
      return(TO_SCM_BOOLEAN(cp->verbose_cursor));
      break;
    case CP_FFT_LOG_FREQUENCY:
      cp->fft_log_frequency = TO_C_BOOLEAN_OR_T(on); 
      if (cp->ffting) calculate_fft(cp, NULL); 
      return(TO_SCM_BOOLEAN(cp->fft_log_frequency));
      break;
    case CP_FFT_LOG_MAGNITUDE:
      cp->fft_log_magnitude = TO_C_BOOLEAN_OR_T(on); 
      if (cp->ffting) calculate_fft(cp, NULL); 
      return(TO_SCM_BOOLEAN(cp->fft_log_magnitude));
      break;
    case CP_SPECTRO_HOP:
      cp->spectro_hop = g_imin(1, on, DEFAULT_SPECTRO_HOP); 
      if (cp->ffting) calculate_fft(cp, NULL); 
      return(TO_SCM_INT(cp->spectro_hop));
      break;
    case CP_FFT_SIZE:
      cp->fft_size = g_imin(1, on, DEFAULT_FFT_SIZE); 
      calculate_fft(cp, NULL);
      return(TO_SCM_INT(cp->fft_size));
      break;
    case CP_FFT_STYLE: 
      cp->fft_style = g_mus_iclamp(0, on, DEFAULT_FFT_STYLE, MAX_FFT_STYLE); 
      calculate_fft(cp, NULL); 
      return(TO_SCM_INT(cp->fft_style)); 
      break;
    case CP_FFT_WINDOW:
      cp->fft_window = g_mus_iclamp(0, on, DEFAULT_FFT_WINDOW, NUM_FFT_WINDOWS - 1); 
      calculate_fft(cp, NULL); 
      return(TO_SCM_INT(cp->fft_window));
      break;
    case CP_TRANSFORM_TYPE:
      cp->transform_type = g_imin(0, on, DEFAULT_TRANSFORM_TYPE); 
      calculate_fft(cp, NULL); 
      return(TO_SCM_INT(cp->transform_type));
      break;
    case CP_NORMALIZE_FFT:      
      if (INTEGER_P(on))
	cp->normalize_fft = TO_SMALL_C_INT(on);
      else
	if (NUMBER_P(on))
	  cp->normalize_fft = ((int)TO_C_DOUBLE_WITH_ORIGIN(on, caller));
	else
	  if (FALSE_P(on))
	    cp->normalize_fft = 0;
	  else 
	    if (TRUE_P(on))
	      cp->normalize_fft = 1;
	    else cp->normalize_fft = DEFAULT_NORMALIZE_FFT;
      calculate_fft(cp, NULL); 
      return(TO_SCM_INT(cp->normalize_fft));
      break;
    case CP_SHOW_MIX_WAVEFORMS: 
      cp->show_mix_waveforms = TO_C_BOOLEAN_OR_T(on); 
      update_graph(cp, NULL); 
      return(TO_SCM_BOOLEAN(cp->show_mix_waveforms));
      break;
    case CP_GRAPH_STYLE:
      cp->graph_style = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, DEFAULT_GRAPH_STYLE, caller);
      update_graph(cp, NULL); 
      return(TO_SCM_INT(cp->graph_style));
      break;
    case CP_DOT_SIZE:
      cp->dot_size = g_imin(0, on, DEFAULT_DOT_SIZE); 
      update_graph(cp, NULL);
      return(TO_SCM_INT(cp->dot_size));
      break;
    case CP_SHOW_AXES:
      cp->show_axes = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, DEFAULT_SHOW_AXES, caller); 
      update_graph(cp, NULL); 
      return(TO_SCM_INT(cp->show_axes));
      break;
    case CP_GRAPHS_HORIZONTAL:
      cp->graphs_horizontal = TO_C_BOOLEAN_OR_T(on); 
      update_graph(cp, NULL); 
      return(TO_SCM_BOOLEAN(cp->graphs_horizontal));
      break;
    case CP_SYNC:
      cp->sync = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, 1, caller); 
      return(TO_SCM_INT(cp->sync)); 
      break;
    case CP_FRAMES:
      /* if less than current, delete, else zero pad */
      curlen = current_ed_samples(cp);
      newlen = TO_C_INT_OR_ELSE_WITH_ORIGIN(on, curlen, caller);
      if (curlen > newlen)
	delete_samples(newlen-1, curlen-newlen, cp, "(set-frames)");
      else
	{
	  if (newlen > curlen)
	    extend_with_zeros(cp, curlen, newlen-curlen, "(set-frames)");
	}
      update_graph(cp, NULL);
      break;
    }
  return(TO_SCM_BOOLEAN(val));
}

enum {CP_MIN_DB, CP_SPECTRO_X_ANGLE, CP_SPECTRO_Y_ANGLE, CP_SPECTRO_Z_ANGLE, CP_SPECTRO_X_SCALE, CP_SPECTRO_Y_SCALE, CP_SPECTRO_Z_SCALE,
      CP_SPECTRO_CUTOFF, CP_SPECTRO_START, CP_FFT_BETA, CP_AP_SX, CP_AP_SY, CP_AP_ZX, CP_AP_ZY, CP_MAXAMP, CP_EDPOS_MAXAMP
};


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


static SCM cp_fread(SCM snd_n, SCM chn_n, int fld, char *caller)
{
  chan_info *cp;
  snd_info *sp;
  snd_state *ss;
  int i;
  SCM res = SCM_EOL;
  if (SCM_EQ_P(snd_n, SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = CONS(cp_fread(TO_SMALL_SCM_INT(i), chn_n, fld, caller), res);
	}
      return(REVERSE_LIST(res));
    }
  if (SCM_EQ_P(chn_n, SCM_BOOL_T))
    {
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      for (i = 0; i < sp->nchans; i++)
	res = CONS(cp_fread(snd_n, TO_SMALL_SCM_INT(i), fld, caller), res);
      return(REVERSE_LIST(res));
    }
  SND_ASSERT_CHAN(caller, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, caller);
  switch(fld)
    {
    case CP_AP_SX:           if (cp->axis) return(TO_SCM_DOUBLE((cp->axis)->sx));     break;
    case CP_AP_SY:           if (cp->axis) return(TO_SCM_DOUBLE((cp->axis)->sy));     break;
    case CP_AP_ZX:           if (cp->axis) return(TO_SCM_DOUBLE((cp->axis)->zx));     break;
    case CP_AP_ZY:           if (cp->axis) return(TO_SCM_DOUBLE((cp->axis)->zy));     break;
    case CP_MIN_DB:          return(TO_SCM_DOUBLE(cp->min_dB));                       break;
    case CP_SPECTRO_X_ANGLE: return(TO_SCM_DOUBLE(cp->spectro_x_angle));              break;
    case CP_SPECTRO_Y_ANGLE: return(TO_SCM_DOUBLE(cp->spectro_y_angle));              break;
    case CP_SPECTRO_Z_ANGLE: return(TO_SCM_DOUBLE(cp->spectro_z_angle));              break;
    case CP_SPECTRO_X_SCALE: return(TO_SCM_DOUBLE(cp->spectro_x_scale));              break;
    case CP_SPECTRO_Y_SCALE: return(TO_SCM_DOUBLE(cp->spectro_y_scale));              break;
    case CP_SPECTRO_Z_SCALE: return(TO_SCM_DOUBLE(cp->spectro_z_scale));              break;
    case CP_SPECTRO_CUTOFF:  return(TO_SCM_DOUBLE(cp->spectro_cutoff));               break;
    case CP_SPECTRO_START:   return(TO_SCM_DOUBLE(cp->spectro_start));                break;
    case CP_FFT_BETA:        return(TO_SCM_DOUBLE(cp->fft_beta));                     break;
    case CP_MAXAMP:          return(TO_SCM_DOUBLE(get_maxamp(cp->sound, cp, AT_CURRENT_EDIT_POSITION)));        break;
    case CP_EDPOS_MAXAMP:    return(TO_SCM_DOUBLE(get_maxamp(cp->sound, cp, to_c_edit_position(cp, cp_edpos, S_maxamp)))); break;
    }
  return(SCM_BOOL_F);
}

static SCM cp_fwrite(SCM snd_n, SCM chn_n, SCM on, int fld, char *caller)
{
  chan_info *cp;
  snd_info *sp;
  snd_state *ss;
  int i;
  Float curamp;
  Float newamp[1];
  SCM res = SCM_EOL;
  if (SCM_EQ_P(snd_n, SCM_BOOL_T))
    {
      ss = get_global_state();
      for (i = 0; i < ss->max_sounds; i++)
	{
	  sp = ss->sounds[i];
	  if ((sp) && (sp->inuse))
	    res = CONS(cp_fwrite(TO_SMALL_SCM_INT(i), chn_n, on, fld, caller), res);
	}
      return(REVERSE_LIST(res));
    }
  if (SCM_EQ_P(chn_n, SCM_BOOL_T))
    {
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      for (i = 0; i < sp->nchans; i++)
	res = CONS(cp_fwrite(snd_n, TO_SMALL_SCM_INT(i), on, fld, caller), res);
      return(REVERSE_LIST(res));
    }
  SND_ASSERT_CHAN(caller, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, caller);
  switch (fld)
    {
    case CP_AP_SX:
      reset_x_display(cp, mus_fclamp(0.0, TO_C_DOUBLE(on), 1.0), cp->axis->zx);
      break;
    case CP_AP_ZX:
      reset_x_display(cp, cp->axis->sx, mus_fclamp(0.0, TO_C_DOUBLE(on), 1.0));
      break;
    case CP_AP_SY:
      reset_y_display(cp, mus_fclamp(0.0, TO_C_DOUBLE(on), 1.0), cp->axis->zy);
      break;
    case CP_AP_ZY:
      reset_y_display(cp, cp->axis->sy, mus_fclamp(0.0, TO_C_DOUBLE(on), 1.0)); 
      break;
    case CP_MIN_DB:
      cp->min_dB = TO_C_DOUBLE(on); 
      cp->lin_dB = pow(10.0, cp->min_dB * 0.05); 
      calculate_fft(cp, NULL); 
      break;
    case CP_SPECTRO_X_ANGLE: cp->spectro_x_angle = TO_C_DOUBLE(on); calculate_fft(cp, NULL); break;
    case CP_SPECTRO_Y_ANGLE: cp->spectro_y_angle = TO_C_DOUBLE(on); calculate_fft(cp, NULL); break;
    case CP_SPECTRO_Z_ANGLE: cp->spectro_z_angle = TO_C_DOUBLE(on); calculate_fft(cp, NULL); break;
    case CP_SPECTRO_X_SCALE: cp->spectro_x_scale = TO_C_DOUBLE(on); calculate_fft(cp, NULL); break;
    case CP_SPECTRO_Y_SCALE: cp->spectro_y_scale = TO_C_DOUBLE(on); calculate_fft(cp, NULL); break;
    case CP_SPECTRO_Z_SCALE: cp->spectro_z_scale = TO_C_DOUBLE(on); calculate_fft(cp, NULL); break;
    case CP_SPECTRO_CUTOFF:  
      cp->spectro_cutoff = mus_fclamp(0.0, TO_C_DOUBLE(on), 1.0); 
      calculate_fft(cp, NULL); 
      return(TO_SCM_DOUBLE(cp->spectro_cutoff)); 
      break;
    case CP_SPECTRO_START:   
      cp->spectro_start = mus_fclamp(0.0, TO_C_DOUBLE(on), 1.0); 
      calculate_fft(cp, NULL); 
      return(TO_SCM_DOUBLE(cp->spectro_start));   
      break;
    case CP_FFT_BETA:        
      cp->fft_beta = mus_fclamp(0.0, TO_C_DOUBLE(on), 1.0); 
      calculate_fft(cp, NULL); 
      return(TO_SCM_DOUBLE(cp->fft_beta));             
      break;
    case CP_MAXAMP:
      curamp = get_maxamp(cp->sound, cp, AT_CURRENT_EDIT_POSITION);
      newamp[0] = TO_C_DOUBLE(on);
      if (curamp != newamp[0])
	{
	  scale_to(cp->state, cp->sound, cp, newamp, 1, FALSE);
	  update_graph(cp, NULL);
	}
      break;
    }
  return(on);
}


#define WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(name_reversed, name) \
static SCM name_reversed(SCM arg1, SCM arg2, SCM arg3) \
{ \
  if (NOT_BOUND_P(arg1)) \
    return(name(SCM_BOOL_T, SCM_UNDEFINED, SCM_UNDEFINED)); \
  else { \
    if (NOT_BOUND_P(arg2)) \
      return(name(arg1, SCM_UNDEFINED, SCM_UNDEFINED)); \
    else { \
      if (NOT_BOUND_P(arg3)) \
        return(name(arg2, arg1, SCM_UNDEFINED)); \
      else return(name(arg3, arg1, arg2)); \
}}}

static SCM g_edit_position(SCM snd_n, SCM chn_n) 
{
  #define H_edit_position "(" S_edit_position " &optional snd chn) -> current edit history position in snd's channel chn"
  return(cp_iread(snd_n, chn_n, CP_EDIT_CTR, S_edit_position));
}

static SCM g_set_edit_position(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(INTEGER_P(on), on, SCM_ARG1, "set-" S_edit_position, "an integer");
  return(cp_iwrite(snd_n, chn_n, on, CP_EDIT_CTR, "set-" S_edit_position));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_edit_position_reversed, g_set_edit_position)

static SCM g_ffting(SCM snd_n, SCM chn_n) 
{
  #define H_ffting "(" S_ffting " &optional snd chn) -> #t if fft display is active in snd's channel chn"
  return(cp_iread(snd_n, chn_n, CP_FFTING, S_ffting));
}

static SCM g_set_ffting(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_ffting, "a boolean");
  return(cp_iwrite(snd_n, chn_n, on, CP_FFTING, "set-" S_ffting));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_ffting_reversed, g_set_ffting)

static SCM g_waving(SCM snd_n, SCM chn_n) 
{
  #define H_waving "(" S_waving " &optional snd chn) -> #t if time domain display is active in snd's channel chn"
  return(cp_iread(snd_n, chn_n, CP_WAVING, S_waving));
}

static SCM g_set_waving(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_waving, "a boolean");
  return(cp_iwrite(snd_n, chn_n, on, CP_WAVING, "set-" S_waving));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_waving_reversed, g_set_waving)

static SCM g_graphing(SCM snd_n, SCM chn_n) 
{
  #define H_graphing "(" S_graphing " &optional snd chn) -> #t if lisp-generated data display is active in snd's channel chn"
  return(cp_iread(snd_n, chn_n, CP_LISP_GRAPHING, S_graphing));
}

static SCM g_set_graphing(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_graphing, "a boolean");
  return(cp_iwrite(snd_n, chn_n, on, CP_LISP_GRAPHING, "set-" S_graphing));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_graphing_reversed, g_set_graphing)

static SCM g_cursor(SCM snd_n, SCM chn_n) 
{
  #define H_cursor "(" S_cursor " &optional snd chn) -> current cursor location in snd's channel chn"
  return(cp_iread(snd_n, chn_n, CP_CURSOR, S_cursor));
}

static SCM g_set_cursor(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(INTEGER_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_cursor, "an integer");
  return(cp_iwrite(snd_n, chn_n, on, CP_CURSOR, "set-" S_cursor));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_cursor_reversed, g_set_cursor)

static SCM g_cursor_style(SCM snd_n, SCM chn_n) 
{
  #define H_cursor_style "(" S_cursor_style " &optional snd chn) -> current cursor style in snd's channel chn"
  return(cp_iread(snd_n, chn_n, CP_CURSOR_STYLE, S_cursor_style));
}

static SCM g_set_cursor_style(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(INTEGER_P(on) || PROCEDURE_P(on), on, SCM_ARG1, "set-" S_cursor_style, "an integer");
  return(cp_iwrite(snd_n, chn_n, on, CP_CURSOR_STYLE, "set-" S_cursor_style));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_cursor_style_reversed, g_set_cursor_style)

static SCM g_cursor_size(SCM snd_n, SCM chn_n) 
{
  #define H_cursor_size "(" S_cursor_size " &optional snd chn) -> current cursor size in snd's channel chn"
  return(cp_iread(snd_n, chn_n, CP_CURSOR_SIZE, S_cursor_size));
}

static SCM g_set_cursor_size(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(INTEGER_P(on), on, SCM_ARG1, "set-" S_cursor_size, "an integer");
  return(cp_iwrite(snd_n, chn_n, on, CP_CURSOR_SIZE, "set-" S_cursor_size));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_cursor_size_reversed, g_set_cursor_size)

static SCM g_cursor_position(SCM snd, SCM chn)
{
  #define H_cursor_position "(" S_cursor_position " &optional snd chn) -> current cursor position (x y) in snd's channel chn"
  return(cp_iread(snd, chn, CP_CURSOR_POSITION, S_cursor_position));
}

static SCM g_frames(SCM snd_n, SCM chn_n, SCM edpos)
{
  #define H_frames "(" S_frames " &optional snd chn edpos) -> number of frames of data in snd's channel chn"
  SCM res;
  if (BOUND_P(edpos))
    {
      cp_edpos = edpos;
      snd_protect(cp_edpos);
      res = cp_iread(snd_n, chn_n, CP_EDPOS_FRAMES, S_frames);
      snd_unprotect(cp_edpos);
      return(res);
    }
  return(cp_iread(snd_n, chn_n, CP_FRAMES, S_frames));
}

static SCM g_set_frames(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_cursor_size, "a number");
  return(cp_iwrite(snd_n, chn_n, on, CP_FRAMES, "set-" S_frames));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_frames_reversed, g_set_frames)

static SCM g_maxamp(SCM snd_n, SCM chn_n, SCM edpos) 
{
  #define H_maxamp "(" S_maxamp " &optional snd chn edpos) -> max amp of data in snd's channel chn"
  SCM res;
  if (BOUND_P(edpos))
    {
      cp_edpos = edpos;
      snd_protect(cp_edpos);
      res = cp_fread(snd_n, chn_n, CP_EDPOS_MAXAMP, S_maxamp);
      snd_unprotect(cp_edpos);
      return(res);
    }
  return(cp_fread(snd_n, chn_n, CP_MAXAMP, S_maxamp));
}

static SCM g_set_maxamp(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_maxamp, "a number");
  return(cp_fwrite(snd_n, chn_n, on, CP_MAXAMP, "set-" S_maxamp));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_maxamp_reversed, g_set_maxamp)

static SCM g_squelch_update(SCM snd_n, SCM chn_n) 
{
  #define H_squelch_update "(" S_squelch_update " &optional snd chn) -> #t if updates (redisplays) are off in snd's channel chn"
  return(cp_iread(snd_n, chn_n, CP_SQUELCH_UPDATE, S_squelch_update));
}

static SCM g_set_squelch_update(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_squelch_update, "a boolean");
  return(cp_iwrite(snd_n, chn_n, on, CP_SQUELCH_UPDATE, "set-" S_squelch_update));
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_squelch_update_reversed, g_set_squelch_update)

static SCM g_ap_sx(SCM snd_n, SCM chn_n) 
{
  #define H_x_position_slider "(" S_x_position_slider " &optional snd chn) -> current x axis position slider of snd channel chn"
  return(cp_fread(snd_n, chn_n, CP_AP_SX, S_x_position_slider));
}

static SCM g_set_ap_sx(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_x_position_slider, "a number");
  return(cp_fwrite(snd_n, chn_n, on, CP_AP_SX, "set-" S_x_position_slider));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_ap_sx_reversed, g_set_ap_sx)

static SCM g_ap_sy(SCM snd_n, SCM chn_n) 
{
  #define H_y_position_slider "(" S_y_position_slider " &optional snd chn) -> current y axis position slider of snd channel chn"
  return(cp_fread(snd_n, chn_n, CP_AP_SY, S_y_position_slider));
}

static SCM g_set_ap_sy(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_y_position_slider, "a number");
  return(cp_fwrite(snd_n, chn_n, on, CP_AP_SY, "set-" S_y_position_slider));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_ap_sy_reversed, g_set_ap_sy)

static SCM g_ap_zx(SCM snd_n, SCM chn_n) 
{
  #define H_x_zoom_slider "(" S_x_zoom_slider " &optional snd chn) -> current x axis zoom slider of snd channel chn"
  return(cp_fread(snd_n, chn_n, CP_AP_ZX, S_x_zoom_slider));
}

static SCM g_set_ap_zx(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_x_zoom_slider, "a number");
  return(cp_fwrite(snd_n, chn_n, on, CP_AP_ZX, "set-" S_x_zoom_slider));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_ap_zx_reversed, g_set_ap_zx)


static SCM g_ap_zy(SCM snd_n, SCM chn_n) 
{
  #define H_y_zoom_slider "(" S_y_zoom_slider " &optional snd chn) -> current y axis zoom slider of snd channel chn"
  return(cp_fread(snd_n, chn_n, CP_AP_ZY, S_y_zoom_slider));
}

static SCM g_set_ap_zy(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(NUMBER_P(on), on, SCM_ARG1, "set-" S_y_zoom_slider, "a number");
  return(cp_fwrite(snd_n, chn_n, on, CP_AP_ZY, "set-" S_y_zoom_slider));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_ap_zy_reversed, g_set_ap_zy)

static SCM g_edit_hook(SCM snd_n, SCM chn_n) 
{
  #define H_edit_hook "(" S_edit_hook " &optional snd chn) -> snd's channel chn's edit-hook"
  return(cp_iread(snd_n, chn_n, CP_EDIT_HOOK, S_edit_hook));
}

static SCM g_undo_hook(SCM snd_n, SCM chn_n) 
{
  #define H_undo_hook "(" S_undo_hook " &optional snd chn) -> snd's channel chn's undo-hook"
  return(cp_iread(snd_n, chn_n, CP_UNDO_HOOK, S_undo_hook));
}

static SCM g_show_y_zero(SCM snd, SCM chn)
{
  #define H_show_y_zero "(" S_show_y_zero " (snd #t) (chn #t)) -> #t if Snd should include a line at y = 0.0"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_SHOW_Y_ZERO, S_show_y_zero));
  return(TO_SCM_BOOLEAN(show_y_zero(get_global_state())));
}

static SCM g_set_show_y_zero(SCM on, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_show_y_zero, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, on, CP_SHOW_Y_ZERO, "set-" S_show_y_zero));
  else
    {
      ss = get_global_state();
      set_show_y_zero(ss, TO_C_BOOLEAN_OR_T(on));
      return(TO_SCM_BOOLEAN(show_y_zero(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_y_zero_reversed, g_set_show_y_zero)

static SCM g_min_dB(SCM snd, SCM chn) 
{
  #define H_min_dB "(" S_min_dB " (snd #t) (chn #t)) -> min dB value displayed in fft graphs using dB scales"
  snd_state *ss;
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_MIN_DB, S_min_dB));
  ss = get_global_state();
  return(TO_SCM_DOUBLE(ss->min_dB));
}

static SCM g_set_min_dB(SCM val, SCM snd, SCM chn) 
{
  Float db;
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_min_dB, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_MIN_DB, "set-" S_min_dB));
  else
    {
      db = TO_C_DOUBLE(val);
      ss = get_global_state();
      ss->min_dB = db;
      ss->lin_dB = pow(10.0, db*0.05);
      cp_fwrite(SCM_BOOL_T, SCM_BOOL_T, val, CP_MIN_DB, "set-" S_min_dB);
      return(TO_SCM_DOUBLE(ss->min_dB));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_min_dB_reversed, g_set_min_dB)

static SCM g_fft_beta(SCM snd, SCM chn) 
{
  #define H_fft_beta "(" S_fft_beta " *optional (snd #t) (chn #t)) -> 'beta' fft data window parameter value (0.0)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_FFT_BETA, S_fft_beta));
  return(TO_SCM_DOUBLE(fft_beta(get_global_state())));
}

static SCM g_set_fft_beta(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_fft_beta, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_FFT_BETA, "set-" S_fft_beta));
  else
    {
      ss = get_global_state();
      set_fft_beta(ss, mus_fclamp(0.0, TO_C_DOUBLE(val), 1.0));
      return(TO_SCM_DOUBLE(fft_beta(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_beta_reversed, g_set_fft_beta)

static SCM g_spectro_cutoff(SCM snd, SCM chn) 
{
  #define H_spectro_cutoff "(" S_spectro_cutoff " *optional (snd #t) (chn #t)) -> amount of frequency shown in spectra (1.0)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_SPECTRO_CUTOFF, S_spectro_cutoff));
  return(TO_SCM_DOUBLE(spectro_cutoff(get_global_state())));
}

static SCM g_set_spectro_cutoff(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_cutoff, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_SPECTRO_CUTOFF, "set-" S_spectro_cutoff));
  else
    {
      ss = get_global_state();
      set_spectro_cutoff(ss, mus_fclamp(0.0, TO_C_DOUBLE(val), 1.0));
      return(TO_SCM_DOUBLE(spectro_cutoff(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_cutoff_reversed, g_set_spectro_cutoff)

static SCM g_spectro_start(SCM snd, SCM chn) 
{
  #define H_spectro_start "(" S_spectro_start " *optional (snd #t) (chn #t)) -> lower bound of frequency in spectral displays (0.0)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_SPECTRO_START, S_spectro_start));
  return(TO_SCM_DOUBLE(spectro_start(get_global_state())));
}

static SCM g_set_spectro_start(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_start, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_SPECTRO_START, "set-" S_spectro_start));
  else
    {
      ss = get_global_state();
      set_spectro_start(ss, mus_fclamp(0.0, TO_C_DOUBLE(val), 1.0));
      return(TO_SCM_DOUBLE(spectro_start(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_start_reversed, g_set_spectro_start)

static SCM g_spectro_x_angle(SCM snd, SCM chn) 
{
  #define H_spectro_x_angle "(" S_spectro_x_angle " *optional (snd #t) (chn #t)) -> spectrogram x-axis viewing angle (90.0)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_SPECTRO_X_ANGLE, S_spectro_x_angle));
  return(TO_SCM_DOUBLE(spectro_x_angle(get_global_state())));
}

static SCM g_set_spectro_x_angle(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_x_angle, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_SPECTRO_X_ANGLE, "set-" S_spectro_x_angle));
  else
    {
      ss = get_global_state();
      set_spectro_x_angle(ss, TO_C_DOUBLE(val));
      return(TO_SCM_DOUBLE(spectro_x_angle(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_x_angle_reversed, g_set_spectro_x_angle)

static SCM g_spectro_x_scale(SCM snd, SCM chn) 
{
  #define H_spectro_x_scale "(" S_spectro_x_scale " *optional (snd #t) (chn #t)) -> scaler (stretch) along the spectrogram x axis (1.0)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_SPECTRO_X_SCALE, S_spectro_x_scale));
  return(TO_SCM_DOUBLE(spectro_x_scale(get_global_state())));
}

static SCM g_set_spectro_x_scale(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_x_scale, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_SPECTRO_X_SCALE, "set-" S_spectro_x_scale));
  else
    {
      ss = get_global_state();
      set_spectro_x_scale(ss, TO_C_DOUBLE(val));
      return(TO_SCM_DOUBLE(spectro_x_scale(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_x_scale_reversed, g_set_spectro_x_scale)

static SCM g_spectro_y_angle(SCM snd, SCM chn) 
{
  #define H_spectro_y_angle "(" S_spectro_y_angle " *optional (snd #t) (chn #t)) -> spectrogram y-axis viewing angle (0.0)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_SPECTRO_Y_ANGLE, S_spectro_y_angle));
  return(TO_SCM_DOUBLE(spectro_y_angle(get_global_state())));
}

static SCM g_set_spectro_y_angle(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_y_angle, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_SPECTRO_Y_ANGLE, "set-" S_spectro_y_angle));
  else
    {
      ss = get_global_state();
      set_spectro_y_angle(ss, TO_C_DOUBLE(val));
      return(TO_SCM_DOUBLE(spectro_y_angle(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_y_angle_reversed, g_set_spectro_y_angle)

static SCM g_spectro_y_scale(SCM snd, SCM chn) 
{
  #define H_spectro_y_scale "(" S_spectro_y_scale " *optional (snd #t) (chn #t)) -> scaler (stretch) along the spectrogram y axis (1.0)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_SPECTRO_Y_SCALE, S_spectro_y_scale));
  return(TO_SCM_DOUBLE(spectro_y_scale(get_global_state())));
}

static SCM g_set_spectro_y_scale(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_y_scale, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_SPECTRO_Y_SCALE, "set-" S_spectro_y_scale));
  else
    {
      ss = get_global_state();
      set_spectro_y_scale(ss, TO_C_DOUBLE(val));
      return(TO_SCM_DOUBLE(spectro_y_scale(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_y_scale_reversed, g_set_spectro_y_scale)

static SCM g_spectro_z_angle(SCM snd, SCM chn) 
{
  #define H_spectro_z_angle "(" S_spectro_z_angle " *optional (snd #t) (chn #t)) -> spectrogram z-axis viewing angle (-2.0)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_SPECTRO_Z_ANGLE, S_spectro_z_angle));
  return(TO_SCM_DOUBLE(spectro_z_angle(get_global_state())));
}

static SCM g_set_spectro_z_angle(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_z_angle, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_SPECTRO_Z_ANGLE, "set-" S_spectro_z_angle));
  else
    {
      ss = get_global_state();
      set_spectro_z_angle(ss, TO_C_DOUBLE(val));
      return(TO_SCM_DOUBLE(spectro_z_angle(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_z_angle_reversed, g_set_spectro_z_angle)

static SCM g_spectro_z_scale(SCM snd, SCM chn) 
{
  #define H_spectro_z_scale "(" S_spectro_z_scale " *optional (snd #t) (chn #t)) -> scaler (stretch) along the spectrogram z axis (0.1)"
  if (BOUND_P(snd))
    return(cp_fread(snd, chn, CP_SPECTRO_Z_SCALE, S_spectro_z_scale));
  return(TO_SCM_DOUBLE(spectro_z_scale(get_global_state())));
}

static SCM g_set_spectro_z_scale(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_z_scale, "a number"); 
  if (BOUND_P(snd))
    return(cp_fwrite(snd, chn, val, CP_SPECTRO_Z_SCALE, "set-" S_spectro_z_scale));
  else
    {
      ss = get_global_state();
      set_spectro_z_scale(ss, TO_C_DOUBLE(val));
      return(TO_SCM_DOUBLE(spectro_z_scale(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_z_scale_reversed, g_set_spectro_z_scale)

static SCM g_spectro_hop(SCM snd, SCM chn)
{
  #define H_spectro_hop "(" S_spectro_hop " (snd #t) (chn #t)) -> hop amount (pixels) in spectral displays"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_SPECTRO_HOP, S_spectro_hop));
  return(TO_SCM_INT(spectro_hop(get_global_state())));
}

static SCM g_set_spectro_hop(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_spectro_hop, "a number"); 
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_SPECTRO_HOP, "set-" S_spectro_hop));
  else
    {
      ss = get_global_state();
      set_spectro_hop(ss, TO_C_INT_OR_ELSE(val, 0));
      return(TO_SCM_INT(spectro_hop(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_spectro_hop_reversed, g_set_spectro_hop)


static SCM g_show_marks(SCM snd, SCM chn)
{
  #define H_show_marks "(" S_show_marks " (snd #t) (chn #t)) -> #t if Snd should show marks"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_SHOW_MARKS, S_show_marks));
  return(TO_SCM_BOOLEAN(show_marks(get_global_state())));
}

static SCM g_set_show_marks(SCM on, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_show_marks, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, on, CP_SHOW_MARKS, "set-" S_show_marks));
  else
    {
      ss = get_global_state();
      set_show_marks(ss, TO_C_BOOLEAN_OR_T(on));
      return(TO_SCM_BOOLEAN(show_marks(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_marks_reversed, g_set_show_marks)

static SCM g_show_fft_peaks(SCM snd, SCM chn)
{
  #define H_show_fft_peaks "(" S_show_fft_peaks " (snd #t) (chn #t)) -> #t if fft display should include peak list"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_SHOW_FFT_PEAKS, S_show_fft_peaks));
  return(TO_SCM_BOOLEAN(show_fft_peaks(get_global_state())));
}

static SCM g_set_show_fft_peaks(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_show_fft_peaks, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_SHOW_FFT_PEAKS, "set-" S_show_fft_peaks));
  else
    {
      ss = get_global_state();
      set_show_fft_peaks(ss, TO_C_BOOLEAN_OR_T(val));
      return(TO_SCM_BOOLEAN(show_fft_peaks(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_fft_peaks_reversed, g_set_show_fft_peaks)

static SCM g_zero_pad(SCM snd, SCM chn)
{
  #define H_zero_pad "(" S_zero_pad " (snd #t) (chn #t)) -> zero padding used in fft as a multiple of fft size (0)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_ZERO_PAD, S_zero_pad));
  return(TO_SCM_INT(zero_pad(get_global_state())));
}

static SCM g_set_zero_pad(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_zero_pad, "a number"); 
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_ZERO_PAD, "set-" S_zero_pad));
  else
    {
      ss = get_global_state();
      set_zero_pad(ss, g_imin(0, val, DEFAULT_ZERO_PAD));
      return(TO_SCM_INT(zero_pad(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_zero_pad_reversed, g_set_zero_pad)

static SCM g_wavelet_type(SCM snd, SCM chn)
{
  #define H_wavelet_type "(" S_wavelet_type " (snd #t) (chn #t)) -> wavelet used in wavelet-transform (0)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_WAVELET_TYPE, S_wavelet_type));
  return(TO_SCM_INT(wavelet_type(get_global_state())));
}

static SCM g_set_wavelet_type(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(INTEGER_P(val), val, SCM_ARG1, "set-" S_wavelet_type, "an integer"); 
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_WAVELET_TYPE, "set-" S_wavelet_type));
  else
    {
      ss = get_global_state();
      set_wavelet_type(ss, mus_iclamp(0, TO_C_INT(val), NUM_WAVELETS-1));
      return(TO_SCM_INT(wavelet_type(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_wavelet_type_reversed, g_set_wavelet_type)

static SCM g_fft_log_frequency(SCM snd, SCM chn)
{
  #define H_fft_log_frequency "(" S_fft_log_frequency " (snd #t) (chn #t)) -> #t if fft displays use log on the frequency axis (#f)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_FFT_LOG_FREQUENCY, S_fft_log_frequency));
  return(TO_SCM_BOOLEAN(fft_log_frequency(get_global_state())));
}

static SCM g_set_fft_log_frequency(SCM on, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_fft_log_frequency, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, on, CP_FFT_LOG_FREQUENCY, "set-" S_fft_log_frequency));
  else
    {
      ss = get_global_state();
      set_fft_log_frequency(ss, TO_C_BOOLEAN_OR_T(on)); 
      return(TO_SCM_BOOLEAN(fft_log_frequency(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_log_frequency_reversed, g_set_fft_log_frequency)

static SCM g_fft_log_magnitude(SCM snd, SCM chn)
{
  #define H_fft_log_magnitude "(" S_fft_log_magnitude " (snd #t) (chn #t)) -> #t if fft displays use dB (#f)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_FFT_LOG_MAGNITUDE, S_fft_log_magnitude));
  return(TO_SCM_BOOLEAN(fft_log_magnitude(get_global_state())));
}

static SCM g_set_fft_log_magnitude(SCM on, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_fft_log_magnitude, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, on, CP_FFT_LOG_MAGNITUDE, "set-" S_fft_log_magnitude));
  else
    {
      ss = get_global_state();
      set_fft_log_magnitude(ss, TO_C_BOOLEAN_OR_T(on)); 
      return(TO_SCM_BOOLEAN(fft_log_magnitude(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_log_magnitude_reversed, g_set_fft_log_magnitude)

static SCM g_show_mix_waveforms(SCM snd, SCM chn)
{
  #define H_show_mix_waveforms "(" S_show_mix_waveforms " (snd #t) (chn #t)) -> #t if Snd should display mix waveforms"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_SHOW_MIX_WAVEFORMS, S_show_mix_waveforms));
  return(TO_SCM_BOOLEAN(show_mix_waveforms(get_global_state())));
}

static SCM g_set_show_mix_waveforms(SCM on, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_show_mix_waveforms, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, on, CP_SHOW_MIX_WAVEFORMS, "set-" S_show_mix_waveforms));
  else
    {
      ss = get_global_state();
      set_show_mix_waveforms(ss, TO_C_BOOLEAN_OR_T(on));
      return(TO_SCM_BOOLEAN(show_mix_waveforms(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_mix_waveforms_reversed, g_set_show_mix_waveforms)

static SCM g_verbose_cursor(SCM snd, SCM chn)
{
  #define H_verbose_cursor "(" S_verbose_cursor " (snd #t) (chn #t)) -> #t if the cursor's position and so on is displayed in the minibuffer"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_VERBOSE_CURSOR, S_verbose_cursor));
  return(TO_SCM_BOOLEAN(verbose_cursor(get_global_state())));
}

static SCM g_set_verbose_cursor(SCM on, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_verbose_cursor, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, on, CP_VERBOSE_CURSOR, "set-" S_verbose_cursor));
  else
    {
      ss = get_global_state();
      set_verbose_cursor(ss, TO_C_BOOLEAN_OR_T(on));
      return(TO_SCM_BOOLEAN(verbose_cursor(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_verbose_cursor_reversed, g_set_verbose_cursor)


static SCM g_wavo(SCM snd, SCM chn)
{
  #define H_wavo "(" S_wavo " (snd #t) (chn #t)) -> #t if Snd's time domain display is a 'wavogram'"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_WAVO, S_wavo));
  return(TO_SCM_BOOLEAN(wavo(get_global_state())));
}

static SCM g_set_wavo(SCM val, SCM snd, SCM chn) 
{
  int on;
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_wavo, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_WAVO, "set-" S_wavo));
  else
    {
      ss = get_global_state();
      on = TO_C_BOOLEAN_OR_T(val);
      set_wavo(ss, on);
      return(TO_SCM_BOOLEAN(wavo(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_wavo_reversed, g_set_wavo)

static SCM g_wavo_hop(SCM snd, SCM chn)
{
  #define H_wavo_hop "(" S_wavo_hop " (snd #t) (chn #t)) -> wavogram spacing between successive traces"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_WAVO_HOP, S_wavo_hop));
  return(TO_SCM_INT(wavo_hop(get_global_state())));
}

static SCM g_set_wavo_hop(SCM val, SCM snd, SCM chn) 
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_wavo_hop, "a number"); 
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_WAVO_HOP, "set-" S_wavo_hop));
  else
    {
      ss = get_global_state();
      set_wavo_hop(ss, TO_C_INT_OR_ELSE(val, 0));
      return(TO_SCM_INT(wavo_hop(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_wavo_hop_reversed, g_set_wavo_hop)

static SCM g_wavo_trace(SCM snd, SCM chn)
{
  #define H_wavo_trace "(" S_wavo_trace " (snd #t) (chn #t)) -> length (samples) of each trace in the wavogram (64)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_WAVO_TRACE, S_wavo_trace));
  return(TO_SCM_INT(wavo_trace(get_global_state())));
}

static SCM g_set_wavo_trace(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG1, "set-" S_wavo_trace, "a number"); 
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_WAVO_TRACE, "set-" S_wavo_trace));
  else
    {
      ss = get_global_state();
      set_wavo_trace(ss, TO_C_INT_OR_ELSE(val, 0));
      return(TO_SCM_INT(wavo_trace(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_wavo_trace_reversed, g_set_wavo_trace)

static SCM g_fft_size(SCM snd, SCM chn)
{
  #define H_fft_size "(" S_fft_size " (snd #t) (chn #t)) -> current fft size (256)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_FFT_SIZE, S_fft_size));
  return(TO_SCM_INT(fft_size(get_global_state())));
}

static SCM g_set_fft_size(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  int len;
  ASSERT_TYPE(INTEGER_P(val), val, SCM_ARG1, "set-" S_fft_size, "an integer"); 
  len = TO_C_INT(val);
  if (len <= 0) return(SCM_BOOL_F);
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_FFT_SIZE, "set-" S_fft_size));
  else
    {
      ss = get_global_state();
      set_fft_size(ss, (int)pow(2, (ceil(log((double)(len))/log(2.0)))));
      return(TO_SCM_INT(fft_size(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_size_reversed, g_set_fft_size)

static SCM g_fft_style(SCM snd, SCM chn)
{
  #define H_fft_style "(" S_fft_style " (snd #t) (chn #t)) -> normal-fft, sonogram, or spectrogram"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_FFT_STYLE, S_fft_style));
  return(TO_SCM_INT(fft_style(get_global_state())));
}

static SCM g_set_fft_style(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  int style;
  ASSERT_TYPE(INTEGER_P(val), val, SCM_ARG1, "set-" S_fft_style, "an integer"); 
  style = mus_iclamp(NORMAL_FFT, TO_C_INT(val), SPECTROGRAM);
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, TO_SMALL_SCM_INT(style), CP_FFT_STYLE, "set-" S_fft_style));
  else
    {
      ss = get_global_state();
      set_fft_style(ss, style);
      return(TO_SCM_INT(fft_style(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_style_reversed, g_set_fft_style)

static SCM g_fft_window(SCM snd, SCM chn)
{
  #define H_fft_window "(" S_fft_window " (snd #t) (chn #t)) -> current fft data window choice (e.g. blackman2-window)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_FFT_WINDOW, S_fft_window));
  return(TO_SCM_INT(fft_window(get_global_state())));
}

static SCM g_set_fft_window(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  int win;
  ASSERT_TYPE(INTEGER_P(val), val, SCM_ARG1, "set-" S_fft_window, "an integer"); 
  win = mus_iclamp(0, TO_C_INT(val), NUM_FFT_WINDOWS - 1);
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, TO_SMALL_SCM_INT(win), CP_FFT_WINDOW, "set-" S_fft_window));
  else
    {
      ss = get_global_state();
      set_fft_window(ss, win);
      return(TO_SCM_INT(fft_window(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_fft_window_reversed, g_set_fft_window)

static SCM g_transform_type(SCM snd, SCM chn)
{
  #define H_transform_type "(" S_transform_type " (snd #t) (chn #t)) -> transform type, e.g. fourier-transform"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_TRANSFORM_TYPE, S_transform_type));
  return(TO_SCM_INT(transform_type(get_global_state())));
}

static SCM g_set_transform_type(SCM val, SCM snd, SCM chn)
{
  int type;
  snd_state *ss;
  ASSERT_TYPE(INTEGER_P(val), val, SCM_ARG1, "set-" S_transform_type, "an integer"); 
  type = mus_iclamp(0, TO_C_INT(val), max_transform_type());
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, TO_SMALL_SCM_INT(type), CP_TRANSFORM_TYPE, "set-" S_transform_type));
  else
    {
      ss = get_global_state();
      set_transform_type(ss, type);
      return(TO_SCM_INT(transform_type(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_transform_type_reversed, g_set_transform_type)

static SCM g_normalize_fft(SCM snd, SCM chn)
{
  #define H_normalize_fft "(" S_normalize_fft " (snd #t) (chn #t)) -> one of '(" S_dont_normalize " " S_normalize_by_channel " " S_normalize_by_sound " " S_normalize_globally ") \
decides whether spectral data is normalized before display (default: " S_normalize_by_channel ")"

  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_NORMALIZE_FFT, S_normalize_fft));
  return(TO_SCM_INT(normalize_fft(get_global_state())));
}

static SCM g_set_normalize_fft(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_normalize_fft, "an integer");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_NORMALIZE_FFT, "set-" S_normalize_fft));
  else
    {
      ss = get_global_state();
      set_normalize_fft(ss, TO_C_BOOLEAN_OR_T(val));
      return(TO_SCM_INT(normalize_fft(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_normalize_fft_reversed, g_set_normalize_fft)

static SCM g_max_fft_peaks(SCM snd, SCM chn)
{
  #define H_max_fft_peaks "(" S_max_fft_peaks " (snd #t) (chn #t)) -> max number of fft peaks reported in fft display"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_MAX_FFT_PEAKS, S_max_fft_peaks));
  return(TO_SCM_INT(max_fft_peaks(get_global_state())));
}

static SCM g_set_max_fft_peaks(SCM n, SCM snd, SCM chn)
{
  int lim;
  snd_state *ss;
  ASSERT_TYPE(INTEGER_P(n), n, SCM_ARG1, "set-" S_max_fft_peaks, "an integer"); 
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, n, CP_MAX_FFT_PEAKS, "set-" S_max_fft_peaks));
  else
    {
      lim = TO_C_INT(n);
      ss = get_global_state();
      if (lim >= 0)
	set_max_fft_peaks(ss, lim);
      return(TO_SCM_INT(max_fft_peaks(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_max_fft_peaks_reversed, g_set_max_fft_peaks)

static SCM g_graph_style(SCM snd, SCM chn)
{
  #define H_graph_style "(" S_graph_style " (snd #t) (chn #t)) -> one of '(" S_graph_lines " " S_graph_dots " " S_graph_dots_and_lines " " S_graph_lollipops " " S_graph_filled ") \
determines how graphs are drawn (default: " S_graph_lines ")"

  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_GRAPH_STYLE, S_graph_style));
  return(TO_SCM_INT(graph_style(get_global_state())));
}

static SCM g_set_graph_style(SCM style, SCM snd, SCM chn)
{
  snd_state *ss;
  int val;
  ASSERT_TYPE(INTEGER_P(style), style, SCM_ARG1, "set-" S_graph_style, "an integer"); 
  val = TO_C_INT(style);
  if (BOUND_P(snd))
    {
      if ((GRAPH_STYLE_OK((val & 0xf))) &&
	  (GRAPH_STYLE_OK(((val >> 8) & 0xf))) &&
	  (GRAPH_STYLE_OK(((val >> 16) & 0xf))))
	return(cp_iwrite(snd, chn, style, CP_GRAPH_STYLE, "set-" S_graph_style));
    }
  else
    {
      if (GRAPH_STYLE_OK(val))
	{
	  ss = get_global_state();
	  set_graph_style(ss, val);
	  return(TO_SCM_INT(graph_style(ss)));
	}
    }
  mus_misc_error("set-" S_graph_style, "invalid style", style);
  return(SCM_BOOL_F);
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_graph_style_reversed, g_set_graph_style)

static SCM g_dot_size(SCM snd, SCM chn)
{
  #define H_dot_size "(" S_dot_size " (snd #t) (chn #t)) -> size in pixels of dots when graphing with dots (1)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_DOT_SIZE, S_dot_size));
  return(TO_SCM_INT(dot_size(get_global_state())));
}

static SCM g_set_dot_size(SCM size, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(NUMBER_P(size), size, SCM_ARG1, "set-" S_dot_size, "a number"); 
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, size, CP_DOT_SIZE, "set-" S_dot_size));
  else
    {
      ss = get_global_state();
      set_dot_size(ss, (Latus)TO_C_INT_OR_ELSE(size, 0));
      return(TO_SCM_INT(dot_size(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_dot_size_reversed, g_set_dot_size)

static SCM g_show_axes(SCM snd, SCM chn)
{
  #define H_show_axes "(" S_show_axes "(snd #t) (chn #t)) -> show-all-axes if Snd should display axes"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_SHOW_AXES, S_show_axes));
  return(TO_SCM_INT(show_axes(get_global_state())));
}

static SCM g_set_show_axes(SCM on, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_show_axes, "an integer");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, on, CP_SHOW_AXES, "set-" S_show_axes));
  else
    {
      ss = get_global_state();
      set_show_axes(ss, mus_iclamp(SHOW_NO_AXES, TO_C_INT_OR_ELSE(on, SHOW_ALL_AXES), SHOW_X_AXIS));
      return(TO_SCM_INT(show_axes(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_show_axes_reversed, g_set_show_axes)

static SCM g_graphs_horizontal(SCM snd, SCM chn)
{
  #define H_graphs_horizontal "(" S_graphs_horizontal " (snd #t) (chn #t)) -> #t if the time domain, fft, and lisp graphs are layed out horizontally (#t)"
  if (BOUND_P(snd))
    return(cp_iread(snd, chn, CP_GRAPHS_HORIZONTAL, S_graphs_horizontal));
  return(TO_SCM_BOOLEAN(graphs_horizontal(get_global_state())));
}

static SCM g_set_graphs_horizontal(SCM val, SCM snd, SCM chn)
{
  snd_state *ss;
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, SCM_ARG1, "set-" S_graphs_horizontal, "a boolean");
  if (BOUND_P(snd))
    return(cp_iwrite(snd, chn, val, CP_GRAPHS_HORIZONTAL, "set-" S_graphs_horizontal));
  else
    {
      ss = get_global_state();
      set_graphs_horizontal(ss, TO_C_BOOLEAN_OR_T(val)); 
      return(TO_SCM_BOOLEAN(graphs_horizontal(ss)));
    }
}

WITH_REVERSED_BOOLEAN_CHANNEL_ARGS(g_set_graphs_horizontal_reversed, g_set_graphs_horizontal)


static SCM g_peaks(SCM filename, SCM snd_n, SCM chn_n)
{
  #define H_peaks "(" S_peaks " &optional filename snd chn) writes current fft peaks data to filename, or \
to the help dialog if filename is omitted"

  chan_info *cp;
  char *name = NULL;
  int err;
  ASSERT_TYPE((STRING_P(filename) || (FALSE_P(filename)) || (NOT_BOUND_P(filename))), filename, SCM_ARG1, S_peaks, "a string or #f");
  SND_ASSERT_CHAN(S_peaks, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_peaks);
  if (STRING_P(filename))
    name = mus_expand_filename(TO_C_STRING(filename));
  else name = NULL;
  err = display_fft_peaks(cp, name);
  if (name) FREE(name);
  if ((STRING_P(filename)) && (err == 0)) 
    return(filename);
  return(SCM_BOOL_F);
}

static SCM g_left_sample(SCM snd_n, SCM chn_n) 
{
  #define H_left_sample "(" S_left_sample " &optional snd chn) -> left sample number in time domain window"
  return(cp_iread(snd_n, chn_n, CP_AP_LOSAMP, S_left_sample));
}

static SCM g_set_left_sample(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(INTEGER_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_left_sample, "an integer");
  return(cp_iwrite(snd_n, chn_n, on, CP_AP_LOSAMP, "set-" S_left_sample));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_left_sample_reversed, g_set_left_sample)

static SCM g_right_sample(SCM snd_n, SCM chn_n) 
{
  #define H_right_sample "(" S_right_sample " &optional snd chn) -> right sample number in time domain window"
  return(cp_iread(snd_n, chn_n, CP_AP_HISAMP, S_right_sample));
}

static SCM g_set_right_sample(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(INTEGER_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_right_sample, "an integer");
  return(cp_iwrite(snd_n, chn_n, on, CP_AP_HISAMP, "set-" S_right_sample));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_right_sample_reversed, g_set_right_sample)

static SCM g_channel_sync(SCM snd_n, SCM chn_n) 
{
  #define H_channel_sync "(" S_channel_sync " &optional snd chn) -> sync field of chn"
  return(cp_iread(snd_n, chn_n, CP_SYNC, S_channel_sync));
}

static SCM g_set_channel_sync(SCM on, SCM snd_n, SCM chn_n) 
{
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(on), on, SCM_ARG1, "set-" S_channel_sync, "an integer");
  return(cp_iwrite(snd_n, chn_n, on, CP_SYNC, "set-" S_channel_sync));
}

WITH_REVERSED_CHANNEL_ARGS(g_set_channel_sync_reversed, g_set_channel_sync)

static SCM g_edits(SCM snd_n, SCM chn_n)
{
  #define H_edits "(" S_edits " &optional snd chn) returns a vector of undoable and redoable edits in snd's channel chn"
  chan_info *cp;
  int i;
  SND_ASSERT_CHAN(S_edits, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_edits);
  for (i = cp->edit_ctr + 1; i < cp->edit_size; i++)
    if (!(cp->edits[i])) break;
  return(SCM_LIST2(TO_SCM_INT(cp->edit_ctr),
		   TO_SCM_INT(i - cp->edit_ctr - 1)));
}

static SCM g_set_x_bounds(SCM bounds, SCM snd_n, SCM chn_n)
{
  chan_info *cp;
  Float x0, x1;
  SND_ASSERT_CHAN("set-" S_x_bounds, snd_n, chn_n, 2);
  ASSERT_TYPE(LIST_P(bounds), bounds, SCM_ARG1, "set-" S_x_bounds, "a list");
  cp = get_cp(snd_n, chn_n, "set-" S_x_bounds);
  x0 = TO_C_DOUBLE(SCM_CAR(bounds));
  x1 = TO_C_DOUBLE(SCM_CADR(bounds));
  if (x1 > x0)
    set_x_axis_x0x1(cp, x0, x1);
  else ERROR(IMPOSSIBLE_BOUNDS,
	     SCM_LIST2(TO_SCM_STRING("set-" S_x_bounds),
		       bounds));
  return(SCM_BOOL_F);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_x_bounds_reversed, g_set_x_bounds)

static SCM g_set_y_bounds(SCM bounds, SCM snd_n, SCM chn_n)
{
  chan_info *cp;
  Float low, hi;
  int len = 0;
  SCM y0 = SCM_UNDEFINED, y1 = SCM_UNDEFINED;
  SND_ASSERT_CHAN("set-" S_y_bounds, snd_n, chn_n, 2);
  ASSERT_TYPE(LIST_P_WITH_LENGTH(bounds, len), bounds, SCM_ARG1, "set-" S_y_bounds, "a list");
  cp = get_cp(snd_n, chn_n, "set-" S_y_bounds);
  if (len > 0)
    {
      y0 = SCM_CAR(bounds);
      if (len > 1)
	y1 = SCM_CADR(bounds);
    }
  if (NUMBER_P(y0))
    {
      low = TO_C_DOUBLE(y0);
      if (NUMBER_P(y1))
	hi = TO_C_DOUBLE(y1);
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
    set_y_axis_y0y1(cp, low, hi);
  else ERROR(IMPOSSIBLE_BOUNDS,
	     SCM_LIST2(TO_SCM_STRING("set-" S_y_bounds),
		       bounds));
  return(SCM_BOOL_F);
}

WITH_REVERSED_CHANNEL_ARGS(g_set_y_bounds_reversed, g_set_y_bounds)

static SCM g_x_bounds(SCM snd_n, SCM chn_n)
{
  #define H_x_bounds "(" S_x_bounds " &optional snd chn) returns a list (x0 x1) giving the current x axis bounds of snd channel chn"
  chan_info *cp;
  axis_info *ap;
  SND_ASSERT_CHAN(S_x_bounds, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_x_bounds);
  ap = cp->axis;
  return(SCM_LIST2(TO_SCM_DOUBLE(ap->x0),
		   TO_SCM_DOUBLE(ap->x1)));
}

static SCM g_y_bounds(SCM snd_n, SCM chn_n)
{
  #define H_y_bounds "(" S_y_bounds " &optional snd chn) returns a list (y0 y1) giving the current y axis bounds of snd channel chn"
  chan_info *cp;
  axis_info *ap;
  SND_ASSERT_CHAN(S_y_bounds, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_y_bounds);
  ap = cp->axis;
  return(SCM_LIST2(TO_SCM_DOUBLE(ap->y0),
		   TO_SCM_DOUBLE(ap->y1)));
}

static SCM g_forward_sample(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_sample "(" S_forward_sample " &optional (count 1) snd chn) moves the cursor forward count samples"
  chan_info *cp;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(count), count, SCM_ARG1, S_forward_sample, "an integer");
  SND_ASSERT_CHAN(S_forward_sample, snd, chn, 2);
  cp = get_cp(snd, chn, S_forward_sample);
  handle_cursor(cp, cursor_move(cp, TO_C_INT_OR_ELSE(count, 1))); 
  return(TO_SCM_INT(cp->cursor));
}

static SCM g_backward_sample(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_sample "(" S_backward_sample " &optional (count 1) snd chn) moves the cursor back count samples"
  chan_info *cp;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(count), count, SCM_ARG1, S_backward_sample, "an integer");
  SND_ASSERT_CHAN(S_backward_sample, snd, chn, 2);
  cp = get_cp(snd, chn, S_backward_sample);
  handle_cursor(cp, cursor_move(cp, -(TO_C_INT_OR_ELSE(count, 1)))); 
  return(TO_SCM_INT(cp->cursor));
}

static void after_fft(snd_state *ss, chan_info *cp, Float scaler)
{
  if ((!(ss->fft_hook_active)) &&
      (HOOKED(fft_hook)))
    {
      ss->fft_hook_active = 1;
      g_c_run_progn_hook(fft_hook,
			 SCM_LIST3(TO_SMALL_SCM_INT((cp->sound)->index),
				   TO_SMALL_SCM_INT(cp->chan),
				   TO_SCM_DOUBLE(scaler)),
			 S_fft_hook);
      ss->fft_hook_active = 0;
    }
}

#if (!USE_NO_GUI)
static SCM g_channel_widgets(SCM snd, SCM chn)
{
  chan_info *cp;
  SND_ASSERT_CHAN(S_channel_widgets, snd, chn, 1);
  cp = get_cp(snd, chn, S_channel_widgets);
  return(CONS(SND_WRAP(channel_graph(cp)),
	      SCM_EOL));
}
#endif

void g_init_chn(SCM local_doc)
{
  cp_edpos = SCM_UNDEFINED;

#if (!USE_NO_GUI)
  DEFINE_PROC(S_channel_widgets,         g_channel_widgets, 0, 2, 0,         "returns channel widgets");
#endif

  DEFINE_PROC(S_edits,                   g_edits, 0, 2, 0,                   H_edits);
  DEFINE_PROC(S_peaks,                   g_peaks, 0, 3, 0,                   H_peaks);
  DEFINE_PROC(S_edit_hook,               g_edit_hook, 0, 2, 0,               H_edit_hook);
  DEFINE_PROC(S_undo_hook,               g_undo_hook, 0, 2, 0,               H_undo_hook);

  define_procedure_with_reversed_setter(S_x_position_slider, SCM_FNC g_ap_sx, H_x_position_slider,
					"set-" S_x_position_slider, SCM_FNC g_set_ap_sx, SCM_FNC g_set_ap_sx_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_y_position_slider, SCM_FNC g_ap_sy, H_y_position_slider,
					"set-" S_y_position_slider, SCM_FNC g_set_ap_sy, SCM_FNC g_set_ap_sy_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_x_zoom_slider, SCM_FNC g_ap_zx, H_x_zoom_slider,
					"set-" S_x_zoom_slider, SCM_FNC g_set_ap_zx, SCM_FNC g_set_ap_zx_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_y_zoom_slider, SCM_FNC g_ap_zy, H_y_zoom_slider,
					"set-" S_y_zoom_slider, SCM_FNC g_set_ap_zy, SCM_FNC g_set_ap_zy_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_frames, SCM_FNC g_frames, H_frames,
					"set-" S_frames, SCM_FNC g_set_frames, SCM_FNC g_set_frames_reversed,
					local_doc, 0, 3, 0, 3);

  define_procedure_with_reversed_setter(S_maxamp, SCM_FNC g_maxamp, H_maxamp,
					"set-" S_maxamp, SCM_FNC g_set_maxamp, SCM_FNC g_set_maxamp_reversed,
					local_doc, 0, 3, 0, 3);

  DEFINE_PROC(S_forward_sample,    g_forward_sample, 0, 3, 0,    H_forward_sample);
  DEFINE_PROC(S_backward_sample,   g_backward_sample, 0, 3, 0,   H_backward_sample);
  DEFINE_PROC(S_cursor_position,   g_cursor_position, 0, 2, 0,   H_cursor_position);

  define_procedure_with_reversed_setter(S_edit_position, SCM_FNC g_edit_position, H_edit_position,
					"set-" S_edit_position, SCM_FNC g_set_edit_position, SCM_FNC g_set_edit_position_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_ffting, SCM_FNC g_ffting, H_ffting,
					"set-" S_ffting, SCM_FNC g_set_ffting, SCM_FNC g_set_ffting_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_waving, SCM_FNC g_waving, H_waving,
					"set-" S_waving, SCM_FNC g_set_waving, SCM_FNC g_set_waving_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_graphing, SCM_FNC g_graphing, H_graphing,
					"set-" S_graphing, SCM_FNC g_set_graphing, SCM_FNC g_set_graphing_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_squelch_update, SCM_FNC g_squelch_update, H_squelch_update,
					"set-" S_squelch_update, SCM_FNC g_set_squelch_update, SCM_FNC g_set_squelch_update_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_cursor, SCM_FNC g_cursor, H_cursor,
					"set-" S_cursor, SCM_FNC g_set_cursor, SCM_FNC g_set_cursor_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_cursor_style, SCM_FNC g_cursor_style, H_cursor_style,
					"set-" S_cursor_style, SCM_FNC g_set_cursor_style, SCM_FNC g_set_cursor_style_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_cursor_size, SCM_FNC g_cursor_size, H_cursor_size,
					"set-" S_cursor_size, SCM_FNC g_set_cursor_size, SCM_FNC g_set_cursor_size_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_left_sample, SCM_FNC g_left_sample, H_left_sample,
					"set-" S_left_sample, SCM_FNC g_set_left_sample, SCM_FNC g_set_left_sample_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_right_sample, SCM_FNC g_right_sample, H_right_sample,
					"set-" S_right_sample, SCM_FNC g_set_right_sample, SCM_FNC g_set_right_sample_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_channel_sync, SCM_FNC g_channel_sync, H_channel_sync,
					"set-" S_channel_sync, SCM_FNC g_set_channel_sync, SCM_FNC g_set_channel_sync_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_max_fft_peaks, SCM_FNC g_max_fft_peaks, H_max_fft_peaks,
					"set-" S_max_fft_peaks, SCM_FNC g_set_max_fft_peaks, SCM_FNC g_set_max_fft_peaks_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_show_y_zero, SCM_FNC g_show_y_zero, H_show_y_zero,
					"set-" S_show_y_zero, SCM_FNC g_set_show_y_zero, SCM_FNC g_set_show_y_zero_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_show_marks, SCM_FNC g_show_marks, H_show_marks,
					"set-" S_show_marks, SCM_FNC g_set_show_marks, SCM_FNC g_set_show_marks_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_wavo, SCM_FNC g_wavo, H_wavo,
					"set-" S_wavo, SCM_FNC g_set_wavo, SCM_FNC g_set_wavo_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_wavo_hop, SCM_FNC g_wavo_hop, H_wavo_hop,
					"set-" S_wavo_hop, SCM_FNC g_set_wavo_hop, SCM_FNC g_set_wavo_hop_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_wavo_trace, SCM_FNC g_wavo_trace, H_wavo_trace,
					"set-" S_wavo_trace, SCM_FNC g_set_wavo_trace, SCM_FNC g_set_wavo_trace_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_show_fft_peaks, SCM_FNC g_show_fft_peaks, H_show_fft_peaks,
					"set-" S_show_fft_peaks, SCM_FNC g_set_show_fft_peaks, SCM_FNC g_set_show_fft_peaks_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_zero_pad, SCM_FNC g_zero_pad, H_zero_pad,
					"set-" S_zero_pad, SCM_FNC g_set_zero_pad, SCM_FNC g_set_zero_pad_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_verbose_cursor, SCM_FNC g_verbose_cursor, H_verbose_cursor,
					"set-" S_verbose_cursor, SCM_FNC g_set_verbose_cursor, SCM_FNC g_set_verbose_cursor_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_fft_log_frequency, SCM_FNC g_fft_log_frequency, H_fft_log_frequency,
					"set-" S_fft_log_frequency, SCM_FNC g_set_fft_log_frequency, SCM_FNC g_set_fft_log_frequency_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_fft_log_magnitude, SCM_FNC g_fft_log_magnitude, H_fft_log_magnitude,
					"set-" S_fft_log_magnitude, SCM_FNC g_set_fft_log_magnitude, SCM_FNC g_set_fft_log_magnitude_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_min_dB, SCM_FNC g_min_dB, H_min_dB,
					"set-" S_min_dB, SCM_FNC g_set_min_dB, SCM_FNC g_set_min_dB_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_wavelet_type, SCM_FNC g_wavelet_type, H_wavelet_type,
					"set-" S_wavelet_type, SCM_FNC g_set_wavelet_type, SCM_FNC g_set_wavelet_type_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_cutoff, SCM_FNC g_spectro_cutoff, H_spectro_cutoff,
					"set-" S_spectro_cutoff, SCM_FNC g_set_spectro_cutoff, SCM_FNC g_set_spectro_cutoff_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_start, SCM_FNC g_spectro_start, H_spectro_start,
					"set-" S_spectro_start, SCM_FNC g_set_spectro_start, SCM_FNC g_set_spectro_start_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_x_angle, SCM_FNC g_spectro_x_angle, H_spectro_x_angle,
					"set-" S_spectro_x_angle, SCM_FNC g_set_spectro_x_angle, SCM_FNC g_set_spectro_x_angle_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_x_scale, SCM_FNC g_spectro_x_scale, H_spectro_x_scale,
					"set-" S_spectro_x_scale, SCM_FNC g_set_spectro_x_scale, SCM_FNC g_set_spectro_x_scale_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_y_angle, SCM_FNC g_spectro_y_angle, H_spectro_y_angle,
					"set-" S_spectro_y_angle, SCM_FNC g_set_spectro_y_angle, SCM_FNC g_set_spectro_y_angle_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_y_scale, SCM_FNC g_spectro_y_scale, H_spectro_y_scale,
					"set-" S_spectro_y_scale, SCM_FNC g_set_spectro_y_scale, SCM_FNC g_set_spectro_y_scale_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_z_angle, SCM_FNC g_spectro_z_angle, H_spectro_z_angle,
					"set-" S_spectro_z_angle, SCM_FNC g_set_spectro_z_angle, SCM_FNC g_set_spectro_z_angle_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_z_scale, SCM_FNC g_spectro_z_scale, H_spectro_z_scale,
					"set-" S_spectro_z_scale, SCM_FNC g_set_spectro_z_scale, SCM_FNC g_set_spectro_z_scale_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_fft_beta, SCM_FNC g_fft_beta, H_fft_beta,
					"set-" S_fft_beta, SCM_FNC g_set_fft_beta, SCM_FNC g_set_fft_beta_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_spectro_hop, SCM_FNC g_spectro_hop, H_spectro_hop,
					"set-" S_spectro_hop, SCM_FNC g_set_spectro_hop, SCM_FNC g_set_spectro_hop_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_fft_size, SCM_FNC g_fft_size, H_fft_size,
					"set-" S_fft_size, SCM_FNC g_set_fft_size, SCM_FNC g_set_fft_size_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_fft_style, SCM_FNC g_fft_style, H_fft_style,
					"set-" S_fft_style, SCM_FNC g_set_fft_style, SCM_FNC g_set_fft_style_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_fft_window, SCM_FNC g_fft_window, H_fft_window,
					"set-" S_fft_window, SCM_FNC g_set_fft_window, SCM_FNC g_set_fft_window_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_transform_type, SCM_FNC g_transform_type, H_transform_type,
					"set-" S_transform_type, SCM_FNC g_set_transform_type, SCM_FNC g_set_transform_type_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_normalize_fft, SCM_FNC g_normalize_fft, H_normalize_fft,
					"set-" S_normalize_fft, SCM_FNC g_set_normalize_fft, SCM_FNC g_set_normalize_fft_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_show_mix_waveforms, SCM_FNC g_show_mix_waveforms, H_show_mix_waveforms,
					"set-" S_show_mix_waveforms, SCM_FNC g_set_show_mix_waveforms, SCM_FNC g_set_show_mix_waveforms_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_graph_style, SCM_FNC g_graph_style, H_graph_style,
					"set-" S_graph_style, SCM_FNC g_set_graph_style, SCM_FNC g_set_graph_style_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_dot_size, SCM_FNC g_dot_size, H_dot_size,
					"set-" S_dot_size, SCM_FNC g_set_dot_size, SCM_FNC g_set_dot_size_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_show_axes, SCM_FNC g_show_axes, H_show_axes,
					"set-" S_show_axes, SCM_FNC g_set_show_axes, SCM_FNC g_set_show_axes_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_graphs_horizontal, SCM_FNC g_graphs_horizontal, H_graphs_horizontal,
					"set-" S_graphs_horizontal, SCM_FNC g_set_graphs_horizontal, SCM_FNC g_set_graphs_horizontal_reversed,
					local_doc, 0, 2, 0, 3);

  define_procedure_with_reversed_setter(S_x_bounds, SCM_FNC g_x_bounds, H_x_bounds,
					"set-" S_x_bounds, SCM_FNC g_set_x_bounds, SCM_FNC g_set_x_bounds_reversed,
					local_doc, 0, 2, 1, 2);

  define_procedure_with_reversed_setter(S_y_bounds, SCM_FNC g_y_bounds, H_y_bounds,
					"set-" S_y_bounds, SCM_FNC g_set_y_bounds, SCM_FNC g_set_y_bounds_reversed,
					local_doc, 0, 2, 1, 2);

  #define H_fft_hook S_fft_hook " (snd chn scaler) is called just after a spectrum is calculated."
  #define H_graph_hook S_graph_hook " (snd chn y0 y1) is called each time a graph is about to be updated. If it returns #t, the display is not updated."
  #define H_after_graph_hook S_after_graph_hook " (snd chn) is called after a graph is updated."
  #define H_lisp_graph_hook S_lisp_graph_hook " (snd chn) is called just before the lisp graph is updated."
  #define H_mouse_press_hook S_mouse_press_hook " (snd chn button state x y) is called upon mouse button press within the lisp graph."
  #define H_mouse_release_hook S_mouse_press_hook " (snd chn button state x y) is called upon mouse button release within the lisp graph."
  #define H_mouse_drag_hook S_mouse_press_hook " (snd chn button state x y) is called upon mouse drag within the lisp graph."
  #define H_mark_click_hook S_mark_click_hook " (id) is called when a mark is clicked; return #t to squelch the default message."
  #define H_key_press_hook S_key_press_hook " (snd chn key state) is called upon a key press if the mouse is in the lisp graph. \
If it returns #t, the key press is not passed to the main handler. 'state' refers to the control, meta, and shift keys."
  #define H_initial_graph_hook S_initial_graph_hook " (snd chn dur) is called when a sound is displayed for the first time"

  fft_hook =           MAKE_HOOK(S_fft_hook, 3, H_fft_hook);                     /* args = sound channel scaler */
  graph_hook =         MAKE_HOOK(S_graph_hook, 4, H_graph_hook);                 /* args = sound channel y0 y1 */
  after_graph_hook =   MAKE_HOOK(S_after_graph_hook, 2, H_after_graph_hook);     /* args = sound channel */
  lisp_graph_hook =    MAKE_HOOK(S_lisp_graph_hook, 2, H_lisp_graph_hook);       /* args = sound channel */
  mouse_press_hook =   MAKE_HOOK(S_mouse_press_hook, 6, H_mouse_press_hook);     /* args = sound channel button state x y */
  mouse_release_hook = MAKE_HOOK(S_mouse_release_hook, 6, H_mouse_release_hook); /* args = sound channel button state x y */
  mouse_drag_hook =    MAKE_HOOK(S_mouse_drag_hook, 6, H_mouse_drag_hook);       /* args = sound channel button state x y */
  key_press_hook =     MAKE_HOOK(S_key_press_hook, 4, H_key_press_hook);         /* args = sound channel key state */
  mark_click_hook =    MAKE_HOOK(S_mark_click_hook, 1, H_mark_click_hook);       /* arg = id */
  initial_graph_hook = MAKE_HOOK(S_initial_graph_hook, 3, H_initial_graph_hook); /* args = sound channel duration */
}


