#include "snd.h"
#include "vct.h"
#include "clm2scm.h"

#if HAVE_LOCALE_H
  #include <locale.h>
#endif

static int remove_temp_files(chan_info *cp, void *ignore)
{
  free_sound_list(cp);
  free_mix_list(cp);
  return(0);
}

#ifdef DEBUG_MEMORY
  void mem_report(void);
#endif

void snd_exit_cleanly(snd_state *ss)
{  
  if ((save_state_on_exit(ss)) && (save_state_file(ss)))
    save_state(ss, save_state_file(ss));
  mus_sound_finalize();
  cleanup_dac();
  map_over_chans(ss, remove_temp_files, NULL);
  cleanup_region_temp_files();
  cleanup_recording();
  forget_temps();
#ifdef DEBUG_MEMORY
  mem_report();
#endif
}

int snd_not_current(snd_info *sp, void *ignore)
{
  /* check for change in update status */
  int needs_update;
  snd_state *ss;
  needs_update = (file_write_date(sp->fullname) != sp->write_date);
  if (needs_update != sp->need_update)
    {
      ss = sp->state;
      sp->need_update = needs_update;
      if ((needs_update) && (auto_update(ss)))
	snd_update(ss, sp);
      else snd_file_bomb_icon(sp, needs_update);
    }
  return(0);
}


/* ---------------- save sound state (options, or entire state) ---------------- */

static int fneq(Float a, Float b)
{
  /* floating point != replacement */
  return(fabs(a - b) > .00001);
}

static char *show_axes2string(int ax)
{
  switch (ax)
    {
    case SHOW_NO_AXES: return("show-no-axes"); break;
    case SHOW_X_AXIS: return("show-x-axis"); break;
    default: return("show-all-axes"); break;
    }
}

static char *zoom_focus_style_name(int choice)
{
  switch (choice)
    {
    case FOCUS_LEFT: return(S_focus_left); break;
    case FOCUS_RIGHT: return(S_focus_right); break;
    case FOCUS_MIDDLE: return(S_focus_middle); break;
    default: return(S_focus_active); break;
    }
}

static char *normalize_fft_name(int choice)
{
  switch (choice)
    {
    case DONT_NORMALIZE: return(S_dont_normalize); break;
    case NORMALIZE_BY_CHANNEL:return(S_normalize_by_channel); break;
    case NORMALIZE_BY_SOUND: return(S_normalize_by_sound); break;
    case NORMALIZE_GLOBALLY:return(S_normalize_globally); break;
    default:return(S_normalize_by_channel); break;
    }
}

static char *graph_style_name(int choice)
{
  switch (choice)
    {
    case GRAPH_DOTS: return(S_graph_dots); break;
    case GRAPH_DOTS_AND_LINES: return(S_graph_dots_and_lines); break;
    case GRAPH_LOLLIPOPS: return(S_graph_lollipops); break;
    case GRAPH_FILLED: return(S_graph_filled); break;
    case GRAPH_LINES: 
    default: return(S_graph_lines); break;
    }
}

static char *fft_style_name(int choice)
{
  switch (choice)
    {
    case SONOGRAM: return(S_sonogram); break;
    case SPECTROGRAM: return(S_spectrogram); break;
    default: return(S_normal_fft); break;
    }
}

static char *x_axis_style_name(int choice)
{
  switch(choice)
    {
    case X_IN_SAMPLES: return(S_x_in_samples); break;
    case X_TO_ONE: return(S_x_to_one); break;
    default: return(S_x_in_seconds); break;
    }
}

static char *speed_style_name(int choice)
{
  switch (choice)
    {
    case SPEED_AS_RATIO: return(S_speed_as_ratio); break;
    case SPEED_AS_SEMITONE: return(S_speed_as_semitone); break;
    default: return(S_speed_as_float); break;
    }
}

static char *channel_style_name(int choice)
{
  switch (choice)
    {
    case CHANNELS_COMBINED: return(S_channels_combined); break;
    case CHANNELS_SUPERIMPOSED: return(S_channels_superimposed); break;
    default: return(S_channels_separate); break;
    }
}

static char *enved_target_name(int choice)
{
  switch (choice)
    {
    case SPECTRUM_ENV: return(S_spectrum_env); break;
    case SRATE_ENV: return(S_srate_env); break;
    default: return(S_amplitude_env); break;
    }
}

static char *b2s(int val) {if (val) return("#t"); else return("#f");}

#define white_space "      "

static void pss_ss(FILE *fd, char *name, char *val) {fprintf(fd, "(set! (%s) %s)\n", name, val);}
static void pss_sq(FILE *fd, char *name, char *val) {fprintf(fd, "(set! (%s) \"%s\")\n", name, val);}
static void pss_sd(FILE *fd, char *name, int val) {fprintf(fd, "(set! (%s) %d)\n", name, val);}
static void pss_sf(FILE *fd, char *name, Float val) {fprintf(fd, "(set! (%s) %.4f)\n", name, val);}

static void psp_ss(FILE *fd, char *name, char *val) {fprintf(fd, "%s(set! (%s sfile) %s)\n", white_space, name, val);}
static void psp_sd(FILE *fd, char *name, int val) {fprintf(fd, "%s(set! (%s sfile) %d)\n", white_space, name, val);}
static void psp_sf(FILE *fd, char *name, Float val) {fprintf(fd, "%s(set! (%s sfile) %.4f)\n", white_space, name, val);}

static void pcp_ss(FILE *fd, char *name, char *val, int chan) {fprintf(fd, "%s(set! (%s sfile %d) %s)\n", white_space, name, chan, val);}
static void pcp_sd(FILE *fd, char *name, int val, int chan) {fprintf(fd, "%s(set! (%s sfile %d) %d)\n", white_space, name, chan, val);}
static void pcp_sf(FILE *fd, char *name, Float val, int chan) {fprintf(fd, "%s(set! (%s sfile %d) %.4f)\n", white_space, name, chan, val);}
static void pcp_sl(FILE *fd, char *name, Float val1, Float val2, int chan) {fprintf(fd, "%s(set! (%s sfile %d) (list %.4f %.4f))\n", white_space, name, chan, val1, val2);}

static void save_snd_state_options (snd_state *ss, FILE *fd)
{ /* for save options menu choice (.snd) -- mostly saving snd_state info */
  time_t ts;
  char time_buf[TIME_STR_SIZE];
  char *locale = NULL;

#if HAVE_SETLOCALE
  locale = copy_string(setlocale(LC_NUMERIC, "C")); /* must use decimal point in floats since Scheme assumes that format */
#endif

#if HAVE_STRFTIME
  time(&ts);
  strftime(time_buf, TIME_STR_SIZE, STRFTIME_FORMAT, localtime(&ts));
  fprintf(fd, "\n;;; Snd %s (%s) options saved %s\n", SND_RPM_VERSION, SND_VERSION, time_buf);
#else
  fprintf(fd, "\n;;; Snd %s (%s)\n", SND_RPM_VERSION, SND_VERSION);
#endif

  if (fft_size(ss) != DEFAULT_FFT_SIZE) pss_sd(fd, S_fft_size, fft_size(ss));
  if (minibuffer_history_length(ss) != DEFAULT_MINIBUFFER_HISTORY_LENGTH) pss_sd(fd, S_minibuffer_history_length, minibuffer_history_length(ss));
  if (fft_window(ss) != DEFAULT_FFT_WINDOW) pss_ss(fd, S_fft_window, mus_fft_window_name(fft_window(ss)));
  if (fft_style(ss) != DEFAULT_FFT_STYLE) pss_ss(fd, S_fft_style, fft_style_name(fft_style(ss)));
  if (x_axis_style(ss) != DEFAULT_AXIS_STYLE) pss_ss(fd, S_x_axis_style, x_axis_style_name(x_axis_style(ss)));
  if (graph_style(ss) != DEFAULT_GRAPH_STYLE) pss_ss(fd, S_graph_style, graph_style_name(graph_style(ss)));
  if (speed_style(ss) != DEFAULT_SPEED_STYLE) pss_ss(fd, S_speed_style, speed_style_name(speed_style(ss)));
  if (channel_style(ss) != DEFAULT_CHANNEL_STYLE) pss_ss(fd, S_channel_style, channel_style_name(channel_style(ss)));
  if (enved_target(ss) != DEFAULT_ENVED_TARGET) pss_ss(fd, S_enved_target, enved_target_name(enved_target(ss)));
  if (transform_type(ss) != DEFAULT_TRANSFORM_TYPE) pss_ss(fd, S_transform_type, transform_type_name(transform_type(ss)));
  if (zoom_focus_style(ss) != FOCUS_ACTIVE) pss_ss(fd, S_zoom_focus_style, zoom_focus_style_name(zoom_focus_style(ss)));
  if (normalize_fft(ss) != DEFAULT_NORMALIZE_FFT) pss_ss(fd, S_normalize_fft, normalize_fft_name(normalize_fft(ss)));
  if (trap_segfault(ss) != DEFAULT_TRAP_SEGFAULT) pss_ss(fd, S_trap_segfault, b2s(trap_segfault(ss)));
  if (show_selection_transform(ss) != DEFAULT_SHOW_SELECTION_TRANSFORM) pss_ss(fd, S_show_selection_transform, b2s(show_selection_transform(ss)));
  if (with_mix_tags(ss) != DEFAULT_WITH_MIX_TAGS) pss_ss(fd, S_with_mix_tags, b2s(with_mix_tags(ss)));
  if (sinc_width(ss) != DEFAULT_SINC_WIDTH) pss_sd(fd, S_sinc_width, sinc_width(ss));
  if (speed_tones(ss) != DEFAULT_SPEED_TONES) pss_sd(fd, S_speed_tones, speed_tones(ss));
  if (ss->init_window_width != DEFAULT_INIT_WINDOW_WIDTH) pss_sd(fd, S_window_width, widget_width(MAIN_SHELL(ss)));
  if (ss->init_window_height != DEFAULT_INIT_WINDOW_HEIGHT) pss_sd(fd, S_window_height, widget_height(MAIN_SHELL(ss)));
  if (ss->init_window_x != DEFAULT_INIT_WINDOW_X) pss_sd(fd, S_window_x, widget_x(MAIN_SHELL(ss)));
  if (ss->init_window_y != DEFAULT_INIT_WINDOW_Y) pss_sd(fd, S_window_y, widget_y(MAIN_SHELL(ss)));
  if (default_output_chans(ss) != DEFAULT_OUTPUT_CHANS) pss_sd(fd, S_default_output_chans, default_output_chans(ss));
  if (default_output_srate(ss) != DEFAULT_OUTPUT_SRATE) pss_sd(fd, S_default_output_srate, default_output_srate(ss));
  if (default_output_type(ss) != DEFAULT_OUTPUT_TYPE) pss_sd(fd, S_default_output_type, default_output_type(ss));
  if (default_output_format(ss) != DEFAULT_OUTPUT_FORMAT) pss_sd(fd, S_default_output_format, default_output_format(ss));
  if (auto_resize(ss) != DEFAULT_AUTO_RESIZE) pss_ss(fd, S_auto_resize, b2s(auto_resize(ss)));
  if (graphs_horizontal(ss) != DEFAULT_GRAPHS_HORIZONTAL) pss_ss(fd, S_graphs_horizontal, b2s(graphs_horizontal(ss)));
  if (auto_update(ss) != DEFAULT_AUTO_UPDATE) pss_ss(fd, S_auto_update, b2s(auto_update(ss)));
  if (color_inverted(ss) != DEFAULT_COLOR_INVERTED) pss_ss(fd, S_color_inverted, b2s(color_inverted(ss)));
  if (zero_pad(ss) != DEFAULT_ZERO_PAD) pss_sd(fd, S_zero_pad, zero_pad(ss));
  if (ask_before_overwrite(ss) != DEFAULT_ASK_BEFORE_OVERWRITE) pss_ss(fd, S_ask_before_overwrite, b2s(ask_before_overwrite(ss)));
  if (dac_folding(ss) != DEFAULT_DAC_FOLDING) pss_ss(fd, S_dac_folding, b2s(dac_folding(ss)));
  if (wavo(ss) != DEFAULT_WAVO) pss_ss(fd, S_wavo, b2s(wavo(ss)));
  if (wavo_hop(ss) != DEFAULT_WAVO_HOP) pss_sd(fd, S_wavo_hop, wavo_hop(ss));
  if (wavo_trace(ss) != DEFAULT_WAVO_TRACE) pss_sd(fd, S_wavo_trace, wavo_trace(ss));
  if (spectro_hop(ss) != DEFAULT_SPECTRO_HOP) pss_sd(fd, S_spectro_hop, spectro_hop(ss));
  if (color_map(ss) != DEFAULT_COLOR_MAP) pss_sd(fd, S_colormap, color_map(ss));
  if (wavelet_type(ss) != DEFAULT_WAVELET_TYPE) pss_sd(fd, S_wavelet_type, wavelet_type(ss));
  if (dot_size(ss) != DEFAULT_DOT_SIZE) pss_sd(fd, S_dot_size, dot_size(ss));
  if (dac_size(ss) != DEFAULT_DAC_SIZE) pss_sd(fd, S_dac_size, dac_size(ss));
  if (movies(ss) != DEFAULT_MOVIES) pss_ss(fd, S_movies, b2s(movies(ss)));
  if (selection_creates_region(ss) != DEFAULT_SELECTION_CREATES_REGION) pss_ss(fd, S_selection_creates_region, b2s(selection_creates_region(ss)));
  if (fit_data_on_open(ss) != DEFAULT_FIT_DATA_ON_OPEN) pss_ss(fd, S_fit_data_on_open, b2s(fit_data_on_open(ss)));
  if (save_state_on_exit(ss) != DEFAULT_SAVE_STATE_ON_EXIT) pss_ss(fd, S_save_state_on_exit, b2s(save_state_on_exit(ss)));
  if (filter_env_order(ss) != DEFAULT_FILTER_ENV_ORDER) pss_sd(fd, S_filter_env_order, filter_env_order(ss));
  if (filter_env_in_hz(ss) != DEFAULT_FILTER_ENV_IN_HZ) pss_ss(fd, S_filter_env_in_hz, b2s(filter_env_in_hz(ss)));
  if (max_fft_peaks(ss) != DEFAULT_MAX_FFT_PEAKS) pss_sd(fd, S_max_fft_peaks, max_fft_peaks(ss));
  if (max_regions(ss) != DEFAULT_MAX_REGIONS) pss_sd(fd, S_max_regions, max_regions(ss));
  if (corruption_time(ss) != DEFAULT_CORRUPTION_TIME) pss_sf(fd, S_corruption_time, corruption_time(ss));
  if (verbose_cursor(ss) != DEFAULT_VERBOSE_CURSOR) pss_ss(fd, S_verbose_cursor, b2s(verbose_cursor(ss)));
  if (show_indices(ss) != DEFAULT_SHOW_INDICES) pss_ss(fd, S_show_indices, b2s(show_indices(ss)));
  if (show_backtrace(ss) != DEFAULT_SHOW_BACKTRACE) pss_ss(fd, S_show_backtrace, b2s(show_backtrace(ss)));
  if (show_fft_peaks(ss) != DEFAULT_SHOW_FFT_PEAKS) pss_ss(fd, S_show_fft_peaks, b2s(show_fft_peaks(ss)));
  if (show_y_zero(ss) != DEFAULT_SHOW_Y_ZERO) pss_ss(fd, S_show_y_zero, b2s(show_y_zero(ss)));
  if (show_axes(ss) != DEFAULT_SHOW_AXES) pss_ss(fd, S_show_axes, show_axes2string(show_axes(ss)));
  if (show_marks(ss) != DEFAULT_SHOW_MARKS) pss_ss(fd, S_show_marks, b2s(show_marks(ss)));
  if (use_sinc_interp(ss) != DEFAULT_USE_SINC_INTERP) pss_ss(fd, S_use_sinc_interp, b2s(use_sinc_interp(ss)));
  if (data_clipped(ss) != DEFAULT_DATA_CLIPPED) pss_ss(fd, S_data_clipped, b2s(data_clipped(ss)));
  if (previous_files_sort(ss) != DEFAULT_PREVIOUS_FILES_SORT) pss_sd(fd, S_previous_files_sort, previous_files_sort(ss));
  if (fft_log_magnitude(ss) != DEFAULT_FFT_LOG_MAGNITUDE) pss_ss(fd, S_fft_log_magnitude, b2s(fft_log_magnitude(ss)));
  if (fft_log_frequency(ss) != DEFAULT_FFT_LOG_FREQUENCY) pss_ss(fd, S_fft_log_frequency, b2s(fft_log_frequency(ss)));
  if (print_length(ss) != DEFAULT_PRINT_LENGTH) pss_sd(fd, S_print_length, print_length(ss));
  if (show_usage_stats(ss) != DEFAULT_SHOW_USAGE_STATS) pss_ss(fd, S_show_usage_stats, b2s(show_usage_stats(ss)));
  if (show_mix_waveforms(ss) != DEFAULT_SHOW_MIX_WAVEFORMS) pss_ss(fd, S_show_mix_waveforms, b2s(show_mix_waveforms(ss)));
  if (mix_waveform_height(ss) != DEFAULT_MIX_WAVEFORM_HEIGHT) pss_sd(fd, S_mix_waveform_height, mix_waveform_height(ss));
  if (mix_tag_height(ss) != DEFAULT_MIX_TAG_HEIGHT) pss_sd(fd, S_mix_tag_height, mix_tag_height(ss));
  if (mix_tag_width(ss) != DEFAULT_MIX_TAG_WIDTH) pss_sd(fd, S_mix_tag_width, mix_tag_width(ss));
  if (enved_waving(ss) != DEFAULT_ENVED_WAVING) pss_ss(fd, S_enved_waving, b2s(enved_waving(ss)));
  if (enved_dBing(ss) != DEFAULT_ENVED_DBING) pss_ss(fd, S_enved_dBing, b2s(enved_dBing(ss)));
  if (enved_clipping(ss) != DEFAULT_ENVED_CLIPPING) pss_ss(fd, S_enved_clipping, b2s(enved_clipping(ss)));
  if (enved_exping(ss) != DEFAULT_ENVED_EXPING) pss_ss(fd, S_enved_exping, b2s(enved_exping(ss)));

  if (vu_font(ss) != DEFAULT_VU_FONT) pss_sq(fd, S_vu_font, vu_font(ss));
  if (save_state_file(ss) != NULL) pss_sq(fd, S_save_state_file, save_state_file(ss));
  if (temp_dir(ss) != DEFAULT_TEMP_DIR) pss_sq(fd, S_temp_dir, temp_dir(ss));
  if (save_dir(ss) != DEFAULT_SAVE_DIR) pss_sq(fd, S_save_dir, save_dir(ss));
  if ((eps_file(ss) != DEFAULT_EPS_FILE) && (strcmp(eps_file(ss), "snd.eps") != 0)) pss_sq(fd, S_eps_file, eps_file(ss));
  if (strcmp(listener_prompt(ss), DEFAULT_LISTENER_PROMPT) != 0) pss_sq(fd, S_listener_prompt, listener_prompt(ss));
  if ((audio_state_file(ss) != NULL) && (strcmp(audio_state_file(ss), AUDIO_STATE_FILE) != 0)) pss_sq(fd, S_audio_state_file, audio_state_file(ss));
  if (audio_input_device(ss) != DEFAULT_AUDIO_INPUT_DEVICE) pss_sd(fd, S_audio_input_device, audio_input_device(ss));
  if (audio_output_device(ss) != DEFAULT_AUDIO_OUTPUT_DEVICE) pss_sd(fd, S_audio_output_device, audio_output_device(ss));

  if (fneq(fft_beta(ss), DEFAULT_FFT_BETA)) pss_sf(fd, S_fft_beta, fft_beta(ss));
  if (fneq(reverb_decay(ss), DEFAULT_REVERB_DECAY)) pss_sf(fd, S_reverb_decay, reverb_decay(ss));
  if (fneq(ss->min_dB, DEFAULT_MIN_DB)) pss_sf(fd, S_min_dB, ss->min_dB);
  if (fneq(ss->Hankel_Jn, DEFAULT_HANKEL_JN)) pss_sf(fd, S_hankel_jn, ss->Hankel_Jn);
  if (fneq(color_cutoff(ss), DEFAULT_COLOR_CUTOFF)) pss_sf(fd, S_color_cutoff, color_cutoff(ss));
  if (fneq(color_scale(ss), DEFAULT_COLOR_SCALE)) pss_sf(fd, S_color_scale, color_scale(ss));
  if (fneq(spectro_x_scale(ss), DEFAULT_SPECTRO_X_SCALE)) pss_sf(fd, S_spectro_x_scale, spectro_x_scale(ss));
  if (fneq(spectro_y_scale(ss), DEFAULT_SPECTRO_Y_SCALE)) pss_sf(fd, S_spectro_y_scale, spectro_y_scale(ss));
  if (fneq(spectro_z_scale(ss), DEFAULT_SPECTRO_Z_SCALE)) pss_sf(fd, S_spectro_z_scale, spectro_z_scale(ss));
  if (fneq(spectro_z_angle(ss), DEFAULT_SPECTRO_Z_ANGLE)) pss_sf(fd, S_spectro_z_angle, spectro_z_angle(ss));
  if (fneq(spectro_x_angle(ss), DEFAULT_SPECTRO_X_ANGLE)) pss_sf(fd, S_spectro_x_angle, spectro_x_angle(ss));
  if (fneq(spectro_y_angle(ss), DEFAULT_SPECTRO_Y_ANGLE)) pss_sf(fd, S_spectro_y_angle, spectro_y_angle(ss));
  if (fneq(spectro_cutoff(ss), DEFAULT_SPECTRO_CUTOFF)) pss_sf(fd, S_spectro_cutoff, spectro_cutoff(ss));
  if (fneq(spectro_start(ss), DEFAULT_SPECTRO_START)) pss_sf(fd, S_spectro_start, spectro_start(ss));
  if (fneq(vu_size(ss), DEFAULT_VU_SIZE)) pss_sf(fd, S_vu_size, vu_size(ss));
  if (fneq(vu_font_size(ss), DEFAULT_VU_FONT_SIZE)) pss_sf(fd, S_vu_font_size, vu_font_size(ss));
  if (fneq(enved_base(ss), DEFAULT_ENVED_BASE)) pss_sf(fd, S_enved_base, enved_base(ss));
  if (fneq(enved_power(ss), DEFAULT_ENVED_POWER)) pss_sf(fd, S_enved_power, enved_power(ss));
  if (fneq(eps_bottom_margin(ss), DEFAULT_EPS_BOTTOM_MARGIN)) pss_sf(fd, S_eps_bottom_margin, eps_bottom_margin(ss));
  if (fneq(eps_left_margin(ss), DEFAULT_EPS_LEFT_MARGIN)) pss_sf(fd, S_eps_left_margin, eps_left_margin(ss));
  save_recorder_state(fd);

  fprintf(fd, ";;; end of snd options\n");
  if (locale)
    {
#if HAVE_SETLOCALE
      setlocale(LC_NUMERIC, locale);
#endif
      FREE(locale);
    }
}

static FILE *open_restart_file(char *name, int append)
{
  FILE *fd;
  char *str;
  char *buf = NULL;
  if (!name) return(NULL);
  buf = (char *)CALLOC(256, sizeof(char));
  str = name;
  if ((*str) == '~')
    {
      strcpy(buf, getenv("HOME"));
      strcat(buf, ++str);
      str = buf;
    }
  if (append)
    fd = fopen(str, "a");
  else fd = fopen(str, "w");
  FREE(buf);
  return(fd);
}

FILE *open_snd_init_file (snd_state *ss)
{ /* needed also by keyboard macro saver */
  return(open_restart_file(ss->init_file, TRUE));
}

static char *save_options_or_error(snd_state *ss)
{
  FILE *fd;
  fd = open_snd_init_file(ss);
  if (fd) save_snd_state_options(ss, fd);
  if ((!fd) || (fclose(fd) != 0))
    return(mus_format("save-options in %s: %s",
		      ss->init_file,
		      strerror(errno)));
  return(NULL);
}

int save_options(snd_state *ss)
{
  char *error;
  error = save_options_or_error(ss);
  if (error)
    {
      snd_error(error);
      FREE(error);
      return(-1);
    }
  return(0);
}

static int save_sound_state (snd_info *sp, void *ptr) 
{
  int chan;
  FILE *fd;
  chan_info *cp;
  axis_info *ap;
  char *tmpstr = NULL;
  fd = (FILE *)ptr;
  fprintf(fd, "(let ((sfile (or (%s \"%s\") (%s \"%s\"))))\n  (if sfile\n    (begin\n",
	  S_find_sound,
	  sp->shortname,
	  (sp->read_only) ? S_view_sound : S_open_sound,
	  sp->fullname);
  if (sp->sync != DEFAULT_SYNC) psp_sd(fd, S_sync, sp->sync);
  if (sp->contrasting != DEFAULT_CONTRASTING) psp_ss(fd, S_contrasting, b2s(sp->contrasting));
  if (sp->contrast != DEFAULT_CONTRAST) psp_sf(fd, S_contrast, sp->contrast);
  if (sp->expanding != DEFAULT_EXPANDING) psp_ss(fd, S_expanding, b2s(sp->expanding));
  if (sp->expand != DEFAULT_EXPAND) psp_sf(fd, S_expand, sp->expand);
  if (sp->expand_ramp != DEFAULT_EXPAND_RAMP) psp_sf(fd, S_expand_ramp, sp->expand_ramp);
  if (sp->expand_hop != DEFAULT_EXPAND_HOP) psp_sf(fd, S_expand_hop, sp->expand_hop);
  if (sp->expand_length != DEFAULT_EXPAND_LENGTH) psp_sf(fd, S_expand_length, sp->expand_length);
  if (sp->srate != DEFAULT_SPEED) psp_sf(fd, S_speed, sp->srate);
  if (sp->speed_tones != DEFAULT_SPEED_TONES) psp_sd(fd, S_speed_tones, sp->speed_tones);
  if (sp->speed_style != DEFAULT_SPEED_STYLE) psp_ss(fd, S_speed_style, speed_style_name(sp->speed_style));
  if (sp->reverbing != DEFAULT_REVERBING) psp_ss(fd, S_reverbing, b2s(sp->reverbing));
  if (sp->revscl != DEFAULT_REVERB_SCALE) psp_sf(fd, S_reverb_scale, sp->revscl);
  if (sp->revlen != DEFAULT_REVERB_LENGTH) psp_sf(fd, S_reverb_length, sp->revlen);
  if (sp->revfb != DEFAULT_REVERB_FEEDBACK) psp_sf(fd, S_reverb_feedback, sp->revfb);
  if (sp->revlp != DEFAULT_REVERB_LOWPASS) psp_sf(fd, S_reverb_lowpass, sp->revlp);
  if (sp->reverb_decay != DEFAULT_REVERB_DECAY) psp_sf(fd, S_reverb_decay, sp->reverb_decay);
  if (sp->amp != DEFAULT_AMP) psp_sf(fd, S_amp, sp->amp);
  if (sp->filtering != DEFAULT_FILTERING) psp_ss(fd, S_filtering, b2s(sp->filtering));
  if (sp->filter_order != DEFAULT_FILTER_ORDER) psp_sd(fd, S_filter_order, sp->filter_order);
  if (sp->filter_dBing != DEFAULT_FILTER_DBING) psp_ss(fd, S_filter_dBing, b2s(sp->filter_dBing));
  if (sp->filter_env) 
    {
      psp_ss(fd, S_filter_env, tmpstr = env_to_string(sp->filter_env));
      if (tmpstr) FREE(tmpstr);
    }
  if (sp->cursor_follows_play) psp_ss(fd, S_cursor_follows_play, b2s(sp->cursor_follows_play));
  if (PROCEDURE_P(sp->search_proc))
    {
      fprintf(fd, "      ;;; currently not trying to restore the local search procedure\n");
      fprintf(fd, "      ;;; %s\n", g_print_1(sp->search_proc, __FUNCTION__));
    }
  for (chan = 0; chan < sp->nchans; chan++)
    {
      cp = sp->chans[chan];
      ap = cp->axis;
      if (!(cp->waving)) pcp_ss(fd, S_waving, b2s(cp->waving), chan);
      if (cp->ffting) pcp_ss(fd, S_ffting, b2s(cp->ffting), chan);
      if (cp->lisp_graphing) pcp_ss(fd, S_graphing, b2s(cp->lisp_graphing), chan);
      if ((ap->x0 != 0.0) || (ap->x1 != 0.1)) pcp_sl(fd, S_x_bounds, ap->x0, ap->x1, chan);
      if ((ap->y0 != -1.0) || (ap->y1 != 1.0)) pcp_sl(fd, S_y_bounds, ap->y0, ap->y1, chan);
      if (cp->cursor != 0) pcp_sd(fd, S_cursor, cp->cursor, chan);
      if (cp->cursor_size != DEFAULT_CURSOR_SIZE) pcp_sd(fd, S_cursor_size, cp->cursor_size, chan);
      if (cp->cursor_style != CURSOR_CROSS) pcp_sd(fd, S_cursor_style, cp->cursor_style, chan);
      if (cp->show_marks != DEFAULT_SHOW_MARKS) pcp_ss(fd, S_show_marks, b2s(cp->show_marks), chan);
      if (cp->show_y_zero != DEFAULT_SHOW_Y_ZERO) pcp_ss(fd, S_show_y_zero, b2s(cp->show_y_zero), chan);
      if (cp->wavo != DEFAULT_WAVO) pcp_ss(fd, S_wavo, b2s(cp->wavo), chan);
      if (cp->wavo_hop != DEFAULT_WAVO_HOP) pcp_sd(fd, S_wavo_hop, cp->wavo_hop, chan);
      if (cp->wavo_trace != DEFAULT_WAVO_TRACE) pcp_sd(fd, S_wavo_trace, cp->wavo_trace, chan);
      if (cp->max_fft_peaks != DEFAULT_MAX_FFT_PEAKS) pcp_sd(fd, S_max_fft_peaks, cp->max_fft_peaks, chan);
      if (cp->show_fft_peaks != DEFAULT_SHOW_FFT_PEAKS) pcp_ss(fd, S_show_fft_peaks, b2s(cp->show_fft_peaks), chan);
      if (cp->fft_log_frequency != DEFAULT_FFT_LOG_FREQUENCY) pcp_ss(fd, S_fft_log_frequency, b2s(cp->fft_log_frequency), chan);
      if (cp->fft_log_magnitude != DEFAULT_FFT_LOG_MAGNITUDE) pcp_ss(fd, S_fft_log_magnitude, b2s(cp->fft_log_magnitude), chan);
      if (cp->verbose_cursor != DEFAULT_VERBOSE_CURSOR) pcp_ss(fd, S_verbose_cursor, b2s(cp->verbose_cursor), chan);
      if (cp->zero_pad != DEFAULT_ZERO_PAD) pcp_sd(fd, S_zero_pad, cp->zero_pad, chan);
      if (cp->wavelet_type != DEFAULT_WAVELET_TYPE) pcp_sd(fd, S_wavelet_type, cp->wavelet_type, chan);
      if (fneq(cp->min_dB, DEFAULT_MIN_DB)) pcp_sf(fd, S_min_dB, cp->min_dB, chan);
      if (fneq(cp->spectro_x_angle, DEFAULT_SPECTRO_X_ANGLE)) pcp_sf(fd, S_spectro_x_angle, cp->spectro_x_angle, chan);
      if (fneq(cp->spectro_y_angle, DEFAULT_SPECTRO_Y_ANGLE)) pcp_sf(fd, S_spectro_y_angle, cp->spectro_y_angle, chan);
      if (fneq(cp->spectro_z_angle, DEFAULT_SPECTRO_Z_ANGLE)) pcp_sf(fd, S_spectro_z_angle, cp->spectro_z_angle, chan);
      if (fneq(cp->spectro_x_scale, DEFAULT_SPECTRO_X_SCALE)) pcp_sf(fd, S_spectro_x_scale, cp->spectro_x_scale, chan);
      if (fneq(cp->spectro_y_scale, DEFAULT_SPECTRO_Y_SCALE)) pcp_sf(fd, S_spectro_y_scale, cp->spectro_y_scale, chan);
      if (fneq(cp->spectro_z_scale, DEFAULT_SPECTRO_Z_SCALE)) pcp_sf(fd, S_spectro_z_scale, cp->spectro_z_scale, chan);
      if (fneq(cp->spectro_cutoff, DEFAULT_SPECTRO_CUTOFF)) pcp_sf(fd, S_spectro_cutoff, cp->spectro_cutoff, chan);
      if (fneq(cp->spectro_start, DEFAULT_SPECTRO_START)) pcp_sf(fd, S_spectro_start, cp->spectro_start, chan);
      if (fneq(cp->fft_beta, DEFAULT_FFT_BETA)) pcp_sf(fd, S_fft_beta, cp->fft_beta, chan);
      if (cp->spectro_hop != DEFAULT_SPECTRO_HOP) pcp_sd(fd, S_spectro_hop, cp->spectro_hop, chan);
      if (cp->fft_size != DEFAULT_FFT_SIZE) pcp_sd(fd, S_fft_size, cp->fft_size, chan);
      if (cp->fft_style != DEFAULT_FFT_STYLE) pcp_ss(fd, S_fft_style, fft_style_name(cp->fft_style), chan);
      if (cp->fft_window != DEFAULT_FFT_WINDOW) pcp_ss(fd, S_fft_window, mus_fft_window_name(cp->fft_window), chan);
      if (cp->transform_type != DEFAULT_TRANSFORM_TYPE) pcp_ss(fd, S_transform_type, transform_type_name(cp->transform_type), chan);
      if (cp->normalize_fft != DEFAULT_NORMALIZE_FFT) pcp_ss(fd, S_normalize_fft, normalize_fft_name(cp->normalize_fft), chan);
      if (cp->graph_style != DEFAULT_GRAPH_STYLE) pcp_ss(fd, S_graph_style, graph_style_name(cp->graph_style), chan);
      if (cp->show_mix_waveforms != DEFAULT_SHOW_MIX_WAVEFORMS) pcp_ss(fd, S_show_mix_waveforms, b2s(cp->show_mix_waveforms), chan);
      if (cp->dot_size != DEFAULT_DOT_SIZE) pcp_sd(fd, S_dot_size, cp->dot_size, chan);
      if (cp->show_axes != DEFAULT_SHOW_AXES) pcp_sd(fd, S_show_axes, cp->show_axes, chan);
      if (cp->graphs_horizontal != DEFAULT_GRAPHS_HORIZONTAL) pcp_ss(fd, S_graphs_horizontal, b2s(cp->graphs_horizontal), chan);
      edit_history_to_file(fd, cp);
    }
  fprintf(fd, "      )))\n");
  return(0);
}

static char *save_state_or_error (snd_state *ss, char *save_state_name)
{
  FILE *save_fd;
  char *locale = NULL;
  save_fd = open_restart_file(save_state_name, FALSE);
  if (save_fd == NULL) 
    return(mus_format("can't write %s: %s", 
		      save_state_name, 
		      strerror(errno)));

  else
    {
#if HAVE_SETLOCALE
      locale = copy_string(setlocale(LC_NUMERIC, "C")); /* must use decimal point in floats since Scheme assumes that format */
#endif
      save_prevlist(save_fd);                                /* list of previous files (View: Files option) */
      map_over_sounds(ss, save_sound_state, (void *)save_fd);  /* current sound state -- will traverse chans */
      save_macro_state(save_fd);                             /* current unsaved keyboard macros (snd-chn.c) */
      save_envelope_editor_state(save_fd);                   /* current envelope editor window state */
      save_regions(ss, save_fd);                              /* regions */
      save_snd_state_options(ss, save_fd);                    /* options = user-settable global state variables */
      if (transform_dialog_is_active()) fprintf(save_fd, "(%s)\n", S_transform_dialog);
      if (enved_dialog_is_active()) fprintf(save_fd, "(%s)\n", S_enved_dialog);
      if (color_dialog_is_active()) fprintf(save_fd, "(%s)\n", S_color_dialog);
      if (orientation_dialog_is_active()) fprintf(save_fd, "(%s)\n", S_orientation_dialog);
      if (file_dialog_is_active()) fprintf(save_fd, "(%s)\n", S_file_dialog); /* View: Files dialog, not Open: File */
      if (region_dialog_is_active()) fprintf(save_fd, "(%s)\n", S_region_dialog);
      if (record_dialog_is_active()) fprintf(save_fd, "(%s)\n", S_recorder_dialog);

      /* TODO save mix? */

      /* the problem here (with saving hooks) is that it is not straightforward to save the function source
       *   (with the current print-set! source option, or with an earlier procedure->string function using
       *   procedure_environment etc); many types print in this case in ways that are not readable.
       *   The functions may depend on globals that are not in loaded files, or that were changed since
       *   loading, and trying to map over the current module's obarray, saving each such variable in
       *   its current form, is a major undertaking (although this can be done for simple vars; additionally, 
       *   what if the user has changed these
       *   before restoring -- should the old forms be restored?  Perhaps the new files associated
       *   with dumping (libguile/dump.c) will address this issue.  And, things like search functions
       *   and hooks might be viewed as temporary to begin with. If the function source is long,
       *   some sort of pretty-printer is really needed, but I couldn't get slib's to work.
       */
      save_user_key_bindings(save_fd);

      if (locale)
	{
#if HAVE_SETLOCALE
	  setlocale(LC_NUMERIC, locale);
#endif
	  FREE(locale);
	}

      if (fclose(save_fd) != 0)
	return(mus_format("can't close %s: %s [%s[%d] %s]", 
			  save_state_name, strerror(errno), 
			  __FILE__, __LINE__, __FUNCTION__));

    }
  return(NULL);
}

int save_state (snd_state *ss, char *save_state_name)
{
  char *error;
  error = save_state_or_error(ss, save_state_name);
  if (error)
    {
      snd_error(error);
      FREE(error);
      return(-1);
    }
  return(0);
}

static char *file_extension(char *arg)
{
  char *dot = NULL, *sp;
  if (arg) 
    for (sp = arg; (*sp) != '\0'; sp++) 
      if ((*sp) == '.') 
	dot = (++sp);
  return(dot);
}

static SCM start_hook;

static int dont_start(snd_state *ss, char *filename)
{
  SCM res = SCM_BOOL_F;
  if (HOOKED(start_hook))
    res = g_c_run_or_hook(start_hook,
			  SCM_LIST1(TO_SCM_STRING(filename)),
			  S_start_hook);
  return(TRUE_P(res));
}

static char *startup_filename = NULL;

int handle_next_startup_arg(snd_state *ss, int auto_open_ctr, char **auto_open_file_names, int with_title)
{
  char *argname;
  argname = auto_open_file_names[auto_open_ctr];
  if (argname)
    { /* wanted to use "-d" and "-i" but they're in use */
      if ((strcmp("-h", argname) == 0) || 
	  (strcmp("-horizontal", argname) == 0) ||
	  (strcmp("-v", argname) == 0) || 
	  (strcmp("-vertical", argname) == 0) ||
	  (strcmp("-notebook", argname) == 0) ||
	  (strcmp("-separate", argname) == 0) ||
	  (strcmp("-noglob", argname) == 0) ||
	  (strcmp("-noinit", argname) == 0))
	return(auto_open_ctr + 1);
      else
	{
	  if ((strcmp("-p", argname) == 0) ||
	      (strcmp("-preload", argname) == 0))
	    {
	      /* preload sound files in dir (can be ., should be unquoted) */
	      auto_open_ctr++;
	      add_directory_to_prevlist(ss, auto_open_file_names[auto_open_ctr]);
	    }
	  else
	    {
	      if ((strcmp("-l", argname) == 0) ||
		  (strcmp("-load", argname) == 0) ||
		  ((file_extension(argname)) && 
		   (strcmp(file_extension(argname), "scm") == 0)))
		{
		  if ((strcmp("-l", argname) == 0) || 
		      (strcmp("-load", argname) == 0)) 
		    auto_open_ctr++;
		  snd_load_file(auto_open_file_names[auto_open_ctr]);
		}
	      else
		{
		  if ((strcmp("-e", argname) == 0) ||
		      (strcmp("-eval", argname) == 0))
		    {
		      /* evaluate expression */
		      auto_open_ctr++;
		      snd_eval_str(ss, auto_open_file_names[auto_open_ctr]);
		    }
		  else
		    {
		      if ((with_title) && 
			  (strcmp("-title", argname) == 0))
			{
			  auto_open_ctr++;
			  ss->startup_title = copy_string(auto_open_file_names[auto_open_ctr]);
			}
		      else
			{
			  if (startup_filename == NULL)
			    {
			      startup_filename = copy_string(argname);
			      if (dont_start(ss, startup_filename)) snd_exit(1);
			    }
			  snd_open_file_unselected(argname, ss);
			}
		    }
		}
	    }
	}
    }
  return(auto_open_ctr + 1);
}

static SCM g_save_state(SCM filename) 
{
  #define H_save_state "(" S_save_state " filename) saves the current Snd state in filename; (load filename) restores it)"

  char *error;
  SCM result;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_save_state, "a string");
  error = save_state_or_error(get_global_state(), 
			      TO_C_STRING(filename));
  if (error)
    {
      result = TO_SCM_STRING(error);
      FREE(error);
      ERROR(CANNOT_SAVE,
	    SCM_LIST3(TO_SCM_STRING(S_save_state),
		      filename,
		      result));
    }
  return(filename);
}

static SCM g_save_options(SCM filename)
{
  #define H_save_options "(" S_save_options " filename) saves Snd options in filename"
  char *name = NULL;
  FILE *fd;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_save_options, "a string");
  name = mus_expand_filename(TO_C_STRING(filename));
  fd = fopen(name, "w");
  if (name) FREE(name);
  if (fd) 
    save_snd_state_options(get_global_state(), fd);
  if ((!fd) || 
      (fclose(fd) != 0))
    ERROR(CANNOT_SAVE, 
	  SCM_LIST3(TO_SCM_STRING(S_save_options),
		    filename,
		    TO_SCM_STRING(strerror(errno))));
  return(filename);
}

static SCM g_exit(SCM val) 
{
  #define H_exit "(" S_exit ") exits Snd"
  snd_state *ss;
  ss = get_global_state();
  if (dont_exit()) return(SCM_BOOL_T);
  snd_exit_cleanly(ss); 
  snd_exit(TO_C_INT_OR_ELSE(val,1)); 
  return(SCM_BOOL_F);
}

static SCM g_mem_report(void) 
{
#if DEBUG_MEMORY
  mem_report(); 
#endif
  return(SCM_BOOL_F);
}

void g_init_main(SCM local_doc)
{
  DEFINE_PROC(S_save_options, g_save_options, 1, 0, 0, H_save_options);
  DEFINE_PROC(S_save_state,   g_save_state, 1, 0, 0,   H_save_state);
  DEFINE_PROC(S_exit,         g_exit, 0, 1, 0,         H_exit);

  DEFINE_PROC("mem-report",   g_mem_report, 0, 0, 0, "(mem-report) writes memory usage stats to memlog");

  #define H_start_hook S_start_hook " (filename) is called upon start-up. If it returns #t, snd exits immediately."
  start_hook =          MAKE_HOOK(S_start_hook, 1, H_start_hook);                   /* arg = argv filename if any */
}
