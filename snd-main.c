#include "snd.h"
#include "clm2xen.h"

#if HAVE_RUBY
  #define TO_VAR_NAME(Str) xen_scheme_constant_to_ruby(Str)
  #define TO_PROC_NAME(Str) xen_scheme_procedure_to_ruby(Str)
#else
  #define TO_VAR_NAME(Str) Str
  #define TO_PROC_NAME(Str) Str
#endif

static void remove_temp_files(chan_info *cp)
{
  free_sound_list(cp);
  free_mix_list(cp);
}

#ifdef DEBUG_MEMORY
  void mem_report(void);
#endif

static XEN exit_hook;

int snd_exit_cleanly(bool force_exit)
{  
  XEN res = XEN_FALSE;
  ss->exiting = true; /* if segfault during exit code, don't try to restart at event loop! */
  if (XEN_HOOKED(exit_hook))
    res = run_or_hook(exit_hook, 
		      XEN_EMPTY_LIST,
		      S_exit_hook);
  if ((XEN_TRUE_P(res)) && (!force_exit)) return(0);
  cleanup_dac();
  for_each_chan(remove_temp_files);
  cleanup_region_temp_files();
  cleanup_recording();
  forget_temps();
#ifdef DEBUG_MEMORY
  mem_report();
#endif
  return(1);
}

void sound_not_current(snd_info *sp, void *ignore)
{
  /* check for change in update status */
  bool needs_update;
  needs_update = (file_write_date(sp->filename) != sp->write_date);
  if (needs_update != sp->need_update)
    {
      sp->need_update = needs_update;
      if ((needs_update) && (auto_update(ss)))
	snd_update(sp);
      else snd_file_bomb_icon(sp, needs_update);
    }
}

/* ---------------- save sound state (options, or entire state) ---------------- */

static bool fneq(Float a, Float b)
{
  /* floating point != replacement */
  return(fabs(a - b) > .00001);
}

static char *cursor_style_name(cursor_style_t style)
{
  switch (style)
    {
    case CURSOR_CROSS: return(TO_VAR_NAME(S_cursor_cross)); break;
    case CURSOR_LINE:  return(TO_VAR_NAME(S_cursor_line));  break;
    default: /* proc?? */ return(TO_VAR_NAME(S_cursor_cross)); break;
    }
}

static char *show_axes2string(show_axes_t ax)
{
  switch (ax)
    {
    case SHOW_NO_AXES:           return(TO_VAR_NAME(S_show_no_axes));             break;
    case SHOW_X_AXIS:            return(TO_VAR_NAME(S_show_x_axis));              break;
    case SHOW_X_AXIS_UNLABELLED: return(TO_VAR_NAME(S_show_x_axis_unlabelled));   break;
    case SHOW_ALL_AXES:          return(TO_VAR_NAME(S_show_all_axes));            break;
    default:                     return(TO_VAR_NAME(S_show_all_axes_unlabelled)); break;
    }
}

static char *zoom_focus_style_name(zoom_focus_t choice)
{
  switch (choice)
    {
    case ZOOM_FOCUS_LEFT:   return(TO_VAR_NAME(S_zoom_focus_left));   break;
    case ZOOM_FOCUS_RIGHT:  return(TO_VAR_NAME(S_zoom_focus_right));  break;
    case ZOOM_FOCUS_MIDDLE: return(TO_VAR_NAME(S_zoom_focus_middle)); break;
    default:                return(TO_VAR_NAME(S_zoom_focus_active)); break;
    }
}

static char *transform_normalization_name(fft_normalize_t choice)
{
  switch (choice)
    {
    case DONT_NORMALIZE:      return(TO_VAR_NAME(S_dont_normalize));       break;
    case NORMALIZE_BY_CHANNEL:return(TO_VAR_NAME(S_normalize_by_channel)); break;
    case NORMALIZE_BY_SOUND:  return(TO_VAR_NAME(S_normalize_by_sound));   break;
    case NORMALIZE_GLOBALLY:  return(TO_VAR_NAME(S_normalize_globally));   break;
    default:                  return(TO_VAR_NAME(S_normalize_by_channel)); break;
    }
}

static char *graph_style_name(graph_style_t choice)
{
  switch (choice)
    {
    case GRAPH_DOTS:           return(TO_VAR_NAME(S_graph_dots));           break;
    case GRAPH_DOTS_AND_LINES: return(TO_VAR_NAME(S_graph_dots_and_lines)); break;
    case GRAPH_LOLLIPOPS:      return(TO_VAR_NAME(S_graph_lollipops));      break;
    case GRAPH_FILLED:         return(TO_VAR_NAME(S_graph_filled));         break;
    case GRAPH_LINES: 
    default:                   return(TO_VAR_NAME(S_graph_lines));          break;
    }
}

static char *transform_graph_type_name(graph_type_t choice)
{
  switch (choice)
    {
    case GRAPH_AS_SONOGRAM:    return(TO_VAR_NAME(S_graph_as_sonogram));    break;
    case GRAPH_AS_SPECTROGRAM: return(TO_VAR_NAME(S_graph_as_spectrogram)); break;
    default:                   return(TO_VAR_NAME(S_graph_once));           break;
    }
}

static char *time_graph_type_name(graph_type_t choice)
{
  switch (choice)
    {
    case GRAPH_AS_WAVOGRAM: return(TO_VAR_NAME(S_graph_as_wavogram)); break;
    default:                return(TO_VAR_NAME(S_graph_once));        break;
    }
}

static char *x_axis_style_name(x_axis_style_t choice)
{
  switch(choice)
    {
    case X_AXIS_IN_SAMPLES:    return(TO_VAR_NAME(S_x_axis_in_samples));    break;
    case X_AXIS_AS_PERCENTAGE: return(TO_VAR_NAME(S_x_axis_as_percentage)); break;
    case X_AXIS_IN_BEATS:      return(TO_VAR_NAME(S_x_axis_in_beats));      break;
    default:                   return(TO_VAR_NAME(S_x_axis_in_seconds));    break;
    }
}

static char *speed_control_style_name(speed_style_t choice)
{
  switch (choice)
    {
    case SPEED_CONTROL_AS_RATIO:    return(TO_VAR_NAME(S_speed_control_as_ratio));    break;
    case SPEED_CONTROL_AS_SEMITONE: return(TO_VAR_NAME(S_speed_control_as_semitone)); break;
    default:                        return(TO_VAR_NAME(S_speed_control_as_float));    break;
    }
}

static char *channel_style_name(channel_style_t choice)
{
  switch (choice)
    {
    case CHANNELS_COMBINED:     return(TO_VAR_NAME(S_channels_combined));     break;
    case CHANNELS_SUPERIMPOSED: return(TO_VAR_NAME(S_channels_superimposed)); break;
    default:                    return(TO_VAR_NAME(S_channels_separate));     break;
    }
}

static char *enved_target_name(enved_target_t choice)
{
  switch (choice)
    {
    case ENVED_SPECTRUM: return(TO_VAR_NAME(S_enved_spectrum));  break;
    case ENVED_SRATE:    return(TO_VAR_NAME(S_enved_srate));     break;
    default:             return(TO_VAR_NAME(S_enved_amplitude)); break;
    }
}

#if HAVE_RUBY
static char *b2s(bool val) {if (val) return("true"); else return("false");}
#else
static char *b2s(bool val) {if (val) return("#t"); else return("#f");}
#endif

#define white_space "      "
static bool b_ok = false;

#if HAVE_RUBY
static void pss_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "set_%s(%s)\n", TO_PROC_NAME(name), val);}
static void pss_sq(FILE *fd, const char *name, const char *val) {fprintf(fd, "set_%s(\"%s\")\n", TO_PROC_NAME(name), val);}
static void pss_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "set_%s(%d)\n", TO_PROC_NAME(name), val);}
static void pss_sf(FILE *fd, const char *name, Float val) {fprintf(fd, "set_%s(%.4f)\n", TO_PROC_NAME(name), val);}
static void pss_sl(FILE *fd, const char *name, Float val1, Float val2) 
  {fprintf(fd, "%sset_%s([%f, %f])\n", white_space, TO_PROC_NAME(name), val1, val2);}

static void psp_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "%sset_%s(%s, sfile)\n", white_space, TO_PROC_NAME(name), val);}
static void psp_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "%sset_%s(%d, sfile)\n", white_space, TO_PROC_NAME(name), val);}
static void psp_sf(FILE *fd, const char *name, Float val) {fprintf(fd, "%sset_%s(%.4f, sfile)\n", white_space, TO_PROC_NAME(name), val);}
static void psp_sl(FILE *fd, const char *name, Float val1, Float val2) 
  {fprintf(fd, "%sset_%s([%f, %f], sfile)\n", white_space, TO_PROC_NAME(name), val1, val2);}

static void pcp_ss(FILE *fd, const char *name, const char *val, int chan) {fprintf(fd, "%sset_%s(%s, sfile, %d)\n", white_space, TO_PROC_NAME(name), val, chan);}
static void pcp_sd(FILE *fd, const char *name, int val, int chan)   {fprintf(fd, "%sset_%s(%d, sfile, %d)\n", white_space, TO_PROC_NAME(name), val, chan);}
static void pcp_sod(FILE *fd, const char *name, off_t val, int chan)   {fprintf(fd, "%sset_%s(" OFF_TD ", sfile, %d)\n", white_space, TO_PROC_NAME(name), val, chan);}
static void pcp_sf(FILE *fd, const char *name, Float val, int chan) {fprintf(fd, "%sset_%s(%.4f, sfile, %d)\n", white_space, TO_PROC_NAME(name), val, chan);}
static void pcp_sl(FILE *fd, const char *name, Float val1, Float val2, int chan) 
  {fprintf(fd, "%sset_%s([%f, %f], sfile, %d)\n", white_space, TO_PROC_NAME(name), val1, val2, chan);}
#endif
#if HAVE_GUILE || (!HAVE_EXTENSION_LANGUAGE)
static void pss_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "(set! (%s) %s)\n", name, val);}
static void pss_sq(FILE *fd, const char *name, const char *val) {fprintf(fd, "(set! (%s) \"%s\")\n", name, val);}
static void pss_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "(set! (%s) %d)\n", name, val);}
static void pss_sf(FILE *fd, const char *name, Float val) {fprintf(fd, "(set! (%s) %.4f)\n", name, val);}
static void pss_sl(FILE *fd, const char *name, Float val1, Float val2) {fprintf(fd, "%s(set! (%s) (list %f %f))\n", white_space, name, val1, val2);}

static void psp_ss(FILE *fd, const char *name, const char *val) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile) %s)\n", white_space, name, val);}
static void psp_sd(FILE *fd, const char *name, int val)   
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile) %d)\n", white_space, name, val);}
static void psp_sf(FILE *fd, const char *name, Float val) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile) %.4f)\n", white_space, name, val);}
static void psp_sl(FILE *fd, const char *name, Float val1, Float val2) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile) (list %f %f))\n", white_space, name, val1, val2);}

static void pcp_ss(FILE *fd, const char *name, const char *val, int chan) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %s)\n", white_space, name, chan, val);}
static void pcp_sd(FILE *fd, const char *name, int val, int chan)   
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %d)\n", white_space, name, chan, val);}
static void pcp_sod(FILE *fd, const char *name, off_t val, int chan)   
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) " OFF_TD ")\n", white_space, name, chan, val);}
static void pcp_sf(FILE *fd, const char *name, Float val, int chan) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %.4f)\n", white_space, name, chan, val);}
static void pcp_sl(FILE *fd, const char *name, Float val1, Float val2, int chan) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) (list %f %f))\n", white_space, name, chan, val1, val2);}
#endif

static void save_snd_state_options (FILE *fd)
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
  fprintf(fd, "\n%s Snd %s (%s) options saved %s\n", XEN_COMMENT_STRING, SND_VERSION, SND_DATE, time_buf);
#else
  fprintf(fd, "\n%s Snd %s (%s)\n", XEN_COMMENT_STRING, SND_VERSION, SND_DATE);
#endif

  if (transform_size(ss) != DEFAULT_TRANSFORM_SIZE) pss_sd(fd, S_transform_size, transform_size(ss));
  if (minibuffer_history_length(ss) != DEFAULT_MINIBUFFER_HISTORY_LENGTH) pss_sd(fd, S_minibuffer_history_length, minibuffer_history_length(ss));
  if (fft_window(ss) != DEFAULT_FFT_WINDOW) pss_ss(fd, S_fft_window, TO_VAR_NAME(mus_fft_window_name(fft_window(ss))));
  if (transform_graph_type(ss) != DEFAULT_TRANSFORM_GRAPH_TYPE) pss_ss(fd, S_transform_graph_type, transform_graph_type_name(transform_graph_type(ss)));
  if (time_graph_type(ss) != DEFAULT_TIME_GRAPH_TYPE) pss_ss(fd, S_time_graph_type, time_graph_type_name(time_graph_type(ss)));
  if (x_axis_style(ss) != DEFAULT_X_AXIS_STYLE) pss_ss(fd, S_x_axis_style, x_axis_style_name(x_axis_style(ss)));
  if (beats_per_minute(ss) != DEFAULT_BEATS_PER_MINUTE) pss_sf(fd, S_beats_per_minute, beats_per_minute(ss));
  if (graph_style(ss) != DEFAULT_GRAPH_STYLE) pss_ss(fd, S_graph_style, graph_style_name(graph_style(ss)));
  if (region_graph_style(ss) != DEFAULT_GRAPH_STYLE) pss_ss(fd, S_region_graph_style, graph_style_name(region_graph_style(ss)));
  if (channel_style(ss) != DEFAULT_CHANNEL_STYLE) pss_ss(fd, S_channel_style, channel_style_name(channel_style(ss)));
  if (enved_target(ss) != DEFAULT_ENVED_TARGET) pss_ss(fd, S_enved_target, enved_target_name(enved_target(ss)));
  if (transform_type(ss) != DEFAULT_TRANSFORM_TYPE) pss_ss(fd, S_transform_type, TO_VAR_NAME(transform_type_name(transform_type(ss))));
  if (zoom_focus_style(ss) != ZOOM_FOCUS_ACTIVE) pss_ss(fd, S_zoom_focus_style, zoom_focus_style_name(zoom_focus_style(ss)));
  if (transform_normalization(ss) != DEFAULT_TRANSFORM_NORMALIZATION) pss_ss(fd, S_transform_normalization, transform_normalization_name(transform_normalization(ss)));
  if (optimization(ss) != DEFAULT_OPTIMIZATION) pss_sd(fd, S_optimization, optimization(ss));
  if (trap_segfault(ss) != DEFAULT_TRAP_SEGFAULT) pss_ss(fd, S_trap_segfault, b2s(trap_segfault(ss)));
  if (show_selection_transform(ss) != DEFAULT_SHOW_SELECTION_TRANSFORM) pss_ss(fd, S_show_selection_transform, b2s(show_selection_transform(ss)));
  if (with_gl(ss) != DEFAULT_WITH_GL) pss_ss(fd, S_with_gl, b2s(with_gl(ss)));
  if (with_mix_tags(ss) != DEFAULT_WITH_MIX_TAGS) pss_ss(fd, S_with_mix_tags, b2s(with_mix_tags(ss)));
  if (with_relative_panes(ss) != DEFAULT_WITH_RELATIVE_PANES) pss_ss(fd, S_with_relative_panes, b2s(with_relative_panes(ss)));
  if (sinc_width(ss) != DEFAULT_SINC_WIDTH) pss_sd(fd, S_sinc_width, sinc_width(ss));
  if (ss->init_window_width != DEFAULT_INIT_WINDOW_WIDTH) pss_sd(fd, S_window_width, widget_width(MAIN_SHELL(ss)));
  if (ss->init_window_height != DEFAULT_INIT_WINDOW_HEIGHT) pss_sd(fd, S_window_height, widget_height(MAIN_SHELL(ss)));
  if (ss->init_window_x != DEFAULT_INIT_WINDOW_X) pss_sd(fd, S_window_x, widget_x(MAIN_SHELL(ss)));
  if (ss->init_window_y != DEFAULT_INIT_WINDOW_Y) pss_sd(fd, S_window_y, widget_y(MAIN_SHELL(ss)));
  if (default_output_chans(ss) != DEFAULT_OUTPUT_CHANS) pss_sd(fd, S_default_output_chans, default_output_chans(ss));
  if (default_output_srate(ss) != DEFAULT_OUTPUT_SRATE) pss_sd(fd, S_default_output_srate, default_output_srate(ss));
  if (default_output_type(ss) != DEFAULT_OUTPUT_TYPE) pss_ss(fd, S_default_output_type, mus_header_type_to_constant_name(default_output_type(ss)));
  if (default_output_format(ss) != DEFAULT_OUTPUT_FORMAT) pss_ss(fd, S_default_output_format, mus_data_format_to_constant_name(default_output_format(ss)));
  if (auto_resize(ss) != DEFAULT_AUTO_RESIZE) pss_ss(fd, S_auto_resize, b2s(auto_resize(ss)));
  if (graphs_horizontal(ss) != DEFAULT_GRAPHS_HORIZONTAL) pss_ss(fd, S_graphs_horizontal, b2s(graphs_horizontal(ss)));
  if (auto_update(ss) != DEFAULT_AUTO_UPDATE) pss_ss(fd, S_auto_update, b2s(auto_update(ss)));
  if (color_inverted(ss) != DEFAULT_COLOR_INVERTED) pss_ss(fd, S_color_inverted, b2s(color_inverted(ss)));
  if (zero_pad(ss) != DEFAULT_ZERO_PAD) pss_sd(fd, S_zero_pad, zero_pad(ss));
  if (ask_before_overwrite(ss) != DEFAULT_ASK_BEFORE_OVERWRITE) pss_ss(fd, S_ask_before_overwrite, b2s(ask_before_overwrite(ss)));
  if (dac_combines_channels(ss) != DEFAULT_DAC_COMBINES_CHANNELS) pss_ss(fd, S_dac_combines_channels, b2s(dac_combines_channels(ss)));
  if (emacs_style_save_as(ss) != DEFAULT_EMACS_STYLE_SAVE_AS) pss_ss(fd, S_emacs_style_save_as, b2s(emacs_style_save_as(ss)));
  if (wavo_hop(ss) != DEFAULT_WAVO_HOP) pss_sd(fd, S_wavo_hop, wavo_hop(ss));
  if (wavo_trace(ss) != DEFAULT_WAVO_TRACE) pss_sd(fd, S_wavo_trace, wavo_trace(ss));
  if (spectro_hop(ss) != DEFAULT_SPECTRO_HOP) pss_sd(fd, S_spectro_hop, spectro_hop(ss));
  if (color_map(ss) != DEFAULT_COLOR_MAP) pss_sd(fd, S_colormap, color_map(ss));
  if (color_map_size(ss) != DEFAULT_COLOR_MAP_SIZE) pss_sd(fd, S_colormap_size, color_map_size(ss));
  if (wavelet_type(ss) != DEFAULT_WAVELET_TYPE) pss_sd(fd, S_wavelet_type, wavelet_type(ss));
  if (cursor_style(ss) != DEFAULT_CURSOR_STYLE) pss_ss(fd, S_cursor_style, cursor_style_name(cursor_style(ss)));
  if (cursor_size(ss) != DEFAULT_CURSOR_SIZE) pss_sd(fd, S_cursor_size, cursor_size(ss));
  if (dot_size(ss) != DEFAULT_DOT_SIZE) pss_sd(fd, S_dot_size, dot_size(ss));
  if (dac_size(ss) != DEFAULT_DAC_SIZE) pss_sd(fd, S_dac_size, dac_size(ss));
  if (selection_creates_region(ss) != DEFAULT_SELECTION_CREATES_REGION) pss_ss(fd, S_selection_creates_region, b2s(selection_creates_region(ss)));
  if (enved_filter_order(ss) != DEFAULT_ENVED_FILTER_ORDER) pss_sd(fd, S_enved_filter_order, enved_filter_order(ss));
  if (max_transform_peaks(ss) != DEFAULT_MAX_TRANSFORM_PEAKS) pss_sd(fd, S_max_transform_peaks, max_transform_peaks(ss));
  if (max_regions(ss) != DEFAULT_MAX_REGIONS) pss_sd(fd, S_max_regions, max_regions(ss));
  if (auto_update_interval(ss) != DEFAULT_AUTO_UPDATE_INTERVAL) pss_sf(fd, S_auto_update_interval, auto_update_interval(ss));
  if (cursor_update_interval(ss) != DEFAULT_CURSOR_UPDATE_INTERVAL) pss_sf(fd, S_cursor_update_interval, cursor_update_interval(ss));
  if (cursor_location_offset(ss) != DEFAULT_CURSOR_LOCATION_OFFSET) pss_sd(fd, S_cursor_location_offset, cursor_location_offset(ss));
  if (verbose_cursor(ss) != DEFAULT_VERBOSE_CURSOR) pss_ss(fd, S_verbose_cursor, b2s(verbose_cursor(ss)));
  if (show_indices(ss) != DEFAULT_SHOW_INDICES) pss_ss(fd, S_show_indices, b2s(show_indices(ss)));
  if (show_backtrace(ss) != DEFAULT_SHOW_BACKTRACE) pss_ss(fd, S_show_backtrace, b2s(show_backtrace(ss)));
  if (show_transform_peaks(ss) != DEFAULT_SHOW_TRANSFORM_PEAKS) pss_ss(fd, S_show_transform_peaks, b2s(show_transform_peaks(ss)));
  if (show_y_zero(ss) != DEFAULT_SHOW_Y_ZERO) pss_ss(fd, S_show_y_zero, b2s(show_y_zero(ss)));
  if (show_grid(ss) != DEFAULT_SHOW_GRID) pss_ss(fd, S_show_grid, b2s(show_grid(ss)));
  if (show_axes(ss) != DEFAULT_SHOW_AXES) pss_ss(fd, S_show_axes, show_axes2string(show_axes(ss)));
  if (show_marks(ss) != DEFAULT_SHOW_MARKS) pss_ss(fd, S_show_marks, b2s(show_marks(ss)));
  if (data_clipped(ss) != DEFAULT_DATA_CLIPPED) pss_ss(fd, S_data_clipped, b2s(data_clipped(ss)));
  if (previous_files_sort(ss) != DEFAULT_PREVIOUS_FILES_SORT) pss_sd(fd, S_previous_files_sort, previous_files_sort(ss));
  if (fft_log_magnitude(ss) != DEFAULT_FFT_LOG_MAGNITUDE) pss_ss(fd, S_fft_log_magnitude, b2s(fft_log_magnitude(ss)));
  if (fft_log_frequency(ss) != DEFAULT_FFT_LOG_FREQUENCY) pss_ss(fd, S_fft_log_frequency, b2s(fft_log_frequency(ss)));
  if (print_length(ss) != DEFAULT_PRINT_LENGTH) pss_sd(fd, S_print_length, print_length(ss));
  if (show_mix_waveforms(ss) != DEFAULT_SHOW_MIX_WAVEFORMS) pss_ss(fd, S_show_mix_waveforms, b2s(show_mix_waveforms(ss)));
  if (mix_waveform_height(ss) != DEFAULT_MIX_WAVEFORM_HEIGHT) pss_sd(fd, S_mix_waveform_height, mix_waveform_height(ss));
  if (mix_tag_height(ss) != DEFAULT_MIX_TAG_HEIGHT) pss_sd(fd, S_mix_tag_height, mix_tag_height(ss));
  if (mix_tag_width(ss) != DEFAULT_MIX_TAG_WIDTH) pss_sd(fd, S_mix_tag_width, mix_tag_width(ss));
  if (enved_wave_p(ss) != DEFAULT_ENVED_WAVE_P) pss_ss(fd, S_enved_wave_p, b2s(enved_wave_p(ss)));
  if (enved_in_dB(ss) != DEFAULT_ENVED_IN_DB) pss_ss(fd, S_enved_in_dB, b2s(enved_in_dB(ss)));
  if (enved_clip_p(ss) != DEFAULT_ENVED_CLIP_P) pss_ss(fd, S_enved_clip_p, b2s(enved_clip_p(ss)));
  if (enved_style(ss) == ENVELOPE_EXPONENTIAL) pss_ss(fd, S_enved_style, TO_VAR_NAME(S_envelope_exponential));

  if (vu_font(ss)) pss_sq(fd, S_vu_font, vu_font(ss));
  if (save_state_file(ss))
    pss_sq(fd, S_save_state_file, save_state_file(ss));
  if (temp_dir(ss)) pss_sq(fd, S_temp_dir, temp_dir(ss));
  if (save_dir(ss)) pss_sq(fd, S_save_dir, save_dir(ss));
  if (ladspa_dir(ss)) pss_sq(fd, S_ladspa_dir, ladspa_dir(ss));
  if ((eps_file(ss)) && 
      ((DEFAULT_EPS_FILE == NULL) || (strcmp(eps_file(ss), DEFAULT_EPS_FILE) != 0)))
    pss_sq(fd, S_eps_file, eps_file(ss));
  if ((listener_prompt(ss)) && 
      ((DEFAULT_LISTENER_PROMPT == NULL) || (strcmp(listener_prompt(ss), DEFAULT_LISTENER_PROMPT) != 0)))
    pss_sq(fd, S_listener_prompt, listener_prompt(ss));
  if ((html_program(ss)) && (strcmp(html_program(ss), DEFAULT_HTML_PROGRAM) != 0))
    pss_sq(fd, S_html_program, html_program(ss));
  if (audio_input_device(ss) != DEFAULT_AUDIO_INPUT_DEVICE) pss_sd(fd, S_audio_input_device, audio_input_device(ss));
  if (audio_output_device(ss) != DEFAULT_AUDIO_OUTPUT_DEVICE) pss_sd(fd, S_audio_output_device, audio_output_device(ss));

  if (fneq(fft_window_beta(ss), DEFAULT_FFT_WINDOW_BETA)) pss_sf(fd, S_fft_window_beta, fft_window_beta(ss));
  if (fneq(ss->min_dB, DEFAULT_MIN_DB)) pss_sf(fd, S_min_dB, ss->min_dB);
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
  if (fneq(eps_size(ss), DEFAULT_EPS_SIZE)) pss_sf(fd, S_eps_size, eps_size(ss));

  if ((fneq(contrast_control_min(ss), DEFAULT_CONTRAST_CONTROL_MIN)) ||
      (fneq(contrast_control_max(ss), DEFAULT_CONTRAST_CONTROL_MAX)))
    pss_sl(fd, S_contrast_control_bounds, contrast_control_min(ss), contrast_control_max(ss));
  if (fneq(contrast_control_amp(ss), DEFAULT_CONTRAST_CONTROL_AMP)) pss_sf(fd, S_contrast_control_amp, contrast_control_amp(ss));
  if ((fneq(expand_control_min(ss), DEFAULT_EXPAND_CONTROL_MIN)) ||
      (fneq(expand_control_max(ss), DEFAULT_EXPAND_CONTROL_MAX)))
    pss_sl(fd, S_expand_control_bounds, expand_control_min(ss), expand_control_max(ss));
  if (fneq(expand_control_ramp(ss), DEFAULT_EXPAND_CONTROL_RAMP)) pss_sf(fd, S_expand_control_ramp, expand_control_ramp(ss));
  if (fneq(expand_control_hop(ss), DEFAULT_EXPAND_CONTROL_HOP)) pss_sf(fd, S_expand_control_hop, expand_control_hop(ss));
  if (fneq(expand_control_jitter(ss), DEFAULT_EXPAND_CONTROL_JITTER)) pss_sf(fd, S_expand_control_jitter, expand_control_jitter(ss));
  if (fneq(expand_control_length(ss), DEFAULT_EXPAND_CONTROL_LENGTH)) pss_sf(fd, S_expand_control_length, expand_control_length(ss));
  if (speed_control_tones(ss) != DEFAULT_SPEED_CONTROL_TONES) pss_sd(fd, S_speed_control_tones, speed_control_tones(ss));
  if (speed_control_style(ss) != DEFAULT_SPEED_CONTROL_STYLE) pss_ss(fd, S_speed_control_style, speed_control_style_name(speed_control_style(ss)));
  if ((fneq(speed_control_min(ss), DEFAULT_SPEED_CONTROL_MIN)) ||
      (fneq(speed_control_max(ss), DEFAULT_SPEED_CONTROL_MAX)))
    pss_sl(fd, S_speed_control_bounds, speed_control_min(ss), speed_control_max(ss));
  if ((fneq(reverb_control_scale_min(ss), DEFAULT_REVERB_CONTROL_SCALE_MIN)) ||
      (fneq(reverb_control_scale_max(ss), DEFAULT_REVERB_CONTROL_SCALE_MAX)))
    pss_sl(fd, S_reverb_control_scale_bounds, reverb_control_scale_min(ss), reverb_control_scale_max(ss));
  if ((fneq(reverb_control_length_min(ss), DEFAULT_REVERB_CONTROL_LENGTH_MIN)) ||
      (fneq(reverb_control_length_max(ss), DEFAULT_REVERB_CONTROL_LENGTH_MAX)))
    pss_sl(fd, S_reverb_control_length_bounds, reverb_control_length_min(ss), reverb_control_length_max(ss));
  if (fneq(reverb_control_feedback(ss), DEFAULT_REVERB_CONTROL_FEEDBACK)) pss_sf(fd, S_reverb_control_feedback, reverb_control_feedback(ss));
  if (fneq(reverb_control_lowpass(ss), DEFAULT_REVERB_CONTROL_LOWPASS)) pss_sf(fd, S_reverb_control_lowpass, reverb_control_lowpass(ss));
  if (fneq(reverb_control_decay(ss), DEFAULT_REVERB_CONTROL_DECAY)) pss_sf(fd, S_reverb_control_decay, reverb_control_decay(ss));
  if ((fneq(amp_control_min(ss), DEFAULT_AMP_CONTROL_MIN)) ||
      (fneq(amp_control_max(ss), DEFAULT_AMP_CONTROL_MAX)))
    pss_sl(fd, S_amp_control_bounds, amp_control_min(ss), amp_control_max(ss));
  if (filter_control_order(ss) != DEFAULT_FILTER_CONTROL_ORDER) pss_sd(fd, S_filter_control_order, filter_control_order(ss));
  if (filter_control_in_dB(ss) != DEFAULT_FILTER_CONTROL_IN_DB) pss_ss(fd, S_filter_control_in_dB, b2s(filter_control_in_dB(ss)));
  if (filter_control_in_hz(ss) != DEFAULT_FILTER_CONTROL_IN_HZ) pss_ss(fd, S_filter_control_in_hz, b2s(filter_control_in_hz(ss)));
  if (cursor_follows_play(ss) != DEFAULT_CURSOR_FOLLOWS_PLAY)
    pss_ss(fd, S_cursor_follows_play, b2s((bool)(cursor_follows_play(ss)))); /* a boolean from the user's point of view */
  if ((fneq(tempo_control_min(ss), DEFAULT_TEMPO_CONTROL_MIN)) ||
      (fneq(tempo_control_max(ss), DEFAULT_TEMPO_CONTROL_MAX)))
    pss_sl(fd, S_tempo_control_bounds, tempo_control_min(ss), tempo_control_max(ss));
  if (in_show_controls(ss) != DEFAULT_SHOW_CONTROLS) pss_ss(fd, S_show_controls, b2s(in_show_controls(ss)));

  save_recorder_state(fd);

  fprintf(fd, _("%s end of snd options\n"), XEN_COMMENT_STRING);
  if (locale)
    {
#if HAVE_SETLOCALE
      setlocale(LC_NUMERIC, locale);
#endif
      FREE(locale);
    }
}

void global_control_panel_state(void)
{
  char *buf;
  snd_help_append("\n\nCurrent control panel defaults:\n\n");
  buf = (char *)CALLOC(1024, sizeof(char));
  mus_snprintf(buf, 1024, "amp bounds: %.3f to %.3f\n", 
	       amp_control_min(ss), amp_control_max(ss));
  snd_help_append(buf);
  mus_snprintf(buf, 1024, "speed bounds: %.3f to %.3f, tones: %d, style: %s\n",
	       speed_control_min(ss), speed_control_max(ss),
	       speed_control_tones(ss),
	       speed_control_style_name(speed_control_style(ss)));
  snd_help_append(buf);
  mus_snprintf(buf, 1024, "expand bounds: %.3f to %.3f, ramp: %.3f, hop: %.3f, length: %.3f, jitter: %.3f\n",
	       expand_control_min(ss), expand_control_max(ss),
	       expand_control_ramp(ss), expand_control_hop(ss), expand_control_length(ss), expand_control_jitter(ss));
  snd_help_append(buf);
  mus_snprintf(buf, 1024, "contrast bounds: %.3f to %.3f, amp: %.3f\n",
	       contrast_control_min(ss), contrast_control_max(ss),
	       contrast_control_amp(ss));
  snd_help_append(buf);
  mus_snprintf(buf, 1024, "reverb scale: %.3f to %.3f, length: %.3f to %.3f, feedbacl: %.3f, lowpass: %.3f, decay: %.3f\n",
	       reverb_control_scale_min(ss), reverb_control_scale_max(ss),
	       reverb_control_length_min(ss), reverb_control_length_max(ss),
	       reverb_control_feedback(ss), reverb_control_lowpass(ss), reverb_control_decay(ss));
  snd_help_append(buf);
  mus_snprintf(buf, 1024, "filter order: %d, in dB: %s, in Hz: %s\n",
	       filter_control_order(ss),
	       b2s(filter_control_in_dB(ss)),
	       b2s(filter_control_in_hz(ss)));
  snd_help_append(buf);
  mus_snprintf(buf, 1024, "tempo bounds: %.3f to %.3f\n",
	       tempo_control_min(ss), tempo_control_max(ss));
  snd_help_append(buf);
  snd_help_back_to_top();
  FREE(buf);
}

static FILE *open_restart_file(char *name, bool append)
{
  FILE *fd;
  char *str;
  char *buf = NULL;
  if (!name) return(NULL);
  buf = (char *)CALLOC(256, sizeof(char));
  str = name;
  if (((*str) == '~') && (getenv("HOME") != NULL))
    {
      strcpy(buf, getenv("HOME"));
      strcat(buf, ++str);
      str = buf;
    }
  if (append)
    fd = FOPEN(str, "a");
  else fd = FOPEN(str, "w");
  FREE(buf);
  return(fd);
}

FILE *open_snd_init_file (void)
{ /* needed also by keyboard macro saver */
  return(open_restart_file(ss->init_file, true));
}

static char *save_options_or_error(void)
{
  FILE *fd;
  fd = open_snd_init_file();
  if (fd) save_snd_state_options(fd);
  if ((!fd) || (FCLOSE(fd) != 0))
    return(mus_format(_("save-options in %s hit error: %s"),
		      ss->init_file,
		      strerror(errno)));
  return(NULL);
}

int save_options(void)
{
  char *error;
  error = save_options_or_error();
  if (error)
    {
      snd_error(error);
      FREE(error);
      return(-1);
    }
  return(0);
}

#if HAVE_GUILE
static void save_property_list(FILE *fd, XEN property_list, int chan)
{
  XEN ignore_list;
  ignore_list = scm_assoc(C_STRING_TO_XEN_SYMBOL("save-state-ignore"), property_list);
  if (!(XEN_LIST_P(ignore_list)))
    {
      if (chan == -1)
	fprintf(fd, "%s(set! (%s sfile) \'%s)\n", white_space, S_sound_properties, XEN_AS_STRING(property_list));
      else fprintf(fd, "%s(set! (%s sfile %d) \'%s)\n", white_space, S_channel_properties, chan, XEN_AS_STRING(property_list));
    }
  else
    {
      XEN new_properties = XEN_EMPTY_LIST;
      int i, property_len, gc_loc;
      gc_loc = snd_protect(new_properties);
      property_len = XEN_LIST_LENGTH(property_list);
      for (i = 0; i < property_len; i++)
	{
	  XEN property;
	  property = XEN_LIST_REF(property_list, i);
	  if (XEN_FALSE_P(scm_member(XEN_CAR(property), ignore_list)))
	    new_properties = XEN_CONS(property, new_properties);
	}
      if (!(XEN_NULL_P(new_properties)))
	{
	  if (chan == -1)
	    fprintf(fd, "%s(set! (%s sfile) \'%s)\n", white_space, S_sound_properties, XEN_AS_STRING(new_properties));
	  else fprintf(fd, "%s(set! (%s sfile %d) \'%s)\n", white_space, S_channel_properties, chan, XEN_AS_STRING(new_properties));
	}
      snd_unprotect_at(gc_loc);
    }
}
#endif

static int find_sound_nth(snd_info *nsp)
{
  snd_info *sp;
  int i, which = 0;
  for (i = 0; i < nsp->index; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	if ((strcmp(nsp->short_filename, sp->short_filename) == 0) || 
	    (strcmp(nsp->filename, sp->filename) == 0))
	  which++;
    }
  return(which);
}

void open_save_sound_block(snd_info *sp, FILE *fd, bool with_nth)
{
  /* here we have to use the 'nth' arg to find_sound -- it should return #f if such an 'nth' case is not found,
   *   so that we can tell when to open another view on a given file
   */
#if HAVE_RUBY
  fprintf(fd, "begin\n  sfile = %s(\"%s\", %d)\n  if (sfile == false)\n    sfile = %s(\"%s\")\n  end\n",
	  TO_PROC_NAME(S_find_sound),
	  sp->short_filename,
	  (with_nth) ? find_sound_nth(sp) : 0,
	  TO_PROC_NAME((sp->read_only) ? S_view_sound : S_open_sound),
	  sp->filename);
  
#else
  fprintf(fd, "(let ((sfile (or (%s \"%s\" %d) (%s \"%s\"))))\n  (if sfile\n    (begin\n",
	  S_find_sound,
	  sp->short_filename, /* short filename ok because find-sound searches for that name as well as the full filename */
	  (with_nth) ? find_sound_nth(sp) : 0,
	  (sp->read_only) ? S_view_sound : S_open_sound,
	  sp->filename);
#endif
}

void close_save_sound_block(FILE *fd)
{
#if HAVE_RUBY
  fprintf(fd, "end\n");
#else
  if (!b_ok) fprintf(fd, "      #f\n"); /* avoid empty begin if no field was output */
  fprintf(fd, "      )))\n");
#endif
}

static bool default_envelope_p(env *e)
{
  return((e) &&
	 (e->pts == 2) &&
	 (e->base == 1.0) &&
	 (e->data[0] == 0.0) &&
	 (e->data[1] == 1.0) &&
	 (e->data[2] == 1.0) &&
	 (e->data[3] == 1.0));
}

static void save_sound_state (snd_info *sp, void *ptr) 
{
  /* called only after the global settings have been established, so here we can't use the DEFAULT_* macros that are ambiguous */
  int chan;
  FILE *fd;
  chan_info *cp;
  axis_info *ap;
  char *tmpstr = NULL;
  fd = (FILE *)ptr;
  open_save_sound_block(sp, fd, true);
  b_ok = false; /* can't have empty begin statement */
  if (sp->sync != DEFAULT_SYNC) psp_sd(fd, S_sync, sp->sync);
  if (sp->contrast_control_p != DEFAULT_CONTRAST_CONTROL_P) psp_ss(fd, S_contrast_control_p, b2s(sp->contrast_control_p));
  if (fneq(sp->contrast_control, DEFAULT_CONTRAST_CONTROL)) psp_sf(fd, S_contrast_control, sp->contrast_control);
  if ((fneq(sp->contrast_control_min, DEFAULT_CONTRAST_CONTROL_MIN)) ||
      (fneq(sp->contrast_control_max, DEFAULT_CONTRAST_CONTROL_MAX)))
    psp_sl(fd, S_contrast_control_bounds, sp->contrast_control_min, sp->contrast_control_max);
  if (fneq(sp->contrast_control_amp, DEFAULT_CONTRAST_CONTROL_AMP)) psp_sf(fd, S_contrast_control_amp, sp->contrast_control_amp);
  if (sp->expand_control_p != DEFAULT_EXPAND_CONTROL_P) psp_ss(fd, S_expand_control_p, b2s(sp->expand_control_p));
  if (fneq(sp->expand_control, DEFAULT_EXPAND_CONTROL)) psp_sf(fd, S_expand_control, sp->expand_control);
  if ((fneq(sp->expand_control_min, DEFAULT_EXPAND_CONTROL_MIN)) ||
      (fneq(sp->expand_control_max, DEFAULT_EXPAND_CONTROL_MAX)))
    psp_sl(fd, S_expand_control_bounds, sp->expand_control_min, sp->expand_control_max);
  if (fneq(sp->expand_control_ramp, DEFAULT_EXPAND_CONTROL_RAMP)) psp_sf(fd, S_expand_control_ramp, sp->expand_control_ramp);
  if (fneq(sp->expand_control_hop, DEFAULT_EXPAND_CONTROL_HOP)) psp_sf(fd, S_expand_control_hop, sp->expand_control_hop);
  if (fneq(sp->expand_control_jitter, DEFAULT_EXPAND_CONTROL_JITTER)) psp_sf(fd, S_expand_control_jitter, sp->expand_control_jitter);
  if (fneq(sp->expand_control_length, DEFAULT_EXPAND_CONTROL_LENGTH)) psp_sf(fd, S_expand_control_length, sp->expand_control_length);
  if (sp->speed_control_tones != DEFAULT_SPEED_CONTROL_TONES) psp_sd(fd, S_speed_control_tones, sp->speed_control_tones);
  if (sp->speed_control_style != DEFAULT_SPEED_CONTROL_STYLE) psp_ss(fd, S_speed_control_style, speed_control_style_name(sp->speed_control_style));
  if (fneq(sp->speed_control, DEFAULT_SPEED_CONTROL)) 
    {
#if HAVE_SCM_MAKE_RATIO
      if (sp->speed_control == SPEED_CONTROL_AS_RATIO)
	{
	  /* this is only Guile, so we don't need to handle the Ruby syntax */
	  fprintf(fd, "%s(set! (%s sfile) %d/%d)\n", white_space, S_speed_control, sp->speed_control_numerator, sp->speed_control_denominator);
	}
      else
#endif
      psp_sf(fd, S_speed_control, sp->speed_control);
    }
  if ((fneq(sp->speed_control_min, DEFAULT_SPEED_CONTROL_MIN)) ||
      (fneq(sp->speed_control_max, DEFAULT_SPEED_CONTROL_MAX)))
    psp_sl(fd, S_speed_control_bounds, sp->speed_control_min, sp->speed_control_max);
  if (sp->reverb_control_p != DEFAULT_REVERB_CONTROL_P) psp_ss(fd, S_reverb_control_p, b2s(sp->reverb_control_p));
  if (fneq(sp->reverb_control_scale, DEFAULT_REVERB_CONTROL_SCALE)) psp_sf(fd, S_reverb_control_scale, sp->reverb_control_scale);
  if ((fneq(sp->reverb_control_scale_min, DEFAULT_REVERB_CONTROL_SCALE_MIN)) ||
      (fneq(sp->reverb_control_scale_max, DEFAULT_REVERB_CONTROL_SCALE_MAX)))
    psp_sl(fd, S_reverb_control_scale_bounds, sp->reverb_control_scale_min, sp->reverb_control_scale_max);
  if (fneq(sp->reverb_control_length, DEFAULT_REVERB_CONTROL_LENGTH)) psp_sf(fd, S_reverb_control_length, sp->reverb_control_length);
  if ((fneq(sp->reverb_control_length_min, DEFAULT_REVERB_CONTROL_LENGTH_MIN)) ||
      (fneq(sp->reverb_control_length_max, DEFAULT_REVERB_CONTROL_LENGTH_MAX)))
    psp_sl(fd, S_reverb_control_length_bounds, sp->reverb_control_length_min, sp->reverb_control_length_max);
  if (fneq(sp->reverb_control_feedback, DEFAULT_REVERB_CONTROL_FEEDBACK)) psp_sf(fd, S_reverb_control_feedback, sp->reverb_control_feedback);
  if (fneq(sp->reverb_control_lowpass, DEFAULT_REVERB_CONTROL_LOWPASS)) psp_sf(fd, S_reverb_control_lowpass, sp->reverb_control_lowpass);
  if (fneq(sp->reverb_control_decay, DEFAULT_REVERB_CONTROL_DECAY)) psp_sf(fd, S_reverb_control_decay, sp->reverb_control_decay);
  if (fneq(sp->amp_control, DEFAULT_AMP_CONTROL)) psp_sf(fd, S_amp_control, sp->amp_control);
  if ((fneq(sp->amp_control_min, DEFAULT_AMP_CONTROL_MIN)) ||
      (fneq(sp->amp_control_max, DEFAULT_AMP_CONTROL_MAX)))
    psp_sl(fd, S_amp_control_bounds, sp->amp_control_min, sp->amp_control_max);
  if (sp->filter_control_p != DEFAULT_FILTER_CONTROL_P) psp_ss(fd, S_filter_control_p, b2s(sp->filter_control_p));
  if (sp->filter_control_order != DEFAULT_FILTER_CONTROL_ORDER) psp_sd(fd, S_filter_control_order, sp->filter_control_order);
  if (sp->filter_control_in_dB != DEFAULT_FILTER_CONTROL_IN_DB) psp_ss(fd, S_filter_control_in_dB, b2s(sp->filter_control_in_dB));
  if (sp->filter_control_in_hz != DEFAULT_FILTER_CONTROL_IN_HZ) psp_ss(fd, S_filter_control_in_hz, b2s(sp->filter_control_in_hz));
  if ((sp->filter_control_envelope) && (!(default_envelope_p(sp->filter_control_envelope))))
    {
      psp_ss(fd, S_filter_control_envelope, tmpstr = env_to_string(sp->filter_control_envelope));
      if (tmpstr) FREE(tmpstr);
    }
  if (sp->cursor_follows_play != DONT_FOLLOW) 
    psp_ss(fd, S_cursor_follows_play, b2s((bool)(sp->cursor_follows_play))); /* a boolean from the user's point of view */

  if ((XEN_VECTOR_P(sp->properties)) &&
      (XEN_LIST_P(XEN_VECTOR_REF(sp->properties, 0))) &&
      (!(XEN_NULL_P(XEN_VECTOR_REF(sp->properties, 0)))))
    {
#if HAVE_RUBY
      fprintf(fd, "%sset_%s([%s], sfile)\n", white_space, TO_PROC_NAME(S_sound_properties), XEN_AS_STRING(XEN_VECTOR_REF(sp->properties, 0)));
#else
#if HAVE_GUILE
      save_property_list(fd, XEN_VECTOR_REF(sp->properties, 0), -1);
#else
      fprintf(fd, "%s(set! (%s sfile) \'%s)\n", white_space, S_sound_properties, XEN_AS_STRING(XEN_VECTOR_REF(sp->properties, 0)));
#endif
#endif
    }
  for (chan = 0; chan < sp->nchans; chan++)
    {
      cp = sp->chans[chan];
      ap = cp->axis;
      if (!(cp->graph_time_p)) pcp_ss(fd, S_time_graph_p, b2s(cp->graph_time_p), chan);
      if (cp->graph_transform_p) pcp_ss(fd, S_transform_graph_p, b2s(cp->graph_transform_p), chan);
      if (cp->graph_lisp_p) pcp_ss(fd, S_lisp_graph_p, b2s(cp->graph_lisp_p), chan);
      if (((ap->x0 != 0.0) || (ap->x1 != 0.1)) && (ap->x1 > .0005)) pcp_sl(fd, S_x_bounds, ap->x0, ap->x1, chan);
      if ((ap->y0 != -1.0) || (ap->y1 != 1.0)) pcp_sl(fd, S_y_bounds, ap->y0, ap->y1, chan);
      if (CURSOR(cp) != 0) pcp_sod(fd, S_cursor, CURSOR(cp), chan);
      if (cp->cursor_size != DEFAULT_CURSOR_SIZE) pcp_sd(fd, S_cursor_size, cp->cursor_size, chan);
      if (cp->cursor_style != DEFAULT_CURSOR_STYLE) pcp_ss(fd, S_cursor_style, cursor_style_name(cp->cursor_style), chan);
      if (cp->show_marks != show_marks(ss)) pcp_ss(fd, S_show_marks, b2s(cp->show_marks), chan);
      if (cp->show_y_zero != show_y_zero(ss)) pcp_ss(fd, S_show_y_zero, b2s(cp->show_y_zero), chan);
      if (cp->show_grid != show_grid(ss)) pcp_ss(fd, S_show_grid, b2s(cp->show_grid), chan);
      if (cp->wavo_hop != wavo_hop(ss)) pcp_sd(fd, S_wavo_hop, cp->wavo_hop, chan);
      if (cp->wavo_trace != wavo_trace(ss)) pcp_sd(fd, S_wavo_trace, cp->wavo_trace, chan);
      if (cp->max_transform_peaks != max_transform_peaks(ss)) pcp_sd(fd, S_max_transform_peaks, cp->max_transform_peaks, chan);
      if (cp->show_transform_peaks != show_transform_peaks(ss)) pcp_ss(fd, S_show_transform_peaks, b2s(cp->show_transform_peaks), chan);
      if (cp->fft_log_frequency != fft_log_frequency(ss)) pcp_ss(fd, S_fft_log_frequency, b2s(cp->fft_log_frequency), chan);
      if (cp->fft_log_magnitude != fft_log_magnitude(ss)) pcp_ss(fd, S_fft_log_magnitude, b2s(cp->fft_log_magnitude), chan);
      if (cp->verbose_cursor != verbose_cursor(ss)) pcp_ss(fd, S_verbose_cursor, b2s(cp->verbose_cursor), chan);
      if (cp->zero_pad != zero_pad(ss)) pcp_sd(fd, S_zero_pad, cp->zero_pad, chan);
      if (cp->wavelet_type != wavelet_type(ss)) pcp_sd(fd, S_wavelet_type, cp->wavelet_type, chan);
      if (fneq(cp->min_dB, ss->min_dB)) pcp_sf(fd, S_min_dB, cp->min_dB, chan);
      if (fneq(cp->spectro_x_angle, spectro_x_angle(ss))) pcp_sf(fd, S_spectro_x_angle, cp->spectro_x_angle, chan);
      if (fneq(cp->spectro_y_angle, spectro_y_angle(ss))) pcp_sf(fd, S_spectro_y_angle, cp->spectro_y_angle, chan);
      if (fneq(cp->spectro_z_angle, spectro_z_angle(ss))) pcp_sf(fd, S_spectro_z_angle, cp->spectro_z_angle, chan);
      if (fneq(cp->spectro_x_scale, spectro_x_scale(ss))) pcp_sf(fd, S_spectro_x_scale, cp->spectro_x_scale, chan);
      if (fneq(cp->spectro_y_scale, spectro_y_scale(ss))) pcp_sf(fd, S_spectro_y_scale, cp->spectro_y_scale, chan);
      if (fneq(cp->spectro_z_scale, spectro_z_scale(ss))) pcp_sf(fd, S_spectro_z_scale, cp->spectro_z_scale, chan);
      if (fneq(cp->spectro_cutoff, spectro_cutoff(ss))) pcp_sf(fd, S_spectro_cutoff, cp->spectro_cutoff, chan);
      if (fneq(cp->spectro_start, spectro_start(ss))) pcp_sf(fd, S_spectro_start, cp->spectro_start, chan);
      if (fneq(cp->fft_window_beta, fft_window_beta(ss))) pcp_sf(fd, S_fft_window_beta, cp->fft_window_beta, chan);
      if (cp->spectro_hop != spectro_hop(ss)) pcp_sd(fd, S_spectro_hop, cp->spectro_hop, chan);
      if (cp->transform_size != transform_size(ss)) pcp_sd(fd, S_transform_size, cp->transform_size, chan);
      if (cp->transform_graph_type != transform_graph_type(ss)) pcp_ss(fd, S_transform_graph_type, transform_graph_type_name(cp->transform_graph_type), chan);
      if (cp->time_graph_type != time_graph_type(ss)) pcp_ss(fd, S_time_graph_type, time_graph_type_name(cp->time_graph_type), chan);
      if (cp->fft_window != fft_window(ss)) pcp_ss(fd, S_fft_window, TO_VAR_NAME(mus_fft_window_name(cp->fft_window)), chan);
      if (cp->transform_type != transform_type(ss)) pcp_ss(fd, S_transform_type, TO_VAR_NAME(transform_type_name(cp->transform_type)), chan);
      /* this is assuming the added transform definition (if any) can be found -- maybe not a good idea */
      if (cp->transform_normalization != transform_normalization(ss)) pcp_ss(fd, S_transform_normalization, transform_normalization_name(cp->transform_normalization), chan);
      if (cp->time_graph_style != graph_style(ss)) pcp_ss(fd, S_time_graph_style, graph_style_name(cp->time_graph_style), chan);
      if (cp->lisp_graph_style != graph_style(ss)) pcp_ss(fd, S_lisp_graph_style, graph_style_name(cp->lisp_graph_style), chan);
      if (cp->transform_graph_style != graph_style(ss)) pcp_ss(fd, S_transform_graph_style, graph_style_name(cp->transform_graph_style), chan);
      if (cp->show_mix_waveforms != show_mix_waveforms(ss)) pcp_ss(fd, S_show_mix_waveforms, b2s(cp->show_mix_waveforms), chan);
      if (cp->dot_size != dot_size(ss)) pcp_sd(fd, S_dot_size, cp->dot_size, chan);
      if (cp->x_axis_style != x_axis_style(ss)) pcp_ss(fd, S_x_axis_style, x_axis_style_name(cp->x_axis_style), chan);
      if (cp->beats_per_minute != beats_per_minute(ss)) pcp_sf(fd, S_beats_per_minute, cp->beats_per_minute, chan);
      if (cp->show_axes != show_axes(ss)) pcp_ss(fd, S_show_axes, show_axes2string(cp->show_axes), chan);
      if (cp->graphs_horizontal != graphs_horizontal(ss)) pcp_ss(fd, S_graphs_horizontal, b2s(cp->graphs_horizontal), chan);
      if ((XEN_VECTOR_P(cp->properties)) &&
	  (XEN_LIST_P(XEN_VECTOR_REF(cp->properties, 0))) &&
	  (!(XEN_NULL_P(XEN_VECTOR_REF(cp->properties, 0)))))
	{
#if HAVE_RUBY
	  fprintf(fd, "%sset_%s([%s], sfile, %d)\n", 
		  white_space, 
		  TO_PROC_NAME(S_channel_properties), 
		  XEN_AS_STRING(XEN_VECTOR_REF(cp->properties, 0)),
		  chan);
#else
#if HAVE_GUILE
	  save_property_list(fd, XEN_VECTOR_REF(cp->properties, 0), chan);
#else
	  fprintf(fd, "%s(set! (%s sfile %d) \'%s)\n", 
		  white_space, 
		  S_channel_properties, 
		  chan,
		  XEN_AS_STRING(XEN_VECTOR_REF(cp->properties, 0)));
#endif
#endif
	}
      edit_history_to_file(fd, cp);
    }
  close_save_sound_block(fd);
}

static XEN after_save_state_hook;

static char *save_state_or_error (char *save_state_name)
{
#if HAVE_RUBY
  #define BPAREN ""
  #define EPAREN ""
#else
  #define BPAREN "("
  #define EPAREN ")"
#endif
  FILE *save_fd;
  char *locale = NULL;
  save_fd = open_restart_file(save_state_name, false);
  if (save_fd == NULL) 
    return(mus_format(_("can't write %s: %s"), 
		      save_state_name, 
		      strerror(errno)));
  else
    {
#if HAVE_SETLOCALE
      locale = copy_string(setlocale(LC_NUMERIC, "C")); /* must use decimal point in floats since Scheme assumes that format */
#endif
      save_prevlist(save_fd);                                 /* list of previous files (View: Files option) */
      save_snd_state_options(save_fd);                    /* options = user-settable global state variables */
      /* the global settings need to precede possible local settings */
      for_each_sound(save_sound_state, (void *)save_fd);  /* current sound state -- will traverse chans */
      save_macro_state(save_fd);                              /* current unsaved keyboard macros (snd-chn.c) */
      save_envelope_editor_state(save_fd);                    /* current envelope editor window state */
      save_regions(save_fd);                              /* regions */
      if (transform_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_transform_dialog));
      if (enved_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_enved_dialog));
      if (color_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_color_dialog));
      if (orientation_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_orientation_dialog));
      if (view_files_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_view_files_dialog));
      if (region_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_view_regions_dialog));
      if (record_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_recorder_dialog));
      /* the problem here (with saving hooks) is that it is not straightforward to save the function source
       *   (with the current print-set! source option, or with an earlier procedure->string function using
       *   procedure_environment etc); many types print in this case in ways that are not readable.
       *   The functions may depend on globals that are not in loaded files, or that were changed since
       *   loading, and trying to map over the current module's obarray, saving each such variable in
       *   its current form, is a major undertaking (although this can be done for simple vars); additionally, 
       *   what if the user has changed these
       *   before restoring -- should the old forms be restored?  Perhaps the new files associated
       *   with dumping (libguile/dump.c) will address this issue.  And, things like search functions
       *   and hooks might be viewed as temporary to begin with. If the function source is long,
       *   some sort of pretty-printer is really needed, but I couldn't get slib's to work.
       */
      if (locale)
	{
#if HAVE_SETLOCALE
	  setlocale(LC_NUMERIC, locale);
#endif
	  FREE(locale);
	}
      snd_fclose(save_fd, save_state_name);

      if (XEN_HOOKED(after_save_state_hook))
	run_hook(after_save_state_hook, 
		 XEN_LIST_1(C_TO_XEN_STRING(save_state_name)),
		 S_after_save_state_hook);
    }
  return(NULL);
}

int save_state (char *save_state_name)
{
  char *error;
  error = save_state_or_error(save_state_name);
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

static XEN start_hook;

static bool dont_start(char *filename)
{
  XEN res = XEN_FALSE;
  if (XEN_HOOKED(start_hook))
    res = run_or_hook(start_hook,
		      XEN_LIST_1(C_TO_XEN_STRING(filename)),
		      S_start_hook);
  return(XEN_TRUE_P(res));
}

static char *startup_filename = NULL;
static int script_arg = 0, script_argn = 0;
static char **script_args;

static XEN g_script_arg(void) 
{
  #define H_script_arg "(" S_script_arg "): where we are in the startup arg list"
  return(C_TO_XEN_INT(script_arg));
}

static XEN g_set_script_arg(XEN arg) {script_arg = XEN_TO_C_INT(arg); return(arg);}
static XEN g_script_args(void)
{
  #define H_script_args "(" S_script_args "): the args passed to Snd at startup as a list of strings"
  XEN lst = XEN_EMPTY_LIST;
  int i;
  for (i = script_argn - 1; i >= 0; i--)
    lst = XEN_CONS(C_TO_XEN_STRING(script_args[i]), lst);
  return(lst);
}

int handle_next_startup_arg(int auto_open_ctr, char **auto_open_file_names, bool with_title, int args)
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
	  (strcmp("-nostdin", argname) == 0) ||
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
	      if ((auto_open_ctr >= args) ||
		  (auto_open_file_names[auto_open_ctr] == NULL))
		snd_error(_("%s but no directory to preload?"), argname);
	      else add_directory_to_prevlist(auto_open_file_names[auto_open_ctr]);
	    }
	  else
	    {
	      if ((strcmp("-l", argname) == 0) ||
		  (strcmp("-load", argname) == 0) ||
		  (strcmp("-b", argname) == 0) ||
		  (strcmp("-batch", argname) == 0) ||
		  ((file_extension(argname)) && 
		   ((strcmp(file_extension(argname), "scm") == 0) ||
		    (strcmp(file_extension(argname), "cl") == 0) ||
		    (strcmp(file_extension(argname), "lisp") == 0) ||
		    (strcmp(file_extension(argname), "rb") == 0))))
		{
		  if ((strcmp("-l", argname) == 0) || 
		      (strcmp("-load", argname) == 0) ||
		      (strcmp("-b", argname) == 0) || 
		      (strcmp("-batch", argname) == 0))
		    auto_open_ctr++;
		  if ((auto_open_ctr >= args) ||
		      (auto_open_file_names[auto_open_ctr] == NULL))
		    snd_error(_("%s but no file to load?"), argname);
		  else 
		    {
		      script_arg = auto_open_ctr;
		      script_argn = args;
		      script_args = auto_open_file_names;
		      snd_load_file(auto_open_file_names[auto_open_ctr]);
		      if (script_arg > auto_open_ctr)
			auto_open_ctr = script_arg;
		    }
		}
	      else
		{
		  if ((strcmp("-e", argname) == 0) ||
		      (strcmp("-eval", argname) == 0))
		    {
		      /* evaluate expression */
		      auto_open_ctr++;
		      if ((auto_open_ctr >= args) ||
			  (auto_open_file_names[auto_open_ctr] == NULL))
			snd_error(_("%s but no form to evaluate?"), argname);
		      else snd_eval_str(auto_open_file_names[auto_open_ctr]);
		    }
		  else
		    {
		      if ((with_title) && 
			  (strcmp("-title", argname) == 0))
			{
			  auto_open_ctr++;
			  if ((auto_open_ctr >= args) ||
			      (auto_open_file_names[auto_open_ctr] == NULL))
			    snd_error(_("-title but no title?")); /* for gtk -- Xt handles the Motif case */
			  else ss->startup_title = copy_string(auto_open_file_names[auto_open_ctr]);
			}
		      else
			{
			  if (strcmp("-I", argname) == 0)
			    {
			      /* added 24-Oct-02: add to load path in either extension language */
			      auto_open_ctr++;
			      if ((auto_open_ctr >= args) ||
				  (auto_open_file_names[auto_open_ctr] == NULL))
				snd_error(_("-I but no path?"));
			      else 
				{
#if HAVE_RUBY
				  extern VALUE rb_load_path; /* prepend -I as they appear (kinda unintuitive) */
				  rb_ary_unshift(rb_load_path, rb_str_new2(auto_open_file_names[auto_open_ctr]));
#else
  #if HAVE_GUILE
				  char buf[256];
				  sprintf(buf, "(set! %%load-path (cons \"%s\" %%load-path))", auto_open_file_names[auto_open_ctr]);
				  XEN_EVAL_C_STRING(buf);
  #endif
#endif
				}
			    }
			  else
			    {
			      if (startup_filename == NULL)
				{
				  startup_filename = copy_string(argname);
				  if (dont_start(startup_filename)) snd_exit(1);
				}
			      snd_open_file_unselected(argname, false);
			    }
			}
		    }
		}
	    }
	}
    }
  return(auto_open_ctr + 1);
}

static XEN g_save_state(XEN filename) 
{
  #define H_save_state "(" S_save_state " filename): save the current Snd state in filename; (load filename) restores it)"
  char *error;
  XEN result;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_save_state, "a string");
  error = save_state_or_error(XEN_TO_C_STRING(filename));
  if (error)
    {
      result = C_TO_XEN_STRING(error);
      FREE(error);
      XEN_ERROR(CANNOT_SAVE,
		XEN_LIST_3(C_TO_XEN_STRING(S_save_state),
			   filename,
			   result));
    }
  return(filename);
}

static XEN g_save_options(XEN filename)
{
  #define H_save_options "(" S_save_options " filename): save Snd options in filename"
  char *name = NULL;
  FILE *fd;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_save_options, "a string");
  name = mus_expand_filename(XEN_TO_C_STRING(filename));
  fd = FOPEN(name, "w");
  if (name) FREE(name);
  if (fd) 
    save_snd_state_options(fd);
  if ((!fd) || 
      (FCLOSE(fd) != 0))
    XEN_ERROR(CANNOT_SAVE, 
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_options),
			 filename,
			 C_TO_XEN_STRING(strerror(errno))));
  return(filename);
}

static XEN g_exit(XEN val) 
{
  #define H_exit "(" S_exit "): exit Snd"
  if (snd_exit_cleanly(false))
    snd_exit(XEN_TO_C_INT_OR_ELSE(val, 1)); 
  return(XEN_FALSE);
}

#if DEBUG_MEMORY
static XEN g_mem_report(void) 
{
  mem_report(); 
  return(XEN_FALSE);
}
#endif

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_save_options_w, g_save_options)
XEN_NARGIFY_1(g_save_state_w, g_save_state)
XEN_ARGIFY_1(g_exit_w, g_exit)
#if DEBUG_MEMORY
  XEN_NARGIFY_0(g_mem_report_w, g_mem_report)
#endif
XEN_NARGIFY_0(g_script_arg_w, g_script_arg)
XEN_NARGIFY_1(g_set_script_arg_w, g_set_script_arg)
XEN_NARGIFY_0(g_script_args_w, g_script_args)
#else
#define g_save_options_w g_save_options
#define g_save_state_w g_save_state
#define g_exit_w g_exit
#if DEBUG_MEMORY
  #define g_mem_report_w g_mem_report
#endif
#define g_script_arg_w g_script_arg
#define g_set_script_arg_w g_set_script_arg
#define g_script_args_w g_script_args
#endif

void g_init_main(void)
{
  XEN_DEFINE_PROCEDURE(S_save_options, g_save_options_w, 1, 0, 0, H_save_options);
  XEN_DEFINE_PROCEDURE(S_save_state,   g_save_state_w,   1, 0, 0, H_save_state);
  XEN_DEFINE_PROCEDURE(S_exit,         g_exit_w,         0, 1, 0, H_exit);

#if DEBUG_MEMORY
  XEN_DEFINE_PROCEDURE("mem-report",   g_mem_report_w, 0, 0, 0, "(mem-report) writes memory usage stats to memlog");
#endif

  #define H_start_hook S_start_hook " (filename): called upon start-up. If it returns #t, snd exits immediately."
  XEN_DEFINE_HOOK(start_hook, S_start_hook, 1, H_start_hook);                   /* arg = argv filename if any */

  #define H_exit_hook S_exit_hook " (): called upon exit. \
If it returns #t, Snd does not exit.  This can be used to check for unsaved edits, or to perform cleanup activities."

  XEN_DEFINE_HOOK(exit_hook, S_exit_hook, 0, H_exit_hook);

  #define H_after_save_state_hook S_after_save_state_hook " (filename): called after Snd state has been saved; \
filename is the save state file."
  XEN_DEFINE_HOOK(after_save_state_hook, S_after_save_state_hook, 1, H_after_save_state_hook);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_script_arg, g_script_arg_w, H_script_arg, S_setB S_script_arg, g_set_script_arg_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE(S_script_args, g_script_args_w, 0, 0, 0, H_script_args);
}
