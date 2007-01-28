#include "snd.h"
#include "clm-strings.h"
#include "sndlib-strings.h"
#include "clm2xen.h"

static void remove_temp_files(chan_info *cp)
{
  free_sound_list(cp);
  free_mix_list(cp);
}

static XEN exit_hook;
static XEN before_exit_hook;

bool snd_exit_cleanly(bool force_exit)
{  
  XEN res = XEN_FALSE;
  ss->exiting = true; /* if segfault during exit code, don't try to restart at event loop! */

  /* before-exit-hook can cancel the exit, whereas exit-hook can't */
  if (XEN_HOOKED(before_exit_hook))
    res = run_or_hook(before_exit_hook, 
		      XEN_EMPTY_LIST,
		      S_before_exit_hook);
  if ((XEN_TRUE_P(res)) && (!force_exit)) return(false); /* does it make any sense to call this hook if we're forced to exit anyway? */

  if (XEN_HOOKED(exit_hook))
    run_hook(exit_hook, 
	     XEN_EMPTY_LIST,
	     S_exit_hook);

#if HAVE_FAM
  if (ss->fam_ok)
    {
      cleanup_edit_header_watcher();
      cleanup_new_file_watcher();
      if (ss->fam_connection)
	FAMClose(ss->fam_connection);
      ss->fam_connection = NULL;
    }
#endif
  cleanup_dac();
  for_each_normal_chan(remove_temp_files);
  cleanup_region_temp_files();
  cleanup_recording();
  forget_temps();
#if MUS_DEBUGGING
  clear_listener_strings();
  mem_report();
#endif
  return(true);
}

void sound_not_current(snd_info *sp)
{
  /* check for change in update status */
  bool needs_update;
  if (ss->fam_ok) return;
  needs_update = (file_write_date(sp->filename) != sp->write_date);
  if (needs_update != sp->need_update)
    {
      sp->need_update = needs_update;
      if (needs_update)
	{
	  if (auto_update(ss))
	    snd_update(sp); /* will stop bomb via need_update flag */
	  else start_bomb(sp);
	}
      else stop_bomb(sp);
    }
}


/* -------- ss watcher lists -------- */

#define SS_WATCHER_SIZE_INCREMENT 2

int add_ss_watcher(ss_watcher_t type, void (*watcher)(ss_watcher_reason_t reason, void *data), void *context)
{
  int loc = -1;
  if (!(ss->watchers))
    {
      loc = 0;
      ss->watchers_size = SS_WATCHER_SIZE_INCREMENT;
      ss->watchers = (ss_watcher **)CALLOC(ss->watchers_size, sizeof(ss_watcher *));
    }
  else
    {
      int i;
      for (i = 0; i < ss->watchers_size; i++)
	if (!(ss->watchers[i]))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = ss->watchers_size;
	  ss->watchers_size += SS_WATCHER_SIZE_INCREMENT;
	  ss->watchers = (ss_watcher **)REALLOC(ss->watchers, ss->watchers_size * sizeof(ss_watcher *));
	  for (i = loc; i < ss->watchers_size; i++) ss->watchers[i] = NULL;
	}
    }
  ss->watchers[loc] = (ss_watcher *)CALLOC(1, sizeof(ss_watcher));
  ss->watchers[loc]->watcher = watcher;
  ss->watchers[loc]->context = context;
  ss->watchers[loc]->loc = loc;
  ss->watchers[loc]->type = type;
  return(loc);
}

void call_ss_watchers(ss_watcher_t type, ss_watcher_reason_t reason)
{
  if (ss->watchers)
    {
      int i;
      for (i = 0; i < ss->watchers_size; i++)
	if ((ss->watchers[i]) &&
	    ((type == SS_ANY_WATCHER) ||
	     (ss->watchers[i]->type == type)))
	  (*(ss->watchers[i]->watcher))(reason, ss->watchers[i]->context);
    }
  run_watchers();
}



/* ---------------- save sound state (options, or entire state) ---------------- */

#if HAVE_GUILE

static void save_loaded_files_list(FILE *fd, const char *current_filename)
{
  /* make sure all previously loaded code is available */
  XEN old_list;
  char *full_name;
  int len;
  old_list = XEN_NAME_AS_C_STRING_TO_VALUE("*snd-loaded-files*");
  len = XEN_LIST_LENGTH(old_list);
  if (len > 0)
    {
      char **files;
      int i, gc_loc, new_files = 0;
      full_name = mus_expand_filename(current_filename);
      gc_loc = snd_protect(old_list);
      files = (char **)CALLOC(len, sizeof(char *));
      for (i = len - 1; i >= 0; i--)
	{
	  char *curfile;
	  curfile = XEN_TO_C_STRING(XEN_LIST_REF(old_list, i));
	  if ((strcmp(curfile, current_filename) != 0) &&
	      (strcmp(curfile, full_name) != 0) &&
	      (mus_file_probe(curfile)))
	    files[new_files++] = copy_string(curfile);
	}
      snd_unprotect_at(gc_loc);
      FREE(full_name);
      if (new_files > 0)
	{
	  fprintf(fd, ";;; reload any missing files\n");
	  fprintf(fd, "(for-each\n  (lambda (file)\n");
	  fprintf(fd, "    (if (and (not (member file *snd-loaded-files*))\n             (file-exists? file))\n        (load file)))\n");
	  fprintf(fd, "  '(");
	  for (i = 0; i < new_files; i++)
	    fprintf(fd, "\"%s\" ", files[i]);
	  fprintf(fd, "))\n\n");
	}
      for (i = 0; i < new_files; i++) FREE(files[i]);
      FREE(files);
    }
}
#endif

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
    case SHOW_BARE_X_AXIS:       return(TO_VAR_NAME(S_show_bare_x_axis));         break;
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
      /* proc?? */
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
    case X_AXIS_AS_CLOCK:      return(TO_VAR_NAME(S_x_axis_as_clock));      break;
    case X_AXIS_IN_SAMPLES:    return(TO_VAR_NAME(S_x_axis_in_samples));    break;
    case X_AXIS_AS_PERCENTAGE: return(TO_VAR_NAME(S_x_axis_as_percentage)); break;
    case X_AXIS_IN_BEATS:      return(TO_VAR_NAME(S_x_axis_in_beats));      break;
    case X_AXIS_IN_MEASURES:   return(TO_VAR_NAME(S_x_axis_in_measures));   break;
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

static char *b2s(bool val) {return((val) ? (char *)PROC_TRUE : (char *)PROC_FALSE);} /* cast needed by g++ > 3.4 */

#define white_space "      "
static bool b_ok = false;

#if HAVE_RUBY
static void pss_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "set_%s(%s)\n", TO_PROC_NAME(name), val);}
static void pss_sq(FILE *fd, const char *name, const char *val) {fprintf(fd, "set_%s(\"%s\")\n", TO_PROC_NAME(name), val);}
static void pss_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "set_%s(%d)\n", TO_PROC_NAME(name), val);}
static void pss_sf(FILE *fd, const char *name, Float val) {fprintf(fd, "set_%s(%.4f)\n", TO_PROC_NAME(name), val);}

static void pss_sl(FILE *fd, const char *name, Float val1, Float val2) 
  {fprintf(fd, "set_%s([%f, %f])\n", TO_PROC_NAME(name), val1, val2);}

static void psp_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "%sset_%s(%s, sfile)\n", white_space, TO_PROC_NAME(name), val);}
static void psp_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "%sset_%s(%d, sfile)\n", white_space, TO_PROC_NAME(name), val);}
static void psp_sf(FILE *fd, const char *name, Float val) {fprintf(fd, "%sset_%s(%.4f, sfile)\n", white_space, TO_PROC_NAME(name), val);}

static void psp_sl(FILE *fd, const char *name, Float val1, Float val2) 
  {fprintf(fd, "%sset_%s([%f, %f], sfile)\n", white_space, TO_PROC_NAME(name), val1, val2);}

static void pcp_ss(FILE *fd, const char *name, const char *val, int chan) 
  {fprintf(fd, "%sset_%s(%s, sfile, %d)\n", white_space, TO_PROC_NAME(name), val, chan);}

static void pcp_sss(FILE *fd, const char *name, const char *val, int chan, const char *grf) 
  {fprintf(fd, "%sset_%s(\"%s\", sfile, %d, %s)\n", white_space, TO_PROC_NAME(name), val, chan, TO_VAR_NAME(grf));}

static void pcp_sd(FILE *fd, const char *name, int val, int chan)   
  {fprintf(fd, "%sset_%s(%d, sfile, %d)\n", white_space, TO_PROC_NAME(name), val, chan);}

static void pcp_sod(FILE *fd, const char *name, off_t val, int chan)   
  {fprintf(fd, "%sset_%s(" OFF_TD ", sfile, %d)\n", white_space, TO_PROC_NAME(name), val, chan);}

static void pcp_sf(FILE *fd, const char *name, Float val, int chan) 
  {fprintf(fd, "%sset_%s(%.4f, sfile, %d)\n", white_space, TO_PROC_NAME(name), val, chan);}

static void pcp_sl(FILE *fd, const char *name, Float val1, Float val2, int chan) 
  {fprintf(fd, "%sset_%s([%f, %f], sfile, %d)\n", white_space, TO_PROC_NAME(name), val1, val2, chan);}
#endif

#if HAVE_FORTH
static void pss_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "%s set-%s drop\n", val, name);}
static void pss_sq(FILE *fd, const char *name, const char *val) {fprintf(fd, "\"%s\" set-%s drop\n", val, name);}
static void pss_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "%d set-%s drop\n", val, name);}
static void pss_sf(FILE *fd, const char *name, Float val) {fprintf(fd, "%.4f set-%s drop\n", val, name);}
static void pss_sl(FILE *fd, const char *name, Float val1, Float val2) 
{fprintf(fd, "%s'( %f %f ) set-%s drop\n", white_space, val1, val2, name);}

static void psp_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "%s%s sfile set-%s drop\n", white_space, val, name);}
static void psp_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "%s%d sfile set-%s drop\n", white_space, val, name);}
static void psp_sf(FILE *fd, const char *name, Float val) {fprintf(fd, "%s%.4f sfile set-%s drop\n", white_space, val, name);}

static void psp_sl(FILE *fd, const char *name, Float val1, Float val2) 
  {fprintf(fd, "%s'( %f %f ) sfile set-%s drop\n", white_space, val1, val2, name);}

static void pcp_ss(FILE *fd, const char *name, const char *val, int chan) 
  {fprintf(fd, "%s%s sfile %d set-%s drop\n", white_space, val, chan, name);}

static void pcp_sss(FILE *fd, const char *name, const char *val, int chan, const char *grf) 
  {fprintf(fd, "%s\"%s\" sfile %d %s set-%s drop\n", white_space, val, chan, grf, name);}

static void pcp_sd(FILE *fd, const char *name, int val, int chan)   
  {fprintf(fd, "%s%d sfile %d set-%s drop\n", white_space, val, chan, name);}

static void pcp_sod(FILE *fd, const char *name, off_t val, int chan)   
  {fprintf(fd, "%s" OFF_TD " sfile %d set-%s drop\n", white_space, val, chan, name);}

static void pcp_sf(FILE *fd, const char *name, Float val, int chan) 
  {fprintf(fd, "%s%.4f sfile %d set-%s drop\n", white_space, val, chan, name);}

static void pcp_sl(FILE *fd, const char *name, Float val1, Float val2, int chan) 
  {fprintf(fd, "%s'( %f %f ) sfile %d set-%s drop\n", white_space, val1, val2, chan, name);}
#endif

#if HAVE_SCHEME || (!HAVE_EXTENSION_LANGUAGE)
static void pss_ss(FILE *fd, const char *name, const char *val) {fprintf(fd, "(set! (%s) %s)\n", name, val);}
static void pss_sq(FILE *fd, const char *name, const char *val) {fprintf(fd, "(set! (%s) \"%s\")\n", name, val);}
static void pss_sd(FILE *fd, const char *name, int val)   {fprintf(fd, "(set! (%s) %d)\n", name, val);}
static void pss_sf(FILE *fd, const char *name, Float val) {fprintf(fd, "(set! (%s) %.4f)\n", name, val);}
static void pss_sl(FILE *fd, const char *name, Float val1, Float val2) {fprintf(fd, "(set! (%s) (list %f %f))\n", name, val1, val2);}

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

static void pcp_sss(FILE *fd, const char *name, const char *val, int chan, const char *grf) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d %s) \"%s\")\n", white_space, name, chan, grf, val);}

static void pcp_sd(FILE *fd, const char *name, int val, int chan)   
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %d)\n", white_space, name, chan, val);}

static void pcp_sod(FILE *fd, const char *name, off_t val, int chan)   
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) " OFF_TD ")\n", white_space, name, chan, val);}

static void pcp_sf(FILE *fd, const char *name, Float val, int chan) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) %.4f)\n", white_space, name, chan, val);}

static void pcp_sl(FILE *fd, const char *name, Float val1, Float val2, int chan) 
  {b_ok = true; fprintf(fd, "%s(set! (%s sfile %d) (list %f %f))\n", white_space, name, chan, val1, val2);}
#endif

static void save_options(FILE *fd)
{
  char *locale = NULL;

#if HAVE_SETLOCALE
  locale = copy_string(setlocale(LC_NUMERIC, "C")); /* must use decimal point in floats since Scheme assumes that format */
#endif

  fprintf(fd, "\n%s Snd %s (%s) options saved %s\n", XEN_COMMENT_STRING, SND_VERSION, SND_DATE, snd_local_time());

  if (transform_size(ss) != DEFAULT_TRANSFORM_SIZE) pss_sd(fd, S_transform_size, transform_size(ss));
  if (minibuffer_history_length(ss) != DEFAULT_MINIBUFFER_HISTORY_LENGTH) pss_sd(fd, S_minibuffer_history_length, minibuffer_history_length(ss));
  if (fft_window(ss) != DEFAULT_FFT_WINDOW) pss_ss(fd, S_fft_window, TO_VAR_NAME(mus_fft_window_name(fft_window(ss))));
  if (transform_graph_type(ss) != DEFAULT_TRANSFORM_GRAPH_TYPE) pss_ss(fd, S_transform_graph_type, transform_graph_type_name(transform_graph_type(ss)));
  if (time_graph_type(ss) != DEFAULT_TIME_GRAPH_TYPE) pss_ss(fd, S_time_graph_type, time_graph_type_name(time_graph_type(ss)));
  if (x_axis_style(ss) != DEFAULT_X_AXIS_STYLE) pss_ss(fd, S_x_axis_style, x_axis_style_name(x_axis_style(ss)));
  if (fneq(beats_per_minute(ss), DEFAULT_BEATS_PER_MINUTE)) pss_sf(fd, S_beats_per_minute, beats_per_minute(ss));
  if (beats_per_measure(ss) != DEFAULT_BEATS_PER_MEASURE) pss_sd(fd, S_beats_per_measure, beats_per_measure(ss));
  if (graph_style(ss) != DEFAULT_GRAPH_STYLE) pss_ss(fd, S_graph_style, graph_style_name(graph_style(ss)));
  if (region_graph_style(ss) != DEFAULT_GRAPH_STYLE) pss_ss(fd, S_region_graph_style, graph_style_name(region_graph_style(ss)));
  if (channel_style(ss) != DEFAULT_CHANNEL_STYLE) pss_ss(fd, S_channel_style, channel_style_name(channel_style(ss)));
  if (enved_target(ss) != DEFAULT_ENVED_TARGET) pss_ss(fd, S_enved_target, enved_target_name(enved_target(ss)));
  if (transform_type(ss) != DEFAULT_TRANSFORM_TYPE) pss_ss(fd, S_transform_type, TO_VAR_NAME(transform_program_name(transform_type(ss))));
  if (zoom_focus_style(ss) != ZOOM_FOCUS_ACTIVE) pss_ss(fd, S_zoom_focus_style, zoom_focus_style_name(zoom_focus_style(ss)));
  if (transform_normalization(ss) != DEFAULT_TRANSFORM_NORMALIZATION) pss_ss(fd, S_transform_normalization, transform_normalization_name(transform_normalization(ss)));
  if (optimization(ss) != DEFAULT_OPTIMIZATION) pss_sd(fd, S_optimization, optimization(ss));
  if (trap_segfault(ss) != DEFAULT_TRAP_SEGFAULT) pss_ss(fd, S_trap_segfault, b2s(trap_segfault(ss)));
  if (with_file_monitor(ss) != DEFAULT_WITH_FILE_MONITOR) pss_ss(fd, S_with_file_monitor, b2s(with_file_monitor(ss)));
  if (just_sounds(ss) != DEFAULT_JUST_SOUNDS) pss_ss(fd, S_just_sounds, b2s(just_sounds(ss)));
  if (show_selection_transform(ss) != DEFAULT_SHOW_SELECTION_TRANSFORM) pss_ss(fd, S_show_selection_transform, b2s(show_selection_transform(ss)));
  if (with_gl(ss) != DEFAULT_WITH_GL) pss_ss(fd, S_with_gl, b2s(with_gl(ss)));
  if (with_mix_tags(ss) != DEFAULT_WITH_MIX_TAGS) pss_ss(fd, S_with_mix_tags, b2s(with_mix_tags(ss)));
  if (with_relative_panes(ss) != DEFAULT_WITH_RELATIVE_PANES) pss_ss(fd, S_with_relative_panes, b2s(with_relative_panes(ss)));
  if (sinc_width(ss) != DEFAULT_SINC_WIDTH) pss_sd(fd, S_sinc_width, sinc_width(ss));
  if (ss->init_window_width != DEFAULT_INIT_WINDOW_WIDTH) pss_sd(fd, S_window_width, ss->init_window_width);
  if (ss->init_window_height != DEFAULT_INIT_WINDOW_HEIGHT) pss_sd(fd, S_window_height, ss->init_window_height);
  if (ss->init_window_x != DEFAULT_INIT_WINDOW_X) pss_sd(fd, S_window_x, ss->init_window_x);
  if (ss->init_window_y != DEFAULT_INIT_WINDOW_Y) pss_sd(fd, S_window_y, ss->init_window_y);
  if (default_output_chans(ss) != DEFAULT_OUTPUT_CHANS) pss_sd(fd, S_default_output_chans, default_output_chans(ss));
  if (default_output_srate(ss) != DEFAULT_OUTPUT_SRATE) pss_sd(fd, S_default_output_srate, default_output_srate(ss));
  if (default_output_header_type(ss) != DEFAULT_OUTPUT_HEADER_TYPE) 
    pss_ss(fd, S_default_output_header_type, mus_header_type_to_string(default_output_header_type(ss)));
  if (default_output_data_format(ss) != DEFAULT_OUTPUT_DATA_FORMAT) 
    pss_ss(fd, S_default_output_data_format, mus_data_format_to_string(default_output_data_format(ss)));
  if (auto_resize(ss) != DEFAULT_AUTO_RESIZE) pss_ss(fd, S_auto_resize, b2s(auto_resize(ss)));
  if (graphs_horizontal(ss) != DEFAULT_GRAPHS_HORIZONTAL) pss_ss(fd, S_graphs_horizontal, b2s(graphs_horizontal(ss)));
  if (auto_update(ss) != DEFAULT_AUTO_UPDATE) pss_ss(fd, S_auto_update, b2s(auto_update(ss)));
  if (color_inverted(ss) != DEFAULT_COLOR_INVERTED) pss_ss(fd, S_color_inverted, b2s(color_inverted(ss)));
  if (zero_pad(ss) != DEFAULT_ZERO_PAD) pss_sd(fd, S_zero_pad, zero_pad(ss));
  if (ask_before_overwrite(ss) != DEFAULT_ASK_BEFORE_OVERWRITE) pss_ss(fd, S_ask_before_overwrite, b2s(ask_before_overwrite(ss)));
  if (dac_combines_channels(ss) != DEFAULT_DAC_COMBINES_CHANNELS) pss_ss(fd, S_dac_combines_channels, b2s(dac_combines_channels(ss)));
  if (wavo_hop(ss) != DEFAULT_WAVO_HOP) pss_sd(fd, S_wavo_hop, wavo_hop(ss));
  if (wavo_trace(ss) != DEFAULT_WAVO_TRACE) pss_sd(fd, S_wavo_trace, wavo_trace(ss));
  if (spectro_hop(ss) != DEFAULT_SPECTRO_HOP) pss_sd(fd, S_spectro_hop, spectro_hop(ss));
  if (color_map(ss) != DEFAULT_COLOR_MAP) pss_sd(fd, S_colormap, color_map(ss));
  if (color_map_size(ss) != DEFAULT_COLOR_MAP_SIZE) pss_sd(fd, S_colormap_size, color_map_size(ss));
  if (wavelet_type(ss) != DEFAULT_WAVELET_TYPE) pss_sd(fd, S_wavelet_type, wavelet_type(ss));
  if (cursor_style(ss) != DEFAULT_CURSOR_STYLE) pss_ss(fd, S_cursor_style, cursor_style_name(cursor_style(ss)));
  if (tracking_cursor_style(ss) != DEFAULT_TRACKING_CURSOR_STYLE) pss_ss(fd, S_tracking_cursor_style, cursor_style_name(tracking_cursor_style(ss)));
  if (cursor_size(ss) != DEFAULT_CURSOR_SIZE) pss_sd(fd, S_cursor_size, cursor_size(ss));
  if (dot_size(ss) != DEFAULT_DOT_SIZE) pss_sd(fd, S_dot_size, dot_size(ss));
  if (dac_size(ss) != DEFAULT_DAC_SIZE) pss_sd(fd, S_dac_size, dac_size(ss));
  if (selection_creates_region(ss) != DEFAULT_SELECTION_CREATES_REGION) pss_ss(fd, S_selection_creates_region, b2s(selection_creates_region(ss)));
  if (enved_filter_order(ss) != DEFAULT_ENVED_FILTER_ORDER) pss_sd(fd, S_enved_filter_order, enved_filter_order(ss));
  if (max_transform_peaks(ss) != DEFAULT_MAX_TRANSFORM_PEAKS) pss_sd(fd, S_max_transform_peaks, max_transform_peaks(ss));
  if (max_regions(ss) != DEFAULT_MAX_REGIONS) pss_sd(fd, S_max_regions, max_regions(ss));
  if (fneq(auto_update_interval(ss), DEFAULT_AUTO_UPDATE_INTERVAL)) pss_sf(fd, S_auto_update_interval, auto_update_interval(ss));
  if (fneq(cursor_update_interval(ss), DEFAULT_CURSOR_UPDATE_INTERVAL)) pss_sf(fd, S_cursor_update_interval, cursor_update_interval(ss));
  if (cursor_location_offset(ss) != DEFAULT_CURSOR_LOCATION_OFFSET) pss_sd(fd, S_cursor_location_offset, cursor_location_offset(ss));
  if (verbose_cursor(ss) != DEFAULT_VERBOSE_CURSOR) pss_ss(fd, S_with_verbose_cursor, b2s(verbose_cursor(ss)));
  if (show_indices(ss) != DEFAULT_SHOW_INDICES) pss_ss(fd, S_show_indices, b2s(show_indices(ss)));
  if (show_backtrace(ss) != DEFAULT_SHOW_BACKTRACE) pss_ss(fd, S_show_backtrace, b2s(show_backtrace(ss)));
  if (show_transform_peaks(ss) != DEFAULT_SHOW_TRANSFORM_PEAKS) pss_ss(fd, S_show_transform_peaks, b2s(show_transform_peaks(ss)));
  if (show_y_zero(ss) != DEFAULT_SHOW_Y_ZERO) pss_ss(fd, S_show_y_zero, b2s(show_y_zero(ss)));
  if (show_grid(ss) != DEFAULT_SHOW_GRID) pss_ss(fd, S_show_grid, b2s((bool)show_grid(ss)));
  if (fneq(grid_density(ss), DEFAULT_GRID_DENSITY)) pss_sf(fd, S_grid_density, grid_density(ss));
  if (show_sonogram_cursor(ss) != DEFAULT_SHOW_SONOGRAM_CURSOR) pss_ss(fd, S_show_sonogram_cursor, b2s(show_sonogram_cursor(ss)));
  if (show_axes(ss) != DEFAULT_SHOW_AXES) pss_ss(fd, S_show_axes, show_axes2string(show_axes(ss)));
  if (show_marks(ss) != DEFAULT_SHOW_MARKS) pss_ss(fd, S_show_marks, b2s(show_marks(ss)));
  if (clipping(ss) != DEFAULT_CLIPPING) pss_ss(fd, S_clipping, b2s(clipping(ss)));
  if (view_files_sort(ss) != DEFAULT_VIEW_FILES_SORT) pss_sd(fd, S_view_files_sort, view_files_sort(ss));
  if (fft_log_magnitude(ss) != DEFAULT_FFT_LOG_MAGNITUDE) pss_ss(fd, S_fft_log_magnitude, b2s(fft_log_magnitude(ss)));
  if (fft_log_frequency(ss) != DEFAULT_FFT_LOG_FREQUENCY) pss_ss(fd, S_fft_log_frequency, b2s(fft_log_frequency(ss)));
  if (print_length(ss) != DEFAULT_PRINT_LENGTH) pss_sd(fd, S_print_length, print_length(ss));
  if (show_mix_waveforms(ss) != DEFAULT_SHOW_MIX_WAVEFORMS) pss_ss(fd, S_show_mix_waveforms, b2s(show_mix_waveforms(ss)));
  if (mix_waveform_height(ss) != DEFAULT_MIX_WAVEFORM_HEIGHT) pss_sd(fd, S_mix_waveform_height, mix_waveform_height(ss));
  if (mix_tag_height(ss) != DEFAULT_MIX_TAG_HEIGHT) pss_sd(fd, S_mix_tag_height, mix_tag_height(ss));
  if (mix_tag_width(ss) != DEFAULT_MIX_TAG_WIDTH) pss_sd(fd, S_mix_tag_width, mix_tag_width(ss));
  if (mark_tag_height(ss) != DEFAULT_MARK_TAG_HEIGHT) pss_sd(fd, S_mark_tag_height, mark_tag_height(ss));
  if (mark_tag_width(ss) != DEFAULT_MARK_TAG_WIDTH) pss_sd(fd, S_mark_tag_width, mark_tag_width(ss));
  if (enved_wave_p(ss) != DEFAULT_ENVED_WAVE_P) pss_ss(fd, S_enved_wave_p, b2s(enved_wave_p(ss)));
  if (enved_in_dB(ss) != DEFAULT_ENVED_IN_DB) pss_ss(fd, S_enved_in_dB, b2s(enved_in_dB(ss)));
  if (enved_clip_p(ss) != DEFAULT_ENVED_CLIP_P) pss_ss(fd, S_enved_clip_p, b2s(enved_clip_p(ss)));
  if (enved_style(ss) == ENVELOPE_EXPONENTIAL) pss_ss(fd, S_enved_style, TO_VAR_NAME(S_envelope_exponential));

  if ((!tiny_font(ss)) || (strcmp(tiny_font(ss), DEFAULT_TINY_FONT) != 0))
    pss_sq(fd, S_tiny_font, tiny_font(ss));
  if ((!peaks_font(ss)) || (strcmp(peaks_font(ss), DEFAULT_PEAKS_FONT) != 0))
    pss_sq(fd, S_peaks_font, peaks_font(ss));
  if ((!bold_peaks_font(ss)) || (strcmp(bold_peaks_font(ss), DEFAULT_BOLD_PEAKS_FONT) != 0))
    pss_sq(fd, S_bold_peaks_font, bold_peaks_font(ss));
  if ((!axis_label_font(ss)) || (strcmp(axis_label_font(ss), DEFAULT_AXIS_LABEL_FONT) != 0))
    pss_sq(fd, S_axis_label_font, axis_label_font(ss));
  if ((!axis_numbers_font(ss)) || (strcmp(axis_numbers_font(ss), DEFAULT_AXIS_NUMBERS_FONT) != 0))
    pss_sq(fd, S_axis_numbers_font, axis_numbers_font(ss));
  if (listener_font(ss))
    pss_sq(fd, S_listener_font, listener_font(ss));
#if USE_MOTIF || USE_GTK
  if (in_graph_cursor(ss) != DEFAULT_GRAPH_CURSOR)
    pss_sd(fd, S_graph_cursor, in_graph_cursor(ss));
#endif

  save_added_sound_file_extensions(fd);
  save_added_source_file_extensions(fd);

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
  if (html_dir(ss))
    pss_sq(fd, S_html_dir, html_dir(ss));
  if (audio_input_device(ss) != DEFAULT_AUDIO_INPUT_DEVICE) pss_sd(fd, S_audio_input_device, audio_input_device(ss));
  if (audio_output_device(ss) != DEFAULT_AUDIO_OUTPUT_DEVICE) pss_sd(fd, S_audio_output_device, audio_output_device(ss));

  if (fneq(fft_window_alpha(ss), DEFAULT_FFT_WINDOW_ALPHA)) pss_sf(fd, S_fft_window_alpha, fft_window_alpha(ss));
  if (fneq(fft_window_beta(ss), DEFAULT_FFT_WINDOW_BETA)) pss_sf(fd, S_fft_window_beta, fft_window_beta(ss));
  if (fneq(min_dB(ss), DEFAULT_MIN_DB)) pss_sf(fd, S_min_dB, min_dB(ss));
  if (fneq(log_freq_start(ss), DEFAULT_LOG_FREQ_START)) pss_sf(fd, S_log_freq_start, log_freq_start(ss));
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
  if (vu_in_dB(ss) != DEFAULT_VU_IN_DB) pss_ss(fd, S_vu_in_dB, b2s(vu_in_dB(ss)));
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
  if (with_tracking_cursor(ss) != DEFAULT_WITH_TRACKING_CURSOR)
    pss_ss(fd, S_with_tracking_cursor, b2s((bool)(with_tracking_cursor(ss)))); /* a boolean from the user's point of view */
  if ((fneq(tempo_control_min(ss), DEFAULT_TEMPO_CONTROL_MIN)) ||
      (fneq(tempo_control_max(ss), DEFAULT_TEMPO_CONTROL_MAX)))
    pss_sl(fd, S_tempo_control_bounds, tempo_control_min(ss), tempo_control_max(ss));
  if (in_show_controls(ss) != DEFAULT_SHOW_CONTROLS) pss_ss(fd, S_show_controls, b2s(in_show_controls(ss)));

  save_recorder_state(fd);
  save_colors(fd);

  if (fneq(mus_srate(), MUS_DEFAULT_SAMPLING_RATE)) pss_sf(fd, S_mus_srate, mus_srate());
  if (mus_file_buffer_size() != MUS_DEFAULT_FILE_BUFFER_SIZE) pss_sd(fd, S_mus_file_buffer_size, mus_file_buffer_size());
  if (mus_array_print_length() != MUS_DEFAULT_ARRAY_PRINT_LENGTH) pss_sd(fd, S_mus_array_print_length, mus_array_print_length());
  if (clm_table_size_c() != MUS_DEFAULT_CLM_TABLE_SIZE) pss_sd(fd, S_clm_table_size, clm_table_size_c());
  {
    int srate = 0, chans = 0, format = 0;
    mus_header_raw_defaults(&srate, &chans, &format);
    if ((chans != 2) ||
	(srate != 44100) ||
	(format != MUS_BSHORT))
      {
#if HAVE_SCHEME
	fprintf(fd, "(set! (mus-header-raw-defaults) (list %d %d %s))\n",
		srate,
		chans,
		mus_data_format_to_string(format));
#endif
#if HAVE_RUBY
	fprintf(fd, "set_mus_header_raw_defaults([%d, %d, %s])\n",
		srate,
		chans,
		mus_data_format_to_string(format));
#endif
#if HAVE_FORTH
	fprintf(fd, "'( %d %d %s ) set-mus-header-raw-defaults drop\n",
		srate,
		chans,
		mus_data_format_to_string(format));
#endif
      }
  }
  
  fprintf(fd, _("%s end of snd options\n"), XEN_COMMENT_STRING);
  if (locale)
    {
#if HAVE_SETLOCALE
      setlocale(LC_NUMERIC, locale);
#endif
      FREE(locale);
    }
}

/* next two are for the help menu */
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

void global_fft_state(void)
{
  char *buf;
  snd_help_append("\n\nCurrent FFT defaults:\n\n");
  buf = (char *)CALLOC(1024, sizeof(char));
  mus_snprintf(buf, 1024, "fft size: %d\n    type: %s\n    window: %s (alpha: %.3f, beta: %.3f)\n",
	       (int)transform_size(ss), 
	       TO_VAR_NAME(transform_program_name(transform_type(ss))),
	       TO_VAR_NAME(mus_fft_window_name(fft_window(ss))),
	       fft_window_alpha(ss),
	       fft_window_beta(ss));
  snd_help_append(buf);
  mus_snprintf(buf, 1024, "    graph-type: %s\n    show-peaks: %s (max: %d)\n    show-selection-fft: %s\n",
	       transform_graph_type_name(transform_graph_type(ss)),
	       b2s(show_transform_peaks(ss)),
	       max_transform_peaks(ss),
	       b2s(show_selection_transform(ss)));
  snd_help_append(buf);
  mus_snprintf(buf, 1024, "    log freq: %s (start: %.3f)\n    dB: %s, min-dB: %.3f\n    normalization: %s\n",
	       b2s(fft_log_frequency(ss)),
	       log_freq_start(ss),
	       b2s(fft_log_magnitude(ss)),	       
	       min_dB(ss),
	       transform_normalization_name(transform_normalization(ss)));
  snd_help_append(buf);
  snd_help_back_to_top();
  FREE(buf);
}


#if HAVE_SCHEME
static void save_property_list(FILE *fd, XEN property_list, int chan)
{
  XEN ignore_list;
  ignore_list = XEN_ASSOC(C_STRING_TO_XEN_SYMBOL("save-state-ignore"), property_list);
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
	  if (XEN_FALSE_P(XEN_MEMBER(XEN_CAR(property), ignore_list)))
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

#if HAVE_RUBY
static void save_property_list(FILE *fd, XEN property_list, int chan)
{
  XEN ignore_list;
  ignore_list = rb_ary_assoc(property_list, C_STRING_TO_XEN_SYMBOL("save_state_ignore"));
  if (!(XEN_VECTOR_P(ignore_list)))
    {
      if (chan == -1)
	fprintf(fd, "%sset_%s(%s, sfile)\n",
		white_space,
		TO_PROC_NAME(S_sound_properties),
		XEN_AS_STRING(property_list));
      else fprintf(fd, "%sset_%s(%s, sfile, %d)\n",
		   white_space,
		   TO_PROC_NAME(S_channel_properties),
		   XEN_AS_STRING(property_list),
		   chan);
    }
  else
    {
      XEN ignore_vec, new_properties = XEN_EMPTY_LIST;
      int i, property_len, gc_loc;
      gc_loc = snd_protect(new_properties);
      property_len = XEN_LIST_LENGTH(property_list);
      ignore_vec = XEN_VECTOR_REF(ignore_list, 1);
      if (XEN_VECTOR_P(ignore_vec))
	{
	  for (i = 0; i < property_len; i++)
	    {
	      XEN property;
	      property = XEN_LIST_REF(property_list, i);
	      if (XEN_FALSE_P(rb_ary_includes(ignore_vec, XEN_CAR(property))))
		new_properties = XEN_CONS(property, new_properties);
	    }
	}
      else
	{
	  for (i = 0; i < property_len; i++)
	  new_properties = XEN_CONS(XEN_LIST_REF(property_list, i), new_properties);
	}
      if (!(XEN_NULL_P(new_properties)))
	{
	  if (chan == -1)
	    fprintf(fd, "%sset_%s(%s, sfile)\n",
		    white_space,
		    TO_PROC_NAME(S_sound_properties),
		    XEN_AS_STRING(new_properties));
	  else fprintf(fd, "%sset_%s(%s, sfile, %d)\n",
		       white_space,
		       TO_PROC_NAME(S_channel_properties),
		       XEN_AS_STRING(new_properties),
		       chan);
	}
      snd_unprotect_at(gc_loc);
    }
}
#endif

#if HAVE_FORTH
static void save_property_list(FILE *fd, XEN property_list, int chan)
{
  XEN ignore_list;
  ignore_list = XEN_ASSOC(C_STRING_TO_XEN_SYMBOL("save-state-ignore"), property_list);
  if (!(XEN_LIST_P(ignore_list)))
    {
      if (chan == -1)
	fprintf(fd, "%s%s sfile set-%s drop\n",
		white_space,
		fth_to_c_dump(property_list),
		S_sound_properties);
      else fprintf(fd, "%s%s sfile %d set-%s drop\n",
		   white_space,
		   fth_to_c_dump(property_list),
		   chan,
		   S_channel_properties);
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
	  if (XEN_FALSE_P(XEN_MEMBER(XEN_CAR(property), ignore_list)))
	    new_properties = XEN_CONS(property, new_properties);
	}
      if (!(XEN_NULL_P(new_properties)))
	{
	  if (chan == -1)
	    fprintf(fd, "%s%s sfile set-%s drop\n",
		    white_space,
		    fth_to_c_dump(new_properties),
		    S_sound_properties);
	  else fprintf(fd, "%s%s sfile %d set-%s drop\n",
		       white_space,
		       fth_to_c_dump(new_properties),
		       chan,
		       S_channel_properties);
	}
      snd_unprotect_at(gc_loc);
    }
}
#endif

#if (!HAVE_EXTENSION_LANGUAGE)
static void save_property_list(FILE *fd, XEN property_list, int chan) {}
#endif

static void check_selection(FILE *fd, chan_info *cp)
{
  if (selection_is_active_in_channel(cp))
    {
      off_t beg, end;
      beg = selection_beg(cp);
      end = selection_end(cp);
      pcp_ss(fd, S_selection_member, b2s(true), cp->chan);
      pcp_sod(fd, S_selection_position, beg, cp->chan);
      pcp_sod(fd, S_selection_frames, end - beg + 1, cp->chan);     
    }
}

static int find_sound_nth(snd_info *nsp)
{
  int i, which = 0;
  for (i = 0; i < nsp->index; i++)
    {
      snd_info *sp;
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
	  TO_PROC_NAME((sp->user_read_only) ? S_view_sound : S_open_sound),
	  sp->filename);
  
#endif
#if HAVE_SCHEME
  fprintf(fd, "(let ((sfile (or (%s \"%s\" %d) (%s \"%s\"))))\n  (if sfile\n    (begin\n",
	  S_find_sound,
	  sp->short_filename, /* short filename ok because find-sound searches for that name as well as the full filename */
	  (with_nth) ? find_sound_nth(sp) : 0,
	  (sp->user_read_only) ? S_view_sound : S_open_sound,
	  sp->filename);
#endif
#if HAVE_FORTH
  fprintf(fd, "\"%s\" %d %s to sfile\nsfile false? [if] \"%s\" %s to sfile [then]\n",
	  sp->short_filename,
	  (with_nth) ? find_sound_nth(sp) : 0,
	  S_find_sound,
	  sp->filename,
	  (sp->user_read_only ? S_view_sound : S_open_sound));
#endif
}

void close_save_sound_block(FILE *fd, bool need_f)
{
#if HAVE_RUBY
  fprintf(fd, "end\n");
#endif
#if HAVE_SCHEME
  if (need_f)
    fprintf(fd, "      #f)))\n"); /* avoid empty begin if no field was output */
  else fprintf(fd, "      )))\n");
#endif
#if HAVE_FORTH
  fprintf(fd, "\n");
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

static void save_sound_state(snd_info *sp, void *ptr) 
{
  /* called only after the global settings have been established, so here we can't use the DEFAULT_* macros that are ambiguous */
  int chan;
  FILE *fd;
  chan_info *cp;
  axis_info *ap;
  char *tmpstr = NULL;
  fd = (FILE *)ptr;
  open_save_sound_block(sp, fd, true);
  b_ok = false; /* can't have empty begin statement in old Guiles */
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
#if XEN_HAVE_RATIOS
      if (sp->speed_control == SPEED_CONTROL_AS_RATIO)
	{
	  /* no ratios in Ruby */
#if HAVE_FORTH
	  fprintf(fd, "%s%d/%d set-%s drop\n", white_space, sp->speed_control_numerator, sp->speed_control_denominator, S_speed_control);
#else
	  fprintf(fd, "%s(set! (%s sfile) %d/%d)\n", white_space, S_speed_control, sp->speed_control_numerator, sp->speed_control_denominator);
#endif
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
  if (sp->with_tracking_cursor != DONT_TRACK) 
    psp_ss(fd, S_with_tracking_cursor, b2s((bool)(sp->with_tracking_cursor))); /* a boolean from the user's point of view */

  if ((XEN_VECTOR_P(sp->properties)) &&
      (XEN_LIST_P(XEN_VECTOR_REF(sp->properties, 0))) &&
      (!(XEN_NULL_P(XEN_VECTOR_REF(sp->properties, 0)))))
    {
      save_property_list(fd, XEN_VECTOR_REF(sp->properties, 0), -1);
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
      if (cp->tracking_cursor_style != DEFAULT_TRACKING_CURSOR_STYLE) pcp_ss(fd, S_tracking_cursor_style, cursor_style_name(cp->tracking_cursor_style), chan);
      if (cp->show_marks != show_marks(ss)) pcp_ss(fd, S_show_marks, b2s(cp->show_marks), chan);
      if (cp->show_y_zero != show_y_zero(ss)) pcp_ss(fd, S_show_y_zero, b2s(cp->show_y_zero), chan);
      if (cp->show_grid != show_grid(ss)) pcp_ss(fd, S_show_grid, b2s((bool)(cp->show_grid)), chan);
      if (cp->show_sonogram_cursor != show_sonogram_cursor(ss)) pcp_ss(fd, S_show_sonogram_cursor, b2s(cp->show_sonogram_cursor), chan);
      if (cp->wavo_hop != wavo_hop(ss)) pcp_sd(fd, S_wavo_hop, cp->wavo_hop, chan);
      if (cp->wavo_trace != wavo_trace(ss)) pcp_sd(fd, S_wavo_trace, cp->wavo_trace, chan);
      if (cp->max_transform_peaks != max_transform_peaks(ss)) pcp_sd(fd, S_max_transform_peaks, cp->max_transform_peaks, chan);
      if (cp->show_transform_peaks != show_transform_peaks(ss)) pcp_ss(fd, S_show_transform_peaks, b2s(cp->show_transform_peaks), chan);
      if (cp->fft_log_frequency != fft_log_frequency(ss)) pcp_ss(fd, S_fft_log_frequency, b2s(cp->fft_log_frequency), chan);
      if (cp->fft_log_magnitude != fft_log_magnitude(ss)) pcp_ss(fd, S_fft_log_magnitude, b2s(cp->fft_log_magnitude), chan);
      if (cp->verbose_cursor != verbose_cursor(ss)) pcp_ss(fd, S_with_verbose_cursor, b2s(cp->verbose_cursor), chan);
      if (cp->zero_pad != zero_pad(ss)) pcp_sd(fd, S_zero_pad, cp->zero_pad, chan);
      if (cp->wavelet_type != wavelet_type(ss)) pcp_sd(fd, S_wavelet_type, cp->wavelet_type, chan);
      if (fneq(cp->min_dB, min_dB(ss))) pcp_sf(fd, S_min_dB, cp->min_dB, chan);
      if (fneq(cp->spectro_x_angle, spectro_x_angle(ss))) pcp_sf(fd, S_spectro_x_angle, cp->spectro_x_angle, chan);
      if (fneq(cp->spectro_y_angle, spectro_y_angle(ss))) pcp_sf(fd, S_spectro_y_angle, cp->spectro_y_angle, chan);
      if (fneq(cp->spectro_z_angle, spectro_z_angle(ss))) pcp_sf(fd, S_spectro_z_angle, cp->spectro_z_angle, chan);
      if (fneq(cp->spectro_x_scale, spectro_x_scale(ss))) pcp_sf(fd, S_spectro_x_scale, cp->spectro_x_scale, chan);
      if (fneq(cp->spectro_y_scale, spectro_y_scale(ss))) pcp_sf(fd, S_spectro_y_scale, cp->spectro_y_scale, chan);
      if (fneq(cp->spectro_z_scale, spectro_z_scale(ss))) pcp_sf(fd, S_spectro_z_scale, cp->spectro_z_scale, chan);
      if (fneq(cp->spectro_cutoff, spectro_cutoff(ss))) pcp_sf(fd, S_spectro_cutoff, cp->spectro_cutoff, chan);
      if (fneq(cp->spectro_start, spectro_start(ss))) pcp_sf(fd, S_spectro_start, cp->spectro_start, chan);
      if (fneq(cp->fft_window_alpha, fft_window_alpha(ss))) pcp_sf(fd, S_fft_window_alpha, cp->fft_window_alpha, chan);
      if (fneq(cp->fft_window_beta, fft_window_beta(ss))) pcp_sf(fd, S_fft_window_beta, cp->fft_window_beta, chan);
      if (cp->spectro_hop != spectro_hop(ss)) pcp_sd(fd, S_spectro_hop, cp->spectro_hop, chan);
      if (cp->transform_size != transform_size(ss)) pcp_sd(fd, S_transform_size, cp->transform_size, chan);
      if (cp->transform_graph_type != transform_graph_type(ss)) 
	pcp_ss(fd, S_transform_graph_type, transform_graph_type_name(cp->transform_graph_type), chan);
      if (cp->time_graph_type != time_graph_type(ss)) pcp_ss(fd, S_time_graph_type, time_graph_type_name(cp->time_graph_type), chan);
      if (cp->fft_window != fft_window(ss)) pcp_ss(fd, S_fft_window, TO_VAR_NAME(mus_fft_window_name(cp->fft_window)), chan);
      if (cp->transform_type != transform_type(ss)) pcp_ss(fd, S_transform_type, TO_VAR_NAME(transform_program_name(cp->transform_type)), chan);
      /* this is assuming the added transform definition (if any) can be found -- maybe not a good idea */
      if (cp->transform_normalization != transform_normalization(ss)) 
	pcp_ss(fd, S_transform_normalization, transform_normalization_name(cp->transform_normalization), chan);
      if (cp->time_graph_style != graph_style(ss)) pcp_ss(fd, S_time_graph_style, graph_style_name(cp->time_graph_style), chan);
      if (cp->lisp_graph_style != graph_style(ss)) pcp_ss(fd, S_lisp_graph_style, graph_style_name(cp->lisp_graph_style), chan);
      if (cp->transform_graph_style != graph_style(ss)) pcp_ss(fd, S_transform_graph_style, graph_style_name(cp->transform_graph_style), chan);
      if (cp->show_mix_waveforms != show_mix_waveforms(ss)) pcp_ss(fd, S_show_mix_waveforms, b2s(cp->show_mix_waveforms), chan);
      if (cp->dot_size != dot_size(ss)) pcp_sd(fd, S_dot_size, cp->dot_size, chan);
      if (fneq(cp->grid_density, grid_density(ss))) pcp_sf(fd, S_grid_density, cp->grid_density, chan);
      if (cp->x_axis_style != x_axis_style(ss)) pcp_ss(fd, S_x_axis_style, x_axis_style_name(cp->x_axis_style), chan);
      if (fneq(cp->beats_per_minute, beats_per_minute(ss))) pcp_sf(fd, S_beats_per_minute, cp->beats_per_minute, chan);
      if (cp->beats_per_measure != beats_per_measure(ss)) pcp_sd(fd, S_beats_per_measure, cp->beats_per_measure, chan);
      if (cp->show_axes != show_axes(ss)) pcp_ss(fd, S_show_axes, show_axes2string(cp->show_axes), chan);
      if (cp->graphs_horizontal != graphs_horizontal(ss)) pcp_ss(fd, S_graphs_horizontal, b2s(cp->graphs_horizontal), chan);
      if ((XEN_VECTOR_P(cp->properties)) &&
	  (XEN_LIST_P(XEN_VECTOR_REF(cp->properties, 0))) &&
	  (!(XEN_NULL_P(XEN_VECTOR_REF(cp->properties, 0)))))
	{
	  save_property_list(fd, XEN_VECTOR_REF(cp->properties, 0), chan);
	}

      /* ap->default_xlabel if not null, user explicitly set it */
      /* ylabel can only be a user choice -- never set by Snd */
      if (cp->axis)
	{
	  if (cp->axis->default_xlabel) pcp_sss(fd, S_x_axis_label, cp->axis->default_xlabel, chan, "time-graph");
	  if (cp->axis->ylabel)         pcp_sss(fd, S_y_axis_label, cp->axis->ylabel,         chan, "time-graph");
	}
      if ((cp->fft) &&
	  (cp->fft->axis))
	{
	  if (cp->fft->axis->default_xlabel) pcp_sss(fd, S_x_axis_label, cp->fft->axis->default_xlabel, chan, "transform-graph");
	  if (cp->fft->axis->ylabel)         pcp_sss(fd, S_y_axis_label, cp->fft->axis->ylabel,         chan, "transform-graph");
	}
      /* lisp_info is hidden in snd-chn.c */

      edit_history_to_file(fd, cp, true);
      check_selection(fd, cp);
      if (selected_channel() == cp)
	{
#if HAVE_SCHEME
	  fprintf(fd, "%s(set! _saved_snd_selected_sound_ sfile)\n", white_space);
	  fprintf(fd, "%s(set! _saved_snd_selected_channel_ %d)\n", white_space, cp->chan);
#endif
#if HAVE_RUBY
	  fprintf(fd, "%ssaved_snd_selected_sound = sfile\n", white_space);
	  fprintf(fd, "%ssaved_snd_selected_channel = %d\n", white_space, cp->chan);
#endif
#if HAVE_FORTH
	  fprintf(fd, "%ssfile to saved_snd_selected_sound\n", white_space);
	  fprintf(fd, "%s%d to saved_snd_selected_channel\n", white_space, cp->chan);
#endif
	}
    }
  close_save_sound_block(fd, !b_ok);
}

static XEN after_save_state_hook;
static XEN before_save_state_hook;

void save_state(const char *save_state_name)
{
  FILE *save_fd;
  char *locale = NULL, *fullname;
  bool append_new_state = false;
  if (!save_state_name)
    {
      snd_error("no save state file name?");
      return;
    }
  fullname = mus_expand_filename(save_state_name);
  if (XEN_HOOKED(before_save_state_hook))
    {
      XEN res = XEN_FALSE;
      res = run_or_hook(before_save_state_hook, 
			XEN_LIST_1(C_TO_XEN_STRING(fullname)),
			S_before_save_state_hook);
      append_new_state = XEN_TO_C_BOOLEAN(res);
    }
  if (append_new_state)
    save_fd = FOPEN(fullname, "a");
  else save_fd = FOPEN(fullname, "w");
  if (fullname) {FREE(fullname); fullname = NULL;}
  if (save_fd == NULL)
    {
      snd_error(_("can't write %s: %s"), save_state_name, snd_io_strerror());
      return;
    }

#if HAVE_GUILE
  /* try to make sure all previously loaded files are now loaded */
  save_loaded_files_list(save_fd, save_state_name);
#endif
#if HAVE_SETLOCALE
  locale = copy_string(setlocale(LC_NUMERIC, "C")); /* must use decimal point in floats since Scheme assumes that format */
#endif
  save_options(save_fd);                            /* options = user-settable global state variables */
  /* the global settings need to precede possible local settings */

  if (ss->active_sounds > 0)
    {
      if (ss->selected_sound != NO_SELECTION)
	{
#if HAVE_SCHEME
	  fprintf(save_fd, "\n(define _saved_snd_selected_sound_ #f)\n");
	  fprintf(save_fd, "(define _saved_snd_selected_channel_ #f)\n");
#endif
#if HAVE_RUBY
	  fprintf(save_fd, "\nsaved_snd_selected_sound = -1\n");
	  fprintf(save_fd, "saved_snd_selected_channel = -1\n");
#endif
#if HAVE_FORTH
	  fprintf(save_fd, "\n#f value saved_snd_selected_sound\n");
	  fprintf(save_fd, "#f value saved_snd_selected_channel\n");
	  fprintf(save_fd, "#f value sfile\n");
#endif
	}
      for_each_sound_with_void(save_sound_state, (void *)save_fd);      /* current sound state -- will traverse chans */
      if (ss->selected_sound != NO_SELECTION)
	{
#if HAVE_SCHEME
	  fprintf(save_fd, "(if _saved_snd_selected_sound_\n");
	  fprintf(save_fd, "  (begin\n");
	  fprintf(save_fd, "    (%s _saved_snd_selected_sound_)\n", S_select_sound);
	  fprintf(save_fd, "    (%s _saved_snd_selected_channel_)))\n", S_select_channel);
#endif
#if HAVE_RUBY
	  fprintf(save_fd, "if saved_snd_selected_sound != -1\n");
	  fprintf(save_fd, "  select_sound(saved_snd_selected_sound)\n");
	  fprintf(save_fd, "  select_channel(saved_snd_selected_channel)\n");
	  fprintf(save_fd, "end\n");
#endif
#if HAVE_FORTH
	  fprintf(save_fd, "saved_snd_selected_sound false? not [if]\n");
	  fprintf(save_fd, "  saved_snd_selected_sound   %s drop\n", S_select_sound);
	  fprintf(save_fd, "  saved_snd_selected_channel %s drop\n", S_select_channel);
	  fprintf(save_fd, "[then]\n\n");
#endif
	}
    }
  fprintf(save_fd, "\n");
  save_macro_state(save_fd);                              /* current unsaved keyboard macros (snd-chn.c) */
  save_envelope_editor_state(save_fd);                    /* current envelope editor window state */
  save_regions(save_fd);                                  /* regions */
  
  if (transform_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_transform_dialog));
  if (enved_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_enved_dialog));
  if (color_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_color_dialog));
  if (orientation_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_orientation_dialog));
  if (region_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_view_regions_dialog));
  if (record_dialog_is_active()) fprintf(save_fd, BPAREN "%s" EPAREN "\n", TO_PROC_NAME(S_recorder_dialog));
  save_post_it_dialog_state(save_fd);
  save_find_dialog_state(save_fd);
  save_edit_header_dialog_state(save_fd);
  save_print_dialog_state(save_fd);
  save_file_dialog_state(save_fd);
  save_view_files_dialogs(save_fd);
  
  /* saving mix/track state is problematic.  For example, if we make a selection, mix it,
   *   then make another selection and mix it, we get an edit list:
   *   
   *   (change-samples-with-origin 266 451 "set! -mix-0 (mix-selection 266)" "/home/bil/zap/snd/snd_3309_9.snd" sfile 0 #f (list 1145009982 1848))
   *   (change-samples-with-origin 1655 480 "set! -mix-1 (mix-selection 1655)" "/home/bil/zap/snd/snd_3309_10.snd" sfile 0 #f (list 1145009982 1964))
   *   (set! (selection-member? sfile 0) #t)
   *   (set! (selection-position sfile 0) 816)
   *   (set! (selection-frames sfile 0) 480)
   *
   *  which won't even work for the current selection case!  If we mix some piece of a sound
   *    being edited in another window, we'd need to keep all edits in sync during the restore!
   *  But each mix has a temp file of its data, so perhaps an internal function to fake a mix
   *    using it?
   *
   *  :(edit-list->function)
   *    #<procedure #f ((snd chn) 
   *       (let ((-mix-0 0) (-mix-1 1)) 
   *         (set! -mix-0 (mix-selection 266)) 
   *         ;; selection can change here!
   *         (set! -mix-1 (mix-selection 1655))))>
   *
   *  which is also wrong!  This has to use the shadowing temp files.
   *    so edit-list->function fails if selections involved (mix-selection) [or is this what is expected?]
   *
   *  To keep the mix/track dialogs in sync with the newly restored mixes would require keeping the
   *     old mix/track numbers and mapping to the new ones.
   */
  
  pss_ss(save_fd, S_show_listener, b2s(listener_is_visible()));
  
  /* the problem here (with saving hooks) is that it is not straightforward to save the function source
   *   (with the current print-set! source option, or with an earlier procedure->string function using
   *   procedure_environment etc); many types print in this case in ways that are not readable.
   *   The functions may depend on globals that are not in loaded files, or that were changed since
   *   loading, and trying to map over the current module's obarray, saving each such variable in
   *   its current form, is a major undertaking (although this can be done for simple vars); additionally, 
   *   what if the user has changed these before restoring -- should the old forms be restored?
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


char *save_options_in_prefs(void)
{
#if HAVE_EXTENSION_LANGUAGE
  FILE *fd;
  char *fullname;
#if HAVE_GUILE
  #define SND_PREFS "~/.snd_prefs_guile"
#endif
#if HAVE_GAUCHE
  #define SND_PREFS "~/.snd_prefs_gauche"
#endif
#if HAVE_RUBY
  #define SND_PREFS "~/.snd_prefs_ruby"
#endif
#if HAVE_FORTH
  #define SND_PREFS "~/.snd_prefs_forth"
#endif
  fullname = mus_expand_filename(SND_PREFS);
  fd = FOPEN(fullname, "w");
  if (!fd)
    {
      snd_error(_("can't write %s: %s"), SND_PREFS, snd_io_strerror());
      return(NULL);
    }
  save_options(fd);
  snd_fclose(fd, SND_PREFS);
  FREE(fullname);
  return(SND_PREFS);
#else
  return(NULL);
#endif
}

#if 0
static char *file_extension(char *arg)
{
  char *dot = NULL, *sp;
  if (arg) 
    for (sp = arg; (*sp) != '\0'; sp++) 
      if ((*sp) == '.') 
	dot = (++sp);
  return(dot);
}
#endif

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

static void printout_to_stdout(const char *msg, void *ignore)
{
  fprintf(stdout, "%s\n", msg);
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
	  (strcmp("-noinit", argname) == 0) ||
	  (strcmp("-nogtkrc", argname) == 0))
	return(auto_open_ctr + 1);
      else
	{
	  if (strcmp("-init", argname) == 0)
	    return(auto_open_ctr + 2);
	  else
	    {
	      if ((strcmp("-p", argname) == 0) ||
		  (strcmp("-preload", argname) == 0))
		{
		  /* preload sound files in dir (can be ., should be unquoted) */
		  auto_open_ctr++;
		  if ((auto_open_ctr >= args) ||
		      (auto_open_file_names[auto_open_ctr] == NULL))
		    snd_error(_("%s but no directory to add?"), argname);
		  else view_files_add_directory(NULL_WIDGET, auto_open_file_names[auto_open_ctr]);
		}
	      else
		{
		  if ((strcmp("-l", argname) == 0) ||
		      (strcmp("-load", argname) == 0) ||
		      (strcmp("-b", argname) == 0) ||
		      (strcmp("-batch", argname) == 0) ||
		      (source_file_p(argname)))
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
			  redirect_everything_to(printout_to_stdout, NULL);
			  snd_load_file(auto_open_file_names[auto_open_ctr]);
			  redirect_everything_to(NULL, NULL);
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
			  else 
			    {
			      char *buf;
			      buf = auto_open_file_names[auto_open_ctr];
			      redirect_everything_to(printout_to_stdout, NULL);
			      snd_report_result(snd_catch_any(eval_str_wrapper, (void *)buf, buf), buf);
			      redirect_everything_to(NULL, NULL);
			    }
			}
		      else
			{
			  if ((with_title) && 
			      (strcmp("-title", argname) == 0))
			    {
			      auto_open_ctr++;
			      if ((auto_open_ctr >= args) ||
				  (auto_open_file_names[auto_open_ctr] == NULL))
				snd_error_without_format(_("-title but no title?")); /* for gtk -- Xt handles the Motif case */
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
				    snd_error_without_format(_("-I but no path?"));
				  else 
				    {
				      XEN_ADD_TO_LOAD_PATH(auto_open_file_names[auto_open_ctr]);
				    }
				}
			      else
				{
				  if (startup_filename == NULL)
				    {
				      startup_filename = copy_string(argname);
				      if (dont_start(startup_filename)) snd_exit(1);
				    }
				  ss->open_requestor = FROM_STARTUP;
				  snd_open_file(argname, FILE_READ_WRITE);
				}
			    }
			}
		    }
		}
	    }
	}
    }
  return(auto_open_ctr + 1);
}

static void save_state_error_handler(const char *msg, void *data)
{
  char *filename = (char *)data;
  XEN fname;
  if (filename)
    {
      fname = C_TO_XEN_STRING(filename);
      FREE(filename); /* this is "name" below */
    }
  else fname = C_TO_XEN_STRING(save_state_file(ss));
  redirect_snd_error_to(NULL, NULL);
  XEN_ERROR(CANNOT_SAVE,
	    XEN_LIST_2(C_TO_XEN_STRING(S_save_state),
		       fname));
}

static XEN g_save_state(XEN filename) 
{
  char *name = NULL;
  XEN res;
  #define H_save_state "(" S_save_state " :optional filename): save the current Snd state in filename; (load filename) restores it.  The \
default " S_save_state " filename is " DEFAULT_SAVE_STATE_FILE ". It can be changed via " S_save_state_file "."

  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(filename), filename, XEN_ONLY_ARG, S_save_state, "a string");

  if (XEN_BOUND_P(filename))
    name = copy_string(XEN_TO_C_STRING(filename));
  else name = copy_string(save_state_file(ss));

  redirect_snd_error_to(save_state_error_handler, (void *)name);
  save_state(name);
  redirect_snd_error_to(NULL, NULL);

  res = C_TO_XEN_STRING(name);
  FREE(name);
  return(res);
}

static XEN g_exit(XEN val) 
{
  #define H_exit "(" S_exit "): exit Snd"
  if (snd_exit_cleanly(EXIT_NOT_FORCED))
    snd_exit(XEN_TO_C_INT_OR_ELSE(val, 1)); 
  return(XEN_FALSE);
}

static int snd_access(char *dir, const char *caller)
{
  int err;
  char *temp;
  temp = shorter_tempnam(dir, "snd_");
  err = mus_file_create(temp);
  if (err == -1)
    {
      XEN res;
      FREE(temp);
      temp = mus_format(_("%s: directory %s is not writable: %s"), caller, dir, snd_open_strerror());
      res = C_TO_XEN_STRING(temp);
      FREE(temp);
      XEN_ERROR(NO_SUCH_FILE,
		XEN_LIST_1(res));
    }
  else snd_close(err, temp);
  snd_remove(temp, IGNORE_CACHE);
  FREE(temp);
  return(1);
}

static XEN g_temp_dir(void) {return(C_TO_XEN_STRING(temp_dir(ss)));}
static XEN g_set_temp_dir(XEN val) 
{
  #define H_temp_dir "(" S_temp_dir "): name of directory for temp files (or " PROC_FALSE "=null)"
  char *dir = MUS_DEFAULT_TEMP_DIR;
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_temp_dir, "a string or " PROC_FALSE "=default (null)"); 
  if (XEN_STRING_P(val)) dir = XEN_TO_C_STRING(val);
  if (snd_access(dir, S_temp_dir))
    {
      if (temp_dir(ss)) FREE(temp_dir(ss));
      set_temp_dir(copy_string(dir));
    }
  return(C_TO_XEN_STRING(temp_dir(ss)));
}

static XEN g_ladspa_dir(void) {return(C_TO_XEN_STRING(ladspa_dir(ss)));}
static XEN g_set_ladspa_dir(XEN val) 
{
  #define H_ladspa_dir "(" S_ladspa_dir "): name of directory for ladspa plugin libraries"
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_ladspa_dir, "a string or " PROC_FALSE "=default (null)"); 
  if (ladspa_dir(ss)) FREE(ladspa_dir(ss));
  if (XEN_FALSE_P(val))
    set_ladspa_dir(copy_string(DEFAULT_LADSPA_DIR));
  else set_ladspa_dir(copy_string(XEN_TO_C_STRING(val)));
  return(C_TO_XEN_STRING(ladspa_dir(ss)));
}

static XEN g_save_state_file(void) {return(C_TO_XEN_STRING(save_state_file(ss)));}
static XEN g_set_save_state_file(XEN val) 
{
  char *filename;
  #define H_save_state_file "(" S_save_state_file "): the name of the saved state file (\"saved-snd." XEN_FILE_EXTENSION "\")"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_save_state_file, "a string"); 
  filename = XEN_TO_C_STRING(val);
  if (save_state_file(ss)) FREE(save_state_file(ss));
  in_set_save_state_file(copy_string(filename));
  return(C_TO_XEN_STRING(save_state_file(ss)));
}

static XEN g_save_dir(void) {return(C_TO_XEN_STRING(save_dir(ss)));}
static XEN g_set_save_dir(XEN val) 
{
  #define H_save_dir "(" S_save_dir "): name of directory for saved state data (or " PROC_FALSE "=null)"
  char *dir = MUS_DEFAULT_SAVE_DIR;
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_save_dir, "a string or " PROC_FALSE "=default (null)"); 
  if (XEN_STRING_P(val)) dir = XEN_TO_C_STRING(val);
  if (snd_access(dir, S_save_dir))
    {
      if (save_dir(ss)) FREE(save_dir(ss));
      set_save_dir(copy_string(dir));
    }
  return(C_TO_XEN_STRING(save_dir(ss)));
}


static int snd_screen_height(void)
{
#if USE_MOTIF
  return(HeightOfScreen(ScreenOfDisplay(MAIN_DISPLAY(ss), 0)));
#else
#if USE_GTK
  return(gdk_screen_height());
#else
  return(4000);
#endif
#endif
}

static int snd_screen_width(void)
{
#if USE_MOTIF
  return(WidthOfScreen(ScreenOfDisplay(MAIN_DISPLAY(ss), 0)));
#else
#if USE_GTK
  return(gdk_screen_width());
#else
  return(4000);
#endif
#endif
}

static XEN g_window_height(void) 
{
  #define H_window_height "(" S_window_height "): current Snd window height in pixels"
  return(C_TO_XEN_INT(widget_height(MAIN_SHELL(ss))));
}

static XEN g_set_window_height(XEN height) 
{
  Latus val;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(height), height, XEN_ONLY_ARG, S_setB S_window_height, "a number"); 
  val = (Latus)XEN_TO_C_INT_OR_ELSE(height, 0);
  if ((val > 0) && (val < snd_screen_height()))
    {
#if (!USE_NO_GUI)
      set_widget_height(MAIN_SHELL(ss), val);
#endif
      ss->init_window_height = val;
    }
  return(height);
}

static XEN g_window_width(void) 
{
  #define H_window_width "(" S_window_width "): current Snd window width in pixels"
  return(C_TO_XEN_INT(widget_width(MAIN_SHELL(ss))));
}

static XEN g_set_window_width(XEN width) 
{
  Latus val;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(width), width, XEN_ONLY_ARG, S_setB S_window_width, "a number"); 
  val = (Latus)XEN_TO_C_INT_OR_ELSE(width, 0);
  if ((val > 0) && (val < snd_screen_width()))
    {
#if (!USE_NO_GUI)
      set_widget_width(MAIN_SHELL(ss), val);
#endif
      ss->init_window_width = val;
    }
  return(width);
}

static XEN g_window_x(void) 
{
  #define H_window_x "(" S_window_x "): current Snd window x position in pixels"
  return(C_TO_XEN_INT(widget_x(MAIN_SHELL(ss))));
}

static XEN g_set_window_x(XEN val) 
{
  Locus x;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_window_x, "a number"); 
  x = (Locus)XEN_TO_C_INT_OR_ELSE(val, 0);
  if ((x >= 0) && (x < snd_screen_width()))
    {
      set_widget_x(MAIN_SHELL(ss), x);
      ss->init_window_x = x;
    }
  return(val);
}

static XEN g_window_y(void) 
{
  #define H_window_y "(" S_window_y "): current Snd window y position in pixels"
  return(C_TO_XEN_INT(widget_y(MAIN_SHELL(ss))));
}

static XEN g_set_window_y(XEN val) 
{
  Locus y;
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_window_y, "a number"); 
  y = (Locus)XEN_TO_C_INT_OR_ELSE(val, 0);
  if ((y >= 0) && (y < snd_screen_height()))
    {
      set_widget_y(MAIN_SHELL(ss), y);
      ss->init_window_y = y;
    }
  return(val);
}

static XEN g_just_sounds(void)
{
  #define H_just_sounds "(" S_just_sounds "): the 'just sounds' choice in the file chooser dialog"
  return(C_TO_XEN_BOOLEAN(just_sounds(ss)));
}

static XEN g_set_just_sounds(XEN on) 
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ARG_1, S_setB S_just_sounds, "a boolean");
  set_just_sounds(XEN_TO_C_BOOLEAN(on));
  reflect_just_sounds();
  return(C_TO_XEN_BOOLEAN(just_sounds(ss)));
}

static XEN g_tiny_font(void) {return(C_TO_XEN_STRING(tiny_font(ss)));}
static XEN g_set_tiny_font(XEN val) 
{
  #define H_tiny_font "(" S_tiny_font "): font use for some info in the graphs"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_tiny_font, "a string"); 
  set_tiny_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(tiny_font(ss)));
}

static XEN g_axis_label_font(void) {return(C_TO_XEN_STRING(axis_label_font(ss)));}
static XEN g_set_axis_label_font(XEN val) 
{
  #define H_axis_label_font "(" S_axis_label_font "): font used for axis labels"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_axis_label_font, "a string"); 
  set_axis_label_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(axis_label_font(ss)));
}

static XEN g_axis_numbers_font(void) {return(C_TO_XEN_STRING(axis_numbers_font(ss)));}
static XEN g_set_axis_numbers_font(XEN val) 
{
  #define H_axis_numbers_font "(" S_axis_numbers_font "): font used for axis numbers"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_axis_numbers_font, "a string"); 
  set_axis_numbers_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(axis_numbers_font(ss)));
}

static XEN g_listener_font(void) {return(C_TO_XEN_STRING(listener_font(ss)));}
static XEN g_set_listener_font(XEN val) 
{
  #define H_listener_font "(" S_listener_font "): font used by the lisp listener"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_listener_font, "a string");
  set_listener_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(listener_font(ss)));
}

static XEN g_bold_peaks_font(void) {return(C_TO_XEN_STRING(bold_peaks_font(ss)));}
static XEN g_set_bold_peaks_font(XEN val) 
{
  #define H_bold_peaks_font "(" S_bold_peaks_font "): bold font used by fft peak display"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_bold_peaks_font, "a string"); 
  set_bold_peaks_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(bold_peaks_font(ss)));
}

static XEN g_peaks_font(void) {return(C_TO_XEN_STRING(peaks_font(ss)));}
static XEN g_set_peaks_font(XEN val) 
{
  #define H_peaks_font "(" S_peaks_font "): normal font used by fft peak display"
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_peaks_font, "a string"); 
  set_peaks_font(XEN_TO_C_STRING(val)); 
  return(C_TO_XEN_STRING(peaks_font(ss)));
}


static XEN g_audio_output_device(void) {return(C_TO_XEN_INT(audio_output_device(ss)));}
static XEN g_set_audio_output_device(XEN val) 
{
  #define H_audio_output_device "(" S_audio_output_device "): the current sndlib default output device (" S_mus_audio_default ")"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_audio_output_device, "an integer"); 
  set_audio_output_device(XEN_TO_C_INT(val)); 
  return(C_TO_XEN_INT(audio_output_device(ss)));
}

static XEN g_audio_input_device(void) {return(C_TO_XEN_INT(audio_input_device(ss)));}
static XEN g_set_audio_input_device(XEN val) 
{
  #define H_audio_input_device "(" S_audio_input_device "): the current sndlib default input device (" S_mus_audio_default ")"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_audio_input_device, "an integer"); 
  set_audio_input_device(XEN_TO_C_INT(val)); 
  return(C_TO_XEN_INT(audio_input_device(ss)));
}

static XEN g_minibuffer_history_length(void) {return(C_TO_XEN_INT(minibuffer_history_length(ss)));}
static XEN g_set_minibuffer_history_length(XEN val) 
{
  #define H_minibuffer_history_length "(" S_minibuffer_history_length "): the minibuffer history length. \
This pertains to the M-p and M-n commands."
  int len;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_minibuffer_history_length, "an integer");
  len = XEN_TO_C_INT(val);
  if (len > 0)
    set_minibuffer_history_length(len);
  return(C_TO_XEN_INT(minibuffer_history_length(ss)));
}

static XEN g_auto_resize(void) {return(C_TO_XEN_BOOLEAN(auto_resize(ss)));}
static XEN g_set_auto_resize(XEN val) 
{
  #define H_auto_resize "(" S_auto_resize "): " PROC_TRUE " if Snd can change its main window size as it pleases (default: " PROC_TRUE ")"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_auto_resize, "a boolean");
  set_auto_resize(XEN_TO_C_BOOLEAN(val)); 
#if USE_MOTIF
  XtVaSetValues(MAIN_SHELL(ss), XmNallowShellResize, auto_resize(ss), NULL);
#endif
  return(C_TO_XEN_BOOLEAN(auto_resize(ss)));
}

static XEN g_color_cutoff(void) {return(C_TO_XEN_DOUBLE(color_cutoff(ss)));}
static XEN g_set_color_cutoff(XEN val) 
{
  #define H_color_cutoff "(" S_color_cutoff "): color map cutoff point (default .003).  Any values \
below the cutoff are displayed in the background color"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_color_cutoff, "a number");
  set_color_cutoff(mus_fclamp(0.0,
			      XEN_TO_C_DOUBLE(val),
			      0.25)); 
  return(C_TO_XEN_DOUBLE(color_cutoff(ss)));
}

static XEN g_color_inverted(void) {return(C_TO_XEN_BOOLEAN(color_inverted(ss)));}
static XEN g_set_color_inverted(XEN val) 
{
  #define H_color_inverted "(" S_color_inverted "): whether the colormap in operation should be inverted"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_color_inverted, "a boolean");
  set_color_inverted(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(color_inverted(ss)));
}

static XEN g_color_scale(void) {return(C_TO_XEN_DOUBLE(color_scale(ss)));}
static XEN g_set_color_scale(XEN val) 
{
  #define H_color_scale "(" S_color_scale "): darkness setting for colormaps (0.5)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_color_scale, "a number"); 
  set_color_scale(mus_fclamp(0.0,
			     XEN_TO_C_DOUBLE(val),
			     1000.0)); 
  return(C_TO_XEN_DOUBLE(color_scale(ss)));
}

static XEN g_selection_creates_region(void) {return(C_TO_XEN_BOOLEAN(selection_creates_region(ss)));}
static XEN g_set_selection_creates_region(XEN val) 
{
  #define H_selection_creates_region "(" S_selection_creates_region "): " PROC_TRUE " if a region should be created each time a selection is made. \
The default is currently " PROC_TRUE ", but that may change.  If you're dealing with large selections, and have no need of \
regions (saved selections), you can speed up many operations by setting this flag to " PROC_FALSE
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_selection_creates_region, "a boolean");
  set_selection_creates_region(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(selection_creates_region(ss)));
}

static XEN g_print_length(void) {return(C_TO_XEN_INT(print_length(ss)));}
static XEN g_set_print_length(XEN val) 
{
  int len;
  #define H_print_length "(" S_print_length "): number of vector elements to print in the listener (default: 12)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_print_length, "an integer"); 
  len = XEN_TO_C_INT(val);
  if (len < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_print_length, XEN_ONLY_ARG, val, "must be >= 0");
  set_print_length(len);
  mus_vct_set_print_length(len);
  return(C_TO_XEN_INT(print_length(ss)));
}

static XEN g_show_indices(void) {return(C_TO_XEN_BOOLEAN(show_indices(ss)));}
static XEN g_set_show_indices(XEN val) 
{
  #define H_show_indices "(" S_show_indices "): " PROC_TRUE " if sound name should be preceded by its index in the sound display."
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_indices, "a boolean");
  set_show_indices(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(show_indices(ss)));
}

static XEN g_show_backtrace(void) {return(C_TO_XEN_BOOLEAN(show_backtrace(ss)));}
static XEN g_set_show_backtrace(XEN val) 
{
  #define H_show_backtrace "(" S_show_backtrace "): " PROC_TRUE " to show backtrace automatically upon error"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_show_backtrace, "a boolean");
  set_show_backtrace(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(show_backtrace(ss)));
}

static XEN g_trap_segfault(void) {return(C_TO_XEN_BOOLEAN(trap_segfault(ss)));}
static XEN g_set_trap_segfault(XEN val) 
{
  #define H_trap_segfault "(" S_trap_segfault "): " PROC_TRUE " if Snd should try to trap (and whine about) segfaults"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_trap_segfault, "a boolean");
  set_trap_segfault(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(trap_segfault(ss)));
}

static XEN g_with_relative_panes(void) {return(C_TO_XEN_BOOLEAN(with_relative_panes(ss)));}
static XEN g_set_with_relative_panes(XEN val) 
{
  #define H_with_relative_panes "(" S_with_relative_panes "): " PROC_TRUE " if multichannel sounds should try to maintain relative pane sizes"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_relative_panes, "a boolean");
  set_with_relative_panes(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_relative_panes(ss)));
}

static XEN g_with_background_processes(void) {return(C_TO_XEN_BOOLEAN(with_background_processes(ss)));}
static XEN g_set_with_background_processes(XEN val) 
{
  #define H_with_background_processes "(" S_with_background_processes "): " PROC_TRUE " if Snd should use background (idle time) processing"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_background_processes, "a boolean");
  set_with_background_processes(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_background_processes(ss)));
}

static XEN g_with_file_monitor(void) {return(C_TO_XEN_BOOLEAN(with_file_monitor(ss)));}
static XEN g_set_with_file_monitor(XEN val) 
{
  #define H_with_file_monitor "(" S_with_file_monitor "): " PROC_TRUE " if the file alteration monitor is active"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_with_file_monitor, "a boolean");
  set_with_file_monitor(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(with_file_monitor(ss)));
}

static XEN g_snd_version(void) 
{
  #define H_snd_version "(" S_snd_version "): current Snd version (a string)"
  return(C_TO_XEN_STRING(SND_DATE));
}

static XEN g_color_dialog(XEN managed) 
{
  widget_t w;
  #define H_color_dialog "(" S_color_dialog "): start the Color dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_color_dialog, "a boolean");
  w = start_color_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_orientation_dialog(XEN managed) 
{
  widget_t w;
  #define H_orientation_dialog "(" S_orientation_dialog " :optional (managed " PROC_TRUE ")): start the Orientation dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_orientation_dialog, "a boolean");
  w = start_orientation_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_transform_dialog(XEN managed) 
{
  widget_t w;
  #define H_transform_dialog "(" S_transform_dialog " :optional (managed " PROC_TRUE ")): start the Transforms dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ONLY_ARG, S_transform_dialog, "a boolean");
  w = fire_up_transform_dialog(XEN_TO_C_BOOLEAN(managed));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_print_dialog(XEN managed, XEN direct_to_printer) 
{
  widget_t w;
  #define H_print_dialog "(" S_print_dialog " :optional managed direct): start the File Print dialog"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(managed), managed, XEN_ARG_1, S_print_dialog, "a boolean");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(direct_to_printer), direct_to_printer, XEN_ARG_2, S_print_dialog, "a boolean");
  w = make_file_print_dialog(!(XEN_FALSE_P(managed)), XEN_TRUE_P(direct_to_printer));
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_preferences_dialog(void)
{
  widget_t w;
  #define H_preferences_dialog "(" S_preferences_dialog "): start the Options:Preferences dialog"
  w = start_preferences_dialog();
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_abort(void)
{
  #define H_abort "(" S_abort "): exit Snd via \"abort\", presumably to land in the debugger"
  abort();
  return(XEN_FALSE);
}

static XEN g_abortq(void)
{
  #define H_abortQ "(" S_c_g "): allow pending user interface events to occur, returning " PROC_TRUE " if C-g was typed"
  check_for_event();
  if (ss->stopped_explicitly)
    {
      ss->stopped_explicitly = false;
      return(XEN_TRUE);
    }
  return(XEN_FALSE);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_save_state_file_w, g_save_state_file)
XEN_NARGIFY_1(g_set_save_state_file_w, g_set_save_state_file)
XEN_NARGIFY_0(g_save_dir_w, g_save_dir)
XEN_NARGIFY_1(g_set_save_dir_w, g_set_save_dir)
XEN_NARGIFY_0(g_temp_dir_w, g_temp_dir)
XEN_NARGIFY_1(g_set_temp_dir_w, g_set_temp_dir)
XEN_NARGIFY_0(g_ladspa_dir_w, g_ladspa_dir)
XEN_NARGIFY_1(g_set_ladspa_dir_w, g_set_ladspa_dir)
XEN_ARGIFY_1(g_save_state_w, g_save_state)
XEN_ARGIFY_1(g_exit_w, g_exit)
XEN_NARGIFY_0(g_script_arg_w, g_script_arg)
XEN_NARGIFY_1(g_set_script_arg_w, g_set_script_arg)
XEN_NARGIFY_0(g_script_args_w, g_script_args)
XEN_NARGIFY_0(g_window_x_w, g_window_x)
XEN_NARGIFY_1(g_set_window_x_w, g_set_window_x)
XEN_NARGIFY_0(g_window_y_w, g_window_y)
XEN_NARGIFY_1(g_set_window_y_w, g_set_window_y)
XEN_NARGIFY_0(g_window_width_w, g_window_width)
XEN_NARGIFY_1(g_set_window_width_w, g_set_window_width)
XEN_NARGIFY_0(g_window_height_w, g_window_height)
XEN_NARGIFY_1(g_set_window_height_w, g_set_window_height)
XEN_NARGIFY_0(g_just_sounds_w, g_just_sounds)
XEN_NARGIFY_1(g_set_just_sounds_w, g_set_just_sounds)
XEN_NARGIFY_0(g_audio_output_device_w, g_audio_output_device)
XEN_NARGIFY_1(g_set_audio_output_device_w, g_set_audio_output_device)
XEN_NARGIFY_0(g_audio_input_device_w, g_audio_input_device)
XEN_NARGIFY_1(g_set_audio_input_device_w, g_set_audio_input_device)
XEN_NARGIFY_0(g_minibuffer_history_length_w, g_minibuffer_history_length)
XEN_NARGIFY_1(g_set_minibuffer_history_length_w, g_set_minibuffer_history_length)
XEN_NARGIFY_0(g_auto_resize_w, g_auto_resize)
XEN_NARGIFY_1(g_set_auto_resize_w, g_set_auto_resize)
XEN_NARGIFY_0(g_color_cutoff_w, g_color_cutoff)
XEN_NARGIFY_1(g_set_color_cutoff_w, g_set_color_cutoff)
XEN_NARGIFY_0(g_color_inverted_w, g_color_inverted)
XEN_NARGIFY_1(g_set_color_inverted_w, g_set_color_inverted)
XEN_NARGIFY_0(g_color_scale_w, g_color_scale)
XEN_NARGIFY_1(g_set_color_scale_w, g_set_color_scale)
XEN_NARGIFY_0(g_selection_creates_region_w, g_selection_creates_region)
XEN_NARGIFY_1(g_set_selection_creates_region_w, g_set_selection_creates_region)
XEN_NARGIFY_0(g_print_length_w, g_print_length)
XEN_NARGIFY_1(g_set_print_length_w, g_set_print_length)
XEN_NARGIFY_0(g_show_indices_w, g_show_indices)
XEN_NARGIFY_1(g_set_show_indices_w, g_set_show_indices)
XEN_NARGIFY_0(g_show_backtrace_w, g_show_backtrace)
XEN_NARGIFY_1(g_set_show_backtrace_w, g_set_show_backtrace)
XEN_NARGIFY_0(g_trap_segfault_w, g_trap_segfault)
XEN_NARGIFY_1(g_set_trap_segfault_w, g_set_trap_segfault)
XEN_NARGIFY_0(g_with_relative_panes_w, g_with_relative_panes)
XEN_NARGIFY_1(g_set_with_relative_panes_w, g_set_with_relative_panes)
XEN_NARGIFY_0(g_with_background_processes_w, g_with_background_processes)
XEN_NARGIFY_1(g_set_with_background_processes_w, g_set_with_background_processes)
XEN_NARGIFY_0(g_with_file_monitor_w, g_with_file_monitor)
XEN_NARGIFY_1(g_set_with_file_monitor_w, g_set_with_file_monitor)
XEN_NARGIFY_0(g_tiny_font_w, g_tiny_font)
XEN_NARGIFY_1(g_set_tiny_font_w, g_set_tiny_font)
XEN_NARGIFY_0(g_peaks_font_w, g_peaks_font)
XEN_NARGIFY_1(g_set_peaks_font_w, g_set_peaks_font)
XEN_NARGIFY_0(g_bold_peaks_font_w, g_bold_peaks_font)
XEN_NARGIFY_1(g_set_bold_peaks_font_w, g_set_bold_peaks_font)
XEN_NARGIFY_0(g_axis_label_font_w, g_axis_label_font)
XEN_NARGIFY_1(g_set_axis_label_font_w, g_set_axis_label_font)
XEN_NARGIFY_0(g_axis_numbers_font_w, g_axis_numbers_font)
XEN_NARGIFY_1(g_set_axis_numbers_font_w, g_set_axis_numbers_font)
XEN_NARGIFY_0(g_listener_font_w, g_listener_font)
XEN_NARGIFY_1(g_set_listener_font_w, g_set_listener_font)
XEN_NARGIFY_0(g_snd_version_w, g_snd_version)
XEN_ARGIFY_1(g_color_dialog_w, g_color_dialog)
XEN_ARGIFY_1(g_orientation_dialog_w, g_orientation_dialog)
XEN_ARGIFY_1(g_transform_dialog_w, g_transform_dialog)
XEN_ARGIFY_2(g_print_dialog_w, g_print_dialog)
XEN_NARGIFY_0(g_preferences_dialog_w, g_preferences_dialog)
XEN_NARGIFY_0(g_abort_w, g_abort)
XEN_NARGIFY_0(g_abortq_w, g_abortq)
#else
#define g_save_state_file_w g_save_state_file
#define g_set_save_state_file_w g_set_save_state_file
#define g_save_dir_w g_save_dir
#define g_set_save_dir_w g_set_save_dir
#define g_temp_dir_w g_temp_dir
#define g_set_temp_dir_w g_set_temp_dir
#define g_ladspa_dir_w g_ladspa_dir
#define g_set_ladspa_dir_w g_set_ladspa_dir
#define g_save_state_w g_save_state
#define g_exit_w g_exit
#define g_script_arg_w g_script_arg
#define g_set_script_arg_w g_set_script_arg
#define g_script_args_w g_script_args
#define g_window_x_w g_window_x
#define g_set_window_x_w g_set_window_x
#define g_window_y_w g_window_y
#define g_set_window_y_w g_set_window_y
#define g_window_width_w g_window_width
#define g_set_window_width_w g_set_window_width
#define g_window_height_w g_window_height
#define g_set_window_height_w g_set_window_height
#define g_just_sounds_w g_just_sounds
#define g_set_just_sounds_w g_set_just_sounds
#define g_audio_output_device_w g_audio_output_device
#define g_set_audio_output_device_w g_set_audio_output_device
#define g_audio_input_device_w g_audio_input_device
#define g_set_audio_input_device_w g_set_audio_input_device
#define g_minibuffer_history_length_w g_minibuffer_history_length
#define g_set_minibuffer_history_length_w g_set_minibuffer_history_length
#define g_auto_resize_w g_auto_resize
#define g_set_auto_resize_w g_set_auto_resize
#define g_color_cutoff_w g_color_cutoff
#define g_set_color_cutoff_w g_set_color_cutoff
#define g_color_inverted_w g_color_inverted
#define g_set_color_inverted_w g_set_color_inverted
#define g_color_scale_w g_color_scale
#define g_set_color_scale_w g_set_color_scale
#define g_selection_creates_region_w g_selection_creates_region
#define g_set_selection_creates_region_w g_set_selection_creates_region
#define g_print_length_w g_print_length
#define g_set_print_length_w g_set_print_length
#define g_show_indices_w g_show_indices
#define g_set_show_indices_w g_set_show_indices
#define g_show_backtrace_w g_show_backtrace
#define g_set_show_backtrace_w g_set_show_backtrace
#define g_trap_segfault_w g_trap_segfault
#define g_set_trap_segfault_w g_set_trap_segfault
#define g_with_relative_panes_w g_with_relative_panes
#define g_set_with_relative_panes_w g_set_with_relative_panes
#define g_with_background_processes_w g_with_background_processes
#define g_set_with_background_processes_w g_set_with_background_processes
#define g_with_file_monitor_w g_with_file_monitor
#define g_set_with_file_monitor_w g_set_with_file_monitor
#define g_tiny_font_w g_tiny_font
#define g_set_tiny_font_w g_set_tiny_font
#define g_peaks_font_w g_peaks_font
#define g_set_peaks_font_w g_set_peaks_font
#define g_bold_peaks_font_w g_bold_peaks_font
#define g_set_bold_peaks_font_w g_set_bold_peaks_font
#define g_axis_label_font_w g_axis_label_font
#define g_set_axis_label_font_w g_set_axis_label_font
#define g_axis_numbers_font_w g_axis_numbers_font
#define g_set_axis_numbers_font_w g_set_axis_numbers_font
#define g_listener_font_w g_listener_font
#define g_set_listener_font_w g_set_listener_font
#define g_snd_version_w g_snd_version
#define g_color_dialog_w g_color_dialog
#define g_orientation_dialog_w g_orientation_dialog
#define g_transform_dialog_w g_transform_dialog
#define g_print_dialog_w g_print_dialog
#define g_preferences_dialog_w g_preferences_dialog
#define g_abort_w g_abort
#define g_abortq_w g_abortq
#endif

void g_init_main(void)
{
  XEN_DEFINE_PROCEDURE(S_save_state,   g_save_state_w,   0, 1, 0, H_save_state);
#if HAVE_GUILE
  XEN_EVAL_C_STRING("(define %exit exit)");
#endif
#if HAVE_FORTH			/* exit is an existing word */
  XEN_DEFINE_PROCEDURE("snd-" S_exit,  g_exit_w,         0, 1, 0, H_exit);
#else
  XEN_DEFINE_PROCEDURE(S_exit,         g_exit_w,         0, 1, 0, H_exit);
#endif

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_state_file, g_save_state_file_w, H_save_state_file,
				   S_setB S_save_state_file, g_set_save_state_file_w, 0, 0, 1, 0);
  
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_save_dir, g_save_dir_w, H_save_dir,
				   S_setB S_save_dir, g_set_save_dir_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_temp_dir, g_temp_dir_w, H_temp_dir,
				   S_setB S_temp_dir, g_set_temp_dir_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_ladspa_dir, g_ladspa_dir_w, H_ladspa_dir,
				   S_setB S_ladspa_dir, g_set_ladspa_dir_w,  0, 0, 1, 0);

  #define H_start_hook S_start_hook " (filename): called upon start-up. If it returns " PROC_TRUE ", snd exits immediately."
  start_hook = XEN_DEFINE_HOOK(S_start_hook, 1, H_start_hook);                   /* arg = argv filename if any */

  #define H_before_exit_hook S_before_exit_hook " (): called upon exit. \
If it returns " PROC_TRUE ", Snd does not exit.  This can be used to check for unsaved edits."

  before_exit_hook = XEN_DEFINE_HOOK(S_before_exit_hook, 0, H_before_exit_hook);

  #define H_exit_hook S_exit_hook " (): called upon exit.  This can be used to perform cleanup activities."

  exit_hook = XEN_DEFINE_HOOK(S_exit_hook, 0, H_exit_hook);

  #define H_after_save_state_hook S_after_save_state_hook " (filename): called after Snd state has been saved; \
filename is the save state file."
  after_save_state_hook = XEN_DEFINE_HOOK(S_after_save_state_hook, 1, H_after_save_state_hook);

  #define H_before_save_state_hook S_before_save_state_hook " (filename): called before Snd state is saved. If \
the hook functions return " PROC_TRUE ", the save state process opens 'filename' for appending, rather than truncating."
  before_save_state_hook = XEN_DEFINE_HOOK(S_before_save_state_hook, 1, H_before_save_state_hook);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_script_arg, g_script_arg_w, H_script_arg, S_setB S_script_arg, g_set_script_arg_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE(S_script_args, g_script_args_w, 0, 0, 0, H_script_args);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_x, g_window_x_w, H_window_x,
				   S_setB S_window_x, g_set_window_x_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_y, g_window_y_w, H_window_y,
				   S_setB S_window_y, g_set_window_y_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_width, g_window_width_w, H_window_width,
				   S_setB S_window_width, g_set_window_width_w,  0, 0, 1, 0);  

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_window_height, g_window_height_w, H_window_height,
				   S_setB S_window_height, g_set_window_height_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_audio_output_device, g_audio_output_device_w, H_audio_output_device,
				   S_setB S_audio_output_device, g_set_audio_output_device_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_audio_input_device, g_audio_input_device_w, H_audio_input_device,
				   S_setB S_audio_input_device, g_set_audio_input_device_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_minibuffer_history_length, g_minibuffer_history_length_w, H_minibuffer_history_length,
				   S_setB S_minibuffer_history_length, g_set_minibuffer_history_length_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_auto_resize, g_auto_resize_w, H_auto_resize,
				   S_setB S_auto_resize, g_set_auto_resize_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_cutoff, g_color_cutoff_w, H_color_cutoff,
				   S_setB S_color_cutoff, g_set_color_cutoff_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_inverted, g_color_inverted_w, H_color_inverted,
				   S_setB S_color_inverted, g_set_color_inverted_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_color_scale, g_color_scale_w, H_color_scale,
				   S_setB S_color_scale, g_set_color_scale_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_selection_creates_region, g_selection_creates_region_w, H_selection_creates_region,
				   S_setB S_selection_creates_region, g_set_selection_creates_region_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_print_length, g_print_length_w, H_print_length,
				   S_setB S_print_length, g_set_print_length_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_indices, g_show_indices_w, H_show_indices,
				   S_setB S_show_indices, g_set_show_indices_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_show_backtrace, g_show_backtrace_w, H_show_backtrace,
				   S_setB S_show_backtrace, g_set_show_backtrace_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_trap_segfault, g_trap_segfault_w, H_trap_segfault,
				   S_setB S_trap_segfault, g_set_trap_segfault_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_relative_panes, g_with_relative_panes_w, H_with_relative_panes,
				   S_setB S_with_relative_panes, g_set_with_relative_panes_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_background_processes, g_with_background_processes_w, H_with_background_processes,
				   S_setB S_with_background_processes, g_set_with_background_processes_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_with_file_monitor, g_with_file_monitor_w, H_with_file_monitor,
				   S_setB S_with_file_monitor, g_set_with_file_monitor_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_tiny_font, g_tiny_font_w, H_tiny_font,
				   S_setB S_tiny_font, g_set_tiny_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_peaks_font, g_peaks_font_w, H_peaks_font,
				   S_setB S_peaks_font, g_set_peaks_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_bold_peaks_font, g_bold_peaks_font_w, H_bold_peaks_font,
				   S_setB S_bold_peaks_font, g_set_bold_peaks_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_axis_label_font, g_axis_label_font_w, H_axis_label_font,
				   S_setB S_axis_label_font, g_set_axis_label_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_axis_numbers_font, g_axis_numbers_font_w, H_axis_numbers_font,
				   S_setB S_axis_numbers_font, g_set_axis_numbers_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_listener_font, g_listener_font_w, H_listener_font,
				   S_setB S_listener_font, g_set_listener_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_just_sounds, g_just_sounds_w, H_just_sounds, 
				   S_setB S_just_sounds, g_set_just_sounds_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_snd_version,           g_snd_version_w,           0, 0, 0, H_snd_version);
  XEN_DEFINE_PROCEDURE(S_color_dialog,          g_color_dialog_w,          0, 1, 0, H_color_dialog);
  XEN_DEFINE_PROCEDURE(S_orientation_dialog,    g_orientation_dialog_w,    0, 1, 0, H_orientation_dialog);
  XEN_DEFINE_PROCEDURE(S_transform_dialog,      g_transform_dialog_w,      0, 1, 0, H_transform_dialog);
  XEN_DEFINE_PROCEDURE(S_print_dialog,          g_print_dialog_w,          0, 2, 0, H_print_dialog);
  XEN_DEFINE_PROCEDURE(S_preferences_dialog,    g_preferences_dialog_w,    0, 0, 0, H_preferences_dialog);
  XEN_DEFINE_PROCEDURE(S_abort,                 g_abort_w,                 0, 0, 0, H_abort);
  XEN_DEFINE_PROCEDURE(S_c_g,                   g_abortq_w,                0, 0, 0, H_abortQ);
}
