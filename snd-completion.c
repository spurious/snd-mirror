#include "snd.h"
#include "vct.h"
#include "sndlib-strings.h"

/* TODO  snd-apropos and help merged with Guile's?
 * TODO  completion activated in emacs (snd-as-subjob)
 */

#if HAVE_DIRENT_H
  #include <dirent.h>
#else
  #define dirent direct
  #if HAVE_SYS_NDIR_H
    #include <sys/ndir.h>
  #endif
  #if HAVE_SYS_DIR_H
    #include <sys/dir.h>
  #endif
  #if HAVE_NDIR_H
    #include <ndir.h>
  #endif
#endif

#define NUM_COMMANDS 563

static char *snd_commands[NUM_COMMANDS] ={
  S_abort, S_activate_listener, S_add_mark, S_add_player, S_add_sound_file_extension, 
  S_add_to_main_menu, S_add_to_menu, S_add_transform,
  S_after_graph_hook, S_after_open_hook,
  S_amp, S_amplitude_env, S_append_to_minibuffer, S_as_one_edit, S_ask_before_overwrite,
  S_audio_input_device, S_audio_output_device, S_audio_state_file,
  S_auto_resize, S_auto_update, S_autocorrelate, S_autocorrelation,
  S_axis_label_font, S_axis_numbers_font,

  S_backward_graph, S_backward_mark, S_backward_mix, S_backward_sample, 
  S_basic_color, S_before_fft_hook, S_bind_key,
  S_bold_button_font, S_bomb, S_button_font,

  S_c_g, S_call_apply, S_cepstrum, S_change_menu_label, 
  S_channel_style, S_channel_sync, S_channels, S_channels_combined, S_channels_separate,
  S_channels_superimposed, S_chans, S_chebyshev_transform, S_clear_audio_inputs,
  S_close_hook, S_close_sound, S_close_sound_file, 
  S_color2list, S_color_cutoff, S_color_dialog, S_color_inverted, S_color_scale, S_colorQ, S_colormap,
  S_comment, S_contrast, S_contrast_amp, S_contrast_func, S_contrasting,
  S_convolve_arrays, S_convolve_selection_with, S_convolve_with, S_corruption_time, S_count_matches,
  S_cursor, S_cursor_claim_selection, S_cursor_color, S_cursor_cross,
  S_cursor_follows_play, S_cursor_in_middle, S_cursor_in_view, S_cursor_line, S_cursor_no_action,
  S_cursor_on_left, S_cursor_on_right, S_cursor_style, S_cursor_update_display, S_cut,

  S_dac_folding, S_dac_size, S_data_clipped, S_data_color, S_data_format, S_data_location,
  S_default_output_chans, S_default_output_format, S_default_output_srate, S_default_output_type,
  S_define_envelope,
  S_delete_mark, S_delete_marks, S_delete_region, S_delete_sample, S_delete_samples, S_delete_selection, S_describe_audio,
  S_dismiss_all_dialogs, S_display_edits, S_dont_normalize, S_dot_size, S_during_open_hook,

  S_edit_fragment, S_edit_header_dialog, S_edit_hook, S_edit_position, S_edit_tree, S_edits,
  S_env_selection, S_env_sound, S_enved_active_env, S_enved_base, S_enved_clipping, S_enved_dBing,
  S_enved_dialog, S_enved_exping, S_enved_power, S_enved_selected_env, S_enved_target,
  S_enved_waveform_color, S_enved_waving, S_eps_bottom_margin, S_eps_file, S_eps_left_margin, 
  S_exit, S_exit_hook, S_expand, S_expand_hop,
  S_expand_length, S_expand_ramp, S_expanding,

  S_fft, S_fft_beta, S_fft_hook, S_fft_log_frequency, S_fft_log_magnitude,
  S_fft_size, S_fft_style, S_fft_window, S_ffting, S_fht, S_file_dialog, S_file_name,
  S_filter_dBing, S_filter_env, S_filter_env_in_hz, S_filter_env_order,
  S_filter_order, S_filter_selection, S_filter_sound, S_filter_waveform_color, S_filtering,
  S_find, S_find_mark, S_find_sound, S_finish_progress_report,
  S_fit_data_on_open, S_focus_active, S_focus_left, S_focus_middle,
  S_focus_right, S_forward_graph, S_forward_mark, S_forward_mix, S_forward_sample, S_fourier_transform, S_frames,
  S_free_mix_sample_reader, S_free_sample_reader, S_free_track_sample_reader,

  S_graph, S_graph_ps, S_graph_color, S_graph_cursor,
  S_graph_dots, S_graph_dots_and_lines, S_graph_filled, S_graph_hook,
  S_graph_lines, S_graph_lollipops, S_graph_style, S_graphing, S_graphs_horizontal,

  S_hadamard_transform, S_hankel_transform, S_header_type, S_help_dialog, S_help_text_font,
  S_hide_listener, S_highlight_color, S_html_dir,

  S_id_region, S_in, S_initial_x0, S_initial_x1, S_initial_y0, S_initial_y1, S_insert_region, S_insert_sample,
  S_insert_samples, S_insert_selection, S_insert_silence, S_insert_sound,

  S_just_sounds_hook,

  S_key, S_key_binding, S_key_press_hook, S_keyboard_no_action,

  S_left_sample, S_line_size, S_listener_color, S_listener_font, S_listener_prompt, S_load_colormap, S_loop_samples,

  S_make_color, S_make_mix_sample_reader, S_make_player,
  S_make_region, S_make_region_sample_reader, S_make_sample_reader, S_make_track_sample_reader, S_make_vct,
  S_map_across_all_chans, S_map_across_chans, S_map_across_sound_chans, S_map_all_chans, S_map_chan, S_map_chans, S_map_sound_chans,
  S_mark_color, S_mark_drag_hook, S_mark_name, S_mark_sample, S_mark_sync, S_mark_sync_max, S_markQ, S_marks,
  S_max_fft_peaks, S_max_regions, S_max_sounds, S_maxamp, S_memo_sound, S_menu_hook, S_min_dB, S_minibuffer_history_length, S_mix,
  S_mix_amp, S_mix_amp_changed_hook, S_mix_amp_env, S_mix_anchor, S_mix_chans, S_mix_color,
  S_mix_length, S_mix_locked, S_mix_name, S_mix_panel, S_mix_position, S_mix_position_changed_hook,
  S_mix_region, S_mix_sample_readerQ, S_mix_selection, S_mix_sound, S_mix_sound_channel, 
  S_mix_sound_index, S_mix_speed, S_mix_speed_changed_hook,
  S_mix_tag_height, S_mix_tag_width, S_mix_tag_y, S_mix_track,
  S_mix_vct, S_mix_waveform_height, S_mixQ, S_mixes,
  S_mouse_drag_hook, S_mouse_press_hook, S_mouse_release_hook, S_movies, S_multichannel_mix_hook, S_mus_error_hook,

  S_name_click_hook, S_new_sound, S_next_mix_sample, S_next_sample, S_next_track_sample, S_normal_fft,
  S_normalize_by_channel, S_normalize_by_sound, S_normalize_fft, S_normalize_globally, S_normalize_on_open, S_normalize_view,

  S_open_alternate_sound, S_open_hook, S_open_raw_sound, S_open_sound, S_open_sound_file, S_orientation_dialog,
  S_output_comment_hook, S_output_name_hook,

  S_peaks, S_play, S_play_and_wait, S_play_hook, S_play_mix, S_play_region, S_play_selection, S_play_track, S_playerQ,
  S_position_color, S_prefix_arg, S_preload_directory, S_preload_file,
  S_previous_files_sort, S_previous_sample, S_print_length,
  S_progress_report, S_prompt_in_minibuffer, S_protect_region, S_pushed_button_color,

  S_raw_chans, S_raw_format, S_raw_srate, S_read_only,
  S_recorder_autoload, S_recorder_buffer_size, S_recorder_dialog,
  S_recorder_file, S_recorder_gain, S_recorder_in_amp, S_recorder_in_format, 
  S_recorder_max_duration, S_recorder_out_amp, S_recorder_out_chans,
  S_recorder_out_format, S_recorder_srate, S_recorder_trigger, S_redo, 
  S_region_chans, S_region_dialog, S_region_id, S_region_length,
  S_region_maxamp, S_region_sample, S_region_samples, S_region_samples_vct,
  S_region_srate, S_regionQ, S_regions,
  S_remove_from_menu, S_report_in_minibuffer, S_restore_control_panel,
  S_restore_marks, S_restore_region, S_reverb_decay, S_reverb_feedback, S_reverb_funcs, S_reverb_length, S_reverb_lowpass, S_reverb_scale,
  S_reverbing, S_reverse_selection, S_reverse_sound, S_revert_sound, S_right_sample,

  S_sample, S_sample_reader_at_endQ, S_sample_readerQ, S_samples, S_samples2sound_data, S_samples_vct, S_sash_color,
  S_save_control_panel, S_save_dir, S_save_edit_history, S_save_envelopes, S_save_hook,
  S_save_listener, S_save_macros, S_save_marks, S_save_options,
  S_save_region, S_save_selection, S_save_sound, S_save_sound_as, S_save_state, S_save_state_file,
  S_save_state_on_exit, S_scale_by, S_scale_selection_by, S_scale_selection_to, S_scale_to,
  S_scan_across_all_chans, S_scan_across_chans, S_scan_across_sound_chans, S_scan_all_chans, S_scan_chan, S_scan_chans, S_scan_sound_chans,
  S_search_procedure, S_select_all, S_select_region,
  S_selected_channel, S_selected_data_color, S_selected_graph_color, S_selected_mix, S_selected_mix_color, S_selected_sound,
  S_selection_color, S_selection_creates_region, S_selection_length, S_selection_member, S_selection_position, 
  S_selection_to_temp, S_selection_to_temps, S_selectionQ,
  S_short_file_name, S_show_all_axes, S_show_axes, S_show_controls, S_show_fft_peaks, S_show_indices, S_show_listener, S_show_marks,
  S_show_mix_waveforms, S_show_no_axes, S_show_selection_transform, S_show_usage_stats, S_show_x_axis, S_show_y_zero,
  S_sinc_width, S_smooth, S_smooth_selection,
  S_snd_apropos, S_snd_error, S_snd_error_hook, S_snd_help, S_snd_spectrum, S_snd_tempnam, S_snd_version, S_snd_warning, S_snd_warning_hook,
  S_sonogram, S_sound_files_in_directory,
  S_sound_to_temp, S_sound_to_temps, S_soundQ, S_soundfont_info, S_sounds,
  S_spectro_cutoff, S_spectro_hop, S_spectro_start, S_spectro_x_angle, S_spectro_x_scale, S_spectro_y_angle, S_spectro_y_scale,
  S_spectro_z_angle, S_spectro_z_scale, S_spectrogram, S_spectrum_env, S_speed, S_speed_as_float, S_speed_as_ratio, S_speed_as_semitone,
  S_speed_style, S_speed_tones, S_squelch_update, S_srate, S_srate_env,
  S_src_selection, S_src_sound, S_start_hook, S_start_playing, S_start_playing_hook, S_start_progress_report,
  S_stop_player, S_stop_playing, S_stop_playing_channel_hook, S_stop_playing_hook, S_stop_playing_region_hook,
  S_swap_channels, S_sync, S_syncd_marks,

  S_temp_dir, S_temp_filenames, S_temp_to_selection, S_temp_to_sound, S_temps_to_selection, S_temps_to_sound, S_text_focus_color,
  S_tiny_font, S_track_sample_readerQ,
  S_transform_dialog, S_transform_sample, S_transform_samples, S_transform_samples_vct, 
  S_transform_size, S_transform_type, S_trap_segfault,

  S_unbind_key, S_undo, S_undo_hook, S_uniting, S_update_fft, S_update_graph, S_update_lisp_graph, 
  S_update_sound, S_use_raw_defaults, S_use_sinc_interp,

  S_vct2list, S_vct_samples, S_vct_sound_file, S_vct_addB, S_vct_copy, S_vct_doB, S_vct_fillB, S_vct_length, S_vct_mapB, S_vct_moveB,
  S_vct_multiplyB, S_vct_offsetB, S_vct_peak, S_vct_ref, S_vct_scaleB, S_vct_setB, S_vct_subtractB, S_vct_p, S_vcts_doB, S_vcts_mapB,
  S_verbose_cursor, S_view_sound, S_vu_font, S_vu_font_size, S_vu_size,

  S_walsh_transform, S_wavelet_transform, S_wavelet_type, S_waving, S_wavo, S_wavo_hop, S_wavo_trace,
  S_window_height, S_window_width, S_window_x, S_window_y, S_with_mix_tags,

  S_x_axis_style, S_x_bounds, S_x_in_samples, S_x_in_seconds, S_x_position_slider, S_x_to_one, S_x_zoom_slider,

  S_y_bounds, S_y_position_slider, S_y_zoom_slider, S_yes_or_no_p,

  S_zero_pad, S_zoom_color, S_zoom_focus_style
};

static char *current_match = NULL;

static int complete_one_set(char *text, int num_commands, char **commands)
{
  int i, j, len, curlen, matches = 0;
  len = strlen(text);
  for (i = 0; i < num_commands; i++)
    {
      if (text[0] < commands[i][0]) break;
      if (text[0] == commands[i][0])
	{
	  if (strncmp(text, commands[i], len) == 0)
	    {
	      matches++;
	      add_possible_completion(commands[i]);
	      if (current_match == NULL)
		current_match = copy_string(commands[i]);
	      else 
		{
		  curlen = snd_strlen(current_match);
		  for (j = 0; j < curlen; j++)
		    if (current_match[j] != commands[i][j])
		      {
			current_match[j] = '\0';
			break;
		      }
		}
	    }
	}
    }
  return(matches);
}

#if HAVE_GUILE
  int mus_num_commands(void);
  char **mus_commands(void);
#endif

int sndlib_num_commands(void);
const char **sndlib_commands(void);

#if DEBUGGING
void check_snd_commands(void);
void check_snd_commands(void)
{
  int i, len;
  char **names;
  if (strcmp(snd_commands[NUM_COMMANDS-1], S_zoom_focus_style) != 0)
    fprintf(stderr, "last command (%d) is %s?", 
	    NUM_COMMANDS, 
	    snd_commands[NUM_COMMANDS - 1]);
  for (i = 1; i < NUM_COMMANDS; i++)
    if (strcmp(snd_commands[i-1], snd_commands[i]) >= 0)
      fprintf(stderr, "%s >= %s\n", 
	      snd_commands[i-1], 
	      snd_commands[i]);
  names = (char **)sndlib_commands();
  len = sndlib_num_commands();
  for (i = 1; i < len; i++)
    if (strcmp(names[i-1], names[i]) >= 0)
      fprintf(stderr, "%s >= %s\n", 
	      names[i-1], 
	      names[i]);
#if HAVE_GUILE
  names = mus_commands();
  len = mus_num_commands();
  for (i = 1; i < len; i++)
    if (strcmp(names[i-1], names[i]) >= 0)
      fprintf(stderr, "%s >= %s\n", 
	      names[i-1], 
	      names[i]);
#endif
}
#endif


char *command_completer(char *original_text)
{
  int i, len, beg, matches = 0;
  char *text;
  /* first back up to some delimiter to get the current command */
  current_match = NULL;
  set_completion_matches(0);
  if ((original_text) && (*original_text))
    {
      len = strlen(original_text);
      for (i = len-1; i >= 0; i--)
	if ((!(isalpha((int)(original_text[i])))) &&
	    (!(isdigit((int)(original_text[i])))) &&
	    (original_text[i] != '-'))
	  break;
      beg = i+1;
      if (beg == len) return(copy_string(original_text));
      if (beg > 0) 
	text = (char *)(original_text+beg);
      else text = original_text;
      matches = complete_one_set(text, NUM_COMMANDS, snd_commands);
      matches += complete_one_set(text, sndlib_num_commands(), (char **)sndlib_commands());
#if HAVE_GUILE
      matches += complete_one_set(text, mus_num_commands(), mus_commands());
#endif
    }
  else return(copy_string(original_text));
  set_completion_matches(matches);
  if ((current_match) && (*current_match))
    {
      if (beg == 0)
	return(current_match);
      else
	{
	  len = snd_strlen(current_match) + beg + 2;
	  text = (char *)CALLOC(len, sizeof(char));
	  strncpy(text, original_text, beg);
	  strcat(text, current_match);
	  FREE(current_match);
	  return(text);
	}
    }
  return(copy_string(original_text));
}

/* ---------------- COMMAND/FILENAME COMPLETIONS ---------------- */

typedef char *(*completer_func)(char *text);
static completer_func *completer_funcs = NULL;
static int completer_funcs_size = 0;
static int completer_funcs_end = 0;

int add_completer_func(char *(*func)(char *))
{
  if (completer_funcs_size == completer_funcs_end)
    {
      completer_funcs_size += 8;
      if (completer_funcs == NULL)
	completer_funcs = (completer_func *)CALLOC(completer_funcs_size, sizeof(completer_func));
      else completer_funcs = (completer_func *)REALLOC(completer_funcs, completer_funcs_size * sizeof(completer_func));
    }
  completer_funcs[completer_funcs_end] = func;
  completer_funcs_end++;
  return(completer_funcs_end-1);
}

static int completion_matches = 0;
int get_completion_matches(void) {return(completion_matches);}
void set_completion_matches(int matches) {completion_matches = matches;}
static int save_completions = 0;
static char **possible_completions = NULL;
static int possible_completions_size = 0;
static int possible_completions_ctr = 0;

void set_save_completions(int save) {save_completions = save;}

void add_possible_completion(char *text)
{
  int i;
  if (save_completions)
    {
      if (possible_completions_size == possible_completions_ctr)
	{
	  possible_completions_size += 16;
	  if (possible_completions == NULL)
	    possible_completions = (char **)CALLOC(possible_completions_size, sizeof(char *));
	  else
	    {
	      possible_completions = (char **)REALLOC(possible_completions, possible_completions_size * sizeof(char *));
	      for (i = possible_completions_ctr; i < possible_completions_size; i++) possible_completions[i] = NULL;
	    }
	}
      if (possible_completions[possible_completions_ctr]) FREE(possible_completions[possible_completions_ctr]);
      possible_completions[possible_completions_ctr] = copy_string(text);
      possible_completions_ctr++;
    }
}

void display_completions(snd_state *ss)
{
  if (possible_completions_ctr > 0)
    snd_completion_help(ss, possible_completions_ctr, possible_completions);
}

char *complete_text(char *text, int func)
{
  /* given text, call proc table entry func, return new text (not text!) */
  completion_matches = -1; /* i.e. no completer */
  possible_completions_ctr = 0;
  if ((func >= 0) && 
      (func < completer_funcs_end))
    return((*completer_funcs[func])(text));
  else return(copy_string(text));
}

void clear_possible_completions(void) {possible_completions_ctr = 0;}

char *filename_completer(char *text)
{
#if HAVE_OPENDIR
  /* assume text is a partial filename */
  /* get directory name, opendir, read files checking for match */
  /* return name of same form as original (i.e. don't change user's directory indication) */
  /* if directory, add "/" -- is_directory(name) static in snd-xfile.c */
  char *full_name = NULL, *dir_name = NULL, *file_name = NULL, *current_match = NULL;
  int i, j, k, len, curlen, matches = 0;
  struct dirent *dirp;
  DIR *dpos;

  full_name = mus_file_full_name(text);
  len = snd_strlen(full_name);
  for (i = len-1; i > 0; i--)
    if (full_name[i] == '/')
      break;

  dir_name = (char *)CALLOC(i+1, sizeof(char));
  strncpy(dir_name, full_name, i);
  file_name = (char *)CALLOC(len-i+2, sizeof(char));
  for (j = 0, k = i+1; k < len; j++, k++) 
    file_name[j] = full_name[k];
  if (full_name) 
    {
      FREE(full_name); 
      full_name = NULL;
    }
  len = snd_strlen(file_name);
  if ((dpos = opendir(dir_name)) != NULL)
    {
      while ((dirp = readdir(dpos)) != NULL)
	{
	  if (dirp->d_name[0] != '.')
	    {
	      /* match dirp->d_name against rest of text */
	      if (strncmp(dirp->d_name, file_name, len) == 0)
		{
		  matches++;
		  add_possible_completion(dirp->d_name);
		  if (current_match == NULL)
		    current_match = copy_string(dirp->d_name);
		  else 
		    {
		      curlen = strlen(current_match);
		      for (j = 0; j < curlen; j++)
			if (current_match[j] != dirp->d_name[j])
			  {
			    current_match[j] = '\0';
			    break;
			  }
		      }
		}
	    }
	}
#if defined(CLOSEDIR_VOID)
      closedir(dpos);
#else
      if (closedir(dpos) != 0) 
	snd_error("closedir %s failed! [%s[%d] %s]",
		  dir_name,
		  __FILE__, __LINE__, __FUNCTION__);
#endif
    }
  if (dir_name) FREE(dir_name);
  if (file_name) FREE(file_name);
  set_completion_matches(matches);
  if ((current_match) && (*current_match))
    {
      /* attach matched portion to user's indication of dir */
      len = snd_strlen(text);
      for (i = len-1; i >= 0; i--)
	if (text[i] == '/')
	  break;
      if (i < 0) return(current_match);
      curlen = strlen(current_match) + len + 3;
      file_name = (char *)CALLOC(curlen, sizeof(char));
      strncpy(file_name, text, i+1);
      strcat(file_name, current_match);
      if (is_directory(file_name)) strcat(file_name, "/");
      FREE(current_match);
      return(file_name);
    }
#endif
  return(copy_string(text));
}

char *srate_completer(char *text)
{
  set_completion_matches(1);
  while ((text) && (*text == ' ')) text++;
  if (strcmp(text, "4410") == 0) return(copy_string("44100"));
  if (strcmp(text, "441") == 0) return(copy_string("44100"));
  if (strcmp(text, "44") == 0) return(copy_string("44100"));
  if (strcmp(text, "2205") == 0) return(copy_string("22050"));
  if (strcmp(text, "220") == 0) return(copy_string("22050"));
  if (strcmp(text, "22") == 0) return(copy_string("22050"));
  if (strcmp(text, "2") == 0) return(copy_string("22050"));
  if (strcmp(text, "4800") == 0) return(copy_string("48000"));
  if (strcmp(text, "480") == 0) return(copy_string("48000"));
  if (strcmp(text, "48") == 0) return(copy_string("48000"));
  if (strcmp(text, "800") == 0) return(copy_string("8000"));
  if (strcmp(text, "80") == 0) return(copy_string("8000"));
  if (strcmp(text, "8") == 0) return(copy_string("8000"));
  set_completion_matches(0);
  return(copy_string(text));
}

char *info_completer(char *text)
{
  snd_info *sp = NULL;
  char *new_text, *new_file;
  int i, beg, parens, len;
  sp = selected_sound(get_global_state());
  if (sp)
    {
      if (sp->evaling) return(copy_string(text));   /* C-x C-x so nothing useful for completion to work on */
      if (sp->searching) return(copy_string(text)); /* C-s or C-r so as above */
      if ((sp->marking) || (sp->finding_mark)) return(copy_string(text)); /* C-x C-m etc */
      if (sp->printing) return(copy_string(text));  /* C-x C-d so anything is possible */
      if (sp->amping) return(env_name_completer(text));
      if (use_filename_completer(sp->filing)) return(filename_completer(text));
      if (sp->loading) return(filename_completer(text)); /* C-x C-l */
      if (sp->macroing) 
	{
	  new_text = command_completer(text);
	  if (get_completion_matches() == 0)
	    {
	      beg = 0;
	      parens = 0;
	      /* filename would have to be a string in this context */
	      len = snd_strlen(text);
	      for (i = 0; i < len; i++)
		if (text[i] == '\"')
		  {
		    beg = i + 1;
		    parens++;
		    break;
		  }
	      if ((beg > 0) && (parens & 1)) /* i.e. there is a string and we're in it */
		{
		  if (new_text) FREE(new_text);
		  new_file = filename_completer((char *)(text+beg));
		  len = beg + 2 + snd_strlen(new_file);
		  new_text = (char *)CALLOC(len, sizeof(char));
		  strncpy(new_text, text, beg);
		  strcat(new_text, new_file);
		  return(new_text);
		}
	      else return(new_text);
	    }
	  else return(new_text);
	}
      return(copy_string(text));
    }
  else return(command_completer(text));
}

static int find_indentation(char *str, int loc)
{
  int line_beg = 0, open_paren = -1, parens, i;
  parens = 0;
  for (i = loc-1; i >= 0; i--)
    {
      if (str[i] == ')') parens--;
      if (str[i] == '(') parens++;
      if (parens == 1) 
	{
	  open_paren = i; 
	  break;
	}
    }
  if (open_paren == -1) return(1);
  if (open_paren == 0) return(3);
  for (i = open_paren-1; i > 0; i--)
    {
      if (str[i] == '\n') 
	{
	  line_beg = i; 
	  break;
	}
    }
  if (line_beg == 0) return(1);
  return(open_paren - line_beg + 2);
}

char *complete_listener_text(char *old_text, int end, int *try_completion, char **to_file_text)
{
  int len, i, k, spaces, text_pos = 0, cr_pos = 0;
  char *new_text = NULL, *file_text = NULL, *new_file = NULL;
  len = strlen(old_text);
  for (i = len-1; i > 0; i--)
    {
      if (old_text[i] == '\n')
	{
	  /* tab as indentation */
	  /* look at previous line to decide */
	  spaces = find_indentation(old_text, i);
	  if (spaces > 0)
	    {
	      file_text = (char *)CALLOC(spaces + 1, sizeof(char));
	      for (k = 0; k < spaces; k++) file_text[k] = ' ';
	      file_text[spaces] = 0;
	      append_listener_text(end, file_text);
	      FREE(file_text);
	      file_text = NULL;
	    }
	  (*try_completion) = 0;
	  return(NULL);
	}
      if (old_text[i] == ';')
	{
	  /* this isn't quite right, but how much effort should we put in it? */
	  spaces = 20;
	  for (k = i-1; k > 0; k--) 
	    if (old_text[k] == '\n') 
	      {
		cr_pos = k; 
		break;
	      } 
	    else 
	      if ((!(isspace((int)(old_text[k])))) && 
		  (text_pos == 0)) 
		text_pos = k;
	  if (text_pos > 0)
	    text_pos -= cr_pos;
	  if (cr_pos == 0) spaces--; 
	  if (text_pos < spaces)
	    {
	      file_text = (char *)CALLOC(spaces + 2, sizeof(char));
	      for (k = text_pos+1; k < spaces; k++) file_text[k - text_pos - 1] = ' ';
	      file_text[spaces] = ';';
	      file_text[spaces + 1] = 0;
	      append_listener_text(end - 1, file_text);
	      FREE(file_text);
	    }
	  (*try_completion) = 0;
	  return(NULL);
	}
      if (old_text[i] == '\"')
	{
	  file_text = copy_string((char *)(old_text + i + 1));
	  new_file = filename_completer(file_text);
	  len = i + 2 + snd_strlen(new_file);
	  new_text = (char *)CALLOC(len, sizeof(char));
	  strncpy(new_text, old_text, i + 1);
	  strcat(new_text, new_file);
	  if (new_file) FREE(new_file);
	  break;
	}
      if (isspace((int)(old_text[i]))) break;
    }
  if (new_text == NULL) new_text = command_completer(old_text);
  (*try_completion) = 1;
  (*to_file_text) = file_text;
  return(new_text);
}



#if HAVE_GUILE

static char *snd_apropos(char *old_text)
{
  int i, matches = 0, len = 0;
  char *new_text = NULL, *buffer = NULL;
  clear_possible_completions();
  set_save_completions(TRUE);
  new_text = command_completer(old_text);
  matches = get_completion_matches();
  if (new_text) {FREE(new_text); new_text = NULL;}
  if ((matches > 0) && (possible_completions_ctr > 0))
    {
      for (i = 0; i < possible_completions_ctr; i++) 
	len += (snd_strlen(possible_completions[i]) + 3);
      buffer = (char *)CALLOC(len, sizeof(char));
      for (i = 0; i < possible_completions_ctr; i++)
	{
	  strcat(buffer, possible_completions[i]);
	  strcat(buffer, " ");
	}
    }
  set_save_completions(FALSE);
  return(buffer);
}

static SCM g_apropos(SCM text)
{
  #define H_apropos "(" S_snd_apropos " name) returns possible continuations of name"
  char *res = NULL, *str = NULL;
  SCM val = SCM_BOOL_F;
  SCM_ASSERT((gh_string_p(text) || gh_symbol_p(text)), text, SCM_ARG1, S_snd_apropos);
  if (gh_string_p(text))
    str = TO_NEW_C_STRING(text);
  else str = gh_symbol2newstr(text, NULL);
  res = snd_apropos(str);
  if (str) {free(str); str = NULL;}
  if (res) 
    {
      val = TO_SCM_STRING(res);
      FREE(res);
    }
  return(val);
}

static SCM g_save_listener(SCM filename)
{
  #define H_save_listener "(" S_save_listener " filename) saves the current listener text in filename"
  char *urn = NULL;
  FILE *fp = NULL;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_save_listener);
  urn = TO_NEW_C_STRING(filename);
  fp = fopen(urn, "w");
  if (fp)
    {
      save_listener_text(fp);
      if (fclose(fp) != 0)
	snd_error("save-listener: close file %s: %s\n", urn, strerror(errno));
    }
  else scm_throw(CANNOT_SAVE,
		 SCM_LIST3(TO_SCM_STRING(S_save_listener),
			   filename,
			   TO_SCM_STRING(strerror(errno))));
  if (urn) free(urn);
  return(filename);
}

void g_init_completions(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure(S_snd_apropos,   SCM_FNC g_apropos, 1, 0, 0),       H_apropos);
  DEFINE_PROC(gh_new_procedure(S_save_listener, SCM_FNC g_save_listener, 1, 0, 0), H_save_listener);
}
#endif
