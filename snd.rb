# snd.rb: Snd Ruby code and tests

set_window_width 800
set_window_height 500

set_listener_font "9x15"
set_help_text_font "9x15"
set_axis_numbers_font "9x15"

set_show_mix_waveforms #t
set_trap_segfault #f
set_show_backtrace #t
set_show_indices #t

set_listener_prompt ":"
show_listener

beige = make_color 0.96, 0.96, 0.86
blue = make_color 0, 0, 1
set_selected_graph_color beige
set_selected_data_color blue

def car(v)
  v[0]
end

$mouse_enter_graph_hook = Proc.new {|snd, chn| 
			            if sound? snd then
			               focus_widget car channel_widgets snd, chn
                                    end 
                                   }

$mouse_enter_listener_hook = Proc.new { |widget| 
                                        focus_widget widget 
                                      }

def display_energy(snd, chn)
  ls = left_sample
  rs = right_sample
  data1 = make_graph_data(snd, chn)
  data = data1
  if not vct? data
    data = data1[1]
  end
  len = vct_length data
  sr = srate snd
  y_max = y_zoom_slider(snd, chn)
  vct_multiply!(data, data)
  graph(data, "energy", ls / sr, rs / sr, 0.0, y_max * y_max, snd, chn, false)
  end



# snd-test.scm translations
# ---------------------------------------- test 0 ----------------------------------------

if Enved_amplitude != 0 then snd_print sprintf("# Enved_amplitude => %d\n", Enved_amplitude) end
if Autocorrelation != 4 then snd_print sprintf("# Autocorrelation => %d\n", Autocorrelation) end
if Bartlett_window != 4 then snd_print sprintf("# Bartlett_window => %d\n", Bartlett_window) end
if Blackman2_window != 6 then snd_print sprintf("# Blackman2_window => %d\n", Blackman2_window) end
if Blackman3_window != 7 then snd_print sprintf("# Blackman3_window => %d\n", Blackman3_window) end
if Blackman4_window != 8 then snd_print sprintf("# Blackman4_window => %d\n", Blackman4_window) end
if Cauchy_window != 12 then snd_print sprintf("# Cauchy_window => %d\n", Cauchy_window) end
if Channels_combined != 1 then snd_print sprintf("# Channels_combined => %d\n", Channels_combined) end
if Channels_separate != 0 then snd_print sprintf("# Channels_separate => %d\n", Channels_separate) end
if Channels_superimposed != 2 then snd_print sprintf("# Channels_superimposed => %d\n", Channels_superimposed) end
if Chebyshev_transform != 5 then snd_print sprintf("# Chebyshev_transform => %d\n", Chebyshev_transform) end
if Cursor_in_middle != 3 then snd_print sprintf("# Cursor_in_middle => %d\n", Cursor_in_middle) end
if Cursor_in_view != 0 then snd_print sprintf("# Cursor_in_view => %d\n", Cursor_in_view) end
if Cursor_no_action != 5 then snd_print sprintf("# Cursor_no_action => %d\n", Cursor_no_action) end
if Cursor_on_left != 1 then snd_print sprintf("# Cursor_on_left => %d\n", Cursor_on_left) end
if Cursor_on_right != 2 then snd_print sprintf("# Cursor_on_right => %d\n", Cursor_on_right) end
if Cursor_update_display != 4 then snd_print sprintf("# Cursor_update_display => %d\n", Cursor_update_display) end
if Dolph_chebyshev_window != 16 then snd_print sprintf("# Dolph_chebyshev_window => %d\n", Dolph_chebyshev_window) end
if Exponential_window != 9 then snd_print sprintf("# Exponential_window => %d\n", Exponential_window) end
if Zoom_focus_active != 2 then snd_print sprintf("# Zoom_focus_active => %d\n", Zoom_focus_active) end
if Zoom_focus_left != 0 then snd_print sprintf("# Zoom_focus_left => %d\n", Zoom_focus_left) end
if Zoom_focus_middle != 3 then snd_print sprintf("# Zoom_focus_middle => %d\n", Zoom_focus_middle) end
if Zoom_focus_right != 1 then snd_print sprintf("# Zoom_focus_right => %d\n", Zoom_focus_right) end
if Fourier_transform != 0 then snd_print sprintf("# Fourier_transform => %d\n", Fourier_transform) end
if Gaussian_window != 14 then snd_print sprintf("# Gaussian_window => %d\n", Gaussian_window) end
if Graph_dots != 1 then snd_print sprintf("# Graph_dots => %d\n", Graph_dots) end
if Graph_dots_and_lines != 3 then snd_print sprintf("# Graph_dots_and_lines => %d\n", Graph_dots_and_lines) end
if Graph_filled != 2 then snd_print sprintf("# Graph_filled => %d\n", Graph_filled) end
if Graph_lines != 0 then snd_print sprintf("# Graph_lines => %d\n", Graph_lines) end
if Graph_lollipops != 4 then snd_print sprintf("# Graph_lollipops => %d\n", Graph_lollipops) end
if Hadamard_transform != 7 then snd_print sprintf("# Hadamard_transform => %d\n", Hadamard_transform) end
if Haar_transform != 8 then snd_print sprintf("# Haar_transform => %d\n", Haar_transform) end
if Hamming_window != 5 then snd_print sprintf("# Hamming_window => %d\n", Hamming_window) end
if Hankel_transform != 2 then snd_print sprintf("# Hankel_transform => %d\n", Hankel_transform) end
if Hanning_window != 1 then snd_print sprintf("# Hanning_window => %d\n", Hanning_window) end
if Kaiser_window != 11 then snd_print sprintf("# Kaiser_window => %d\n", Kaiser_window) end
if Keyboard_no_action != 6 then snd_print sprintf("# Keyboard_no_action => %d\n", Keyboard_no_action) end
if Cepstrum != 6 then snd_print sprintf("# Cepstrum => %d\n", Cepstrum) end
if Graph_transform_once != 0 then snd_print sprintf("# Graph_transform_once => %d\n", Graph_transform_once) end
if Parzen_window != 3 then snd_print sprintf("# Parzen_window => %d\n", Parzen_window) end
if Poisson_window != 13 then snd_print sprintf("# Poisson_window => %d\n", Poisson_window) end
if Rectangular_window != 0 then snd_print sprintf("# Rectangular_window => %d\n", Rectangular_window) end
if Riemann_window != 10 then snd_print sprintf("# Riemann_window => %d\n", Riemann_window) end
if Graph_transform_as_sonogram != 1 then snd_print sprintf("# Graph_transform_as_sonogram => %d\n", Graph_transform_as_sonogram) end
if Graph_transform_as_spectrogram != 2 then snd_print sprintf("# Graph_transform_as_spectrogram => %d\n", Graph_transform_as_spectrogram) end
if Graph_time_once != 0 then snd_print sprintf("# Graph_time_once => %d\n", Graph_time_once) end
if Graph_time_as_wavogram != 1 then snd_print sprintf("# Graph_time_as_wavogram => %d\n", Graph_time_as_wavogram) end
if Enved_spectrum != 1 then snd_print sprintf("# Enved_spectrum => %d\n", Enved_spectrum) end
if Speed_control_as_float != 0 then snd_print sprintf("# Speed_control_as_float => %d\n", Speed_control_as_float) end
if Speed_control_as_ratio != 1 then snd_print sprintf("# Speed_control_as_ratio => %d\n", Speed_control_as_ratio) end
if Speed_control_as_semitone != 2 then snd_print sprintf("# Speed_control_as_semitone => %d\n", Speed_control_as_semitone) end
if Enved_srate != 2 then snd_print sprintf("# Enved_srate => %d\n", Enved_srate) end
if Tukey_window != 15 then snd_print sprintf("# Tukey_window => %d\n", Tukey_window) end
if Walsh_transform != 3 then snd_print sprintf("# Walsh_transform => %d\n", Walsh_transform) end
if Wavelet_transform != 1 then snd_print sprintf("# Wavelet_transform => %d\n", Wavelet_transform) end
if Welch_window != 2 then snd_print sprintf("# Welch_window => %d\n", Welch_window) end
if Cursor_cross != 0 then snd_print sprintf("# Cursor_cross => %d\n", Cursor_cross) end
if Cursor_line != 1 then snd_print sprintf("# Cursor_line => %d\n", Cursor_line) end
if Dont_normalize_transform != 0 then snd_print sprintf("# Dont_normalize_transform => %d\n", Dont_normalize_transform) end
if Normalize_transform_by_channel != 1 then snd_print sprintf("# Normalize_transform_by_channel => %d\n", Normalize_transform_by_channel) end
if Normalize_transform_by_sound != 2 then snd_print sprintf("# Normalize_transform_by_sound => %d\n", Normalize_transform_by_sound) end
if Normalize_transform_globally != 3 then snd_print sprintf("# Normalize_transform_globally => %d\n", Normalize_transform_globally) end
if X_axis_in_samples != 1 then snd_print sprintf("# X_axis_in_samples => %d\n", X_axis_in_samples) end
if X_axis_in_beats != 4 then snd_print sprintf("# X_axis_in_beats => %d\n", X_axis_in_beats) end
if X_axis_in_seconds != 0 then snd_print sprintf("# X_axis_in_seconds => %d\n", X_axis_in_seconds) end
if X_axis_as_percentage != 2 then snd_print sprintf("# X_axis_as_percentage => %d\n", X_axis_as_percentage) end
if Enved_add_point != 0 then snd_print sprintf("# Enved_add_point => %d\n", Enved_add_point) end
if Enved_delete_point != 1 then snd_print sprintf("# Enved_delete_point => %d\n", Enved_delete_point) end
if Enved_move_point != 2 then snd_print sprintf("# Enved_move_point => %d\n", Enved_move_point) end
if Time_graph != 0 then snd_print sprintf("# Time_graph => %d\n", Time_graph) end
if Transform_graph != 1 then snd_print sprintf("# Transform_graph => %d\n", Transform_graph) end
if Lisp_graph != 2 then snd_print sprintf("# Lisp_graph => %d\n", Lisp_graph) end
if Copy_context != 0 then snd_print sprintf("# Copy_context => %d\n", Copy_context) end
if Cursor_context != 3 then snd_print sprintf("# Cursor_context => %d\n", Cursor_context) end
if Selection_context != 2 then snd_print sprintf("# Selection_context => %d\n", Selection_context) end
if Mus_next != 0 then snd_print sprintf("# Mus_next => %d\n", Mus_next) end
if Mus_aifc != 1 then snd_print sprintf("# Mus_aifc => %d\n", Mus_aifc) end
if Mus_riff != 2 then snd_print sprintf("# Mus_riff => %d\n", Mus_riff) end
if Mus_nist != 4 then snd_print sprintf("# Mus_nist => %d\n", Mus_nist) end
if Mus_raw != 10 then snd_print sprintf("# Mus_raw => %d\n", Mus_raw) end
if Mus_ircam != 14 then snd_print sprintf("# Mus_ircam => %d\n", Mus_ircam) end
if Mus_aiff != 56 then snd_print sprintf("# Mus_aiff => %d\n", Mus_aiff) end
if Mus_bicsf != 3 then snd_print sprintf("# Mus_bicsf => %d\n", Mus_bicsf) end
if Mus_voc != 8 then snd_print sprintf("# Mus_voc => %d\n", Mus_voc) end
if Mus_svx != 7 then snd_print sprintf("# Mus_svx => %d\n", Mus_svx) end
if Mus_soundfont != 31 then snd_print sprintf("# Mus_soundfont => %d\n", Mus_soundfont) end
if Mus_bshort != 1 then snd_print sprintf("# Mus_bshort => %d\n", Mus_bshort) end
if Mus_lshort != 10 then snd_print sprintf("# Mus_lshort => %d\n", Mus_lshort) end
if Mus_mulaw != 2 then snd_print sprintf("# Mus_mulaw => %d\n", Mus_mulaw) end
if Mus_alaw != 6 then snd_print sprintf("# Mus_alaw => %d\n", Mus_alaw) end
if Mus_byte != 3 then snd_print sprintf("# Mus_byte => %d\n", Mus_byte) end
if Mus_ubyte != 7 then snd_print sprintf("# Mus_ubyte => %d\n", Mus_ubyte) end
if Mus_bfloat != 4 then snd_print sprintf("# Mus_bfloat => %d\n", Mus_bfloat) end
if Mus_lfloat != 12 then snd_print sprintf("# Mus_lfloat => %d\n", Mus_lfloat) end
if Mus_bint != 5 then snd_print sprintf("# Mus_bint => %d\n", Mus_bint) end
if Mus_lint != 11 then snd_print sprintf("# Mus_lint => %d\n", Mus_lint) end
if Mus_bintn != 17 then snd_print sprintf("# Mus_bintn => %d\n", Mus_bintn) end
if Mus_lintn != 18 then snd_print sprintf("# Mus_lintn => %d\n", Mus_lintn) end
if Mus_b24int != 8 then snd_print sprintf("# Mus_b24int => %d\n", Mus_b24int) end
if Mus_l24int != 16 then snd_print sprintf("# Mus_l24int => %d\n", Mus_l24int) end
if Mus_bdouble != 9 then snd_print sprintf("# Mus_bdouble => %d\n", Mus_bdouble) end
if Mus_ldouble != 13 then snd_print sprintf("# Mus_ldouble => %d\n", Mus_ldouble) end
if Mus_ubshort != 14 then snd_print sprintf("# Mus_ubshort => %d\n", Mus_ubshort) end
if Mus_ulshort != 15 then snd_print sprintf("# Mus_ulshort => %d\n", Mus_ulshort) end
if Mus_bfloat_unscaled != 20 then snd_print sprintf("# Mus_bfloat_unscaled => %d\n", Mus_bfloat_unscaled) end
if Mus_lfloat_unscaled != 21 then snd_print sprintf("# Mus_lfloat_unscaled => %d\n", Mus_lfloat_unscaled) end
if Mus_bdouble_unscaled != 22 then snd_print sprintf("# Mus_bdouble_unscaled => %d\n", Mus_bdouble_unscaled) end
if Mus_ldouble_unscaled != 23 then snd_print sprintf("# Mus_ldouble_unscaled => %d\n", Mus_ldouble_unscaled) end
if Mus_audio_default != 0 then snd_print sprintf("# Mus_audio_default => %d\n", Mus_audio_default) end
if Mus_audio_duplex_default != 1 then snd_print sprintf("# Mus_audio_duplex_default => %d\n", Mus_audio_duplex_default) end
if Mus_audio_line_out != 4 then snd_print sprintf("# Mus_audio_line_out => %d\n", Mus_audio_line_out) end
if Mus_audio_line_in != 5 then snd_print sprintf("# Mus_audio_line_in => %d\n", Mus_audio_line_in) end
if Mus_audio_microphone != 6 then snd_print sprintf("# Mus_audio_microphone => %d\n", Mus_audio_microphone) end
if Mus_audio_speakers != 7 then snd_print sprintf("# Mus_audio_speakers => %d\n", Mus_audio_speakers) end
if Mus_audio_dac_out != 10 then snd_print sprintf("# Mus_audio_dac_out => %d\n", Mus_audio_dac_out) end
if Mus_audio_adat_in != 2 then snd_print sprintf("# Mus_audio_adat_in => %d\n", Mus_audio_adat_in) end
if Mus_audio_aes_in != 3 then snd_print sprintf("# Mus_audio_aes_in => %d\n", Mus_audio_aes_in) end
if Mus_audio_digital_in != 8 then snd_print sprintf("# Mus_audio_digital_in => %d\n", Mus_audio_digital_in) end
if Mus_audio_digital_out != 9 then snd_print sprintf("# Mus_audio_digital_out => %d\n", Mus_audio_digital_out) end
if Mus_audio_adat_out != 11 then snd_print sprintf("# Mus_audio_adat_out => %d\n", Mus_audio_adat_out) end
if Mus_audio_aes_out != 12 then snd_print sprintf("# Mus_audio_aes_out => %d\n", Mus_audio_aes_out) end
if Mus_audio_dac_filter != 13 then snd_print sprintf("# Mus_audio_dac_filter => %d\n", Mus_audio_dac_filter) end
if Mus_audio_mixer != 14 then snd_print sprintf("# Mus_audio_mixer => %d\n", Mus_audio_mixer) end
if Mus_audio_line1 != 15 then snd_print sprintf("# Mus_audio_line1 => %d\n", Mus_audio_line1) end
if Mus_audio_line2 != 16 then snd_print sprintf("# Mus_audio_line2 => %d\n", Mus_audio_line2) end
if Mus_audio_line3 != 17 then snd_print sprintf("# Mus_audio_line3 => %d\n", Mus_audio_line3) end
if Mus_audio_aux_input != 18 then snd_print sprintf("# Mus_audio_aux_input => %d\n", Mus_audio_aux_input) end
if Mus_audio_cd != 19 then snd_print sprintf("# Mus_audio_cd => %d\n", Mus_audio_cd) end
if Mus_audio_aux_output != 20 then snd_print sprintf("# Mus_audio_aux_output => %d\n", Mus_audio_aux_output) end
if Mus_audio_spdif_in != 21 then snd_print sprintf("# Mus_audio_spdif_in => %d\n", Mus_audio_spdif_in) end
if Mus_audio_spdif_out != 22 then snd_print sprintf("# Mus_audio_spdif_out => %d\n", Mus_audio_spdif_out) end
if Mus_audio_amp != 23 then snd_print sprintf("# Mus_audio_amp => %d\n", Mus_audio_amp) end
if Mus_audio_srate != 24 then snd_print sprintf("# Mus_audio_srate => %d\n", Mus_audio_srate) end
if Mus_audio_channel != 25 then snd_print sprintf("# Mus_audio_channel => %d\n", Mus_audio_channel) end
if Mus_audio_format != 26 then snd_print sprintf("# Mus_audio_format => %d\n", Mus_audio_format) end
if Mus_audio_port != 37 then snd_print sprintf("# Mus_audio_port => %d\n", Mus_audio_port) end
if Mus_audio_imix != 27 then snd_print sprintf("# Mus_audio_imix => %d\n", Mus_audio_imix) end
if Mus_audio_igain != 28 then snd_print sprintf("# Mus_audio_igain => %d\n", Mus_audio_igain) end
if Mus_audio_reclev != 29 then snd_print sprintf("# Mus_audio_reclev => %d\n", Mus_audio_reclev) end
if Mus_audio_pcm != 30 then snd_print sprintf("# Mus_audio_pcm => %d\n", Mus_audio_pcm) end
if Mus_audio_pcm2 != 31 then snd_print sprintf("# Mus_audio_pcm2 => %d\n", Mus_audio_pcm2) end
if Mus_audio_ogain != 32 then snd_print sprintf("# Mus_audio_ogain => %d\n", Mus_audio_ogain) end
if Mus_audio_line != 33 then snd_print sprintf("# Mus_audio_line => %d\n", Mus_audio_line) end
if Mus_audio_synth != 34 then snd_print sprintf("# Mus_audio_synth => %d\n", Mus_audio_synth) end
if Mus_audio_bass != 35 then snd_print sprintf("# Mus_audio_bass => %d\n", Mus_audio_bass) end
if Mus_audio_treble != 36 then snd_print sprintf("# Mus_audio_treble => %d\n", Mus_audio_treble) end
if Mus_audio_direction != 39 then snd_print sprintf("# Mus_audio_direction => %d\n", Mus_audio_direction) end
if Mus_audio_samples_per_channel != 38 then snd_print sprintf("# Mus_audio_samples_per_channel => %d\n", Mus_audio_samples_per_channel) end


# ---------------------------------------- test 1 ----------------------------------------

if ask_before_overwrite != false then snd_print sprintf("# ask_before_overwrite: %s\n", ask_before_overwrite) end
if audio_output_device != 0 then snd_print sprintf("# audio_output_device: %s\n", audio_output_device) end
if audio_state_file != ".snd-mixer" then snd_print sprintf("# audio_state_file: %s\n", audio_state_file) end
if auto_resize != true then snd_print sprintf("# auto_resize: %s\n", auto_resize) end
if auto_update != false then snd_print sprintf("# auto_update: %s\n", auto_update) end
if channel_style != 0 then snd_print sprintf("# channel_style: %s\n", channel_style) end
if color_cutoff != 0.003 then snd_print sprintf("# color_cutoff: %s\n", color_cutoff) end
if color_inverted != true then snd_print sprintf("# color_inverted: %s\n", color_inverted) end
if color_scale != 1.0 then snd_print sprintf("# color_scale: %s\n", color_scale) end
if colormap != -1 then snd_print sprintf("# colormap: %s\n", colormap) end
if auto_update_interval != 60.0 then snd_print sprintf("# auto_update_interval: %s\n", auto_update_interval) end
if dac_combines_channels != true then snd_print sprintf("# dac_combines_channels: %s\n", dac_combines_channels) end
if emacs_style_save_as != false then snd_print sprintf("# emacs_style_save_as: %s\n", emacs_style_save_as) end
if dac_size != 256 then snd_print sprintf("# dac_size: %s\n", dac_size) end
if minibuffer_history_length != 8 then snd_print sprintf("# minibuffer_history_length: %s\n", minibuffer_history_length) end
if data_clipped != false then snd_print sprintf("# data_clipped: %s\n", data_clipped) end
if default_output_chans != 1 then snd_print sprintf("# default_output_chans: %s\n", default_output_chans) end
if default_output_format != 1 then snd_print sprintf("# default_output_format: %s\n", default_output_format) end
if default_output_srate != 22050 then snd_print sprintf("# default_output_srate: %s\n", default_output_srate) end
if default_output_type != 0 then snd_print sprintf("# default_output_type: %s\n", default_output_type) end
if dot_size != 1 then snd_print sprintf("# dot_size: %s\n", dot_size) end
if enved_base != 1.0 then snd_print sprintf("# enved_base: %s\n", enved_base) end
if enved_clip? != false then snd_print sprintf("# enved_clip?: %s\n", enved_clip?) end
if enved_filter_order != 40 then snd_print sprintf("# enved_filter_order: %s\n", enved_filter_order) end
if enved_filter != true then snd_print sprintf("# enved_filter: %s\n", enved_filter) end
if enved_in_dB != false then snd_print sprintf("# enved_in_dB: %s\n", enved_in_dB) end
if enved_exp? != false then snd_print sprintf("# enved_exp?: %s\n", enved_exp?) end
if enved_power != 3.0 then snd_print sprintf("# enved_power: %s\n", enved_power) end
if enved_target != 0 then snd_print sprintf("# enved_target: %s\n", enved_target) end
if enved_wave? != false then snd_print sprintf("# enved_wave?: %s\n", enved_wave?) end
if enved_active_env != nil then snd_print sprintf("# enved_active_env: %s\n", enved_active_env) end
if enved_selected_env != nil then snd_print sprintf("# enved_selected_env: %s\n", enved_selected_env) end
if eps_file != "snd.eps" then snd_print sprintf("# eps_file: %s\n", eps_file) end
if eps_bottom_margin != 0.0 then snd_print sprintf("# eps_bottom_margin: %s\n", eps_bottom_margin) end
if eps_left_margin != 0.0 then snd_print sprintf("# eps_left_margin: %s\n", eps_left_margin) end
if eps_size != 1.0 then snd_print sprintf("# eps_size: %s\n", eps_size) end
if fft_window_beta != 0.0 then snd_print sprintf("# fft_window_beta: %s\n", fft_window_beta) end
if fft_log_frequency != false then snd_print sprintf("# fft_log_frequency: %s\n", fft_log_frequency) end
if fft_log_magnitude != false then snd_print sprintf("# fft_log_magnitude: %s\n", fft_log_magnitude) end
if transform_size != 256 then snd_print sprintf("# transform_size: %s\n", transform_size) end
if transform_graph_type != 0 then snd_print sprintf("# transform_graph_type: %s\n", transform_graph_type) end
if fft_window != 6 then snd_print sprintf("# fft_window: %s\n", fft_window) end
if filter_env_in_hz != false then snd_print sprintf("# filter_env_in_hz: %s\n", filter_env_in_hz) end
if graph_cursor != 34 then snd_print sprintf("# graph_cursor: %s\n", graph_cursor) end
if graph_style != 0 then snd_print sprintf("# graph_style: %s\n", graph_style) end
if graphs_horizontal != true then snd_print sprintf("# graphs_horizontal: %s\n", graphs_horizontal) end
if hankel_jn != 0.0 then snd_print sprintf("# hankel_jn: %s\n", hankel_jn) end
if just_sounds != false then snd_print sprintf("# just_sounds: %s\n", just_sounds) end
if listener_prompt != ">" then snd_print sprintf("# listener_prompt: %s\n", listener_prompt) end
if max_transform_peaks != 100 then snd_print sprintf("# max_transform_peaks: %s\n", max_transform_peaks) end
if max_regions != 16 then snd_print sprintf("# max_regions: %s\n", max_regions) end
if min_dB != -60.0 then snd_print sprintf("# min_dB: %s\n", min_dB) end
if movies != true then snd_print sprintf("# movies: %s\n", movies) end
if selection_creates_region != true then snd_print sprintf("# selection_creates_region: %s\n", selection_creates_region) end
if transform_normalization != normalize_transform_by_channel then snd_print sprintf("# transform_normalization: %s\n", transform_normalization) end
if previous_files_sort != 0 then snd_print sprintf("# previous_files_sort: %s\n", previous_files_sort) end
if print_length != 12 then snd_print sprintf("# print_length: %s\n", print_length) end
if recorder_autoload != false then snd_print sprintf("# recorder_autoload: %s\n", recorder_autoload) end
if recorder_buffer_size != 4096 then snd_print sprintf("# recorder_buffer_size: %s\n", recorder_buffer_size) end
if recorder_file != false then snd_print sprintf("# recorder_file: %s\n", recorder_file) end
if recorder_max_duration != 1000000.0 then snd_print sprintf("# recorder_max_duration: %s\n", recorder_max_duration) end
if recorder_out_chans != 2 then snd_print sprintf("# recorder_out_chans: %s\n", recorder_out_chans) end
if recorder_srate != 22050 then snd_print sprintf("# recorder_srate: %s\n", recorder_srate) end
if recorder_trigger != 0.0 then snd_print sprintf("# recorder_trigger: %s\n", recorder_trigger) end
if region_graph_style != graph_lines then snd_print sprintf("# region_graph_style: %s\n", region_graph_style) end
if reverb_control_decay != 1.0 then snd_print sprintf("# reverb_control_decay: %s\n", reverb_control_decay) end
if save_state_file != "saved_snd.scm" then snd_print sprintf("# save_state_file: %s\n", save_state_file) end
if show_axes != 1 then snd_print sprintf("# show_axes: %s\n", show_axes) end
if show_transform_peaks != false then snd_print sprintf("# show_transform_peaks: %s\n", show_transform_peaks) end
if show_indices != false then snd_print sprintf("# show_indices: %s\n", show_indices) end
if show_backtrace != false then snd_print sprintf("# show_backtrace: %s\n", show_backtrace) end
if show_marks != true then snd_print sprintf("# show_marks: %s\n", show_marks) end
if show_mix_waveforms != true then snd_print sprintf("# show_mix_waveforms: %s\n", show_mix_waveforms) end
if show_selection_transform != false then snd_print sprintf("# show_selection_transform: %s\n", show_selection_transform) end
if show_usage_stats != false then snd_print sprintf("# show_usage_stats: %s\n", show_usage_stats) end
if show_y_zero != false then snd_print sprintf("# show_y_zero: %s\n", show_y_zero) end
if sinc_width != 10 then snd_print sprintf("# sinc_width: %s\n", sinc_width) end
if spectro_cutoff != 1.0 then snd_print sprintf("# spectro_cutoff: %s\n", spectro_cutoff) end
if spectro_hop != 4 then snd_print sprintf("# spectro_hop: %s\n", spectro_hop) end
if spectro_start != 0.0 then snd_print sprintf("# spectro_start: %s\n", spectro_start) end
if spectro_x_angle != 90.0 then snd_print sprintf("# spectro_x_angle: %s\n", spectro_x_angle) end
if spectro_x_scale != 1.0 then snd_print sprintf("# spectro_x_scale: %s\n", spectro_x_scale) end
if spectro_y_angle != 0.0 then snd_print sprintf("# spectro_y_angle: %s\n", spectro_y_angle) end
if spectro_y_scale != 1.0 then snd_print sprintf("# spectro_y_scale: %s\n", spectro_y_scale) end
if spectro_z_angle != -2.0 then snd_print sprintf("# spectro_z_angle: %s\n", spectro_z_angle) end
if spectro_z_scale != 0.1 then snd_print sprintf("# spectro_z_scale: %s\n", spectro_z_scale) end
if speed_control_style != 0 then snd_print sprintf("# speed_control_style: %s\n", speed_control_style) end
if speed_control_tones != 12 then snd_print sprintf("# speed_control_tones: %s\n", speed_control_tones) end
if temp_dir != false then snd_print sprintf("# temp_dir: %s\n", temp_dir) end
if ladspa_dir != false then snd_print sprintf("# ladspa_dir: %s\n", ladspa_dir) end
if tiny_font != "6x12" then snd_print sprintf("# tiny_font: %s\n", tiny_font) end
if transform_type != 0 then snd_print sprintf("# transform_type: %s\n", transform_type) end
if trap_segfault != false then snd_print sprintf("# trap_segfault: %s\n", trap_segfault) end
if use_sinc_interp != true then snd_print sprintf("# use_sinc_interp: %s\n", use_sinc_interp) end
if verbose_cursor != false then snd_print sprintf("# verbose_cursor: %s\n", verbose_cursor) end
if vu_font != false then snd_print sprintf("# vu_font: %s\n", vu_font) end
if vu_font_size != 1.0 then snd_print sprintf("# vu_font_size: %s\n", vu_font_size) end
if vu_size != 1.0 then snd_print sprintf("# vu_size: %s\n", vu_size) end
if wavelet_type != 0 then snd_print sprintf("# wavelet_type: %s\n", wavelet_type) end
if time_graph_type != graph_time_once then snd_print sprintf("# time_graph_type: %s\n", time_graph_type) end
if wavo_hop != 3 then snd_print sprintf("# wavo_hop: %s\n", wavo_hop) end
if wavo_trace != 64 then snd_print sprintf("# wavo_trace: %s\n", wavo_trace) end
if x_axis_style != 0 then snd_print sprintf("# x_axis_style: %s\n", x_axis_style) end
if beats_per_minute != 60.0 then snd_print sprintf("# beats_per_minute: %s\n", beats_per_minute) end
if zero_pad != 0 then snd_print sprintf("# zero_pad: %s\n", zero_pad) end
if zoom_focus_style != 2 then snd_print sprintf("# zoom_focus_style: %s\n", zoom_focus_style) end
if mix_waveform_height != 20 then snd_print sprintf("# mix_waveform_height: %s\n", mix_waveform_height) end
if mix_tag_width != 6 then snd_print sprintf("# mix_tag_width: %s\n", mix_tag_width) end
if mix_tag_height != 14 then snd_print sprintf("# mix_tag_height: %s\n", mix_tag_height) end
if audio_output_device != 0 then snd_print sprintf("# audio_output_device: %s\n", audio_output_device) end
if selected_mix != -1 then snd_print sprintf("# selected_mix: %s\n", selected_mix) end

