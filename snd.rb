# snd.rb: Snd Ruby code and tests
#
# TODO: add examp.rb and bess.rb

def provided?(feature) feature == $".find{|obj| obj == feature} end
# not sure this is kosher in Ruby -- I'm using $" via rb_provided as a feature list, but doc says its a filename array

def snd_display(str) 
  snd_print str
  $stderr.write str
end

def fneq(a, b)
  (a - b).abs > .001
end

def ffneq(a, b)
  (a - b).abs > .01
end

def vequal(a, b)
  alen = a.length
  blen = b.length
  happy = (alen == blen)
  if happy then
    0.upto(alen - 1) do |i|
      if fneq(a[i], b[i]) then happy = false end
      break if !happy
    end
  end
  happy
end


# snd-test.scm translations
# ---------------------------------------- test 0 ----------------------------------------

if Enved_amplitude != 0 then snd_display sprintf("\n# Enved_amplitude => %d", Enved_amplitude) end
if Autocorrelation != 3 then snd_display sprintf("\n# Autocorrelation => %d", Autocorrelation) end
if Bartlett_window != 4 then snd_display sprintf("\n# Bartlett_window => %d", Bartlett_window) end
if Blackman2_window != 6 then snd_display sprintf("\n# Blackman2_window => %d", Blackman2_window) end
if Blackman3_window != 7 then snd_display sprintf("\n# Blackman3_window => %d", Blackman3_window) end
if Blackman4_window != 8 then snd_display sprintf("\n# Blackman4_window => %d", Blackman4_window) end
if Cauchy_window != 12 then snd_display sprintf("\n# Cauchy_window => %d", Cauchy_window) end
if Channels_combined != 1 then snd_display sprintf("\n# Channels_combined => %d", Channels_combined) end
if Channels_separate != 0 then snd_display sprintf("\n# Channels_separate => %d", Channels_separate) end
if Channels_superimposed != 2 then snd_display sprintf("\n# Channels_superimposed => %d", Channels_superimposed) end
if Chebyshev_transform != 4 then snd_display sprintf("\n# Chebyshev_transform => %d", Chebyshev_transform) end
if Cursor_in_middle != 3 then snd_display sprintf("\n# Cursor_in_middle => %d", Cursor_in_middle) end
if Cursor_in_view != 0 then snd_display sprintf("\n# Cursor_in_view => %d", Cursor_in_view) end
if Cursor_on_left != 1 then snd_display sprintf("\n# Cursor_on_left => %d", Cursor_on_left) end
if Cursor_on_right != 2 then snd_display sprintf("\n# Cursor_on_right => %d", Cursor_on_right) end
if Dolph_chebyshev_window != 16 then snd_display sprintf("\n# Dolph_chebyshev_window => %d", Dolph_chebyshev_window) end
if Exponential_window != 9 then snd_display sprintf("\n# Exponential_window => %d", Exponential_window) end
if Zoom_focus_active != 2 then snd_display sprintf("\n# Zoom_focus_active => %d", Zoom_focus_active) end
if Zoom_focus_left != 0 then snd_display sprintf("\n# Zoom_focus_left => %d", Zoom_focus_left) end
if Zoom_focus_middle != 3 then snd_display sprintf("\n# Zoom_focus_middle => %d", Zoom_focus_middle) end
if Zoom_focus_right != 1 then snd_display sprintf("\n# Zoom_focus_right => %d", Zoom_focus_right) end
if Fourier_transform != 0 then snd_display sprintf("\n# Fourier_transform => %d", Fourier_transform) end
if Gaussian_window != 14 then snd_display sprintf("\n# Gaussian_window => %d", Gaussian_window) end
if Graph_dots != 1 then snd_display sprintf("\n# Graph_dots => %d", Graph_dots) end
if Graph_dots_and_lines != 3 then snd_display sprintf("\n# Graph_dots_and_lines => %d", Graph_dots_and_lines) end
if Graph_filled != 2 then snd_display sprintf("\n# Graph_filled => %d", Graph_filled) end
if Graph_lines != 0 then snd_display sprintf("\n# Graph_lines => %d", Graph_lines) end
if Graph_lollipops != 4 then snd_display sprintf("\n# Graph_lollipops => %d", Graph_lollipops) end
if Hadamard_transform != 6 then snd_display sprintf("\n# Hadamard_transform => %d", Hadamard_transform) end
if Haar_transform != 7 then snd_display sprintf("\n# Haar_transform => %d", Haar_transform) end
if Hamming_window != 5 then snd_display sprintf("\n# Hamming_window => %d", Hamming_window) end
if Hankel_transform != 8 then snd_display sprintf("\n# Hankel_transform => %d", Hankel_transform) end
if Hanning_window != 1 then snd_display sprintf("\n# Hanning_window => %d", Hanning_window) end
if Kaiser_window != 11 then snd_display sprintf("\n# Kaiser_window => %d", Kaiser_window) end
if Keyboard_no_action != 4 then snd_display sprintf("\n# Keyboard_no_action => %d", Keyboard_no_action) end
if Cepstrum != 5 then snd_display sprintf("\n# Cepstrum => %d", Cepstrum) end
if Parzen_window != 3 then snd_display sprintf("\n# Parzen_window => %d", Parzen_window) end
if Poisson_window != 13 then snd_display sprintf("\n# Poisson_window => %d", Poisson_window) end
if Rectangular_window != 0 then snd_display sprintf("\n# Rectangular_window => %d", Rectangular_window) end
if Riemann_window != 10 then snd_display sprintf("\n# Riemann_window => %d", Riemann_window) end
if Graph_as_sonogram != 1 then snd_display sprintf("\n# Graph_as_sonogram => %d", Graph_as_sonogram) end
if Graph_as_spectrogram != 2 then snd_display sprintf("\n# Graph_as_spectrogram => %d", Graph_as_spectrogram) end
if Graph_once != 0 then snd_display sprintf("\n# Graph_once => %d", Graph_once) end
if Graph_as_wavogram != 3 then snd_display sprintf("\n# Graph_as_wavogram => %d", Graph_as_wavogram) end
if Enved_spectrum != 1 then snd_display sprintf("\n# Enved_spectrum => %d", Enved_spectrum) end
if Speed_control_as_float != 0 then snd_display sprintf("\n# Speed_control_as_float => %d", Speed_control_as_float) end
if Speed_control_as_ratio != 1 then snd_display sprintf("\n# Speed_control_as_ratio => %d", Speed_control_as_ratio) end
if Speed_control_as_semitone != 2 then snd_display sprintf("\n# Speed_control_as_semitone => %d", Speed_control_as_semitone) end
if Enved_srate != 2 then snd_display sprintf("\n# Enved_srate => %d", Enved_srate) end
if Tukey_window != 15 then snd_display sprintf("\n# Tukey_window => %d", Tukey_window) end
if Walsh_transform != 2 then snd_display sprintf("\n# Walsh_transform => %d", Walsh_transform) end
if Wavelet_transform != 1 then snd_display sprintf("\n# Wavelet_transform => %d", Wavelet_transform) end
if Welch_window != 2 then snd_display sprintf("\n# Welch_window => %d", Welch_window) end
if Cursor_cross != 0 then snd_display sprintf("\n# Cursor_cross => %d", Cursor_cross) end
if Cursor_line != 1 then snd_display sprintf("\n# Cursor_line => %d", Cursor_line) end
if Dont_normalize != 0 then snd_display sprintf("\n# Dont_normalize => %d", Dont_normalize) end
if Normalize_by_channel != 1 then snd_display sprintf("\n# Normalize_by_channel => %d", Normalize_by_channel) end
if Normalize_by_sound != 2 then snd_display sprintf("\n# Normalize_by_sound => %d", Normalize_by_sound) end
if Normalize_globally != 3 then snd_display sprintf("\n# Normalize_globally => %d", Normalize_globally) end
if X_axis_in_samples != 1 then snd_display sprintf("\n# X_axis_in_samples => %d", X_axis_in_samples) end
if X_axis_in_beats != 4 then snd_display sprintf("\n# X_axis_in_beats => %d", X_axis_in_beats) end
if X_axis_in_seconds != 0 then snd_display sprintf("\n# X_axis_in_seconds => %d", X_axis_in_seconds) end
if X_axis_as_percentage != 2 then snd_display sprintf("\n# X_axis_as_percentage => %d", X_axis_as_percentage) end
if Enved_add_point != 0 then snd_display sprintf("\n# Enved_add_point => %d", Enved_add_point) end
if Enved_delete_point != 1 then snd_display sprintf("\n# Enved_delete_point => %d", Enved_delete_point) end
if Enved_move_point != 2 then snd_display sprintf("\n# Enved_move_point => %d", Enved_move_point) end
if Time_graph != 0 then snd_display sprintf("\n# Time_graph => %d", Time_graph) end
if Transform_graph != 1 then snd_display sprintf("\n# Transform_graph => %d", Transform_graph) end
if Lisp_graph != 2 then snd_display sprintf("\n# Lisp_graph => %d", Lisp_graph) end
if Copy_context != 0 then snd_display sprintf("\n# Copy_context => %d", Copy_context) end
if Cursor_context != 3 then snd_display sprintf("\n# Cursor_context => %d", Cursor_context) end
if Selection_context != 2 then snd_display sprintf("\n# Selection_context => %d", Selection_context) end
if Mus_next != 0 then snd_display sprintf("\n# Mus_next => %d", Mus_next) end
if Mus_aifc != 1 then snd_display sprintf("\n# Mus_aifc => %d", Mus_aifc) end
if Mus_riff != 2 then snd_display sprintf("\n# Mus_riff => %d", Mus_riff) end
if Mus_nist != 4 then snd_display sprintf("\n# Mus_nist => %d", Mus_nist) end
if Mus_raw != 10 then snd_display sprintf("\n# Mus_raw => %d", Mus_raw) end
if Mus_ircam != 14 then snd_display sprintf("\n# Mus_ircam => %d", Mus_ircam) end
if Mus_aiff != 56 then snd_display sprintf("\n# Mus_aiff => %d", Mus_aiff) end
if Mus_bicsf != 3 then snd_display sprintf("\n# Mus_bicsf => %d", Mus_bicsf) end
if Mus_voc != 8 then snd_display sprintf("\n# Mus_voc => %d", Mus_voc) end
if Mus_svx != 7 then snd_display sprintf("\n# Mus_svx => %d", Mus_svx) end
if Mus_soundfont != 31 then snd_display sprintf("\n# Mus_soundfont => %d", Mus_soundfont) end
if Mus_bshort != 1 then snd_display sprintf("\n# Mus_bshort => %d", Mus_bshort) end
if Mus_lshort != 10 then snd_display sprintf("\n# Mus_lshort => %d", Mus_lshort) end
if Mus_mulaw != 2 then snd_display sprintf("\n# Mus_mulaw => %d", Mus_mulaw) end
if Mus_alaw != 6 then snd_display sprintf("\n# Mus_alaw => %d", Mus_alaw) end
if Mus_byte != 3 then snd_display sprintf("\n# Mus_byte => %d", Mus_byte) end
if Mus_ubyte != 7 then snd_display sprintf("\n# Mus_ubyte => %d", Mus_ubyte) end
if Mus_bfloat != 4 then snd_display sprintf("\n# Mus_bfloat => %d", Mus_bfloat) end
if Mus_lfloat != 12 then snd_display sprintf("\n# Mus_lfloat => %d", Mus_lfloat) end
if Mus_bint != 5 then snd_display sprintf("\n# Mus_bint => %d", Mus_bint) end
if Mus_lint != 11 then snd_display sprintf("\n# Mus_lint => %d", Mus_lint) end
if Mus_bintn != 17 then snd_display sprintf("\n# Mus_bintn => %d", Mus_bintn) end
if Mus_lintn != 18 then snd_display sprintf("\n# Mus_lintn => %d", Mus_lintn) end
if Mus_b24int != 8 then snd_display sprintf("\n# Mus_b24int => %d", Mus_b24int) end
if Mus_l24int != 16 then snd_display sprintf("\n# Mus_l24int => %d", Mus_l24int) end
if Mus_bdouble != 9 then snd_display sprintf("\n# Mus_bdouble => %d", Mus_bdouble) end
if Mus_ldouble != 13 then snd_display sprintf("\n# Mus_ldouble => %d", Mus_ldouble) end
if Mus_ubshort != 14 then snd_display sprintf("\n# Mus_ubshort => %d", Mus_ubshort) end
if Mus_ulshort != 15 then snd_display sprintf("\n# Mus_ulshort => %d", Mus_ulshort) end
if Mus_bfloat_unscaled != 20 then snd_display sprintf("\n# Mus_bfloat_unscaled => %d", Mus_bfloat_unscaled) end
if Mus_lfloat_unscaled != 21 then snd_display sprintf("\n# Mus_lfloat_unscaled => %d", Mus_lfloat_unscaled) end
if Mus_bdouble_unscaled != 22 then snd_display sprintf("\n# Mus_bdouble_unscaled => %d", Mus_bdouble_unscaled) end
if Mus_ldouble_unscaled != 23 then snd_display sprintf("\n# Mus_ldouble_unscaled => %d", Mus_ldouble_unscaled) end
if Mus_audio_default != 0 then snd_display sprintf("\n# Mus_audio_default => %d", Mus_audio_default) end
if Mus_audio_duplex_default != 1 then snd_display sprintf("\n# Mus_audio_duplex_default => %d", Mus_audio_duplex_default) end
if Mus_audio_line_out != 4 then snd_display sprintf("\n# Mus_audio_line_out => %d", Mus_audio_line_out) end
if Mus_audio_line_in != 5 then snd_display sprintf("\n# Mus_audio_line_in => %d", Mus_audio_line_in) end
if Mus_audio_microphone != 6 then snd_display sprintf("\n# Mus_audio_microphone => %d", Mus_audio_microphone) end
if Mus_audio_speakers != 7 then snd_display sprintf("\n# Mus_audio_speakers => %d", Mus_audio_speakers) end
if Mus_audio_dac_out != 10 then snd_display sprintf("\n# Mus_audio_dac_out => %d", Mus_audio_dac_out) end
if Mus_audio_adat_in != 2 then snd_display sprintf("\n# Mus_audio_adat_in => %d", Mus_audio_adat_in) end
if Mus_audio_aes_in != 3 then snd_display sprintf("\n# Mus_audio_aes_in => %d", Mus_audio_aes_in) end
if Mus_audio_digital_in != 8 then snd_display sprintf("\n# Mus_audio_digital_in => %d", Mus_audio_digital_in) end
if Mus_audio_digital_out != 9 then snd_display sprintf("\n# Mus_audio_digital_out => %d", Mus_audio_digital_out) end
if Mus_audio_adat_out != 11 then snd_display sprintf("\n# Mus_audio_adat_out => %d", Mus_audio_adat_out) end
if Mus_audio_aes_out != 12 then snd_display sprintf("\n# Mus_audio_aes_out => %d", Mus_audio_aes_out) end
if Mus_audio_dac_filter != 13 then snd_display sprintf("\n# Mus_audio_dac_filter => %d", Mus_audio_dac_filter) end
if Mus_audio_mixer != 14 then snd_display sprintf("\n# Mus_audio_mixer => %d", Mus_audio_mixer) end
if Mus_audio_line1 != 15 then snd_display sprintf("\n# Mus_audio_line1 => %d", Mus_audio_line1) end
if Mus_audio_line2 != 16 then snd_display sprintf("\n# Mus_audio_line2 => %d", Mus_audio_line2) end
if Mus_audio_line3 != 17 then snd_display sprintf("\n# Mus_audio_line3 => %d", Mus_audio_line3) end
if Mus_audio_aux_input != 18 then snd_display sprintf("\n# Mus_audio_aux_input => %d", Mus_audio_aux_input) end
if Mus_audio_cd != 19 then snd_display sprintf("\n# Mus_audio_cd => %d", Mus_audio_cd) end
if Mus_audio_aux_output != 20 then snd_display sprintf("\n# Mus_audio_aux_output => %d", Mus_audio_aux_output) end
if Mus_audio_spdif_in != 21 then snd_display sprintf("\n# Mus_audio_spdif_in => %d", Mus_audio_spdif_in) end
if Mus_audio_spdif_out != 22 then snd_display sprintf("\n# Mus_audio_spdif_out => %d", Mus_audio_spdif_out) end
if Mus_audio_amp != 23 then snd_display sprintf("\n# Mus_audio_amp => %d", Mus_audio_amp) end
if Mus_audio_srate != 24 then snd_display sprintf("\n# Mus_audio_srate => %d", Mus_audio_srate) end
if Mus_audio_channel != 25 then snd_display sprintf("\n# Mus_audio_channel => %d", Mus_audio_channel) end
if Mus_audio_format != 26 then snd_display sprintf("\n# Mus_audio_format => %d", Mus_audio_format) end
if Mus_audio_port != 37 then snd_display sprintf("\n# Mus_audio_port => %d", Mus_audio_port) end
if Mus_audio_imix != 27 then snd_display sprintf("\n# Mus_audio_imix => %d", Mus_audio_imix) end
if Mus_audio_igain != 28 then snd_display sprintf("\n# Mus_audio_igain => %d", Mus_audio_igain) end
if Mus_audio_reclev != 29 then snd_display sprintf("\n# Mus_audio_reclev => %d", Mus_audio_reclev) end
if Mus_audio_pcm != 30 then snd_display sprintf("\n# Mus_audio_pcm => %d", Mus_audio_pcm) end
if Mus_audio_pcm2 != 31 then snd_display sprintf("\n# Mus_audio_pcm2 => %d", Mus_audio_pcm2) end
if Mus_audio_ogain != 32 then snd_display sprintf("\n# Mus_audio_ogain => %d", Mus_audio_ogain) end
if Mus_audio_line != 33 then snd_display sprintf("\n# Mus_audio_line => %d", Mus_audio_line) end
if Mus_audio_synth != 34 then snd_display sprintf("\n# Mus_audio_synth => %d", Mus_audio_synth) end
if Mus_audio_bass != 35 then snd_display sprintf("\n# Mus_audio_bass => %d", Mus_audio_bass) end
if Mus_audio_treble != 36 then snd_display sprintf("\n# Mus_audio_treble => %d", Mus_audio_treble) end
if Mus_audio_direction != 39 then snd_display sprintf("\n# Mus_audio_direction => %d", Mus_audio_direction) end
if Mus_audio_samples_per_channel != 38 then snd_display sprintf("\n# Mus_audio_samples_per_channel => %d", Mus_audio_samples_per_channel) end


# ---------------------------------------- test 1 ----------------------------------------

if ask_before_overwrite != false then snd_display sprintf("\n# ask_before_overwrite: %s", ask_before_overwrite) end
if audio_output_device != 0 then snd_display sprintf("\n# audio_output_device: %s", audio_output_device) end
if audio_state_file != ".snd-mixer" then snd_display sprintf("\n# audio_state_file: %s", audio_state_file) end
if auto_resize != true then snd_display sprintf("\n# auto_resize: %s", auto_resize) end
if auto_update != false then snd_display sprintf("\n# auto_update: %s", auto_update) end
if channel_style != 0 then snd_display sprintf("\n# channel_style: %s", channel_style) end
if fneq(color_cutoff, 0.003) then snd_display sprintf("\n# color_cutoff: %s", color_cutoff) end
if color_inverted != true then snd_display sprintf("\n# color_inverted: %s", color_inverted) end
if color_scale != 1.0 then snd_display sprintf("\n# color_scale: %s", color_scale) end
if colormap != -1 then snd_display sprintf("\n# colormap: %s", colormap) end
if auto_update_interval != 60.0 then snd_display sprintf("\n# auto_update_interval: %s", auto_update_interval) end
if dac_combines_channels != true then snd_display sprintf("\n# dac_combines_channels: %s", dac_combines_channels) end
if emacs_style_save_as != false then snd_display sprintf("\n# emacs_style_save_as: %s", emacs_style_save_as) end
if dac_size != 256 then snd_display sprintf("\n# dac_size: %s", dac_size) end
if minibuffer_history_length != 8 then snd_display sprintf("\n# minibuffer_history_length: %s", minibuffer_history_length) end
if data_clipped != false then snd_display sprintf("\n# data_clipped: %s", data_clipped) end
if default_output_chans != 1 then snd_display sprintf("\n# default_output_chans: %s", default_output_chans) end
if default_output_format != 1 then snd_display sprintf("\n# default_output_format: %s", default_output_format) end
if default_output_srate != 22050 then snd_display sprintf("\n# default_output_srate: %s", default_output_srate) end
if default_output_type != 0 then snd_display sprintf("\n# default_output_type: %s", default_output_type) end
if dot_size != 1 then snd_display sprintf("\n# dot_size: %s", dot_size) end
if enved_base != 1.0 then snd_display sprintf("\n# enved_base: %s", enved_base) end
if enved_clip? != false then snd_display sprintf("\n# enved_clip?: %s", enved_clip?) end
if enved_filter_order != 40 then snd_display sprintf("\n# enved_filter_order: %s", enved_filter_order) end
if enved_filter != true then snd_display sprintf("\n# enved_filter: %s", enved_filter) end
if enved_in_dB != false then snd_display sprintf("\n# enved_in_dB: %s", enved_in_dB) end
if enved_exp? != false then snd_display sprintf("\n# enved_exp?: %s", enved_exp?) end
if enved_power != 3.0 then snd_display sprintf("\n# enved_power: %s", enved_power) end
if enved_target != 0 then snd_display sprintf("\n# enved_target: %s", enved_target) end
if enved_wave? != false then snd_display sprintf("\n# enved_wave?: %s", enved_wave?) end
if enved_active_env != nil then snd_display sprintf("\n# enved_active_env: %s", enved_active_env) end
if enved_selected_env != nil then snd_display sprintf("\n# enved_selected_env: %s", enved_selected_env) end
if eps_file != "snd.eps" then snd_display sprintf("\n# eps_file: %s", eps_file) end
if eps_bottom_margin != 0.0 then snd_display sprintf("\n# eps_bottom_margin: %s", eps_bottom_margin) end
if eps_left_margin != 0.0 then snd_display sprintf("\n# eps_left_margin: %s", eps_left_margin) end
if eps_size != 1.0 then snd_display sprintf("\n# eps_size: %s", eps_size) end
if fft_window_beta != 0.0 then snd_display sprintf("\n# fft_window_beta: %s", fft_window_beta) end
if fft_log_frequency != false then snd_display sprintf("\n# fft_log_frequency: %s", fft_log_frequency) end
if fft_log_magnitude != false then snd_display sprintf("\n# fft_log_magnitude: %s", fft_log_magnitude) end
if transform_size != 256 then snd_display sprintf("\n# transform_size: %s", transform_size) end
if transform_graph_type != 0 then snd_display sprintf("\n# transform_graph_type: %s", transform_graph_type) end
if fft_window != 6 then snd_display sprintf("\n# fft_window: %s", fft_window) end
if filter_env_in_hz != false then snd_display sprintf("\n# filter_env_in_hz: %s", filter_env_in_hz) end
if graph_cursor != 34 then snd_display sprintf("\n# graph_cursor: %s", graph_cursor) end
if graph_style != 0 then snd_display sprintf("\n# graph_style: %s", graph_style) end
if graphs_horizontal != true then snd_display sprintf("\n# graphs_horizontal: %s", graphs_horizontal) end
if hankel_jn != 0.0 then snd_display sprintf("\n# hankel_jn: %s", hankel_jn) end
if just_sounds != false then snd_display sprintf("\n# just_sounds: %s", just_sounds) end
if listener_prompt != ">" then snd_display sprintf("\n# listener_prompt: %s", listener_prompt) end
if max_transform_peaks != 100 then snd_display sprintf("\n# max_transform_peaks: %s", max_transform_peaks) end
if max_regions != 16 then snd_display sprintf("\n# max_regions: %s", max_regions) end
if min_dB != -60.0 then snd_display sprintf("\n# min_dB: %s", min_dB) end
if selection_creates_region != true then snd_display sprintf("\n# selection_creates_region: %s", selection_creates_region) end
if transform_normalization != Normalize_by_channel then snd_display sprintf("\n# transform_normalization: %s", transform_normalization) end
if previous_files_sort != 0 then snd_display sprintf("\n# previous_files_sort: %s", previous_files_sort) end
if print_length != 12 then snd_display sprintf("\n# print_length: %s", print_length) end
if recorder_autoload != false then snd_display sprintf("\n# recorder_autoload: %s", recorder_autoload) end
if recorder_buffer_size != 4096 then snd_display sprintf("\n# recorder_buffer_size: %s", recorder_buffer_size) end
if recorder_file != "" then snd_display sprintf("\n# recorder_file: %s", recorder_file) end
if recorder_max_duration != 1000000.0 then snd_display sprintf("\n# recorder_max_duration: %s", recorder_max_duration) end
if recorder_out_chans != 2 then snd_display sprintf("\n# recorder_out_chans: %s", recorder_out_chans) end
if recorder_srate != 22050 then snd_display sprintf("\n# recorder_srate: %s", recorder_srate) end
if recorder_trigger != 0.0 then snd_display sprintf("\n# recorder_trigger: %s", recorder_trigger) end
if region_graph_style != Graph_lines then snd_display sprintf("\n# region_graph_style: %s", region_graph_style) end
if reverb_control_decay != 1.0 then snd_display sprintf("\n# reverb_control_decay: %s", reverb_control_decay) end
if save_state_file != "saved-snd.rb" then snd_display sprintf("\n# save_state_file: %s", save_state_file) end
if show_axes != 1 then snd_display sprintf("\n# show_axes: %s", show_axes) end
if show_transform_peaks != false then snd_display sprintf("\n# show_transform_peaks: %s", show_transform_peaks) end
if show_indices != false then snd_display sprintf("\n# show_indices: %s", show_indices) end
if show_backtrace != false then snd_display sprintf("\n# show_backtrace: %s", show_backtrace) end
if show_marks != true then snd_display sprintf("\n# show_marks: %s", show_marks) end
if show_mix_waveforms != true then snd_display sprintf("\n# show_mix_waveforms: %s", show_mix_waveforms) end
if show_selection_transform != false then snd_display sprintf("\n# show_selection_transform: %s", show_selection_transform) end
if show_y_zero != false then snd_display sprintf("\n# show_y_zero: %s", show_y_zero) end
if sinc_width != 10 then snd_display sprintf("\n# sinc_width: %s", sinc_width) end
if spectro_cutoff != 1.0 then snd_display sprintf("\n# spectro_cutoff: %s", spectro_cutoff) end
if spectro_hop != 4 then snd_display sprintf("\n# spectro_hop: %s", spectro_hop) end
if spectro_start != 0.0 then snd_display sprintf("\n# spectro_start: %s", spectro_start) end
if spectro_x_angle != 90.0 then snd_display sprintf("\n# spectro_x_angle: %s", spectro_x_angle) end
if spectro_x_scale != 1.0 then snd_display sprintf("\n# spectro_x_scale: %s", spectro_x_scale) end
if spectro_y_angle != 0.0 then snd_display sprintf("\n# spectro_y_angle: %s", spectro_y_angle) end
if spectro_y_scale != 1.0 then snd_display sprintf("\n# spectro_y_scale: %s", spectro_y_scale) end
if spectro_z_angle != 358.0 then snd_display sprintf("\n# spectro_z_angle: %s", spectro_z_angle) end
if fneq(spectro_z_scale, 0.1) then snd_display sprintf("\n# spectro_z_scale: %s", spectro_z_scale) end
if speed_control_style != 0 then snd_display sprintf("\n# speed_control_style: %s", speed_control_style) end
if speed_control_tones != 12 then snd_display sprintf("\n# speed_control_tones: %s", speed_control_tones) end
if temp_dir != "" then snd_display sprintf("\n# temp_dir: %s", temp_dir) end
if ladspa_dir != "" then snd_display sprintf("\n# ladspa_dir: %s", ladspa_dir) end
if tiny_font != "6x12" then snd_display sprintf("\n# tiny_font: %s", tiny_font) end
if transform_type != 0 then snd_display sprintf("\n# transform_type: %s", transform_type) end
if trap_segfault != false then snd_display sprintf("\n# trap_segfault: %s", trap_segfault) end
if use_sinc_interp != true then snd_display sprintf("\n# use_sinc_interp: %s", use_sinc_interp) end
if verbose_cursor != false then snd_display sprintf("\n# verbose_cursor: %s", verbose_cursor) end
if vu_font != "" then snd_display sprintf("\n# vu_font: %s", vu_font) end
if vu_font_size != 1.0 then snd_display sprintf("\n# vu_font_size: %s", vu_font_size) end
if vu_size != 1.0 then snd_display sprintf("\n# vu_size: %s", vu_size) end
if wavelet_type != 0 then snd_display sprintf("\n# wavelet_type: %s", wavelet_type) end
if time_graph_type != Graph_once then snd_display sprintf("\n# time_graph_type: %s", time_graph_type) end
if wavo_hop != 3 then snd_display sprintf("\n# wavo_hop: %s", wavo_hop) end
if wavo_trace != 64 then snd_display sprintf("\n# wavo_trace: %s", wavo_trace) end
if x_axis_style != 0 then snd_display sprintf("\n# x_axis_style: %s", x_axis_style) end
if beats_per_minute != 60.0 then snd_display sprintf("\n# beats_per_minute 60: %s", beats_per_minute) end
if zero_pad != 0 then snd_display sprintf("\n# zero_pad: %s", zero_pad) end
if zoom_focus_style != 2 then snd_display sprintf("\n# zoom_focus_style: %s", zoom_focus_style) end
if mix_waveform_height != 20 then snd_display sprintf("\n# mix_waveform_height: %s", mix_waveform_height) end
if mix_tag_width != 6 then snd_display sprintf("\n# mix_tag_width: %s", mix_tag_width) end
if mix_tag_height != 14 then snd_display sprintf("\n# mix_tag_height: %s", mix_tag_height) end
if audio_output_device != 0 then snd_display sprintf("\n# audio_output_device: %s", audio_output_device) end
if selected_mix != false then snd_display sprintf("\n# selected_mix: %s", selected_mix) end

# ---------------------------------------- test 2 ----------------------------------------

def test_headers(name, chns, sr, dur, typ, frm)
  file = "/home/bil/sf1/" << name
  if mus_sound_chans(file) != chns then snd_display sprintf("\n# %s: chans: %d %d", name, chns, mus_sound_chans(file)) end
  if mus_sound_srate(file) != sr then snd_display sprintf("\n# %s: srate: %d %d", name, sr, mus_sound_srate(file)) end
  if fneq(mus_sound_duration(file), dur) then snd_display sprintf("\n# %s: duration: %f %f", name, dur, mus_sound_duration(file)) end
  if (mus_sound_data_format(file) != -1) and
     (mus_sound_header_type(file) != 33) and
     ((mus_sound_length(file) + 1) < (mus_sound_datum_size(file) * mus_sound_chans(file) * mus_sound_duration(file) * mus_sound_srate(file)))
    then snd_display sprintf("\n# %s: length: %d (%d: %d * %d * %f * %d)", 
			   name,
			   mus_sound_length(file),
			   mus_sound_datum_size(file) * mus_sound_chans(file) * mus_sound_duration(file) * mus_sound_srate(file),
			   mus_sound_datum_size(file), mus_sound_chans(file), mus_sound_duration(file), mus_sound_srate(file))
  end
  if ((mus_sound_frames(file) - (mus_sound_samples(file) / mus_sound_chans(file))) > 1)
    then snd_display sprintf("\n# %s: frames: %d %d",
		      name,
                      mus_sound_frames(file),
		      mus_sound_samples(file) / mus_sound_chans(file))
  end

  if (mus_header_type_name(mus_sound_header_type(file)) != typ)
    then snd_display sprintf("\n# %s: type: %s %s",
		   	   name,
			   mus_header_type_name(mus_sound_header_type(file)),
			   typ)
  end
  if (mus_data_format_name(mus_sound_data_format(file)) != frm)
    then snd_display sprintf("\n# %s: format: [%s] [%s]",
    	 	    	   file,
			   mus_data_format_name(mus_sound_data_format(file)),
			   frm)
  end
end

test_headers("8svx-8.snd", 1, 22050, 1.88766443729401, "SVX8", "signed byte (8 bits)")
test_headers("Fnonull.aif", 1, 8000, 0.00112499995157123, "AIFC", "mulaw (8 bits)")
test_headers("Pmiscck.aif", 1, 8000, 0.00112499995157123, "AIFC", "mulaw (8 bits)")
test_headers("Pmiscck.wav", 1, 8000, 0.00112499995157123, "RIFF", "mulaw (8 bits)")
test_headers("Pnossnd.aif", 1, 8000, 0.0, "AIFC", "mulaw (8 bits)")
test_headers("Poffset.aif", 1, 8000, 0.00112499995157123, "AIFC", "mulaw (8 bits)")
test_headers("Porder.aif", 1, 8000, 0.00112499995157123, "AIFC", "mulaw (8 bits)")
test_headers("Ptjunk.aif", 1, 8000, 0.00112499995157123, "AIFC", "mulaw (8 bits)")
test_headers("Ptjunk.wav", 1, 8000, 0.00112499995157123, "RIFF", "mulaw (8 bits)")
test_headers("SINE24-S.WAV", 2, 44100, 2.0, "RIFF", "little endian int (24 bits)")
test_headers("a1.asf", 1, 16000, 0.0, "asf", "unsupported")
test_headers("a2.asf", 1, 8000, 0.0, "asf", "unsupported")
test_headers("addf8.afsp", 1, 8000, 2.9760000705719, "Sun", "big endian short (16 bits)")
test_headers("addf8.d", 1, 8000, 2.9760000705719, "SPPACK", "big endian short (16 bits)")
test_headers("addf8.dwd", 1, 8000, 2.9760000705719, "DiamondWare", "little endian short (16 bits)")
test_headers("addf8.nh", 2, 44100, 0.269931972026825, "raw (no header)", "big endian short (16 bits)")
test_headers("addf8.sd", 1, 8000, 2.9760000705719, "ESPS", "big endian short (16 bits)")
test_headers("addf8.sf_mipseb", 1, 8000, 2.9760000705719, "IRCAM", "big endian short (16 bits)")
test_headers("addf8.sf_sun", 1, 8000, 2.9760000705719, "IRCAM", "big endian short (16 bits)")
test_headers("addf8.sf_vax_b", 1, 8000, 2.9760000705719, "IRCAM", "big endian short (16 bits)")
test_headers("addf8.wav", 1, 8000, 2.9760000705719, "RIFF", "little endian short (16 bits)")
test_headers("aebass.krz", 1, 44100, 3.0, "Kurzweil 2000", "big endian short (16 bits)")
test_headers("aiff-16.snd", 2, 44100, 0.746666669845581, "AIFF", "big endian short (16 bits)")
test_headers("aiff-8.snd", 2, 44100, 0.746666669845581, "AIFF", "signed byte (8 bits)")
test_headers("alaw.aifc", 1, 44100, 0.0367800444364548, "AIFC", "alaw (8 bits)")
test_headers("alaw.wav", 1, 11025, 8.70666694641113, "RIFF", "alaw (8 bits)")
test_headers("astor_basia.mp2", 2, 44100, 1.02179133892059, "raw (no header)", "big endian short (16 bits)")
test_headers("c.asf", 1, 8000, 0.0, "asf", "unsupported")
test_headers("ce-c3.w02", 1, 33000, 3.88848495483398, "TX-16W", "unsupported")
test_headers("ce-c4.w03", 1, 33000, 2.91618180274963, "TX-16W", "unsupported")
test_headers("ce-d2.w01", 1, 33000, 3.46439385414124, "TX-16W", "unsupported")
test_headers("clbonef.wav", 1, 22050, 2.57832193374634, "RIFF", "little endian float (32 bits)")
test_headers("cranker.krz", 1, 44100, 3.48267579078674, "Kurzweil 2000", "big endian short (16 bits)")
test_headers("d40130.aif", 1, 10000, 0.100000001490116, "AIFF", "big endian short (16 bits)")
test_headers("d40130.au", 1, 10000, 0.100000001490116, "Sun", "big endian short (16 bits)")
test_headers("d40130.dsf", 1, 8000, 0.125, "Delusion", "little endian short (16 bits)")
test_headers("d40130.fsm", 1, 8000, 0.125249996781349, "Farandole", "little endian short (16 bits)")
test_headers("d40130.iff", 1, 10000, 0.100000001490116, "SVX8", "signed byte (8 bits)")
test_headers("d40130.pat", 1, 10000, 0.100000001490116, "Gravis Ultrasound patch", "little endian short (16 bits)")
test_headers("d40130.sds", 1, 10000, 0.100000001490116, "MIDI sample dump", "unsupported")
test_headers("d40130.sdx", 1, 10000, 0.100000001490116, "Sample dump", "unsigned little endian short (16 bits)")
test_headers("d40130.sf", 1, 10000, 0.100000001490116, "IRCAM", "little endian short (16 bits)")
test_headers("d40130.smp", 1, 8000, 0.125, "SMP", "little endian short (16 bits)")
test_headers("d40130.sou", 1, 8000, 0.125, "SBStudioII", "little endian short (16 bits)")
test_headers("d40130.st3", 1, 8000, 0.125, "Digiplayer ST3", "unsigned little endian short (16 bits)")
test_headers("d40130.uwf", 1, 8000, 0.125249996781349, "Ultratracker", "little endian short (16 bits)")
test_headers("d40130.voc", 1, 10000, 0.100100003182888, "VOC", "unsigned byte (8 bits)")
test_headers("d40130.w00", 1, 16000, 0.0625, "TX-16W", "unsupported")
test_headers("d40130.wav", 1, 10000, 0.100000001490116, "RIFF", "little endian short (16 bits)")
test_headers("d43.wav", 1, 10000, 0.100000001490116, "RIFF", "little endian short (16 bits)")
test_headers("digit0v0.aiff", 1, 8000, 0.560000002384186, "AIFC", "big endian short (16 bits)")
test_headers("esps-16.snd", 1, 8000, 3.09737491607666, "ESPS", "big endian short (16 bits)")
test_headers("forest.aiff", 2, 44100, 3.907143, "AIFF", "big endian short (16 bits)")
test_headers("g721.au", 1, 11025, 4.35328817367554, "Sun", "unsupported")
test_headers("g722.aifc", 1, 44100, 0.0184353739023209, "AIFC", "unsupported")
test_headers("gong.wve", 1, 8000, 3.96799993515015, "PSION", "alaw (8 bits)")
test_headers("gsm610.wav", 1, 11025, 1.7687075138092, "RIFF", "unsupported")
test_headers("inrs-16.snd", 1, 8000, 2.46399998664856, "INRS", "little endian short (16 bits)")
test_headers("kirk.wve", 1, 8000, 1.40799999237061, "PSION", "alaw (8 bits)")
test_headers("loop.aiff", 1, 44100, 0.0367120169103146, "AIFC", "big endian short (16 bits)")
test_headers("m.asf", 1, 8000, 0.0, "asf", "unsupported")
test_headers("mary-sun4.sig", 1, 8000, 5.95137500762939, "Comdisco SPW signal", "big endian double (64 bits)")
test_headers("mocksong.wav", 1, 11025, 7.86956930160522, "RIFF", "little endian short (16 bits)")
test_headers("mono24.wav", 1, 22050, 1.98997735977173, "RIFF", "little endian int (24 bits)")
test_headers("msadpcm.wav", 1, 11025, 4.43501138687134, "RIFF", "unsupported")
test_headers("n8.snd", 1, 44100, 0.0367800444364548, "Sun", "signed byte (8 bits)")
test_headers("nasahal.aif", 1, 11025, 9.89841270446777, "AIFF", "signed byte (8 bits)")
test_headers("nasahal.avi", 1, 11025, 0.0, "AVI", "unsupported")
test_headers("nasahal.dig", 1, 11025, 9.89841270446777, "Sound Designer 1", "big endian short (16 bits)")
test_headers("nasahal.ivc", 2, 44100, 0.449002265930176, "raw (no header)", "big endian short (16 bits)")
test_headers("nasahal.pat", 1, 11025, 3.95410442352295, "Gravis Ultrasound patch", "unsigned byte (8 bits)")
test_headers("nasahal.snd", 1, 11025, 9.89841270446777, "SNDT", "unsigned byte (8 bits)")
test_headers("nasahal.svx", 1, 11025, 9.89841270446777, "SVX8", "signed byte (8 bits)")
test_headers("nasahal.v8", 1, 8000, 13.6412496566772, "Covox V8", "unsigned byte (8 bits)")
test_headers("nasahal.voc", 1, 11025, 9.89941024780273, "VOC", "unsigned byte (8 bits)")
test_headers("nasahal.vox", 2, 44100, 0.224444448947906, "raw (no header)", "big endian short (16 bits)")
test_headers("nasahal8.wav", 1, 11025, 9.89841270446777, "RIFF", "unsigned byte (8 bits)")
test_headers("nasahalad.smp", 1, 11025, 4.94920635223389, "Goldwave sample", "little endian short (16 bits)")
test_headers("next-16.snd", 1, 22050, 1.00004529953003, "Sun", "big endian short (16 bits)")
test_headers("next-8.snd", 1, 22050, 0.226757362484932, "Sun", "signed byte (8 bits)")
test_headers("next-dbl.snd", 1, 22050, 0.226757362484932, "Sun", "big endian double (64 bits)")
test_headers("oboe.ldbl", 1, 22050, 2.30512475967407, "RIFF", "little endian double (64 bits)")
test_headers("next-flt.snd", 1, 22050, 0.226757362484932, "Sun", "big endian float (32 bits)")
test_headers("next-mulaw.snd", 1, 8012, 2.03295063972473, "Sun", "mulaw (8 bits)")
test_headers("next24.snd", 1, 44100, 0.0367800444364548, "Sun", "big endian int (24 bits)")
test_headers("nist-01.wav", 1, 16000, 2.26912498474121, "NIST", "little endian short (16 bits)")
test_headers("nist-10.wav", 1, 16000, 2.26912498474121, "NIST", "big endian short (16 bits)")
test_headers("nist-16.snd", 1, 16000, 1.02400004863739, "NIST", "big endian short (16 bits)")
test_headers("nist-shortpack.wav", 1, 16000, 4.53824996948242, "NIST", "unsupported")
test_headers("none.aifc", 1, 44100, 0.0367800444364548, "AIFC", "big endian short (16 bits)")
test_headers("nylon2.wav", 2, 22050, 1.14376413822174, "RIFF", "unsupported")
test_headers("o2.avr", 1, 44100, 0.0183900222182274, "AVR", "big endian short (16 bits)")
test_headers("o2.bicsf", 1, 44100, 0.0367800444364548, "IRCAM", "big endian short (16 bits)")
test_headers("o2.mpeg1", 2, 44100, 0.00709750549867749, "raw (no header)", "big endian short (16 bits)")
test_headers("o2.sd2", 2, 44100, 0.0183900222182274, "raw (no header)", "big endian short (16 bits)")
test_headers("o2.sf2", 1, 44100, 0.0367800444364548, "SoundFont", "little endian short (16 bits)")
test_headers("o2.smp", 1, 8000, 0.202749997377396, "SMP", "little endian short (16 bits)")
test_headers("o2.voc", 1, 44100, 0.0368934236466885, "VOC", "little endian short (16 bits)")
test_headers("o2.wave", 1, 44100, 0.0367800444364548, "RIFF", "little endian short (16 bits)")
test_headers("o2_12bit.aiff", 1, 44100, 0.0367800444364548, "AIFF", "big endian short (16 bits)")
test_headers("o2_18bit.aiff", 1, 44100, 0.0367800444364548, "AIFF", "big endian int (24 bits)")
test_headers("o2_711u.wave", 1, 44100, 0.0367800444364548, "RIFF", "mulaw (8 bits)")
test_headers("o2_722.snd", 1, 44100, 0.0183900222182274, "Sun", "unsupported")
test_headers("o2_726.aiff", 1, 8000, 0.0367499999701977, "AIFC", "unsupported")
test_headers("o2_726.snd", 1, 44100, 0.0230158735066652, "Sun", "unsupported")
test_headers("o2_728.aiff", 1, 8000, 0.0367499999701977, "AIFC", "unsupported")
test_headers("o2_8.iff", 1, 44100, 0.0367800444364548, "SVX8", "signed byte (8 bits)")
test_headers("o2_8.voc", 1, 44100, 0.0370294786989689, "VOC", "unsigned byte (8 bits)")
test_headers("o2_dvi.wave", 1, 44100, 0.0232199542224407, "RIFF", "unsupported")
test_headers("o2_float.bicsf", 1, 44100, 0.0367800444364548, "IRCAM", "big endian float (32 bits)")
test_headers("o2_gsm.aiff", 1, 8000, 0.0367499999701977, "AIFC", "unsupported")
test_headers("o2_u8.avr", 1, 44100, 0.0367800444364548, "AVR", "unsigned byte (8 bits)")
test_headers("o2_u8.wave", 1, 44100, 0.0367800444364548, "RIFF", "unsigned byte (8 bits)")
test_headers("oboe.g721", 1, 22050, 1.15287983417511, "Sun", "unsupported")
test_headers("oboe.g723_24", 1, 22050, 0.864761888980865, "Sun", "unsupported")
test_headers("oboe.g723_40", 1, 22050, 1.44126987457275, "Sun", "unsupported")
test_headers("oboe.sf2", 1, 22050, 2.30512475967407, "SoundFont", "little endian short (16 bits)")
test_headers("oboe.paf", 1, 22050, 2.305125, "Ensoniq Paris", "big endian short (16 bits)")
test_headers("oboe.smp", 1, 22050, 2.305125, "snack SMP", "little endian short (16 bits)")
test_headers("oboe.nsp", 1, 22050, 2.305125, "CSL", "little endian short (16 bits)")
test_headers("oki.snd", 2, 44100, 0.0041950112208724, "raw (no header)", "big endian short (16 bits)")
test_headers("orv-dvi-adpcm.wav", 1, 44100, 1.92725622653961, "RIFF", "unsupported")
test_headers("riff-16.snd", 1, 22050, 1.88766443729401, "RIFF", "little endian short (16 bits)")
test_headers("riff-8-u.snd", 1, 11025, 0.506848096847534, "RIFF", "unsigned byte (8 bits)")
test_headers("rooster.wve", 1, 8000, 2.04800009727478, "PSION", "alaw (8 bits)")
test_headers("sd1-16.snd", 1, 44100, 0.400544226169586, "Sound Designer 1", "big endian short (16 bits)")
test_headers("segfault.snd", 16777216, 576061440, 1.24986669902682e-7, "Sun", "unsupported")
test_headers("sf-16.snd", 1, 22050, 1.88766443729401, "IRCAM", "big endian short (16 bits)")
test_headers("si654.adc", 1, 16000, 6.71362495422363, "ADC/OGI", "big endian short (16 bits)")
test_headers("smp-16.snd", 1, 8000, 5.2028751373291, "SMP", "little endian short (16 bits)")
test_headers("sound.pat", 1, 8000, 1.95050001144409, "Gravis Ultrasound patch", "unsigned little endian short (16 bits)")
test_headers("sound.sap", 1, 8000, 1.95050001144409, "Goldwave sample", "little endian short (16 bits)")
test_headers("sound.sds", 1, 8000, 1.95050001144409, "MIDI sample dump", "unsupported")
test_headers("sound.sfr", 1, 8000, 1.95050001144409, "SRFS", "little endian short (16 bits)")
test_headers("sound.v8", 1, 8000, 1.95050001144409, "Covox V8", "unsigned byte (8 bits)")
test_headers("sound.vox", 2, 44100, 0.044217687100172, "raw (no header)", "big endian short (16 bits)")
test_headers("step.omf", 1, 11025, 8.70666694641113, "OMF", "signed byte (8 bits)")
test_headers("step.qt", 1, 11025, 8.70630359649658, "Quicktime", "unsigned byte (8 bits)")
test_headers("sun-16-afsp.snd", 1, 8000, 2.9760000705719, "Sun", "big endian short (16 bits)")
test_headers("sun-mulaw.snd", 1, 8000, 4.61950016021729, "Sun", "mulaw (8 bits)")
test_headers("sw1038t_short.wav", 2, 8000, 6.0, "NIST", "mulaw (8 bits)")
test_headers("swirl.pat", 1, 22050, 1.0619500875473, "Gravis Ultrasound patch", "unsigned little endian short (16 bits)")
test_headers("sy85.snd", 1, 8000, 5.05600023269653, "Sy-85", "big endian short (16 bits)")
test_headers("sy99.snd", 1, 8000, 4.54400014877319, "Sy-99", "big endian short (16 bits)")
test_headers("telephone.wav", 1, 16000, 2.27881240844727, "NIST", "little endian short (16 bits)")
test_headers("truspech.wav", 1, 8000, 1.1599999666214, "RIFF", "unsupported")
test_headers("ulaw.aifc", 1, 44100, 0.0367800444364548, "AIFC", "mulaw (8 bits)")
test_headers("voc-8-u.snd", 1, 8000, 1.49937498569489, "VOC", "unsigned byte (8 bits)")
test_headers("voxware.wav", 1, 8000, 0.324000000953674, "RIFF", "unsupported")
test_headers("wd.w00", 1, 8000, 0.202749997377396, "Sy-99", "big endian short (16 bits)")
test_headers("wd1.smp", 1, 8000, 0.202749997377396, "SMP", "little endian short (16 bits)")
test_headers("wd1.wav", 1, 44100, 0.0367800444364548, "RIFF", "little endian short (16 bits)")
test_headers("wheel.mat", 2, 44100, 0.145646259188652, "raw (no header)", "big endian short (16 bits)")
test_headers("b8.pvf", 1, 44100, 0.036803, "Portable Voice Format", "signed byte (8 bits)")
test_headers("b16.pvf", 1, 44100, 0.036803, "Portable Voice Format", "big endian short (16 bits)")
test_headers("b32.pvf", 1, 44100, 0.036803, "Portable Voice Format", "big endian int (32 bits)")
test_headers("wood.dsf", 1, 8000, 0.202749997377396, "Delusion", "little endian short (16 bits)")
test_headers("wood.dvi", 1, 22100, 0.0278733037412167, "RIFF", "unsupported")
test_headers("wood.dwd", 1, 22100, 0.0733936652541161, "DiamondWare", "signed byte (8 bits)")
test_headers("wood.fsm", 1, 8000, 0.202999994158745, "Farandole", "little endian short (16 bits)")
test_headers("wood.mad", 1, 22100, 0.0372398197650909, "RIFF", "unsupported")
test_headers("wood.maud", 1, 44100, 0.0183900222182274, "MAUD", "big endian short (16 bits)")
test_headers("wood.pat", 1, 22100, 0.0733936652541161, "Gravis Ultrasound patch", "little endian short (16 bits)")
test_headers("wood.riff", 1, 44100, 0.0367800444364548, "RIFF", "little endian short (16 bits)")
test_headers("wood.rifx", 1, 44100, 0.0367800444364548, "RIFF", "big endian short (16 bits)")
test_headers("wood.sds", 1, 22100, 0.0733936652541161, "MIDI sample dump", "unsupported")
test_headers("wood.sdx", 1, 22100, 0.0733936652541161, "Sample dump", "unsigned little endian short (16 bits)")
test_headers("wood.sf", 1, 44100, 0.0367800444364548, "IRCAM", "big endian short (16 bits)")
test_headers("wood.sndr", 2, 44100, 0.0092290248721838, "raw (no header)", "big endian short (16 bits)")
test_headers("wood.sndt", 1, 44100, 0.0367800444364548, "SNDT", "unsigned byte (8 bits)")
test_headers("wood.st3", 1, 8000, 0.202749997377396, "Digiplayer ST3", "unsigned little endian short (16 bits)")
test_headers("wood.uwf", 1, 8000, 0.202999994158745, "Ultratracker", "little endian short (16 bits)")
test_headers("wood.w00", 1, 16000, 0.101374998688698, "TX-16W", "unsupported")
test_headers("wood12.aiff", 1, 44100, 0.0367800444364548, "AIFF", "big endian short (16 bits)")
test_headers("wood16.dwd", 2, 44100, 0.0367800444364548, "DiamondWare", "little endian short (16 bits)")
test_headers("wood16.wav", 2, 44100, 0.0367800444364548, "RIFF", "little endian short (16 bits)")
test_headers("wood16.nsp", 2, 44100, 0.0367800444364548, "CSL", "little endian short (16 bits)")
test_headers("wood16.smp", 2, 44100, 0.0367800444364548, "snack SMP", "little endian short (16 bits)")
test_headers("wood24.aiff", 1, 44100, 0.0367800444364548, "AIFF", "big endian int (24 bits)")
test_headers("woodblock.aiff", 1, 44100, 0.0367800444364548, "AIFF", "big endian short (16 bits)")
test_headers("woodflt.snd", 1, 44100, 0.0367800444364548, "Sun", "big endian float (32 bits)")
test_headers("RealDrums.sf2", 1, 44100, 6.39725637435913, "SoundFont", "little endian short (16 bits)")
test_headers("32bit.sf", 1, 44100, 4.6, "IRCAM", "little endian float (32 bits, unscaled)")
test_headers("PCM_48_8bit_m.w64", 1, 48000, 0.375, "SoundForge", "unsigned byte (8 bits)")
test_headers("addf8.24we", 1, 8000, 2.976000, "RIFF", "little endian int (24 bits)")
test_headers("hybrid.snd", 1, 44100, 4.600000, "BICSF", "big endian float (32 bits)")
test_headers("zulu_a4.w11", 1, 33000, 1.21987879276276, "TX-16W", "unsupported")

# ---------------------------------------- test 3 ----------------------------------------

$oboe = open_sound "oboe.snd"
$td = temp_dir     
if (sample(1000) - 0.0328).abs > .001 then snd_print sprintf("# sample: %s\n", sample(1000)); end
set_show_controls true
if not provided? "snd-nogui" then
  enved_dialog
  if !dialog_widgets[2] then snd_print sprintf("# enved_dialog?") end

#	    (set! (enved_active_env) '(0.0 0.0 1.0 1.0 2.0 0.0))
#	    (if (not (equal? (enved_active_env) (list 0.0 0.0 1.0 1.0 2.0 0.0)))
#		(snd_display ";set enved_active_env: ~A?" (enved_active_env)))
#   
  orientation_dialog
  if !dialog_widgets[1] then snd_print sprintf("# orientation_dialog?") end
end
dismiss_all_dialogs

set_amp_control 0.5
if ffneq(amp_control, 0.5) then snd_display sprintf("\n# amp_control: %s", amp_control) end
set_amp_control 1.0
if fneq(amp_control, 1.0) then snd_display sprintf("\n# init amp_control: %s", amp_control) end
set_ask_before_overwrite true
if ask_before_overwrite !=  true then snd_display sprintf("\n# ask_before_overwrite: %s", ask_before_overwrite) end
set_ask_before_overwrite false 
if ask_before_overwrite != false then snd_display sprintf("\n# ask_before_overwrite: %s", ask_before_overwrite) end
set_audio_state_file "not_a_file"
if audio_state_file !=  "not_a_file" then snd_display sprintf("\n# audio_state_file: %s", audio_state_file) end
set_audio_state_file ".snd_mixer" 
if audio_state_file != ".snd_mixer" then snd_display sprintf("\n# audio_state_file: %s", audio_state_file) end
set_audio_input_device 1
if audio_input_device !=  1 then snd_display sprintf("\n# audio_input_device: %s", audio_input_device) end
set_audio_input_device 0 
if audio_input_device != 0 then snd_display sprintf("\n# audio_input_device: %s", audio_input_device) end
set_audio_output_device 1
if audio_output_device !=  1 then snd_display sprintf("\n# audio_output_device: %s", audio_output_device) end
set_audio_output_device 0 
if audio_output_device != 0 then snd_display sprintf("\n# audio_output_device: %s", audio_output_device) end
set_auto_resize false
if auto_resize !=  false then snd_display sprintf("\n# auto_resize: %s", auto_resize) end
set_auto_resize true 
if auto_resize != true then snd_display sprintf("\n# auto_resize: %s", auto_resize) end
set_auto_update true
if auto_update !=  true then snd_display sprintf("\n# auto_update: %s", auto_update) end
set_auto_update false 
if auto_update != false then snd_display sprintf("\n# auto_update: %s", auto_update) end
set_channel_style 1
if channel_style !=  1 then snd_display sprintf("\n# channel_style: %s", channel_style) end
set_channel_style 0 
if channel_style != 0 then snd_display sprintf("\n# channel_style: %s", channel_style) end
set_colormap 0
if colormap !=  0 then snd_display sprintf("\n# colormap: %s", colormap) end
set_colormap 2 
if colormap != 2 then snd_display sprintf("\n# colormap: %s", colormap) end
set_color_cutoff 0.01
if fneq(color_cutoff, 0.01) then snd_display sprintf("\n# color_cutoff: %s", color_cutoff) end
set_color_cutoff 0.003 
if fneq(color_cutoff, 0.003) then snd_display sprintf("\n# color_cutoff: %s", color_cutoff) end
set_color_inverted false
if color_inverted !=  false then snd_display sprintf("\n# color_inverted: %s", color_inverted) end
set_color_inverted true 
if color_inverted != true then snd_display sprintf("\n# color_inverted: %s", color_inverted) end
set_color_scale 0.5
if fneq(color_scale, 0.5) then snd_display sprintf("\n# color_scale: %s", color_scale) end
set_color_scale 1.0 
if fneq(color_scale, 1.0) then snd_display sprintf("\n# color_scale: %s", color_scale) end
set_contrast_control 0.5
if fneq(contrast_control, 0.5) then snd_display sprintf("\n# contrast_control: %s", contrast_control) end
set_contrast_control 0.0 
if fneq(contrast_control, 0.0) then snd_display sprintf("\n# contrast_control: %s", contrast_control) end
set_contrast_control_amp 0.5
if fneq(contrast_control_amp, 0.5) then snd_display sprintf("\n# contrast_control_amp: %s", contrast_control_amp) end
set_contrast_control_amp 1.0 
if fneq(contrast_control_amp, 1.0) then snd_display sprintf("\n# contrast_control_amp: %s", contrast_control_amp) end
set_contrast_control? true
if contrast_control? !=  true then snd_display sprintf("\n# contrast_control?: %s", contrast_control?) end
set_contrast_control? false 
if contrast_control? != false then snd_display sprintf("\n# contrast_control?: %s", contrast_control?) end
set_auto_update_interval 120.0
if fneq(auto_update_interval, 120.0) then snd_display sprintf("\n# auto_update_interval: %s", auto_update_interval) end
set_auto_update_interval 60.0 
if fneq(auto_update_interval, 60.0) then snd_display sprintf("\n# auto_update_interval: %s", auto_update_interval) end
set_cursor_follows_play true
if cursor_follows_play !=  true then snd_display sprintf("\n# cursor_follows_play: %s", cursor_follows_play) end
set_cursor_follows_play false 
if cursor_follows_play != false then snd_display sprintf("\n# cursor_follows_play: %s", cursor_follows_play) end
set_dac_combines_channels false
if dac_combines_channels !=  false then snd_display sprintf("\n# dac_combines_channels: %s", dac_combines_channels) end
set_dac_combines_channels true 
if dac_combines_channels != true then snd_display sprintf("\n# dac_combines_channels: %s", dac_combines_channels) end
set_dac_size 512
if dac_size !=  512 then snd_display sprintf("\n# dac_size: %s", dac_size) end
set_dac_size 256 
if dac_size != 256 then snd_display sprintf("\n# dac_size: %s", dac_size) end
set_minibuffer_history_length 16
if minibuffer_history_length !=  16 then snd_display sprintf("\n# minibuffer_history_length: %s", minibuffer_history_length) end
set_minibuffer_history_length 8 
if minibuffer_history_length != 8 then snd_display sprintf("\n# minibuffer_history_length: %s", minibuffer_history_length) end
set_data_clipped true
if data_clipped !=  true then snd_display sprintf("\n# data_clipped: %s", data_clipped) end
set_data_clipped false 
if data_clipped != false then snd_display sprintf("\n# data_clipped: %s", data_clipped) end
set_default_output_chans 2
if default_output_chans !=  2 then snd_display sprintf("\n# default_output_chans: %s", default_output_chans) end
set_default_output_chans 1 
if default_output_chans != 1 then snd_display sprintf("\n# default_output_chans: %s", default_output_chans) end
set_default_output_format 1
if default_output_format !=  1 then snd_display sprintf("\n# default_output_format: %s", default_output_format) end
set_default_output_format 1 
if default_output_format != 1 then snd_display sprintf("\n# default_output_format: %s", default_output_format) end
set_default_output_srate 44100
if default_output_srate !=  44100 then snd_display sprintf("\n# default_output_srate: %s", default_output_srate) end
set_default_output_srate 22050 
if default_output_srate != 22050 then snd_display sprintf("\n# default_output_srate: %s", default_output_srate) end
set_default_output_type 1
if default_output_type !=  1 then snd_display sprintf("\n# default_output_type: %s", default_output_type) end
set_default_output_type 0 
if default_output_type != 0 then snd_display sprintf("\n# default_output_type: %s", default_output_type) end
set_dot_size 4
if dot_size !=  4 then snd_display sprintf("\n# dot_size: %s", dot_size) end
set_dot_size 1 
if dot_size != 1 then snd_display sprintf("\n# dot_size: %s", dot_size) end
set_enved_base 1.5
if fneq(enved_base, 1.5) then snd_display sprintf("\n# enved_base: %s", enved_base) end
set_enved_base 1.0 
if fneq(enved_base, 1.0) then snd_display sprintf("\n# enved_base: %s", enved_base) end
set_enved_clip? true
if enved_clip? !=  true then snd_display sprintf("\n# enved_clip?: %s", enved_clip?) end
set_enved_clip? false 
if enved_clip? != false then snd_display sprintf("\n# enved_clip?: %s", enved_clip?) end
set_enved_in_dB true
if enved_in_dB !=  true then snd_display sprintf("\n# enved_in_dB: %s", enved_in_dB) end
set_enved_in_dB false 
if enved_in_dB != false then snd_display sprintf("\n# enved_in_dB: %s", enved_in_dB) end
set_enved_exp? true
if enved_exp? !=  true then snd_display sprintf("\n# enved_exp?: %s", enved_exp?) end
set_enved_exp? false 
if enved_exp? != false then snd_display sprintf("\n# enved_exp?: %s", enved_exp?) end
set_enved_power 3.5
if fneq(enved_power, 3.5) then snd_display sprintf("\n# enved_power: %s", enved_power) end
set_enved_power 3.0 
if fneq(enved_power, 3.0) then snd_display sprintf("\n# enved_power: %s", enved_power) end
set_enved_target 1
if enved_target !=  1 then snd_display sprintf("\n# enved_target: %s", enved_target) end
set_enved_target 0 
if enved_target != 0 then snd_display sprintf("\n# enved_target: %s", enved_target) end
set_enved_wave? true
if enved_wave? !=  true then snd_display sprintf("\n# enved_wave?: %s", enved_wave?) end
set_enved_wave? false 
if enved_wave? != false then snd_display sprintf("\n# enved_wave?: %s", enved_wave?) end
set_eps_file "snd_1.eps"
if eps_file !=  "snd_1.eps" then snd_display sprintf("\n# eps_file: %s", eps_file) end
set_eps_file "snd.eps" 
if eps_file != "snd.eps" then snd_display sprintf("\n# eps_file: %s", eps_file) end
set_eps_left_margin 72.0
if eps_left_margin !=  72.0 then snd_display sprintf("\n# eps_left_margin: %s", eps_left_margin) end
set_eps_left_margin 0.0 
if eps_left_margin != 0.0 then snd_display sprintf("\n# eps_left_margin: %s", eps_left_margin) end
set_eps_size 2.0
if eps_size !=  2.0 then snd_display sprintf("\n# eps_size: %s", eps_size) end
set_eps_size 1.0 
if eps_size != 1.0 then snd_display sprintf("\n# eps_size: %s", eps_size) end
set_eps_bottom_margin 36.0
if fneq(eps_bottom_margin, 36.0) then snd_display sprintf("\n# eps_bottom_margin: %s", eps_bottom_margin) end
set_eps_bottom_margin 0.0 
if fneq(eps_bottom_margin, 0.0) then snd_display sprintf("\n# eps_bottom_margin: %s", eps_bottom_margin) end
set_expand_control 2.0
if fneq(expand_control, 2.0) then snd_display sprintf("\n# expand_control: %s", expand_control) end
set_expand_control 1.0 
if fneq(expand_control, 1.0) then snd_display sprintf("\n# expand_control: %s", expand_control) end
set_expand_control_hop 0.1
if fneq(expand_control_hop, 0.1) then snd_display sprintf("\n# expand_control_hop: %s", expand_control_hop) end
set_expand_control_hop 0.05 
if fneq(expand_control_hop, 0.05) then snd_display sprintf("\n# expand_control_hop: %s", expand_control_hop) end
set_expand_control_length 0.2
if fneq(expand_control_length, 0.2) then snd_display sprintf("\n# expand_control_length: %s", expand_control_length) end
set_expand_control_length 0.15 
if fneq(expand_control_length, 0.15) then snd_display sprintf("\n# expand_control_length: %s", expand_control_length) end
set_expand_control_ramp 0.2
if fneq(expand_control_ramp, 0.2) then snd_display sprintf("\n# expand_control_ramp: %s", expand_control_ramp) end
set_expand_control_ramp 0.4 
if fneq(expand_control_ramp, 0.4) then snd_display sprintf("\n# expand_control_ramp: %s", expand_control_ramp) end
set_expand_control? true
if expand_control? !=  true then snd_display sprintf("\n# expand_control?: %s", expand_control?) end
set_expand_control? false 
if expand_control? != false then snd_display sprintf("\n# expand_control?: %s", expand_control?) end
set_fft_window_beta 0.5
if fneq(fft_window_beta, 0.5) then snd_display sprintf("\n# fft_window_beta: %s", fft_window_beta) end
set_fft_window_beta 0.0 
if fneq(fft_window_beta, 0.0) then snd_display sprintf("\n# fft_window_beta: %s", fft_window_beta) end
set_fft_log_frequency true
if fft_log_frequency !=  true then snd_display sprintf("\n# fft_log_frequency: %s", fft_log_frequency) end
set_fft_log_frequency false 
if fft_log_frequency != false then snd_display sprintf("\n# fft_log_frequency: %s", fft_log_frequency) end
set_fft_log_magnitude true
if fft_log_magnitude !=  true then snd_display sprintf("\n# fft_log_magnitude: %s", fft_log_magnitude) end
set_fft_log_magnitude false 
if fft_log_magnitude != false then snd_display sprintf("\n# fft_log_magnitude: %s", fft_log_magnitude) end
set_transform_size 512
if transform_size !=  512 then snd_display sprintf("\n# transform_size: %s", transform_size) end
set_transform_size 256 
if transform_size != 256 then snd_display sprintf("\n# transform_size: %s", transform_size) end
set_transform_graph_type 1
if transform_graph_type !=  1 then snd_display sprintf("\n# transform_graph_type: %s", transform_graph_type) end
set_transform_graph_type 0 
if transform_graph_type != 0 then snd_display sprintf("\n# transform_graph_type: %s", transform_graph_type) end
set_fft_window 5
if fft_window !=  5 then snd_display sprintf("\n# fft_window: %s", fft_window) end
set_fft_window 6 
if fft_window != 6 then snd_display sprintf("\n# fft_window: %s", fft_window) end
set_transform_graph? true
if transform_graph? !=  true then snd_display sprintf("\n# transform_graph?: %s", transform_graph?) end
set_transform_graph? false 
if transform_graph? != false then snd_display sprintf("\n# transform_graph?: %s", transform_graph?) end
set_filter_control_in_dB true
if filter_control_in_dB !=  true then snd_display sprintf("\n# filter_control_in_dB: %s", filter_control_in_dB) end
set_filter_control_in_dB false 
if filter_control_in_dB != false then snd_display sprintf("\n# filter_control_in_dB: %s", filter_control_in_dB) end
set_enved_filter false
if enved_filter !=  false then snd_display sprintf("\n# enved_filter: %s", enved_filter) end
set_enved_filter true 
if enved_filter != true then snd_display sprintf("\n# enved_filter: %s", enved_filter) end
set_enved_filter_order 20
if enved_filter_order !=  20 then snd_display sprintf("\n# enved_filter_order: %s", enved_filter_order) end
set_enved_filter_order 40 
if enved_filter_order != 40 then snd_display sprintf("\n# enved_filter_order: %s", enved_filter_order) end
set_filter_env_in_hz true
if filter_env_in_hz !=  true then snd_display sprintf("\n# filter_env_in_hz: %s", filter_env_in_hz) end
set_filter_env_in_hz false 
if filter_env_in_hz != false then snd_display sprintf("\n# filter_env_in_hz: %s", filter_env_in_hz) end
set_filter_control_order 40
if filter_control_order !=  40 then snd_display sprintf("\n# filter_control_order: %s", filter_control_order) end
set_filter_control_order 20 
if filter_control_order != 20 then snd_display sprintf("\n# filter_control_order: %s", filter_control_order) end
set_filter_control? true
if filter_control? !=  true then snd_display sprintf("\n# filter_control?: %s", filter_control?) end
set_filter_control? false 
if filter_control? != false then snd_display sprintf("\n# filter_control?: %s", filter_control?) end
set_graph_cursor 33
if graph_cursor !=  33 then snd_display sprintf("\n# graph_cursor: %s", graph_cursor) end
set_graph_cursor 34 
if graph_cursor != 34 then snd_display sprintf("\n# graph_cursor: %s", graph_cursor) end
set_graph_style 1
if graph_style !=  1 then snd_display sprintf("\n# graph_style: %s", graph_style) end
set_graph_style 0 
if graph_style != 0 then snd_display sprintf("\n# graph_style: %s", graph_style) end
set_just_sounds true
if just_sounds !=  true then snd_display sprintf("\n# just_sounds: %s", just_sounds) end
set_just_sounds false 
if just_sounds != false then snd_display sprintf("\n# just_sounds: %s", just_sounds) end
set_listener_prompt ":"
if listener_prompt !=  ":" then snd_display sprintf("\n# listener_prompt: %s", listener_prompt) end
set_listener_prompt ">" 
if listener_prompt != ">" then snd_display sprintf("\n# listener_prompt: %s", listener_prompt) end
set_max_transform_peaks 10
if max_transform_peaks !=  10 then snd_display sprintf("\n# max_transform_peaks: %s", max_transform_peaks) end
set_max_transform_peaks 100 
if max_transform_peaks != 100 then snd_display sprintf("\n# max_transform_peaks: %s", max_transform_peaks) end
set_max_regions 6
if max_regions !=  6 then snd_display sprintf("\n# max_regions: %s", max_regions) end
set_max_regions 16 
if max_regions != 16 then snd_display sprintf("\n# max_regions: %s", max_regions) end
set_min_dB -90.0
if fneq(min_dB, -90.0) then snd_display sprintf("\n# min_dB: %s", min_dB) end
set_min_dB -60.0 
if fneq(min_dB, -60.0) then snd_display sprintf("\n# min_dB: %s", min_dB) end
set_mix_waveform_height 40
if mix_waveform_height !=  40 then snd_display sprintf("\n# mix_waveform_height: %s", mix_waveform_height) end
set_mix_waveform_height 20 
if mix_waveform_height != 20 then snd_display sprintf("\n# mix_waveform_height: %s", mix_waveform_height) end
set_mix_tag_height 20
if mix_tag_height !=  20 then snd_display sprintf("\n# mix_tag_height: %s", mix_tag_height) end
set_mix_tag_height 14 
if mix_tag_height != 14 then snd_display sprintf("\n# mix_tag_height: %s", mix_tag_height) end
set_mix_tag_width 20
if mix_tag_width !=  20 then snd_display sprintf("\n# mix_tag_width: %s", mix_tag_width) end
set_mix_tag_width 6 
if mix_tag_width != 6 then snd_display sprintf("\n# mix_tag_width: %s", mix_tag_width) end
set_selection_creates_region false
if selection_creates_region !=  false then snd_display sprintf("\n# selection_creates_region: %s", selection_creates_region) end
set_selection_creates_region true 
if selection_creates_region != true then snd_display sprintf("\n# selection_creates_region: %s", selection_creates_region) end
set_transform_normalization Dont_normalize
if transform_normalization !=  Dont_normalize then snd_display sprintf("\n# transform_normalization: %s", transform_normalization) end
set_transform_normalization Normalize_by_channel 
if transform_normalization != Normalize_by_channel then snd_display sprintf("\n# transform_normalization: %s", transform_normalization) end
set_previous_files_sort 1
if previous_files_sort !=  1 then snd_display sprintf("\n# previous_files_sort: %s", previous_files_sort) end
set_previous_files_sort 0 
if previous_files_sort != 0 then snd_display sprintf("\n# previous_files_sort: %s", previous_files_sort) end
set_print_length 16
if print_length !=  16 then snd_display sprintf("\n# print_length: %s", print_length) end
set_print_length 12 
if print_length != 12 then snd_display sprintf("\n# print_length: %s", print_length) end
set_recorder_autoload true
if recorder_autoload !=  true then snd_display sprintf("\n# recorder_autoload: %s", recorder_autoload) end
set_recorder_autoload false 
if recorder_autoload != false then snd_display sprintf("\n# recorder_autoload: %s", recorder_autoload) end
set_recorder_out_chans 1
if recorder_out_chans !=  1 then snd_display sprintf("\n# recorder_out_chans: %s", recorder_out_chans) end
set_recorder_out_chans 2 
if recorder_out_chans != 2 then snd_display sprintf("\n# recorder_out_chans: %s", recorder_out_chans) end
set_recorder_buffer_size 256
if recorder_buffer_size !=  256 then snd_display sprintf("\n# recorder_buffer_size: %s", recorder_buffer_size) end
set_recorder_buffer_size 4096 
if recorder_buffer_size != 4096 then snd_display sprintf("\n# recorder_buffer_size: %s", recorder_buffer_size) end
set_recorder_max_duration 1000.0
if fneq(recorder_max_duration, 1000.0) then snd_display sprintf("\n# recorder_max_duration: %s", recorder_max_duration) end
set_recorder_max_duration 1000000.0 
if fneq(recorder_max_duration, 1000000.0) then snd_display sprintf("\n# recorder_max_duration: %s", recorder_max_duration) end
set_recorder_trigger 0.1
if fneq(recorder_trigger, 0.1) then snd_display sprintf("\n# recorder_trigger: %s", recorder_trigger) end
set_recorder_trigger 0.0 
if fneq(recorder_trigger, 0.0) then snd_display sprintf("\n# recorder_trigger: %s", recorder_trigger) end
set_region_graph_style Graph_lollipops
if region_graph_style !=  Graph_lollipops then snd_display sprintf("\n# region_graph_style: %s", region_graph_style) end
set_region_graph_style Graph_lines 
if region_graph_style != Graph_lines then snd_display sprintf("\n# region_graph_style: %s", region_graph_style) end
set_reverb_control_decay 2.0
if fneq(reverb_control_decay, 2.0) then snd_display sprintf("\n# reverb_control_decay: %s", reverb_control_decay) end
set_reverb_control_decay 1.0 
if fneq(reverb_control_decay, 1.0) then snd_display sprintf("\n# reverb_control_decay: %s", reverb_control_decay) end
set_reverb_control_feedback 1.6
if fneq(reverb_control_feedback, 1.6) then snd_display sprintf("\n# reverb_control_feedback: %s", reverb_control_feedback) end
set_reverb_control_feedback 1.09 
if fneq(reverb_control_feedback, 1.09) then snd_display sprintf("\n# reverb_control_feedback: %s", reverb_control_feedback) end
set_reverb_control_length 2.0
if fneq(reverb_control_length, 2.0) then snd_display sprintf("\n# reverb_control_length: %s", reverb_control_length) end
set_reverb_control_length 1.0 
if fneq(reverb_control_length, 1.0) then snd_display sprintf("\n# reverb_control_length: %s", reverb_control_length) end
set_reverb_control_lowpass 0.9
if fneq(reverb_control_lowpass, 0.9) then snd_display sprintf("\n# reverb_control_lowpass: %s", reverb_control_lowpass) end
set_reverb_control_lowpass 0.7 
if fneq(reverb_control_lowpass, 0.7) then snd_display sprintf("\n# reverb_control_lowpass: %s", reverb_control_lowpass) end
set_reverb_control_scale 0.2
if ffneq(reverb_control_scale, 0.2) then snd_display sprintf("\n# reverb_control_scale: %s", reverb_control_scale) end
set_reverb_control_scale 0.0 
if fneq(reverb_control_scale, 0.0) then snd_display sprintf("\n# reverb_control_scale: %s", reverb_control_scale) end
set_reverb_control? true
if reverb_control? !=  true then snd_display sprintf("\n# reverb_control?: %s", reverb_control?) end
set_reverb_control? false 
if reverb_control? != false then snd_display sprintf("\n# reverb_control?: %s", reverb_control?) end
set_show_axes 0
if show_axes !=  0 then snd_display sprintf("\n# show_axes: %s", show_axes) end
set_show_axes 1 
if show_axes != 1 then snd_display sprintf("\n# show_axes: %s", show_axes) end
set_show_transform_peaks true
if show_transform_peaks !=  true then snd_display sprintf("\n# show_transform_peaks: %s", show_transform_peaks) end
set_show_transform_peaks false 
if show_transform_peaks != false then snd_display sprintf("\n# show_transform_peaks: %s", show_transform_peaks) end
set_show_indices true
if show_indices !=  true then snd_display sprintf("\n# show_indices: %s", show_indices) end
set_show_indices false 
if show_indices != false then snd_display sprintf("\n# show_indices: %s", show_indices) end
set_show_backtrace true
if show_backtrace !=  true then snd_display sprintf("\n# show_backtrace: %s", show_backtrace) end
set_show_backtrace false 
if show_backtrace != false then snd_display sprintf("\n# show_backtrace: %s", show_backtrace) end
set_show_marks false
if show_marks !=  false then snd_display sprintf("\n# show_marks: %s", show_marks) end
set_show_marks true 
if show_marks != true then snd_display sprintf("\n# show_marks: %s", show_marks) end
set_show_mix_waveforms false
if show_mix_waveforms !=  false then snd_display sprintf("\n# show_mix_waveforms: %s", show_mix_waveforms) end
set_show_mix_waveforms true 
if show_mix_waveforms != true then snd_display sprintf("\n# show_mix_waveforms: %s", show_mix_waveforms) end
set_show_selection_transform true
if show_selection_transform !=  true then snd_display sprintf("\n# show_selection_transform: %s", show_selection_transform) end
set_show_selection_transform false 
if show_selection_transform != false then snd_display sprintf("\n# show_selection_transform: %s", show_selection_transform) end
set_show_y_zero true
if show_y_zero !=  true then snd_display sprintf("\n# show_y_zero: %s", show_y_zero) end
set_show_y_zero false 
if show_y_zero != false then snd_display sprintf("\n# show_y_zero: %s", show_y_zero) end
set_sinc_width 40
if sinc_width !=  40 then snd_display sprintf("\n# sinc_width: %s", sinc_width) end
set_sinc_width 10 
if sinc_width != 10 then snd_display sprintf("\n# sinc_width: %s", sinc_width) end
set_spectro_cutoff 0.7
if fneq(spectro_cutoff, 0.7) then snd_display sprintf("\n# spectro_cutoff: %s", spectro_cutoff) end
set_spectro_cutoff 1.0 
if fneq(spectro_cutoff, 1.0) then snd_display sprintf("\n# spectro_cutoff: %s", spectro_cutoff) end
set_spectro_hop 10
if spectro_hop !=  10 then snd_display sprintf("\n# spectro_hop: %s", spectro_hop) end
set_spectro_hop 4 
if spectro_hop != 4 then snd_display sprintf("\n# spectro_hop: %s", spectro_hop) end
set_spectro_start 0.1
if fneq(spectro_start, 0.1) then snd_display sprintf("\n# spectro_start: %s", spectro_start) end
set_spectro_start 0.0 
if fneq(spectro_start, 0.0) then snd_display sprintf("\n# spectro_start: %s", spectro_start) end
set_spectro_x_angle 60.0
if fneq(spectro_x_angle, 60.0) then snd_display sprintf("\n# spectro_x_angle: %s", spectro_x_angle) end
set_spectro_x_angle 90.0 
if fneq(spectro_x_angle, 90.0) then snd_display sprintf("\n# spectro_x_angle: %s", spectro_x_angle) end
set_spectro_x_scale 2.0
if fneq(spectro_x_scale, 2.0) then snd_display sprintf("\n# spectro_x_scale: %s", spectro_x_scale) end
set_spectro_x_scale 1.0 
if fneq(spectro_x_scale, 1.0) then snd_display sprintf("\n# spectro_x_scale: %s", spectro_x_scale) end
set_spectro_y_angle 60.0
if fneq(spectro_y_angle, 60.0) then snd_display sprintf("\n# spectro_y_angle: %s", spectro_y_angle) end
set_spectro_y_angle 0.0 
if fneq(spectro_y_angle, 0.0) then snd_display sprintf("\n# spectro_y_angle: %s", spectro_y_angle) end
set_spectro_y_scale 2.0
if fneq(spectro_y_scale, 2.0) then snd_display sprintf("\n# spectro_y_scale: %s", spectro_y_scale) end
set_spectro_y_scale 1.0 
if fneq(spectro_y_scale, 1.0) then snd_display sprintf("\n# spectro_y_scale: %s", spectro_y_scale) end
set_spectro_z_angle 60.0
if fneq(spectro_z_angle, 60.0) then snd_display sprintf("\n# spectro_z_angle: %s", spectro_z_angle) end
set_spectro_z_angle 358.0 
if fneq(spectro_z_angle, 358.0) then snd_display sprintf("\n# spectro_z_angle: %s", spectro_z_angle) end
set_spectro_z_scale 0.2
if fneq(spectro_z_scale, 0.2) then snd_display sprintf("\n# spectro_z_scale: %s", spectro_z_scale) end
set_spectro_z_scale 0.1 
if fneq(spectro_z_scale, 0.1) then snd_display sprintf("\n# spectro_z_scale: %s", spectro_z_scale) end
set_speed_control 0.5
if fneq(speed_control, 0.5) then snd_display sprintf("\n# speed_control: %s", speed_control) end
set_speed_control 1.0 
if fneq(speed_control, 1.0) then snd_display sprintf("\n# speed_control: %s", speed_control) end
set_speed_control_style 1
if speed_control_style !=  1 then snd_display sprintf("\n# speed_control_style: %s", speed_control_style) end
set_speed_control_style 0 
if speed_control_style != 0 then snd_display sprintf("\n# speed_control_style: %s", speed_control_style) end
set_speed_control_tones 18
if speed_control_tones !=  18 then snd_display sprintf("\n# speed_control_tones: %s", speed_control_tones) end
set_speed_control_tones 12 
if speed_control_tones != 12 then snd_display sprintf("\n# speed_control_tones: %s", speed_control_tones) end
set_sync 1
if sync !=  1 then snd_display sprintf("\n# sync: %s", sync) end
set_sync 0 
if sync != 0 then snd_display sprintf("\n# sync: %s", sync) end
set_tiny_font "9x15"
if tiny_font !=  "9x15" then snd_display sprintf("\n# tiny_font: %s", tiny_font) end
set_tiny_font "6x12" 
if tiny_font != "6x12" then snd_display sprintf("\n# tiny_font: %s", tiny_font) end
set_transform_type 1
if transform_type !=  1 then snd_display sprintf("\n# transform_type: %s", transform_type) end
set_transform_type 0 
if transform_type != 0 then snd_display sprintf("\n# transform_type: %s", transform_type) end
set_use_sinc_interp false
if use_sinc_interp !=  false then snd_display sprintf("\n# use_sinc_interp: %s", use_sinc_interp) end
set_use_sinc_interp true 
if use_sinc_interp != true then snd_display sprintf("\n# use_sinc_interp: %s", use_sinc_interp) end
set_verbose_cursor true
if verbose_cursor !=  true then snd_display sprintf("\n# verbose_cursor: %s", verbose_cursor) end
set_verbose_cursor false 
if verbose_cursor != false then snd_display sprintf("\n# verbose_cursor: %s", verbose_cursor) end
set_vu_size 2.0
if fneq(vu_size, 2.0) then snd_display sprintf("\n# vu_size: %s", vu_size) end
set_vu_size 1.0 
if fneq(vu_size, 1.0) then snd_display sprintf("\n# vu_size: %s", vu_size) end
set_vu_font_size 2.0
if fneq(vu_font_size, 2.0) then snd_display sprintf("\n# vu_font_size: %s", vu_font_size) end
set_vu_font_size 1.0 
if fneq(vu_font_size, 1.0) then snd_display sprintf("\n# vu_font_size: %s", vu_font_size) end
set_wavelet_type 1
if wavelet_type !=  1 then snd_display sprintf("\n# wavelet_type: %s", wavelet_type) end
set_wavelet_type 0 
if wavelet_type != 0 then snd_display sprintf("\n# wavelet_type: %s", wavelet_type) end
set_time_graph? true
if time_graph? !=  true then snd_display sprintf("\n# time_graph?: %s", time_graph?) end
set_time_graph? false 
if time_graph? != false then snd_display sprintf("\n# time_graph?: %s", time_graph?) end
set_time_graph_type Graph_as_wavogram
if time_graph_type !=  Graph_as_wavogram then snd_display sprintf("\n# time_graph_type: %s", time_graph_type) end
set_time_graph_type Graph_once 
if time_graph_type != Graph_once then snd_display sprintf("\n# time_graph_type: %s", time_graph_type) end
set_wavo_hop 6
if wavo_hop !=  6 then snd_display sprintf("\n# wavo_hop: %s", wavo_hop) end
set_wavo_hop 3 
if wavo_hop != 3 then snd_display sprintf("\n# wavo_hop: %s", wavo_hop) end
set_wavo_trace 128
if wavo_trace !=  128 then snd_display sprintf("\n# wavo_trace: %s", wavo_trace) end
set_wavo_trace 64 
if wavo_trace != 64 then snd_display sprintf("\n# wavo_trace: %s", wavo_trace) end
set_with_mix_tags false
if with_mix_tags !=  false then snd_display sprintf("\n# with_mix_tags: %s", with_mix_tags) end
set_with_mix_tags true 
if with_mix_tags != true then snd_display sprintf("\n# with_mix_tags: %s", with_mix_tags) end
set_x_axis_style 1
if x_axis_style !=  1 then snd_display sprintf("\n# x_axis_style: %s", x_axis_style) end
set_x_axis_style 0 
if x_axis_style != 0 then snd_display sprintf("\n# x_axis_style: %s", x_axis_style) end
set_beats_per_minute 120.0
if fneq(beats_per_minute, 120.0) then snd_display sprintf("\n# beats_per_minute 120: %s", beats_per_minute) end
set_beats_per_minute 30.0 
if fneq(beats_per_minute, 30.0) then snd_display sprintf("\n# beats_per_minute 30: %s", beats_per_minute) end
set_zero_pad 1
if zero_pad !=  1 then snd_display sprintf("\n# zero_pad: %s", zero_pad) end
set_zero_pad 0 
if zero_pad != 0 then snd_display sprintf("\n# zero_pad: %s", zero_pad) end
set_zoom_focus_style 1
if zoom_focus_style != 1 then snd_display sprintf("\n# zoom_focus_style: %s", zoom_focus_style) end
set_zoom_focus_style 2 
if zoom_focus_style != 2 then snd_display sprintf("\n# zoom_focus_style: %s", zoom_focus_style) end

close_sound $oboe


# ---------------------------------------- test 6 ----------------------------------------

v0 = make_vct 10
v1 = make_vct 10
if v0 == 10 then snd_display "v0 is 10?!?" end
vlst = make_vct 3
if not vct?(v0) then snd_display "v0 isn't a vct?!?" end
if vct_length(v0) != 10 then snd_display sprintf("\n#v0 length = %d?", vct_length(v0)) end
vct_fill!(v0, 1.0)
vct_fill!(v1, 0.5)
if v0 == v1 then snd_display sprintf("\n#vct equal? %s %s", v0, v1) end
v2 = v1
v3 = make_vct 10
v4 = make_vct 3
if v1 != v2 then snd_display sprintf("\n#vct not eq? %s %s", v1, v2) end
vct_fill!(v3, 0.5)
if v3 != v1 then snd_display sprintf("\n#vct not equal? %s %s", v3, v1) end
if v4 == v1 then snd_display sprintf("\n#len diff vct equal? %s %s", v4, v1) end
vct_set!(vlst, 1, .1)
if !(vequal(vct2list(vlst), [0.0, 0.1, 0.0])) then snd_display sprintf("\n#vct2list: %s?", vct2list(vlst)) end
vect = [0, 1, 2, 3]
v2 = vector2vct vect
if sprintf("%s", v2) != "#<vct[len=4]: 0.000 1.000 2.000 3.000>" then snd_display sprintf("\n# vector2list->%s", v2) end
v3 = v2
if vct_length(v2) != 4 then snd_display sprintf("\n#vector2vct length: %d?", vct_length(v2)) end
if fneq(vct_ref(v2, 2), 2.0) then snd_display sprintf("\n#vector2vct: %s?", v2) end
vct_move!(v2, 0, 2)
if fneq(vct_ref(v2, 0), 2.0) then snd_display sprintf("\n#vct_move!: %s?", v2) end
v2 = make_vct 4
0.upto(3) do |i|
  vct_set!(v2, i, i)
end
vct_move!(v2, 3, 2, true)
if fneq(vct_ref( v2, 3), 2.0) || fneq(vct_ref(v2, 2), 1.0) then snd_display sprintf("\n# vct_move! back: %s?", v2) end
v0 = make_vct 3
begin
vct_ref(v0, 10)
rescue
  if $!.message[0..8] != "Mus_error" then snd_display sprintf("\n# vct_ref high index: %s", $!) end
end
begin
vct_ref(v0, -1)
rescue
  if $!.message[0..8] != "Mus_error" then snd_display sprintf("\n# vct_ref low index: %s", $!) end
end
begin
vct_set!(v0, 10, 1.0)
rescue
  if $!.message[0..8] != "Mus_error" then snd_display sprintf("\n# vct_set! high index: %s", $!) end
end
begin
vct_set!(v0, -1, 1.0)
rescue
  if $!.message[0..8] != "Mus_error" then snd_display sprintf("\n# vct_set! low index: %s", $!) end
end
begin
vct_move!(v0, 10, 0, true)
rescue
  if $!.message[0..8] != "Mus_error" then snd_display sprintf("\n# vct_move! high index: %s", $!) end
end
begin
vct_move!(v0, 0, 10, true)
rescue
  if $!.message[0..8] != "Mus_error" then snd_display sprintf("\n# vct_move! high 2 index: %s", $!) end
end
begin
vct_move!(v0, -10, 0, false)
rescue
  if $!.message[0..8] != "Mus_error" then snd_display sprintf("\n# vct_move! back high index: %s", $!) end
end
begin
vct_move!(v0, 0, -10, false)
rescue
  if $!.message[0..8] != "Mus_error" then snd_display sprintf("\n# vct_move! back high 2 index: %s", $!) end
end
v = make_vct 4
vv = make_vct 4
ctr = 0
vct_map!(v, Proc.new {|| 
                      ctr = ctr + 1
                      if (ctr < 3) then ctr else 0 end
		     })
if !(vequal(v, vct(1.0, 2.0, 0.0, 0.0))) then snd_display sprintf("\n# vct_map! with symbol: %s", v) end
ctr = 0
v0 = make_vct 10
v1 = make_vct 10
vct_fill!(v0, 1.0)
vct_fill!(v1, 0.5)
0.upto(9) do |i|
  if fneq(vct_ref(v0, i), 1.0) then snd_display sprintf("\n# v0[%d] = %f?", i, vct_ref(v0, i)) end
  if fneq(vct_ref(v1, i), 0.5) then snd_display sprintf("\n# v1[%d] = %f?", i, vct_ref(v1, i)) end
end
vct_add!(v0, v1)
0.upto(9) do |i|
  if fneq(v0[i], 1.5) then snd_display sprintf("\n#add v0[%d] = %f?", i, vct_ref(v0, i)) end
end
vct_subtract!(v0, v1)
0.upto(9) do |i|
  if fneq(vct_ref(v0, i), 1.0) then snd_display sprintf("\n#subtract v0[%d] = %f?", i, v0[i]) end
end
v2 = vct_copy v0
0.upto(9) do |i|
  if fneq(v2[i], 1.0) then snd_display sprintf("\n#copy v0[%d] = %f?", i, v2[i]) end
end
vct_scale!(v2, 5.0)
0.upto(9) do |i|
  if fneq(v2[i], 5.0) then snd_display sprintf("\n#scale v2[%d] = %f?", i, v2[i]) end
end
vct_offset!(v0, -1.0)
0.upto(9) do |i|
  if fneq(v0[i], 0.0) then snd_display sprintf("\n#offset v0[%d] = %f?", i, v0[i]) end
end
vct_multiply!(v2, v1)
0.upto(9) do |i|
  if fneq(v2[i], 2.5) then snd_display sprintf("\n#multiply v2[%d] = %f?", i, v2[i]) end
end
if fneq(vct_peak(v2), 2.5) then snd_display sprintf("\n#v2's peak is %f?", vct_peak(v2)) end
v2[5] = 123.0
if fneq(vct_peak(v2), 123.0) then snd_display sprintf("\n#v2's set peak is %f?", vct_peak(v2)) end
vn = make_vct 32
vb = make_vct 64
vs = make_vct 3
vss = make_vct 1
0.upto(31) do |i|
  vn[i] = i
end
vnew  = vct_subseq(vn, 3)
if fneq(vnew[0], 3.0) then snd_display sprintf("\n#vct_subseq[3:] %s?", vnew[0]) end
if vct_length(vnew) != 29 then snd_display sprintf("\n#vct_subseq[3:] length: %s?", vct_length(vnew)) end
vnew = vct_subseq(vn, 3, 8)
if fneq(vnew[0], 3.0) then snd_display sprintf("\n#vct_subseq[3:8] %s?", vct_ref(vnew, 0)) end
if vct_length(vnew) != 6 then snd_display sprintf("\n#vct_subseq[3:8] length: %s?", vct_length(vnew)) end
vct_subseq(vn, 3, 3, vs)
if fneq(vs[0], 3.0) || fneq(vs[1], 0.0) || fneq(vs[2], 0.0) then snd_display sprintf("\n# vct_subseq[3:32vs] %s?", vs) end
vct_subseq(vn, 0, 32, vs)
if vct_length(vs) != 3 then snd_display sprintf("\n#vct_subseq[0:322vs] length: %s?", vct_length(vs)) end
vct_subseq(vn, 2, 3, vss)
if fneq(vss[0], 2.0) then snd_display sprintf("\n#vct_subseq[2:32vss] %s?", vss[0]) end
vb[8] = 123.0
vct_subseq(vn, 1, 8, vb)
if fneq(vb[0], 1.0) then snd_display sprintf("\n#vct_subseq[1:82vb] %s?", vb[0]) end
if fneq(vb[8], 123.0) then snd_display sprintf("\n#vct_subseq[1:82vb][8] %s?", vb[8]) end
if fneq(vct(1.0, 2.0, 3.0)[1], 2.0) then snd_display sprintf("\n# (vct...) = %s?", vct(1.0, 2.0, 3.0)[1]) end
v1 = vct(1, 2, 3, 4)
if fneq(v1[1], 2.0) then snd_display sprintf("\n# (v1 1) = %s?", v1[1]) end





# from test 9
load "/home/bil/cl/bird.rb"
make_birds
close_sound(find_sound("test.snd"))




def display_energy(snd, chn)
  ls = left_sample
  rs = right_sample
  data1 = make_graph_data(snd, chn)
  data = data1
  if not(vct?(data)) then
    data = data1[1]
  end
  len = vct_length data
  sr = srate snd
  y_max = y_zoom_slider(snd, chn)
  vct_multiply!(data, data)
  graph(data, "energy", ls / sr, rs / sr, 0.0, y_max * y_max, snd, chn, false)
  end

oboe = open_sound "oboe.snd"
save_state "s61.rb"
close_sound oboe
load "s61.rb"
oboe = find_sound "oboe.snd"
if (oboe == false) then snd_print "oops"
else close_sound oboe
end


# from test 25

$snd_widgets = main_widgets()
$shell = $snd_widgets[1]
$dpy = RXtDisplay($shell)
$scr = RDefaultScreenOfDisplay $dpy

if Rheight($scr) != 1200 then snd_display sprintf("\n# screen height: %d", Rheight($scr)) end
if Rwidth($scr) != 1600 then snd_display sprintf("\n# screen width: %d", Rwidth($scr)) end
if Rndepths($scr) != 7 then snd_display sprintf("\n# screen ndepths: %d", Rndepths($scr)) end
if Rwhite_pixel($scr)[1] !=  65535 then snd_display sprintf("\n# screen white_pixel: %d", Rwhite_pixel($scr)[1]) end
if Rblack_pixel($scr)[1] !=  0 then snd_display sprintf("\n# screen black_pixel: %d", Rblack_pixel($scr)[1]) end
if Rbacking_store($scr)  then snd_display sprintf("\n# screen backing_store") end
if Rmin_maps($scr) != 1 then snd_display sprintf("\n# screen min_maps: %d", Rmin_maps($scr)) end
if Rmax_maps($scr) != 1 then snd_display sprintf("\n# screen max_maps: %d", Rmax_maps($scr)) end
if Rsave_unders($scr) then snd_display sprintf("\n# screen save_unders") end
if !RGC? Rdefault_gc($scr) then snd_display sprintf("\n# screen default_gc: %s", Rdefault_gc($scr)) end
if !RWindow? Rroot($scr) then snd_display sprintf("\n# screen root: %s", Rroot($scr)) end
if !RColormap? Rcmap($scr) then snd_display sprintf("\n# screen colormap: %s", Rcmap($scr)) end

if RDisplayOfScreen($scr) != Rdisplay($scr) then snd_display sprintf("\n# DisplayOfScreen: %s %s", RDisplayOfScreen($scr), Rdisplay($scr)) end
if RRootWindowOfScreen($scr) != Rroot($scr) then snd_display sprintf("\n# RootWindowOfScreen: %s %s", RRootWindowOfScreen($scr), Rroot($scr)) end
if RBlackPixelOfScreen($scr) != Rblack_pixel($scr) then snd_display sprintf("\n# BlackPixelOfScreen: %s %s", RBlackPixelOfScreen($scr), Rblack_pixel($scr)) end
if RWhitePixelOfScreen($scr) != Rwhite_pixel($scr) then snd_display sprintf("\n# WhitePixelOfScreen: %s %s", RWhitePixelOfScreen($scr), Rwhite_pixel($scr)) end
if RDefaultColormapOfScreen($scr) != Rcmap($scr) then snd_display sprintf("\n# DefaultColormapOfScreen: %s %s", RDefaultColormapOfScreen($scr), Rcmap($scr)) end
if RDefaultDepthOfScreen($scr) != Rroot_depth($scr) then snd_display sprintf("\n# DefaultDepthOfScreen: %s %s", RDefaultDepthOfScreen($scr), Rroot_depth($scr)) end
if RDefaultGCOfScreen($scr) != Rdefault_gc($scr) then snd_display sprintf("\n# DefaultGCOfScreen: %s %s", RDefaultGCOfScreen($scr), Rdefault_gc($scr)) end
if RDefaultVisualOfScreen($scr) != Rroot_visual($scr) then snd_display sprintf("\n# DefaultVisualOfScreen: %s %s", RDefaultVisualOfScreen($scr), Rroot_visual($scr)) end
if RWidthOfScreen($scr) != Rwidth($scr) then snd_display sprintf("\n# WidthOfScreen: %s %s", RWidthOfScreen($scr), Rwidth($scr)) end
if RHeightOfScreen($scr) != Rheight($scr) then snd_display sprintf("\n# HeightOfScreen: %s %s", RHeightOfScreen($scr), Rheight($scr)) end
if RWidthMMOfScreen($scr) != Rmwidth($scr) then snd_display sprintf("\n# WidthMMOfScreen: %s %s", RWidthMMOfScreen($scr), Rmwidth($scr)) end
if RHeightMMOfScreen($scr) != Rmheight($scr) then snd_display sprintf("\n# HeightMMOfScreen: %s %s", RHeightMMOfScreen($scr), Rmheight($scr)) end
if RPlanesOfScreen($scr) != Rroot_depth($scr) then snd_display sprintf("\n# PlanesOfScreen: %s %s", RPlanesOfScreen($scr), Rroot_depth($scr)) end
if RMinCmapsOfScreen($scr) != Rmin_maps($scr) then snd_display sprintf("\n# MinCmapsOfScreen: %s %s", RMinCmapsOfScreen($scr), Rmin_maps($scr)) end
if RMaxCmapsOfScreen($scr) != Rmax_maps($scr) then snd_display sprintf("\n# MaxCmapsOfScreen: %s %s", RMaxCmapsOfScreen($scr), Rmax_maps($scr)) end
if RDoesSaveUnders($scr) != Rsave_unders($scr) then snd_display sprintf("\n# DoesSaveUnders: %s %s", RDoesSaveUnders($scr), Rsave_unders($scr)) end
if RDoesBackingStore($scr) != Rbacking_store($scr) then snd_display sprintf("\n# DoesBackingStore: %s %s", RDoesBackingStore($scr), Rbacking_store($scr)) end
if REventMaskOfScreen($scr) != Rroot_input_mask($scr) then snd_display sprintf("\n# EventMaskOfScreen: %s %s", REventMaskOfScreen($scr), Rroot_input_mask($scr)) end
if RXDisplayOfScreen($scr) != Rdisplay($scr) then snd_display sprintf("\n# XDisplayOfScreen: %s %s", RXDisplayOfScreen($scr), Rdisplay($scr)) end
if RXRootWindowOfScreen($scr) != Rroot($scr) then snd_display sprintf("\n# XRootWindowOfScreen: %s %s", RXRootWindowOfScreen($scr), Rroot($scr)) end
if RXBlackPixelOfScreen($scr) != Rblack_pixel($scr) then snd_display sprintf("\n# XBlackPixelOfScreen: %s %s", RXBlackPixelOfScreen($scr), Rblack_pixel($scr)) end
if RXWhitePixelOfScreen($scr) != Rwhite_pixel($scr) then snd_display sprintf("\n# XWhitePixelOfScreen: %s %s", RXWhitePixelOfScreen($scr), Rwhite_pixel($scr)) end
if RXDefaultColormapOfScreen($scr) != Rcmap($scr) then snd_display sprintf("\n# XDefaultColormapOfScreen: %s %s", RXDefaultColormapOfScreen($scr), Rcmap($scr)) end
if RXDefaultDepthOfScreen($scr) != Rroot_depth($scr) then snd_display sprintf("\n# XDefaultDepthOfScreen: %s %s", RXDefaultDepthOfScreen($scr), Rroot_depth($scr)) end
if RXDefaultGCOfScreen($scr) != Rdefault_gc($scr) then snd_display sprintf("\n# XDefaultGCOfScreen: %s %s", RXDefaultGCOfScreen($scr), Rdefault_gc($scr)) end
if RXDefaultVisualOfScreen($scr) != Rroot_visual($scr) then snd_display sprintf("\n# XDefaultVisualOfScreen: %s %s", RXDefaultVisualOfScreen($scr), Rroot_visual($scr)) end
if RXWidthOfScreen($scr) != Rwidth($scr) then snd_display sprintf("\n# XWidthOfScreen: %s %s", RXWidthOfScreen($scr), Rwidth($scr)) end
if RXHeightOfScreen($scr) != Rheight($scr) then snd_display sprintf("\n# XHeightOfScreen: %s %s", RXHeightOfScreen($scr), Rheight($scr)) end
if RXWidthMMOfScreen($scr) != Rmwidth($scr) then snd_display sprintf("\n# XWidthMMOfScreen: %s %s", RXWidthMMOfScreen($scr), Rmwidth($scr)) end
if RXHeightMMOfScreen($scr) != Rmheight($scr) then snd_display sprintf("\n# XHeightMMOfScreen: %s %s", RXHeightMMOfScreen($scr), Rmheight($scr)) end
if RXPlanesOfScreen($scr) != Rroot_depth($scr) then snd_display sprintf("\n# XPlanesOfScreen: %s %s", RXPlanesOfScreen($scr), Rroot_depth($scr)) end
if RXMinCmapsOfScreen($scr) != Rmin_maps($scr) then snd_display sprintf("\n# XMinCmapsOfScreen: %s %s", RXMinCmapsOfScreen($scr), Rmin_maps($scr)) end
if RXMaxCmapsOfScreen($scr) != Rmax_maps($scr) then snd_display sprintf("\n# XMaxCmapsOfScreen: %s %s", RXMaxCmapsOfScreen($scr), Rmax_maps($scr)) end
if RXDoesSaveUnders($scr) != Rsave_unders($scr) then snd_display sprintf("\n# XDoesSaveUnders: %s %s", RXDoesSaveUnders($scr), Rsave_unders($scr)) end
if RXDoesBackingStore($scr) != Rbacking_store($scr) then snd_display sprintf("\n# XDoesBackingStore: %s %s", RXDoesBackingStore($scr), Rbacking_store($scr)) end
if RXEventMaskOfScreen($scr) != Rroot_input_mask($scr) then snd_display sprintf("\n# XEventMaskOfScreen: %s %s", RXEventMaskOfScreen($scr), Rroot_input_mask($scr)) end

exit
