# snd_test.rb: Snd Ruby code and tests

def provided?(feature) feature == $".find{|obj| obj == feature} end
# not sure this is kosher in Ruby -- I'm using $" via rb_provided as a feature list, but doc says its a filename array

def snd_display(str) 
  snd_print str
  $stderr.write str
end

def fneq(a, b)
  (a - b).abs > 0.001
end

def ffneq(a, b)
  (a - b).abs > 0.01
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
if Hadamard_transform != 5 then snd_display sprintf("\n# Hadamard_transform => %d", Hadamard_transform) end
if Haar_transform != 6 then snd_display sprintf("\n# Haar_transform => %d", Haar_transform) end
if Hamming_window != 5 then snd_display sprintf("\n# Hamming_window => %d", Hamming_window) end
if Hann_window != 1 then snd_display sprintf("\n# Hann_window => %d", Hann_window) end
if Kaiser_window != 11 then snd_display sprintf("\n# Kaiser_window => %d", Kaiser_window) end
if Keyboard_no_action != 4 then snd_display sprintf("\n# Keyboard_no_action => %d", Keyboard_no_action) end
if Cepstrum != 4 then snd_display sprintf("\n# Cepstrum => %d", Cepstrum) end
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
if X_axis_in_beats != 3 then snd_display sprintf("\n# X_axis_in_beats => %d", X_axis_in_beats) end
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
if Mus_next != 1 then snd_display sprintf("\n# Mus_next => %d", Mus_next) end
if Mus_aifc != 2 then snd_display sprintf("\n# Mus_aifc => %d", Mus_aifc) end
if Mus_riff != 3 then snd_display sprintf("\n# Mus_riff => %d", Mus_riff) end
if Mus_nist != 5 then snd_display sprintf("\n# Mus_nist => %d", Mus_nist) end
if Mus_raw != 11 then snd_display sprintf("\n# Mus_raw => %d", Mus_raw) end
if Mus_ircam != 14 then snd_display sprintf("\n# Mus_ircam => %d", Mus_ircam) end
if Mus_aiff != 48 then snd_display sprintf("\n# Mus_aiff => %d", Mus_aiff) end
if Mus_bicsf != 4 then snd_display sprintf("\n# Mus_bicsf => %d", Mus_bicsf) end
if Mus_voc != 9 then snd_display sprintf("\n# Mus_voc => %d", Mus_voc) end
if Mus_svx != 8 then snd_display sprintf("\n# Mus_svx => %d", Mus_svx) end
if Mus_soundfont != 25 then snd_display sprintf("\n# Mus_soundfont => %d", Mus_soundfont) end
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
if Mus_bfloat_unscaled != 19 then snd_display sprintf("\n# Mus_bfloat_unscaled => %d", Mus_bfloat_unscaled) end
if Mus_lfloat_unscaled != 20 then snd_display sprintf("\n# Mus_lfloat_unscaled => %d", Mus_lfloat_unscaled) end
if Mus_bdouble_unscaled != 21 then snd_display sprintf("\n# Mus_bdouble_unscaled => %d", Mus_bdouble_unscaled) end
if Mus_ldouble_unscaled != 22 then snd_display sprintf("\n# Mus_ldouble_unscaled => %d", Mus_ldouble_unscaled) end
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
if auto_resize != true then snd_display sprintf("\n# auto_resize: %s", auto_resize) end
if auto_update != false then snd_display sprintf("\n# auto_update: %s", auto_update) end
if channel_style != 0 then snd_display sprintf("\n# channel_style: %s", channel_style) end
if fneq(color_cutoff, 0.003) then snd_display sprintf("\n# color_cutoff: %s", color_cutoff) end
if color_inverted != true then snd_display sprintf("\n# color_inverted: %s", color_inverted) end
if color_scale != 1.0 then snd_display sprintf("\n# color_scale: %s", color_scale) end
if colormap != 0 then snd_display sprintf("\n# colormap: %s", colormap) end
if auto_update_interval != 60.0 then snd_display sprintf("\n# auto_update_interval: %s", auto_update_interval) end
if dac_combines_channels != true then snd_display sprintf("\n# dac_combines_channels: %s", dac_combines_channels) end
if emacs_style_save_as != false then snd_display sprintf("\n# emacs_style_save_as: %s", emacs_style_save_as) end
if dac_size != 256 then snd_display sprintf("\n# dac_size: %s", dac_size) end
if minibuffer_history_length != 8 then snd_display sprintf("\n# minibuffer_history_length: %s", minibuffer_history_length) end
if data_clipped != false then snd_display sprintf("\n# data_clipped: %s", data_clipped) end
if default_output_chans != 1 then snd_display sprintf("\n# default_output_chans: %s", default_output_chans) end
if default_output_format != 1 then snd_display sprintf("\n# default_output_format: %s", default_output_format) end
if default_output_srate != 22050 then snd_display sprintf("\n# default_output_srate: %s", default_output_srate) end
if default_output_type != 1 then snd_display sprintf("\n# default_output_type: %s", default_output_type) end
if dot_size != 1 then snd_display sprintf("\n# dot_size: %s", dot_size) end
if enved_base != 1.0 then snd_display sprintf("\n# enved_base: %s", enved_base) end
if enved_clip? != false then snd_display sprintf("\n# enved_clip?: %s", enved_clip?) end
if enved_filter_order != 40 then snd_display sprintf("\n# enved_filter_order: %s", enved_filter_order) end
if enved_filter != true then snd_display sprintf("\n# enved_filter: %s", enved_filter) end
if enved_in_dB != false then snd_display sprintf("\n# enved_in_dB: %s", enved_in_dB) end
if enved_style != Envelope_linear then snd_display sprintf("\n# enved_style: %s", enved_style) end
if enved_power != 3.0 then snd_display sprintf("\n# enved_power: %s", enved_power) end
if enved_target != 0 then snd_display sprintf("\n# enved_target: %s", enved_target) end
if enved_wave? != false then snd_display sprintf("\n# enved_wave?: %s", enved_wave?) end
if enved_envelope != nil then snd_display sprintf("\n# enved_envelope: %s", enved_envelope) end
if eps_file != "snd.eps" then snd_display sprintf("\n# eps_file: %s", eps_file) end
if eps_bottom_margin != 0.0 then snd_display sprintf("\n# eps_bottom_margin: %s", eps_bottom_margin) end
if eps_left_margin != 0.0 then snd_display sprintf("\n# eps_left_margin: %s", eps_left_margin) end
if eps_size != 1.0 then snd_display sprintf("\n# eps_size: %s", eps_size) end
if fft_window_beta != 0.0 then snd_display sprintf("\n# fft_window_beta: %s", fft_window_beta) end
if fft_log_frequency != false then snd_display sprintf("\n# fft_log_frequency: %s", fft_log_frequency) end
if fft_log_magnitude != false then snd_display sprintf("\n# fft_log_magnitude: %s", fft_log_magnitude) end
if transform_size != 512 then snd_display sprintf("\n# transform_size: %s", transform_size) end
if transform_graph_type != 0 then snd_display sprintf("\n# transform_graph_type: %s", transform_graph_type) end
if fft_window != 6 then snd_display sprintf("\n# fft_window: %s", fft_window) end
if graph_cursor != 34 then snd_display sprintf("\n# graph_cursor: %s", graph_cursor) end
if graph_style != 0 then snd_display sprintf("\n# graph_style: %s", graph_style) end
if graphs_horizontal != true then snd_display sprintf("\n# graphs_horizontal: %s", graphs_horizontal) end
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
# test_headers("Pnossnd.aif", 1, 8000, 0.0, "AIFC", "mulaw (8 bits)")
test_headers("Poffset.aif", 1, 8000, 0.00112499995157123, "AIFC", "mulaw (8 bits)")
test_headers("Porder.aif", 1, 8000, 0.00112499995157123, "AIFC", "mulaw (8 bits)")
test_headers("Ptjunk.aif", 1, 8000, 0.00112499995157123, "AIFC", "mulaw (8 bits)")
test_headers("Ptjunk.wav", 1, 8000, 0.00112499995157123, "RIFF", "mulaw (8 bits)")
test_headers("SINE24-S.WAV", 2, 44100, 2.0, "RIFF", "little endian int (24 bits)")
test_headers("a1.asf", 1, 16000, 0.0, "asf", "unknown")
test_headers("a2.asf", 1, 8000, 0.0, "asf", "unknown")
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
test_headers("c.asf", 1, 8000, 0.0, "asf", "unknown")
test_headers("ce-c3.w02", 1, 33000, 3.88848495483398, "TX-16W", "unknown")
test_headers("ce-c4.w03", 1, 33000, 2.91618180274963, "TX-16W", "unknown")
test_headers("ce-d2.w01", 1, 33000, 3.46439385414124, "TX-16W", "unknown")
test_headers("clbonef.wav", 1, 22050, 2.57832193374634, "RIFF", "little endian float (32 bits)")
test_headers("cranker.krz", 1, 44100, 3.48267579078674, "Kurzweil 2000", "big endian short (16 bits)")
test_headers("d40130.aif", 1, 10000, 0.100000001490116, "AIFF", "big endian short (16 bits)")
test_headers("d40130.au", 1, 10000, 0.100000001490116, "Sun", "big endian short (16 bits)")
test_headers("d40130.dsf", 1, 8000, 0.125, "Delusion", "little endian short (16 bits)")
test_headers("d40130.fsm", 1, 8000, 0.125249996781349, "Farandole", "little endian short (16 bits)")
test_headers("d40130.iff", 1, 10000, 0.100000001490116, "SVX8", "signed byte (8 bits)")
test_headers("d40130.pat", 1, 10000, 0.100000001490116, "Gravis Ultrasound patch", "little endian short (16 bits)")
test_headers("d40130.sds", 1, 10000, 0.100000001490116, "MIDI sample dump", "unknown")
test_headers("d40130.sdx", 1, 10000, 0.100000001490116, "Sample dump", "unsigned little endian short (16 bits)")
test_headers("d40130.sf", 1, 10000, 0.100000001490116, "IRCAM", "little endian short (16 bits)")
test_headers("d40130.smp", 1, 8000, 0.125, "SMP", "little endian short (16 bits)")
test_headers("d40130.sou", 1, 8000, 0.125, "SBStudioII", "little endian short (16 bits)")
test_headers("d40130.st3", 1, 8000, 0.125, "Digiplayer ST3", "unsigned little endian short (16 bits)")
test_headers("d40130.uwf", 1, 8000, 0.125249996781349, "Ultratracker", "little endian short (16 bits)")
test_headers("d40130.voc", 1, 10000, 0.100100003182888, "VOC", "unsigned byte (8 bits)")
test_headers("d40130.w00", 1, 16000, 0.0625, "TX-16W", "unknown")
test_headers("d40130.wav", 1, 10000, 0.100000001490116, "RIFF", "little endian short (16 bits)")
test_headers("d43.wav", 1, 10000, 0.100000001490116, "RIFF", "little endian short (16 bits)")
test_headers("digit0v0.aiff", 1, 8000, 0.560000002384186, "AIFC", "big endian short (16 bits)")
test_headers("esps-16.snd", 1, 8000, 3.09737491607666, "ESPS", "big endian short (16 bits)")
test_headers("forest.aiff", 2, 44100, 3.907143, "AIFF", "big endian short (16 bits)")
test_headers("g721.au", 1, 11025, 4.35328817367554, "Sun", "unknown")
test_headers("g722.aifc", 1, 44100, 0.0184353739023209, "AIFC", "unknown")
test_headers("gong.wve", 1, 8000, 3.96799993515015, "PSION", "alaw (8 bits)")
test_headers("gsm610.wav", 1, 11025, 1.7687075138092, "RIFF", "unknown")
test_headers("inrs-16.snd", 1, 8000, 2.46399998664856, "INRS", "little endian short (16 bits)")
test_headers("kirk.wve", 1, 8000, 1.40799999237061, "PSION", "alaw (8 bits)")
test_headers("loop.aiff", 1, 44100, 0.0367120169103146, "AIFC", "big endian short (16 bits)")
test_headers("m.asf", 1, 8000, 0.0, "asf", "unknown")
# test_headers("mary-sun4.sig", 1, 8000, 5.95137500762939, "Comdisco SPW signal", "big endian double (64 bits)")
test_headers("mocksong.wav", 1, 11025, 7.86956930160522, "RIFF", "little endian short (16 bits)")
test_headers("mono24.wav", 1, 22050, 1.98997735977173, "RIFF", "little endian int (24 bits)")
test_headers("msadpcm.wav", 1, 11025, 4.43501138687134, "RIFF", "unknown")
test_headers("n8.snd", 1, 44100, 0.0367800444364548, "Sun", "signed byte (8 bits)")
test_headers("nasahal.aif", 1, 11025, 9.89841270446777, "AIFF", "signed byte (8 bits)")
test_headers("nasahal.avi", 1, 11025, 0.0, "AVI", "unknown")
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
test_headers("nist-shortpack.wav", 1, 16000, 4.53824996948242, "NIST", "unknown")
test_headers("none.aifc", 1, 44100, 0.0367800444364548, "AIFC", "big endian short (16 bits)")
test_headers("nylon2.wav", 2, 22050, 1.14376413822174, "RIFF", "unknown")
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
test_headers("o2_722.snd", 1, 44100, 0.0183900222182274, "Sun", "unknown")
test_headers("o2_726.aiff", 1, 8000, 0.0367499999701977, "AIFC", "unknown")
test_headers("o2_726.snd", 1, 44100, 0.0230158735066652, "Sun", "unknown")
test_headers("o2_728.aiff", 1, 8000, 0.0367499999701977, "AIFC", "unknown")
test_headers("o2_8.iff", 1, 44100, 0.0367800444364548, "SVX8", "signed byte (8 bits)")
test_headers("o2_8.voc", 1, 44100, 0.0370294786989689, "VOC", "unsigned byte (8 bits)")
test_headers("o2_dvi.wave", 1, 44100, 0.0232199542224407, "RIFF", "unknown")
test_headers("o2_float.bicsf", 1, 44100, 0.0367800444364548, "IRCAM", "big endian float (32 bits)")
test_headers("o2_gsm.aiff", 1, 8000, 0.0367499999701977, "AIFC", "unknown")
test_headers("o2_u8.avr", 1, 44100, 0.0367800444364548, "AVR", "unsigned byte (8 bits)")
test_headers("o2_u8.wave", 1, 44100, 0.0367800444364548, "RIFF", "unsigned byte (8 bits)")
test_headers("oboe.g721", 1, 22050, 1.15287983417511, "Sun", "unknown")
test_headers("oboe.g723_24", 1, 22050, 0.864761888980865, "Sun", "unknown")
test_headers("oboe.g723_40", 1, 22050, 1.44126987457275, "Sun", "unknown")
test_headers("oboe.sf2", 1, 22050, 2.30512475967407, "SoundFont", "little endian short (16 bits)")
test_headers("oboe.paf", 1, 22050, 2.305125, "Ensoniq Paris", "big endian short (16 bits)")
test_headers("oboe.smp", 1, 22050, 2.305125, "snack SMP", "little endian short (16 bits)")
test_headers("oboe.nsp", 1, 22050, 2.305125, "CSL", "little endian short (16 bits)")
test_headers("oki.snd", 2, 44100, 0.0041950112208724, "raw (no header)", "big endian short (16 bits)")
test_headers("orv-dvi-adpcm.wav", 1, 44100, 1.92725622653961, "RIFF", "unknown")
test_headers("riff-16.snd", 1, 22050, 1.88766443729401, "RIFF", "little endian short (16 bits)")
test_headers("riff-8-u.snd", 1, 11025, 0.506848096847534, "RIFF", "unsigned byte (8 bits)")
test_headers("rooster.wve", 1, 8000, 2.04800009727478, "PSION", "alaw (8 bits)")
test_headers("sd1-16.snd", 1, 44100, 0.400544226169586, "Sound Designer 1", "big endian short (16 bits)")
# test_headers("segfault.snd", 16777216, 576061440, 1.24986669902682e-7, "Sun", "unknown")
test_headers("sf-16.snd", 1, 22050, 1.88766443729401, "IRCAM", "big endian short (16 bits)")
test_headers("si654.adc", 1, 16000, 6.71362495422363, "ADC/OGI", "big endian short (16 bits)")
test_headers("smp-16.snd", 1, 8000, 5.2028751373291, "SMP", "little endian short (16 bits)")
test_headers("sound.pat", 1, 8000, 1.95050001144409, "Gravis Ultrasound patch", "unsigned little endian short (16 bits)")
test_headers("sound.sap", 1, 8000, 1.95050001144409, "Goldwave sample", "little endian short (16 bits)")
test_headers("sound.sds", 1, 8000, 1.95050001144409, "MIDI sample dump", "unknown")
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
test_headers("truspech.wav", 1, 8000, 1.1599999666214, "RIFF", "unknown")
test_headers("ulaw.aifc", 1, 44100, 0.0367800444364548, "AIFC", "mulaw (8 bits)")
test_headers("voc-8-u.snd", 1, 8000, 1.49937498569489, "VOC", "unsigned byte (8 bits)")
test_headers("voxware.wav", 1, 8000, 0.324000000953674, "RIFF", "unknown")
test_headers("wd.w00", 1, 8000, 0.202749997377396, "Sy-99", "big endian short (16 bits)")
test_headers("wd1.smp", 1, 8000, 0.202749997377396, "SMP", "little endian short (16 bits)")
test_headers("wd1.wav", 1, 44100, 0.0367800444364548, "RIFF", "little endian short (16 bits)")
test_headers("wheel.mat", 2, 44100, 0.145646259188652, "raw (no header)", "big endian short (16 bits)")
test_headers("b8.pvf", 1, 44100, 0.036803, "Portable Voice Format", "signed byte (8 bits)")
test_headers("b16.pvf", 1, 44100, 0.036803, "Portable Voice Format", "big endian short (16 bits)")
test_headers("b32.pvf", 1, 44100, 0.036803, "Portable Voice Format", "big endian int (32 bits)")
test_headers("wood.dsf", 1, 8000, 0.202749997377396, "Delusion", "little endian short (16 bits)")
test_headers("wood.dvi", 1, 22100, 0.0278733037412167, "RIFF", "unknown")
test_headers("wood.dwd", 1, 22100, 0.0733936652541161, "DiamondWare", "signed byte (8 bits)")
test_headers("wood.fsm", 1, 8000, 0.202999994158745, "Farandole", "little endian short (16 bits)")
test_headers("wood.mad", 1, 22100, 0.0372398197650909, "RIFF", "unknown")
test_headers("wood.maud", 1, 44100, 0.0183900222182274, "MAUD", "big endian short (16 bits)")
test_headers("wood.pat", 1, 22100, 0.0733936652541161, "Gravis Ultrasound patch", "little endian short (16 bits)")
test_headers("wood.riff", 1, 44100, 0.0367800444364548, "RIFF", "little endian short (16 bits)")
test_headers("wood.rifx", 1, 44100, 0.0367800444364548, "RIFF", "big endian short (16 bits)")
test_headers("wood.sds", 1, 22100, 0.0733936652541161, "MIDI sample dump", "unknown")
test_headers("wood.sdx", 1, 22100, 0.0733936652541161, "Sample dump", "unsigned little endian short (16 bits)")
test_headers("wood.sf", 1, 44100, 0.0367800444364548, "IRCAM", "big endian short (16 bits)")
test_headers("wood.sndr", 2, 44100, 0.0092290248721838, "raw (no header)", "big endian short (16 bits)")
test_headers("wood.sndt", 1, 44100, 0.0367800444364548, "SNDT", "unsigned byte (8 bits)")
test_headers("wood.st3", 1, 8000, 0.202749997377396, "Digiplayer ST3", "unsigned little endian short (16 bits)")
test_headers("wood.uwf", 1, 8000, 0.202999994158745, "Ultratracker", "little endian short (16 bits)")
test_headers("wood.w00", 1, 16000, 0.101374998688698, "TX-16W", "unknown")
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
test_headers("zulu_a4.w11", 1, 33000, 1.21987879276276, "TX-16W", "unknown")

# ---------------------------------------- test 3 ----------------------------------------

snd_display "test 3"
$oboe = open_sound "oboe.snd"
$td = temp_dir     
if (sample(1000) - 0.0328).abs > 0.001 then snd_print sprintf("# sample: %s\n", sample(1000)); end
set_show_controls true
if not provided? "snd-nogui" then
  enved_dialog
  if !dialog_widgets[2] then snd_print sprintf("# enved_dialog?") end

#	    (set! (enved_envelope) '(0.0 0.0 1.0 1.0 2.0 0.0))
#	    (if (not (equal? (enved_envelope) (list 0.0 0.0 1.0 1.0 2.0 0.0)))
#		(snd_display ";set enved_envelope: ~A?" (enved_envelope)))
#   
  orientation_dialog
  if !dialog_widgets[1] then snd_print sprintf("# orientation_dialog?") end
end

set_amp_control 0.5
if ffneq(amp_control, 0.5) then snd_display sprintf("\n# amp_control: %s", amp_control) end
set_amp_control 1.0
if fneq(amp_control, 1.0) then snd_display sprintf("\n# init amp_control: %s", amp_control) end
set_ask_before_overwrite true
if ask_before_overwrite !=  true then snd_display sprintf("\n# ask_before_overwrite: %s", ask_before_overwrite) end
set_ask_before_overwrite false 
if ask_before_overwrite != false then snd_display sprintf("\n# ask_before_overwrite: %s", ask_before_overwrite) end
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
set_default_output_type Mus_riff
if default_output_type !=  Mus_riff then snd_display sprintf("\n# default_output_type: %s", default_output_type) end
set_default_output_type Mus_next
if default_output_type != Mus_next then snd_display sprintf("\n# default_output_type: %s", default_output_type) end
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
set_enved_style Envelope_exponential
if enved_style !=  Envelope_exponential then snd_display sprintf("\n# enved_style: %s", enved_style) end
set_enved_style Envelope_linear
if enved_style != Envelope_linear then snd_display sprintf("\n# enved_style: %s", enved_style) end
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
set_expand_control_jitter 0.2
if fneq(expand_control_jitter, 0.2) then snd_display sprintf("\n# expand_control_jitter: %s", expand_control_jitter) end
set_expand_control_jitter 0.1
if fneq(expand_control_jitter, 0.1) then snd_display sprintf("\n# expand_control_jitter: %s", expand_control_jitter) end
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
set_filter_control_in_hz true
if filter_control_in_hz !=  true then snd_display sprintf("\n# filter_control_in_hz: %s", filter_control_in_hz) end
set_filter_control_in_hz false 
if filter_control_in_hz != false then snd_display sprintf("\n# filter_control_in_hz: %s", filter_control_in_hz) end
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

snd_display "test 6"
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
vct_set!(vlst, 1, 0.1)
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
snd_display "test 9"
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
set_sound_properties([1234], oboe)
save_state "s61.rb"
close_sound oboe
load "s61.rb"
oboe = find_sound "oboe.snd"
if (oboe == false) then snd_print "oops"
else 
  if ((sound_properties(oboe) == nil) || (sound_properties(oboe)[0] != 1234)) 
    then snd_print "prop bad"
  end
  close_sound oboe
end

# these taken from Michael Scholz's examples
# load "examp.rb"
# with_sound { fm_violin }
# oboe = find_sound "test.snd"
# if (oboe == false) 
#   then message("can't find test.snd")
# else
#   if (frames(oboe) != 22051) then message("test.snd bad len") end
#   close_sound oboe
# end
# 
# with_sound(:play, 1,
# 	   :channels, 2,
# 	   :scaled_to, 0.3,
# 	   :reverb, :jc_reverb,
# 	   :statistics, true) { 
#   0.upto(2) { |i| 
#     metalamp = [0, 0, 0.5, 1, 5, 1, 10, 0.5, 15, 0.25, 35, 0.1, 100, 0]
# 
#     fm_violin(i * 0.1, 1, 220 + i * 10, 0.1, 
# 	      :fm_index, i * 0.5, :distance, i * 0.05, :amp_env, metalamp)
# 
#     fm_violin(i * 0.1, 1, 2200 - i * 10, 0.1, 
# 	      :fm_index, i * 0.5, :distance, i * -0.05, :amp_env, metalamp)
#   }
# }
# oboe = find_sound "test.snd"
# if (oboe == false) 
#   then message("can't find test.snd(2)")
# else
#   if (frames(oboe) != 48512) then message("test.snd(2) bad len") end
#   close_sound oboe
# end

if not provided? "snd-motif" then
  exit
end
if not provided? "xm" then
  exit
end

# from test 25
snd_display "test 25"
$snd_widgets = main_widgets()
$shell = $snd_widgets[1]
$dpy = RXtDisplay($shell)
$scr = RDefaultScreenOfDisplay $dpy

if Rheight($scr) != 1200 then snd_display sprintf("\n# screen height: %d", Rheight($scr)) end
if Rwidth($scr) != 1600 then snd_display sprintf("\n# screen width: %d", Rwidth($scr)) end
if Rndepths($scr) != 7 then snd_display sprintf("\n# screen ndepths: %d", Rndepths($scr)) end
if Rblack_pixel($scr)[1] !=  0 then snd_display sprintf("\n# screen black_pixel: %d", Rblack_pixel($scr)[1]) end
if Rbacking_store($scr)  then snd_display sprintf("\n# screen backing_store") end
if Rmin_maps($scr) != 1 then snd_display sprintf("\n# screen min_maps: %d", Rmin_maps($scr)) end
if Rmax_maps($scr) != 1 then snd_display sprintf("\n# screen max_maps: %d", Rmax_maps($scr)) end
if Rsave_unders($scr) then snd_display sprintf("\n# screen save_unders") end
if !RGC? Rdefault_gc($scr) then snd_display sprintf("\n# screen default_gc: %s", Rdefault_gc($scr)) end
if !RWindow? Rroot($scr) then snd_display sprintf("\n# screen root: %s", Rroot($scr)) end
if !RColormap? Rcmap($scr) then snd_display sprintf("\n# screen colormap: %s", Rcmap($scr)) end

if RDisplayOfScreen($scr) != Rdisplay($scr) 
  then snd_display sprintf("\n# DisplayOfScreen: %s %s", RDisplayOfScreen($scr), Rdisplay($scr)) 
end
if RRootWindowOfScreen($scr) != Rroot($scr) 
  then snd_display sprintf("\n# RootWindowOfScreen: %s %s", RRootWindowOfScreen($scr), Rroot($scr)) 
end
if RBlackPixelOfScreen($scr) != Rblack_pixel($scr) 
  then snd_display sprintf("\n# BlackPixelOfScreen: %s %s", RBlackPixelOfScreen($scr), Rblack_pixel($scr)) 
end
if RWhitePixelOfScreen($scr) != Rwhite_pixel($scr) 
  then snd_display sprintf("\n# WhitePixelOfScreen: %s %s", RWhitePixelOfScreen($scr), Rwhite_pixel($scr)) 
end
if RDefaultColormapOfScreen($scr) != Rcmap($scr) 
  then snd_display sprintf("\n# DefaultColormapOfScreen: %s %s", RDefaultColormapOfScreen($scr), Rcmap($scr)) 
end
if RDefaultDepthOfScreen($scr) != Rroot_depth($scr) 
  then snd_display sprintf("\n# DefaultDepthOfScreen: %s %s", RDefaultDepthOfScreen($scr), Rroot_depth($scr)) 
end
if RDefaultGCOfScreen($scr) != Rdefault_gc($scr) 
  then snd_display sprintf("\n# DefaultGCOfScreen: %s %s", RDefaultGCOfScreen($scr), Rdefault_gc($scr)) 
end
if RDefaultVisualOfScreen($scr) != Rroot_visual($scr) 
  then snd_display sprintf("\n# DefaultVisualOfScreen: %s %s", RDefaultVisualOfScreen($scr), Rroot_visual($scr)) 
end
if RWidthOfScreen($scr) != Rwidth($scr) 
  then snd_display sprintf("\n# WidthOfScreen: %s %s", RWidthOfScreen($scr), Rwidth($scr)) 
end
if RHeightOfScreen($scr) != Rheight($scr) 
  then snd_display sprintf("\n# HeightOfScreen: %s %s", RHeightOfScreen($scr), Rheight($scr)) 
end
if RWidthMMOfScreen($scr) != Rmwidth($scr) 
  then snd_display sprintf("\n# WidthMMOfScreen: %s %s", RWidthMMOfScreen($scr), Rmwidth($scr)) 
end
if RHeightMMOfScreen($scr) != Rmheight($scr) 
  then snd_display sprintf("\n# HeightMMOfScreen: %s %s", RHeightMMOfScreen($scr), Rmheight($scr)) 
end
if RPlanesOfScreen($scr) != Rroot_depth($scr) 
  then snd_display sprintf("\n# PlanesOfScreen: %s %s", RPlanesOfScreen($scr), Rroot_depth($scr)) 
end
if RMinCmapsOfScreen($scr) != Rmin_maps($scr) 
  then snd_display sprintf("\n# MinCmapsOfScreen: %s %s", RMinCmapsOfScreen($scr), Rmin_maps($scr)) 
end
if RMaxCmapsOfScreen($scr) != Rmax_maps($scr) 
  then snd_display sprintf("\n# MaxCmapsOfScreen: %s %s", RMaxCmapsOfScreen($scr), Rmax_maps($scr)) 
end
if RDoesSaveUnders($scr) != Rsave_unders($scr) 
  then snd_display sprintf("\n# DoesSaveUnders: %s %s", RDoesSaveUnders($scr), Rsave_unders($scr)) 
end
if RDoesBackingStore($scr) != Rbacking_store($scr) 
  then snd_display sprintf("\n# DoesBackingStore: %s %s", RDoesBackingStore($scr), Rbacking_store($scr)) 
end
if REventMaskOfScreen($scr) != Rroot_input_mask($scr) 
  then snd_display sprintf("\n# EventMaskOfScreen: %s %s", REventMaskOfScreen($scr), Rroot_input_mask($scr)) 
end
if RXDisplayOfScreen($scr) != Rdisplay($scr) then snd_display sprintf("\n# XDisplayOfScreen: %s %s", RXDisplayOfScreen($scr), Rdisplay($scr)) 
end
if RXRootWindowOfScreen($scr) != Rroot($scr) then snd_display sprintf("\n# XRootWindowOfScreen: %s %s", RXRootWindowOfScreen($scr), Rroot($scr)) 
end
if RXBlackPixelOfScreen($scr) != Rblack_pixel($scr) 
  then snd_display sprintf("\n# XBlackPixelOfScreen: %s %s", RXBlackPixelOfScreen($scr), Rblack_pixel($scr)) 
end
if RXWhitePixelOfScreen($scr) != Rwhite_pixel($scr) 
  then snd_display sprintf("\n# XWhitePixelOfScreen: %s %s", RXWhitePixelOfScreen($scr), Rwhite_pixel($scr)) 
end
if RXDefaultColormapOfScreen($scr) != Rcmap($scr) 
  then snd_display sprintf("\n# XDefaultColormapOfScreen: %s %s", RXDefaultColormapOfScreen($scr), Rcmap($scr)) 
end
if RXDefaultDepthOfScreen($scr) != Rroot_depth($scr) 
  then snd_display sprintf("\n# XDefaultDepthOfScreen: %s %s", RXDefaultDepthOfScreen($scr), Rroot_depth($scr)) 
end
if RXDefaultGCOfScreen($scr) != Rdefault_gc($scr) 
  then snd_display sprintf("\n# XDefaultGCOfScreen: %s %s", RXDefaultGCOfScreen($scr), Rdefault_gc($scr)) 
end
if RXDefaultVisualOfScreen($scr) != Rroot_visual($scr) 
  then snd_display sprintf("\n# XDefaultVisualOfScreen: %s %s", RXDefaultVisualOfScreen($scr), Rroot_visual($scr)) 
end
if RXWidthOfScreen($scr) != Rwidth($scr) 
  then snd_display sprintf("\n# XWidthOfScreen: %s %s", RXWidthOfScreen($scr), Rwidth($scr)) 
end
if RXHeightOfScreen($scr) != Rheight($scr) 
  then snd_display sprintf("\n# XHeightOfScreen: %s %s", RXHeightOfScreen($scr), Rheight($scr)) 
end
if RXWidthMMOfScreen($scr) != Rmwidth($scr) 
  then snd_display sprintf("\n# XWidthMMOfScreen: %s %s", RXWidthMMOfScreen($scr), Rmwidth($scr)) 
end
if RXHeightMMOfScreen($scr) != Rmheight($scr) 
  then snd_display sprintf("\n# XHeightMMOfScreen: %s %s", RXHeightMMOfScreen($scr), Rmheight($scr)) 
end
if RXPlanesOfScreen($scr) != Rroot_depth($scr) 
  then snd_display sprintf("\n# XPlanesOfScreen: %s %s", RXPlanesOfScreen($scr), Rroot_depth($scr)) 
end
if RXMinCmapsOfScreen($scr) != Rmin_maps($scr) 
  then snd_display sprintf("\n# XMinCmapsOfScreen: %s %s", RXMinCmapsOfScreen($scr), Rmin_maps($scr))
end
if RXMaxCmapsOfScreen($scr) != Rmax_maps($scr) 
  then snd_display sprintf("\n# XMaxCmapsOfScreen: %s %s", RXMaxCmapsOfScreen($scr), Rmax_maps($scr)) 
end
if RXDoesSaveUnders($scr) != Rsave_unders($scr) 
  then snd_display sprintf("\n# XDoesSaveUnders: %s %s", RXDoesSaveUnders($scr), Rsave_unders($scr)) 
end
if RXDoesBackingStore($scr) != Rbacking_store($scr) 
  then snd_display sprintf("\n# XDoesBackingStore: %s %s", RXDoesBackingStore($scr), Rbacking_store($scr)) 
end
if RXEventMaskOfScreen($scr) != Rroot_input_mask($scr) 
  then snd_display sprintf("\n# XEventMaskOfScreen: %s %s", RXEventMaskOfScreen($scr), Rroot_input_mask($scr)) 
end

procs0_len = 353
procs0 = [Proc.new {|| add_mark()}, Proc.new {|| amp_control()}, Proc.new {|| ask_before_overwrite()}, Proc.new {|| audio_input_device()}, Proc.new {|| audio_output_device()}, Proc.new {|| auto_resize()}, Proc.new {|| auto_update()}, Proc.new {|| auto_update_interval()}, Proc.new {|| axis_info()}, Proc.new {|| axis_label_font()}, Proc.new {|| axis_numbers_font()}, Proc.new {|| basic_color()}, Proc.new {|| beats_per_minute()}, Proc.new {|| beats_per_minute()}, Proc.new {|| bold_peaks_font()}, Proc.new {|| bomb()}, Proc.new {|| c_g?()}, Proc.new {|| channel2vct()}, Proc.new {|| channel_amp_envs()}, Proc.new {|| channel_properties()}, Proc.new {|| channel_style()}, Proc.new {|| channel_widgets()}, Proc.new {|| channels()}, Proc.new {|| chans()}, Proc.new {|| clear_listener()}, Proc.new {|| color_cutoff()}, Proc.new {|| color_inverted()}, Proc.new {|| color_scale()}, Proc.new {|| colormap()}, Proc.new {|| comment()}, Proc.new {|| contrast_control()}, Proc.new {|| contrast_control_amp()}, Proc.new {|| contrast_control?()}, Proc.new {|| current_font()}, Proc.new {|| cursor()}, Proc.new {|| cursor_color()}, Proc.new {|| cursor_follows_play()}, Proc.new {|| cursor_position()}, Proc.new {|| cursor_size()}, Proc.new {|| cursor_style()}, Proc.new {|| dac_combines_channels()}, Proc.new {|| dac_size()}, Proc.new {|| data_clipped()}, Proc.new {|| data_color()}, Proc.new {|| data_format()}, Proc.new {|| data_location()}, Proc.new {|| default_output_chans()}, Proc.new {|| default_output_format()}, Proc.new {|| default_output_srate()}, Proc.new {|| default_output_type()}, Proc.new {|| delete_mark()}, Proc.new {|| delete_marks()}, Proc.new {|| delete_selection()}, Proc.new {|| dialog_widgets()}, Proc.new {|| display_edits()}, Proc.new {|| dot_size()}, Proc.new {|| edit_fragment()}, Proc.new {|| edit_position()}, Proc.new {|| edit_tree()}, Proc.new {|| edits()}, Proc.new {|| emacs_style_save_as()}, Proc.new {|| enved_envelope()}, Proc.new {|| enved_base()}, Proc.new {|| enved_clip?()}, Proc.new {|| enved_style()}, Proc.new {|| enved_filter()}, Proc.new {|| enved_filter_order()}, Proc.new {|| enved_in_dB()}, Proc.new {|| enved_power()}, Proc.new {|| enved_target()}, Proc.new {|| enved_wave?()}, Proc.new {|| enved_waveform_color()}, Proc.new {|| eps_bottom_margin()}, Proc.new {|| eps_file()}, Proc.new {|| eps_left_margin()}, Proc.new {|| eps_size()}, Proc.new {|| equalize_panes()}, Proc.new {|| expand_control()}, Proc.new {|| expand_control_hop()}, Proc.new {|| expand_control_length()}, Proc.new {|| expand_control_ramp()}, Proc.new {|| expand_control?()}, Proc.new {|| fft_log_frequency()}, Proc.new {|| fft_log_magnitude()}, Proc.new {|| fft_window()}, Proc.new {|| fft_window_beta()}, Proc.new {|| file_name()}, Proc.new {|| filter_control_coeffs()}, Proc.new {|| filter_control_envelope()}, Proc.new {|| filter_control_in_dB()}, Proc.new {|| filter_control_order()}, Proc.new {|| filter_control?()}, Proc.new {|| filter_control_in_hz()}, Proc.new {|| filter_control_waveform_color()}, Proc.new {|| finish_progress_report()}, Proc.new {|| foreground_color()}, Proc.new {|| forget_region()}, Proc.new {|| frames()}, Proc.new {|| graph2ps()}, Proc.new {|| graph_color()}, Proc.new {|| graph_cursor()}, Proc.new {|| graph_style()}, Proc.new {|| graphs_horizontal()}, Proc.new {|| header_type()}, Proc.new {|| highlight_color()}, Proc.new {|| just_sounds()}, Proc.new {|| ladspa_dir()}, Proc.new {|| left_sample()}, Proc.new {|| lisp_graph?()}, Proc.new {|| listener_color()}, Proc.new {|| listener_font()}, Proc.new {|| listener_prompt()}, Proc.new {|| listener_selection()}, Proc.new {|| listener_text_color()}, Proc.new {|| little_endian?()}, Proc.new {|| locsig_type()}, Proc.new {|| main_widgets()}, Proc.new {|| make_all_pass()}, Proc.new {|| make_asymmetric_fm()}, Proc.new {|| make_buffer()}, Proc.new {|| make_comb()}, Proc.new {|| make_convolve()}, Proc.new {|| make_delay()}, Proc.new {|| make_env()}, Proc.new {|| make_filter()}, Proc.new {|| make_fir_filter()}, Proc.new {|| make_formant()}, Proc.new {|| make_frame()}, Proc.new {|| make_granulate()}, Proc.new {|| make_graph_data()}, Proc.new {|| make_iir_filter()}, Proc.new {|| make_locsig()}, Proc.new {|| make_mixer()}, Proc.new {|| make_notch()}, Proc.new {|| make_one_pole()}, Proc.new {|| make_one_zero()}, Proc.new {|| make_oscil()}, Proc.new {|| make_phase_vocoder()}, Proc.new {|| make_ppolar()}, Proc.new {|| make_pulse_train()}, Proc.new {|| make_rand()}, Proc.new {|| make_rand_interp()}, Proc.new {|| make_readin()}, Proc.new {|| make_region_sample_reader()}, Proc.new {|| make_sample_reader()}, Proc.new {|| make_sawtooth_wave()}, Proc.new {|| make_sine_summation()}, Proc.new {|| make_square_wave()}, Proc.new {|| make_src()}, Proc.new {|| make_sum_of_cosines()}, Proc.new {|| make_table_lookup()}, Proc.new {|| make_triangle_wave()}, Proc.new {|| make_two_pole()}, Proc.new {|| make_two_zero()}, Proc.new {|| make_wave_train()}, Proc.new {|| make_waveshape()}, Proc.new {|| make_zpolar()}, Proc.new {|| mark_color()}, Proc.new {|| mark_home()}, Proc.new {|| mark_name()}, Proc.new {|| mark_sample()}, Proc.new {|| mark_sync()}, Proc.new {|| mark_sync_max()}, Proc.new {|| marks()}, Proc.new {|| max_regions()}, Proc.new {|| max_transform_peaks()}, Proc.new {|| maxamp()}, Proc.new {|| min_dB()}, Proc.new {|| minibuffer_history_length()}, Proc.new {|| mix_amp()}, Proc.new {|| mix_amp_env()}, Proc.new {|| mix_anchor()}, Proc.new {|| mix_chans()}, Proc.new {|| mix_color()}, Proc.new {|| mix_frames()}, Proc.new {|| mix_home()}, Proc.new {|| mix_locked?()}, Proc.new {|| view_mixes_dialog()}, Proc.new {|| mix_position()}, Proc.new {|| mix_region()}, Proc.new {|| mix_selection()}, Proc.new {|| mix_speed()}, Proc.new {|| mix_tag_height()}, Proc.new {|| mix_tag_width()}, Proc.new {|| mix_tag_y()}, Proc.new {|| mix_track()}, Proc.new {|| mix_waveform_height()}, Proc.new {|| mix?()}, Proc.new {|| mixes()}, Proc.new {|| mus_array_print_length()}, Proc.new {|| mus_file_buffer_size()}, Proc.new {|| mus_rand_seed()}, Proc.new {|| mus_sound_prune()}, Proc.new {|| mus_srate()}, Proc.new {|| optimization()}, Proc.new {|| peak_env_info()}, Proc.new {|| peaks_font()}, Proc.new {|| position_color()}, Proc.new {|| previous_files_sort()}, Proc.new {|| previous_files_sort_procedure()}, Proc.new {|| print_length()}, Proc.new {|| pushed_button_color()}, Proc.new {|| read_only()}, Proc.new {|| recorder_autoload()}, Proc.new {|| recorder_buffer_size()}, Proc.new {|| recorder_file()}, Proc.new {|| recorder_gain()}, Proc.new {|| recorder_in_device()}, Proc.new {|| recorder_in_format()}, Proc.new {|| recorder_max_duration()}, Proc.new {|| recorder_out_chans()}, Proc.new {|| recorder_out_format()}, Proc.new {|| recorder_srate()}, Proc.new {|| recorder_trigger()}, Proc.new {|| redo_edit()}, Proc.new {|| region_chans()}, Proc.new {|| region_frames()}, Proc.new {|| region_graph_style()}, Proc.new {|| region_maxamp()}, Proc.new {|| region_sample()}, Proc.new {|| region_samples2vct()}, Proc.new {|| region_srate()}, Proc.new {|| regions()}, Proc.new {|| reset_controls()}, Proc.new {|| reset_listener_cursor()}, Proc.new {|| restore_controls()}, Proc.new {|| reverb_control_decay()}, Proc.new {|| reverb_control_feedback()}, Proc.new {|| reverb_control_length()}, Proc.new {|| reverb_control_lowpass()}, Proc.new {|| reverb_control_scale()}, Proc.new {|| reverb_control?()}, Proc.new {|| reverse_channel()}, Proc.new {|| reverse_selection()}, Proc.new {|| reverse_sound()}, Proc.new {|| revert_sound()}, Proc.new {|| right_sample()}, Proc.new {|| sample()}, Proc.new {|| samples()}, Proc.new {|| samples2sound_data()}, Proc.new {|| samples2vct()}, Proc.new {|| sash_color()}, Proc.new {|| save_controls()}, Proc.new {|| save_dir()}, Proc.new {|| save_envelopes()}, Proc.new {|| save_macros()}, Proc.new {|| save_marks()}, Proc.new {|| save_state_file()}, Proc.new {|| scale_selection_to()}, Proc.new {|| scale_to()}, Proc.new {|| search_procedure()}, Proc.new {|| select_all()}, Proc.new {|| select_channel()}, Proc.new {|| select_sound()}, Proc.new {|| selected_channel()}, Proc.new {|| selected_data_color()}, Proc.new {|| selected_graph_color()}, Proc.new {|| selected_sound()}, Proc.new {|| selection_chans()}, Proc.new {|| selection_color()}, Proc.new {|| selection_creates_region()}, Proc.new {|| selection_frames()}, Proc.new {|| selection_maxamp()}, Proc.new {|| selection_member?()}, Proc.new {|| selection_position()}, Proc.new {|| selection_srate()}, Proc.new {|| selection?()}, Proc.new {|| short_file_name()}, Proc.new {|| show_axes()}, Proc.new {|| show_backtrace()}, Proc.new {|| show_controls()}, Proc.new {|| show_indices()}, Proc.new {|| show_listener()}, Proc.new {|| show_marks()}, Proc.new {|| show_mix_waveforms()}, Proc.new {|| show_selection_transform()}, Proc.new {|| show_transform_peaks()}, Proc.new {|| show_y_zero()}, Proc.new {|| sinc_width()}, Proc.new {|| smooth_channel()}, Proc.new {|| smooth_selection()}, Proc.new {|| smooth_sound()}, Proc.new {|| snd_gcs()}, Proc.new {|| snd_tempnam()}, Proc.new {|| snd_version()}, Proc.new {|| sound_files_in_directory()}, Proc.new {|| sound_loop_info()}, Proc.new {|| sound_properties()}, Proc.new {|| sound_widgets()}, Proc.new {|| sound?()}, Proc.new {|| soundfont_info()}, Proc.new {|| sounds()}, Proc.new {|| spectro_cutoff()}, Proc.new {|| spectro_hop()}, Proc.new {|| spectro_start()}, Proc.new {|| spectro_x_angle()}, Proc.new {|| spectro_x_scale()}, Proc.new {|| spectro_y_angle()}, Proc.new {|| spectro_y_scale()}, Proc.new {|| spectro_z_angle()}, Proc.new {|| spectro_z_scale()}, Proc.new {|| speed_control()}, Proc.new {|| speed_control_style()}, Proc.new {|| speed_control_tones()}, Proc.new {|| squelch_update()}, Proc.new {|| srate()}, Proc.new {|| start_progress_report()}, Proc.new {|| stop_playing()}, Proc.new {|| swap_channels()}, Proc.new {|| sync()}, Proc.new {|| temp_dir()}, Proc.new {|| text_focus_color()}, Proc.new {|| time_graph_type()}, Proc.new {|| time_graph?()}, Proc.new {|| tiny_font()}, Proc.new {|| transform_graph_type()}, Proc.new {|| transform_graph?()}, Proc.new {|| transform_normalization()}, Proc.new {|| transform_sample()}, Proc.new {|| transform_samples2vct()}, Proc.new {|| transform_samples_size()}, Proc.new {|| transform_size()}, Proc.new {|| transform_type()}, Proc.new {|| trap_segfault()}, Proc.new {|| undo()}, Proc.new {|| update_lisp_graph()}, Proc.new {|| update_time_graph()}, Proc.new {|| update_transform_graph()}, Proc.new {|| vct()}, Proc.new {|| verbose_cursor()}, Proc.new {|| vu_font()}, Proc.new {|| vu_font_size()}, Proc.new {|| vu_size()}, Proc.new {|| wavelet_type()}, Proc.new {|| wavo_hop()}, Proc.new {|| wavo_trace()}, Proc.new {|| window_height()}, Proc.new {|| window_width()}, Proc.new {|| window_x()}, Proc.new {|| window_y()}, Proc.new {|| with_gl()}, Proc.new {|| with_mix_tags()}, Proc.new {|| with_relative_panes()}, Proc.new {|| x_axis_style()}, Proc.new {|| x_bounds()}, Proc.new {|| x_position_slider()}, Proc.new {|| x_zoom_slider()}, Proc.new {|| y_bounds()}, Proc.new {|| y_position_slider()}, Proc.new {|| y_zoom_slider()}, Proc.new {|| zero_pad()}, Proc.new {|| zoom_color()}, Proc.new {|| zoom_focus_style()}]

set_procs0_len = 211
set_procs0 = [Proc.new {|a| set_amp_control(a)}, Proc.new {|a| set_ask_before_overwrite(a)}, Proc.new {|a| set_audio_input_device(a)}, Proc.new {|a| set_audio_output_device(a)}, Proc.new {|a| set_auto_resize(a)}, Proc.new {|a| set_auto_update(a)}, Proc.new {|a| set_auto_update_interval(a)}, Proc.new {|a| set_axis_label_font(a)}, Proc.new {|a| set_axis_numbers_font(a)}, Proc.new {|a| set_beats_per_minute(a)}, Proc.new {|a| set_beats_per_minute(a)}, Proc.new {|a| set_bold_peaks_font(a)}, Proc.new {|a| set_channel_properties(a)}, Proc.new {|a| set_channel_style(a)}, Proc.new {|a| set_color_cutoff(a)}, Proc.new {|a| set_color_inverted(a)}, Proc.new {|a| set_color_scale(a)}, Proc.new {|a| set_colormap(a)}, Proc.new {|a| set_comment(a)}, Proc.new {|a| set_contrast_control(a)}, Proc.new {|a| set_contrast_control_amp(a)}, Proc.new {|a| set_contrast_control?(a)}, Proc.new {|a| set_current_font(a)}, Proc.new {|a| set_cursor(a)}, Proc.new {|a| set_cursor_color(a)}, Proc.new {|a| set_cursor_follows_play(a)}, Proc.new {|a| set_cursor_size(a)}, Proc.new {|a| set_cursor_style(a)}, Proc.new {|a| set_dac_combines_channels(a)}, Proc.new {|a| set_dac_size(a)}, Proc.new {|a| set_data_clipped(a)}, Proc.new {|a| set_data_color(a)}, Proc.new {|a| set_default_output_chans(a)}, Proc.new {|a| set_default_output_format(a)}, Proc.new {|a| set_default_output_srate(a)}, Proc.new {|a| set_default_output_type(a)}, Proc.new {|a| set_dot_size(a)}, Proc.new {|a| set_edit_position(a)}, Proc.new {|a| set_emacs_style_save_as(a)}, Proc.new {|a| set_enved_envelope(a)}, Proc.new {|a| set_enved_base(a)}, Proc.new {|a| set_enved_clip?(a)}, Proc.new {|a| set_enved_style(a)}, Proc.new {|a| set_enved_filter(a)}, Proc.new {|a| set_enved_filter_order(a)}, Proc.new {|a| set_enved_in_dB(a)}, Proc.new {|a| set_enved_power(a)}, Proc.new {|a| set_enved_target(a)}, Proc.new {|a| set_enved_wave?(a)}, Proc.new {|a| set_enved_waveform_color(a)}, Proc.new {|a| set_eps_bottom_margin(a)}, Proc.new {|a| set_eps_file(a)}, Proc.new {|a| set_eps_left_margin(a)}, Proc.new {|a| set_eps_size(a)}, Proc.new {|a| set_expand_control(a)}, Proc.new {|a| set_expand_control_hop(a)}, Proc.new {|a| set_expand_control_length(a)}, Proc.new {|a| set_expand_control_ramp(a)}, Proc.new {|a| set_expand_control?(a)}, Proc.new {|a| set_fft_log_frequency(a)}, Proc.new {|a| set_fft_log_magnitude(a)}, Proc.new {|a| set_fft_window(a)}, Proc.new {|a| set_fft_window_beta(a)}, Proc.new {|a| set_filter_control_envelope(a)}, Proc.new {|a| set_filter_control_in_dB(a)}, Proc.new {|a| set_filter_control_order(a)}, Proc.new {|a| set_filter_control?(a)}, Proc.new {|a| set_filter_control_in_hz(a)}, Proc.new {|a| set_filter_control_waveform_color(a)}, Proc.new {|a| set_foreground_color(a)}, Proc.new {|a| set_frames(a)}, Proc.new {|a| set_graph_color(a)}, Proc.new {|a| set_graph_cursor(a)}, Proc.new {|a| set_graph_style(a)}, Proc.new {|a| set_graphs_horizontal(a)}, Proc.new {|a| set_highlight_color(a)}, Proc.new {|a| set_just_sounds(a)}, Proc.new {|a| set_ladspa_dir(a)}, Proc.new {|a| set_left_sample(a)}, Proc.new {|a| set_lisp_graph?(a)}, Proc.new {|a| set_listener_color(a)}, Proc.new {|a| set_listener_font(a)}, Proc.new {|a| set_listener_prompt(a)}, Proc.new {|a| set_listener_text_color(a)}, Proc.new {|a| set_locsig_type(a)}, Proc.new {|a| set_mark_color(a)}, Proc.new {|a| set_mark_name(a)}, Proc.new {|a| set_mark_sample(a)}, Proc.new {|a| set_mark_sync(a)}, Proc.new {|a| set_max_regions(a)}, Proc.new {|a| set_max_transform_peaks(a)}, Proc.new {|a| set_maxamp(a)}, Proc.new {|a| set_min_dB(a)}, Proc.new {|a| set_minibuffer_history_length(a)}, Proc.new {|a| set_mix_amp(a)}, Proc.new {|a| set_mix_amp_env(a)}, Proc.new {|a| set_mix_anchor(a)}, Proc.new {|a| set_mix_color(a)}, Proc.new {|a| set_mix_frames(a)}, Proc.new {|a| set_mix_locked?(a)}, Proc.new {|a| set_mix_position(a)}, Proc.new {|a| set_mix_speed(a)}, Proc.new {|a| set_mix_tag_height(a)}, Proc.new {|a| set_mix_tag_width(a)}, Proc.new {|a| set_mix_tag_y(a)}, Proc.new {|a| set_mix_track(a)}, Proc.new {|a| set_mix_waveform_height(a)}, Proc.new {|a| set_mus_array_print_length(a)}, Proc.new {|a| set_mus_file_buffer_size(a)}, Proc.new {|a| set_mus_rand_seed(a)}, Proc.new {|a| set_optimization(a)}, Proc.new {|a| set_peaks_font(a)}, Proc.new {|a| set_position_color(a)}, Proc.new {|a| set_previous_files_sort(a)}, Proc.new {|a| set_previous_files_sort_procedure(a)}, Proc.new {|a| set_print_length(a)}, Proc.new {|a| set_pushed_button_color(a)}, Proc.new {|a| set_read_only(a)}, Proc.new {|a| set_recorder_autoload(a)}, Proc.new {|a| set_recorder_buffer_size(a)}, Proc.new {|a| set_recorder_file(a)}, Proc.new {|a| set_recorder_gain(a)}, Proc.new {|a| set_recorder_in_device(a)}, Proc.new {|a| set_recorder_in_format(a)}, Proc.new {|a| set_recorder_max_duration(a)}, Proc.new {|a| set_recorder_out_chans(a)}, Proc.new {|a| set_recorder_out_format(a)}, Proc.new {|a| set_recorder_srate(a)}, Proc.new {|a| set_recorder_trigger(a)}, Proc.new {|a| set_region_graph_style(a)}, Proc.new {|a| set_reverb_control_decay(a)}, Proc.new {|a| set_reverb_control_feedback(a)}, Proc.new {|a| set_reverb_control_length(a)}, Proc.new {|a| set_reverb_control_lowpass(a)}, Proc.new {|a| set_reverb_control_scale(a)}, Proc.new {|a| set_reverb_control?(a)}, Proc.new {|a| set_right_sample(a)}, Proc.new {|a| set_sample(a)}, Proc.new {|a| set_samples(a)}, Proc.new {|a| set_sash_color(a)}, Proc.new {|a| set_save_dir(a)}, Proc.new {|a| set_save_state_file(a)}, Proc.new {|a| set_selected_channel(a)}, Proc.new {|a| set_selected_data_color(a)}, Proc.new {|a| set_selected_graph_color(a)}, Proc.new {|a| set_selected_sound(a)}, Proc.new {|a| set_selection_color(a)}, Proc.new {|a| set_selection_creates_region(a)}, Proc.new {|a| set_selection_frames(a)}, Proc.new {|a| set_selection_member?(a)}, Proc.new {|a| set_selection_position(a)}, Proc.new {|a| set_show_axes(a)}, Proc.new {|a| set_show_backtrace(a)}, Proc.new {|a| set_show_controls(a)}, Proc.new {|a| set_show_indices(a)}, Proc.new {|a| set_show_listener(a)}, Proc.new {|a| set_show_marks(a)}, Proc.new {|a| set_show_mix_waveforms(a)}, Proc.new {|a| set_show_selection_transform(a)}, Proc.new {|a| set_show_transform_peaks(a)}, Proc.new {|a| set_show_y_zero(a)}, Proc.new {|a| set_sinc_width(a)}, Proc.new {|a| set_sound_properties(a)}, Proc.new {|a| set_spectro_cutoff(a)}, Proc.new {|a| set_spectro_hop(a)}, Proc.new {|a| set_spectro_start(a)}, Proc.new {|a| set_spectro_x_angle(a)}, Proc.new {|a| set_spectro_x_scale(a)}, Proc.new {|a| set_spectro_y_angle(a)}, Proc.new {|a| set_spectro_y_scale(a)}, Proc.new {|a| set_spectro_z_angle(a)}, Proc.new {|a| set_spectro_z_scale(a)}, Proc.new {|a| set_speed_control(a)}, Proc.new {|a| set_speed_control_style(a)}, Proc.new {|a| set_speed_control_tones(a)}, Proc.new {|a| set_sync(a)}, Proc.new {|a| set_temp_dir(a)}, Proc.new {|a| set_text_focus_color(a)}, Proc.new {|a| set_time_graph_type(a)}, Proc.new {|a| set_time_graph?(a)}, Proc.new {|a| set_tiny_font(a)}, Proc.new {|a| set_transform_graph_type(a)}, Proc.new {|a| set_transform_graph?(a)}, Proc.new {|a| set_transform_normalization(a)}, Proc.new {|a| set_transform_size(a)}, Proc.new {|a| set_transform_type(a)}, Proc.new {|a| set_trap_segfault(a)}, Proc.new {|a| set_verbose_cursor(a)}, Proc.new {|a| set_vu_font(a)}, Proc.new {|a| set_vu_font_size(a)}, Proc.new {|a| set_vu_size(a)}, Proc.new {|a| set_wavelet_type(a)}, Proc.new {|a| set_wavo_hop(a)}, Proc.new {|a| set_wavo_trace(a)}, Proc.new {|a| set_window_height(a)}, Proc.new {|a| set_window_width(a)}, Proc.new {|a| set_window_x(a)}, Proc.new {|a| set_window_y(a)}, Proc.new {|a| set_with_gl(a)}, Proc.new {|a| set_with_mix_tags(a)}, Proc.new {|a| set_with_relative_panes(a)}, Proc.new {|a| set_x_axis_style(a)}, Proc.new {|a| set_x_bounds(a)}, Proc.new {|a| set_x_position_slider(a)}, Proc.new {|a| set_x_zoom_slider(a)}, Proc.new {|a| set_y_bounds(a)}, Proc.new {|a| set_y_position_slider(a)}, Proc.new {|a| set_y_zoom_slider(a)}, Proc.new {|a| set_zero_pad(a)}, Proc.new {|a| set_zoom_color(a)}, Proc.new {|a| set_zoom_focus_style(a)}]

procs1_len = 464
procs1 =[Proc.new {|a| add_mark(a)}, Proc.new {|a| add_player(a)}, Proc.new {|a| add_sound_file_extension(a)}, Proc.new {|a| all_pass(a)}, Proc.new {|a| all_pass?(a)}, Proc.new {|a| amp_control(a)}, Proc.new {|a| as_one_edit(a)}, Proc.new {|a| asymmetric_fm(a)}, Proc.new {|a| asymmetric_fm?(a)}, Proc.new {|a| autocorrelate(a)}, Proc.new {|a| axis_info(a)}, Proc.new {|a| beats_per_minute(a)}, Proc.new {|a| beats_per_minute(a)}, Proc.new {|a| bomb(a)}, Proc.new {|a| buffer2frame(a)}, Proc.new {|a| buffer2sample(a)}, Proc.new {|a| buffer_empty?(a)}, Proc.new {|a| buffer_full?(a)}, Proc.new {|a| buffer?(a)}, Proc.new {|a| channel2vct(a)}, Proc.new {|a| channel_amp_envs(a)}, Proc.new {|a| channel_properties(a)}, Proc.new {|a| channel_style(a)}, Proc.new {|a| channel_widgets(a)}, Proc.new {|a| channels(a)}, Proc.new {|a| chans(a)}, Proc.new {|a| clear_array(a)}, Proc.new {|a| clm_channel(a)}, Proc.new {|a| color2list(a)}, Proc.new {|a| color?(a)}, Proc.new {|a| comb(a)}, Proc.new {|a| comb?(a)}, Proc.new {|a| comment(a)}, Proc.new {|a| contrast_control(a)}, Proc.new {|a| contrast_control_amp(a)}, Proc.new {|a| contrast_control?(a)}, Proc.new {|a| convolve(a)}, Proc.new {|a| convolve_selection_with(a)}, Proc.new {|a| convolve_with(a)}, Proc.new {|a| convolve?(a)}, Proc.new {|a| current_font(a)}, Proc.new {|a| cursor(a)}, Proc.new {|a| cursor_follows_play(a)}, Proc.new {|a| cursor_position(a)}, Proc.new {|a| cursor_size(a)}, Proc.new {|a| cursor_style(a)}, Proc.new {|a| data_format(a)}, Proc.new {|a| data_location(a)}, Proc.new {|a| db2linear(a)}, Proc.new {|a| degrees2radians(a)}, Proc.new {|a| delay(a)}, Proc.new {|a| delay?(a)}, Proc.new {|a| delete_mark(a)}, Proc.new {|a| delete_marks(a)}, Proc.new {|a| delete_sample(a)}, Proc.new {|a| disk_kspace(a)}, Proc.new {|a| display_edits(a)}, Proc.new {|a| dot_size(a)}, Proc.new {|a| draw_dots(a)}, Proc.new {|a| draw_lines(a)}, Proc.new {|a| edit_fragment(a)}, Proc.new {|a| edit_position(a)}, Proc.new {|a| edit_tree(a)}, Proc.new {|a| edits(a)}, Proc.new {|a| env(a)}, Proc.new {|a| env_channel(a)}, Proc.new {|a| env_selection(a)}, Proc.new {|a| env_sound(a)}, Proc.new {|a| env?(a)}, Proc.new {|a| equalize_panes(a)}, Proc.new {|a| expand_control(a)}, Proc.new {|a| expand_control_hop(a)}, Proc.new {|a| expand_control_length(a)}, Proc.new {|a| expand_control_ramp(a)}, Proc.new {|a| expand_control?(a)}, Proc.new {|a| fft_log_frequency(a)}, Proc.new {|a| fft_log_magnitude(a)}, Proc.new {|a| fft_window(a)}, Proc.new {|a| fft_window_beta(a)}, Proc.new {|a| file2frame?(a)}, Proc.new {|a| file2sample?(a)}, Proc.new {|a| file_name(a)}, Proc.new {|a| fill_polygon(a)}, Proc.new {|a| filter_control_coeffs(a)}, Proc.new {|a| filter_control_envelope(a)}, Proc.new {|a| filter_control_in_dB(a)}, Proc.new {|a| filter_control_order(a)}, Proc.new {|a| filter_control?(a)}, Proc.new {|a| filter_selection(a)}, Proc.new {|a| filter_sound(a)}, Proc.new {|a| filter?(a)}, Proc.new {|a| find_mark(a)}, Proc.new {|a| find_sound(a)}, Proc.new {|a| finish_progress_report(a)}, Proc.new {|a| fir_filter?(a)}, Proc.new {|a| foreground_color(a)}, Proc.new {|a| forget_region(a)}, Proc.new {|a| formant(a)}, Proc.new {|a| formant?(a)}, Proc.new {|a| frame2file?(a)}, Proc.new {|a| frame2list(a)}, Proc.new {|a| frame?(a)}, Proc.new {|a| frames(a)}, Proc.new {|a| free_sample_reader(a)}, Proc.new {|a| granulate(a)}, Proc.new {|a| granulate?(a)}, Proc.new {|a| graph(a)}, Proc.new {|a| graph2ps(a)}, Proc.new {|a| graph_data(a)}, Proc.new {|a| graph_style(a)}, Proc.new {|a| graphs_horizontal(a)}, Proc.new {|a| header_type(a)}, Proc.new {|a| hz2radians(a)}, Proc.new {|a| iir_filter?(a)}, Proc.new {|a| in_hz(a)}, Proc.new {|a| left_sample(a)}, Proc.new {|a| linear2db(a)}, Proc.new {|a| lisp_graph_style(a)}, Proc.new {|a| lisp_graph?(a)}, Proc.new {|a| list2vct(a)}, Proc.new {|a| load_font(a)}, Proc.new {|a| locsig?(a)}, Proc.new {|a| make_all_pass(a)}, Proc.new {|a| make_asymmetric_fm(a)}, Proc.new {|a| make_buffer(a)}, Proc.new {|a| make_comb(a)}, Proc.new {|a| make_convolve(a)}, Proc.new {|a| make_delay(a)}, Proc.new {|a| make_env(a)}, Proc.new {|a| make_file2frame(a)}, Proc.new {|a| make_file2sample(a)}, Proc.new {|a| make_filter(a)}, Proc.new {|a| make_fir_filter(a)}, Proc.new {|a| make_formant(a)}, Proc.new {|a| make_frame(a)}, Proc.new {|a| make_granulate(a)}, Proc.new {|a| make_graph_data(a)}, Proc.new {|a| make_iir_filter(a)}, Proc.new {|a| make_locsig(a)}, Proc.new {|a| make_mix_sample_reader(a)}, Proc.new {|a| make_mixer(a)}, Proc.new {|a| make_notch(a)}, Proc.new {|a| make_one_pole(a)}, Proc.new {|a| make_one_zero(a)}, Proc.new {|a| make_oscil(a)}, Proc.new {|a| make_phase_vocoder(a)}, Proc.new {|a| make_ppolar(a)}, Proc.new {|a| make_pulse_train(a)}, Proc.new {|a| make_rand(a)}, Proc.new {|a| make_rand_interp(a)}, Proc.new {|a| make_readin(a)}, Proc.new {|a| make_region_sample_reader(a)}, Proc.new {|a| make_sample_reader(a)}, Proc.new {|a| make_sawtooth_wave(a)}, Proc.new {|a| make_sine_summation(a)}, Proc.new {|a| make_square_wave(a)}, Proc.new {|a| make_src(a)}, Proc.new {|a| make_sum_of_cosines(a)}, Proc.new {|a| make_table_lookup(a)}, Proc.new {|a| make_track_sample_reader(a)}, Proc.new {|a| make_triangle_wave(a)}, Proc.new {|a| make_two_pole(a)}, Proc.new {|a| make_two_zero(a)}, Proc.new {|a| make_vct(a)}, Proc.new {|a| make_wave_train(a)}, Proc.new {|a| make_waveshape(a)}, Proc.new {|a| make_zpolar(a)}, Proc.new {|a| mark_home(a)}, Proc.new {|a| mark_name(a)}, Proc.new {|a| mark_sample(a)}, Proc.new {|a| mark_sync(a)}, Proc.new {|a| mark?(a)}, Proc.new {|a| marks(a)}, Proc.new {|a| max_transform_peaks(a)}, Proc.new {|a| maxamp(a)}, Proc.new {|a| min_dB(a)}, Proc.new {|a| mix(a)}, Proc.new {|a| mix_amp(a)}, Proc.new {|a| mix_amp_env(a)}, Proc.new {|a| mix_anchor(a)}, Proc.new {|a| mix_chans(a)}, Proc.new {|a| mix_color(a)}, Proc.new {|a| mix_frames(a)}, Proc.new {|a| mix_home(a)}, Proc.new {|a| mix_locked?(a)}, Proc.new {|a| mix_position(a)}, Proc.new {|a| mix_region(a)}, Proc.new {|a| mix_sample_reader?(a)}, Proc.new {|a| mix_selection(a)}, Proc.new {|a| mix_speed(a)}, Proc.new {|a| mix_tag_y(a)}, Proc.new {|a| mix_track(a)}, Proc.new {|a| mix_vct(a)}, Proc.new {|a| mix?(a)}, Proc.new {|a| mixer?(a)}, Proc.new {|a| mixes(a)}, Proc.new {|a| mus_a0(a)}, Proc.new {|a| mus_a1(a)}, Proc.new {|a| mus_a2(a)}, Proc.new {|a| mus_b1(a)}, Proc.new {|a| mus_b2(a)}, Proc.new {|a| mus_channel(a)}, Proc.new {|a| mus_channels(a)}, Proc.new {|a| mus_close(a)}, Proc.new {|a| mus_cosines(a)}, Proc.new {|a| mus_data(a)}, Proc.new {|a| mus_bytes_per_sample(a)}, Proc.new {|a| mus_data_format_name(a)}, Proc.new {|a| mus_describe(a)}, Proc.new {|a| mus_error_to_string(a)}, Proc.new {|a| mus_expand_filename(a)}, Proc.new {|a| mus_feedback(a)}, Proc.new {|a| mus_feedforward(a)}, Proc.new {|a| mus_file_name(a)}, Proc.new {|a| mus_file_prescaler(a)}, Proc.new {|a| mus_formant_radius(a)}, Proc.new {|a| mus_frequency(a)}, Proc.new {|a| mus_header_type_name(a)}, Proc.new {|a| mus_hop(a)}, Proc.new {|a| mus_increment(a)}, Proc.new {|a| mus_input?(a)}, Proc.new {|a| mus_inspect(a)}, Proc.new {|a| mus_length(a)}, Proc.new {|a| mus_location(a)}, Proc.new {|a| mus_name(a)}, Proc.new {|a| mus_offset(a)}, Proc.new {|a| mus_order(a)}, Proc.new {|a| mus_output?(a)}, Proc.new {|a| mus_phase(a)}, Proc.new {|a| mus_ramp(a)}, Proc.new {|a| mus_random(a)}, Proc.new {|a| mus_scaler(a)}, Proc.new {|a| mus_sound_chans(a)}, Proc.new {|a| mus_sound_close_input(a)}, Proc.new {|a| mus_sound_comment(a)}, Proc.new {|a| mus_sound_data_format(a)}, Proc.new {|a| mus_sound_data_location(a)}, Proc.new {|a| mus_sound_datum_size(a)}, Proc.new {|a| mus_sound_duration(a)}, Proc.new {|a| mus_sound_forget(a)}, Proc.new {|a| mus_sound_frames(a)}, Proc.new {|a| mus_sound_header_type(a)}, Proc.new {|a| mus_sound_length(a)}, Proc.new {|a| mus_sound_loop_info(a)}, Proc.new {|a| mus_sound_maxamp(a)}, Proc.new {|a| mus_sound_maxamp_exists?(a)}, Proc.new {|a| mus_sound_open_input(a)}, Proc.new {|a| mus_sound_samples(a)}, Proc.new {|a| mus_sound_srate(a)}, Proc.new {|a| mus_sound_type_specifier(a)}, Proc.new {|a| mus_sound_write_date(a)}, Proc.new {|a| mus_width(a)}, Proc.new {|a| mus_xcoeffs(a)}, Proc.new {|a| mus_ycoeffs(a)}, Proc.new {|a| next_sample(a)}, Proc.new {|a| notch(a)}, Proc.new {|a| notch?(a)}, Proc.new {|a| one_pole(a)}, Proc.new {|a| one_pole?(a)}, Proc.new {|a| one_zero(a)}, Proc.new {|a| one_zero?(a)}, Proc.new {|a| open_sound(a)}, Proc.new {|a| oscil(a)}, Proc.new {|a| oscil?(a)}, Proc.new {|a| partials2polynomial(a)}, Proc.new {|a| partials2wave(a)}, Proc.new {|a| partials2waveshape(a)}, Proc.new {|a| peak_env_info(a)}, Proc.new {|a| phase_partials2wave(a)}, Proc.new {|a| phase_vocoder?(a)}, Proc.new {|a| player?(a)}, Proc.new {|a| position2x(a)}, Proc.new {|a| position2y(a)}, Proc.new {|a| preload_directory(a)}, Proc.new {|a| preload_file(a)}, Proc.new {|a| previous_sample(a)}, Proc.new {|a| progress_report(a)}, Proc.new {|a| prompt_in_minibuffer(a)}, Proc.new {|a| pulse_train(a)}, Proc.new {|a| pulse_train?(a)}, Proc.new {|a| pv_amp_increments(a)}, Proc.new {|a| pv_amps(a)}, Proc.new {|a| pv_freqs(a)}, Proc.new {|a| pv_outctr(a)}, Proc.new {|a| pv_phase_increments(a)}, Proc.new {|a| pv_phases(a)}, Proc.new {|a| radians2degrees(a)}, Proc.new {|a| radians2hz(a)}, Proc.new {|a| rand(a)}, Proc.new {|a| rand_interp(a)}, Proc.new {|a| rand_interp?(a)}, Proc.new {|a| rand?(a)}, Proc.new {|a| read_mix_sample(a)}, Proc.new {|a| read_only(a)}, Proc.new {|a| read_sample(a)}, Proc.new {|a| read_track_sample(a)}, Proc.new {|a| readin(a)}, Proc.new {|a| readin?(a)}, Proc.new {|a| recorder_gain(a)}, Proc.new {|a| recorder_out_amp(a)}, Proc.new {|a| redo_edit(a)}, Proc.new {|a| region_chans(a)}, Proc.new {|a| region_frames(a)}, Proc.new {|a| region_maxamp(a)}, Proc.new {|a| region_sample(a)}, Proc.new {|a| region_samples2vct(a)}, Proc.new {|a| region_srate(a)}, Proc.new {|a| region?(a)}, Proc.new {|a| report_in_minibuffer(a)}, Proc.new {|a| reset_controls(a)}, Proc.new {|a| restart_env(a)}, Proc.new {|a| restore_controls(a)}, Proc.new {|a| reverb_control_decay(a)}, Proc.new {|a| reverb_control_feedback(a)}, Proc.new {|a| reverb_control_length(a)}, Proc.new {|a| reverb_control_lowpass(a)}, Proc.new {|a| reverb_control_scale(a)}, Proc.new {|a| reverb_control?(a)}, Proc.new {|a| reverse_channel(a)}, Proc.new {|a| reverse_sound(a)}, Proc.new {|a| revert_sound(a)}, Proc.new {|a| right_sample(a)}, Proc.new {|a| sample(a)}, Proc.new {|a| sample2file?(a)}, Proc.new {|a| sample_reader_at_end?(a)}, Proc.new {|a| sample_reader_home(a)}, Proc.new {|a| sample_reader_position(a)}, Proc.new {|a| sample_reader?(a)}, Proc.new {|a| samples(a)}, Proc.new {|a| samples2sound_data(a)}, Proc.new {|a| samples2vct(a)}, Proc.new {|a| save_controls(a)}, Proc.new {|a| save_edit_history(a)}, Proc.new {|a| save_envelopes(a)}, Proc.new {|a| save_listener(a)}, Proc.new {|a| save_marks(a)}, Proc.new {|a| save_options(a)}, Proc.new {|a| save_selection(a)}, Proc.new {|a| save_state(a)}, Proc.new {|a| sawtooth_wave(a)}, Proc.new {|a| sawtooth_wave?(a)}, Proc.new {|a| scale_by(a)}, Proc.new {|a| scale_channel(a)}, Proc.new {|a| scale_selection_by(a)}, Proc.new {|a| scale_selection_to(a)}, Proc.new {|a| scale_sound_by(a)}, Proc.new {|a| scale_sound_to(a)}, Proc.new {|a| scale_to(a)}, Proc.new {|a| search_procedure(a)}, Proc.new {|a| select_all(a)}, Proc.new {|a| select_channel(a)}, Proc.new {|a| select_sound(a)}, Proc.new {|a| selected_channel(a)}, Proc.new {|a| selection_frames(a)}, Proc.new {|a| selection_maxamp(a)}, Proc.new {|a| selection_member?(a)}, Proc.new {|a| selection_position(a)}, Proc.new {|a| short_file_name(a)}, Proc.new {|a| show_axes(a)}, Proc.new {|a| show_controls(a)}, Proc.new {|a| show_marks(a)}, Proc.new {|a| show_mix_waveforms(a)}, Proc.new {|a| show_transform_peaks(a)}, Proc.new {|a| show_y_zero(a)}, Proc.new {|a| sine_summation(a)}, Proc.new {|a| sine_summation?(a)}, Proc.new {|a| smooth_channel(a)}, Proc.new {|a| smooth_sound(a)}, Proc.new {|a| snd_print(a)}, Proc.new {|a| snd_spectrum(a)}, Proc.new {|a| snd_warning(a)}, Proc.new {|a| sound_data2vct(a)}, Proc.new {|a| sound_data_chans(a)}, Proc.new {|a| sound_data_length(a)}, Proc.new {|a| sound_data_maxamp(a)}, Proc.new {|a| sound_data?(a)}, Proc.new {|a| sound_files_in_directory(a)}, Proc.new {|a| sound_loop_info(a)}, Proc.new {|a| sound_properties(a)}, Proc.new {|a| sound_widgets(a)}, Proc.new {|a| sound?(a)}, Proc.new {|a| soundfont_info(a)}, Proc.new {|a| spectro_cutoff(a)}, Proc.new {|a| spectro_hop(a)}, Proc.new {|a| spectro_start(a)}, Proc.new {|a| spectro_x_angle(a)}, Proc.new {|a| spectro_x_scale(a)}, Proc.new {|a| spectro_y_angle(a)}, Proc.new {|a| spectro_y_scale(a)}, Proc.new {|a| spectro_z_angle(a)}, Proc.new {|a| spectro_z_scale(a)}, Proc.new {|a| speed_control(a)}, Proc.new {|a| speed_control_style(a)}, Proc.new {|a| speed_control_tones(a)}, Proc.new {|a| square_wave(a)}, Proc.new {|a| square_wave?(a)}, Proc.new {|a| squelch_update(a)}, Proc.new {|a| srate(a)}, Proc.new {|a| src?(a)}, Proc.new {|a| start_progress_report(a)}, Proc.new {|a| stop_player(a)}, Proc.new {|a| stop_playing(a)}, Proc.new {|a| sum_of_cosines(a)}, Proc.new {|a| sum_of_cosines?(a)}, Proc.new {|a| swap_channels(a)}, Proc.new {|a| sync(a)}, Proc.new {|a| syncd_marks(a)}, Proc.new {|a| table_lookup(a)}, Proc.new {|a| table_lookup?(a)}, Proc.new {|a| tap(a)}, Proc.new {|a| time_graph_style(a)}, Proc.new {|a| time_graph_type(a)}, Proc.new {|a| time_graph?(a)}, Proc.new {|a| track_sample_reader?(a)}, Proc.new {|a| transform_graph_style(a)}, Proc.new {|a| transform_graph_type(a)}, Proc.new {|a| transform_graph?(a)}, Proc.new {|a| transform_normalization(a)}, Proc.new {|a| transform_sample(a)}, Proc.new {|a| transform_samples2vct(a)}, Proc.new {|a| transform_samples_size(a)}, Proc.new {|a| transform_size(a)}, Proc.new {|a| transform_type(a)}, Proc.new {|a| triangle_wave(a)}, Proc.new {|a| triangle_wave?(a)}, Proc.new {|a| two_pole(a)}, Proc.new {|a| two_pole?(a)}, Proc.new {|a| two_zero(a)}, Proc.new {|a| two_zero?(a)}, Proc.new {|a| undo(a)}, Proc.new {|a| update_lisp_graph(a)}, Proc.new {|a| update_time_graph(a)}, Proc.new {|a| update_transform_graph(a)}, Proc.new {|a| vct(a)}, Proc.new {|a| vct2channel(a)}, Proc.new {|a| vct2list(a)}, Proc.new {|a| vct2samples(a)}, Proc.new {|a| vct2sound_data(a)}, Proc.new {|a| vct2vector(a)}, Proc.new {|a| vct_convolve!(a)}, Proc.new {|a| vct_copy(a)}, Proc.new {|a| vct_length(a)}, Proc.new {|a| vct_peak(a)}, Proc.new {|a| vct?(a)}, Proc.new {|a| vector2vct(a)}, Proc.new {|a| verbose_cursor(a)}, Proc.new {|a| view_sound(a)}, Proc.new {|a| wave_train(a)}, Proc.new {|a| wave_train?(a)}, Proc.new {|a| wavelet_type(a)}, Proc.new {|a| waveshape(a)}, Proc.new {|a| waveshape?(a)}, Proc.new {|a| wavo_hop(a)}, Proc.new {|a| wavo_trace(a)}, Proc.new {|a| x2position(a)}, Proc.new {|a| x_axis_style(a)}, Proc.new {|a| x_bounds(a)}, Proc.new {|a| x_position_slider(a)}, Proc.new {|a| x_zoom_slider(a)}, Proc.new {|a| y2position(a)}, Proc.new {|a| y_bounds(a)}, Proc.new {|a| y_position_slider(a)}, Proc.new {|a| y_zoom_slider(a)}, Proc.new {|a| zero_pad(a)}]

set_procs1_len = 128
set_procs1 = [Proc.new {|a, b| set_amp_control(a, b)}, Proc.new {|a, b| set_beats_per_minute(a, b)}, Proc.new {|a, b| set_beats_per_minute(a, b)}, Proc.new {|a, b| set_channel_properties(a, b)}, Proc.new {|a, b| set_channel_style(a, b)}, Proc.new {|a, b| set_comment(a, b)}, Proc.new {|a, b| set_contrast_control(a, b)}, Proc.new {|a, b| set_contrast_control_amp(a, b)}, Proc.new {|a, b| set_contrast_control?(a, b)}, Proc.new {|a, b| set_current_font(a, b)}, Proc.new {|a, b| set_cursor(a, b)}, Proc.new {|a, b| set_cursor_follows_play(a, b)}, Proc.new {|a, b| set_cursor_size(a, b)}, Proc.new {|a, b| set_cursor_style(a, b)}, Proc.new {|a, b| set_dot_size(a, b)}, Proc.new {|a, b| set_edit_position(a, b)}, Proc.new {|a, b| set_expand_control(a, b)}, Proc.new {|a, b| set_expand_control_hop(a, b)}, Proc.new {|a, b| set_expand_control_length(a, b)}, Proc.new {|a, b| set_expand_control_ramp(a, b)}, Proc.new {|a, b| set_expand_control?(a, b)}, Proc.new {|a, b| set_fft_log_frequency(a, b)}, Proc.new {|a, b| set_fft_log_magnitude(a, b)}, Proc.new {|a, b| set_fft_window(a, b)}, Proc.new {|a, b| set_fft_window_beta(a, b)}, Proc.new {|a, b| set_filter_control_envelope(a, b)}, Proc.new {|a, b| set_filter_control_in_dB(a, b)}, Proc.new {|a, b| set_filter_control_order(a, b)}, Proc.new {|a, b| set_filter_control?(a, b)}, Proc.new {|a, b| set_foreground_color(a, b)}, Proc.new {|a, b| set_frames(a, b)}, Proc.new {|a, b| set_graph_style(a, b)}, Proc.new {|a, b| set_graphs_horizontal(a, b)}, Proc.new {|a, b| set_left_sample(a, b)}, Proc.new {|a, b| set_lisp_graph_style(a, b)}, Proc.new {|a, b| set_lisp_graph?(a, b)}, Proc.new {|a, b| set_mark_name(a, b)}, Proc.new {|a, b| set_mark_sample(a, b)}, Proc.new {|a, b| set_mark_sync(a, b)}, Proc.new {|a, b| set_max_transform_peaks(a, b)}, Proc.new {|a, b| set_maxamp(a, b)}, Proc.new {|a, b| set_min_dB(a, b)}, Proc.new {|a, b| set_mix_amp(a, b)}, Proc.new {|a, b| set_mix_amp_env(a, b)}, Proc.new {|a, b| set_mix_anchor(a, b)}, Proc.new {|a, b| set_mix_color(a, b)}, Proc.new {|a, b| set_mix_frames(a, b)}, Proc.new {|a, b| set_mix_locked?(a, b)}, Proc.new {|a, b| set_mix_position(a, b)}, Proc.new {|a, b| set_mix_speed(a, b)}, Proc.new {|a, b| set_mix_tag_y(a, b)}, Proc.new {|a, b| set_mix_track(a, b)}, Proc.new {|a, b| set_mus_a0(a, b)}, Proc.new {|a, b| set_mus_a1(a, b)}, Proc.new {|a, b| set_mus_a2(a, b)}, Proc.new {|a, b| set_mus_b1(a, b)}, Proc.new {|a, b| set_mus_b2(a, b)}, Proc.new {|a, b| set_mus_cosines(a, b)}, Proc.new {|a, b| set_mus_data(a, b)}, Proc.new {|a, b| set_mus_feedback(a, b)}, Proc.new {|a, b| set_mus_feedforward(a, b)}, Proc.new {|a, b| set_mus_formant_radius(a, b)}, Proc.new {|a, b| set_mus_frequency(a, b)}, Proc.new {|a, b| set_mus_hop(a, b)}, Proc.new {|a, b| set_mus_increment(a, b)}, Proc.new {|a, b| set_mus_length(a, b)}, Proc.new {|a, b| set_mus_location(a, b)}, Proc.new {|a, b| set_mus_phase(a, b)}, Proc.new {|a, b| set_mus_ramp(a, b)}, Proc.new {|a, b| set_mus_scaler(a, b)}, Proc.new {|a, b| set_mus_width(a, b)}, Proc.new {|a, b| set_read_only(a, b)}, Proc.new {|a, b| set_recorder_gain(a, b)}, Proc.new {|a, b| set_recorder_out_amp(a, b)}, Proc.new {|a, b| set_reverb_control_decay(a, b)}, Proc.new {|a, b| set_reverb_control_feedback(a, b)}, Proc.new {|a, b| set_reverb_control_length(a, b)}, Proc.new {|a, b| set_reverb_control_lowpass(a, b)}, Proc.new {|a, b| set_reverb_control_scale(a, b)}, Proc.new {|a, b| set_reverb_control?(a, b)}, Proc.new {|a, b| set_right_sample(a, b)}, Proc.new {|a, b| set_sample(a, b)}, Proc.new {|a, b| set_samples(a, b)}, Proc.new {|a, b| set_selected_channel(a, b)}, Proc.new {|a, b| set_selection_frames(a, b)}, Proc.new {|a, b| set_selection_member?(a, b)}, Proc.new {|a, b| set_selection_position(a, b)}, Proc.new {|a, b| set_show_axes(a, b)}, Proc.new {|a, b| set_show_controls(a, b)}, Proc.new {|a, b| set_show_marks(a, b)}, Proc.new {|a, b| set_show_mix_waveforms(a, b)}, Proc.new {|a, b| set_show_transform_peaks(a, b)}, Proc.new {|a, b| set_show_y_zero(a, b)}, Proc.new {|a, b| set_sound_properties(a, b)}, Proc.new {|a, b| set_spectro_cutoff(a, b)}, Proc.new {|a, b| set_spectro_hop(a, b)}, Proc.new {|a, b| set_spectro_start(a, b)}, Proc.new {|a, b| set_spectro_x_angle(a, b)}, Proc.new {|a, b| set_spectro_x_scale(a, b)}, Proc.new {|a, b| set_spectro_y_angle(a, b)}, Proc.new {|a, b| set_spectro_y_scale(a, b)}, Proc.new {|a, b| set_spectro_z_angle(a, b)}, Proc.new {|a, b| set_spectro_z_scale(a, b)}, Proc.new {|a, b| set_speed_control(a, b)}, Proc.new {|a, b| set_speed_control_style(a, b)}, Proc.new {|a, b| set_speed_control_tones(a, b)}, Proc.new {|a, b| set_sync(a, b)}, Proc.new {|a, b| set_time_graph_style(a, b)}, Proc.new {|a, b| set_time_graph_type(a, b)}, Proc.new {|a, b| set_time_graph?(a, b)}, Proc.new {|a, b| set_transform_graph_style(a, b)}, Proc.new {|a, b| set_transform_graph_type(a, b)}, Proc.new {|a, b| set_transform_graph?(a, b)}, Proc.new {|a, b| set_transform_normalization(a, b)}, Proc.new {|a, b| set_transform_size(a, b)}, Proc.new {|a, b| set_transform_type(a, b)}, Proc.new {|a, b| set_verbose_cursor(a, b)}, Proc.new {|a, b| set_wavelet_type(a, b)}, Proc.new {|a, b| set_wavo_hop(a, b)}, Proc.new {|a, b| set_wavo_trace(a, b)}, Proc.new {|a, b| set_x_axis_style(a, b)}, Proc.new {|a, b| set_x_bounds(a, b)}, Proc.new {|a, b| set_x_position_slider(a, b)}, Proc.new {|a, b| set_x_zoom_slider(a, b)}, Proc.new {|a, b| set_y_bounds(a, b)}, Proc.new {|a, b| set_y_position_slider(a, b)}, Proc.new {|a, b| set_y_zoom_slider(a, b)}, Proc.new {|a, b| set_zero_pad(a, b)}]

procs2_len = 277
procs2 = [Proc.new {|a, b| add_mark(a, b)}, Proc.new {|a, b| add_player(a, b)}, Proc.new {|a, b| all_pass(a, b)}, Proc.new {|a, b| amp_control(a, b)}, Proc.new {|a, b| array_interp(a, b)}, Proc.new {|a, b| as_one_edit(a, b)}, Proc.new {|a, b| asymmetric_fm(a, b)}, Proc.new {|a, b| axis_info(a, b)}, Proc.new {|a, b| beats_per_minute(a, b)}, Proc.new {|a, b| beats_per_minute(a, b)}, Proc.new {|a, b| bomb(a, b)}, Proc.new {|a, b| buffer2frame(a, b)}, Proc.new {|a, b| channel2vct(a, b)}, Proc.new {|a, b| channel_amp_envs(a, b)}, Proc.new {|a, b| channel_properties(a, b)}, Proc.new {|a, b| channel_widgets(a, b)}, Proc.new {|a, b| clm_channel(a, b)}, Proc.new {|a, b| comb(a, b)}, Proc.new {|a, b| contrast_enhancement(a, b)}, Proc.new {|a, b| convolution(a, b)}, Proc.new {|a, b| convolve(a, b)}, Proc.new {|a, b| convolve_files(a, b)}, Proc.new {|a, b| convolve_selection_with(a, b)}, Proc.new {|a, b| convolve_with(a, b)}, Proc.new {|a, b| current_font(a, b)}, Proc.new {|a, b| cursor(a, b)}, Proc.new {|a, b| cursor_position(a, b)}, Proc.new {|a, b| cursor_size(a, b)}, Proc.new {|a, b| cursor_style(a, b)}, Proc.new {|a, b| delay(a, b)}, Proc.new {|a, b| delete_marks(a, b)}, Proc.new {|a, b| delete_sample(a, b)}, Proc.new {|a, b| delete_samples(a, b)}, Proc.new {|a, b| display_edits(a, b)}, Proc.new {|a, b| dot_product(a, b)}, Proc.new {|a, b| dot_size(a, b)}, Proc.new {|a, b| draw_dot(a, b)}, Proc.new {|a, b| draw_dots(a, b)}, Proc.new {|a, b| draw_lines(a, b)}, Proc.new {|a, b| edit_fragment(a, b)}, Proc.new {|a, b| edit_position(a, b)}, Proc.new {|a, b| edit_tree(a, b)}, Proc.new {|a, b| edits(a, b)}, Proc.new {|a, b| env_channel(a, b)}, Proc.new {|a, b| env_interp(a, b)}, Proc.new {|a, b| env_selection(a, b)}, Proc.new {|a, b| env_sound(a, b)}, Proc.new {|a, b| fft(a, b)}, Proc.new {|a, b| fft_log_frequency(a, b)}, Proc.new {|a, b| fft_log_magnitude(a, b)}, Proc.new {|a, b| fft_window(a, b)}, Proc.new {|a, b| fft_window_beta(a, b)}, Proc.new {|a, b| file2frame(a, b)}, Proc.new {|a, b| file2sample(a, b)}, Proc.new {|a, b| fill_polygon(a, b)}, Proc.new {|a, b| filter(a, b)}, Proc.new {|a, b| filter_selection(a, b)}, Proc.new {|a, b| filter_sound(a, b)}, Proc.new {|a, b| find_mark(a, b)}, Proc.new {|a, b| fir_filter(a, b)}, Proc.new {|a, b| foreground_color(a, b)}, Proc.new {|a, b| formant(a, b)}, Proc.new {|a, b| formant_bank(a, b)}, Proc.new {|a, b| frame_add(a, b)}, Proc.new {|a, b| frame_multiply(a, b)}, Proc.new {|a, b| frame2buffer(a, b)}, Proc.new {|a, b| frame2frame(a, b)}, Proc.new {|a, b| frame2sample(a, b)}, Proc.new {|a, b| frame_ref(a, b)}, Proc.new {|a, b| frames(a, b)}, Proc.new {|a, b| granulate(a, b)}, Proc.new {|a, b| graph(a, b)}, Proc.new {|a, b| graph_data(a, b)}, Proc.new {|a, b| graph_style(a, b)}, Proc.new {|a, b| graphs_horizontal(a, b)}, Proc.new {|a, b| iir_filter(a, b)}, Proc.new {|a, b| ina(a, b)}, Proc.new {|a, b| inb(a, b)}, Proc.new {|a, b| insert_sample(a, b)}, Proc.new {|a, b| key(a, b)}, Proc.new {|a, b| key_binding(a, b)}, Proc.new {|a, b| left_sample(a, b)}, Proc.new {|a, b| lisp_graph_style(a, b)}, Proc.new {|a, b| lisp_graph?(a, b)}, Proc.new {|a, b| locsig_ref(a, b)}, Proc.new {|a, b| locsig_reverb_ref(a, b)}, Proc.new {|a, b| make_all_pass(a, b)}, Proc.new {|a, b| make_asymmetric_fm(a, b)}, Proc.new {|a, b| make_buffer(a, b)}, Proc.new {|a, b| make_comb(a, b)}, Proc.new {|a, b| make_convolve(a, b)}, Proc.new {|a, b| make_delay(a, b)}, Proc.new {|a, b| make_env(a, b)}, Proc.new {|a, b| make_filter(a, b)}, Proc.new {|a, b| make_fir_filter(a, b)}, Proc.new {|a, b| make_formant(a, b)}, Proc.new {|a, b| make_frame(a, b)}, Proc.new {|a, b| make_granulate(a, b)}, Proc.new {|a, b| make_graph_data(a, b)}, Proc.new {|a, b| make_iir_filter(a, b)}, Proc.new {|a, b| make_locsig(a, b)}, Proc.new {|a, b| make_mixer(a, b)}, Proc.new {|a, b| make_notch(a, b)}, Proc.new {|a, b| make_one_pole(a, b)}, Proc.new {|a, b| make_one_zero(a, b)}, Proc.new {|a, b| make_oscil(a, b)}, Proc.new {|a, b| make_phase_vocoder(a, b)}, Proc.new {|a, b| make_ppolar(a, b)}, Proc.new {|a, b| make_pulse_train(a, b)}, Proc.new {|a, b| make_rand(a, b)}, Proc.new {|a, b| make_rand_interp(a, b)}, Proc.new {|a, b| make_readin(a, b)}, Proc.new {|a, b| make_region_sample_reader(a, b)}, Proc.new {|a, b| make_sample_reader(a, b)}, Proc.new {|a, b| make_sawtooth_wave(a, b)}, Proc.new {|a, b| make_sine_summation(a, b)}, Proc.new {|a, b| make_sound_data(a, b)}, Proc.new {|a, b| make_square_wave(a, b)}, Proc.new {|a, b| make_src(a, b)}, Proc.new {|a, b| make_sum_of_cosines(a, b)}, Proc.new {|a, b| make_table_lookup(a, b)}, Proc.new {|a, b| make_track_sample_reader(a, b)}, Proc.new {|a, b| make_triangle_wave(a, b)}, Proc.new {|a, b| make_two_pole(a, b)}, Proc.new {|a, b| make_two_zero(a, b)}, Proc.new {|a, b| make_vct(a, b)}, Proc.new {|a, b| make_wave_train(a, b)}, Proc.new {|a, b| make_waveshape(a, b)}, Proc.new {|a, b| make_zpolar(a, b)}, Proc.new {|a, b| mark_sample(a, b)}, Proc.new {|a, b| marks(a, b)}, Proc.new {|a, b| max_transform_peaks(a, b)}, Proc.new {|a, b| maxamp(a, b)}, Proc.new {|a, b| min_dB(a, b)}, Proc.new {|a, b| mix(a, b)}, Proc.new {|a, b| mix_amp(a, b)}, Proc.new {|a, b| mix_amp_env(a, b)}, Proc.new {|a, b| mix_region(a, b)}, Proc.new {|a, b| mix_selection(a, b)}, Proc.new {|a, b| mix_sound(a, b)}, Proc.new {|a, b| mix_vct(a, b)}, Proc.new {|a, b| mixer_multiply(a, b)}, Proc.new {|a, b| mixes(a, b)}, Proc.new {|a, b| multiply_arrays(a, b)}, Proc.new {|a, b| mus_bank(a, b)}, Proc.new {|a, b| mus_fft(a, b)}, Proc.new {|a, b| mus_mix(a, b)}, Proc.new {|a, b| mus_sound_close_output(a, b)}, Proc.new {|a, b| mus_sound_seek_frame(a, b)}, Proc.new {|a, b| notch(a, b)}, Proc.new {|a, b| one_pole(a, b)}, Proc.new {|a, b| one_zero(a, b)}, Proc.new {|a, b| oscil(a, b)}, Proc.new {|a, b| oscil_bank(a, b)}, Proc.new {|a, b| pad_channel(a, b)}, Proc.new {|a, b| partials2polynomial(a, b)}, Proc.new {|a, b| partials2wave(a, b)}, Proc.new {|a, b| partials2waveshape(a, b)}, Proc.new {|a, b| peak_env_info(a, b)}, Proc.new {|a, b| phase_partials2wave(a, b)}, Proc.new {|a, b| polar2rectangular(a, b)}, Proc.new {|a, b| polynomial(a, b)}, Proc.new {|a, b| position2x(a, b)}, Proc.new {|a, b| position2y(a, b)}, Proc.new {|a, b| progress_report(a, b)}, Proc.new {|a, b| prompt_in_minibuffer(a, b)}, Proc.new {|a, b| pulse_train(a, b)}, Proc.new {|a, b| ramp_channel(a, b)}, Proc.new {|a, b| rand(a, b)}, Proc.new {|a, b| rand_interp(a, b)}, Proc.new {|a, b| recorder_in_amp(a, b)}, Proc.new {|a, b| rectangular2polar(a, b)}, Proc.new {|a, b| redo_edit(a, b)}, Proc.new {|a, b| region_sample(a, b)}, Proc.new {|a, b| region_samples2vct(a, b)}, Proc.new {|a, b| remove_from_menu(a, b)}, Proc.new {|a, b| report_in_minibuffer(a, b)}, Proc.new {|a, b| reverse_channel(a, b)}, Proc.new {|a, b| reverse_sound(a, b)}, Proc.new {|a, b| right_sample(a, b)}, Proc.new {|a, b| ring_modulate(a, b)}, Proc.new {|a, b| sample(a, b)}, Proc.new {|a, b| sample2buffer(a, b)}, Proc.new {|a, b| sample2frame(a, b)}, Proc.new {|a, b| samples(a, b)}, Proc.new {|a, b| samples2sound_data(a, b)}, Proc.new {|a, b| samples2vct(a, b)}, Proc.new {|a, b| save_edit_history(a, b)}, Proc.new {|a, b| save_region(a, b)}, Proc.new {|a, b| save_selection(a, b)}, Proc.new {|a, b| sawtooth_wave(a, b)}, Proc.new {|a, b| scale_by(a, b)}, Proc.new {|a, b| scale_channel(a, b)}, Proc.new {|a, b| scale_sound_by(a, b)}, Proc.new {|a, b| scale_sound_to(a, b)}, Proc.new {|a, b| scale_to(a, b)}, Proc.new {|a, b| select_all(a, b)}, Proc.new {|a, b| selection_frames(a, b)}, Proc.new {|a, b| selection_maxamp(a, b)}, Proc.new {|a, b| selection_member?(a, b)}, Proc.new {|a, b| selection_position(a, b)}, Proc.new {|a, b| show_axes(a, b)}, Proc.new {|a, b| show_marks(a, b)}, Proc.new {|a, b| show_mix_waveforms(a, b)}, Proc.new {|a, b| show_transform_peaks(a, b)}, Proc.new {|a, b| show_y_zero(a, b)}, Proc.new {|a, b| sine_summation(a, b)}, Proc.new {|a, b| smooth_channel(a, b)}, Proc.new {|a, b| smooth_sound(a, b)}, Proc.new {|a, b| snd_spectrum(a, b)}, Proc.new {|a, b| sound_data2vct(a, b)}, Proc.new {|a, b| spectro_cutoff(a, b)}, Proc.new {|a, b| spectro_hop(a, b)}, Proc.new {|a, b| spectro_start(a, b)}, Proc.new {|a, b| spectro_x_angle(a, b)}, Proc.new {|a, b| spectro_x_scale(a, b)}, Proc.new {|a, b| spectro_y_angle(a, b)}, Proc.new {|a, b| spectro_y_scale(a, b)}, Proc.new {|a, b| spectro_z_angle(a, b)}, Proc.new {|a, b| spectro_z_scale(a, b)}, Proc.new {|a, b| square_wave(a, b)}, Proc.new {|a, b| squelch_update(a, b)}, Proc.new {|a, b| sum_of_cosines(a, b)}, Proc.new {|a, b| sine_bank(a, b)}, Proc.new {|a, b| swap_channels(a, b)}, Proc.new {|a, b| table_lookup(a, b)}, Proc.new {|a, b| tap(a, b)}, Proc.new {|a, b| time_graph_style(a, b)}, Proc.new {|a, b| time_graph_type(a, b)}, Proc.new {|a, b| time_graph?(a, b)}, Proc.new {|a, b| transform_graph_style(a, b)}, Proc.new {|a, b| transform_graph_type(a, b)}, Proc.new {|a, b| transform_graph?(a, b)}, Proc.new {|a, b| transform_normalization(a, b)}, Proc.new {|a, b| transform_sample(a, b)}, Proc.new {|a, b| transform_samples2vct(a, b)}, Proc.new {|a, b| transform_samples_size(a, b)}, Proc.new {|a, b| transform_size(a, b)}, Proc.new {|a, b| transform_type(a, b)}, Proc.new {|a, b| triangle_wave(a, b)}, Proc.new {|a, b| two_pole(a, b)}, Proc.new {|a, b| two_zero(a, b)}, Proc.new {|a, b| unbind_key(a, b)}, Proc.new {|a, b| undo_edit(a, b)}, Proc.new {|a, b| update_lisp_graph(a, b)}, Proc.new {|a, b| update_time_graph(a, b)}, Proc.new {|a, b| update_transform_graph(a, b)}, Proc.new {|a, b| vct(a, b)}, Proc.new {|a, b| vct2channel(a, b)}, Proc.new {|a, b| vct2samples(a, b)}, Proc.new {|a, b| vct2sound_data(a, b)}, Proc.new {|a, b| vct_add!(a, b)}, Proc.new {|a, b| vct_convolve!(a, b)}, Proc.new {|a, b| vct_fill!(a, b)}, Proc.new {|a, b| vct_multiply!(a, b)}, Proc.new {|a, b| vct_offset!(a, b)}, Proc.new {|a, b| vct_ref(a, b)}, Proc.new {|a, b| vct_scale!(a, b)}, Proc.new {|a, b| vct_subseq(a, b)}, Proc.new {|a, b| vct_subtract!(a, b)}, Proc.new {|a, b| verbose_cursor(a, b)}, Proc.new {|a, b| wave_train(a, b)}, Proc.new {|a, b| wavelet_type(a, b)}, Proc.new {|a, b| waveshape(a, b)}, Proc.new {|a, b| wavo_hop(a, b)}, Proc.new {|a, b| wavo_trace(a, b)}, Proc.new {|a, b| x2position(a, b)}, Proc.new {|a, b| x_axis_style(a, b)}, Proc.new {|a, b| x_bounds(a, b)}, Proc.new {|a, b| x_position_slider(a, b)}, Proc.new {|a, b| x_zoom_slider(a, b)}, Proc.new {|a, b| xramp_channel(a, b)}, Proc.new {|a, b| y2position(a, b)}, Proc.new {|a, b| y_bounds(a, b)}, Proc.new {|a, b| y_position_slider(a, b)}, Proc.new {|a, b| y_zoom_slider(a, b)}, Proc.new {|a, b| zero_pad(a, b)}]

set_procs2_len = 69
set_procs2 = [Proc.new {|a, b, c| set_amp_control(a, b, c)}, Proc.new {|a, b, c| set_beats_per_minute(a, b, c)}, Proc.new {|a, b, c| set_beats_per_minute(a, b, c)}, Proc.new {|a, b, c| set_channel_properties(a, b, c)}, Proc.new {|a, b, c| set_current_font(a, b, c)}, Proc.new {|a, b, c| set_cursor(a, b, c)}, Proc.new {|a, b, c| set_cursor_size(a, b, c)}, Proc.new {|a, b, c| set_cursor_style(a, b, c)}, Proc.new {|a, b, c| set_dot_size(a, b, c)}, Proc.new {|a, b, c| set_edit_position(a, b, c)}, Proc.new {|a, b, c| set_fft_log_frequency(a, b, c)}, Proc.new {|a, b, c| set_fft_log_magnitude(a, b, c)}, Proc.new {|a, b, c| set_fft_window(a, b, c)}, Proc.new {|a, b, c| set_fft_window_beta(a, b, c)}, Proc.new {|a, b, c| set_foreground_color(a, b, c)}, Proc.new {|a, b, c| set_frames(a, b, c)}, Proc.new {|a, b, c| set_graph_style(a, b, c)}, Proc.new {|a, b, c| set_graphs_horizontal(a, b, c)}, Proc.new {|a, b, c| set_left_sample(a, b, c)}, Proc.new {|a, b, c| set_lisp_graph_style(a, b, c)}, Proc.new {|a, b, c| set_lisp_graph?(a, b, c)}, Proc.new {|a, b, c| set_mark_sample(a, b, c)}, Proc.new {|a, b, c| set_max_transform_peaks(a, b, c)}, Proc.new {|a, b, c| set_maxamp(a, b, c)}, Proc.new {|a, b, c| set_min_dB(a, b, c)}, Proc.new {|a, b, c| set_mix_amp(a, b, c)}, Proc.new {|a, b, c| set_mix_amp_env(a, b, c)}, Proc.new {|a, b, c| set_recorder_in_amp(a, b, c)}, Proc.new {|a, b, c| set_right_sample(a, b, c)}, Proc.new {|a, b, c| set_sample(a, b, c)}, Proc.new {|a, b, c| set_samples(a, b, c)}, Proc.new {|a, b, c| set_selection_frames(a, b, c)}, Proc.new {|a, b, c| set_selection_member?(a, b, c)}, Proc.new {|a, b, c| set_selection_position(a, b, c)}, Proc.new {|a, b, c| set_show_axes(a, b, c)}, Proc.new {|a, b, c| set_show_marks(a, b, c)}, Proc.new {|a, b, c| set_show_mix_waveforms(a, b, c)}, Proc.new {|a, b, c| set_show_transform_peaks(a, b, c)}, Proc.new {|a, b, c| set_show_y_zero(a, b, c)}, Proc.new {|a, b, c| set_spectro_cutoff(a, b, c)}, Proc.new {|a, b, c| set_spectro_hop(a, b, c)}, Proc.new {|a, b, c| set_spectro_start(a, b, c)}, Proc.new {|a, b, c| set_spectro_x_angle(a, b, c)}, Proc.new {|a, b, c| set_spectro_x_scale(a, b, c)}, Proc.new {|a, b, c| set_spectro_y_angle(a, b, c)}, Proc.new {|a, b, c| set_spectro_y_scale(a, b, c)}, Proc.new {|a, b, c| set_spectro_z_angle(a, b, c)}, Proc.new {|a, b, c| set_spectro_z_scale(a, b, c)}, Proc.new {|a, b, c| set_time_graph_style(a, b, c)}, Proc.new {|a, b, c| set_time_graph_type(a, b, c)}, Proc.new {|a, b, c| set_time_graph?(a, b, c)}, Proc.new {|a, b, c| set_transform_graph_style(a, b, c)}, Proc.new {|a, b, c| set_transform_graph_type(a, b, c)}, Proc.new {|a, b, c| set_transform_graph?(a, b, c)}, Proc.new {|a, b, c| set_transform_normalization(a, b, c)}, Proc.new {|a, b, c| set_transform_size(a, b, c)}, Proc.new {|a, b, c| set_transform_type(a, b, c)}, Proc.new {|a, b, c| set_verbose_cursor(a, b, c)}, Proc.new {|a, b, c| set_wavelet_type(a, b, c)}, Proc.new {|a, b, c| set_wavo_hop(a, b, c)}, Proc.new {|a, b, c| set_wavo_trace(a, b, c)}, Proc.new {|a, b, c| set_x_axis_style(a, b, c)}, Proc.new {|a, b, c| set_x_bounds(a, b, c)}, Proc.new {|a, b, c| set_x_position_slider(a, b, c)}, Proc.new {|a, b, c| set_x_zoom_slider(a, b, c)}, Proc.new {|a, b, c| set_y_bounds(a, b, c)}, Proc.new {|a, b, c| set_y_position_slider(a, b, c)}, Proc.new {|a, b, c| set_y_zoom_slider(a, b, c)}, Proc.new {|a, b, c| set_zero_pad(a, b, c)}]
 
procs3_len = 169
procs3 = [Proc.new {|a, b, c| add_mark(a, b, c)}, Proc.new {|a, b, c| add_player(a, b, c)}, Proc.new {|a, b, c| all_pass(a, b, c)}, Proc.new {|a, b, c| amplitude_modulate(a, b, c)}, Proc.new {|a, b, c| array_interp(a, b, c)}, Proc.new {|a, b, c| asymmetric_fm(a, b, c)}, Proc.new {|a, b, c| axis_info(a, b, c)}, Proc.new {|a, b, c| bind_key(a, b, c)}, Proc.new {|a, b, c| channel2vct(a, b, c)}, Proc.new {|a, b, c| channel_amp_envs(a, b, c)}, Proc.new {|a, b, c| clm_channel(a, b, c)}, Proc.new {|a, b, c| comb(a, b, c)}, Proc.new {|a, b, c| convolution(a, b, c)}, Proc.new {|a, b, c| convolve_files(a, b, c)}, Proc.new {|a, b, c| convolve_with(a, b, c)}, Proc.new {|a, b, c| current_font(a, b, c)}, Proc.new {|a, b, c| cursor(a, b, c)}, Proc.new {|a, b, c| delay(a, b, c)}, Proc.new {|a, b, c| delete_sample(a, b, c)}, Proc.new {|a, b, c| delete_samples(a, b, c)}, Proc.new {|a, b, c| delete_samples_with_origin(a, b, c)}, Proc.new {|a, b, c| display_edits(a, b, c)}, Proc.new {|a, b, c| draw_dot(a, b, c)}, Proc.new {|a, b, c| draw_dots(a, b, c)}, Proc.new {|a, b, c| draw_lines(a, b, c)}, Proc.new {|a, b, c| draw_string(a, b, c)}, Proc.new {|a, b, c| edit_fragment(a, b, c)}, Proc.new {|a, b, c| edit_tree(a, b, c)}, Proc.new {|a, b, c| env_channel(a, b, c)}, Proc.new {|a, b, c| env_sound(a, b, c)}, Proc.new {|a, b, c| fft(a, b, c)}, Proc.new {|a, b, c| file2frame(a, b, c)}, Proc.new {|a, b, c| file2sample(a, b, c)}, Proc.new {|a, b, c| fill_polygon(a, b, c)}, Proc.new {|a, b, c| filter_sound(a, b, c)}, Proc.new {|a, b, c| find_mark(a, b, c)}, Proc.new {|a, b, c| foreground_color(a, b, c)}, Proc.new {|a, b, c| formant_bank(a, b, c)}, Proc.new {|a, b, c| frame_add(a, b, c)}, Proc.new {|a, b, c| frame_multiply(a, b, c)}, Proc.new {|a, b, c| frame2file(a, b, c)}, Proc.new {|a, b, c| frame2frame(a, b, c)}, Proc.new {|a, b, c| frame_set!(a, b, c)}, Proc.new {|a, b, c| frames(a, b, c)}, Proc.new {|a, b, c| graph(a, b, c)}, Proc.new {|a, b, c| graph_data(a, b, c)}, Proc.new {|a, b, c| in_any(a, b, c)}, Proc.new {|a, b, c| insert_sample(a, b, c)}, Proc.new {|a, b, c| insert_samples(a, b, c)}, Proc.new {|a, b, c| key(a, b, c)}, Proc.new {|a, b, c| key_binding(a, b, c)}, Proc.new {|a, b, c| locsig(a, b, c)}, Proc.new {|a, b, c| locsig_reverb_set!(a, b, c)}, Proc.new {|a, b, c| locsig_set!(a, b, c)}, Proc.new {|a, b, c| make_all_pass(a, b, c)}, Proc.new {|a, b, c| make_asymmetric_fm(a, b, c)}, Proc.new {|a, b, c| make_buffer(a, b, c)}, Proc.new {|a, b, c| make_color(a, b, c)}, Proc.new {|a, b, c| make_comb(a, b, c)}, Proc.new {|a, b, c| make_convolve(a, b, c)}, Proc.new {|a, b, c| make_delay(a, b, c)}, Proc.new {|a, b, c| make_env(a, b, c)}, Proc.new {|a, b, c| make_filter(a, b, c)}, Proc.new {|a, b, c| make_fir_filter(a, b, c)}, Proc.new {|a, b, c| make_formant(a, b, c)}, Proc.new {|a, b, c| make_frame(a, b, c)}, Proc.new {|a, b, c| make_granulate(a, b, c)}, Proc.new {|a, b, c| make_graph_data(a, b, c)}, Proc.new {|a, b, c| make_iir_filter(a, b, c)}, Proc.new {|a, b, c| make_locsig(a, b, c)}, Proc.new {|a, b, c| make_mixer(a, b, c)}, Proc.new {|a, b, c| make_notch(a, b, c)}, Proc.new {|a, b, c| make_one_pole(a, b, c)}, Proc.new {|a, b, c| make_one_zero(a, b, c)}, Proc.new {|a, b, c| make_oscil(a, b, c)}, Proc.new {|a, b, c| make_phase_vocoder(a, b, c)}, Proc.new {|a, b, c| make_ppolar(a, b, c)}, Proc.new {|a, b, c| make_pulse_train(a, b, c)}, Proc.new {|a, b, c| make_rand(a, b, c)}, Proc.new {|a, b, c| make_rand_interp(a, b, c)}, Proc.new {|a, b, c| make_readin(a, b, c)}, Proc.new {|a, b, c| make_region_sample_reader(a, b, c)}, Proc.new {|a, b, c| make_sample_reader(a, b, c)}, Proc.new {|a, b, c| make_sawtooth_wave(a, b, c)}, Proc.new {|a, b, c| make_sine_summation(a, b, c)}, Proc.new {|a, b, c| make_square_wave(a, b, c)}, Proc.new {|a, b, c| make_src(a, b, c)}, Proc.new {|a, b, c| make_sum_of_cosines(a, b, c)}, Proc.new {|a, b, c| make_table_lookup(a, b, c)}, Proc.new {|a, b, c| make_track_sample_reader(a, b, c)}, Proc.new {|a, b, c| make_triangle_wave(a, b, c)}, Proc.new {|a, b, c| make_two_pole(a, b, c)}, Proc.new {|a, b, c| make_two_zero(a, b, c)}, Proc.new {|a, b, c| make_wave_train(a, b, c)}, Proc.new {|a, b, c| make_waveshape(a, b, c)}, Proc.new {|a, b, c| make_zpolar(a, b, c)}, Proc.new {|a, b, c| marks(a, b, c)}, Proc.new {|a, b, c| maxamp(a, b, c)}, Proc.new {|a, b, c| mix(a, b, c)}, Proc.new {|a, b, c| mix_region(a, b, c)}, Proc.new {|a, b, c| mix_selection(a, b, c)}, Proc.new {|a, b, c| mix_vct(a, b, c)}, Proc.new {|a, b, c| mixer_multiply(a, b, c)}, Proc.new {|a, b, c| mixer_ref(a, b, c)}, Proc.new {|a, b, c| move_locsig(a, b, c)}, Proc.new {|a, b, c| multiply_arrays(a, b, c)}, Proc.new {|a, b, c| mus_bank(a, b, c)}, Proc.new {|a, b, c| mus_fft(a, b, c)}, Proc.new {|a, b, c| mus_mix(a, b, c)}, Proc.new {|a, b, c| notch(a, b, c)}, Proc.new {|a, b, c| oscil(a, b, c)}, Proc.new {|a, b, c| oscil_bank(a, b, c)}, Proc.new {|a, b, c| outa(a, b, c)}, Proc.new {|a, b, c| outb(a, b, c)}, Proc.new {|a, b, c| outc(a, b, c)}, Proc.new {|a, b, c| outd(a, b, c)}, Proc.new {|a, b, c| pad_channel(a, b, c)}, Proc.new {|a, b, c| partials2wave(a, b, c)}, Proc.new {|a, b, c| peak_env_info(a, b, c)}, Proc.new {|a, b, c| phase_partials2wave(a, b, c)}, Proc.new {|a, b, c| position2x(a, b, c)}, Proc.new {|a, b, c| position2y(a, b, c)}, Proc.new {|a, b, c| progress_report(a, b, c)}, Proc.new {|a, b, c| prompt_in_minibuffer(a, b, c)}, Proc.new {|a, b, c| ramp_channel(a, b, c)}, Proc.new {|a, b, c| read_peak_env_info_file(a, b, c)}, Proc.new {|a, b, c| redo_edit(a, b, c)}, Proc.new {|a, b, c| region_sample(a, b, c)}, Proc.new {|a, b, c| region_samples2vct(a, b, c)}, Proc.new {|a, b, c| reverse_channel(a, b, c)}, Proc.new {|a, b, c| reverse_sound(a, b, c)}, Proc.new {|a, b, c| sample(a, b, c)}, Proc.new {|a, b, c| sample2frame(a, b, c)}, Proc.new {|a, b, c| samples(a, b, c)}, Proc.new {|a, b, c| samples2sound_data(a, b, c)}, Proc.new {|a, b, c| samples2vct(a, b, c)}, Proc.new {|a, b, c| save_edit_history(a, b, c)}, Proc.new {|a, b, c| save_region(a, b, c)}, Proc.new {|a, b, c| save_selection(a, b, c)}, Proc.new {|a, b, c| scale_by(a, b, c)}, Proc.new {|a, b, c| scale_channel(a, b, c)}, Proc.new {|a, b, c| scale_sound_by(a, b, c)}, Proc.new {|a, b, c| scale_sound_to(a, b, c)}, Proc.new {|a, b, c| scale_to(a, b, c)}, Proc.new {|a, b, c| smooth_channel(a, b, c)}, Proc.new {|a, b, c| smooth_sound(a, b, c)}, Proc.new {|a, b, c| snd_spectrum(a, b, c)}, Proc.new {|a, b, c| sound_data2vct(a, b, c)}, Proc.new {|a, b, c| sound_data_ref(a, b, c)}, Proc.new {|a, b, c| spectrum(a, b, c)}, Proc.new {|a, b, c| swap_channels(a, b, c)}, Proc.new {|a, b, c| transform_sample(a, b, c)}, Proc.new {|a, b, c| transform_samples2vct(a, b, c)}, Proc.new {|a, b, c| unbind_key(a, b, c)}, Proc.new {|a, b, c| undo(a, b, c)}, Proc.new {|a, b, c| vct(a, b, c)}, Proc.new {|a, b, c| vct2channel(a, b, c)}, Proc.new {|a, b, c| vct2samples(a, b, c)}, Proc.new {|a, b, c| vct2sound_data(a, b, c)}, Proc.new {|a, b, c| vct2sound_file(a, b, c)}, Proc.new {|a, b, c| vct_add!(a, b, c)}, Proc.new {|a, b, c| vct_move!(a, b, c)}, Proc.new {|a, b, c| vct_set!(a, b, c)}, Proc.new {|a, b, c| vct_subseq(a, b, c)}, Proc.new {|a, b, c| waveshape(a, b, c)}, Proc.new {|a, b, c| write_peak_env_info_file(a, b, c)}, Proc.new {|a, b, c| x2position(a, b, c)}, Proc.new {|a, b, c| xramp_channel(a, b, c)}, Proc.new {|a, b, c| y2position(a, b, c)}]

all_args = [1.5, "/hiho", [0, 1], 1234, make_vct(3), make_color(0.95, 0.95, 0.95), :mus_error, Proc.new {|| true}, make_sound_data(2, 3), :order, 0, 1, -1, false, true, ?c, 0.0, 1.0, -1.0, nil, 3, 4, 2, 8, 16, 64]
length_all_args = 26


view_sound("obtest.snd")
tests = 10000

0.upto(tests) do |i|
  begin
    choice = kernel_rand(procs0_len)
    if (procs0[choice].nil?)
      print "procs0 nil: ", choice
    else
      procs0[choice].call()
      c_g?()
    end
  rescue
  end
end

0.upto(tests) do |i|
  begin
    choice = kernel_rand(set_procs0_len)
    if (set_procs0[choice].nil?)
      print "set_procs0 nil: ", choice, "\n"
    else
      arg = all_args[kernel_rand(length_all_args)]
      set_procs0[choice].call(arg)
      if (frames() > 4000000)
        print "->", frames(), "\n"
	c_g_!()
	end
      c_g?()
    end
  rescue
  end
end

0.upto(tests) do |i|
  begin
    choice = kernel_rand(procs1_len)
    if (procs1[choice].nil?)
      print "procs1 nil: ", choice, "\n"
    else
      arg = all_args[kernel_rand(length_all_args)]
      procs1[choice].call(arg)
      c_g?()
    end
  rescue
  end
end

0.upto(tests) do |i|
  begin
    choice = kernel_rand(set_procs1_len)
    if (set_procs1[choice].nil?)
      print "set_procs1 nil: ", choice
    else
      set_procs1[choice].call(all_args[kernel_rand(length_all_args)], all_args[kernel_rand(length_all_args)])
      c_g?()
    end
  rescue
  end
end

0.upto(tests) do |i|
  begin
    choice = kernel_rand(procs2_len)
    if (procs2[choice].nil?)
      print "procs2 nil: ", choice, "\n"
    else
      arg1 = all_args[kernel_rand(length_all_args)]
      arg2 = all_args[kernel_rand(length_all_args)]
      procs2[choice].call(arg1, arg2)
      c_g?()
    end
  rescue
  end
end

0.upto(tests) do |i|
  begin
    choice = kernel_rand(set_procs2_len)
    if (set_procs2[choice].nil?)
      print "set_procs2 nil: ", choice
    else
      set_procs2[choice].call(all_args[kernel_rand(length_all_args)], all_args[kernel_rand(length_all_args)], all_args[kernel_rand(length_all_args)])
      c_g?()
    end
  rescue
  end
end

0.upto(tests) do |i|
  begin
    choice = kernel_rand(procs3_len)
    if (procs3[choice].nil?)
      print "procs3 nil: ", choice
    else
      procs3[choice].call(all_args[kernel_rand(length_all_args)], all_args[kernel_rand(length_all_args)], all_args[kernel_rand(length_all_args)])
      c_g?()
    end
  rescue
  end
end

exit

