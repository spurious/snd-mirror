# snd.rb: Snd Ruby code and tests

set_window_width 800
set_window_height 500

set_listener_font "9x15"
set_help_text_font "9x15"
set_axis_numbers_font "9x15"

set_show_mix_waveforms #t
set_trap_segfault #f
# set_show_backtrace #t
# set_show_indices #t

# set_listener_prompt ":"
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

def provided?(feature) feature == $".find{|obj| obj == feature} end
# not sure this is kosher in Ruby -- I'm using $" via rb_provided as a feature list, but doc says its a filename array

def snd_display(str) 
  snd_print str
  $stderr.write str
end

def fneq(a, b)
  (a - b).abs > .001
end

# snd-test.scm translations
# ---------------------------------------- test 0 ----------------------------------------

if Enved_amplitude != 0 then snd_display sprintf("\n# Enved_amplitude => %d\n", Enved_amplitude) end
if Autocorrelation != 4 then snd_display sprintf("\n# Autocorrelation => %d\n", Autocorrelation) end
if Bartlett_window != 4 then snd_display sprintf("\n# Bartlett_window => %d\n", Bartlett_window) end
if Blackman2_window != 6 then snd_display sprintf("\n# Blackman2_window => %d\n", Blackman2_window) end
if Blackman3_window != 7 then snd_display sprintf("\n# Blackman3_window => %d\n", Blackman3_window) end
if Blackman4_window != 8 then snd_display sprintf("\n# Blackman4_window => %d\n", Blackman4_window) end
if Cauchy_window != 12 then snd_display sprintf("\n# Cauchy_window => %d\n", Cauchy_window) end
if Channels_combined != 1 then snd_display sprintf("\n# Channels_combined => %d\n", Channels_combined) end
if Channels_separate != 0 then snd_display sprintf("\n# Channels_separate => %d\n", Channels_separate) end
if Channels_superimposed != 2 then snd_display sprintf("\n# Channels_superimposed => %d\n", Channels_superimposed) end
if Chebyshev_transform != 5 then snd_display sprintf("\n# Chebyshev_transform => %d\n", Chebyshev_transform) end
if Cursor_in_middle != 3 then snd_display sprintf("\n# Cursor_in_middle => %d\n", Cursor_in_middle) end
if Cursor_in_view != 0 then snd_display sprintf("\n# Cursor_in_view => %d\n", Cursor_in_view) end
if Cursor_no_action != 5 then snd_display sprintf("\n# Cursor_no_action => %d\n", Cursor_no_action) end
if Cursor_on_left != 1 then snd_display sprintf("\n# Cursor_on_left => %d\n", Cursor_on_left) end
if Cursor_on_right != 2 then snd_display sprintf("\n# Cursor_on_right => %d\n", Cursor_on_right) end
if Cursor_update_display != 4 then snd_display sprintf("\n# Cursor_update_display => %d\n", Cursor_update_display) end
if Dolph_chebyshev_window != 16 then snd_display sprintf("\n# Dolph_chebyshev_window => %d\n", Dolph_chebyshev_window) end
if Exponential_window != 9 then snd_display sprintf("\n# Exponential_window => %d\n", Exponential_window) end
if Zoom_focus_active != 2 then snd_display sprintf("\n# Zoom_focus_active => %d\n", Zoom_focus_active) end
if Zoom_focus_left != 0 then snd_display sprintf("\n# Zoom_focus_left => %d\n", Zoom_focus_left) end
if Zoom_focus_middle != 3 then snd_display sprintf("\n# Zoom_focus_middle => %d\n", Zoom_focus_middle) end
if Zoom_focus_right != 1 then snd_display sprintf("\n# Zoom_focus_right => %d\n", Zoom_focus_right) end
if Fourier_transform != 0 then snd_display sprintf("\n# Fourier_transform => %d\n", Fourier_transform) end
if Gaussian_window != 14 then snd_display sprintf("\n# Gaussian_window => %d\n", Gaussian_window) end
if Graph_dots != 1 then snd_display sprintf("\n# Graph_dots => %d\n", Graph_dots) end
if Graph_dots_and_lines != 3 then snd_display sprintf("\n# Graph_dots_and_lines => %d\n", Graph_dots_and_lines) end
if Graph_filled != 2 then snd_display sprintf("\n# Graph_filled => %d\n", Graph_filled) end
if Graph_lines != 0 then snd_display sprintf("\n# Graph_lines => %d\n", Graph_lines) end
if Graph_lollipops != 4 then snd_display sprintf("\n# Graph_lollipops => %d\n", Graph_lollipops) end
if Hadamard_transform != 7 then snd_display sprintf("\n# Hadamard_transform => %d\n", Hadamard_transform) end
if Haar_transform != 8 then snd_display sprintf("\n# Haar_transform => %d\n", Haar_transform) end
if Hamming_window != 5 then snd_display sprintf("\n# Hamming_window => %d\n", Hamming_window) end
if Hankel_transform != 2 then snd_display sprintf("\n# Hankel_transform => %d\n", Hankel_transform) end
if Hanning_window != 1 then snd_display sprintf("\n# Hanning_window => %d\n", Hanning_window) end
if Kaiser_window != 11 then snd_display sprintf("\n# Kaiser_window => %d\n", Kaiser_window) end
if Keyboard_no_action != 6 then snd_display sprintf("\n# Keyboard_no_action => %d\n", Keyboard_no_action) end
if Cepstrum != 6 then snd_display sprintf("\n# Cepstrum => %d\n", Cepstrum) end
if Graph_transform_once != 0 then snd_display sprintf("\n# Graph_transform_once => %d\n", Graph_transform_once) end
if Parzen_window != 3 then snd_display sprintf("\n# Parzen_window => %d\n", Parzen_window) end
if Poisson_window != 13 then snd_display sprintf("\n# Poisson_window => %d\n", Poisson_window) end
if Rectangular_window != 0 then snd_display sprintf("\n# Rectangular_window => %d\n", Rectangular_window) end
if Riemann_window != 10 then snd_display sprintf("\n# Riemann_window => %d\n", Riemann_window) end
if Graph_transform_as_sonogram != 1 then snd_display sprintf("\n# Graph_transform_as_sonogram => %d\n", Graph_transform_as_sonogram) end
if Graph_transform_as_spectrogram != 2 then snd_display sprintf("\n# Graph_transform_as_spectrogram => %d\n", Graph_transform_as_spectrogram) end
if Graph_time_once != 0 then snd_display sprintf("\n# Graph_time_once => %d\n", Graph_time_once) end
if Graph_time_as_wavogram != 1 then snd_display sprintf("\n# Graph_time_as_wavogram => %d\n", Graph_time_as_wavogram) end
if Enved_spectrum != 1 then snd_display sprintf("\n# Enved_spectrum => %d\n", Enved_spectrum) end
if Speed_control_as_float != 0 then snd_display sprintf("\n# Speed_control_as_float => %d\n", Speed_control_as_float) end
if Speed_control_as_ratio != 1 then snd_display sprintf("\n# Speed_control_as_ratio => %d\n", Speed_control_as_ratio) end
if Speed_control_as_semitone != 2 then snd_display sprintf("\n# Speed_control_as_semitone => %d\n", Speed_control_as_semitone) end
if Enved_srate != 2 then snd_display sprintf("\n# Enved_srate => %d\n", Enved_srate) end
if Tukey_window != 15 then snd_display sprintf("\n# Tukey_window => %d\n", Tukey_window) end
if Walsh_transform != 3 then snd_display sprintf("\n# Walsh_transform => %d\n", Walsh_transform) end
if Wavelet_transform != 1 then snd_display sprintf("\n# Wavelet_transform => %d\n", Wavelet_transform) end
if Welch_window != 2 then snd_display sprintf("\n# Welch_window => %d\n", Welch_window) end
if Cursor_cross != 0 then snd_display sprintf("\n# Cursor_cross => %d\n", Cursor_cross) end
if Cursor_line != 1 then snd_display sprintf("\n# Cursor_line => %d\n", Cursor_line) end
if Dont_normalize_transform != 0 then snd_display sprintf("\n# Dont_normalize_transform => %d\n", Dont_normalize_transform) end
if Normalize_transform_by_channel != 1 then snd_display sprintf("\n# Normalize_transform_by_channel => %d\n", Normalize_transform_by_channel) end
if Normalize_transform_by_sound != 2 then snd_display sprintf("\n# Normalize_transform_by_sound => %d\n", Normalize_transform_by_sound) end
if Normalize_transform_globally != 3 then snd_display sprintf("\n# Normalize_transform_globally => %d\n", Normalize_transform_globally) end
if X_axis_in_samples != 1 then snd_display sprintf("\n# X_axis_in_samples => %d\n", X_axis_in_samples) end
if X_axis_in_beats != 4 then snd_display sprintf("\n# X_axis_in_beats => %d\n", X_axis_in_beats) end
if X_axis_in_seconds != 0 then snd_display sprintf("\n# X_axis_in_seconds => %d\n", X_axis_in_seconds) end
if X_axis_as_percentage != 2 then snd_display sprintf("\n# X_axis_as_percentage => %d\n", X_axis_as_percentage) end
if Enved_add_point != 0 then snd_display sprintf("\n# Enved_add_point => %d\n", Enved_add_point) end
if Enved_delete_point != 1 then snd_display sprintf("\n# Enved_delete_point => %d\n", Enved_delete_point) end
if Enved_move_point != 2 then snd_display sprintf("\n# Enved_move_point => %d\n", Enved_move_point) end
if Time_graph != 0 then snd_display sprintf("\n# Time_graph => %d\n", Time_graph) end
if Transform_graph != 1 then snd_display sprintf("\n# Transform_graph => %d\n", Transform_graph) end
if Lisp_graph != 2 then snd_display sprintf("\n# Lisp_graph => %d\n", Lisp_graph) end
if Copy_context != 0 then snd_display sprintf("\n# Copy_context => %d\n", Copy_context) end
if Cursor_context != 3 then snd_display sprintf("\n# Cursor_context => %d\n", Cursor_context) end
if Selection_context != 2 then snd_display sprintf("\n# Selection_context => %d\n", Selection_context) end
if Mus_next != 0 then snd_display sprintf("\n# Mus_next => %d\n", Mus_next) end
if Mus_aifc != 1 then snd_display sprintf("\n# Mus_aifc => %d\n", Mus_aifc) end
if Mus_riff != 2 then snd_display sprintf("\n# Mus_riff => %d\n", Mus_riff) end
if Mus_nist != 4 then snd_display sprintf("\n# Mus_nist => %d\n", Mus_nist) end
if Mus_raw != 10 then snd_display sprintf("\n# Mus_raw => %d\n", Mus_raw) end
if Mus_ircam != 14 then snd_display sprintf("\n# Mus_ircam => %d\n", Mus_ircam) end
if Mus_aiff != 56 then snd_display sprintf("\n# Mus_aiff => %d\n", Mus_aiff) end
if Mus_bicsf != 3 then snd_display sprintf("\n# Mus_bicsf => %d\n", Mus_bicsf) end
if Mus_voc != 8 then snd_display sprintf("\n# Mus_voc => %d\n", Mus_voc) end
if Mus_svx != 7 then snd_display sprintf("\n# Mus_svx => %d\n", Mus_svx) end
if Mus_soundfont != 31 then snd_display sprintf("\n# Mus_soundfont => %d\n", Mus_soundfont) end
if Mus_bshort != 1 then snd_display sprintf("\n# Mus_bshort => %d\n", Mus_bshort) end
if Mus_lshort != 10 then snd_display sprintf("\n# Mus_lshort => %d\n", Mus_lshort) end
if Mus_mulaw != 2 then snd_display sprintf("\n# Mus_mulaw => %d\n", Mus_mulaw) end
if Mus_alaw != 6 then snd_display sprintf("\n# Mus_alaw => %d\n", Mus_alaw) end
if Mus_byte != 3 then snd_display sprintf("\n# Mus_byte => %d\n", Mus_byte) end
if Mus_ubyte != 7 then snd_display sprintf("\n# Mus_ubyte => %d\n", Mus_ubyte) end
if Mus_bfloat != 4 then snd_display sprintf("\n# Mus_bfloat => %d\n", Mus_bfloat) end
if Mus_lfloat != 12 then snd_display sprintf("\n# Mus_lfloat => %d\n", Mus_lfloat) end
if Mus_bint != 5 then snd_display sprintf("\n# Mus_bint => %d\n", Mus_bint) end
if Mus_lint != 11 then snd_display sprintf("\n# Mus_lint => %d\n", Mus_lint) end
if Mus_bintn != 17 then snd_display sprintf("\n# Mus_bintn => %d\n", Mus_bintn) end
if Mus_lintn != 18 then snd_display sprintf("\n# Mus_lintn => %d\n", Mus_lintn) end
if Mus_b24int != 8 then snd_display sprintf("\n# Mus_b24int => %d\n", Mus_b24int) end
if Mus_l24int != 16 then snd_display sprintf("\n# Mus_l24int => %d\n", Mus_l24int) end
if Mus_bdouble != 9 then snd_display sprintf("\n# Mus_bdouble => %d\n", Mus_bdouble) end
if Mus_ldouble != 13 then snd_display sprintf("\n# Mus_ldouble => %d\n", Mus_ldouble) end
if Mus_ubshort != 14 then snd_display sprintf("\n# Mus_ubshort => %d\n", Mus_ubshort) end
if Mus_ulshort != 15 then snd_display sprintf("\n# Mus_ulshort => %d\n", Mus_ulshort) end
if Mus_bfloat_unscaled != 20 then snd_display sprintf("\n# Mus_bfloat_unscaled => %d\n", Mus_bfloat_unscaled) end
if Mus_lfloat_unscaled != 21 then snd_display sprintf("\n# Mus_lfloat_unscaled => %d\n", Mus_lfloat_unscaled) end
if Mus_bdouble_unscaled != 22 then snd_display sprintf("\n# Mus_bdouble_unscaled => %d\n", Mus_bdouble_unscaled) end
if Mus_ldouble_unscaled != 23 then snd_display sprintf("\n# Mus_ldouble_unscaled => %d\n", Mus_ldouble_unscaled) end
if Mus_audio_default != 0 then snd_display sprintf("\n# Mus_audio_default => %d\n", Mus_audio_default) end
if Mus_audio_duplex_default != 1 then snd_display sprintf("\n# Mus_audio_duplex_default => %d\n", Mus_audio_duplex_default) end
if Mus_audio_line_out != 4 then snd_display sprintf("\n# Mus_audio_line_out => %d\n", Mus_audio_line_out) end
if Mus_audio_line_in != 5 then snd_display sprintf("\n# Mus_audio_line_in => %d\n", Mus_audio_line_in) end
if Mus_audio_microphone != 6 then snd_display sprintf("\n# Mus_audio_microphone => %d\n", Mus_audio_microphone) end
if Mus_audio_speakers != 7 then snd_display sprintf("\n# Mus_audio_speakers => %d\n", Mus_audio_speakers) end
if Mus_audio_dac_out != 10 then snd_display sprintf("\n# Mus_audio_dac_out => %d\n", Mus_audio_dac_out) end
if Mus_audio_adat_in != 2 then snd_display sprintf("\n# Mus_audio_adat_in => %d\n", Mus_audio_adat_in) end
if Mus_audio_aes_in != 3 then snd_display sprintf("\n# Mus_audio_aes_in => %d\n", Mus_audio_aes_in) end
if Mus_audio_digital_in != 8 then snd_display sprintf("\n# Mus_audio_digital_in => %d\n", Mus_audio_digital_in) end
if Mus_audio_digital_out != 9 then snd_display sprintf("\n# Mus_audio_digital_out => %d\n", Mus_audio_digital_out) end
if Mus_audio_adat_out != 11 then snd_display sprintf("\n# Mus_audio_adat_out => %d\n", Mus_audio_adat_out) end
if Mus_audio_aes_out != 12 then snd_display sprintf("\n# Mus_audio_aes_out => %d\n", Mus_audio_aes_out) end
if Mus_audio_dac_filter != 13 then snd_display sprintf("\n# Mus_audio_dac_filter => %d\n", Mus_audio_dac_filter) end
if Mus_audio_mixer != 14 then snd_display sprintf("\n# Mus_audio_mixer => %d\n", Mus_audio_mixer) end
if Mus_audio_line1 != 15 then snd_display sprintf("\n# Mus_audio_line1 => %d\n", Mus_audio_line1) end
if Mus_audio_line2 != 16 then snd_display sprintf("\n# Mus_audio_line2 => %d\n", Mus_audio_line2) end
if Mus_audio_line3 != 17 then snd_display sprintf("\n# Mus_audio_line3 => %d\n", Mus_audio_line3) end
if Mus_audio_aux_input != 18 then snd_display sprintf("\n# Mus_audio_aux_input => %d\n", Mus_audio_aux_input) end
if Mus_audio_cd != 19 then snd_display sprintf("\n# Mus_audio_cd => %d\n", Mus_audio_cd) end
if Mus_audio_aux_output != 20 then snd_display sprintf("\n# Mus_audio_aux_output => %d\n", Mus_audio_aux_output) end
if Mus_audio_spdif_in != 21 then snd_display sprintf("\n# Mus_audio_spdif_in => %d\n", Mus_audio_spdif_in) end
if Mus_audio_spdif_out != 22 then snd_display sprintf("\n# Mus_audio_spdif_out => %d\n", Mus_audio_spdif_out) end
if Mus_audio_amp != 23 then snd_display sprintf("\n# Mus_audio_amp => %d\n", Mus_audio_amp) end
if Mus_audio_srate != 24 then snd_display sprintf("\n# Mus_audio_srate => %d\n", Mus_audio_srate) end
if Mus_audio_channel != 25 then snd_display sprintf("\n# Mus_audio_channel => %d\n", Mus_audio_channel) end
if Mus_audio_format != 26 then snd_display sprintf("\n# Mus_audio_format => %d\n", Mus_audio_format) end
if Mus_audio_port != 37 then snd_display sprintf("\n# Mus_audio_port => %d\n", Mus_audio_port) end
if Mus_audio_imix != 27 then snd_display sprintf("\n# Mus_audio_imix => %d\n", Mus_audio_imix) end
if Mus_audio_igain != 28 then snd_display sprintf("\n# Mus_audio_igain => %d\n", Mus_audio_igain) end
if Mus_audio_reclev != 29 then snd_display sprintf("\n# Mus_audio_reclev => %d\n", Mus_audio_reclev) end
if Mus_audio_pcm != 30 then snd_display sprintf("\n# Mus_audio_pcm => %d\n", Mus_audio_pcm) end
if Mus_audio_pcm2 != 31 then snd_display sprintf("\n# Mus_audio_pcm2 => %d\n", Mus_audio_pcm2) end
if Mus_audio_ogain != 32 then snd_display sprintf("\n# Mus_audio_ogain => %d\n", Mus_audio_ogain) end
if Mus_audio_line != 33 then snd_display sprintf("\n# Mus_audio_line => %d\n", Mus_audio_line) end
if Mus_audio_synth != 34 then snd_display sprintf("\n# Mus_audio_synth => %d\n", Mus_audio_synth) end
if Mus_audio_bass != 35 then snd_display sprintf("\n# Mus_audio_bass => %d\n", Mus_audio_bass) end
if Mus_audio_treble != 36 then snd_display sprintf("\n# Mus_audio_treble => %d\n", Mus_audio_treble) end
if Mus_audio_direction != 39 then snd_display sprintf("\n# Mus_audio_direction => %d\n", Mus_audio_direction) end
if Mus_audio_samples_per_channel != 38 then snd_display sprintf("\n# Mus_audio_samples_per_channel => %d\n", Mus_audio_samples_per_channel) end


# ---------------------------------------- test 1 ----------------------------------------

if ask_before_overwrite != false then snd_display sprintf("\n# ask_before_overwrite: %s\n", ask_before_overwrite) end
if audio_output_device != 0 then snd_display sprintf("\n# audio_output_device: %s\n", audio_output_device) end
if audio_state_file != ".snd-mixer" then snd_display sprintf("\n# audio_state_file: %s\n", audio_state_file) end
if auto_resize != true then snd_display sprintf("\n# auto_resize: %s\n", auto_resize) end
if auto_update != false then snd_display sprintf("\n# auto_update: %s\n", auto_update) end
if channel_style != 0 then snd_display sprintf("\n# channel_style: %s\n", channel_style) end
if fneq(color_cutoff, 0.003) then snd_display sprintf("\n# color_cutoff: %s\n", color_cutoff) end
if color_inverted != true then snd_display sprintf("\n# color_inverted: %s\n", color_inverted) end
if color_scale != 1.0 then snd_display sprintf("\n# color_scale: %s\n", color_scale) end
if colormap != -1 then snd_display sprintf("\n# colormap: %s\n", colormap) end
if auto_update_interval != 60.0 then snd_display sprintf("\n# auto_update_interval: %s\n", auto_update_interval) end
if dac_combines_channels != true then snd_display sprintf("\n# dac_combines_channels: %s\n", dac_combines_channels) end
if emacs_style_save_as != false then snd_display sprintf("\n# emacs_style_save_as: %s\n", emacs_style_save_as) end
if dac_size != 256 then snd_display sprintf("\n# dac_size: %s\n", dac_size) end
if minibuffer_history_length != 8 then snd_display sprintf("\n# minibuffer_history_length: %s\n", minibuffer_history_length) end
if data_clipped != false then snd_display sprintf("\n# data_clipped: %s\n", data_clipped) end
if default_output_chans != 1 then snd_display sprintf("\n# default_output_chans: %s\n", default_output_chans) end
if default_output_format != 1 then snd_display sprintf("\n# default_output_format: %s\n", default_output_format) end
if default_output_srate != 22050 then snd_display sprintf("\n# default_output_srate: %s\n", default_output_srate) end
if default_output_type != 0 then snd_display sprintf("\n# default_output_type: %s\n", default_output_type) end
if dot_size != 1 then snd_display sprintf("\n# dot_size: %s\n", dot_size) end
if enved_base != 1.0 then snd_display sprintf("\n# enved_base: %s\n", enved_base) end
if enved_clip? != false then snd_display sprintf("\n# enved_clip?: %s\n", enved_clip?) end
if enved_filter_order != 40 then snd_display sprintf("\n# enved_filter_order: %s\n", enved_filter_order) end
if enved_filter != true then snd_display sprintf("\n# enved_filter: %s\n", enved_filter) end
if enved_in_dB != false then snd_display sprintf("\n# enved_in_dB: %s\n", enved_in_dB) end
if enved_exp? != false then snd_display sprintf("\n# enved_exp?: %s\n", enved_exp?) end
if enved_power != 3.0 then snd_display sprintf("\n# enved_power: %s\n", enved_power) end
if enved_target != 0 then snd_display sprintf("\n# enved_target: %s\n", enved_target) end
if enved_wave? != false then snd_display sprintf("\n# enved_wave?: %s\n", enved_wave?) end
if enved_active_env != nil then snd_display sprintf("\n# enved_active_env: %s\n", enved_active_env) end
if enved_selected_env != nil then snd_display sprintf("\n# enved_selected_env: %s\n", enved_selected_env) end
if eps_file != "snd.eps" then snd_display sprintf("\n# eps_file: %s\n", eps_file) end
if eps_bottom_margin != 0.0 then snd_display sprintf("\n# eps_bottom_margin: %s\n", eps_bottom_margin) end
if eps_left_margin != 0.0 then snd_display sprintf("\n# eps_left_margin: %s\n", eps_left_margin) end
if eps_size != 1.0 then snd_display sprintf("\n# eps_size: %s\n", eps_size) end
if fft_window_beta != 0.0 then snd_display sprintf("\n# fft_window_beta: %s\n", fft_window_beta) end
if fft_log_frequency != false then snd_display sprintf("\n# fft_log_frequency: %s\n", fft_log_frequency) end
if fft_log_magnitude != false then snd_display sprintf("\n# fft_log_magnitude: %s\n", fft_log_magnitude) end
if transform_size != 256 then snd_display sprintf("\n# transform_size: %s\n", transform_size) end
if transform_graph_type != 0 then snd_display sprintf("\n# transform_graph_type: %s\n", transform_graph_type) end
if fft_window != 6 then snd_display sprintf("\n# fft_window: %s\n", fft_window) end
if filter_env_in_hz != false then snd_display sprintf("\n# filter_env_in_hz: %s\n", filter_env_in_hz) end
if graph_cursor != 34 then snd_display sprintf("\n# graph_cursor: %s\n", graph_cursor) end
if graph_style != 0 then snd_display sprintf("\n# graph_style: %s\n", graph_style) end
if graphs_horizontal != true then snd_display sprintf("\n# graphs_horizontal: %s\n", graphs_horizontal) end
if hankel_jn != 0.0 then snd_display sprintf("\n# hankel_jn: %s\n", hankel_jn) end
if just_sounds != false then snd_display sprintf("\n# just_sounds: %s\n", just_sounds) end
if listener_prompt != ">" then snd_display sprintf("\n# listener_prompt: %s\n", listener_prompt) end
if max_transform_peaks != 100 then snd_display sprintf("\n# max_transform_peaks: %s\n", max_transform_peaks) end
if max_regions != 16 then snd_display sprintf("\n# max_regions: %s\n", max_regions) end
if min_dB != -60.0 then snd_display sprintf("\n# min_dB: %s\n", min_dB) end
if movies != true then snd_display sprintf("\n# movies: %s\n", movies) end
if selection_creates_region != true then snd_display sprintf("\n# selection_creates_region: %s\n", selection_creates_region) end
if transform_normalization != Normalize_transform_by_channel then snd_display sprintf("\n# transform_normalization: %s\n", transform_normalization) end
if previous_files_sort != 0 then snd_display sprintf("\n# previous_files_sort: %s\n", previous_files_sort) end
if print_length != 12 then snd_display sprintf("\n# print_length: %s\n", print_length) end
if recorder_autoload != false then snd_display sprintf("\n# recorder_autoload: %s\n", recorder_autoload) end
if recorder_buffer_size != 4096 then snd_display sprintf("\n# recorder_buffer_size: %s\n", recorder_buffer_size) end
if recorder_file != "" then snd_display sprintf("\n# recorder_file: %s\n", recorder_file) end
if recorder_max_duration != 1000000.0 then snd_display sprintf("\n# recorder_max_duration: %s\n", recorder_max_duration) end
if recorder_out_chans != 2 then snd_display sprintf("\n# recorder_out_chans: %s\n", recorder_out_chans) end
if recorder_srate != 22050 then snd_display sprintf("\n# recorder_srate: %s\n", recorder_srate) end
if recorder_trigger != 0.0 then snd_display sprintf("\n# recorder_trigger: %s\n", recorder_trigger) end
if region_graph_style != Graph_lines then snd_display sprintf("\n# region_graph_style: %s\n", region_graph_style) end
if reverb_control_decay != 1.0 then snd_display sprintf("\n# reverb_control_decay: %s\n", reverb_control_decay) end
if save_state_file != "saved-snd.rb" then snd_display sprintf("\n# save_state_file: %s\n", save_state_file) end
if show_axes != 1 then snd_display sprintf("\n# show_axes: %s\n", show_axes) end
if show_transform_peaks != false then snd_display sprintf("\n# show_transform_peaks: %s\n", show_transform_peaks) end
if show_indices != false then snd_display sprintf("\n# show_indices: %s\n", show_indices) end
if show_backtrace != false then snd_display sprintf("\n# show_backtrace: %s\n", show_backtrace) end
if show_marks != true then snd_display sprintf("\n# show_marks: %s\n", show_marks) end
if show_mix_waveforms != true then snd_display sprintf("\n# show_mix_waveforms: %s\n", show_mix_waveforms) end
if show_selection_transform != false then snd_display sprintf("\n# show_selection_transform: %s\n", show_selection_transform) end
if show_usage_stats != false then snd_display sprintf("\n# show_usage_stats: %s\n", show_usage_stats) end
if show_y_zero != false then snd_display sprintf("\n# show_y_zero: %s\n", show_y_zero) end
if sinc_width != 10 then snd_display sprintf("\n# sinc_width: %s\n", sinc_width) end
if spectro_cutoff != 1.0 then snd_display sprintf("\n# spectro_cutoff: %s\n", spectro_cutoff) end
if spectro_hop != 4 then snd_display sprintf("\n# spectro_hop: %s\n", spectro_hop) end
if spectro_start != 0.0 then snd_display sprintf("\n# spectro_start: %s\n", spectro_start) end
if spectro_x_angle != 90.0 then snd_display sprintf("\n# spectro_x_angle: %s\n", spectro_x_angle) end
if spectro_x_scale != 1.0 then snd_display sprintf("\n# spectro_x_scale: %s\n", spectro_x_scale) end
if spectro_y_angle != 0.0 then snd_display sprintf("\n# spectro_y_angle: %s\n", spectro_y_angle) end
if spectro_y_scale != 1.0 then snd_display sprintf("\n# spectro_y_scale: %s\n", spectro_y_scale) end
if spectro_z_angle != -2.0 then snd_display sprintf("\n# spectro_z_angle: %s\n", spectro_z_angle) end
if fneq(spectro_z_scale, 0.1) then snd_display sprintf("\n# spectro_z_scale: %s\n", spectro_z_scale) end
if speed_control_style != 0 then snd_display sprintf("\n# speed_control_style: %s\n", speed_control_style) end
if speed_control_tones != 12 then snd_display sprintf("\n# speed_control_tones: %s\n", speed_control_tones) end
if temp_dir != "" then snd_display sprintf("\n# temp_dir: %s\n", temp_dir) end
if ladspa_dir != "" then snd_display sprintf("\n# ladspa_dir: %s\n", ladspa_dir) end
if tiny_font != "6x12" then snd_display sprintf("\n# tiny_font: %s\n", tiny_font) end
if transform_type != 0 then snd_display sprintf("\n# transform_type: %s\n", transform_type) end
if trap_segfault != false then snd_display sprintf("\n# trap_segfault: %s\n", trap_segfault) end
if use_sinc_interp != true then snd_display sprintf("\n# use_sinc_interp: %s\n", use_sinc_interp) end
if verbose_cursor != false then snd_display sprintf("\n# verbose_cursor: %s\n", verbose_cursor) end
if vu_font != "" then snd_display sprintf("\n# vu_font: %s\n", vu_font) end
if vu_font_size != 1.0 then snd_display sprintf("\n# vu_font_size: %s\n", vu_font_size) end
if vu_size != 1.0 then snd_display sprintf("\n# vu_size: %s\n", vu_size) end
if wavelet_type != 0 then snd_display sprintf("\n# wavelet_type: %s\n", wavelet_type) end
if time_graph_type != Graph_time_once then snd_display sprintf("\n# time_graph_type: %s\n", time_graph_type) end
if wavo_hop != 3 then snd_display sprintf("\n# wavo_hop: %s\n", wavo_hop) end
if wavo_trace != 64 then snd_display sprintf("\n# wavo_trace: %s\n", wavo_trace) end
if x_axis_style != 0 then snd_display sprintf("\n# x_axis_style: %s\n", x_axis_style) end
if beats_per_minute != 60.0 then snd_display sprintf("\n# beats_per_minute: %s\n", beats_per_minute) end
if zero_pad != 0 then snd_display sprintf("\n# zero_pad: %s\n", zero_pad) end
if zoom_focus_style != 2 then snd_display sprintf("\n# zoom_focus_style: %s\n", zoom_focus_style) end
if mix_waveform_height != 20 then snd_display sprintf("\n# mix_waveform_height: %s\n", mix_waveform_height) end
if mix_tag_width != 6 then snd_display sprintf("\n# mix_tag_width: %s\n", mix_tag_width) end
if mix_tag_height != 14 then snd_display sprintf("\n# mix_tag_height: %s\n", mix_tag_height) end
if audio_output_device != 0 then snd_display sprintf("\n# audio_output_device: %s\n", audio_output_device) end
if selected_mix != -1 then snd_display sprintf("\n# selected_mix: %s\n", selected_mix) end

# ---------------------------------------- test 2 ----------------------------------------

def test_headers(name, chns, sr, dur, typ, frm)
  file = "/home/bil/sf1/" << name
  if mus_sound_chans(file) != chns then snd_display sprintf("\n# %s: chans: %d %d\n", name, chns, mus_sound_chans(file)) end
  if mus_sound_srate(file) != sr then snd_display sprintf("\n# %s: srate: %d %d\n", name, sr, mus_sound_srate(file)) end
  if fneq(mus_sound_duration(file), dur) then snd_display sprintf("\n# %s: duration: %f %f\n", name, dur, mus_sound_duration(file)) end
  if (mus_sound_data_format(file) != -1) and
     (mus_sound_header_type(file) != 33) and
     ((mus_sound_length(file) + 1) < (mus_sound_datum_size(file) * mus_sound_chans(file) * mus_sound_duration(file) * mus_sound_srate(file)))
    then snd_display sprintf("\n# %s: length: %d (%d: %d * %d * %f * %d)\n", 
			   name,
			   mus_sound_length(file),
			   mus_sound_datum_size(file) * mus_sound_chans(file) * mus_sound_duration(file) * mus_sound_srate(file),
			   mus_sound_datum_size(file), mus_sound_chans(file), mus_sound_duration(file), mus_sound_srate(file))
  end
  if ((mus_sound_frames(file) - (mus_sound_samples(file) / mus_sound_chans(file))) > 1)
    then snd_display sprintf("\n# %s: frames: %d %d\n",
		      name,
                      mus_sound_frames(file),
		      mus_sound_samples(file) / mus_sound_chans(file))
  end

  if (mus_header_type_name(mus_sound_header_type(file)) != typ)
    then snd_display sprintf("\n# %s: type: %s %s\n",
		   	   name,
			   mus_header_type_name(mus_sound_header_type(file)),
			   typ)
  end
  if (mus_data_format_name(mus_sound_data_format(file)) != frm)
    then snd_display sprintf("\n# %s: format: [%s] [%s]\n",
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
test_headers("ce-c3.w02", 1, 33000, 3.88848495483398, "TX-16", "unsupported")
test_headers("ce-c4.w03", 1, 33000, 2.91618180274963, "TX-16", "unsupported")
test_headers("ce-d2.w01", 1, 33000, 3.46439385414124, "TX-16", "unsupported")
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
test_headers("d40130.w00", 1, 16000, 0.0625, "TX-16", "unsupported")
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
test_headers("wood.w00", 1, 16000, 0.101374998688698, "TX-16", "unsupported")
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
test_headers("zulu_a4.w11", 1, 33000, 1.21987879276276, "TX-16", "unsupported")

# ---------------------------------------- test 3 ----------------------------------------

oboe = open_sound "oboe.snd"
td = temp_dir     
set_temp_dir "/hoho/wasup"
if temp_dir != "/hoho/wasup" then snd_display sprintf("\n# temp_dir: %s?\n", temp_dir) end
set_temp_dir td
if fneq(sample(1000), 0.0328) then snd_display sprintf("\n# sample: %s\n", sample(1000)); end
set_show_controls #t
if provided? "snd-nogui" then
  enved_dialog
  if !dialog_widgets[2] then snd_display sprintf("\n# enved_dialog?") end
  


end
close_sound oboe
dismiss_all_dialogs
