# snd_test.rb: Snd Ruby code and tests
#
#  test 0: constants
#  test 1: defaults
#  test 2: headers
#  test 3: variables
#  test 4: sndlib
#  test 5: simple overall checks
#  test 6: vcts
#  test 7: colors
#  test 8: clm
#  test 9: mix
#  test 10: marks
#  test 11: dialogs
#  test 12: extensions
#  test 13: menus, edit lists, hooks, etc
#  test 14: all together now
#  test 15: chan-local vars
#  test 16: regularized funcs
#  test 17: dialogs and graphics
#  test 18: enved
#  test 19: save and restore
#  test 20: transforms
#  test 21: new stuff
#  test 22: run
#  test 23: with-sound
#  test 24: user-interface
#  test 25: X/Xt/Xm
#  test 26: Gtk
#  test 27: GL
#  test 28: errors
#  test all done

def provided?(feature)
  $".member?(feature)
end

def snd_display(*args)
  str = format(*args)
  snd_print str
  unless provided? "snd-nogui"
    $stderr.write str
  end
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

$snd_test = -1 unless defined? $snd_test
$with_exit = ($snd_test < 0) unless defined? $with_exit
$full_test = ($snd_test < 0)
$total_test = 28

$sample_reader_tests = 300
$original_prompt = listener_prompt
$default_file_buffer_size = 65536

set_mus_file_buffer_size($default_file_buffer_size)
set_with_background_processes(false)
set_show_backtrace(true)

def mus_audio_playback_amp
  vals = make_vct(32)
  mus_audio_mixer_read(Mus_audio_default, Mus_audio_amp, 0, vals)
  ch0_amp = vals[0]
  mus_audio_mixer_read(Mus_audio_default, Mus_audio_amp, 1, vals)
  ch1_amp = vals[0]
  [ch0_amp, ch1_amp]
end

def mus_audio_playback_amp=(val)
  vals = make_vct(32)
  vals[0] = val
  mus_audio_mixer_write(Mus_audio_default, Mus_audio_amp, 0, vals)
  mus_audio_mixer_write(Mus_audio_default, Mus_audio_amp, 1, vals)
  val
end

mus_audio_playback_amp = 0.0

def make_color_with_catch(c1, c2, c3)
  make_color(c1, c2, c3)
rescue
  make_color(1, 0, 0)
end

def without_errors(&body)
  body.call
rescue
  # snd_display("\n# without_errors: %s", $!.inspect)
  $!.to_s.split[1].chop.intern
end

show_listener
set_window_x(600)
set_window_y(10)

require "examp.rb"
require "ws.rb"
require "hooks.rb"
unless provided? "snd-nogui"
  # if libxm.so is a separate module
  if provided?("snd-motif") and (not provided?("xm"))
    require "libxm.so"
  end
  if provided?("snd-gtk") and (not provided?("xg"))
    # libxg.so can't be loaded under that name because it lacks a
    # function Init_libxg(). So I symlinked it to
    # site_ruby/.../gtk/libxm.so
    require "gtk/libxm.so"                        # Attention!!!
  end
  require "snd-xm.rb"
  require "popup.rb"
end

def my_random(n)
  if n.zero?
    0.0
  else
    if n < 0
      -mus_random(n).abs
    else
      mus_random(n).abs
    end
  end
end

def irandom(n)
  my_random(n).to_i
end

def real_time
  process_times.utime
end

def hundred(n)
  (100.0 * n).to_i
end

def with_time(&body)
  rst = Time.new
  pst = process_times
  body.call
  red = Time.new
  ped = process_times
  snd_display("\n# real: %7.4f, utime: %7.4f, stime: %7.4f",
              red - rst, ped.utime - pst.utime, ped.stime - pst.stime)
end

snd_display("# Snd version: %s", snd_version)

$test_number = -1
$timings = Array.new($total_test + 1, 0.0)

$before_test_hook = Hook.new("$before_test_hook", 1)
$after_test_hook = Hook.new("$after_test_hook", 1)

$before_test_hook.add_hook!("snd-test") do |n|
  $test_number = n
  if n > 0
    $timings[n - 1] = hundred(real_time - $timings[n - 1])
  end
  $timings[n] = real_time
  snd_display("\n# test %d", n)
  set_show_backtrace(false)
end

$after_test_hook.add_hook!("snd-test") do |n|
  if sounds
    snd_display("\n# test %d: open sounds: %s",
                n, sounds.map do |snd| short_file_name(snd) end.inspect)
    sounds.each do |snd| close_sound(snd) end
  end
  snd_display("\n# test %d done\n#", n)
end

$overall_start_time = real_time
snd_display("\n# %s\n#", Time.new.strftime("%d-%b %H:%M %Z"))

# snd-test.scm translations
# ---------------- test 0: constants ----------------

# list = [[:Symbol, value], ...]
def test00(lst, exec = false)
  if exec
    # global variable functions
    lst.each do |sym, val|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = send(format("set_%s", sym).intern, send(sym))) != val
          snd_display("\n# %s set def: %s", sym, res.inspect)
        end
      rescue
        snd_display("\n# %s set def: %s (%s)", sym, res.inspect, $!.inspect)
      end
    end
  else
    # constants
    lst.each do |sym, val|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = eval(sym.to_s)) != val
          snd_display("\n# %s => %s", sym, res.inspect)
        end
      rescue
        snd_display("\n# %s => %s (%s)", sym, res.inspect, $!.inspect)
      end
    end
  end
end

def test0(consts, defs)
  test00(consts)
  test00(defs, true)
  set_max_transform_peaks(-123)
  set_zero_pad(-123)
  test00([[:max_transform_peaks, 100], [:zero_pad, 0]], true)
  # XEN_EMPTY_LIST is Qnil
  if zero_pad(true, true) != nil
    snd_display("\n# zero_pad(true, true) set def: %s", zero_pad(true, true).inspect)
  end
  if provided?("snd-motif")
    [:axis_label_font,
      :axis_numbers_font,
      :tiny_font,
      :peaks_font,
      :bold_peaks_font].each do |sym|
      val = send(sym)
      if (res = send(format("set_%s", sym.to_s), "8x123")) != val
        snd_display("\n# set %s to bogus value: %s %s", sym.inspect, val.inspect, res.inspect)
      end
    end
  end
  unless provided?("snd-nogui")
    # XEN_EMPTY_LIST is Qnil
    # set_enved_envelope([]) sets enved_envelope to nil
    set_enved_envelope((enved_envelope or []))
    if enved_envelope != nil
      snd_display("\n# enved_envelope set def: %s", enved_envelope.inspect)
    end
  end
end

consts = [
  [:Enved_amplitude, 0],
  [:Autocorrelation, 3],
  [:Bartlett_window, 4],
  [:Blackman2_window, 6],
  [:Blackman3_window, 7],
  [:Blackman4_window, 8],
  [:Cauchy_window, 12],
  [:Channels_combined, 1],
  [:Channels_separate, 0],
  [:Channels_superimposed, 2],
  [:Connes_window, 18],
  [:Cursor_in_middle, 3],
  [:Cursor_in_view, 0],
  [:Cursor_on_left, 1],
  [:Cursor_on_right, 2],
  [:Dolph_chebyshev_window, 16],
  [:Exponential_window, 9],
  [:Zoom_focus_active, 2],
  [:Zoom_focus_left, 0],
  [:Zoom_focus_middle, 3],
  [:Zoom_focus_right, 1],
  [:Fourier_transform, 0],
  [:Gaussian_window, 14],
  [:Graph_dots, 1],
  [:Graph_dots_and_lines, 3],
  [:Graph_filled, 2],
  [:Graph_lines, 0],
  [:Graph_lollipops, 4],
  [:Haar_transform, 5],
  [:Hamming_window, 5],
  [:Hann_window, 1],
  [:Hann_poisson_window, 17],
  [:Kaiser_window, 11],
  [:Keyboard_no_action, 4],
  [:Cepstrum, 4],
  [:Parzen_window, 3],
  [:Poisson_window, 13],
  [:Rectangular_window, 0],
  [:Riemann_window, 10],
  [:Graph_as_sonogram, 1],
  [:Graph_as_spectrogram, 2],
  [:Graph_once, 0],
  [:Graph_as_wavogram, 3],
  [:Enved_spectrum, 1],
  [:Speed_control_as_float, 0],
  [:Speed_control_as_ratio, 1],
  [:Speed_control_as_semitone, 2],
  [:Enved_srate, 2],
  [:Tukey_window, 15],
  [:Walsh_transform, 2],
  [:Wavelet_transform, 1],
  [:Welch_window, 2],
  [:Cursor_cross, 0],
  [:Cursor_line, 1],
  [:Dont_normalize, 0],
  [:Envelope_linear, 0],
  [:Envelope_exponential, 1],
  [:Normalize_by_channel, 1],
  [:Normalize_by_sound, 2],
  [:Normalize_globally, 3],
  [:X_axis_in_samples, 1],
  [:X_axis_in_beats, 3],
  [:X_axis_in_seconds, 0],
  [:X_axis_as_percentage, 2],
  [:Enved_add_point, 0],
  [:Enved_delete_point, 1],
  [:Enved_move_point, 2],
  unless provided?("snd-nogui")
    [:Time_graph, 0]
  end,
  unless provided?("snd-nogui")
    [:Transform_graph, 1]
  end,
  unless provided?("snd-nogui")
    [:Lisp_graph, 2]
  end,
  [:Copy_context, 0],
  [:Cursor_context, 3],
  [:Selection_context, 2],
  [:Mark_context, 4],
  [:Show_no_axes, 0],
  [:Show_all_axes, 1],
  [:Show_x_axis, 2],
  [:Show_all_axes_unlabelled, 3],
  [:Show_x_axis_unlabelled, 4],
  # sndlib constants
  [:Mus_unsupported, 0],
  [:Mus_next, 1],
  [:Mus_aifc, 2],
  [:Mus_riff, 3],
  [:Mus_nist, 5],
  [:Mus_raw, 11],
  [:Mus_ircam, 14],
  [:Mus_aiff, 48],
  [:Mus_bicsf, 4],
  [:Mus_voc, 9],
  [:Mus_svx, 8],
  [:Mus_soundfont, 25],
  #
  [:Mus_interp_none, 0],
  [:Mus_interp_linear, 1],
  [:Mus_interp_sinusoidal, 2],
  [:Mus_interp_all_pass, 3],
  [:Mus_interp_lagrange, 4],
  [:Mus_interp_bezier, 5],
  [:Mus_interp_hermite, 6],
  #
  [:Mus_unknown, 0],
  [:Mus_bshort, 1],
  [:Mus_lshort, 10],
  [:Mus_mulaw, 2],
  [:Mus_alaw, 6],
  [:Mus_byte, 3],
  [:Mus_ubyte, 7],
  [:Mus_bfloat, 4],
  [:Mus_lfloat, 12],
  [:Mus_bint, 5],
  [:Mus_lint, 11],
  [:Mus_bintn, 17],
  [:Mus_lintn, 18],
  [:Mus_b24int, 8],
  [:Mus_l24int, 16],
  [:Mus_bdouble, 9],
  [:Mus_ldouble, 13],
  [:Mus_ubshort, 14],
  [:Mus_ulshort, 15],
  [:Mus_bfloat_unscaled, 19],
  [:Mus_lfloat_unscaled, 20],
  [:Mus_bdouble_unscaled, 21],
  [:Mus_ldouble_unscaled, 22],
  #
  [:Mus_audio_default, 0],
  [:Mus_audio_duplex_default, 1],
  [:Mus_audio_line_out, 4],
  [:Mus_audio_line_in, 5],
  [:Mus_audio_microphone, 6],
  [:Mus_audio_speakers, 7],
  [:Mus_audio_adat_in, 2],
  [:Mus_audio_aes_in, 3],
  [:Mus_audio_digital_in, 8],
  [:Mus_audio_digital_out, 9],
  [:Mus_audio_adat_out, 11],
  [:Mus_audio_aes_out, 12],
  [:Mus_audio_dac_filter, 13],
  [:Mus_audio_mixer, 14],
  [:Mus_audio_line1, 15],
  [:Mus_audio_line2, 16],
  [:Mus_audio_line3, 17],
  [:Mus_audio_aux_input, 18],
  [:Mus_audio_cd, 19],
  [:Mus_audio_aux_output, 20],
  [:Mus_audio_spdif_in, 21],
  [:Mus_audio_spdif_out, 22],
  [:Mus_audio_amp, 23],
  [:Mus_audio_srate, 24],
  [:Mus_audio_channel, 25],
  [:Mus_audio_format, 26],
  [:Mus_audio_port, 37],
  [:Mus_audio_imix, 27],
  [:Mus_audio_igain, 28],
  [:Mus_audio_reclev, 29],
  [:Mus_audio_pcm, 30],
  [:Mus_audio_pcm2, 31],
  [:Mus_audio_ogain, 32],
  [:Mus_audio_line, 33],
  [:Mus_audio_synth, 34],
  [:Mus_audio_bass, 35],
  [:Mus_audio_treble, 36],
  [:Mus_audio_direction, 39],
  [:Mus_audio_samples_per_channel, 38]]

defs = [
  [:region_graph_style, Graph_lines],
  [:ask_before_overwrite, false],
  [:audio_output_device, 0],
  [:auto_resize, true],
  [:auto_update, false],
  [:channel_style, 0],
  [:color_cutoff, 0.003],
  [:color_inverted, true],
  [:color_scale, 1.0],
  [:auto_update_interval, 60.0],
  [:cursor_update_interval, 0.05],
  [:cursor_location_offset, 0],
  [:dac_combines_channels, true],
  [:dac_size, 256],
  [:minibuffer_history_length, 8],
  [:data_clipped, false],
  [:default_output_chans, 1],
  [:default_output_format, Mus_bshort],
  [:default_output_srate, 22050],
  [:default_output_type, Mus_next],
  [:dot_size, 1],
  [:cursor_size, 15],
  [:cursor_style, Cursor_cross],
  [:enved_base, 1.0],
  [:enved_clip?, false],
  unless provided?("snd-nogui")
    [:enved_filter, true]
  end,
  [:enved_filter_order, 40],
  [:enved_in_dB, false],
  [:enved_style, Envelope_linear],
  [:enved_power, 3.0],
  [:enved_target, 0],
  [:enved_wave?, false],
  [:eps_file, "snd.eps"],
  [:eps_bottom_margin, 0.0],
  [:eps_left_margin, 0.0],
  [:eps_size, 1.0],
  [:fft_window_beta, 0.0],
  [:fft_log_frequency, false],
  [:fft_log_magnitude, false],
  [:transform_size, 512],
  [:transform_graph_type, 0],
  [:fft_window, 6],
  unless provided?("snd-nogui")
    [:graph_cursor, 34]
  end,
  [:graph_style, Graph_lines],
  [:graphs_horizontal, true],
  [:html_dir, "."],
  [:html_program, "mozilla"],
  [:just_sounds, false],
  [:listener_prompt, ">"],
  [:max_transform_peaks, 100],
  [:max_regions, 16],
  [:min_dB, -60.0],
  [:log_freq_start, 32.0],
  [:selection_creates_region, true],
  [:transform_normalization, Normalize_by_channel],
  [:previous_files_sort, 0],
  [:print_length, 12],
  [:recorder_autoload, false],
  [:recorder_buffer_size, 4096],
  [:recorder_file, ""],
  [:recorder_max_duration, 1000000.0],
  [:recorder_out_chans, 2],
  [:recorder_in_chans, 0],
  [:recorder_srate, 22050],
  [:recorder_trigger, 0.0],
  [:save_state_file, "saved-snd.rb"],
  [:show_axes, 1],
  [:show_transform_peaks, false],
  [:show_indices, false],
  [:show_backtrace, false],
  [:show_marks, true],
  [:show_mix_waveforms, true],
  [:show_selection_transform, false],
  [:show_y_zero, false],
  [:show_grid, false],
  [:grid_density, 1.0],
  [:show_sonogram_cursor, false],
  [:sinc_width, 10],
  [:spectro_cutoff, 1.0],
  [:spectro_hop, 4],
  [:spectro_start, 0.0],
  [:spectro_x_angle, (provided?("gl") ? 300.0 : 90.0)],
  [:spectro_x_scale, (provided?("gl") ? 1.5 : 1.0)],
  [:spectro_y_angle, (provided?("gl") ? 320.0 : 0.0)],
  [:spectro_y_scale, 1.0],
  [:spectro_z_angle, (provided?("gl") ? 0.0 : 358.0)],
  [:spectro_z_scale, (provided?("gl") ? 1.0 : 0.1)],
  [:temp_dir, ""],
  [:ladspa_dir, ""],
  unless provided?("snd-nogui")
    [:tiny_font, if provided?("snd-motif")
                   "6x12"
                 elsif provided?("snd-gtk")
                   "Monospace 8"
                 end]
  end,
  [:transform_type, 0],
  [:trap_segfault, true], # snd-test.scm says #f but snd-0.h says true
  [:optimization, 0],
  [:run_safety, 0],
  [:clm_table_size, 512],
  [:verbose_cursor, false],
  [:vu_font, ""],
  [:vu_font_size, 1.0],
  [:vu_size, 1.0],
  [:wavelet_type, 0],
  [:time_graph_type, Graph_once],
  [:wavo_hop, 3],
  [:wavo_trace, 64],
  [:x_axis_style, 0],
  [:beats_per_minute, 60.0],
  [:zero_pad, 0],
  [:zoom_focus_style, 2],
  [:mix_waveform_height, 20],
  [:mix_tag_width, 6],
  [:mix_tag_height, 14],
  [:mark_tag_width, 10],
  [:mark_tag_height, 4]]

if $full_test or $snd_test == 0
  $before_test_hook.call(0)
  if sounds or mixes or marks or regions
    snd_display("\n# start up sounds: %s mixes: %s marks: %s regions: %s",
                sounds.inspect, mixes.inspect, marks.inspect, regions.inspect)
  end
  test0(consts, defs)
  $after_test_hook.call(0)
end

# ---------------- test 1: defaults ----------------

$good_colormap = provided?("gl") ? 2 : 0
$better_colormap = 0

# :normal        = [[Symbol, val], ...]
# :without_error = [Symbol, ...]
# :cadr          = [[Symbol, val], ...]
def test01(lst, type = :normal)
  case type
  when :normal
    lst.each do |sym, val|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = send(sym)) != val
          snd_display("\n# %s %s != %s", sym, res.inspect, val.inspect)
        end
      rescue
        snd_display("\n# %s => %s (%s)", sym, res.inspect, $!.inspect)
      end
    end
  when :without_error
    lst.each do |sym|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = without_errors do send(sym) end) != :No_such_sound
          snd_display("\n# %s %s != :No_such_sound", sym, res.inspect)
        end
      rescue
        snd_display("\n# %s %s != :No_such_sound (%s)", sym, res.inspect, $!.inspect)
      end
    end
  when :cadr
    lst.each do |sym, val|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = send(sym)[1]) != val
          snd_display("\n# %s %s != %s", sym, res.inspect, val.inspect)
        end
      rescue
        snd_display("\n# %s => %s (%s)", sym, res.inspect, $!.inspect)
      end
    end
  end
end

def test1(controls, specials, cadr)
  test01(controls, :normal)
  test01(specials, :without_error)
  test01(cadr, :cadr)
  unless provided?("snd-nogui")
    # XEN_EMPTY_LIST is Qnil
    # set_enved_envelope([]) sets enved_envelope to nil
    set_enved_envelope((enved_envelope or []))
    if enved_envelope != nil
      snd_display("\n# enved_envelope set def: %s", enved_envelope.inspect)
    end
  end
end

controls = [
  [:ask_before_overwrite, false],
  [:audio_output_device, 0],
  [:auto_resize, true],
  [:auto_update, false],
  [:channel_style, 0],
  [:color_cutoff, 0.003],
  [:color_inverted, true],
  [:color_scale, 1.0],
  unless provided? "snd-nogui"
    [:colormap, $good_colormap]
  end,
  [:contrast_control_amp, 1.0],
  [:auto_update_interval, 60.0],
  [:cursor_update_interval, 0.05],
  [:cursor_location_offset, 0],
  [:cursor_follows_play, false],
  [:cursor_size, 15], 
  [:cursor_style, Cursor_cross],
  [:dac_combines_channels, true],
  [:dac_size, 256],
  [:minibuffer_history_length, 8],
  [:data_clipped, false],
  [:default_output_chans, 1],
  [:default_output_format, Mus_bshort],
  [:default_output_srate, 22050],
  [:default_output_type, Mus_next],
  [:dot_size, 1],
  [:enved_base, 1.0],
  [:enved_clip?, false],
  [:enved_filter_order, 40],
  unless provided? "snd-nogui"
    [:enved_filter, true]
  end,
  [:enved_in_dB, false],
  [:enved_style, Envelope_linear],
  [:enved_power, 3.0],
  [:enved_target, 0],
  [:enved_wave?, false],
  [:eps_file, "snd.eps"],
  [:eps_bottom_margin, 0.0],
  [:eps_left_margin, 0.0],
  [:eps_size, 1.0],
  [:expand_control_hop, 0.05],
  [:expand_control_jitter, 0.1],
  [:expand_control_length, 0.15],
  [:expand_control_ramp, 0.4],
  [:fft_window_beta, 0.0],
  [:fft_log_frequency, false],
  [:fft_log_magnitude, false],
  [:transform_size, 512],
  [:transform_graph_type, 0],
  [:fft_window, 6],
  [:filter_control_in_dB, false],
  [:filter_control_in_hz, false],
  [:filter_control_order, 20],
  unless provided? "snd-nogui"
    [:graph_cursor, 34]
  end,
  [:graph_style, Graph_lines],
  [:html_dir, "."],
  [:html_program, "mozilla"],
  [:graphs_horizontal, true],
  [:just_sounds, false],
  [:listener_prompt, ">"],
  [:max_transform_peaks, 100],
  [:max_regions, 16],
  [:min_dB, -60.0],
  [:log_freq_start, 32.0],
  [:selection_creates_region, true],
  [:transform_normalization, Normalize_by_channel],
  [:previous_files_sort, 0],
  [:print_length, 12],
  [:recorder_autoload, false],
  [:recorder_buffer_size, 4096],
  [:recorder_file, ""],
  [:recorder_max_duration, 1000000.0],
  [:recorder_out_chans, 2],
  [:recorder_in_chans, 0],
  [:recorder_srate, 22050],
  [:recorder_trigger, 0.0],
  [:region_graph_style, Graph_lines],
  [:reverb_control_feedback, 1.09],
  [:reverb_control_lowpass, 0.7],
  [:save_state_file, "saved-snd.rb"],
  [:show_axes, 1],
  [:show_transform_peaks, false],
  [:show_indices, false],
  [:show_backtrace, false],
  [:show_marks, true],
  [:show_mix_waveforms, true],
  [:show_selection_transform, false],
  [:show_y_zero, false],
  [:show_grid, false],
  [:grid_density, 1.0],
  [:show_sonogram_cursor, false],
  [:show_controls, false],
  [:sinc_width, 10],
  [:spectro_cutoff, 1.0],
  [:spectro_hop, 4],
  [:spectro_start, 0.0],
  [:spectro_x_angle, (provided?("gl") ? 300.0 : 90.0)],
  [:spectro_x_scale, (provided?("gl") ? 1.5 : 1.0)],
  [:spectro_y_angle, (provided?("gl") ? 320.0 : 0.0)],
  [:spectro_y_scale, 1.0],
  [:spectro_z_angle, (provided?("gl") ? 0.0 : 358.0)],
  [:spectro_z_scale, (provided?("gl") ? 1.0 : 0.1)],
  [:temp_dir, ""],
  [:ladspa_dir, ""],
  unless provided?("snd-nogui")
    [:tiny_font, if provided?("snd-motif")
                   "6x12"
                 elsif provided?("snd-gtk")
                   "Monospace 8"
                 end]
  end,
  [:transform_type, 0],
  [:optimization, 0],
  [:run_safety, 0],
  [:clm_table_size, 512],
  [:verbose_cursor, false],
  [:vu_font, ""],
  [:vu_font_size, 1.0],
  [:vu_size, 1.0],
  [:wavelet_type, 0],
  [:time_graph_type, Graph_once],
  [:wavo_hop, 3],
  [:wavo_trace, 64],
  [:x_axis_style, 0],
  [:beats_per_minute, 60.0],
  [:zero_pad, 0],
  [:zoom_focus_style, 2],
  [:mix_waveform_height, 20],
  [:mix_tag_width, 6],
  [:mix_tag_height, 14],
  [:mark_tag_width, 10],
  [:mark_tag_height, 4],
  [:with_mix_tags, (provided?("snd-nogui") ? false : true)],
  [:with_relative_panes, true]]

specials = [
  :amp_control,
  :contrast_control,
  :contrast_control?,
  :expand_control,
  :expand_control?,
  :transform_graph?,
  :filter_control_coeffs,
  :filter_control_envelope,
  :filter_control?,
  :lisp_graph?,
  :read_only,
  :reverb_control_length,
  :reverb_control_scale,
  :reverb_control?,
  :speed_control,
  :sync,
  :time_graph?]

cadr = [
  [:amp_control_bounds, 8.0],
  [:contrast_control_bounds, 10.0],
  [:expand_control_bounds, 20.0],
  [:reverb_control_length_bounds, 5.0],
  [:reverb_control_scale_bounds, 4.0],
  [:speed_control_bounds, 20.0]]

unless provided? "snd-nogui"
  unless colormap? $good_colormap
    $good_colormap = (1..19).detect do |c| colormap?(c) end
  end
  unless colormap? $better_colormap
    $better_colormap = ($good_colormap..19).detect do |c| colormap?(c) end
  end
end

if $full_test or $snd_test == 1
  $before_test_hook.call(1)
  sounds.each do |snd| close_sound(snd) end if sounds
  test1(controls, specials, cadr)
  snd_display("\n# $snd_opened_sound: %d", $snd_opened_sound) if $snd_opened_sound
  $after_test_hook.call(1)
end

# ---------------- test all done

set_previous_files_sort(0)
File.unlink("saved-snd.rb") if File.exists?("saved-snd.rb")
clear_sincs
stop_playing
regions.each do |n| forget_region(n) end if regions
tracks.each do |n| free_track(n) end if tracks
save_listener("test.output")
printf("\n# all done!\n# %s\n", $original_prompt)
$timings[$total_test] = hundred(real_time - $timings[$total_test])
set_print_length(64)
printf("# times: %s\n# total %s\n", $timings.inspect, real_time - $overall_start_time)

mus_audio_playback_amp = 1.0

exit if $with_exit

# snd-test.rb ends here
