# snd_test.rb: Snd Ruby code and tests
#
#  test 00: constants
#  test 01: defaults
#  test 02: headers
#  test 03: variables
#  test 04: sndlib
#  test 05: simple overall checks
#  test 06: vcts
#  test 07: colors
#  test 08: clm
#  test 09: mix
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

require "examp.rb"
require "ws.rb"
require "hooks.rb"
require "extensions.rb"
require "mix.rb"
unless provided? "snd-nogui"
  provided?("snd-motif") and (not provided?("xm")) and require("libxm.so")
  provided?("snd-gtk")   and (not provided?("xg")) and require("libxg.so")
  require "snd-xm.rb"
  include Snd_XM
  require "popup.rb"
end

# global variables marked with "unless defined?" may be set here
load_init_file(".sndtestrc")

$original_save_dir = (save_dir or "/zap/snd")
$original_temp_dir = (temp_dir or "/zap/tmp")
$original_prompt = listener_prompt
$sample_reader_tests = 300
$default_file_buffer_size = 65536
$home_dir = ENV["HOME"]

Total_test = 28

$sf_dir = "/home/bil/sf1/"   unless defined? $sf_dir
$snd_test = -1               unless defined? $snd_test
$with_exit = ($snd_test < 0) unless defined? $with_exit
$full_test = ($snd_test < 0) unless defined? $full_test
$clear_listener = true       unless defined? $clear_listener
# test 4, test084
$test_long_file_tests = true unless defined? $test_long_file_tests
# test 4 with big file, test114
$with_big_file = false       unless defined? $with_big_file
$bigger_snd = "/home/bil/zap/sounds/bigger.snd" unless defined? $bigger_snd

$test00 = true unless defined? $test00
$test01 = true unless defined? $test01
$test02 = true unless defined? $test02
$test03 = true unless defined? $test03
$test04 = true unless defined? $test04
$test05 = true unless defined? $test05
$test06 = true unless defined? $test06
$test07 = true unless defined? $test07
$test08 = true unless defined? $test08
$test09 = true unless defined? $test09
$test10 = true unless defined? $test10
$test11 = true unless defined? $test11
$test12 = true unless defined? $test12
$test13 = true unless defined? $test13
$test14 = true unless defined? $test14
$test15 = true unless defined? $test15
$test16 = true unless defined? $test16
$test17 = true unless defined? $test17
$test18 = true unless defined? $test18
$test19 = true unless defined? $test19
$test20 = true unless defined? $test20
$test21 = true unless defined? $test21
$test22 = true unless defined? $test22
$test23 = true unless defined? $test23
$test24 = true unless defined? $test24
$test25 = true unless defined? $test25
$test26 = true unless defined? $test26
$test27 = true unless defined? $test27
$test28 = true unless defined? $test28

def snd_display(*args)
  str = "\n# " + format(*args)
  snd_print str
  $stderr.write(str) unless provided? "snd-nogui"
  nil
end

def fneq(a, b)
  (a - b).abs > 0.001
end

def ffneq(a, b)
  (a - b).abs > 0.01
end

def fffneq(a, b)
  (a - b).abs > 0.1
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

set_mus_file_buffer_size($default_file_buffer_size)
set_with_background_processes(false)
set_show_backtrace(true)

# getter: mus_audio_playback_amp
# setter: mus_audio_playback_amp(val)
def mus_audio_playback_amp(val = :getter)
  vals = make_vct(32)
  if val == :getter
    mus_audio_mixer_read(Mus_audio_default, Mus_audio_amp, 0, vals)
    ch0_amp = vals[0]
    mus_audio_mixer_read(Mus_audio_default, Mus_audio_amp, 1, vals)
    ch1_amp = vals[0]
    [ch0_amp, ch1_amp]
  else
    vals[0] = val
    mus_audio_mixer_write(Mus_audio_default, Mus_audio_amp, 0, vals)
    mus_audio_mixer_write(Mus_audio_default, Mus_audio_amp, 1, vals)
    val
  end
end

$orig_audio_amp = mus_audio_playback_amp.first
mus_audio_playback_amp(0.0)

def make_color_with_catch(c1, c2, c3)
  make_color(c1, c2, c3)
rescue
  make_color(1, 0, 0)
end

def delete_file(file)
  File.unlink(file) if File.exists?(file)
end

def with_file(file, &body)
  if File.exists?(full_name = $sf_dir + file)
    body.call(full_name)
  else
    snd_display("%s missing?", full_name)
  end
end

def rb_error_to_mus_tag
  if $!
    if /^#<RangeError/ =~ $!.inspect
      :out_of_range
    else
      $!.inspect.split(/:/)[2].split(/,/)[0].split(/[\d>]/).to_s.strip.tr(' ', '_').downcase.intern
    end
  else
    nil
  end
end

show_listener
set_window_x(600)
set_window_y(10)

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

class Snd_test_time
  def initialize(&body)
    @real_time = Time.now
    @process_time = process_times
    @real = @utime = @stime = 0.0
    @return = nil
    run(&body) if block_given?
  end
  attr_reader :real, :utime, :stime, :return
  
  def inspect
    format("#<%s: real: %8.3f, utime: %8.3f, stime: %8.3f>", self.class, @real, @utime, @stime)
  end
  
  def start
    @real_time = Time.now
    @process_time = process_times
  end
  
  def stop
    @real = Time.now - @real_time
    cur_time = process_times
    @utime = cur_time.utime - @process_time.utime
    @stime = cur_time.stime - @process_time.stime
  end
  
  def display
    snd_display(self.inspect)
  end

  def run(&body)
    start
    @return = body.call
    display
  end
end

# returns last result in body
def with_time(&body)
  Snd_test_time.new(&body).return
end

$timings = Array.new(0)

$before_test_hook = Hook.new("$before_test_hook", 1)
$after_test_hook = Hook.new("$after_test_hook", 1)

$before_test_hook.add_hook!("snd-test") do |n|
  $timings.push([n, Snd_test_time.new])
  snd_display("test %d", n)
  set_show_backtrace(false)
end

$after_test_hook.add_hook!("snd-test") do |n|
  $timings.last.last.stop
  if sounds
    snd_display("test %d: open sounds: %s",
                n, sounds.map do |snd| short_file_name(snd) end.inspect)
    sounds.each do |snd| close_sound(snd) end
  end
  snd_display("test %d done\n#", n)
end

snd_display("=== Snd version: %s ===", snd_version)
snd_display("%s\n#", Time.new.strftime("%d-%b %H:%M %Z"))
$overall_start_time = Snd_test_time.new

# snd-test.scm translations
# ---------------- test 00: constants ----------------

# list = [[:Symbol, value], ...]
def test000(lst, exec = false)
  if exec
    # global snd_var functions
    lst.each do |sym, val|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = set_snd_var(sym, snd_var(sym))) != val
          snd_display("set_%s: %s?", sym, res.inspect)
        end
      rescue
        snd_display("set_%s: %s (%s)", sym, res.inspect, $!.inspect)
      end
    end
  else
    # constants
    lst.each do |sym, val|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = eval(sym.to_s)) != val
          snd_display("%s => %s?", sym, res.inspect)
        end
      rescue
        snd_display("%s => %s (%s)", sym, res.inspect, $!.inspect)
      end
    end
  end
end

def test00(consts, defs)
  test000(consts)
  test000(defs, true)
  set_max_transform_peaks(-123)
  set_zero_pad(-123)
  test000([[:max_transform_peaks, 100], [:zero_pad, 0]], true)
  # XEN_EMPTY_LIST is Qnil
  if zero_pad(true, true) != nil
    snd_display("set_zero_pad(true, true): %s?", zero_pad(true, true).inspect)
  end
  if provided?("snd-motif")
    [:axis_label_font,
      :axis_numbers_font,
      :tiny_font,
      :peaks_font,
      :bold_peaks_font].each do |sym|
      val = snd_var(sym)
      if (res = set_snd_var(sym, "8x123")) != val
        snd_display("set_%s to bogus value: %s %s?", sym, val.inspect, res.inspect)
      end
    end
  end
  unless provided?("snd-nogui")
    # XEN_EMPTY_LIST is Qnil
    # set_enved_envelope([]) sets enved_envelope to nil
    set_enved_envelope((enved_envelope or []))
    if enved_envelope != nil
      snd_display("set_enved_envelope: %s?", enved_envelope.inspect)
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

if $test00 and $full_test or $snd_test == 0
  $before_test_hook.call(0)
  if sounds or mixes or marks or regions
    snd_display("start up sounds: %s mixes: %s marks: %s regions: %s",
                sounds.inspect, mixes.inspect, marks.inspect, regions.inspect)
  end
  test00(consts, defs)
  $after_test_hook.call(0)
end

# ---------------- test 01: defaults ----------------

$good_colormap = provided?("gl") ? 2 : 0
$better_colormap = 0

# :normal        = [[Symbol, val], ...]
# :without_error = [Symbol, ...]
# :cadr          = [[Symbol, val], ...]
def test001(lst, type = :normal)
  case type
  when :normal
    lst.each do |sym, val|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = snd_var(sym)) != val
          snd_display("%s %s != %s?", sym, res.inspect, val.inspect)
        end
      rescue
        snd_display("%s => %s (%s)", sym, res.inspect, $!.inspect)
      end
    end
  when :without_error
    lst.each do |sym|
      next unless sym.kind_of?(Symbol)
      if (res = snd_catch do snd_var(sym) end) != :no_such_sound
        snd_display("%s %s != :No_such_sound?", sym, res.inspect)
      end
    end
  when :cadr
    lst.each do |sym, val|
      next unless sym.kind_of?(Symbol)
      begin
        if (res = snd_var(sym)[1]) != val
          snd_display("%s %s != %s?", sym, res.inspect, val.inspect)
        end
      rescue
        snd_display("%s => %s (%s)", sym, res.inspect, $!.inspect)
      end
    end
  end
end

def test01(controls, specials, cadr)
  test001(controls, :normal)
  test001(specials, :without_error)
  test001(cadr, :cadr)
  unless provided?("snd-nogui")
    # XEN_EMPTY_LIST is Qnil
    # set_enved_envelope([]) sets enved_envelope to nil
    set_enved_envelope((enved_envelope or []))
    if enved_envelope != nil
      snd_display("set_enved_envelope: %s?", enved_envelope.inspect)
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

if $test01 and $full_test or $snd_test == 1
  $before_test_hook.call(1)
  sounds.each do |snd| close_sound(snd) end if sounds
  test01(controls, specials, cadr)
  snd_display("$snd_opened_sound: %d", $snd_opened_sound) if $snd_opened_sound
  $after_test_hook.call(1)
end

# ---------------- test 02: headers ----------------

def test_headers(name, chns, sr, dur, typ, frm)
  with_file(name) do |file|
    if mus_sound_chans(file) != chns then snd_display("%s: chans: %d %d?", name, chns, mus_sound_chans(file)) end
    if mus_sound_srate(file) != sr then snd_display("%s: srate: %d %d?", name, sr, mus_sound_srate(file)) end
    if fneq(mus_sound_duration(file), dur) then snd_display("%s: duration: %f %f?", name, dur, mus_sound_duration(file)) end
    if (mus_sound_data_format(file) != -1) and
        (mus_sound_header_type(file) != 33) and
        ((mus_sound_length(file) + 1) < (mus_sound_datum_size(file) * mus_sound_chans(file) * mus_sound_duration(file) * mus_sound_srate(file)))
    then snd_display("%s: length: %d (%d: %d * %d * %f * %d)?", 
                             name,
                             mus_sound_length(file),
                             mus_sound_datum_size(file) * mus_sound_chans(file) * mus_sound_duration(file) * mus_sound_srate(file),
                             mus_sound_datum_size(file), mus_sound_chans(file), mus_sound_duration(file), mus_sound_srate(file))
    end
    if ((mus_sound_frames(file) - (mus_sound_samples(file) / mus_sound_chans(file))) > 1)
    then snd_display("%s: frames: %d %d?",
                             name,
                             mus_sound_frames(file),
                             mus_sound_samples(file) / mus_sound_chans(file))
    end

    if (mus_header_type_name(mus_sound_header_type(file)) != typ)
    then snd_display("%s: type: %s %s?",
                             name,
                             mus_header_type_name(mus_sound_header_type(file)),
                             typ)
    end
    if (mus_data_format_name(mus_sound_data_format(file)) != frm)
    then snd_display("%s: format: [%s] [%s]?",
                             file,
                             mus_data_format_name(mus_sound_data_format(file)),
                             frm)
    end
  end
end

if $test02 and $full_test or $snd_test == 2
  $before_test_hook.call(2)
  test_headers("5_secs.aiff", 1, 44100, 5.303107, "AIFF", "big endian short (16 bits)")
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
  test_headers("mary-sun4.sig", 1, 8000, 4.47612476348877, "Comdisco SPW signal", "big endian double (64 bits)")
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
  test_headers("aifc-float.snd", 1, 22050, 0.226757362484932, "AIFC", "big endian float (32 bits)")
  test_headers("next-mulaw.snd", 1, 8012, 2.03295063972473, "Sun", "mulaw (8 bits)")
  test_headers("next24.snd", 1, 44100, 0.0367800444364548, "Sun", "big endian int (24 bits)")
  test_headers("nist-01.wav", 1, 16000, 2.26912498474121, "NIST", "little endian short (16 bits)")
  test_headers("nist-10.wav", 1, 16000, 2.26912498474121, "NIST", "big endian short (16 bits)")
  test_headers("nist-16.snd", 1, 16000, 1.02400004863739, "NIST", "big endian short (16 bits)")
  test_headers("nist-shortpack.wav", 1, 16000, 4.53824996948242, "NIST", "unknown")
  test_headers("none.aifc", 1, 44100, 0.0367800444364548, "AIFC", "big endian short (16 bits)")
  test_headers("nylon2.wav", 2, 22050, 1.14376413822174, "RIFF", "unknown")
  test_headers("o2.adf", 1, 44100, 0.036780, "CSRE adf", "little endian short (16 bits)")
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
  test_headers("o28.mpc", 1, 44100, 0.036780, "AKAI 4", "little endian short (16 bits)")
  test_headers("oboe.g721", 1, 22050, 1.15287983417511, "Sun", "unknown")
  test_headers("oboe.g723_24", 1, 22050, 0.864761888980865, "Sun", "unknown")
  test_headers("oboe.g723_40", 1, 22050, 1.44126987457275, "Sun", "unknown")
  test_headers("oboe.kts", 1, 22050, 2.305125, "Korg", "big endian short (16 bits)")
  test_headers("oboe.its", 1, 22050, 2.305125, "Impulse Tracker", "little endian short (16 bits)")
  test_headers("oboe.sf2", 1, 22050, 2.30512475967407, "SoundFont", "little endian short (16 bits)")
  test_headers("oboe.paf", 1, 22050, 2.305125, "Ensoniq Paris", "big endian short (16 bits)")
  test_headers("oboe.pf1", 1, 22050, 2.305125, "Ensoniq Paris", "little endian short (16 bits)")
  test_headers("oboe.smp", 1, 22050, 2.305125, "snack SMP", "little endian short (16 bits)")
  test_headers("oboe.nsp", 1, 22050, 2.305125, "CSL", "little endian short (16 bits)")
  test_headers("oboe.nvf", 1, 8000, 6.353500, "Creative NVF", "unknown")
  test_headers("oboe-ulaw.voc", 1, 22050, 2.305669, "VOC", "mulaw (8 bits)")
  test_headers("oboe-lf32.sf", 1, 22050, 2.305669, "IRCAM", "little endian float (32 bits)")
  test_headers("oboe.wfp", 1, 22050, 2.305125, "Turtle Beach", "little endian short (16 bits)")
  test_headers("oki.snd", 2, 44100, 0.0041950112208724, "raw (no header)", "big endian short (16 bits)")
  test_headers("oki.wav", 1, 44100, 0.016780, "RIFF", "unknown")
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
  test_headers("trumps22.adp", 1, 22050, 3.092880, "RIFF", "unknown")
  test_headers("truspech.wav", 1, 8000, 1.1599999666214, "RIFF", "unknown")
  test_headers("ulaw.aifc", 1, 44100, 0.0367800444364548, "AIFC", "mulaw (8 bits)")
  test_headers("voc-8-u.snd", 1, 8000, 1.49937498569489, "VOC", "unsigned byte (8 bits)")
  test_headers("o28.voc", 1, 44100, 0.036893, "VOC", "little endian short (16 bits)")
  test_headers("voxware.wav", 1, 8000, 0.324000000953674, "RIFF", "unknown")
  test_headers("wd.w00", 1, 8000, 0.202749997377396, "Sy-99", "big endian short (16 bits)")
  test_headers("wd1.smp", 1, 8000, 0.202749997377396, "SMP", "little endian short (16 bits)")
  test_headers("wd1.wav", 1, 44100, 0.0367800444364548, "RIFF", "little endian short (16 bits)")
  test_headers("wheel.mat", 2, 44100, 0.145646259188652, "raw (no header)", "big endian short (16 bits)")
  test_headers("b8.pvf", 1, 44100, 0.036803, "Portable Voice Format", "signed byte (8 bits)")
  test_headers("b16.pvf", 1, 44100, 0.036803, "Portable Voice Format", "big endian short (16 bits)")
  test_headers("b32.pvf", 1, 44100, 0.036803, "Portable Voice Format", "big endian int (32 bits)")
  test_headers("water.voc", 2, 32000, 42.3463897705078, "VOC", "little endian short (16 bits)")
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
  test_headers("oboe.sf6", 1, 22050, 2.305125, "SoundForge", "little endian short (16 bits)")
  test_headers("addf8.24we", 1, 8000, 2.976000, "RIFF", "little endian int (24 bits)")
  test_headers("hybrid.snd", 1, 44100, 4.600000, "BICSF", "big endian float (32 bits)")
  test_headers("litmanna.sf", 1, 44100, 0.533, "IRCAM", "little endian short (16 bits)")
  test_headers("M1F1-float64C-AFsp.aif", 2, 8000, 2.9366, "AIFC", "big endian double (64 bits)")
  test_headers("MacBoing.wav", 1, 11127, 0.696, "RIFF", "unsigned byte (8 bits)")
  test_headers("t15.aiff", 2, 44100, 135.00, "AIFC", "little endian short (16 bits)")
  test_headers("tomf8.aud", 1, 8000, 2.016000, "INRS", "little endian short (16 bits)")
  test_headers("Xhs001x.nsp", 1, 10000, 6.017400, "CSL", "little endian short (16 bits)")
  test_headers("zulu_a4.w11", 1, 33000, 1.21987879276276, "TX-16W", "unknown")
  [["/home/bil/./sf1/o2.voc",      "/home/bil/sf1/o2.voc"],
    ["~/./sf1/o2.voc",             $home_dir + "/sf1/o2.voc"],
    ["~/cl/../sf1/o2.voc",         $home_dir + "/sf1/o2.voc"],
    ["/home/bil/cl/../sf1/o2.voc", "/home/bil/sf1/o2.voc"]].each do |in_name, real_name|
    if mus_expand_filename(in_name) != real_name
      snd_display("mus_expand_filename %s => %s?", in_name, mus_expand_filename(in_name))
    end
    if File.exists?("/home/bil/./sf1/o2.voc")
      if sound?(ind = open_sound(in_name))
        if file_name(ind) != real_name
          snd_display("expand file name %s: %s?", in_name, file_name(ind))
        end
        close_sound(ind)
      else
      end
    end
  end
  $after_test_hook.call(2)
end

# ---------------- test 03: variables ----------------

# :normal
# :bad_args
def test003(lst, type = :normal)
  case type
  when :normal
    lst.each do |sym, initval, newval|
      next unless sym.kind_of?(Symbol)
      set_snd_var(sym, newval)
      if initval.kind_of?(Float)
        if fneq((nowval = snd_var(sym)), newval)
          snd_display("set_%s %s != %s?", sym, nowval.inspect, newval.inspect)
        end
      else
        if (nowval = snd_var(sym)) != newval
          snd_display("set_%s %s != %s?", sym, nowval.inspect, newval.inspect)
        end
      end
      set_snd_var(sym, initval)
    end
  when :bad_args
    lst.each do |sym, initval, newval|
      next unless sym.kind_of?(Symbol)
      begin
        set_snd_var(sym, newval)
      rescue
        set_snd_var(sym, initval)
      end
      if (nowval = snd_var(sym)) == newval
        snd_display("set_%s (bad set) %s == %s?", sym, nowval.inspect, newval.inspect)
      end
      set_snd_var(sym, initval)
    end
  end
end

def test03(vars1, vars2)
  ind = open_sound("oboe.snd")
  td = temp_dir
  snd_catch do
    if set_temp_dir($home_dir + "/test") != ($home_dir + "/test")
      snd_display("set_temp_dir: %s?", temp_dir)
    end
    set_temp_dir((td or ""))
  end
  if fneq(sample(1000), 0.0328)
    snd_display("sample: %s?", sample(1000).inspect)
  end
  set_show_controls true
  unless provided? "snd-nogui"
    Snd_hooks.each_with_index do |h, i|
      unless h.kind_of?(Hook) and hook?(h)
        snd_display("Snd_hooks[%d] %s?", i, h.inspect)
      end
    end
    wid = enved_dialog
    if dialog_widgets[2] != wid
      snd_display("enved_dialog -> %s %s?", wid.inspect, dialog_widgets[2].inspect)
    end
    snd_display("enved_dialog?") unless dialog_widgets[2]
    set_enved_envelope([0.0, 0.0, 1.0, 1.0, 2.0, 0.0])
    if enved_envelope != [0.0, 0.0, 1.0, 1.0, 2.0, 0.0]
      snd_display("set_enved_envelope: %s?", enved_envelope.inspect)
    end
    set_enved_envelope(enved_envelope)
    if enved_envelope != [0.0, 0.0, 1.0, 1.0, 2.0, 0.0]
      snd_display("set_enved_envelope to self: %s?", enved_envelope.inspect)
    end
    wid = orientation_dialog
    snd_display("orientation_dialog?") unless dialog_widgets[1]
    if dialog_widgets[1] != wid
      snd_display("orientation_dialog -> %s %s?", wid.inspect, dialog_widgets[1].inspect)
    end
    test003(vars1, :normal)
    test003(vars2, :bad_args)
    set_enved_filter_order(5)
    if enved_filter_order != 6
      snd_display("set_enved_filter_order 5: %s?", enved_filter_order.inspect)
    end
    zero_to_one = [0, 0.0, 50, 0.5, 100, 1.0]
    mod_down = [0, 1.0, 50, 0.5, 100, 0.0]
    set_enved_envelope(:zero_to_one)
    if enved_envelope != zero_to_one
      snd_display("set_enved_envelope (Symbol): %s %s?",
                  enved_envelope.inspect, zero_to_one.inspect)
    end
    set_enved_envelope("mod_down")
    if enved_envelope != mod_down
      snd_display("set_enved_envelope (String): %s %s?",
                  enved_envelope.inspect, mod_down.inspect)
    end
  end
  if search_procedure.kind_of?(Proc)
    snd_display("global search procedure: %s?", search_procedure.inspect)
  end
  set_search_procedure(lambda do |y| y > 0.1 end)
  unless search_procedure.kind_of?(Proc)
    snd_display("set_search_procedure: %s?", search_procedure.inspect)
  end
  snd_display("search 0.1 > 0.2?") unless search_procedure.call(0.2)
  snd_display("search 0.1 > 0.02?") if search_procedure.call(0.02)
  set_search_procedure(lambda do |y| y < 0.0 end)
  snd_display("search 0.0 < 0.02?") if search_procedure.call(0.02)
  set_search_procedure(false)
  if search_procedure.kind_of?(Proc)
    snd_display("set_search_procedure after reset: %s?", search_procedure.inspect)
  end
  set_search_procedure(lambda do |y| y > 0.1 end)
  unless search_procedure.kind_of?(Proc)
    snd_display("set_search_procedure: %s?", search_procedure.inspect)
  end
  close_sound(ind)
  dismiss_all_dialogs unless provided? "snd-nogui"
end

vars = [
  [:amp_control, 1.0, 0.5],
  [:amp_control_bounds, [0.0, 8.0], [1.0, 5.0]],
  [:ask_before_overwrite, false, true],
  [:audio_input_device, 0, 1],
  [:audio_output_device, 0, 1],
  [:auto_resize, true, false],
  [:auto_update, false, true],
  [:channel_style, 0, 1],
  unless provided? "snd-nogui"
    [:colormap, $good_colormap, $better_colormap]
  end,
  [:color_cutoff, 0.003, 0.01],
  [:color_inverted, true, false],
  [:color_scale, 1.0, 0.5],
  [:contrast_control, 0.0, 0.5],
  [:contrast_control_bounds, [0.0, 10.0], [1.0, 5.0]],
  [:contrast_control_amp, 1.0, 0.5],
  [:contrast_control?, false, true],
  [:auto_update_interval, 60.0, 120.0],
  [:cursor_update_interval, 0.05, 0.1],
  [:cursor_location_offset, 0, 32768],
  [:cursor_follows_play, false, true],
  [:cursor_size, 15, 30],
  [:cursor_style, Cursor_cross, Cursor_line],
  [:dac_combines_channels, true, false],
  [:dac_size, 256, 512],
  [:minibuffer_history_length, 8, 16],
  [:data_clipped, false, true],
  [:default_output_chans, 1, 2],
  [:default_output_format, 1, 1],
  [:default_output_srate, 22050, 44100],
  [:default_output_type, Mus_next, Mus_aifc],
  [:dot_size, 1, 4],
  [:enved_base, 1.0, 1.5],
  [:enved_clip?, false, true],
  [:enved_in_dB, false, true],
  [:enved_style, Envelope_linear, Envelope_exponential],
  [:enved_power, 3.0, 3.5],
  [:enved_target, 0, 1],
  [:enved_wave?, false, true],
  [:eps_file, "snd.eps", "snd-1.eps"],
  [:eps_left_margin, 0.0, 72.0],
  [:eps_size, 1.0, 2.0],
  [:eps_bottom_margin, 0.0, 36.0],
  [:expand_control, 1.0, 2.0],
  [:expand_control_bounds, [0.001, 20.0], [1.0, 2.0]],
  [:expand_control_hop, 0.05, 0.1],
  [:expand_control_jitter, 0.1, 0.2],
  [:expand_control_length, 0.15, 0.2],
  [:expand_control_ramp, 0.4, 0.2],
  [:expand_control?, false, true],
  [:fft_window_beta, 0.0, 0.5],
  [:fft_log_frequency, false, true],
  [:fft_log_magnitude, false, true],
  [:transform_size, 512, 1024],
  [:transform_graph_type, 0, 1],
  [:fft_window, 6, 5],
  [:transform_graph?, false, true],
  [:filter_control_in_dB, false, true],
  [:filter_control_envelope, [0.0, 1.0, 1.0, 1.0], [0.0, 1.0, 1.0, 0.0]],
  unless provided? "snd-nogui"
    [:enved_filter, true, false]
  end,
  [:enved_filter_order, 40, 20],
  [:filter_control_in_hz, false, true],
  [:filter_control_order, 20, 40],
  [:filter_control?, false, true],
  unless provided? "snd-nogui"
    [:graph_cursor, 34, 33]
  end,
  [:graph_style, 0, 1],
  [:just_sounds, false, true],
  [:listener_prompt, ">", ":"],
  [:max_transform_peaks, 100, 10],
  [:max_regions, 16, 6],
  [:min_dB, -60.0, -90.0],
  [:log_freq_start, 32.0, 10.0],
  [:mix_waveform_height, 20, 40],
  [:mix_tag_height, 14, 20],
  [:mix_tag_width, 6, 20],
  [:mark_tag_height, 4, 20],
  [:mark_tag_width, 10, 20],
  [:selection_creates_region, true, false],
  [:transform_normalization, Normalize_by_channel, Dont_normalize],
  [:previous_files_sort, 0, 1],
  [:print_length, 12, 16],
  [:recorder_autoload, false, true],
  [:recorder_out_chans, 2, 1],
  [:recorder_in_chans, 0, 1],
  [:recorder_buffer_size, 4096, 256],
  [:recorder_max_duration, 1000000.0, 1000.0],
  [:recorder_trigger, 0.0, 0.1],
  [:region_graph_style, Graph_lines, Graph_lollipops],
  [:reverb_control_decay, 1.0, 2.0],
  [:reverb_control_feedback, 1.09, 1.6],
  [:reverb_control_length, 1.0, 2.0],
  [:reverb_control_length_bounds, [0.0, 0.5], [1.0, 2.0]],
  [:reverb_control_lowpass, 0.7, 0.9],
  [:reverb_control_scale, 0.0, 0.2],
  [:reverb_control_scale_bounds, [0.0, 4.0], [0.0, 0.2]],
  [:reverb_control?, false, true],
  [:show_axes, 1, 0],
  [:show_transform_peaks, false, true],
  [:show_indices, false, true],
  [:show_backtrace, false, true],
  [:show_marks, true, false],
  [:show_mix_waveforms, true, false],
  [:show_selection_transform, false, true],
  [:show_y_zero, false, true],
  [:show_grid, false, true],
  [:grid_density, 1.0, 0.5],
  [:show_sonogram_cursor, false, true],
  [:sinc_width, 10, 40],
  [:spectro_cutoff, 1.0, 0.7],
  [:spectro_hop, 4, 10],
  [:spectro_start, 0.0, 0.1],
  [:spectro_x_angle, (provided?("gl") ? 300.0 : 90.0), 60.0],
  [:spectro_x_scale, (provided?("gl") ? 1.5 : 1.0), 2.0],
  [:spectro_y_angle, (provided?("gl") ? 320.0 : 0.0), 60.0],
  [:spectro_y_scale, 1.0, 2.0],
  [:spectro_z_angle, (provided?("gl") ? 0.0 : 358.0), 60.0],
  [:spectro_z_scale, (provided?("gl") ? 1.0 : 0.1), 0.2],
  [:speed_control, 1.0, 0.5],
  [:speed_control_bounds, [0.05, 20.0], [1.0, 5.0]],
  [:speed_control_style, 0, 1],
  [:speed_control_tones, 12, 18],
  [:sync, 0, 1],
  if provided? "snd-motif"
    [:tiny_font, "6x12", "9x15"]
  end,
  [:transform_type, 0, 1],
  [:verbose_cursor, false, true],
  [:vu_size, 1.0, 2.0],
  [:vu_font_size, 1.0, 2.0],
  [:wavelet_type, 0, 1],
  [:time_graph?, false, true],
  [:time_graph_type, Graph_once, Graph_as_wavogram],
  [:wavo_hop, 3, 6],
  [:wavo_trace, 64, 128],
  [:with_mix_tags, (provided?("snd-nogui") ? false : true), false],
  [:with_relative_panes, true, false],
  [:with_gl, provided?("gl"), false],
  [:x_axis_style, 0, 1],
  [:beats_per_minute, 30.0, 120.0],
  [:zero_pad, 0, 1],
  [:zoom_focus_style, 2, 1],
  [:window_width, window_width, 300],
  [:window_height, window_height, 300],
  [:window_x, 10, 123],
  [:window_y, 10, 321],
  [:color_scale, color_scale, 100.0]]

bad_args = [
  [:amp_control, 1.0, [-1.0, 123.123]],
  [:amp_control_bounds, [0.0, 8.0], [false, [0.0], [1.0, 0.0], 2.0]],
  [:channel_style, 0, [32, -1, 1.0]],
  unless provided? "snd-nogui"
    [:colormap, $good_colormap, [321, -123]]
  end,
  [:color_cutoff, 0.003, [-1.0, 123.123]],
  [:color_scale, 1.0, [-32.0, 2000.0]],
  [:contrast_control, 0.0, [-123.123, 123.123]],
  [:contrast_control_bounds, [0.0, 10.0], [false, [0.0], [1.0, 0.0], 2.0]],
  [:cursor_size, 15, [1.123, -2.5]],
  [:dac_size, 256, [-1, 0, -123]],
  [:dot_size, 1, [0, -1, -123]],
  [:enved_target, 0, [123, -321]],
  [:expand_control, 1.0, [-1.0, 0.0]],
  [:expand_control_bounds, [0.001, 20.0], [false, [0.0], [1.0, 0.0], 2.0]],
  [:expand_control_hop, 0.05, [-1.0]],
  [:expand_control_length, 0.15, [-1.0, 0.0]],
  [:expand_control_ramp, 0.4, [-1.0, 1.0, 123.123]],
  [:fft_window_beta, 0.0, [-1.0, 123.123]],
  [:transform_size, 512, [-1, 0]],
  [:zero_pad, 0, [-1, -123]],
  [:cursor_style, Cursor_cross, [-1]],
  [:cursor_style, Cursor_line, [2, 123]],
  [:transform_graph_type, 0, [-1, 123]],
  [:fft_window, 6, [-1, 123]],
  [:enved_filter_order, 40, [-1, 0]],
  [:filter_control_order, 20, [-10, -1, 0]],
  [:max_transform_peaks, 100, [-1]],
  [:max_regions, 16, [-1, -123]],
  [:previous_files_sort, 0, [-1, 123]],
  [:reverb_control_length, 1.0, [-1.0]],
  [:show_axes, 1, [-1, 123]],
  [:sinc_width, 10, [-10]],
  [:spectro_cutoff, 1.0, [-1.0]],
  [:spectro_hop, 4, [-10, -1, 0]],
  [:spectro_start, 0.0, [-1.0]],
  [:speed_control, 1.0, [0.0]],
  [:speed_control_bounds, [0.05, 20.0], [false, [0.0], [1.0, 0.0], 2.0]],
  [:speed_control_style, 0, [-1, 10]],
  [:transform_type, 0, [-1, 123]],
  [:wavelet_type, 0, [-1, 123]],
  [:wavo_hop, 3, [0, -123]],
  [:wavo_trace, 64, [0, -123]],
  [:x_axis_style, 0, [-1, 123]],
  [:zoom_focus_style, 2, [-1, 123]]]

if $test03 and $full_test or $snd_test == 3
  $before_test_hook.call(3)
  test03(vars, bad_args)
  $after_test_hook.call(3)
end

# ---------------- test 04: sndlib ----------------

def play_sound_1(file)
  sound_fd = mus_sound_open_input(file)
  chans = mus_sound_chans(file)
  frames = mus_sound_frames(file)
  srate = mus_sound_srate(file)
  bufsize = 256
  data = make_sound_data(chans, bufsize)
  bytes = bufsize * chans * 2
  audio_fd = mus_audio_open_output(Mus_audio_default, srate, chans, Mus_lshort, bytes)
  if audio_fd == -1
    audio_fd = mus_audio_open_output(Mus_audio_default, srate, chans, Mus_bshort, bytes)
  end
  if audio_fd == -1
    snd_display("can't play %s", file)
  else
    0.step(frames, bufsize) do
      mus_sound_read(sound_fd, 0, bufsize - 1, chans, data)
      mus_audio_write(audio_fd, data, bufsize)
    end
    mus_audio_close(audio_fd)
  end
rescue
  snd_display("can't open audio")
ensure
  mus_sound_close_input(sound_fd)
end

def frame2byte(file, frame)
  mus_sound_data_location(file) + mus_sound_chans(file) * mus_sound_datum_size(file) * frame
end

def show_input_1(in_sys = 0)
  our_short = (little_endian? ? Mus_lshort : Mus_bshort)
  our_srate = 22050
  our_dac_buffer_size_in_bytes = 512
  our_dac_buffer_size_in_shorts = 256
  our_chans = 1
  our_chan = 0
  in_port = begin
              mus_audio_open_input(in_sys << 16 | Mus_audio_default,
                                   our_srate, our_chans, our_short,
                                   our_dac_buffer_size_in_bytes)
            rescue
              -1
            end
  if in_port == -1
    snd_display("can't open audio input port")
  else
    data = make_sound_data(our_chans, our_dac_buffer_size_in_shorts)
    vobj = make_vct(our_dac_buffer_size_in_shorts)
    10.times do |i|
      mus_audio_read(in_port, data, our_dac_buffer_size_in_shorts)
      graph(sound_data2vct(data, our_chan, vobj))
    end
    mus_audio_close(in_port)
  end
end

def test004(formats)
  oboe_snd = "oboe.snd"
  chns = mus_sound_chans(oboe_snd)
  dl = mus_sound_data_location(oboe_snd)
  fr = mus_sound_frames(oboe_snd)
  smps = mus_sound_samples(oboe_snd)
  len = mus_sound_length(oboe_snd)
  size = mus_sound_datum_size(oboe_snd)
  com = mus_sound_comment(oboe_snd)
  sr = mus_sound_srate(oboe_snd)
  m1 = mus_sound_maxamp_exists?(oboe_snd)
  mal = mus_sound_maxamp(oboe_snd)
  mz = mus_sound_maxamp "z.snd"
  bytes = mus_bytes_per_sample(mus_sound_data_format(oboe_snd))
  sys = mus_audio_systems
  if mz[0].nonzero? or mz[1].nonzero?
    snd_display("mus_sound_maxamp z.snd: %s?", mz.inspect)
  end
  formats.each do |frm, siz|
    if mus_bytes_per_sample(frm) != siz
      snd_display("mus_bytes_per_sample %d != %d?", mus_bytes_per_sample(frm), siz)
    end
  end
  hiho = "hiho.tmp"
  mus_sound_report_cache hiho
  fp = File.open(hiho)
  if (res = fp.readline).chomp! != "sound table:"
    snd_display("print-cache 1: %s?", res)
  end
  fp.close
  delete_file(hiho)
  snd_display("mus_audio_report: %s?", mus_audio_report) if mus_audio_report.length < 10
  snd_display("mus_audio_systems: %d?", sys) if sys != 1 and sys != 2
  snd_display("oboe: mus_sound_chans %d?", chns)         if chns != 1
  snd_display("oboe: mus_sound_data_location %d?", dl)   if dl != 28
  snd_display("oboe: mus_sound_frames %d?", fr)          if fr != 50828
  snd_display("oboe: mus_sound_samples %d?", smps)       if smps != 50828
  snd_display("oboe: mus_sound_length %d?", len)         if len != (50828 * 2 + 28)
  snd_display("oboe: mus_sound_datum_size %d?", size)    if size != 2
  snd_display("oboe: mus_sound_bytes %d?", bytes)        if bytes != 2
  snd_display("oboe: mus_sound_srate %d?", sr)           if sr != 22050
  snd_display("oboe: mus_sound_comment %s?", com)        unless com
  if m1
    snd_display("oboe: mus_sound_maxamp_exists? before maxamp: %s?", m1.inspect)
  end
  unless mus_sound_maxamp_exists?(oboe_snd)
    snd_display("oboe: mus_sound_maxamp_exists? after maxamp: %s?",
                mus_sound_maxamp_exists?(oboe_snd).inspect)
  end
  snd_display("oboe: mus_sound_maxamp %f?", mal[1])     if mal[1] != 0.14724
  snd_display("oboe: mus_sound_maxamp at %d?", mal[0])  if mal[0] != 24971
  set_mus_sound_maxamp(oboe_snd, [1234, 0.5])
  mal = mus_sound_maxamp(oboe_snd)
  snd_display("oboe: set_mus_sound_maxamp %f?", mal[1]) if mal[1] != 0.5
  snd_display("oboe: set_mus_sound_maxamp at %d?", mal[0]) if mal[0] != 1234
  mus_audio_set_oss_buffers(4, 12)
  res = Time.at(mus_sound_write_date(oboe_snd)).localtime.strftime("%d-%b-%Y %H:%M")
  if res != "01-Nov-2004 06:10"
    snd_display("mus_sound_write_date oboe.snd: %s?", res)
  end
  res = Time.at(mus_sound_write_date("pistol.snd")).localtime.strftime("%d-%b-%Y %H:%M")
  if res != "01-Jul-2004 13:06"
    snd_display("mus_sound_write_date pistol.snd: %s?", res)
  end
  if mus_audio_sun_outputs(1, 2, 3)
    snd_display("mus_audio_sun_outputs: %s?", mus_audio_sun_outputs(1, 2, 3).inspect)
  end
end

def test014
  index = open_sound("oboe.snd")
  lfname = "test"
  32.times do lfname << "-test" end
  lfname += ".snd"
  snd_display("variable_graph thinks anyting is a graph...") if variable_graph?(index)
  snd_display("player? thinks anything is a player...") if player?(index)
  snd_display("%s is not a sound?", index.inspect) unless sound?(index)
  save_sound_as(lfname, index)
  close_sound(index)
  index = open_sound(lfname)
  snd_display("can't find test...snd") unless sound?(index)
  if (not file_name(index).length >= lfname.length) or
      (not short_file_name(index).length >= lfname.length)
    snd_display("file_name lengths: %d %d %d?",
                file_name(index).length, short_file_name(index).length, lfname.length)
  end
  close_sound(index)
  mus_sound_forget(lfname)
  delete_file(lfname)
end

def test024
  with_file("forest.aiff") do |fsnd|
    File.copy(fsnd, "fmv.snd")
    index = open_sound("fmv.snd")
    if sound_loop_info(index) != mus_sound_loop_info(fsnd)
      snd_display("loop_info: %s %s?", sound_loop_info(index), mus_sound_loop_info(fsnd))
    end
    save_sound_as("fmv1.snd", index, Mus_aifc)
    close_sound(index)
    if mus_sound_loop_info("fmv1.snd") != [12000, 14000, 1, 2, 3, 4, 1, 1]
      snd_display("saved loop_info: %s?", mus_sound_loop_info("fmv1.snd"))
    end
  end
  index = open_sound("oboe.snd")
  save_sound_as("fmv.snd", index, Mus_aifc)
  close_sound(index)
  index = open_sound("fmv.snd")
  if sound_loop_info(index) != nil
    snd_display("null loop_info: %s?", sound_loop_info(index).inspect)
  end
  set_sound_loop_info(index, [1200, 1400, 4, 3, 2, 1])
  if sound_loop_info(index) != [1200, 1400, 4, 3, 2, 1, 1, 1]
    snd_display("set null loop_info: %s?", sound_loop_info(index).inspect)
  end
  save_sound_as("fmv1.snd", :sound, index, "header-type".intern, Mus_aifc)
  close_sound(index)
  if mus_sound_loop_info("fmv1.snd") != [1200, 1400, 4, 3, 2, 1, 1, 1]
    snd_display("saved null loop_info: %s?", mus_sound_loop_info("fmv1.snd").inspect)
  end
  index = open_sound("fmv.snd")
  set_sound_loop_info(index, [1200, 1400, 4, 3, 2, 1, 1, 0])
  if sound_loop_info(index) != [1200, 1400, 0, 0, 2, 1, 1, 0]
    snd_display("null set_sound_loop_info (no mode1): %s?", sound_loop_info(index).inspect)
  end
  save_sound_as("fmv1.snd", index, Mus_aifc)
  close_sound(index)
  if mus_sound_loop_info("fmv1.snd") != [1200, 1400, 0, 0, 2, 1, 1, 0]
    snd_display("saved null loop_info (no mode1): %s?", mus_sound_loop_info("fmv1.snd").inspect)
  end
  delete_file("fmv.snd")
  delete_file("fmv1.snd")
end

def test034(func, lst)
  lst.each do |f, val|
    with_file(f) do |fsnd|
      if (res = send(func, fsnd)) != val
        snd_display("%s %s => %s != %s?", func, fsnd.inspect, res.inspect, val.inspect)
      end
    end
  end
end

def test044
  oboe_snd = "oboe.snd"
  if (res = snd_catch do set_mus_sound_maxamp(oboe_snd, [1234]) end) != :wrong_type_arg
    snd_display("set_mus_sound_maxamp bad arg: %s?", res.inspect)
  end
  if (not mus_sound_type_specifier(oboe_snd) == 0x646e732e) and # little endian reader
      (not mus_sound_type_specifier(oboe_snd) == 0x2e736e64)    # big endian reader
    snd_display("oboe: mus_sound_type_specifier: %x?", mus_sound_type_specifier(oboe_snd))
  end
  res = Time.at(file_write_date(oboe_snd)).localtime.strftime("%d-%b-%Y %H:%M")
  if res != "01-Nov-2004 06:10"
    snd_display("file_write_date oboe.snd: %s?", res)
  end
  play_sound_1(oboe_snd)
  let(1) do |lasth|
    until mus_header_type_name(lasth) == "unsupported" do lasth += 1 end
    if lasth < 50
      snd_display("header_type[%d] == %s?", lasth, mus_header_type_name(lasth).inspect)
    end
  end
  let(1) do |lasth|
    until mus_data_format_name(lasth) == "unknown" do lasth += 1 end
    if lasth < 10
      snd_display("data_format[%d] == %s?", lasth, mus_data_format_name(lasth).inspect)
    end
  end
  unless provided? "snd-nogui"
    [:Dont_normalize, :Normalize_globally, :Normalize_by_channel].each do |val_sym|
      val = eval("#{val_sym}")
      set_transform_normalization(val)
      if (res = transform_normalization) != val
        snd_display("set_transform_normalization(%s) => %d?", val_sym, res)
      end
    end
  end
end

def test054
  len = 100
  [[Mus_bshort, 2 ** -15],
    [Mus_lshort, 2 ** -15],
    [Mus_mulaw, 0.02],
    [Mus_alaw, 0.02],
    [Mus_byte, 2 ** -7],
    [Mus_lfloat, 2 ** -23],
    [Mus_bint, 2 ** -23],
    [Mus_lint, 2 ** -23],
    [Mus_b24int, 2 ** -23],
    [Mus_l24int, 2 ** -23],
    [Mus_ubshort, 2 ** -15],
    [Mus_ulshort, 2 ** -15],
    [Mus_ubyte, 2 ** -7],
    [Mus_bfloat, 2 ** -23],
    [Mus_bdouble, 2 ** -23],
    [Mus_ldouble, 2 ** -23]].each do |type, allowed_diff|
    ind = new_sound("test.snd", Mus_next, Mus_lfloat, 22050, 1)
    v = make_vct(len)
    maxdiff = 0.0
    maxpos = false
    vct_set!(v, 0, 0.999)
    vct_set!(v, 1, -1.0)
    vct_set!(v, 2, 0.1)
    vct_set!(v, 3, -0.1)
    vct_set!(v, 4, 0.01)
    vct_set!(v, 5, -0.01)
    vct_set!(v, 6, 0.001)
    vct_set!(v, 7, -0.001)
    vct_set!(v, 8, 0.0)
    9.upto(len - 1) do |i|
      vct_set!(v, i, 1.0 - my_random(2.0))
    end
    vct2channel(v, 0, len, ind, 0)
    save_sound_as("test1.snd", ind, Mus_next, "data-format".intern, type)
    close_sound(ind)
    ind = open_sound("test1.snd")
    v1 = channel2vct(0, len, ind, 0)
    len.times do |i|
      if (diff = (vct_ref(v, i) - vct_ref(v1, i)).abs) > maxdiff
        maxdiff = diff
        maxpos = i
      end
    end
    if maxdiff > allowed_diff
      snd_display("%s: %f %d %f %f?",
                  mus_data_format_name(type),
                  maxdiff, maxpos,
                  vct_ref(v, maxpos), vct_ref(v1, maxpos))
    end
    close_sound(ind)
    delete_file("test1.snd")
  end
end

def test064(fields, devices)
  oboe_snd = "oboe.snd"
  ob = view_sound(oboe_snd)
  samp = sample(1000, ob)
  old_comment = mus_sound_comment(oboe_snd)
  str = format("written %s", Time.new.localtime.strftime("%a %d-%b-%Y %H:%M %Z"))
  set_comment(ob, str)
  save_sound_as("test.snd", ob, Mus_aifc, Mus_bdouble)
  set_filter_control_in_hz(true)
  ab = open_sound("test.snd")
  if (res = mus_sound_comment("test.snd")) != str
    snd_display("output_comment: %s != %s?", res, str)
  end
  if (res = comment(ab)) != str
    snd_display("comment: %s != %s?", res, str)
  end
  close_sound(ab)
  if (res = mus_sound_comment(oboe_snd)) != old_comment
    snd_display("set_comment overwrote current: %s != %s?", res, old_comment)
  end
  set_filter_control_in_hz(false)
  save_sound_as("test.snd", ob, Mus_raw)
  ab = open_raw_sound("test.snd", 1, 22050, Mus_bshort)
  if header_type(ab) != Mus_raw
    snd_display("save_as Mus_raw -> %s?", mus_header_type_name(header_type(ab)))
  end
  if mus_sound_header_type("test.snd") != Mus_raw
    snd_display("saved_as Mus_raw -> %s?",
                mus_header_type_name(mus_sound_header_type("test.snd")))
  end
  if (res = sample(1000, ab)) != samp
    snd_display("Mus_raw[1000] = %f?", res)
  end
  close_sound(ab)
  $output_comment_hook.reset_hook!
  $output_comment_hook.add_hook!("snd-test-4") do |string|
    string + " [written by me]"
  end
  save_sound_as(:file, "test.snd",
                :sound, ob,
                "header-type".intern, Mus_riff,
                "data-format".intern, Mus_lfloat)
  $output_comment_hook.reset_hook!
  ab = open_sound("test.snd")
  if (res = comment(ab)) != (str + " [written by me]")
    snd_display("output_comment_hook: %s\n(%s)?", res, mus_sound_comment("test.snd"))
  end
  close_sound(ab)
  save_sound_as("test.snd", ob, Mus_next, Mus_bshort)
  ab = open_sound("test.snd")
  set_y_bounds([-3.0, 3.0], ab, 0)
  set_data_format(ab, Mus_lshort)
  if find_sound("test.snd") != ab
    ab = find_sound("test.snd")
  end
  if (res = data_format(ab)) != Mus_lshort
    snd_display("set_data_format: %s?", mus_data_format_name(res))
  end
  if (res = y_bounds(ab, 0)) != [-3.0, 3.0]
    snd_display("set data format y_bounds: %s?", res)
  end
  set_y_bounds([2.0], ab, 0)
  if (res = y_bounds(ab, 0)) != [-2.0, 2.0]
    snd_display("set data format y_bounds 1: %s?", res)
  end
  set_y_bounds([-2.0], ab, 0)
  if (res = y_bounds(ab, 0)) != [-2.0, 2.0]
    snd_display("set data format y_bounds -2: %s?", res)
  end
  set_header_type(ab, Mus_aifc)
  if find_sound("test.snd") != ab
    ab = find_sound("test.snd")
  end
  if (res = header_type(ab)) != Mus_aifc
    snd_display("set_header_type: %s?", mus_header_type_name(res))
  end
  set_channels(ab, 3)
  if find_sound("test.snd") != ab
    ab = find_sound("test.snd")
  end
  if (res = channels(ab)) != 3
    snd_display("set_channles: %s?", res)
  end
  set_data_location(ab, 1234)
  if find_sound("test.snd") != ab
    ab = find_sound("test.snd")
  end
  if (res = data_location(ab)) != 1234
    snd_display("set_data_location: %s?", res)
  end
  old_size = data_size(ab)
  set_data_size(ab, 1234)
  if find_sound("test.snd") != ab
    ab = find_sound("test.snd")
  end
  if (res = data_size(ab)) != 1234
    snd_display("set_data_size: %s?", res)
  end
  set_data_size(ab, old_size)
  set_srate(ab, 12345)
  if find_sound("test.snd") != ab
    ab = find_sound("test.snd")
  end
  if (res = srate(ab)) != 12345
    snd_display("set_srate: %s?", res)
  end
  close_sound(ab)
  [
    [:Mus_aifc, :Mus_bdouble],
    [:Mus_riff, :Mus_lfloat],
    [:Mus_nist, :Mus_bint],
    [:Mus_aiff, :Mus_b24int],
    [:Mus_ircam, :Mus_mulaw],
    [:Mus_next, :Mus_alaw],
    [:Mus_next, :Mus_bdouble],
    [:Mus_next, :Mus_bshort],
    [:Mus_next, :Mus_bfloat],
    [:Mus_next, :Mus_bshort]
  ].each do |sym_hdr, sym_fmt|
    hdr = eval("#{sym_hdr}")
    fmt = eval("#{sym_fmt}")
    save_sound_as("test.snd", ob, hdr, fmt)
    ab = open_sound("test.snd")
    if header_type(ab) != hdr
      snd_display("save_as %s -> %s?", sym_hdr, mus_header_type_name(header_type(ab)))
    end
    if mus_sound_header_type("test.snd") != hdr
      snd_display("saved_as %s -> %s?",
                  sym_hdr, mus_header_type_name(mus_sound_header_type("test.snd")))
    end
    if data_format(ab) != fmt
      snd_display("save_as %s -> %s?", sym_fmt, mus_data_format_name(data_format(ab)))
    end
    if mus_sound_data_format("test.snd") != fmt
      snd_display("saved_as %s -> %s?",
                  sym_fmt, mus_data_format_name(mus_sound_data_format("test.snd")))
    end
    if fneq((res = sample(1000, ab)), samp)
      snd_display("%s(%s)[1000] = %f (%f)?", sym_hdr, sym_fmt, res, samp)
    end
    close_sound(ab)
  end
  close_sound(ob)
  [["t15.aiff", [[132300, 0.148], [132300, 0.126]]],
    ["M1F1-float64C-AFsp.aif", [[8000, -0.024], [8000, 0.021]]]].each do |f, vals|
    with_file(f) do |fsnd|
      ind = open_sound(fsnd)
      chn = -1
      if vals.detect do |val| chn += 1; fneq(sample(val[0], ind, chn), val[1]) end
        snd_display("%s trouble: %s",
                    fsnd,
                    vals.map_with_index do |val, i| sample(val[0], ind, i) end.inspect)
      end
      close_sound(ind)
    end
  end
  # xen.h
  # 
  # #if HAVE_RB_NUM2LL
  # #define C_TO_XEN_LONG_LONG(a)           C_TO_XEN_ULONG(a)
  # [...]
  # 
  # returns unsigned values on negatives (see below)
  $bad_header_hook.reset_hook!
  $bad_header_hook.add_hook!("snd-test-4") do |n| true end
  [
    ["bad_chans.snd", [0, 22050, 0]],
    ["bad_srate.snd", [1, 0, 0]],
    ["bad_data_format.snd", [1, 22050, 4411]],
    ["bad_chans.aifc", [0, 22050, 0]],
    ["bad_srate.aifc", [1, 0, 0]],
    ["bad_length.aifc", [1, 22050, -10]],
    ["bad_chans.riff", [0, 22050, 0]],
    ["bad_srate.riff", [1, 0, 0]],
    ["bad_chans.nist", [0, 22050, 0]],
    ["bad_srate.nist", [1, 0, 0]],
    ["bad_length.nist", [1, 22050, -10]]].each do |f, vals|
    with_file(f) do |fsnd|
      res = nil
      snd_catch do
        res = [mus_sound_chans(fsnd), mus_sound_srate(fsnd), mus_sound_frames(fsnd)]
      end
      # Ruby's off_t is `unsigned long' not `long long',
      # negative return values are unsigned on bad_length.* files
      if res != vals and /bad_length/ !~ fsnd
        snd_display("%s: %s != %s?", fsnd, res.inspect, vals.inspect)
      end
    end
  end
  ind = open_sound("/usr/include/sys/" + $home_dir + "/cl/oboe.snd")
  if (not sound?(ind)) or (short_file_name(ind) != "oboe.snd")
    snd_display("open_sound with slashes: %s != %s?", ind, (sound?(ind) and short_file_name(ind)))
  end
  ["bad_chans.snd",
    "bad_srate.snd",
    "bad_data_format.snd",
    "bad_chans.aifc",
    "bad_srate.aifc",
    "bad_length.aifc",
    "bad_chans.riff",
    "bad_srate.riff",
    "bad_chans.nist",
    "bad_srate.nist",
    "bad_length.nist"].each do |f|
    with_file(f) do |fsnd|
      snd_catch do
        insert_sound(fsnd)
        convolve_with(fsnd)
        mix(fsnd)
        snd = open_sound(fsnd)
        snd.kind_of?(Numeric) and sound?(snd) and close_sound(snd)
      end
    end
  end
  close_sound(ind)
  ob = open_sound(oboe_snd)
  sd = samples2sound_data
  mx = sound_data_maxamp(sd)
  if (res = sound_data_length(sd)) != 50828
    snd_display("oboe->sd: len %d?", res)
  end
  if fneq((res = sound_data_ref(sd, 0, 1000)), 0.0328369)
    snd_display("oboe->sd[1000]: %f?", res)
  end
  if mx.length != 1
    snd_display("sound_data_maxamp oboe.snd: %s?", sound_data_maxamp(sd).inspect)
  end
  if (res = maxamp(ob, 0)) != mx[0]
    snd_display("sound_data_maxamp oboe.snd: %f != %f?", sound_data_maxamp(sd)[0], res)
  end
  if (res = snd_catch do set_selected_channel(1) end) != :no_such_channel
    snd_display("set_selected_channel bad chan: %s?", res)
  end
  if (res = snd_catch do set_selected_channel(123456, 1) end) != :no_such_sound
    snd_display("set_selected_channel bad snd: %s?", res)
  end
  [[2, 1000], [-1, 1000], [0, -1], [0, 10000000]].each do |chn, frm|
    if (res = snd_catch do sound_data_ref(sd, chn, frm) end) != :out_of_range
      snd_display("sound_data_ref bad chan or frame: %d %d %s?", chn, frm, res.inspect)
    end
  end
  [[2, 1000], [-1, 1000], [0, -1], [0, 10000000]].each do |chn, frm|
    if (res = snd_catch do sound_data_set!(sd, chn, frm, 1) end) != :out_of_range
      snd_display("sound_data_set! bad chan or frame: %d %d %s?", chn, frm, res.inspect)
    end
  end
  v = make_vct(3)
  if (res = snd_catch do vct2sound_data(v, sd, 2) end) != :out_of_range
    snd_display("vct2sound_data bad chan: %s?", res.inspect)
  end
  close_sound(ob)
  snd_display("selected_sound %d %s?", selected_sound, sounds.inspect) if selected_sound
  vals = make_vct(32)
  if mus_audio_mixer_read(Mus_audio_microphone, Mus_audio_amp, 0, vals) == -1
    snd_display("mus_audio_mixer_read?")
  end
  fields.each do |field|
    devices.each do |device|
      snd_catch do
        if mus_audio_mixer_read(device, field, 0, vals) != -1
          mus_audio_mixer_write(device, field, 0, vals)
        end
      end
    end
  end
  with_file("a.sf2") do |fsnd|
    fil = open_sound(fsnd)
    loops = soundfont_info(fil)
    if loops.nil? or loops[0][2] != 65390 or loops[1][1] != 65490
      snd_display("soundfont_info: %s", loops.inspect)
    end
    close_sound(fil)
  end
end

def test074
  fmv5_snd = "fmv5.snd"
  delete_file(fmv5_snd)
  fd = mus_sound_open_output(fmv5_snd, 22050, 1, Mus_bshort, Mus_aiff, "no comment")
  sdata = make_sound_data(1, 100)
  100.times do |i| sound_data_set!(sdata, 0, i, i * 0.01) end
  if sdata.to_s != "#<sound-data: 1 chan, 100 frames>"
    snd_display(", print sound_data: %s?", sdata.to_s)
  end
  edat = sdata
  edat1 = make_sound_data(1, 100)
  edat2 = make_sound_data(2, 100)
  snd_display("sound_data %s != %s?", sdata, edat) if sdata != edat
  snd_display("sound_data 1 %s == %s?", sdata, edat1) if sdata == edat1
  snd_display("sound_data 2 %s == %s?", edat2, edat1) if edat2 == edat1
  100.times do |i| sound_data_set!(edat1, 0, i, sound_data_ref(sdata, 0, i)) end
  snd_display("sound_data 3 %s != %s?", sdata, edat1) if sdata != edat1
  v0 = make_vct(100)
  v1 = make_vct(3)
  sound_data2vct(sdata, 0, v0)
  snd_display("sound_data2vct: %s?", v0) if fneq(vct_ref(v0, 10), 0.1)
  sound_data2vct(sdata, 0, v1)
  snd_display("sound_data2vct (small): %s?", v1) if fneq(vct_ref(v1, 1), 0.01)
  vct2sound_data(v0, sdata, 0)
  if fneq((res = sound_data_ref(sdata, 0, 10)), 0.1)
    snd_display("vct2sound_data: %s", res)
  end
  if (res = snd_catch do sound_data2vct(sdata, 2, v0) end) != :out_of_range
    snd_display("sound_data2vct bad chan: %s?", res.inspect)
  end
  if (res = snd_catch do mus_audio_write(1, make_sound_data(3, 3), 123) end) != :out_of_range
    snd_display("mus_audio_write bad frames: %s?", res.inspect)
  end
  v0 = make_vct(10)
  sdata2 = make_sound_data(2, 10)
  10.times do |i|
    sound_data_set!(sdata2, 0, i, 0.1)
    sound_data_set!(sdata2, 1, i, 0.2)
  end
  sound_data2vct(sdata2, 0, v0)
  snd_display("sound_data2vct[1]: %s?", v0) if fneq(vct_ref(v0, 1), 0.1)
  sound_data2vct(sdata2, 1, v0)
  snd_display("sound_data2vct[2]: %s?", v0) if fneq(vct_ref(v0, 1), 0.2)
  vct2sound_data(v0, sdata2, 0)
  if fneq((res = sound_data_ref(sdata2, 0, 1)), 0.2)
    snd_display("vct2sound_data[2]: %s?", res)
  end
  vct_fill!(v0, 0.3)
  vct2sound_data(v0, sdata2, 1)
  if fneq((res = sound_data_ref(sdata2, 1, 1)), 0.3)
    snd_display("vct2sound_data[3]: %s?", res)
  end
  mus_sound_write(fd, 0, 99, 1, sdata)
  mus_sound_close_output(fd, 200)
  fd = mus_sound_reopen_output(fmv5_snd, 1, Mus_bshort, Mus_aiff, mus_sound_data_location(fmv5_snd))
  mus_sound_close_output(fd, 200)
  fd = mus_sound_open_input(fmv5_snd)
  mus_sound_read(fd, 0, 99, 1, sdata)
  if fneq((res = sound_data_ref(sdata, 0, 10)), 0.1)
    snd_display("mus_sound_write: %s", res)
  end
  if (pos = mus_sound_seek_frame(fd, 20)) != IO.new(fd).tell
    snd_display("1 mus_sound_seek_frame: %d %d?", pos, IO.new(fd).tell)
  end
  if frame2byte(fmv5_snd, 20) != pos
    snd_display("2 mus_sound_seek_frame: %d %d?", pos, frame2byte(fmv5_snd, 20))
  end
  mus_sound_read(fd, 0, 10, 1, sdata)
  if fneq((res = sound_data_ref(sdata, 0, 0)), 0.2)
    snd_display("2 mus_sound_seek: %s?", res)
  end
  mus_sound_close_input(fd)
  delete_file(fmv5_snd)
  if (res = snd_catch do
        mus_sound_open_output("fmv.snd", 22050, -1, Mus_bshort, Mus_aiff, "no comment")
      end) != :out_of_range
    snd_display("mus_sound_open_output bad chans: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_open_output("fmv.snd", 22050, 1, -1, Mus_aiff, "no comment")
      end) != :out_of_range
    snd_display("mus_sound_open_output bad format: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_open_output("fmv.snd", 22050, 1, Mus_bshort, -1, "no comment")
      end) != :out_of_range
    snd_display("mus_sound_open_output bad type: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", -1, Mus_bshort, Mus_aiff, false)
      end) != :out_of_range
    snd_display("mus_sound_reopen_output bad chans: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", 1, -1, Mus_aiff, false)
      end) != :out_of_range
    snd_display("mus_sound_reopen_output bad format: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", 1, Mus_bshort, -1, false)
      end) != :out_of_range
    snd_display("mus_sound_reopen_output bad type: %s?", res)
  end
  delete_file("fmv.snd")
  [:mus_audio_open_output, :mus_audio_open_input].each do |sym|
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, -1, Mus_lshort, 512)
        end) != :out_of_range
      snd_display("%s bad chans: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, 1, -1, 512)
        end) != :out_of_range
      snd_display("%s bad format: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, -1, 22050, 1, Mus_lshort, 512)
        end) != :out_of_range
      snd_display("%s bad device: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, -22050, 1, Mus_lshort, 512)
        end) != :out_of_range
      snd_display("%s bad srate: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, 1, Mus_lshort, -512)
        end) != :out_of_range
      snd_display("%s bad size: %s", sym, res)
    end
  end
  ["trunc.snd",
    "trunc.aiff",
    "trunc.wav",
    "trunc.sf",
    "trunc.voc",
    "trunc.nist",
    "bad.wav",
    "badform.aiff",
    "trunc1.aiff"].each do |file|
    with_file(file) do |fsnd|
      if (res = snd_catch do open_sound(fsnd) end) != :mus_error
        snd_display("open_sound %s: %s", file, res)
      end
    end
  end
  $open_raw_sound_hook.add_hook!("snd-test-044") do |file, choice| [1, 22050, Mus_bshort] end
  with_file("empty.snd") do |fsnd|
    ind = open_sound(fsnd)
    if data_format(ind) != Mus_bshort or
        channels(ind) != 1 or
        srate(ind) != 22050 or
        data_location(ind) != 0 or
        frames(ind) != 0
      snd_display("open raw: %d %d %d %d %d?",
                  data_format(ind),
                  channels(ind),
                  srate(ind),
                  data_location(ind),
                  frames(ind))
    end
    close_sound(ind)
  end
  $open_raw_sound_hook.reset_hook!
  vals = make_vct(32)
  [:mus_audio_mixer_read, :mus_audio_mixer_write].each do |sym|
    if (res = snd_catch do send(sym, -1, Mus_audio_amp, 0, vals) end) != :out_of_range
      snd_display("%s bad device: %s?", sym, res)
    end
    if (res = snd_catch do send(sym, Mus_audio_microphone, -1, 0, vals) end) != :out_of_range
      snd_display("%s bad fields: %s?", sym, res)
    end
  end
  mus_audio_mixer_write(Mus_audio_microphone, Mus_audio_amp, 0, make_vct(1))
  ind = open_sound("/usr/local/" + Dir.pwd + "/2.snd")
  sd1 = samples2sound_data(12000, 10, ind, 0)
  vc1 = sound_data2vct(sd1)
  vc2 = samples2vct(12000, 10, ind, 0)
  sd2 = vct2sound_data(vc2)
  snd_display("samples2sound_data2vct: %s %s?", vc1.inspect, vc2.inspect) if vc1 != vc2
  snd_display("sound_data2vct2sound_data: %s %s?", sd1.inspect, sd2.inspect) if sd1 != sd2
  scale_by(2.0, ind, 0)
  sd1 = samples2sound_data(12000, 10, ind, 0, false, 0)
  vc1 = sound_data2vct(sd1)
  vc2 = samples2vct(12000, 10, ind, 0, false, 0)
  sd2 = vct2sound_data(vc2)
  snd_display("edpos samples2sound_data2vct: %s %s?", vc1.inspect, vc2.inspect) if vc1 != vc2
  snd_display("edpos sound_data2vct2sound_data: %s %s?", sd1.inspect, sd2.inspect) if sd1 != sd2
  sd1 = samples2sound_data(12000, 10, ind, 1)
  vc1 = sound_data2vct(sd1)
  vc2 = samples2vct(12000, 10, ind, 1)
  sd2 = vct2sound_data(vc2)
  snd_display("1 samples2sound_data2vct: %s %s?", vc1.inspect, vc2.inspect) if vc1 != vc2
  snd_display("1 sound_data2vct2sound_data: %s %s?", sd1.inspect, sd2.inspect) if sd1 != sd2
  scale_by(2.0, ind, 1)
  sd1 = samples2sound_data(12000, 10, ind, 1)
  vc1 = sound_data2vct(sd1)
  vc2 = samples2vct(12000, 10, ind, 1)
  sd2 = vct2sound_data(vc2)
  snd_display("1 scaled samples2sound_data2vct: %s %s?", vc1.inspect, vc2.inspect) if vc1 != vc2
  snd_display("1 scaled sound_data2vct2sound_data: %s %s?", sd1.inspect, sd2.inspect) if sd1 != sd2
  close_sound(ind)
  sd1 = make_sound_data(1, 32)
  sd2 = make_sound_data(2, 64)
  32.times do |i| sound_data_set!(sd1, 0, i, i * 0.01) end
  64.times do |i|
    sound_data_set!(sd2, 0, i, i * 0.1)
    sound_data_set!(sd2, 1, i, i * 0.2)
  end
  sound_data2sound_data(sd2, sd1, 3, 6, 32)
  [[0, 0.0], [2, 0.02], [3, 0.0], [6, 0.3], [10, 0.1]].each do |idx, val|
    if fneq(res = sound_data_ref(sd1, 0, idx), val)
      snd_display("sound_data2sound_data %d: %f?", idx, res)
    end
  end
  sound_data2sound_data(sd1, sd2, 0, 10, 32)
  if fneq(res = sound_data_ref(sd2, 0, 5), 0.2)
      snd_display("sound_data2sound_data 2 5: %f?", res)
  end
end

def test084
  [1, 2, 4, 8].each do |chans|
    [[Mus_bshort, Mus_next],
      [Mus_bfloat, Mus_aifc],
      [Mus_lshort, Mus_aifc],
      [Mus_lfloat, Mus_riff],
      [Mus_lshort, Mus_nist],
      [Mus_bint, Mus_aiff],
      [Mus_lint, Mus_next],
      [Mus_bintn, Mus_next],
      [Mus_lintn, Mus_next],
      [Mus_b24int, Mus_aifc],
      [Mus_l24int, Mus_riff],
      [Mus_bfloat, Mus_ircam],
      [Mus_bfloat_unscaled, Mus_next],
      [Mus_lfloat_unscaled, Mus_next],
      [Mus_bdouble_unscaled, Mus_next],
      [Mus_ldouble_unscaled, Mus_next],
      [Mus_bdouble, Mus_next],
      [Mus_ldouble, Mus_next],
      [Mus_ulshort, Mus_next],
      [Mus_ubshort, Mus_next]].each do |df, ht|
      samps = case chans
              when 1
                100000
              when 2
                50000
              else
                1000
              end
      sdata = make_sound_data(chans, samps)
      ndata = make_sound_data(chans, samps)
      chans.times do |chn|
        samps.times do |i|
          sound_data_set!(sdata, chn, i, mus_random(1.0))
        end
      end
      delete_file("fmv5.snd")
      fd = mus_sound_open_output("fmv5.snd", 22050, chans, df, ht, "no, comment")
      mus_sound_write(fd, 0, samps - 1, chans, sdata)
      mus_sound_close_output(fd, (samps * chans * mus_bytes_per_sample(df)))
      fd = mus_sound_open_input("fmv5.snd")
      mus_sound_read(fd, 0, samps - 1, chans, ndata)
      pos = mus_sound_seek_frame(fd, 100)
      if (res = IO.new(fd).tell) != pos
        snd_display("mus_sound_seek_frame[%d]: chans %d %d (%s %s)?",
                    pos, chans, res, mus_header_type_name(ht), mus_data_format_name(df))
      end
      if (res = frame2byte("fmv5.snd", 100)) != pos
        snd_display("mus_sound_seek_frame(100): chans %d %d %d (%s %s)?",
                    chans, pos, res, mus_header_type_name(ht), mus_data_format_name(df))
      end
      mus_sound_close_input(fd)
      delete_file("fmv5.snd")
      res = catch(:read_write_error) do
        chans.times do |chn|
          samps.times do |i|
            if fneq(x = sound_data_ref(sdata, chn, i), y = sound_data_ref(ndata, chn, i))
              throw(:read_write_error, format("read_write trouble: %s %s",
                                              mus_header_type_name(ht),
                                              mus_data_format_name(df)))
            end
          end
        end
      end
      snd_display(res) if res.kind_of?(String)
    end
  end
end

def test094
  fmv = "fmv.snd"
  fd = mus_sound_open_output(fmv, 22050, 1, Mus_bshort, Mus_next, "no comment")
  sdata = make_sound_data(1, 10)
  sound_data_set!(sdata, 0, 1, 0.1)
  mus_sound_write(fd, 0, 9, 1, sdata)
  mus_sound_close_output(fd, 20)
  fd = mus_sound_open_input(fmv)
  mus_sound_read(fd, 0, 9, 1, sdata)
  if fneq(sound_data_ref(sdata, 0, 0), 0.0) or
      fneq(sound_data_ref(sdata, 0, 1), 0.1) or
      fneq(sound_data_ref(sdata, 0, 2), 0.0) or
      fneq(sound_data_ref(sdata, 0, 6), 0.0)
    snd_display("read/write: %s?", sound_data2list(sdata).inspect)
  end
  mus_sound_close_input(fd)
  fd = mus_sound_reopen_output(fmv, 1, Mus_bshort, Mus_next, mus_sound_data_location(fmv))
  mus_sound_seek_frame(fd, 0)
  sound_data_set!(sdata, 0, 2, 0.1)
  sound_data_set!(sdata, 0, 3, 0.1)
  mus_sound_write(fd, 0, 9, 1, sdata)
  mus_sound_close_output(fd, 20)
  fd = mus_sound_open_input(fmv)
  mus_sound_read(fd, 0, 9, 1, sdata)
  if fneq(sound_data_ref(sdata, 0, 0), 0.0) or
      fneq(sound_data_ref(sdata, 0, 1), 0.1) or
      fneq(sound_data_ref(sdata, 0, 2), 0.1) or
      fneq(sound_data_ref(sdata, 0, 3), 0.1) or
      fneq(sound_data_ref(sdata, 0, 6), 0.0)
    snd_display("read/write: %s?", sound_data2list(sdata).inspect)
  end
  mus_sound_close_input(fd)
  #
  # check data_clipped choices
  # 
  ind = view_sound("oboe.snd")
  set_data_clipped(false)
  map_channel(lambda do |y| y * 10.0 end, 0, frames(), ind, 0)
  save_sound_as("test.snd", ind, Mus_next, Mus_bfloat)
  undo(1, ind, 0)
  ind1 = open_sound("test.snd")
  if fneq(res1 = maxamp(ind1, 0), (res2 = 10.0 * maxamp(ind, 0)))
    snd_display("clipping 0: %f %f?", res1, res2)
  end
  close_sound(ind1)
  delete_file("test.snd")
  set_data_clipped(true)
  map_channel(lambda do |y| y * 10.0 end, 0, frames(), ind, 0)
  save_sound_as("test.snd", ind, Mus_next, Mus_bfloat)
  undo(1, ind, 0)
  ind1 = open_sound("test.snd")
  if fneq(res = maxamp(ind1, 0), 1.0)
    snd_display("clipping 1: %f %f?", res, maxamp(ind, 0))
  end
  close_sound(ind1)
  delete_file("test.snd")
  set_data_clipped(false)
  mx = maxamp(ind)
  map_channel(lambda do |y| y + (1.001 - mx) end, 0, frames(), ind, 0)
  save_sound_as("test.snd", ind, Mus_next, Mus_bshort)
  ind1 = open_sound("test.snd")
  unless (res = scan_channel(lambda do |y| y < 0.0 end)).kind_of?(Array)
    snd_display("clipping 2: %s?", res.inspect)
  end
  close_sound(ind1)
  delete_file("test.snd")
  set_data_clipped(true)
  save_sound_as("test.snd", ind, Mus_next, Mus_bshort)
  ind1 = open_sound("test.snd")
  unless (res = scan_channel(lambda do |y| y < 0.0 end)).kind_of?(Array)
    snd_display("clipping 3: %s?", res.inspect)
  end
  close_sound(ind1)
  delete_file("test.snd")
  set_data_clipped(false)
  close_sound(ind)
  delete_file(fmv)
  com = "this is a comment which we'll repeat enough times to trigger an internal loop" * 3
  mus_sound_open_output(fmv, 22050, 4, Mus_lshort, Mus_riff, com)
  sdata = make_sound_data(4, 10)
  4.times do |i| sound_data_set!(sdata, i, 1, 0.1) end
  mus_sound_write(fd, 0, 9, 4, sdata)
  mus_sound_close_output(fd, 80)
  fd = mus_sound_open_input(fmv)
  mus_sound_read(fd, 0, 9, 4, sdata)
  4.times do |i|
    if fneq(sound_data_ref(sdata, i, 0), 0.0) or
        fneq(sound_data_ref(sdata, i, 1), 0.1) or
        fneq(sound_data_ref(sdata, i, 2), 0.0) or
        fneq(sound_data_ref(sdata, i, 6), 0.0)
    snd_display("read/write[%d]: %s?", i, sound_data_channel2list(sdata, i).inspect)
    end
  end
  mus_sound_close_input(fd)
  fd = mus_sound_reopen_output(fmv, 4, Mus_lshort, Mus_riff, mus_sound_data_location(fmv))
  mus_sound_seek_frame(fd, 0)
  4.times do |i|
    sound_data_set!(sdata, i, 2, 0.1)
    sound_data_set!(sdata, i, 3, 0.1)
  end
  mus_sound_write(fd, 0, 9, 4, sdata)
  mus_sound_close_output(fd, 80)
  fd = mus_sound_open_input(fmv)
  mus_sound_read(fd, 0, 9, 4, sdata)
  if fneq(sound_data_ref(sdata, 0, 0), 0.0) or
      fneq(sound_data_ref(sdata, 0, 1), 0.1) or
      fneq(sound_data_ref(sdata, 0, 2), 0.1) or
      fneq(sound_data_ref(sdata, 0, 3), 0.1) or
      fneq(sound_data_ref(sdata, 0, 6), 0.0)
    snd_display("re-read/write[%d]: %s?", i, sound_data_channel2list(sdata, i).inspect)
  end
  mus_sound_close_input(fd)
  delete_file(fmv)
  with_file("32bit.sf") do |fsnd|
    ind = open_sound(fsnd)
    if fneq(res = maxamp(ind, 0), 0.228)
      snd_display("32bit max: %f?", res)
    end
    close_sound(ind)
  end
  [
    ["next-dbl.snd", 10, 10,
      vct(0.475, 0.491, 0.499, 0.499, 0.492, 0.476, 0.453, 0.423, 0.387, 0.344)],
    ["oboe.ldbl", 1000, 10,
      vct(0.033, 0.035, 0.034, 0.031, 0.026, 0.020, 0.013, 0.009, 0.005, 0.004)],
    ["next-flt.snd", 10, 10,
      vct(0.475, 0.491, 0.499, 0.499, 0.492, 0.476, 0.453, 0.423, 0.387, 0.344)],
    ["clbonef.wav", 1000, 10,
      vct(0.111, 0.101, 0.070, 0.032, -0.014, -0.060, -0.085, -0.108, -0.129, -0.152)],
    ["next-8.snd", 10, 10,
      vct(0.898, 0.945, 0.977, 0.992, 0.992, 0.977, 0.945, 0.906, 0.844, 0.773)],
    ["o2_u8.wave", 1000, 10,
      vct(-0.164, -0.219, -0.258, -0.242, -0.180, -0.102, -0.047, 0.000, 0.039, 0.055)],
    ["next-16.snd", 1000, 10,
      vct(-0.026, -0.022, -0.024, -0.030, -0.041, -0.048, -0.050, -0.055, -0.048, -0.033)],
    ["o2.wave", 1000, 10,
      vct(-0.160, -0.216, -0.254, -0.239, -0.175, -0.102, -0.042, 0.005, 0.041, 0.059)],
    ["o2_18bit.aiff", 1000, 10,
      vct(-0.160, -0.216, -0.254, -0.239, -0.175, -0.102, -0.042, 0.005, 0.041, 0.059)],
    ["o2_12bit.aiff", 1000, 10,
      vct(-0.160, -0.216, -0.254, -0.239, -0.175, -0.102, -0.042, 0.005, 0.041, 0.059)],
    ["next24.snd", 1000, 10,
      vct(-0.160, -0.216, -0.254, -0.239, -0.175, -0.102, -0.042, 0.005, 0.041, 0.059)],
    ["mono24.wav", 1000, 10,
      vct(0.005, 0.010, 0.016, 0.008, -0.007, -0.018, -0.025, -0.021, -0.005, 0.001)],
    ["o2_711u.wave", 1000, 10,
      vct(-0.164, -0.219, -0.254, -0.242, -0.172, -0.103, -0.042, 0.005, 0.042, 0.060)],
    ["alaw.wav", 1000, 10,
      vct(-0.024, -0.048, -0.024, 0.000, 0.008, 0.008, 0.000, -0.040, -0.064, -0.024)],
    ["b32.pvf", 1000, 10,
      vct(-0.160, -0.216, -0.254, -0.239, -0.175, -0.102, -0.042, 0.005, 0.041, 0.059)],
    ["b32.wave", 1000, 10,
      vct(-0.160, -0.216, -0.254, -0.239, -0.175, -0.102, -0.042, 0.005, 0.041, 0.059)],
    ["b32.snd", 1000, 10,
      vct(-0.160, -0.216, -0.254, -0.239, -0.175, -0.102, -0.042, 0.005, 0.041, 0.059)],
    ["32bit.sf", 1000, 10,
      vct(0.016, 0.014, 0.013, 0.011, 0.010, 0.010, 0.010, 0.010, 0.012, 0.014)],
    ["nist-shortpack.wav", 10000, 10,
      vct(0.021, 0.018, 0.014, 0.009, 0.004, -0.001, -0.004, -0.006, -0.007, -0.008)],
    ["wood.sds", 1000, 10,
      vct(-0.160, -0.216, -0.254, -0.239, -0.175, -0.102, -0.042, 0.005, 0.041, 0.059)],
    ["oboe.g721", 1000, 10,
      vct(-0.037, -0.040, -0.040, -0.041, -0.042, -0.038, -0.028, -0.015, -0.005, 0.002)],
    ["oboe.g723_40", 1000, 10,
      vct(-0.037, -0.040, -0.041, -0.041, -0.041, -0.038, -0.028, -0.015, -0.005, 0.003)],
    ["mus10.snd", 10000, 10,
      vct(0.004, 0.001, 0.005, 0.009, 0.017, 0.015, 0.008, 0.011, 0.009, 0.012)],
    ["ieee-text-16.snd", 1000, 10,
      vct(-0.052, -0.056, -0.069, -0.077, -0.065, -0.049, -0.054, -0.062, -0.066, -0.074)],
    ["hcom-16.snd", 10000, 10,
      vct(0.000, 0.000, 0.000, 0.008, 0.000, -0.016, -0.016, -0.016, -0.008, 0.000)],
    ["ce-c3.w02", 1000, 10,
      vct(0.581, 0.598, 0.596, 0.577, 0.552, 0.530, 0.508, 0.479, 0.449, 0.425)],
    ["nasahal.avi", 20000, 10,
      vct(0.390, 0.120, -0.399, -0.131, 0.464, 0.189, -0.458, -0.150, 0.593, 0.439)],
    ["oki.wav", 100, 10,
      vct(0.396, 0.564, 0.677, 0.779, 0.761, 0.540, 0.209, -0.100, -0.301, -0.265)],
    ["trumps22.adp", 5000, 10,
      vct(0.267, 0.278, 0.309, 0.360, 0.383, 0.414, 0.464, 0.475, 0.486, 0.495)]
  ].each do |file, beg, dur, data|
    with_file(file) do |fsnd|
      ind = open_sound(fsnd)
      ndata = samples2vct(beg, dur, ind, 0)
      snd_display("%s: %s != %s", file, data.inspect, ndata.inspect) unless vequal(data, ndata)
      close_sound(ind)
    end
  end
end

def test104
  ["no error",
    "no frequency method",
    "no phase method",
    "null gen arg to method",
    "no length method",
    "no free method",
    "no describe method",
    "no data method",
    "no scaler method",
    "memory allocation failed",
    "unstable two pole error",
    "can't open file",
    "no sample input",
    "no sample output",
    "no such channel",
    "no file name provided",
    "no location method",
    "no channel method",
    "no such fft window",
    "unsupported data format",
    "header read failed",
    "unsupported header type",
    "file descriptors not initialized",
    "not a sound file",
    "file closed",
    "write error",
    "header write failed",
    "can't open temp file",
    "interrupted",
    "bad envelope",
    "audio channels not available",
    "audio srate not available",
    "audio format not available",
    "no audio input available",
    "audio configuration not available",
    "no audio lines available",
    "audio write error",
    "audio size not available",
    "audio device not available",
    "can't close audio",
    "can't open audio",
    "audio read error",
    "audio amp not available",
    "can't write audio",
    "can't read audio",
    "no audio read permission",
    "can't close file",
    "arg out of range",
    "midi open error",
    "midi read error",
    "midi write error",
    "midi close error",
    "midi init error",
    "midi misc error",
    "no channels method",
    "no hop method",
    "no width method",
    "no file-name method",
    "no ramp method",
    "no run method",
    "no increment method",
    "no inspect method",
    "no offset method",
    "no xcoeff method",
    "no ycoeff method",
    "no xcoeffs method",
    "no ycoeffs method"].each_with_index do |err, i|
    if (res = mus_error_to_string(i)) != err
      snd_display("mus_error_to_string %d: %s %s?", i, err.inspect, res.inspect)
    end
  end
  new_id = mus_make_error("hiho all messed up")
  if (res = mus_error_to_string(new_id)) != "hiho all messed up"
    snd_display("mus_make_error %d: %s?", new_id, res.inspect)
  end
  oboe_snd = "oboe.snd"
  cur_srate = mus_sound_srate(oboe_snd)
  cur_chans = mus_sound_chans(oboe_snd)
  cur_format = mus_sound_data_format(oboe_snd)
  cur_type = mus_sound_header_type(oboe_snd)
  cur_loc = mus_sound_data_location(oboe_snd)
  cur_samps = mus_sound_samples(oboe_snd)
  set_mus_sound_srate(oboe_snd, cur_srate * 2)
  if (res = mus_sound_srate(oboe_snd)) != (cur_srate * 2)
    snd_display("set_mus_sound_srate: %s != %s", cur_srate, res)
  end
  set_mus_sound_samples(oboe_snd, cur_samps * 2)
  if (res = mus_sound_samples(oboe_snd)) != (cur_samps * 2)
    snd_display("set_mus_sound_samples: %s != %s", cur_samps, res)
  end
  set_mus_sound_chans(oboe_snd, cur_chans * 2)
  if (res = mus_sound_chans(oboe_snd)) != (cur_chans * 2)
    snd_display("set_mus_sound_chans: %s != %s", cur_chans, res)
  end
  set_mus_sound_data_location(oboe_snd, cur_loc * 2)
  if (res = mus_sound_data_location(oboe_snd)) != (cur_loc * 2)
    snd_display("set_mus_sound_data_location: %s != %s", cur_loc, res)
  end
  set_mus_sound_header_type(oboe_snd, Mus_nist)
  if (res = mus_sound_header_type(oboe_snd)) != Mus_nist
    snd_display("set_mus_sound_header_type: %s != %s", cur_type, res)
  end
  set_mus_sound_data_format(oboe_snd, Mus_lintn)
  if (res = mus_sound_data_format(oboe_snd)) != Mus_lintn
    snd_display("set_mus_sound_data_format: %s != %s", cur_type, res)
  end
  set_mus_sound_srate(oboe_snd, cur_srate)
  set_mus_sound_samples(oboe_snd, cur_srate)
  set_mus_sound_chans(oboe_snd, cur_srate)
  set_mus_sound_data_location(oboe_snd, cur_srate)
  set_mus_sound_header_type(oboe_snd, cur_srate)
  set_mus_sound_data_format(oboe_snd, cur_srate)
end

# with big file
#
# with_sound(:output, $bigger_snd, :srate, 44100, :play, false) do
#   72000.times do |i|
#     fm_violin(i, 0.1, 440, i / 72000.0 * 0.9 + 0.01)
#   end
# end

def test114
  if File.exists?($bigger_snd)
    if (res = mus_sound_samples($bigger_snd)) != 3175160310
      snd_display("bigger samples: %d?", res)
    end
    if (res = mus_sound_frames($bigger_snd)) != 3175160310
      snd_display("bigger frames: %d?", res)
    end
    if (res = mus_sound_length($bigger_snd)) != 6350320648
      snd_display("bigger bytes: %d?", res)
    end
    if fneq(res = mus_sound_duration($bigger_snd),71999.1015)
      snd_display("bigger dur: %f?", res)
    end
    let(open_sound($bigger_snd)) do |ind|
      if (res = frames(ind)) != 3175160310
        snd_display("bigger frames: %d?", res)
      end
      if (res = frames(ind, 0, 0)) != 3175160310
        snd_display("bigger edpos-frames: %d?", res)
      end
      let(add_mark(44100 * 50000, ind)) do |m1|
        if (res = mark_sample(m1)) != 44100 * 50000
          snd_display("bigger mark at: %d?", res)
        end
        set_mark_sample(m1, 44100 * 66000)
        if (res = mark_sample(m1)) != 44100 * 66000
          snd_display("bigger mark to: %d?", res)
        end
      end
      unless provided? "snd-nogui"
        if mix?(mx = mix_sound("oboe.snd", 44100 * 60000))
          if (res = mix_position(mx)) != 44100 * 60000
            snd_display("bigger mix at: %d?", res)
          end
          set_mix_position(mx, 44100 * 61000)
          if (res = mix_position(mx)) != 44100 * 61000
            snd_display("bigger mix to: %d?", res)
          end
        else
          snd_display("no mix tag from mix_sound")
        end
        undo(2)
      else
        undo(1)
      end
      let(selection_creates_region) do |old_select|
        set_selection_creates_region(false)
        select_all(ind)
        if (res1 = selection_frames) != (res2 = frames(ind))
          snd_display("bigger select_all: %d %d?", res1, res2)
        end
        set_selection_position(44100 * 50000)
        if (res = selection_position) != (44100 * 50000)
          snd_display("bigger selection_position: %d?", res)
        end
        set_selection_position(0)
        set_selection_frames(44100 * 65000)
        if (res = selection_frames) != (44100 * 65000)
          snd_display("bigger selection_frames: %d?", res)
        end
        set_selection_creates_region(old_select)
      end
      set_cursor(44100 * 50000, ind)
      if (res = cursor(ind)) != (44100 * 50000)
        snd_display("bigger cursor: %d?", res)
      end
      let(backward_sample) do |val|
        if (res = cursor(ind)) != ((44100 * 50000) - 1)
          snd_display("backup bigger cursor: %d", res)
        end
        if val != ((44100 * 50000) - 1)
          snd_display("backup rtn bigger cursor: %d", cursor(ind))
        end
      end
      let(forward_sample) do |val|
        if (res = cursor(ind)) != (44100 * 50000)
          snd_display("up bigger cursor: %d", res)
        end
        if val != (44100 * 50000)
          snd_display("up rtn bigger cursor: %d", cursor(ind))
        end
      end
      let(add_mark(44123 * 51234, ind)) do |m1|
        if (res = mark_sample(m1)) != (44123 * 51234)
          snd_display("bigger mark at: %d", res)
        end
        let(find_mark(44123 * 51234)) do |mid|
          if (not mid.kind_of?(Numeric)) or mid != m1
            snd_display("bigger find_mark: %s %s", mid.inspect, m1.inspect)
          end
        end
      end
      unless provided? "snd-nogui"
        let(mix_sound("oboe.snd", 44123 * 51234),
            find_mix(44123 * 51234)) do |mx, mxd|
          if (not mxd.kind_of?(Numeric)) or mxd != mx
            snd_display("bigger find_mix: %s %s", mxd.inspect, mx.inspect)
          end
        end
      end
      set_cursor(44123 * 51234, ind)
      if (res = cursor(ind)) != (44123 * 51234)
        snd_display("bigger cursor 123: %d?", res)
      end
      close_sound(ind)
    end
  end
end

def test124
  ind = new_sound("tmp.snd", Mus_riff, Mus_l24int, 22050, 1, :size, 100000)
  x = -0.5
  incr = 1.0 / frames()
  map_channel(lambda do |n|
                val = x
                x += incr
                val
              end)
  save_sound
  close_sound(ind)
  ind = open_sound("tmp.snd")
  reg = select_all
  [[:Mus_next, :Mus_l24int],
    [:Mus_aifc, :Mus_l24int],
    [:Mus_next, :Mus_l24int],
    [:Mus_next, :Mus_bfloat]].each do |ht, df|
    save_selection("tmp1.snd", eval("#{ht}"), eval("#{df}"))
    ind1 = open_sound("tmp1.snd")
    x = -0.5
    incr = 1.0 / frames()
    err = scan_channel(lambda do |n|
                         val = x
                         x += incr
                         fneq(val, n)
                       end, 0, 100000, ind1)
    if err.kind_of?(Array)
      snd_display("%s (%s) selection not saved correctly? %s", df, ht, err.inspect)
    end
    close_sound(ind1)
  end
  delete_file("tmp1.snd")
  close_sound(ind)
  delete_file("tmp.snd")
  ind = new_sound("tmp.snd", Mus_next, Mus_bfloat, 22050, 1, :size, 10, :comment, false)
  map_channel(lambda do |y| 1.0 end)
  env_channel([0.0, 0.0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4,
                0.5, 0.5, 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9])
  unless vequal((res = channel2vct), vct(0.000, 0.100, 0.200, 0.300, 0.400,
                                         0.500, 0.600, 0.700, 0.800, 0.900))
    snd_display("ramp env by 0.1: %s", res.inspect)
  end
  close_sound(ind)
  delete_file("tmp.snd")
end

# unrecoverable sndlib errors on test134
def test134
  magic_words = [".snd", "FORM", "AIFF", "AIFC", "COMM", "COMT", "INFO", "INST", "inst", "MARK",
    "SSND", "FVER", "NONE", "ULAW", "ulaw", "ima4", "raw ", "sowt", "in32", "in24",
    "ni23", "fl32", "FL32", "fl64", "twos", "ALAW", "alaw", "APPL", "CLM ", "RIFF",
    "RIFX", "WAVE", "fmt ", "data", "fact", "clm ", "NIST", "8SVX", "16SV", "Crea",
    "tive", "SOUN", "D SA", "MPLE", "BODY", "VHDR", "CHAN", "ANNO", "NAME", "2BIT",
    "HCOM", "FSSD", "%//\n", "%---", "ALaw", "Soun", "MAUD", "MHDR", "MDAT", "mdat",
    "MThd", "sfbk", "sdta", "shdr", "pdta", "LIST", "GF1P", "ATCH", "$SIG", "NAL_",
    "GOLD", " SAM", "SRFS", "Diam", "ondW", "CSRE", "SND ", "SNIN", "SNDT", "DDSF",
    "FSMu", "UWFD", "LM89", "SY80", "SY85", "SCRS", "DSPL", "AVI ", "strf", "movi",
    "PRAM", " paf", "fap ", "DS16", "HEDR", "HDR8", "SDA_", "SDAB", "SD_B", "NOTE",
    "file", "=sam", "SU7M", "SU7R", "PVF1", "PVF2", "AUTH", "riff", "TWIN", "IMPS",
    "SMP1", "Maui", "SDIF", "NVF "]
  len = magic_words.length
  ctr = 0
  $open_raw_sound_hook.reset_hook!
  $open_raw_sound_hook.add_hook!("snd-test-104") do |a, b| true end
  $bad_header_hook.reset_hook!
  $bad_header_hook.add_hook!("snd-test-104") do |n| true end
  magic_words.each do |magic|
    delete_file("test.snd")
    mus_sound_forget("test.snd")
    File.open("test.snd", "w") do |fp|
      fp.write magic
      128.times do fp.write(mus_random(1.0)) end
    end
    res = snd_catch do open_sound("test.snd") end
    if res.kind_of?(Numeric) and sound?(res)
      snd_display("open_sound garbage %s: %s?", magic, res)
    end
    delete_file("test.snd")
    mus_sound_forget("test.snd")
    File.open("test.snd", "w") do |fp|
      fp.write magic
      128.times do fp.write(mus_random(128)) end
    end
    res = snd_catch do open_sound("test.snd") end
    if res.kind_of?(Numeric) and sound?(res)
      snd_display("open_sound plausible garbage %s: %s?", magic, res)
    end
    delete_file("test.snd")
    mus_sound_forget("test.snd")
    File.open("test.snd", "w") do |fp|
      fp.write magic
      (1...12).each do
        if (ctr + 1) < len
          fp.write(magic_words[ctr + 1])
        else
          fp.write(magic_words[i])
        end
      end
    end
    res = snd_catch do open_sound("test.snd") end
    if res.kind_of?(Numeric) and sound?(res)
      snd_display("open_sound very plausible garbage %s: %s?", magic, res)
    end
    ctr += 1
  end
  delete_file("test.snd")
  mus_sound_forget("test.snd")
end

def make_aifc_file(frames, auth_lo, bits)
  File.open("test.aif", "w") do |fp|
    fp.write "FORM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0146); # len
    fp.write "AIFCFVER"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0004); # version chunk size
    fp.putc(0242); fp.putc(0200); fp.putc(0121); fp.putc(0100); # version
    fp.write "COMM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0046); # COMM chunk size
    fp.putc(0000); fp.putc(0001);                 # 1 chan
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(frames); # frames
    fp.putc(0000); fp.putc(bits);                 # bits
    fp.putc(0100); fp.putc(0016); fp.putc(0254); fp.putc(0104); fp.putc(0000);
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000);
    # srate as 80-bit float (sheesh)
    fp.write "NONE"                               # compression
    fp.putc(0016);                                # pascal string len
    fp.write "not compressed"
    fp.putc(0000);
    fp.write "AUTH"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(auth_lo); # AUTH chunk size
    fp.write "bil"
    fp.putc(0000);
    fp.write "SSND"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0014); # SSND chunk size
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # SSND data loc
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # block size?
    fp.putc(0000); fp.putc(0101); fp.putc(0000); fp.putc(0100); # two samples
  end
end

def test144
  $open_raw_sound_hook.reset_hook!
  $open_raw_sound_hook.add_hook!("snd-test-114") do |a, b| true end
  $bad_header_hook.reset_hook!
  $bad_header_hook.add_hook!("snd-test-114") do |n| true end
  delete_file("test.snd")
  mus_sound_forget("test.snd")
  File.open("test.snd", "w") do |fp|
    fp.write ".snd"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0034); # location
    fp.putc(0000); fp.putc(0001); fp.putc(0215); fp.putc(0030); # nominal size
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0022); # format
    fp.putc(0000); fp.putc(0000); fp.putc(0126); fp.putc(0042); # srate
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0001); # chans
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # comment
    fp.putc(0000); fp.putc(0001); # samp 1
  end
  if (res = mus_sound_data_format("test.snd")) != Mus_bshort
    snd_display("next 18: %d?", res)
  end
  delete_file("test.snd")
  mus_sound_forget("test.snd")
  File.open("test.snd", "w") do |fp|
    fp.write ".snd"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0004); # location
    fp.putc(0000); fp.putc(0001); fp.putc(0215); fp.putc(0030); # nominal size
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0022); # format
    fp.putc(0000); fp.putc(0000); fp.putc(0126); fp.putc(0042); # srate
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0001); # chans
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # comment
    fp.putc(0000); fp.putc(0001); # samp 1
  end
  res = snd_catch do open_sound("test.snd") end
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound next bad location %d: %d?", data_location(res), res)
    close_sound(res)
  end
  delete_file("test.snd")
  mus_sound_forget("test.snd")
  File.open("test.snd", "w") do |fp|
    fp.write ".snd"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0034); # location
    fp.putc(0000); fp.putc(0001); fp.putc(0215); fp.putc(0030); # nominal size
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0122); # format
    fp.putc(0000); fp.putc(0000); fp.putc(0126); fp.putc(0042); # srate
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0001); # chans
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # comment
    fp.putc(0000); fp.putc(0001); # samp 1
  end
  res = snd_catch do open_sound("test.snd") end
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound next bad format %d: %d?", data_format(res), res)
    close_sound(res)
  end
  delete_file("test.snd")
  mus_sound_forget("test.snd")
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  # 
  # correct make_aifc_file(002, 004, 020)
  # 
  make_aifc_file(0102, 004, 020)
  ind = open_sound("test.aif")
  if (res = frames(ind)) != 2
    snd_display("bad frames in header: %d", res)
  end
  close_sound(ind)
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  make_aifc_file(002, 150, 020)
  res = snd_catch do open_sound("test.aif") end
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound aifc no ssnd chunk %d: %d?", data_location(res), res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  make_aifc_file(002, 000, 020)
  res = snd_catch do open_sound("test.aif") end
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound aifc 0-len auth chunk %d: %d?", data_location(res), res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  make_aifc_file(002, 150, 120)
  res = snd_catch do open_sound("test.aif") end
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound aifc bits 80 %d: %d?", data_format(res), res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  File.open("test.aif", "w") do |fp|
    fp.write "FORM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0176); # len
    fp.write "AIFCFVER"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0004); # version chunk size
    fp.putc(0242); fp.putc(0200); fp.putc(0121); fp.putc(0100); # version
    fp.write "COMM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0046); # COMM chunk size
    fp.putc(0000); fp.putc(0001);                 # 1 chan
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0002); # frames
    fp.putc(0000); fp.putc(0020);                 # bits
    fp.putc(0100); fp.putc(0016); fp.putc(0254); fp.putc(0104); fp.putc(0000);
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000);
    # srate as 80-bit float (sheesh)
    fp.write "NONE"                               # compression
    fp.putc(0016);                                # pascal string len
    fp.write "not compressed"
    fp.putc(0000);
    fp.write "AUTH"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0004); # AUTH chunk size
    fp.write "bil"
    fp.putc(0000);
    fp.write "ANNO"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0004); # AUTH chunk size
    fp.write "cat"
    fp.putc(0000);
    fp.write "NAME"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0004); # AUTH chunk size
    fp.write "dog"
    fp.putc(0000);
    fp.write "SSND"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0004); # AUTH chunk size

    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0014); # SSND chunk size
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # SSND data loc
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # block size?
    fp.putc(0000); fp.putc(0101); fp.putc(0000); fp.putc(0100); # two samples
  end
  if (res = mus_sound_comment("test.aif")).length != 15
    snd_display("aifc 3 aux comments: %s", res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  File.open("test.aif", "w") do |fp|
    fp.write "FORM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0142); # len
    fp.write "AIFC"
    fp.write "SSND"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0014); # SSND chunk size
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # SSND data location
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # block size?
    fp.putc(0000); fp.putc(0101); fp.putc(0000); fp.putc(0100); # two samples
    fp.write "COMM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0046); # COMM chunk size
    fp.putc(0000); fp.putc(0001);                 # 1 chan
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0002); # frames
    fp.putc(0000); fp.putc(0020);                 # bits
    fp.putc(0100); fp.putc(0016); fp.putc(0254); fp.putc(0104); fp.putc(0000);
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000);
    # srate as 80-bit float (sheesh)
    fp.write "NONE"                               # compression
    fp.putc(0016);                                # pascal string len
    fp.write "not compressed"
    fp.putc(0000);
    fp.write "COMT"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0014);
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000);
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000);
    fp.write "bil"
    fp.putc(0000);
  end
  if (res = mus_sound_comment("test.aif"))[0..2] != "bil"
    snd_display("aifc trailing comt comments: %s", res)
  end
  if (res = mus_sound_frames("test.aif")) != 2
    snd_display("aifc trailing comt frames: %d", res)
  end
  ind = open_sound("test.aif")
  if fneq(sample(0), 0.00198) or
      fneq(sample(1), 0.00195) or
      fneq(sample(2), 0.0) or
      fneq(sample(3), 0.0)
    snd_display("aifc trailing comt samps: %d %d %d %d",
                sample(0),  sample(1), sample(2), sample(3))
  end
  close_sound(ind)
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  File.open("test.aif", "w") do |fp|
    fp.write "FORM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0142); # len
    fp.write "AIFC"
    fp.write "SSND"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0014); # SSND chunk size
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # SSND data location
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # block size?
    fp.putc(0000); fp.putc(0101); fp.putc(0000); fp.putc(0100); # two samples
    fp.write "COMM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0046); # COMM chunk size
    fp.putc(0000); fp.putc(0001);                 # 1 chan
    fp.putc(0000); fp.putc(0000); fp.putc(0100); fp.putc(0102); # frames
    fp.putc(0000); fp.putc(0020);                 # bits
    fp.putc(0100); fp.putc(0016); fp.putc(0254); fp.putc(0104); fp.putc(0000);
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000);
    # srate as 80-bit float (sheesh)
    fp.write "NONE"                               # compression
    fp.putc(0016);                                # pascal string len
    fp.write "not compressed"
    fp.putc(0000);
    fp.write "SSND"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0014);
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000);
    fp.putc(0000); fp.putc(0101); fp.putc(0000); fp.putc(0100);
  end
  res = snd_catch do open_sound("test.aif") end
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound aifc 2 ssnd chunks %d: %d?", data_location(res), res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  File.open("test.aif", "w") do |fp|
    fp.write "FORM"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0040); # len
    fp.write "AIFC"
    fp.write "SSND"
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0014); # SSND chunk size
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # SSND data location
    fp.putc(0000); fp.putc(0000); fp.putc(0000); fp.putc(0000); # block size?
    fp.putc(0000); fp.putc(0101); fp.putc(0000); fp.putc(0100); # two samples
  end
  res = snd_catch do open_sound("test.aif") end
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound aifc no comm chunk: %s?", res.inspect)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  $open_raw_sound_hook.reset_hook!
  $bad_header_hook.reset_hook!
end

def test04(formats, comments, fields, devices)
  # to catch sndlib errors
  $snd_error_hook.reset_hook!
  $mus_error_hook.reset_hook!
  $snd_error_hook.add_hook!("snd-test") do |msg| true end
  $mus_error_hook.add_hook!("snd-test") do |type, msg| true end
  clear_listener if $clear_listener
  let(open_sound("/usr//usr/include/" + $home_dir + "/cl/oboe.snd")) do |ind|
    show_input_1
    close_sound(ind)
  end
  test004(formats)
  test014
  test024
  test034(:mus_sound_comment, comments)
  test034(:mus_sound_maxamp,
          [["4.aiff", [810071, 0.245, 810071, 0.490, 810071, 0.735, 810071, 0.980]]])
  with_file("4.aiff") do |f|
    set_mus_sound_maxamp(f, [12345, 0.5, 54321, 0.2, 0, 0.1, 9999, 0.01])
  end
  test034(:mus_sound_maxamp,
          [["4.aiff", [12345, 0.5, 54321, 0.2, 0, 0.1, 9999, 0.01]]])
  test044
  test054
  test064(fields, devices)
  test074
  test084 if $test_long_file_tests
  test094
  test104
  test114 if $with_big_file
  test124
  # unrecoverable sndlib errors on test134
  # test134
  test144
  $snd_error_hook.reset_hook!
  $mus_error_hook.reset_hook!
  (sounds or []).each do |snd| close_sound(snd) end
end
  
formats = [
  [Mus_bshort, 2],
  [Mus_lshort, 2],
  [Mus_mulaw, 1],
  [Mus_alaw, 1],
  [Mus_byte, 1],
  [Mus_ubyte, 1],
  [Mus_bfloat, 4],
  [Mus_lfloat, 4],
  [Mus_bint, 4],
  [Mus_lint, 4],
  [Mus_bintn, 4],
  [Mus_lintn, 4],
  [Mus_b24int, 3],
  [Mus_l24int, 3],
  [Mus_bdouble, 8],
  [Mus_ldouble, 8],
  [Mus_ubshort, 2],
  [Mus_ulshort, 2],
  [Mus_bdouble_unscaled, 8],
  [Mus_ldouble_unscaled, 8],
  [Mus_bfloat_unscaled, 4],
  [Mus_lfloat_unscaled, 4]]

comments = [
  ["nasahal8.wav", "ICRD: 1997-02-22\nIENG: Paul R. Roger\nISFT: Sound Forge 4.0\n"],
  ["8svx-8.snd",  "File created by Sound Exchange  "],
  ["sun-16-afsp.snd", "AFspdate:1981/02/11 23:03:34 UTC"],
  ["smp-16.snd", "Converted using Sox.                                        "],
  ["d40130.au", "1994 Jesus Villena"],
  ["wood.maud", "file written by SOX MAUD-export "],
  ["addf8.sf_mipseb", "date=\"Feb 11 18:03:34 1981\" info=\"Original recorded at 20 kHz, 15-bit D/A, digitally filtered and resampled\" speaker=\"AMK female\" text=\"Add the sum to the product of these three.\" "],
  ["mary-sun4.sig", "MARY HAD A LITTLE LAMB\n"],
  ["nasahal.pat", "This patch saved with Sound Forge 3.0."],
  ["next-16.snd", ";Written on Mon 1-Jul-91 at 12:10 PDT  at localhost (NeXT) using Allegro CL and clm of 25-June-91"],
  ["wood16.nsp", "Created by Snack   "],
  ["wood.sdx", "1994 Jesus Villena"],
  ["clmcom.aif", "this is a comment"],
  ["anno.aif", "1994 Jesus Villena\n"],
  ["telephone.wav", "sample_byte_format -s2 01\nchannel_count -i 1\nsample_count -i 36461\nsample_rate -i 16000\nsample_n_bytes -i 2\nsample_sig_bits -i 16\n"]]

fields = [Mus_audio_amp,
  Mus_audio_srate,
  Mus_audio_channel,
  Mus_audio_format,
  Mus_audio_port,
  Mus_audio_imix,
  Mus_audio_igain,
  Mus_audio_reclev,
  Mus_audio_pcm,
  Mus_audio_pcm2,
  Mus_audio_ogain,
  Mus_audio_line,
  Mus_audio_line1,
  Mus_audio_line2,
  Mus_audio_line3,
  Mus_audio_cd,
  Mus_audio_synth,
  Mus_audio_bass,
  Mus_audio_treble,
  Mus_audio_direction,
  Mus_audio_samples_per_channel]

devices = [Mus_audio_default,
  Mus_audio_duplex_default,
  Mus_audio_line_out,
  Mus_audio_line_in,
  Mus_audio_microphone,
  Mus_audio_speakers,
  Mus_audio_dac_out,
  Mus_audio_adat_in,
  Mus_audio_aes_in,
  Mus_audio_digital_in,
  Mus_audio_digital_out,
  Mus_audio_adat_out,
  Mus_audio_aes_out,
  Mus_audio_dac_filter,
  Mus_audio_mixer,
  Mus_audio_line1,
  Mus_audio_line2,
  Mus_audio_line3,
  Mus_audio_aux_input,
  Mus_audio_cd,
  Mus_audio_aux_output,
  Mus_audio_spdif_in,
  Mus_audio_spdif_out]

if $test04 and $full_test or $snd_test == 4
  $before_test_hook.call(4)
  test04(formats, comments, fields, devices)
  $after_test_hook.call(4)
end

# ---------------- test 05: simple overall checks ----------------

if $test05 and $full_test or $snd_test == 5
  $before_test_hook.call(5)
  $after_test_hook.call(5)
end

# ---------------- test all done

$overall_start_time.stop
set_previous_files_sort(0)
delete_file("saved-snd.rb")
clear_sincs
stop_playing
regions.each do |n| forget_region(n) end if regions
tracks.each do |n| free_track(n) end if tracks
snd_display("all done!")
$timings.each do |tst| snd_display("test %2d %s", tst.first, tst.last.inspect) end
snd_display("total   %s\n", $overall_start_time.inspect)
save_listener("test.output")
mus_audio_playback_amp($orig_audio_amp)

exit if $with_exit

# snd-test.rb ends here
