# snd_test.rb: Snd Ruby code and tests -*- snd-ruby -*-
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
include Dsp
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

$with_backtrace = false      unless defined? $with_backtrace
$sf_dir = "/home/bil/sf1/"   unless defined? $sf_dir
$snd_test = -1               unless defined? $snd_test
$with_exit = ($snd_test < 0) unless defined? $with_exit
$full_test = ($snd_test < 0) unless defined? $full_test
$with_big_file = false       unless defined? $with_big_file
$bigger_snd = "/home/bil/zap/sounds/bigger.snd" unless defined? $bigger_snd

$all_args = false unless defined? $all_args

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

# for use with $snd_error_hook, $mus_error_hook, snd_display etc.
def snd_backtrace
  bt = ($@ ? $@ : caller).join("\n# ")
  str = $! ? format("# %s%s\n", $!, bt) : format("# %s\n", bt)
  $stderr.print(str)
  nil
end

if provided? "snd-nogui"
  def snd_info(*args)
    clm_print("# %s\n", format(*args))
    nil
  end
else
  def snd_info(*args)
    str = format(*args)
    clm_print("\n# %s", str)
    $stderr.printf("# %s\n", str)
    nil
  end
end

if $with_backtrace
  def snd_display(*args)
    snd_info(*args)
    snd_backtrace
  end
else
  alias snd_display snd_info
end

# $snd_error_hook and $mus_error_hook
def snd_display_error(*args)
  snd_info(*args)
  snd_backtrace
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

# compares Arrays and Vcts
def vct_equal(val0, val1, err = 0.001)
  v0 = if val0.kind_of?(Array)
           list2vct(val0)
       elsif vct?(val0)
         val0
       else
         nil
       end
  v1 = if val1.kind_of?(Array)
           list2vct(val1)
       elsif vct?(val1)
         val1
       else
         nil
       end
  v0 and v1 and (vct_peak(vct_subtract!(vct_copy(v0), v1)) <= err)
end

def vequal(v0, v1)
  vct_equal(v0, v1, 0.001)
end

def vvequal(v0, v1)
  vct_equal(v0, v1, 0.00002)
end

def vfequal(v0, v1)
  vct_equal(v0, v1, 0.01)
end

def vffequal(v0, v1)
  vct_equal(v0, v1, 0.1)
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

def file_copy(f1, f2)
  # older ruby versions haven't fileutils.rb with FileUtils#copy
  File.exists?(f1) and system("cp #{f1} #{f2}")
end

def delete_file(file)
  File.unlink(file) if File.exists?(file)
end

def delete_files(*files)
  files.each do |f| delete_file(f) end
end

def with_file(file, &body)
  if File.exists?(full_name = $sf_dir + file)
    body.call(full_name)
  else
    snd_info("%s missing?", full_name)
  end
end

def snd_error_test
  Snd_error_tags.each do |tag|
    if (res = snd_catch(tag) do snd_throw(tag, "snd-test") end.first) != tag
      snd_display("snd_catch: %s -> %s", tag.inspect, res.inspect)
    end
  end
  Snd_error_tags.each do |tag|
    if (res = snd_catch(tag) do snd_raise(tag, "snd-test") end.first) != tag
      snd_display("snd_catch: %s -> %s", tag.inspect, res.inspect)
    end
  end
end
# snd_error_test

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
    snd_info(self.inspect)
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

# map_chan* procedure
$init_channel = lambda do |y| 1.0 end

$timings = Array.new(0)

$before_test_hook = Hook.new("$before_test_hook", 1)
$after_test_hook = Hook.new("$after_test_hook", 1)

$before_test_hook.add_hook!("snd-test") do |n|
  $timings.push([n, Snd_test_time.new])
  snd_info("test %d", n)
  set_show_backtrace(false)
end

$after_test_hook.add_hook!("snd-test") do |n|
  $timings.last.last.stop
  if sounds
    snd_info("test %d: open sounds: %s",
                n, sounds.map do |snd| short_file_name(snd) end.inspect)
    sounds.each do |snd| close_sound(snd) end
  end
  snd_info("test %d done\n#", n)
end

snd_info("=== Snd version: %s ===", snd_version)
snd_info("%s\n#", Time.new.strftime("%d-%b %H:%M %Z"))
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
    snd_info("start up sounds: %s mixes: %s marks: %s regions: %s",
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
      if (res = snd_catch do snd_var(sym) end.first) != :no_such_sound
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
  snd_info("$snd_opened_sound: %d", $snd_opened_sound) if $snd_opened_sound
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
                     name,
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
  # test_headers("a1.asf", 1, 16000, 0.0, "asf", "unknown")
  # test_headers("a2.asf", 1, 8000, 0.0, "asf", "unknown")
  test_headers("a1.asf", 1, 16000, 3.736562, "asf", "unknown")
  test_headers("a2.asf", 1, 8000, 4.630625, "asf", "unknown")
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
  # test_headers("c.asf", 1, 8000, 0.0, "asf", "unknown")
  test_headers("c.asf", 1, 8000, 21.368126, "asf", "unknown")
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
  # test_headers("m.asf", 1, 8000, 0.0, "asf", "unknown")
  test_headers("m.asf", 1, 8000, 64.964622, "asf", "unknown")
  test_headers("mary-sun4.sig", 1, 8000, 4.47612476348877, "Comdisco SPW signal", "big endian double (64 bits)")
  test_headers("mocksong.wav", 1, 11025, 7.86956930160522, "RIFF", "little endian short (16 bits)")
  test_headers("mono24.wav", 1, 22050, 1.98997735977173, "RIFF", "little endian int (24 bits)")
  test_headers("msadpcm.wav", 1, 11025, 4.43501138687134, "RIFF", "unknown")
  test_headers("n8.snd", 1, 44100, 0.0367800444364548, "Sun", "signed byte (8 bits)")
  test_headers("nasahal.aif", 1, 11025, 9.89841270446777, "AIFF", "signed byte (8 bits)")
  # test_headers("nasahal.avi", 1, 11025, 0.0, "AVI", "unknown")
  test_headers("nasahal.avi", 1, 11025, 10.432744, "AVI", "little endian short (16 bits)")
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
  dismiss_all_dialogs
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
  snd_display("oboe: mus_sound_maxamp %f?", mal[1])     if fneq(mal[1], 0.14724)
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
  ind = open_sound("oboe.snd")
  lfname = "test" + "-test" * 32 + ".snd"
  snd_display("variable_graph thinks anyting is a graph...") if variable_graph?(ind)
  snd_display("player? thinks anything is a player...") if player?(ind)
  snd_display("%s is not a sound?", ind.inspect) unless sound?(ind)
  save_sound_as(lfname, ind)
  close_sound(ind)
  ind = open_sound(lfname)
  snd_display("can't find test...snd") unless sound?(ind)
  if (not file_name(ind).length >= lfname.length) or
      (not short_file_name(ind).length >= lfname.length)
    snd_display("file_name lengths: %d %d %d?",
                file_name(ind).length, short_file_name(ind).length, lfname.length)
  end
  close_sound(ind)
  mus_sound_forget(lfname)
  delete_file(lfname)
end

def test024
  with_file("forest.aiff") do |fsnd|
    file_copy(fsnd, "fmv.snd")
    ind = open_sound("fmv.snd")
    if sound_loop_info(ind) != mus_sound_loop_info(fsnd)
      snd_display("loop_info: %s %s?", sound_loop_info(ind), mus_sound_loop_info(fsnd))
    end
    save_sound_as("fmv1.snd", ind, Mus_aifc)
    close_sound(ind)
    if mus_sound_loop_info("fmv1.snd") != [24981, 144332, 0, 0, 60, 0, 1, 0]
      snd_display("saved loop_info: %s?", mus_sound_loop_info("fmv1.snd").inspect)
    end
  end
  ind = open_sound("oboe.snd")
  save_sound_as("fmv.snd", ind, Mus_aifc)
  close_sound(ind)
  ind = open_sound("fmv.snd")
  if sound_loop_info(ind) != nil
    snd_display("null loop_info: %s?", sound_loop_info(ind).inspect)
  end
  set_sound_loop_info(ind, [1200, 1400, 4, 3, 2, 1])
  if sound_loop_info(ind) != [1200, 1400, 4, 3, 2, 1, 1, 1]
    snd_display("set null loop_info: %s?", sound_loop_info(ind).inspect)
  end
  save_sound_as("fmv1.snd", :sound, ind, "header-type".intern, Mus_aifc)
  close_sound(ind)
  if mus_sound_loop_info("fmv1.snd") != [1200, 1400, 4, 3, 2, 1, 1, 1]
    snd_display("saved null loop_info: %s?", mus_sound_loop_info("fmv1.snd").inspect)
  end
  ind = open_sound("fmv.snd")
  set_sound_loop_info(ind, [1200, 1400, 4, 3, 2, 1, 1, 0])
  if sound_loop_info(ind) != [1200, 1400, 0, 0, 2, 1, 1, 0]
    snd_display("null set_sound_loop_info (no mode1): %s?", sound_loop_info(ind).inspect)
  end
  save_sound_as("fmv1.snd", ind, Mus_aifc)
  close_sound(ind)
  if mus_sound_loop_info("fmv1.snd") != [1200, 1400, 0, 0, 2, 1, 1, 0]
    snd_display("saved null loop_info (no mode1): %s?", mus_sound_loop_info("fmv1.snd").inspect)
  end
  delete_files("fmv.snd", "fmv1.snd")
end

def test034(func, lst)
  nequal_fnc = if func == :mus_sound_maxamp
                lambda do |a, b| !vequal(a, b) end
              else
                lambda do |a, b| a != b end
              end
  lst.each do |f, val|
    with_file(f) do |fsnd|
      if nequal_fnc.call(res = send(func, fsnd), val)
        snd_display("%s %s => %s != %s?", func, fsnd.inspect, res.inspect, val.inspect)
      end
    end
  end
end

def test044
  oboe_snd = "oboe.snd"
  if (res = snd_catch do set_mus_sound_maxamp(oboe_snd, [1234]) end.first) != :wrong_type_arg
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
        snd_display("%s trouble[%d]: %s",
                    fsnd,
                    chn,
                    vals.map_with_index do |val, i| sample(val[0], ind, i) end.inspect)
      end
      close_sound(ind)
    end
  end
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
      if (res = snd_catch do
            [mus_sound_chans(fsnd), mus_sound_srate(fsnd), mus_sound_frames(fsnd)]
          end.first) != vals
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
  if (res = snd_catch do set_selected_channel(1) end.first) != :no_such_channel
    snd_display("set_selected_channel bad chan: %s?", res)
  end
  if (res = snd_catch do set_selected_channel(123456, 1) end.first) != :no_such_sound
    snd_display("set_selected_channel bad snd: %s?", res)
  end
  [[2, 1000], [-1, 1000], [0, -1], [0, 10000000]].each do |chn, frm|
    if (res = snd_catch do sound_data_ref(sd, chn, frm) end.first) != :out_of_range
      snd_display("sound_data_ref bad chan or frame: %d %d %s?", chn, frm, res.inspect)
    end
  end
  [[2, 1000], [-1, 1000], [0, -1], [0, 10000000]].each do |chn, frm|
    if (res = snd_catch do sound_data_set!(sd, chn, frm, 1) end.first) != :out_of_range
      snd_display("sound_data_set! bad chan or frame: %d %d %s?", chn, frm, res.inspect)
    end
  end
  v = make_vct(3)
  if (res = snd_catch do vct2sound_data(v, sd, 2) end.first) != :out_of_range
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
  if (res = snd_catch do sound_data2vct(sdata, 2, v0) end.first) != :out_of_range
    snd_display("sound_data2vct bad chan: %s?", res.inspect)
  end
  if (res = snd_catch do mus_audio_write(1, make_sound_data(3, 3), 123) end.first) != :out_of_range
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
      end.first) != :out_of_range
    snd_display("mus_sound_open_output bad chans: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_open_output("fmv.snd", 22050, 1, -1, Mus_aiff, "no comment")
      end.first) != :out_of_range
    snd_display("mus_sound_open_output bad format: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_open_output("fmv.snd", 22050, 1, Mus_bshort, -1, "no comment")
      end.first) != :out_of_range
    snd_display("mus_sound_open_output bad type: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", -1, Mus_bshort, Mus_aiff, false)
      end.first) != :out_of_range
    snd_display("mus_sound_reopen_output bad chans: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", 1, -1, Mus_aiff, false)
      end.first) != :out_of_range
    snd_display("mus_sound_reopen_output bad format: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", 1, Mus_bshort, -1, false)
      end.first) != :out_of_range
    snd_display("mus_sound_reopen_output bad type: %s?", res)
  end
  delete_file("fmv.snd")
  [:mus_audio_open_output, :mus_audio_open_input].each do |sym|
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, -1, Mus_lshort, 512)
        end.first) != :out_of_range
      snd_display("%s bad chans: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, 1, -1, 512)
        end.first) != :out_of_range
      snd_display("%s bad format: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, -1, 22050, 1, Mus_lshort, 512)
        end.first) != :out_of_range
      snd_display("%s bad device: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, -22050, 1, Mus_lshort, 512)
        end.first) != :out_of_range
      snd_display("%s bad srate: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, 1, Mus_lshort, -512)
        end.first) != :out_of_range
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
      # open_sound returns index or false; only if file doesn't exists
      # it returns :no_such_file
      if (res = snd_catch(:all) do
            snd_raise(:mus_error) unless open_sound(fsnd)
          end.first) != :mus_error
        snd_display("open_sound %s: %s", file, res.inspect)
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
    if (res = snd_catch do send(sym, -1, Mus_audio_amp, 0, vals) end.first) != :out_of_range
      snd_display("%s bad device: %s?", sym, res)
    end
    if (res = snd_catch do send(sym, Mus_audio_microphone, -1, 0, vals) end.first) != :out_of_range
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
  if fneq(res1 = maxamp(ind1, 0), res2 = 10.0 * maxamp(ind, 0))
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
  if (res = scan_channel(lambda do |y| y < 0.0 end)).kind_of?(Array)
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
  # INFO: mus_sound_forget added [MS]
  mus_sound_forget(oboe_snd)
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
    if fneq(res = mus_sound_duration($bigger_snd), 71999.1015)
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
      if mix?(mx = mix_sound("oboe.snd", 44100 * 60000))
        if (res = mix_position(mx)) != 44100 * 60000
          snd_display("bigger mix at: %d?", res)
        end
        set_mix_position(mx, 44100 * 61000)
        if (res = mix_position(mx)) != 44100 * 61000
          snd_display("bigger mix to: %d?", res)
        end
        undo(2)
      else
        snd_display("no mix tag from mix_sound: %s?", mx.inspect)
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
      let(mix_sound("oboe.snd", 44123 * 51234)) do |mx|
        mxd = find_mix(44123 * 51234)
        unless mxd.kind_of?(Numeric) or mix?(mx) or mxd == mx
          snd_display("bigger find_mix: %s %s", mxd.inspect, mx.inspect)
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
  map_channel($init_channel)
  env_channel([0.0, 0.0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4,
                0.5, 0.5, 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9])
  unless vequal((res = channel2vct), vct(0.000, 0.100, 0.200, 0.300, 0.400,
                                         0.500, 0.600, 0.700, 0.800, 0.900))
    snd_display("ramp env by 0.1: %s", res.inspect)
  end
  close_sound(ind)
  delete_file("tmp.snd")
end

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
  magic_words.each_with_index do |magic, i|
    delete_file("test.snd")
    mus_sound_forget("test.snd")
    File.open("test.snd", "w") do |fp|
      fp.write magic
      128.times do fp.write(mus_random(1.0)) end
    end
    res = snd_catch do
      snd_raise(:mus_error) unless open_sound("test.snd")
    end.first
    if res.kind_of?(Numeric) and sound?(res)
      snd_display("open_sound garbage %s: %s?", magic, res)
      close_sound(res)
    end
    delete_file("test.snd")
    mus_sound_forget("test.snd")
    File.open("test.snd", "w") do |fp|
      fp.write magic
      128.times do fp.write(mus_random(128)) end
    end
    res = snd_catch do
      snd_raise(:mus_error) unless open_sound("test.snd")
    end.first
    if res.kind_of?(Numeric) and sound?(res)
      snd_display("open_sound plausible garbage %s: %s?", magic, res)
      close_sound(res)
    end
    delete_file("test.snd")
    mus_sound_forget("test.snd")
    File.open("test.snd", "w") do |fp|
      fp.write magic
      (1...12).each do |i|
        if (ctr + i) < len
          fp.write(magic_words[ctr + i])
        else
          fp.write(magic_words[i])
        end
      end
    end
    res = snd_catch do
      snd_raise(:mus_error) unless open_sound("test.snd")
    end.first
    if res.kind_of?(Numeric) and sound?(res)
      snd_display("open_sound very plausible garbage %s: %s?", magic, res)
      close_sound(res)
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
  res = snd_catch do open_sound("test.snd") end.first
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
  res = snd_catch do open_sound("test.snd") end.first
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
  res = snd_catch do open_sound("test.aif") end.first
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound aifc no ssnd chunk %d: %d?", data_location(res), res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  make_aifc_file(002, 000, 020)
  res = snd_catch do open_sound("test.aif") end.first
  if res.kind_of?(Numeric) and sound?(res)
    snd_display("open_sound aifc 0-len auth chunk %d: %d?", data_location(res), res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  make_aifc_file(002, 150, 120)
  res = snd_catch do open_sound("test.aif") end.first
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
  res = snd_catch do open_sound("test.aif") end.first
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
  res = snd_catch do open_sound("test.aif") end.first
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
  $snd_error_hook.reset_hook!
  $mus_error_hook.reset_hook!
  $snd_error_hook.add_hook!("snd-test") do |msg|
    # if msg.empty?
    #   snd_display_error("<SND-INTERRUPT>")
    # else
    #   snd_display_error("<SND-ERROR: %s>", msg)
    # end
    true
  end
  $mus_error_hook.add_hook!("snd-test") do |type, msg|
    # deals with sndlib's :mus_error
    true
  end
  clear_listener
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
  test034(:mus_sound_maxamp, [["4.aiff", [12345, 0.5, 54321, 0.2, 0, 0.1, 9999, 0.01]]])
  test044
  test054
  test064(fields, devices)
  test074
  test084
  test094
  test104
  test114 if $with_big_file and RUBY_VERSION >= "1.8.0"
  test124
  test134
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

def append_sound(filename)
  insert_sound(filename, frames())
end

def test_edpos(ind1, func_sym, func_body = nil, &change_thunk)
  if func_body.kind_of?(Proc)
    fr1 = func_body.call(ind1, 0, false)
    fr2 = func_body.call(ind1, 0, 0)
    fr3 = func_body.call(ind1, 0, Current_edit_position)
    fr4 = func_body.call(ind1, 0, lambda do |snd, chn| 0 end)
    unless fr1 == fr2 and fr1 == fr3 and fr1 == fr4
      snd_display("initial %s: %s %s %s %s?", func_sym, fr1, fr2, fr3, fr4)
    end
    change_thunk.call
    fr5 = func_body.call(ind1, 0, false)
    fr6 = func_body.call(ind1, 0, 1)
    fr7 = func_body.call(ind1, 0, Current_edit_position)
    fr8 = func_body.call(ind1, 0, lambda do |snd, chn| edit_position(snd, chn) end)
    unless fr5 == fr6 and fr5 == fr7 and fr5 == fr8
      snd_display("%s (edpos 1): %s %s %s %s?", func_sym, fr5, fr6, fr7, fr8)
    end
    fr5 = func_body.call(ind1, 0, 0)
    fr6 = func_body.call(ind1, 0, lambda do |snd, chn| 0 end)
    unless fr1 == fr5 and fr1 == fr6
      snd_display("%s (edpos -1): %s %s %s?", func_sym, fr1, fr5, fr6)
    end
  else
    fr1 = send(func_sym, ind1, 0, false)
    fr2 = send(func_sym, ind1, 0, 0)
    fr3 = send(func_sym, ind1, 0, Current_edit_position)
    fr4 = send(func_sym, ind1, 0, lambda do |snd, chn| 0 end)
    unless fr1 == fr2 and fr1 == fr3 and fr1 == fr4
      snd_display("initial %s: %s %s %s %s?", func_sym, fr1, fr2, fr3, fr4)
    end
    change_thunk.call
    fr5 = send(func_sym, ind1, 0, false)
    fr6 = send(func_sym, ind1, 0, 1)
    fr7 = send(func_sym, ind1, 0, Current_edit_position)
    fr8 = send(func_sym, ind1, 0, lambda do |snd, chn| edit_position(snd, chn) end)
    unless fr5 == fr6 and fr5 == fr7 and fr5 == fr8
      snd_display("%s (edpos 1): %s %s %s %s?", func_sym, fr5, fr6, fr7, fr8)
    end
    fr5 = send(func_sym, ind1, 0, 0)
    fr6 = send(func_sym, ind1, 0, lambda do |snd, chn| 0 end)
    unless fr1 == fr5 and fr1 == fr6
      snd_display("%s (edpos -1): %s %s %s?", func_sym, fr1, fr5, fr6)
    end
  end
  revert_sound(ind1)
end

def test_edpos_1(func_sym, ind1, &body)
  v0 = samples2vct(12000, 10, ind1, 0)
  body.call(ind1, 0)
  v1 = samples2vct(12000, 10, ind1, 0)
  snd_display("%s (0) no change!\n# %s\n# %s", func_sym, v0.inspect, v1.inspect) if vequal(v0, v1)
  body.call(ind1, 0)
  v2 = samples2vct(12000, 10, ind1, 0)
  snd_display("%s (1)\n# %s\n# %s", func_sym, v1.inspect, v2.inspect) unless vequal(v1, v2)
  body.call(ind1, lambda do |snd, chn| 0 end)
  v2 = samples2vct(12000, 10, ind1, 0)
  snd_display("%s (2)\n# %s\n# %s", func_sym, v1.inspect, v2.inspect) unless vequal(v1, v2)
  revert_sound(ind1)
end

def test_orig(func0, func1, func_name, ind1)
  v0 = samples2vct(12000, 10, ind1, 0)
  func0.call(ind1)
  v1 = samples2vct(12000, 10, ind1, 0)
  if vfequal(v0, v1)
    snd_display("%s (orig: 0) no change!\n# %s\n# %s", func_name, v0.inspect, v1.inspect)
  end
  func1.call(ind1)
  v2 = samples2vct(12000, 10, ind1, 0)
  # INFO vfequal --> vffequal
  unless vffequal(v0, v2)
    snd_display("%s (orig: 1)\n# %s\n# %s", func_name, v0.inspect, v2.inspect)
  end
  revert_sound(ind1)
end

def old_map_channel(beg = false, dur = false, snd = false, chn = false, edpos = false, &body)
  map_channel(lambda do |y|
                val = body.call(y)
                if val.kind_of?(Array)
                  list2vct(val)
                else
                  val
                end
              end,
              beg, dur, snd, chn, edpos, edpos)
end

def make_bandpass_2(flo1, fhi1, flo2, fhi2, len = 30)
  f1 = make_bandpass(flo1, fhi1, len)
  f2 = make_bandpass(flo2, fhi2, len)
  vct_add!(mus_xcoeffs(f1), mus_xcoeffs(f2))
  f1
end

def bandpass_2(f, input)
  fir_filter(f, input)
end

def check_maxamp(ind, val, name)
  if fneq(maxamp(ind, 0), val)
    snd_display("maxamp amp_env %s: %s should be %s", name, maxamp(ind), val)
  end
  pos = find(lambda do |y| y.abs >= (val - 0.001) end)
  snd_display("actual maxamp %s vals not right", name) unless pos
  mx = 0.0
  scan_chan(lambda do |y|
              if y.abs > mx
                mx = y.abs
              end
              false
            end)
  snd_display("actual %s max: %s (correct: %s)", name, mx, val) if fneq(mx, val)
end

def check_env_vals(name, gen)
  ctr = -1
  scan_chan(lambda do |y|
              if fneq(val = env(gen), y)
                snd_display("check_env_vals %s at %d: %f %f", name, ctr, val, y)
                true
              else
                false
              end
            end)
end

def our_x2position(ind, x)
  ax = axis_info(ind, 0)
  [ax[10].to_f + ((x - ax[2]) * (ax[12].to_f - ax[10])) / (ax[4] - ax[2]), x2position(x, ind)]
end

def region2vct_1(reg, chn, len)
  region2vct(0, len, reg, chn)
end

def region_to_vct(reg, chn, len)
  rs = make_region_sample_reader(0, reg, chn)
  make_vct!(len) do next_sample(rs) end
end

# basic edit tree cases
def test005
  snd_display("dac is running?") if dac_is_running
  ind = new_sound("test.snd")
  str = format("
EDITS: 0

 (begin) [0:2]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, end_mark)
", Dir.pwd)
  if (res = display_edits) != str
    snd_display("new 0: %s %s?", str, res)
  end
  insert_samples(10, 10, make_vct(10))
  str = format("
EDITS: 2

 (begin) [0:2]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, end_mark)

 (silence 1 9) ; pad-channel [1:3]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, cp->sounds[-1][0:8, 0.000])
   (at 10, end_mark)

 (insert 10 10) ; insert-samples [2:4]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, cp->sounds[-1][0:8, 0.000])
   (at 10, cp->sounds[1][0:9, 1.000]) [buf: 10] 
   (at 20, end_mark)
", Dir.pwd, Dir.pwd, Dir.pwd)
  if (res = display_edits) != str
    snd_display("new 1: %s %s?", str, res)
  end
  undo
  insert_samples(0, 10, make_vct(10))
  str = format("
EDITS: 2

 (begin) [0:2]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, end_mark)

 (silence 1 9) ; pad-channel [1:3]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, cp->sounds[-1][0:8, 0.000])
   (at 10, end_mark)

 (insert 0 10) ; insert-samples [2:4]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 10] 
   (at 10, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 11, cp->sounds[-1][0:8, 0.000])
   (at 20, end_mark)
", Dir.pwd, Dir.pwd, Dir.pwd)
  if (res = display_edits) != str
    snd_display("new 2: %s %s?", str, res)
  end
  undo(2)
  insert_samples(0, 10, make_vct(10))
  str = format("
EDITS: 1

 (begin) [0:2]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, end_mark)

 (insert 0 10) ; insert-samples [1:3]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 10] 
   (at 10, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 11, end_mark)
", Dir.pwd, Dir.pwd)
  if (res = display_edits) != str
    snd_display("new 3: %s %s?", str, res)
  end
  undo
  set_sample(0, 0.5)
  str = format("
EDITS: 1

 (begin) [0:2]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, end_mark)

 (set 0 1) ; set_sample(0, 0.5000 [1:2]:
   (at 0, cp->sounds[1][0:0, 1.000]) [buf: 1] 
   (at 1, end_mark)
", Dir.pwd)
  if (res = display_edits) != str
    snd_display("new 4: %s %s?", str, res)
  end
  undo
  set_samples(0, 10, make_vct(10))
  str = format("
EDITS: 1

 (begin) [0:2]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, end_mark)

 (set 0 10) ; set-samples [1:2]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
", Dir.pwd)
  if (res = display_edits) != str
    snd_display("new 5: %s %s?", str, res)
  end
  idx = 1
  test_idx = 5
  test_output = lambda do |str|
    idx += 1
    test_idx += 1
    res = display_edits(ind, 0, idx).split(/\n/)[0..1].join("\n")
    str = str.split(/\n/)[0..1].join("\n")
    # FIXME: Edits are not in every case in a buffer but in a temp
    # file, so we look only for the first line of text.
    snd_display("new %d: %s %s?", test_idx, str, res) unless res == str
  end
  delete_samples(3, 4)
  test_output.call("
 (delete 3 4) ; delete_samples(3, 4 [2:3]:
   (at 0, cp->sounds[1][0:2, 1.000]) [buf: 10] 
   (at 3, cp->sounds[1][7:9, 1.000]) [buf: 10] 
   (at 6, end_mark)
")
  set_samples(1, 4, make_vct(4))
  test_output.call("
 (set 1 4) ; set-samples [3:4]:
   (at 0, cp->sounds[1][0:0, 1.000]) [buf: 10] 
   (at 1, cp->sounds[2][0:3, 1.000]) [buf: 4] 
   (at 5, cp->sounds[1][9:9, 1.000]) [buf: 10] 
   (at 6, end_mark)
")
  undo(2)
  insert_samples(2, 3, make_vct(3))
  insert_samples(2, 1, make_vct(1))
  insert_samples(4, 1, make_vct(1))
  insert_samples(15, 1, make_vct(1))
  str = format("
EDITS: 5

 (begin) [0:2]:
   (at 0, cp->sounds[0][0:0, 0.000]) [file: %s/test.snd[0]]
   (at 1, end_mark)

 (set 0 10) ; set-samples [1:2]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 10] 
   (at 10, end_mark)

 (insert 2 3) ; insert-samples [2:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[2][0:2, 1.000]) [buf: 3] 
   (at 5, cp->sounds[1][2:9, 1.000]) [buf: 10] 
   (at 13, end_mark)

 (insert 2 1) ; insert-samples [3:5]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[3][0:0, 1.000]) [buf: 1] 
   (at 3, cp->sounds[2][0:2, 1.000]) [buf: 3] 
   (at 6, cp->sounds[1][2:9, 1.000]) [buf: 10] 
   (at 14, end_mark)

 (insert 4 1) ; insert-samples [4:7]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[3][0:0, 1.000]) [buf: 1] 
   (at 3, cp->sounds[2][0:0, 1.000]) [buf: 3] 
   (at 4, cp->sounds[4][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[2][1:2, 1.000]) [buf: 3] 
   (at 7, cp->sounds[1][2:9, 1.000]) [buf: 10] 
   (at 15, end_mark)

 (insert 15 1) ; insert-samples [5:8]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[3][0:0, 1.000]) [buf: 1] 
   (at 3, cp->sounds[2][0:0, 1.000]) [buf: 3] 
   (at 4, cp->sounds[4][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[2][1:2, 1.000]) [buf: 3] 
   (at 7, cp->sounds[1][2:9, 1.000]) [buf: 10] 
   (at 15, cp->sounds[5][0:0, 1.000]) [buf: 1] 
   (at 16, end_mark)
", Dir.pwd)
  test_idx += 1
  idx += 2
  if (res = display_edits) != str
    snd_display("new 8: %s?", res)
  end
  delete_samples(2, 1)
  test_output.call("
 (delete 2 1) ; delete_samples(2, 1 [6:7]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[2][0:0, 1.000]) [buf: 3] 
   (at 3, cp->sounds[4][0:0, 1.000]) [buf: 1] 
   (at 4, cp->sounds[2][1:2, 1.000]) [buf: 3] 
   (at 6, cp->sounds[1][2:9, 1.000]) [buf: 10] 
   (at 14, cp->sounds[5][0:0, 1.000]) [buf: 1] 
   (at 15, end_mark)
")
  delete_samples(0, 5)
  test_output.call("
 (delete 0 5) ; delete_samples(0, 5 [7:4]:
   (at 0, cp->sounds[2][2:2, 1.000]) [buf: 3] 
   (at 1, cp->sounds[1][2:9, 1.000]) [buf: 10] 
   (at 9, cp->sounds[5][0:0, 1.000]) [buf: 1] 
   (at 10, end_mark)
")
  delete_samples(6, 4)
  test_output.call("
 (delete 6 4) ; delete_samples(6, 4 [8:3]:
   (at 0, cp->sounds[2][2:2, 1.000]) [buf: 3] 
   (at 1, cp->sounds[1][2:6, 1.000]) [buf: 10] 
   (at 6, end_mark)
")
  delete_samples(0, 1)
  test_output.call("
 (delete 0 1) ; delete_samples(0, 1 [9:2]:
   (at 0, cp->sounds[1][2:6, 1.000]) [buf: 10] 
   (at 5, end_mark)
")
  delete_samples(0, 5)
  test_output.call("
 (delete 0 5) ; delete_samples(0, 5 [10:1]:
   (at 0, end_mark)
")
  delete_samples(0, 10)
  snd_display("no-op delete deleted something! %s", display_edits) unless edit_position == 10
  insert_samples(0, 3, make_vct(3))
  test_output.call("
 (insert 0 3) ; insert-samples [11:2]:
   (at 0, cp->sounds[6][0:2, 1.000]) [buf: 3] 
   (at 3, end_mark)
")
  delete_samples(2, 1)
  test_output.call("
 (delete 2 1) ; delete_samples(2, 1 [12:2]:
   (at 0, cp->sounds[6][0:1, 1.000]) [buf: 3] 
   (at 2, end_mark)
")
  set_sample(0, 0.5)
  test_output.call("
 (set 0 1) ; set_sample(0, 0.5000 [13:3]:
   (at 0, cp->sounds[7][0:0, 1.000]) [buf: 1] 
   (at 1, cp->sounds[6][1:1, 1.000]) [buf: 3] 
   (at 2, end_mark)
")
  set_sample(1, 0.5)
  test_output.call("
 (set 1 1) ; set_sample(1, 0.5000 [14:3]:
   (at 0, cp->sounds[7][0:0, 1.000]) [buf: 1] 
   (at 1, cp->sounds[8][0:0, 1.000]) [buf: 1] 
   (at 2, end_mark)
")
  map_channel($init_channel, 0, 10)
  test_output.call("
 (set 0 10) ; map-channel [15:2]:
   (at 0, cp->sounds[9][0:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  insert_samples(0, 10, make_vct(10))
  test_output.call("
 (insert 0 10) ; insert-samples [16:3]:
   (at 0, cp->sounds[10][0:9, 1.000]) [buf: 10] 
   (at 10, cp->sounds[9][0:9, 1.000]) [buf: 10] 
   (at 20, end_mark)
")
  set_samples(2, 3, make_vct(3))
  test_output.call("
 (set 2 3) ; set-samples [17:5]:
   (at 0, cp->sounds[10][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[11][0:2, 1.000]) [buf: 3] 
   (at 5, cp->sounds[10][5:9, 1.000]) [buf: 10] 
   (at 10, cp->sounds[9][0:9, 1.000]) [buf: 10] 
   (at 20, end_mark)
")
  set_samples(0, 12, make_vct(12))
  test_output.call("
 (set 0 12) ; set-samples [18:3]:
   (at 0, cp->sounds[12][0:11, 1.000]) [buf: 12] 
   (at 12, cp->sounds[9][2:9, 1.000]) [buf: 10] 
   (at 20, end_mark)
")
  set_samples(30, 10, make_vct(10))
  test_output.call("
 (silence 20 11) ; pad-channel [19:4]:
   (at 0, cp->sounds[12][0:11, 1.000]) [buf: 12] 
   (at 12, cp->sounds[9][2:9, 1.000]) [buf: 10] 
   (at 20, cp->sounds[-1][0:10, 0.000])
   (at 31, end_mark)
")
  test_output.call("
 (set 30 10) ; set-samples [20:5]:
   (at 0, cp->sounds[12][0:11, 1.000]) [buf: 12] 
   (at 12, cp->sounds[9][2:9, 1.000]) [buf: 10] 
   (at 20, cp->sounds[-1][0:9, 0.000])
   (at 30, cp->sounds[13][0:9, 1.000]) [buf: 10] 
   (at 40, end_mark)
")
  close_sound(ind)
end

# scale/ramp
def test015
  ind = new_sound("test.snd")
  idx = -1
  test_name = "scl"
  test_output = lambda do |ed, str|
    idx += 1
    res = display_edits(ind, 0, ed).split(/\n/)[0..1].join("\n")
    str = str.split(/\n/)[0..1].join("\n")
    snd_display("%s %d: %s %s", test_name, idx, str, res) unless res == str
  end
  map_channel($init_channel, 0, 10)
  scale_channel(0.5)
  test_output.call(2, "
 (scale 0 10) ; scale_channel(0.500, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 0.500]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 0, 3)
  test_output.call(2, "
 (scale 0 3) ; scale_channel(0.500, 0, 3 [2:3]:
   (at 0, cp->sounds[1][0:2, 0.500]) [buf: 10] 
   (at 3, cp->sounds[1][3:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 5, 5)
  test_output.call(2, "
 (scale 5 5) ; scale_channel(0.500, 5, 5 [2:3]:
   (at 0, cp->sounds[1][0:4, 1.000]) [buf: 10] 
   (at 5, cp->sounds[1][5:9, 0.500]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 2, 4)
  test_output.call(2, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [2:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:5, 0.500]) [buf: 10] 
   (at 6, cp->sounds[1][6:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 10, 10)
  snd_display("scale beyond end edited? %s", display_edits) unless edit_position == 1
  scale_channel(0.5, 100, 10)
  snd_display("scale way beyond end edited? %s", display_edits) unless edit_position == 1
  scale_channel(0.5, 5, 10)
  test_output.call(2, "
 (scale 5 5) ; scale_channel(0.500, 5, 5 [2:3]:
   (at 0, cp->sounds[1][0:4, 1.000]) [buf: 10] 
   (at 5, cp->sounds[1][5:9, 0.500]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  set_sample(4, 0.5)
  test_output.call(2, "
 (set 4 1) ; set_sample(4, 0.5000 [2:4]:
   (at 0, cp->sounds[1][0:3, 1.000]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  scale_channel(0.5, 0, 4)
  test_output.call(3, "
 (scale 0 4) ; scale_channel(0.500, 0, 4 [3:4]:
   (at 0, cp->sounds[1][0:3, 0.500]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  scale_channel(0.5, 4, 1)
  test_output.call(4, "
 (scale 4 1) ; scale_channel(0.500, 4, 1 [4:4]:
   (at 0, cp->sounds[1][0:3, 0.500]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 0.500]) [buf: 1] 
   (at 5, cp->sounds[1][5:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  scale_channel(0.5, 0, 7)
  test_output.call(5, "
 (scale 0 7) ; scale_channel(0.500, 0, 7 [5:5]:
   (at 0, cp->sounds[1][0:3, 0.250]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 0.250]) [buf: 1] 
   (at 5, cp->sounds[1][5:6, 0.500]) [buf: 10] 
   (at 7, cp->sounds[1][7:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  scale_channel(0.5, 1, 4)
  test_output.call(6, "
 (scale 1 4) ; scale_channel(0.500, 1, 4 [6:6]:
   (at 0, cp->sounds[1][0:0, 0.250]) [buf: 10] 
   (at 1, cp->sounds[1][1:3, 0.125]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 0.125]) [buf: 1] 
   (at 5, cp->sounds[1][5:6, 0.500]) [buf: 10] 
   (at 7, cp->sounds[1][7:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo(4)
  scale_channel(0.5, 1, 8)
  test_output.call(3, "
 (scale 1 8) ; scale_channel(0.500, 1, 8 [3:6]:
   (at 0, cp->sounds[1][0:0, 1.000]) [buf: 10] 
   (at 1, cp->sounds[1][1:3, 0.500]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 0.500]) [buf: 1] 
   (at 5, cp->sounds[1][5:8, 0.500]) [buf: 10] 
   (at 9, cp->sounds[1][9:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo(2)
  idx = -1
  test_name = "ramp"
  ramp_channel(0.0, 1.0)
  test_output.call(2, "
 (ramp 0 10) ; ramp_channel(0.000, 1.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [1]-0.000 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  scale_channel(0.5)
  test_output.call(3, "
 (scale 0 10) ; scale_channel(0.500, 0, false [3:2]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]-0.000 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 0, 5)
  test_output.call(3, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [3:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]-0.000 -> 0.444]) [buf: 10] 
   (at 5, cp->sounds[1][5:9, 1.000, [1]0.556 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 2, 4)
  test_output.call(3, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.111]) [buf: 10] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.222 -> 0.556]) [buf: 10] 
   (at 6, cp->sounds[1][6:9, 1.000, [1]0.667 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 5, 5)
  test_output.call(3, "
 (scale 5 5) ; scale_channel(0.500, 5, 5 [3:3]:
   (at 0, cp->sounds[1][0:4, 1.000, [1]-0.000 -> 0.444]) [buf: 10] 
   (at 5, cp->sounds[1][5:9, 0.500, [1]0.556 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo(2)
  ramp_channel(0.2, 0.6, 2, 6)
  test_output.call(2, "
 (ramp 2 6) ; ramp_channel(0.200, 0.600, 2, 6 [2:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.600]) [buf: 10] 
   (at 8, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(3, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [3:5]:
   (at 0, cp->sounds[1][0:1, 0.500]) [buf: 10] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.360]) [buf: 10] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.440 -> 0.600]) [buf: 10] 
   (at 8, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 2, 6)
  test_output.call(3, "
 (scale 2 6) ; scale_channel(0.500, 2, 6 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:7, 0.500, [1]0.200 -> 0.600]) [buf: 10] 
   (at 8, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 5, 4)
  test_output.call(3, "
 (scale 5 4) ; scale_channel(0.500, 5, 4 [3:6]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:4, 1.000, [1]0.200 -> 0.360]) [buf: 10] 
   (at 5, cp->sounds[1][5:7, 0.500, [1]0.440 -> 0.600]) [buf: 10] 
   (at 8, cp->sounds[1][8:8, 0.500]) [buf: 10] 
   (at 9, cp->sounds[1][9:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  set_sample(4, 0.5)
  test_output.call(3, "
 (set 4 1) ; set_sample(4, 0.5000 [3:6]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.440 -> 0.600]) [buf: 10] 
   (at 8, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5, 4, 1)
  test_output.call(3, "
 (scale 4 1) ; scale_channel(0.500, 4, 1 [3:6]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][4:4, 0.500, [1]0.360 -> 0.360]) [buf: 10] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.440 -> 0.600]) [buf: 10] 
   (at 8, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  delete_sample(4)
  test_output.call(3, "
 (delete 4 1) ; delete_samples(4, 1 [3:5]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][5:7, 1.000, [1]0.440 -> 0.600]) [buf: 10] 
   (at 7, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 9, end_mark)
")
  undo
  delete_samples(4, 2)
  test_output.call(3, "
 (delete 4 2) ; delete_samples(4, 2 [3:5]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][6:7, 1.000, [1]0.520 -> 0.600]) [buf: 10] 
   (at 6, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 8, end_mark)
")
  undo
  delete_samples(4, 3)
  test_output.call(3, "
 (delete 4 3) ; delete_samples(4, 3 [3:5]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][7:7, 1.000, [1]0.600 -> 0.600]) [buf: 10] 
   (at 5, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 7, end_mark)
")
  undo
  delete_samples(4, 4)
  test_output.call(3, "
 (delete 4 4) ; delete_samples(4, 4 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 6, end_mark)
")
  undo
  delete_samples(4, 5)
  test_output.call(3, "
 (delete 4 5) ; delete_samples(4, 5 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][9:9, 1.000]) [buf: 10] 
   (at 5, end_mark)
")
  undo
  scale_channel(0.5, 4, 2)
  test_output.call(3, "
 (scale 4 2) ; scale_channel(0.500, 4, 2 [3:6]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][4:5, 0.500, [1]0.360 -> 0.440]) [buf: 10] 
   (at 6, cp->sounds[1][6:7, 1.000, [1]0.520 -> 0.600]) [buf: 10] 
   (at 8, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  pad_channel(4, 1)
  test_output.call(3, "
 (silence 4 1) ; pad-channel [3:6]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[-1][0:0, 0.000])
   (at 5, cp->sounds[1][4:7, 1.000, [1]0.360 -> 0.600]) [buf: 10] 
   (at 9, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 11, end_mark)
")
  close_sound(ind)
end

# xramp
def test025
  ind = new_sound("test.snd")
  idx = -1
  test_name = "xramp"
  test_output = lambda do |ed, str|
    idx += 1
    res = display_edits(ind, 0, ed).split(/\n/)[0..1].join("\n")
    str = str.split(/\n/)[0..1].join("\n")
    snd_display("%s %d: %s %s", test_name, idx, str, res) unless res == str
  end
  map_channel($init_channel, 0, 10)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(2, "
 (ramp 0 10) ; xramp_channel(0.000, 1.000, 32.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  xramp_channel(0.0, 1.0, 0.325)
  test_output.call(2, "
 (ramp 0 10) ; xramp_channel(0.000, 1.000, 0.325, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]-0.000 -> -1.124, off: 1.481, scl: -1.481]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  xramp_channel(0.0, 1.0, 0.0)
  test_output.call(2, format("
 (scale 0 10) ; scale_channel(0.000, 0, false [2:2]:
   (at 0, cp->sounds[0][0:9, 0.000]) [file: %s/test.snd[0]]
   (at 10, end_mark)
", Dir.pwd))
  undo
  xramp_channel(0.0, 1.0, 1.0)
  test_output.call(2, "
 (ramp 0 10) ; ramp_channel(0.000, 1.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [1]-0.000 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  xramp_channel(0.5, 1.5, 32.0)
  test_output.call(2, "
 (ramp 0 10) ; xramp_channel(0.500, 1.500, 32.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]0.000 -> 3.466, off: 0.468, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  if fneq(maxamp, 1.5) or fneq(sample(0), 0.5)
    snd_display("xramp 5 vals: %f %f", maxamp, sample(0))
  end
  undo
  xramp_channel(-0.5, 1.5, 32.0)
  test_output.call(2, "
 (ramp 0 10) ; xramp_channel(-0.500, 1.500, 32.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]0.000 -> 3.466, off: -0.565, scl: 0.065]) [buf: 10] 
   (at 10, end_mark)
")
  if fneq(maxamp, 1.5) or fneq(sample(0), -0.5)
    snd_display("xramp 6 vals: %f %f", maxamp, sample(0))
  end
  undo
  xramp_channel(0.0, 1.0, 32.0)
  vals = channel2vct
  scale_channel(0.5)
  test_output.call(3, "
 (scale 0 10) ; scale_channel(0.500, 0, false [3:2]:
   (at 0, cp->sounds[1][0:9, 0.500, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  ctr = 0
  if res = scan_chan(lambda do |y|
                       if fneq(y, 0.5 * vals[ctr])
                         true
                       else
                         ctr += 1
                         false
                       end
               end)
    snd_display("trouble in xramp 7: %s", res.inspect)
  end
  undo
  delete_sample(0)
  test_output.call(3, "
 (delete 0 1) ; delete_samples(0, 1 [3:2]:
   (at 0, cp->sounds[1][1:9, 1.000, [4]0.385 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 9, end_mark)
")
  ctr = 1
  if res = scan_chan(lambda do |y|
                       if fneq(y, vals[ctr])
                         true
                       else
                         ctr += 1
                         false
                       end
               end)
    snd_display("trouble in xramp 8: %s", res.inspect)
  end
  undo
  delete_samples(0, 2)
  test_output.call(3, "
 (delete 0 2) ; delete_samples(0, 2 [3:2]:
   (at 0, cp->sounds[1][2:9, 1.000, [4]0.770 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 8, end_mark)
")
  ctr = 2
  if res = scan_chan(lambda do |y|
                       if fneq(y, vals[ctr])
                         true
                       else
                         ctr += 1
                         false
                       end
               end)
    snd_display("trouble in xramp 9: %s", res.inspect)
  end
  undo
  delete_sample(0)
  delete_sample(0)
  test_output.call(4, "
 (delete 0 1) ; delete_samples(0, 1 [4:2]:
   (at 0, cp->sounds[1][2:9, 1.000, [4]0.770 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 8, end_mark)
")
  undo(2)
  delete_sample(4)
  test_output.call(3, "
 (delete 4 1) ; delete_samples(4, 1 [3:3]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[1][5:9, 1.000, [4]1.925 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 9, end_mark)
")
  undo
  delete_samples(4, 2)
  test_output.call(3, "
 (delete 4 2) ; delete_samples(4, 2 [3:3]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[1][6:9, 1.000, [4]2.310 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 8, end_mark)
")
  undo
  scale_channel(0.5, 4, 2)
  test_output.call(3, "
 (scale 4 2) ; scale_channel(0.500, 4, 2 [3:4]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[1][4:5, 0.500, [4]1.540 -> 1.925, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 6, cp->sounds[1][6:9, 1.000, [4]2.310 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  ctr = 0
  if res = scan_chan(lambda do |y|
                       if (ctr > 5 and fneq(y, vals[ctr])) or
                           (ctr < 4 and fneq(y, vals[ctr])) or
                           ((ctr == 4 or ctr == 5) and fneq(y, 0.5 * vals[ctr]))
                         true
                       else
                         ctr += 1
                         false
                       end
               end)
    snd_display("trouble in xramp 13: %s", res.inspect)
  end
  undo
  scale_channel(0.5, 0, 2)
  test_output.call(3, "
 (scale 0 2) ; scale_channel(0.500, 0, 2 [3:3]:
   (at 0, cp->sounds[1][0:1, 0.500, [4]0.000 -> 0.385, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 2, cp->sounds[1][2:9, 1.000, [4]0.770 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  pad_channel(4, 2)
  test_output.call(3, "
 (silence 4 2) ; pad-channel [3:4]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[-1][0:1, 0.000])
   (at 6, cp->sounds[1][4:9, 1.000, [4]1.540 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 12, end_mark)
")
  undo
  set_sample(4, 1.0)
  test_output.call(3, "
 (set 4 1) ; set_sample(4, 1.0000 [3:4]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:9, 1.000, [4]1.925 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  set_samples(4, 2, make_vct(2))
  test_output.call(3, "
 (set 4 2) ; set-samples [3:4]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[2][0:1, 1.000]) [buf: 2] 
   (at 6, cp->sounds[1][6:9, 1.000, [4]2.310 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  undo
  scale_channel(0.5)
  set_samples(4, 2, make_vct(2))
  test_output.call(4, "
 (set 4 2) ; set-samples [4:4]:
   (at 0, cp->sounds[1][0:3, 0.500, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[2][0:1, 1.000]) [buf: 2] 
   (at 6, cp->sounds[1][6:9, 0.500, [4]2.310 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  close_sound(ind)
  ind = new_sound("test.snd")
  test_name = "multi-ramp"
  idx = 0
  map_channel($init_channel, 0, 100)
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  ramp_channel(0.0, 1.0)
  test_output.call(12, "
 (ramp 0 100) ; ramp_channel(0.000, 1.000, 0, false [12:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 0.091]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.101 -> 0.192]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.202 -> 0.293]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.495]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.505 -> 0.596]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.707 -> 0.798]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.808 -> 0.899]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000]) [buf: 100] 
   (at 100, end_mark)
")
  snd_display("multi-ramp 1 maxamp: %f", maxamp) if fneq(maxamp, 0.5)
  undo
  ramp_channel(0.1, 1.0, 10, 90)
  test_output.call(12, "
 (ramp 10 90) ; ramp_channel(0.100, 1.000, 10, 90 [12:11]:
   (at 0, cp->sounds[1][0:9, 0.500]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.100 -> 0.191]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.201 -> 0.292]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.302 -> 0.393]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.403 -> 0.494]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.504 -> 0.596]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.707 -> 0.798]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.808 -> 0.899]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000]) [buf: 100] 
   (at 100, end_mark)
")
  snd_display("multi-ramp 2 maxamp: %f", maxamp) if fneq(maxamp, 0.5)
  undo
  ramp_channel(0.0, 0.9, 0, 90)
  test_output.call(12, "
 (ramp 0 90) ; ramp_channel(0.000, 0.900, 0, 90 [12:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 0.091]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.101 -> 0.192]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.202 -> 0.293]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.496]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.506 -> 0.597]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.607 -> 0.698]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.708 -> 0.799]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.809 -> 0.900]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500]) [buf: 100] 
   (at 100, end_mark)
")
  snd_display("multi-ramp 3 maxamp: %f", maxamp) if fneq(maxamp, 0.5)
  snd_display("multi-ramp 3 sample 89: %f", sample(89)) if fneq(sample(89), 0.45)
  snd_display("multi-ramp 3 sample 90: %f", sample(90)) if fneq(sample(90), 0.5)
  undo
  ramp_channel(0.1, 0.9, 10, 80)
  test_output.call(12, "
 (ramp 10 80) ; ramp_channel(0.100, 0.900, 10, 80 [12:11]:
   (at 0, cp->sounds[1][0:9, 0.500]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.100 -> 0.191]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.201 -> 0.292]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.495]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.505 -> 0.596]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.708 -> 0.799]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.809 -> 0.900]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500]) [buf: 100] 
   (at 100, end_mark)
")
  revert_sound
  map_channel($init_channel, 0, 100)
  ramp_channel(0.0, 1.0)
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  test_output.call(12, "
 (scale 90 10) ; scale_channel(0.500, 90, 10 [12:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 0.091]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.101 -> 0.192]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.202 -> 0.293]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.495]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.505 -> 0.596]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.707 -> 0.798]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.808 -> 0.899]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000]) [buf: 100] 
   (at 100, end_mark)
")
  close_sound(ind)
end

# ramp2
def test035
  ind = new_sound("test.snd")
  idx = -1
  test_name = "ramp2"
  test_output = lambda do |ed, str|
    idx += 1
    res = display_edits(ind, 0, ed).split(/\n/)[0..1].join("\n")
    str = str.split(/\n/)[0..1].join("\n")
    snd_display("%s %d: %s %s", test_name, idx, str, res) unless res == str
  end
  map_chan($init_channel, 0, 10)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.010, 0.040, 0.090, 0.160, 0.250, 0.360, 0.490, 0.640, 0.810, 1.000))
    snd_display("ramp2 (0): %s", res.inspect)
  end
  scale_channel(0.5)
  test_output.call(4, "
 (scale 0 11) ; scale_channel(0.500, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]-0.000 -> 0.400, [2]-0.000 -> 0.400]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [2]0.500 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.005, 0.020, 0.045, 0.080, 0.250, 0.360, 0.490, 0.640, 0.810, 1.000))
    snd_display("ramp2 (2): %s", res.inspect)
  end
  undo
  scale_channel(0.5, 2, 4)
  test_output.call(4, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [2]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [2]0.200 -> 0.500]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [2]0.600 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo(2)
  ramp_channel(0.75, 0.25)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.750, 0.250, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]0.750 -> 0.250]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  ramp_channel(0.2, 0.6, 2, 6)
  test_output.call(3, "
 (ramp 2 6) ; ramp_channel(0.200, 0.600, 2, 6 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.700, [2]0.200 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [1]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.400, [2]0.200 -> 0.360]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.440 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  set_sample(4, 0.5)
  test_output.call(4, "
 (set 4 1) ; set_sample(4, 0.5000 [4:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.300, [2]0.200 -> 0.280]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.440 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo(3)
  close_sound(ind)
  ind = new_sound("test.snd")
  map_channel($init_channel, 0, 100)
  test_name = "multi-ramp2"
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  ramp_channel(0.0, 1.0)
  ramp_channel(1.0, 0.0)
  test_output.call(13, "
 (ramp 0 100) ; ramp_channel(1.000, 0.000, 0, false [13:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 0.091, [2]1.000 -> 0.909]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.101 -> 0.192, [2]0.899 -> 0.808]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.202 -> 0.293, [2]0.798 -> 0.707]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394, [2]0.697 -> 0.606]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.495, [2]0.596 -> 0.505]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.505 -> 0.596, [2]0.495 -> 0.404]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697, [2]0.394 -> 0.303]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.707 -> 0.798, [2]0.293 -> 0.202]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.808 -> 0.899, [2]0.192 -> 0.101]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000, [2]0.091 -> -0.000]) [buf: 100] 
   (at 100, end_mark)
")
  undo(12)
  ramp_channel(0.0, 1.0, 10, 20)
  ramp_channel(0.0, 1.0, 50, 10)
  ramp_channel(0.0, 1.0, 25, 10)
  test_output.call(4, "
 (ramp 25 10) ; ramp_channel(0.000, 1.000, 25, 10 [4:8]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 100] 
   (at 10, cp->sounds[1][10:24, 1.000, [1]-0.000 -> 0.737]) [buf: 100] 
   (at 25, cp->sounds[1][25:29, 1.000, [1]0.789 -> 1.000, [2]-0.000 -> 0.444]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [1]0.556 -> 1.000]) [buf: 100] 
   (at 35, cp->sounds[1][35:49, 1.000]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [1]-0.000 -> 1.000]) [buf: 100] 
   (at 60, cp->sounds[1][60:99, 1.000]) [buf: 100] 
   (at 100, end_mark)
")
  close_sound(ind)
end

# ramp-xramp, xramp-ramp
def test045
  ind = new_sound("test.snd")
  idx = -1
  test_name = "ramp-xramp"
  test_output = lambda do |ed, str|
    idx += 1
    res = display_edits(ind, 0, ed).split(/\n/)[0..1].join("\n")
    str = str.split(/\n/)[0..1].join("\n")
    snd_display("%s %d: %s %s", test_name, idx, str, res) unless res == str
  end
  map_chan($init_channel, 0, 10)
  ramp_channel(0.0, 1.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case1 = channel2vct,
                vct(0.000, 0.001, 0.006, 0.018, 0.039, 0.075, 0.135, 0.233, 0.387, 0.628, 1.000))
    snd_display("ramp-xramp (1): %s", case1.inspect)
  end
  scale_channel(0.5)
  test_output.call(4, "
 (scale 0 11) ; scale_channel(0.500, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]-0.000 -> 0.400, [4]0.000 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [4]1.733 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case2 = channel2vct,
                vct(0.000, 0.001, 0.003, 0.009, 0.019, 0.075, 0.135, 0.233, 0.387, 0.628, 1.000))
    snd_display("ramp-xramp (2): %s", case2.inspect)
  end
  undo
  scale_channel(0.5, 2, 4)
  test_output.call(4, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [4]0.693 -> 1.733, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [4]2.079 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo(2)
  xramp_channel(0.75, 0.25, 32.0)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.750, 0.250, 32.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [4]3.466 -> 0.000, off: 0.234, scl: 0.016]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  xramp_channel(0.2, 0.6, 3.0, 2, 6)
  test_output.call(3, "
 (ramp 2 6) ; xramp_channel(0.200, 0.600, 3.000, 2, 6 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.700, [4]-0.000 -> 1.099, off: -0.000, scl: 0.200]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [1]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.400, [4]-0.000 -> 0.439, off: -0.000, scl: 0.200]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [4]0.659 -> 1.099, off: -0.000, scl: 0.200]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  set_sample(4, 0.5)
  test_output.call(4, "
 (set 4 1) ; set_sample(4, 0.5000 [4:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.300, [4]-0.000 -> 0.220, off: -0.000, scl: 0.200]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [4]0.659 -> 1.099, off: -0.000, scl: 0.200]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  revert_sound
  test_name = "xramp-ramp"
  map_chan($init_channel, 0, 10)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case1, (res = channel2vct))
    snd_display("xramp-ramp (1): %s", res.inspect)
  end
  scale_channel(0.5)
  test_output.call(4, "
 (scale 0 11) ; scale_channel(0.500, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]-0.000 -> 0.400, [4]0.000 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [4]1.733 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case2, (res = channel2vct))
    snd_display("xramp-ramp (2): %s", res.inspect)
  end
  undo
  scale_channel(0.5, 2, 4)
  test_output.call(4, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [4]0.693 -> 1.733, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [4]2.079 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo(2)
  ramp_channel(0.75, 0.25)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.750, 0.250, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.750 -> 0.250, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  ramp_channel(0.2, 0.6, 2, 6)
  test_output.call(3, "
 (ramp 2 6) ; ramp_channel(0.200, 0.600, 2, 6 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.600, [4]0.693 -> 2.426, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [4]2.773 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.360, [4]0.693 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.440 -> 0.600, [4]1.733 -> 2.426, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [4]2.773 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  set_sample(4, 0.5)
  test_output.call(4, "
 (set 4 1) ; set_sample(4, 0.5000 [4:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280, [4]0.693 -> 1.040, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.440 -> 0.600, [4]1.733 -> 2.426, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [4]2.773 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  close_sound(ind)
end

# ramp2+xramp
def test055
  ind = new_sound("test.snd")
  idx = -1
  test_name = "ramp2+xramp"
  test_output = lambda do |ed, str|
    idx += 1
    res = display_edits(ind, 0, ed).split(/\n/)[0..1].join("\n")
    str = str.split(/\n/)[0..1].join("\n")
    snd_display("%s %d: %s %s", test_name, idx, str, res) unless res == str
  end
  map_chan($init_channel, 0, 10)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case1 = channel2vct,
                vct(0.000, 0.000, 0.001, 0.005, 0.015, 0.038, 0.081, 0.163, 0.310, 0.565, 1.000))
    snd_display("ramp2+xramp (1): %s", case1.inspect)
  end
  scale_channel(0.5)
  test_output.call(5, "
 (scale 0 11) ; scale_channel(0.500, 0, false [5:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  scale_channel(0.5, 0, 5)
  test_output.call(5, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [5:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]-0.000 -> 0.400, [2]-0.000 -> 0.400, [4]0.000 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [2]0.500 -> 1.000, [4]1.733 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case2 = channel2vct,
                vct(0.000, 0.000, 0.001, 0.003, 0.008, 0.038, 0.081, 0.163, 0.310, 0.565, 1.000))
    snd_display("ramp2+xramp (2): %s", case2.inspect)
  end
  undo
  scale_channel(0.5, 2, 4)
  test_output.call(5, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [5:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [2]-0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [2]0.200 -> 0.500, [4]0.693 -> 1.733, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [2]0.600 -> 1.000, [4]2.079 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo(2)
  ramp_channel(0.75, 0.25)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.750, 0.250, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]0.750 -> 0.250, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  ramp_channel(0.2, 0.6, 2, 6)
  test_output.call(4, "
 (ramp 2 6) ; ramp_channel(0.200, 0.600, 2, 6 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.700, [2]0.200 -> 0.600, [4]0.693 -> 2.426, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [4]2.773 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(5, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [5:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [1]-0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.400, [2]0.200 -> 0.360, [4]0.693 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.440 -> 0.600, [4]1.733 -> 2.426, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [4]2.773 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  set_sample(4, 0.5)
  test_output.call(5, "
 (set 4 1) ; set_sample(4, 0.5000 [5:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.300, [2]0.200 -> 0.280, [4]0.693 -> 1.040, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.440 -> 0.600, [4]1.733 -> 2.426, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [4]2.773 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  revert_sound
  idx = -1
  test_name = "xramp+ramp2"
  map_chan($init_channel, 0, 10)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(4, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case1, (res = channel2vct))
    snd_display("xramp+ramp2 (1): %s", res.inspect)
  end
  close_sound(ind)
end

# multi-ramp2+xramp
# multi-ramp-xramp
# xramp2
# multi-xramp2
def test065
  ind = new_sound("test.snd")
  idx = -1
  test_name = "multi-ramp2+xramp"
  test_output = lambda do |ed, str|
    idx += 1
    res = display_edits(ind, 0, ed).split(/\n/)[0..1].join("\n")
    str = str.split(/\n/)[0..1].join("\n")
    snd_display("%s %d: %s %s", test_name, idx, str, res) unless res == str
  end
  map_channel($init_channel, 0, 100)
  scale_channel(0.5)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  case3 = channel2vct
  undo(4)
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(14, "
 (ramp 0 100) ; ramp_channel(0.000, 1.000, 0, false [14:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 0.091, [2]0.000 -> 0.091, [4]3.466 -> 3.151, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.101 -> 0.192, [2]0.101 -> 0.192, [4]3.116 -> 2.801, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.202 -> 0.293, [2]0.202 -> 0.293, [4]2.766 -> 2.451, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394, [2]0.303 -> 0.394, [4]2.416 -> 2.100, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.495, [2]0.404 -> 0.495, [4]2.065 -> 1.750, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.505 -> 0.596, [2]0.505 -> 0.596, [4]1.715 -> 1.400, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697, [2]0.606 -> 0.697, [4]1.365 -> 1.050, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.707 -> 0.798, [2]0.707 -> 0.798, [4]1.015 -> 0.700, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.808 -> 0.899, [2]0.808 -> 0.899, [4]0.665 -> 0.350, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000, [2]0.909 -> 1.000, [4]0.315 -> -0.000, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 100, end_mark)
")
  unless vequal(case3, res = channel2vct)
    snd_display("multi-ramp2+xramp: %s", res.inspect)
  end
  revert_sound
  map_channel($init_channel, 0, 100)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0, 10, 20)
  ramp_channel(0.0, 1.0, 50, 10)
  ramp_channel(0.0, 1.0, 25, 10)
  test_output.call(5, "
 (ramp 25 10) ; ramp_channel(0.000, 1.000, 25, 10 [5:8]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]3.466 -> 3.151, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 10, cp->sounds[1][10:24, 1.000, [1]-0.000 -> 0.737, [4]3.116 -> 2.626, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 25, cp->sounds[1][25:29, 1.000, [1]0.789 -> 1.000, [2]-0.000 -> 0.444, [4]2.591 -> 2.451, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [1]0.556 -> 1.000, [4]2.416 -> 2.275, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 35, cp->sounds[1][35:49, 1.000, [4]2.240 -> 1.750, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [1]-0.000 -> 1.000, [4]1.715 -> 1.400, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 60, cp->sounds[1][60:99, 1.000, [4]1.365 -> 0.000, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 100, end_mark)
")
  close_sound(ind)
  ind = new_sound("test.snd")
  idx = -1
  test_name = "multi-xramp-ramp"
  map_channel($init_channel, 0, 100)
  scale_channel(0.5)
  ramp_channel(0.0, 1.0)
  xramp_channel(1.0, 0.0, 32.0)
  case3 = channel2vct
  undo(3)
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  ramp_channel(0.0, 1.0)
  xramp_channel(1.0, 0.0, 32.0)
  test_output.call(13, "
 (ramp 0 100) ; xramp_channel(1.000, 0.000, 32.000, 0, false [13:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 0.091, [4]3.466 -> 3.151, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.101 -> 0.192, [4]3.116 -> 2.801, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.202 -> 0.293, [4]2.766 -> 2.451, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394, [4]2.416 -> 2.100, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.495, [4]2.065 -> 1.750, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.505 -> 0.596, [4]1.715 -> 1.400, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697, [4]1.365 -> 1.050, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.707 -> 0.798, [4]1.015 -> 0.700, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.808 -> 0.899, [4]0.665 -> 0.350, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000, [4]0.315 -> -0.000, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 100, end_mark)
")
  unless vequal(case3, res = channel2vct)
    snd_display("multi-ramp-xramp: %s", res.inspect)
  end
  undo(12)
  xramp_channel(0.0, 1.0, 3.0, 10, 20)
  xramp_channel(0.0, 1.0, 3.0, 50, 10)
  xramp_channel(0.0, 1.0, 3.0, 25, 10)
  test_output.call(4, "
 (ramp 25 10) ; xramp_channel(0.000, 1.000, 3.000, 25, 10 [4:8]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 100] 
   (at 10, cp->sounds[1][10:24, 1.000, [4]-0.000 -> 0.810, off: -0.500, scl: 0.500]) [buf: 100] 
   (at 25, cp->sounds[1][25:29, 1.000, [3]0.000 -> 0.488, [4]0.867 -> 1.099, off: -0.500, scl: 0.500, off2: -0.500, scl2: 0.500]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [4]0.610 -> 1.099, off: -0.500, scl: 0.500]) [buf: 100] 
   (at 35, cp->sounds[1][35:49, 1.000]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [4]0.000 -> 1.099, off: -0.500, scl: 0.500]) [buf: 100] 
   (at 60, cp->sounds[1][60:99, 1.000]) [buf: 100] 
   (at 100, end_mark)
")
  revert_sound
  map_channel($init_channel, 0, 100)
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  xramp_channel(1.0, 0.0, 32.0)
  ramp_channel(0.0, 1.0)
  test_output.call(13, "
 (ramp 0 100) ; ramp_channel(0.000, 1.000, 0, false [13:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 0.091, [4]3.466 -> 3.151, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.101 -> 0.192, [4]3.116 -> 2.801, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.202 -> 0.293, [4]2.766 -> 2.451, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394, [4]2.416 -> 2.100, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.495, [4]2.065 -> 1.750, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.505 -> 0.596, [4]1.715 -> 1.400, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697, [4]1.365 -> 1.050, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.707 -> 0.798, [4]1.015 -> 0.700, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.808 -> 0.899, [4]0.665 -> 0.350, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000, [4]0.315 -> -0.000, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 100, end_mark)
")
  undo(12)
  ramp_channel(0.0, 1.0, 10, 20)
  ramp_channel(0.0, 1.0, 50, 10)
  ramp_channel(0.0, 1.0, 25, 10)
  test_output.call(4, "
 (ramp 25 10) ; ramp_channel(0.000, 1.000, 25, 10 [4:8]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 100] 
   (at 10, cp->sounds[1][10:24, 1.000, [1]-0.000 -> 0.737]) [buf: 100] 
   (at 25, cp->sounds[1][25:29, 1.000, [1]0.789 -> 1.000, [2]-0.000 -> 0.444]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [1]0.556 -> 1.000]) [buf: 100] 
   (at 35, cp->sounds[1][35:49, 1.000]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [1]-0.000 -> 1.000]) [buf: 100] 
   (at 60, cp->sounds[1][60:99, 1.000]) [buf: 100] 
   (at 100, end_mark)
")
  close_sound(ind)
  ind = new_sound("test.snd")
  idx = -1
  test_name = "xramp2"
  map_chan($init_channel, 0, 10)
  xramp_channel(0.0, 1.0, 2.0)
  xramp_channel(0.0, 1.0, 2.0)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 2.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [3]0.000 -> 0.693, [4]0.000 -> 0.693, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.005, 0.022, 0.053, 0.102, 0.172, 0.266, 0.390, 0.549, 0.750, 1.000))
    snd_display("xramp2 (1): %s", res.inspect)
  end
  scale_channel(0.5)
  test_output.call(4, "
 (scale 0 11) ; scale_channel(0.500, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [3]0.000 -> 0.693, [4]0.000 -> 0.693, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [3]0.000 -> 0.277, [4]0.000 -> 0.277, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [3]0.347 -> 0.693, [4]0.347 -> 0.693, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.003, 0.011, 0.027, 0.051, 0.172, 0.266, 0.390, 0.549, 0.750, 1.000))
    snd_display("xramp2 (2): %s", res.inspect)
  end
  undo
  scale_channel(0.5, 2, 4)
  test_output.call(4, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [3]0.000 -> 0.069, [4]0.000 -> 0.069, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [3]0.139 -> 0.347, [4]0.139 -> 0.347, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [3]0.416 -> 0.693, [4]0.416 -> 0.693, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo(2)
  xramp_channel(0.75, 0.25, 0.3)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.750, 0.250, 0.300, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [3]-1.204 -> -0.000, [4]0.000 -> 0.693, off: -1.000, scl: 1.000, off2: 0.964, scl2: -0.714]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  xramp_channel(0.2, 0.6, 32.0, 2, 6)
  test_output.call(3, "
 (ramp 2 6) ; xramp_channel(0.200, 0.600, 32.000, 2, 6 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [4]0.000 -> 0.069, off: -1.000, scl: 1.000]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [3]0.000 -> 3.466, [4]0.139 -> 0.485, off: -1.000, scl: 1.000, off2: 0.187, scl2: 0.013]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [4]0.555 -> 0.693, off: -1.000, scl: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [4]0.000 -> 0.069, off: -1.000, scl: 1.000]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [3]0.000 -> 1.386, [4]0.139 -> 0.277, off: -1.000, scl: 1.000, off2: 0.187, scl2: 0.013]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [3]2.079 -> 3.466, [4]0.347 -> 0.485, off: -1.000, scl: 1.000, off2: 0.187, scl2: 0.013]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [4]0.555 -> 0.693, off: -1.000, scl: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  set_sample(4, 0.5)
  test_output.call(4, "
 (set 4 1) ; set_sample(4, 0.5000 [4:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [4]0.000 -> 0.069, off: -1.000, scl: 1.000]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [3]0.000 -> 0.693, [4]0.139 -> 0.208, off: -1.000, scl: 1.000, off2: 0.187, scl2: 0.013]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [3]2.079 -> 3.466, [4]0.347 -> 0.485, off: -1.000, scl: 1.000, off2: 0.187, scl2: 0.013]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [4]0.555 -> 0.693, off: -1.000, scl: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo(3)
  close_sound(ind)
  ind = new_sound("test.snd")
  idx = -1
  test_name = "multi-xramp2"
  map_channel($init_channel, 0, 100)
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  xramp_channel(0.0, 1.0, 3.0)
  xramp_channel(1.0, 0.0, 0.3)
  test_output.call(13, "
 (ramp 0 100) ; xramp_channel(1.000, 0.000, 0.300, 0, false [13:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [3]-1.204 -> -1.095, [4]-0.000 -> 0.100, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [3]-1.082 -> -0.973, [4]0.111 -> 0.211, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [3]-0.961 -> -0.851, [4]0.222 -> 0.322, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [3]-0.839 -> -0.730, [4]0.333 -> 0.433, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [3]-0.718 -> -0.608, [4]0.444 -> 0.544, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [3]-0.596 -> -0.486, [4]0.555 -> 0.655, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [3]-0.474 -> -0.365, [4]0.666 -> 0.766, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [3]-0.353 -> -0.243, [4]0.777 -> 0.877, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [3]-0.231 -> -0.122, [4]0.888 -> 0.988, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [3]-0.109 -> -0.000, [4]0.999 -> 1.099, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
   (at 100, end_mark)
")
  close_sound(ind)
end

# ramp3
# multi-ramp3
def test075
  ind = new_sound("test.snd")
  idx = -1
  test_name = "ramp3"
  test_output = lambda do |ed, str|
    idx += 1
    res = display_edits(ind, 0, ed).split(/\n/)[0..1].join("\n")
    str = str.split(/\n/)[0..1].join("\n")
    snd_display("%s %d: %s %s", test_name, idx, str, res) unless res == str
  end
  map_chan($init_channel, 0, 10)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [3]-0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216, 0.343, 0.512, 0.729, 1.000))
    snd_display("ramp3 (1): %s", res.inspect)
  end
  scale_channel(0.5)
  test_output.call(5, "
 (scale 0 11) ; scale_channel(0.500, 0, false [5:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [3]-0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  scale_channel(0.5, 0, 5)
  test_output.call(5, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [5:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]-0.000 -> 0.400, [2]-0.000 -> 0.400, [3]-0.000 -> 0.400]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [2]0.500 -> 1.000, [3]0.500 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  scale_channel(0.5, 2, 4)
  test_output.call(5, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [5:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [2]-0.000 -> 0.100, [3]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [2]0.200 -> 0.500, [3]0.200 -> 0.500]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [2]0.600 -> 1.000, [3]0.600 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo(2)
  ramp_channel(0.75, 0.25)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.750, 0.250, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [3]0.750 -> 0.250]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  ramp_channel(0.2, 0.6, 2, 6)
  test_output.call(4, "
 (ramp 2 6) ; ramp_channel(0.200, 0.600, 2, 6 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [2]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.700, [2]0.200 -> 0.700, [3]0.200 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [2]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(5, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [5:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [1]-0.000 -> 0.100, [2]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.400, [2]0.200 -> 0.400, [3]0.200 -> 0.360]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.500 -> 0.700, [3]0.440 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [2]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo
  set_sample(4, 0.5)
  test_output.call(5, "
 (set 4 1) ; set_sample(4, 0.5000 [5:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]-0.000 -> 0.100, [2]-0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.300, [2]0.200 -> 0.300, [3]0.200 -> 0.280]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.500 -> 0.700, [3]0.440 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [2]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo(3)
  close_sound(ind)
  ind = new_sound("test.snd")
  idx = -1
  test_name = "multi-ramp3"
  map_channel($init_channel, 0, 100)
  test_name = ""
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  ramp_channel(0.0, 1.0)
  ramp_channel(1.0, -0.5)
  ramp_channel(-0.5, 1.5)
  test_output.call(14, "
 (ramp 0 100) ; ramp_channel(-0.500, 1.500, 0, false [14:11]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 0.091, [2]1.000 -> 0.864, [3]-0.500 -> -0.318]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 0.500, [1]0.101 -> 0.192, [2]0.848 -> 0.712, [3]-0.298 -> -0.116]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 0.500, [1]0.202 -> 0.293, [2]0.697 -> 0.561, [3]-0.096 -> 0.086]) [buf: 100] 
   (at 30, cp->sounds[1][30:39, 0.500, [1]0.303 -> 0.394, [2]0.545 -> 0.409, [3]0.106 -> 0.288]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 0.500, [1]0.404 -> 0.495, [2]0.394 -> 0.258, [3]0.308 -> 0.490]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 0.500, [1]0.505 -> 0.596, [2]0.242 -> 0.106, [3]0.510 -> 0.692]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 0.500, [1]0.606 -> 0.697, [2]0.091 -> -0.045, [3]0.712 -> 0.894]) [buf: 100] 
   (at 70, cp->sounds[1][70:79, 0.500, [1]0.707 -> 0.798, [2]-0.061 -> -0.197, [3]0.914 -> 1.096]) [buf: 100] 
   (at 80, cp->sounds[1][80:89, 0.500, [1]0.808 -> 0.899, [2]-0.212 -> -0.348, [3]1.116 -> 1.298]) [buf: 100] 
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000, [2]-0.364 -> -0.500, [3]1.318 -> 1.500]) [buf: 100] 
   (at 100, end_mark)
")
  undo(13)
  ramp_channel(0.0, 1.0, 10, 30)
  ramp_channel(0.0, 1.0, 50, 20)
  ramp_channel(0.0, 1.0, 20, 15)
  ramp_channel(0.0, 1.0, 30, 30)
  test_output.call(5, "
 (ramp 30 30) ; ramp_channel(0.000, 1.000, 30, 30 [5:10]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 1.000, [1]0.000 -> 0.310]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 1.000, [1]0.345 -> 0.655, [2]-0.000 -> 0.643]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [1]0.690 -> 0.828, [2]0.714 -> 1.000, [3]0.000 -> 0.138]) [buf: 100] 
   (at 35, cp->sounds[1][35:39, 1.000, [1]0.862 -> 1.000, [2]0.172 -> 0.310]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 1.000, [1]0.345 -> 0.655]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [1]-0.000 -> 0.474, [2]0.690 -> 1.000]) [buf: 100] 
   (at 60, cp->sounds[1][60:69, 1.000, [1]0.526 -> 1.000]) [buf: 100] 
   (at 70, cp->sounds[1][70:99, 1.000]) [buf: 100] 
   (at 100, end_mark)
")
  close_sound(ind)
  ind = new_sound("test.snd")
  map_chan($init_channel, 0, 10)
  idx = -1
  test_name = "ramp+xramp"
  ramp_channel(0.0, 1.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "xramp+xramp"
  undo(2)
  xramp_channel(0.0, 1.0, 0.32)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [3]0.000 -> 3.466, [4]0.000 -> -1.139, off: 1.471, scl: -1.471, off2: -0.032, scl2: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "xramp+xramp+xramp"
  undo(2)
  xramp_channel(0.0, 1.0, 0.32)
  xramp_channel(0.0, 1.0, 32.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(4, "
 (set 0 11) ; env_channel_with_base([0.000, 0.000, 1.000, 1.000], 32.0000, 0, false [4:2]:
   (at 0, cp->sounds[2][0:10, 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "xramp+xramp+ramp"
  undo(3)
  xramp_channel(0.0, 1.0, 0.32)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [3]0.000 -> 3.466, [4]0.000 -> -1.139, off: 1.471, scl: -1.471, off2: -0.032, scl2: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "xramp+ramp"
  undo(3)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "ramp+ramp+xramp"
  undo(2)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(4, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "ramp+ramp+ramp+ramp"
  undo(3)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(5, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [5:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [3]-0.000 -> 1.000, [4]-0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "ramp+ramp+ramp+xramp"
  undo(4)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(5, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [5:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]-0.000 -> 1.000, [2]-0.000 -> 1.000, [3]-0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  close_sound(ind)
end

def test085
  data = make_vct(101, 1.0)
  rto1_data = make_vct(101)
  xto1_data = make_vct(101)
  cos_data = make_vct(101)
  let(make_env(:envelope, [0, 0, 1, 1], :end, 100, :base, 32.0)) do |xe|
    incr = PI / 101.0
    ang = -0.5 * PI
    101.times do |i|
      rto1_data[i] = i * 0.01
      xto1_data[i] = env(xe)
      cos_data[i] = cos(ang)
      ang += incr
    end
  end
  ind = new_sound("test.snd")
  set_to_1 = lambda do map_chan($init_channel, 0, 100) end
  cset_to_1 = lambda do |dat| 101.times do |i| dat[i] = 1.0 end end
  ramp_to_1 = lambda do ramp_channel(0.0, 1.0) end
  cramp_to_1 = lambda do |dat| vct_multiply!(dat, rto1_data) end
  scale_by_half = lambda do scale_channel(0.5) end
  cscale_by_half = lambda do |dat| vct_scale!(dat, 0.5) end
  scale_by_two = lambda do scale_channel(2.0, 30, 40) end
  cscale_by_two = lambda do |dat| (30...70).each do |i| dat[i] *= 2.0 end end
  xramp_to_1 = lambda do xramp_channel(0.0, 1.0, 32.0) end
  cxramp_to_1 = lambda do |dat| vct_multiply!(dat, xto1_data) end
  scale_mid = lambda do scale_channel(0.125, 30, 30) end
  cscale_mid = lambda do |dat| (30...60).each do |i| dat[i] *= 0.125 end end
  on_air = lambda do scale_channel(0.0, 10, 30) end
  con_air = lambda do |dat| (10...40).each do |i| dat[i] = 0.0 end end
  rev_channel2vct = lambda do
    len = data.length
    rd = make_sample_reader(len - 1, ind, 0, -1)
    dat = make_vct(len)
    (len - 1).downto(0) do |i| dat[i] = read_sample(rd) end
    free_sample_reader(rd)
    dat
  end
  set_squelch_update(true, ind)
  # 0 case
  set_to_1.call
  unless vvequal(data, res = channel2vct)
    snd_display("0 case! %s", res.inspect)
  end
  unless vvequal(data, res = rev_channel2vct.call)
    snd_display("0 case rev! %s", res.inspect)
  end
  # 1 case
  [[scale_by_two, cscale_by_two, :scale_by_two],
    [ramp_to_1, cramp_to_1, :ramp_to_1],
    [xramp_to_1, cxramp_to_1, :xramp_to_1],
    [scale_by_half, cscale_by_half, :scale_by_half],
    [scale_mid, cscale_mid, :scale_mid],
    [on_air, con_air, :on_air]].each do |func, check, name|
    revert_sound
    set_to_1.call
    cset_to_1.call(data)
    func.call
    check.call(data)
    unless vvequal(data, res = channel2vct)
      snd_display("1 case: %s\n%s\n%s", name, data.inspect, res.inspect)
    end
    unless vvequal(data, res = rev_channel2vct.call)
      snd_display("1 rev case: c%s\n%s\n%s", name, data.inspect, res.inspect)
    end
  end
  # 2 case
  [[scale_by_two, cscale_by_two, :scale_by_two],
    [ramp_to_1, cramp_to_1, :ramp_to_1],
    [xramp_to_1, cxramp_to_1, :xramp_to_1],
    [scale_by_half, cscale_by_half, :scale_by_half],
    [scale_mid, cscale_mid, :scale_mid],
    [on_air, con_air, :on_air]].each do |func, check, name|
    [[scale_by_two, cscale_by_two, :scale_by_two],
      [ramp_to_1, cramp_to_1, :ramp_to_1],
      [xramp_to_1, cxramp_to_1, :xramp_to_1],
      [scale_by_half, cscale_by_half, :scale_by_half],
      [scale_mid, cscale_mid, :scale_mid],
      [on_air, con_air, :on_air]].each do |func1, check1, name1|
      revert_sound
      set_to_1.call
      cset_to_1.call(data)
      func.call
      check.call(data)
      func1.call
      check1.call(data)
      unless vvequal(data, res = channel2vct)
        snd_display("2 case: %s (%s)\n%s\n%s", name1, name, data.inspect, res.inspect)
      end
      unless vvequal(data, res = rev_channel2vct.call)
        snd_display("2 rev case: c%s (c%s)\n%s\n%s", name1, name, data.inspect, res.inspect)
      end
    end
  end
  # 3 case
  [[scale_by_two, cscale_by_two, :scale_by_two],
    [ramp_to_1, cramp_to_1, :ramp_to_1],
    [xramp_to_1, cxramp_to_1, :xramp_to_1],
    [scale_by_half, cscale_by_half, :scale_by_half],
    [scale_mid, cscale_mid, :scale_mid],
    [on_air, con_air, :on_air]].each do |func, check, name|
    [[scale_by_two, cscale_by_two, :scale_by_two],
      [ramp_to_1, cramp_to_1, :ramp_to_1],
      [xramp_to_1, cxramp_to_1, :xramp_to_1],
      [scale_by_half, cscale_by_half, :scale_by_half],
      [scale_mid, cscale_mid, :scale_mid],
      [on_air, con_air, :on_air]].each do |func1, check1, name1|
      [[scale_by_two, cscale_by_two, :scale_by_two],
        [ramp_to_1, cramp_to_1, :ramp_to_1],
        [xramp_to_1, cxramp_to_1, :xramp_to_1],
        [scale_by_half, cscale_by_half, :scale_by_half],
        [scale_mid, cscale_mid, :scale_mid],
        [on_air, con_air, :on_air]].each do |func2, check2, name2|
        revert_sound
        set_to_1.call
        cset_to_1.call(data)
        func.call
        check.call(data)
        func1.call
        check1.call(data)
        func2.call
        check2.call(data)
        unless vvequal(data, res = channel2vct)
          snd_display("2 case: %s (%s (%s))\n%s\n%s",
                      name2, name1, name, data.inspect, res.inspect)
        end
        unless vvequal(data, res = rev_channel2vct.call)
          snd_display("2 rev case: c%s (c%s (c%s))\n%s\n%s",
                      name2, name1, name, data.inspect, res.inspect)
        end
      end
    end
  end
  if $all_args
    # 4 case
    [[scale_by_two, cscale_by_two, :scale_by_two],
      [ramp_to_1, cramp_to_1, :ramp_to_1],
      [xramp_to_1, cxramp_to_1, :xramp_to_1],
      [scale_by_half, cscale_by_half, :scale_by_half],
      [scale_mid, cscale_mid, :scale_mid],
      [on_air, con_air, :on_air]].each do |func, check, name|
      [[scale_by_two, cscale_by_two, :scale_by_two],
        [ramp_to_1, cramp_to_1, :ramp_to_1],
        [xramp_to_1, cxramp_to_1, :xramp_to_1],
        [scale_by_half, cscale_by_half, :scale_by_half],
        [scale_mid, cscale_mid, :scale_mid],
        [on_air, con_air, :on_air]].each do |func1, check1, name1|
        [[scale_by_two, cscale_by_two, :scale_by_two],
          [ramp_to_1, cramp_to_1, :ramp_to_1],
          [xramp_to_1, cxramp_to_1, :xramp_to_1],
          [scale_by_half, cscale_by_half, :scale_by_half],
          [scale_mid, cscale_mid, :scale_mid],
          [on_air, con_air, :on_air]].each do |func2, check2, name2|
          [[scale_by_two, cscale_by_two, :scale_by_two],
            [ramp_to_1, cramp_to_1, :ramp_to_1],
            [xramp_to_1, cxramp_to_1, :xramp_to_1],
            [scale_by_half, cscale_by_half, :scale_by_half],
            [scale_mid, cscale_mid, :scale_mid],
            [on_air, con_air, :on_air]].each do |func3, check3, name3|
            revert_sound
            set_to_1.call
            cset_to_1.call(data)
            func.call
            check.call(data)
            func1.call
            check1.call(data)
            func2.call
            check2.call(data)
            func3.call
            check3.call(data)
            unless vvequal(data, res = channel2vct)
              snd_display("2 case: %s (%s (%s (%s)))\n%s\n%s",
                          name3, name2, name1, name, data.inspect, res.inspect)
            end
            unless vvequal(data, res = rev_channel2vct.call)
              snd_display("2 rev case: c%s (c%s (c%s (c%s)))\n%s\n%s",
                          name3, name2, name1, name, data.inspect, res.inspect)
            end
          end
        end
      end
    end
    # 5 case
    [[scale_by_two, cscale_by_two, :scale_by_two],
      [ramp_to_1, cramp_to_1, :ramp_to_1],
      [xramp_to_1, cxramp_to_1, :xramp_to_1],
      [scale_by_half, cscale_by_half, :scale_by_half],
      [scale_mid, cscale_mid, :scale_mid],
      [on_air, con_air, :on_air]].each do |func, check, name|
      [[scale_by_two, cscale_by_two, :scale_by_two],
        [ramp_to_1, cramp_to_1, :ramp_to_1],
        [xramp_to_1, cxramp_to_1, :xramp_to_1],
        [scale_by_half, cscale_by_half, :scale_by_half],
        [scale_mid, cscale_mid, :scale_mid],
        [on_air, con_air, :on_air]].each do |func1, check1, name1|
        [[scale_by_two, cscale_by_two, :scale_by_two],
          [ramp_to_1, cramp_to_1, :ramp_to_1],
          [xramp_to_1, cxramp_to_1, :xramp_to_1],
          [scale_by_half, cscale_by_half, :scale_by_half],
          [scale_mid, cscale_mid, :scale_mid],
          [on_air, con_air, :on_air]].each do |func2, check2, name2|
          [[scale_by_two, cscale_by_two, :scale_by_two],
            [ramp_to_1, cramp_to_1, :ramp_to_1],
            [xramp_to_1, cxramp_to_1, :xramp_to_1],
            [scale_by_half, cscale_by_half, :scale_by_half],
            [scale_mid, cscale_mid, :scale_mid],
            [on_air, con_air, :on_air]].each do |func3, check3, name3|
            [[scale_by_two, cscale_by_two, :scale_by_two],
              [ramp_to_1, cramp_to_1, :ramp_to_1],
              [xramp_to_1, cxramp_to_1, :xramp_to_1],
              [scale_by_half, cscale_by_half, :scale_by_half],
              [scale_mid, cscale_mid, :scale_mid],
              [on_air, con_air, :on_air]].each do |func4, check4, name4|
              revert_sound
              set_to_1.call
              cset_to_1.call(data)
              func.call
              check.call(data)
              func1.call
              check1.call(data)
              func2.call
              check2.call(data)
              func3.call
              check3.call(data)
              func4.call
              check4.call(data)
              unless vvequal(data, res = channel2vct)
                snd_display("2 case: %s (%s (%s (%s (%s))))\n%s\n%s",
                            name4, name3, name2, name1, name, data.inspect, res.inspect)
              end
              unless vvequal(data, res = rev_channel2vct.call)
                snd_display("2 rev case: c%s (c%s (c%s (c%s (c%s))))\n%s\n%s",
                            name4, name3, name2, name1, name, data.inspect, res.inspect)
              end
            end
          end
        end
      end
    end
    # 6 case
    [[scale_by_two, cscale_by_two, :scale_by_two],
      [ramp_to_1, cramp_to_1, :ramp_to_1],
      [xramp_to_1, cxramp_to_1, :xramp_to_1],
      [scale_by_half, cscale_by_half, :scale_by_half],
      [scale_mid, cscale_mid, :scale_mid],
      [on_air, con_air, :on_air]].each do |func, check, name|
      [[scale_by_two, cscale_by_two, :scale_by_two],
        [ramp_to_1, cramp_to_1, :ramp_to_1],
        [xramp_to_1, cxramp_to_1, :xramp_to_1],
        [scale_by_half, cscale_by_half, :scale_by_half],
        [scale_mid, cscale_mid, :scale_mid],
        [on_air, con_air, :on_air]].each do |func1, check1, name1|
        [[scale_by_two, cscale_by_two, :scale_by_two],
          [ramp_to_1, cramp_to_1, :ramp_to_1],
          [xramp_to_1, cxramp_to_1, :xramp_to_1],
          [scale_by_half, cscale_by_half, :scale_by_half],
          [scale_mid, cscale_mid, :scale_mid],
          [on_air, con_air, :on_air]].each do |func2, check2, name2|
          [[scale_by_two, cscale_by_two, :scale_by_two],
            [ramp_to_1, cramp_to_1, :ramp_to_1],
            [xramp_to_1, cxramp_to_1, :xramp_to_1],
            [scale_by_half, cscale_by_half, :scale_by_half],
            [scale_mid, cscale_mid, :scale_mid],
            [on_air, con_air, :on_air]].each do |func3, check3, name3|
            [[scale_by_two, cscale_by_two, :scale_by_two],
              [ramp_to_1, cramp_to_1, :ramp_to_1],
              [xramp_to_1, cxramp_to_1, :xramp_to_1],
              [scale_by_half, cscale_by_half, :scale_by_half],
              [scale_mid, cscale_mid, :scale_mid],
              [on_air, con_air, :on_air]].each do |func4, check4, name4|
              [[scale_by_two, cscale_by_two, :scale_by_two],
                [ramp_to_1, cramp_to_1, :ramp_to_1],
                [xramp_to_1, cxramp_to_1, :xramp_to_1],
                [scale_by_half, cscale_by_half, :scale_by_half],
                [scale_mid, cscale_mid, :scale_mid],
                [on_air, con_air, :on_air]].each do |func5, check5, name5|
                revert_sound
                set_to_1.call
                cset_to_1.call(data)
                func.call
                check.call(data)
                func1.call
                check1.call(data)
                func2.call
                check2.call(data)
                func3.call
                check3.call(data)
                func4.call
                check4.call(data)
                func5.call
                check5.call(data)
                unless vvequal(data, res = channel2vct)
                  snd_display("2 case: %s (%s (%s (%s (%s (%s)))))\n%s\n%s",
                              name5, name4, name3, name2, name1, name, data.inspect, res.inspect)
                end
                unless vvequal(data, res = rev_channel2vct.call)
                  snd_display("2 rev case: c%s (c%s (c%s (c%s (c%s (c%s)))))\n%s\n%s",
                              name5, name4, name3, name2, name1, name, data.inspect, res.inspect)
                end
              end
            end
          end
        end
      end
    end
  end
  close_sound(ind)
end

def test095
  ind = open_sound("oboe.snd")
  set_cursor(1000)
  delete_sample(321)
  unless (res = cursor) == 999
    snd_display("delete_sample before cursor: %d", res)
  end
  unless (res = cursor(ind, 0, 0)) == 1000
    snd_display("delete_sample before cursor (0): %d", res)
  end
  undo
  unless (res = cursor) == 1000
    snd_display("delete_sample after cursor undo: %d", res)
  end
  undo(-1)
  unless (res = cursor) == 999
    snd_display("delete_sample before cursor redo: %d", res)
  end
  redo_edit(-1)
  delete_samples(0, 100)
  unless (res = cursor) == 900
    snd_display("delete_samples before cursor: %d", res)
  end
  undo
  delete_samples(1100, 100)
  unless (res = cursor) == 1000
    snd_display("delete_samples after cursor: %d", res)
  end
  undo
  insert_samples(100, 100, make_vct(100))
  unless (res = cursor) == 1100
    snd_display("insert_samples before cursor: %d", res)
  end
  undo
  insert_samples(1100, 100, make_vct(100))
  unless (res = cursor) == 1000
    snd_display("insert_samples after cursor: %d", res)
  end
  undo
  set_samples(0, 100, make_vct(100))
  unless (res = cursor) == 1000
    snd_display("set_samples cursor: %d", res)
  end
  set_show_axes(Show_all_axes_unlabelled, ind, 0)
  update_time_graph
  close_sound(ind)
end

def test105
  ind = open_sound("oboe.snd")
  bnds = x_bounds(ind)
  xp = x_position_slider
  yp = y_position_slider
  xz = x_zoom_slider
  yz = y_zoom_slider
  if (res = snd_completion(" open-so")) != " open-sound"
    snd_display("completion: %s", res)
  end
  if (res = snd_completion(" open-sound")) != " open-sound"
    snd_display("completion: %s", res)
  end
  # INFO
  # Zoom_focus_right (constant) replaced by zoom_focus_style
  if (res = snd_completion(" zoom_focus_s")) != " zoom_focus_style"
    snd_display("completion: %s", res)
  end
  play_and_wait("oboe.snd")
  play_and_wait("oboe.snd", 12000)
  play_and_wait("oboe.snd", 12000, 15000)
  play_and_wait(0, false, false, false, false, edit_position - 1)
  old_speed = speed_control(ind)
  old_style = speed_control_style
  old_open = show_controls(ind)
  set_show_controls(true, ind)
  set_speed_control(-2.0, ind)
  play_and_wait(12345, ind)
  set_speed_control_style(Speed_control_as_semitone)
  set_speed_control(0.5, ind)
  set_speed_control_style(Speed_control_as_ratio)
  set_speed_control(0.25, ind)
  set_speed_control(old_speed, ind)
  set_speed_control_style(old_style)
  set_show_controls(old_open, ind)
  bomb(ind, true)
  k = disk_kspace("oboe.snd")
  snd_display("disk_kspace = %s", k.inspect) if (not k.kind_of?(Numeric)) or k <= 0
  k = disk_kspace("/baddy/hiho")
  # #if (!HAVE_STATFS) && (!HAVE_STATVFS) in snd-file.c
  # disk_kspace returns 1234567 in every case
  if k != 1234567 and k != -1
    snd_display("disk_kspace of bogus file = %s", k.inspect)
  end
  if (res = transform_frames).nonzero?
    snd_display("trandform_frames %d", res)
  end
  set_transform_graph?(true)
  unless (res = fft_peak(ind, 0, 1.0))
    snd_display("fft_peak %s?", res.inspect)
  end
  set_time_graph?(true)
  if (res = x_axis_label) != "time"
    snd_display("def time x_axis_label: %s?", res)
  end
  set_x_axis_label("no time", ind, 0, Time_graph)
  if (res = x_axis_label) != "no time"
    snd_display("time x_axis_label: %s?", res)
  end
  graph([0, 0, 1, 1, 2, 0], "lisp")
  update_lisp_graph
  if (res = x_axis_label(ind, 0, Lisp_graph)) != "lisp"
    snd_display("def lisp x_axis_label: %s?", res)
  end
  set_x_axis_label("no lisp", ind, 0, Lisp_graph)
  if (res = x_axis_label(ind, 0, Lisp_graph)) != "no lisp"
    snd_display("lisp x_axis_label: %s?", res)
  end
  graph_data(make_vct(4))
  update_lisp_graph
  graph(vct(0, 0, 1, 1, 2, 0))
  32.times do
    graph(vct(0, 1, 2))
    graph([vct(0, 1, 2), vct(3, 2, 1), vct(1, 2, 3)])
    graph([vct(0, 1, 2), vct(3, 2, 1)])
  end
  set_x_bounds([0.0, 0.01])
  data = make_graph_data
  if vct?(data)
    mid = (0.5 * data.length).round
    if data.length != (right_sample - left_sample + 1)
      snd_display("make_graph_data bounds: %s %s -> %d", left_sample, right_sample, data.length)
    end
    if fneq(res1 = data[mid], res2 = sample(left_sample + mid))
      snd_display("make_graph_data[%d]: %f %f?", mid, res1, res2)
    end
  end
  data = make_graph_data(ind, 0, 0, 100, 199)
  if vct?(data)
    mid = (0.5 * data.length).round
    if data.length != 100
      snd_display("make_graph_data 100:199: %d", data.length)
    end
    if fneq(res1 = data[50], res2 = sample(50))
      snd_display("make_graph_data: %f %f?", res1, res2)
    end
  end
  set_x_bounds([0.0, 0.1])
  update_transform_graph
  if (res = x_axis_label(ind, 0, Transform_graph)) != "frequency"
    snd_display("def fft x_axis_label: %s", res)
  end
  set_x_axis_label("fourier", ind, 0, Transform_graph)
  if (res = x_axis_label(ind, 0, Transform_graph)) != "fourier"
    snd_display("fft x_axis_label: %s", res)
  end
  if transform_frames.kind_of?(Numeric) and transform_frames.zero?
    snd_display("transform_graph? transform-frames: %d?", trandform_frames)
  end
  update_transform_graph
  peaks("tmp.peaks")
  pks_data = IO.readlines("tmp.peaks")
  if /Snd: fft peaks/ !~ pks_data[0]
    snd_display("peaks 1: %s", pks_data[0].inspect)
  end
  if /fft 512 points beginning at sample 0/ !~ pks_data[2]
    snd_display("peaks 2: %s", pks_data[2].inspect)
  end
  if /86.132812  1.00000/ !~ pks_data[4] and /0.000000  1.00000/ !~ pks_data[4]
    snd_display("peaks 3: %s", pks_data[4].inspect)
  end
  delete_file("tmp.peaks")
  peaks
  if provided?("xm") and (!dialog_widgets[20] or !RXtIsManaged(dialog_widgets[20]))
    snd_display("peaks but no help?")
  end
  dismiss_all_dialogs
  num_transforms = 6
  num_transform_graph_types = 3
  set_transform_graph?(true, ind, 0)
  set_transform_size(64, ind, 0)
  num_transforms.times do |i|
    set_transform_type(i)
    snd_display("transform? %d?", i) unless transform?(i)
    num_transform_graph_types.times do |j|
      set_transform_graph_type(j, ind, 0)
      update_transform_graph(ind, 0)
    end
  end
  set_transform_type(Fourier_transform)
  unless (res = transform?(transform_type))
    snd_display("transform? %d %d?", res, Fourier_transform)
  end
  unless transform?(Autocorrelation)
    snd_display("transform? Autocorrelation")
  end
  snd_display("read_only open_sound: %s?", read_only(ind).inspect) if read_only(ind)
  set_read_only(true, ind)
  snd_display("set_read_only: %s?", read_only(ind).inspect) unless read_only(ind)
  a_ctr = 0
  bind_key(?a, 0, lambda do a_ctr = 3 end)
  key(?a, 0)
  snd_display("bind_key: %s?", a_ctr.inspect) if a_ctr != 3
  unbind_key(?a, 0)
  a_ctr = 0
  key(?a, 0)
  5.times do |i|
    psf = eps_file
    if psf.kind_of?(String)
      delete_file(psf)
      set_graph_style(i)
      graph2ps
      if File.exists?(psf)
        File.unlink(psf)
      else
        snd_display("graph2ps: %s?", psf.inspect)
      end
    end
  end
  if (err = snd_catch do graph2ps("/bad/bad.eps") end.first) != :cannot_print
    snd_display("graph2ps err: %s?", err.inspect)
  end
  n2 = open_sound("2.snd") or open_sound("4.aiff")
  set_transform_graph?(true, n2)
  [Channels_superimposed, Channels_combined, Channels_combined].each do |style|
    set_channel_style(style, n2)
    if (res = channel_style(n2)) != style
      snd_display("channel_style->%d: %s?", style, res)
    end
    graph2ps("aaa.eps")
  end
  close_sound(n2)
  if channels(ind) == 1
    set_channel_style(Channels_superimposed, ind)
    if (res = channel_style(ind)) != Channels_separate
      snd_display("channel_style[0]->%d: %s?", Channels_separate, res)
    end
  end
  set_sync(32, ind)
  if (res = sync(ind)) != 32
    snd_display("sync->32: %s?", res)
  end
  set_sync(0, ind)
  set_channel_sync(12, ind, 0)
  if (res = channel_sync(ind, 0)) != 12
    snd_display("channel_sync->12: %s?", res)
  end
  set_channel_sync(0, ind, 0)
  snd_display("unbind_key: %s?", a_ctr) if a_ctr.nonzero?
  snd_display("x_position_slider: %f?", xp) if fneq(xp, 0.0)
  snd_display("y_position_slider: %f?", yp) if fneq(yp, 0.0)
  snd_display("x_zoom_slider: %f?", xz) if fneq(xz, 0.04338)
  snd_display("y_zoom_slider: %f?", yz) if fneq(yz, 1.0)
  snd_display("x_bounds: %s?", bnds.inspect) if fneq(bnds[0], 0.0) or fneq(bnds[1], 0.1)
  if (res = find_sound("oboe.snd")) != ind
    snd_display("oboe: index %d != %d?", res, ind)
  end
  [[:sound?, true],
    [:chans, 1],
    [:channels, 1],
    [:frames, 50828],
    [:srate, 22050],
    [:data_location, 28],
    [:data_size, 50828 * 2],
    [:data_format, Mus_bshort],
    [:maxamp, 0.14724],
    [:comment, ""],].each do |func, val|
    notequal = case val
               when Float
                 lambda do |a, b| fneq(a, b) end
               else
                 lambda do |a, b| a != b end
               end
    if notequal.call(res = send(func, ind), val)
      snd_display("oboe: %s %s?", func, res.inspect)
    end
  end
  if (res = short_file_name(ind)) != "oboe.snd"
    snd_display("oboe: short name: %s?", res.inspect)
  end
  if (res = count_matches(lambda do |y| y > 0.125 end)) != 1313
    snd_display("oboe: count_matches %d?", res)
  end
  let(find(lambda do |y| y > 0.13 end)) do |spot|
    if (not spot.kind_of?(Array)) or spot[1] != 8862
      snd_display("find: %s?", spot.inspect)
    end
  end
  set_right_sample(3000)
  if ((res = right_sample) - 3000).abs > 1
    snd_display("right_sample: %d?", res)
  end
  set_left_sample(1000)
  if ((res = left_sample) - 1000).abs > 1
    snd_display("left_sample: %d?", res)
  end
  let(edits) do |eds|
    snd_display("edits: %s?", eds.inspect) if eds[0].nonzero? or eds[1].nonzero?
    snd_display("edit_position: %s %s?", edit_position, eds.inspect) if edit_position != eds[0]
  end
  play_and_wait(0, ind, 0)
  bomb(ind, false)
  select_all(ind, 0)
  let(regions.first) do |r0|
    snd_display("selection?") unless selection?
    snd_display("region?") unless region?(r0)
    if (res = selection_chans) != 1
      snd_display("selection_chans (1): %d?", res)
    end
    if (res = selection_srate) != srate(ind)
      snd_display("selection_srate: %d %d?", res, srate(ind))
    end
    if fneq(res = region_maxamp(r0), maxamp(ind))
      snd_display("region_maxamp (1): %f?", res)
    end
    if fneq(res = selection_maxamp(ind, 0), maxamp(ind))
      snd_display("selection_maxamp (1): %f?", res)
    end
    save_region(r0, "temp.dat")
    if File.exist?("temp.dat")
      File.unlink("temp.dat")
    else
      snd_display("save_region file disappeared?")
    end
    play_region(r0, true)   # needs to be true here or it never gets run
    snd_display("regions: %s", regions.inspect) if regions.length != ($test04 ? 2 : 1)
    unless (res = selection_member?(ind))
      snd_display("selection_member?: %s?", res)
    end
    if (res = selection_frames) != 50828
      snd_display("selection_frames: %d?", res)
    end
    if (res = selection_position).nonzero?
      snd_display("selection_position: %d?", res)
    end
    if (res = region_position(r0, 0)).nonzero?
      snd_display("region_position: %d?", res)
    end
    if fneq(res = selection_maxamp(ind, 0), maxamp(ind))
      snd_display("selection_maxamp: %f?")
    end
    [[:region_srate, 22050],
      [:region_chans, 1],
      [:region_frames, 50828],
      [:region_maxamp, maxamp(ind)]].each do |func, val|
      notequal = case val
                 when Float
                   lambda do |a, b| fneq(a, b) end
                 else
                   lambda do |a, b| a != b end
                 end
      if notequal.call(res = send(func, r0), val)
        snd_display("%s: %s?", func, res.inspect)
      end
    end
    samps1 = samples2vct(0, 50827, ind, 0)
    samps2 = region2vct(0, 50828, r0, 0)
    rd = make_sample_reader(0, ind, 0, 1)
    snd_display("%s not sample_reader?", rd.inspect) unless sample_reader?(rd)
    if (res = sample_reader_position(rd)).nonzero?
      snd_display("initial sample_reader_position: %d?", res)
    end
    if (res = sample_reader_home(rd)) != [ind, 0]
      snd_display("sample_reader_home: %s %s?", res.inspect, [ind, 0].inspect)
    end
    snd_display("%s init at end?", rd.inspect) if sample_reader_at_end?(rd)
    if (res = snd_catch do region2vct(-1, 1233, r0) end.first) != :no_such_sample
      snd_display("region2vct -1: %s", res.inspect)
    end
    if res = snd_catch do region2vct(12345678, 1, r0) end.first
      snd_display("region2vct 12345678: %s", res.inspect)
    end
    if (res = format("%s", rd.inspect)) != "#<sample-reader: oboe.snd[0: 0] from 0, at 0>"
      snd_display("sample_reader actually got: %s", res.inspect)
    end
    erd = rd
    snd_display("sample_reader equal? %s %s", rd, erd) if erd != rd
    50827.times do |i|
      val = (i % 2).nonzero? ? next_sample(rd) : read_sample(rd)
      if val != samps1[i] or val != samps2[i]
        snd_display("readers disagree at %d (%f %f %f)", i, val, samps1[i], samps2[i])
        break
      end
    end
    free_sample_reader(rd)
  end
  if (res = snd_catch do make_sample_reader(0, ind, -1) end.first) != :no_such_channel
    snd_display("make_sample_reader bad chan -1: %s?", val.inspect)
  end
  if (res = snd_catch do make_sample_reader(0, ind, 1) end.first) != :no_such_channel
    snd_display("make_sample_reader bad chan 1: %s?", val.inspect)
  end
  let(make_sample_reader(0)) do |fd|
    snd_display("sample_reader: mix %s?", fd.inspect) if mix_sample_reader?(fd)
    snd_display("sample_reader: region %s?", fd.inspect) if region_sample_reader?(fd)
    snd_display("sample_reader: track %s?", fd.inspect) if track_sample_reader?(fd)
    snd_display("sample_reader: normal %s?", fd.inspect) unless sample_reader?(fd)
    snd_display("sample_reader: position %s?", fd.inspect) if sample_reader_position(fd).nonzero?
    free_sample_reader(fd)
    if (res = format("%s", fd.inspect))[-16, 16] != "at eof or freed>"
      snd_display("freed sample_reader: %s [%s]?", res, res[-16, 16])
    end
  end
  if (res = snd_catch do
        reg = regions.first
        make_region_sample_reader(0, reg, region_chans(reg) + 1)
      end.first) != :no_such_channel
    snd_display("make_region_sample_reader bad chan (2): %s %s", res.inspect, regions.inspect)
  end
  revert_sound(ind)
  insert_sample(100, 0.5, ind)
  if (res = snd_catch do insert_sound("oboe.snd", 0, 1) end.first) != :no_such_channel
    snd_display("insert_sound bad chan (1): %s", res.inspect)
  end
  if (res = snd_catch do insert_sample(-12, 1.0) end.first) != :no_such_sample
    snd_display("insert_sample bad pos: %s", res.inspect)
  end
  set_show_axes(Show_no_axes, ind, 0)
  update_transform_graph(ind)
  update_time_graph(ind)
  if fneq(res1 = sample(100), 0.5) or (res2 = frames(ind)) != 50829
    snd_display("insert_sample: %f %d", res1, res2)
  end
  v0 = Array.new(3, 0.25)
  v1 = make_vct(3, 0.75)
  insert_samples(200, 3, v0, ind)
  insert_samples(300, 3, v1, ind)
  if fneq(res1 = sample(201), 0.25) or fneq(res2 = sample(301), 0.75) or frames(ind) != 50835
    snd_display("insert_samples: %f %f %d?", res1, res2, frames(ind))
  end
  save_sound_as("hiho.snd", ind, Mus_next, Mus_bshort, :srate, 22050)
  nind = view_sound("hiho.snd")
  if fneq(res1 = sample(101, nind), res2 = sample(101, ind))
    snd_display("save_sound_as: %f %f?", res1, res2)
  end
  snd_display("read_only view_sound: %s?", read_only(nind).inspect) unless read_only(nind)
  set_speed_control_style(Speed_control_as_semitone, nind)
  if (res = speed_control_style(nind)) != Speed_control_as_semitone
    snd_display("speed_control_style set semi: %d?", res)
  end
  set_speed_control_tones(-8, nind)
  if (res = speed_control_tones(nind)) != 12
    snd_display("speed_control_tones -8: %d?", res)
  end
  set_speed_control_tones(18, nind)
  if (res = speed_control_tones(nind)) != 18
    snd_display("speed_control_tones 18: %d?", res)
  end
  graph2ps("aaa.eps")
  close_sound(nind)
  revert_sound(ind)
  set_sample(50, 0.5, ind)
  snd_display("set_sample: %f?", sample(50)) if fneq(sample(50), 0.5)
  set_samples(60, 3, Array.new(3, 0.25), ind)
  if fneq(sample(60), 0.25) or fneq(sample(61), 0.25)
    snd_display("set_samples: %f %f?", sample(60), sample(61))
  end
  set_samples(10, 3, [0.1, 0.2, 0.3], ind)
  unless vequal(samples2vct(10, 3, ind), [0.1, 0.2, 0.3])
    snd_display("set_samples via list: %s?", samples2vct(10, 3, ind).inspect)
  end
  revert_sound(ind)
  save_sound_as("temporary.snd", ind)
  set_samples(100000, 20000, "temporary.snd", ind)
  unless vequal(res1 = samples2vct(110000, 10), res2 = samples2vct(10000, 10))
    snd_display("set_samples to self: %s %s?", res1.inspect, res2.inspect)
  end
  revert_sound(ind)
  delete_file("temporary.snd")
  delete_sample(100, ind)
  snd_display("delete_sample: %d?", frames(ind)) if frames(ind) != 50827
  delete_samples(0, 100, ind)
  snd_display("delete_samples: %d?", frames(ind)) if frames(ind) != 50727
  revert_sound(ind)
  maxa = maxamp(ind)
  scale_to(0.5, ind)
  newmaxa = maxamp(ind)
  snd_display("scale_to: %f?", newmaxa) if fneq(newmaxa, 0.5)
  undo(1, ind)
  scale_by(2.0, ind)
  newmaxa = maxamp(ind)
  snd_display("scale_by: %f?", newmaxa) if fneq(newmaxa, 2.0 * maxa)
  revert_sound(ind)
  scale_by(-1, ind)
  mix("oboe.snd")
  snd_display("invert+mix -> %f?", maxamp) if fneq(maxamp(ind, 0), 0.0)
  revert_sound(ind)
  select_all(ind)
  snd_display("regions (2): %s", regions.inspect) if regions.length != ($test04 ? 3 : 2)
  scale_selection_to(0.5)
  newmaxa = maxamp(ind)
  snd_display("scale_selection_to: %f?", newmaxa) if fneq(newmaxa, 0.5)
  revert_sound(ind)
  select_all(ind)
  scale_selection_by(2.0)
  newmaxa = maxamp(ind)
  snd_display("scale_selection_by: %f?", newmaxa) if fneq(newmaxa, 2.0 * maxa)
  revert_sound(ind)
  select_all(ind)
  rread = make_region_sample_reader(0, regions.first)
  sread = make_sample_reader(0, ind)
  rvect = region2vct(0, 100, regions.first)
  svect = samples(0, 100, ind)
  if fneq(res = region_sample(1, regions.first), rvect[1])
    snd_display("region_sample: %f %f?", res, rvect[1])
  end
  100.times do |i|
    rval = next_sample(rread)
    sval = next_sample(sread)
    snd_display("sample_read: %f %f?", rval, sval) if fneq(rval, sval)
    snd_display("region_samples: %f %f?", rval, rvect[i]) if fneq(rval, rvect[i])
    snd_display("samples: %f %f?", sval, svect[i]) if fneq(sval, svect[i])
  end
  free_sample_reader(rread)
  val0 = next_sample(sread)
  snd_display("premature end?") if sample_reader_at_end?(sread)
  previous_sample(sread)
  val1 = previous_sample(sread)
  snd_display("previous_sample: %f %f?", val0, val1) if fneq(val0, val1)
  free_sample_reader(rread)
  revert_sound(ind)
  s100 = sample(100)
  s40 = sample(40)
  len = frames
  addlen = mus_sound_frames("fyow.snd")
  set_cursor_style(Cursor_line)
  set_cursor_size(25)
  set_cursor(50, ind)
  snd_display("cursor_style: %s?", cursor_style) if cursor_style != Cursor_line
  snd_display("cursor_size: %s?", cursor_size) if cursor_size != 25
  set_cursor_style(Cursor_cross)
  set_cursor_size(15)
  set_cursor(30, ind, 0)
  set_cursor_style(Cursor_line)
  set_cursor_size(20)
  set_cursor(20, ind, 0)
  set_cursor_style(lambda do |snd, chn, ax|
                     x, y = cursor_position
                     size = (cursor_size / 2.0).round
                     draw_line(x - size, y - size, x + size, y + size, snd, chn, Cursor_context)
                     draw_line(x - size, y + size, x + size, y - size, snd, chn, Cursor_context)
                   end,
                   ind, 0)
  unless (res = cursor_style(ind, 0)).kind_of?(Proc)
    snd_display("set_cursor_style to Proc: %s", res.inspect)
  end
  set_cursor(50, ind)
  insert_sound("fyow.snd", cursor, 0, ind, 0)
  if fneq(sample(40), s40) or sample(100) == s100 or ffneq(sample(100), 0.001831)
    snd_display("insert_sound: %f %f %f %f?", sample(40), s40, sample(100), s100)
  end
  snd_display("insert_sound len: %d", frames) if frames != (addlen + len)
  save_sound_as("not-temporary.snd")
  insert_samples(0, 100, "not-temporary.snd")
  set_cursor(frames(ind, 0, 0) - 2, ind, 0, 0)
  revert_sound
  if (res1 = cursor(ind, 0)) != (res2 = frames(ind, 0, 0) - 2)
    snd_display("set edpos cursor: %d %d %d?", cursor, res1, res2)
  end
  delete_file("not-temporary.snd")
  id = make_region(0, 99)
  insert_region(60, id, ind)
  snd_display("insert_region len: %d?", frames) if frames != (len + 100)
  snd_display("insert_region: %f %f?", sample(100), s40) if fneq(sample(100), s40)
  if (res = snd_catch do insert_region(0, 1000 + regions.max) end.first) != :no_such_region
    snd_display("insert_region bad id: %s", res.inspect)
  end
  save_region(id, "fmv.snd")
  [[:mus_sound_header_type, Mus_next],
    [:mus_sound_data_format, Mus_out_format],
    [:mus_sound_srate, region_srate(id)],
    [:mus_sound_chans, region_chans(id)],
    [:mus_sound_frames, region_frames(id)],
  ].each do |func, var|
    if (res = send(func, "fmv.snd")) != var
      snd_display("save_region %s: %s (%s)?", func, res.inspect, val.inspect)
    end
  end
  snd_display("save_region position: %d", region_position(id, 0)) if region_position(id, 0).nonzero?
  delete_file("fmv.snd")
  save_region(id, "fmv.snd", Mus_riff, Mus_lshort, "this is a comment")
  [[:mus_sound_header_type, Mus_riff],
    [:mus_sound_data_format, Mus_lshort],
    [:mus_sound_comment, "this is a comment"],
    [:mus_sound_frames, region_frames(id)],
  ].each do |func, var|
    if (res = send(func, "fmv.snd")) != var
      snd_display("save_region %s: %s (%s)?", func, res.inspect, val.inspect)
    end
  end
  delete_file("fmv.snd")
  save_region(id,
              :file, "fmv.snd",
              "header-type".intern, Mus_riff,
              "data-format".intern, Mus_lshort,
              :comment, "this is a comment")
  [[:mus_sound_header_type, Mus_riff],
    [:mus_sound_data_format, Mus_lshort],
    [:mus_sound_comment, "this is a comment"],
    [:mus_sound_frames, region_frames(id)],
  ].each do |func, var|
    if (res = send(func, "fmv.snd")) != var
      snd_display("save_region opt %s: %s (%s)?", func, res.inspect, val.inspect)
    end
  end
  delete_file("fmv.snd")
  save_region(id,
              :comment, "this is a comment",
              :file, "fmv.snd",
              "data-format".intern, Mus_lshort,
              "header-type".intern, Mus_riff)
  [[:mus_sound_header_type, Mus_riff],
    [:mus_sound_data_format, Mus_lshort],
    [:mus_sound_comment, "this is a comment"],
    [:mus_sound_frames, region_frames(id)],
  ].each do |func, var|
    if (res = send(func, "fmv.snd")) != var
      snd_display("save_region opt1 %s: %s (%s)?", func, res.inspect, val.inspect)
    end
  end
  delete_file("fmv.snd")
  save_region(id, "fmv.snd", "data-format".intern, Mus_bshort)
  [[:mus_sound_header_type, Mus_next],
    [:mus_sound_data_format, Mus_bshort],
    [:mus_sound_frames, region_frames(id)],
  ].each do |func, var|
    if (res = send(func, "fmv.snd")) != var
      snd_display("save_region opt2 %s: %s (%s)?", func, res.inspect, val.inspect)
    end
  end
  delete_file("fmv.snd")
  delete_file("aaa.eps")
  close_sound(ind)
end

def test115
  if (res = snd_catch do new_sound("hi.snd", 0, 1, 100, 0) end.first) != :out_of_range
    snd_display("new_sound bad chan: %d?", res)
  end
  # 
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 2, "unequal lens")
  insert_silence(0, 1000, ind, 1)
  if (res1 = frames(ind, 0)) != 1 or (res2 = frames(ind, 1)) != 1001
    snd_display("silence 1: %d %d?", res1, res2)
  end
  save_sound(ind)
  if (res1 = frames(ind, 0)) != 1001 or (res2 = frames(ind, 1)) != 1001
    snd_display("saved silence 1: %d %d?", res1, res2)
  end
  if (res = mus_sound_frames("fmv.snd")) != 1001
    snd_display("saved frames silence 1: %d?", res)
  end
  v0 = samples2vct(0, 1000, ind, 0)
  v1 = samples2vct(0, 1000, ind, 1)
  snd_display("auto-pad 0: %f?", vct_peak(v0)) if fneq(vct_peak(v0), 0.0)
  snd_display("silence 0: %f?", vct_peak(v1)) if fneq(vct_peak(v1), 0.0)
  close_sound(ind)
  delete_file("fmv.snd")
  # 
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 2, "unequal lens")
  pad_channel(0, 1000, ind, 1)
  if (res1 = frames(ind, 0)) != 1 or (res2 = frames(ind, 1)) != 1001
    snd_display("silence: %d %d?", res1, res2)
  end
  v0 = samples2vct(0, 1000, ind, 0)
  v1 = samples2vct(0, 1000, ind, 1)
  snd_display("pad 0: %f?", vct_peak(v0)) if fneq(vct_peak(v0), 0.0)
  snd_display("pad 1: %f?", vct_peak(v1)) if fneq(vct_peak(v1), 0.0)
  map_channel($init_channel, 0, 2, ind, 0)
  map_channel($init_channel, 0, 1002, ind, 1)
  pad_channel(0, 1000, ind, 0, 1)
  if (res = frames(ind, 1)) != 1002
    snd_display("pad_channel ed 1: %d?", res)
  end
  close_sound(ind)
  delete_file("fmv.snd")
  # 
  ind = new_sound("fmv.snd", Mus_ircam, Mus_bshort, 22050, 2, "this is a comment")
  v0 = make_vct(128)
  vct_set!(v0, 64, 0.5)
  vct_set!(v0, 127, 0.5)
  vct2samples(0, 128, v0, ind, 0)
  make_selection(0, 126)
  smooth_selection
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(127), 0.5) or fneq(sample(120), 0.4962) or
      fneq(sample(32), 0.07431) or fneq(sample(64), 0.25308)
    snd_display("smooth_selection: %s?", v0.inspect)
  end
  revert_sound(ind)
  vct_fill!(v0, 0.0)
  vct_set!(v0, 10, 0.5)
  vct2channel(v0)
  select_all
  set_sinc_width(40)
  src_selection(0.5)
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(20), 0.5) or fneq(sample(30), 0.0) or fneq(sample(17), -0.1057)
    snd_display("src_selection: %s?", v0.inspect)
  end
  revert_sound(ind)
  vct_fill!(v0, 0.0)
  vct_set!(v0, 10, 0.5)
  vct2channel(v0, 0)
  select_all
  filter_selection([0, 0, 0.1, 1, 1, 0], 40)
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(29), 0.1945) or fneq(sample(39), -0.0137) or fneq(sample(24), -0.01986)
    snd_display("filter_selection: %s?", v0.inspect)
  end
  revert_sound(ind)
  vct_fill!(v0, 1.0)
  vct2channel(v0)
  select_all
  filter_selection(make_one_zero(:a0, 0.5, :a1, 0.0))
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(29), 0.5) or fneq(sample(39), 0.5) or fneq(sample(24), 0.5)
    snd_display("filter_selection one_zero: %s?", v0.inspect)
  end
  revert_sound(ind)
  vct_fill!(v0, 1.0)
  vct2channel(v0)
  delete_file("fmv5.snd")
  select_all
  env_selection([0, 0, 1, 1, 2, 0], 1.0)
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(64), 1.0) or fneq(sample(20), 0.3125) or fneq(sample(119), 0.127)
    snd_display("env_selection: %s?", v0.inspect)
  end
  save_selection("fmv5.snd", Mus_next, Mus_bint, 22050, "")
  revert_sound(ind)
  # 
  if (res = snd_catch do file2array("/baddy/hiho", 0, 0, 128, v0) end.first) != :no_such_file
    snd_display("file2array w/o file: %s", res.inspect)
  end
  if (res = snd_catch do file2array("fmv5.snd", 123, 0, 128, v0) end.first) != :no_such_channel
    snd_display("file2array w/o channel: %s", res.inspect)
  end
  file2array("fmv5.snd", 0, 0, 128, v0)
  if fneq(v0[64], 1.0) or fneq(v0[20], 0.3125) or fneq(v0[119], 0.127)
    snd_display("save_selection: %f %f %f %s?", v0[64], v0[20], v0[119], v0.inspect)
  end
  if (res = mus_sound_data_format("fmv5.snd")) != Mus_bint
    snd_display("save_selection type: %s?", mus_data_format_name(res))
  end
  if (res = mus_sound_header_type("fmv5.snd")) != Mus_next
    snd_display("save_selection format: %s?", mus_header_type_name(res))
  end
  if (res = mus_sound_srate("fmv5.snd")) != 22050
    snd_display("save_selection srate: %d?", res)
  end
  vct_fill!(v0, 0.0)
  vct_set!(v0, 100, 0.5)
  vct_set!(v0, 2, -0.5)
  vct2channel(v0)
  select_all
  snd_catch do reverse_selection end
  save_selection("fmv4.snd", Mus_riff, Mus_lfloat, 44100, "this is a comment")
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(27), 0.5) or fneq(sample(125), -0.5)
    snd_display("reverse_selection: %s?", v0.inspect)
  end
  file2array("fmv4.snd", 0, 0, 128, v0)
  if fneq(sample(27), 0.5) or fneq(sample(125), -0.5)
    snd_display("save reverse_selection: %s?", v0.inspect)
  end
  if (res = mus_sound_header_type("fmv4.snd")) != Mus_riff
    snd_display("save_selection type 1: %s", mus_header_type_name(res))
  end
  if (res = mus_sound_data_format("fmv4.snd")) != Mus_lfloat
    snd_display("save_selection format 1: %s", mus_data_format_name(res))
  end
  if (res = mus_sound_srate("fmv4.snd")) != 44100
    snd_display("save_selection srate 1: %d", res)
  end
  if (res = mus_sound_comment("fmv4.snd")) != "this is a comment"
    snd_display("save_selection comment: %s", res)
  end
  delete_file("fmv4.snd")
  # 
  save_selection(:file, "fmv4.snd",
                 "header-type".intern, Mus_riff,
                 "data-format".intern, Mus_lfloat,
                 :srate, 44100,
                 :comment, "this is a comment")
  if (res = mus_sound_header_type("fmv4.snd")) != Mus_riff
    snd_display("save_selection opt type 1: %s", mus_header_type_name(res))
  end
  if (res = mus_sound_data_format("fmv4.snd")) != Mus_lfloat
    snd_display("save_selection opt format 1: %s", mus_data_format_name(res))
  end
  if (res = mus_sound_srate("fmv4.snd")) != 44100
    snd_display("save_selection opt srate 1: %d", res)
  end
  if (res = mus_sound_comment("fmv4.snd")) != "this is a comment"
    snd_display("save_selection opt comment: %s", res)
  end
  delete_file("fmv4.snd")
  # 
  save_selection(:file, "fmv4.snd", "data-format".intern, Mus_bfloat, :channel, 0)
  if (res = mus_sound_header_type("fmv4.snd")) != Mus_next
    snd_display("save_selection opt1 type 1: %s", mus_header_type_name(res))
  end
  if (res = mus_sound_data_format("fmv4.snd")) != Mus_bfloat
    snd_display("save_selection opt1 format 1: %s", mus_data_format_name(res))
  end
  if (res = mus_sound_chans("fmv4.snd")) != 1
    snd_display("save_selection opt1 chans: %d", res)
  end
  delete_file("fmv4.snd")
  revert_sound(ind)
  vct_fill!(v0, 0.0)
  vct_set!(v0, 2, 1.0)
  v1 = make_vct(256)
  128.times do |i| v1[i] = v0[i] end
  vct2channel(v1)
  select_all
  convolve_selection_with("fmv5.snd", 0.5)
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(66), -0.5)
    snd_display("convolve_selection_with: %f %f %s?", v0[66], sample(66), v0.inspect)
  end
  close_sound(ind)
  delete_file("fmv.snd")
end

def test125
  obind = open_sound("oboe.snd")
  vol = maxamp(obind)
  dur = frames
  set_amp_control(2.0, obind)
  if (res = fffneq(amp_control(obind), 2.0))
    snd_display("set_amp_control: %f?", res)
  end
  reset_controls(obind)
  if (res = ffneq(amp_control(obind), 1.0))
    snd_display("reset amp_control: %f?", res)
  end
  set_amp_control_bounds([0.0, 4.0], obind)
  if (res = amp_control_bounds(obind)) != [0.0, 4.0]
    snd_display("amp_control_bounds: %s?", res.inspect)
  end
  set_amp_control(2.0, obind)
  if (res = snd_catch do apply_controls(obind) end.first) == :no_such_sound
    snd_display("apply_controls: can't find oboe.snd? %s", res.inspect)
  end
  newamp = maxamp(obind)
  snd_display("apply amp: %f -> %f?", vol, newamp) if (2.0 * vol - newamp).abs > 0.05
  set_amp_control_bounds([0.0, 8.0], obind)
  set_speed_control_bounds([1.0, 5.0], obind)
  if (res = speed_control_bounds(obind)) != [1.0, 5.0]
    snd_display("speed_control_bounds: %s?", res)
  end
  set_speed_control(0.5, obind)
  set_speed_control_bounds([0.05, 20.0], obind)
  add_mark(1234)
  apply_controls(obind)
  newdur = frames(obind)
  set_speed_control(1.0, obind)
  snd_display("apply speed: %d -> %d?", dur, newdur) unless newdur - 2.0 * dur < 256
  set_contrast_control?(true, obind)
  set_contrast_control_bounds([0.5, 2.5], obind)
  if (res = contrast_control_bounds(obind)) != [0.5, 2.5]
    snd_display("contrast_control_bounds: %s?", res)
  end
  set_contrast_control(1.0, obind)
  apply_controls(obind)
  set_contrast_control_bounds([0.0, 10.0], obind)
  if (res = contrast_control_bounds(obind)) != [0.0, 10.0]
    snd_display("contrast_control_bounds (2): %s?", res)
  end
  secamp = maxamp(obind)
  secdur = frames(obind)
  snd_display("apply contrast: %f?", secamp) if fneq(secamp, 0.989)
  snd_display("apply contrast length: %d -> %d?", newdur, secdur) if secdur != newdur
  undo(3, obind)
  set_reverb_control?(true, obind)
  set_reverb_control_scale_bounds([0.0, 1.0], obind)
  if (res = reverb_control_scale_bounds(obind)) != [0.0, 1.0]
    snd_display("reverb_control_scale_bounds: %s?", res.inspect)
  end
  set_reverb_control_length_bounds([0.0, 2.0], obind)
  if (res = reverb_control_length_bounds(obind)) != [0.0, 2.0]
    snd_display("reverb_control_length_bounds: %s?", res.inspect)
  end
  set_reverb_control_scale(0.2, obind)
  apply_controls(obind)
  revamp = maxamp(obind)
  revdur = frames(obind)
  snd_display("apply reverb scale: %f?", revamp) if ffneq(revamp, 0.214)
  unless revdur - ((reverb_control_decay * 22050).round + 50828) < 256
    snd_display("apply reverb length: %d?", revdur)
  end
  undo(1, obind)
  set_expand_control?(true, obind)
  set_expand_control_bounds([1.0, 3.0], obind)
  if (res = expand_control_bounds(obind)) != [1.0, 3.0]
    snd_display("expand_control_bounds: %s?", res.inspect)
  end
  set_expand_control(1.5, obind)
  apply_controls(obind)
  expamp = maxamp(obind)
  expdur = frames(obind)
  snd_display("apply expand_control scale: %f?", expamp) if (expamp - 0.152).abs > 0.01
  snd_display("apply expand_control length: %d?", expdur) unless expdur > 1.25 * 50828
  set_expand_control_bounds([0.001, 20.0], obind)
  undo(1, obind)
  set_filter_control?(true, obind)
  set_filter_control_order(40, obind)
  set_filter_control_envelope([0, 0.0, 1, 0.5, 1, 0.0], obind)
  apply_controls(obind)
  fltamp = maxamp(obind)
  fltdur = frames(obind)
  snd_display("apply filter scale: %f?", fltamp) if (fltamp - 0.01).abs > 0.03 # orig 0.005
  snd_display("apply filter length: %d?", fltdur) if fltdur - (40 + 50828) > 256
  undo(1, obind)
  revert_sound(obind)
  make_selection(1000, 1000)
  scale_selection_to(0.1)
  scale_selection_by(2.0)
  make_selection(2000, 2001)
  scale_selection_by(2.0)
  scale_selection_to(0.5)
  make_selection(1000, 2001)
  scale_selection_to(0.5)
  scale_selection_by(0.5)
  make_selection(2000, 2000)
  scale_selection_by(2.0)
  scale_selection_to(0.5)
  make_selection(1000, 1001)
  scale_selection_to(0.1)
  scale_selection_by(2.0)
  make_selection(999, 2002)
  scale_selection_to(1.0)
  scale_selection_by(0.5)
  tree = edit_tree
  true_tree = [
    [0, 0, 0, 998, 1.0, 0.0, 0.0, 0],
    [999, 0, 999, 999, 0.999969720840454, 0.0, 0.0, 0],
    # [1000, 0, 1000, 1000, 6.09052181243896, 0.0, 0.0, 0],
    [1000, 0, 1000, 1000, 6.10186210646809, 0.0, 0.0, 0],
    [1001, 0, 1001, 1001, 0.999969720840454, 0.0, 0.0, 0],
    [1002, 0, 1002, 1999, 0.499984979629517, 0.0, 0.0, 0],
    # [2000, 0, 2000, 2000, 7.54652404785156, 0.0, 0.0, 0],
    [2000, 0, 2000, 2000, 7.52918206118976, 0.0, 0.0, 0],
    # [2001, 0, 2001, 2001, 3.7732629776001, 0.0, 0.0, 0],
    [2001, 0, 2001, 2001, 3.6966380866426, 0.0, 0.0, 0],
    [2002, 0, 2002, 2002, 0.999969720840454, 0.0, 0.0, 0],
    [2003, 0, 2003, 50827, 1.0, 0.0, 0.0, 0],
    [50828, -2, 0, 0, 0.0, 0.0, 0.0, 0]]
  if tree.length != true_tree.length
    snd_display("edit trees are not same length: %d %d?", tree.length, true_tree.length)
  else
    tree.each_with_index do |branch, i|
      true_branch = true_tree[i]
      if branch[0] != true_branch[0] or
          branch[1] != true_branch[1] or
          branch[2] != true_branch[2] or
          branch[3] != true_branch[3] or
          fneq(branch[4], true_branch[4])
        snd_display("edit trees disagree at %d:\n# %s\n# %s",
                    i, branch.inspect, true_branch.inspect)
      end
    end
  end
  insert_silence(1001, 8)
  insert_silence(900, 50)
  insert_silence(2005, 1)
  insert_silence(999, 2)
  tree = edit_tree
  true_tree = [
    [0, 0, 0, 899, 1.0, 0.0, 0.0, 0],
    [900, -1, 0, 49, 0.0, 0.0, 0.0, 0],
    [950, 0, 900, 948, 1.0, 0.0, 0.0, 0],
    [999, -1, 0, 1, 0.0, 0.0, 0.0, 0],
    [1001, 0, 949, 998, 1.0, 0.0, 0.0, 0],
    [1051, 0, 999, 999, 0.999969720840454, 0.0, 0.0, 0],
    # [1052, 0, 1000, 1000, 6.09052181243896, 0.0, 0.0, 0],
    [1052, 0, 1000, 1000, 6.10186210646809, 0.0, 0.0, 0],
    [1053, -1, 0, 7, 0.0, 0.0, 0.0, 0],
    [1061, 0, 1001, 1001, 0.999969720840454, 0.0, 0.0, 0],
    [1062, 0, 1002, 1946, 0.499984979629517, 0.0, 0.0, 0],
    [2007, -1, 0, 0, 0.0, 0.0, 0.0, 0],
    [2008, 0, 1947, 1999, 0.499984979629517, 0.0, 0.0, 0],
    # [2061, 0, 2000, 2000, 7.54652404785156, 0.0, 0.0, 0],
    [2061, 0, 2000, 2000, 7.52918206118976, 0.0, 0.0, 0],
    # [2062, 0, 2001, 2001, 3.7732629776001, 0.0, 0.0, 0],
    [2062, 0, 2001, 2001, 3.6966380866426, 0.0, 0.0, 0],
    [2063, 0, 2002, 2002, 0.999969720840454, 0.0, 0.0, 0],
    [2064, 0, 2003, 50827, 1.0, 0.0, 0.0, 0],
    [50889, -2, 0, 0, 0.0, 0.0, 0.0, 0]]
  if tree.length != true_tree.length
    snd_display("silenced edit trees are not same length: %d %d?", tree.length, true_tree.length)
  else
    tree.each_with_index do |branch, i|
      true_branch = true_tree[i]
      if branch[0] != true_branch[0] or
          branch[1] != true_branch[1] or
          branch[2] != true_branch[2] or
          branch[3] != true_branch[3] or
          fneq(branch[4], true_branch[4])
        snd_display("silenced edit trees disagree at %d:\n# %s\n# %s",
                    i, branch.inspect, true_branch.inspect)
      end
    end
  end
  if fneq(sample(998), -0.03) or fneq(sample(999), 0.0) or
      fneq(sample(1000), 0.0) or fneq(sample(1001), -0.03)
    snd_display("insert_silence [999 for 2]: %f %f %f %f?",
                sample(998), sample(999), sample(1000), sample(1001))
  end
  if fneq(sample(2006), -0.033) or fneq(sample(2007), 0.0) or fneq(sample(2008), -0.033)
    snd_display("insert_silence [2007 for 1]: %f %f %f?", sample(2006), sample(2007), sample(2008))
  end
  revert_sound(obind)
  add_mark(1200, obind, 0)
  mark_num = marks(obind, 0).length
  scale_by(2.0, obind, 0)
  mark_now = marks(obind, 0).length
  snd_display("mark lost after scaling?") if mark_num != mark_now
  set_selection_position(0)
  set_selection_frames(100)
  scale_selection_to(0.5)
  mark_now = marks(obind, 0).length
  snd_display("mark lost after scaling scaling?") if mark_num != mark_now
  m1 = add_mark(1000)
  set_cursor(100, obind, 0)
  key(?u, 4, obind)
  key(?1, 0, obind)
  key(?0, 0, obind)
  key(?0, 0, obind)
  key(?o, 4, obind)
  snd_display("mark after zeros: %d (1100)?", mark_sample(m1)) if mark_sample(m1) != 1100
  revert_sound(obind)
  frs = frames(obind)
  make_region(0, 999, obind, 0)
  snd_display("make_region but no selection? %s", selection?.inspect) unless selection?
  delete_selection
  snd_display("delete_selection: %d?", frames(obind)) if frames(obind) != frs - 1000
  val = sample(0, obind, 0)
  undo
  snd_display("delete_selection val: %f %f?", val, sample(1000)) if fneq(sample(1000), val)
  insert_selection
  if (res = snd_catch do insert_selection(0, obind, 123) end.first) != :no_such_channel
    snd_display("insert_selection bad chan: %s?", res.inspect)
  end
  if (res = snd_catch do mix_selection(0, obind, 123) end.first) != :no_such_channel
    snd_display("mix_selection bad chan: %s?", res.inspect)
  end
  snd_display("insert_selection: %d?", frames(obind)) if frames(obind) != frs + 1000
  snd_display("insert_selection val: %f %f?", val, sample(2000)) if fneq(sample(2000), val)
  val = sample(900)
  mix_selection
  snd_display("mix_selection val: %f %f?", val * 2.0, sample(900)) if fneq(sample(900), val * 2.0)
  snd_display("mix_selection: %d?", frames(obind)) if frames(obind) != frs + 1000
  close_sound(obind)
end

def test135
  ind = open_sound("2.snd")
  apply_to_sound = 0
  apply_to_channel = 1
  apply_to_selection = 2
  len = frames(ind)
  set_sync(1, ind)
  set_speed_control(0.5, ind)
  apply_controls(ind, apply_to_sound)             # temp 1
  if (frames - 2 * len).abs > 256
    snd_display("apply srate 0.5: %d %d?", frames, len * 2)
  end
  make_selection(0, frames)
  set_speed_control(0.5, ind)
  apply_controls(ind, apply_to_selection)         # temp 2
  if (frames - 4 * len).abs > 256
    snd_display("apply srate 0.5 to selection: %d %d?", frames, len * 4)
  end
  env_sound([0, 0, 1, 1], 0, frames, 32.0)        # temp 3
  reg = select_all                                # make multi_channel region
  insert_region(0, reg)                           # temp 4
  insert_selection(0)                             # temp 5
  revert_sound(ind)
  set_speed_control(0.5)
  set_sync(0, ind)
  set_selected_channel(ind, 1)
  apply_controls(ind, apply_to_channel)
  if (frames(ind, 1) - 2 * len).abs > 256
    snd_display("apply srate 0.5 to chan 1: %d %d?", frames(ind, 1), len * 2)
  end
  if frames(ind, 0) != len
    snd_display("apply srate 0.5 but chan 0: %d %d?", frames(ind, 0), len)
  end
  set_speed_control(0.5, ind)
  apply_controls(ind, apply_to_sound, 1000)
  make_selection(2000, 4000)
  set_speed_control(0.5, ind)
  apply_controls(ind, apply_to_selection)
  set_selected_channel(ind, false)
  snd_display("selected_channel false: %s?", selected_channel(ind).inspect) if selected_channel(ind)
  close_sound(ind)
  #
  ind1 = open_sound("oboe.snd")
  mx1 = maxamp(ind1, 0)
  ind2 = open_sound("2.snd")
  mx20 = maxamp(ind2, 0)
  mx21 = maxamp(ind2, 1)
  select_sound(ind1)
  scale_sound_by(2.0)
  if fneq(res = maxamp(ind1, 0), 2.0 * mx1)
    snd_display("scale_sound_by 2.0: %f %f?", mx1, res)
  end
  if (res1 = edit_fragment(1, ind1, 0)) !=
      (res2 = ["scale_channel(2.000, 0, false", "scale", 0, 50828])
    snd_display("scale_sound_by:\n# %s\n# %s", res1.inspect, res2.inspect)
  end
  scale_sound_to(0.5)
  if fneq(res = maxamp(ind1, 0), 0.5)
    snd_display("scale_sound_to 0.5: %f?", res)
  end
  if (res1 = edit_fragment(2, ind1, 0)) !=
      (res2 = ["scale_channel(1.698, 0, false", "scale", 0, 50828])
    snd_display("scale_sound_to:\n# %s\n# %s", res1.inspect, res2.inspect)
  end
  scale_sound_by(0.0, 0, 1000, ind1, 0)
  if fneq(res = maxamp(ind1, 0), 0.5)
    snd_display("scale_sound_by 0.0: %f?", res)
  end
  if (res1 = edit_fragment(3, ind1, 0)) !=
      (res2 = ["scale_channel(0.000, 0, 1000", "scale", 0, 1000])
    snd_display("scale_sound_by 0.0:\n# %s\n# %s", res1.inspect, res2.inspect)
  end
  v = samples2vct(0, 1000, ind1, 0)
  if fneq(res = vct_peak(v), 0.0)
    snd_display("scale_sound_by 0.0 [0:1000]: %f", res)
  end
  revert_sound(ind1)
  oldv = samples2vct(12000, 10, ind1, 0)
  scale_sound_by(2.0, 12000, 10, ind1, 0)
  newv = samples2vct(12000, 10, ind1, 0)
  10.times do |i|
    if fneq(res1 = oldv[i] * 2.0, res2 = newv[i])
      snd_display("scale %d: %f %f?", i, res1, res2)
    end
  end
  if (res1 = edit_fragment(1, ind1, 0)) !=
      (res2 = ["scale_channel(2.000, 12000, 10", "scale", 12000, 10])
    snd_display("scale_sound_by 2.0 [12000:10]:\n# %s\n# %s", res1.inspect, res2.inspect)
  end
  revert_sound(ind1)
  # 
  select_sound(ind2)
  scale_sound_by(2.0)
  if fneq(res = maxamp(ind2, 0), 2.0 * mx20)
    snd_display("2:0 scale_sound_by 2.0: %f %f?", mx20, res)
  end
  if fneq(res = maxamp(ind2, 1), 2.0 * mx21)
    snd_display("2:1 scale_sound_by 2.0: %f %f?", mx21, res)
  end
  if (res1 = edit_fragment(1, ind2, 0)) !=
      (res2 = ["scale_channel(2.000, 0, false", "scale", 0, 50828])
    snd_display("2 scale_sound_by:\n# %s\n# %s", res1.inspect, res2.inspect)
  end
  scale_sound_to(0.5)
  if fneq(res = [maxamp(ind2, 0), maxamp(ind2, 1)].max, 0.5)
    snd_display("2 scale_sound_to 0.5: %f %s?", res, maxamp(ind2).inspect)
  end
  scale_sound_by(0.0, 0, 1000, ind2, 1)
  if fneq(res = maxamp(ind2, 0), 0.5)
    snd_display("2 scale_sound_by 0.0: %f?", res)
  end
  if (res1 = edit_fragment(3, ind2, 1)) !=
      (res2 = ["scale_channel(0.000, 0, 1000", "scale", 0, 1000])
    snd_display("2:1 scale_sound_by 0.0:\n# %s\n# %s", res1.inspect, res2.inspect)
  end
  v = samples2vct(0, 1000, ind2, 1)
  if fneq(res = vct_peak(v), 0.0)
    snd_display("2:1 scale_sound_by 0.0 [0:1000]: %f", res)
  end
  revert_sound(ind2)
  oldv = samples2vct(12000, 10, ind2, 0)
  scale_sound_by(2.0, 12000, 10, ind2, 0)
  newv = samples2vct(12000, 10, ind2, 0)
  10.times do |i|
    if fneq(res1 = oldv[i] * 2.0, res2 = newv[i])
      snd_display("2 scale %d: %f %f?", i, res1, res2)
    end
  end
  revert_sound(ind2)
  #
  set_sync(3, ind2)
  set_sync(3, ind1)
  scale_sound_by(2.0)
  if fneq(res = maxamp(ind1, 0), mx1)
    snd_display("sync scale_sound_by 2.0: %f %f?", mx1, res)
  end
  if fneq(res = maxamp(ind2, 0), 2.0 * mx20)
    snd_display("2:0 sync scale_sound_by 2.0: %f %f?", mx20, res)
  end
  if fneq(res = maxamp(ind2, 1), 2.0 * mx21)
    snd_display("2:1 sync scale_sound_by 2.0: %f %f?", mx21, res)
  end
  scale_sound_to(1.0, 20000, 40000, ind2, 1)
  if fneq(res = maxamp(ind1, 0), mx1)
    snd_display("sync scale_sound_to 1.0: %f %f?", mx1, res)
  end
  if fneq(res = maxamp(ind2, 0), 2.0 * mx20)
    snd_display("2:0 sync scale_sound_to 1.0: %f %f?", mx20, res)
  end
  if fneq(res = maxamp(ind2, 1), 1.0)
    snd_display("2:1 sync scale_sound_to 1.0: %f?", res)
  end
  close_sound(ind1)
  close_sound(ind2)
end

def test145
  ind = open_sound("now.snd")
  cur_amp = amp_control(ind)
  snd_display("$snd_opened_sound: %d %d?", $snd_opened_sound, ind) if $snd_opened_sound != ind
  set_amp_control(0.5, ind)
  if fneq(res = amp_control(ind), 0.5)
    snd_display("amp_control (0.5): %f?", res)
  end
  set_amp_control(0.25, ind, 0)
  if fneq(res = amp_control(ind), 0.5)
    snd_display("amp_control after local set (0.5): %f?", res)
  end
  if fneq(res = amp_control(ind, 0), 0.25)
    snd_display("amp_control 0 (0.25): %f?", res)
  end
  set_amp_control(1.0, ind)
  if fneq(res = amp_control(ind), 1.0)
    snd_display("amp_control after local set (1.0): %f?", res)
  end
  if fneq(res = amp_control(ind, 0), 0.25)
    snd_display("amp_control 0 after set (0.25): %f?", res)
  end
  #
  set_transform_graph_type(Graph_as_sonogram, ind, 0)
  if (res = transform_frames(ind, 0)).nonzero?
    snd_display("transform_frames: %d?", res)
  end
  if (res = transform_sample(0, 0, ind, 0))
    snd_display("transform_sample (empty): %f?", res)
  end
  if (res = transform2vct(ind, 0))
    snd_display("transform2vct (empty): %s?", res.inspect)
  end  
  close_sound(ind)
  #
  ind = open_sound("4.aiff")
  if fneq(res = amp_control(ind), 1.0)
    snd_display("amp_control upon open (1.0): %f?", res)
  end
  if fneq(res = amp_control(ind, 2), 1.0)
    snd_display("amp_control 2 upon open (1.0): %f?", res)
  end
  set_amp_control(0.5, ind)
  if fneq(res = amp_control(ind, 2), 0.5)
    snd_display("amp_control 2 after global set (0.5): %f?", res)
  end
  set_amp_control(0.25, ind, 2)
  if fneq(res = amp_control(ind, 2), 0.25)
    snd_display("amp_control 2 (0.25): %f?", res)
  end
  after_ran = false
  $after_apply_hook.reset_hook!
  $after_apply_hook.add_hook!("snd-test") do |snd| after_ran = snd end
  apply_controls(ind)
  snd_display("$after_apply_hook: %d?", after_ran) if ind != after_ran
  $after_apply_hook.reset_hook!
  revert_sound(ind)
  set_sync(1, ind)
  scale_to(vct(0.1, 0.2))
  mx = maxamp(ind, true)
  if fneq(mx[0], 0.1) or fneq(mx[1], 0.2) or fneq(mx[2], 0.2) or fneq(mx[3], 0.2)
    snd_display("scale_to with vector: %s?", mx.inspect)
  end
  set_filter_control_envelope([0, 0, 1, 1], ind)
  if [0.0, 0.0, 1.0, 1.0] != filter_control_envelope(ind)
    snd_display("set_filter_control_envelope: %s?", filter_control_envelope(ind).inspect)
  end
  set_filter_control_order(20, ind)
  unless vequal(res = filter_control_coeffs(ind),
                vct(-0.007, 0.010, -0.025, 0.029, -0.050, 0.055, -0.096, 0.109, -0.268, 0.241,
                    0.241, -0.268, 0.109, -0.096, 0.055, -0.050, 0.029, -0.025, 0.010, -0.007))
    snd_display("highpass coeffs: %s?", res.inspect)
  end
  set_filter_control_envelope(filter_control_envelope(ind), ind)
  if [0.0, 0.0, 1.0, 1.0] != filter_control_envelope(ind)
    snd_display("set_filter_control_envelope to self: %s?", filter_control_envelope(ind).inspect)
  end
  set_filter_control_envelope([0, 1, 1, 0], ind)
  unless vequal(res = filter_control_coeffs(ind),
                vct(0.003, 0.002, 0.004, 0.002, 0.007, 0.003, 0.014, 0.012, 0.059, 0.394,
                    0.394, 0.059, 0.012, 0.014, 0.003, 0.007, 0.002, 0.004, 0.002, 0.003))
    snd_display("lowpass coeffs: %s?", res.inspect)
  end
  close_sound(ind)
end

def xtest155(obind)
  axinfo = axis_info(obind, 0, Time_graph)
  losamp, hisamp, x0, y0, x1, y1 = axinfo[0, 6]
  xpos = x0 + 0.5 * (x1 - x0)
  ypos = y0 + 0.75 * (y1 - y0)
  cp_x = lambda do |x|
    axinfo[10] + (x - x0) * ((axinfo[12] - axinfo[10]) / (x1 - x0))
  end
  cp_y = lambda do |y|
    axinfo[13] + (y1 - y) * ((axinfo[11] - axinfo[13]) / (y1 - y0))
  end
  select_channel(0)
  set_cursor(100, obind)
  xy = cursor_position(obind)
  if fneq(res0 = position2x(xy[0]), res1 = cursor(obind).to_f / srate(obind))
    snd_display("cursor_position: %s %f %f?", xy[0], res0, res1)
  end
  if fneq(res = position2x(x2position(xpos)), xpos)
    snd_display("x<->position: %f %f?", res, xpos)
  end
  if ((res = position2y(y2position(ypos))) - ypos).abs > 0.5
    snd_display("y<->position: %f %f?", res, ypos)
  end
  if losamp != (res = left_sample(obind, 0))
    snd_display("axis-info[0 losamp]: %s %s?", losamp, res)
  end
  if hisamp != (res = right_sample(obind, 0))
    snd_display("axis-info[1 hisamp]: %s %s?", hisamp, res)
  end
  snd_display("axis_info[6 xmin]: %f?", axinfo[6]) if fneq(axinfo[6], 0.0)
  snd_display("axis_info[7 ymin]: %f?", axinfo[7]) if fneq(axinfo[7], -1.0)
  snd_display("axis_info[9 ymax]: %f?", axinfo[9]) if fneq(axinfo[9], 1.0)
  res = our_x2position(obind, x0)
  snd_display("x0->position: %s?", res.inspect) if (res[0] - res[1]).abs > 1
  res = our_x2position(obind, x1)
  snd_display("x1->position: %s?", res.inspect) if (res[0] - res[1]).abs > 1
  res = our_x2position(obind, 0.5 * (x0 + x1))
  snd_display("xmid->position: %s?", res.inspect) if (res[0] - res[1]).abs > 1
  unless $full_test
    if ((res = x2position(xpos)) - cp_x.call(xpos)).abs > 1
      snd_display("cp_x 0.5: %s %s?", res, cp_x.call(xpos))
    end
    if ((res = y2position(ypos)) - cp_y.call(ypos)).abs > 1
      snd_display("cp_y 0.75: %s %s?", res, cp_y.call(ypos))
    end
    10.times do |i|
      xxpos = x0 + my_random(x1 - x0)
      yypos = y0 + my_random(y1 - y0)
      if ((res = x2position(xxpos)) - cp_x.call(xxpos)).abs > 1
        snd_display("cp_x[%d] %s: %s %s?", i, xxpos, res, cp_x.call(xxpos))
      end
      if ((res = y2position(yypos)) - cp_y.call(yypos)).abs > 1
        snd_display("cp_y[%d] %s: %s %s?", i, yypos, res, cp_y.call(yypos))
      end
      if fneq(res = position2x(cp_x.call(xxpos).to_i), xxpos)
        snd_display("x2position cp_x %s %s?", xxpos, res)
      end
      # INFO: fneq --> fffneq
      if fffneq(res = position2y(cp_y.call(yypos).to_i), yypos)
        snd_display("y2position cp_y %s %s?", yypos, res)
      end
    end
  end
  set_left_sample(1234, obind, 0)
  if (res = axis_info(obind, 0)[0]) != 1234
    snd_display("axis_info[0 losamp at 1234]: %s?", res)
  end
  axinfo = axis_info(obind, 0)
  x0 = axinfo[2]
  x1 = axinfo[4]
  res = our_x2position(obind, x0)
  snd_display("x0a->position: %s?", res.inspect) if (res[0] - res[1]).abs > 1
  res = our_x2position(obind, x1)
  snd_display("x1a->position: %s?", res.inspect) if (res[0] - res[1]).abs > 1
  res = our_x2position(obind, 0.5 * (x0 + x1))
  snd_display("xmida->position: %s?", res.inspect) if (res[0] - res[1]).abs > 1
  set_y_bounds([-2.0, 3.0], obind, 0)
  if fneq(res = axis_info(obind, 0)[7], -2.0)
    snd_display("axis_info[7 ymin -2.0]: %s?", res)
  end
  if fneq(res = axis_info(obind, 0)[9], 3.0)
    snd_display("axis_info[9 ymax 2.0]: %s?", res)
  end
end

def test155
  obind = open_sound("4.aiff")
  amps = maxamp(obind, true)
  set_window_width(600) if window_width < 600
  set_window_height(600) if window_height < 600
  set_x_bounds([0.0, 0.1], obind, 0)
  set_show_axes(Show_x_axis, obind, 0)
  update_time_graph
  set_amp_control(0.1, obind)
  select_channel(2)
  if (res = snd_catch do apply_controls(obind, 1) end.first) == :no_such_sound
    snd_display("apply_controls can\'t find 4.aiff: %s?", res.inspect)
  end
  newamps = maxamp(obind, true)
  if fneq(amps[0], newamps[0]) or
      fneq(amps[1], newamps[1]) or
      (0.1 * amps[2] - newamps[2]).abs > 0.05 or
      fneq(amps[3], newamps[3])
    snd_display("apply amps:\n# %s\n# %s", amps.inspect, newamps.inspect)
  end
  undo(1, obind, 2)
  set_amp_control(0.1, obind)
  make_region(0, frames(obind), obind, 1)
  snd_catch do apply_controls(obind, 2) end
  newamps = maxamp(obind, true)
  if fneq(amps[0], newamps[0]) or
      (0.1 * amps[1] - newamps[1]).abs > 0.05
      fneq(amps[2], newamps[2]) or
      fneq(amps[3], newamps[3])
    snd_display("apply selection amp:\n# %s\n# %s", amps.inspect, newamps.inspect)
  end
  xtest155(obind) unless provided? "snd-nogui"
  close_sound(obind)
end

def test165
  ind1 = open_sound("oboe.snd")
  test_orig(lambda do |snd| src_sound(2.0, 1.0, ind1) end,
            lambda do |snd| src_sound(0.5, 1.0, ind1) end, :src_sound, ind1)
  test_orig(lambda do |snd| src_channel(2.0) end,
            lambda do |snd| src_channel(0.5) end, :src_channel, ind1)
  test_orig(lambda do |snd| scale_by(2.0, ind1) end,
            lambda do |snd| scale_by(0.5, ind1) end, :scale_by, ind1)
  test_orig(lambda do |snd| scale_sound_by(2.0, ind1) end,
            lambda do |snd| scale_sound_by(0.5, ind1) end, :scale_sound_by, ind1)
  test_orig(lambda do |snd| scale_channel(2.0) end,
            lambda do |snd| scale_channel(0.5) end, :scale_channel, ind1)
  test_orig(lambda do |snd| reverse_sound(ind1) end,
            lambda do |snd| reverse_sound(ind1) end, :reverse_sound, ind1)
  test_orig(lambda do |snd| reverse_channel() end,
            lambda do |snd| reverse_channel() end, :reverse_channel, ind1)
  test_orig(lambda do |snd| env_sound([0, 1.0, 1, 2.0], ind1) end,
            lambda do |snd| env_sound([0, 1.0, 1, 0.5], ind1) end, :env_sound, ind1)
  test_orig(lambda do |snd| env_sound([0, 1.0, 1, 2.0, 2, 1.0], ind1) end,
            lambda do |snd| env_sound([0, 1.0, 1, 0.5, 2, 1.0], ind1) end, :env_sound, ind1)
  test_orig(lambda do |snd| env_channel(make_env(:envelope, [0, 1.0, 1, 2.0], :end, frames)) end,
            lambda do |snd| env_channel(make_env(:envelope, [0, 1.0, 1, 2.0], :end, frames)) end,
            :env_channel, ind1)
  test_orig(lambda do |snd| env_channel([0, 1.0, 1, 2.0]) end,
            lambda do |snd| env_channel([0, 1.0, 1, 0.5]) end, :env_channel, ind1)
  test_orig(lambda do |snd|
              env_channel(make_env(:envelope, [0, 2, 1, 2, 2, 0.5, 3, 0.5], :base, 0, :end, frames))
            end,
            lambda do |snd|
              env_channel(make_env(:envelope, [0, 0.5, 1, 0.5, 2, 2, 3, 2], :base, 0, :end, frames))
            end, :env_channel, ind1)
  test_orig(lambda do |snd| map_channel(lambda do |y| y * 2.0 end) end,
            lambda do |snd| map_channel(lambda do |y| y * 0.5 end) end, :map_channel, ind1)
  test_orig(lambda do |snd| map_channel(lambda do |y| y * 2.0 end, 1234) end,
            lambda do |snd| map_channel(lambda do |y| y * 0.5 end, 1234) end, :map_channel, ind1)
  test_orig(lambda do |snd| map_channel(lambda do |y| y * 2.0 end, 12005, 10) end,
            lambda do |snd| map_channel(lambda do |y| y * 0.5 end, 12005, 10) end,
            :map_channel, ind1)
  vect = make_vct(1)
  test_orig(lambda do |snd| map_channel(lambda do |y|
                                          vect[0] = y * 2.0
                                          vect
                                        end) end,
            lambda do |snd| old_map_channel do |y| [y * 0.5] end end,
            :old_map_channel, ind1)
  vect = make_vct(2)
  outp = false
  test_orig(lambda do |snd| map_channel(lambda do |y|
                                          vect[0] = y * 2.0
                                          vect[1] = y * 2.0
                                          vect
                                        end) end,
            lambda do |snd| map_channel(lambda do |y|
                                          outp = outp ? false : y * 0.5
                                        end) end,
            :map_channel, ind1)
  test_orig(lambda do |snd| map_chan(lambda do |y| y * 2.0 end) end,
            lambda do |snd| map_chan(lambda do |y| y * 0.5 end) end, :map_chan, ind1)
  test_orig(lambda do |snd| pad_channel(1000, 2000, ind1) end,
            lambda do |snd| delete_samples(1000, 2000, ind1)  end, :pad_channel, ind1)
  test_orig(lambda do |snd| clm_channel(make_one_zero(:a0, 2.0, :a1, 0.0)) end,
            lambda do |snd| clm_channel(make_one_zero(:a0, 0.5, :a1, 0.0)) end, :clm_channel, ind1)
  test_orig(lambda do |snd| clm_channel(make_one_pole(:a0, 2.0, :b1, 0.0)) end,
            lambda do |snd| clm_channel(make_one_pole(:a0, 0.5, :b1, 0.0)) end, :clm_channel, ind1)
  test_orig(lambda do |snd| filter_sound(make_one_zero(:a0, 2.0, :a1, 0.0)) end,
            lambda do |snd| filter_sound(make_one_zero(:a0, 0.5, :a1, 0.0)) end, :filter_sound,ind1)
  if (res = snd_catch do src_sound([0, 0, 1, 1]) end.first) != :out_of_range
    snd_display("src_sound env at 0: %s", res.inspect)
  end
  if (res = snd_catch do src_sound([0, 1, 1, -1]) end.first) != :out_of_range
    snd_display("src_sound env through 0: %s", res.inspect)
  end
  # 
  scale_to(1.0, ind1)
  v0 = make_vct(10)
  v1 = samples2vct(12000, 10, ind1, 0)
  v0[0] = 1.0
  array2file("fmv3.snd", v0, 10, 22050, 1)
  file_copy("oboe.snd", "fmv4.snd")
  convolve_with("fmv3.snd", 1.0, ind1)
  convolve_files("fmv4.snd", "fmv3.snd", 1.0, "fmv5.snd")
  v2 = samples2vct(12000, 10, ind1, 0)
  snd_display("convolve_with (orig: 0)\n# %s\n# %s", v1.inspect, v2.inspect) unless vfequal(v1, v2)
  file2array("fmv5.snd", 0, 12000, 10, v2)
  snd_display("convolve_files (orig: 0)\n# %s\n# %s", v1.inspect, v2.inspect) unless vfequal(v1, v2)
  delete_files("fmv3.snd", "fmv5.snd")
  convolve_files("2.snd", "oboe.snd", 0.5, "fmv5.snd")
  if fneq((res = mus_sound_maxamp("fmv5.snd"))[1], 0.5)
    snd_display("convolve_files stereo: %s", res.inspect)
  end
  delete_file("fmv5.snd")
  scale_to(0.25, ind1)
  # INFO
  # XEN_EMPTY_LIST is Qnil
  # but g_set_y_bounds requires a list not nil, so the following doesn't work
  # 
  # set_y_bounds([], ind1)
  # if (res = y_bounds(ind1)) != [-0.25, 0.25]
  #   snd_display("y_bounds []: %s", res.inspect)
  # end
  revert_sound(ind1)
  #
  scale_to(1.0, ind1)
  v0 = make_vct(10)
  v1 = samples2vct(12000, 10, ind1, 0)
  v0[5] = 1.0
  array2file("fmv3.snd", v0, 10, 22050, 1)
  convolve_with("fmv3.snd", 1.0, ind1)
  convolve_files("fmv4.snd", "fmv3.snd", 1.0, "fmv5.snd")
  v2 = samples2vct(12005, 10, ind1, 0)
  snd_display("convolve_with (orig: 2)\n# %s\n# %s", v1.inspect, v2.inspect) unless vfequal(v1, v2)
  file2array("fmv5.snd", 0, 12005, 10, v2)
  snd_display("convolve_files (orig: 2)\n# %s\n# %s", v1.inspect, v2.inspect) unless vfequal(v1, v2)
  delete_files("fmv3.snd", "fmv4.snd", "fmv5.snd")
  revert_sound(ind1)
  #
  old_val = selection_creates_region
  old_regions = regions
  set_selection_creates_region(false)
  select_all(ind1)
  set_selection_creates_region(old_val)
  if old_regions != regions
    snd_display("selection_creates_region: %s -> %s", old_regions.inspect, regions.inspect)
  end
  convolve_selection_with("pistol.snd", maxamp)
  data = samples2vct(12000, 10, ind1, 0)
  convolve_with("pistol.snd", maxamp(ind1, 0, 0), ind1, 0, 0)
  new_data = samples2vct(12000, 10, ind1, 0)
  unless vfequal(data, new_data)
    snd_display("convolve_selection_with:\n# %s\n# %s", data.inspect, new_data.inspect)
  end
  revert_sound(ind1)
  #
  make_selection(1000, 2000, ind1)
  ma = maxamp(ind1)
  convolve_selection_with("pistol.snd", ma)
  if fneq(maxamp(ind1), ma)
    snd_display("convolve_selection_with 1000: %f %f?", ma, maxamp(ind1))
  end
  make_selection(1000, 2000, ind1)
  id = make_region
  snd_display("make_region argless: %s?", id.inspect) unless region?(id)
  if (res1 = region_frames(id, 0)) != (res2 = selection_frames)
    snd_display("region/selection_frames: %s %s (%s)?", res1, res2, region_frames(id).inspect)
  end
  if (res1 = region_sample(0, id)) != (res2 = sample(1000, ind1))
    snd_display("region_sample from make_region: %s %s?", res1, res2)
  end
  close_sound(ind1)
end

def test175
  ind = open_sound("2.snd")
  reg = make_region(0, 100, ind, true)
  if (res = region_chans(reg)) != 2
    snd_display("make_region chan true: %d", res)
  end
  close_sound(ind)
  # 
  ind = open_sound("2.snd")
  v0 = samples2vct(12000, 10, ind, 0)
  v1 = samples2vct(12000, 10, ind, 1)
  swap_channels(ind)
  v2 = samples2vct(12000, 10, ind, 0)
  v3 = samples2vct(12000, 10, ind, 1)
  if vequal(v0, v2) or vequal(v1, v3)
    snd_display("swap_channels 0: no change!\n# %s\n# %s\n# %s\n# %s", v0, v2, v1, v3)
  end
  swap_channels(ind)
  v2 = samples2vct(12000, 10, ind, 0)
  v3 = samples2vct(12000, 10, ind, 1)
  unless vequal(v0, v2) or vequal(v1, v3)
    snd_display("swap_channels 1: \n# %s\n# %s\n# %s\n# %s", v0, v2, v1, v3)
  end
  set_cursor(100, ind, 0)
  set_cursor(200, ind, 1)
  if (res0 = cursor(ind, 0)) != 100 or (res1 = cursor(ind, 1)) != 200
    snd_display("cursor: %s %s?", res0, res1)
  end
  forward_sample(10, ind, 0)
  forward_sample(-10, ind, 1)
  if (res0 = cursor(ind, 0)) != 110 or (res1 = cursor(ind, 1)) != 190
    snd_display("cursor (1): %s %s?", res0, res1)
  end
  backward_sample(-10, ind, 0)
  backward_sample(10, ind, 1)
  if (res0 = cursor(ind, 0)) != 120 or (res1 = cursor(ind, 1)) != 180
    snd_display("cursor (2): %s %s?", res0, res1)
  end
  set_sync(1, ind)
  scale_by([0.5, 0.25], ind)
  scale_by(vct(2.0, 4.0), ind)
  revert_sound(ind)
  amps = maxamp(ind, true)
  swap_channels(ind, 0, ind)
  newamps = maxamp(ind, true)
  if fneq(amps[0], newamps[1]) or fneq(amps[1], newamps[0])
    snd_display("swap_channels with cp def: %s %s?", amps.inspect, newamps.inspect)
  end
  swap_channels(ind, 1)
  newamps = maxamp(ind, true)
  if fneq(amps[0], newamps[0]) or fneq(amps[1], newamps[1])
    snd_display("swap_channels with cp def 0: %s %s?", amps.inspect, newamps.inspect)
  end
  close_sound(ind)
end

def test185
  ind1 = open_sound("oboe.snd")
  ind2 = open_sound("2.snd")
  ups1 = count_matches(lambda do |n| n > 0.1 end, 0, ind1, 0)
  ups2 = let(0) do |count|
    scan_chan(lambda do |n|
                count += 1 if n > 0.1
                false
              end, 0, frames(ind1), ind1, 0)
    count
  end
  snd_display("scan_chan: %s %s?", ups1, ups2) if ups1 != ups2
  ups1 = count_matches(lambda do |n| n > 0.03 end, 0, ind2, 0)
  ups2 = count_matches(lambda do |n| n > 0.03 end, 0, ind2, 1)
  ups3 = let(0) do |count|
    scan_chan(lambda do |n|
                count += 1 if n > 0.03
                false
              end, 0, frames(ind2), ind2, 0)
    count
  end
  ups4 = let(0) do |count|
    scan_chan(lambda do |n|
                count += 1 if n > 0.03
                false
              end, 0, frames(ind2), ind2, 1)
    count
  end
  snd_display("2[0] scan_chan: %s %s?", ups1, ups3) if ups1 != ups3
  snd_display("2[1] scan_chan: %s %s?", ups2, ups4) if ups2 != ups4
  set_sync(true, ind2)
  total = let(0) do |count|
    scan_chans do |n|
      count += 1 if n > 0.03
      false
    end
    count
  end
  snd_display("scan_chans: %s %s?", total, ups1 + ups2) if total != ups1 + ups2
  set_sync(false, ind2)
  total = let(0) do |count|
    scan_sound_chans(0, frames(ind2), ind2) do |n|
      count += 1 if n > 0.03
      false
    end
    count
  end
  snd_display("scan_sound_chans: %s %s?", total, ups1 + ups2) if total != ups1 + ups2
  total = let(0) do |count|
    scan_across_all_chans do |data, len|
      data.each do |val| count += 1 if val > 0.03 end
      false
    end
    count
  end
  ups3 = count_matches(lambda do |n| n > 0.03 end, 0, ind1, 0)
  if total != ups1 + ups2 + ups3
    snd_display("scan_across_all_chans: %s %s?", total, ups1 + ups2 + ups3)
  end
  total = let(0) do |count|
    scan_all_chans do |n|
      count += 1 if n > 0.03
      false
    end
    count
  end
  ups3 = count_matches(lambda do |n| n > 0.03 end, 0, ind1, 0)
  if total != ups1 + ups2 + ups3
    snd_display("scan_all_chans: %s %s?", total, ups1 + ups2 + ups3)
  end
  select_sound(ind1)
  forward_graph
  if selected_sound != ind2 or selected_channel != 0
    snd_display("forward from %d 0 to %d %d?", ind1, selected_sound, selected_channel)
  end
  forward_graph
  if selected_sound != ind2 or selected_channel != 1
    snd_display("forward from %d 0 to %d %d?", ind2, selected_sound, selected_channel)
  end
  forward_graph(1)
  if selected_sound != ind1 or selected_channel != 0
    snd_display("forward from %d 1 to %d %d?", ind2, selected_sound, selected_channel)
  end
  forward_graph(2)
  if selected_sound != ind2 or selected_channel != 1
    snd_display("forward from %d 0 to %d %d?", ind1, selected_sound, selected_channel)
  end
  forward_graph(0)
  if selected_sound != ind2 or selected_channel != 1
    snd_display("forward 0 from %d 1 to %d %d?", ind1, selected_sound, selected_channel)
  end
  # 
  backward_graph(2)
  if selected_sound != ind1 or selected_channel != 0
    snd_display("backward 2 from %d 1 to %d %d?", ind2, selected_sound, selected_channel)
  end
  backward_graph
  if selected_sound != ind2 or selected_channel != 1
    snd_display("backward 2 from %d 0 to %d %d?", ind1, selected_sound, selected_channel)
  end
  forward_graph(-1)
  if selected_sound != ind2 or selected_channel != 0
    snd_display("forward -1 from %d 0 to %d %d?", ind1, selected_sound, selected_channel)
  end
  backward_graph(-1)
  if selected_sound != ind2 or selected_channel != 1
    snd_display("backward -1 from %d 0 to %d %d?", ind1, selected_sound, selected_channel)
  end
  close_sound(ind1)
  close_sound(ind2)
end

def test195
  ind1 = open_sound("oboe.snd")
  ind2 = open_sound("2.snd")
  ups1 = maxamp(ind1, 0)
  ups2 = maxamp(ind2, true)
  map_chan(lambda do |n| n * 2.0 end, 0, frames(ind1), "ignore: times 2", ind1, 0)
  map_sound_chans(0, frames(ind2), "ignore: times 2", ind2) do |n| n * 2.0 end
  ups3 = maxamp(ind1, 0)
  ups4 = maxamp(ind2, true)
  snd_display("map_chan: %s %s?", ups3, ups1 * 2.0) if fneq(ups3, ups1 * 2.0)
  if fneq(ups4[0], ups2[0] * 2.0) or fneq(ups4[1], ups2[1] * 2.0)
    snd_display("map_sound_chans: %s %s?", ups2.map do |n| 2.0 * n end.inspect, ups4.inspect)
  end
  set_sync(true, ind1)
  set_sync(true, ind2)
  map_chans do |n| n * 0.5 end
  ups3 = maxamp(ind1, 0)
  ups4 = maxamp(ind2, true)
  snd_display("map_chans: %s %s?", ups3, ups1) if fneq(ups3, ups1)
  if fneq(ups4[0], ups2[0]) or fneq(ups4[1], ups2[1])
    snd_display("map_chans: %s %s?", ups2.inspect, ups4.inspect)
  end
  set_sync(false, ind1)
  len_err = false
  map_across_all_chans do |data, len|
    if len != 3
      len_err = len
    else
      data[0] *= 4.0
      data[1], data[2] = data[2], data[1]
    end
    data
  end
  snd_display("map_across_all_chans len: %d?", len_err) if len_err
  ups3 = maxamp(ind1, 0)
  ups4 = maxamp(ind2, true)
  snd_display("map_across_all_chans 1: %s %s?", ups3, ups1) if fneq(ups3, ups1 * 4.0)
  if fneq(ups4[0], ups2[1])
    snd_display("map_across_all_chans 2: %s %s?", ups2.inspect, ups4.inspect)
  end
  revert_sound(ind1)
  revert_sound(ind2)
  map_all_chans do |n| n * 4.0 end
  ups3 = maxamp(ind1, 0)
  ups4 = maxamp(ind2, 0)
  ups5 = maxamp(ind1, 0, 0)
  ups6 = maxamp(ind2, 0, 0)
  snd_display("map_all_chans: %f %f?", ups3, ups5) if fneq(ups3, ups5 * 4.0)
  snd_display("map_all_chans (2): %f %f?", ups4, ups6) if fneq(ups4, ups6 * 4.0)
  close_sound(ind1)
  close_sound(ind2)
end

def get_test_args(args, snd, chn, edpos)
  [(args[0] or snd), (args[1] or chn), (args[2] or edpos)]
end

def test205
  ind1 = open_sound("oboe.snd")
  len = frames(ind1)
  ctr = 0
  map_chan(lambda do |n|
             ctr = (ctr == 1) ? 0 : 1
             ctr.zero? ? n * 2.0 : false
           end, 0, frames(ind1), "ignore: cut 2", ind1, 0)
  snd_display("map_chan cut: %d %d?", len, frames(ind1)) if frames(ind1) > (len * 2 + 1)
  revert_sound(ind1)
  ctr = 0
  map_chan(lambda do |n|
             ctr += 1
             ctr > 3 ? true : n
           end, 0, frames(ind1), "ignore: cut none", ind1, 0)
  snd_display("map_chan no-edit count: %d?", ctr) if ctr > 4
  revert_sound(ind1)
  v1 = make_vct(2)
  map_chan(lambda do |n|
             v1[0] = n
             v1[1] = n * 3.0
             v1
           end, 0, frames(ind1), "ignore: cut 2", ind1, 0)
  snd_display("map_chan double: %d %d?", len, frames(ind1)) if (frames(ind1) - len * 2).abs > 3
  close_sound(ind1)
  ind1 = open_sound("oboe.snd")
  test_edpos(ind1, :maxamp) do scale_by(2.0, ind1, 0) end
  test_edpos(ind1, :frames) do src_sound(2.0, 1.0, ind1, 0) end
  test_edpos(ind1, :count_matches, lambda do |*args|
               snd, chn, edpos = get_test_args(args, 0, 0, Current_edit_position)
               count_matches(lambda do |n1| n1 > 0.1 end, 0, snd, chn, edpos)
             end) do
    scale_by(2.0, ind1, 0)
  end
  test_edpos(ind1, :find, lambda do |*args|
               snd, chn, edpos = get_test_args(args, 0, 0, Current_edit_position)
               find(lambda do |n2| n2 > 0.1 end, 0, snd, chn, edpos)[1]
             end) do
    delete_samples(0, 100, ind1, 0)
  end
  test_edpos(ind1, :scan_chan, lambda do |*args|
               snd, chn, edpos = get_test_args(args, 0, 0, Current_edit_position)
               samp = 0
               scan_chan(lambda do |n3|
                           if n3 > 0.1
                             samp
                           else
                             samp += 1
                             false
                           end
                         end, 0, frames(snd, chn), snd, chn, edpos)
               samp
             end) do
    delete_samples(0, 100, ind1, 0)
  end
  #
  src_sound(2.0, 1.0, ind1, 0)
  play_and_wait(0, ind1, 0, false, false, 0)
  play_and_wait(0, ind1, 0, false, false, 1)
  play_and_wait(0, ind1, 0, false, false, lambda do |snd, chn| edit_position(snd, chn) end)
  undo(1, ind1, 0)
  play_and_wait(0, ind1, 0, false, false, 1)
  #
  delete_samples(0, 10000, ind1, 0)
  save_sound_as("fmv.snd", ind1, "edit-position".intern, 0)
  save_sound_as("fmv1.snd", ind1, "edit-position".intern, lambda do |snd, chn| 1 end)
  if (res = snd_catch do
        save_sound_as("fmv2.snd", ind1, :channel, 1234)
      end.first) != :no_such_channel
    snd_display("save_sound_as bad chan: %s", res.inspect)
  end
  if (res0 = mus_sound_frames("fmv.snd")) != (res1 = frames(ind1, 0, 0))
    snd_display("save_sound_as (edpos): %s %s?", res0, res1)
  end
  if (res0 = mus_sound_frames("fmv1.snd")) != (res1 = frames(ind1, 0, 1))
    snd_display("save_sound_as (edpos 1): %s %s?", res0, res1)
  end
  if (res0 = mus_sound_frames("fmv.snd")) == (res1 = frames(ind1, 0, 1))
    snd_display("save_sound_as (edpos 1)(2): %s %s?", res0, res1)
  end
  ind2 = open_sound("fmv.snd")
  ind3 = open_sound("fmv1.snd")
  unless vequal(res0 = samples2vct(12000, 10, ind1, 0, false, 0),
                res1 = samples2vct(12000, 10, ind2, 0))
    snd_display("save_sound_as (edpos 3): %s %s?", res0, res1)
  end
  unless vequal(res0 = samples2vct(12000, 10, ind1, 0, false, 1),
                res1 = samples2vct(12000, 10, ind3, 0))
    snd_display("save_sound_as (edpos 4): %s %s?", res0, res1)
  end
  if vequal(res0 = samples2vct(12000, 10, ind2), res1 = samples2vct(12000, 10, ind3, 0))
    snd_display("save_sound_as (edpos 5): %s %s?", res0, res1)
  end
  select_sound(ind3)
  set_comment("hiho")
  snd_display("set_comment no index: %s?", comment) if comment != "hiho"
  close_sound(ind2)
  close_sound(ind3)
  delete_files("fmv.snd", "fmv1.snd")
  #
  test_edpos_1(:reverse_sound, ind1) do |snd, pos|
    reverse_sound(snd, 0, pos)
  end
  test_edpos_1(:env_sound, ind1) do |snd, pos|
    env_sound([0, 0, 1, 1, 2, 0], 0, 20000, 1.0, snd, 0, pos)
  end
  test_edpos_1(:src_sound, ind1) do |snd, pos|
    src_sound(0.5, 1.0, snd, 0, pos)
  end
  test_edpos_1(:filter_sound, ind1) do |snd, pos|
    filter_sound(make_fir_filter(6, vct(0.1, 0.2, 0.3, 0.3, 0.2, 0.1)), 6, snd, 0, pos)
  end
  test_edpos_1(:convolve_with, ind1) do |snd, pos|
    convolve_with("pistol.snd", 0.5, snd, 0, pos)
  end
  #
  ind = new_sound("fmv.snd")
  v = make_vct!(2000) do |i| sin(i * (PI / 5.0)) end
  vct2channel(v, 0, 2000, ind, 0)
  filter_sound([0, 0, 0.09, 0, 0.1, 1, 0.11, 0, 1, 0], 1024)
  snd_display("filter_sound maxamp 1: %s?", maxamp) if maxamp > 0.025
  undo
  filter_sound([0, 0, 0.19, 0, 0.2, 1, 0.21, 0, 1, 0], 1024)
  snd_display("filter_sound maxamp 2: %s?", maxamp) if maxamp < 0.9
  undo
  filter_sound([0, 0, 0.29, 0, 0.3, 1, 0.31, 0, 1, 0], 1024)
  snd_display("filter_sound maxamp 3: %s?", maxamp) if maxamp > 0.02
  close_sound(ind)
  #
  set_previous_files_sort_procedure(lambda do |lst|
                                      lst.sort do |a, b|
                                        dura = mus_sound_duration(a)
                                        durb = mus_sound_duration(b)
                                        if dura > durb
                                          1
                                        elsif dura < durb
                                          -1
                                        else
                                          0
                                        end
                                      end
                                    end)
  unless (res = previous_files_sort_procedure).kind_of?(Proc)
    snd_display("previous_files_sort_procedure: %s", res.inspect)
  end
  set_previous_files_sort(5)
  close_sound(ind1)
  #
  if (res = snd_catch do
        set_previous_files_sort_procedure(lambda do |a, b, c| false end)
      end.first) != :bad_arity
    snd_display("previous_files_sort_procedure arity error: %s?", res.inspect)
  end
  5.times do |i| set_previous_files_sort(i) end
  set_previous_files_sort(1)
  dismiss_all_dialogs
end

def test215
  File.open("sndtst", "w") do |fp|
    # snd-ruby eats all args that's why this test with ruby
    # #!#{$home_dir}/cl/snd -noinit -batch
    fp.print "\
#!/usr/bin/env ruby
require \"examp.rb\"
require \"sndlib.so\" unless provided? \"sndlib\"
(ARGV.length != 1) ? print(\"usage: script file-name...\\n\") : printf(\"%s: %s\\n\", ARGV[0], mus_sound_comment(ARGV[0]))
exit\n"
    fp.chmod(0755)
  end
  if (res = shell("sndtst fyow.snd")) !=
      "fyow.snd: ;Written on Mon 1-Jul-91 at 12:10 PDT  at localhost (NeXT) using Allegro CL and clm of 25-June-91\n"
    snd_display("script: %s?", res)
  end
  delete_file("sndtst")
  File.open("sndtst", "w") do |fp|
    fp.print "\
#!#{$home_dir}/cl/snd -noinit -batch
open_sound \"fmv.snd\"
scale_by 2.0
save_sound
exit\n"
    fp.chmod(0755)
  end
  delete_file("fmv.snd")
  mus_sound_prune
  file_copy("oboe.snd", "fmv.snd")
  sleep 1
  ind = open_sound("fmv.snd")
  samps = samples2vct(5000, 10)
  date = mus_sound_write_date("fmv.snd")
  if frames(ind) != mus_sound_frames("oboe.snd")
    snd_display("cp oboe.snd -> fmv.snd: %s %s?", frames(ind), mus_sound_frames("oboe.snd"))
  end
  s_in = s_out = nil
  $update_hook.reset_hook!
  $update_hook.add_hook!("snd-test") do |snd|
    s_in = snd
    lambda do |newsnd| s_out = newsnd end
  end
  scale_by 3.0
  snd_catch do
    add_mark(101, ind)
    add_mark(202, ind)
  end
  system "sndtst"
  if (res = mus_sound_write_date("fmv.snd")) == date
    snd_display("script didn\'t overwrite fmv.snd?")
  end
  set_sync(123, ind)
  nind = update_sound(ind)
  if edits(nind) != [0, 0]
    snd_display("update_sound edits: %s?", edits(nind).inspect)
  end
  if (res = marks(nind, 0).map do |x| mark_sample(x) end) != [101, 202]
    snd_display("update_sound marks: %s?", res.inspect)
  end
  snd_display("update_sound sync: %s?", sync(nind)) if sync(nind) != 123
  nsamps = samples2vct(5000, 10)
  unless vequal(samps, vct_scale!(nsamps, 0.5))
    snd_display("update_sound amps: %s %s?", samps.inspect, nsamps.inspect)
  end
  if (not s_in.kind_of?(Integer)) or ind != s_in
    snd_display("update_hook init: %s %s?", ind, s_in)
  end
  if (not s_out.kind_of?(Integer)) or nind != s_in
    snd_display("update_hook done: %s %s?", nind, s_out)
  end
  $update_hook.reset_hook!
  close_sound(ind)
  ind = open_sound("fmv.snd")
  $update_hook.add_hook!("snd-test") do |snd|
    lambda do snd_error("oops") end
  end
  system "sndtst"
  if (res = snd_catch do
        # Proc#arity returns -1 instead of 1
        snd_raise(:bad_arity) if RUBY_VERSION < "1.8.0"
        update_sound(ind)
      end.first) != :bad_arity
    snd_display("bad update_hook result: %s", res.inspect)
  end
  close_sound(find_sound("fmv.snd"))
  delete_files("fmv.snd", "sndtst")
end

def peak_env_equal?(name, index, e, diff)
  rd = make_sample_reader(0, index, 0)
  e_size = e.first.length
  samps_per_bin = (frames(index) / e_size.to_f).ceil
  mins, maxs = e[0, 2]
  max_diff = 0.0
  callcc do |ret|
    e_bin = 0
    samp = 0
    mx = -10.0
    mn = 10.0
    until e_bin == e_size
      if samp >= samps_per_bin.floor
        mxdiff = (mx - maxs[e_bin]).abs
        mndiff = (mn - mins[e_bin]).abs
        if mxdiff > max_diff
          max_diff = mxdiff
        end
        if mndiff > max_diff
          max_diff = mndiff
        end
        if mxdiff > diff or mndiff > diff
          snd_display("%s: peak_env_equal? [bin %d of %d]: %s %s %f?",
                      name, e_bin, e_size, mn.inspect, mx.inspect, [mxdiff, mndiff].max)
          ret.call(false)
        end
        samp = 0
        mx = -10.0
        mn = 10.0
        e_bin += 1
      end
      val = next_sample(rd)
      if val < mn
        mn = val
      end
      if val > mx
        mx = val
      end
      samp += 1
      true
    end
  end
end

def test225
  ind = open_sound("oboe.snd")
  mx = maxamp(ind, 0)
  if (e0 = channel_amp_envs(ind, 0)).nil?
    snd_display("no amp env data")
  else
    mx1 = vct_peak(e0[0])
    mx2 = vct_peak(e0[1])
    snd_display("amp env max: %s %s %s?", mx, mx1, mx2) if fneq(mx, [mx1, mx2].max)
    peak_env_equal?("straight peak", ind, e0, 0.0001)
    # 
    scale_by 3.0
    e1 = channel_amp_envs(ind, 0, 1)
    mx3 = vct_peak(e1[0])
    mx4 = vct_peak(e1[1])
    if fneq(mx1 * 3.0, mx3) or fneq(mx2 * 3.0, mx4)
      snd_display("3.0 amp env max: %s %s %s %s?", mx1, mx2, mx3, mx4)
    end
    peak_env_equal?("scaled peak", ind, e1, 0.0001)
    if fneq(maxamp(ind, 0), 3.0 * mx)
      snd_display("maxamp after scale: %s %s?", mx, maxamp(ind, 0))
    end
    undo
    # 
    set_selection_member?(false, true)
    set_selection_member?(true, ind, 0)
    set_selection_position(20000, ind, 0)
    set_selection_frames(12000, ind, 0)
    scale_selection_by 3.0
    e1 = channel_amp_envs(ind, 0, 1)
    mx3 = vct_peak(e1[0])
    mx4 = vct_peak(e1[1])
    if fneq(mx1 * 3.0, mx3) or fneq(mx2 * 3.0, mx4)
      snd_display("selection 3.0 amp env max: %s %s %s %s?", mx1, mx2, mx3, mx4)
    end
    if fneq(maxamp(ind, 0), 3.0 * mx)
      snd_display("maxamp after selection scale: %s %s?", mx, maxamp(ind, 0))
    end
    peak_env_equal?("selection peak", ind, e1, 0.0001)
    # 
    map_chan(lambda do |n| n.abs end, ind, 0)
    e1 = channel_amp_envs(ind, 0, 2)
    mx3 = vct_peak(e1[0])
    mx4 = vct_peak(e1[1])
    if fneq(mx2 * 3.0, mx4)
      snd_display("abs selection 3.0 amp env max: %s %s %s %s?", mx1, mx2, mx3, mx4)
    end
    if fneq(maxamp(ind, 0), 3.0 * mx)
      snd_display("maxamp after abs selection scale: %s %s?", mx, maxamp(ind, 0))
    end
    if ffneq(mx3, 0.03)
      snd_display("abs max: %s %s?", mx3, mx4)
    end
    peak_env_equal?("map_chan peak", ind, e1, 0.0001)
    #
    delete_samples(10000, 5000)
    e1 = channel_amp_envs(ind, 0)
    mx3 = vct_peak(e1[0])
    mx4 = vct_peak(e1[1])
    if fneq(mx2 * 3.0, mx4)
      snd_display("abs selection 3.0 amp env max: %s %s %s %s?", mx1, mx2, mx3, mx4)
    end
    if fneq(maxamp(ind, 0), 3.0 * mx)
      snd_display("maxamp after abs selection scale: %s %s?", mx, maxamp(ind, 0))
    end
    if ffneq(mx3, 0.03)
      snd_display("abs max: %s %s?", mx3, mx4)
    end
    peak_env_equal?("delete peak", ind, e1, 0.0001)
    #
    scale_selection_by -0.333
    e1 = channel_amp_envs(ind, 0, 4)
    mx3 = vct_peak(e1[0])
    mx4 = vct_peak(e1[1])
    if fneq(maxamp(ind, 0), mx)
      snd_display("maxamp after minus selection scale: %s %s?", mx, maxamp(ind, 0))
    end
    if fneq(maxamp(ind, 0), mx3)
      snd_display("mx3 maxamp after minus abs selection scale: %s %s?", mx, mx3)
    end
    peak_env_equal?("scale_selection peak", ind, e1, 0.0001)
  end
  revert_sound(ind)
  ramp_channel(0.0, 1.0)
  peak_env_equal?("ramp_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.001)
  undo
  env_channel([0, 0, 1, 1, 2, 0])
  peak_env_equal?("env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.002)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.5, :end, frames - 1))
  peak_env_equal?("scaled env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.002)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0], 0.5, :end, frames - 1))
  peak_env_equal?("scaled nokey env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.001)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.5, :offset, 0.5, :end, frames - 1))
  peak_env_equal?("scaled and offset env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.001)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0.5, 3, 0], :base, 0.0, :end, frames - 1))
  peak_env_equal?("env_channel base 0.0 peak", ind, channel_amp_envs(ind, 0, 1), 0.001)
  undo
  xramp_channel(0.0, 1.0, 32.0)
  peak_env_equal?("xramp_channel 32.0 peak", ind, channel_amp_envs(ind, 0, 1), 0.008)
  undo
  xramp_channel(0.0, 1.0, 0.032)
  peak_env_equal?("xramp_channel 0.032 peak", ind, channel_amp_envs(ind, 0, 1), 0.004)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0.5, 3, 0], :base, 10.0, :end, frames - 1))
  peak_env_equal?("env_channel base 10.0 peak", ind, channel_amp_envs(ind, 0, 1), 0.01)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0], :base, 0.1, :end, frames - 1))
  peak_env_equal?("env_channel base 0.1 peak", ind, channel_amp_envs(ind, 0, 1), 0.003)
  undo
  insert_samples(1000, 5000, make_vct(5000, 0.5))
  peak_env_equal?("insert_samples peak", ind, channel_amp_envs(ind, 0, 1), 0.0001)
  undo
  set_samples(500, 100, make_vct(100, 0.1))
  peak_env_equal?("set_samples peak", ind, channel_amp_envs(ind, 0, 1), 0.0001)
  undo
  #
  revert_sound(ind)
  ramp_channel(0.0, 1.0)
  ramp_channel(1.0, 0.0)
  peak_env_equal?("2 ramp_channel peak", ind, channel_amp_envs(ind, 0, 2), 0.002)
  #
  revert_sound(ind)
  env_channel([0, 0, 1, 1])
  env_channel([0, 0, 1, 1, 2, 0])
  peak_env_equal?("2 env_channel peak", ind, channel_amp_envs(ind, 0, 2), 0.002)
  revert_sound(ind)
  ramp_channel(0.0, 1.0, 12000, 5000)
  peak_env_equal?("ramp_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.002)
  undo
  env_channel([0, 0, 1, 1, 2, 0], 12000, 5000)
  peak_env_equal?("env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.003)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.5, :end, 4999), 12000, 5000)
  peak_env_equal?("scaled env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.004)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0], 0.5, :end, 4999), 12000, 5000)
  peak_env_equal?("scaled nokey env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.004)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.5, :offset, 0.5, :end, 4999), 12000, 5000)
  peak_env_equal?("scaled and offset env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.002)
  undo
  xramp_channel(0.0, 1.0, 32.0, 2000, 1000)
  peak_env_equal?("xramp_channel 32.0 peak (1)", ind, channel_amp_envs(ind, 0, 1), 0.009)
  undo
  xramp_channel(0.0, 1.0, 0.032, 2000, 1000)
  peak_env_equal?("xramp_channel 0.032 peak (1)", ind, channel_amp_envs(ind, 0, 1), 0.01)
  undo
  env_channel(make_env([0, 0, 1, 1, 2, 0.5, 3, 0], :base, 10.0, :end, 4999), 12000, 5000)
  peak_env_equal?("env_channel base 10.0 peak", ind, channel_amp_envs(ind, 0, 1), 0.1)
  undo
  #
  revert_sound(ind)
  ramp_channel(0.0, 1.0)
  ramp_channel(1.0, 0.0, 2000, 1000)
  peak_env_equal?("2 ramp_channel peak", ind, channel_amp_envs(ind, 0, 2), 0.002)
  #
  revert_sound(ind)
  env_channel([0, 0, 1, 1])
  env_channel([0, 0, 1, 1, 2, 0], 2000, 1000)
  peak_env_equal?("2 env_channel peak", ind, channel_amp_envs(ind, 0, 2), 0.002)
  # 
  revert_sound(ind)
  env_channel([0, 0, 1, 1])
  env_channel([0, 0, 1, 1, 2, 0])
  env_channel([0, 0, 1, 1], 12000, 5000)
  peak_env_equal?("3 env_channel peak", ind, channel_amp_envs(ind, 0, 3), 0.01)
  revert_sound(ind)
  close_sound(ind)
  #
  ind = new_sound("test.snd")
  map_chan(lambda do |y| 1.0 end, 0, 50000)
  ramp_channel(0.5, 1.0, 1000, 4000)
  mn, mx = channel_amp_envs(ind, 0)[0, 2]
  (mn.length - 4).times do |i|
    if mn[i] < 0.5
      snd_display("peak min: %s %d?", mn[i], i)
      break
    end
    if mx[i] < 0.5
      snd_display("peak max: %s %d?", mx[i], i)
      break
    end
  end
  undo
  map_chan(lambda do |y| -1.0 end, 0, 50000)
  ramp_channel(0.5, 1.0, 1000, 4000)
  mn, mx = channel_amp_envs(ind, 0)[0, 2]
  (mn.length - 4).times do |i|
    if mn[i] > -0.5
      snd_display("1 peak min: %s %d?", mn[i], i)
      break
    end
    if mx[i] > -0.5
      snd_display("1 peak max: %s %d?", mx[i], i)
      break
    end
  end
  close_sound(ind)
end

$g_init_val = 0

def test_channel_func(name, index, init_val, func, &val_func)
  len = frames(index)
  chns = chans(index)
  $g_init_val = init_val
  2.times do |k|
    val = val_func.call(len)
    set_sync(k, index)
    chns.times do |i|
      map_channel(lambda do |n| 0.0 end, 0, len, index, i)
      if res = scan_channel(lambda do |n| n.abs > 0.001 end, 0, len, index, i)
        snd_display("%s init scan: %s?", name, res.inspect)
      end
    end
    chns.times do |i|
      map_channel(lambda do |n| $g_init_val end, 0, len, index, i)
      func.call(0, len, index, i, false)
      chns.times do |j|
        vi = channel2vct(0, len, index, j)
        if j == i
          snd_display("%s chan func: %s %s?", name, vi.inspect, val.inspect) unless vequal(vi, val)
        else
          if res = scan_channel(lambda do |n| n.abs > 0.001 end, 0, len, index, j)
            snd_display("%s chan func leaks? %d %d: %s", name, i, j, res.inspect)
          end
        end
      end
      map_channel(lambda do |n| 0.0 end, 0, len, index, i)
    end
    chns.times do |i|
      map_channel(lambda do |n| $g_init_val end, 0, len, index, i)
      ed = edit_position(index, i)
      map_channel(lambda do |n| $g_init_val + 1.0 end, 0, len, index, i)
      func.call(0, len, index, i, ed)
      chns.times do |j|
        vi = channel2vct(0, len, index, j)
        if j == i
          unless vequal(vi, val)
            snd_display("%s ed chan func: %s %s?", name, vi.inspect, val.inspect)
          end
        else
          if res = scan_channel(lambda do |n| n.abs > 0.001 end, 0, len, index, j)
            snd_display("%s ed chan func leaks? %d %d %d: %s", name, i, j, ed, res.inspect)
          end
        end
      end
      map_channel(lambda do |n| 0.0 end, 0, len, index, i)
    end
    beg = dur = (len / 3.0).floor
    nv = val_func.call(dur)
    vct_fill!(val, 0.0)
    let(beg) do |i|
      dur.times do |j|
        val[i] = nv[j]
        i += 1
      end
    end
    chns.times do |i|
      map_channel(lambda do |n| $g_init_val end, beg, dur, index, i)
      func.call(beg, dur, index, i, false)
      add_mark(beg, index, i)
      chns.times do |j|
        vi = channel2vct(0, len, index, j)
        if j == i
          unless vequal(vi, val)
            snd_display("%s chan func n: %s %s?", name, vi.inspect, val.inspect)
          end
        else
          if res = scan_channel(lambda do |n| n.abs > 0.001 end, 0, len, index, j)
            snd_display("%s dur chan func leaks? %d %d: %s", name, i, j, res.inspect)
          end
        end
      end
      map_channel(lambda do |n| 0.0 end, 0, len, index, i)
    end
  end
end

def test235
  index = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 2, "channel tests")
  insert_silence(0, 10, index, 0)
  insert_silence(0, 10, index, 1)
  test_channel_func(:env, index, 0.0,
                    lambda do |beg, dur, index, chan, edpos|
                      clm_channel(make_env(:envelope, [0, 0, 1, 1], :end, dur - 1),
                                  beg, dur, index, chan, edpos)
                    end) do |dur|
    e = make_env(:envelope, [0, 0, 1, 1], :end, dur - 1)
    make_vct!(dur) do env(e) end
  end
  test_channel_func(:oscil, index, 0.0,
                    lambda do |beg, dur, index, chan, edpos|
                      clm_channel(make_oscil(:frequency, 0.0, "initial-phase".intern, PI / 2.0),
                                  beg, dur, index, chan, edpos)
                    end) do |dur| make_vct!(dur) do 1.0 end end
  test_channel_func(:scale_channel, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      scale_channel(0.5, beg, dur, index, chan, edpos)
                    end) do |dur| make_vct!(dur) do 0.5 end end
  test_channel_func(:env_channel, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      env_channel(make_env(:envelope, [0, 0, 1, 1], :end, dur - 1),
                                  beg, dur, index, chan, edpos)
                    end) do |dur|
    e = make_env(:envelope, [0, 0, 1, 1], :end, dur - 1)
    make_vct!(dur) do env(e) end
  end
  test_channel_func(:env_channel, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      env_channel([0, 0, 1, 1], beg, dur, index, chan, edpos)
                    end) do |dur|
    e = make_env(:envelope, [0, 0, 1, 1], :end, dur - 1)
    make_vct!(dur) do env(e) end
  end
  test_channel_func(:vct2channel, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      vct2channel(make_vct!(dur) do -1.0 end, beg, dur, index, chan)
                    end) do |dur| make_vct!(dur) do -1.0 end end
  test_channel_func(:pad_channel, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      delete_samples(beg, dur, index, chan, edpos)
                      pad_channel(beg, dur, index, chan, edpos)
                    end) do |dur| make_vct(dur) end
  test_channel_func(:insert_samples, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      delete_samples(beg, dur, index, chan, edpos)
                      insert_samples(beg, dur, make_vct!(dur) do -1.0 end, index, chan, edpos)
                    end) do |dur| make_vct!(dur) do -1.0 end end
  test_channel_func(:set_samples, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      set_samples(beg, dur, make_vct!(dur) do -1.0 end,
                                  index, chan, false, "test_channel", edpos)
                    end) do |dur| make_vct!(dur) do -1.0 end end
  test_channel_func(:reverse_channel, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      env_channel(make_env(:envelope, [0, 0, 1, 1], :end, dur - 1),
                                  beg, dur, index, chan, edpos)
                      reverse_channel(beg, dur, index, chan)
                    end) do |dur|
    e = make_env(:envelope, [0, 1, 1, 0], :end, dur - 1)
    make_vct!(dur) do env(e) end
  end
  test_channel_func(:smooth_channel, index, 1.0,
                    lambda do |beg, dur, index, chan, edpos|
                      env_channel(make_env(:envelope, [0, 0, 1, 1], :end, dur - 1),
                                  beg, dur, index, chan, edpos)
                      set_sample(beg + dur, 1.0, index, chan)
                      smooth_channel(beg, dur, index, chan)
                      if beg.nonzero?
                        set_sample(beg + dur, 0.0, index, chan)
                      end
                    end) do |dur|
    make_vct!(dur) do |i| 0.5 + 0.5 * cos(PI + (PI * i) / dur) end
  end
  #
  snd_display("channel edits: %s?", edits(index)) if edits(index) != [276, 0]
  old_max = maxamp(index, true)
  regdata = (regions or []).map do |n| region2vct(0, 10, n) end
  old_pos0 = edit_position(index, 0)
  old_pos1 = edit_position(index, 1)
  old_reglen = (regions or []).map do |n| region_frames(n) end
  s61_files = []
  $save_state_hook.add_hook!("snd-test") do |file|
    s61_files.push(file)
    false
  end
  delete_file("s61.rb")
  save_state("s61.rb")
  close_sound(index)
  (regions or []).each do |n| forget_region(n) end
  load("s61.rb")
  if (res = (regions or []).map do |n| region_frames(n) end) != old_reglen
    snd_display("region_frames after save: %s %s?", old_reglen.inspect, res.inspect)
  end
  (regions or []).zip(regdata) do |n, data|
    unless vequal(res = region2vct(0, 10, n), data)
      snd_display("region after save %s: %s %s?", n, data.inspect, res.inspect)
    end
  end
  index = find_sound("fmv.snd")
  if (res = maxamp(index, true)) != old_max
    snd_display("maxes: %s %s?", res.inspect, old_max.inspect)
  end
  snd_display("saved channel edits: %s?", edits(index)) if edits(index) != [276, 0]
  10.times do |i|
    pos = irandom(edits(index).first)
    scale_channel(my_random(2.0).abs, my_random(5.0), my_random(5.0), index, 0, pos)
    set_edit_position((edits(index).first * 0.7).floor, index)
  end
  close_sound(index)
  (regions or []).each do |n| forget_region(n) end
  s61_files.each do |file| delete_file(file) end
  delete_file("s61.rb")
end

def test245
  index = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 2, "channel tests")
  sw = sinc_width
  set_sinc_width(10)
  v0 = make_vct(10)
  v0[0] = 1.0
  vct2channel(v0, 0, 10, index, 0)
  src_channel(0.5, 0, 10, index, 0)
  s = make_src(:srate, 0.5,
               :input, let(1.0) do |val|
                 lambda do |dir|
                   rtn, val = val, 0.0
                   rtn
                 end
               end)
  v = make_vct!(10) do src(s) end
  unless vequal(res = channel2vct(0, 10, index, 0), v)
    snd_display("src_channel: %s %s?", v.inspect, res.inspect)
  end
  unless vequal(res = channel2vct(0, 10, index, 1), make_vct(10))
    snd_display("src_channel leaks: %s?", res.inspect)
  end
  if (res = snd_catch do src(s, 1.0, lambda do |a, b| a end) end.first) != :bad_arity
    snd_display("src bad func: %s?", res.inspect)
  end
  revert_sound(index)
  vct2channel(v0, 0, 10, index, 1)
  vct2channel(v0, 10, 10, index, 1)
  src_channel(make_env(:envelope, [1, 1, 2, 2], :end, 20), 0, 20, index, 1)
  unless vequal(res = channel2vct(0, 10, index, 1),
                vct(1.000, -0.000, -0.048, 0.068, -0.059, 0.022, 0.030, -0.100, 0.273, 0.606))
    snd_display("src_channel env: %s?", res.inspect)
  end
  unless vequal(res = channel2vct(0, 10, index, 0), make_vct(10))
    snd_display("src_channel env leaks: %s?", res.inspect)
  end
  # 
  revert_sound(index)
  vct2channel(v0, 0, 10, index, 1)
  vct2channel(v0, 10, 10, index, 1)
  src_channel(make_env(:envelope, [1, 1, 2, 2], :end, 20), 0, 20, index, 1)
  unless vequal(res = channel2vct(0, 10, index, 1),
                vct(1.000, -0.000, -0.048, 0.068, -0.059, 0.022, 0.030, -0.100, 0.273, 0.606))
    snd_display("src_channel env: %s?", res.inspect)
  end
  unless vequal(res = channel2vct(0, 10, index, 0), make_vct(10))
    snd_display("src_channel env leaks: %s?", res.inspect)
  end
  # 
  revert_sound(index)
  vct2channel(v0, 0, 10, index, 1)
  vct2channel(v0, 10, 10, index, 1)
  src_channel([1, 1, 2, 2], 0, 20, index, 1)
  unless vequal(res = channel2vct(0, 10, index, 1),
                vct(1.000, -0.000, -0.051, 0.069, -0.056, 0.015, 0.042, -0.117, 0.320, 0.568))
    snd_display("src_channel lst: %s?", res.inspect)
  end
  unless vequal(res = channel2vct(0, 10, index, 0), make_vct(10))
    snd_display("src_channel lst leaks: %s?", res.inspect)
  end
  set_sinc_width(sw)
  close_sound(index)
end

def test255
  ind = open_sound("oboe.snd")
  rid0 = make_region(2000, 2020, ind, 0)
  rid0_data = region2vct_1(rid0, 0, 20)
  scale_sound_by(2.0)
  play_region(rid0, true)
  unless vequal(res = region2vct_1(rid0, 0, 20), rid0_data)
    snd_display("deferred region after scaling:\n# %s\n# %s", rid0_data.inspect, res.inspect)
  end
  unless vequal(res = region_to_vct(rid0, 0, 20), rid0_data)
    snd_display("deferred region after scaling (rs):\n# %s\n# %s", rid0_data.inspect, res.inspect)
  end
  undo
  scale_by(4.0)
  play_region(rid0, true)
  unless vequal(res = region2vct_1(rid0, 0, 20), rid0_data)
    snd_display("file region after scaling:\n# %s\n# %s", rid0_data.inspect, res.inspect)
  end
  unless vequal(res = region_to_vct(rid0, 0, 20), rid0_data)
    snd_display("file region after scaling (rs):\n# %s\n# %s", rid0_data.inspect, res.inspect)
  end
  rid1 = make_region(2000, 2020, ind, 0)
  rid1_data = region2vct_1(rid1, 0, 20)
  scale_to(0.5)
  unless vequal(res = region2vct_1(rid1, 0, 20), rid1_data)
    snd_display("deferred region after scale_to:\n# %s\n# %s", rid1_data.inspect, res.inspect)
  end
  close_sound(ind)
  play_region(rid0, true)
  play_region(rid1, true)
  unless vequal(res = region2vct_1(rid1, 0, 20), rid1_data)
    snd_display("deferred region after close:\n# %s\n# %s", rid1_data.inspect, res.inspect)
  end
  unless vequal(res = region2vct_1(rid0, 0, 20), rid0_data)
    snd_display("file region after close:\n# %s\n# %s", rid0_data.inspect, res.inspect)
  end
  [[2000, 20, 2000, 20],
    [2000, 10, 2000, 20],
    [2000, 20, 2000, 10],
    [0,    20, 2000, 20],
    [2000, 20, 0,    20],
    [0,    10, 2000, 20],
    [2000, 20, 0,    10]].each do |s1, l1, s2, l2|
    ind = open_sound("2.snd")
    set_selection_member?(false, true)
    set_selection_member?(true, ind, 0)
    set_selection_position(s1, ind, 0)
    set_selection_frames(l1, ind, 0)
    set_selection_member?(true, ind, 1)
    set_selection_position(s2, ind, 1)
    set_selection_frames(l2, ind, 1)
    rid2 = make_region
    rid20_data = region2vct_1(rid2, 0, l1)
    rid21_data = region2vct_1(rid2, 1, l2)
    if (res = region_chans(rid2)) != 2
      snd_display("region_chans of sync\'d sound: %s?", res)
    end
    swap_channels(ind, 0, ind, 1)
    unless vequal(res = region2vct_1(rid2, 0, l1), rid20_data)
      snd_display("deferred region after scaling (20):\n# %s\n# %s",
                  rid20_data.inspect, res.inspect)
    end
    unless vequal(res = region_to_vct(rid2, 0, l1), rid20_data)
      snd_display("deferred region after scaling (20 rs):\n# %s\n# %s",
                  rid20_data.inspect, res.inspect)
    end
    unless vequal(res = region2vct_1(rid2, 1, l2), rid21_data)
      snd_display("deferred region after scaling (21):\n# %s\n# %s",
                  rid21_data.inspect, res.inspect)
    end
    unless vequal(res = region_to_vct(rid2, 1, l2), rid21_data)
      snd_display("deferred region after scaling (21 rs):\n# %s\n# %s",
                  rid21_data.inspect, res.inspect)
    end
    close_sound(ind)
    unless vequal(res = region2vct_1(rid2, 0, l1), rid20_data)
      snd_display("deferred region after scaling (20):\n# %s\n# %s",
                  rid20_data.inspect, res.inspect)
    end
    unless vequal(res = region_to_vct(rid2, 0, l1), rid20_data)
      snd_display("deferred region after scaling (20 rs):\n# %s\n# %s",
                  rid20_data.inspect, res.inspect)
    end
    unless vequal(res = region2vct_1(rid2, 1, l2), rid21_data)
      snd_display("deferred region after scaling (21):\n# %s\n# %s",
                  rid21_data.inspect, res.inspect)
    end
    unless vequal(res = region_to_vct(rid2, 1, l2), rid21_data)
      snd_display("deferred region after scaling (21 rs):\n# %s\n# %s",
                  rid21_data.inspect, res.inspect)
    end
  end
  ind = open_sound("obtest.snd")
  set_read_only(true, ind)
  delete_samples(0, 1000, ind, 0)
  if res = save_sound(ind)
    snd_display("save_sound read_only: %s?", res.inspect)
  end
  if (res = edits(ind)) != [1, 0]
    snd_display("read_only ignored? ", res.inspect)
  end
  unless provided? "snd-nogui"
    if (res = widget_text(sound_widgets(ind)[3])) != "can't write obtest.snd (it is read-only)"
      snd_display("read_only report_in_minibuffer: %s?", res.inspect)
    end
    unless (res = widget_text(sound_widgets(ind)[4])).kind_of?(String)
      snd_display("widget_text of listener: %s?", res.inspect)
    end
  end
  set_read_only(false, ind)
  revert_sound(ind)
  save_sound(ind)
  unless provided? "snd-nogui"
    if (res = widget_text(sound_widgets(ind)[3])) != "(no changes need to be saved)"
      snd_display("save unneeded report_in_minibuffer: %s?", res.inspect)
    end
  end
  key(?j, 4)
  unless provided? "snd-nogui"
    if (res = widget_text(sound_widgets(ind)[3])) != "no marks"
      snd_display("C-j w/o marks: %s?", res.inspect)
    end
  end
  key(?-, 4)
  key(?j, 4)
  key(?j, 4)
  key(?x, 4)
  key(?c, 0)
  unless provided? "snd-nogui"
    if (res = widget_text(main_widgets()[1]))
      snd_display("widget_text of non-text widget: %s", res.inspect)
    end
    set_widget_text(channel_widgets(ind, 0)[2], "F")
    if (res = widget_text(channel_widgets(ind, 0)[2])) != "F"
      snd_display("set button label to F: %s?", res.inspect)
    end
    if (res = widget_text(sound_widgets(ind)[3])) != "no marks"
      snd_display("C-x c w/o marks: %s?", res.inspect)
    end
  end
  add_mark(123)
  key(?u, 4)
  key(?6, 4)
  key(?j, 4)
  unless provided? "snd-nogui"
    if (res = widget_text(sound_widgets(ind)[3])) != "no such mark"
      snd_display("C-u 6 C-j: %s?", res.inspect)
    end
  end
  key(?u, 4)
  key(?6, 4)
  key(?x, 4)
  key(?c, 0)
  unless provided? "snd-nogui"
    if (res = widget_text(sound_widgets(ind)[3])) != "no such mark"
      snd_display("C-u 6 C-x c: %s?", res.inspect)
    end
  end
  close_sound(ind)
  #
  view_sound("obtest.snd")
  delete_samples(0, 1000, ind, 0)
  save_sound(ind)
  if (res = edits(ind)) != [1, 0]
    snd_display("view read_only ignored? ", res.inspect)
  end
  unless provided? "snd-nogui"
    if (res = widget_text(sound_widgets(ind)[3])) != "can't write obtest.snd (it is read-only)"
      snd_display("view read_only report_in_minibuffer: %s?", res.inspect)
    end
  end
  close_sound(ind)
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1)
  insert_silence(0, 150000)
  map_channel(lambda do |y| 0.5 end)
  env_sound([0, 0, 1, 1, 2, 0])
  fp(1.0, 0.3, 20)
  old_cursor = cursor_follows_play
  set_cursor_follows_play(true)
  play_and_wait
  set_cursor_follows_play(old_cursor)
  close_sound(ind)
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1)
  [150, 1500, 150000].each do |dur|
    insert_silence(0, dur)
    map_channel($init_channel)
    env_sound([0, 0, 1, 1, 2, 0])
    rd = make_sample_reader(frames - 1, ind, 0, -1)
    if (res = sample_reader_position(rd)) != (frames - 1)
      snd_display("sample_reader_position: %s?", res)
    end
    map_channel(lambda do |y| read_sample(rd) end)
    pos = 0
    e = make_env([0, 0, 1, 1, 2, 0], :end, dur)
    scan_channel(lambda do |y|
                   if fneq(val = env(e), y)
                     snd_display("trouble in reverse read at %d %s %s", pos, val, y)
                     true
                   else
                     pos += 1
                     false
                   end
                 end)
    revert_sound
  end
  close_sound(ind)
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1)
  insert_silence(0, 1000)
  map_channel($init_channel)
  env_sound([0, 0, 1, 1, 2, 0])
  scale_channel(0.0, 100, 200)
  rd = make_sample_reader(frames - 1, ind, 0, -1)
  map_channel(lambda do |y| read_sample(rd) end)
  pos = 0
  e = make_env([0, 0, 1, 1, 2, 0], :end, 1000)
  scan_channel(lambda do |y|
                 val = env(e)
                 if ((pos > 900 or pos <= 700) and fneq(val, y)) or
                     (pos > 700 and pos <= 900 and fneq(y, 0.0))
                   snd_display("trouble in reverse read 2 at %d %s %s", pos, val, y)
                   true
                 else
                   pos += 1
                   false
                 end
               end)
  close_sound(ind)
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1)
  insert_silence(0, 150000)
  map_channel($init_channel)
  edpos = edit_position
  7.times do |i|
    scale_channel(0.5, 1000, 12345) if i == 5
    env_sound([0, 0, 1, 1, 2.5, 0, 3, 1, 4, 0])
    case i
    when 1
      delete_samples(50, 100)
    when 2
      insert_samples(300, 100, make_vct!(100) do 0.5 end)
    when 3
      scale_channel(0.0, 1000, 1000)
    when 4
      vct2channel(make_vct!(100) do 0.5 end, 500, 100)
    when 6
      env_sound([0, 1, 1, 0], 10000, 2000)
    end
    rd = make_sample_reader(frames - 1, ind, 0, -1)
    map_channel(lambda do |y| read_sample(rd) end)
    rd = make_sample_reader(frames - 1, ind, 0, -1)
    map_channel(lambda do |y| read_sample(rd) end)
    old_rd = make_sample_reader(0, ind, 0, 1, edit_position(ind, 0) - 2)
    pos = 0
    scan_channel(lambda do |y|
                   if fneq(val = read_sample(old_rd), y)
                     snd_display("trouble in reverse (%d) read at %d %s %s", i, pos, val, y)
                     true
                   else
                     pos += 1
                     false
                   end
                 end)
  end
  set_edit_position(edpos, ind, 0)
  close_sound(ind)
  #
  reader = nil
  last_proc = nil
  scan_again = lambda do
    if sample_reader_at_end?(reader)
      false
    else
      if val = last_proc.call(read_sample(reader))
        [val, sample_reader_position(reader) - 1]
      else
        scan_again.call
      end
    end
  end
  my_scan_chan = lambda do |proc|
    if proc
      last_proc = proc
      reader = make_sample_reader(0)
    end
    scan_again.call
  end
  ind = open_sound("oboe.snd")
  set_cursor(1000, ind, 0)
  if fneq(res = sample(1000), sample)
    snd_display("sample no args: %s %s", sample, res)
  end
  if (res = my_scan_chan.call(lambda do |y| y > 0.1 end)) != [true, 4423]
    snd_display("my_scan_chan: %s?", res.inspect)
  end
  if (res = scan_again.call) != [true, 4463]
    snd_display("scan_again: %s?", res.inspect)
  end
  if (res = find(lambda do |y| find(lambda do |yy| yy > 0.1 end) end)) != [[true, 4423], 0]
    snd_display("find twice: %s?", res.inspect)
  end
  if (res = find(lambda do |y| count_matches(lambda do |yy| yy > 0.1 end) end)) != [2851, 0]
    snd_display("find+count: %s?", res.inspect)
  end
  set_cursor(1000)
  # INFO: set_sample(0.5) isn't possible
  set_sample(1000, 0.5)
  snd_display("set sample no arg: %s %s?", sample(1000), sample(0)) if fneq(sample(1000), 0.5)
  close_sound(ind)
end

def test265
  ind = new_sound("test.snd")
  map_chan(lambda do |y| 1.0 end, 0, 1000)
  env_channel(make_env([0, 1, 1, 1], :scaler, 0.5, :end, 1000))
  check_maxamp(ind, 0.5, "simple scaler")
  check_env_vals("simple scaler", make_env([0, 1, 1, 1], :scaler, 0.5, :end, 1000))
  if edit_position == 2
    undo
  else
    snd_display("env+scl was no-op")
  end
  env_channel(make_env([0, 1, 1, 1], :offset, 0.5, :end, 1000))
  check_maxamp(ind, 1.5, "simple scaler")
  check_env_vals("simple scaler", make_env([0, 1, 1, 1], :offset, 0.5, :end, 1000))
  if edit_position == 2
    undo
  else
    snd_display("env+offset was no-op")
  end
  env_channel(make_env([0, 0, 1, 1, 2, 0], :offset, 0.5, :scaler, 2.0, :end, 1000))
  check_maxamp(ind, 2.5, "off+scl")
  check_env_vals("off+scl", make_env([0, 0, 1, 1, 2, 0], :offset, 0.5, :scaler, 2.0, :end, 1000))
  undo
  env_channel(make_env([0, -1, 1, 0, 2, -1], :offset, 0.5, :scaler, 2.0, :end, 1000))
  check_maxamp(ind, 1.5, "off+scl #2")
  mx = -12.0
  scan_chan(lambda do |y|
              if y > mx
                mx = y
              end
              false
            end)
  snd_display("non abs max: %f (correct: 0.5)", mx) if fneq(mx, 0.5)
  check_env_vals("off+scl #2",
                 make_env([0, -1, 1, 0, 2, -1], :offset, 0.5, :scaler, 2.0, :end, 1000))
  undo
  env_sound([0, 0.5, 1, 0.75, 2, 0.25], 0, frames, 32.0)
  check_maxamp(ind, 0.75, "xramp")
  check_env_vals("xramp", make_env([0, 0.5, 1, 0.75, 2, 0.25], :base, 32.0, :end, 1000))
  undo
  env_channel_with_base([0, 0.5, 1, 0.75, 2, 0.25], 32.0)
  check_maxamp(ind, 0.75, "xramp1")
  check_env_vals("xramp1", make_env([0, 0.5, 1, 0.75, 2, 0.25], :base, 32.0, :end, 1000))
  close_sound(ind)
  #
  hlb = make_hilbert_transform(8)
  data = make_vct!(20) do |i| hilbert_transform(hlb, (i == 0 ? 1.0 : 0.0)) end
  unless vequal(data, vct(0.0, -0.010, 0.0, -0.046, 0.0, -0.152, 0.0, -0.614, 0.0, 0.614,
                          0.0, 0.152, 0.0, 0.046, 0.0, 0.010, 0.0, 0.0, 0.0, 0.0))
    snd_display("hilbert_transform impule response: %s?", data.inspect)
  end
  ind = new_sound("test.snd")
  pad_channel(0, 1000)
  set_sample(100, 1.0)
  h = make_hilbert_transform(100)
  4.times do map_channel(lambda do |y| hilbert_transform(h, y) end) end
  if (sample(500) - 0.98).abs > 0.01
    snd_display("hilbert impule: %s", sample(500))
  end
  set_sample(500, 0.0)
  if maxamp(ind, 0) > 0.02
    snd_display("hilbert sidelobes: %s", maxamp(ind, 0))
  end
  revert_sound
  pad_channel(0, 1000)
  set_sample(100, 1.0)
  lo = make_lowpass(PI * 0.1, 20)
  hi = make_highpass(PI * 0.1, 20)
  map_channel(lambda do |y| lowpass(lo, y) + highpass(hi, y) end)
  if fneq(res = sample(120), 1.0)
    snd_display("lowpass+highpass impulse: %s", res)
  end
  set_sample(120, 0.0)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("lowpass+highpass sidelobes: %s", res)
  end
  undo(2)
  lo = make_bandpass(PI * 0.1, PI * 0.2, 20)
  hi = make_bandstop(PI * 0.1, PI * 0.2, 20)
  map_channel(lambda do |y| bandpass(lo, y) + bandpass(hi, y) end)
  if fneq(res = sample(120), 1.0)
    snd_display("bandpass+bandstop impulse: %s", res)
  end
  set_sample(120, 0.0)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("bandpass+bandstop sidelobes: %s", res)
  end
  close_sound(ind)
  # 
  ind = new_sound("test.snd")
  map_channel(lambda do |y| 1.0 - my_random(2.0) end, 0, 10000)
  f2 = make_bandpass_2(0.12 * PI, 0.15 * PI, 0.22 * PI, 0.25 * PI, 100)
  map_channel(lambda do |y| bandpass_2(f2, y) end)
  data = channel2vct
  undo
  f1 = make_bandpass(0.12 * PI, 0.15 * PI, 100)
  f2 = make_bandpass(0.22 * PI, 0.25 * PI, 100)
  map_channel(lambda do |y| bandpass(f1, y) + bandpass(f2, y) end)
  data1 = channel2vct
  vct_subtract!(data, data1)
  snd_display("fir_filter 2: %s", vct_peak(data)) if vct_peak(data) > 0.00001
  undo
  close_sound(ind)
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "ramp re-order tests", 100)
  map_channel(lambda do |y| 1.0 end)
  [["ramp-xramp", true,
      lambda do
        env_sound([0, 0, 1, 1, 2, 0])
        env_sound([0, 0, 1, 1], 0, 100, 2.0)
      end,
      lambda do
        env_sound([0, 0, 1, 1], 0, 100, 2.0)
        env_sound([0, 0, 1, 1, 2, 0])
      end],
    ["ramp2-xramp (1)", true,
      lambda do
        env_sound([0, 0, 1, 1, 2, 0])
        env_sound([0, 0, 1, 1, 3, 0])
        env_sound([0, 0, 1, 1], 0, 100, 2.0)
      end,
      lambda do 
        env_sound([0, 0, 1, 1, 2, 0])
        env_sound([0, 0, 1, 1], 0, 100, 2.0)
        env_sound([0, 0, 1, 1, 3, 0])
      end],
    ["ramp2-xramp (2)", true,
      lambda do
        env_sound([0, 0, 1, 1, 2, 0])
        env_sound([0, 0, 1, 1])
        env_sound([0, 0, 1, 1, 3, 0], 0, 100, 2.0)
      end,
      lambda do 
        env_sound([0, 0, 1, 1, 3, 0], 0, 100, 2.0)
        env_sound([0, 0, 1, 1, 2, 0])
        env_sound([0, 0, 1, 1])
      end],
    ["xramp2-ramp (1)", true,
      lambda do
        env_sound([0, 0, 1, 1, 2, 0], 0, 100, 2.0)
        env_sound([0, 0, 1, 1])
        env_sound([0, 0, 1, 1, 3, 0], 0, 100, 3.0)
      end,
      lambda do 
        env_sound([0, 0, 1, 1, 2, 0], 0, 100, 2.0)
        env_sound([0, 0, 1, 1, 3, 0], 0, 100, 3.0)
        env_sound([0, 0, 1, 1])
      end],
    ["xramp2-ramp (2)", true,
      lambda do
        env_sound([0, 0, 1, 1, 2, 0], 0, 100, 2.0)
        env_sound([0, 0, 1, 1, 3, 0])
        env_sound([0, 0, 1, 1], 0, 100, 3.0)
      end,
      lambda do 
        env_sound([0, 0, 1, 1, 3, 0])
        env_sound([0, 0, 1, 1, 2, 0], 0, 100, 2.0)
        env_sound([0, 0, 1, 1], 0, 100, 3.0)
      end],
    ["ramp4", true,
      lambda do
        env_sound([0, 0, 1, 1])
        env_sound([0, 0, 1, 1, 2, 0])
        env_sound([0, 0, 1, 1, 3, 0])
        env_sound([0, 0, 1, 1, 4, 0])
      end,
      lambda do 
        env_sound([0, 0, 1, 1, 4, 0])
        env_sound([0, 0, 1, 1, 2, 0])
        env_sound([0, 0, 1, 1, 3, 0])
        env_sound([0, 0, 1, 1])
      end]].each do |name, try_scale, f1, f2|
    edpos = edit_position(ind, 0)
    f1.call
    v1 = channel2vct(0, 100, ind, 0)
    set_edit_position(edpos, ind, 0)
    f2.call
    v2 = channel2vct(0, 100, ind, 0)
    unless vequal(v1, v2)
      snd_display("env reordering test %s: %s %s", name, v1.inspect, v2.inspect)
    end
    set_edit_position(edpos, ind, 0)
    if try_scale
      scale_by(2.0)
      f1.call
      v1 = channel2vct(0, 100, ind, 0)
      set_edit_position(edpos, ind, 0)
      f2.call
      scale_by(2.0)
      v2 = channel2vct(0, 100, ind, 0)
      unless vequal(v1, v2)
        snd_display("scaled (2) env reordering test %s: %s %s", name, v1.inspect, v2.inspect)
      end
      set_edit_position(edpos, ind, 0)
      f1.call
      scale_by(0.5)
      v1 = channel2vct(0, 100, ind, 0)
      set_edit_position(edpos, ind, 0)
      scale_by(0.5)
      f2.call
      v2 = channel2vct(0, 100, ind, 0)
      unless vequal(v1, v2)
        snd_display("scaled (0.5) env reordering test %s: %s %s", name, v1.inspect, v2.inspect)
      end
      set_edit_position(edpos, ind, 0)
    end
  end
  close_sound(ind)
  # offset channel
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "offset tests", 10)
  offset_channel(0.1)
  unless vequal(res = channel2vct(0, 10), make_vct(10, 0.1))
    snd_display("offset_channel (0.1): %s", res.inspect)
  end
  offset_channel(-0.2, 5, 5)
  unless vequal(res = channel2vct(0, 10),
                vct(0.1, 0.1, 0.1, 0.1, 0.1, -0.1, -0.1, -0.1, -0.1, -0.1))
    snd_display("offset_channel (-0.1): %s", res.inspect)
  end
  undo
  offset_channel(0.9, 0, 10, ind, 0)
  unless vequal(res = channel2vct(0, 10), make_vct(10, 1.0))
    snd_display("offset_channel (1): %s", res.inspect)
  end
  revert_sound(ind)
  # sine_env and sine_ramp...
  map_channel($init_channel)
  sine_ramp(0.0, 1.0)
  unless vequal(res = channel2vct,
                vct(0.000, 0.024, 0.095, 0.206, 0.345, 0.500, 0.655, 0.794, 0.905, 0.976))
    snd_display("sine_ramp 0 1: %s", res.inspect)
  end
  revert_sound(ind)
  offset_channel(1.0)
  sine_ramp(1.0, 0.0)
  unless vequal(res = channel2vct,
                vct(1.000, 0.976, 0.905, 0.794, 0.655, 0.500, 0.345, 0.206, 0.095, 0.024))
    snd_display("sine_ramp 1 0: %s", res.inspect)
  end
  close_sound(ind)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "sine_env tests", 100)
  # map_channel($init_channel)
  map_channel(lambda do |y| 1.0 end)
  sine_env_channel([0, 0, 1, 1, 2, -0.5, 3, 1])
  if (not vequal(res1 = channel2vct(20, 10),
                 vct(0.664, 0.708, 0.750, 0.790, 0.827, 0.862, 0.893, 0.921, 0.944, 0.964))) or
      (not vequal(res2 = channel2vct(60, 10),
                  vct(-0.381, -0.417, -0.446, -0.47, -0.486, -0.497, -0.5, -0.497, -0.486, -0.47)))
    snd_display("sine_env_channel 0:\n# %s\n# %s", res1.inspect, res2.inspect)
  end
  if (res = edit_position(ind, 0)) != 2
    snd_display("as_one_edit sine_env_channel: %s", res)
  end
  revert_sound(ind)
  offset_channel(-1.0)
  sine_env_channel([0, 0, 1, 1, 2, 1, 3, 0], 40, 20)
  if (not vequal(res1 = channel2vct(40, 20),
                 vct(-0.000, -0.050, -0.188, -0.389, -0.611, -0.812, -0.950, -1.000,
                     -1.000, -1.000, -1.000, -1.000, -1.000, -1.000, -1.000, -0.950,
                     -0.812, -0.611, -0.389, -0.188))) or
      (not vequal(res2 = channel2vct(30, 10), make_vct(10, -1.0)))
    snd_display("off+sine_env:\n# %s\n# %s", res1.inspect, res2.inspect)
  end
  revert_sound(ind)
  ptree_channel(lambda do |y, d, f| y * 2.0 end, 0, frames, ind, 0, false, false,
                lambda do |p, d| vct(1.0) end)
  revert_sound(ind)
  scale_by(0.0)
  dither_channel
  mx = maxamp
  snd_display("dithering: %s", mx) if mx < 0.00003 or mx > 0.0001
  revert_sound(ind)
  map_channel(ring_mod(10, [0, 0, 1, hz2radians(100)]))
  map_channel(osc_formants(0.99, [400, 800, 1200], [400, 800, 1200], [4, 2, 3]))
  map_channel(zecho(0.5, 0.75, 6, 10.0))
  map_channel(flecho(0.5, 0.9))
  filtered_env([0, 0, 1, 1, 2, 0])
  map_channel(formant_filter(0.99, 2400))
  map_channel(comb_filter(0.8, 32))
  map_channel(zcomb(0.8, 32, [0, 0, 1, 10]))
  map_channel(notch_filter(0.8, 32))
  ind1 = open_sound("now.snd")
  select_sound(ind1)
  snd_display("squelch_vowels init: %s?", maxamp) if fneq(maxamp, 0.309)
  squelch_vowels
  snd_display("squelch_vowels maxamp: %s?", maxamp) if ffneq(maxamp, 0.047)
  select_sound(ind)
  map_channel(cross_synthesis(ind1, 0.5, 128, 6.0))
  revert_sound(ind1)
  fft_edit(40, 8000)
  fft_squelch(0.1)
  close_sound(ind)
  revert_sound(ind1)
  scramble_channel(0.01)
  revert_sound(ind1)
  close_sound(ind1)
end

def test275
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "special env tests", 100)
  map_channel($init_channel)
  blackman4_ramp(0.0, 1.0)
  vals = channel2vct
  undo
  blackman4_env_channel([0, 0, 1, 1])
  unless vequal(res = channel2vct, vals)
    snd_display("blackman4_env_channel/ramp:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  blackman4_ramp(0.0, 1.0, 0, 50)
  vals = channel2vct
  undo
  blackman4_env_channel([0, 0, 1, 1, 2, 1])
  unless vequal(res = channel2vct, vals)
    snd_display("blackman4_env_channel/ramp 1:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  blackman4_env_channel([0, 0, 1, 1, 2, -0.5, 3, 0])
  unless vequal(res = channel2vct(60, 10),
                vct(-0.109, -0.217, -0.313, -0.392, -0.451, -0.488, -0.499, -0.499, -0.499, -0.499))
    snd_display("blackman4_env_channel to -0.5: %s", res.inspect)
  end
  undo
  # 
  ramp_squared(0.0, 1.0)
  vals = channel2vct
  undo
  env_squared_channel([0, 0, 1, 1])
  unless vequal(res = channel2vct, vals)
    snd_display("env_squared/ramp:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  ramp_squared(0.0, 1.0, true, 0, 50)
  vals = channel2vct
  undo
  env_squared_channel([0, 0, 1, 1, 2, 1])
  unless vequal(res = channel2vct, vals)
    snd_display("env_squared/ramp 1:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 0])
  unless vequal(res = channel2vct(60, 10),
                vct(-0.450, -0.466, -0.478, -0.488, -0.494, -0.499, -0.500, -0.500, -0.498, -0.496))
    snd_display("env_squared to -0.5: %s", res.inspect)
  end
  undo
  env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 0], false)
  unless vequal(res = channel2vct(60, 10),
                vct(-0.004, -0.080, -0.158, -0.240, -0.324, -0.410, -0.500, -0.500, -0.498, -0.496))
    snd_display("env_squared unsymmetric to -0.5: %s", res.inspect)
  end
  undo
  # 
  ramp_squared(0.0, 1.0)
  vals = channel2vct
  undo
  env_expt_channel([0, 0, 1, 1], 2)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt2/ramp:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 0])
  vals = channel2vct
  undo
  env_expt_channel([0, 0, 1, 1, 2, -0.5, 3, 0], 2.0)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt2/env_squared:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 0], false)
  vals = channel2vct
  undo
  env_expt_channel([0, 0, 1, 1, 2, -0.5, 3, 0], 2.0, false)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt2/env_squared unsymmetric:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  #
  ramp_expt(0.0, 1.0, 32.0)
  vals = channel2vct
  undo
  env_expt_channel([0, 0, 1, 1], 32.0)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt/ramp 32:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  ramp_expt(0.0, 1.0, 32.0, false, 0, 50)
  vals = channel2vct
  undo
  env_expt_channel([0, 0, 1, 1, 2, 1], 32.0)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt/ramp 1 32.0:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  ramp_expt(0.0, 1.0, 0.1)
  vals = channel2vct
  undo
  env_expt_channel([0, 0, 1, 1], 0.1)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt/ramp 0.1:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  env_expt_channel([0, 0, 1, 1, 2, -0.5, 3, 0], 12.0)
  unless vequal(res = channel2vct(30, 10),
                vct(0.319, 0.472, 0.691, 1.000, 0.537, 0.208, -0.022, -0.182, -0.291, -0.365))
    snd_display("env_expt to -0.5 12.0\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  env_expt_channel([0, 0, 1, 1, 2, -0.5, 3, 0], 12.0, false)
  unless vequal(res = channel2vct(30, 10),
                vct(0.319, 0.472, 0.691, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000))
    snd_display("env_expt ot -0.5 12.0 unsymmetric:\n# %s\n# %s", vals.inspect, res.inspect)
  end
  undo
  close_sound(ind)
  #
  ind0 = open_sound("oboe.snd")
  ind1 = open_sound("pistol.snd")
  4.times do ramp_channel(0.0, 1.0, 0, false, ind1, 0) end
  make_selection(1000, 2000, ind1, 0)
  set_sync(1, ind0)
  set_selected_sound(ind0)
  env_selection([0, 0, 1, 1])
  if (res0 = edit_position(ind0, 0)) != 0 or (res1 = edit_position(ind1)) != 5
    snd_display("selection override of sync field: %s %s?", res0, res1)
  end
  env_sound([0, 0, 1, 1, 2, 0])
  if (res0 = edit_position(ind0, 0)) != 1 or (res1 = edit_position(ind1)) != 5
    snd_display("sync field over selection: %s %s?", res0, res1)
  end
  close_sound(ind0)
  close_sound(ind1)
end

def test05
  test005
  test015
  test025
  test035
  test045
  test055
  test065
  test075
  test085
  test095
  test105 unless provided? "snd-nogui"
  test115
  test125
  test135
  test145
  test155
  test165
  test175
  test185
  test195
  test205
  test215
  test225
  test235
  test245
  test255
  test265
  test275
  delete_file("test.snd")
end

if $test05 and $full_test or $snd_test == 5
  $before_test_hook.call(5)
  test05
  $after_test_hook.call(5)
end

# ---------------- test 06: vcts ----------------

def test06
  $snd_error_hook.reset_hook!
  $mus_error_hook.reset_hook!
  $snd_error_hook.add_hook!("test 6") do |msg|
    if msg.empty?
      snd_display_error("<SND-INTERRUPT>")
    else
      snd_display_error("<SND-ERROR: %s>", msg)
    end
    true
  end
  $mus_error_hook.add_hook!("test 6") do |type, msg|
    if type.zero?
      snd_display_error("<MUS-WARNING: %s>", msg)
    else
      snd_display_error("<MUS-ERROR %s: %s>", mus_error_to_string(type), msg)
    end
    true
  end
  # 
  $snd_error_hook.reset_hook!
  $mus_error_hook.reset_hook!
end

if $test06 and $full_test or $snd_test == 6
  $before_test_hook.call(6)
  test06
  $after_test_hook.call(6)
end

# ---------------- test all done

$overall_start_time.stop
set_previous_files_sort(0)
delete_file("saved-snd.rb")
clear_sincs
stop_playing
regions.each do |n| forget_region(n) end if regions
tracks.each do |n| free_track(n) end if tracks
snd_info("all done!\n#")
$timings.each do |tst| snd_info("test %2d %s", tst.first, tst.last.inspect) end
snd_info("total   %s\n", $overall_start_time.inspect)
save_listener("test.output")
mus_audio_playback_amp($orig_audio_amp)

exit if $with_exit

# snd-test.rb ends here
