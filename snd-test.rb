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
#  test all done

$VERBOSE = false
$DEBUG = false

require "examp.rb"
require "ws.rb"
require "hooks.rb"
require "extensions.rb"
require "mix.rb"
require "marks.rb"
require "pvoc.rb"
require "bird.rb"
require "v.rb"
unless provided? :snd_nogui
  require "rgb.rb"
  provided?(:snd_motif) and (not provided?(:xm)) and require("libxm.so")
  provided?(:snd_gtk)   and (not provided?(:xg)) and require("libxg.so")
  require "snd-xm.rb"
  include Snd_XM
  require "popup.rb"
end

reset_all_hooks

# global variables marked with "unless defined?" may be set in
# `pwd`/.sndtestrc or ~/.sndtestrc
load_init_file(".sndtestrc")

$original_save_dir = (save_dir or "/zap/snd")
$original_temp_dir = (temp_dir or "/zap/tmp")
$original_prompt = listener_prompt
$sample_reader_tests = 300
$default_file_buffer_size = 65536
$home_dir = ENV["HOME"]

$audio_amp_zero = 0.0        unless defined? $audio_amp_zero
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

if provided? :snd_nogui
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

def fneq_err(f1, f2, err = 0.001)
  (f1 - f2).abs > err
end

def fneq(a, b)
  fneq_err(a, b, 0.001)
end

def ffneq(a, b)
  fneq_err(a, b, 0.01)
end

def fffneq(a, b)
  fneq_err(a, b, 0.1)
end

def any2vct(obj)
  case obj
  when Vct
    obj
  when Array
    if obj.empty?
      nil
    else
      obj.to_vct
    end
  else
    nil
  end
end

# compares Arrays and Vcts
def vequal_err(val0, val1, err = 0.001)
  (v0 = any2vct(val0)) and (v1 = any2vct(val1)) and v0.subtract(v1).peak <= err
end

def vequal(v0, v1)
  vequal_err(v0, v1, 0.001)
end

def vvequal(v0, v1)
  vequal_err(v0, v1, 0.00002)
end

def vfequal(v0, v1)
  vequal_err(v0, v1, 0.01)
end

def vffequal(v0, v1)
  vequal_err(v0, v1, 0.1)
end

def cneq(a, b)
  (a.real - b.real).abs > 0.001 or (a.image - b.image).abs > 0.001
end

def log0(try_it = true)
  try_it ? log(0) : 0.0
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
mus_audio_playback_amp($audio_amp_zero)

def make_color_with_catch(c1, c2, c3)
  make_color(c1, c2, c3)
rescue
  make_color(1, 0, 0)
end

def file_copy(f1, f2)
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
    if (res = snd_catch(tag) do snd_throw(tag, "snd-test") end).first != tag
      snd_display("snd_catch (throwing 1): %s -> %s", tag.inspect, res.inspect)
    end
  end
  Snd_error_tags.each do |tag|
    if (res = snd_catch(tag) do snd_raise(tag, "snd-test") end).first != tag
      snd_display("snd_catch (raising 1): %s -> %s", tag.inspect, res.inspect)
    end
  end
  Snd_error_tags.each do |tag|
    if (res = snd_catch(tag, :okay) do snd_throw(tag, "snd-test") end).first != :okay
      snd_display("snd_catch (throwing 2): %s -> %s", tag.inspect, res.inspect)
    end
  end
  Snd_error_tags.each do |tag|
    if (res = snd_catch(tag, :okay) do snd_raise(tag, "snd-test") end).first != :okay
      snd_display("snd_catch (raising 2): %s -> %s", tag.inspect, res.inspect)
    end
  end
end
snd_error_test if $full_test

show_listener
set_window_x(600)
set_window_y(10)

def irandom(n)
  random(n).to_i
end

class Snd_test_time
  def initialize
    @real_time = Time.now
    @process_time = process_times
    @real = @utime = @stime = 0.0
  end
  attr_reader :real, :utime, :stime
  
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
  
  def display(msg)
    str = if msg
            " (" + msg + ")"
          else
            ""
          end
    snd_info("real: %1.3f, utime: %1.3f, stime: %1.3f%s", @real, @utime, @stime, str) 
  end

  def run(msg, &body)
    start
    ret = body.call
    stop
    display(msg)
    ret
  end
end

# returns last result in body
def with_time(msg = nil, &body)
  wt = Snd_test_time.new
  wt.run(msg, &body)
end

# map_chan* procedure
$init_channel = lambda do |y| 1.0 end
$timings = Array.new(0)

$before_test_hook = make_hook("$before_test_hook", 1, "snd-test") do |n|
  $timings.push([n, Snd_test_time.new])
  snd_info("test %d", n)
  set_show_backtrace(false)
end

$after_test_hook = make_hook("$after_test_hook", 1, "snd-test") do |n|
  $timings.last.last.stop
  if sounds
    snd_info("test %d: open sounds: %s", n, short_file_name(true))
    sounds.each do |snd| close_sound(snd) end
  end
  dismiss_all_dialogs unless provided? :snd_nogui
  snd_info("test %d done\n#", n)
end

unless hook? $before_test_hook
  snd_display("$before_test_hook not a hook: %s?", $before_test_hook.inspect)
end
unless hook? $after_test_hook
  snd_display("$after_test_hook not a hook: %s?", $after_test_hook.inspect)
end

snd_info("===  Snd version: %s", snd_version)
snd_info("=== Ruby version: %s (%s) [%s]", RUBY_VERSION, RUBY_RELEASE_DATE, RUBY_PLATFORM)
snd_info("%s\n#", Time.now.localtime.strftime("%a %d-%b-%Y %H:%M %Z"))
$overall_start_time = Snd_test_time.new

# snd-test.scm translations
# ---------------- test 00: constants ----------------

# list = [[:Symbol, value], ...]
def test000(lst, exec = false)
  if exec
    # global snd_var functions
    lst.each do |sym, val|
      next unless symbol?(sym)
      begin
        if (res = set_snd_var(sym, snd_var(sym))) != val
          snd_display("set_%s: %s?", sym, res)
        end
      rescue
        snd_display("set_%s: %s (%s)", sym, res, $!.inspect)
      end
    end
  else
    # constants
    lst.each do |sym, val|
      next unless symbol?(sym)
      begin
        if (res = eval(sym.to_s)) != val
          snd_display("%s => %s?", sym, res)
        end
      rescue
        snd_display("%s => %s (%s)", sym, res, $!.inspect)
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
    snd_display("set_zero_pad(true, true): %s?", zero_pad(true, true))
  end
  if provided? :snd_motif
    [:axis_label_font,
      :axis_numbers_font,
      :tiny_font,
      :peaks_font,
      :bold_peaks_font].each do |sym|
      val = snd_var(sym)
      if (res = set_snd_var(sym, "8x123")) != val
        snd_display("set_%s to bogus value: %s %s?", sym, val, res)
      end
    end
  end
  unless provided? :snd_nogui
    # XEN_EMPTY_LIST is Qnil
    # set_enved_envelope([]) sets enved_envelope to nil
    set_enved_envelope((enved_envelope or []))
    if enved_envelope != nil
      snd_display("set_enved_envelope: %s?", enved_envelope)
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
  unless provided? :snd_nogui
    [:Time_graph, 0]
  end,
  unless provided? :snd_nogui
    [:Transform_graph, 1]
  end,
  unless provided? :snd_nogui
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
  [:Mus_chebyshev_first_kind, 1],
  [:Mus_chebyshev_second_kind, 2],
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
  unless provided? :snd_nogui
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
  unless provided? :snd_nogui
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
  [:spectro_x_angle, (provided?(:gl) ? 300.0 : 90.0)],
  [:spectro_x_scale, (provided?(:gl) ? 1.5 : 1.0)],
  [:spectro_y_angle, (provided?(:gl) ? 320.0 : 0.0)],
  [:spectro_y_scale, 1.0],
  [:spectro_z_angle, (provided?(:gl) ? 0.0 : 358.0)],
  [:spectro_z_scale, (provided?(:gl) ? 1.0 : 0.1)],
  [:temp_dir, ""],
  [:ladspa_dir, ""],
  unless provided? :snd_nogui
    [:tiny_font, if provided? :snd_motif
                   "6x12"
                 elsif provided? :snd_gtk
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
    snd_info("start up sounds: %s mixes: %s marks: %s regions: %s", sounds, mixes, marks, regions)
  end
  test00(consts, defs)
  $after_test_hook.call(0)
end

# ---------------- test 01: defaults ----------------

$good_colormap = provided?(:gl) ? 2 : 0
$better_colormap = 0

# :normal        = [[Symbol, val], ...]
# :without_error = [Symbol, ...]
# :cadr          = [[Symbol, val], ...]
def test001(lst, type = :normal)
  case type
  when :normal
    lst.each do |sym, val|
      next unless symbol?(sym)
      begin
        if (res = snd_var(sym)) != val
          snd_display("%s %s != %s?", sym, res, val)
        end
      rescue
        snd_display("%s => %s (%s)", sym, res, $!.inspect)
      end
    end
  when :without_error
    lst.each do |sym|
      next unless symbol?(sym)
      if (res = snd_catch do snd_var(sym) end).first != :no_such_sound
        snd_display("%s %s != :No_such_sound?", sym, res.inspect)
      end
    end
  when :cadr
    lst.each do |sym, val|
      next unless symbol?(sym)
      begin
        if (res = snd_var(sym)[1]) != val
          snd_display("%s %s != %s?", sym, res, val)
        end
      rescue
        snd_display("%s => %s (%s)", sym, res, $!.inspect)
      end
    end
  end
end

def test01(controls, specials, cadr)
  test001(controls, :normal)
  test001(specials, :without_error)
  test001(cadr, :cadr)
  unless provided? :snd_nogui
    # XEN_EMPTY_LIST is Qnil
    # set_enved_envelope([]) sets enved_envelope to nil
    set_enved_envelope((enved_envelope or []))
    if enved_envelope != nil
      snd_display("set_enved_envelope: %s?", enved_envelope)
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
  unless provided? :snd_nogui
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
  unless provided? :snd_nogui
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
  unless provided? :snd_nogui
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
  [:spectro_x_angle, (provided?(:gl) ? 300.0 : 90.0)],
  [:spectro_x_scale, (provided?(:gl) ? 1.5 : 1.0)],
  [:spectro_y_angle, (provided?(:gl) ? 320.0 : 0.0)],
  [:spectro_y_scale, 1.0],
  [:spectro_z_angle, (provided?(:gl) ? 0.0 : 358.0)],
  [:spectro_z_scale, (provided?(:gl) ? 1.0 : 0.1)],
  [:temp_dir, ""],
  [:ladspa_dir, ""],
  unless provided? :snd_nogui
    [:tiny_font, if provided? :snd_motif
                   "6x12"
                 elsif provided? :snd_gtk
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
  [:with_mix_tags, (provided?(:snd_nogui) ? false : true)],
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

unless provided? :snd_nogui
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
      next unless symbol?(sym)
      set_snd_var(sym, newval)
      if float?(initval)
        if fneq((nowval = snd_var(sym)), newval)
          snd_display("set_%s %s != %s?", sym, nowval, newval)
        end
      else
        if (nowval = snd_var(sym)) != newval
          snd_display("set_%s %s != %s?", sym, nowval, newval)
        end
      end
      set_snd_var(sym, initval)
    end
  when :bad_args
    lst.each do |sym, initval, newval|
      next unless symbol?(sym)
      begin
        set_snd_var(sym, newval)
      rescue
        set_snd_var(sym, initval)
      end
      if (nowval = snd_var(sym)) == newval
        snd_display("set_%s (bad set) %s == %s?", sym, nowval, newval)
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
    snd_display("sample: %s?", sample(1000))
  end
  set_show_controls true
  unless provided? :snd_nogui
    Snd_hooks.each_with_index do |h, i|
      snd_display("Snd_hooks[%d] %s?", i, h.inspect) unless hook?(h)
    end
    wid = enved_dialog
    if dialog_widgets[2] != wid
      snd_display("enved_dialog -> %s %s?", wid, dialog_widgets[2])
    end
    snd_display("enved_dialog?") unless dialog_widgets[2]
    set_enved_envelope([0.0, 0.0, 1.0, 1.0, 2.0, 0.0])
    if enved_envelope != [0.0, 0.0, 1.0, 1.0, 2.0, 0.0]
      snd_display("set_enved_envelope: %s?", enved_envelope)
    end
    set_enved_envelope(enved_envelope)
    if enved_envelope != [0.0, 0.0, 1.0, 1.0, 2.0, 0.0]
      snd_display("set_enved_envelope to self: %s?", enved_envelope)
    end
    wid = orientation_dialog
    snd_display("orientation_dialog?") unless dialog_widgets[1]
    if dialog_widgets[1] != wid
      snd_display("orientation_dialog -> %s %s?", wid, dialog_widgets[1])
    end
    test003(vars1, :normal)
    test003(vars2, :bad_args)
    set_enved_filter_order(5)
    if enved_filter_order != 6
      snd_display("set_enved_filter_order 5: %s?", enved_filter_order)
    end
    zero_to_one = [0, 0.0, 50, 0.5, 100, 1.0]
    mod_down = [0, 1.0, 50, 0.5, 100, 0.0]
    set_enved_envelope(:zero_to_one)
    if enved_envelope != zero_to_one
      snd_display("set_enved_envelope (Symbol): %s %s?", enved_envelope, zero_to_one)
    end
    set_enved_envelope("mod_down")
    if enved_envelope != mod_down
      snd_display("set_enved_envelope (String): %s %s?", enved_envelope, mod_down)
    end
  end
  if proc?(search_procedure)
    snd_display("global search procedure: %s?", search_procedure)
  end
  set_search_procedure(lambda do |y| y > 0.1 end)
  unless proc?(search_procedure)
    snd_display("set_search_procedure: %s?", search_procedure)
  end
  snd_display("search 0.1 > 0.2?") unless search_procedure.call(0.2)
  snd_display("search 0.1 > 0.02?") if search_procedure.call(0.02)
  set_search_procedure(lambda do |y| y < 0.0 end)
  snd_display("search 0.0 < 0.02?") if search_procedure.call(0.02)
  set_search_procedure(false)
  if proc?(search_procedure)
    snd_display("set_search_procedure after reset: %s?", search_procedure)
  end
  set_search_procedure(lambda do |y| y > 0.1 end)
  unless proc?(search_procedure)
    snd_display("set_search_procedure: %s?", search_procedure)
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
  unless provided? :snd_nogui
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
  unless provided? :snd_nogui
    [:enved_filter, true, false]
  end,
  [:enved_filter_order, 40, 20],
  [:filter_control_in_hz, false, true],
  [:filter_control_order, 20, 40],
  [:filter_control?, false, true],
  unless provided? :snd_nogui
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
  [:spectro_x_angle, (provided?(:gl) ? 300.0 : 90.0), 60.0],
  [:spectro_x_scale, (provided?(:gl) ? 1.5 : 1.0), 2.0],
  [:spectro_y_angle, (provided?(:gl) ? 320.0 : 0.0), 60.0],
  [:spectro_y_scale, 1.0, 2.0],
  [:spectro_z_angle, (provided?(:gl) ? 0.0 : 358.0), 60.0],
  [:spectro_z_scale, (provided?(:gl) ? 1.0 : 0.1), 0.2],
  [:speed_control, 1.0, 0.5],
  [:speed_control_bounds, [0.05, 20.0], [1.0, 5.0]],
  [:speed_control_style, 0, 1],
  [:speed_control_tones, 12, 18],
  [:sync, 0, 1],
  if provided? :snd_motif
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
  [:with_mix_tags, (provided?(:snd_nogui) ? false : true), false],
  [:with_relative_panes, true, false],
  [:with_gl, provided?(:gl), false],
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
  unless provided? :snd_nogui
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
    snd_display("can\'t play %s", file)
  else
    0.step(frames, bufsize) do
      mus_sound_read(sound_fd, 0, bufsize - 1, chans, data)
      mus_audio_write(audio_fd, data, bufsize)
    end
    mus_audio_close(audio_fd)
  end
rescue
  snd_display("can\'t open audio")
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
  in_port = snd_catch(:mus_error, -1) do
    mus_audio_open_input(in_sys << 16 | Mus_audio_default,
                                 our_srate, our_chans, our_short,
                                 our_dac_buffer_size_in_bytes)
  end.first
  if in_port == -1
    snd_display("can\'t open audio input port")
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
    snd_display("mus_sound_maxamp z.snd: %s?", mz)
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
    snd_display("oboe: mus_sound_maxamp_exists? before maxamp: %s?", m1)
  end
  unless mus_sound_maxamp_exists?(oboe_snd)
    snd_display("oboe: mus_sound_maxamp_exists? after maxamp: %s?",
                mus_sound_maxamp_exists?(oboe_snd))
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
    snd_display("mus_audio_sun_outputs: %s?", mus_audio_sun_outputs(1, 2, 3))
  end
end

def test014
  ind = open_sound("oboe.snd")
  lfname = "test" + "-test" * 32 + ".snd"
  snd_display("variable_graph thinks anyting is a graph...") if variable_graph?(ind)
  snd_display("player? thinks anything is a player...") if player?(ind)
  snd_display("%s is not a sound?", ind) unless sound?(ind)
  save_sound_as(lfname, ind)
  close_sound(ind)
  ind = open_sound(lfname)
  snd_display("can\'t find test...snd") unless sound?(ind)
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
      snd_display("saved loop_info: %s?", mus_sound_loop_info("fmv1.snd"))
    end
  end
  ind = open_sound("oboe.snd")
  save_sound_as("fmv.snd", ind, Mus_aifc)
  close_sound(ind)
  ind = open_sound("fmv.snd")
  if sound_loop_info(ind) != nil
    snd_display("null loop_info: %s?", sound_loop_info(ind))
  end
  set_sound_loop_info(ind, [1200, 1400, 4, 3, 2, 1])
  if sound_loop_info(ind) != [1200, 1400, 4, 3, 2, 1, 1, 1]
    snd_display("set null loop_info: %s?", sound_loop_info(ind))
  end
  save_sound_as("fmv1.snd", :sound, ind, :header_type, Mus_aifc)
  close_sound(ind)
  if mus_sound_loop_info("fmv1.snd") != [1200, 1400, 4, 3, 2, 1, 1, 1]
    snd_display("saved null loop_info: %s?", mus_sound_loop_info("fmv1.snd"))
  end
  ind = open_sound("fmv.snd")
  set_sound_loop_info(ind, [1200, 1400, 4, 3, 2, 1, 1, 0])
  if sound_loop_info(ind) != [1200, 1400, 0, 0, 2, 1, 1, 0]
    snd_display("null set_sound_loop_info (no mode1): %s?", sound_loop_info(ind))
  end
  save_sound_as("fmv1.snd", ind, Mus_aifc)
  close_sound(ind)
  if mus_sound_loop_info("fmv1.snd") != [1200, 1400, 0, 0, 2, 1, 1, 0]
    snd_display("saved null loop_info (no mode1): %s?", mus_sound_loop_info("fmv1.snd"))
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
        snd_display("%s %s => %s != %s?", func, fsnd, res, val)
      end
    end
  end
end

def test044
  oboe_snd = "oboe.snd"
  if (res = snd_catch do set_mus_sound_maxamp(oboe_snd, [1234]) end).first != :wrong_type_arg
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
      snd_display("header_type[%d] == %s?", lasth, mus_header_type_name(lasth))
    end
  end
  let(1) do |lasth|
    until mus_data_format_name(lasth) == "unknown" do lasth += 1 end
    if lasth < 10
      snd_display("data_format[%d] == %s?", lasth, mus_data_format_name(lasth))
    end
  end
  unless provided? :snd_nogui
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
      vct_set!(v, i, 1.0 - random(2.0))
    end
    vct2channel(v, 0, len, ind, 0)
    save_sound_as("test1.snd", ind, Mus_next, :data_format, type)
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
  str = format("written %s", Time.now.localtime.strftime("%a %d-%b-%Y %H:%M %Z"))
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
                :header_type, Mus_riff,
                :data_format, Mus_lfloat)
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
                    vals.map_with_index do |val, i| sample(val[0], ind, i) end)
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
      res = [snd_catch { mus_sound_chans(fsnd) }.first,
             snd_catch { mus_sound_srate(fsnd) }.first,
             snd_catch { mus_sound_frames(fsnd) }.first]
      # bad_length.aifc returns :mus_error
      if res.first != :mus_error and res != vals
        snd_display("%s: %s != %s?", fsnd, res, vals)
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
        number?(snd) and sound?(snd) and close_sound(snd)
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
    snd_display("sound_data_maxamp oboe.snd: %s?", sound_data_maxamp(sd))
  end
  if (res = maxamp(ob, 0)) != mx[0]
    snd_display("sound_data_maxamp oboe.snd: %f != %f?", sound_data_maxamp(sd)[0], res)
  end
  if (res = snd_catch do set_selected_channel(1) end).first != :no_such_channel
    snd_display("set_selected_channel bad chan: %s?", res)
  end
  if (res = snd_catch do set_selected_channel(123456, 1) end).first != :no_such_sound
    snd_display("set_selected_channel bad snd: %s?", res)
  end
  [[2, 1000], [-1, 1000], [0, -1], [0, 10000000]].each do |chn, frm|
    if (res = snd_catch do sound_data_ref(sd, chn, frm) end).first != :out_of_range
      snd_display("sound_data_ref bad chan or frame: %d %d %s?", chn, frm, res.inspect)
    end
  end
  [[2, 1000], [-1, 1000], [0, -1], [0, 10000000]].each do |chn, frm|
    if (res = snd_catch do sound_data_set!(sd, chn, frm, 1) end).first != :out_of_range
      snd_display("sound_data_set! bad chan or frame: %d %d %s?", chn, frm, res.inspect)
    end
  end
  v = make_vct(3)
  if (res = snd_catch do vct2sound_data(v, sd, 2) end).first != :out_of_range
    snd_display("vct2sound_data bad chan: %s?", res.inspect)
  end
  close_sound(ob)
  snd_display("selected_sound %d %s?", selected_sound, sounds) if selected_sound
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
      snd_display("soundfont_info: %s", loops)
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
  if sdata.inspect != "#<sound-data: 1 chan, 100 frames>"
    snd_display(", print sound_data: %s?", sdata.inspect)
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
  if (res = snd_catch do sound_data2vct(sdata, 2, v0) end).first != :out_of_range
    snd_display("sound_data2vct bad chan: %s?", res.inspect)
  end
  if (res = snd_catch do mus_audio_write(1, make_sound_data(3, 3), 123) end).first != :out_of_range
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
      end).first != :out_of_range
    snd_display("mus_sound_open_output bad chans: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_open_output("fmv.snd", 22050, 1, -1, Mus_aiff, "no comment")
      end).first != :out_of_range
    snd_display("mus_sound_open_output bad format: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_open_output("fmv.snd", 22050, 1, Mus_bshort, -1, "no comment")
      end).first != :out_of_range
    snd_display("mus_sound_open_output bad type: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", -1, Mus_bshort, Mus_aiff, false)
      end).first != :out_of_range
    snd_display("mus_sound_reopen_output bad chans: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", 1, -1, Mus_aiff, false)
      end).first != :out_of_range
    snd_display("mus_sound_reopen_output bad format: %s?", res)
  end
  if (res = snd_catch do
        mus_sound_reopen_output("fmv.snd", 1, Mus_bshort, -1, false)
      end).first != :out_of_range
    snd_display("mus_sound_reopen_output bad type: %s?", res)
  end
  delete_file("fmv.snd")
  [:mus_audio_open_output, :mus_audio_open_input].each do |sym|
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, -1, Mus_lshort, 512)
        end).first != :out_of_range
      snd_display("%s bad chans: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, 1, -1, 512)
        end).first != :out_of_range
      snd_display("%s bad format: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, -1, 22050, 1, Mus_lshort, 512)
        end).first != :out_of_range
      snd_display("%s bad device: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, -22050, 1, Mus_lshort, 512)
        end).first != :out_of_range
      snd_display("%s bad srate: %s", sym, res)
    end
    if (res = snd_catch do
          send(sym, Mus_audio_default, 22050, 1, Mus_lshort, -512)
        end).first != :out_of_range
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
      if (res = snd_catch(:all) do open_sound(fsnd) end).first != :mus_error
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
    if (res = snd_catch do send(sym, -1, Mus_audio_amp, 0, vals) end).first != :out_of_range
      snd_display("%s bad device: %s?", sym, res)
    end
    if (res = snd_catch do send(sym, Mus_audio_microphone, -1, 0, vals) end).first != :out_of_range
      snd_display("%s bad fields: %s?", sym, res)
    end
  end
  mus_audio_mixer_write(Mus_audio_microphone, Mus_audio_amp, 0, make_vct(1))
  ind = open_sound("/usr/local/" + Dir.pwd + "/2.snd")
  sd1 = samples2sound_data(12000, 10, ind, 0)
  vc1 = sound_data2vct(sd1)
  vc2 = samples2vct(12000, 10, ind, 0)
  sd2 = vct2sound_data(vc2)
  snd_display("samples2sound_data2vct: %s %s?", vc1, vc2) if vc1 != vc2
  snd_display("sound_data2vct2sound_data: %s %s?", sd1, sd2) if sd1 != sd2
  scale_by(2.0, ind, 0)
  sd1 = samples2sound_data(12000, 10, ind, 0, false, 0)
  vc1 = sound_data2vct(sd1)
  vc2 = samples2vct(12000, 10, ind, 0, false, 0)
  sd2 = vct2sound_data(vc2)
  snd_display("edpos samples2sound_data2vct: %s %s?", vc1, vc2) if vc1 != vc2
  snd_display("edpos sound_data2vct2sound_data: %s %s?", sd1, sd2) if sd1 != sd2
  sd1 = samples2sound_data(12000, 10, ind, 1)
  vc1 = sound_data2vct(sd1)
  vc2 = samples2vct(12000, 10, ind, 1)
  sd2 = vct2sound_data(vc2)
  snd_display("1 samples2sound_data2vct: %s %s?", vc1, vc2) if vc1 != vc2
  snd_display("1 sound_data2vct2sound_data: %s %s?", sd1, sd2) if sd1 != sd2
  scale_by(2.0, ind, 1)
  sd1 = samples2sound_data(12000, 10, ind, 1)
  vc1 = sound_data2vct(sd1)
  vc2 = samples2vct(12000, 10, ind, 1)
  sd2 = vct2sound_data(vc2)
  snd_display("1 scaled samples2sound_data2vct: %s %s?", vc1, vc2) if vc1 != vc2
  snd_display("1 scaled sound_data2vct2sound_data: %s %s?", sd1, sd2) if sd1 != sd2
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
      snd_display(res) if string?(res)
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
    snd_display("read/write: %s?", sound_data2list(sdata))
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
    snd_display("read/write: %s?", sound_data2list(sdata))
  end
  mus_sound_close_input(fd)
  #
  # check data_clipped choices
  #
  ind = view_sound("oboe.snd")
  set_data_clipped(false)
  map_channel(lambda do |y| y * 10.0 end, 0, frames(ind), ind, 0)
  save_sound_as("test.snd", ind, Mus_next, Mus_bfloat)
  undo_edit(1, ind, 0)
  ind1 = open_sound("test.snd")
  if fneq(res1 = maxamp(ind1, 0), res2 = 10.0 * maxamp(ind, 0))
    snd_display("clipping 0: %f %f?", res1, res2)
  end
  close_sound(ind1)
  delete_file("test.snd")
  set_data_clipped(true)
  map_channel(lambda do |y| y * 10.0 end, 0, frames(ind), ind, 0)
  save_sound_as("test.snd", ind, Mus_next, Mus_bfloat)
  undo_edit(1, ind, 0)
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
  unless array?(res = scan_channel(lambda do |y| y < 0.0 end))
    snd_display("clipping 2: %s?", res)
  end
  close_sound(ind1)
  delete_file("test.snd")
  set_data_clipped(true)
  save_sound_as("test.snd", ind, Mus_next, Mus_bshort)
  ind1 = open_sound("test.snd")
  if array?(res = scan_channel(lambda do |y| y < 0.0 end))
    snd_display("clipping 3: %s?", res)
  end
  close_sound(ind1)
  delete_file("test.snd")
  set_data_clipped(false)
  close_sound(ind)
  # 
  delete_file(fmv)
  com = "this is a comment which we\'ll repeat enough times to trigger an internal loop" * 3
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
    snd_display("read/write[%d]: %s?", i, sound_data_channel2list(sdata, i))
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
    snd_display("re-read/write[0]: %s?", sound_data_channel2list(sdata, 0))
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
      snd_display("%s: %s != %s", file, data, ndata) unless vequal(data, ndata)
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
      snd_display("mus_error_to_string %d: %s %s?", i, err, res)
    end
  end
  new_id = mus_make_error("hiho all messed up")
  if (res = mus_error_to_string(new_id)) != "hiho all messed up"
    snd_display("mus_make_error %d: %s?", new_id, res)
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
        undo_edit(2)
      else
        snd_display("no mix tag from mix_sound: %s?", mx)
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
          if (not number?(mid)) or mid != m1
            snd_display("bigger find_mark: %s %s", mid, m1)
          end
        end
      end
      let(mix_sound("oboe.snd", 44123 * 51234)) do |mx|
        mxd = find_mix(44123 * 51234)
        unless number?(mxd) or mix?(mx) or mxd == mx
          snd_display("bigger find_mix: %s %s", mxd, mx)
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
    if array?(err)
      snd_display("%s (%s) selection not saved correctly? %s", df, ht, err)
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
    snd_display("ramp env by 0.1: %s", res)
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
    res = snd_catch do open_sound("test.snd") end.first
    if number?(res) and sound?(res)
      snd_display("open_sound garbage %s: %s?", magic, res)
      close_sound(res)
    end
    delete_file("test.snd")
    mus_sound_forget("test.snd")
    File.open("test.snd", "w") do |fp|
      fp.write magic
      128.times do fp.write(mus_random(128)) end
    end
    res = snd_catch do open_sound("test.snd") end.first
    if number?(res) and sound?(res)
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
    res = snd_catch do open_sound("test.snd") end.first
    if number?(res) and sound?(res)
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
  if number?(res) and sound?(res)
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
  if number?(res) and sound?(res)
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
  if number?(res) and sound?(res)
    snd_display("open_sound aifc no ssnd chunk %d: %d?", data_location(res), res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  make_aifc_file(002, 000, 020)
  res = snd_catch do open_sound("test.aif") end.first
  if number?(res) and sound?(res)
    snd_display("open_sound aifc 0-len auth chunk %d: %d?", data_location(res), res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  make_aifc_file(002, 150, 120)
  res = snd_catch do open_sound("test.aif") end.first
  if number?(res) and sound?(res)
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
  if number?(res) and sound?(res)
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
  if number?(res) and sound?(res)
    snd_display("open_sound aifc no comm chunk: %s?", res)
    close_sound(res)
  end
  delete_file("test.aif")
  mus_sound_forget("test.aif")
  $open_raw_sound_hook.reset_hook!
  $bad_header_hook.reset_hook!
end

def test04(formats, comments, fields, devices)
  let(open_sound("/usr//usr/include/" + $home_dir + "/cl/oboe.snd")) do |ind|
    show_input_1
    close_sound(ind)
  end
  #
  # check data_clipped choices moved at the beginning of test04
  # because of disappearing of file descriptors:
  # 
  # mus_file_seek_frame: file descriptors not realloc'd? (tfd: 4, io_fd_size: 12): Mus_error
  # from io.c, mus_file_seek_frame(), io_fds[tfd] == NULL!
  # 
  test094
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
  test104
  test114 if $with_big_file and RUBY_VERSION >= "1.8.0"
  test124
  test134
  test144
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
  if proc?(func_body)
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
  snd_display("%s (0) no change!\n# %s\n# %s", func_sym, v0, v1) if vequal(v0, v1)
  body.call(ind1, 0)
  v2 = samples2vct(12000, 10, ind1, 0)
  snd_display("%s (1)\n# %s\n# %s", func_sym, v1, v2) unless vequal(v1, v2)
  body.call(ind1, lambda do |snd, chn| 0 end)
  v2 = samples2vct(12000, 10, ind1, 0)
  snd_display("%s (2)\n# %s\n# %s", func_sym, v1, v2) unless vequal(v1, v2)
  revert_sound(ind1)
end

def test_orig(func0, func1, func_name, ind1)
  v0 = samples2vct(12000, 10, ind1, 0)
  func0.call(ind1)
  v1 = samples2vct(12000, 10, ind1, 0)
  if vfequal(v0, v1)
    snd_display("%s (orig: 0) no change!\n# %s\n# %s", func_name, v0, v1)
  end
  func1.call(ind1)
  v2 = samples2vct(12000, 10, ind1, 0)
  # INFO vfequal --> vffequal
  unless vffequal(v0, v2)
    snd_display("%s (orig: 1)\n# %s\n# %s", func_name, v0, v2)
  end
  revert_sound(ind1)
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
  undo_edit
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
  undo_edit(2)
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
  undo_edit
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
  undo_edit
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
    if (res = display_edits(ind, 0, idx)) != str
      snd_display("new %d: %s %s?", test_idx, str, res)
    end
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
  undo_edit(2)
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
    if (res = display_edits(ind, 0, ed)) != str
      snd_display("%s %d: %s %s?", test_name, idx, str, res)
    end
  end
  map_channel($init_channel, 0, 10)
  scale_channel(0.5)
  test_output.call(2, "
 (scale 0 10) ; scale_channel(0.500, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 0.500]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  scale_channel(0.5, 0, 3)
  test_output.call(2, "
 (scale 0 3) ; scale_channel(0.500, 0, 3 [2:3]:
   (at 0, cp->sounds[1][0:2, 0.500]) [buf: 10] 
   (at 3, cp->sounds[1][3:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  scale_channel(0.5, 5, 5)
  test_output.call(2, "
 (scale 5 5) ; scale_channel(0.500, 5, 5 [2:3]:
   (at 0, cp->sounds[1][0:4, 1.000]) [buf: 10] 
   (at 5, cp->sounds[1][5:9, 0.500]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  scale_channel(0.5, 2, 4)
  test_output.call(2, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [2:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:5, 0.500]) [buf: 10] 
   (at 6, cp->sounds[1][6:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
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
  undo_edit
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
  undo_edit(4)
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
  undo_edit(2)
  idx = -1
  test_name = "ramp"
  ramp_channel(0.0, 1.0)
  test_output.call(2, "
 (ramp 0 10) ; ramp_channel(0.000, 1.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [1]0.000 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  scale_channel(0.5)
  test_output.call(3, "
 (scale 0 10) ; scale_channel(0.500, 0, false [3:2]:
   (at 0, cp->sounds[1][0:9, 0.500, [1]0.000 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  scale_channel(0.5, 0, 5)
  test_output.call(3, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [3:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]0.000 -> 0.444]) [buf: 10] 
   (at 5, cp->sounds[1][5:9, 1.000, [1]0.556 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  scale_channel(0.5, 2, 4)
  test_output.call(3, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.111]) [buf: 10] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.222 -> 0.556]) [buf: 10] 
   (at 6, cp->sounds[1][6:9, 1.000, [1]0.667 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  scale_channel(0.5, 5, 5)
  test_output.call(3, "
 (scale 5 5) ; scale_channel(0.500, 5, 5 [3:3]:
   (at 0, cp->sounds[1][0:4, 1.000, [1]0.000 -> 0.444]) [buf: 10] 
   (at 5, cp->sounds[1][5:9, 0.500, [1]0.556 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit(2)
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
  undo_edit
  scale_channel(0.5, 2, 6)
  test_output.call(3, "
 (scale 2 6) ; scale_channel(0.500, 2, 6 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:7, 0.500, [1]0.200 -> 0.600]) [buf: 10] 
   (at 8, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
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
  undo_edit
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
  undo_edit
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
  undo_edit
  delete_sample(4)
  test_output.call(3, "
 (delete 4 1) ; delete_samples(4, 1 [3:5]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][5:7, 1.000, [1]0.440 -> 0.600]) [buf: 10] 
   (at 7, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 9, end_mark)
")
  undo_edit
  delete_samples(4, 2)
  test_output.call(3, "
 (delete 4 2) ; delete_samples(4, 2 [3:5]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][6:7, 1.000, [1]0.520 -> 0.600]) [buf: 10] 
   (at 6, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 8, end_mark)
")
  undo_edit
  delete_samples(4, 3)
  test_output.call(3, "
 (delete 4 3) ; delete_samples(4, 3 [3:5]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][7:7, 1.000, [1]0.600 -> 0.600]) [buf: 10] 
   (at 5, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 7, end_mark)
")
  undo_edit
  delete_samples(4, 4)
  test_output.call(3, "
 (delete 4 4) ; delete_samples(4, 4 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][8:9, 1.000]) [buf: 10] 
   (at 6, end_mark)
")
  undo_edit
  delete_samples(4, 5)
  test_output.call(3, "
 (delete 4 5) ; delete_samples(4, 5 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000]) [buf: 10] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.280]) [buf: 10] 
   (at 4, cp->sounds[1][9:9, 1.000]) [buf: 10] 
   (at 5, end_mark)
")
  undo_edit
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
  undo_edit
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
    if (res = display_edits(ind, 0, ed)) != str
      snd_display("%s %d: %s %s?", test_name, idx, str, res)
    end
  end
  map_channel($init_channel, 0, 10)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(2, "
 (ramp 0 10) ; xramp_channel(0.000, 1.000, 32.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  xramp_channel(0.0, 1.0, 0.325)
  test_output.call(2, "
 (ramp 0 10) ; xramp_channel(0.000, 1.000, 0.325, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]0.000 -> -1.124, off: 1.481, scl: -1.481]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  xramp_channel(0.0, 1.0, 0.0)
  test_output.call(2, format("
 (scale 0 10) ; scale_channel(0.000, 0, false [2:2]:
   (at 0, cp->sounds[0][0:9, 0.000]) [file: %s/test.snd[0]]
   (at 10, end_mark)
", Dir.pwd))
  undo_edit
  xramp_channel(0.0, 1.0, 1.0)
  test_output.call(2, "
 (ramp 0 10) ; ramp_channel(0.000, 1.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [1]0.000 -> 1.000]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  xramp_channel(0.5, 1.5, 32.0)
  test_output.call(2, "
 (ramp 0 10) ; xramp_channel(0.500, 1.500, 32.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]0.000 -> 3.466, off: 0.468, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  if fneq(maxamp, 1.5) or fneq(sample(0), 0.5)
    snd_display("xramp 5 vals: %f %f", maxamp, sample(0))
  end
  undo_edit
  xramp_channel(-0.5, 1.5, 32.0)
  test_output.call(2, "
 (ramp 0 10) ; xramp_channel(-0.500, 1.500, 32.000, 0, false [2:2]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]0.000 -> 3.466, off: -0.565, scl: 0.065]) [buf: 10] 
   (at 10, end_mark)
")
  if fneq(maxamp, 1.5) or fneq(sample(0), -0.5)
    snd_display("xramp 6 vals: %f %f", maxamp, sample(0))
  end
  undo_edit
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
    snd_display("trouble in xramp 7: %s", res)
  end
  undo_edit
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
    snd_display("trouble in xramp 8: %s", res)
  end
  undo_edit
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
    snd_display("trouble in xramp 9: %s", res)
  end
  undo_edit
  delete_sample(0)
  delete_sample(0)
  test_output.call(4, "
 (delete 0 1) ; delete_samples(0, 1 [4:2]:
   (at 0, cp->sounds[1][2:9, 1.000, [4]0.770 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 8, end_mark)
")
  undo_edit(2)
  delete_sample(4)
  test_output.call(3, "
 (delete 4 1) ; delete_samples(4, 1 [3:3]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[1][5:9, 1.000, [4]1.925 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 9, end_mark)
")
  undo_edit
  delete_samples(4, 2)
  test_output.call(3, "
 (delete 4 2) ; delete_samples(4, 2 [3:3]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[1][6:9, 1.000, [4]2.310 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 8, end_mark)
")
  undo_edit
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
    snd_display("trouble in xramp 13: %s", res)
  end
  undo_edit
  scale_channel(0.5, 0, 2)
  test_output.call(3, "
 (scale 0 2) ; scale_channel(0.500, 0, 2 [3:3]:
   (at 0, cp->sounds[1][0:1, 0.500, [4]0.000 -> 0.385, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 2, cp->sounds[1][2:9, 1.000, [4]0.770 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  pad_channel(4, 2)
  test_output.call(3, "
 (silence 4 2) ; pad-channel [3:4]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[-1][0:1, 0.000])
   (at 6, cp->sounds[1][4:9, 1.000, [4]1.540 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 12, end_mark)
")
  undo_edit
  set_sample(4, 1.0)
  test_output.call(3, "
 (set 4 1) ; set_sample(4, 1.0000 [3:4]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:9, 1.000, [4]1.925 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
  set_samples(4, 2, make_vct(2))
  test_output.call(3, "
 (set 4 2) ; set-samples [3:4]:
   (at 0, cp->sounds[1][0:3, 1.000, [4]0.000 -> 1.155, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 4, cp->sounds[2][0:1, 1.000]) [buf: 2] 
   (at 6, cp->sounds[1][6:9, 1.000, [4]2.310 -> 3.466, off: -0.032, scl: 0.032]) [buf: 10] 
   (at 10, end_mark)
")
  undo_edit
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
  undo_edit
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
  undo_edit
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
  undo_edit
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
    if (res = display_edits(ind, 0, ed)) != str
      snd_display("%s %d: %s %s?", test_name, idx, str, res)
    end
  end
  map_chan($init_channel, 0, 10)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.010, 0.040, 0.090, 0.160, 0.250, 0.360, 0.490, 0.640, 0.810, 1.000))
    snd_display("ramp2 (0): %s", res)
  end
  scale_channel(0.5)
  test_output.call(4, "
 (scale 0 11) ; scale_channel(0.500, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]0.000 -> 1.000, [2]0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]0.000 -> 0.400, [2]0.000 -> 0.400]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [2]0.500 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.005, 0.020, 0.045, 0.080, 0.250, 0.360, 0.490, 0.640, 0.810, 1.000))
    snd_display("ramp2 (2): %s", res)
  end
  undo_edit
  scale_channel(0.5, 2, 4)
  test_output.call(4, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [2]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [2]0.200 -> 0.500]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [2]0.600 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit(2)
  ramp_channel(0.75, 0.25)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.750, 0.250, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.750 -> 0.250]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  ramp_channel(0.2, 0.6, 2, 6)
  test_output.call(3, "
 (ramp 2 6) ; ramp_channel(0.200, 0.600, 2, 6 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.700, [2]0.200 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [1]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.400, [2]0.200 -> 0.360]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.440 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  set_sample(4, 0.5)
  test_output.call(4, "
 (set 4 1) ; set_sample(4, 0.5000 [4:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.300, [2]0.200 -> 0.280]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.440 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit(3)
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
   (at 90, cp->sounds[1][90:99, 0.500, [1]0.909 -> 1.000, [2]0.091 -> 0.000]) [buf: 100] 
   (at 100, end_mark)
")
  undo_edit(12)
  ramp_channel(0.0, 1.0, 10, 20)
  ramp_channel(0.0, 1.0, 50, 10)
  ramp_channel(0.0, 1.0, 25, 10)
  test_output.call(4, "
 (ramp 25 10) ; ramp_channel(0.000, 1.000, 25, 10 [4:8]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 100] 
   (at 10, cp->sounds[1][10:24, 1.000, [1]0.000 -> 0.737]) [buf: 100] 
   (at 25, cp->sounds[1][25:29, 1.000, [1]0.789 -> 1.000, [2]0.000 -> 0.444]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [1]0.556 -> 1.000]) [buf: 100] 
   (at 35, cp->sounds[1][35:49, 1.000]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [1]0.000 -> 1.000]) [buf: 100] 
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
    if (res = display_edits(ind, 0, ed)) != str
      snd_display("%s %d: %s %s?", test_name, idx, str, res)
    end
  end
  map_chan($init_channel, 0, 10)
  ramp_channel(0.0, 1.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case1 = channel2vct,
                vct(0.000, 0.001, 0.006, 0.018, 0.039, 0.075, 0.135, 0.233, 0.387, 0.628, 1.000))
    snd_display("ramp-xramp (1): %s", case1)
  end
  scale_channel(0.5)
  test_output.call(4, "
 (scale 0 11) ; scale_channel(0.500, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]0.000 -> 0.400, [4]0.000 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [4]1.733 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case2 = channel2vct,
                vct(0.000, 0.001, 0.003, 0.009, 0.019, 0.075, 0.135, 0.233, 0.387, 0.628, 1.000))
    snd_display("ramp-xramp (2): %s", case2)
  end
  undo_edit
  scale_channel(0.5, 2, 4)
  test_output.call(4, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [4]0.693 -> 1.733, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [4]2.079 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit(2)
  xramp_channel(0.75, 0.25, 32.0)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.750, 0.250, 32.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [4]3.466 -> 0.000, off: 0.234, scl: 0.016]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  xramp_channel(0.2, 0.6, 3.0, 2, 6)
  test_output.call(3, "
 (ramp 2 6) ; xramp_channel(0.200, 0.600, 3.000, 2, 6 [3:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.700, [4]0.000 -> 1.099, off: 0.000, scl: 0.200]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [1]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.400, [4]0.000 -> 0.439, off: 0.000, scl: 0.200]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [4]0.659 -> 1.099, off: 0.000, scl: 0.200]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  set_sample(4, 0.5)
  test_output.call(4, "
 (set 4 1) ; set_sample(4, 0.5000 [4:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.300, [4]0.000 -> 0.220, off: 0.000, scl: 0.200]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [4]0.659 -> 1.099, off: 0.000, scl: 0.200]) [buf: 11] 
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
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case1, (res = channel2vct))
    snd_display("xramp-ramp (1): %s", res)
  end
  scale_channel(0.5)
  test_output.call(4, "
 (scale 0 11) ; scale_channel(0.500, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]0.000 -> 0.400, [4]0.000 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [4]1.733 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case2, (res = channel2vct))
    snd_display("xramp-ramp (2): %s", res)
  end
  undo_edit
  scale_channel(0.5, 2, 4)
  test_output.call(4, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [4]0.693 -> 1.733, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [4]2.079 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit(2)
  ramp_channel(0.75, 0.25)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.750, 0.250, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.750 -> 0.250, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
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
  undo_edit
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
    if (res = display_edits(ind, 0, ed)) != str
      snd_display("%s %d: %s %s?", test_name, idx, str, res)
    end
  end
  map_chan($init_channel, 0, 10)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case1 = channel2vct,
                vct(0.000, 0.000, 0.001, 0.005, 0.015, 0.038, 0.081, 0.163, 0.310, 0.565, 1.000))
    snd_display("ramp2+xramp (1): %s", case1)
  end
  scale_channel(0.5)
  test_output.call(5, "
 (scale 0 11) ; scale_channel(0.500, 0, false [5:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  scale_channel(0.5, 0, 5)
  test_output.call(5, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [5:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]0.000 -> 0.400, [2]0.000 -> 0.400, [4]0.000 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [2]0.500 -> 1.000, [4]1.733 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case2 = channel2vct,
                vct(0.000, 0.000, 0.001, 0.003, 0.008, 0.038, 0.081, 0.163, 0.310, 0.565, 1.000))
    snd_display("ramp2+xramp (2): %s", case2)
  end
  undo_edit
  scale_channel(0.5, 2, 4)
  test_output.call(5, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [5:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [2]0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [2]0.200 -> 0.500, [4]0.693 -> 1.733, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [2]0.600 -> 1.000, [4]2.079 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit(2)
  ramp_channel(0.75, 0.25)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.750, 0.250, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.750 -> 0.250, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  ramp_channel(0.2, 0.6, 2, 6)
  test_output.call(4, "
 (ramp 2 6) ; ramp_channel(0.200, 0.600, 2, 6 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.700, [2]0.200 -> 0.600, [4]0.693 -> 2.426, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [4]2.773 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(5, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [5:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [1]0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.400, [2]0.200 -> 0.360, [4]0.693 -> 1.386, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.440 -> 0.600, [4]1.733 -> 2.426, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [4]2.773 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  set_sample(4, 0.5)
  test_output.call(5, "
 (set 4 1) ; set_sample(4, 0.5000 [5:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [4]0.000 -> 0.347, off: -0.032, scl: 0.032]) [buf: 11] 
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
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(case1, (res = channel2vct))
    snd_display("xramp+ramp2 (1): %s", res)
  end
  revert_sound(ind)
  map_channel($init_channel, 0, 100)
  scale_channel(0.75)
  ramp_channel(0.5, 1.0)
  ptree_channel(lambda do |y| y * (1.0 / 0.75) end)
  scale_channel(2.0)
  ramp_channel(1.0, 0.5)
  ptree_channel(lambda do |y| y * 0.25 end)
  scale_channel(4.0)
  ramp_channel(0.0, 1.0)
  snd_display("rprpr max: %f?", maxamp) if fneq(maxamp, 1.0)
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
    if (res = display_edits(ind, 0, ed)) != str
      snd_display("%s %d: %s %s?", test_name, idx, str, res)
    end
  end
  map_channel($init_channel, 0, 100)
  scale_channel(0.5)
  xramp_channel(1.0, 0.0, 32.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  case3 = channel2vct
  undo_edit(4)
  10.times do |i| scale_channel(0.5, i * 10, 10) end
  xramp_channel(1.0, 0.0, 32.0)
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
    snd_display("multi-ramp2+xramp: %s", res)
  end
  revert_sound
  map_channel($init_channel, 0, 100)
  xramp_channel(1.0, 0.0, 32.0)
  ramp_channel(0.0, 1.0, 10, 20)
  ramp_channel(0.0, 1.0, 50, 10)
  ramp_channel(0.0, 1.0, 25, 10)
  test_output.call(5, "
 (ramp 25 10) ; ramp_channel(0.000, 1.000, 25, 10 [5:8]:
   (at 0, cp->sounds[1][0:9, 1.000, [4]3.466 -> 3.151, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 10, cp->sounds[1][10:24, 1.000, [1]0.000 -> 0.737, [4]3.116 -> 2.626, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 25, cp->sounds[1][25:29, 1.000, [1]0.789 -> 1.000, [2]0.000 -> 0.444, [4]2.591 -> 2.451, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [1]0.556 -> 1.000, [4]2.416 -> 2.275, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 35, cp->sounds[1][35:49, 1.000, [4]2.240 -> 1.750, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [1]0.000 -> 1.000, [4]1.715 -> 1.400, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 60, cp->sounds[1][60:99, 1.000, [4]1.365 -> 0.000, off: -0.032, scl: 0.032]) [buf: 100] 
   (at 100, end_mark)
")
  close_sound(ind)
  ind = new_sound("test.snd")
  idx = -1
  test_name = "multi-ramp-xramp"
  map_channel($init_channel, 0, 100)
  scale_channel(0.5)
  ramp_channel(0.0, 1.0)
  xramp_channel(1.0, 0.0, 32.0)
  case3 = channel2vct
  undo_edit(3)
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
    snd_display("multi-ramp-xramp: %s", res)
  end
  undo_edit(12)
  xramp_channel(0.0, 1.0, 3.0, 10, 20)
  xramp_channel(0.0, 1.0, 3.0, 50, 10)
  xramp_channel(0.0, 1.0, 3.0, 25, 10)
  test_output.call(4, "
 (ramp 25 10) ; xramp_channel(0.000, 1.000, 3.000, 25, 10 [4:8]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 100] 
   (at 10, cp->sounds[1][10:24, 1.000, [4]0.000 -> 0.810, off: -0.500, scl: 0.500]) [buf: 100] 
   (at 25, cp->sounds[1][25:29, 1.000, [3]0.000 -> 0.488, [4]0.867 -> 1.099, off: -0.500, scl: 0.500, off2: -0.500, scl2: 0.500]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [4]0.610 -> 1.099, off: -0.500, scl: 0.500]) [buf: 100] 
   (at 35, cp->sounds[1][35:49, 1.000]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [4]0.000 -> 1.099, off: -0.500, scl: 0.500]) [buf: 100] 
   (at 60, cp->sounds[1][60:99, 1.000]) [buf: 100] 
   (at 100, end_mark)
")
  revert_sound
  test_name = "multi-xramp-ramp"
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
  undo_edit(12)
  ramp_channel(0.0, 1.0, 10, 20)
  ramp_channel(0.0, 1.0, 50, 10)
  ramp_channel(0.0, 1.0, 25, 10)
  test_output.call(4, "
 (ramp 25 10) ; ramp_channel(0.000, 1.000, 25, 10 [4:8]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 100] 
   (at 10, cp->sounds[1][10:24, 1.000, [1]0.000 -> 0.737]) [buf: 100] 
   (at 25, cp->sounds[1][25:29, 1.000, [1]0.789 -> 1.000, [2]0.000 -> 0.444]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [1]0.556 -> 1.000]) [buf: 100] 
   (at 35, cp->sounds[1][35:49, 1.000]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [1]0.000 -> 1.000]) [buf: 100] 
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
    snd_display("xramp2 (1): %s", res)
  end
  scale_channel(0.5)
  test_output.call(4, "
 (scale 0 11) ; scale_channel(0.500, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [3]0.000 -> 0.693, [4]0.000 -> 0.693, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  scale_channel(0.5, 0, 5)
  test_output.call(4, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [4:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [3]0.000 -> 0.277, [4]0.000 -> 0.277, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [3]0.347 -> 0.693, [4]0.347 -> 0.693, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.003, 0.011, 0.027, 0.051, 0.172, 0.266, 0.390, 0.549, 0.750, 1.000))
    snd_display("xramp2 (2): %s", res)
  end
  undo_edit
  scale_channel(0.5, 2, 4)
  test_output.call(4, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [3]0.000 -> 0.069, [4]0.000 -> 0.069, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [3]0.139 -> 0.347, [4]0.139 -> 0.347, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [3]0.416 -> 0.693, [4]0.416 -> 0.693, off: -1.000, scl: 1.000, off2: -1.000, scl2: 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit(2)
  xramp_channel(0.75, 0.25, 0.3)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.750, 0.250, 0.300, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [3]-1.204 -> 0.000, [4]0.000 -> 0.693, off: -1.000, scl: 1.000, off2: 0.964, scl2: -0.714]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
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
  undo_edit
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
  undo_edit(3)
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
   (at 0, cp->sounds[1][0:9, 0.500, [3]-1.204 -> -1.095, [4]0.000 -> 0.100, off: -0.500, scl: 0.500, off2: 1.429, scl2: -1.429]) [buf: 100] 
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
    if (res = display_edits(ind, 0, ed)) != str
      snd_display("%s %d: %s %s?", test_name, idx, str, res)
    end
  end
  map_chan($init_channel, 0, 10)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [3]0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  unless vequal(res = channel2vct,
                vct(0.000, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216, 0.343, 0.512, 0.729, 1.000))
    snd_display("ramp3 (1): %s", res)
  end
  scale_channel(0.5)
  test_output.call(5, "
 (scale 0 11) ; scale_channel(0.500, 0, false [5:2]:
   (at 0, cp->sounds[1][0:10, 0.500, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [3]0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  scale_channel(0.5, 0, 5)
  test_output.call(5, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [5:3]:
   (at 0, cp->sounds[1][0:4, 0.500, [1]0.000 -> 0.400, [2]0.000 -> 0.400, [3]0.000 -> 0.400]) [buf: 11] 
   (at 5, cp->sounds[1][5:10, 1.000, [1]0.500 -> 1.000, [2]0.500 -> 1.000, [3]0.500 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  scale_channel(0.5, 2, 4)
  test_output.call(5, "
 (scale 2 4) ; scale_channel(0.500, 2, 4 [5:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [2]0.000 -> 0.100, [3]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:5, 0.500, [1]0.200 -> 0.500, [2]0.200 -> 0.500, [3]0.200 -> 0.500]) [buf: 11] 
   (at 6, cp->sounds[1][6:10, 1.000, [1]0.600 -> 1.000, [2]0.600 -> 1.000, [3]0.600 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit(2)
  ramp_channel(0.75, 0.25)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.750, 0.250, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [3]0.750 -> 0.250]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  ramp_channel(0.2, 0.6, 2, 6)
  test_output.call(4, "
 (ramp 2 6) ; ramp_channel(0.200, 0.600, 2, 6 [4:4]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [2]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:7, 1.000, [1]0.200 -> 0.700, [2]0.200 -> 0.700, [3]0.200 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [2]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  scale_channel(0.5, 0, 5)
  test_output.call(5, "
 (scale 0 5) ; scale_channel(0.500, 0, 5 [5:5]:
   (at 0, cp->sounds[1][0:1, 0.500, [1]0.000 -> 0.100, [2]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:4, 0.500, [1]0.200 -> 0.400, [2]0.200 -> 0.400, [3]0.200 -> 0.360]) [buf: 11] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.500 -> 0.700, [3]0.440 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [2]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit
  set_sample(4, 0.5)
  test_output.call(5, "
 (set 4 1) ; set_sample(4, 0.5000 [5:6]:
   (at 0, cp->sounds[1][0:1, 1.000, [1]0.000 -> 0.100, [2]0.000 -> 0.100]) [buf: 11] 
   (at 2, cp->sounds[1][2:3, 1.000, [1]0.200 -> 0.300, [2]0.200 -> 0.300, [3]0.200 -> 0.280]) [buf: 11] 
   (at 4, cp->sounds[2][0:0, 1.000]) [buf: 1] 
   (at 5, cp->sounds[1][5:7, 1.000, [1]0.500 -> 0.700, [2]0.500 -> 0.700, [3]0.440 -> 0.600]) [buf: 11] 
   (at 8, cp->sounds[1][8:10, 1.000, [1]0.800 -> 1.000, [2]0.800 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  undo_edit(3)
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
  undo_edit(13)
  ramp_channel(0.0, 1.0, 10, 30)
  ramp_channel(0.0, 1.0, 50, 20)
  ramp_channel(0.0, 1.0, 20, 15)
  ramp_channel(0.0, 1.0, 30, 30)
  test_output.call(5, "
 (ramp 30 30) ; ramp_channel(0.000, 1.000, 30, 30 [5:10]:
   (at 0, cp->sounds[1][0:9, 1.000]) [buf: 100] 
   (at 10, cp->sounds[1][10:19, 1.000, [1]0.000 -> 0.310]) [buf: 100] 
   (at 20, cp->sounds[1][20:29, 1.000, [1]0.345 -> 0.655, [2]0.000 -> 0.643]) [buf: 100] 
   (at 30, cp->sounds[1][30:34, 1.000, [1]0.690 -> 0.828, [2]0.714 -> 1.000, [3]0.000 -> 0.138]) [buf: 100] 
   (at 35, cp->sounds[1][35:39, 1.000, [1]0.862 -> 1.000, [2]0.172 -> 0.310]) [buf: 100] 
   (at 40, cp->sounds[1][40:49, 1.000, [1]0.345 -> 0.655]) [buf: 100] 
   (at 50, cp->sounds[1][50:59, 1.000, [1]0.000 -> 0.474, [2]0.690 -> 1.000]) [buf: 100] 
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
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "xramp+xramp"
  undo_edit(2)
  xramp_channel(0.0, 1.0, 0.32)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(3, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [3]0.000 -> 3.466, [4]0.000 -> -1.139, off: 1.471, scl: -1.471, off2: -0.032, scl2: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "xramp+xramp+xramp"
  undo_edit(2)
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
  undo_edit(3)
  xramp_channel(0.0, 1.0, 0.32)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  test_output.call(4, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [3]0.000 -> 3.466, [4]0.000 -> -1.139, off: 1.471, scl: -1.471, off2: -0.032, scl2: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "xramp+ramp"
  undo_edit(3)
  xramp_channel(0.0, 1.0, 32.0)
  ramp_channel(0.0, 1.0)
  test_output.call(3, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [3:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "ramp+ramp+xramp"
  undo_edit(2)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(4, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [4:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "ramp+ramp+ramp+ramp"
  undo_edit(3)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  test_output.call(5, "
 (ramp 0 11) ; ramp_channel(0.000, 1.000, 0, false [5:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [3]0.000 -> 1.000, [4]0.000 -> 1.000]) [buf: 11] 
   (at 11, end_mark)
")
  idx = -1
  test_name = "ramp+ramp+ramp+xramp"
  undo_edit(4)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  ramp_channel(0.0, 1.0)
  xramp_channel(0.0, 1.0, 32.0)
  test_output.call(5, "
 (ramp 0 11) ; xramp_channel(0.000, 1.000, 32.000, 0, false [5:2]:
   (at 0, cp->sounds[1][0:10, 1.000, [1]0.000 -> 1.000, [2]0.000 -> 1.000, [3]0.000 -> 1.000, [4]0.000 -> 3.466, off: -0.032, scl: 0.032]) [buf: 11] 
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
    (len - 1).downto(0) do |i| dat[i] = rd.call end
    free_sample_reader(rd)
    dat
  end
  set_squelch_update(true, ind)
  # 0 case
  set_to_1.call
  unless vvequal(data, res = channel2vct)
    snd_display("0 case! %s", res)
  end
  unless vvequal(data, res = rev_channel2vct.call)
    snd_display("0 case rev! %s", res)
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
      snd_display("1 case: %s\n%s\n%s", name, data, res)
    end
    unless vvequal(data, res = rev_channel2vct.call)
      snd_display("1 rev case: c%s\n%s\n%s", name, data, res)
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
        snd_display("2 case: %s (%s)\n%s\n%s", name1, name, data, res)
      end
      unless vvequal(data, res = rev_channel2vct.call)
        snd_display("2 rev case: c%s (c%s)\n%s\n%s", name1, name, data, res)
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
                      name2, name1, name, data, res)
        end
        unless vvequal(data, res = rev_channel2vct.call)
          snd_display("2 rev case: c%s (c%s (c%s))\n%s\n%s",
                      name2, name1, name, data, res)
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
                          name3, name2, name1, name, data, res)
            end
            unless vvequal(data, res = rev_channel2vct.call)
              snd_display("2 rev case: c%s (c%s (c%s (c%s)))\n%s\n%s",
                          name3, name2, name1, name, data, res)
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
                            name4, name3, name2, name1, name, data, res)
              end
              unless vvequal(data, res = rev_channel2vct.call)
                snd_display("2 rev case: c%s (c%s (c%s (c%s (c%s))))\n%s\n%s",
                            name4, name3, name2, name1, name, data, res)
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
                              name5, name4, name3, name2, name1, name, data, res)
                end
                unless vvequal(data, res = rev_channel2vct.call)
                  snd_display("2 rev case: c%s (c%s (c%s (c%s (c%s (c%s)))))\n%s\n%s",
                              name5, name4, name3, name2, name1, name, data, res)
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
  undo_edit
  unless (res = cursor) == 1000
    snd_display("delete_sample after cursor undo: %d", res)
  end
  undo_edit(-1)
  unless (res = cursor) == 999
    snd_display("delete_sample before cursor redo: %d", res)
  end
  redo_edit(-1)
  delete_samples(0, 100)
  unless (res = cursor) == 900
    snd_display("delete_samples before cursor: %d", res)
  end
  undo_edit
  delete_samples(1100, 100)
  unless (res = cursor) == 1000
    snd_display("delete_samples after cursor: %d", res)
  end
  undo_edit
  insert_samples(100, 100, make_vct(100))
  unless (res = cursor) == 1100
    snd_display("insert_samples before cursor: %d", res)
  end
  undo_edit
  insert_samples(1100, 100, make_vct(100))
  unless (res = cursor) == 1000
    snd_display("insert_samples after cursor: %d", res)
  end
  undo_edit
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
  snd_display("disk_kspace = %s", k) if (not number?(k)) or k <= 0
  k = disk_kspace("/baddy/hiho")
  # #if (!HAVE_STATFS) && (!HAVE_STATVFS) in snd-file.c
  # disk_kspace returns 1234567 in every case
  if k != 1234567 and k != -1
    snd_display("disk_kspace of bogus file = %s", k)
  end
  if (res = transform_frames).nonzero?
    snd_display("trandform_frames %d", res)
  end
  set_transform_graph?(true)
  unless (res = fft_peak(ind, 0, 1.0))
    snd_display("fft_peak %s?", res)
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
  if number?(transform_frames) and transform_frames.zero?
    snd_display("transform_graph? transform-frames: %d?", trandform_frames)
  end
  update_transform_graph
  peaks("tmp.peaks")
  pks_data = IO.readlines("tmp.peaks")
  if /Snd: fft peaks/ !~ pks_data[0]
    snd_display("peaks 1: %s", pks_data[0])
  end
  if /fft 512 points beginning at sample 0/ !~ pks_data[2]
    snd_display("peaks 2: %s", pks_data[2])
  end
  if /86.132812  1.00000/ !~ pks_data[4] and /0.000000  1.00000/ !~ pks_data[4]
    snd_display("peaks 3: %s", pks_data[4])
  end
  delete_file("tmp.peaks")
  peaks
  if provided?(:xm) and (!dialog_widgets[20] or !RXtIsManaged(dialog_widgets[20]))
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
  snd_display("read_only open_sound: %s?", read_only(ind)) if read_only(ind)
  set_read_only(true, ind)
  snd_display("set_read_only: %s?", read_only(ind)) unless read_only(ind)
  a_ctr = 0
  bind_key(?a, 0, lambda do a_ctr = 3 end)
  key(?a, 0)
  snd_display("bind_key: %s?", a_ctr) if a_ctr != 3
  unbind_key(?a, 0)
  a_ctr = 0
  key(?a, 0)
  5.times do |i|
    psf = eps_file
    if string?(psf)
      delete_file(psf)
      set_graph_style(i)
      graph2ps
      if File.exists?(psf)
        File.unlink(psf)
      else
        snd_display("graph2ps: %s?", psf)
      end
    end
  end
  if (err = snd_catch(:cannot_print, 12345) do graph2ps("/bad/bad.eps") end).first != 12345
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
  snd_display("x_bounds: %s?", bnds) if fneq(bnds[0], 0.0) or fneq(bnds[1], 0.1)
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
    snd_display("oboe: short name: %s?", res)
  end
  if (res = count_matches(lambda do |y| y > 0.125 end)) != 1313
    snd_display("oboe: count_matches %d?", res)
  end
  let(find(lambda do |y| y > 0.13 end)) do |spot|
    if (not array?(spot)) or spot[1] != 8862
      snd_display("find: %s?", spot)
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
    snd_display("edits: %s?", eds) if eds[0].nonzero? or eds[1].nonzero?
    snd_display("edit_position: %s %s?", edit_position, eds) if edit_position != eds[0]
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
    snd_display("regions: %s", regions) if regions.length != ($test04 ? 2 : 1)
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
        snd_display("%s: %s?", func, res)
      end
    end
    samps1 = samples2vct(0, 50827, ind, 0)
    samps2 = region2vct(0, 50828, r0, 0)
    rd = make_sample_reader(0, ind, 0, 1)
    snd_display("%s not sample_reader?", rd) unless sample_reader?(rd)
    if (res = sample_reader_position(rd)).nonzero?
      snd_display("initial sample_reader_position: %d?", res)
    end
    if (res = sample_reader_home(rd)) != [ind, 0]
      snd_display("sample_reader_home: %s %s?", res, [ind, 0])
    end
    snd_display("%s init at end?", rd) if sample_reader_at_end?(rd)
    if (res = snd_catch do region2vct(-1, 1233, r0) end).first != :no_such_sample
      snd_display("region2vct -1: %s", res.inspect)
    end
    if res = snd_catch do region2vct(12345678, 1, r0) end.first
      snd_display("region2vct 12345678: %s", res.inspect)
    end
    if (res = format("%s", rd)) != "#<sample-reader: oboe.snd[0: 0] from 0, at 0>"
      snd_display("sample_reader actually got: %s", res)
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
  if (res = snd_catch do make_sample_reader(0, ind, -1) end).first != :no_such_channel
    snd_display("make_sample_reader bad chan -1: %s?", res.inspect)
  end
  if (res = snd_catch do make_sample_reader(0, ind, 1) end).first != :no_such_channel
    snd_display("make_sample_reader bad chan 1: %s?", res.inspect)
  end
  let(make_sample_reader(0)) do |fd|
    snd_display("sample_reader: mix %s?", fd) if mix_sample_reader?(fd)
    snd_display("sample_reader: region %s?", fd) if region_sample_reader?(fd)
    snd_display("sample_reader: track %s?", fd) if track_sample_reader?(fd)
    snd_display("sample_reader: normal %s?", fd) unless sample_reader?(fd)
    snd_display("sample_reader: position %s?", fd) if sample_reader_position(fd).nonzero?
    free_sample_reader(fd)
    if (res = format("%s", fd))[-16, 16] != "at eof or freed>"
      snd_display("freed sample_reader: %s [%s]?", res, res[-16, 16])
    end
  end
  if (res = snd_catch do
        reg = regions.first
        make_region_sample_reader(0, reg, region_chans(reg) + 1)
      end).first != :no_such_channel
    snd_display("make_region_sample_reader bad chan (2): %s %s", res.inspect, regions)
  end
  revert_sound(ind)
  insert_sample(100, 0.5, ind)
  if (res = snd_catch do insert_sound("oboe.snd", 0, 1) end).first != :no_such_channel
    snd_display("insert_sound bad chan (1): %s", res.inspect)
  end
  if (res = snd_catch do insert_sample(-12, 1.0) end).first != :no_such_sample
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
  snd_display("read_only view_sound: %s?", read_only(nind)) unless read_only(nind)
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
    snd_display("set_samples via list: %s?", samples2vct(10, 3, ind))
  end
  revert_sound(ind)
  save_sound_as("temporary.snd", ind)
  set_samples(100000, 20000, "temporary.snd", ind)
  unless vequal(res1 = samples2vct(110000, 10), res2 = samples2vct(10000, 10))
    snd_display("set_samples to self: %s %s?", res1, res2)
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
  undo_edit(1, ind)
  scale_by(2.0, ind)
  newmaxa = maxamp(ind)
  snd_display("scale_by: %f?", newmaxa) if fneq(newmaxa, 2.0 * maxa)
  revert_sound(ind)
  scale_by(-1, ind)
  mix("oboe.snd")
  snd_display("invert+mix -> %f?", maxamp) if fneq(maxamp(ind, 0), 0.0)
  revert_sound(ind)
  select_all(ind)
  snd_display("regions (2): %s", regions) if regions.length != ($test04 ? 3 : 2)
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
  unless proc?(res = cursor_style(ind, 0))
    snd_display("set_cursor_style to Proc: %s", res)
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
  if (res = snd_catch do insert_region(0, 1000 + regions.max) end).first != :no_such_region
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
      snd_display("save_region %s: %s (%s)?", func, res, var)
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
      snd_display("save_region %s: %s (%s)?", func, res, var)
    end
  end
  delete_file("fmv.snd")
  save_region(id,
              :file, "fmv.snd",
              :header_type, Mus_riff,
              :data_format, Mus_lshort,
              :comment, "this is a comment")
  [[:mus_sound_header_type, Mus_riff],
    [:mus_sound_data_format, Mus_lshort],
    [:mus_sound_comment, "this is a comment"],
    [:mus_sound_frames, region_frames(id)],
  ].each do |func, var|
    if (res = send(func, "fmv.snd")) != var
      snd_display("save_region opt %s: %s (%s)?", func, res, var)
    end
  end
  delete_file("fmv.snd")
  save_region(id,
              :comment, "this is a comment",
              :file, "fmv.snd",
              :data_format, Mus_lshort,
              :header_type, Mus_riff)
  [[:mus_sound_header_type, Mus_riff],
    [:mus_sound_data_format, Mus_lshort],
    [:mus_sound_comment, "this is a comment"],
    [:mus_sound_frames, region_frames(id)],
  ].each do |func, var|
    if (res = send(func, "fmv.snd")) != var
      snd_display("save_region opt1 %s: %s (%s)?", func, res, var)
    end
  end
  delete_file("fmv.snd")
  save_region(id, "fmv.snd", :data_format, Mus_bshort)
  [[:mus_sound_header_type, Mus_next],
    [:mus_sound_data_format, Mus_bshort],
    [:mus_sound_frames, region_frames(id)],
  ].each do |func, var|
    if (res = send(func, "fmv.snd")) != var
      snd_display("save_region opt2 %s: %s (%s)?", func, res, var)
    end
  end
  delete_file("fmv.snd")
  delete_file("aaa.eps")
  close_sound(ind)
end

def test115
  if (res = snd_catch do new_sound("hi.snd", 0, 1, 100, 0) end).first != :out_of_range
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
    snd_display("smooth_selection: %s?", v0)
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
    snd_display("src_selection: %s?", v0)
  end
  revert_sound(ind)
  vct_fill!(v0, 0.0)
  vct_set!(v0, 10, 0.5)
  vct2channel(v0, 0)
  select_all
  filter_selection([0, 0, 0.1, 1, 1, 0], 40)
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(29), 0.1945) or fneq(sample(39), -0.0137) or fneq(sample(24), -0.01986)
    snd_display("filter_selection: %s?", v0)
  end
  revert_sound(ind)
  vct_fill!(v0, 1.0)
  vct2channel(v0)
  select_all
  filter_selection(make_one_zero(:a0, 0.5, :a1, 0.0))
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(29), 0.5) or fneq(sample(39), 0.5) or fneq(sample(24), 0.5)
    snd_display("filter_selection one_zero: %s?", v0)
  end
  revert_sound(ind)
  vct_fill!(v0, 1.0)
  vct2channel(v0)
  delete_file("fmv5.snd")
  select_all
  env_selection([0, 0, 1, 1, 2, 0], 1.0)
  v0 = samples2vct(0, 128, ind, 0, v0)
  if fneq(sample(64), 1.0) or fneq(sample(20), 0.3125) or fneq(sample(119), 0.127)
    snd_display("env_selection: %s?", v0)
  end
  save_selection("fmv5.snd", Mus_next, Mus_bint, 22050, "")
  revert_sound(ind)
  # 
  if (res = snd_catch do file2array("/baddy/hiho", 0, 0, 128, v0) end).first != :no_such_file
    snd_display("file2array w/o file: %s", res.inspect)
  end
  if (res = snd_catch do file2array("fmv5.snd", 123, 0, 128, v0) end).first != :no_such_channel
    snd_display("file2array w/o channel: %s", res.inspect)
  end
  file2array("fmv5.snd", 0, 0, 128, v0)
  if fneq(v0[64], 1.0) or fneq(v0[20], 0.3125) or fneq(v0[119], 0.127)
    snd_display("save_selection: %f %f %f %s?", v0[64], v0[20], v0[119], v0)
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
    snd_display("reverse_selection: %s?", v0)
  end
  file2array("fmv4.snd", 0, 0, 128, v0)
  if fneq(sample(27), 0.5) or fneq(sample(125), -0.5)
    snd_display("save reverse_selection: %s?", v0)
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
                 :header_type, Mus_riff,
                 :data_format, Mus_lfloat,
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
  save_selection(:file, "fmv4.snd", :data_format, Mus_bfloat, :channel, 0)
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
    snd_display("convolve_selection_with: %f %f %s?", v0[66], sample(66), v0)
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
    snd_display("amp_control_bounds: %s?", res)
  end
  set_amp_control(2.0, obind)
  if (res = snd_catch do apply_controls(obind) end).first == :no_such_sound
    snd_display("apply_controls: can\'t find oboe.snd? %s", res.inspect)
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
  undo_edit(3, obind)
  set_reverb_control?(true, obind)
  set_reverb_control_scale_bounds([0.0, 1.0], obind)
  if (res = reverb_control_scale_bounds(obind)) != [0.0, 1.0]
    snd_display("reverb_control_scale_bounds: %s?", res)
  end
  set_reverb_control_length_bounds([0.0, 2.0], obind)
  if (res = reverb_control_length_bounds(obind)) != [0.0, 2.0]
    snd_display("reverb_control_length_bounds: %s?", res)
  end
  set_reverb_control_scale(0.2, obind)
  apply_controls(obind)
  revamp = maxamp(obind)
  revdur = frames(obind)
  snd_display("apply reverb scale: %f?", revamp) if ffneq(revamp, 0.214)
  unless revdur - ((reverb_control_decay * 22050).round + 50828) < 256
    snd_display("apply reverb length: %d?", revdur)
  end
  undo_edit(1, obind)
  set_expand_control?(true, obind)
  set_expand_control_bounds([1.0, 3.0], obind)
  if (res = expand_control_bounds(obind)) != [1.0, 3.0]
    snd_display("expand_control_bounds: %s?", res)
  end
  set_expand_control(1.5, obind)
  apply_controls(obind)
  expamp = maxamp(obind)
  expdur = frames(obind)
  snd_display("apply expand_control scale: %f?", expamp) if (expamp - 0.152).abs > 0.01
  snd_display("apply expand_control length: %d?", expdur) unless expdur > 1.25 * 50828
  set_expand_control_bounds([0.001, 20.0], obind)
  undo_edit(1, obind)
  set_filter_control?(true, obind)
  set_filter_control_order(40, obind)
  set_filter_control_envelope([0, 0.0, 1, 0.5, 1, 0.0], obind)
  apply_controls(obind)
  fltamp = maxamp(obind)
  fltdur = frames(obind)
  snd_display("apply filter scale: %f?", fltamp) if (fltamp - 0.01).abs > 0.03 # orig 0.005
  snd_display("apply filter length: %d?", fltdur) if fltdur - (40 + 50828) > 256
  undo_edit(1, obind)
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
    [1000, 0, 1000, 1000, 6.09052181243896, 0.0, 0.0, 0],
    [1001, 0, 1001, 1001, 0.999969720840454, 0.0, 0.0, 0],
    [1002, 0, 1002, 1999, 0.499984979629517, 0.0, 0.0, 0],
    [2000, 0, 2000, 2000, 7.54652404785156, 0.0, 0.0, 0],
    [2001, 0, 2001, 2001, 3.7732629776001, 0.0, 0.0, 0],
    [2002, 0, 2002, 2002, 0.999969720840454, 0.0, 0.0, 0],
    [2003, 0, 2003, 50827, 1.0, 0.0, 0.0, 0],
    [50828, -2, 0, 0, 0.0, 0.0, 0.0, 0]]
  if tree.length != true_tree.length
    snd_display("edit trees are not same length: %d %d?", tree.length, true_tree.length)
  else
    tree.zip(true_tree) do |branch, true_branch|
      if branch[0] != true_branch[0] or
          branch[1] != true_branch[1] or
          branch[2] != true_branch[2] or
          branch[3] != true_branch[3] or
          fneq_err(branch[4], true_branch[4], 0.02)
        snd_display("edit trees disagree:\n# %s\n# %s", branch, true_branch)
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
    [1052, 0, 1000, 1000, 6.09052181243896, 0.0, 0.0, 0],
    [1053, -1, 0, 7, 0.0, 0.0, 0.0, 0],
    [1061, 0, 1001, 1001, 0.999969720840454, 0.0, 0.0, 0],
    [1062, 0, 1002, 1946, 0.499984979629517, 0.0, 0.0, 0],
    [2007, -1, 0, 0, 0.0, 0.0, 0.0, 0],
    [2008, 0, 1947, 1999, 0.499984979629517, 0.0, 0.0, 0],
    [2061, 0, 2000, 2000, 7.54652404785156, 0.0, 0.0, 0],
    [2062, 0, 2001, 2001, 3.7732629776001, 0.0, 0.0, 0],
    [2063, 0, 2002, 2002, 0.999969720840454, 0.0, 0.0, 0],
    [2064, 0, 2003, 50827, 1.0, 0.0, 0.0, 0],
    [50889, -2, 0, 0, 0.0, 0.0, 0.0, 0]]
  if tree.length != true_tree.length
    snd_display("silenced edit trees are not same length: %d %d?", tree.length, true_tree.length)
  else
    tree.zip(true_tree) do |branch, true_branch|
      if branch[0] != true_branch[0] or
          branch[1] != true_branch[1] or
          branch[2] != true_branch[2] or
          branch[3] != true_branch[3] or
          fneq_err(branch[4], true_branch[4], 0.02)
        snd_display("silenced edit trees disagree:\n# %s\n# %s",
                    branch, true_branch)
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
  snd_display("make_region but no selection? %s", selection?) unless selection?
  delete_selection
  snd_display("delete_selection: %d?", frames(obind)) if frames(obind) != frs - 1000
  val = sample(0, obind, 0)
  undo_edit
  snd_display("delete_selection val: %f %f?", val, sample(1000)) if fneq(sample(1000), val)
  insert_selection
  if (res = snd_catch do insert_selection(0, obind, 123) end).first != :no_such_channel
    snd_display("insert_selection bad chan: %s?", res.inspect)
  end
  if (res = snd_catch do mix_selection(0, obind, 123) end).first != :no_such_channel
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
  snd_display("selected_channel false: %s?", selected_channel(ind)) if selected_channel(ind)
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
    snd_display("scale_sound_by:\n# %s\n# %s", res1, res2)
  end
  scale_sound_to(0.5)
  if fneq(res = maxamp(ind1, 0), 0.5)
    snd_display("scale_sound_to 0.5: %f?", res)
  end
  if (res1 = edit_fragment(2, ind1, 0)) !=
      (res2 = ["scale_channel(1.698, 0, false", "scale", 0, 50828])
    snd_display("scale_sound_to:\n# %s\n# %s", res1, res2)
  end
  scale_sound_by(0.0, 0, 1000, ind1, 0)
  if fneq(res = maxamp(ind1, 0), 0.5)
    snd_display("scale_sound_by 0.0: %f?", res)
  end
  if (res1 = edit_fragment(3, ind1, 0)) !=
      (res2 = ["scale_channel(0.000, 0, 1000", "scale", 0, 1000])
    snd_display("scale_sound_by 0.0:\n# %s\n# %s", res1, res2)
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
    snd_display("scale_sound_by 2.0 [12000:10]:\n# %s\n# %s", res1, res2)
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
    snd_display("2 scale_sound_by:\n# %s\n# %s", res1, res2)
  end
  scale_sound_to(0.5)
  if fneq(res = [maxamp(ind2, 0), maxamp(ind2, 1)].max, 0.5)
    snd_display("2 scale_sound_to 0.5: %f %s?", res, maxamp(ind2))
  end
  scale_sound_by(0.0, 0, 1000, ind2, 1)
  if fneq(res = maxamp(ind2, 0), 0.5)
    snd_display("2 scale_sound_by 0.0: %f?", res)
  end
  if (res1 = edit_fragment(3, ind2, 1)) !=
      (res2 = ["scale_channel(0.000, 0, 1000", "scale", 0, 1000])
    snd_display("2:1 scale_sound_by 0.0:\n# %s\n# %s", res1, res2)
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
    snd_display("transform2vct (empty): %s?", res)
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
    snd_display("scale_to with vector: %s?", mx)
  end
  set_filter_control_envelope([0, 0, 1, 1], ind)
  if [0.0, 0.0, 1.0, 1.0] != filter_control_envelope(ind)
    snd_display("set_filter_control_envelope: %s?", filter_control_envelope(ind))
  end
  set_filter_control_order(20, ind)
  unless vequal(res = filter_control_coeffs(ind),
                vct(-0.007, 0.010, -0.025, 0.029, -0.050, 0.055, -0.096, 0.109, -0.268, 0.241,
                    0.241, -0.268, 0.109, -0.096, 0.055, -0.050, 0.029, -0.025, 0.010, -0.007))
    snd_display("highpass coeffs: %s?", res)
  end
  set_filter_control_envelope(filter_control_envelope(ind), ind)
  if [0.0, 0.0, 1.0, 1.0] != filter_control_envelope(ind)
    snd_display("set_filter_control_envelope to self: %s?", filter_control_envelope(ind))
  end
  set_filter_control_envelope([0, 1, 1, 0], ind)
  unless vequal(res = filter_control_coeffs(ind),
                vct(0.003, 0.002, 0.004, 0.002, 0.007, 0.003, 0.014, 0.012, 0.059, 0.394,
                    0.394, 0.059, 0.012, 0.014, 0.003, 0.007, 0.002, 0.004, 0.002, 0.003))
    snd_display("lowpass coeffs: %s?", res)
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
  snd_display("x0->position: %s?", res) if (res[0] - res[1]).abs > 1
  res = our_x2position(obind, x1)
  snd_display("x1->position: %s?", res) if (res[0] - res[1]).abs > 1
  res = our_x2position(obind, 0.5 * (x0 + x1))
  snd_display("xmid->position: %s?", res) if (res[0] - res[1]).abs > 1
  unless $full_test
    if ((res = x2position(xpos)) - cp_x.call(xpos)).abs > 1
      snd_display("cp_x 0.5: %s %s?", res, cp_x.call(xpos))
    end
    if ((res = y2position(ypos)) - cp_y.call(ypos)).abs > 1
      snd_display("cp_y 0.75: %s %s?", res, cp_y.call(ypos))
    end
    10.times do |i|
      xxpos = x0 + random(x1 - x0)
      yypos = y0 + random(y1 - y0)
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
  snd_display("x0a->position: %s?", res) if (res[0] - res[1]).abs > 1
  res = our_x2position(obind, x1)
  snd_display("x1a->position: %s?", res) if (res[0] - res[1]).abs > 1
  res = our_x2position(obind, 0.5 * (x0 + x1))
  snd_display("xmida->position: %s?", res) if (res[0] - res[1]).abs > 1
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
  if (res = snd_catch do apply_controls(obind, 1) end).first == :no_such_sound
    snd_display("apply_controls can\'t find 4.aiff: %s?", res.inspect)
  end
  newamps = maxamp(obind, true)
  if fneq(amps[0], newamps[0]) or
      fneq(amps[1], newamps[1]) or
      (0.1 * amps[2] - newamps[2]).abs > 0.05 or
      fneq(amps[3], newamps[3])
    snd_display("apply amps:\n# %s\n# %s", amps, newamps)
  end
  undo_edit(1, obind, 2)
  set_amp_control(0.1, obind)
  make_region(0, frames(obind), obind, 1)
  snd_catch do apply_controls(obind, 2) end
  newamps = maxamp(obind, true)
  if fneq(amps[0], newamps[0]) or
      (0.1 * amps[1] - newamps[1]).abs > 0.05
      fneq(amps[2], newamps[2]) or
      fneq(amps[3], newamps[3])
    snd_display("apply selection amp:\n# %s\n# %s", amps, newamps)
  end
  xtest155(obind) unless provided? :snd_nogui
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
  if (res = snd_catch do src_sound([0, 0, 1, 1]) end).first != :out_of_range
    snd_display("src_sound env at 0: %s", res.inspect)
  end
  if (res = snd_catch do src_sound([0, 1, 1, -1]) end).first != :out_of_range
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
  snd_display("convolve_with (orig: 0)\n# %s\n# %s", v1, v2) unless vfequal(v1, v2)
  file2array("fmv5.snd", 0, 12000, 10, v2)
  snd_display("convolve_files (orig: 0)\n# %s\n# %s", v1, v2) unless vfequal(v1, v2)
  delete_files("fmv3.snd", "fmv5.snd")
  convolve_files("2.snd", "oboe.snd", 0.5, "fmv5.snd")
  if fneq((res = mus_sound_maxamp("fmv5.snd"))[1], 0.5)
    snd_display("convolve_files stereo: %s", res)
  end
  delete_file("fmv5.snd")
  scale_to(0.25, ind1)
  # INFO
  # XEN_EMPTY_LIST is Qnil
  # but g_set_y_bounds requires a list not nil, so the following doesn't work
  # 
  # set_y_bounds([], ind1)
  # if (res = y_bounds(ind1)) != [-0.25, 0.25]
  #   snd_display("y_bounds []: %s", res)
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
  snd_display("convolve_with (orig: 2)\n# %s\n# %s", v1, v2) unless vfequal(v1, v2)
  file2array("fmv5.snd", 0, 12005, 10, v2)
  snd_display("convolve_files (orig: 2)\n# %s\n# %s", v1, v2) unless vfequal(v1, v2)
  delete_files("fmv3.snd", "fmv4.snd", "fmv5.snd")
  revert_sound(ind1)
  #
  old_val = selection_creates_region
  old_regions = regions
  set_selection_creates_region(false)
  select_all(ind1)
  set_selection_creates_region(old_val)
  if old_regions != regions
    snd_display("selection_creates_region: %s -> %s", old_regions, regions)
  end
  convolve_selection_with("pistol.snd", maxamp)
  data = samples2vct(12000, 10, ind1, 0)
  convolve_with("pistol.snd", maxamp(ind1, 0, 0), ind1, 0, 0)
  new_data = samples2vct(12000, 10, ind1, 0)
  unless vfequal(data, new_data)
    snd_display("convolve_selection_with:\n# %s\n# %s", data, new_data)
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
  snd_display("make_region argless: %s?", id) unless region?(id)
  if (res1 = region_frames(id, 0)) != (res2 = selection_frames)
    snd_display("region/selection_frames: %s %s (%s)?", res1, res2, region_frames(id))
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
    snd_display("swap_channels with cp def: %s %s?", amps, newamps)
  end
  swap_channels(ind, 1)
  newamps = maxamp(ind, true)
  if fneq(amps[0], newamps[0]) or fneq(amps[1], newamps[1])
    snd_display("swap_channels with cp def 0: %s %s?", amps, newamps)
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
    snd_display("map_sound_chans: %s %s?", ups2.map do |n| 2.0 * n end, ups4)
  end
  set_sync(true, ind1)
  set_sync(true, ind2)
  map_chans do |n| n * 0.5 end
  ups3 = maxamp(ind1, 0)
  ups4 = maxamp(ind2, true)
  snd_display("map_chans: %s %s?", ups3, ups1) if fneq(ups3, ups1)
  if fneq(ups4[0], ups2[0]) or fneq(ups4[1], ups2[1])
    snd_display("map_chans: %s %s?", ups2, ups4)
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
    snd_display("map_across_all_chans 2: %s %s?", ups2, ups4)
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
  undo_edit(1, ind1, 0)
  play_and_wait(0, ind1, 0, false, false, 1)
  #
  delete_samples(0, 10000, ind1, 0)
  save_sound_as("fmv.snd", ind1, :edit_position, 0)
  save_sound_as("fmv1.snd", ind1, :edit_position, lambda do |snd, chn| 1 end)
  if (res = snd_catch do
        save_sound_as("fmv2.snd", ind1, :channel, 1234)
      end).first != :no_such_channel
    snd_display("save_sound_as bad chan: %s", res)
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
  undo_edit
  filter_sound([0, 0, 0.19, 0, 0.2, 1, 0.21, 0, 1, 0], 1024)
  snd_display("filter_sound maxamp 2: %s?", maxamp) if maxamp < 0.9
  undo_edit
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
  unless proc?(res = previous_files_sort_procedure)
    snd_display("previous_files_sort_procedure: %s", res)
  end
  set_previous_files_sort(5)
  close_sound(ind1)
  #
  if (res = snd_catch do
        set_previous_files_sort_procedure(lambda do |a, b, c| false end)
      end).first != :bad_arity
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
require \"sndlib.so\" unless provided? :sndlib
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
    snd_display("update_sound edits: %s?", edits(nind))
  end
  if (res = marks(nind, 0).map do |x| mark_sample(x) end) != [101, 202]
    snd_display("update_sound marks: %s?", res)
  end
  snd_display("update_sound sync: %s?", sync(nind)) if sync(nind) != 123
  nsamps = samples2vct(5000, 10)
  unless vequal(samps, vct_scale!(nsamps, 0.5))
    snd_display("update_sound amps: %s %s?", samps, nsamps)
  end
  if (not integer?(s_in)) or ind != s_in
    snd_display("update_hook init: %s %s?", ind, s_in)
  end
  if (not integer?(s_out)) or nind != s_in
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
      end).first != :bad_arity
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
                      name, e_bin, e_size, mn, mx, [mxdiff, mndiff].max)
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
    undo_edit
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
  undo_edit
  env_channel([0, 0, 1, 1, 2, 0])
  peak_env_equal?("env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.002)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.5, :end, frames - 1))
  peak_env_equal?("scaled env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.002)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0], 0.5, :end, frames - 1))
  peak_env_equal?("scaled nokey env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.001)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.5, :offset, 0.5, :end, frames - 1))
  peak_env_equal?("scaled and offset env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.001)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0.5, 3, 0], :base, 0.0, :end, frames - 1))
  peak_env_equal?("env_channel base 0.0 peak", ind, channel_amp_envs(ind, 0, 1), 0.001)
  undo_edit
  xramp_channel(0.0, 1.0, 32.0)
  peak_env_equal?("xramp_channel 32.0 peak", ind, channel_amp_envs(ind, 0, 1), 0.008)
  undo_edit
  xramp_channel(0.0, 1.0, 0.032)
  peak_env_equal?("xramp_channel 0.032 peak", ind, channel_amp_envs(ind, 0, 1), 0.004)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0.5, 3, 0], :base, 10.0, :end, frames - 1))
  peak_env_equal?("env_channel base 10.0 peak", ind, channel_amp_envs(ind, 0, 1), 0.01)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0], :base, 0.1, :end, frames - 1))
  peak_env_equal?("env_channel base 0.1 peak", ind, channel_amp_envs(ind, 0, 1), 0.003)
  undo_edit
  insert_samples(1000, 5000, make_vct(5000, 0.5))
  peak_env_equal?("insert_samples peak", ind, channel_amp_envs(ind, 0, 1), 0.0001)
  undo_edit
  set_samples(500, 100, make_vct(100, 0.1))
  peak_env_equal?("set_samples peak", ind, channel_amp_envs(ind, 0, 1), 0.0001)
  undo_edit
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
  undo_edit
  env_channel([0, 0, 1, 1, 2, 0], 12000, 5000)
  peak_env_equal?("env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.003)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.5, :end, 4999), 12000, 5000)
  peak_env_equal?("scaled env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.004)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0], 0.5, :end, 4999), 12000, 5000)
  peak_env_equal?("scaled nokey env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.004)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.5, :offset, 0.5, :end, 4999), 12000, 5000)
  peak_env_equal?("scaled and offset env_channel peak", ind, channel_amp_envs(ind, 0, 1), 0.002)
  undo_edit
  xramp_channel(0.0, 1.0, 32.0, 2000, 1000)
  peak_env_equal?("xramp_channel 32.0 peak (1)", ind, channel_amp_envs(ind, 0, 1), 0.009)
  undo_edit
  xramp_channel(0.0, 1.0, 0.032, 2000, 1000)
  peak_env_equal?("xramp_channel 0.032 peak (1)", ind, channel_amp_envs(ind, 0, 1), 0.01)
  undo_edit
  env_channel(make_env([0, 0, 1, 1, 2, 0.5, 3, 0], :base, 10.0, :end, 4999), 12000, 5000)
  peak_env_equal?("env_channel base 10.0 peak", ind, channel_amp_envs(ind, 0, 1), 0.1)
  undo_edit
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
  undo_edit
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
        snd_display("%s init scan: %s?", name, res)
      end
    end
    chns.times do |i|
      map_channel(lambda do |n| $g_init_val end, 0, len, index, i)
      func.call(0, len, index, i, false)
      chns.times do |j|
        vi = channel2vct(0, len, index, j)
        if j == i
          snd_display("%s chan func: %s %s?", name, vi, val) unless vequal(vi, val)
        else
          if res = scan_channel(lambda do |n| n.abs > 0.001 end, 0, len, index, j)
            snd_display("%s chan func leaks? %d %d: %s", name, i, j, res)
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
            snd_display("%s ed chan func: %s %s?", name, vi, val)
          end
        else
          if res = scan_channel(lambda do |n| n.abs > 0.001 end, 0, len, index, j)
            snd_display("%s ed chan func leaks? %d %d %d: %s", name, i, j, ed, res)
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
            snd_display("%s chan func n: %s %s?", name, vi, val)
          end
        else
          if res = scan_channel(lambda do |n| n.abs > 0.001 end, 0, len, index, j)
            snd_display("%s dur chan func leaks? %d %d: %s", name, i, j, res)
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
                      clm_channel(make_oscil(:frequency, 0.0, :initial_phase, PI / 2.0),
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
    snd_display("region_frames after save: %s %s?", old_reglen, res)
  end
  (regions or []).zip(regdata) do |n, data|
    unless vequal(res = region2vct(0, 10, n), data)
      snd_display("region after save %s: %s %s?", n, data, res)
    end
  end
  index = find_sound("fmv.snd")
  if (res = maxamp(index, true)) != old_max
    snd_display("maxes: %s %s?", res, old_max)
  end
  snd_display("saved channel edits: %s?", edits(index)) if edits(index) != [276, 0]
  10.times do |i|
    pos = irandom(edits(index).first)
    scale_channel(random(2.0).abs, random(5.0), random(5.0), index, 0, pos)
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
    snd_display("src_channel: %s %s?", v, res)
  end
  unless vequal(res = channel2vct(0, 10, index, 1), make_vct(10))
    snd_display("src_channel leaks: %s?", res)
  end
  if (res = snd_catch do src(s, 1.0, lambda do |a, b| a end) end).first != :bad_arity
    snd_display("src bad func: %s?", res)
  end
  revert_sound(index)
  vct2channel(v0, 0, 10, index, 1)
  vct2channel(v0, 10, 10, index, 1)
  src_channel(make_env(:envelope, [1, 1, 2, 2], :end, 20), 0, 20, index, 1)
  unless vequal(res = channel2vct(0, 10, index, 1),
                vct(1.000, 0.000, -0.048, 0.068, -0.059, 0.022, 0.030, -0.100, 0.273, 0.606))
    snd_display("src_channel env: %s?", res)
  end
  unless vequal(res = channel2vct(0, 10, index, 0), make_vct(10))
    snd_display("src_channel env leaks: %s?", res)
  end
  # 
  revert_sound(index)
  vct2channel(v0, 0, 10, index, 1)
  vct2channel(v0, 10, 10, index, 1)
  src_channel(make_env(:envelope, [1, 1, 2, 2], :end, 20), 0, 20, index, 1)
  unless vequal(res = channel2vct(0, 10, index, 1),
                vct(1.000, 0.000, -0.048, 0.068, -0.059, 0.022, 0.030, -0.100, 0.273, 0.606))
    snd_display("src_channel env: %s?", res)
  end
  unless vequal(res = channel2vct(0, 10, index, 0), make_vct(10))
    snd_display("src_channel env leaks: %s?", res)
  end
  # 
  revert_sound(index)
  vct2channel(v0, 0, 10, index, 1)
  vct2channel(v0, 10, 10, index, 1)
  src_channel([1, 1, 2, 2], 0, 20, index, 1)
  unless vequal(res = channel2vct(0, 10, index, 1),
                vct(1.000, 0.000, -0.051, 0.069, -0.056, 0.015, 0.042, -0.117, 0.320, 0.568))
    snd_display("src_channel lst: %s?", res)
  end
  unless vequal(res = channel2vct(0, 10, index, 0), make_vct(10))
    snd_display("src_channel lst leaks: %s?", res)
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
    snd_display("deferred region after scaling:\n# %s\n# %s", rid0_data, res)
  end
  unless vequal(res = region_to_vct(rid0, 0, 20), rid0_data)
    snd_display("deferred region after scaling (rs):\n# %s\n# %s", rid0_data, res)
  end
  undo_edit
  scale_by(4.0)
  play_region(rid0, true)
  unless vequal(res = region2vct_1(rid0, 0, 20), rid0_data)
    snd_display("file region after scaling:\n# %s\n# %s", rid0_data, res)
  end
  unless vequal(res = region_to_vct(rid0, 0, 20), rid0_data)
    snd_display("file region after scaling (rs):\n# %s\n# %s", rid0_data, res)
  end
  rid1 = make_region(2000, 2020, ind, 0)
  rid1_data = region2vct_1(rid1, 0, 20)
  scale_to(0.5)
  unless vequal(res = region2vct_1(rid1, 0, 20), rid1_data)
    snd_display("deferred region after scale_to:\n# %s\n# %s", rid1_data, res)
  end
  close_sound(ind)
  play_region(rid0, true)
  play_region(rid1, true)
  unless vequal(res = region2vct_1(rid1, 0, 20), rid1_data)
    snd_display("deferred region after close:\n# %s\n# %s", rid1_data, res)
  end
  unless vequal(res = region2vct_1(rid0, 0, 20), rid0_data)
    snd_display("file region after close:\n# %s\n# %s", rid0_data, res)
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
      snd_display("deferred region after scaling (20):\n# %s\n# %s", rid20_data, res)
    end
    unless vequal(res = region_to_vct(rid2, 0, l1), rid20_data)
      snd_display("deferred region after scaling (20 rs):\n# %s\n# %s", rid20_data, res)
    end
    unless vequal(res = region2vct_1(rid2, 1, l2), rid21_data)
      snd_display("deferred region after scaling (21):\n# %s\n# %s", rid21_data, res)
    end
    unless vequal(res = region_to_vct(rid2, 1, l2), rid21_data)
      snd_display("deferred region after scaling (21 rs):\n# %s\n# %s", rid21_data, res)
    end
    close_sound(ind)
    unless vequal(res = region2vct_1(rid2, 0, l1), rid20_data)
      snd_display("deferred region after scaling (20):\n# %s\n# %s", rid20_data, res)
    end
    unless vequal(res = region_to_vct(rid2, 0, l1), rid20_data)
      snd_display("deferred region after scaling (20 rs):\n# %s\n# %s", rid20_data, res)
    end
    unless vequal(res = region2vct_1(rid2, 1, l2), rid21_data)
      snd_display("deferred region after scaling (21):\n# %s\n# %s", rid21_data, res)
    end
    unless vequal(res = region_to_vct(rid2, 1, l2), rid21_data)
      snd_display("deferred region after scaling (21 rs):\n# %s\n# %s",
                  rid21_data, res)
    end
  end
  ind = open_sound("obtest.snd")
  set_read_only(true, ind)
  delete_samples(0, 1000, ind, 0)
  if res = save_sound(ind)
    snd_display("save_sound read_only: %s?", res)
  end
  if (res = edits(ind)) != [1, 0]
    snd_display("read_only ignored? ", res)
  end
  unless provided? :snd_nogui
    if (res = widget_text(sound_widgets(ind)[3])) != "can\'t write obtest.snd (it is read-only)"
      snd_display("read_only report_in_minibuffer: %s?", res)
    end
    unless string?(res = widget_text(sound_widgets(ind)[4]))
      snd_display("widget_text of listener: %s?", res)
    end
  end
  set_read_only(false, ind)
  revert_sound(ind)
  save_sound(ind)
  unless provided? :snd_nogui
    if (res = widget_text(sound_widgets(ind)[3])) != "(no changes need to be saved)"
      snd_display("save unneeded report_in_minibuffer: %s?", res)
    end
  end
  key(?j, 4)
  unless provided? :snd_nogui
    if (res = widget_text(sound_widgets(ind)[3])) != "no marks"
      snd_display("C-j w/o marks: %s?", res)
    end
  end
  key(?-, 4)
  key(?j, 4)
  key(?j, 4)
  key(?x, 4)
  key(?c, 0)
  unless provided? :snd_nogui
    if (res = widget_text(main_widgets()[1]))
      snd_display("widget_text of non-text widget: %s", res)
    end
    set_widget_text(channel_widgets(ind, 0)[2], "F")
    if (res = widget_text(channel_widgets(ind, 0)[2])) != "F"
      snd_display("set button label to F: %s?", res)
    end
    if (res = widget_text(sound_widgets(ind)[3])) != "no marks"
      snd_display("C-x c w/o marks: %s?", res)
    end
  end
  add_mark(123)
  key(?u, 4)
  key(?6, 4)
  key(?j, 4)
  unless provided? :snd_nogui
    if (res = widget_text(sound_widgets(ind)[3])) != "no such mark"
      snd_display("C-u 6 C-j: %s?", res)
    end
  end
  key(?u, 4)
  key(?6, 4)
  key(?x, 4)
  key(?c, 0)
  unless provided? :snd_nogui
    if (res = widget_text(sound_widgets(ind)[3])) != "no such mark"
      snd_display("C-u 6 C-x c: %s?", res)
    end
  end
  close_sound(ind)
  #
  view_sound("obtest.snd")
  delete_samples(0, 1000, ind, 0)
  save_sound(ind)
  if (res = edits(ind)) != [1, 0]
    snd_display("view read_only ignored? ", res)
  end
  unless provided? :snd_nogui
    if (res = widget_text(sound_widgets(ind)[3])) != "can\'t write obtest.snd (it is read-only)"
      snd_display("view read_only report_in_minibuffer: %s?", res)
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
    map_channel(lambda do |y| rd.call end)
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
  map_channel(lambda do |y| rd.call end)
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
    map_channel(lambda do |y| rd.call end)
    rd = make_sample_reader(frames - 1, ind, 0, -1)
    map_channel(lambda do |y| rd.call end)
    old_rd = make_sample_reader(0, ind, 0, 1, edit_position(ind, 0) - 2)
    pos = 0
    scan_channel(lambda do |y|
                   if fneq(val = old_rd.call, y)
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
      if val = last_proc.call(reader.call)
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
    snd_display("my_scan_chan: %s?", res)
  end
  if (res = scan_again.call) != [true, 4463]
    snd_display("scan_again: %s?", res)
  end
  if (res = find(lambda do |y| find(lambda do |yy| yy > 0.1 end) end)) != [[true, 4423], 0]
    snd_display("find twice: %s?", res)
  end
  if (res = find(lambda do |y| count_matches(lambda do |yy| yy > 0.1 end) end)) != [2851, 0]
    snd_display("find+count: %s?", res)
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
    undo_edit
  else
    snd_display("env+scl was no-op")
  end
  env_channel(make_env([0, 1, 1, 1], :offset, 0.5, :end, 1000))
  check_maxamp(ind, 1.5, "simple scaler")
  check_env_vals("simple scaler", make_env([0, 1, 1, 1], :offset, 0.5, :end, 1000))
  if edit_position == 2
    undo_edit
  else
    snd_display("env+offset was no-op")
  end
  env_channel(make_env([0, 0, 1, 1, 2, 0], :offset, 0.5, :scaler, 2.0, :end, 1000))
  check_maxamp(ind, 2.5, "off+scl")
  check_env_vals("off+scl", make_env([0, 0, 1, 1, 2, 0], :offset, 0.5, :scaler, 2.0, :end, 1000))
  undo_edit
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
  undo_edit
  env_sound([0, 0.5, 1, 0.75, 2, 0.25], 0, frames, 32.0)
  check_maxamp(ind, 0.75, "xramp")
  check_env_vals("xramp", make_env([0, 0.5, 1, 0.75, 2, 0.25], :base, 32.0, :end, 1000))
  undo_edit
  env_channel_with_base([0, 0.5, 1, 0.75, 2, 0.25], 32.0)
  check_maxamp(ind, 0.75, "xramp1")
  check_env_vals("xramp1", make_env([0, 0.5, 1, 0.75, 2, 0.25], :base, 32.0, :end, 1000))
  close_sound(ind)
  #
  hlb = make_hilbert_transform(8)
  data = make_vct!(20) do |i| hilbert_transform(hlb, (i == 0 ? 1.0 : 0.0)) end
  unless vequal(data, vct(0.0, -0.010, 0.0, -0.046, 0.0, -0.152, 0.0, -0.614, 0.0, 0.614,
                          0.0, 0.152, 0.0, 0.046, 0.0, 0.010, 0.0, 0.0, 0.0, 0.0))
    snd_display("hilbert_transform impule response: %s?", data)
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
  undo_edit(2)
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
  map_channel(lambda do |y| 1.0 - random(2.0) end, 0, 10000)
  f2 = make_bandpass_2(0.12 * PI, 0.15 * PI, 0.22 * PI, 0.25 * PI, 100)
  map_channel(lambda do |y| bandpass_2(f2, y) end)
  data = channel2vct
  undo_edit
  f1 = make_bandpass(0.12 * PI, 0.15 * PI, 100)
  f2 = make_bandpass(0.22 * PI, 0.25 * PI, 100)
  map_channel(lambda do |y| bandpass(f1, y) + bandpass(f2, y) end)
  data1 = channel2vct
  vct_subtract!(data, data1)
  snd_display("fir_filter 2: %s", vct_peak(data)) if vct_peak(data) > 0.00001
  undo_edit
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
      snd_display("env reordering test %s: %s %s", name, v1, v2)
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
        snd_display("scaled (2) env reordering test %s: %s %s", name, v1, v2)
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
        snd_display("scaled (0.5) env reordering test %s: %s %s", name, v1, v2)
      end
      set_edit_position(edpos, ind, 0)
    end
  end
  close_sound(ind)
  # offset channel
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "offset tests", 10)
  offset_channel(0.1)
  unless vequal(res = channel2vct(0, 10), make_vct(10, 0.1))
    snd_display("offset_channel (0.1): %s", res)
  end
  offset_channel(-0.2, 5, 5)
  unless vequal(res = channel2vct(0, 10),
                vct(0.1, 0.1, 0.1, 0.1, 0.1, -0.1, -0.1, -0.1, -0.1, -0.1))
    snd_display("offset_channel (-0.1): %s", res)
  end
  undo_edit
  offset_channel(0.9, 0, 10, ind, 0)
  unless vequal(res = channel2vct(0, 10), make_vct(10, 1.0))
    snd_display("offset_channel (1): %s", res)
  end
  revert_sound(ind)
  # sine_env and sine_ramp...
  map_channel($init_channel)
  sine_ramp(0.0, 1.0)
  unless vequal(res = channel2vct,
                vct(0.000, 0.024, 0.095, 0.206, 0.345, 0.500, 0.655, 0.794, 0.905, 0.976))
    snd_display("sine_ramp 0 1: %s", res)
  end
  revert_sound(ind)
  offset_channel(1.0)
  sine_ramp(1.0, 0.0)
  unless vequal(res = channel2vct,
                vct(1.000, 0.976, 0.905, 0.794, 0.655, 0.500, 0.345, 0.206, 0.095, 0.024))
    snd_display("sine_ramp 1 0: %s", res)
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
    snd_display("sine_env_channel 0:\n# %s\n# %s", res1, res2)
  end
  if (res = edit_position(ind, 0)) != 2
    snd_display("as_one_edit sine_env_channel: %s", res)
  end
  revert_sound(ind)
  offset_channel(-1.0)
  sine_env_channel([0, 0, 1, 1, 2, 1, 3, 0], 40, 20)
  if (not vequal(res1 = channel2vct(40, 20),
                 vct(0.000, -0.050, -0.188, -0.389, -0.611, -0.812, -0.950, -1.000,
                     -1.000, -1.000, -1.000, -1.000, -1.000, -1.000, -1.000, -0.950,
                     -0.812, -0.611, -0.389, -0.188))) or
      (not vequal(res2 = channel2vct(30, 10), make_vct(10, -1.0)))
    snd_display("off+sine_env:\n# %s\n# %s", res1, res2)
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
  undo_edit
  blackman4_env_channel([0, 0, 1, 1])
  unless vequal(res = channel2vct, vals)
    snd_display("blackman4_env_channel/ramp:\n# %s\n# %s", vals, res)
  end
  undo_edit
  blackman4_ramp(0.0, 1.0, 0, 50)
  vals = channel2vct
  undo_edit
  blackman4_env_channel([0, 0, 1, 1, 2, 1])
  unless vequal(res = channel2vct, vals)
    snd_display("blackman4_env_channel/ramp 1:\n# %s\n# %s", vals, res)
  end
  undo_edit
  blackman4_env_channel([0, 0, 1, 1, 2, -0.5, 3, 0])
  unless vequal(res = channel2vct(60, 10),
                vct(-0.109, -0.217, -0.313, -0.392, -0.451, -0.488, -0.499, -0.499, -0.499, -0.499))
    snd_display("blackman4_env_channel to -0.5: %s", res)
  end
  undo_edit
  # 
  ramp_squared(0.0, 1.0)
  vals = channel2vct
  undo_edit
  env_squared_channel([0, 0, 1, 1])
  unless vequal(res = channel2vct, vals)
    snd_display("env_squared/ramp:\n# %s\n# %s", vals, res)
  end
  undo_edit
  ramp_squared(0.0, 1.0, true, 0, 50)
  vals = channel2vct
  undo_edit
  env_squared_channel([0, 0, 1, 1, 2, 1])
  unless vequal(res = channel2vct, vals)
    snd_display("env_squared/ramp 1:\n# %s\n# %s", vals, res)
  end
  undo_edit
  env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 0])
  unless vequal(res = channel2vct(60, 10),
                vct(-0.450, -0.466, -0.478, -0.488, -0.494, -0.499, -0.500, -0.500, -0.498, -0.496))
    snd_display("env_squared to -0.5: %s", res)
  end
  undo_edit
  env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 0], false)
  unless vequal(res = channel2vct(60, 10),
                vct(-0.004, -0.080, -0.158, -0.240, -0.324, -0.410, -0.500, -0.500, -0.498, -0.496))
    snd_display("env_squared unsymmetric to -0.5: %s", res)
  end
  undo_edit
  # 
  ramp_squared(0.0, 1.0)
  vals = channel2vct
  undo_edit
  env_expt_channel([0, 0, 1, 1], 2)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt2/ramp:\n# %s\n# %s", vals, res)
  end
  undo_edit
  env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 0])
  vals = channel2vct
  undo_edit
  env_expt_channel([0, 0, 1, 1, 2, -0.5, 3, 0], 2.0)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt2/env_squared:\n# %s\n# %s", vals, res)
  end
  undo_edit
  env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 0], false)
  vals = channel2vct
  undo_edit
  env_expt_channel([0, 0, 1, 1, 2, -0.5, 3, 0], 2.0, false)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt2/env_squared unsymmetric:\n# %s\n# %s", vals, res)
  end
  undo_edit
  #
  ramp_expt(0.0, 1.0, 32.0)
  vals = channel2vct
  undo_edit
  env_expt_channel([0, 0, 1, 1], 32.0)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt/ramp 32:\n# %s\n# %s", vals, res)
  end
  undo_edit
  ramp_expt(0.0, 1.0, 32.0, false, 0, 50)
  vals = channel2vct
  undo_edit
  env_expt_channel([0, 0, 1, 1, 2, 1], 32.0)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt/ramp 1 32.0:\n# %s\n# %s", vals, res)
  end
  undo_edit
  ramp_expt(0.0, 1.0, 0.1)
  vals = channel2vct
  undo_edit
  env_expt_channel([0, 0, 1, 1], 0.1)
  unless vequal(res = channel2vct, vals)
    snd_display("env_expt/ramp 0.1:\n# %s\n# %s", vals, res)
  end
  undo_edit
  env_expt_channel([0, 0, 1, 1, 2, -0.5, 3, 0], 12.0)
  unless vequal(res = channel2vct(30, 10),
                vct(0.319, 0.472, 0.691, 1.000, 0.537, 0.208, -0.022, -0.182, -0.291, -0.365))
    snd_display("env_expt to -0.5 12.0\n# %s\n# %s", vals, res)
  end
  undo_edit
  env_expt_channel([0, 0, 1, 1, 2, -0.5, 3, 0], 12.0, false)
  unless vequal(res = channel2vct(30, 10),
                vct(0.319, 0.472, 0.691, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000))
    snd_display("env_expt ot -0.5 12.0 unsymmetric:\n# %s\n# %s", vals, res)
  end
  undo_edit
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
  test105 unless provided? :snd_nogui
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

def test006
  v0 = make_vct(10)
  v1 = Vct.new(10)
  vlst = make_vct(3)
  snd_display("v0 isn\'t a vct?") unless vct?(v0)
  snd_display("v0 isn\'t kind_of? Vct?") unless v0.kind_of?(Vct)
  snd_display("v0 is 10!?") if v0 == 10
  snd_display("10 is a vct?") if vct?(10)
  snd_display("v0 length = %d?", v0.length) if v0.length != 10
  vct_fill!(v0, 1.0)
  v1.fill(0.5)
  snd_display("vct %s.eql?(%s)?", v0, v1) if v0.eql?(v1)
  snd_display("vct %s == %s?", v0, v1) if v0 == v1
  v2 = v1
  v3 = Vct.new(10)
  v4 = make_vct(3)
  snd_display("vct not %s.eql?(%s)?", v1, v2) unless v1.eql?(v2)
  vct_fill!(v3, 0.5)
  snd_display("vct not %s.eql?(%s)?", v2, v1) unless v2.eql?(v1)
  snd_display("len diff vct %s.eql?(%s)?", v4, v1) if v4.eql?(v1)
  v3[0] = 1.0
  snd_display("vct_set!: %s", v3[0]) if fneq(v3[0], 1.0)
  vlst[1] = 0.1
  unless vequal(res = vct2list(vlst), [0.0, 0.1, 0.0])
    snd_display("vct2list: %s?", res)
  end
  vect = [0.0, 1.0, 2.0, 3.0]
  v123 = vct(0.0, 1.0, 2.0, 3.0)
  v2 = vect.to_vct
  v3 = v2
  str = format("%s", v2.inspect)
  str1 = format("%s", make_vct(32).inspect)
  unless (res = vector2vct(make_array(0))).eql?(false)
    snd_display("vector2vct empty vect: %s", res)
  end
  unless (res = make_array(0).to_vct).eql?(false)
    snd_display("make_array(0).to_vct empty vect: %s", res)
  end
  if str != "#<vct[len=4]: 0.000 1.000 2.000 3.000>"
    snd_display("vct print:\n# %s\n# %s?", str, v2.inspect)
  end
  if print_length == 12 and
      str1 != "#<vct[len=32]: 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 ...>"
    snd_display("vct(32) print: %s?", str1)
  end
  snd_display("vector2vct: %s", v2) unless vequal(v123, v2)
  unless (res = vct2vector(v123)) == vect
    snd_display("vct2vector:\n# %s\n# %s", vect, res)
  end
  unless (res = v123.to_a) == vect
    snd_display("v123.to_a:\n# %s\n# %s", vect, res)
  end
  snd_display("vct=? %s %s", v2, v3) unless v3.eql?(v2)
  snd_display("vector2vct length: %s?", v2.lenght) if v2.length != 4
  snd_display("vector2vct: %s?", v2) if fneq(v2[2], 2.0)
  vct_move!(v2, 0, 2)
  snd_display("vct_move!: %s?", v2) if fneq(v2[0], 2.0)
  v2 = Vct.new(4) do |i| i end
  v2.move!(3, 2, true)
  snd_display("vct_move! back: %s?", v2) if fneq(v2[3], 2.0) or fneq(v2[2], 1.0)
  unless vequal(vct(4, 3, 2, 1), res = vct_reverse!(vct(1, 2, 3, 4)))
    snd_display("vct_reverse: %s?", res)
  end
  unless vequal(vct(3, 2, 1), res = vct(1, 2, 3).reverse)
    snd_display("vct_reverse: %s?", res)
  end
  unless vequal(vct(2, 1), res = vct_reverse!(vct(1, 2)))
    snd_display("vct_reverse: %s?", res)
  end
  unless vequal(vct(1), res = vct(1).reverse)
    snd_display("vct_reverse: %s?", res)
  end
  unless vequal(vct(4, 3, 2, 1), res = vct(1, 2, 3, 4).reverse(4))
    snd_display("vct_reverse: %s?", res)
  end
  unless vequal(vct(3, 2, 1), res = vct_reverse!(vct(1, 2, 3), 3))
    snd_display("vct_reverse: %s?", res)
  end
  unless vequal(vct(2, 1), res = vct(1, 2).reverse(2))
    snd_display("vct_reverse: %s?", res)
  end
  unless vequal(vct(1), res = vct_reverse!(vct(1), 1))
    snd_display("vct_reverse: %s?", res)
  end
  #
  vv0 = Vct.new(3)
  if (res = snd_catch do vct_ref(vv0, 10) end).first != :out_of_range
    snd_display("vct_ref high index: %s", res)
  end
  if (res = snd_catch do vv0[-1] end).first != :out_of_range
    snd_display("[] low index: %s", res)
  end
  if (res = snd_catch do vct_set!(vv0, 10, 1.0) end).first != :out_of_range
    snd_display("vct_set! high index: %s", res)
  end
  if (res = snd_catch do vv0[-1] = 1.0 end).first != :out_of_range
    snd_display("[]= low index: %s", res)
  end
  if (res = snd_catch do vct_move!(vv0, 10, 0, true) end).first != :out_of_range
    snd_display("vct_move! high index: %s", res)
  end
  if (res = snd_catch do vv0.move(0, 10, true) end).first != :out_of_range
    snd_display("v.move high 2 index: %s", res)
  end
  if (res = snd_catch do vct_move!(vv0, -10, 0, false) end).first != :out_of_range
    snd_display("vct_move! back high index: %s", res)
  end
  if (res = snd_catch do vv0.move!(0, -10, false) end).first != :out_of_range
    snd_display("v.move! back high 2 index: %s", res)
  end
  10.times do |i|
    snd_display("fill v0[%d] = %f?", i, v0[i]) if fneq(v0[i], 1.0)
    snd_display("preset v1[%d] = %f?", i, v1[i]) if fneq(v1[i], 0.5)
  end
  # add
  v0.add(v1).each_with_index do |x, i|
    snd_display("v0.add[%d] = %f?", i, x) if fneq(x, 1.5)
  end
  vct_add!(v0, v1)
  v0.each_with_index do |x, i|
    snd_display("add v0[%d] = %f?", i, x) if fneq(x, 1.5)
  end
  # subtract
  v0.subtract(v1).each_with_index do |x, i|
    snd_display("v0.subtract[%d] = %f?", i, x) if fneq(x, 1.0)
  end
  vct_subtract!(v0, v1)
  v0.each_with_index do |x, i| snd_display("subtract v0[%d] = %f?", i, x) if fneq(x, 1.0) end
  # dup, vct_copy
  v0.dup.each_with_index do |x, i|
    snd_display("v0.dup[%d] = %f?", i, x) if fneq(x, 1.0)
  end
  v2 = vct_copy(v0)
  v2.each_with_index do |x, i|
    snd_display("copy v2[%d] = %f?", i, x) if fneq(x, 1.0)
  end
  # scale
  v2.scale(5.0).each_with_index do |x, i|
    snd_display("v2.scale[%d] = %f?", i, x) if fneq(x, 5.0)
  end
  vct_scale!(v2, 5.0)
  v2.each_with_index do |x, i| snd_display("scale v2[%d] = %f?", i, x) if fneq(x, 5.0) end
  # offset
  v0.offset(-1.0).each_with_index do |x, i|
    snd_display("v0.offset[%d] = %f?", i, x) if fneq(x, 0.0)
  end
  vct_offset!(v0, -1.0)
  v0.each_with_index do |x, i| snd_display("offset v0[%d] = %f?", i, x) if fneq(x, 0.0) end
  # multiply
  v2.multiply(v1).each_with_index do |x, i|
    snd_display("v2.multiply[%d] = %f?", i, x) if fneq(x, 2.5)
  end
  vct_multiply!(v2, v1)
  v2.each_with_index do |x, i| snd_display("multiply v2[%d] = %f?", i, x) if fneq(x, 2.5) end
  # 
  snd_display("v2\'s peak is %f?", vct_peak(v2)) if fneq(vct_peak(v2), 2.5)
  snd_display("v2.peak is %f?", vct_peak(v2)) if fneq(v2.peak, 2.5)
  vct_set!(v2, 5, 123.0)
  snd_display("v2\'s set peak is %f?", vct_peak(v2)) if fneq(vct_peak(v2), 123.0)
  snd_display("v2.peak is %f?", vct_peak(v2)) if fneq(v2.peak, 123.0)
  vn = Vct.new(32) do |i| i end
  vb = make_vct(64)
  vs = make_vct(3)
  vss = Vct.new(1)
  vnew = vct_subseq(vn, 3)
  snd_display("vct_subseq[3:] %f?", vneq[0]) if fneq(vnew[0], 3.0)
  snd_display("vct_subseq[3:] length %d?", vnew.length) if vnew.length != 29
  vnew = vn.subseq(3, 8)
  snd_display("v.subseq[3:8] %f?", vneq[0]) if fneq(vnew[0], 3.0)
  snd_display("v.subseq[3:8] length %d?", vnew.length) if vnew.length != 6
  vct_subseq(vn, 3, 3, vs)
  if fneq(vs[0], 3.0) or fneq(vs[1], 0.0) or fneq(vs[2], 0.0)
    snd_display("vct_subseq[3:3->vs] %s?", vs)
  end
  vn.subseq(0, 32, vs)
  snd_display("v.subseq[:32->vs] length %d?", vs.length) if vs.length != 3
  vn.subseq(2, 3, vss)
  snd_display("v.subseq[2:3->vss] %f?", vss[0]) if fneq(vss[0], 2.0)
  vct_set!(vb, 8, 123.0)
  vct_subseq(vn, 1, 8, vb)
  snd_display("vct_subseq[1:8->vb] %f?", vb[0]) if fneq(vb[0], 1.0)
  snd_display("vct_subseq[1:8->vb][8] %f?", vb[8]) if fneq(vb[8], 123.0)
  v0.map do |val| PI end.each_with_index do |x, i|
    snd_display("v0.map[%d] = %f?", i, x) if fneq(x, PI)
  end
  vct_map!(v0, lambda do | | 1.0 end)
  v0.each_with_index do |x, i| snd_display("map v0[%d] = %f?", i, x) if fneq(x, 1.0) end
end

def test016
  fd = open_sound_file
  close_sound_file(fd, 0)
  name = little_endian? ? "test.wav" : "test.snd"
  frm = little_endian? ? Mus_lfloat : Mus_bfloat
  if (res = mus_sound_frames(name)).nonzero?
    snd_display("open_sound_file no out frames: %s?", res)
  end
  if (res = mus_sound_data_format(name)) != frm
    snd_display("open_sound_file format: %s", mus_data_format_name(res))
  end
  delete_file("hiho.snd")
  mus_sound_forget("hiho.snd")
  typ = little_endian? ? Mus_riff : Mus_aifc
  fd = open_sound_file("hiho.snd", 2, 44100, false, typ)
  vct2sound_file(fd, vct(0.1, 0.2, 0.01, 0.02), 4)
  close_sound_file(fd, 4 * 4)
  if (res = mus_sound_chans("hiho.snd")) != 2
    snd_display("open_sound_file 2 chans: %s?", res)
  end
  if (res = mus_sound_srate("hiho.snd")) != 44100
    snd_display("open_sound_file srate: %s?", res)
  end
  if (res = mus_sound_header_type("hiho.snd")) != typ
    snd_display("open_sound_file type: %s?", res)
  end
  ind = open_sound("hiho.snd")
  if fneq(maxamp(ind, 0), 0.1) or fneq(maxamp(ind, 1), 0.2)
    snd_display("vct2sound_file vals: %s %s?", channel2vct(0, 2, ind, 0), channel2vct(0, 2, ind, 1))
  end
  if (res = frames(ind)) != 2
    snd_display("vct2channel frames: %d?", res)
  end
  close_sound(ind)
  delete_file("hiho.snd")
  mus_sound_forget("hiho.snd")
  fd = open_sound_file(:channels, 4, :file, "hiho.snd", :header_type, typ, :srate, 8192)
  close_sound_file(fd, 0)
  if (res = mus_sound_chans("hiho.snd")) != 4
    snd_display("open_sound_file 4 chans: %s?", res)
  end
  if (res = mus_sound_srate("hiho.snd")) != 8192
    snd_display("open_sound_file low srate: %s?", res)
  end
  if (res = mus_sound_header_type("hiho.snd")) != typ
    snd_display("open_sound_file (2nd) type: %s?", res)
  end
  v2 = make_vct(10, 2.5)
  fd = open_sound_file("hiho.snd", 1, :srate, 22050, :comment, "hiho is from snd-test")
  vct2sound_file(fd, v2, 10)
  close_sound_file(fd, 10)
  if (res = snd_catch do vct2sound_file(-1, v2, 1) end).first != :out_of_range
    snd_display("vct2sound_file bad fd: %s?", res)
  end
  v3 = Vct.new(40)
  file2array("hiho.snd", 0, 0, 10, v3)
  snd_display("vct2sound_file: %s %s?", v2, v3) if fneq(v3[5], v2[5])
  snd_display("vct(...) = %f?", vct(1.0, 2.0, 3.0)[1]) if fneq(vct(1.0, 2.0, 3.0)[1], 2.0)
  v1 = [1, 2, 3, 4].to_vct
  snd_display("v1[1] = %f?", v1[1]) if fneq(v1[1], 2.0)
end

def test026
  ind = open_sound("oboe.snd")
  set_speed_control(0.5, ind)
  play_and_wait
  apply_controls
  revert_sound
  reset_controls(ind)
  # 
  # try some special cases
  #
  apply_controls
  if edit_position(ind).nonzero?
    snd_display("apply_controls with no change: %s: %s",
                edits(ind), edit_tree(ind))
  end
  set_speed_control(-1.0, ind)
  apply_controls
  if edit_position(ind) != 1
    snd_display("apply_controls with srate -1.0: %s: %s",
                edits(ind), edit_tree(ind))
  end
  if (res0 = frames(ind, 0) - res1 = frames(ind, 0, 0)).abs > 2
    snd_display("apply_controls srate -1.0 lengths: %s %s", res0, res1)
  end
  if fneq(res0 = maxamp, 0.147) or (res1 = sample(9327)).abs < 0.01
    snd_display("apply_controls srate -1.0 samples: %f %f?", res0, res1)
  end
  if fneq(res = speed_control(ind), 1.0)
    snd_display("apply_controls -1.0 -> %f?", res)
  end
  ctr = 0
  $dac_hook.add_hook!("snd-test") do |data|
    ctr += 1
    c_g! if ctr >= 3
  end
  play_and_wait
  snd_display("ctr after dac_hook: %d", ctr) if ctr != 3
  set_speed_control(1.5)
  apply_controls
  # orig: if fneq(res = sample(28245), 0.0)
  if fneq_err(res = sample(28245), 0.0, 0.045)
    snd_display("dac_hook stop apply_controls? %f", res)
  end
  $dac_hook.reset_hook!
  revert_sound
  set_speed_control(1.5)
  ctr = 0
  $dac_hook.add_hook!("snd-test") do |data|
    ctr += 1
    apply_controls if ctr == 3
  end
  play_and_wait
  if edit_position(ind, 0) != 1
    snd_display("apply_controls from hook: %s %s", edits(ind), edit_tree(ind))
  end
  $dac_hook.reset_hook!
  revert_sound
  set_speed_control(1.5)
  stop_playing
  $after_apply_hook.add_hook!("snd-test") do |s|
    if (res = snd_catch do apply_controls end).first != :cannot_apply_controls
      snd_display("after_apply_hook: recursive attempt apply_controls: %s", res)
    end
  end
  apply_controls
  $after_apply_hook.reset_hook!
  $dac_hook.add_hook!("snd-test") do |s|
    if (res = snd_catch do apply_controls end).first != :cannot_apply_controls
      snd_display("dac_hook: recursive attempt apply_controls: %s", res)
    end
  end
  $dac_hook.reset_hook!
  revert_sound
  close_sound(ind)
  # 
  # Vct.new.map twice, Vct.new twice, and vct_map! twice
  # 
  v1 = Vct.new(32)
  v1.map! do
    v2 = Vct.new(3)
    v2.map! do 0.1 end
    v2.first
  end
  snd_display("v.map! twice: %f?", v1[12]) if fneq(v1[12], 0.1)
  Vct.new(32) do Vct.new(3) do 0.1 end.first end
  snd_display("Vct.new twice: %f?", v1[12]) if fneq(v1[12], 0.1)
  v1 = make_vct(32)
  vct_map!(v1, lambda do | |
             v2 = make_vct(3)
             vct_map!(v2, lambda do | | 0.1 end)
             vct_ref(v2, 0)
           end)
  snd_display("vct_map! twice: %f?", v1[12]) if fneq(v1[12], 0.1)
  hi = make_vct(3)
  if (res = snd_catch do vct_subseq(hi, 1, 0) end).first != :out_of_range
    snd_display("vct_subseq 1 0: %s", res.inspect)
  end
  if (res = snd_catch do vct() end).first != :wrong_type_arg
    snd_display("vct() -> %s?", res.inspect)
  end
  if (res = snd_catch do make_vct(0) end).first != :out_of_range
    snd_display("make_vct(0) -> %s?", res.inspect)
  end
  ho = make_vct(3)
  vct_add!(hi, ho, 4)
  v0 = make_vct(5, 0.1)
  v1 = make_vct(6, 0.2)
  v0.add!(v1, 2)
  snd_display("v.add! + offset: %s?", v0) unless vequal(v0, [0.1, 0.1, 0.3, 0.3, 0.3].to_vct)
end

def test036
  # 
  # vct methods
  # 
  if (v1 = Vct.new(10)) != (v2 = make_vct(10))
    snd_display("Vct.new 0.000: %s %s?", v1, v2)
  end
  if (v1 = Vct.new(10, 3.14)) != (v2 = make_vct(10, 3.14))
    snd_display("Vct.new 3.140: %s %s?", v1, v2)
  end
  v1 = Vct.new(10) do |i| i * 0.01 end
  v2 = make_vct(10)
  ctr = -1
  vct_map!(v2, lambda do | | (ctr += 1) * 0.01 end)
  if v1 != v2
    snd_display("Vct.new 0.000...0.090: %s %s?", v1, v2)
  end
  if vct_ref(v1, 8) != v2[8] or v2[8] != 0.08
    snd_display("Vct#[]: %s %s?", vct_ref(v1, 8), v2[8])
  end
  vct_set!(v1, 8, 0.5)
  v2[8] = 0.5
  if vct_ref(v1, 8) != v2[8] or v2[8] != 0.5
    snd_display("Vct#[]=: %s %s?", vct_ref(v1, 8), v2[8])
  end
  if v1.length != vct_length(v2) or v2.length != vct_length(v1) or v2.length != 10
    snd_display("Vct#length: %s %s %s %s?", v1.length, vct_length(v1), v2.length, vct_length(v2))
  end
  v1.each_with_index do |val, i|
    if val != vct_ref(v2, i)
      snd_display("Vct#each: %s %s?", val, vct_ref(v2, i))
    end
  end
  if (v1 <=> v2).nonzero?
    snd_display("Vct#<=> (0): %s?", (v1 <=> v2))
  end
  if (v3 = Vct.new(10) do |i| i * 0.001 end <=> v1) != -1
    snd_display("Vct#<=> (-1): %s?", (v3 <=> v1))
  end
  if (v2 <=> (v3 = Vct.new(10) do |i| i * 0.001 end)) != 1
    snd_display("Vct#<=> (1): %s?", (v2 <=> v3))
  end
  v2.map! do |val| val + 0.5 end
  v3 = v1.map do |val| val + 0.5 end
  if v2 != v3
    snd_display("Vct#map(!): %s %s?", v2, v3)
  end
  v2 = v1.dup
  if (v1 <=> v2).nonzero?
    snd_display("Vct#dup: %s?", v1, v2)
  end
  vec1 = make_array(10) do |i| i * 0.01 end
  vec1[8] = 0.5
  vec2 = v2.to_a
  if vec1 != vec2
    snd_display("Vct#to_a: %s %s?", vec1, vec2)
  end
  if vec1.to_vct != v1
    snd_display("Array#to_vct: %s %s?", vec1.to_vct, v1)
  end
  if vct2string(v1) != v2.to_str or
      v2.to_str != "vct(0.000, 0.010, 0.020, 0.030, 0.040, 0.050, 0.060, 0.070, 0.500, 0.090)"
    snd_display("Vct#to_str:\n# %s\n# %s?", vct2string(v1), v2.to_str)
  end
  if v1.peak != vct_peak(v2)
    snd_display("Vct#peak: %s %s?", v1.peak, vct_peak(v2))
  end
  v3 = v1.dup
  v3.add!(v2)
  v4 = v1.add(v2)
  if v3 != v4
    snd_display("Vct#add(!): %s %s?", v3, v4)
  end
  v3 = v1.dup
  v3.subtract!(v2)
  v4 = v1.subtract(v2)
  if v3 != v4
    snd_display("Vct#subtract(!): %s %s?", v3, v4)
  end
  v3 = v1.dup
  v3.multiply!(v2)
  v4 = v1.multiply(v2)
  if v3 != v4
    snd_display("Vct#multiply(!): %s %s?", v3, v4)
  end
  v3 = v1.dup
  v3.offset!(0.5)
  v4 = v1.offset(0.5)
  if v3 != v4
    snd_display("Vct#offset(!): %s %s?", v3, v4)
  end
  v3 = v1.dup
  v3.scale!(2.0)
  v4 = v1.scale(2.0)
  if v3 != v4
    snd_display("Vct#scale(!): %s %s?", v3, v4)
  end
  v3 = Vct.new(10)
  v4 = Vct.new(10)
  v3.fill(0.5)
  vct_fill!(v4, 0.5)
  if v3 != v4
    snd_display("Vct#fill: %s %s?", v3, v4)
  end
  if v1.first != vct_ref(v2, 0)
    snd_display("Vct#first: %s %s?", v1.first, vct_ref(v2, 0))
  end
  if v1.last != vct_ref(v2, vct_length(v2) - 1)
    snd_display("Vct#last: %s %s?", v1.last, vct_ref(v2, vct_length(v2) - 1))
  end
  v1.first = 0.2
  vct_set!(v2, 0, 0.2)
  if v1.first != vct_ref(v2, 0) or v1.first != 0.2
    snd_display("Vct#first: %s %s?", v1.first, vct_ref(v2, 0))
  end
  v1.last = 0.3
  vct_set!(v2, vct_length(v2) - 1, 0.3)
  if v1.last != vct_ref(v2, vct_length(v2) - 1) or v1.last != 0.3
    snd_display("Vct#last: %s %s?", v1.last, vct_ref(v2, vct_length(v2) - 1))
  end
  # 
  # make_fm_violin (v.rb)
  # 
  samps = 1000
  ind = new_sound(:file, "fmv.snd", :srate, 22050, :channels, 2, :size, samps)
  dur = samples2seconds(samps)
  # thunk
  fmv1 = make_fm_violin(0, dur, 440, 0.5, :thunk?, true)
  v3 = make_vct(samps)
  vct_map!(v3, fmv1)
  vct2channel(v3, 0, samps, ind, 0)
  # proc with one arg
  fmv2 = make_fm_violin(0, dur, 440, 0.5, :thunk?, false)
  map_channel(fmv2, 0, samps, ind, 1)
  unless vfequal(v3 = channel2vct(100, 100, ind, 0), v4 = channel2vct(100, 100, ind, 1))
    snd_display("make_fm_violin:\n# %s\n# %s?", v3, v4)
  end
  close_sound(ind)
  delete_file("fmv.snd")
end

def test06
  # setting print_length (12) and vct_print_length (10) to the same size
  set_print_length(print_length)
  test006
  test016
  test026
  test036
end

if $test06 and $full_test or $snd_test == 6
  $before_test_hook.call(6)
  test06
  $after_test_hook.call(6)
end

# ---------------- test 07: colors ----------------

def test007
  c1 = snd_catch(:no_such_color, false) do make_color(0, 0, 1) end.first
  c2 = c1
  c3 = snd_catch(:no_such_color, false) do make_color(0, 0, 1) end.first
  snd_display("color equal? %s %s", c1, c2) unless c1.equal?(c2)
  snd_display("color eql? %s %s", c1, c2) unless c1.eql?(c2)
  snd_display("color == %s %s", c1, c2) unless c1 == c2
  if provided? :snd_motif
    snd_display("diff color equal? %s %s", c1, c3) unless c1.eql?(c3)
    snd_display("diff color eql? %s %s", c1, c3) unless c1.eql?(c3)
  end
  if (res = color2list(c1)) != [0.0, 0.0, 1.0]
    snd_display("color2list: %s %s?", c1, res)
  end
  true_color_list = [
    [0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 1.0, 1.0],
    [0.0, 0.0, 7.01915007248035e-4], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0],
    [0.0, 0.0, 0.49999], [1.0, 0.0, 0.0], [1.0, 0.0, 0.0], [0.0, 0.0, 1.0],
    [1.0, 0.0, 1.0], [0.0, 0.500007629510948, 0.4], [1.0, 0.0, 0.0],
    [1.0, 0.0, 0.0], [0.0, 0.0, 0.0]]
  Last_colormap.times do |i|
    if colormap?(i)
      unless vequal(res0 = colormap_ref(i, 0), res1 = true_color_list[i])
        snd_display("colormap_ref[%d]: %s (%s)", i, res0, res1)
      end
    end
  end
  snd_catch do
    [[:basic_color, Ivory2],
      [:cursor_color, Red],
      [:data_color, Black],
      [:enved_waveform_color, Blue],
      [:filter_control_waveform_color, Blue],
      [:graph_color, White],
      [:highlight_color, Ivory1],
      [:listener_color, Alice_blue],
      [:listener_text_color, Black],
      [:mark_color, Red],
      [:mix_color, Dark_gray],
      [:position_color, Ivory3],
      [:pushed_button_color, Lightsteelblue1],
      [:sash_color, Light_green],
      [:selected_data_color, Black],
      [:selected_graph_color, White],
      [:selection_color, Lightsteelblue1],
      [:text_focus_color, White],
      [:zoom_color, Ivory4],
      [:quit_button_color, Indian_red],
      [:help_button_color, Lightsteelblue2],
      [:reset_button_color, Goldenrod1],
      [:doit_button_color, Palegreen2],
      [:doit_again_button_color, Darkolivegreen1]].each do |getfnc, initval|
      snd_display("%s not color?", initval) unless color?(initval)
      set_snd_var(getfnc, Beige)
      snd_display("set_%s != Beige (%s)?", getfnc, snd_var(getfnc)) if snd_var(getfnc) != Beige
      set_snd_var(getfnc, initval)
    end
    ind = open_sound("oboe.snd")
    set_selected_data_color(Light_green)
    set_data_color(Blue)
    set_selected_graph_color(Light_green)
    red = make_color_with_catch(1.0, 0.0, 0.0)
    set_foreground_color(red, ind, 0, Cursor_context)
    if provided? :snd_motif
      if (res = foreground_color(ind, 0, Cursor_context)) != red
        snd_display("set_foreground_color cursor: %s %s?", res, red)
      end
      set_foreground_color(Blue)
      if (res = foreground_color) != Blue
        snd_display("set_foreground_color: %s %s?", res, Blue)
      end
      set_foreground_color(Black, ind)
      if (res = foreground_color(ind)) != Black
        snd_display("set_foreground_color with ind: %s %s?", res, Black)
      end
    end
    set_selected_graph_color(make_color_with_catch(0.96, 0.96, 0.86))
    set_data_color(Black)
    set_selected_data_color(Blue)
    set_data_color(White)
    close_sound(ind)
  end
end

def test017
  [[512, 0.005], [64, 0.04]].each do |n, err|
    set_colormap_size(n)
    10.times do |i|
      x = random(1.0)
      r = (x < (3.0 / 4)) ? ((7.0 / 8) * x) : ((11.0 / 8) * x - 3.0 / 8)
      g = (x < (3.0 / 8)) ? ((7.0 / 8) * x) : ((x < (3.0 / 4)) ?
                                               ((29.0 / 24) * x - 1.0 / 8) :
                                                 ((7.0 / 8) * x + 1.0 / 8))
      b = (x < (3.0 / 8)) ? ((29.0 / 24) * x) : ((7.0 / 8) * x + 1.0 / 8)
      rgb = colormap_ref(Bone_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("bone %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = (x < (4.0 / 5)) ? ((5.0 / 4) * x) : 1.0
      g = (4.0 / 5) * x
      b = (1.0 / 2) * x
      rgb = colormap_ref(Copper_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("copper %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = 0.0
      g = x
      b = 1.0 - g / 2.0
      rgb = colormap_ref(Winter_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("winter %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = 1.0
      g = x
      b = 0.0
      rgb = colormap_ref(Autumn_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("autumn %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = x
      g = 1.0 - r
      b = 1.0
      rgb = colormap_ref(Cool_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("cool %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = (x < (3.0 / 8)) ? ((8.0 / 3) * x) : 1.0
      g = (x < (3.0 / 8)) ? 0.0 : ((x < (3.0 / 4)) ? ((8.0 / 3) * x - 1.0) : 1.0)
      b = (x < (3.0 / 4)) ? 0.0 : (4.0 * x - 3)
      rgb = colormap_ref(Hot_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("hot %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = (x < (3.0 / 8)) ? 0.0 : ((x < (5.0 / 8)) ? (4.0 * x - 3.0 / 2) :
                                     ((x < (7.0 / 8)) ? 1.0 : (-4.0 * x + 9.0 / 2)))
      g = (x < (1.0 / 8)) ? 0.0 : ((x < (3.0 / 8)) ? (4.0 * x - 0.5) :
                                     (((x < (5.0 / 8)) ? 1.0 :
                                         ((x < (7.0 / 8)) ? (-4.0 * x + 7.0 / 2) : 0.0))))
      b = (x < (1.0 / 8)) ? (4.0 * x + 0.5) : ((x < (3.0 / 8)) ? 1.0 :
                                                 ((x < (5.0 / 8)) ? (-4.0 * x + 5.0 / 2) : 0.0))
      rgb = colormap_ref(Jet_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("jet %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = (x < (3.0 / 8)) ? ((14.0 / 9) * x) : ((2.0 / 3) * x + 1.0 / 3)
      g = (x < (3.0 / 8)) ? ((2.0 / 3) * x) :
        ((x < (3.0 / 4)) ? ((14.0 / 9) * x - 1.0 / 3) : ((2.0 / 3) * x + 1.0 / 3))
      b = (x < (3.0 / 4)) ? ((2.0 / 3) * x) : (2.0 * x - 1.0)
      rgb = colormap_ref(Pink_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("pink %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = 1.0
      g = x
      b = 1.0 - g
      rgb = colormap_ref(Spring_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("spring %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = x
      g = x
      b = x
      rgb = colormap_ref(Gray_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("gray %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = 0.0
      g = 0.0
      b = 0.0
      rgb = colormap_ref(Black_and_white_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("black_and_white %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = x
      g = 0.5 + r / 2.0
      b = 0.4
      rgb = colormap_ref(Summer_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("summer %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      r = (x < (2.0 / 5)) ? 1.0 : ((x < (3.0 / 5)) ? (-5.0 * x + 3) :
                                     ((x < (4.0 / 5)) ? 0.0 : (10.0 / 3 * x - 8.0 / 3)))
      g = (x < (2.0 / 5)) ? ((5.0 / 2) * x) : ((x < (3.0 / 5)) ? 1.0 :
                                                 ((x < (4.0 / 5)) ? (-5.0 * x + 4) : 0.0))
      b = (x < (3.0 / 5)) ? 0.0 : ((x < (4.0 / 5)) ? (5.0 * x - 3) : 1.0)
      rgb = colormap_ref(Rainbow_colormap, x)
      r1, g1, b1 = rgb
      if x < 1.0 - 1.0 / n and (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("rainbow %.3f (%.3f): %s %s",
                    x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
    10.times do |i|
      x = random(1.0)
      rgb = colormap_ref(Prism_colormap, x)
      if x < 1.0 - 1.0 / n and
          (not vequal(rgb, [1.0, 0.0, 0.0])) and
          (not vequal(rgb, [1.0, 0.5, 0.0])) and
          (not vequal(rgb, [1.0, 1.0, 0.0])) and
          (not vequal(rgb, [0.0, 1.0, 0.0])) and
          (not vequal(rgb, [0.0, 0.0, 1.0])) and
          (not vequal(rgb, [0.6667, 0.0, 1.0]))
        snd_display("prism %s", rgb)
      end
    end
    10.times do |i|
      x = random(1.0)
      rgb = colormap_ref(Flag_colormap, x)
      if x < 1.0 - 1.0 / n and
          (not vequal(rgb, [1.0, 0.0, 0.0])) and
          (not vequal(rgb, [1.0, 1.0, 1.0])) and
          (not vequal(rgb, [0.0, 0.0, 1.0])) and
          (not vequal(rgb, [0.0, 0.0, 0.0]))
        snd_display("flag %s", rgb)
      end
    end
  end
end

def test027
  ind = add_colormap("white", lambda do |size|
                       [make_vct(size, 1.0), make_vct(size,1.0), make_vct(size,1.0)]
                     end)
  unless res = colormap?(ind)
    snd_display("add_colormap %s: %s?", ind, res)
  end
  unless vequal(res = colormap_ref(ind, 0.5), [1.0, 1.0, 1.0])
    snd_display("white colormap: %s?", res)
  end
  if (res = snd_catch do set_colormap(ind) end).first == :no_such_colormap or colormap != ind
    snd_display("colormap white: %s %s %s", res, ind, colormap)
  end
  if (res = colormap_name(ind)) != "white"
    snd_display("white colormap name: %s?", res)
  end
  if (res = snd_catch do delete_colormap(1234) end).first != :no_such_colormap
    snd_display("delete_colormap 1234: %s?", res)
  end
  if (res = snd_catch do colormap_ref(1234, 0.5) end).first != :no_such_colormap
    snd_display("colormap_ref 1234: %s?", res)
  end
  if (res = snd_catch do colormap_ref(-1, 0.5) end).first != :no_such_colormap
    snd_display("colormap_ref -1: %s?", res)
  end
  if (res = snd_catch do set_colormap(1234) end).first != :no_such_colormap
    snd_display("set_colormap 1234: %s?", res)
  end
  if (res = snd_catch do set_colormap(-1) end).first != :no_such_colormap
    snd_display("set_colormap -1: %s?", res)
  end
  if (res = snd_catch do colormap_ref(Copper_colormap, 2.0) end).first != :out_of_range
    snd_display("colormap_ref 2.0: %s?", res)
  end
  #
  set_colormap_size($old_colormap_size)
  if (res = colormap_size) != $old_colormap_size
    snd_display("set_colormap_size: %d %d?", res, $old_colormap_size)
  end
  if (res = colormap_name(Black_and_white_colormap)) != "black-and-white"
    snd_display("black-and-white: %s?", res)
  end
  if (res = colormap_name(Gray_colormap)) != "gray"
    snd_display("gray: %s?", res)
  end
  if (res = colormap_name(Rainbow_colormap)) != "rainbow"
    snd_display("rainbow: %s?", res)
  end
  purple_cmap = add_colormap("purple",
                             lambda do |size|
                               r = make_vct(size)
                               g = make_vct(size)
                               b = make_vct(size)
                               er = [0, 60, 60, 116, 128, 252, 192, 252, 256, 60]
                               eg = [0,  0, 64,   0, 128, 252, 192, 252, 256,  0]
                               eb = [0, 80,          128, 252, 192,   0, 256, 80]
                               incr = 256.0 / size
                               x = 0.0
                               size.times do |i|
                                 r[i] = envelope_interp(x, er) / 256.0
                                 g[i] = envelope_interp(x, eg) / 256.0
                                 b[i] = envelope_interp(x, eb) / 256.0
                                 x += incr
                               end
                               [r, g, b]
                             end)
  sin_cmap = add_colormap("sin",
                          lambda do |size|
                            r = make_vct(size)
                            g = make_vct(size)
                            b = make_vct(size)
                            incr = (2.0 * PI) / size
                            x = 0.0
                            size.times do |i|
                              r[i] = sin(1.5 * x).abs
                              g[i] = sin(3.5 * x).abs
                              b[i] = sin(2.5 * x).abs
                              x += incr
                            end
                            [r, g, b]
                          end)
  another_sin_cmap = add_colormap("another-sin",
                                  lambda do |size|
                                    r = make_vct(size)
                                    g = make_vct(size)
                                    b = make_vct(size)
                                    incr = (2.0 * PI) / size
                                    x = 0.0
                                    size.times do |i|
                                      r[i] = sin(2.5 * x).abs
                                      g[i] = sin(3.5 * x).abs
                                      b[i] = sin(4.5 * x).abs
                                      x += incr
                                    end
                                    [r, g, b]
                                  end)
  delete_colormap(Pink_colormap)
  if res = colormap?(Pink_colormap)
    snd_display("delete_colormap %s: %s?", Pink_colormap, res)
  end
  if (res = snd_catch do set_colormap(Pink_colormap) end).first != :no_such_colormap or
      colormap == Pink_colormap
    snd_display("delete pink colormap: %s %s %s?",
                res, Pink_colormap, colormap)
  end
  [1024, 256, 2, 512].each do |n|
    set_colormap_size(n)
    10.times do |i|
      x = random(1.0)
      r = (x < 4.0 / 5) ? ((5.0 / 4) * x) : 1.0
      g = (4.0 / 5) * x
      b = 0.5 * x
      rgb = colormap_ref(Copper_colormap, x)
      r1, g1, b1 = rgb
      err = 0.01
      if n > 2 and
          x < 1.0 - 1.0 / n and
          (fneq_err(r, r1, err) or fneq_err(g, g1, err) or fneq_err(b, b1, err))
        snd_display("copper size reset %d: %.3f (%.3f): %s %s",
                    n, x, [(r - r1).abs, (g - g1).abs, (b - b1).abs].max,
                    [r, g, b], [r1, g1, b1])
      end
    end
  end
  set_colormap_size(512)
end

def test07
  $old_colormap_size = colormap_size
  test007
  test017
  test027
end

if (not provided?(:snd_nogui)) and ($test07 and $full_test or $snd_test == 7)
  $before_test_hook.call(7)
  test07
  $after_test_hook.call(7)
end

# ---------------- test 08: clm ----------------

def jc_reverb_1(decay_dur, low_pass, volume, amp_env)
  allpass1 = make_all_pass(-0.7, 0.7, 1051)
  allpass2 = make_all_pass(-0.7, 0.7,  337)
  allpass3 = make_all_pass(-0.7, 0.7,  113)
  comb1 = make_comb(0.742, 4799)
  comb2 = make_comb(0.733, 4999)
  comb3 = make_comb(0.715, 5399)
  comb4 = make_comb(0.697, 5801)
  outdel = make_delay((0.013 * srate()).round)
  dur = decay_dur + frames() / srate()
  envA = (amp_env ? make_env(:envelope, amp_env, :scaler, volume, :duration, dur) : false)
  comb_sum_1 = comb_sum_2 = comb_sum = all_sums = delA = delB = 0.0
  map_chan(lambda do |inval|
             allpass_sum = all_pass(allpass3, all_pass(allpass2, all_pass(allpass1, inval)))
             comb_sum_2, comb_sum_1 = comb_sum_1, comb_sum
             comb_sum = (comb(comb1, allpass_sum) + comb(comb2, allpass_sum) +
                           comb(comb3, allpass_sum) + comb(comb4, allpass_sum))
             all_sums = if low_pass
                          0.25 * (comb_sum + comb_sum_2) + 0.5 * comb_sum_1
                        else
                          comb_sum
                        end
             inval + if envA
                       env(envA) * delay(outdel, all_sums)
                     else
                       volume * delay(outdel, all_sums)
                     end
           end, 0, (dur * srate()).to_i)
end

# scissor-tailed flycatcher
#
# mix a scissor-tailed flycatcher call into the current sound see
# bird.scm for lots more birds
def scissor(begin_time)
  bigbird(begin_time, 0.05, 1800, 1800, 0.2,
          [0, 0, 40, 1, 60, 1, 100, 0],           # scissor
          [0, 0, 25, 1, 75, 1, 100, 0],
          [1, 0.5, 2, 1, 3, 0.5, 4, 0.1, 5, 0.01])
end

# fm_violin
def fm_violin_1(start, dur, freq, amp, *args)
  fm_index              = get_args(args, :fm_index, 1.0)
  amp_env               = get_args(args, :amp_env, [0, 0, 25, 1, 75, 1, 100, 0])
  periodic_vibrato_rate = get_args(args, :periodic_vibrato_rate, 5.0)
  random_vibrato_rate   = get_args(args, :random_vibrato_rate, 16.0)
  periodic_vibrato_amp  = get_args(args, :periodic_vibrato_amp, 0.0025)
  random_vibrato_amp    = get_args(args, :random_vibrato_amp, 0.005)
  noise_amount          = get_args(args, :noise_amount, 0.0)
  noise_freq            = get_args(args, :noise_freq, 1000.0)
  ind_noise_freq        = get_args(args, :ind_noise_freq, 10.0)
  ind_noise_amount      = get_args(args, :ind_noise_amount, 0.0)
  amp_noise_freq        = get_args(args, :amp_noise_freq, 20.0)
  amp_noise_amount      = get_args(args, :amp_noise_amount, 0.0)
  gliss_env             = get_args(args, :gliss_env, [0, 0, 100, 0])
  gliss_amount          = get_args(args, :gliss_amount, 0.0)
  fm1_env               = get_args(args, :fm1_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0])
  fm2_env               = get_args(args, :fm2_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0])
  fm3_env               = get_args(args, :fm3_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0])
  fm1_rat               = get_args(args, :fm1_rat, 1.0)
  fm2_rat               = get_args(args, :fm2_rat, 3.0)
  fm3_rat               = get_args(args, :fm3_rat, 4.0)
  fm1_index             = get_args(args, :fm1_index, false)
  fm2_index             = get_args(args, :fm2_index, false)
  fm3_index             = get_args(args, :fm3_index, false)
  base                  = get_args(args, :base, 1.0)
  reverb_amount         = get_args(args, :reverb_amount, 0.01)
  degree                = get_args(args, :degree, false)
  degrees               = get_args(args, :degrees, false)
  distance              = get_args(args, :distance, 1.0)
  frq_scl = hz2radians(freq)
  modulate = fm_index.nonzero?
  maxdev = frq_scl * fm_index
  logfreq = log(freq)
  sqrtfreq = sqrt(freq)
  index1 = (fm1_index or [PI, maxdev * 5.0 / logfreq].min)
  index2 = (fm2_index or [PI, maxdev * 3.0 * (8.5 - logfreq) / (3.0 + freq * 0.001)].min)
  index3 = (fm3_index or [PI, maxdev * 4.0 / sqrtfreq].min)
  easy_case = (noise_amount.zero? and
                 fm1_env == fm2_env and 
                 fm1_env == fm3_env and 
                 fm1_rat == fm1_rat.floor and 
                 fm2_rat == fm2_rat.floor and 
                 fm3_rat == fm3_rat.floor)
  norm = ((easy_case and modulate and 1.0) or index1)
  carrier = make_oscil(:frequency, freq)
  fmosc1 = if modulate
             if easy_case
               make_polyshape(:frequency, fm1_rat * freq,
                              :coeffs, partials2polynomial([fm1_rat.to_i, index1,
                                                            (fm2_rat / fm1_rat).floor, index2,
                                                            (fm3_rat / fm1_rat).floor, index3]))
             else
               make_oscil(:frequency, fm1_rat * freq)
             end
           else
             false
           end
  fmosc2 = (modulate and (easy_case or make_oscil(:frequency, fm2_rat * freq)))
  fmosc3 = (modulate and (easy_case or make_oscil(:frequency, fm3_rat * freq)))
  ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur, :base, base)
  indf1 = (modulate and make_env(:envelope, fm1_env, :scaler, norm, :duration, dur))
  indf2 = (modulate and
             (easy_case or make_env(:envelope, fm2_env, :scaler, index2, :duration, dur)))
  indf3 = (modulate and
             (easy_case or make_env(:envelope, fm3_env, :scaler, index3, :duration, dur)))
  frqf = make_env(:envelope, gliss_env, :scaler, gliss_amount * frq_scl, :duration, dur)
  pervib = make_triangle_wave(periodic_vibrato_rate, periodic_vibrato_amp *  frq_scl)
  ranvib = make_rand_interp(random_vibrato_rate, random_vibrato_amp * frq_scl)
  fm_noi = (noise_amount.nonzero? and make_rand(noise_freq, PI * noise_amount))
  ind_noi = ((ind_noise_amount.nonzero? and ind_noise_freq.nonzero?) and 
               make_rand_interp(ind_noise_freq, ind_noise_amount))
  amp_noi = ((amp_noise_amount.nonzero? and amp_noise_freq.nonzero?) and
               make_rand_interp(amp_noise_freq, amp_noise_amount))
  fuzz = modulation = 0.0
  ind_fuzz = amp_fuzz = 1.0
  out_data = make_vct!(seconds2samples(dur)) do
    fuzz = rand(fm_noi) if noise_amount.nonzero?
    vib = env(frqf) + triangle_wave(pervib) + rand_interp(ranvib)
    ind_fuzz = 1.0 + rand_interp(ind_noi) if ind_noi
    amp_fuzz = 1.0 + rand_interp(amp_noi) if amp_noi
    if modulate
      modulation = if easy_case
                     env(indf1) * polyshape(fmosc1, 1.0, vib)
                   else
                     (env(indf1) * oscil(fmosc1, fm1_rat * vib + fuzz) +
                                  env(indf2) * oscil(fmosc2, fm2_rat * vib + fuzz) +
                                  env(indf3) * oscil(fmosc3, fm3_rat * vib + fuzz))
                   end
    end
    env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz * modulation)
  end
  beg = seconds2samples(start)
  loc = make_locsig(:channels, channels(false),
                    :degree, (degree or degrees or rbm_random(90.0)),
                    :reverb, reverb_amount,
                    :distance, distance)
  channels(false).times do |chn|
    mix_vct(vct_scale!(vct_copy(out_data), locsig_ref(loc, chn)),
            beg, false, chn, false, get_func_name)
  end
end

def fltit
  coeffs = vct(0.1, 0.2, 0.3, 0.4, 0.4, 0.3, 0.2, 0.1)
  flt = make_fir_filter(8, coeffs)
  es = make_array(8) do |i|
    if i == 5
      make_env(:envelope, [0, 0.4, 1, 1], :duration, 1.0)
    else
      make_env(:envelope, [0, coeffs[i], 1, 0], :end, 100)
    end
  end
  lambda do |x|
    val = fir_filter(flt, x)
    xcof = flt.xcoeffs
    es.each_with_index do |en, i| xcof[i] = env(en) end
    val
  end
end

def freq_sweep(dur)
  phase = 0.0
  freq = 0.0
  incr = PI / (dur * 1.05 * mus_srate())
  map_channel(lambda do |y|
                val = sin(phase)
                phase += freq
                freq += incr
                0.5 * val
              end)
end

def make_ssb_am_1(freq, order = 40)
  carrier_freq = freq.abs
  cos_carrier = make_oscil(carrier_freq, 0.5 * PI)
  sin_carrier = make_oscil(carrier_freq)
  dly = make_delay(order)
  hlb = make_hilbert_transform(order)
  lambda do |y, fm|
    ccos = oscil(cos_carrier, fm)
    csin = oscil(sin_carrier, fm)
    yh = hilbert_transform(hlb, y)
    yd = delay(dly, y)
    if freq > 0.0
      ccos * yd - csin * yh # shift up
    else
      ccos * yd + csin * yh # shift down
    end
  end
end

def ssb_am_1(gen, y, fm = 0.0)
  gen.call(y, fm)
end

def rough_spectrum(ind)
  rd = make_sample_reader(0, ind, 0)
  mx = 0.0
  spect = make_vct!(10) do
    sum = 0.0
    1000.times do
      val = rd.call
      sum += val * val
    end
    if sum > mx
      mx = sum
    end
    sum
  end
  if mx.nonzero?
    vct_scale!(spect, 1.0 / mx)
  else
    spect
  end
end

def print_and_check(gen, name, desc)
  snd_display("mus_name %s: %s?", name, gen.name) if gen.name != name
  #  xen->sample: #<Proc:0x084bdd14@/usr/home/mike/Project/Sndtest/snd-test-new.rb:4470>
  unless gen.name == "xen->sample"
    snd_display("mus_describe %s: %s?", gen.name, gen) if gen.to_s != desc
  end
  egen = gen
  snd_display("eql? %s: %s?", gen, egen) unless egen.eql?(gen)
end

def test_gen_equal(g0, g1, g2)
  # g0 = g1 at start != g2
  g3 = g0
  gad = make_frame(2)
  snd_display("let %s %s.eql? %s?", g0.name, g0, g3) unless g0.eql?(g3)
  snd_display("arg %s %s.eql? %s?", g0.name, g0, g1) unless g0.eql?(g1)
  snd_display("%s %s == %s?", g0.name, g0, g1) if g0 == g1
  snd_display("%s %s == %s?", g0.name, g0, g2) if g0 == g2
  snd_display("%s == frame %s %s?", g0.name, g0, gad) if g0 == gad
  g0.run
  g3.run
  g3.run
  snd_display("run let %s %s.eql? %s?", g0.name, g0, g3) unless g0.eql?(g3)
  snd_display("arg %s %s.eql? %s?", g0.name, g0, g1) if g0.eql?(g1)
  snd_display("run %s %s == %s?", g0.name, g0, g1) if g0 == g1
  snd_display("run %s %s != %s?", g0.name, g0, g2) if g0 == g2
end

def fm_test(gen)
  snd_display("%s not a gen?", gen) unless mus_generator?(gen)
  gen.frequency = 0.0
  gen.phase = 0.0
  gen.run(0.0)
  if fneq(res = gen.phase, 0.0)
    snd_display("%s phase(0): %s?", gen, res)
  end
  gen.run(1.0)
  if fneq(res = gen.phase, 1.0)
    snd_display("%s phase(1): %s?", gen, res)
  end
  gen.run(0.0)
  if fneq(res = gen.phase, 1.0)
    snd_display("%s phase(1, 0): %s?", gen, res)
  end
  gen.frequency = radians2hz(2.0)
  gen.run(0.0)
  if fneq(res = gen.phase, 3.0)
    snd_display("%s phase(1, 2): %s %s?", gen, res, gen.frequency)
  end
  gen.run(1.0)
  if fneq(res = gen.phase, 6.0)
    snd_display("%s phase(3, 2, 1): %s %s?", gen, res, gen.frequency)
  end
  10.times do gen.run(10.0) end
  if fneq(res = gen.phase, 26 + 100 - 2 * PI * 20)
    snd_display("%s phase (over): %s %s?", gen, res, gen.frequency)
  end
  gen.frequency = 0.0
  gen.phase = 0.0
  gen.run(1234567812345678)
  gen.run(-1234567812345678)
  gen.run(log0)
  gen.frequency = 0.0
  gen.phase = 0.0
  gen.run(-2.0)
  if fneq(res = gen.phase, -2.0) and fneq(res, 2 * PI - 2)
    snd_display("phase %s freq: %s?", res, gen.frequency)
  end
end

def frame_equal?(f1, f2)
  if f1 and f2 and (len = f1.length) == f2.length
    callcc do |ret|
      len.times do |chn|
        if fneq(frame_ref(f1, chn), frame_ref(f2, chn))
          ret.call(false)
        end
      end
      true
    end
  else
    false
  end
end

def make_random_frame(size)
  fr = make_frame(size)
  size.times do |chn| frame_set!(fr, chn, 1.0 - random(2.0)) end
  fr
end

def make_random_mixer(size)
  mx = make_mixer(size)
  size.times do |i|
    size.times do |j|
      mixer_set!(mx, i, j, 1.0 - random(2.0))
    end
  end
  mx
end

def mixer_copy(umx)
  size = umx.length
  mx = make_mixer(size)
  size.times do |i|
    size.times do |j|
      mixer_set!(mx, i, j, mixer_ref(umx, i, j))
    end
  end
  mx
end

def test008
  set_mus_srate(22050)
  samps = seconds2samples(1.0)
  secs = samples2seconds(22050)
  snd_display("seconds2samples: %s?", samps) if samps != 22050
  snd_display("samples2seconds: %s?", secs) if fneq(secs, 1.0)
  if mus_file_buffer_size != $default_file_buffer_size
    snd_display("mus_file_buffer_size: %d?", mus_file_buffer_size)
  end
  if (res = snd_catch do set_mus_file_buffer_size(false) end).first != :wrong_type_arg
    snd_display("mus_file_buffer_size bad size: %s?", res)
  end
  set_mus_file_buffer_size(128)
  if (res = mus_file_buffer_size) != 128
    snd_display("set_mus_file_buffer_size: %d?", res)
  end
  set_mus_file_buffer_size($default_file_buffer_size)
  if (res = mus_array_print_length) != 8
    snd_display("mus_array_print_length: %d?", res)
  end
  set_mus_array_print_length(32)
  if (res = mus_array_print_length) != 32
    snd_display("set_mus_array_print_length: %d?", res)
  end
  set_mus_array_print_length(8)
  snd_display("mus_srate: %f?", mus_srate) if fneq(mus_srate, 22050.0)
  if fneq(res = hz2radians(1.0), 2.84951704088598e-4)
    snd_display("hz2radians: %f?", res)
  end
  if fneq(res = radians2hz(2.84951704088598e-4), 1.0)
    snd_display("radians2hz: %f?", res)
  end
  if fneq(res = radians2degrees(1.0), 57.2957801818848)
    snd_display("radians2degrees: %f?", res)
  end
  if fneq(res = degrees2radians(57.2957801818848), 1.0)
    snd_display("degrees2radians: %f?", res)
  end
  if fneq(res = linear2db(0.25), -12.0411996841431)
    snd_display("linear2db: %f?", res)
  end
  if fneq(res = db2linear(-12.0411996841431), 0.25)
    snd_display("db2linear: %f?", res)
  end
  if fneq(hz2radians(1.0), in_hz(1.0))
    snd_display("in_hz: %f?", in_hz(1.0))
  end
  if fneq(res = ring_modulate(0.4, 0.5), 0.2)
    snd_display("ring_modulate: %f?", res)
  end
  if fneq(res = amplitude_modulate(1.0, 0.5, 0.4), 0.7)
    snd_display("amplitude_modulate: %f?", res)
  end
  if fneq(res = contrast_enhancement(0.1, 0.75), sin(0.1 * (PI / 2) + 0.75 * sin(0.1 * 2.0 * PI)))
    snd_display("contrast_enhancement: %f (0.562925306221587)", res)
  end
  #
  [
    [partials2polynomial([1, 1, 2, 1], Mus_chebyshev_first_kind),
     vct(-1.0, 1.0, 2.0)],
    [partials2polynomial([1, 1, 2, 1], Mus_chebyshev_second_kind),
     vct(1.0, 2.0, 0.0)],
    [partials2polynomial([1, 1, 2, 1, 3, 1, 5, 1], Mus_chebyshev_first_kind),
     vct(-1.0, 3.0, 2.0, -16.0, 0.0, 16.0)],
    [partials2polynomial([1, 1, 2, 1, 3, 1, 5, 1], Mus_chebyshev_second_kind),
     vct(1.0, 2.0, -8.0, 0.0, 16.0, 0.0)],
    [partials2polynomial([1, 1, 2, 0.5, 3, 0.1, 6, 0.01], Mus_chebyshev_first_kind),
      vct(-0.51, 0.7, 1.18, 0.4, -0.48, 0.0, 0.32)],
    [partials2polynomial([1, 1, 2, 0.5, 3, 0.1, 6, 0.01], Mus_chebyshev_second_kind),
      vct(0.9, 1.06, 0.4, -0.32, 0.0, 0.32, 0.0)],
    [partials2polynomial([1, 9, 2, 3, 3, 5, 4, 7, 5, 1]),
      vct(4.0, -1.0, -50.0, 0.0, 56.0, 16.0)],
    [partials2polynomial([7, 1]),
     vct(0.0, -7.0, 0.0, 56.0, 0.0, -112.0, 0.0, 64.0)],
    [partials2polynomial([7, 1], Mus_chebyshev_first_kind),
     vct(0.0, -7.0, 0.0, 56.0, 0.0, -112.0, 0.0, 64.0)],
    [partials2polynomial([7, 1], Mus_chebyshev_second_kind),
     vct(-1.0, 0.0, 24.0, 0.0, -80.0, 0.0, 64.0, 0.0)],
  ].each_with_index do |args, i|
    vals, orig = args
    snd_display("partials2polynomial[%d]: %s?", i + 1, vals) unless vequal(vals, orig)
  end
  # 
  # check phase-quadrature cancellations
  #
  cos_coeffs = partials2polynomial([1, 1, 2, 1], Mus_chebyshev_first_kind)
  sin_coeffs = partials2polynomial([1, 1, 2, 1], Mus_chebyshev_second_kind)
  incr = (2 * PI * 440.0) / 22050.0
  a = 0.0
  1100.times do
    x = cos(a)
    y = sin(a)
    cax = polynomial(cos_coeffs, x)
    sax = polynomial(sin_coeffs, x)
    upper = cos(2 * a) * cax - sin(2 * a) * y * sax
    lower = cos(2 * a) * cax + sin(2 * a) * y * sax
    upper2 = cos(a * 3) + cos(a * 4)
    lower2 = 1.0 + cos(a)
    if fneq(upper, upper2) or fneq(lower, lower2)
      snd_display("%f %f, %f %f?", upper, upper2, lower, lower2)
    end
    a += incr
  end
  #
  amps = list2vct([1.0])
  phases = list2vct([0.0])
  val = sine_bank(amps, phases)
  snd_display("sine_bank: %f 0.0?", val) if fneq(val, 0.0)
  vct_set!(phases, 0, PI / 2)
  val = sine_bank(amps, phases)
  snd_display("sine_bank: %f 1.0?", val) if fneq(val, 1.0)
  amps = list2vct([0.5, 0.25, 1.0])
  phases = list2vct([1.0, 0.5, 2.0])
  val = sine_bank(amps, phases)
  snd_display("sine_bank: %f 1.449?", val) if fneq(val, 1.44989)
  val = sine_bank(amps, phases, 3)
  snd_display("sine_bank (3): %f 1.449?", val) if fneq(val, 1.44989)
  val = sine_bank(amps, phases, 1)
  snd_display("sine_bank (1): %f 0.421?", val) if fneq(val, 0.4207)
  #
  amps = list2vct([1.0])
  oscs = make_array(1, false)
  oscs[0] = make_oscil(440.0)
  val = oscil_bank(amps, oscs, false)
  snd_display("oscil_bank: %f 0.0?", val) if fneq(val, 0.0)
  oscs[0].phase = PI / 2
  val = oscil_bank(amps, oscs)
  snd_display("oscil_bank: %f 1.0?", val) if fneq(val, 1.0)
  #
  rdat = make_vct(16)
  idat = make_vct(16)
  vdat = make_vct(16)
  vct_set!(rdat, 0, 1.0)
  vct_set!(vdat, 0, 1.0)
  v0 = spectrum(rdat, idat, make_fft_window(Rectangular_window, 16), 1)
  v1 = snd_spectrum(vdat, Rectangular_window, 16, true)
  8.times do |i|
    if fneq(vct_ref(v0, i), vct_ref(v1, i))
      snd_display("spectra not equal: %s %s?", v0, v1)
    end
  end
  v0 = spectrum(rdat, idat, make_fft_window(Rectangular_window, 17), 1)
  v1 = snd_spectrum(vdat, Rectangular_window, 16, true)
  8.times do |i|
    if fneq(vct_ref(v0, i), vct_ref(v1, i))
      snd_display("spectra not equal: %s %s?", v0, v1)
    end
  end
  if (res = snd_catch do spectrum(rdat, idat, false, -1) end).first != :out_of_range
    snd_display("spectrum bad type: %s", res.inspect)
  end
  #
  rdat = make_vct(16)
  idat = make_vct(16)
  xdat = make_vct(16)
  ydat = make_vct(16)
  rvec = make_array(16, 0.0)
  ivec = make_array(16, 0.0)
  vct_set!(rdat, 0, 1.0)
  vct_set!(idat, 1, 1.0)
  vct_set!(xdat, 0, 1.0)
  vct_set!(ydat, 1, 1.0)
  rvec[0] = 1.0
  ivec[1] = 1.0
  v0 = convolution(rdat, idat, 8)
  v1 = vct_convolve!(xdat, ydat)
  snd_display("vct convolution: %s", v0) if fneq(v0[0], 0.0) or fneq(v0[1], 1.0)
  snd_display("vct_convolve!: %s", v1) if fneq(v1[0], 0.0) or fneq(v1[1], 1.0)
  8.times do |i|
    snd_display("convolutions not equal: %s %s?", v0, v1) if fneq(v0[i], v1[i])
  end
  if (res = snd_catch do convolution(rdat, idat, -1) end).first != :out_of_range
    snd_display("convolution bad len: %s", res.inspect)
  end
  convolution(rdat, idat, 20)
  idat = make_vct(8)
  convolution(rdat, idat, 20)
  #
  rdat = make_vct(16)
  idat = make_vct(16)
  xdat = make_vct(16)
  ydat = make_vct(16)
  rdat[3] = 1.0
  xdat[3] = 1.0
  fft(rdat, idat, 1)
  mus_fft(xdat, ydat, 16, 1)
  snd_display("ffts: %s %s", rdat, xdat) if fneq(rdat[0], xdat[0])
  fft(rdat, idat, -1)
  mus_fft(xdat, ydat, 17, -1)
  16.times do |i|
    if (i == 3 and (fneq(rdat[i], 16.0) or fneq(xdat[i], 16.0))) or
        (i != 3 and (fneq(rdat[i], 0.0) or fneq(xdat[i], 0.0)))
      snd_display("fft real[%d]: %f %f?", i, rdat[i], xdat[i])
    end
    if fneq(idat[i], 0.0) or fneq(ydat[i], 0.0)
      snd_display("fft imag[%d]: %f %f?", i, idat[i], ydat[i])
    end
  end
  if (res = snd_catch do mus_fft(xdat, ydat, -1, 0) end).first != :out_of_range
    snd_display("mus_fft bad len: %s", res.inspect)
  end
  #
  rdat = make_vct(20)
  idat = make_vct(19)
  rdat[3] = 1.0
  mus_fft(rdat, idat)
  convolution(rdat, idat)
  spectrum(rdat, idat, false)
  #
  v0 = make_vct(10)
  v1 = make_vct(10)
  vct_fill!(v0, 1.0)
  multiply_arrays(v0, v1, 1)
  unless vequal(v0, vct(0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
    snd_display("multiply_arrays[0]: %s?", v0)
  end
  multiply_arrays(v0, v1, 100)
  snd_display("multiply_arrays[100]: %s?", v0) if fneq(vct_peak(v0), 0.0)
  vct_fill!(v0, 1.0)
  vct_fill!(v1, 0.5)
  multiply_arrays(v0, v1)
  snd_display("multiply_arrays: %f?", v0[0]) if fneq(v0[0], 0.5)
  if fneq(res = dot_product(v0, v1), 2.5)
    snd_display("dot_product: %f?", res)
  end
  if fneq(res = dot_product(v0, v1, 10), 2.5)
    snd_display("dot_product (10): %f?", res)
  end
  if fneq(res = dot_product(v0, v1, 3), 0.75)
    snd_display("dot_product (3): %f?", res)
  end
  clear_array(v0)
  snd_display("clear_array: %s?", v0) if fneq(v0[3], 0.0)
  vct_fill!(v0, 1.0)
  vct_fill!(v1, 0.5)
  if fneq((res = rectangular2polar(v0, v1))[0], 1.118)
    snd_display("rectangular2polar: %s?", res)
  end
  vct_fill!(v0, 1.0)
  vct_fill!(v1, 1.0)
  rectangular2polar(v0, v1)
  if fneq(v0[0], sqrt(2.0)) or fneq(v1[0], -atan2(1.0, 1.0))
    snd_display("rectangular2polar (%f %f): %f %f?", sqrt(2.0), -atan2(1.0, 1.0), v0[0], v1[0])
  end
  polar2rectangular(v0, v1)
  if fneq(v0[0], 1.0) or fneq(v1[0], 1.0)
    snd_display("polar2rectangular (1 1): %f %f?", v0[0], v1[0])
  end
  v0 = make_vct(1)
  v1 = make_vct(1)
  v = make_vct(1)
  val = 0.123
  vct_set!(v0, 0, 1.0)
  vct_set!(v1, 0, 1.0)
  vct_map!(v, lambda do | |
             rectangular2polar(v0, v1)
             val = vct_ref(v0, 0)
             polar2rectangular(v0, v1)
             vct_ref(v1, 0)
           end)
  snd_display("run r->p not inverted: %s?", v) if fneq(v[0], 1.0)
  snd_display("r->p: %f?", val) if fneq(val, sqrt(2.0))
  #
  ind = open_sound("oboe.snd")
  rl = channel2vct(1200, 512)
  im = make_vct(512)
  fft(rl, im, 512)
  rl_copy = vct_copy(rl)
  im_copy = vct_copy(im)
  rectangular2polar(rl, im)
  polar2rectangular(rl, im)
  512.times do |i|
    if fneq(rl[i], rl_copy[i]) or fneq(im[i], im_copy[i])
      snd_display("polar2rectangular[%d]: %f %f %f %f?", i, rl[i], rl_copy[i], im[i], im_copy[i])
    end
  end
  close_sound(ind)
  #
  if defined? edot_product                        # edot_product in dsp.rb
    vals = make_vct(1, 1.0)
    if fneq(res = edot_product(0.0, vals), 1.0)
      snd_display("edot 1.0: %s?", res)
    end
    vals[0] = 0.0
    if fneq(res = edot_product(0.0, vals), 0.0)
      snd_display("edot 0.0: %s?", res)
    end
    vals = make_array(1, 1.0)
    if fneq(res = edot_product(0.0, vals), 1.0)
      snd_display("edot 1.0: %s?", res)
    end
    vals[0] = Complex(0.0)
    if cneq(res = edot_product(0.0, vals), Complex(0.0))
      snd_display("edot i: %s?", res)
    end
    vals = make_vct(4, 1.0)
    v1 = edot_product(0.25 * TWO_PI, vals)
    v2 = exp(0.00 * TWO_PI) +
         exp(0.25 * TWO_PI) +
         exp(0.50 * TWO_PI) +
         exp(0.75 * TWO_PI)
    snd_display("edot 4 i: %s %s?", v1, v2) if fneq(v1, v2)
    vals = make_array(4) do |i| i + 1.0 end
    v1 = edot_product(0.25 * TWO_PI * Complex(0.0), vals)
    v2 = 1 * exp(0.00 * TWO_PI* Complex(0.0)) +
         2 * exp(0.25 * TWO_PI* Complex(0.0)) +
         3 * exp(0.50 * TWO_PI* Complex(0.0)) +
         4 * exp(0.75 * TWO_PI* Complex(0.0))
    snd_display("edot 4 -i: %s %s?", v1, v2) if cneq(v1, v2)
    vals.map! do |i| i + Complex(1.0) end
    v1 = edot_product(0.25 * TWO_PI * Complex(0.0, -1), vals)
    v2 = Complex(1.0) * exp(0.00 * TWO_PI* Complex(0.0, -1)) +
         Complex(2.0) * exp(0.25 * TWO_PI* Complex(0.0, -1)) +
         Complex(3.0) * exp(0.50 * TWO_PI* Complex(0.0, -1)) +
         Complex(4.0) * exp(0.75 * TWO_PI* Complex(0.0, -1))
    snd_display("edot 4 -i * i: %s %s?", v1, v2) if cneq(v1, v2)
  end
  #
  v0 = vct(1.0, 0.5, 0.1)
  if fneq(res0 = polynomial(v0, 0.0), 1.0) or
      fneq(res1 = polynomial(v0, 1.0), 1.6) or
      fneq(res2 = polynomial(v0, 2.0), 2.4)
    snd_display("polynomial: %f %f %f?", res0, res1, res2)
  end
  v0 = make_vct!(10) do |i| i end
  if fneq(res = array_interp(v0, 3.5), 3.5)
    snd_display("array_interp: %f?", res)
  end
  if fneq(res = array_interp(v0, 13.5), 3.5)
    snd_display("array_interp (13.5): %f?", res)
  end
  if fneq(res = array_interp(v0, -6.5), 3.5)
    snd_display("array_interp (-6.5): %f?", res)
  end
  if fneq(res = array_interp(v0, 103.6), 3.6)
    snd_display("array_interp (103.6): %f?", res)
  end
  if fneq(res = array_interp(v0, -106.6), 3.4)
    snd_display("array_interp (-106.6): %f?", res)
  end
  if fneq(res = array_interp(v0, -0.5), 4.5)
    snd_display("array_interp (-0.5): %f?", res)
  end
  if fneq(res = array_interp(v0, -0.9), 8.1)
    snd_display("array_interp (-0.9): %f?", res)
  end
  if fneq(res = array_interp(v0, -0.1), 0.9)
    snd_display("array_interp (-0.1): %f?", res)
  end
  if fneq(res = array_interp(v0, 9.1), 8.1)
    snd_display("array_interp (9.1): %f?", res)
  end
  if fneq(res = array_interp(v0, 9.9), 0.9)
    snd_display("array_interp (9.9): %f?", res)
  end
  if fneq(res = array_interp(v0, 10.1), 0.1)
    snd_display("array_interp (10.1): %f?", res)
  end
  if (res = snd_catch do array_interp(v0, 1, -10) end).first != :out_of_range
    snd_display("array_interp bad index: %s", res.inspect)
  end
  #
  v0 = make_vct!(10) do |i| i end
  if fneq(res = mus_interpolate(Mus_interp_linear, 1.5, v0), 1.5)
    snd_display("mus_interpolate linear: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_all_pass, 1.5, v0), 1.667)
    snd_display("mus_interpolate all-pass: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_none, 1.5, v0), 1.0)
    snd_display("mus_interpolate none: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_hermite, 1.5, v0), 1.5)
    snd_display("mus_interpolate hermite: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_bezier, 1.5, v0), 1.5)
    snd_display("mus_interpolate bezier: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_lagrange, 1.5, v0), 1.5)
    snd_display("mus_interpolate lagrange: %f?", res)
  end
  v0.map_with_index! do |val, i| sin(PI * (i / 5.0)) end
  if fneq(res = mus_interpolate(Mus_interp_linear, 1.5, v0), 0.7694)
    snd_display("mus_interpolate linear sin: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_all_pass, 1.5, v0), 0.9048)
    snd_display("mus_interpolate all-pass sin: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_none, 1.5, v0), 0.5877)
    snd_display("mus_interpolate none sin: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_hermite, 1.5, v0), 0.8061)
    snd_display("mus_interpolate hermite sin: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_bezier, 1.5, v0), 0.6959)
    snd_display("mus_interpolate bezier sin: %f?", res)
  end
  if fneq(res = mus_interpolate(Mus_interp_lagrange, 1.5, v0), 0.7975)
    snd_display("mus_interpolate lagrange sin: %f?", res)
  end
end

def test018
  gen = make_delay(3)
  gen2 = make_delay(3)
  gen1 = make_delay(4, :initial_contents, [1.0, 0.5, 0.25, 0.0])
  gen3 = make_delay(4, :initial_contents, vct(1.0, 0.5, 0.25, 0.0))
  print_and_check(gen, "delay", "delay: line[3, step]: [0.000 0.000 0.000]")
  v0 = make_vct!(10) do |i| delay(gen, i) end
  v1 = make_vct!(10) do |i| delay?(gen2) ? delay(gen2, i) : -1.0 end
  snd_display("map delay: %s %s?", v0, v1) unless vequal(v1, v0)
  snd_display("%s not a delay?", gen) unless delay?(gen)
  snd_display("delay length: %d?", gen.length) if gen.length != 3
  if fneq(v0[1], 0.0) or fneq(v0[4], 1.0) or fneq(v0[8], 5.0)
    snd_display("delay output: %s?", v0)
  end
  if fneq(delay(gen1), 1.0) or fneq(delay(gen1), 0.5) or
      fneq(delay(gen1), 0.25) or fneq(delay(gen1), 0.0) or fneq(delay(gen1), 0.0)
    snd_display("delay with list initial-contents confused")
  end
  if fneq(delay(gen3), 1.0) or fneq(delay(gen3), 0.5) or
      fneq(delay(gen3), 0.25) or fneq(delay(gen3), 0.0) or fneq(delay(gen3), 0.0)
    snd_display("delay with vct initial-contents confused")
  end
  if (res = snd_catch do make_delay(:size, false) end).first != :wrong_type_arg
    snd_display("make_delay bad size false: %s", res.inspect)
  end
  if (res = snd_catch do
        make_delay(3, :initial_element, make_oscil)
      end).first != :wrong_type_arg
    snd_display("make_delay bad initial element: %s", res.inspect)
  end
  if (res = snd_catch do make_delay(-3) end).first != :out_of_range
    snd_display("make_delay bad size: %s", res.inspect)
  end
  d1 = make_delay(3)
  d2 = make_delay(3)
  d3 = make_delay(4)
  delay(d1, 1.0)
  delay(d2, 1.0)
  delay(d3, 1.0)
  test_gen_equal(d1, d2, d3)
  test_gen_equal(make_delay(3, :initial_element, 1.0),
                 make_delay(3, :initial_element, 1.0),
                 make_delay(3, :initial_element, 0.5))
  test_gen_equal(make_delay(3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_delay(3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_delay(3, :initial_contents, [1.0, 1.0, 1.0]))
  gen = make_delay(5)
  delay(gen, 1.0)
  delay(gen, 0.0)
  delay(gen, 0.5)
  data = vct_copy(gen.data)
  vct_set!(gen.data, 0, 0.3)
  snd_display("delay data 0: %f?", gen.data[0]) if fneq(gen.data[0], 0.3)
  vct_set!(data, 0, 0.75)
  gen.data = data
  snd_display("delay set data 0: %f?", gen.data[0]) if fneq(gen.data[0], 0.75)
  delay(gen, 0.0)
  delay(gen, 0.0)
  if fneq(res = delay(gen, 0.0), 0.75)
    snd_display("set delay data: %f %s?", res, gen.data)
  end
  if res = make_oscil.data
    snd_display("mus_data osc: %s?", res)
  end
  #
  del = make_delay(5, :max_size, 8)
  delay(del, 1.0)
  4.times do delay(del, 0.0) end
  v0 = make_vct!(5) do delay(del, 0.0, 0.4) end
  snd_display("zdelay: %s?", v0) unless vequal(v0, vct(0.6, 0.4, 0.0, 0.0, 0.0))
  delay(del, 1.0)
  delay(del, 0.0, 0.4)
  if (res = del.to_s) != "delay: line[5,8, linear]: [0.000 0.000 0.000 1.000 0.000]"
    snd_display("describe zdelay: %s", res)
  end
  if (res = snd_catch do tap(make_oscil) end).first != :wrong_type_arg
    snd_display("tap of oscil: %s?", res.inspect)
  end
  #
  dly = make_delay(3)
  flt = make_one_zero(0.5, 0.4)
  v = make_vct(20)
  inval = 1.0
  vct_map!(v, lambda do | |
             res = delay(dly, inval + one_zero(flt, tap(dly)) * 0.6)
             inval = 0.0
             res
           end)
  unless vequal(v, vct(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.3, 0.24, 0.0, 0.09,
                       0.144, 0.058, 0.027, 0.065, 0.052, 0.022, 0.026, 0.031, 0.019, 0.013))
    snd_display("tap with low pass: %s?", v)
  end
  #
  dly = make_delay(3)
  v = make_vct(20)
  inval = 1.0
  vct_map!(v, lambda do | |
             res = delay(dly, inval + tap(dly))
             inval = 0.0
             res
           end)
  unless vequal(v, vct(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0,
                       0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0))
    snd_display("simple tap: %s?", v)
  end
  dly = make_delay(6)
  v = make_vct(20)
  inval = 1.0
  vct_map!(v, lambda do | |
             res = delay(dly, inval + tap(dly, -2.0))
             inval = 0.0
             res
           end)
  set_print_length([20, print_length].max)
  unless vequal(v, vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
                       1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    snd_display("tap back 2: %s?", v)
  end
  #
  dly = make_delay(3, :initial_element, 32.0)
  unless vct?(dly.data)
    snd_display("delay data not vct?")
  else
    if dly.data.length != 3
      snd_display("delay data len not 3: %d (%s)?", dly.data.length, dly.data)
    else
      if fneq(res = dly.data[1], 32.0)
        snd_display("delay [1] 32: %f?", res)
      end
    end
  end
  if (res = snd_catch do dly.length = -1 end).first != :out_of_range
    snd_display("len to -1 -> %s?", res.inspect)
  end
  if (res = snd_catch do dly.length = 0 end).first != :out_of_range
    snd_display("len to 0 -> %s?", res.inspect)
  end
  if (res = snd_catch do dly.length = 100 end).first != :out_of_range
    snd_display("len to 100 -> %s?", res.inspect)
  end
  if (res = snd_catch do dly.data[100] = 0.1 end).first != :out_of_range
    snd_display("data 100 to 0.1 -> %s?", res.inspect)
  end
  data = make_vct(32, 1.0)
  dly.data = data
  snd_display("set delay data not vct?") unless vct?(dly.data)
  snd_display("set delay [1] 1: %f?", dly.data[1]) if fneq(dly.data[1], 1.0)
  snd_display("set delay data len(32): %d?", dly.data.length) if dly.data.length != 32
  if (res = snd_catch do dly.length = 100 end).first != :out_of_range
    snd_display("set len to 100 -> %s", res.inspect)
  end
  if (res = snd_catch do dly.data[100] = 0.1 end).first != :out_of_range
    snd_display("set data 100 to 0.1 -> %s", res.inspect)
  end
  #
  d1 = make_delay(4)
  d2 = make_delay(4, :max_size, 5, :type, Mus_interp_linear)
  d3 = make_delay(4, :max_size, 5, :type, Mus_interp_all_pass)
  d4 = make_delay(4, :max_size, 5, :type, Mus_interp_none)
  d5 = make_delay(4, :max_size, 4, :type, Mus_interp_lagrange)
  d6 = make_delay(4, :max_size, 4, :type, Mus_interp_hermite)
  d7 = make_delay(4, :max_size, 4, :type, Mus_interp_linear)
  v1 = make_vct(20)
  v2 = make_vct(20)
  v3 = make_vct(20)
  v4 = make_vct(20)
  v5 = make_vct(20)
  v6 = make_vct(20)
  v7 = make_vct(20)
  [[d1, Mus_interp_none],
    [d2, Mus_interp_linear],
    [d3, Mus_interp_all_pass],
    [d4, Mus_interp_none],
    [d5, Mus_interp_lagrange],
    [d6, Mus_interp_hermite],
    [d7, Mus_interp_linear]].each_with_index do |args, i|
    dly, type = args
    snd_display("d%d interp type: %s?", i + 1, dly.interp_type) if dly.interp_type != type
  end
  [[v1, d1], [v2, d2], [v3, d3], [v4, d4], [v5, d5], [v6, d6], [v7, d7]].each do |v, d|
    v[0] = delay(d, 1.0)
    delay_tick(d, 0.0)
  end
  j = -0.2
  (1...20).each do |i|
    [[v1, d1], [v2, d2], [v3, d3], [v4, d4], [v5, d5], [v6, d6], [v7, d7]].each do |v, d|
      v[i] = tap(d, j)
    end
    j -= 0.2
  end
  set_print_length([20, print_length].max)
  if (not vequal(v1, vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                         1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0))) and
      (not vequal(v1, vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                          0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0)))
    snd_display("delay interp none (1): %s?", v1)
  end
  unless vequal(v2, vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.4, 0.6, 0.8,
                        1.0, 0.8, 0.6, 0.4, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0))
    snd_display("delay interp linear (2): %s?", v2)
  end
  unless vequal(v3, vct(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.429, 0.143, 0.095, 0.905,
                        0.397, 0.83, 0.793, 0.912, -0.912, 0.608, -0.261, 0.065, -0.007))
    snd_display("delay interp all-pass (3): %s?", v3)
  end
  if (not vequal(v4, vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                         1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0))) and
      (not vequal(v4, vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                          0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0)))
    snd_display("delay interp none (4): %s?", v4)
  end
  unless vequal(v5, vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.12, 0.28, 0.48, 0.72,
                        1.0, 0.96, 0.84, 0.64, 0.36, 0.0, -0.08, -0.12, -0.12, -0.08))
    snd_display("delay interp lagrange (5): %s?", v5)
  end
  unless vequal(v6, vct(0.0, -0.016, -0.048, -0.072, -0.064, 0.0, 0.168, 0.424, 0.696, 0.912,
                        1.0, 0.912, 0.696, 0.424, 0.168, 0.0, -0.064, -0.072, -0.048, -0.016))
    snd_display("delay interp hermite (6): %s?", v6)
  end
  unless vequal(v7, vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.4, 0.6, 0.8,
                        1.0, 0.8, 0.6, 0.4, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0))
    snd_display("delay interp linear (7): %s?", v7)
  end
  #
  dly = make_delay(:size, 2, :max_size, 3)
  impulse = 1.0
  data = make_vct!(5) do
    val = delay(dly, impulse, 0.4)
    impulse = 0.0
    val
  end
  unless vequal(data, vct(0.0, 0.0, 0.6, 0.4, 0.0))
    snd_display("delay size 2, max 3, off 0.4: %s", data)
  end
  dly = make_delay(:size, 2, :max_size, 3)
  impulse = 1.0
  data = make_vct!(5) do
    val = delay(dly, impulse, -0.4)
    impulse = 0.0
    val
  end
  unless vequal(data, vct(0.0, 0.4, 0.6, 0.0, 0.0))
    snd_display("delay size 2, max 3, off -0.4: %s", data)
  end
  # 
  dly = make_delay(:size, 1, :max_size, 2)
  impulse = 1.0
  data = make_vct!(5) do
    val = delay(dly, impulse, 0.4)
    impulse = 0.0
    val
  end
  unless vequal(data, vct(0.0, 0.6, 0.4, 0.0, 0.0))
    snd_display("delay size 1, max 2, off 0.4: %s", data)
  end
  dly = make_delay(:size, 1, :max_size, 2)
  impulse = 1.0
  data = make_vct!(5) do
    val = delay(dly, impulse, -0.4)
    impulse = 0.0
    val
  end
  unless vequal(data, vct(0.0, 0.6, 0.4, 0.0, 0.0))
    snd_display("delay size 1, max 2, off -0.4: %s", data)
  end
  # 
  dly = make_delay(:size, 0, :max_size, 1)
  impulse = 1.0
  data = make_vct!(5) do
    val = delay(dly, impulse, 0.4)
    impulse = 0.0
    val
  end
  unless vequal(data, vct(0.6, 0.0, 0.0, 0.0, 0.0))
    snd_display("delay size 0, max 1, off 0.4: %s", data)
  end
  dly = make_delay(:size, 0, :max_size, 1)
  if fneq(res = delay(dly, 0.0), 0.0)
    snd_display("initial delay 0 size val: %f?", res)
  end
  dly = make_delay(:size, 0, :max_size, 1)
  impulse = 1.0
  data = make_vct!(5) do
    val = delay(dly, impulse, -0.4)
    impulse = 0.0
    val
  end
  unless vequal(data, vct(1.4, 0.0, 0.0, 0.0, 0.0))
    snd_display("delay size 0, max 1, off -0.4: %s", data)
  end
  dly = make_delay(:size, 0, :max_size, 100)
  v = make_vct!(10) do |i| delay(dly, 0.5, i) end
  unless vequal(v, vct(0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
    snd_display("delay 0 -> 100: %s", v)
  end
  9.downto(0) do |i| v[i] = delay(dly, 0.5, i) end
  unless vequal(v, vct(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0))
    snd_display("delay 100 -> 0: %s", v)
  end
  dly.reset
  10.times do |i| v[i] = delay(dly, (i.odd? ? 1.0 : 0.0), i * 0.1) end
  unless vequal(v, vct(0.0, 0.9, 0.0, 0.7, 0.0, 0.5, 0.0, 0.3, 0.0, 0.1))
    snd_display("delay 0 -> 100 0.1: %s", v)
  end
  dly.reset
  10.times do |i| v[i] = delay(dly, (i.odd? ? 1.0 : 0.0), 1.0 + i * 0.1) end
  unless vequal(v, vct(0.0, 0.0, 0.8, 0.3, 0.6, 0.5, 0.4, 0.7, 0.2, 0.9))
    snd_display("delay 0 -> 100 1.1: %s", v)
  end
end

def test028
  gen = make_all_pass(0.4, 0.6, 3)
  gen1 = make_all_pass(0.4, 0.6, 3)
  print_and_check(gen,
                  "all-pass",
                  "all-pass: feedback: 0.400, feedforward: 0.600, line[3, step]:[0.000 0.000 0.000]")
  v0 = make_vct!(10) do all_pass(gen, 1.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | all_pass?(gen1) ? all_pass(gen1, 1.0) : -1.0 end)
  snd_display("map all-pass: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not all-pass?", gen) unless all_pass?(gen)
  snd_display("all-pass length: %d?", gen.length) if gen.length != 3
  snd_display("all-pass order: %d?", gen.order) if gen.order != 3
  snd_display("all-pass feedback: %f?", gen.feedback) if fneq(gen.feedback, 0.4)
  snd_display("all-pass feedforward: %f?", gen.feedforward) if fneq(gen.feedforward, 0.6)
  if fneq(v0[1], 0.6) or fneq(v0[4], 1.84) or fneq(v0[8], 2.336)
    snd_display("all-pass output: %s?", v0)
  end
  gen.feedback = 0.5
  snd_display("all-pass set_feedback: %f?", gen.feedback) if fneq(gen.feedback, 0.5)
  gen.feedforward = 0.5
  snd_display("all-pass set_feedforward: %f?", gen.feedforward) if fneq(gen.feedforward, 0.5)
  d1 = make_all_pass(0.7, 0.5, 3)
  d2 = make_all_pass(0.7, 0.5, 3)
  d3 = make_all_pass(0.7, 0.5, 4)
  all_pass(d1, 1.0)
  all_pass(d2, 1.0)
  all_pass(d3, 1.0)
  test_gen_equal(d1, d2, d3)
  test_gen_equal(make_all_pass(0.7, 0.5, 3, :initial_element, 1.0),
                 make_all_pass(0.7, 0.5, 3, :initial_element, 1.0),
                 make_all_pass(0.7, 0.5, 3, :initial_element, 0.5))
  test_gen_equal(make_all_pass(0.7, 0.5, 3, :initial_element, 1.0),
                 make_all_pass(0.7, 0.5, 3, :initial_element, 1.0),
                 make_all_pass(0.5, 0.5, 3, :initial_element, 1.0))
  test_gen_equal(make_all_pass(0.7, 0.5, 3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_all_pass(0.7, 0.5, 3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_all_pass(0.7, 0.5, 3, :initial_contents, [1.0, 1.0, 1.0]))
  err = snd_catch do make_all_pass(:feedback, 0.2, :feedforward, 0.1, :size, -1) end
  if err.first != :out_of_range or
      err[1] != "make_all_pass" or
      err[2] != "size _1 < 0?" or
    snd_display("make_all_pass bad size error message: %s", err.inspect)
  end
  #
  gen = make_average(4)
  gen1 = make_average(4)
  print_and_check(gen, "average", "average: 0.000, line[4]:[0.000 0.000 0.000 0.000]")
  v0 = make_vct!(10) do average(gen, 1.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | average?(gen1) ? average(gen1, 1.0) : -1.0 end)
  snd_display("map average: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not average?", gen) unless average?(gen)
  snd_display("average length: %d?", gen.length) if gen.length != 4
  snd_display("average order: %d?", gen.order) if gen.order != 4
  if fneq(v0[1], 0.5) or fneq(v0[4], 1.0) or fneq(v0[8], 1.0)
    snd_display("average output: %s?", v0)
  end
  gen = make_average(8)
  if fneq(val = average(gen), 0.0)
    snd_display("empty average: %f?", val)
  end
  if fneq(val = average(gen, 1.0), 0.125)
    snd_display("average 1: %f?", val)
  end
  if fneq(val = average(gen, 1.0), 0.25)
    snd_display("average 2: %f?", val)
  end
  if fneq(val = average(gen, 0.5), 0.3125)
    snd_display("average 2: %f?", val)
  end
  4.times do average(gen, 0.0) end
  if fneq(val = average(gen, 0.0), 0.3125)
    snd_display("average 6: %f?", val)
  end
  if fneq(val = average(gen, 0.0), 0.1875)
    snd_display("average 7: %f?", val)
  end
  if fneq(val = average(gen, 0.0), 0.0625)
    snd_display("average 8: %f?", val)
  end
  if fneq(val = average(gen, 0.0), 0.0)
    snd_display("average 9: %f?", val)
  end
  gen = make_average(10, :initial_element, 0.5)
  if fneq(val = average(gen, 0.5), 0.5)
    snd_display("average initial_element: %f?", val)
  end
  gen = make_average(3, :initial_contents, [1.0, 1.0, 1.0])
  if fneq(val = average(gen, 1.0), 1.0)
    snd_display("average initial_contents: %f?", val)
  end
  d1 = make_average(3, :initial_contents, [0.7, 0.5, 3])
  d2 = make_average(3, :initial_contents, vct(0.7, 0.5, 3))
  d3 = make_average(4, :initial_contents, [0.7, 0.5, 0.1, 4])
  average(d1, 1.0)
  average(d2, 1.0)
  average(d3, 1.0)
  test_gen_equal(d1, d2, d3)
  test_gen_equal(make_average(3, :initial_element, 1.0),
                 make_average(3, :initial_element, 1.0),
                 make_average(3, :initial_element, 0.5))
  test_gen_equal(make_average(3, :initial_element, 1.0),
                 make_average(3, :initial_element, 1.0),
                 make_average(4, :initial_element, 1.0))
  test_gen_equal(make_average(3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_average(3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_average(3, :initial_contents, [1.0, 1.0, 1.0]))
  err = snd_catch do make_average(:size, -2) end
  if err.first != :out_of_range or
      err[1] != "make_average" or
      err[2] != "size _2 < 0?" or
    snd_display("make_average bad size error message: %s", err.inspect)
  end
  #
  gen = make_comb(0.4, 3)
  gen1 = make_comb(0.4, 3)
  print_and_check(gen, "comb", "comb: scaler: 0.400, line[3, step]: [0.000 0.000 0.000]")
  v0 = make_vct!(10) do comb(gen, 1.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | comb?(gen1) ? comb(gen1, 1.0) : -1.0 end)
  snd_display("map comb: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not comb?", gen) unless comb?(gen)
  snd_display("comb length: %d?", gen.length) if gen.length != 3
  snd_display("comb order: %d?", gen.order) if gen.order != 3
  snd_display("comb feedback: %f?", gen.feedback) if fneq(gen.feedback, 0.4)
  if fneq(v0[1], 0.0) or fneq(v0[4], 1.0) or fneq(v0[8], 1.4)
    snd_display("comb output: %s?", v0)
  end
  d1 = make_comb(0.7, 3)
  d2 = make_comb(0.7, 3)
  d3 = make_comb(0.7, 4)
  comb(d1, 1.0)
  comb(d2, 1.0)
  comb(d3, 1.0)
  test_gen_equal(d1, d2, d3)
  test_gen_equal(make_comb(0.7, 3, :initial_element, 1.0),
                 make_comb(0.7, 3, :initial_element, 1.0),
                 make_comb(0.7, 3, :initial_element, 0.5))
  test_gen_equal(make_comb(0.7, 3, :initial_element, 1.0),
                 make_comb(0.7, 3, :initial_element, 1.0),
                 make_comb(0.5, 3, :initial_element, 1.0))
  test_gen_equal(make_comb(0.7, 3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_comb(0.7, 3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_comb(0.7, 3, :initial_contents, [1.0, 1.0, 1.0]))
  del = make_comb(0.0, 5, :max_size, 8)
  comb(del, 1.0)
  4.times do comb(del, 0.0) end
  v0 = make_vct!(5) do comb(del, 0.0, 0.4) end
  snd_display("zcomb: %s", v0) unless vequal(v0, vct(0.600, 0.400, 0.000, 0.000, 0.000))
  comb(del, 1.0)
  comb(del, 0.0, 0.4)
  if (res = del.to_s) != "comb: scaler: 0.000, line[5,8, linear]: [0.000 0.000 0.000 1.000 0.000]"
    snd_display("describe zcom: %s", res)
  end
  del.feedback = 1.0
  snd_display("comb feedback set: %f?", del.feedback) if fneq(del.feedback, 1.0)
  #
  gen = make_notch(0.4, 3)
  gen1 = make_notch(0.4, 3)
  print_and_check(gen, "notch", "notch: scaler: 0.400, line[3, step]: [0.000 0.000 0.000]")
  v0 = make_vct!(10) do notch(gen, 1.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | notch?(gen1) ? notch(gen1, 1.0) : -1.0 end)
  snd_display("map notch: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not notch?", gen) unless notch?(gen)
  snd_display("notch length: %d?", gen.length) if gen.length != 3
  snd_display("notch order: %d?", gen.order) if gen.order != 3
  snd_display("notch feedforward: %f?", gen.feedforward) if fneq(gen.feedforward, 0.4)
  if fneq(v0[1], 0.4) or fneq(v0[4], 1.4) or fneq(v0[8], 1.4)
    snd_display("notch output: %s?", v0)
  end
  d1 = make_notch(0.7, 3)
  d2 = make_notch(0.7, 3)
  d3 = make_notch(0.7, 4)
  notch(d1, 1.0)
  notch(d2, 1.0)
  notch(d3, 1.0)
  test_gen_equal(d1, d2, d3)
  test_gen_equal(make_notch(0.7, 3, :initial_element, 1.0),
                 make_notch(0.7, 3, :initial_element, 1.0),
                 make_notch(0.7, 3, :initial_element, 0.5))
  test_gen_equal(make_notch(0.7, 3, :initial_element, 1.0),
                 make_notch(0.7, 3, :initial_element, 1.0),
                 make_notch(0.5, 3, :initial_element, 1.0))
  test_gen_equal(make_notch(0.7, 3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_notch(0.7, 3, :initial_contents, [1.0, 0.0, 0.0]),
                 make_notch(0.7, 3, :initial_contents, [1.0, 1.0, 1.0]))
  # make sure all-pass is the same as comb/notch given the appropriate
  # feedback/forward settings
  [[make_comb(0.5, 5), vct(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.5)],
    [make_all_pass(0.5, 0.0, 5), vct(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.5)],
    [make_notch(0.5, 5), vct(0.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0)],
    [make_all_pass(0.0, 0.5, 5), vct(0.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0)]
  ].each do |gen, v1|
    v0 = make_vct!(11) do |i| gen.run(i.zero? ? 1.0 : 0.0) end
    snd_display("0 %s (0.5, 0.0, 5): %s", gen.name, v0) unless vequal(v0, v1)
  end
  # make sure all-pass is the same as zcomb/znotch given the
  # appropriate feedback/forward and "pm" settings
  [[make_comb(0.5, 5, :max_size, 20),
      vct(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.5)],
    [make_all_pass(0.5, 0.0, 5, :max_size, 20),
      vct(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.5)],
    [make_notch(0.5, 5, :max_size, 20),
      vct(0.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0)],
    [make_all_pass(0.0, 0.5, 5, :max_size, 20),
      vct(0.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0)]].each do |gen, v1|
    v0 = make_vct!(11) do |i| gen.run(i.zero? ? 1.0 : 0.0) end
    snd_display("1 %s (0.5, 0.0, 5): %s", gen.name, v0) unless vequal(v0, v1)
  end
  # now actually use the size difference
  [[make_comb(0.5, 5, :max_size, 20),
      vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 0.4, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.16, 0.36, 0.2, 0.04, 0.0, 0.0, 0.0)],
    [make_all_pass(0.5, 0.0, 5, :max_size, 20),
      vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 0.4, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.16, 0.36, 0.2, 0.04, 0.0, 0.0, 0.0)],
    [make_notch(0.5, 5, :max_size, 20),
      vct(0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 0.4, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)],
    [make_all_pass(0.0, 0.5, 5, :max_size, 20),
      vct(0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 0.4, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)]].each do |gen, v1|
    angle = -0.2
    v0 = make_vct!(20) do |i| gen.run((i.zero? ? 1.0 : 0.0), angle += 0.2) end
    snd_display("2 %s (0.5, 0.0, 5): %s", gen.name, v0) unless vequal(v0, v1)
  end
  [[make_comb(0.5, 5, :max_size, 20),
      vct(0.0, 0.0, 0.0, 0.0, 0.8, 0.0, 0.0, 0.16, 0.16, 0.0,
          0.08, 0.064, 0.016, 0.035, 0.013, 0.018, 0.007, 0.007, 0.003, 0.002)],
    [make_all_pass(0.5, 0.0, 5, :max_size, 20),
      vct(0.0, 0.0, 0.0, 0.0, 0.8, 0.0, 0.0, 0.16, 0.16, 0.0,
          0.08, 0.064, 0.016, 0.035, 0.013, 0.018, 0.007, 0.007, 0.003, 0.002)],
    [make_notch(0.5, 5, :max_size, 20),
      vct(0.5, 0.0, 0.0, 0.0, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)],
    [make_all_pass(0.0, 0.5, 5, :max_size, 20),
      vct(0.5, 0.0, 0.0, 0.0, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)]].each do |gen, v1|
    angle = +0.2
    v0 = make_vct!(20) do |i| gen.run((i.zero? ? 1.0 : 0.0), angle -= 0.2) end
    snd_display("3 %s (0.5, 0.0, 5): %s", gen.name, v0) unless vequal(v0, v1)
  end
  [[make_comb(0.5, 5, :max_size, 20),
      vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.95, 0.06, 0.0, 0.0, 0.0,
          0.428, 0.079, 0.004, 0.0, 0.0, 0.182, 0.067, 0.008, 0.0, 0.0)],
    [make_all_pass(0.5, 0.0, 5, :max_size, 20),
      vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.95, 0.06, 0.0, 0.0, 0.0,
          0.428, 0.079, 0.004, 0.0, 0.0, 0.182, 0.067, 0.008, 0.0, 0.0)],
    [make_notch(0.5, 5, :max_size, 20),
      vct(0.5, 0.0, 0.0, 0.0, 0.0, 0.95, 0.06, 0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)],
    [make_all_pass(0.0, 0.5, 5, :max_size, 20),
      vct(0.5, 0.0, 0.0, 0.0, 0.0, 0.95, 0.06, 0.0, 0.0, 0.0, 0.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)]].each do |gen, v1|
    angle = -0.01
    v0 = make_vct!(20) do |i| gen.run((i.zero? ? 1.0 : 0.0), angle += 0.01) end
    snd_display("4 %s (0.5, 0.0, 5): %s", gen.name, v0) unless vequal(v0, v1)
  end
  # now run off either end of the delay line "by accident"
  [[make_comb(0.5, 5, :max_size, 10),
      vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5,
          1.0, 0.25, 0.125, 0.094, 0.062, 0.055, 0.047, 0.039, 0.031, 0.029)],
    [make_all_pass(0.5, 0.0, 5, :max_size, 10),
      vct(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5,
          1.0, 0.25, 0.125, 0.094, 0.062, 0.055, 0.047, 0.039, 0.031, 0.029)],
    [make_notch(0.5, 5, :max_size, 10),
      vct(0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 1.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)],
    [make_all_pass(0.0, 0.5, 5, :max_size, 10),
      vct(0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 1.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)]].each do |gen, v1|
    angle = -0.5
    v0 = make_vct!(20) do |i| gen.run((i.zero? ? 1.0 : 0.0), angle += 0.5) end
    snd_display("5 %s (0.5, 0.0, 5): %s", gen.name, v0) unless vequal(v0, v1)
  end
  [[make_comb(0.5, 5, :max_size, 10),
      vct(0.0, 0.0, 0.0, 0.5, 0.0, 0.125, 0.0, 0.031, 0.016, 0.004,
          1.0, 0.0, 0.25, 0.031, 0.0, 0.012, 0.002, 0.250, 0.125, 0.008)],
    [make_all_pass(0.5, 0.0, 5, :max_size, 10),
      vct(0.0, 0.0, 0.0, 0.5, 0.0, 0.125, 0.0, 0.031, 0.016, 0.004,
          1.0, 0.0, 0.25, 0.031, 0.0, 0.012, 0.002, 0.250, 0.125, 0.008)],
    [make_notch(0.5, 5, :max_size, 10),
      vct(0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)],
    [make_all_pass(0.0, 0.5, 5, :max_size, 10),
      vct(0.5, 0.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)]].each do |gen, v1|
    angle = +0.5
    v0 = make_vct!(20) do |i| gen.run((i.zero? ? 1.0 : 0.0), angle -= 0.5) end
    snd_display("6 %s (0.5, 0.0, 5): %s", gen.name, v0) unless vequal(v0, v1)
  end
end

def test038
  gen = make_one_pole(0.4, 0.7)
  gen1 = make_one_pole(0.4, 0.7)
  print_and_check(gen, "one-pole", "one-pole: a0: 0.400, b1: 0.700, y1: 0.000")
  v0 = make_vct!(10) do one_pole(gen, 1.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | one_pole?(gen1) ? one_pole(gen1, 1.0) : -1.0 end)
  snd_display("map one_pole: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not one_pole?", gen) unless one_pole?(gen)
  snd_display("one_pole order: %d?", gen.order) if gen.order != 1
  snd_display("one_pole a0: %f?", gen.a0) if fneq(gen.a0, 0.4)
  snd_display("one_pole b1: %f?", gen.b1) if fneq(gen.b1, 0.7)
  if fneq(v0[1], 0.12) or fneq(v0[4], 0.275) or fneq(v0[8], 0.245)
    snd_display("one_pole output: %s?", v0)
  end
  snd_display("1p ycoeff 1 0.7: %s?", gen) if fneq(gen.ycoeff(1), 0.7)
  gen.ycoeff = 1, 0.1
  snd_display("1p set_ycoeff 1 0.1: %s?", gen) if fneq(gen.ycoeff(1), 0.1)
  snd_display("1p xcoeff 0 0.4: %s?", gen) if fneq(gen.xcoeff(0), 0.4)
  gen.xcoeff = 0, 0.3
  snd_display("1p set_xcoeff 0 0.3: %s?", gen) if fneq(gen.xcoeff(0), 0.3)
  # 
  gen = make_one_zero(0.4, 0.7)
  gen1 = make_one_zero(0.4, 0.7)
  print_and_check(gen, "one-zero", "one-zero: a0: 0.400, a1: 0.700, x1: 0.000")
  v0 = make_vct!(10) do one_zero(gen, 1.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | one_zero?(gen1) ? one_zero(gen1, 1.0) : -1.0 end)
  snd_display("map one_zero: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not one_zero?", gen) unless one_zero?(gen)
  snd_display("one_zero order: %d?", gen.order) if gen.order != 1
  snd_display("one_zero a0: %f?", gen.a0) if fneq(gen.a0, 0.4)
  snd_display("one_zero a1: %f?", gen.a1) if fneq(gen.a1, 0.7)
  if fneq(v0[1], 1.1)
    snd_display("one_zero output: %s?", v0)
  end
  snd_display("1z xcoeff 0 0.4: %s?", gen) if fneq(gen.xcoeff(0), 0.4)
  gen.xcoeff = 0, 0.1
  snd_display("1z set_xcoeff 0 0.1: %s?", gen) if fneq(gen.xcoeff(0), 0.1)
  # 
  gen = make_two_zero(0.4, 0.7, 0.3)
  gen1 = make_two_zero(0.4, 0.7, 0.3)
  print_and_check(gen,
                  "two-zero",
                  "two-zero: a0: 0.400, a1: 0.700, a2: 0.300, x1: 0.000, x2: 0.000")
  v0 = make_vct!(10) do two_zero(gen, 1.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | two_zero?(gen1) ? two_zero(gen1, 1.0) : -1.0 end)
  snd_display("map two_zero: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not two_zero?", gen) unless two_zero?(gen)
  snd_display("two_zero order: %d?", gen.order) if gen.order != 2
  snd_display("two_zero a0: %f?", gen.a0) if fneq(gen.a0, 0.4)
  snd_display("two_zero a1: %f?", gen.a1) if fneq(gen.a1, 0.7)
  snd_display("two_zero a2: %f?", gen.a2) if fneq(gen.a2, 0.3)
  if fneq(v0[1], 1.1) or fneq(v0[8], 1.4)
    snd_display("two_zero output: %s?", v0)
  end
  snd_display("2z xcoeff 0 0.4: %s?", gen) if fneq(gen.xcoeff(0), 0.4)
  gen.xcoeff = 0, 0.1
  snd_display("2z set_xcoeff 0 0.1: %s?", gen) if fneq(gen.xcoeff(0), 0.1)
  gen = make_two_zero(0.4, 0.7, 0.3)
  if fneq(val = gen.call(1.0, 0.0), 0.4)
    snd_display("2zero->0.4: %f?", val)
  end
  if fneq(val = gen.call(0.5, 0.0), 0.9)
    snd_display("2zero->0.9: %f?", val)
  end
  if fneq(val = gen.call(1.0, 0.0), 1.05)
    snd_display("2zero->1.05: %f?", val)
  end
  # 
  gen = make_two_pole(0.4, 0.7, 0.3)
  gen1 = make_two_pole(0.4, 0.7, 0.3)
  print_and_check(gen,
                  "two-pole",
                  "two-pole: a0: 0.400, b1: 0.700, b2: 0.300, y1: 0.000, y2: 0.000")
  v0 = make_vct!(10) do two_pole(gen, 1.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | two_pole?(gen1) ? two_pole(gen1, 1.0) : -1.0 end)
  snd_display("map two_pole: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not two_pole?", gen) unless two_pole?(gen)
  snd_display("two_pole order: %d?", gen.order) if gen.order != 2
  snd_display("two_pole a0: %f?", gen.a0) if fneq(gen.a0, 0.4)
  snd_display("two_pole b1: %f?", gen.b1) if fneq(gen.b1, 0.7)
  snd_display("two_pole b2: %f?", gen.b2) if fneq(gen.b2, 0.3)
  if fneq(v0[1], 0.12) or fneq(v0[8], 0.201)
    snd_display("two_pole output: %s?", v0)
  end
  snd_display("2p ycoeff 1 0.7: %s?", gen) if fneq(gen.ycoeff(1), 0.7)
  gen.ycoeff = 1, 0.1
  snd_display("2p set_ycoeff 1 0.1: %s?", gen) if fneq(gen.ycoeff(1), 0.1)
  snd_display("2p xcoeff 0 0.4: %s?", gen) if fneq(gen.xcoeff(0), 0.4)
  gen.xcoeff = 0, 0.3
  snd_display("2p set_xcoeff 0 0.3: %s?", gen) if fneq(gen.xcoeff(0), 0.3)
  gen = make_two_pole(0.4, 0.7, 0.3)
  if fneq(val = gen.call(1.0, 0.0), 0.4)
    snd_display("a0->out 2pole: %f?", val)
  end
  if fneq(val = gen.call(0.5, 0.0), -0.08)
    snd_display("a0->out 2pole (-0.08): %f?", val)
  end
  if fneq(val = gen.call(1.0, 0.0), 0.336)
    snd_display("a0->out 2pole (0.336): %f?", val)
  end
  if (res = snd_catch do make_two_pole(:b1, 3.0) end).first != :mus_error
    snd_display("make_two_pole bad b1: %s", res.inspect)
  end
  if (res = snd_catch do make_two_pole(:b2, 2.0) end).first != :mus_error
    snd_display("make_two_pole bad b2: %s", res.inspect)
  end
  if (res = snd_catch do make_two_pole(:b2, 2.0, :b1) end).first != :mus_error
    snd_display("make_two_pole bad keys: %s", res.inspect)
  end
  if (res = snd_catch do make_two_pole(:b2, 2.0, 3.0) end).first != :mus_error
    snd_display("make_two_pole bad args: %s", res.inspect)
  end
  #
  gen = make_oscil(440.0)
  gen1 = make_oscil(440.0)
  gen2 = make_oscil(440.0)
  print_and_check(gen, "oscil", "oscil freq: 440.000Hz, phase: 0.000")
  v0 = make_vct!(10) do oscil(gen, 0.0) end
  v1 = make_vct!(10) do mus_apply(gen1, 0.0, 0.0) end
  v2 = make_vct(10)
  vct_map!(v2, lambda do | | oscil?(gen2) ? oscil(gen2, 0.0) : -1.0 end)
  snd_display("map oscil: %s %s?", v0, v2) unless vequal(v0, v2)
  snd_display("%s not oscil?", gen) unless oscil?(gen)
  snd_display("oscil phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  snd_display("oscil frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  snd_display("oscil cosines: %d?", gen.cosines) if fneq(gen.cosines, 1)
  if fneq(v0[1], 0.125) or fneq(v0[8], 0.843)
    snd_display("oscil output: %s?", v0)
  end
  gen.phase = 0.0
  snd_display("oscil set_phase: %f?", gen.phase) if fneq(gen.phase, 0.0)
  gen.frequency = 100.0
  snd_display("oscil set_frequency: %f?", gen.frequency) if fneq(gen.frequency, 100.0)
  #
  v0.each_with_index do |val, i|
    if fneq(val, v1[i])
      snd_display("mus_apply oscil at %d: %f %f?", i, val, v1[i])
    end
  end
  if fneq(mus_apply(), 0.0)
    snd_display("mus_apply(): %s?", mus_apply())
  end
  gen1 = make_oscil(100.0)
  gen2 = make_oscil(-100.0)
  mx = 0.0
  100.times do mx = [mx, (gen1.run + gen2.run).abs].max end
  if fneq(mx, 0.0)
    snd_display("1 oscil +-: %f?", mx)
  end
  gen1 = make_oscil(100.0, PI * 0.5)
  gen2 = make_oscil(-100.0, PI * 0.5)
  mx = 0.0
  100.times do mx = [mx, (gen1.run - gen2.run).abs].max end
  if fneq(mx, 0.0)
    snd_display("2 oscil +-: %f?", mx)
  end
  fm_test(make_oscil)
  fm_test(make_sine_summation)
  fm_test(make_square_wave)
  fm_test(make_triangle_wave)
  fm_test(make_sum_of_cosines)
  fm_test(make_sum_of_sines)
  fm_test(make_sawtooth_wave)
  fm_test(make_rand)
  fm_test(make_rand_interp)
  fm_test(make_pulse_train)
  #
  gen = make_oscil(440.0)
  gen1 = make_oscil(440.0)
  10.times do
    if fneq(oval = oscil(gen, 0.1), mval = mus_run(gen1, 0.1))
      snd_display("mus_run %f but oscil %f?", oval, mval)
    end
  end
  gen = make_oscil(440.0)
  gen1 = make_oscil(440.0)
  gen2 = make_oscil(440.0)
  gen3 = make_oscil(440.0)
  fm_index = hz2radians(440.0)
  v0 = make_vct(10)
  v1 = make_vct(10)
  10.times do |i|
    v0[i] = oscil(gen, fm_index * oscil(gen1, 0.0))
    v1[i] = mus_apply(gen2, fm_index * mus_apply(gen3, 0.0, 0.0), 0.0)
  end
  if fneq(v0[1], 0.125) or fneq(v0[6], 0.83) or fneq(v0[8], 0.987)
    snd_display("oscil fm output: %s?", v0)
  end
  v0.each_with_index do |val, i|
    if fneq(val, v1[i])
      snd_display("mus_apply fm oscil at %d: %f %f?", i, val, v1[i])
    end
  end
  test_gen_equal(make_oscil(440.0), make_oscil(440.0), make_oscil(100.0))
  test_gen_equal(make_oscil(440.0), make_oscil(440.0), make_oscil(440.0, 1.0))
  gen = make_oscil(440.0)
  gen1 = make_oscil(440.0)
  pm_index = 2.0
  v0 = make_vct!(10) do gen.call(0.0, pm_index * gen1.call(0.0, 0.0)) end
  if fneq(v0[1], 0.367) or fneq(v0[6], 0.854) or fneq(v0[8], 0.437)
    snd_display("oscil pm output: %s?", v0)
  end
  gen = make_oscil(440.0)
  1100.times do |i|
    if fneq(val1 = sin(gen.phase), val2 = gen.call(0.0, 0.0))
      snd_display("oscil (sin): %d: %f %f?", i, val1, val2)
    end
  end
  gen = make_oscil(440.0, :initial_phase, PI * 0.5)
  a = 0.0
  900.times do |i|
    if fneq(val1 = cos(a), val2 = gen.call(0.0, 0.0))
      snd_display("oscil (cos): %d: %f %f?", i, val1, val2)
    end
    a += (2 * PI * 440) / 22050
  end
  gen = make_oscil(0.0)
  gen1 = make_oscil(40.0)
  a = 0.0
  1100.times do |i|
    if fneq(val1 = sin(sin(a)), val2 = oscil(gen, 0.0, oscil(gen1, 0.0)))
      snd_display("oscil  pm: %d: %f %f?", i, val1, val2)
    end
    a += (2 * PI * 40) / 22050
  end
  gen = make_oscil(0.0)
  gen1 = make_oscil(40.0)
  a = 0.0
  a1 = 0.0
  1100.times do |i|
    fm = sin(a)
    if fneq(val1 = sin(a1), val2 = oscil(gen, oscil(gen1, 0.0)))
      snd_display("oscil  fm: %d: %f %f?", i, val1, val2)
    end
    a += (2 * PI * 40) / 22050
    a1 += fm
  end
  #
  if (res = snd_catch do mus_location(make_oscil) end).first != :mus_error
    snd_display("mus_location bad gen: %s", res.inspect)
  end
  if (res = snd_catch do set_mus_location(make_oscil, 0) end).first != :mus_error
    snd_display("set_mus_location bad gen: %s", res.inspect)
  end
  if (res = snd_catch do mus_scaler(make_oscil) end).first != :mus_error
    snd_display("mus_scaler bad gen: %s", res.inspect)
  end
  if (res = snd_catch do set_mus_scaler(make_oscil, 0) end).first != :mus_error
    snd_display("set_mus_scaler bad gen: %s", res.inspect)
  end
  if (res = snd_catch do mus_length(make_oscil) end).first != :mus_error
    snd_display("mus_length bad gen: %s", res.inspect)
  end
  if (tag = (res = snd_catch do set_mus_length(make_oscil, 0) end).first) !=
      :mus_error and tag != :out_of_range
    snd_display("set_mus_length bad gen: %s", res.inspect)
  end
  if (res = snd_catch do mus_frequency(make_one_pole) end).first != :mus_error
    snd_display("mus_frequency bad gen: %s", res.inspect)
  end
  if (res = snd_catch do set_mus_frequency(make_one_pole, 0) end).first != :mus_error
    snd_display("set_mus_frequency bad gen: %s", res.inspect)
  end
  if (res = snd_catch do mus_scaler(make_delay(3)) end).first != :mus_error
    snd_display("mus_scaler bad gen: %s", res.inspect)
  end
  if (res = snd_catch do set_mus_scaler(make_delay(3), 0) end).first != :mus_error
    snd_display("set_mus_scaler bad gen: %s", res.inspect)
  end
  if (res = snd_catch do make_delay(1024 * 1024 * 40) end).first != :out_of_range
    snd_display("make_delay huge line 1: %s", res.inspect)
  end
  if (res = snd_catch do make_delay(32, :max_size, 1024 * 1024 * 40) end).first != :out_of_range
    snd_display("make_delay huge line 2: %s", res.inspect)
  end
  #
  amps = make_array(3) do |i| (i + 1) * 0.1 end
  oscils = make_array(3) do |i| make_oscil(:frequency, (i + 1) * 220.0) end
  fms = make_array(3) do |i| i * 0.05 end
  results = make_array(10) do |i| oscil_bank(amps, oscils, fms) end
  if fneq(results[1], 0.12639) or fneq(results[5], 0.48203) or fneq(results[9], 0.41001)
    snd_display("oscil_bank: %s?", results)
  end
end

def test048
  gen = make_sum_of_cosines(10, 440.0)
  gen1 = make_sum_of_cosines(10, 440.0)
  print_and_check(gen,
                  "sum-of-cosines",
                  "sum-of-cosines freq: 440.000Hz, phase: 0.000, cosines: 10")
  v0 = make_vct!(10) do sum_of_cosines(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | sum_of_cosines?(gen1) ? sum_of_cosines(gen1, 0.0) : -1.0 end)
  snd_display("map sum_of_cosines: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not sum_of_cosines?", gen) unless sum_of_cosines?(gen)
  snd_display("sum_of_cosines phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  snd_display("sum_of_cosines frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  snd_display("sum_of_cosines scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.1)
  snd_display("sum_of_cosines cosines: %d?", gen.cosines) if fneq(gen.cosines, 10)
  snd_display("sum_of_cosines length: %d?", gen.length) if fneq(gen.length, 10)
  if fneq(v0[1], 0.722) or fneq(v0[8], -0.143)
    snd_display("sum_of_cosines output: %s?", v0)
  end
  gen.scaler = 0.5
  snd_display("sum_of_cosines set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  gen.cosines = 5
  snd_display("sum_of_cosines set_cosines: %d?", gen.cosines) if fneq(gen.cosines, 5)
  snd_display("sum_of_cosines set_cosines->scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.2)
  test_gen_equal(make_sum_of_cosines(3, 440),
                 make_sum_of_cosines(3, 440),
                 make_sum_of_cosines(5, 440))
  test_gen_equal(make_sum_of_cosines(3, 440),
                 make_sum_of_cosines(3, 440),
                 make_sum_of_cosines(5, 440, 1))
  test_gen_equal(make_sum_of_cosines(3, 440),
                 make_sum_of_cosines(3, 440),
                 make_sum_of_cosines(5, 400))
  gen = make_sum_of_cosines(10)
  1100.times do |i|
    den = sin(gen.phase * 0.5)
    val1 = den.zero? ? 1.0 : [1.0, gen.scaler * (sin(gen.phase * (gen.cosines + 0.5)) /
                                                   (2.0 * den) - 0.5)].min
    if (val1 - (val2 = gen.run(0.0))).abs > 0.002
      snd_display("sum_of_cosines: %d: %f %f?", i, val1, val2)
    end
  end
  gen1 = make_sum_of_cosines(10, 100.0)
  gen2 = make_sum_of_cosines(10, -100.0)
  mx = 0.0
  100.times do mx = [mx, (gen1.run - gen2.run).abs].max end
  if fneq(mx, 0.0)
    snd_display("sum_of_cosines +-: %f?", mx)
  end
  # 
  gen = make_sum_of_sines(10, 440.0)
  gen1 = make_sum_of_sines(10, 440.0)
  print_and_check(gen,
                  "sum-of-sines",
                  "sum-of-sines freq: 440.000Hz, phase: 0.000, sines: 10")
  v0 = make_vct!(10) do sum_of_sines(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | sum_of_sines?(gen1) ? sum_of_sines(gen1, 0.0) : -1.0 end)
  snd_display("map sum_of_sines: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not sum_of_sines?", gen) unless sum_of_sines?(gen)
  snd_display("sum_of_sines phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  snd_display("sum_of_sines frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  snd_display("sum_of_sines scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.1315)
  snd_display("sum_of_sines sines: %d?", gen.cosines) if fneq(gen.cosines, 10)
  snd_display("sum_of_sines length: %d?", gen.length) if fneq(gen.length, 10)
  if fneq(v0[1], 0.784) or fneq(v0[8], 0.181)
    snd_display("sum_of_sines output: %s?", v0)
  end
  gen.scaler = 0.5
  snd_display("sum_of_sines set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  gen.cosines = 5
  snd_display("sum_of_sines set_sines: %d?", gen.cosines) if fneq(gen.cosines, 5)
  snd_display("sum_of_sines set_sines->scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.2525)
  test_gen_equal(make_sum_of_sines(3, 440),
                 make_sum_of_sines(3, 440),
                 make_sum_of_sines(5, 440))
  test_gen_equal(make_sum_of_sines(3, 440),
                 make_sum_of_sines(3, 440),
                 make_sum_of_sines(5, 440, 1))
  test_gen_equal(make_sum_of_sines(3, 440),
                 make_sum_of_sines(3, 440),
                 make_sum_of_sines(5, 400))
  gen = make_sum_of_sines(5)
  1100.times do |i|
    den = sin(gen.phase * 0.5)
    val1 = den.zero? ? 1.0 : [1.0, gen.scaler * (sin(gen.phase * (gen.cosines + 0.5)) /
                                                   (2.0 * den) - 0.5)].min
    if fneq(val1 = sum_of_n_sines(gen.phase, 5) * gen.scaler, val2 = gen.run(0.0))
      snd_display("sum_of_sines: %d: %f %f?", i, val1, val2)
    end
  end
  gen1 = make_sum_of_sines(10, 100.0)
  gen2 = make_sum_of_sines(10, -100.0)
  mx = 0.0
  100.times do mx = [mx, (gen1.run + gen2.run).abs].max end
  if fneq(mx, 0.0)
    snd_display("sum_of_sines +-: %f?", mx)
  end
  # 
  gen = make_sine_summation(440.0)
  gen1 = make_sine_summation(440.0)
  print_and_check(gen,
                  "sine-summation",
                  "sine-summation: frequency: 440.000, phase: 0.000, n: 1, a: 0.500, ratio: 1.000")
  v0 = make_vct!(10) do sine_summation(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | sine_summation?(gen1) ? sine_summation(gen1, 0.0) : -1.0 end)
  snd_display("map sine_summation: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not sine_summation?", gen) unless sine_summation?(gen)
  snd_display("sine_summation phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  snd_display("sine_summation frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  if fneq(v0[1], 0.249) or fneq(v0[8], 1.296)
    snd_display("sine_summation output: %s?", v0)
  end
  snd_display("sine_summation set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  gen.scaler = 0.75
  snd_display("sine_summation set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.75)
  snd_display("sine_summation cosines: %d?", gen.cosines) if fneq(gen.cosines, 1)
  snd_display("sine_summation increment: %f?", gen.increment) if fneq(gen.increment, 1.0)
  test_gen_equal(make_sine_summation(440),
                 make_sine_summation(440),
                 make_sine_summation(400))
  test_gen_equal(make_sine_summation(440),
                 make_sine_summation(440),
                 make_sine_summation(440, 1))
  test_gen_equal(make_sine_summation(440),
                 make_sine_summation(440),
                 make_sine_summation(440, 0.0, 3))
  gen1 = make_sine_summation(1000, 0, 1, 0.0, 1)
  gen2 = make_oscil(1000)
  gen3 = make_sine_summation(1000, 0, 1, 0.5, 2)
  gen4 = make_oscil(1000)
  gen5 = make_oscil(3000)
  gen6 = make_sine_summation(500, 3.0, 10, 0.1, 0.4)
  if fneq(res = gen6.phase, 3.0)
    snd_display("sine_summation phase (3): %f?", res)
  end
  if fneq(res = gen6.frequency, 500.0)
    snd_display("sine_summation frequency (500): %f?", res)
  end
  if fneq(res = gen6.scaler, 0.1)
    snd_display("sine_summation scaler (0.1): %f?", res)
  end
  if (res = gen6.cosines) != 10
    snd_display("sine_summation cosines (10): %d?", res)
  end
  if fneq(res = gen6.increment, 0.4)
    snd_display("sine_summation increment (0.4): %f?", res)
  end
  100.times do |i|
    ss = sine_summation(gen1, 0.0)
    os = oscil(gen2, 0.0)
    ss1 = sine_summation(gen3, 0.0)
    os1 = oscil(gen4, 0.0) + 0.5 * oscil(gen5, 0.0)
    if ffneq(ss, os)
      snd_display("sine_summation 1: %d: os: %s ss: %s?", i, os, ss)
      break
    end
    if ffneq(ss1, os1)
      snd_display("sine_summation 2: %d: os1: %s ss1: %s?", i, os1, ss1)
      break
    end
  end
  gen1 = make_sine_summation(440.0, 0.0, 0)
  sine_summation(gen1)
  if fneq(val = sine_summation(gen1), 0.125050170279874)
    snd_display("sine_summation n=0: %f?", val)
  end
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat)
  pad_channel(0, 1000)
  gen = make_cosine_summation(100.0)
  map_channel(lambda do |y| 0.2 * cosine_summation(gen, 0.5) end)
  unless vequal(res = channel2vct(280, 10),
                vct(0.229, 0.224, 0.218, 0.211, 0.203, 0.195, 0.187, 0.178, 0.169, 0.160))
    snd_display("cosine_summation: %s?", res)
  end
  undo_edit
  angle = 0.0
  map_channel(lambda do |y|
                val = sum_of_n_sines(angle, 3)
                angle += 0.1
                val * 0.1
              end)
  unless vequal(res = channel2vct(260, 10),
                vct(0.226, 0.200, 0.166, 0.129, 0.091, 0.056, 0.025, 0.001, -0.015, -0.023))
    snd_display("sum_of_n_sines: %s?", res)
  end
  undo_edit
  angle = 0.0
  map_channel(lambda do |y|
                val = sum_of_n_odd_sines(angle, 3)
                angle += 0.1
                val * 0.1
              end)
  unless vequal(res = channel2vct(260, 10),
                vct(0.035, 0.007, 0.000, 0.014, 0.039, 0.069, 0.091, 0.100, 0.092, 0.070))
    snd_display("sum_of_n_odd_sines: %s?", res)
  end
  undo_edit
  angle = 0.0
  map_channel(lambda do |y|
                val = sum_of_n_odd_cosines(angle, 3)
                angle += 0.1
                val * 0.1
              end)
  unless vequal(res = channel2vct(250, 10),
                vct(0.270, 0.298, 0.292, 0.253, 0.189, 0.112, 0.037, -0.024, -0.061, -0.072))
    snd_display("sum_of_n_odd_cosines: %s?", res)
  end
  undo_edit
  close_sound(ind)
  # 
  gen = make_asymmetric_fm(440.0)
  gen1 = make_asymmetric_fm(440.0)
  print_and_check(gen,
                  "asymmetric-fm",
                  "asymmetric-fm freq: 440.000Hz, phase: 0.000, ratio: 1.000, r: 1.000")
  v0 = make_vct!(10) do asymmetric_fm(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | asymmetric_fm?(gen1) ? asymmetric_fm(gen1, 0.0) : -1.0 end)
  snd_display("map asymmetric_fm: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not asymmetric_fm?", gen) unless asymmetric_fm?(gen)
  snd_display("asymmetric_fm phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  gen.phase = 1.0
  snd_display("asymmetric_fm set_phase: %f?", gen.phase) if fneq(gen.phase, 1.0)
  snd_display("asymmetric_fm frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  gen.frequency = 100.0
  snd_display("asymmetric_fm set_frequency: %f?", gen.frequency) if fneq(gen.frequency, 100.0)
  if fneq(v0[2], 0.248) or fneq(v0[8], 0.843)
    snd_display("asymmetric_fm output: %s?", v0)
  end
  snd_display("asymmetric_fm set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 1.0)
  gen.scaler = 0.5
  snd_display("asymmetric_fm set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  snd_display("asymmetric_fm increment: %f?", gen.increment) if fneq(gen.increment, 1.0)
  test_gen_equal(make_asymmetric_fm(440),
                 make_asymmetric_fm(440),
                 make_asymmetric_fm(100))
  test_gen_equal(make_asymmetric_fm(440),
                 make_asymmetric_fm(440),
                 make_asymmetric_fm(440, 1))
  test_gen_equal(make_asymmetric_fm(440),
                 make_asymmetric_fm(440),
                 make_asymmetric_fm(440, 0.0, 3))
  gen1 = make_asymmetric_fm(1000, 0, 1, 0.1)
  gen2 = make_oscil(1000)
  100.times do |i|
    ss = asymmetric_fm(gen1, 0.0, 0.0)
    os = oscil(gen2, 0.0)
    if ffneq(ss, os)
      snd_display("asymmetric_fm 1: %d: os: %s ss: %s?", i, os, ss)
      break
    end
  end
  gen3 = make_asymmetric_fm(1000, 0, 1.0, 0.2)
  gen4 = make_oscil(1000)
  gen5 = make_oscil(200)
  fm1 = hz2radians(0.2 * 1000)
  vct0 = make_vct(2048)
  vct1 = make_vct!(2048) do |i|
    vct0[i] = asymmetric_fm(gen3, 1.0, 0.0)
    oscil(gen4, fm1 * oscil(gen5))
  end
  spectr1 = snd_spectrum(vct0, Rectangular_window, 2048, true)
  spectr2 = snd_spectrum(vct1, Rectangular_window, 2048, true)
  (1...512).each do |i|
    if ffneq(spectr1[i], spectr2[i])
      snd_display("asymmetric_fm 2: %d: %f %f?", i * (22050 / 2048), spectr1[i], spectr2[i])
      break
    end
  end
  gen = make_asymmetric_fm(40.0, 0.0, 1.0, 0.1)
  gen1 = make_asyfm(:frequency, 40.0, :ratio, 0.1, :index, 2.0)
  a = 0.0
  1100.times do |i|
    val1 = asymmetric_fm(gen, 2.0)
    val3 = asyfm_J(gen1, 0.0)
    r = 1.0
    ratio = 0.1
    index = 2.0
    freq = hz2radians(40.0)
    phase = a
    cr = 0.5 * (r - 1.0 / r)
    sr = 0.5 * (r + 1.0 / r)
    th = a
    mth = ratio * th
    val2 = exp(index * cr * cos(mth)) * sin(th + index * sr * sin(mth))
    if fneq(val1, val2) or fneq(val1, val3)
      snd_display("asyfm by hand: %d: 1 %f 2 %f 3 %f?", i, val1, val2, val3)
    end
    a += (2 * PI * 40) / 22050
  end
  gen3 = make_asymmetric_fm(1000, 0, 2.0, 0.1)
  gen4 = make_asymmetric_fm(1000, 0, 0.5, 0.1)
  vct0 = make_vct(2048)
  vct1 = make_vct!(2048) do |i|
    vct0[i] = asymmetric_fm(gen3, 2.0, 0.0)
    asymmetric_fm(gen4, 2.0, 0.0)
  end
  spectr1 = snd_spectrum(vct0, Rectangular_window, 2048, true)
  spectr2 = snd_spectrum(vct1, Rectangular_window, 2048, true)
  s1_loc = 0
  s2_loc = 0
  (1...256).each do |i|
    s1_loc = i if (1.0 - spectr1[i]).abs < 0.01
    s2_loc = i if (1.0 - spectr2[i]).abs < 0.01
  end
  snd_display("asymmetric_fm peaks: %d %d?", s1_loc, s2_loc) if s2_loc > s1_loc
  center = ((22050 / 2048.0) * 0.5 * (s1_loc + s2_loc)).round
  if (1000 - center).abs > 60
    snd_display("asymmetric_fm center: %s?", center)
  end
  gen3.scaler = 0.5
  2048.times do |i| vct0[i] = asymmetric_fm(gen3, 2.0, 0.0) end
  spectr1 = snd_spectrum(vct0, Rectangular_window, 2048, true)
  (1...256).each do |i|
    s1_loc = i if (1.0 - spectr1[i]).abs < 0.01
  end
  snd_display("asymmetric_fm set r peaks: %d %d?", s1_loc, s2_loc) if s2_loc != s1_loc
  2048.times do |i| vct0[i] = asymmetric_fm(gen3, 2.0, 0.0) end
  snd_spectrum(vct0, Rectangular_window, 2048, true, 0.0, true)
  (1...256).each do |i|
    s1_loc = i if (1.0 - spectr1[i]).abs < 0.01
  end
  snd_display("asymmetric_fm set r in place peaks: %d %d?", s1_loc, s2_loc) if s2_loc != s1_loc
end

def test058
  gen = make_fir_filter(3, vct(0.5, 0.25, 0.125))
  gen1 = make_fir_filter(3, vct(0.5, 0.25, 0.125))
  print_and_check(gen, "fir-filter", "fir-filter: order: 3, xs: [0.500 0.250 0.125]")
  v0 = make_vct!(10) do |i| fir_filter(gen, i.zero? ? 1.0 : 0.0) end
  v1 = make_vct(10)
  inp = -1
  vct_map!(v1, lambda do | |
             inp += 1
             fir_filter?(gen1) ? fir_filter(gen1, inp.zero? ? 1.0 : 0.0) : -1.0
           end)
  snd_display("map fir_filter: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not fir_filter?", gen) unless fir_filter?(gen)
  snd_display("fir_filter length: %d?", gen.length) if gen.length != 3
  if fneq(v0[1], 0.25) or fneq(v0[2], 0.125)
    snd_display("fir_filter output: %s?", v0)
  end
  data = gen.xcoeffs
  snd_display("fir_filter xcoeffs: %s?", data) if fneq(data[1], 0.25)
  if (res = snd_catch do mus_xcoeff(gen, 123) end).first != :mus_error
    snd_display("xcoeff 123: %s", res.inspect)
  end
  if (res = snd_catch do mus_ycoeff(gen, 123) end).first != :mus_error
    snd_display("fir ycoeff 123: %s", res.inspect)
  end
  f1 = make_fir_filter(3, vct(0.5, 0.25, 0.125))
  f2 = make_fir_filter(3, vct(0.5, 0.25, 0.125))
  f3 = make_fir_filter(3, vct(0.75, 0.25, 0.125))
  fir_filter(f1, 1.0)
  fir_filter(f2, 1.0)
  fir_filter(f3, 1.0)
  test_gen_equal(f1, f2, f3)
  f1 = make_fir_filter(3, vct(0.5, 0.25, 0.125))
  f2 = make_fir_filter(3, vct(0.5, 0.25, 0.125))
  f3 = make_fir_filter(2, vct(0.5, 0.25))
  fir_filter(f1, 1.0)
  fir_filter(f2, 1.0)
  fir_filter(f3, 1.0)
  test_gen_equal(f1, f2, f3)
  coeffs = vct(0.1, 0.2, 0.3, 0.4, 0.4, 0.3, 0.2, 0.1)
  flt = make_fir_filter(8, coeffs)
  es = make_array(8) do |i| make_env([0, coeffs[i], 1, 0], :end, 101) end
  es[5] = make_env([0, 0.4, 1, 1], :end, 101)
  data = make_vct!(100) do |i|
    val = fir_filter(flt, (i % 12).zero? ? 1.0 : 0.0)
    xcof = flt.xcoeffs
    es.each_with_index do |en, j| xcof[j] = env(en) end
    val
  end
  if fneq(data[1], 0.2) or fneq(data[10], 0.0) or
      fneq(data[18], 0.166) or fneq(data[89], 0.923)
    snd_display("filter xcoeffs: %s?", data)
  end
  # 
  gen = make_iir_filter(3, vct(0.5, 0.25, 0.125))
  gen1 = make_iir_filter(3, vct(0.5, 0.25, 0.125))
  print_and_check(gen, "iir-filter", "iir-filter: order: 3, ys: [0.500 0.250 0.125]")
  v0 = make_vct!(10) do |i| iir_filter(gen, i.zero? ? 1.0 : 0.0) end
  v1 = make_vct(10)
  inp = -1
  vct_map!(v1, lambda do | |
             inp += 1
             iir_filter?(gen1) ? iir_filter(gen1, inp.zero? ? 1.0 : 0.0) : -1.0
           end)
  snd_display("map iir_filter: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not iir_filter?", gen) unless iir_filter?(gen)
  snd_display("iir_filter length: %d?", gen.length) if gen.length != 3
  if fneq(v0[1], -0.25) or fneq(v0[2], -0.062)
    snd_display("iir_filter output: %s?", v0)
  end
  data = gen.ycoeffs
  snd_display("iir_filter ycoeffs: %s?", data) if fneq(data[1], 0.25)
  if (res = snd_catch do mus_ycoeff(gen, 123) end).first != :mus_error
    snd_display("ycoeff 123: %s", res.inspect)
  end
  if (res = snd_catch do mus_xcoeff(gen, 123) end).first != :mus_error
    snd_display("iir xcoeff 123: %s", res.inspect)
  end
  f1 = make_iir_filter(3, vct(0.5, 0.25, 0.125))
  f2 = make_iir_filter(3, vct(0.5, 0.25, 0.125))
  f3 = make_iir_filter(3, vct(0.75, 0.25, 0.125))
  iir_filter(f1, 1.0)
  iir_filter(f2, 1.0)
  iir_filter(f3, 1.0)
  test_gen_equal(f1, f2, f3)
  f1 = make_iir_filter(3, vct(0.5, 0.25, 0.125))
  f2 = make_iir_filter(3, vct(0.5, 0.25, 0.125))
  f3 = make_iir_filter(2, vct(0.5, 0.25))
  iir_filter(f1, 1.0)
  iir_filter(f2, 1.0)
  iir_filter(f3, 1.0)
  test_gen_equal(f1, f2, f3)
  # 
  gen = make_filter(3, vct(0.5, 0.25, 0.125), vct(0.5, 0.25, 0.125))
  gen1 = make_filter(3, vct(0.5, 0.25, 0.125), vct(0.5, 0.25, 0.125))
  print_and_check(gen,
                  "filter",
                  "filter: order: 3, xs: [0.500 0.250 0.125], ys: [0.500 0.250 0.125]")
  v0 = make_vct!(10) do |i| filter(gen, i.zero? ? 1.0 : 0.0) end
  v1 = make_vct(10)
  inp = -1
  vct_map!(v1, lambda do | |
             inp += 1
             filter?(gen1) ? filter(gen1, inp.zero? ? 1.0 : 0.0) : -1.0
           end)
  snd_display("map filter: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not filter?", gen) unless filter?(gen)
  snd_display("filter length: %d?", gen.length) if gen.length != 3
  if fneq(v0[1], 0.125) or fneq(v0[2], 0.031)
    snd_display("filter output: %s?", v0)
  end
  gen2 = make_biquad(0.1, 0.2, 0.3, 0.4, 0.5)
  snd_display("make_biquad: %s?", gen2) unless filter?(gen2)
  xs = gen.xcoeffs
  ys = gen.ycoeffs
  if xs != vct(0.5, 0.25, 0.125) or xs != ys
    snd_display("mus_xcoeffs: %s %s?", xs, ys)
  end
  if (res = snd_catch do
        make_filter(:order, 2, :xcoeffs, vct(1.0, 0.5), :ycoeffs, vct(2.0, 1.0, 0.5))
      end).first != :wrong_type_arg
    snd_display("make_filter bad coeffs: %s", res.inspect)
  end
  if (res = snd_catch do
        make_filter(:order, 0, :xcoeffs, vct(1.0, 0.5))
      end).first != :out_of_range
    snd_display("make_filter bad order: %s", res.inspect)
  end
  if (res = snd_catch do
        make_fir_filter(:order, 22, :xcoeffs, vct(1.0, 0.5))
      end).first != :wrong_type_arg
    snd_display("make_fir_filter bad coeffs: %s", res.inspect)
  end
  if (res = snd_catch do
        make_iir_filter(:order, 22, :ycoeffs, vct(1.0, 0.5))
      end).first != :wrong_type_arg
    snd_display("make_iir_filter bad coeffs: %s", res.inspect)
  end
  if (res = snd_catch do
        make_fir_filter(-1)
      end).first != :out_of_range
    snd_display("make_fir_filter bad order: %s", res.inspect)
  end
  unless iir_filter?(res = make_filter(:order, 2, :ycoeffs, vct(1.0, 0.5)))
    snd_display("make_filter with only y: %s", res)
  end
  f1 = make_filter(3, vct(0.5, 0.25, 0.125), vct(0.5, 0.25, 0.125))
  f2 = make_filter(3, vct(0.5, 0.25, 0.125), vct(0.5, 0.25, 0.125))
  f3 = make_filter(3, vct(0.5, 0.25, 0.125), vct(0.5, 0.5, 0.5))
  filter(f1, 1.0)
  filter(f2, 1.0)
  filter(f3, 1.0)
  test_gen_equal(f1, f2, f3)
  f1 = make_filter(3, vct(0.5, 0.25, 0.125), vct(0.5, 0.25, 0.125))
  f2 = make_filter(3, vct(0.5, 0.25, 0.125), vct(0.5, 0.25, 0.125))
  f3 = make_filter(3, vct(0.5, 0.5, 0.125), vct(0.5, 0.25, 0.0625))
  filter(f1, 1.0)
  filter(f2, 1.0)
  filter(f3, 1.0)
  test_gen_equal(f1, f2, f3)
  fr = make_fir_filter(6, vct(0, 1, 2, 3, 4, 5))
  snd_display("filter_length: %d?", fr.length) if fr.length != 6
  unless vequal(res = cascade2canonical([vct(1, 0.8, 0), vct(1, 1.4, 0.65), vct(1, 0, 0)]),
                vct(1.000, 2.200, 1.770, 0.520, 0.000, 0.000, 0.000))
    snd_display("cascade2canonical 1: %s?", res)
  end
  unless vequal(res = cascade2canonical([vct(1, -0.9, 0), vct(1, 1, 0.74), vct(1, -1.6, 0.8)]),
                vct(1.000, -1.500, 0.480, -0.330, 0.938, -0.533, 0.000))
    snd_display("cascade2canonical 2: %s?", res)
  end
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050)
  pad_channel(0, 10000)
  freq_sweep(0.45)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.962, 0.998, 0.998, 0.998, 0.998, 0.999, 0.999, 0.998, 0.997, 1.000))) and
      (not vequal(sp, vct(0.963, 0.999, 0.999, 0.999, 0.999, 0.999, 1.000, 1.000, 0.998, 0.997)))
    snd_display("initial rough spectrum: %s?", sp)
  end
  b = make_butter_high_pass(440.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v,vct(0.915, -0.162, -0.146, -0.131, -0.117, -0.103, -0.09, -0.078, -0.066, -0.056))
    snd_display("butter high: %s?", v)
  end
  b = make_butter_high_pass(1000.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.150, 0.833, 0.980, 0.994, 0.997, 0.998, 0.999, 0.998, 0.997, 1.000))) and
      (not vequal(sp, vct(0.150, 0.833, 0.981, 0.995, 0.998, 0.999, 1.000, 1.000, 0.998, 0.997)))
    snd_display("hp rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_butter_low_pass(440.0)
  v.map_with_index! do |val, i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(0.004, 0.014, 0.026, 0.035, 0.043, 0.049, 0.053, 0.055, 0.057, 0.057))
    snd_display("butter low: %s?", v)
  end
  b = make_butter_low_pass(1000.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(1.000, 0.212, 0.024, 0.005, 0.001, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("lp rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_butter_band_pass(440.0, 50.0)
  v.map_with_index! do |val, i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(0.007, 0.014, 0.013, 0.013, 0.012, 0.011, 0.009, 0.008, 0.007, 0.005))
    snd_display("butter bandpass: %s?", v)
  end
  b = make_butter_band_pass(1000.0, 500.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(0.888, 1.000, 0.144, 0.056, 0.027, 0.014, 0.008, 0.004, 0.002, 0.000))
    snd_display("bp rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_butter_band_reject(440.0, 50.0)
  v.map_with_index! do |val, i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v,
                vct(0.993, -0.014, -0.013, -0.013, -0.012, -0.011, -0.009, -0.008, -0.007, -0.005))
    snd_display("butter bandstop: %s?", v)
  end
  b = make_butter_band_reject(1000.0, 500.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.662, 0.687, 0.953, 0.980, 0.989, 0.994, 0.997, 0.997, 0.997, 1.000))) and
      (not vequal(sp, vct(0.664, 0.689, 0.955, 0.982, 0.992, 0.996, 0.999, 1.000, 0.999, 0.998)))
    snd_display("bs rough spectrum: %s?", sp)
  end
  undo_edit
  #
  v = spectrum2coeffs(10, vct(0, 1.0, 0, 0, 0, 0, 0, 0, 1.0, 0))
  v1 = make_fir_coeffs(10, vct(0, 1.0, 0, 0, 0, 0, 0, 0, 1.0, 0))
  unless vequal(v, vct(-0.190, -0.118, 0.000, 0.118, 0.190, 0.190, 0.118, 0.000, -0.118, -0.190))
    snd_display("spectrum2coeffs: %s?", v)
  end
  unless vequal(v, v1)
    snd_display("spectrum2coeffs v make_fir_coeffs: %s %s?", v, v1)
  end
  notched_spectr = make_vct(20)
  notched_spectr[2] = 1.0
  v = spectrum2coeffs(20, notched_spectr)
  v1 = make_fir_coeffs(20, notched_spectr)
  unless vequal(v, vct(0.095, 0.059, 0.000, -0.059, -0.095, -0.095, -0.059, 0.000, 0.059, 0.095,
                       0.095, 0.059, 0.000, -0.059, -0.095, -0.095, -0.059, 0.000, 0.059, 0.095))
    snd_display("spectrum2coeffs (notch): %s?", v)
  end
  unless vequal(v, v1)
    snd_display("spectrum2coeffs v(2) make_fir_coeffs: %s %s?", v, v1)
  end
  flt = make_fir_filter(20, v)
  map_channel(lambda do |y| fir_filter(flt, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(0.007, 0.493, 1.000, 0.068, 0.030, 0.019, 0.014, 0.011, 0.009, 0.009))
    snd_display("sp->coeff rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  rspect = make_vct!(20) do random(1.0) end
  v = spectrum2coeffs(20, rspect)
  v1 = make_fir_coeffs(20, rspect)
  unless vequal(v, v1)
    snd_display("spectrum2coeffs v(3) make_fir_coeffs:\n# %s\n# %s", v, v1)
  end
  b = make_highpass(hz2radians(1000.0), 10)
  v = make_vct!(20) do |i| highpass(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v,
                vct(-0.001, -0.002, -0.005, -0.011, -0.021, -0.034, -0.049, -0.065, -0.078, -0.087,
                    0.909, -0.087, -0.078, -0.065, -0.049, -0.034, -0.021, -0.011, -0.005, -0.002))
    snd_display("dsp.rb high: %s?", v)
  end
  b = make_highpass(hz2radians(1000.0), 20)
  map_channel(lambda do |y| highpass(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.053, 0.774, 0.998, 0.997, 0.997, 0.996, 0.996, 0.996, 0.997, 1.000))) and
      (not vequal(sp, vct(0.053, 0.776, 1.000, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 1.000)))
    snd_display("dsp hp rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_lowpass(hz2radians(1000.0), 10)
  v = make_vct!(20) do |i| lowpass(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(0.001, 0.002, 0.005, 0.011, 0.021, 0.034, 0.049, 0.065, 0.078, 0.087,
                       0.091, 0.087, 0.078, 0.065, 0.049, 0.034, 0.021, 0.011, 0.005, 0.002))
    snd_display("dsp.rb low: %s?", v)
  end
  b = make_lowpass(hz2radians(1000.0), 20)
  map_channel(lambda do |y| lowpass(b, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(1.000, 0.054, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("dsp lp rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_bandpass(hz2radians(1500.0), hz2radians(2000.0), 10)
  v = make_vct!(20) do |i| bandpass(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(0.001, -0.001, -0.005, -0.011, -0.017, -0.019, -0.013, 0.003, 0.022, 0.039,
                       0.045, 0.039, 0.022, 0.003, -0.013, -0.019, -0.017, -0.011, -0.005, -0.001))
    snd_display("dsp.rb bp: %s?", v)
  end
  b = make_bandpass(hz2radians(1500.0), hz2radians(2000.0), 20)
  map_channel(lambda do |y| bandpass(b, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(0.010, 1.000, 0.154, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("dsp bp rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_bandstop(hz2radians(1500.0), hz2radians(2000.0), 10)
  v = make_vct!(20) do |i| bandstop(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(-0.001, 0.001, 0.005, 0.011, 0.017, 0.019, 0.013, -0.003, -0.022, -0.039,
                       0.955, -0.039, -0.022, -0.003, 0.013, 0.019, 0.017, 0.011, 0.005, 0.001))
    snd_display("dsp.rb bs: %s?", v)
  end
  b = make_bandstop(hz2radians(1500.0), hz2radians(2000.0), 20)
  map_channel(lambda do |y| bandstop(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.904, 0.425, 0.821, 0.998, 0.997, 0.996, 0.996, 0.996, 0.997, 1.000))) and
      (not vequal(sp, vct(0.906, 0.425, 0.822, 1.000, 0.999, 0.998, 0.998, 0.998, 0.998, 1.000)))
    snd_display("dsp bp rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_differentiator(10)
  v = make_vct!(20) do |i| differentiator(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(-0.008, 0.011, -0.021, 0.039, -0.066, 0.108, -0.171, 0.270, -0.456, 0.977,
                       0.000, -0.977, 0.456, -0.270, 0.171, -0.108, 0.066, -0.039, 0.021, -0.011))
    snd_display("dsp.rb df: %s?", v)
  end
  b = make_differentiator(20)
  map_channel(lambda do |y| differentiator(b, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(0.004, 0.027, 0.075, 0.147, 0.242, 0.362, 0.506, 0.674, 0.864, 1.000))
    snd_display("dsp df rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_iir_high_pass_1(440.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v,
                vct(0.941, -0.111, -0.098, -0.086, -0.076, -0.067, -0.059, -0.052, -0.046, -0.041))
    snd_display("iir high: %s?", v)
  end
  b = make_iir_high_pass_1(1000.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.228, 0.706, 0.879, 0.940, 0.967, 0.982, 0.990, 0.994, 0.996, 1.000))) and
      (not vequal(sp, vct(0.229, 0.709, 0.883, 0.944, 0.971, 0.986, 0.994, 0.999, 1.000, 1.000)))
    snd_display("iir_1 hp rough spectrum: %s?", sp)
  end
  undo_edit
  # 
  b = make_iir_low_pass_1(440.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(0.059, 0.111, 0.098, 0.086, 0.076, 0.067, 0.059, 0.052, 0.046, 0.041))
    snd_display("iir_1 low: %s?", v)
  end
  b = make_iir_low_pass_1(1000.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(1.000, 0.402, 0.164, 0.080, 0.043, 0.023, 0.013, 0.006, 0.003, 0.001))
    snd_display("iir_1 lp rough spectrum: %s?", sp)
  end
  undo_edit
  #
  b = make_iir_high_pass_2(440.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v,
                vct(0.915, -0.162, -0.146, -0.131, -0.117, -0.103, -0.090, -0.078, -0.066, -0.056))
    snd_display("iir_2 high: %s?", v)
  end
  b = make_iir_high_pass_2(1000.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.150, 0.833, 0.980, 0.994, 0.997, 0.998, 0.999, 0.998, 0.997, 1.000))) and
      (not vequal(sp, vct(0.150, 0.833, 0.981, 0.995, 0.998, 0.999, 1.000, 1.000, 0.998, 0.997)))
    snd_display("iir_2 hp rough spectrum: %s?", sp)
  end
  undo_edit
  #
  b = make_iir_low_pass_2(440.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(0.004, 0.014, 0.026, 0.035, 0.043, 0.049, 0.053, 0.055, 0.057, 0.057))
    snd_display("iir_2 low: %s?", v)
  end
  b = make_iir_low_pass_2(1000.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(1.000, 0.212, 0.024, 0.005, 0.001, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("iir_2 lp rough spectrum: %s?", sp)
  end
  undo_edit
  #
  b = make_iir_band_pass_2(440.0, 490.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(0.007, 0.014, 0.013, 0.013, 0.012, 0.010, 0.009, 0.008, 0.006, 0.004))
    snd_display("iir bp-2 bandpass: %s?", v)
  end
  b = make_iir_band_pass_2(1000.0, 1500.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  unless vequal(sp, vct(0.239, 1.000, 0.117, 0.041, 0.019, 0.010, 0.005, 0.003, 0.001, 0.000))
    snd_display("iir bp-2 rough spectrum: %s?", sp)
  end
  undo_edit
  #
  b = make_iir_band_stop_2(440.0, 500.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v,
                vct(0.992, -0.017, -0.016, -0.015, -0.014, -0.012, -0.011, -0.009, -0.007, -0.005))
    snd_display("iir bp-2 bandstop: %s?", v)
  end
  b = make_iir_band_stop_2(1000.0, 1500.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.836, 0.525, 0.943, 0.979, 0.989, 0.994, 0.997, 0.997, 0.997, 1.000))) and
      (not vequal(sp, vct(0.838, 0.527, 0.945, 0.981, 0.991, 0.996, 0.999, 1.000, 0.999, 0.998)))
    snd_display("iir bs-2 rough spectrum: %s?", sp)
  end
  undo_edit
  #
  b = make_butter_hp(4, 440.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  if (not vequal(v, vct(0.725, -0.466, -0.315, -0.196, -0.104,
                        -0.036, 0.014, 0.047, 0.0685, 0.0775))) and
      (not vequal(v, vct(0.725, -0.466, -0.315, -0.196, -0.104,
                         -0.035, 0.015, 0.049, 0.070, 0.081)))
    snd_display("butter hp: %s?", v)
  end
  b = make_butter_hp(4, 1000.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.0505, 0.982, 1.0, 1.0, 0.998, 0.998, 0.999, 0.998, 0.996, 0.999))) and
      (not vequal(sp, vct(0.051, 0.982, 1.0, 1.0, 0.998, 0.998, 0.998, 0.999, 0.997, 0.995))) and
      (not vequal(sp, vct(0.051, 0.991, 1.0, 1.0, 0.998, 0.998, 0.999, 0.999, 0.997, 0.995)))
    snd_display("butter hp rough spectrum: %s?", sp)
  end
  undo_edit
  #
  b = make_butter_lp(4, 440.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, make_vct(10))
    snd_display("butter lp: %s?", v)
  end
  b = make_butter_lp(4, 1000.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(1.000, 0.035, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))) and
      (not vequal(sp, vct(1.000, 0.038, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000)))
    snd_display("butter lp rough spectrum: %s?", sp)
  end
  undo_edit
  #
  b = make_butter_bp(4, 440.0, 500.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, make_vct(10))
    snd_display("butter bp: %s?", v)
  end
  b = make_butter_bp(4, 1000.0, 1500.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp, vct(0.026, 1.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.0))) and
      (not vequal(sp, vct(0.022, 1.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.0))) and
      (not vequal(sp, vct(0.042, 1.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.0)))
    snd_display("butter bp 4 rough spectrum: %s?", sp)
  end
  undo_edit
  #
  b = make_butter_bs(4, 440.0, 500.0)
  v = make_vct!(10) do |i| butter(b, i.zero? ? 1.0 : 0.0) end
  if (not vequal(v, vct(0.978, -0.043, -0.041, -0.038, -0.035,
                        -0.031, -0.026, -0.0225, -0.015, -0.0085))) and
      (not vequal(v, vct(0.978, -0.043, -0.041, -0.038, -0.035,
                         -0.031, -0.027, -0.022, -0.017, -0.011)))
    snd_display("butter bs: %s?", v)
  end
  b = make_butter_bs(4, 1000.0, 1500.0)
  map_channel(lambda do |y| butter(b, y) end)
  sp = rough_spectrum(ind)
  if (not vequal(sp,  vct(0.798, 0.657, 1.0, 0.997, 0.996, 0.997, 0.997, 0.996, 0.995, 0.998))) and
      (not vequal(sp, vct(0.795, 0.668, 1.0, 0.997, 0.996, 0.997, 0.997, 0.997, 0.995, 0.994))) and
      (not vequal(sp, vct(0.801, 0.698, 1.0, 0.997, 0.996, 0.997, 0.997, 0.997, 0.995, 0.994))) and
      (not vequal(sp, vct(0.884, 0.586, 1.0, 0.996, 0.996, 0.997, 0.997, 0.997, 0.995, 0.994))) and
      (not vequal(sp, vct(0.798, 0.686, 1.0, 0.997, 0.996, 0.997, 0.997, 0.997, 0.995, 0.994))) and
      (not vequal(sp, vct(0.793, 0.667, 1.0, 0.997, 0.996, 0.997, 0.997, 0.997, 0.995, 0.994)))
    snd_display("butter bs 4 rough spectrum: %s?", sp)
  end
  undo_edit
  #
  close_sound(ind)
end

def test068
  gen = make_sawtooth_wave(440.0)
  gen1 = make_sawtooth_wave(440.0)
  print_and_check(gen, "sawtooth-wave", "sawtooth-wave freq: 440.000Hz, phase: 3.142, amp: 1.000")
  v0 = make_vct!(10) do |i| sawtooth_wave(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | sawtooth_wave?(gen1) ? sawtooth_wave(gen1, 0.0) : -1.0 end)
  snd_display("map sawtooth_wave: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not sawtooth_wave?", gen) unless sawtooth_wave?(gen)
  snd_display("sawtooth_wave phase: %f?", gen.phase) if fneq(gen.phase, 4.39538)
  snd_display("sawtooth_wave frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  gen.frequency = 100.0
  snd_display("sawtooth_wave set_frequency: %f?", gen.frequency) if fneq(gen.frequency, 100.0)
  snd_display("sawtooth_wave scaler: %f?", gen.scaler) if fneq(gen.scaler, 1.0)
  gen.scaler = 0.5
  snd_display("sawtooth_wave set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  if fneq(v0[1], 0.04) or fneq(v0[8], 0.319)
    snd_display("sawtooth_wave output: %s?", v0)
  end
  test_gen_equal(make_sawtooth_wave(440.0),
                 make_sawtooth_wave(440.0),
                 make_sawtooth_wave(120.0))
  test_gen_equal(make_sawtooth_wave(440.0),
                 make_sawtooth_wave(440.0),
                 make_sawtooth_wave(440.0, 1.0, 1.0))
  test_gen_equal(make_sawtooth_wave(440.0),
                 make_sawtooth_wave(440.0),
                 make_sawtooth_wave(440.0, 0.5))
  gen1 = make_sawtooth_wave(100.0)
  gen2 = make_sawtooth_wave(-100.0)
  mx = 0.0
  100.times do mx = [mx, (gen1.run + gen2.run).abs].max end
  snd_display("sawtooth_wave +-: %f?", mx) if fneq(mx, 0.0)
  # 
  gen = make_square_wave(440.0)
  gen1 = make_square_wave(440.0)
  print_and_check(gen, "square-wave", "square-wave freq: 440.000Hz, phase: 0.000, amp: 1.000")
  v0 = make_vct!(10) do |i| square_wave(gen, 0.0) end
  v1 = make_vct(10)
  w = 1.0
  vct_map!(v1, lambda do | |
             w = gen1.width
             square_wave?(gen1) ? square_wave(gen1, 0.0) : -1.0
           end)
  snd_display("mus_width opt: %f?", w) if fneq(w, 0.5)
  snd_display("map square_wave: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not square_wave?", gen) unless square_wave?(gen)
  snd_display("square_wave phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  snd_display("square_wave frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  snd_display("square_wave scaler: %f?", gen.scaler) if fneq(gen.scaler, 1.0)
  gen.scaler = 0.5
  snd_display("square_wave set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  snd_display("square_wave width: %f?", gen.width) if fneq(gen.width, 0.5)
  gen.width = 0.75
  snd_display("square_wave set_width: %f?", gen.width) if fneq(gen.width, 0.75)
  if fneq(v0[1], 1.0) or fneq(v0[8], 1.0)
    snd_display("square_wave output: %s?", v0)
  end
  test_gen_equal(make_square_wave(440.0),
                 make_square_wave(440.0),
                 make_square_wave(120.0))
  test_gen_equal(make_square_wave(440.0),
                 make_square_wave(440.0),
                 make_square_wave(440.0, 1.0, 1.0))
  test_gen_equal(make_square_wave(440.0),
                 make_square_wave(440.0),
                 make_square_wave(440.0, 0.5))
  old_srate = mus_srate
  set_mus_srate(500.0)
  gen = make_square_wave(100.0, -0.5, HALF_PI)
  v0 = make_vct!(20) do |i| gen.run end
  unless vequal(v0, vct(-0.5, -0.5, 0.0, 0.0, -0.5, -0.5, -0.5, 0.0, 0.0, -0.5,
                        -0.5, -0.5, 0.0, 0.0, -0.5, -0.5, -0.5, 0.0, 0.0, -0.5))
    snd_display("square_wave -0.5: %s?", v0)
  end
  set_mus_srate(old_srate)
  # 
  gen = make_triangle_wave(440.0)
  gen1 = make_triangle_wave(440.0, 1.0, PI)
  gen2 = make_triangle_wave(440.0)
  print_and_check(gen, "triangle-wave", "triangle-wave freq: 440.000Hz, phase: 0.000, amp: 1.000")
  v0 = make_vct!(10) do |i| triangle_wave(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | triangle_wave?(gen2) ? triangle_wave(gen2, 0.0) : -1.0 end)
  snd_display("map triangle_wave: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not triangle_wave?", gen) unless triangle_wave?(gen)
  snd_display("triangle_wave phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  snd_display("init triangle_wave phase: %f?", gen1.phase) if fneq(gen1.phase, PI)
  snd_display("triangle_wave frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  snd_display("triangle_wave scaler: %f?", gen.scaler) if fneq(gen.scaler, 1.0)
  gen.scaler = 0.5
  snd_display("triangle_wave set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  if fneq(v0[1], 0.08) or fneq(v0[8], 0.639)
    snd_display("triangle_wave output: %s?", v0)
  end
  test_gen_equal(make_triangle_wave(440.0),
                 make_triangle_wave(440.0),
                 make_triangle_wave(120.0))
  test_gen_equal(make_triangle_wave(440.0),
                 make_triangle_wave(440.0),
                 make_triangle_wave(440.0, 1.0, 1.0))
  test_gen_equal(make_triangle_wave(440.0),
                 make_triangle_wave(440.0),
                 make_triangle_wave(440.0, 0.5))
  gen1 = make_triangle_wave(100.0)
  gen2 = make_triangle_wave(-100.0)
  mx = 0.0
  100.times do mx = [mx, (gen1.run + gen2.run).abs].max end
  snd_display("triangle_wave +-: %f?", mx) if fneq(mx, 0.0)
  # 
  gen = make_pulse_train(440.0)
  gen1 = make_pulse_train(440.0)
  print_and_check(gen, "pulse-train", "pulse-train freq: 440.000Hz, phase: 0.000, amp: 1.000")
  v0 = make_vct!(10) do |i| pulse_train(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | pulse_train?(gen1) ? pulse_train(gen1, 0.0) : -1.0 end)
  snd_display("map pulse_train: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not pulse_train?", gen) unless pulse_train?(gen)
  snd_display("pulse_train phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  snd_display("pulse_train frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  snd_display("pulse_train scaler: %f?", gen.scaler) if fneq(gen.scaler, 1.0)
  gen.scaler = 0.5
  snd_display("pulse_train set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  if fneq(v0[0], 1.0) or fneq(v0[8], 0.0)
    snd_display("pulse_train output: %s?", v0)
  end
  test_gen_equal(make_pulse_train(440.0),
                 make_pulse_train(440.0),
                 make_pulse_train(120.0))
  test_gen_equal(make_pulse_train(440.0),
                 make_pulse_train(440.0),
                 make_pulse_train(440.0, 1.0, 1.0))
  test_gen_equal(make_pulse_train(440.0),
                 make_pulse_train(440.0),
                 make_pulse_train(440.0, 0.5))
  old_srate = mus_srate
  set_mus_srate(500.0)
  gen = make_pulse_train(100.0, -0.5, HALF_PI)
  v0 = make_vct!(20) do |i| gen.run end
  unless vequal(v0, vct(0.0, 0.0, 0.0, 0.0, -0.5, 0.0, 0.0, 0.0, 0.0, -0.5,
                        0.0, 0.0, 0.0, 0.0, -0.5, 0.0, 0.0, 0.0, 0.0, -0.5))
    snd_display("pulse_train -0.5: %s?", v0)
  end
  set_mus_srate(old_srate)
  # 
  gen = make_ppolar(0.1, 1200.0)
  v0 = make_vct!(10) do |i| two_pole(gen, i.zero? ? 1.0 : 0.0) end
  snd_display("%s not two_pole?", gen) unless two_pole?(gen)
  snd_display("ppolar order: %d?", gen.order) if gen.order != 2
  snd_display("ppolar a0: %f?", gen.a0) if fneq(gen.a0, 1.0)
  snd_display("ppolar b1: %f?", gen.b1) if fneq(gen.b1, -0.188)
  snd_display("ppolar b2: %f?", gen.b2) if fneq(gen.b2, 0.01)
  if fneq(v0[0], 1.0) or fneq(v0[1], 0.188)
    snd_display("ppolar output: %s?", v0)
  end
  z1 = make_ppolar(0.1, 600.0)
  z2 = make_ppolar(0.1, 600.0)
  z3 = make_ppolar(0.1, 1200.0)
  two_pole(z1, 1.0)
  two_pole(z2, 1.0)
  two_pole(z3, 1.0)
  test_gen_equal(z1, z2, z3)
  z1 = make_ppolar(0.1, 600.0)
  z2 = make_ppolar(0.1, 600.0)
  z3 = make_ppolar(0.2, 1200.0)
  two_pole(z1, 1.0)
  two_pole(z2, 1.0)
  two_pole(z3, 1.0)
  test_gen_equal(z1, z2, z3)
  z1 = make_ppolar(0.1, 600.0)
  z2 = make_ppolar(0.1, 600.0)
  z3 = make_ppolar(0.1, 600.0)
  two_pole(z1, 1.0)
  two_pole(z2, 1.0)
  two_pole(z3, 0.5)
  test_gen_equal(z1, z2, z3)
  # 
  gen = make_zpolar(:radius, 0.1, :frequency, 1200.0)
  v0 = make_vct!(10) do |i| two_zero(gen, i.zero? ? 1.0 : 0.0) end
  snd_display("%s not two_zero?", gen) unless two_zero?(gen)
  snd_display("zpolar order: %d?", gen.order) if gen.order != 2
  snd_display("zpolar a0: %f?", gen.a0) if fneq(gen.a0, 1.0)
  snd_display("zpolar a1: %f?", gen.a1) if fneq(gen.a1, -0.188)
  snd_display("zpolar a2: %f?", gen.a2) if fneq(gen.a2, 0.01)
  if fneq(v0[0], 1.0) or fneq(v0[1], -0.188)
    snd_display("zpolar output: %s?", v0)
  end
  z1 = make_zpolar(0.1, 600.0)
  z2 = make_zpolar(0.1, 600.0)
  z3 = make_zpolar(0.1, 1200.0)
  two_zero(z1, 1.0)
  two_zero(z2, 1.0)
  two_zero(z3, 1.0)
  test_gen_equal(z1, z2, z3)
  z1 = make_zpolar(0.1, 600.0)
  z2 = make_zpolar(0.1, 600.0)
  z3 = make_zpolar(0.2, 1200.0)
  two_zero(z1, 1.0)
  two_zero(z2, 1.0)
  two_zero(z3, 1.0)
  test_gen_equal(z1, z2, z3)
  z1 = make_zpolar(0.1, 600.0)
  z2 = make_zpolar(0.1, 600.0)
  z3 = make_zpolar(0.1, 600.0)
  two_zero(z1, 1.0)
  two_zero(z2, 1.0)
  two_zero(z3, 0.5)
  test_gen_equal(z1, z2, z3)
  # 
  gen = make_formant(0.9, 1200.0, 1.0)
  gen1 = make_formant(0.9, 1200.0, 1.0)
  print_and_check(gen, "formant", "formant: radius: 0.900, frequency: 1200.000, (gain: 1.000)")
  v0 = make_vct!(10) do |i| formant(gen, i.zero? ? 1.0 : 0.0) end
  v1 = make_vct(10)
  inp = -1
  vct_map!(v1, lambda do | |
             inp += 1
             formant?(gen1) ? formant(gen1, inp.zero? ? 1.0 : 0.0) : -1.0
           end)
  snd_display("map formant: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not formant?", gen) unless formant?(gen)
  snd_display("formant order: %d?", gen.order) if gen.order != 2
  snd_display("formant a0: %f?", gen.a0) if fneq(gen.a0, 0.06371)
  snd_display("formant a1: %f?", gen.a1) if fneq(gen.a1, 1.0)
  snd_display("formant a2: %f?", gen.a2) if fneq(gen.a2, -0.9)
  snd_display("formant b1: %f?", gen.b1) if fneq(gen.b1, -1.6957893)
  snd_display("formant b2: %f?", gen.b2) if fneq(gen.b2, 0.81)
  snd_display("formant radius: %f?", mus_formant_radius(gen)) if fneq(mus_formant_radius(gen), 0.9)
  snd_display("formant frequency: %f?", gen.frequency) if fneq(gen.frequency, 1200.0)
  if fneq(v0[0], 0.064) or fneq(v0[1], 0.108)
    snd_display("formant output: %s?", v0)
  end
  snd_display("formant gain: %f?", gen.scaler) if fneq(gen.scaler, 1.0)
  snd_display("formant ycoeff 2 0.81: %f?", gen.ycoeff(2)) if fneq(gen.ycoeff(2), 0.81)
  set_mus_ycoeff(gen, 2, 0.1)
  snd_display("formant set_ycoeff 2 0.1: %f?", gen.ycoeff(2)) if fneq(gen.ycoeff(2), 0.1)
  snd_display("formant xcoeff 2 -0.9: %f?", gen.xcoeff(2)) if fneq(gen.xcoeff(2), -0.9)
  set_mus_xcoeff(gen, 2, 0.3)
  snd_display("formant set_xcoeff 2 0.3: %f?", gen.xcoeff(2)) if fneq(gen.xcoeff(2), 0.3)
  gen.a0 = 0.5
  snd_display("formant set_a0: %f?", gen.a0) if fneq(gen.a0, 0.5)
  gen.a1 = 0.5
  snd_display("formant set_a1: %f?", gen.a1) if fneq(gen.a1, 0.5)
  gen.a2 = 0.5
  snd_display("formant set_a2: %f?", gen.a2) if fneq(gen.a2, 0.5)
  gen.b1 = 0.5
  snd_display("formant set_b1: %f?", gen.b1) if fneq(gen.b1, 0.5)
  gen.b2 = 0.5
  snd_display("formant set_b2: %f?", gen.b2) if fneq(gen.b2, 0.5)
  set_mus_formant_radius(gen, 0.01)
  if fneq(res = mus_formant_radius(gen), 0.01)
    snd_display("formant set_radius: %f?", res)
  end
  gen.scaler = 2.0
  snd_display("formant set_gain: %f?", gen.scaler) if fneq(gen.scaler, 2.0)
  f1 = make_formant(0.9, 1200.0, 1.0)
  f2 = make_formant(0.9, 1200.0, 1.0)
  f3 = make_formant(0.9,  600.0, 1.0)
  formant(f1, 1.0)
  formant(f2, 1.0)
  formant(f3, 1.0)
  test_gen_equal(f1, f2, f3)
  f1 = make_formant(0.90, 1200.0, 1.0)
  f2 = make_formant(0.90, 1200.0, 1.0)
  f3 = make_formant(0.99, 1200.0, 1.0)
  formant(f1, 1.0)
  formant(f2, 1.0)
  formant(f3, 1.0)
  test_gen_equal(f1, f2, f3)
  f1 = make_formant(0.9, 1200.0, 1.0)
  f2 = make_formant(0.9, 1200.0, 1.0)
  f3 = make_formant(0.9, 1200.0, 0.5)
  formant(f1, 1.0)
  formant(f2, 1.0)
  formant(f3, 0.5)
  test_gen_equal(f1, f2, f3)
  # 
  frm = make_formant(0.1, 440.0)
  mus_set_formant_radius_and_frequency(frm, 2.0, 100.0)
  if fneq(res = mus_formant_radius(frm), 2.0)
    snd_display("set_formant_radius_and_frequency (radius): %f?", res)
  end
  if fneq(res = mus_frequency(frm), 100.0)
    snd_display("set_formant_radius_and_frequency (freq): %f?", res)
  end
  fs = make_array(1) do make_formant(0.1, 1000.0) end
  f0 = make_formant(0.1, 1000.0)
  amps = make_array(1, 1.0)
  v0 = make_vct!(10) do |i| formant(f0, i.zero? ? 1.0 : 0.0) end
  v1 = make_vct!(10) do |i| old_formant_bank(amps, fs, i.zero? ? 1.0 : 0.0) end
  snd_display("formant_bank: %s %s?", v0, v1) unless vequal(v0, v1)
  # 
  fs = [make_formant(0.1, 1000.0), make_formant(0.2, 100.0)]
  f0 = make_formant(0.1, 1000.0)
  f1 = make_formant(0.2, 100.0)
  amps = [0.5, 0.25]
  v0 = make_vct!(10) do |i|
    val = i.zero? ? 1.0 : 0.0
    (0.5 * formant(f0, val)) + (0.25 * formant(f1, val))
  end
  v1 = make_vct!(10) do |i| old_formant_bank(amps, fs, i.zero? ? 1.0 : 0.0) end
  snd_display("formant_bank 1: %s %s?", v0, v1) unless vequal(v0, v1)
  # 
  fs = [make_formant(0.1, 1000.0), make_formant(0.2, 100.0)]
  amps = vct(0.5, 0.25)
  v = make_vct!(5) do |i| old_formant_bank(amps, fs, i.zero? ? 1.0 : 0.0) end
  unless vequal(v, vct(0.146, 0.029, -0.011, -0.003, 0.000))
    snd_display("run formant_bank: %s?", v)
  end
  #
  ob = open_sound("oboe.snd")
  poltergeist = lambda do |frek, amp, r, gain, frek_env, r_env|
    # test courtesy of Anders Vinjar
    filt = make_formant(r, frek, gain)
    fe = make_env(:envelope, frek_env, :end, frames, :offset, frek)
    re = make_env(:envelope, r_env, :end, frames, :offset, r)
    lambda do |y|
      outval = formant(filt, amp * y)
      mus_set_formant_radius_and_frequency(filt, env(re), env(fe))
      outval
    end
  end
  map_chan(poltergeist.call(300, 0.1, 0.0, 30.0, [0, 100, 1, 4000], [0, 0.99, 1, 0.9]))
  play_and_wait(0, ob)
  close_sound(ob)
end

include Mixer_matrix

def test078
  gen = make_mixer(2, 0.5, 0.25, 0.125, 1.0)
  fr0 = make_frame(2, 1.0, 1.0)
  fr1 = make_frame(2, 0.0, 0.0)
  print_and_check(gen,
                  "mixer",
                  "mixer: chans: 2, vals: [
 0.500 0.250
 0.125 1.000
]")
  ap = mus_array_print_length
  mx = make_mixer(8)
  set_mus_array_print_length(4)
  mx.length.times do |i|
    mx.length.times do |j|
      mixer_set!(mx, i, j, j + i * 8)
    end
  end
  print_and_check(mx,
                  "mixer",
                  "mixer: chans: 8, vals: [
 0.000 1.000 2.000 3.000...
 8.000 9.000 10.000 11.000...
 16.000 17.000 18.000 19.000...
 24.000 25.000 26.000 27.000...
]")
  set_mus_array_print_length(12)
  print_and_check(mx,
                  "mixer",
                  "mixer: chans: 8, vals: [
 0.000 1.000 2.000 3.000 4.000 5.000 6.000 7.000
 8.000 9.000 10.000 11.000 12.000 13.000 14.000 15.000
 16.000 17.000 18.000 19.000 20.000 21.000 22.000 23.000
 24.000 25.000 26.000 27.000 28.000 29.000 30.000 31.000
 32.000 33.000 34.000 35.000 36.000 37.000 38.000 39.000
 40.000 41.000 42.000 43.000 44.000 45.000 46.000 47.000
 48.000 49.000 50.000 51.000 52.000 53.000 54.000 55.000
 56.000 57.000 58.000 59.000 60.000 61.000 62.000 63.000
]")
  set_mus_array_print_length(ap)
  print_and_check(fr0, "frame", "frame[2]: [1.000 1.000]")
  snd_display("%s not a frame?", fr0) unless frame?(fr0)
  snd_display("%s not a mixer?", gen) unless mixer?(gen)
  snd_display("frame=? %s %s?", fr0, fr1) if fr0.eql?(fr1)
  snd_display("frame channels: %d?", fr0.channels) if fr0.channels != 2
  snd_display("frame length: %d?", fr1.length) if fr1.length != 2
  snd_display("mixer channels: %d?", gen.channels) if gen.channels != 2
  frame2frame(fr0, gen, fr1)
  if fneq(frame_ref(fr0, 0), 1.0) or
      fneq(frame_ref(fr1, 1), 1.25) or
      fneq(mixer_ref(gen, 0, 0), 0.5)
    snd_display("fr0: %s?", fr0)
  end
  frame_set!(fr1, 0, 1.0)
  fr3 = frame_add(fr0, fr1)
  fr4 = frame_multiply(fr0, fr1)
  fr5 = sample2frame(fr1, 0.5)
  if fneq(frame_ref(fr3, 0), 2.0) or
      fneq(frame_ref(fr4, 0), 1.0)
    snd_display("fr+*: %s %s?", fr3, fr4)
  end
  if fneq(res = frame_ref(fr5, 0), 0.5)
    snd_display("sample2frame: %f?", res)
  end
  sample2frame(fr1, 0.5, fr5)
  if fneq(res = frame_ref(fr5, 0), 0.5)
    snd_display("repeat sample2frame: %f?", res)
  end
  fr3 = make_frame(2)
  fr4 = make_frame(4)
  frame_set!(fr3, 0, 1.0)
  frame_set!(fr4, 0, 0.5)
  frame_set!(fr4, 2, 0.5)
  unless vequal(frame2list(res = frame_add(fr3, fr4)), [1.5, 0.0])
    snd_display("frame_add unequal chans: %s?", res)
  end
  fr3.reset
  if fneq(frame_ref(fr3, 0), 0.0)
    snd_display("reset frame: %s?", fr3)
  end
  fr3 = make_frame(2)
  fr4 = make_frame(4)
  frame_set!(fr3, 0, 1.0)
  frame_set!(fr4, 0, 0.5)
  frame_set!(fr4, 2, 1.0)
  unless vequal(frame2list(res = frame_multiply(fr3, fr4)), [0.5, 0.0])
    snd_display("frame_multiply unequal chans: %s?", res)
  end
  mx1 = make_mixer(2, 1.0, 0.0, 0.0, 1.0)
  mx2 = mixer_multiply(gen, mx1)
  fr4 = make_frame(2, 1.0, 1.0)
  fr5 = make_frame(2, 1.0, 1.0)
  if fneq(res = frame2sample(mx1, fr1), 1.0)
    snd_display("frame2sample: %s?", res)
  end
  if fneq(res = frame2sample(fr5, fr4), 2.0)
    snd_display("frame2sample: %s?", res)
  end
  unless (res = frame2list(fr1)).eql?([1.0, 1.25])
    snd_display("frame2list: %s?", res)
  end
  if fneq(mixer_ref(mx2, 0, 1), 0.25) or fneq(mixer_ref(mx2, 1, 0), 0.125)
    snd_display("mixer_multiply: %s?", mx2)
  end
  unless mx2.eql?(gen)
    snd_display("mixer=? %s %s?", gen, mx2)
  end
  if mx2.eql?(mx1)
    snd_display("mixer!=? %s %s?", mx1, mx2)
  end
  snd_display("mus_data frame: %s?", fr4.data) unless vct?(fr4.data)
  # mus-data doesn't apply from scheme (ruby) level here
  # snd_display("mus_data mixer: %s?", mx1.data) unless vct?(mx1.data)
  mixer_set!(mx2, 0, 0, 2.0)
  if fneq(mixer_ref(mx2, 0, 0), 2.0)
    snd_display("mixer_set!: %s?", mx2)
  end
  fr0 = sample2frame(mx2, 1.0)
  if fneq(frame_ref(fr0, 0), 2.0) or fneq(frame_ref(fr0, 1), 0.25)
    snd_display("sample2frame: %s?", fr0)
  end
  frout = make_frame(2)
  sample2frame(mx2, 1.0, frout)
  unless frout.eql?(fr0)
    snd_display("sample2frame via frout: %s %s?", frout, fr0)
  end
  #
  mx1 = make_scalar_mixer(2, 2.0)
  mx2 = make_mixer(2, 0.1, 0.2, 0.3, 0.4)
  nmx = mixer_add(mx1, mx2)
  if fneq(mixer_ref(mx1, 0, 0), 2.0) or
      fneq(mixer_ref(mx1, 0, 1), 0.0) or
      fneq(mixer_ref(mx1, 1, 0), 0.0) or
      fneq(mixer_ref(mx1, 1, 1), 2.0)
    snd_display("make_scalar_mixer 2: %s?", mx1)
  end
  if fneq(mixer_ref(mx2, 0, 0), 0.1) or
      fneq(mixer_ref(mx2, 0, 1), 0.2) or
      fneq(mixer_ref(mx2, 1, 0), 0.3) or
      fneq(mixer_ref(mx2, 1, 1), 0.4)
    snd_display("make_mixer 0.1, 0.2, 0.3, 0.4: %s?", mx2)
  end
  if fneq(mixer_ref(nmx, 0, 0), 2.1) or
      fneq(mixer_ref(nmx, 0, 1), 0.2) or
      fneq(mixer_ref(nmx, 1, 0), 0.3) or
      fneq(mixer_ref(nmx, 1, 1), 2.4)
    snd_display("mixer_add: %s?", nmx)
  end
  mx1 = mixer_multiply(mx1, 0.5)
  if fneq(mixer_ref(mx1, 0, 0), 1.0) or
      fneq(mixer_ref(mx1, 0, 1), 0.0) or
      fneq(mixer_ref(mx1, 1, 0), 0.0) or
      fneq(mixer_ref(mx1, 1, 1), 1.0)
    snd_display("mixer_multiply (identity): %s?", mx1)
  end
  mx1.reset
  if fneq(mixer_ref(mx1, 0, 0), 0.0)
    snd_display("reset mixer: %s?", mx1)
  end
  #
  if (res = snd_catch do make_mixer(2, 0.0, 0.0, 0.0, 0.0, 0.0) end).first != :mus_error
    snd_display("make_mixer extra args: %s", res.inspect)
  end
  if (res = snd_catch do
        fr1 = make_frame(2, 1.0, 0.0)
        frame2sample(make_oscil, fr1)
      end).first != :mus_error
    snd_display("frame2sample bad arg: %s", res.inspect)
  end
  hi = make_mixer(1, 1)
  if (res = snd_catch do mixer_set!(hi, 1, 1, 1.0) end).first != :mus_error
    snd_display("mixer_set! 1 1 of 0: %s (%s)", res.inspect, hi)
  end
  hi = make_mixer(1)
  if (res = snd_catch do mixer_set!(hi, 1, 0, 1.0) end).first != :mus_error
    snd_display("mixer_set! 1 0 of 0: %s (%s)", res.inspect, hi)
  end
  hi = make_mixer(1)
  if (res = snd_catch do mixer_set!(hi, 0, 1, 1.0) end).first != :mus_error
    snd_display("mixer_set! 0 1 of 0: %s (%s)", res.inspect, hi)
  end
  hi = make_frame(1)
  if (res = snd_catch do frame_set!(hi, 1, 1.0) end).first != :mus_error
    snd_display("frame_set! 1 of 0: %s (%s)", res.inspect, hi)
  end
  if (res = snd_catch do make_frame(0) end).first != :out_of_range
    snd_display("make_frame 0: %s", res.inspect)
  end
  if (res = snd_catch do make_mixer(0) end).first != :out_of_range
    snd_display("make_mixer 0: %s", res.inspect)
  end
  #
  fr1 = make_frame(1, 1)
  fr2 = make_frame(2, 1, 2)
  fr4 = make_frame(4, 1, 2, 3, 4)
  fr8 = make_frame(8, 1, 2, 3, 4, 5, 6, 7, 8)
  mx1 = make_mixer(1, 5)
  mx1id = make_mixer(1, 1)
  mx2 = make_mixer(2, 1, 2, 3, 4)
  mx2id = make_mixer(2, 1, 0, 0, 1)
  mx4 = make_mixer(4)
  mx4id = make_mixer(4)
  mx8 = make_mixer(8)
  mx8id = make_mixer(8)
  4.times do |i|
    mixer_set!(mx4id, i, i, 1)
    mixer_set!(mx4, 0, i, 1)
  end
  8.times do |i|
    mixer_set!(mx8id, i, i, 1)
    mixer_set!(mx8, i, 0, 1)
  end
  unless (res = frame2frame(fr1, mx1id)).eql?(make_frame(1, 1))
    snd_display("frame2frame 1 id: %s?", res)
  end
  unless (res = frame2frame(fr1, mx1)).eql?(make_frame(1, 5))
    snd_display("frame2frame 1: %s?", res)
  end
  unless (res = frame2frame(fr1, mx2id)).eql?(make_frame(2, 1, 0))
    snd_display("frame2frame 2 1 id: %s?", res)
  end
  unless (res = frame2frame(fr1, mx2)).eql?(make_frame(2, 1, 2))
    snd_display("frame2frame 2 1: %s?", res)
  end
  unless (res = frame2frame(fr1, mx4)).eql?(make_frame(4, 1, 1, 1, 1))
    snd_display("frame2frame 4 1: %s?", res)
  end
  unless (res = frame2frame(fr1, mx8)).eql?(make_frame(8, 1, 0, 0, 0, 0, 0, 0, 0))
    snd_display("frame2frame 8 1: %s?", res)
  end
  unless (res = frame2frame(fr2, mx1)).eql?(make_frame(1, 5))
    snd_display("frame2frame 1 2: %s?", res)
  end
  unless (res = frame2frame(fr2, mx2id)).eql?(make_frame(2, 1, 2))
    snd_display("frame2frame 2 id 2: %s?", res)
  end
  unless (res = frame2frame(fr2, mx2)).eql?(make_frame(2, 7, 10))
    snd_display("frame2frame 2 2: %s?", res)
  end
  unless (res = frame2frame(fr2, mx4id)).eql?(make_frame(4, 1, 2, 0, 0))
    snd_display("frame2frame 4 id 2: %s?", res)
  end
  unless (res = frame2frame(fr2, mx8id)).eql?(make_frame(8, 1, 2, 0, 0, 0, 0, 0, 0))
    snd_display("frame2frame 8 id 2: %s?", res)
  end
  unless (res = frame2frame(fr2, mx4)).eql?(make_frame(4, 1, 1, 1, 1))
    snd_display("frame2frame 4 2: %s?", res)
  end
  unless (res = frame2frame(fr2, mx8)).eql?(make_frame(8, 3, 0, 0, 0, 0, 0, 0, 0))
    snd_display("frame2frame 8 2: %s?", res)
  end
  unless (res = frame2frame(fr4, mx1)).eql?(make_frame(1, 5))
    snd_display("frame2frame 1 4: %s?", res)
  end
  unless (res = frame2frame(fr8, mx1)).eql?(make_frame(1, 5))
    snd_display("frame2frame 1 8: %s?", res)
  end
  unless (res = frame2frame(fr4, mx4)).eql?(make_frame(4, 1, 1, 1, 1))
    snd_display("frame2frame 4 4: %s?", res)
  end
  unless (res = frame2frame(fr4, mx8)).eql?(make_frame(8, 10, 0, 0, 0, 0, 0, 0, 0))
    snd_display("frame2frame 8 4: %s?", res)
  end
  #
  fr1 = make_frame(2)
  fr2 = make_frame(2)
  mx1 = make_mixer(2)
  mx2 = make_mixer(2)
  frame_set!(fr1, 0, 0.1)
  fradd = frame_add(fr1, fr1, fr2)
  unless fr2.eql?(fradd)
    snd_display("frame_add with res frame: %s %s?", fr2, fradd)
  end
  unless fr2.eql?(make_frame(2, 0.2, 0.0))
    snd_display("frame_add res: %s?", fr2)
  end
  fradd = frame_multiply(fr1, fr1, fr2)
  unless fr2.eql?(fradd)
    snd_display("frame_multiply with res frame: %s %s?", fr2, fradd)
  end
  if fneq(frame_ref(fr2, 0), 0.01) or fneq(frame_ref(fr2, 1), 0.0)
    snd_display("frame_multiply res: %s?", fr2)
  end
  mixer_set!(mx1, 0, 0, 0.1)
  mxadd = mixer_multiply(mx1, mx1, mx2)
  unless mx2.eql?(mxadd)
    snd_display("mixer_multiply with res mixer: %s %s?", mx2, mxadd)
  end
  if fneq(mixer_ref(mx2, 0, 0), 0.01)
    snd_display("mixer_multiply res: %s?", mx2)
  end
  #
  [1, 2, 4, 8].each do |chans|
    m1 = make_mixer(chans)
    if m1.channels != chans or m1.length != chans
      snd_display("mixer %d chans but: %d %d?", chans, m1.channels, m1.length)
    end
    chans.times do |i|
      chans.times do |j|
        mixer_set!(m1, i, j, i * 0.01 + j * 0.1)
      end
    end
    chans.times do |i|
      chans.times do |j|
        if fneq(res0 = mixer_ref(m1, i, j), res1 = i * 0.01 + j * 0.1)
          snd_display("mixer[%d %d] = %f (%f)?", i, j, res0, res1)
        end
      end
    end
    mempty = make_mixer(chans)
    midentity = make_mixer(chans)
    mpick = make_mixer(chans)
    chans.times do |i| mixer_set!(midentity, i, i, 1.0) end
    mixer_set!(mpick, chans - 1, chans - 1, 1.0)
    mzero = mixer_multiply(m1, mempty)
    msame = mixer_multiply(m1, midentity)
    mone = mixer_multiply(m1, mpick)
    chans.times do |i|
      chans.times do |j|
        if fneq(res = mixer_ref(mzero, i, j), 0.0)
          snd_display("mzero %d %d = %f?", i, j, res)
        end
        if fneq(res0 = mixer_ref(m1, i, j), res1 = mixer_ref(msame, i, j))
          snd_display("msame %f %f?", res0, res1)
        end
        if fneq(res = mixer_ref(mone, i, j), 0.0) and
            i != chans - 1 and
            j != chans - 1
          snd_display("mone %d %d = %f?", i, j, res)
        end
      end
    end
  end
  #
  mx = make_mixer(4, 4)
  if (res = snd_catch do mx.length = 2 end).first != :mus_error
    snd_display("set_mixer_length: %s %d", res.inspect, mx.length)
  end
  #
  if fneq(res = mixer_determinant(make_mixer(2, 1, 2, 3, 4)), -2.0)
    snd_display("mixer_determinant -2: %f?", res)
  end
  if fneq(res = mixer_determinant(make_mixer(3, 1, 2, 3, 4, 5, 6, 7, 8, 9)), 0.0)
    snd_display("mixer_determinant 0: %f?", res)
  end
  if fneq(res = mixer_determinant(make_mixer(4, 1, 2, 3, 4, 8, 7, 6, 5, 1, 8, 2, 7, 3, 6, 4, 5)),
          -144.0)
    snd_display("mixer_determinant -144: %f?", res)
  end
  if fneq(res = mixer_determinant(make_mixer(5,  2, 3, 5, 7, 11,  13, 17, 19, 23, 29,
                                             31, 37, 41, 43, 47,  53, 59, 61, 67, 71,
                                             73, 79, 83, 89, 97)), -4656.0)
    snd_display("mixer_determinant -4656: %f?", res)
  end
  if fneq(res = mixer_determinant(make_mixer(6,  2, 3, 5, 7, 11, 13,   17, 19, 23, 29, 31, 37,
                                             41, 43, 47, 53, 59, 61,  67, 71, 73, 79, 83, 89,  
                                             97, 101, 103, 107, 109, 113,
                                             127, 131, 137, 139, 149, 151)), -14304.0)
    snd_display("mixer_determinant -14304: %f?", res)
  end
  unless mixer_equal?(res = mixer_transpose(make_mixer(2, 1, 2, 3, 4)),
                      make_mixer(2, 1.0, 3.0, 2.0, 4.0))
    snd_display("mixer_transpose 1: %s?", res)
  end
  unless mixer_equal?(res = mixer_transpose(make_mixer(3, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
                      make_mixer(3, 1.0, 4.0, 7.0, 2.0, 5.0, 8.0, 3.0, 6.0, 9.0))
    snd_display("mixer_transpose 2: %s?", res)
  end
  unless mixer_equal?(res = mixer_multiply(make_mixer(2, 1, 0, 0, 1), make_mixer(2, 2, 0, 0, 2)),
                      make_mixer(2, 2.0, 0.0, 0.0, 2.0))
    snd_display("mixer_multiply 1: %s?", res)
  end
  unless mixer_equal?(res = mixer_multiply(make_mixer(3, 2, 3, 5, 7, 11, 13, 19, 23, 29),
                                           make_mixer(3, 41, 43, 47, 53, 59, 61, 67, 71, 73)),
                      make_mixer(3, 576, 618, 642, 1741, 1873, 1949, 3941, 4233, 4413))
    snd_display("mixer_multiply 2: %s?", res)
  end
  unless mixer_equal?(res = mixer_inverse(make_mixer(2, 1, 0, 0, 1)), make_mixer(2, 1, 0, 0, 1))
    snd_display("mixer_inverse 1: %s?", res)
  end
  unless mixer_equal?(res = mixer_inverse(make_mixer(2, 2, 3, 5, 8)), make_mixer(2, 8, -3, -5, 2))
    snd_display("mixer_inverse 2: %s?", res)
  end
  unless mixer_equal?(res = mixer_inverse(make_mixer(3,  2, 3, 5,  7, 11, 13,  17, 19, 23)),
                      make_mixer(3, -0.077, -0.333, 0.205, -0.769, 0.5, -0.115,
                                 0.692, -0.167, -0.013))
    snd_display("mixer_inverse 3: %s?", res)
  end
  unless mixer_equal?(res = mixer_inverse(make_mixer(4,  2, 3, 5, 7,  17, 19, 23, 29,
                                                     41, 43, 47, 53,  67, 71, 73, 97)),
                      make_mixer(4, -7, 4.708, -1.042, -0.333, 9, -6.396, 1.396, 0.5, 
                                 -1, 0.875, -0.042, -0.167, -1, 0.771, -0.271, 0))
    snd_display("mixer_inverse 4: %s?", res)
  end
  unless mixer_equal?(res = mixer_inverse(make_mixer(6,  2, 3, 5, 7, 11, 13,
                                                     17, -19, 23, 29, 31, 37,
                                                     41, 43, 47, 53, 59, 61,
                                                     67, 71, 73, 79, 83, 89,
                                                     97, 101, 103, 107, 109, 113,
                                                     127, 131, 137, 139, 149, 151)), 
                      make_mixer(6, -1.355, 0.02, -0, 1.09, -1.153, 0.333, 0.092,
                                -0.025, 0, -0.042, 0.07, -0.029, 1.612,
                                 0.006, -0.25, -1.205, 1.249, -0.264,
                                 0.079, 0.002, 0.25, -0.314, 0.425, -0.241,
                                 -0.551, -0.011, 0.25, 0.2, -0.476, 0.188,
                                 0.068, 0.009, -0.25, 0.306, -0.145, 0.028))
    snd_display("mixer_inverse 5: %s?", res)
  end
  unless mixer_equal?(res = mixer_multiply(make_mixer(2, 2, 3, 5, 8),
                                           mixer_inverse(make_mixer(2, 2, 3, 5, 8))),
                      make_scalar_mixer(2, 1.0))
    snd_display("mixer_inverse 6: %s?", res)
  end
  unless mixer_equal?(res = mixer_multiply(make_mixer(3, 2, 3, 5, 7, 11, 13, 17, 19, 23),
                                           mixer_inverse(make_mixer(3, 2, 3, 5,
                                                                    7, 11, 13,
                                                                    17, 19, 23))),
                      make_scalar_mixer(3, 1.0))
    snd_display("mixer_inverse 7: %s?", res)
  end
  unless mixer_diagonal?(make_scalar_mixer(2, 2.0))
    snd_display("mixer_diagonal 1")
  end
  unless mixer_diagonal?(make_mixer(3, 1, 0, 0, 0, 1, 0, 0, 0, 1))
    snd_display("mixer_diagonal 2")
  end
  if mixer_diagonal?(make_mixer(3, 1, 0, 0, 0, 1, 1, 0, 0, 1))
    snd_display("mixer_diagonal 3")
  end
  unless mixer_diagonal?(make_mixer(3, 0, 0, 0, 0, 1, 0, 0, 0, 1))
    snd_display("mixer_diagonal 4")
  end
  unless mixer_symmetric?(make_mixer(3, 0, 0, 0, 0, 1, 0, 0, 0, 1))
    snd_display("mixer_symmetric 1")
  end
  unless mixer_symmetric?(make_mixer(3, 1, 2, 0, 2, 1, 0, 0, 0, 1))
    snd_display("mixer_symmetric 2")
  end
  if mixer_symmetric?(make_mixer(3, 1, 2, 0, 2, 1, 0, 0, 2, 1))
    snd_display("mixer_symmetric 3")
  end
  unless mixer_equal?(make_scalar_mixer(2, 2.0), make_mixer(2, 2.0, 0, 0, 2.0))
    snd_display("mixer_equal? 1")
  end
  if mixer_equal?(make_mixer(2, 1, 2, 3, 4), make_mixer(3, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    snd_display("mixer_equal? 2")
  end
  if mixer_equal?(make_mixer(2, 1, 2, 3, 4), make_mixer(2, 1, 2, 3, 5))
    snd_display("mixer_equal? 3")
  end
  unless mixer_equal?(res = mixer_poly(make_mixer(2, 1, 0, 0, 1), 1, 1), make_mixer(2, 2, 0, 0, 2))
    snd_display("mixer_poly 1: %s?", res)
  end
  unless mixer_equal?(res = mixer_poly(make_mixer(1, 1), 1), make_mixer(1, 1))
    snd_display("mixer_poly 2: %s?", res)
  end
  unless mixer_equal?(res = mixer_poly(make_mixer(2, 1, 0, 0, 1), 1, 0, 0),
                      make_mixer(2, 1, 0, 0, 1))
    snd_display("mixer_poly 3: %s?", res)
  end
  unless mixer_equal?(res = mixer_poly(make_mixer(2, 1, 2, 4, 3), 1, 0, 0), 
                      make_mixer(2, 9, 8, 16, 17))
    snd_display("mixer_poly 4: %s?", res)
  end
  unless mixer_equal?(res = mixer_poly(make_mixer(2, 1, 2, 4, 3), 1, 1, 0), 
                      make_mixer(2, 10, 10, 20, 20))
    snd_display("mixer_poly 5: %s?", res)
  end
  unless mixer_equal?(res = mixer_poly(make_mixer(2, 1, 2, 4, 3), 1, 1, 2), 
                      make_mixer(2, 12, 10, 20, 22))
    snd_display("mixer_poly 6: %s?", res)
  end
  unless mixer_equal?(res = mixer_poly(make_mixer(2, 1, 2, 4, 3), 1, 0, 0, 0), 
                      make_mixer(2, 41, 42, 84, 83))
    snd_display("mixer_poly 7: %s?", res)
  end
  unless mixer_equal?(res = mixer_poly(make_mixer(2, 1, 2, 4, 3), 1, 0, 1, 0), 
                      make_mixer(2, 42, 44, 88, 86))
    snd_display("mixer_poly 8: %s?", res)
  end
end

def test088
  # 
  # try random input to mixer_inverse
  #
  (2...20).each do |k|
    mx = make_random_mixer(k)
    imx = mixer_inverse(mixer_copy(mx))
    mmx = mixer_multiply(mx, imx)
    unless mixer_equal?(mmx, make_scalar_mixer(k, 1.0))
      snd_display("mixer_inverse %d: %s * %s -> %s?", k, mx, imx, mmx)
    end
  end
  unless frame_equal?(res = frame_reverse(make_frame(2, 0.5, 2.0)),
                      make_frame(2, 2.0, 0.5))
    snd_display("frame_reverse 2: %s?", res)
  end
  unless frame_equal?(res = frame_reverse(make_frame(3, 0.5, 1.0, 2.0)),
                      make_frame(3, 2.0, 1.0, 0.5))
    snd_display("frame_reverse 3: %s?", res)
  end
  #
  hi = make_mixer(3, 10, 5, 1, 1, 20, 5, 1, 3, 7)
  ho = make_mixer(3, 10, 5, 2, 1, 3, 2, 1, 3, 2)
  unless mixer_equal?(res = mixer_multiply(hi, ho),
                      make_mixer(3, 106, 68, 32, 35, 80, 52, 20, 35, 22))
    snd_display("mixer_multiply 3x3 1: %s?", res)
  end
  unless mixer_equal?(res = mixer_multiply(hi, mixer_transpose(ho)),
                      make_mixer(3, 127, 27, 27, 120, 71, 71, 39, 24, 24))
    snd_display("mixer_multiply 3x3 2: %s?", res)
  end
  unless mixer_equal?(res = mixer_multiply(mixer_transpose(hi), mixer_transpose(ho)),
                      make_mixer(3, 107, 15, 15, 156, 71, 71, 49, 30, 30))
    snd_display("mixer_multiply 3x3 3: %s?", res)
  end
  unless frame_equal?(res = mixer_solve(make_mixer(2, 0.001, 1, 1, 2), make_frame(2, 1, 3)),
                      make_frame(2, 1.002, 0.999))
    snd_display("mixer_solve G1: %s?", res)
  end
  unless frame_equal?(res = mixer_solve(make_mixer(2, 0.0001, 1, 1, 1), make_frame(2, 1, 3)),
                      make_frame(2, 2, 1))
    snd_display("mixer_solve G2: %s?", res)
  end
  unless frame_equal?(res = mixer_solve(make_mixer(2, 0.986, 0.579, 0.409, 0.237),
                                        make_frame(2, 0.235, 0.107)),
                      make_frame(2, 2, -3))
    snd_display("mixer_solve G3: %s?", res)
  end
  # G4, G5 (invert_matrix) skipped
  unless frame_equal?(res = mixer_solve(make_mixer(3, 1, 4, 7, 2, 5, 8, 3, 6, 10),
                                        make_frame(3, 1, 1, 1)),
                      make_frame(3, -0.333, 0.333, 0))
    snd_display("mixer_solve G6: %s?", res)
  end
  unless frame_equal?(res = mixer_solve(make_mixer(2, 1, 0, 0, 1.0e-6), make_frame(2, 1, 1.0e-6)),
                      make_frame(2, 1, 1))
    snd_display("mixer_solve G7: %s?", res)
  end
  # G8, G9 (invert_matrix) skipped
  unless frame_equal?(res = mixer_solve(make_mixer(2, 10, 100000, 1, 1), make_frame(2, 100000, 2)),
                      make_frame(2, 1, 1))
    snd_display("mixer_solve G10: %s?", res)
  end
  # 
  [[:Hamming_window, 0.0, vct(0.080, 0.115, 0.215, 0.364, 0.540, 0.716, 0.865, 1.000,
                              1.000, 0.865, 0.716, 0.540, 0.364, 0.215, 0.115, 0.080)],
   [:Rectangular_window, 0.0, vct(1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000,
                                  1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000)],
   [:Hann_window, 0.0, vct(0.000, 0.038, 0.146, 0.309, 0.500, 0.691, 0.854, 1.000,
                           1.000, 0.854, 0.691, 0.500, 0.309, 0.146, 0.038, 0.000)],
   [:Welch_window, 0.0, vct(0.000, 0.234, 0.438, 0.609, 0.750, 0.859, 0.938, 1.000,
                            1.000, 0.938, 0.859, 0.750, 0.609, 0.438, 0.234, 0.000)],
   [:Connes_window, 0.0, vct(0.000, 0.055, 0.191, 0.371, 0.562, 0.739, 0.879, 1.000,
                             1.000, 0.879, 0.739, 0.562, 0.371, 0.191, 0.055, 0.000)],
   [:Parzen_window, 0.0, vct(0.000, 0.125, 0.250, 0.375, 0.500, 0.625, 0.750, 1.000,
                             1.000, 0.750, 0.625, 0.500, 0.375, 0.250, 0.125, 0.000)],
   [:Bartlett_window, 0.0, vct(0.000, 0.125, 0.250, 0.375, 0.500, 0.625, 0.750, 1.000,
                               1.000, 0.750, 0.625, 0.500, 0.375, 0.250, 0.125, 0.000)],
   [:Blackman2_window, 0.0, vct(0.005, 0.020, 0.071, 0.177, 0.344, 0.558, 0.775, 1.000,
                                1.000, 0.775, 0.558, 0.344, 0.177, 0.071, 0.020, 0.005)],
   [:Blackman3_window, 0.0, vct(0.000, 0.003, 0.022, 0.083, 0.217, 0.435, 0.696, 1.000,
                                1.000, 0.696, 0.435, 0.217, 0.083, 0.022, 0.003, 0.000)],
   [:Blackman4_window, 0.0, vct(0.002, 0.002, 0.003, 0.017, 0.084, 0.263, 0.562, 1.000,
                                1.000, 0.562, 0.263, 0.084, 0.017, 0.003, 0.002, 0.002)],
   [:Exponential_window, 0.0, vct(0.000, 0.087, 0.181, 0.283, 0.394, 0.515, 0.646, 0.944,
                                  0.944, 0.646, 0.515, 0.394, 0.283, 0.181, 0.087, 0.000)],
   [:Riemann_window, 0.0, vct(0.000, 0.139, 0.300, 0.471, 0.637, 0.784, 0.900, 1.000,
                              1.000, 0.900, 0.784, 0.637, 0.471, 0.300, 0.139, 0.000)],
   [:Kaiser_window, 2.5, vct(0.304, 0.426, 0.550, 0.670, 0.779, 0.871, 0.941, 1.000,
                             1.000, 0.941, 0.871, 0.779, 0.670, 0.550, 0.426, 0.304)],
   [:Cauchy_window, 2.5, vct(0.138, 0.173, 0.221, 0.291, 0.390, 0.532, 0.719, 1.000,
                             1.000, 0.719, 0.532, 0.390, 0.291, 0.221, 0.173, 0.138)],
   [:Poisson_window, 2.5, vct(0.082, 0.112, 0.153, 0.210, 0.287, 0.392, 0.535, 1.000,
                              1.000, 0.535, 0.392, 0.287, 0.210, 0.153, 0.112, 0.082)],
   [:Gaussian_window, 1.0, vct(0.607, 0.682, 0.755, 0.823, 0.882, 0.932, 0.969, 1.000,
                               1.000, 0.969, 0.932, 0.882, 0.823, 0.755, 0.682, 0.607)],
   [:Tukey_window, 0.0, vct(0.000, 0.038, 0.146, 0.309, 0.500, 0.691, 0.854, 1.000,
                            1.000, 0.854, 0.691, 0.500, 0.309, 0.146, 0.038, 0.000)],
   [:Hann_poisson_window, 0.0, vct(0.000, 0.038, 0.146, 0.309, 0.500, 0.691, 0.854, 1.000,
                                   1.000, 0.854, 0.691, 0.500, 0.309, 0.146, 0.038, 0.000)],
   [:Dolph_chebyshev_window, 1.0, vct(1.000, 0.274, 0.334, 0.393, 0.446, 0.491, 0.525, 0.546,
                                      0.553, 0.546, 0.525, 0.491, 0.446, 0.393, 0.334, 0.274)]
  ].each do |win, beta, vals|
    unless vequal(res = make_fft_window(eval("#{win}"), 16, beta), vals)
      snd_display("%s: %s?", win, res)
    end
  end
  #
  gen = make_env(:envelope, [0, 0, 1, 1, 2, 0], :scaler, 0.5, :end, 10)
  gen1 = make_env(:envelope, [0, 0, 1, 1, 2, 0], :scaler, 0.5, :end, 10)
  print_and_check(gen,
                  "env",
                  "env: linear, pass: 0 (dur: 11), index: 0, scaler: 0.5000, offset: 0.0000, data: [0.000 0.000 1.000 1.000 2.000 0.000]")
  snd_display("%s not env?", gen) unless env?(gen)
  snd_display("env scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  snd_display("env base (1.0): %f?", gen.increment) if fneq(gen.increment, 1.0)
  snd_display("env length: %d?", gen.length) if gen.length != 10
  v0 = make_vct!(10) do env(gen) end
  v1 = make_vct(10)
  off = 123.0
  vct_map!(v1, lambda do | |
             off = gen1.offset
             env?(gen1) ? env(gen1) : -1.0
           end)
  snd_display("mus_offset opt: %s?", off) if fneq(off, 0.0)
  snd_display("map env: %s %s?", v0, v1) unless vequal(v0, v1)
  if fneq(v0[0], 0.0) or fneq(v0[1], 0.1) or fneq(v0[6], 0.4)
    snd_display("env output: %s?", v0)
  end
  if fneq(res = env_interp(1.6, gen), 0.2)
    snd_display("env_interp %s at 1.6: %f?", gen, res)
  end
  gen = make_env(:envelope, [0, 1, 1, 0], :base, 32.0, :end, 10)
  snd_display("env base (32.0): %f?", gen.increment) if fneq(gen.increment, 32.0)
  v0.map! do |val| env(gen) end
  if fneq(v0[0], 1.0) or fneq(v0[1], 0.698) or fneq(v0[8], 0.032)
    snd_display("%s output: %s?", gen, v0)
  end
  gen = make_env(:envelope, [0, 1, 1, 0], :base, 0.0325, :end, 10)
  snd_display("env base (0.0325): %f?", gen.increment) if fneq(gen.increment, 0.0325)
  v0.map! do |val| env(gen) end
  if fneq(v0[0], 1.0) or fneq(v0[1], 0.986) or fneq(v0[8], 0.513)
    snd_display("%s output: %s?", gen, v0)
  end
  gen = make_env(:envelope, [0, 1, 1, 0.5, 2, 0], :base, 0.0, :end, 10, :offset, 1.0)
  snd_display("mus_offset: %f?", gen.offset) if fneq(gen.offset, 1.0)
  snd_display("env base (0.0): %f?", gen.increment) if fneq(gen.increment, 0.0)
  v0.map_with_index! do |val, i|
    if i == 3
      if gen.location != 3
        snd_display("env location: %d?", gen.location)
      end
    end
    env(gen)
  end
  if fneq(v0[0], 2.0) or fneq(v0[6], 1.5) or fneq(v0[8], 1.5)
    snd_display("%s output: %s?", gen, v0)
  end
  if fneq(res = env_interp(1.5, gen), 1.5)
    snd_display("env_interp %s at 1.5: %f?", gen, res)
  end
  gen.location = 6
  snd_display("set_mus_location (6): %d?", gen.location) if gen.location != 6
  if fneq(val = env(gen), 1.5)
    snd_display("set_mus_location 6 -> %f (1.5)?", val)
  end
  gen.location = 0
  if fneq(val = env(gen), 2.0)
    snd_display("set_mus_location 0 -> %f (2.0)?", val)
  end
  gen = make_env([0, 0, 1, -1, 2, 0], :end, 10)
  5.times do |i|
    if fneq(val = env(gen), i / -5.0)
      snd_display("neg env: %d %f?", i, val)
    end
  end
  5.times do |i|
    if fneq(val = env(gen), -1.0 + i / 5.0)
      snd_display("neg env: %d %f?", i, val)
    end
  end
  gen = make_env([0, 0, 1, -1, 2, 0], :end, 10, :base, 0.5)
  vct(0.0, -0.14869, -0.31950, -0.51571, -0.74110,
      -1.0, -0.74110, -0.51571, -0.31950, -0.14869).each_with_index do |val, i|
    if fneq(res = env(gen), val)
      snd_display("neg exp env: %d %f?", i, res)
    end
  end
  e = make_env([0, 0, 1, 1], :end, 9)
  if fneq(res = env_interp(1.0, e), 1.0)
    snd_display("env_interp 0011 at 1: %f?", res)
  end
  if fneq(res = env_interp(2.0, e), 1.0)
    snd_display("env_interp 0011 at 2: %f?", res)
  end
  if fneq(res = env_interp(0.0, e), 0.0)
    snd_display("env_interp 0011 at 0: %f?", res)
  end
  if fneq(res = env_interp(0.444, e), 0.444)
    snd_display("env_interp 0011 at 0.444: %f?", res)
  end
  e.reset
  10.times do |i|
    if fneq(val = env(e), i * 0.111111)
      snd_display("ramp env over 10: %f at %d?", val, i)
    end
  end
  e = make_env([0, 0, 0.5, 0.5, 1, 1], :base, 32, :end, 9)
  x = 0.0
  vct(0, 0.0243, 0.0667, 0.1412, 0.2716, 0.5, 0.5958, 0.709, 0.8425, 1).each_with_index do |val, i|
    if fneq(res = env_interp(x, e), val)
      snd_display("[0, 0.5, 1] env_interp over 10: %f at %d (%f)", res, i, val)
    end
    x += 0.111111
  end
  e = make_env([0, -1.0, 1, 1], :base, 32, :end, 9)
  x = 0.0
  vct(-1.0, -0.9697, -0.9252, -0.8597, -0.7635,
      -0.6221, -0.4142, -0.1088, 0.34017, 1.0).each_with_index do |val, i|
    if fneq(res = env_interp(x, e), val)
      snd_display("[-1, 1] env_interp over 10: %f at %d (%f)", res, i, val)
    end
    x += 0.111111
  end
  e = make_env([0, -1.0, 0.5, 0.5, 1, 0], :base, 32, :end, 9)
  x = 0.0
  vct(-1.0, -0.952, -0.855, -0.661, -0.274,
      0.5, 0.356, 0.226, 0.107, 0.0).each_with_index do |val, i|
    if fneq(res = env_interp(x, e), val)
      snd_display("[-1, 0.5, 0] env_interp over 10: %f at %d (%f)", res, i, val)
    end
    x += 0.111111
  end
  e = make_env([0, 0.0, 0.5, 0.5, 1, -1.0], :base, 32, :end, 9)
  x = 0.0
  vct(0, 0.085, 0.177, 0.276, 0.384, 0.5, -0.397, -0.775, -0.933, -1).each_with_index do |val, i|
    if fneq(res = env_interp(x, e), val)
      snd_display("[0, 0.5, -1] env_interp over 10: %f at %d (%f)", res, i, val)
    end
    x += 0.111111
  end
  #
  e = make_env([0, 0, 1, 1], :end, 9, :base, 4)
  if fneq(res = env_interp(1.0, e), 1.0)
    snd_display("env_interp 0011 4 at 1: %f?", res)
  end
  if fneq(res = env_interp(0.0, e), 0.0)
    snd_display("env_interp 0011 4 at 0: %f?", res)
  end
  if fneq(res = env_interp(0.45, e), 0.2839)
    snd_display("env_interp 0011 4 at 0.45: %f?", res)
  end
  e = make_env([0, 0, 1, 1], :end, 9, :base, 0.2)
  if fneq(res = env_interp(1.0, e), 1.0)
    snd_display("env_interp 0011 2 at 1: %f?", res)
  end
  if fneq(res = env_interp(0.0, e), 0.0)
    snd_display("env_interp 0011 2 at 0: %f?", res)
  end
  if fneq(res = env_interp(0.45, e), 0.6387)
    snd_display("env_interp 0011 2 at 0.45: %f?", res)
  end
  e = make_env([0, 0, 1, 1], :offset, 2.0)
  e.offset = 3.0
  snd_display("set_mus_offset env: %f?", e.offset) if fneq(e.offset, 3.0)
  #
  e1 = make_env([0, 0, 1, 1], :base, 32.0, :end, 10)
  vct(0, 0.013, 0.032, 0.059, 0.097, 0.150, 0.226, 0.333, 0.484, 0.698, 1).each do |val|
    if fneq(res = env(e1), val)
      snd_display("exp env direct (32.0): %f %f", res, val)
    end
  end
  e1 = make_env([0, 1, 1, 2], :base, 32.0, :end, 10)
  vct(1, 1.013, 1.032, 1.059, 1.097, 1.15, 1.226, 1.333, 1.484, 1.698, 2).each do |val|
    if fneq(res = env(e1), val)
      snd_display("exp env direct (32.0) offset: %f %f", res, val)
    end
  end
  e1 = make_env([0, 1, 1, 2], :base, 32.0, :dur, 11)
  vct(1, 1.013, 1.032, 1.059, 1.097, 1.15, 1.226, 1.333, 1.484, 1.698, 2).each do |val|
    if fneq(res = env(e1), val)
      snd_display("exp env direct (32.0) offset (and dur): %f %f", res, val)
    end
  end
  e1 = make_env([0, 0, 1, 1], :base, 0.032, :end, 10)
  vct(0.000, 0.301, 0.514, 0.665, 0.772, 0.848, 0.902, 0.940, 0.967, 0.986, 1.0).each do |val|
    if fneq(res = env(e1), val)
      snd_display("exp env direct (0.032): %f %f", res, val)
    end
  end
  #
  e1 = make_env([0, 0, 1, 1], :base, 0.03125, :end, 10)
  e2 = make_env([0, 0, 1, 1, 2, 0], :base, 32.0, :end, 10)
  e3 = make_env([0, 0, 0.1, 1, 2, 0], :base, 1.1, :end, 100)
  10.times do |i|
    lv1 = env_interp(i * 0.1, e1)
    lv2 = env(e1)
    lv3 = env_interp(i * 0.2, e2)
    lv4 = env(e2)
    snd_display("env_interp[rmp %f]: %f (%f)?", i * 0.1, lv1, lv2) if fneq(lv1, lv2)
    snd_display("env_interp[pyr %f]: %f (%f)?", i * 0.2, lv3, lv4) if fneq(lv3, lv4)
  end
  100.times do |i|
    lv5 = env_interp(i * 0.02, e3)
    lv6 = env(e3)
    snd_display("env_interp[tri %f]: %f (%f)?", i * 0.02, lv5, lv6) if fneq(lv5, lv6)
  end
  #
  e1 = make_env([0, 0, 1, 1, 2, 0], :end, 9)
  lv1 = make_vct!(11) do env(e1) end
  lv2 = make_vct!(11) do env(e1) end
  e1.reset
  lv3 = make_vct!(11) do env(e1) end
  snd_display("mus_reset: %s %s?", lv1, lv3) unless vequal(lv1, lv3)
  snd_display("mus_reset 1: %s?", lv2) unless vequal(lv2, make_vct(11))
  #
  gen = make_env([0, 0, 1, 1, 2, 0], :end, 10)
  4.times do env(gen) end
  if fneq(res = env(gen), 0.8)
    snd_display("env(5): %f?", res)
  end
  gen.reset
  4.times do env(gen) end
  if fneq(res = env(gen), 0.8)
    snd_display("mus_reset (via reset): %f?", res)
  end
  gen.location = 6
  if fneq(res = env(gen), 0.8)
    snd_display("set_mus_location 6 -> %f (0.8)?", res)
  end
  gen = make_env([0, 0, 1, 1], :base, 0.032, :end, 11)
  gen.location = 5
  if fneq(res = env(gen), 0.817)
    snd_display("set env location with base: %f %s?", res, gen)
  end
  gen = make_env([0, 0, 1, 1], :base, 0.032, :dur, 12)
  gen.location = 5
  if fneq(res = env(gen), 0.817)
    snd_display("set env location with base and dur: %f %s?", res, gen)
  end
  #
  test_gen_equal(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.50, :end,  9),
                 make_env([0, 0, 1, 1, 2, 0], :scaler, 0.50, :end,  9),
                 make_env([0, 0, 1, 1, 2, 0], :scaler, 0.25, :end,  9))
  test_gen_equal(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.50, :end,  9),
                 make_env([0, 0, 1, 1, 2, 0], :scaler, 0.50, :end,  9),
                 make_env([0, 0, 1, 1, 2, 0], :scaler, 0.50, :end, 10))
  test_gen_equal(make_env([0, 0, 1, 1, 2, 0], :scaler, 0.50, :end,  9),
                 make_env([0, 0, 1, 1, 2, 0], :scaler, 0.50, :end,  9),
                 make_env([0, 0, 1, 1, 3, 0], :scaler, 0.50, :end,  9))
  #
  # Snd threats an empty list [] as nil (Qnil), therefore here will be
  # raised an error :wrong_type-arg not :no_data
  if (res = snd_catch do make_env(:envelope, []) end).first != :wrong_type_arg # :no_data
    snd_display("make_env null env: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:end, 0) end).first != :no_data
    snd_display("make_env no env: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:envelope, [0, 0], :end, -1) end).first != :out_of_range
    snd_display("make_env bad end: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:envelope, [0, 0], :start, -1) end).first != :out_of_range
    snd_display("make_env bad start: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:envelope, [0, 0], :dur, -1) end).first != :out_of_range
    snd_display("make_env bad dur: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:envelope, [0, 0], :duration, -1.0) end).first != :out_of_range
    snd_display("make_env bad duration: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:envelope, [0, 0], :base, -1.0) end).first != :out_of_range
    snd_display("make_env bad base: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:envelope, [1, 1, 0, 0], :end, 10) end).first != :mus_error
    snd_display("make_env bad env 1 1 0 0: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:envelope, [0, 1, -1, 0], :end, 10) end).first != :mus_error
    snd_display("make_env bad env 0 1 -1 0: %s", res.inspect)
  end
  if (res = snd_catch do make_env(:envelope, [0, 1, 1, 0], :end, 10, :dur, 10) end).first != :mus_error
    snd_display("make_env bad end/dur: %s", res.inspect)
  end
end

def test098
  gen = make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 1]))
  gen1 = make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 1], make_vct(512)))
  gen2 = partials2wave([1, 1, 2, 1, 3, 1, 4, 1], false, true)
  gen3 = make_table_lookup
  gen4 = make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 1]))
  print_and_check(gen,
                  "table-lookup",
                  "table-lookup: freq: 440.000Hz, phase: 0.000, length: 512, interp: linear")
  snd_display("table_lookup length: %d?", gen.length) if gen.length != 512
  snd_display("default table_lookup length: %d?", gen3.length) if gen3.length != 512
  v0 = make_vct!(10) do table_lookup(gen, 0.0) end
  v1 = make_vct!(10) do mus_apply(gen1, 0.0) end
  v2 = make_vct(10)
  vct_map!(v2, lambda do | | table_lookup?(gen4) ? table_lookup(gen4) : -1.0 end)
  snd_display("map table_lookup: %s %s?", v0, v2) unless vequal(v0, v2)
  gen4 = make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 1]))
  vct_map!(v2, lambda do | | table_lookup(gen4) end)
  snd_display("map table_lookup (no fm): %s %s?", v0, v2) unless vequal(v0, v2)
  snd_display("%s not table_lookup?", gen) unless table_lookup?(gen)
  snd_display("mus_data table_lookup: %s?", gen.data) unless vct?(gen.data)
  snd_display("table_lookup phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  gen.phase = 1.0
  snd_display("table_lookup set_phase: %f?", gen.phase) if fneq(gen.phase, 1.0)
  snd_display("table_lookup frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  gen.frequency = 100.0
  snd_display("table_lookup set_frequency: %f?", gen.frequency) if fneq(gen.frequency, 100.0)
  if fneq(v0[1], 0.373) or fneq(v0[8], 1.75)
    snd_display("table_lookup output: %s?", v0)
  end
  snd_display("mus_apply table_lookup: %s %s?", v0, v1) unless vequal(v0, v1)
  gen = make_table_lookup(440.0, :wave, phase_partials2wave([1, 1, 0, 2, 1, HALF_PI]))
  v0.map! do |val| table_lookup(gen, 0.0) end
  if fneq(v0[1], 1.094) or fneq(v0[8], 0.421)
    snd_display("table_lookup phase output: %s?", v0)
  end
  if fneq(vct_peak(partials2wave([1, 1, 2, 1])), 1.76035475730896) or
      fneq(vct_peak(partials2wave([1, 1, 2, 1], false, true)), 1.0) or
      fneq(vct_peak(partials2wave([1, 1, 2, 1, 3, 1, 4, 1], false, true)), 1.0)
    snd_display("normalized partials?")
  end
  gen.data = phase_partials2wave([1, 1, 0, 2,1, HALF_PI], false, true)
  #
  test_gen_equal(make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 1])),
                 make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 1])),
                 make_table_lookup(100.0, :wave, partials2wave([1, 1, 2, 1])))
  test_gen_equal(make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 1])),
                 make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 1])),
                 make_table_lookup(440.0, :wave, partials2wave([1, 1, 2, 0.5])))
  #
  hi = make_table_lookup(:size, 256)
  snd_display("table_lookup set length: %d?", hi.length) if hi.length != 256
  if (res = snd_catch do make_table_lookup(:size, 0) end).first != :out_of_range
    snd_display("table_lookup size 0: %s", res.inspect)
  end
  gen = make_table_lookup(440.0, :wave, partials2wave([1, 1]))
  a = 0.0
  1100.times do |i|
    if fneq(val1 = sin(a), val2 = gen.run(0.0))
      snd_display("table_lookup [1, 1]: %d: %f %f?", i, val1, val2)
    end
    a += (TWO_PI * 440.0) / 22050.0
  end
  gen = make_table_lookup(4.0, :wave, partials2wave([1, 1]))
  a = 0.0
  1100.times do |i|
    if fneq(val1 = sin(a), val2 = gen.run(0.0))
      snd_display("table_lookup [1, 1] 4: %d: %f %f?", i, val1, val2)
    end
    a += (TWO_PI * 4.0) / 22050.0
  end
  gen = make_table_lookup(440.0, :wave, partials2wave([1, 0.75, 3, 0.25]))
  a = 0.0
  1100.times do |i|
    val1 = 0.75 * sin(a) + 0.25 * sin(3.0 * a)
    if fneq(val1, val2 = gen.run(0.0))
      snd_display("table_lookup [1, 0.75, 3, 0.25]: %d: %f %f?", i, val1, val2)
    end
    a += (TWO_PI * 440.0) / 22050.0
  end
  gen = make_table_lookup(0.0, :wave, partials2wave([1, 1]))
  gen1 = make_table_lookup(40.0, :wave, partials2wave([1, 1]))
  a = 0.0
  a1 = 0.0
  100.times do |i|
    if fneq(val1 = sin(a1), val2 = gen.run(gen1.run(0.0)))
      snd_display("table_lookup/table_lookup fm: %d: %f %f?", i, val1, val2)
    end
    a1 += sin(a)
    a += (TWO_PI * 40.0) / 22050.0
  end
  [[:Mus_interp_none, vct(0, 0.000, 0.000, 0.000, 0.000, 1.000, 1.000, 1.000, 1.000, 1.000)],
    [:Mus_interp_linear, vct(0, 0.200, 0.400, 0.600, 0.800, 1.000, 0.800, 0.600, 0.400, 0.200)],
    [:Mus_interp_lagrange, vct(0, 0.120, 0.280, 0.480, 0.720, 1.000, 0.960, 0.840, 0.640, 0.360)],
    [:Mus_interp_all_pass, vct(1, 0.000, 0.429, 0.143, 0.095, 0.905, 0.397, 0.830, 0.793, 0.912)],
    [:Mus_interp_hermite, vct(0, 0.168, 0.424, 0.696, 0.912, 1.000, 0.912, 0.696, 0.424, 0.168)]
  ].each do |type_sym, vals|
    type = eval("#{type_sym}")
    tbl = make_table_lookup(:frequency, 0.0, :size, 4, :type, type)
    tbl.data[1] = 1.0
    fm = (TWO_PI * 0.2) / 4.0
    v = make_vct!(10) do table_lookup(tbl, fm) end
    snd_display("tbl interp %s: %s?", type_sym, v) unless vequal(v, vals)
    snd_display("tbl interp_type (%s) %d?", type_sym, tbl.interp_type) if tbl.interp_type != type
  end
  #
  gen0 = make_waveshape(440.0, :wave, partials2waveshape([1, 1]))
  gen = make_waveshape(440.0, :size, 512, :partials, [1, 1])
  gen1 = make_waveshape(440.0, :wave, partials2waveshape([1, 1]))
  print_and_check(gen, "waveshape", "waveshape freq: 440.000Hz, phase: 0.000, size: 512")
  snd_display("waveshape length: %d?", gen.length) if gen.length != 512
  v0 = make_vct!(10) do
    if fneq(val0 = waveshape(gen0, 1.0, 0.0), val = mus_apply(gen, 1.0, 0.0))
      snd_display("waveshape: %f != %f?", val, val0)
    end
    val
  end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | waveshape?(gen1) ? waveshape(gen1, 1.0, 0.0) : -1.0 end)
  snd_display("map waveshape: %s %s?", v0, v1) unless vequal(v0, v1)
  gen1 = make_waveshape(440.0, :wave, partials2waveshape([1, 1]))
  vct_map!(v1, lambda do | | waveshape(gen1, 1.0) end)
  snd_display("map waveshape (no fm): %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not waveshape?", gen) unless waveshape?(gen)
  snd_display("waveshape phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  gen.phase = 1.0
  snd_display("waveshape set_phase: %f?", gen.phase) if fneq(gen.phase, 1.0)
  snd_display("waveshape frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  gen.frequency = 100.0
  snd_display("waveshape set_frequency: %f?", gen.frequency) if fneq(gen.frequency, 100.0)
  snd_display("mus_data waveshape: %s?", gen.data) unless vct?(gen.data)
  if fneq(v0[1], 0.125) or fneq(v0[8], 0.843)
    snd_display("waveshape output: %s?", v0)
  end
  gen0.data = make_vct(32)
  gen0.length = 32
  snd_display("set_mus_length waveshape: %d?", gen0.length) if gen0.length != 32
  #
  test_gen_equal(make_waveshape(440.0, :partials, [1, 1]),
                 make_waveshape(440.0, :partials, [1, 1]),
                 make_waveshape(100.0, :partials, [1, 1]))
  test_gen_equal(make_waveshape(440.0, :partials, [1, 1]),
                 make_waveshape(440.0, :partials, [1, 1]),
                 make_waveshape(4400.0, :partials, [1, 1, 2, 0.5]))
  #
  unless vequal(d11 = partials2waveshape([1, 1], 16),
            vct(-1.000, -0.867, -0.733, -0.600, -0.467, -0.333, -0.200, -0.067,
                0.067, 0.200, 0.333, 0.467, 0.600, 0.733, 0.867, 1.000))
    snd_display("partials2waveshape 1 1: %s?", d11)
  end
  unless vequal(d11 = partials2waveshape([2, 1], 16),
            vct(1.000, 0.502, 0.076, -0.280, -0.564, -0.778, -0.920, -0.991,
                -0.991, -0.920, -0.778, -0.564, -0.280, 0.076, 0.502, 1.000))
    snd_display("partials2waveshape 2 1: %s?", d11)
  end
  #
  gen = make_waveshape(440.0, :partials, [1, 1])
  1100.times do |i|
    a = gen.phase
    if fneq(val1 = sin(a), val2 = gen.run(1.0, 0.0))
      snd_display("waveshaper [1, 1] %d: %f %f?", i, val1, val2)
      break
    end
  end
  gen = make_waveshape(440.0) # check default for partials: [1, 1])
  1100.times do |i|
    a = gen.phase
    if fneq(val1 = sin(a), val2 = gen.run(1.0, 0.0))
      snd_display("waveshaper default [1, 1] %d: %f %f?", i, val1, val2)
      break
    end
  end
  gen = make_waveshape(440.0, :partials, [2, 1])
  incr = (TWO_PI * 440.0) / mus_srate()
  a = 0.0
  1100.times do |i|
    if fneq(val1 = sin(-HALF_PI + 2.0 * a), val2 = gen.run(1.0, 0.0))
      snd_display("waveshaper [2, 1] %d: %f %f?", i, val1, val2)
      break
    end
    a += incr
  end
  gen = make_waveshape(440.0, :partials, [1, 1, 2, 0.5])
  incr = (TWO_PI * 440.0) / mus_srate()
  a = 0.0
  1100.times do |i|
    a = gen.phase
    val1 = (1.0 / 1.5) * (sin(a) + 0.5 * sin(-HALF_PI + 2.0 * a))
    if fneq(val1, val2 = gen.run(1.0, 0.0))
      snd_display("waveshaper [1, 1, 2, 0.5] %d: %f %f?", i, val1, val2)
      break
    end
    a += incr
  end
  gen = make_waveshape(440.0, :partials, [1, 1])
  1100.times do |i|
    a = gen.phase
    if fneq(val1 = 0.5 * sin(a), val2 = gen.run(0.5, 0.0))
      snd_display("waveshaper [1, 1] 0.5 %d: %f %f?", i, val1, val2)
      break
    end
  end
  #
  if (res = snd_catch do
        make_waveshape(440.0, :partials, [1, 1], :size, false)
      end).first != :wrong_type_arg
    snd_display("make_waveshape bad size: %s", res.inspect)
  end
  if (res = snd_catch do make_waveshape(440.0, :wave, 3.14) end).first != :wrong_type_arg
    snd_display("make_waveshape bad wave: %s", res.inspect)
  end
  if (res = snd_catch do make_waveshape(440.0, :size, 0) end).first != :out_of_range
    snd_display("make_waveshape bad size: %s", res.inspect)
  end
  let(make_waveshape(:size, 256)) do |hi|
    snd_display("waveshape set_length: %d?", hi.length) if hi.length != 256
  end
  gen = make_waveshape(0.0, :wave, partials2waveshape([1, 1]))
  gen1 = make_waveshape(40.0, :wave, partials2waveshape([1, 1]))
  a1 = 0.0
  a = 0.0
  400.times do |i|
    if ((val1 = sin(a1)) - (val2 = waveshape(gen, 1.0, waveshape(gen1, 1.0)))).abs > 0.002
      snd_display("waveshape fm: %d: %f %f?", i, val1, val2)
    end
    a1 += sin(a)
    a += (TWO_PI * 40.0) / 22050.0
  end
  #
  gen0 = make_polyshape(440.0, :coeffs, partials2polynomial([1, 1]))
  gen = make_polyshape(440.0, :partials, [1, 1], :kind, Mus_chebyshev_first_kind)
  gen1 = make_polyshape(440.0)
  print_and_check(gen,
                  "polyshape",
                  "polyshape freq: 440.000Hz, phase: 0.000, coeffs[2]: [0.000 1.000]")
  snd_display("polyshape length: %d?", gen.length) if gen.length != 2
  v0 = make_vct!(10) do
    if fneq(val0 = polyshape(gen0, 1.0, 0.0), val = mus_apply(gen, 1.0, 0.0))
      snd_display("polyshape: %f != %f?", val, val0)
    end
    val
  end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | polyshape?(gen1) ? polyshape(gen1, 1.0, 0.0) : -1.0 end)
  snd_display("map polyshape: %s %s?", v0, v1) unless vequal(v0, v1)
  gen1 = make_polyshape(440.0, :coeffs, partials2polynomial([1, 1]))
  vct_map!(v1, lambda do | | polyshape(gen1, 1.0) end)
  snd_display("map polyshape (no fm): %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not polyshape?", gen) unless polyshape?(gen)
  snd_display("polyshape phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  gen.phase = 1.0
  snd_display("polyshape set_phase: %f?", gen.phase) if fneq(gen.phase, 1.0)
  snd_display("polyshape frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  gen.frequency = 100.0
  snd_display("polyshape set_frequency: %f?", gen.frequency) if fneq(gen.frequency, 100.0)
  snd_display("mus_data polyshape: %s?", gen.data) unless vct?(gen.data)
  if fneq(v0[1], 0.125) or fneq(v0[8], 0.843)
    snd_display("polyshape output: %s?", v0)
  end
  gen0.data = make_vct(32)
  gen0.length = 32
  snd_display("set_mus_length polyshape: %d?", gen0.length) if gen0.length != 32
  #
  test_gen_equal(make_polyshape(440.0, :partials, [1, 1]),
                 make_polyshape(440.0),
                 make_polyshape(100.0, :partials, [1, 1]))
  test_gen_equal(make_polyshape(440.0, :partials, [1, 1]),
                 make_polyshape(440.0),
                 make_polyshape(4400.0, :partials, [1, 1, 2, 0.5]))
  #
  gen = make_polyshape(440.0, :partials, [1, 1])
  1100.times do |i|
    a = gen.phase
    if fneq(val1 = sin(a), val2 = gen.run(1.0, 0.0))
      snd_display("polyshaper [1, 1] %d: %f %f?", i, val1, val2)
      break
    end
  end
  gen = make_polyshape(440.0) # check default for partials: [1, 1])
  1100.times do |i|
    a = gen.phase
    if fneq(val1 = sin(a), val2 = gen.run(1.0, 0.0))
      snd_display("polyshaper default [1, 1] %d: %f %f?", i, val1, val2)
      break
    end
  end
  gen = make_polyshape(440.0, :initial_phase, HALF_PI, :partials, [2, 1])
  incr = (TWO_PI * 440.0) / mus_srate()
  a = 0.0
  1100.times do |i|
    if fneq(val1 = cos(2.0 * a), val2 = gen.run(1.0, 0.0))
      snd_display("polyshaper [2, 1] %d: %f %f?", i, val1, val2)
      break
    end
    a += incr
  end
  gen = make_polyshape(440.0, :initial_phase, HALF_PI, :partials, [1, 1, 2, 0.5])
  incr = (TWO_PI * 440.0) / mus_srate()
  a = 0.0
  1100.times do |i|
    val1 = cos(a) + 0.5 * cos(2.0 * a)
    if fneq(val1, val2 = gen.run(1.0, 0.0))
      snd_display("polyshaper [1, 1, 2, 0.5] %d: %f %f?", i, val1, val2)
      break
    end
    a += incr
  end
  gen = make_polyshape(440.0, :partials, [1, 1])
  1100.times do |i|
    a = gen.phase
    if fneq(val1 = 0.5 * sin(a), val2 = gen.run(0.5, 0.0))
      snd_display("polyshaper [1, 1] 0.5 %d: %f %f?", i, val1, val2)
      break
    end
  end
  #
  if (res = snd_catch do make_polyshape(440.0, :coeffs, 3.14) end).first != :wrong_type_arg
    snd_display("make_polyshape bad coeffs: %s", res.inspect)
  end
  gen = make_polyshape(0.0, :coeffs, partials2polynomial([1, 1]))
  gen1 = make_polyshape(40.0, :coeffs, partials2polynomial([1, 1]))
  a1 = 0.0
  a = 0.0
  400.times do |i|
    if ((val1 = sin(a1)) - (val2 = polyshape(gen, 1.0, polyshape(gen1, 1.0)))).abs > 0.002
      snd_display("polyshape fm: %d: %f %f?", i, val1, val2)
      break
    end
    a1 += sin(a)
    a += (TWO_PI * 40.0) / 22050.0
  end
end

def test108
  gen = make_wave_train(440.0, 0.0, make_vct(20))
  gen1 = make_wave_train(440.0, 0.0, make_vct(20))
  print_and_check(gen,
                  "wave-train",
                  "wave-train freq: 440.000Hz, phase: 0.000, size: 20, interp: linear")
  20.times do |i|
    gen.data[i] = i * 0.5
    gen1.data[i] = gen.data[i]
  end
  snd_display("wave_train length: %d?", gen.length) if gen.length != 20
  v0 = make_vct!(10) do wave_train(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | wave_train?(gen1) ? wave_train(gen1) : -1.0 end)
  snd_display("map wave_train: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not wave_train?", gen) unless wave_train?(gen)
  snd_display("wave_train phase: %f?", gen.phase) if fneq(gen.phase, 0.0)
  gen.phase = 1.0
  snd_display("wave_train set_phase: %f?", gen.phase) if fneq(gen.phase, 1.0)
  snd_display("wave_train frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  gen.frequency = 100.0
  snd_display("wave_train set_frequency: %f?", gen.frequency) if fneq(gen.frequency, 100.0)
  if fneq(v0[1], 0.5) or fneq(v0[8], 4.0)
    snd_display("wave_train output: %s?", v0)
  end
  gen.reset
  snd_display("wave_train reset phase: %f?", gen.phase) if fneq(gen.phase, 0.0)
  if fneq(res = wave_train(gen, 0.0), 0.0)
    snd_display("wave_train data: %f?", res)
  end
  snd_display("mus_data wave_train: %s?", gen.data) unless vct?(gen.data)
  gen.data = make_vct(3)
  make_oscil().data = make_vct(3)
  #
  test_gen_equal(make_wave_train(440.0, 0.0, make_vct(20)),
                 make_wave_train(440.0, 0.0, make_vct(20)),
                 make_wave_train(100.0, 0.0, make_vct(20)))
  test_gen_equal(make_wave_train(440.0, 0.0, make_vct(20)),
                 make_wave_train(440.0, 0.0, make_vct(20)),
                 make_wave_train(440.0, 1.0, make_vct(20)))
  #
  let(make_wave_train(:size, 256)) do |hi|
    snd_display("wave_train set_length: %d?", hi.length) if hi.length != 256
    hi.length = 128
    snd_display("wave_train set_length: %d?", hi.length) if hi.length != 128
  end
  [[:Mus_interp_none, vct(0.000, 1.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 1.000)],
    [:Mus_interp_linear, vct(0.200, 0.800, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.200, 0.800)],
    [:Mus_interp_lagrange, vct(0.120, 0.960, -0.080, 0.000, 0.0, 0.0, 0.0, 0.0, 0.120, 0.960)],
    [:Mus_interp_hermite, vct(0.168, 0.912, -0.064, -0.016, 0.0, 0.0, 0.0, 0.000, 0.168, 0.912)]
  ].each do |type_sym, vals|
    type = eval("#{type_sym}")
    fm = (TWO_PI * 0.2) / 4.0
    tbl = make_wave_train(:frequency, 3000.0, :initial_phase, fm, :size, 4, :type, type)
    tbl.data[1] = 1.0
    v = make_vct!(10) do wave_train(tbl, 0.0) end
    snd_display("wt tbl interp %s: %s?", type_sym, v) unless vequal(v, vals)
    snd_display("wt tbl interp_type (%s) %d?", type_sym, tbl.interp_type) if tbl.interp_type != type
  end
  if (res = snd_catch do make_wave_train(:size, 0) end).first != :out_of_range
    snd_display("wave_train size 0: %s", res.inspect)
  end
  #
  ind = new_sound(:size, 1000)
  table = vct(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  gen = make_wave_train(1000.0, :wave, table)
  map_channel(lambda do |y| wave_train(gen) end)
  if fneq(res = maxamp, 0.6)
    snd_display("wave_train 0 max: %s?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.100, 0.200, 0.300, 0.400, 0.500, 0.600, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.100, 0.200, 0.300, 0.400, 0.500, 0.600))
    snd_display("wave_train 0 data: %s?", res)
  end
  unless vequal(res = channel2vct(85, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.100, 0.200, 0.300,
                    0.400, 0.500, 0.600, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.100, 0.200, 0.300))
    snd_display("wave_train 0 data 85: %s?", res)
  end
  undo_edit
  #
  table = make_vct(10, 0.1)
  gen = make_wave_train(1000.0, :initial_phase, 3.14159, :wave, table)
  map_channel(lambda do |y| wave_train(gen) end)
  if fneq(res = maxamp, 0.1)
    snd_display("wave_train 1 max: %s?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.000))
    snd_display("wave_train 1 data: %s?", res)
  end
  undo_edit
  #
  table = make_vct(10, 0.1)
  gen = make_wave_train(2000.0, :wave, table)
  map_channel(lambda do |y| wave_train(gen) end)
  if fneq(res = maxamp, 0.1)
    snd_display("wave_train 2 max: %s?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.000, 0.000, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.000, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100))
    snd_display("wave_train 2 data: %s?", res)
  end
  # INFO: snd-test.scm awaits this result but snd-ruby as well as
  # snd-guile result in that below
  #
  # set_print_length([30, print_length].max)
  # unless vequal(res = channel2vct(440, 30),
  #               vct(0.000, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
  #                   0.100, 0.100, 0.100, 0.000, 0.000, 0.100, 0.100, 0.100,
  #                   0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.000,
  #                   0.100, 0.100, 0.100, 0.100, 0.100, 0.100))
  #   snd_display("wave_train 2 data 440: %s?", res)
  # end
  unless vequal(res = channel2vct(440, 30),
                vct(0.000, 0.000, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.000, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.000,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100))
    snd_display("wave_train 2 data 440: %s?", res)
  end
  undo_edit
  # 
  table = make_vct(10, 0.1)
  gen = make_wave_train(3000.0, :wave, table)
  map_channel(lambda do |y| wave_train(gen) end)
  if fneq(res = maxamp, 0.2)
    snd_display("wave_train 3 max: %s?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.200, 0.200, 0.100, 0.100, 0.100, 0.100, 0.100, 0.200,
                    0.200, 0.200, 0.100, 0.100, 0.100, 0.100, 0.100, 0.200,
                    0.200, 0.100, 0.100, 0.100, 0.100, 0.100))
    snd_display("wave_train 3 data: %s?", res)
  end
  unless vequal(res = channel2vct(440, 30),
                vct(0.100, 0.200, 0.200, 0.200, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.200, 0.200, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.200, 0.200, 0.200, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.200, 0.200, 0.100, 0.100, 0.100, 0.100))
    snd_display("wave_train 3 data 440: %s?", res)
  end
  undo_edit
  # 
  table = make_vct(10, 0.1)
  gen = make_wave_train(5000.0, :wave, table)
  map_channel(lambda do |y| wave_train(gen) end)
  if fneq(res = maxamp, 0.3)
    snd_display("wave_train 4 max: %s?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.200, 0.200, 0.200,
                    0.200, 0.300, 0.200, 0.200, 0.200, 0.200, 0.300, 0.200,
                    0.200, 0.200, 0.300, 0.200, 0.200, 0.200, 0.200, 0.300,
                    0.200, 0.200, 0.200, 0.300, 0.200, 0.200))
    snd_display("wave_train 4 data: %s?", res)
  end
  unless vequal(res = channel2vct(440, 30),
                vct(0.200, 0.200, 0.300, 0.200, 0.200, 0.200, 0.300, 0.200,
                    0.200, 0.200, 0.300, 0.300, 0.200, 0.200, 0.200, 0.300,
                    0.200, 0.200, 0.200, 0.300, 0.200, 0.200, 0.200, 0.200,
                    0.300, 0.200, 0.200, 0.200, 0.300, 0.200))
    snd_display("wave_train 4 data 440: %s?", res)
  end
  undo_edit
  # 
  table = make_vct(10, 0.1)
  gen = make_wave_train(1000.0, :wave, table)
  e = make_env([0, 1, 1, 2], :end, 1000)
  base_freq = mus_frequency(gen)
  map_channel(lambda do |y|
                res = wave_train(gen)
                set_mus_frequency(gen, env(e) * base_freq)
                res
              end)
  if fneq(res = maxamp, 0.1)
    snd_display("wave_train 5 max: %s?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100))
    snd_display("wave_train 5 data: %s?", res)
  end
  unless vequal(res = channel2vct(440, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.100))
    snd_display("wave_train 5 data 440: %s?", res)
  end
  unless vequal(res = channel2vct(900, 30),
                vct(0.100, 0.000, 0.000, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.000, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.000, 0.000, 0.100, 0.100, 0.100, 0.100))
    snd_display("wave_train 5 data 900: %s?", res)
  end
  undo_edit
  # 
  table = make_vct(10, 0.1)
  gen = make_wave_train(500.0, :wave, table)
  ctr = 0
  map_channel(lambda do |y|
                res = wave_train(gen)
                if ctr > 22
                  ctr = 0
                  vct_scale!(mus_data(gen), 1.05)
                else
                  ctr += 1
                end
                res
              end)
  if fneq(res = maxamp, 0.704)
    snd_display("wave_train 6 max: %s?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("wave_train 6 data: %s?", res)
  end
  # INFO: snd-test.scm awaits this result but snd-ruby as well as
  # snd-guile result in that below
  #
  # set_print_length([30, print_length].max)
  # unless vequal(res = channel2vct(440, 30),
  #               vct(0.000, 0.241, 0.241, 0.241, 0.241, 0.241, 0.241, 0.241,
  #                   0.241, 0.241, 0.241, 0.000, 0.000, 0.000, 0.000, 0.000,
  #                   0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
  #                   0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
  #   snd_display("wave_train 6 data 440: %s?", res)
  # end
  unless vequal(res = channel2vct(440, 30),
                vct(0.000, 0.000, 0.241, 0.241, 0.241, 0.241, 0.241, 0.241,
                    0.241, 0.241, 0.241, 0.241, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("wave_train 6 data 440: %s?", res)
  end
  unless vequal(res = channel2vct(900, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.639, 0.639, 0.639))
    snd_display("wave_train 6 data 900: %s?", res)
  end
  undo_edit
  fname = file_name(ind)
  close_sound(ind)
  delete_file(fname)
  #
  gen = make_readin("oboe.snd", 0, 1490)
  gen1 = make_readin("oboe.snd", 0, 1490)
  print_and_check(gen, "readin", "readin: oboe.snd[chan 0], loc: 1490, dir: 1")
  v0 = make_vct!(10) do readin(gen) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | |
             if readin?(gen1)
               if gen1.channel.zero?
                 readin(gen1)
               else
                 1.0
               end
             else
               if gen1.file_name == "oboe.snd"
                 -1.0
               else
                 -1.0
               end
             end
           end)
  snd_display("map readin: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not readin?", gen) unless readin?(gen)
  snd_display("%s not input?", gen) unless mus_input?(gen)
  snd_display("readin length: %s?", gen.length) if gen.length != 50828
  snd_display("readin chan: %s?", gen.channel) if gen.channel.nonzero?
  snd_display("readin mus_file_name: %s?", gen.file_name) if gen.file_name != "oboe.snd"
  if fneq(v0[1], -0.009) or fneq(v0[7], 0.029)
    snd_display("readin output: %s?", v0)
  end
  set_mus_location(gen, 1000)
  snd_display("readin set_mus_location: %s?", mus_location(gen)) if mus_location(gen) != 1000
  if fneq(res = readin(gen), 0.033)
    snd_display("readin set_mus_location data: %f?", res)
  end
  set_mus_increment(gen, -1)
  snd_display("readin set_mus_increment: %s?", mus_increment(gen)) if fneq(mus_increment(gen), -1.0)
  if (res = snd_catch do make_readin("/baddy/hiho", 0, 124) end).first != :no_such_file
    snd_display("make_readin w/o file: %s", res.inspect)
  end
  if (res = snd_catch do make_readin("oboe.snd", 123, 124) end).first != :out_of_range
    snd_display("make_readin with bad chan: %s", res.inspect)
  end
  #
  test_gen_equal(make_readin("oboe.snd", 0),
                 make_readin("oboe.snd", 0),
                 make_readin("oboe.snd", 0, 1230))
  test_gen_equal(make_readin("oboe.snd", 0, :size, 512),
                 make_readin("oboe.snd", 0, :size, 512),
                 make_readin("pistol.snd", 0, :size, 512))
  test_gen_equal(make_readin("2.snd", 1),
                 make_readin("2.snd", 1),
                 make_readin("2.snd", 0))
  #
  gen = make_readin("2.snd", 1, :size, 1024)
  print_and_check(gen, "readin", "readin: 2.snd[chan 1], loc: 0, dir: 1")
  v0.map! do readin(gen) end
  snd_display("readin chan 1: %s?", gen.channel) if gen.channel != 1
  if fneq(v0[1], 0.01) or fneq(v0[7], -0.006)
    snd_display("readin 1 output: %s?", v0)
  end
  print_and_check(gen, "readin", "readin: 2.snd[chan 1], loc: 10, dir: 1")
  # 
  gen = make_file2sample("oboe.snd")
  print_and_check(gen, "file->sample", "file->sample: oboe.snd")
  snd_display("%s not file2sample?", gen) unless file2sample?(gen)
  snd_display("%s not input?", gen) unless mus_input?(gen)
  snd_display("file2sample length: %s?", gen.length) if gen.length != 50828
  snd_display("file2sample mus_file_name: %s?", gen.file_name) if gen.file_name != "oboe.snd"
  v0 = make_vct!(10) do |i| file2sample(gen, 1490 + i) end
  if fneq(v0[1], -0.009) or fneq(v0[7], 0.029)
    snd_display("file2sample output: %s?", v0)
  end
  snd_display("file2sample increment: %s?", mus_increment(gen)) if fneq(mus_increment(gen), 0.0)
  set_mus_increment(gen, 1)
  snd_display("file2sample set_increment: %s?", mus_increment(gen)) if fneq(mus_increment(gen), 1.0)
  #
  ind = open_sound("oboe.snd")
  gen = make_snd2sample(ind)
  gen1 = make_snd2sample(ind)
  print_and_check(gen, "snd->sample", "snd->sample: reading oboe.snd (1 chan) at 0:[no readers]")
  snd_display("snd2sample not eql? itself?") unless gen.eql?(gen)
  snd_display("snd2sample eql? not itself?") if gen.eql?(gen1)
  snd_display("%s not snd2sample?", gen) unless snd2sample?(gen)
  snd_display("%s not input?", gen) unless mus_input?(gen)
  snd_display("snd2sample length: %s?", gen.length) if gen.length != frames(ind)
  if gen.file_name != (Dir.pwd + "/oboe.snd")
    snd_display("snd2sample mus_file_name: %s?", gen.file_name)
  end
  v0 = make_vct!(10) do |i| snd2sample(gen, 1490 + i) end
  if fneq(v0[1], -0.009) or fneq(v0[7], 0.029)
    snd_display("snd2sample output: %s?", v0)
  end
  snd_display("snd2sample channels: %s?", mus_channels(gen)) if mus_channels(gen) != 1
  snd_display("snd2sample location: %s?", mus_location(gen)) if mus_location(gen) != 1499
  v0.map_with_index! do |val, i| ina(1490 + i, gen) end
  if fneq(v0[1], -0.009) or fneq(v0[7], 0.029)
    snd_display("snd2sample output ina: %s?", v0)
  end
  close_sound(ind)
  # 
  ind = open_sound("2.snd")
  gen = make_snd2sample(ind)
  v0 = make_vct!(10) do |i|
    snd2sample(gen, 1490 + i, 0)
    snd2sample(gen, 1490 + i, 1)
  end
  print_and_check(gen,
                  "snd->sample",
                  "snd->sample: reading 2.snd (2 chans) at 1499:[#<sample-reader: 2.snd[0: 0] from 1490, at 1500>, #<sample-reader: 2.snd[1: 0] from 1490, at 1500>]")
  snd_display("%s not snd2sample?", gen) unless snd2sample?(gen)
  snd_display("%s not input?", gen) unless mus_input?(gen)
  snd_display("snd2sample length: %s?", gen.length) if gen.length != frames(ind)
  if gen.file_name != (Dir.pwd + "/2.snd")
    snd_display("snd2sample mus_file_name: %s?", gen.file_name)
  end
  snd_display("snd2sample channels (2): %s?", mus_channels(gen)) if mus_channels(gen) != 2
  snd_display("snd2sample location (2): %s?", mus_location(gen)) if mus_location(gen) != 1499
  close_sound(ind)
end

def test118
  fgen = make_file2sample("oboe.snd", 512)
  #
  gen = make_file2frame("oboe.snd")
  print_and_check(gen, "file->frame", "file->frame: oboe.snd")
  snd_display("%s not file2frame?", gen) unless file2frame?(gen)
  snd_display("%s not input?", gen) unless mus_input?(gen)
  snd_display("file2frame length: %s?", gen.length) if gen.length != 50828
  v0 = make_vct!(10) do |i| frame_ref(file2frame(gen, 1490 + i, 0), 0) end
  snd_display("%s not file2frame?", gen) unless file2frame?(gen)
  snd_display("file2frame mus_file_name: %s?", gen.file_name) if gen.file_name != "oboe.snd"
  if fneq(v0[1], -0.009) or fneq(v0[7], 0.029)
    snd_display("file2frame output: %s?", v0)
  end
  # 
  delete_files("fmv.snd", "fmv1.snd", "fmv2.snd", "fmv3.snd")
  gen = make_sample2file("fmv.snd", 2, Mus_lshort, Mus_riff)
  print_and_check(gen, "sample->file", "sample->file: fmv.snd")
  snd_display("%s not sample2file?", gen) unless sample2file?(gen)
  snd_display("%s not output?", gen) unless mus_output?(gen)
  snd_display("sample2file length: %s?", gen.length) if gen.length != mus_file_buffer_size
  genx = gen
  snd_display("sample2file eql? %s %s", genx, gen) unless gen.eql?(genx)
  snd_display("sample2file mus_file_name: %s?", gen.file_name) if gen.file_name != "fmv.snd"
  100.times do |i|
    sample2file(gen, i, 0, i * 0.001)
    sample2file(gen, i, 1, i * 0.010)
  end
  outa(50, 0.015, gen)
  outb(50, 0.150, gen)
  out_any(60, 0.015, 0, gen)
  out_any(60, 0.150, 1, gen)
  mus_close(gen)
  gen = make_file2sample("fmv.snd")
  print_and_check(gen, "file->sample", "file->sample: fmv.snd")
  val0 = in_any(20, 0, gen)
  val1 = in_any(20, 1, gen)
  val2 = ina(30, gen)
  val3 = inb(30, gen)
  val4 = file2sample(gen, 40, 0)
  val5 = file2sample(gen, 40, 1)
  val6 = in_any(50, 0, gen)
  val7 = in_any(50, 1, gen)
  val8 = in_any(60, 0, gen)
  val9 = in_any(60, 1, gen)
  snd_display("sample2file channels: %s?", mus_channels(gen)) if mus_channels(gen) != 2
  snd_display("%s not input?", gen) unless mus_input?(gen)
  snd_display("in_any: %s %s?", val0, val1) if fneq(val0, 0.02) or fneq(val1, 0.2)
  snd_display("ina|b: %s %s?", val2, val3) if fneq(val2, 0.03) or fneq(val3, 0.3)
  snd_display("sample2file: %s %s?", val4, val5) if fneq(val4, 0.04) or fneq(val5, 0.4)
  snd_display("outa|b: %s %s?", val6, val7) if fneq(val6, 0.065) or fneq(val7, 0.65)
  snd_display("out_any: %s %s?", val8, val9) if fneq(val8, 0.075) or fneq(val9, 0.75)
  #
  gen = make_sample2file("fmv.snd", 4, Mus_lshort, Mus_riff)
  print_and_check(gen, "sample->file", "sample->file: fmv.snd")
  10.times do |i|
    outa(i, 0.1, gen)
    outb(i, 0.2, gen)
    outc(i, 0.3, gen)
    outd(i, 0.4, gen)
  end
  10.times do |i|
    outa(i, 0.01, gen)
    outb(i, 0.02, gen)
    outc(i, 0.03, gen)
    outd(i, 0.04, gen)
  end
  mus_close(gen)
  gen = make_file2sample("fmv.snd")
  print_and_check(gen, "file->sample", "file->sample: fmv.snd")
  10.times do |i|
    if fneq(res1 = ina(i, gen), 0.11) or
        fneq(res2 = inb(i, gen), 0.22) or
        fneq(res3 = in_any(i, 2, gen), 0.33) or
        fneq(res4 = in_any(i, 3, gen), 0.44)
      snd_display("4-chan out/in[%d]: %s %s %s %s?", i, res1, res2, res3, res4)
    end
  end
  if (res = snd_catch do
        make_sample2file("fmv.snd", -1, Mus_lshort, Mus_next)
      end).first != :out_of_range
    snd_display("make_sample2file bad chans: %s", res.inspect)
  end
  if (res = snd_catch do mus_location(make_oscil()) end).first != :mus_error
    snd_display("set_mus_location(make_oscil()): %s", res.inspect)
  end
  if (res = snd_catch do
        make_sample2file("fmv.snd", 1, -1, Mus_next)
      end).first != :out_of_range
    snd_display("make_sample2file bad format: %s", res.inspect)
  end
  if (res = snd_catch do
        make_sample2file("fmv.snd", 1, Mus_lshort, -1)
      end).first != :out_of_range
    snd_display("make_sample2file bad type: %s", res.inspect)
  end
  # 
  gen = make_frame2file("fmv1.snd", 2, Mus_bshort, Mus_next)
  print_and_check(gen, "frame->file", "frame->file: fmv1.snd")
  snd_display("%s not frame2file?", gen) unless frame2file?(gen)
  snd_display("%s not output?", gen) unless mus_output?(gen)
  snd_display("frame2file length: %s?", gen.length) if gen.length != mus_file_buffer_size
  snd_display("frame2file mus_file_name: %s?", gen.file_name) if gen.file_name != "fmv1.snd"
  gen.length = 4096
  snd_display("frame2file length: %s?", gen.length) if gen.length != 4096
  gen.length = 8192
  fr0 = make_frame(2, 0.0, 0.0)
  100.times do |i|
    frame_set!(fr0, 0, i * 0.001)
    frame_set!(fr0, 1, i * 0.010)
    frame2file(gen, i, fr0)
  end
  mus_close(gen)
  gen = make_file2frame("fmv1.snd", 1024)
  val4 = file2frame(gen, 40)
  frout = make_frame(2)
  if fneq(frame_ref(val4, 0), 0.04) or fneq(frame_ref(val4, 1), 0.4)
    snd_display("frame2file output: %s?", val4)
  end
  file2frame(gen, 40, frout)
  unless frout.eql?(val4)
    snd_display("frame2file output via frame: %s %s?", frout, val4)
  end
  #
  gen = make_sample2file("fmv2.snd", 4, Mus_bshort, Mus_aifc)
  print_and_check(gen, "sample->file", "sample->file: fmv2.snd")
  snd_display("%s not sample2file?", gen) unless sample2file?(gen)
  snd_display("%s not output?", gen) unless mus_output?(gen)
  100.times do |i|
    sample2file(gen, i, 0, i * 0.001)
    sample2file(gen, i, 1, i * 0.010)
    sample2file(gen, i, 2, i * 0.002)
    sample2file(gen, i, 3, i * 0.003)
  end
  outa(50, 0.015, gen)
  outb(50, 0.150, gen)
  outc(50, 0.020, gen)
  outd(50, 0.030, gen)
  out_any(60, 0.015, 0, gen)
  out_any(60, 0.150, 1, gen)
  out_any(60, 0.020, 2, gen)
  out_any(60, 0.030, 3, gen)
  mus_close(gen)
  gen = make_file2sample("fmv2.snd")
  val0 = in_any(20, 2, gen)
  val1 = in_any(20, 3, gen)
  val2 = file2sample(gen, 50, 2)
  val3 = file2sample(gen, 50, 3)
  val4 = file2sample(gen, 60, 2)
  val5 = file2sample(gen, 60, 3)
  snd_display("file2sample channels (4): %s?", mus_channels(gen)) if mus_channels(gen) != 4
  snd_display("file2sample increment: %s?", mus_increment(gen)) if fneq(mus_increment(gen), 0.0)
  snd_display("in_any(0, 4): %s %s?", val0, val1) if fneq(val0, 0.04) or fneq(val1, 0.06)
  snd_display("file2sample(4): %s %s?", val2, val3) if fneq(val2, 0.12) or fneq(val3, 0.18)
  snd_display("in_any(4, 4): %s %s?", val4, val5) if fneq(val4, 0.14) or fneq(val5, 0.21)
  #
  delete_file("fmv.snd")
  mus_sound_forget("fmv.snd")
  sf = make_sample2file("fmv.snd", 2, Mus_bshort, Mus_next, "this is a comment")
  10.times do |i|
    sample2file(sf, i, 0, i * 0.10)
    sample2file(sf, i, 1, i * 0.01)
  end
  mus_close(sf)
  if (res = mus_sound_chans("fmv.snd")) != 2
    snd_display("sample2file chans: %s?", res)
  end
  if (res = mus_sound_frames("fmv.snd")) != 10
    snd_display("sample2file frames: %s?", res)
  end
  if (res = mus_sound_samples("fmv.snd")) != 20
    snd_display("sample2file samples: %s?", res)
  end
  if (res = mus_sound_header_type("fmv.snd")) != Mus_next
    snd_display("sample2file type: %s?", res)
  end
  if (res = mus_sound_data_format("fmv.snd")) != Mus_bshort
    snd_display("sample2file format: %s?", res)
  end
  if (res = mus_sound_comment("fmv.snd")) != "this is a comment"
    snd_display("sample2file comment: %s?", res)
  end
  rd = make_file2sample("fmv.snd")
  10.times do |i|
    if fneq(c0 = file2sample(rd, i, 0), i * 0.10) or fneq(c1 = file2sample(rd, i, 1), i * 0.01)
      snd_display("sample2file2sample at %d: %s %s?", i, c0, c1)
      break
    end
  end
  mus_close(rd)
  sf = continue_sample2file("fmv.snd")
  10.times do |i|
    sample2file(sf, i + 5, 0, i * -0.02)
    sample2file(sf, i + 5, 1, i * -0.01)
  end
  mus_close(sf)
  mus_sound_forget("fmv.snd")
  if (res = mus_sound_chans("fmv.snd")) != 2
    snd_display("continue_sample2file chans: %s?", res)
  end
  if (res = mus_sound_frames("fmv.snd")) != 15
    snd_display("continue_sample2file frames: %s?", res)
  end
  if (res = mus_sound_samples("fmv.snd")) != 30
    snd_display("continue_sample2file samples: %s?", res)
  end
  if (res = mus_sound_header_type("fmv.snd")) != Mus_next
    snd_display("continue_sample2file type: %s?", res)
  end
  if (res = mus_sound_data_format("fmv.snd")) != Mus_bshort
    snd_display("continue_sample2file format: %s?", res)
  end
  if (res = mus_sound_comment("fmv.snd")) != "this is a comment"
    snd_display("continue_sample2file comment: %s?", res)
  end
  ind = open_sound("fmv.snd")
  unless vequal(c0 = channel2vct(0, 15, ind, 0),
                vct(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.58, 0.66,
                    0.74, 0.82, -0.1, -0.12, -0.14, -0.16, -0.18))
    snd_display("continue_sample2file (0): %s", c0)
  end
  unless vequal(c0 = channel2vct(0, 15, ind, 1),
                vct(0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.05, 0.05,
                    0.05, 0.05, -0.05, -0.06, -0.07, -0.08, -0.09))
    snd_display("continue_sample2file (1): %s", c0)
  end
  close_sound(ind)
  #
  delete_file("fmv.snd")
  mus_sound_forget("fmv.snd")
  sf = make_frame2file("fmv.snd", 2, Mus_lfloat, Mus_riff, "this is a comment")
  10.times do |i|
    frame2file(sf, i, make_frame(2, i * 0.10, i * 0.01))
  end
  mus_close(sf)
  if (res = mus_sound_chans("fmv.snd")) != 2
    snd_display("frame2file chans: %s?", res)
  end
  if (res = mus_sound_frames("fmv.snd")) != 10
    snd_display("frame2file frames: %s?", res)
  end
  if (res = mus_sound_samples("fmv.snd")) != 20
    snd_display("frame2file samples: %s?", res)
  end
  if (res = mus_sound_header_type("fmv.snd")) != Mus_riff
    snd_display("frame2file type: %s?", res)
  end
  if (res = mus_sound_data_format("fmv.snd")) != Mus_lfloat
    snd_display("frame2file format: %s?", res)
  end
  if (res = mus_sound_comment("fmv.snd")) != "this is a comment"
    snd_display("frame2file comment: %s?", res)
  end
  rd = make_file2sample("fmv.snd")
  10.times do |i|
    f0 = file2frame(rd, i)
    if f0.length != 2 or
        fneq(frame_ref(f0, 0), i * 0.10) or
        fneq(frame_ref(f0, 1), i * 0.01)
      snd_display("frame2file2frame at %d: %s?", i, f0)
      break
    end
  end
  mus_close(rd)
  sf = continue_frame2file("fmv.snd")
  10.times do |i|
    frame2file(sf, i + 5, make_frame(2, i * -0.02, i * -0.01))
  end
  mus_close(sf)
  mus_sound_forget("fmv.snd")
  if (res = mus_sound_chans("fmv.snd")) != 2
    snd_display("continue_frame2file chans: %s?", res)
  end
  if (res = mus_sound_frames("fmv.snd")) != 15
    snd_display("continue_frame2file frames: %s?", res)
  end
  if (res = mus_sound_samples("fmv.snd")) != 30
    snd_display("continue_frame2file samples: %s?", res)
  end
  if (res = mus_sound_header_type("fmv.snd")) != Mus_riff
    snd_display("continue_frame2file type: %s?", res)
  end
  if (res = mus_sound_data_format("fmv.snd")) != Mus_lfloat
    snd_display("continue_frame2file format: %s?", res)
  end
  if (res = mus_sound_comment("fmv.snd")) != "this is a comment"
    snd_display("continue_frame2file comment: %s?", res)
  end
  ind = open_sound("fmv.snd")
  unless vequal(c0 = channel2vct(0, 15, ind, 0),
                vct(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.58, 0.66,
                    0.74, 0.82, -0.1, -0.12, -0.14, -0.16, -0.18))
    snd_display("continue_frame2file (0): %s", c0)
  end
  unless vequal(c0 = channel2vct(0, 15, ind, 1),
                vct(0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.05, 0.05,
                    0.05, 0.05, -0.05, -0.06, -0.07, -0.08, -0.09))
    snd_display("continue_frame2file (1): %s", c0)
  end
  close_sound(ind)
  delete_file("fmv.snd")
  mus_sound_forget("fmv.snd")
  #
  os = make_oscil(440.0)
  v0 = make_vct!(1000) do 0.1 * oscil(os) end
  array2file("fmv3.snd", v0, 10000, 22050, 1)     # 10000 deliberate
  v1 = make_vct(1000)
  file2array("fmv3.snd", 0, 0, 1000, v1)
  vct2vector(v0).zip(vct2vector(v1)) do |val1, val2|
    snd_display("array2file2array: %f %f?", val1, val2) if fneq(val1, val2)
  end
  #
  if (res = snd_catch do
        array2file("fmv3.snd", v0, -1, 1000, 1)
      end).first != :out_of_range
    snd_display("array2file bad samps: %s", res.inspect)
  end
  if (res = snd_catch do
        array2file("/bad/baddy/fmv3.snd", v0, 1, 1000, 1)
      end).first != :mus_error
    snd_display("array2file bad file: %s", res.inspect)
  end
  if (res = snd_catch do
        file2array("fmv3.snd", -1, 0, -1, v0)
      end).first != :out_of_range
    snd_display("file2array bad samps: %s", res.inspect)
  end
end

def test128
  gen = make_rand(10000.0)
  print_and_check(gen, "rand", "rand freq: 10000.000Hz, phase: 0.000, amp: 1.000")
  v0 = make_vct!(10) do rand(gen, 0.0) end
  snd_display("%s not rand?", gen) unless rand?(gen)
  snd_display("rand phase: %f?", gen.phase) if fneq(gen.phase, 3.3624296)
  snd_display("rand frequency %f?", gen.frequency) if fneq(gen.frequency, 10000.0)
  gen.scaler = 0.5
  snd_display("rand set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  snd_display("rand output: %s", v0) if v0[1] == v0[8]
  # 
  gen = make_rand(10000.0, :envelope, [0, 0, 1, 1])
  print_and_check(gen,
                  "rand",
                  "rand freq: 10000.000Hz, phase: 0.000, amp: 1.000, with distribution envelope")
  v0 = make_vct!(10) do rand(gen, 0.0) end
  snd_display("(dist) %s not rand?", gen) unless rand?(gen)
  snd_display("(dist) rand frequency %f?", gen.frequency) if fneq(gen.frequency, 10000.0)
  snd_display("(dist) rand output: %s", v0) if v0[1] == v0[8]
  if (not vct?(gen.data)) or gen.length != gen.data.length or gen.length != 512
    snd_display("(dist) rand data: %d %s?", gen.length, gen.data)
  end
  #
  gen1 = make_rand(10000.0, :envelope, [0, 0, 1, 1])
  gen2 = make_rand(10000.0, :envelope, [0, 1, 1, 0])
  up1 = 0
  down1 = 0
  bad1 = 0
  up2 = 0
  down2 = 0
  bad2 = 0
  1000.times do |i|
    val1 = rand(gen1)
    val2 = rand(gen2)
    if val1 >= 0.5
      up1 += 1
    else
      if val1 >= 0.0
        down1 += 1
      else
        bad1 += 1
      end
    end
    if val2 >= 0.5
      up2 += 1
    else
      if val2 >= 0.0
        down2 += 1
      else
        bad2 += 1
      end
    end
  end
  if bad1.nonzero? or bad2.nonzero? or 2.5 * down1 > up1 or 2.5 * up2 > down2
    snd_display("rand dist: %s %s %s,  %s %s %s", down1, up1, bad1, down2, up2, bad2)
  end
  test_gen_equal(make_rand(1000), make_rand(1000), make_rand(500))
  test_gen_equal(make_rand(1000), make_rand(1000), make_rand(1000, 0.5))
  # 
  gen = make_rand_interp(4000.0)
  print_and_check(gen, "rand-interp", gen.to_s)
  v0 = make_vct!(10) do rand_interp(gen, 0.0) end
  snd_display("%s not rand_interp?", gen) unless rand_interp?(gen)
  snd_display("rand_interp phase: %f?", gen.phase) if fneq(gen.phase, 5.114882)
  snd_display("rand_interp frequency %f?", gen.frequency) if fneq(gen.frequency, 4000.0)
  gen.scaler = 0.5
  snd_display("rand_interp set_scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.5)
  snd_display("rand_interp output: %s", v0) if v0[1] == v0[8]
  # 
  gen = make_rand_interp(4000.0, :envelope, [-1, 1, 0, 0, 1, 1])
  print_and_check(gen, "rand-interp", gen.to_s)
  v0 = make_vct!(10) do rand_interp(gen, 0.0) end
  snd_display("(dist) %s not rand_interp?", gen) unless rand_interp?(gen)
  snd_display("(dist) rand_interp output: %s", v0) if v0[1] == v0[8]
  if (not vct?(gen.data)) or gen.length != gen.data.length or gen.length != 512
    snd_display("(dist) rand_interp data: %d %s?", gen.length, gen.data)
  end
  #
  gen = make_rand(10000.0, 1.0)
  gen1 = make_rand_interp(10000.0, 1.0)
  1000.times do
    val1 = gen.run(0.0)
    val2 = gen1.run(0.0)
    snd_display(",rand: %s %s?", val1, gen) if val1 > 1.0 or val1 < -1.0
    snd_display(",rand_interp: %s %s?", val2, gen1) if val2 > 1.0 or val2 < -1.0
  end
  gen = make_rand(10000.0, :distribution, inverse_integrate([0, 0, 1, 1]))
  print_and_check(gen,
                  "rand",
                  "rand freq: 10000.000Hz, phase: 0.000, amp: 1.000, with distribution envelope")
  v0.map! do rand(gen, 0.0) end
  snd_display("(dist 2) %s not rand?", gen) unless rand?(gen)
  snd_display("(dist 2) rand frequency %f?", gen.frequency) if fneq(gen.frequency, 10000.0)
  snd_display("(dist 2) rand output: %s", v0) if v0[1] == v0[8]
  if (not vct?(gen.data)) or gen.length != gen.data.length or gen.length != 512
    snd_display("(dist 2) rand data: %d %s?", gen.length, gen.data)
  end
  #
  gen1 = make_rand(10000.0, :distribution, inverse_integrate([0, 0, 1, 1]))
  gen2 = make_rand(10000.0, :distribution, inverse_integrate([0, 1, 1, 0]))
  up1 = 0
  down1 = 0
  bad1 = 0
  up2 = 0
  down2 = 0
  bad2 = 0
  1000.times do |i|
    val1 = rand(gen1)
    val2 = rand(gen2)
    if val1 >= 0.5
      up1 += 1
    else
      if val1 >= 0.0
        down1 += 1
      else
        bad1 += 1
      end
    end
    if val2 >= 0.5
      up2 += 1
    else
      if val2 >= 0.0
        down2 += 1
      else
        bad2 += 1
      end
    end
  end
  if bad1.nonzero? or bad2.nonzero? or 2.5 * down1 > up1 or 2.5 * up2 > down2
    snd_display("rand dist 2: %s %s %s,  %s %s %s", down1, up1, bad1, down2, up2, bad2)
  end
  #
  v1 = inverse_integrate([-1, 1, 1, 1])
  snd_display("inverse_integrate -1 to 1 uniform: %s?", v1) if fneq(v1[4], -0.984)
  v1 = inverse_integrate([0, 1, 1, 1])
  snd_display("inverse_integrate 0 to 1 uniform: %s?", v1) if fneq(v1[4], 0.008)
  v1 = inverse_integrate([0, 1, 1, 0])
  snd_display("inverse_integrate 0 to 1 1 to 0: %s?", v1) if fneq(v1[4], 0.004)
  v1 = inverse_integrate([0, 0, 0.5, 1, 1, 0])
  snd_display("inverse_integrate triangle: %s?", v1) if fneq(v1[4], 0.073)
  v1 = inverse_integrate(gaussian_envelope(1.0))
  snd_display("inverse_integrate gaussian: %s?", v1) if fneq(v1[4], -0.593)
end

def test138
  minp = 1.0
  maxp = -1.0
  1100.times do
    val1 = mus_random(1.0)
    minp = val1 if val1 < minp
    maxp = val1 if val1 > maxp
    snd_display("mus_random: %f?", val1) if val1 > 1.0 or val1 < -1.0
  end
  snd_display("mus_random: %f %f", minp, maxp) if maxp < 0.9 or minp > -0.9
  minp = 12.0
  maxp = -12.0
  1100.times do
    val1 = mus_random(12.0)
    minp = val1 if val1 < minp
    maxp = val1 if val1 > maxp
    snd_display("mus_random (12): %f?", val1) if val1 > 12.0 or val1 < -12.0
  end
  snd_display("mus_random (12): %f %f", minp, maxp) if maxp < 11.0 or minp > -11.0
  v = lambda do |n|
    hits = make_array(10, 0)
    n.times do |i|
      y = (5 + mus_random(5.0)).floor
      hits[y] += 1
    end
    sum = 0.0
    p = n / 10.0
    hits.each do |val|
      num = val - p
      sum += (num * num) / p
    end
    sum
  end
  if (res = v.call(10000)) < 4.0
    snd_display("mus_random not so random? %f (chi)", res)
  end
  v1 = lambda do |n|
    hits = make_array(10, 0)
    gen = make_rand(22050.0)
    n.times do |i|
      y = (5 + 5 * rand(gen, 0.0)).floor
      hits[y] += 1
    end
    sum = 0.0
    p = n / 10.0
    hits.each do |val|
      num = val - p
      sum += (num * num) / p
    end
    sum
  end
  if (res = v1.call(10000)) < 4.0
    snd_display("rand not so random? %f (chi)", res)
  end
  v2 = lambda do |n|                              # Kolmogorov-Smirnov
    vals = make_array(n) do 0.5 + mus_random(0.5) end.sort
    sn = sqrt(n)
    k_p = 0.0
    k_m = 0.0
    incr = 1.0 / n
    y = 0.0
    x = incr
    (1...n).each do |i|
      kp = x - vals[i]
      km = vals[i] - y
      k_p = kp if kp > k_p
      k_m = km if km > k_m
      y = x
      x += incr
    end
    [ sn * k_p, sn * k_m,
     0.07089 - 0.15 / sn,
     0.1601 - 0.014 / sn,
     0.3793 - 0.15 / sn,
     0.5887 - 0.15 / sn]
  end
  vr = v2.call(1000)
  kp = vr[0]
  km = vr[1]
  k = vr[3]
  if kp < k or km < k
    snd_display("mus_random not so random? %f (chi)", vr)
  end
  #
  data = make_vct!(65536) do mus_random(1.0) end
  ndat = snd_spectrum(data, Rectangular_window, 65536, true, 0.0, false, false)
  peak = vct_peak(ndat)
  sum = 0.0
  if peak > 1000.0
    snd_display("mus_random spectral peak: %f?", peak)
  end
  32768.times do |i| sum += ndat[i] end
  if (res = sum / 32768.0) > 200.0
    snd_display("random average: %f %f?", res, ndat[0])
  end
  data.map! do mus_random(1.0) end
  autocorrelate(data)
  data[0] = 0.0
  pk = vct_peak(data)
  if pk > 1000
    snd_display("random autocorrelate peak: %f?", pk)
  end
  sum = 0.0
  32768.times do |i| sum += ndat[i].abs end
  if (res = sum / 32768.0) > 200.0
    snd_display("random autocorrelate average: %f?", res)
  end
end

def test148
  set_locsig_type(Mus_interp_linear)
  gen = make_locsig(30.0, :channels, 2)
  gen1 = make_locsig(60.0, :channels, 2)
  gen2 = make_locsig(60.0, :channels, 4)
  gen200 = make_locsig(200.0, :channels, 4)
  gen3 = gen1
  fr0 = locsig(gen, 0, 1.0)
  print_and_check(gen, "locsig", "locsig: chans 2, outn: [0.667 0.333], interp: linear")
  snd_display("%s not locsig?", gen) unless locsig?(gen)
  snd_display("locsig %s.eql?(%s)?", gen1, gen3) unless gen1.eql?(gen3)
  snd_display("locsig %s == %s?", gen1, gen3) unless gen1 == gen3
  snd_display("locsig 1 %s.eql?(%s)?", gen1, gen2) if gen1.eql?(gen2)
  snd_display("locsig 2 %s == %s?", gen, gen1) if gen == gen1
  snd_display("locsig 3 %s == %s?", gen, gen2) if gen == gen2
  if fneq(frame_ref(fr0, 0), 0.667) or fneq(frame_ref(fr0, 1), 0.333)
    snd_display("locsig output: %s?", fr0)
  end
  if fneq(res1 = locsig_ref(gen, 0), 0.667) or fneq(res2 = locsig_ref(gen, 1), 0.333)
    snd_display("locsig ref: %f %f?", res1, res2)
  end
  unless vequal(mus_data(gen), vct(0.667, 0.333))
    snd_display("locsig gen outn: %s?", mus_data(gen))
  end
  unless vequal(mus_data(gen1), vct(0.333, 0.667))
    snd_display("locsig gen1 outn: %s?", mus_data(gen1))
  end
  unless vequal(mus_data(gen2), vct(0.333, 0.667, 0.000, 0.000))
    snd_display("locsig gen2 outn: %s?", mus_data(gen2))
  end
  unless vequal(mus_data(gen200), vct(0.000, 0.000, 0.778, 0.222))
    snd_display("locsig gen200 outn: %s?", mus_data(gen200))
  end
  locsig_set!(gen, 0, 0.25)
  unless vequal(mus_data(gen), vct(0.250, 0.333))
    snd_display("locsig gen 0.25 outn: %s?", mus_data(gen))
  end
  fr0 = locsig(gen, 0, 1.0)
  if fneq(res = locsig_ref(gen, 0), 0.25)
    snd_display("locsig_set!: %f?", res)
  end
  locsig_set!(gen, 0, 0.5)
  unless vequal(mus_data(gen), vct(0.500, 0.333))
    snd_display("locsig gen 0.5 outn: %s?", mus_data(gen))
  end
  fr0 = locsig(gen, 0, 1.0)
  if fneq(res = locsig_ref(gen, 0), 0.5)
    snd_display("locsig_set!: %f?", res)
  end
  gen = make_locsig(120.0, 2.0, 0.1, :channels, 4)
  unless vequal(mus_data(gen), vct(0.000, 0.333, 0.167, 0.000))
    snd_display("locsig gen 120 outn: %s?", mus_data(gen))
  end
  fr0 = locsig(gen, 0, 1.0)
  if fneq(frame_ref(fr0, 1), 0.333) or fneq(frame_ref(fr0, 2), 0.167)
    snd_display("locsig quad output: %s?", fr0)
  end
  gen = make_locsig(300.0, 2.0, 0.1, :channels, 4)
  unless vequal(mus_data(gen), vct(0.167, 0.000, 0.000, 0.333))
    snd_display("locsig gen 300 outn: %s?", mus_data(gen))
  end
  fr0 = locsig(gen, 0, 1.0)
  if fneq(frame_ref(fr0, 3), 0.333) or fneq(frame_ref(fr0, 0), 0.167)
    snd_display("300 locsig quad output: %s?", fr0)
  end
  move_locsig(gen1, 90.0, 1.0)
  unless vequal(mus_data(gen1), vct(0.000, 1.000))
    snd_display("locsig gen1 90 outn: %s?", mus_data(gen1))
  end
  move_locsig(gen1, 0.0, 1.0)
  unless vequal(mus_data(gen1), vct(1.000, 0.000))
    snd_display("locsig gen1 0 outn: %s?", mus_data(gen1))
  end
  if fneq(locsig_ref(gen1, 0), 1.0) or fneq(locsig_ref(gen1, 1), 0.0)
    snd_display("move_locsig 0 1: %s?", gen1)
  end
  move_locsig(gen1, 45.0, 1.0)
  unless vequal(mus_data(gen1), vct(0.500, 0.500))
    snd_display("locsig gen1 45 outn: %s?", mus_data(gen1))
  end
  if fneq(locsig_ref(gen1, 0), 0.5) or fneq(locsig_ref(gen1, 1), 0.5)
    snd_display("move_locsig 45 1: %s?", gen1)
  end
  move_locsig(gen1, 135.0, 2.0)
  unless vequal(mus_data(gen1), vct(0.000, 0.500))
    snd_display("locsig gen1 135 outn: %s?", mus_data(gen1))
  end
  move_locsig(gen1, -270.0, 3.0)
  unless vequal(mus_data(gen1), vct(0.000, 0.333))
    snd_display("locsig gen1 -270 outn: %s?", mus_data(gen1))
  end
  [1, 2, 4, 8].each do |chans|
    m1 = make_locsig(:channels, chans)
    if m1.channels != chans or m1.length != chans
      snd_display("locsig %d chans but: %d %d?", chans, m1.channels, m1.length)
    end
    chans.times do |i| locsig_set!(m1, i, i * 0.1) end
    chans.times do |i|
      if fneq(locsig_ref(m1, i), i * 0.1)
        snd_display("locsig[%d] = %f (%f)?", i, locsig_ref(m1, i), i * 0.1)
      end
    end
  end
  #
  if (res = snd_catch do make_locsig(:channels, 0) end).first != :mus_error
    snd_display("make_locsig bad (0) chans: %s", res.inspect)
  end
  if (res = snd_catch do make_locsig(:channels, -2) end).first != :out_of_range
    snd_display("make_locsig bad (-2) chans: %s", res.inspect)
  end
  if (res = snd_catch do make_locsig(:output, 1) end).first != :wrong_type_arg
    snd_display("make_locsig bad output: %s", res.inspect)
  end
  if (res = snd_catch do locsig_ref(make_locsig(), 1) end).first != :mus_error
    snd_display("locsig_ref bad chan: %s", res.inspect)
  end
  if (res = snd_catch do
        locs = make_locsig(200, :channels, 2)
        locsig_ref(locs, 2)
      end).first != :mus_error
    snd_display("locsig_ref bad chan: %s", res.inspect)
  end
  if (res = snd_catch do
        locs = make_locsig()
        locsig_set!(locs, 2, 0.1)
      end).first != :mus_error
    snd_display("locsig_set! bad chan (2): %s", res.inspect)
  end
  if (res = snd_catch do
        locs = make_locsig(:reverb, 0.1)
        locsig_reverb_ref(locs, 2)
      end).first != :mus_error
    snd_display("locsig_reverb_ref bad reverb chan (2): %s", res.inspect)
  end
  if (res = snd_catch do
        locs = make_locsig(:reverb, 0.1)
        locsig_reverb_set!(locs, 2, 0.1)
      end).first != :mus_error
    snd_display("locsig_reverb_set! bad reverb chan (2): %s", res.inspect)
  end
  #
  locs = make_locsig(:channels, 8, :degree, 0)
  move_locsig(locs, 180.0, 1.0)
  snd_display("move_locsig by jump: %s?", locs.data) if fneq(locsig_ref(locs, 0), 0.0)
  unless vequal(locs.data, vct(0.000, 0.000, 0.000, 0.000, 1.000, 0.000, 0.000, 0.000))
    snd_display("move_locsig by jump data: %s?", locs.data)
  end
  move_locsig(locs, 120.0, 1.0)
  unless vequal(locs.data, vct(0.000, 0.000, 0.333, 0.667, 0.000, 0.000, 0.000, 0.000))
    snd_display("move_locsig by jump 120 data: %s?", locs.data)
  end
  move_locsig(locs, -20.0, 1.0)
  unless vequal(locs.data, vct(0.556, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.444))
    snd_display("move_locsig by jump -20 data: %s?", locs.data)
  end
  #
  sf = make_sample2file("fmv4.snd", 8, Mus_bshort, Mus_next, "this is a comment")
  sfrev = make_sample2file("fmv4.reverb", 8, Mus_bshort, Mus_next, "this is a comment")
  locs = make_locsig(:channels, 8, :degree, 0, :distance, 1.0, :reverb, 0.1,
                     :output, sf, :revout, sfrev, :type, Mus_interp_linear)
  unless vequal(locs.data, vct(1.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("ws not move_locsig by jump data: %s?", locs.data)
  end
  unless vequal(locs.xcoeffs, vct(0.100, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("ws not move_locsig by jump rev data: %s?", locs.xcoeffs)
  end
  move_locsig(locs, 180.0, 2.0)
  snd_display("ws move_locsig by jump: %s?", locs.data) if fneq(locsig_ref(locs, 0), 0.0)
  unless vequal(locs.data, vct(0.000, 0.000, 0.000, 0.000, 0.500, 0.000, 0.000, 0.000))
    snd_display("ws move_locsig by jump data: %s?", locs.data)
  end
  unless vequal(locs.xcoeffs, vct(0.000, 0.000, 0.000, 0.000, 0.071, 0.000, 0.000, 0.000))
    snd_display("ws move_locsig by jump rev data: %s?", locs.xcoeffs)
  end
  move_locsig(locs, 120.0, 3.0)
  unless vequal(locs.data, vct(0.000, 0.000, 0.111, 0.222, 0.000, 0.000, 0.000, 0.000))
    snd_display("ws move_locsig by jump 120 data: %s?", locs.data)
  end
  unless vequal(locs.xcoeffs, vct(0.000, 0.000, 0.019, 0.038, 0.000, 0.000, 0.000, 0.000))
    snd_display("ws move_locsig by jump 120 rev data: %s?", locs.xcoeffs)
  end
  move_locsig(locs, -20.0, 4.0)
  unless vequal(locs.data, vct(0.139, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.111))
    snd_display("ws move_locsig by jump -20 data: %s?", locs.data)
  end
  unless vequal(locs.xcoeffs, vct(0.028, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.022))
    snd_display("ws move_locsig by jump -20 rev data: %s?", locs.xcoeffs)
  end
  mus_close(sf)
  mus_close(sfrev)
  delete_files("fmv4.snd", "fmv4.reverb")
  mus_sound_prune
  #
  gen = make_frame2file("fmv4.snd", 2, Mus_bshort, Mus_next)
  rev = make_frame2file("fmv4.reverb", 1, Mus_bshort, Mus_next)
  lc = make_locsig(60.0, :reverb, 0.1, :channels, 2, :output, gen, :revout, rev)
  100.times do |i| locsig(lc, i, 1.0) end
  if fneq(res = locsig_reverb_ref(lc, 0), 0.1)
    snd_display("locsig_reverb_ref: %f?", res)
  end
  locsig_reverb_set!(lc, 0, 0.3)
  if fneq(res = locsig_reverb_ref(lc, 0), 0.3)
    snd_display("locsig_reverb_set!: %f?", res)
  end
  locsig_reverb_set!(lc, 0, 0.2)
  if fneq(res = locsig_reverb_ref(lc, 0), 0.2)
    snd_display("locsig_reverb_set!: %f?", res)
  end
  mus_close(gen)
  mus_close(rev)
  v0 = make_vct(100)
  v1 = make_vct(100)
  v2 = make_vct(100)
  file2array("fmv4.snd", 0, 0, 100, v0)
  file2array("fmv4.snd", 1, 0, 100, v1)
  file2array("fmv4.reverb", 0, 0, 100, v2)
  snd_display("locsig reverb: %s?", v2) if fneq(v2[0], 0.1)
  snd_display("locsig direct: %f %f?", v0[0], v1[0]) if fneq(2 * v0[0], v1[0])
  # 
  gen = make_frame2file("fmv4.snd", 4, Mus_bshort, Mus_next)
  rev = make_frame2file("fmv4.reverb", 4, Mus_bshort, Mus_next)
  lc = make_locsig(60.0, :reverb, 0.1, :channels, 4, :distance, 4.0, :output, gen, :revout, rev)
  print_and_check(lc,
                  "locsig",
                  "locsig: chans 4, outn: [0.083 0.167 0.000 0.000], revn: [0.017 0.033 0.000 0.000], interp: linear")
  100.times do |i| locsig(lc, i, 1.0) end
  4.times do |i|
    locsig_reverb_set!(lc, i, i * 0.1)
    if fneq(res = locsig_reverb_ref(lc, i), i * 0.1)
      snd_display("locsig_reverb_set![%d]: %f?", i, res)
    end
  end
  print_and_check(lc,
                  "locsig",
                  "locsig: chans 4, outn: [0.083 0.167 0.000 0.000], revn: [0.000 0.100 0.200 0.300], interp: linear")
  snd_display("out data locsig: %s?", lc.data) unless vct?(lc.data)
  snd_display("rev data locsig: %s?", lc.xcoeffs) unless vct?(lc.xcoeffs)
  xcs = lc.xcoeffs
  if fneq(res = mus_xcoeff(lc, 0), xcs[0])
    snd_display("locsig xcoeff: %f %f?", res, xcs[0])
  end
  if fneq(res = mus_xcoeff(lc, 1), 0.1)
    snd_display("locsig xcoeff 1: %f %f?", res, xcs[0])
  end
  mus_close(gen)
  mus_close(rev)
  # 
  print_and_check(make_locsig(160, :channels, 4),
                  "locsig",
                  "locsig: chans 4, outn: [0.000 0.222 0.778 0.000], interp: linear")
  print_and_check(make_locsig(-200, :channels, 4),
                  "locsig",
                  "locsig: chans 4, outn: [0.000 0.222 0.778 0.000], interp: linear")
  print_and_check(make_locsig(160, :channels, 4, :distance, 0.5),
                  "locsig",
                  "locsig: chans 4, outn: [0.000 0.222 0.778 0.000], interp: linear")
  print_and_check(make_locsig(320, :channels, 4),
                  "locsig",
                  "locsig: chans 4, outn: [0.556 0.000 0.000 0.444], interp: linear")
  print_and_check(make_locsig(-40, :channels, 4),
                  "locsig",
                  "locsig: chans 4, outn: [0.556 0.000 0.000 0.444], interp: linear")
  print_and_check(make_locsig(320, :channels, 2),
                  "locsig",
                  "locsig: chans 2, outn: [0.000 1.000], interp: linear")
  print_and_check(make_locsig(-40, :channels, 2),
                  "locsig",
                  "locsig: chans 2, outn: [0.000 1.000], interp: linear")
  # 
  locsig_data = lambda do |g|
    make_vct!(g.channels) do |i| locsig_ref(g, i) end
  end
  gen = make_locsig(-0.1, :channels, 8)
  unless vequal(locsig_data.call(gen), vct(0.998, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.002))
    snd_display("locsig -0.1(8): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(-359.9, :channels, 8)
  unless vequal(locsig_data.call(gen), vct(0.998, 0.002, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("locsig -359.9(8): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(-359.9, :channels, 4)
  unless vequal(locsig_data.call(gen), vct(0.999, 0.001, 0.000, 0.000))
    snd_display("locsig -359.9(4): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(-360.1, :channels, 8)
  unless vequal(locsig_data.call(gen), vct(0.998, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.002))
    snd_display("locsig -360.1(8): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(-700, :channels, 8)
  unless vequal(locsig_data.call(gen), vct(0.556, 0.444, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("locsig -700(8): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(-700, :channels, 2)
  unless vequal(locsig_data.call(gen), vct(0.778, 0.222))
    snd_display("locsig -700(2): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(20, :channels, 2)
  unless vequal(locsig_data.call(gen), vct(0.778, 0.222))
    snd_display("locsig 20(2): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(123456.0, :channels, 8)
  unless vequal(locsig_data.call(gen), vct(0.467, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.533))
    snd_display("locsig 123456(8): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(336.0, :channels, 8)
  unless vequal(locsig_data.call(gen), vct(0.467, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.533))
    snd_display("locsig 336(8): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(-123456.0, :channels, 8)
  unless vequal(locsig_data.call(gen), vct(0.467, 0.533, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("locsig -123456(8): %s?", locsig_data.call(gen))
  end
  gen = make_locsig(24.0, :channels, 8)
  unless vequal(locsig_data.call(gen), vct(0.467, 0.533, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("locsig 24(8): %s?", locsig_data.call(gen))
  end
  #
  [0, 1, 2, 4].each do |rev_chans|
    locsig_scalers = lambda do |chans, degree, type|
      if chans == 1
        vct(1.0)
      else
        deg = chans == 2 ? [0.0, [90.0, degree].min].max : degree.divmod(360.0)[1]
        degs_per_chan = chans == 2 ? 90.0 : (360.0 / chans)
        pos = deg / degs_per_chan
        left = pos.floor
        right = (left + 1) % chans
        frac = pos - left
        v = make_vct(chans)
        if type == Mus_interp_linear
          v[left] = 1.0 - frac
          v[right] = frac
        else
          ldeg = HALF_PI * (0.5 - frac)
          norm = sqrt(2.0) * 0.5
          c = cos(ldeg)
          s = sin(ldeg)
          v[left] = norm * (c + s)
          v[right] = norm * (c - s)
        end
        v
      end
    end
    delete_file("test.reverb")
    revfile = if rev_chans > 0
                make_frame2file("test.reverb", rev_chans, Mus_bshort, Mus_next)
              else
                false
              end
    [Mus_interp_linear, Mus_interp_sinusoidal].each do |type|
      set_locsig_type(type)
      snd_display("locsig_type: %s %s?", type, locsig_type) if locsig_type != type
      callcc do |quit|
        [0.0, 45.0, 90.0, 1234.0].each do |deg|
          gen = make_locsig(deg, :channels, 1, :revout, revfile, :reverb, 0.1, :distance, 2.0)
          revs = revfile ? locsig_scalers.call(rev_chans, deg, type) : []
          snd_display("locsig %s: %s?", deg, gen) if gen.channels != 1
          revs.each_with_index do |val, i|
            if fneq(res1 = locsig_reverb_ref(gen, i), res2 = ((0.1 / sqrt(2.0)) * val))
              snd_display("mono locrev[%d] %s at %s: %f %f?", type, gen, deg, res1, res2)
              quit.call
            end
          end
        end
        [Mus_interp_linear, Mus_interp_sinusoidal].each do |ltype|
          [0.0, 45.0, 90.0, 1234.0].each do |deg|
            gen = make_locsig(deg, :channels, 1, :type, ltype)
            snd_display("locsig %s: %s?", deg, gen) if gen.channels != 1
            if fneq(res = locsig_ref(gen, 0), 1.0)
              snd_display("locsig[%d] scaler %s: %s?", ltype, deg, res)
            end
          end
        end
        [2, 3, 4, 5, 8, 12, 16, 24].each do |chans|
          [0.0, 45.0, 90.0, 120.0, 180.0, 275.0, 315.0, 300.0, 15.0, 1234.0].each do |deg|
            gen = make_locsig(deg, :channels, chans, :revout, revfile, :reverb, 0.1)
            snd_display("multi locsig %s: %s?", deg, gen) if gen.channels != chans
            quit.call
            locsig_scalers.call(chans, deg, type).each_with_index do |val, i|
              if fneq(res = locsig_ref(gen, i), val)
                snd_display("locrev[%d] %s at %s: %f %f?", type, gen, deg, res, val)
                quit.call
              end
            end
            (revfile ? locsig_scalers.call(rev_chans, deg, type) : []).each_with_index do |val, i|
              if fneq(res1 = locsig_reverb_ref(gen, i), res2 = 0.1 * val)
                snd_display("locrev[%d] %s at %s: %f %f?", type, gen, deg, res1, res2)
                quit.call
              end
            end
          end
          [].each do |chans|
            [Mus_interp_linear, Mus_interp_sinusoidal].each do |ltype|
              [0.0, 45.0, 90.0, 120.0, 180.0, 275.0, 315.0, 300.0, 15.0, 1234.0].each do |deg|
                gen = make_locsig(deg, :channels, chans, :type, ltype,
                                  :revout, revfile, :reverb, 0.1)
                if gen.channels != chans
                  snd_display("stereo locsig %s: %s?", deg, gen)
                  quit.call
                end
                locsig_scalers.call(chans, deg, ltype).each_with_index do |val, i|
                  if fneq(res = locsig_ref(gen, i), val)
                    snd_display("locrev[%d] %s at %s: %f %f?", ltype, gen, deg, res, val)
                    quit.call
                  end
                end
                (revfile ? locsig_scalers.call(rev_chans,deg,ltype): []).each_with_index do |val, i|
                  if fneq(res1 = locsig_reverb_ref(gen, i), res2 = 0.1 * val)
                    snd_display("locrev[%d] %s at %s: %f %f?", ltype, gen, deg, res1, res2)
                    quit.call
                  end
                end
              end
            end
          end
        end
      end
    end
  end
end

def test158
  gen = make_src(:srate, 2.0)
  gen1 = make_src(:srate, 2.0)
  gen2 = make_src(:srate, 0.0)
  rd = make_readin("oboe.snd", 0, 2000)
  rd1 = make_readin("oboe.snd", 0, 2000)
  print_and_check(gen, "src", "src: width: 10, x: 0.000, incr: 2.000, sinc table len: 10000")
  v0 = make_vct!(10) do src(gen, 0.0, lambda do |dir| readin(rd) end) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do || src?(gen1) ? src(gen1, 0.0, lambda do |dir| readin(rd1) end) : -1.0 end)
  snd_display("run src: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not src?", gen) unless src?(gen)
  if fneq(v0[1], 0.001) or fneq(v0[7], 0.021)
    snd_display("src output: %s?", v0)
  end
  snd_display("src increment: %f?", gen.increment) if fneq(gen.increment, 2.0)
  snd_display("src 0.0 increment: %f?", gen2.increment) if fneq(gen2.increment, 0.0)
  snd_display("readin increment: %f?", rd.increment) if fneq(rd.increment, 1.0)
  snd_display("src length: %d?", gen.length) if gen.length != 10
  gold = gen
  gen = make_src(lambda do |dir| 0.0 end)
  snd_display("src eql? %s %s?", gen, gold) if gen.eql? gold
  if (res = snd_catch do make_src(:width, -1) end).first != :out_of_range
    snd_display("make_src bad width: %s", res.inspect)
  end
  #
  s1 = make_src(lambda do |y| 1.0 end, 2.0)
  src(s1, 25.0)
  src(s1, 25.0)
  src(s1, 125.0)
  src(s1, -25.0)
  src(s1, -125.0)
  10.times do |i| make_src(lambda do |y| 1.0 end, 1.5, :width, 5 + i * 10) end
  clear_sincs
  #
  ctr = 0.0
  gen = make_src(:srate, 2.0, :input, lambda do |dir|
                   val = ctr
                   ctr += 1
                   val
                 end)
  v0 = make_vct!(10) do src(gen, 0.0) end
  ctr = 0.0
  gen.reset
  v0.each_with_index do |old_val, i|
    if fneq(old_val, new_val = src(gen, 0.0))
      snd_display("reset src %d: %f %f?", i, old_val, new_val)
    end
  end
  s1 = make_src(lambda do |dir| 1.0 end)
  if fneq(res = src(s1, log0(false)), 1.0) # log(0) -> -Infinity: rejected by src
    snd_display("inf as sr_change: %s?", res)
  end
  #
  gen = make_granulate(:expansion, 2.0)
  gen1 = make_granulate(:expansion, 2.0)
  rd = make_readin("oboe.snd", 0, 4000, 1, 2048)
  rd1 = make_readin(:file, "oboe.snd",
                    :channel, 0,
                    :start, 4000,
                    :direction, 1,
                    :size, mus_file_buffer_size)
  print_and_check(gen,
                  "granulate",
                  "granulate: expansion: 2.000 (551/1102), scaler: 0.600, length: 0.150 secs (3308 samps), ramp: 0.060")
  v0 = make_vct!(1000) do granulate(gen, lambda do |dir| readin(rd) end) end
  v1 = make_vct(1000)
  vct_map!(v1, lambda do | |
             granulate?(gen1) ? granulate(gen1, lambda do |dir| readin(rd1) end) : -1.0
           end)
  if (worst = (vct_peak(v0) - vct_peak(v1)).abs) > 0.01
    snd_display("run granulate: %f?", worst)
  end
  genx = gen1
  snd_display("granulate eql? %s %s %s", genx, gen1, genx.eql?(gen1)) unless genx.eql?(gen1)
  snd_display("granulate eql? %s %s?", gen, gen1) if gen.eql? gen1
  snd_display("granulate output peak: %f?", vct_peak(v0)) if vct_peak(v0).zero?
  snd_display("%s not granulate?", gen) unless granulate?(gen)
  snd_display("granulate increment: %f?", gen.increment) if fneq(gen.increment, 2.0)
  snd_display("granulate scaler: %f?", gen.scaler) if fneq(gen.scaler, 0.6)
  snd_display("granulate frequency: %f?", gen.frequency) if fneq(gen.frequency, 0.05)
  snd_display("granulate ramp: %d?", gen.ramp) if gen.ramp != 1323
  snd_display("granulate length: %d?", gen.length) if gen.length != 3308
  snd_display("granulate hop: %d?", gen.hop) if gen.hop != 1102
  gen.hop = 1000
  snd_display("granulate set_hop: %d?", gen.hop) if gen.hop != 1000
  gen.ramp = 1000
  snd_display("granulate set_ramp: %d?", gen.ramp) if gen.ramp != 1000
  gen.length = 3000
  snd_display("granulate set_length: %d?", gen.length) if gen.length != 3000
  gen.increment = 3.0
  snd_display("granulate set_increment: %f?", gen.increment) if ffneq(gen.increment, 3.0)
  gen.frequency = 0.1
  snd_display("granulate set_frequency: %f?", gen.frequency) if fneq(gen.frequency, 0.1)
  # 
  if (res = snd_catch do make_granulate(lambda do |a, b| a end) end).first != :bad_arity
    snd_display("make_granulate bad func: %s", res.inspect)
  end
  if (res = snd_catch do make_granulate(:hop, 35.0, :length, 35.0) end).first != :out_of_range
    snd_display("make_granulate bad sizes: %s", res.inspect)
  end
  #
  ind = open_sound("oboe.snd")
  mx = maxamp
  rd = make_sample_reader(0)
  grn = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir| rd.call end,
                       :edit, lambda do |g|
                         mus_data(g).map! do |val| val *= 2.0 end
                         0
                       end)
  map_channel(lambda do |y| granulate(grn) end)
  if maxamp / mx < 1.5 or mx / maxamp > 2.5
    snd_display("gran edit 2* (0): %f %f?", mx, maxamp)
  end
  undo_edit
  rd = make_sample_reader(0)
  grn = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir| rd.call end,
                       :edit, lambda do |g|
                         mus_data(g).map! do |val| val *= 4.0 end
                         0
                       end)
  map_channel(lambda do |y| granulate(grn) end)
  if maxamp / mx < 3.0 or mx / maxamp > 6.0
    snd_display("gran edit 4* (0): %f %f?", mx, maxamp)
  end
  revert_sound(ind)
  rd = make_sample_reader(0)
  grn = make_granulate(:expansion, 2.0,
                       :edit, lambda do |g|
                         mus_data(g).map! do |val| val *= 2.0 end
                         0
                       end)
  map_channel(lambda do |y| granulate(grn, lambda do |dir| rd.call end) end)
  if maxamp / mx < 1.5 or mx / maxamp > 2.5
    snd_display("gran edit 2* (1): %f %f?", mx, maxamp)
  end
  undo_edit
  rd = make_sample_reader(0)
  grn = make_granulate(:expansion, 2.0,
                       :edit, lambda do |g|
                         mus_data(g).map! do |val| val *= 4.0 end
                         0
                       end)
  map_channel(lambda do |y| granulate(grn, lambda do |dir| rd.call end) end)
  if maxamp / mx < 3.0 or mx / maxamp > 6.0
    snd_display("gran edit 4* (1): %f %f?", mx, maxamp)
  end
  revert_sound(ind)
  rd = make_sample_reader(0)
  grn = make_granulate(:expansion, 2.0)
  map_channel(lambda do |y|
                granulate(grn,
                          lambda do |dir|
                            rd.call
                          end,
                          lambda do |g|
                            mus_data(g).map! do |val| val *= 2.0 end
                            0
                          end)
                end)
  if maxamp / mx < 1.5 or mx / maxamp > 2.5
    snd_display("gran edit 2* (2): %f %f?", mx, maxamp)
  end
  undo_edit
  rd = make_sample_reader(0)
  grn = make_granulate(:expansion, 2.0)
  map_channel(lambda do |y|
                granulate(grn,
                          lambda do |dir|
                            rd.call
                          end,
                          lambda do |g|
                            mus_data(g).map! do |val| val *= 4.0 end
                            0
                          end)
                end)
  if maxamp / mx < 3.0 or mx / maxamp > 6.0
    snd_display("gran edit 4* (2): %f %f?", mx, maxamp)
  end
  close_sound(ind)
  ind = open_sound("oboe.snd")
  grn = make_granulate(:expansion, 2.0, :length, 0.01, :hop, 0.05)
  rd = make_sample_reader(0)
  map_channel(lambda do |y| granulate(grn, lambda do |dir| rd.call end) end)
  if (res = maxamp) > 0.2
    snd_display("trouble in granulate len 0.01 hop 0.05: %f?", res)
  end
  undo_edit
  grn = make_granulate(:expansion, 2.0, :length, 0.04, :hop, 0.05)
  rd = make_sample_reader(0)
  map_channel(lambda do |y| granulate(grn, lambda do |dir| rd.call end) end)
  if (res = maxamp) > 0.2
    snd_display("trouble in granulate len 0.04 hop 0.05: %f?", res)
  end
  undo_edit
  grn = make_granulate(:expansion, 2.0, :length, 0.01, :hop, 0.25)
  rd = make_sample_reader(0)
  map_channel(lambda do |y| granulate(grn, lambda do |dir| rd.call end) end)
  if (res = maxamp) > 0.2
    snd_display("trouble in granulate len 0.01 hop 0.25: %f?", res)
  end
  undo_edit
  grn = make_granulate(:expansion, 2.0, :length, 0.4, :hop, 0.5)
  rd = make_sample_reader(0)
  map_channel(lambda do |y| granulate(grn, lambda do |dir| rd.call end) end)
  if (res = maxamp) > 0.2
    snd_display("trouble in granulate len 0.4 hop 0.5: %f?", res)
  end
  undo_edit
  close_sound(ind)
end

def test168
  ind = new_sound(:size, 1000)
  gen = make_granulate(:jitter, 0.0, :hop, 0.004, :length, 0.001)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.06)
    snd_display("gran 0 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.007, 0.013, 0.020, 0.027, 0.033, 0.040, 0.047,
                    0.053, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.053,
                    0.047, 0.040, 0.033, 0.027, 0.020, 0.013, 0.007, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 0 data: %s?", res)
  end
  unless vequal(res = channel2vct(85, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.007, 0.013, 0.020, 0.027,
                    0.033, 0.040, 0.047, 0.053, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.053, 0.047, 0.040, 0.033, 0.027, 0.020,
                    0.013, 0.007, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 0 data 85: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.002, :length, 0.001)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.06)
    snd_display("gran 1 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.007, 0.013, 0.020, 0.027, 0.033, 0.040, 0.047,
                    0.053, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.053,
                    0.047, 0.040, 0.033, 0.027, 0.020, 0.013, 0.007, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 1 data: %s?", res)
  end
  unless vequal(res = channel2vct(40, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.007, 0.013, 0.020,
                    0.027, 0.033, 0.040, 0.047, 0.053, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.053, 0.047, 0.040, 0.033, 0.027,
                    0.020, 0.013, 0.007, 0.000, 0.000, 0.000))
    snd_display("gran 1 data 40: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.002, :length, 0.001, :ramp, 0.1)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.06)
    snd_display("gran 2 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.030, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.030, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 2 data: %s?", res)
  end
  unless vequal(res = channel2vct(40, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.030, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.030, 0.000, 0.000, 0.000))
    snd_display("gran 2 data 40: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.002, :length, 0.001, :ramp, 0.5)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.06)
    snd_display("gran 3 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.005, 0.011, 0.016, 0.022, 0.027, 0.033, 0.038,
                    0.044, 0.049, 0.055, 0.060, 0.060, 0.055, 0.049, 0.044,
                    0.038, 0.033, 0.027, 0.022, 0.016, 0.011, 0.005, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 3 data: %s?", res)
  end
  unless vequal(res = channel2vct(85, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.005, 0.011, 0.016, 0.022,
                    0.027, 0.033, 0.038, 0.044, 0.049, 0.055, 0.060, 0.060,
                    0.055, 0.049, 0.044, 0.038, 0.033, 0.027, 0.022, 0.016,
                    0.011, 0.005, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 3 data 85: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.001, :length, 0.001, :ramp, 0.5)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.06)
    snd_display("gran 4 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.005, 0.011, 0.016, 0.022, 0.027, 0.033, 0.038,
                    0.044, 0.049, 0.055, 0.060, 0.060, 0.055, 0.049, 0.044,
                    0.038, 0.033, 0.027, 0.022, 0.016, 0.011, 0.005, 0.005,
                    0.011, 0.016, 0.022, 0.027, 0.033, 0.038))
    snd_display("gran 4 data: %s?", res)
  end
  unless vequal(res = channel2vct(85, 30),
                vct(0.022, 0.016, 0.011, 0.005, 0.005, 0.011, 0.016, 0.022,
                    0.027, 0.033, 0.038, 0.044, 0.049, 0.055, 0.060, 0.060,
                    0.055, 0.049, 0.044, 0.038, 0.033, 0.027, 0.022, 0.016,
                    0.011, 0.005, 0.005, 0.011, 0.016, 0.022))
    snd_display("gran 4 data 85: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.001, :length, 0.001, :ramp, 0.25, :scaler, 1.0)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.1)
    snd_display("gran 5 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.020, 0.040, 0.060, 0.080, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.080, 0.060, 0.040, 0.020, 0.020,
                    0.040, 0.060, 0.080, 0.100, 0.100, 0.100))
    snd_display("gran 5 data: %s?", res)
  end
  unless vequal(res = channel2vct(85, 30),
                vct(0.080, 0.060, 0.040, 0.020, 0.020, 0.040, 0.060, 0.080,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.080, 0.060,
                    0.040, 0.020, 0.020, 0.040, 0.060, 0.080))
    snd_display("gran 5 data 85: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.001, :length, 0.002, :ramp, 0.5, :scaler, 1.0)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.105)
    snd_display("gran 6 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.005, 0.009, 0.014, 0.018, 0.023, 0.027, 0.032,
                    0.036, 0.041, 0.045, 0.050, 0.055, 0.059, 0.064, 0.068,
                    0.073, 0.077, 0.082, 0.086, 0.091, 0.095, 0.100, 0.105,
                    0.105, 0.105, 0.105, 0.105, 0.105, 0.105))
    snd_display("gran 6 data: %s?", res)
  end
  unless vequal(res = channel2vct(85, 30),
                vct(0.105, 0.105, 0.105, 0.105, 0.105, 0.105, 0.105, 0.105,
                    0.105, 0.105, 0.105, 0.105, 0.105, 0.105, 0.105, 0.105,
                    0.105, 0.105, 0.105, 0.105, 0.105, 0.105, 0.105, 0.105,
                    0.105, 0.105, 0.105, 0.105, 0.105, 0.105))
    snd_display("gran 6 data 85: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.001, :length, 0.005, :ramp, 0.5, :scaler, 1.0)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.264)
    snd_display("gran 7 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.002, 0.004, 0.005, 0.007, 0.009, 0.011, 0.013,
                    0.015, 0.016, 0.018, 0.020, 0.022, 0.024, 0.025, 0.027,
                    0.029, 0.031, 0.033, 0.035, 0.036, 0.038, 0.040, 0.044,
                    0.047, 0.051, 0.055, 0.058, 0.062, 0.065))
    snd_display("gran 7 data: %s?", res)
  end
  unless vequal(res = channel2vct(85, 30),
                vct(0.244, 0.244, 0.244, 0.244, 0.245, 0.247, 0.249, 0.251,
                    0.253, 0.255, 0.256, 0.258, 0.260, 0.262, 0.264, 0.264,
                    0.262, 0.260, 0.258, 0.256, 0.255, 0.253, 0.251, 0.249,
                    0.247, 0.245, 0.245, 0.247, 0.249, 0.251))
    snd_display("gran 7 data 85: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.01, :length, 0.001, :ramp, 0.5,
                       :scaler, 1.0, :expansion, 2.0)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.1)
    snd_display("gran 8 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.009, 0.018, 0.027, 0.036, 0.045, 0.055, 0.064,
                    0.073, 0.082, 0.091, 0.100, 0.100, 0.091, 0.082, 0.073,
                    0.064, 0.055, 0.045, 0.036, 0.027, 0.018, 0.009, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 8 data: %s?", res)
  end
  unless vequal(res = channel2vct(220, 30),
                vct(0.000, 0.009, 0.018, 0.027, 0.036, 0.045, 0.055, 0.064,
                    0.073, 0.082, 0.091, 0.100, 0.100, 0.091, 0.082, 0.073,
                    0.064, 0.055, 0.045, 0.036, 0.027, 0.018, 0.009, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 8 data 220: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.01, :length, 0.001, :ramp, 0.5,
                       :scaler, 1.0, :expansion, 0.5)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.1)
    snd_display("gran 9 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.000, 0.009, 0.018, 0.027, 0.036, 0.045, 0.055, 0.064,
                    0.073, 0.082, 0.091, 0.100, 0.100, 0.091, 0.082, 0.073,
                    0.064, 0.055, 0.045, 0.036, 0.027, 0.018, 0.009, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 9 data: %s?", res)
  end
  unless vequal(res = channel2vct(220, 30),
                vct(0.000, 0.009, 0.018, 0.027, 0.036, 0.045, 0.055, 0.064,
                    0.073, 0.082, 0.091, 0.100, 0.100, 0.091, 0.082, 0.073,
                    0.064, 0.055, 0.045, 0.036, 0.027, 0.018, 0.009, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("gran 9 data 220: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.001, :length, 0.005, :ramp, 0.5, :scaler, 1.0)
  map_channel(lambda do |y|
                granulate(gen,
                          lambda do |dir| 0.1 end,
                          lambda do |g|
                            g.data.map! do |val| val *= 2.0 end
                            0
                          end)
              end)
  if fneq(res = maxamp, 2 * 0.264)
    snd_display("gran 10 max: %f?", res)
  end
  unless vequal(vct_scale!(res = channel2vct(0, 30), 0.5),
                vct(0.000, 0.002, 0.004, 0.005, 0.007, 0.009, 0.011, 0.013,
                    0.015, 0.016, 0.018, 0.020, 0.022, 0.024, 0.025, 0.027,
                    0.029, 0.031, 0.033, 0.035, 0.036, 0.038, 0.040, 0.044,
                    0.047, 0.051, 0.055, 0.058, 0.062, 0.065))
    snd_display("gran 10 data: %s?", res)
  end
  unless vequal(vct_scale!(res = channel2vct(85, 30), 0.5),
                vct(0.244, 0.244, 0.244, 0.244, 0.245, 0.247, 0.249, 0.251,
                    0.253, 0.255, 0.256, 0.258, 0.260, 0.262, 0.264, 0.264,
                    0.262, 0.260, 0.258, 0.256, 0.255, 0.253, 0.251, 0.249,
                    0.247, 0.245, 0.245, 0.247, 0.249, 0.251))
    snd_display("gran 10 data 85: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.005, :length, 0.002, :ramp, 0.0, :scaler, 1.0)
  forward = true
  ctr = -0.5
  incr = 0.001
  map_channel(lambda do |y|
                granulate(gen,
                          lambda do |dir|
                            ctr += incr
                            ctr
                          end,
                          lambda do |g|
                            len = g.length
                            if forward
                              forward = false
                            else
                              forward = true
                              vct_reverse!(g.data, len)
                            end
                            len
                          end)
              end)
  if (res = maxamp) > 0.6
    snd_display("gran 11 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(-0.499, -0.498, -0.497, -0.496, -0.495, -0.494, -0.493, -0.492,
                    -0.491, -0.490, -0.489, -0.488, -0.487, -0.486, -0.485, -0.484,
                    -0.483, -0.482, -0.481, -0.480, -0.479, -0.478, -0.477, -0.476,
                    -0.475, -0.474, -0.473, -0.472, -0.471, -0.470))
    snd_display("gran 11 data: %s?", res)
  end
  unless vequal(res = channel2vct(100, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, -0.345, -0.346, -0.347, -0.348, -0.349, -0.350,
                    -0.351, -0.352, -0.353, -0.354, -0.355, -0.356, -0.357, -0.358,
                    -0.359, -0.360, -0.361, -0.362, -0.363, -0.364))
    snd_display("gran 11 data 100: %s?", res)
  end
  undo_edit
  ctr = -0.5
  incr = 0.001
  gen = make_granulate(:jitter, 0.0, :hop, 0.005, :length, 0.002, :ramp, 0.0, :scaler, 1.0,
                       :input, lambda do |dir|
                         ctr += incr
                         ctr
                       end)
  map_channel(lambda do |y| granulate(gen) end)
  if (res = maxamp) > 0.6
    snd_display("gran 12 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(-0.499, -0.498, -0.497, -0.496, -0.495, -0.494, -0.493, -0.492,
                    -0.491, -0.490, -0.489, -0.488, -0.487, -0.486, -0.485, -0.484,
                    -0.483, -0.482, -0.481, -0.480, -0.479, -0.478, -0.477, -0.476,
                    -0.475, -0.474, -0.473, -0.472, -0.471, -0.470))
    snd_display("gran 12 data: %s?", res)
  end
  unless vequal(res = channel2vct(100, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, -0.389, -0.388, -0.387, -0.386, -0.385, -0.384,
                    -0.383, -0.382, -0.381, -0.380, -0.379, -0.378, -0.377, -0.376,
                    -0.375, -0.374, -0.373, -0.372, -0.371, -0.370))
    snd_display("gran 12 data 100: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.001, :length, 0.005, :ramp, 0.5, :scaler, 1.0,
                       :input, lambda do |dir| 0.1 end,
                       :edit, lambda do |g|
                         g.data.map! do |val| val *= 2.0 end
                         0
                       end)

  map_channel(lambda do |y| granulate(gen) end)
  if (res = maxamp) > 0.6
    snd_display("gran 13 max: %f?", res)
  end
  unless vequal(vct_scale!(res = channel2vct(0, 30), 0.5),
                vct(0.000, 0.002, 0.004, 0.005, 0.007, 0.009, 0.011, 0.013,
                    0.015, 0.016, 0.018, 0.020, 0.022, 0.024, 0.025, 0.027,
                    0.029, 0.031, 0.033, 0.035, 0.036, 0.038, 0.040, 0.044,
                    0.047, 0.051, 0.055, 0.058, 0.062, 0.065))
    snd_display("gran 13 data: %s?", res)
  end
  unless vequal(vct_scale!(res = channel2vct(85, 30), 0.5),
                vct(0.244, 0.244, 0.244, 0.244, 0.245, 0.247, 0.249, 0.251,
                    0.253, 0.255, 0.256, 0.258, 0.260, 0.262, 0.264, 0.264,
                    0.262, 0.260, 0.258, 0.256, 0.255, 0.253, 0.251, 0.249,
                    0.247, 0.245, 0.245, 0.247, 0.249, 0.251))
    snd_display("gran 13 data 85: %s?", res)
  end
  undo_edit
  forward = true
  ctr = -0.5
  incr = 0.001
  gen = make_granulate(:jitter, 0.0, :hop, 0.005, :length, 0.002, :ramp, 0.0, :scaler, 1.0,
                       :input, lambda do |dir|
                         ctr += incr
                         ctr
                       end,
                       :edit, lambda do |g|
                         len = mus_length(g)
                         if forward
                           forward = false
                         else
                           forward = true
                           vct_reverse!(mus_data(g), len)
                         end
                         len
                       end)

  map_channel(lambda do |y| granulate(gen) end)
  if (res = maxamp) > 0.6
    snd_display("gran 14 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(-0.499, -0.498, -0.497, -0.496, -0.495, -0.494, -0.493, -0.492,
                    -0.491, -0.490, -0.489, -0.488, -0.487, -0.486, -0.485, -0.484,
                    -0.483, -0.482, -0.481, -0.480, -0.479, -0.478, -0.477, -0.476,
                    -0.475, -0.474, -0.473, -0.472, -0.471, -0.470))
    snd_display("gran 14 data: %s?", res)
  end
  unless vequal(res = channel2vct(100, 30),
                vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, -0.345, -0.346, -0.347, -0.348, -0.349, -0.350,
                    -0.351, -0.352, -0.353, -0.354, -0.355, -0.356, -0.357, -0.358,
                    -0.359, -0.360, -0.361, -0.362, -0.363, -0.364))
    snd_display("gran 14 data 100: %s?", res)
  end
  undo_edit
  #
  gen = make_granulate(:jitter, 0.0, :hop, 0.004, :length, 0.001, :ramp, 0.0)
  e = make_env(:envelope, [0, 0, 1, 0.5], :end, 1000)
  base_ramp_len = mus_length(gen)
  map_channel(lambda do |y|
                result = granulate(gen, lambda do |dir| 0.1 end)
                set_mus_ramp(gen, (base_ramp_len * env(e)).round)
                result
              end)
  if fneq(res = maxamp, 0.06)
    snd_display("granf 0 max: %f?", res)
  end
  if (mus_ramp(gen) - 0.5 * mus_length(gen)).abs > 1
    snd_display("granf 0 ramp: %s %s?", gen.ramp, gen.length)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 0 data: %s?", res)
  end
  unless vequal(res = channel2vct(440, 30),
                vct(0.000, 0.012, 0.024, 0.036, 0.048, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.048, 0.036, 0.024, 0.012, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 0 data 440: %s?", res)
  end
  unless vequal(res = channel2vct(880, 30),
                vct(0.000, 0.006, 0.012, 0.018, 0.024, 0.030, 0.036, 0.042,
                    0.048, 0.054, 0.060, 0.060, 0.060, 0.060, 0.054, 0.048,
                    0.042, 0.036, 0.030, 0.024, 0.018, 0.012, 0.006, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 0 data 880: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.004, :length, 0.001, :ramp, 0.0)
  e = make_env(:envelope, [0, 1, 1, 0.25], :end, 1000)
  base_hop_len = mus_hop(gen)
  map_channel(lambda do |y|
                result = granulate(gen, lambda do |dir| 0.1 end)
                set_mus_hop(gen, (base_hop_len * env(e)).round)
                result
              end)
  if fneq(res = maxamp, 0.06)
    snd_display("granf 1 max: %f?", res)
  end
  if (mus_hop(gen) - 0.001 * srate()).abs > 1
    snd_display("granf 1 hop: %s?", gen.hop)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 1 data: %s?", res)
  end
  unless vequal(res = channel2vct(900, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060))
    snd_display("granf 1 data 900: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.004, :length, 0.001, :ramp, 0.0)
  e = make_env(:envelope, [0, 1, 1, 0.25], :end, 1000)
  base_freq = mus_frequency(gen)
  map_channel(lambda do |y|
                result = granulate(gen, lambda do |dir| 0.1 end)
                set_mus_frequency(gen, base_freq * env(e))
                result
              end)
  if fneq(res = maxamp, 0.06)
    snd_display("granf 2 max: %f?", res)
  end
  if (mus_hop(gen) - 0.001 * srate()).abs > 1
    snd_display("granf 2 hop: %s?", gen.hop)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 2 data: %s?", res)
  end
  unless vequal(res = channel2vct(900, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060))
    snd_display("granf 2 data 900: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.002, :length, 0.001, :ramp, 0.0, :scaler, 1.0)
  base_freq = mus_frequency(gen)
  map_channel(lambda do |y| granulate(gen, lambda do |dir| 0.1 end) end)
  if fneq(res = maxamp, 0.1)
    snd_display("granf 3 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 3 data: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.004, :length, 0.001, :ramp, 0.0, :scaler, 1.0)
  e = make_env(:envelope, [0, 1, 1, 0], :end, 1000)
  base_freq = mus_frequency(gen)
  map_channel(lambda do |y|
                result = granulate(gen, lambda do |dir| 0.1 end)
                set_mus_scaler(gen, env(e))
                result
              end)
  if fneq(res = maxamp, 0.1)
    snd_display("granf 4 max: %f?", res)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100,
                    0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.100, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 4 data: %s?", res)
  end
  unless vequal(res = channel2vct(440, 30),
                vct(0.056, 0.056, 0.056, 0.056, 0.056, 0.056, 0.056, 0.056,
                    0.056, 0.056, 0.056, 0.056, 0.056, 0.056, 0.056, 0.056,
                    0.056, 0.056, 0.056, 0.056, 0.056, 0.056, 0.056, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 4 data 440: %s?", res)
  end
  unless vequal(res = channel2vct(900, 30),
                vct(0.012, 0.012, 0.012, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 4 data 900: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.006, :length, 0.001, :ramp, 0.0, :max_size, 2200)
  e = make_env(:envelope, [0, 1, 1, 5], :end, 1000)
  base_len = mus_length(gen)
  map_channel(lambda do |y|
                result = granulate(gen, lambda do |dir| 0.1 end)
                set_mus_length(gen, (base_len * env(e)).round)
                result
              end)
  if fneq(res = maxamp, 0.06)
    snd_display("granf 5 max: %f?", res)
  end
  if (gen.length - 5 * base_len).abs > 10
    snd_display("granf 5 length: %s %s?", mus_length(gen), 5 * base_len)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 5 data: %s?", res)
  end
  unless vequal(res = channel2vct(440, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 5 data 440: %s?", res)
  end
  unless vequal(res = channel2vct(800, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060))
    snd_display("granf 5 data 800: %s?", res)
  end
  undo_edit
  gen = make_granulate(:jitter, 0.0, :hop, 0.006, :length, 0.005, :ramp, 0.0, :max_size, 2200)
  e = make_env(:envelope, [0, 1, 1, 0.2], :end, 1000)
  base_len = mus_length(gen)
  map_channel(lambda do |y|
                result = granulate(gen, lambda do |dir| 0.1 end)
                set_mus_length(gen, (base_len * env(e)).round)
                result
              end)
  if fneq(res = maxamp, 0.06)
    snd_display("granf 6 max: %f?", res)
  end
  if (gen.length - 0.2 * base_len).abs > 4
    snd_display("granf 6 length: %s %s?", mus_length(gen), 0.2 * base_len)
  end
  unless vequal(res = channel2vct(0, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.060))
    snd_display("granf 6 data: %s?", res)
  end
  unless vequal(res = channel2vct(820, 30),
                vct(0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060,
                    0.060, 0.060, 0.060, 0.060, 0.060, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000))
    snd_display("granf 6 data 820: %s?", res)
  end
  undo_edit
  fname = file_name(ind)
  close_sound(ind)
  delete_file(fname)
end

def test178
  ind = new_sound("tmp.snd", Mus_next, Mus_bfloat, 22050, 1, :size, 10000)
  gen = make_granulate(:expansion, 20.0,
                       :input, lambda do |dir| 0.01 end,
                       :length, 0.00995,
                       :hop, 0.01,
                       :ramp, 0.0,
                       :scaler, 1.0,
                       :jitter, 0.0)
  clm_channel(gen)                                # 0.01 max stable
  snd_display("granulate stable 1: %f?", maxamp) if fneq(maxamp, 0.01)
  if minval = scan_channel(lambda do |y| y < 0.0099 end)
    snd_display("granulate stable 1 min: %s?", minval)
  end
  undo_edit
  gen = make_granulate(:expansion, 20.0,
                       :input, lambda do |dir| 0.1 end,
                       :length, 0.00995,
                       :hop, 0.01,
                       :ramp, 0.0,
                       :scaler, 0.5,
                       :jitter, 0.0)
  clm_channel(gen)                                # 0.05 max stable
  snd_display("granulate stable 2: %f?", maxamp) if fneq(maxamp, 0.05)
  if minval = scan_channel(lambda do |y| y < 0.0499 end)
    snd_display("granulate stable 2 min: %s?", minval)
  end
  undo_edit
  gen = make_granulate(:expansion, 20.0,
                       :input, lambda do |dir| 0.05 end,
                       :length, 0.099975,
                       :hop, 0.1,
                       :ramp, 0.0,
                       :scaler, 1.0,
                       :jitter, 0.0)
  clm_channel(gen)                                # 0.05 max stable
  snd_display("granulate stable 3: %f?", maxamp) if fneq(maxamp, 0.05)
  if minval = scan_channel(lambda do |y| y < 0.0499 end)
    snd_display("granulate stable 3 min: %s %f?", minval, sample(minval[1]))
  end
  undo_edit
  ctr = 0
  gen = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir|
                         val = ctr * 0.0001
                         ctr += 1
                         val
                       end,
                       :length, 0.01,
                       :hop, 0.1,
                       :ramp, 0.0,
                       :scaler, 1.0,
                       :jitter, 0.0)
  clm_channel(gen)
  snd_display("granulate ramped 4: %f?", maxamp) if fneq(maxamp, 0.462)
  vals = count_matches(lambda do |y| y != 0.0 end)
  if (vals - 1104).abs > 10
    snd_display("granulate ramped 4 not 0.0: %s?", vals)
  end
  if (not vequal(res1 = channel2vct(2203, 10),
                 vct(0.000, 0.000, 0.110, 0.110, 0.110, 0.111, 0.111, 0.111, 0.111, 0.111))) or
      (not vequal(res2 = channel2vct(4523, 10),
                  vct(0.232, 0.232, 0.232, 0.232, 0.232, 0.232, 0.232, 0.232, 0.233, 0.233))) or
      (not vequal(res3 = channel2vct(8928, 10),
                  vct(0.452, 0.452, 0.452, 0.452, 0.452, 0.452, 0.452, 0.452, 0.452, 0.452)))
    snd_display("granulate ramped 4 data off: %s %s %s?", res1, res2, res3)
  end
  undo_edit
  ctr = 0
  gen = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir|
                         val = ctr * 0.0001
                         ctr += 1
                         val
                       end,
                       :length, 0.00995,
                       :hop, 0.01,
                       :ramp, 0.0,
                       :scaler, 1.0,
                       :jitter, 0.0)
  clm_channel(gen)
  snd_display("granulate ramped 5: %f?", maxamp) if fneq(maxamp, 0.505)
  vals = count_matches(lambda do |y| y != 0.0 end)
  mxoff = 0.0
  mx = maxamp
  len = frames
  cur = 0.0
  incr = mx / len
  scan_channel(lambda do |y|
                 diff = (cur - y).abs
                 if diff > mxoff
                   mxoff = diff
                 end
                 cur += incr
                 false
               end)
  snd_display("granulate ramped 5 mxoff: %f?", mxoff) if mxoff > 0.02
  undo_edit
  ctr = 0
  gen = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir|
                         val = ctr * 0.0001
                         ctr += 1
                         val
                       end,
                       :length, 0.00995,
                       :hop, 0.01,
                       :ramp, 0.5,
                       :scaler, 1.0,
                       :jitter, 0.0)
  clm_channel(gen)
  snd_display("granulate ramped 6: %f?", maxamp) if fneq(maxamp, 0.495)
  if (not vequal(res1 = channel2vct(2000, 10),
                 vct(0.018, 0.019, 0.020, 0.021, 0.022, 0.023, 0.024, 0.025, 0.026, 0.027))) or
      (not vequal(res2 = channel2vct(8000, 10),
                  vct(0.294, 0.298, 0.301, 0.305, 0.309, 0.313, 0.316, 0.320, 0.324, 0.328)))
    snd_display("granulate ramped 6 data: %s %s?", res1, res2)
  end
  undo_edit
  ctr = 0
  gen = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir|
                         val = ctr * 0.0001
                         ctr += 1
                         val
                       end,
                       :length, 0.00995,
                       :hop, 0.01,
                       :ramp, 0.25,
                       :scaler, 1.0,
                       :jitter, 0.0)
  clm_channel(gen)
  snd_display("granulate ramped 7: %f?", maxamp) if fneq(maxamp, 0.505)
  if (not vequal(res1 = channel2vct(2000, 10),
                 vct(0.037, 0.039, 0.040, 0.042, 0.044, 0.046, 0.048, 0.050, 0.052, 0.054))) or
      (not vequal(res2 = channel2vct(8000, 10),
                  vct(0.404, 0.404, 0.404, 0.404, 0.404, 0.405, 0.405, 0.405, 0.405, 0.405)))
    snd_display("granulate ramped 7 data: %s %s?", res1, res2)
  end
  undo_edit
  ctr = 0
  gen = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir|
                         val = ctr * 0.0001
                         ctr += 1
                         val
                       end,
                       :length, 0.05,
                       :hop, 0.01,
                       :ramp, 0.25,
                       :scaler, 0.1,
                       :jitter, 0.0)
  clm_channel(gen)
  snd_display("granulate ramped 8: %f?", maxamp) if fneq(maxamp, 0.201)
  mxoff = 0.0
  mx = maxamp
  len = frames
  cur = 0.0
  incr = mx / len
  scan_channel(lambda do |y|
                 diff = (cur - y).abs
                 if diff > mxoff
                   mxoff = diff
                 end
                 cur += incr
                 false
               end)
  snd_display("granulate ramped 8 mxoff: %f?", mxoff) if mxoff > 0.01
  undo_edit
  ctr = 0
  gen = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir|
                         val = ctr * 0.0001
                         ctr += 1
                         val
                       end,
                       :length, 0.1,
                       :hop, 0.01,
                       :ramp, 0.1,
                       :scaler, 0.1,
                       :jitter, 0.0)
  clm_channel(gen)
  snd_display("granulate ramped 9: %f?", maxamp) if fneq(maxamp, 0.501)
  mxoff = 0.0
  mx = maxamp
  len = frames() - 2000
  cur = sample(2000)
  incr = (mx - cur) / len
  scan_channel(lambda do |y|
                 diff = (cur - y).abs
                 if diff > mxoff
                   mxoff = diff
                 end
                 cur += incr
                 false
               end, 2000)
  snd_display("granulate ramped 9 mxoff: %f?", mxoff) if mxoff > 0.001
  undo_edit
  ctr = 0
  gen = make_granulate(:expansion, 2.0,
                       :input, lambda do |dir|
                         val = ctr * 0.0001
                         ctr += 1
                         val
                       end,
                       :length, 0.4,
                       :hop, 0.01,
                       :ramp, 0.4,
                       :scaler, 0.025,
                       :jitter, 0.0)
  clm_channel(gen)
  snd_display("granulate ramped 10: %f?", maxamp) if fneq(maxamp, 0.433)
  undo_edit
  close_sound(ind)
end

def test188
  v0 = make_vct(32)
  v1 = make_vct(256)
  v2 = make_vct(256)
  v01 = make_vct(32)
  v11 = make_vct(256)
  v21 = make_vct(256)
  1.upto(15) do |i| v0[i] = v01[i] = 1.0 / i end
  v1[0] = v11[0] = 1.0
  gen = make_convolve(:filter, v0)
  gen1 = make_convolve(:filter, v01)
  n = n1 = -1
  print_and_check(gen, "convolve", "convolve: size: 64")
  snd_display("%s not convolve?", gen) unless convolve?(gen)
  genx = gen1
  snd_display("convolve %s.eql?(%s)", genx, gen1) unless genx.eql?(gen1)
  snd_display("convolve %s.eql?(%s)", gen, gen1) if gen.eql?(gen1)
  snd_display("convolve fft len: %d?", mus_length(gen)) if mus_length(gen) != 64
  128.times do |i|
    v2[i] = convolve(gen, lambda do |dir|
                       n += 1
                       v1[n]
                     end)
  end
  vct_map!(v21, lambda do | |
             if convolve?(gen1)
               convolve(gen1, lambda do |dir|
                          n1 += 1
                          v11[n1]
                        end)
             else
               -1.0
             end
           end)
  snd_display("run gran: %s %s?", v2, v21) unless vequal(v2, v21)
  if fneq(v2[0], 0.0) or fneq(v2[1], 1.0) or fneq(v2[4], 0.25) or fneq(v2[7], 0.143)
    snd_display("convolve output: %s?", v2)
  end
  if (res = snd_catch do convolve(gen, lambda do |a, b| a end) end).first != :bad_arity
    snd_display("convolve bad func: %s", res.inspect)
  end
  convolve_files("oboe.snd", "fyow.snd", 0.5, "fmv.snd")
  if fneq(res = mus_sound_maxamp("fmv.snd")[1], 0.5)
    snd_display("convolve_files: %f != 0.5?", res)
  end
  #
  fd = mus_sound_open_input("oboe.snd")
  chans = mus_sound_chans("oboe.snd")
  data = make_sound_data(chans, 2000)
  snd_display("%s not sound_data?", data) unless sound_data?(data)
  snd_display("sound_data chans: %d?", data.chans) if sound_data_chans(data) != 1
  snd_display("sound_data length: %d?", sound_data_length(data)) if data.length != 2000
  mus_sound_read(fd, 0, 1999, chans, data)
  mus_sound_close_input(fd)
  if fneq(res = data[0, 1497], 0.02893066)
    snd_display("mus_sound_read: %f?", res)
  end
  #
  ind = new_sound("fmv.snd")
  set_sample(1, 0.1)
  save_sound(ind)
  if edits(ind, 0) != [0, 0]
    snd_display("weird: edits not cleared after save_sound: %s?", edits(ind, 0))
  end
  close_sound(ind)
  ind = open_sound("fmv.snd")
  if frames(ind, 0) != 2
    snd_display("save_sound 2 samps: %s?", frames(ind, 0))
  end
  if fneq(sample(0), 0.0) or fneq(sample(1), 0.1)
    snd_display("save_sound: %f %f?", sample(0), sample(1))
  end
  3.upto(5) do |i|
    set_sample(i, i * 0.1)
    save_sound(ind)
    if edits(ind, 0) != [0, 0]
      snd_display("weird: edits not cleared after save_sound %d: %s?", i, edits(ind, 0))
    end
    close_sound(ind)
    ind = open_sound("fmv.snd")
    if frames(ind, 0) != i + 1
      snd_display("save_sound %d samps: %s?", i + 1, frames(ind, 0))
    end
    if fneq(sample(0), 0.0) or fneq(sample(1), 0.1) or fneq(sample(i), i * 0.1)
      snd_display("save_sound %d: %f %f %f?", i, sample(0), sample(1), sample(i))
    end
  end
  close_sound(ind)
  #
  ind = new_sound("test.snd", :srate, 22050, :channels, 1, :size, 1000)
  gen = make_ssb_am(100.0)
  map_channel(lambda do |y| ssb_am(gen, 0.0) end)
  snd_display("ssb_am 0.0: %f?", maxamp) if fneq(maxamp, 0.0)
  gen1 = make_oscil(220.0)
  map_channel(lambda do |y| 0.5 * oscil(gen1) end)
  gen = make_ssb_am(100.0, 100)
  map_channel(lambda do |y| ssb_am(gen, y) end)
  delete_samples(0, 200)
  if defined? asin  # ruby 1.6.6 hasn't arcus functions
    gen1 = make_oscil(320.0, :initial_phase, asin(2.0 * sample(0)))
    map_channel(lambda do |y| y - 0.5 * oscil(gen1) end)
    snd_display("ssb_am cancelled: %f?", maxamp) if maxamp > 0.003
    undo_edit(3)
  else
    undo_edit(2)
  end
  gen = make_ssb_am(100.0, 100)
  map_channel(lambda do |y| ssb_am(gen, y, hz2radians(50.0)) end)
  delete_samples(0, 180)
  if defined? asin
    gen1 = make_oscil(370.0, :initial_phase, asin(2.0 * sample(0)))
    map_channel(lambda do |y| y - 0.5 * oscil(gen1) end)
    snd_display("ssb_am fm cancelled: %f?", maxamp) if maxamp > 0.003
  end
  close_sound(ind)
  #
  if defined? mus_ssb_bank             # not defined if --with-modules
    bands = make_array(3) do make_bandpass(hz2radians(500.0), hz2radians(600.0), 10) end
    ssbs = make_array(3) do make_ssb_am(100.0 + rbm_random(400.0)) end
    mus_ssb_bank(ssbs, bands, 0.1, 3)
  end
  #
  ind = new_sound("test.snd", :srate, 22050, :channels, 1, :size, 1000)
  ctr = 0
  map_channel(lambda do |y|
                val = sin((TWO_PI * ctr) / 50)
                ctr += 1
                val
              end)
  ssb_bank(441, 882, 1, 100)
  delete_samples(0, 217)
  if defined? asin
    gen1 = make_oscil(882.0, :initial_phase, asin(sample(0)))
    map_channel(lambda do |y| y - oscil(gen1) end)
    snd_display("ssb_bank cancelled: %f?", maxamp) if maxamp > 0.04
  end
  close_sound(ind)
  #
  if $rbm_output
    snd_display("$rbm_output %s", $rbm_output)
    $rbm_output = nil
  end
  nind = new_sound("fmv.snd", Mus_aifc, Mus_bshort, 22050, 1, "this is a comment")
  time = fm_violin_1(0, 1, 440, 0.1)
  play_and_wait(0, nind)
  save_sound(nind)
  snd_display("save_sound clobbered %s?", nind) unless sound?(nind)
  oboe_index = (find_sound("oboe.snd") or open_sound("oboe.snd"))
  snd_display("find_sound found bogus case: %s?", oboe_index) if oboe_index == nind
  cnvtest(oboe_index, nind, 0.1)
  select_sound(nind)
  select_channel(0)
  snd_display("selected_sound: %s?", selected_sound) if selected_sound != nind
  snd_display("selected_channel: %s?", selected_channel) if selected_channel != 0
  jc_reverb_1(1.0, false, 0.1, false)
  play_and_wait(0, nind)
  voiced2unvoiced(1.0, 256, 2.0, 2.0)
  map_chan(fltit)
  close_sound(oboe_index)
  snd_display("close_sound clobbered %s?", nind) unless sound?(nind)
  fr = frames(nind, 0)
  10.times do
    delete_samples(10, 100, nind, 0)
    save_sound(nind)
  end
  snd_display("delete_samples: %d %d?", fr, frames(nind, 0)) if frames(nind, 0) != fr - 1000
  revert_sound(nind)
  close_sound(nind)
  delete_file("fmv.snd")
  nind = new_sound("fmv.snd")
  if (res1 = header_type(nind)) != (res2 = default_output_type)
    snd_display("new_sound default header_type: %s %s?",
                mus_header_type_name(res1),
                mus_header_type_name(res2))
  end
  if (res1 = data_format(nind)) != (res2 = default_output_format)
    snd_display("new_sound default data_format: %s %s?",
                mus_data_format_name(res1),
                mus_data_format_name(res2))
  end
  if (res1 = channels(nind)) != (res2 = default_output_chans)
    snd_display("new_sound default chans: %s %s?", res1, res2)
  end
  if (res1 = srate(nind)) != (res2 = default_output_srate)
    snd_display("new_sound default srate: %s %s?", res1, res2)
  end
  close_sound(nind)
  delete_file("fmv.snd")
end

def test198
  nind = new_sound("fmv.snd", Mus_nist, Mus_bshort, 22050, 1, "this is a comment")
  set_sample(0, 1.0, nind)
  start_progress_report(nind)
  convolve_with("oboe.snd")
  progress_report(0.1, "hiho", 0, 1, nind)
  snd_display("convolve_with: %f?", sample(1000)) if fneq(sample(1000), -0.22299)
  progress_report(0.3, "hiho", 0, 1, nind)
  revert_sound(nind)
  progress_report(0.5, "hiho", 0, 1, nind)
  set_sample(200, 0.0001)
  set_sample(100, 1.0)
  progress_report(0.8, "hiho", 0, 1, nind)
  smooth_sound(0, 100)
  finish_progress_report(nind)
  if fneq(sample(50), 0.5) or fneq(sample(30), 0.20608) or fneq(sample(90), 0.9755)
    snd_display("smooth: %f %f %f?", sample(50), sample(30), sample(90))
  end
  undo_edit
  set_sinc_width(40)
  set_sample(100, 0.5)
  snd_display("set_sample(100): %f?", sample(100)) if fneq(sample(100), 0.5)
  src_sound(0.1)
  if fneq(sample(1000), 0.5) or fneq(sample(1024), 0.0625) or fneq(sample(1010), 0.0)
    snd_display("src_sound: %f %f %f?", sample(100), sample(1024), sample(1010))
  end
  revert_sound(nind)
  close_sound(nind)
  # 
  nind = new_sound("fmv.snd", Mus_riff, Mus_lshort, 22050, 1, "this is a comment", 22050)
  snd_display("new_sound initial_length: %d?", frames(nind)) if frames(nind) != 22050
  mix("pistol.snd")
  map_chan(expsrc(2.0, nind))
  undo_edit
  eds = edits
  snd_display("undo edits: %s?", eds) if eds[0] != 1 or eds[1] != 1
  snd_display("undo edit_position: %d %s?", edit_position, eds) if edit_position != eds[0]
  expsnd([0, 1, 2, 0.4])
  map_chan(comb_chord(0.95, 100, 0.3))
  map_chan(formants(0.99, 900, 0.02, 1800, 0.01, 2700))
  map_chan(moving_formant(0.99, [0, 1200, 1, 2400]))
  scale_to(0.3)
  eds = edits
  snd_display("edits(6): %s?", eds) if eds[0] != 6 or eds[1] != 0
  snd_display("edit_position(6): %d %s?", edit_position, eds) if edit_position != eds[0]
  set_edit_position(1)
  snd_display("set_edit_position(1): %d?", edit_position) if edit_position != 1
  set_edit_position(4)
  snd_display("set_edit_position(4): %d?", edit_position) if edit_position != 4
  revert_sound(nind)
  mix("pistol.snd")
  map_chan(zecho(0.5, 0.75, 6, 10.0), 0, 65000)
  map_chan(am(440))
  add_mark(1200)
  add_mark(2300)
  key(?x, 4)
  key(?c, 0)                                      # trigger mark_define_region
  reverse_sound(nind)
  revert_sound(nind)
  mid = mix_sound("pistol.snd", 0)
  if mix?(mid) and mix_home(mid) != [selected_sound, 0]
    snd_display("mix_sound mix_home: %s (%d or %d 0)?", mix_home(mid), selected_sound, nind)
  end
  hello_dentist(40.0, 0.1)
  fp(1.0, 0.3, 20)
  revert_sound(nind)
  enveloped_mix("oboe.snd", 0, [0, 0, 1, 1, 2, 0])
  pvoc(:pitch, 0.5, :time, 1.0, :snd, nind)
  revert_sound(nind)
  close_sound(nind)
end

def mus_mix_1(outf, inf, outloc = 0, frms = false, inloc = 0, mx = false, envs = false)
  unless number?(frms)
    frms = mus?(inf) ? mus_length(inf) : mus_sound_frames(inf)
  end
  mus_mix(outf, inf, outloc, frms, inloc, mx, envs)
  mus?(outf) and mus_close(outf)
end

def test208
  make_mix_output = lambda do |name, i|
    if i == 0 or i == 1
      name
    else
      continue_sample2file(name)
    end
  end
  make_mix_input = lambda do |name, i|
    if i == 0 or i == 2
      name
    else
      make_file2frame(name)
    end
  end
  4.times do |k|
    delete_files("fmv.snd", "fmv1.snd", "fmv2.snd", "fmv3.snd")
    v0 = make_vct(12)
    vct_fill!(v0, 0.1)
    array2file("fmv1.snd", v0, 12, 22050, 1)
    vct_fill!(v0, 0.2)
    array2file("fmv2.snd", v0, 12, 22050, 2)
    vct_fill!(v0, 0.3)
    array2file("fmv3.snd", v0, 12, 22050, 4)
    v0.map_with_index! do |val, i| i * 0.01 end
    array2file("fmv.snd", v0, 12, 22050, 1)
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("fmv1.snd", k))
    file2array("fmv.snd", 0, 0, 12, v0)
    v0.each_with_index do |val, i|
      if fneq(val, 0.1 + i * 0.01)
        snd_display("%d mus_mix(1->1): %s?", k, v0)
        break
      end
    end
    mus_mix_1(make_mix_output.call("fmv.snd", k),
              make_mix_input.call("fmv2.snd", k),
              3, 9, 0, make_mixer(2, 0.3, 0.0, 0.7, 0.0))
    file2array("fmv.snd", 0, 0, 12, v0)
    if fneq(v0[0], 0.1) or fneq(v0[3], 0.33) or fneq(v0[9], 0.19)
      snd_display("%d mus_mix(2->1): %s?", k, v0)
    end
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("fmv3.snd", k))
    file2array("fmv.snd", 0, 0, 12, v0)
    if fneq(v0[0], 0.4) or fneq(v0[3], 0.33)
      snd_display("%d mus_mix(4->1): %s?", k, v0)
    end
    vf = make_array(1) do
      make_array(1) do
        make_env(:envelope, [0, 0, 1, 1], :end, 10)
      end
    end
    mus_mix_1(make_mix_output.call("fmv.snd", k),
              make_mix_input.call("fmv1.snd", k),
              0, 12, 0, make_mixer(1, 1.0), vf)
    file2array("fmv.snd", 0, 0, 12, v0)
    if fneq(v0[0], 0.4) or fneq(v0[3], 0.36) or fneq(v0[9], 0.28)
      snd_display("%d mus_mix(env): %s?", k, v0)
    end
    mus_mix_1(make_mix_output.call("fmv.snd", k),
              make_mix_input.call("fmv2.snd", k),
              0, 12, 0, make_mixer(2, 1.0, 1.0, 1.0, 1.0), vf)
    vf = make_array(2)
    vf1 = make_array(2)
    vf2 = make_array(2)
    vf[0] = vf1
    vf[1] = vf2
    vf1[0] = make_env(:envelope, [0, 0, 1, 1], :end, 9)
    vf2[1] = make_env(:envelope, [0, 0, 1, 1], :end, 9)
    mus_mix_1(make_mix_output.call("fmv.snd", k),
              make_mix_input.call("fmv2.snd", k),
              0, 12, 0, make_mixer(2, 1.0, 1.0, 1.0, 1.0), vf)
    if (res = snd_catch do
          vf[0] = make_oscil
          mus_mix_1(make_mix_output.call("fmv.snd", k),
                    make_mix_input.call("fmv2.snd", k),
                    0, 12, 0, make_mixer(2, 1.0, 1.0, 1.0, 1.0), vf)
        end).first != :bad_type
      snd_display("%d mix w oscil-array: %s", k, res.inspect)
    end
    vf1[0] = make_env(:envelope, [0, 0, 1, 1], :end, 9)
    vf2[1] = make_env(:envelope, [0, 0, 1, 1], :end, 9)
    if (res = snd_catch do
          vf1[0] = make_oscil
          vf2[1] = sqrt(-1.0)
          mus_mix_1(make_mix_output.call("fmv.snd", k),
                    make_mix_input.call("fmv2.snd", k),
                    0, 12, 0, make_mixer(2, 1.0, 1.0, 1.0, 1.0), vf)
        end).first != :bad_type
      snd_display("%d mix w oscil-env: %s", k, res.inspect)
    end
    delete_file("fmv.snd")
    v0.map_with_index! do |val, i| i * 0.01 end
    array2file("fmv.snd", v0, 12, 22050, 4)
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("fmv1.snd", k))
    file2array("fmv.snd", 0, 0, 3, v0)            # chan 0 start 0 len 3
    if fneq(v0[0], 0.1) or fneq(v0[2], 0.18)
      snd_display("%d mus_mix(1->4): %s?", k, v0)
    end
    mus_mix_1(make_mix_output.call("fmv.snd", k),
              make_mix_input.call("fmv2.snd", k),
              0, 3, 0, make_mixer(2, 0.3, 0.0, 0.7, 0.0))
    file2array("fmv.snd", 0, 0, 3, v0)
    if fneq(v0[0], 0.3) or fneq(v0[2], 0.38)
      snd_display("%d mus_mix(2->4): %s?", k, v0)
    end
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("fmv3.snd", k), 0, 2, 0)
    file2array("fmv.snd", 0, 0, 3, v0)
    if fneq(v0[0], 0.6) or fneq(v0[2], 0.38)
      snd_display("%d mus_mix(4->4): %s?", k, v0)
    end
    #
    delete_file("fmv.snd")
    v0 = make_vct(12)
    len = mus_sound_frames("oboe.snd")
    array2file("fmv.snd", v0, 12, 22050, 1)
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("oboe.snd", k))
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("oboe.snd", k),
              0, len, 0, make_mixer(1, 0.5))
    egen = make_array(1)
    outv = make_array(1)
    outv[0] = egen
    egen[0] = make_env(:envelope, [0, 0, 1, 1], :end, len)
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("oboe.snd", k),
              0, len, 0, false, outv)
    egen[0] = make_env(:envelope, [0, 1, 1, 0], :end, len)
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("oboe.snd", k),
              0, len, 0, make_mixer(1, 1.0), outv)
    ind_oboe = open_sound("oboe.snd")
    ind_mix = open_sound("fmv.snd")
    unless vequal(res1 = channel2vct(1000, 10, ind_oboe),
                  res2 = vct_scale!(channel2vct(1000, 10, ind_mix), 1.0 / 2.5))
      snd_display("%d mus_mix 1 chan:\n# %s\n# %s?", k, res1, res2)
    end
    close_sound(ind_oboe)
    close_sound(ind_mix)
    # 
    delete_file("fmv.snd")
    v0 = make_vct(12)
    len = mus_sound_frames("2.snd")
    array2file("fmv.snd", v0, 12, 22050, 2)
    if (res = mus_sound_chans("fmv.snd")) != 2
      snd_display("%d array2file chans %s?", k, res)
    end
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("2.snd", k))
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("2.snd", k),
              0, len, 0, make_mixer(2, 0.5, 0.0, 0.0, 0.5))
    egen0 = make_array(2)
    egen1 = make_array(2)
    outv = make_array(2)
    outv[0] = egen0
    outv[1] = egen1
    egen0[0] = make_env(:envelope, [0, 0, 1, 1], :end, len)
    egen1[1] = make_env(:envelope, [0, 0, 1, 1], :end, len)
    mus_mix_1(make_mix_output.call("fmv.snd", k), make_mix_input.call("2.snd", k),
              0, len, 0, false, outv)
    ind_mix = open_sound("fmv.snd")
    if channels(ind_mix) != 2
      snd_display("%d fmv re-read chans %d %d?", k, mus_sound_chans("fmv.snd"), channels(ind_mix))
    end
    unless vequal(res = channel2vct(1000, 10, ind_mix, 0),
                  vct(0.003, 0.010, 0.012, 0.011, 0.008, 0.004, 0.002, 0.002, 0.007, 0.017))
      snd_display("%d mus_mix 2 chan (2.snd written: %s):\n# %s\n# %s?",
                  k,
                  Time.at(mus_sound_write_date("2.snd")).localtime.strftime("%d-%b %H:%M %Z"),
                  res,
                  channel2vct(1000, 10, ind_mix, 1))
    end
    close_sound(ind_mix)
    delete_file("fmv.snd")
  end
end

def test218
  gen = make_phase_vocoder(false, 512, 4, 256, 1.0, false, false, false)
  if fneq((res = snd_catch do phase_vocoder(gen) end).first, 0.0)
    snd_display("simple no-in pv call: %s", res.inspect)
  end
  if (res = snd_catch do gen = make_phase_vocoder(:fft_size, 1234) end).first != :out_of_range
    snd_display("pv bad fft: %s?", res.inspect)
  end
  ind = open_sound("oboe.snd")
  pv = make_phase_vocoder(false, 512, 4, 128, 1.0, false, false, false)
  rd = make_sample_reader(0)
  snd_display("%s not phase_vocoder?", pv) unless phase_vocoder?(pv)
  print_and_check(pv,
                  "phase-vocoder",
                  "phase-vocoder: outctr: 128, interp: 128, filptr: 0, N: 512, D: 128, in_data: nil")
  pv = make_phase_vocoder
  set_phase_vocoder_outctr(pv, 120)
  if (res = phase_vocoder_outctr(pv)) != 120
    snd_display("pv set outctr: %d?", res)
  end
  select_sound(ind)
  map_chan(lambda do |val| phase_vocoder(pv, lambda do |dir| next_sample(rd) end) end)
  phase_vocoder_amp_increments(pv)[0] = 0.1
  if fneq(res = phase_vocoder_amp_increments(pv)[0], 0.1)
    snd_display("set_phase_vocoder_amp_increments: %f?", res)
  end
  phase_vocoder_amps(pv)[0] = 0.1
  if fneq(res = phase_vocoder_amps(pv)[0], 0.1)
    snd_display("set_phase_vocoder_amps: %f?", res)
  end
  phase_vocoder_phases(pv)[0] = 0.1
  if fneq(res = phase_vocoder_phases(pv)[0], 0.1)
    snd_display("set_phase_vocoder_phases: %f?", res)
  end
  phase_vocoder_phase_increments(pv)[0] = 0.1
  if fneq(res = phase_vocoder_phase_increments(pv)[0], 0.1)
    snd_display("set_phase_vocoder_phase_increments: %f?", res)
  end
  phase_vocoder_freqs(pv)[0] = 0.1
  if fneq(res = phase_vocoder_freqs(pv)[0], 0.1)
    snd_display("set_phase_vocoder_freqs: %f?", res)
  end
  undo_edit(1)
  free_sample_reader(rd)
  # 
  lastphases = make_vct(512)
  pv = make_phase_vocoder(false, 512, 4, 128, 1.0,
                          false,
                          lambda { |v|
                            n = mus_length(v)
                            d = mus_hop(v)
                            freqs = phase_vocoder_freqs(v)
                            pscl = 1.0 / d
                            kscl = TWO_PI / n
                            (n / 2).times do |k|
                              phasediff = freqs[k] - lastphases[k]
                              lastphases[k] = freqs[k]
                              while phasediff > PI
                                phasediff -= TWO_PI
                              end
                              while phasediff < -PI
                                phasediff += TWO_PI
                              end
                              freqs[k] = 0.5 * (pscl * phasediff + k * kscl)
                            end
                            false
                          },
                          false)
  rd = make_sample_reader(0)
  map_chan(lambda do |val| phase_vocoder(pv, lambda do |dir| next_sample(rd) end) end)
  undo_edit(1)
  free_sample_reader(rd)
  # 
  pv = make_phase_vocoder(false, 512, 4, (128 * 2.0).to_i, 1.0, false, false, false)
  rd = make_sample_reader(0)
  len = (2.0 * frames(ind)).to_i
  data = make_vct!(len) do phase_vocoder(pv, lambda do |dir| next_sample(rd) end) end
  set_samples(0, len, data)
  undo_edit(1)
  free_sample_reader(rd)
  #
  incalls = outcalls = 0
  pv = make_phase_vocoder(false,
                          512, 4, (128 * 2.0).to_i, 1.0,
                          lambda { |v, infunc|
                            incalls += 1
                            true
                          },
                          false,
                          lambda { |v|
                            outcalls += 1
                            0.0
                          })
  rd = make_sample_reader(0)
  len = (2.0 * frames(ind)).to_i
  data = make_vct!(len) do phase_vocoder(pv, lambda do |dir| next_sample(rd) end) end
  set_samples(0, len, data)
  undo_edit(1)
  free_sample_reader(rd)
  if incalls.zero? or outcalls.zero?
    snd_display("phase_vocoder incalls: %d, outcalls: %d?", incalls, outcalls)
  end
  set_phase_vocoder_outctr(pv, phase_vocoder_outctr(pv))
  if (res = snd_catch do phase_vocoder(pv, lambda do |a, b| a end) end).first != :bad_arity
    snd_display("phase_vocoder bad input func: %s", res.inspect)
  end
  if (res = snd_catch do
        make_phase_vocoder(false, 512, 4, 256, 1.0, lambda do |a, b, c| false end, false, false)
      end).first != :bad_arity
    snd_display("make_phase_vocoder bad analyze func: %s", res.inspect)
  end
  if (res = snd_catch do
        make_phase_vocoder(false, 512, 4, 256, 1.0,
                           lambda do |a, b| 0.0 end,
                           lambda do |a, b, c| false end, false)
      end).first != :bad_arity
    snd_display("make_phase_vocoder bad edit func: %s", res.inspect)
  end
  if (res = snd_catch do
        make_phase_vocoder(false, 512, 4, 256, 1.0,
                           lambda do |a, b| 0.0 end,
                           lambda do |a| false end,
                           lambda do |a, b| 0 end)
      end).first != :bad_arity
    snd_display("make_phase_vocoder bad synthesize func: %s", res.inspect)
  end
  geno = make_phase_vocoder(lambda do |dir| 0.0 end)
  genx = make_phase_vocoder(:input, lambda do |dir| 0.0 end)
  snd_display("phase_vocoder %s.eql?(%s)?", geno, genx) if geno.eql?(genx)
  snd_display("mus_frequency phase_vocoder: %f?", genx.frequency) if fneq(genx.frequency, 1.0)
  set_mus_frequency(genx, 2.0)
  snd_display("set_mus_frequency phase_vocoder: %f?", genx.frequency) if fneq(genx.frequency, 2.0)
  snd_display("mus_increment phase_vocoder: %d?", genx.increment) if genx.increment != 128
  set_mus_increment(genx, 256)
  snd_display("set_mus_increment phase_vocoder: %d?", genx.increment) if genx.increment != 256
  snd_display("mus_hop phase_vocoder: %d?", genx.hop) if genx.hop != 128
  set_mus_hop(genx, 64)
  snd_display("set_mus_hop phase_vocoder: %d?", genx.hop) if genx.hop != 64
  snd_display("mus_length phase_vocoder: %d?", genx.length) if genx.length != 512
  genxx = genx
  snd_display("phase_vocoder %s.eql?(%s)?", genxx, genx) unless genx.eql?(genxx)
  close_sound(ind)
end

def test228
  ind = open_sound("oboe.snd")
  gen = make_moog_filter(500.0, 0.1)
  snd_display("moog freq: %f?", gen.frequency) if fneq(gen.frequency, 500.0)
  snd_display("moog Q: %f?", gen.Q) if fneq(gen.Q, 0.1)
  snd_display("moog state: %s?", gen.state) unless vct?(gen.state)
  snd_display("moog A: %f?", gen.A) if fneq(gen.A, 0.0)
  snd_display("moog freqtable: %f?", gen.freqtable) if fneq(gen.freqtable, -0.861)
  vals = make_vct!(20) do |i| moog_filter(gen, i.zero? ? 1.0 : 0.0) end
  unless vequal(vals, vct(0.0, 0.0, 0.0025, 0.0062, 0.0120, 0.0198, 0.0292, 0.0398,
                          0.0510, 0.0625, 0.0739, 0.0847, 0.0946, 0.1036, 0.1113, 0.1177,
                          0.1228, 0.1266, 0.1290, 0.1301))
    snd_display("moog output: %s?", vals)
  end
  close_sound(ind)
  #
  gen = make_ssb_am(440.0)
  gen1 = make_ssb_am(440.0)
  print_and_check(gen,
                  "ssb-am",
                  "ssb-am: shift: up, sin/cos: 440.000000 Hz (0.000000 radians), order: 40")
  v0 = make_vct!(10) do ssb_am(gen, 0.0) end
  v1 = make_vct(10)
  vct_map!(v1, lambda do | | ssb_am?(gen1) ? ssb_am(gen1, 0.0) : -1.0 end)
  snd_display("map ssb_am: %s %s?", v0, v1) unless vequal(v0, v1)
  snd_display("%s not ssb_am?", gen) unless ssb_am?(gen)
  snd_display("ssb_am phase: %f?", gen.phase) if fneq(gen.phase, 1.253787)
  snd_display("ssb_am frequency: %f?", gen.frequency) if fneq(gen.frequency, 440.0)
  snd_display("ssb_am order: %d?", gen.order) if gen.order != 40
  snd_display("ssb_am cosines: %d?", gen.cosines) if gen.cosines != 1
  snd_display("ssb_am length: %d?", gen.length) if gen.length != 40
  snd_display("ssb_am interp_type: %d?", gen.interp_type) if gen.interp_type != Mus_interp_none
  snd_display("ssb_am xcoeff 0: %f?", gen.xcoeff(0)) if fneq(gen.xcoeff(0), 0.0)
  snd_display("ssb_am xcoeff 1: %f?", gen.xcoeff(1)) if fneq(gen.xcoeff(1), -0.001)
  # 
  test_gen_equal(make_ssb_am(440.0), make_ssb_am(440.0), make_ssb_am(500.0))
  # 
  o1 = make_ssb_am(400.0)
  o2 = make_ssb_am_1(400.0)
  100.times do |i|
    inval = sin(0.1 * i)
    o1o = ssb_am(o1, inval)
    o2o = ssb_am_1(o2, inval)
    if fneq(o1o, o2o)
      snd_display("ssb_am (up): %f %f at %d?", o1o, o2o, i)
      break
    end
  end
  # 
  o1 = make_ssb_am(400.0)
  o2 = make_ssb_am_1(400.0)
  100.times do |i|
    inval = sin(0.1 * i)
    fmval = sin(0.2 * i)
    o1o = ssb_am(o1, inval, fmval)
    o2o = ssb_am_1(o2, inval, fmval)
    if fneq(o1o, o2o)
      snd_display("ssb_am + fm (up): %f %f at %d?", o1o, o2o, i)
      break
    end
  end
  # 
  o1 = make_ssb_am(-100.0)
  o2 = make_ssb_am_1(-100.0)
  100.times do |i|
    inval = rbm_random(1.0)
    o1o = ssb_am(o1, inval)
    o2o = ssb_am_1(o2, inval)
    if fneq(o1o, o2o)
      snd_display("ssb_am (down): %f %f at %d?", o1o, o2o, i)
      break
    end
  end
  # 
  o1 = make_ssb_am(1000.0, 100)
  o2 = make_ssb_am_1(1000.0, 100)
  100.times do |i|
    inval = rbm_random(1.0)
    o1o = ssb_am(o1, inval)
    o2o = ssb_am_1(o2, inval)
    if fneq(o1o, o2o)
      snd_display("ssb_am (down): %f %f at %d?", o1o, o2o, i)
      break
    end
  end
  #
  index = open_sound("pistol.snd")
  data = channel2vct(0, 100)
  convolve_with("oboe.snd", false)
  scl = maxamp
  convolve_with("oboe.snd", scl, index, 0, 0)
  snd_display("convolve_with amps: %f %f?", maxmap, scl) if fneq(maxamp, scl)
  preader = make_sample_reader(0, index, 0, 1, 1)
  reader = make_sample_reader(0)
  frames().times do |i|
    val0 = preader.call
    val1 = reader.call
    if fneq(val0, val1)
      snd_display("convolve_with amps at: %d: %f %f?", i, val0, val1)
      break
    end
  end
  close_sound(index)
  reader = make_sample_reader(0, "pistol.snd")
  10.times do |i|
    if fneq(data[i], next_sample(reader))
      snd_display("external reader trouble")
    end
  end
  free_sample_reader(reader)
end

def test238
  [[:all_pass,       false, 0.0, false],
   [:asymmetric_fm,  false, 0.0, false],
   [:average,        false, 1.0, false],
   [:comb,           false, 0.0, false],
   [:convolve,       [:filter, vct(0, 1, 2)], lambda { |dir| 0.0 }, false],
   [:delay,          false, 0.0, false],
   [:env,            [:envelope, [0, 1, 1, 0]], false, false],
   [:filter,         [:xcoeffs, vct(0, 1, 2)], 0.0, false],
   [:filter,         [:ycoeffs, vct(0, 1, 2)], 0.0, false],
   [:filter,         [:xcoeffs, vct(1, 2, 3), :ycoeffs, vct(0, 1, 2)], 0.0, false],
   [:fir_filter,     [:xcoeffs, vct(0, 1, 2)], 0.0, false],
   [:formant,        false, 0.0, false],
   [:frame,          [3], 0, lambda { |gen, ind| frame_ref(gen, ind) }, false],
   [:granulate,      false, lambda { |dir| 0.0 }, false],
   [:iir_filter,     [:ycoeffs, vct(0, 1, 2)], 0.0, false],
   [:locsig,         false, 0.0, lambda { |gen, a| locsig(gen, 0, a) }],
   [:mixer,          [3, 3], 0, lambda { |gen, a| mixer_ref(gen, a, 0) }],
   [:notch,          false, 0.0, false],
   [:one_pole,       false, 0.0, false],
   [:one_zero,       false, 0.0, false],
   [:oscil,          false, 0.0, false],
   [:pulse_train,    false, 0.0, false],
   [:rand,           false, 0.0, false],
   [:rand_interp,    false, 0.0, false],
   [:sawtooth_wave,  false, 0.0, false],
   [:sine_summation, false, 0.0, false],
   [:square_wave,    false, 0.0, false],
   [:src,            false, lambda { |dir| 0.0 }, lambda { |gen, a| src(gen, 0.0, a) }],
   [:sum_of_cosines, false, 0.0, false],
   [:sum_of_sines,   false, 0.0, false],
   [:table_lookup,   false, 0.0, false],
   [:triangle_wave,  false, 0.0, false],
   [:two_pole,       false, 0.0, false],
   [:two_zero,       false, 0.0, false],
   [:wave_train,     false, 0.0, false],
   [:waveshape,      false, 0.0, false],
   [:polyshape,      false, 0.0, false],
   [:phase_vocoder,  false, lambda { |dir| 0.0 }, false],
   [:ssb_am,         false, 0.0, false]].each do |name_sym, make_args, arg, run_func|
    gen = if make_args
            send(format("make_%s", name_sym), *make_args)
          else
            send(format("make_%s", name_sym))
          end
    snd_display("%s: %s?", name_sym, gen) unless send(format("%s%c", name_sym, ??), gen)
    tag = if proc?(run_func)
            snd_catch do arg ? run_func.call(gen, arg) : run_func.call(gen) end.first
          else
            snd_catch do arg ? send(name_sym, gen, arg) : send(name_sym, gen) end.first
          end
    if (not number?(tag)) and (not frame?(tag))
      snd_display("%s (make_gen, gen, gen? test): %s %s?", name_sym, arg, tag)
    end
    unless RUBY_VERSION < "1.6.8"
      [:mus_channel,
       :mus_channels,
       :mus_cosines,
       :mus_data,
       :mus_feedback,
       :mus_feedforward,
       :mus_formant_radius,
       :mus_frequency,
       :mus_hop,
       :mus_increment,
       :mus_length,
       :mus_location,
       :mus_mix,
       :mus_order,
       :mus_phase,
       :mus_ramp,
       :mus_random,
       :mus_run,
       :mus_scaler,
       :mus_xcoeffs,
       :mus_ycoeffs].each do |func_sym|
        tag = snd_catch do send(func_sym, gen) end.first
        next if :undefined == tag
        if symbol?(tag) and
            tag != :mus_error and
            tag != :out_of_range and
            tag != :wrong_type_arg
          snd_display("%s: %s %s?", name_sym, func_sym, tag.inspect)
        end
        set_tag = snd_catch do send(format("set_", func_sym).intern, gen, tag) end.first
        if symbol?(set_tag) and
            set_tag != :mus_error and
            set_tag != :out_of_range and
            set_tag != :wrong_type_arg
          snd_display("%s: set_%s to %s %s?", name_sym, func_sym, tag, set_tag)
        end
      end
    end
  end
  # 
  functions = [
    [:all_pass,       false, false],
    [:asymmetric_fm,  false, false],
    [:average,        false, false],
    [:comb,           false, false],
    [:convolve,       [:filter, vct(0, 1, 2), :input, lambda { |dir| 1.0 }], false],
    [:delay,          false, false],
    [:env, [:envelope, [0, 1, 1, 0], :end, 10], lambda { |gen, ignored| env(gen) }],
    [:filter,         [:xcoeffs, vct(0, 1, 2)], false],
    [:filter,         [:ycoeffs, vct(0, 1, 2)], false],
    [:filter,         [:xcoeffs, vct(1, 2, 3), :ycoeffs, vct(0, 1, 2)], false],
    [:fir_filter,     [:xcoeffs, vct(0, 1, 2)], 0.0, false],
    [:formant,        [:radius, 0.1, :frequency, 440.0], 0.0, false],
    [:granulate,      [:input, lambda { |dir| 1.0 }], false],
    [:iir_filter,     [:ycoeffs, vct(0, 1, 2)], false],
    [:locsig,         false, lambda { |gen, a| frame_ref(locsig(gen, 0, 1.0), 0) }],
    [:notch,          false, false],
    [:one_pole,       [0.3, 0.7], false],
    [:one_zero,       [0.5, 0.5], false],
    [:oscil,          false, false],
    [:pulse_train,    false, false],
    [:sawtooth_wave,  false, false],
    [:sine_summation, false, false],
    [:square_wave,    false, false],
    [:sum_of_cosines, false, false],
    [:sum_of_sines,   false, false],
    [:table_lookup,   [:wave, make_vct(128, 0.1)], false],
    [:triangle_wave,  false, false],
    [:two_pole,       [0.1, 0.3, 0.6], false],
    [:two_zero,       [0.1, 0.3, 0.5], false],
    [:waveshape,      [:frequency, 440.0, :wave, partials2waveshape([1, 1])], false],
    [:polyshape,      [:frequency, 440.0, :partials, [1, 1]], false],
    [:phase_vocoder,  [lambda { |dir| 1.0 }], false],
    [:ssb_am,         false, false]]
  functions.each do |name_sym, make_args, run_func|
    gen = if make_args
            send(format("make_%s", name_sym), *make_args)
          else
            send(format("make_%s", name_sym))
          end
    data = make_vct!(10) do |i|
      if proc?(run_func)
        run_func.call(gen, i.zero? ? 1.0 : 0.0)
      else
        send(name_sym, gen, i.zero? ? 1.0 : 0.0)
      end
    end
    2.times do |k|
      mus_reset(gen)
      unless proc?(run_func)                      # env, locsig
        not_zero = false
        first_val = k.zero? ? send(name_sym, gen, 1.0) : mus_apply(gen, 1.0, 0.0)
        if data[0] != 0.0
          not_zero = true
        end
        if fneq(data[0], first_val)
          snd_display("[%s] %s: 0 %f %f?", k.zero? ? :run : :apply, name_sym, data[0], first_val)
        end
        (1...10).each do |i|
          old_val = data[i]
          new_val = k.zero? ? send(name_sym, gen, 0.0) : mus_apply(gen, 0.0, 0.0)
          if old_val != 0.0
            not_zero = true
          end
          if fneq(old_val, new_val)
            snd_display("[%s] %s: %d %f %f?",
                        k.zero? ? :run : :apply, name_sym, i, old_val, new_val)
          end
        end
        if (not not_zero) and name_sym != :polyshape
          snd_display("%s not much of a reset test!", name_sym)
        end
      end
    end
  end
  random_args = [
    1.5,
    "/hiho",
    [0, 1],
    1234,
    make_vct(3),
    make_color_with_catch(0.95, 0.95, 0.95),
    [0, 1],
    3 / 4.0,
    :mus_error,
    sqrt(-1.0),
    make_delay(32),
    lambda { || true },
    make_sound_data(2, 3),
    :order,
    0,
    1,
    -1,
    make_hook("snd_test"),
    false,
    true,
    ?c,
    0.0,
    1.0,
    -1.0, 
    [],
    3,
    4,
    2,
    8,
    16,
    32,
    64,
    make_array(0),
    [1, 2],
    2.0 ** 21.5,
    2.0 ** -18.0,
    12345678901234567890,
    log0]
  functions.each do |name_sym, make_args, run_func|
    gen = if make_args
            send(format("make_%s", name_sym), *make_args)
          else
            send(format("make_%s", name_sym))
          end
    random_args.each do |arg1|
      snd_catch do 
        if proc?(run_func)
          run_func.call(gen, arg1)
        else
          send(name_sym, gen, arg1)
        end
      end
      random_args.each do |arg2|
        snd_catch do mus_run(gen, arg1, arg2) end
      end
    end
  end
end

def test248
  random_args = [
    2.0 ** 21.5,
    2.0 ** -18.0,
    12345678901234567890,
    1.5,
    "/hiho",
    [0, 1],
    1234,
    make_vct(3),
    make_color_with_catch(0.1, 0.2, 0.3),
    [0, 1],
    3 / 4.0,
    sqrt(-1.0),
    make_delay(32),
    lambda { || 0.0 },
    lambda { |dir| 1.0 },
    lambda { |a, b, c| 1.0 },
    0,
    1,
    -1,
    false,
    true,
    ?c,
    0.0,
    1.0,
    -1.0,
    [],
    32,
    [1, 2]]
  random_gen = lambda do |*args|
    [:make_all_pass,
     :make_asymmetric_fm,
     :make_average,
     :make_table_lookup,
     :make_triangle_wave,
     :make_comb,
     :make_convolve,
     :make_delay,
     :make_env,
     :make_fft_window,
     :make_filter,
     :make_fir_filter,
     :make_formant,
     :make_frame,
     :make_granulate,
     :make_iir_filter,
     :make_locsig,
     :make_mixer,
     :make_notch,
     :make_one_pole,
     :make_one_zero,
     :make_oscil,
     :make_ppolar,
     :make_pulse_train,
     :make_rand,
     :make_rand_interp,
     :make_sawtooth_wave,
     :make_sine_summation,
     :make_square_wave,
     :make_src,
     :make_sum_of_cosines,
     :make_sum_of_sines, 
     :make_two_pole,
     :make_two_zero,
     :make_wave_train,
     :make_waveshape,
     :make_polyshape,
     :make_zpolar,
     :make_phase_vocoder,
     :make_ssb_am].each do |make_func|
      if mus_generator?(gen = snd_catch do send(make_func, *args) end.first)
        random_args.each do |arg|
          snd_catch do gen.run(arg) end
        end
      end
    end
  end
  random_gen.call
  random_args.each do |arg1|
    random_gen.call(arg1)
    random_args.each do |arg2|
      random_gen.call(arg1, arg2)
      random_args.each do |arg3|
        random_gen.call(arg1, arg2, arg3)
        random_args.each do |arg4|
          random_gen.call(arg1, arg2, arg3, arg4)
        end
      end
    end
  end
end

def test08
  test008
  test018
  test028
  test038
  test048
  test058
  test068
  test078
  test088
  test098
  test108
  test118
  test128
  test138
  test148
  test158
  test168
  test178
  test188
  test198
  test208
  test218
  test228
  test238
  test248 if $all_args
end

if $test08 and $full_test or $snd_test == 8
  $before_test_hook.call(8)
  test08
  $after_test_hook.call(8)
end

# ---------------- test 09: mix ----------------

module Test_event
  # see event.scm
  def drag_event(widget, button, state, x0, y0, x1, y1)
    e = RXEvent(RButtonPress)
    e1 = RXEvent(RMotionNotify)
    dpy = RXtDisplay(widget)
    window = RXtWindow(widget)
    Rset_type(e, RButtonPress)
    Rset_window(e, window)
    Rset_display(e, dpy)
    Rset_root(e, RRootWindow(dpy, RDefaultScreen(dpy)))
    Rset_x(e, x0)
    Rset_y(e, y0)
    Rset_x_root(e, 0)
    Rset_y_root(e, 0)
    Rset_state(e, state)
    Rset_button(e, button)
    Rset_time(e, [:Time, RCurrentTime])
    Rset_same_screen(e, true)
    Rset_subwindow(e, [:Window, RNone])
    err = RXSendEvent(dpy, window, false, RButtonPressMask, e)
    if err.nonzero?
      Rset_window(e1, window)
      Rset_display(e1, dpy)
      Rset_root(e1, RRootWindow(dpy, RDefaultScreen(dpy)))
      Rset_x_root(e1, x0)
      Rset_y_root(e1, y0)
      Rset_state(e1, state)
      Rset_time(e1, [:Time, RCurrentTime + 300])
      Rset_same_screen(e1, true)
      Rset_subwindow(e1, [:Window, RNone])
      Rset_is_hint(e1, RNotifyNormal)
      den = if (x1 - x0).abs > 10 or (y1 - y0).abs > 10
              10
            else
              2
            end
      xdiff = ((x1 - x0) / den.to_f).floor
      ydiff = ((y1 - y0) / den.to_f).floor
      xn = x0 + xdiff
      yn = y0 + ydiff
      den.times do
        Rset_x(e1, xn)
        Rset_y(e1, yn)
        RXSendEvent(dpy, window, false, RButtonMotionMask, e1)
        xn += xdiff
        yn += ydiff
      end
      Rset_type(e, RButtonRelease)
      Rset_time(e, [:Time, RCurrentTime + 500])
      Rset_x(e, x1)
      Rset_y(e, y1)
      RXSendEvent(dpy, window, false, RButtonMotionMask, e)
    end
  end

  def force_event
    app = main_widgets[0]
    until ((msk = RXtAppPending(app)) & (RXtIMXEvent | RXtIMAlternateInput)).zero?
      RXtDispatchEvent(RXtAppNextEvent(app))
    end
  end

  def click_button(button, value = false, bits = false)
    if RWidget?(button)
      if RXtIsSensitive(button)
        if RXmIsPushButton(button) or RXmIsPushButtonGadget(button)
          if RXtHasCallbacks(button, RXmNactivateCallback) == RXtCallbackHasSome
            RXtCallCallbacks(button, RXmNactivateCallback,
                             let(RXmPushButtonCallbackStruct()) do |but|
                               Rset_click_count(but, 0)
                               Rset_event(but, let(RXEvent(RButtonPress)) do |e|
                                            Rset_state(e, (bits or 0))
                                            e
                                          end)
                               but
                             end)
          else
            snd_display("pushbutton %s has no active callbacks", RXtName(button))
          end
        else
          if RXmIsToggleButton(button) or RXmIsToggleButtonGadget(button)
            if RXtHasCallbacks(button, RXmNvalueChangedCallback) == RXtCallbackHasSome
              RXtCallCallbacks(button, RXmNvalueChangedCallback,
                               let(RXmToggleButtonCallbackStruct()) do |tgl|
                                 Rset_value(tgl, value)
                                 Rset_event(tgl, let(RXEvent(RButtonPress)) do |e|
                                              Rset_state(e, (bits or 0))
                                              e
                                            end)
                                 tgl
                               end)
            else
              snd_display("togglebutton %s has no valueChanged callbacks", RXtName(button))
            end
          else
            if RXmIsArrowButton(button)
              if RXtHasCallbacks(button, RXmNactivateCallback) == RXtCallbackHasSome
                RXtCallCallbacks(button, RXmNactivateCallback,
                                 let(RXmArrowButtonCallbackStruct()) do |arr|
                                   Rset_click_count(arr, 0)
                                   Rset_event(arr, let(RXEvent(RButtonPress)) do |e|
                                                Rset_state(e, (bits or 0))
                                                e
                                              end)
                                   arr
                                 end)
              else
                snd_display("arrowbutton %s has no active callbacks", RXtName(button))
              end
            else
              snd_display("%s (%s) is not a push or toggle button",
                          RXtName(button), RXtName(RXtParent(button)))
            end
          end
        end
      else
        snd_display("%s is not sensitive", RXtName(button))
      end
    else
      snd_display("%s is not a widget", button)
    end
  end
end if provided? :xm

if provided? :xm
  include Test_event
end

def track_end(id)
  track_position(id) + track_frames(id) - 1
end

define_envelope("$xrmx",  [0, 0, 1, 1], 32.00)
define_envelope("$xrmx1", [0, 0, 1, 1],  0.03)
define_envelope("$xrmx2", [0, 0, 1, 1],  0.50)

def test009
  new_index = new_sound("hiho.wave", Mus_next, Mus_bshort, 22050, 1)
  select_sound(new_index)
  if res = find_mix(0, new_index, 0)
    snd_display("found non-existent mix: %s?", res)
  end
  unless mix?(mix_id = mix("pistol.snd", 100))
    snd_display("%s not mix?", mix_id)
  end
  view_files_dialog
  pos = mix_position(mix_id)
  len = mix_frames(mix_id)
  loc = mix_locked?(mix_id)
  inv = mix_inverted?(mix_id)
  anc = mix_tag_position(mix_id)
  spd = mix_speed(mix_id)
  trk = mix_track(mix_id)
  snd, chn = mix_home(mix_id)[0, 2]
  chns = mix_chans(mix_id)
  amp = mix_amp(mix_id, 0)
  mr = make_mix_sample_reader(mix_id)
  snd_display("%s not mix_sample_reader?", mr) unless mix_sample_reader?(mr)
  snd_display("mix_sample_reader: track %s?", mr) if track_sample_reader?(mr)
  snd_display("mix_sample_reader: region %s?", mr) if region_sample_reader?(mr)
  snd_display("mix_sample_reader: normal %s?", mr) if sample_reader?(mr)
  if (res = sample_reader_position(mr)).nonzero?
    snd_display("mix_sample_reader_position: %d?", res)
  end
  snd_display("mix_sample_reader at end? %s", mr) if sample_reader_at_end?(mr)
  if (res = sample_reader_home(mr)) != mix_id
    snd_display("%s home: %s?", mr, res)
  end
  if mr.to_s[0, 22] != "#<mix-sample-reader mi"
    snd_display("mix_sample_reader actually got: [%s]?", mr.to_s[0, 22])
  end
  if (res = snd_catch do mix_amp(mix_id, 1234) end).first != :no_such_channel
    snd_display("mix_amp bad chan: %s", res.inspect)
  end
  if (res = snd_catch do set_mix_amp(mix_id, 1234, 0.1) end).first != :no_such_channel
    snd_display("set_mix_amp bad chan: %s", res.inspect)
  end
  if (res = snd_catch do set_mix_amp_env(mix_id, 1234, [0, 0, 1, 1]) end).first != :no_such_channel
    snd_display("set_mix_amp_env bad chan: %s", res.inspect)
  end
  99.times do |i|
    mx = i.odd? ? read_mix_sample(mr) : read_mix_sample(mr)
    sx = sample(100 + i)
    snd_display("read_mix_sample: %f %f?", mx, sx) if fneq(mx, sx)
  end
  if fneq(mx = mr.call, sx = sample(199))
    snd_display("mix_sample 100: %f %f?", mx, sx)
  end
  free_sample_reader(mr)
  #
  snd_display("mix_position: %d?", pos) if pos != 100
  snd_display("mix_frames: %d?", len) if len != 41623
  snd_display("mix_locked?: %s?", loc) if loc
  snd_display("mix_inverted?: %s?", inv) if inv
  snd_display("mix_tag_position: %d?", anc) if anc.nonzero?
  snd_display("mix_track: %d?", trk) if trk.nonzero?
  snd_display("snd mix_home: %d?", snd) if snd != new_index
  snd_display("chn mix_home: %d?", chn) if chn.nonzero?
  snd_display("mix_chans: %d?", chns) if chns != 1
  snd_display("mix_amp: %f?", amp) if fneq(amp, 1.0)
  snd_display("mix_speed: %f?", spd) if fneq(spd, 1.0)
  if (res = play_mix(mix_id)) != 0
    snd_display("can\'t play mix: %s", res)
  end
  if (res = snd_catch do set_mix_track(mix_id, -1) end).first != :out_of_range
    snd_display("set_mix_track -1: %s (%s)", res.inspect, mix_track(mix_id))
  end
  set_mix_position(mix_id, 200)
  set_mix_amp(mix_id, 0, 0.5)
  set_mix_speed(mix_id, 2.0)
  trk = make_track(mix_id)
  if (res = snd_catch do play_track(1231233) end).first != :no_such_track
    snd_display("play_track bad track: %s", res.inspect)
  end
  if (res = snd_catch do play_track(1231233, true) end).first != :no_such_track
    snd_display("play_track bad track true: %s", res.inspect)
  end
  if (res = snd_catch do play_track(1231233, 0) end).first != :no_such_track
    snd_display("play_track bad track index: %s", res.inspect)
  end
  play_track(trk)
  set_mix_tag_position(mix_id, 30)
  set_mix_amp_env(mix_id, 0, [0, 0, 1, 1])
  val = mix_amp_env(mix_id, 0)
  set_mix_amp_env(mix_id, 0, mix_amp_env(mix_id, 0))
  unless vequal(res = mix_amp_env(mix_id, 0), val)
    snd_display("set_mix_amp_env to self: %s %s?", val, res)
  end
  set_mix_tag_y(mix_id, 20)
  pos = mix_position(mix_id)
  spd = mix_speed(mix_id)
  trk = mix_track(mix_id)
  amp = mix_amp(mix_id, 0)
  my = mix_tag_y(mix_id)
  anc = mix_tag_position(mix_id)
  snd_display("set_mix_position: %d?", pos) if pos != 200
  snd_display("set_mix_speed: %f?", spd) if fneq(spd, 2.0)
  snd_display("set_mix_track: %d?", trk) unless track?(trk)
  snd_display("set_mix_mix_tag_y: %d?", my) if my != 20
  snd_display("set_mix_amp: %f?", amp) if fneq(amp, 0.5)
  snd_display("set_mix_tag_position: %d?", anc) if anc != 30
  if (res = mix_amp_env(mix_id, 0)) != [0, 0, 1, 1]
    snd_display("set_mix_amp_env: %s?", res)
  end
  #
  mix_vct(make_vct(3, 0.1), 100)
  set_cursor(0)
  if (not mix?(nid = forward_mix)) or cursor != mix_position(nid)
    snd_display("forward_mix %s %s %s?", nid, cursor, (mix?(nid) and mix_position(nid)))
  end
  if (not mix?(nid1 = forward_mix(2))) or cursor != mix_position(nid1)
    snd_display("forward_mix(2) %s %s %s %s %s?",
                nid, nid1, cursor, (mix?(nid1) and mix_position(nid1)), mixes)
  end
  if (not mix?(nid1 = backward_mix)) or cursor != mix_position(nid1)
    snd_display("backward_mix %s %s %s?", nid1, cursor, (mix?(nid1) and mix_position(nid1)))
  end
  if (not mix?(nid = find_mix(100))) or mix_position(nid) != 100
    snd_display("find_mix(100) %s %s %s?",
                nid, (mix?(nid) and mix_position(nid)),
                mixes(new_index, 0).map do |m| mix_position(m) end)
  end
  if (not mix?(nid = find_mix(200))) or mix_position(nid) != 200
    snd_display("find_mix(200) %s %s?", nid, (mix?(nid) and mix_position(nid)))
  end
  #
  mix_id = mix("oboe.snd", 100)
  set_mix_waveform_height(40)
  set_mix_property(:hiho, 123, mix_id)
  if (res = mix_property(:hiho, mix_id)) != 123
    snd_display("mix_property: %s?", res)
  end
  if (res = mix_property(:not_there, mix_id))
    snd_display("mix_not_property: %s?", res)
  end
  update_time_graph
  set_mix_waveform_height(20)
  revert_sound(new_index)
  # 
  # now track tests (mix.rb)
  # 
  trk = make_track
  mix_ids = make_array(6) do |i| mix("oboe.snd", i * 1000) end
  set_mix_track(mix_ids[0], trk)
  set_mix_track(mix_ids[2], trk)
  set_mix_track(mix_ids[4], trk)
  if (res = track_position(trk)).nonzero?
    snd_display("track_position: %d?", res)
  end
  mr = make_track_sample_reader(trk)
  if mr.to_s[0, 24] != "#<track-sample-reader tr"
    snd_display("track sample_reader actually got: [%s]", mr.to_s[0, 24])
  end
  if (res = sample_reader_position(mr)) != 0
    snd_display("track sample_reader position: %d?", res)
  end
  snd_display("track sample_reader at end? %s", mr) if sample_reader_at_end?(mr)
  if (res = sample_reader_home(mr)) != [trk, 0]
    snd_display("track %s home: %s?", mr, res)
  end
  snd_display("track sample_reader: mix %s?", mr) if mix_sample_reader?(mr)
  snd_display("track sample_reader: region %s?", mr) if region_sample_reader?(mr)
  snd_display("track sample_reader: normal %s?", mr) if sample_reader?(mr)
  free_sample_reader(mr)
  #
  curend = track_end(trk)
  curframes = track_frames(trk)
  curmixpos = track(trk).map do |m| mix_position(m) end
  curmixframes = track(trk).map do |m| mix_frames(m) end
  set_track_position(trk, 500)
  if (res = track_position(trk)) != 500
    snd_display("set_track_position: %d?", res)
  end
  if (res = mix_position(mix_ids[0])) != 500
    snd_display("track_position 0 =  %d?", res)
  end
  if (res = mix_position(mix_ids[1])) != 1000
    snd_display("track_position 1 =  %d?", res)
  end
  if (res = mix_position(mix_ids[4])) != 4500
    snd_display("track_position 4 =  %d?", res)
  end
  unless (track_end(trk) - (curend + 500)).abs < 2
    snd_display("track_end: %d (cur+500: %d, %d, %d + %d -> %d\n# %s\n# %s\n# %s\n# %s\n# %s)?",
                track_end(trk), curend + 500, curframes,
                track_position(trk), track_frames(trk),
                track_position(trk) + track_frames(trk) -1,
                track(trk),
                track(trk).map { |m| mix_frames(m) },
                track(trk).map { |m| mix_position(m) },
                curmixpos, curmixframes)
  end
  set_track_amp(trk, 0.5)
  snd_display("set_track_amp: %f?", track_amp(trk)) if fneq(track_amp(trk), 0.5)
  set_track_amp(trk, track_amp(trk) + 0.25)
  snd_display("incf track_amp: %f?", track_amp(trk)) if fneq(track_amp(trk), 0.75)
  transpose_track(trk, 12)
  snd_display("transpose_track: %f?", track_speed(trk)) if fneq(track_speed(trk), 2.0)
  retempo_track(trk, 2.0)
  if track_frames(trk) != ((4000 + 50828) / 2)
    snd_display("track_tempo: %d (%d, %s %s)?",
                track_frames(trk), (4000 + 50828) / 2,
                track(trk),
                track(trk).map { |m| mix_frames(m) })
  end
  set_track_color(trk, make_color_with_catch(0.8, 0.8, 0.8))
  trk2 = make_track
  set_mix_track(mix_ids[1], trk2)
  set_mix_track(mix_ids[3], trk2)
  set_track_color(trk2, make_color_with_catch(0.2, 0.8, 0.0))
  t2 = track2vct(trk2)
  t3 = mix2vct(mix_ids[5])
  if fneq(t2[1000], t3[1000]) or fneq(t3[1000], 0.0328369)
    snd_display("track2vct: %f, mix2vct: %f (0.0328369)?", t2[1000], t3[1000])
  end
  set_track_amp_env(trk, [0, 0, 1, 1])
  play_and_wait
  #
  v1 = envelope_interp(1.0, [0, 0, 2.0, 1.0])
  v2 = envelope_interp(1.0, [0, 0.0, 1, 1.0, 2, 0.0])
  v3 = envelope_interp(2.0, [0, 0.0, 1, 1.0])
  v4 = envelope_interp(0.0, [1, 0.5, 2, 0])
  snd_display("envelope_interp(1): %f (0.5)?", v1) if fneq(v1, 0.5)
  snd_display("envelope_interp(2): %f (1.0)?", v2) if fneq(v2, 1.0)
  snd_display("envelope_interp(3): %f (1.0)?", v3) if fneq(v3, 1.0)
  snd_display("envelope_interp(4): %f (0.5)?", v4) if fneq(v4, 0.5)
  v1 = envelope_interp( 0.0, [-1,  0, 0, 1, 1, -1])
  v2 = envelope_interp(-0.5, [-1,  0, 0, 1, 1, -1])
  v3 = envelope_interp(-0.5, [-1, -1, 0, 1, 1, -1])
  v4 = envelope_interp(-0.5, [-1, -1, 1, 1])
  v5 = envelope_interp(-1.5, [-1, -1, 1, 1])
  v6 = envelope_interp( 1.5, [-1, -1, 1, 1])
  snd_display("envelope_interp(1a): %f ( 1.0)?", v1) if fneq(v1,  1.0)
  snd_display("envelope_interp(2a): %f ( 0.5)?", v2) if fneq(v2,  0.5)
  snd_display("envelope_interp(3a): %f ( 0.0)?", v3) if fneq(v3,  0.0)
  snd_display("envelope_interp(4a): %f (-0.5)?", v4) if fneq(v4, -0.5)
  snd_display("envelope_interp(5a): %f (-1.0)?", v5) if fneq(v5, -1.0)
  snd_display("envelope_interp(6a): %f ( 1.0)?", v6) if fneq(v6,  1.0)
  v1 = multiply_envelopes([0.0, 0.0, 2.0, 0.5], [0.0, 0.0, 1.0, 2.0, 2.0, 1.0])
  v2 = window_envelope(1.0, 3.0, [0.0, 0.0, 5.0, 1.0])
  snd_display("multiply_envelopes: %s?", v1) unless vequal(v1, [0, 0, 0.5, 0.5, 1, 0.5])
  snd_display("window_envelope: %s?", v2) unless vequal(v2, [1, 0.2, 3, 0.6])
  close_sound(new_index)
  dismiss_all_dialogs
end

def test019
  ind = new_sound("new.snd")
  trk33 = make_track
  mxs = make_array(10) do |i|
    v = make_vct(1, i * 0.05)
    if mix?(m = mix_vct(v, i, ind, 0))
      set_mix_track(m, trk33)
    else
      snd_display("mix_vct at %d failed?", i)
      break
    end
    m
  end
  tr = make_track_sample_reader(trk33)
  tr1 = make_track_sample_reader(trk33, true, 5)
  10.times do |i|
    if fneq(val = (i.odd? ? read_track_sample(tr) : read_track_sample(tr)), i * 0.05)
      snd_display("read track at %d: %f?", i, val)
      break
    end
  end
  if fneq(val = read_track_sample(tr1), 0.05 * 5)
    snd_display("track_sample_reader withbeg: %f %f %s?", val, 0.05 * 5, tr1)
  end
  free_sample_reader(tr)
  free_sample_reader(tr1)
  save_sound(ind)
  snd_display("saved mixes not re-activated?") unless mix?(mxs[0])
  close_sound(ind)
  #
  ind = open_sound("oboe.snd")
  open_readers = make_array(100)
  mix1 = mix_vct(vct(0.1, 0.2, 0.3), 120, ind, 0, true, "origin!")
  mix2 = mix_vct(vct(0.1, 0.2, 0.3), 1200, ind, 0, true)
  mix3 = mix_vct(vct(0.1, 0.2, 0.3), 12000, ind, 0, true)
  trk123 = make_track
  reg1 = make_region(200, 300, ind, 0)
  set_mix_track(mix1, trk123)
  set_mix_track(mix2, trk123)
  set_mix_track(mix3, trk123)
  $sample_reader_tests.times do |i|
    r = irandom(100)
    case irandom(4)
    when 0
      unless sample_reader?(open_readers[r] = make_sample_reader(random(30000), ind, 0))
        snd_display("sample_reader? %s?", open_readers[r])
      end
      next_sample(open_readers[r])
      if (res = sample_reader_home(open_readers[r])) != [ind, 0]
        snd_display("sample_reader_home %s?", res)
      end
    when 1
      unless region_sample_reader?(open_readers[r] = make_region_sample_reader(random(90), reg1))
        snd_display("region_sample_reader? %s?", open_readers[r])
      end
      next_sample(open_readers[r])
    when 2
      unless mix_sample_reader?(open_readers[r] = make_mix_sample_reader(mix1))
        snd_display("mix_sample_reader? %s?", open_readers[r])
      end
      if fneq(res = read_mix_sample(open_readers[r]), 0.1)
        snd_display("read_mix_sample: %s?", res)
      end
    else
      unless track_sample_reader?(open_readers[r] = make_track_sample_reader(trk123))
        snd_display("track_sample_reader? %s?", open_readers[r])
      end
      if fneq(res = read_track_sample(open_readers[r]), 0.1)
        snd_display("read_track_sample: %s?", res)
      end
    end
    if random(1.0) > 0.25
      rr = random(100)
      if open_readers[rr]
        if sample_reader?(open_readers[rr])
          free_sample_reader(open_readers[rr])
        elsif mix_sample_reader?(open_readers[rr])
          free_sample_reader(open_readers[rr])
        elsif track_sample_reader?(open_readers[rr])
          free_sample_reader(open_readers[rr])
        end
      end
      open_readers[rr] = false
    end
  end
  open_readers.clear
  close_sound(ind)
  #
  id = open_sound("oboe.snd")
  make_selection(1000, 2000, id, 0)
  mix_id = mix_selection(3000, id, 0)
  set_mix_amp(mix_id, 0, 0.5)
  snd_display("mix_amp 0.5: %f?", mix_amp(mix_id, 0)) if fneq(mix_amp(mix_id, 0), 0.5)
  scale_by(0.5)
  snd_display("mix not locked? %s?", mix_id) unless mix_locked?(mix_id)
  if (res = snd_catch do set_mix_amp(mix_id, 0, 1.0) end).first != :no_such_mix
    snd_display("set locked mix amp: %s", res.inspect)
  end
  if (res = snd_catch do set_mix_position(mix_id, 10) end).first != :no_such_mix
    snd_display("set locked mix position: %s", res.inspect)
  end
  if (res = snd_catch do set_mix_speed(mix_id, 1.5) end).first != :no_such_mix
    snd_display("set locked mix speed: %s", res.inspect)
  end
  if (res = snd_catch do set_mix_amp_env(mix_id,0 , [0, 0, 1, 1]) end).first != :no_such_mix
    snd_display("set locked mix amp env: %s", res.inspect)
  end
  undo_edit
  if mix_locked?(mix_id)
    snd_display("undo locked mix: %s?", mix_id)
  else
    set_mix_position(mix_id, 10)
    if (res = mix_position(mix_id)) != 10
      snd_display("mix_position 10: %d?", res)
    end
  end
  close_sound(id)
  #
  set_print_length(30)
  index = new_sound("test.snd")
  v1 = make_vct(1, 0.1)
  v2 = make_vct(2, 0.2)
  v3 = make_vct(3, 0.3)
  id1 = [0, 10, 20].map do |start| mix_vct(v1, start) end
  id2 = [1, 12, 23].map do |start| mix_vct(v2, start) end
  id3 = [2, 14, 26].map do |start| mix_vct(v3, start) end
  trk1 = make_track
  unless vequal(res = channel2vct,
                vct(0.1, 0.2, 0.5, 0.3, 0.3, 0, 0, 0, 0, 0, 0.1, 0, 0.2, 0.2, 0.3,
                    0.3, 0.3, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2,  0, 0.3, 0.3, 0.3))
    snd_display("mix tests off to a bad start: %s?", res)
  end
  unless vequal(res = mix2vct(id2[0]), vct(0.2, 0.2))
    snd_display("mix2vct of 0.2: %s?", res)
  end
  set_mix_track(id1[0], trk1)
  tr1 = trk1
  if (res = track(tr1)) != [id1[0]]
    snd_display("1 track2%s %s?", res, [id1[0]])
  end
  if (res = track_position(tr1)) != mix_position(id1[0])
    snd_display("1 track_position %s %s (%s)?", tr1, res, mix_position(id1[0]))
  end
  if (res1 = track_frames(tr1)) != (res2 = mix_frames(id1[0]))
    snd_display("1 track_frames %s frames: %s (mix frames: %s)?", track(tr1), res1, res2)
  end
  if (res1 = track_end(tr1)) != (res2 = mix_position(id1[0])) + (res3 = mix_frames(id1[0])) - 1
    snd_display("1 track_end %s %s %s?", res1, res2, res3)
  end
  if fneq(res1 = track_amp(tr1), res2 = mix_amp(id1[0], 0))
    snd_display("1 track_amp %f %f?", res1, res2)
  end
  if fneq(res1 = track_speed(tr1), res2 = mix_speed(id1[0]))
    snd_display("1 track_speed %f %f?", res1, res2)
  end
  unless vequal(res1 = track2vct(tr1), res2 = mix2vct(id1[0]))
    snd_display("1 track2vct %s %s?", res1, res2)
  end
  set_track_amp(tr1, 0.0)
  unless vequal(res = channel2vct,
                vct(0, 0.2, 0.5, 0.3, 0.3, 0, 0, 0, 0, 0, 0.1, 0, 0.2, 0.2, 0.3,
                    0.3, 0.3, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.3, 0.3))
    snd_display("first mix deleted: %s?", res)
  end
  undo_edit
  if fneq(res = mix_amp(id1[0], 0), 1.0)
    snd_display("1 undo delete_track amp: %s?", res)
  end
  set_track_amp(tr1, 2.0)
  unless vequal(res = channel2vct,
                vct(0.2, 0.2, 0.5, 0.3, 0.3, 0, 0, 0, 0, 0, 0.1, 0, 0.2, 0.2, 0.3,
                    0.3, 0.3, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.3, 0.3))
    snd_display("1 set_track_amp: %s?", res)
  end
  set_track_position(tr1, 8)
  if (res = track_position(tr1)) != 8
    snd_display("moved track 1: %s?", track_position(tr1))
  end
  unless vequal(res = channel2vct,
                vct(0, 0.2, 0.5, 0.3, 0.3, 0, 0, 0, 0.2, 0, 0.1, 0, 0.2, 0.2, 0.3,
                    0.3, 0.3, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.3, 0.3))
    snd_display("1 set_track_position 8: %s?", res)
  end
  reverse_track(tr1)
  unless vequal(res = channel2vct,
                vct(0, 0.2, 0.5, 0.3, 0.3, 0, 0, 0, 0.2, 0, 0.1, 0, 0.2, 0.2, 0.3,
                    0.3, 0.3, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.3, 0.3))
    snd_display("1 reverse track: %s?", res)
  end
  # 
  trk2 = make_track(id1[1], id2[1], id3[1])
  tr2 = trk2
  if (res = track_position(tr2)) != mix_position(id1[1])
    snd_display("2 track_position %s %s (%s)?", tr2, res, mix_position(id1[1]))
  end
  set_track_amp(tr2, 2.0)
  unless vequal(res = channel2vct,
                vct(0, 0.2, 0.5, 0.3, 0.3, 0, 0, 0, 0.2, 0, 0.2, 0, 0.4, 0.4, 0.6,
                    0.6, 0.6, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.3, 0.3))
    snd_display("2 set_track_amp: %s?", res)
  end
  set_track_position(tr2, track_position(tr2) - 1)
  revert_sound(index)
  #
  id1 = [0, 10, 20].map do |start| mix_vct(v1, start) end
  id2 = [1, 12, 23].map do |start| mix_vct(v2, start) end
  id3 = [2, 14, 26].map do |start| mix_vct(v3, start) end
  trk1 = make_track
  unless vequal(res = channel2vct,
                vct(0.1, 0.2, 0.5, 0.3, 0.3, 0, 0, 0, 0, 0, 0.1, 0, 0.2, 0.2, 0.3,
                    0.3, 0.3, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.3, 0.3))
    snd_display("mix tests 2nd start: %s?", res)
  end
  tr1 = make_track(*id1)
  tr2 = make_track(*id3)
  old_pos = track(tr1).map do |m| mix_position(m) end
  if old_pos != (res = id1.map do |m| mix_position(m) end)
    snd_display("old_pos: %s %s?", old_pos, res)
  end
  retempo_track(tr1, 2)
  unless vequal(res = channel2vct,
                vct(0.1, 0.2, 0.5, 0.3, 0.3, 0.1, 0, 0, 0, 0, 0.1, 0, 0.2, 0.2, 0.3,
                    0.3, 0.3, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0, 0.3, 0.3, 0.3))
    snd_display("3 track-tempo 0.5: %s -> %s, %s?",
                old_pos, track(tr1).map do |m| mix_position(m) end, res)
  end
  set_track_amp(tr1, 0.0)
  unless vequal(res = channel2vct,
                vct(0, 0.2, 0.5, 0.3, 0.3, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0.3,
                    0.3, 0.3, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0, 0.3, 0.3, 0.3))
    snd_display("3 track_amp 0: %s?", res)
  end
  delete_all_mixes
  close_sound(index)
  #
  ind = open_sound("2.snd")
  md = mix("1a.snd", 1000, 0, ind, 1, true)
  snd_display("maxamp after mix into chan 2: %f?", maxamp(ind, 1)) if fneq(maxamp(ind, 1), 0.1665)
  set_mix_amp(md, 0, 0.0)
  if (res1 = edits(ind, 0)) != [0, 0] or (res2 = edits(ind, 1) != [2, 0])
    snd_display("mix into chan2 zeroed: %f %f?", res1, res2)
  end
  if fneq(res = maxamp(ind, 1), 0.066)
    snd_display("maxamp afer mix zeroed into chan 2: %f?", res)
  end
  set_mix_amp(md, 0, 0.5)
  if fneq(res = maxamp(ind, 1), 0.116)
    snd_display("maxamp afer mix 0.5 into chan 2: %f?", res)
  end
  set_mix_speed(md, 2.0)
  if fneq((res1 = mix_frames(md)) / (res2 = mus_sound_frames("1a.snd")).to_f, 0.5)
    snd_display("mix srate chan 2: %s %s?", res1, res2)
  end
  update_time_graph
  set_mix_speed(md, 0.5)
  update_time_graph
  set_mix_amp(md, 0, 1.0)
  if fneq(res = maxamp(ind, 1), 0.116)
    snd_display("non-sync mix_speed: %f?", res)
  end
  set_mix_amp_env(md, 0, [0, 0, 1, 1, 2, 0])
  update_time_graph
  set_mix_speed(md, 1.0)
  update_time_graph
  revert_sound(ind)
  set_sync(1, ind)
  m0 = maxamp(ind, 0)
  m1 = maxamp(ind, 1)
  len = frames(ind, 0)
  md = mix("2.snd")
  if frames(ind, 0) != len or fneq(maxamp(ind, 0), 2 * m0) or  fneq(maxamp(ind, 1), 2 * m1)
    snd_display("mix twice synced: m0: %f -> %f, m1: %f -> %f, len: %d -> %d?",
                m0, maxamp(ind, 0), m1, maxamp(ind, 1), len, frames(ind, 0))
  end
  if provided?(:snd_motif) and provided?(:xm)
    wid = view_mixes_dialog
    if wid != (res = dialog_widgets[16])
      snd_display("view_mixes_dialog -> %s %s?", wid, res)
    end
    mixd = dialog_widgets[16]
    spdscr = find_child(mixd, "mix-speed")
    dragged = false
    $mix_drag_hook.add_hook!("snd-test") do |n| dragged = n end
    RXtCallCallbacks(spdscr, RXmNvalueChangedCallback,
                     let(RXmScrollBarCallbackStruct()) do |cb|
                       Rset_value(cb, 650)
                       Rset_event(cb, RXEvent())
                       cb
                     end)
    xy = mix_tag_xy(md)
    cwid = channel_widgets(ind, 0)[0]
    x = xy[0] + 1
    y = xy[1] - 2
    pos = mix_position(md)
    focus_widget(cwid)
    drag_event(cwid, 1, 0, x, y, x + 50, y)
    force_event
    RXtUnmanageChild(mixd)
    $mix_drag_hook.reset_hook!
  end
  $mix_release_hook.reset_hook!
  close_sound(ind)
end

def test029
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 1, "mix tests")
  insert_silence(0, 20, ind)
  indout = new_sound("test.snd", Mus_next, Mus_bshort, 22050, 1, "mix tests")
  insert_silence(0, 10, indout)
  set_sample(2, 0.5, indout, 0)
  set_sample(5, 0.25, indout, 0)
  save_sound(indout)
  close_sound(indout)
  tag = mix("test.snd")
  samps = channel2vct(0, 20)
  v = make_vct(20)
  v[2] = 0.5
  v[5] = 0.25
  snd_display("mix 1->1: %s %s?", samps, v) unless  vequal(samps, v)
  snd_display("mix 1->1 tag: %s?", tag) unless mix?(tag)
  undo_edit
  tag = mix("test.snd", 5)
  samps = channel2vct(0, 20)
  v = make_vct(20)
  v[7] = 0.5
  v[10] = 0.25
  snd_display("mix 1->1 at 5: %s %s?", samps, v) unless  vequal(samps, v)
  snd_display("mix 1->1 at 5 tag: %s?", tag) unless mix?(tag)
  undo_edit
  tag = mix("test.snd", 0, 0, ind, 0, false)
  samps = channel2vct(0, 20)
  v = make_vct(20)
  v[2] = 0.5
  v[5] = 0.25
  snd_display("mix 1->1 at 0: %s %s?", samps, v) unless  vequal(samps, v)
  snd_display("mix 1->1 at 0 tag: %s?", tag) if mix?(tag)
  undo_edit
  indout = new_sound("test.snd", Mus_next, Mus_bshort, 22050, 2, "mix tests")
  insert_silence(0, 10, indout, 0)
  insert_silence(0, 10, indout, 1)
  set_sample(2, 0.5, indout, 0)
  set_sample(5, 0.25, indout, 0)
  set_sample(2, 0.95, indout, 1)
  set_sample(5, 0.125, indout, 1)
  save_sound(indout)
  close_sound(indout)
  tag = mix("test.snd", 0, 1)
  samps = channel2vct(0, 20)
  v = make_vct(20)
  v[2] = 0.95
  v[5] = 0.125
  snd_display("mix 2->1: %s %s?", samps, v) unless  vequal(samps, v)
  snd_display("mix 2->1 tag: %s?", tag) unless mix?(tag)
  undo_edit
  tag = mix("test.snd", 5, 1)
  samps = channel2vct(0, 20)
  v = make_vct(20)
  v[7] = 0.95
  v[10] = 0.125
  snd_display("mix 2->1 at 5: %s %s?", samps, v) unless  vequal(samps, v)
  snd_display("mix 2->1 at 5 tag: %s?", tag) unless mix?(tag)
  undo_edit
  close_sound(ind)
  #
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 2, "mix tests")
  insert_silence(0, 20, ind, 0)
  insert_silence(0, 20, ind, 1)
  tag = mix("test.snd")
  samps0 = channel2vct(0, 20, ind, 0)
  samps1 = channel2vct(0, 20, ind, 1)
  v = make_vct(20)
  v[2] = 0.5
  v[5] = 0.25
  snd_display("mix 1->1 (2): %s %s?", samps0, v) unless  vequal(samps0, v)
  v[2] = 0.95
  v[5] = 0.125
  snd_display("mix 1->1 (3): %s %s?", samps1, v) unless  vequal(samps1, v)
  snd_display("mix 1->1 tag: %s?", tag) unless mix?(tag)
  undo_edit(1, ind, 0)
  undo_edit(1, ind, 1)
  tag = mix("test.snd", 0, 1, ind, 1, false)
  samps0 = channel2vct(0, 20, ind, 0)
  samps1 = channel2vct(0, 20, ind, 1)
  v = make_vct(20)
  snd_display("mix 1->1 (4): %s %s?", samps0, v) unless  vequal(samps0, v)
  v[2] = 0.95
  v[5] = 0.125
  snd_display("mix 1->1 (5): %s %s?", samps1, v) unless  vequal(samps1, v)
  snd_display("mix 1->1 tag: %s?", tag) if mix?(tag)
  undo_edit(1, ind, 1)
  set_sync(1, ind)
  tag = mix("test.snd")
  samps0 = channel2vct(0, 20, ind, 0)
  samps1 = channel2vct(0, 20, ind, 1)
  v = make_vct(20)
  v[2] = 0.5
  v[5] = 0.25
  snd_display("mix 1->1 (6): %s %s?", samps0, v) unless  vequal(samps0, v)
  v[2] = 0.95
  v[5] = 0.125
  snd_display("mix 1->1 (7): %s %s?", samps1, v) unless  vequal(samps1, v)
  undo_edit
  set_cursor(5, ind)
  tag = mix("test.snd")
  samps0 = channel2vct(0, 20, ind, 0)
  samps1 = channel2vct(0, 20, ind, 1)
  v = make_vct(20)
  v[7] = 0.5
  v[10] = 0.25
  snd_display("mix 1->1 (8): %s %s?", samps0, v) unless  vequal(samps0, v)
  v[7] = 0.95
  v[10] = 0.125
  snd_display("mix 1->1 (9): %s %s?", samps1, v) unless  vequal(samps1, v)
  undo_edit
  close_sound(ind)
  delete_files("test.snd", "fmv.snd")
  #
  # check ripple_mixes
  #
  ind = open_sound("oboe.snd")
  data = channel2vct(100, 100)
  m1 = mix_vct(data, 321, ind, 0, true)
  m2 = mix_vct(data, 123, ind, 0, true)
  set_mix_position(m1, 500)
  if (res = mix_position(m1)) != 500
    snd_display("mix_position m1[0]: %s?", res)
  end
  if (res = mix_position(m2)) != 123
    snd_display("mix_position m2[0]: %s?", res)
  end
  undo_edit
  set_mix_position(m2, 500)
  if (res = mix_position(m2)) != 500
    snd_display("mix_position m2[1]: %s?", res)
  end
  if (res = mix_position(m1)) != 321
    snd_display("mix_position m1[1]: %s?", res)
  end
  undo_edit
  insert_silence(0, 100)
  if (res = mix_position(m1)) != 321 + 100
    snd_display("mix_position m1[2]: %s?", res)
  end
  if (res = mix_position(m2)) != 123 + 100
    snd_display("mix_position m2[2]: %s?", res)
  end
  delete_samples(0, 50)
  if (res = mix_position(m1)) != 321 + 50
    snd_display("mix_position m1[3]: %s?", res)
  end
  if (res = mix_position(m2)) != 123 + 50
    snd_display("mix_position m2[3]: %s?", res)
  end
  undo_edit(2)
  ntrack = make_track
  set_mix_track(m1, ntrack)
  set_mix_track(m2, ntrack)
  reverse_track(ntrack)
  if (res = mix_position(m1)) != 123
    snd_display("mix_position m1[4]: %s?", res)
  end
  if (res = mix_position(m2)) != 321
    snd_display("mix_position m2[4]: %s?", res)
  end
  undo_edit
  set_mix_position(m2, 500)
  undo_edit
  scale_channel(0.5, 1000, 100)
  if (res = mix_position(m2)) != 123
    snd_display("mix_position m2[5]: %s?", res)
  end
  if (res = mix_position(m1)) != 321
    snd_display("mix_position m1[5]: %s?", res)
  end
  undo_edit
  set_mix_position(m2, 500)
  undo_edit
  ptree_channel(lambda do |y| y * 0.5 end, 2000, 100)
  if (res = mix_position(m2)) != 123
    snd_display("mix_position m2[6]: %s?", res)
  end
  if (res = mix_position(m1)) != 321
    snd_display("mix_position m1[6]: %s?", res)
  end
  undo_edit
  set_mix_position(m2, 500)
  undo_edit
  ramp_channel(0.0, 1.0, 3000, 100)
  if (res = snd_catch do
        if (res = mix_position(m2)) != 123
          snd_display("mix_position m2[7]: %s?", res)
        end
        if (res = mix_position(m1)) != 321
          snd_display("mix_position m1[7]: %s?", res)
        end
      end).first
    snd_display("mix_position trouble: %s", res.inspect)
  end
  close_sound(ind)
  #
  # check that current console is correct
  #
  ind = open_sound("storm.snd")
  set_x_bounds([0, 80])
  make_selection(1000000, 1050000)
  m1 = mix_selection(900000)
  m2 = mix_selection(400000)
  as_one_edit(lambda do | |
                set_mix_position(m1, 0)
                set_mix_position(m2, 1)
              end)
  if (res1 = mix_position(m1)) != 0 or (res2 = mix_position(m2)) != 1
    snd_display("as_one_edit positions: %s %s?", res1, res2)
  end
  undo_channel
  if (res1 = mix_position(m1)) != 900000 or (res2 = mix_position(m2)) != 400000
    snd_display("as_one_edit positions after undo: (%s): %s (%s): %s?", m1, res1, m2, res2)
  end
  redo_channel
  if (res1 = mix_position(m1)) != 0 or (res2 = mix_position(m2)) != 1
    snd_display("as_one_edit positions after redo: %s %s?", res1, res2)
  end
  close_sound(ind)
  #
  ind = open_sound("2.snd")
  make_selection(0, 10000, ind)
  if (res = selection_chans) != 2
    snd_display("stereo selection: %s?", res)
  end
  set_sync(true, ind)
  md = mix_selection(500, ind)
  if (res = mix_chans(md)) != 2
    snd_display("mix_chans of stereo selection: %s?", res)
  end
  unless mix?(md + 1)
    snd_display("where is 2nd mix? %s %s?", md, mixes)
  end
  if (res = edit_position(ind, 0)) != 1
    snd_display("edit_position 0 after stereo mix selection: %s?", res)
  end
  if (res = edit_position(ind, 1)) != 1
    snd_display("edit_position 1 after stereo mix selection: %s?", res)
  end
  set_sync(false, ind)
  undo_edit(1, ind, 0)
  delete_sample(25, ind, 0)
  set_mix_position(md + 1, 750)
  if (res = edit_position(ind, 1)) != 2
    snd_display("edit_position 1 after stereo mix selection moved: %s?", res)
  end
  revert_sound(ind)
  delete_sample(25, ind, 1)
  if mix?(md) or mix?(md + 1)
    snd_display("undo mix stereo selection: %s %s?", mix?(dm), mix?(md + 1))
  end
  close_sound(ind)
end

def test_mix_disconnect(name, id0, chn0, id1, chn1)
  amp0 = mix_amp(id0, chn0)
  amp1 = mix_amp(id1, chn1)
  env0 = mix_amp_env(id0, chn0)
  env1 = mix_amp_env(id1, chn1)
  set_mix_amp(id0, chn0, mix_amp(id0, chn0) * 0.5)
  if fneq(mix_amp(id0, chn0), 0.5 * amp0) or
      fneq(mix_amp(id1, chn1), amp1)
    snd_display("pan_mix disconnect amp %s: %f (%f) %f (%f)?",
                name, mix_amp(id0, chn0), amp0, mix_amp(id1, chn1), amp1)
  end
  set_mix_amp_env(id1, chn1, [0.0, random(1.0), 1.0, random(1.0)])
  if mix_amp_env(id0, chn0) != env0 or (not vequal(mix_amp_env(id1, chn1), (env1 or [])))
    snd_display("pan_mix disconnect amp_env %s: %s (%s) %s (%s)?",
                name,
                mix_amp_env(id0, chn0), env0,
                mix_amp_env(id1, chn1), env1)
  end
  if id0 != id1
    pos0 = mix_position(id0)
    pos1 = mix_position(id1)
    spd0 = mix_speed(id0)
    spd1 = mix_speed(id1)
    set_mix_position(id0, pos0 + 12)
    if mix_position(id0) == pos0 or mix_position(id1) != pos1
      snd_display("pan_mix disconnect position %s: %s (%s) %s (%s)?",
                  name, mix_position(id0), pos0, mix_position(id1), pos1)
    end
    set_mix_speed(id1, mix_speed(id1) * 1.5)
    if fneq(mix_speed(id1), 1.5 * spd1) or fneq(mix_speed(id0), spd0)
      snd_display("pan_mix disconnect speed %s: %s (%s) %s (%s)?",
                  name, mix_speed(id0), spd0, mix_speed(id1), spd1)
    end
  end
end

def check_copied_mix(original, copy, pos)
  snd_display("copy_mix returns bad mix: %s?", copy) unless mix?(copy)
  if (res = mix_track(copy)).nonzero?
    snd_display("copy_mix set track: %s %s?", res, mix_track(original))
  end
  if (res1 = mix_chans(copy)) != (res2 = mix_chans(original))
    snd_display("copy_mix chans: %s %s?", res1, res2)
  end
  if (res1 = mix_tag_position(copy)) != (res2 = mix_tag_position(original))
    snd_display("copy_mix anchor: %s %s?", res1, res2)
  end
  if (res1 = mix_frames(copy)) != (res2 = mix_frames(original))
    snd_display("copy_mix frames: %s %s?", res1, res2)
  end
  if (res = mix_position(copy)) != pos
    snd_display("copy_mix set position: %s %s?", res, pos)
  end
  if (res1 = mix_speed(copy)) != (res2 = mix_speed(original))
    snd_display("copy_mix speed: %s %s?", res1, res2)
  end
  if (res1 = mix_maxamp(copy)) != (res2 = mix_maxamp(original))
    snd_display("copy_mix maxamp: %s %s?", res1, res2)
  end
  mix_chans(copy).times do |i|
    if fneq(res1 = mix_amp(copy, i), res2 = mix_amp(original, i))
      snd_display("copy_mix amp[%d]: %f %f?", i, res1, res2)
    end
    copy_amp_env = mix_amp_env(copy, i)
    original_amp_env = mix_amp_env(original, i)
    if array?(copy_amp_env) and array?(original_amp_env)
      unless vequal(copy_amp_env, original_amp_env)
        snd_display("copy_mix amp_env[%d]: %s %s?", i, copy_amp_env, original_amp_env)
      end
    else
      if copy_amp_env != original_amp_env
        snd_display("copy_mix amp_env[%d]: %s %s?", i, copy_amp_env, original_amp_env)
      end
    end
  end
  if (res1 = mix_home(copy)) != (res2 = mix_home(original))
    snd_display("copy_mix home: %s %s?", res1, res2)
  end
end

def test039
  ind = new_sound("test.snd")
  v = Vct.new(20) do |i| i * 0.01 end
  vct2channel(v)
  v.map! do |val| -val end
  mx = mix_vct(v, 10)
  hi = make_mix_sample_reader(mx, 0)
  ho = make_mix_sample_reader(mx, 5)
  10.times do |i|
    ho_val = ho.call
    hi_val = hi.call
    if fneq(hi_val, i * -0.01)
      snd_display("mix_reader at %d from 0: %f?", i, hi_val)
      break
    end
    if fneq(ho_val, (i + 5)* -0.01)
      snd_display("mix_reader at %d from 5: %f?", i, ho_val)
      break
    end
  end
  revert_sound(ind)
  v = Vct.new(21)
  v.fill(0.5)
  vct2channel(v)
  mx = mix_vct(v, 10)
  set_mix_amp_env(mx, 0, [0, 0, 1, 1])
  hi = make_mix_sample_reader(mx, 0)
  ho = make_mix_sample_reader(mx, 10)
  10.times do |i|
    ho_val = ho.call
    hi_val = hi.call
    if fneq(hi_val, i * 0.025)
      snd_display("mix_reader env\'d at %d from 0: %f?", i, hi_val)
      break
    end
    if fneq(ho_val, (i + 10)* 0.025)
      snd_display("mix_reader env\'d at %d from 5: %f?", i, ho_val)
      break
    end
  end
  close_sound(ind)
  #
  ind = open_sound("oboe.snd")
  id = mix_vct(Vct.new(10, 0.1))
  set_mix_position(id, 100)
  if (res1 = mix_position(id)) != 100 or (res2 = edit_position(ind, 0)) != 2
    snd_display("mix_position init: %s %s?", res1, res2)
  end
  set_mix_position(id, 100)
  if (res1 = mix_position(id)) != mix_position(id) or (res2 = edit_position(ind, 0)) != 2
    snd_display("mix_position 2 (no-op): %s %s?", res1, res2)
  end
  set_mix_amp(id, 0, 1.0)
  if fneq(res1 = mix_amp(id, 0), 1.0) or (res2 = edit_position(ind, 0)) != 2
    snd_display("mix_amp no-op: %s %s?", res1, res2)
  end
  set_mix_amp(id, 0, 0.5)
  if fneq(res1 = mix_amp(id, 0), 0.5) or (res2 = edit_position(ind, 0)) != 3
    snd_display("mix_amp 0.5: %s %s?", res1, res2)
  end
  set_mix_amp(id, 0, mix_amp(id, 0))
  if fneq(res1 = mix_amp(id, 0), 0.5) or (res2 = edit_position(ind, 0)) != 3
    snd_display("mix_amp no-op: %s %s?", res1, res2)
  end
  set_mix_speed(id, 1.0)
  if fneq(res1 = mix_speed(id), 1.0) or (res2 = edit_position(ind, 0)) != 3
    snd_display("mix_speed no-op: %s %s?", res1, res2)
  end
  set_mix_speed(id, 0.5)
  if fneq(res1 = mix_speed(id), 0.5) or (res2 = edit_position(ind, 0)) != 4
    snd_display("mix_speed 0.5: %s %s?", res1, res2)
  end
  set_mix_speed(id, mix_speed(id))
  if fneq(res1 = mix_speed(id), 0.5) or (res2 = edit_position(ind, 0)) != 4
    snd_display("mix_speed 2 no-op: %s %s?", res1, res2)
  end
  set_mix_amp_env(id, 0, [0, 0, 1, 1])
  if (res = edit_position(ind, 0)) != 5
    snd_display("mix_amp init: %s %s?", mix_amp_env(id, 0), res)
  end
  set_mix_amp_env(id, 0, [0, 0, 1, 1])
  if (res = edit_position(ind, 0)) != 5
    snd_display("mix_amp no-op: %s %s?", mix_amp_env(id, 0), res)
  end
  close_sound(ind)
  #
  if provided?(:snd_motif) and provided?(:xm)
    ind = open_sound("oboe.snd")
    mix1 = mix_vct([0.1, 0.2, 0.3].to_vct, 120, ind, 0, true, "origin!")
    mix2 = mix_vct([0.1, 0.2, 0.3].to_vct, 1200, ind, 0, true)
    mix3 = mix_vct([0.1, 0.2, 0.3].to_vct, 12000, ind, 0, true)
    set_mix_track(mix1, make_track)
    set_mix_track(mix3, mix_track(mix1))
    if mixes(ind, 0) != [mix1, mix2, mix3]
      snd_display("mixes: %s %s?", mixes(ind, 0), [mix1, mix2, mix3])
    end
    if mixes() != [[[mix1, mix2, mix3]]]
      snd_display("mixes all: %s %s?", mixes(), [[[mix1, mix2, mix3]]])
    end
    view_mixes_dialog
    set_mix_dialog_mix(mix1)
    mixd = dialog_widgets[16]
    nxt = find_child(mixd, "Next")
    prev = find_child(mixd, "Previous")
    tplay = find_child(mixd, "mix-track-play")
    click_button(tplay)
    force_event
    if (not RXtIsSensitive(nxt)) or RXtIsSensitive(prev)
      snd_display("mix_dialog next/previous: %s %s %s %s?",
                  nxt, RXtIsSensitive(nxt), prev, RXtIsSensitive(prev))
    end
    click_button(nxt)
    force_event
    click_button(nxt)
    force_event
    if RXtIsSensitive(nxt) or (not RXtIsSensitive(prev))
      snd_display("mix_dialog next/previous: %s %s %s %s?",
                  nxt, RXtIsSensitive(nxt), prev, RXtIsSensitive(prev))
    end
    click_button(prev)
    force_event
    click_button(prev)
    force_event
    dismiss_all_dialogs
    close_sound(ind)
  end
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "copy_mix tests", 300)
  mix1 = mix_vct(Vct.new(10, 0.5), 10)
  copy_mix1 = copy_mix(mix1)
  old_color = mix_color(mix1)
  set_mix_color(mix1, make_color_with_catch(0, 1, 1))
  new_color = mix_color(mix1)
  if (res1 = new_color) != (res2 = make_color_with_catch(0, 1, 1))
    snd_display("mix_color %s %s %s %s?", mix1, res2, res1, old_color)
  end
  check_copied_mix(mix1, copy_mix1, 10)
  set_mix_amp(mix1, 2.0)
  copy_mix1 = copy_mix(mix1, 20)
  check_copied_mix(mix1, copy_mix1, 20)
  set_mix_speed(mix1, 2.0)
  copy_mix1 = copy_mix(mix1, 40)
  check_copied_mix(mix1, copy_mix1, 40)
  set_mix_amp_env(mix1, 0, [0, 0, 1, 1])
  copy_mix1 = copy_mix(mix1, 60)
  check_copied_mix(mix1, copy_mix1, 60)
  set_mix_tag_position(mix1, 2)
  copy_mix1 = copy_mix(mix1, 80)
  check_copied_mix(mix1, copy_mix1, 80)
  set_mix_position(mix1, 100)
  copy_mix1 = copy_mix(mix1)
  check_copied_mix(mix1, copy_mix1, 100)
  mix1 = mix("2a.snd", 100, true, ind)
  set_mix_amp(mix1, 0, 0.5)
  set_mix_amp(mix1, 1, 0.5)
  set_mix_amp_env(mix1,0, [0, 1, 1, 0])
  copy_mix1 = copy_mix(mix1, 200)
  check_copied_mix(mix1, copy_mix1, 200)
  set_mix_amp_env(mix1,0, [0, 0, 1, 1])
  copy_mix1 = copy_mix(mix1, 300)
  check_copied_mix(mix1, copy_mix1, 300)
  mix2 = copy_mix(copy_mix1, 500)
  check_copied_mix(copy_mix1, mix2, 500)
  close_sound(ind)
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 4, "copy_mix tests", 300)
  mix1 = mix("2a.snd", 10, true, ind, 2)
  copy_mix1 = copy_mix(mix1, 20)
  check_copied_mix(mix1, copy_mix1, 20)
  close_sound(ind)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "copy_mix tests", 300)
  old_color = mix_color()
  set_mix_color(make_color_with_catch(1, 1, 0))
  mix1 = mix_vct(Vct.new(10, 0.5), 10)
  if (res1 = mix_color) != (res2 = make_color_with_catch(1, 1, 0)) or
      (res3 = mix_color(mix1)) != make_color_with_catch(1, 1, 0)
    snd_display("set_mix_color %s %s %s %s?", res1, res3, res2, old_color)
  end
  set_mix_color(old_color)
  save_mix(mix1, "test.snd")
  ind1 = open_sound("test.snd")
  if (res1 = frames(ind1)) != (res2 = mix_frames(mix1))
    snd_display("save_mix frames: %s %s?", res1, res2)
  end
  unless vequal(res1 = channel2vct(0, 10, ind1), res2 = mix2vct(mix1))
    snd_display("save_mix data: %s %s?", res1, res2)
  end
  close_sound(ind1)
  close_sound(ind)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "lock mix tests", 300)
  mix1 = mix_vct(Vct.new(10, 0.5), 10)
  if res = mix_locked?(mix1)
    snd_display("make mix locked: %s %s?", mix1, res)
  end
  delete_mix(mix1)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("delete_mix maxamp: %f?", res)
  end
  unless res = mix_locked?(mix1)
    snd_display("delete_mix not locked: %s %s?", mix1, res)
  end
  undo_channel(1, ind, 0)
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("undelete mix maxamp: %f?", res)
  end
  if res = mix_locked?(mix1)
    snd_display("undelete mix locked: %s %s?", mix1, res)
  end
  redo_channel(1, ind, 0)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("redelete mix maxamp: %f?", res)
  end
  unless res = mix_locked?(mix1)
    snd_display("redelete mix not locked: %s %s?", mix1, res)
  end
  undo_edit(2)
  snd_display("undo 2 kept mix?") if mix?(mix1)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("no delete_mix maxamp: %f?", res)
  end
  if (res = snd_catch do
        if res = mix_locked?(mix1)
          snd_display("no delete_mix locked: %s %s?", mix1, res)
        end
      end).first != :no_such_mix
    snd_display("pending mix release accessible?")
  end
  if (res = snd_catch do set_mix_locked?(mix1, true) end).first != :no_such_mix
    snd_display("pending mix release settable?")
  end
  redo_edit
  if res = mix_locked?(mix1)
    snd_display("mix un/locked: %s %s?", mix1, res)
  end
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("reundelete mix maxamp: %f?", res)
  end
  track1 = make_track(mix1)
  delete_track(track1)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("delete_track maxamp: %f?", res)
  end
  undo_edit
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("undelete track maxamp: %f?", res)
  end
  redo_edit
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("redelete track maxamp: %f?", res)
  end
  revert_sound(ind)
  mix2 = mix_vct(Vct.new(10, 0.5), 10)
  track2 = make_track(mix2)
  set_track_amp_env(track2, [0, 0, 1, 1])
  delete_track(track2)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("delete_track (amp_env) maxamp: %f?", res)
  end
  undo_edit
  mix3 = mix_vct(Vct.new(10, 0.5), 10)
  set_mix_track(mix3, track2)
  delete_track(track2)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("redelete track (amp_env) maxamp: %f?", res)
  end
  close_sound(ind)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "mix undo tests", 300)
  mix1 = mix_vct(Vct.new(10, 0.1), 10)
  track1 = make_track
  track2 = make_track
  set_mix_track(mix1, track1)
  if (res = mix_track(mix1)) != track1
    snd_display("mix_track start %s: %s?", track1, res)
  end
  set_track_amp(track2, 2.0)
  set_mix_amp(mix1, 0, 3.0)
  if fneq(res = maxamp(ind, 0), 0.3)
    snd_display("mix_track chain setup: %f %f?", res, mix_amp(mix1, 0))
  end
  set_mix_track(mix1, track2)
  if (res = mix_track(mix1)) != track2
    snd_display("set_mix_track %s: %s?", track2, res)
  end
  if fneq(res = maxamp(ind, 0), 0.6)
    snd_display("mix_track chain set: %f %f?", res, mix_amp(mix1, 0))
  end
  undo_edit
  if (res = mix_track(mix1)) != track1
    snd_display("mix_track undo %s: %s?", track1, res)
  end
  if fneq(res = maxamp(ind, 0), 0.3)
    snd_display("mix_track undo setup: %f %f?", res, mix_amp(mix1, 0))
  end
  undo_edit(2)  
  if (res = mix_track(mix1)) != 0
    snd_display("mix_track undo2: %s %s %s: %s?", track1, track2, res, edit_position(ind, 0))
  end
  if fneq(res = maxamp(ind, 0), 0.1)
    snd_display("mix_track chain undo: %f %f?", res, mix_amp(mix1, 0))
  end
  set_mix_position(mix1, 20)
  if fneq(res = maxamp(ind, 0), 0.1)
    snd_display("mix_track chain undo: %f %f?", res, mix_amp(mix1, 0))
  end
  pos0 = mix_tag_position(mix1)
  y0 = mix_tag_y(mix1)
  set_mix_amp(mix1, 1.0)
  set_mix_tag_position(mix1, 3)
  set_mix_tag_y(mix1, 6)
  if (res = mix_tag_position(mix1)) != 3
    snd_display("mix_tag_position chain test 0: %s?", res)
  end
  if (res = mix_tag_y(mix1)) != 6
    snd_display("mix_tag_y chain test 0: %s?", res)
  end
  undo_edit
  if (res = mix_tag_position(mix1)) != pos0
    snd_display("mix_tag_position chain test 1: %s?", res)
  end
  if (res = mix_tag_y(mix1)) != y0
    snd_display("mix_tag_y chain test 1: %s?", res)
  end
  close_sound(ind)
end

def check_copied_track(original, copy, pos)
  snd_display("copy_track returns bad track: %s?", copy) unless track?(copy)
  if (res = track_track(copy)).nonzero?
    snd_display("copy_track set track: %s %s?", res, track_track(original))
  end
  if (res1 = track_chans(copy)) != (res2 = track_chans(original))
    snd_display("copy_track chans: %s %s?", res1, res2)
  end
  if (res1 = track_frames(copy)) != (res2 = track_frames(original))
    snd_display("copy_track frames: %s %s?", res1, res2)
  end
  if (res = track_position(copy)) != pos
    snd_display("copy_track set position: %s %s?", res, pos)
  end
  if (res1 = track_speed(copy)) != (res2 = track_speed(original))
    snd_display("copy_track speed: %s %s?", res1, res2)
  end
  if (res1 = track_amp(copy)) != (res2 = track_amp(original))
    snd_display("copy_track amp: %s %s?", res1, res2)
  end
  copy_amp_env = track_amp_env(copy)
  original_amp_env = track_amp_env(original)
  if array?(copy_amp_env) and array?(original_amp_env)
    unless vequal(copy_amp_env, original_amp_env)
      snd_display("copy_track amp_env: %s %s?", copy_amp_env, original_amp_env)
    end
  else
    if copy_amp_env != original_amp_env
      snd_display("copy_track amp_env: %s %s?", copy_amp_env, original_amp_env)
    end
  end
  if (res1 = (track(copy) or []).length) != (res2 = (track(original) or []).length)
    snd_display("copy_track mix lists differ: %s %s?", res1, res2)
  else
    if track(original) and track(copy)
      track(original).zip(track(copy)) do |orig, cop|
        if (res1 = mix_track(orig)) == original
          if (res2 = mix_track(cop)) != copy
            snd_display("copy_track mix_tracks: %s -> %s?", res1, res2)
          end
        end
      end
    end
  end
  if track_chans(original) > 0
    unless vequal(vcopy = track2vct(copy), vorig = track2vct(original))
      snd_display("copy_track data: %s %s?", vcopy, vorig)
    end
  end
end

# state: [amp, speed, track, env, color]
def track_states_match?(track_id, state)
  (track_amp(track_id) - state[0]).abs < 0.0001 and
    (track_speed(track_id) - state[1]).abs < 0.0001 and
    track_track(track_id) == state[2] and
    (((not track_amp_env(track_id)) and (not state[3])) or
       vequal(track_amp_env(track_id), state[3])) and
    track_color(track_id) == state[4]
end

def track_state2list(track_id)
  [track_amp(track_id),
   track_speed(track_id),
   track_track(track_id),
   track_amp_env(track_id),
   track_color(track_id)]
end

def test049
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "copy-mix tests", 300)
  mix1 = mix_vct(Vct.new(10, 0.1), 10)
  track1 = make_track
  track2 = make_track
  edpos = edit_position(ind, 0)
  set_mix_track(mix1, track1)
  if (res = mix_track(mix1)) != track1
    snd_display("mix_track start %s: %s", track1, res)
  end
  set_track_amp(track2, 2.0)
  set_mix_amp(mix1, 0, 3.0)
  if fneq(res = maxamp(ind, 0), 0.3)
    snd_display("mix_track chain setup: %f %f?", res, mix_amp(mix1, 0))
  end
  set_mix_track(mix1, track2)
  if (res = mix_track(mix1)) != track2
    snd_display("set_mix_track %s: %s?", track2, res)
  end
  if fneq(res = maxamp(ind, 0), 0.6)
    snd_display("mix_track chain set: %f %f?", res, mix_amp(mix1, 0))
  end
  undo_edit
  if (res = mix_track(mix1)) != track1
    snd_display("mix_track undo %s: %s?", track1, res)
  end
  if fneq(res = maxamp(ind, 0), 0.3)
    snd_display("mix_track undo setup: %f %f?", res, mix_amp(mix1, 0))
  end
  set_edit_position(edpos, ind, 0)
  if (res = mix_track(mix1)) != 0
    snd_display("mix_track undo2: %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.1)
    snd_display("mix_track chain undo: %f %f?", res, mix_amp(mix1, 0))
  end
  set_mix_position(mix1, 20)
  if fneq(res = maxamp(ind, 0), 0.1)
    snd_display("mix_track chain undo: %f %f?", res, mix_amp(mix1, 0))
  end
  pos0 = mix_tag_position(mix1)
  y0 = mix_tag_y(mix1)
  set_mix_amp(mix1, 1.0)
  set_mix_tag_position(mix1, 3)
  set_mix_tag_y(mix1, 6)
  set_mix_amp(mix1, 0, 0.1)
  if (res = mix_tag_position(mix1)) != 3
    snd_display("mix_tag_position chain test 0: %s?", res)
  end
  if (res = mix_tag_y(mix1)) != 6
    snd_display("mix_tag_y chain test 0: %s?", res)
  end
  undo_edit
  if (res = mix_tag_position(mix1)) != pos0
    snd_display("mix_tag_position chain test 1: %s?", res)
  end
  if (res = mix_tag_y(mix1)) != y0
    snd_display("mix_tag_y chain test 1: %s?", res)
  end
  redo_edit
  if (res = mix_tag_position(mix1)) != 3
    snd_display("mix_tag_position chain test 2: %s?", res)
  end
  if (res = mix_tag_y(mix1)) != 6
    snd_display("mix_tag_y chain test 2: %s?", res)
  end
  close_sound(ind)
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "copy-track tests",  300)
  if (res = snd_catch do copy_track(0) end).first != :no_such_track
    snd_display("copy_track 0: %s", res)
  end
  mix1 = mix_vct(Vct.new(10, 1.0), 100)
  track0 = make_track
  track1 = make_track(mix1)
  if (res = track(track1)) != [mix1]
    snd_display("make_track for copy: %s %s?", mix1, res)
  end
  if (res = mix_track(mix1)) != track1
    snd_display("make_track for copy mix: %s %s?", res, track1)
  end
  copy_track0 = copy_track(track0)
  edpos = edit_position(ind, 0)
  copy_track1 = copy_track(track1)
  if (res = track(track1)) != [mix1]
    snd_display("copy_track clobbered original: %s %s?", mix1, res)
  end
  if (res = mix_track(mix1)) != track1
    snd_display("copy_track for clobbered original: %s %s?", res, track1)
  end
  check_copied_track(track0, copy_track0, -1)
  check_copied_track(track1, copy_track1, 100)
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("copy_track not atomic? %s %s?", edpos, res)
  end
  undo_edit(2)
  mix1 = mix_vct(Vct.new(10, 1.0), 50)
  track1 = make_track(mix1)
  track2 = copy_track(track1, 200)
  check_copied_track(track1, track2, 200)
  undo_edit
  set_track_amp(track1, 2.0)
  set_track_amp_env(track1, [0, 0, 1, 1])
  set_mix_amp(mix1, 0.25)
  track2 = copy_track(track1, 200)
  check_copied_track(track1, track2, 200)
  revert_sound(ind)
  mix1 = mix_vct(Vct.new(10, 1.0), 50)
  mix2 = mix_vct(Vct.new(10, 1.0), 75)
  track1 = make_track(mix1, mix2)
  track2 = copy_track(track1, 200)
  check_copied_track(track1, track2, 200)
  mix3 = mix_vct(Vct.new(10, 1.0), 10)
  mix4 = mix_vct(Vct.new(10, 1.0), 20)
  track3 = make_track(mix3, mix4)
  set_track_track(track3, track1)
  track2 = copy_track(track1, 300)
  check_copied_track(track1, track2, 300)
  close_sound(ind)
  #
  # empty and 1-mix tracks
  #
  old_with_mix_tags = with_mix_tags
  set_with_mix_tags(true)
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "track tests",  1000)
  mix1 = mix_vct(Vct.new(10, 0.4), 100)
  snd_display("can\'t even get track tests started!") unless mix?(mix1)
  track1 = make_track(mix1)
  snd_display("track? %s", track1) unless track?(track1)
  if (res = snd_catch do set_track_track(track1, -1) end).first != :out_of_range
    snd_display("set_track_track -1: %s (%s)", res, track_track(track1))
  end
  if (res = track_chans(track1)) != 1
    snd_display("track_chans 1 mix: %s?", res)
  end
  if (res = mix_track(mix1)) != track1
    snd_display("make_track didn\'t set track? %s?", res)
  end
  if (res = track(track1)) != [mix1]
    snd_display("track 1: %s %s?", mix1, res)
  end
  unless track_states_match?(track1, [1.0, 1.0, 0, nil, false])
    snd_display("track states 1: %s?", track_state2list(track1))
  end
  if (res = edit_position(ind, 0)) != 2
    snd_display("tracked mix edit position: %s %s?", res, edit_tree(ind, 0))
  end
  if fneq(res = maxamp(ind, 0), 0.4)
    snd_display("mixed maxamp 0.4: %f?", res)
  end
  if (res = track_position(track1)) != 100
    snd_display("track position mix1: %s?", res)
  end
  if (res = track_frames(track1)) != 10
    snd_display("track frames mix1: %s?", res)
  end
  set_track_amp(track1, 2.0)
  unless track_states_match?(track1, [2.0, 1.0, 0, nil, false])
    snd_display("track states 2: %s?", track_state2list(track1))
  end
  if (res = edit_position(ind, 0)) != 3
    snd_display("tracked mix amp-2 edit position: %s %s?", res, edit_tree(ind, 0))
  end
  if fneq(res = maxamp(ind, 0), 0.8)
    snd_display("mixed maxamp 0.8: %f?", res)
  end
  undo_edit(1)
  unless track_states_match?(track1, [1.0, 1.0, 0, nil, false])
    snd_display("track states 3 (undo): %s?", track_state2list(track1))
  end
  if (res = edit_position(ind, 0)) != 2
    snd_display("tracked mix edit position (undo): %s %s?", res, edit_tree(ind, 0))
  end
  if fneq(res = maxamp(ind, 0), 0.4)
    snd_display("mixed maxamp 0.4 (undo): %f?", res)
  end
  redo_edit(1)
  unless track_states_match?(track1, [2.0, 1.0, 0, nil, false])
    snd_display("track states 4 (redo): %s?", track_state2list(track1))
  end
  if (res = edit_position(ind, 0)) != 3
    snd_display("tracked mix amp-2 edit position (redo): %s %s?", res, edit_tree(ind, 0))
  end
  if fneq(res = maxamp(ind, 0), 0.8)
    snd_display("mixed maxamp 0.8 (redo): %f?", res)
  end
  mr = make_mix_sample_reader(mix1)
  tr = make_track_sample_reader(track1)
  mix_frames(mix1).times do |i|
    ms = read_mix_sample(mr)
    ts = read_track_sample(tr)
    if fneq(ms, ts) or fneq(ms, 0.8)
      snd_display("mix+track readers: %s %s (0.8)?", ms, ts)
      break
    end
  end
  [99, 105, 111,
   199, 207, 211,
   299, 306, 311].zip([0.0, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]) do |samp, val|
    snd_display("track read (1) %d: %s (%s)?", samp, sample(samp), val) if fneq(sample(samp), val)
  end
  #
  snd_display("track read 99: %f?", sample(99)) if fneq(sample(99), 0.0)
  snd_display("track read 111: %f?", sample(111)) if fneq(sample(111), 0.0)
  set_track_position(track1, 200)
  if (res = track_position(track1)) != 200
    snd_display("track_position mix1 200: %s?", res)
  end
  if (res = track_frames(track1)) != 10
    snd_display("track_frames mix1 200: %s?", res)
  end
  if (res = mix_position(mix1)) != 200
    snd_display("mix_position mix1 1 200: %s?", res)
  end
  if (res = mix_frames(mix1)) != 10
    snd_display("mix_frames mix1 1 200: %s?", res)
  end
  unless track_states_match?(track1, [2.0, 1.0, 0, nil, false])
    snd_display("track states 5 (move): %s?", track_state2list(track1))
  end
  [99, 105, 111,
   199, 207, 211,
   299, 306, 311].zip([0.0, 0.0, 0.0, 0.0, 0.8, 0.0, 0.0, 0.0, 0.0]) do |samp, val|
    snd_display("track read (2) %d: %s (%s)?", samp, sample(samp), val) if fneq(sample(samp), val)
  end
  undo_edit(1)
  #
  set_track_position(track1, 300)
  if (res = track_position(track1)) != 300
    snd_display("track_position mix1 300: %s?", res)
  end
  if (res = track_frames(track1)) != 10
    snd_display("track_frames mix1 300: %s?", res)
  end
  if (res = mix_position(mix1)) != 300
    snd_display("mix_position mix1 1 300: %s?", res)
  end
  if (res = mix_frames(mix1)) != 10
    snd_display("mix_frames mix1 1 300: %s?", res)
  end
  unless track_states_match?(track1, [2.0, 1.0, 0, nil, false])
    snd_display("track states 6 (move mix): %s?", track_state2list(track1))
  end
  [99, 105, 111,
   199, 207, 211,
   299, 306, 311].zip([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 0.0]) do |samp, val|
    snd_display("track read (3) %d: %s (%s)?", samp, sample(samp), val) if fneq(sample(samp), val)
  end
  undo_edit(1)
  #
  track2 = make_track
  pos = edit_position(ind)
  if (res = track(track2)) != nil # []
    snd_display("empty track: %s %s?", mix1, res)
  end
  if (res = track_chans(track2)).nonzero?
    snd_display("track_chans no mix: %s?", res)
  end
  if (res = track_position(track2)) != -1
    snd_display("empyt track position: %s?", res)
  end
  if (res = track_frames(track2)).nonzero?
    snd_display("empyt track frames: %s?", res)
  end
  unless track_states_match?(track2, [1.0, 1.0, 0, nil, false])
    snd_display("empty track states: %s?", track_state2list(track2))
  end
  if (res = snd_catch do set_track_track(track1, track1) end).first != :out_of_range
    snd_display("circular track: %s", res.inspect)
  end
  set_track_track(track1, track2)
  unless track_states_match?(track1, [2.0, 1.0, track2, nil, false])
    snd_display("track states 8 (track): %s?", track_state2list(track1))
  end
  if (res = snd_catch do set_track_track(track2, track1) end).first != :out_of_range
    snd_display("circular track 2: %s", res.inspect)
  end
  if fneq(res = maxamp(ind, 0), 0.8)
    snd_display("track+track maxamp 0.8: %f?", res)
  end
  set_track_amp(track2, 0.5)
  unless track_states_match?(track2, [0.5, 1.0, 0, nil, false])
    snd_display("empty track states 9 (amp): %s?", track_state2list(track2))
  end
  if fneq(res = maxamp(ind, 0), 0.4)
    snd_display("track+track maxamp 0.4: %f?", res)
  end
  set_track_amp(track1, 0.5)
  if fneq(res = maxamp(ind, 0), 0.1)
    snd_display("track+track maxamp 0.1: %f?", res)
  end
  set_track_track(track1, 0)
  if fneq(res = maxamp(ind, 0), 0.2)
    snd_display("track+track maxamp 0.2: %f?", res)
  end
  set_edit_position(pos, ind)
  unless track_states_match?(track1, [2.0, 1.0, 0, nil, false])
    snd_display("track states 10 (edit-pos): %s?", track_state2list(track1))
  end
  set_track_speed(track1, 0.5)
  if (res = mix_frames(mix1)) != 20
    snd_display("mix_frames with track_speed: %s?", res)
  end
  if (res = track_frames(track1)) != 20
    snd_display("track_frames with track_speed: %s?", res)
  end
  unless track_states_match?(track1, [2.0, 0.5, 0, nil, false])
    snd_display("track states 11 (speed): %s?", track_state2list(track1))
  end
  set_mix_track(mix1, 0)
  if (res = track(track1)) != nil # []
    snd_display("empty track 1: %s %s?", mix1, res)
  end
  if (res = mix_frames(mix1)) != 10
    snd_display("mix_frames without track_speed: %s?", res)
  end
  if (res = track_frames(track1)) != 0
    snd_display("track_frames without track_speed: %s?", res)
  end
  if (res = track_position(track1)) != -1
    snd_display("empty track1 position: %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.4)
    snd_display("no track maxamp 0.4: %f?", res)
  end
  set_mix_track(mix1, track2)
  if (res = track(track2)) != [mix1]
    snd_display("track 2: %s %s?", mix1, res)
  end
  if (res = track(track1)) != nil
    snd_display("empty track (set 2): %s %s %s?", mix1, res, track(track2))
  end
  if (res = track_position(track2)) != 100
    snd_display("track 2 position mix1: %s?", res)
  end
  if (res = track_frames(track2)) != 10
    snd_display("track 2 frames mix1: %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.2)
    snd_display("track 2 maxamp 0.2: %f %f %f?", res, mix_amp(mix1), track_amp(track2))
  end
  set_mix_amp_env(mix1, 0, [0, 0, 1, 1])
  tv = track2vct(track2)
  mv = mix2vct(mix1)
  if mv.length != tv.length or
      (not vequal(tv, mv)) or
      (not vequal(tv, [0, 0.022, 0.044, 0.067, 0.089, 0.111, 0.133, 0.156, 0.178, 0.2].to_vct))
    snd_display("amp env ramp track2 mix1: %s %s?", tv, mv)
  end
  set_track_amp_env(track2, [0, 0, 1, 1])
  tv = track2vct(track2)
  mv = mix2vct(mix1)
  if mv.length != tv.length or
      (not vequal(tv, mv)) or
      (not vequal(tv, [0, 0.002, 0.008, 0.018, 0.032, 0.05, 0.072, 0.098, 0.128, 0.162].to_vct))
    snd_display("amp env 2 ramp track2 mix1: %s %s?", tv, mv)
  end
  set_mix_amp_env(mix1, 0, false)
  tv = track2vct(track2)
  mv = mix2vct(mix1)
  if mv.length != tv.length or
      (not vequal(tv, mv)) or
      (not vequal(tv, [0, 0.022, 0.044, 0.067, 0.089, 0.111, 0.133, 0.156, 0.178, 0.2].to_vct))
    snd_display("amp env ramp track2: %s %s?", tv, mv)
  end
  set_track_speed(track1, 1.0)
  set_mix_track(mix1, track1)
  tv = track2vct(track1)
  mv = mix2vct(mix1)
  if mv.length != tv.length or
      (not vequal(tv, mv)) or
      (not vequal(tv, Vct.new(10, 0.8)))
    snd_display("no amp env track1 mix1: %s %s?", tv, mv)
  end
  color = make_color_with_catch(0, 1, 0)
  set_track_color(track1, color)
  if (res = track_color(track1)) != color
    snd_display("track color gree: %s?", res)
  end
  unless track_states_match?(track1, [2.0, 1.0, 0, nil, color])
    snd_display("track 1 states 9 (color): %s?", track_state2list(track1))
  end
  free_track(track2)
  snd_display("free_track track?") if track?(track2)
  snd_display("free_track tracks: %s in %s?", track2, tracks) if tracks.member?(track2)
  revert_sound(ind)
  # 
  unless track_states_match?(track1, [1.0, 1.0, 0, nil, false])
    snd_display("track states 12 after revert: %s?", track_state2list(track1))
  end
  snd_display("revert2tracks: %s %s?", track1, tracks) unless tracks.member?(track1)
  if (res = track(track1)) != nil
    snd_display("revert past mix track1: %s %s %s?", res, mix1, mix?(mix1))
  end
  if (res = snd_catch do track2vct(track1) end).first != :no_such_channel
    snd_display("track2vct empty track: %s?", res.inspect)
  end
  mix1 = mix_vct(Vct.new(1, 0.1), 50)
  set_mix_track(mix1, track1)
  if (res = track_position(track1)) != 50
    snd_display("track 1 position mix-samp: %s?", res)
  end
  if (res = track_frames(track1)) != 1
    snd_display("track 1 frames mix-samp: %s?", res)
  end
  start_state = track_state2list(track1)
  set_track_amp_env(track1, [0, 1, 1, 0])
  undo_edit
  unless track_states_match?(track1, start_state)
    snd_display("undo after set_track_amp_env: %s %s?", start_state, track_state2list(track1))
  end
  redo_edit
  unless vequal(res = track_amp_env(track1), [0, 1, 1, 0].to_vct)
    snd_display("redo set_track_amp_env: %s?", res)
  end
  set_track_amp_env(track1, false)
  unless track_states_match?(track1, start_state)
    snd_display("redo/undo after set_track_amp_env: %s %s?", start_state, track_state2list(track1))
  end
  #
  edpos = edit_position(ind, 0)
  state = track_state2list(track1)
  as_one_edit_rb do
    set_track_amp(track1, 4.0)
    set_track_speed(track1, 1.5)
    set_track_amp_env(track1, [0, 1, 1, 0])
  end
  if (res = edit_position(ind, 0)) - 1 != edpos
    snd_display("backup in as_one_edit: %s %s?", edpos, res)
  end
  unless track_states_match?(track1, [4.0, 1.5, 0, [0, 1, 1, 0], false])
    snd_display("track states after as_one_edit: %s?", track_state2list(track1))
  end
  undo_edit
  unless track_states_match?(track1, state)
    snd_display("track states after undone as_one_edit: %s %s?", state, track_state2list(track1))
  end
  redo_edit
  unless track_states_match?(track1, [4.0, 1.5, 0, [0, 1, 1, 0], false])
    snd_display("track states after as_one_edit redo: %s?", track_state2list(track1))
  end
  set_track_amp_env(track1, false)
  unless track_states_match?(track1, [4.0, 1.5, 0, nil, false])
    snd_display("track states after amp_env false: %s?", track_state2list(track1))
  end
  undo_edit
  revert_sound(ind)
  #
  # multi mix tracks
  #
  mix1 = mix_vct(Vct.new(100, 0.1), 50)
  mix2 = mix_vct(Vct.new(100, 0.2), 250)
  set_mix_track(mix1, track1)
  set_mix_track(mix2, track1)
  if (res = track_chans(track1)) != 1
    snd_display("track_chans mono mix: %s?", res)
  end
  if (res = track_position(track1)) != 50
    snd_display("track_position mix2/3: %s?", res)
  end
  if (res = track_frames(track1)) != 300
    snd_display("track_frames mix2/3: %s?", res)
  end
  if (res = track(track1)) != [mix1, mix2]
    snd_display("track1 mix2/3 track: %s %s %s?", mix1, mix2, res)
  end
  unless track_states_match?(track1, [1.0, 1.0, 0, nil, false])
    snd_display("track states 1 mix2/3: %s?", track_state2list(track1))
  end
  if fneq(res = maxamp, 0.2)
    snd_display("track1 mix2/3 maxamp: %f?", res)
  end
  edpos = edit_position(ind, 0)
  set_track_amp(track1, 2.0)
  if fneq(res = maxamp, 0.4)
    snd_display("track1 mix2/3 *2 maxamp: %f?", res)
  end
  if fneq(sample(51), 0.2) or fneq(sample(251), 0.4)
    snd_display("track1 mix2/3 *2 samples: %f %f?", sample(51), sample(251))
  end
  if fneq(res1 = mix_amp(mix1), 1.0) or fneq(res2 = mix_amp(mix2), 1.0)
    snd_display("track1 mix2/3 mix amps: %f %f?", res1, res2)
  end
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("track amp set was not atomic: %s %s?", edpos, res)
  end
  undo_edit
  set_track_position(track1, 100)
  if (res = mix_position(mix1)) != 100
    snd_display("set_track_position 100 mix1: %s?", res)
  end
  if (res = mix_position(mix2)) != 300
    snd_display("set_track_position 100 mix2: %s?", res)
  end
  if (res = track_position(track1)) != 100
    snd_display("set_track_position 100 track1: %s?", res)
  end
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("track position set was not atomic: %s %s?", edpos, res)
  end
  #
  if fneq( sample( 51), 0.0) or
      fneq(sample( 99), 0.0) or
      fneq(sample(251), 0.0) or
      fneq(sample(299), 0.0)
    snd_display("set_track_position, bad cancel?: %f %f %f %f?",
                sample(51), sample(99), sample(251), sample(299))
  end
  if fneq( sample(100), 0.1) or
      fneq(sample(199), 0.1) or
      fneq(sample(301), 0.2) or
      fneq(sample(399), 0.2)
    snd_display("set_track_position, bad remix after cancel?: %f %f %f %f?",
                sample(100), sample(199), sample(301), sample(399))
  end
  undo_edit
  set_track_speed(track1, 0.5)
  if (res1 = mix_frames(mix1)) != 200 or (res2 = mix_frames(mix2)) != 200
    snd_display("set_track_speed mix_frames: %s %s %s?", res1, res2, track_frames(track1))
  end
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("track speed set was not atomic: %s %s?", edpos, res)
  end
  unless track_states_match?(track1, [1.0, 0.5, 0, nil, false])
    snd_display("track states speed set mix2/3: %s?", track_state2list(track1))
  end
  undo_edit
  old_track_color = track_color(track1)
  old_mix1_color = mix_color(mix1)
  old_mix2_color = mix_color(mix2)
  color = make_color_with_catch(0, 1, 1)
  set_track_color(track1, color)
  if (res = edit_position(ind, 0)) != edpos
    snd_display("track color set was an edit?: %s %s?", edpos, res)
  end
  unless track_states_match?(track1, [1.0, 1.0, 0, nil, color])
    snd_display("track states color set mix2/3: %s?", track_state2list(track1))
  end
  if (res1 = mix_color(mix1)) != color or
      (res2 = mix_color(mix2)) != color or
      (res3 = track_color(track1)) != color
    snd_display("set_track_color mix2/3: %s %s %s?", res1, res2, res3)
  end
  set_track_amp_env(track1, [0, 0, 0.5, 0, 0.51, 1, 1, 1])
  unless vequal(res = track_amp_env(track1), [0, 0, 0.5, 0, 0.51, 1, 1, 1].to_vct)
    snd_display("set_track_amp_env mix2/3: %s?", res)
  end
  if fneq( sample( 51), 0.0) or
      fneq(sample( 99), 0.0) or
      fneq(sample(251), 0.2) or
      fneq(sample(299), 0.2)
    snd_display("set_track_position, bad remix after cancel?: %f %f %f %f?",
                sample(51), sample(99), sample(251), sample(299))
  end
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("track amp env set was not atomic: %s %s?", edpos, res)
  end
  unless track_states_match?(track1, [1.0, 1.0, 0, [0, 0, 0.5, 0, 0.51, 1, 1, 1], color])
    snd_display("track states amp_env set mix2/3: %s?", track_state2list(track1))
  end
  if (res = track_color(track1)) != color
    snd_display("track_color not stacked: %s %s?", color, res)
  end
  set_track_color(track1, old_track_color)
  edpos = edit_position(ind, 0)
  track2a = make_track(mix1, mix2)
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("make_track not atomic: %s %s %s?", edpos, res, track(track2a))
  end
  if (res1 = mix_track(mix1)) != track2a and (res2 = mix_track(mix2)) != track2a
    snd_display("make_track didn\'t reset track: %s %s %s?", res1, res2, track2a)
  end
  if (res = track(track1)) != nil
    snd_display("make_track didn\'t cancel old track: %s?", res)
  end
  edpos = edit_position(ind, 0)
  delete_track(track2a)
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("delete_track not atomic: %s %s?", edpos, res)
  end
  if (not (res1 = mix_locked?(mix1))) or (not (res2 = mix_locked?(mix1)))
    snd_display("delete_track didn\'t lock mixes: %s %s?", res1, res2)
  end
  if fneq(res = track_amp(track2a), 0.0)
    snd_display("delete_track track amp: %f?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("delete_track maxamp: %f?", res)
  end
  # :no_such_mix here has no meaning
  snd_catch(:no_such_mix) do set_mix_track(mix1, 0) end
  snd_display("locked mix set track: %s?", mix_track(mix1)) if mix_track(mix1).zero?
  old_amp = mix_amp(mix1, 0)
  snd_catch(:no_such_mix) do set_mix_amp(mix1, 0, 123.0) end
  snd_display("locked mix set amp: %f?", mix_amp(mix1)) if fneq(mix_amp(mix1, 0), old_amp)
  old_speed = mix_speed(mix1)
  snd_catch(:no_such_mix) do set_mix_speed(mix1, 123.0) end
  snd_display("locked mix set speed: %f?", mix_speed(mix1)) if fneq(mix_speed(mix1), old_speed)
  old_pos = mix_position(mix1)
  snd_catch(:no_such_mix) do set_mix_position(mix1, 123) end
  snd_display("locked mix set position: %s?", mix_position(mix1)) if mix_position(mix1) != old_pos
  undo_edit
  if fneq(res = maxamp(ind, 0), 0.2)
    snd_display("undo delete_track maxamp: %f?", res)
  end
  if (res1 = mix_locked?(mix1)) or (res2 = mix_locked?(mix1))
    snd_display("undo delete_track didn\'t unlock mixes: %s %s?", res1, res2)
  end
  revert_sound(ind)
  #
  mix1 = mix_vct(Vct.new(100, 0.2), 50)
  mix2 = mix_vct(Vct.new(100, 0.2), 250)
  mix3 = mix_vct(Vct.new(100, 0.2), 500)
  track3 = make_track(mix1, mix2, mix3)
  edpos = edit_position(ind, 0)
  lock_track(track3)
  if (not (res1 = mix_locked?(mix1))) or
      (not (res2 = mix_locked?(mix2))) or
      (not (res3 = mix_locked?(mix3)))
    snd_display("lock_track: %s %s %s?", res1, res2, res3)
  end
  if (res = track(track3)) != nil
    snd_display("locked track: %s?", res)
  end
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("lock track not atomic: %s %s?", edpos, res)
  end
  close_sound(ind)
  if (res = track(track3)) != nil
    snd_display("close_sound unset track: %s %s?", res, track(track3).map do |m| mix?(m) end)
  end
end

def test059
  # stereo track
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 2, "track tests", 1000)
  mixid = mix("2a.snd", 100, true, ind)
  trk = mix_track(mixid)
  mixids = track(trk)
  if (res = mix_track(mixid)) <= 0
    snd_display("multimix 1: %s %s %s?", res, trk, mixids)
  end
  if (res = mixids.map do |m| mix_track(m) end).uniq.length != 1
    snd_display("multimix 2: %s?", res)
  end
  if (res1 = edit_position(ind, 0)) != (res2 = edit_position(ind, 1)) or res1 != 1
    snd_display("multimix 3 edpos: %s %s?", res1, res2)
  end
  if (res = track_chans(trk)) != 2
    snd_display("track_chans stereo mix: %s?", res)
  end
  # track properties
  set_track_property(:hiho, 123, trk)
  if (res = track_property(:hiho, trk)) != 123
    snd_display("track_property: %s?", res)
  end
  if res = track_property(:not_there, trk)
    snd_display("track-not-property: %s?", res)
  end
  set_track_property(:hi, "hi", trk)
  if (res = track_property(:hi, trk)) != "hi"
    snd_display("track_property 1: %s?", res)
  end
  # 2chan basic stuff
  max0 = maxamp(ind, 0)
  max1 = maxamp(ind, 1)
  set_track_amp(trk, 2.0)
  if fneq(res1 = maxamp(ind, 0), 2 * max0) or fneq(res2 = maxamp(ind, 1), 2 * max1)
    snd_display("2chn track_amp: %f %f -> %f %f?", max0, max1, res1, res2)
  end
  if (res1 = edit_position(ind, 0)) != (res2 = edit_position(ind, 1)) or res1 != 2
    snd_display("2chn amp edpos: %s %s?", res1, res2)
  end
  undo_edit(1, ind, 0)
  undo_edit(1, ind, 1)
  set_track_position(trk, 500)
  if (res = track_position(trk)) != 500
    snd_display("2chn track_position: %s?", res)
  end
  if (res = mixids.map do |m| mix_position(m) end).uniq.length != 1 or
      mix_position(mixids.first) != 500
    snd_display("2chn track_position mixes: %s?", res)
  end
  if (res1 = edit_position(ind, 0)) != (res2 = edit_position(ind, 1)) or res1 != 2
    snd_display("2chn position edpos: %s %s?", res1, res2)
  end
  undo_edit(1, ind, 0)
  undo_edit(1, ind, 1)
  old_frames = frames
  set_track_speed(trk, 0.5)
  if (res1 = frames(ind, 0)) != (res2 = frames(ind, 1)) and
      res1 != (100 + 2 * (old_frames - 100))
    snd_display("2chn speed: %s %s %s?", res1, res2, old_frames * 2)
  end
  if (res1 = edit_position(ind, 0)) != (res2 = edit_position(ind, 1)) or res1 != 2
    snd_display("2chn speed edpos: %s %s?", res1, res2)
  end
  undo_edit(1, ind, 0)
  undo_edit(1, ind, 1)
  set_track_amp_env(trk, [0, 1, 0.1, 2, 0.9, 2, 1, 0])
  if fneq(res1 = maxamp(ind, 0), 2 * max0) or fneq(res2 = maxamp(ind, 1), 2 * max1)
    snd_display("2chn track_env: %f %f -> %f %f?", max0, max1, res1, res2)
  end
  if (res1 = edit_position(ind, 0)) != (res2 = edit_position(ind, 1)) or res1 != 2
    snd_display("2chn env edpos: %s %s?", res1, res2)
  end
  undo_edit(1, ind, 0)
  undo_edit(1, ind, 1)
  close_sound(ind)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "track tests", 1000)
  track4 = make_track
  mixid = mix_vct(Vct.new(10, 0.5), 100, ind, 0, true, "snd-test", track4)
  if (res = mix_track(mixid)) != track4
    snd_display("mix_vct with track: %s?", res)
  end
  if (res = edit_position(ind, 0)) != 1
    snd_display("mix_vct w/track not atomic: %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("mix_vct+track maxamp: %f?", res)
  end
  if fneq(res = mix_maxamp(mixid), 0.5)
    snd_display("mix_maxamp: %f?", res)
  end
  if fneq(res = track_maxamp(track4, 0), 0.5)
    snd_display("track_maxamp: %f?", res)
  end
  undo_edit
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("mix_vct+track undo maxamp: %f?", res)
  end
  redo_edit
  if (res = mix_track(mixid)) != track4
    snd_display("mix_vct with track (redo): %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("mix_vct+track redo maxamp: %f?", res)
  end
  amix = mix("1a.snd", 200, 0, ind, 0, true, false, track4)
  if (res = mix_track(amix)) != track4
    snd_display("mix with track: %s?", res)
  end
  if (res = edit_position(ind, 0)) != 2
    snd_display("mix w/track not atomic: %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("mix+track maxamp: %f?", res)
  end
  undo_edit
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("mix+track undo maxamp: %f?", res)
  end
  redo_edit
  if (res = mix_track(amix)) != track4
    snd_display("mix with track (redo): %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("mix+track redo maxamp: %f?", res)
  end
  if (res = track(track4)) != [mixid, amix]
    snd_display("mix_vct+mix+track: %s?", res)
  end
  rid = make_region(100, 110, ind, 0)
  if fneq(res = region_maxamp(rid), 0.5)
    snd_display("region(mix) picked up wrong section: %f?", res)
  end
  rmix = mix_region(900, rid, ind, 0, track4)
  if (res = mix_track(rmix)) != track4
    snd_display("mix_region with track: %s?", res)
  end
  if (res = edit_position(ind, 0)) != 3
    snd_display("mix_region w/track not atomic: %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.56)
    snd_display("mix_region+track maxamp: %f?", res)
  end
  undo_edit
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("mix_region+track undo maxamp: %f?", res)
  end
  redo_edit
  if (res = mix_track(rmix)) != track4
    snd_display("mix_region with track (redo): %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.56)
    snd_display("mix_region+track redo maxamp: %f?", res)
  end
  if (res = track(track4)) != [mixid, amix, rmix]
    snd_display("mix_vct+mix_region+mix+track: %s?", res)
  end
  make_selection(400, 500, ind, 0)
  smix = mix_selection(4000, ind, 0)
  set_mix_track(smix, track4)
  if (res = mix_track(smix)) != track4
    snd_display("mix_selection with track: %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.56)
    snd_display("mix_selection+track maxamp: %f?", res)
  end
  undo_edit
  if fneq(res = maxamp(ind, 0), 0.56)
    snd_display("mix_selection+track undo maxamp: %f?", res)
  end
  redo_edit
  if (res = mix_track(smix)) != track4
    snd_display("mix_selection with track (redo): %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.56)
    snd_display("mix_selection+track redo maxamp: %f?", res)
  end
  if (res = track(track4)) != [mixid, amix, rmix, smix]
    snd_display("mix_vct+mix_selection+mix+track: %s?", res)
  end
  set_track_amp(track4, 0.5)
  if fneq(res = maxamp(ind, 0), 0.28)
    snd_display("mix_selection+track reset amp maxamp: %f?", res)
  end
  bmix = mix_vct(Vct.new(10, 0.75), 4300, ind, 0, true, "snd-test", track4)
  if (res = mix_track(bmix)) != track4
    snd_display("mix_vct with track amp: %s?", res)
  end
  if (res = edit_position(ind, 0)) != 7
    snd_display("mix_vct w/track not atomic: %s?", res)
  end
  if fneq(res = maxamp(ind, 0), 0.375)
    snd_display("mix_vct+track amp maxamp: %f?", res)
  end
  if (res = track(track4)) != [mixid, amix, rmix, smix, bmix]
    snd_display("mix_vct+mix_selection+mix+track amp: %s?", res)
  end
  delete_track(track4)
  if fneq(res = maxamp(ind, 0), 0.0)
    snd_display("maxamp delete_track4: %f?", res)
  end
  close_sound(ind)
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 2, "track tests", 1000)
  trk = make_track
  mix1 = mix_vct(Vct.new(10, 0.50), 10, ind, 0, true, "snd-test", trk)
  mix2 = mix_vct(Vct.new(10, 0.25), 20, ind, 1, true, "snd-test", trk)
  mix3 = mix_vct(Vct.new(10, 0.30), 30, ind, 0, true, "snd-test", trk)
  mix4 = mix_vct(Vct.new(10, 0.10), 40, ind, 1, true, "snd-test", trk)
  if (res = track_chans(trk)) != 2
    snd_display("track_chans 4 mix_vct: %s %s?", res, track(trk).map do |m| mix_home(m) end)
  end
  if fneq(res = maxamp(ind, 0), 0.5)
    snd_display("maxamp mix_vct 4 0: %f?", res)
  end
  if fneq(res = maxamp(ind, 1), 0.25)
    snd_display("maxamp mix_vct 4 1: %f?", res)
  end
  if (res = track_position(trk)) != 10
    snd_display("overall track_position: %s?", res)
  end
  if (res = track_frames(trk)) != 40
    snd_display("overall track_frames: %s?", res)
  end
  if (res = track_position(trk, 0)) != 10
    snd_display("chn0 track_position: %s?", res)
  end
  if (res = track_position(trk, 1)) != 20
    snd_display("chn1 track_position: %s?", res)
  end
  if (res = track_frames(trk, 0)) != 30
    snd_display("chn0 track_frames: %s?", res)
  end
  if (res = track_frames(trk, 1)) != 30
    snd_display("chn1 track_frames: %s?", res)
  end
  if (res = track(trk, 0)) != [mix1, mix3]
    snd_display("track + chan0: %s %s?", res, track(trk))
  end
  if (res = track(trk, 1)) != [mix2, mix4]
    snd_display("track + chan1: %s %s?", res, track(trk))
  end
  set_track_position(trk, 0)
  if (res = track_position(trk)) != 0
    snd_display("overall track_position 0: %s?", res)
  end
  if (res = track_frames(trk)) != 40
    snd_display("overall track_frames 0: %s?", res)
  end
  if (res = track_position(trk, 0)) != 0
    snd_display("chn0 track_position 0: %s?", res)
  end
  if (res = track_position(trk, 1)) != 10
    snd_display("chn1 track_position 0: %s?", res)
  end
  if (res = track_frames(trk, 0)) != 30
    snd_display("chn0 track_frames 0: %s?", res)
  end
  if (res = track_frames(trk, 1)) != 30
    snd_display("chn1 track_frames 0: %s?", res)
  end
  if (res = mix_position(mix3)) != 20
    snd_display("chn0 track_position mix3:  %s?", res)
  end
  if (res = mix_position(mix4)) != 30
    snd_display("chn1 track_position mix4:  %s?", res)
  end
  set_track_position(trk, 0, 20)
  if (res = track_position(trk)) != 10
    snd_display("overall track_position 1: %s?", res)
  end
  if (res = track_frames(trk)) != 40
    snd_display("overall track_frames 1: %s?", res)
  end
  if (res = track_position(trk, 0)) != 20
    snd_display("chn0 track_position 1: %s?", res)
  end
  if (res = track_position(trk, 1)) != 10
    snd_display("chn1 track_position 1: %s?", res)
  end
  if (res = track_frames(trk, 0)) != 30
    snd_display("chn0 track_frames 1: %s?", res)
  end
  if (res = track_frames(trk, 1)) != 30
    snd_display("chn1 track_frames 1: %s?", res)
  end
  if (res = mix_position(mix3)) != 40
    snd_display("chn0 track_position mix3 1:  %s?", res)
  end
  if (res = mix_position(mix4)) != 30
    snd_display("chn1 track_position mix4 1:  %s?", res)
  end
  set_track_position(trk, 1, 20)
  if (res = track_position(trk)) != 20
    snd_display("overall track_position 2: %s?", res)
  end
  if (res = track_frames(trk)) != 30
    snd_display("overall track_frames 2: %s?", res)
  end
  if (res = track_position(trk, 0)) != 20
    snd_display("chn0 track_position 2: %s?", res)
  end
  if (res = track_position(trk, 1)) != 20
    snd_display("chn1 track_position 2: %s?", res)
  end
  if (res = track_frames(trk, 0)) != 30
    snd_display("chn0 track_frames 2: %s?", res)
  end
  if (res = track_frames(trk, 1)) != 30
    snd_display("chn1 track_frames 2: %s?", res)
  end
  if (res = mix_position(mix3)) != 40
    snd_display("chn0 track_position mix3 2:  %s?", res)
  end
  if (res = mix_position(mix4)) != 40
    snd_display("chn1 track_position mix4 2:  %s?", res)
  end
  close_sound(ind)
  #
  ind0 = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "track tests", 60)
  ind1 = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "track tests", 60)
  mix_vct(Vct.new(3, 0.5), 10, ind0, 0, false)
  mix_vct(Vct.new(3, 0.2), 40, ind0, 0, false)
  mix1 = mix_vct(Vct.new(3, 0.5), 10, ind1, 0, true)
  mix2 = mix_vct(Vct.new(3, 0.2), 40, ind1, 0, true)
  trk = make_track(mix1, mix2)
  filter_sound([0.1, 0.2, 0.3, 0.3, 0.2, 0.1].to_vct, 6, ind0, 0)
  filter_track(trk, [0.1, 0.2, 0.3, 0.3, 0.2, 0.1])
  if (res = edit_position(ind0, 0)) != 3
    snd_display("filter_sound edpos: %s?", res)
  end
  if (res = edit_position(ind1, 0)) != 4
    snd_display("filter_track edpos: %s?", res)
  end
  if fneq(res1 = maxamp(ind0, 0), 0.4) or fneq(res2 = maxamp(ind1, 0), 0.4)
    snd_display("filter_track maxamps: %f %f?", res1, res2)
  end
  unless vequal(res1 = channel2vct(0, 50, ind0, 0), res2 = channel2vct(0, 50, ind1, 0))
    snd_display("filters not the same:\n# %s\n# %s?", res1, res2)
  end
  close_sound(ind0)
  close_sound(ind1)
end

def test069
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 4, "track2vct tests", 1000)
  trk = make_track
  mix1 = mix_vct(Vct.new(10, 0.50), 10, ind, 0, true, "snd-test", trk)
  mix2 = mix_vct(Vct.new(10, 0.25), 20, ind, 1, true, "snd-test", trk)
  mix3 = mix_vct(Vct.new(10, 0.30), 30, ind, 2, true, "snd-test", trk)
  mix4 = mix_vct(Vct.new(10, 0.10), 40, ind, 1, true, "snd-test", trk)
  if (res = track_chans(trk)) != 3
    snd_display("track_chans arg: %s?", res)
  end
  v = track2vct(trk, 0)
  snd_display("track2vct 0: %s?", v) unless vequal(v, Vct.new(10, 0.5))
  v = track2vct(trk, 2)
  snd_display("track2vct 2: %s?", v) unless vequal(v, Vct.new(10, 0.3))
  v = track2vct(trk, 1)
  v1 = Vct.new(30) do |i|
    if i < 10
      0.25
    elsif i > 19
      0.1
    else
      0.0
    end
  end
  snd_display("track2vct 3: %s?", v) unless vequal(v, v1)
  # 
  if (res = snd_catch do track2vct(trk, 3) end).first != :no_such_channel
    snd_display("track2vct track 3: %s", res.inspect)
  end
  if (res = snd_catch do track2vct(trk + 123) end).first != :no_such_track
    snd_display("track2vct untrack: %s", res.inspect)
  end
  save_track(trk, "test.snd")
  ind0 = open_sound("test.snd")
  snd_display("save_track chans: %s?", chans(ind0)) if chans(ind0) != 3
  snd_display("save_track frames: %s?", frames(ind0)) if frames(ind0) != 40
  unless vequal(res = maxamp(ind0, true), [0.5, 0.25, 0.3])
    snd_display("save_track maxamp: %s?", res)
  end
  v = channel2vct(0, 20, ind0, 0)
  v1 = Vct.new(20) do |i|
    if i < 10
      0.5
    else
      0.0
    end
  end
  snd_display("save_track 0: %s?", v) unless vequal(v, v1)
  if (res = snd_catch do save_track(trk, "test.snd", 3) end).first != :no_such_channel
    snd_display("save_track track 3: %s", res.inspect)
  end
  if (res = snd_catch do track2vct(trk + 123, "test.snd") end).first != :no_such_track
    snd_display("save_track untrack: %s", res.inspect)
  end
  close_sound(ind0)
  save_track(trk, "test.snd", 1)
  ind0 = open_sound("test.snd")
  snd_display("save_track chan1: %s?", chans(ind0)) if chans(ind0) != 1
  snd_display("save_track chan1 frames: %s?", frames(ind0)) if frames(ind0) != 30
  if fneq(res = maxamp(ind0, 0), 0.25)
    snd_display("save_track maxamp chan1: %f?", res)
  end
  v = channel2vct(0, 30, ind0, 0)
  v1 = Vct.new(30) do |i|
    if i < 10
      0.25
    elsif i > 19
      0.1
    else
      0.0
    end
  end
  snd_display("save_track chan1: %s?", v) unless vequal(v, v1)
  close_sound(ind0)
  close_sound(ind)
  if (res = snd_catch do save_track(trk, "test.snd") end).first != :no_such_channel
    snd_display("save_track empty track: %s", res.inspect)
  end
  if (res = snd_catch do save_track(trk, "test.snd", 1) end).first != :no_such_channel
    snd_display("save_track empty track (1): %s", res.inspect)
  end
  if (res = snd_catch do track2vct(trk) end).first != :no_such_channel
    snd_display("track2vct empty track: %s", res.inspect)
  end
  if (res = snd_catch do track2vct(trk, 1) end).first != :no_such_channel
    snd_display("track2vct empty track (1): %s", res.inspect)
  end
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "track amp_env tests", 300)
  track0 = make_track
  set_track_amp_env(track0, [0, 0, 1, 1])
  set_track_position(track0, 123)
  set_track_speed(track0, 0.5)
  set_track_speed(track0, 1.0)
  mix1 = mix_vct(Vct.new(10, 1.0), 10)
  set_mix_track(mix1, track0)
  tdata = track2vct(track0)
  mdata = mix2vct(mix1)
  snd_display("1 mix track: %s %s?", tdata, mdata) unless vequal(tdata, mdata)
  set_mix_position(mix1, 30)
  if (res = mix_position(mix1)) != 30
    snd_display("mix_pos change track_pos: %s %s?", res, track_position(track0))
  end
  close_sound(ind)
  #
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "track amp_env tests", 300)
  trk = make_track
  mix1 = mix_vct(Vct.new(100, 0.5),   0, ind, 0, true, "snd-test", trk)
  mix2 = mix_vct(Vct.new(100, 0.3), 200, ind, 0, true, "snd-test", trk)
  edpos = edit_position(ind, 0)
  data = channel2vct(0, 300, ind, 0)
  vdata = Vct.new(300) do |i|
    if i < 100
      0.5
    elsif i > 199
      0.3
    else
      0.0
    end
  end
  e = make_env(:envelope, [0, 0, 1, 1], :end, 299)
  unless vequal_err(data, vdata, 0.00001)
    snd_display("track not yet ramped:\n# %s\n# %s?", data, vdata)
  end
  set_track_amp_env(trk, [0, 0, 1, 1])
  vdata.map! do |val| val *= env(e) end
  data = channel2vct(0, 300, ind, 0)
  unless vequal_err(data, vdata, 0.002)
    snd_display("track_amp_env ramped:\n# %s\n# %s?", data, vdata)
  end
  if (res = edit_position(ind, 0)) != edpos + 1
    snd_display("track_amp_env ramped not atomic: %s %s?", edpos, res)
  end
  set_mix_amp_env(mix1, [0, 1, 1, 0])
  if (res = edit_position(ind, 0)) != edpos + 2
    snd_display("mix_amp_env ramped not atomic: %s %s?", edpos, res)
  end
  data = channel2vct(0, 300, ind, 0)
  e = make_env(:envelope, [0, 1, 1, 0], :end, 99)
  vdata.map_with_index! do |val, i|
    break if i > 99
    val *= env(e)
  end
  unless vequal_err(data, vdata, 0.001)
    snd_display("track_amp_env + mix_amp_env ramped:\n# %s\n# %s?", data, vdata)
  end
  set_mix_amp(mix1, 2.0)
  if (res = edit_position(ind, 0)) != edpos + 3
    snd_display("mix_amp ramped not atomic: %s %s?", edpos, res)
  end
  data = channel2vct(0, 300, ind, 0)
  vdata.map_with_index! do |val, i|
    break if i > 99
    val *= 2.0
  end
  unless vequal_err(data, vdata, 0.001)
    snd_display("track_amp_env + mix_amp_env + amp ramped:\n# %s\n# %s?", data, vdata)
  end
  set_track_position(trk, 100)
  if (res = edit_position(ind, 0)) != edpos + 4
    snd_display("track_position + mix_amp_env  ramped not atomic: %s %s?", edpos, res)
  end
  if (res = frames(ind, 0)) != 400
    snd_display("set_track_position extended file: %s?", res)
  end
  if (res = track_frames(trk)) != 300
    snd_display("set_track_position extended file track: %s?", res)
  end
  data = channel2vct(100, 300, ind, 0)
  unless vequal_err(data, vdata, 0.001)
    snd_display("track_amp_env + mix_amp_env + amp ramped + position:\n# %s\n# %s?", data, vdata)
  end
  #
  set_mix_amp_env(mix1, false)
  set_mix_amp(mix2, 0, 10.0 / 3);
  e = make_env(:envelope, [0, 0, 1, 1], :end, 299)
  vdata.map_with_index! do |val, i|
    if i < 100 or i > 199
      env(e)
    else
      val *= env(e)
    end
  end
  data = channel2vct(100, 300, ind, 0)
  unless vequal_err(data, vdata, 0.003)
    snd_display("track_amp_env to 1.0:\n# %s\n# %s?", data, vdata)
  end
  set_track_speed(trk, 0.5)
  if (res = track_frames(trk)) != 400
    snd_display("track frames after speed+amp-env: %s?", res)
  end
  if (res = frames(ind, 0)) != 500
    snd_display("set_track_speed extended file: %s?", res)
  end
  if fneq(sample(100), 0.0) or
      fneq_err(sample(200), 0.25, 0.0015) or
      fneq_err(sample(400), 0.75, 0.0015)
    snd_display("track-amp-env+speed0.5 samps: %f %f %f?", sample(100), sample(200), sample(400))
  end
  revert_sound(ind)
  #
  mix1 = mix_vct(Vct.new(100, 1.0),   0, ind, 0, true, "snd-test", trk)
  mix2 = mix_vct(Vct.new(100, 1.0), 100, ind, 0, true, "snd-test", trk)
  if (res = track(trk)) != [mix1, mix2]
    snd_display("unset track upon revert: %s %s?", res, [mix1, mix2])
  end
  set_track_amp_env(trk, [0, 0, 1, 1])
  if fneq( sample(  0), 0.000) or
      fneq(sample( 50), 0.252) or
      fneq(sample( 99), 0.500) or
      fneq(sample(100), 0.500) or
      fneq(sample(199), 1.000)
    snd_display("mix_speed/position track + track_amp_env: %s?",
                [0, 50, 99, 100, 199].map do |m| sample(m) end)
  end
  set_mix_speed(mix2, 0.25)
  if fneq( sample(  0), 0.000) or
      fneq(sample( 50), 0.101) or
      fneq(sample( 99), 0.200) or
      fneq(sample(100), 0.200) or
      fneq(sample(200), 0.400) or
      fneq_err(sample(300), 0.600, 0.003) or
      fneq_err(sample(400), 0.801, 0.01) or
      fneq_err(sample(450), 0.900, 0.01)
    snd_display("mix_speed lengthens track + track_amp_env: %s?",
                [0, 50, 99, 100, 200, 300, 400, 450].map do |m| sample(m) end)
  end
  undo_edit
  set_mix_speed(mix2, 2.0)
  if fneq( sample(  0), 0.000) or
      fneq(sample( 50), 0.337) or
      fneq(sample( 99), 0.667) or
      fneq(sample(151), 0.000) or
      fneq(sample(200), 0.000) or
      fneq_err(sample(110), 0.730, 0.1) or
      fneq_err(sample(135), 0.900, 0.1)
    snd_display("mix_speed lengthens track + track_amp_env: %s?",
                [0, 50, 99, 110, 135, 151, 200].map do |m| sample(m) end)
  end
  undo_edit
  set_mix_position(mix2, 400)
  if fneq( sample(  0), 0.000) or
      fneq(sample( 50), 0.101) or
      fneq(sample( 99), 0.200) or
      fneq(sample(100), 0.000) or
      fneq(sample(200), 0.000) or
      fneq(sample(300), 0.000) or
      fneq(sample(400), 0.800) or
      fneq(sample(450), 0.901)
    snd_display("mix_position lengthens track + track_amp_env: %s?",
                [0, 50, 99, 100, 200, 300, 400, 450].map do |m| sample(m) end)
  end
  undo_edit
  set_track_position(trk, 300)
  set_mix_position(mix1, 0)
  if fneq( sample(  0), 0.000) or
      fneq(sample( 50), 0.101) or
      fneq(sample( 99), 0.200) or
      fneq(sample(100), 0.000) or
      fneq(sample(200), 0.000) or
      fneq(sample(300), 0.000) or
      fneq(sample(400), 0.800) or
      fneq(sample(450), 0.901)
    snd_display("mix_position (backwards) lengthens track + track_amp_env: %s?",
                [0, 50, 99, 100, 200, 300, 400, 450].map do |m| sample(m) end)
  end
  undo_edit(2)
  set_mix_position(mix2, 50)
  if fneq( sample(  0), 0.000) or
      fneq(sample( 49), 0.330) or
      fneq(sample( 50), 0.670) or
      fneq(sample( 99), 1.330) or
      fneq(sample(100), 0.670) or
      fneq(sample(149), 1.000) or
      fneq(sample(200), 0.000)
    snd_display("mix_position 2 shortens track + track_amp_env: %s?",
                [0, 49, 50, 99, 100, 149, 200].map do |m| sample(m) end)
  end
  undo_edit
  set_mix_position(mix1, 100)
  if fneq( sample(  0), 0.000) or
      fneq(sample( 50), 0.000) or
      fneq(sample( 99), 0.000) or
      fneq(sample(100), 0.000) or
      fneq(sample(150), 1.010) or
      fneq(sample(199), 2.000) or
      fneq(sample(200), 0.000)
    snd_display("mix_position 1 shortens track + track_amp_env: %s?",
                [0, 50, 99, 100, 150, 199, 200].map do |m| sample(m) end)
  end
  undo_edit
  close_sound(ind)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 2,
                  "multi-channel track position tests", 300)
  mix1 = mix_vct(Vct.new(10, 1.0),   0, ind, 0)
  mix2 = mix_vct(Vct.new(10, 1.0), 200, ind, 1)
  trk = make_track(mix1, mix2)
  if (res = track_chans(trk)) != 2
    snd_display("2chan track_position test off to bad start: %s?", res)
  end
  if (res1 =  track_position(trk, 0)) != 0 or
      (res2 = track_position(trk, 1)) != 200 or
      (res3 = track_position(trk)) != 0 or
      (res4 = mix_position(mix1)) != 0 or
      (res5 = mix_position(mix2)) != 200
    snd_display("2chan track_position pos: %s %s %s %s %s?", res1, res2, res3, res4, res5)
  end
  set_track_position(trk, 0, 25)
  if (res1 =  track_position(trk, 0)) != 25 or
      (res2 = track_position(trk, 1)) != 200 or
      (res3 = track_position(trk)) != 25 or
      (res4 = mix_position(mix1)) != 25 or
      (res5 = mix_position(mix2)) != 200
    snd_display("2chan track_position pos 2: %s %s %s %s %s?", res1, res2, res3, res4, res5)
  end
  set_track_position(trk, 1, 100)
  if (res1 =  track_position(trk, 0)) != 25 or
      (res2 = track_position(trk, 1)) != 100 or
      (res3 = track_position(trk)) != 25 or
      (res4 = mix_position(mix1)) != 25 or
      (res5 = mix_position(mix2)) != 100
    snd_display("2chan track_position pos 3: %s %s %s %s %s?", res1, res2, res3, res4, res5)
  end
  set_track_position(trk, 1, 10)
  if (res1 =  track_position(trk, 0)) != 25 or
      (res2 = track_position(trk, 1)) != 10 or
      (res3 = track_position(trk)) != 10 or
      (res4 = mix_position(mix1)) != 25 or
      (res5 = mix_position(mix2)) != 10
    snd_display("2chan track_position pos 4: %s %s %s %s %s?", res1, res2, res3, res4, res5)
  end
  set_mix_position(mix1, 0)
  set_mix_position(mix2, 200)
  if (res1 =  track_position(trk, 0)) != 0 or
      (res2 = track_position(trk, 1)) != 200 or
      (res3 = track_position(trk)) != 0 or
      (res4 = mix_position(mix1)) != 0 or
      (res5 = mix_position(mix2)) != 200
    snd_display("2chan track_position pos 5: %s %s %s %s %s?", res1, res2, res3, res4, res5)
  end
  #
  set_track_amp_env(trk, [0, 0, 1, 1])
  if (not vequal(res1 = track2vct(trk, 0),
                 vct(0.000, 0.005, 0.011, 0.016, 0.021, 0.026, 0.032, 0.037, 0.042, 0.048))) or
      (not vequal(res2 = track2vct(trk, 1),
                  vct(0.952, 0.958, 0.963, 0.968, 0.974, 0.979, 0.984, 0.989, 0.995, 1.000)))
    snd_display("2chan track-pos amp-env: %s %s?", res1, res2)
  end
  set_track_position(trk, 0, 100)
  if (not vequal(res1 = track2vct(trk, 0),
                 vct(0.000, 0.010, 0.020, 0.030, 0.040, 0.051, 0.061, 0.071, 0.081, 0.091))) or
      (not vequal(res2 = track2vct(trk, 1),
                  vct(0.909, 0.919, 0.929, 0.939, 0.949, 0.960, 0.970, 0.980, 0.990, 1.000)))
    snd_display("2chan track-pos amp-env 2: %s %s?", res1, res2)
  end
 set_track_position(trk, 1, 100)
  if (not vequal(res1 = track2vct(trk, 0),
                 vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000))) or
      (not vequal(res2 = track2vct(trk, 1),
                  vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000)))
    snd_display("2chan track-pos amp-env 3: %s %s?", res1, res2)
  end
 set_track_position(trk, 1, 0)
  if (not vequal(res1 = track2vct(trk, 0),
                 vct(0.909, 0.919, 0.929, 0.939, 0.949, 0.960, 0.970, 0.980, 0.990, 1.000))) or
      (not vequal(res2 = track2vct(trk, 1),
                  vct(0.000, 0.010, 0.020, 0.030, 0.040, 0.051, 0.061, 0.071, 0.081, 0.091)))
    snd_display("2chan track-pos amp-env 4: %s %s?", res1, res2)
  end
  mix3 = mix_vct(Vct.new(10, 1.0), 200, ind, 1)
  set_mix_track(mix3, trk)
  if (not vequal(res = track2vct(trk, 0),
                 vct(0.476, 0.481, 0.487, 0.492, 0.497, 0.503, 0.508, 0.513, 0.519, 0.524))) or
      (not vequal(channel2vct(0, 10, ind, 1),
                 vct(0.000, 0.005, 0.011, 0.016, 0.021, 0.026, 0.032, 0.037, 0.042, 0.048))) or
      (not vequal(channel2vct(200, 10, ind, 1),
                  vct(0.952, 0.958, 0.963, 0.968, 0.974, 0.979, 0.984, 0.989, 0.995, 1.000)))
    snd_display("2chan track-pos amp-env 5: %s %s?", res, track2vct(trk, 1))
  end
  edpos = edit_position(ind, 1)
  set_track_position(trk, 0, 50)
  if (res = edit_position(ind, 1)) != edpos
    snd_display("set track pos changed edpos: %s %s?", edpos, res)
  end
  if (not vequal(res = track2vct(trk, 0),
                 vct(0.238, 0.243, 0.249, 0.254, 0.259, 0.265, 0.270, 0.275, 0.280, 0.286))) or
      (not vequal(channel2vct(0, 10, ind, 1),
                 vct(0.000, 0.005, 0.011, 0.016, 0.021, 0.026, 0.032, 0.037, 0.042, 0.048))) or
      (not vequal(channel2vct(200, 10, ind, 1),
                  vct(0.952, 0.958, 0.963, 0.968, 0.974, 0.979, 0.984, 0.989, 0.995, 1.000)))
    snd_display("2chan track-pos amp-env 6: %s %s?", res, track2vct(trk, 1))
  end
  close_sound(ind)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1,
                  "multi-channel track position tests", 300)
  mix1 = mix_vct(Vct.new(10, 1.0), 0, ind, 0)
  trk = make_track
  set_track_amp_env(trk, [0, 0, 1, 1])
  unless vequal(res = track_amp_env(trk), [0, 0, 1, 1].to_vct)
    snd_display("empty track env: %s?", res)
  end
  set_mix_track(mix1, trk)
  unless vequal(res = track2vct(trk),
                vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000))
    snd_display("track amp-env 1 mix: %s?", res)
  end
  set_track_position(trk, 10)
  unless vequal(res = channel2vct(0, 20, ind, 0),
                vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                    0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000))
    snd_display("move 1 mix track + env: %s", res)
  end
  unless vequal(res = mix2vct(mix1),
                vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000))
    snd_display("track amp-env 1 mix2vct: %s?", res)
  end
  set_mix_track(mix1, 0)
  unless vequal(res = mix2vct(mix1), Vct.new(10, 1.0)) 
    snd_display("untrack mix2vct: %s?", res)
  end
  unless vequal(res = track_amp_env(trk), [0, 0, 1, 1])
    snd_display("newly empty track env: %s?", res)
  end
  set_mix_speed(mix1, 0.5)
  set_mix_track(mix1, trk)
  if (res1 = mix_frames(mix1)) != 20 or (res2 = track_frames(trk)) != 20
    snd_display("mix_speed for track frames: %s %s?", res1, res2)
  end
  if fneq(sample(30), 0.0) or fneq(sample(10), 0.0) or fneq(sample(20), 0.526)
    snd_display("mix_speed + track amp env: %s?", track2vct(trk))
  end
  set_track_speed(trk, 2.0)
  if (res1 = mix_frames(mix1)) != 10 or (res2 = track_frames(trk)) != 10
    snd_display("mix_speed (2) for track frames: %s %s?", res1, res2)
  end
  unless vequal(res = track2vct(trk),
                vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000))
    snd_display("mix+track speed + amp-env: %s?", res)
  end
  close_sound(ind)
end

def test079
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "track + track tests", 300)
  mix1 = mix_vct(Vct.new(10, 1.0), 20, ind, 0)
  track1 = make_track
  track2 = make_track
  edpos = edit_position(ind, 0)
  # 
  # track->track+env
  # 
  set_track_amp_env(track2, [0, 0, 1, 1])
  set_track_track(track1, track2)
  set_mix_track(mix1, track1)
  if (res1 = track(track1)) != (res2 = track(track2)) or res1 != [mix1]
    snd_display("embedded track: %s %s %s?", res1, res2, [mix1])
  end
  if (res1 = mix_position(mix1)) != (res2 = track_position(track1)) or
      (res3 = track_position(track2)) != res1 or
      res1 != 20
    snd_display("embedded track pos: %s %s %s?", res1, res2, res3)
  end
  if (res1 = mix_frames(mix1)) != (res2 = track_frames(track1)) or
      (res3 = track_frames(track2)) != res1 or
      res1 != 10
    snd_display("embedded track dur: %s %s %s?", res1, res2, res3)
  end
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000)))
    snd_display("embedded track amp-env(t->e): %s %s %s?", res1, res2, res3)
  end
  edpos = edit_position(ind, 0)
  set_track_position(track1, 50)
  if (res1 = mix_position(mix1)) != (res2 = track_position(track1)) or
      (res3 = track_position(track2)) != res1 or
      res1 != 50
    snd_display("embedded track set pos: %s %s %s?", res1, res2, res3)
  end
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000)))
    snd_display("embedded track amp-env(t->e) set pos: %s %s %s?", res1, res2, res3)
  end
  set_edit_position(edpos, ind, 0)
  set_track_position(track2, 50)
  if (res1 = mix_position(mix1)) != (res2 = track_position(track1)) or
      (res3 = track_position(track2)) != res1 or
      res1 != 50
    snd_display("embedded track set pos 1: %s %s %s?", res1, res2, res3)
  end
  set_edit_position(edpos, ind, 0)
  set_mix_position(mix1, 50)
  if (res1 = mix_position(mix1)) != (res2 = track_position(track1)) or
      (res3 = track_position(track2)) != res1 or
      res1 != 50
    snd_display("embedded track set pos 2: %s %s %s?", res1, res2, res3)
  end
  set_edit_position(edpos, ind, 0)
  set_track_speed(track2, 0.5)
  if (res1 = mix_position(mix1)) != (res2 = track_position(track1)) or
      (res3 = track_position(track2)) != res1 or
      res1 != 20
    snd_display("embedded track set speed: %s %s %s?", res1, res2, res3)
  end
  set_edit_position(edpos, ind, 0)
  set_track_speed(track1, 0.5)
  if (res1 = mix_position(mix1)) != (res2 = track_position(track1)) or
      (res3 = track_position(track2)) != res1 or
      res1 != 20
    snd_display("embedded track set speed 1: %s %s %s?", res1, res2, res3)
  end
  set_edit_position(edpos, ind, 0)
  set_mix_speed(mix1, 0.5)
  if (res1 = mix_position(mix1)) != (res2 = track_position(track1)) or
      (res3 = track_position(track2)) != res1 or
      res1 != 20
    snd_display("embedded track set speed 2: %s %s %s?", res1, res2, res3)
  end
  set_edit_position(edpos, ind, 0)
  if (res1 = mix_frames(mix1)) != (res2 = track_frames(track1)) or
      (res3 = track_frames(track2)) != res1 or
      res1 != 10
    snd_display("embedded track undo set speed: %s %s %s?", res1, res2, res3)
  end
  set_track_amp(track2, 0.5)
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0.000, 0.111, 0.222, 0.333, 0.444,
                            0.556, 0.667, 0.778, 0.889, 1.000).scale(0.5)))
    snd_display("embedded track set amp(t->e): %s %s %s?", res1, res2, res3)
  end
  undo_edit
  set_track_amp(track1, 0.5)
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0.000, 0.111, 0.222, 0.333, 0.444,
                            0.556, 0.667, 0.778, 0.889, 1.000).scale(0.5)))
    snd_display("embedded track set amp(t->e) 1: %s %s %s?", res1, res2, res3)
  end
  undo_edit
  set_mix_amp(mix1, 0.5)
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0.000, 0.111, 0.222, 0.333, 0.444,
                            0.556, 0.667, 0.778, 0.889, 1.000).scale(0.5)))
    snd_display("embedded track set amp(t->e) 2: %s %s %s?", res1, res2, res3)
  end
  undo_edit
  # 
  set_track_amp_env(track1, [0, 0, 1, 1])
  set_track_amp_env(track1, false)
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000)))
    snd_display("embedded track amp-env(t->e) 1: %s %s %s?", res1, res2, res3)
  end
  set_mix_amp_env(mix1, 0, [0, 0, 1, 1])
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0, 0.010, 0.040, 0.090, 0.160, 0.250, 0.360, 0.490, 0.640, 0.810, 1)))
    snd_display("embedded track amp-env(t->e) 2: %s %s %s?", res1, res2, res3)
  end
  set_track_amp_env(track1, [0, 0, 1, 1])
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0, 0.001, 0.008, 0.027, 0.064, 0.125, 0.216, 0.343, 0.512, 0.729, 1)))
    snd_display("embedded track amp-env(t->e) 3: %s %s %s?", res1, res2, res3)
  end
  set_mix_amp_env(mix1, 0, false)
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0, 0.010, 0.040, 0.090, 0.160, 0.250, 0.360, 0.490, 0.640, 0.810, 1)))
    snd_display("embedded track amp-env(t->e) 4: %s %s %s?", res1, res2, res3)
  end
  set_track_amp_env(track1, false)
  if (not vequal(res1 = mix2vct(mix1), res2 = track2vct(track1))) or
      (not vequal(res1, res3 = track2vct(track2))) or
      (not vequal(res3, vct(0.000, 0.111, 0.222, 0.333, 0.444, 0.556, 0.667, 0.778, 0.889, 1.000)))
    snd_display("embedded track amp-env(t->e) 1: %s %s %s?", res1, res2, res3)
  end
  if (res1 = mix_frames(mix1)) != (res2 = track_frames(track1)) or
      (res3 = track_frames(track2)) != res1 or
      res1 != 10
    snd_display("embedded track dur back out: %s %s %s?", res1, res2, res3)
  end
  # 
  # two mixes track(1)+track-amp-env
  # 
  mix2 = mix_vct(Vct.new(10, 1.0), 30, ind, 0)
  set_mix_track(mix2, track1)
  if (not vequal(track2vct(track1), track2vct(track2))) or
      (not vequal(res = channel2vct(20, 20, ind, 0),
                  vct(0.000, 0.056, 0.111, 0.167, 0.222, 0.278, 0.333, 0.389, 0.444, 0.500,
                      0.500, 0.556, 0.611, 0.667, 0.722, 0.778, 0.833, 0.889, 0.944, 1.000)))
    snd_display("embedded track 2mix ampenv: %s?", res)
  end
  set_mix_track(mix2, track2)
  if (not vequal(track2vct(track1), track2vct(track2))) or
      (not vequal(res = channel2vct(20, 20, ind, 0),
                  vct(0.000, 0.056, 0.111, 0.167, 0.222, 0.278, 0.333, 0.389, 0.444, 0.500,
                      0.500, 0.556, 0.611, 0.667, 0.722, 0.778, 0.833, 0.889, 0.944, 1.000)))
    snd_display("embedded track 2mix ampenv: %s?", res)
  end
  set_mix_track(mix2, track1)
  if (res1 = track(track1)) != (res2 = track(track2)) or res1 != [mix1, mix2]
    snd_display("embedded track 2mix: %s %s %s?", res1, res2, [mix1, mix2])
  end
  if (res1 = track_position(track1)) != (res2 = track_position(track2)) or res1 != 20
    snd_display("embedded track pos 2mix: %s %s?", res1, res2)
  end
  if (res1 = track_frames(track1)) != (res2 = track_frames(track2)) or res1 != 20
    snd_display("embedded track dur 2mix: %s %s?", res1, res2)
  end
  set_track_position(track1, 50)
  if (res1 = track_position(track1)) != (res2 = track_position(track2)) or
      (res3 = mix_position(mix1)) != res1 or
      res1 != 50
    snd_display("embedded track set pos 2mix: %s %s?", res1, res2, res3)
  end
  if (not vequal(track2vct(track1), track2vct(track2))) or
      (not vequal(res = channel2vct(50, 20, ind, 0),
                  vct(0.000, 0.056, 0.111, 0.167, 0.222, 0.278, 0.333, 0.389, 0.444, 0.500,
                      0.500, 0.556, 0.611, 0.667, 0.722, 0.778, 0.833, 0.889, 0.944, 1.000)))
    snd_display("embedded track amp-env(t->e) set pos 2mix: %s?", res)
  end
  undo_edit
  set_track_position(track2, 50)
  if (res1 = track_position(track1)) != (res2 = track_position(track2)) or
      (res3 = mix_position(mix1)) != res1 or
      res1 != 50
    snd_display("embedded track set pos 2mix 1: %s %s?", res1, res2, res3)
  end
  undo_edit
  set_track_speed(track1, 0.5)
  if (res1 = track_frames(track1)) != (res2 = track_frames(track2)) or res1 != 30
    snd_display("embedded track set speed 2mix: %s %s?", res1, res2)
  end
  undo_edit
  set_track_speed(track2, 0.5)
  if (res1 = track_frames(track1)) != (res2 = track_frames(track2)) or res1 != 30
    snd_display("embedded track set speed 2mix 1: %s %s?", res1, res2)
  end
  undo_edit
  set_track_amp(track1, 0.5)
  if (not vequal(track2vct(track1), track2vct(track2))) or
      (not vequal(res = channel2vct(20, 20, ind, 0),
                  vct(0.000, 0.056, 0.111, 0.167, 0.222, 0.278, 0.333, 0.389, 0.444, 0.500,
                      0.500, 0.556, 0.611, 0.667, 0.722,
                      0.778, 0.833, 0.889, 0.944, 1.000).scale(0.5)))
    snd_display("embedded track amp(t->e) set pos 2mix 2: %s?", res)
  end
  undo_edit
  set_track_amp(track2, 0.5)
  if (not vequal(track2vct(track1), track2vct(track2))) or
      (not vequal(res = channel2vct(20, 20, ind, 0),
                  vct(0.000, 0.056, 0.111, 0.167, 0.222, 0.278, 0.333, 0.389, 0.444, 0.500,
                      0.500, 0.556, 0.611, 0.667, 0.722,
                      0.778, 0.833, 0.889, 0.944, 1.000).scale(0.5)))
    snd_display("embedded track amp(t->e) set pos 2mix 3: %s?", res)
  end
  undo_edit
  set_mix_amp(mix1, 0.5)
  set_mix_amp(mix2, 0.5)
  if (not vequal(track2vct(track1), track2vct(track2))) or
      (not vequal(res = channel2vct(20, 20, ind, 0),
                  vct(0.000, 0.056, 0.111, 0.167, 0.222, 0.278, 0.333, 0.389, 0.444, 0.500,
                      0.500, 0.556, 0.611, 0.667, 0.722,
                      0.778, 0.833, 0.889, 0.944, 1.000).scale(0.5)))
    snd_display("embedded track amp(t->e) set pos 2mix 4: %s?", res)
  end
  undo_edit(2)
  set_track_amp_env(track1, [0, 0, 1, 1])
  set_track_amp_env(track2, false)
  if (not vequal(track2vct(track1), track2vct(track2))) or
      (not vequal(res = channel2vct(20, 20, ind, 0),
                  vct(0.000, 0.056, 0.111, 0.167, 0.222, 0.278, 0.333, 0.389, 0.444, 0.500,
                      0.500, 0.556, 0.611, 0.667, 0.722, 0.778, 0.833, 0.889, 0.944, 1.000)))
    snd_display("embedded track amp-env(t->e) 2mix 1: %s?", res)
  end
  set_track_amp_env(track2, [0, 0, 1, 1])
  if (not vequal(track2vct(track1), track2vct(track2))) or
      (not vequal(res = channel2vct(20, 20, ind, 0),
                  vct(0.000, 0.002, 0.010, 0.022, 0.040, 0.062, 0.090, 0.122, 0.160, 0.202,
                      0.250, 0.303, 0.360, 0.422, 0.490, 0.562, 0.640, 0.722, 0.810, 0.903)))
    snd_display("embedded track amp-env(t->e) 2mix 2: %s?", res)
  end
  #
  edpos = edit_position(ind, 0)
  track3 = make_track(mix1, mix2)
  if (res1 = track(track1)) != nil or
      (res2 = track(track2)) != nil or
      (res3 = track(track3)) != [mix1, mix2]
    snd_display("make_track overrides: %s %s %s?", res1, res2, res3)
  end
  unless vequal(res = channel2vct(20, 20, ind, 0), Vct.new(20, 1.0))
    snd_display("make_track overrides vals: %s?", res)
  end
  set_edit_position(edpos, ind, 0)
  track4 = make_track(mix2)
  if (res1 = track(track1)) != [mix1] or
      (res2 = track(track2)) != [mix1] or
      (res3 = track(track3)) != nil or
      (res4 = track(track4)) != [mix2]
    snd_display("make_track again overrides: %s %s %s %s?", res1, res2, res3, res4)
  end
  unless vequal(res = channel2vct(20, 20, ind, 0),
                vct(0.000, 0.010, 0.040, 0.090, 0.160, 0.250, 0.360, 0.490, 0.640, 0.810,
                    1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000))
    snd_display("make_track again overrides vals: %s?", res)
  end
  delete_file("s61.rb")
  save_state("s61.rb")
  close_sound(ind)
  # 
  load("s61.rb")
  ind = find_sound("test.snd")
  snd_display("can\'t restore test.snd: %s?", sounds) unless sound?(ind)
  unless vequal(res = channel2vct(20, 20, ind, 0),
                vct(0.000, 0.010, 0.040, 0.090, 0.160, 0.250, 0.360, 0.490, 0.640, 0.810,
                    1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000))
    snd_display("track save/restore: %s?", res)
  end
  close_sound(ind) if sound?(ind)
  #
  # track-tempo tests
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bfloat, 22050, 1, "track tests", 1000)
  trk = make_track
  initial_edpos = edit_position(ind, 0)
  if fneq(res = track_tempo(trk), 1.0)
    snd_display("track_tempo: %f?", res)
  end
  set_track_tempo(trk, 0.5)
  if fneq(res = track_tempo(trk), 0.5)
    snd_display("set_track_tempo: %f?", res)
  end
  if (res = edit_position(ind, 0)) != initial_edpos
    snd_display("no-op set_track_tempo edits: %s %s?", initial_edpos, res)
  end
  set_track_tempo(trk, 1.0)
  mix0 = mix_vct(Vct.new(10, 0.1), 100)
  set_mix_track(mix0, trk)
  if (res = mix_position(mix0)) != 100
    snd_display("track tempo initial mix pos: %s?", res)
  end
  set_track_tempo(trk, 0.5)
  if (res = mix_position(mix0)) != 100
    snd_display("track tempo mix pos: %s?", res)
  end
  set_track_tempo(trk, 1.0)
  mix1 = mix_vct(Vct.new(10, 0.3), 300)
  set_mix_track(mix1, trk)
  if (res = mix_position(mix0)) != 100
    snd_display("track (2) tempo initial mix0 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 300
    snd_display("track (2) tempo initial mix1 pos: %s?", res)
  end
  edpos1 = edit_position(ind, 0)
  set_track_tempo(trk, 0.5)
  if (res = mix_position(mix0)) != 100
    snd_display("track tempo (2) mix0 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 500
    snd_display("track tempo (2) mix1 pos: %s?", res)
  end
  if (res = edit_position(ind, 0)) != edpos1 + 1
    snd_display("track tempo not atomic: %s %s?", edpos1, res)
  end
  set_track_tempo(trk, 1.0)
  if (res = mix_position(mix0)) != 100
    snd_display("track (2) tempo back mix0 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 300
    snd_display("track (2) tempo back mix1 pos: %s?", res)
  end
  set_track_tempo(trk, 2.0)
  if (res = mix_position(mix0)) != 100
    snd_display("track tempo (2) mix0 2 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 200
    snd_display("track tempo (2) mix1 2 pos: %s?", res)
  end
  set_track_tempo(trk, 1.0)
  mix2 = mix_vct(Vct.new(10, 0.4), 400)
  set_mix_track(mix2, trk)
  if (res = mix_position(mix0)) != 100
    snd_display("track (3) tempo initial mix0 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 300
    snd_display("track (3) tempo initial mix1 pos: %s?", res)
  end
  if (res = mix_position(mix2)) != 400
    snd_display("track (3) tempo initial mix2 pos: %s?", res)
  end
  set_track_tempo(trk, 0.5)
  if (res = mix_position(mix0)) != 100
    snd_display("track tempo (3) mix0 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 500
    snd_display("track tempo (3) mix1 pos: %s?", res)
  end
  if (res = mix_position(mix2)) != 700
    snd_display("track tempo (3) mix2 pos: %s?", res)
  end
  set_track_tempo(trk, 1.0)
  if (res = mix_position(mix0)) != 100
    snd_display("track (3) tempo back mix0 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 300
    snd_display("track (3) tempo back mix1 pos: %s?", res)
  end
  if (res = mix_position(mix2)) != 400
    snd_display("track (3) tempo back mix2 pos: %s?", res)
  end
  set_track_tempo(trk, 2.0)
  if (res = mix_position(mix0)) != 100
    snd_display("track tempo (3) mix0 2 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 200
    snd_display("track tempo (3) mix1 2 pos: %s?", res)
  end
  if (res = mix_position(mix2)) != 250
    snd_display("track tempo (3) mix2 2 pos: %s?", res)
  end
  #
  set_track_amp_env(trk, [0, 0, 1, 1])
  set_track_tempo(trk, 1.0)
  if (res = mix_position(mix0)) != 100
    snd_display("track (4) tempo mix0 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 300
    snd_display("track (4) tempo mix1 pos: %s?", res)
  end
  if (res = mix_position(mix2)) != 400
    snd_display("track (4) tempo mix2 pos: %s?", res)
  end
  set_track_amp_env(trk, [0, 1, 1, 0])
  set_track_tempo(trk, 2.0)
  if (res = mix_position(mix0)) != 100
    snd_display("track tempo (4) mix0 2 pos: %s?", res)
  end
  if (res = mix_position(mix1)) != 200
    snd_display("track tempo (4) mix1 2 pos: %s?", res)
  end
  if (res = mix_position(mix2)) != 250
    snd_display("track tempo (4) mix2 2 pos: %s?", res)
  end
  close_sound(ind)
end

def test089
  #
  # pan-mix tests
  #
  ind = new_sound("fmv.snd", Mus_next, Mus_bfloat, 22050, 1, "pan_mix tests")
  id0 = pan_mix("1a.snd")
  if (res1 = mix_track(id0)) != 0 or
      fneq(res2 = mix_amp(id0, 0), 1.0) or
      (res3 = mix_amp_env(id0, 0)) != nil
    snd_display("pan_mix 1->1 all opt: %s %s %s?", res1, res2, res3)
  end
  if fneq(res1 = maxamp(ind, 0), res2 = mix_maxamp(id0))
    snd_display("pan_mix 1->1 maxamps: %s %s?", res1, res2)
  end
  if (res = mix_position(id0)) != 0
    snd_display("pan_mix 1->1 pos: %s?", res)
  end
  if (res = mix_chans(id0)) != 1
    snd_display("pan_mix 1->1 mix_chans: %s?", res)
  end
  revert_sound(ind)
  #
  id0 = pan_mix("1a.snd", 10000, [0, 0, 1, 1])
  if (res1 = mix_track(id0)) != 0 or
      fneq(res2 = mix_amp(id0, 0), 1.0) or
      (res3 = mix_amp_env(id0, 0)) != [0, 0, 1, 1]
    snd_display("pan_mix 1->1 2: %s %s %s?", res1, res2, res3)
  end
  if (res = mix_position(id0)) != 10000
    snd_display("pan_mix 1->1 pos 2: %s?", res)
  end
  revert_sound(ind)
  # 
  id0 = pan_mix("1a.snd", 80000, [0, 0, 1, 1])
  if (res1 = mix_track(id0)) != 0 or
      fneq(res2 = mix_amp(id0, 0), 1.0) or
      (res3 = mix_amp_env(id0, 0)) != [0, 0, 1, 1]
    snd_display("pan_mix 1->1 3: %s %s %s?", res1, res2, res3)
  end
  if (res = mix_position(id0)) != 80000
    snd_display("pan_mix 1->1 pos 4: %s?", res)
  end
  if (res = frames(ind, 0)) != 80000 + mus_sound_frames("1a.snd")
    snd_display("pan_mix past end frames: %s?", res)
  end
  revert_sound(ind)
  # 
  id0 = pan_mix("2a.snd", 100)
  if (res1 = mix_track(id0)) == 0 or
      (not (res2 = mix?(id0 + 1))) or
      (res3 = mix_track(id0 + 1)) != res1
    snd_display("pan_mix 2->1: %s %s %s?", res1, res2, res3)
  end
  if (res = mix_chans(id0)) != 2
    snd_display("pan_mix 2->1 mix_chans: %s", res)
  end
  if (res1 = mix_position(id0)) != (res2 = mix_position(id0 + 1)) or
      (res3 = track_position(mix_track(id0))) != res1 or
      res1 != 100
    snd_display("pan_mix 2->1 pos: %s %s %s?", res1, res2, res3)
  end
  if fneq(res1 = mix_maxamp(id0), res2 = maxamp(ind, 0)) or
      fneq(res3 = mix_maxamp(id0 + 1), 0.0)
    snd_display("pan_mix 2->1 maxamps: %f %f %f?", res1, res2, res3)
  end
  if (res = track(mix_track(id0))) != [id0, id0 + 1]
    snd_display("pan_mix 2->1 track: %s %s?", res, id0)
  end
  max1 = maxamp(ind, 0)
  maxid0 = mix_maxamp(id0)
  maxid1 = mix_maxamp(id0 + 1)
  set_track_amp_env(mix_track(id0), [0, 0, 0, 0])
  if fneq(res1 = mix_maxamp(id0), 0.0) or fneq(res2 = mix_maxamp(id0 + 1), max1)
    snd_display("pan_mix 2->1 maxamps (reversed): %s %s %s?", res1, res2, maxamp(ind, 0))
  end
  revert_sound(ind)
  #
  maxs = mus_sound_maxamp("2a.snd")
  id0 = pan_mix("2a.snd", 100, 0.4)
  expected_max = [0.4 * maxs[1], 0.6 * maxs[3]].max
  if fneq(res = maxamp(ind, 0), expected_max)
    snd_display("pan_mix scaled: %s %s?", res, maxs)
  end
  if (res1 = mix_position(id0)) != (res2 = mix_position(id0 + 1)) or
      (res3 = track_position(mix_track(id0))) != res1 or
      res1 != 100
    snd_display("pan_mix 2->1 pos 2: %s %s %s?", res1, res2, res3)
  end
  if fneq(res1 = mix_amp(id0, 0), 1.0) or fneq(res2 = mix_amp(id0 + 1, 1), 1.0)
    snd_display("pan_mix 2->1 mix_amp: %f %f?", res1, res2)
  end
  if fneq(res1 = mix_amp(id0, 1), 0.0) or fneq(res2 = mix_amp(id0 + 1, 0), 0.0)
    snd_display("pan_mix 2->1 mix_amp (off case): %f %f?", res1, res2)
  end
  unless vequal(res = track_amp_env(mix_track(id0)), [0, 0.4, 1, 0.4])
    snd_display("pan_mix 2->1 0.4 env: %s?", res)
  end
  revert_sound(ind)
  #
  id0 = pan_mix("2a.snd", 100, [0, 0, 1, 1])
  if (res1 = mix_position(id0)) != (res2 = mix_position(id0 + 1)) or
      (res3 = track_position(mix_track(id0))) != res1 or
      res1 != 100
    snd_display("pan_mix 2->1 pos 3: %s %s %s?", res1, res2, res3)
  end
  if fneq(res1 = mix_amp(id0, 0), 1.0) or fneq(res2 = mix_amp(id0 + 1, 1), 1.0)
    snd_display("pan_mix 2->1 mix_amp 3: %f %f?", res1, res2)
  end
  if fneq(res1 = mix_amp(id0, 1), 0.0) or fneq(res2 = mix_amp(id0 + 1, 0), 0.0)
    snd_display("pan_mix 2->1 mix_amp (off case) 3: %f %f?", res1, res2)
  end
  unless vequal(res = track_amp_env(mix_track(id0)), [0, 0, 1, 1])
    snd_display("pan_mix 2->1 ramp env: %s?", res)
  end
  if fneq(res1 = maxamp(ind, 0), 0.0372) or
      fneq(res1, [res2 = mix_maxamp(id0), res3 = mix_maxamp(id0 + 1)].max)
    snd_display("pan_mix 2->1 ramp maxamp: %s %s %s?", res1, res2, res3)
  end
  revert_sound(ind)
  #
  mus_sound_forget("4.aiff")
  id0 = pan_mix("4.aiff", 100)
  if (res1 = mix_position(id0)) != (res2 = mix_position(id0 + 1)) or
      (res3 = track_position(mix_track(id0))) != res1 or
      res1 != 100
    snd_display("pan_mix 4->1 pos 4: %s %s %s?", res1, res2, res3)
  end
  if fneq(res1 = mix_amp(id0, 0), 1.0) or fneq(res2 = mix_amp(id0 + 1, 1), 1.0)
    snd_display("pan_mix 4->1 mix_amp 4: %f %f?", res1, res2)
  end
  if fneq(res1 = mix_amp(id0, 1), 0.0) or fneq(res2 = mix_amp(id0 + 1, 0), 0.0)
    snd_display("pan_mix 4->1 mix_amp (off case) 4: %f %f?", res1, res2)
  end
  unless vequal(res = track_amp_env(mix_track(id0)), [0, 1, 1, 1])
    snd_display("pan_mix 4->1 ramp env 4: %s?", res)
  end
  maxs = mus_sound_maxamp("4.aiff")
  if fneq(res = maxamp(ind, 0), maxs[1])
    snd_display("pan_mix ramp 4->1 maxamp 4: %s %s?", res, maxs)
  end
  set_track_amp_env(mix_track(id0), [0, 0, 1, 0])
  if fneq(res = maxamp(ind, 0), maxs[3])
    snd_display("pan_mix ramp 4->1 maxamp 4(2): %s %s?", res, maxs)
  end
  close_sound(ind)
end

def test099
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 2, "pan-mix tests")
  id0 = pan_mix("1a.snd")
  id1 = id0 + 1
  trk = make_track(id0)
  snd_display("pan_mix 1->2 track: %s?", trk) unless track?(trk)
  if (res = mix_chans(id0)) != 1
    snd_display("pan_mix 1->2 mix_chans: %s?", res)
  end
  if fneq(res1 = mix_amp(id0), 1.0) or fneq(res2 = mix_amp(id1), 1.0)
    snd_display("pan_mix 1->2 amps: %s %s?", res1, res2)
  end
  if fneq(res1 = maxamp(ind, 0), res2 = mix_maxamp(id0)) or fneq(res3 = maxamp(ind, 1), 0.0)
    snd_display("pan_mix 1->2 maxamps: %s %s %s?", res1, res2, res3)
  end
  if (res = track(mix_track(id0))) != [id0]
    snd_display("pan_mix 1->2 track: %s %s?", res, [id0])
  end
  set_track_amp_env(mix_track(id0), [0, 0, 1, 0])
  if fneq(res1 = maxamp(ind, 1), res2 = mix_maxamp(id1)) or fneq(res3 = maxamp(ind, 0), 0.0)
    snd_display("pan_mix 1->2 maxamps reversed: %s %s %s?", res1, res2, res3)
  end
  revert_sound(ind)
  #
  id0 = pan_mix("2a.snd", 100)
  id1 = id0 + 1
  trk = mix_track(id0)
  if (res = mix_chans(id0)) != 2
    snd_display("pan_mix 2->2 mix_chans: %s?", res)
  end
  if (res = track(trk)) != [id0, id1]
    snd_display("pan_mix 2->2 track: %s %s?", res, id0)
  end
  if fneq(res1 = mix_amp(id0, 0), 1.0) or fneq(res2 = mix_amp(id0, 1), 0.0) or
      fneq(res3 = mix_amp(id1, 0), 0.0) or fneq(res4 = mix_amp(id1, 1), 1.0)
    snd_display("pan_mix 2->2 amps: %s %s %s %s?", res1, res2, res3, res4)
  end
  if fneq(res1 = maxamp(ind, 0), res2 = mix_maxamp(id0)) or fneq(res3 = maxamp(ind, 1), 0.0)
    snd_display("pan_mix 2->2 maxamps: %s %s %s?", res1, res2, res3)
  end
  set_track_amp_env(mix_track(id0), [0, 0, 1, 0])
  if fneq(res1 = maxamp(ind, 1), res2 = mix_maxamp(id1)) or fneq(res3 = maxamp(ind, 0), 0.0)
    snd_display("pan_mix 2->2 maxamps reversed: %s %s %s?", res1, res2, res3)
  end
  revert_sound(ind)
  #
  id0 = pan_mix("2a.snd", 1000, 0.4)
  id1 = id0 + 1
  trk = mix_track(id0)
  maxs = mus_sound_maxamp("2a.snd")
  if fneq(res1 = maxamp(ind, 0), 0.4 * maxs[1]) or
      fneq(res2 = maxamp(ind, 1), 0.6 * maxs[3])
    snd_display("pan_mix 2->2 0.4: %s %s?", res1, res2)
  end
  unless vequal(track_amp_env(trk), [0, 0.4, 1, 0.4])
    snd_display("pan_mix 2->2 0.4 env: %s?", res)
  end
  if (res = track(trk)) != [id0, id1]
    snd_display("pan_mix 2->2 track 0.4: %s %s?", res, id0)
  end
  revert_sound(ind)
  #
  id0 = pan_mix("4.aiff")
  maxs = mus_sound_maxamp("4.aiff")
  if fneq(res1 = maxamp(ind, 0), maxs[1]) or fneq(res2 = maxamp(ind, 1), 0.0)
    snd_display("pan_mix 4->2 max: %s %s?", res1, res2)
  end
  if (res = mix_chans(id0)) != 4
    snd_display("pan_mix 4->2 mix_chans: %s?", res)
  end
  close_sound(ind)
  # 
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 4, "pan-mix tests")
  id0 = pan_mix("1a.snd")
  id1 = id0 + 1
  trk = make_track(id0)
  if (res = track(mix_track(id0))) != [id0]
    snd_display("pan_mix 1->4 track: %s %s?", res, id0)
  end
  close_sound(ind)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bshort, 22050, 1, "pan-mix-* tests")
  id0 = pan_mix_vct(Vct.new(100, 0.3))
  if (res1 = mix_track(id0)) != 0 or
      fneq(res2 = mix_amp(id0, 0), 1.0) or
      (res3 = mix_amp_env(id0, 0)) != nil
    snd_display("pan_mix_vct 1->1 all opt: %s %s %s?", res1, res2, res3)
  end
  if fneq(res1 = maxamp(ind, 0), res2 = mix_maxamp(id0)) or fneq(res1, 0.3)
    snd_display("pan_mix_vct 1->1 maxamps: %s %s?", res1, res2)
  end
  if (res = mix_position(id0)) != 0
    snd_display("pan_mix_vct 1->1 pos: %s?", res)
  end
  if (res = mix_chans(id0)) != 1
    snd_display("pan_mix_vct 1->1 mix_chans: %s?", res)
  end
  ind1 = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 1, "pan-mix-* tests")
  reg = make_region(0, 50, ind, 0)
  id1 = pan_mix_region(reg)
  if fneq(res1 = maxamp(ind1, 0), res2 = mix_maxamp(id1)) or fneq(res1, 0.3)
    snd_display("pan_mix_region 1->1 maxamps: %s %s?", res1, res2)
  end
  select_all
  revert_sound(ind)
  id0 = pan_mix_selection(0, 1.0, ind, 0)
  if fneq(res1 = maxamp(ind, 0), res2 = mix_maxamp(id0)) or fneq(res1, 0.3)
    snd_display("pan_mix_selection 1->1 maxamps: %s %s?", res1, res2)
  end
  close_sound(ind)
  close_sound(ind1)
end

def test109
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 1, "locked pan_mix tests")
  id0 = pan_mix("1a.snd")
  max1a = mus_sound_maxamp("1a.snd")[1]
  max2a = mus_sound_maxamp("2a.snd")[1]
  snd_display("no-tag pan_mix: %s %s?", id0, mixes) unless mix?(id0) and mix_locked?(id0)
  if fneq(res = maxamp(ind, 0), max1a)
    snd_display("no-tag pan_mix 1->1 maxamps: %s %s?", res, max1a)
  end
  snd_display("no-tag pan_mix 1->1 not locked?") unless mix_locked?(id0)
  revert_sound(ind)
  id0 = pan_mix("2a.snd", 100)
  if fneq(res = maxamp(ind, 0), max2a)
    snd_display("no-tag pan_mix 2->1 maxamps: %s %s?", res, max1a)
  end
  unless vequal(res = channel2vct(3000, 10), Vct.new(10, 0.0))
    snd_display("no-tag pan_mix 2->1 channel 2: %s?", res)
  end
  snd_display("no-tag pan_mix 2->1 not locked?") unless mix_locked?(id0)
  close_sound(ind)
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 2, "locked pan_mix tests")
  id0 = pan_mix("1a.snd")
  unless vequal(res = maxamp(ind, true), [max1a, 0.0])
    snd_display("no-tag pan_mix 1->2 maxamps: %s %s?", res, max1a)
  end
  snd_display("no-tag pan_mix 1->2 not locked?") unless mix_locked?(id0)
  revert_sound(ind)
  id0 = pan_mix("2a.snd", 100)
  unless vequal(res = maxamp(ind, true), [max1a, 0.0])
    snd_display("no-tag pan_mix 2->2 maxamps: %s %s?", res, max1a)
  end
  snd_display("no-tag pan_mix 2->2 not locked?") unless mix_locked?(id0)
  close_sound(ind)
  set_with_mix_tags(true)
  # 
  ind = new_sound("test.snd", Mus_next, Mus_bshort, 22050, 2, "copy sample-reader tests", 1000)
  vct2channel(Vct.new(10) do |i| (i + 1) * 0.1 end, 101, 10, ind, 0)
  vct2channel(Vct.new(10) do |i| (i + 1) * 0.1 end, 201, 10, ind, 1)
  rd1 = make_sample_reader(100, ind, 0)
  rd2 = make_sample_reader(200, ind, 1)
  rd1.call
  rd2.call
  rd1.call
  rd2.call
  rd11 = copy_sample_reader(rd1)
  rd22 = copy_sample_reader(rd2)
  unless sample_reader?(rd11) and sample_reader?(rd22)
    snd_display("copy_sample_reader (normal): %s %s?", rd11, rd22)
  end
  if mix_sample_reader?(rd11) or mix_sample_reader?(rd22) or 
      track_sample_reader?(rd11) or track_sample_reader?(rd22) or 
      region_sample_reader?(rd11) or region_sample_reader?(rd22)
    snd_display("copy_sample_reader? trouble: %s %s?", rd11, rd22)
  end
  if (res1 = sample_reader_home(rd11)) != [ind, 0] or
      (res2 = sample_reader_home(rd22)) != [ind, 1]
    snd_display("copy_sample_reader home: %s %s?", res1, res2)
  end
  if (res1 = sample_reader_at_end?(rd11)) or (res2 = sample_reader_at_end?(rd22))
    snd_display("copy_sample_reader end: %s %s?", res1, res2)
  end
  if (res1 = sample_reader_position(rd11)) != (res2 = sample_reader_position(rd1)) or res1 != 102 or
      (res3 = sample_reader_position(rd22)) != (res4 = sample_reader_position(rd2)) or res3 != 202
    snd_display("copy_sample_reader position: %s %s %s %s?", res1, res2, res3, res4)
  end
  v1 = Vct.new(12) do |i|
    if i < 9
      (i + 2) * 0.1
    else
      0.0
    end
  end
  10.times do |i|
    rd1v = rd1.call
    rd11v = rd11.call
    rd2v = next_sample(rd2)
    rd22v = read_sample(rd22)
    if fneq(rd1v, rd11v) or
        fneq(rd1v, v1[i]) or
        fneq(rd2v, rd22v) or
        fneq(rd2v, v1[i])
      snd_display("copy_sample_reader vals at %d: %s %s %s %s %s?",
                  i, rd1v, rd11v, rd2v, rd22v, v1[i])
      break
    end
  end
  free_sample_reader(rd1)
  free_sample_reader(rd11)
  #
  mx1m = mix_vct(Vct.new(10) do |i| (i + 1) * 0.1 end,  95, ind, 0)
  mx2m = mix_vct(Vct.new(10) do |i| (i + 1) * 0.1 end, 195, ind, 1)
  mx1rd = make_mix_sample_reader(mx1m, 2)
  mx2rd = make_mix_sample_reader(mx2m, 4)
  mx1rd.call
  mx1rd.call
  val1 = mx1rd.call
  val2 = mx2rd.call
  if fneq(val1, val2) or fneq(val2, 0.5)
    snd_display("mix_sample_reader (precopy) vals: %s %s?", val1, val2)
  end
  mx11rd = copy_sample_reader(mx1rd)
  mx22rd = copy_sample_reader(mx2rd)
  unless mix_sample_reader?(mx11rd) and mix_sample_reader?(mx22rd)
    snd_display("copy_sample_reader (mix): %s %s?", mx11rd, mx22rd)
  end
  if sample_reader?(mx11rd) or sample_reader?(mx22rd) or 
      track_sample_reader?(mx11rd) or track_sample_reader?(mx22rd) or 
      region_sample_reader?(mx11rd) or region_sample_reader?(mx22rd)
    snd_display("copy_sample_reader? trouble (mix): %s %s?", mx11rd, mx22rd)
  end
  if (res1 = sample_reader_home(mx11rd)) != mx1m or
      (res2 = sample_reader_home(mx22rd)) != mx2m
    snd_display("copy_sample_reader home (mix): %s %s?", res1, res2)
  end
  if (res1 = sample_reader_at_end?(mx11rd)) or (res2 = sample_reader_at_end?(mx22rd))
    snd_display("copy_sample_reader end (mix): %s %s?", res1, res2)
  end
  if (res1 =sample_reader_position(mx11rd)) != (res2 =sample_reader_position(mx1rd)) or res1 != 5 or
      (res3 = sample_reader_position(mx22rd)) != (res4 = sample_reader_position(mx2rd)) or res3 != 5
    snd_display("copy_sample_reader position (mix): %s %s %s %s?", res1, res2, res3, res4)
  end
  if (res = snd_catch do next_sample(mx11rd) end).first != :wrong_type_arg
    snd_display("next_sample of mix reader: %s", res.inspect)
  end
  v1 = Vct.new(8) do |i|
    if i < 5
      (i + 6) * 0.1
    else
      0.0
    end
  end
  6.times do |i|
    mx1rdv = mx1rd.call
    mx11rdv = mx11rd.call
    mx2rdv = read_mix_sample(mx2rd)
    mx22rdv = read_mix_sample(mx22rd)
    if fneq(mx1rdv, mx11rdv) or
        fneq(mx1rdv, v1[i]) or
        fneq(mx2rdv, mx22rdv) or
        fneq(mx2rdv, v1[i])
      snd_display("copy_sample_reader (mix) vals at %d: %s %s %s %s %s?",
                  i, mx1rdv, mx11rdv, mx2rdv, mx22rdv, v1[i])
      break
    end
  end
  free_sample_reader(mx1rd)
  free_sample_reader(mx11rd)
  #
  trk = make_track
  set_mix_track(mx1m, trk)
  set_mix_track(mx2m, trk)
  mx1rd = make_track_sample_reader(trk, 0, 2)
  mx2rd = make_track_sample_reader(trk, 1, 4)
  mx1rd.call
  mx1rd.call
  val1 = mx1rd.call
  val2 = mx2rd.call
  if fneq(val1, val2) or fneq(val2, 0.5)
    snd_display("track_sample_reader (precopy) vals: %s %s?", val1, val2)
  end
  mx11rd = copy_sample_reader(mx1rd)
  mx22rd = copy_sample_reader(mx2rd)
  unless track_sample_reader?(mx11rd) and track_sample_reader?(mx22rd)
    snd_display("copy_sample_reader (track): %s %s?", mx11rd, mx22rd)
  end
  if sample_reader?(mx11rd) or sample_reader?(mx22rd) or 
      mix_sample_reader?(mx11rd) or mix_sample_reader?(mx22rd) or 
      region_sample_reader?(mx11rd) or region_sample_reader?(mx22rd)
    snd_display("copy_sample_reader? trouble (track): %s %s?", mx11rd, mx22rd)
  end
  if (res1 = sample_reader_home(mx11rd)) != [trk, 0] or
      (res2 = sample_reader_home(mx22rd)) != [trk, 1]
    snd_display("copy_sample_reader home (track): %s %s?", res1, res2)
  end
  if (res1 = sample_reader_at_end?(mx11rd)) or (res2 = sample_reader_at_end?(mx22rd))
    snd_display("copy_sample_reader end (track): %s %s?", res1, res2)
  end
  if (res1 =sample_reader_position(mx11rd)) != (res2 =sample_reader_position(mx1rd)) or res1 != 5 or
      (res3 = sample_reader_position(mx22rd)) != (res4 = sample_reader_position(mx2rd)) or res3 != 5
    snd_display("copy_sample_reader position (track): %s %s %s %s?", res1, res2, res3, res4)
  end
  if (res = snd_catch do next_sample(mx11rd) end).first != :wrong_type_arg
    snd_display("next_sample of track reader: %s", res.inspect)
  end
  v1 = Vct.new(8) do |i|
    if i < 5
      (i + 6) * 0.1
    else
      0.0
    end
  end
  6.times do |i|
    mx1rdv = mx1rd.call
    mx11rdv = mx11rd.call
    mx2rdv = read_track_sample(mx2rd)
    mx22rdv = read_track_sample(mx22rd)
    if fneq(mx1rdv, mx11rdv) or
        fneq(mx1rdv, v1[i]) or
        fneq(mx2rdv, mx22rdv) or
        fneq(mx2rdv, v1[i])
      snd_display("copy_sample_reader (track) vals at %d: %s %s %s %s %s?",
                  i, mx1rdv, mx11rdv, mx2rdv, mx22rdv, v1[i])
      break
    end
  end
  free_sample_reader(mx1rd)
  free_sample_reader(mx11rd)
  #
  set_sync(1, ind)
  reg = make_region(90, 220, ind, true)
  if (res = region_frames(reg)) != 220 - 90 + 1
    snd_display("make_region frames: %s?", res)
  end
  if (res = region_chans(reg)) != 2
    snd_display("make_region chans: %s?", res)
  end
  if (res = region_frames(reg, 0)) != 220 - 90 + 1
    snd_display("make_region frames[0]: %s?", res)
  end
  if (res = region_frames(reg, 1)) != 220 - 90 + 1
    snd_display("make_region frames[1]: %s?", res)
  end
  if (res = region_position(reg)) != 90
    snd_display("make_region position: %s?", res)
  end
  if (res = region_position(reg, 0)) != 90
    snd_display("make_region position[0]: %s?", res)
  end
  if (res = region_position(reg, 1)) != 90
    snd_display("make_region position[1]: %s?", res)
  end
  rd1 = make_region_sample_reader(  0, reg, 0)
  rd2 = make_region_sample_reader(100, reg, 1)
  rd11 = copy_sample_reader(rd1)
  rd22 = copy_sample_reader(rd2)
  unless region_sample_reader?(rd11) and region_sample_reader?(rd22)
    snd_display("copy_sample_reader (region): %s %s?", rd11, rd22)
  end
  if sample_reader?(rd11) or sample_reader?(rd22) or 
      mix_sample_reader?(rd11) or mix_sample_reader?(rd22) or 
      track_sample_reader?(rd11) or track_sample_reader?(rd22)
    snd_display("copy_sample_reader? trouble (region): %s %s?", rd11, rd22)
  end
  if (res1 = sample_reader_home(rd11)) != [reg, 0] or
      (res2 = sample_reader_home(rd22)) != [reg, 1]
    snd_display("copy_sample_reader home (region): %s %s?", res1, res2)
  end
  if (res1 = sample_reader_at_end?(rd11)) or (res2 = sample_reader_at_end?(rd22))
    snd_display("copy_sample_reader end (region): %s %s?", res1, res2)
  end
  if (res1 = sample_reader_position(rd11)) != (res2 = sample_reader_position(rd1)) or res1 != 0 or
      (res3 = sample_reader_position(rd22)) != (res4 = sample_reader_position(rd2)) or res3 != 100
    snd_display("copy_sample_reader position (region): %s %s %s %s?", res1, res2, res3, res4)
  end
  vct(0.000, 0.000, 0.000, 0.000, 0.000, 0.100, 0.200, 0.300,
      0.400, 0.500, 0.600, 0.800, 1.000, 1.200, 1.400, 0.500,
      0.600, 0.700, 0.800, 0.900).each_with_index do |val, i|
    rd1v = rd1.call
    rd11v = rd11.call
    rd2v = read_region_sample(rd2)
    rd22v = read_region_sample(rd22)
    if fneq(rd1v, rd11v) or
        fneq(rd1v, val) or
        fneq(rd2v, rd22v) or
        fneq(rd2v, val)
      snd_display("copy_sample_reader (region) vals at %d: %s %s %s %s %s?",
                  i, rd1v, rd11v, rd2v, rd22v, val)
      break
    end
  end
  free_sample_reader(rd1)
  free_sample_reader(rd11)
  close_sound(ind)
  #
  # define_envelope tests
  # 
  ind = new_sound("tmp.snd", Mus_next, Mus_bfloat, 22050, 1, "mix with exp env tests", 1000)
  mx = mix_vct(Vct.new(10, 1.0), 100)
  set_mix_amp_env(mx, $xrmx)
  unless vequal(res = channel2vct(100, 10),
                vct(0.000, 0.015, 0.037, 0.070, 0.118, 0.189, 0.293, 0.446, 0.670, 1.000))
    snd_display("mix_vct + exp env: %s?", res)
  end
  revert_sound(ind)
  mx = mix_vct(Vct.new(100, 1.0), 100)
  set_mix_amp_env(mx, $xrmx)
  trk = make_track(mx)
  set_track_amp_env(trk, [0, 0, 1, 1])
  unless vequal(res = channel2vct(120, 10),
                vct(0.006, 0.008, 0.009, 0.010, 0.011, 0.012, 0.013, 0.014, 0.015, 0.017))
    snd_display("mix_vct + exp + track ramp (20): %s?", res)
  end
  unless vequal(res = channel2vct(190, 10),
                vct(0.665, 0.702, 0.740, 0.777, 0.814, 0.851, 0.888, 0.926, 0.963, 1.000))
    snd_display("mix_vct + exp + track ramp (90): %s?", res)
  end
  set_track_amp_env(trk, $xrmx)
  unless vequal(res = channel2vct(190, 10),
                vct(0.538, 0.589, 0.641, 0.692, 0.743, 0.795, 0.846, 0.897, 0.949, 1.000))
    snd_display("mix_vct + exp + track exp (90): %s?", res)
  end
  set_mix_amp_env(mx, [0, 0, 1, 1])
  unless vequal(res = channel2vct(190, 10),
                vct(0.665, 0.702, 0.740, 0.777, 0.814, 0.851, 0.888, 0.926, 0.963, 1.000))
    snd_display("mix_vct + ramp + track exp (90): %s?", res)
  end
  set_mix_amp_env(mx, false)
  unless vequal(res = channel2vct(190, 10),
                vct(0.721, 0.748, 0.776, 0.804, 0.834, 0.865, 0.897, 0.930, 0.964, 1.000))
    snd_display("mix_vct + track exp (90): %s?", res)
  end
  set_track_amp_env(trk, false)
  set_mix_amp_env(mx, $xrmx)
  unless vequal(res = channel2vct(190, 10),
                vct(0.721, 0.748, 0.776, 0.804, 0.834, 0.865, 0.897, 0.930, 0.964, 1.000))
    snd_display("mix_vct exp + track null (90): %s?", res)
  end
  revert_sound(ind)
  mx = mix_vct(Vct.new(100, 1.0), 100)
  mx1r = mix_vct(Vct.new(100, 1.0), 200)
  trk1 = make_track(mx, mx1r)
  set_track_amp_env(trk1, $xrmx2)
  unless vequal(res = channel2vct(190, 10),
                vct(0.542, 0.547, 0.552, 0.557, 0.562, 0.567, 0.572, 0.576, 0.581, 0.586))
    snd_display("mix_vct track amp env $xrmx2 190 10: %s?", res)
  end
  unless vequal(res = channel2vct(200, 10),
                vct(0.586, 0.591, 0.596, 0.601, 0.606, 0.611, 0.615, 0.620, 0.625, 0.630))
    snd_display("mix_vct track amp env $xrmx2 200 10: %s?", res)
  end
  close_sound(ind)
  # 
  ind = new_sound("tmp.snd", Mus_next, Mus_bfloat, 22050, 2, "stereo mix with exp env tests", 1000)
  mx0t = mix_vct(Vct.new(10, 1.0), 100, ind, 0)
  mx1t = mix_vct(Vct.new(10, 1.0), 100, ind, 1)
  trk = make_track(mx0t, mx1t)
  set_mix_inverted?(mx1t, true)
  set_track_amp_env(trk, $xrmx)
  unless vequal(res = channel2vct(100, 10, ind, 0),
                vct(0.000, 0.015, 0.037, 0.070, 0.118, 0.189, 0.293, 0.446, 0.670, 1.000))
    snd_display("0 mix_vct + exp env: %s?", res)
  end
  unless vequal(res = channel2vct(100, 10, ind, 1),
                vct(1.000, 0.985, 0.963, 0.930, 0.882, 0.811, 0.707, 0.554, 0.330, 0.000))
    snd_display("1 mix_vct + exp env: %s?", res)
  end
  close_sound(ind)
end

def test119
  old = tempo_control_bounds
  if fneq(old[0], 0.0) or fneq(old[1], 8.0)
    snd_display("tempo_control_bounds defaults: %s?", old)
  end
  set_tempo_control_bounds([0.0, 2.0])
  old = tempo_control_bounds
  if fneq(old[0], 0.0) or fneq(old[1], 2.0)
    snd_display("set_tempo_control_bounds [0.0, 2.0]: %s?", old)
  end
  set_tempo_control_bounds([0.0, 8.0])
  ind = new_sound("test.snd", :size, 10)
  mx1 = mix_vct(Vct.new(2, 0.1), 0)
  mx2 = mix_vct(Vct.new(2, 0.2), 2)
  mx3 = mix_vct(Vct.new(2, 0.3), 4)
  mx4 = mix_vct(Vct.new(2, 0.4), 6)
  mx5 = mix_vct(Vct.new(2, 0.5), 8)
  unless (res = channel2vct, vct(0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5, 0.5))
    snd_display("delete_all_tracks init: %s?", res)
  end
  delete_all_tracks
  unless (res = channel2vct, vct(0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5, 0.5))
    snd_display("delete_all_tracks no-op: %s?", res)
  end
  trk = make_track(mx1, mx3)
  trk1 = make_track
  set_mix_track(mx4, trk1)
  delete_all_tracks
  unless (res = channel2vct, vct(0, 0, 0.2, 0.2, 0, 0, 0, 0, 0.5, 0.5))
    snd_display("delete_all_tracks: %s?", res)
  end
  close_sound(ind)
  #
  ind = open_sound("oboe.snd")
  mx = mix_vct(Vct.new(100, 0.1), 1000)
  fr = mus_sound_frames("1a.snd")
  [[lambda do pad_channel(0,     100) end, 1100, false, :pad0],
   [lambda do pad_channel(0,    2000) end, 3000, false, :pad20],
   [lambda do pad_channel(800,   100) end, 1100, false, :pad800],
   [lambda do pad_channel(850,   100) end, 1100, false, :pad800],
   [lambda do pad_channel(990,   100) end, 1100, false, :pad990],
   [lambda do pad_channel(1010,  100) end, 1000, true,  :pad1010],
   [lambda do pad_channel(1050,   10) end, 1000, true,  :pad1050],
   [lambda do pad_channel(1110,  100) end, 1000, false, :pad1110],
   [lambda do pad_channel(2000,  100) end, 1000, false, :pad2000],
   [lambda do insert_samples(0,    100, Vct.new(100, 0.2)) end, 1100, false, :insert0],
   [lambda do insert_samples(800,  100, Vct.new(100, 0.2)) end, 1100, false, :insert800],
   [lambda do insert_samples(990,  100, Vct.new(100, 0.2)) end, 1100, false, :insert990],
   [lambda do insert_samples(1010, 100, Vct.new(100, 0.2)) end, 1000, true,  :insert1010],
   [lambda do insert_samples(1050,  10, Vct.new(100, 0.2)) end, 1000, true,  :insert1050],
   [lambda do insert_samples(1110, 100, Vct.new(100, 0.2)) end, 1000, false, :insert1110],
   [lambda do insert_samples(2000, 100, Vct.new(100, 0.2)) end, 1000, false, :insert2000],
   [lambda do insert_sound("1a.snd",    0) end, fr + 1000, false, :inserts0],
   [lambda do insert_sound("1a.snd",  800) end, fr + 1000, false, :inserts800],
   [lambda do insert_sound("1a.snd",  990) end, fr + 1000, false, :inserts990],
   [lambda do insert_sound("1a.snd", 1010) end,      1000, true,  :inserts1010],
   [lambda do insert_sound("1a.snd", 1050) end,      1000, true,  :inserts1050],
   [lambda do insert_sound("1a.snd", 1110) end,      1000, false, :inserts1110],
   [lambda do insert_sound("1a.snd", 2000) end,      1000, false, :inserts2000],
   [lambda do delete_samples(0,    100) end,  900, false, :delete0],
   [lambda do delete_samples(0,   2000) end, 1000, true,  :delete20],
   [lambda do delete_samples(800,  100) end,  900, false, :delete800],
   [lambda do delete_samples(850,  100) end,  900, false, :delete850],
   [lambda do delete_samples(950,   40) end,  960, false, :delete950],
   [lambda do delete_samples(990,  100) end, 1000, true,  :delete990],
   [lambda do delete_samples(1010, 100) end, 1000, true,  :delete1010],
   [lambda do delete_samples(1050,  10) end, 1000, true,  :delete1050],
   [lambda do delete_samples(1110, 100) end, 1000, false, :delete1110],
   [lambda do delete_samples(2000, 100) end, 1000, false, :delete2000],
   [lambda do set_samples(0,    100, Vct.new(100,  0.2)) end, 1000, false, :set0],
   [lambda do set_samples(0,   2000, Vct.new(2000, 0.2)) end, 1000, true,  :set0],
   [lambda do set_samples(800,  100, Vct.new(100,  0.2)) end, 1000, false, :set800],
   [lambda do set_samples(990,  100, Vct.new(100,  0.2)) end, 1000, true,  :set990],
   [lambda do set_samples(1010, 100, Vct.new(100,  0.2)) end, 1000, true,  :set1010],
   [lambda do set_samples(1050,  10, Vct.new(100,  0.2)) end, 1000, true,  :set1050],
   [lambda do set_samples(1110, 100, Vct.new(100,  0.2)) end, 1000, false, :set1110],
   [lambda do set_samples(2000, 100, Vct.new(100,  0.2)) end, 1000, false, :set2000],
   [lambda do scale_channel(2.0, 0,    100) end, 1000, false, :scale0],
   [lambda do scale_channel(2.0, 0,   2000) end, 1000, true,  :scale20],
   [lambda do scale_channel(2.0, 800,  100) end, 1000, false, :scale800],
   [lambda do scale_channel(2.0, 850,  100) end, 1000, false, :scale850],
   [lambda do scale_channel(2.0, 950,   40) end, 1000, false, :scale950],
   [lambda do scale_channel(2.0, 990,  100) end, 1000, true,  :scale990],
   [lambda do scale_channel(2.0, 1010, 100) end, 1000, true,  :scale1010],
   [lambda do scale_channel(2.0, 1050,  10) end, 1000, true,  :scale1050],
   [lambda do scale_channel(2.0, 1110, 100) end, 1000, false, :scale1110],
   [lambda do scale_channel(2.0, 2000, 100) end, 1000, false, :scale2000],
   [lambda do env_channel([0, 0, 1, 1], 0,    100) end, 1000, false, :env0],
   [lambda do env_channel([0, 0, 1, 1], 0,   2000) end, 1000, true,  :env20],
   [lambda do env_channel([0, 0, 1, 1], 800,  100) end, 1000, false, :env800],
   [lambda do env_channel([0, 0, 1, 1], 850,  100) end, 1000, false, :env850],
   [lambda do env_channel([0, 0, 1, 1], 950,   40) end, 1000, false, :env950],
   [lambda do env_channel([0, 0, 1, 1], 990,  100) end, 1000, true,  :env990],
   [lambda do env_channel([0, 0, 1, 1], 1010, 100) end, 1000, true,  :env1010],
   [lambda do env_channel([0, 0, 1, 1], 1050,  10) end, 1000, true,  :env1050],
   [lambda do env_channel([0, 0, 1, 1], 1110, 100) end, 1000, false, :env1110],
   [lambda do env_channel([0, 0, 1, 1], 2000, 100) end, 1000, false, :env2000],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 0,    100) end, 1000, false, :ptree0],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 0,   2000) end, 1000, true,  :ptree20],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 800,  100) end, 1000, false, :ptree800],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 850,  100) end, 1000, false, :ptree850],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 950,   40) end, 1000, false, :ptree950],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 990,  100) end, 1000, true,  :ptree990],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 1010, 100) end, 1000, true,  :ptree1010],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 1050,  10) end, 1000, true,  :ptree1050],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 1110, 100) end, 1000, false, :ptree1110],
   [lambda do ptree_channel(lambda do |y| y * 2.0 end, 2000, 100) end, 1000, false, :ptree2000],
  ].each do |func, beg, lock, name|
    edpos = edit_position(ind, 0)
    func.call
    if (res = mix_locked?(mx)) != lock
      snd_display("%s mix locked: %s %s?", name, lock, res)
    end
    if (res = mix_position(mx)) != beg
      snd_display("%s mix lock beg: %s %s?", name, beg, res)
    end
    set_edit_position(edpos, ind, 0)
  end
  close_sound(ind)
end

def test09
  test009
  test019
  test029
  test039
  test049
  test059
  test069
  test079
  test089
  test099
  begin
    old_mix_tag = with_mix_tags
    set_with_mix_tags(false)
    test109
  rescue
    raise
  ensure
    set_with_mix_tags(old_mix_tag)
  end
  test119
end

if (not provided?(:snd_nogui)) and ($test09 and $full_test or $snd_test == 9)
  $before_test_hook.call(9)
  test09
  $after_test_hook.call(9)
end

# ---------------- test 10: marks ----------------

def data_max(beg, fin)
  maxval = 0.0
  sounds2array.each do |snd|
    channels(snd).times do |chn|
      scan_chan(lambda do |data|
                  maxval = [maxval, data.abs].max
                  false
                end, 0, false, snd, chn)
    end
  end
  maxval
end

def data_max2(beg, fin, snd)
  maxval = 0.0
  channels(snd).times do |chn|
    scan_chan(lambda do |data|
                maxval = [maxval, data.abs].max
                false
              end, 0, false, snd, chn)
  end
  maxval
end

def data_max1(beg, fin, snd, chn)
  maxval = 0.0
  scan_chan(lambda do |data|
              maxval = [maxval, data.abs].max
              false
            end, beg, fin, snd, chn)
  maxval
end

def test0010a
  ind0 = view_sound("oboe.snd")
  ind1 = view_sound("pistol.snd")
  v0 = Vct.new(100)
  v0.fill(0.1)
  vc = [mix_vct(v0, 0, ind0),
        mix_vct(v0, 1000, ind0),
        mix_vct(v0, 2000, ind0),
        mix_vct(v0, 3000, ind0),
        mix_vct(v0, 4000, ind0),
        mix_vct(v0, 0, ind1),
        mix_vct(v0, 1000, ind1),
        mix_vct(v0, 2000, ind1),
        mix_vct(v0, 3000, ind1),
        mix_vct(v0, 4000, ind1)]
  t0 = make_track(vc[0], vc[3], vc[5])
  t1 = make_track(vc[2], vc[6], vc[8])
  set_track_amp(t0, 0.5)
  with_time("transpose_track(t1, 3)") do transpose_track(t1, 3) end
  set_track_color(t1, make_color_with_catch(0, 0, 1))
  t0e = track_end(t0)
  set_track_position(t0, 1000)
  if (res = track_position(t0)) != 1000
    snd_display("track_position: %s?", res)
  end
  if (res = track_end(t0)) != t0e + 1000
    snd_display("track_end: %s?", res)
  end
  if (res = track_frames(t0)) != 3100
    snd_display("track_frames: %s?", res)
  end
  retempo_track(t0, 2.0)
  unless provided? :snd_nogui
    col = color2list(track_color(t1))
    if fneq(col[0], 0.0) or fneq(col[1], 0.0) or fneq(col[2], 1.0)
      snd_display("track_color: %s?", col)
    end
  end
  if (res = track_frames(t0)) != 1600
    snd_display("track_tempo -> length: %s?", res)
  end
  close_sound(ind0)
  close_sound(ind1)
  #
  ind0 = new_sound("fmv.snd", Mus_aifc, Mus_bshort, 22050, 1, "this is a comment")
  v0 = make_array(10, 1.0)
  insert_samples(0, 10, v0, ind0)
  with_time("env_sound([0, 0, 1, 1], 0, 10, 1.0, ind0)") do
    env_sound([0, 0, 1, 1], 0, 10, 1.0, ind0)
  end
  10.times do |i|
    if fneq(sample(i), i * 0.1111)
      snd_display("1 env_sound[%d]: %s?", i, sample(i))
    end
  end
  undo_edit
  env_sound(make_env([0, 0, 1, 1], :end, 9), 0, 10, 1.0, ind0)
  10.times do |i|
    if fneq(sample(i), i * 0.1111)
      snd_display("2 env_sound[%d]: %s?", i, sample(i))
    end
  end
  undo_edit
  env_sound([0, 0, 0.5, 1, 1, 1], 0, 10, 0.0, ind0)
  if fneq(sample(3), 0.0) or fneq(sample(8), 1.0)
    snd_display("env_sound stepped: %s %s?", sample(3), sample(8))
  end
  undo_edit
  env_sound([0, 0, 1, 1], 0, 10, 32.0, ind0)
  if fneq(sample(3), 0.07) or fneq(sample(8), 0.67)
    snd_display("env_sound exp: %s %s?", sample(3), sample(8))
  end
  undo_edit
  env_sound(make_env([0, 0, 1, 1], :base, 32.0, :end, 9), 0, 10, 32.0, ind0)
  if fneq(sample(3), 0.07) or fneq(sample(8), 0.67)
    snd_display("env_sound exp: %s %s?", sample(3), sample(8))
  end
  undo_edit
  env_sound([0, 2])
  10.times do |i|
    if fneq(sample(i), 2.0)
      snd_display("3 env_sound[%d]: %s?", i, sample(i))
    end
  end
  undo_edit
  env_sound([0, 2], 2, 4, 1.0, ind0)
  if fneq(sample(1), 1.0) or
      fneq(sample(2), 2.0) or
      fneq(sample(5), 2.0) or
      fneq(sample(8), 1.0)
    snd_display("3 env_sound exp: %s %s %s %s?", sample(1), sample(2),  sample(5), sample(8))
  end
  undo_edit
  (1...10).each do |i| set_sample(i, 0.0) end
  filter_sound([0, 1, 1, 0], 4)
  if fneq(sample(1), 0.3678) or
      fneq(sample(2), 0.3678) or
      fneq(sample(3), 0.132) or
      fneq(sample(4), 0.0)
    snd_display("filter_sound env: %s?", samples(0, 8))
  end
  undo_edit
  filter_sound([0, 1, 1, 0], 1024)
  undo_edit
  filter_sound(make_fir_filter(6, [0.1, 0.2, 0.3, 0.3, 0.2, 0.1].to_vct))
  undo_edit
  filter_sound(make_delay(120))
  undo_edit
  filter_sound(make_formant(0.99, 1200))
  undo_edit
  filter_sound([0.125, 0.25, 0.25, 0.125].to_vct, 4)
  if fneq(sample(0), 0.125) or
      fneq(sample(1), 0.25) or
      fneq(sample(2), 0.25) or
      fneq(sample(5), 0.0)
    snd_display("filter_sound direct: %s?", samples(0, 8))
  end
  revert_sound(ind0)
  close_sound(ind0)
end

def test0010
  ind0 = new_sound("fmv.snd", Mus_aifc, Mus_bshort, 22050, 2, "this is a comment")
  ind1 = new_sound("fmv1.snd", Mus_aifc, Mus_bshort, 22050, 1, "this is a comment")
  v0 = make_array(10, 1.0)
  set_sync(123, ind0)
  set_sync(123, ind1)
  insert_samples(0, 10, v0, ind0, 0)
  insert_samples(0, 10, v0, ind0, 1)
  insert_samples(0, 10, v0, ind1, 0)
  env_sound([0, 0, 1, 1], 0, 10, 1.0, ind0)
  10.times do |i|
    if fneq(res = sample(i, ind0, 0), i * 0.1111)
      snd_display("ind0:0 1 env_sound[%d]: %s?", i, res)
    end
    if fneq(res = sample(i, ind0, 1), i * 0.1111)
      snd_display("ind0:1 1 env_sound[%d]: %s?", i, res)
    end
    if fneq(res = sample(i, ind0, 0), i * 0.1111)
      snd_display("ind1:0 1 env_sound[%d]: %s?", i, res)
    end
  end
  undo_edit
  env_sound(make_env(:envelope, [0, 0, 1, 1], :end, 9), 0, 10, 1.0, ind0)
  10.times do |i|
    if fneq(res = sample(i, ind0, 0), i * 0.1111)
      snd_display("ind0:0 2 env_sound[%d]: %s?", i, res)
    end
    if fneq(res = sample(i, ind0, 1), i * 0.1111)
      snd_display("ind0:1 2 env_sound[%d]: %s?", i, res)
    end
    if fneq(res = sample(i, ind0, 0), i * 0.1111)
      snd_display("ind1:0 2 env_sound[%d]: %s?", i, res)
    end
  end
  undo_edit
  env_sound([0, 0, 0.5, 1, 1, 1], 0, 10, 0.0, ind0)
  if fneq(res1 = sample(3, ind0, 0), 0.0) or fneq(res2 = sample(8, ind0, 0), 1.0)
    snd_display("ind0:0 env_sound stepped: %s %s?", res1, res2)
  end
  if fneq(res1 = sample(3, ind0, 0), 0.0) or fneq(res2 = sample(8, ind0, 0), 1.0)
    snd_display("ind0:1 env_sound stepped: %s %s?", res1, res2)
  end
  if fneq(res1 = sample(3, ind0, 0), 0.0) or fneq(res2 = sample(8, ind0, 0), 1.0)
    snd_display("ind1:0 env_sound stepped: %s %s?", res1, res2)
  end
  undo_edit
  revert_sound(ind0)
  revert_sound(ind1)
  insert_samples(0, 10, v0, ind0, 0)
  insert_samples(0, 10, v0, ind0, 1)
  insert_samples(0, 10, v0, ind1, 0)
  filter_sound(make_one_zero(:a0, 0.5, :a1, 0.0), 0, ind0)
  10.times do |i|
    if fneq(res = sample(i, ind0, 0), 0.5)
      snd_display("ind0:0 1 filter_sound[%d]: %s?", i, res)
    end
    if fneq(res = sample(i, ind0, 1), 0.5)
      snd_display("ind0:1 1 filter_sound[%d]: %s?", i, res)
    end
    if fneq(res = sample(i, ind0, 0), 0.5)
      snd_display("ind1:0 1 filter_sound[%d]: %s?", i, res)
    end
  end
  close_sound(ind0)
  close_sound(ind1)
  #
  ind0 = new_sound("fmv.snd", Mus_aifc, Mus_bshort, 22050, 1, "this is a comment")
  v0 = Vct.new(10, 0.1)
  old5 = sample(5, ind0)
  insert_samples(10, 10, v0, ind0)
  env_sound([0, 0, 1, 2], 10, 10, 1.0, ind0)
  10.times do |i|
    if fneq(res = sample(i + 10, ind0), i * 0.0222)
      snd_display("env_sound[%d]: %s?", i + 10, res)
    end
  end
  if fneq(res = sample(5, ind0), old5)
    snd_display("env_sound[5]: %s %s?", old5, res)
  end
  undo_edit
  env_sound([0, 0, 1, 2], 10, 10, 4.0, ind0)
  v0 = channel2vct(10, 10)
  if fneq(v0[3], 0.039) or fneq(v0[8], 0.162)
    snd_display("env_sound 4.0: %s?", v0)
  end
  undo_edit
  env_sound([0, 0, 1, 2], 10, 10, 0.05, ind0)
  v0 = channel2vct(10, 10)
  if fneq(v0[3], 0.133) or fneq(v0[8], 0.196)
    snd_display("env_sound 0.05: %s?", v0)
  end
  close_sound(ind0)
  #
  ind0 = new_sound("fmv.snd", Mus_aifc, Mus_bshort, 22050, 2, "this is a comment")
  ind1 = new_sound("fmv1.snd", Mus_next, Mus_bshort, 22050, 1, "this is a comment")
  insert_samples(0, 10, make_array(10, 1.00), ind0, 0)
  insert_samples(0, 10, make_array(10, 0.10), ind0, 1)
  insert_samples(0, 10, make_array(10, 0.01), ind1, 0)
  if fneq(res = data_max1(0, 9, ind0, 0), 1.00)
    snd_display("scan_chan[0,0]: %s?", res)
  end
  if fneq(res = data_max1(0, 9, ind0, 1), 0.10)
    snd_display("scan_chan[0,1]: %s?", res)
  end
  if fneq(res = data_max1(0, 9, ind1, 0), 0.01)
    snd_display("scan_chan[1,0]: %s?", res)
  end
  if fneq(res = data_max1(0, 9, false, false), 0.01)
    snd_display("scan_chans: %s?", res)
  end
  if fneq(res = data_max(0, 9), 1.00)
    snd_display("scan_all_chans: %s?", res)
  end
  if fneq(res = data_max2(0, 9, ind0), 1.00)
    snd_display("scan_across_all_chans: %s?", res)
  end
  close_sound(ind0)
  close_sound(ind1)
end

def test0110
  ind0 = new_sound("fmv.snd", Mus_aifc, Mus_bshort, 22050, 2, "this is a comment")
  mix("oboe.snd")
  m1 = add_mark(100)
  delete_sample(10)
  m2 = add_mark(200)
  delete_sample(10)
  m3 = add_mark(300)
  undo_edit
  save_sound
  if marks(ind0, 0).length != 2
    snd_display("marks after save: %s?", marks(ind0, 0))
  end
  if (not mark?(m1)) or mark_sample(m1) != 99
    snd_display("save_sound mark1: %s?", mark_sample(m1))
  end
  if (not mark?(m2)) or mark_sample(m2) != 200
    snd_display("save_sound mark2: %s?", mark_sample(m1))
  end
  snd_display("save_sound mark3: %s?", m3) if mark?(m3)
  close_sound(ind0)
  #
  fd = open_sound("oboe.snd")
  m1 = add_mark(123)
  sync_val = mark_sync_max() + 1
  snd_display("mark?") unless mark?(m1)
  snd_display("add_mark: %s?", mark_sample(m1)) if mark_sample(m1) != 123
  set_mark_property(:hiho, 123, m1)
  snd_display("mark_property: %s?", mark_properties(m1)) if mark_property(:hiho, m1) != 123
  if (res = mark_property(:not_there, m1))
    snd_display("mark_not_property: %s?", res)
  end
  if (res = snd_catch do mark_sample(12345678) end).first != :no_such_mark
    snd_display("mark_sample bad mark: %s", res.inspect)
  end
  if (res = snd_catch do add_mark(123, 123) end).first != :no_such_sound
    snd_display("add_mark bad sound: %s", res.inspect)
  end
  if (m2 = snd_catch do add_mark(12345, fd, 0) end.first) == :no_such_mark
    snd_display("add_mark failed?: %s", m2)
  end
  snd_display("add_mark 0 0: %s?", mark_sample(m2)) if mark_sample(m2) != 12345
  snd_display("init mark_sync: %s?", mark_sync(m2)) if mark_sync(m2) != 0
  set_mark_sync(m2, sync_val)
  if (res = mark_sync(m2)) != sync_val
    snd_display("set_mark_sync (%s): %s?", sync_val, res)
  end
  syncs = syncd_marks(sync_val)
  chans = marks(fd, 0)
  samps = chans.map do |chn| mark_sample(chn) end
  snd_display("syncd_marks: %s?", syncs) if syncs != [m2]
  snd_display("marks: %s?", chans) if chans != [m1, m2]
  snd_display("map samps: %s?", samps) if samps != [mark_sample(m1), mark_sample(m2)]
  delete_samples(200, 100, fd, 0)
  chans = marks(fd)
  samps = chans.first.map do |chn| mark_sample(chn) end
  snd_display("map samps: %s?", samps) if samps != [mark_sample(m1, 0), mark_sample(m2, 0) - 100]
  if (res = describe_mark(m2)) != [[:mark, m2, :sound, fd, "oboe.snd", :channel, 0], 12345, 12245]
    snd_display("describe_mark: %s?", res)
  end
  set_mark_sync(m1, mark_sync(m2))
  move_syncd_marks(sync_val, 100)
  chans = marks(fd)
  samps = chans.first.map do |chn| mark_sample(chn) end
  if samps != [mark_sample(m1, 0) + 100, mark_sample(m2, 0)]
    snd_display("syncd move samps: %s?", samps)
  end
  set_cursor(500)
  backward_mark
  snd_display("backward_mark: %s?", cursor) if cursor != mark_sample(m1)
  forward_mark(1)
  snd_display("forward_mark: %s?", cursor) if cursor != mark_sample(m2)
  set_mark_sync(m1, true)
  snd_display("mark_sync via bool: %s?", mark_sync(m1)) if mark_sync(m1) != 1
  delete_mark(m1)
  chans = marks(fd, 0)
  snd_display("delete_mark: %s?", chans) if chans != [m2]
  undo_edit
  chans = marks(fd, 0)
  snd_display("delete_mark then undo: %s?", chans) if chans != [m1, m2]
  redo_edit
  snd_display("init mark_name: %s?", mark_name(m2)) if mark_name(m2) != ""
  set_mark_name(m2, "hiho!")
  snd_display("set_mark_name: %s?", mark_name(m2)) if mark_name(m2) != "hiho!"
  undo_edit
  snd_display("undo mark_name: %s?", mark_name(m2)) if mark_name(m2) != ""
  redo_edit
  snd_display("redo mark_name: %s?", mark_name(m2)) if mark_name(m2) != "hiho!"
  #
  m3 = find_mark("hiho!")
  m4 = find_mark(mark_sample(m2))
  m5 = find_mark("not-a-mark")
  m6 = find_mark(123456787)
  m7 = mark_name2id("hiho!")
  snd_display("find_mark: %s %s %s %s?", m2, m3, m4, m7) unless [m2, m3, m4, m7].uniq.length == 1
  snd_display("find-not-a-mark: %s %s?", m5, m6) unless [m5, m6, false].uniq.length == 1
  set_mark_sample(m2, 2000)
  m1 = add_mark(1000)
  m3 = add_mark(3000)
  m4 = add_mark(4000)
  insert_samples(2500, 500, Vct.new(500), fd, 0)
  samps = (marks(fd, 0) or []).map do |m| mark_sample(m) end
  snd_display("insert ripple: %s?", samps) if samps != [1000, 2000, 3500, 4500]
  set_mark_sample(m3, 300)
  set_cursor(500)
  backward_mark
  snd_display("sort marks: %s?", cursor) if cursor != 300
  snd_display("mark_home: %s?", mark_home(m2)) if mark_home(m2) != [fd, 0]
  sd = open_sound("4.aiff")
  m3 = add_mark(1000, sd, 2)
  m4 = add_mark(1000, sd, 3)
  snd_display("mark->sound 4: %s?", mark_home(m3)) if mark_home(m3) != [sd, 2]
  close_sound(sd)
  file = save_marks(fd)
  if (not file) or file != Dir.pwd + "/oboe.marks"
    snd_display("save_marks: %s?", file)
  end
  file = save_marks(fd, "hiho.marks")
  if (not file) or file != "hiho.marks"
    snd_display("save_marks with arg: %s?", file)
  end
  # system returns true on success ($0 == 0) and false on $? != 0
  unless system(format("diff hiho.marks %s", Dir.pwd + "/oboe.marks"))
    snd_display("save marks differs")
  end
  close_sound(fd)
  #
  s1 = open_sound("oboe.snd")
  s2 = open_sound("oboe.snd")
  add_mark(123, s1, 0)
  add_mark(321, s2, 0)
  set_verbose_cursor(true)
  delete_file("s61.rb")
  save_state("s61.rb")
  set_verbose_cursor(false)
  close_sound(s1)
  close_sound(s2)
  load("s61.rb")
  snd_display("save_state verbose_cursor?") unless verbose_cursor
  s1 = find_sound("oboe.snd", 0)
  s2 = find_sound("oboe.snd", 1)
  if (not sound?(s1)) or (not sound?(s2))
    snd_display("can\'t re-open sounds: %s %s?", s1, s2)
  else
    m1 = marks(s1)
    m2 = marks(s2)
    if m1.length != 1 or m2.length != 1 or m1.first.length != 1 or m2.first.length != 1
      snd_display("save_marks via save_state to: %s %s?", m1, m2)
    end
    samp1 = mark_sample(m1[0][0])
    samp2 = mark_sample(m2[0][0])
    if samp1 != 123 or samp2 != 321
      snd_display("save_marks via save_state posiitons: %s %s?", samp1, samp2)
    end
  end
  close_sound(s1) if sound?(s1)
  close_sound(s2) if sound?(s2)
  fd = open_sound("pistol.snd")
  if file = save_marks
    snd_display("save_marks no marks: %s?", file)
  end
  close_sound(fd)
  fd = open_sound("oboe.snd")
  load("oboe.marks")
  mlst = marks(fd, 0)
  snd_display("restore_marks: %s?", mlst) if mlst.length != 2
  if (res1 = mark_sample(mlst[0])) != 123 or (res2 = mark_sample(mlst[1])) != 12345
    snd_display("restored_marks: %s %s?", res1, res2)
  end
  close_sound(fd)
  fd = open_sound("oboe.snd")
  m1 = add_mark(1000)
  m2 = add_mark(2500)
  m3 = add_mark(frames() - 4000)
  ms = marks(fd, 0)
  src_sound(-0.5)
  if (res1 = marks(fd, 0)) != (res2 = marks(fd, 0, 0).reverse)
    snd_display("src rev marks: %s %s?", res1, res2)
  end
  if (res = (marks(fd, 0) or []).map do |m| mark_sample(m) end) != [7998, 96654, 99654]
    snd_display("src rev mark locs: %s?", res)
  end
  close_sound(fd)
  fd = open_sound("4.aiff")
  m1 = add_mark(1000, fd, 0)
  m2 = add_mark(2000, fd, 1)
  m3 = add_mark(3000, fd, 2)
  m4 = add_mark(4000, fd, 3)
  snd_display("marks (no args): %s?", marks) if marks.length.zero?
  save_marks(fd)
  close_sound(fd)
  fd = open_sound("4.aiff")
  load("4.marks")
  delete_file("4.marks")
  marks(fd).each_with_index do |mlst, i|
    if mlst.length != 1
      snd_display("save_marks[%d]: %s?", i, mlst)
    end
    if (res = mark_sample(mlst.first)) != (i + 1) * 1000
      snd_display("save_marks[%d] at %s?", i, res)
    end
  end
  close_sound(fd)
end

def test0210
  ind = open_sound("pistol.snd")
  samp1 = 1834
  samp2 = 8345
  m1 = add_mark(samp1, ind, 0)
  m2 = add_mark(samp2)
  src_sound(-1.0)
  snd_display("src -1 m1: %s?", mark_sample(m1)) if mark_sample(m1) != 39788
  snd_display("src -1 m2: %s?", mark_sample(m2)) if mark_sample(m2) != 33277
  undo_edit
  src_sound(0.5)
  snd_display("src 0.5 m1: %s?", mark_sample(m1)) if mark_sample(m1) != 2 * samp1
  snd_display("src 0.5 m2: %s?", mark_sample(m2)) if mark_sample(m2) != 2 * samp2
  undo_edit
  delete_samples(1000, 100)
  snd_display("delete_samples 100 m1: %s?", mark_sample(m1)) if mark_sample(m1) != samp1 - 100
  insert_silence(1000, 100)
  snd_display("insert_silence 100 m1: %s?", mark_sample(m1)) if mark_sample(m1) != samp1
  revert_sound(ind)
  delete_samples(2000, 100)
  snd_display("delete_samples (2) 100 m1: %s?", mark_sample(m1)) if mark_sample(m1) != samp1
  snd_display("delete_samples (2) 100 m2: %s?", mark_sample(m2)) if mark_sample(m2) != samp2 - 100
  insert_silence(2000, 100)
  snd_display("insert_silence (2) 100 m1: %s?", mark_sample(m1)) if mark_sample(m1) != samp1
  snd_display("insert_silence (2) 100 m2: %s?", mark_sample(m2)) if mark_sample(m2) != samp2
  revert_sound(ind)
  delete_samples(10000, 100)
  snd_display("delete_samples (3) 100 m1: %s?", mark_sample(m1)) if mark_sample(m1) != samp1
  snd_display("delete_samples (3) 100 m2: %s?", mark_sample(m2)) if mark_sample(m2) != samp2
  insert_silence(10000, 100)
  snd_display("insert_silence (3) 100 m1: %s?", mark_sample(m1)) if mark_sample(m1) != samp1
  snd_display("insert_silence (3) 100 m2: %s?", mark_sample(m2)) if mark_sample(m2) != samp2
  src_sound([0, 0.5, 1, 0.5, 2, 1])
  snd_display("src env 0.5 m1: %s?", mark_sample(m1)) if mark_sample(m1) != 2 * samp1
  snd_display("src env 0.5 m2: %s?", mark_sample(m2)) if mark_sample(m2) != 2 * samp2
  undo_edit
  reverse_sound
  snd_display("reverse_sound m1: %s?", mark_sample(m1)) if mark_sample(m1) != 39788
  snd_display("reverse_sound m2: %s?", mark_sample(m2)) if mark_sample(m2) != 33277
  undo_edit
  src_sound([0, -0.5, 1, -0.5, 2, -1])
  snd_display("src -env m1: %s?", mark_sample(m1)) if mark_sample(m1) != 68598
  snd_display("src -env m2: %s?", mark_sample(m2)) if mark_sample(m2) != 61160
  revert_sound(ind)
  # 
  src_channel(make_env(:envelope, [0, 0.5, 1, 1], :end, 8000), 2000, 10000)
  snd_display("src_channel (1) m1: %s?", mark_sample(m1)) if mark_sample(m1) != samp1
  snd_display("src_channel (1) m2: %s?", mark_sample(m2)) if mark_sample(m2) != 11345
  undo_edit
  src_channel(make_env(:envelope, [0, 0.5, 1, 1], :end, 8000), 0, 8000)
  snd_display("src_channel (2) m1: %s?", mark_sample(m1)) if mark_sample(m1) != 3303
  snd_display("src_channel (2) m2: %s?", mark_sample(m2)) if mark_sample(m2) != samp2
  undo_edit
  src_channel(make_env(:envelope, [0, 0.5, 1, 1], :end, 8000), 10000, 8000)
  snd_display("src_channel (3) m1: %s?", mark_sample(m1)) if mark_sample(m1) != samp1
  snd_display("src_channel (3) m2: %s?", mark_sample(m2)) if mark_sample(m2) != samp2
  close_sound(ind)
  #
  ind = open_sound("2.snd")
  set_sync(true, ind)
  m3 = add_mark(1000, ind, 0)
  m4 = add_mark(8000, ind, 1)
  swap_channels
  if (res1 = mark_home(m3)) != [ind, 1] or (res2 = mark_home(m4)) != [ind, 0]
    snd_display("swapped mark homes: %s %s?", res1, res2)
  end
  if (res1 = mark_sample(m3)) != 1000 or (res2 = mark_sample(m4)) != 8000
    snd_display("swapped mark samples: %s %s?", res1, res2)
  end
  close_sound(ind)
  #
  ind = open_sound("2.snd")
  set_sync(true, ind)
  m3 = add_mark(1000, ind, 0)
  delete_samples(1000, 10, ind, 1)
  swap_channels
  if (res = mark_home(m3)) != [ind, 1]
    snd_display("edited swapped mark home: %s?", res)
  end
  if (res = mark_sample(m3)) != 1000
    snd_display("edited swapped mark sample: %s?", res)
  end
  delete_marks
  close_sound(ind)
  #
  ind = open_sound("oboe.snd")
  m1 = add_mark(123, ind, 0) 
  m2 = add_mark(234, ind, 0)
  define_selection_via_marks(m1, m2)
  if (not selection?)
    snd_display("define_selection_via_marks failed?")
  else
    mc = selection_members
    snd_display("selection_members after mark def: %s [[%s, 0]]", mc, ind) if mc != [[ind, 0]]
    snd_display("selection_position 123: %s?", selection_position) if selection_position != 123
    snd_display("selection_frames 112: %s?", selection_frames) if selection_frames != 112
  end
  m1 = add_mark(1000, ind, 0) 
  m2 = add_mark(2000, ind, 0)
  define_selection_via_marks(m1, m2)
  if (not selection?)
    snd_display("define_selection_via_marks repeat failed?")
  else
    mc = selection_members
    snd_display("selection_members after 2nd mark def: %s [[%s, 0]]", mc, ind) if mc != [[ind, 0]]
    snd_display("selection_position 1000: %s?", selection_position) if selection_position != 1000
    snd_display("selection_frames 1001: %s?", selection_frames) if selection_frames != 1001
  end
  set_selection_member?(false, true)
  snd_display("can\'t clear selection via selection_members?") if selection?
  set_selection_member?(true, ind, 0)
  set_selection_position(2000, ind, 0)
  set_selection_frames(1234, ind, 0)
  snap_marks
  unless mark?(m1 = find_mark(2000, ind, 0))
    snd_display("snap_marks start: %s?", (marks(ind, 0) or []).map do |m| mark_sample(m) end)
  end
  unless mark?(m2 = find_mark(2000 + 1234, ind, 0))
    snd_display("snap_marks end: %s?", (marks(ind, 0) or []).map do |m| mark_sample(m) end)
  end
  set_selection_position(frames(ind, 0) + 1234, ind, 0)
  if (res = selection_position(ind, 0)) != frames(ind) - 1
    snd_display("selection_position past eof: %s %s?", res, frames(ind) - 1)
  end
  revert_sound(ind)
  src_sound([0, 0.5, 1, 1.75665])
  close_sound(ind)
  #
  ind = open_sound("oboe.snd")
  100.times do |i|
    current_marks = marks(ind, 0)
    current_samples = []
    if current_marks and (not current_marks.empty?)
      current_samples = current_marks.map do |m| mark_sample(m) end
      id = current_marks[irandom(current_marks.length - 1)]
      if find_mark(mark_sample(id)) != id
        snd_display("two marks at %s: %s?",
                    mark_sample(id), current_marks.map do |m| mark_sample(m) end)
      end
      if res = find_mark("not-a-mark")
        snd_display("find-bogus-mark: %s?", res)
      end
    end
    case irandom(15)
    when 0
      beg = irandom(frames)
      dur = [1, irandom(100)].max
      insert_silence(beg, dur)
      if current_marks and (not current_marks.empty?)
        current_marks.zip(current_samples) do |id, old_loc|
          if old_loc > beg
            if mark?(id)
              if (res = mark_sample(id)) != old_loc + dur
                snd_display("insert, mark %d %d -> %d (%d)?", id, old_loc, res, dur)
              end
            else
              snd_display("insert clobbered mark: %s?", id)
            end
          end
        end
      end
    when 1
      undo_edit if edits(ind, 0)[0] > 0
    when 2
      redo_edit if edits(ind, 0)[1] > 0
    when 3
      scale_channel((maxamp(ind, 0) > 0.1) ? 0.5 : 2.0)
      if (res = marks(ind, 0)) != current_marks
        snd_display("scaling changed marks: %s %s?", res, current_marks)
      end
      if (res = (marks(ind, 0) or []).map do |m| mark_sample(m) end) != current_samples
        snd_display("scaling changed mark locations: %s %s?", res, current_samples)
      end
    when 4
      set_sample(irandom(frames - 1), 0.5)
      if (res = marks(ind, 0)) != current_marks
        snd_display("set_sample changed marks: %s %s?", res, current_marks)
      end
      if (res = (marks(ind, 0) or []).map do |m| mark_sample(m) end) != current_samples
        snd_display("set_sample changed mark location: %s %s?", res, current_samples)
      end
    when 5
      beg = irandom(frames)
      dur = [1, irandom(100)].max
      len = beg + dur
      delete_samples(beg, dur)
      if current_marks and (not current_marks.empty?)
        current_marks.zip(current_samples) do |id, old_loc|
          if old_loc > beg and old_loc < len and mark?(id)
            snd_display("delete did not clobber mark: %s %s [%s, %s]?", id, old_loc, beg, dur)
          else
            if old_loc > len and (res = mark_sample(id)) != old_loc - dur
              snd_display("delete ripple mark, mark %d %d -> %d (%d)?", id, old_loc, res, dur)
            else
              if old_loc < beg and (res = mark_sample(id)) != old_loc
                snd_display("delete but mark before: %s %s %s %s?", id, old_loc, res, dur)
              end
            end
          end
        end
      end
    when 6
      revert_sound
    when 7
      if current_marks and current_marks.length > 1
        id = current_marks[irandom(current_marks.length - 1)]
        delete_mark(id)
        snd_display("delete_mark failed: %s?", id) if mark?(id)
        if (res = marks(ind, 0)).length != current_marks.length - 1
          snd_display("delete_mark list trouble: %s %s %s?", id, current_marks, res)
        end
      end
    when 8
      rate = (frames() > 200000) ? 2.0 : 0.5
      src_channel(rate)
      if current_marks and (not current_marks.empty?)
        current_marks.zip(current_samples) do |id, old_loc|
          unless mark?(id)
            snd_display("src_channel clobbered mark: %s?", id)
          else
            if ((old_loc / rate) - (res = mark_sample(id))).abs > 2
              snd_display("src_channel moved mark: %s?", id)
            end
          end
        end
      end
    when 9
      reverse_channel
      if current_marks and (not current_marks.empty?)
        current_marks.zip(current_samples) do |id, old_loc|
          unless mark?(id)
            snd_display("reverse_channel clobbered mark: %s?", id)
          else
            if ((frames() - old_loc) - (res = mark_sample(id))).abs > 2
              snd_display("reverse_channel moved mark: %s % % (%s)?",
                          id, old_loc, frames() - old_loc, res)
            end
          end
        end
      end
    else
      add_mark(irandom(frames() - 1))
    end
  end
  close_sound(ind)
  #
  defined?(mark_sync_color) and provided?(:xm) and mark_sync_color("blue")
  ind = open_sound("oboe.snd")
  m0 = add_mark(4321)
  delete_sample(100)
  m1 = add_mark(1234)
  val0 = describe_mark(m0)
  val1 = describe_mark(m1)
  if val0[0][1] != m0 or
      val0[0][3] != ind or
      val0[0][6] != 0 or
      val0[1] != 4321 or
      val0[2] != 4320
    snd_display("describe_mark m0: %s?", val0)
  end
  if val1[0][1] != m1 or
      val1[0][3] != ind or
      val1[0][6] != 0 or
      val1[1] != false or
      val1[2] != 1234
    snd_display("describe_mark m1: %s?", val1)
  end
  delete_mark(m0)
  delete_sample(5000)
  val0 = describe_mark(m0)
  val1 = describe_mark(m1)
  if val0[0][1] != m0 or
      val0[0][3] != ind or
      val0[0][6] != 0 or
      val0[1] != 4321 or
      val0[2] != false or
      val0[3] != false
    snd_display("describe_mark m0 (1): %s?", val0)
  end
  if val1[0][1] != m1 or
      val1[0][3] != ind or
      val1[0][6] != 0 or
      val1[1] != false or
      val1[2] != 1234 or
      val1[3] != 1234
    snd_display("describe_mark m1 (1): %s?", val1)
  end
  revert_sound(ind)
  $draw_mark_hook.add_hook!("snd-test") do |id| true end
  m0 = add_mark(4321)
  m1 = add_mark(1234)
  dur = frames(ind) / srate(ind).to_f
  pad_marks([m0, m1], 0.01)
  if fneq(res = frames(ind) / srate(ind).to_f, dur + 0.02)
    snd_display("pad_marks: %s %s?", dur, res)
  end
  if mark_sample(m0) != 4763 and mark_sample(m0) != 4761
    snd_display("pad_marks m0 pos: %s", mark_sample(m0))
  end
  if fneq(res = sample(1235), 0.0)
    snd_display("pad_marks 1235: %s?", res)
  end
  close_sound(ind)
  $draw_mark_hook.reset_hook!
  # 
  ind = open_sound("oboe.snd")
  if res = forward_mark
    snd_display("forward_mark when no marks: %s?", res)
  end
  if res = backward_mark
    snd_display("backward_mark when no marks: %s?", res)
  end
  if res = find_mark(12345)
    snd_display("find_mark when no mark: %s?", res)
  end
  m0 = add_mark(123, ind, 0)
  delete_sample(0)
  m1 = add_mark(23, ind, 0)
  set_mark_name(m1, "23")
  delete_sample(0)
  snd_display("can\'t find 00th mark") unless find_mark(123, ind, 0, 0)
  snd_display("can\'t find 01th mark") unless find_mark("23")
  snd_display("can\'t find 02th mark") unless find_mark(121)
  delete_mark(find_mark("23"))
  scale_by(2.0)
  m1 = add_mark(1234)
  set_mark_name(m1, "23")
  if (m10 = find_mark("23"))
    if (res = mark_sample(m10)) != 1234
      snd_display("mark 10th: %s?", res)
    end
  else
    snd_display("can\'t find 10th mark?")
  end
  if (m11 = find_mark("23", ind, 0, 1))
    if (res = mark_sample(m11, 1)) != 23
      snd_display("mark 11th: %s?", res)
    end
  else
    snd_display("can\'t find 11th mark?")
  end
  if (m12 = find_mark("23", ind, 0, 2))
    snd_display("can\'t find 12th mark: %s %s %s?", m12, mark_sample(m12, 2), mark_name(m12, 2))
  end
  close_sound(ind)
  with_file("forest.aiff") do |fsnd|
    ind = open_sound(fsnd)
    mark_loops
    if (res = (marks(ind, 0) or []).map do |m| mark_sample(m) end) != [24981, 144332]
      snd_display("forest marked loops: %s %s?", marks(ind, 0), res)
    end
    close_sound(ind)
  end
end

def test10
  clear_sincs
  test0010a unless provided? :snd_nogui
  test0010
  test0110
  test0210
end

if $test10 and $full_test or $snd_test == 10
  $before_test_hook.call(10)
  test10
  $after_test_hook.call(10)
end

# ---------------- test 11: dialogs ----------------

def string_equal_ignore_white_space(s1, s2)
  s1 == s2 or s1.delete(" \n") == s2.delete(" \n")
end

def test11
  snd_catch do peaks() end
  mus_audio_describe
  envd = enved_dialog
  cold = color_dialog
  ord  = orientation_dialog
  trd  = transform_dialog
  fild = view_files_dialog
  regd = view_regions_dialog
  pd   = print_dialog
  ehd  = snd_catch do edit_header_dialog end.first
  if (res = dialog_widgets[0]) != cold
    snd_display("color_dialog -> %s %s?", cold, res)
  end
  if (res = dialog_widgets[17]) != pd
    snd_display("print_dialog -> %s %s?", pd, res)
  end
  if (res = dialog_widgets[5]) != trd
    snd_display("transform_dialog -> %s %s?", trd, res)
  end
  if (res = dialog_widgets[19]) != regd
    snd_display("view_regions_dialog -> %s %s?", regd, res)
  end
  if (res1 = open_file_dialog(false)) != (res2 = dialog_widgets[6])
    snd_display("open_file_dialog -> %s %s?", res1, res2)
  end
  if (res1 = mix_file_dialog(false)) != (res2 = dialog_widgets[11])
    snd_display("mix_file_dialog -> %s %s?", res1, res2)
  end
  unless provided? :snd_gtk
    set_recorder_file("hiho.snd")
    unless string?(res = recorder_file)
      snd_display("set_recorder_file: %s?", res)
    end
    set_recorder_in_format(Mus_mulaw)
    if (res = recorder_in_format) != Mus_mulaw
      snd_display("set_recorder_in_format: %s?", res)
    end
    set_recorder_in_device(Mus_audio_line_in)
    if (res = recorder_in_device) != Mus_audio_line_in
      snd_display("set_recorder_in_device: %s?", res)
    end
    set_recorder_out_format(Mus_mulaw)
    if (res = recorder_out_format) != Mus_mulaw
      snd_display("set_recorder_out_format: %s?", res)
    end
    set_recorder_out_type(Mus_aifc)
    if (res = recorder_out_type) != Mus_aifc
      snd_display("set_recorder_out_type: %s?", res)
    end
    set_recorder_srate(44100)
    if (res = recorder_srate) != 44100
      snd_display("set_recorder_srate: %s?", res)
    end
    set_recorder_gain(0, 0.5)
    if fneq(res = recorder_gain(0), 0.5)
      snd_display("set_recorder_gain: %s?", res)
    end
    set_recorder_out_amp(0, 0.5)
    if ffneq(res = recorder_out_amp(0), 0.5)
      snd_display("set_recorder_out_amp: %s?", res)
    end
    set_recorder_in_amp(0, 0, 0.5)
    if ffneq(res = recorder_in_amp(0, 0), 0.5)
      snd_display("set_recorder_in_amp: %s?", res)
    end
  end
  held = help_dialog("Test", "snd-test here")
  if (res = menu_widgets.length) != 7
    snd_display("menu_widgets: %s?", res)
  end
  if (res = widget_position(menu_widgets[0])) != [0, 0]
    snd_display("position main menubar: %s?", res)
  end
  if (res = dialog_widgets[14]) != held
    snd_display("help_dialog -> %s %s?", held, res)
  end
  define_envelope("env4", [0, 1, 1, 0])
  save_envelopes("hiho.env")
  load("hiho.env")
  snd_display("save_envelopes: %s?", $env4) if $env4 != [0.0, 1.0, 1.0, 0.0]
  delete_file("hiho.env")
  help_dialog("test2", "this is the next test",
              ["string 1{open-sound}", "{env-sound}string2", "string{close-sound}3"],
              ["extsnd.html#sndopen", "extsnd.html#sndenv", "extsnd.html#sndclose"])
  dismiss_all_dialogs
  #
  ind = open_sound("oboe.snd")
  edit_header_dialog(ind)
  close_sound(ind)
  dismiss_all_dialogs
  if (res = snd_url(:open_sound)) != "extsnd.html#opensound"
    snd_display("snd_url :open_sound: %s?", res)
  end
  if (res = snd_url("open_sound")) != "extsnd.html#opensound"
    snd_display("snd_url \"open_sound\": %s?", res)
  end
  unless array?(res = snd_urls)
    snd_display("snd_urls: %s?", res)
  end
  str2 = snd_help(:open_sound)
  str3 = snd_help("open_sound")
  unless string_equal_ignore_white_space(str2, str3)
    snd_help("snd_help open_sound: %s %s?", str2, str3)
  end
  unless string?(snd_help(:open_soud))
    snd_display("snd_help :open_soud (misspelled on purpose) failed")
  end
  unless string_equal_ignore_white_space(res = snd_help(:enved_base), "enved_base):(envelope, editor, exponential, base, value, (1.0)")
    snd_display("snd_help :enved_base: %s", res)
  end
  unless string_equal_ignore_white_space(res = snd_help(:vu_font), "vu_font):(name, of, font, used, to, make, VU, meter, labels, (courier)")
    snd_display("snd_help :vu_font: %s", res)
  end
  unless string_equal_ignore_white_space(res = snd_help("enved_base"), "enved_base):(envelope, editor, exponential, base, value, (1.0)")
    snd_display("snd_help \"enved_base\": %s", res)
  end
  unless string_equal_ignore_white_space(res = snd_help("vu_font"), "vu_font):(name, of, font, used, to, make, VU, meter, labels, (courier)")
    snd_display("snd_help \"vu_font\": %s", res)
  end
  old_val = Hamming_window
  str1 = snd_help(:Hamming_window)
  str2 = snd_help("Hamming_window")
  if (not string_equal_ignore_white_space(str1, str2)) or
      (not string_equal_ignore_white_space(str1, "A raised cosine"))
    snd_display("snd_help Hamming_window: %s %s?", str1, str2)
  end
  if (not number?(Hamming_window)) or Hamming_window != old_val
    snd_display("snd_help clobbered out-of-module variable: %s %s?", old_val, Hamming_window)
  end
  #
  set_show_indices(true)
  ind = open_sound("oboe.snd")
  if sound_widgets(ind).length < 4
    snd_display("sound_widgets: %s?", sound_widgets(ind))
  end
  report_in_minibuffer("hi there", ind)
  if (res = widget_text(sound_widgets(ind)[3])) != "hi there"
    snd_display("report_in_minibuffer: %s?", res)
  end
  append_to_minibuffer("away!", ind)
  if (res = widget_text(sound_widgets(ind)[3])) != "hi thereaway!"
    snd_display("report_in_minibuffer 1: %s?", res)
  end
  if (res = widget_text(main_widgets[1]))
    snd_display("widget text should be false: %s?", res)
  end
  if (not (res1 = widget_text(sound_widgets(ind)[1]))) or
      res1 != (res2 = format("%d: %s", ind, short_file_name(ind)))
    snd_display("name text: %s %s?", res1, res2)
  end
  close_sound
  set_show_indices(false)
  #
  if provided?(:xm) and provided?(:snd_motif)
    snd_error("a test")
    errwid = dialog_widgets[3]
    if RWidget?(errwid)
      if RWidget?(ok = find_child(errwid, "OK"))
        RXtCallCallbacks(ok, RXmNactivateCallback, false)
      end
    else
      snd_display("snd_error no dialog?")
    end
  end
  #
  define_envelope("test_ramp", [0, 0, 1, 1])
  if $test_ramp != [0, 0, 1, 1]
    snd_display("define_envelope $test_ramp: %s?", $test_ramp)
  end
  define_envelope("test_ramp", [0, 1, 1, 0])
  if $test_ramp != [0, 1, 1, 0]
    snd_display("re-define-envelope $test_ramp: %s?", $test_ramp)
  end
  define_envelope("ramp32",  [0, 0, 1, 1],      32.000)
  define_envelope("ramp032", [0, 0, 1, 1],       0.032)
  define_envelope("ramp12",  [0, 0, 1, 1, 2, 0], 3.000)
  define_envelope("ramp012", [0, 0, 1, 1, 2, 0], 0.300)
  [[$ramp32,  "ramp32",  32.000],
   [$ramp032, "ramp032",  0.032],
   [$ramp12,  "ramp12",   3.000],
   [$ramp012, "ramp012",  0.300]].each do |e, name, base|
    if fneq(ebase = property(e.object_id, :envelope_base), base)
      snd_display("define_envelope %s base: %s %s?", name, base, ebase)
    end
  end
  #
  ind = new_sound("fmv.snd", Mus_aifc, Mus_bshort, 22050, 1, "define-envelope tests", 10)
  map_channel($init_channel)
  env_sound($ramp32)
  unless vequal(res = channel2vct, [0, 0.015, 0.037, 0.07, 0.118, 0.189, 0.293, 0.446, 0.67, 1])
    snd_display("define_envelope $ramp32 env_sound: %s?", res)
  end
  undo_edit
  env_channel($ramp032)
  unless vequal(res = channel2vct, [0, 0.328, 0.552, 0.705, 0.809, 0.88, 0.929, 0.962, 0.985, 1])
    snd_display("define_envelope $ramp032 env_sound: %s?", res)
  end
  undo_edit
  close_sound(ind)
end

if (not provided?(:snd_nogui)) and ($test11 and $full_test or $snd_test == 11)
  $before_test_hook.call(11)
  test11
  $after_test_hook.call(11)
end

# ---------------- test 12: extensions ----------------

def spectral_difference(snd1, snd2)
  size = [frames(snd1), frames(snd2)].max
  pow2 = (log(size) / log(2)).ceil
  fftlen = (2 ** pow2).to_i
  fdr1 = make_vct(fftlen)
  fdr2 = make_vct(fftlen)
  samples2vct(0, fftlen, snd1, 0, fdr1)
  samples2vct(0, fftlen, snd2, 0, fdr2)
  spectr1 = snd_spectrum(fdr1, Blackman2_window, fftlen, true)
  spectr2 = snd_spectrum(fdr2, Blackman2_window, fftlen, true)
  diff = 0.0
  diffs = spectr1.subtract(spectr2)
  diffs.each do |d| diff += d.abs end
  diff
end

def test_spectral_difference(snd1, snd2, maxok)
  s1 = open_sound(snd1)
  s2 = open_sound(snd2)
  if (not sound?(s1)) or (not sound?(s2))
    snd_display("open_sound %s or %s failed?", snd1, snd2)
  end
  diff = spectral_difference(s1, s2)
  close_sound(s1)
  close_sound(s2)
  if diff > maxok
    snd_display("translate spectral difference %s %s: %s > %s?", snd1, snd2, diff, maxok)
  end
end

def test12
  sf_dir_files = []
  if string?($sf_dir)
    sound_files_in_directory($sf_dir).each do |file|
      dir = $sf_dir + file
      snd_catch(:mus_error) do
        mus_sound_chans(dir).between?(1, 255) and
          mus_sound_data_format(dir) >= 0 and
          mus_sound_srate(dir) > 0 and
          mus_sound_frames(dir) >= 0 and
          sf_dir_files.push(dir)
      end
    end
  end
  open_files = []
  open_ctr = 0
  add_sound_file_extension("wave")
  until open_ctr == 32
    len = open_files.length
    open_chance = (8 - len) * 0.125
    close_chance = len * 0.125
    if len.zero? or random(1.0) > 0.5
      name = sf_dir_files[random(sf_dir_files.length).floor]
      ht = snd_catch(:all, 0) do mus_sound_header_type(name) end.first
      df = snd_catch(:all, 0) do mus_sound_data_format(name) end.first
      fd = if ht == Mus_raw or ht == Mus_unsupported or df == Mus_unknown
             -1
           else
             snd_catch(:all, -1) do view_sound(name) end.first or -1
           end
      if fd != -1
        open_ctr += 1
        open_files.push(fd)
      end
    else
      if len > 0 and random(1.0) > 0.3
        fd = open_files[random(open_files.length).floor]
        close_sound(fd)
        open_files.delete_if do |a| a == fd end
      end
    end
  end
  open_files.each do |s| close_sound(s) end
  open_files = []
  if sounds2array.length != 0
    snd_display("active_sounds: %s %s?",
                sounds.inspect, sounds2array.map do |s| short_file_name(s) end)
  end
  fd = open_raw_sound(:file, $sf_dir + "addf8.nh",
                      :channels, 1, :srate, 8012, :data_format, Mus_mulaw)
  if data_format(fd) != Mus_mulaw
    snd_display("open_raw_sound: %s?", mus_data_format_name(data_format(fd)))
  end
  close_sound(fd)
  #
  $bad_header_hook.reset_hook!
  with_time("test_spectral_difference(oboe.snd, oboe.g723_24, 20)") do
    test_spectral_difference("oboe.snd", $sf_dir + "oboe.g723_24", 20.0)
  end
  test_spectral_difference("oboe.snd", $sf_dir + "oboe.g723_40", 3.0)
  test_spectral_difference("oboe.snd", $sf_dir + "oboe.g721", 6.0)
  test_spectral_difference($sf_dir + "o2.wave", $sf_dir + "o2_dvi.wave", 10.0)
  test_spectral_difference($sf_dir + "wood.riff", $sf_dir + "wood.sds", 4.0)
  test_spectral_difference($sf_dir + "nist-10.wav", $sf_dir + "nist-shortpack.wav", 1.0)
  $bad_header_hook.add_hook!("snd-test") do |n| true end
  #
  # dangling readers (overall)
  #
  ind = open_sound("oboe.snd")
  hi = make_sample_reader(0, ind, 0)
  close_sound(ind)
  snd_display("dangling reader: %s?", hi) unless sample_reader?(hi)
  snd_display("dangling reader format: %s?", hi) unless string?(hi.to_s)
  val = hi.call
  val1 = next_sample(hi)
  val2 = previous_sample(hi)
  val3 = read_sample(hi)
  if fneq(val, 0.0) or fneq(val1, 0.0) or fneq(val2, 0.0) or fneq(val3, 0.0)
    snd_display("dangling read: %s %s %s %s?", val, val1, val2, val3)
  end
  if res = sample_reader_home(hi)
    snd_display("dangling reader home: %s?", res)
  end
  if (res = sample_reader_position(hi)).nonzero?
    snd_display("dangling sample_reader_position: %s?", res)
  end
  unless (res = sample_reader_at_end?(hi))
    snd_display("dangling reader eof: %s?", res)
  end
  free_sample_reader(hi)
  #
  # same (pruned edit)
  #
  ind = open_sound("oboe.snd")
  delete_samples(100, 100)
  hi = make_sample_reader(0, ind, 0)
  revert_sound
  delete_samples(100, 100)
  snd_display("pruned dangling reader: %s?", hi) unless sample_reader?(hi)
  snd_display("pruned dangling reader format: %s?", hi) unless string?(hi.to_s)
  val = hi.call
  val1 = next_sample(hi)
  val2 = previous_sample(hi)
  val3 = read_sample(hi)
  if fneq(val, 0.0) or fneq(val1, 0.0) or fneq(val2, 0.0) or fneq(val3, 0.0)
    snd_display("pruned dangling read: %s %s %s %s?", val, val1, val2, val3)
  end
  if (res = sample_reader_home(hi)) != [ind, 0]
    snd_display("pruned dangling reader home: %s?", res)
  end
  unless (res = sample_reader_at_end?(hi))
    snd_display("pruned dangling reader eof: %s?", res)
  end
  free_sample_reader(hi)
  close_sound(ind)
  #
  # region reader
  # 
  ind = open_sound("oboe.snd")
  reg = make_region(1000, 2000, ind, 0)
  rd = make_region_sample_reader(0, reg)
  snd_display("region_sample_reader mix: %s?", rd) if mix_sample_reader?(rd)
  snd_display("region_sample_reader region: %s?", rd) unless region_sample_reader?(rd)
  snd_display("region_sample_reader track: %s?", rd) if track_sample_reader?(rd)
  snd_display("region_sample_reader normal: %s?", rd) if sample_reader?(rd)
  if (res = sample_reader_position(rd)).nonzero?
    snd_display("region_sample_reader position: %s?", res)
  end
  if (res = sample_reader_home(rd)) != [reg, 0]
    snd_display("region_sample_reader home: %s?", res)
  end
  if (res = sample_reader_at_end?(rd))
    snd_display("region_sample_reader_at_end?: %s?", res)
  end
  val = rd.call
  snd_display("region_sample_reader at start: %s?", val) if fneq(val, 0.0328)
  unless string?(res = rd.to_s)
    snd_display("region_sample_reader: %s?", res)
  end
  close_sound(ind)
  val = next_sample(rd)
  snd_display("region_sample_reader at 1: %s?", val) if fneq(val, 0.0348)
  forget_region(reg)
  val = read_sample(rd)
  snd_display("region_sample_reader at end: %s?", val) if fneq(val, 0.0)
  snd_display("region_sample_reader after deletion?") unless sample_reader_at_end?(rd)
  free_sample_reader(rd)
  #
  # mix reader
  #
  save_md = 0
  mix_click_sets_amp
  ind = open_sound("oboe.snd")
  reg = make_region(1000, 2000, ind, 0)
  md = mix_region(0, reg, ind, 0)
  rd = make_mix_sample_reader(md)
  set_mix_property(:hi, "hi", md)
  save_md = md
  if (res = mix_property(:hi, md)) != "hi"
    snd_display("mix_property: %s?", res)
  end
  val = rd.call
  snd_display("mix_sample_reader at start: %s?", val) if fneq(val, 0.0328)
  unless string?(res = rd.to_s)
    snd_display("mix_sample_reader: %s?", res)
  end
  close_sound(ind)
  if (res = snd_catch do mix_property(:hi, md) end).first != :no_such_mix
    snd_display("mix_property bad mix: %s", res.inspect)
  end
  if (res = rd.to_s) != "#<mix-sample-reader: inactive>"
    snd_display("mix_sample_reader released: %s?", res)
  end
  val = read_mix_sample(rd)
  snd_display("mix_sample_reader at end: %s?", val) if fneq(val, 0.0)
  free_sample_reader(rd)
  #
  # track reader
  #
  ind = open_sound("oboe.snd")
  reg = make_region(1000, 2000, ind, 0)
  md = mix_region(0, reg, ind, 0)
  trk = make_track
  set_mix_track(md, trk)
  rd = make_track_sample_reader(trk)
  val = rd.call
  snd_display("track_sample_reader at start: %s?", val) if fneq(val, 0.0328)
  unless string?(res = rd.to_s)
    snd_display("track_sample_reader: %s?", res)
  end
  close_sound(ind)
  if (res = snd_catch do mix_property(:hi, save_md) end).first != :no_such_mix
    snd_display("mix_property bad mix: %s", res.inspect)
  end
  unless string?(res = rd.to_s)
    snd_display("track_sample_reader released: %s?", res)
  end
  val = read_track_sample(rd)
  snd_display("track_sample_reader at end: %s?", val) if fneq(val, 0.0)
  free_sample_reader(rd)
  #
  [:mix_amp,
   :mix_tag_position,
   :mix_chans,
   :mix_track,
   :mix_frames,
   :mix_locked?,
   :mix_position,
   :mix_home,
   :mix_speed,
   :mix_tag_y].each do |func_sym|
    if (res = snd_catch do send(func_sym, md) end).first != :no_such_mix
      snd_display("%s: %s", func_sym, res.inspect)
    end
  end
  $mix_click_hook.reset_hook!
  $close_hook.reset_hook!
  #
  ind = open_sound("oboe.snd")
  reg = make_region(1000, 2000, ind, 0)
  md1 = mix_region(0, reg, ind, 0)
  md2 = mix_region(1000, reg, ind, 0)
  trk = make_track(md1, md2)
  rd = make_track_sample_reader(trk)
  val = rd.call
  snd_display("track_sample_reader (1) at start: %s?", val) if fneq(val, 0.0328)
  unless string?(res = rd.to_s)
    snd_display("track_sample_reader (1): %s?", res)
  end
  undo_edit(1)
  delete_sample(5000)
  unless string?(res = rd.to_s)
    snd_display("track_sample_reader (1) released: %s?", res)
  end
  val = read_track_sample(rd)
  snd_display("track_sample_reader (1) at end: %s?", val) if fneq(val, 0.0348)
  set_with_mix_tags(false)
  md1 = mix_region(0, reg)
  snd_display("mix_region + false tags: %s?", md1) if md1 != -1
  set_with_mix_tags(true)
  close_sound(ind)
  unless string?(res = rd.to_s)
    snd_display("track_sample_reader (2) released: %s?", res)
  end
  val = read_track_sample(rd)
  snd_display("track_sample_reader (2) at end: %s?", val) if fneq(val, 0.0)
  free_sample_reader(rd)
  #
  sfiles = []
  ffiles = []
  each_sound_file($sf_dir) do |file|
    snd_catch do
      if mus_sound_chans(file) > 16
        ffiles.push(file)
      end
    end
  end
  map_sound_files($sf_dir) do |file|
    snd_catch do
      if mus_sound_chans(file) > 16
        sfiles.push(file)
      end
    end
  end
  if ffiles != [$sf_dir + "s24.snd"] or sfiles != [$sf_dir + "s24.snd"]
    snd_display("map|for_each_sound_file(s): %s %s?", ffiles, sfiles)
  end
end

if (not provided?(:snd_nogui)) and ($test12 and $full_test or $snd_test == 12)
  $before_test_hook.call(12)
  test12
  $after_test_hook.call(12)
end

# ---------------- test 13: menus, edit lists, hooks, etc ----------------

def execute_and_wait(*cmd)
  val = false
  File.popen(format(*cmd)) do |f|
    until val == true or f.eof or c_g?
      val = f.getc
    end
  end
  val
end

def loop_through_files(description, select, &make_cmd)
  data = if select
           selection_to_temps(Mus_next, Mus_out_format)
         else
           sound_to_temps(Mus_next, Mus_out_format)
         end
  stopped = false
  output_names = make_array(data.length) do |i|
    break if stopped
    outname = format("/tmp/snd-test-%d.snd", random(1.0).object_id)
    stopped = execute_and_wait(make_cmd.call(data[i], outname))
    outname
  end
  if select
    temps_to_selection(data, output_names, description)
  else
    temps_to_sound(data, output_names, description)
  end
end

def copyfile_1(select)
  loop_through_files("(cp)", select) do |input, output|
    "cp " + input + " " + output
  end
end

def carg0(hook)
  hook.call
end

def carg1(hook)
  hook.call(1)
end

def carg2(hook)
  hook.call(1, 2)
end

def carg3(hook)
  hook.call(1, 2, 3)
end

def carg4(hook)
  hook.call(1, 2, 3, 4)
end

def carg5(hook)
  hook.call(1, 2, 3, 4, 5)
end

def carg6(hook)
  hook.call(1, 2, 3, 4, 5, 6)
end

def carg7(hook)
  hook.call(1, 2, 3, 4, 5, 6, 7)
end

def harg0
  32
end

def harg1(a)
  a + 32
end

def harg2(a, b)
  a + b + 32
end

def harg3(a, b, c)
  a + b + c + 32
end

def harg4(a, b, c, d)
  a + b + c + d + 32
end

def harg5(a, b, c, d, e)
  a + b + c + d + e + 32
end

def harg6(a, b, c, d, e, f)
  a + b + c + d + e + f + 32
end

def harg7(a, b, c, d, e, f, g)
  a + b + c + d + e + f + g + 32
end

def test_hooks
  funcs = [[:harg0, :carg0, 32],
           [:harg1, :carg1, 33],
           [:harg2, :carg2, 35],
           [:harg3, :carg3, 38],
           [:harg4, :carg4, 42],
           [:harg5, :carg5, 47],
           [:harg6, :carg6, 53],
           [:harg7, :carg7, 60]]
  Snd_hooks.each do |hook|
    if hook? hook
      if hook.arity.between?(0, 7)
        vals = funcs[hook.arity]
        hook.add_hook!(get_func_name, &method(vals[0]))
        if (res = send(vals[1], hook)) != vals[2]
          snd_display("hook.call: %s (%s) %s?", res, vals[2], hook.inspect)
        end
      else
        snd_display("hook arity: %s %s?", hook.arity, hook.inspect)
      end
    end
  end
  reset_all_hooks
  Snd_hooks.each do |hook|
    if hook? hook
      snd_display("%s not empty?", hook.inspect) unless hook.empty?
    end
  end
end

def test_menus
  each_child(menu_widgets.first) do |w|
    unless RXmIsRowColumn(w)
      each_child(RXtGetValues(w, [RXmNsubMenuId, 0])[1]) do |menu|
        if RXmIsPushButton(menu) and RXtIsSensitive(menu) and
            (not ["Exit", "New", 
                  "Save   C-x C-s", 
                  "Close  C-x k",
                  "Save options",
                  "Mixes", "clm", "fm-violin"].member?(RXtName(menu)))
          RXtCallCallbacks(menu, RXmNactivateCallback, snd_global_state)
        end
      end
    end
  end
  sounds2array.each do |snd| close_sound(snd) end
  dismiss_all_dialogs
end

def test0013
  fd = view_sound("oboe.snd")
  mb = add_to_main_menu("clm")
  if (res = snd_catch do
        add_to_menu(-1, "fm-violin", lambda do | | false end)
      end).first != :no_such_menu
    snd_display("add_to_menu bad menu: %s", res.inspect)
  end
  if (res = snd_catch do add_to_main_menu("oops", make_delay(11)) end).first != :bad_arity
    snd_display("add_to_main_menu non-thunk: %s", res.inspect)
  end
  # returns :wrong_type_arg not :bad_arity
  if (res = snd_catch do add_to_menu(3, "oops", make_delay(12)) end).first != :wrong_type_arg
    snd_display("add_to_menu non-thunk: %s", res.inspect)
  end
  set_cursor(2000, fd)
  set_transform_graph_type(Graph_once)
  set_transform_graph?(true, fd)
  unless provided? :snd_nogui
    add_to_menu(mb, "not here", lambda do | | snd_display("oops") end)
    remove_from_menu(mb,"not here")
    add_to_menu(3, "Denoise", lambda do | | report_in_minibuffer("denoise") end)
  end
  $help_hook.reset_hook!
  hi = snd_help(:cursor_position)
  $help_hook.add_hook!("snd-test") do |a, b|
    if a != "cursor_position"
      snd_display("$help_hook subject: %s?", a)
    end
    if b != "(cursor-position (snd #f) (chn #f)): current cursor position (x y in pixels) in snd's channel chn"
      snd_display("$help_hook text: %s?", b)
    end
    "hiho:" + b
  end
  ho = snd_help(:cursor_position)
  if ho.length != hi.length + 5
    snd_display("$help_hook %s -> %s?", hi, ho)
  end
  $help_hook.reset_hook!
  $help_hook.add_hook!("snd-test") do |a, b| false end
  ho = snd_help(:cursor_position)
  snd_display("$help_hook false: %s %s?", hi, ho) if hi != ho
  $help_hook.reset_hook!
  $mark_drag_triangle_hook.reset_hook!
  if $mark_drag_triangle_hook.member?("mdt-test")
    snd_display("mdt-test is member of %s?", $mark_drag_triangle_hook.inspect)
  end
  $mark_drag_triangle_hook.add_hook!("mdt-test") do |a, b, c, d| false end
  unless $mark_drag_triangle_hook.member?("mdt-test")
    snd_display("mdt-test is not member of %s?", $mark_drag_triangle_hook.inspect)
  end
  $mark_drag_triangle_hook.reset_hook!
  # 
  fr = frames(fd)
  chn = chans(fd)
  sr = srate(fd)
  mx = maxamp(fd)
  copyfile_1(false)
  if (res = edit_fragment) != ["(cp)", "set", 0, 50828]
    snd_display("copyfile_1: %s?", res)
  end
  if fr != frames(fd) or chn != chans(fd) or fneq(mx, maxamp(fd)) or fneq(sr, srate(fd))
    snd_display("copyfile_1 (1): %s %s %s %s?", frames(fd), chans(fd), srate(fd), maxamp(fd))
  end
  eds = edits
  preload_file("oboe.snd")
  preload_directory(".")
  select_all
  copyfile_1(true)
  if (res = edit_fragment) != ["(cp)", "set", 0, 50828]
    snd_display("copyfile_1 (select): %s?", res)
  end
  if (res = edits) != [eds[0] + 1, eds[1]]
    snd_display("copyfile_1 (select eds): %s %s?", eds, res)
  end
  if fr != frames(fd) or chn != chans(fd) or fneq(mx, maxamp(fd)) or fneq(sr, srate(fd))
    snd_display("copyfile_1 (2): %s %s %s %s?", frames(fd), chans(fd), srate(fd), maxamp(fd))
  end
  #
  set_transform_size(256, fd, 0)
  dpys = [Graph_once, Graph_as_sonogram, Graph_as_spectrogram] * 2
  ffts = [Fourier_transform] * 3
  ffts += [Autocorrelation] * 3
  dpys.zip(ffts) do |dpy_type, fft_type|
    set_transform_graph_type(dpy_type, fd, 0)
    set_transform_type(fft_type, fd, 0)
    update_transform_graph(fd, 0)
    transform2vct(fd, 0)
  end
  if (res = snd_catch do transform_sample(5000, 0, fd, 0) end).first != :no_such_sample
    snd_display("access invalid (bin) transform sample: %s", res.inspect)
  end
  if (res = snd_catch do transform_sample(0, 5000, fd, 0) end).first != :no_such_sample
    snd_display("access invalid (slice) transform sample: %s", res.inspect)
  end
  close_sound(fd)
  set_transform_type(Fourier_transform)
  $after_open_hook.add_hook!("snd-test") do |snd|
    set_x_axis_style(X_axis_in_samples, snd, true)
  end
  fd = open_sound("2.snd")
  close_sound(fd)
  $after_open_hook.reset_hook!
  $after_open_hook.add_hook!("snd-test") do |snd|
    set_x_axis_style(X_axis_in_percentage, snd, true)
  end
  $initial_graph_hook.add_hook!("snd-test") do |snd, chn, dur|
    if mus_sound_maxamp_exists?(file_name(snd))
      amp_vals = mus_sound_maxamp(file_name(snd))
      max_val = amp_vals[chn * 2 + 1]
      [0.0, dur, -max_val, max_val]
    else
      [0.0, dur, -1.0, 1.0]
    end
  end
  $after_open_hook.reset_hook!
  $initial_graph_hook.reset_hook!
  $initial_graph_hook.add_hook!("snd-test") do |snd, chn, dur|
    [0.0, dur, -1.0, 1.0, "a label", -4.0, 4.0]
  end
  fd = open_sound("2.snd")
  if defined? axis_info                           # snd-nogui hasn't axis_info
    ax = axis_info
    if array?(ax) and (fneq(ax[2], 0.0) or
                         fneq(ax[3], -1.0) or
                         fneq(ax[4], mus_sound_duration("2.snd")) or
                         fneq(ax[5], 1.0) or
                         fneq(ax[6], 0.0) or
                         fneq(ax[7], -4.0) or
                         fneq(ax[8], mus_sound_duration("2.snd")) or
                         fneq(ax[9], 4.0))
      snd_display("$initial_graph_hook with ymin/max: %s?", ax)
    end
  end
  $initial_graph_hook.reset_hook!
  set_selection_position(1000, fd, 1)
  set_selection_frames(10, fd, 1)
  set_selection_member?(true, fd, 1)
  snd_display("chan 0 is selection_member?") if selection_member?(fd, 0)
  2.times do |chn|
    set_selection_position(1000, fd, chn)
    set_selection_frames(10, fd, chn)
    set_selection_member?(true, fd, chn)
  end
  scale_selection_to([0.5, 0.25].to_vct)
  if fneq(maxamp(fd, 0), 0.5) or fneq(maxamp(fd, 1), 0.25)
    snd_display("scale_selection_to with vector: %s?", maxamp(fd, true))
  end
  close_sound(fd)
  #
  fd = open_sound("obtest.snd")
  if defined? window_property
    names = short_file_name(true)
    if provided?(:xm) then RXSynchronize(RXtDisplay(main_widgets[1]), true) end
    set_window_property("SND_VERSION", "WM_NAME",
                        format("snd (%s)%s",
                               Time.now.localtime.strftime("%d-%b %H:%M %Z"),
                               (names ? names.inspect : "")))
    gotit = false
    old_size = vu_size
    $window_property_changed_hook.add_hook!("snd-test") do |hi|
      gotit = true
      false
    end
    set_window_property("SND_VERSION", "SND_COMMAND", "set_vu_size(0.5)")
    if (res = window_property("SND_VERSION", "SND_COMMAND")) != "set_vu_size(0.5)"
      snd_display("window_property: %s?", res)
    end
    $window_property_changed_hook.reset_hook!
    set_window_property("SND_VERSION", "SND_COMMAND", "make_array(10, 3.14)")
    if provided?(:xm) then RXSynchronize(RXtDisplay(main_widgets[1]), false) end
    if (not gotit) or fneq(vu_size, 0.5)
      snd_display("set_window_property: gotit: %s vu_size (should be 0.5): %s?", gotit, vu_size)
    end
    set_vu_size(old_size)
  end
  if hook? $new_widget_hook
    ctr = 0
    added = 0
    $close_hook.reset_hook!
    set_with_background_processes(true)
    set_vu_size(1.25)
    $new_widget_hook.add_hook!("snd-test") do |w| added += 1 end
    if provided? :snd_motif
      snd_catch do test_menus end
    end
    dismiss_all_dialogs
    $close_hook.reset_hook!
    sounds2array.each do |snd| close_sound(snd) end
    if sound? fd
      snd_display("close all didn\'t: %s %s %s %s %s?",
                  fd, sound?(fd), short_file_name(fd), $close_hook.inspect, sounds)
      close_sound(fd)
    end
    fd = open_sound("obtest.snd")
    set_with_background_processes(false)
    snd_display("no widgets added?") if added.zero?
    $new_widget_hook.reset_hook!
    close_sound(fd)
  end
  sounds2array.each do |snd| close_sound(snd) end
  test_hooks
  $bad_header_hook.add_hook!("snd-hook") do |n| true end
  ind = open_sound("oboe.snd")
  set_cursor(2000)
  key(?u, 4, ind)
  key(?1, 0, ind)
  key(?0, 0, ind)
  key(?0, 0, ind)
  key(?x, 4, ind)
  key(?z, 4, ind)
  if (res = edit_fragment) != ["smooth_channel(2000, 100", "set", 2000, 100]
    snd_display("C-x C-z fragment: %s?", res)
  end
  unless vequal(res = samples2vct(2010, 10),
                [0.064, 0.063, 0.063, 0.062, 0.062, 0.061, 0.060, 0.059, 0.059, 0.058].to_vct)
    snd_display("C-x C-z samps: %s?", res)
  end
  set_cursor(0)
  select_all
  key(?x, 4, ind)
  key(?o, 0, ind)
  key(?-, 4, ind)
  key(?x, 4, ind)
  key(?o, 0, ind)
  key(?x, 4, ind)
  key(?o, 0, ind)
  key(?x, 4, ind)
  key(?p, 0, ind)
  set_selection_member?(false, true)
  revert_sound(ind)
  set_search_procedure(ind, lambda do |n4| n4 > 0.1 end)
  key(?a, 4, ind, 0)
  snd_display("C-a cursor: %s?", cursor(ind, 0)) if cursor(ind, 0).nonzero?
  key(?s, 4, ind, 0)
  key(?s, 4, ind, 0)
  snd_display("search_procedure C-s C-s cursor: %s?", cursor(ind, 0)) if cursor(ind, 0) != 4423
  unless provided? :snd_nogui
    if (res = widget_text(sound_widgets(ind)[3])) != "y = .101 at .201 (4423)"
      snd_display("C-s 4423 report_in_minibuffer: %s?", res)
    end
  end
  set_search_procedure(ind, lambda do |n| n > 0.2 end)
  set_cursor(0, ind, 0)
  key(?s, 4, ind, 0)
  key(?s, 4, ind, 0)
  if cursor(ind, 0).nonzero?
    snd_display("search_procedure C-s C-s cursor failed: %s?", cursor(ind, 0))
  end
  unless provided? :snd_nogui
    if (res = widget_text(sound_widgets(ind)[3])) != "not found" or res != "not found (wrapped)"
      snd_display("C-s failure report_in_minibuffer: %s?", res)
    end
  end
  snd = chn = 0
  edit_hook(ind, 0).reset_hook!
  edit_hook(ind, 0).add_hook!("snd-test") do | | snd + chn end
  edit_hook(ind, 0).reset_hook!
  after_edit_hook(ind, 0).reset_hook!
  after_edit_hook(ind, 0).add_hook!("snd-test") do | | snd + chn end
  after_edit_hook(ind, 0).reset_hook!
  undo_hook(ind, 0).reset_hook!
  undo_hook(ind, 0).add_hook!("snd-test") do | | snd + chn end
  undo_hook(ind, 0).reset_hook!
  calls = 0
  undo_hook(ind, 0).add_hook!("snd-test") do | | calls += 1 end
  delete_sample(0, ind, 0)
  undo_edit(1)
  redo_edit(1)
  revert_sound(ind)
  snd_display("undo_hook called %s times (3)?", calls) if calls != 3
  undo_hook(ind, 0).reset_hook!
  close_sound(ind)
end

def test0113
  $open_raw_sound_hook.reset_hook! unless $open_raw_sound_hook.empty?
  $open_raw_sound_hook.add_hook!("snd-hook") do |file, choice| [1, 22050, Mus_bshort] end
  with_file("addf8.nh") do |file|
    ind = open_sound(file)
    play_and_wait(0, ind)
    $open_raw_sound_hook.reset_hook!
    if (res = [chans(ind), srate(ind), data_format(ind), frames(ind)]) \
      != [1, 22050, Mus_bshort, 23808]
      snd_display("open_raw: %s?", res)
    end
    set_search_procedure(ind, lambda do |n| n > 0.2 end)
    close_sound(ind)
  end
  save_as_dialog = true
  save_as_name = "hiho"
  save_as_index = -1
  $after_save_as_hook.reset_hook!
  $after_save_as_hook.add_hook!("snd-test") do |ind, name, dial|
    save_as_index = ind
    save_as_name = name
    save_as_dialog = dial
  end
  ind = open_sound("oboe.snd")
  save_sound_as("test.snd", ind, Mus_raw)
  close_sound(ind)
  $open_raw_sound_hook.reset_hook!
  $after_save_as_hook.reset_hook!
  snd_display("$after_save_as_hook dialog: %s?", save_as_dialog) if save_as_dialog
  snd_display("$after_save_as_hook index: %s (%s)?", save_as_index, ind) if save_as_index != ind
  if Dir.pwd + "/test.snd" != save_as_name
    snd_display("$after_save_as_hook name: %s?", save_as_name)
  end
  $open_raw_sound_hook.add_hook!("snd-hook-1") do |file, choice|
    if File.basename(file) != "test.snd"
      snd_display("$open_raw_sound_hook file: %s?", file)
    end
    unless choice == false
      snd_display("$open_raw_sound_hook choice 1: %s?", choice)
    end
    [2, 44100, Mus_mulaw]
  end
  ind = open_sound("test.snd")
  if (res = [header_type(ind), data_format(ind), chans(ind), srate(ind), frames(ind)]) \
    != [Mus_raw, Mus_mulaw, 2, 44100, 50828]
    snd_display("$open_raw_sound_hook 1: %s?", res)
  end
  close_sound(ind)
  $open_raw_sound_hook.add_hook!("snd-hook-2") do |file, choice|
    if choice != [2, 44100, Mus_mulaw]
      snd_display("$open_raw_sound_hook choice 2: %s?", choice)
    end
    [1, 22050, Mus_lint]
  end
  ind = open_sound("test.snd")
  if (res = [header_type(ind), data_format(ind), chans(ind), srate(ind), frames(ind)]) \
    != [Mus_raw, Mus_lint, 1, 22050, 50828 / 2]
    snd_display("$open_raw_sound_hook 2: %s?", res)
  end
  close_sound(ind)
  $open_raw_sound_hook.reset_hook!
  $open_raw_sound_hook.add_hook!("snd-hook-2") do |file, choice|
    [2]
  end
  ind = open_sound("test.snd")
  if (res = [header_type(ind), data_format(ind), chans(ind), srate(ind)]) \
    != [Mus_raw, Mus_lint, 2, 22050]
    snd_display("$open_raw_sound_hook 3: %s?", res)
  end
  close_sound(ind)
  $open_raw_sound_hook.reset_hook!
  $open_raw_sound_hook.add_hook!("snd-hook-2") do |file, choice|
    [1, 22050, Mus_bshort, 120, 320]
  end
  ind = open_sound("test.snd")
  if (res = [header_type(ind), data_format(ind), chans(ind), srate(ind),
             data_location(ind), data_size(ind), frames(ind)]) \
    != [Mus_raw, Mus_bshort, 1, 22050, 120, 320, 160]
    snd_display("$open_raw_sound_hook 4: %s?", res)
  end
  close_sound(ind)
  $open_raw_sound_hook.reset_hook!
  #
  $during_open_hook.reset_hook!
  ind = open_sound("oboe.snd")
  mx0 = maxamp(ind)
  save_sound_as("test.snd", ind, Mus_next, Mus_bfloat)
  close_sound(ind)
  $during_open_hook.add_hook!("snd-test") do |fd, name, reason|
    case mus_sound_data_format(name)
    when Mus_bfloat, Mus_bdouble, Mus_lfloat, Mus_ldouble
      set_mus_file_prescaler(fd, 4.0)
    else
      snd_display("not set_mus_file_prescaler (data_format: %s)",
                  mus_data_format_name(mus_sound_data_format(name)))
    end
  end
  ind1 = open_sound("test.snd")
  mx1 = maxamp(ind1)
  snd_display("set_mus_file_prescaler: %s -> %s?", mx0, mx1) if fneq(mx1, mx0 * 4.0)
  close_sound(ind1)
  $during_open_hook.reset_hook!
  #
  ind = op = sl = aop = dop = cl = ig = scl = sel = other = false
  $open_hook.add_hook!("snd-test") do |filename|
    if filename != mus_expand_filename("oboe.snd")
      snd_display("$open_hook: %s?", filename)
    end
    op = true
    false
  end
  $after_open_hook.add_hook!("snd-test") do |snd|
    aop = snd
  end
  $during_open_hook.add_hook!("snd-test") do |fd, filename, reason|
    if filename != mus_expand_filename("oboe.snd")
      snd_display("$during_open_hook filename: %s?", filename)
    end
    if reason != 1
      snd_display("$during_open_hook reason: %s?", reason)
    end
    dop = true
  end
  $initial_graph_hook.add_hook!("snd-test") do |snd, chn, dur|
    if chn.nonzero?
      snd_display("$initial_graph_hook (channel): %s not 0?", chn)
    end
    ig = true
    false
  end
  $selection_changed_hook.add_hook!("snd-test") do | |
    sel = true
  end
  ind = open_sound("oboe.snd")
  snd_display("$open_hook not called?") unless op
  snd_display("$during_open_hook not called?") unless dop
  snd_display("$initial_graph_hook not called?") unless ig
  snd_display("$after_open_hook not called?") unless number?(aop)
  snd_display("$after_open_hook %s but ind: %s?", aop, ind) if aop != ind
  snd_display("$selection_changed_hook called for no reason?") if sel
  select_all
  snd_display("$selection_changed_hook not called?") unless sel
  $open_hook.reset_hook!
  $during_open_hook.reset_hook!
  $after_open_hook.reset_hook!
  $initial_graph_hook.reset_hook!
  $selection_changed_hook.reset_hook!
  $open_hook.add_hook!("snd-test") do |filename| true end
  unless (res = open_sound("pistol.snd")) == false
    snd_display("$open_hook true, but open_sound -> %s?", res)
    close_sound(res) if sound?(res)
  end
  $open_hook.reset_hook!
  #
  gr = agr = gbf = abf = false
  $before_transform_hook.reset_hook!
  $after_transform_hook.reset_hook!
  $after_graph_hook.reset_hook!
  $graph_hook.reset_hook!
  $graph_hook.add_hook!("snd-test") do |snd, chn, y0, y1|
    snd_display("$graph_hook: %s not %s?", snd, ind) if snd != ind
    snd_display("$graph_hook: (channel): %s not 0?", chn) if chn.nonzero?
    gr = true
    false
  end
  $after_graph_hook.add_hook!("snd-test") do |snd, chn|
    snd_display("$after_graph_hook: %s not %s?", snd, ind) if snd != ind
    snd_display("$after_graph_hook: (channel): %s not 0?", chn) if chn.nonzero?
    agr = true
  end
  $before_transform_hook.add_hook!("snd-test") do |snd, chn|
    gbf = true
    cursor
  end
  $after_transform_hook.add_hook!("snd-test") do |snd, chn, scale|
    if transform_graph?(snd, chn) and transform_graph_type(snd, chn) == Graph_once
      report_in_minibuffer((2 * transform2vct(snd, chn).peak / transform_size(snd, chn)).to_s, snd)
    end
    abf = true
    false
  end
  set_transform_graph?(true, ind, 0)
  set_time_graph?(true, ind, 0)
  update_transform_graph(ind, 0)
  unless gr
    snd_display("$graph_hook not called? %s %s %s %s?",
                time_graph?(ind), short_file_name(ind), ind, sounds)
  end
  snd_display("$after_graph_hook not called?") unless agr
  snd_display("$before_transform_hook not called?") unless gbf
  snd_display("$after_transform_hook not called?") unless abf
  $before_transform_hook.reset_hook!
  set_transform_graph?(false, ind, 0)
  $graph_hook.reset_hook!
  $after_graph_hook.reset_hook!
  #
  other = open_sound("pistol.snd")
  $select_sound_hook.add_hook!("snd-test") do |snd|
    snd_display("$select_sound_hook: %s not %s?", snd, ind) if snd != ind
    sl = true
  end
  $select_channel_hook.add_hook!("snd-test") do |snd, chn|
    snd_display("$select_channel_hook: %s not %s?", snd, ind) if snd != ind
    snd_display("$select_channel_hook: (channel): %s not 0?", chn) if chn.nonzero?
    scl = true
  end
  select_sound(ind)
  snd_display("$select_sound_hook not called?") unless sl
  snd_display("$select_channel_hook not called?") unless scl
  $select_sound_hook.reset_hook!
  $select_channel_hook.reset_hook!
  #
  spl = stl = ph = ph1 = pc = false
  $start_playing_hook.add_hook!("snd-test") do |snd|
    snd_display("$start_playing_hook: %s not %s?", snd, ind) if snd != ind
    spl = true
    false
  end
  $stop_playing_hook.add_hook!("snd-test") do |snd|
    snd_display("$stop_playing_hook: %s not %s?", snd, ind) if snd != ind
    stl = true
    false
  end
  $play_hook.add_hook!("snd-test") do |n|
    snd_display("$play_hook samps: %s?", n) if n < 128
    set_expand_control_hop(expand_control_hop)
    set_expand_control_length(expand_control_length)
    set_expand_control_ramp(expand_control_ramp)
    set_contrast_control_amp(contrast_control_amp)
    set_reverb_control_lowpass(reverb_control_lowpass)
    set_reverb_control_feedback(reverb_control_feedback)
    ph = true
  end
  $dac_hook.add_hook!("snd-test") do |n|
    snd_display("$dac_hook data: %s?", n) unless sound_data?(n)
    if sound_data_length(n) < 128 and sound_data_length(n) != 64
      snd_display("$dac_hook data length: %s?", sound_data_length(n))
    end
    ph1 = true
  end
  set_expand_control?(true, ind)
  set_reverb_control?(true, ind)
  play_and_wait(0, ind)
  set_expand_control?(false, ind)
  set_reverb_control?(false, ind)
  snd_display("$start_playing_hook not called?") unless spl
  snd_display("$stop_playing_hook not called?") unless stl
  snd_display("$play_hook not called?") unless ph
  snd_display("$dac_hook not called?") unless ph1
  $start_playing_hook.reset_hook!
  $start_playing_selection_hook.reset_hook!
  $stop_playing_hook.reset_hook!
  $play_hook.reset_hook!
  $dac_hook.reset_hook!
  $play_hook.add_hook!("snd-test") do |n|
    set_expand_control_hop(0.02)
    set_expand_control_length(0.02)
    set_expand_control_ramp(0.2)
    set_contrast_control_amp(0.5)
    set_reverb_control_lowpass(0.02)
    set_reverb_control_feedback(0.02)
  end
  play_and_wait(0, ind)
  $play_hook.reset_hook!
  $start_playing_hook.add_hook!("snd-test") do |snd| true end
  play("4.aiff")
  $start_playing_hook.reset_hook!
  #
  ss = false
  old_reg = selection_creates_region
  set_selection_creates_region(true)
  $stop_playing_selection_hook.add_hook!("snd-test") do | | ss = true end
  reg = select_all
  play_selection(true)
  play_region(reg, true)
  snd_display("$stop_playing_selection_hook not called?") unless ss
  $start_playing_selection_hook.reset_hook!
  ctr = 0
  $dac_hook.add_hook!("snd-test") do |n|
    ctr += 1
    stop_playing
  end
  play_and_wait(0, ind)
  snd_display("stop_playing: %s?", ctr) if ctr > 2
  $dac_hook.reset_hook!
  #
  pl = make_player(ind, 0)
  ctr = 0
  snd_display("make_player: %s?", pl) unless player?(pl)
  snd_display("players: %s?", players.inspect) if (not players) # players returs nil if empty
  $dac_hook.add_hook!("snd-test") do |n|
    ctr += 1
    if player?(pl)
      stop_player(pl)
    else
      if ctr == 1
        snd_display("player messed up")
      end
    end
  end
  add_player(pl)
  start_playing(1, 22050, false)
  snd_display("stop_player: %s?", ctr) if ctr > 2
  $dac_hook.reset_hook!
  pl = make_player(ind, 0)
  free_player(pl)
  snd_display("free_player: %s?", pl) if player?(pl)
  #
  e0 = e1 = u0 = u1 = a0 = a1 = false
  edit_hook(ind, 0).add_hook!("snd-test-1")         do | | e0 = true end
  edit_hook(other, 0).add_hook!("snd-test-2")       do | | e1 = true; false end
  undo_hook(ind, 0).add_hook!("snd-test-1")         do | | u0 = true end
  undo_hook(other, 0).add_hook!("snd-test-2")       do | | u1 = true end
  after_edit_hook(ind, 0).add_hook!("snd-test-1")   do | | a0 = true end
  after_edit_hook(other, 0).add_hook!("snd-test-2") do | | a1 = true end
  # 
  delete_sample(0, ind, 0)
  snd_display("edit_hook true didn\'t disallow edit!") if edit_position(ind, 0) != 0
  snd_display("edit_hook true not called?") unless e0
  snd_display("after_edit_hook 0 called?") if a0
  undo_edit(1, ind, 0)
  snd_display("undo_hook called?") if u0
  # 
  delete_sample(0, other, 0)
  snd_display("edit_hook false didn\'t allow edit!") if edit_position(other, 0) != 1
  snd_display("edit_hook false not called?") unless e1
  snd_display("after_edit_hook 1 not called?") unless a1
  undo_edit(1, other, 0)
  snd_display("undo_hook not called?") unless u1
  # 
  edit_hook(ind, 0).reset_hook!
  edit_hook(other, 0).reset_hook!
  undo_hook(ind, 0).reset_hook!
  undo_hook(other, 0).reset_hook!
  after_edit_hook(ind, 0).reset_hook!
  after_edit_hook(other, 0).reset_hook!
  #
  se = sw = me = false
  $snd_error_hook.add_hook!("snd-test") do |msg| se = true end
  $snd_warning_hook.add_hook!("snd-test") do |msg| sw = true end
  $mus_error_hook.add_hook!("snd-test") do |type, msg| me = true end
  snd_error("uhoh")
  snd_warning("hiho")
  mus_sound_samples("/bad/baddy")
  snd_display("$snd_error_hook not called?") unless se
  snd_display("$snd_warning_hook not called?") unless sw
  snd_display("$mus_error_hook not called?") unless me
  $snd_error_hook.reset_hook!
  $snd_warning_hook.reset_hook!
  $mus_error_hook.reset_hook!
  $snd_error_hook.add_hook!("snd-test") do |msg| se = msg; false end
  snd_error("not an error")
  snd_display("$snd_error_hook saw: %s", se) if se != "not an error"
  $snd_error_hook.reset_hook!
  #
  $exit_hook.add_hook!("snd-test-1") do | | false end
  $exit_hook.add_hook!("snd-test-2") do | | true end
  $exit_hook.add_hook!("snd-test-3") do | | false end
  exit
  $exit_hook.reset_hook!
  #
  sh = false
  delete_file("baddy.snd")
  $save_hook.add_hook!("snd-test") do |snd, filename|
    if (not string?(filename)) or filename != mus_expand_filename("baddy.snd")
      snd_display("$save_hook filename: %s?", filename)
    end
    snd_display("$save_hook: %s not %s?", snd, ind) if snd != ind
    sh = true
  end
  save_sound_as("baddy.snd", ind)
  snd_display("$save_hook not called?") unless sh
  if File.exists?("baddy.snd")
    snd_display("$save_hook didn\'t cancel save?")
    delete_file("baddy.snd")
  end
  $save_hook.reset_hook!
  $close_hook.add_hook!("snd-test") do |snd|
    snd_display("$close_hook: %s not %s?", snd, ind) if snd != ind
    cl = true
    false
  end
  close_sound(ind)
  snd_display("$close_hook not called?") unless cl
  $close_hook.reset_hook!
  close_sound(other)
end

def test0213
  $print_hook.add_hook!("snd-test") do |str|
    if str[0] == ?[ and (print_length == 30 and
                           str != "[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ...]" or
                           print_length == 12 and
                           str != "[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ...]")
      snd_display("array abbreviation: %s?", str)
    end
    false
  end
  snd_print(make_array(128, 1))
  $print_hook.reset_hook!
  #
  ind = new_sound("fmv.snd", Mus_next, Mus_bshort, 22050, 1, "auto-update test")
  ind1 = new_sound("fmv1.snd", Mus_next, Mus_bshort, 22050, 1, "auto-update test")
  old_update = auto_update
  pad_channel(0, 1000, ind, 0)
  pad_channel(0, 1000, ind1, 0)
  save_sound(ind)
  save_sound(ind1)
  set_auto_update(true)
  sleep(1)
  file_copy("oboe.snd", "fmv1.snd")
  set_sample(100, 0.55, ind, 0, false)
  if fneq(res = sample(100, ind, 0), 0.55)
    snd_display("set_sample: %s?", res)
  end
  save_sound(ind)
  ind1 = find_sound("fmv1.snd")
  if (res = frames(ind1)) != mus_sound_frames("oboe.snd")
    snd_display("fmv1 after update: %s?", res)
  end
  set_auto_update(old_update)
  close_sound(ind)
  close_sound(ind1)
  delete_files("fmv.snd", "fmv1.snd")
  in1 = open_sound("oboe.snd")
  in2 = open_sound("2.snd")
  set_sync(1, in1)
  set_sync(1, in2)
  play_and_wait(0, false, false, true)
  close_sound(in1)
  close_sound(in2)
  #
  ind = open_sound("oboe.snd")
  edit_hook_ctr = 0
  after_edit_hook_ctr = 0
  edit_hook(ind, 0).add_hook!("snd-test") do | |
    edit_hook_ctr += 1
    true
  end
  after_edit_hook(ind, 0).add_hook!("snd-test") do | |
    after_edit_hook_ctr += 1
    true
  end
  all_tests = [
    [:apply_controls, lambda { | |
        set_amp_control(0.5, ind, 0)
        apply_controls(ind)
        set_amp_control(1.0, ind, 0)
      }],
    [:clm_channel, lambda { | | clm_channel(make_two_zero(1, -1)) }],
    [:convolve_selection_with, lambda { | |
        reg = select_all(ind, 0)
        convolve_selection_with("1a.snd", 0.5)
        forget_region(reg) if region?(reg)
      }],
    [:convolve_with, lambda { | | convolve_with("1a.snd", 0.5, ind, 0)}],
    [:delete_mix, lambda { | |
        mx = mix_vct(make_vct(3, 0.2), 123)
        delete_mix(mx) if mix?(mx)
      }],
    [:delete_sample, lambda { | | delete_sample(123, ind, 0) }],
    [:delete_samples, lambda { | | delete_samples(123, 123, ind, 0) }],
    [:delete_selection, lambda { | |
        reg = select_all(ind, 0)
        delete_selection
        forget_region(reg) if region?(reg)
      }],
    [:env_channel, lambda { | | env_channel([0, 0, 1, 1]) }],
    [:env_selection, lambda { | |
        reg = select_all(ind, 0)
        env_selection([0, 0, 1, 1], 1.0)
        forget_region(reg) if region?(reg)
      }],
    [:env_sound, lambda { | | env_sound([0, 0, 1, 1]) }],
    [:filter_sound, lambda { | | filter_sound([0, 1, 1, 0], 1024) }],
    [:filter_selection, lambda { | |
        reg = select_all(ind, 0)
        filter_selection([0, 0, 1, 1], 6)
        forget_region(reg) if region?(reg)
      }],
    [:insert_region, lambda { | |
        reg = make_region(0, 100, ind, 0)
        insert_region(123, reg, ind, 0)
        forget_region(reg) if region?(reg)
      }],
    [:insert_sample, lambda { | | insert_sample(123, 0.5, ind, 0) }],
    [:insert_samples, lambda { | | insert_samples(123, 3, make_vct(3, 1.0), ind, 0) }],
    [:insert_selection, lambda { | |
        reg = select_all(ind, 0)
        insert_selection(120, ind, 0)
        forget_region(reg) if region?(reg)
      }],
    [:insert_silence, lambda { | | insert_silence(123, 456, ind, 0) }],
    [:insert_sound, lambda { | | insert_sound("1a.snd", 123) }],
    [:map_chan, lambda { | | map_chan(lambda { |y| y + 0.2 }) }],
    [:map_channel, lambda { | | map_channel(lambda { |y| y + 0.2 }) }],
    [:mix, lambda { | | mix("1a.snd", 123) }],
    [:mix_amp, lambda { | |
        mx = mix_vct(make_vct(3, 1.0), 123)
        set_mix_amp(mx, 0, 0.123) if mix?(mx)
      }],
    [:mix_amp_env, lambda { | |
        mx = mix_vct(make_vct(3, 1.0), 123)
        set_mix_amp_env(mx, 0, [0, 0, 1, 1]) if mix?(mx)
      }],
    [:mix_track, lambda { | |
        mx = mix_vct(make_vct(3, 1.0), 123)
        set_mix_track(mx, make_track) if mix?(mx)
      }],
    [:mix_locked?, lambda { | |
        mx = mix_vct(make_vct(3, 1.0), 123)
        set_mix_locked?(mx, true) if mix?(mx)
      }],
    [:mix_position, lambda { | |
        mx = mix_vct(make_vct(3, 1.0), 123)
        set_mix_position(mx, 123) if mix?(mx)
      }],
    [:mix_speed, lambda { | |
        mx = mix_vct(make_vct(3, 1.0), 123)
        set_mix_speed(mx, 0.123) if mix?(mx)
      }],
    [:mix_region, lambda { | |
        reg = make_region(0, 100, ind, 0)
        mix_region(123, reg, ind, 0)
        forget_region(reg) if region?(reg)
      }],
    [:mix_selection, lambda { | |
        reg = select_all(ind, 0)
        mix_selection(1234, ind, 0)
        forget_region(reg) if region?(reg)
      }],
    [:mix_vct, lambda { | | mix_vct(make_vct(10, 0.3), 123) }],
    [:pad_channel, lambda { | | pad_channel(123, 456, ind, 0) }],
    [:ptree_channel, lambda { | | ptree_channel(lambda { |y| y + 0.2 }) }],
    [:ramp_channel, lambda { | | ramp_channel(0.0, 0.5, 123, 456) }],
    [:reverse_channel, lambda { | | reverse_channel(123, 456, ind, 0) }],
    [:reverse_sound, lambda { | | reverse_sound(ind, 0) }],
    [:reverse_selection, lambda { | |
        reg = select_all(ind, 0)
        reverse_selection
        forget_region(reg) if region?(reg)
      }],
    [:scale_by, lambda { | | scale_by(2.0) }],
    [:scale_channel, lambda { | | scale_channel(0.5, 123, 456, ind, 0) }],
    [:scale_selection_by, lambda { | |
        reg = select_all(ind, 0)
        scale_selection_by(2.0)
        forget_region(reg) if region?(reg)
      }],
    [:scale_selection_to, lambda { | |
        reg = select_all(ind, 0)
        scale_selection_to(0.5)
        forget_region(reg) if region?(reg)
      }],
    [:scale_to, lambda { | | scale_to(0.4) }],
    [:scale_sound_by, lambda { | | scale_sound_by(2.0) }],
    [:scale_sound_to, lambda { | | scale_sound_to(0.5) }],
    [:smooth_channel, lambda { | | smooth_channel(123, 456, ind, 0) }],
    [:smooth_sound, lambda { | | smooth_sound(123, 456, ind, 0) }],
    [:smooth_selection, lambda { | |
        reg = select_all(ind, 0)
        smooth_selection
        forget_region(reg) if region?(reg)
      }],
    [:src_channel, lambda { | | src_channel(0.5, 123, 456, ind, 0) }],
    [:src_sound, lambda { | | src_sound([0, 0.5, 1, 1]) }],
    [:src_selection, lambda { | |
        reg = select_all(ind, 0)
        src_selection(0.5)
        forget_region(reg) if region?(reg)
      }],
    [:swap_channels, lambda { | |
        ind1 = open_sound("1a.snd")
        swap_channels(ind, 0, ind1, 0)
        close_sound(ind1)
      }],
    [:track_amp, lambda { | |
        tr = (tracks ? tracks.first : make_track)
        mx = mix_vct(make_vct(3, 0.1), 0, ind, 0, true, "none", tr)
        set_track_amp(tr, 0.5)
      }],
    [:track_amp_env, lambda { | |
        tr = (tracks ? tracks.first : make_track)
        mx = mix_vct(make_vct(3, 0.1), 0, ind, 0, true, "none", tr)
        set_track_amp_env(tr, [0, 0, 1, 1])
      }],
    [:track_position, lambda { | |
        tr = (tracks ? tracks.first : make_track)
        mx = mix_vct(make_vct(3, 0.1), 0, ind, 0, true, "none", tr)
        set_track_position(tr, 5)
      }],
    [:track_speed, lambda { | |
        tr = (tracks ? tracks.first : make_track)
        mx = mix_vct(make_vct(3, 0.1), 0, ind, 0, true, "none", tr)
        set_track_speed(tr, 0.5)
      }],
    [:vct2channel, lambda { | | vct2channel(make_vct(3), 123, 3, ind, 0) }],
    [:vct2samples, lambda { | | vct2samples(123, 3, make_vct(3)) }],
    [:xramp_channel, lambda { | | xramp_channel(0.5, 1.0, 32.0, 123, 456, ind, 0) }]]
  # 
  all_tests.each do |name, func|
    func.call
    if (res = edit_position(ind, 0)) != 0
      snd_display("%s: blocked edit: %s?", name, res)
    end
    if edit_hook_ctr != 1
      snd_display("%s: edit_hook calls: %s?", name, edit_hook_ctr)
    end
    if after_edit_hook_ctr != 0
      snd_display("%s: after_edit_hook calls: %s?", name, after_edit_hook_ctr)
    end
    edit_hook_ctr = 0
    if (res = mixes(ind, 0)) != nil
      snd_display("%s: mixes: %s?", name, res)
    end
  end
  #
  edit_hook_ctr = 0
  after_edit_hook_ctr = 0
  edit_hook(ind, 0).reset_hook!
  after_edit_hook(ind, 0).reset_hook!
  edit_hook(ind, 0).add_hook!("snd-test") do | |
    edit_hook_ctr += 1
    false
  end
  after_edit_hook(ind, 0).add_hook!("snd-test") do | |
    after_edit_hook_ctr += 1
    true
  end
  # 
  all_tests.each do |name, func|
    func.call
    unless (res = edit_position(ind, 0)) > 0
      snd_display("%s: unblocked edit: %s?", name, res)
    end
    unless edit_hook_ctr > 0
      snd_display("%s: unblocked edit_hook calls: %s?", name, edit_hook_ctr)
    end
    unless after_edit_hook_ctr > 0
      snd_display("%s: unblocked after_edit_hook calls: %s?", name, after_edit_hook_ctr)
    end
    edit_hook_ctr = 0
    after_edit_hook_ctr = 0
    revert_sound(ind)
    if (res = mixes(ind, 0)) != nil
      snd_display("%s: mixes: %s?", name, res)
    end
  end
  close_sound(ind)
end

def test13
  reset_all_hooks
  test0013
  test0113
  test0213
end

if $test13 and $full_test or $snd_test == 13
  $before_test_hook.call(13)
  test13
  $after_test_hook.call(13)
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
$timings.each do |tst| snd_info("test %2d  %s", tst.first, tst.last.inspect) end
snd_info("total    %s\n", $overall_start_time.inspect)
save_listener("test.output")
mus_audio_playback_amp($orig_audio_amp)

exit if $with_exit

# snd-test.rb ends here
