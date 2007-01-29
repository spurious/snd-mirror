# examp.rb -- Guile -> Ruby translation -*- snd-ruby -*-

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Wed Sep 04 18:34:00 CEST 2002
# Changed: Sun Dec 31 00:39:24 CET 2006

# Commentary:
#
# Extensions to Ruby:
# 
# provided?(feature)
# provide(feature)
# features(all)
# 
# array?(obj)   alias list?(obj)
# hash?(obj)
# string?(obj)
# regexp?(obj)
# symbol?(obj)
# number?(obj)
# integer?(obj)
# float?(obj)
# rational?(obj)
# complex?(obj)
# boolean?(obj)
# proc?(obj)
# thunk?(obj)
# method?(obj)
# func?(obj)
# mus?(obj)
# get_func_name(n)
# assert_type(condition, obj, pos, msg)
# identity(arg)
# ignore(*rest)
# enum(*names)
#
# class Object
#  null?
#  function?(obj)
#  snd_func(name, *rest, &body)
#  set_snd_func(name, val, *rest, &body)
#  snd_apropos(str_or_sym)
#
# NilClass(arg)
# Fixnum(arg)
#
# class NilClass
#  each
#  apply(func, *rest, &body)
#  empty?
#  zero?
#  nonzero?
#  to_vct
#  to_vector
#  to_poly
#  +(other)
#  -(other)
#  *(other)
#  
# backward compatibility methods:
#  String#to_sym, Symbol#to_sym
#  make_array(len, init, &body)
#  Array#insert
#  Float#step
#  Range#step
#  Enumerable#each_index
#  Enumerable#zip
#
# class Array
#  to_pairs
#  each_pair do |x, y| ... end
#  to_string(len)
#  first=(val)
#  last=(val)
#  pick
#  rand
#  rand!
#  add(other)
#  add!(other)
#  subtract(other)
#  subtract!(other)
#  multiply(other)
#  multiply!(other)
#  offset(scl)
#  offset!(scl)
#  scale(scl)
#  scale!(scl)
#  to_vector
#  car
#  car=
#  cadr
#  cadr=
#  caddr
#  caddr=
#  cadddr
#  cadddr=
#  caddddr
#  caddddr=
#  cdr
#  step(n)
#  apply(func, *rest, &body)
#
# class Vec < Array
#  Vec[]
#  initialize(len, init, &body)
#  inspect
#  to_s
#  to_vector
#  +(other)
#  -(other)
#  *(other)
#
# Vec(obj)
# make_vector(len, init, &body)
# vector?(obj)
# vector(*args)
#
# class String
#  to_vector
#  to_vct
#
# Vct(obj)
# make_vct!(len, init) do |i| ... end
#
# class Vct
#  Vct[]
#  name
#  to_sound_data(sd, chn)
#  to_vct
#  to_vector
#  apply(func, *rest, &body)
#  +(other)   handles self.offset (Numeric) and self.add (Array, Vec, Vct)
#  -(other)   handles self.offset (Numeric) and self.subtract (Array, Vec, Vct)
#  *(other)   handles self.scale (Numeric) and self.multiply (Array, Vec, Vct)
#  step(n)
#  [](idx, size)
#
# class Fixnum
#  +(other)   handles other.offset on Vct, Array, and Vec
#  *(other)   handles other.scale on Vct, Array, and Vec
#
# class Float
#  +(other)   handles other.offset on Vct, Array, and Vec
#  *(other)   handles other.scale on Vct, Array, and Vec
#
# SoundData(ary)          can be used to reread evaled output from sound_data2string
# sound_data2string(sd)   produces a string which can be evaled and reread with SoundData
#
# class SoundData
#  name
#  to_vct(chn)
#  to_a
#  length
#  scale!(scl)
#  fill!(val)
#  each(chn)
#  each_with_index(chn)
#  map(chn)
#  map!(chn)
#
# mus_a0(gen)
# set_mus_a0(gen, val)
# mus_a1(gen)
# set_mus_a1(gen, val)
# mus_a2(gen)
# set_mus_a2(gen, val)
# mus_b1(gen)
# set_mus_b1(gen, val)
# mus_b2(gen)
# set_mus_b2(gen, val)
#
# class Mus
#  run(arg1, arg2)
#  apply(*rest)
#  inspect
#  close
#  xcoeff=(index, val)
#  ycoeff=(index, val)
#  a0  a0=(val)
#  a1  a1=(val)
#  a2  a2=(val)
#  b1  b1=(val)
#  b2  b2=(val)
#
# class Musgen   base class for generators written in Ruby
#  initialize
#  inspect
#  to_s
#  run(val1, val2)
#  apply(*rest)
#  eql?(other)
#  reset
#
# class Numeric
#  positive?
#  negative?
#
# class Integer
#  even?
#  odd?
#  prime?
#
# module Enumerable
#  map_with_index do |x, i| ... end
#  map_with_index! do |x, i| ... end
#  cycle
#  cycle=(val)
#
# as_one_edit_rb(*origin, &body)
# map_channel_rb(beg, dur, snd, chn, edpos, edname, &body)
# map_chan_rb(beg, dur, edpos, snd, chn, &body)
#
# with_silence(exception) do |old_verbose, old_debug| ... end
# 
# module Info
#  description=(text)
#  description
#
# class Proc
#  to_method(name, klass)
#  to_str
#  to_body
#  source
#  source=
#
# make_proc2method(name, prc)
# make_proc_with_setter(name, getter, setter)
# make_proc_with_source(string, bind)
# proc_source(prc)   set_proc_source(prc, val)
#
# Multi-line input to the Snd listener and Emacs/inf-snd.el
# 
# $emacs_eval_hook.call(line)
# run_emacs_eval_hook(line)
# 
# class Snd_eval
#  Snd_eval.count_level(line)
#
# class Snd_prompt
#  initialize(level)
#  inspect
#  update(level)
#  reset
#  
# start_emacs_eval(file)
# start_listener_eval(file)
# stop_emacs_eval
# stop_listener_eval
#
# Debugging resp. inspecting local variables
# 
# debug_properties(name)      set_debug_properties(name, val)
# debug_property(key, name)   set_debug_property(key, val, name)
# debug_binding(name)         set_debug_binding(bind, name)
# display_all_variables(name)
# each_variables(&body)
#
# let(*rest) do |*rest| ... end
#
# Utilities:
#
# close_sound_extend(snd)
# times2samples(start, dur)
# random(n)
# logn(r, b)
# car(v), cadr(v), caddr(v), cdr(v)
# warning(*args), die(*args), error(*args)
# clm_message(*args), message(*args), debug(*args), debug_trace(*args)
#
# class Snd
#  Snd.add_sound_path(path)
#  Snd.open_from_path(fname)
#  Snd.find_from_path(fname)
#  Snd.fullname(fname)
#  Snd.load_path
#  Snd.message(*args)
#  Snd.display(*args)
#  Snd.warning(*args)
#  Snd.die(*args)
#  Snd.error(*args)
#  Snd.debug(*args)
#  Snd.debug_trace(*args)
#  Snd.sounds
#  Snd.regions
#  Snd.tracks
#  Snd.marks(snd, chn)
#  Snd.snd(snd)
#  Snd.chn(chn)
#  Snd.catch(tag, retval)
#  Snd.throw(tag, *rest)
#  Snd.raise(tag, *rest)
#
# snd_catch(tag, retval)
# snd_throw(tag, *rest)
# snd_raise(tag, *rest)
#
# gloop(*args) do |args| ... end
# get_args(args, key, default)
# get_shift_args(args, key, default)
# get_class_or_key(args, klass, key, default)
# optkey(args, *rest)
# load_init_file(file)
#
# edit_list_proc_counter
# set_edit_list_proc_counter
#
# module Examp (examp.scm)
#  selection_rms
#  region_rms(n)
#  window_samples(snd, chn)
#  display_energy(snd, chn)
#  display_db(snd, chn)
#  window_rms
#  fft_peak(snd, chn, scale)
#  finfo(file)
#  correlate(snd, chn, y0, y1)
#
#  zoom_spectrum(snd, chn, y0, y1)
#  zoom_fft(snd, chn, y0, y1)
#  superimpose_ffts(snd, chn, y0, y1)
#  locate_zero(limit)
#  shell(cmd, *rest)
#
#  mpg(mpgfile, rawfile)
#  read_ogg(filename)
#  write_ogg(snd)
#  read_speex(filename)
#  write_speex(snd)
#  read_flac(filename)
#  write_flac(snd)
#  read_ascii(in_filename, out_filename, out_type, out_format, out_srate)
#  auto_dot(snd, chn, y0, y1)
#
#  first_mark_in_window_at_left
#  flash_selected_data(interval)
#  mark_loops
#  do_all_chans(origin) do |y| ... end
#  update_graphs
#  do_chans(*origin) do |y| ... end
#  do_sound_chans(*origin) do |y| ... end
#  every_sample? do |y| ... end
#  sort_samples(nbins)
#  place_sound(mono_snd, stereo_snd, pan_env)
#
#  fft_edit(bottom, top, snd, chn)
#  fft_squelch(squelch, snd, chn)
#  fft_cancel(lo_freq, hi_freq, snd, chn)
#  ramp(gen, up)
#  make_ramp(size)
#  squelch_vowels(snd, chn)
#  fft_env_data(fft_env, snd, chn)
#  fft_env_edit(fft_env, snd, chn)
#  fft_env_interp(env1, env2, interp, snd, chn)
#  fft_smoother(cutoff, start, samps, snd, chn)
#
#  comb_filter(scaler, size)
#  comb_chord(scaler, size, amp, interval_one, interval_two)
#  zcomb(scaler, size, pm)
#  notch_filter(scaler, size)
#  formant_filter(radius, freq)
#  formants(r1, f1, r2, f2, r3, f3)
#  moving_formant(radius, move)
#  osc_formants(radius, bases, amounts, freqs)
#  echo(scaler, secs)
#  zecho(scaler, secs, freq, amp)
#  flecho(scaler, secs)
#  ring_mod(freq, gliss_env)
#  am(freq)
#  vibro(speed, depth)
#  hello_dentist(freq, amp, snd, chn)
#  fp(sr, osamp, osfreq, snd, chn)
#  compand(doc)
#  compand_channel(beg, dur, snd, chn, edpos)
#  expsrc(rate, snd, chn)
#  expsnd(gr_env, snd, chn)
#  cross_synthesis(cross_snd, amp, fftsize, r)
#  voiced2unvoiced(amp, fftsize, r, temp, snd, chn)
#  pulse_voice(cosin, freq, amp, fftsize, r, snd, chn)
#  cnvtest(snd0, snd1, amp)
#
#  swap_selection_channels
#  make_sound_interp(start, snd, chn)
#  sound_interp(func, loc)
#  sound_via_sound(snd1, snd2)
#  env_sound_interp(envelope, time_scale, snd, chn)
#  granulated_sound_interp(envelope, time_scale, grain_length, grain_envelope, output_hop, snd, chn)
#  title_with_date
#  filtered_env(en, snd, chn)
#
#  class Mouse
#   initialize
#   press(snd, chn, button, state, x, y)
#   drag(snd, chn, button, state, x, y)
#
#  files_popup_buffer(type, position, name)
#
#  class Snd_buffers
#   initialize
#   switch_to_buffer
#   open(snd)
#   close(snd)
#
#  find_click(loc)
#  remove_clicks
#  search_for_click
#  zero_plus
#  next_peak
#  find_pitch(pitch)
#  file2vct(file)
#  add_notes(notes, snd, chn)
#  region_play_list(data)
#  region_play_sequence(data)
#  replace_with_selection
#  explode_sf2
#
#  class Next_file
#   initialize
#   open_next_file_in_directory
#  click_middle_button_to_open_next_file_in_directory
#
#  chain_dsps(start, dur, *dsps)
#
#  module Cursor_follows_play
#   local_dac_func(data)
#   local_start_playing_func(snd)
#   local_stop_playing_func(snd)
#   current_cursor(snd, chn)
#   set_current_cursor(val, snd, chn)
#   original_cursor(snd, chn)
#   set_original_cursor(val, snd, chn)
#  if_cursor_follows_play_it_stays_where_play_stopped(enable)
#
#  smooth_channel_via_ptree(beg, dur, snd, chn, edpos)
#  ring_modulate_channel(freq, beg, dur, snd, chn, edpos)
#  scramble_channels(*new_order)
#  scramble_channel(silence)
#
#  reverse_by_blocks(block_len, snd, chn)
#  reverse_within_blocks(block_len, snd, chn)
#  sound2segment_data(main_dir, output_file)
#  channel_clipped?(snd, chn)
#  scan_sound(func, beg, dur, snd)      or scan_sound_rb(beg, dur, snd) do |y, chn| ... end
#  
# class Moog_filter < Musgen (moog.scm)
#   initialize(freq, q)
#   frequency=(freq)
#   filter(insig)
#
# module Moog
#  make_moog_filter(freq, q)
#  moog_filter(moog, insig)
#  moog(freq, q)
# 
# Code:

unless defined? $LOADED_FEATURES then alias $LOADED_FEATURES $" end

def provided?(feature)
  assert_type((symbol?(feature) or string?(feature)), feature, 0, "a symbol or a string")
  $LOADED_FEATURES.map do |f| File.basename(f) end.member?(feature.to_s.tr("_", "-"))
end

def provide(feature)
  assert_type((symbol?(feature) or string?(feature)), feature, 0, "a symbol or a string")
  $LOADED_FEATURES.push(feature.to_s)
end

def features(all = nil)
  if all
    $LOADED_FEATURES.map do |f| File.basename(f) end
  else
    $LOADED_FEATURES.map do |f|
      next if f.include?("/") or f.include?(".")
      f
    end.compact
  end
end

def make_polar(r, theta)
  Complex.new(cos(theta) * r, sin(theta) * r)
end

def make_rectangular(re, im = 1.0)
  Complex.new(re, im)
end

def array?(obj)
  obj.kind_of?(Array)
end
alias list? array?

def hash?(obj)
  obj.kind_of?(Hash)
end

def string?(obj)
  obj.kind_of?(String)
end

def regexp?(obj)
  obj.kind_of?(Regexp)
end

def symbol?(obj)
  obj.kind_of?(Symbol)
end

def number?(obj)
  obj.kind_of?(Numeric)
end

def integer?(obj)
  obj.kind_of?(Fixnum)
end

def float?(obj)
  obj.kind_of?(Float)
end

def rational?(obj)
  obj.kind_of?(Rational)
end

def complex?(obj)
  obj.kind_of?(Complex)
end

def boolean?(obj)
  obj.kind_of?(TrueClass) or obj.kind_of?(FalseClass)
end

def proc?(obj)
  obj.kind_of?(Proc)
end

def thunk?(obj)
  obj.kind_of?(Proc) and obj.arity.zero?
end

def method?(obj)
  obj.kind_of?(Method)
end

def func?(obj)
  obj.kind_of?(String) or obj.kind_of?(Symbol)
end

def mus?(obj)
  obj.kind_of?(Mus)
end

def binding?(obj)
  obj.kind_of?(Binding)
end

def get_func_name(n = 1)
  if ca = caller(n)[0].scan(/^.*:in `(.*)'/).first
    ca.first
  else
    "top_level"
  end
end

def assert_type(condition, obj, pos, msg)
  condition or Kernel.raise(TypeError,
                            format("%s: wrong type arg %d, %s, wanted %s",
                                   get_func_name(2), pos, obj.inspect, msg))
end

def identity(arg)
  arg
end

def ignore(*rest)
  nil
end

# backward compatibility aliases and constants (mostly from snd7.scm)
# alias new old
if provided? :snd
  alias save_options                    save_state
  alias delete_samples_with_origin      delete_samples
  alias default_output_type             default_output_header_type
  alias default_output_format           default_output_data_format
  alias previous_files_sort             view_files_sort
  alias preload_directory               add_directory_to_view_files_list
  alias preload_file                    add_file_to_view_files_list
  alias $previous_files_select_hook     $view_files_select_hook
  alias recorder_in_format              recorder_in_data_format
  alias recorder_out_format             recorder_out_data_format
  alias recorder_out_type               recorder_out_header_type
  Sort_files_by_name = 0
  Sort_files_by_date = 2
  Sort_files_by_size = 4
  Sort_files_by_entry = -1
  alias mus_audio_sun_outputs           mus_sun_set_outputs
  alias set_oss_buffers                 mus_oss_set_buffers
  alias mus_audio_set_oss_buffers       mus_oss_set_buffers
  unless defined? mus_file_data_clipped
    alias mus_file_data_clipped         mus_clipping
    alias set_mus_file_data_clipped     set_mus_clipping
  end
  alias mus_data_clipped                mus_clipping
  alias set_mus_data_clipped            set_mus_clipping
  alias dac_is_running                  playing
  # backwards compatibility for snd 8
  alias make_ppolar                     make_two_pole
  alias make_zpolar                     make_two_zero
  alias make_average                    make_moving_average
  alias average                         moving_average
  alias average?                        moving_average?
  # *windowed_maxamp -> dsp.rb
  def samples2sound_data(beg = 0,
                         num = false,
                         snd = false,
                         chn = false,
                         obj = false,
                         pos = false,
                         sd_chan = 0)
    len = (num or frames(snd, chn))
    gen = (obj or make_sound_data(1, len))
    vct2sound_data(channel2vct(beg, len, snd, chn, pos), gen, sd_chan)
  end

  def open_sound_file(*args)
    mus_sound_open_output(get_args(args, :file, (little_endian ? "test.wav" : "test.snd")),
                          get_args(args, :srate, 22050),
                          get_args(args, :channels, 1),
                          (little_endian ? Mus_lfloat : Mus_bfloat),
                          get_args(args, :header_type, (little_endian ? Mus_riff : Mus_next)),
                          get_args(args, :comment, ""))
  end

  alias close_sound_file mus_sound_close_output

  def vct2sound_file(fd, v, samps)
    mus_sound_write(fd, 0, samps - 1, 1, vct2sound_data(v))
  end
end

# enum("foo", :bar, "FOO_BAR")
# produces three constants
# Foo     == 0
# Bar     == 1
# FOO_BAR == 2
def enum(*names)
  names.flatten.map_with_index do |name, i|
    const_name = name.to_s
    if const_name[0].between?(?a, ?z)
      const_name[0] += ?A - ?a
    end
    Object.const_set(const_name, i)
    const_name
  end
end

class Object
  def null?
    self.nil? or
      (self.respond_to?(:zero?) and self.zero?) or
      (self.respond_to?(:empty?) and self.empty?) or
      (self.respond_to?(:length) and self.length.zero?)
  end

  def function?(obj)
    func?(obj) and Snd.catch(:all, false) do self.method(obj) end.first
  rescue
    false
  end
  
  # Float(nil) ==> 0.0 like Integer(nil) ==> 0
  def new_Float(numb)
    if numb.kind_of?(NilClass)
      0.0
    else
      old_Float(numb)
    end
  end
  alias old_Float Float
  alias Float new_Float

  def snd_func(name, *rest, &body)
    assert_type(func?(name), name, 0, "a string or a symbol")
    send(name.to_s, *rest, &body)
  end
  
  def set_snd_func(name, val, *rest, &body)
    assert_type(func?(name), name, 0, "a string or a symbol")
    send(format("set_%s", name.to_s), val, *rest, &body)
  end

# snd_apropos(str_or_sym)
# if `str_or_sym' is a symbol, returns snd_help result,
# if `str_or_sym' is a string or regexp it looks in
#   self.public_methods,
#   self.protected_methods,
#   self.private_methods,
#   Object.constants, and
#   Kernel.global_variables and returns an array of strings or nil.
# 
#     [].snd_apropos(/^apply/)     ==> ["apply", "apply_controls"]
# vct(0).snd_apropos("subseq")     ==> ["subseq", "vct_subseq"]
#        snd_apropos(/^mus_sound/) ==> ["mus_sound_...", ...]
  def snd_apropos(str_or_sym)
    case str_or_sym
    when Symbol
      snd_help(str_or_sym)
    when String, Regexp
      res = []
      [self.public_methods,
        self.protected_methods,
        self.private_methods,
        Object.constants,
        Kernel.global_variables].each do |m| res += m.grep(/#{str_or_sym}/) end
      res
    else
      nil
    end
  end
end

def NilClass(arg)
  nil
end

alias Fixnum Integer

class NilClass
  # FIXME (dangerous)
  # def method_missing(id, *args, &body)
  #   nil
  # end

  def each
    nil
  end

  def apply(func, *rest, &body)
    nil
  end
  
  def empty?
    true
  end

  # Integer(nil) ==> 0
  def zero?
    true
  end
  
  def nonzero?
    false
  end

  def to_vct
    vector2vct([])
  end

  def to_vector
    vector()
  end

  def to_poly
    poly()
  end

  def +(other)
    other
  end

  def -(other)
    other
  end

  def *(other)
    snd_func(other.class.name, nil)
  end
end

# If $DEBUG = true, on older Ruby versions warnings occur about
# missing NilClass#to_str and Symbol#to_str
if $DEBUG and RUBY_VERSION < "1.8.0"
  class Object
    def method_missing(id, *args)
      if id == :to_str
        self.class.class_eval do define_method(id, lambda do self.to_s end) end
        id.id2name
      else
        Kernel.raise(NameError,
                     format("[version %s] undefined method `%s'", RUBY_VERSION, id.id2name))
      end
    end
  end
end
  
class String
  def to_sym
    self.intern
  end unless defined? "a".to_sym
end

class Symbol
  def to_sym
    self
  end unless defined? :a.to_sym
end

alias object_id __id__ unless defined? object_id

# with_silence(exception) do |old_verbose, old_debug| ... end
# 
# subpress debug messages (mostly on older Ruby versions)
# 
# with_silence do $global_var ||= value end
# with_silence(LoadError) do require("nonexistent.file") end
def with_silence(exception = StandardError)
  old_verbose = $VERBOSE
  old_debug = $DEBUG
  $VERBOSE = false
  $DEBUG = false
  ret = if block_given?
          begin
            yield(old_verbose, old_debug)
          rescue exception
            false
          end
        else
          false
        end
  $VERBOSE = old_verbose
  $DEBUG = old_debug
  ret
end

# Provides descriptions of instances of classes, see nb.rb,
# xm-enved.rb, etc.
#
# m = lambda do |*args| puts args end
# m.info = "my description"
# puts m.info
module Info
  def description=(val)
    @description = val.to_s
  end
  alias info= description=
  
  def description
    if defined?(@description) and string?(@description) and (not @description.empty?)
      @description
    else
      "no description available"
    end
  end
  alias info description
end

require "ws"
alias snd_help get_help unless defined? snd_help

$array_print_length = 10

def print_length
  $array_print_length
end unless defined? print_length

def set_print_length(val)
  $array_print_length = val
end unless defined? set_print_length

# Older Ruby versions lack Array.new(10) do |i| ... end
# make_array
# make_array(10)
# make_array(10, 1.0)
# make_array(10) do |i| ... end
def make_array(len = 0, init = nil)
  assert_type((number?(len) and len >= 0), len, 0, "a number")
  len = Integer(len)
  if block_given?
    Array.new(len, init).map_with_index do |x, i| yield(i) end
  else
    Array.new(len, init)
  end
end

class Array
  def insert(pos, *args)
    unless args.empty?
      if pos < 0
        pos = self.length - (pos.abs - 1)
      end
      tmp = self.dup
      self[pos, args.length] = args
      self[pos + args.length..-1] = tmp[pos..-1]
    end
    self
  end unless defined? [].insert

  # [0.0, 0.0, 0.5, 0.2, 1.0, 1.0].to_pairs --> [[0.0, 0.0], [0.5, 0.2], [1.0, 1.0]]
  def to_pairs
    ary = []
    self.step(2) do |a, b| ary.push([a, b]) end
    ary
  end

  # [0.0, 0.0, 0.5, 0.2, 1.0, 1.0].each_pair do |x, y| print x, " ", y, "\n" end
  # --> 0.0 0.0
  #     0.5 0.2
  #     1.0 1.0
  def each_pair
    ary = []
    self.step(2) do |a, b| ary.push(yield(a, b)) end
    ary
  end

  # prints flat float array more prettily
  def to_string(len = print_length)
    ary = self.flatten
    str = "["
    ary.each_with_index do |val, i|
      if i < len
        str += "%1.3f, " % val.to_f
      else
        break
      end
    end
    if ary.length > len
      str += "..."
    else
      str.chop!.chop!
    end
    str += "]"
  end

  alias old_to_s to_s
  alias to_s inspect

  def first=(val)
    self[0] = val
  end

  def last=(val)
    self[-1] = val
  end

  # ary.pick       ==> random value
  # ary.pick(3)    ==> [x, y, z]
  # ary.pick(true) ==> whole ary randomized
  def array_pick(n = 1)
    n = self.length if n == true
    if n == 1
      self[kernel_rand(self.length)]
    else
      (0...n).map do |i| self[kernel_rand(self.length)] end
    end
  end
  alias pick array_pick

  def array_rand
    tmp = self.dup
    tmp.each_index do |i|
      r = kernel_rand(tmp.length)
      tmp[r], tmp[i] = tmp[i], tmp[r]
    end
    tmp
  end
  alias rand array_rand
  
  def array_rand!
    self.each_index do |i|
      r = kernel_rand(self.length)
      self[r], self[i] = self[i], self[r]
    end
    self
  end
  alias rand! array_rand!
  
  def add(other)
    assert_type((array?(other) or vct?(other)), 0, other, "an array, a vector or a vct")
    new_ary = self.dup
    [self.length, other.length].min.times do |i| new_ary[i] += other[i] end
    new_ary
  end

  def add!(other)
    assert_type((array?(other) or vct?(other)), 0, other, "an array, a vector or a vct")
    [self.length, other.length].min.times do |i| self[i] += other[i] end
    self
  end

  def subtract(other)
    assert_type((array?(other) or vct?(other)), 0, other, "an array, a vector or a vct")
    new_ary = self.dup
    [self.length, other.length].min.times do |i| new_ary[i] -= other[i] end
    new_ary
  end

  def subtract!(other)
    assert_type((array?(other) or vct?(other)), 0, other, "an array, a vector or a vct")
    [self.length, other.length].min.times do |i| self[i] -= other[i] end
    self
  end
  
  def multiply(other)
    assert_type((array?(other) or vct?(other)), 0, other, "an array, a vector or a vct")
    new_ary = self.dup
    [self.length, other.length].min.times do |i| new_ary[i] *= other[i] end
    new_ary
  end

  def multiply!(other)
    assert_type((array?(other) or vct?(other)), 0, other, "an array, a vector or a vct")
    [self.length, other.length].min.times do |i| self[i] *= other[i] end
    self
  end

  def offset(scl)
    assert_type(number?(scl), 0, scl, "a number")
    scl = Float(scl)
    self.class.new(self.length) do |i| self[i] + scl end
  end

  def offset!(scl)
    assert_type(number?(scl), 0, scl, "a number")
    scl = Float(scl)
    self.map! do |val| val += scl end
  end

  def scale(scl)
    assert_type(number?(scl), 0, scl, "a number")
    scl = Float(scl)
    self.class.new(self.length) do |i| self[i] * scl end
  end

  def scale!(scl)
    assert_type(number?(scl), 0, scl, "a number")
    scl = Float(scl)
    self.map! do |val| val *= scl end
  end

  def to_vector
    Vec.new(self.length) do |i| Float(self[i]) end
  end
  
  def car
    self[0]
  end
  
  def car=(val)
    self[0] = val
  end

  def cadr
    self[1]
  end

  def cadr=(val)
    self[1] = val
  end

  def caddr
    self[2]
  end

  def caddr=(val)
    self[2] = val
  end

  def cadddr
    self[3]
  end
  def cadddr=(val)
    self[3] = val
  end

  def caddddr
    self[4]
  end

  def caddddr=(val)
    self[4] = val
  end

  def cdr
    self[1..-1]
  end

  def step(n = 1)
    0.step(self.length - n, n) do |i| yield(*self[i, n]) end
    self
  end

  add_help(:apply,
           "Array#apply([:func,] *rest, &body)
applies function or procedure with possible rest args \
to each element of Array or subclasses of Array.
                                  [0, 1, 2].apply(\"a: %d\\n\") do |fmt, a| printf(fmt, a) end
                                  [0, 1, 2].apply(:printf, \"a: %d\\n\")
both produce
a: 0
a: 1
a: 2
                               [1, 2, 3, 4].apply(:+)      # ==> 10
                 %w(snd sndplay with_sound).apply(:length) # ==> [3, 7, 10] 
          [[1, 2, 3, 4], [1, 2, 3], [1, 2]].apply(:max)    # ==> [4, 3, 2]
[vct(0.1, 0.2, 0.3), vct(-0.1, -0.2, -0.3)].apply(:peak)   # ==> [0.3, 0.3]
                                     sounds.apply(:map) do |s| puts s end
                                     sounds.apply(:close_sound)")
  def apply(func, *rest, &body)
    if block_given? and (not symbol?(func))
      rest.unshift(func)
      self.map do |item| yield(*rest + [item]) end
    else
      assert_type((func?(func) or proc?(func) or method?(func)),
                  func, 0, "a function (string or symbol), a method or a proc")
      case func
      when Proc, Method
        self.map do |item| func.call(*rest + [item]) end
      when Symbol, String
        if body and self.methods.member?(func.to_s)
          # map, each, ...
          self.send(func, *rest, &body)
        else
          receiver = self.compact.first
          if receiver and receiver.methods.member?(func.to_s)
            # methods
            case func.to_sym
            when :+, :-, :*
              res = receiver
              self[1..-1].compact.map do |item| res = res.send(func, *rest + [item]) end
              res
            else
              len = rest.length + ((array?(receiver) and receiver.length) or 1)
              if receiver.method(func).arity.abs == len
                # remove_file (String(WS) in ws.rb)
                self.map do |item| send(func, *rest + [item]) end
              else
                # length, max, min, ...
                self.map do |item| item.send(func, *rest) end
              end
            end
          else
            # functions
            self.map do |item| send(func, *rest + [item]) end
          end
        end
      end
    end
  end

  # original operands +, -, and * can now handle nil and numberic (offset, multiply)
  # 
  # [].+(ary)      concatenate arrays
  # [].+(number)   [].add(number)
  unless defined? [].ary_plus
    alias old_ary_plus +
    def ary_plus(other)
      case other
      when Numeric
        self.offset(other)
      when NilClass
        self
      else
        self.old_ary_plus(other)
      end
    end
    alias + ary_plus
  end

  # [].-(ary)     intersection
  # [1, 2, 3, 4] - [2, 3] ==> [1, 4]
  # [] - number   [].offset
  unless defined? [].ary_minus
    alias old_ary_minus -
    def ary_minus(other)
      case other
      when Numeric
        self.offset(-other)
      when NilClass
        self
      else
        self.old_ary_minus(other)
      end
    end
    alias - ary_minus
  end
  
  # [].*(n)   repetition or [].join(n)
  # [5] * 3              ==> [5, 5, 5]
  # ["foo", "bar"] * "-" ==> "foo-bar"
  unless defined? [].ary_times
    alias old_ary_times *
    def ary_times(other)
      case other
      when NilClass
        nil.to_a
      else
        self.old_ary_times(other)
      end
    end
    alias * ary_times
  end
end

# name Vector is in use (lib/ruby/1.9/matrix.rb)
class Vec < Array
  def self.[](*ary)
    self.new(ary.length) do |i| ary[i] end
  end
  
  def initialize(len, init = 0.0, &body)
    assert_type((number?(len) and len >= 0), len, 0, "a number")
    @name = "vector"
    len = Integer(len)
    if block_given?
      super(len, &body)
    else
      super(len, init)
    end
    test = self.detect do |x| (not number?(x)) end
    assert_type((not test), test, 0, "only numeric elements")
  end

  def inspect
    str = "%s(" % @name
    self.each do |val| str += "%s, " % val end
    if self.length > 0 then str.chop!.chop! end
    str += ")"
    str
  end

  def to_s
    if self.length > 0
      vals = ":"
      self.map do |val| vals += " %s" % val end
    else
      vals = ""
    end
    format("#<%s[%d]%s>", self.class, self.length, vals)
  end
  
  def +(other)
    case other
    when Numeric
      self.offset(other).to_vector
    when Array, Vec, Vct
      self.add(other.to_vector)
    when NilClass
      self
    end
  end
  
  def -(other)
    case other
    when Numeric
      self.offset(-other).to_vector
    when Array, Vec, Vct
      self.subtract(other.to_vector)
    when NilClass
      self
    end
  end

  def *(other)
    case other
    when Numeric
      self.scale(other).to_vector
    when Array, Vec, Vct
      self.multiply(other.to_vector)
    when NilClass
      nil.to_vector
    end
  end
end

def Vec(obj)
  if obj.nil? then obj = [] end
  assert_type(obj.respond_to?(:to_vector), obj, 0,
              "an object containing method 'to_vector' (Vct, String, Array and subclasses)")
  obj.to_vector
end

def make_vector(len, init = 0.0, &body)
  Vec.new(len, init, &body)
end

def vector?(obj)
  obj.kind_of?(Vec)
end

def vector(*args)
  args.to_vector
end

class String
  def to_vector
    if self.scan(/^vector\([-+,.)\d\s]+/).null?
      nil
    else
      eval(self)
    end
  end
  
  def to_vct
    if self.scan(/^vct\([-+,.)\d\s]+/).null?
      nil
    else
      eval(self)
    end
  end
end

def Vct(obj)
  if obj.nil? then obj = [] end
  assert_type(obj.respond_to?(:to_vct), obj, 0,
              "an object containing method 'to_vct' (Vct, String, Array and subclasses)")
  obj.to_vct
end

def make_vct!(len, init = 0.0, &body)
  if block_given?
    Vct.new(len, &body)
  else
    Vct.new(len, init)
  end
end

class Vct
  def self.[](*ary)
    self.new(ary.length) do |i| ary[i] end
  end

  def name
    self.class.to_s.downcase
  end
  
  def to_sound_data(sd = nil, chn = 0)
    if sound_data?(sd)
      vct2sound_data(self, sd, chn)
    else
      vct2sound_data(self)
    end
  end

  def to_vct(chn = 0)           # CHN for compatibility with sound-data
    self
  end

  def to_vector
    Vec.new(self.length) do |i| self[i] end
  end

  def apply(*rest, &body)
    self.to_a.apply(*rest, &body)
  end
  
  def +(other)
    assert_type((number?(other) or vct?(other) or array?(other) or other.nil?), other, 0,
                "a number, an array, a vector or a vct")
    case other
    when Numeric
      self.offset(other)
    when Array, Vec, Vct
      self.add(other.to_vct)
    when NilClass
      self
    end
  end

  def -(other)
    assert_type((number?(other) or vct?(other) or array?(other) or other.nil?), other, 0,
                "a number, an array, a vector or a vct")
    case other
    when Numeric
      self.offset(-other)
    when Array, Vec, Vct
      self.subtract(other.to_vct)
    when NilClass
      self
    end
  end

  def *(other)
    assert_type((number?(other) or vct?(other) or array?(other) or other.nil?), other, 0,
                "a number, an array, a vector or a vct")
    case other
    when Numeric
      self.scale(other)
    when Array, Vec, Vct
      self.multiply(other.to_vct)
    when NilClass
      nil.to_vct
    end
  end

  def step(n = 1, &body)
    self.to_a.step(n, &body)
  end

  # v = vct(0, 1, 2, 3, 4)
  # v[2..4]  ==> vct(2.000, 3.000, 4.000)
  # v[2...4] ==> vct(2.000, 3.000)
  # v[3, 4]  ==> vct(3.000, 4.000)
  # v[-1]    ==> 4.0
  def vct_ref_extend(idx, size = nil)
    case idx
    when Fixnum
      if idx < 0 then idx += self.length end
      if idx < 0 then Snd.raise(:out_of_range, "index < 0", idx) end
      if integer?(size)
        size += idx - 1
        if size >= self.length then size = self.length - 1 end
        if size.between?(0, self.length - 1) and size >= idx
          self.subseq(idx, size)
        else
          nil.to_vct # i.e. false
        end
      else
        vct_ref(self, idx)
      end
    when Range
      beg = idx.first
      len = idx.last
      if beg < 0 then beg += self.length end
      if len < 0 then len += self.length end
      if len >= self.length then len = self.length - 1 end
      # exclude_end?: (1..2)  ==> false
      #               (1...2) ==> true
      if idx.exclude_end? then len -= 1 end
      if beg.between?(0, self.length - 1) and len >= beg
        self.subseq(beg, len)
      else
        nil.to_vct # i.e. false
      end
    end
  end
  # alias [] vct_ref_extend
end

class Fixnum
  # no reloading (load "examp.rb")
  unless defined? 0.new_int_plus
    alias int_plus +
    def new_int_plus(other)
      case other
      when Vct, Array, Vec
        other.offset(Float(self))
      when NilClass
        self
      else
        self.int_plus(other)
      end
    end
    alias + new_int_plus
  end

  unless defined? 0.new_int_times
    alias int_times *
    def new_int_times(other)
      case other
      when Vct, Array, Vec
        other.scale(self)
      when NilClass
        0
      else
        self.int_times(other)
      end
    end
    alias * new_int_times
  end
end

class Float
  # no reloading (load "examp.rb")
  unless defined? 0.0.new_float_plus
    alias float_plus +
    def new_float_plus(other)
      case other
      when Vct, Array, Vec
        other.offset(self)
      when NilClass
        self
      else
        self.float_plus(other)
      end
    end
    alias + new_float_plus
  end

  unless defined? 0.0.new_float_times
    alias float_times *
    def new_float_times(other)
      case other
      when Vct, Array, Vec
        other.scale(self)
      when NilClass
        0.0
      else
        self.float_times(other)
      end
    end
    alias * new_float_times
  end
end

def SoundData(ary)
  assert_type((array?(ary) and vct?(ary.first)), ary, 0, "an array of vcts")
  sd = SoundData.new(ary.length, ary.first.length)
  ary.each_with_index do |v, chn| vct2sound_data(v, sd, chn) end
  sd
end

def sound_data2string(sd)
  sd.to_a.to_s
end

class SoundData
  def name
    "sound-data"
  end
  
  def to_vct(chn = 0)
    sound_data2vct(self, chn)
  end
  
  # returns an array of sd.chans vcts
  def to_a
    sound_data2vector(self)
  end

  alias sd_length length
  def length
    self.size / self.chans
  end

  def fill!(val)
    sound_data_fill!(self, val)
  end
  
  alias sd_each each
  def each(chn = nil)
    if chn
      self.length.times do |i| yield(self[chn, i]) end
    else
      self.sd_each do |val| yield(val) end
    end
  end

  def each_with_index(chn = nil)
    if chn
      self.length.times do |i| yield(self[chn, i], i) end
    else
      self.length.times do |i|
        self.chans.times do |j| yield(self[j, i], i) end
      end
    end
  end
  
  def map(chn = nil)
    sd = nil
    if chn
      sd = self.dup
      self.each_with_index(chn) do |val, i| sd[chn, i] = yield(val) end
    else
      sd = SoundData.new(self.chans, self.length)
      self.chans.times do |j|
        self.each_with_index(j) do |val, i| sd[j, i] = yield(val) end
      end
    end
    sd
  end

  def map!(chn = nil)
    if chn
      self.each_with_index(chn) do |val, i| self[chn, i] = yield(val) end
    else
      self.chans.times do |j|
        self.each_with_index(j) do |val, i| self[j, i] = yield(val) end
      end
    end
    self
  end
end

def mus_a0(gen)
  mus_xcoeff(gen, 0)
end

def set_mus_a0(gen, val)
  set_mus_xcoeff(gen, 0, val)
end

def mus_a1(gen)
  mus_xcoeff(gen, 1)
end

def set_mus_a1(gen, val)
  set_mus_xcoeff(gen, 1, val)
end

def mus_a2(gen)
  mus_xcoeff(gen, 2)
end

def set_mus_a2(gen, val)
  set_mus_xcoeff(gen, 2, val)
end

def mus_b1(gen)
  mus_ycoeff(gen, 1)
end

def set_mus_b1(gen, val)
  set_mus_ycoeff(gen, 1, val)
end

def mus_b2(gen)
  mus_ycoeff(gen, 2)
end

def set_mus_b2(gen, val)
  set_mus_ycoeff(gen, 2, val)
end

class Mus
  # clm_gen.call(a1, a2) requires 2 arguments but clm_gen.run([a1, [a2]])
  # 0, 1 or 2.
  # 
  # clm_gen.run([arg1, [arg2]])
  def run(arg1 = 0.0, arg2 = 0.0)
    mus_run(self, arg1, arg2)
  end

  def apply(*rest)
    mus_apply(self, *rest)
  end

  alias mus_inspect inspect
  def inspect
    "#<" + mus_describe(self) + ">"
  end

  def close
    mus_close(self)
  end
  
  # gen.xcoeff = 0, 0.4
  # set_mus_xcoeff(gen, index, val)
  def xcoeff=(args)
    set_mus_xcoeff(self, *args.flatten[0, 2])
  end
  
  # gen.ycoeff = 0, 0.4
  # set_mus_ycoeff(gen, index, val)
  def ycoeff=(args)
    set_mus_ycoeff(self, *args.flatten[0, 2])
  end
  
  def a0
    mus_xcoeff(self, 0)
  end

  def a0=(val)
    set_mus_xcoeff(self, 0, val)
  end
  
  def a1
    mus_xcoeff(self, 1)
  end

  def a1=(val)
    set_mus_xcoeff(self, 1, val)
  end
  
  def a2
    mus_xcoeff(self, 2)
  end

  def a2=(val)
    set_mus_xcoeff(self, 2, val)
  end
  
  def b1
    mus_ycoeff(self, 1)
  end

  def b1=(val)
    set_mus_ycoeff(self, 1, val)
  end
  
  def b2
    mus_ycoeff(self, 2)
  end

  def b2=(val)
    set_mus_ycoeff(self, 2, val)
  end
end

# base class for generators written in Ruby
class Musgen
  def initialize
    @frequency = 440.0
    @phase = 0.0
    @scaler = 1.0
    @length = 0
    @data = nil
    @increment = 0
    @interp_type = -1
    @file_name = ""
  end
  attr_accessor :frequency
  attr_accessor :phase
  attr_accessor :scaler
  attr_accessor :increment
  attr_reader   :length
  attr_reader   :data
  attr_reader   :interp_type
  attr_reader   :file_name
  
  def inspect
    format("%s.new()", self.class)
  end

  def to_s
    format("#<%s>", self.class)
  end

  def run(val1 = 0.0, val2 = 0.0)
    self.run_func(val1, val2)
  end
  alias call run

  def apply(*rest)
    self.run_func(*rest)
  end

  def eql?(other)
    self == other
  end

  def reset
    @frequency = 440.0
    @phase = 0.0
    @scaler = 1.0
    @increment = 0
    self
  end
end

class Numeric
  def positive?
    self > 0
  end

  def negative?
    self < 0
  end
end

class Integer
  def even?
    self.modulo(2) == 0
  end unless defined? 1.even?

  def odd?
    self.modulo(2) != 0
  end unless defined? 1.odd?
  
  def prime?
    (self == 2) or
    (self.odd? and 3.step(sqrt(self), 2) do |i| return false if self.modulo(i) == 0 end)
  end
end

class Float
  # step accepts floats as arguments (still implemented in newer versions)
  def step(upto, step)
    counter = self
    while counter < upto
      yield(counter)
      counter += step
    end
    counter
  end unless 1.1.respond_to?(:step)
end

class Range
  def step(n = 1, &body)
    self.to_a.step(n, &body)
  end unless defined? Range.new(0, 1).step
end

module Enumerable
  def map_with_index
    i = -1
    self.map do |x| yield(x, i += 1) end
  end

  def map_with_index!
    i = -1
    self.map! do |x| yield(x, i += 1) end
  end
  
  def cycle
    unless defined? @cycle_index then @cycle_index = 0 end
    val = self[@cycle_index % self.length]
    @cycle_index += 1
    if @cycle_index == self.length then @cycle_index = 0 end
    val
  end

  def cycle=(val)
    unless defined? @cycle_index then @cycle_index = 0 end
    self[@cycle_index % self.length] = val
    @cycle_index += 1
    if @cycle_index == self.length then @cycle_index = 0 end
    val
  end
  attr_accessor :cycle_index

  # backward compatibility methods
  def each_index
    self.each_with_index do |val, i| yield(i) end
  end unless vct(0).respond_to?(:each_index)
  
  # Enumerable#zip, new in ruby core since 19-Nov-2002.
  # a = [4, 5, 6]
  # b = [7, 8, 9]
  # [1, 2, 3].zip(a, b) --> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  # [1, 2].zip(a, b)    --> [[1, 4, 7], [2, 5, 8]]
  # a.zip([1, 2],[8])   --> [[4, 1, 8], [5, 2, nil], [6, nil, nil]]
  def zip(*objs)
    args = objs.map do |obj| obj.to_a end
    res = self.to_a
    res.each_with_index do |val, i|
      ary = [val]
      args.each do |obj| ary.push(obj[i]) end
      if block_given?
        yield(*ary)
      else
        res[i] = ary
      end
    end
    res
  end unless [].respond_to?(:zip)
end

def as_one_edit_rb(*origin, &body)
  # ruby compatibility:
  # ruby pre 1.9: lambda do end.arity != lambda do | | end.arity
  # ruby     1.9: they are even (0)
  as_one_edit(lambda do | | body.call end, origin.empty? ? "" : format(*origin))
end

def map_channel_rb(beg = 0, dur = false,
                   snd = false, chn = false, edpos = false, edname = false, &func)
  map_channel(func, beg, dur, snd, chn, edpos, edname) 
end

add_help(:map_chan_rb,
         "map_chan(func,[start=0,[end=false,[edname=false,[snd=false,[chn=false,[edpos=false]]]]]])\
  map_chan applies func to samples in the specified channel.\
  It is the old (\"irregular\") version of map_channel.")
def map_chan_rb(beg = 0, dur = false, ednam = false, snd = false, chn = false, edpos = false, &func)
  map_chan(func, beg, dur, ednam, snd, chn, edpos) 
end

class Proc
  include Info
  alias run call

  add_help(:to_method,
           "Proc#to_method(name, [klass=Object])  \
converts a Proc to a Method 'name' in the given class, default Object.  \
'name' can be a string or a symbol.

m = lambda do |*args| p args end
m.to_method(:func)
func(1, 2, 3) ==> [1, 2, 3]

lambda do |x| p x end.to_method(:foo);  foo(\"text1\") ==> \"text1\"
lambda do |x| p x end.to_method(\"bar\"); bar(\"text2\") ==> \"text2\"")
  def to_method(name, klass = Object)
    assert_type((symbol?(name) or string?(name)), name, 0, "a symbol or a string")
    assert_type((klass.kind_of?(Class) and klass.class == Class), name, 1, "a class, e.g. Object")
    name = case name
           when String
             name.intern
           when Symbol
             name
           end
    body = self
    klass.class_eval do define_method(name, body) end
  end

  # Important:
  # The following works only with newer ruby versions (I assume >=
  # 1.8.x).  Proc#inspect must return #<Proc:0x80c96a0@xxx:x> to
  # locate the source file of the procedure, not only #<Proc:0x80c96a0>!

  # Functions to_str and to_body try to search the procedure source
  # code in a file determined by to_s.  It is only a simple scanner
  # which doesn't look for the whole Ruby syntax. ;-)
  # 
  # It doesn't work if no source file exists, i.e, if the code is
  # eval'ed by the Snd listener (or in Emacs).  You must load the file
  # instead.
  # 
  # with_sound(:notehook, lambda do |name| snd_print(name) if name =~ /viol/ end) do
  #   fm_violin(0, 1, 440, 0.3)
  # end
  # 
  # $clm_notehook = lambda do |name| clm_print(name) if name =~ /viol/ end
  # 
  # with_sound do
  #   fm_violin(0, 1, 440, 0.3)
  # end
  # 
  # with_sound(:save_body, true) do
  #  ...
  # end

  # returns something like 'lambda do ... end'
  def to_str
    if body = self.source
      return body
    end
    file, line = self.to_s.sub(/>/, "").split(/@/).last.split(/:/)
    if file[0] == ?( and file[-1] == ?)
      if $VERBOSE
        warning("%s#%s: no file found for procedure %s", self.class, get_func_name, self.inspect)
      end
      body = ""
    elsif (not File.exist?(file))
      if $VERBOSE
        warning("%s#%s: Sorry, you need a higher ruby version to use Proc#to_str.
It works only with newer ruby versions (I assume >= 1.8.x).
Proc#inspect must return #<Proc:0x01234567@xxx:x> not only %s!",
             self.class, get_func_name, self.inspect)
      end
      body = ""
    else
      lineno = line.to_i
      body = ""
      blck = i = 0
      first_line = true
      File.foreach(file) do |ln|
        i += 1
        next if i < lineno
        body << ln
        if first_line
          if (ln.scan(/\s*do\b|\{/).length - ln.scan(/\s*end\b|\}/).length).zero? and
              (ln.scan(/\(/).length - ln.scan(/\)/).length).zero?
            break
          else
            first_line = false
            blck = 1
            next
          end
        end
        next if /\s*\S+\s*(if|unless|while|until)+/ =~ ln
        break if (blck += Snd_eval.count_level(ln)).zero?
        break if blck.negative?
      end
    end
    unless self.source then self.source = body end
    body
  end

  # returns the inner body without 'lambda do end'
  def to_body
    if (body = self.to_str).null?
      ""
    elsif body.split(/\n/).length == 1
      body.chomp.sub(/^(?:\s*\w+(?:\(.*\))??\s*(?:do\s+|\{\s*))(.*)\s*(?:end|\})$/, '\1').strip
    else
      body.split(/\n/)[1..-2].join("\n")
    end
  end

  # property set in g_edit_list_to_function (snd-edits.c)
  def source
    property(self.object_id, :proc_source)
  end

  def source=(val)
    set_property(self.object_id, :proc_source, val)
  end
end

def make_proc2method(name, prc)
  assert_type(func?(name), name, 0, "a symbol or a string")
  assert_type(proc?(prc), prc, 1, "a proc")
  prc.to_method(name)
end

# produces two new functions: NAME and SET_NAME
# val = 10
# make_proc_with_setter(:foo, lambda { puts val }, lambda { |a| val = a })
# foo ==> 10
# set_foo(12)
# foo ==> 12
def make_proc_with_setter(name, getter, setter)
  make_proc2method(name, getter)
  make_proc2method(format("set_%s", name).intern, setter)
end

# prc = make_proc_with_source(%(lambda do |a, b, c| puts a, b, c end))
# prc.call(1, 2, 3)
# prc.source ==> "lambda do |a, b, c| puts a, b, c end"
# 
# With the second argument BIND one can use local variables known in
# the current (or other) environment in the proc body:
# 
# os = make_oscil(:frequency, 330)
# prc = make_proc_with_source(%(lambda do 10.times do |i| p os.run end end), binding)
# puts prc.source   ==> lambda do 10.times do |i| p os.run end end
# prc.call          ==> ..., 0.748837699712728
# puts
# prc.call          ==> ..., 0.97679449812022
def make_proc_with_source(string, bind = binding)
  assert_type(string?(string), string, 0, "a string")
  assert_type(binding?(bind), bind, 1, "a binding object")
  if proc?(prc = (res = Snd.catch(:all) do eval(string, bind) end).first)
    prc.source = string
    prc
  else
    Snd.raise(:runtime_error, res, prc, string)
  end
end

make_proc_with_setter(:proc_source,
                      lambda { |prc|
                        assert_type(proc?(prc), prc, 0, "a proc")
                        prc.source
                      },
                      lambda { |prc, val|
                        assert_type(proc?(prc), prc, 0, "a proc")
                        assert_type(string?(val), val, 1, "a string")
                        prc.source = val
                      })

# Multi-line input to the Snd listener and Emacs/inf-snd.el.
# A simple parser collects multi-line input, e.g.
# 
# with_sound do
#   fm_violin(0.0, 0.1, 330, 0.1)
#   fm_violin(0.1, 0.1, 660, 0.1)
# end
# 
# and evals it.
#
# ~/.snd
# set_listener_prompt("snd> ")   # optional
# start_listener_eval            # installs read-hook for snd-listener input
# start_emacs_eval               # installs emacs-eval-hook

make_hook("$emacs_eval_hook", 1, "\
emacs_eval_hook(line):  called each time inf-snd.el sends a line to the Snd process.  \
The hook functions may do their best to deal with multi-line input; \
they can collect multi-line input and eval it by itself.  \
One example is install_eval_hooks(file, retval, input, hook, &reset_cursor) in examp.rb.")

# inf-snd.el calls this function each time a line was sent to the
# emacs buffer.
def run_emacs_eval_hook(line)
  if $emacs_eval_hook.empty?
    # without emacs-eval-hook only single line eval
    file = "(emacs-eval-hook)"
    set_snd_input(:emacs)
    begin
      Snd.display(eval(line, TOPLEVEL_BINDING, file, 1).inspect)
    rescue Interrupt, ScriptError, NameError, StandardError
      Snd.display(verbose_message_string(true, "# ", file))
    end
    set_snd_input(:snd)
    nil
  else
    $emacs_eval_hook.call(line)
  end
end

class Snd_eval
  class << Snd_eval
    Open_token = %w(class module def do { while until if unless case begin for)
    Close_token = %w(end })

    def count_level(line)
      eval_level = 0
      # skip strings and symbols which may contain reserved words
      line.gsub(/(:\w+|".+")/, "").split(/\b/).each do |s|
        case s
        when *Open_token
          eval_level += 1
        when *Close_token
          eval_level -= 1
        end
      end
      eval_level
    end
  end
end

class Snd_prompt
  # level number inserted into original prompt
  # ">"     --> "(0)>"
  # "snd> " --> "snd(0)> "
  def initialize(level)
    @listener_prompt = listener_prompt
    @base_prompt = listener_prompt.split(/(\(\d+\))?(>)?\s*$/).car.to_s
    @rest_prompt = listener_prompt.scan(/>\s*$/).car.to_s
    update(level)
  end

  def inspect
    format("#<%s %s(0)%s>", self.class, @base_prompt, @rest_prompt)
  end

  def update(level)
    set_listener_prompt(format("%s(%d)%s", @base_prompt, level, @rest_prompt))
  end

  def reset
    set_listener_prompt(@listener_prompt)
  end
end

def install_eval_hooks(file, retval, input, hook, &reset_cursor)
  eval_level = 0
  eval_line = ""
  prompt = Snd_prompt.new(eval_level)
  reset_cursor.nil? or reset_cursor.call
  $exit_hook.add_hook!(file) do prompt.reset end
  hook.add_hook!(file) do |line|
    eval_line << line << "\n"
    eval_level += Snd_eval.count_level(line)
    if eval_level.negative?
      eval_level = 0
      eval_line = ""
    end
    if eval_level.zero?
      set_snd_input(input)
      begin
        Snd.display(eval(eval_line, TOPLEVEL_BINDING, file, 1).inspect)
      rescue Interrupt, ScriptError, NameError, StandardError 
        Snd.display(verbose_message_string(true, "# ", file))
      ensure
        eval_line = ""
      end
    end
    prompt.update(eval_level)
    reset_cursor.nil? or reset_cursor.call
    retval
  end
end

# installs the emacs-eval-hook
def start_emacs_eval(name = "(emacs)")
  install_eval_hooks(name, nil, :emacs, $emacs_eval_hook) do
    $stdout.print(listener_prompt)
    $stdout.flush
  end
end

# installs the read-hook
def start_listener_eval(name = "(snd)")
  set_show_listener(true)
  install_eval_hooks(name, true, :snd, $read_hook)
end

def stop_emacs_eval(name = "(emacs)")
  $emacs_eval_hook.remove_hook!(name)
  $exit_hook.run_hook_by_name(name)
  $exit_hook.remove_hook!(name)
end

def stop_listener_eval(name = "(snd)")
  $read_hook.remove_hook!(name)
  $exit_hook.run_hook_by_name(name)
  $exit_hook.remove_hook!(name)
  reset_listener_cursor
  clm_print("\n%s", listener_prompt)
end

# Debugging resp. inspecting local variables

make_proc_with_setter(:debug_properties,
                      lambda { |name|
                        property(name, :debug_property)
                      },
                      lambda { |name, val|
                        set_property(name, :debug_property, val)
                      })

make_proc_with_setter(:debug_property,
                      lambda { |key, name|
                        hash?(h = debug_properties(name)) and h[key]
                      },
                      lambda { |key, val, name|
                        unless hash?(h = debug_properties(name)) and h.store(key, [val] + h[key])
                          unless array?(a = property(:debug, :names)) and a.push(name)
                            set_property(:debug, :names, [name])
                          end
                          set_debug_properties(name, {key, [val]})
                        end
                      })

make_proc_with_setter(:debug_binding,
                      lambda { |name|
                        debug_property(:binding, name)
                      },
                      lambda { |bind, *name|
                        assert_type(binding?(bind), bind, 0, "a binding object")
                        name = (name.car or get_func_name(3))
                        set_debug_property(:binding, bind, name)
                      })

# shows all local variables of functions prepared by set_debug_binding(binding)
# 
# def function1
#   [...]
#   set_debug_binding(binding)
# end
# 
# def function2
#   [...]
#   set_debug_binding(binding)
# end
# [...]
# 
# display_all_variables
def display_all_variables(name = nil)
  if name
    [name]
  else
    (property(:debug, :names) or [])
  end.each do |nm|
    debug_binding(nm).each do |bind|
      Snd.message("=== %s ===", nm)
      Snd.message()
      eval("local_variables", bind).each do |var|
        Snd.message("%s = %s", var, eval(var, bind).inspect)
      end
      Snd.message()
    end
  end
end

# each_variables provides all local variable names and their values in
# the given proc context
# 
# def function
#   [...]
#   each_variables do |k, v|
#     Snd.display("%s = %s", k, v)
#   end
# end
def each_variables(&prc)
  eval("local_variables", prc).each do |var| yield(var, eval(var, prc)) end
end

# let(8, :foo, "bar") do |a, b, c|
#   printf("a: %d, b: %s, c: %s\n", a, b, c)
# end
#
# Simulates a save local variable environment and restores old
# variables to their original values.
def let(*args, &prc)
  locals = Hash.new
  eval("local_variables", prc).each do |name| locals[name] = eval(name, prc) end
  # yield(*args)
  # See ruby/ChangeLog: Tue Jul 18 16:52:29 2006  Yukihiro Matsumoto  <matz@ruby-lang.org>
  prc.call(*args)
rescue Interrupt, ScriptError, NameError, StandardError
  Kernel.raise
ensure
  @locals = locals
  locals.each_key do |name| eval("#{name} = @locals[#{name.inspect}]", prc) end
  remove_instance_variable("@locals")
end

include Math

TWO_PI = PI * 2.0  unless defined? TWO_PI
HALF_PI = PI * 0.5 unless defined? HALF_PI

# for irb (rgb.rb)
def make_color(r, g, b)
  [:Pixel, 0]
end unless defined? make_color

def doc(*rest)
  # dummy for old Kernel.doc
end

require "env"
require "dsp"
require "rgb"

##
## Utilities
##

if provided? :snd_nogui
  alias close_sound_extend close_sound
else
  def close_sound_extend(snd)
    # 5 == Notebook
    if main_widgets[5] and selected_sound and selected_sound <= snd
      idx = 0
      snds = sounds() and idx = snds.index(snd)
      close_sound(snd)
      snds = sounds() and set_selected_sound(snds[idx < snds.length ? idx : -1])
    else
      close_sound(snd)
    end
  end
end

add_help(:times2samples,
         "times2samples(start, dur) \
START and DUR are in seconds; returns array [beg, end] in samples.")
def times2samples(start, dur)
  beg = seconds2samples(start)
  [beg, beg + seconds2samples(dur)]
end

def random(val)
  if val.zero?
    val
  else
    case val
    when Fixnum
      kernel_rand(val)
    when Float
      val.negative? ? -mus_random(val).abs : mus_random(val).abs
    end
  end
end

def logn(r, b = 10)
  if r <= 0 then Snd.raise(:ruby_error, r, "r must be > 0") end
  if b <= 0 or b == 1 then Snd.raise(:ruby_error, b, "b must be > 0 and != 1") end
  log(r) / log(b)
end

def car(v)
  v[0]
end

def cadr(v)
  v[1]
end

def caddr(v)
  v[2]
end

def cdr(v)
  v[1..-1]
end

def verbose_message_string(stack_p, remark, *args)
  fmt_remark = format("\n%s", remark)
  args.to_a.car = String(args.to_a.car)
  str = if args.length < 2
          args.car
        else
          format(*args)
        end
  str = if str.split(/\n/).length > 1
          str.split(/\n/).join(fmt_remark)
        else
          format("%s%s", remark, args.car)
        end
  if $!
    str += format("[%s] %s (%s)", rb_error_to_mus_tag.inspect, snd_error_to_message, $!.class)
    if stack_p then str += format("\n%s%s", remark, $!.backtrace.join(fmt_remark)) end
  else
    if stack_p and caller(2) then str += format("\n%s%s", remark, caller(2).join(fmt_remark)) end
  end
  str
end

def warning(*args)
  str = "Warning: " << verbose_message_string($VERBOSE, nil, *args)
  if provided? :snd
    snd_warning(str)
    nil
  else
    clm_message(str)
  end
end

def die(*args)
  message(verbose_message_string(true, nil, *args))
  exit(1) unless provided? :snd
end

def error(*args)
  Snd.raise(:runtime_error, verbose_message_string(true, nil, *args))
end

make_proc_with_setter(:snd_input,
                      lambda { property(:snd_input, :snd_listener) },
                      lambda { |val| set_property(:snd_input, :snd_listener, val) })

# like clm_print(fmt, *args)

def clm_message(*args)
  msg = if args.null?
          ""
        elsif args.length == 1
          String(args.car)
        else
          format(*args)
        end
  if provided?(:snd) and !provided?(:snd_nogui)
    clm_print("\n%s", msg)
    if ENV["EMACS"]
      $stdout.print(msg, "\n")
    else
      nil
    end
  else
    $stdout.print(msg, "\n")
  end
end

# like clm_print(*args), in emacs it prepends msg with a comment sign

def message(*args)
  msg = if args.null?
          ""
        elsif args.length == 1
          String(args.car)
        else
          format(*args)
        end
  clm_message(verbose_message_string(false, "# ", msg))
end

# debug(var1, var2) --> #<DEBUG: ClassName: value1, ClassName: value2>

def debug(*args)
  fmt = ""
  args.each do |arg|
    fmt += format("%s: %s", arg.class, arg.inspect)
    fmt += ", "
  end
  message("#<DEBUG: %s>", fmt.chomp(", "))
end

def debug_trace(*args)
  debug(*args)
  clm_message(verbose_message_string(true, "# "))
end

if provided? :snd then set_snd_input(:snd) end

class Snd
  class << Snd
    Snd_path = Array.new

    if provided? :snd
      def add_sound_path(path)
        Snd_path.push(path)
        add_directory_to_view_files_list(path)
      end
      
      def open_from_path(fname)
        find_sound(snd_file = Snd.fullname(fname)) or open_sound(snd_file)
      end
      
      def find_from_path(fname)
        find_sound(Snd.fullname(fname))
      end
    else
      def add_sound_path(path)
        Snd_path.push(path)
      end
    end

    def fullname(fname)
      if File.exist?(fname)
        fname
      else
        f = File.basename(fname)
        callcc do |brk|
          Snd_path.each do |path|
            File.exist?(path + "/" + f) and brk.call(path + "/" + f)
          end
          Snd.raise(:no_such_file, fname)
        end
      end
    end
    
    def load_path
      Snd_path
    end

    def message(*args)
      msg = if args.null?
              ""
            elsif args.length == 1
              String(args.car)
            else
              format(*args)
            end
      clm_message(verbose_message_string(false, "# ", msg))
    end

    def display(*args)
      args[0] = String(args[0])
      msg = format(*args)
      if snd_input == :snd
        snd_print("\n" + msg)
        if $VERBOSE then $stdout.print(msg, "\n") end
        nil
      else
        $stdout.print(msg, "\n")
      end
    end
    
    def warning(*args)
      if provided? :snd
        snd_warning(verbose_message_string($VERBOSE, nil, *args))
        nil
      else
        args[0] = "Warning: " + String(args[0])
        Snd.display(verbose_message_string($VERBOSE, "# ", *args))
      end
    end

    def die(*args)
      Snd.display(verbose_message_string(true, nil, *args))
      exit(1) unless provided? :snd
    end

    def error(*args)
      Snd.raise(:runtime_error, verbose_message_string(true, nil, *args))
    end

    def debug(*args)
      fmt = ""
      args.each do |arg|
        fmt += format("%s: %s", arg.class, arg.inspect)
        fmt += ", "
      end
      Snd.message("#<DEBUG: %s>", fmt.chomp(", "))
    end

    def debug_trace(*args)
      Snd.debug(*args)
      Snd.display(verbose_message_string(true, "# "))
    end

    def sounds
      (Kernel.sounds or []).reverse
    end

    def regions
      (Kernel.regions or []).reverse
    end

    def tracks
      (Kernel.tracks or []).reverse
    end
    
    def marks(snd = false, chn = false)
      (Kernel.marks(snd, chn) or [])
    end

    def snd(sn = false)
      sn or selected_sound or Snd.sounds.car
    end

    def chn(ch = false)
      ch or selected_channel or 0
    end

    def catch(tag = :all, retval = :undefined)
      old_debug = $DEBUG
      $DEBUG = false
      val = Kernel.catch(tag) do yield end
      # catch/throw part
      if array?(val) and val.car == :snd_throw # [:snd_throw, tag, get_func_name(2), *rest]
        if retval != :undefined
          if proc?(retval)
            retval.call(val.cdr)
          else
            [retval]
          end
        else
          val.cdr
        end
      else
        [val]
      end
      # ruby1.9/ChangeLog
      # Thu Feb  2 16:01:24 2006  Yukihiro Matsumoto  <matz@ruby-lang.org>
      # 	* error.c (Init_Exception): change NameError to direct subclass of
      # 	  Exception so that default rescue do not handle it silently.
    rescue Interrupt, ScriptError, NameError, StandardError
      # raise part
      if (tag == (mus_tag = rb_error_to_mus_tag) or tag == :all)
        if retval != :undefined
          if proc?(retval)
            retval.call(mus_tag, snd_error_to_message)
          else
            [retval]
          end
        else
          [mus_tag, snd_error_to_message]
        end
      else
        Kernel.raise
      end
    ensure
      $DEBUG = old_debug
    end

    def throw(tag, *rest)
      Kernel.throw(tag, [:snd_throw, tag, get_func_name(2), *rest])
    end

    def raise(tag, *rest)
      msg = format("%s: %s:", get_func_name(2), tag)
      rest.each do |s| msg += format(" %s,", s) end
      msg.chomp!(",")
      exception = case tag
                  when :out_of_range
                    RangeError
                  when :wrong_type_arg
                    TypeError
                  when *Snd_error_tags
                    StandardError
                  else
                    Ruby_exceptions[tag] or RuntimeError
                  end
      Kernel.raise(exception, msg, caller(1))
    end
  end
end

# nearly all are instances of StandardError
Snd_error_tags = [# clm2xen.c
                  :mus_error,
                  :arg_error,
                  # snd-0.h
                  :no_such_track,
                  :no_such_envelope,
                  :no_such_sample,
                  :no_such_edit,
                  :cannot_save,
                  :cant_update_file,
                  # snd-chn.c
                  :cant_open_file,
                  # snd-dac.c
                  :bad_format,
                  :no_such_player,
                  # snd-draw.c
                  :no_such_widget,
                  :no_such_graphics_context,
                  :no_such_axis,
                  :bad_length,
                  # snd-edits.c
                  :no_such_direction,
                  :no_such_region,
                  # snd-env.c
                  :env_error,
                  # snd-error.c
                  :snd_error,
                  # snd-gxcolormaps.c
                  :no_such_colormap,
                  :colormap_error,
                  # snd-key.c
                  :no_such_key,
                  # snd-ladspa.c
                  :no_such_plugin,
                  :plugin_error,
                  # snd-main.c
                  :memory_error,
                  # snd-marks.c
                  :no_such_mark,
                  # snd-menu.c
                  :no_such_menu,
                  # snd-mix.c
                  :no_such_mix,
                  :io_error,
                  # snd-print.c
                  :cannot_print,
                  # snd-run.c
                  :wrong_number_of_args,
                  :cannot_parse,
                  # snd-snd.c
                  :no_such_sound,
                  :not_a_sound_file,
                  :cannot_apply_controls,
                  :bad_size,
                  :snd_internal_error,
                  # snd-xdraw.c
                  :no_current_font,
                  # snd-xen.c
                  :no_active_selection,
                  :bad_arity,
                  # snd-xmain.c
                  :xt_error,
                  # snd-xxen.c
                  :no_such_color,
                  # snd.c
                  :gsl_error,
                  # sndlib2xen.h
                  :no_such_channel,
                  :no_such_file,
                  :bad_type,
                  :no_data,
                  :bad_header,
                  # xen.h
                  :out_of_range,
                  :wrong_type_arg]              # TypeError

def rb_error_to_mus_tag
  # to_s and string error-names intended here
  # otherwise e.g. NameError goes to case StandardError!
  case $!.class.to_s
    # case 1
    # No_such_file: file->array /baddy/hiho No such file or directory
    # case 2
    # insert_region: No_such_region: 1004
    # case 3 (mus_error)
    # mus_ycoeff__invalid_index_123__order___3?: Mus_error
    # can't translate /usr/gnu/sound/sf1/oboe.g721 to /usr/gnu/sound/sf1/oboe.g721.snd:
    #   : Mus_error>
  when "StandardError"
    # err = $!.message.split(/\n/).first.downcase.split(/:/).map do |e| e.strip.chomp(">") end
    err = $!.message.delete("\n").downcase.split(/:/).compact.map do |e| e.strip.chomp(">") end
    Snd_error_tags.detect do |tag| err.member?(tag.to_s) end or :standard_error
  when "RangeError"
    :out_of_range
  when "TypeError"
    :wrong_type_arg
  when "ArgumentError"
    :wrong_number_of_args
  else
    # converts ruby exceptions to symbols: NoMethodError --> :no_method_error
    $!.class.to_s.gsub(/([A-Z])/) do |c| "_" + c.tr("A-Z", "a-z") end[1..-1].intern
  end
end

def snd_error_to_message
  err = $!.message.split(/:/).map do |e| e.strip.chomp("\n") end
  str = err.join(": ")
  if err.length > 1 and (len = str.scan(/~A/).length).positive?
    str.gsub!(/~A/, "%s")
    str = if $!.class == RangeError
            format(str.strip, if string?(s = err.cadr.split(/,/)[1..-2].join(","))
                                eval(s)
                              else
                                0
                              end)
          else
            format(str.strip, *if string?(s = str.slice!(str.index("[")..str.index("]")))
                                eval(s)
                              else
                                [0] * len
                              end)
          end
  end
  str.gsub(rb_error_to_mus_tag.to_s.capitalize + ": ", "")
rescue Interrupt, ScriptError, NameError, StandardError
  if $DEBUG
    $stderr.printf("# Warning (%s)\n", get_func_name)
    each_variables do |k, v| $stderr.printf("# %s = %s\n", k, v.inspect) end
  end
  str
end

add_help(:snd_catch,
         "snd_catch([tag=:all, [retval=:undefined]])  \
catchs snd_throw and exceptions and \
returns body's last value wrapped in an array if all goes well.  \
If a snd_throw tag meets snd_catch's, returns an array with the tag name, \
the function name from where was thrown and optional arguments given to snd_throw.  \
If an exception was risen and the exception name meets tag name, \
returns an array with tag name and the exception message, otherwise reraises exception.  \
If retval is given and tag matches exception or snd_throw tag, returns retval.  \
If retval is a procedure, calls retval with tag name and message.

res = snd_catch do 10 + 2 end
puts res ==> [12]

res = Snd.catch(:no_such_file) do
  open_sound(\"unknown-file.snd\")
end
puts res ==> [:no_such_file,
             \"open_sound: no_such_file: Unknown_file.snd No such file or directory\"]

res = Snd.catch(:finish) do
  10.times do |i|
    if i == 8 then snd_throw(:finish, i) end
  end
end
puts res ==> [:finish, \"top_level\", 8]

res = Snd.catch(:all, lambda do |tag, msg| Snd.display([tag, msg]) end) do
  set_listener_prompt(17)
end
==> [:wrong_type_arg, \"set_listener-prompt: wrong type arg 0, 17, wanted a string\"]
puts res ==> nil

The lambda function handles the error in the last case.")
def snd_catch(tag = :all, retval = :undefined, &body)
  Snd.catch(tag, retval, &body)
end

add_help(:snd_throw,
         "snd_throw(tag, *rest)  \
jumps to the corresponding snd_catch('tag') and returns an array \
with tag, function name and possible *rest strings or values.")
def snd_throw(tag, *rest)
  Snd.throw(tag, *rest)
end

class Break < StandardError
end

Ruby_exceptions = {
  :script_error,          ScriptError,
  :load_error,            LoadError,
  :name_error,            NameError,
  :not_implemented_error, NotImplementedError,
  :syntax_error,          SyntaxError,
  :interrupt,             Interrupt,
  :system_exit,           SystemExit,
  :standard_error,        StandardError,
  :arg_error,             ArgumentError,
  :float_domain_error,    FloatDomainError,
  :index_error,           IndexError,
  :io_error,              IOError,
  :eof_error,             EOFError,
  :local_jump_error,      LocalJumpError,
  :no_memory_error,       NoMemoryError,
  :range_error,           RangeError,
  :regexp_error,          RegexpError,
  :runtime_error,         RuntimeError,
  :security_error,        SecurityError,
  :system_call_error,     SystemCallError,
  :system_stack_error,    SystemStackError,
  :thread_error,          ThreadError,
  :type_error,            TypeError,
  :zero_division_error,   ZeroDivisionError,
  :break,                 Break}

add_help(:snd_raise,
         "snd_raise(tag, *rest)  \
raises an exception 'tag' with an error message \
containing function name, tag and possible *rest strings or values.  \
'tag' is a symbol, \
a Ruby exception looks like :local_jump_error instead of LocalJumpError, \
a Snd error tag looks like :no_such_sound.")
def snd_raise(tag, *rest)
  Snd.raise(tag, *rest)
end

# for irb
def c_g?
  false
end unless defined? c_g?

def srate
  mus_srate
end unless defined? srate

# general purpose loop

add_help(:gloop,
         "gloop(*args) { |args| ... }
 :step   = 1
 :before = nil (thunk)
 :after  = nil (thunk)

args[0]: Range    (each)
         Hash(s)  (each)
         Array(s) (each_with_index) [args.last == Fixnum --> step]
         Fixnum   (times)
         Fixnum   [args[1] == :step --> step]

A general purpose loop, handling Range, Hash, Array, Vec, Vct, Fixnum,
with optional step.  Returns the result of body as array like map.

Examples:
  Range
    gloop(0..3) do |i| puts i end
  Hash               (loops over all Hashs consecutively)
    gloop({1, :a, 2, :b}, {11, :aa, 22, :bb}) do |k, v|
      print('key: ', k, ' value: ', v)
      puts
    end
  Array, Vec, Vct
    gloop([0, 1]) do |x, i|
      print(i, ': ', x)
      puts end
  Arrays with step   (mixes all Arrays)
    gloop([0, 1, 2, 3], [:a, :b, :c, :d], [55, 66, 77, 88, 99], 2) do |x, i|
      print(i, ': ', x.inspect)
      puts
    end
  Numeric (like Integer#times)
    gloop(3) do |i| puts i end
  Numeric with step (like Integer#step)
    gloop(6, 2) do |i| puts i end
  a simple body call
    gloop do puts 'empty' end")
def gloop(*args, &body)
  step   = get_shift_args(args, :step, 1)
  before = get_shift_args(args, :before)
  after  = get_shift_args(args, :after)
  do_extra = lambda do |thunk| thunk?(thunk) ? thunk.call : snd_func(thunk) end
  result = []
  case args[0]
  when Range
    args[0].step(step) do |i|
      do_extra.call(before) if before
      result << body.call(i)
      do_extra.call(after) if after
    end
  when Array, Vec, Vct
    lmax = args.map do |x| x.length end.max
    0.step(lmax - 1, step.round) do |i|
      do_extra.call(before) if before
      result << body.call(*args.map do |x| x[i] end << i)
      do_extra.call(after) if after
    end
  when Hash
    args.each do |x| x.each do |k, v|
        do_extra.call(before) if before
        result << body.call(k, v)
        do_extra.call(after) if after
      end
    end
  when Numeric
    0.step(args[0], number?(args[1]) ? args[1] : step) do |i|
      do_extra.call(before) if before
      result << body.call(i)
      do_extra.call(after) if after
    end
  else
    do_extra.call(before) if before
    result << body.call
    do_extra.call(after) if after
  end
  result
end

# get_args(args, key, default = nil)
#
# returns value, whether DEFAULT or value of KEY found in ARGS

def get_args(args, key, default = nil)
  if args.member?(key)
    arg = args[args.index(key) + 1]
    default = arg.nil? ? default : arg
  end
  default
end

def get_shift_args(args, key, default = nil)
  default = get_args(args, key, default)
  if args.member?(key)
    i = args.index(key)
    2.times do args.delete_at(i) end
  end
  default
end

# var = get_class_or_key(args, Klass, :key, default = nil)

def get_class_or_key(args, klass, key, default = nil)
  if (not symbol?(args.first)) and args.first.kind_of?(klass)
    args.shift
  else
    get_shift_args(args, key, default)
  end
end

# var1, var2, var3, var4 = optkey(args, [:key, default],
#                                       [:number, 1],
#                                       [Array, :list, [0, 1, 2, 3]],
#                                       :var_w/o_default_value)
#
# Key-default pairs must be included in brackets while keys alone can
# be included in brackets or not, see last key
# ":var_w/o_default_value" above.  If no default value is specified,
# nil is used.

def optkey(args, *rest)
  args_1 = args.dup
  bind = binding?(rest.car) ? rest.shift : nil
  @locals = nil
  vals = rest.map do |keys|
    val = if array?(keys)
            case keys.length
            when 1
              name = keys.car.to_s
              get_class_or_key(args_1, Object, keys.car, nil)
            when 2
              name = keys.car.to_s
              get_class_or_key(args_1, keys.cadr.class, *keys)
            when 3
              name = keys.cadr.to_s
              get_class_or_key(args_1, *keys)
            else
              assert_type(keys.length.between?(1, 3), keys, 1,
                          "an array of one to three elements [class, :key, default]")
            end
          else
            name = keys.to_s
            get_class_or_key(args_1, Object, keys, nil)
          end
    @locals = val
    eval("#{name} = @locals", bind)
    val
  end
  remove_instance_variable("@locals")
  if vals.length == 1
    vals.first
  else
    vals
  end
end

add_help(:load_init_file,
         "load_init_file(file) \
Returns false if file doesn't exist, otherwise loads it. \
File may reside in current working dir or in $HOME dir.")
def load_init_file(file)
  if File.exist?(file)
    Snd.catch do load(file) end
  elsif File.exist?(f = ENV["HOME"] + "/" + file)
    Snd.catch do load(f) end
  else
    false
  end
end

let(-1) do |count|
  # see rotate_phase(func, snd, chn) in dsp.rb
  # it's necessary to produce a uniq method name
  make_proc_with_setter(:edit_list_proc_counter, lambda { count }, lambda { count += 1 })
end

module Examp
  # (ext)snd.html examples made harder to break
  #
  # this mainly involves keeping track of the current sound/channel

  add_help(:selection_rms, "selection_rms() -> rms of selection data using sample readers")
  def selection_rms
    if selection?
      reader = make_sample_reader(selection_position, false, false)
      len = selection_frames
      sum = 0.0
      len.times do
        val = next_sample(reader)
        sum += val * val
      end
      free_sample_reader(reader)
      sqrt(sum / len)
    else
      Snd.raise(:no_active_selection)
    end
  end

  add_help(:region_rms, "region_rms([n=0]) -> rms of region n's data (chan 0)")
  def region_rms(n = 0)
    if region?(n)
      data = region2vct(0, 0, n)
      sqrt(dot_product(data, data) / data.length)
    else
      Snd.raise(:no_such_region)
    end
  end

  add_help(:window_samples, "window_samples([snd=false, [chn=false]]) \
-> samples in snd channel chn in current graph window")
  def window_samples(snd = false, chn = false)
    wl = left_sample(snd, chn)
    wr = right_sample(snd, chn)
    channel2vct(wl, 1 + (wr - wl), snd, chn)
  end

  add_help(:display_energy,
           "display_energy(snd, chn) \
is a $lisp_graph_hook function to display the time domain data as energy (squared).
$lisp_graph_hook.add_hook!(\"display-energy\", &method(:display_energy).to_proc)")
  def display_energy(snd, chn)
    ls = left_sample(snd, chn)
    rs = right_sample(snd, chn)
    datal = make_graph_data(snd, chn)
    data = vct?(datal) ? datal : datal[1]
    sr = srate(snd)
    y_max = y_zoom_slider(snd, chn)
    if data and ls and rs
      vct_multiply!(data, data)
      graph(data, "energy", ls / sr, rs / sr, 0.0, y_max * y_max, snd, chn)
    end
  end

  add_help(:display_db,
           "display_db(snd, chn) \
is a lisp-graph-hook function to display the time domain data in dB.
$lisp_graph_hook.add_hook!(\"display-db\", &method(:display_db).to_proc)")
  def display_db(snd, chn)
    if datal = make_graph_data(snd, chn)
      dB = lambda do |val|
        if val < 0.001
          -60.0
        else
          20.0 * log10(val)
        end
      end
      data = vct?(datal) ? datal : datal[1]
      sr = srate(snd)
      ls = left_sample(snd, chn)
      rs = right_sample(snd, chn)
      data.map! do |val| 60.0 + dB.call(val.abs) end
      graph(data, "dB", ls / sr, rs / sr, 0.0, 60.0, snd, chn)
    end
  end

  add_help(:window_rms, "window_rms() -> rms of data in currently selected graph window")
  def window_rms
    ls = left_sample
    rs = right_sample
    data = channel2vct(ls, 1 + (rs - ls))
    sqrt(dot_product(data, data), data.length)
  end

  add_help(:fft_peak,
           "fft_peak(snd, chn, scale)  returns the peak spectral magnitude
$after_transform_hook.add_hook!(\"fft-peak\") do |snd, chn, scale|
  fft_peak(snd, chn, scale)
end")
  def fft_peak(snd, chn, scale)
    if transform_graph? and transform_graph_type == Graph_once
      report_in_minibuffer(((2.0 * vct_peak(transform2vct(snd, chn))) / transform_size).to_s, snd)
    else
      false
    end
  end

  # 'info' from extsnd.html using format

  add_help(:finfo, "finfo(file) -> description (as a string) of file")
  def finfo(file)
    chans = mus_sound_chans(file)
    sr = mus_sound_srate(file)
    format("%s: chans: %d, srate: %d, %s, %s, len: %1.3f",
           file, chans, sr,
           mus_header_type_name(mus_sound_header_type(file)),
           mus_data_format_name(mus_sound_data_format(file)),
           mus_sound_samples(file).to_f / (chans * sr.to_f))
  end

  # Correlation
  #
  # correlation of channels in a stereo sound

  add_help(:correlate,
           "correlate(snd, chn, y0, y1) \
returns the correlation of snd's 2 channels (intended for use with $graph_hook)
$graph_hook.add_hook!(\"correlate\") do |snd, chn, y0, y1|
  correlate(snd, chn, y0, y1)
end")

  def correlate(snd, chn, y0, y1)
    if channels(snd) == 2 and frames(snd, 0) > 1 and frames(snd, 1) > 1
      ls = left_sample(snd, 0)
      rs = right_sample(snd, 0)
      ilen = 1 + (rs - ls)
      pow2 = (log(ilen) / log(2)).ceil
      fftlen = (2 ** pow2).to_i
      fftlen2 = fftlen / 2
      fftscale = 1.0 / fftlen
      rl1 = channel2vct(ls, fftlen, snd, 0)
      rl2 = channel2vct(ls, fftlen, snd, 1)
      im1 = make_vct(fftlen)
      im2 = make_vct(fftlen)
      fft(rl1, im1, 1)
      fft(rl2, im2, 1)
      tmprl = vct_copy(rl1)
      tmpim = vct_copy(im1)
      data3 = make_vct(fftlen2)
      vct_multiply!(tmprl, rl2)
      vct_multiply!(tmpim, im2)
      vct_multiply!(im2, rl1)
      vct_multiply!(rl2, im1)
      vct_add!(tmprl, tmpim)
      vct_subtract!(im2, rl2)
      fft(tmprl, im2, -1)
      vct_add!(data3, tmprl)
      vct_scale!(data3, fftscale)
      graph(data3, "lag time", 0, fftlen2)
    else
      report_in_minibuffer("correlate wants stereo input")
    end
  end

  # set transform-size based on current time domain window size
  # 
  # also zoom spectrum based on y-axis zoom slider

  add_help(:zoom_spectrum,
           "zoom_spectrum(snd, chn, y0, y1) \
sets the transform size to correspond to the time-domain window size (use with $graph_hook)
$graph_hook.add_hook!(\"zoom-spectrum\") do |snd, chn, y0, y1|
  zoom_spectrum(snd, chn, y0, y1)
end")
  def zoom_spectrum(snd, chn, y0, y1)
    if transform_graph?(snd, chn) and transform_graph_type(snd, chn) == Graph_once
      set_transform_size((log(right_sample(snd, chn) - left_sample(snd, chn)) / log(2.0)).ceil,
                         snd, chn)
      set_spectro_cutoff(y_zoom_slider(snd, chn), snd, chn)
    end
    false
  end

  add_help(:zoom_fft,
           "zoom_fft(snd, chn, y0, y1) \
sets the transform size if the time domain is not displayed (use with $graph_hook) 
It also sets the spectrum display start point based on the x position slider---\
this can be confusing if fft normalization is on (the default)
$graph_hook.add_hook!(\"zoom-fft\") do |snd, chn, y0, y1|
  zoom_fft(snd, chn, y0, y1)
end")
  def zoom_fft(snd, chn, y0, y1)
    if transform_graph?(snd, chn) and
        (not time_graph?(snd, chn)) and
        transform_graph_type(snd, chn) == Graph_once
      set_transform_size(2 ** (log(right_sample(snd, chn) - left_sample(snd, chn)) / log(2.0)).ceil,
                         snd, chn)
      set_spectro_start(x_position_slider(snd, chn), snd, chn)
      set_spectro_cutoff(y_zoom_slider(snd, chn), snd, chn)
    end
    false
  end

  # superimpose spectra of sycn'd sounds

  add_help(:superimpose_ffts,
           "superimpose_ffts(snd, chn, y0, y1) \
superimposes ffts of multiple (syncd) sounds (use with $graph_hook)
$graph_hook.add_hook!(\"superimpose-ffts\") do |snd, chn, y0, y1|
  superimpose_ffts(snd, chn, y0, y1)
end")
  def superimpose_ffts(snd, chn, y0, y1)
    maxsync = Snd.sounds.map do |s| sync(s) end.max
    if sync(snd) > 0 and
        snd == Snd.sounds.map do |s| sync(snd) == sync(s) ? s : maxsync + 1 end.min
      ls = left_sample(snd, chn)
      rs = right_sample(snd, chn)
      pow2 = (log(rs - ls) / log(2)).ceil
      fftlen = (2 ** pow2).to_i
      if pow2 > 2
        ffts = []
        Snd.sounds.each do |s|
          if sync(snd) == sync(s) and channels(s) > chn
            fdr = channel2vct(ls, fftlen, s, chn)
            fdi = make_vct(fftlen)
            spectr = make_vct(fftlen / 2)
            ffts.push(spectr.add(spectrum(fdr, fdi, false, 2)))
          end
        end
        graph(ffts, "spectra", 0.0, 0.5, false, false, snd, chn)
      end
    end
    false
  end

  # c-g? example (Anders Vinjar)

  add_help(:locate_zero,
           "locate_zero(limit) \
looks for successive samples that sum to less than 'limit', moving the cursor if successful")
  def locate_zero(limit)
    start = cursor
    sf = make_sample_reader(start, false, false)
    val0 = sf.call.abs
    val1 = sf.call.abs
    n = start
    until sample_reader_at_end?(sf) or c_g? or val0 + val1 < limit
      val0, val1 = val1, sf.call.abs
      n += 1
    end
    free_sample_reader(sf)
    set_cursor(n)
  end
  
  # make a system call from irb or snd listener
  # 
  #   shell("df") for example
  # or to play a sound whenever a file is closed:
  #   $close-hook.add_hook!() do |snd| shell("sndplay wood16.wav"); false end

  add_help(:shell, "shell(cmd, *rest) \
sends 'cmd' to a shell (executes it as a shell command) and returns the result string.")
  def shell(cmd, *rest)
    str = ""
    unless cmd.null?
      IO.popen(format(cmd, *rest), "r") do |f| str = f.readlines.join end
    end
    str
  end

  # translate mpeg input to 16-bit linear and read into Snd
  # 
  # mpg123 with the -s switch sends the 16-bit (mono or stereo) representation of
  #   an mpeg file to stdout.  There's also apparently a switch to write 'wave' output.

  add_help(:mpg, "mpg(file, tmpname) converts file from MPEG to raw 16-bit samples using mpg123
mpg(\"mpeg.mpg\", \"mpeg.raw\")")
  def mpg(mpgfile, rawfile)
    b0 = b1 = b2 = b3 = 0
    File.open(mpgfile, "r") do |fd|
      b0 = fd.readchar
      b1 = fd.readchar
      b2 = fd.readchar
      b3 = fd.readchar
    end
    if b0 != 255 or (b1 & 0b11100000) != 0b11100000
      Snd.display("%s is not an MPEG file (first 11 bytes: %b %b",
                  mpgfile.inspect, b0, b1 & 0b11100000)
    else
      id = (b1 & 0b11000) >> 3
      layer = (b1 & 0b110) >> 1
      srate_index = (b2 & 0b1100) >> 2
      channel_mode = (b3 & 0b11000000) >> 6
      if id == 1
        Snd.display("odd: %s is using a reserved Version ID", mpgfile.inspect)
      end
      if layer == 0
        Snd.display("odd: %s is using a reserved layer description", mpgfile.inspect)
      end
      chans = channel_mode == 3 ? 1 : 2
      mpegnum = id.zero? ? 4 : (id == 2 ? 2 : 1)
      mpeg_layer = layer == 3 ? 1 : (layer == 2 ? 2 : 3)
      srate = [44100, 48000, 32000, 0][srate_index] / mpegnum
      Snd.display("%s: %s Hz, %s, MPEG-%s",
                  mpgfile.inspect, srate, chans == 1 ? "mono": "stereo", mpeg_layer)
      system(format("mpg123 -s %s > %s", mpgfile, rawfile))
      open_raw_sound(rawfile, chans, srate, little_endian? ? Mus_lshort : Mus_bshort)
    end
  end

  # read and write OGG files

  add_help(:read_ogg,
           "read_ogg(filename) read OGG files
$open_hook.add_hook!(\"read-ogg\") do |filename|
  if mus_sound_header_type(filename) == Mus_raw
    read_ogg(filename)
  else
    false
  end
end")
  def read_ogg(filename)
    flag = false
    File.open(filename, "r") do |fd|
      flag = fd.readchar == ?O and fd.readchar == ?g and fd.readchar == ?g and fd.readchar == ?S
    end
    if flag
      aufile = filename + ".au"
      File.unlink(aufile) if File.exist?(aufile)
      system(format("ogg123 -d au -f %s %s", aufile, filename))
      aufile
    else
      false
    end
  end

  def write_ogg(snd)
    if edits(snd)[0] > 0 or header_type(snd) != Mus_riff
      file = file_name(snd) + ".tmp"
      save_sound_as(file, snd, Mus_riff)
      system("oggenc " + file)
      File.unlink(file)
    else
      system("oggenc " + file_name(snd))
    end
  end

  # read and write Speex files

  def read_speex(filename)
    wavfile = filename + ".wav"
    File.unlink(wavfile) if File.exist?(wavfile)
    system(format("speexdec %s %s", filename, wavfile))
    wavfile
  end

  def write_speex(snd)
    if edits(snd)[0] > 0 or header_type(snd) != Mus_riff
      file = file_name(snd) + ".wav"
      spxfile = file_name(snd) + "spx"
      save_sound_as(file, snd, Mus_riff)
      system(format("speexenc %s %s", file, spxfile))
      File.unlink(file)
    else
      system(format("speexenc %s %s", file_name(snd), spxfile))
    end
  end

  # read and write FLAC files

  def read_flac(filename)
    system(format("flac -d %s", filename))
  end

  def write_flac(snd)
    if edits(snd)[0] > 0 or header_type(snd) != Mus_riff
      file = file_name(snd) + ".wav"
      save_sound_as(file, snd, Mus_riff)
      system(format("flac %s", file))
      File.unlink(file)
    else
      system(format("flac %s ", file_name(snd)))
    end
  end

  # read ASCII files
  #
  # these are used by Octave (WaveLab) -- each line has one integer,
  # apparently a signed short.

  def read_ascii(in_filename,
                 out_filename = "test.snd",
                 out_type = Mus_next,
                 out_format = Mus_bshort,
                 out_srate = 44100)
    in_buffer = IO.readlines(in_filename)         # array of strings
    out_snd = new_sound(out_filename, out_type, out_format, out_srate, 1,
                       format("created by %s: %s", get_func_name, in_filename))
    bufsize = 512
    data = make_vct(bufsize)
    loc = 0
    frame = 0
    short2float = 1.0 / 32768.0
    as_one_edit_rb do | |
      in_buffer.each do |line|
        line.split.each do |str_val|
          val = eval(str_val)
          data[loc] = val * short2float
          loc += 1
          if loc == bufsize
            vct2channel(data, frame, bufsize, out_snd, 0)
            frame += bufsize
            loc = 0
          end
        end
      end
      if loc > 0
        vct2channel(data, frame, loc, out_snd, 0)
      end
    end
  end

  # make dot size dependent on number of samples being displayed
  #
  # this could be extended to set time_graph_style to Graph_lines if
  # many samples are displayed, etc

  add_help(:auto_dot,
           "auto_dot(snd, chn, y0, y1) \
sets the dot size depending on the number of samples being displayed (use with $graph_hook)
$graph_hook.add_hook!(\"auto-dot\") do |snd, chn, y0, y1|
  auto_dot(snd, chn, y0, y1)
end")
  def auto_dot(snd, chn, y0, y1)
    dots = right_sample(snd, chn) - left_sample(snd, chn)
    case dots
    when 100
      set_dot_size(1, snd, chn)
    when 50
      set_dot_size(2, snd, chn)
    when 25
      set_dot_size(3, snd, chn)
    else
      set_dot_size(5, snd, chn)
    end
    false
  end

  # move window left edge to mark upon 'm'
  # 
  # in large sounds, it can be pain to get the left edge of the window
  # aligned with a specific spot in the sound.  In this code, we
  # assume the desired left edge has a mark, and the 'm' key (without
  # control) will move the window left edge to that mark.

  add_help(:first_mark_in_window_at_left,
           "first_mark_in_window_at_left() \
moves the graph so that the leftmost visible mark is at the left edge
bind_key(?m, 0, lambda do | | first_mark_in_window_at_left end)")
  def first_mark_in_window_at_left
    keysnd = Snd.snd
    keychn = Snd.chn
    current_left_sample = left_sample(keysnd, keychn)
    chan_marks = marks(keysnd, keychn)
    if chan_marks.null?
      report_in_minibuffer("no marks!")
    else
      leftmost = chan_marks.map do |m| mark_sample(m) end.detect do |m| m > current_left_sample end
      if leftmost.null?
        report_in_minibuffer("no mark in window")
      else
        set_left_sample(leftmost, keysnd, keychn)
        Keyboard_no_action
      end
    end
  end
  
  # flash selected data red and green

  add_help(:flash_selected_data,
           "flash_selected_data(millisecs) causes the selected data to flash red and green")
  def flash_selected_data(interval)
    if selected_sound
      set_selected_data_color(selected_data_color == Red ? Green : Red)
      call_in(interval, lambda do | | flash_selected_data(interval) end)
    end
  end

  # use loop info (if any) to set marks at loop points

  add_help(:mark_loops,
           "mark_loops() places marks at loop points found in the selected sound's header")
  def mark_loops
    loops = (sound_loop_info or mus_sound_loop_info(file_name))
    if loops and !loops.empty?
      unless loops[0].zero? and loops[1].zero?
        add_mark(loops[0])
        add_mark(loops[1])
        unless loops[2].zero? and loops[3].zero?
          add_mark(loops[2])
          add_mark(loops[3])
        end     
      end
    else
      Snd.display("%s has no loop info", short_file_name.inspect)
    end
  end

  # mapping extensions (map arbitrary single-channel function over
  # various channel collections)

  add_help(:do_all_chans,
           "do_all_chans(edhist) do |y| ... end  \
applies func to all active channels, using edhist as the edit history indication:
do_all_chans(\"double all samples\", do |val| 2.0 * val end)")
  def do_all_chans(origin, &func)
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        map_channel(func, 0, false, snd, chn, false, origin)
      end
    end
  end

  add_help(:update_graphs, "update_graphs() updates (redraws) all graphs")
  def update_graphs
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        update_time_graph(snd, chn)
      end
    end
  end

  add_help(:do_chans,
           "do_chans(edhist) do |y| ... end  \
applies func to all sync'd channels using edhist as the edit history indication")
  def do_chans(*origin, &func)
    snc = sync
    if snc > 0
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          if sync(snd) == snc
            map_channel(func, 0, false, snd, chn, false, (origin.empty? ? "" : format(*origin)))
          end
        end
      end
    else
      snd_warning("sync not set")
    end
  end

  add_help(:do_sound_chans,
           "do_sound_chans(edhist) do |y| ... end  \
applies func to all selected channels using edhist as the edit history indication")
  def do_sound_chans(*origin, &func)
    if snd = selected_sound
      channels(snd).times do |chn|
        map_channel(func, 0, false, snd, chn, false, (origin.empty? ? "" : format(*origin)))
      end
    else
      snd_warning("no selected sound")
    end
  end

  add_help(:every_sample?,
           "every_sample? do |y| ... end  \
-> true if func is not false for all samples in the current channel, \
otherwise it moves the cursor to the first offending sample")
  def every_sample?(&func)
    snd = Snd.snd
    chn = Snd.chn
    if baddy = scan_channel(lambda do |y| (not func.call(y)) end, 0, frames(snd, chn), snd, chn)
      set_cursor(baddy[1])
    end
    (not baddy)
  end

  add_help(:sort_samples, "sort_samples(bins) provides a histogram in 'bins' bins")
  def sort_samples(nbins)
    bins = make_array(nbins, 0)
    scan_channel(lambda do |y|
                   bin = (y.abs * nbins).floor
                   bins[bin] += 1
                   false
                 end)
    bins
  end

  # mix mono sound into stereo sound panning according to env

  add_help(:place_sound,
           "place_sound(mono_snd, stereo_snd, pan_env) \
mixes a mono sound into a stereo sound, splitting it into two copies \
whose amplitudes depend on the envelope 'pan-env'.  \
If 'pan-env' is a number, the sound is split such that 0 is all in channel 0 \
and 90 is all in channel 1.")
  def place_sound(mono_snd, stereo_snd, pan_env)
    len = frames(mono_snd)
    if number?(pan_env)
      pos = pan_env / 90.0
      rd0 = make_sample_reader(0, mono_snd, false)
      rd1 = make_sample_reader(0, mono_snd, false)
      map_channel(lambda do |y| y + pos * read_sample(rd1) end, 0, len, stereo_snd, 1)
      map_channel(lambda do |y| y + (1.0 - pos) * read_sample(rd0) end, 0, len, stereo_snd, 0)
    else
      e0 = make_env(:envelope, pan_env, :end, len - 1)
      e1 = make_env(:envelope, pan_env, :end, len - 1)
      rd0 = make_sample_reader(0, mono_snd, false)
      rd1 = make_sample_reader(0, mono_snd, false)
      map_channel(lambda do |y| y + env(e1) * read_sample(rd1) end, 0, len, stereo_snd, 1)
      map_channel(lambda do |y| y + (1.0 - env(e0)) * read_sample(rd0) end, 0, len, stereo_snd, 0)
    end
  end

  # FFT-based editing

  add_help(:fft_edit,
           "fft_edit(low_Hz, high_Hz) \
ffts an entire sound, removes all energy below low-Hz and all above high-Hz, then inverse ffts.")
  def fft_edit(bottom, top, snd = false, chn = false)
    sr = srate(snd).to_f
    len = frames(snd, chn)
    fsize = (2.0 ** (log(len) / log(2.0)).ceil).to_i
    rdata = channel2vct(0, fsize, snd, chn)
    idata = make_vct(fsize)
    lo = (bottom / (sr / fsize)).round
    hi = (top / (sr / fsize)).round
    fft(rdata, idata, 1)
    j = fsize - 1
    lo.times do |i|
      rdata[i] = rdata[j] = 0.0
      rdata[i] = rdata[j] = 0.0
      j -= 1
    end
    j = fsize - hi
    (hi..(fsize / 2)).each do |i|
      rdata[i] = rdata[j] = 0.0
      rdata[i] = rdata[j] = 0.0
      j -= 1
    end
    fft(rdata, idata, -1)
    vct_scale!(rdata, 1.0 / fsize)
    vct2channel(rdata, 0, len - 1, snd, chn, false, format("%s(%s, %s", get_func_name, bottom, top))
  end

  add_help(:fft_squelch,
           "fft_squelch(squelch, [snd=false, [chn=false]]) \
ffts an entire sound, sets all bins to 0.0 whose energy is below squelch, then inverse ffts")
  def fft_squelch(squelch, snd = false, chn = false)
    sr = srate(snd).to_f
    len = frames(snd, chn)
    fsize = (2.0 ** (log(len) / log(2.0)).ceil).to_i
    rdata = channel2vct(0, fsize, snd, chn)
    idata = make_vct(fsize)
    fsize2 = fsize / 2
    fft(rdata, idata, 1)
    vr = vct_copy(rdata)
    vi = vct_copy(idata)
    rectangular2polar(vr, vi)
    scaler = vct_peak(vr)
    scl_squelch = squelch * scaler
    j = fsize - 1
    fsize2.times do |i|
      if sqrt(rdata[i] * rdata[i] + idata[i] * idata[i]) < scl_squelch
        rdata[i] = rdata[j] = 0.0
        rdata[i] = rdata[j] = 0.0
      end
      j -= 1
    end
    fft(rdata, idata, -1)
    vct_scale!(rdata, 1.0 / fsize)
    vct2channel(rdata, 0, len - 1, snd, chn, false, format("%s(%s", get_func_name, squelch))
    scaler
  end

  add_help(:fft_cancel,
           "fft_cancel(lo_freq, hi_freq, [snd=false, [chn=false]]) \
ffts an entire sound, sets the bin(s) representing lo_freq to hi_freq to 0.0, then inverse ffts")
  def fft_cancel(lo_freq, hi_freq, snd = false, chn = false)
    sr = srate(snd).to_f
    len = frames(snd, chn)
    fsize = (2.0 ** (log(len) / log(2.0)).ceil).to_i
    rdata = channel2vct(0, fsize, snd, chn)
    idata = make_vct(fsize)
    fsize2 = fsize / 2
    fft(rdata, idata, 1)
    hz_bin = sr / fsize
    lo_bin = (lo_freq / hz_bin).round
    hi_bin = (hi_freq / hz_bin).round
    j = fsize - lo_bin - 1
    fsize2.times do |i|
      if i > hi_bin
        rdata[i] = rdata[j] = 0.0
        rdata[i] = rdata[j] = 0.0
      end
      j -= 1
    end
    fft(rdata, idata, -1)
    vct_scale!(rdata, 1.0 / fsize)
    vct2channel(rdata, 0, len - 1, snd, chn, false,
                format("%s(%s, %s", get_func_name, lo_freq, hi_freq))
  end

  # same idea but used to distinguish vowels (steady-state) from consonants

  add_help(:ramp,
           "ramp(gen, up) \
is a kind of CLM generator that produces a ramp of a given length, \
then sticks at 0.0 or 1.0 until the 'up' argument changes")
  def ramp(gen, up)
    ctr, size = gen[0, 2]
    val = ctr / size
    gen[0] = [size, [0, ctr + (up ? 1 : -1)].max].min
    val
  end

  add_help(:make_ramp, "make_ramp([size=128]) returns a ramp generator")
  def make_ramp(size = 128)
    [0, size]
  end

  add_help(:squelch_vowels,
           "squelch_vowels([snd=false, [chn=false]]) \
suppresses portions of a sound that look like steady-state")
  def squelch_vowels(snd = false, chn = false)
    fft_size = 32
    fft_mid = fft_size / 2
    rl = make_vct(fft_size)
    im = make_vct(fft_size)
    ramper = make_ramp(256)
    peak = maxamp / fft_mid
    read_ahead = make_sample_reader(0, snd, chn)
    ctr = fft_size - 1
    ctr.times do |i| rl[i] = read_sample(read_ahead) end
    in_vowel = false
    map_channel(lambda do |y|
                  rl[ctr] = read_sample(read_ahead)
                  ctr += 1
                  if ctr == fft_size
                    fft(rl, im, 1)
                    vct_multiply!(rl, rl)
                    vct_multiply!(im, im)
                    vct_add!(rl, im)
                    in_vowel = (rl[0] + rl[1] + rl[2] + rl[3]) > peak
                    vct_fill!(im, 0.0)
                    ctr = 0
                  end
                  y * (1.0 - ramp(ramper, in_vowel))
                end, 0, false, snd, chn, false, "squelch_vowels(")
  end

  add_help(:fft_env_data,
           "fft_env_data(fft-env, [snd=false, [chn=false]]) \
applies fft_env as spectral env to current sound, returning vct of new data")
  def fft_env_data(fft_env, snd = false, chn = false)
    sr = srate(snd)
    len = frames(snd, chn)
    fsize = (2 ** (log(len) / log(2.0)).ceil).to_i
    rdata = channel2vct(0, fsize, snd, chn)
    idata = make_vct(fsize)
    fsize2 = fsize / 2
    en = make_env(:envelope, fft_env, :end, fsize2 - 1)
    fft(rdata, idata, 1)
    j = fsize - 1
    fsize2.times do |i|
      val = env(en)
      rdata[i] *= val
      idata[i] *= val
      rdata[j] *= val
      idata[j] *= val
      j -= 1
    end
    fft(rdata, idata, -1)
    vct_scale!(rdata, 1.0 / fsize)
  end

  add_help(:fft_env_edit,
           "fft_env_edit(fft-env, [snd=false, [chn=false]]) \
edits (filters) current chan using fft_env")
  def fft_env_edit(fft_env, snd = false, chn = false)
    vct2channel(fft_env_data(fft_env, snd, chn), 0, frames(snd, chn) - 1, snd, chn, false,
                format("%s(%s", get_func_name, fft_env.inspect))
  end

  add_help(:fft_env_interp,
           "fft_env_interp(env1, env2, interp, [snd=false, [chn=false]]) \
interpolates between two fft-filtered versions (env1 and env2 are the spectral envelopes) \
following interp (an env between 0 and 1)")
  def fft_env_interp(env1, env2, interp, snd = false, chn = false)
    data1 = fft_env_data(env1, snd, chn)
    data2 = fft_env_data(env2, snd, chn)
    len = frames(snd, chn)
    en = make_env(:envelope, interp, :end, len - 1)
    new_data = make_vct!(len) do |i|
      pan = env(en)
      (1.0 - pan) * data1[i] + pan * data2[i]
    end
    vct2channel(new_data, 0, len - 1, snd, chn, false,
                format("%s(%s, %s, %s", get_func_name, env1.inspect, env2.inspect, interp.inspect))
  end

  add_help(:fft_smoother,
           "fft_smoother(cutoff, start, samps, [snd=false, [chn=false]]) \
uses fft-filtering to smooth a section:
vct2channel(fft_smoother(0.1, cursor, 400, 0, 0), cursor, 400)")
  def fft_smoother(cutoff, start, samps, snd = false, chn = false)
    fftpts = (2 ** (log(samps + 1) / log(2.0)).ceil).to_i
    rl = channel2vct(start, samps, snd, chn)
    im = make_vct(fftpts)
    top = (fftpts * cutoff.to_f).floor
    old0 = rl[0]
    old1 = rl[samps - 1]
    oldmax = vct_peak(rl)
    fft(rl, im, 1)
    (top...fftpts).each do |i| rl[i] = im[i] = 0.0 end
    fft(rl, im, -1)
    vct_scale!(rl, 1.0 / fftpts)
    newmax = vct_peak(rl)
    if newmax.zero?
      rl
    else
      if (scl = oldmax / newmax) > 1.5
        vct_scale!(rl, scl)
      end
      new0 = rl[0]
      new1 = rl[samps - 1]
      offset0 = old0 - new0
      offset1 = old1 - new1
      incr = offset0 == offset1 ? 0.0 : ((offset1 - offset0) / samps)
      trend = offset0 - incr
      rl.map do |val|
        trend += incr
        val += trend
      end
    end
  end

  # comb-filter 

  add_help(:comb_filter,
           "comb_filter(scaler, size) \
returns a comb-filter ready for map_channel etc: map_channel(comb_filter(0.8, 32)).  \
If you're in a hurry use: clm_channel(make_comb(0.8, 32)) instead")
  def comb_filter(scaler, size)
    cmb = make_comb(scaler, size)
    lambda do |inval| comb(cmb, inval) end
  end

  # by using filters at harmonically related sizes, we can get chords:

  add_help(:comb_chord,
           "comb_chord(scaler, size, amp, [interval_one=0.75, [interval_two=1.2]]) \
returns a set of harmonically-related comb filters: map_channel(comb_chord(0.95, 100, 0.3))")
  def comb_chord(scaler, size, amp, interval_one = 0.75, interval_two = 1.2)
    c1 = make_comb(scaler, size.to_i)
    c2 = make_comb(scaler, (interval_one * size).to_i)
    c3 = make_comb(scaler, (interval_two * size).to_i)
    lambda do |inval| amp * (comb(c1, inval) + comb(c2, inval) + comb(c3, inval)) end
  end

  # or change the comb length via an envelope:

  add_help(:zcomb,
           "zcomb(scaler, size, pm) \
returns a comb filter whose length varies according to an envelope: \
map_channel(zcomb(0.8, 32, [0, 0, 1, 10]))")
  def zcomb(scaler, size, pm)
    max_envelope_1 = lambda do |en, mx|
      1.step(en.length - 1, 2) do |i| mx = [mx, en[i]].max.to_f end
      mx
    end
    cmb = make_comb(scaler, size, :max_size, (max_envelope_1.call(pm, 0.0) + size + 1).to_i)
    penv = make_env(:envelope, pm, :end, frames)
    lambda do |inval| comb(cmb, inval, env(penv)) end
  end

  add_help(:notch_filter,
           "notch_filter(scaler, size) returns a notch-filter: map_channel(notch_filter(0.8, 32))")
  def notch_filter(scaler, size)
    gen = make_notch(scaler, size)
    lambda do |inval| notch(gen, inval) end
  end

  add_help(:formant_filter,
                   "formant_filter(radius, frequency) \
returns a formant generator: map_channel(formant_filter(0.99, 2400)). \
Faster is: filter_sound(make_formant(0.99, 2400))")
  def formant_filter(radius, freq)
    frm = make_formant(radius, freq)
    lambda do |inval| formant(frm, inval) end
  end

  # to impose several formants, just add them in parallel:

  add_help(:formants,
           "formants(r1, f1, r2, f2, r3, f3) \
returns 3 formant filters in parallel: map_channel(formants(0.99, 900, 0.98, 1800, 0.99 2700))")
  def formants(r1, f1, r2, f2, r3, f3)
    fr1 = make_formant(r1, f1)
    fr2 = make_formant(r2, f2)
    fr3 = make_formant(r3, f3)
    lambda do |inval| formant(fr1, inval) + formant(fr1, inval) + formant(fr1, inval) end
  end

  add_help(:moving_formant,
           "moving_formant(radius, move) \
returns a time-varying (in frequency) formant filter: \
map_channel(moving_formant(0.99, [0, 1200, 1, 2400]))")
  def moving_formant(radius, move)
    frm = make_formant(radius, move[1])
    menv = make_env(:envelope, move, :end, frames)
    lambda do |inval|
      val = formant(frm, inval)
      frm.frequency = env(menv)
      val
    end
  end

  add_help(:osc_formants,
           "osc_formants(radius, bases, amounts, freqs) \
set up any number of independently oscillating formants, then calls map_channel: \
osc_formants(0.99, vct(400, 800, 1200), vct(400, 800, 1200), vct(4, 2, 3))")
  def osc_formants(radius, bases, amounts, freqs)
    len = bases.length
    frms = make_array(len) do |i| make_formant(radius, bases[i]) end
    oscs = make_array(len) do |i| make_oscil(freqs[i]) end
    map_channel_rb() do |x|
      val = 0.0
      frms.each_with_index do |frm, i|
        val += formant(frm, x)
        set_mus_frequency(frm, bases[i] + amounts[i] * oscil(oscs[i]))
      end
      val
    end
  end

  # echo

  add_help(:echo, "echo(scaler, secs) returns an echo maker: map_channel(echo(0.5, 0.5), 0 44100)")
  def echo(scaler, secs)
    del = make_delay((secs * srate()).round)
    lambda do |inval|
      inval + delay(del, scaler * (tap(del) + inval))
    end
  end

  add_help(:zecho,
           "zecho(scaler, secs, freq, amp) \
returns a modulated echo maker: map_channel(zecho(0.5, 0.75, 6, 10.0), 0, 65000)")
  def zecho(scaler, secs, freq, amp)
    os = make_oscil(freq)
    len = (secs * srate()).round
    del = make_delay(len, :max_size, (len + amp + 1).to_i)
    lambda do |inval|
      inval + delay(del, scaler * (tap(del) + inval), amp * oscil(os))
    end
  end

  add_help(:flecho,
           "flecho(scaler, secs]) \
returns a low-pass filtered echo maker: map_channel(flecho(0.5, 0.9), 0, 75000)" )
  def flecho(scaler, secs)
    flt = make_fir_filter(:order, 4, :xcoeffs, vct(0.125, 0.25, 0.25, 0.125))
    del = make_delay((secs * srate()).round)
    lambda do |inval|
      inval + delay(del, fir_filter(flt, scaler * (tap(del) + inval)))
    end
  end

  # ring-mod and am
  #
  # CLM instrument is ring-modulate.ins

  add_help(:ring_mod,
           "ring_mod(freq, gliss_env) \
returns a time-varying ring-modulation filter: \
map_channel(ring_mod(10, [0, 0, 1, hz2radians(100)]))")
  def ring_mod(freq, gliss_env)
    os = make_oscil(:frequency, freq)
    len = frames()
    sr = srate()
    dur = (len / sr).round
    genv = make_env(:envelope, gliss_env, :end, len)
    lambda do |inval| oscil(os, env(genv)) * inval end
  end

  add_help(:am, "am(freq) returns an amplitude-modulator: map_channel(am(440))")
  def am(freq)
    os = make_oscil(freq)
    lambda do |inval| amplitude_modulate(1.0, inval, oscil(os)) end
  end

  def vibro(speed, depth)
    sine = make_oscil(speed)
    scl = 0.5 * depth
    offset = 1.0 - scl
    lambda do |inval| inval * (offset + scl * oscil(sine)) end
  end

  # hello-dentist
  #
  # CLM instrument version is in clm.html

  add_help(:hello_dentist,
           "hello_dentist(frq, amp, [snd=false, [chn=false]]) \
varies the sampling rate randomly, making a voice sound quavery: hello_dentist(40.0, 0.1)")
  def hello_dentist(freq, amp, snd = false, chn = false)
    rn = make_rand_interp(:frequency, freq, :amplitude, amp)
    i = 0
    len = frames()
    in_data = channel2vct(0, len, snd, chn)
    out_len = (len * (1.0 + 2.0 * amp)).to_i
    out_data = make_vct(out_len)
    rd = make_src(:srate, 1.0,
                  :input, lambda do |dir|
                    val = i.between?(0, len - 1) ? in_data[i] : 0.0
                    i += dir
                    val
                  end)
    vct2channel(vct_map!(out_data, lambda do | | src(rd, rand_interp(rn)) end),
                0, len, snd, chn, false, format("%s(%s, %s", get_func_name, freq, amp))
  end

  # a very similar function uses oscil instead of rand-interp, giving
  # various "Forbidden Planet" sound effects:

  add_help(:fp,
           "fp(sr, osamp, osfrq, [snd=false, [chn=false]]) \
varies the sampling rate via an oscil: fp(1.0, 0.3, 20)")
  def fp(sr, osamp, osfreq, snd = false, chn = false)
    os = make_oscil(:frequency, osfreq)
    s = make_src(:srate, sr)
    len = frames(snd, chn)
    sf = make_sample_reader(0, snd, chn)
    out_data = make_vct!(len) do
      src(s, osamp * oscil(os), lambda do |dir| dir > 0 ? next_sample(sf) : previous_sample(sf) end)
    end
    free_sample_reader(sf)
    vct2channel(out_data, 0, len, snd, chn, false,
                format("%s(%s, %s, %s", get_func_name, sr, osamp, osfreq))
  end

  # compand, compand-channel

  add_help(:compand, "compand() returns a compander: map_channel(compand())")
  def compand(doc = false)
    tbl = vct(-1.000, -0.960, -0.900, -0.820, -0.720, -0.600, -0.450, -0.250, 
              0.000, 0.250, 0.450, 0.600, 0.720, 0.820, 0.900, 0.960, 1.000)
    lambda do |inval| array_interp(tbl, 8.0 + 8.0 * inval, tbl.length) end
  end

  add_help(:compand_channel,
           "compand_channel([beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]) \
applies a standard compander to sound")
  def compand_channel(beg = 0, dur = false, snd = false, chn = false, edpos = false)
    tbl = vct(-1.000, -0.960, -0.900, -0.820, -0.720, -0.600, -0.450, -0.250, 
              0.000, 0.250, 0.450, 0.600, 0.720, 0.820, 0.900, 0.960, 1.000)
    ptree_channel(lambda { |inval| array_interp(tbl, 8.0 + 8.0 * inval, tbl.length)},
                  beg, dur, snd, chn, edpos, true, false,
                  format("%s(%s, %s", get_func_name, beg, dur))
  end

  # shift pitch keeping duration constant
  # 
  # both src and granulate take a function argument to get input
  # whenever it is needed.  in this case, src calls granulate which
  # reads the currently selected file.  CLM version is in expsrc.ins

  add_help(:expsrc,
           "expsrc(rate, [snd=false, [chn=false]]) \
uses sampling-rate conversion and granular synthesis to produce a sound \
at a new pitch but at the original tempo.  It returns a function for map_chan.")
  def expsrc(rate, snd = false, chn = false)
    gr = make_granulate(:expansion, rate)
    sr = make_src(:srate, rate)
    vsize = 1024
    vbeg = 0
    v = channel2vct(0, vsize)
    inctr = 0
    lambda do |inval|
      src(sr, 0.0, lambda do |dir|
            granulate(gr, lambda do |dr|
                        val = v[inctr]
                        inctr += dr
                        if inctr >= vsize
                          vbeg += inctr
                          inctr = 0
                          v = channel2vct(vbeg, vsize, snd, chn)
                        end
                        val
                      end)
          end)
    end
  end

  # the next (expsnd) changes the tempo according to an envelope; the
  # new duration will depend on the expansion envelope -- we integrate
  # it to get the overall expansion, then use that to decide the new
  # length.

  add_help(:expsnd,
           "expsnd(gr_env, [snd=false, [chn=false]]) \
uses the granulate generator to change tempo according to an envelope: expsnd([0, 0.5, 2, 2.0])")
  def expsnd(gr_env, snd = false, chn = false)
    dur = ((frames(snd, chn) / srate(snd)) * integrate_envelope(gr_env) / envelope_last_x(gr_env))
    gr = make_granulate(:expansion, gr_env[1], :jitter, 0)
    ge = make_env(:envelope, gr_env, :duration, dur)
    sound_len = (srate(snd) * dur).to_i
    len = [sound_len, frames(snd, chn)].max
    sf = make_sample_reader(0, snd, chn)
    out_data = make_vct!(len) do
      val = granulate(gr, lambda do |dir| next_sample(sf) end)
      gr.increment = env(ge)
      val
    end
    free_sample_reader(sf)
    vct2channel(out_data, 0, len, snd, chn, false, format("%s(%s", get_func_name, gr_env.inspect))
  end

  # cross-synthesis
  #
  # CLM version is in clm.html

  add_help(:cross_synthesis,
           "cross_synthesis(cross_snd, amp, fftsize, r) \
does cross-synthesis between 'cross-snd' (a sound index) and the currently selected sound: \
map_channel(cross_synthesis(1, 0.5, 128, 6.0))")
  def cross_synthesis(cross_snd, amp, fftsize, r)
    freq_inc = fftsize / 2
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    spectr = make_vct(freq_inc)
    inctr = 0
    ctr = freq_inc
    radius = 1.0 - r / fftsize.to_f
    bin = srate() / fftsize.to_f
    formants = make_array(freq_inc) do |i| make_formant(radius, i * bin) end
    lambda do |inval|
      if ctr == freq_inc
        fdr = channel2vct(inctr, fftsize, cross_snd, 0)
        inctr += freq_inc
        spectrum(fdr, fdi, false, 2)
        vct_subtract!(fdr, spectr)
        vct_scale!(fdr, 1.0 / freq_inc)
        ctr = 0
      end
      ctr += 1
      vct_add!(spectr, fdr)
      amp * formant_bank(spectr, formants, inval)
    end
  end

  # similar ideas can be used for spectral cross-fades, etc -- for example:

  add_help(:voiced2unvoiced,
           "voiced2unvoiced(amp, fftsize, r, tempo, [snd=false, [chn=false]]) \
turns a vocal sound into whispering: voiced2unvoiced(1.0, 256, 2.0, 2.0)")
  def voiced2unvoiced(amp, fftsize, r, tempo, snd = false, chn = false)
    freq_inc = fftsize / 2
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    spectr = make_vct(freq_inc)
    noi = make_rand(:frequency, srate(snd) / 3.0)
    inctr = 0
    ctr = freq_inc
    radius = 1.0 - r.to_f / fftsize
    bin = srate(snd).to_f / fftsize
    len = frames(snd, chn)
    outlen = (len / tempo).floor
    hop = (freq_inc * tempo).floor
    out_data = make_vct([len, outlen].max)
    formants = make_array(freq_inc) do |i| make_formant(radius, i * bin) end
    old_peak_amp = new_peak_amp = 0.0
    callcc do |brk|
      outlen.times do |i|
        if ctr == freq_inc
          if c_g?
            brk.call("interrupted")
          end
          fdr = channel2vct(inctr, fftsize, snd, chn)
          if (pk = vct_peak(fdr)) > old_peak_amp
            old_peak_amp = pk
          end
          spectrum(fdr, fdi, false, 2)
          inctr += hop
          vct_subtract!(fdr, spectr)
          vct_scale!(fdr, 1.0 / freq_inc)
          ctr = 0
        end
        ctr += 1
        vct_add!(spectr, fdr)
        if (outval = formant_bank(spectr, formants, rand(noi))).abs > new_peak_amp
          new_peak_amp = outval.abs
        end
        out_data[i] = outval
      end
      vct_scale!(out_data, amp * (old_peak_amp / new_peak_amp))
      vct2channel(out_data, 0, [len, outlen].max, snd, chn, false,
                  format("%s(%s, %s, %s, %s", get_func_name, amp, fftsize, r, tempo))
    end
  end

  # very similar but use sum-of-cosines (glottal pulse train?) instead of white noise

  add_help(:pulse_voice,
           "pulse_voice(cosin, freq=440.0, amp=1.0, fftsize=256, r=2.0, snd=false, chn=false) \
uses sum-of-cosines to manipulate speech sounds")
  def pulse_voice(cosin, freq = 440.0, amp = 1.0, fftsize = 256, r = 2.0, snd = false, chn = false)
    freq_inc = fftsize / 2
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    spectr = make_vct(freq_inc)
    pulse = make_sum_of_cosines(cosin, freq)
    inctr = 0
    ctr = freq_inc
    radius = 1.0 - r / fftsize
    bin = srate(snd) / fftsize
    len = frames(snd, chn)
    out_data = make_vct(len)
    old_peak_amp = new_peak_amp = 0.0
    formants = make_array(freq_inc) do |i| make_formant(radius, i * bin) end
    callcc do |brk|
      out_data.map do |i|
        outval = 0.0
        if ctr == freq_inc
          if c_g? then brk.call("interrupted") end
          fdr = channel2vct(inctr, fftsize, snd, chn)
          pk = vct_peak(fdr)
          if pk > old_peak_amp then old_peak_amp = pk end
          spectrum(fdr, fdi, false, 2)
          inctr += freq_inc
          vct_subtract!(fdr, spectr)
          vct_scale!(fdr, 1.0 / freq_inc)
          ctr = 0
        end
        ctr += 1
        vct_add!(spectr, fdr)
        outval = formant_bank(spectr, formants, sum_of_cosines(pulse))
        if outval.abs > new_peak_amp then new_peak_amp = outval.abs end
        outval
      end
      vct_scale!(out_data, amp * (old_peak_amp / new_peak_amp))
      vct2channel(out_data, 0, len, snd, chn)
    end
  end
  # pulse_voice(80,   20.0, 1.0, 1024, 0.01)
  # pulse_voice(80,  120.0, 1.0, 1024, 0.2)
  # pulse_voice(30,  240.0, 1.0, 1024, 0.1)
  # pulse_voice(30,  240.0, 1.0, 2048)
  # pulse_voice( 6, 1000.0, 1.0,  512)

  # convolution example

  add_help(:cnvtest,
           "cnvtest(snd0, snd1, amp) \
convolves snd0 and snd1, scaling by amp, returns new max amp: cnvtest(0, 1, 0.1)")
  def cnvtest(snd0, snd1, amp)
    flt_len = frames(snd0)
    total_len = flt_len + frames(snd1)
    cnv = make_convolve(:filter, channel2vct(0, flt_len, snd0))
    sf = make_sample_reader(0, snd1, false)
    out_data = make_vct!(total_len) do convolve(cnv, lambda do |dir| next_sample(sf) end) end
    free_sample_reader(sf)
    vct_scale!(out_data, amp)
    max_samp = vct_peak(out_data)
    vct2channel(out_data, 0, total_len, snd1)
    if max_samp > 1.0
      set_y_bounds(snd1, [-max_samp, max_samp])
    end
    max_samp
  end

  # swap selection chans

  add_help(:swap_selection_channels,
           "swap_selection_channels) swaps the currently selected data's channels")
  def swap_selection_channels
    find_selection_sound = lambda do |not_this|
      callcc do |ret|
        Snd.sounds.each do |snd|
          channels(snd).times do |chn|
            if selection_member?(snd, chn) and
                (not_this.empty? or snd != not_this[0] or chn != not_this[1])
              ret.call([snd, chn])
            end
          end
        end
      end
    end
    if selection?
      if selection_chans == 2
        beg = selection_position
        len = selection_frames
        snd_chn0 = find_selection_sound.call([])
        snd_chn1 = find_selection_sound.call(snd_chn0)
        if snd_chn1
          swap_channels(snd_chn0[0], snd_chn0[1], snd_chn1[0], snd_chn1[1], beg, len)
        else
          Snd.raise(:wrong_number_of_channels, "need two channels to swap")
        end
      else
        Snd.raise(:wrong_number_of_channels, "need a stereo selection")
      end
    else
      Snd.raise(:no_active_selection)
    end
  end

  # sound interp
  # 
  # make-sound-interp sets up a sound reader that reads a channel at
  # an arbitary location, interpolating between samples if necessary,
  # the corresponding "generator" is sound-interp

  add_help(:make_sound_interp,
           "make_sound_interp(start, [snd=false, [chn=false]]) \
an interpolating reader for snd's channel chn")
  def make_sound_interp(start, snd = false, chn = false)
    bufsize = 2048
    buf4size = 128
    data = channel2vct(start, bufsize, snd, chn)
    curbeg = start
    curend = start + bufsize
    lambda do |loc|
      if loc < curbeg
        curbeg = [buf4size + (loc - bufsize), 0].max
        curend = curbeg + bufsize
        data = channel2vct(curbeg, bufsize, snd, chn)
      else
        if loc > curend
          curbeg = [loc - buf4size, 0].max
          curend = curbeg + bufsize
          data = channel2vct(curbeg, bufsize, snd, chn)
        end
      end
      array_interp(data, loc - curbeg, bufsize)
    end
  end

  add_help(:sound_interp,
           "sound_interp(func, loc) \
sample at loc (interpolated if necessary) from func created by make_sound_interp")
  def sound_interp(func, loc)
    func.call(loc)
  end

  def sound_via_sound(snd1, snd2)
    intrp = make_sound_interp(0, snd1, 0)
    len = frames(snd1, 0) - 1
    rd = make_sample_reader(0, snd2, 0)
    mx = maxamp(snd2, 0)
    map_channel(lambda do |val|
                  sound_interp(intrp, (len * 0.5 * (1.0 + (read_sample(rd) / mx))).floor)
                end)
  end
  
  # env_sound_interp takes an envelope that goes between 0 and 1
  # (y-axis), and a time-scaler (1.0 = original length) and returns a
  # new version of the data in the specified channel that follows that
  # envelope (that is, when the envelope is 0 we get sample 0, when
  # the envelope is 1 we get the last sample, envelope = 0.5 we get
  # the middle sample of the sound and so on. env_sound_interp([0, 0,
  # 1, 1]) will return a copy of the current sound;
  # env_sound_interp([0, 0, 1, 1, 2, 0], 2.0) will return a new sound
  # with the sound copied first in normal order, then reversed.
  # src_sound with an envelope could be used for this effect, but it
  # is much more direct to apply the envelope to sound sample
  # positions.

  add_help(:env_sound_interp,
           "env_sound_interp(env, [time_scale=1.0, [snd=false, [chn=false]]]) \
reads snd's channel chn according to env and time-scale")
  def env_sound_interp(envelope, time_scale = 1.0, snd = false, chn = false)
    len = frames(snd, chn)
    newlen = (time_scale.to_f * len).floor
    rd = make_sound_interp(0, snd, chn)
    read_env = make_env(:envelope, envelope, :end, newlen, :scaler, len)
    tempfilename = snd_tempnam
    fil = mus_sound_open_output(tempfilename, srate(snd), 1, false, Mus_next,
                                format("%s temp file", get_func_name))
    bufsize = 8192
    data = make_sound_data(1, bufsize)
    data_ctr = 0
    newlen.times do |i|
      data[0, data_ctr] = sound_interp(rd, env(read_env))
      data_ctr += 1
      if bufsize == data_ctr
        mus_sound_write(fil, 0, bufsize - 1, 1, data)
        data_ctr = 0
      end
    end
    if data_ctr > 0
      mus_sound_write(fil, 0, data_ctr - 1, 1, data)
    end
    mus_sound_close_output(fil, 4 * newlen)
    set_samples(0, newlen, tempfilename, snd, chn, false,
                format("%s(%s, %s", get_func_name, envelope.inspect, time_scale))
    File.unlink(tempfilename)
  end

  def granulated_sound_interp(envelope,
                              time_scale = 1.0,
                              grain_length = 0.1,
                              grain_envelope = [0, 0, 1, 1, 2, 1, 3, 0],
                              output_hop = 0.05,
                              snd = false,
                              chn = false)
    len = frames(snd, chn)
    newlen = (time_scale * len).floor
    read_env = make_env(envelope, :end, newlen, :scaler, len)
    tempfilename = snd_tempnam
    fil = mus_sound_open_output(tempfilename, srate(snd), 1, false, Mus_next,
                                "granulated_sound_interp temp file")
    grain_frames = (grain_length * srate).round
    hop_frames = (output_hop * srate).round
    num_readers = (grain_length / output_hop).round + 1
    readers = make_array(num_readers, false)
    grain_envs = make_array(num_readers) do |i| make_env(grain_envelope, :end, grain_frames) end
    next_reader_starts_at = 0
    next_reader = 0
    bufsize = 8192
    data = make_sound_data(1, bufsize)
    data_ctr = 0
    jitter = srate * 0.005
    newlen.times do |i|
      position_in_original = env(read_env)
      if i >= next_reader_starts_at
        readers[next_reader] =
          make_sample_reader([0, (position_in_original + mus_random(jitter)).round].max, false)
        grain_envs[next_reader].reset
        next_reader += 1
        if next_reader >= num_readers then next_reader = 0 end
        next_reader_starts_at += hop_frames
      end
      sum = 0.0
      readers.each_with_index do |rd, j|
        if sample_reader?(rd)
          sum += env(grain_envs[j]) * next_sample(rd)
        end
      end
      sound_data_set!(data, 0, data_ctr, sum)
      data_ctr += 1
      if bufsize == data_ctr
        mus_sound_write(fil, 0, bufsize - 1, 1, data)
        data_ctr = 0
      end
    end
    if data_ctr > 0
      mus_sound_write(fil, 0, data_ctr - 1, 1, data)
    end
    mus_sound_close_output(fil, 4 * newlen)
    set_samples(0, newlen, tempfilename, snd, chn, true,
                format("%s(%s, %s, %s, %s, %s",
                       get_func_name,
                       envelope.inspect,
                       time_scale,
                       grain_length,
                       grain_envelope.inspect,
                       output_hop))
    File.unlink(tempfilename)
  end
  # granulated_sound_interp([0, 0, 1, 0.1, 2, 1], 1.0, 0.2, [0, 0, 1, 1, 2, 0])
  # granulated_sound_interp([0, 0, 1, 1], 2.0)
  # granulated_sound_interp([0, 0, 1, 0.1, 2, 1], 1.0, 0.2, [0, 0, 1, 1, 2, 0], 0.02)
  
  # add date and time to title bar
  # 
  # The window manager's property that holds the Snd window's title is
  # WM_NAME, we can use the window_property function (used normally
  # for CLM/Snd communication) to reset this value.  The Snd window's
  # identifier is SND_VERSION.  Here we're also using the true
  # argument to short_file_name to get a list of all current sounds.

  $retitle_time = 60 * 1000                       # once a minute
  add_help(:title_with_date,
           "title_with_date() \
causes Snd's main window to display the time of day.  \
To turn off this clock, set $retitle_time to 0")
  def title_with_date
    names = short_file_name(true)
    set_window_property("SND_VERSION", "WM_NAME",
                        format("snd (%s)%s", Time.new.localtime.strftime("%d-%b %H:%M %Z"),
                               names ? format(": %s", names.inspect) : ""))
    if $retitle_time > 0
      call_in($retitle_time, title_with_date)
    end
  end
  
  # filtered-env 

  add_help(:filtered_env,
           "filtered_env(env, [snd=false, [chn=false]]) \
is a time-varying one-pole filter: when env is at 1.0, no filtering, \
as env moves to 0.0, low-pass gets more intense; amplitude and low-pass amount move together")
  def filtered_env(en, snd = false, chn = false)
    flt = make_one_pole(1.0, 0.0)
    amp_env = make_env(:envelope, en, :end, frames - 1)
    map_channel(lambda do |val|
                  env_val = env(amp_env)
                  set_mus_xcoeff(flt, 0, env_val)
                  set_mus_ycoeff(flt, 1, env_val - 1.0)
                  one_pole(flt, env_val * val)
                end, 0, false, snd, chn, false, format("%s(%s", get_func_name, en.inspect))
  end

  # lisp graph with draggable x axis

  class Mouse
    def initialize
      @down = 0
      @pos = 0.0
      @x1 = 1.0
    end

    def press(snd, chn, button, state, x, y)
      @pos = x / @x1
      @down = @x1
    end

    def drag(snd, chn, button, state, x, y)
      xnew = x / @x1
      @x1 = [1.0, [0.1, @down + (@pos - xnew)].max].min
      graph(make_vct!((100 * @x1).floor) do |i| i * 0.01 end, "ramp", 0.0, @x1)
    end
  end

=begin  
  let(Mouse.new) do |mouse|
    $mouse_drag_hook.add_hook!("Mouse") do |snd, chn, button, state, x, y|
      mouse.drag(snd, chn, button, state, x, y)
    end
    $mouse_press_hook.add_hook!("Mouse") do |snd, chn, button, state, x, y|
      mouse.press(snd, chn, button, state, x, y)
    end
  end
=end

  # pointer focus within Snd
  # 
  # $mouse_enter_graph_hook.add_hook!("focus") do |snd, chn|
  #   focus_widget(channel_widgets(snd, chn)[0])
  # end
  # $mouse_enter_listener_hook.add_hook!("focus") do |widget| focus_widget(widget) end
  # $mouse_enter_text_hook.add_hook!("focus") do |widget| focus_widget(widget) end

  # View: Files dialog chooses which sound is displayed
  #
  # by Anders Vinjar

  add_help(:files_popup_buffer,
           "files_popup_buffer(type, position, name) \
hides all sounds but the one the mouse touched in the current files list.  \
Use with $mouse_enter_label_hook.
$mouse_enter_label_hook.add_hook!(\"files-popup\") do |type, position, name|
  files_popup_buffer(type, position, name)
end")
  def files_popup_buffer(type, position, name)
    if snd = find_sound(name)
      curr_buffer = Snd.snd
      width, height= widget_size(sound_widgets(curr_buffer)[0])
      Snd.sounds.each do |s| hide_widget(sound_widgets(s)[0]) end
      show_widget(sound_widgets(snd)[0])
      set_widget_size(sound_widgets(snd)[0], [widht, height])
      select_sound(snd)
    end
  end

  # C-x b support along the same lines

  class Snd_buffers
    def initialize
      @last_buffer = false
      @current_buffer = false
      @last_width = 0
      @last_heigth = 0
    end

    def switch_to_buffer
      prompt = if @last_buffer
                 if @last_buffer[1] > 0
                   format("switch to buf: (default \"%s\" chan %d) ",
                          short_file_name(@last_buffer[0]), @last_buffer[1])
                 else
                   format("switch to buf: (default \"%s\") ",
                          short_file_name(@last_buffer[0]))
                 end
               else
                 "switch to buf: (default: make new sound)"
               end
      prompt_in_minibuffer(prompt,
                           lambda do |response|
                             width, heigth = widget_size(sound_widgets(@current_buffer[0])[0])
                             callcc do |give_up|
                               if (not string?(response)) or response.empty?
                                 temp = @current_buffer
                                 if @last_buffer
                                   @current_buffer = @last_buffer
                                 else
                                   index = new_sound()
                                   @current_buffer = [index, 0]
                                 end
                                 @last_buffer = temp
                               else
                                 if index = find_sound(response)
                                   @last_buffer, @current_buffer = @current_buffer, [index, 0]
                                 else
                                   give_up.call(report_in_minibuffer("can't find " + response))
                                 end
                               end
                               close_all_buffers
                               report_in_minibuffer("")
                               open_current_buffer(width, heigth)
                             end
                           end)
    end

    def open(snd)
      close_all_buffers
      @last_buffer = @current_buffer
      @current_buffer = [snd, 0]
      open_current_buffer((@last_width.zero? ? window_width : @last_width),
                          (@last_heigth.zero? ? (window_height - 10) : @last_heigth))
      false
    end
    
    def close(snd)
      if @current_buffer and snd == @current_buffer[0]
        closer = @current_buffer[0]
        close_all_buffers
        @current_buffer = if @last_buffer
                            @last_buffer
                          else
                            sounds ? [sounds[0], 0] : false
                          end
        @last_buffer = Snd.sounds.detect do |s|
          s != closer and ((not @current_buffer) or s != @current_buffer[0])
        end
        if @last_buffer
          @last_buffer = [@last_buffer, 0]
        end
        if @current_buffer
          open_current_buffer(@last_width, @last_heigth)
        end
      end
      false
    end

    private
    def open_current_buffer(width, heigth)
      @last_width = width
      @last_heigth = heigth
      if sound_pane = sound_widgets(@current_buffer[0])[0]
        show_widget(sound_pane)
        set_widget_size(sound_pane, [width, heigth])
        select_sound(@current_buffer[0])
        select_channel(@current_buffer[1])
      end
    end

    def close_all_buffers
      Snd.sounds.each do |s| hide_widget(sound_widgets(s)[0]) end
    end
  end

=begin
  let(Snd_buffers.new) do |bufs|
    # C-x b
    bind_key(?b, 0, lambda do | | bufs.switch_to_buffer end, true, "C-x b: switch to buffer")
    $close_hook.add_hook!("buffers") do |snd| bufs.close(snd) end
    $after_open_hook.add_hook!("buffers") do |snd| bufs.open(snd) end
  end
=end

  # remove-clicks

  add_help(:find_click, "find_click(loc) finds the next click starting at 'loc'")
  def find_click(loc)
    reader = make_sample_reader(loc, false, false)
    samp0 = samp1 = samp2 = 0.0
    samps = make_vct(10)
    len = frames()
    samps_ctr = 0
    callcc do |ret|
      (loc...len).each do |ctr|
        ret.call(ctr) if c_g?
        samp0, samp1, samp2 = samp1, samp2, next_sample(reader)
        samps[samps_ctr] = samp0
        if samps_ctr < 9
          samps_ctr += 1
        else
          samps_ctr = 0
        end
        local_max = [0.1, vct_peak(samps)].max
        if ((samp0 - samp1).abs > local_max) and
            ((samp1 - samp2).abs > local_max) and
            ((samp0 - samp2).abs < (local_max / 2))
          ret.call(ctr - 1)
        end
      end
      false
    end
  end

  add_help(:remove_clicks, "remove_clicks() tries to find and smooth-over clicks")
  def remove_clicks
    loc = 0
    while (click = find_click(loc)) and (not c_g?())
      smooth_sound(click - 2, 4)
      loc = click + 2
    end
  end

  # searching examples (zero+, next-peak)

  add_help(:search_for_click, "search_for_click() looks for the next click (for use with C-s)")
  def search_for_click
    samp0 = samp1 = samp2 = 0.0
    samps = Vct.new(10)
    sctr = 0
    lambda do |val|
      samp0, samp1, samp2 = samp1, samp2, val
      samps[sctr] = val
      sctr += 1
      if sctr >= 10 then sctr = 0 end
      local_max = [0.1, samps.peak].max
      if ((samp0 - samp1).abs >= local_max) and
          ((samp1 - samp2).abs >= local_max) and
          ((samp0 - samp2).abs <= (local_max / 2))
        -1
      else
        false
      end
    end
  end

  add_help(:zero_plus,
           "zero_plus() \
finds the next positive-going zero crossing (if searching forward) (for use with C-s)")
  def zero_plus
    lastn = 0.0
    lambda do |n|
      rtn = lastn < 0.0 and n >= 0.0 and -1
      lastn = n
      rtn
    end
  end

  add_help(:next_peak,
           "next_peak() \
finds the next max or min point in the time-domain waveform (for use with C-s)")
  def next_peak
    last0 = last1 = false
    lambda do |n|
      rtn = number?(last0) and
        ((last0 < last1 and last1 > n) or (last0 > last1 and last1 < n)) and -1
      last0, last1 = last1, n
      rtn
    end
  end

  add_help(:find_pitch,
           "find_pitch(pitch) \
finds the point in the current sound where 'pitch' (in Hz) predominates -- C-s find_pitch(300) \
In most cases, this will be slightly offset from the true beginning of the note")
  def find_pitch(pitch)
    interpolated_peak_offset = lambda do |la, ca, ra|
      pk = 0.001 + [la, ca, ra].max
      logla = log([la, 0.0000001].max / pk) / log(10)
      logca = log([ca, 0.0000001].max / pk) / log(10)
      logra = log([ra, 0.0000001].max / pk) / log(10)
      0.5 * (logla - logra) / ((logla + logra) - 2 * logca)
    end
    data = make_vct(transform_size)
    data_loc = 0
    lambda do |n|
      data[data_loc] = n
      data_loc += 1
      rtn = false
      if data_loc == transform_size
        data_loc = 0
        if vct_peak(data) > 0.001
          spectr = snd_spectrum(data, Rectangular_window, transform_size)
          pk = 0.0
          pkloc = 0
          (transform_size / 2).times do |i|
            if spectr[i] > pk
              pk = spectr[i]
              pkloc = i
            end
          end
          pit = (pkloc + (pkloc > 0 ? interpolated_peak_offset.call(*spectr[pkloc - 1, 3]) : 0.0) *
                        srate()) / transform_size
          if (pitch - pit).abs < srate / (2 * transform_size)
            rtn = -(transform_size / 2)
          end
        end
        vct_fill!(data, 0.0)
      end
      rtn
    end
  end

  # file2vct and a sort of cue-list, I think

  add_help(:file2vct, "file2vct(file) returns a vct with file's data")
  def file2vct(file)
    len = mus_sound_frames(file)
    reader = make_sample_reader(0, file)
    data = make_vct!(len) do next_sample(reader) end
    free_sample_reader(reader)
    data
  end

  add_help(:add_notes,
           "add_notes(notes, [snd=false, [chn=false]]) \
adds (mixes) 'notes' which is a list of lists of the form: \
[file, offset=0.0, amp=1.0] starting at the cursor in the \
currently selected channel: add_notes([[\"oboe.snd\"], [\"pistol.snd\", 1.0, 2.0]])")
  def add_notes(notes, snd = false, chn = false)
    start = cursor(snd, chn)
    as_one_edit_rb("%s(%s", get_func_name, notes.inspect) do
      (notes or []).each do |note|
        file, offset, amp = note
        beg = start + (srate(snd) * (offset or 0.0)).floor
        if amp and amp != 1.0
          mix_vct(vct_scale!(file2vct(file), amp), beg, snd, chn, false,
                  format("%s(%s", get_func_name, notes.inspect))
        else
          mix(file, beg, 0, snd, chn, false)
        end
      end
    end
  end

  add_help(:region_play_list,
           "region_play_list(data) \
'data' is list of lists [[time, reg], ...], time in secs, \ setting up a sort of play list: \
region_play_list([[0.0, 0], [0.5, 1], [1.0, 2], [1.0, 0]])")
  def region_play_list(data)
    (data or []).each do |tm, rg|
      tm = (1000.0 * tm).floor
      if region?(rg)
        call_in(tm, lambda do | | play_region(rg) end)
      end
    end
  end

  add_help(:region_play_sequence,
           "region_play_sequence(data) \
'data' is list of region ids which will be played one after the other: \
region_play_sequence([0, 2, 1])")
  def region_play_sequence(data)
    time = 0.0
    region_play_list(data.map do |id|
                       cur = time
                       time += region_frames(id) / region_srate(id)
                       [cur, id]
                     end)
  end

  # replace-with-selection

  add_help(:replace_with_selection,
           "replace_with_selection() \
replaces the samples from the cursor with the current selection")
  def replace_with_selection
    beg = cursor
    len = selection_frames
    delete_samples(beg, len)
    insert_selection(beg)
  end

  # explode-sf2

  add_help(:explode_sf2,
           "explode_sf2() \
turns the currently selected soundfont file into a bunch of files of the form sample-name.aif")
  def explode_sf2
    (soundfont_info() or []).each do |name, start, loop_start, loop_end|
      filename = name + ".aif"
      if selection?
        set_selection_member?(false, true)
      end
      set_selection_member?(true)
      set_selection_position(start)
      set_selection_frames(frames - start)
      save_selection(filename, Mus_aifc)
      temp = open_sound(filename)
      set_sound_loop_info([loop_start, loop_end], temp)
      close_sound(temp)
    end
  end

  # open-next-file-in-directory

  class Next_file
    def initialize
      @last_file_opened = ""
      @current_directory = ""
      @current_sorted_files = []
    end

    def open_next_file_in_directory
      unless $open_hook.member?("open-next-file-in-directory")
        $open_hook.add_hook!("open-next-file-in-directory") do |fname|
          self.get_current_directory(fname)
        end
      end
      if @last_file_opened.empty? and sounds
        @last_file_opened = file_name(Snd.snd)
      end
      if @current_directory.empty?
        unless sounds
          get_current_files(Dir.pwd)
        else
          get_current_files(File.split(@last_file_opened).first)
        end
      end
      if @current_sorted_files.empty?
        Snd.raise(:no_such_file)
      else
        next_file = find_next_file
        if find_sound(next_file)
          Snd.raise(:file_already_open, next_file)
        else
          sounds and close_sound(Snd.snd)
          open_sound(next_file)
        end
      end
      true
    end

    private
    def find_next_file
      choose_next = @last_file_opened.empty?
      just_filename = File.basename(@last_file_opened)
      f = callcc do |ret|
        @current_sorted_files.each do |file|
          ret.call(file) if choose_next
          if file == just_filename
            choose_next = true
          end
        end
        @current_sorted_files[0] # wrapped around
      end
      @current_directory + "/" + f
    end

    def get_current_files(dir)
      @current_directory = dir
      @current_sorted_files = sound_files_in_directory(dir).sort
    end

    def get_current_directory(filename)
      Snd.display(@last_file_opened = filename)
      new_path = File.split(mus_expand_filename(filename)).first
      if @current_directory.empty? or @current_directory != new_path
        get_current_files(new_path)
      end
      false
    end
  end

  def click_middle_button_to_open_next_file_in_directory
    nf = Next_file.new
    $mouse_click_hook.add_hook!("next-file") do |snd, chn, button, state, x, y, axis|
      if button == 2
        nf.open_next_file_in_directory
      end
    end
  end

  # chain-dsps

  def chain_dsps(start, dur, *dsps)
    dsp_chain = dsps.map do |gen|
      if array?(gen)
        make_env(:envelope, gen, :duration, dur)
      else
        gen
      end
    end
    run_instrument(start, dur) do
      val = 0.0
      dsp_chain.each do |gen|
        if env?(gen)
          val *= gen.run
        elsif readin?(gen)
          val += gen.run
        else
          val = gen.run(val)
        end
      end
      val
    end
  end

=begin
  with_sound() do
    chain_dsps(0, 1.0, [0, 0, 1, 1, 2, 0], make_oscil(:frequency, 440))
    chain_dsps(0, 1.0, [0, 0, 1, 1, 2, 0], make_one_pole(0.5), make_readin("oboe.snd"))
    chain_dsps(0, 1.0, [0, 0, 1, 1, 2, 0], let(make_oscil(:frequency, 220),
                                                make_oscil(:frequency, 440)) do |osc1, osc2|
                  lambda do |val| osc1.run(val) + osc2.run(2.0 * val) end
                end)
  end
=end

  # cursor-follows-play and stays where it was when the play ended

  module Cursor_follows_play
    def local_dac_func(data)
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          if cursor(snd, chn) != original_cursor(snd, chn)
            set_current_cursor(cursor(snd, chn), snd, chn)
          end
        end
      end
    end

    def local_start_playing_func(snd)
      channels(snd).times do |chn|
        set_original_cursor(cursor(snd, chn), snd, chn)
        set_current_cursor(cursor(snd, chn), snd, chn)
      end
    end

    def local_stop_playing_func(snd)
      set_cursor(current_cursor(snd, 0), snd, true)
    end

    def current_cursor(snd, chn)
      channel_property(:cursor, snd, chn)
    end

    def set_current_cursor(val, snd, chn)
      set_channel_property(:cursor, val, snd, chn)
    end

    def original_cursor(snd, chn)
      channel_property(:original_cursor, snd, chn)
    end

    def set_original_cursor(val, snd, chn)
      set_channel_property(:original_cursor, val, snd, chn)
    end
  end

  def if_cursor_follows_play_it_stays_where_play_stopped(enable = true)
    include Cursor_follows_play
    if enable
      $dac_hook.add_hook!("cursor") do |data| local_dac_func(data) end
      $start_playing_hook.add_hook!("cursor") do |snd| local_start_playing_func(snd) end
      $stop_playing_hook.add_hook!("cursor") do |snd| local_stop_playing_func(snd) end
    else
      $dac_hook.remove_hook!("cursor")
      $start_playing_hook.remove_hook!("cursor")
      $stop_playing_hook.remove_hook!("cursor")
    end
  end
  
  # smooth-channel as virtual op

  def smooth_channel_via_ptree(beg = 0, dur = false, snd = false, chn = false, edpos = false)
    y0 = sample(beg, snd, chn, edpos)
    y1 = sample(beg + (dur or frames(snd, chn) - 1), snd, chn, edpos)
    init_angle = y1 > y0 ? PI : 0.0
    off = 0.5 * (y0 + y1)
    scale = 0.5 * (y1 - y0).abs
    data1 = vct(0.0, 0.0, init_angle, off, scale)
    ptree_channel(lambda { |y, data, forward|
                    angle = data[0]
                    incr = data[1]
                    val = data[3] + data[4] * cos(data[2] + angle)
                    if forward
                      data[0] = angle + incr
                    else
                      data[0] = angle - incr
                    end
                    val
                  }, beg, dur, snd, chn, edpos, true,
                  lambda { |frag_beg, frag_dur|
                    incr = PI / frag_dur
                    data1[1] = incr
                    data1[0] = frag_beg * incr
                    data1
                  }, format("%s(%s, %s", get_func_name, beg, dur))
  end

  # ring-modulate-channel (ring-mod as virtual op)

  def ring_modulate_channel(freq, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda { |y, data, forward|
                    angle = data[0]
                    incr = data[1]
                    val = y * sin(angle)
                    if forward
                      data[0] = angle + incr
                    else
                      data[0] = angle - incr
                    end
                    val
                  }, beg, dur, snd, chn, edpos, false,
                  lambda { |frag_beg, frag_dur|
                    incr = (TWO_PI * freq) / srate(snd)
                    vct((frag_beg * incr).divmod(TWO_PI).last, incr)
                  }, format("%s(%s, %s, %s", get_func_name, freq, beg, dur))
  end

  # re-order channels

  def scramble_channels(*new_order)
    len = new_order.length
    swap_once = lambda do |current, desired, n|
      if n != len
        cur_orig, cur_cur = current[n][0, 2]
        dst = desired[n]
        if cur_orig != dst
          swap_channels(false, cur_cur, false, dst)
          current[dst][0] = cur_orig
        end
        swape_once.call(current, desired, n + 1)
      end
    end
    swap_once.call(make_array(len) do |i| [i, i] end, new_order, 0)
  end

  def scramble_channel(silence)
    buffer = make_moving_average(128)
    silence = silence / 128.0
    edges = []
    samp = 0
    in_silence = true
    old_max = max_regions
    old_tags = with_mix_tags
    set_max_regions(1024)
    set_with_mix_tags(false)
    scan_channel(lambda do |y|
                   if (now_silent = moving_average(buffer, y * y) < silence) != in_silence
                     edges.push(samp)
                   end
                   in_silence = now_silent
                   samp += 1
                   false
                 end)
    edges.push(frames)
    len = edges.length
    pieces = make_array(len)
    start = 0
    edges.each_with_index do |fin, i|
      pieces[i] = make_region(0, fin)
      start = fin
    end
    start = 0
    as_one_edit(lambda do
                  scale_by(0.0)
                  len.times do
                    this = random(len)
                    reg, pieces[this] = pieces[this], false
                    unless reg
                      (this.round + 1).upto(len - 1) do |i|
                        reg = pieces[i]
                        reg and pieces[i] = false
                      end
                    end
                    mix_region(start, reg)
                    start += region_frames(reg)
                    forget_region(reg)
                  end
                end)
  rescue
  ensure
    set_max_regions(old_max)
    set_with_mix_tags(old_tags)
  end

  # reorder blocks within channel

  add_help(:reverse_by_blocks,
           "reverse_by_blocks(block_len, snd=false, chn=false) \
divide sound into block-len blocks, recombine blocks in reverse order.")
  def reverse_by_blocks(block_len, snd = false, chn = false)
    len = frames(snd, chn)
    num_blocks = (len / (srate(snd).to_f * block_len)).floor
    if num_blocks > 1
      actual_block_len = len / num_blocks
      rd = make_sample_reader(len - actual_block_len, snd, chn)
      beg = 0
      ctr = 1
      map_channel(lambda do |y|
                    val = read_sample(rd)
                    if beg < 10
                      val = val * beg * 0.1
                    else
                      if beg > actual_block_len - 10
                        val = val * (actual_block_len - beg) * 0.1
                      end
                    end
                    beg += 1
                    if beg == actual_block_len
                      ctr += 1
                      beg = 0
                      rd = make_sample_reader([len - ctr * actual_block_len, 0].max, snd, chn)
                    end
                    val
                  end,
                  0, false, snd, chn, false, format("%s(%s", get_func_name, block_len))
    end
  end

  add_help(:reverse_within_blocks,
           "reverse_within_blocks(block_len, snd=false, chn=false) \
divide sound into blocks, recombine in order, but each block internally reversed.")
  def reverse_within_blocks(block_len, snd = false, chn = false)
    len = frames(snd, chn)
    num_blocks = (len / (srate(snd).to_f * block_len)).floor
    if num_blocks > 1
      actual_block_len = len / num_blocks
      no_clicks_env = [0.0, 0.0, 0.01, 1.0, 0.99, 1.0, 1.0, 0.0]
      as_one_edit(lambda do | |
                    0.step(len, actual_block_len) do |beg|
                      reverse_channel(beg, actual_block_len, snd, chn)
                      env_channel(no_clicks_env, beg, actual_block_len, snd, chn)
                    end
                  end,
                  format("%s(%s", get_func_name, block_len))
    else
      reverse_channel(0, false, snd, chn)
    end
  end

  def segment_maxamp(name, beg, dur)
    mx = 0.0
    rd = make_sample_reader(beg, name)
    dur.times do mx = [mx, next_sample(rd).abs].max end
    free_sample_reader(rd)
    mx
  end

  def segment_sound(name, high, low)
    len = mus_sound_frames(name)
    reader = make_sample_reader(0, name)
    avg = make_moving_average(:size, 128)
    lavg = make_moving_average(:size, 2048)
    segments = Vct.new(100)
    segctr = 0
    possible_end = 0
    in_sound = false
    len.times do |i|
      samp = next_sample(reader).abs
      val = moving_average(avg, samp)
      lval = moving_average(lavg, samp)
      if in_sound
        if val < low
          possible_end = i
          if lval < low
            segments[segctr] = possible_end + 128
            segctr += 1
            in_sound = false
          end
        else
          if val > high
            segments[segctr] = i - 128
            segctr += 1
            in_sound = true
          end
        end
      end
    end
    free_sample_reader(reader)
    if in_sound
      segments[segctr] = len
      [segctr + 1, segments]
    else
      [segctr, segments]
    end
  end

  def do_one_directory(fd, dir_name, ins_name, high = 0.01, low = 0.001)
    snd_print("# #{dir_name}")
    sound_files_in_directory(dir_name).each do |sound|
      sound_name = dir_name + "/" + sound
      boundary_data = segment_sound(sound_name, high, low)
      segments, boundaries = boundary_data
      fd.printf("\n\n#    ", sound)
      fd.printf("(%s %s", ins_name, sound_name.inspect)
      0.step(segments, 2) do |bnd|
        segbeg = boundaries[bnd].to_i
        segbeg = boundaries[bnd + 1].to_i
        fd.printf("(%s %s %s)", segbeg, segdur, segment_maxamp(sound_name, segbeg, segdur))
        fd.printf(")")
      end
      mus_sound_forget(sound_name)
    end
  end
  
  def sound2segment_data(main_dir, output_file = "sounds.data")
    File.open(output_file, "w") do |fd|
      old_fam = with_file_monitor
      set_with_file_monitor(false)
      fd.printf("# sound data from %s", main_dir.inspect)
      if main_dir[-1] != "/" then main_dir += "/" end
      Dir[main_dir].each do |dir|
        ins_name = dir.downcase.tr(" ", "")
        fd.printf("\n\n# ---------------- %s ----------------", dir)
        if dir == "Piano"
          Dir[main_dir + dir].each do |inner_dir|
            do_one_directory(fd, main_dir + dir + "/" + inner_dir, ins_name, 0.001, 0.0001)
          end
        else
          do_one_directory(fd, main_dir + dir, ins_name)
        end
      end
      set_with_file_monitor(old_fam)
    end
  end
  # sounds2segment_data(ENV['HOME'] + "/.snd.d/iowa/sounds/", "iowa.data")

  add_help(:channel_clipped?,
           "channel_clipped?(snd=false, chn=false)  \
returns true and a sample number if it finds clipping.")
  def channel_clipped?(snd = false, chn = false)
    last_y = 0.0
    scan_channel(lambda do |y|
                   result = (y.abs >= 0.9999 and last_y.abs >= 0.9999)
                   last_y = y
                   result
                 end, 0, false, snd, chn)
  end

  # scan-sound
  
  def scan_sound(func, beg = 0, dur = false, snd = false)
    if sound?(index = Snd.snd(snd))
      if (chns = channels(index)) == 1
        scan_channel(lambda do |y| func.call(y, 0) end, beg, dur, index, 0)
      else
        len = frames(index)
        fin = (dur ? [len, beg + dur].min : len)
        readers = make_array(chns) do |chn| make_sample_reader(beg, index, chn) end
        result = false
        beg.upto(fin) do |i|
          local_result = true
          readers.each_with_index do |rd, chn|
            local_result = (func.call(rd.call, chn) and local_result)
          end
          if local_result
            result = [true, i]
            break
          end
        end
        result
      end
    else
      Snd.raise(:no_such_sound, get_func_name, snd)
    end
  end

  def scan_sound_rb(beg = 0, dur = false, snd = false, &func)
    scan_sound(func, beg, dur, snd)
  end
end

include Examp

module Moog
  class Moog_filter < Musgen
    Gaintable = vct(0.999969, 0.990082, 0.980347, 0.970764, 0.961304, 0.951996,
                    0.94281, 0.933777, 0.924866, 0.916077, 0.90741, 0.898865,
                    0.890442, 0.882141, 0.873962, 0.865906, 0.857941, 0.850067,
                    0.842346, 0.834686, 0.827148, 0.819733, 0.812378, 0.805145,
                    0.798004, 0.790955, 0.783997, 0.77713, 0.770355, 0.763672,
                    0.75708, 0.75058, 0.744141, 0.737793, 0.731537, 0.725342,
                    0.719238, 0.713196, 0.707245, 0.701355, 0.695557, 0.689819,
                    0.684174, 0.678558, 0.673035, 0.667572, 0.66217, 0.65686,
                    0.651581, 0.646393, 0.641235, 0.636169, 0.631134, 0.62619,
                    0.621277, 0.616425, 0.611633, 0.606903, 0.602234, 0.597626,
                    0.593048, 0.588531, 0.584045, 0.579651, 0.575287, 0.570953,
                    0.566681, 0.562469, 0.558289, 0.554169, 0.550079, 0.546051,
                    0.542053, 0.538116, 0.53421, 0.530334, 0.52652, 0.522736,
                    0.518982, 0.515289, 0.511627, 0.507996, 0.504425, 0.500885,
                    0.497375, 0.493896, 0.490448, 0.487061, 0.483704, 0.480377,
                    0.477081, 0.473816, 0.470581, 0.467377, 0.464203, 0.46109,
                    0.457977, 0.454926, 0.451874, 0.448883, 0.445892, 0.442932,
                    0.440033, 0.437134, 0.434265, 0.431427, 0.428619, 0.425842,
                    0.423096, 0.42038, 0.417664, 0.415009, 0.412354, 0.409729,
                    0.407135, 0.404572, 0.402008, 0.399506, 0.397003, 0.394501,
                    0.392059, 0.389618, 0.387207, 0.384827, 0.382477, 0.380127,
                    0.377808, 0.375488, 0.37323, 0.370972, 0.368713, 0.366516,
                    0.364319, 0.362122, 0.359985, 0.357849, 0.355713, 0.353607,
                    0.351532, 0.349457, 0.347412, 0.345398, 0.343384, 0.34137,
                    0.339417, 0.337463, 0.33551, 0.333588, 0.331665, 0.329773,
                    0.327911, 0.32605, 0.324188, 0.322357, 0.320557, 0.318756,
                    0.316986, 0.315216, 0.313446, 0.311707, 0.309998, 0.308289,
                    0.30658, 0.304901, 0.303223, 0.301575, 0.299927, 0.298309,
                    0.296692, 0.295074, 0.293488, 0.291931, 0.290375, 0.288818,
                    0.287262, 0.285736, 0.284241, 0.282715, 0.28125, 0.279755,
                    0.27829, 0.276825, 0.275391, 0.273956, 0.272552, 0.271118,
                    0.269745, 0.268341, 0.266968, 0.265594, 0.264252, 0.262909,
                    0.261566, 0.260223, 0.258911, 0.257599, 0.256317, 0.255035, 0.25375)
    Freqtable = [0, -1,
      0.03311111, -0.9,
      0.06457143, -0.8,
      0.0960272, -0.7,
      0.127483, -0.6,
      0.1605941, -0.5,
      0.1920544, -0.4,
      0.22682086, -0.3,
      0.2615873, -0.2,
      0.29801363, -0.1,
      0.33278003, -0.0,
      0.37086168, 0.1,
      0.40893877, 0.2,
      0.4536417, 0.3,
      0.5, 0.4,
      0.5463583, 0.5,
      0.5943719, 0.6,
      0.6556281, 0.7,
      0.72185487, 0.8,
      0.8096009, 0.9,
      0.87913835, 0.95,
      0.9933787, 1,
      1, 1]

    def initialize(freq, q)
      super()
      @frequency = freq
      @Q = q
      @state = make_vct(4)
      @A = 0.0
      @freqtable = envelope_interp(freq / (srate() * 0.5), Freqtable)
    end
    attr_reader :frequency, :state, :freqtable, :A
    attr_accessor :Q

    def inspect
      format("%s.new(%s, %s)", self.class, @frequency, @Q)
    end

    def to_s
      format("#<%s freq: %1.3f, Q: %s>", self.class, @frequency, @Q)
    end

    def run_func(val1 = 0.0, val2 = 0.0)
      filter(val1)
    end

    def frequency=(freq)
      @freqtable = envelope_interp(freq / (srate() * 0.5), Freqtable)
      @frequency = freq
    end
    
    def filter(insig)
      a = 0.25 * (insig - @A)
      @state.map! do |st|
        new_a = saturate(a + @freqtable * (a - st))
        a = saturate(new_a + st)
        new_a
      end
      ix = @freqtable * 99.0
      ixint = ix.floor
      ixfrac = ix - ixint
      @A = a * @Q * ((1.0 - ixfrac) * Gaintable[ixint + 99] + ixfrac * Gaintable[ixint + 100])
      a
    end

    private
    def saturate(x)
      [[x, -0.95].max, 0.95].min
    end
  end

  add_help(:make_moog_filter,
           "make_moog_filter([freq=440.0, [Q=0]]) makes a new moog_filter generator. \
'frequency' is the cutoff in Hz, \
'Q' sets the resonance: 0 = no resonance, 1: oscillates at 'frequency'")
  def make_moog_filter(freq = 440.0, q = 0)
    Moog_filter.new(freq, q)
  end

  add_help(:moog_filter,
           "moog_filter(moog, [insig=0.0]) is the generator associated with make_moog_filter")
  def moog_filter(mg, insig = 0.0)
    mg.filter(insig)
  end

  def moog(freq, q)
    mg = Moog_filter.new(freq, q)
    lambda do |inval| mg.filter(inval) end
  end
end

include Moog

# examp.rb ends here
