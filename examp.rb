# examp.rb -- Guile -> Ruby translation -*- snd-ruby -*-

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Wed Sep 04 18:34:00 CEST 2002
# Last: Mon Mar 28 04:54:27 CEST 2005

# Commentary:
#
# Extensions to Ruby:
# 
# provided?(feature)
# features
# all_features
# 
# array?(obj)   alias list?(obj)
# string?(obj)
# symbol?(obj)
# number?(obj)
# integer?(obj)
# float?(obj)
# proc?(obj)
# thunk?(obj)
# mus?(obj)
#
# backward compatibility methods:
#  String#to_sym, Symbol#to_sym
#  make_array(len, init) do |i| ... end
#  Array#zip, Array#insert
#
# extensions to existing classes
# 
# class Array
#   to_pairs
#   each_pair do |x, y| ... end
#   to_string(len)
#   first=(val)
#   last=(val)
#
# make_vct!(len, init) do |i| ... end
#
# sound_data2string(sd)
#
# class SoundData
#   length
#   each(chn = nil)
#   each_with_index(chn = nil)
#   map(chn = nil)
#   map!(chn = nil)
#   to_a
#   to_s
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
#   run(arg1 = 0.0, arg2 = 0.0)
#   inspect
#   xcoeff=(index, val)
#   ycoeff=(index, val)
#   a0  a0=(val)
#   a1  a1=(val)
#   a2  a2=(val)
#   b1  b1=(val)
#   b2  b2=(val)
#
# class Integer
#  even?
#  odd?
#  prime?
#
# module Enumerable
#  map_with_index do |x, i| ... end
#  map_with_index! do |x, i| ... end
#  cycle(n)
#
# as_one_edit_rb(*origin, &body)
# map_channel_rb(beg, dur, snd, chn, edpos, edname, &body)
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
#
# Utilities:
#
# close_sound_extend(snd)
# get_func_name(n)
# times2samples(start, dur)
# random(n)
# logn(r, b)
# car(v), cadr(v), caddr(v), cdr(v)
# warn(*args), die(*args), error(*args)
# rbm_message(*args), message(*args), debug(*args), debug_trace(*args)
#
# sounds2array
# regions2array
# snd_snd(snd)
# snd_chn(chn)
# snd_var(name, snd, chn)
# set_snd_var(name, val, snd, chn)
# snd_catch(tag, retval = nil, &body)
# snd_throw(tag, *args)
# snd_raise(tag, *args)
# c_g?() (if not in Snd)
# let(*args) do |*args| ... end
# gloop(*args) do |args| ... end
# get_args(args, key, default)
# get_shift_args(args, key, default)
# get_class_args(args, klass, default)
# get_class_or_key(args, klass, key, default)
# Args(args, *rest)
# load_init_file(file)
#
# module Examp (examp.scm)
#  selection_rms
#  region_rms(n = 0)
#  window_samples(snd = false, chn = false)
#  display_energy(snd = false, chn = false)
#  display_db(snd = false, chn = false)
#  window_rms
#  fft_peak(snd, chn, scale)
#  finfo(file)
#  correlate(snd, chn, y0, y1)
#
#  open_buffer(file)
#  close_buffer(snd)
#  add_to_reopen_menu(snd)
#  check_reopen_menu(file)
#
#  zoom_spectrum(snd, chn, y0, y1)
#  zoom_fft(snd, chn, y0, y1)
#  superimpose_ffts(snd, chn, y0, y1)
#  locate_zero(limit)
#  shell(*cmd)
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
#  do_all_chans(origin, &func)
#  update_graphs
#  do_chans(*origin, &func)
#  do_sound_chans(*origin, &func)
#  every_sample?(&func)
#  sort_samples(nbins)
#  place_sound(mono_snd, stereo_snd, pan_env)
#
#  fft_edit(bottom, top, snd = false, chn = false)
#  fft_squelch(squelch, snd = false, chn = false)
#  fft_cancel(lo_freq, hi_freq, snd = false, chn = false)
#  ramp(gen, up)
#  make_ramp(size = 128)
#  squelch_vowels(snd = false, chn = false)
#  fft_env_data(fft_env, snd = false, chn = false)
#  fft_env_edit(fft_env, snd = false, chn = false)
#  fft_env_interp(env1, env2, interp, snd = false, chn = false)
#  fft_smoother(cutoff, start, samps, snd = false, chn = false)
#
#  comb_filter(scaler, size)
#  comb_chord(scaler, size, amp, interval_one = 0.75, interval_two = 1.2)
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
#  hello_dentist(freq, amp, snd = false, chn = false)
#  fp(sr, osamp, osfreq, snd = false, chn = false)
#  compand(doc = false)
#  compand_channel(beg = 0, dur = false, snd = false, chn = false, edpos = false)
#  expsrc(rate, snd = false, chn = false)
#  expsnd(gr_env, snd = false, chn = false)
#  cross_synthesis(cross_snd, amp, fftsize, r)
#  voiced2unvoiced(amp, fftsize, r, temp, snd = false, chn = false)
#  cnvtest(snd0, snd1, amp)
#
#  swap_selection_channels
#  make_sound_interp(start, snd = false, chn = false)
#  sound_interp(func, loc)
#  env_sound_interp(envelope, time_scale = 1.0, snd = false, chn = false)
#  title_with_date
#  filtered_env(en, snd = false, chn = false)
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
#  add_notes(notes, snd = false, chn = false)
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
#  if_cursor_follows_play_it_stays_where_play_stopped(enable = true)
#
#  smooth_channel_via_ptree(beg = 0, dur = false, snd = false, chn = false, edpos = false)
#  ring_modulate_channel(freq, beg = 0, dur = false, snd = false, chn = false, edpos = false)
#  scramble_channels(*new_order)
#  scramble_channel(silence)
#
# module Moog (moog.scm)
#  class Moog_filter
#    initialize(freq, q)
#    frequency=(freq)
#    filter(insig)
#
#  make_moog_filter(freq = 440.0, q = 0)
#  moog_filter(moog, insig = 0.0)
#  moog(freq, q)
# 
# Code:

##
## Extensions to Ruby
##
 
def provided?(feature)
  case feature
  when Symbol
    $".member?(feature.to_s.tr("_", "-"))
  when String
    $".member?(feature)
  else
    raise(ArgumentError, "provided?: expect a Symbol or String")
  end
end

def features
  $".map do |f|
    next if f.include? "."
    f
  end.compact
end

# features and loaded files
def all_features
  $"
end

def array?(obj)
  obj.kind_of?(Array)
end
alias list? array?

def string?(obj)
  obj.kind_of?(String)
end

def symbol?(obj)
  obj.kind_of?(Symbol)
end

def number?(obj)
  obj.kind_of?(Numeric)
end

def integer?(obj)
  obj.kind_of?(Integer)
end

def float?(obj)
  obj.kind_of?(Float)
end

def proc?(obj)
  obj.kind_of?(Proc)
end

def thunk?(obj)
  obj.kind_of?(Proc) and obj.arity.zero?
end

def mus?(obj)
  obj.kind_of?(Mus)
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
        raise(NameError, format("[version %s] undefined method `%s'", RUBY_VERSION, id.id2name))
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
# supress debug messages (mostly on older Ruby versions)
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
    if defined?(@description) and @description.kind_of?(String) and (not @description.empty?)
      @description
    else
      "no description available"
    end
  end
  alias info description
end

require "ws"

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
  len = if len.kind_of?(Numeric)
          len.abs.to_i
        else
          0
        end
  if block_given?
    Array.new(len, init).map_with_index do |x, i| yield(i) end
  else
    Array.new(len, init)
  end
end

class Array
  # Array#zip, new in ruby core since 19-Nov-2002.
=begin
  a = [4, 5, 6]
  b = [7, 8, 9]
  [1, 2, 3].zip(a, b) --> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  [1, 2].zip(a, b)    --> [[1, 4, 7], [2, 5, 8]]
  a.zip([1, 2],[8])   --> [[4, 1, 8], [5, 2, nil], [6, nil, nil]]
=end
  def zip(*args)
    args.map! do |x| x.to_a end
    self.each_index do |i|
      ary = [self[i]]
      args.each do |x| ary.push(x[i]) end
      if block_given?
        yield(*ary)
      else
        self[i] = ary
      end
    end
    self
  end unless defined? [].zip

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
    if self.length.even?
      0.step(self.length - 1, 2) do |i|
        ary.push([self[i], self[i + 1]])
      end
    end
    ary
  end

  # [0.0, 0.0, 0.5, 0.2, 1.0, 1.0].each_pair do |x, y| print x, " ", y, "\n" end
  # --> 0.0 0.0
  #     0.5 0.2
  #     1.0 1.0
  def each_pair
    ary = []
    if self.length.even?
      0.step(self.length - 1, 2) do |i|
        ary.push(yield([self[i], self[i + 1]]))
      end
    end
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
end

def make_vct!(len, init = 0.0, &body)
  if block_given?
    Vct.new(len, &body)
  else
    Vct.new(len, init)
  end
end

def sound_data2string(sd)
  sd.to_s
end

class SoundData
  alias old_length length
  def length
    self.size / self.chans
  end

  alias old_each each
  def each(chn = nil)
    if chn
      self.length.times do |i| yield(self[chn, i]) end
    else
      old_each do |val| yield(val) end
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

  # returns an array of sd.chans vcts
  def to_a
    make_array(self.chans) do |chn| sound_data2vct(self, chn, Vct.new(self.length)) end
  end

  alias old_inspect inspect
  alias inspect to_s
  # returns a string containing an array of sd.chans vcts
  def to_s
    str = ""
    self.to_a.each do |vc| str += "%s, " % vc.to_s end
    if str.chop!.chop!
      "[" + str + "]"
    else
      "nil"
    end
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

  alias old_inspect inspect
  def inspect
    "#<" + mus_describe(self) + ">"
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

class Integer
  def even?
    self.modulo(2) == 0
  end

  def odd?
    self.modulo(2) != 0
  end

  def prime?
    (self == 2) or
    (self.odd? and 3.step(sqrt(self), 2) do |i| return false if self.modulo(i) == 0 end)
  end
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

  def cycle(n = 1)
    unless defined? @cycle_index
      @cycle_index = -1
    end
    if n == 1
      self[@cycle_index = (@cycle_index + 1) % self.length]
    else
      (0...n).map do |i| self.cycle end
    end
  end

  # FIXME: @cycle_index needs work
  def cycle=(val)
    unless defined? @cycle_index
      @cycle_index = -1
    end
    self[@cycle_index = (@cycle_index + 1) % self.length] = val
  end
  attr_accessor :cycle_index
end

def as_one_edit_rb(*origin, &body)
  # ruby compatibility:
  # ruby pre 1.9: lambda do end.arity != lambda do | | end.arity
  # ruby     1.9: they are even (0)
  as_one_edit(lambda do | | body.call end, origin.empty? ? "" : format(*origin))
end

def map_channel_rb(beg = 0, dur = false,
                   snd = false, chn = false, edpos = false, edname = false, &body)
  map_channel(body, beg, dur, snd, chn, edpos, edname) 
end

class Proc
  include Info
  alias run call

  # converts a Proc to a Method
  #
  # m = lambda do |*args| p args end
  # m.to_method(:func)
  # func(1, 2, 3) ==> [1, 2, 3]
  # 
  # lambda do |x| p x end.to_method(:foo)  foo("text1") --> "text1"
  # lambda do |x| p x end.to_method("bar") bar("text2") --> "text2"
  
  def to_method(name, klass = Object)
    name = case name
           when String
             name.intern
           when Symbol
             name
           else
             error("%s#%s(name, klass = Object): `name' must be a String or Symbol",
                   self.class, get_func_name)
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
  # $rbm_notehook = lambda do |name| clm_print(name) if name =~ /viol/ end
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
    file, line = self.to_s.sub(/>/, "").split(/@/).last.split(/:/)
    if file == "(eval)" or file == "(irb)"
      return "no file found for procedure #{self.inspect}"
    elsif (not File.exist?(file))
      return "Sorry, you need a higher ruby version to use Proc#to_str.
This works only with newer ruby versions (I assume >= 1.8.x).
Proc#inspect must return #<Proc:0x01234567@xxx:x> not only #{self.inspect}!"
    end
    line = line.to_i
    body = ""
    brck = i = 0
    blck = -1
    first_line = true
    File.foreach(file) do |f|
      i += 1
      next if i < line
      body << f
      if first_line
        ary = f.split(/ /)
        blck += ary.grep(/\bdo\b|\{/).length
        blck -= ary.grep(/\bend\b|\}/).length
        brck += ary.grep(/\(/).length
        brck -= ary.grep(/\)/).length
        if blck.zero? and brck.zero?
          first_line = false
          blck = 1
        else
          break if (ary.grep(/\bdo\b|\{/).length == ary.grep(/\bend\b|\}/).length) and
            (ary.grep(/\(/).length == ary.grep(/\)/).length)
        end
        next
      end
      # don't count statement modifiers like: `code if conditional'
      next if /\s*\S+\s*(if|unless|while|until)+/ =~ f
      f.split(/\W+/).each do |s|
        case s
        when "{", "do", "while", "until", "if", "unless", "case", "begin"
          blck += 1
        when "}", "end"
          blck -= 1
        end
      end
      break if blck.zero?
    end
    body
  end

  # returns the inner body without 'lambda' etc.
  def to_body
    body = self.to_str
    return body if body =~ /no file found for procedure/
    if body.split(/\n/).length == 1
      body.chomp!.sub!(/^(?:\s*\w+(?:\(.*\))??\s*(?:do\s+|\{\s*))(.*)\s*(?:end|\})$/, '\1')
    else
      brck = 0
      ws = true
      body = ""
      self.to_str.each_line do |s|
        if ws
          s.each_byte do |c|
            case c
            when ?(
              brck += 1
            when ?)
              brck -= 1
            end
          end
          ws = false if brck.zero?
        else
          body << s
        end
      end
      body = body.split(/\n/)
      str = (body.last.split(/\W+/)[0..-2]).join
      body[-1] = (str.strip.empty? ? "" : str)
      body = body.join("\n")
    end
    body
  end
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

if provided? "snd-nogui"
  alias close_sound_extend close_sound
else
  def close_sound_extend(snd)
    # 5 == Notebook
    if main_widgets[5] and selected_sound <= snd
      idx = 0
      snds = sounds() and idx = snds.index(snd)
      close_sound(snd)
      snds = sounds() and set_selected_sound(snds[idx < snds.length ? idx : -1])
    else
      close_sound(snd)
    end
  end
end

add_help(:get_func_name, "get_func_name([n=1]) returns function name string")
def get_func_name(n = 1)
  caller(n)[0].scan(/^.*:in `(.*)'/)[0][0]
end

add_help(:times2samples,
         "times2samples(start, dur) \
START and DUR are in seconds; returns array [beg, len] in samples")
def times2samples(start, dur = nil)
  beg = seconds2samples(start)
  [beg, beg + seconds2samples(dur)]
end

def random(n)
  if n.zero?
    0.0
  else
    n < 0 ? -mus_random(n).abs : mus_random(n).abs
  end
end
alias rbm_random random

def logn(r, b = 10)
  error("r must be > 0 (r = %s)", r.inspect) if r <= 0
  error("b must be > 0 (b = %s)", b.inspect) if b <= 0
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
  v.shift
  v
end

def verbose_message_string(stack_p, remark, *args)
  str = args.empty? ? "" : format(*args)
  if $!
    if stack_p
      str += format(": %s\n%s%s\n", $!, remark, $@.join(format("\n%s", remark)))
    else
      str += format(": %s", $!)
    end
  else
    if stack_p
      str += format("\n%s%s\n", remark, caller(0)[2..-1].join(format("\n%s", remark)))
    end
  end
  str
end

def warn(*args)
  str = "Warning: " << verbose_message_string($VERBOSE, "", *args)
  if provided? "snd"
    snd_warning(str)
    nil
  else
    rbm_message(str)
  end
end

def die(*args)
  rbm_message(verbose_message_string(true, "", *args))
  exit(1) unless provided? "snd"
end

def error(*args)
  raise verbose_message_string(true, "", *args)
end

# like printf(*args)

def rbm_message(*args)
  if provided?("snd") and (!(ENV["EMACS"] or provided?("snd-nogui")))
    snd_print("\n" + args.empty? ? "" : format(*args))
    nil
  else
    print(args.empty? ? "" : format(*args), "\n")
  end
end

# like printf(*args), prepends a comment sign

def message(*args)
  rbm_message("# %s", args.empty? ? "" : format(*args))
end

# debug("var1: %s, var2: %s", var1, var2) --> #<DEBUG: var1: value1, var2: value2>
# debug(var1, var2)                       --> #<DEBUG: ClassName: value1, ClassName: value2>

def debug(*args)
  if args[0].kind_of?(String) and /%/.match(args[0])
    fmt = args.shift
    fmt = format(fmt, *args.map do |x| x.inspect end)
  else
    len = args.length - 1
    fmt = ""
    args.each_with_index do |x, i|
      fmt += format("%s: %s", x.class, x.inspect)
      fmt += ", " if i < len
    end
  end
  rbm_message("#<DEBUG: %s>", fmt)
end

def debug_trace(*args)
  debug(*args)
  rbm_message(verbose_message_string(true, "# ", ""))
end

def sounds2array
  (sounds or []).reverse
end

def regions2array
  (regions or []).reverse
end

def snd_snd(snd = false)
  snd or selected_sound or (sounds and sounds[0])
end

def snd_chn(chn = false)
  chn or selected_channel or 0
end

# snd_var(:cursor_size) # => value

def snd_var(name, snd = :no_snd, chn = :no_chn)
  case name
  when Symbol
    if chn != :no_chn and chn != :no_chn
      # channel variables
      send(name, snd, chn)
    elsif snd != :no_snd and chn == :no_chn
      # sound variables
      send(name, snd)
    else
      # global variables
      send(name)
    end
  when String
    if chn != :no_chn and chn != :no_chn
      send(name, snd, chn)
    elsif snd != :no_snd and chn == :no_chn
      send(name, snd)
    else
      send(name.intern)
    end
  else
    raise(TypeError, "snd_var(name, [snd, [chn]]): NAME must be String or Symbol")
  end
end

# set_snd_var(:cursor_size, value) # => set_cursor_size(value)

def set_snd_var(name, val, snd = :no_snd, chn = :no_chn)
  if name.kind_of?(Symbol) or name.kind_of?(String)
    func = format("set_%s", name.to_s).intern
    if snd != :no_snd and chn != :no_chn
      send(func, val, snd, chn)
    elsif snd != :no_snd and chn == :no_chn
      send(func, val, snd)
    else
      send(func, val)
    end
  else
    raise(TypeError, "set_snd_var(name, val, [snd, [chn]]): NAME must be String or Symbol")
  end
end

Snd_error_tags = [
  :bad_arity,
  :bad_header,
  :bad_type,
  :cannot_apply_controls,
  :cannot_parse,
  :cannot_print,
  :cannot_save,
  :gsl_error,
  :mus_error,
  :no_active_selection,
  :no_data,
  :no_such_axis,
  :no_such_channel,
  :no_such_color,
  :no_such_colormap,
  :no_such_direction,
  :no_such_edit,
  :no_such_envelope,
  :no_such_file,
  :no_such_graphics_context,
  :no_such_key,
  :no_such_mark,
  :no_such_menu,
  :no_such_mix,
  :no_such_player,
  :no_such_plugin,
  :no_such_region,
  :no_such_sample,
  :no_such_sound,
  :no_such_track,
  :no_such_widget,
  :plugin_error,
  :wrong_number_of_args,
  :out_of_range,
  :wrong_type_arg]

def rb_error_to_mus_tag
  case $!.inspect
    # case 1
    # #<StandardError: No_such_file: file->array /baddy/hiho No such file or directory
    # case 2
    # #<StandardError: insert_region: No_such_region: 1004>
    # case 3 (snd_error)
    # #<StandardError: mus_ycoeff__invalid_index_123__order___3?: Mus_error>
  when /^#<StandardError/
    err = $!.inspect.downcase.split(/:/)[1, 2].map do |e| e.strip.chomp(">") end
    Snd_error_tags.detect do |tag| tag.to_s == err[0] or tag.to_s == err[1] end
  when /^#<RangeError/
    :out_of_range
  when /^#<TypeError/
    :wrong_type_arg
  else
    nil
  end
end

def rb_error_to_message_array
  # case 2 from rb_error_to_mus_tag above
  # "#<RangeError: make-all-pass: arg 6, -1, out of range: size ~A < 0?\n>"
  err = *$!.inspect.split(/:/)[1..-1].map do |e| e.strip.chomp("\n>").sub("~A", "%s") end
  if err.kind_of?(Array) and err.length > 2 and (str = err.delete_at(1).split(/,/))[1]
    err.push(str[1].strip)
    # ["make-all-pass", "size ~A < 0?", "-1"]
  end
  err
end

# if something goes wrong
# returns [:tag_name, message(-array)] or [retval] if not nil

def snd_catch(tag = :all, retval = nil, &body)
  old_debug = $DEBUG
  $DEBUG = false
  ret = catch(tag) do body.call end
  # catch/throw
  if ret.kind_of?(Array) and ret[0] == :snd_throw
    if retval
      [retval]
    else
      ret[1..-1]
    end
  else
    [ret]
  end
rescue StandardError, RangeError, TypeError
  # rescue
  if tag == (mus_tag = rb_error_to_mus_tag) and retval 
    [retval]
  else
    [mus_tag, rb_error_to_message_array].flatten
  end
ensure
  $DEBUG = old_debug
end

def snd_throw(tag, *args)
  throw(tag, [:snd_throw, tag, get_func_name(2), *args])
end

def snd_raise(tag, *args)
  str = format("%s: %s:", get_func_name(2), tag.to_s.capitalize)
  args.each do |s| str += format(" %s", s.inspect) end
  exception = case tag
              when :out_of_range
                RangeError
              when :wrong_type_arg
                TypeError
              else
                StandardError
              end
  raise(exception, str)
end

# for irb
def c_g?
  false
end unless defined? c_g?

#
# let(*args) do |*args| ... end
# a local environment
#
# let(8, :foo, "bar") do |a, b, c|
#   printf("a: %d, b: %s, c: %s\n", a, b, c)
# end

def let(*args, &body)
  body.call(*args)
rescue
  error("%s: args: %s", get_func_name, args.inspect)
end

# general purpose loop

add_help(:gloop,
         "gloop(*args) { |args| ... }
        :step,    1
        :before,  nil (thunk)
        :after,   nil (thunk)

args[0]: Range    (each)
         Hash(s)  (each)
         Array(s) (each_with_index) [args.last == Fixnum --> step]
         Fixnum   (times)
         Fixnum   [args[1] == Fixnum --> step]
A general purpose loop, handling Range, Hash, Array or Arrays, both
with optional step, Fixnum, also with optional step, and a simple body
call, all with its own local variable scope.  Returns the result of
body as array like map.
Examples:
  Range
    gloop(0..3) do |i| puts i end
  Hash               (loops over all Hashs consecutively)
    gloop({1, :a, 2, :b}, {11, :aa, 22, :bb}) do |k, v|
      print('key: ', k, ' value: ', v)
      puts
    end
  Array
    gloop([0, 1]) do |x, i|
      print(i, ': ', x)
      puts end
  Arrays with step   (mixes all Arrays)
    gloop([0, 1, 2, 3], [:a, :b, :c, :d], [55, 66, 77, 88, 99], 2) do |x, i|
      print(i, ': ', x.inspect)
      puts
    end
  Fixnum (like times)
    gloop(3) do |i| puts i end
  Fixnum with step (like step)
    gloop(6, 2) do |i| puts i end
  a simple body call
    gloop do puts 'empty' end")
def gloop(*args, &body)
  step   = get_shift_args(args, :step, 1)
  before = get_shift_args(args, :before, nil)
  after  = get_shift_args(args, :after, nil)
  do_extra = lambda do |thunk| thunk.kind_of?(Proc) ? thunk.call : send(thunk) end
  result = []
  case args[0]
  when Range
    args[0].each do |i|
      do_extra.call(before) if before
      result << body.call(i)
      do_extra.call(after) if after
    end
  when Array
    lmax = args.map do |x| x.length end.max
    0.step(lmax - 1, step) do |i|
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
  when Fixnum
    0.step(args[0], step) do |i|
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

# get_args(args, key, val)
#
# returns value, whether default VAL or value of KEY found in ARGS

def get_args(args, key, default)
  if(key == :help and (args == key or args.member?(key) or args.assoc(key)))
    default = true
  elsif(args.member?(key))
    x = args[args.index(key) + 1]
    default = ((x == nil) ? default : x)
  elsif(args.assoc(key))
    default = (args.assoc(key)[1] rescue default)
  end
  default
end

def get_shift_args(args, key, default)
  default = get_args(args, key, default)
  if args.member?(key)
    i = args.index(key)
    2.times do args.delete_at(i) end
  end
  default
end

def get_class_args(args, klass, default)
  if (arg = args.detect do |x| x.kind_of?(klass) end)
    arg
  else
    default
  end
end

# var = get_class_or_key(args, Klass, :key, default)

def get_class_or_key(args, klass, key, default)
  if args.first.kind_of?(klass)
    args.shift
  else
    get_shift_args(args, key, default)
  end
end

# var1, var2, var3 = Args(args, [Klass, :key, default],
#                               [Numeric, :number, 1],
#                               [Array, :list, [0, 1, 2, 3]])

$strict_args = $DEBUG unless defined? $strict_args

def Args(args, *rest)
  result = rest.map do |keys| get_class_or_key(args, *keys) end
  unless args.empty?
    if $strict_args
      error("rest args: %s", args.inspect)
    else
      warn("rest args ignored: %s", args.inspect)
    end
  end
  result
end

add_help(:load_init_file,
         "load_init_file(file) \
Returns false if file doesn't exist, otherwise loads it. \
File may reside in current working dir or in $HOME dir.")
def load_init_file(file)
  if File.exist?(file)
    load(file)
  elsif File.exist?(f = ENV["HOME"] + "/" + file)
    load(f)
  else
    false
  end
end

module Examp
  # (ext)snd.html examples made harder to break
  #
  # this mainly involves keeping track of the current sound/channel

  add_help(:selection_rms, "selection_rms() -> rms of selection data using sample readers")
  def selection_rms
    if selection?
      reader = make_sample_reader(selection_position)
      len = selection_frames
      sum = 0.0
      len.times do
        val = next_sample(reader)
        sum += val * val
      end
      free_sample_reader(reader)
      sqrt(sum / len)
    else
      snd_raise(:no_active_selection)
    end
  end

  add_help(:region_rms, "region_rms([n=0]) -> rms of region n's data (chan 0)")
  def region_rms(n = 0)
    if region?(n)
      data = region2vct(0, 0, n)
      sqrt(dot_product(data, data) / data.length)
    else
      snd_raise(:no_such_region)
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
           "display_energy([snd=false, [chn=false]]) \
is a $lisp_graph_hook function to display the time domain data as energy (squared).
$lisp_graph_hook.add_hook!(\"energy\") do |snd, chn|
  display_energy(snd, chn)
end")
  def display_energy(snd = false, chn = false)
    ls = left_sample(snd, chn)
    rs = right_sample(snd, chn)
    datal = make_graph_data(snd, chn)
    data = vct?(datal) ? datal : datal[1]
    sr = srate(snd)
    y_max = y_zoom_slider(snd, chn)
    if data and ls and rs
      vct_multiply!(data, data)
      graph(data, "energy", ls / sr, rs / sr, 0.0, y_max * y_max, snd, chn, false)
    end
  end

  add_help(:display_db,
           "display_db([snd=false, [chn=false]]) \
is a lisp-graph-hook function to display the time domain data in dB.
$lisp_graph_hook.add_hook!(\"db\") do |snd, chn|
  display_db(snd, chn)
end")
  def display_db(snd = false, chn = false)
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

  # Buffers Menu
  #
  # patterned after the XEmacs Buffers menu
  # see effects.rb for a much fancier example

  $buffer_names = [] unless defined? $buffer_names
  $buffer_menu = nil unless defined? $buffer_menu

  add_help(:open_buffer,
           "open_buffer(filename) adds a menu item that will select filename (use with $open_hook)
$open_hook.add_hook!(\"buffers\") do |file|
  open_buffer(file)
end")
  def open_buffer(file)
    $buffer_menu ||= add_to_main_menu("Buffers", lambda do | | end)
    add_to_menu($buffer_menu, file, lambda do | | select_sound(find_sound(file)) end)
    $buffer_names.push(file)
    if provided? "snd-xm.rb"
      if provided? "xm"
        set_label_sensitive(menu_widgets[0], "Buffers", true)
      else
        set_sensitive(main_menu($buffer_menu), true)
      end
    end
    false
  end

  add_help(:close_buffer,
           "close_buffer(snd) removes the menu item associated with snd (use with $close_hook)
$close_hook.add_hook!(\"buffers\") do |snd|
  close_buffer(snd)
end")
  def close_buffer(snd)
    remove_from_menu($buffer_menu, file_name(snd))
    $buffer_names.delete(file_name(snd))
    if provided? "snd-xm.rb" and $buffer_menu
      if provided? "xm"
        set_label_sensitive(menu_widgets[0], "Buffers", !$buffer_names.empty?)
      else
        set_sensitive(main_menu($buffer_menu), !$buffer_names.empty?)
      end
    end
    false
  end

  # Reopen Menu
  #
  # a similar idea, here presenting the last-closed sounds
  # this can be used in conjunction with remember-sound-state in extensions.scm

  $reopen_names = [] unless defined? $reopen_names
  $reopen_menu = nil unless defined? $reopen_menu

  add_help(:add_to_reopen_menu,
           "add_to_reopen_menu(snd) adds snd to the Reopen menu (use with $close_hook)
$close_hook.add_hook!(\"reopen\") do |snd|
  add_to_reopen_menu(snd)
end")
  def add_to_reopen_menu(snd)
    $reopen_menu ||= add_to_main_menu("Reopen", lambda do | | end)
    brief_name = short_file_name(snd)
    long_name = file_name(snd)
    unless($reopen_names.member?(brief_name))
      add_to_menu($reopen_menu, brief_name,
                  lambda do | |
                    remove_from_menu($reopen_menu, brief_name)
                    if File.exist?(long_name)
                      open_sound(long_name)
                    end
                  end, 0)
      $reopen_names.push(brief_name)
      if $reopen_names.length > 8
        remove_from_menu($reopen_menu, $reopen_names.shift)
      end
      if provided? "snd-xm.rb"
        if provided? "xm"
          set_label_sensitive(menu_widgets[0], "Reopen", true)
        else
          set_sensitive(main_menu($reopen_menu), true)
        end
      end
    end
    false
  end

  add_help(:check_reopen_menu,
           "check_reopen_menu(filename) \
removes filename from the Reopen menu list (use with $open_hook)
$open_hook.add_hook!(\"reopen\") do |file|
  check_reopen_menu(file)
end")
  def check_reopen_menu(file)
    brief_name = File.basename(file)
    if $reopen_names.member?(brief_name)
      remove_from_menu($reopen_menu, brief_name)
      $reopen_names.delete(brief_name)
    end
    if provided? "snd-xm.rb" and $reopen_menu
      if provided? "xm"
        set_label_sensitive(menu_widgets[0], "Reopen", !$reopen_names.empty?)
      else
        set_sensitive(main_menu($reopen_menu), !$reopen_names.empty?)
      end
    end
    false
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
    maxsync = sounds2array.map do |s| sync(s) end.max
    if sync(snd) > 0 and
        snd == sounds2array.map do |s| sync(snd) == sync(s) ? s : maxsync + 1 end.min
    end
    ls = left_sample(snd, chn)
    rs = right_sample(snd, chn)
    pow2 = (log(rs - ls) / log(2)).ceil
    fftlen = (2 ** pow2).to_i
    if pow2 > 2
      ffts = []
      sounds2array.each do |s|
        if sync(snd) == sync(s) and chans(s) > chn
          fdr = channel2vct(ls, fftlen, s, chn)
          fdi = make_vct(fftlen)
          spectr = make_vct(fftlen / 2)
          ffts.push(vct_add!(spectr, spectrum(fdr, fdi, false, 2)))
        end
      end
      graph(ffts, "spectra", 0.0, 0.5, false, false, snd, chn)
    end
  end

  # c-g? example (Anders Vinjar)

  add_help(:locate_zero,
           "locate_zero(limit) \
looks for successive samples that sum to less than 'limit', moving the cursor if successful")
  def locate_zero(limit)
    start = cursor
    sf = make_sample_reader(start)
    val0 = sf.call.abs
    val1 = sf.call.abs
    until sample_reader_at_end?(sf) or c_g? or val0 + val1 < limit
      val0, val1 = val1, sf.call.abs
    end
    free_sample_reader(sf)
    set_cursor(n)
  end
  
  # make a system call from the listener
  # 
  #   shell("df") for example
  # or to play a sound whenever a file is closed:
  #   $close-hook.add_hook!() do |snd| shell("sndplay wood16.wav"); false end

  add_help(:shell, "shell(*cmd) \
sends 'cmd' to a shell (executes it as a shell command) and returns the result.")
  def shell(*cmd)
    str = ""
    File.popen(format(*cmd)) do |f| str << f.gets until f.eof end
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
      message("%s is not an MPEG file (first 11 bytes: %b %b", mpgfile.inspect, b0, b1 & 0b11100000)
    else
      id = (b1 & 0b11000) >> 3
      layer = (b1 & 0b110) >> 1
      srate_index = (b2 & 0b1100) >> 2
      channel_mode = (b3 & 0b11000000) >> 6
      if id == 1
        message("odd: %s is using a reserved Version ID", mpgfile.inspect)
      end
      if layer == 0
        message("odd: %s is using a reserved layer description", mpgfile.inspect)
      end
      chans = channel_mode == 3 ? 1 : 2
      mpegnum = id.zero? ? 4 : (id == 2 ? 2 : 1)
      mpeg_layer = layer == 3 ? 1 : (layer == 2 ? 2 : 3)
      srate = [44100, 48000, 32000, 0][srate_index] / mpegnum
      message("%s: %s Hz, %s, MPEG-%s",
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
      File.unlink(aufile) if File.exists?(aufile)
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
    File.unlink(wavfile) if File.exists?(wavfile)
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
    keysnd = snd_snd
    keychn = snd_chn
    current_left_sample = left_sample(keysnd, keychn)
    chan_marks = marks(keysnd, keychn)
    if chan_marks.zero?
      report_in_minibuffer("no marks")
    else
      leftmost = chan_marks.map do |m| mark_sample(m) end.detect do |m| m > current_left_sample end
      if leftmost
        set_left_sample(leftmost, keysnd, keychn)
        Keyboard_no_action
      else
        report_in_minibuffer("no mark in window")
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
      message("%s has no loop info", short_file_name.inspect)
    end
  end

  # mapping extensions (map arbitrary single-channel function over
  # various channel collections)

  add_help(:do_all_chans,
           "do_all_chans(edhist, &func) \
applies func to all active channels, using edhist as the edit history indication:
do_all_chans(\"double all samples\", do |val| 2.0 * val end)")
  def do_all_chans(origin, &func)
    sounds2array.each do |snd|
      channels(snd).times do |chn|
        map_channel(func, 0, false, snd, chn, false, origin)
      end
    end
  end

  add_help(:update_graphs, "update_graphs() updates (redraws) all graphs")
  def update_graphs
    sounds2array.each do |snd|
      channels(snd).times do |chn|
        update_time_graph(snd, chn)
      end
    end
  end

  add_help(:do_chans,
           "do_chans(edhist, &func) \
applies func to all sync'd channels using edhist as the edit history indication")
  def do_chans(*origin, &func)
    snc = sync
    if snc > 0
      sounds2array.each do |snd|
        channels(snd).times do |chn|
          if sync(snd) == snc
            map_channel(func, 0, false, snd, chn, false, format(*origin))
          end
        end
      end
    else
      snd_warning("sync not set")
    end
  end

  add_help(:do_sound_chans,
           "do_sound_chans(edhist, &func) \
applies func to all selected channels using edhist as the edit history indication")
  def do_sound_chans(*origin, &func)
    if snd = selected_sound
      channels(snd).times do |chn| map_channel(func, 0, false, snd, chn, false, format(*origin)) end
    else
      snd_warning("no selected sound")
    end
  end

  add_help(:every_sample?,
           "every_sample(&func) \
-> true if func is not false for all samples in the current channel, \
otherwise it moves the cursor to the first offending sample")
  def every_sample?(&func)
    if baddy = scan_channel(lambda do |y| (not func.call(y)) end)
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
    if pan_env.kind_of?(Numeric)
      pos = pan_env / 90.0
      rd0 = make_sample_reader(0, mono_snd)
      rd1 = make_sample_reader(0, mono_snd)
      map_channel(lambda do |y| y + pos * read_sample(rd1) end, 0, len, stereo_snd, 1)
      map_channel(lambda do |y| y + (1.0 - pos) * read_sample(rd0) end, 0, len, stereo_snd, 0)
    else
      e0 = make_env(:envelope, pan_env, :end, len - 1)
      e1 = make_env(:envelope, pan_env, :end, len - 1)
      rd0 = make_sample_reader(0, mono_snd)
      rd1 = make_sample_reader(0, mono_snd)
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
    vct2channel(rdata, 0, len - 1, snd, chn, false, format("fft_edit %s %s", bottom, top))
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
    vct2channel(rdata, 0, len - 1, snd, chn, false, format("fft_squelch %s", squelch))
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
    vct2channel(rdata, 0, len - 1, snd, chn, false, format("fft_cancel %s %s", lo_freq, hi_freq))
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
                end, 0, false, snd, chn, false, "squelch_vowels")
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
                format("%s %s", get_func_name, fft_env.inspect))
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
                format("%s %s %s %s", get_func_name, env1.inspect, env2.inspect, interp.inspect))
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
set up any number of independently oscillating formants: \
map_channel(osc_formants(0.99, [400, 800, 1200], [400, 800, 1200], [4, 2, 3]))")
  def osc_formants(radius, bases, amounts, freqs)
    len = bases.length
    frms = make_array(len) do |i| make_formant(radius, bases[i]) end
    oscs = make_array(len) do |i| make_oscil(freqs[i]) end
    lambda do |x|
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
    srate = (srate() rescue $rbm_srate)
    dur = (len / srate).round
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
                0, len, snd, chn, false, format("%s %f %f", get_func_name, freq, amp))
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
                format("%s %f %f %f", get_func_name, sr, osamp, osfreq))
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
                  format("%s %s %s", get_func_name, beg, dur))
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
            granulate(gr, lambda do |dir|
                        val = v[inctr]
                        inctr += dir
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
    vct2channel(out_data, 0, len, snd, chn, false, format("%s %s", get_func_name, gr_env.inspect))
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
                  format("%s %s %s %s %s", get_func_name, amp, fftsize, r, tempo))
    end
  end

  # convolution example

  add_help(:cnvtest,
           "cnvtest(snd0, snd1, amp) \
convolves snd0 and snd1, scaling by amp, returns new max amp: cnvtest(0, 1, 0.1)")
  def cnvtest(snd0, snd1, amp)
    flt_len = frames(snd0)
    total_len = flt_len + frames(snd1)
    cnv = make_convolve(:filter, channel2vct(0, flt_len, snd0))
    sf = make_sample_reader(0, snd1)
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
        sounds2array.each do |snd|
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
          swap_channel(snd_chn0[0], snd_chn0[1], snd_chn1[0], snd_chn1[1], beg, len)
        else
          snd_raise(:wrong_number_of_channels, "needs two channels to swap")
        end
      else
        snd_raise(:wrong_number_of_channels, "needs a stereo selection")
      end
    else
      snd_raise(:no_active_selection)
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
        curbeg = buf4size + (loc - bufsize)
        curend = curbeg + bufsize
        data = channel2vct(curbeg, bufsize, snd, chn)
      else
        if loc > curend
          curbeg = loc - buf4size
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
      mus_sound_write(fil, 0, data-ctr - 1, 1, data)
    end
    mus_sound_close_output(fil, 4 * newlen)
    set_samples(0, newlen, tempfilename, snd, chn, false,
                format("%s %s %s", get_func_name, envelope.inspect, time_scale))
    File.unlink(tempfilename)
  end

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
                end, 0, false, snd, chn, false, format("%s %s", get_func_name, en.inspect))
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
      curr_buffer = snd_snd
      width, height= widget_size(sound_widgets(curr_buffer)[0])
      sounds2array.each do |s| hide_widget(sound_widgets(s)[0]) end
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
                             debug(response)
                             callcc do |give_up|
                               if (not response.kind_of?(String)) or response.empty?
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
        @last_buffer = sounds2array.detect do |s|
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
      sounds2array.each do |s| hide_widget(sound_widgets(s)[0]) end
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
    reader = make_sample_reader(loc)
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
    samps = make_vct(10)
    sctr = 0
    lambda do |val|
      samp0, samp1, samp2 = samp1, samp2, val
      samps[sctr] = val
      sctr += 1
      if sctr >= 10
        sctr 0
      end
      local_max = [0.1, vct_peak(samps)].max
      if ((samp0 - samp1).abs > local_max) and
          ((samp1 - samp2).abs > local_max) and
          ((samp0 - samp2).abs < (local_max / 2))
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
      rtn = last0.kind_of?(Numeric) and
        ((last0 < last1 and last1 > n) or (last0 > last1 and last1 < n)) and
        -1
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
    as_one_edit_rb("%s %s", get_func_name, notes.inspect) do
      (notes or []).each do |note|
        file, offset, amp = note
        beg = start + (srate(snd) * (offset or 0.0)).floor
        if amp and amp != 1.0
          mix_vct(vct_scale!(file2vct(file), amp), beg, snd, chn, false, get_func_name)
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
        @last_file_opened = file_name(snd_snd)
      end
      if @current_directory.empty?
        unless sounds
          get_current_files(Dir.pwd)
        else
          get_current_files(File.split(@last_file_opened).first)
        end
      end
      if @current_sorted_files.empty?
        snd_raise(:no_such_file)
      else
        next_file = find_next_file
        if find_sound(next_file)
          snd_raise(:file_already_open, next_file)
        else
          sounds and close_sound(snd_snd)
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
      message(@last_file_opened = filename)
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
      if gen.kind_of?(Array)
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
      sounds2array.each do |snd|
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
    data = vct(0.0, 0.0, init_angle, off, scale)
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
                    data[1] = incr
                    data[0] = frag_beg * incr
                    data
                  }, format("%s %s %s", get_func_name, beg, dur))
  end

  # ring-modulate-channel (ring-mod as virtual op)

  def ring_modulate_channel(freq, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda { |y, data, forward|
                    angle, incr = data
                    val = y * sin(angle)
                    if forward
                      data[0] = angle + incr
                    else
                      data[0] = angle - incr
                    end
                    val
                  }, beg, dur, snd, snd, edpos, false,
                  lambda { |frag_beg, frag_dur|
                    incr = (TWO_PI * freq) / srate(snd)
                    vct((frag_beg * incr).divmod(TWO_PI).last, incr)
                  }, format("%s %s %s", get_func_name, beg, dur))
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
    buffer = make_average(128)
    silence = silence / 128.0
    edges = []
    samp = 0
    in_silence = true
    old_max = max_regions
    old_tags = with_mix_tags
    set_max_regions(1024)
    set_with_mix_tags(false)
    scan_channel(lambda do |y|
                   if (now_silent = average(buffer, y * y) < silence) != in_silence
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
end

include Examp

module Moog
  add_help(:Moog, "#{self.class} #{self.name}

Moog style four pole lowpass filter clm unit generator low pass, 24db/Oct, \
variable resonance, warm, analog sound ;-) [all this digital wizardry \
and we're back where we started!]

original C instrument by Tim Stilson
translation into clm and tuning by 
  Fernando Lopez-Lezcano, nando@ccrma.stanford.edu
  http://ccrma.stanford.edu/~nando/clm/moog

translated to Snd scheme function by Bill
(and translated to Snd ruby function by M. Scholz)")

  class Moog_filter
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
      @frequency = freq
      @Q = q
      @state = make_vct(4)
      @A = 0.0
      @freqtable = envelope_interp(freq / (srate() * 0.5), Freqtable)
    end
    attr_reader :frequency, :state, :freqtable, :A
    attr_accessor :Q

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
  def moog_filter(moog, insig = 0.0)
    moog.filter(insig)
  end
  
  def moog(freq, q)
    mg = Moog_filter.new(freq, q)
    lambda do |inval| mg.filter(inval) end
  end
end

include Moog

# examp.rb ends here
