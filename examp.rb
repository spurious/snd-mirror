# examp.rb -- Guile -> Ruby translation -*- snd-ruby -*-

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Wed Sep 04 18:34:00 CEST 2002
# Last: Fri Mar 04 01:37:00 CET 2005

# Commentary:
#
# Extensions to Ruby:
# 
# provided?(string_or_symbol)
#
# backward compatibility methods:
#  String#to_sym, Symbol#to_sym
#  make_array(len, init) do |i| ... end
#  array?
#  Array#zip, Array#insert
#
# extensions to existing classes
# 
# class Array
#   to_pairs
#   each_pair do |x, y| ... end
#   to_string(len = 8)
#   cycle(n = 1)
#
# make_vct!(len, init) do |i| ... end
# class Vct
#   cycle(n = 1)
#
# class Integer
#  even?
#  odd?
#  prime?
#
# module Enumerable
#  map_with_index do |x, i| ... end
#  map_with_index! do |x, i| ... end
#
# with_silence(exception) do |old_verbose, old_debug| ... end
# 
# module Info
#  description=(text)
#  description
#
# thunk?(thunk)
# class Proc
#  to_method(name, klass)
#  to_str
#  to_body
#
# module Kernel
#  doc(str)
#  Kernel.doc(func)
#  putd(func)
#
# snd_putd(func)
#
# Utilities:
#
# close_sound_extend(snd)
# get_func_name(n)
# times2samples(start, dur)
# rbm_random(n)
# logn(r, b)
# car(v), cadr(v), caddr(v), cdr(v)
# warn(*args), die(*args), error(*args)
# rbm_message(*args), message(*args), debug(*args), debug_trace(*args)
# snd_snd(snd)
# snd_chn(chn)
# snd_var(name, snd, chn)
# set_snd_var(name, val, snd, chn)
# snd_catch(tag, &body)
# snd_throw(tag, *args)
# snd_raise(tag, *args)
# c_g?() (if not in Snd)
# let(*args) do |*args| ... end
# gloop(*args) do |args| ... end
# shell(*cmd)
# get_args(args, key, default)
# get_shift_args(args, key, default)
# get_class_args(args, klass, default)
# get_class_or_key(args, klass, key, default)
# Args(args, *rest)
# load_init_file(file)
#
# Buffers
# 
# open_buffer(file)
# close_buffer(snd)
# add_to_reopen_menu(snd)
# check_reopen_menu(file)
#
# FM
#
# class Instrument
#   fm_violin_rb(start, dur, freq, amp, *args)
#   jc_reverb_rb(start, dur, *args)
#
# n_rev(*args)
# hello_dentist(frq, amp)
# ring_mod(freq, gliss_env)
# am(freq)
# vibro(speed, depth)
# fp(sr, osamp, osfreq)
# compand(h)
# compand_channel(beg, dur, snd, chn, edpos)
# fft_peak(snd, chn, scale)
# cross_synthesis(cross_snd, amp, fftsize, r)
# osc_formants(radius, bases, amounts, freqs)
# echo(scaler, secs)
# zecho(scaler, secs, freq, amp)
# flecho(scaler, secs)
# comb_filter(scaler, size)
# zcomb(scaler, size, pm)
# notch_filter(scaler, size)
# formant_filter(radius, freq)
# filtered_env(e, snd, chn)
# fft_edit(bottom, top, snd, chn)
# fft_squelch(squelch, snd, chn)
# fft_cancel(lo_freq, hi_freq, snd, chn)
# ramp(gen, up)
# make_ramp(size)
# squelch_vowels(snd, chn)
# scramble_channels(*new_order)
# scramble_channel(silence)
#
# module Dsp (see dsp.scm)
#  butter(b, sig)
#  make_butter_high_pass(freq)
#  make_butter_low_pass(freq)
#  make_butter_band_pass(freq, band)
#  make_butter_band_reject(freq, band)
#  down_oct(h)
#  spike(h)
#  zero_phase(h)
#  rotate_phase(func)
#  spot_freq(samp, snd, chn)
#  make_hilbert_transform(len)
#  hilbert_transform(f, input)
#  make_lowpass(fc, len)
#  lowpass(f, input)
#  make_highpass(fc, len)
#  highpass(f, input)
#  make_bandpass(flo, fhi, len)
#  bandpass(f, input)
#  make_bandstop(flo, fhi, len)
#  bandstop(f, input)
#
# module Moog (see moog.scm)
#  make_moog_filter(freq, q)
#  moog_filter(m, sig)
#  

# Code:

##
## Extensions to Ruby
##
 
def provided?(string_or_symbol)
  $".member?(string_or_symbol.to_s)
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

# Older Ruby versions lack Array.new(10) do |i| ... end
# make_array
# make_array(10)
# make_array(10, 1.0)
# make_array(10) do |i| ... end
def make_array(len = 0, init = nil, &body)
  len = if len.kind_of?(Numeric)
          len.abs.to_i
        else
          0
        end
  ary = Array.new(len, init)
  # if block_given? then ary.map!(&body) end
  len.times do |i| ary[i] = body.call(i) end if block_given?
  ary
end

def array?(obj)
  obj.kind_of?(Array)
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
  def to_string(len = 8)
    ary = self.flatten
    nlen = [ary.length, len].min
    str = "["
    nlen.times do |i|
      str += "%1.3f" % ary[i].to_f
      str += ", " if i < nlen - 1
    end
    str += ", ..." if ary.length > len
    str += "]"
  end

  # Cycles through the array and returns the next N values.  If end of
  # array is reached, it again continues with index 0 and so on.
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
  attr_reader :cycle_index
end

def make_vct!(len, init = 0.0, &body)
  list2vct(make_array(len, init, &body))
end

class Vct
  # Cycles through the vct and returns the next N values.  If end of
  # vct is reached, it continues with index 0 and so on.
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
  attr_reader :cycle_index
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
end

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

def thunk?(thunk)
  thunk.kind_of?(Proc) and thunk.arity.zero?
end

class Proc
  include Info

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
TWO_PI = PI * 2.0
HALF_PI = PI * 0.5

require "ws"
require "env"
include Env

#
# Lisp-like documentation for Modules, Classes, and Methods.
#
# Example:
# 
# class Bar
#   doc "documentation of #{self.class} #{self.name}"
#
#   def barfoo(a = nil)
#     doc("documentation of " + get_func_name) if a == :help
#     puts "we are in " + get_func_name
#   end
# end
# 
# module Foo
#   doc "documentation of #{self.class} #{self.name}"
#
#   def foobar(*args)
#     doc("documentation of " + get_func_name) if get_args(args, :help, false)
#     puts "we are in " + get_func_name
#   end
# end
# 
# def foo(a, b = nil, c = nil)
#   doc("documentation of " + get_func_name) if a == :help
#
#   puts "we are in " + get_func_name
#   puts a + b + c
# end
# 
# putd(Foo)
# putd(Bar)
# putd(:foo)
# f = Bar.new
# f.putd(:barfoo)
# include Foo
# putd(:foobar)
# foo("a", "b", "c")
# f.barfoo
# foobar
#

module Kernel
  @@docs = Hash.new

  def doc(str)
    if self.class <= Module
      @@docs[self.name] = str
    else
      @@docs[get_func_name(2)] = str
      throw(:__Kernel_doc__)
    end
  end
  
  def Kernel.doc(func)
    @@docs[func.class <= Module ? func.name : func.to_s] || "#{func}: No documentation available"
  end

  #
  # putd(:func)
  #
  # puts documentation (for usage in an Emacs Snd session)
  # 
  
  def putd(func)
    func = func.intern if func.kind_of?(String)
    catch(:__Kernel_doc__) do send(func, :help) end unless func.class <= Module
  rescue
  ensure
    printf("%s\n", Kernel.doc(func))
  end
end unless defined? @@docs

#
# snd_putd(:func)
#
# snd put documentation (for usage in Snd)
# 

def snd_putd(func)
  func = func.intern if func.kind_of?(String)
  catch(:__Kernel_doc__) do send(func, :help) end unless func.class <= Module
rescue
ensure
  rbm_message(Kernel.doc(func))
end

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

def get_func_name(n = 1)
  doc("get_func_name([n=1])
returns function name string\n") if n == :help
  caller(n)[0].scan(/^.*:in `(.*)'/)[0][0]
end

def times2samples(start, dur = nil)
  doc("times2samples(start, dur)
START and DUR are in seconds;
returns array [beg, len] in samples\n") if start == :help
  beg = seconds2samples(start)
  [beg, beg + seconds2samples(dur)]
end

def rbm_random(n)
  mus_random(n - 0.000001).abs
end

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
  str = format(*args)
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
private :verbose_message_string

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
    snd_print("\n" + format(*args))
    nil
  else
    print(format(*args), "\n")
  end
end

# like printf(*args), prepends a comment sign
def message(*args)
  rbm_message("# %s", format(*args))
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
    # #<StandardError: No_such_file: file->array /baddy/hiho No such file or directory
    # #<StandardError: insert_region: No_such_region: 1004>
  when /^#<StandardError/
      err = $!.inspect.downcase.split(/:/)[1, 2].map do |e| e.strip end
      Snd_error_tags.detect do |tag| tag.to_s == err[0] or tag.to_s == err[1] end
  when /^#<RangeError/
      :out_of_range
  when /^#<TypeError/
      :wrong_type_arg
  else
    nil
  end
end

# returns [:tag_name, message]
def snd_catch(tag = :all, &body)
  ret = catch(tag) do body.call end
  (ret.kind_of?(Array) and ret[0] == :snd_throw) ? ret[1, 2] : [ret]
rescue
  [rb_error_to_mus_tag, ""]
end

def snd_throw(tag, *args)
  str = format("%s: %s", get_func_name(2), tag.inspect)
  args.each do |s| str += format(" %s", s.inspect) end
  throw(tag, [:snd_throw, tag, str])
end

def snd_raise(tag, *args)
  str = format("%s: %s:", tag.to_s.capitalize, get_func_name(2))
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

def gloop(*args, &body)
  doc("gloop(*args) { |args| ... }
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
    gloop do puts 'empty' end\n") if args.member?(:help)
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

def shell(*cmd)
  str = ""
  File.popen(format(*cmd)) do |f| str << f.gets until f.eof end
  str
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

def load_init_file(file)
  doc("load_init_file(file)
Returns false if file doesn't exist, otherwise loads it. File may
reside in current working dir or in $HOME dir.\n") if file == :help
  if File.exist?(file)
    load(file)
  elsif File.exist?(f = ENV["HOME"] + "/" + file)
    load(f)
  else
    false
  end
end

##
## Buffers Menu
##

# all buffers menu functions in my ~/.snd-ruby.rb
#
# $open_hook.add_hook!("snd-init-hook") do |file|
#   open_buffer(file)
#   check_reopen_menu(file)
#   false
# end
# 
# $close_hook.add_hook!("snd-init-hook") do |snd|
#   close_buffer(snd)
#   add_to_reopen_menu(snd)
#   false
# end

$buffer_names = [] unless defined? $buffer_names
$buffer_menu = nil unless defined? $buffer_menu

def open_buffer(file)
  doc("open_buffer(file)
Adds a menu item that will select filename (use with $open_hook). See
also close_buffer().
Usage in ~./snd-ruby.rb
$open_hook.add_hook!(\"my-hook\") { |file| open_buffer(file) }
$close_hook.add_hook!(\"my-hook\") { |snd| close_buffer(snd) }\n") if file == :help
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

def close_buffer(snd)
  doc("close_buffer(snd)
Removes the menu item associated with snd (use with $close_hook). See
also open_buffer().
Usage in ~./snd-ruby.rb
$open_hook.add_hook!(\"my-hook\") { |file| open_buffer(file) }
$close_hook.add_hook!(\"my-hook\") { |snd| close_buffer(snd) }\n") if snd == :help
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

##
## Reopen Menu
##

$reopen_names = [] unless defined? $reopen_names
$reopen_menu = nil unless defined? $reopen_menu

def add_to_reopen_menu(snd)
  doc("add_to_reopen_menu(snd)
Adds snd to the Reopen menu (use with $close_hook). See also
check_reopen_menu().
Usage in ~./snd-ruby.rb
$open_hook.add_hook!(\"my-hook\") { |file| check_reopen_menu(file) }
$close_hook.add_hook!(\"my-hook\") { |snd| add_to_reopen_menu(snd) }\n") if snd == :help
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

def check_reopen_menu(file)
  doc("check_reopen_menu(file)
Removes filename from the Reopen menu list (use with $open_hook). See
also add_to_reopen_menu().
Usage in ~./snd-ruby.rb
$open_hook.add_hook!(\"my-hook\") { |file| check_reopen_menu(file) }
$close_hook.add_hook!(\"my-hook\") { |snd| add_to_reopen_menu(snd) }\n") if file == :help
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

##
## FM
##

class Instrument
  def fm_violin_rb(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.5, *args)
    doc("fm_violin_rb([start=0.0[, dur=1.0[, freq=440.0[, amp=0.5[, *args]]]]])
	:fm_index,              1.0
	:amp_env,               [0, 0, 25, 1, 75, 1, 100, 0]
	:periodic_vibrato_rate, 5.0
	:random_vibrato_rate,   16.0
	:periodic_vibrato_amp,  0.0025
	:random_vibrato_amp,    0.005
	:noise_amount,          0.0
	:noise_freq,            1000.0
	:ind_noise_freq,        10.0
	:ind_noise_amount,      0.0
	:amp_noise_freq,        20.0
	:amp_noise_amount,      0.0
	:gliss_env,             [0, 0, 100, 0]
	:gliss_amount,          0.0
	:fm1_env,               [0, 1, 25, 0.4, 75, 0.6, 100, 0]
	:fm2_env,               [0, 1, 25, 0.4, 75, 0.6, 100, 0]
	:fm3_env,               [0, 1, 25, 0.4, 75, 0.6, 100, 0]
	:fm1_rat,               1.0
	:fm2_rat,               3.0
	:fm3_rat,               4.0
	:fm1_index,             false
	:fm2_index,             false
	:fm3_index,             false
	:base,                  1.0
	:index_type,            :violin
	:reverb_amount,         0.01
	:degree,                kernel_rand(90.0)
	:distance,              1.0
   Ruby: fm_violin_rb(0, 1, 440, 0.1, :fm_index, 2.0)
  Guile: (fm-violin 0 1 440 0.1 :fm-index 2.0)
Example: with_sound do fm_violin_rb(0, 1, 440, 0.1, :fm_index, 2.0) end\n") if start == :help
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
    index_type            = get_args(args, :index_type, :violin)
    rev_amount            = get_args(args, :reverb_amount, 0.01)
    degree                = get_args(args, :degree, kernel_rand(90.0))
    distance              = get_args(args, :distance, 1.0)
    frq_scl = hz2radians(freq)
    modulate = fm_index.nonzero?
    maxdev = frq_scl * fm_index
    vln = (index_type != :cello)
    logfreq = log(freq)
    sqrtfreq = sqrt(freq)
    index1 = (fm1_index or [PI, maxdev * (vln ? 5.0 : 7.5) / logfreq].min)
    index2 = (fm2_index or [PI, maxdev * 3.0 * \
                (vln ? ((8.5 - logfreq) / (3.0 + freq * 0.001)) : (15.0 / sqrtfreq))].min)
    index3 = (fm3_index or [PI, maxdev * (vln ? 4.0 : 8.0) / sqrtfreq].min)
    easy_case = (noise_amount.zero? and
                   (fm1_env == fm2_env) and 
                   (fm1_env == fm3_env) and 
                   (fm1_rat - fm1_rat.floor).zero? and 
                   (fm2_rat - fm2_rat.floor).zero? and 
                   (fm3_rat - fm3_rat.floor).zero?)
    coeffs = (easy_case and modulate and 
                partials2polynomial([fm1_rat.floor, index1, 
                                     (fm2_rat / fm1_rat).floor, index2,
                                     (fm3_rat / fm1_rat).floor, index3]))
    norm = ((easy_case and modulate and 1.0) or index1)
    carrier = make_oscil(freq)
    fmosc1 = (modulate and make_oscil(fm1_rat * freq))
    fmosc2 = (modulate and (easy_case or make_oscil(fm2_rat * freq)))
    fmosc3 = (modulate and (easy_case or make_oscil(fm3_rat * freq)))
    ampf = make_env(amp_env, amp, dur, 0.0, base)
    indf1 = (modulate and make_env(fm1_env, norm, dur))
    indf2 = (modulate and (easy_case or make_env(fm2_env, index2, dur)))
    indf3 = (modulate and (easy_case or make_env(fm3_env, index3, dur)))
    frqf = make_env(gliss_env, gliss_amount * frq_scl, dur)
    pervib = make_triangle_wave(periodic_vibrato_rate, periodic_vibrato_amp *  frq_scl)
    ranvib = make_rand_interp(random_vibrato_rate, random_vibrato_amp * frq_scl)
    fm_noi = (noise_amount.nonzero? and make_rand(noise_freq, PI * noise_amount))
    ind_noi = ((ind_noise_amount.nonzero? and ind_noise_freq.nonzero?) and 
                 make_rand_interp(ind_noise_freq, ind_noise_amount))
    amp_noi = ((amp_noise_amount.nonzero? and amp_noise_freq.nonzero?) and
                 make_rand_interp(amp_noise_freq, amp_noise_amount))
    fuzz = 0.0
    ind_fuzz = amp_fuzz = 1.0
    run_instrument(start, dur, :reverb_amount, rev_amount, :degree, degree, :distance, distance) do
      fuzz = rand(fm_noi) if noise_amount.nonzero?
      vib = env(frqf) + triangle_wave(pervib) + rand_interp(ranvib)
      ind_fuzz = 1.0 + rand_interp(ind_noi) if ind_noi
      amp_fuzz = 1.0 + rand_interp(amp_noi) if amp_noi
      if modulate
        if easy_case
          modulation = env(indf1) * polynomial(coeffs, oscil(fmosc1, vib))
        else
          modulation = (env(indf1) * oscil(fmosc1, fm1_rat * vib + fuzz) + \
                        env(indf2) * oscil(fmosc2, fm2_rat * vib + fuzz) + \
                        env(indf3) * oscil(fmosc3, fm3_rat * vib + fuzz))
        end
      end
      env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz * modulation)
    end
  end
  
  def jc_reverb_rb(start, dur, *args)
    low_pass = get_args(args, :low_pass, false)
    volume   = get_args(args, :volume, 1.0)
    amp_env  = get_args(args, :amp_env, false)
    delay1   = get_args(args, :delay1, 0.013)
    delay2   = get_args(args, :delay2, 0.011)
    delay3   = get_args(args, :delay3, 0.015)
    delay4   = get_args(args, :delay4, 0.017)
    double   = get_args(args, :double, false)
    chan2 = (@channels > 1)
    chan4 = (@channels == 4)
    if double and chan4
      error("%s: not set up for doubled reverb in quad", get_func_name)
    end
    allpass1 = make_all_pass(-0.7, 0.7, 1051)
    allpass2 = make_all_pass(-0.7, 0.7,  337)
    allpass3 = make_all_pass(-0.7, 0.7,  113)
    comb1 = make_comb(0.742, 4799)
    comb2 = make_comb(0.733, 4999)
    comb3 = make_comb(0.715, 5399)
    comb4 = make_comb(0.697, 5801)
    outdel1 = make_delay((delay1 * @srate).round)
    outdel2 = (chan2 ? make_delay((delay2 * @srate).round) : false)
    outdel3 = ((chan4 or double) ? make_delay((delay3 * @srate).round) : false)
    outdel4 = ((chan4 or (double and chan2)) ? make_delay((delay4 * @srate).round) : false)
    envA = (amp_env ? make_env(:envelope, amp_env, :scaler, volume, :duration, dur) : false)
    comb_sum_1 = comb_sum = 0.0
    reverb_frame = make_frame(@channels)
    run_reverb(start, dur) do |ho, i|
      allpass_sum = all_pass(allpass3, all_pass(allpass2, all_pass(allpass1, ho)))
      comb_sum_2, comb_sum_1 = comb_sum_1, comb_sum
      comb_sum = (comb(comb1, allpass_sum) + comb(comb2, allpass_sum) + \
                  comb(comb3, allpass_sum) + comb(comb4, allpass_sum))
      all_sums = if low_pass
                   0.25 * (comb_sum + comb_sum_2) + 0.5 * comb_sum_1
                 else
                   comb_sum
                 end
      delA = delay(outdel1, all_sums)
      if double
        delA += delay(outdel3, all_sums)
      end
      if envA
        volume = env(envA)
      end
      frame_set!(reverb_frame, 0, volume * delA)
      if chan2
        delB = delay(outdel2, all_sums)
        if double
          delB += delay(outdel4, all_sums)
        end
        frame_set!(reverb_frame, 1, volume * delB)
        if chan4
          frame_set!(reverb_frame, 2, volume * delay(outdel3, all_sums))
          frame_set!(reverb_frame, 3, volume * delay(outdel4, all_sums))
        end
      end
      reverb_frame
    end
  end
end

class Snd_Instrument
  alias fm_violin fm_violin_rb
  alias jc_reverb jc_reverb_rb
end
  
def n_rev(*args)
  doc("n_rev(*args)
	:amount,   0.1
	:filter,   0.5
	:feedback, 1.09
Reverb from Michael McNabb's Nrev (see new-effects.scm).
Usage: n_rev([:amount, 0.2, :filter, 0.8])\n") if get_args(args, :help, false)
  amount   = get_args(args, :amount, 0.1)
  filter   = get_args(args, :filter, 0.5)
  feedback = get_args(args, :feedback, 1.09)
  set_reverb_control?(true)
  set_reverb_control_scale(amount)
  set_reverb_control_lowpass(filter)
  set_reverb_control_feedback(feedback)
  apply_controls(selected_sound(), 0)
  restore_controls()
end

def hello_dentist(freq = 40.0, amp = 0.1)
  doc("hello_dentist([freq=40.0[, amp=0.1]])
Varies the sampling rate randomly, making a voice sound quavery (see
examp.scm).
Usage: hello_dentist(40.0, 0.1)\n") if freq == :help
  rn = make_rand_interp(freq, amp)
  i = 0
  len = frames()
  in_data = channel2vct(0, len)
  out_len = (len * (1.0 + 2 * amp)).round
  out_data = make_vct(out_len)
  # make_src(input, srate=1.0, width=5)
  rd = make_src(lambda do |dir|
		  val = i.between?(0, len - 1) ? in_data[i] : 0.0
		  i += dir
		  val
		end)
  vct2channel(vct_map!(out_data, lambda do | | src(rd, rand_interp(rn)) end))
end

def ring_mod(freq = 10, gliss_env = [0, 0, 1, hz2radians(100)])
  doc("ring_mod([freq=10.0[, gliss_env=[0, 0, 1, hz2radians(100)]]])
Returns a time-varying ring-modulation filter (see examp.scm).
Usage: map_chan(ring_mod(10, [0, 0, 1, hz2radians(100)]))\n") if freq == :help
  os = make_oscil(freq)
  len = frames()
  srate = (srate() rescue $rbm_srate)
  dur = (len / srate).round
  genv = make_env(gliss_env, :end, len)
  lambda do |i| oscil(os, env(genv)) * i end
end

def am(freq = 440.0)
  doc("am([freq=440.0])
Returns an amplitude-modulator (see examp.scm).
Usage: map_chan(am(440.0))\n") if freq == :help
  os = make_oscil(freq)
  lambda do |i| amplitude_modulate(1.0, i, oscil(os)) end
end

def vibro(speed = 20, depth = 0.5)
  doc("vibro([speed=20[, depth=0.5]])
This is taken from sox (vibro.c) (see examp.scm).
Usage: map_chan(vibro(20, 0.5))\n") if speed == :help
  sine = make_oscil(speed)
  scl = 0.5 * depth
  offset = 1.0 - scl
  lambda do |i| i * (offset + scl * oscil(sine)) end
end

def fp(sr = 1.0, osamp = 0.3, osfreq = 20)
  doc("fp([sr=1.0[, osamp=0.3[, osfreq=20]]])
Varies the sampling rate via an oscil (see examp.scm).
Usage: fp(1.0, 0.3, 20)\n") if sr == :help
  os = make_oscil(osfreq)
  s = make_src(:srate, sr)
  len = frames()
  sf = make_sample_reader()
  out_data = make_vct(len)
  vct_map!(out_data, lambda do | |
	     src(s, osamp * oscil(os), lambda do |dir|
		   dir > 0 ? next_sample(sf) : previous_sample(sf)
		 end)
	   end)
  free_sample_reader(sf)
  vct2channel(out_data, 0, len)
end

def compand(doc = false)
  doc("compand()
Since there's no state in this function, it can be used without change
in any of the mapping functions (unlike echo, for example) (see
examp.scm).
Usage: map_chan(compand())\n") if doc == :help
  tbl = vct(-1.000, -0.960, -0.900, -0.820, -0.720, -0.600, -0.450, -0.250, 
	    0.000, 0.250, 0.450, 0.600, 0.720, 0.820, 0.900, 0.960, 1.000)
  lambda do |inval| array_interp(tbl, 8.0 + 8.0 * inval, 17) end
end

def compand_channel(beg = 0, dur = false, snd = false, chn = false, edpos = false)
  tbl = vct(-1.000, -0.960, -0.900, -0.820, -0.720, -0.600, -0.450, -0.250, 
	    0.000, 0.250, 0.450, 0.600, 0.720, 0.820, 0.900, 0.960, 1.000)
  ptree_channel(lambda do |inval| array_interp(tbl, 8.0 + 8.0 * inval, 17) end,
                beg, dur, snd, chn, edpos, true, false, format("compand_channel %s %s", beg, dur))
end

def fft_peak(snd, chn, scale)
  if transform_graph? and transform_graph_type == Graph_once
    report_in_minibuffer(((2.0 * vct_peak(transform2vct(snd, chn))) / transform_size).to_s)
    snd
  else
    false
  end
end

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

def echo(scaler, secs)
  del = make_delay((secs * srate()).round)
  lambda do |inval|
    inval + delay(del, scaler * (tap(del) + inval))
  end
end

def zecho(scaler, secs, freq, amp)
  os = make_oscil(freq)
  len = (secs * srate()).round
  del = make_delay(len, "max-size".intern, (len + amp + 1).to_i)
  lambda do |inval|
    inval + delay(del, scaler * (tap(del) + inval), amp * oscil(os))
  end
end

def flecho(scaler, secs)
  flt = make_fir_filter(:order, 4, :xcoeffs, vct(0.125, 0.25, 0.25, 0.125))
  del = make_delay((secs * srate()).round)
  lambda do |inval|
    inval + delay(del, fir_filter(flt, scaler * (tap(del) + inval)))
  end
end

def comb_filter(scaler, size)
  cmb = make_comb(scaler, size)
  lambda do |inval| comb(cmb, inval) end
end

def zcomb(scaler, size, pm)
  max_envelope_1 = lambda do |en, mx|
    1.step(en.length - 1, 2) do |i| mx = [mx, en[i]].max.to_f end
    mx
  end
  cmb = make_comb(scaler, size, "max-size".intern, (max_envelope_1.call(pm, 0.0) + size + 1).to_i)
  penv = make_env(:envelope, pm, :end, frames)
  lambda do |inval| comb(cmb, inval, env(penv)) end
end

def notch_filter(scaler, size)
  gen = make_notch(scaler, size)
  lambda do |inval| notch(gen, inval) end
end

def formant_filter(radius, freq)
  frm = make_formant(radius, freq)
  lambda do |inval| formant(frm, inval) end
end

def filtered_env(e, snd = false, chn = false)
  flt = make_one_pole(1.0, 0.0)
  amp_env = make_env(:envelope, e, :end, frames - 1)
  map_channel(lambda do |val|
                env_val = env(amp_env)
                set_mus_xcoeff(flt, 0, env_val)
                set_mus_ycoeff(flt, 1, env_val - 1.0)
                one_pole(flt, env_val * val)
              end, 0, false, snd, chn, false, format("filtered_env %s", e.inspect))
end

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

def ramp(gen, up)
  ctr, size = gen[0, 2]
  val = ctr / size
  gen[0] = [size, [0, ctr + (up ? 1 : -1)].max].min
  val
end

def make_ramp(size = 128)
  [0, size]
end

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
                  this = rbm_random(len)
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

module Dsp
  doc "#{self.class} #{self.name} contains some definitions of dsp.scm\n"

  def butter(b, sig = nil)
    doc("butter(b, sig)
is the generator side for the various make-butter procedure\n") if b == :help
    filter(b, sig)
  end

  def make_butter_high_pass(freq)
    doc("make_butter_high_pass(freq)
makes a Butterworth filter with high pass cutoff at FREQ\n") if freq == :help
    r = tan(PI * freq / srate())
    r2 = r * r
    c1 = 1.0 / (1.0 + r * sqrt(2.0) + r2)
    c2 = -2.0 * c1
    c3 = c1
    c4 = 2.0 * (r2 - 1.0) * c1
    c5 = ((1.0 - r * sqrt(2.0)) + r2) * c1
    make_filter(3, list2vct([c1, c2, c3]), list2vct([0.0, c4, c5]))
  end
  
  def make_butter_low_pass(freq)
    doc("make_butter_low_pass(freq)
makes a Butterworth filter with low pass cutoff at FREQ.
The result can be used directly:
filter_sound(make_butter_low_pass(500.0)), or via the `butter'
generator\n") if freq == :help
    r = 1.0 / tan(PI * freq / srate())
    r2 = r * r
    c1 = 1.0 / (1.0 + r * sqrt(2.0) + r2)
    c2 = 2.0 * c1
    c3 = c1
    c4 = 2.0 * (1.0 - r2) * c1
    c5 = ((1.0 - r * sqrt(2.0)) + r2) * c1
    make_filter(3, list2vct([c1, c2, c3]), list2vct([0.0, c4, c5]))
  end
  
  def make_butter_band_pass(freq, band = nil)
    doc("make_butter_band_pass(freq, band)
makes a bandpass Butterworth filter with low edge at FREQ width BAND\n") if freq == :help
    d = 2.0 * cos(2.0 * PI * freq / srate())
    c = 1.0 / tan(PI * band / srate())
    c1 = 1.0 / (1.0 + c)
    c2 = 0.0
    c3 = -c1
    c4 = -c * d * c1
    c5 = (c - 1.0) * c1
    make_filter(3, list2vct([c1, c2, c3]), list2vct([0.0, c4, c5]))
  end
  
  def make_butter_band_reject(freq, band = nil)
    doc("make_butter_band_reject(freq, band)
makes a band-reject Butterworth filter with low edge at FREQ width BAND\n") if freq == :help
    d = 2.0 * cos(2.0 * PI * freq / srate())
    c = tan(PI * band / srate())
    c1 = 1.0 / (1.0 + c)
    c2 = -d * c1
    c3 = c1
    c4 = c2
    c5 = (1.0 - c) * c1
    make_filter(3, list2vct([c1, c2, c3]), list2vct([0.0, c4, c5]))
  end
  
  def down_oct(h = nil)
    doc("down_oct()
tries to move a sound down an octave\n") if h == :help
    len = frames()
    pow2 = (log(len) / log(2)).ceil
    fftlen = (2 ** pow2).round
    fftscale = 1.0 / fftlen
    rl1 = channel2vct(0, fftlen)
    im1 = make_vct(fftlen)
    fft(rl1, im1, 1)
    vct_scale!(rl1, fftscale)
    vct_scale!(im1, fftscale)
    rl2 = make_vct(2 * fftlen)
    im2 = make_vct(2 * fftlen)
    k = fftlen / 2
    j = fftlen + fftlen / 2
    (0...(fftlen / 2)).each do |i|
      vct_set!(rl2, i, rl1[i])
      vct_set!(rl2, j, rl1[k])
      vct_set!(im2, i, im1[i])
      vct_set!(im2, j, im1[k])
      k += 1
      j += 1
    end
    fft(rl2, im2, -1)
    vct2channel(rl2, 0, 2 * fftlen)
  end

  def spike(h = nil)
    doc("spike()
multiplies successive samples together to make a sound more spikey\n") if h == :help
    x1 = x2 = 0.0
    amp = maxamp()
    map_chan(lambda do |x0|
	       res = (x0 / (amp * amp)) * x2.abs * x1.abs
	       x2 = x1
	       x1 = x0
	       res
             end)
  end

  def zero_phase(h = nil)
    doc("zero_phase()
calls fft, sets all phases to 0, and un-ffts\n") if h == :help
    len = frames()
    pow2 = (log(len) / log(2)).ceil
    fftlen = (2 ** pow2).round
    fftscale = 1.0 / fftlen
    rl = channel2vct(0, fftlen)
    old_pk = vct_peak(rl)
    im = make_vct(fftlen)
    fft(rl, im, 1)
    rectangular2polar(rl, im)
    vct_scale!(rl, fftscale)
    vct_scale!(im, 0.0)
    fft(rl, im, -1)
    pk = vct_peak(rl)
    vct2channel(vct_scale!(rl, old_pk / pk), 0, len)
  end

  def rotate_phase(func)
    doc("rotate_phase(func)
calls fft, applies FUNC to each phase, then un-ffts\n") if func == :help
    len = frames()
    pow2 = (log(len) / log(2)).ceil
    fftlen = (2 ** pow2).round
    fftlen2 = (fftlen / 2).round
    fftscale = 1.0 / fftlen
    rl = channel2vct(0, fftlen)
    old_pk = vct_peak(rl)
    im = make_vct(fftlen)
    fft(rl, im, 1)
    rectangular2polar(rl, im)
    vct_scale!(rl, fftscale)
    vct_set!(im, 0, 0.0)
    j = fftlen - 1
    (1...fftlen2).each do |i|
      vct_set!(im, i, func.call(vct_ref(im, i)))
      vct_set!(im, j, -vct_ref(im, i))
      j -= 1
    end
    polar2rectangular(rl, im)
    fft(rl, im, -1)
    pk = vct_peak(rl)
    vct2channel(vct_scale!(rl, old_pk / pk), 0, len)
  end

  def spot_freq(samp, snd = false, chn = false)
    doc("spot_freq(samp, [snd[, chn]])
tries to determine the current pitch: spot_freq(left_sample())\n") if samp == :help
    pow2 = (log(srate(snd) / 20.0) / log(2)).ceil
    fftlen = (2 ** pow2).round
    data = autocorrelate(channel2vct(samp, fftlen, snd, chn))
    cor_peak = vct_peak(data)
    callcc do |ret|
      (1...fftlen - 2).each do |i|
        if data[i] < data[i + 1] and data[i + 1] > data[i + 2]
          logla = log10((cor_peak + data[i]) / (2.0 * cor_peak))
          logca = log10((cor_peak + data[i + 1]) / (2.0 * cor_peak))
          logra = log10((cor_peak + data[i + 2]) / (2.0 * cor_peak))
          offset = (0.5 * (logla - logra)) / (logla + logra + -2.0 * logca)
          ret.call(srate(snd) / (2 * (i + 1 + offset)))
        else
          0
        end
      end
    end
  end
  # $graph_hook.add_hook!("examp-left-sample-hook") do |snd, chn, y0, y1|
  #   report_in_minibuffer(format("(freq: %.3f)", spot_freq(left_sample(snd, chn))))
  # end
  #
  # or
  #
  # $mouse_click_hook.add_hook!("examp-cursor-hook") do |snd, chn, button, state, x, y, axis|
  #   if axis == Time_graph
  #     report_in_minibuffer(format("(freq: %.3f)", spot_freq(cursor(snd, chn))))
  #   end
  # end

  def make_hilbert_transform(len = 30)
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = 1.0 - cos(PI * i)
      if i == 0
        arr[k] = 0.0
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end

  def hilbert_transform(f, input)
    fir_filter(f, input)
  end

  def make_lowpass(fc, len = 30)
    fc = fc.to_f
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = sin(fc * i)
      if i == 0
        arr[k] = fc / PI
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end

  def lowpass(f, input)
    fir_filter(f, input)
  end

  def make_highpass(fc, len = 30)
    fc = fc.to_f
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = -sin(fc * i)
      if i == 0
        arr[k] = 1.0 - fc / PI
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end

  def highpass(f, input)
    fir_filter(f, input)
  end

  def make_bandpass(flo, fhi, len = 30)
    flo = flo.to_f
    fhi = fhi.to_f
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = sin(fhi * i) - sin(flo * i)
      if i == 0
        arr[k] = (fhi - flo) / PI
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end

  def bandpass(f, input)
    fir_filter(f, input)
  end

  def make_bandstop(flo, fhi, len = 30)
    flo = flo.to_f
    fhi = fhi.to_f
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = sin(flo * i) - sin(fhi * i)
      if i == 0
        arr[k] = 1.0 - (fhi - flo) / PI
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end

  def bandstop(f, input)
    fir_filter(f, input)
  end
end

module Moog
  doc "#{self.class} #{self.name}
Moog style four pole lowpass filter clm unit generator
  low pass, 24db/Oct, variable resonance, warm, analog sound ;-)
  [all this digital wizardry and we're back where we started!]

original C instrument by Tim Stilson
translation into clm and tuning by 
  Fernando Lopez-Lezcano, nando@ccrma.stanford.edu
  http://ccrma.stanford.edu/~nando/clm/moog

translated to Snd scheme function by Bill
(and translated to Snd ruby function by M. Scholz)\n"
  MOOG_GAINTABLE = vct(0.999969, 0.990082, 0.980347, 0.970764, 0.961304, 0.951996,
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
  MOOG_FREQTABLE = [0, -1,
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
  
  def make_moog_filter(freq, q = nil)
    doc("make_moog_filter(freq, q)
makes a new moog-filter generator. FREQ is the cutoff in Hz, Q sets
the resonance: 0 = no resonance, 1: oscillates at FREQ\n") if freq == :help
    [freq, q, make_vct(4), 0.0, envelope_interp(freq / (srate() * 0.5), MOOG_FREQTABLE)]
  end

  def moog_filter(m, sig = nil)
    doc("moog_filter(m, sig)
is the generator associated with make_moog_filter\n") if m == :help
    saturate = lambda do |x| [[x, -0.95].max, 0.95].min end
    fc = m[4]
    s = m[2]
    a = 0.25 * (sig - m[3])
    0.upto(3) do |cell|
      st = vct_ref(s, cell)
      a = saturate.call(a + fc * (a - st))
      vct_set!(s, cell, a)
      a = saturate.call(a + st)
    end
    out = a
    ix = fc * 99.0
    ixint = ix.round
    ixfrac = ix - ixint
    m[3] = a * m[1] *
                ((1 - ixfrac) * vct_ref(MOOG_GAINTABLE, ixint + 99) +
                 ixfrac * vct_ref(MOOG_GAINTABLE, ixint + 100))
    out
  end
end

# examp.rb ends here
