# examp.rb -- Guile -> Ruby translation

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Wed Apr 09 04:53:00 CEST 2003
# Version: $Revision: 1.52 $

#
# Utilities
#
# Class Hook
#  initialize(name) { |*args| ... }
#  add_hook!(name) { |*args| ... }
#  remove_hook!(name)
#  call(*args)
#  to_s
#  inspect
#
# extensions to Class Integer
#  even?
#  odd?
#  prime?
#
# extension to Module Enumerable
#  map_with_index { |x, i| ... }
#
# get_func_name(n)
# doc(str), putd(func), snd_putd(func)
# remove_if(func, lst)
# car(v), cadr(v), caddr(v), cdr(v)
# warn(*args), die(*args), message(*args)
# let { ... }
# array?(ary)
# to_rary(ary)
# shell(*cmd)
# get_args(args, key, val)
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
# fm_bell(start, dur, freq, amp, *args)
# fm_violin_rb(start, dur, freq, amp, *args)
# jc_reverb_rb(startime, dur, *args)
#
# fm_bell_snd(start, dur, freq, amp, amp_env, index_env, index)
# n_rev(*args)
# hello_dentist(frq, amp)
# ring_mod(freq, gliss_env)
# am(freq)
# vibro(speed, depth)
# fp(sr, osamp, osfreq)
# compand(h)
#
# Module Dsp (see dsp.scm)
#  butter(b, sig)
#  make_butter_high_pass(freq)
#  make_butter_low_pass(freq)
#  make_butter_band_pass(freq, band)
#  make_butter_band_reject(freq, band)
#  down_oct(h)
#  spike(h)
#  zero_phase(h)
#  rotate_phase(func)
#
# Module Env (see env.scm)
#  envelope_interp(*args)
#  stretch_envelope(*args)
#
# Module Moog (see moog.scm)
#  make_moog_filter(freq, q)
#  moog_filter(m, sig)

$IN_SND = true unless defined? $IN_SND

include Math
require "ws"

def get_func_name(n = 1)
  doc("get_func_name([n=1])
returns function name string\n") if n == :help
  caller(n)[0].scan(/^.*:in `(.*)'/)[0][0]
end

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
  # putd(func)
  #
  # put documentation (for usage in an Emacs Snd session)
  # 
  
  def putd(func)
    catch(:__Kernel_doc__) { send(func.to_sym, :help) } unless func.class <= Module
  rescue
  ensure
    puts Kernel.doc(func)
  end
end unless defined? @@docs

#
# snd_putd(func)
#
# snd put documentation (for usage in Snd)
# 

def snd_putd(func)
  catch(:__Kernel_doc__) { send(func.to_sym, :help) } unless func.class <= Module
rescue
ensure
  message Kernel.doc(func)
end

class Hook
  doc "#{self.class} #{self.name}
To set, add, remove, and show named hooks (see popup.rb).

Example:

  if $after_open_hook
    old_after = $after_open_hook
    $after_open_hook = Hook.new(\"original\") do |snd|
      old_after.call(snd) if old_after
    end
  else
    $after_open_hook = Hook.new
  end

  $after_open_hook.add_hook!(\"my_hook\") do |snd|
    set_cursor_follows_play(true, snd)
    set_channel_style(Channels_combined, snd)
  end

  #after_open_hook.to_s -> \"Hook name: my_hook (|*a|)\"

  $after_open_hook.remove_hook!(\"my_hook\")\n"
  
  def initialize(name = nil)
    @hooks = Hash.new
    add_hook!(name) do |*args|
      yield(*args)
    end if name and block_given?
  rescue
    warn get_func_name
  end
  
  def add_hook!(name, &func)
    @hooks.store(name, func)
    set_hook
  end

  def remove_hook!(name)
    @hooks.delete(name)
    @hooks.rehash
    set_hook
  end

  def call(*args)
    @hooks.each_value do |f|
      f.call(*args)
    end
  rescue
    warn get_func_name
  end

  def set_hook
    lambda do |*args|
      @hooks.each_value do |f|
        f.call(*args)
      end
    end
  end
  private :set_hook

  def hook_arity(f)
    case f.arity
    when 0
      "||"
    when 2
      "|a, b|"
    when 3
      "|a, b, c|"
    when -1
      "|*a|"
    when -2
      "|a, *b|"
    end
  end
  private :hook_arity

  def to_s
    str = ""
    @hooks.each do |k, v|
      str << "Hook name: " << k << " (" + hook_arity(v) + ")\n"
    end
    str
  end

  def inspect
    to_s
  end
end

class Integer
  doc "#{self.class} #{self.name}
usefule lisp-like extensions [(evenp x), (oddp x)]:
  x.even?
  x.odd?
  x.prime?\n"

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
  doc "#{self.class} #{self.name}
usefule extensions:
  map_with_index

  ary = [1, 2, 4]
  [3, 5, 7].map_with_index { |x, i| x + ary[i] } --> [4, 7, 11]\n"

  def map_with_index
    i = -1
    map do |x|
      i += 1
      yield(x, i)
    end
  end
end

##
## Utilities
##

def remove_if(func, lst)
  if lst.empty?
    []
  elsif func.call(lst[0])
    remove_if(func, lst[1..-1])
  else
    [lst[0], remove_if(func, lst[1..-1])]
  end
end

def car(v) v[0]; end

def cadr(v) v[1]; end

def caddr(v) v[2]; end

def cdr(v) v.shift; v; end

# warn(*args)
#
# If no error occurs it works like snd_print() or print(), if an error
# occurs it works like C's perror() function.  It works in script mode
# as well as in Snd.

def warn(*args)
  str = "Warning: " << format(*args) << ($! ? ": #{$!}" : "")
  str << (($@ and $DEBUG) ? "\n[#{$@.join("\n")}]" : "")
  message(str)
  $! = nil
end

# die(*args)
#
# If no error occurs it works like snd_error() or print(), if an error
# occurs it works like C's perror() function.  In script mode program
# terminates.

def die(*args)
  str = "Error: " << format(*args) << ($! ? ": #{$!}" : "")
  str << (($@ and $DEBUG) ? "\n[#{$@.join("\n")}]" : "")
  ($IN_SND  and (not ENV['EMACS'])) ? snd_error(str) : $stdout.print(str << "\n")
  $! = nil
  exit(1) unless $IN_SND
end

def message(*args)
  if $IN_SND and (not ENV['EMACS'])
    snd_print("\n" << format(*args))
  else
    $stdout.print(format(*args) << "\n")
  end
end

#
# let { ... }
# a local environment
#

def let
  yield
rescue
  warn get_func_name
end

def array?(ary)
  ary.class == Array
end

def to_rary(ary)
  doc("to_rary(ary)
Gets an array of two arrays (produced e.g. by all_chans()) and returns
an rarray combined by that two.
to_rary([[0, 1, 2], [00, 11, 22]]) ==> [[0, 00], [1, 11], [2, 22]]\n") if ary == :help
  a = ary.first.dup
  b = ary.last.dup
  nary = []
  a.each_with_index do |x, i| nary << [x, b[i]] end
  nary
end

def shell(*cmd)
  doc("shell(*cmd)
Sends cmd to a shell (executes it as a shell command) and returns the
result.\n") if cmd == :help
  str = String.new
  f = IO.popen(format(*cmd))
  str << f.getc until f.eof?
  f.close
  str
end

# get_args(args, key, val)
#
# returns value, whether default VAL or value of KEY found in ARGS

def get_args(args, key, val)
  if(key == :help and (args == key or args.member?(key) or args.assoc(key)))
    val = true
  elsif(args.member?(key))
    x = args[args.index(key) + 1]
    val = ((x == nil) ? val : x)
  elsif(args.assoc(key))
    val = (args.assoc(key)[1] rescue val)
  end
  val
end

def load_init_file(file)
  doc("load_init_file(file)
Returns false if file doesn't exist, otherwise loads it. File may
reside in current working dir or in $HOME dir.\n") if file == :help
  if File.exist? file
    load file
  elsif File.exist? "#{ENV["HOME"]}/#{file}"
    load "#{ENV["HOME"]}/#{file}"
  else
    false
  end
end

##
## Buffers Menu
##

def open_buffer(file)
  doc("open_buffer(file)
Adds a menu item that will select filename (use with $open_hook). See
also close_buffer().
Usage in ~./snd-ruby.rb
$buffer_menu = add_to_main_menu(\"Buffers\")
$open_hook = lambda { |file| open_buffer(file) }
$close_hook = lambda { |snd| close_buffer(snd) }\n") if file == :help
  add_to_menu($buffer_menu, file, lambda { || select_sound(find_sound(file)) })
  false
end

def close_buffer(snd)
  doc("close_buffer(snd)
Removes the menu item associated with snd (use with $close_hook). See
also open_buffer().
Usage in ~./snd-ruby.rb
$buffer_menu = add_to_main_menu(\"Buffers\")
$open_hook = lambda { |file| open_buffer(file) }
$close_hook = lambda { |snd| close_buffer(snd) }\n") if snd == :help
  remove_from_menu($buffer_menu, file_name(snd))
  false
end

##
## Reopen Menu
##

$reopen_names = []

def add_to_reopen_menu(snd)
  doc("add_to_reopen_menu(snd)
Adds snd to the Reopen menu (use with $close_hook). See also
check_reopen_menu().
Usage in ~./snd-ruby.rb
$reopen_menu = add_to_main_menu(\"Reopen\")
$open_hook = lambda { |file| check_reopen_menu(file) }
$close_hook = lambda { |snd| add_to_reopen_menu(snd) }\n") if snd == :help
  brief_name = short_file_name(snd)
  long_name = file_name(snd)
  reopen_max_length = 8
  unless($reopen_names.member?(brief_name))
    add_to_menu($reopen_menu, brief_name,
		lambda { | |
		  remove_from_menu($reopen_menu, brief_name)
		  open_sound(long_name) 
		}, 0)
    $reopen_names << brief_name
    if($reopen_names.length > reopen_max_length)
      goner = $reopen_names.shift
      remove_from_menu($reopen_menu, goner)
    end
  end
  false
end

def check_reopen_menu(file)
  doc("check_reopen_menu(file)
Removes filename from the Reopen menu list (use with $open_hook). See
also add_to_reopen_menu().
Usage in ~./snd-ruby.rb
$reopen_menu = add_to_main_menu(\"Reopen\")
$open_hook = lambda { |file| check_reopen_menu(file) }
$close_hook = lambda { |snd| add_to_reopen_menu(snd) }\n") if file == :help
  just_file = lambda { |name|
    last_slash = -1
    len = name.length
    (0...len).each { |i| last_slash = i if name[i] == ?/ }
    name[last_slash + 1]
  }
  brief_name = just_file.call(file)
  if($reopen_names.member?(brief_name))
    $reopen_names = remove_if(lambda { |n|
				val = (n == brief_name)
				remove_from_menu($reopen_menu, brief_name) if val
				val
			      }, $reopen_names)
  end
  false
end

##
## FM
##

##
## Michael McNabb's FM bell (see bell.scm and fm_bell_snd() below)
##

def fm_bell(start = 0.0, dur = 1.0, freq = 220.0, amp = 0.3, *args)
  doc("fm_bell([start=0.0[, dur=1.0[, freq=220.0[, amp=0.3[, *args]]]]])
	:amp_env,   [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
	:index_env, [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
	:index,     1.0
	:distance,  1.0
	:reverb,    0.01
Usage: with_sound { fm_bell }
       with_sound {
         C = 130.8
         A = 110
         G = 98
         E = 82.4
         notes = [C, A, G, E]
         fbell = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
         abell = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
         fm_bell(0, 12, E, 0.4,
       	  :amp_env, abell,
       	  :index_env, fbell,
       	  :index, 0.1)
         (0...notes.length).each { |i|
           fm_bell(i * 2, 4, notes[i], 0.5,
       	    :amp_env, abell,
       	    :index_env, fbell,
       	    :index, 0.2 * (i + 0.1))
         }
       }\n") if start == :help
  amp_env   = get_args(args, :amp_env, [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0])
  index_env = get_args(args, :index_env, [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2])
  index     = get_args(args, :index, 1.0)
  distance  = get_args(args, :distance, 1.0)
  reverb    = get_args(args, :reverb, 0.01)
  srate = (mus_srate() rescue $rbm_srate)
  chans = (mus_channels($rbm_output) rescue $rbm_channels)
  beg = (srate * start).round
  len = beg + (srate * dur).round
  fmind1 = hz2radians(32.0 * freq)
  fmind2 = hz2radians(4.0 * (8.0 - freq / 50.0))
  fmind3 = fmind2 * 0.705 * (1.4 - freq / 250.0)
  fmind4 = hz2radians(32.0 * (20 - freq / 20))
  mod1 = make_oscil(freq * 2)
  mod2 = make_oscil(freq * 1.41)
  mod3 = make_oscil(freq * 2.82)
  mod4 = make_oscil(freq * 2.4)
  car1 = make_oscil(freq)
  car2 = make_oscil(freq)
  car3 = make_oscil(freq * 2.4)
  indf = make_env(index_env, index, dur)
  ampf = make_env(amp_env, amp, dur)
  loc = make_locsig(kernel_rand(90.0), distance, reverb, $rbm_output, $rbm_reverb, chans)
  beg.upto(len) { |i|
    fmenv = env(indf)
    locsig(loc, i, env(ampf) * (oscil(car1, fmenv * fmind1 * oscil(mod1)) +
				0.15 * oscil(car2, fmenv *
					    (fmind2 * oscil(mod2) + fmind3 * oscil(mod3))) +
				0.15 * oscil(car3, fmenv * fmind4 * oscil(mod4))))
  }
rescue
  die get_func_name
end

# for a faster version see v.rb

def fm_violin_rb(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.3, *args)
  doc("fm_violin_rb([start=0.0[, dur=1.0[, freq=440.0[, amp=0.3[, *args]]]]])
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
	:gliss_env,             [0, 0,  100, 0]
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
	:reverb_amount,         0.01
	:index_type,            :violin
	:degree,                false
	:distance,              1.0
	:degrees,               false
   Ruby: fm_violin_rb(0, 1, 440, 0.1, :fm_index, 2.0)
  Guile: (fm-violin 0 1 440 0.1 :fm-index 2.0)
Example: with_sound { fm_violin_rb(0, 1, 440, 0.1, :fm_index, 2.0) }\n") if start == :help
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
  gliss_env             = get_args(args, :gliss_env, [0, 0,  100, 0])
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
  index_type            = get_args(args, :index_type, :violin)
  degree                = get_args(args, :degree, false)
  distance              = get_args(args, :distance, 1.0)
  degrees               = get_args(args, :degrees, false)
  srate = (mus_srate() rescue $rbm_srate)
  chans = (mus_channels($rbm_output) rescue $rbm_channels)
  beg = (srate * start).round
  len = beg + (srate * dur).round
  frq_scl = hz2radians(freq)
  modulate = fm_index.nonzero?
  maxdev = frq_scl * fm_index
  vln = (not (index_type == :cello))
  logfreq = log(freq)
  sqrtfreq = sqrt(freq)
  index1 = (fm1_index or [PI, maxdev * (vln ? 5.0 : 7.5) / logfreq].min)
  index2 = (fm2_index or [PI, maxdev * 3.0 * 
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
  vib = 0.0
  modulation = 0.0
  loc = make_locsig((degree or degrees or kernel_rand(90.0)), 
		    distance, reverb_amount, $rbm_output, $rbm_reverb, chans)
  fuzz = 0.0
  ind_fuzz = 1.0
  amp_fuzz = 1.0
  beg.upto(len) { |i|
    fuzz = rand(fm_noi) if noise_amount.nonzero?
    vib = env(frqf) + triangle_wave(pervib) + rand_interp(ranvib)
    ind_fuzz = 1.0 + rand_interp(ind_noi) if ind_noi
    amp_fuzz = 1.0 + rand_interp(amp_noi) if amp_noi
    if(modulate)
      if(easy_case)
	modulation = env(indf1) * polynomial(coeffs, oscil(fmosc1, vib))
      else
	modulation = env(indf1) * oscil(fmosc1, fm1_rat * vib + fuzz) +
	  env(indf2) * oscil(fmosc2, fm2_rat * vib + fuzz) +
	  env(indf3) * oscil(fmosc3, fm3_rat * vib + fuzz)
      end
    end
    locsig(loc, i, env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz * modulation))
  }
rescue
  die get_func_name
end

# for a faster version see v.rb

def jc_reverb_rb(startime, dur, *args)
  doc("jc_reverb_rb(startime, dur, *args)
	:low_pass, false
	:volume,   1.0
	:amp_env1, false
	:amp_env2, false
	:delay1,   0.013
	:delay2,   0.011
The old Chowning reverberator (see examp.scm).
Usage: jc_reverb_rb(0, 2.5, :volume, 0.1)
       with_sound(:reverb, :jc_reverb) { fm_violin }\n") if get_args(args, :help, false)
  low_pass = get_args(args, :low_pass, false)
  volume   = get_args(args, :volume, 1.0)
  amp_env1 = get_args(args, :amp_env1, false)
  amp_env2 = get_args(args, :amp_env2, false)
  delay1   = get_args(args, :delay1, 0.013)
  delay2   = get_args(args, :delay2, 0.011)
  allpass1 = make_all_pass(-0.700, 0.700, 1051)
  allpass2 = make_all_pass(-0.700, 0.700, 337)
  allpass3 = make_all_pass(-0.700, 0.700, 113)
  comb1 = make_comb(0.742, 4799)
  comb2 = make_comb(0.733, 4999)
  comb3 = make_comb(0.715, 5399)
  comb4 = make_comb(0.697, 5801)
  srate = (mus_srate() rescue $rbm_srate)
  chans = (mus_channels($rbm_output) rescue $rbm_channels)
  outdel1 = make_delay((delay1 * srate).round)
  outdel2 = make_delay((delay2 * srate).round) if chans == 2
  beg = (srate * startime).round
  len = beg + (srate * dur).round
  envA = (amp_env1 ? make_env(amp_env1, volume, dur) : false)
  envB = (amp_env2 ? make_env(amp_env2, volume, dur) : false)
  delA = (envA ? env(envA) : volume)
  delB = (envB ? env(envB) : volume)
  allpass_sum, all_sums = 0.0, 0.0
  comb_sumA, comb_sum_1A, comb_sum_2A = 0.0, 0.0, 0.0
  comb_sumB, comb_sum_1B, comb_sum_2B = 0.0, 0.0, 0.0
  beg.upto(len) { |i|
    ho = ina(i, $rbm_reverb)
    allpass_sum = all_pass(allpass3, all_pass(allpass2, all_pass(allpass1, ho)))
    comb_sum_2A = comb_sum_1A
    comb_sum_1A = comb_sumA
    comb_sumA = comb(comb1, allpass_sum) + comb(comb2, allpass_sum) +
      comb(comb3, allpass_sum) + comb(comb4, allpass_sum)
    if(low_pass)
      all_sums = 0.25 * (comb_sumA + comb_sum_2A) + 0.5 * comb_sum_1A
    else
      all_sums = comb_sumA
    end
    outa(i, delA * delay(outdel1, all_sums), $rbm_output)
    if($rbm_reverb_channels == 2)
      ho = inb(i, $rbm_reverb)
      allpass_sum = all_pass(allpass3, all_pass(allpass2, all_pass(allpass1, ho)))
      comb_sum_2B = comb_sum_1B
      comb_sum_1B = comb_sumB
      comb_sumB = comb(comb1, allpass_sum) + comb(comb2, allpass_sum) +
	comb(comb3, allpass_sum) + comb(comb4, allpass_sum)
      if(low_pass)
	all_sums = 0.25 * (comb_sumB + comb_sum_2B) + 0.5 * comb_sum_1B
      else
	all_sums = comb_sumB
      end
    end
    outb(i, delB * delay(outdel2, all_sums), $rbm_output) if chans == 2
  }
rescue
  die get_func_name
end

#
# Michael McNabb's FM bell (see bell.scm)
#

def fm_bell_snd(start = 0.0, dur = 1.1, freq = 220.0, amp = 0.3, 
		amp_env = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0], 
		index_env = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2], 
		index = 1.0)
  doc("fm_bell_snd(start=0.0[, dur=1.0[, freq=220.0[, amp=0.3
         [, amp_env=[0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
         [, index_env=[0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
         [, index=1.0]]]]]])
Mixes in one fm bell note (see bell.scm).
fm_bell_snd works with fm_play in difference to fm_bell, which works
with with_sound.
fm_play(:output, \"bell.snd\") {
  fbell = [0, 1, 2, 1.1000, 25, 0.7500, 75, 0.5000, 100, 0.2000]
  abell = [0, 0, 0.1000, 1, 10, 0.6000, 25, 0.3000, 50, 0.1500, 90, 0.1000, 100, 0]
  fm_bell_snd(0.0, 1.0, 220.0, 0.5, abell, fbell, 1.0)
}\n") if start == :help
  srate = (srate() rescue 22050);
  beg = (srate * start).round;
  len = beg + (srate * dur).round;
  fmind1 = hz2radians(32.0 * freq);
  fmind2 = hz2radians(4.0 * (8.0 - freq / 50.0));
  fmind3 = fmind2 * 0.705 * (1.4 - freq / 250.0);
  fmind4 = hz2radians(32.0 * (20 - freq / 20));
  mod1 = make_oscil(freq * 2);
  mod2 = make_oscil(freq * 1.41);
  mod3 = make_oscil(freq * 2.82);
  mod4 = make_oscil(freq * 2.4);
  car1 = make_oscil(freq);
  car2 = make_oscil(freq);
  car3 = make_oscil(freq * 2.4);
  indf = make_env(index_env, index, dur);
  ampf = make_env(amp_env, amp, dur);
  out_data = make_vct(len);
  vct_map!(out_data, 
	   lambda { | |
	     fmenv = env(indf);
	     env(ampf) * (oscil(car1, fmenv * fmind1 * oscil(mod1)) +
			  0.15 * oscil(car2, fmenv *
				      (fmind2 * oscil(mod2) + fmind3 * oscil(mod3))) +
			  0.15 * oscil(car3, fmenv * fmind4 * oscil(mod4)));
	   });
  mix_vct(out_data, beg, false, 0, false);
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
rescue
  die get_func_name
end

def hello_dentist(freq = 40.0, amp = 0.1)
  doc("hello_dentist([freq=40.0[, amp=0.1]])
Varies the sampling rate randomly, making a voice sound quavery (see
examp.scm).
Usage: hello_dentist(40.0, 0.1)\n") if freq == :help
  rn = make_rand_interp(freq, amp)
  i, j = 0, 0
  len = frames()
  in_data = samples2vct(0, len)
  out_len = (len * (1.0 + 2 * amp)).round
  out_data = make_vct(out_len)
  # make_src(input, srate=1.0, width=5)
  rd = make_src(lambda { |dir|
		  val = ((i >= 0) and (i < len)) ? vct_ref(in_data, i) : 0.0
		  i = i + dir
		  val
		})
  vct2channel(vct_map!(out_data, lambda { || src(rd, rand_interp(rn)) }))
rescue
  die get_func_name
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
  lambda { |i| oscil(os, env(genv)) * i }
rescue
  die get_func_name
end

def am(freq = 440.0)
  doc("am([freq=440.0])
Returns an amplitude-modulator (see examp.scm).
Usage: map_chan(am(440.0))\n") if freq == :help
  os = make_oscil(freq)
  lambda { |i| amplitude_modulate(1.0, i, oscil(os)) }
rescue
  die get_func_name
end

def vibro(speed = 20, depth = 0.5)
  doc("vibro([speed=20[, depth=0.5]])
This is taken from sox (vibro.c) (see examp.scm).
Usage: map_chan(vibro(20, 0.5))\n") if speed == :help
  sine = make_oscil(speed)
  scl = 0.5 * depth
  offset = 1.0 - scl
  lambda { |i| i * (offset + scl * oscil(sine)) }
rescue
  die get_func_name
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
  vct_map!(out_data, lambda { | |
	     src(s, osamp * oscil(os), lambda { |dir|
		   dir > 0 ? next_sample(sf) : previous_sample(sf)
		 })
	   })
  free_sample_reader(sf)
  vct2samples(0, len, out_data)
rescue
  die get_func_name
end

def compand(doc = false)
  doc("compand()
Since there's no state in this function, it can be used without change
in any of the mapping functions (unlike echo, for example) (see
examp.scm).
Usage: map_chan(compand())\n") if doc == :help
  tbl = vct(-1.000, -0.960, -0.900, -0.820, -0.720, -0.600, -0.450, -0.250, 
	    0.000, 0.250, 0.450, 0.600, 0.720, 0.820, 0.900, 0.960, 1.000)
  lambda { |i| 
    index = 8.0 + 8.0 * i
    array_interp(tbl, index, 17)
  }
rescue
  die get_func_name
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
    rl1 = samples2vct(0, fftlen)
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
    vct2samples(0, 2 * fftlen, rl2)
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
    rl = samples2vct(0, fftlen)
    old_pk = vct_peak(rl)
    im = make_vct(fftlen)
    fft(rl, im, 1)
    rectangular2polar(rl, im)
    vct_scale!(rl, fftscale)
    vct_scale!(im, 0.0)
    fft(rl, im, -1)
    pk = vct_peak(rl)
    vct2samples(0, len, vct_scale!(rl, old_pk / pk))
  end

  def rotate_phase(func)
    doc("rotate_phase(func)
calls fft, applies FUNC to each phase, then un-ffts\n") if func == :help
    len = frames()
    pow2 = (log(len) / log(2)).ceil
    fftlen = (2 ** pow2).round
    fftlen2 = (fftlen / 2).round
    fftscale = 1.0 / fftlen
    rl = samples2vct(0, fftlen)
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
    vct2samples(0, len, vct_scale!(rl, old_pk / pk))
  end
end

module Env
  doc "#{self.class} #{self.name}
contains the envelope_interp() only. It is needed by module Moog (see
env.scm)\n"

  def envelope_interp(*args)
    doc("envelope_interp(*args)
envelope_interp(x, env, base = 1.0) -> value of env at x;
base controls connecting segment type:
envelope_interp(0.3, [0, 0, 0.5, 1, 1, 0]) -> 0.6\n") if args.first == :help
    x = args[0]
    env = args[1]
    base = args[2]
    if (not env) or env.empty?
      0.0
    elsif x <= env[0] or env[2..-1].empty?
      env[1]
    elsif env[2] > x
      if env[1] == env[3] or (base and base == 0.0)
        env[1]
      elsif (not base) or base == 1.0
        env[1] + (x - env[0]) * ((env[3] - env[1]) / (env[2] - env[0]))
      else
        env[1] + ((env[3] - env[1]) / (base - 1.0)) *
                                     ((base ** ((x - env[0]) / (env[2] - env[0]))) - 1.0)
      end
    else
      envelope_interp(x, env[2..-1])
    end
  end

  def stretch_envelope(*args)
    doc("stretch_envelope(*args)
args: env, old_attack, new_attack, old_decay, new_decay
Takes ENV and returns a new envelope based on it but with the attack
and optionally decay portions stretched or squeezed; OLD_ATTACK is the
original x axis attack end point, NEW_ATTACK is where that section
should end in the new envelope.  Similarly for OLD_DECAY and
NEW_DECAY.  This mimics divseg in early versions of CLM and its
antecedents in Sambox and Mus10 (linen).
stretch_envelope([0, 0, 1, 1], 0.1, 0.2)
                 -> [0, 0, 0.2, 0.1, 1.0, 1]
stretch_envelope([0, 0, 1, 1, 2, 0], 0.1, 0.2, 1.5, 1.6)
                 -> [0, 0, 0.2, 0.1, 1.1, 1, 1.6, 0.5, 2.0, 0]\n") if args[0] == :help
    fn = args[0].map do |x| x.to_f end
    old_att = (args[1].to_f or false)
    new_att = (args[2].to_f or false)
    old_dec = (args[3].to_f or false)
    new_dec = (args[4].to_f or false)
    if old_att and (not new_att)
      warn "wrong number of arguments"
    elsif not new_att
      fn
    elsif old_dec and (not new_dec)
      warn "wrong number of arguments"
    else
      new_x = x0 = fn[0]
      last_x = fn[fn.length - 2]
      y0 = fn[1]
      new_fn = [y0, x0]
      scl = (new_att - x0) / [0.0001, old_att - x0].max
      stretch_envelope_1 = lambda do |new_fn, old_fn|
        if old_fn.empty?
          new_fn
        else
          x1 = old_fn[0]
          y1 = old_fn[1]
          if x0 < old_att and x1 >= old_att
            if x1 == old_att
              y0 = y1
            else
              y0 = y0 + (y1 - y0) * ((old_att - x0) / (x1 - x0))
            end
            x0 = old_att
            new_x = new_att
            new_fn << new_x << y0
            scl = (old_dec ? ((new_dec - new_att) / (old_dec - old_att)) :
                   ((last_x - new_att) / (last_x - old_att)))
          end
          if old_dec and x0 < old_dec and x1 >= old_dec
            if x1 == old_dec
              y0 = y1
            else
              y0 = y0 + (y1 - y0) * ((old_dec - x0) / (x1 - x0))
            end
            x0 = old_dec
            new_x = new_dec
            new_fn << new_x << y0
            scl = (last_x - new_dec) / (last_x - old_dec)
          end
          unless x0 == x1
            new_x += scl * (x1 - x0)
            new_fn << new_x << y1
            x0, y0 = x1, y1
          end
          stretch_envelope_1.call(new_fn, old_fn[2..-1])
        end
      end
      if old_dec and old_dec == old_att
        old_dec = 0.000001 * last_x
      end
      stretch_envelope_1.call(new_fn, fn[2..-1])
    end
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
  http://www-ccrma.stanford.edu/~nando/clm/moog

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

  include Env
  
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
