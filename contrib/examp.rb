# examp.rb -- Guile -> Ruby translation

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Fr  Feb 28 04:40:38 CET 2003
# Version: $Revision: 1.29 $

#
# Utilities
#
# get_func_name(n)
# doc(str), putd(func), snd_putd(func)
# help
# car(v), cadr(v), caddr(v), cdr(v)
# warn(str), die(str), message(*args)
# shell(cmd)
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
# jc_reverb_rb(*args)
# with_sound(*args) { ... }
#
# fm_play(func, outfile, play_f)
# fm_bell_snd(start, dur, freq, amp, amp_env, index_env, index)
# n_rev(*args)
# hello_dentist(frq, amp)
# ring_mod(freq, gliss_env)
# am(freq)
# vibro(speed, depth)
# fp(sr, osamp, osfreq)
# compand(h)
#
# Module NB (see nb.scm)
#  nb(note, file), unb(file), prune_db
#  files_popup_info(type, position, name)
#  files_popup_quit(type, position, name)
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
#
# Module Moog (see moog.scm)
#  make_moog_filter(freq, q)
#  moog_filter(m, sig)

include Math

$IN_SND = true unless defined? $IN_SND

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

def help
  message("## Functions available
#
# get_func_name(n)
# doc(str), putd(func), snd_putd(func)
# help
# car(v), cadr(v), caddr(v), cdr(v)
# warn(str), die(str), message(*args)
# shell(cmd)
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
# jc_reverb_rb(*args)
# with_sound(*args) { ... }
#
# fm_play(func, outfile, play_f)
# fm_bell_snd(start, dur, freq, amp, amp_env, index_env, index)
# n_rev(*args)
# hello_dentist(frq, amp)
# ring_mod(freq, gliss_env)
# am(freq)
# vibro(speed, depth)
# fp(sr, osamp, osfreq)
# compand(h)
#
# Module NB (see nb.scm)
#  nb(note, file), unb(file), prune_db
#  files_popup_info(type, position, name)
#  files_popup_quit(type, position, name)
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
#
# Module Moog (see moog.scm)
#  make_moog_filter(freq, q)
#  moog_filter(m, sig)
#
# Global variables
#
# $IN_SND               = #{$IN_SND}
#
# $rbm_file_name        = \"#{$rbm_file_name}\"
# $rbm_srate            = #{$rbm_srate}
# $rbm_channels         = #{$rbm_channels}
# $rbm_header_type      = Mus_next (#{$rbm_header_type})
# $rbm_data_format      = Mus_bshort (#{$rbm_data_format})
# $rbm_comment          = \"#{$rbm_comment}\"
# $rbm_statistics       = #{$rbm_statistics}
# $rbm_play             = #{$rbm_play}
# $rbm_player           = #{$rbm_player}
# $rbm_reverb_file_name = \"#{$rbm_reverb_file_name}\"
# $rbm_reverb_channels  = #{$rbm_reverb_channels}
# $rbm_reverb_func      = #{$rbm_reverb_func}
#
# Example: snd_putd :with_sound
#          prints documentation string of with_sound()\n")
end

##
## Utilities
##

def car(v) v[0]; end

def cadr(v) v[1]; end

def caddr(v) v[2]; end

def cdr(v) v.shift; v; end

# warn([str="Warning"])
#
# If no error occurs it works like snd_print() or print(), if an error
# occurs it works like C's perror() function. It works in script mode
# and in Snd too.

def warn(str = "Warning")
  if $IN_SND
    snd_print("\n#{str}#{$! ? ": #{$!}" : ""}")
    snd_print("\n[#{$@.join("\n")}]") if $@ and $DEBUG
  else
    print("#{str}#{$! ? ": #{$!}" : ""}\n")
    print("#{$@.join("\n")}") if $@ and $DEBUG
  end
end

# die([str="Error"[, n=1]])
#
# If no error occurs it works like snd_print() or print(), if an error
# occurs it works like C's perror() function. It exits only in script
# mode with error number n.

def die(str = "Error", n = 1)
  warn(str)
  exit(n) unless $IN_SND
end

def message(*args)
  if $IN_SND
    snd_print "\n" << format(*args)
  else
    STDOUT.print format(*args) << "\n"
  end
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

#
# *clm-like variables used by fm_bell(), fm_violin_rb(), jc_reverb_rb(), and
# with_sound()
#

$rbm_output = false
$rbm_file_name = "test.snd"
$rbm_srate = 22050
$rbm_channels = 1
$rbm_header_type = Mus_next
$rbm_data_format = Mus_bshort
$rbm_comment = "created by #{ENV["USER"]}"
$rbm_statistics = false
$rbm_play = 0
$rbm_player = "sndplay"
$rbm_reverb = false
$rbm_reverb_file_name = "reverb.snd"
$rbm_reverb_channels = 1
$rbm_reverb_func = false

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
	    partials2polynomial([fm1_rat, index1, 
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

def jc_reverb_rb(args = [])
  doc("jc_reverb_rb([args=[]])
	:decay,    1.0
	:low_pass, false
	:volume,   1.0
	:amp_env1, false
	:amp_env2, false
	:delay1,   0.013
	:delay2,   0.011
The old Chowning reverberator (see examp.scm).
Usage: jc_reverb_rb(:decay, 2.0, :volume, 0.1)
       with_sound(:reverb, :jc_reverb) { fm_violin }\n") if get_args(args, :help, false)
  decay    = get_args(args, :decay, 1.0)
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
  dur = decay + mus_sound_frames($rbm_file_name) / srate.to_f
  len = (srate * dur).round
  envA = (amp_env1 ? make_env(amp_env1, volume, dur) : false)
  envB = (amp_env2 ? make_env(amp_env2, volume, dur) : false)
  delA = (envA ? env(envA) : volume)
  delB = (envB ? env(envB) : volume)
  allpass_sum, all_sums = 0.0, 0.0
  comb_sumA, comb_sum_1A, comb_sum_2A = 0.0, 0.0, 0.0
  comb_sumB, comb_sum_1B, comb_sum_2B = 0.0, 0.0, 0.0
  0.upto(len) { |i|
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

def with_sound(*args)
  doc("with_sound(*args) { ... }
	:output,            $rbm_file_name
	:continue_old_file, false
	:channels,          $rbm_channels
	:statistics,        $rbm_statistics
	:play,              $rbm_play
	:player,            $rbm_player
	:srate,             $rbm_srate
	:header_type,       $rbm_header_type
	:data_format,       $rbm_data_format
	:comment,           $rbm_comment
	:reverb,            false
	:revfile,           $rbm_reverb_file_name
	:reverb_channels,   $rbm_reverb_channels
	:reverb_data,       []
	:scaled_to,         false
	:scaled_by,         false
Usage: with_sound(:play, 1, :statistics, true) { fm_violin }\n") if get_args(args, :help, false)
  output            = get_args(args, :output, $rbm_file_name)
  continue_old_file = get_args(args, :continue_old_file, false)
  channels          = get_args(args, :channels, $rbm_channels)
  statistics        = get_args(args, :statistics, $rbm_statistics)
  play              = get_args(args, :play, $rbm_play)
  player            = get_args(args, :player, $rbm_player)
  srate             = get_args(args, :srate, $rbm_srate)
  header_type       = get_args(args, :header_type, $rbm_header_type)
  data_format       = get_args(args, :data_format, $rbm_data_format)
  comment           = get_args(args, :comment, $rbm_comment)
  reverb            = get_args(args, :reverb, $rbm_reverb_func)
  revfile           = get_args(args, :revfile, $rbm_reverb_file_name)
  reverb_channels   = get_args(args, :reverb_channels, $rbm_reverb_channels)
  reverb_data       = get_args(args, :reverb_data, [])
  scaled_to         = get_args(args, :scaled_to, false)
  scaled_by         = get_args(args, :scaled_by, false)
  $rbm_file_name = output
  $rbm_channels = channels
  $rbm_srate = srate
  $rbm_reverb_file_name = revfile
  $rbm_reverb_channels = reverb_channels
  case play
  when true
    play = 1
  when false, nil
    play = 0
  else
    play = play.abs
  end
  $rbm_play = play
  if $IN_SND and (snd = find_sound(output))
    close_sound(snd)
  end
  unless continue_old_file
    old_srate = mus_srate
    mus_set_srate(srate)
    $rbm_output = $rbm_reverb = false
    File.unlink(output) if File.exist?(output)
    $rbm_output = make_sample2file(output, channels, data_format, header_type, comment)
    if reverb
      File.unlink(revfile) if File.exist?(revfile)
      $rbm_reverb = make_sample2file(revfile, reverb_channels, data_format, header_type, "rev")
    end
  else
    $rbm_output = continue_sample2file(output)
    $rbm_reverb = continue_sample2file(revfile) if reverb
  end
  atime = Time.new if statistics
  yield
  if reverb
    mus_close($rbm_reverb)
    $rbm_reverb = make_file2sample(revfile)
    (reverb.class == Proc) ? reverb.call(reverb_data) : send(reverb, reverb_data)
  end
  unless continue_old_file
    if reverb
      mus_close($rbm_reverb)
      $rbm_reverb = false
    end
    mus_close($rbm_output)
    $rbm_output = false
    mus_set_srate(old_srate)
  end
  if $IN_SND
    snd = open_sound(output)
    olds = sync(snd)
    set_sync(true, snd)
    scale_to(scaled_to, snd) if scaled_to
    scale_by(scaled_by, snd) if scaled_by
    save_sound(snd) if scaled_to or scaled_by
    set_sync(olds, snd)
  end
  if statistics
    rtime = Time.new - atime
    samps = mus_sound_samples(output)
    max_amp = mus_sound_maxamp(output)
    srate = srate.to_f
    message("    Sound File: %s", output)
    message("      Duration: %.4f", (samps / srate / channels))
    message("  Compute time: %.3f, Compute ratio: %.2f", rtime,
	    rtime * (srate / samps) * channels)
    message("  OutA max amp: %.3f (near %.3f secs)", max_amp[1], max_amp[0] / srate)
    message("  OutB max amp: %.3f (near %.3f secs)",
	    max_amp[3], max_amp[2] / srate) if channels == 2
    if(reverb)
      max_amp = mus_sound_maxamp(revfile)
      message("  RevA max amp: %.3f (near %.3f secs)", max_amp[1], max_amp[0] / srate)
      message("  RevB max amp: %.3f (near %.3f secs)",
	      max_amp[3], max_amp[2] / srate) if reverb_channels == 2
    end
  end
  1.upto(play) { |i| ($IN_SND ? play_and_wait(snd) : system("#{player} #{output}")) }
  output
rescue
  die get_func_name
end

=begin
# Examples:

with_sound { fm_violin }

with_sound(:channels, 2,
	   :play, 3,
	   :statistics, true,
	   :reverb_channels, 2,
	   :reverb, :jc_reverb,	# or :reverb, "jc_reverb",
	   :reverb_data, [:decay, 0.8, :volume, 0.3],
	   :reverb_channels, 1) { 
  0.upto(3) { |i| fm_violin_rb(i, 1, 220 * (i + 1), 0.3, :distance, i * 0.4) }
}

with_sound(:play, 1,
	   :channels, 2,
	   :scaled_to, 0.3,
	   :reverb, :jc_reverb,
	   :statistics, true) { 
  0.upto(20) { |i| 
    metalamp = [0, 0, 0.5, 1, 5, 1, 10, 0.5, 15, 0.25, 35, 0.1, 100, 0]

    fm_violin_rb(i * 0.1, 1, 220 + i * 10, 0.1, 
	      :fm_index, i * 0.5, :distance, i * 0.05, :amp_env, metalamp)

    fm_violin_rb(i * 0.1, 1, 2200 - i * 10, 0.1, 
	      :fm_index, i * 0.5, :distance, i * -0.05, :amp_env, metalamp)
  }
}

with_sound(:play, 1) { 
  fm_violin_rb(0, 1, 440)
  with_sound(:continue_old_file, true, :play, 0) {
    fm_violin_rb(1, 1, 220)
  }
  with_sound(:continue_old_file, true, :play, 0) {
    fm_violin_rb(2, 1, 880)
    with_sound(:continue_old_file, true, :play, 0) {
      fm_violin_rb(3, 1, 660)
    }
  }
  fm_violin_rb(4, 1, 440)
}

=end

def fm_play(func, outfile = "test.snd", play_f = true)
  doc("fm_play(func[, outfile=\"test.snd\"[, play_f=true]])
Usage: fm_play(lambda { fm_bell_snd(0, 1, 440, 0.1) })\n") if func == :help
  snd = new_sound(outfile, Mus_next, Mus_bshort, 22050, 1, "created by fm_play()")
  atime = Time.new
  func.call()
  btime = Time.new
  save_sound(snd)
  message("time: #{btime - atime}")
  play() if play_f
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
fm_play(lambda {
  fbell = [0, 1, 2, 1.1000, 25, 0.7500, 75, 0.5000, 100, 0.2000]
  abell = [0, 0, 0.1000, 1, 10, 0.6000, 25, 0.3000, 50, 0.1500, 90, 0.1000, 100, 0]
  fm_bell_snd(0.0, 1.0, 220.0, 0.5, abell, fbell, 1.0)
}, \"bell.snd\")\n") if start == :help
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

def n_rev(args = [])
  doc("n_rev([args=[]])
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

#
# from nb.scm
# 

module NB
  $nb_database = "nb"
  doc "#{self.class} #{self.name} is a translation of nb.scm.
Provide pop-up help in the Files viewer.
If you have `dbm', any data associated with the file in the dbm
database will also be posted.  The database name is defined by
$nb_database (#{$nb_database}).  You may replace the require statement
with an other database library which you have, e.g. `gdbm'.
The function nb(note[, file=@current_file]) adds NOTE to the info
currently associated with FILE.
To remove a file's info, unb([file=@current_file]).
To clean non-existent file references out of the database,
prune_db()."

  require "dbm"

  @current_file = nil

  def nb(note, file = @current_file)
    doc("nb(note[, file=@current_file])
Adds NOTE to the info associated with FILE.\n") if note == :help
    ptr = DBM.open($nb_database) rescue warn("DBM.open(#{$nb_database}")
    if ptr
      current_note = (ptr.key?(file) ? ptr.fetch(file) : "")
      ptr.store(file, current_note.empty? ? note : (note + "\n" + current_note))
      ptr.close
    end
  end

  def unb(file = @current_file)
    doc("unb([file=@current_file])
Removes FILE's info from the nb (dbm) data base.\n") if file == :help
    ptr = DBM.open($nb_database) rescue warn("DBM.open(#{$nb_database}")
    if ptr
      ptr.delete(file)
      ptr.close
    end
  end

  def prune_db(doc = nil)
    doc("prune_db([doc=nil]
Cleans up the nb (dbm) data base by removing references to
non-existent files.\n") if doc == :help
    ptr = DBM.open($nb_database) rescue warn("DBM.open(#{$nb_database}")
    ptr.delete_if { |k, v| k.empty? } if ptr
    ptr.close
  end

  def files_popup_info(type, position = nil, name = nil)
    doc("files_popup_info(type, position, name)
It's intended as a mouse-enter-label hook function.
It causes a description of the file to popup when the mouse crosses
the filename.\n") if type == :help
    ptr = DBM.open($nb_database) rescue warn("DBM.open(#{$nb_database}")
    file_info = lambda { |file|
      chans = mus_sound_chans(file)
      srate = mus_sound_srate(file)
      len = format("%.3f", (mus_sound_samples(file).to_f / (chans * srate)))
      d_format = mus_data_format_name(mus_sound_data_format(file))
      h_type = mus_header_type_name(mus_sound_header_type(file))
      date = Time.at(mus_sound_write_date(file)).localtime.strftime "%a %d-%b-%y %H:%M %Z"
      max_amp = mus_sound_maxamp(file)
      notes = ((ptr and ptr.key?(file)) ? ptr.fetch(file) : "")
      " #{file}
    chans: #{chans}, srate: #{srate}
   length: #{len} (#{mus_sound_frames(file)} samples)
   format: #{d_format} [#{h_type}]
 maxamp A: #{"%.3f" % max_amp[1]} (near #{"%.3f" % (max_amp[0] / srate)} secs)#{"
 maxamp B: #{"%.3f" % max_amp[3]} (near #{"%.3f" % (max_amp[2] / srate)} secs)" if chans == 2}
  written: #{date}#{"
  comment: #{mus_sound_comment(file)}" unless mus_sound_comment(file).empty?}#{"

#{notes}" unless notes.empty?}"
    }
    alert_color = make_color(1.0, 1.0, 0.94)
    current_file_viewer = 0
    previous_file_viewer = 1
    region_viewer = 2
    unless type == region_viewer
      help_exists = dialog_widgets()[14]
      @current_file = name
      help_dialog(name, file_info.call(name))
      help_widget = dialog_widgets()[14]
      if help_widget
	unless help_exists
	  files_dialog = dialog_widgets()[8]
	  files_position = widget_position(files_dialog)
	  files_size = widget_size(files_dialog)
	  set_widget_position(help_widget,
                              [files_position[0] + files_size[0] + 10, files_position[1] + 10])
	end
	recolor_widget(help_widget, alert_color)
      end
    end
    ptr.close if ptr
  end

  def files_popup_quit(type, position = nil, name = nil)
    doc("files_popup_quit(type, position, name)
It's intended as a mouse-leave-label hook function.
It unhighlights the popped-up info about a file as the mouse leaves
the associated label.\n") if type == :help
    widget = dialog_widgets()[14]
    recolor_widget(widget, basic_color()) if widget
  end

  $mouse_enter_label_hook = lambda { |t, p, n| files_popup_info(t, p, n) }
  $mouse_leave_label_hook = lambda { |t, p, n| files_popup_quit(t, p, n) }
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
envelope_interp(.3 [0, 0, 0.5, 1, 1, 0]) -> 0.6\n") if args.first == :help
    x = args[0]
    env = args[1]
    base = args[2]
    if not env
      0.0
    elsif x <= env[0] or env[2..-1].empty?
      env[1]
    elsif env[2] > x
      if env[1] == env[3] or (base and base == 0.0)
        env[1]
      elsif (not base) or base == 1.0
        env[1] + (x - env[0]) * (env[3] - env[1]) / (env[2] - env[0])
      else
        env[1] + (env[3] - env[1]) / (base - 1.0) *
                                     ((base ** ((x - env[0]) / (env[2] - env[0]))) - 1.0)
      end
    else
      envelope_interp(x, env[2..-1])
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
